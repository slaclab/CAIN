      RECURSIVE SUBROUTINE EVLOAD(IDLM,XOUT,IRTN)
	USE FLCHTYP
	USE EVLMOD
      IMPLICIT NONE
      INTEGER IDLM,IRTN
      TYPE(FLCHTYPE) XOUT
C	INCLUDE 'include/evlmod.h'

      XOUT%L=1
	XOUT%X=0
	XOUT%C(1)=1
	XOUT%C(2)=0
	IF(IDLM.LE.0.OR.IDLM.GT.MLOAD) GOTO 920
	IF(LOADMOD(IDLM)%NP.LE.0) GOTO 921
	CALL EVLOAD0(LOADMOD(IDLM)%PL,LOADMOD(IDLM)%PC,
     %   LOADMOD(IDLM)%NP,
     %   LOADMOD(IDLM)%GSTRLM,LOADMOD(IDLM)%NGSTRLM,XOUT,IRTN)
	RETURN
 920  IRTN=2020
C         Invalid range of IDLM
	RETURN
 921  IRTN=2021
C         IDLM-th load module already deallocated
	RETURN
	END

      RECURSIVE SUBROUTINE EVLOAD0(PL,PC,NP,GSTRLM,NGSTRLM,XOUT,IRTN)
	USE FLCHTYP
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER NP,PL(3,NP),NGSTRLM,IRTN
	CHARACTER(*) GSTRLM
      TYPE(FLCHTYPE) PC(*),XOUT
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
	INCLUDE 'include/ctrlcm.h'
	INTEGER ML,M,M1,MARG0
	PARAMETER (ML=50,M=10,M1=M+1,MARG0=10)
C        These parameters must be the same as in EVCMPL
      TYPE(FLCHTYPE) X(M1*ML+MARG0*ML)
	REAL*8 X2,DGAMMA
      REAL*8 ACOSH,ASINH,ATANH
      INCLUDE 'include/ufncm.h'
      INTEGER IP,K,I,ID,N
	INTEGER, PARAMETER:: MIND=100
	INTEGER IND(MIND)
      CHARACTER*80 ERR1
	INTEGER IDBG,IFL,IDONE
	COMMON/EVCMPLDBG/IDBG,IFL,IDONE
C
	IF(EVALNEST.GE.MAXRECURS) GOTO 999
	EVALNEST=EVALNEST+1
	NGSTR2(EVALNEST)=0
	XOUT%L=1
	XOUT%X=0
      XOUT%C(1)=1
	XOUT%C(2)=0
      IP=0
 200  IF(IDBG.NE.0.AND.IDONE.LT.IDBG) THEN
        IF(IP.EQ.0) THEN
	    WRITE(IFL,210) NP,EVALNEST,(PL(1,I),I=1,NP)
 210      FORMAT(' -- EVLOAD  # of instructions=',I3,' EvalNest=',I1,
     %       /,(3X,15I5))
	  ELSEIF(X(PL(2,IP))%L.EQ.1) THEN
	    WRITE(IFL,220) IP,PL(2,IP),X(PL(2,IP))%X
 220      FORMAT(I3,' X(',I4,')=',1PD15.7)
        ELSEIF(X(PL(2,IP))%L.EQ.2) THEN
	    WRITE(IFL,230) IP,PL(2,IP),
     %       GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2))
 230      FORMAT(I3,' X(',I4,')="',A,'"')
        ENDIF
	ENDIF
	IP=IP+1
	if(ip.le.0.or.ip.gt.np) print *,'(EVLOAD) IP=',ip
      IF(PL(1,IP).EQ.0) THEN
        IRTN=0
        XOUT=X(1)
	  IF(IDBG.GE.1) IDONE=IDONE+1
        GOTO 990
      ENDIF
	IF(PL(1,IP).GT.1000) GOTO 600
      IF(PL(1,IP).GT.100) GOTO 500
      GOTO (      1,  2,  3,  4,  5,  6,  7,  8,900,
C                             +   -   *   /   ^
     %       10, 11, 12, 13, 14, 15, 16, 17,900,900,
C            ==  /=   <   >  <=  >=  &&  ||
     %      900, 21, 22, 23, 24, 25, 26, 27, 28, 29,
     %       30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
     %       40, 41, 42, 43, 44, 45, 46, 47, 48, 49), PL(1,IP)
      GOTO 900
C
C   1: load PC(IP) into X(PL(2,IP))
 1    IF(PC(IP)%L.EQ.1) THEN
        X(PL(2,IP))=PC(IP)
	ELSE
	  CALL FLCHSET2(GSTRLM(PC(IP)%C(1):PC(IP)%C(2)),X(PL(2,IP)),ERR1)
	  IF(ERR1.NE.' ') GOTO 940
	ENDIF
      GOTO 200
C   2: load X(PL(3,IP))  into X(PL(2,IP))
 2    X(PL(2,IP))=X(PL(3,IP))
      GOTO 200
C   3: load PL(3,IP)-th scalar parameter  into X(PL(2,IP))
C       (if PL(3,IP)>0, PL(3,IP)-th floating variable.
C           else scalar character string of ID=-PL(3,IP)
 3    IF(PL(3,IP).GE.0) THEN
        X(PL(2,IP))%L=1
	  X(PL(2,IP))%X=VPAR(PL(3,IP))
	ELSE
	  CALL EVARRGET(-PL(3,IP),0,IND,X(PL(2,IP)),ERR1)
	  IF(ERR1.NE.' ') GOTO 950
	  CALL FLCHSET2(GSTR(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)),
     %    X(PL(2,IP)),ERR1)
	  IF(ERR1.NE.' ') GOTO 940
C              above 3 lines added Mar.26.2002
	ENDIF
      GOTO 200
C   4,5,6,7:  X(PL(2,IP)) +-*/ X(PL(3,IP))  into X(PL(2,IP))
 4    IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
	  X(PL(2,IP))%X=X(PL(2,IP))%X+X(PL(3,IP))%X
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.2) THEN
		CALL FLCHSET2(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2))
     %     //GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2)),
     %       X(PL(2,IP)),ERR1)
	  IF(ERR1.NE.' ') GOTO 940
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200 
 5    IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=X(PL(2,IP))%X-X(PL(3,IP))%X
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200 
 6    IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=X(PL(2,IP))%X*X(PL(3,IP))%X
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.1) THEN
	 CALL FLCHSETN(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)),
     %    NINT(X(PL(3,IP))%X),X(PL(2,IP)),ERR1)
	  IF(ERR1.NE.' ') GOTO 940
	ELSEIF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.2) THEN
	 CALL FLCHSETN(GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2)),
     %    NINT(X(PL(2,IP))%X),X(PL(2,IP)),ERR1)
	  IF(ERR1.NE.' ') GOTO 940
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200 
 7    IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(3,IP))%X.EQ.0) GOTO 901
        X(PL(2,IP))%X=X(PL(2,IP))%X/X(PL(3,IP))%X
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200 
C   8:  X(PL(2,IP))^X(PL(3,IP))  into X(PL(2,IP))
 8    IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(3,IP))%X.EQ.0) THEN
          X(PL(2,IP))%X=1
        ELSEIF(X(PL(2,IP))%X.EQ.0) THEN
          IF(X(PL(3,IP))%X.LT.0) GOTO 902
        ELSEIF(X(PL(2,IP))%X.LT.0) THEN
          K=NINT(X(PL(3,IP))%X)
          IF(ABS(DFLOAT(K)-X(PL(3,IP))%X).LE.1D-12) THEN
            X(PL(2,IP))%X=X(PL(2,IP))%X**K
          ELSE
            GOTO 903
          ENDIF
        ELSE
          X(PL(2,IP))%X=X(PL(2,IP))%X**X(PL(3,IP))%X
        ENDIF
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
C  10: ==
 10   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.EQ.X(PL(3,IP))%X) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.2) THEN
	  X(PL(2,IP))%L=1
	  IF(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)).EQ.
     %     GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2))) THEN
	    X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  11: /= 
 11   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.NE.X(PL(3,IP))%X) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.2) THEN
	  X(PL(2,IP))%L=1
	  IF(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)).NE.
     %     GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2))) THEN
	    X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  12: >
 12   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.LT.X(PL(3,IP))%X) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.2) THEN
	  X(PL(2,IP))%L=1
	  IF(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)).LT.
     %     GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2))) THEN
	    X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  13: >= 
 13   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.GT.X(PL(3,IP))%X) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.2) THEN
	  X(PL(2,IP))%L=1
	  IF(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)).GT.
     %     GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2))) THEN
	    X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  14: < 
 14   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.LE.X(PL(3,IP))%X) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.2) THEN
	  X(PL(2,IP))%L=1
	  IF(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)).LE.
     %     GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2))) THEN
	    X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  15: <=  
 15   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.GE.X(PL(3,IP))%X) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSEIF(X(PL(2,IP))%L.EQ.2.AND.X(PL(3,IP))%L.EQ.2) THEN
	  X(PL(2,IP))%L=1
	  IF(GSTR2(EVALNEST)(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)).GE.
     %     GSTR2(EVALNEST)(X(PL(3,IP))%C(1):X(PL(3,IP))%C(2))) THEN
	    X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  16: &&
 16   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.NE.0.AND.X(PL(3,IP))%X.NE.0) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  17:  ||
 17   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.NE.0.OR.X(PL(3,IP))%X.NE.0) THEN
          X(PL(2,IP))%X=1
	  ELSE
	    X(PL(2,IP))%X=0
	  ENDIF
	ELSE
	  GOTO 930
	ENDIF
	GOTO 200
C  21: unary + (actually no operation)
 21   GOTO 200
C  22: unary - (change sign of X(PL(2,IP)) )
 22   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=-X(PL(2,IP))%X
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
C  23-45:  Single argument function (Int, etc) on X(PL(2,IP))
 23   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=INT(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 24   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=NINT(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 25   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.GT.0) THEN
          X(PL(2,IP))%X=1
        ELSEIF(X(PL(2,IP))%X.EQ.0) THEN
          X(PL(2,IP))%X=0
        ELSE
          X(PL(2,IP))%X=-1
        ENDIF
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 26   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.GE.0) THEN
          X(PL(2,IP))%X=1
        ELSE
          X(PL(2,IP))%X=0
        ENDIF
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 27   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=ABS(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 28   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.GE.0) THEN
          X(PL(2,IP))%X=X(PL(2,IP))%X-INT(X(PL(2,IP))%X)
        ELSE
          X(PL(2,IP))%X=X(PL(2,IP))%X+INT(-X(PL(2,IP))%X)
        ENDIF
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 29   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.LT.0) GOTO 904
        X(PL(2,IP))%X=SQRT(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 30   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=EXP(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 31   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.LE.0) GOTO 905
        X(PL(2,IP))%X=LOG(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 32   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.LE.0) GOTO 906
        X(PL(2,IP))%X=LOG10(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 33   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=COS(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 34   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=SIN(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 35   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=TAN(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 36   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(ABS(X(PL(2,IP))%X).GT.1) GOTO 907
        X(PL(2,IP))%X=ACOS(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 37   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(ABS(X(PL(2,IP))%X).GT.1) GOTO 908
        X(PL(2,IP))%X=ASIN(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 38   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=ATAN(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 39   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=COSH(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 40   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=SINH(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 41   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=TANH(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 42   IF(X(PL(2,IP))%L.EQ.1) THEN
        X(PL(2,IP))%X=ASINH(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 43   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.LT.1) GOTO 909
        X(PL(2,IP))%X=ACOSH(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 44   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(ABS(X(PL(2,IP))%X).GE.1) GOTO 910
        X(PL(2,IP))%X=ATANH(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 45   IF(X(PL(2,IP))%L.EQ.1) THEN
        IF(X(PL(2,IP))%X.LE.0.AND.
     %   ABS(X(PL(2,IP))%X-NINT(X(PL(2,IP))%X)).LE.1D-14) GOTO 911
        X(PL(2,IP))%X=DGAMMA(X(PL(2,IP))%X)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
C  46,47:  double argument function on X(PL(2,IP)) and X(PL(2,IP)+1)
C          store results  into X(PL(2,IP))
 46   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(2,IP)+1)%L.EQ.1) THEN
        X2=X(PL(2,IP)+1)%X
        IF(ABS(X2).EQ.0) GOTO 912
        X(PL(2,IP))%X=MOD(X(PL(2,IP))%X,X2)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 47   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(2,IP)+1)%L.EQ.1) THEN
        X2=X(PL(2,IP)+1)%X
        IF(X(PL(2,IP))%X.EQ.0.AND.X2.EQ.0) GOTO 913
        X(PL(2,IP))%X=ATAN2(X(PL(2,IP))%X,X2)
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
C  48,49:  Min,Max of X(PL(2,IP)+i) (i=0,1,2,...PL(3,IP)-1)
C          store results  into X(PL(2,IP))
 48   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(PL(3,IP).LE.0) GOTO 914
        IF(PL(3,IP).GE.2) THEN
	    X2=X(PL(2,IP))%X
          DO I=1,PL(3,IP)-1
	      IF(X(PL(2,IP)+I)%L.NE.1) GOTO 930
            X2=MIN(X2,X(PL(2,IP)+I)%X)
          ENDDO
          X(PL(2,IP))%X=X2
        ENDIF
	ELSE
	  GOTO 930
	ENDIF
      GOTO 200
 49   IF(X(PL(2,IP))%L.EQ.1.AND.X(PL(3,IP))%L.EQ.1) THEN
        IF(PL(3,IP).LE.0) GOTO 914
        IF(PL(3,IP).GE.2) THEN
	    X2=X(PL(2,IP))%X
          DO I=1,PL(3,IP)-1
	      IF(X(PL(2,IP)+I)%L.NE.1) GOTO 930
            X2=MAX(X2,X(PL(2,IP)+I)%X)
          ENDDO
          X(PL(2,IP))%X=X2
	  ENDIF
	ELSE
	  GOTO 930
      ENDIF
      GOTO 200
C  101 to 999   User defined function
 500  ERR1=' '
	ID=PL(1,IP)-100
      CALL EVUFN(NAMUFN(ID),X(PL(2,IP)),PL(3,IP),GSTRLM,
     %     X(PL(2,IP)),ERR1)
      IF(ERR1.NE.' ') GOTO 960
      GOTO 200
C  >=1001  array
 600  ERR1=' '
	ID=PL(1,IP)-1000
	DO I=1,PL(3,IP)
	  IF(X(PL(2,IP)+I-1)%L.NE.1) GOTO 930
	  IND(I)=NINT(X(PL(2,IP)+I-1)%X)
	ENDDO
      CALL EVARRGET(ID,PL(3,IP),IND,X(PL(2,IP)),ERR1)
      IF(ERR1.NE.' ') GOTO 950
	CALL FLCHSET2(GSTR(X(PL(2,IP))%C(1):X(PL(2,IP))%C(2)),
     %    X(PL(2,IP)),ERR1)
	IF(ERR1.NE.' ') GOTO 940
C              above 3 lines added Mar.26.2002
      GOTO 200
C
 900  WRITE(MSGFL,'(''Program Error in EVAL/EVLOAD0'')')
      CALL STOPCAIN(100)
C
 901  IRTN=1001
C         Zero divide
      GOTO 990
 902  IRTN=1002
C         Negative power of zero.
      GOTO 990
 903  IRTN=1003
C         Fractional power of negative number.
      GOTO 990
 904  IRTN=1004
C         Negative number under square root.
      GOTO 990
 905  IRTN=1005
C         Non-positive number under log.
      GOTO 990
 906  IRTN=1006
C         Non-positive number under log10.
      GOTO 990
 907  IRTN=1007
C         Abs[Argument of ArcCos] > 1.
      GOTO 990
 908  IRTN=1008
C         Abs[Argument of ArcSin] > 1.
      GOTO 990
 909  IRTN=1009
C         Argument of ArcCosh < 1.
      GOTO 990
 910  IRTN=1010
C         Abs[Argument of ArcTanh] > 1.
      GOTO 990
 911  IRTN=1011
C         Gamma function for non-positive integer.
      GOTO 990
 912  IRTN=1012
C         2nd arg.of Mod=0.
      GOTO 990
 913  IRTN=1013
C         Argument of Atan2 = (0,0).
      GOTO 990
 914  IRTN=1014
C         No argument for Min/Max
      GOTO 990
 930  IRTN=1030
C         Float-char mismatch
      GOTO 990
 940  IRTN=1040
C         Stack GSTR2 overflow
      GOTO 990
 950  IRTN=1050
C         Error in EVARRCGET
      GOTO 990
 960  IRTN=1060
C         Error in user-defined function
      GOTO 990
 990  EVALLAST=EVALNEST
	EVALNEST=EVALNEST-1
	RETURN
 999  IRTN=1099
C         Recursive level exceeded
	RETURN
      END
