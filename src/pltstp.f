      SUBROUTINE PLTSTP(FILE,LR,KIN,FUNX,FUNY,XYMM,LOGHV,
     %   TITLE,TITLEX,TITLEY,ICOLOR,MSGFL)
	USE FLCHTYP
	USE BEAMCM
      IMPLICIT NONE
      INTEGER FILE,LR(2),KIN(3),LOGHV(2),ICOLOR,MSGFL
      CHARACTER*(*) FUNX,FUNY
      REAL*8 XYMM(2,2)
      CHARACTER*(*) TITLE,TITLEX,TITLEY
      INTEGER M,KN,GN,K,I,N,NC0(2,3),NC(3),J,NN,LNEW,IM,IRTN
      INTEGER IFLRKN
      REAL*8 XYM(2,2),X1,Y1,XY1,DUMMY(1)
	TYPE(FLCHTYPE) F1,F2
      REAL*8 XYW0(2,2)/0.0,13.0,0.0,10.0/,XYW(2,2)/0.0,10.8,0.0,10.0/
      CHARACTER*256 TTL(2,3)
      CHARACTER*1 SEP/';'/
      INTEGER IDLM(2)
      INTEGER ITYPE(3)/0,0,1/
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/tstpcm.h'
      INCLUDE 'include/topdraw.h'
	CHARACTER(80) ERR
C
      IF(NTSTP.LE.0) GOTO 900
      CALL EVCMPL(FUNX,IDLM(1),ERR)
      IF(ERR.NE.' ') GOTO 910
      CALL EVCMPL(FUNY,IDLM(2),ERR)
      IF(ERR.NE.' ') GOTO 920
      CALL RDTTL(TITLE,2,TTL(1,1),NC0(1,1),SEP)
      CALL RDTTL(TITLEX,2,TTL(1,2),NC0(1,2),SEP)
      CALL RDTTL(TITLEY,2,TTL(1,3),NC0(1,3),SEP)
      DO 200 I=1,3
        NC(I)=NC0(1,I)
 200  CONTINUE
      NN=0
      DO 260 M=1,NTSTP
        IWTSTP(M)=NN+1
        DO 210 N=1,NP
	    IF(LOST(N).NE.0) CYCLE
          IF(PNAME(N).EQ.TSTPNM(M)) THEN
            J=IFLRKN(N,LR,KIN,0,0,0,ITYPE)
            IF(J.EQ.0) GOTO 260
            KN=KIND(N)
            GN=GEN(N)
            GOTO 220
          ENDIF
 210    CONTINUE
        GOTO 260
 220    DO 240 K=1,NTSTDT
          IF(TSTPNM(M).NE.TSTPNM2(K)) GOTO 240
          CALL SETVAR2(KN,GN,TSTPDT(1,K),TSTPDT(5,K),TSTPDT(9,K),1D0,
     %         TSTPNM(M))
          CALL EVLOAD(IDLM(1),F1,IRTN)
          IF(IRTN.EQ.0.AND.F1%L.EQ.1) THEN
            CALL EVLOAD(IDLM(2),F2,IRTN)
            IF(IRTN.EQ.0.AND.F2%L.EQ.1) THEN
              NN=NN+1
              WORK(1+2*(NN-1))=F1%X
              WORK(2+2*(NN-1))=F2%X
            ENDIF
          ENDIF
 240    CONTINUE
 260  CONTINUE
      IWTSTP(NTSTP+1)=NN+1
      IF(NN.LE.1) GOTO 1000
      DO 280 I=1,2
      IF(XYMM(1,I).EQ.UNDEF%X.OR.XYMM(2,I).EQ.UNDEF%X) THEN
        XYM(1,I)=1D60
        XYM(2,I)=-1D60
        DO 270 N=1,NN
          XYM(1,I)=MIN(XYM(1,I),WORK(I+2*(N-1)))
          XYM(2,I)=MAX(XYM(2,I),WORK(I+2*(N-1)))
 270    CONTINUE
        IF(XYM(1,I).EQ.XYM(2,I)) THEN
          IF(XYM(1,I).EQ.0) THEN
            XYM(1,I)=-1
            XYM(2,I)=1
          ELSE
            XYM(1,I)=XYM(1,I)-0.5*ABS(XYM(1,I))
            XYM(2,I)=XYM(2,I)+0.5*ABS(XYM(2,I))
          ENDIF
        ELSE
          XY1=XYM(2,I)-XYM(1,I)
          XYM(1,I)=XYM(1,I)-0.05*XY1
          XYM(2,I)=XYM(2,I)+0.05*XY1
        ENDIF
        XYMM(1,I)=XYM(1,I)
        XYMM(2,I)=XYM(2,I)
      ELSE
        XYM(1,I)=XYMM(1,I)
        XYM(2,I)=XYMM(2,I)
      ENDIF
 280  CONTINUE
      IM=1
      LNEW=1
      X1=XYW(2,1)+0.2
      Y1=XYW(2,2)-1.0-0.2
      DO 400 M=1,NTSTP
        NN=IWTSTP(M+1)-IWTSTP(M)
        IF(NN.LE.0) GOTO 400
        N=IWTSTP(M)
        CALL SCAT(FILE,LNEW,-IM,DUMMY,ICOLOR,NN,WORK(1+2*(N-1)),XYW,XYM,
     %       LOGHV,TTL,NC)
        IF(LNEW.EQ.1) WRITE(FILE,340) X1+1.0,Y1,'name'
        Y1=Y1-0.3
        WRITE(FILE,320) X1,Y1,X1+0.8,Y1,PATTRN(IM)
 320    FORMAT(2(2F7.3,';'),/,
     %    ' SET PATTERN ',A,';JOIN 1 TEXT PATTERNED')
        WRITE(FILE,340) X1+1.0,Y1,TSTPNM(M)
 340    FORMAT(' TITLE ',2F7.3,' SIZE 1.4 ',1H',A,1H')
        LNEW=0
        IM=MOD(IM,MLMODE)+1
 400  CONTINUE
      GOTO 1000
 900  WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.PLTSTP) No particles satisfying the condition.')
      RETURN
 910  WRITE(MSGFL,915) FUNX,ERR
 915  FORMAT(' (SUBR.PLTSTP) Invalid function "',A,'".',/,3X,A)
      GOTO 1000
 920  WRITE(MSGFL,925) FUNY,ERR
 925  FORMAT(' (SUBR.PLTSTP) Invalid function "',A,'".',/,3X,A)
      GOTO 1000
1000	IF(IDLM(2).GE.1) CALL EVLMFREE(IDLM(2))
	IF(IDLM(2).GE.1) CALL EVLMFREE(IDLM(2))
	RETURN
      END
