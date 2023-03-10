	SUBROUTINE MATCHFUN(N,M,X,OBJFUN,CONFUN)
	USE FLCHTYP
c	USE BEAMLN
	USE ARRAYMOD
	USE MATCHMOD
	IMPLICIT NONE
	INTEGER N,M
	REAL(8) X(N),OBJFUN,CONFUN(M)
	INCLUDE 'include/nameleng.h'
	INCLUDE 'include/evparc.h'
	INCLUDE 'include/ctrlcm.h'
	INTEGER I,IRTN
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR

	IF(MSGLVL.GE.2) THEN
	  WRITE(MSGFL,200)
200     FORMAT(' SUBR.MATCHFUN called. Variables=')
        DO I=1,NVAR
	    WRITE(MSGFL,220) VARNAM(I),X(I)
220       FORMAT(5X,A,'=',1PD14.7)
        ENDDO
	ENDIF
	DO I=1,N
	  IF(KVAR(1,I).GE.0) THEN
	    VPAR(KVAR(1,I))=X(I)
	  ELSE
	    ARR(-KVAR(1,I))%VAL(KVAR(2,I))=X(I)
	  ENDIF
	ENDDO
	CALL MATCHSET(IRTN)
	IF(IRTN.NE.0) GOTO 990
	OBJFUN=0
	CONFUN=0
	DO I=1,NCOND
	  CALL EVAL0(GSTRMATCH(COND(I)%C(1):COND(I)%C(2)),FC,ERR)
	  IF(ERR.NE.' '.OR.FC%L.NE.1) GOTO 920
	  CONFUN(I)=FC%X
	  COND(I)%X=FC%X
	  IF(I.LE.NCOND-NPOS) THEN
	    OBJFUN=OBJFUN+FC%X**2
	  ELSEIF(FC%X.LT.0) THEN
	    OBJFUN=OBJFUN+FC%X**2
	  ENDIF
	ENDDO
	CONV=SQRT(OBJFUN)
	IF(MINFUN%L.EQ.2) THEN
	  CALL EVAL0(GSTRMATCH(MINFUN%C(1):MINFUN%C(2)),FC,ERR)
	  IF(ERR.NE.' '.OR.FC%L.NE.1) GOTO 930
	  MINFUN%X=FC%X
	  OBJFUN=OBJFUN+FC%X
	ENDIF
	IRTN=0
	GOTO 1000
920   IRTN=1020
	OBJFUN=100000
	IF(MSGLVL.GE.0) WRITE(MSGFL,925)
925   FORMAT(' (SUBR.MATCHFUN) Error in evaluating the matching ',
     %   'condition "',A,'"')
	GOTO 1000
930   IRTN=1030
	OBJFUN=100000
	IF(MSGLVL.GE.0) WRITE(MSGFL,935)
935   FORMAT(' (SUBR.MATCHFUN) Error in evaluating the MINFUN "',
     %   A,'"')
	GOTO 1000
990   OBJFUN=100000
	GOTO 1000

1000  RETURN
	END

	    