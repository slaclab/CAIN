      SUBROUTINE RDARRAY(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=2)
      CHARACTER*12 OP(MOP)/'FREE','expression'/
      INTEGER NFF(MOP)/-1,1/
      INTEGER NC,J,I,L,KK,K,IDAR,NIND,IFREE,NC1
	TYPE(FLCHTYPE) FC
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
	INCLUDE 'include/nameleng.h'
	CHARACTER(MCHAR) NAM
	CHARACTER(80) ERR
      REAL*8 X
	CHARACTER*9 MSG(0:1)/'(new)','(revised)'/
	INTEGER, PARAMETER:: MIND=50
	INTEGER NNIND(MIND),IND(MIND),DIM2(2,MIND)
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('ARRAY',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GE.2) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.EQ.0) RETURN
	IFREE=0
      DO J=1,NBL
	  I=KBL(J)
	  IF(OP(I).EQ.'FREE') THEN
	    IFREE=1
	  ELSEIF(OP(I).EQ.'expression') THEN
          IF(REL(J).NE.0.AND.REL(J).NE.1) GOTO 910
          CALL BLKREC(LNKW(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          CALL EVLHS(TEXT(1:NC),KK,NAM,NC1,IDAR,MIND,NIND,NNIND,IND,ERR)
	    IF(ERR.NE.' ') GOTO 930
	    IF(KK.EQ.1.OR.NIND.EQ.0) GOTO  940     ! scalar variable name
	    IF(IFREE.EQ.0) THEN      !  allocation
	      IF(NIND.EQ.0) GOTO 950
	      IF(REL(J).EQ.1) THEN
              CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
              IF(IRTN.NE.0) GOTO 990
              CALL EVAL0(TEXT(1:NC),FC,ERR)
              IF(ERR.NE.' ') GOTO 920
	      ELSE
	        FC%L=1
	        IF(NAM(1:1).EQ.'$') FC%L=1
	        FC%X=0
	        FC%C(1)=1
	        FC%C(2)=0
	      ENDIF
	      K=1
	      DO L=1,NIND
	        IF(NNIND(L).GE.3) GOTO 950
	        IF(NNIND(L).EQ.1) THEN
	          DIM2(1,L)=1
	          DIM2(2,L)=IND(K)
	        ELSE
	          DIM2(1,L)=IND(K)
	          DIM2(2,L)=IND(K+1)
	        ENDIF
	        IF(DIM2(2,L).LT.DIM2(1,L)) GOTO 950
	        K=K+NNIND(L)
	      ENDDO
	      CALL EVDEFARR(NAM(1:NC1),2,NIND,2,DIM2,FC%X,IDAR,IRTN,ERR)
C               Data initialization for floating array only.
	      IF(IRTN.GE.2) GOTO 930
	      IF(MSGLVL.GE.1) THEN
	        CALL ARRAYSTR(NAM(1:NC1),NIND,2,2,DIM2,TEXT,NC)
	        WRITE(MSGFL,180) TEXT(1:NC),MSG(IRTN)
180	        FORMAT('  ARRAY defined : ',A,1X,A)
	      ENDIF
	    ELSE            !  free
	      IF(NIND.NE.0.OR.REL(J).NE.0) GOTO 960
	      IF(KK.EQ.0) GOTO 970
	      CALL EVARFREE(IDAR)
	      IF(MSGLVL.GE.1) THEN
	        WRITE(MSGFL,200) NAM(1:NC1)
200           FORMAT('  ARRAY FREE: array ',A,' freed.')
            ENDIF
	    ENDIF
	  ENDIF
	ENDDO
      IRTN=0
      RETURN

 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDARRAY) Invalid relational operator ',
     %    'in ARRAY command.')
      GOTO 990
 920  IRTN=1002
      WRITE(MSGFL,925) TEXT(1:NC),ERR
 925  FORMAT(' (SUBR.RDARRAY) Invalid expression "',A,'".',/,
     %    3X,A)
      GOTO 990
 930  IRTN=935
      WRITE(MSGFL,'(A)') ERR
	GOTO 990
 940  IRTN=945
      WRITE(MSGFL,945) NAM(1:NC1)
 945  FORMAT(' (SUBR.RDARRAY) Scalar variable name in ARRAY command.',A)
	GOTO 990
 950  IRTN=955
      WRITE(MSGFL,955)
 955  FORMAT(' (SUBR.RDARRAY) Invalid array subscript range.')
	GOTO 990
 960  IRTN=965
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.RDARRAY) Invalid ARRAY FREE syntax.')
	GOTO 990
 970  IRTN=975
      WRITE(MSGFL,975) NAM(1:NC1)
 975  FORMAT(' (SUBR.RDARRAY) Trying to free undefined array ',A)
	GOTO 990
 990  CALL PRECHO(LN(1,1),LN(2,1),LN(1,3),LN(2,3),LINE,NCHAR)
      RETURN
      END

