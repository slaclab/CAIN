      SUBROUTINE EVINIT(K,NSP,SP,MSGFL)
	USE FLCHTYP
	USE ARRAYMOD
	USE EVLMOD
C  K=1:  Initialize everything
C    2:  Add special characters (If NSP>=1) only
C    3:  Change MSGFL only
      IMPLICIT NONE
C
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/funlis.h'
      INCLUDE 'include/chspcm.h'
      INCLUDE 'include/evalcm.h'
      INCLUDE 'include/evalcm2.h'
C	INCLUDE 'include/evlmod.h'
	INCLUDE 'include/evcnst.h'
C
      INTEGER K,NSP,MSGFL,I,J
      CHARACTER*1 SP(*)
      INTEGER NCALL/0/
      SAVE NCALL
C
      IF(K.LE.0.OR.K.GE.4) GOTO 900
      IF(NCALL.EQ.0.AND.K.NE.1) GOTO 910
      NCALL=NCALL+1
      IF(K.EQ.1.OR.K.EQ.3) THEN
        IF(MSGFL.GE.1.AND.MSGFL.LE.199) THEN
          MSGFILE=MSGFL
          MSGFILE2=MSGFL
        ELSE
          MSGFILE=6
          MSGFILE2=6
        ENDIF
      ENDIF
      IF(K.EQ.3) RETURN
      IF(K.EQ.1) THEN
        NPAR=MPAR0
        NPAR0=MPAR0
        DO 100 I=1,MPAR0
          VPAR(I)=VPAR0(I)
          NAMPAR(I)=NAMPAR0(I)
 100    CONTINUE
        NUFN=0
        NCHSP=0
        DO 120 I=1,MFUN
          NAMFUN(I)=NAMFUN0(I)
          NVFUN(I)=NVFUN0(I)
 120    CONTINUE
      ENDIF
      IF(NSP.GE.1) THEN
        IF(K.EQ.1) NCHSP=0
        DO 200 I=1,NSP
          DO 180 J=0,MCHNM
            IF(SP(I).EQ.CHNM(J)) GOTO 200
 180      CONTINUE
          NCHSP=NCHSP+1
          CHSP(NCHSP)=SP(I)
 200    CONTINUE
C   Clear load module
	  DO I=1,MLOAD
	    LOADMOD(I)%NP=0
	  ENDDO
C   Clear array
        NARRAY=0
	  NGSTR=0
	  EVALNEST=0
	  EVCMPLNEST=0
	ENDIF
      RETURN
 900  WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.EVINIT) Invalid first argument.')
      RETURN
 910  WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.EVINIT) call EVINIT with K=1 first.')
      RETURN
      END
