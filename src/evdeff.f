      SUBROUTINE EVDEFF(NAME,NV,IRTN)
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER NV,IRTN
      CHARACTER*(*) NAME
C
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/funlis.h'
	INCLUDE 'include/evchcod.h'
C
      CHARACTER*(MCHAR) NAM
      INTEGER I,J,NC,IUFN,IDEL,LCH
      INTEGER LCHCOD
C
      J=0
      NC=LEN(NAME)
      NAM=' '
      IDEL=0
      IF(NC.LE.0) RETURN
      DO I=1,NC
        LCH=LCHCOD(NAME(I:I))
        IF(LCH.NE.C_BLANCK) THEN
          IF(J.EQ.0.AND.LCH.EQ.C_MINUS) THEN
            IDEL=1
            CYCLE
          ELSEIF(LCH.GE.C_NUM.AND.LCH.LE.C_VAR) THEN
            IF(J.EQ.0.AND.LCH.LT.C_EXP) GOTO 910
	    ELSEIF(LCH.EQ.C_DOLLAR) THEN
	      IF(J.NE.0) GOTO 910
	    ELSE
	      GOTO 910
          ENDIF
          J=J+1
	    IF(J.GT.MCHAR) GOTO 950
          NAM(J:J)=NAME(I:I)
        ENDIF
      ENDDO
      IF(NPAR.GE.1) THEN
        DO I=1,NPAR
          IF(NAM.EQ.NAMPAR(I)) GOTO 920
        ENDDO
      ENDIF
      DO I=1,MFUN
        IF(NAM.EQ.NAMFUN(I)) GOTO 920
      ENDDO
 	IF(NARRAY.GT.0) THEN
	  DO I=1,NARRAY
	    IF(ARR(I)%TYPE.NE.0) THEN
	      IF(NAM.EQ.ARR(I)%NAME) GOTO 920
	    ENDIF
	  ENDDO
	ENDIF
      IF(NUFN.NE.0) THEN
        DO I=1,NUFN
          IF(NAM.EQ.NAMUFN(I)) THEN
            IUFN=I
            IF(IDEL.EQ.1) GOTO 300
            IRTN=1
            GOTO 220
          ENDIF
        ENDDO
      ENDIF
      IF(IDEL.EQ.1) GOTO 930
      IUFN=NUFN+1
      IF(IUFN.GT.MUFN00) GOTO 900
      IF(NV.LT.0.OR.(NV.GT.MARG.AND.NV.LT.1000)) GOTO 940
      IRTN=0
      NUFN=IUFN
      NAMUFN(IUFN)=NAM
      NVUFN(IUFN)=NV
 220  RETURN
C
 300  IF(IUFN.NE.NUFN) THEN
        DO I=IUFN,NUFN-1
          NAMUFN(I)=NAMUFN(I+1)
        ENDDO
      ENDIF
      NUFN=NUFN-1
      IRTN=0
      RETURN
 900  IRTN=100
      RETURN
 910  IRTN=10
      RETURN
 920  IRTN=11
      RETURN
 930  IRTN=12
      RETURN
 940  IRTN=13
      RETURN
 950  IRTN=14
      RETURN
      END
