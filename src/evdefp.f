      SUBROUTINE EVDEFP(NAME,VALUE,IRTN)
C Define a parameter with name NAME and value VALUE.
C IRTN=0:  new parameter defined.
C      1:  parameter value revised.
C     10:  invalid form of parameter name
C     11:  specified name is reserved 
C     12:  try to delete nonexistent parameter
C    100:  too many parameters
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER IRTN,IDPAR
      CHARACTER*(*) NAME
      REAL*8 VALUE
C
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/funlis.h'
	INCLUDE 'include/evchcod.h'
C
      CHARACTER*(MCHAR) NAM
      INTEGER IENT,I,J,NC,IPAR,IDEL,LCH,LCHCOD
C
      IENT=1
      GOTO 100
      ENTRY EVDEFP1(NAME,VALUE,IDPAR,IRTN)
      IENT=2
 100  J=0
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
      IF(NUFN.GE.1) THEN
        DO I=1,NUFN
          IF(NAM.EQ.NAMUFN(I)) GOTO 920
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
      IF(NPAR.NE.0) THEN
        DO I=1,NPAR
          IF(NAM.EQ.NAMPAR(I)) THEN
            IF(I.LE.NPAR0) GOTO 920
            IPAR=I
            IF(IDEL.EQ.1) GOTO 300
            IRTN=1
            GOTO 220
          ENDIF
        ENDDO
      ENDIF
      IF(IDEL.EQ.1) GOTO 930
      IPAR=NPAR+1
      IF(IPAR.GT.MPAR) GOTO 900
      IRTN=0
      NPAR=IPAR
      NAMPAR(IPAR)=NAM
 220  VPAR(IPAR)=VALUE
      IF(IENT.EQ.2) IDPAR=IPAR
      RETURN
 300  IF(IPAR.NE.NPAR) THEN
        DO I=IPAR,NPAR-1
          VPAR(I)=VPAR(I+1)
          NAMPAR(I)=NAMPAR(I+1)
        ENDDO
      ENDIF
      NPAR=NPAR-1
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
 950  IRTN=14
      RETURN
      END
      SUBROUTINE EVDEFP2(N,IDPAR,X)
      INTEGER N,IDPAR(N)
      REAL*8 X(N)
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INTEGER I
      DO 200 I=1,N
        VPAR(IDPAR(I))=X(I)
 200  CONTINUE
      RETURN
      END
