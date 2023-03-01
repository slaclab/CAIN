      FUNCTION EVRDNM(T,I0,I,ERR)
C  Read floating number (atof)
      IMPLICIT NONE
      CHARACTER*(*) T,ERR
      INTEGER I0,I
      REAL*8 EVRDNM
	INCLUDE 'include/evchcod.h'
      INTEGER NC,IDEC,LCH,IEXP,IRTN1
      REAL*8 X
      INTEGER LCHCOD
      REAL*8 FLOT1
C
      EVRDNM=0
      NC=LEN(T)
      LCH=LCHCOD(T(I0:I0))
      IDEC=0
      IF(LCH.EQ.C_DEC) IDEC=1
C              decimal point
      IEXP=0
C         IEXP=0 ---- before exponent part
C              1 ---- after 'E','D','Q'
C              2 ---- after +- in the exponent part
C              3 ---- after a number in the exponent part
      I=I0
 260  I=I+1
      IF(I.LE.NC) THEN
        LCH=LCHCOD(T(I:I))
        IF(LCH.EQ.C_BLANCK) THEN
          GOTO 260
        ELSEIF(LCH.GE.C_NUM.AND.LCH.LE.C_NUM+9) THEN
          IF(IEXP.NE.0) IEXP=3
          GOTO 260
        ELSEIF(LCH.EQ.C_DEC) THEN
          IF(IEXP.NE.0) THEN
            ERR=' Decimal point in exponent. "'//T(I0:I)
     %             //'".   (EVRDNM)'
            RETURN
          ELSEIF(IDEC.NE.0) THEN
            ERR=' Two decimal points "'//T(I0:I)//'".   (EVRDNM)'
            RETURN
          ENDIF
          IDEC=1
          GOTO 260
        ELSEIF(LCH.EQ.C_PLUS.OR.LCH.EQ.C_MINUS) THEN
C                 '+' or '-'
          IF(IEXP.EQ.2) THEN
            ERR=' Illegal +/- in exponent. "'//T(I0:I)
     %                  //'".   (EVRDNM)'
            RETURN
          ELSEIF(IEXP.EQ.1) THEN
            IEXP=2
            GOTO 260
          ENDIF
        ELSEIF(LCH.GE.C_EXP.AND.LCH.LT.C_UALPHA) THEN
C     exponent mark
          IF(IEXP.EQ.0) THEN
            IEXP=1
            GOTO 260
          ENDIF
        ENDIF
      ENDIF
      IF(IEXP.GE.1.AND.IEXP.LE.2) THEN
        ERR='Exponent does not close.   (EVRDNM)'
        RETURN
      ENDIF
      X=FLOT1(T(I0:I-1),I-I0,IRTN1)
      IF(IRTN1.NE.0) THEN
        ERR='Program error 1.   (EVRDNM)'
        RETURN
      ENDIF
      EVRDNM=X
      ERR=' '
      RETURN
      END
