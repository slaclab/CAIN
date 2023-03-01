      SUBROUTINE EXLOGC(TEXT,NC,IRTN)
C  Evaluate logical expression
C  TEXT(1): left-hand-side, TEXT(3): right-hand-side
C  TEXT(2): relational operator  =, <, >, <=, =>, >=, => (left adjusted)
C  IRTN=0: false
C       1: true
C      >1: error
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER NC(3),IRTN
      CHARACTER*(*) TEXT(3)
      INCLUDE 'include/ctrlcm.h'
      INTEGER J,IRTNEV
      REAL*8 XL,XR
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR
      J=1
      CALL EVAL0(TEXT(J)(1:NC(J)),FC,ERR)
      IF(ERR.NE.' ') GOTO 900
	IF(FC%L.NE.1) GOTO 920
	XL=FC%X
      J=3
      CALL EVAL0(TEXT(J)(1:NC(J)),FC,ERR)
	XR=FC%X
      IF(ERR.NE.' ') GOTO 900
	IF(FC%L.NE.1) GOTO 920
      IRTN=0
      IF(TEXT(2)(1:NC(2)).EQ.'=') THEN
        IF(XL.EQ.XR) IRTN=1
      ELSEIF(TEXT(2)(1:NC(2)).EQ.'<') THEN
        IF(XL.LT.XR) IRTN=1
      ELSEIF(TEXT(2)(1:NC(2)).EQ.'>') THEN
        IF(XL.GT.XR) IRTN=1
      ELSEIF(TEXT(2)(1:NC(2)).EQ.'=<'.OR.TEXT(2)(1:NC(2)).EQ.'<=') THEN
        IF(XL.LE.XR) IRTN=1
      ELSEIF(TEXT(2)(1:NC(2)).EQ.'=>'.OR.TEXT(2)(1:NC(2)).EQ.'>=') THEN
        IF(XL.GE.XR) IRTN=1
      ELSEIF(TEXT(2)(1:NC(2)).EQ.'<>'.OR.TEXT(2)(1:NC(2)).EQ.'><') THEN
        IF(XL.NE.XR) IRTN=1
      ELSE
        GOTO 910
      ENDIF
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905) TEXT(J)(1:NC(J)),ERR
 905  FORMAT(' (SUBR.EXLOGC) Error in evaluating "',A,'".',/,3X,A)
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915) TEXT(2)(1:NC(2))
 915  FORMAT(' (SUBR.EXLOGC) Invalid relational operator "',A,'".')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925) TEXT(J)(1:NC(J))
 925  FORMAT(' (SUBR.EXLOGC) Unexpected character expression "',A,'".')
      RETURN
      END
