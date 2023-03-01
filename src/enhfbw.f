      FUNCTION ENHFBW(Y)
	USE FLCHTYP
	USE ARRAYMOD
	USE LASRDATA
      IMPLICIT NONE
      REAL*8 ENHFBW,Y
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/lasrcm.h'
      INTEGER IRTN
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR
      IF(LENHBW.EQ.0) THEN
        ENHFBW=1
      ELSE
        CALL EVDEFP('Y',Y,IRTN)
        CALL EVAL0(ENHBW(1:NCEHBW),FC,ERR)
        IF(ERR.NE.' '.OR.FC%L.NE.1) THEN
          WRITE(MSGFL,100) ENHBW(1:NCEHBW),ERR
 100      FORMAT(' ** Error in rate enhancement function ',
     %      'of nonlinear Breit-Wheeler',/,3X,A,/,3X,A)
          CALL STOPCAIN(100)
        ENDIF
	  ENHFBW=FC%X
      ENDIF
      RETURN
      END
