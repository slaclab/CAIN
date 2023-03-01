      FUNCTION ENHFCP(Y)
	USE FLCHTYP
	USE LASRDATA
      IMPLICIT NONE
      REAL*8 ENHFCP,Y
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/lasrcm.h'
      INTEGER IRTN
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR
      IF(LENHCP.EQ.0) THEN
        ENHFCP=1
      ELSE
        CALL EVDEFP('Y',Y,IRTN)
        CALL EVAL0(ENHCP(1:NCEHCP),FC,ERR)
        IF(ERR.NE.' '.OR.FC%L.NE.1) THEN
          WRITE(MSGFL,100) ENHCP(1:NCEHCP),ERR
 100      FORMAT(' ** Error in weight function ',
     %      'of nonlinear Compton',/,3X,A,/,3X,A)
          CALL STOPCAIN(100)
        ENDIF
	  ENHFCP=FC%X
      ENDIF
      RETURN
      END
