	SUBROUTINE RESUME
C Ask for response for debug
C Any key except space or return will cause stop.
	IMPLICIT NONE
	CHARACTER*1 A
	INCLUDE 'include/ctrlcm.h'
	WRITE(MSGFL,100)
100   FORMAT('Press RETURN or SPACE to continue. ',
     %   'Any other key to stop.')
	CALL FILEECHO(1)
	READ(5,200) A
200   FORMAT(A)
      IF(A.NE.' ') THEN
	  CALL CPUTIM('    ',3)
	  CALL STOPCAIN(0)
	ENDIF
	RETURN
	END