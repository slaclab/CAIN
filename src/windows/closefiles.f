      SUBROUTINE CLOSEFILES
	IMPLICIT NONE
	INCLUDE '../include/ctrlcm.h'
C
	CLOSE(OUTFL)
	CLOSE(TDFL)
	IF(MSGDEST.GE.2) CALL FILEECHO(1)
	RETURN
	END