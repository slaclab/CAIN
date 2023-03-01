	SUBROUTINE CHKMAGNAME(NAM,IRTN)
C  Check if the name is a valid magnet/beamline name
C      IRTN>0   Valid. IRTN is the number of characters
C          =0   Empty name
C          <0   (-RTN)th character is invalid
	IMPLICIT NONE
	INTEGER IRTN
	CHARACTER(*) NAM
	INTEGER N0,N,I
	CHARACTER(1) C

	N0=LEN(NAM)
	IRTN=0
	IF(N0.LE.0) RETURN
	N=0
	DO I=N0,1,-1
	  IF(NAM(I:I).NE.' ') THEN
	    N=I
	    EXIT
	  ENDIF
	ENDDO
	IF(N.EQ.0) RETURN
	DO I=1,N
	  C=NAM(I:I)
	  IF(C.GE.'A'.AND.C.LE.'Z') CYCLE
	  IF(C.GE.'a'.AND.C.LE.'z') CYCLE
	  IF(C.GE.'0'.AND.C.LE.'9') CYCLE
	  IF(C.EQ.'_') CYCLE
	  IRTN=-I
	  RETURN
	ENDDO
	IRTN=N
	RETURN
	END
