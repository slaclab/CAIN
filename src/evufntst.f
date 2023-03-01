	SUBROUTINE EVUFNTST(K1,X,NV,Y,GSTRX,IRTN,IERARG,ERRMSG)
C  Test particle functions
C        'TestT','TestX','TestY','TestS',
C        'TestEn','TestPx','TestPy','TestPs'
C  IRTN=0  normal
C       1  warning (ERRMSG)
C     101  Wrong number of arguments
C     102  Range of the IERARG-th argument invalid
C     103  Type (float/char) of the IERARG-th argument mismatch
C     200  Others. (ERRMSG)
	USE FLCHTYP
	USE BEAMCM
      IMPLICIT NONE
      INTEGER K1,NV,IRTN,IERARG
      TYPE(FLCHTYPE) Y,X(NV)
      CHARACTER*(*) GSTRX,ERRMSG
	INTEGER I
	CHARACTER(4) NAMP

	IF(X(1)%L.EQ.1) THEN
	  WRITE(NAMP,'("T",I3)') NINT(X(1)%X)
	ELSE
	  NAMP='T'//GSTRX(X(1)%C(1):X(1)%C(2))
	ENDIF
	DO I=1,NP
        IF(PNAME(I).EQ.NAMP) THEN
	    IF(K1.LE.4) THEN
	      Y%X=TXYS(K1-1,I)
	    ELSE
	      Y%X=EP(K1-5,I)
	    ENDIF
	    IRTN=0
	    RETURN
	  ENDIF
      ENDDO
      ERRMSG='Test particle "'//NAMP//'" not found.'
	IRTN=200
	RETURN
	END
