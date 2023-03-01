	RECURSIVE SUBROUTINE EVUFNLASER(FNAME,K,X,NV,Y,
     %  IRTN,IERARG,ERRMSG)
C  Laser-related functions
C     'LaserIntensity','LaserRange'
C  IRTN=0  normal
C       1  warning (ERRMSG)
C     101  Wrong number of arguments
C     102  Range of the IERARG-th argument invalid
C     103  Type (float/char) of the IERARG-th argument mismatch
C     200  Others. (ERRMSG)
	USE FLCHTYP
	USE LASRDATA
      IMPLICIT NONE
      INTEGER K,NV,IRTN,IERARG
      TYPE(FLCHTYPE) Y,X(NV)
      CHARACTER*(*) FNAME,ERRMSG
	INCLUDE 'include/lasrcm.h'
	INTEGER NV1,I,J,K1,K2
	REAL(8) TXYS2(0:3),DUMMY,DUMMY2,DMM(3,3)

	IF(K.EQ.1) THEN
	  NV1=5
	ELSEIF(K.EQ.2) THEN
	  NV1=3
	ENDIF
	I=1
      IF(NV.EQ.NV1) THEN
	  IF(X(NV1)%L.NE.1) THEN
	    IERARG=NV1
	    IRTN=103
	    RETURN
	  ENDIF
	  I=NINT(X(NV1)%X)
	ELSEIF(NV.NE.NV1-1) THEN
	  IRTN=101
	  RETURN
	ENDIF
	IF(I.LE.0.OR.I.GT.NLSR) GOTO 900
	IF(K.EQ.1) THEN
	  DO J=0,3
	    IF(X(J+1)%L.NE.1) THEN
	      IERARG=J+1
	      IRTN=103
	      RETURN
	    ENDIF
	    TXYS2(J)=X(J+1)%X
	  ENDDO
	  CALL LSRGEO(I,TXYS2,Y%X,DUMMY,DMM,DUMMY2,0)
	ELSEIF(K.EQ.2) THEN
	  K1=NINT(X(1)%X)
	  IF(K1.LE.0.OR.K1.GE.3) THEN
	    IERARG=1
	    IRTN=102
	    RETURN
	  ENDIF
	  K2=NINT(X(2)%X)
	  IF(K2.LT.0.OR.K2.GT.3) THEN
	    IERARG=2
	    IRTN=102
	    RETURN
	  ENDIF
	  Y%X=LSRRNG(K1,K2,I)
	ENDIF
	IRTN=0
	RETURN

 900	WRITE(ERRMSG,402) I,FNAME
 402  FORMAT('Laser#',I2,' undefined. FUNC.',A,' failed.')
      IRTN=200
	RETURN
	END