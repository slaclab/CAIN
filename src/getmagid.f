	SUBROUTINE GETMAGID(NAM,IDBL,N)
C   Get magnet position of a magnet in a beamline
C   Input
C     NAM    Magnet name plus '.n' where n is a positive integer,
C            meaning the n-th occurence of the magnet in the beamline.
C            If '.n' is omitted, n=1 assumed.
C     IDBL   Beamline ID number. Its existence not checked.
C   Output
C     N      If N>=1, N-th magnet in the beam line (first magnet is 1)
C            N=0 : not found.
C            N=-1: illeagal form of '.n' part
	USE BEAMLN
	IMPLICIT NONE
	CHARACTER(*) NAM
	INTEGER IDBL,N
	INTEGER NC,I,I0,I1,I2,I3,N1,IRTN
	INTEGER IPAK1

	NC=LEN(NAM)
	N=-1
	IF(NC.LE.0) RETURN
	I0=0
	I2=-1
	DO I=1,NC
	  IF(NAM(I:I).EQ.'.') THEN
	    IF(I0.EQ.0) RETURN
	    I2=0
	  ELSEIF(NAM(I:I).NE.' ') THEN
	    IF(I2.LT.0) THEN
	      IF(I0.EQ.0) I0=I
	      I1=I
	    ELSE
	      IF(I2.EQ.0) I2=I
	      I3=I
	    ENDIF
	  ENDIF
	ENDDO
	IF(I2.LE.0) THEN
	  N1=1
	ELSE
	  N1=IPAK1(NAM(I2:I3),I3-I2+1,IRTN)
	  IF(IRTN.NE.0) RETURN
	  N1=MAX(1,N1)
	ENDIF
	DO I=BL(IDBL)%NEXP,1,-1
	  IF(NAM(I0:I1).EQ.MAG(BL(IDBL)%MAGID(I))%NAME) THEN
	    IF(N1.GE.BL(IDBL)%MAGNID(I)) THEN
	      N=I
	      RETURN
	    ENDIF
	  ENDIF
	ENDDO
	N=0
	RETURN
	END
