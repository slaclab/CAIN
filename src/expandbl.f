	SUBROUTINE EXPANDBL(NAM,IRTN)
C  Expand beamline into magnets.
C  Define  BL%NEXP
C  Allocate and fill BL%MAGID, BL%MAGNID, BL%SBL
C  IRTN   0:   normal
C       100:   ?
C       101:   undefined beamline name
C       102:   nesting level too deep
C       103:   recursive citation of beamline name
C      1000:   memory allocation error

	USE BEAMLN
	IMPLICIT NONE
	INTEGER IRTN
	CHARACTER(*) NAM
      INCLUDE 'include/ctrlcm.h'
	INTEGER MLEVEL
	INTEGER II,JJ,ITYPE,IREP,NEXP,N,ISTAT
	REAL(8) S,APERT1(2)

	DO IREP=1,2
C      repeat twice. First to count the number of magnets.
	  NEXP=0
	  S=0
	  CALL STARTBL(NAM,II,APERT1)
	  IF(II.LE.0) THEN
	    IRTN=101                !  undefined beamline name
	    RETURN
	  ENDIF

200	  CALL NEXTMAG(JJ,APERT1)
        IF(JJ.LT.0) THEN
	    IRTN=100
	    IF(JJ.EQ.-2) IRTN=102   !  nesting level too deep
	    IF(JJ.EQ.-3) IRTN=103   !  recursive citation of beamline name
	    RETURN
	  ENDIF
	  IF(JJ.NE.0) THEN
	    NEXP=NEXP+1
	    IF(IREP.EQ.2) THEN
	      BL(II)%MAGID(NEXP)=JJ
	      BL(II)%MAGNID(NEXP)=1
	      IF(NEXP.GE.2) THEN
	        DO N=NEXP-1,1,-1
	          IF(BL(II)%MAGID(N).EQ.JJ) THEN
	            BL(II)%MAGNID(NEXP)=BL(II)%MAGNID(N)+1
	            EXIT
	          ENDIF
	        ENDDO
	      ENDIF
	      S=S+MAG(JJ)%LENGTH%X
	      BL(II)%SBL(NEXP)=S
	    ENDIF
	    GOTO 200
	  ENDIF
	  IF(IREP.EQ.1) THEN
	    IF(BL(II)%NEXP.NE.0) THEN
	      DEALLOCATE(BL(II)%MAGID,STAT=ISTAT)
	      DEALLOCATE(BL(II)%MAGNID,STAT=ISTAT)
	      DEALLOCATE(BL(II)%SBL,STAT=ISTAT)
	      IF(BL(II)%LTWISS) THEN
	        DEALLOCATE(BL(II)%TWISS,BL(II)%TMAT,STAT=ISTAT)
	      ENDIF
	    ENDIF
	    BL(II)%NEXP=NEXP
	    ALLOCATE(BL(II)%MAGID(NEXP),STAT=ISTAT)
	    IF(ISTAT.NE.0) GOTO 900
	    ALLOCATE(BL(II)%MAGNID(NEXP),STAT=ISTAT)
	    IF(ISTAT.NE.0) GOTO 900
	    ALLOCATE(BL(II)%SBL(0:NEXP),STAT=ISTAT)
	    IF(ISTAT.NE.0) GOTO 900
	    BL(II)%SBL(0)=0
	    BL(II)%LTWISS=.FALSE.
	  ENDIF
	ENDDO
	BL(II)%STOT=S
	IRTN=0
	RETURN
900   IRTN=1000
	DEALLOCATE(BL(II)%MAGID,STAT=ISTAT)
	DEALLOCATE(BL(II)%MAGNID,STAT=ISTAT)
	DEALLOCATE(BL(II)%SBL,STAT=ISTAT)
	RETURN
	END