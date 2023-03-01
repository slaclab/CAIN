	SUBROUTINE MAGSET(IDBL,ERR)
C  Set up magnet parameters which are defined as meta-expressions.
C  If IDBL=0, set all magnets.
C  If IDBL=beamline id, set those that actually appear in the beamline
	USE FLCHTYP
	USE BEAMLN
	USE ARRAYMOD
	USE MATCHMOD
	IMPLICIT NONE
	INTEGER IDBL
	CHARACTER(*) ERR
	INTEGER I,J,K
	TYPE(FLCHTYPE) FC

	IF(IDBL.EQ.0) THEN
	  DO J=1,NMAG
	    DO K=1,2
	      IF(K.EQ.1) THEN
	        FC=MAG(J)%LENGTH
	      ELSEIF(K.EQ.2) THEN
	        FC=MAG(J)%K1
	      ENDIF
	      IF(FC%L.EQ.2) THEN
	        CALL EVAL0(GSTRMG(FC%C(1):FC%C(2)),FC,ERR)
	        IF(ERR.NE.' '.OR.FC%L.NE.1) GOTO 950
	        IF(K.EQ.1) THEN
	          MAG(J)%LENGTH%X=FC%X
	        ELSEIF(K.EQ.2) THEN
	          MAG(J)%K1%X=FC%X
	        ENDIF
	      ENDIF
	    ENDDO
	  ENDDO
	ELSEIF(IDBL.GT.0.AND.IDBL.LE.NBEAMLINE) THEN
	  DO I=1,BL(IDBL)%NEXP
	    IF(BL(IDBL)%MAGNID(I).NE.1) CYCLE
	    J=BL(IDBL)%MAGID(I)
	    DO K=1,2
	      IF(K.EQ.1) THEN
	        FC=MAG(J)%LENGTH
	      ELSEIF(K.EQ.2) THEN
	        FC=MAG(J)%K1
	      ENDIF
	      IF(FC%L.EQ.2) THEN
	        CALL EVAL0(GSTRMG(FC%C(1):FC%C(2)),FC,ERR)
	        IF(ERR.NE.' '.OR.FC%L.NE.1) GOTO 950
	        IF(K.EQ.1) THEN
	          MAG(J)%LENGTH%X=FC%X
	        ELSEIF(K.EQ.2) THEN
	          MAG(J)%K1%X=FC%X
	        ENDIF
	      ENDIF
	    ENDDO
	  ENDDO
	ELSE
	  GOTO 900
	ENDIF
	ERR=' '
	RETURN
900   ERR=' (SUBR.MAGSET) Invalid Bbeamline ID'
	RETURN
950   ERR=' (SUBR.MAGSET) Error in evaluating magnet parameter "'//
     %     GSTRMG(FC%C(1):FC%C(2))//'"'
	RETURN
	END

