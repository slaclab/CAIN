	SUBROUTINE NEXTMAG(JMAG,APERT)
C      Find the next magnet in the beamline specified by SUBR.STARTBL
C       JMAG>0   magnet ID
C           =0   no more element
C           =-1  STARTBL not called yet
C           =-2  nesting level too deep
C           =-3  recursive citation of beamline name
	USE BEAMLN
	IMPLICIT NONE
	INTEGER JMAG
	REAL(8) APERT(2)
      INCLUDE 'include/ctrlcm.h'
	INCLUDE 'include/blexpand.h'
	INTEGER ITYPE,J,L,I

	IF(LVL.LE.0) THEN
	  JMAG=-1
	  GOTO 900
	ENDIF
200	IF(LVL.LE.0) GOTO 400
	ELN(LVL)=ELN(LVL)+DIR(LVL)
	IF(ELN(LVL).GT.BL(BLID(LVL))%NEL.OR.ELN(LVL).LE.0) THEN
	  LVL=LVL-1
	  GOTO 200
	ENDIF
	ITYPE=BL(BLID(LVL))%ELTYPE(ELN(LVL))
	J=BL(BLID(LVL))%ELID(ELN(LVL))
	IF(ITYPE.EQ.0) THEN
	  JMAG=J
	  DO I=1,2
	    IF(MAG(J)%APERT(I).NE.0) THEN
	      APERT(I)=MAG(J)%APERT(I)
	    ELSE
	      APERT(I)=APERT0(I,LVL)
	    ENDIF
	  ENDDO
	  RETURN
	ENDIF
	IF(ITYPE.EQ.1.OR.ITYPE.EQ.-1) THEN
	  IF(LVL.GE.MLEVEL) THEN
	    IF(MSGLVL.GE.0) THEN
	      WRITE(MSGFL,240)
240         FORMAT(' (SUBR.NEXTMAG) Beamline nesting level too deep.')
          ENDIF
	    JMAG=-2
	    LVL=0
	    GOTO 900
	  ENDIF
	  LVL=LVL+1
	  DIR(LVL)=ITYPE*DIR(LVL-1)
	  IF(DIR(LVL).EQ.1) THEN
	    ELN(LVL)=0
	  ELSE
	    ELN(LVL)=BL(J)%NEL+1
	  ENDIF
	  BLID(LVL)=J
C            check recursive citation
	  IF(LVL.GE.2) THEN
	    DO L=1,LVL-1
	      IF(BLID(L).EQ.J) THEN
	        JMAG=-3
	        LVL=0
	        GOTO 900
	      ENDIF
	    ENDDO
	  ENDIF
C
        IF(LVL.GE.2) THEN
	    DO I=1,2
	      IF(BL(BLID(LVL))%APERT(I).NE.0) THEN
	        APERT0(I,LVL)=BL(BLID(LVL))%APERT(I)
	      ELSE
	        APERT0(I,LVL)=APERT0(I,LVL-1)
	      ENDIF
	    ENDDO
	  ELSE
	    APERT0(1:2,LVL)=BL(BLID(LVL))%APERT
	  ENDIF
	  GOTO 200
	ENDIF
C      should not come here
400   JMAG=0
	RETURN

900   RETURN
	END
