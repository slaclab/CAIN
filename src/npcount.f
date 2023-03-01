      FUNCTION NPCOUNT(L,LR,KIN,LGEN,IGEN,LLOST,ITYPE,FSEL)
C  Count the number of particles in the list
C  L=0:  total number (ignore other parameters)
C        including LOST particles.
C   >0:  those satisfying the conditions
C  LLOST=0:  not include lost particles
C        1:  include only lost particles (LOST=2)
	USE FLCHTYP
	USE BEAMCM
      IMPLICIT NONE
      INTEGER NPCOUNT,L,LR(2),KIN(3),LGEN,IGEN,LLOST,ITYPE(3)
	CHARACTER(*) FSEL
      INTEGER N,II,IFLRKN,IDLM,IRTN
	TYPE(FLCHTYPE) FSEL1
	CHARACTER(80) ERR
C      INCLUDE 'include/beamcm.h'
C
      IF(L.LE.0) THEN
        NPCOUNT=NP
      ELSE
        NPCOUNT=0
        IF(NP.GE.1) THEN
	    IDLM=0
	    IF(FSEL.NE.' ') THEN
		    CALL EVCMPL(FSEL,IDLM,ERR)
	      IF(ERR.NE.' ') GOTO 950
	    ENDIF
          DO 200 N=1,NP
            II=IFLRKN(N,LR,KIN,LGEN,IGEN,LLOST,ITYPE)
            IF(II.EQ.0) CYCLE
	      IF(IDLM.NE.0) THEN
	        CALL SETVAR(N,IRTN)
              IF(IRTN.NE.0) CYCLE
	        CALL EVLOAD(IDLM,FSEL1,IRTN)
	        IF(FSEL1%L.NE.1) CYCLE
	        IF(FSEL1%X.EQ.0) CYCLE
	      ENDIF
				NPCOUNT=NPCOUNT+1
 200      CONTINUE
          IF(IDLM.GE.1) CALL EVLMFREE(IDLM)
        ENDIF
      ENDIF
      RETURN
950   NPCOUNT=0
	RETURN
      END
