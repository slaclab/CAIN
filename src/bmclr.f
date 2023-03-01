      SUBROUTINE BMCLR(LR,KIN,ITYPE,LGEN,IGEN,FSEL,NCLR)
	USE BEAMCM
C  Eliminate particles
C      .or. is applied within each array
C      .and. is applied over different arrays
C    LR     1: right-going,  2: left-going
C    KIN    1: photon,  2: electron,  3: positron
C    ITYPE  1: normal particle  
C           2: incoherent pair,  
C           3: test particle
C    LGEN   If =0, all generation (IGEN is ignored)
C           1:=, 2:<> or ><, 3:<, 4:>, 5:<= or =<, 6:>= or =>.
C   Lost particles are cleared in any case
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER LR(2),KIN(3),ITYPE(3),LGEN,IGEN,NCLR
	CHARACTER(*) FSEL
      INCLUDE 'include/ctrlcm.h'
      INTEGER IFLRKN
      INTEGER N,K,IDLM,IRTN
	TYPE(FLCHTYPE) FSEL1
	CHARACTER(80) ERR
C
      NCLR=0
      IF(NP.EQ.0) RETURN
	IDLM=0
	IF(FSEL.NE.' ') THEN
	  CALL EVCMPL(FSEL,IDLM,ERR)
	  IF(ERR.NE.' ') GOTO 900
	ENDIF
      DO 200 N=1,NP
        IF(LOST(N).EQ.0) THEN
          IF(IFLRKN(N,LR,KIN,LGEN,IGEN,0,ITYPE).EQ.0) CYCLE
	    IF(IDLM.NE.0) THEN
	      CALL SETVAR(N,IRTN)
            IF(IRTN.EQ.0) THEN
	        CALL EVLOAD(IDLM,FSEL1,IRTN)
	        IF(FSEL1%L.NE.1.OR.IRTN.NE.0) CYCLE
	        IF(FSEL1%X.EQ.0) CYCLE
	      ENDIF
	    ENDIF
	  ENDIF
        LOST(N)=1
        NCLR=NCLR+1
 200  CONTINUE
      IF(NCLR.EQ.0) RETURN
      IF(NCLR.EQ.NP) THEN
        NP=0
      ELSE
        CALL DELLOS
      ENDIF
	GOTO 1000
900   K=-1
      WRITE(MSGFL,905) FSEL,ERR
905   FORMAT(' (SUBR.BMCLR) SELECT function "',A,'" invalid.',/,
     %   '    "',A,'"')
      GOTO 1000
1000	IF(IDLM.GE.1) CALL EVLMFREE(IDLM)
      RETURN
      END
