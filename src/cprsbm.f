      SUBROUTINE CPRSBM(FAC)
	USE BEAMCM
C           Corrected on May.17.1999
      IMPLICIT NONE
      REAL*8 FAC
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/ctrlcm.h'
      INTEGER N1,NCLR,N
      REAL*8 RANDCAIN,R
	EXTERNAL RANDCAIN
C
      IF(FAC.GE.1.OR.FAC.LE.0) RETURN
      NCLR=0
      DO 300 N=1,NP
        IF(LOST(N).NE.0.AND.LOST(N).NE.2) THEN
          NCLR=NCLR+1
        ELSEIF(PNAME(N)(1:1).NE.'T') THEN
          R=RANDCAIN()
          IF(R.GE.FAC) THEN
            LOST(N)=1
            NCLR=NCLR+1
          ELSE
            WGT(N)=WGT(N)/FAC
          ENDIF
        ENDIF
 300  CONTINUE
      IF(NCLR.NE.0) THEN
        N1=NP
        CALL DELLOS
        IF(MSGLVL.GE.0) THEN
          WRITE(MSGFL,320) N1,NP
 320      FORMAT(' +++ Number of macro-particles compressed',/,
     %           '        from',I10,' to',I10)
        ENDIF
      ENDIF
      RETURN
      END
