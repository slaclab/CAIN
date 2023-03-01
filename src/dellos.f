      SUBROUTINE DELLOS
	USE BEAMCM
C Eliminate lost particles (except LOST=2) in the list
C Caution:  particle numbering and NP will change!
      IMPLICIT NONE
C      INCLUDE 'include/beamcm.h'
      INTEGER N
C
      IF(NP.LE.0) RETURN
      N=0
 200  N=N+1
      IF(LOST(N).NE.0.AND.LOST(N).NE.2) THEN
        IF(N.NE.NP) THEN
 220      IF(LOST(NP).NE.0.AND.LOST(NP).NE.2) THEN
            NP=NP-1
            IF(NP.GT.N) GOTO 220
          ELSE
            CALL COPYBM(NP,N)
          ENDIF
        ENDIF
        NP=NP-1
      ENDIF
      IF(N.LT.NP) GOTO 200
 300  RETURN
      END

      
