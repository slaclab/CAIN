      FUNCTION IFLRKN(N,LR,KIN,LGEN,IGEN,LLOST,ITYP)
C  LR       : right, left
C  KIN      : photon, electron, positron
C  LGEN,IGEN:  if LGEN>0, logical condition
C  ITYP     : normal, inc.particle, test particle
C  LLOST    : 0: not include lost particles
C             1: only include lost particles with LOST=2
C             There is no way to include both
C             Lost particles with LOST=1 can never be included
	USE BEAMCM
      IMPLICIT NONE
      INTEGER IFLRKN,N,LR(2),KIN(3),LGEN,IGEN,LLOST,ITYP(3)
C      INCLUDE 'include/beamcm.h'
      INTEGER LR1,II,ITY,IFRELI
      IFLRKN=0
      IF(N.LE.0.OR.N.GT.NP) RETURN
	IF(LLOST.EQ.0) THEN
        IF(LOST(N).NE.0) RETURN
	ELSE
	  IF(LOST(N).NE.2) RETURN
	ENDIF
      LR1=1
      IF(EP(3,N).LT.0) LR1=2
      IF(LR(LR1).EQ.0) RETURN
      IF(KIN(KIND(N)).EQ.0) RETURN
      IF(PNAME(N).EQ.'    ') THEN
        ITY=1
      ELSEIF(PNAME(N)(1:1).EQ.'I') THEN
        ITY=2
      ELSEIF(PNAME(N)(1:1).EQ.'T') THEN
        ITY=3
      ELSE
        RETURN
      ENDIF
      IF(ITYP(ITY).EQ.0) RETURN
      IF(LGEN.GE.1.AND.LGEN.LE.6) THEN
        II=IFRELI(GEN(N),LGEN,IGEN)
        IF(II.EQ.0) RETURN
      ENDIF      
      IFLRKN=1
      RETURN
      END

