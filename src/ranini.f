C This file provides RANDS and RANINI random number generation
C routines for machines other than SLAC VM
C
C RANINI(INIT)  -- subroutine to initialize random number generator
C
C RANDCAIN()    -- function returning REAL*8 random number
C
C RANDS()       -- same function, returns REAL*4 random number
C
C N.B. Results of RANDS are not defined if RANINI is not called first.
C  (Design decision: did not include initialization test in RANDS for
C   speed reasons.)
C
C Written by Glenn Horton-Smith (GAS), SLAC, 26-Oct-1993
C Based on RANDS ASSEMBLE by Mark Bennett, SLAC
C Adapted from random number generator package "Super Duper"
C  by G. Marsaglia, et al, McGill University, School of Computer Science
C
C Change by K.Yokoya
C   Name change
C     RAN11A -> RANINI
C     RAN11  -> RANDS
C     DRAN11 -> RANDCAIN
C   RANDS  Rewritten as pure REAL*4 routine. 20% faster than the
C          original (conversion from REAL*8)
C   RANDN  Generate N random numbers. If N>3, faster than calling
C          RANDCAIN N times.
C              (benchmark on KEK-SAD (HP workstation) )
C
C------------------- RANINI -------------------------------------
      SUBROUTINE RANINI(INIT)
      IMPLICIT NONE
      INTEGER*4 INIT
C
      COMMON /RAN11C/MCGN,SRGN
      INTEGER*4 MCGN,SRGN
C
      IF (INIT.EQ.0) THEN
         MCGN= 12345
         SRGN= 1073
      ELSE
         MCGN= IOR(INIT,1)
         SRGN= IOR(IAND(INIT,2047),1)
      ENDIF
      RETURN
      END
