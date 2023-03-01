      FUNCTION BBPHI(I,J,L,K)
	USE BBPKCM
      IMPLICIT NONE
	INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/bbpkcm.h'
      INTEGER I,J,L,K
      REAL*8 BBPHI
C
      BBPHI=0
      IF(I.LE.0.OR.I.GT.NXY(1).OR.J.LE.0.OR.J.GT.NXY(2)) GOTO 910
      IF(L.LE.0.OR.L.GT.2) GOTO 920
      IF(K.LT.0.OR.K.GT.3) GOTO 930
      IF(K.EQ.0) THEN
        BBPHI=PHI(I,J,L)
      ELSE
        BBPHI=DPHI(K,I,J,L)
      ENDIF
      RETURN
 910  WRITE(MSGFL,915)  
 915  FORMAT(' (FUNC.BBPHI) The point out of the mesh region.')
      RETURN
 920  WRITE(MSGFL,925) 
 925  FORMAT(' (FUNC.BBPHI) Wrong beam number. Must be 1 or 2.')
      RETURN
 930  WRITE(MSGFL,935) 
 935  FORMAT(' (FUNC.BBPHI) Wring function id number. Must be 0 to 3.')
      RETURN
      END
