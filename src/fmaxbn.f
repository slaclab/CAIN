      SUBROUTINE FMAXBN(N,F,J1,F1,J2,F2,L,X)
C  Find the largest two
C  If L><0, F(i) is divided by the bin width X(i)-X(i-1)
      IMPLICIT NONE
      INTEGER N,J1,J2,L
      REAL*8 FJ,F1,F2,F(N),X(0:N)
      INTEGER J
C
      F1=F(1)
      IF(L.NE.0) F1=F1/(X(1)-X(0))
      F2=F1
      J1=1
      J2=1
      IF(N.LE.1) RETURN
      DO 200 J=1,N
        FJ=F(J)
        IF(L.NE.0) FJ=FJ/(X(J)-X(J-1))
        IF(FJ.GE.F2) THEN
          IF(FJ.GE.F1) THEN
            F2=F1
            F1=FJ
            J2=J1
            J1=J
          ELSE
            F2=FJ
            J2=J
          ENDIF
        ENDIF
 200  CONTINUE
      RETURN
      END

