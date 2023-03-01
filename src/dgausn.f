C******************** DGAUSN *******************************************
      SUBROUTINE DGAUSN(X,N)
C  GENERATE GAUSSIAN RANDUM DEVIATES WITH ZERO MEAN AND UNIT S.D.
C   OUTPUT: (X(I),I=1,N)  RANDOM DEVIATES. R*8.
C  Called routines
C   RANDCAIN    real*8 uniform random number in (0,1)
      IMPLICIT NONE
      INTEGER N,I
      REAL*8 X(N),P,P1,P2,Q,T
C------------------------
      REAL*8 PP1/0.334624883253D0/, QQ2/0.090230446775D0/,
     1       QQ3/0.049905685242D0/, QQ4/0.027852994157D0/,
     2       QQ5/0.015645650215D0/
      REAL*8 A3/ 4.5585614D+01/, A2/ 2.1635544D+00/, A1/ 2.7724523D+00/,
     1     A0/ 2.5050240D+00/,
     2     B4/ 4.0314354D+02/, B3/-2.7713713D+02/, B2/ 7.9731883D+01/,
     3     B1/-1.4946512D+01/, B0/ 2.2157257D+00/,
     4     C4/ 4.1394487D+03/, C3/-1.5585873D+03/, C2/ 2.4648581D+02/,
     5     C1/-2.4719139D+01/, C0/ 2.4335936D+00/,
     6     D4/ 4.0895693D+04/, D3/-8.5400893D+03/, D2/ 7.4942805D+02/,
     7     D1/-4.1028898D+01/, D0/ 2.6346872D+00/,
     8     E4/ 3.9399134D+05/, E3/-4.6004775D+04/, E2/ 2.2566998D+03/,
     9     E1/-6.8317697D+01/, E0/ 2.8224654D+00/,
     O     F0/-8.1807613D-02/, F1/-2.8358733D+00/, F2/ 1.4902469D+00/
      REAL*8 XMAX/6.0D0/
C------------------------
      CALL RANDN(X,N)
      DO 200 I=1,N
      P=X(I)-0.5D0
      P1=ABS(P)
      IF(P1.GE.PP1) GOTO 120
      P2=P**2
      X(I)=(((A3*P2+A2)*P2+A1)*P2+A0)*P
      GOTO 200
 120  Q=0.5D0-P1
      IF(Q.GE.QQ2) THEN
        X(I)=(((B4*Q+B3)*Q+B2)*Q+B1)*Q+B0
      ELSEIF(Q.GE.QQ3) THEN
        X(I)=(((C4*Q+C3)*Q+C2)*Q+C1)*Q+C0
      ELSEIF(Q.GE.QQ4) THEN
        X(I)=(((D4*Q+D3)*Q+D2)*Q+D1)*Q+D0
      ELSEIF(Q.GE.QQ5) THEN
        X(I)=(((E4*Q+E3)*Q+E2)*Q+E1)*Q+E0
      ELSE
        IF(Q.LE.0D0) THEN
          X(I)=XMAX
        ELSE
          T=SQRT(-2D0*LOG(Q))
          X(I)=T+F0+F1/(F2+T)
        ENDIF
      ENDIF
      IF(P.LT.0D0) X(I)=-X(I)
  200 CONTINUE
      RETURN
      END
