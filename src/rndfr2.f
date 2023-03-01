C************************ RNDFR2 *************************************
      SUBROUTINE RNDFR2(A1,B1,A2,B2,X,C,S)
C Generate random number x satisfying the distribution function
C   df/dx =  1 + A1*cos(x)+B1*sin(x) + A2*cos(2x)+B2*sin(2x)
C                     (0<=x<2*pi)
C Positive-definiteness of df/dx is not checked.
C Input:
C  A1,B1,A2,B2 : The coefficients defined above.
C Output:
C  X
C  C,S   = cos(X), sin(X)
C
      IMPLICIT NONE
      REAL*8 X,C,S,A1,B1,A2,B2
      REAL*8 P,C0,C2,S2,F1,F2
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      REAL*8 PI/3.14159 26535 89793 238D0/
      REAL*8 PI2/6.28318 53071 79586 477D0/
      X=RANDCAIN()*PI2
      C=COS(X)
      S=SIN(X)
      F1=-(A1*C+B1*S)
      P=RANDCAIN()
      IF(P.LT.F1) GOTO 220
      C2=C**2-S**2
      S2=2*C*S
      F2=1+A2*C2+B2*S2
      IF(P.LT.F2) GOTO 260
      C0=-S
      S=C
      C=C0
      X=X+PI/2
      IF(P.LT.2+A1*C+B1*S) GOTO 240
  220 C=-C
      S=-S
      X=X+PI
  240 IF(X.GE.PI2) X=X-PI2
  260 RETURN
      END
