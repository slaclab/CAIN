      SUBROUTINE BBQMOM(L,MX,MY,Q)
C Unvectorizable version
	USE BBPKCM
      IMPLICIT NONE
C      INCLUDE 'include/bbpkcm.h'
      INTEGER L,MX,MY
      REAL*8 Q(MX,MY)
      INTEGER I,J,N,N1
      REAL*8 X,Y,X1,Y1,DX1H,DY1H,A,A1,B,B1
      COMPLEX*16 C,Z11,Z12,Z21,Z22,ZZ11,ZZ12,ZZ21,ZZ22
C
C Harmonic expansion around the center-of-mass
C   dPhi   dPhi          R0**(m+1)   
C   ---- -i---- = - Sum -------------QMOM1(m) (over m=0 to infty)
C    dx     dy          (X+iY)**(m+1)
C         X=x-Xcm,  Y=y-Ycm
C
C     QMOM1(m)= integral [(x+iy)/R0]**m Rho(x,y) dxdy /R0
C
C   Valid for x**2+y**2 >> NX*DXY(1), NY*DXY(2)
C

      IF(LEL.EQ.0) RETURN
C
      DO 220 N=0,NMOM
        QMOM(N,1,L)=0
 220  CONTINUE
      X1=XYMIN(1,L)-0.5D0*DXY(1,L)-XYCM(1,L)
      Y1=XYMIN(2,L)-0.5D0*DXY(2,L)-XYCM(2,L)
      DX1H=DXY(1,L)/(2*R00(L))
      DY1H=DXY(2,L)/(2*R00(L))
      C=DCMPLX(0D0,-1/(4*DX1H*DY1H*R00(L)))
      DO 400 I=1,NXY(1)
        X=(I*DXY(1,L)+X1)/R00(L)
        DO 380 J=1,NXY(2)
          Y=(J*DXY(2,L)+Y1)/R00(L)
          ZZ11=DCMPLX(X+DX1H,Y+DY1H)
          ZZ21=DCMPLX(X-DX1H,Y+DY1H)
          ZZ12=DCMPLX(X+DX1H,Y-DY1H)
          ZZ22=DCMPLX(X-DX1H,Y-DY1H)
          Z11=ZZ11*C*Q(I,J)
          Z21=ZZ21*C*Q(I,J)
          Z12=ZZ12*C*Q(I,J)
          Z22=ZZ22*C*Q(I,J)
          DO 360 N=0,NMOM
            Z11=Z11*ZZ11
            Z21=Z21*ZZ21
            Z12=Z12*ZZ12
            Z22=Z22*ZZ22
            B1=(N+1)*(N+2)
            QMOM(N,1,L)=QMOM(N,1,L)+(Z11-Z21-Z12+Z22)/B1
 360      CONTINUE
 380    CONTINUE
 400  CONTINUE
      IF(LEL.EQ.1) RETURN
C
C Expansion using elliptic coordinate
C  dPhi   dPhi                                                    
C  ---- -i---- = - Sum exp[-(m+1)(u-u0+iv)]*QMOM2(m)                                                         
C   dx     dy                    over m=0 to infty
C             2                                    sinh[(m+1)(u+iv)]
C   QMOM2(m)=---integral dxdy Rho(x,y)exp[-(m+1)u0]-----------------
C             L                                       sinh(u+iv)
C      u0 is introduced only to avoid overflow/underflow.
C   Here, x+iy and u+iv are related as
C      cosh(u+iv)=(x+iy)/L
C         L = Sqrt[(Wx**2-Wy**2)/2]
C         u0= (1/2)*log[(Wx+Wy)/(Wx-Wy)]
C         Wx=NXY(1)*DXY(1),  Wy=NXY(2)*DXY(2)
C  Valid outside the charge region (wider region than the
C  harmonic expansion)
C
      A=2*R00(L)/EL(L)/EU0
      B=(EL(L)/(2*R00(L)))**2
      A1=1
      DO 440 N=0,NMOM
        A1=A1*A
        B1=A1
        QMOM(N,2,L)=0
        DO 420 I=0,N/2
          N1=N-2*I
          QMOM(N,2,L)=QMOM(N,2,L)+B1*QMOM(N1,1,L)
          IF(N.NE.0) B1=-DFLOAT(N1*(N1-1))/DFLOAT((I+1)*(N-I))*B*B1
 420    CONTINUE
 440  CONTINUE
      RETURN
      END
