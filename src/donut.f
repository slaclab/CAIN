      SUBROUTINE DONUTTABLE(LSR,FMAX,MSGLVL,MSGFL,IRTN)
	USE LASRDATA
	IMPLICIT NONE
	INTEGER LSR,MSGLVL,MSGFL,IRTN
	REAL*8 FMAX
	INTEGER NR,NZ,NN,ISTAT,IR,IZ,N,IWARN,IRTN1
	REAL*8 RMAX,ZMIN,ZMAX,R,Z,DR,DZ,DF,AK,SIGMIN
	COMPLEX*16 F(3)
	INCLUDE 'include/lasrcm.h'
	REAL*8 PI/3.141592653589793238D0/

C		SPAR(1)=a, SPAR(2)=b, SPAR(3)=f, SPAR(4)=sig0
	CALL CPUTIM('DONUTTABLE',1)
	IF(LSPAR(LSR).NE.DONUT_SHAPE) THEN
	  IRTN=10
	  WRITE(MSGFL,100) LSR
100     FORMAT(' (SUBR.DONUTTABLE) Laser #',I5,' is not donut type.')
	  GOTO 500
	ENDIF
	AK=1/WLLSR(LSR)
	CALL SETDONUT(2*PI*WLLSR(LSR),SPAR(1,LSR),SPAR(2,LSR),
     %   SPAR(3,LSR),SPAR(4,LSR))
	RMAX=SPAR(5,LSR)
	ZMAX=ZETAMAX(LSR)
	ZMIN=-ZMAX
C  Estimate the required step size for linear interpolation
	SIGMIN=SPAR(3,LSR)/(2*AK*SPAR(4,LSR))
	DR=0.1D0*SIGMIN
	DZ=0.1D0*SPAR(3,LSR)*SIGMIN/SPAR(4,LSR)
	IWARN=0
	NR=INT(RMAX/DR)+1
C         avoid huge table
	IF(NR.GT.501) THEN
	  IWARN=IWARN+1
	  NR=501
	ENDIF
	NZ=2*INT((ZMAX-ZMIN)/DZ/2)+1
	IF(NZ.GT.501) THEN
	  IWARN=IWARN+2
	  NZ=501
	ENDIF
	DR=RMAX/(NR-1)
	DZ=(ZMAX-ZMIN)/(NZ-1)
	NN=NR*NZ
	NLSRFL(LSR)=0
	ALLOCATE(LSRDT(1,LSR)%PTP(1:NN),STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IRTN=100
	  WRITE(MSGFL,200) LSR
200     FORMAT(' (SUBR.DONUTTABLE) Memory alloc failed for Laser #',I5)
	  GOTO 500
	ENDIF
	ALLOCATE(LSRDT(1,LSR)%PTPNV(2,1:NN),STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  DEALLOCATE(LSRDT(1,LSR)%PTP,STAT=ISTAT)
	  IRTN=100
	  WRITE(MSGFL,200) LSR
	  GOTO 500
	ENDIF
	IF(MSGLVL.GE.1) THEN
	  WRITE(MSGFL,220) LSR,RMAX,DR,NR,ZMIN,ZMAX,DZ,NZ
220     FORMAT(' --- Table of Donut-shape profile for laser #',I2,/,
     %   '   ',9X,'0<r<',1PD10.3,'m  dr=',1PD9.3,'m  Nr=',I4,/,
     %   '   ',1PD10.3,'<z<',1PD10.3,'m  dz=',1PD9.3,'m  Nz=',I4)
      ENDIF
	IF(IWARN.GT.0.AND.MSGLVL.GE.0) THEN
	  IF(IWARN.EQ.1) THEN
	    WRITE(MSGFL,240) 'RMAX'
	  ELSEIF(IWARN.EQ.2) THEN
	    WRITE(MSGFL,240) 'ZMAX'
	  ELSE
	    WRITE(MSGFL,240) 'RMAX and ZMAX'
240       FORMAT('     ',A,' too large. Coarse mesh used. ',
     %       'Maybe inaccurate.')
	  ENDIF
	ENDIF
	NLSRFL(LSR)=1
	LSRDT(1,LSR)%ITYPE=TYPERZ
	LSRDT(1,LSR)%NDIM=2
	LSRDT(1,LSR)%NVAL(1)=NR
	LSRDT(1,LSR)%NVAL(2)=NZ
	LSRDT(1,LSR)%NDATA=NN
	LSRDT(1,LSR)%NVDEFINED=.TRUE.
	LSRDT(1,LSR)%VALMM(1,1)=0D0
	LSRDT(1,LSR)%VALMM(2,1)=RMAX
	LSRDT(1,LSR)%VALMM(1,2)=ZMIN
	LSRDT(1,LSR)%VALMM(2,2)=ZMAX
	LSRDT(1,LSR)%DVAL(1)=DR
	LSRDT(1,LSR)%DVAL(2)=DZ
	LSRDT(1,LSR)%TWISTCOORD=.FALSE.
	DO 300 IZ=1,NZ
	  Z=ZMIN+DZ*(IZ-1)
	  DO 280 IR=1,NR
	    R=(IR-1)*DR
	    N=IR+(IZ-1)*NR
	    CALL DONUTPHI(Z,R,F,IRTN1)
	    IF(IRTN1.NE.0.AND.MSGLVL.GE.0) THEN
	      WRITE(MSGFL,260) Z,R
260         FORMAT(' Donut integration poor convergence for',/,
     %             '   z=',1PD10.3,'  r=',1PD10.3)
	    ENDIF
	    LSRDT(1,LSR)%PTP(N)=DREAL(F(1))**2+DIMAG(F(1))**2
	    FMAX=MAX(FMAX,LSRDT(1,LSR)%PTP(N))
cc	    F(3)=F(3)+DCMPLX(0D0,AK)*F(1)
C            The above line replaced by the following 2 lines (May.15.2001)
	    F(2)=F(2)/F(1)
	    F(3)=F(3)/F(1)+DCMPLX(0D0,AK)
	    DF=SQRT(DIMAG(F(2))**2+DIMAG(F(3))**2)
	    LSRDT(1,LSR)%PTPNV(1,N)=DIMAG(F(2))/DF
	    LSRDT(1,LSR)%PTPNV(2,N)=DIMAG(F(3))/DF
280     CONTINUE
300   CONTINUE
      IRTN=0
500	CALL CPUTIM('DONUTTABLE',2)
	RETURN
	END
C    Donut-shape Laser Profile
C  parameters
C        L :  wavelength of the laser (m)
C        a :  outer radius of axicon mirror (m)
C        b :  inner radius of axicon mirror (m)
C        f :  focal length (m)
C        s0:  r.m.s. radius of input laser (m)
C  Formula
C     When the input laser field is
C          Real( E0*exp[-r^2/4/s0^2]*exp[i*k*(z-t)]*F(z-t) )
C     the field at (z,r,t) (z is measured from the focal point)
C     is given by
C       E(z,r,t)=Real( E0*Phi(z,r)*exp[i*k*(z-t+const)]*F(z-t) )
C     where
C           k
C    Phi= ---- * integral P * Q * J0[kr(x+b)/(z+f)] dx   over 0 < x < a-b
C          z+f
C          P = Sqrt[x*(x+b)] exp[-x^2/4/s0^2]
C          Q = exp[ -ik(x+b)^2/2*z/f/(z+f) + ikr^2/2/(z+f) ]
C             k= 2*pi/L
C      The power integrated over the transverse plane z=const is
C      proportional to
C        integral Abs[Phi(z,r)]^2 rdr  over 0<r<infty
C            = s0**2 * [1-exp(-(a^2-b^2)/2/s0^2)]
C  Usage
C    (1)   Define the parameters by
C               CALL  SETDONUT(L,a,b,f,s0)
C    (2)   For each (z,r),
C               CALL DONUTPHI(z,r,F,FLIM)
C          where F and FLIM are COMPLEX*16 array of length 3.
C               F(1)=Phi
C               F(2)=dPhi/dz
C               F(3)=dPhi/dr
C          FLIM is the limit of F for b->0 and a->infty.
C          (normally, not needed)
C  Files
C    * donutpar.h must be in the same directory
C    * deint.f, dbesjy.f needed
C
	SUBROUTINE SETDONUT(WL,A0,B0,FL0,SIG00)
	IMPLICIT NONE
	REAL*8 WL,FL0,A0,B0,SIG00
	INCLUDE 'include/donutpar.h'
	REAL*8 PI/3.141592653589793238D0/
	FL=FL0
	A=A0
	B=B0
	SIG0=SIG00
	AK=2*PI/WL
	AB1=A-B
	RETURN
	END

	SUBROUTINE DNTINTGRNT(X,NF,F)
	IMPLICIT NONE
	INCLUDE 'include/donutpar.h'
	INTEGER NF
	INTEGER K
	REAL*8 X,F(6)
	REAL*8 X0,C1,C2,J0,J1,ZF
	COMPLEX*16 DPHI(3),C
	REAL*8 DBESJY
	C1=(X/(2*SIG0))**2
	IF(C1.GT.100) THEN
	  DO 100 K=1,6
	    F(K)=0
100     CONTINUE
C	  DPHI(1)=0
C	  DPHI(2)=0
C	  DPHI(3)=0
	  RETURN
	ENDIF
	X0=X+B
	ZF=Z+FL
	C2=AK/2*(-X0**2*Z/(FL*ZF)+R**2/ZF)
	C=SQRT(X*X0)*EXP(DCMPLX(-C1,C2))
	J0=DBESJY(0,1,AK*R*X0/ZF)
	J1=DBESJY(1,1,AK*R*X0/ZF)
	DPHI(1)=J0*C
	DPHI(2)=DCMPLX(0D0,-AK/2*(X0**2+R**2)/ZF**2)*DPHI(1)
     %    +AK*R*X0/ZF**2*J1*C
      DPHI(3)=DCMPLX(0D0,AK*R/ZF)*DPHI(1)-AK*X0/ZF*J1*C
	DO 200 K=1,3
	  F(2*K-1)=DREAL(DPHI(K))
	  F(2*K)=DIMAG(DPHI(K))
200   CONTINUE
	RETURN
	END

	SUBROUTINE DONUTPHI(Z0,R0,F,IRTN)
C	SUBROUTINE DONUTPHI(Z0,R0,F,FLIM)
	IMPLICIT NONE
	INTEGER IRTN
	REAL*8 Z0,R0
	COMPLEX*16 F(3)
C	COMPLEX*16 FLIM(3)
	INCLUDE 'include/donutpar.h'
C
C     k
C   ----- * integral P * Q * J0[kr(x+b)/(z+f)]  dx    over 0 < x < a-b
C    z+f
C          P = Sqrt[x*(x+b)] exp[-x^2/4/s0^2]
C          Q = exp[ -ik(x+b)^2/2*z/f/(z+f) + ikr^2/2/(z+f) ]
C      k= 2*pi/(wavelength)
C      s0=SIG0
C
C  Note:
C    *   Above expression does not include the factor exp[i(k*z-omega*t)]
C    *   F(2) and F(3) is the derivative w.r.t. z and r.
C
C  In the limit b->0, a->infty, the result is (returned as FLIM)
C
C    2*k*s0^2            ikr^2     (krs0)^2
C    -------- * Q * exp[ ------ - ----------*Q]
C      z+f               2(z+f)     (z+f)^2
C      where
C                 2ikzs0^2
C       1/Q = 1 + ----------
C                  (z+f)f
C
	INTEGER NF
	PARAMETER (NF=6)
	REAL*8 G(NF),EPS(NF),ZF
	COMPLEX*16 CQ,DCQDZ,CEX
	INTEGER K,MSG,LCONV(NF)
	EXTERNAL DNTINTGRNT
	
	Z=Z0
	R=R0
	MSG=0
	DO 200 K=1,NF
	  IF(K.LE.2) THEN
	    EPS(K)=1D-5
	    LCONV(K)=3
	  ELSE
	    LCONV(K)=0
	    EPS(K)=1D60
	  ENDIF
200   CONTINUE
	CALL DEINT(DNTINTGRNT,0D0,AB1,NF,LCONV,EPS,0,G,MSG,IRTN)
	ZF=Z+FL
	DO 300 K=1,3
	  F(K)=DCMPLX(G(2*K-1),G(2*K))*AK/ZF
300   CONTINUE
      F(2)=F(2)-F(1)/ZF
C	CQ=1/DCMPLX(1D0,2*AK*SIG0**2*Z/(ZF*FL))
C	DCQDZ=DCMPLX(0D0,-2*AK*SIG0**2/ZF**2)*CQ**2
C	CEX=DCMPLX(0D0,AK*R**2/(2*ZF))-(AK*R*SIG0/ZF)**2*CQ
C	FLIM(1)=AK*SIG0**2*2/ZF*CQ*EXP(CEX)
C      FLIM(2)=(-1/ZF+DCQDZ/CQ
C     %         + (AK*R*SIG0/ZF)**2*(2/ZF*CQ-DCQDZ))*FLIM(1)
C	IF(R.EQ.0) THEN
C	  FLIM(3)=0
C	ELSE
C	  FLIM(3)=2/R*CEX*FLIM(1)
C	ENDIF
	RETURN
	END
