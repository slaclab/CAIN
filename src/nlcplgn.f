      
      Subroutine NLCPLGN(PE1,WL,EV,HE1,stokes1,Xi2,DT,PMAX,
     %	NPh,PE2,PG,Stokes2,Prob,WGT,ISPIN,IRTN)

!	  CALL NLCPLGN(PE1,WL,NL,HE1,HL,PD,DT,PMAX,ISPIN1,
!     %    NPH,PE2,HE2,PG,HG,PROB,WGT,IRTN)



C Input
C   PE1(0:3)  Initial electron 4-momentum (eV/c)
C   WL      Laser photon energy (eV)
C   EV(1:3,3)   Unit vector along the laser direction
C   EV(1:3,1), EV(1:3,2)  Define basis vector for the laser Stokes parameter.
C   HE1     initial electron helicity
C   stokes1  Stokes parameters of the initial Laser, it will describe
c           the laser spin  
c   Xi2    laser field parameters, which is relate to the 
c           laser normlaized vector potential
C   DT      Time interval times the velocity of light, which unit is meter
C   PMAX    Maximum probability of radiation in one time step.If the probability
c           turns out to be >=PMAX, return with no radiation and IRTN=100.
C           PMAX smaller than 0.1 is recommended.
C   ISPIN   Flag to take into account polarization effects.
C Output
C   NPH     Harmonic Number of absorbed laser photon (no radiation if NPH=0)
C   PE2(0:3)  Final electron 4-momentum (eV/c)
C   PG(0:3)   Final photon 4-momentum (eV/c)
C   HE2     Final electron helicity
C   Stokes2 Final photon stokes parameter
C   PROB    Calculated event probability in the given time interval.
C   WGT     Event weight. 0<=WGT<=1.
C   IRTN    Return code
C    

	 use NLCPLPTAB
	 Implicit none
       Real*8  PE1(0:3),WL,NL(3),EV(3,3),HE1,stokes1(3)
	 Real*8  Xi2,PD,DT,PMAX,XiMax,LambdaMax
       Integer Nmax,Mx,Mlambda,NPh,ISPIN,IRTN
	 Real*8 PE2(0:3),PG(0:3),HE2,Stokes2(3),x,y,Phi,Prob,WGT
       Real*8 p1(0:3),k1(0:3),lambda
	 INTEGER I
	 Real*8 ScalarP4
	 Real*8 re0/2.82E-15/,AME/0.511006E6/,lambdaC/3.861593e-13/,
     %    hc/1.24E-6/,alfa/0.0072992D0/,PI/3.141592653589793238d0/,
     %    sigma0/6.662186563E-29/,Rerror/1.0E-6/
       Include 'include/nlcpcm.h'
!       Include '../include/nlcpcm.h'

	 Nmax=Mph
	 Mx=My
	 Mlambda=MLM
       Ximax=Sqrt(XISQMX)
	 LambdaMax=LMMAX
c              XiMax     maximum of Xi^2
c              LambdaMax maximum of  lambda parmater
c              Nmax      Maximum number of laser photons to be absorbed in one process. 
c              Mx        Number of abscissa for x parameter. Default=20.
c              My        Number of abscissa for finial energy. Default=20.
c              Mphi      Number of abscissa for azimuth angle. Default=20.
c              Mxi       Number of abscissa for xi parameter. Default=20.
c              Mlambda   Number of abscissa for lambda parameter. Default=20.



       PG=0.0
	 p1=PE1
	 NL=EV(1:3,3)
	 k1(0)=WL
	 k1(1)=WL*NL(1)
	 k1(2)=WL*NL(2)
	 k1(3)=WL*NL(3)
	 WGT=0

	 lambda=2*ScalarP4(p1,k1)/AME**2

	 call harmonic(PE1(0),WL,Xi2,lambda,Dt,NPh,Prob,
     %       Lambdamax,Ximax,Nmax,Mx,My,Mphi,Mxi,Mlambda)
!       if(prob.gt.0.1) print *,'prob=',prob
c            Where NPh is the harmonic number of Compton Scattering
c            Prob is the integral of Wn, which alreadly included the factor 

       if(Nph.eq.0) goto 100
	 call ProbabilityWn(NPh,XI2,lambda,y,phi,
     %      Lambdamax,Ximax,Nmax,Mx,My,Mphi,Mxi,Mlambda)  !  Phi: from -Pi to Pi
                                             
	 call Kinematics(NPh,XI2,lambda,y,x,phi,p1,k1,PE2,PG,stokes1)

       call Polarization(Nph,XI2,lambda,x,phi,stokes1,Stokes2)

	 WGT=1

100    return
	 end
		      
