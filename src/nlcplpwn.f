         Subroutine ProbabilityWn(No,xi2,lambda,y,phi,
     %    Lambdamax,Ximax,Nmax,Mx,My,Mphi,Mxi,Mlambda)
c            Under the harmnic No Compton scattering, when Xi2 and lambda
c            are given, this sub. can give the Probability(ProbabilityWn)
c            and the value TIntWn, which will give the probablity of 
c            No-th ProbabilityWn Compton Scattering
	   use NLCPLPTAB
	   Implicit none
         integer NMax,Mx,My,Mphi,Mxi,Mlambda
         real*8  Lambdamax,Ximax
	   real*8 y,Phi,phi0,Xi,xi2,lambda,P,ProbWn,ProbWnMax
c           Xi2: Xi**2, Xi is the normalized laser vector potential
c           lambda: 
         integer n,No,Ny,Nphi,Nxi,Nlambda
	   real(8) w1,w2,w3,w4,lambda1 
c            No: ProbabilityWn number of Compton scattering
c            Nxi: the position of Xi2 in Table TIntWn(n,Xi2,lambda)   
c            Nlambda: the position of lambda in Table TIntWn(n,Xi2,lambda)   
c         include 'commonTable01.h'
c         include 'constant.h'
	 Real*8 PI/3.141592653589793238d0/
	REAL*8 RANDCAIN


	   Xi=Xi2*Mxi/Ximax**2
         Nxi=Min(Int(Xi),Mxi-1)
	   w3=Nxi+1-Xi
c            Calculate the position of Xi2 in Table TIntWn(n,Xi2,lambda)   

	   lambda1=lambda*Mlambda/Lambdamax
	   Nlambda=Min(Int(lambda1),Mlambda-1)
	   w4=Nlambda+1-lambda1
          
	   ProbWnMax=Max(TWnMax(No,Nxi,Nlambda),TWnMax(No,Nxi+1,Nlambda),
     %      TWnMax(No,Nxi,Nlambda+1),TWnMax(No,Nxi+1,Nlambda+1))


40	   p=RANDCAIN()
	   y=RANDCAIN()
         if(p.lt.0.or.p.gt.1) print *,'p=',p
         if(y.lt.0.or.y.gt.1) print *,'y=',y
	   phi0=2*RANDCAIN()-1
	   phi=Pi*phi0
	   phi0=abs(phi0)
	   
	   Ny=Min(Int(y*My),My-1)
         w1=Ny+1-y*My
	   Nphi=Min(Int(Phi0*Mphi),Mphi-1)
         w2=Nphi+1-phi0*Mphi
	     if(w1.le.-0.0001.or.w1.ge.1.00001) print *,' w1=',w1
	     if(w2.le.-0.0001.or.w2.ge.1.00001) print *,' w2=',w2
         

	   ProbWn=w1*w2*w3*w4*TWn(No,Ny,Nphi,Nxi,Nlambda)
     %    +w1*(1-w2)*w3*w4*TWn(No,Ny,Nphi+1,Nxi,Nlambda)
     %    +w1*w2*(1-w3)*w4*TWn(No,Ny,Nphi,Nxi+1,Nlambda)
     %    +w1*w2*w3*(1-w4)*TWn(No,Ny,Nphi,Nxi,Nlambda+1)
     %    +w1*(1-w2)*(1-w3)*w4*TWn(No,Ny,Nphi+1,Nxi+1,Nlambda)
     %    +w1*(1-w2)*w3*(1-w4)*TWn(No,Ny,Nphi+1,Nxi,Nlambda+1)
     %    +w1*w2*(1-w3)*(1-w4)*TWn(No,Ny,Nphi,Nxi+1,Nlambda+1)
     %    +w1*(1-w2)*(1-w3)*(1-w4)*TWn(No,Ny,Nphi+1,Nxi+1,Nlambda+1)

     %    +(1-w1)*w2*w3*w4*TWn(No,Ny+1,Nphi,Nxi,Nlambda)
     %    +(1-w1)*(1-w2)*w3*w4*TWn(No,Ny+1,Nphi+1,Nxi,Nlambda)
     %    +(1-w1)*w2*(1-w3)*w4*TWn(No,Ny+1,Nphi,Nxi+1,Nlambda)
     %    +(1-w1)*w2*w3*(1-w4)*TWn(No,Ny+1,Nphi,Nxi,Nlambda+1)
     %    +(1-w1)*(1-w2)*(1-w3)*w4*TWn(No,Ny+1,Nphi+1,Nxi+1,Nlambda)
     %    +(1-w1)*(1-w2)*w3*(1-w4)*TWn(No,Ny+1,Nphi+1,Nxi,Nlambda+1)
     %    +(1-w1)*w2*(1-w3)*(1-w4)*TWn(No,Ny+1,Nphi,Nxi+1,Nlambda+1)
     % +(1-w1)*(1-w2)*(1-w3)*(1-w4)*TWn(No,Ny+1,Nphi+1,Nxi+1,Nlambda+1)

	    ProbWn=ProbWn/ProbWnMax
c	    if(ProbWn.gt.1) print *,' ProbWn=',ProbWn
  
	      if(p.GT.ProbWn) goto 40
	   return
	   end
c------------------------------------------------------------------------------
	    subroutine Interact_Rate(n,Phi,x,Xi,lambda,Wn)
c          Wn is the total W function with unpolarized scattered photon and electron
c          Wn= -An(0)^2/Xi^2 + {1-x+1/(1-x)} {An(1)^2-An(0)*An(2)}
              Implicit none
			integer n
			real*8 	Phi,x,Xi,lambda,Wn					 
	    	real*8 Xi2,v,A(0:2),pe1,pe2
			real*8 alfaP1,alfaP2
			real*8 x1,const,xmax,z
			integer Ncall/0/
              save Ncall 
              Ncall=Ncall+1

c              lambda=4*WL*E0/AMe**2
              x1=x
	        Xi2=Xi**2
	        xmax=n*lambda/(1+Xi2+n*lambda)
	    if(x1.gE.0.and.x1.LE.Xmax*1.0000000001) then
	        if(x1.ge.0.9999*Xmax) x1=0.9999*Xmax
	        v=x1/(1-x1)
	        pe1=1.0
	        pe2=0.0
c	        pe1=sqrt((1+stokes(3))/2)
c	        pe2=0.5*stokes(1)/(1+stokes(3))
	        z=n*lambda/xmax*((xmax-x1)/x1)
c	        z=n*lambda*(1-x1)/x1-(1+Xi2)
              if(z.lt.0) print *,'z=',z
			alfaP1=-sqrt(8.0D0)/lambda*v*sqrt(z)*cos(Phi)
	        alfaP2=v/lambda
c              alfaP1=sqrt(8.)/lambda*sqrt(z)*(pe1*cos(Phi)+pe2*sin(Phi))
c              AlfaP1=alfa1/Xi, AlfaP2=Alfa2/Xi


	        call functionA(n,Xi,alfaP1,alfaP2,A)
              
C                 A(0:2) are the An(i) function, i=0,1,2. and An(0) is divided by Xi
 
	        const=A(1)**2-A(0)*Xi*A(2)
			Wn=-A(0)**2+(1.0-x1+1.0/(1-x1))*const
c              Where Wn is divded by Xi^2, the correct Wn is Wn=-A0**2+Xi^2*(1-x1+1/(1-x1))*const
		else
               Wn=0.0
          endif
	end
c------------------------------------------------------------------------
	 subroutine InteractRate1(n,Phi,x,Xi,lambda,Wn,Wn1)

c          Wn is the total W function with unpolarized scattered photon and electron
c          Wn= -An(0)^2/Xi^2 + {1-x+1/(1-x)} {An(1)^2-An(0)*An(2)}
        Implicit none
	  integer n
	  real*8 	Phi,x,Xi,lambda,Wn,Wn1					 
	  real*8 Xi2,v,A(0:2),pe1,pe2
 	  real*8 alfaP1,alfaP2,alfa1,alfa2
	  real*8 x1,const,xmax,z,zp,f
	  Real*8 error/1.0E-12/
	  integer Ncall/0/
        save Ncall 
        Ncall=Ncall+1
c        lambda=4*WL*E0/AMe**2

	  pe1=1.0
		pe2=0.0;  !  pe1=sqrt((1+stokes(3))/2); pe2=0.5*stokes(1)/(1+stokes(3))
	  Xi2=Xi**2
!	  if(lambda.le.1.0E-8) lambda=1.0E-8
	  xmax=n*lambda/(1+Xi2+n*lambda)
	  x1=x
        
	  if(x1.gE.0.and.x1.LE.Xmax) then
	  if(x.le.1.0E-16) x1=min(xmax,1.0E-16)
	  if(x1.ge.0.9999999*Xmax) x1=0.9999999*Xmax

	  v=x1/(1-x1)
	  z=n*lambda/v-(1.0+Xi2)
		zp=(n*lambda-v*(1.0+Xi2))*v  !	  zp=v^2*z
	  if(z.lt.0) print *,'lambda=',lambda,'x=',x,'z=',z

	  alfaP2=v/lambda
		alfa2=alfaP2*xi2
	  alfaP1=-sqrt(8.0d0)*(sqrt(zp)/lambda)*cos(Phi)
		alfa1=alfaP1*xi
!        AlfaP1=alfa1/Xi, AlfaP2=Alfa2/Xi**2

	  f=Xi2-n*lambda/v+z*(cos(Phi))**2  ! f=Xi2-n/alfaP2+(alfaP1/alfaP2)**2/8

	    call functionA(n,Xi,alfaP1,alfaP2,A)

	    const=A(1)**2-A(0)*Xi*A(2)
		Wn=-A(0)**2+(1-x1+1/(1-x1))*const
          Wn1=f*A(0)**2+0.5*(3-x1+1/(1-x1))*const
c              Wn2=-(1+f)*A(0)**2+0.5*(-1-x1+1/(1-x1))*const
c              Where Wn, Wn1 and Wn2 is divded by Xi^2,
c              the correct Wn is Wn=-A0**2+Xi^2*(1-x1+1/(1-x1))*const, and so on.

		else
               Wn=0.0
               Wn1=0.0
          endif
		
		
	end
