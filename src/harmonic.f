         Subroutine harmonic(E0,WL,xi2,lambda,Dt,No,PROB,
     %	   Lambdamax,Ximax,Nmax,Mx,My,Mphi,Mxi,Mlambda)
c          When Xi2 and lambda are given, this sub. will give the harmonic number (No)
c          and the value TIntWn, which will give the probablity of Noth harmonic
c          Compton Scattering, we should notic that TIntW alreadly included const factor
	   use NLCPLPTAB
	   Implicit none
	   integer No
	   real*8 E0,WL,xi2,lambda,Dt,PROB
c           initial electron energy E0 and photon energy WL
c           Xi2: =Xi**2, Xi is the normalized laser vector potential
c	      lambda=4*EE(n)*WL/AME**2
c           Dt is the time interval
c           No: harmonic number of Compton scattering

         integer NMax,Mx,My,Mphi,Mxi,Mlambda
	   real*8  Lambdamax,Ximax
	   real*8 x
	   REAL*8 RANDCAIN
         integer n,Nxi,Nlambda 
c            n: harmonic number of Compton scattering
c            Nxi: the position of Xi2 in Table TIntWn(n,Xi2,lambda)   
c            Nlambda: the position of lambda in Table TIntWn(n,Xi2,lambda)   
	   real*8 A1,A2,DIntWn
	   real*8 q0,const,Xi,P1,P2,lambda1
	   integer ncall/0/
	   save ncall
	 Real*8 re0/2.82E-15/,AME/0.511006E6/,lambdaC/3.861593e-13/,
     %    hc/1.24E-6/,alfa/0.0072992D0/,PI/3.141592653589793238d0/,
     %    sigma0/6.662186563E-29/,Rerror/1.0E-6/

c         include 'constant.h'
c         include 'commonTable01.h'
	   ncall=ncall+1

			q0=E0+Xi2*WL/lambda
			const=alfa*AME*Xi2/q0*(Dt/LambdaC)


	   A1=0.
	   A2=0.
         x=RANDCAIN()
         Xi=Xi2*Mxi/Ximax**2
	   Nxi=Min(Int(Xi),Mxi-1)
	   P1=Nxi+1-Xi
c            Calculate the position of Xi2 in Table TIntWn(n,Xi2,lambda)   
	   
	   lambda1=lambda*Mlambda/Lambdamax
	   Nlambda=Min(Int(lambda1),Mlambda-1)
	   P2=Nlambda+1-lambda1
	   No=0
	   do n=1,Nmax
            if(Nxi.Gt.(Mxi+1).or.Nlambda.GT.(Mlambda+1)) print *,
     %		  'Nxi=',Nxi,'Nlambda=',Nlambda
		  DIntWn=P1*P2*TintWn(n,Nxi,Nlambda)*Const
     %           +P1*(1-P2)*TintWn(n,Nxi,Nlambda+1)*Const
     %           +(1-P1)*P2*TintWn(n,Nxi+1,Nlambda)*Const
     %           +(1-P1)*(1-P2)*TintWn(n,Nxi+1,Nlambda+1)*Const
		  A2=A1+DIntWn
c	      if(A2.ge.1) print *,' A2=',A2
	      if(No.eq.0) then
			  if((x.GT.A1).and.(x.LE.A2)) then
				No=n
			  endif
	      endif
	      A1=A2
	   enddo
	   Prob=A2

	   return
	   end
	   
