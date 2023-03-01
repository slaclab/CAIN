      Subroutine Polarization(Nph,XISQ,lambda,x,phi,stokes1,stokes2)
	use NLCPLPTAB
	Implicit none
	integer NPh
	Real(8) E0,WL,XISQ,lambda,x,phi,stokes1(3),stokes2(3)
	real(8) Xi2,Xi,Phi0,Wn,Wn1,A(0:2),f0,f1,f3,stokes2P(3)
	Real(8) v,u,ss,const,alfaP1,alfaP2
	
	Xi2=XISQ
	Xi=sqrt(XI2)
	v=x/(1.0-x)
	u=sqrt(Nph*lambda*(1-x)/x-(1+Xi2))
	alfaP1=-sqrt(8.0D0)/lambda*v*u*cos(Phi)
	alfaP2=v/lambda
c       alfaP1=sqrt(8.)/lambda*v*u*(pe1*cos(Phi)+pe2*sin(Phi))
c       AlfaP1=alfa1/Xi, AlfaP2=alfa2/Xi^2

	call functionA(Nph,Xi,alfaP1,alfaP2,A)
C         A(0:2) are the An(i) function, i=0,1,2. and An(0) is divided by Xi

	const=A(1)**2-A(0)*Xi*A(2)
	f0=-A(0)**2+(1.0-x+1.0/(1-x))*const
      f1=u**2*sin(2*phi)*A(0)**2+sqrt(8.0)*u*sin(phi)*A(0)*A(1)
      f3=-(1+2*u**2*(sin(phi))**2)*A(0)**2+2*const
      stokes2P(1)=f1/f0
      stokes2P(2)=0.0
	stokes2P(3)=f3/f0
      ss=stokes2P(1)**2+stokes2P(2)**2+stokes2P(3)**2
c	   if((ss-1.0).GT.1.0E-6) then
c          print *,'Stokes=',sqrt(ss),'>1'
c		print *,'Stokes1=',Stokes2(1),'Stokes3=',Stokes2(3)
c		stop
c	   endif
!         Call InteractRate1(NPh,Phi,x,Xi,lambda,Wn,Wn1)
!         stokes2(3)=2.0*(Wn1/Wn)-1.0
  
      phi0=0.5*atan2(stokes1(1),stokes1(3))
      stokes2(3)=cos(2*phi0)*stokes2P(3)+sin(2*phi0)*stokes2P(1)
      stokes2(1)=-sin(2*phi0)*stokes2P(3)+cos(2*phi0)*stokes2P(1)
      stokes2(2)=stokes2P(2)
	return
	end
	   

