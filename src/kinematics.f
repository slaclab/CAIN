
       Subroutine Kinematics(N,Xi2,lambda,y,x,phi,p1,k1,p2,k2,stokes)

c            Nonlinear Compton scattering for strang laser field in general frame
c Input:   n,Xi2,lambda,y,phi,k1,stokes,e1,e2
c              N: harmonic number  
c              Xi2=Xi^2: Xi is the normalized laser vector potential
c              Phi: azimuth angle of the scattered photon
c              p1: the initial electron 4-vector 
c              k1: the initial photon 4-vector 
c              stokes(1-3): stokes parameters
c              e1,e2,k1 the initial photon axes  

c Output:  p2,k2
c              theta: the scattering polar angle
c              k2:   the finial photon 4-vector 
c              p2:   the scattered electrons 4-energy-momentum tensor 
	       Implicit none
	       Integer N
		   real(8) Xi2,lambda,y,phi
		   real(8) p1(0:3),k1(0:3),p2(0:3),k2(0:3),stokes(3)
		   real(8) xmax,x,phi0,sum,u1,u2
		   real(8) e1(3),e2(3)
		   Integer j
		   real*8 AMe/0.511006E+6/
	       integer ncall/0/
	       save ncall
c               print *, 'the initial e-photn energy:',p1(0),k1(0)

	       ncall=ncall+1


		e1(1)=1. 
          e1(2)=0. 
          e1(3)=0. 
		e2(1)=0. 
          e2(2)=1. 
          e2(3)=0. 
 

               if(y.gT.1.0) print *,'y=',y
               xmax=N*lambda/(1+Xi2+n*lambda)
			 x=y*xmax  
               
	         sum=stokes(1)**2+stokes(2)**2+stokes(3)**2
               if(sum.ge.1.000001)
     %			  print *,'Stokes parameters are mistaken'
            if(sum.ge.1.000001) print *,'stokes(1)=',stokes(1)
            if(sum.ge.1.000001) print *,'stokes(2)=',stokes(2)
            if(sum.ge.1.000001) print *,'stokes(3)=',stokes(3)
            if(sum.ge.1.000001) stop

	         phi0=-0.5*atan2(stokes(1),stokes(3))
c			 if(abs(stokes(1)).le.0.1) then
c	           phi0=0.5*Asin(stokes(1))
c	         else
c	           phi0=0.5*Acos(stokes(3))
c			 endif
			 

			 u1=N*(1-x)-(2+Xi2)*x/lambda
			 u2=x*sqrt(N*lambda*(1-x)/x-(1+Xi2))

			 do j=1,3
				k2(j)=x*p1(j)+u1*k1(j)+AME*u2
     %                 *(e1(j)*cos(phi+phi0)+e2(j)*sin(phi+phi0))
			 enddo
c			 if(k2(1).le.0) print *,'kx=',k2(1)
			 k2(0)=x*p1(0)+u1*k1(0)

			 do j=0,3
			   p2(j)=p1(j)+(n-x/(1-x)*Xi2/lambda)*k1(j)-k2(j)
			 enddo
	       return
	   end
