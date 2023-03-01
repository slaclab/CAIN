      subroutine NLCPLST4(Nmax,My0,Mphi0,Mxi0,Mlambda,Lambdamax,Ximax,
     %                	 IRTN)
C  Create table for nonlinear Compton (linear pol)
	use NLCPLPTAB
	Implicit none	
      integer NMax,Mx,My0,Mphi0,Mxi0,Mlambda,IRTN
      real*8  Lambdamax,Ximax
	real*8 Ximax2,Dxi2,Dlambda,Dphi
	real*8 x,Dx,xmax,y,Ymax,Ymin,phi,xi2,lambda,Xi
      real*8 Wn,Wn1,WnInt,Total
      real*8 sum,ssum,sum1,ssum1,cy,cphi,MMax,MMax1
	integer n,ky,kyy,kphi,Kxi2,klambda,Harmmax
      Include 'include/nlcpcm.h'
	LOGICAL DBG/.FALSE./

      Real*8 PI/3.141592653589793238d0/
	
	CALL CPUTIM('NLCPLST4',1)
	Mph=Nmax
	My=My0
	Mx=My
	Mphi=Mphi0
	Mxi=Mxi0
	MLM=Mlambda
	LMMAX=Lambdamax
	XISQMX=Ximax**2
	 
	CALL DEALLOCNLCPLST
	Allocate(TWn(Nmax,0:My,0:Mphi,0:Mxi,0:Mlambda),
     %  WWn(Nmax,0:My,0:Mxi,0:Mlambda),TWnMax(Nmax,0:Mxi,0:Mlambda),
     %  TIntWn(Nmax,0:Mxi,0:Mlambda),
     %  WWnphi(Nmax,0:Mphi,0:Mxi,0:MLambda),
     %  TWn1(Nmax,0:My,0:Mphi,0:Mxi,0:Mlambda),
     %  WWn1(Nmax,0:My,0:Mxi,0:Mlambda),TWn1Max(Nmax,0:Mxi,0:Mlambda),
     %  TIntWn1(Nmax,0:Mxi,0:Mlambda),
     %  WWn1phi(Nmax,0:Mphi,0:Mxi,0:MLambda))
      allocated=1

	Ximax2=Ximax**2         !         Ximax2 is Xi0**2 
	Dxi2=Ximax2/Mxi
	Dlambda=Lambdamax/Mlambda
      Dphi=Pi/Mphi

	do Kxi2=0,Mxi 
	  if(Kxi2.eq.0) then
	    xi2=1.0E-12
	  else
	    xi2=Dxi2*Kxi2
	  endif
        Xi=sqrt(xi2)
	  do klambda=0,Mlambda
	    if(klambda.eq.0) then
	      lambda=1.0e-10
	    else
            lambda=Dlambda*klambda
	    endif
	    do n=1,Nmax
	      Xmax=n*lambda/(1+xi2+n*lambda)
            TIntWn(n,Kxi2,klambda)=0
				TIntWn1(n,Kxi2,klambda)=0
	      MMax=0.0
			  MMax1=0.0

	      do ky=0,My
	        y=(1.0d0/My)*ky
	    	  x=y*Xmax
	        cy=1.0
	        if(ky.eq.0.or.ky.eq.My) cy=0.5d0
	        sum=0
					sum1=0
				  do kphi=0,Mphi
                Phi=Dphi*kphi
	          cphi=1.0
	          if(kphi.eq.0.or.kphi.eq.Mphi) cphi=0.5d0
                call InteractRate1(n,Phi,x,Xi,lambda,Wn,Wn1)
            IF(DBG) THEN
	        if(kxi2.eq.Mxi.and.klambda.eq.Mlambda.and.n.eq.3.and.
     %          (ky.ge.My-2)) then
	          write(6,999) ky,Phi/Pi, Wn
999             format(i5,f10.5,d15.5)
              endif
			    if((n.GT.Nmax).or.(N.Le.0)) print *,'n=', n              
              if((ky.GT.My).or.(ky.LT.0)) print *,'ky=', ky              
              if((kphi.GT.Mphi).or.(Kphi.LT.0)) print *,'ky=', ky              
              if((kXi2.GT.Mxi).or.(kXi2.LT.0)) print *,'kxi2=', kXi2              
              if((klambda.GT.Mlambda).or.(klambda.LT.0))
     %          print *,'klambda=', klambda
	      ENDIF           
                TWn(n,ky,kphi,Kxi2,klambda)=Wn
						TWn1(n,ky,kphi,Kxi2,klambda)=Wn1
		        sum=sum+Wn*cphi
						sum1=sum1+Wn1*cphi
	          MMax=Max(MMax,Wn)
						MMax1=Max(MMax1,Wn1)
              enddo   ! kphi
	        WWn(n,ky,Kxi2,klambda)=sum*Dphi/Pi
	        WWn1(n,ky,Kxi2,klambda)=sum1*Dphi/Pi
	        TIntWn(n,Kxi2,klambda)=TIntWn(n,Kxi2,klambda)+
     %            cy*WWn(n,ky,Kxi2,klambda)*(Xmax/My)
	        TIntWn1(n,Kxi2,klambda)=TIntWn1(n,Kxi2,klambda)+
     %            cy*WWn1(n,ky,Kxi2,klambda)*(Xmax/My)
	      enddo  ! ky
			  TWnMax(n,Kxi2,klambda)=MMax
			  TWn1Max(n,Kxi2,klambda)=MMax1

	      do kphi=0,Mphi
              Phi=Dphi*kphi
	        ssum=0
					ssum1=0
	        do ky=0,My
	          y=(1.0d0/My)*ky
		   	    x=y*Xmax
	          cy=1.0
	          if(ky.eq.0.or.ky.eq.My) cy=0.5d0
			      call InteractRate1(n,Phi,x,Xi,lambda,Wn,Wn1)
		        ssum=ssum+Wn*cy
						ssum1=ssum1+Wn1*cy
         			enddo   ! ky
	        WWnPhi(n,kPhi,Kxi2,klambda)=ssum*(Xmax/My)
	        WWn1Phi(n,kPhi,Kxi2,klambda)=ssum1*(Xmax/My)
			  enddo  ! kPhi
			enddo ! n
	  enddo   ! klambda
	enddo   !  Kxi2
	CALL CPUTIM('NLCPLST4',2)
	return
	end   
