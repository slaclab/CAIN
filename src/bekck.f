      subroutine bekck(l,xys,fxyele,fxypos,fround,bbavgd,bbvard)
c--   use bassetti-erskine formula to calculate electric field
c     due to 2-d gaussian distribution of electrons and positrons.
	use bbcom
      implicit none
      integer l
      real*8 xys(2),fxyele(2),fxypos(2)
      include 'include/ctrlcm.h'
      logical fround
      integer l2
      real*8 dr(2),dr2,fxyval(2),fxyrnd(2)
      real*8 bbavgd(2,2,2),bbvard(2,2,2)
      real*8 eps/0.d-14/
      real(8) pi/3.141592653589793238d0/
      real*8 xi1,yi1,xi2,yi2
      real*8 u1,v1,u2,v2,densig
      real*8 x1,x2,sig1,sig2
      complex*16 fxycmp,w1,w2
      logical flag1,flag2
      integer ncall/0/
      integer iep
      save eps,pi,ncall
c
      ncall=ncall+1
      l2=3-l
      do iep=1,2
         dr=xys(1:2)-bbavgd(:,l2,iep)
         dr2=sum(dr**2)
c         print *, " bekck l2,iep=",l2,iep," bbvard(1,l2,iep)= ",
c     &      bbvard(1,l2,iep), " bbvard(2,l2,iep)= ",    
c     &      bbvard(2,l2,iep)
         fxyrnd=-dr/dr2*wchgt(l2,iep)*(1.-exp(
     &           -0.5*sum(dr**2/bbvard(:,l2,iep)**2)))
         if(fround) then
            fxyval=fxyrnd
         else
            if(bbvard(1,l2,iep).ge.bbvard(2,l2,iep)) then
               x1=dr(1)
               x2=dr(2)
               sig1=bbvard(1,l2,iep)
               sig2=bbvard(2,l2,iep)
            else
               x1=dr(2)
               x2=dr(1)
               sig1=bbvard(2,l2,iep)
               sig2=bbvard(1,l2,iep)
            endif
            densig=sqrt(2*(sig1**2-sig2**2)+eps)
            xi1=x1/densig
            yi1=x2/densig
            xi2=x1*sig2/sig1/densig
            yi2=x2*sig1/sig2/densig
            call wofz(abs(xi1),abs(yi1),u1,v1,flag1)
            call wofz(abs(xi2),abs(yi2),u2,v2,flag2)
            if(flag1.or.flag2) then
               fxyval=0
            else
               w1=cmplx(u1,v1)
               w2=cmplx(u2,v2)
               fxycmp=w1-exp(-0.5*sum(dr**2/bbvard(:,l2,iep)**2))*w2
               if(bbvard(1,l2,iep).ge.bbvard(2,l2,iep)) then
                  fxyval(1)=-sqrt(pi)*wchgt(l2,iep)/densig
     &                 *imag(fxycmp)*sign(1.d0,dr(1))
                  fxyval(2)=-sqrt(pi)*wchgt(l2,iep)/densig
     &                 *real(fxycmp)*sign(1.d0,dr(2))
               else
                  fxyval(1)=-sqrt(pi)*wchgt(l2,iep)/densig
     &                 *real(fxycmp)*sign(1.d0,dr(1))
                  fxyval(2)=-sqrt(pi)*wchgt(l2,iep)/densig
     &                 *imag(fxycmp)*sign(1.d0,dr(2))
               endif
c               if(outchg.or.mod(ncall,10000000).eq.0) then
c                  write(msgfl,105)  ncall,iep,sig2/sig1,
c     &                 fxyrnd,
c     &                 fxyval,
c     &                 densig
c 105              format('  bekck ncall= ',i12,' iep= ',i3,
c     &                 ' sig2/sig1= ',(1pe11.3)             
c     &                 ' fxyrnd= ',2(1pe11.3),
c     &                 ' fxyval= ',2(1pe11.3),
c     &                 ' densig= ',(1pe11.3))
c                  write(msgfl,101) x1,x2,sig1,sig2,
c     &                 xi1,yi1,xi2,yi2
c 101              format(' bekck ',
c     &                 ' x1,x2= ',2(1pe11.3),
c     &                 ' sig1,sig2= ',2(1pe11.3),
c     &                 ' xi1,yi1= ',2(1pe11.3),
c     &                 ' xi2,yi2= ',2(1pe11.3))
c                  write(msgfl,109) u1,v1,u2,v2,dr,
c     &                 exp(-0.5*sum(dr**2/bbvard(:,l2,iep)**2))*u2,
c     &                 u1-exp(-0.5*sum(dr**2/bbvard(:,l2,iep)**2))*u2
c 109              format(' bekck ',
c     &                 ' u1,v1= ',2(1pe15.7),
c     &                 ' u2,v2= ',2(1pe11.3),
c     &                 ' dr= ',2(1pe11.3),
c     &                 ' exp(...)*u2= ',(1pe15.7),
c     &                 ' u1-exp(...)*u2= ',(1pe15.7))
c               endif
            endif
         endif
         if(iep.eq.1) then
            fxyele=fxyval
         else
            fxypos=fxyval
         endif      
      enddo
      return
      end
