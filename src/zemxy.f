      subroutine zemxy(pxy,is)
      use bbcom
      implicit none
      real*8 pxy(2)
      integer is
      external fcnemxy
      real*8 pxy0(2)
      real*8 fxy(2)
      real*8 diag(2)
      integer info
      integer nfev
      real*8 fjac(2,2)
      real*8 rr(3)
      real*8 qtf(2),wa1(2),wa2(2),wa3(2),wa4(2)

      real*8 xtol/1.d-10/
      integer maxfev/10000000/
      integer ml/1/
      integer mu/1/
      integer epsfcn/1d-20/
      integer mode/1/
      real*8 factor/100./
      integer nprint /-1/
      integer ldfjac/2/
      integer lr/3/
      save xtol,maxfev,ml,mu,epsfcn,mode,factor,nprint
      save ldfjac,lr
      include 'include/pushcm.h'
      include 'include/ctrlcm.h'
c
      pxy0=pxy
      call hybrd(fcnemxy,2,pxy,fxy,xtol,maxfev,ml,mu,epsfcn,diag,
     &           mode,factor,nprint,info,nfev,fjac,ldfjac,rr,lr,
     &           qtf,wa1,wa2,wa3,wa4) 
c      write(msgfl,102) it,is,mpll,pxy0,
c     &     pxy,fxy,info
c 102  format('  after hybrd it,is= ',i5,i3,' l= ',i3,
c     &     ' pxy0= ',2(1pe11.3),
c     &     ' pxy= ',2(1pe11.3),
c     &     ' fxy= ',2(1pe11.3),
c     &     ' info= ',i3)    
      if(info.ne.1) then
         pxy=pxy0
      endif
      return
      end
