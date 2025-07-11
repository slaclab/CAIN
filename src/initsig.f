      subroutine initsig(is)
      use bbcom
      implicit none
      integer is
      real*8 bbq1,bbq2,bbq4
      real*8 muchk,varchk,karg,zbin,rini
      integer i,nzbin
c      real*8 delt/1.e-2/
      real*8 delt/1.e-1/
      real*8 kumax/5.99/
      real*8 pi/3.141592653589793238d0/
      save delt,kumax,pi
      include 'include/pushcm.h'
      include 'include/ctrlcm.h'
c     
      if(nmulga.eq.2) then
         bbq1=0.
         bbq2=0.
         bbq4=0.
         zbin=0.
         nzbin=0
         do i=1,mpnn(mpll,mpkk)
            bbq1=bbq1+wchga(i,mpll,mpkk)*pxyua(i,mpixy,mpll,mpkk)
            bbq2=bbq2+wchga(i,mpll,mpkk)
     &           *(pxyua(i,mpixy,mpll,mpkk)-bbavg(mpixy,mpll,mpkk))**2
            bbq4=bbq4+wchga(i,mpll,mpkk)
     &           *(pxyua(i,mpixy,mpll,mpkk)-bbavg(mpixy,mpll,mpkk))**4
            if(abs(pxyua(i,mpixy,mpll,mpkk)-bbavg(mpixy,mpll,mpkk))
     &           /bbvar(mpixy,mpll,mpkk).lt.delt) then
               zbin=zbin+wchga(i,mpll,mpkk)
               nzbin=nzbin+1
            endif
         enddo
         muchk=bbq1/wchgt(mpll,mpkk)
         varchk=sqrt(bbq2/wchgt(mpll,mpkk))
         mpikur=bbq4/wchgt(mpll,mpkk)/bbvar(mpixy,mpll,mpkk)**4
         mpilam=sqrt(0.5*pi)*zbin/wchgt(mpll,mpkk)/delt
c         if(mpnn(mpll,mpkk).gt.0) then
c            write(msgfl,102) it,is,mpixy,mpll,mpkk,
c     &           muchk,bbavg(mpixy,mpll,mpkk),
c     &           varchk,bbvar(mpixy,mpll,mpkk),
c     &           mpikur,mpilam,
c     &           delt,
c     &           mpnn(mpll,mpkk),nzbin
c 102        format(' initsig it,is= ',i5,i3,
c     &           ' ixy= ',i3,' l= ',i3,' k= ',i3,
c     &           ' muchk,mu= ',2(1pe11.3),
c     &           ' varchk,var= ',2(1pe11.3),
c     &           ' mpikur,mpilam= ',2(1pe11.3),
c     &           ' delt= ',(1pe11.3),
c     &           ' mpnn,nzbin= ',2i9)
c         endif
      else
         print *, " initsig does not support nmulga= ",nmulga,
     &        " program will now stop "
         stop
      endif
      return
      end
