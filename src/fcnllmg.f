      subroutine fcnllmg(nvar,aa,dlnfda,iflag)
      use bbcom
      implicit none
      integer nvar
      real*8 aa(nvar)
      real*8 dlnfda(nvar)
      integer iflag
c     
      include 'include/pushcm.h'
      include 'include/ctrlcm.h'

      real*8 sig(2),rrr(2),alp(2)
      real*8 var,karg
      real*8 alpdr(2),dsigdr(2),farg,harg(2),dalpdr(2)
      real*8 etai(2)/-1.,1./
      save etai
      
      call cputim('fcnllmg',1)
      dlnfda=0
      rrr(2)=aa(1)
      if(rrr(2).lt.epsilon(rrr(2)).or.rrr(2).gt.3./mpikur) then
         dlnfda=huge(dlnfda(1))
         return
      endif
      rrr(1)=1-rrr(2)
      karg=1.-1./rrr(1)*(1.-mpikur*rrr(2)/3.)
      var=1.
      alp=var*(1.+etai*sqrt(karg))
      sig=sqrt(alp)
      dalpdr=0.5*var*etai/rrr(1)/sqrt(karg)*(mpikur/3.+karg-1.)
      dsigdr=0.5/sqrt(alp)*dalpdr
      farg=rrr(1)/sig(1)+rrr(2)/sig(2)-mpilam
      harg=1./sig*(1.-etai*rrr/sig*dsigdr)
      dlnfda(1)=farg*(harg(2)-harg(1))
      if(it.eq.126.and.mpis.eq.7.and.mpll.eq.1) then
         write(msgfl,102) it,mpis,mpixy,mpll,mpkk,
     &        nvar,sig,rrr,var,mpikur,mpilam,dlnfda
 102     format(' fcnllmg it,is= ',i5,i3
     &        ' ixy= ',i3,' l= ',i3,' k= ',i3,
     &        ' nvar= ',i3,
     &        ' sig= '2(1pe11.3),
     &        ' rrr= '2(1pe11.3),
     &        ' var,kur= '2(1pe11.3),
     &        ' lam,dlnfda= ',2(1pe11.3))
      endif
      call cputim('fcnllmg',2)
      return
      end
