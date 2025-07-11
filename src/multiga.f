      subroutine multiga(nnu,lr,is)
      use bbcom
      implicit none
      integer nnu,lr,is
      external fcnllmg
      real*8 aa0(nnu)
      real*8 aa(nnu)
      real*8 dlnfda(nnu)
      real*8 diag(nnu)
      integer info
      integer nfev
      real*8 fjac(nnu,nnu)
      real*8 rr(lr)
      real*8 qtf(nnu),wa1(nnu),wa2(nnu),wa3(nnu),wa4(nnu)
      real*8 mu,sig(2),rrr(2),alp(2),karg
      real*8 mu0,sig0(2)
      real*8 dmu,dsig(2),drrr

      integer i,j,ixy
      integer mml
      integer mmu
      integer ldfjac
      real*8 xtol/1.d-10/
      integer maxfev/10000000/
      integer epsfcn/1d-20/
      integer mode/1/
      real*8 factor/100./
      real*8 etai(2)/-1.,1./
      integer nprint /-1/
      save xtol,maxfev,epsfcn,mode,factor,nprint,etai
      include 'include/pushcm.h'
      include 'include/ctrlcm.h'
c     
      if(nmulga.ne.2) then
         print *, " multiga nmulga= ", nmulga,
     &        "not supported.  program will now stop"
         stop
      endif
      mml=nnu-1
      mmu=nnu-1
      ldfjac=nnu
      mpis=is
      do mpixy=1,2
         do mpkk=1,2
cccccccccbegin calculate aa
            call initsig(is)
            if(mpikur.gt.3.) then
               aa(1)=1.5/mpikur
cccccccccend   calculate aa
               aa0=aa
               call hybrd(fcnllmg,nnu,aa,dlnfda,xtol,
     &              maxfev,mml,mmu,epsfcn,
     &              diag,mode,factor,nprint,
     &              info,nfev,fjac,ldfjac,rr,lr,
     &              qtf,wa1,wa2,wa3,wa4)
               if(info.ne.1.or.aa(1).lt.epsilon(aa(1))
     &              .or.aa(1).gt.3./mpikur) then
                  sig=bbvar(mpixy,mpll,mpkk)
                  rrr=0.5
               else
                  rrr(2)=aa(1)
                  rrr(1)=1.-rrr(2)
                  karg=1.-1./rrr(1)*(1.-mpikur*rrr(2)/3.)
                  alp=1.+etai*sqrt(karg)
                  sig=sqrt(alp)*bbvar(mpixy,mpll,mpkk)
               endif
               mu=bbavg(mpixy,mpll,mpkk)
               sig0=bbvar(mpixy,mpll,mpkk)
               bbavga(mpixy,mpll,mpkk,1:2)=mu
               bbvara(mpixy,mpll,mpkk,1:2)=sig
               bbrrra(mpixy,mpll,mpkk,1:2)=rrr
c               write(msgfl,102) it,is,mpixy,mpll,mpkk,
c     &              mpikur,mpilam,mu,sig0,sig,rrr,dlnfda,
c     &              mpnn(mpll,mpkk),info
c 102           format('  after hybrd it,is= ',i5,i3,
c     &              ' ixy= ',i3,' l= ',i3,' k= ',i3,
c     &              ' kur,lam= ',2(1pe11.3),
c     &              ' mu= ',(1pe11.3),
c     &              ' sig0= ',2(1pe11.3),
c     &              ' sig= ',2(1pe11.3),
c     &              ' rrr= ',2(1pe11.3),
c     &              ' drrr= ',(1pe11.3),
c     &              ' mpnn,info= ',i9,i3)
            else
               bbavga(mpixy,mpll,mpkk,1:2)=bbavg(mpixy,mpll,mpkk)
               bbvara(mpixy,mpll,mpkk,1:2)=bbvar(mpixy,mpll,mpkk)
               bbrrra(mpixy,mpll,mpkk,1:2)=0.5
            endif
         enddo
      enddo
      return
      end
