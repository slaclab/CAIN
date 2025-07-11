      SUBROUTINE BBBIN(T,IS,ss)
	USE BEAMCM
	USE BBCOM
        use lumcom
C  Binning for beam-beam force
      IMPLICIT NONE
      INTEGER IS
      REAL*8 T,ss(2)
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/bbcom.h'
      include 'include/ctrlcm.h'
      include 'include/cnstcm.h'
      include 'include/pushcm.h'
      INTEGER IXY,L,K,N,I,IJ(2),I00,I10,I01,I11,I000(2)
      REAL*8 PXY(2),PSIZE1,WCHG,WOUT(2),WTOT(2)
      REAL*8 CHG(3)/0D0,-1D0,1D0/
      real*8 bbqmax(2)
      real*8 bbq1(2,2,2),bbq2(2,2,2)
      real*8 bbq1t(2,2),bbq2t(2,2),bbtavg(2,2),bbtvar(2,2)
      real*8 bbq1s(2,2),bbq2s(2,2),bbsavg(2,2),bbsvar(2,2)
      real*8 bbavg0(2,2,2),bbvar2(2,2,2)
      real*8 ds,fac0,rnp,rwal
      real*8 fldtest(2)
      integer fchgd/49/
      integer fvard/53/
      integer ios,iflag
      integer ijmax(2,2)
      real*8 pxyuse(2)
      real*8 fbeval(2)
      real*8 wgtmax(2)
      integer ngtmax(2)
      integer ku,kv,nwal,lr,nnu
      character(30) chgnam
      character(24) varnam
      integer nodd(2)/40514,1/
      real*8 fbemax(2)/-1d20,-1d20/
      real*8 fbemin(2)/1d20,1d20/
      real*8 fakbe/0.451d0/
      real*8 fakwal/0.1/
c      logical emzero/.true./
      logical emzero/.false./
      integer iout
c      integer nout/12/
      integer nout/0/
      integer itout(12)/51, 65, 70,126,159,162,
     &                 189,283,344,387,400,512/
      integer isout(12)/ 1,  1,  1,  7,  1, 1,
     &     3,  4,  3,  5,  6, 7/
      integer ncall/0/
      save nodd
      save fbemax
      save fbemin
      save fakbe
      save fakwal
      save nout,isout,itout,fchgd,ncall
      save nnu,lr
C
      CALL CPUTIM('BBBIN',1)
      ncall=ncall+1
      if(ncall.eq.1) then
         if(fldbe.gt.3.or.(fldbe.eq.0.and.bealt.gt.3)) then
            nmulga=max(fldbe,bealt)-3
c            nnu=3*nmulga-1
c            nnu=2*nmulga-1
c            nnu=nmulga-1
            nnu=nmulga/2
            lr=(nnu*(nnu+1))/2
            allocate(bbavga(2,2,2,nmulga))
            allocate(bbvara(2,2,2,nmulga))
            allocate(bbrrra(2,2,2,nmulga))
          endif
      endif
      ds=ss(2)-ss(1)
      fac0=-2*mass(2)*re/ds
      chgnam='ChargeDistributionT000000S0000'
      varnam='VarianceDataT000000S0000'
      outchg=.false.
      do iout=1,nout
         if(is.eq.isout(iout).and.it.eq.itout(iout)) then
            outchg=.true.
            write(chgnam(20:25),'(i6.6)') it
            write(chgnam(27:30),'(i4.4)') is
            open(unit=fchgd,file=chgnam,status="new",
     &           form='formatted',
     &           iostat=ios)            
            print *, " bbbin return opening file ",
     &           chgnam," ios= ",ios
            if(ios.ne.0) then
               print *, " bbbin program will now stop"
               stop
            endif
            write(varnam(14:19),'(i6.6)') it
            write(varnam(21:24),'(i4.4)') is
            open(unit=fvard,file=varnam,status="new",
     &           form='formatted',
     &           iostat=ios)            
            print *, " bbbin return opening file ",
     &           varnam," ios= ",ios
            if(ios.ne.0) then
               print *, " bbbin program will now stop"
               stop
            endif
            exit
         endif
      enddo
      ngtmax=0
      wgtmax=-1d20
      bbqmax=-1.d20
      wchgt=0
      bbq1=0
      bbq2=0
      bbq1t=0
      bbq2t=0
      bbq1s=0
      bbq2s=0
      DO 180 L=1,2
        WOUT(L)=0
        WTOT(L)=0
c      write(msgfl,49) l,xymin(:,l),bbdxy(:,l)
c 49   format(' bbbin entry l= ',i3,
c     &     ' xymin(:,l)= ',2(1pe11.3),
c     &     ' bbdxy(:,l)= ',2(1pe11.3))
180   CONTINUE
      I000(1)=0
      I000(2)=NXY(1)*NXY(2)
      DO 200 I=1,NXY(1)*NXY(2)*2
        BBQ(I)=0
 200  CONTINUE
      PSIZE1=1-PSIZE
      mpnn=0
      if(fldbe.gt.3.or.(fldbe.eq.0.and.bealt.gt.3)) then
         rnp=np
         rwal=fakwal*rnp
         nwal=rwal
         allocate (wchga(nwal,2,2))
         allocate (pxyua(nwal,2,2,2))
      endif
      DO 300 N=1,NP
         IF(ISBIN(N).NE.IS) GOTO 300
         IF(KIND(N).EQ.1) GOTO 300
         IF(LOST(N).NE.0) GOTO 300
         IF(PNAME(N).NE.'    ') GOTO 300
         IF(TXYS(0,N).GT.T) GOTO 300
C     exclude those created at time>T.
         IF(EP(3,N).GE.0) THEN
            L=1
         ELSE
            L=2
         ENDIF
         IF(BBFLG(L).EQ.0) GOTO 300
         k=kind(n)-1
         DO 210 IXY=1,2
            PXY(IXY)=TXYS(IXY,N)+EP(IXY,N)/EP(0,N)*(T-TXYS(0,N))
            pxyuse(ixy)=pxy(ixy)
            PXY(IXY)=(PXY(IXY)-XYMIN(IXY,L))/BBDXY(IXY,L)+1D0
            IJ(IXY)=INT(PXY(IXY))
            IF(IJ(IXY).LE.0.OR.IJ(IXY).GT.NXY(IXY)) THEN
               WOUT(L)=WOUT(L)+WGT(N)
               GOTO 300
            ENDIF
            PXY(IXY)=PXY(IXY)-IJ(IXY)
            WTOT(L)=WTOT(L)+WGT(N)
 210     CONTINUE
         WCHG=WGT(N)*CHG(KIND(N))
         if(wgt(n).gt.wgtmax(l)) then
            ngtmax(l)=n
            wgtmax(l)=wgt(n)
         endif
         wchgt(l,k)=wchgt(l,k)+wchg
         bbq1(:,l,k)=bbq1(:,l,k)+wchg*pxyuse(:)
         bbq2(:,l,k)=bbq2(:,l,k)+wchg*pxyuse(:)**2
         bbq1t(l,k)=bbq1t(l,k)+wchg*txys(0,n)
         bbq2t(l,k)=bbq2t(l,k)+wchg*txys(0,n)**2
         bbq1s(l,k)=bbq1s(l,k)+wchg*txys(3,n)
         bbq2s(l,k)=bbq2s(l,k)+wchg*txys(3,n)**2
         mpnn(l,k)=mpnn(l,k)+1
         if(fldbe.gt.3.or.(fldbe.eq.0.and.bealt.gt.3)) then
            if(mpnn(l,k).gt.nwal) then
               write(msgfl,104) it,is,l,k,mpnn(l,k),
     &              nwal
 104           format(' it,is= ',i5,i3,' l= ',i3,' k= ',i3,
     &              ' mpnn= ',i10,
     &              ' is greater than nwal= ',i10,
     &              ' program will now stop ')
               stop
            endif
            wchga(mpnn(l,k),l,k)=wchg
            pxyua(mpnn(l,k),1:2,l,k)=pxyuse(1:2)
         endif
         if(it.eq.2956.and.is.eq.12) then
            write(msgfl,123) it,is,l,k,n,mpnn(l,k),
     &           gen(n),wgt(n),ep(0,n),ep(1:3,n),       
     &           wchg,
     &           wchgt(l,k),
     &           pxyuse,bbq1(1:2,l,k),bbq2(1:2,l,k)       
 123        format(' it,is= ',i5,i3,' l= ',i3,' k= ',i3,' n= ',i10,
     &           ' mpnn= ',i10,' gen= ',i4,
     &           ' wgt= ',(1pe11.3),       
     &           ' ep(0)= ',(1pe11.3),       
     &           ' ep(1:3)= ',3(1pe11.3),       
     &           ' wchg= ',(1pe11.3),
     &           ' wchgt= ',(1pe11.3),
     &           ' pxyuse= ',2(1pe11.3),
     &           ' bbq1= ',2(1pe11.3),
     &           ' bbq2= ',2(1pe11.3))
         endif
         if(outchg) then
            write(fchgd,582) l,k,pxyuse(1:2),wchg
 582        format(1x,2i3,3(1pe14.6))
         endif
         IF(PSIZE.LE.0.OR.
     %        (PXY(1).GE.PSIZE.AND.PXY(1).LE.PSIZE1.AND.
     %        PXY(2).GE.PSIZE.AND.PXY(2).LE.PSIZE1)) THEN
            I=I000(L)+IJ(1)+NXY(1)*(IJ(2)-1)
            BBQ(I)=BBQ(I)+WCHG
            print *, " WARNING:  Alternative bbq calc "
            if(abs(bbq(i)).gt.bbqmax(l)) then
               bbqmax(l)=abs(bbq(i))
               ijmax(1:2,l)=ij
            endif
         ELSE
c     print *, " initial is,pxy,ij= ", is,pxyuse,ij
c     print *, " reset   is,pxy,ij= ", is,pxy,ij
            DO 240 IXY=1,2
               IF(PXY(IXY).LT.0.5D0) THEN
                  IF(IJ(IXY).NE.1) THEN
                     IJ(IXY)=IJ(IXY)-1
                     PXY(IXY)=0.5D0-PXY(IXY)/PSIZE
                  ELSE
                     PXY(IXY)=1
                  ENDIF
               ELSE
                  IF(IJ(IXY).EQ.NXY(IXY)) THEN
                     IJ(IXY)=IJ(IXY)-1
                     PXY(IXY)=0
                  ELSE
                     PXY(IXY)=0.5D0+(1-PXY(IXY))/PSIZE
                  ENDIF
               ENDIF
 240        CONTINUE
            I00=I000(L)+IJ(1)+NXY(1)*(IJ(2)-1)
            I10=I00+1
            I01=I00+NXY(1)
            I11=I01+1
            BBQ(I00)=BBQ(I00)+WCHG*PXY(1)*PXY(2)
            BBQ(I10)=BBQ(I10)+WCHG*(1-PXY(1))*PXY(2)
            BBQ(I01)=BBQ(I01)+WCHG*PXY(1)*(1-PXY(2))
            BBQ(I11)=BBQ(I11)+WCHG*(1-PXY(1))*(1-PXY(2))
            if(abs(bbq(i00)).gt.bbqmax(l)) then
               bbqmax(l)=abs(bbq(i00))
               ijmax(1:2,l)=ij
            endif
c     print *, " final   is,pxy,ij= ", is,pxy,ij
         ENDIF
 300  CONTINUE
      if(outchg) then
         close(unit=fchgd,iostat=ios)
         close(unit=fvard,iostat=ios)
      endif
      DO 320 L=1,2
        WTOT(L)=WTOT(L)+WOUT(L)
        IF(WTOT(L).NE.0) THEN
          WOUT(L)=WOUT(L)/WTOT(L)
          WGTOUT(L)=MAX(WGTOUT(L),WOUT(L))
        ENDIF
 320  CONTINUE
      do 405 l=1,2
      do 407 k=1,2
      if(mpnn(l,k).lt.2) then
         wchgt(l,k)=1.d-12
         bbq1(:,l,k)=0.d0
         bbq2(1,l,k)=1.d0
         bbq2(2,l,k)=2.d0
      endif
 407  continue
 405  continue
      do ixy=1,2
         bbavg0(ixy,:,:)=bbq1(ixy,:,:)/wchgt
         bbvar2(ixy,:,:)=bbq2(ixy,:,:)/wchgt-bbavg0(ixy,:,:)**2
         bbvar(ixy,:,:)=sqrt(bbq2(ixy,:,:)/wchgt-bbavg0(ixy,:,:)**2)
      enddo
      do l=1,2
         if(abs(bbdxy(2,l)-bbdxy(1,l)).lt.1.e-20) then
            if(mpnn(l,1).gt.mpnn(l,2)) then
               ku=1
               kv=2
            else
               ku=2
               kv=1
            endif
            mpll=l
            if(emzero) then
               bbavg(1:2,l,ku)=bbavg0(1:2,l,ku)
               call zemxy(bbavg(1:2,l,ku),is)
               bbavg(1:2,l,kv)=bbavg(1:2,l,ku)
            else
               bbavg(1:2,l,1:2)=bbavg0(1:2,l,1:2)
            endif
         else
            bbavg(1:2,l,1:2)=bbavg0(1:2,l,1:2)
         endif
      enddo
      bbtavg=bbq1t/wchgt
      bbsavg=bbq1s/wchgt
      bbtvar=sqrt(bbq2t/wchgt-bbtavg**2)
      bbsvar=sqrt(bbq2s/wchgt-bbsavg**2)
      fbeval=-2.*finstrc*hbarc*fakbe/dslum*(
     &     wchgt(:,1)/sqrt(bbvar(1,:,1)**2+bbvar(2,:,1)**2)
     &    +wchgt(:,2)/sqrt(bbvar(1,:,2)**2+bbvar(2,:,2)**2))
      fbemax=max(fbemax,fbeval)
      fbemin=min(fbemin,fbeval)
      do l=1,2
c         write(msgfl,101) it,is,l,t,np,emzero,
c     &        ngtmax(l),wgtmax(l),
c     &    bbvar(1,l,1),bbvar(2,l,1),
c     &        wchgt(l,1),mpnn(l,1),
c     &    bbvar(1,l,2),bbvar(2,l,2),
c     &        wchgt(l,2),mpnn(l,2),
c     &        fbeval(l),fbemax(l),fbemin(l)
 101     format(' it,is= ',i5,i3,' l= ',i3,' t= ',(1pe11.3),
     &        ' np= ',i8,' emzero= ',l3,
     &        ' ngtmx= ',i8,' wgtmx= ',(1pe11.3),
     &        ' e- x,yv= ',2(1pe11.3),
     &        ' e- wchg,mpnn= ',(1pe11.3),i6,
     &        ' e+ x,yv= ',2(1pe11.3),
     &        ' e+ wchgt,mpnn= ',(1pe11.3),i6,
     &        ' fbeval,fbemx,fbemn= ',3(1pe11.3))
c         write(msgfl,102) it,is,l,t,
c     &        wchgt(l,1),mpnn(l,1),
c     &        bbavg0(1:2,l,1),
c     &        bbavg(1:2,l,1),
c     &        wchgt(l,2),mpnn(l,2),
c     &        bbavg0(1:2,l,2),
c     &        bbavg(1:2,l,2)
 102     format(' it,is= ',i5,i3,' l= ',i3,' t= ',(1pe11.3),
     &        ' e- wchg,mpnn= ',(1pe11.3),i6,
     &        ' e- x,yavg0= ',2(1pe11.3),
     &        ' e- x,yavg= ',2(1pe11.3),
     &        ' e+ wchgt,mpnn= ',(1pe11.3),i6,
     &        ' e+ x,yavg0= ',2(1pe11.3),
     &        ' e+ x,yavg= ',2(1pe11.3))
         if(fldbe.gt.3.or.(fldbe.eq.0.and.bealt.gt.3)) then
            mpll=l
            call multiga(nnu,lr,is)
            do i=1,nmulga
c              write(msgfl,103) it,is,l,i,
c    &              wchgt(l,1),mpnn(l,1),
c    &              bbavga(1:2,l,1,i),
c    &              bbvara(1:2,l,1,i),
c    &              bbrrra(1:2,l,1,i),
c    &              wchgt(l,2),mpnn(l,2),
c    &              bbavga(1:2,l,2,i),
c    &              bbvara(1:2,l,2,i),
c    &              bbrrra(1:2,l,2,i)
 103           format(' it,is= ',i5,i3,' l= ',i3,' i= ',i3,
     &              ' e- wchg,mpnn= ',(1pe11.3),i6,
     &              ' e- x,yavga= ',2(1pe11.3),
     &              ' e- x,yvara= ',2(1pe11.3),
     &              ' e- x,yrrra= ',2(1pe11.3),
     &              ' e+ wchgt,mpnn= ',(1pe11.3),i6,
     &              ' e+ x,yavga= ',2(1pe11.3),
     &              ' e+ x,yvara= ',2(1pe11.3),
     &              ' e+ x,yrrra= ',2(1pe11.3))
               write(fvard,591) it,is,l,i,
     &              wchgt(l,1),mpnn(l,1),
     &              bbavga(1:2,l,1,i),
     &              bbvara(1:2,l,1,i),
     &              bbrrra(1:2,l,1,i),
     &              wchgt(l,2),mpnn(l,2),
     &              bbavga(1:2,l,2,i),
     &              bbvara(1:2,l,2,i),
     &              bbrrra(1:2,l,2,i)
 591           format(1x,i5,3i3,4x,
     &              (1pe11.3),i6,
     &              2x,2(1pe11.3),
     &              2x,2(1pe11.3),
     &              2x,2(1pe11.3),4x,
     &              (1pe11.3),i6,
     &              2x,2(1pe11.3),
     &              2x,2(1pe11.3),
     &              2x,2(1pe11.3))
            enddo
         else
            do i=1,nmulga
               write(fvard,591) it,is,l,i,
     &              wchgt(l,1),mpnn(l,1),
     &              bbavg(1:2,l,1),
     &              bbvar(1:2,l,1),
     &              0.5,0.5,
     &              wchgt(l,2),mpnn(l,2),
     &              bbavg(1:2,l,2),
     &              bbvar(1:2,l,2),
     &              0.5,0.5
            enddo
         endif            
      enddo
      if(fldbe.gt.3.or.(fldbe.eq.0.and.bealt.gt.3)) then
         deallocate(wchga)
         deallocate(pxyua)
      endif
      CALL CPUTIM('BBBIN',2)
      RETURN
      END
