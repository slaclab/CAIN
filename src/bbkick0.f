      SUBROUTINE BBKICK0(L,DS,KIN,XYS,EP,FLD,FF,NOUT,NXYD,BBQD,is)
C--   Assume the following beam-beam field, which is valid
C     when the main part of the beam is almost parallel to the s-sxis
C     Magnetic field =  sgn*es \times (Electric field)
C     es= unit vector along s-axis
C     sgn=1 when the beam creating the field is right-going,
C     -1 when left-going.
C     L  :  1(2) when the particle receiving the kick is right(left)
C     going.
C     FLD:  electric/magnetic field (V/m)
C     FF :  Lorentz force (V/m)
      use bbcom
      IMPLICIT NONE
      INTEGER L,KIN,NOUT,NXYD(2),is
      REAL*8 DS,XYS(3),EP(0:3),FLD(3,2),FF(3),BBQD(*)
      INTEGER L2,IOUT(1)
      REAL*8 FAC0,FXY(2),SGN
      real*8 fxyele(2),fxypos(2)
      real*8 fuvele(2),fuvpos(2)
      real*8 drele(2),dr2ele,drpos(2),dr2pos
      real*8 vr2ele,vr2pos
      real*8 fxyalt(2),fldalt(2),fldpos(2),fldf(2),fldele(2)
      real*8 fldabs,altabs,fldeabs,fldpabs
      logical maxdet
      integer iele/1/
      integer ipos/2/
      integer nnis/100/
      real*8 fldmax(2,2)/-1.d20,-1.d20,-1.d20,-1.d20/
      real*8 xyu(3),xyc(3),xyt(3),xyd(3),xyr(3)
      real*8 rri,rrial,frial,fxy2mx,fxy2,vr
      real*8 phi,phial
      real*8 nsgmx/2./
      real*8 pi/3.141592653589793238d0/
      integer(8) ncall/0/
      integer nrrial/800/
      integer nphial/100/
      integer femp/48/
      integer ios,iflag,kdom
      integer i,j,nj,ig
      logical fround/.false./
c     logical fdemax/.true./
      logical fdemax/.false./
      character(23) empnam
      save iele,ipos,nnis
      save fldmax,nsgmx,pi
      save fround
      save fdemax
      save ncall,nrrial,nphial
      save femp
      INCLUDE 'include/cnstcm.h'
      include 'include/ctrlcm.h'
      include 'include/pushcm.h'
C     
      ncall=ncall+1
      if(ncall.eq.1) then
         print *, " bbkick0 fround= ",fround,
     &        " fdemax= ",fdemax," nt,nnis= ",nt,nnis,
     &        " bealt= ",bealt    
      endif
      fac0=-2*mass(2)*re/ds
      L2=3-L
      IF(L2.EQ.1) THEN
         SGN=1
      ELSE
         SGN=-1
      ENDIF
      fxyalt=0
      xyu=xys
      if(abs(wchgt(l2,1)).gt.abs(wchgt(l2,2))) then
         kdom=1
         xyc(1:2)=bbavg(:,l2,1)
         vr=sqrt(sum(bbvar(1:2,l2,1)**2))
      else
         kdom=2
         xyc(1:2)=bbavg(:,l2,2)
         vr=sqrt(sum(bbvar(1:2,l2,2)**2))
      endif
      xyd(1:2)=xyu(1:2)-xyc(1:2)
      if(fldbe.eq.0
     &     .or.(fldbe.eq.3.and.sqrt(sum(xyd(1:2)**2))/vr.gt.zfsgm)) then
         CALL BBKCK(L,1,XYU,fxy,NOUT,NXYD(1),NXYD(2),BBQD,0,IOUT)
         if(abs(wchgt(l2,1)).gt.abs(wchgt(l2,2))) then
            fxyele=fxy
            fxypos=0.
         else
            fxyele=0.
            fxypos=fxy
         endif
      elseif(fldbe.eq.1) then
         call bekck(l,xyu(1:2),fxyele,fxypos,fround,bbavg,bbvar)
         fxy=fxyele+fxypos
      elseif(fldbe.eq.2) then
         mpll=3-l
         call fcnemxy(2,xyu,fxy,iflag)
      elseif(fldbe.gt.3) then
         fxyele=0
         fxypos=0
         do i=1,nmulga
            call bekck(l,xyu(1:2),fuvele,fuvpos,fround,
     &           bbavga(:,:,:,i),bbvara(:,:,:,i))
            fxyele=fxyele+bbrrra(1:2,3-l,1,i)*fuvele
            fxypos=fxypos+bbrrra(1:2,3-l,2,i)*fuvpos
         enddo
         fxy=fxyele+fxypos
      else
         fxy=0
      endif
      FLD(1,1)=FAC0*FXY(1)
      FLD(2,1)=FAC0*FXY(2)
      FLD(3,1)=0
      FLD(1,2)=-SGN*FAC0*FXY(2)
      FLD(2,2)=SGN*FAC0*FXY(1)
      FLD(3,2)=0
      fldabs=sqrt(sum(fld(1:2,1)**2))

      fldele=fac0*fxyele
      fldpos=fac0*fxypos
      fldeabs=sqrt(sum(fldele**2))
      fldpabs=sqrt(sum(fldpos**2))

      
      
      if(bealt.ge.0) then 
         if(fldbe.eq.0) then
            if(bealt.eq.1.or.(bealt.eq.3
     &           .and.abs(bbdxy(2,3-l)-bbdxy(1,3-l)).gt.1.e-20)) then
               call bekck(l,xyu(1:2),fxyele,fxypos,fround,bbavg,bbvar)
               fxyalt=fxyele+fxypos
c               if(ncall.lt.1000) then
c                  write(msgfl,331) it,is,l2,
c     &                 fxyele,    
c     &                 fxypos,
c     &                 fxyalt
c 331              format(' bbkick0 it,is= ',i5,i4,' l2= ',i3,
c     &                 ' fxyele= ',2(1pe11.3),
c     &                 ' fxypos= ',2(1pe11.3),
c     &                 ' fxyalt= ',2(1pe11.3))
c               endif
            elseif(bealt.gt.3) then
               fxyele=0
               fxypos=0
               do i=1,nmulga
                  call bekck(l,xyu(1:2),fuvele,fuvpos,fround,
     &                 bbavga(:,:,:,i),bbvara(:,:,:,i))
                  fxyele=fxyele+bbrrra(1:2,3-l,1,i)*fuvele
                  fxypos=fxypos+bbrrra(1:2,3-l,2,i)*fuvpos
c                  if(ncall.lt.1000) then
c                     write(msgfl,333) it,is,l2,i,
c     &                    bbrrra(1:2,3-l,1,i),
c     &                    bbrrra(1:2,3-l,2,i),
c     &                    fxyele,    
c     &                    fxypos,
c     &                    fxyele+fxypos
c 333                 format(' bbkick0 it,is= ',i5,i4,' l2= ',i3,' i= ',i3
c     &                    ' bbrrra(1:2,3-l,1,i)= ',2(1pe11.3),
c     &                    ' bbrrra(1:2,3-l,2,i)= ',2(1pe11.3),
c     &                    ' fxyele= ',2(1pe11.3),
c     &                    ' fxypos= ',2(1pe11.3),
c     &                    ' fxyalt= ',2(1pe11.3))
c                  endif
               enddo
               fxyalt=fxyele+fxypos
            elseif(bealt.eq.2.or.bealt.eq.3) then
               mpll=3-l
               call fcnemxy(2,xyu,fxyalt,iflag)
            endif
         elseif(bealt.eq.0) then
            call bbkck(l,1,xyu,fxyalt,nout,nxyd(1),nxyd(2),bbqd,0,iout)
            fxyele=0
            fxypos=0
         endif
      endif
      fldalt=fac0*fxyalt
      altabs=sqrt(sum(fldalt**2))
      
      if(outchg.or.(fldabs.gt.1.e5.and.fldabs.gt.fldmax(l2,kdom))) then
         if(fldabs.gt.1.e5.and.fldabs.gt.fldmax(l2,kdom)) then
            maxdet=.true.
            fldmax(l2,kdom)=fldabs
         else
            maxdet=.false.
         endif
         drele=xyu(1:2)-bbavg(:,l2,iele)
         drpos=xyu(1:2)-bbavg(:,l2,ipos)
         dr2ele=sum(drele**2)
         dr2pos=sum(drpos**2)
         vr2ele=sum(bbvar(1:2,l2,iele)**2)
         vr2pos=sum(bbvar(1:2,l2,ipos)**2)
         write(msgfl,101) it,is,l2,kdom,
     &        bbvar(1,l2,iele),bbvar(2,l2,iele),
     &        mpnn(l2,iele),wchgt(l2,iele),
     &        bbvar(1,l2,ipos),bbvar(2,l2,ipos),
     &        mpnn(l2,ipos),wchgt(l2,ipos),
     &        sqrt(dr2ele/vr2ele),
     &        sqrt(dr2pos/vr2pos),
     &        fldabs,maxdet
 101     format(' bbkick0 it,is= ',i5,i4,' l2= ',i3,
     &        ' k= ',i3,    
     &        ' e- bbxvar,bbyvar= ',2(1pe11.3),
     &        ' e- mpnn,wchgt= ',i7,(1pe11.3),
     &        ' e+ bbxvar,bbyvar= ',2(1pe11.3),
     &        ' e+ mpnn,wchgt= ',i7,(1pe11.3),
     &        ' nsigele= ',(1pe11.3),
     &        ' nsigpos= ',(1pe11.3),
     &        ' fldabs= ',(1pe11.3),
     &        ' maxdet= ',l2)    
         
         write(msgfl,103) it,is,l2,kdom,
     &        fld(1:2,1),fldalt,
     &        fldabs,altabs,    
     &        fldabs/max(altabs,1d-300),    
     &        sqrt(dr2ele/vr2ele),
     &        sqrt(dr2pos/vr2pos),
     &        fldele,fldpos,    
     &        maxdet
 103     format(' bbkick0 it,is= ',i5,i4,' l2= ',i3,
     &        ' k= ',i3,    
     &        ' fld(:,1)= ',2(1pe11.3),
     &        ' fldalt= ',2(1pe11.3),
     &        ' |fld|,|fldalt|= ',2(1pe11.3),    
     &        ' |fld|/|fldalt|= ',(1pe11.3),    
     &        ' nsigele= ',(1pe11.3),    
     &        ' nsigpos= ',(1pe11.3),
     &        ' fldele= ',2(1pe11.3),
     &        ' fldpos= ',2(1pe11.3),
     &        ' maxdet= ',l2)    
      endif
      IF(KIN.EQ.1) RETURN
      FF(1)=CHARGE(KIN)*(FLD(1,1)-EP(3)/EP(0)*FLD(2,2))
      FF(2)=CHARGE(KIN)*(FLD(2,1)+EP(3)/EP(0)*FLD(1,2))
      FF(3)=CHARGE(KIN)*(EP(1)*FLD(2,2)-EP(2)*FLD(1,2))/EP(0)
c      if(abs(ep(0)-8.224e8).lt.0.002e8.and.ep(3).lt.0.)
c     &     write(msgfl,149) it,is,l2,kin,
c     &     charge(kin),fld(1:2,1),fld(1:2,2),
c     &     ep(0),ep(1:3),ff(1:2)
c 149  format(' bbkick0 it,is= ',i5,i4,' l2= ',i3,
c     &     ' kin= ',i3,    
c     &     ' charge= ',(1pe11.3),
c     &     ' fld(:,1)= ',2(1pe11.3),
c     &     ' fld(:,2)= ',2(1pe11.3),
c     &     ' ep(0)= ',(1pe11.3),
c     &     ' ep(1:3)= ',3(1pe11.3),
c     &     ' ff= ',2(1pe11.3))
      RETURN
      END
