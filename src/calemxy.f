      subroutine calemxy(xy,fvec,mx,my,bbqd,xymind,bbdxyd)
      implicit none
      real*8 xy(2)
      real*8 fvec(2)
      integer mx,my
      real*8 bbqd(mx,my),xymind(2),bbdxyd(2)
c
      real*8 xy0(2),xyij(2),dxy(2)
      real*8 rr02,rr2
      integer i,j
      integer ncall/0/
      save ncall
c
      include 'include/ctrlcm.h'
c
      ncall=ncall+1
      if(mod(ncall,100000).eq.0) then
         write(msgfl,101) ncall,mx,my,bbdxyd,xy,xymind
 101     format(' calemxy ncall= ',i9,' mx,my= ',2i9,
     &      ' bbdxyd= ',2(1pe11.3),
     &      ' xy= ',2(1pe11.3),
     &      ' xymind= ',2(1pe11.3))
      endif
      if(mx.ne.my.or.abs(bbdxyd(2)-bbdxyd(1)).gt.1.e-20) then
         print *, "calemxy  mx = ",mx," my= ",my," bbdxyd= ",bbdxyd,
     &        " program will now stop"
         stop
      endif
      fvec=0.
      rr02=0.25*bbdxyd(1)**2
      do i=1,mx
         xyij(1)=i
         do j=1,my
            xyij(2)=j
            xy0=xymind+(xyij-0.5)*bbdxyd
            dxy=xy-xy0
            rr2=sum(dxy**2)
            if(rr2.lt.rr02) then
               fvec=fvec-bbqd(i,j)*dxy/rr02
            else
               fvec=fvec-bbqd(i,j)*dxy/rr2
            endif
c            if(mod(ncall,10000000).eq.0.and.abs(bbqd(i,j)).gt.1.) then
c               write(msgfl,103) ncall,rr02,rr2,xyij,xy0,dxy,
c     &              bbqd(i,j),fvec
c 103           format(' calemxy ncall= ',i9,
c     &      ' rr02,rr2= ',2(1pe11.3),       
c     &      ' xyij= ',2(1pe11.3),       
c     &      ' xy0= ',2(1pe11.3),       
c     &      ' dxy= ',2(1pe11.3),       
c     &      ' bbqd(i,j)= ',(1pe11.3),       
c     &      ' fvec= ',2(1pe11.3))
c            endif
         enddo
      enddo
      return
      end
