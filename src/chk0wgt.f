      subroutine chk0wgt(point)
      use beamcm
      implicit none
      real*4 point
      INCLUDE 'include/ctrlcm.h'
      include 'include/pushcm.h'
      integer n
c     
      do n=1,np
         if(kind(n).ne.1.and.lost(n).eq.0.and.wgt(n).lt.1.) then
            write(msgfl,123) point,it,n,kind(n),lost(n),isbin(n),
     &           gen(n),wgt(n),ep(0,n),ep(1:3,n)       
 123        format(' chk0wgt point= ',(1pe11.3),' it= ',i5,' n= ',i10,
     &           ' kind= ',i3,
     &           ' lost= ',i3,
     &           ' isbin= ',i3,
     &           ' gen= ',i4,
     &           ' wgt= ',(1pe11.3),       
     &           ' ep(0)= ',(1pe11.3),       
     &           ' ep(1:3)= ',3(1pe11.3))
         endif
      enddo
      return
      end

