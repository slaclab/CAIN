      subroutine fcnemxy(nvar,xy,fvec,iflag)
      use bbcom
      implicit none
      integer nvar
      real*8 xy(nvar)
      real*8 fvec(nvar)
      integer iflag
      integer i000(2)
c
      call cputim('fcnemxy',1)
      i000(1)=0
      i000(2)=nxy(1)*nxy(2)
      call calemxy(xy,fvec,nxy(1),nxy(2),bbq(i000(mpll)),
     &     xymin(1,mpll),bbdxy(1,mpll))
      call cputim('fcnemxy',2)
      return
      end
