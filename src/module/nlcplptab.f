
	MODULE NLCPLPTAB
        IMPLICIT NONE
        INTEGER ALLOCATED/0/
        REAL(8), ALLOCATABLE:: TWn(:, :, :, :, :),WWn(:, :, :, :)
        REAL(8), ALLOCATABLE:: TWn1(:, :, :, :, :),WWn1(:, :, :, :)
c       real*8 TWn(MNmax,0:My,0:Mphi,0:Mxi2,0:MLM)
c       real*8 WWn(MNmax,0:My,0:Mxi2,0:MLM)
c          TWn(n,y,phi,xi^2,labmda) is the Table of Wn(n,y,phi,xi^2,labmda)
c          WWn(n,y,xi^2,labmda) is the Table of Wn(n,y,xi^2,labmda)

        REAL(8), ALLOCATABLE:: WWnphi(:, :, :, :),TIntWn(:, :, :)
        REAL(8), ALLOCATABLE:: WWn1phi(:, :, :, :),TIntWn1(:, :, :)
c       real*8 WWnphi(MNmax,0:Mphi,0:Mxi2,0:MLM)
c       real*8 TIntWn(MNmax,0:Mxi2,0:MLM)
c          WWn(n,phi,xi^2,labmda) is the Table of Wn(n,y,xi^2,labmda)
c          TIntWn(n,xi^2,labmda) is the table of the integral of Wn to y and phi

        REAL(8), ALLOCATABLE:: TWnMax(:, :, :),TWn1Max(:, :, :)
c      real*8 TWnMax(MNmax,0:Mxi2,0:MLM)
c         TWnMax(n,xi^2,labmda) is the table of the maximum of Wn to y and phi
c       Common/LinPolTable1/LMMAX,Ximax,Nmax

      CONTAINS
	  SUBROUTINE DEALLOCNLCPLST
! end: Chaned Jun.28.2002
!-------------------------------------------------------
!      Added by Li Dongguo, March.18.2003 
        INTEGER ISTAT
         IF(ALLOCATED.ne.0) THEN
           DEALLOCATE(TWn, WWn, TWnMax, TIntWn, WWnphi, TWn1, WWn1, 
     %       TWn1Max,TIntWn1, WWn1phi,STAT=ISTAT)
	     ALLOCATED=0
	   ENDIF
	   RETURN
!      End add
        END SUBROUTINE DEALLOCNLCPLST
	END MODULE NLCPLPTAB

