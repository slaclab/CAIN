      MODULE BBCOM
	  IMPLICIT NONE
	  integer fldbe,bealt
          integer nmulga
	  integer mpnn(2,2),mpll,mpkk,mpixy,mpis
          integer, allocatable:: imu(:),isig(:),irrr(:)
	  INTEGER MXY
	  INTEGER BBON,BBFLG(2),NXY(2),NMOM,LBBEL,NBBPL,IFLBPL
	  REAL(8), ALLOCATABLE:: BBQ(:),BBWORK(:)
	  real(8) wchgt(2,2)
          real(8), allocatable:: wchga(:,:,:),pxyua(:,:,:,:)
	  real(8), allocatable:: bbavga(:,:,:,:),bbvara(:,:,:,:)
	  real(8), allocatable:: bbrrra(:,:,:,:)
          real(8), allocatable:: mua(:),siga(:),rrra(:)
          real(8), allocatable:: muu(:),sigu(:)
          real(8), allocatable:: expj(:)
	  real(8) bbavg(2,2,2),bbvar(2,2,2),zfsgm
	  real(8) mpikur,mpilam
	  REAL(8) XYMIN(2,2),XYMAX(2,2),XYCENT(2,2),
     %   BBDXY(2,2),BBXYM(2,2,2),PSIZE,
     %   SBBPL(5),EMAX(2),WGTOUT(2),
     %   BBR00(2),BBEL(2),BBU00
      END MODULE BBCOM
