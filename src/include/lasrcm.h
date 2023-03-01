C      INTEGER MLSR
C      PARAMETER (MLSR=10)
C	  INCLUDE 'lasrcm0.h'
      INTEGER LRLSR(MLSR),LTRLSR(MLSR),LTPAR(MLSR),LSPAR(MLSR),NLSR
      REAL*8 WLLSR(MLSR),OMGLSR(MLSR),PLSR(MLSR),TXYSLS(0:3,MLSR),
     %  EVLSR(3,3,MLSR),TPAR(3,MLSR),
     %  SPAR(6,MLSR),TDL(2,MLSR),ZETAMAX(MLSR),
     %  LSRRNG(2,0:3,MLSR),STKSLS(3,MLSR),
     %  TRLSR(0:3,0:3,MLSR),TRILSR(0:3,0:3,MLSR),TR0LSR(0:3,MLSR)
      COMMON/LASRCM/WLLSR,OMGLSR,PLSR,TXYSLS,EVLSR,TPAR,
     %  SPAR,TDL,ZETAMAX,LSRRNG,STKSLS,TRLSR,TRILSR,TR0LSR,LRLSR,
     %  LTRLSR,LTPAR,LSPAR,NLSR
C
C  NLSR		Number of lasers defined.
C
C  LRLSR	1: right-going, 2: left-going, 0: undef
C  WLLSR	Wavelength (meter)
C  OMGLSR	Laser photon energy (eV)
C  PLSR		Peak power density (W/m**2)
C  TXYSLS	Origin in (t,x,y,s) (meter)
C  EVLSR	Laser basis vector e1,e2,e3.
C  LSRRNG   Range of non-zero laser field LSRRNG(i,j,*)
C           i=1: min, 2: max,
C           j=0: tau-zeta,  1: xi,  2: eta, 3: zeta
C  STKSLS	Stokes parameters
C
C --- Lorentz transformation
C  LTRLSR	0 or 1. Flag whether Lorentz transformation introduced 
C           after laser definition
C  TRLSR	Accumulated Lorentz transformation matrix (4x4)
C  TRILSR	Inverse matrix of TRLSR
C  TR0LSR	Origin shift by Lorentz transformation
C
C --- Time structure
C  LTPAR	Time structure type
C			 1: Gaussian  2: trapezoidal  5: defined by file
C  TPAR		Time structure parameters
C			For Gaussian  TPAR(1)=SIGT, TPAR(2)=GCUTT
C			For trapezoidal, TPAR(1)=TTOT, TPAR(2)=TEDGE
C
C --- Space structure
C  LSPAR    Space structure type
C            1: Gaussian    4: donut-shape   5: defined by file
C  SPAR     Spatial structure parameters
C           For Gauss  SPAR(1)=Rayleigh length in e1 direction
C                      SPAR(2)=Rayleigh length in e2 direction
C                      SPAR(3)=not used
C                      SPAR(4)=not used
C                      SPAR(5)=GCUT (Transverse Gaussian cut off)
C           For donut  SPAR(1)=outer radius of axicon mirror
C                      SPAR(2)=inner radius of axicon mirror
C                      SPAR(3)=focal length
C                      SPAR(4)=initial rms radius
C                      SPAR(5)=maximum r
C                      SPAR(6)=not used
C  ZETAMAX  Max abs(zeta) for calculation zeta distribution
C           (needed for donuts-shape and file with ORDER=XY)
C  RLLSR	Rayleigh length in e1,e2 direction
C  TDL      Times Diffraction Limit in e1 and e2 direction
C  GCLSR	GCUT (Transverse Gaussian cut off)
C  TDLLSR	TDL (times diffraction limit) factor
C
      INTEGER NPHCP,NPHBW,NPHBH,LENHCP,LENHBW,LENHBH,
     %   NCEHCP,NCEHBW,NCEHBH
      REAL*8 PMAXCP,PSTOPCP,PMAXBW,PSTOPBW,PMAXBH,PSTOPBH,
     %   PMMCP,PSTPCP,PMMBW,PSTPBW,PMMBH,PSTPBH
      COMMON/LASRCM2/PMAXCP,PSTOPCP,PMAXBW,PSTOPBW,PMAXBH,PSTOPBH,
     %   PMMCP,PSTPCP,PMMBW,PSTPBW,PMMBH,PSTPBH,NPHCP,NPHBW,NPHBH,
     %   LENHCP,LENHBW,LENHBH,NCEHCP,NCEHBW,NCEHBH
      CHARACTER*256 ENHCP,ENHBW,ENHBH
      COMMON/LASRCM3/ENHCP,ENHBW,ENHBH

C      INCLUDE 'lasrcm4.h'
C	  INTEGER NLSRFL(MLSR)
C	  TYPE(LASERDATA) LSRDT(2,MLSR)
C	  COMMON/LASRCM4/LSRDT,NLSRFL

	  INTEGER GAUSSIAN,TRAPEZOIDAL,DONUT_SHAPE,DEFINED_BY_FILE
	  PARAMETER (GAUSSIAN=1,TRAPEZOIDAL=2,DONUT_SHAPE=4)
	  PARAMETER (DEFINED_BY_FILE=5)
