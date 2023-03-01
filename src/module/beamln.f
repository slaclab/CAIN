	MODULE BEAMLN
	  USE FLCHTYP
	  IMPLICIT NONE
	  INTEGER, PARAMETER:: MAXMAGNAME=8
	  INTEGER, PARAMETER:: MTWISS=10
	  TYPE MAGNET
c	    SEQUENCE
	    CHARACTER(MAXMAGNAME) NAME
	    TYPE(FLCHTYPE) LENGTH, K1
	    REAL(8) ANGLE, ROTATE, APERT(2), TEDGE(2)
C                           TEDGE= tan(edge angle)
	    INTEGER NC
	  END TYPE MAGNET

	  TYPE BEAMLINE
C	    SEQUENCE
	    CHARACTER(MAXMAGNAME) NAME      !  beamline name
	    INTEGER NC                      !  number of characters in the name
	    INTEGER NEL                     !  number of elements (non-expanded)
	    REAL(8) APERT(2)                !  default aperture
	    INTEGER, POINTER:: ELID(:)      !  ELID(NEL)  element ID
	                                    !  (magnet ID if ELTYPE=1, else beamline ID)
	    INTEGER, POINTER:: ELTYPE(:)    !  ELTYPE(NEL) 0: magnet, 1: beamline
	                                    !    -1: reversed beamline
          INTEGER NEXP                    !  number of magnets when expanded
	    REAL(8) STOT                    !  total length when expanded
	    INTEGER, POINTER:: MAGID(:)     !  MAGID(NEXP).  magnet ID
	    INTEGER, POINTER:: MAGNID(:)    !  MAGNID(NEXP). Count occurance of same magnet
          REAL(8), POINTER:: SBL(:)       !  SBL(0:NEXP)
          LOGICAL  LTWISS                 !  TWISS is allocated and calculated 
			                                !  only when LTWISS=.TRUE.
	    REAL(8), POINTER:: TWISS(:,:)   !  Twiss(MTWISS,0:NEXP)   Twiss params etc
C                 1,2,3,4,5:  Bx, Ax, Ex, Epx, Nux
C                 6,7,8,9,10: By, Ay, Ey, Epy, Nuy
	    REAL(8), POINTER:: TMAT(:,:,:)  !  TMAT(6,6,0:NEXP) transfer matrix from the beamline entrance
	                                    !  Allocated together with TWISS
	  END TYPE

	  INTEGER MMAG,NMAG,NGSTRMG
	  INTEGER MBEAMLINE,NBEAMLINE
	  TYPE(MAGNET), ALLOCATABLE:: MAG(:)
	  TYPE(BEAMLINE), ALLOCATABLE:: BL(:)
	  INTEGER, PARAMETER:: MGSTRMG=20000
	  CHARACTER(MGSTRMG) GSTRMG

	END MODULE
