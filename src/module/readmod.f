	MODULE READMOD
	  USE FLCHTYP
	  IMPLICIT NONE
	  INTEGER,PARAMETER:: MBL=1000,MRDPAR=1000,MFFF=1000
	  INTEGER NBL,KBL(MBL),REL(MBL),LNKW(2,2,MBL),LNBL(2,2,MBL),
     %      KOP(MBL),ID(MBL)
C          length of KOP and ID should be >= MOP.
C          MOP is defined in each subroutine but does not exceed MBL.
        TYPE(FLCHTYPE) PAR(MRDPAR)
        TYPE(FLCHTYPE) FFF(MFFF)
	  INTEGER, PARAMETER:: MCTEXT=2048
        INTEGER, PARAMETER:: MGSTRRD=10000
        INTEGER NGSTRRD
        CHARACTER(MGSTRRD)  GSTRRD
        CHARACTER(MCTEXT) TEXT
C      COMMON/READCM/PAR,FFF,NBL,KBL,REL,LNKW,LNBL,KOP,ID,NGSTRRD
C      COMMON/READCM2/TEXT,GSTRRD
C   These common blocks are used independently in each subroutine.
C   Need not be in common block.
      END MODULE READMOD