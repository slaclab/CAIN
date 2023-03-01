      SUBROUTINE IFNAME(NAM,KIN,ID)
C   Classify an identifier NAM
C  KIN
C        0:  unidentified name
C        E_PARAM:  variable
C        E_ARRAY:  pre-defined function
C        E_USERF:  user-defined function
C        E_ARRAY:  floating array name
C        E_CHAR:   character string name
	USE ARRAYMOD
      IMPLICIT NONE
      CHARACTER*(*) NAM
      INTEGER KIN,ID
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/evchkm.h'
	INCLUDE 'include/evtypcod.h'
      REAL*8 X1
C
      IF(NUFN.GE.1) THEN
        CALL IFUFN(NAM,ID)
        IF(ID.NE.0) THEN
          IF(LCHK.GE.1) CALL EVFCHK(ID)
          KIN=E_USERF
          RETURN
        ENDIF
      ENDIF
      CALL IFFUN(NAM,ID)
      IF(ID.NE.0) THEN
        KIN=E_UNARY
	  RETURN
	ENDIF
      CALL IFPAR(NAM,ID,X1)
      IF(ID.NE.0) THEN
        IF(LCHK.NE.0) CALL EVPCHK(ID)
        KIN=E_PARAM
	  RETURN
	ENDIF
	CALL IFARRAY(NAM,ID)
	IF(ID.NE.0) THEN
	  IF(ID.GT.0) THEN
	    KIN=E_ARRAY
	  ELSE
	    KIN=E_CHAR
	    ID=-ID
	  ENDIF
	  RETURN
	ENDIF
      KIN=0
      RETURN
      END

          
