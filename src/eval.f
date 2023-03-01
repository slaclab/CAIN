      FUNCTION EVAL(T)
	USE FLCHTYP
C           Not used in CAIN
      IMPLICIT NONE
      CHARACTER*(*) T
      REAL*8 EVAL
      CHARACTER*80 ERR
	TYPE(FLCHTYPE) FC
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/evchkm.h'
      INCLUDE 'include/evalcm.h'
C
      LCHK=0
      CALL EVAL0(T,FC,ERR)
      IF(ERR.EQ.' ') THEN
        EVAL=FC%X
        IRTNEV=0
      ELSE
        WRITE(MSGFILE,100) ERR,T
100     FORMAT(A,/,' in the expression "',A,'"')
        EVAL=0
        IRTNEV=1
      ENDIF
      RETURN
      END
