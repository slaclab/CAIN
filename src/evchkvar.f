      SUBROUTINE EVCHKVAR(T,MP,NP,NAMP,MF,NF,NAMF,FC,IRTN)
C  Check the occurences of variables and functions used in an expression
C  Input
C    T    expression
C    MP   dimension of NAMP
C    MF   dimension of NAMF
C  Output
C    NP   number of actually found variables 
C    NF   number of actually found functions
C    NAMP(i) (i=1,min(MP,NP))  variable name.
C    NAMF(i) (i=1,min(MF,NF))  function name.
C    FC   value of the expression
C    IRTN .ne.0 if the expression is invalid
	USE FLCHTYP
	USE ARRAYMOD
      IMPLICIT NONE
	TYPE(FLCHTYPE) FC
      CHARACTER*(*) T
      INTEGER MP,NP,MF,NF,IRTN
      CHARACTER*(*) NAMP(MP),NAMF(MF)
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/evchkm.h'
      INCLUDE 'include/evalcm.h'
      INTEGER I
      CHARACTER*80 ERR
C
      NKPAR=0
      NKFUN=0
      LCHK=1
      CALL EVAL0(T,FC,ERR)
      NP=NKPAR
      NF=NKFUN
      IF(MP.GE.1.AND.NP.GE.1) THEN
        DO 200 I=1,MIN(MP,NP)
	    IF(KPAR(I).GE.0) THEN
            NAMP(I)=NAMPAR(KPAR(I))
	    ELSE
	      NAMP(I)=ARR(-KPAR(I))%NAME
	    ENDIF
 200    CONTINUE
      ENDIF
      IF(MF.GE.1.AND.NF.GE.1) THEN
        DO 220 I=1,MIN(MF,NF)
          NAMF(I)=NAMUFN(KFUN(I))
 220    CONTINUE
      ENDIF
      IF(ERR.EQ.' ') THEN
        IRTN=0
        IRTNEV=0
      ELSE
        IRTN=1
        IRTNEV=1
        WRITE(MSGFILE,'(A)') ERR
      ENDIF
      RETURN
      END

	SUBROUTINE EVCHK0(TYP,ID)
	IMPLICIT NONE
	INTEGER TYP,ID
	INCLUDE 'include/nameleng.h'
	INCLUDE 'include/evparc.h'
	INCLUDE 'include/ufncm.h'
	INCLUDE 'include/evchkm.h'
	INCLUDE 'include/evtypcod.h'
	INTEGER I,K,II

	IF(TYP.EQ.E_USERF) THEN
	  K=0
	  IF(NKFUN.GE.1) THEN
	    DO I=1,MIN(NKFUN,MUFN00)
	      IF(KFUN(I).EQ.ID) THEN
	        K=1
				  EXIT
	      ENDIF
	    ENDDO
	  ENDIF
	  IF(K.EQ.0) THEN
	    NKFUN=NKFUN+1
	    IF(NKFUN.LE.MUFN00) KFUN(NKFUN)=ID
	  ENDIF
	ELSEIF(TYP.EQ.E_PARAM.OR.TYP.EQ.E_ARRAY.OR.TYP.EQ.E_CHAR) THEN
	  IF(TYP.EQ.E_PARAM) THEN
	    II=1
	  ELSE
	    II=-1
	  ENDIF
	  K=0
	  IF(NKPAR.GE.1) THEN
	    DO I=1,MIN(NKPAR,MPAR)
	      IF(KPAR(I).EQ.II*ID) THEN
	        K=1
				  EXIT
	      ENDIF
	    ENDDO
	  ENDIF
	  IF(K.EQ.0) THEN
	    NKPAR=NKPAR+1
	    IF(NKPAR.LE.MPAR) KPAR(NKPAR)=II*ID
	  ENDIF
	ENDIF
	RETURN
	END

