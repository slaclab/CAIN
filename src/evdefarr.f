	SUBROUTINE EVDEFARR(NAME,LCHK,RANK,MDIM,DIM,VAL,ID,IRTN,ERR)
C Define an array with name NAME and initialize by VAL.
C Character string array if the first character is $.
C  (VAL is not used in this case)
C      LCHK  flag to check the confliction with reserved names.
C         0: no check
C         1: check those that Cain cannot change
C         2: check those that users cannot change
C   If MDIM=1, dimension (1:DIM(1))
C         >=2, dimension (DIM(1):DIM(2))
C IRTN=0:  new parameter defined.
C      1:  parameter value revised.
C     10:  invalid form of parameter name
C     11:  conflict with an existing name
C    100:  too many arrays
C    110,120:  reserved name
C    200:  invalid parameter (RANK<=0 etc)
C    300:  allocation error
C All the array elements are initially VAL.
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER LCHK,RANK,MDIM,DIM(MDIM,*),IRTN
C                     DIM(2,RANK)
      CHARACTER*(*) NAME,ERR
      REAL*8 VAL
C
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/funlis.h'
	INCLUDE 'include/evtypcod.h'
	INCLUDE 'include/evchcod.h'
	INCLUDE 'include/ctrlcm3.h'
	
	INTEGER KIN,ID,NC,I0,I1,I,LCH,NVAL,ISTAT,N,ITY,M
	INTEGER LCHCOD
	IF(RANK.LT.0) GOTO 900
	IF(RANK.GE.1) THEN
	  IF(MDIM.LE.0) GOTO 900
	  DO I=1,RANK
		  IF(MDIM.EQ.1) THEN
	      IF(DIM(1,I).LE.0) GOTO 900
	    ELSE
	     IF(DIM(2,I).LT.DIM(1,I)) GOTO 900
	    ENDIF
	  ENDDO
	ENDIF
      NC=LEN(NAME)
	I0=0
      IF(NC.LE.0) GOTO 910
	DO I=1,NC
	  IF(NAME(I:I).NE.' ') THEN
	    IF(I0.EQ.0) I0=I
	    I1=I
	  ENDIF
	ENDDO
	IF(I0.EQ.0) GOTO 910
	
	IF(LCHK.NE.0) THEN
	  IF(LCHK.GE.2) THEN
	    M=MRSVP
	  ELSE
	    M=MRSVP1+MRSVP2+MRSVP3
	  ENDIF
        DO I=1,M
          IF(NAME(I0:I1).EQ.RSVPAR(I)) GOTO 960
        ENDDO
        DO I=1,MUFN
          IF(NAME(I0:I1).EQ.UFNAME(I)) GOTO 970
        ENDDO
	ENDIF

	ITY=1
	DO I=I0,I1
        LCH=LCHCOD(NAME(I:I))
C   20-29   number
C   30-35   alphabet for exponent indication
C   40-62   other upper-case alphabet
C   70-92   other lower-case alphabet
C   99      other characters which may appear in variable names,
C           defined by the subr.EVINIT.
        IF(I.EQ.I0.AND.LCH.EQ.C_DOLLAR) THEN
	    ITY=2
	    CYCLE
	  ENDIF
        IF(LCH.LT.C_NUM.OR.LCH.GT.C_VAR) GOTO 910
        IF(I.EQ.I0.AND.LCH.LE.C_NUM+9) GOTO 910
      ENDDO
	CALL IFNAME(NAME(I0:I1),KIN,ID)
	IF(KIN.EQ.E_PARAM.OR.KIN.EQ.E_UNARY.OR.KIN.EQ.E_USERF) GOTO 920
	IF(KIN.EQ.E_ARRAY.OR.KIN.EQ.E_CHAR) THEN
C             Free at first if already defined
	  CALL EVARFREE(ID)
	  IF(NARRAY.LT.ID) NARRAY=NARRAY+1
C          (This is needed since NARRAY decreases by 1 if ID is the last one)
	  IRTN=1
	ELSE
	  IRTN=0
	  IF(NARRAY.GT.0) THEN
	    ID=0
	    DO I=1,NARRAY
	      IF(ARR(I)%TYPE.EQ.0) THEN
	        ID=I
	        EXIT
	      ENDIF
	    ENDDO
	  ENDIF
	  IF(ID.EQ.0) THEN
	    IF(NARRAY.GE.MARRAY) GOTO 940
	    NARRAY=NARRAY+1
	    ID=NARRAY
	  ENDIF
	ENDIF
	ARR(ID)%NAME=NAME(I0:I1)
	ARR(ID)%NC=I1-I0+1
	ARR(ID)%RANK=RANK
	NVAL=1
	IF(RANK.GE.1) THEN
	  ALLOCATE(ARR(ID)%DIM(3,RANK),STAT=ISTAT)
	  IF(ISTAT.NE.0) GOTO 950
	  DO I=1,RANK
	    IF(MDIM.EQ.1) THEN
	      ARR(ID)%DIM(1,I)=1
	      ARR(ID)%DIM(2,I)=DIM(1,I)
	      N=DIM(1,I)
	    ELSE
	      ARR(ID)%DIM(1,I)=DIM(1,I)
	      ARR(ID)%DIM(2,I)=DIM(2,I)
	      N=DIM(2,I)-DIM(1,I)+1
	    ENDIF
	    ARR(ID)%DIM(3,I)=N
	    NVAL=NVAL*N
	  ENDDO
	ENDIF
	IF(ITY.EQ.1) THEN
	  ALLOCATE(ARR(ID)%VAL(NVAL),STAT=ISTAT)
	ELSE
	  ALLOCATE(ARR(ID)%LC(2,NVAL),STAT=ISTAT)
	ENDIF
	IF(ISTAT.NE.0) THEN
	  IF(RANK.GE.1) DEALLOCATE(ARR(ID)%DIM,STAT=ISTAT)
	  GOTO 950
	ENDIF
	IF(ITY.EQ.1) THEN
	  DO I=1,NVAL
	    ARR(ID)%VAL(I)=VAL
	  ENDDO
	ELSE
	  DO I=1,NVAL
	    ARR(ID)%LC(1,I)=1
	    ARR(ID)%LC(2,I)=0
	  ENDDO
	ENDIF
	ARR(ID)%TYPE=ITY
	ERR=' '
	RETURN

 900  IRTN=200
	ERR='(EVDEFARR) Invalid array defining parameter'
	RETURN
 910  IRTN=10
	ERR='(EVDEFARR) Invalid array name.'//NAME
      RETURN
 920  IRTN=11
	ERR='(EVDEFARR) Conflict with existing name.:'//NAME(I0:I1)
      RETURN
 940  IRTN=100
	ERR='(EVDEFARR) Too many arrays defined.'
	RETURN
 950  IRTN=300
	ERR='(EVDEFARR) Dynamic allocation error'
	RETURN
 960  IRTN=110
	ERR='(EVDEFARR) '//NAME(I0:I1)//' is a reserved name.'
	RETURN
 970  IRTN=120
	ERR='(EVDEFARR) '//NAME(I0:I1)//' is a reserved function name.'
	RETURN
	END
