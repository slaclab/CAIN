	SUBROUTINE EVARRSET(ID,RANK,IND,VAL,ERR)
C  Set an array element of floating type
C    IRTN= 0:  normal
C        100:  invalid array ID
C        101:  RANK does not match
C        102:  invalid subscript range
	USE ARRAYMOD
	IMPLICIT NONE
	INTEGER ID,RANK,IND(*)     ! IND(RANK)
	REAL(8) VAL
	CHARACTER(*) ERR
	INTEGER I,N,IRTN

	IF(ID.LE.0.OR.ID.GT.NARRAY) GOTO 900
	IF(ARR(ID)%TYPE.NE.1) GOTO 900
	IF(RANK.NE.ARR(ID)%RANK) GOTO 910
	IF(RANK.GE.1) THEN
	  CALL ARRIND2N(IND,RANK,ARR(ID)%DIM,N,IRTN)
	  IF(IRTN.NE.0) GOTO 920
	ELSE
	  N=1
	ENDIF
	ARR(ID)%VAL(N)=VAL
	ERR=' '
	RETURN
900	ERR='(EVARRSET) Invalid array ID'
	RETURN
910   ERR='(EVARRSET) Array rank mismatch'
	RETURN
920   ERR='(EVARRSET) Invalid array subscript range'
	RETURN
	END

	SUBROUTINE EVARRCSET(ID,RANK,IND,TEXT,ERR)
C  Set an array element of character type
C    IRTN= 0:  normal
C        100:  invalid array ID
C        101:  RANK does not match
C        102:  invalid subscript range
	USE ARRAYMOD
	IMPLICIT NONE
	INTEGER ID,RANK,IND(*)     ! IND(RANK)
	CHARACTER(*) TEXT
	CHARACTER(*) ERR
	INTEGER I,N,NC,J,IRTN

	IF(ID.LE.0.OR.ID.GT.NARRAY) GOTO 900
	IF(ARR(ID)%TYPE.NE.2) GOTO 900
	IF(RANK.NE.ARR(ID)%RANK) GOTO 910
	IF(RANK.GE.1) THEN
	  CALL ARRIND2N(IND,RANK,ARR(ID)%DIM,N,IRTN)
	  IF(IRTN.NE.0) GOTO 920
	ELSE
	  N=1
	ENDIF
	NC=LEN(TEXT)
	IF(NC.EQ.0) THEN
	  ARR(ID)%LC(1,N)=0
	  ARR(ID)%LC(2,N)=-1
	ELSE
	  I=0
	  IF(ARR(ID)%LC(1,N).EQ.0) THEN
	    I=NGSTR+1
	  ELSEIF(NC.GT.ARR(ID)%LC(2,N)-ARR(ID)%LC(1,N)+1) THEN
	    I=NGSTR+1
	  ENDIF
	  IF(I.NE.0) THEN
	    IF(I+NC-1.GT.MGSTR) GOTO 930
	    ARR(ID)%LC(1,N)=I
	    NGSTR=NGSTR+NC
	  ELSE
	    I=ARR(ID)%LC(1,N)
	  ENDIF
	  ARR(ID)%LC(2,N)=I+NC-1
        DO J=1,NC
	    GSTR(I+J-1:I+J-1)=TEXT(J:J)
	  ENDDO
	ENDIF
	ERR=' '
	RETURN
900	WRITE(ERR,902) ID
902   FORMAT('(EVARRCSET) Invalid array ID=',I5)
	RETURN
910   ERR='(EVARRCSET) Array rank mismatch for '//ARR(ID)%NAME
	RETURN
920   ERR='(EVARRCSET) Invalid array subscript range for '//ARR(ID)%NAME
	RETURN
930   ERR='(EVARRCSET) Global string stack overflow'
	RETURN
	END
