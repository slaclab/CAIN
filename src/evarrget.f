	SUBROUTINE EVARRGET(ID,RANK,IND,X,ERR)
C  Get an array element value
C    IRTN= 0:  normal
C        100:  invalid array ID
C        101:  RANK does not match
C        102:  invalid subscript range
	USE FLCHTYP
	USE ARRAYMOD
	IMPLICIT NONE
	INTEGER ID,RANK,IND(*)    ! IND(RANK)
	TYPE(FLCHTYPE) X
	CHARACTER(*) ERR
	INTEGER I,N,IRTN

	X%L=0
	IF(ID.LE.0.OR.ID.GT.NARRAY) GOTO 900
	IF(ARR(ID)%TYPE.EQ.1.OR.ARR(ID)%TYPE.EQ.2) THEN
	  IF(RANK.NE.ARR(ID)%RANK) GOTO 910
	  IF(RANK.GE.1) THEN
	    CALL ARRIND2N(IND,RANK,ARR(ID)%DIM,N,IRTN)
	    IF(IRTN.NE.0) GOTO 920
	  ELSE
	    IF(ARR(ID)%TYPE.EQ.1) GOTO 930
C               floating scalar is invalid yet
	    N=1
	  ENDIF
	  IF(ARR(ID)%TYPE.EQ.1) THEN
	    X%L=1
	    X%X=ARR(ID)%VAL(N)
	    X%C(1)=1
	    X%C(2)=0
		ELSE
	    X%L=2
	    X%X=0
	    X%C(1)=ARR(ID)%LC(1,N)
	    X%C(2)=ARR(ID)%LC(2,N)
	  ENDIF
	ELSE
	  GOTO 900
	ENDIF
	ERR=' '
C       Above line added Mar.22.2002
	RETURN
900	ERR='(EVARRGET) Invalid array ID'
	RETURN
910   ERR='(EVARRGET) Rank mismatch for array '//ARR(ID)%NAME
	RETURN
920   ERR='(EVARRGET) Invalid subscript range for array '//ARR(ID)%NAME
	RETURN
930   ERR='(EVARRGET) Program error. Floating scalar.'
	RETURN
	END
