	SUBROUTINE EVLHS(T,KIN,NAM,NC,ID,MIND,NIND,NNIND,IND,ERR)
C  Analyse l.h.s. of a substitution expression
C  Expected form
C        scalar_variable_name
C        array_name(i1, i2, ...)
C        array_name(i11:i12, i2, i31:i32,....)
C  The second form is possible in the array allocation command (ARRAY).
C  Note that i's might be expressions.
C  Input
C     T      expression
C     MIND   length of NNIND and IND
C  Output
C    KIN     0: undefined name
C            1: variable_name
C            2: array_name(expr1, expr2,...,exprn)
C            3: character variable or character array
C           <0: error
C    NAM(1:NC)  extracted variable or array name
C    ID      variable ID or array ID
C    NIND    number of index found. (number of commas plus one)
C    NNIND(i)  (i=1,2..NIND) number of data separated by colon
C            in the i-th index. All one in the second case.
C    IND(j)  area where the numbers are stored.
C            Total number is  NNIND(1)+NNIND(2)+....+NNIND(NIND)
C            which must be <= MIND.
C    ERR     error message
	USE FLCHTYP
	IMPLICIT NONE
	CHARACTER(*) T,NAM,ERR
	INTEGER KIN,NC,ID,MIND,NIND,NNIND(MIND),IND(MIND)
	INCLUDE 'include/evtypcod.h'
	INCLUDE 'include/evchcod.h'
	TYPE(FLCHTYPE) FC
	INTEGER KK,I,NC0,IC,L,I0,INAPOS,I00,I01
	INTEGER ML
	PARAMETER (ML=50)
	INTEGER PAREN(ML)
	INTEGER LCHCOD

	NC0=LEN(T)
	L=0
	I0=0
	INAPOS=0
	NIND=1
	NNIND(1)=0
	KK=0
	I00=0
	I01=0
	KIN=-1
	DO I=1,NC0
	  IC=LCHCOD(T(I:I))
	  IF(INAPOS.NE.0) THEN
	    IF((INAPOS.EQ.1.AND.IC.EQ.C_QTE).OR.
     %       (INAPOS.EQ.2.AND.IC.EQ.C_DBLQTE)) INAPOS=0
C   ( Two successive 's or "s under corresponding quotes are not
C     understood as one ' or ", but so long as they appear as
C     a pair, the result will be correct.)
	  ELSEIF(IC.EQ.C_QTE.OR.IC.EQ.C_DBLQTE) THEN
C   7       '  (apostrophe)
          IF(L.EQ.0) GOTO 940
	    INAPOS=1
	    IF(IC.EQ.C_DBLQTE) INAPOS=2
	    IF(I0.EQ.0) I0=I
	    IF(I01.EQ.0) I01=I
	  ELSEIF(IC.EQ.C_BLANCK) THEN
C   0       blanck space
          IF(L.EQ.0.AND.I0.NE.0) I01=I-1
	  ELSEIF(IC.LE.C_OPENPAR3) THEN
C   1-3     opening parenthesis
	    IF(L.GE.ML) GOTO 910
	    IF(L.EQ.0) THEN
	      IF(I0.EQ.0) GOTO 930
	      I00=I0
	      I01=I-1
	      I0=0
	    ELSE
	      IF(I0.EQ.0) I0=I
	    ENDIF
	    L=L+1
	    PAREN(L)=IC-C_OPENPAR1+1
	  ELSEIF(IC.LE.C_CLOSPAR3.OR.IC.EQ.C_COMMA.OR.IC.EQ.C_COLON) THEN
C   4-6     closing parenthesis
C   18      ,  comma
C   150     :  colon
          IF(L.EQ.0) GOTO 920
	    IF(IC.LE.C_CLOSPAR3.AND.IC-C_CLOSPAR1+1.NE.PAREN(L)) GOTO 920
	    IF(IC.EQ.C_COLON.AND.L.NE.1) GOTO 982
	    IF(L.EQ.1) THEN
	      IF(I0.EQ.0) GOTO 900
	      IF(NIND.GE.MIND) GOTO 960
	      CALL EVAL0(T(I0:I-1),FC,ERR)
	      IF(ERR.NE.' ') GOTO 970
	      IF(FC%L.NE.1) GOTO 970     !  character as array index
	      NNIND(NIND)=NNIND(NIND)+1
	      KK=KK+1
	      IND(KK)=NINT(FC%X)
	      IF(IC.NE.C_COLON) THEN
	        NIND=NIND+1
	        NNIND(NIND)=0
	      ENDIF
	      I0=0
	      IF(IC.LE.C_CLOSPAR3) THEN
	        L=0
	        IF(I.LT.NC0) THEN
	          IF(T(I+1:NC0).NE.' ') GOTO 980
	        ENDIF
	        EXIT
	      ENDIF
	    ENDIF
	    IF(IC.LE.C_CLOSPAR3) L=L-1
	  ELSE
	    IF(I0.EQ.0) THEN
			  I0=I
	      IF(L.EQ.0) I00=I
	    ENDIF
	  ENDIF
	ENDDO
	IF(I00.EQ.0) GOTO 930
	IF(I01.EQ.0) I01=NC0
	NC=I01-I00+1
	IF(NC.GT.LEN(NAM)) GOTO 990
	NAM=T(I00:I01)
	CALL IFNAME(T(I00:I01),KIN,ID)
	IF(KIN.EQ.0) THEN
	  KIN=0
	ELSEIF(KIN.EQ.E_PARAM) THEN
	  KIN=1
	ELSEIF(KIN.EQ.E_ARRAY) THEN
	  KIN=2
	ELSEIF(KIN.EQ.E_CHAR) THEN
	  KIN=3
	ELSE
	  GOTO 950
	ENDIF
	NIND=NIND-1
	ERR=' '
	RETURN
900	ERR='Syntax error in '//T(1:NC0)
	RETURN
910   ERR='Too deep parenthesis nest in '//T(1:NC0)
	RETURN
920   ERR='Parentheses do not match in '//T(1:NC0)
	RETURN
930   ERR='No variable/array name given on l.h.s'
	RETURN
940	ERR='Invalid apostrophe in '//T(1:NC0)
	RETURN
950   ERR=T(I00:I01)//' is not variable/array name'
	RETURN
960	ERR='Too many array indeces in '//T(1:NC0)
	RETURN
970   ERR='Invalid expression '//T(1:NC0)
	RETURN
980   ERR='Invalid expression after closing parenthesis'
	RETURN
982   ERR='Invalid use of colon'
      RETURN
990   ERR='Name '//T(I00:I01)//'too long'
	RETURN
	END