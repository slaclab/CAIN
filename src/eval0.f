      RECURSIVE SUBROUTINE EVAL0(T,X0,ERR)
C  Evaluate an expression in the text string T
C  There must not be substitution '='
C    (If LEQOP(see data statement below).ne.0, single '=' is identified as logical '==')
C  Output
C    X0%L  1:  floating expression. The result stored in X0%X
C          2:  character string expression. The result is X0%C, i.e.,
C             the string is GSTR2(X0%C(1):X0%C(2))
C             When this string is needed, USE ARRAYMOD must be inserted and
C             the result must be copied somewhere immediately after return.
C             GSTR2 will be cleared by the next call of EVAL0.
C             
	USE FLCHTYP
	USE ARRAYMOD
      IMPLICIT NONE
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
      INCLUDE 'include/funlis.h'
      INCLUDE 'include/ufncm.h'
      INCLUDE 'include/evchkm.h'
	INCLUDE 'include/evtypcod.h'
	INCLUDE 'include/evchcod.h'
C
      CHARACTER*(*) T,ERR
	TYPE(FLCHTYPE) X0
      INTEGER ML,M
      PARAMETER (ML=50,M=10)
C       M: max. number of pending operator
      INTEGER TYP(ML),BOP(M,ML),N(ML),KK(ML),IDFUN(ML),
     %    IARG(ML),NARG(ML),INTXX(MARG)
C      REAL*8 X(0:M,ML),XX(MARG,ML)
	TYPE(FLCHTYPE) X(0:M,ML),XX(MARG,ML)
      INTEGER INAPOS,NC,I,J,L,I0,II,LCH,TYP1,ID,K,NC1,IFIN,K0
      CHARACTER*1 KKC(3)/')',']','}'/,KKO(3)/'(','[','{'/
      INTEGER LCHCOD
      REAL*8 EVRDNM
	INTEGER LEQOP/1/    !  allow single = as ==
C
C  Input text is divided into blocks. Possible blocks are
C        (The constants are defined in include/evtypcod.h)
C    E_LITERAL:  literal number with the leading sign taken off.
C    E_PARAM  :  parameter name
C    E_ARRAY  :  array name
C    E_UNARY  :  reserved unary operator such as Sin,Cos,etc. 
C                + and - at the head is classified as a unary operator.
C    E_USERF  :  user-defined function
C    E_BINARY :  binary operator. +,-,*,/,^
C   Quantities in parenthesis is classified to type E_LITERAL after
C   reduction.
C   TYP(L) contains the type just preceeding. At the head of
C   expression, it is zero.
C
	IF(EVALNEST.GE.MAXRECURS) GOTO 999
	EVALNEST=EVALNEST+1
	NGSTR2(EVALNEST)=0
      X0%L=1
	X0%X=0
	X0%C(1)=1
	X0%C(2)=0
      ERR=' '
      J=LEN(T)
      IF(J.LE.0) GOTO 800
      DO 160 I=J,1,-1
        IF(T(I:I).NE.' ') THEN
          NC=I
          GOTO 180
        ENDIF
 160  CONTINUE
      GOTO 800
 180  L=1
      TYP(L)=0
      N(L)=0
	X(0,L)%L=1
      X(0,L)%X=0
	X(0,L)%C(1)=1
	X(0,L)%C(2)=0
	INAPOS=0
      IDFUN(L)=0
      I=0
 200  I=I+1
 220  IF(I.GT.NC) THEN
        GOTO 320
      ENDIF
      LCH=LCHCOD(T(I:I))
C  Character code
 240  IF(LCH.EQ.C_BLANCK) THEN
C   0   blanck
        GOTO 200
	ELSEIF(LCH.EQ.C_QTE.OR.LCH.EQ.C_DBLQTE) THEN
	  INAPOS=LCH
C               use GSTR2 temporarily
        NC1=0
	  DO WHILE (I.LT.NC)
	    I=I+1
	    LCH=LCHCOD(T(I:I))
	    IFIN=0
	    IF(LCH.EQ.INAPOS) THEN
	      IF(I.EQ.NC) THEN
				  IFIN=1
	      ELSE
	        I=I+1
	        LCH=LCHCOD(T(I:I))
	        IF(LCH.NE.INAPOS) IFIN=2
	      ENDIF
	    ENDIF
	    IF(IFIN.NE.0) THEN
	      K0=NGSTR2(EVALNEST)
	      IF(2*NC1+K0.GT.MGSTR2) GOTO 910
	      GSTR2(EVALNEST)(K0+1+NC1:K0+2*NC1)
     %          =GSTR2(EVALNEST)(K0+1:K0+NC1)
	      CALL FLCHSET2(GSTR2(EVALNEST)(K0+1+NC1:K0+2*NC1),
     %                X(N(L),L),ERR)
	      IF(ERR.NE.' ') GOTO 904
	      TYP(L)=E_LITERAL
            IF(IFIN.EQ.1) GOTO 200
	      GOTO 240
	    ENDIF
	    NC1=NC1+1
	    K0=NGSTR2(EVALNEST)
	    IF(NC1+K0.GT.MGSTR2) GOTO 910
	    GSTR2(EVALNEST)(NC1+K0:NC1+K0)=T(I:I)
	  ENDDO
	  GOTO 902
      ELSEIF(LCH.GE.C_DEC.AND.LCH.LE.C_NUM+9) THEN
C   19      .  decimal point
C   20-29   number
        IF(TYP(L).EQ.E_LITERAL.OR.TYP(L).EQ.E_PARAM) GOTO 906
        I0=I
	  X(N(L),L)%L=1
        X(N(L),L)%X=EVRDNM(T,I0,I,ERR)
        IF(ERR.NE.' ') GOTO 980
        IF(TYP(L).EQ.E_UNARY) THEN
          CALL EVFUN(IDFUN(L),X(N(L),L),NVFUN(IDFUN(L)),X(N(L),L),ERR)
          IF(ERR.NE.' ') GOTO 980
        ENDIF
        TYP(L)=E_LITERAL
	  GOTO 220
      ELSEIF(LCH.GE.C_EXP.AND.LCH.LE.C_VAR.OR.LCH.EQ.C_DOLLAR) THEN
C       alphabet or other character allowed for the first character
C       of a variable.
C   30-35   alphabet for exponent indication
C   40-62   other upper-case alphabet
C   70-92   other lower-case alphabet
C   99      other characters which may appear in variable names,
C           defined by the subr.EVINIT.
        IF(TYP(L).EQ.E_LITERAL.OR.TYP(L).EQ.E_PARAM.
     %             OR.TYP(L).EQ.E_UNARY) GOTO 906
        I0=I
 280    I=I+1
        IF(I.LE.NC) THEN
          LCH=LCHCOD(T(I:I))
          IF(LCH.EQ.C_BLANCK) GOTO 280
          IF(LCH.GE.C_NUM.AND.LCH.LE.C_VAR) GOTO 280
C             character allowed for variables (incl. numbers)
        ENDIF
        II=I-1
        CALL IFNAME(T(I0:II),TYP1,ID)
	  IF(LCHK.NE.0) CALL EVCHK0(TYP1,ID)
        IF(TYP1.EQ.E_USERF) THEN
          TYP(L)=E_USERF
          IDFUN(L)=ID
          NARG(L)=NVUFN(ID)
        ELSEIF(TYP1.EQ.E_UNARY) THEN
          TYP(L)=E_UNARY
          IDFUN(L)=ID
          NARG(L)=NVFUN(ID)
        ELSEIF(TYP1.EQ.E_PARAM) THEN
          TYP(L)=E_PARAM
	    X(N(L),L)%L=1
          X(N(L),L)%X=VPAR(ID)
	  ELSEIF(TYP1.EQ.E_ARRAY) THEN
	    TYP(L)=E_ARRAY
	    IDFUN(L)=ID
	    NARG(L)=ARR(ID)%RANK
	  ELSEIF(TYP1.EQ.E_CHAR) THEN
	    IF(ARR(ID)%RANK.EQ.0) THEN
            CALL EVARRGET(ID,0,INTXX,X(N(L),L),ERR)
	      IF(ERR.NE.' ') GOTO 980
	      CALL FLCHSET2(GSTR(X(N(L),L)%C(1):X(N(L),L)%C(2)),X(N(L),L),
     %            ERR)
	      IF(ERR.NE.' ') GOTO 980
            TYP(L)=E_LITERAL
	    ELSE
	      TYP(L)=E_ARRAY
	      IDFUN(L)=ID
	      NARG(L)=ARR(ID)%RANK
	    ENDIF
        ELSE
	    GOTO 908
        ENDIF
	  GOTO 220
      ELSEIF(LCH.GE.C_CLOSPAR1.AND.LCH.LE.C_CLOSPAR3) THEN
C   4-6     closing parenthesis
        J=LCH-C_CLOSPAR1+1
        IF(L.EQ.1) GOTO 912
        IF(J.NE.KK(L)) GOTO 914
	  GOTO 320
C   1-3     opening parenthesis
      ELSEIF(LCH.GE.C_OPENPAR1.AND.LCH.LE.C_OPENPAR3) THEN
	  J=LCH-C_OPENPAR1+1
        IF(TYP(L).EQ.E_LITERAL.OR.TYP(L).EQ.E_PARAM) GOTO 916
        IF(L.EQ.ML) GOTO 918
        L=L+1
        IARG(L)=1
        KK(L)=J
        N(L)=0
        TYP(L)=0
	  X(0,L)%L=1
        X(0,L)%X=0
	  X(0,L)%C(1)=1
	  X(0,L)%C(2)=0
		GOTO 200
      ELSEIF(LCH.EQ.C_COMMA) THEN
C   18      ,  comma
        IF(L.EQ.1) THEN
		  GOTO 920
        ELSEIF(TYP(L-1).NE.E_UNARY.AND.TYP(L-1).NE.E_USERF
     %             .AND.TYP(L-1).NE.E_ARRAY) THEN
	    GOTO 920
        ELSE
          CALL EVENDB(TYP(L),N(L),X(0,L),BOP(1,L),ERR)
          IF(ERR.NE.' ') GOTO 980
          XX(IARG(L),L)=X(0,L)
          IARG(L)=IARG(L)+1
          IF(IARG(L).GT.MARG) GOTO 922
        ENDIF
	  X(0,L)%L=1
        X(0,L)%X=0
	  X(0,L)%C(1)=1
	  X(0,L)%C(2)=0
        N(L)=0
        TYP(L)=0
        GOTO 200
C--- binary operators
	ELSEIF(LCH.GE.C_PLUS.AND.LCH.LE.C_SMALL) THEN
C   10-14   binary operator
        IF(LCH.GE.C_AND) THEN
	    J=0
	    IF(I.LT.NC) THEN
	      IF(T(I:I+1).EQ.'==') THEN
	        J=6
	      ELSEIF(T(I:I+1).EQ.'/='.OR.T(I:I+1).EQ.'<>'
     %            .OR.T(I:I+1).EQ.'><') THEN
	        J=7
	      ELSEIF(T(I:I+1).EQ.'<='.OR.T(I:I+1).EQ.'=<') THEN
	        J=10
	      ELSEIF(T(I:I+1).EQ.'>='.OR.T(I:I+1).EQ.'=>') THEN
	        J=11
	      ELSEIF(T(I:I+1).EQ.'&&') THEN
	        J=12
	      ELSEIF(T(I:I+1).EQ.'||') THEN
	        J=13
	      ENDIF
	      IF(J.NE.0) I=I+1
	    ENDIF
	    IF(J.EQ.0) THEN
			  IF(LCH.EQ.C_EQ.AND.LEQOP.NE.0) J=6
	      IF(LCH.EQ.C_SMALL) J=8
	      IF(LCH.EQ.C_LARGE) J=9
	    ENDIF
	    IF(J.EQ.0) GOTO 900
	  ELSE
          J=LCH-C_PLUS+1
	  ENDIF
        IF(TYP(L).EQ.E_UNARY.OR.TYP(L).EQ.E_BINARY) GOTO 924
	  IF(TYP(L).EQ.E_ARRAY) GOTO 925
        CALL EVREDC(N(L),X(0,L),BOP(1,L),J,ERR)
        IF(ERR.NE.' ') GOTO 980
        N(L)=N(L)+1
        IF(N(L).GT.M) GOTO 926
        BOP(N(L),L)=J
        TYP(L)=E_BINARY
        GOTO 200
      ELSE
        GOTO 900
      ENDIF
      GOTO 220
C-------------------------------------
 320  CALL EVENDB(TYP(L),N(L),X(0,L),BOP(1,L),ERR)
      IF(ERR.NE.' ') GOTO 980
      X0=X(0,L)
      IF(L.EQ.1) GOTO 800
      IF(I.GT.NC) GOTO 928
      XX(IARG(L),L)=X0
      L=L-1
	IF(TYP(L).EQ.E_UNARY.OR.TYP(L).EQ.E_USERF.OR.TYP(L).EQ.E_ARRAY)
     %           THEN
        IF(TYP(L).EQ.E_UNARY) THEN
	    IF(NARG(L).LT.1000.AND.IARG(L+1).NE.NARG(L)) THEN
	      GOTO 934
          ELSE
            CALL EVFUN(IDFUN(L),XX(1,L+1),IARG(L+1),X(N(L),L),ERR)
	      IF(ERR.NE.' ') GOTO 980
	    ENDIF
C-----------------------
	  ELSEIF(TYP(L).EQ.E_ARRAY) THEN
	    IF(NARG(L).NE.IARG(L+1)) THEN
	      GOTO 930
	    ELSE
	      DO K=1,IARG(L+1)
	        IF(XX(K,L+1)%L.NE.1) GOTO 932
	        INTXX(K)=NINT(XX(K,L+1)%X)
	      ENDDO
	      CALL EVARRGET(IDFUN(L),IARG(L+1),INTXX,X(N(L),L),ERR)
            IF(ERR.NE.' ') GOTO 980
	      IF(X(N(L),L)%L.EQ.2) THEN
	        CALL FLCHSET2(GSTR(X(N(L),L)%C(1):X(N(L),L)%C(2)),
     %             X(N(L),L),ERR)
              IF(ERR.NE.' ') GOTO 980
            ENDIF
	    ENDIF
C----------------------
        ELSE
          CALL EVUFN(NAMUFN(IDFUN(L)),XX(1,L+1),IARG(L+1),
     %             GSTR2(EVALNEST),X(N(L),L),ERR)
	    IF(ERR.NE.' ') GOTO 980
	  ENDIF
      ELSE
        X(N(L),L)=X0
      ENDIF
      TYP(L)=E_LITERAL
      GOTO 200
C
800   X0=X0
	GOTO 990
C
900   ERR='(EVAL0) Illegal character "'//T(I:I)//'".'
      GOTO 990
902   ERR='(EVAL0) Missing closing apostrophe.'
      GOTO 990
904	ERR='(EVAL0) Too many character data. '//ERR
	GOTO 990
906   ERR='(EVAL0) Syntax error. "'//T(MAX(1,I-20):I)//'".'
      GOTO 990
908   ERR='(EVAL0) Parameter/function "'//T(I0:II)//'" does not exist.'
      GOTO 990
910   ERR='(EVAL0) Too long character string. Buffer short.'
	GOTO 990
912   ERR='(EVAL0) Too many closing parenthesis.'
      GOTO 990
914   ERR='(EVAL0) Opening '//KKO(KK(L))//' does not match with '//
     %        'closing '//KKC(J)//'".'
      GOTO 990
916   ERR='(EVAL0) Syntax error. "'//T(MAX(1,I-10):I)//'" No operator '
     %           //'before '//KKO(J)//'".'
      GOTO 990
918   ERR='(EVAL0) Nest level exceeded.'
      GOTO 990
920   ERR='(EVAL0) Syntax error. "'//T(MAX(1,I-10):I)
     %     //'" Invalid comma.'
      GOTO 990
922   ERR='(EVAL0) Too many arguments for function "'//NAMFUN(IDFUN(L))
     %      //'".'
      GOTO 990
924   ERR='(EVAL0) Syntax error. "'//T(MAX(1,I-10):I)
     %     //'" Double operator.'
	GOTO 990
925   ERR='(EVAL0) Syntax error. "'//T(MAX(1,I-10):I)
     %     //'" No array subscripts.'
	GOTO 990
926   ERR='(EVAL0) Program error 4'
      GOTO 990
928   ERR='(EVAL0) Insufficient closing parenthesis.'
      GOTO 990
930   ERR='(EVAL0) Number of subscripts does not match for "'
     %         //ARR(IDFUN(L))%NAME//'".'
	GOTO 990
932	ERR='Character type as array index'
	GOTO 990
934   ERR='(EVAL0) Number of argument does not match for "'
     %         //NAMFUN(IDFUN(L))//'".'
	GOTO 990
980   ERR='(EVAL0) '//ERR
	GOTO 990
990   EVALLAST=EVALNEST
	EVALNEST=EVALNEST-1
	RETURN
999   ERR='(EVAL0) Recursive level too deep.'
	RETURN
      END
