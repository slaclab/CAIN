      RECURSIVE SUBROUTINE EVCMPL0(T,MP,NP,PL,PC,GSTR3,NGSTR3,ERR)
C  Create a "load module"
C   input
C      T     text of expression
C      MP    prepared length of PL,PC
C      GSTR3 Character area to store character constants
C   output
C      NP    number of instrunctions
C      PL    instruction code
C      PC    constants. Character constants PC%C refers to GSTR3.
C      NSTR3 Used number of bytes in GSTR3
C
C  PL(1,IP)=
C   0: end of module
C   1: load PC(IP) into X(PL(2,IP))
C   2: load X(PL(3,IP))  into X(PL(2,IP))
C   3: load PL(3,IP)-th scalar parameter  into X(PL(2,IP))
C       (if PL(3,IP)>0, PL(3,IP)-th floating variable.
C           else scalar character string of ID=-PL(3,IP)
C   4,5,6,7:  X(PL(2,IP)) +-*/ X(PL(3,IP))  into X(PL(2,IP))
C   8:  X(PL(2,IP))^X(PL(3,IP))  into X(PL(2,IP))
C  10: ==  11: /=   12: >   13: >=  14: <  15: <=  
C  16: &&   17:  ||
C  21: unary + (actually no operation)
C  22: unary - (change sign of X(PL(2,IP)) )
C  23-45:  Single argument function (Int, etc) on X(PL(2,IP))
C  46,47:  double argument function on X(PL(2,IP)) and X(PL(2,IP)+1)
C          store results  into X(PL(2,IP))
C  48,49:  Min,Max of X(PL(2,IP)+i) (i=0,1,2,...PL(3,IP)-1)
C          store results  into X(PL(2,IP))
C  101 to 999   User defined function
C  >=1001  array
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
      CHARACTER*(*) T,ERR,GSTR3
      INTEGER MP,NP,PL(3,MP),NGSTR3
      TYPE(FLCHTYPE) PC(MP)
      INTEGER ML,M,M1,MARG0,M00
      PARAMETER (ML=50,M=10,M1=M+1,MARG0=10,M00=M1*ML)
C         MARG0: max.number of arguments for pre-defined functions
C                (does not include user functions.  See also EVLOAD0)
      INTEGER TYP(ML),BOP(M,ML),N(ML),KK(ML),IDFUN(ML),
     %    IARG(ML),NARG(ML)
CC      TYPE(FLCHTYPE) X(0:M,ML),XX(MARG,ML)
      INTEGER MGSTR3,NC,I,J,L,I0,IX,LCH,TYP1,ID,IP,INAPOS,NC1,IFIN
      CHARACTER*1 KKC(3)/')',']','}'/,KKO(3)/'(','[','{'/
      CHARACTER*80 ERR1
      INTEGER LCHCOD
      REAL*8 EVRDNM
	INTEGER LEQOP/1/    !  allow single = as ==
CC      REAL*8 EVFUN,EVUFN
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
CC      X0=0
	IF(EVCMPLNEST.NE.0) GOTO 980
	IF(EVALNEST.GE.MAXRECURS) GOTO 999
	EVALNEST=EVALNEST+1
	NGSTR2(EVALNEST)=0
	EVCMPLNEST=EVCMPLNEST+1
      ERR=' '
	NP=0
	MGSTR3=LEN(GSTR3)
      J=LEN(T)
      IF(J.LE.0) GOTO 990
      DO 160 I=J,1,-1
        IF(T(I:I).NE.' ') THEN
          NC=I
          GOTO 180
        ENDIF
 160  CONTINUE
      GOTO 990
 180  L=1
      TYP(L)=0
      N(L)=0
CC      X(0,L)=0
      IP=1
      PL(1,IP)=1
      PL(2,IP)=1
      PC(IP)%L=1
	PC(IP)%X=0
	PC(IP)%C(1)=1
	PC(IP)%C(2)=0
      IDFUN(L)=0
      I=0
 200  I=I+1
 220  IF(I.GT.NC) THEN
        GOTO 320
      ENDIF
      LCH=LCHCOD(T(I:I))
240   IF(LCH.EQ.C_BLANCK) THEN
        GOTO 200
	ELSEIF(LCH.EQ.C_QTE.OR.LCH.EQ.C_DBLQTE) THEN
	  INAPOS=LCH
	  NC1=0
	  I0=I+1
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
	      IF(IP.GE.MP) GOTO 962
            IP=IP+1
            PL(1,IP)=1
            PL(2,IP)=N(L)+M1*(L-1)+1
 	      CALL FLCHSET3(T(I0:I0+NC1-1),PC(IP),GSTR3,NGSTR3,ERR)
	      IF(ERR.NE.' ') GOTO 904
	      TYP(L)=E_LITERAL
	      IF(IFIN.EQ.1) GOTO 200
            GOTO 240
	    ENDIF
	    NC1=NC1+1
	  ENDDO
	  GOTO 902
      ELSEIF(LCH.GE.C_DEC.AND.LCH.LE.C_NUM+9) THEN
C           number or decimal point
        IF(TYP(L).EQ.E_LITERAL.OR.TYP(L).EQ.E_PARAM) GOTO 906
        I0=I
CC        X(N(L),L)=EVRDNM(T,I0,I,ERR1)
        IF(IP.GE.MP) GOTO 962
        IP=IP+1
        PL(1,IP)=1
        PL(2,IP)=N(L)+M1*(L-1)+1
	  PC(IP)%L=1
        PC(IP)%X=EVRDNM(T,I0,I,ERR)
        IF(ERR.NE.' ') GOTO 990
        IF(TYP(L).EQ.E_UNARY) THEN
CC          X(N(L),L)=EVFUN(IDFUN(L),X(N(L),L),NVFUN(IDFUN(L)),ERR1)
          IF(IP.GE.MP) GOTO 962
          IP=IP+1
          PL(1,IP)=20+IDFUN(L)
          PL(2,IP)=N(L)+M1*(L-1)+1
          PL(3,IP)=NVFUN(IDFUN(L))
c          IF(ERR1.NE.' ') THEN
c            ERR=ERR1
c            GOTO 990
c          ENDIF
        ENDIF
        TYP(L)=1
	  GOTO 220
      ELSEIF(LCH.GE.C_EXP.AND.LCH.LE.C_VAR.OR.LCH.EQ.C_DOLLAR) THEN
C                        .OR.LCH.EQ.C_DOLLAR has been missing. Added Mar.22.2002
C           alphabet or other character allowed for the first
C           character of a variable.
        IF(TYP(L).EQ.E_LITERAL.OR.TYP(L).EQ.E_PARAM
     %         .OR.TYP(L).EQ.E_UNARY) GOTO 906
        I0=I
 280    I=I+1
        IF(I.LE.NC) THEN
          LCH=LCHCOD(T(I:I))
          IF(LCH.EQ.C_BLANCK) GOTO 280
          IF(LCH.GE.C_NUM.AND.LCH.LE.C_VAR) GOTO 280
C             character allowed for variables (incl. numbers)
        ENDIF
        IX=I-1
        CALL IFNAME(T(I0:IX),TYP1,ID)
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
CC          X(N(L),L)=VPAR(ID)
          IF(IP.GE.MP) GOTO 962
          IP=IP+1
          PL(1,IP)=3
          PL(2,IP)=N(L)+M1*(L-1)+1
          PL(3,IP)=ID
        ELSEIF(TYP1.EQ.E_ARRAY) THEN
          TYP(L)=E_ARRAY
          IDFUN(L)=ID
          NARG(L)=ARR(ID)%RANK
	  ELSEIF(TYP1.EQ.E_CHAR) THEN
          IF(ARR(ID)%RANK.EQ.0) THEN
            TYP(L)=E_PARAM
            IF(IP.GE.MP) GOTO 962
            IP=IP+1
            PL(1,IP)=3
            PL(2,IP)=N(L)+M1*(L-1)+1
            PL(3,IP)=-ID
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
C--- closing parenthesis
        J=LCH-C_CLOSPAR1+1
        IF(L.EQ.1) GOTO 912
        IF(J.NE.KK(L)) GOTO 914
	  GOTO 320
	ELSEIF(LCH.GE.C_OPENPAR1.AND.LCH.LE.C_OPENPAR3) THEN
C--- opening parenthesis
        J=LCH-C_OPENPAR1+1
        IF(TYP(L).EQ.E_LITERAL.OR.TYP(L).EQ.E_PARAM) GOTO 916
        IF(L.EQ.ML) GOTO 918
        L=L+1
        IARG(L)=1
        KK(L)=J
        N(L)=0
        TYP(L)=0
CC        X(0,L)=0
        IF(IP.GE.MP) GOTO 962
        IP=IP+1
        PL(1,IP)=1
        PL(2,IP)=M1*(L-1)+1
	  PC(IP)%L=1
        PC(IP)%X=0
	  PC(IP)%C(1)=1
	  PC(IP)%C(2)=0
        GOTO 200
      ELSEIF(LCH.EQ.C_COMMA) THEN
C---   comma
        IF(L.EQ.1) THEN
          ERR='Syntax error. "'//T(MAX(1,I-10):I)//'" Invalid comma.'
     %           //'   (EVCMPL)'
          GOTO 990
        ELSEIF(TYP(L-1).NE.E_UNARY.AND.TYP(L-1).NE.E_USERF
     %             .AND.TYP(L-1).NE.E_ARRAY) THEN
          ERR='Syntax error. "'//T(MAX(1,I-10):I)//'" Invalid comma.'
     %           //'   (EVCMPL)'
          GOTO 990
        ELSE
          CALL EVENDBC(TYP(L),N(L),M1*(L-1)+1,MP,IP,PL,BOP(1,L),ERR)
          IF(ERR.NE.' ') GOTO 990
CC          XX(IARG(L),L)=X(0,L)
          IF(IP.GE.MP) GOTO 962
          IP=IP+1
          PL(1,IP)=2
          PL(2,IP)=M00+(IARG(L)+MARG0*(L-1))
          PL(3,IP)=M1*(L-1)+1
          IARG(L)=IARG(L)+1
          IF(IARG(L).GT.MARG) GOTO 922
        ENDIF
CC        X(0,L)=0
        IF(IP.GE.MP) GOTO 962
        IP=IP+1
        PL(1,IP)=1
        PL(2,IP)=M1*(L-1)+1
	  PC(IP)%L=1
        PC(IP)%X=0
	  PC(IP)%C(1)=1
	  PC(IP)%C(2)=0
        N(L)=0
        TYP(L)=0
        GOTO 200
	ELSEIF(LCH.GE.C_PLUS.AND.LCH.LE.C_SMALL) THEN
C--- binary operators
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
        IF(TYP(L).EQ.E_UNARY.OR.TYP(L).EQ.E_BINARY
     %            .OR.TYP(L).EQ.E_ARRAY) GOTO 924
	  IF(TYP(L).EQ.E_ARRAY) GOTO 925
        CALL EVREDCC(N(L),M1*(L-1)+1,MP,IP,PL,BOP(1,L),J,ERR1)
        IF(ERR1.NE.' ') THEN
          ERR=ERR1
          GOTO 990
        ENDIF
        N(L)=N(L)+1
        IF(N(L).GT.M) GOTO 926
        BOP(N(L),L)=J
        TYP(L)=E_BINARY
        GOTO 200
      ELSE
        GOTO 900
      ENDIF
      GOTO 220
C----------------
 320  CALL EVENDBC(TYP(L),N(L),M1*(L-1)+1,MP,IP,PL,BOP(1,L),ERR)
      IF(ERR.NE.' ') GOTO 990
CC      X0=X(0,L)
      IF(L.EQ.1) THEN
        IF(IP.GE.MP) GOTO 962
        IP=IP+1
        PL(1,IP)=0
	  NP=IP
        GOTO 990
      ENDIF
      IF(I.GT.NC) GOTO 928
CC      XX(IARG(L),L)=X0
      IF(IP.GE.MP) GOTO 962
      IP=IP+1
      PL(1,IP)=2
      PL(2,IP)=M00+(IARG(L)+MARG0*(L-1))
      PL(3,IP)=M1*(L-1)+1
      L=L-1
      IF(TYP(L).EQ.E_UNARY.OR.TYP(L).EQ.E_USERF
     %           .OR.TYP(L).EQ.E_ARRAY) THEN
        IF(TYP(L).EQ.E_UNARY) THEN
	    IF(IARG(L+1).NE.NARG(L).AND.NARG(L).LT.1000) GOTO 934
CC          X(N(L),L)=EVFUN(IDFUN(L),XX(1,L+1),IARG(L+1),ERR1)
          IF(IP.GE.MP) GOTO 962
          IP=IP+1
          PL(1,IP)=20+IDFUN(L)
          PL(2,IP)=M00+(1+MARG0*L)
          PL(3,IP)=IARG(L+1)
          IF(IP.GE.MP) GOTO 962
          IP=IP+1
          PL(1,IP)=2
          PL(2,IP)=N(L)+M1*(L-1)+1
          PL(3,IP)=M00+(1+MARG0*L)
        ELSEIF(TYP(L).EQ.E_USERF.OR.TYP(L).EQ.E_ARRAY) THEN
	    IF(TYP(L).EQ.E_ARRAY) THEN
	      IF(IARG(L+1).NE.NARG(L)) GOTO 930
          ENDIF
CC          X(N(L),L)=EVUFN(NAMUFN(IDFUN(L)),XX(1,L+1),IARG(L+1),ERR1)
          IF(IP.GE.MP) GOTO 962
          IP=IP+1
	    IF(TYP(L).EQ.E_USERF) THEN
            PL(1,IP)=100+IDFUN(L)
	    ELSEIF(TYP(L).EQ.E_ARRAY) THEN
	      PL(1,IP)=1000+IDFUN(L)
	    ENDIF
          PL(2,IP)=M00+(1+MARG0*L)
          PL(3,IP)=IARG(L+1)
          IF(IP.GE.MP) GOTO 962
          IP=IP+1
          PL(1,IP)=2
          PL(2,IP)=N(L)+M1*(L-1)+1
          PL(3,IP)=M00+(1+MARG0*L)
        ENDIF
CC        IF(ERR1.NE.' ') THEN
CC          ERR=ERR1
CC          GOTO 990
CC        ENDIF
      ELSE
CC        X(N(L),L)=X0
        IF(IP.GE.MP) GOTO 962
        IP=IP+1
        PL(1,IP)=2
        PL(2,IP)=N(L)+M1*(L-1)+1
        PL(3,IP)=M00+(IARG(L+1)+MARG0*L)
      ENDIF
      TYP(L)=E_LITERAL
      GOTO 200
C
900   ERR='(EVCMPL) Illegal character "'//T(I:I)//'".'
      GOTO 990
902   ERR='(EVCMPL) no closing quote'
	GOTO 990
904   ERR='(EVCMPL) Too many character data. '//ERR
	GOTO 990
906   ERR='(EVCMPL) Syntax error. "'//T(MAX(1,I-20):I)//'".'
	GOTO 990
908   ERR='(EVCMPL) Parameter/function "'//T(I0:IX)//'" does not exist.'
	GOTO 990
910   ERR='(EVCMPL) Too long character string. Buffer short.'
	GOTO 990
912   ERR='(EVCMPL) Too many closing parenthesis.'
	GOTO 990
914   ERR='(EVCMPL) Opening '//KKO(KK(L))//' does not match with '//
     %          'closing '//KKC(J)//'.'
	GOTO 990
916   ERR='(EVCMPL) Syntax error. "'//T(MAX(1,I-10):I)//
     %       '" No operator before '//KKO(J)//'.'
	GOTO 990
918   ERR='(EVCMPL) Nest level exceeded.'
	GOTO 990
922   ERR='(EVCMPL) Too many arguments for function "'//NAMFUN(IDFUN(L))
	GOTO 990
924   ERR='(EVCMPL) Syntax error. "'//T(MAX(1,I-10):I)
     %       //'" Double operator.'
	GOTO 990
925   ERR='(EVCMPL) Syntax error. "'//T(MAX(1,I-10):I)
     %     //'" No array subscripts.'
	GOTO 990
926   ERR='(EVCMPL) Program error 4'
	GOTO 990
928   ERR='(EVCMPL) Insufficient closing parenthesis.'
	GOTO 990
930   ERR='(EVCMPL) Number of subscripts does not match for "'
     %           //ARR(IDFUN(L))%NAME
	GOTO 990
934   ERR='(EVCMPL) Number of argument does not match for "'
     %         //NAMFUN(IDFUN(L))
	GOTO 990
962   ERR='(EVCMPL) Compile buffer full.'
      GOTO 990
980   ERR='(EVCMPL) EVCMPL called recursively.'
	RETURN
990   EVALNEST=EVALNEST-1
	EVCMPLNEST=EVCMPLNEST-1
	RETURN
999   ERR='(EVCMPL0) Recursive level too deep.'
	RETURN
      END
