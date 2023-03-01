	SUBROUTINE BMFILE2C(NP1,FILE,TXT,NCTXT,CMNT,MFLG,FLG,
     %    NCFLG,TEXT,NP2,IRTN)
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER NP1,FILE,NCTXT(2),MFLG,NCFLG(MFLG),NP2,IRTN
	CHARACTER(*) TXT(2),CMNT,FLG(MFLG),TEXT
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/chspcm.h'
	INCLUDE 'include/evchcod.h'
	INTEGER, PARAMETER:: MKW=40
	INTEGER NCKW(MKW),NCKW1(MKW),DIM(MKW),ICKW(2,MKW),IDKW(MKW),
     %   NKW,NKW0
	CHARACTER(16) KW(MKW),KW1(MKW)
	INTEGER N,I,J,KC0,KC1,I0,I1,II0,II1,NN,DIM1,IDAR,IEQ,NFUN,
     %   NC,K
	INTEGER LCHCOD
	CHARACTER(80) ERR
	TYPE(FLCHTYPE) FC
	INTEGER, PARAMETER:: MVAR=17
	INTEGER LVAR(MVAR),KVAR(MVAR),IDVAR(MVAR)
C       LVAR(i)=1: Variable VAR(i) replaced by a user keyword.
C                  (They must be given by KVAR(i))
C       KVAR(i)=k: Variable VAR(i) is given by the k-th user function
C       IDVAR(i) : variable ID in array VPAR
	INTEGER NCVAR(MVAR)/
     %  1,1,1,1,2,2,2,2,
     %  2,2,2,3,3,3,
     %  4,3,3/
	CHARACTER(8) VAR(MVAR)/
     %  'T','X','Y','S','En','Px','Py','Ps',
     %  'Sx','Sy','Ss','Xi1','Xi2','Xi3',
     %  'Kind','Gen','Wgt'/
C                    Above names must be the same as in setcnst.f
      INTEGER, PARAMETER:: MFUN=MVAR
	REAL(8) FUNX(MFUN)
	INTEGER IDLMFUN(MFUN)
	INTEGER, PARAMETER:: MP=100
	CHARACTER(16) NAMP(MP),CDUMMY
	INTEGER NP,NF

      CALL CPUTIM('BMFILE2C',1)
	NP2=0
	DO I=1,MVAR
	  CALL EVDEFP1(VAR(I),0D0,IDVAR(I),IRTN)
	ENDDO
C  Keywords
C   Expect a form like KEYWORD=(A,B(3),C)
	CALL BLKRHS0(TXT(1)(1:NCTXT(1)),MKW,NN,ICKW,IRTN)
	IF(IRTN.NE.0) GOTO 900
	LVAR=0
	KVAR=0
	NKW=0
	DO N=1,NN
	  I0=0
	  I1=0
	  II0=0
	  DO I=ICKW(1,N),ICKW(2,N)
	    J=LCHCOD(TXT(1)(I:I))
	    IF(J.GE.C_OPENPAR1.AND.J.LE.C_OPENPAR3) THEN
	      IF(I0.EQ.0) THEN
				  I0=I
	        KC0=J-C_OPENPAR1+1
	      ENDIF
	    ELSEIF(J.GE.C_CLOSPAR1.AND.J.LE.C_CLOSPAR3) THEN
	      I1=I
	      KC1=J-C_CLOSPAR1+1
	    ELSEIF(J.NE.C_BLANCK) THEN
	      IF(I0.EQ.0) THEN
				  IF(II0.EQ.0) II0=I
	        II1=I
	      ENDIF
	    ENDIF
	  ENDDO
	  IF(II0.EQ.0) GOTO 904
	  IF(I0.NE.0.AND.I1.EQ.0.OR.I0.EQ.0.AND.I1.NE.0) GOTO 906
	  DIM1=0
	  IF(I0.NE.0) THEN
	    IF(KC1.NE.KC0) GOTO 906
	    I0=I0+1
	    I1=I1-1
	    IF(I1.LT.I0) GOTO 904
	    CALL EVAL0(TXT(1)(I0:I1),FC,ERR)
	    IF(ERR.NE.' '.OR.FC%L.NE.1) GOTO 908
	    DIM1=NINT(FC%X)
	    IF(DIM1.LE.0) GOTO 908
	  ENDIF
	  NKW=NKW+1
	  KW(NKW)=TXT(1)(II0:II1)
	  KW1(NKW)='_'//TXT(1)(II0:II1)
	  NCKW(NKW)=II1-II0+1
	  NCKW1(NKW)=NCKW(NKW)+1
	  DIM(NKW)=DIM1
	  IF(DIM1.EQ.0) THEN
	    CALL EVDEFP1(KW1(NKW)(1:NCKW1(NKW)),0D0,IDKW(NKW),IRTN)
	    IF(IRTN.GE.10) GOTO 910
	  ELSE
	    CALL EVDEFARR(KW1(NKW)(1:NCKW1(NKW)),2,1,1,DIM1,0D0,IDKW(NKW),
     %      IRTN,ERR)
	    IF(IRTN.GE.10) GOTO 910
	  ENDIF
c	WRITE(MSGFL,999) IRTN,NKW,IDKW(NKW),KW1(NKW)(1:NCKW1(NKW))
c999   format(' IRTN=',I3,'  NKW=',I3,'  IDKW=',I3,'  KW1=',A)
	  DO I=1,MVAR
	    IF(KW(NKW).EQ.VAR(I)) LVAR(I)=1
	  ENDDO
	ENDDO
	NKW0=NKW
	DO I=1,MVAR
	  IF(LVAR(I).EQ.0) THEN
	    NKW=NKW+1
	    KW(NKW)=VAR(I)
          KW1(NKW)=KW(NKW)
	    NCKW(NKW)=NCVAR(I)
	    NCKW1(NKW)=NCKW(NKW)
	    DIM(NKW)=0
	    CALL EVDEFP1(KW1(NKW)(1:NCKW1(NKW)),0D0,IDKW(NKW),IRTN)
c	WRITE(MSGFL,999) IRTN,NKW,IDKW(NKW),KW1(NKW)(1:NCKW1(NKW))
	  ENDIF
	ENDDO
C  Conversion functions
	CALL BLKRHS0(TXT(2)(1:NCTXT(2)),MKW,NN,ICKW,IRTN)
	NFUN=0
	DO N=1,NN
	  IEQ=0
	  II0=0
	  I0=0
	  DO I=ICKW(1,N),ICKW(2,N)
	    IF(TXT(2)(I:I).EQ.'=') THEN
	      IF(IEQ.EQ.0) IEQ=I
	    ELSEIF(TXT(2)(I:I).NE.' ') THEN
	      IF(IEQ.EQ.0) THEN
	        IF(II0.EQ.0) II0=I
	        II1=I
	      ELSE
	        IF(I0.EQ.0) I0=I
	        I1=I
	      ENDIF
	    ENDIF
	  ENDDO
	  IF(II0.EQ.0) GOTO 926
		IF(IEQ.EQ.0) GOTO 928
		IF(I0.EQ.0) GOTO 930
	  NFUN=NFUN+1
	  J=0
	  DO I=1,MVAR
	    IF(TXT(2)(II0:II1).EQ.VAR(I)) THEN
	      KVAR(I)=NFUN
	      J=1
	      EXIT
	    ENDIF
	  ENDDO
	  IF(J.EQ.0) GOTO 916
C   Here, keyword names in KW(i) (1<=i<=NKW0) must be replaced
C   by underscore//KW(i) in TXT(2)(I0:I1).
        CALL BLFL2REP(TXT(2)(I0:I1),NKW0,KW,NCKW,KW1,NCKW1,TEXT,NC)
C        See if FUN contains variables in order not to repeat evaluation
        CALL EVCHKVAR(TEXT(1:NC),MP,NP,NAMP,0,NF,CDUMMY,FC,IRTN)
	  IF(IRTN.NE.0) GOTO 932
	  I0=0
	  IF(NP.GT.0) THEN
	    DO J=1,NP
	      I1=0
	      DO K=1,NKW
	        IF(NAMP(J).EQ.KW1(K)) THEN
	          I1=1
	          EXIT
	        ENDIF
	      ENDDO
	      IF(I1.NE.0) THEN
	        I0=1
	        EXIT
	      ENDIF
	    ENDDO
	  ENDIF
	  IF(I0.EQ.0) THEN
		  IDLMFUN(NFUN)=0
	    FUNX(NFUN)=FC%X
	  ELSE
	    CALL EVCMPL(TEXT(1:NC),IDLMFUN(NFUN),ERR)
	    IF(ERR.NE.' ') GOTO 932
	  ENDIF
	ENDDO
	DO I=1,MVAR
	  IF(LVAR(I).NE.0) THEN
	    IF(KVAR(I).EQ.0) GOTO 922
	  ENDIF
	ENDDO

	CALL BMFILE20C(FILE,NP1,
     %  MVAR,IDVAR,KVAR,NKW,IDKW,KW,KW1,NCKW,NCKW1,DIM,
     %  NFUN,FUNX,IDLMFUN,MFLG,FLG,NCFLG,CMNT,NP2,IRTN)

      DO I=1,NFUN
	  IF(IDLMFUN(I).NE.0) CALL EVLMFREE(IDLMFUN(I))
	ENDDO

	IF(IRTN.NE.0) GOTO 1000
	IRTN=0
	GOTO 1000
900   IRTN=1000
	IF(MSGLVL.GE.0) WRITE(MSGFL,901) TXT(1)(1:NCTXT(1))
901   FORMAT(' (SUBR.BMFILE2) Invalid KEYWORD operand "',A,'"')
      GOTO 1000
904   IRTN=1004
	IF(MSGLVL.GE.0) WRITE(MSGFL,905) TXT(1)(ICKW(1,N):ICKW(2,N))
905   FORMAT(' (SUBR.BMFILE2) Invalid syntax in "',A,'"')
      GOTO 1000
906   IRTN=1006
	IF(MSGLVL.GE.0) WRITE(MSGFL,907) TXT(1)(ICKW(1,N):ICKW(2,N))
907   FORMAT(' (SUBR.BMFILE2) Parens do not match in "',A,'"')
      GOTO 1000
908   IRTN=1009
	IF(MSGLVL.GE.0) WRITE(MSGFL,909) TXT(1)(I0:I1)
909   FORMAT(' (SUBR.BMFILE2) Invalid subscript expression "',A,'"')
      GOTO 1000
910   IRTN=1010
	IF(MSGLVL.GE.0) WRITE(MSGFL,911) '_'//KW(NKW)(1:NCKW(NKW))
911   FORMAT(' (SUBR.BMFILE2) Error in allocating variable "',A,'"')
      GOTO 1000
916   IRTN=1016
      IF(MSGLVL.GE.0) WRITE(MSGFL,917) TXT(2)(II0:II1),
     %  (VAR(I)(1:NCVAR(I)),I=1,MVAR)
917   FORMAT(' (SUBR.BMFILE2) l.h.s "',A,'" must be one of',/,
     %  5X,30(1X,A,:))
      GOTO 1000
922   IRTN=1022
	IF(MSGLVL.GE.0) WRITE(MSGFL,923) VAR(I)(1:NCVAR(I))
923   FORMAT(' (SUBR.BMFILE2) Formula for "',A,'" not given.')
	GOTO 1000
926   IRTN=1026
      IF(MSGLVL.GE.0) WRITE(MSGFL,927) TXT(2)(ICKW(1,N):ICKW(2,N))
927   FORMAT(' (SUBR.BMFILE2) Invalid conversion operand "',A,'"',/,
     %   '   Missing L.H.S.')
      GOTO 1000	
928   IRTN=1028
      IF(MSGLVL.GE.0) WRITE(MSGFL,929) TXT(2)(ICKW(1,N):ICKW(2,N))
929   FORMAT(' (SUBR.BMFILE2) Invalid conversion operand "',A,'"',/,
     %   '   Missing = sign.')
      GOTO 1000	
930   IRTN=1030
      IF(MSGLVL.GE.0) WRITE(MSGFL,931) TXT(2)(ICKW(1,N):ICKW(2,N))
931   FORMAT(' (SUBR.BMFILE2) Invalid conversion operand "',A,'"',/,
     %   '   Missing R.H.S.')
      GOTO 1000
932   IRTN=1032
      IF(MSGLVL.GE.0) WRITE(MSGFL,933) TEXT(1:NC)
933   FORMAT(' (SUBR.BMFILE2) Invalid conversion function "',A,'"')
      GOTO 1000
1000  CALL CPUTIM('BMFILE2C',2)
      RETURN
	END

	SUBROUTINE BMFILE20C(FILE,NP1,
     %  MVAR,IDVAR,KVAR,NKW,IDKW,KW,KW1,NCKW,NCKW1,DIM,
     %  NFUN,FUNX,IDLMFUN,MFLG,FLG,NCFLG,CMNT,NP2,IRTN)
	USE FLCHTYP
	USE ARRAYMOD
	IMPLICIT NONE
	INTEGER FILE,NP1,MVAR,IDVAR(MVAR),KVAR(MVAR),
     %   NKW,IDKW(NKW),NCKW(NKW),NCKW1(NKW),DIM(NKW),
     %   NFUN,IDLMFUN(NFUN),MFLG,NCFLG(MFLG),NP2,IRTN
	REAL(8) FUNX(NFUN)
	CHARACTER(*) KW(NKW),KW1(NKW),FLG(MFLG),CMNT
C   Order of FLG is
C      'BEGIN','END','TERMINATE'
	INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
	INCLUDE 'include/ctrlcm.h'
	INTEGER N,IN,NC,IC,DONE,IKW,IKW1,KK,I,NV
	INTEGER, PARAMETER:: MC=1000
	CHARACTER(MC) TEXT
	REAL(8) VAL
	INTEGER ICASE
C        ICASE=1:  BEGIN defined but END undef
C              2:  BEGIN undef but END defined
C              3:  Both defined
C        IN=0:  Before BEGIN (ICASE=2 never comes)
C           1:  BEGIN found but contents not yet found
C           2:  contents found
C           3:  TERMINATE found

	NP2=0
	IF(NCFLG(1).NE.0) THEN
	  IF(NCFLG(2).NE.0) THEN
	    ICASE=3
	  ELSE
	    ICASE=1
	  ENDIF
	ELSE
	  IF(NCFLG(2).NE.0) THEN
	    ICASE=2
	  ELSE
	    GOTO 900
	  ENDIF
	ENDIF
	DO IKW=1,NKW
	  IF(DIM(IKW).EQ.0) THEN
	    VPAR(IDKW(IKW))=0
	  ELSE
	    DO I=1,DIM(IKW)
	      ARR(IDKW(IKW))%VAL(I)=0
	    ENDDO
	  ENDIF
	ENDDO
	N=0
	IN=0
	IF(ICASE.EQ.2) IN=1
	IKW=0
100	KK=0
	DONE=0
	CALL BMFLNEXTTOK(FILE,CMNT,VAL,TEXT,NC,IC)
	IF(ICASE.LE.2) THEN
	  IF(IC.GE.100.OR.
     %      (IC.LT.100.AND.TEXT(1:NC).EQ.FLG(3)(1:NCFLG(3)))) THEN
	    IF(IN.EQ.2) KK=1
	    IN=3
	    DONE=1
	  ELSEIF(TEXT(1:NC).EQ.FLG(ICASE)(1:NCFLG(ICASE))) THEN
	    IF(IN.EQ.2) KK=1
	    IN=1
	    DONE=1
	  ENDIF
	ELSE
	  IF(IC.GE.100.OR.
     %      (IC.LT.100.AND.TEXT(1:NC).EQ.FLG(3)(1:NCFLG(3)))) THEN
	    IF(IN.NE.0) GOTO 902
	    IN=3
	    DONE=1
	  ELSEIF(TEXT(1:NC).EQ.FLG(1)(1:NCFLG(1))) THEN
	    IF(IN.NE.0) GOTO 904
	    IN=1
	    DONE=1
	  ELSEIF(TEXT(1:NC).EQ.FLG(2)(1:NCFLG(2))) THEN
	    IF(IN.EQ.0) GOTO 906
	    IF(IN.EQ.2) KK=1
	    IN=0
	    DONE=1
	  ENDIF
	ENDIF
	IF(KK.NE.0) THEN
	  CALL BMFL2ADDC(NP1,MVAR,IDVAR,KVAR,NFUN,FUNX,IDLMFUN,IRTN)
	  IF(IRTN.NE.0) GOTO 1000
	  N=N+1
	  IF(NP1.GT.0.AND.N.GE.NP1) GOTO 800
	  DO IKW=1,NKW
	    IF(DIM(IKW).EQ.0) THEN
	      VPAR(IDKW(IKW))=0
	    ELSE
	      DO I=1,DIM(IKW)
	        ARR(IDKW(IKW))%VAL(I)=0
	      ENDDO
	    ENDIF
	  ENDDO
	  IF(ICASE.LE.2) IN=1
	  IKW=0
	ELSEIF(DONE.EQ.0.AND.IN.NE.0.AND.IN.NE.3) THEN
	  IKW1=0
	  DO I=1,NKW
	    IF(TEXT(1:NC).EQ.KW(I)(1:NCKW(I))) THEN
	      IKW1=I
	      EXIT
	    ENDIF
	  ENDDO
	  IF(IKW1.NE.0) THEN
	    IF(IKW.NE.0) GOTO 908
	    IKW=IKW1
	    NV=-1
	  ELSE
	    IF(IC.EQ.3) THEN      !  '='
	      IF(IKW.EQ.0.OR.NV.GE.0) GOTO 910
	      NV=0
	    ELSE
			  IF(IC.NE.1) GOTO 918
	      IF(IKW.EQ.0) GOTO 912
	      IF(NV.LT.0) GOTO 914
	      NV=NV+1
	      IF(NV.GT.MAX(1,DIM(IKW))) GOTO 916
	      IF(DIM(IKW).EQ.0) THEN
	        VPAR(IDKW(IKW))=VAL
	      ELSE
	        ARR(IDKW(IKW))%VAL(NV)=VAL
	      ENDIF
	      IF(NV.GE.MAX(1,DIM(IKW))) IKW=0
	    ENDIF
	  ENDIF
	  IN=2
	ENDIF
	IF(IN.NE.3) GOTO 100
800	IRTN=0
	NP2=N
	GOTO 1000

900   IRTN=1000
      WRITE(MSGFL,901)
901   FORMAT(' (SUBR.BMFILE2) None of BEGIN and END given.')
      GOTO 1000
902   IRTN=1002
      WRITE(MSGFL,903)
903   FORMAT(' (SUBR.BMFILE2) Invalid file end.')
      GOTO 1000
904   IRTN=1004
      WRITE(MSGFL,905) TEXT(1:NC).EQ.FLG(1:NCFLG(1))
905   FORMAT(' (SUBR.BMFILE2) Flag "',A,'" misplaced.')
      GOTO 1000
906   IRTN=1007
      WRITE(MSGFL,907) TEXT(1:NC).EQ.FLG(1:NCFLG(2))
907   FORMAT(' (SUBR.BMFILE2) Flag "',A,'" misplaced.')
      GOTO 1000
908   IRTN=1009
      WRITE(MSGFL,909) KW(I)(1:NCKW(I))
909   FORMAT(' (SUBR.BMFILE2) Keyword "',A,'" misplaced.')
      GOTO 1000
910   IRTN=1011
      WRITE(MSGFL,911)
911   FORMAT(' (SUBR.BMFILE2) Misplaced = sign.')
      GOTO 1000
912   IRTN=1013
      WRITE(MSGFL,913) TEXT(1:NC)
913   FORMAT(' (SUBR.BMFILE2) Redundant number "',A,'"')
      GOTO 1000
914   IRTN=1015
      WRITE(MSGFL,915) TEXT(1:NC)
915   FORMAT(' (SUBR.BMFILE2) Missing = sign before "',A,'"')
      GOTO 1000
916   IRTN=1017
      WRITE(MSGFL,917) KW(IKW)(1:NCKW(IKW))
917   FORMAT(' (SUBR.BMFILE2) Too many number for "',A,'"')
      GOTO 1000
918   IRTN=1019
      WRITE(MSGFL,919) TEXT(1:NC)
919   FORMAT(' (SUBR.BMFILE2) Unidentified character string "',A,'"')
      GOTO 1000
920   IRTN=1020
      WRITE(MSGFL,921)
921   FORMAT(' (SUBR.BMFILE2) Particle buffer full.')
      GOTO 1000


1000	RETURN
	END

	SUBROUTINE BMFL2ADDC(NP1,MVAR,IDVAR,KVAR,
     %    NFUN,FUNX,IDLMFUN,IRTN)
	USE FLCHTYP
	USE ARRAYMOD
	IMPLICIT NONE
	INTEGER NP1,MVAR,IDVAR(MVAR),KVAR(MVAR),NFUN,IDLMFUN(NFUN),IRTN
	REAL(8) FUNX(NFUN)
      INCLUDE 'include/nameleng.h'
      INCLUDE 'include/evparc.h'
	INCLUDE 'include/cnstcm.h'
	INCLUDE 'include/ctrlcm.h'
	INTEGER I,K
	TYPE(FLCHTYPE) FC
	INTEGER KIND,GEN
	REAL(8) TXYS(0:3),EP(0:3),SPIN(3),WGT
	CHARACTER(8) PNAME/'        '/
	REAL*8 BFL(3,2)/0,0,0,0,0,0/
	CHARACTER(80) ERR
c	CHARACTER(8) VAR(MVAR)/
c     %  'T','X','Y','S','En','Px','Py','Ps',
c     %  'Sx','Sy','Ss','Xi1','Xi2','Xi3',
c     %  'Kind','Gen','Wgt'/

	DO I=1,MVAR
	  K=KVAR(I)
	  IF(K.NE.0) THEN
	    IF(IDLMFUN(K).EQ.0) THEN
	      VPAR(IDVAR(I))=FUNX(K)
	    ELSE
            CALL EVLOAD(IDLMFUN(K),FC,IRTN)
	      IF(IRTN.NE.0) GOTO 900
	      VPAR(IDVAR(I))=FC%X
	    ENDIF
	  ENDIF
	ENDDO
	KIND=MAX(1,MIN(3,NINT(VPAR(IDVAR(15)))))
	GEN=MAX(1,NINT(VPAR(IDVAR(16))))
	WGT=MAX(0D0,VPAR(IDVAR(17)))
	DO I=0,3
	  TXYS(I)=VPAR(IDVAR(I+1))
	ENDDO
	DO I=1,3
	  EP(I)=VPAR(IDVAR(I+5))
	  SPIN(I)=VPAR(IDVAR(I+8))
	ENDDO
	EP(0)=SQRT(MASS(KIND)**2+EP(1)**2+EP(2)**2+EP(3)**2)
c	WRITE(MSGFL,999) (txys(i),i=0,3),(ep(i),i=0,3),(spin(i),i=1,3),
c     %     wgt,kind,gen
c999   format(' TXYS=',1P4D11.3,' EP=',4D11.3,/,
c     %       ' SPIN=',3D11.3,' WGT=',D11.3,' KIND=',I1,' GEN=',I1)
	CALL ADDONE(0,KIND,GEN,PNAME,0,WGT,TXYS,EP,SPIN,
     %       0,BFL,IRTN)
	IF(IRTN.NE.0) GOTO 920
	IRTN=0
	RETURN
900   IRTN=1000
	WRITE(MSGFL,905) K
905   FORMAT(' (SUBR.BMFILE2)',I3,'th conversion function invalid.')
      RETURN
920   IRTN=1020
	WRITE(MSGFL,925)
925   FORMAT(' (SUBR.BMFILE2) Particlke buffer full.')
	RETURN
	END

	SUBROUTINE BMFLNEXTTOK(FILE,CMNT,VAL,TEXT,NC,IRTN)
C  Get next token in the beam file
C     Expected form
C         aaa,  bbb=ccc, ddd, 
C     Rule
C         * Do not care about quotes
C         * () must close within a token
C         * Consider ' ' as delimiter unless in (). Comma is replaced by ' '.
C         * Ignore after the comment character
C         * Possible tokens are
C             character string, =,  literal number
C  Input
C   FILE
C   CMNT:   comment character (to the end of file line)
C  Output
C   VAL     real(8) number
C   TEXT(1:NC)
C   IRTN    1: a number  (stored in VAL and in TEXT(1:NC))
C           2: character string (stored in TEXT(1:NC)
C           3: =
C         100: EOF
	IMPLICIT NONE
	INTEGER FILE,NC,IRTN
	CHARACTER(*) CMNT,TEXT
	REAL(8) VAL
	CHARACTER(1) CH
	REAL(8) FLOT1
	INTEGER INPAR
	CHARACTER(1) LASTCHAR
	LOGICAL SAVED/.FALSE./,EOF/.FALSE./
	SAVE LASTCHAR,SAVED,EOF

	NC=0
	IF(EOF) THEN
	  IRTN=100
	  RETURN
	ENDIF
	INPAR=0
100	IF(SAVED) THEN
	  CH=LASTCHAR
	  SAVED=.FALSE.
	ELSE
	  CALL BMFLNEXTCHAR(FILE,CMNT,CH,EOF)
	  IF(EOF) THEN
	    IF(NC.NE.0) GOTO 200
	    IRTN=100
	    RETURN
	  ENDIF
	ENDIF
	IF(INPAR.EQ.0) THEN
	  IF(CH.EQ.',') CH=' '
	  IF(CH.EQ.'(') THEN
	    INPAR=1
	  ELSEIF(CH.EQ.'=') THEN
	    IF(NC.EQ.0) THEN
	      IRTN=3
	      NC=1
	      TEXT='='
	      RETURN
	    ELSE
	      SAVED=.TRUE.
	      LASTCHAR='='
	      GOTO 200
	    ENDIF
	  ELSEIF(CH.EQ.' ') THEN
	    IF(NC.NE.0) GOTO 200
	  ENDIF
	ELSE
	  IF(CH.EQ.')') INPAR=0
	ENDIF
	IF(CH.NE.' '.OR.NC.NE.0) THEN
	  NC=NC+1
	  TEXT(NC:NC)=CH
	ENDIF
	GOTO 100
C       come here only when a token is found
200	VAL=FLOT1(TEXT(1:NC),NC,IRTN)
	IF(IRTN.NE.0) THEN
	  VAL=0
	  IRTN=2
	ELSE
	  IRTN=1
	ENDIF
	RETURN
	END

	SUBROUTINE BMFLNEXTCHAR(FILE,CMNT,CH,EOF)
	IMPLICIT NONE
	INTEGER FILE
	CHARACTER(*) CMNT
	CHARACTER(1) CH
	LOGICAL EOF
	INTEGER I
	INTEGER, PARAMETER:: MCLINE=1000
	CHARACTER(MCLINE+1) LINE
	INTEGER II/0/     !  already read up to LINE(II:II)
	INTEGER NCL/0/       !  length of stored LINE
	SAVE LINE,NCL,II

100	IF(II.GE.NCL) THEN
	  NCL=0
	  DO WHILE(NCL.EQ.0)
	    READ(FILE,'(A)',END=200) LINE	  
	    DO I=MCLINE,1,-1
	      IF(LINE(I:I).NE.' ') THEN
	        NCL=I+1
C                 Add one blanck character at the line end
	        EXIT
	      ENDIF
	    ENDDO
	  ENDDO
	  II=0
	ENDIF
	II=II+1
	IF(CMNT.NE.''.AND.LINE(II:II).EQ.CMNT) THEN
	  II=NCL
	  GOTO 100
	ENDIF
	CH=LINE(II:II)
	EOF=.FALSE.
	RETURN
200   EOF=.TRUE.
	RETURN
	END

	SUBROUTINE BLFL2REP(TXT0,NKW0,KW,NCKW,KW1,NCKW1,TEXT,NC)
C  Replace  keyword by _keyword in the expression
	IMPLICIT NONE
	INTEGER NKW0,NCKW(NKW0),NCKW1(NKW0),NC
	CHARACTER(*) TXT0,KW(NKW0),KW1(NKW0),TEXT
	INTEGER NC0,I,IC,I0,I1,J,IFIN
	INTEGER LCHCOD
	INCLUDE 'include/evchcod.h'

	NC0=LEN(TXT0)
	I0=0
	NC=0
	I=0
100	I=I+1
	IF(I.GT.NC0) RETURN
	IC=LCHCOD(TXT0(I:I))
110	IF(IC.GE.C_NUM.AND.IC.LE.C_VAR) THEN
	  I0=I
	  IFIN=0
120	  IF(I.LT.NC0) THEN
          I=I+1
	    IC=LCHCOD(TXT0(I:I))
	    IF(IC.GE.C_NUM.AND.IC.LE.C_VAR) GOTO 120
	    I1=I-1
	  ELSE
	    I1=I
	    IFIN=1
	  ENDIF
	  DO J=1,NKW0
	    IF(TXT0(I0:I1).EQ.KW(J)(1:NCKW(J))) THEN
	      TEXT(NC+1:NC+NCKW1(J))=KW1(J)(1:NCKW1(J))
	      NC=NC+NCKW1(J)
	      I0=0
	      EXIT
	    ENDIF
	  ENDDO
	  IF(I0.NE.0) THEN
	    TEXT(NC+1:NC+I1-I0+1)=TXT0(I0:I1)
	    NC=NC+I1-I0+1
	  ENDIF
	  IF(IFIN.NE.0) RETURN
	  GOTO 110
	ELSE
	  NC=NC+1
	  TEXT(NC:NC)=TXT0(I:I)
	ENDIF
	GOTO 100
	END
