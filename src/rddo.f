      SUBROUTINE RDDO(IC,ICMD,NCMD,LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER IC,NCMD,ICMD(NCMD),LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=3)
      CHARACTER*12 OP(MOP)/'REPEAT','WHILE','expression'/
      INTEGER NFF(MOP)/0,0,3/
      INCLUDE 'include/cmdnam.h'
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/pushcm.h'
	INCLUDE 'include/nameleng.h'
	INCLUDE 'include/evparc.h'
      INTEGER NC,J,I,IRW,IC1,NC1,K,N,NFIN,NF,IFIN
      CHARACTER*2 LREL(6)/'= ','<>','< ','> ','<=','>='/
	TYPE(FLCHTYPE) FC
      REAL*8 X1
	CHARACTER(80) ERR
	INTEGER, PARAMETER:: MIND=50
	INTEGER NNIND(MIND),IND(MIND),NIND,NCNAM,IDARR,LFCR,LFCL,
     %     IDUMMY(2,1),KK
	CHARACTER(16) NAM
c      INTEGER IRTNEV
c      REAL*8 X
c	TYPE(FLCHTYPE) FCR
C
      IRTN=0
      IF(LN(1,2).EQ.0) GOTO 910
      IF(NESTRT.EQ.0) THEN
        IF(NESTLV.EQ.MNEST) GOTO 900
        NESTLV=NESTLV+1
        NEST(NESTLV)=NEST_DO
	  NESTST(NESTLV)=1
	  IDDOVAR(1,NESTLV)=0
        ICNEST(NESTLV)=IC
        NREP(NESTLV)=0
        IREP(NESTLV)=0
      ENDIF
      CALL CMDBLK('DO',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GE.3) GOTO 990
      IF(IRTN.GE.1) GOTO 910
	IRW=0
	DO J=1,NBL
	  I=KBL(J)
        IF(OP(I).EQ.'REPEAT') THEN
	    IF(IRW.NE.0) GOTO 905
	    IRW=1
	  ELSEIF(OP(I).EQ.'WHILE') THEN
	    IF(IRW.NE.0) GOTO 905
	    IRW=2
	  ELSE
	    IF(IRW.EQ.0) IRW=3
	    IF(IRW.EQ.1) THEN
            IF(NESTRT.EQ.0) THEN
              IF(REL(J).NE.0) GOTO 910
              CALL BLKREC(LNKW(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
              IF(IRTN.NE.0) GOTO 910
              CALL EVAL0(TEXT(1:NC),FC,ERR)
              IF(ERR.NE.' ') GOTO 930
	        IF(FC%L.NE.1) GOTO 950
              NREP(NESTLV)=NINT(FC%X)
            ENDIF
	    ELSEIF(IRW.EQ.2) THEN
            CALL BLKREC(LNKW(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
            IF(IRTN.NE.0) GOTO 910
	      IF(REL(J).EQ.1) THEN
C               comes here in the case of 'IF  x=y'  instead of 'IF x==y'.
	        DO K=NC,1,-1
	          TEXT(K+1:K+1)=TEXT(K:K)
	        ENDDO
	        TEXT(1:1)='('
	        TEXT(NC+2:NC+5)=')==('
	        NC1=NC+6
              CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,
     %              TEXT(NC1:MCTEXT),NC,IRTN,MSGFL)
              IF(IRTN.NE.0) GOTO 910
	        NC=NC1+NC
	        TEXT(NC:NC)=')'
	      ENDIF
            CALL EVAL0(TEXT(1:NC),FC,ERR)
            IF(ERR.NE.' ') GOTO 940
	      IF(FC%L.NE.1) GOTO 950
	      X1=FC%X
	    ELSE
	      IF(NESTRT.EQ.0) THEN
	        IF(REL(J).NE.1) GOTO 910
C--------  right-hand-side
              CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
	        IF(IRTN.NE.0) GOTO 990
              DO K=1,3
                FFF(K)=UNDEF
              ENDDO
              CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
              IF(IRTN.NE.0) GOTO 990
              IF(NF.LE.1.OR.NF.GT.3) GOTO 966
	        DOVAR(1,NESTLV)=NINT(FFF(1)%X)
	        NFIN=NINT(FFF(2)%X)
	        IF(NF.EQ.3) THEN
	          DOVAR(2,NESTLV)=NINT(FFF(3)%X)
	          IF(DOVAR(2,NESTLV).EQ.0) GOTO 966
	        ELSE
	          DOVAR(2,NESTLV)=1
	        ENDIF
	        IF(DOVAR(1,NESTLV).LT.NFIN) THEN
	          IF(DOVAR(2,NESTLV).GT.0) THEN
	            NREP(NESTLV)=1
     %                +INT((NFIN-DOVAR(1,NESTLV)+1D-10)/DOVAR(2,NESTLV))
	          ELSE
	            NREP(NESTLV)=0
	          ENDIF
	        ELSEIF(DOVAR(1,NESTLV).GT.NFIN) THEN
                IF(DOVAR(2,NESTLV).LT.0) THEN
	            NREP(NESTLV)=1
     %                +INT((NFIN-DOVAR(1,NESTLV)-1D-10)/DOVAR(2,NESTLV))
	          ELSE
	            NREP(NESTLV)=0
	          ENDIF
	        ELSE
	          NREP(NESTLV)=1
	        ENDIF
C------------  left-hand-side
              CALL BLKREC(LNKW(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
              IF(IRTN.NE.0) GOTO 990
	        CALL EVLHS(TEXT(1:NC),KK,NAM,NCNAM,IDARR,
     %                        MIND,NIND,NNIND,IND,ERR)
	        IF(ERR.NE.' ') GOTO 910
C               KK=0 undefined name, 1: scalar floating, 2: array floating
C                  3: character
					IF(KK.EQ.0) THEN
C                     (name that has not defined yet)
	          IF(TEXT(1:1).EQ.'$') GOTO 960
	          IF(NIND.NE.0) GOTO 962
                CALL EVDEFP1(NAM(1:NCNAM),0D0,IDARR,IRTN)
	          IF(IRTN.GE.2) THEN
	            CALL SET00(NAM(1:NCNAM),0D0,'',MSGLVL,IRTN)  ! for error message
	            GOTO 990
	          ENDIF
	          IDDOVAR(1,NESTLV)=IDARR
	          IDDOVAR(2,NESTLV)=0
	        ELSEIF(KK.EQ.1) THEN
	          IDDOVAR(1,NESTLV)=IDARR
	          IDDOVAR(2,NESTLV)=0
	        ELSEIF(KK.EQ.2) THEN
	          IF(NIND.NE.ARR(IDARR)%RANK) GOTO 964
	          DO K=1,NIND
	            IF(NNIND(K).NE.1) GOTO 964
	          ENDDO
	          CALL ARRIND2N(IND,NIND,ARR(IDARR)%DIM,N,IRTN)
	          IF(IRTN.NE.0) GOTO 964
	          IDDOVAR(1,NESTLV)=IDARR
	          IDDOVAR(2,NESTLV)=N
	        ELSEIF(KK.EQ.3) THEN
	          GOTO 960
	        ENDIF
	        IF(NESTLV.GE.2) THEN
	          DO K=1,NESTLV-1
	            IF(NEST(K).EQ.NEST_DO) THEN
	              IF(IDDOVAR(1,K).NE.0) THEN
	                IF(IDDOVAR(1,NESTLV).EQ.IDDOVAR(1,K).AND.
     %                   IDDOVAR(2,NESTLV).EQ.IDDOVAR(2,K)) GOTO 970
	              ENDIF
	            ENDIF
	          ENDDO
	        ENDIF
c-------------
	      ENDIF
	    ENDIF
        ENDIF
	ENDDO
C
      IREP(NESTLV)=IREP(NESTLV)+1
	IFIN=0
      IF(IRW.EQ.1.OR.IRW.EQ.3) THEN
        IF(IREP(NESTLV).GT.NREP(NESTLV)) IFIN=1
	ELSEIF(IRW.EQ.2) THEN
	  IF(X1.EQ.0) IFIN=1
	ENDIF
      IF(IFIN.NE.0) THEN
        CALL FNDEND(NEST_DO,IC,ICMD,NCMD,IC1,IRTN)
        IF(IRTN.GE.10) GOTO 920
	  NESTST(NESTLV)=4
        NESTRT=1
        IC=IC1
      ELSE
	  IF(IRW.EQ.3) THEN
	    N=DOVAR(1,NESTLV)+(IREP(NESTLV)-1)*DOVAR(2,NESTLV)
	    IF(IDDOVAR(2,NESTLV).EQ.0) THEN
	      VPAR(IDDOVAR(1,NESTLV))=N
	    ELSE
	      ARR(IDDOVAR(1,NESTLV))%VAL(IDDOVAR(2,NESTLV))=N
	    ENDIF
	  ENDIF
        NESTRT=0
        IC=IC+1
      ENDIF
      IRTN=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,902) MNEST
 902  FORMAT(' (SUBR.RDDO) Nest level too deep. ',/,
     %  ' Sum of PUSH,IF,DO nest must be <=',I2)
      RETURN
 905  IRTN=1005
      WRITE(MSGFL,906) MNEST
 906  FORMAT(' (SUBR.RDDO) REPEAT/WHILE must be the first operand.')
      RETURN
 910  IRTN=1010
      WRITE(MSGFL,911)
 911  FORMAT(' (SUBR.RDDO) Invalid syntax for DO command.')
      CALL PRECHO(LN(1,1),LN(2,1),LN(1,3),LN(2,3),LINE,NCHAR)
	IF(IRW.EQ.1.OR.IRW.EQ.2) THEN
        WRITE(MSGFL,912)
 912    FORMAT('  Must be "DO REPEAT expression;" or "DO WHILE ',
     %   'logical-expression;".')
	ENDIF
      RETURN
 920  IRTN=1020
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDDO) Corresponding ENDDO not found.')
      RETURN
 930  IRTN=1030
      WRITE(MSGFL,935) TEXT(1:NC),ERR
 935  FORMAT(' (SUBR.RDDO) Error in evaluating the expression',
     %   '   "',A,'".',/,3X,A) 
      RETURN
 940  IRTN=1040
      WRITE(MSGFL,945) ERR,TEXT(1:NC)
 945  FORMAT(' (SUBR.RDDO) ',A,/,
     %   '   Error in evaluating the logical expression',/,
     %   '   "',A,'".') 
      RETURN
 950  IRTN=1050
      WRITE(MSGFL,945) TEXT(1:NC)
 955  FORMAT(' (SUBR.RDDO) Unexpected character expression "',A,'"')
      RETURN
 960  IRTN=1060
      WRITE(MSGFL,961)
 961  FORMAT(' (SUBR.RDDO) DO control variable must floating type.')
      RETURN
 962  IRTN=1062
      WRITE(MSGFL,963)
 963  FORMAT(' (SUBR.RDDO) Undefine array for DO control variable.')
      RETURN
 964  IRTN=1064
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.RDDO) Invalid array index for DO control ',
     %   'variable.')
      RETURN
 966  IRTN=1066
      WRITE(MSGFL,967)
 967  FORMAT(' (SUBR.RDDO) Invalid range of DO control ',
     %   'variable.')
      IF(NF.EQ.1) WRITE(MSGFL,968)
 968  FORMAT('        Missing () ?')
      RETURN
 970  IRTN=1070
      IF(IDDOVAR(2,NESTLV).EQ.0) THEN
	  NC=NCNAM
	  TEXT(1:NC)=NAM(1:NC)
	ELSE
	  CALL ARRAYSTR(NAM(1:NCNAM),NIND,1,1,IND,TEXT,NC)
	ENDIF
      WRITE(MSGFL,971) TEXT(1:NC)
 971  FORMAT(' (SUBR.RDDO) DO control variable ',A,' already used ',
     %  'in upper level.')
      RETURN
 990  IRTN=1009
      RETURN
      END
