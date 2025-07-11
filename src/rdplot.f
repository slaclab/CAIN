      SUBROUTINE RDPLOT(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE ARRAYMOD
	USE BBCOM
	USE LASRDATA
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP,MOP1
      PARAMETER (MOP=50,MOP1=8)
      CHARACTER*12 OP(MOP)/
     %  'HISTOGRAM','SCATTER','TESTPARTICLE','LUMINOSITY','BBFIELD',
     %  'BLOPTICS','BLGEOMETRY','FUNCTION',
     %  'INCP','RIGHT','LEFT','LOST','HLOG','VLOG','XLOG',
     %  'HLINEAR','VLINEAR','XLINEAR','NONEWPAGE',
     %  'KIND','MAXNP','FILE','APPEND','S','INTERPOLATE',
     %  'HSCALE','VSCALE','RANGE','LINEMODE','COLOR','BEAMLINE',
     %  'TXYS','E3','E1','VHRATIO','MAGWIDTH','PAPER','FILTER',
     %  'PARAMETER','H','V','TITLE','HTITLE','VTITLE',
C            must not insert operands
C            between 'RIGHT' and 'LEFT',
C            between 'HLOG' and 'XLOG', 
C            between 'HLINEAR' and 'XLINEAR', 
C            between 'HSCALE' and 'VSCALE', 
C            between 'H' and 'V', 
C            between 'TITLE' and 'VTITLE'
     %  'PERBIN','PERHVAR','ERRBAR','SELECT','LASERPHOTON','T'/
      INTEGER NFF(MOP)/-1,-1,-1,-1,-1,-1,-1,-1,
     %       0,0,0,0,0,0,0,
     %       0,0,0,0,
     %       3,1,1,0,5,1,
     %       3,3,3,10,1,1,
     %       4,3,3,1,1,2,1,
     %       1,1,1,1,1,1,
     %       0,0,0,1,0,1/
      INTEGER IDKIND,IDMXNP,IDS,IDSCL,IDRNG,IDLMOD,IDT,IDINTP,IDTXYS,
     %       IDE3,IDE1,IDVHRATIO,IDMAGWIDTH,IDPAPER
	INTEGER IPRL,IPPARAM,IPINCP1,IPLOST,IPHV,IPTTL,IPLOGHV,IPLINEAR,
     %        IPCOLOR,IPLMOD,IPKIND,IPSEL,IPSCL,IPRNG,IPFILE,IPAPPEND,
     %        IPLASER,IPNAMFIL
	INTEGER IOP(MOP)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
C      INCLUDE 'include/bbcom.h'
	INCLUDE 'include/topdraw.h'
	INCLUDE 'include/lasrcm.h'
      INTEGER J,I,II,NF,NBIN,NC,K,IRTN1,IOS,LTS,LSR
      INTEGER LR(2),LOGHV(3),NEW,KIN(3),KIN2(2),NKIN2,
     %     FILE,FILE1,NBBPL1,NCHV(2),NCTTL(3),LLOST,ICOLOR,
     %     MAXNP,INCP,ITYPE(3),NDIV,NLMOD,PERHVAR,LERRBAR
	TYPE(FLCHTYPE) FC
      CHARACTER*16 PARNAM
      CHARACTER*256 HV(2),TTL(3)
      REAL*8 HVSCL(3,2),XYMM(2,2),RANGE(2),SLMOD(10),TS,DSINTP
	REAL(8) TXYS0(0:3),E30(3),E10(3),PAPER(2),HVMAT(2,3),VHRATIO,
     %   MAGWIDTH
	INTEGER NCBLNAM
	CHARACTER(32) BLNAM
	CHARACTER(512) FILENAME,FILENAME1
	CHARACTER(80) ERR,NAMFIL
	INTEGER NCFN,NCFN1,NCNAMFIL
	CHARACTER(1024) FSELECT
	INTEGER NCFSEL
	SAVE LR,HV,NCHV,LOGHV,NLMOD,SLMOD,ICOLOR,FSELECT,NCFSEL,
     %     NBIN,HVSCL,RANGE,MAXNP,FILE,FILENAME,NCFN
	INTEGER NCALL/0/
	SAVE NCALL
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      CALL CPUTIM('RDPLOT',1)
      CALL CMDBLK('RDPLOT',MOP,OP,1,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) GOTO 1000
	NCALL=NCALL+1
C
      IOP=0
	TTL=';;'
	NCTTL=2
	NEW=1
	PARNAM=' '
	NCBLNAM=0
      NKIN2=0
      PERHVAR=0
	LERRBAR=0
      INCP=0
	FILENAME1=' '
	IF(NCALL.EQ.1) THEN
		LR(1)=1
	  LR(2)=1
		FSELECT=' '
	  NCFSEL=1
	  FILENAME=' '
	  NCFN=1
	  FILE=TDFL
	ENDIF
	TXYS0=0
	E30=0
	E10=0
	E30(3)=1
	E10(1)=1
	VHRATIO=1.0
	MAGWIDTH=1.0
	PAPER(1)=13
	PAPER(2)=10
	NCNAMFIL=0
      J=1
      DO I=1,MOP
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'PARAMETER') IPPARAM=I
        IF(OP(I).EQ.'KIND') IDKIND=ID(I)
        IF(OP(I).EQ.'MAXNP') IDMXNP=ID(I)
        IF(OP(I).EQ.'S') IDS=ID(I)
	  IF(OP(I).EQ.'T') IDT=ID(I)
        IF(OP(I).EQ.'HSCALE') IDSCL=ID(I)
        IF(OP(I).EQ.'RANGE') IDRNG=ID(I)
        IF(OP(I).EQ.'LINEMODE') IDLMOD=ID(I)
	  IF(OP(I).EQ.'INTERPOLATE') IDINTP=ID(I)
	  IF(OP(I).EQ.'TXYS') IDTXYS=ID(I)
	  IF(OP(I).EQ.'E3') IDE3=ID(I)
	  IF(OP(I).EQ.'E1') IDE1=ID(I)
	  IF(OP(I).EQ.'VHRATIO') IDVHRATIO=ID(I)
	  IF(OP(I).EQ.'MAGWIDTH') IDMAGWIDTH=ID(I)
	  IF(OP(I).EQ.'PAPER') IDPAPER=ID(I)
	  IF(OP(I).EQ.'RIGHT') IPRL=I
	  IF(OP(I).EQ.'INCP') IPINCP1=I
	  IF(OP(I).EQ.'LOST') IPLOST=I
	  IF(OP(I).EQ.'KIND') IPKIND=I
	  IF(OP(I).EQ.'H') IPHV=I
	  IF(OP(I).EQ.'TITLE') IPTTL=I
	  IF(OP(I).EQ.'HLOG') IPLOGHV=I
	  IF(OP(I).EQ.'HLINEAR') IPLINEAR=I
	  IF(OP(I).EQ.'COLOR') IPCOLOR=I
	  IF(OP(I).EQ.'LINEMODE') IPLMOD=I
	  IF(OP(I).EQ.'SELECT') IPSEL=I
	  IF(OP(I).EQ.'HSCALE') IPSCL=I
	  IF(OP(I).EQ.'RANGE') IPRNG=I
	  IF(OP(I).EQ.'FILTER') IPNAMFIL=I
	  IF(OP(I).EQ.'FILE') IPFILE=I
	  IF(OP(I).EQ.'APPEND') IPAPPEND=I
	  IF(OP(I).EQ.'LASERPHOTON') IPLASER=I
      ENDDO
      DO I=1,J-1
        PAR(I)=UNDEF
      ENDDO
	NGSTRRD=0
C
      II=KBL(1)
      DO 300 J=2,NBL
        I=KBL(J)
	  IOP(I)=1
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'NONEWPAGE') THEN
            NEW=0
          ELSEIF(OP(I).EQ.'PERBIN') THEN
            PERHVAR=0
          ELSEIF(OP(I).EQ.'PERHVAR') THEN
            PERHVAR=1
	    ELSEIF(OP(I).EQ.'ERRBAR') THEN
	      LERRBAR=1
          ENDIF
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          IF(I.EQ.IPPARAM) THEN
            PARNAM=TEXT(1:NC)
          ELSEIF(I.GE.IPHV.AND.I.LE.IPHV+1) THEN
            HV(I-IPHV+1)=TEXT(1:NC)
            NCHV(I-IPHV+1)=NC
	    ELSEIF(I.GE.IPTTL.AND.I.LE.IPTTL+2) THEN
	      CALL EVAL0(TEXT(1:NC),FC,ERR)
	      IF(ERR.NE.' '.OR.FC%L.NE.2) GOTO 975
            TTL(I-IPTTL+1)=GSTR2(EVALLAST)(FC%C(1):FC%C(2))
            NCTTL(I-IPTTL+1)=FC%C(2)-FC%C(1)+1
	    ELSEIF(I.EQ.IPNAMFIL) THEN
	      CALL EVAL0(TEXT(1:NC),FC,ERR)
	      IF(ERR.NE.' '.OR.FC%L.NE.2) GOTO 975
	      NCNAMFIL=MIN(LEN(NAMFIL),FC%C(2)-FC%C(1)+1)
	      NAMFIL=GSTR2(EVALLAST)(FC%C(1):FC%C(2))
		  ELSEIF(OP(I).EQ.'COLOR') THEN
	      CALL APSOFF(TEXT(1:NC),NC)
	      CALL TOUPPER(TEXT(1:NC))
	      IOP(I)=0
            DO K=1, MCOLR
	        IF(TEXT(1:NC).EQ.COLOR(K)) THEN
	          ICOLOR=K
	          IOP(I)=1
	          EXIT
	        ENDIF
	      ENDDO
		  ELSEIF(OP(I).EQ.'BEAMLINE') THEN
	      CALL APSOFF(TEXT(1:NC),NC)
            NCBLNAM=NC
            BLNAM=TEXT(1:NC)
		  ELSEIF(OP(I).EQ.'SELECT') THEN
            NCFSEL=NC
            FSELECT=TEXT(1:NC)
	    ELSEIF(OP(I).EQ.'FILE') THEN
	      CALL EVAL0(TEXT(1:NC),FC,ERR)
	      IF(ERR.NE.' ') GOTO 990
	      IF(FC%L.EQ.1) THEN
	        FILE1=NINT(FC%X)
	      ELSE
	        FILENAME1=GSTR2(EVALLAST)(FC%C(1):FC%C(2))
	        NCFN1=FC%C(2)-FC%C(1)+1
            ENDIF
          ELSE
            DO K=1,NFF(I)
	        FFF(K)=UNDEF
            ENDDO
            CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
            IF(IRTN.NE.0) GOTO 990
            IF(NF.GT.NFF(I)) GOTO 900
            IF(NF.GE.1) THEN
              DO 240 K=1,NF
                IF(FFF(K).NE.UNDEF) THEN
                  PAR(ID(I)+K-1)=FFF(K)
                  IF(ID(I).EQ.IDKIND) THEN
                    NKIN2=NKIN2+1
                    IF(NKIN2.LE.2) KIN2(NKIN2)=NINT(FFF(K)%X)
                  ENDIF
                ENDIF
 240          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 300  CONTINUE
      IF(II.EQ.0) GOTO 910
C---  parameters not filled yet
	IF(IOP(IPRL).NE.0.OR.IOP(IPRL+1).NE.0) THEN
	  LR(1)=IOP(IPRL)
	  LR(2)=IOP(IPRL+1)
	ELSEIF(NEW.NE.0) THEN
	  LR(1)=1
	  LR(2)=1
	ENDIF
C   File#/file name
C   When new plot (NEW.NE.0),
C     if file#/file name specified in this call,
C             ----  open if filename given
C     if not specifed in this call
C             ----  FILE=TDFL
C   When NONEWPAGE (NEW.EQ.0)
C             ---  ignore the file#/name in this call even if specified.
C                  If the save filename is ' ', use old FILE.
C                  Otherwise, to re-open with append mode.
C    In any case, if the saved filename is not ' ', the file must be closed
C    afterwards.
	IF(NEW.NE.0) THEN
	  IF(IOP(IPFILE).NE.0) THEN	 
	    IF(FILENAME1.NE.' ') THEN
            FILE=98
	      FILENAME=FILENAME1
	      NCFN=NCFN1
c              print *, " rdplot file= ",file," filename= ",filename
            CALL OPENFL(FILE,FILENAME(1:NCFN),'UNKNOWN',IOP(IPAPPEND),
     %          NC,IRTN1)
c            print *, " rdplot file= ",file," filename= ",filename,
c     &          " irtn1= ",irtn1       
            IF(IRTN1.NE.0) GOTO 982
	    ELSE
	      FILE=FILE1
	    ENDIF
	  ELSE
	    FILE=TDFL
	  ENDIF
	ELSE
	  IF(FILENAME.NE.' ') THEN
	    FILE=98
	    CALL OPENFL(FILE,FILENAME(1:NCFN),'UNKNOWN',1,NC,IRTN1)
          IF(IRTN1.NE.0) GOTO 982
	  ENDIF
      ENDIF
	IF(IOP(IPLMOD).NE.0) THEN
	  NLMOD=1
	  DO I=1,10
          IF(PAR(IDLMOD+I-1).EQ.UNDEF) EXIT
          NLMOD=NLMOD+1
          SLMOD(NLMOD)=PAR(IDLMOD+I-1)%X
        ENDDO
	ELSEIF(NEW.NE.0) THEN
	  NLMOD=1
	  SLMOD(1)=1
	ENDIF
	IF(IOP(IPKIND).NE.0) THEN
	  KIN=0
	  DO I=1,3
          IF(PAR(IDKIND+I-1).NE.UNDEF) THEN
            J=NINT(PAR(IDKIND+I-1)%X)
            IF(J.GE.1.AND.J.LE.3) KIN(J)=1
          ENDIF
        ENDDO
	ELSEIF(NEW.NE.0) THEN
	  KIN=0
	ENDIF
	DO J=1,2
	  IF(IOP(IPSCL+J-1).NE.0) THEN
          DO I=1,3
            HVSCL(I,J)=PAR(IDSCL+I-1+3*(J-1))%X
          ENDDO
	  ELSEIF(NEW.NE.0) THEN
	    DO I=1,3
	      HVSCL(I,J)=UNDEF%X
	    ENDDO
	  ENDIF
      ENDDO
	DO I=1,3
	  IF(IOP(IPLOGHV+I-1).EQ.0) THEN
		  IF(IOP(IPLINEAR+I-1).EQ.0) THEN
		    IF(NEW.NE.0) LOGHV(I)=0
	    ELSE
	      LOGHV(I)=0
	    ENDIF
	  ELSE
	    IF(IOP(IPLINEAR+I-1).EQ.0) THEN
	      LOGHV(I)=1
	    ELSE
	      LOGHV(I)=0   !  invalid  both LOG and LINEAR
	    ENDIF
	  ENDIF
	ENDDO
	IF(PAR(IDMXNP).NE.UNDEF) THEN
	  MAXNP=MAX(0,NINT(PAR(IDMXNP)%X))
	ELSEIF(NEW.NE.0) THEN
	  MAXNP=0
	ENDIF

C--- parameters already filled if specified
	IF(NEW.NE.0) THEN
	  IF(IOP(IPCOLOR).EQ.0) ICOLOR=0
	  IF(IOP(IPSEL).EQ.0) THEN
	    FSELECT=' '
	    NCFSEL=1
	  ENDIF
	  DO I=1,2
	    IF(IOP(IPHV+I-1).EQ.0) THEN
	      NCHV(I)=1
	      HV(I)=' '
	    ENDIF
	  ENDDO
	ENDIF
C---------------
      IF(OP(II).EQ.'BBFIELD') GOTO 600
	IF(OP(II).EQ.'BLOPTICS') GOTO 700
	IF(OP(II).EQ.'BLGEOMETRY') GOTO 750

      DO I=1,3
        CALL APSOFF(TTL(I),NC)
      ENDDO
      DO I=1,2
        XYMM(I,1)=HVSCL(I,1)
        XYMM(I,2)=HVSCL(I,2)
      ENDDO
      IF(OP(II).EQ.'LUMINOSITY') GOTO 580
      DO I=1,2
        IF(LOGHV(I).GE.1) THEN
          IF(XYMM(1,I).LE.0.OR.XYMM(2,I).LE.0) GOTO 920
          IF(OP(II).EQ.'SCATTER'.OR.I.EQ.1) THEN
            IF(XYMM(1,I).EQ.UNDEF%X.OR.XYMM(2,I).EQ.UNDEF%X)
     %        GOTO 930
          ENDIF
        ENDIF
      ENDDO
      IF(OP(II).EQ.'FUNCTION') GOTO 500
      CALL ALTRUE(LR,2)
      CALL ALTRUE(KIN,3)
C  Rule of selecting incoherent particles
C   When INCP specified, include only incoherent particles
C   When SELECT is specified, include both normal and incoherent particles
C   When none of INCP and SELECT is specified, include only normal particles
C  In any case SELECT function, if specified, is applied on the subset defined above.
      ITYPE(1)=1
      ITYPE(2)=0
      ITYPE(3)=0
	INCP=IOP(IPINCP1)
      IF(INCP.NE.0) THEN
        ITYPE(1)=0
        ITYPE(2)=1
	ELSEIF(IOP(IPSEL).NE.0) THEN
        ITYPE(1)=1
        ITYPE(2)=1
      ENDIF
	LLOST=IOP(IPLOST)
      GOTO (440,460,480), II
C- Histogram
 440  IF(HVSCL(3,1).EQ.UNDEF%X) THEN
        NBIN=50
      ELSE
        NBIN=NINT(HVSCL(3,1))
      ENDIF
      CALL PLHIST(FILE,NEW,LR,KIN,LLOST,ITYPE,FSELECT(1:NCFSEL),
     %   HV(1)(1:NCHV(1)),HV(2)(1:NCHV(2)),XYMM,LOGHV,NBIN,LERRBAR,
     %   TTL(1)(1:NCTTL(1)),TTL(2)(1:NCTTL(2)),TTL(3)(1:NCTTL(3)),
     %   1,ICOLOR,MSGFL)
      IRTN=0
      GOTO 1000
C- Scatter plot
 460  IF(IOP(IPLASER).NE.0) THEN
        IF(NLSR.LE.0) GOTO 980
	  IF(PAR(IDT).NE.UNDEF) THEN
	    LTS=1
	    TS=PAR(IDT)%X
	  ELSEIF(PAR(IDS).NE.UNDEF) THEN
	    LTS=2
	    TS=PAR(IDS)%X
	  ELSE
	    GOTO 978
	  ENDIF
	  LSR=1
	ELSE
	  LSR=0
	ENDIF
	CALL PLSCAT(FILE,NEW,LR,KIN,LLOST,ITYPE,FSELECT(1:NCFSEL),
     %  HV(1)(1:NCHV(1)),HV(2)(1:NCHV(2)),XYMM,LOGHV,MAXNP,
     %  LSR,NLSR,LTS,TS,
     %  TTL(1)(1:NCTTL(1)),TTL(2)(1:NCTTL(2)),TTL(3)(1:NCTTL(3)),
     %  ICOLOR,MSGFL)
        IRTN=0
      GOTO 1000
C- Test Particles
 480  CALL PLTSTP(FILE,LR,KIN,HV(1)(1:NCHV(1)),HV(2)(1:NCHV(2)),
     %  XYMM,LOGHV,
     %  TTL(1)(1:NCTTL(1)),TTL(2)(1:NCTTL(2)),TTL(3)(1:NCTTL(3)),
     %  ICOLOR,MSGFL)
      IRTN=0
      GOTO 1000
C- Function
 500  IF(PARNAM.EQ.' ') GOTO 940
 	IF(IOP(IPRNG).NE.0) THEN
        DO I=1,2
          IF(PAR(IDRNG+I-1).EQ.UNDEF) GOTO 950
          RANGE(I)=PAR(IDRNG+I-1)%X
        ENDDO
        IF(PAR(IDRNG+2).NE.UNDEF) THEN
          NDIV=NINT(PAR(IDRNG+2)%X)
        ELSE
          NDIV=0
        ENDIF
	ELSEIF(NEW.NE.0) THEN
	  GOTO 950
	ENDIF
      CALL PLFUNC(FILE,NEW,PARNAM,RANGE,NDIV,LOGHV(3),
     %   HV(1)(1:NCHV(1)),HV(2)(1:NCHV(2)),
     %   XYMM,LOGHV,NLMOD,SLMOD,
     %   TTL(1)(1:NCTTL(1)),TTL(2)(1:NCTTL(2)),TTL(3)(1:NCTTL(3)),
     %   ICOLOR,MSGFL,IRTN)
      IF(IRTN.NE.0) GOTO 990
      IRTN=0
      GOTO 1000
C- Luminosity
 580  IF(NKIN2.NE.2) GOTO 960
      CALL LUMPLT(FILE,NEW,KIN2,HV(2)(1:NCHV(2)),PERHVAR,
     %    LOGHV(2),XYMM(1,2),UNDEF,
     %    TTL(1)(1:NCTTL(1)),TTL(2)(1:NCTTL(2)),TTL(3)(1:NCTTL(3)),
     %    1,ICOLOR,IRTN)
      IF(IRTN.NE.0) GOTO 990
      GOTO 1000
C- Beam-beam field
 600  IF(BBON.EQ.0) THEN
        WRITE(MSGFL,610)
 610    FORMAT(' (SUBR.PLOT) Plot beam-beam field effective in ',
     %     'PUSH loop only.')
        IRTN=0
        GOTO 1000
      ENDIF
      NBBPL1=0
      DO 620 I=1,5
        IF(PAR(IDS+I-1).NE.UNDEF) THEN
          NBBPL1=NBBPL1+1
          SBBPL(NBBPL1)=PAR(IDS+I-1)%X
        ENDIF
 620  CONTINUE
      NBBPL=NBBPL1
      IFLBPL=FILE
      IRTN=0
c     GOTO 1000
      return
C- Beamline optics
 700  IF(NCBLNAM.LE.0) GOTO 970
	IF(PAR(IDINTP).NE.UNDEF) THEN
	  DSINTP=MAX(0D0,PAR(IDINTP)%X)
	ELSE
	  DSINTP=0
	ENDIF
	CALL PLBLOPT(FILE,BLNAM(1:NCBLNAM),DSINTP,IRTN1)
	IF(IRTN1.NE.0) GOTO 990
	GOTO 1000
C- Beamline geometry
 750  IF(NCBLNAM.LE.0) GOTO 970
	IF(PAR(IDTXYS).NE.UNDEF) THEN
	  DO I=1,3   !! note: TXYS(0) not used
	    TXYS0(I)=PAR(IDTXYS+I)%X
	  ENDDO
	ENDIF
	IF(PAR(IDE3).NE.UNDEF) THEN
	  DO I=1,3
		  E30(I)=PAR(IDE3+I-1)%X
	  ENDDO
	ENDIF
	IF(PAR(IDE1).NE.UNDEF) THEN
	  DO I=1,3
		  E10(I)=PAR(IDE1+I-1)%X
	  ENDDO
	ENDIF
	IF(PAR(IDMAGWIDTH).NE.UNDEF) MAGWIDTH=PAR(IDMAGWIDTH)%X
	IF(PAR(IDPAPER).NE.UNDEF) THEN
	  DO I=1,2
	    PAPER(I)=PAR(IDPAPER+I-1)%X
	  ENDDO
	ENDIF
	HVMAT=0
	HVMAT(1,3)=1
	HVMAT(2,1)=1 
	IF(PAR(IDVHRATIO).NE.UNDEF) VHRATIO=PAR(IDVHRATIO)%X
      CALL PLBLGEOM(BLNAM(1:NCBLNAM),TXYS0(1),E10,E30,HVMAT,
     %  PAPER,VHRATIO,MAGWIDTH,NAMFIL(1:NCNAMFIL),FILE,IRTN1)
	IF(IRTN1.NE.0) GOTO 990
	GOTO 1000
C
 900  IRTN=1000
      WRITE(MSGFL,905) OP(I)
 905  FORMAT(' (SUBR.RDPLOT) Too many numbers for ',
     %  'operand "',A,'".')
      GOTO 1000
 910  IRTN=1001
      WRITE(MSGFL,915) (OP(I),I=1,MOP1)
 915  FORMAT(' (SUBR.RDPLOT) One the following ',
     %  'must be specified.',/,2X,10(1X,A))
      GOTO 1000
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDPLOT) Scale min/max < 0 for log-scale plot.')
      GOTO 1000
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.RDPLOT) Scale min/max must be specified ',
     %  'for log-scale plot.')
      GOTO 1000
 940  IRTN=1004
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.RDPLOT) PARAMETER operand not defined.')
      GOTO 1000
 950  IRTN=1005
      WRITE(MSGFL,955)
 955  FORMAT(' (SUBR.RDPLOT) RANGE parameter not specified.')
      GOTO 1000
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.RDPLOT) Left/right going beams must be ',
     %  'specified in PLOT LUMINOSITY')
      GOTO 1000
 970  IRTN=1007
      WRITE(MSGFL,972)
 972  FORMAT(' (SUBR.RDPLOT) PLOT BLOPTICS. Invalid beamline name.')
      GOTO 1000
 975  IRTN=1008
      WRITE(MSGFL,976) OP(I),ERR
 976  FORMAT(' (SUBR.RDPLOT) Invalid character expression for ',A,'.',/,
     %     3X,A)
      GOTO 1000
 978  IRTN=1008
      WRITE(MSGFL,979)
 979  FORMAT(' (SUBR.RDPLOT) PLOT SCAT, LASERPHOTON requires either S ',
     %     'or T operand.')
      GOTO 1000
 980  IRTN=1008
      WRITE(MSGFL,981)
 981  FORMAT(' (SUBR.RDPLOT) PLOT SCAT, LASERPHOTON. No laser defined.')
      GOTO 1000
 982  IRTN=1008
	WRITE(MSGFL,983) FILE,FILENAME(1:NCFN)
 983  FORMAT(' (SUBR.RDPLOT) File open error. Unit number',I3,/,
     %  '  File name ',A)
      GOTO 1000
 990  IRTN=1009
      GOTO 1000
 1000 IF(FILENAME.NE.' ') CLOSE(FILE,IOSTAT=IOS)
	CALL CPUTIM('RDPLOT',2)
      RETURN
      END
