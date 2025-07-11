      SUBROUTINE PLHIST(FILE,NEW,LR,KIN,LLOST,ITYPE,FSEL,
     %   FUNH,FUNV,XYMM,LOGHV,NBIN,LERRBAR,TITLE,TITLEX,TITLEY,
     %   LM,ICOLOR,MSGFL)
C  XYMM(1,1),XYMM(2,1):  x-range
C  XYMM(2,2):          y-max.  If XYMM(1,2)=UNDEF or XYMM(2,2)=UNDEF,
C                    y-max is automatically determined and
C                    stored in XYMM(2,2).
C    LLOST=0: plot only non-lost particles (LOST=0)
C          1: plot only lost particles with LOST=2
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER FILE,NEW,LR(2),KIN(3),LLOST,ITYPE(3),LOGHV(2),
     %   NBIN,LERRBAR,LM,ICOLOR,MSGFL
      CHARACTER*(*) FSEL,FUNH,FUNV
      REAL*8 XYMM(2,2)
      CHARACTER*(*) TITLE,TITLEX,TITLEY
      INTEGER MBIN,NBN
      PARAMETER (MBIN=500)
      REAL*8 XBIN(0:MBIN),PBIN(MBIN),FBIN(MBIN),ERRP(MBIN),ERRF(MBIN),
     %    BINW,FMAX,FMAX2,FMIN,X1,Y1,WGT1,FIN,FOUT,PWEIGHT,DUMMY(1)
	TYPE(FLCHTYPE) FSEL1,XC
      INTEGER NPCOUNT,IFLRKN
      INTEGER I,IM,I2,N,NC0(2,3),NC(3),NP,IRTN,NDTIN,NDTOUT,NC1
      REAL*8 XLGSCL
      REAL*8 XYW0(2,2)/0.0,13.0,0.0,10.0/,XYW(2,2)/0.0,10.8,0.0,10.0/
      CHARACTER*256 TTL(2,3)
      CHARACTER*1 SEP/';'/
      CHARACTER*256 TEXT
      INTEGER IDLMH,IDLMV,IDLMSEL
C
	IDLMH=0
      NP=NPCOUNT(0,LR,KIN,0,0,LLOST,ITYPE,FSEL)
      IF(NP.EQ.0) GOTO 900
      NBN=NBIN
      IF(NBIN.LE.1.OR.NBIN.GT.MBIN) NBN=50
      IF(LOGHV(1).LE.0) THEN
        BINW=(XYMM(2,1)-XYMM(1,1))/NBN
        DO 160 I=0,NBN
          XBIN(I)=XYMM(1,1)+I*BINW
 160    CONTINUE
      ELSE
        BINW=LOG(XYMM(2,1)/XYMM(1,1))/NBN
        DO 180 I=0,NBN
          XBIN(I)=XYMM(1,1)*EXP(BINW*I)
 180    CONTINUE
      ENDIF
      CALL EVCMPL(FUNH,IDLMH,TEXT)
	IF(TEXT.NE.' ') GOTO 920
	IDLMV=0
	IF(FUNV.NE.' ') THEN
	  CALL EVCMPL(FUNV,IDLMV,TEXT)
	  IF(TEXT.NE.' ') GOTO 940
	ENDIF
	IDLMSEL=0
	IF(FSEL.NE.' ') THEN
	  CALL EVCMPL(FSEL,IDLMSEL,TEXT)
	  IF(TEXT.NE.' ') GOTO 930
	ENDIF
	PBIN=0
	ERRP=0
	IF(IDLMV.NE.0) THEN
	  FBIN=0
	  ERRF=0
	ENDIF
	FBIN=0
      NDTIN=0
      NDTOUT=0
      FIN=0
      FOUT=0
      DO 220 N=1,NP
        I=IFLRKN(N,LR,KIN,0,0,LLOST,ITYPE)
        IF(I.EQ.0) CYCLE
        WGT1=PWEIGHT(N)
        IF(WGT1.EQ.0) CYCLE
        CALL SETVAR(N,IRTN)
        IF(IRTN.NE.0) CYCLE
	  IF(IDLMSEL.NE.0) THEN
	    CALL EVLOAD(IDLMSEL,FSEL1,IRTN)
	    IF(FSEL1%L.NE.1.OR.IRTN.NE.0) CYCLE
	    IF(FSEL1%X.EQ.0) CYCLE
	  ENDIF
        CALL EVLOAD(IDLMH,XC,IRTN)
        IF(XC%L.NE.1.OR.IRTN.NE.0) CYCLE
        IF(LOGHV(1).LE.0) THEN
          I=INT((XC%X-XYMM(1,1))/BINW+1)
        ELSE
          IF(XC%X.LE.0) GOTO 220
          I=INT(LOG(XC%X/XYMM(1,1))/BINW+1)
        ENDIF
        IF(I.GE.1.AND.I.LE.NBN) THEN
          PBIN(I)=PBIN(I)+WGT1
	    ERRP(I)=ERRP(I)+WGT1**2
	    IF(IDLMV.NE.0) THEN
	      CALL EVLOAD(IDLMV,XC,IRTN)
	      IF(XC%L.NE.1.OR.IRTN.NE.0) CYCLE
	      FBIN(I)=FBIN(I)+WGT1*XC%X
	      ERRF(I)=ERRF(I)+WGT1*XC%X**2
	    ENDIF
          NDTIN=NDTIN+1
          FIN=FIN+WGT1
        ELSE
          NDTOUT=NDTOUT+1
          FOUT=FOUT+WGT1
        ENDIF
 220  CONTINUE
      IF(NDTIN.EQ.0) THEN
        IF(NDTOUT.EQ.0) GOTO 900
        GOTO 910
      ENDIF
	DO I=1,NBN
        
	ENDDO
	IF(IDLMV.EQ.0) THEN
        FMIN=PBIN(1)
        DO I=1,NBN
	    ERRP(I)=SQRT(ERRP(I))
          FMIN=MIN(FMIN,PBIN(I))
        ENDDO
	ELSE
	  FMAX=-1D60
	  FMIN=1D60
	  DO I=1,NBN
	    IF(PBIN(I).NE.0) THEN
c 	      FBIN(I)=FBIN(I)/PBIN(I)               turn off averaging over bin  
c	      ERRF(I)=SQRT(MAX(0D0,ERRF(I)/PBIN(I)-FBIN(I)**2))
	      FBIN(I)=FBIN(I)
	      ERRF(I)=SQRT(MAX(0D0,ERRF(I)-FBIN(I)**2))
	      ERRP(I)=SQRT(ERRP(I))
	      FMAX=MAX(FMAX,FBIN(I))
	      FMIN=MIN(FMIN,FBIN(I))
	    ELSE
c	      FBIN(I)=UNDEF%X
	    ENDIF
	  ENDDO
	  IF(LOGHV(2).GT.0.AND.FMIN.LE.0) GOTO 950
	ENDIF
      IF(XYMM(1,2).EQ.UNDEF%X.OR.XYMM(2,2).EQ.UNDEF%X) THEN
	  IF(IDLMV.EQ.0) THEN
C         Determine fmax using the second largest one,
C         if the first largest is too large.
          CALL FMAXBN(NBN,PBIN,IM,FMAX,I2,FMAX2,0,DUMMY)
          FMAX=FMAX+ERRP(IM)
          FMAX2=FMAX2+ERRP(I2)
          IF(LOGHV(2).LE.0.AND.FMAX2.LE.0.2*FMAX.AND.FMAX2.NE.0) THEN
c   disable 2nd largest feature T.Barklow 03aug2020           FMAX=FMAX2
          ELSE
            IM=0
          ENDIF
          IF(LOGHV(2).LE.0) THEN
            FMAX=FMAX*1.05
            FMIN=0
          ELSE
            FMAX=FMAX*1.2
            FMIN=MAX(FMIN*0.9,FMAX/1D4)
          ENDIF
          XYMM(2,2)=FMAX
          XYMM(1,2)=FMIN
	  ELSE
	    IF(XYMM(1,2).EQ.UNDEF%X) THEN
	      IF(LOGHV(2).LE.0) THEN
	        XYMM(1,2)=FMIN-0.05D0*(FMAX-FMIN)
	      ELSE
	        XYMM(1,2)=FMIN*(FMAX/FMIN)**(-0.05D0)
	      ENDIF
	    ENDIF
	    IF(XYMM(2,2).EQ.UNDEF%X) THEN
	      IF(LOGHV(2).LE.0) THEN
	        XYMM(2,2)=FMAX+0.05D0*(FMAX-FMIN)
	      ELSE
	        XYMM(2,2)=FMAX*(FMAX/FMIN)**0.05D0
	      ENDIF
	    ENDIF
	    IM=0
	  ENDIF
      ELSE
        FMAX=XYMM(2,2)
        FMIN=XYMM(1,2)
        IM=0
      ENDIF
      CALL RDTTL(TITLE,2,TTL(1,1),NC0(1,1),SEP)
      CALL RDTTL(TITLEX,2,TTL(1,2),NC0(1,2),SEP)
      CALL RDTTL(TITLEY,2,TTL(1,3),NC0(1,3),SEP)
      DO 260 I=1,3
        NC(I)=NC0(1,I)
 260  CONTINUE
	IF(IDLMV.EQ.0) THEN
        CALL HIST(FILE,NEW,LM,ICOLOR,NBN,XBIN,PBIN,LERRBAR,ERRP,
     %      XYW,XYMM,LOGHV,TTL,NC)
	ELSE
        CALL HIST(FILE,NEW,LM,ICOLOR,NBN,XBIN,FBIN,LERRBAR,ERRF,
     %      XYW,XYMM,LOGHV,TTL,NC)
	ENDIF
      X1=XYW(2,1)+0.1
      Y1=XYW(1,2)+0.25*6
      WRITE(FILE,360) X1,Y1,X1,Y1-0.25,NDTIN,
     %  X1,Y1-0.5,NDTOUT,X1,Y1-0.75,X1,Y1-1.0,FIN,
     %  X1,Y1-1.25,FOUT
 360  FORMAT(' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     % ' MORE ',1H','# of macro particles',1H',/,
     % ' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     % ' MORE ',1H',' in bin range ',I12,1H',/,
     % ' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     % ' MORE ',1H',' out of range ',I12,1H',/,
     % ' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     % ' MORE ',1H','# of real  particles',1H',/,
     % ' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     % ' MORE ',1H','   in   ',1PD9.3,1H',/,
     % ' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     % ' MORE ',1H','   out  ',1PD9.3,1H')
C
      IF(IM.NE.0) THEN
        X1=XYW(2,1)+0.1
        Y1=XYW(1,2)+0.25*8
        WRITE(FILE,520) X1,Y1,XLGSCL(LOGHV(1),XBIN(IM-1),XBIN(IM)),
     %     BINW/2,X1,Y1-0.25,PBIN(IM)
 520    FORMAT(' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     %    ' MORE ',1H','bin',1PD10.3,'+',1PD10.3,1H',/,
     %    ' CASE ',1H','   ',10X,    'M',10X,    1H',/,
     %    ' TITLE ',0P2F7.3,' SIZE 1.3 ',2H'',/,
     %    ' MORE ',1H','       =',1PD9.3,1H')
      ENDIF
      GOTO 1000
 900  CALL TYPTTL(LR,KIN,LLOST,ITYPE,TEXT,NC1)
      WRITE(MSGFL,905) TEXT(1:NC1),FSEL
 905  FORMAT(' (SUBR.PLHIST) No particles satisfying the condition:',
     %    /,10X,A,1X,A)
      GOTO 1000
 910  WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.PLHIST) No particles in the specified bin range.')
      GOTO 1000
 920  WRITE(MSGFL,925) FUNH,TEXT(1:80)
 925  FORMAT(' (SUBR.PLHIST) Invalid function "',A,'".',/,5X,A)
      GOTO 1000
 930  WRITE(MSGFL,935) FSEL,TEXT(1:80)
 935  FORMAT(' (SUBR.PLHIST) Invalid SELECT function "',A,'".',/,5X,A)
      GOTO 1000
 940  WRITE(MSGFL,945) FUNV,TEXT(1:80)
 945  FORMAT(' (SUBR.PLHIST) Invalid function "',A,'".',/,5X,A)
      GOTO 1000
 950  WRITE(MSGFL,955) FUNV
 955  FORMAT(' (SUBR.PLHIST) Vertical log axis apecified but',/,
     %   '   the function "',A,'" <0 for some particles.')
      GOTO 1000
1000  IF(IDLMH.GE.1) CALL EVLMFREE(IDLMH)
	IF(IDLMV.GE.1) CALL EVLMFREE(IDLMV)
	IF(IDLMSEL.GE.1) CALL EVLMFREE(IDLMSEL)
	RETURN
      END
