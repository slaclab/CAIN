      SUBROUTINE RDLQED(LN,LINE,NCHAR,IRTN)
C The first operand must be either COMPTON or
C BREITWHEELER
	USE FLCHTYP
	USE READMOD
	USE LASRDATA
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
	INCLUDE 'include/lasrcm.h'
      INTEGER MOP
      PARAMETER (MOP=18)
      CHARACTER*16 OP(MOP)/
     %     'COMPTON','BREITWHEELER','BETHEHEITLER',
     %     'LINEARPOL','CIRCULARPOL',
     %  'NPH','NY','NXI','NLAMBDA','NQ','NPHI',
     %  'XIMAX','LAMBDAMAX','ETAMAX',
     %  'PMAX','PSTOP','ENHANCEFUNCTION','LENHANCE'/
      INTEGER NFF(MOP)/-1,-1,-1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1/
	INTEGER IDNPH,IDNY,IDNXI,IDNLAM,IDNQ,IDNPHI,IDXIM,IDLAMM,IDETAM,
     %   IDPMAX,IDPSTOP,IDENHF,IDLENH
      INTEGER LCPBW,NPH,NY,NXI,NLAM,NQ,NPHI,LENH,NCENHF,LCIRCLIN
      REAL*8 XIMAX,LAMMAX,ETAMAX,PMAX,PSTOP
      EXTERNAL ENHFCP,ENHFBW
C
      INTEGER L,J,K,NF,I,NC
      CHARACTER*256 ENHFUN
C
      IRTN=0
      L=LN(1,2)
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('LASERQED',MOP,OP,1,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
C set default
	J=1
      DO 160 I=1,MOP
	  ID(I)=J
	  IF(OP(I).EQ.'NPH') IDNPH=ID(I)
	  IF(OP(I).EQ.'NY') IDNY=ID(I)
	  IF(OP(I).EQ.'NXI') IDNXI=ID(I)
	  IF(OP(I).EQ.'NLAMBDA') IDNLAM=ID(I)
	  IF(OP(I).EQ.'NQ') IDNQ=ID(I)
	  IF(OP(I).EQ.'NPHI') IDNPHI=ID(I)
	  IF(OP(I).EQ.'XIMAX') IDXIM=ID(I)
	  IF(OP(I).EQ.'LAMBDAMAX') IDLAMM=ID(I)
	  IF(OP(I).EQ.'ETAMAX') IDETAM=ID(I)
	  IF(OP(I).EQ.'PMAX') IDPMAX=ID(I)
	  IF(OP(I).EQ.'PSTOP') IDPSTOP=ID(I)
	  IF(OP(I).EQ.'ENHANCEFUNCTION') IDENHF=ID(I)
	  IF(OP(I).EQ.'LENHANCE') IDLENH=ID(I)
	  J=J+MAX(0,NFF(I))
 160  CONTINUE
      DO 180 I=1,J-1
	  PAR(I)%L=1
        PAR(I)%X=0
 180  CONTINUE
	LCIRCLIN=1
	PAR(IDNY)%X=20
	PAR(IDNXI)%X=20
	PAR(IDNLAM)%X=20
	PAR(IDNPHI)%X=20
	PAR(IDNQ)%X=50
	PAR(IDPMAX)%X=0.5D0
	PAR(IDPSTOP)%X=100
	NGSTRRD=0
      NCENHF=0
C
      LCPBW=KBL(1)
      DO 300 J=2,NBL
        I=KBL(J)
	  IF(NFF(I).EQ.0) THEN
	    IF(OP(I).EQ.'LINEARPOL') THEN
	      LCIRCLIN=2
	    ELSEIF(OP(I).EQ.'CIRCULARPOL') THEN
	      LCIRCLIN=1
	    ENDIF
	  ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          IF(OP(I).EQ.'ENHANCEFUNCTION') THEN
            CALL APSOFF(TEXT(1:NC),NC)
            ENHFUN=TEXT(1:NC)
            NCENHF=NC
          ELSE
            DO 220 K=1,NFF(I)
	        FFF(K)=UNDEF
 220        CONTINUE
            CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
            IF(IRTN.NE.0) GOTO 990
	      IF(NF.GT.NFF(I)) GOTO 950
            IF(NF.GE.1) THEN
              DO 240 K=1,NF
                IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 240          CONTINUE
            ENDIF
          ENDIF
	  ENDIF
 300  CONTINUE
      NPH=NINT(PAR(IDNPH)%X)
      NY=NINT(PAR(IDNY)%X)
      NXI=NINT(PAR(IDNXI)%X)
      NLAM=NINT(PAR(IDNLAM)%X)
      NQ=NINT(PAR(IDNQ)%X)
	NPHI=NINT(PAR(IDNPHI)%X)
      XIMAX=PAR(IDXIM)%X
      LAMMAX=PAR(IDLAMM)%X
      ETAMAX=PAR(IDETAM)%X
      PMAX=MIN(1D0,PAR(IDPMAX)%X)
	IF(PMAX.LE.0) PMAX=0.5D0
	PSTOP=PAR(IDPSTOP)%X
	IF(PSTOP.LE.0) PSTOP=100
      LENH=0
      IF(NCENHF.GE.1) THEN
	  LENH=NINT(PAR(IDLENH)%X)
	  IF(LENH.LE.0) LENH=1
	ENDIF
      IF(LCPBW.EQ.1) THEN
C   Compton
        IF(MSGLVL.GE.1)
     %    CALL RDLQED1(1,LCIRCLIN,NY,NPH,NXI,NLAM,NPHI,XIMAX,LAMMAX,
     %    PMAX,PSTOP,LENH,ENHFUN,NCENHF,MSGFL)
        NPHCP=NPH
        PMAXCP=PMAX
	  PSTOPCP=PSTOP
        IF(NPH.GE.1) THEN
          IF(XIMAX.LE.0.OR.LAMMAX.LE.0) GOTO 910
          LENHCP=LENH
          IF(LENH.NE.0) THEN
            ENHCP=ENHFUN
            NCEHCP=NCENHF
          ENDIF
	    CALL NLCPST(LCIRCLIN,NY,NPH,NXI,NLAM,NPHI,XIMAX,LAMMAX,
     %      LENH,ENHFCP,IRTN)
          IF(IRTN.NE.0) GOTO 930
        ENDIF
      ELSEIF(LCPBW.EQ.2) THEN
C  Breit-Wheeler
        IF(MSGLVL.GE.1)
     %    CALL RDLQED1(2,LCIRCLIN,NY,NPH,NXI,NQ,NPHI,XIMAX,ETAMAX,
     %    PMAX,PSTOP,LENH,ENHFUN,NCENHF,MSGFL)
        NPHBW=NPH
        PMAXBW=PMAX
	  PSTOPBW=PSTOP
        IF(NPH.GE.1) THEN
	    IF(LCIRCLIN.EQ.2) GOTO 960
          IF(XIMAX.LE.0.OR.ETAMAX.LE.0) GOTO 920
          LENHBW=LENH
          IF(LENH.NE.0) THEN
            ENHBW=ENHFUN
            NCEHBW=NCENHF
          ENDIF
          CALL NLBWST(NY,NPH,NXI,NQ,XIMAX,ETAMAX,
     %      LENH,ENHFBW,IRTN)
          IF(IRTN.NE.0) GOTO 940
       ENDIF
      ELSE 
C  Bethe-Heitler
        IF(MSGLVL.GE.1)
     %    CALL RDLQED1(3,LCIRCLIN,NY,NPH,NXI,NQ,NPHI,XIMAX,ETAMAX,
     %    PMAX,PSTOP,LENH,ENHFUN,NCENHF,MSGFL)
        NPHBH=NPH
        PMAXBH=PMAX
      ENDIF
      IRTN=0
      RETURN
C
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDLQED) XIMAX and LAMBDAMAX must be ',
     %  'specified.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDLQED) XIMAX and ETAMAX must be ',
     %  'specified.')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.RDLQED) Initialization of nonlinear',
     %  ' Compton failed.')
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.RDLQED) Initialization of nonlinear',
     %  ' Compton failed.')
      RETURN
 950  IRTN=1005
      WRITE(MSGFL,955) OP(J)
 955  FORMAT(' (SUBR.RDLQED) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.RDLQED) Sorry. Nonliner Breit-Wheeler not ready ',
     %  'for linear polarization')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDLQED1(L,LCIRCLIN,NY,NPH,NXI,NQ,NPHI,XIMAX,QMAX,PMAX,
     %      PSTOP,LENH,ENHFUN,NCENHF,MSGFL)
      IMPLICIT NONE
      INTEGER L,LCIRCLIN,NY,NPH,NXI,NQ,NPHI,LENH,NCENHF,MSGFL
      REAL*8 XIMAX,QMAX,PMAX,PSTOP
      CHARACTER*(*) ENHFUN
      CHARACTER*13 CPBW(3)/'Compton','Breit-Wheeler','Bethe-Heitler'/
      CHARACTER*6 LAMETA(2)/'Lambda','Eta'/
      CHARACTER*6 LAMQ(2)/'Lambda','Q'/
	CHARACTER*8 LLCIRCLIN(2)/'Circular','Linear'/
C
	WRITE(MSGFL,100)
100   FORMAT(' +++ LASERQED +++')
      IF(NPH.LT.0) THEN
        WRITE(MSGFL,110) CPBW(L)
 110    FORMAT(T5,'Ignore Laser ',A)
      ELSEIF(NPH.EQ.0) THEN
        WRITE(MSGFL,120) CPBW(L)
 120    FORMAT(T5,'Interaction type',T50,'Linear ',A)
      ELSE
        WRITE(MSGFL,140) CPBW(L),LLCIRCLIN(LCIRCLIN),NPH,
     %     XIMAX,LAMETA(L),QMAX,PMAX,PSTOP,NY,NXI,LAMQ(L),NQ
 140    FORMAT(
     %   T5,'Interaction type',T50,'Nonlinear ',A,/,
     %   T5,'Polarization',T50,A,/,
     %   T5,'Max.number of photons to be absorbed',T50,I10,/,
     %   T5,'Maximum Xi',T50,0PF10.5,/,
     %   T5,'Maximum ',A,T50,0PF10.5,/,
     %   T5,'Maximum event probability per step',T50,0PF10.6,/,
     %   T5,'Maximum probality per step to stop job',T50,0PF10.2,/,
     %   T5,'Number of final energy points',T50,I10,/, 
     %   T5,'Number of Xi''s',T50,I10,/, 
     %   T5,'Number of parameter ',A,T50,I10)
        IF(LCIRCLIN.EQ.2) WRITE(MSGFL,160) NPHI
 160    FORMAT(T5,'Number of azimuthal angle',T50,I10)
        IF(LENH.NE.0) THEN
	    IF(NCENHF.LE.25) THEN
		  WRITE(MSGFL,170) ENHFUN(1:NCENHF)
 170        FORMAT(T5,'Rate enhancement funtion =',T50,A)
	    ELSE
	      WRITE(MSGFL,180) ENHFUN(1:NCENHF)
 180        FORMAT(T5,'Rate enhancement funtion =',/,T8,A)
          ENDIF
	  ENDIF
      ENDIF


      RETURN
      END

  
