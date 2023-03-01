      SUBROUTINE RDLUM(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE LUMCOM
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/lumcom2.h'
C
      INTEGER MOP
      PARAMETER (MOP=10)
      INTEGER MBINLM
      PARAMETER (MBINLM=201)
C   Note that length of PAR (MRDPAR defined in readmod.f) must be long
C   enough.
      CHARACTER*12 OP(MOP)/'PPINT','KIND','FREP','W',
     %   'E1','E2','WX','WY','HELICITY','ALLPOL'/
      INTEGER NFF(MOP)/0,2,1,MBINLM,MBINLM,MBINLM,2,2,0,0/
      INTEGER IDKIND,IDFREP,IDW,IDE(2),IDWX,IOPW,IOPE(2)
      INTEGER LHELI(0:2),LPPINT2,NBNW1,NBNE1,NBNEE(2),RLK(2),
     %   J,NF,I,NC,K,N
      INTEGER NCOMPL(0:2)/1,4,16/
      REAL*8 FREP1,WMM(2),EMM(2,2),PAR1
      INTEGER NPARDEF
      LOGICAL EQBINW,EQBINE(2),CHECKBIN
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      IF(NLUM.GE.MLUM) GOTO 980
      CALL CMDBLK('LUMINOSITY',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
C set default
      LPPINT2=0
      LHELI(0)=1
      LHELI(1)=0
      LHELI(2)=0
      J=1
      DO 180 I=1,MOP
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'KIND') IDKIND=ID(I)
        IF(OP(I).EQ.'FREP') IDFREP=ID(I)
        IF(OP(I).EQ.'W') IDW=ID(I)
        IF(OP(I).EQ.'E1') IDE(1)=ID(I)
        IF(OP(I).EQ.'E2') IDE(2)=ID(I)
        IF(OP(I).EQ.'WX') IDWX=ID(I)
        IF(OP(I).EQ.'W') IOPW=I
        IF(OP(I).EQ.'E1') IOPE(1)=I
        IF(OP(I).EQ.'E2') IOPE(2)=I
 180  CONTINUE
      IF(J-1.GT.MRDPAR) CALL STOPCAIN(120)
C            Program error in control/main/rdlum.h
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
C
      DO 300 J=1,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'PPINT') LPPINT2=1
          IF(OP(I).EQ.'HELICITY') LHELI(1)=1
          IF(OP(I).EQ.'ALLPOL') LHELI(2)=1
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          DO 220 K=1,NFF(I)
	      FFF(K)=UNDEF
 220      CONTINUE
          CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
          IF(IRTN.NE.0) GOTO 990
          IF(NF.GT.NFF(I)) GOTO 920
          IF(NF.GE.1) THEN
            DO 240 K=1,NF
              IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 240        CONTINUE
          ENDIF
        ENDIF
 300  CONTINUE
      DO 420 J=1,2
        IF(PAR(IDKIND+J-1).EQ.UNDEF) GOTO 930
        RLK(J)=NINT(PAR(IDKIND+J-1)%X)
        IF(RLK(J).LE.0.OR.RLK(J).GE.4) GOTO 930
 420  CONTINUE
      FREP1=PAR(IDFREP)%X
      IF(NLUM.EQ.0) THEN
        IF(FREP1.EQ.UNDEF%X) FREP1=1
        FREP=FREP1
      ELSE
        IF(FREP1.NE.UNDEF%X.AND.FREP1.NE.FREP) GOTO 900
      ENDIF
      IF(KOP(IOPW).NE.0) THEN
        WMM(1)=0
        WMM(2)=0
        N=NPARDEF(MBINLM,PAR(IDW))
        IF(N.LE.3) THEN
          IF(N.GE.1) WMM(1)=PAR(IDW)%X
          IF(N.GE.2) WMM(2)=PAR(IDW+1)%X
          IF(WMM(1).GE.WMM(2)) GOTO 950
          IF(N.GE.3) THEN
            NBNW1=NINT(PAR(IDW+2)%X)
            IF(NBNW1.LE.1) NBNW1=50
          ELSE
            NBNW1=50
          ENDIF
          EQBINW=.TRUE.
        ELSE
          IF(CHECKBIN(PAR(IDW),N)) THEN
            NBNW1=NINT(PAR(IDW+2)%X)
            WMM(1)=PAR(IDW)%X
            WMM(2)=PAR(IDW+N-1)%X
            NBNW1=N-1
            EQBINW=.FALSE.
          ELSE
            WRITE(MSGFL,430) 1
            NBNW1=0
          ENDIF
        ENDIF
      ELSE
        NBNW1=0
      ENDIF
      IF((KOP(IOPE(1)).NE.0.AND.KOP(IOPE(2)).EQ.0).OR.
     %   (KOP(IOPE(1)).EQ.0.AND.KOP(IOPE(2)).NE.0)) GOTO 960
      IF(KOP(IOPE(1)).NE.0) THEN
        DO 440 I=1,2
          EMM(1,I)=0
          EMM(2,I)=0
          N=NPARDEF(MBINLM,PAR(IDE(I)))
          IF(N.LE.3) THEN
            IF(N.GE.1) EMM(1,I)=PAR(IDE(I))%X
            IF(N.GE.2) EMM(2,I)=PAR(IDE(I)+1)%X
            IF(EMM(1,I).GE.EMM(2,I)) GOTO 970
            IF(N.GE.3) THEN
              NBNEE(I)=NINT(PAR(IDE(I)+2)%X)
              IF(NBNEE(I).LE.1) NBNEE(I)=50
            ELSE
              NBNEE(I)=50
            ENDIF
            EQBINE(I)=.TRUE.
          ELSE
            IF(CHECKBIN(PAR(IDE(I)),N)) THEN
              EMM(1,I)=PAR(IDE(I))%X
              EMM(2,I)=PAR(IDE(I)+N-1)%X
              NBNEE(I)=N-1
              EQBINE(I)=.FALSE.
            ELSE
              WRITE(MSGFL,430) 2
 430          FORMAT(' Wrong energy bin. ',I1,
     %               '-D differential lum. ignored.')
              NBNEE(1)=0
              NBNEE(2)=0
              GOTO 445
            ENDIF
          ENDIF
 440    CONTINUE
      ELSE
        NBNEE(1)=0
        NBNEE(2)=0
      ENDIF
 445  NLUM=NLUM+1
      KLUM(1,NLUM)=RLK(1)
      KLUM(2,NLUM)=RLK(2)
      NBNWLM(NLUM)=NBNW1
      NBNELM(1,NLUM)=NBNEE(1)
      NBNELM(2,NLUM)=NBNEE(2)
      K=0
      IF(NBNW1.GE.1) THEN
        LBINW(NLUM)=2
        IF(EQBINW) LBINW(NLUM)=1
        IPBINW(NLUM)=IPLUM0
        IPLUM0=IPLUM0+NBNW1+1
      ELSE
        LBINW(NLUM)=0
        IPBINW(NLUM)=1
      ENDIF
      DO 450 I=0,2
        LHEL(I,NLUM)=LHELI(I)
        IPWLUM(I,NLUM)=IPLUM0+K*NBNW1
        IF(LHEL(I,NLUM).NE.0) K=K+NCOMPL(I)
 450  CONTINUE
      IPLUM0=IPLUM0+K*NBNW1
      NBNE1=NBNEE(1)*NBNEE(2)
      DO 455 I=1,2
        IF(NBNE1.GT.0) THEN
          LBINE(I,NLUM)=2
          IF(EQBINE(I)) LBINE(I,NLUM)=1
          IPBINE(I,NLUM)=IPLUM0
          IPLUM0=IPLUM0+NBNEE(I)+1
        ELSE
          LBINE(I,NLUM)=0
          IPBINE(I,NLUM)=1
        ENDIF
 455  CONTINUE
      K=0
      DO 460 I=0,2
        IPELUM(I,NLUM)=IPLUM0+K*NBNE1
        IF(LHEL(I,NLUM).NE.0) K=K+NCOMPL(I)
 460  CONTINUE
      IPLUM0=IPLUM0+K*NBNE1
      IF(IPLUM0-1.GT.MWLUM) GOTO 910
      IF(NBNW1.GE.1) THEN
        WMMLUM(1,NLUM)=WMM(1)
        WMMLUM(2,NLUM)=WMM(2)
        DO 470 J=0,NBNW1
          IF(EQBINW) THEN
            DLUM(IPBINW(NLUM)+J)=WMM(1)+J*(WMM(2)-WMM(1))/NBNW1
          ELSE
            DLUM(IPBINW(NLUM)+J)=PAR(IDW+J)%X
          ENDIF
 470    CONTINUE
      ENDIF
      IF(NBNEE(1).GE.1) THEN
        DO 480 I=1,2
          EMMLUM(1,I,NLUM)=EMM(1,I)
          EMMLUM(2,I,NLUM)=EMM(2,I)
          DO 475 J=0,NBNEE(I)
            IF(EQBINE(I)) THEN
              DLUM(IPBINE(I,NLUM)+J)=
     %            EMM(1,I)+J*(EMM(2,I)-EMM(1,I))/NBNEE(I)
            ELSE
              DLUM(IPBINE(I,NLUM)+J)=PAR(IDE(I)+J)%X
            ENDIF
 475      CONTINUE
 480    CONTINUE
      ENDIF
      DO 520 J=1,2
        PAR1=PAR(IDWX+2*(J-1))%X
        IF(PAR1.EQ.UNDEF%X) THEN
          IF(NLUM.EQ.1) XYLMM(1,J)=0
        ELSE
          IF(NLUM.NE.1) THEN
            IF(XYLMM(1,J).NE.0.AND.XYLMM(1,J).NE.PAR1) GOTO 940
          ENDIF
          XYLMM(1,J)=PAR1
        ENDIF
        PAR1=PAR(IDWX+1+2*(J-1))%X
        IF(PAR1.EQ.UNDEF%X) THEN
          IF(NLUM.EQ.1) XYLMM(2,J)=XYLMM(1,J)
        ELSE
          IF(XYLMM(2,J).NE.0.AND.XYLMM(2,J).NE.PAR1) GOTO 940
          XYLMM(2,J)=PAR1
        ENDIF
        IF(XYLMM(2,J).LT.XYLMM(1,J)) THEN
          PAR1=XYLMM(1,J)
          XYLMM(1,J)=XYLMM(2,J)
          XYLMM(2,J)=PAR1
        ENDIF
 520  CONTINUE
      IPPFLG(NLUM)=LPPINT2
      IF(MSGLVL.GE.1)
     %  CALL RDLUM1(NLUM,KLUM(1,NLUM),FREP,
     %  LBINW(NLUM),NBNWLM(NLUM),WMMLUM(1,NLUM),
     %  LBINE(1,NLUM),NBNELM(1,NLUM),EMMLUM(1,1,NLUM),
     %  XYLMM,LHEL(0,NLUM),
     %  IPPFLG(NLUM),MSGFL)
      IRTN=0
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.RDLUM) FREP parameter incorrectly ',
     %  'specified.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDLUM) Work area MWLUM for luminosity ',
     %   'insufficient.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925) OP(I)
 925  FORMAT(' (SUBR.RDLUM) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.RDLUM) KIND not specified.')
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.RDLUM) Invalid WX and/or WY for luminosity. ',/,
     %  '    Must be common to all luminosities.')
      RETURN
 950  IRTN=1005
      WRITE(MSGFL,955)
 955  FORMAT(' (SUBR.RDLUM) Invalid range of W.')
      RETURN
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.RDLUM) None or both E1 and E2 must be specified.')
      RETURN
 970  IRTN=1007
      WRITE(MSGFL,975)
 975  FORMAT(' (SUBR.RDLUM) Invalid range of E1 or E2.')
      RETURN
 980  IRTN=1008
      WRITE(MSGFL,985)
 985  FORMAT(' (SUBR.RDLUM) Too many LUMINOSITY commands.')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDLUM1(NLUM,KLUM,FREP,LBNW,NBNW,WMM,LBNE,NBNE,EMM,
     %   XYLMM,LHEL,LPPINT2,MSGFL)
      IMPLICIT NONE
      INTEGER NLUM,KLUM(2),LBNW,NBNW,LBNE(2),NBNE(2),LHEL(0:2),
     %   LPPINT2,MSGFL
      REAL*8 FREP,WMM(2),EMM(2,2),XYLMM(2,2)
      CHARACTER*8 KIN(3)/'photon','electron','positron'/
C      REAL*8 PI/3.14159 26535 89793 238D0/
      INTEGER I,J
      CHARACTER*22 TEXT
      CHARACTER*7 EQBN(2)/'  equal','unequal'/
C
      WRITE(MSGFL,100) NLUM,KIN(KLUM(1)),KIN(KLUM(2)),FREP
 100  FORMAT(' +++ ',I2,'-th Luminosity definition +++   ',
     %   A,'- ',A,/,
     %   '  Repetition frequency',T40,1PD10.3,' Hz')
      IF(LBNW.NE.0) THEN
        WRITE(MSGFL,200) EQBN(LBNW),WMM(1),WMM(2),NBNW
 200    FORMAT('  C.M.energy range (',A,' bin)',/,
     %        T30,'(',-9PF9.2,',',-9PF9.2,') GeV',/,
     %    '  Number of bins',T40,I10)
      ENDIF
      IF(LBNE(1).NE.0) THEN
        WRITE(MSGFL,210) (EMM(1,I),EMM(2,I),NBNE(I),EQBN(LBNE(I)),I=1,2)
 210    FORMAT('  E1-E2 differential luminosity',/,
     %      '       E1=(',-9PF9.2,',',-9PF9.2,') GeV',I5,1X,A,' bins',/,
     %      '       E1=(',-9PF9.2,',',-9PF9.2,') GeV',I5,1X,A,' bins')
      ENDIF
      DO 300 J=1,2
        DO 240 I=1,2
          IF(XYLMM(J,I).EQ.0) THEN
            TEXT(1+11*(I-1):11*I)='  Undefined'
          ELSE
            WRITE(TEXT(1+11*(I-1):11*I),220) XYLMM(J,I)
 220        FORMAT(1PD10.3,'m')
          ENDIF
 240    CONTINUE
        IF(J.EQ.1) THEN
          WRITE(MSGFL,260) TEXT(1:22)
 260      FORMAT('  Size of Hor/Ver mesh range',T38,A)
        ELSE
          WRITE(MSGFL,280) TEXT(1:22)
 280      FORMAT('          maximum mesh range',T38,A)
        ENDIF
 300  CONTINUE
      IF(LHEL(1).GE.1) WRITE(MSGFL,310)
 310  FORMAT('  Helicity luminosity computed')
      IF(LHEL(2).GE.1) WRITE(MSGFL,320)
 320  FORMAT('  Luminosity of all polarization computed')
      IF(LPPINT2.NE.0) WRITE(MSGFL,330)
 330  FORMAT('  Particle-particle interaction',T40,'        ON')
      RETURN
      END
C------------------------
      FUNCTION NPARDEF(NPAR,PAR)
	USE FLCHTYP
      IMPLICIT NONE
      INTEGER NPARDEF,NPAR
      TYPE(FLCHTYPE) PAR(*)
      INTEGER I
      NPARDEF=0
      DO 200 I=1,NPAR
        IF(PAR(I).EQ.UNDEF) RETURN
        NPARDEF=I
 200  CONTINUE
      RETURN
      END
C-------------------------
      FUNCTION CHECKBIN(BIN,N)
	USE FLCHTYP
      IMPLICIT NONE
      LOGICAL CHECKBIN
      INTEGER N
      TYPE(FLCHTYPE) BIN(N)
      INTEGER I
      CHECKBIN=.FALSE.
      DO 200 I=2,N
        IF(BIN(I)%X.LE.BIN(I-1)%X) RETURN
 200  CONTINUE
      CHECKBIN=.TRUE.
      RETURN
      END
