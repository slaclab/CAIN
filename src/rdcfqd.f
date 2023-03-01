      SUBROUTINE RDCFQD(LN,LINE,NCHAR,IRTN)
C The first operand must be either BEAMSTRAHLUNG or
C PAIRCREATION.
C Enhancement function not ready yet.
	USE FLCHTYP
	USE READMOD
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/cfqedcm.h'
      INTEGER MOP
      PARAMETER (MOP=6)
      CHARACTER*16 OP(MOP)/
     %  'BEAMSTRAHLUNG','PAIRCREATION',
     %  'POLARIZATION','PMAX','WENHANCE','ENHANCEFUNCTION'/
      INTEGER NFF(MOP)/-1,-1,0,1,1,1/,LOP(MOP),
     %  IPPOL,IDPMAX,IDWENH
      INTEGER LBSCO,LENH,NCENHF
      REAL*8 PMAX,WENH
C      EXTERNAL ENHFBS,ENHFCO
C
      INTEGER L,J,NF,I,K,NC
      CHARACTER*256 ENHFUN
C
      IRTN=0
      L=LN(1,2)
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('CFQED',MOP,OP,1,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
C set default
      J=1
      DO 180 I=1,MOP
        LOP(I)=0
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'POLARIZATION') IPPOL=I
        IF(OP(I).EQ.'PMAX') IDPMAX=ID(I)
        IF(OP(I).EQ.'WENHANCE') IDWENH=ID(I)
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
      NCENHF=0
C
      LBSCO=KBL(1)
      DO 260 J=2,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          LOP(I)=1
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          IF(OP(I).EQ.'ENHANCEFUNCTION') THEN
            CALL APSOFF(TEXT(1:NC),NC)
            ENHFUN=TEXT(1:NC)
            NCENHF=NC
          ELSE
	      FFF(1)=UNDEF
            CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
            IF(IRTN.NE.0) GOTO 990
            IF(NF.GE.1) THEN
              DO 240 K=1,NF
                IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 240          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 260  CONTINUE
      IF(PAR(IDPMAX).EQ.UNDEF) THEN
        PMAX=0.1D0
      ELSE
        PMAX=PAR(IDPMAX)%X
        IF(PMAX.LE.0.OR.PMAX.GE.1) GOTO 910
      ENDIF
      LENH=0
      IF(NCENHF.GE.1) LENH=1
      IF(LENH.GE.1) GOTO 940
      IF(PAR(IDWENH).EQ.UNDEF) THEN
        WENH=1
      ELSE
        WENH=PAR(IDWENH)%X
        IF(WENH.LT.0) GOTO 920
      ENDIF
      IF(LBSCO.EQ.1) THEN
        LBMST=1
        LPOLBS=LOP(IPPOL)
        IF(LOP(IPPOL).NE.0) LPOLBS=1
        PMAXBS=PMAX
        WENHBS=WENH
        LENHBS=LENH
        IF(LENH.NE.0) THEN
          ENHBS=ENHFUN
          NCEHBS=NCENHF
        ENDIF
        IF(MSGLVL.GE.1)
     %    CALL RDCFQD1(1,LPOLBS,PMAX,WENH,LENH,ENHFUN,NCENHF,MSGFL)
      ELSE
        LCOHP=1
        LPOLCO=LOP(IPPOL)
        PMAXCO=PMAX
        WENHCO=WENH
        LENHCO=LENH
        IF(LENH.NE.0) THEN
          ENHCO=ENHFUN
          NCEHCO=NCENHF
        ENDIF
        IF(MSGLVL.GE.1)
     %    CALL RDCFQD1(2,LPOLCO,PMAX,WENH,LENH,ENHFUN,NCENHF,MSGFL)
      ENDIF
      IRTN=0
      RETURN
C
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDCFQD) PMAX must be 0<PMAX<1.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925) WENH
 925  FORMAT(' (SUBR.RDCFQD) Enhancement factor WENHANCE=',1PD11.4,
     %  ' must be >0.')
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.RDCFQD) Sorry! Enhancement function for ',
     % 'beamstrahlung and ',/,
     % '    coherent pair creation is not ready yet.')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDCFQD1(L,LPOL,PMAX,WENH,LENH,ENHFUN,NCENHF,MSGFL)
      IMPLICIT NONE
      INTEGER L,LPOL,LENH,NCENHF,MSGFL
      REAL*8 PMAX,WENH
      CHARACTER*(*) ENHFUN
      CHARACTER*13 BSCO(2)/'Beamstrahlung','Coherent Pair'/
C
      WRITE(MSGFL,100) BSCO(L)
 100  FORMAT(' +++ CFQED ',A,' +++')
      IF(LPOL.EQ.0) THEN
        WRITE(MSGFL,110) 'Not included'
 110    FORMAT(T5,'Polarization effect ',T50,A)
      ELSE
        WRITE(MSGFL,110) 'Included'
      ENDIF
      WRITE(MSGFL,120) PMAX
 120  FORMAT(T5,'Maximum event probability per step',
     %        T50,0PF10.6)
      IF(WENH.NE.1D0) WRITE(MSGFL,140) WENH
 140  FORMAT(T5,'Rate enhancement factor',T48,1PD12.5)
      IF(LENH.NE.0) WRITE(MSGFL,180) ENHFUN(1:NCENHF)
 180  FORMAT(T5,'Rate enhancement funtion =',/,T8,A)
      RETURN
      END

  
