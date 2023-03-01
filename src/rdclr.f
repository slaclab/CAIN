      SUBROUTINE RDCLR(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE BBCOM
	USE LUMCOM
	USE LASRDATA
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=18)
      CHARACTER*16 OP(MOP)/'BEAM','LASER','LASERQED','BBFIELD',
     %   'LUMINOSITY','EXTERNALFIELD','CFQED','PPINT',
     %   'INCP','TESTPARTICLE','RIGHT','LEFT','KIND','SELECT',
C              'TESTPARTICLE' must follow 'INCP'
     %   'COMPTON','BREITWHEELER','BEAMSTRAHLUNG',
     %   'COHERENTPAIR'/
      INTEGER NFF(MOP)/-1,-1,-1,-1,-1,-1,-1,-1,
     %   0,0,0,0,3,1,
     %   0,0,0,0/
      INTEGER LOP(MOP)
      INTEGER IDKIND,IPINCP1,IPRGHT,IPCMPT,IPBMST,IPSELECT
      INTEGER KIN(3),LR(2),LSRQ(3),ICLR(3),NCFSEL
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
C      INCLUDE 'include/bbcom.h'
      INCLUDE 'include/cfqedcm.h'
      INCLUDE 'include/lasrcm.h'
      INCLUDE 'include/extfcm.h'
      INCLUDE 'include/lumcom2.h'
      INTEGER J,NF,I,NC,K,II
	CHARACTER(1024) FSELECT
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('CLEAR',MOP,OP,1,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
      J=1
      DO 180 I=1,MOP
        LOP(I)=0
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'KIND') IDKIND=ID(I)
        IF(OP(I).EQ.'INCP') IPINCP1=I
        IF(OP(I).EQ.'RIGHT') IPRGHT=I
        IF(OP(I).EQ.'COMPTON') IPCMPT=I
        IF(OP(I).EQ.'BEAMSTRAHLUNG') IPBMST=I
	  IF(OP(I).EQ.'SELECT') IPSELECT=I
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
	NCFSEL=1
	FSELECT=' '
C
      II=KBL(1)
      DO 300 J=2,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          LOP(I)=1
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          DO 220 K=1,NFF(I)
	      FFF(K)=UNDEF
 220      CONTINUE
          CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
          IF(IRTN.NE.0) GOTO 990
          IF(NF.GT.NFF(I)) GOTO 900
		  IF(OP(I).EQ.'SELECT') THEN
            NCFSEL=NC
            FSELECT=TEXT(1:NC)
	    ELSE
            IF(NF.GE.1) THEN
              DO 240 K=1,NF
                IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 240          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 300  CONTINUE
      GOTO (310,400,450,500,550,600,650,700), II
C BEAM
 310  DO 320 J=1,3
        KIN(J)=0
 320  CONTINUE
      DO 330 I=1,3
        IF(PAR(IDKIND+I-1).NE.UNDEF) THEN
          J=NINT(PAR(IDKIND+I-1)%X)
          IF(J.GE.1.AND.J.LE.3) KIN(J)=1
        ENDIF
 330  CONTINUE
      CALL ALTRUE(KIN,3)
      DO 350 I=1,2
        LR(I)=LOP(IPRGHT+I-1)
 350  CONTINUE
      CALL ALTRUE(LR,2)
      ICLR(2)=LOP(IPINCP1)
      ICLR(3)=LOP(IPINCP1+1)
      ICLR(1)=0
      IF(ICLR(2).EQ.0.AND.ICLR(3).EQ.0) THEN
	  ICLR(1)=1
	  IF(LOP(IPSELECT).NE.0) ICLR(2)=1
C          The above one line added Mar.21.2002
	ENDIF
      CALL BMCLR(LR,KIN,ICLR,0,0,FSELECT(1:NCFSEL),K)
	IF(K.LT.0) GOTO 990
      IF(MSGLVL.GE.1) THEN
        CALL TYPTTL(LR,KIN,0,ICLR,TEXT,NC)
        WRITE(MSGFL,360) K,TEXT(1:NC),FSELECT(1:NCFSEL)
 360    FORMAT(' +++ Following',I6,' particles have been cleared',
     %       ' out +++',/,5X,A,1X,A)
      ENDIF
      GOTO 800
C LASER
 400  IF(NLSR.NE.0.AND.MSGLVL.GE.1) THEN
        WRITE(MSGFL,410) NLSR
 410    FORMAT(' +++',I2,' laser(s) eliminated.')
      ENDIF
	CALL FREELSRDT(0)
      NLSR=0
      GOTO 800
C LASERQED
 450  DO 460 I=1,3
        LSRQ(I)=LOP(IPCMPT+I-1)
 460  CONTINUE
      CALL ALTRUE(LSRQ,2)
      IF(LSRQ(1).NE.0) NPHCP=-1
      IF(LSRQ(2).NE.0) NPHBW=-1
      IF(LSRQ(3).NE.0) NPHBH=-1
      GOTO 800
C BBFIELD
 500  IF(BBON.GE.1.AND.MSGLVL.GE.1) WRITE(MSGFL,510)
 510  FORMAT(' +++ Beam-beam field turned off.')
      BBON=0
      GOTO 800
C LUMINOSITY
 550  IF(NLUM.GE.1.AND.MSGLVL.GE.1) THEN
        WRITE(MSGFL,560)
 560    FORMAT(' +++ Luminosity integral cleared out.')
      ENDIF
      NLUM=0
      GOTO 800
C EXTERNAL FIELD  
 600  IF(LEXTF.GE.1.AND.MSGLVL.GE.1) THEN
        WRITE(MSGFL,610)
 610    FORMAT(' +++ External field cleared out.')
      ENDIF
      LEXTF=0
      GOTO 800
C CFQED
 650  CALL ALTRUE(LOP(IPBMST),2)
      IF(LOP(IPBMST).NE.0) THEN
        IF(LBMST.GE.1.AND.MSGLVL.GE.1) WRITE(MSGFL,660)
 660    FORMAT(' +++ Beamstrahlung turned off.')
        LBMST=0
      ENDIF
      IF(LOP(IPBMST+1).NE.0) THEN
        IF(LCOHP.GE.1.AND.MSGLVL.GE.1) WRITE(MSGFL,670)
 670    FORMAT(' +++ Coherent pair creation turned off.')
        LCOHP=0
      ENDIF
      GOTO 800
C PPINT
 700  LPPINT0=0
      IF(MSGLVL.GE.1) WRITE(MSGFL,710)
 710  FORMAT(' +++ PPINT turned off.')
      GOTO 800
C---
 800  IRTN=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) OP(I)
 905  FORMAT(' (SUBR.RDCLR) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDCLR1(NXY,BBDXY,PSIZE,LBMST,LCOHP,MSGFL)
      IMPLICIT NONE
      INTEGER NXY(2),LBMST,LCOHP,MSGFL
      REAL*8 BBDXY(2,2),PSIZE
      INTEGER I,L
      CHARACTER*5 LLRR(2)/'Right','Left'/
      CHARACTER*3 ONOFF(0:1)/'OFF',' ON'/
      WRITE(MSGFL,200) (NXY(I),I=1,2),
     %   (LLRR(L),(BBDXY(I,L),I=1,2),L=1,2),PSIZE,
     %   ONOFF(LBMST),ONOFF(LCOHP)
 200  FORMAT(' +++ Beam-Beam Interaction Parameters +++',/,
     %  T5,'Number of bins  (x,y)',T50,2I10,/,
     % 2(T5,'Bin size (x,y) for ',A,'-going beam',T50,1P2D10.3,' m',/),
     %  T5,'Particle size / bin size',T50,0PF10.3,/,
     %  T5,'Beamstrahlung',T50,7X,A,/,
     %  T5,'Coherent pair creation',T50,7X,A)
      RETURN
      END

