      SUBROUTINE RDWRIT(KC,LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER KC,LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP,MOP1
      PARAMETER (MOP=28,MOP1=9)
C      1 to MOP1: exclusive
      CHARACTER*13 OP(MOP)/'BEAM','STATISTICS','PARAMETER',
     %   'ARRAY','CPUTIME','LUMINOSITY','BLOPTICS','BLGEOMETRY',
     %   'MAGNETS',
     %   'RIGHT','LEFT','SHORT','LONG','LOST','MATHEMATICA',
     %   'INCP','KIND','FILE','FORMAT','APPEND',
     %   'TXYS','E3','E1','BEAMLINE','SELECT','COMBINE',
     %   'MOMENTUM','expression'/
      INTEGER NFF(MOP)/0,0,0,0,0,0,0,0,0,
     %   0,0,0,0,0,0,  
     %   0,3,1,1,0,
     %   4,3,3,1,1,0,
     %   1,0/
      INTEGER IDKIND,IDTXYS,IDE3,IDE1,IDMOM,
     %   IPRIGHT,IPLEFT,IPSHRT,IPLONG,IPMATH,IPINCP1,IPAPPEND,
     %   IPLOST
      INTEGER IOP(MOP)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INTEGER J,I,II,NF,NC,K,IRTN1,LFC,LC(2),IOS
	TYPE(FLCHTYPE) FC
      INTEGER LR(2),KIN(3),KIN2(2),NKIN2,FILE,LSHORT,LLONG,INCP,
     %        LBMFMT,LLOST,LCOMB
      INTEGER MCFMT
	PARAMETER (MCFMT=1024)
      INTEGER NCFMT
      CHARACTER*(MCFMT) FMT
      INTEGER MXX,NXX
      PARAMETER (MXX=10000)
      INTEGER LXX(2,MXX)
      TYPE(FLCHTYPE) XX(MXX)
	INTEGER, PARAMETER:: MCHBUF=1024*128
	CHARACTER(MCHBUF), POINTER:: CHBUF
	INTEGER NCHBUF,ISTAT
	REAL*8 X1
      CHARACTER(512) FILENAME
	CHARACTER(80) ERR
	INTEGER NCFN
      CHARACTER*5 CMD1
	INTEGER NCBLNAM
	CHARACTER(32) BLNAM
	REAL(8) E3(3),E1(3),TXYS0(0:3),P00
	CHARACTER(1024) FSELECT
	INTEGER NCFSEL
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      IF(KC.EQ.1) CMD1='PRINT'
      IF(KC.EQ.2) CMD1='WRITE'
      CALL CMDBLK(CMD1,MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,LNKW,LNBL,
     %    LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
	ALLOCATE(CHBUF, STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 1200
      IF(KC.EQ.1) THEN
        FILE=MSGFL
      ELSE
        FILE=OUTFL
      ENDIF
      DO I=1,MOP
        IOP(I)=0
      ENDDO
      NKIN2=0
      DO I=1,3
        KIN(I)=0
      ENDDO
      NXX=0
      FMT=' '
      FILENAME=' '
	NCBLNAM=0
	NCFSEL=1
	FSELECT=' '
	NCHBUF=0
	LCOMB=0
      J=1
      DO 180 I=1,MOP
        ID(I)=J
        IF(OP(I).EQ.'KIND') IDKIND=ID(I)
	  IF(OP(I).EQ.'TXYS') IDTXYS=ID(I)
	  IF(OP(I).EQ.'E3') IDE3=ID(I)
	  IF(OP(I).EQ.'E1') IDE1=ID(I)
	  IF(OP(I).EQ.'MOMENTUM') IDMOM=ID(I)
	  IF(OP(I).EQ.'RIGHT') IPRIGHT=I
	  IF(OP(I).EQ.'LEFT') IPLEFT=I
        IF(OP(I).EQ.'SHORT') IPSHRT=I
        IF(OP(I).EQ.'LONG') IPLONG=I
	  IF(OP(I).EQ.'LOST') IPLOST=I
        IF(OP(I).EQ.'MATHEMATICA') IPMATH=I
        IF(OP(I).EQ.'INCP') IPINCP1=I
        IF(OP(I).EQ.'APPEND') IPAPPEND=I
        J=J+MAX(0,NFF(I))
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
C
      DO 400 J=1,NBL
        I=KBL(J)
	  IOP(I)=1
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'expression') THEN
            CALL RDWRDO(LNKW(1,1,J),LINE,NCHAR,CMD1,MXX,NXX,LXX,
     %         XX,CHBUF,NCHBUF,MSGFL,IRTN)
            IF(IRTN.NE.0) GOTO 990
	    ELSEIF(OP(I).EQ.'COMBINE') THEN
	      LCOMB=1
          ENDIF
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          IF(OP(I).EQ.'FORMAT') THEN
            NCFMT=NC
            FMT=TEXT(1:NC)
	    ELSEIF(OP(I).EQ.'BEAMLINE') THEN
	      CALL APSOFF(TEXT(1:NC),NC)
            NCBLNAM=NC
            BLNAM=TEXT(1:NC)
	    ELSEIF(OP(I).EQ.'SELECT') THEN
            NCFSEL=NC
            FSELECT=TEXT(1:NC)
          ELSE
            IF(OP(I).EQ.'FILE') THEN
	        CALL EVAL0(TEXT(1:NC),FC,ERR)
	        IF(ERR.NE.' ') GOTO 960
	        IF(FC%L.EQ.1) THEN
	          FILE=NINT(FC%X)
	        ELSE
	          FILENAME=GSTR2(EVALLAST)(FC%C(1):FC%C(2))
	          NCFN=FC%C(2)-FC%C(1)+1
              ENDIF
	        GOTO 400
            ENDIF
            DO 360 K=1,NFF(I)
	        FFF(K)=UNDEF
 360        CONTINUE
            CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
            IF(IRTN.NE.0) GOTO 990
            IF(NF.GT.NFF(I)) GOTO 910
            IF(NF.GE.1) THEN
              DO 380 K=1,NF
                IF(FFF(K).NE.UNDEF) THEN
                  PAR(ID(I)+K-1)=FFF(K)
                  IF(ID(I).EQ.IDKIND) THEN
                    NKIN2=NKIN2+1
                    IF(NKIN2.LE.2) KIN2(NKIN2)=NINT(FFF(K)%X)
                  ENDIF
                ENDIF
 380          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 400  CONTINUE
      II=0
      DO 420 I=1,MOP1
        IF(IOP(I).NE.0) THEN
          IF(II.NE.0) GOTO 920
          II=I
        ENDIF
 420  CONTINUE
      IF(II.EQ.0) II=3
	IF(FILENAME.NE.' ') THEN
        FILE=98
        CALL OPENFL(FILE,FILENAME(1:NCFN),'UNKNOWN',IOP(IPAPPEND),
     %        NC,IRTN1)
        IF(IRTN1.NE.0) GOTO 910
      ENDIF
      GOTO (500,500,520,560,600,640,700,740,750), II
C- beam & statistics
 500  LR(1)=IOP(IPRIGHT)
      LR(2)=IOP(IPLEFT)
      CALL ALTRUE(LR,2)
      DO 510 I=1,3
        IF(PAR(IDKIND+I-1).NE.UNDEF) THEN
          K=NINT(PAR(IDKIND+I-1)%X)
          IF(K.GE.1.AND.K.LE.3) KIN(K)=1
        ENDIF
 510  CONTINUE
      CALL ALTRUE(KIN,3)
      LSHORT=IOP(IPSHRT)
      LLONG=IOP(IPLONG)
	LLOST=IOP(IPLOST)
      INCP=IOP(IPINCP1)
      IF(II.EQ.1) THEN
        LBMFMT=0
        IF(LSHORT.NE.0) LBMFMT=1
        IF(IOP(IPMATH).NE.0) LBMFMT=2
        CALL WRBEAM(FILE,LR,KIN,LLOST,FSELECT(1:NCFSEL),LBMFMT,
     %         INCP,IRTN)
        IF(IRTN.NE.0.AND.KC.EQ.2) GOTO 1000
      ELSEIF(II.EQ.2) THEN
        IF(LLONG.NE.0) THEN
          J=2
        ELSEIF(LSHORT.NE.0) THEN
          J=0
        ELSE
          J=1
        ENDIF
        CALL BMSTAT(1,0,J,INCP,LLOST,FILE)
      ENDIF
      GOTO 800
C- parameter
 520  IF(NXX.EQ.0.AND.FMT.EQ.' ') GOTO 800
      IF(FMT.EQ.' ') THEN
        DO J=1,NXX
	    IF(XX(J)%L.EQ.1) THEN
            WRITE(FILE,530) CHBUF(LXX(1,J):LXX(2,J)),XX(J)%X
 530        FORMAT(' ',A,'=',1PD15.8)
          ELSEIF(XX(J)%L.EQ.2) THEN
	      IF(CHBUF(LXX(1,J):LXX(1,J)).EQ.CHBUF(LXX(2,J):LXX(2,J))
     %       .AND.(CHBUF(LXX(1,J):LXX(1,J)).EQ.'"'.OR.
     %             CHBUF(LXX(1,J):LXX(1,J)).EQ."'")
     %			 .AND.CHBUF(LXX(1,J)+1:LXX(2,J)-1)
     %        .EQ.CHBUF(XX(J)%C(1):XX(J)%C(2))) THEN
	        WRITE(FILE,'(A)') CHBUF(XX(J)%C(1):XX(J)%C(2))
C                Literal constant printed as it is
	      ELSE
	        WRITE(FILE,535) CHBUF(LXX(1,J):LXX(2,J)),
     %                 CHBUF(XX(J)%C(1):XX(J)%C(2))
 535          FORMAT(' ',A,'=','"',A,'"')
            ENDIF
	    ENDIF
        ENDDO
      ELSE
C       check format
C       The possible forms are   FORMAT=(.....)
C            FORMAT=(.....)   or  FORMAT= charac.expr.
C       but \, e.g.,  FORMAT=('abc') can be understand in both ways.
C       Here the first form is assumed at first.
C       (1) If enclosed by () ---> take as is
C       (2) If not, evaluate it as an expression
C       (3) If floating expression or an error expression ---> error
C       (4) If character expression,
C              if not enclosed by (), enclose it.
C       This check is not perfect because the grammer is not selfconsistent
	  CALL FMTCHECK(FMT(1:NCFMT),IRTN1)
        IF(IRTN1.EQ.0) THEN
	    CALL EVAL0(FMT(1:NCFMT),FC,ERR)
	    IF(FC%L.EQ.2) THEN
	      FMT=GSTR2(EVALLAST)(FC%C(1):FC%C(2))
	      NCFMT=FC%C(2)-FC%C(1)+1
	      CALL FMTCHECK(FMT(1:NCFMT),IRTN1)
	      IF(IRTN1.EQ.0) THEN
	        FMT='('//FMT(1:NCFMT)//')'
	        NCFMT=NCFMT+2
	      ELSEIF(IRTN1.NE.1) THEN
	        GOTO 980
	      ENDIF
	    ELSE
	      GOTO 980
	    ENDIF
	  ELSEIF(IRTN1.NE.1) THEN
	    GOTO 980
	  ENDIF
        IF(NXX.EQ.0) THEN
          WRITE(FILE,FMT(1:NCFMT),IOSTAT=IOS)
        ELSE
	    CALL WRFMTFC(FILE,XX,NXX,CHBUF,FMT(1:NCFMT),ERR)
	    IF(ERR.NE.' ') THEN
	      WRITE(MSGFL,'(A)')
     %           ' (SUBR.RDWRIT) Format error in command '//CMD1
	      WRITE(MSGFL,'(A)') ERR
			  GOTO 990
	    ENDIF
        ENDIF
      ENDIF
      GOTO 800
C- array
 560  LLONG=IOP(IPLONG)
	CALL EVPRARRAY(LLONG,FILE)
	GOTO 800
C- cputime
 600  LLONG=IOP(IPLONG)
      IF(LLONG.NE.0) THEN
        CALL CPUTIM('    ',3)
      ELSE
        CALL CPUTM2('Total',X1)
        WRITE(FILE,610) X1
 610    FORMAT(' +++ Cputime so far:',0PF12.6,' sec')
      ENDIF
      GOTO 800
C- luminosity
 640  IF(NKIN2.NE.2) GOTO 930
      CALL LUMPRT(FILE,KIN2,IRTN1)
      IF(IRTN1.NE.0) GOTO 990
      GOTO 800
C- beamline optics
700   IF(NCBLNAM.LE.0) GOTO 940
	CALL PRBLOPTICS(BLNAM(1:NCBLNAM),FILE,IRTN1)
	IF(IRTN1.NE.0) GOTO 990
      GOTO 800
C- beamline geometry
740   IF(NCBLNAM.LE.0) GOTO 940
	DO I=0,3
	  TXYS0(I)=0
	  IF(PAR(IDTXYS+I).NE.UNDEF) TXYS0(I)=PAR(IDTXYS+I)%X
	ENDDO
	E3=0
	E1=0
	E3(3)=1
	E1(1)=1
	DO I=1,3
	  IF(PAR(IDE3+I-1).NE.UNDEF) E3(I)=PAR(IDE3+I-1)%X
	  IF(PAR(IDE1+I-1).NE.UNDEF) E1(I)=PAR(IDE1+I-1)%X
	ENDDO
	CALL PRBLGEOM(BLNAM(1:NCBLNAM),TXYS0(1),E1,E3,FILE,IRTN1)
	IF(IRTN1.NE.0) GOTO 990
      GOTO 800
C- magnets
750   IF(PAR(IDMOM).EQ.UNDEF) THEN
        P00=0
	ELSE
		P00=PAR(IDMOM)%X
	ENDIF
	IF(NCBLNAM.LE.0) THEN
        CALL PRMAGLIST(P00,FILE)
	ELSE
	  CALL PRBLMAG(BLNAM(1:NCBLNAM),P00,LCOMB,FILE,MSGFL,IRTN1)
	  IF(IRTN1.NE.0) GOTO 990
	ENDIF
	GOTO 800
C---
 800  IF(FILENAME.NE.' ') CLOSE(FILE,IOSTAT=IOS)
	IRTN=0
      GOTO 1100
C---
C 900  IRTN=1000
C      WRITE(MSGFL,905) TEXT(1:NC)
C 905  FORMAT(' (SUBR.RDWRIT) Invalid operand "',A,'".')
C      GOTO 1000
 910  IRTN=1001
      WRITE(MSGFL,915) OP(I)
 915  FORMAT(' (SUBR.RDWRIT) Too many numbers for ',
     %  'operand "',A,'".')
      GOTO 1000
 920  IRTN=1002
      WRITE(MSGFL,925) (OP(I),I=1,MOP1)
 925  FORMAT(' (SUBR.RDWRIT) One of the following ',
     %  'must be specified.',/,2X,10(1X,A))
      GOTO 1000
 930  IRTN=1003
      WRITE(MSGFL,935) CMD1
 935  FORMAT(' (SUBR.RDWRIT) Left/right going beams must be ',
     %  'specified in ',A,' LUMINOSITY')
      GOTO 1000
 940  IRTN=1004
      WRITE(MSGFL,945) CMD1
 945  FORMAT(' (SUBR.RDWRIT) ',A,' BLOPTICS. Invalid beamline name.')
      GOTO 1000
 950  IRTN=1005
      WRITE(MSGFL,955) CMD1
 955  FORMAT(' (SUBR.RDWRIT) ',A,' BLOPTICS. Invalid Twiss parameter.')
      GOTO 1000
 960  IRTN=1006
      WRITE(MSGFL,965) CMD1,ERR
 965  FORMAT(' (SUBR.RDWRIT) Invalid file name in ',A,' command.',/,A)
      GOTO 1000
 970  IRTN=1007
      WRITE(MSGFL,975) FILENAME(1:NCFN),CMD1
 975  FORMAT(' (SUBR.RDWRIT) File "',A,'" open error in ',A,' command.')
      GOTO 1000
 980  IRTN=1008
      WRITE(MSGFL,985) FMT(1:NCFMT)
 985  FORMAT(' (SUBR.RDWRIT) Invalid format string ',A)
      GOTO 1000
 990  IRTN=1009
      GOTO 1000
 1000 IF(KC.EQ.1) IRTN=0
 1100 DEALLOCATE(CHBUF,STAT=ISTAT)
      RETURN
1200  IRTN=2000
      WRITE(MSGFL,1205)
1205  FORMAT(' (SUBR.RDWRIT) Memory dynamic allocation failed.')
	RETURN
      END
