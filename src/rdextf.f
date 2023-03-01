      SUBROUTINE RDEXTF(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=4)
      CHARACTER*16 OP(MOP)/'S','E','B','V'/
      INTEGER NFF(MOP)/2,3,3,3/
      INTEGER IDS,IDE,IDB,IDV
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/extfcm.h'
      INTEGER LV,J,NF,I,NC,K
      REAL*8 SUM
C
      IRTN=0
      IF(LN(1,2).EQ.0) GOTO 800
      CALL CMDBLK('EXTERNALFIELD',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) GOTO 800
      J=1
      DO 180 I=1,MOP
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'S') IDS=ID(I)
        IF(OP(I).EQ.'E') IDE=ID(I)
        IF(OP(I).EQ.'B') IDB=ID(I)
        IF(OP(I).EQ.'V') IDV=ID(I)
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
      LV=0
C
      DO 300 J=1,NBL
        I=KBL(J)
        IF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          DO 220 K=1,NFF(I)
	      FFF(K)=UNDEF
 220      CONTINUE
          CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
          IF(IRTN.NE.0) GOTO 990
          IF(NF.GT.NFF(I)) GOTO 900
          IF(NF.GE.1) THEN
            DO 240 K=1,NF
              IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 240        CONTINUE
          ENDIF
          IF(OP(I).EQ.'V') LV=1
        ENDIF
 300  CONTINUE
      DO 310 I=1,2
        IF(PAR(IDS+I-1).EQ.UNDEF) THEN
          EXTFSS(I)=(2*I-3)*1D60
          LEXTFB(I)=0
        ELSE
          EXTFSS(I)=PAR(IDS+I-1)%X
          LEXTFB(I)=1
        ENDIF
 310  CONTINUE
      IF(EXTFSS(1).GE.EXTFSS(2)) GOTO 910
      DO 320 I=1,3
        IF(PAR(IDE+I-1).EQ.UNDEF) THEN
          EXTFEB(I,1)=0
        ELSE
          EXTFEB(I,1)=PAR(IDE+I-1)%X
        ENDIF
        IF(PAR(IDB+I-1).EQ.UNDEF) THEN
          EXTFEB(I,2)=0
        ELSE
          EXTFEB(I,2)=PAR(IDB+I-1)%X
        ENDIF
 320  CONTINUE
      EXTFBV(0)=0
      IF(LV.EQ.0) THEN
        EXTFBV(1)=0
        EXTFBV(2)=0
        EXTFBV(3)=1
      ELSE
        SUM=0
        DO 330 I=1,3
          IF(PAR(IDV+I-1).EQ.UNDEF) GOTO 920
          EXTFBV(I)=PAR(IDV+I-1)%X
          SUM=SUM+ABS(EXTFBV(I))
 330    CONTINUE
        IF(SUM.EQ.0) GOTO 920
      ENDIF
      LEXTF=1
      IRTN=0
      IF(MSGLVL.GE.1) CALL RDEXTF1(LEXTFB,EXTFBV,EXTFSS,EXTFEB,MSGFL)
      RETURN
C
 800  LEXTF=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) OP(I)
 905  FORMAT(' (SUBR.RDEXTF) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDEXTF) Invalid range of S.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDEXTF) Invalid definition of V vector.')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDEXTF1(LEXTFB,EXTFBV,EXTFSS,EXTFEB,MSGFL)
      IMPLICIT NONE
      INTEGER LEXTFB(2),MSGFL
      REAL*8 EXTFBV(0:3),EXTFSS(2),EXTFEB(3,2)
      INTEGER I,J,NC
      CHARACTER*120 TEXT
      CHARACTER*1 C(0:3)/'t','x','y','s'/
C
      IF(LEXTFB(1).LE.0.AND.LEXTFB(2).LE.0) THEN
        TEXT='No boundary'
        NC=11
      ELSE
        TEXT='Boundary:'
        NC=9
        IF(LEXTFB(1).GE.1) THEN
          WRITE(TEXT(NC+1:NC+17),200) EXTFSS(1)
 200      FORMAT(' ',1PD11.4,'(m) <')
          NC=NC+17
        ENDIF
        DO 240 I=0,3
          IF(EXTFBV(I).NE.0) THEN
            IF(EXTFBV(I).EQ.1) THEN
              WRITE(TEXT(NC+1:NC+3),210) C(I)
 210          FORMAT(' +',A)
              NC=NC+3
            ELSE
              WRITE(TEXT(NC+1:NC+17),220) EXTFBV(I),C(I)
 220          FORMAT(' +(',1PD11.4,')*',A)
              NC=NC+17
            ENDIF
          ENDIF
 240    CONTINUE
        IF(LEXTFB(2).GE.1) THEN
          WRITE(TEXT(NC+1:NC+16),260) EXTFSS(2)
 260      FORMAT(' <',1PD11.4,'(m)')
          NC=NC+16
        ENDIF
      ENDIF
      WRITE(MSGFL,300) ((EXTFEB(I,J),I=1,3),J=1,2)
 300  FORMAT(' +++ External Field +++',/,
     %  '  Electric Field (Ex,Ey,Es)',T40,1P3D10.3,' V/m',/,
     %  '  Magnetic Field (Bx,By,Bs)',T40,1P3D10.3,' Tesla')
      WRITE(MSGFL,320) TEXT(1:NC)
 320  FORMAT('  ',A)
      RETURN
      END

