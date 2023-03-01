      SUBROUTINE RDBBFL(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE BBCOM
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=9)
      CHARACTER*16 OP(MOP)/'BEAMSTRAHLUNG','PAIR',
     %   'NX','NY','R','PSIZE','WX','WXMAX','NMOM'/
      INTEGER NFF(MOP)/0,0,1,1,1,1,2,2,1/
      INTEGER IDNX,IDNY,IDR,IDPSIZ,IDWX,IDWXM,IDNMOM
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
C      INCLUDE 'include/bbcom.h'
      INTEGER J,NF,I,NC,K
C
      IRTN=0
      IF(LN(1,2).EQ.0) GOTO 800
      CALL CMDBLK('BBFIELD',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %   LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) GOTO 800
      J=1
      DO 180 I=1,MOP
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'NX') IDNX=ID(I)
        IF(OP(I).EQ.'NY') IDNY=ID(I)
        IF(OP(I).EQ.'R') IDR=ID(I)
        IF(OP(I).EQ.'PSIZE') IDPSIZ=ID(I)
        IF(OP(I).EQ.'WX') IDWX=ID(I)
        IF(OP(I).EQ.'WXMAX') IDWXM=ID(I)
        IF(OP(I).EQ.'NMOM') IDNMOM=ID(I)
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
C
      DO 300 J=1,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'BEAMSTRAHLUNG'.OR.OP(I).EQ.'PAIR') GOTO 930
        ELSEIF(NFF(I).GE.1) THEN
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
        ENDIF
 300  CONTINUE
      IF(PAR(IDR).EQ.UNDEF.OR.PAR(IDWX).EQ.UNDEF) GOTO 910
      DO 420 I=1,2
        IF(PAR(IDNX+I-1).EQ.UNDEF) THEN
          NXY(I)=32
        ELSE
          NXY(I)=NINT(PAR(IDNX+I-1)%X)
        ENDIF
 420  CONTINUE
      IF(PAR(IDWX+1).EQ.UNDEF) PAR(IDWX+1)=PAR(IDWX)
      IF(PAR(IDWXM).EQ.UNDEF) PAR(IDWXM)=PAR(IDWX)
      IF(PAR(IDWXM+1).EQ.UNDEF) PAR(IDWXM+1)=PAR(IDWXM)
      DO 440 I=1,2
        BBXYM(1,1,I)=PAR(IDWX+I-1)%X
        BBXYM(1,2,I)=BBXYM(1,1,I)/PAR(IDR)%X
        BBXYM(2,1,I)=PAR(IDWXM+I-1)%X
        BBXYM(2,2,I)=BBXYM(2,1,I)/PAR(IDR)%X
        IF(BBXYM(2,1,I).LT.BBXYM(1,1,I)) GOTO 920
 440  CONTINUE
      IF(PAR(IDPSIZ).EQ.UNDEF) THEN
        PSIZE=1
      ELSE
        PSIZE=MAX(0D0,MIN(1D0,PAR(IDPSIZ)%X))
      ENDIF
      IF(PAR(IDNMOM).EQ.UNDEF) THEN
        NMOM=10
      ELSE
        NMOM=NINT(PAR(IDNMOM)%X)
        NMOM=MIN(NMOM,20)
      ENDIF
      BBON=1
      IRTN=0
      IF(MSGLVL.GE.1) 
     %  CALL RDBBFL1(NXY,BBXYM,PSIZE,NMOM,MSGFL)
      RETURN
C
 800  BBON=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) OP(I)
 905  FORMAT(' (SUBR.RDBBFL) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDBBFL) R and WX(1) must be specified.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDBBFL) WXMAX must >= WX.')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.RDBBFL) Sorry! BEAMSTRAHLUNG and Coherent ',
     % ' PAIR moved to CFQED command.')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDBBFL1(NXY,BBXYM,PSIZE,NMOM,MSGFL)
      IMPLICIT NONE
      INTEGER NXY(2),NMOM,MSGFL
      REAL*8 BBXYM(2,2,2),PSIZE
      INTEGER I,L,J
      CHARACTER*5 LLRR(2)/'Right',' Left'/
      CHARACTER*4 MNMX(2)/'Min.','Max.'/
      WRITE(MSGFL,200) (NXY(I),I=1,2)
 200  FORMAT(' +++ Beam-Beam Interaction Parameters +++',/,
     %  T5,'Number of bins  (x,y)',T50,2I10)
      WRITE(MSGFL,220)
     %   ((MNMX(J),LLRR(L),(BBXYM(J,I,L),I=1,2),J=1,2),L=1,2)
 220  FORMAT(T5,A,'Mesh range (x,y) for ',A,'-going beam',
     %     T50,1P2D10.3,' m')
      WRITE(MSGFL,260) PSIZE,NMOM
 260  FORMAT(T5,'Particle size / bin size',T50,0PF10.3,/,
     %  T5,'Order of harmonic expansion for outside mesh',T50,I10)
      RETURN
      END

