      SUBROUTINE RDDRFT(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=7)
      CHARACTER*16 OP(MOP)/'T','DT','S','RIGHT','LEFT','KIND',
     %     'EXTERNALFIELD'/
      INTEGER NFF(MOP)/1,1,1,0,0,3,0/
      INTEGER IDKIND
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INTEGER I,J,NF,LTS,NC,K,LR(2),KIN(3),LEXT
      REAL*8 TS
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('DRIFT',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %   LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
      DO 160 I=1,2
        LR(I)=0
 160  CONTINUE
      DO 170 I=1,3
        KIN(I)=0
 170  CONTINUE
      J=1
      DO 180 I=1,MOP
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'KIND') IDKIND=ID(I)
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
      LTS=0
      TS=0
      LEXT=0
C
      DO 300 J=1,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'RIGHT') LR(1)=1
          IF(OP(I).EQ.'LEFT') LR(2)=1
          IF(OP(I).EQ.'EXTERNALFIELD') LEXT=1
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
      LTS=0
      DO 320 I=1,3
        IF(PAR(I).NE.UNDEF) THEN
          IF(LTS.NE.0) GOTO 910
          LTS=I
        ENDIF
 320  CONTINUE
      IF(LTS.EQ.0) GOTO 910
      TS=PAR(LTS)%X
      DO 340 I=1,3
        IF(PAR(IDKIND+I-1).NE.UNDEF) THEN
          J=NINT(PAR(IDKIND+I-1)%X)
          IF(J.GE.1.AND.J.LE.3) KIN(J)=1
        ENDIF
 340  CONTINUE
      CALL ALTRUE(KIN,3)
      CALL ALTRUE(LR,2)
      IF(LEXT.EQ.0) THEN
        CALL DRIFT0(LTS,TS,1,0,LR,KIN)
      ELSE
        CALL DRIFTX(LTS,TS,1,0,LR,KIN)
      ENDIF
      CALL TSTPST(MSGFL)
      IRTN=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) OP(I)
 905  FORMAT(' (SUBR.RDDRFT) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915) OP(I)
 915  FORMAT(' (SUBR.RDDRFT) One of T= or DT= or S= must be specified.')
      RETURN
 990  IRTN=1009
      RETURN
      END
