      SUBROUTINE RDLRNZ(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE LASRDATA
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INTEGER MOP
      PARAMETER (MOP=11)
      CHARACTER*16 OP(MOP)/'TXYS','EV','BETAGAMMA','AXIS','ANGLE',
     %   'RIGHT','LEFT','KIND','NOBEAM','EXTERNALFIELD','LASER'/
      INTEGER NFF(MOP)/4,3,1,3,1,0,0,3,0,0,0/,LLOP(MOP)
      INTEGER IDTXYS,IDEV,IDBG,IDAXIS,IDANGL,IDKIND
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/extfcm.h'
      INCLUDE 'include/lasrcm.h'
      INTEGER I,J,NF,NC,K,LR(2),KIN(3),IORDER(3),NORDER,NOBEAM,
     %     LEXT,LLSR
      REAL*8 TXYS0(0:3),EV(3),BG,AXIS(3),ANGLE,SUM
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
        LLOP(I)=0
        IF(OP(I).EQ.'TXYS') THEN
          IDTXYS=ID(I)
          LLOP(I)=1
        ENDIF
        IF(OP(I).EQ.'EV') IDEV=ID(I)
        IF(OP(I).EQ.'BETAGAMMA') THEN
          IDBG=ID(I)
          LLOP(I)=3
        ENDIF
        IF(OP(I).EQ.'AXIS') IDAXIS=ID(I)
        IF(OP(I).EQ.'ANGLE') THEN
          IDANGL=ID(I)
          LLOP(I)=2
        ENDIF
        IF(OP(I).EQ.'KIND') IDKIND=ID(I)
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
      NORDER=0
      NOBEAM=0
      LEXT=0
      LLSR=0
C
      DO 300 J=1,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'RIGHT') LR(1)=1
          IF(OP(I).EQ.'LEFT') LR(2)=1
          IF(OP(I).EQ.'EXTERNALFIELD') LEXT=1
          IF(OP(I).EQ.'LASER') LLSR=1
          IF(OP(I).EQ.'NOBEAM') NOBEAM=1
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
          IF(LLOP(I).NE.0) THEN
            IF(NORDER.NE.0) THEN
              DO 250 K=1,NORDER
                IF(IORDER(K).EQ.LLOP(I)) GOTO 910
 250          CONTINUE
            ENDIF
            NORDER=NORDER+1
            IORDER(NORDER)=LLOP(I)
          ENDIF
        ENDIF
 300  CONTINUE
      IF(NORDER.EQ.0) GOTO 800
      IF(PAR(IDBG).EQ.UNDEF) THEN
        BG=0
      ELSE
        BG=PAR(IDBG)%X
      ENDIF
      DO 310 I=0,3
        IF(PAR(IDTXYS+I).EQ.UNDEF) THEN
          TXYS0(I)=0
        ELSE
          TXYS0(I)=PAR(IDTXYS+I)%X
        ENDIF
 310  CONTINUE
      IF(BG.NE.0) THEN
        SUM=0
        DO 320 I=1,3
          IF(PAR(IDEV+I-1).EQ.UNDEF) THEN
            EV(I)=0
          ELSE
            EV(I)=PAR(IDEV+I-1)%X
            SUM=SUM+EV(I)**2
          ENDIF
 320    CONTINUE
        IF(SUM.EQ.0) GOTO 920
        SUM=1/SQRT(SUM)
        DO 330 I=1,3
          EV(I)=SUM*EV(I)
 330    CONTINUE
      ENDIF
      IF(PAR(IDANGL).EQ.UNDEF) THEN
        ANGLE=0
      ELSE
        ANGLE=PAR(IDANGL)%X
      ENDIF
      IF(ANGLE.NE.0) THEN
        SUM=0
        DO 340 I=1,3
          IF(PAR(IDAXIS+I-1).EQ.UNDEF) THEN
            AXIS(I)=0
          ELSE
            AXIS(I)=PAR(IDAXIS+I-1)%X
            SUM=SUM+AXIS(I)**2
          ENDIF
 340    CONTINUE
        IF(SUM.EQ.0) GOTO 930
        SUM=1/SQRT(SUM)
        DO 350 I=1,3
          AXIS(I)=SUM*AXIS(I)
 350    CONTINUE
      ENDIF
      IF(LEXT.NE.0.AND.LEXTF.EQ.0) THEN
        WRITE(MSGFL,360)
 360    FORMAT(' *** Warning: Lorentz transformation of external ',
     %   'field demanded ',/,'  but no external field defined.')
        LEXT=0
      ENDIF
      IF(LLSR.NE.0.AND.NLSR.EQ.0) THEN
        WRITE(MSGFL,370)
 370    FORMAT(' *** Warning: Lorentz transformation of laser ',
     %   'field demanded ',/,'  but no laser defined.')
        LLSR=0
      ENDIF
      IF(NOBEAM.NE.0.AND.LEXT.EQ.0.AND.LLSR.EQ.0) GOTO 800
      IF(NOBEAM.EQ.0) THEN
        IF(BG.EQ.0.AND.ANGLE.EQ.0.AND.TXYS0(0).EQ.0.AND.
     %  TXYS0(1).EQ.0.AND.TXYS0(2).EQ.0.AND.TXYS0(3).EQ.0) GOTO 800
        DO 380 I=1,3
          IF(PAR(IDKIND+I-1).NE.UNDEF) THEN
            J=NINT(PAR(IDKIND+I-1)%X)
            IF(J.GE.1.AND.J.LE.3) KIN(J)=1
          ENDIF
 380    CONTINUE
        CALL ALTRUE(KIN,3)
        CALL ALTRUE(LR,2)
        CALL LORNTZ(NORDER,IORDER,TXYS0,BG,EV,ANGLE,AXIS,1,0,LR,KIN)
      ENDIF
      IF(LEXT.NE.0) THEN
        CALL EXFLRZ(NORDER,IORDER,TXYS0,BG,EV,ANGLE,AXIS)
      ENDIF
      IF(LLSR.NE.0) THEN
        CALL LSRLRZ(NORDER,IORDER,TXYS0,BG,EV,ANGLE,AXIS)
      ENDIF
      IF(MSGLVL.GE.1) THEN
        CALL RDLRNZ1(NOBEAM,LEXT,LR,KIN,NORDER,IORDER,
     %    TXYS0,ANGLE,AXIS,BG,EV,MSGFL)
      ENDIF
 800  IRTN=0
      RETURN
C
 900  IRTN=1000
      WRITE(MSGFL,905) OP(I)
 905  FORMAT(' (SUBR.RDLRNZ) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.RDLRNZ) TXYS,BETAGAMMA,ANGLE can be specified ',
     %  'at most once for each.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.RDLRNZ) BETAGAMMA specified but EV=0.')
      RETURN
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.RDLRNZ) ANGLE specified but AXIS=0.')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDLRNZ1(NOBEAM,LEXT,LR,KIN,NORDER,IORDER,
     %    TXYS0,ANGLE,AXIS,BG,EV,MSGFL)
      IMPLICIT NONE
      INTEGER NOBEAM,LEXT,NORDER,IORDER(NORDER),LR(2),KIN(3),MSGFL
      REAL*8 TXYS0(0:3),ANGLE,AXIS(3),BG,EV(3)
      INTEGER NC,J,I
      INTEGER ITYPE(3)/1,1,1/
      CHARACTER*256 TEXT
C
      WRITE(MSGFL,200)
 200  FORMAT(' +++ Lorentz Transformation +++')
      DO 280 J=1,NORDER
        IF(IORDER(J).EQ.1) THEN
          WRITE(MSGFL,220) (TXYS0(I),I=0,3)
 220      FORMAT(' Shift of origin (t,x,y,s)',T40,1P4D10.3,' m')
        ELSEIF(IORDER(J).EQ.2) THEN
          WRITE(MSGFL,240) ANGLE,(AXIS(I),I=1,3)
 240      FORMAT(' Rotation by the angle',T70,1PD10.3,' rad',/,
     %         '   around the axis (x,y,s)',T50,1P3D10.3)
        ELSE
          WRITE(MSGFL,260) BG,(EV(I),I=1,3)
 260      FORMAT(' Lorentz boost  beta*gamma=',T70,1PD10.3,/,
     %       '   along the axis (x,y,s)',T50,1P3D10.3)
        ENDIF
 280  CONTINUE
      IF(NOBEAM.EQ.0) THEN
        CALL TYPTTL(LR,KIN,0,ITYPE,TEXT,NC)
        WRITE(MSGFL,400) TEXT(1:NC)
 400    FORMAT(' Following particles have been Lorentz',
     %       ' transformed ',/,5X,A)
      ENDIF
      IF(LEXT.NE.0) THEN
        WRITE(MSGFL,420)
 420    FORMAT(' External field   Lorentz-transformed.')
      ENDIF
      RETURN
      END
