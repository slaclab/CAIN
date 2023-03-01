      SUBROUTINE RDPPI(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE LUMCOM
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
C      INCLUDE 'include/lumcom.h'
      INCLUDE 'include/cnstcm.h'
C
      INTEGER MOP
      PARAMETER (MOP=11)
      CHARACTER*16 OP(MOP)/'BW','BH','LL','BREMSSTRAHLUNG',
     %   'VIRTUALPHOTON','RIGHT','LEFT','ENHANCE','EMIN','LOCAL',
     %   'FIELDSUPPRESSION'/
      INTEGER NFF(MOP)/-1,-1,-1,-1,-1,0,0,1,1,0,0/
      INTEGER IDENH,IDEMIN,IPVPH
      INTEGER LRIGHT,LLEFT,LOCAL,LFLSUP,LPPI
      INTEGER J,NF,I,NC,K
      REAL*8 ENHPPI1,EMIN1
C
      IRTN=0
      IF(LN(1,2).EQ.0) RETURN
      CALL CMDBLK('PPINT',MOP,OP,0,NFF,MBL,NBL,KOP,KBL,REL,
     %    LNKW,LNBL,LN,LINE,NCHAR,IRTN)
      IF(IRTN.GT.1) GOTO 990
      IF(IRTN.EQ.1.OR.NBL.LE.0) RETURN
C set default
      LRIGHT=0
      LLEFT=0
      LFLSUP=0
      LOCAL=0
      ENHPPI1=0.1D0
      EMIN1=2*MASS(2)
      J=1
      DO 180 I=1,MOP
        ID(I)=J
        J=J+MAX(0,NFF(I))
        IF(OP(I).EQ.'ENHANCE') IDENH=ID(I)
        IF(OP(I).EQ.'EMIN') IDEMIN=ID(I)
        IF(OP(I).EQ.'VIRTUALPHOTON') IPVPH=I
 180  CONTINUE
      DO 190 I=1,J-1
        PAR(I)=UNDEF
 190  CONTINUE
	NGSTRRD=0
C
      LPPI=KBL(1)
      DO 300 J=2,NBL
        I=KBL(J)
        IF(NFF(I).EQ.0) THEN
          IF(OP(I).EQ.'RIGHT') LRIGHT=1
          IF(OP(I).EQ.'LEFT') LLEFT=1
          IF(OP(I).EQ.'LOCAL') LOCAL=1
          IF(OP(I).EQ.'FIELDSUPPRESSION') LFLSUP=1
        ELSEIF(NFF(I).GE.1) THEN
          CALL BLKREC(LNBL(1,1,J),LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
          IF(IRTN.NE.0) GOTO 990
          DO 220 K=1,NFF(I)
	      FFF(K)=UNDEF
 220      CONTINUE
          CALL BLKRHS(TEXT(1:NC),FFF,MFFF,NF,GSTRRD,NGSTRRD,IRTN)
          IF(IRTN.NE.0) GOTO 990
          IF(NF.GT.NFF(I)) GOTO 940
          IF(NF.GE.1) THEN
            DO 260 K=1,NF
              IF(FFF(K).NE.UNDEF) PAR(ID(I)+K-1)=FFF(K)
 260        CONTINUE
          ENDIF
        ENDIF
 300  CONTINUE
C
      IF(LPPI.EQ.IPVPH) THEN
        IF(LOCAL.NE.0.AND.LFLSUP.NE.0) THEN
          LFLSUP=0
          WRITE(MSGFL,320)
 320      FORMAT(' (SUBR.RDPPI) FIELDSUPPRESSION cannot be effective',/,
     %           '    when LOCAL is specified. Ignored.')
        ENDIF
        LCLPPI=LOCAL
        LFLPPI=LFLSUP
        IF(PAR(IDEMIN).NE.UNDEF) EMIN1=PAR(IDEMIN)%X
        EMNPPI=EMIN1
      ELSE
        IF(LPPINT0.EQ.0) THEN
          DO 340 I=1,MPPI
            LPPINT(I)=0
 340      CONTINUE
        ENDIF
        LPPINT0=1
        LPPINT(LPPI)=1
        LREPPI(LPPI)=0
        IF(LRIGHT.NE.0.OR.LLEFT.NE.0) THEN
          IF(LPPI.NE.2.AND.LPPI.NE.4) THEN
            WRITE(MSGFL,360) NAMPPI(LPPI)
 360        FORMAT(' (SUBR.RDPPI) RIGHT/LEFT operand irrelevant for ',
     %           A,'. Ignored.')
          ELSE
            IF(LRIGHT.EQ.0) LREPPI(LPPI)=2
            IF(LLEFT.EQ.0) LREPPI(LPPI)=1
          ENDIF
        ENDIF
        IF(PAR(IDENH).NE.UNDEF) ENHPPI1=PAR(IDENH)%X
        ENHPPI(LPPI)=ENHPPI1
      ENDIF
      IF(MSGLVL.GE.1) CALL RDPPI1(LPPI,MSGFL)
      IRTN=0
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945) OP(I)
 945  FORMAT(' (SUBR.RDPPI) Too many numbers for ',
     %  'operand "',A,'".')
      RETURN
 990  IRTN=1009
      RETURN
      END
      SUBROUTINE RDPPI1(LPPI,MSGFL)
	USE LUMCOM
      IMPLICIT NONE
      INTEGER LPPI,MSGFL
C      INCLUDE 'include/lumcom.h'
      CHARACTER*5 LR(2)/'Right','Left'/
      CHARACTER*12 TEXT
C
      IF(LPPI.LE.MPPI) THEN
        WRITE(MSGFL,200) NAMPPI(LPPI)
 200    FORMAT(' +++ Particle-Particle Interaction +++ ',A)
        IF(LREPPI(LPPI).NE.0) THEN
          TEXT='photons'
          IF(LPPI.EQ.4) TEXT='electrons'
          WRITE(MSGFL,240) LR(LREPPI(LPPI)),TEXT(1:9)
 240      FORMAT('  Events by ',A,'-going ',A,' only')
        ENDIF
        WRITE(MSGFL,260) ENHPPI(LPPI)
 260    FORMAT('  Event rate enhancement factor',T40,1PD10.3,/,
     %       '    1: number of macro pair = number of real pair')
      ELSE
        WRITE(MSGFL,200) 'Virtual photon'
        TEXT='included'
        IF(LCLPPI.NE.0) TEXT='not included'
        WRITE(MSGFL,280) TEXT
 280    FORMAT('  Non-local virtual photon effect',T40,A)
        TEXT='included'
        IF(LFLPPI.EQ.0) TEXT='not included'
        WRITE(MSGFL,290) TEXT
 290    FORMAT('  Suppression effect by strong field',T40,A)
        IF(LPPI.LE.3) WRITE(MSGFL,300) EMNPPI
 300    FORMAT('  Minimum pair-particle energy',T40,1PD11.4,' eV')
      ENDIF
      RETURN
      END
