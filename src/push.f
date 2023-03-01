      SUBROUTINE PUSH(IFIRST,NT1,TT,IRTN)
	USE FLCHTYP
	USE BEAMCM
	USE BBCOM
	USE LUMCOM
	USE LASRDATA
      IMPLICIT NONE
      INTEGER IFIRST,NT1,IRTN
      REAL*8 TT(2)
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/pushcm.h'
      INCLUDE 'include/lumcom2.h'
C      INCLUDE 'include/bbcom.h'
      INCLUDE 'include/cfqedcm.h'
      INCLUDE 'include/lasrcm.h'
      INTEGER I,IXY,IRTN1
      INTEGER LR2(2)/1,1/,KIN2(3)/1,1,1/
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR
C
      IF(IFIRST.LE.0) GOTO 500
      NT=NT1
      TINI=TT(1)
      TFIN=TT(2)
      IF(NT.NE.0) THEN
        DTLUM=(TFIN-TINI)/NT
	  CALL EVAL0('Smesh',FC,ERR)
	  DSLUM=FC%X
        IF((NLUM.GE.1.OR.BBON.GE.1).AND.DSLUM.LE.0) GOTO 900
        IF(NLUM.GE.1) THEN
          IF(FREP.LE.0) GOTO 910
          DO 200 IXY=1,2
            DO 180 I=1,2
              IF(XYLMM(I,IXY).EQ.0) GOTO 920
 180        CONTINUE
 200      CONTINUE
        ENDIF
      ENDIF
      IF(LBMST.NE.0) THEN
        PMMBS=0
        NPMXBS=0
      ENDIF
      IF(LCOHP.NE.0) THEN
        PMMCO=0
        NPMXCO=0
      ENDIF
	IF(NLSR.GE.1) CALL LSRWARN(IRTN1)
      IF(NPHCP.GE.0) PMMCP=0
      IF(NPHBW.GE.0) PMMBW=0
      IT=0
      CALL EVDEFP('Time',TINI,IRTN)
      TIMNOW=TINI
      WGTOUT(1)=0
      WGTOUT(2)=0
      CALL DRIFT0(1,TT(1),1,0,LR2,KIN2)
      IF(NLUM.GE.1.OR.LPPINT0.GE.1) CALL LUMINI(1)
 500  CALL DELLOS
      IF(MSGLVL.GE.2) WRITE(MSGFL,510) IT,TIMNOW
 510  FORMAT(' ++ PUSH',I5,'-th step  T=',1PD12.5)
      IRTN=0
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.PUSH) Smesh has to be SET before PUSH.')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.PUSH) FREP=0 for luminosity calculation.')
      RETURN
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.PUSH) XRANGE/YRANGE operand of LUMINOSITY ',/,
     % '    command has not been specified.')
      RETURN
      END
