      SUBROUTINE ADDTSTP(KIND1,NAM,TXYS1,EP1,SPIN1,IRTN)
	USE FLCHTYP
	USE BEAMCM
	USE READMOD
      IMPLICIT NONE
      INTEGER KIND1,IRTN
      CHARACTER*(*) NAM
      REAL*8 TXYS1(0:3),EP1(0:3),SPIN1(0:3)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/pushcm.h'
      INCLUDE 'include/tstpcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER LR2(2)/1,1/,KIN2(3)/1,1,1/
      REAL*8 BFL1(3,2)/0,0,0,0,0,0/
	CHARACTER(8) LKIND(3)/'photon','electron','positron'/
      INTEGER I
	CHARACTER(4) NAM1
C
	NAM1=NAM
      IF(NTSTP.GE.1) THEN
        DO 710 I=1,NTSTP
          IF(TSTPNM(I).EQ.NAM1) GOTO 940
 710    CONTINUE
      ENDIF
      IF(KIND1.LE.0.OR.KIND1.GE.4) GOTO 950
      IF(EP1(3).EQ.0) GOTO 960
      IF(NTSTP.GE.MTSTP) GOTO 970
      NTSTP=NTSTP+1
      TSTPNM(NTSTP)=NAM1
      CALL ADDONE(0,KIND1,1,NAM1,0,0D0,TXYS1,EP1,SPIN1,0,BFL1,IRTN)
      IF(IRTN.NE.0) GOTO 990
      IF(MSGLVL.GE.1) THEN
        WRITE(MSGFL,740) NAM1,LKIND(KIND1),(TXYS1(I),I=0,3),
     %     (EP1(I),I=0,3),(SPIN1(I),I=1,3)
 740    FORMAT(' +++ Test particle "',A,'" (',A,') added.',/,
     %     '  (t,x,y,s)    =',T23,1P4D13.5,' m',/,
     %     '  (En,Px,Py,Ps)=',T23,1P4D13.5,' eV/c',/,
     %     '  (Sx,Sy,Sx)   =',T23,0P3F13.6)
      ENDIF
      IF(INPUSH.GE.1) THEN
        CALL DRIFT0(1,TIMNOW,NP,NP,LR2,KIN2)
        IF(MSGLVL.GE.1) WRITE(MSGFL,750) (TXYS(I,NP),I=0,3)
 750    FORMAT('  Moved to (t,x,y,s)=',T23,1P4D13.5,' m')
      ENDIF
      IRTN=0
      RETURN
 940  IRTN=1004
      WRITE(MSGFL,945) NAM1
 945  FORMAT(' (SUBR.ADDTSTP) Test particle name "',A,
     %   '" already used.')
      RETURN
 950  IRTN=1005
      WRITE(MSGFL,955) KIND1
 955  FORMAT(' (SUBR.ADDTSTP) Invalid test particle specie. =',I3)
      RETURN
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.ADDTSTP) Ps of test particle = 0.')
      RETURN
 970  IRTN=1007
      WRITE(MSGFL,975)
 975  FORMAT(' (SUBR.ADDTSTP) Too many test particles.')
      RETURN
 990  IRTN=1009
      RETURN
      END
