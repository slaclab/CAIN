      SUBROUTINE LUMPLT(FILE,NEW,KIN,FV,PERHVAR,LOGV,VMM,UNDEF,
     %   TTL1,TTL2,TTL3,LM,ICOLOR,IRTN)
	USE LUMCOM
      IMPLICIT NONE
      CHARACTER*(*) FV,TTL1,TTL2,TTL3
      INTEGER FILE,NEW,KIN(2),PERHVAR,LOGV,LM,ICOLOR,IRTN
      REAL*8 VMM(2),UNDEF
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/lumcom2.h'
      INTEGER IL,K,NDHEL,IHEL
      CHARACTER*8 LKIN(3)/'photon','electron','positron'/
C
      IF(NLUM.LE.0) GOTO 900
      DO 200 IL=1,NLUM
        IF(KLUM(1,IL).EQ.KIN(1).AND.KLUM(2,IL).EQ.KIN(2)) GOTO 220
 200  CONTINUE
      GOTO 900
 220  IF(LBINW(IL).EQ.0) GOTO 910
      IF(FV.EQ.' ') THEN
        NDHEL=0
        IF(LHEL(1,IL).GE.1) NDHEL=4
        DO 180 IHEL=0,NDHEL
          CALL LUMPLT0(FILE,LOGV,IHEL,KIN,LUM(IHEL,IL),
     %     VLUM(IL),NBNWLM(IL),DLUM(IPBINW(IL)),
     %     DLUM(IPWLUM(0,IL)+IHEL*NBNWLM(IL)),PERHVAR,ICOLOR,IRTN)
          IF(IRTN.NE.0) GOTO 920
 180    CONTINUE
      ELSE
        CALL LUMPLT1(FILE,NEW,KIN,FV,LHEL(0,IL),LUM(0,IL),
     %     NBNWLM(IL),DLUM(IPBINW(IL)),LOGV,VMM,
     %     DLUM(IPWLUM(0,IL)),DLUM(IPWLUM(1,IL)),DLUM(IPWLUM(2,IL)),
     %     PERHVAR,TTL1,TTL2,TTL3,LM,ICOLOR,IRTN)
        IF(IRTN.EQ.1003) GOTO 920
      ENDIF
      RETURN
 900  IRTN=1000
      IF(MSGLVL.GE.0) WRITE(MSGFL,905) KIN(1),KIN(2)
 905  FORMAT(' (SUBR.LUMPLT) Luminosity KIND=(',I1,',',I1,') ',
     %    'undefined.')
      RETURN
 910  IRTN=1001
      IF(MSGLVL.GE.0) WRITE(MSGFL,915) KIN(1),KIN(2)
 915  FORMAT(' (SUBR.LUMPLT) Differential Luminosity dL/dW ',
     %  ' for KIND=(',I1,',',I1,') is undefined.')
      RETURN
 920  IRTN=0
      WRITE(MSGFL,925) (LKIN(KLUM(K,IL)),K=1,2),
     %     (WMMLUM(K,IL),K=1,2)
 925  FORMAT(' (SUBR.LUMPLT) Warning. (',A,',',A,') luminosity',
     %       ' bin empty ',/,'   Bin range ',1PD10.3,'< W <',1PD10.3,
     %       ' eV')
      RETURN
      END
