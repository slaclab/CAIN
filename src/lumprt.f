      SUBROUTINE LUMPRT(FILE,KIN,IRTN)
	USE LUMCOM
      IMPLICIT NONE
      INTEGER FILE,KIN(2),IRTN
      INCLUDE 'include/ctrlcm.h'
      INCLUDE 'include/lumcom2.h'
      INTEGER IL,IL1,I,J,J1,J2,L,I1,I2,NC,K,II,NB2
      INTEGER KKIN(2)
      INTEGER NCOMPL(0:2)/1,4,16/,IP0(0:2)/0,1,5/,IP(0:2)
      REAL*8 E1,E2,DW,DE(2)
      CHARACTER*2 LKIN(3)/'g ','e-','e+'/
      CHARACTER*2 LH(4)/'++','-+','+-','--'/
      CHARACTER*3 LS(0:3,2)/
     %  '   ','Xi1','Xi2','Xi3','   ','Sx ','Sy ','Ss '/
      INTEGER MC,MC0
      PARAMETER (MC0=13,MC=21*MC0)
      CHARACTER*(MC) TTL,TXT
      CHARACTER*13 EQBN(2)/'  equal space','unequal space'/
C
      CALL CPUTIM('LUMPRT',1)
      DO 200 I=1,2
        IF(KIN(I).EQ.1) THEN
          KKIN(I)=1
        ELSE
          KKIN(I)=2
        ENDIF
 200  CONTINUE
      IF(NLUM.LE.0) GOTO 910
      DO 210 IL1=1,NLUM
        IL=IL1
        IF(KLUM(1,IL).EQ.KIN(1).AND.KLUM(2,IL).EQ.KIN(2)) GOTO 220
 210  CONTINUE
      GOTO 910
 220  IP(0)=1
      IP(1)=1
      IP(2)=1
      TTL=' Unpolarized '
      NC=MC0
      IF(LHEL(1,IL).GE.1) THEN
        IP(1)=NC+1
        WRITE(TTL(NC+1:MC),230) (LH(J),J=1,4)
 230    FORMAT(4('  Helicity ',A2))
        NC=NC+MC0*4
      ENDIF
      IF(LHEL(2,IL).GE.1) THEN
        IP(2)=NC+1
        WRITE(TTL(NC+1:MC),240)
     %     ((LS(J1,KKIN(1)),LS(J2,KKIN(2)),J1=0,3),J2=0,3)
 240    FORMAT(16('   (',A3,',',A3,') '))
        NC=NC+MC0*16
      ENDIF
C-- Total luminosity
      WRITE(FILE,310) (LKIN(KIN(I)),I=1,2),TTL(1:NC)
 310  FORMAT(' +++++ Luminosity (',2A,')   /cm^2/sec',/,18X,A)
      DO 330 L=0,2
        IF(LHEL(L,IL).GE.1) THEN
          WRITE(TXT(IP(L):IP(L)+MC0*NCOMPL(L)-1),320)
C     %       (LUM(IP0(L)+J-1,IL)/1D4,J=1,NCOMPL(L))
C                /1D4 not needed. Corrected on Aug.13.2001
     %       (LUM(IP0(L)+J-1,IL),J=1,NCOMPL(L))
 320      FORMAT(16(1PD13.5,:))
        ENDIF
 330  CONTINUE
      WRITE(FILE,340) TXT(1:NC)
 340  FORMAT(' Total Luminosity ',A)
C-- dL/dW
      IF(LBINW(IL).LE.0) GOTO 500
      WRITE(FILE,350) (LKIN(KIN(K)),K=1,2),(WMMLUM(I,IL),I=1,2),
     %   NBNWLM(IL),EQBN(LBINW(IL))
 350  FORMAT(' +++++ dL/dW      (',2A,')   /cm^2/sec/bin',/,
     %    '  C.M.Energy range (eV) [',1P2D13.5,']',
     %    '  Number of bins',I4,1X,A)
      WRITE(FILE,360) TTL(1:NC)
 360  FORMAT(' W(eV)(bin center)',A)
      DW=(WMMLUM(2,IL)-WMMLUM(1,IL))/NBNWLM(IL)
      DO 400 I=1,NBNWLM(IL)
        DO 380 L=0,2
          IF(LHEL(L,IL).GE.1) THEN
            WRITE(TXT(IP(L):IP(L)+MC0*NCOMPL(L)-1),320)
C     %        (DLUM(IPWLUM(L,IL)+NBNWLM(IL)*(J-1)+I-1)/1D4,
     %        (DLUM(IPWLUM(L,IL)+NBNWLM(IL)*(J-1)+I-1),
     %        J=1,NCOMPL(L))
          ENDIF
 380    CONTINUE
        WRITE(FILE,390) WMMLUM(1,IL)+DW*(I-0.5D0),TXT(1:NC)
 390    FORMAT(3X,1PD13.5,2X,A)
 400  CONTINUE
C-- dL/dEdE
 500  IF(LBINE(1,IL).LE.0) GOTO 600
      WRITE(FILE,520) (LKIN(KIN(K)),K=1,2),
     %  (K,(EMMLUM(I,K,IL),I=1,2),NBNELM(K,IL),EQBN(LBINE(K,IL)),
     %   K=1,2)
 520  FORMAT(' +++++ dL/dE1dE2  (',2A,')   /cm^2/sec/bin',
     %  2(/,'  beam',I1,' Energy range (eV) [',1P2D13.5,']',
     %    '  Number of bins',I4,1X,A))
      DO 530 K=1,2
        DE(K)=(EMMLUM(2,K,IL)-EMMLUM(1,K,IL))/NBNELM(K,IL)
 530  CONTINUE
      WRITE(FILE,540) TTL(1:NC)
 540  FORMAT('    E1(eV)       E2(eV)   ',A)
      NB2=NBNELM(1,IL)*NBNELM(2,IL)
      DO 580 I2=1,NBNELM(2,IL)
        E2=EMMLUM(1,2,L)+DE(2)*(I2-0.5D0)
        DO 570 I1=1,NBNELM(1,IL)
          E1=EMMLUM(1,1,L)+DE(1)*(I1-0.5D0)
          II=I1-1+(I2-1)*NBNELM(1,IL)
          DO 550 L=0,2
            IF(LHEL(L,IL).GE.1) THEN
              WRITE(TXT(IP(L):IP(L)+MC0*NCOMPL(L)-1),320)
C     %        (DLUM(IPELUM(L,IL)+NB2*(J-1)+II)/1D4,
     %        (DLUM(IPELUM(L,IL)+NB2*(J-1)+II),
     %         J=1,NCOMPL(L))
            ENDIF
 550      CONTINUE
          WRITE(FILE,560) E1,E2,TXT(1:NC)
 560      FORMAT(1P2D13.5,A)
 570    CONTINUE
 580  CONTINUE
C--
 600  IRTN=0
      GOTO 1000
 910  IRTN=1001
      IF(MSGLVL.GE.0) WRITE(MSGFL,915) (LKIN(KIN(I)),I=1,2)
 915  FORMAT(' (SUBR.LUMPRT) Luminosity (',2A,') is undefined.')
      GOTO 1000
 1000 CALL CPUTIM('LUMPRT',2)
      RETURN
      END
