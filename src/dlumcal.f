      SUBROUTINE DLUMCAL(KA,KB,NDIPB,IPB1,IPB2,IADRS,NP,KIND,EP,WGT,
     %  SPIN,LBINW,NBNW,WMIN,WBIN,WBNEDG,
     %  LBINE,NBNE1,NBNE2,EMIN,EBIN,EBNEDG1,EBNEDG2,
     %  LHEL,LUM,DLUMW0,DLUMW1,DLUMW2,DLUME0,DLUME1,DLUME2,
     %  LUMF1,IPPFLG,IRTN)
      IMPLICIT NONE
      INTEGER KA,KB,NDIPB,IPB1(0:NDIPB-1),IPB2(0:NDIPB-1),
     %   NP,IADRS(2,NP),LHEL(0:2),KIND(NP),NBNW,NBNE1,NBNE2,
     %   IPPFLG,LBINW,LBINE(2),IRTN
      REAL*8 EP(0:3,NP),WGT(NP),SPIN(3,NP),WMIN,WBIN,EMIN(2),EBIN(2),
     %   WBNEDG(0:NBNW),EBNEDG1(0:NBNE1),EBNEDG2(0:NBNE2),
     %   LUM(0:20),DLUMW0(NBNW),DLUMW1(NBNW,4),DLUMW2(NBNW,0:3,0:3),
     %   DLUME0(NBNE1,NBNE2),DLUME1(NBNE1,NBNE2,4),
     %   DLUME2(NBNE1,NBNE2,0:3,0:3),LUMF1
      INTEGER K1,K2,IP1,IP2,N1,N2,I,IE1,IE2,J,J1,J2
      REAL*8 W,HEL1,HEL1P,HEL1M,HEL2,HEL2P,HEL2M,WTWT,HELICT,
     %   DLM0,DLM1(4),DLM2(0:3,0:3),S1,S2
      INTEGER FINDBIN
C
      DO 400 K1=KA,KB
        IF(IPB1(K1).EQ.0) GOTO 400
        IP1=IPB1(K1)
 200    N1=IADRS(1,IP1)
        IF(N1.LE.0) GOTO 400
        IF(LHEL(1).NE.0) THEN
          HEL1=HELICT(KIND(N1),EP(0,N1),SPIN(1,N1))
          HEL1P=(1+HEL1)/2
          HEL1M=(1-HEL1)/2
        ENDIF
        IF(LBINE(1).EQ.1) THEN
          IE1=INT((EP(0,N1)-EMIN(1))/EBIN(1)+1D0)
          IF(IE1.LE.0.OR.IE1.GT.NBNE1) IE1=0
        ELSEIF(LBINE(1).EQ.2) THEN
          IE1=FINDBIN(EP(0,N1),EBNEDG1,NBNE1)
        ENDIF
        DO 380 K2=KA,KB
          IF(IPB2(K2).EQ.0) GOTO 380
          IP2=IPB2(K2)
 220      N2=IADRS(1,IP2)
          IF(N2.LE.0) GOTO 380
          IF(LBINE(2).EQ.1) THEN
            IE2=INT((EP(0,N2)-EMIN(2))/EBIN(2)+1D0)
            IF(IE2.LE.0.OR.IE2.GT.NBNE2) IE2=0
          ELSEIF(LBINE(2).EQ.2) THEN
            IE2=FINDBIN(EP(0,N2),EBNEDG2,NBNE2)
          ENDIF
          WTWT=WGT(N1)*WGT(N2)*LUMF1
          DLM0=WTWT
          IF(LHEL(1).NE.0) THEN
            HEL2=HELICT(KIND(N2),EP(0,N2),SPIN(1,N2))
            HEL2P=(1+HEL2)/2
            HEL2M=(1-HEL2)/2
            DLM1(1)=WTWT*HEL1P*HEL2P
            DLM1(2)=WTWT*HEL1M*HEL2P
            DLM1(3)=WTWT*HEL1P*HEL2M
            DLM1(4)=WTWT*HEL1M*HEL2M
            DO 240 J=1,4
              LUM(J)=LUM(J)+DLM1(J)
 240        CONTINUE
          ENDIF
          IF(LHEL(2).NE.0) THEN
            DO 280 J1=0,3
              S1=1
              IF(J1.NE.0) S1=SPIN(J1,N1)
              DO 260 J2=0,3
                S2=1
                IF(J2.NE.0) S2=SPIN(J2,N2)
                DLM2(J1,J2)=WTWT*S1*S2
                J=5+J1+J2*4
                LUM(J)=LUM(J)+DLM2(J1,J2)
 260          CONTINUE
 280        CONTINUE
          ENDIF
          IF(LBINW.NE.0) THEN
            W=(EP(0,N1)+EP(0,N2))**2-(EP(1,N1)+EP(1,N2))**2
     %         +(EP(2,N1)+EP(2,N2))**2-(EP(3,N1)+EP(3,N2))**2
            W=SQRT(MAX(0D0,W))
            IF(LBINW.EQ.1) THEN
              I=INT((W-WMIN)/WBIN+1D0)
            ELSE
              I=FINDBIN(W,WBNEDG,NBNW)
            ENDIF
            IF(I.GE.1.AND.I.LE.NBNW) THEN
              DLUMW0(I)=DLUMW0(I)+DLM0
              IF(LHEL(1).GE.1) THEN
                DO 320 J=1,4
                  DLUMW1(I,J)=DLUMW1(I,J)+DLM1(J)
 320            CONTINUE
              ENDIF
              IF(LHEL(2).GE.1) THEN
                DO 340 J1=0,3
                  DO 330 J2=0,3
                    DLUMW2(I,J1,J2)=DLUMW2(I,J1,J2)+DLM2(J1,J2)
 330              CONTINUE
 340            CONTINUE
              ENDIF
            ENDIF
          ENDIF
          IF(NBNE1.NE.0) THEN
            IF(IE1.NE.0.AND.IE2.NE.0) THEN
              DLUME0(IE1,IE2)=DLUME0(IE1,IE2)+DLM0
              IF(LHEL(1).GE.1) THEN
                DO 350 J=1,4
                  DLUME1(IE1,IE2,J)=DLUME1(IE1,IE2,J)+DLM1(J)
 350            CONTINUE
              ENDIF
              IF(LHEL(2).GE.1) THEN
                DO 370 J1=0,3
                  DO 360 J2=0,3
                    DLUME2(IE1,IE2,J1,J2)
     %                =DLUME2(IE1,IE2,J1,J2)+DLM2(J1,J2)
 360              CONTINUE
 370            CONTINUE
              ENDIF
            ENDIF
          ENDIF
          IF(IPPFLG.NE.0) THEN
            CALL PPINT2(N1,N2,LUMF1,IRTN)
C              call direct (without virtual photon) particle-particle
C              interaction. Not ready
            IF(IRTN.NE.0) RETURN
          ENDIF
          IP2=IADRS(2,IP2)
          IF(IP2.NE.0) GOTO 220
 380    CONTINUE
        IP1=IADRS(2,IP1)
        IF(IP1.NE.0) GOTO 200
 400  CONTINUE
      IRTN=0
      RETURN
      END
      FUNCTION HELICT(KIND,EP,SPIN)
      IMPLICIT NONE
      INTEGER KIND
      REAL*8 HELICT,EP(0:3),SPIN(3)
      REAL*8 P,PS
      IF(KIND.EQ.1) THEN
        HELICT=SPIN(2)
      ELSE
        P=EP(1)**2+EP(2)**2+EP(3)**2
        IF(P.NE.0) THEN
          PS=EP(1)*SPIN(1)+EP(2)*SPIN(2)+EP(3)*SPIN(3)
          HELICT=PS/SQRT(P)
        ELSE
          HELICT=0
        ENDIF
      ENDIF
      RETURN
      END
C----------------------------------------------------
      FUNCTION FINDBIN(X,BIN,NBIN)
      IMPLICIT NONE
      INTEGER FINDBIN,NBIN
      REAL*8 X,BIN(0:NBIN)
      INTEGER I1,I2
      IF(X.LT.BIN(0).OR.X.GT.BIN(NBIN)) THEN
        FINDBIN=0
        RETURN
      ENDIF
      I1=1
      I2=NBIN
      FINDBIN=NBIN/2
 200  IF(X.LT.BIN(FINDBIN)) THEN
        I2=FINDBIN
      ELSE
        I1=FINDBIN+1
      ENDIF
      FINDBIN=(I1+I2)/2
      IF(I1.EQ.I2) RETURN
      GOTO 200
      END
