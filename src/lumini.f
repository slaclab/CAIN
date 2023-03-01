      SUBROUTINE LUMINI(LL)
C  Initialize luminosity calculation routines.
C   LL=0: Initialize all
C      1: clear luminosity sum only
C
C  KKLUM(K,L):  flag for bin fill of K-th particle kind
C               of L-th beam (either for luminosity or
C               for particle-particle interaction)
C  KVPH(L):     flag to generate virtual photon by
C               electron/positron in L-th beam
	USE LUMCOM
      IMPLICIT NONE
      INTEGER LL
      INCLUDE 'include/lumcom2.h'
      INTEGER M,K,J,I,N,L,KIN,IL,N1
      INTEGER NCOMPL(0:2)/1,4,16/
C
      IF(LL.NE.0) GOTO 260
      NLUM=0
      IPLUM0=1
      FREP=1
      IPDD(0)=0
      NDD(0)=1
      NDD2(0)=1
      DO 200 M=1,MMM
        IPDD(M)=IPDD(M-1)+NDD2(M-1)
        NDD(M)=2*NDD(M-1)
        NDD2(M)=NDD(M)**2
 200  CONTINUE
      DO 240 K=0,NDD(MMM)-1
        KIJDD(K)=0
        J=K
        N=1
 220    IF(MOD(J,2).EQ.1) KIJDD(K)=KIJDD(K)+N
        N=4*N
        J=J/2
        IF(J.GT.0) GOTO 220
 240  CONTINUE
      RETURN
C
 260  DO 280 L=1,2
        DO 270 KIN=1,3
          KKLUM(KIN,L)=0
 270    CONTINUE
 280  CONTINUE
      IF(NLUM.GE.1) THEN
        DO 380 IL=1,NLUM
          DO 310 I=0,20
            LUM(I,IL)=0
 310      CONTINUE
          VLUM(IL)=0
          DO 320 L=1,2
            KKLUM(KLUM(L,IL),L)=1
 320      CONTINUE
          IF(LBINW(IL).GE.1) THEN
            N1=NBNWLM(IL)
            DO 340 I=0,2
              IF(LHEL(I,IL).GE.1) THEN
                DO 330 N=1,N1*NCOMPL(I)
                  DLUM(IPWLUM(I,IL)+N-1)=0
 330            CONTINUE
              ENDIF
 340        CONTINUE
          ENDIF
          IF(LBINE(1,IL).GE.1) THEN
            N1=NBNELM(1,IL)*NBNELM(2,IL)
            DO 360 I=0,2
              IF(LHEL(I,IL).GE.1) THEN
                DO 350 N=1,N1*NCOMPL(I)
                  DLUM(IPELUM(I,IL)+N-1)=0
 350            CONTINUE
              ENDIF
 360        CONTINUE
          ENDIF
 380    CONTINUE
      ENDIF
      KVPH0=0
      IF(LPPINT0.NE.0) THEN
        DO 400 L=1,2
          KVPH(L)=0
 400    CONTINUE
        DO 460 I=1,MPPI
          IF(LPPINT(I).EQ.0) GOTO 460
          GOTO (410,420,430,440), I
C  Breit-Wheeler
 410      KKLUM(1,1)=1
          KKLUM(1,2)=1
          GOTO 460
C  Bethe-Heitler
 420      IF(LREPPI(I).EQ.0) THEN
            KVPH(1)=1
            KVPH(2)=1
            KKLUM(1,1)=1
            KKLUM(1,2)=1
          ELSE
            KVPH(3-LREPPI(I))=1
            KKLUM(1,LREPPI(I))=1
          ENDIF
          GOTO 460
C  Landau-Lifshitz
 430      KVPH(1)=1
          KVPH(2)=1
          GOTO 460
C  Breamsstrahlung
 440      IF(LREPPI(I).EQ.0) THEN
            KVPH(1)=1
            KVPH(2)=1
            KKLUM(1,1)=1
            KKLUM(1,2)=1
          ELSE
            KVPH(3-LREPPI(I))=1
            KKLUM(1,LREPPI(I))=1
          ENDIF
 460    CONTINUE
        IF(KVPH(1).NE.0.OR.KVPH(2).NE.0) KVPH0=1
      ENDIF
      RETURN
      END

