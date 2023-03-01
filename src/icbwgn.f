      SUBROUTINE ICBWGN(K1,K2,H1,H2,ENH,VOL,DT,
     %   NPMAX,NP,P1,P2,MSGFL,IRTN)
C  Breit-Wheeler event generator for incoherent pair
C  Include
C     *  circular polarization of initial photons
C  Does not include
C     *  other polarization
C     *  polarization change when an event is not generated
C
C  Input
C   K1(0:3), K2(0:3)   Energy-momentum of initial photons (eV,eV/c)
C   H1,H2        Circular polarization of initial photons
C   ENH          Event rate enhancement factor
C                If the weights (number of real particles represented
C                by a macro particle) of initial photons and the final
C                pair are WGT1,WGT2,WGT respectively, then
C                  ENH=WGT1*WGT2/WGT
C   VOL          Mesh volume where the initial photons are located
C                  (m**3)
C   DT           Time interval * velocity of light  (m)
C   NPMAX        Maximum number of pairs which can be stored.
C                (defines the dimension of P1 and P2)
C   MSGFL        File unit number for error messages
C  Output
C   NP           Number of pairs created
C   P1(0:3,NPMAX),P2(0:3,NPMAX)    P1(*,I),P2(*,I) (I=1,NP)
C                Energy-momentum of created
C                electron and positrons.
C   IRTN         Return code. If not zero, more than NPMAX pairs
C                have been created.
      IMPLICIT NONE
      INTEGER NPMAX,NP,MSGFL,IRTN
      REAL*8 K1(0:3),K2(0:3),H1,H2,ENH,DT,VOL,
     %     P1(0:3,NPMAX),P2(0:3,NPMAX)
      INTEGER NDIV,IDIV,I,ITR
      REAL*8 K1K2,A,A2,A2I,B,B2,R,SIGTOT,PROB,RND1,RND2,H,
     %   E3(3),E1(3),E2(3),NORM,PT(3),PT1,KK(3),
     %   PHI,SINPHI,COSPHI,SINTH,COSTH1,COSTH2,
     %   C1,C2,C3,D1,D2,W0,W1,SGN,SH,CH,G,PTKK,EXPT,Z,DZ,F,DF
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      REAL*8 PMAX/0.05D0/
      REAL*8 EMASS/0.51099906D6/,SIG0/0.24946731D-28/
C        SIG0=Pi*(Classical electron radius)**2 in meter**2
      REAL*8 PI/3.14159 26535 89793 238D0/
      INTEGER ITMAX/20/
      REAL*8 ASINH
C         ASINH not needed if provided by the system
C
      K1K2=K1(0)*K2(0)-K1(1)*K2(1)-K1(2)*K2(2)-K1(3)*K2(3)
      A2=K1K2/(2*EMASS**2)
      NP=0
      IRTN=0
      IF(A2.LE.1) RETURN
      A=SQRT(A2)
      A2I=1/A2
      B2=A2-1
      B=SQRT(B2)
      R=ASINH(B)
      H=H1*H2
      C1=2*(1+(B2+0.5D0)*A2I**2-H)
      G=R*C1+(-1-A2I+3*H)*B/A
      SIGTOT=SIG0*A2I*G
      PROB=SIGTOT*DT/VOL*(K1K2/(K1(0)*K2(0)))*ENH
      IF(PROB.LE.PMAX) THEN
        NDIV=1
      ELSE
        NDIV=INT(PROB/PMAX+1D0)
        PROB=PROB/NDIV
      ENDIF
      DO 400 IDIV=1,NDIV
        RND1=RANDCAIN()
        IF(RND1.GE.PROB) GOTO 400
        IF(NP.GE.NPMAX) GOTO 900
        NP=NP+1
        IF(NP.EQ.1) THEN
          C1=C1/G
          C2=(1-H)/G
          C3=(2*A2*H-1)*A2I**2/G
          W0=A*EMASS
          W1=W0*(2*W0+K1(0)+K2(0))
          D1=(W0+K2(0))/W1
          D2=(W0+K1(0))/W1
C         E3: unit vector along k1 in CM frame
          DO 210 I=1,3
            E3(I)=D1*K1(I)-D2*K2(I)
 210      CONTINUE
C         E1: unit vector perpendicular to E3
          DO 220 I=1,3
            E1(I)=-E3(1)*E3(I)
 220      CONTINUE
          E1(1)=1+E1(1)
          NORM=E1(1)**2+E1(2)**2+E1(3)**2
          IF(NORM.EQ.0) THEN
            DO 230 I=1,3
              E1(I)=-E3(2)*E3(I)
 230        CONTINUE
            E1(2)=1+E1(2)
            NORM=E1(1)**2+E1(2)**2+E1(3)**2
          ENDIF
          NORM=1/SQRT(NORM)
          DO 240 I=1,3
            E1(I)=NORM*E1(I)
 240      CONTINUE
          DO 250 I=1,3
            E2(I)=E3(I2(I))*E1(I3(I))-E3(I3(I))*E1(I2(I))
 250      CONTINUE
        ENDIF
        RND2=2*RANDCAIN()-1
        IF(RND2.LT.0) THEN
          RND2=-RND2
          SGN=-1
        ELSE
          SGN=1
        ENDIF
C   Solve   C1*t-C2*tanh(t)+C3*cosh(t)*sinh(t)=RND2
        IF(H.GE.0.9.AND.B.GT.2) THEN
          Z=ASINH(SQRT(RND2)*B)
        ELSE
          Z=RND2*R
        ENDIF
        ITR=0
 260    ITR=ITR+1
        SH=SINH(Z)
        CH=SQRT(1+SH**2)
        F=C1*Z-C2*SH/CH+C3*CH*SH-RND2
        DF=C1-C2/CH**2+C3*(CH**2+SH**2)
        DZ=-F/DF
        Z=Z+DZ
        IF(ABS(DZ).GE.1D-8) THEN
          IF(ITR.LT.ITMAX) GOTO 260
          GOTO 920
        ENDIF
        SH=SINH(Z)
        CH=SQRT(1+SH**2)
        EXPT=CH+SH
        IF(SGN.GE.0) THEN
          COSTH1=1/(EXPT*CH)
          COSTH2=2-COSTH1
        ELSE
          COSTH2=1/(EXPT*CH)
          COSTH1=2-COSTH2
        ENDIF
C         COSTH1=1-p/omega*cos(theta)
C         COSTH2=1+p/omega*cos(theta)
        SINTH=MIN(1D0,SQRT(MAX(0D0,B2-SH**2)/(B2*CH**2)))
        PHI=2*PI*RANDCAIN()
        SINPHI=SIN(PHI)
        COSPHI=COS(PHI)
        PTKK=0
        PT1=EMASS*B*SINTH
        DO 320 I=1,3
          PT(I)=PT1*(COSPHI*E1(I)+SINPHI*E2(I))
          KK(I)=K1(I)+K2(I)
          PTKK=PTKK+KK(I)*PT(I)
 320    CONTINUE
        PTKK=PTKK/(2*W1)
        DO 340 I=1,3
          PT(I)=PT(I)+KK(I)*PTKK
 340    CONTINUE
        DO 360 I=1,3
          P1(I,NP)=0.5D0*(COSTH1*K1(I)+COSTH2*K2(I))+PT(I)
          P2(I,NP)=KK(I)-P1(I,NP)
 360    CONTINUE
        P1(0,NP)=SQRT(EMASS**2+P1(1,NP)**2+P1(2,NP)**2+P1(3,NP)**2)
        P2(0,NP)=SQRT(EMASS**2+P2(1,NP)**2+P2(2,NP)**2+P2(3,NP)**2)
 400  CONTINUE
      RETURN
 900  IRTN=1000
      IF(MSGFL.GE.1) THEN
        WRITE(MSGFL,910)
 910    FORMAT(' (SUBR.ICBWGN) Too many pairs created in one step.')
      ENDIF
      RETURN
 920  IRTN=1001
      IF(MSGFL.GE.1) THEN
        WRITE(MSGFL,930) B,H,RND2
 930    FORMAT(' (SUBR.INBWGN) Newton iteration failed. Call the ',
     %    'programmer.',/,
     %    '   B=',1PD10.3,'  H=',0PF7.4,'  RND2=',0PF9.6)
      ENDIF
      RETURN
      END

        
        
        
      
