CC******************************************************************CC
C*                                                                  *C
C*==================================================================*C
C* Subroutine LNCPGN(P1,W,N,PE,UV1,UV2,PP,PD,DT,PMAX,LPCH,LPE,LPP,  *C
C*      L,P3,K3,PE3,PP3,MSG,PROB,IRTN)                              *C
C*==================================================================*C
C*                                                                  *C
C* (Purpose)                                                        *C
C*    Compton Event Generator Including Polarization Effects        *C
C*                                                                  *C
C* (Inputs)                                                         *C
C*      P1 (*): initial electron momentum  (eV/c)                   *C
C*      W     : initial photon energy  (eV)                         *C
C*      N  (*): unit vector along initial photon momentum           *C
C*      PE (*): polarization vector of initial electron             *C
C*      UV1(*): unit vector associated with initial photon          *C
C*      UV2(*): unit vector associated with initial photon          *C
C*      PP (*): Stokes parameter of initial photon                  *C
C*      PD    : power density of the laser beam  (Watt/m**2)        *C
C*      DT    : time interval times velocity of light (m)           *C
C*      PMAX  : set IRTN when the event probability exceeds PMAX    *C
C*      LPCH  : flag to take into account the change of polarization*C
C*              when event is not generated.                        *C
C*      LPE   : flag to compute final electron polarization         *C
C*      LPG   : flag to compute final photon polarization           *C
C*      MSG   : flag to print debugging message                     *C
C*                                                                  *C
C* (Outputs)                                                        *C
C*      L     : L=0  no  photon                                     *C
C*            : L=1  1   photon                                     *C
C*      P3 (*): final electron momentum    (eV/c)                   *C
C*      K3 (*): final photon momentum      (eV/c)                   *C
C*      PE3(*): polarization vector of final electron               *C
C*      PP3(*): Stokes parameter of final photon                    *C
C*      PROB  : Approximate event probability                       *C
C*      IRTN  : return code.                                        *C
C               0:normal,  100: PMAX exceeded                       *C
C*                                                                  *C
C* (Relation)                                                       *C
C*    Calls RAN and RNDFR2 subroutines.                             *C
C* (Update Record)                                                  *C
C*    11/23/93  T.Ohgaki   Original version.                        *C
C*    10/23/96  T.Ohgaki   Modify final polarization.               *C
C*     1/22/97  K.Yokoya   LPCH, PMAX, IRTN introduced              *C
C*                         Final polarization changed.              *C
C*                                                                  *C
C* Original name: LSRCMP                                            *C
C* Changed to LNCPGN in CAIN.                                       *C
CC******************************************************************CC
      SUBROUTINE LNCPGN(P1,W,N,PE,UV1,UV2,PP,PD,DT,PMAX,LPCH,LPE,LPP,
     %  L,P3,K3,PE3,PP3,MSG,PROB,IRTN)
      IMPLICIT NONE
      REAL*8  P1(3),K1(3),W,N(3),PE(3),UV1(3),UV2(3),PP(3),PD,DT,PMAX
      REAL*8  P3(3),K3(3),PE3(3),PP3(3),PROB
      INTEGER LPCH,LPE,LPP,L,MSG,IRTN
      REAL*8  C0,P,E1,V(3),GA,VR,C,W1,X,Y,N1(3),WY0,WY,PP1(3)
      REAL*8  COSTH,SINTH,W2,COSAP,SINAP,UVA(3),UVB(3)
      REAL*8  UVA1(3),UVB1(3),UVA2(3),UVB2(3)
      REAL*8  COSAP2,SINAP2,U1,U2
      REAL*8  RANDCAIN
	EXTERNAL RANDCAIN
      REAL*8  N2(3),P2(3),K2(3)
      REAL*8  LX,XY1,H,WINT,WINT0,WINT1,VN2,VP2,E2,PHI,Z,
     %    COSPH,SINPH,A1,B1,A2,B2
      INTEGER I,LPP1,LPE1,LN1K1
	INCLUDE 'include/ctrlcm.h'
 
      REAL*8  COSPH2,SINPH2,PPL(3),F(3),F1(3),GX,G(3),G1(3)
      REAL*8  F3,F11,F22,F33,KK(3),NN(3),NN1(3)
      REAL*8  GL1,GL2,GL3,GL4,GL5,N1PE,N2PE,NN1PE,N1N2PE,K1K2PE,
     %        GLPE(3)
      REAL*8  UPH,A,PPL1(3),PE2(3),CD1(3),CD2(3)
      REAL*8  COSAPD,SINAPD,PHI2,COSAPD2,SINAPD2,PP2(3)
      REAL*8  W3,N3(3),EX(3),UVD1(3),COSBD,SINBD,PHI3,N3EX
      REAL*8  COSBD2,SINBD2,GA2,E3,GA3
      REAL*8  RHO(3),RHO1,RHO2,RHO3,RHO4,RHO5(3)
 
      REAL*8  C10,C20
      REAL*8  appl,appl1,ppl12av,s0,x0,probav,xrobav
 
C***************************************
      REAL*8  M2TOMB/1.d31/
      REAL*8  M/0.510999062D+6/
      REAL*8  RE/2.81794092D-15/
      REAL*8  PI/3.141592653589793238D0/
      REAL*8  CV/2.99792458D+8/
      REAL*8  EL/1.60217733D-19/
      REAL*8  C00/2.0775063D-18/
C            C00=4*PI*(RE**2)/(CV*EL)
C***************************************

c     REAL*8  w3min/62.5d9/
      REAL*8  w3min/62.27d9/
c     REAL*8  w3min/42.5d9/
      REAL*8  applmx/-1.d40/
      REAL*8  appl1mx/-1.d40/
      REAL*8  ppl12sm/0./
      INTEGER nppl12/0/
      REAL*8  probsm/0./
      REAL*8  xrobsm/0./
      INTEGER nprob/0/
      
      L=0
      IRTN=0
      C0=C00*PD*DT/W
      P=RANDCAIN()
      IF(MSG.GE.3) WRITE(MSGFL,910) 'W,PD,DT,C0,P=',W,PD,DT,C0,P
 910  FORMAT(1X,A,1P6D11.4,:,/,(5X,1P6D11.4,:))
 920  FORMAT(1X,A,1P3D17.10,:,/,(5X,1P3D17.10,:))
C--
C      IF (P.GE.2*C0) GOTO 800
C--
      IF(MSG.GE.1.AND.MSG.LE.2) 
     %   WRITE(MSGFL,910) 'W,PD,DT,C0,P=',W,PD,DT,C0,P
      IF(MSG.GE.2) WRITE(MSGFL,910) 'UV1,UV2=',UV1,UV2
C***************************************
      E1=SQRT(M**2+P1(1)**2+P1(2)**2+P1(3)**2)
      DO 10 I=1,3
         V(I)=P1(I)/E1
 10   CONTINUE
      s0=m**2+2.*e1*w*(1.-n(1)*v(1)-n(2)*v(2)-n(3)*v(3))
      x0=s0/m**2-1.
      IF(MSG.GE.2) WRITE(MSGFL,920) 'V=',V
      GA=E1/M
      VR=((N(1)-V(1))**2+(N(2)-V(2))**2+(N(3)-V(3))**2+1/(GA**2))/2
      W1=GA*W*VR
C        laser photon energy in the rest frame of electron
      X=2*W1/M
      IF(X.GE.0.01) THEN
        LX=LOG(1+X)
      ELSE
        LX=(((-0.25D0*X+0.333333333333333D0)*X-0.5D0)*X+1D0)*X
      ENDIF
      C=C0*VR*LX/X
      PROB=C
C       Approximate probability.
      IF(C.GT.PMAX) THEN
        IRTN=100
        RETURN
      ENDIF
C--
      P=RANDCAIN()
      IF (LPCH.EQ.0.AND.P.GE.C) RETURN
C-- 
      IF(MSG.GE.2) WRITE(MSGFL,910) 'GA.VR.C.E1=',GA,VR,C,E1
      LPE1=1
      IF (PE(1).EQ.0.AND.PE(2).EQ.0.AND.PE(3).EQ.0) LPE1=0
      IF(PP(2).NE.0.AND.LPE1.NE.0) THEN
        LN1K1=1
        CALL CALN1K1(GA,VR,W1,N,V,N1,K1)
        H=PP(2)*(N1(1)*PE(1)+N1(2)*PE(2)+N1(3)*PE(3))
      ELSE
        LN1K1=0
        H=0
      ENDIF
      IF(X.GE.0.1) THEN
        WINT0=(((-8D0/X-4D0)/X+1D0)*LX+0.5D0+8D0/X-0.5D0/(1+X)**2)
     %         /(2*LX)
      ELSE
        WINT0=(((0.69014550D0*X-0.59166667D0)*X+0.47777777778D0)*X
     %         -0.333333333333333D0)*X+0.666666666666667D0
      ENDIF
      IF(H.NE.0.OR.LPCH.NE.0) THEN
        IF(X.GE.0.1) THEN
          WINT1=(-(1+2/X)*LX+2+0.5D0/(1+X)**2)/(2*LX)
        ELSE
          WINT1=((((-0.554166667D0*X+0.4527777778D0)*X
     %         -0.333333333333333D0)*X+0.166666666666667D0)*X)
        ENDIF
        WINT=WINT0+H*WINT1
      ELSE
        WINT=WINT0
      ENDIF
      PROB=C*WINT
c     write (msgfl,823) " s0,x0,e1,prob= ",s0,x0,e1,prob
c 823  format(1x,a,4d14.6)
      IF(P.GE.PROB) GOTO 800
C**************************************
 21   Z=LX*RANDCAIN()
      IF(Z.LE.0.01D0) THEN
        Y=((((Z/5D0+1D0)*Z/4D0+1D0)*Z/3D0+1D0)*Z/2D0+1D0)*Z/X
      ELSE
        Y=(EXP(Z)-1)/X
      ENDIF
      XY1=X*Y+1
      COSTH=1-2*Y
      WY0=1/XY1+X*Y+COSTH**2
C********************************************************************
      IF(H.NE.0) WY0=WY0-X*Y*COSTH*(XY1+1)/XY1*H
C****************************************************************
      WY=WY0/(2*XY1)
      P=RANDCAIN()
      IF (P.GE.WY) GOTO 21
      IF(LN1K1.EQ.0) THEN
        CALL CALN1K1(GA,VR,W1,N,V,N1,K1)
        LN1K1=1
      ENDIF
      IF(MSG.GE.2)   WRITE(MSGFL,910) 'N1,K1=',N1,K1
      SINTH=2*SQRT(Y*(1-Y))
      W2=W1/XY1
      IF(MSG.GE.2)
     %  WRITE(MSGFL,910) 'WY.COSTH.SINTH.W2=',WY,COSTH,SINTH,W2
C*****************************************************
      LPP1=1
      IF (PP(1).EQ.0.AND.PP(2).EQ.0.AND.PP(3).EQ.0) LPP1=0
      COSAP=(UV1(1)*V(1)+UV1(2)*V(2)+UV1(3)*V(3))
      SINAP=(UV2(1)*V(1)+UV2(2)*V(2)+UV2(3)*V(3))
      IF(COSAP.EQ.0.AND.SINAP.EQ.0) THEN
        COSAP=1
        SINAP=0
      ELSE
        PHI=SQRT(COSAP**2+SINAP**2)
        COSAP=COSAP/PHI
        SINAP=SINAP/PHI
      ENDIF
      IF(MSG.GE.2) WRITE(MSGFL,910) 'COSAP.SINAP=',COSAP,SINAP
      DO 30 I=1,3
        UVA(I)= COSAP*UV1(I)+SINAP*UV2(I)
        UVB(I)=-SINAP*UV1(I)+COSAP*UV2(I)
 30   CONTINUE
      IF(MSG.GE.2) WRITE(MSGFL,910) 'UVA.UVB=',UVA,UVB
      IF (LPP1.NE.0) THEN
         COSAP2=COSAP**2-SINAP**2
         SINAP2=2*SINAP*COSAP
         IF(MSG.GE.2) WRITE(MSGFL,910) 'COSAP2.SINAP2=',COSAP2,SINAP2
         PP1(1)=COSAP2*PP(1)-SINAP2*PP(3)
         PP1(2)=PP(2)
         PP1(3)=SINAP2*PP(1)+COSAP2*PP(3)
         IF(MSG.GE.2) WRITE(MSGFL,910) 'PP1=',PP1
      ENDIF
      UVA1(1)=UVB(2)*N1(3)-UVB(3)*N1(2)
      UVA1(2)=UVB(3)*N1(1)-UVB(1)*N1(3)
      UVA1(3)=UVB(1)*N1(2)-UVB(2)*N1(1)
      DO 35 I=1,3
         UVB1(I)=UVB(I)
 35   CONTINUE
      IF(MSG.GE.2) WRITE(MSGFL,910) 'UVA1,UVB1=',UVA1,UVB1
C******************************************************
      IF(LPP1.EQ.0) THEN
c        PHI=2*PI*RAN11(0)
c        PHI=2*PI*RAN()
        PHI=2*PI*RANDCAIN()
        COSPH=COS(PHI)
        SINPH=SIN(PHI)
      ELSE
        U1=-X*Y*SINTH/(XY1*WY0)*PP1(2)
          A1=U1*(UVA1(1)*PE(1)+UVA1(2)*PE(2)+UVA1(3)*PE(3))
          B1=U1*(UVB1(1)*PE(1)+UVB1(2)*PE(2)+UVB1(3)*PE(3))
CCCC        U2=SINTH**2/WY0
CCCC          A2=U2*PP1(1)
CCCC          B2=U2*(-PP1(3))
        U2=-SINTH**2/WY0
          A2=U2*PP1(3)
          B2=U2*PP1(1)
        CALL RNDFR2(A1,B1,A2,B2,PHI,COSPH,SINPH)
      ENDIF
      IF(MSG.GE.2) WRITE(MSGFL,910) 'PHI.COSPH,SINPH=',PHI,COSPH,SINPH
C******************************************************
      DO 40 I=1,3
         N2(I)=N1(I)*COSTH+SINTH*(UVA1(I)*COSPH+UVB1(I)*SINPH)
         K2(I)=W2*N2(I)
         P2(I)=K1(I)-K2(I)
 40   CONTINUE
      IF(MSG.GE.2) WRITE(MSGFL,910) 'K2.P2=',K2,P2
C*******************************************************
      E2=SQRT(M**2+P2(1)**2+P2(2)**2+P2(3)**2)
      VN2=GA*(GA/(GA+1)*(V(1)*N2(1)+V(2)*N2(2)+V(3)*N2(3))+1)
      VP2=GA*(GA/(GA+1)*(V(1)*P2(1)+V(2)*P2(2)+V(3)*P2(3))+E2)
      DO 50 I=1,3
         P3(I)=P2(I)+V(I)*VP2
         K3(I)=W2*(N2(I)+VN2*V(I))
 50   CONTINUE
      IF(MSG.GE.2) WRITE(MSGFL,910) 'K3.P3=',K3,P3
      IF(MSG.GE.2) WRITE(MSGFL,910) 'E1.E2,W,W2=',e1,e2,W,W2
 
C*****7************20*** Final polarization *****50******************7
      IF(LPE.EQ.0.AND.LPP.EQ.0) GOTO 700
      COSPH2=COSPH**2-SINPH**2
      SINPH2=2*COSPH*SINPH
 
CCCC      PPL(1)=-SINPH2*PP1(1)-COSPH2*PP1(3)      
      PPL(1)=-COSPH2*PP1(1)+SINPH2*PP1(3)
      PPL(2)=PP1(2)
CCCC      PPL(3)= COSPH2*PP1(1)-SINPH2*PP1(3)
      PPL(3)=-SINPH2*PP1(1)-COSPH2*PP1(3)
      IF(MSG.GE.2) WRITE(MSGFL,910) 'PPL=',PPL
      DO 60 I=1,3
         KK(I)=K1(I)-K2(I)
         NN(I)=N1(I)+N2(I)
 60   CONTINUE
      IF(MSG.GE.2) WRITE(MSGFL,910) 'KK.NN=',KK,NN
      NN1(1)=N1(2)*N2(3)-N1(3)*N2(2)
      NN1(2)=N1(3)*N2(1)-N1(1)*N2(3)
      NN1(3)=N1(1)*N2(2)-N1(2)*N2(1)
 
      UPH=1+A1*COSPH+B1*SINPH+A2*COSPH2+B2*SINPH2
      A=WY0*UPH
      IF(MSG.GE.2) WRITE(MSGFL,910) 'UPH.A=',UPH,A
C****************************** PE ***********************************
C      IF(LPE.LE.0) GOTO 300
      DO 70 I=1,3
         F(I)=-2*Y*(K1(I)*COSTH+K2(I))/M
         F1(I)=-2*Y*(K1(I)+K2(I)*COSTH)/M
 70   CONTINUE
      IF(MSG.GE.2) WRITE(MSGFL,910) 'F.F1=',F,F1
      IF(LPE.LE.0) GOTO 300
 
      GX=SINTH**2/M*(1+1/XY1)/(1-1/XY1+4/X)
      DO 80 I=1,3
         G(I)=F(I)+GX*KK(I)
         G1(I)=F1(I)+GX*KK(I)
 80   CONTINUE
         IF(MSG.GE.2) WRITE(MSGFL,910) 'G.G1=',G,G1
 
      N1PE=N1(1)*PE(1)+N1(2)*PE(2)+N1(3)*PE(3)
      N2PE=N2(1)*PE(1)+N2(2)*PE(2)+N2(3)*PE(3)
      NN1PE=NN1(1)*PE(1)+NN1(2)*PE(2)+NN1(3)*PE(3)
      N1N2PE=N1PE+N2PE
      K1K2PE=W1*N1PE-W2*N2PE
 
      GL1=1+COSTH**2+(1-1/XY1)*X/4*SINTH**2
      GL2=(1+1/XY1)*X/4*(1+COSTH)
      GL3=-(1-1/XY1)*X/4
      GL4=(1-1/XY1)/(M*(1-1/XY1+4/X))*(1+COSTH)
      GL5=-2/((M**2*X)*(1-1/XY1+4/X))*(SINTH**2+2*COSTH)
 
      IF(MSG.GE.2) WRITE(MSGFL,910) 'GL=',GL1,GL2,GL3,GL4,GL5
      DO 100 I=1,3
        GLPE(I)=GL1*PE(I)+GL2*(N2(I)*N1PE-N1(I)*N2PE)
     %         +GL3*(NN(I)*N1N2PE+NN1(I)*NN1PE)
     %         +(GL4*N1N2PE+GL5*K1K2PE)*KK(I)
 100    CONTINUE
        IF(MSG.GE.2) WRITE(MSGFL,910) 'GLPE=',GLPE
 
      DO 120 I=1,3
         PE2(I)=(G(I)*PPL(2)+GLPE(I))/A
 120  CONTINUE
         IF(MSG.GE.2) WRITE(MSGFL,910) 'PE2=',PE2
 
      GA2=E2/M
      E3=SQRT(P3(1)**2+P3(2)**2+P3(3)**2+M**2)
      GA3=E3/M
      IF(MSG.GE.2) WRITE(MSGFL,910) 'GA2.GA3=',GA2,GA3
      
      RHO(1)=(P1(2)*P2(3)-P1(3)*P2(2))/M**2
      RHO(2)=(P1(3)*P2(1)-P1(1)*P2(3))/M**2
      RHO(3)=(P1(1)*P2(2)-P1(2)*P2(1))/M**2
      RHO1=(1+GA)*(1+GA3)*(1+GA2)
      RHO2=RHO(1)**2+RHO(2)**2+RHO(3)**2
      RHO3=RHO(1)*PE2(1)+RHO(2)*PE2(2)+RHO(3)*PE2(3)
      RHO4=(1+GA+GA3+GA2)
      RHO5(1)=PE2(2)*RHO(3)-PE2(3)*RHO(2)
      RHO5(2)=PE2(3)*RHO(1)-PE2(1)*RHO(3)
      RHO5(3)=PE2(1)*RHO(2)-PE2(2)*RHO(1)
 
      DO 160 I=1,3
         PE3(I)=((RHO1-RHO2)*PE2(I)+RHO(I)*RHO3+RHO4*RHO5(I))/RHO1
 160  CONTINUE
C***************************** PP ***********************************
 300  IF(LPP.LE.0) GOTO 700
 
      F3=SINTH**2
      F11=2*COSTH
      F22=(XY1+1/XY1)*COSTH
      F33=1+COSTH**2
      IF(MSG.GE.2) WRITE(MSGFL,910) 'F3.F11.F22.F33=',F3,F11,F22,F33
 
      PPL1(1)=F11*PPL(1)/A
      PPL1(2)=(F22*PPL(2)+F1(1)*PE(1)+F1(2)*PE(2)+F1(3)*PE(3))/A
      PPL1(3)=(F3+F33*PPL(3))/A
      IF(MSG.GE.2) WRITE(MSGFL,910) 'PPL1=',PPL1(1),PPL1(2),PPL1(3)
 
      CD1(1)=NN1(1)
      CD1(2)=NN1(2)
      CD1(3)=NN1(3)
      
      CD2(1)=N2(2)*CD1(3)-N2(3)*CD1(2)
      CD2(2)=N2(3)*CD1(1)-N2(1)*CD1(3)   
      CD2(3)=N2(1)*CD1(2)-N2(2)*CD1(1)
 
      COSAPD=(CD1(1)*V(1)+CD1(2)*V(2)+CD1(3)*V(3))
      SINAPD=(CD2(1)*V(1)+CD2(2)*V(2)+CD2(3)*V(3))
 
      IF (COSAPD.EQ.0.AND.SINAPD.EQ.0) THEN 
         COSAPD=1
         SINAPD=0
      ELSE
         PHI2=SQRT(COSAPD**2+SINAPD**2)
         COSAPD=COSAPD/PHI2
         SINAPD=SINAPD/PHI2
      ENDIF
      COSAPD2=COSAPD**2-SINAPD**2
      SINAPD2=2*SINAPD*COSAPD
      IF(MSG.GE.2) WRITE(MSGFL,910) 'COSAPD.SINAPD=',COSAPD,SINAPD
 
      PP2(1)=COSAPD2*PPL1(1)-SINAPD2*PPL1(3)
      PP2(2)=PPL1(2)     
      PP2(3)=SINAPD2*PPL1(1)+COSAPD2*PPL1(3)
      DO 320 I=1,3
CCCC        UVB2(I)=CD1(I)*SINAPD+CD2(I)*COSAPD
        UVB2(I)=-CD1(I)*SINAPD+CD2(I)*COSAPD
 320  CONTINUE
      W3=SQRT(K3(1)**2+K3(2)**2+K3(3)**2)
      DO 330 I=1,3
         N3(I)=K3(I)/W3
 330  CONTINUE
         IF(MSG.GE.2) WRITE(MSGFL,910) 'N3=',N3
      UVA2(1)=UVB2(2)*N3(3)-UVB2(3)*N3(2)
      UVA2(2)=UVB2(3)*N3(1)-UVB2(1)*N3(3)
      UVA2(3)=UVB2(1)*N3(2)-UVB2(2)*N3(1)
     
      EX(1)=1
      EX(2)=0
      EX(3)=0
      N3EX=N3(1)*EX(1)+N3(2)*EX(2)+N3(3)*EX(3)
      DO 350 I=1,3
         UVD1(I)=EX(I)-N3(I)*N3EX
 350  CONTINUE   
         IF(MSG.GE.2) WRITE(MSGFL,910) 'UVD1=',UVD1         
      COSBD=UVD1(1)*UVA2(1)+UVD1(2)*UVA2(2)+UVD1(3)*UVA2(3)
      SINBD=UVD1(1)*UVB2(1)+UVD1(2)*UVB2(2)+UVD1(3)*UVB2(3)
      IF (COSBD.EQ.0.AND.SINBD.EQ.0) THEN 
         COSBD=1
         SINBD=0
      ELSE
         PHI3=SQRT(COSBD**2+SINBD**2)
         COSBD=COSBD/PHI3
         SINBD=SINBD/PHI3
      ENDIF
 
      COSBD2=COSBD**2-SINBD**2
      SINBD2=2*SINBD*COSBD
      PP3(1)=COSBD2*PP2(1)-SINBD2*PP2(3)
      PP3(2)=PP2(2)
      PP3(3)=SINBD2*PP2(1)+COSBD2*PP2(3)
      if(nppl12 .eq. 0) then
         write(msgfl,843) " w3min= ",w3min
 843     format(1x,a,d14.6)
      endif
      if(w3.gt.w3min) then
         nprob=nprob+1
         probsm=probsm+prob
         probav=probsm/nprob
c         xrobsm=xrobsm+prob*w/pd/dt
         xrobsm=xrobsm+prob/pd/dt*cv*el*w*m2tomb
         xrobav=xrobsm/nprob
         ppl12sm=ppl12sm+ppl1(2)
         nppl12=nppl12+1
         ppl12av=ppl12sm/nppl12
         appl=sqrt(ppl(1)**2+ppl(2)**2+ppl(3)**2)
         appl1=sqrt(ppl1(1)**2+ppl1(2)**2+ppl1(3)**2)
         if(appl.gt.applmx) then
            applmx=appl
         endif
         if(appl1.gt.appl1mx) then
            appl1mx=appl1
         endif
         write(msgfl,852) " w3,costh,applmx=",
     %    w3,costh,applmx,
     %    " ppl1(2),appl1,appl1mx= ", ppl1(2),appl1,appl1mx,
c     %    " nppl12,ppl12sm,ppl12av= ", nppl12,ppl12sm,ppl12av
     %        " ppl12av,probav,xrobav(mb)= ",
     %    ppl12av,probav,xrobav
 852  format(1x,a,3d14.6,3x,a,3d14.6,3x,a,3d14.6)
      endif
 
      IF(MSG.GE.2) WRITE(MSGFL,910) 'PP2=',PP2(1),PP2(2),PP2(3)
      IF(MSG.GE.2) WRITE(MSGFL,910) 'COSBD.SINBD=',COSBD,SINBD
C
 700  L=1
      RETURN
C**********************************************************************
 800  IF(LPCH.EQ.0) RETURN
C  Change of polarization when no event is generated
C  Following program replaced the one by Ohgaki. The latter has been
C  copied to a temp file junk/polchange
C  Calculate only the electron polarization.
      IF(PP(2).EQ.0) RETURN
      IF(LN1K1.EQ.0) CALL CALN1K1(GA,VR,W1,N,V,N1,K1)
      C10=(1-C*WINT0)/(1-C*WINT)
      C20=C*WINT1*PP(2)/(1-C*WINT)
      DO 820 I=1,3
        PE(I)=C10*PE(I)-C20*N1(I)
 820  CONTINUE
      C10=PE(1)**2+PE(2)**2+PE(3)**2
      IF(C10.GT.1) THEN
        C10=1/SQRT(C10)
        DO 830 I=1,3
          PE(I)=C10*PE(I)
 830    CONTINUE
      ENDIF
      RETURN
      END
C*************************************************************
      SUBROUTINE CALN1K1(GA,VR,W1,N,V,N1,K1)
      IMPLICIT NONE
      REAL*8 GA,VR,W1,N(3),V(3),N1(3),K1(3)
      INTEGER I
      REAL*8 VR1
      VR1=GA/(GA+1)*(GA*VR+1)
      DO 20 I=1,3
        N1(I)=(N(I)-VR1*V(I))/(GA*VR)
        K1(I)=W1*N1(I)
 20   CONTINUE
      RETURN
      END
