CC*******************************************************************CC
C*                                                                   *C
C*================================================================== *C 
C* Subroutine LNBWGN(W1,N1,PP1,UV11,UV12,W2,N2,PP2,UV21,UV22         *C
C*                       ,PD,DT,LPE,L,P3,P4,PE3,PE4,MSG)             *C 
C*================================================================== *C
C*                                                                   *C
C* (Purpose)                                                         *C      
C*    Breit-Wheeler Process Event Generator                          *C
C*                          Including Polarization Effects           *C 
C*                                                                   *C 
C* (Inputs)                                                          *C
C*      W1     : initial photon energy  (eV)  (Laser)                *C
C*      N1 (*) : unit vector along initial photon momentum           *C
C*      PP1(*) : Stokes parameter of initial photon                  *C
C*      UV11(*): unit vector associated with initial photon          *C
C*      UV12(*): unit vector associated with initial photon          *C
C*      W2     : initial photon energy  (eV)                         *C
C*      N2 (*) : unit vector along initial photon momentum           *C
C*      PP2(*) : Stokes parameter of initial photon                  *C
C*      UV21(*): unit vector associated with initial photon          *C
C*      UV22(*): unit vector associated with initial photon          *C
C*      PD     : power density of the laser beam  (Watt/m**2)        *C
C*      DT     : time interval times velocity of light (m)           *C
C*      LPE    : flag to compute final electreon polarization        *C 
C*      MSG    : flag to print debugging message                     *C
C*                                                                   *C
C* (Outputs)                                                         *C 
C*      L     : L=0  no  electron                                    *C 
C*            : L=1  electron and positron                           *C
C*      P3 (*): final positron momentum    (eV/c)                    *C
C*      P4 (*): final electron momentum    (eV/c)                    *C
C*      PE3(*): polarization vector of final positron                *C
C*      PE4(*): polarization vector of final electron                *C
C*                                                                   *C 
C* (Relation)                                                        *C
C*    Calls RANDCAIN subroutine.                                     *C   
C* (Update Record)                                                   *C
C*    01/18/94  K.Yokoya and T.Ohgaki  Original version.             *C
C*    06/23/94  G.Horton-Smith         Use RANDCAIN, not RAN         *C
C*    10/23/96  T.Ohgaki               Modify final polarization.    *C
C*    01/08/97  T.Ohgaki               Incoherent pair creation.     *C
C*                                                                   *C
C* Original name: LSRBWH                                             *C
C* Changed to LNBWGN in CAIN.                                        *C
CC*******************************************************************CC
 
      SUBROUTINE LNBWGN(W1,N1,PP1,UV11,UV12,W2,N2,PP2,UV21,UV22 
     %                      ,PD,DT,LPE,L,P3,P4,PE3,PE4,MSG)             
 
      IMPLICIT NONE   
      REAL*8  W1,N1(3),PP1(3),UV11(3),UV12(3)
      REAL*8  W2,N2(3),PP2(3),UV21(3),UV22(3)
      REAL*8  PD,DT
      REAL*8  P3(3),P4(3),PE3(3),PE4(3) 
      INTEGER LPE,L,MSG 
      REAL*8  RANDCAIN
	EXTERNAL RANDCAIN
	INCLUDE 'include/ctrlcm.h'
 
      REAL*8  S0,Z0
      REAL*8  C0,P
      REAL*8  V(3),GA,VR,C
      REAL*8  W,X,X1,ZR,Z,CZ,Y,COSTH
      REAL*8  VR1,N11(3)
      REAL*8  COSAP1,SINAP1,PHI1,UVA1(3),UVB1(3),COSAP12,SINAP12
      REAL*8  PP11(3),UVA11(3),UVB11(3)
      REAL*8  COSAP2,SINAP2,PHI2,UVA2(3),UVB2(3),COSAP22,SINAP22
      REAL*8  PP21(3),UVA21(3),UVB21(3)
      REAL*8  PHI,COSPH,SINPH
      REAL*8  COSPH2,SINPH2,PPL1(3),PPL2(3)
      REAL*8  SM,UM,COSTH1,D,F0,F3,F11,F22,F33,WY0,WY,SINTH,E
      REAL*8  P1(3),P2(3)
      REAL*8  VP3,VP4
      REAL*8  A,A1,A2,A3,D1,D2,K1(3),K2(3),PE1(3),PE2(3)
C      REAL*8  GA2,E3,GA3,RHO3(3),RHO31,RHO32,RHO33,RHO34,RHO35(3)
C      REAL*8  E4,GA4,RHO4(3),RHO41,RHO42,RHO43,RHO44,RHO45(3)
 
      INTEGER I,LINK1,LPP1,LPP2
 
      REAL*8  COSAP,SINAP
      REAL*8  DT1,C10,C11,WB,FB(3),EX(3),N3EX,FBP
      REAL*8  UVD1(3),COSBD,SINBD,PHI3,COSBD2,SINBD2
      REAL*8  bet,sigfb,sigfb0,sigfb00,omb2,brb,lnbr
      REAL*8  rrx,xx,rejden,rejnum,rejfac,omx2,pnew
      REAL*8  f3xi,f11xi,f22xi,f33xi,pnewav,cxwyav
      REAL*8  xnewav,xxwyav
      REAL*8  rejfmx/-1.d40/
 
      REAL*8  xnewsm/0./
      REAL*8  pnewsm/0./
      INTEGER npnew/0/
      INTEGER npaccp/0/
c      REAL*8  w2min/62.5d9/
      REAL*8  w2min/62.27d9/

      REAL*8  cxwysm/0./
      REAL*8  xxwysm/0./
      INTEGER ncxwy/0/

      
      REAL*8  M2TOFB/1.d43/
      REAL*8  M2TOMB/1.d31/
      REAL*8  M/0.510999062D+6/
      REAL*8  RE/2.81794092D-15/
      REAL*8  PI/3.14159265D0/
      REAL*8  CV/2.99792458D+8/
      REAL*8  EL/1.60217733D-19/
      REAL*8  C00/2.0775063D-18/
C     C00=4*PI*(RE**2)/(CV*EL)OA
C***************************** 1 to 2 *********************************
      Z0=N1(1)*N2(1)+N1(2)*N2(2)+N1(3)*N2(3)
      S0=2*W1*W2*(1-Z0)
C--   
      IF (4*M**2.GT.S0) GOTO 900
      bet=sqrt(1.-4.*M**2/S0)
      omb2=1.-bet**2
      brb=(1.+bet)/(1.-bet)
      lnbr=log(brb)
c23456789012345678901234567890123456789012345678901234567890123456789012
c       10        20        30        40        50        60        70      
      sigfb=m2tofb*0.25*pi*re**2*bet*omb2*(
     %   4.*lnbr*(1.-pp1(2)*pp2(2))-4.*bet*(1-3.*pp1(2)*pp2(2))
     %  +4.*omb2*lnbr*(1.-(pp1(3)+pp2(3))+pp1(1)*pp2(1)+pp1(3)*pp2(3))
     %     -2.*omb2**2*(2.*bet/omb2+lnbr)
     %    *(1.-(pp1(3)+pp2(3))+pp1(3)*pp2(3))
     %     +8.*bet*(pp1(1)*pp2(1)+pp1(3)*pp2(3)))
      sigfb0=m2tofb*0.25*pi*re**2*bet*omb2*(4.*(lnbr-bet)
     %  +4.*omb2*lnbr
     %  -2.*omb2**2*(2.*bet/omb2+lnbr))
      sigfb00=m2tofb*0.5*pi*re**2*omb2*(
     %   (3.-bet**4)*lnbr-2.*bet*(2.-bet**2))
c     print *, " pp1= ", pp1," pp2= ",pp2
c     write(msgfl,211) " w1,w2,bet,sigfb,sigfb0,sigfb00= ",
c    &   w1,w2,bet,sigfb,sigfb0,sigfb00
c 211  format(1x,a,6(1p,d14.6))
      pnew=sigfb/m2tofb*pd*dt/cv/el/w1
c     write(msgfl,223) " lnbwgn s0, pnew= ", s0,pnew
c 223  format(1x,a,2d14.6)
      rrx=randcain()
      xx=(brb**(2.*rrx-1.)-1.)/(brb**(2.*rrx-1.)+1.)
      omx2=1.-xx**2
      rejden=8./omx2
      f0=2.*(1.+xx**2)/omx2+4.*omb2/omx2-4.*omb2**2/omx2*2
      f3=4.*omb2**2/omx2**2-4.*omb2/omx2
      f11=4.*omb2/omx2-2.
      f22=2.*(1.+xx**2)/omx2-4.*omb2*(1+xx**2)/omx2**2
      f33=4.*omb2/omx2-4.*omb2**2/omx2**2-2.
      f3xi=f3*(pp1(3)+pp2(3))
      f11xi=f11*pp1(1)*pp2(1)
      f22xi=f22*pp1(2)*pp2(2)
      f33xi=f33*pp1(3)*pp2(3)
      rejnum=f0+f3xi+f11xi-f22xi+f33xi
      rejfac=rejnum/rejden
      if(rejfac.gt.rejfmx) then
         rejfmx=rejfac
      endif
c     print *, " xx= ",xx," f0= ",f0," f11= ",f11,
c    %     " f22= ",f22," f33= ",f33
c     print *, " xx= ",xx," f0= ",f0," f11xi= ",f11xi,
c    %     " f22xi= ",f22xi," f33xi= ",f33xi
c     print *, " rrx,xx= ", rrx,xx
c     print *, " xx= ",xx," rejnum= ",rejnum,
c    %   " rejden= ",rejden," rejfac= ",rejfac," rejfmx= ",rejfmx
      
C--
      IF(MSG.GE.3) PRINT *,'Z0,S0=',Z0,S0
C***************************** 3 to 5 *********************************
      C0=C00*PD*DT/W1             
      P=RANDCAIN()
C--
C      IF (P.GE.2*C0) GOTO 800
C--
      IF(MSG.GE.3) PRINT *,'W,PD,DT,C0,P=',W,PD,DT,C0,P
      IF(MSG.GE.2) PRINT *,'UV11,UV12=',UV11,UV12
      IF(MSG.GE.2) PRINT *,'UV21,UV22=',UV21,UV22
C***************************** 6 to 7 *********************************   
      DO 10 I=1,3
         V(I)=(W1*N1(I)+W2*N2(I))/(W1+W2)  
 10   CONTINUE
      GA=(W1+W2)/SQRT(S0)
      VR=((N1(1)-V(1))**2+(N1(2)-V(2))**2+(N1(3)-V(3))**2+1/GA**2)/2
      C=C0*VR
C--
C      IF (P.GE.C) GOTO 800
C--
      IF(MSG.GE.2) PRINT *,'V=',V(1),V(2),V(3)
      IF(MSG.GE.2) PRINT *,'GA,VR,C',GA,VR,C
C***************************** 8 to 9  ********************************
      W=GA*W1*VR
      X=SQRT(W**2-M**2)/W
      X1=M**2/(W**2+W*SQRT(W**2-M**2))
      ZR=RANDCAIN()
      Z=1-2*ZR
      CZ=2*LOG(S0/(4*M**2))
      Y=(1+(1-EXP(CZ*Z))/(EXP(CZ)-1))/(1+EXP(CZ*Z))
      IF(MSG.GE.2) PRINT *,'W,X,X1',W,X,X1
      IF(MSG.GE.2) PRINT *,'ZR,Z,CZ,Y=',ZR,Z,CZ,Y
      COSTH=1-2*Y
C******************************* 10 ***********************************
      IF (PP1(1).EQ.0.AND.PP1(2).EQ.0.AND.PP1(3).EQ.0.AND.PP2(1).EQ.0
     %    .AND.PP2(2).EQ.0.AND.PP2(3).EQ.0) THEN
         LINK1=0
      ELSE
         LINK1=1
         VR1=GA/(GA+1)*(GA*VR+1)
         DO 20 I=1,3
            N11(I)=(N1(I)-VR1*V(I))/(GA*VR)
 20      CONTINUE
      ENDIF
C******************************** 11 **********************************
      LPP1=1
      IF (PP1(1).EQ.0.AND.PP1(2).EQ.0.AND.PP1(3).EQ.0) LPP1=0
      COSAP1=(UV11(1)*V(1)+UV11(2)*V(2)+UV11(3)*V(3))
      SINAP1=(UV12(1)*V(1)+UV12(2)*V(2)+UV12(3)*V(3))
      IF (COSAP1.EQ.0.AND.SINAP1.EQ.0) THEN 
         COSAP1=1
         SINAP1=0
      ELSE
         PHI1=SQRT(COSAP1**2+SINAP1**2)
         COSAP1=COSAP1/PHI1
         SINAP1=SINAP1/PHI1
      ENDIF
      IF(MSG.GE.2) PRINT *,'COSAP1,SINAP1=',COSAP1,SINAP1
 
      DO 30 I=1,3 
         UVA1(I)= COSAP1*UV11(I)+SINAP1*UV12(I)
         UVB1(I)=-SINAP1*UV11(I)+COSAP1*UV12(I)
         IF (MSG.GE.2) PRINT *,'UVA1,UVB1=',UVA1(I),UVB1(I)
 30   CONTINUE
 
      IF (LPP1.NE.0) THEN
         COSAP12=COSAP1**2-SINAP1**2
         SINAP12=2*SINAP1*COSAP1
         IF (MSG.GE.2) PRINT *,'COSAP12,SINAP12=',COSAP12,SINAP12
         PP11(1)=COSAP12*PP1(1)-SINAP12*PP1(3)
         PP11(2)=PP1(2)
         PP11(3)=SINAP12*PP1(1)+COSAP12*PP1(3)
         IF (MSG.GE.2) PRINT *,'PP11',PP11(1),PP11(2),PP11(3)
      ENDIF
      UVA11(1)=UVB1(2)*N11(3)-UVB1(3)*N11(2)
      UVA11(2)=UVB1(3)*N11(1)-UVB1(1)*N11(3)
      UVA11(3)=UVB1(1)*N11(2)-UVB1(2)*N11(1)
      DO 40 I=1,3
         UVB11(I)=UVB1(I)
 40   CONTINUE
      IF (MSG.GE.2) PRINT *,'UVA11=',UVA11(1),UVA11(2),UVA11(3)
      IF (MSG.GE.2) PRINT *,'UVB11=',UVB11(1),UVB11(2),UVB11(3)
C******************************* 12 **********************************
      LPP2=1
      IF (PP2(1).EQ.0.AND.PP2(2).EQ.0.AND.PP2(3).EQ.0) LPP2=0
      COSAP2=(UV21(1)*V(1)+UV21(2)*V(2)+UV21(3)*V(3))
      SINAP2=(UV22(1)*V(1)+UV22(2)*V(2)+UV22(3)*V(3))
      IF (COSAP2.EQ.0.AND.SINAP2.EQ.0) THEN 
         COSAP2=1
         SINAP2=0
      ELSE
         PHI2=SQRT(COSAP2**2+SINAP2**2)
         COSAP2=COSAP2/PHI2
         SINAP2=SINAP2/PHI2
      ENDIF
      IF(MSG.GE.2) PRINT *,'COSAP2,SINAP2=',COSAP2,SINAP2
 
      DO 50 I=1,3 
         UVA2(I)= COSAP2*UV21(I)+SINAP2*UV22(I)
         UVB2(I)=-SINAP2*UV21(I)+COSAP2*UV22(I)
         IF (MSG.GE.2) PRINT *,'UVA2,UVB2=',UVA2(I),UVB2(I)
 50   CONTINUE
 
      IF (LPP2.NE.0) THEN
         COSAP22=COSAP2**2-SINAP2**2
         SINAP22=2*SINAP2*COSAP2
         IF (MSG.GE.2) PRINT *,'COSAP22,SINAP22=',COSAP22,SINAP22
         PP21(1)=COSAP22*PP2(1)-SINAP22*PP2(3)
         PP21(2)=PP2(2)
         PP21(3)=SINAP22*PP2(1)+COSAP22*PP2(3)
         IF (MSG.GE.2) PRINT *,'PP21',PP21(1),PP21(2),PP21(3)
      ENDIF
      UVA21(1)=-(UVB2(2)*N11(3)-UVB2(3)*N11(2))
      UVA21(2)=-(UVB2(3)*N11(1)-UVB2(1)*N11(3))
      UVA21(3)=-(UVB2(1)*N11(2)-UVB2(2)*N11(1))
      DO 60 I=1,3
         UVB21(I)=UVB2(I)
 60   CONTINUE
      IF (MSG.GE.2) PRINT *,'UVA21=',UVA21(1),UVA21(2),UVA21(3)
      IF (MSG.GE.2) PRINT *,'UVB21=',UVB21(1),UVB21(2),UVB21(3)
C******************************* 13 ***********************************
      PHI=RANDCAIN()
      COSPH=COS(PHI)
      SINPH=SIN(PHI)
      IF (MSG.GE.2) PRINT *,'PHI,COSPH,SINPH',PHI,COSPH,SINPH
C******************************* 14 *********************************** 
      COSPH2=COSPH**2-SINPH**2
      SINPH2=2*COSPH*SINPH
      PPL1(1)=-SINPH2*PP11(1)-COSPH2*PP11(3)      
      PPL1(2)= PP11(2)
      PPL1(3)= COSPH2*PP11(1)-SINPH2*PP11(3)
      PPL2(1)=-SINPH2*PP21(1)-COSPH2*PP21(3)      
      PPL2(2)= PP21(2)
      PPL2(3)= COSPH2*PP21(1)-SINPH2*PP21(3)
      IF (MSG.GE.2) PRINT *,'PPL1=',PPL1(1),PPL1(2),PPL1(3)
      IF (MSG.GE.2) PRINT *,'PPL2=',PPL2(1),PPL2(2),PPL2(3)
C**************************** 15 to 16 ******************************** 
      SM=2*M**2/((1+X)*X1)*(X*(1-2*Y)-1)
      UM=-2*M**2/((1+X)*X1)*(X*(1-2*Y)+1)
      COSTH1=1+2*M**2*(1/UM+1/SM)
      D=-(SM/UM+UM/SM)
      F0=D-(1-COSTH1**2)
      F3=(1-COSTH1**2)
      F11=2*COSTH1
      F22=D*COSTH1
      F33=1+COSTH1**2
      WY0=F0+F3*(PPL1(3)+PPL2(3))+F11*PPL1(1)*PPL2(1)
     %   -F22*PPL1(2)*PPL2(2)+F33*PPL1(3)*PPL2(3)
      WY=WY0*X*(1+X)*X1/4
     %   *(-CZ*EXP(CZ*Z)/((EXP(CZ)-1)*(1+EXP(CZ*Z)))-Y*CZ*EXP(CZ*Z)
     %    /(1+EXP(CZ*Z)))
      IF(MSG.GE.3) PRINT *,'SM,UM,COSTH1,D=',SM,UM,COSTH1,D
      IF(MSG.GE.3) PRINT *,'F0,F3,F11,F22,F33=',F0,F3,F11,F22,F33
      IF(MSG.GE.3) PRINT *,'WY0=',WY0
      IF(MSG.GE.2) PRINT *,'WY,COSTH,SINTH,E=',WY,COSTH,SINTH,E
      IF(MSG.GE.2) PRINT *,'c*wy= ',c*wy
c      if(npnew .eq. 0) then
c         write(msgfl,843) " w2min= ",w2min
c 843     format(1x,a,d14.6)
c      endif
      if(w2.gt.w2min) then
         npnew=npnew+1
c         xnewsm=xnewsm+pnew*w1/pd/dt
         xnewsm=xnewsm+pnew/pd/dt*cv*el*w1*m2tomb
         xnewav=xnewsm/npnew
         pnewsm=pnewsm+pnew
         pnewav=pnewsm/npnew
         ncxwy=ncxwy+1
c         xxwysm=xxwysm+c*wy*w1/pd/dt
         xxwysm=xxwysm+c*wy/pd/dt*cv*el*w1*m2tomb
         xxwyav=xxwysm/ncxwy
         cxwysm=cxwysm+c*wy
         cxwyav=cxwysm/ncxwy
c         write(msgfl,852)
c     %        " s0,w2,npnew,pnew,pnewav= ",
c     %     s0,w2,npnew,pnew,pnewav,
c     %    " ncxwy,cxwy,cxwyav= ", ncxwy,c*wy,cxwyav
c         write(msgfl,852)
c     %        " s0,w2,npnew,xnew,xnewav(mb)= ",
c     %     s0,w2,npnew,pnew*w1/pd/dt,xnewav,
c     %    " ncxwy,xxwy,xxwyav= ", ncxwy,c*wy*w1/pd/dt,xxwyav
c 852  format(1x,a,2d14.6,i8,2d14.6,3x,a,i8,2d14.6)
      endif
      IF (P.GE.C*WY) GOTO 800
      if(w2.gt.w2min) then
         npaccp=npaccp+1
         print *, " lnwbgn npnew,npaccp= ", npnew,npaccp
      endif
      IF (LINK1.EQ.0) THEN
         VR1=GA/(GA+1)*(GA*VR+1)
         DO 70 I=1,3
            N11(I)=(N1(I)-VR1*V(I))/(GA*VR)
 70      CONTINUE
      ENDIF
      SINTH=2*SQRT(Y*(1-Y))
      E=W         
      IF(MSG.GE.3) PRINT *,'ACC SM,UM,COSTH1,D=',SM,UM,COSTH1,D
      IF(MSG.GE.3) PRINT *,'ACC F0,F3,F11,F22,F33=',F0,F3,F11,F22,F33
      IF(MSG.GE.3) PRINT *,'ACC WY0=',WY0
      IF(MSG.GE.2) PRINT *,'ACC WY,COSTH,SINTH,E=',WY,COSTH,SINTH,E
C******************************* 17 ***********************************
      DO 80 I=1,3
         P1(I)=SQRT(W**2-M**2)*(N11(I)*COSTH
     %               +SINTH*(UVA1(I)*COSPH+UVB1(I)*SINPH))
         P2(I)=-P1(I)
         IF (MSG.GE.2) PRINT *,'P1.P2',P1(I),P2(I)
 80   CONTINUE
C******************************* 18 ***********************************
      VP3=GA*(GA/(1+GA)*(P1(1)*V(1)+P1(2)*V(2)+P1(3)*V(3))+E)
      VP4=GA*(GA/(1+GA)*(P2(1)*V(1)+P2(2)*V(2)+P2(3)*V(3))+E)
      IF (MSG.GE.3) PRINT *,'VP3.VP4',VP3,VP4
      DO 90 I=1,3
         P3(I)=P1(I)+V(I)*VP3            
         P4(I)=P2(I)+V(I)*VP4
         IF (MSG.GE.2) PRINT *,'P3.P4',P3(I),P4(I)
 90   CONTINUE
C******************************* 19 ***********************************
      A=WY0
      A1=SQRT(1+X)*X1
      A2=1+A1
      A3=(1-COSTH1)/M
      D1=(A2-X*(1-2*Y))/(A1*A2)
      D2=(A2+X*(1-2*Y))/(A1*A2)
      DO 100 I=1,3
         K1(I)=W*N11(I)
         K2(I)=-W*N11(I)
         PE1(I)=A3*(D2*P1(I)-K2(I)-COSTH1*(D1*P1(I)-K1(I)))*PPL1(2)
     %       +A3*(-(D1*P1(I)-K1(I))+COSTH1*(D2*P1(I)-K2(I)))*(-PPL2(2))
         PE2(I)=A3*(-(D1*P2(I)-K2(I))+COSTH1*(D2*P2(I)-K1(I)))*PPL1(2)
     %       +A3*((D2*P2(I)-K1(I))-COSTH1*(D1*P2(I)-K2(I)))*(-PPL2(2))
         PE2(I)=-PE2(I)
         PE1(I)=PE1(I)/A
         PE2(I)=PE2(I)/A
         IF(MSG.GE.2) PRINT *,'PE1.PE2=',PE1(I),PE2(I)
 100  CONTINUE
      IF(MSG.GE.2) PRINT *,'A=',A
C******************************* 20 ***********************************
      CALL LNBWPE(GA,E,V,P1,P2,P3,P4,PE1,PE2,PE3,PE4,MSG)
C**********************************************************************
 700  L=1
      RETURN
C**********************************************************************
 800  L=0
      IF (PP2(1).NE.0.AND.PP2(3).NE.0) THEN
        COSAP=(UV21(1)*V(1)+UV21(2)*V(2)+UV21(3)*V(3))
        SINAP=(UV22(1)*V(1)+UV22(2)*V(2)+UV22(3)*V(3))
 
        IF(COSAP.EQ.0.AND.SINAP.EQ.0) THEN
          COSAP=1
          SINAP=0
        ELSE
          PHI=SQRT(COSAP**2+SINAP**2)
          COSAP=COSAP/PHI
          SINAP=SINAP/PHI
        ENDIF
        IF(MSG.GE.2) WRITE(MSGFL,910) 'COSAP.SINAP=',COSAP,SINAP
 910    FORMAT(1X,A,1P6D11.4,:,/,(5X,1P6D11.4,:))
        
        COSAP2=COSAP**2-SINAP**2
        SINAP2=2*SINAP*COSAP
 
        PP21(1)=COSAP2*PP2(1)-SINAP2*PP2(3)
        PP21(2)=PP2(2)
        PP21(3)=SINAP2*PP2(1)+COSAP2*PP2(3)
 
        DO 120 I=1,3
          UVA2(I)= COSAP*UV21(I)+SINAP*UV22(I)
          UVB2(I)=-SINAP*UV21(I)+COSAP*UV22(I)
 120    CONTINUE
 
      ELSE
 
        PP21(1)=PP2(1)
        PP21(2)=PP2(2)
        PP21(3)=PP2(3)
 
        DO 130 I=1,3
          UVA2(I)=UV21(I)
          UVB2(I)=UV22(I)
 130    CONTINUE
 
      ENDIF
 
      PPL2(1)=-PP21(3)
      PPL2(2)=PP21(2)
      PPL2(3)=PP21(1)
 
      DT1=DT/GA            
      C10=0.25D0*PI*RE**2*(1+X)*X1*X
      C11=0.5D0*DLOG((1+X)/X1)
      WB=-C10*(2-PPL2(3)+(PPL2(3)-1)*X**2+(X**4-3+PPL2(3)
     %   *(1+X**2)*(1+X)*X1)/X*C11)
 
      FB(1)=-C10*(1-2*(1+X)*X1*C11/X)*PPL2(1)
      FB(2)=C10*(3-2*C11/X)*PPL2(2)
      FB(3)=-C10*(-(1+X)*X1+(1+X**2)*(1+X)*X1/X*C11
     %            +(1+(1+X)*X1-(1+X**2)*(1+X)*X1/X*C11)*PPL2(3))
 
      FBP=FB(1)*PPL2(1)+FB(2)*PPL2(2)+FB(3)*PPL2(3)
      IF(MSG.GE.2) WRITE(MSGFL,910) 'WB.FB=',WB,FB(1),FB(2),FB(3)
 
      DO 140 I=1,3
        PPL2(I)=(PPL2(I)*(1-WB*DT)-FB(I)*DT)/(1-(WB+FBP)*DT)
 140  CONTINUE
      IF(MSG.GE.2) WRITE(MSGFL,910) 'PPL2=',PPL2(1),PPL2(2),PPL2(3)
 
      PP21(1)=-PPL2(3)
      PP21(2)=PPL2(2)
      PP21(3)=PPL2(1)
 
      EX(1)=1
      EX(2)=0
      EX(3)=0
 
      N3EX=N2(1)*EX(1)+N2(2)*EX(2)+N2(3)*EX(3)
      DO 150 I=1,3
        UVD1(I)=EX(I)-N2(I)*N3EX
 150  CONTINUE
 
      UVA2(1)=UVB2(2)*N2(3)-UVB2(3)*N2(2)
      UVA2(2)=UVB2(3)*N2(1)-UVB2(1)*N2(3)
      UVA2(3)=UVB2(1)*N2(2)-UVB2(2)*N2(1)
 
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
      PP2(1)=COSBD2*PP21(1)-SINBD2*PP21(3)
      PP2(2)=PP21(2)
      PP2(3)=SINBD2*PP21(1)+COSBD2*PP21(3)
      IF(MSG.GE.2) WRITE(MSGFL,910) 'PP2=',PP2(1),PP2(2),PP2(3)
 
      RETURN
C**********************************************************************
 900  L=0
      RETURN
      END
