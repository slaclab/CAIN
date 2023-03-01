      SUBROUTINE LORNTZ(NTRANS,ITRANS,TXYS0,BG,EV,ANGLE,AXIS,
     %   N1,N20,LR,KIN)
C  Lorentz transformation
C  NTRANS: Number of transformation (<=3)
C  ITRANS(i) (i=1,NTRANS):  Specifies in which order to transform.
C          1: shift of origin
C          2: rotation of coordinate axis
C          3: Lorentz boost
C       At most once for each transformation.
C  TXYS0   Origin shift
C  BG      Beta*Gamma of Lorentz boost (coordinate boost)
C  EV      Unit vector along the boost direction
C  ANGLE   Rotation angle
C  AXIS    Unit vector along the rotation axis.
C  N1,N20   Applies from particle #N1 to #N20.
C           (If N20=0, N20=NP is adopted)
C  LR(2),KIN(3)  Flag for right/left and particle species
C          to be transformed.
C ++++ transformation of Stokes parameter not ready ++++
	USE BEAMCM
      IMPLICIT NONE
      INTEGER NTRANS,ITRANS(NTRANS),N1,N20,LR(2),KIN(3)
      REAL*8 TXYS0(0:3),BG,EV(3),ANGLE,AXIS(3)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER I,J,N,N2,II,IFLRKN
      REAL*8 T(0:3,0:3),R(3,3),GAM,GAMX,BET,
     %   V(3),V1(3),BG1,GAM1,GAM2,AX2(3),ANG2,SUM,
     %   E1(3),E2(3),E3(3),E1NEW(3),CO,SI,CO2,SI2
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/,ITYPE(3)/1,1,1/
C
      IF(N20.EQ.0) THEN
        N2=NP
      ELSE
        N2=N20
      ENDIF
      IF(N2-N1.LT.0) RETURN
      CALL CPUTIM('LORNTZ',1)
      DO 600 II=1,NTRANS
        GOTO (300,340,400), ITRANS(II)
        GOTO 600
C--- Translation
 300    DO 330 N=N1,N2
          IF(IFLRKN(N,LR,KIN,0,0,0,ITYPE).EQ.0) GOTO 330
          DO 320 I=0,3
            TXYS(I,N)=TXYS(I,N)-TXYS0(I)
 320      CONTINUE
 330    CONTINUE
        GOTO 560
C--- Rotation
 340    CALL ROTMAT(ANGLE,AXIS,1,3,1,3,R)
        DO 380 N=N1,N2
          IF(IFLRKN(N,LR,KIN,0,0,0,ITYPE).EQ.0) GOTO 380
          CALL MATVEC(R,3,3,TXYS(1,N))
          CALL MATVEC(R,3,3,EP(1,N))
          IF(ISPIN.EQ.0) GOTO 380
          IF(KIND(N).NE.1) THEN
            CALL MATVEC(R,3,3,SPIN(1,N))
          ELSE
C             Photon Stokes parameter changes because of the
C             basis vector convention:
C              E1= Xvector - (Xvector*V)V 
            SUM=0
            DO 350 I=1,3
              E3(I)=EP(I,N)/EP(0,N)
              SUM=R(I,1)*E3(I)
              E1(I)=0
 350        CONTINUE
            E1(1)=1
            DO 360 I=1,3
              E1NEW(I)=R(I,1)-E3(I)*SUM
              E1(I)=E1(I)-E3(1)*E3(I)
 360        CONTINUE
C                E1NEW: new X-axis seen in the old frame.
            CO=0
            SI=0
            DO 370 I=1,3
              E2(I)=E3(I2(I))*E1(I3(I))-E3(I3(I))*E1(I2(I))
              CO=CO+E1NEW(I)*E1(I)
              SI=SI+E1NEW(I)*E2(I)
 370        CONTINUE
            SUM=CO**2+SI**2
            CO2=(CO**2-SI**2)/SUM
            SI2=2*CO*SI/SUM
            SUM=SPIN(3,N)
            SPIN(3,N)=CO2*SUM+SI2*SPIN(1,N)
            SPIN(1,N)=-SI2*SUM+CO2*SPIN(1,N)
          ENDIF
 380    CONTINUE
        GOTO 560
C--- Lorentz boost
 400    GAM=SQRT(1+BG**2)
        GAMX=BG**2/(GAM+1)
        BET=BG/GAM
        T(0,0)=GAM
        DO 420 I=1,3
          T(0,I)=-BG*EV(I)
          T(I,0)=T(0,I)
          DO 410 J=1,3
            T(I,J)=GAMX*EV(I)*EV(J)
            IF(I.EQ.J) T(I,J)=T(I,J)+1
 410      CONTINUE
 420    CONTINUE
        DO 520 N=N1,N2
          IF(IFLRKN(N,LR,KIN,0,0,0,ITYPE).EQ.0) GOTO 520
          IF(ISPIN.NE.0) THEN
            DO 430 I=1,3
              V1(I)=-EP(I,N)/EP(0,N)
 430        CONTINUE
            IF(KIND(N).NE.1) THEN
              BG1=SQRT(EP(1,N)**2+EP(2,N)**2+EP(3,N)**2)
     %               /MASS(KIND(N))
              GAM1=SQRT(BG1**2+1)
            ENDIF
          ENDIF
          CALL MATVEC(T,4,4,TXYS(0,N))
          CALL MATVEC(T,4,4,EP(0,N))
          IF(ISPIN.EQ.0) GOTO 520
          IF(KIND(N).NE.1) THEN
C             Thomas Precession
            DO 440 I=1,3
              V(I)=EV(I)*BET
 440        CONTINUE
            AX2(1)=V(2)*V1(3)-V(3)*V1(2)
            AX2(2)=V(3)*V1(1)-V(1)*V1(3)
            AX2(3)=V(1)*V1(2)-V(2)*V1(1)
            SUM=SQRT(AX2(1)**2+AX2(2)**2+AX2(3)**2)
            DO 450 I=1,3
              AX2(I)=AX2(I)/SUM
 450        CONTINUE
            GAM2=GAM*GAM1*(1+V(1)*V1(1)+V(2)*V1(2)+V(3)*V1(3))
            ANG2=2*ATAN2(GAM*GAM1*SUM,1+GAM+GAM1+GAM2)
            CALL ROTMAT(ANG2,AX2,1,3,1,3,R)
            CALL MATVEC(R,3,3,SPIN(1,N))
          ELSE
            DO 460 I=1,3
              E1(I)=-V1(1)*V1(I)
              E1NEW(I)=EV(I2(I))*V1(I3(I))-EV(I3(I))*V1(I2(I))
 460        CONTINUE
            E1(1)=1+E1(1)
            CO=0
            SI=0
            DO 470 I=1,3
              E2(I)=V1(I2(I))*E1(I3(I))-V1(I3(I))*E1(I2(I))
              CO=CO+E1NEW(I)*E1(I)
              SI=SI+E1NEW(I)*E2(I)
 470        CONTINUE
            SUM=CO**2+SI**2
            CO2=(CO**2-SI**2)/SUM
            SI2=2*CO*SI/SUM
            SUM=SPIN(3,N)
            SPIN(3,N)=CO2*SUM+SI2*SPIN(1,N)
            SPIN(1,N)=-SI2*SUM+CO2*SPIN(1,N)
            DO 480 I=1,3
              V1(I)=EP(I,N)/EP(0,N)
              E1(I)=-V1(1)*V1(I)
 480        CONTINUE
            E1(1)=1+E1(1)
            CO=0
            SI=0
            DO 490 I=1,3
              E2(I)=V1(I2(I))*E1(I3(I))-V1(I3(I))*E1(I2(I))
              CO=CO+E1NEW(I)*E1(I)
              SI=SI+E1NEW(I)*E2(I)
 490        CONTINUE
            SUM=CO**2+SI**2
            CO2=(CO**2-SI**2)/SUM
            SI2=2*CO*SI/SUM
            SUM=SPIN(3,N)
            SPIN(3,N)=CO2*SUM-SI2*SPIN(1,N)
            SPIN(1,N)=SI2*SUM+CO2*SPIN(1,N)
          ENDIF
 520    CONTINUE
C
 560    CALL EXFLRZ(ITRANS(II),TXYS0,R,GAM,BET,EV,T)
 600  CONTINUE
      CALL CPUTIM('LORNTZ',2)
      RETURN
      END
