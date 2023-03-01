      SUBROUTINE EXFSPN(SPIN,LLLE,LLLB,EB1,MAT,OM,TAU)
C Solve BMT equation under constant field
      IMPLICIT NONE
      LOGICAL LLLE,LLLB
      REAL*8 SPIN(3),EB1(3,2),MAT(0:3,2,2),OM(2),TAU
      INTEGER I,N,NDIV
      REAL*8 BG(0:3),DA(2,2),AX0(3),AX(3),ANG0,ANG,ANG1,ERR,SGN,
     %    DTAU,R(3,3),ANOM
      REAL*8 ANOME
      EXTERNAL ANOME
      REAL*8 EPS/1D-10/
C
      IF(TAU.EQ.0) RETURN
      DO 200 I=0,3
        BG(I)=MAT(I,1,1)+MAT(I,1,2)
 200  CONTINUE
C       Anomalous magnetic moment is a function of Upsilon.
C       Upsilon does not change along the trajectory in a
C       constant field.
      ANOM=ANOME(EB1,BG,0)
      CALL EXFSPX(BG,LLLE,LLLB,EB1,ANG0,AX0,ANOM)
      SGN=1
      IF(TAU.LT.0) SGN=-1
      DTAU=ABS(TAU)
      IF(ANG0.NE.0) DTAU=MIN(DTAU,1/ANG0)
      DTAU=DTAU*SGN
      CALL EXTFDA(OM,DTAU,DA)
      DO 220 I=0,3
        BG(I)=MAT(I,1,1)*DA(1,1)+MAT(I,2,1)*DA(2,1)
     %       +MAT(I,1,2)*DA(1,2)+MAT(I,2,2)*DA(2,2)
 220  CONTINUE
      CALL EXFSPX(BG,LLLE,LLLB,EB1,ANG,AX,ANOM)
      ERR=1-(AX(1)*AX0(1)+AX(2)*AX0(2)+AX(3)*AX0(3))**2
C        some problem in this criterion. think later.
      ERR=SQRT(MAX(0D0,ERR))*(DTAU)**2/4/EPS
      IF(ERR.GE.1) THEN
        DTAU=DTAU/ERR
      ENDIF
      IF(DTAU.EQ.TAU) THEN
        CALL ROTMAT(DTAU*ANG0/2,AX0,1,3,1,3,R)
        CALL MATVEC(R,3,3,SPIN)
        CALL ROTMAT(DTAU*ANG/2,AX,1,3,1,3,R)
        CALL MATVEC(R,3,3,SPIN)
      ELSE
        NDIV=TAU/DTAU
        DTAU=TAU/NDIV 
        CALL ROTMAT(DTAU*ANG0/2,AX0,1,3,1,3,R)
        CALL MATVEC(R,3,3,SPIN)
        DO 300 N=1,NDIV
          CALL EXTFDA(OM,DTAU*N,DA)
          DO 240 I=0,3
            BG(I)=MAT(I,1,1)*DA(1,1)+MAT(I,2,1)*DA(2,1)
     %           +MAT(I,1,2)*DA(1,2)+MAT(I,2,2)*DA(2,2)
 240      CONTINUE
          CALL EXFSPX(BG,LLLE,LLLB,EB1,ANG,AX,ANOM)
          ANG1=ANG*DTAU
          IF(N.EQ.NDIV) ANG1=ANG1/2
          CALL ROTMAT(ANG1,AX,1,3,1,3,R)
          CALL MATVEC(R,3,3,SPIN)
          ANG0=ANG
          DO 260 I=1,3
            AX0(I)=AX(I)
 260      CONTINUE
 300    CONTINUE
      ENDIF
      RETURN
      END
C-------------------------------------------------------------------------
      SUBROUTINE EXFSPX(BG,LLLE,LLLB,EB1,ANG,AX,ANOM)
      IMPLICIT NONE
      LOGICAL LLLE,LLLB
      REAL*8 BG(0:3),EB1(3,2),ANG,AX(3),ANOM
      INTEGER I
      REAL*8 EV(3),SUM,EVDOTB,BT(3),BL(3),GA1,ANOM1,C1
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
C
      DO 200 I=1,3
        AX(I)=0
 200  CONTINUE
      ANOM1=ANOM+1
      IF(LLLB) THEN
        SUM=BG(1)**2+BG(2)**2+BG(3)**2
        IF(SUM.NE.0) THEN
          SUM=SQRT(SUM)
          EVDOTB=0
          DO 220 I=1,3
            EV(I)=BG(I)/SUM
            EVDOTB=EV(I)*EB1(I,2)
 220      CONTINUE
          DO 240 I=1,3
            BL(I)=EVDOTB*EV(I)
            BT(I)=EB1(I,2)-BL(I)
 240      CONTINUE
          GA1=ANOM*BG(0)+1
          DO 260 I=1,3
            AX(I)=AX(I)-GA1*BT(I)-ANOM1*BL(I)
 260      CONTINUE
        ELSE
          DO 280 I=1,3
            AX(I)=AX(I)-ANOM1*EB1(I,2)
 280      CONTINUE
        ENDIF
      ENDIF
      IF(LLLE) THEN
        C1=ANOM+1/(BG(0)+1)
        DO 300 I=1,3
          AX(I)=AX(I)+C1*(BG(I2(I))*EB1(I3(I),1)
     %                   -BG(I3(I))*EB1(I2(I),1))
 300    CONTINUE
      ENDIF
      ANG=AX(1)**2+AX(2)**2+AX(3)**2
      IF(ANG.NE.0) THEN
        ANG=SQRT(ANG)
        DO 320 I=1,3
          AX(I)=AX(I)/ANG
 320    CONTINUE
      ENDIF
      RETURN
      END
