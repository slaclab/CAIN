      SUBROUTINE BMTEQ(EP,CHARG,MASS,ANOM,EB,DT,SPIN)
C  Solution of BMT equation for short time interval
C  EP(0:3)   Energy momentum (eV,eV/c)
C  CHARG     Sign of charge
C  MASS      Mass in eV/c**2
C  ANOM      Coefficient of anomolous magnetic moment
C  EB(3,2)   Electric and magnetic field (V/m)
C  DT        Time interval (m)
      IMPLICIT NONE
      REAL*8 EP(0:3),CHARG,MASS,ANOM,EB(3,2),DT,SPIN(3)
      INTEGER I
      REAL*8 PP,BL(3),BT(3),PE(3),BL1,GAM,A1,GA1,CE1,
     %   ANG,AX(3),R(3,3)
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
C
      PP=EP(1)**2+EP(2)**2+EP(3)**2
      IF(PP.EQ.0) THEN
        DO 240 I=1,3
          BL(I)=EB(I,2)
          BT(I)=0
          PE(I)=0
 240    CONTINUE
      ELSE
        BL1=(EP(1)*EB(1,2)+EP(2)*EB(2,2)+EP(3)*EB(3,2))/PP
        DO 280 I=1,3
          BL(I)=EP(I)*BL1
          BT(I)=EB(I,2)-BL(I)
          PE(I)=(EP(I2(I))*EB(I3(I),1)-EP(I3(I))*EB(I2(I),1))/MASS
 280    CONTINUE
      ENDIF
      GAM=EP(0)/MASS
      A1=ANOM+1
      GA1=GAM*ANOM+1
      CE1=ANOM+1/(GAM+1)
      ANG=0
      DO 300 I=1,3
        AX(I)=-CHARG*(GA1*BT(I)+A1*BL(I)-CE1*PE(I))
        ANG=ANG+AX(I)**2
 300  CONTINUE
      IF(ANG.NE.0) THEN
        ANG=SQRT(ANG)
        DO 320 I=1,3
          AX(I)=-AX(I)/ANG
C            Sign inverted on May.8.2000, realizing that ROTMAT generates rotation
C            of coordinate rather than object.
 320    CONTINUE
        ANG=ANG/EP(0)*DT
        CALL ROTMAT(ANG,AX,1,3,1,3,R)
        CALL MATVEC(R,3,3,SPIN(1))
      ENDIF
      RETURN
      END
