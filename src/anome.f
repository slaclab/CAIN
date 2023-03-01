      FUNCTION ANOME(EB,EP,L)
C  Electron anomalous magnetic moment in a field.
C  Input
C    EB(3,2)   EB(*,1)=electric field
C              EB(*,2)=magnetic field * velocity of light
C    EP(0:3)   Energy/c-momentum
C    L         0,1,2: specifies the unit system for EB and EP.
C              0: when EB in 1/m (multiplied by e/m) and
C                 EP in mc.
C              1: when EB in V/m and EP in mc, or when
C                 EB in 1/m and EP in eV/c
C              2: when EB in V/m and EP in eV/c.
C  Output
C    ANOME     Coefficient of anomalous magnetic moment
C              ( = alpha/(2*Pi) + O(alpha^2) at low field.)
C
      IMPLICIT NONE
      INTEGER L
      REAL*8 ANOME,EB(3,2),EP(0:3)
      INTEGER I
      REAL*8 PSQ,EE,ELSQ,FT,FTSQ,UPS,ANOME0
      EXTERNAL ANOME0
      INTEGER I1(3)/2,3,1/,I2(3)/3,1,2/
      REAL*8 EMASS/0.51099906D6/,CMPTNWL/3.86159323D-13/
C
      PSQ=EP(1)**2+EP(2)**2+EP(3)**2
      EE=(EB(1,1)*EP(1)+EB(2,1)*EP(2)+EB(3,1)*EP(3))/PSQ
      ELSQ=EE**2*PSQ
      FTSQ=0
      DO 200 I=1,3
        FT=EP(0)*(EB(I,1)-EE*EP(I))
     %     +(EP(I1(I))*EB(I2(I),2)-EP(I2(I))*EB(I1(I),2))
        FTSQ=FTSQ+FT**2
 200  CONTINUE
      UPS=CMPTNWL*SQRT(FTSQ+ELSQ)
      IF(L.NE.0) UPS=UPS/EMASS**L
      ANOME=ANOME0(UPS)
      RETURN
      END
C--------------------------------------------------------------------
      FUNCTION ANOME0(UPS)
C  Coefficient of anomalous magnetic moment of electron as a function
C  of upsilon.
C   a = alpha/(2*pi)*F(U) + a2
C            2             x*dx                   x      t**3 
C     F(U)= --- integral -------- * integral sin[---(t + ----)]*dt
C            U           (1+x)**3                 U       3     
C     F(0)=1
C     a2 is the correction of O[(alpha)**2]
C  Since the upsilon dependence of a2 is not known, this routine gives
C      ANOME0 = [alpha/(2*pi) + a2]*F(U)
C  where the quantity in [ ] is known as the coefficient at upsilon-->0.
C  Accuracy:  relative error of F is < 0.8E-5 for any upsilon.
      IMPLICIT NONE
      REAL*8 ANOME0,UPS
      REAL*8 UPS0/0.6125D0/,ANOM00/0.001159652193D0/,
     % A1/12.190054896D0/,A2/24.159295870D0/,A3/-37.341016656D0/,
     % A4/-190.56332408D0/,A5/-267.06921477D0/,A6/-80.540475512D0/,
     % A7/-95.539356489D0/,A8/246.29207314D0/,
     % B1/0.51911469393D0/,B2/0.75556983816D0/,B3/-0.98346938317D0/,
     % B4/0.31707943752D0/,B5/-1.5451047715D0/,B6/0.67601308567D0/,
     % B7/-0.061924565451D0/,B8/-0.23548134968D0/
      REAL*8 U2,U3,U4,UL
C
      IF(UPS.LE.0) THEN
        ANOME0=1
      ELSEIF(UPS.LE.UPS0) THEN
        UL=LOG(UPS)
        ANOME0=1+UPS**2*(A1*UL+A2+UPS*(A3*UL+A4
     %                  +UPS*(A5*UL+A6+UPS*(A7*UL+A8))))
      ELSE
        U2=UPS**(-2)
        U3=U2**(1D0/3D0)
        U4=U3**2
        UL=LOG(UPS)
        ANOME0=B1*U3+B2*U4+(B3*UL+B4)*U2+(B5*U3+B6*U4+(B7*UL+B8)*U2)*U2
      ENDIF
      ANOME0=ANOM00*ANOME0
      RETURN
      END
