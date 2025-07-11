      SUBROUTINE RNGKTT(XYS0,EP0,DT,EXTFXY,
     %       BBFLAV,ERMIN,DXY,L,DS,KIN,NXY,BBQ,IRTN,is)
      IMPLICIT NONE
      INTEGER L,KIN,NXY(2),IRTN,is
      REAL*8 XYS0(3),EP0(0:3),DT,EXTFXY(3),
     %       BBFLAV(3,2),ERMIN,DXY(2),DS,BBQ(*)
      INTEGER NDIV,IDIV,JJ,I,K,IT,IOUT
      REAL*8 DT1,WG,XYS00(3),EP00(0:3),XYS1(3),EP1(0:3),
     %    XYS2(3,2),EP2(0:3,2),FXYS1(3),FF(3),
     %    BBFLD(3,2),ERMIN1,ER
      REAL*8 RTW(4)/0.166666666666666667D0,0.333333333333333333D0,
     %     0.333333333333333333D0,0.166666666666666667D0/
      REAL*8 RTD(4)/0D0,0.5D0,0.5D0,1D0/
      REAL*8 XYSN(3,4),PXYSN(3,4)
      REAL*8 EMASS2/0.2611200393D12/
C
      IRTN=0
      DO 180 I=1,3
        XYS00(I)=XYS0(I)
        EP00(I)=EP0(I)
 180  CONTINUE
      EP00(0)=EP0(0)
      ERMIN1=16*ERMIN
      IT=-1
 200  IT=IT+1
      JJ=MOD(IT,2)+1
      NDIV=2**IT
      DT1=DT/NDIV
      DO 210 I=1,3
        BBFLAV(I,1)=0
        BBFLAV(I,2)=0
        XYS0(I)=XYS00(I)
        EP0(I)=EP00(I)
 210  CONTINUE
      EP0(0)=EP00(0)
      DO 400 IDIV=1,NDIV
        DO 220 I=1,3
          XYS2(I,JJ)=XYS0(I)
          EP2(I,JJ)=EP0(I)
          XYS1(I)=XYS0(I)
          EP1(I)=EP0(I)
 220    CONTINUE
        EP1(0)=EP0(0)
        DO 300 K=1,4
          IF(K.NE.1) THEN
            DO 240 I=1,3
              XYS1(I)=XYS0(I)+RTD(K)*XYSN(I,K-1)
              EP1(I)=EP0(I)+RTD(K)*PXYSN(I,K-1)
 240        CONTINUE
            EP1(0)=SQRT(EMASS2+EP1(1)**2+EP1(2)**2+EP1(3)**2)
          ENDIF
          CALL BBKICK0(L,DS,KIN,XYS1,EP1,BBFLD,FF,IOUT,NXY,BBQ,is)
          DO 250 I=1,3
            FXYS1(I)=EXTFXY(I)+FF(I)
 250      CONTINUE
          DO 260 I=1,3
            XYSN(I,K)=DT1*EP1(I)/EP1(0)
            PXYSN(I,K)=DT1*FXYS1(I)
            XYS2(I,JJ)=XYS2(I,JJ)+RTW(K)*XYSN(I,K)
            EP2(I,JJ)=EP2(I,JJ)+RTW(K)*PXYSN(I,K)
 260      CONTINUE
          IF(K.EQ.1.OR.(IDIV.EQ.NDIV.AND.K.EQ.4)) THEN
            IF(IDIV.EQ.1.OR.K.EQ.4) THEN
              WG=1D0/DFLOAT(2*NDIV)
            ELSE
              WG=1D0/DFLOAT(NDIV)
            ENDIF
            DO 270 I=1,3
              BBFLAV(I,1)=BBFLAV(I,1)+WG*BBFLD(I,1)
              BBFLAV(I,2)=BBFLAV(I,2)+WG*BBFLD(I,2)
 270        CONTINUE
          ENDIF
 300    CONTINUE
        EP2(0,JJ)=SQRT(EMASS2+EP2(1,JJ)**2+EP2(2,JJ)**2+EP2(3,JJ)**2)
        DO 320 I=1,3
          XYS0(I)=XYS2(I,JJ)
          EP0(I)=EP2(I,JJ)
 320    CONTINUE
        EP0(0)=EP2(0,JJ)
 400  CONTINUE
      IF(IT.EQ.0) GOTO 200
      ER=MAX(ABS(XYS2(1,1)-XYS2(1,2))/DXY(1),
     %       ABS(XYS2(2,1)-XYS2(2,2))/DXY(2))
      IF(ER.GE.ERMIN1) THEN
        IF(IT.GE.6) THEN
          IRTN=100
        ELSE
          GOTO 200
        ENDIF
      ENDIF
      RETURN
      END

