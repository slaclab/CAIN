      SUBROUTINE BBKICK(T1,IS,SS)
C  Integration of equation of motion.
C  Old version. Now replaced with SLVEQM.
	USE BEAMCM
	USE BBCOM
      IMPLICIT NONE
      INTEGER IS
      REAL*8 T1,SS(2)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/bbcom.h'
      INCLUDE 'include/cfqedcm.h'
      INCLUDE 'include/cnstcm.h'
      INTEGER L,L2,N,I,NDIV,IDIV,KIN,IOUT
      REAL*8 XYS(3),XYS0(3),EP0(0:3),EP1(0:3),FXYS(3),FXYS0(3),
     %   DT,DT1,DS,SGN,EMASS2,ERXY(2),ER,
     %   BBFLD(3,2),BBFLD0(3,2),BBFL1(3,2),EXTFXY(3),FF(3),WG,
     %   ANOM1,ANOME
      EXTERNAL ANOME
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      REAL*8 ERMIN/1D-3/
C      ERMIN: maximum error in x and y in one time step in units
C             of mesh size of on-coming beam (the mesh size of the
C             co-moving beam might be undefined)
C
      CALL CPUTIM('BBKICK',1)
      DS=SS(2)-SS(1)
      EMASS2=EMASS**2
      DO 400 N=1,NP
        IF(ISBIN(N).NE.IS) GOTO 400
        IF(LBBFIN(N).NE.0) GOTO 400
        IF(EP(3,N).GE.0) THEN
          L=1
          SGN=1
        ELSE
          L=2
          SGN=-1
        ENDIF
        L2=3-L
        IF(BBFLG(L2).EQ.0) GOTO 400
        KIN=KIND(N)
        IF(KIN.EQ.1) THEN
          IF(LCOHP.LE.0) GOTO 400
C    Beam field on photon for coherent-pair calculation
          DT=T1-TXYS(0,N)
          DO 160 I=1,3
            XYS(I)=TXYS(I,N)+EP(I,N)/EP(0,N)*DT
 160      CONTINUE
          CALL BBKICK0(L,DS,1,TXYS(1,N),EP(0,N),BBFLD0,FF,IOUT,
     %                 NXY,BBQ)
          CALL BBKICK0(L,DS,1,XYS,EP(0,N),BBFLD,FF,IOUT,NXY,BBQ)
          DO 170 I=1,3
            FLD(I,1,N)=FLD(I,1,N)+0.5D0*(BBFLD0(I,1)+BBFLD(I,1))
            FLD(I,2,N)=FLD(I,2,N)+0.5D0*(BBFLD0(I,2)+BBFLD(I,2))
 170      CONTINUE
          GOTO 390
        ENDIF
        DT=T1-TXYS(0,N)
        DO 180 I=1,3
          XYS0(I)=TXYS(I,N)
 180    CONTINUE
        DO 190 I=0,3
          EP0(I)=EP(I,N)
 190    CONTINUE
        DO 200 I=1,3
          EXTFXY(I)=CHARGE(KIN)*(FLD(I,1,N)
     %      +(EP0(I2(I))*FLD(I3(I),2,N)
     %       -EP0(I3(I))*FLD(I2(I),2,N))/EP0(0))
 200    CONTINUE
C-- Determine time step size
        CALL BBKICK0(L,DS,KIN,XYS0,EP0,BBFLD0,FF,IOUT,NXY,BBQ)
        DO 210 I=1,3
          FXYS0(I)=EXTFXY(I)+FF(I)
          EP1(I)=EP0(I)+FXYS0(I)*DT
 210    CONTINUE
        EP1(0)=SQRT(EMASS2+EP1(1)**2+EP1(2)**2+EP1(3)**2)
        DO 215 I=1,3
          XYS(I)=XYS0(I)+(EP0(I)/EP0(0)+EP1(I)/EP1(0))/2*DT
 215    CONTINUE
        CALL BBKICK0(L,DS,KIN,XYS,EP1,BBFLD,FF,IOUT,NXY,BBQ)
        DO 220 I=1,3
          FXYS(I)=EXTFXY(I)+FF(I)
 220    CONTINUE
        DO 225 I=1,2
          ERXY(I)=((FXYS(I)-FXYS0(I))/EP1(0)*DT**2/4)/BBDXY(I,L2)
 225    CONTINUE
        ER=MAX(ABS(ERXY(1)),ABS(ERXY(2)))
C          Error in one step is proportional to 1/NDIV**3
C          Total error is to 1/NDIV**2
        NDIV=MAX(1,NINT(SQRT(ER/ERMIN)))
        IF(NDIV.EQ.1) THEN
C-- One step is enough
          DO 230 I=1,3
            BBFL1(I,1)=0.5D0*(BBFLD0(I,1)+BBFLD(I,1))
            BBFL1(I,2)=0.5D0*(BBFLD0(I,2)+BBFLD(I,2))
            EP1(I)=EP0(I)+(FXYS0(I)+FXYS(I))/2*DT
 230      CONTINUE
          EP1(0)=SQRT(EMASS2+EP1(1)**2+EP1(2)**2+EP1(3)**2)
          DO 235 I=1,3
            XYS(I)=XYS0(I)+(EP0(I)/EP0(0)+EP1(I)/EP1(0))/2*DT
 235      CONTINUE
        ELSE
C-- NDIV steps
          IF(NDIV.GT.25)
     %       NDIV=MIN(200,INT(5D0*SQRT(DFLOAT(NDIV))))
C            Prevent too large NDIV for computing time,
C            assuming very low energy particles do not need
C            high accuracy.
          DT1=DT/NDIV
          DO 300 IDIV=1,NDIV
            WG=1
            IF(IDIV.EQ.NDIV) WG=0.5D0
            IF(IDIV.NE.1) THEN
              DO 240 I=0,3
                EP0(I)=EP1(I)
 240          CONTINUE
            ENDIF
            DO 245 I=1,2
              XYS(I)=XYS0(I)+EP0(I)/EP0(0)*DT1+FXYS0(I)/EP0(0)*DT1**2/2
 245        CONTINUE
            CALL BBKICK0(L,DS,KIN,XYS,EP0,BBFLD,FF,IOUT,NXY,BBQ)
            DO 250 I=1,3
              FXYS(I)=EXTFXY(I)+FF(I)
              BBFL1(I,1)=BBFL1(I,1)+WG*BBFLD(I,1)
              BBFL1(I,2)=BBFL1(I,2)+WG*BBFLD(I,2)
 250        CONTINUE
            DO 260 I=1,3
              EP1(I)=EP0(I)+(FXYS0(I)+FXYS(I))/2*DT1
 260        CONTINUE
            EP1(0)=SQRT(EMASS2+EP1(1)**2+EP1(2)**2+EP1(3)**2)
            DO 265 I=1,3
              XYS(I)=XYS0(I)+(EP0(I)/EP0(0)+EP1(I)/EP1(0))/2*DT1
              XYS0(I)=XYS(I)
              FXYS0(I)=FXYS(I)
 265        CONTINUE
 300      CONTINUE
        ENDIF
        DO 320 I=1,3
          FLD(I,1,N)=FLD(I,1,N)+BBFL1(I,1)/NDIV
          FLD(I,2,N)=FLD(I,2,N)+BBFL1(I,2)/NDIV
 320    CONTINUE
        IF(ISPIN.GE.1) THEN
          ANOM1=ANOME(FLD(1,1,N),EP(0,N),2)
          CALL BMTEQ(EP(0,N),CHARGE(KIN),MASS(KIN),ANOM1,
     %       FLD(1,1,N),DT,SPIN(1,N))
        ENDIF
C-- Store new variables. 
C    Extrapolate back to the initial time by a straight line.
C    Effects of the external field have already taken into account
C    so that only straight line extrapolation is enough at ENDPUSH
C    for particles with LBBFIN=1.
        DO 340 I=0,3
          EP(I,N)=EP1(I)
 340    CONTINUE
        DO 360 I=1,3
          TXYS(I,N)=XYS(I)-DT*EP(I,N)/EP(0,N)
 360    CONTINUE
C--
 390    LBBFIN(N)=1
 400  CONTINUE
      CALL CPUTIM('BBKICK',2)
      RETURN
      END

