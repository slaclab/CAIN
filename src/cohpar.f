      SUBROUTINE COHPAR(T1,IRTN)
	USE BEAMCM
      IMPLICIT NONE
      INTEGER IRTN
      REAL*8 T1
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/cfqedcm.h'
      INTEGER MPH
      PARAMETER (MPH=5)
      REAL*8 EPEP(0:3,2),TXYSEP(0:3,2),SPINEP(3,2),PROB,WGT1,
     %     PP,FT(3),FT1,FLD1(3,2),CHI,STOKES(3),EV(3,3),
     %     CO2,SI2,DT
      INTEGER ISPIN1,NP0,N,K,IGEN,KIN,IS1,LBBF,I,LPH,LPH1,LPAIR,
     %   NDIV,IDIV
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      INTEGER NCALL/0/
C
      NCALL=NCALL+1
      IF(ISPIN.GE.1.AND.LPOLCO.GE.1) THEN
        ISPIN1=1
      ELSE
        ISPIN1=0
      ENDIF
      CALL CPUTIM('COHPAR',1)
      NP0=NP
      DO 400 N=1,NP0
        IF(KIND(N).NE.1) GOTO 400
	  IF(LOST(N).NE.0) GOTO 400
        IF(PNAME(N).NE.'    ') GOTO 400
        CALL FTREB(EP(0,N),FLD(1,1,N),FT)
        FT1=SQRT(FT(1)**2+FT(2)**2+FT(3)**2)
        IF(FT1.EQ.0) GOTO 400
        IF(ISPIN1.GE.1) THEN
          PP=SQRT(EP(1,N)**2+EP(2,N)**2+EP(3,N)**2)
          DO 220 I=1,3
            EV(I,3)=EP(I,N)/PP
            EV(I,1)=FT(I)/FT1
 220      CONTINUE
          DO 230 I=1,3
            EV(I,2)=EV(I2(I),3)*EV(I3(I),1)-EV(I3(I),3)*EV(I2(I),1)
 230      CONTINUE
          CO2=(EV(1,1)**2-EV(1,2))/(1-EV(1,3)**2)
          SI2=-2*EV(1,1)*EV(1,2)/(1-EV(1,3)**2)
          STOKES(3)=CO2*SPIN(3,N)+SI2*SPIN(1,N)
          STOKES(1)=-SI2*SPIN(3,N)+CO2*SPIN(1,N)
          STOKES(2)=SPIN(2,N)
        ENDIF
        DT=T1-TXYS(0,N)
        NDIV=1
        IDIV=1
        LPH=0          
240     CALL COHPGN(EP(0,N),TXYS(0,N),FT1,DT,PMAXCO,WENHCO,
     %    LPH1,LPAIR,EPEP,TXYSEP,ISPIN1,EV,STOKES,SPINEP,CHI,PROB,IRTN)
        IF(IRTN.NE.0) THEN
	    IF(NDIV.NE.1) GOTO 900
	    NDIV=INT(PROB/PMAXCO)+1
	    DT=DT/NDIV
	    GOTO 240
	  ENDIF
        PMMCO=MAX(PMMCO,PROB)
        IF(LPH1.EQ.1) LPH=1
        IF(LPAIR.GE.1) THEN
          IGEN=GEN(N)+1
          IS1=ISBIN(N)
          WGT1=WGT(N)/WENHCO
          DO 260 I=1,3
            FLD1(I,1)=FLD(I,1,N)
            FLD1(I,2)=FLD(I,2,N)
 260      CONTINUE
C          LBBF=LBBFIN(N)
          LBBF=0
          DO 300 K=1,2
            KIN=K+1
            CALL ADDONE(0,KIN,IGEN,'    ',IS1,WGT1,
     %           TXYSEP(0,K),EPEP(0,K),SPINEP(1,K),LBBF,FLD1,IRTN)
            print *, " cohpar np= ",np," kin= ",kin,
     &           " epep(0,k)= ",epep(0,k)," epep(3,k)= ",epep(3,k)
            IF(IRTN.NE.0) GOTO 990
 300      CONTINUE
        ENDIF
        IF(LPH1.EQ.2) THEN
          LOST(N)=1
          WGT(N)=0
	    GOTO 400
        ENDIF
	IDIV=IDIV+1
        IF(IDIV.LE.NDIV) GOTO 240
	IF(LPH.GT.0) THEN
          IF(ISPIN1.GE.1) THEN
            SPIN(3,N)=CO2*STOKES(3)-SI2*STOKES(1)
            SPIN(1,N)=SI2*STOKES(3)+CO2*STOKES(1)
            SPIN(2,N)=STOKES(2)
          ENDIF
        ENDIF
 400  CONTINUE
      IRTN=0
      GOTO 1000
 900  IRTN=1000
      WRITE(MSGFL,905) prob,pmaxco
 905  FORMAT(' (SUBR.COHPAR) Algorithm of coherent pair ',
     %     'generation wrong.',/,'    Call the programmer',
     %   ' prob,pmaxco= ',2(1pe11.3))
      GOTO 1000
 990  IRTN=1001
      GOTO 1000
 1000 CALL CPUTIM('COHPAR',2)
      RETURN
      END
