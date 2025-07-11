      SUBROUTINE BMST(T1,IRTN)
	USE BEAMCM
      IMPLICIT NONE
      INTEGER IRTN
      REAL*8 T1
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/cfqedcm.h'
      INCLUDE 'include/cnstcm.h'
      REAL*8 EPPH(0:3),TXYSPH(0:3),STOKES(3),WGTPH
      INTEGER ISPIN1,NP0,N,LEL,LEGEN,LPH,IGEN,I,J
      REAL*8 PP,FT(3),FT1,EV(3,3),E1(3),CO,SI,CO2,SI2,E11,
     %     UPSILN,PROB
      INTEGER I2(3)/2,3,1/,I3(3)/3,1,2/
      INTEGER MPMXBS
      PARAMETER (MPMXBS=100000000)
c      PARAMETER (MPMXBS=10)
c      REAL*8 PMXDAT(3,MPMXBS)
c      SAVE PMXDAT
C
      CALL CPUTIM('BMST',1)
      NP0=NP
      IF(ISPIN.GE.1.AND.LPOLBS.GE.1) THEN
        ISPIN1=1
      ELSE
        ISPIN1=0
      ENDIF
      STOKES(1)=0.0D0
      STOKES(2)=0.0D0
      STOKES(3)=0.0D0
      DO 400 N=1,NP0
        IF(KIND(N).EQ.1) GOTO 400
	  IF(LOST(N).NE.0) GOTO 400
        IF(PNAME(N).NE.'    ') GOTO 400
        CALL FTREB(EP(0,N),FLD(1,1,N),FT)
        IF(CHARGE(KIND(N)).LT.0) THEN
          DO 200 I=1,3
            FT(I)=-FT(I)
 200      CONTINUE
        ENDIF
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
        ENDIF
        CALL BMSTGN(EP(0,N),TXYS(0,N),FT1,T1,PMAXBS,WENHBS,
     %    LEL,LEGEN,LPH,
     %    EPPH,TXYSPH,WGTPH,ISPIN1,EV,SPIN(1,N),STOKES,
     %    UPSILN,PROB,IRTN,n,kind(n),isbin(n),fld(1:3,1,n))
        IF(IRTN.GE.1000) GOTO 900
        PMMBS=MAX(PMMBS,PROB)
        IF(IRTN.GE.100) THEN
          NPMXBS=NPMXBS+1
c          IF(NPMXBS.LE.MPMXBS) THEN
c            PMXDAT(1,NPMXBS)=EP(0,N)
c            PMXDAT(2,NPMXBS)=UPSILN
c           PMXDAT(3,NPMXBS)=PROB
c          ENDIF
          IF(NPMXBS.GE.MPMXBS) GOTO 910
        ENDIF
        IF(LPH.GE.2) THEN
C---- Photon radiated
C         change the basis vector for Stokes parameter
          IF(ISPIN1.GE.1) THEN
            E11=0
            DO 240 I=1,3
              E1(I)=-EV(I,3)*EV(1,3)
              IF(I.EQ.1) E1(I)=E1(I)+1D0
              E11=E11+E1(I)**2
 240        CONTINUE
            E11=1/SQRT(E11)
            CO=0
            SI=0
            DO 250 I=1,3
              E1(I)=E11*E1(I)
              CO=CO+E1(I)*EV(I,1)
              SI=SI+E1(I)*EV(I,2)
 250        CONTINUE
            CO2=CO**2-SI**2
            SI2=2*CO*SI
          ENDIF
          IGEN=GEN(N)+1
          WGTPH=WGTPH*WGT(N)
          IF(ISPIN1.GE.1) THEN
            E11=STOKES(3)
            STOKES(3)=CO2*E11+SI2*STOKES(1)
            STOKES(1)=-SI2*E11+CO2*STOKES(1)
          ENDIF
          CALL ADDONE(0,1,IGEN,'    ',ISBIN(N),WGTPH,
     %      TXYSPH(0),EPPH(0),STOKES(1),0,FLD(1,1,N),IRTN)
          IF(IRTN.NE.0) GOTO 990
        ENDIF
        GEN(N)=GEN(N)+LEGEN
 400  CONTINUE
      IRTN=0
      CALL CPUTIM('BMST',2)
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905) n,kind(n),ep(3,n),EP(0,N),UPSILN,PROB
 905  FORMAT(' (SUBR.BMST) Radiation probability in one time step',
     %  ' exceeds unity.',/,
     %  '   n=',i10,' kind= ',i3,'   pz=',1PD10.3,'eV',  
     %  '   En=',1PD9.3,'eV,  Upsilon=',1PD9.3,'  Prob=',1PD9.3)
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,912) PMAXBS
 912  FORMAT(' (SUBR.BMST) Radiation probability in one time step',
     %  ' exceeds PMAX=',0PF6.4,' too many times.')
c     %  5X,'    En(eV)     Upsilon     Prob   ')
c      WRITE(MSGFL,914) ((PMXDAT(I,J),I=1,3),J=1,MPMXBS)
c 914  FORMAT(5X,1PD12.3,1PD12.3,0PF10.5)
      RETURN
 990  IRTN=1002
      RETURN
      END

