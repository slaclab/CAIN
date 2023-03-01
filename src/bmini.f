      SUBROUTINE BMINI(LR,KIND1,NP1,AN,TXYS0,LDIST,BETA,ALPHA,
     %  EMIT,E0,SIGE,SIGT,SLOPE,CRAB,GCUT,IGWGT,ETA,ETAP,ESLOPE,
     %  XYROLL,DALPHADE,DALPHADT,SPIN1,IRTN)
	USE BEAMCM
      IMPLICIT NONE
      INTEGER LR,KIND1,NP1,LDIST(3),IGWGT,IRTN
      REAL*8 AN,TXYS0(0:3),BETA(2),ALPHA(2),EMIT(2),
     %  E0,SIGE,SIGT,SLOPE(2),CRAB(2),GCUT(4),ETA(2),ETAP(2),
     %  ESLOPE,XYROLL,DALPHADE(2),DALPHADT(2),SPIN1(3)
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/nestcm.h'
      INCLUDE 'include/pushcm.h'
      INCLUDE 'include/cnstcm.h'
      INCLUDE 'include/ctrlcm.h'
      INTEGER I,N,N0
      INTEGER LR2(2)/1,1/,KIN2(3)/1,1,1/
      REAL*8 DERFC
      REAL*8 SIG(2),R,TH,SGN,CO,SI,PTOT,WGT1,SAVE,ALPHA1,DALPHA1,BETA1
      REAL*8 PI/3.14159 26535 89793 238D0/,
     %       SQRT3/1.73205080D0/,SQRT2/1.41421356D0/
C
      CALL CPUTIM('BMINI',1)
      IF(KIND1.LE.0.OR.KIND1.GE.4) GOTO 900
      IF(LR.LE.0.OR.LR.GE.3) GOTO 910
      IF(E0.LE.MASS(KIND1)) GOTO 920
      IF(NP+NP1.GT.MP-MPNEW) GOTO 930
      N0=NP+1
      NP=NP+NP1
      DO I=1,2
        IF(BETA(I).EQ.0) GOTO 940
        SIG(I)=SQRT(EMIT(I)*BETA(I))
      ENDDO
      SGN=1
      IF(LR.EQ.2) SGN=-1
C--- Time distribution
      IF(SIGT.EQ.0) THEN
        DO N=N0,NP
          TXYS(0,N)=0
        ENDDO
      ELSE
        IF(LDIST(2).LE.0) THEN
	print *,'  N0=',n0,'  NP=',np,'  NP1=',np1
          CALL DGAUSC(WORK(N0:NP),NP1,GCUT(3))
        ELSE
          CALL RANDN(WORK(N0:NP),NP1)
          DO N=N0,NP
            WORK(N)=(2*WORK(N)-1)*SQRT3
          ENDDO
        ENDIF
        DO N=N0,NP
          TXYS(0,N)=SIGT*WORK(N)
        ENDDO
      ENDIF
C--- Energy distribution
      IF(SIGE.EQ.0) THEN
        DO N=N0,NP
          EP(0,N)=0
        ENDDO
      ELSE
        IF(LDIST(3).LE.0) THEN
          CALL DGAUSC(WORK(N0:NP),NP1,GCUT(4))
        ELSE
          CALL RANDN(WORK(N0:NP),NP1)
          DO N=N0,NP
            WORK(N)=(2*WORK(N)-1)*SQRT3
          ENDDO
        ENDIF
        DO N=N0,NP
          EP(0,N)=SIGE*WORK(N)+ESLOPE*TXYS(0,N)
        ENDDO
      ENDIF
	IF(ESLOPE.NE.0) THEN
	  DO N=N0,NP
	    EP(0,N)=EP(0,N)+ESLOPE*TXYS(0,N)
	  ENDDO
	ENDIF
C            At this point EP(0,N) is (E-E0)/E0
C--- Betatron distribution
      IF(LDIST(1).LE.0) THEN
        DO I=1,2
          CALL EXPDIS(WORK(N0:NP),NP1,GCUT(I))
		  CALL RANDN(WORK(N0+MP:NP+MP),NP1)
          DO N=N0,NP
	      DALPHA1=EP(0,N)*DALPHADE(I)+TXYS(0,N)*DALPHADT(I)
	      ALPHA1=ALPHA(I)+DALPHA1
	      BETA1=BETA(I)*(1+DALPHA1**2)
		  WORK(N)=SQRT(EMIT(I)*BETA1)*SQRT(2*WORK(N))
            TH=2*PI*WORK(N+MP)
            CO=COS(TH)
            TXYS(I,N)=WORK(N)*CO
            EP(I,N)=-(ALPHA1*TXYS(I,N)+WORK(N)*SIN(TH))/BETA1
          ENDDO
        ENDDO
      ELSE
        CALL RANDN(WORK(N0:NP),NP1)
        CALL RANDN(WORK(N0+MP:NP+MP),NP1)
        DO N=N0,NP
          R=SQRT(WORK(N))
          TH=2*PI*WORK(N+MP)
          TXYS(1,N)=2*SIG(1)*R*COS(TH)
          TXYS(2,N)=2*SIG(1)*R*SIN(TH)
          EP(1,N)=0
          EP(2,N)=0
        ENDDO
      ENDIF
C--- Eta function
      DO I=1,2
        DO N=N0,NP
          TXYS(I,N)=TXYS(I,N)+ETA(I)*EP(0,N)
          EP(I,N)=EP(I,N)+ETAP(I)*EP(0,N)
        ENDDO
      ENDDO
C--- Roll in x-y plane
      IF(XYROLL.NE.0) THEN
        CO=COS(XYROLL)
        SI=SIN(XYROLL)
        DO N=N0,NP
          SAVE=TXYS(1,N)
          TXYS(1,N)=CO*SAVE-SI*TXYS(2,N)
          TXYS(2,N)=SI*SAVE+CO*TXYS(2,N)
          SAVE=EP(1,N)
          EP(1,N)=CO*SAVE-SI*EP(2,N)
          EP(2,N)=SI*SAVE+CO*EP(2,N)
        ENDDO
      ENDIF
C--- Origin
      DO N=N0,NP
        DO I=1,2
          TXYS(I,N)=TXYS0(I)+TXYS(I,N)+CRAB(I)*TXYS(0,N)
        ENDDO
        TXYS(0,N)=TXYS0(0)+TXYS(0,N)
C        TXYS(3,N)=TXYS0(3)+TXYS(3,N)   Jan.24.2000 replaced
        TXYS(3,N)=TXYS0(3)
        EP(0,N)=E0+E0*EP(0,N)
        PTOT=EP(0,N)**2-MASS(KIND1)**2
        IF(PTOT.LE.0) GOTO 950
        DO I=1,2
          EP(I,N)=SLOPE(I)+EP(I,N)
        ENDDO
        EP(3,N)=SQRT(PTOT/(1+EP(1,N)**2+EP(2,N)**2))
        DO I=1,2
          EP(I,N)=EP(I,N)*EP(3,N)
        ENDDO
        EP(3,N)=SGN*EP(3,N)
      ENDDO
C
      WGT1=AN/NP1
      IF(IGWGT.EQ.0) THEN
        IF(LDIST(1).LE.0)
     %    WGT1=WGT1*(1-EXP(-GCUT(1)**2/2D0))*(1-EXP(-GCUT(2)**2/2D0))
        IF(LDIST(2).LE.0) WGT1=WGT1*(1-DERFC(GCUT(3)/SQRT2))
      ENDIF
      DO N=N0,NP
        KIND(N)=KIND1
        GEN(N)=1
        PNAME(N)='    '
	  LOST(N)=0
        WGT(N)=WGT1
        DO I=1,3
          SPIN(I,N)=SPIN1(I)
        ENDDO
      ENDDO
      IF(MSGLVL.GE.1) 
     %  CALL BMINI1(LR,KIND1,NP1,AN,TXYS0,LDIST,BETA,ALPHA,
     %  EMIT,SIG,E0,SIGE,SIGT,SLOPE,CRAB,GCUT,WGT1,ETA,ETAP,ESLOPE,
     %  XYROLL,DALPHADE,DALPHADT,SPIN1,MSGFL)
      IF(INPUSH.GE.1) THEN
        CALL DRIFT0(1,TIMNOW,NP-NP1+1,0,LR2,KIN2)
        IF(MSGLVL.GE.1) WRITE(MSGFL,520) TIMNOW
 520    FORMAT(' +++ New particles pushed to Time=',1PD13.6,' m.')
      ENDIF
      IRTN=0
      GOTO 1000
C
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.BMINI) Particle "KIND" must be ',
     %    '1 or 2 or 3.')
      GOTO 1000
 910  IRTN=1001
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.BMINI) RIGHT/LEFT not specified.')
      GOTO 1000
 920  IRTN=1002
      WRITE(MSGFL,925)
 925  FORMAT(' (SUBR.BMINI) Energy < rest mass.')
      GOTO 1000
 930  IRTN=1003
      WRITE(MSGFL,935)
 935  FORMAT(' (SUBR.BMINI) Too many macro-particles.',/,
     %    '  Use ALLOCATE command to increase MP.')
      GOTO 1000
 940  IRTN=1004
      WRITE(MSGFL,945)
 945  FORMAT(' (SUBR.BMINI) Non-positive beta function.')
      GOTO 1000
 950  IRTN=1005
      WRITE(MSGFL,955)
 955  FORMAT(' (SUBR.BMINI) Energy < rest mass occured ',
     %  'due to SIGE.')
C
 1000 CALL CPUTIM('BMINI',2)
      RETURN
      END
      SUBROUTINE BMINI1(LR,KIND1,NP1,AN,TXYS0,LDIST,BETA,ALPHA,
     %  EMIT,SIG,E0,SIGE,SIGT,SLOPE,CRAB,GCUT,WGT1,ETA,ETAP,ESLOPE,
     %  XYROLL,DALPHADE,DALPHADT,SPIN1,MSGFL)
	USE BEAMCM
      IMPLICIT NONE
      INTEGER LR,KIND1,NP1,LDIST(3),MSGFL
      REAL*8 AN,TXYS0(0:3),BETA(2),ALPHA(2),EMIT(2),
     %  E0,SIG(2),SIGE,SIGT,SLOPE(2),CRAB(2),GCUT(4),WGT1,
     %  ETA(2),ETAP(2),ESLOPE,XYROLL,DALPHADE(2),DALPHADT(2),SPIN1(3)
C      INCLUDE 'include/beamcm.h'
      INTEGER N0,I
      CHARACTER*5 LLRR(2)/'Right','Left'/
      CHARACTER*8 KIN(3)/'Gamma','Electron','Positron'/
C
      N0=NP-NP1+1
      WRITE(MSGFL,300) LLRR(LR),KIN(KIND1)
 300  FORMAT(' +++ New Beam Defined: ',A,' going ',A)
      WRITE(MSGFL,320) E0,AN,NP1,WGT1,(TXYS0(I),I=0,3)
 320  FORMAT(T5,'Energy',T40,1PD10.3,' eV',/,
     % T5,'Number of real particles',T40,1PD10.4,/,
     % T5,'Number of macro particles',T40,I10,/,
     % T5,'Weight of a macro-particle',T40,1PD10.4,/,
     % T5,'Reference point (t,x,y,s) (m)',T40,1P4D10.3)
      IF(LDIST(1).LE.0) THEN
        WRITE(MSGFL,340) (BETA(I),I=1,2),(ALPHA(I),I=1,2),
     %  (EMIT(I),I=1,2),(SIG(I),I=1,2),(GCUT(I),I=1,2)
 340    FORMAT(T5,'--- Transverse Distribution',T40,'  Gaussian',/,
     %   T5,'Beta (x,y)',T40,1P2D10.3,' m',/,
     %   T5,'Alpha (x,y)',T40,1P2D10.2,/,
     %   T5,'Emittance (x,y)',T40,1P2D10.3,' rad.m',/,
     %   T5,'R.m.s. beam size (x,y)',T40,1P2D10.3,' m',/,
     %   T5,'Gaussian tail cut off',T40,0P2F10.2,' sigma')
      ELSE
        WRITE(MSGFL,360) (2*SIG(I),I=1,2)
 360    FORMAT(T5,'--- Transverse Distribution',
     %                 T36,'Uniform Ellips',/,
     %   T5,'Radius (x,y)',T40,1P2D10.3,' m')
      ENDIF
      WRITE(MSGFL,380) (SLOPE(I),I=1,2),(CRAB(I),I=1,2),
     %   (ETA(I),I=1,2),(ETAP(I),I=1,2),(DALPHADE(I),I=1,2),
     %   (DALPHADT(I),I=1,2),XYROLL
 380  FORMAT(
     % T5,'Slope (x,y)',T40,1P2D10.3,' rad',/,
     % T5,'Crab angle (x,y)',T40,1P2D10.3,' rad',/,
     % T5,'Eta function (x,y)',T40,1P2D10.3,' m',/,
     % T5,'Eta prime (x,y)',T40,1P2D10.3,/,
     % T5,'dAlpha/de (x,y)',T40,1P2D10.3,/,
     % T5,'dAlpha/dt (x,y)',T40,1P2D10.3,' 1/m',/,
     % T5,'Beam roll in x-y plane',T40,1PD10.3,' rad')
      IF(LDIST(2).LE.0) THEN
        WRITE(MSGFL,400) SIGT,GCUT(3)
 400    FORMAT(T5,'--- T distribution',T40,'  Gaussian',/,
     %   T5,'R.m.s.Bunch length and Gaussian cut',T40,1PD10.3,
     %       ' m ',0PF5.2)
      ELSE
        WRITE(MSGFL,420) 2*SQRT(3D0)*SIGT
 420    FORMAT(T5,'--- T distribution',T40,'   Uniform',/,
     %   T5,'Full bunch length',T40,6PF10.3,' mic')
      ENDIF
      IF(LDIST(3).LE.0) THEN
        WRITE(MSGFL,440) SIGE,GCUT(4)
 440    FORMAT(T5,'--- E distribution',T40,'  Gaussian',/,
     %   T5,'Relat. energy spread and Gauss cut',T40,1PD10.3,
     %       0PF10.2)
      ELSE
        WRITE(MSGFL,460) 2*SQRT(3D0)*SIGE
 460    FORMAT(T5,'--- E distribution',T40,'   Uniform',/,
     %   T5,'Full relative energy spread',T40,1PD10.3)
      ENDIF
      WRITE(MSGFL,470) ESLOPE
 470  FORMAT(T5,'Relative coherent energy slope',
     %    T40,1PD10.3,' 1/m')
      IF(KIND1.EQ.1) THEN
        WRITE(MSGFL,480) (SPIN1(I),I=1,3)
 480    FORMAT(T5,'Stokes parameter',T40,0P3F10.5)
      ELSE
        WRITE(MSGFL,490) (SPIN1(I),I=1,3)
 490    FORMAT(T5,'Spin (x,y,s)',T40,0P3F10.5)
      ENDIF
      WRITE(MSGFL,500)
 500  FORMAT('   --- Statistics of generated data ---')
      CALL BMSTAT(N0,NP,1,0,0,MSGFL)
      RETURN
      END
