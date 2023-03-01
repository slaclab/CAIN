	SUBROUTINE LSRPROP0(E,NXY,DXY,XYMM,Z0,WL,ZR,XYZ00,SIGR,
     %   P,NXYZ,DXYZ,XYZMM,IDEBUG1,IRTN)
C   Propagate the field defined by E,NXY,DXY,XYMM at z=Z0
C   to 3D mesh defined by NXYZ,DXYZ,XYZMM.
C   The latter must be in the twist coordinate defined by
C   ZR,XYZ00,SIGR.
C   The result (power density) is stored in P.
C
C  Procedure
C   (1) Rewrite the input data E(NXY(1),NXY(2)) into the
C       new temporary mesh EE0(NXYA(1),NXYA(2)) by interpolation.
C       NXYA has to be defined such the the same (x,y) mesh can be
C       used throughout the whole z region with sufficient accuracy.
C       The mesh size is DXYA in the near region and DXYB in the
C       far region (the latter changes from z to z). The center
C       is always XYZ00.
C   (2) Compute the Fourier transform EE0 and store into EK0
C   The for each z,
C   (3) Propagete EK0 to z to get EK and get its nverse Fourier
C       transform EE
C   (4) Interpolate EE for the twist coordinate to get P.
C
      IMPLICIT NONE
	INTEGER NXY(2),NXYZ(3),IDEBUG1,IRTN
	REAL(8) DXY(2),XYMM(2,2),Z0,WL,ZR,XYZ00(3),SIGR(2),
     %   DXYZ(3),XYZMM(2,3)
	COMPLEX(8) E(NXY(1),NXY(2))
	REAL(8) P(NXYZ(1)*NXYZ(2)*NXYZ(3))
	INTEGER NXYA(2),NXYB(2)
	INTEGER ISTAT,IZ0,IZ1,IZ2,IZD,IZ,I,J,I0,J0,L,II,IS,IJ,N,N0,MXY,
     %     IRTN1
	REAL(8) DXYA(2),DZ,X1,Y1,Z1,Z2,
     %  P1,P2,DUV(2),TH,DXYB(2),DXY3(2),XYMINA(2),XYMINB(2),XYMIN0(2),
     %  XYMIN3(2),TWF,TWF2,SUM,COEF
	COMPLEX(8) CF(-1:1),CF1
	LOGICAL NEAR
	COMPLEX(8), ALLOCATABLE:: EE0(:,:),EK0(:,:),EE(:,:),EE1(:,:)
	REAL(8),ALLOCATABLE:: ZPL(:),PPL(:),FXYPL(:,:,:),QPL(:,:,:),
     %       RMSPL(:,:)
	INTEGER,PARAMETER:: NQPL=5
	INTEGER IQPL(NQPL)
	REAL(8) ZQPL(NQPL)
	REAL(8) PI/3.141592653589793238D0/
	INCLUDE 'include/ctrlcm.h'

	DO L=1,2
	  NXYA(L)=256
	  DXYA(L)=SQRT(4*PI/NXYA(L))*SIGR(L)
	ENDDO
	NXYB=NXYA
C       NXYB is always the same as NXYA. Introduced for the legibility of code.

	ALLOCATE(EE0(NXYA(1),NXYA(2)),EK0(NXYA(1),NXYA(2)),
     %   EE(NXYA(1),NXYA(2)),EE1(NXYZ(1),NXYZ(2)),STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IRTN=1001
        RETURN
	ENDIF
	IF(IDEBUG1.NE.0) THEN
	  MXY=MIN(NXYZ(1)-1,NXYZ(2)-1)/2
	  ALLOCATE(ZPL(NXYZ(3)),PPL(NXYZ(3)),FXYPL(3,2,NXYZ(3)),
     %       QPL(2,0:MXY,NQPL),RMSPL(0:MXY,NQPL),STAT=ISTAT)
	  IF(ISTAT.NE.0) THEN
	    IRTN=1001
          GOTO 800
	  ENDIF
	ENDIF
C  Mesh change from NXY(1)*NXY(2) to NXYA(1)*NXYA(2) at z=Z0
	DO L=1,2
	  XYMINA(L)=XYZ00(L)-DXYA(L)*(NXYA(L)-1)/2
	  XYMIN0(L)=XYMM(1,L)
	ENDDO
	CALL LSRGRIDCHG(E,NXY,XYMIN0,DXY,EE0,NXYA,XYMINA,DXYA)
C  Store the initial Fourier transform for near region propagation
	EK0=EE0
	CALL ZFFT2S(EK0,NXYA(1),NXYA(1),NXYA(2),-1,-1)
	EK0=(DXYA(1)*DXYA(2))*EK0
C  Propagation
	IF(IDEBUG1.NE.0) THEN
	  IQPL(1)=1
	  IQPL(2)=NINT((-ZR-XYZMM(1,3))/DXYZ(3)+1D0)
	  IQPL(3)=(NXYZ(3)+1)/2
	  IQPL(4)=NINT((+ZR-XYZMM(1,3))/DXYZ(3)+1D0)
	  IQPL(5)=NXYZ(3)
	ENDIF
	P=0
	DO IZ=1,NXYZ(3)
	  Z1=XYZMM(1,3)+DXYZ(3)*(IZ-1)   !  z in twist coord.
	  NEAR=ABS(Z1).LE.ZR
	  IF(NEAR) THEN
	    TWF=1
	  ELSE
	    TWF=ABS(Z1)/ZR
	  ENDIF
	  Z2=Z1*TWF+XYZ00(3)             !  z in normal coord.
	  TWF2=TWF**2
	  DZ=Z2-Z0                       !  propagation distance
	  N0=NXYZ(1)*NXYZ(2)*(IZ-1)
	  IF(NEAR) THEN
C  Near region
	    DXYB=DXYA
	    DUV=1/(NXYA*DXYA)
	    DO J=1,NXYA(2)
	      DO I=1,NXYA(1)
	        TH=-PI*WL*DZ*((I-0.5D0*(NXYA(1)+1))**2*DUV(1)**2
     %                     +(J-0.5D0*(NXYA(2)+1))**2*DUV(2)**2)
	        EE(I,J)=EK0(I,J)*DCMPLX(COS(TH),SIN(TH))
	      ENDDO
	    ENDDO
			CALL ZFFT2S(EE,NXYA(1),NXYA(1),NXYA(2),1,1)
	    EE=(DUV(1)*DUV(2))*EE
	  ELSE
C  Far region
	    DXYB=WL*ABS(DZ)/(DXYA*NXYA)
	    DO J=1,NXYA(2)
	      DO I=1,NXYA(1)
	        TH=PI/(WL*DZ)*((I-0.5D0*(NXYA(1)+1))**2*DXYA(1)**2
     %                      +(J-0.5D0*(NXYA(2)+1))**2*DXYA(2)**2)
	        EE(I,J)=EE0(I,J)*DCMPLX(COS(TH),SIN(TH))
	      ENDDO
	    ENDDO
			IS=1
			IF(DZ.LT.0) IS=-1
			CALL ZFFT2S(EE,NXYA(1),NXYA(1),NXYA(2),-IS,-IS)
			EE=(DXYA(1)*DXYA(2)*DCMPLX(0D0,-1D0/(WL*DZ)))*EE
CC     Following phase transformation is omitted because
CC     it is not needed to compute the power and because it
CC     makes interpolation harder due to the rapid change of phase
CC	    DO J=1,NXYB(2)
CC	      DO I=1,NXYB(1)
CC	        TH=PI/(WL*DZ)*((I-0.5D0*(NXYB(1)+1))**2*DXYB(1)**2
CC     %                      +(J-0.5D0*(NXYB(2)+1))**2*DXYB(2)**2)
CC	        EE(I,J)=EE(I,J)*DCMPLX(COS(TH),SIN(TH))
CC	      ENDDO
CC	    ENDDO
	  ENDIF
C      Change mesh (DXYB,NXYB) --> (DXYZ,NXYZ)
	  DO L=1,2
	    XYMINB(L)=XYZ00(L)-DXYB(L)*(NXYB(L)-1)/2D0
	    XYMIN3(L)=TWF2*XYZMM(1,L)+XYZ00(L)
	    DXY3(L)=TWF2*DXYZ(L)
	  ENDDO
	  CALL LSRGRIDCHG(EE,NXYB,XYMINB,DXYB,
     %    EE1,NXYZ(1:2),XYMIN3,DXY3)
C      Store the power density
        SUM=0
	  DO J=1,NXYZ(2)
	    DO I=1,NXYZ(1)
	      N=I+NXYZ(1)*(J-1)+N0
	      P(N)=DREAL(EE1(I,J))**2+DIMAG(EE1(I,J))**2
	      SUM=SUM+P(N)
	    ENDDO
	  ENDDO
	  IF(IDEBUG1.NE.0) THEN
	    ZPL(IZ)=Z2
	    PPL(IZ)=SUM*DXY3(1)*DXY3(2)
	    FXYPL(1,1:2,IZ)=DXY3(1:2)*NXYZ(1:2)
	    CALL LSRCORE(P(N0+1:N0+NXYZ(1)*NXYZ(2)),NXYZ(1:2),DXY3,XYMIN3,
     %      XYZ00(1:2),COEF,
     %      FXYPL(3,1:2,IZ),FXYPL(2,1:2,IZ),Z2,IDEBUG1,0,IRTN1)
	    DO I=1,NQPL
	      IF(IQPL(I).EQ.IZ) THEN
	        ZQPL(I)=Z2
	        CALL LSRTAILDIS(P(N0+1:N0+NXYZ(1)*NXYZ(2)),NXYZ(1:2),DXY3,
     %           QPL(1:2,0:MXY,I),RMSPL(0:MXY,I))
	        CALL LSRGRIDCHGPLT(EE,NXYB,XYMINB,DXYB,
     %           EE1,NXYZ(1:2),XYMIN3,DXY3,Z2,TDFL)
	        EXIT
	      ENDIF
	    ENDDO
	  ENDIF
	ENDDO
	IF(IDEBUG1.NE.0) THEN
        CALL PRINTRADIUS(p,nxyz,xyzmm,dxyz,zr,XYZ00)
	  CALL LSRPROPPLT(NXYZ(3),ZPL,PPL,FXYPL,TDFL)
        CALL LSRTAILPLT(QPL,RMSPL,MXY,NQPL,ZQPL,TDFL)
	ENDIF

	IRTN=0
800	DEALLOCATE(EE0,EK0,EE,EE1,STAT=ISTAT)
	IF(IDEBUG1.NE.0) DEALLOCATE(ZPL,PPL,FXYPL,QPL,STAT=ISTAT)
	RETURN
	END

	SUBROUTINE LSRGRIDCHG(EA,NA,MINA,DA,EB,NB,MINB,DB)
C  2D GRID change from NA(1)*NA(2) to NB(1)*NB(2)
C     MIN:  lowest grid coordinate
C     N:    number of grid lines
C     D:    grid integval
C        (whole span of grid = (N-1)*D )
C  Use linear or quadratic or cubic interpolation
C   Allow extrapolation by half mesh outside the A-grid.
	IMPLICIT NONE
	INTEGER NA(2),NB(2)
	REAL(8) MINA(2),MINB(2),DA(2),DB(2)
	COMPLEX(8) EA(NA(1),NA(2)),EB(NB(1),NB(2))
	INTEGER L,I,J,I0,J0,II
	REAL(8) PX,PY,X1,Y1,EDGE(2,2)
	COMPLEX(8) CF(-1:2),CF1,CS1,CS2,CD1,CD2
	INTEGER ORDER/1/

	DO L=1,2
	  EDGE(1,L)=MINA(L)-0.5D0*DA(L)
	  EDGE(2,L)=MINA(L)+(NA(L)-0.5D0)*DA(L)
	ENDDO
	EB=0
	IF(ORDER.EQ.1) THEN
        DO J=1,NB(2)
	    Y1=MINB(2)+DB(2)*(J-1)
	    IF(Y1.LT.EDGE(1,2).OR.Y1.GT.EDGE(2,2)) CYCLE
	    PY=(Y1-MINA(2))/DA(2)+1
	    J0=MAX(1,MIN(NA(2)-1,INT(PY)))
	    PY=PY-J0
	    DO I=1,NB(1)
	      X1=MINB(1)+DB(1)*(I-1)
	      IF(X1.LT.EDGE(1,1).OR.X1.GT.EDGE(2,1)) CYCLE
	      PX=(X1-MINA(1))/DA(1)+1
	      I0=MAX(1,MIN(NA(1)-1,INT(PX)))
	      PX=PX-I0       !    0 < PX < 1
	      EB(I,J)=(1-PX)*((1-PY)*EA(I0,J0)+PY*EA(I0,J0+1))
     %               +PX*((1-PY)*EA(I0+1,J0)+PY*EA(I0+1,J0+1))
	    ENDDO
	  ENDDO
	ELSEIF(ORDER.EQ.2) THEN
        DO J=1,NB(2)
	    Y1=MINB(2)+DB(2)*(J-1)
	    IF(Y1.LT.EDGE(1,2).OR.Y1.GT.EDGE(2,2)) CYCLE
	    PY=(Y1-MINA(2))/DA(2)+1
	    J0=MAX(2,MIN(NA(2)-1,NINT(PY)))
	    PY=PY-J0
	    DO I=1,NB(1)
	      X1=MINB(1)+DB(1)*(I-1)
	      IF(X1.LT.EDGE(1,1).OR.X1.GT.EDGE(2,1)) CYCLE
	      PX=(X1-MINA(1))/DA(1)+1
	      I0=MAX(2,MIN(NA(1)-1,NINT(PX)))
	      PX=PX-I0       !    -0.5 < PX < 0.5
	      DO II=-1,1
	        CF(II)=EA(I0+II,J0)+(EA(I0+II,J0+1)-EA(I0+II,J0-1))/2*PY
     %          +(EA(I0+II,J0+1)+EA(I0+II,J0-1)-2*EA(I0+II,J0))/2*PY**2
	      ENDDO
	      EB(I,J)=CF(0)+(CF(1)-CF(-1))/2*PX
     %          +(CF(1)+CF(-1)-2*CF(0))/2*PX**2
	    ENDDO
	  ENDDO
	ELSEIF(ORDER.EQ.3) THEN
        DO J=1,NB(2)
	    Y1=MINB(2)+DB(2)*(J-1)
	    IF(Y1.LT.EDGE(1,2).OR.Y1.GT.EDGE(2,2)) CYCLE
	    PY=(Y1-MINA(2))/DA(2)+1
	    J0=MAX(2,MIN(NA(2)-2,INT(PY)))
	    PY=PY-J0-0.5D0
	    DO I=1,NB(1)
	      X1=MINB(1)+DB(1)*(I-1)
	      IF(X1.LT.EDGE(1,1).OR.X1.GT.EDGE(2,1)) CYCLE
	      PX=(X1-MINA(1))/DA(1)+1
	      I0=MAX(2,MIN(NA(1)-2,INT(PX)))
	      PX=PX-I0-0.5D0       !    -0.5 < PX < 0.5
	      DO II=-1,2
	        CS1=EA(I0+II,J0+1)+EA(I0+II,J0)
	        CD1=EA(I0+II,J0+1)-EA(I0+II,J0)
	        CS2=EA(I0+II,J0+2)+EA(I0+II,J0-1)
	        CD2=EA(I0+II,J0+2)-EA(I0+II,J0-1)
	        CF(II)=(9*CS1-CS2)/16+(27*CD1-CD2)/24*PY
     %          +(-CS1+CS2)/4*PY**2+(-3*CD1+CD2)/6*PY**3
	      ENDDO
	      CS1=CF(1)+CF(0)
	      CD1=CF(1)-CF(0)
	      CS2=CF(2)+CF(-1)
	      CD2=CF(2)-CF(-1)
	      EB(I,J)=(9*CS1-CS2)/16+(27*CD1-CD2)/24*PX
     %          +(-CS1+CS2)/4*PX**2+(-3*CD1+CD2)/6*PX**3
	    ENDDO
	  ENDDO
	ENDIF
	RETURN
	END

	SUBROUTINE LSRGRIDCHGPLT(EA,NA,MINA,DA,EB,NB,MINB,DB,Z,TDFL)
C      Check interpolated field along x-axis
	IMPLICIT NONE
	INTEGER NA(2),NB(2),TDFL
	REAL(8) MINA(2),MINB(2),DA(2),DB(2),Z
	COMPLEX(8) EA(NA(1),NA(2)),EB(NB(1),NB(2))
	INTEGER I,JB,JA,JJ(2),K
	REAL(8) YB,YA,F1,FMAX,FMIN,PY
	INTEGER LF/1/    !  1: Power,  2: Real(E)
	CHARACTER(5) LFF(3)/'Power','Re(E)','Im(E)'/
	CHARACTER(5) LM(2)/'SOLID','DASH'/
	REAL(8) GRIDCHGFUN

	JB=NB(2)/2
	YB=MINB(2)+(JB-1)*DB(2)
	JA=MAX(1,MIN(NA(2)-1,INT((YB-MINA(2))/DA(2)+1D0+10000D0)-10000))
	YA=MINA(2)+(JA-1)*DA(2)
	PY=(YB-YA)/DA(2)
	IF(PY.LE.0.5D0) THEN
	  JJ(1)=JA
	  JJ(2)=JA+1
	ELSE
	  JJ(1)=JA+1
	  JJ(2)=JA
	  PY=1-PY
	ENDIF
	FMAX=0
	FMIN=0
	DO I=1,NB(1)
	  F1=GRIDCHGFUN(EB(I,JB),LF)
	  FMAX=MAX(FMAX,F1)
	  FMIN=MIN(FMIN,F1)
	ENDDO
	DO I=1,NA(1)
	  DO K=1,2
	    F1=GRIDCHGFUN(EA(I,JJ(K)),LF)
	    FMAX=MAX(FMAX,F1)
	    FMIN=MIN(FMIN,F1)
	  ENDDO
	ENDDO

	WRITE(TDFL,200) MINB(1)/1D-6,(MINB(1)+DB(1)*(NB(1)-1))/1D-6,
     %  FMIN,FMAX,LFF(LF),Z/1D-3
200   FORMAT(' NEWFRAME; SET FONT DUPLEX',/,
     % " SET LIMIT X",1P2D12.4," Y",2D12.4,/,
     % " TITLE TOP 'Interpolation of ",A," at z=",0PF9.5,"mm'",/,
     % " PLOT AXIS")

      WRITE(TDFL,300) ((MINB(1)+DB(1)*(I-1))/1D-6,
     %  GRIDCHGFUN(EB(I,JB),LF),I=1,NB(1))
300   FORMAT(3(1PD11.4,D12.4,';'))
      WRITE(TDFL,320) 'RED'
320   FORMAT(' SET COLOR ',A)
      WRITE(TDFL,340) 'SOLID'
340   FORMAT(' JOIN 1 ',A)
	
	DO K=1,2
        WRITE(TDFL,300) ((MINA(1)+DA(1)*(I-1))/1D-6,
     %    GRIDCHGFUN(EA(I,JJ(K)),LF),I=1,NA(1))
        WRITE(TDFL,320) 'BLUE'
	  WRITE(TDFL,340) LM(K)
	ENDDO
	WRITE(TDFL,400) PY
400   FORMAT(" TITLE 9.0 0.5 SIZE 1.6 'Py=",F6.3,"'",/,
     %  " PLOT AXIS")

	RETURN
	END

	FUNCTION GRIDCHGFUN(CF,K)
	IMPLICIT NONE
	INTEGER K
	REAL(8) GRIDCHGFUN
	COMPLEX(8) CF
	IF(K.EQ.1) THEN
	  GRIDCHGFUN=DREAL(CF)**2+DIMAG(CF)**2
	ELSEIF(K.EQ.2) THEN
	  GRIDCHGFUN=DREAL(CF)
	ELSEIF(K.EQ.3) THEN
	  GRIDCHGFUN=DIMAG(CF)
	ELSE
	  GRIDCHGFUN=0
	ENDIF
	RETURN
	END

	SUBROUTINE PRINTRADIUS(P,NXYZ,XYZMM,DXYZ,ZR,XYZ00)
	IMPLICIT NONE
	INTEGER NXYZ(3)
	REAL(8) P(NXYZ(1)*NXYZ(2)*NXYZ(3)),XYZMM(2,3),DXYZ(3),
     %  ZR,XYZ00(3)
	INTEGER I,J,IZ,N
	REAL(8) SUM,XY(2),AV(2),RMS(2),Z,TWF,TWF2
	INCLUDE 'include/ctrlcm.h'

	WRITE(MSGFL,100)
100   FORMAT(' (SUBR.PRINTRADIUS) COMPUTED LASER BEAM RMS RADIUS',/,
     %  '     Z(MM) ','   POWER INT ','   <X>(MIC) ','   <Y>(MIC) ',
     %  '  SIGX(MIC) ','  SIGY(MIC) ')
	DO IZ=1,NXYZ(3)
	  Z=XYZMM(1,3)+DXYZ(3)*(IZ-1)
	  IF(ABS(Z).LE.ZR) THEN
	    TWF=1
	  ELSE
	    TWF=ABS(Z)/ZR
	  ENDIF
	  Z=Z*TWF+XYZ00(3)
	  TWF2=TWF**2
	  AV=0
	  SUM=0
	  RMS=0
	  DO J=1,NXYZ(2)
	    XY(2)=TWF2*(XYZMM(1,2)+DXYZ(2)*(J-1))
	    DO I=1,NXYZ(1)
	      N=I+NXYZ(1)*(J-1+NXYZ(2)*(IZ-1))
	      XY(1)=TWF2*(XYZMM(1,1)+DXYZ(1)*(I-1))
	      SUM=SUM+P(N)
	      AV=AV+P(N)*XY
	      RMS=RMS+P(N)*XY**2
	    ENDDO
	  ENDDO
	  AV=AV/SUM
	  RMS=SQRT(MAX(0D0,RMS/SUM-AV**2))
	  SUM=SUM*DXYZ(1)*DXYZ(2)*TWF2**2
	  AV=XYZ00(1:2)+AV
	  RMS=RMS
	  WRITE(MSGFL,200) Z,SUM,AV,RMS
200     FORMAT(1X,3PF10.5,1PD13.6,6P2F12.3,6P2F12.3)
      ENDDO
	RETURN
	END
	  
	SUBROUTINE LSRPROPPLT(NZ,ZPL,PPL,FXYPL,TDFL)
	IMPLICIT NONE
	INTEGER NZ,TDFL
	REAL(8) ZPL(NZ),PPL(NZ),FXYPL(3,2,NZ)

	REAL(8) XWL0/2.0/,XWH0/11.5/,YWL0/2.0/,YWH0/9.0/,
     %   XWL,XWH,YWL,YWH
      CHARACTER(1) LXY(2)/'x','y'/
	CHARACTER(7) COLOR(3)/'MAGENTA','BLUE','RED'/
	CHARACTER(9)  LFUN(3)/'W0x,y1/10','S0rms1','S0core1'/
      CHARACTER(9) LFUNC(3)/' X   X   ','GX   X','GX    X'/

	INTEGER I,J,L,NEXP
	REAL(8) UNITZ,UNITP,UNITF,PMIN,PMAX,FMAX,X1,Y1

	PMIN=PPL(1)
	PMAX=PPL(1)
	FMAX=0
	DO I=1,NZ
	  PMIN=MIN(PMIN,PPL(I))
	  PMAX=MAX(PMAX,PPL(I))
	  DO L=1,2
	    FXYPL(1,L,I)=FXYPL(1,L,I)/10
	    DO J=1,3
	      FMAX=MAX(FMAX,FXYPL(J,L,I))
	    ENDDO
	  ENDDO
	ENDDO
	UNITF=1D-6
	FMAX=1.05*FMAX/UNITF
	PMAX=PMAX+0.05*(PMAX-PMIN)
	PMIN=MAX(0D0,PMIN-0.05*(PMAX-PMIN))
	NEXP=3*(INT(LOG10(PMAX)/3+1000)-1000)
	UNITP=10D0**NEXP
	PMAX=PMAX/UNITP
	PMIN=PMIN/UNITP
	UNITZ=1D-3

	WRITE(TDFL,100)
100   FORMAT(' NEWFRAME; SET FONT DUPLEX')
	CALL TDHEAD(TDFL)

	WRITE(TDFL,200) XWL0,XWH0,YWL0,YWH0,
     %   ZPL(1)/UNITZ,ZPL(NZ)/UNITZ,PMIN,PMAX,
     %   (XWL0+XWH0)/2,YWH0+0.6,
     %   XWH0+1.1,(YWL0+YWH0)/2,NEXP,
     %   (XWL0+XWH0)/2,YWL0-1.0
200   FORMAT(' SET WINDOW X',0P2F7.3,' Y',2F7.3,/,
     % ' SET LIMIT X',1P2D12.4,' Y',2D12.4,/,
     % ' SET AXIS LEFT OFF; SET LABELS RIGHT ON',/,
     % " TITLE",0P2F7.3," SIZE 2.0 CENTER ''",/,
     % " MORE 'Laser Field Data Exteded to 3D'",/,
     % " TITLE",0P2F7.3," SIZE 2.0 ANGLE 90 CENTER ''",/,
     % " MORE 'Integrated Power 102",I2,"3'",/,
     % " CASE '                   X",2X,"X'",/,
     % " TITLE",0P2F7.3," SIZE 2.5 CENTER 'z (mm)'",/,
     % " PLOT AXIS")
      WRITE(TDFL,220) (ZPL(I)/UNITZ,PPL(I)/UNITP,I=1,NZ)
220   FORMAT(3(1PD11.4,D12.4,';'))
      WRITE(TDFL,240) 'SOLID'
240   FORMAT(' JOIN 1 ',A)

	DO L=1,2
	  YWL=YWH0-(YWH0-YWL0)/2*L
	  YWH=YWL+(YWH0-YWL0)/2
	  WRITE(TDFL,260) 'WHITE'
260     FORMAT(' SET COLOR ',A)
	  WRITE(TDFL,300) XWL0,XWH0,YWL,YWH,
     %   ZPL(1)/UNITZ,ZPL(NZ)/UNITZ,0D0,FMAX,
     %   XWL0-1.0,(YWL+YWH)/2,LXY(L)
300   FORMAT(' SET WINDOW X',0P2F7.3,' Y',2F7.3,/,
     % ' SET LIMIT X',1P2D12.4,' Y',2D12.4,/,
     % ' SET AXIS ALL OFF LEFT ON BOTTOM ON',/,
     % ' SET LABELS LEFT ON BOTTOM OFF',/,
     % " TITLE",0P2F7.3," SIZE 2.0 ANGLE 90 CENTER '",A,"'",/,
     % " PLOT AXIS")
	  DO J=1,3
	    WRITE(TDFL,260) COLOR(J)
	    WRITE(TDFL,220) (ZPL(I)/UNITZ,FXYPL(J,L,I)/UNITF,
     %       I=1,NZ)
	    WRITE(TDFL,240) 'SOLID'
	  ENDDO
	ENDDO
	X1=0.5
	Y1=2.2
	DO J=1,3
	  WRITE(TDFL,260) COLOR(J)
	  WRITE(TDFL,400) X1,Y1,X1,Y1+1.0
400     FORMAT(2(2F7.3,';'),'JOIN 1 TEXT')
        WRITE(TDFL,420) X1,Y1+1.2,LFUN(J),LFUNC(J)
420     FORMAT(" TITLE",2F7.3," SIZE 2.0 ANGLE 90 '",A,"'",/,
     %         " CASE '",A,"'")
	  Y1=Y1+2.5
	ENDDO
	WRITE(TDFL,440) X1,Y1
440   FORMAT(" TITLE",2F7.3," SIZE 2.0 ANGLE 90 '(Mm)'",/,
     %    " CASE ' G  '")
	RETURN
	END

	SUBROUTINE LSRTAILDIS(P,NXY,DXY,Q,RMS)
	IMPLICIT NONE
	INTEGER NXY(2)
	REAL(8) P(NXY(1),NXY(2)),Q(2,0:*),DXY(2),RMS(0:*)
C        Length of Q = MIN(NXY(1),NXY(2))/2

	INTEGER M(2),NX0,NY0,K,I,J,II
	REAL(8) SUM,RMS1,X00

	M(1)=MIN(NXY(1)-1,NXY(2)-1)/2
	M(2)=M(1)/2
	NX0=(NXY(1)+1)/2
	NY0=(NXY(2)+1)/2
	X00=0.5D0*(NXY(1)+1)
	DO II=1,2
	  SUM=0
	  RMS1=0
	  DO K=0,II*M(II),II
	    DO I=NX0-K,NX0+K,II
	      SUM=SUM+P(I,NY0-K)+P(I,NY0+K)
	      IF(II.EQ.1) RMS1=RMS1+(I-X00)**2*(P(I,NY0-K)+P(I,NY0+K))
	    ENDDO
	    IF(K.GE.1) THEN
	      DO J=NY0-K+1,NY0+K-1,II
	        SUM=SUM+P(NX0-K,J)+P(NX0+K,J)
	        IF(II.EQ.1) RMS1=RMS1
     %           +(NX0-K-X00)**2*P(NX0-K,J)+(NX0+K-X00)**2*P(NX0+K,J)
	      ENDDO
	    ENDIF
	    Q(II,K/II)=SUM*DXY(1)*DXY(2)
	    IF(II.EQ.2) Q(II,K/II)=Q(II,K/II)*4
	    IF(II.EQ.1) RMS(K)=RMS1*DXY(1)*DXY(2)
	  ENDDO
	ENDDO
	DO K=0,M(1)
	  IF(Q(1,K).NE.0) THEN
	    RMS(K)=SQRT(MAX(0D0,RMS(K)/Q(1,K)))*DXY(1)
	  ENDIF
	ENDDO

	RETURN
	END

	SUBROUTINE LSRTAILPLT(QPL,RMSPL,MXY,NQPL,ZQPL,TDFL)
	IMPLICIT NONE
	INTEGER MXY,NQPL,TDFL
	REAL(8) QPL(2,0:MXY,NQPL),RMSPL(0:MXY,NQPL),ZQPL(NQPL)
	INTEGER I,IZ,MM(2),II,NEXQ
	REAL(8) QMAX,X1,Y1,RMSMAX,RMSMIN,UNITQ
	CHARACTER(7) COLOR(5)/'RED','GREEN','CYAN','BLUE','MAGENTA'/
	CHARACTER(6) LMODE(2)/'SOLID','DASHES'/
	REAL(8) XL/3.7/,XH/11.5/,YL/1.0/,YH/9.0/

	MM(1)=MXY
	MM(2)=MXY/2
	QMAX=0
	RMSMAX=0
	RMSMIN=1D60
	DO IZ=1,NQPL
	  QMAX=MAX(QMAX,QPL(1,MM(1),IZ),QPL(2,MM(2),IZ))
	  DO I=0,MM(1)
	    RMSMAX=MAX(RMSMAX,RMSPL(I,IZ))
	    RMSMIN=MIN(RMSMIN,RMSPL(I,IZ))
	  ENDDO
	ENDDO
	QMAX=1.05*QMAX
	NEXQ=INT(LOG10(QMAX)+1000)-1000
	UNITQ=10D0**NEXQ
	RMSMAX=1.05*RMSMAX
	RMSMIN=MAX(RMSMIN,RMSMAX/100)
	WRITE(TDFL,200)
200   FORMAT(' NEWFRAME; SET FONT DUPLEX')
	CALL TDHEAD(TDFL)
	WRITE(TDFL,220) XL,XH,YL,YH,0,MXY,0D0,QMAX/UNITQ,
     %  (XL+XH)/2,YH+0.5,XL-1.0,(YH+YL)/2,
     %  NEXQ,(XL+XH)/2,YL-0.65
220   FORMAT(' SET WINDOW X',2F7.3,' Y',2F7.3,/,
     % ' SET LIMIT X',2I6,' Y',1P2D12.4,/,
     % " SET AXIS RIGHT OFF",/,
     % " TITLE",0P2F7.3," CENTER SIZE 2.0 'Check Mesh Size and Range'"
     %    ,/,
     % " TITLE",0P2F7.3," ANGLE 90 CENTER SIZE 2.0 ''",/,
     % " MORE 'Power integral in +n Mesh at center (102",I2,")'",/,
     % " CASE '                  M                    X",2X," '",/,
     % " TITLE",0P2F7.3," CENTER SIZE 2.0 'n'",/,
     % " PLOT AXIS")
	X1=0.4
	Y1=8.0
	WRITE(TDFL,260) X1+1.2,Y1
260   FORMAT(" TITLE",2F7.3," SIZE 1.6 ' z (mm)'; PLOT AXIS")
      DO IZ=1,NQPL
        DO II=1,2
	    WRITE(TDFL,300) (I*II,QPL(II,I,IZ)/UNITQ,I=0,MM(II))
300       FORMAT(4(I5,1PD12.4,';'))
          WRITE(TDFL,320) COLOR(MOD(IZ-1,5)+1)
320       FORMAT(' SET COLOR ',A)
          WRITE(TDFL,340) LMODE(II)
340       FORMAT(' JOIN 1 ',A)
        ENDDO
        Y1=Y1-0.4
	  WRITE(TDFL,360) X1,Y1,X1+1.0,Y1
360     FORMAT(2(2F7.3,';'),'JOIN 1 TEXT')
        WRITE(TDFL,320) 'WHITE'
	  WRITE(TDFL,400) X1+1.2,Y1,ZQPL(IZ)/1D-3
400     FORMAT(" TITLE",2F7.3," SIZE 1.6 '",0PF7.3,"'; PLOT AXIS")
      ENDDO
	Y1=Y1-0.5
	WRITE(TDFL,420) X1,Y1,X1,Y1-0.3
420   FORMAT(" TITLE",2F7.3," SIZE 1.6 'dash:'",/,
     %  " TITLE",2F7.3," SIZE 1.6 '  2x coarse mesh'",/,
     %  ' PLOT AXIS')
      WRITE(TDFL,500) 0,MXY,RMSMIN/1D-6,RMSMAX/1D-6,XH+0.6,(YL+YH)/2
500   FORMAT(' SET LIMIT X',2I6,' Y',1P2D12.4,/,
     % " SET SCALE Y LOG",/,
     % " SET AXIS ALL OFF RIGHT ON; SET LABELS RIGHT ON",/,
     % " TITLE",0P2F7.3," ANGLE 90 CENTER RIGHT SIZE 2.0 ''",/,
     % " MORE 'dot: rms(x) (Mm) in +n Mesh at center'",/,
     % " CASE '             G      M                '",/,
     % " PLOT AXIS")
      DO IZ=1,NQPL
        WRITE(TDFL,300) (I,RMSPL(I,IZ)/1D-6,I=0,MM(1))
	  WRITE(TDFL,320) COLOR(MOD(IZ-1,5)+1)
	  WRITE(TDFL,340) 'DOTS'
	ENDDO
	RETURN
	END
