      SUBROUTINE LSRFOCUS(E,NXY,DXY,XYMM,Z0,ZMAX,WL,
     %   XYFOC,ZFOC,BFOC,SIGFOC,EMIT,IDEBUG1,MSGFL,IRTN)
C  Given the complex field data E on z=Z0, estimate the focal point
C  properties of the beam core.
C  This is intended to define the twist coordinate system. Need not
C  be rigorous. What is not easy is to define the 'core'.
C  Simple root-mean-square does not give a useful beam size when
C  the field has a diffraction tail.
C  The method should work only when
C    *  Z0 is close to the focus
C    *  Focal points in x and y planes are close to each other
C  Input
C      E(NXY(1),NXY(2))  Complex field at z=Z0
C      NXY(2)    Number of mesh points
C      DXY(2)    Mesh interval
C      XYMM(2)   Min-max of the mesh region.
C      Z0        Reference z
C      ZMAX      Estimate the beam envelope in the regio(Z0-ZMAX,Z0+ZMAX)
C      WL        wave length
C  Output
C   XYFOC(2)  x(y)-coordinate of the focal point in x(y) plane
C   BFOC(2)   Rayleigh length
C   EMIT(2)   Emittance in rad.m
C   ZFOC(2)   focal point in x and y plane
C  Return code
C    100     memory allocation error
C    101     field zero ?
C    102,103     Rayleigh relation not obtained
C    200,201,202 Failed to find the core size
	IMPLICIT NONE
	INTEGER NXY(2),MSGFL,IDEBUG1,IRTN
	REAL(8) DXY(2),XYMM(2,2),Z0,WL,ZMAX,XYFOC(2),ZFOC(2),BFOC(2),
     %     SIGFOC(2),EMIT(2)
	COMPLEX(8) E(NXY(1),NXY(2))
	INTEGER I,J,L,IZ,ISTAT,ISIG
	REAL(8) I0,J0,XYC(2)
	INTEGER,PARAMETER:: MZ=5
	REAL(8) SUM,LAMBAR,FSQ,EDE,XY(2),XYINT(5,2),A1,B1,PH,PH0,DXY1(2)
	COMPLEX(8) DE(2),C0,C1
	INTEGER IWORK(0:2)
	REAL(8) ZZ(MZ),SIGCORE(2,MZ),AA(0:2,0:2),BB(0:2),WORK(0:2),SIG20,
     %       XYMIN(2),COEF,SIGRMS(2),X,Y
	COMPLEX(8),ALLOCATABLE:: EE(:,:)
	REAL(8),ALLOCATABLE:: P(:,:)
	REAL(8) PI/3.141592653589793238D0/

	LAMBAR=WL/(2*PI)
C  First estimate the parameters using simple root-mean-square
	SUM=0
	DO J=1,NXY(2)
	  XY(2)=XYMM(1,2)+DXY(2)*(J-1)
	  DO I=1,NXY(1)
	    XY(1)=XYMM(1,1)+DXY(1)*(I-1)
	    FSQ=DREAL(E(I,J))**2+DIMAG(E(I,J))**2
	    SUM=SUM+FSQ
	    XYINT(1,1:2)=XYINT(1,1:2)+XY(1:2)*FSQ
	    XYINT(2,1:2)=XYINT(2,1:2)+XY(1:2)**2*FSQ
	    IF(I.EQ.1) THEN
	      DE(1)=-(3*E(I,J)-4*E(I+1,J)+E(I+2,J))/2/DXY(1)
	    ELSEIF(I.EQ.NXY(1)) THEN
	      DE(1)=(3*E(I,J)-4*E(I-1,J)+E(I-2,J))/2/DXY(1)
	    ELSE
	      DE(1)=(E(I+1,J)-E(I-1,J))/2/DXY(1)
	    ENDIF
	    IF(J.EQ.1) THEN
	      DE(2)=-(3*E(I,J)-4*E(I,J+1)+E(I,J+2))/2/DXY(2)
	    ELSEIF(J.EQ.NXY(2)) THEN
	      DE(2)=(3*E(I,J)-4*E(I,J-1)+E(I,J-2))/2/DXY(2)
	    ELSE
	      DE(2)=(E(I,J+1)-E(I,J-1))/2/DXY(2)
	    ENDIF
		  DO L=1,2
	      EDE=DREAL(DE(L))*DIMAG(E(I,J))-DIMAG(DE(L))*DREAL(E(I,J))
	      XYINT(3,L)=XYINT(3,L)+EDE
	      XYINT(4,L)=XYINT(4,L)+XY(L)*EDE
	      XYINT(5,L)=XYINT(5,L)+DREAL(DE(L))**2+DIMAG(DE(L))**2
	    ENDDO
	  ENDDO
	ENDDO
	IF(SUM.LE.0) THEN
	  IRTN=101
	  RETURN
	ENDIF
	XYINT=XYINT/SUM
	DO L=1,2
	  XYINT(3,L)=LAMBAR*XYINT(3,L)
	  XYINT(4,L)=LAMBAR*XYINT(4,L)
	  XYINT(5,L)=LAMBAR**2*XYINT(5,L)
	ENDDO
	DO L=1,2
	  XYFOC(L)=XYINT(1,L)
	  SIGFOC(L)=XYINT(2,L)-XYINT(1,L)**2
	  A1=XYINT(4,L)-XYINT(1,L)*XYINT(3,L)
	  B1=XYINT(5,L)-XYINT(3,L)**2
	  IF(SIGFOC(L).LE.0.OR.B1.LE.0) THEN
	    IRTN=102
	    RETURN
	  ENDIF
	  SIGFOC(L)=SQRT(SIGFOC(L))
	  BFOC(L)=SIGFOC(L)/SQRT(B1)
	  ZFOC(L)=-A1/B1
	  EMIT(L)=SIGFOC(L)**2/BFOC(L)
	ENDDO
C  Now estimate the core envelope at 5 points, Z0-ZMAX, Z0-B, Z0, Z0+B, Z0+ZMAX 
C  where B is the x-y averaged Rayleigh length found above.
	ALLOCATE(EE(NXY(1),NXY(2)),P(NXY(1),NXY(2)),STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IRTN=100
	  RETURN
	ENDIF
	ZZ(1)=-ZMAX
	ZZ(2)=-SQRT(BFOC(1)*BFOC(2))
	ZZ(3)=0
	ZZ(4)=-ZZ(2)
	ZZ(5)=-ZZ(1)

	DO IZ=1,MZ
	  IF(ZZ(IZ).EQ.0) THEN
	    DO J=1,NXY(2)
	      DO I=1,NXY(1)
	        P(I,J)=DREAL(E(I,J))**2+DIMAG(E(I,J))**2
	      ENDDO
	    ENDDO
	    XYMIN(1:2)=XYMM(1,1:2)
	    DXY1=DXY
	  ELSE
C       E(x,y,z)= (i/(lambda*z)) * int dx'dy' exp[i*pi/(lambda*z)*[(x-x')^2+(y-y')^2]] * E(x',y',0)
C         = (i/(lambda*z)) * exp[i*pi/(lambda*z)*(x^2+y^2)]
C            * int dx'dy' exp[-2*pi*i/(lambda*z)*(x*x'+y*y')] * exp[i*pi/(lambda*z)*(x'^2+y'^2)] * E(x',y',0)
C       To perform this integral by FFT, the new mesh size must be
C            DXY1(L)=lambda*abs(z)/(DXY(L)*NXY(L))
C       The origin must be shifted for using FFT
	    ISIG=1
	    IF(ZZ(IZ).LT.0) ISIG=-1
	    I0=0.5D0*(NXY(1)+1)
	    J0=0.5D0*(NXY(2)+1)
	    DXY1=WL*ABS(ZZ(IZ))/(DXY*NXY)
	    XYMIN(1)=XYMM(1,1)+(DXY(1)-DXY1(1))*(NXY(1)-1)/2D0
	    XYMIN(2)=XYMM(1,2)+(DXY(2)-DXY1(2))*(NXY(2)-1)/2D0
	    DO J=1,NXY(2)
	      Y=XYMM(1,2)+DXY(2)*(J-1)
	      DO I=1,NXY(1)
	        X=XYMM(1,1)+DXY(1)*(I-1)
	        PH=((X-XYMIN(1))**2+(Y-XYMIN(2))**2)*PI/(WL*ZZ(IZ))
	        EE(I,J)=E(I,J)*EXP(DCMPLX(0D0,PH))
	      ENDDO
	    ENDDO
	    CALL ZFFT2(EE,NXY(1),NXY(1),NXY(2),-ISIG,-ISIG)
	    C0=DCMPLX(0D0,DXY(1)*DXY(2)/(WL*ZZ(IZ)))
	    PH0=PI/(WL*ZZ(IZ))
     %      *((XYMM(1,1)-XYMIN(1))**2+(XYMM(1,2)-XYMIN(2))**2)
	    DO J=1,NXY(2)
	      Y=XYMIN(2)+DXY1(2)*(J-1)
	      DO I=1,NXY(1)
	        X=XYMIN(1)+DXY1(1)*(I-1)
	        PH=((X-XYMM(1,1))**2+(Y-XYMM(1,2))**2)*PI/(WL*ZZ(IZ))+PH0
	        C1=C0*EE(I,J)*EXP(DCMPLX(0D0,PH))
C                This phase factor is not actually needed here
	        P(I,J)=DREAL(C1)**2+DIMAG(C1)**2
	      ENDDO
	    ENDDO
	  ENDIF
	  CALL LSRCORE(P,NXY,DXY1,XYMIN,XYFOC,
     %     COEF,SIGCORE(1:2,IZ),SIGRMS,ZZ(IZ),IDEBUG1,MSGFL,IRTN)
	  IF(IRTN.NE.0) GOTO 800
	ENDDO
	
C      Fit the core size SIGCORE(1:2,1:MZ) by least square of relative error.
	DO L=1,2
	  AA=0
	  BB=0
	  DO I=0,2
	    DO IZ=1,MZ
	      BB(I)=BB(I)+ZZ(IZ)**I/SIGCORE(L,IZ)**2
	    ENDDO
	    DO J=0,2
	      DO IZ=1,MZ
	        AA(I,J)=AA(I,J)+ZZ(IZ)**(I+J)/SIGCORE(L,IZ)**4
	      ENDDO
	    ENDDO
	  ENDDO
	  CALL DLUDCP(AA,3,3,IWORK,WORK,ISTAT)
	  CALL DLUBKS(AA,3,3,IWORK,BB)
C          SIGCORE(z)**2 = BB(0) + BB(1)*z + BB(2)*z**2
C              (z is measured from Z0)
	  SIG20=BB(0)-BB(1)**2/4/BB(2)
	  IF(SIG20.LE.0) THEN
	    IRTN=103
	    GOTO 800
	  ENDIF
	  SIGFOC(L)=SQRT(SIG20)
	  BFOC(L)=SQRT(SIG20/BB(2))
	  EMIT(L)=SIG20/BFOC(L)
	  ZFOC(L)=Z0-BB(1)/2/BB(2)
	ENDDO
	IRTN=0

800	DEALLOCATE(EE,P,STAT=ISTAT)
	RETURN
	END

	SUBROUTINE LSRCORE(P,NXY,DXY,XYMIN,XYFOC,COEF,SIGCORE,SIGRMS,Z1,
     %    IDEBUG1,MSGFL,IRTN)
C  Find the core size which minimizes
C      integral [ P(x,y) - coef*Exp(-0.5*(x^2/sigx^2+y^2/sigy^2)) ]^2 dxdy
C   Z1 needed for printing only
C  Output
C      SIGORE(2) =  sigx, sigy of the core
C      SIGRMS(2) =  root-mean-square
C      COEF    =  coef
	IMPLICIT NONE
	INTEGER NXY(2),MSGFL,IDEBUG1,IRTN
	REAL(8) P(NXY(1),NXY(2)),DXY(2),XYMIN(2),XYFOC(2),COEF,SIGCORE(2),
     %   SIGRMS(2),Z1
	INTEGER I,J,ITR,YET
	REAL(8) DVOL,SIG2(2),SIG20(2),X,Y,EX,INTEG(0:5),F(2),FF(2,2),DET,
     %     DSIG2(2),PMAX
	CHARACTER(9) MSG
	REAL(8) PI/3.141592653589793238D0/

	DVOL=DXY(1)*DXY(2)
	PMAX=0
	ITR=-1
200   ITR=ITR+1
	IF(ITR.GT.15) GOTO 920
	IF(ITR.NE.0) SIG20=SIG2
	INTEG=0
	DO J=1,NXY(2)
	  Y=XYMIN(2)+DXY(2)*(J-1)-XYFOC(2)
	  DO I=1,NXY(1)
	    X=XYMIN(1)+DXY(1)*(I-1)-XYFOC(1)
	    IF(ITR.EQ.0) THEN
	      EX=P(I,J)
	      PMAX=MAX(PMAX,P(I,J))
	    ELSE
	      EX=0.5D0*(X**2/SIG20(1)+Y**2/SIG20(2))
	      IF(EX.LE.80) THEN
	        EX=EXP(-EX)*P(I,J)
	      ELSE
	        EX=0
	      ENDIF
	    ENDIF
	    IF(EX.NE.0) THEN
	      INTEG(0)=INTEG(0)+EX
	      INTEG(1)=INTEG(1)+EX*X**2
	      INTEG(2)=INTEG(2)+EX*Y**2
	      IF(ITR.GE.2) THEN
	        INTEG(3)=INTEG(3)+EX*X**4
	        INTEG(4)=INTEG(4)+EX*X**2*Y**2
	        INTEG(5)=INTEG(5)+EX*Y**4
	      ENDIF
	    ENDIF
	  ENDDO
	ENDDO
	IF(INTEG(0).LE.0) GOTO 900
	INTEG=INTEG*DVOL
	IF(ITR.LE.1) THEN
	  SIG2=INTEG(1:2)/INTEG(0)
	  IF(ITR.EQ.1) SIG2=2*SIG2
	  IF(ITR.EQ.0) SIGRMS=SQRT(MAX(0D0,SIG2))
	  YET=1
	ELSE
	  F=SIG2*INTEG(0)-2*INTEG(1:2)
	  FF(1,1)=INTEG(0)+INTEG(1)/(2*SIG2(1))-INTEG(3)/SIG2(1)**2
	  FF(1,2)=SIG2(1)/(2*SIG2(2)**2)*INTEG(2)-INTEG(4)/SIG2(2)**2
	  FF(2,1)=SIG2(2)/(2*SIG2(1)**2)*INTEG(1)-INTEG(4)/SIG2(1)**2
	  FF(2,2)=INTEG(0)+INTEG(2)/(2*SIG2(2))-INTEG(5)/SIG2(2)**2
	  DET=FF(1,1)*FF(2,2)-FF(2,1)*FF(1,2)
	  DSIG2(1)=-(FF(2,2)*F(1)-FF(1,2)*F(2))/DET
	  DSIG2(2)=-(-FF(2,1)*F(1)+FF(1,1)*F(2))/DET
	  SIG2=SIG2+DSIG2
	  DSIG2=DSIG2/SIG2
	  YET=0
	  IF(ABS(DSIG2(1)).GE.1D-4.OR.ABS(DSIG2(2)).GE.1D-4) YET=1
	ENDIF
	MSG=' '
	IF(YET.EQ.0) MSG='Converged'
	IF(SIG2(1).LE.0.OR.SIG2(2).LE.0) THEN
	  COEF=0
	ELSE
	  COEF=2*INTEG(0)/(2*PI*SQRT(SIG2(1)*SIG2(2)))
	ENDIF
	IF(IDEBUG1.NE.0.AND.MSGFL.GE.1) THEN
	  IF(ITR.EQ.0) WRITE(MSGFL,280) Z1,INTEG(0),PMAX,(DXY(I),I=1,2)
280     FORMAT(' (LSRCORE) At z=',1PD12.4,'  Integrated power=',1PD11.4,
     %      '  Maximum density=',1PD11.4,/,'        DXY=',1P2D11.4)
	  WRITE(MSGFL,300) ITR,(SIG2(I),I=1,2),COEF,MSG
300     FORMAT('  ITR=',I3,'  Sigxy^2=',1P2D12.4,
     %   '  Coef=',1PD12.4,2X,A)
      ENDIF
	SIGCORE=0
	IF(SIG2(1).LE.0.OR.SIG2(2).LE.0) GOTO 910
	SIGCORE=SQRT(SIG2)
	IF(YET.NE.0) GOTO 200
	IRTN=0
	RETURN
900   IRTN=200
      GOTO 990
910   IRTN=201
      GOTO 990
920   IRTN=202
      GOTO 990
990   RETURN
      END