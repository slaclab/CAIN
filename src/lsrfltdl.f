      SUBROUTINE LSRFLTDL(LSRDT1,TDL,WL,TDLDXY,NIT,IRTN)
C  Dilution by TDL on the file data
C  Fumdamental assumption
C    *  The input field more or less satisfies the Maxwell equation
C       in vaccumm. This means it must not be incoherent.
C       No plasma focusing.
C       Then, the profile must be something like
C           sigma = sqrt[ sigma0^2 + (abs(s)*eps/sigma0)^2 ]
C           s: distance from a point in the file data region
C           eps = (wave length) / (4*pi)
C    *  The file data expresses the field near the
C       focal point.
C
	USE LASRDATA
	IMPLICIT NONE
	INCLUDE 'include/ctrlcm.h'
	TYPE(LASERDATA) LSRDT1
	INTEGER NIT,IRTN
	REAL(8) TDL(2),WL,TDLDXY(2)

	INTEGER NDIM,ITYPE
	INTEGER IL,IZ,IR,IT(2),NJL,NJZ,NJT(2),NJT2,NDL,NDZ,NDT(2),
     %   IXY(2),J
	INTEGER N,I,JL,JZ,JT(2),JT2,K,N0,M1,M2,ISTAT,MMAX(2),JTSRC(2),
     %   N1,NSRC,MM
	INTEGER IWORK(0:2)
	REAL(8) X(2),SUM,X1AV(2),X2AV(2),D(2),
     %  COEF(0:2,2),MAT(0:2,0:2),WORK(0:2),SIG20(2),SIG2(2),F1,Z1,TWF
	REAL(8),ALLOCATABLE:: FF(:),FFF(:,:),PSAVE(:)
	REAL(8) PI/3.141592653589793238D0/

	NDIM=LSRDT1%NDIM
	ITYPE=LSRDT1%ITYPE
	K=0
	IL=0
	IZ=0
	NJL=1
	NDL=1
	NJT2=1
	N=1
	DO I=1,NDIM
	  IF(TABTYPE(ITYPE)(I:I).EQ.'L') THEN
	    IL=I
	    NJL=LSRDT1%NVAL(I)
	    NDL=N
	  ELSEIF(TABTYPE(ITYPE)(I:I).EQ.'Z') THEN
	    IZ=I
	    NDZ=N
	  ELSE
	    IF(K.GE.2) GOTO 900
	    K=K+1
	    IT(K)=I
		  IF(TABTYPE(ITYPE)(I:I).EQ.'R') IR=1
	    IF(TABTYPE(ITYPE)(I:I).EQ.'Y') THEN
	      IXY(K)=2
	    ELSE
	      IXY(K)=1
	    ENDIF
	    NDT(K)=N
	    NJT(K)=LSRDT1%NVAL(I)
	    NJT2=NJT2*NJT(K)
	  ENDIF
	  N=N*LSRDT1%NVAL(I)
	ENDDO
	NIT=K
	
	IF(IZ.EQ.0.OR.NIT.EQ.0) GOTO 900
	IF(IR.NE.0.AND.NIT.GE.2) GOTO 900
	ALLOCATE(PSAVE(NJT2),STAT=ISTAT)
	IF(ISTAT.NE.0) GOTO 920
	NJZ=LSRDT1%NVAL(IZ)

	IF(IR.NE.0) THEN
	  ALLOCATE(FFF(NJT(1),NJT(1)),STAT=ISTAT)
	  IF(ISTAT.NE.0) GOTO 940
	ENDIF
	DO JL=1,NJL
C         Find the required rms shift DXY = SIG0 * sqrt(TDL^2-1)
C         SIG0: minimum of actual minimum size and
C               minimum size evaluated from Rauleigh relation
	  N0=NDL*(JL-1)
	  COEF(0:2,1:NIT)=0
	  MAT(0:2,0:2)=0
	  SIG20(1:NIT)=1D60
	  DO JZ=1,NJZ
	    Z1=LSRDT1%VALMM(1,IZ)+LSRDT1%DVAL(IZ)*(JZ-1)
	    IF(LSRDT1%TWISTCOORD) THEN
	      TWF=MAX(1D0,ABS(Z1)/LSRDT1%ZR)
	      Z1=Z1*TWF
	    ENDIF
	    SUM=0
	    X1AV(1:NIT)=0
	    X2AV(1:NIT)=0
	    N1=N0+NDZ*(JZ-1)+1
	    DO JT2=1,NJT2
	      CALL LSRTYPEINDEX(1,NIT,NJT,JT2,JT,NDT,N1,N)
	      DO K=1,NIT
	        X(K)=LSRDT1%VALMM(1,IT(K))+LSRDT1%DVAL(IT(K))*(JT(K)-1)
	      ENDDO	      
	      F1=LSRDT1%PTP(N)
	      IF(IR.NE.0) THEN
	        IF(JT(1).EQ.1) THEN
	          F1=F1*0.25D0
	        ELSEIF(JT(1).EQ.NJT(1)) THEN
	          F1=F1*(JT(1)-1.5D0)/2D0
	        ELSE
	          F1=F1*(JT(1)-1)
	        ENDIF
	      ENDIF
	      SUM=SUM+F1
	      DO K=1,NIT
	        X1AV(K)=X1AV(K)+X(K)*F1
	        X2AV(K)=X2AV(K)+X(K)**2*F1
	      ENDDO
	    ENDDO
	    IF(SUM.EQ.0) CYCLE
	    X1AV(1:NIT)=X1AV(1:NIT)/SUM*TWF**2
	    X2AV(1:NIT)=X2AV(1:NIT)/SUM*TWF**4
	    IF(IR.NE.0) THEN
	      X2AV(1:NIT)=X2AV(1:NIT)/2	     !  <x^2>=<r^2>/2
	    ELSE
	      X2AV(1:NIT)=MAX(0D0,X2AV(1:NIT)-X1AV(1:NIT)**2)
	    ENDIF
	    SIG20(1:NIT)=MIN(SIG20(1:NIT),X2AV(1:NIT))
	    DO M1=0,2
	      COEF(M1,1:NIT)=COEF(M1,1:NIT)+X2AV(1:NIT)*Z1**M1
	      DO M2=0,2
	        MAT(M1,M2)=MAT(M1,M2)+Z1**(M1+M2)
	      ENDDO
	    ENDDO
	  ENDDO
C           Solve for Rayleigh relation sig^2 = c0 + c1*z + c2*z^2
	  DO K=1,NIT
	    IF(K.EQ.1) THEN
	      CALL DLUDCP(MAT(0:2,0:2),3,3,IWORK,WORK,IRTN)
	      IF(IRTN.NE.0) GOTO 910
	    ENDIF
	    CALL DLUBKS(MAT(0:2,0:2),3,3,IWORK,COEF(0:2,K))
	    IF(COEF(2,K).GT.0) THEN
	      SIG2(K)=(WL/(4*PI))**2/COEF(2,K)
	      SIG20(K)=MIN(SIG20(K),SIG2(K))
	    ENDIF
	    TDLDXY(K)=SQRT(SIG20(K)*MAX(0D0,TDL(IXY(K))**2-1D0))
	    D(K)=TDLDXY(K)/LSRDT1%DVAL(IT(K))
C           rms shift in units of mesh size
          MMAX(K)=INT(5.5D0*D(K)+1D0)+1
	  ENDDO
	  MM=MAX(MMAX(1),MMAX(2))
	  IF(IR.EQ.0) ALLOCATE(FF(0:MM),STAT=ISTAT)
	  IF(ISTAT.NE.0) GOTO 930
	  DO K=1,NIT
	    IF(D(K).LE.0) CYCLE
C        Create redistribution function FF(m) (sum FF = 1)
C           (Gaussian distribution of size TDLDXY in units of the mesh size)
          IF(IR.EQ.0) THEN
            CALL REDISTKERNEL1(MMAX(K),FF(0:MMAX(K)),D(K))
	    ELSE
            CALL REDISTKERNEL2(FFF,NJT(1),D(K),MMAX(K))
	    ENDIF
C        Redistribution
	    DO JZ=1,NJZ
	      N1=N0+NDZ*(JZ-1)+1
	      DO JT2=1,NJT2
	        CALL LSRTYPEINDEX(1,NIT,NJT,JT2,JT,NDT,N1,N)
	        PSAVE(JT2)=LSRDT1%PTP(N)
	        LSRDT1%PTP(N)=0
	      ENDDO
	      DO JT2=1,NJT2
	        CALL LSRTYPEINDEX(1,NIT,NJT,JT2,JTSRC,NDT,N1,NSRC)
	        IF(IR.EQ.0) THEN
	         DO J=MAX(1,JTSRC(K)-MMAX(K)),MIN(NJT(K),JTSRC(K)+MMAX(K))
	            N=NSRC+NDT(K)*(J-JTSRC(K))
	            LSRDT1%PTP(N)=LSRDT1%PTP(N)
     %              +FF(ABS(J-JTSRC(K)))*PSAVE(JT2)
	          ENDDO
	        ELSE
	         DO J=MAX(1,JTSRC(K)-MMAX(K)),MIN(NJT(K),JTSRC(K)+MMAX(K))
	            N=NSRC+NDT(K)*(J-JTSRC(K))
	            LSRDT1%PTP(N)=LSRDT1%PTP(N)
     %              +FFF(J,JTSRC(K))*PSAVE(JT2)
	          ENDDO
	        ENDIF
	      ENDDO
	    ENDDO
	  ENDDO
	  IF(IR.EQ.0) DEALLOCATE(FF,STAT=ISTAT)
	ENDDO
	DEALLOCATE(PSAVE,STAT=ISTAT)
	IF(IR.NE.0) DEALLOCATE(FFF,STAT=ISTAT)
	IRTN=0
	RETURN
900   WRITE(MSGFL,905) TABTYPE(ITYPE)
905   FORMAT(' (SUBR.LSRFLTDL) Invalid laser file data type. ',A)
      IRTN=1000
	GOTO 990
910   WRITE(MSGFL,915)
915   FORMAT(' (SUBR.LSRFLTDL) TDL correction of laser file data ',
     %    'failed.')
      IRTN=1010
	GOTO 990
920   WRITE(MSGFL,925) NJT2*8
925   FORMAT(' (SUBR.LSRFLTDL) Memory allocation failed. Size=',I10,
     %   ' bytes.')
      IRTN=1020
	GOTO 990
930   WRITE(MSGFL,935) (2*MM+1)*8
935   FORMAT(' (SUBR.LSRFLTDL) Memory allocation failed. Size=',I10,
     %   ' bytes.')
      IRTN=1030
	GOTO 990
940   WRITE(MSGFL,945) NJT(1)**2*8
945   FORMAT(' (SUBR.LSRFLTDL) Memory allocation failed. Size=',I10,
     %   ' bytes.')
      IRTN=1040
	GOTO 990
990   RETURN
	END
    
	SUBROUTINE LSRTYPEINDEX(L,ND,NN,J,JJ,NDT,N0,N)
	
C      L=1:    J  -> JJ, N
C        2:    JJ -> J, N
	IMPLICIT NONE
	INTEGER L,ND,NN(ND),J,JJ(ND),NDT(ND),N0,N
	INTEGER J1,K
	IF(L.EQ.1) THEN
	  J1=J-1
	  DO K=1,ND
	    JJ(K)=MOD(J1,NN(K))+1
	    J1=J1/NN(K)
	  ENDDO
	ELSE
	  J1=0
	  DO K=ND,1,-1
	    J1=J1*NN(K)+JJ(K)-1
	  ENDDO
	  J=J1+1
	ENDIF
	N=N0
	DO K=1,ND
	  N=N+NDT(K)*(JJ(K)-1)
	ENDDO
	RETURN
	END  

	SUBROUTINE REDISTKERNEL1(MM,FF,D)
C    FF(m) = integral (1-abs(x))*G(x-m) dx  -1<x<1
C            where G(x) = exp[-x^2/(2*D^2)]/sqrt(2*pi)/D
C       gives for M>=0.  For m<0, FF(-m)=FF(M) 
C       sum FF(m) over -infty < m < infty =1
C      FF(0) = 2*(I(0)-J(0))
C      FF(m) = (1+m)*I(m) - (m-1)*I(m-1) + J(m-1)-J(m)  (m>=1)
C         where  I(m) = integral G(x) dx over m<x<m+1
C                J(m) = integral x*G(x) dx over m<x<m+1
C                     = D^2*[G(m)-G(m+1)]
	IMPLICIT NONE
	INTEGER MM
	REAL(8) FF(0:MM),D
	INTEGER M,NDIV,I
	REAL(8) C0,DX,FINT0,FINT1,X1,F1,SUM
	REAL(8) PI/3.141592653589793238D0/

	FF=0
	C0=1D0/SQRT(2*PI)/D
	DX=0.1D0*D**2/MM
	NDIV=INT(1D0/DX+1)
	DX=1D0/NDIV
	DO M=0,MM
	  FINT0=0
	  FINT1=0
	  DO I=0,NDIV
	    X1=I*DX+M
	    F1=EXP(-0.5D0*(X1/D)**2)
	    IF(I.EQ.0.OR.I.EQ.NDIV) F1=0.5D0*F1
	    FINT0=FINT0+F1
	    FINT1=FINT1+X1*F1
	  ENDDO
	  FINT0=FINT0*C0*DX
	  FINT1=FINT1*C0*DX
	  FF(M)=FF(M)+(M+1)*FINT0-FINT1
	  IF(M.LT.MM) FF(M+1)=FF(M+1)-M*FINT0+FINT1
	ENDDO
C         Should not be renormalized, perhaps.
	SUM=0
	DO M=0,MM
	  SUM=SUM+FF(M)
	ENDDO
	SUM=SUM*2
C	FF(0:MM)= FF(0:MM)/SUM
	FF(0)=2*FF(0)

	RETURN
	END

	SUBROUTINE REDISTKERNEL2(FFF,N,D,MM)
C      2D radial version of REDISTKERNEL1
C   Fundamental expression is
C      F(r1) = Integral r2*dr2/(delta_r)^2 * K(r1,r2) * f(r2)
C      K(r1,r2) = (delta_r/d)^2 * exp[-(r1-r2)/2/d^2] * II0(r1*r2/d^2)
C                    where II0(x) = exp(-x)*I0(x) : Bessel func
C                    delta_r: mesh integrval
C   When r1=i*delta_r, r2=j*delta_r   (0<=i,j<=M=N-1)
C      F(i) = sum C(i,j)*f(j)
C                   abs(i-j)>MM is ignored.
C       C(i,j) = [min(m,j+1)^2-max(0,j-1)^2]*K(i,j)
C                + (j+1/2)*K(i,j+1) + (j-1/2)*K(i,j-1)
C                    (j<m)               (j>0)
C   This is obtained by linear interpolation in r^2.
C 
	IMPLICIT NONE
	INTEGER N,MM
	REAL(8) FFF(0:N-1,0:N-1),D
	INTEGER I,J,M
	REAL(8) DD,X1,X2,F1,F2
	REAL(8) DBESIK

	FFF=0
	DD=D**2
	M=N-1
	DO I=0,M
	  DO J=0,I
	    IF(ABS(I-J).GT.MM) CYCLE
	    X1=I*J/DD
	    X2=0.5D0*(I-J)**2/DD
          FFF(I,J)=EXP(-X2)*DBESIK(0,-1,X1)
	    IF(I.NE.J) FFF(J,I)=FFF(I,J)
	  ENDDO
	ENDDO
	DO I=0,M
	  F2=0
	  DO J=0,M
	    F1=F2
	    F2=FFF(I,J)
	    IF(J.EQ.0) THEN
	      FFF(I,J)=(F2+0.5D0*FFF(I,J+1))/6D0
	    ELSEIF(J.EQ.N) THEN
	      FFF(I,J)=((2*M-1)*F2+(M-0.5D0)*F1)/6D0
	    ELSE
	      FFF(I,J)=(4*J*F2+(J+0.5D0)*FFF(I,J+1)
     %                +(J-0.5D0)*F1)/6D0
	    ENDIF
	  ENDDO
	ENDDO
	RETURN
	END