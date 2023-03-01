	SUBROUTINE DEINT(FUNC,A,B,NF,LCONV,EPS,LSING,G,MSG,IRTN)
C                            Created 2000.05.14
C                            Revised 2003.05.12 (avoid ENTRY)
C  Double exponential integration
C   G(k) = integral F(x,k) dx  over a<x<b  for k=1,2,...NF
C  Method
C   * Change integration variable from x to t:
C       x = (A+B)/2+(B-A)/2*tanh[ALPHA*sinh(t)]    -infty < t < +infty
C   * Use trapezoidal formula for t-integration
C       Cut at abs(t) < TMAX
C   * Start with NT=NT0 points. Double NT every step until
C     the difference from the previous step becomes the required
C     error limit. (NT<=MT)
C   * The coefficients are computed at the first call.
C   * Can be used even when the integrant has a singularity
C     like  x**c (c>-1). (Better to set LSING=1 if c<0) 
C
C  Input
C    FUNC   Name of the subroutine which defines the integrant
C           (declared external)
C    A,B    Integration range
C    NF     Number of functions  (<=MF)
C    LCONV(k), EPS(k)  (k=1,NF) Define the convergence criterion for the
C           kth integral.
C            LCONV(k)=0:  do not check convergence
C                     1:  absolute error.  abs[G(k,n)-G(k,n-1)] < EPS(k)
C                     2:  relative error.
C                         2*abs[G(k,n)-G(k,n-1)]/[abs(G(k,n))+abs(G(k,n-1))] < EPS(k)
C                     3:  relative error for H(k), where H(k) is a complex
C                         number Re(H)=G(k), Im(H)=G(k+1).
C                         2*abs[H(k,n)-H(k,n-1)]
C                            /Sqrt[abs(H(k,n))**2+abs(H(k,n-1))**2] < EPS(k)
C                         (In this case LCONV(k+1) and EPS(k+1) is not used. 
C    LSING  0 or 1. The method can be applied even when the integrand has a
C           singularity like x**c (c>-1) at infinity. It is better to set LSING=1
C           in the case c<0. Then, the functional form of FUNC has to be different
C           (see below).
C    MSG    If >0, write messages on file #MSG
C  Output
C    G(k)   (k=1,NF)
C    IRTN   Return code
C
C  Calling sequence of the subroutine FUNC when LSING=0 must be:
C       CALL FUNC(X,NF,F)
C             Must give F(X,k) (k=1,NF)
C  When LSING=1, the calling sequence of the subroutine FUNC must be:
C       CALL FUNC(X,NF,F,LL)
C     When called with LL=0, Must give F(X,k) (k=1,NF)
C     When called with LL=1, Must give F(X+A,k) (k=1,NF)
C     When called with LL=2, Must give F(B-X,k) (k=1,NF)
	IMPLICIT NONE
	INTEGER NF,LCONV(NF),LSING,MSG,IRTN
	REAL*8 A,B,EPS(NF),G(NF)
	EXTERNAL FUNC
	
	INTEGER MT
	PARAMETER (MT=16384)
	INTEGER NCALL/0/,NT0
	REAL*8 X1(0:MT),WGT(0:MT),TMIN,TMAX,DT
	SAVE X1,WGT,TMIN,TMAX,DT,NCALL,NT0
	REAL*8 T,U,E,C

	INTEGER MF
	PARAMETER (MF=10)
	REAL*8 F(MF),G0(MF),ER(MF),ER1,GAV,BA
	INTEGER NT,I,I0,II,K,ITR,LC,KC,SKIP,MSG1
	CHARACTER*40 FMT,FMT0
	
      MSG1=MSG
      IF(MSG.LE.0) MSG1=6
      IF(NCALL.EQ.0) THEN
	  TMAX=4
	  TMIN=-4
	  DT=(TMAX-TMIN)/MT
	  DO 110 I=0,MT
	    T=TMIN+I*DT
	    U=SINH(T)
	    E=EXP(U)
	    C=E+1/E
	    X1(I)=E/C
	    WGT(I)=2/C**2*COSH(T)
110     CONTINUE
        NT0=32
C          must be 2**n
      ENDIF
	NCALL=NCALL+1
      IF(NF.LE.0.OR.NF.GT.MF) GOTO 900
	SKIP=0
	KC=0
	DO 120 K=1,NF
	  IF(SKIP.EQ.0) THEN
	    IF(LCONV(K).EQ.3) THEN
	      IF(K.EQ.NF) GOTO 910
	      IF(EPS(K).LE.0) GOTO 910
	      SKIP=1
	      KC=1
	    ELSEIF(LCONV(K).LT.0.OR.LCONV(K).GT.3) THEN
            GOTO 910
	    ELSEIF(LCONV(K).NE.0) THEN
	      KC=1
	    ENDIF
	  ELSE
	    SKIP=0
	  ENDIF
120   CONTINUE
      IF(KC.EQ.0) GOTO 920
	IF(MSG.GE.1) THEN
	  WRITE(FMT0,140) NF
140     FORMAT('(I6,2X,1P,',I3,'(D15.7,10X))')
	  WRITE(FMT,150) NF
150     FORMAT('(I6,2X,1P,',I3,'(D15.7,D10.3))')
        WRITE(MSG,160) (K,K=1,NF)
160     FORMAT(' #pnts',2X,100('  integral(',I2,')    error  ',:))
	ENDIF
	BA=B-A
	NT=NT0
	ITR=0
	II=MT/NT
	I0=0
	DO 180 K=1,NF
	  G(K)=0
180   CONTINUE
200   ITR=ITR+1
	DO 220 K=1,NF
	  G0(K)=G(K)
	  G(K)=0
220   CONTINUE
      DO 260 I=I0,MT,II
	  IF(LSING.EQ.0) THEN
	    CALL FUNC(A+BA*X1(I),NF,F)
	  ELSE
	    IF(X1(I).LE.0.1) THEN
	      CALL FUNC(BA*X1(I),NF,F,1)
	    ELSEIF(X1(I).GE.0.9) THEN
	      CALL FUNC(BA*X1(MT-I),NF,F,2)
	    ELSE
	      CALL FUNC(A+BA*X1(I),NF,F,0)
	    ENDIF
	  ENDIF
	  DO 240 K=1,NF
	    G(K)=G(K)+F(K)*WGT(I)
240     CONTINUE
260   CONTINUE
	DO 280 K=1,NF
	  G(K)=G(K)*(B-A)*II*DT
	  IF(ITR.NE.1) G(K)=(G0(K)+G(K))/2
280   CONTINUE
      LC=0
	IF(ITR.NE.1) THEN
	  LC=1
	  SKIP=0
	  DO 300 K=1,NF
	    ER(K)=ABS(G(K)-G0(K))
	    IF(SKIP.EQ.0) THEN
	      IF(LCONV(K).EQ.3) THEN
	        SKIP=1
	        ER1=ER(K)**2+(G(K+1)-G0(K+1))**2
	        GAV=(G(K)**2+G(K+1)**2+G0(K)**2+G0(K+1)**2)/2
	        IF(GAV.NE.0) THEN
                ER1=ER1/GAV
	          IF(ER1.GE.EPS(K)**2) LC=0
	        ELSE
	          LC=0
	        ENDIF
	      ELSE
	        IF(LCONV(K).EQ.2) THEN
	          GAV=(ABS(G(K))+ABS(G0(K)))/2
	          IF(GAV.NE.0) THEN
                  ER1=ER(K)/GAV
	            IF(ER1.GE.EPS(K)) LC=0
	          ELSE
	            LC=0
	          ENDIF
	        ELSE
	          IF(ER(K).GE.EPS(K)) LC=0
	        ENDIF
	      ENDIF
	    ELSE
	      SKIP=0
	    ENDIF
300     CONTINUE
	ENDIF
	IF(MSG.GE.1) THEN
	  IF(ITR.EQ.1) THEN
	    WRITE(MSG,FMT0) MT/II+1,(G(K),K=1,NF)
	  ELSE
	    WRITE(MSG,FMT) MT/II*2+1,(G(K),ER(K),K=1,NF)
	  ENDIF
	ENDIF
	IF(LC.EQ.0) THEN
	  IF(II.EQ.1) GOTO 940
	  IF(ITR.NE.1) II=II/2
	  I0=II/2
	  GOTO 200
	ENDIF
	IF(MSG.GE.1) THEN
	  WRITE(MSG,400)
400     FORMAT('    --- converged.')
      ENDIF
	RETURN
900   IRTN=1000
      WRITE(MSG1,905) MF
905   FORMAT(' (SUBR.DEINT) NF too large. Must be <=',I3)
      RETURN
910   IRTN=1001
      WRITE(MSG1,915)
915   FORMAT(' (SUBR.DEINT) Convergence criterion not given.')
      RETURN
920   IRTN=1002
      WRITE(MSG1,925)
925   FORMAT(' (SUBR.DEINT) Convergence criterion invalid.')
      RETURN
940	IRTN=100
	WRITE(MSG1,945)
945   FORMAT(' (SUBR.DEINT) does not converge.')
      RETURN
	END
	
	SUBROUTINE TESTDEINT
	IMPLICIT NONE
	INTEGER NF
	PARAMETER (NF=3)
	REAL*8 G(NF),EPS(NF)
	INTEGER K,MSG,IRTN,LCONV(NF)
	REAL*8 A,B
	EXTERNAL TESTFUNC
	
	MSG=6
	LCONV=0
	A=0
	B=2
	DO 200 K=1,NF
	  EPS(K)=1D-6
200   CONTINUE
	CALL DEINT(TESTFUNC,A,B,NF,LCONV,EPS,0,G,MSG,IRTN)
	PRINT *,' IRTN=',IRTN
	STOP
	END

	SUBROUTINE TESTFUNC(X,NF,G)
	IMPLICIT NONE
	INTEGER NF
	REAL*8 X,G(NF)
	REAL*8 PI/3.141592653589793238D0/
	G(1)=X
	G(2)=1/(1+X)
	G(3)=PI/2*SIN(PI/2*X)
	RETURN
	END

