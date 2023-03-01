C************************** SQP ************************************
C   26/05/92            MEMBER NAME  SQP      *.FORT     M  E2FORT
      SUBROUTINE SQP(FVAL,N,ME,M,X,OBJFUN,CONFUN,
     %       SCALE,GRAD,DDER,ITMAX,IT,MSGLVL0,MSGFL0,IRTN)
C
C SUCCESSIVE QUADRATIC PROGRAMMING (SQP) for solving nonlinear
C programming problem with constraints:
C   Minimize      F(x)        (x= N-dim vector)
C   Constraints   C(x,i) =0   (i=1,2,...ME)
C                 C(x,i)>=0   (i=ME+1,...M)
C       Subroutine QPQUAL is used.
C
C INPUT:
C  FVAL      User defined subroutine to calculate F(x) and C(x,i).
C            Declared external in the calling routine.  See below.
C  N         Number of variables   (>=1)
C  ME        Number of equality constraints  (>=0)
C  M         Total number of constraints  (>=1)
C  X(I) (I=1,N)  Initial value of vector x.
C  SCALE     logical. .TRUE. if automatic scaling of F and C is to be
C            done. Scaling of X is not performed. In any case, this
C            option is not powerful at all.  It is recommended to
C            scale F, C and X by your self and to set SCALE=.FALSE.
C  GRAD      logical. .TRUE. if dF/dx and dC/dx are calculated by the
C            user routine FVAL.  .FALSE. if they should be evaluated
C            using finite difference by SQP routine.
C  DDER      step of X for numerical differentiation
C            Effective when GRAD=.F. only.
C  ITMAX     Maximum number of iterations.
C            If <=0, default value MIN(200,MAX(100,20*(N+M))) is used.
C  MSGLVL0   Message level.
C            <0 :  no message
C             0 :  error message only
C             1 :  plus the results
C             2 :  plus iteration process
C             3 :  plus detail of iteration process
C  MSGFL0    Destination of message
C OUTPUT:
C  X(I) (I=1,N)   Optimal x.
C  OBJFUN    Value of F(X) at the optimal point.
C  CONFUN(I) (I=1,M)  Values of C(X,I) at the optimal point.
C  IT        number of major iterations
C  IRTN      return code
C
C Structure of FVAL
C   When GRAD=.TRUE.
C     SUBROUTINE FVAL(N,M,X,OBJFUN,CONFUN,K,OBJGRA,NA0,CONGRA)
C     IMPLICIT REAL*8 (A-H,O-Z)
C     REAL*8 X(N),CONFUN(M),OBJGRA(N),CONGRA(NA0,M)
C        If K=1 or 3, calculate
C             OBJFUN=F(X),  CONFUN(I)=C(X,I) (I=1,M)
C        If K=2 or 3, calculate
C             OBJGRA(J)=dF(X)/dX(J)  (J=1,N)
C             CONGRA(J,I)=dC(X,I)/dX(J)  ((I=1,M),J=1,N)
C        Caution: When K=1, derivatives MUST NOT be calculated.
C         (When K=2, F and C may be calculated but not used.)
C        The routine prepared for GRAD=.TRUE. can also be used for
C        GRAD=.FALSE. (In this case, FVAL is called with K=1 only).
C   When GRAD=.FALSE.
C     SUBROUTINE FVAL(N,M,X,OBJFUN,CONFUN)
C     IMPLICIT REAL*8 (A-H,O-Z)
C     REAL*8 X(N),CONFUN(M)
C        calculate  OBJFUN=F(X),  CONFUN(I)=C(X,I) (I=1,M)
C
C----------------------------------------------------
      IMPLICIT NONE
      INTEGER N,ME,M,ITMAX,IT,MSGLVL0,MSGFL0,IRTN
      REAL*8 X(N),OBJFUN,CONFUN(M),DDER
      LOGICAL SCALE,GRAD
      EXTERNAL FVAL
      INTEGER MSGLVL,MSGFL
      COMMON/SQPCOM/MSGLVL,MSGFL
      REAL(8), ALLOCATABLE:: OBJGRA(:),CONGRA(:,:),U(:),B(:,:),
     %  D1(:),D2(:),PK(:),AK(:,:),RHS(:),CONGR1(:,:),NEWX(:),
     %  U2(:),S(:),Y(:),CSCL(:),WW(:)
      INTEGER, ALLOCATABLE:: MJ(:)
      INTEGER MI,NA,MA,ISTAT

      MSGLVL=MSGLVL0
      MSGFL=MSGFL0
      MI=M-ME
      IF(N.LE.0.OR.ME.LT.0.OR.MI.LT.0) GOTO 900
      NA=N+2*ME+MI
      MA=3*ME+2*MI
      ALLOCATE(OBJGRA(NA),CONGRA(NA,MA),U(MA),B(NA,NA),D1(NA),D2(N),
     %  PK(N),AK(NA,M),RHS(MA),CONGR1(NA,M),NEWX(N),U2(M),S(N),
     %  Y(N),CSCL(M),WW(6*NA),MJ(MA),STAT=ISTAT)
      IF(ISTAT.NE.0) GOTO 920
      CALL SQP0(FVAL,N,ME,MI,M,X,OBJFUN,CONFUN,
     %   NA,MA,OBJGRA,CONGRA,U,B,D1,D2,
     %   PK,AK,RHS,CONGR1,NEWX,U2,S,Y,WW,
     %   MJ,CSCL,
     %   SCALE,GRAD,DDER,ITMAX,IT,IRTN)
CCC        PRINT '(1X,4(Z16,2X))',X
      RETURN
C
  900 IRTN=10000
      IF(MSGLVL.GE.0) WRITE(MSGFL,905)
  905 FORMAT(' (SUBR.SQP) INVALID PARAMETER')
      RETURN
  920 IRTN=10002
      IF(MSGLVL.GE.0) WRITE(MSGFL,925)
  925 FORMAT(' (SUBR.SQP) Work area allocation failed.')
      GOTO 1000
1000  DEALLOCATE(OBJGRA,CONGRA,U,B,D1,D2,
     %  PK,AK,RHS,CONGR1,NEWX,U2,S,
     %  Y,CSCL,MJ,STAT=ISTAT)
      RETURN
      END
C-------------------------- IPDEF -------------------------------
      FUNCTION IPDEF(IP,N)
      IPDEF=IP
      IP=IP+N
      RETURN
      END
C************************** SQP0 ***********************************
      SUBROUTINE SQP0(FVAL,N,ME,MI,M,X,OBJFUN,CONFUN,
     %   NA0,MA0,OBJGRA,CONGRA,U,B,D1,D2,
     %   PK,AK,RHS,CONGR1,NEWX,U2,S,Y,
     %   WW,MJ,CSCL,SCALE,GRAD,DDER,ITMAX,IT,IRTN)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(N),U(MA0),OBJGRA(NA0),CONFUN(M),
     %  CONGRA(NA0,MA0),B(NA0,NA0),D1(NA0),D2(N),
     %  PK(N),AK(NA0,M),RHS(MA0),
     %  CONGR1(NA0,M),NEWX(N),U2(M),S(N),
     %  Y(N),WW(6*NA0),CSCL(M)
      INTEGER MJ(MA0)
      LOGICAL GRAD,FEASBL,CORR,BASIC,SCALE
      REAL*8 EPS1/1D-3/,EPS2/1D-3/,EPS3/1D-6/,BOUND/1D20/
      REAL*8 PPINIT/1D2/,RHO/1D1/
      EXTERNAL FVAL
      INTEGER MSGLVL,MSGFL
      COMMON/SQPCOM/MSGLVL,MSGFL
C
      MSGQP=-1
C          (no message from call QPDUAL)
      NCAL=0
C
C--- INITIALIZE
      PENPAR=PPINIT
      ITMX=ITMAX
      IF(ITMAX.LE.0) ITMX=MIN(200,MAX(100,20*(N+M)))
      IT=0
      NQP=0
      DO 220 I=1,N
      DO 210 J=1,N
  210 B(I,J)=0
  220 B(I,I)=1D0
C--- SCALING
      IF(GRAD) THEN
        CALL FVAL(N,M,X,OBJFUN,CONFUN,3,OBJGRA,NA0,CONGRA)
      ELSE
        CALL FVAL(N,M,X,OBJFUN,CONFUN,1,DUMMY,1,DUMMY)
        CALL SQPDIF(FVAL,N,M,NA0,X,OBJGRA,CONGRA,DDER,
     %              WW(2*NA0+1),WW(3*NA0+1),WW(4*NA0+1),NCAL)
      ENDIF
      NCAL=NCAL+1
      IF(SCALE) THEN
        DO 240 I=1,M
        CSCL(I)=0
        DO 230 J=1,N
  230   CSCL(I)=MAX(CSCL(I),ABS(CONGRA(J,I)))
  240   IF(CSCL(I).LT.10.AND.CSCL(I).GT.0.1D0) CSCL(I)=1
        OSCL=0
        DO 250 J=1,N
  250   OSCL=MAX(OSCL,ABS(OBJGRA(J)))
        IF(OSCL.LT.10.AND.OSCL.GT.0.1D0) OSCL=1
        OBJFUN=OBJFUN/OSCL
        DO 260 J=1,N
  260   OBJGRA(J)=OBJGRA(J)/OSCL
        DO 270 I=1,M
        CONFUN(I)=CONFUN(I)/CSCL(I)
        DO 270 J=1,N
  270   CONGRA(J,I)=CONGRA(J,I)/CSCL(I)
      ELSE
        OSCL=1
        DO 280 I=1,M
  280   CSCL(I)=1
      ENDIF
      CALL SQPPVL(ME,MI,M,OBJFUN,CONFUN,PENPAR,PENFUN)
C--- ITERATION
  290 IF(MSGLVL.GE.2) CALL SQPPRT(N,ME,MI,M,X,U,OBJFUN,CONFUN,PENPAR,
     %             PENFUN,OSCL,CSCL,IT,NCAL)
C--- Check termination criteria
      CALL SQPTST(N,ME,M,NA0,X,U,PENFUN,OLDPF,CONFUN,
     %            OBJGRA,CONGRA,WW,BOUND,EPS1,EPS2,IRTN)
      IF(IRTN.EQ.0.OR.IRTN.EQ.4) GOTO 350
      IT=IT+1
C--- Solve QP subprogram
      DO 300 I=1,M
  300 RHS(I)=-CONFUN(I)
      CALL QPDUAL(N,ME,M,NA0,NA0,CONGRA,RHS,0D0,OBJGRA,B,D1,U,OBJVAL,
     %   SCALE,MJ,MSGQP,MSGFL,ITQP,IRTNQP)
      NQP=NQP+1
      IRTQP=MOD(IRTNQP,1000)
      IF(IRTQP.EQ.1.OR.IRTQP.EQ.2.OR.IRTQP.EQ.4) THEN
        IRTN=6
        GOTO 350
      ENDIF
      IF(IRTQP.EQ.3) THEN
        FEASBL=.FALSE.
C             Basic QP subproblem is not feasible>
C             Solve auxiliaru subproblem
        CALL SQPAUX(N,ME,MI,M,NA,MEA,MA,NA0,MA0,CONGRA,RHS,OBJGRA,
     %             B,PENPARM)
        CALL QPDUAL(NA,MEA,MA,NA0,NA0,CONGRA,RHS,0D0,OBJGRA,B,D1,U,
     %     OBLVAL,SCALE,MJ,MSGQP,MSGFL,ITQP,IRTNQP)
        NQP=NQP+1
        IF(IRTNQP.GT.0) THEN
          IRTN=6
          GOTO 350
        ENDIF
        BASIC=.TRUE.
        GOTO 320
      ELSE
        FEASBL=.TRUE.
      ENDIF
C--- Check if second-order correction is needed
      CALL SQPCHK(FVAL,N,M,NA0,X,CONFUN,CONGRA,DDER,CSCL,CONGR1,
     %    D1,AK,WW,WW(NA0+1),WW(2*NA0+1),WW(3*NA0+1),WW(4*NA0+1),
     %    WW(5*NA0+1),GRAD,CORR,NCAL)
      IF(.NOT.CORR) THEN
        BASIC=.TRUE.
        GOTO 320
      ENDIF
C--- Solve modified QP subproblem to obtain second-order correction
      DO 310 J=1,N
      PK(J)=OBJGRA(J)
      DO 310 I=1,M
  310 PK(J)=PK(J)+0.5D0*U(I)*(CONGR1(J,I)-CONGRA(J,I))
      CALL QPDUAL(N,ME,M,NA0,NA0,AK,RHS,0D0,PK,B,D2,U2,OBJVAL,
     %   SCALE,MJ,MSGQP,MSGFL,ITQP,IRTNQP)
      NQP=NQP+1
      IF(IRTNQP.EQ.0) THEN
        BASIC=.FALSE.
      ELSE
        BASIC=.TRUE.
      ENDIF
C--- Update penalty parameter
  320 IF(.NOT.FEASBL) GOTO 340
      UMAX=0
      DO 330 I=1,M
  330 UMAX=MAX(UMAX,ABS(U(I)))
      IF(UMAX.GT.PENPAR) THEN
        PENPAR=UMAX+RHO
        CALL SQPPVL(ME,MI,M,OBJFUN,CONFUN,PENPAR,PENFUN)
      ENDIF
C--- Perform one-dimensional search
  340 CALL SQPQPV(N,NA,NA0,OBJGRA,B,D1,PENPAR,QPOBJ,FEASBL)
      CALL SQPLSC(FVAL,N,ME,MI,M,X,NEWX,D1,D2,BASIC,OBJFUN,CONFUN,
     %  OSCL,CSCL,QPOBJ,PENPAR,PENFUN,OLDPF,EPS3,GRAD,IRTN,NCAL)
C--- Update current solution and approximate Hessian matrix
      CALL SQPUPD(FVAL,N,M,NA0,X,NEWX,OBJGRA,CONGRA,DDER,
     %      OSCL,CSCL,U,B,S,Y,
     %      WW,WW(NA0+1),WW(2*NA0+1),WW(3*NA0+1),WW(4*NA0+1),
     %      WW(5*NA0+1),GRAD,IT,NCAL)
      IF(IRTN.EQ.5.OR.IT.GT.ITMX) GOTO 350
      GOTO 290
C--- Print out results
  350 OBJFUN=OBJFUN*OSCL
      DO 360 J=1,N
  360 OBJGRA(J)=OBJGRA(J)*OSCL
      DO 370 I=1,M
      CONFUN(I)=CONFUN(I)*CSCL(I)
      DO 370 J=1,N
  370 CONGRA(J,I)=CONGRA(J,I)*CSCL(I)
      DO 380 I=1,M
  380 U(I)=U(I)*OSCL/CSCL(I)
      CALL SQPPVL(ME,MI,M,OBJFUN,CONFUN,PENPAR,PENFUN)
      IF(MSGLVL.GE.0)
     %  CALL SQPOUT(N,ME,MI,M,NA0,X,U,OBJFUN,CONFUN,OBJGRA,CONGRA,
     %    WW,PENPAR,PENFUN,IRTN,IRTNQP,IT,NQP,NCAL)
      RETURN
      END
C----------------------- SQPDIF ----------------------------------------
      SUBROUTINE SQPDIF(FVAL,N,M,NA0,X,OBJGRA,CONGRA,DDER,
     %                   Y,CY1,CY2,NCAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(N),Y(N),OBJGRA(N),CONGRA(NA0,M),CY1(M),CY2(M)
      DATA DELTA/1D-5/
      EXTERNAL FVAL
C
      DO 230 J=1,N
      DO 210 I=1,N
  210 Y(I)=X(I)
      IF(DDER.EQ.0) THEN
        STEP=DELTA*MAX(1D0,ABS(X(J)))
      ELSE
        STEP=DDER*MAX(1D0,ABS(X(J)))
      ENDIF
      DSTEP=2*STEP
      Y(J)=X(J)+STEP
      CALL FVAL(N,M,Y,FY1,CY1,1,DUMMY,1,DUMMY)
      Y(J)=X(J)-STEP
      CALL FVAL(N,M,Y,FY2,CY2,1,DUMMY,1,DUMMY)
      NCAL=NCAL+2
      OBJGRA(J)=(FY1-FY2)/DSTEP
      DO 220 K=1,M
  220 CONGRA(J,K)=(CY1(K)-CY2(K))/DSTEP
  230 CONTINUE
      RETURN
      END
C----------------------- SQPPVL ---------------------------------------
      SUBROUTINE SQPPVL(ME,MI,M,OBJFUN,CONFUN,PENPAR,PENFUN)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CONFUN(M)
C
      PENFUN=0
      DO 210 I=1,ME
  210 PENFUN=PENFUN+ABS(CONFUN(I))
      DO 220 I=1,MI
  220 PENFUN=PENFUN+ABS(MIN(0D0,CONFUN(ME+I)))
      PENFUN=OBJFUN+PENPAR*PENFUN
      RETURN
      END
C----------------------- SQPPRT -------------------------------------
      SUBROUTINE SQPPRT(N,ME,MI,M,X,U,OBJFUN,CONFUN,PENPAR,PENFUN,
     %     OSCL,CSCL,IT,NCAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(N),U(M),CONFUN(M),CSCL(M)
      INTEGER MSGLVL,MSGFL
      COMMON/SQPCOM/MSGLVL,MSGFL
C
      WRITE(MSGFL,200) IT,NCAL,PENFUN,PENPAR,OBJFUN*OSCL
  200 FORMAT(' Itr=',I5,' Ncal=',I5,' penal=',1PD11.4,
     % ' (pen.par=',1PD11.4,') obj=',1PD13.6)
      IF(MSGLVL.LE.2) RETURN
      IF(ME.GE.1)
     %  WRITE(MSGFL,220) 'Eq. constr.',(CONFUN(I)*CSCL(I),I=1,ME)
  220 FORMAT(3X,A,T17,1P5D11.4,:,/,(5X,1P6D11.4,:))
      IF(MI.GE.1) WRITE(MSGFL,220)
     %       'Ineq. constr.',(CONFUN(I)*CSCL(I),I=ME+1,ME+MI)
      WRITE(MSGFL,220) 'Variables',(X(J),J=1,N)
      WRITE(MSGFL,220) 'Lagr.multipl.',(U(I)*OSCL/CSCL(I),I=1,ME+MI)
      RETURN
      END
C----------------------- SQPTST -------------------------------------
      SUBROUTINE SQPTST(N,ME,M,NA0,X,U,PENFUN,OLDPF,CONFUN,
     %     OBJGRA,CONGRA,LAGGRA,BOUND,EPS1,EPS2,IRTN)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(N),U(M),CONFUN(M),OBJGRA(N),CONGRA(NA0,M),LAGGRA(N)
C--- Check boundedness
      DO 210 J=1,N
      IF(ABS(X(J)).LE.BOUND) GOTO 210
      IRTN=4
      RETURN
  210 CONTINUE
C--- Check Kuhn-Tucker equations
      DO 220 J=1,N
      LAGGRA(J)=OBJGRA(J)
      DO 220 I=1,M
  220 LAGGRA(J)=LAGGRA(J)-U(I)*CONGRA(J,I)
      DO 230 J=1,N
      IF(ABS(LAGGRA(J)).LE.EPS1*(1D0+ABS(OBJGRA(J)))) GOTO 230
      IRTN=2
      GOTO 270
  230 CONTINUE
C--- Check feasibility
      CMAX=0
      DO 240 I=1,ME
  240 CMAX=MAX(CMAX,ABS(CONFUN(I)))
      DO 250 I=ME+1,M
  250 CMAX=MAX(CMAX,-CONFUN(I))
      IF(CMAX.GT.EPS1) THEN
        IRTN=2
        GOTO 270
      ENDIF
C--- Check complementary slackness
      DO 260 I=ME+1,M
      IF(U(I).LE.EPS1.OR.CONFUN(I).LE.EPS1) GOTO 260
      IRTN=2
      GOTO 270
  260 CONTINUE
      IRTN=1
C--- Check the change in penalty function value
  270 IF(OLDPF-PENFUN.LT.EPS2*(1D0+ABS(PENFUN))) THEN
        IF(IRTN.EQ.2) RETURN
        IRTN=0
      ELSE
        IF(IRTN.EQ.1) RETURN
        IRTN=3
      ENDIF
      RETURN
      END
C----------------------- SQPAUX -------------------------------------
      SUBROUTINE SQPAUX(N,ME,MI,M,NA,MEA,MA,NA0,MA0,
     %     CONGRA,RHS,OBJGRA,B,PENPAR)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OBJGRA(NA0),CONGRA(NA0,MA0),RHS(MA0),B(NA0,NA0)
      DATA SMALL/1D-3/
      INTEGER MSGLVL,MSGFL
      COMMON/SQPCOM/MSGLVL,MSGFL
C
      IF(MSGLVL.GE.3) WRITE(MSGFL,100)
  100 FORMAT(' +++ Solve auxiliary QP subproblem since the basic',
     %  ' QP subprob is infeasible.')
      NAUX=2*ME+MI
      NA=N+NAUX
      MEA=ME
      MIA=MI+NAUX
      MA=ME+MIA
      DO 210 J=1,NAUX
  210 OBJGRA(N+J)=PENPAR
      DO 220 I=1,M
      DO 220 J=1,NAUX
  220 CONGRA(N+J,I)=0
      DO 230 I=1,ME
      CONGRA(N+I,I)=1
  230 CONGRA(N+ME+I,I)=-1
      DO 240 I=1,MI
  240 CONGRA(N+2*ME+I,ME+I)=1
      DO 250 I=1,NAUX
      RHS(M+I)=0
      DO 250 J=1,NA
  250 CONGRA(J,M+I)=0
      DO 260 I=1,NAUX
  260 CONGRA(N+I,M+I)=1
      DO 270 I=1,NAUX
      DO 270 J=1,N
      B(N+I,J)=0
  270 B(J,N+I)=0
      DO 290 I=1,NAUX
      DO 280 J=1,NAUX
  280 B(N+I,N+J)=0
  290 B(N+I,N+I)=SMALL
      RETURN
      END
C----------------------- SQPCHK ------------------------------------
      SUBROUTINE SQPCHK(FVAL,N,M,NA0,X,CONFUN,CONGRA,DDER,
     %   CSCL,CONGR1,D1,AK,W0,W1,W2,W3,W4,CON1,GRAD,CORR,NCAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(N),CONFUN(M),CONGRA(NA0,M),D1(N),AK(NA0,M),CSCL(M),
     %   W0(N),W1(N),W2(N),W3(N),W4(N),CON1(M),CONGR1(NA0,M)
      LOGICAL GRAD,CORR,EMPTY
      EXTERNAL FVAL
C     DATA EPS/0.1D0/
      DATA EPS/0.01D0/
      DATA SIGMA/0.05D0/
      INTEGER MSGLVL,MSGFL
      COMMON/SQPCOM/MSGLVL,MSGFL
C
      CORR=.FALSE.
C--- Conpute coefficient matrix of the constraints of the second-order
C--- correction subproblem
      DO 210 J=1,N
  210 W0(J)=X(J)+D1(J)
      IF(GRAD) THEN
        CALL FVAL(N,M,W0,OBJ1,CON1,3,W1,NA0,CONGR1)
      ELSE
        CALL FVAL(N,M,W0,OBJ1,CON1,1,DUMMY,1,DUMMY)
        CALL SQPDIF(FVAL,N,M,NA0,W0,W1,CONGR1,DDER,W2,W3,W4,NCAL)
      ENDIF
      NCAL=NCAL+1
      DO 220 I=1,M
      CON1(I)=CON1(I)/CSCL(I)
      DO 220 J=1,N
  220 CONGR1(J,I)=CONGR1(J,I)/CSCL(I)
      DO 230 I=1,M
      DO 230 J=1,N
  230 AK(J,I)=0.5D0*(CONGRA(J,I)+CONGR1(J,I))
C--- Compare the agreements with the active constraints
      CAMAX=0
      CDMAX=0
      EMPTY=.TRUE.
      DO 250 I=1,M
      CI=CONFUN(I)
      IF(ABS(CI).GE.EPS) GOTO 250
      EMPTY=.FALSE.
      CA=CI-CON1(I)
      CD=CA
      DO 240 J=1,N
      CA=CA+AK(J,I)*D1(J)
  240 CD=CD+CONGRA(J,I)*D1(J)
      CA=ABS(CA)
      CD=ABS(CD)
      CAMAX=MAX(CA,CAMAX)
      CDMAX=MAX(CD,CDMAX)
  250 CONTINUE
      IF(EMPTY) RETURN
      IF(CAMAX.LT.SIGMA*CDMAX) THEN
        CORR=.TRUE.
        IF(MSGLVL.GE.3) WRITE(MSGFL,300)
  300   FORMAT(' ++ Solve modified QP subproblem to obtain ',
     %         'second-order correction.')
      ENDIF
      RETURN
      END
C----------------------- SQPQPV -------------------------------------
      SUBROUTINE SQPQPV(N,NA,NA0,OBJGRA,B,D1,PENPAR,QPOBJ,FEASBL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 OBJGRA(N),B(NA0,NA0),D1(NA0)
C         B(NA0,N) replaced by B(NA0,NA0). Jun23.2003
      LOGICAL FEASBL
C
      QPOBJ=0
      DO 210 J=1,N
      QPOBJ=QPOBJ+OBJGRA(J)*D1(J)
      DO 210 I=1,N
  210 QPOBJ=QPOBJ+0.5D0*D1(I)*B(I,J)*D1(J)
      IF(FEASBL) RETURN
      DO 220 J=N+1,NA
  220 QPOBJ=QPOBJ+PENPAR*D1(J)
      RETURN
      END
C----------------------- SQPLSC ------------------------------------
      SUBROUTINE SQPLSC(FVAL,N,ME,MI,M,X,NEWX,D1,D2,BASIC,OBJFUN,CONFUN,
     %   OSCL,CSCL,QPOBJ,PENPAR,PENFUN,OLDPF,EPS,GRAD,IRTN,NCAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      EXTERNAL FVAL
      REAL*8 X(N),NEWX(N),D1(N),D2(N),CSCL(M),CONFUN(M),MINST,NEWPF
      LOGICAL BASIC,GRAD
      DATA BETA/0.01D0/,GAMMA/0.5D0/
      INTEGER MSGLVL,MSGFL
      COMMON/SQPCOM/MSGLVL,MSGFL
C
C--- Compute the norm of the dirsction vector D1 and the minimum
C--- stepsize MINST
      D1NORM=0
      DO 210 J=1,N
  210 D1NORM=D1NORM+D1(J)**2
      IF(D1NORM.GT.1) THEN
        D1NORM=SQRT(D1NORM)
        MINST=EPS/D1NORM
      ELSE
        MINST=EPS
      ENDIF
C--- Predicted reduction in the penalty function value
      PRED=OBJFUN+QPOBJ-PENFUN
C--- Initially choose unit stepsize
      ST=1
  220 DO 230 J=1,N
      IF(BASIC) THEN
        NEWX(J)=X(J)+ST*D1(J)
      ELSE
        NEWX(J)=X(J)+ST*D1(J)+ST*ST*(D2(J)-D1(J))
      ENDIF
  230 CONTINUE
      IF(GRAD) THEN
        CALL FVAL(N,M,NEWX,OBJFUN,CONFUN,1,DUMMY,1,DUMMY)
      ELSE
        CALL FVAL(N,M,NEWX,OBJFUN,CONFUN,1,DUMMY,1,DUMMY)
      ENDIF
      NCAL=NCAL+1
      OBJFUN=OBJFUN/OSCL
      DO 240 I=1,M
  240 CONFUN(I)=CONFUN(I)/CSCL(I)
      CALL SQPPVL(ME,MI,M,OBJFUN,CONFUN,PENPAR,NEWPF)
      IF(NEWPF.LT.PENFUN+BETA*ST*PRED) GOTO 250
      ST=GAMMA*ST
      IF(ST.GT.MINST) GOTO 220
      IRTN=5
  250 OLDPF=PENFUN
      PENFUN=NEWPF
      IF(MSGLVL.LE.2) RETURN
      IF(BASIC) THEN
        WRITE(MSGFL,300)
  300   FORMAT(' ++ One-dim. search --- basic search direction')
      ELSE
        WRITE(MSGFL,310)
  310   FORMAT(' ++ One-dim. search --- second order correction')
      ENDIF
      WRITE(MSGFL,320) D1NORM,ST
  320 FORMAT('   Norm of basic search direc.=',1PD11.4,
     %  '  stepsize=',1PD11.4)
      RETURN
      END
C----------------------- SQPUPD ---------------------------------------
      SUBROUTINE SQPUPD(FVAL,N,M,NA0,X,NEWX,OBJGRA,CONGRA,DDER,
     %   OSCL,CSCL,U,B,S,Y,BS,LG,LGNEW,W3,W4,W5,GRAD,IT,NCAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(N),NEWX(N),OBJGRA(N),CONGRA(NA0,M),CSCL(M),U(M),
     %   B(NA0,NA0),LGNEW(N),LG(N),S(N),Y(N),BS(N),W3(N),W4(N),W5(N)
C         B(NA0,M) replaced by B(NA0,NA0). Jun23.2003
      LOGICAL GRAD
      EXTERNAL FVAL
      DATA EPS/1D-5/
C
C--- Compute the gradient of the Lagrangian at X
      DO 210 J=1,N
      LG(J)=OBJGRA(J)
      DO 210 I=1,M
  210 LG(J)=LG(J)-U(I)*CONGRA(J,I)
      IF(GRAD) THEN
        CALL FVAL(N,M,NEWX,DUMMY,W1,2,OBJGRA,NA0,CONGRA)
      ELSE
        CALL SQPDIF(FVAL,N,M,NA0,NEWX,OBJGRA,CONGRA,DDER,W3,W4,W5,NCAL)
      ENDIF
      NCAL=NCAL+1
      DO 220 J=1,N
      OBJGRA(J)=OBJGRA(J)/OSCL
      DO 220 I=1,M
  220 CONGRA(J,I)=CONGRA(J,I)/CSCL(I)
C--- Reset the approximate Hessian to the identity matrix
      NN=5*N
      IF(MOD(IT,NN).EQ.0) THEN
        DO 235 I=1,N
        DO 230 J=1,N
  230   B(I,J)=0
  235   B(I,I)=1
        GOTO 310
      ENDIF
C--- Compute the gradient of the Lagrangian at NEWX
      DO 240 J=1,N
      LGNEW(J)=OBJGRA(J)
      DO 240 I=1,M
  240 LGNEW(J)=LGNEW(J)-U(I)*CONGRA(J,I)
C--- Compute vectors S,Y,BS.
      DO 250 J=1,N
      S(J)=NEWX(J)-X(J)
  250 Y(J)=LGNEW(J)-LG(J)
      DO 260 I=1,N
      BS(I)=0
      DO 260 J=1,N
  260 BS(I)=BS(I)+B(I,J)*S(J)
C--- Check if current matrix B is a sufficiently good approximation
C--- of Hessian of Lagrangian
      WMAX=0
      DO 270 J=1,N
  270 WMAX=MAX(WMAX,ABS(Y(J)-BS(J)))
      IF(WMAX.LT.EPS) GOTO 310
C--- Modify vector Y if necessary in order to maintain positive
C--- definiteness of matrix B
      SY=0
      SBS=0
      DO 280 J=1,N
      SY=SY+S(J)*Y(J)
  280 SBS=SBS+S(J)*BS(J)
      IF(SBS.LT.EPS**2) GOTO 310
      IF(SY.LT.0.2D0*SBS) THEN
        THETA=0.8D0*SBS/(SBS-SY)
        THETA0=1D0-THETA
        SY=0
        DO 290 J=1,N
        Y(J)=THETA*Y(J)+THETA0*BS(J)
  290   SY=SY+S(J)*Y(J)
      ENDIF
C--- Update matrix B using bfgs formula
      C1=1D0/SY
      C2=1D0/SBS
      DO 300 I=1,N
      DO 300 J=1,I
      B(I,J)=B(I,J)+C1*Y(I)*Y(J)-C2*BS(I)*BS(J)
  300 B(J,I)=B(I,J)
C--- Update current solution
  310 DO 320 J=1,N
  320 X(J)=NEWX(J)
      RETURN
      END
C----------------------- SQPOUT ------------------------------------
      SUBROUTINE SQPOUT(N,ME,MI,M,NA0,X,U,OBJFUN,CONFUN,OBJGRA,
     %   CONGRA,LAGGRA,PENPAR,PENFUN,IRTN,IRTNQP,IT,NQP,NCAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(N),U(M),CONFUN(M),OBJGRA(N),CONGRA(NA0,M),LAGGRA(N)
      INTEGER MSGLVL,MSGFL
      COMMON/SQPCOM/MSGLVL,MSGFL
C
      IF(MSGLVL.LE.0.AND.IRTN.EQ.0) RETURN
      DO 210 J=1,N
      LAGGRA(J)=OBJGRA(J)
      DO 210 I=1,M
  210 LAGGRA(J)=LAGGRA(J)-U(I)*CONGRA(J,I)
      WRITE(MSGFL,280) N,ME,MI,IRTN,IT,NCAL,NQP
  280 FORMAT(/,' ******* RESULTS ******* ',
     %  '  N=',I3,'  ME=',I3,'  MI=',I3,'  IRTN=',I5,/,
     %  ' #iter=',I5,' #call=',I5,' #QP problems=',I5)
      IF(IRTN.EQ.0) THEN
        WRITE(MSGFL,300)
  300   FORMAT(' Optimal solution found.')
      ELSEIF(IRTN.EQ.1) THEN
        WRITE(MSGFL,310)
  310   FORMAT(' K-T condition is satisfied but penalty func. is still',
     %         ' decreasing.')
      ELSEIF(IRTN.EQ.2) THEN
        WRITE(MSGFL,320)
  320   FORMAT(' Penalty function is small enough but K-T condition ',
     %         'is violated.')
      ELSEIF(IRTN.EQ.3) THEN
        WRITE(MSGFL,330)
  330   FORMAT(' Penalty function is still decreasing. K-T condition ',
     %         'is violated.')
      ELSEIF(IRTN.EQ.4) THEN
        WRITE(MSGFL,340)
  340   FORMAT(' Generated sequence is regarded as divergent.')
      ELSEIF(IRTN.EQ.5) THEN
        WRITE(MSGFL,350)
  350   FORMAT(' Change of variables became too small.')
      ELSEIF(IRTN.EQ.6) THEN
        WRITE(MSGFL,360) IRTNQP
  360   FORMAT(' QP subproblem failed. IRTNQP=',I8)
      ENDIF
C
      IF(MSGLVL.LE.0) RETURN
      WRITE(MSGFL,380) OBJFUN,PENFUN,PENPAR
  380 FORMAT(' Obj.fun.=',1PD12.5,'  Pen.fun.=',1PD12.5,
     %  ' (Pen.param=',1PD11.4,')')
      WRITE(MSGFL,400) (J,X(J),LAGGRA(J),J=1,N)
  400 FORMAT('     Variables  (grad of Lagrangian)',/,
     %  2('  X(',I2,')=',1PD11.4,'(',1PD13.4,')',:))
      IF(ME.NE.0) WRITE(MSGFL,420) (I,CONFUN(I),U(I),I=1,ME)
  420 FORMAT(' Equality constraints and Lagrange multiplier',/,
     %  2('  C(',I2,')=',1PD11.4,'(',1PD13.4,')',:))
      IF(MI.NE.0) WRITE(MSGFL,430) (I,CONFUN(I),U(I),I=ME+1,ME+MI)
  430 FORMAT(' Inequality constraints and Lagrange multiplier',/,
     %  2('  C(',I2,')=',1PD11.4,'(',1PD13.4,')',:))
      RETURN
      END

C************************* QPDUAL ************************************
      SUBROUTINE QPDUAL(N,ME,M,NADIM,NGDIM,A,B,F0,C,G,X,V,FMIN,
     %       SCALE,ACTIV,MSGLVL0,MSGFL0,IT,IRTN)
C Quadratic Programming using dual method
C  number of variables=N,
C  number of equality constraints=ME
C  number of inequality constraints=M-ME
C Given N*N positive definite symmetric matrix G, N-dim vector C,
C N*M matrix A and M-dim vector B, the program
C Minimizes  the objective function  F=F0+C*X+0.5*XT*G*X
C   subject to  sum X(i)*A(i,j) (i=1..N) =  B(j)   (j=1...ME)
C               sum X(i)*A(i,j) (i=1..N) >= B(j)   (j=ME+1...M)
C                         written by M.Fukushima
C
C INPUT:
C  N     number of variables  (>=1)
C  ME    number of equality constraints  (>=0)
C  M     total number of constraints  (>=ME, >=1)
C  NADIM,NGDIM   first dimension of A and G in the calling routine
C  A     constraint matrix
C  B     r.h.s. of constraints
C  F0    constant term of the objective function.
C        It does not affect the optimization. Needed for printing only.
C  C     first derivative of the objective function
C  G     second derivative of the objective function
C  SCALE   logical.  .TRUE., if automatic scaling is to be done.
C  MSGLVL0   message level.
C        <0 :  no message
C         0 :  error message only
C         1 :  plus the results
C         2 :  plus iteration process
C OUTPUT:
C  X     N-dim vector.  Solution.
C  IT    number of iteration steps
C  V     M-dim vector.  Lagrange multiplier.
C  FMIN  Minimum value of F.
C  ACTIV(I)  (I=1,M)  Integer.  Non-zero if I-th constraint is active.
C  IRTN  return code  IRTN=1000*m+n
C      n=0:    solution obtained.
C        1:    matrix G is not positive definite. (pivot=0 at m-th
C              diagonal element.)
C        2:    equality constraints are not independent. detected at
C              m-th column.
C        3:    the problem is not feasible.
C        4:    number of iterations exceeded the limit
C      999:    work area is not sufficient.

      IMPLICIT NONE
      INTEGER N,ME,M,NADIM,NGDIM,MSGLVL0,MSGFL0,IT,IRTN
      REAL*8 A(NADIM,M),B(M),C(N),G(NGDIM,N),X(N),V(M)
      INTEGER ACTIV(M)
      LOGICAL SCALE
      INTEGER MSGLVL,MSGFL
      COMMON/QPDUALCM/MSGLVL,MSGFL
      REAL(8), ALLOCATABLE:: L(:,:),Q(:,:),W(:),AS(:),D1(:),D2(:),ZV(:),
     %   RV(:),R(:,:),N1(:,:),N2(:,:),SCL(:),RES(:)
      INTEGER, ALLOCATABLE:: JSET(:)
      INTEGER ISTAT
	REAL(8) F0,FMIN

      MSGLVL=MSGLVL0
      MSGFL=MSGFL0
      ALLOCATE(L(N,N),Q(N,N),W(N),AS(N),D1(N),D2(N),ZV(N),RV(M),
     %   R(N,N),N1(N,N),N2(N,N),SCL(M),RES(N+M),JSET(M),STAT=ISTAT)
      IF(ISTAT.NE.0) THEN
        WRITE(MSGFL,100)
100     FORMAT(' (SUBR.QPDUAL) Work area allocation failed.')
        GOTO 1000
      ENDIF
      CALL QPDUL0(N,ME,M,NADIM,NGDIM,A,B,F0,C,G,X,V,L,Q,W,AS,
     %   D1,D2,ZV,RV,R,N1,N2,JSET,ACTIV,
     %  IRTN,IT,FMIN,SCALE,SCL,RES)

1000  DEALLOCATE(L,Q,W,AS,D1,D2,ZV,RV,R,N1,N2,SCL,RES,JSET,STAT=ISTAT)
      RETURN
      END
C************************* QPDUL0 **********************************
      SUBROUTINE QPDUL0(N,ME,M,NADIM,NGDIM,A,B,F0,C,G,X,V,L,Q,W,AS,
     %   D1,D2,ZV,RV,R,N1,N2,JSET,MJ,IRTN,IT,FMIN,SCALE,SCL,RES)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(NADIM,M),B(M),C(N),G(NGDIM,N),X(N),V(M),R(N,N),
     %   N1(N,N),N2(N,N),L(N,N),W(N),Q(N,N),AS(N),
     %   D1(N),D2(N),ZV(N),RV(M),RES(N+M),SCL(M)
      INTEGER JSET(M),MJ(M)
      LOGICAL OPTM,FEASBL,JOPT,SCALE
      CHARACTER*2 CHAR
      DATA ZERO/1D-12/
      INTEGER MSGLVL,MSGFL
      COMMON/QPDUALCM/MSGLVL,MSGFL
C--- Symmetrize matrix G
      DO 120 I=2,N
      DO 120 J=1,I-1
      IF(G(I,J).EQ.G(J,I)) GOTO 120
      IF(G(I,J).EQ.0.AND.G(J,I).NE.0) G(I,J)=G(J,I)
      IF(G(I,J).NE.0.AND.G(J,I).EQ.0) G(J,I)=G(I,J)
  120 CONTINUE
C--- Print input data
      IF(MSGLVL.GE.2) THEN
        WRITE(MSGFL,150) N,ME,M
  150   FORMAT(' ***** QPDUAL *****   N=',I3,' ME=',I3,' M=',I3)
        WRITE(MSGFL,155) (C(I),I=1,N)
  155   FORMAT('  Gradient of objevtive function',/,(1X,1P6D11.4,:))
        WRITE(MSGFL,160)
  160   FORMAT('  Second order derivative of objective function')
        DO 170 I=1,N
        WRITE(MSGFL,165) (G(I,J),J=1,N)
  165   FORMAT(1X,1P6D11.4)
  170   CONTINUE
        WRITE(MSGFL,175)
  175   FORMAT('  Constraints')
        DO 200 J=1,M
        WRITE(MSGFL,180) (A(I,J),I=1,N)
  180   FORMAT(1X,1P6D11.4)
        CHAR='= '
        IF(J.GT.ME) CHAR='>='
        WRITE(MSGFL,190) CHAR,B(J)
  190   FORMAT(53X,A2,1PD11.4)
  200   CONTINUE
      ENDIF
C--- SCALING
      F00=F0
      IF(SCALE) THEN
        OBJSCL=0
        DO 210 J=1,N
  210   OBJSCL=MAX(OBJSCL,ABS(C(J)))
        OBJSCL=OBJSCL/M
        IF(OBJSCL.LT.10.AND.OBJSCL.GT.0.1D0) OBJSCL=1
        F00=F00/OBJSCL
        DO 220 J=1,N
        C(J)=C(J)/OBJSCL
        DO 220 I=1,N
  220   G(I,J)=G(I,J)/OBJSCL
        DO 250 I=1,M
        SCL(I)=0
        DO 230 J=1,N
  230   SCL(I)=MAX(SCL(I),ABS(A(J,I)))
        IF(SCL(I).LT.ZERO) GOTO 920
        IF(SCL(I).LT.10.AND.SCL(I).GT.0.1D0) SCL(I)=1
        DO 240 J=1,N
  240   A(J,I)=A(J,I)/SCL(I)
        B(I)=B(I)/SCL(I)
  250   CONTINUE
      ELSE
        OBJSCL=1
        DO 260 I=1,M
  260   SCL(I)=1
      ENDIF
C--- FIND AN INITIAL SOLUTION
      CALL QPINIT(N,ME,M,NADIM,NGDIM,A,B,C,G,X,V,MJ,JSET,NJ,R,N1,N2,
     %       L,W,Q,IRTINI,ZERO)
      IF(IRTINI.NE.0) GOTO 930
      IT=0
      ITMAX=10*(N+M)
      OPTM=.FALSE.
      FEASBL=.TRUE.
      IF(MSGLVL.GE.2)
     %   CALL QPPRNT(N,M,NADIM,NGDIM,A,B,F00,C,G,X,V,RES,JSET,NJ,
     %               OBJSCL,SCL,IT)
C--- CHECK OPTIMALITY
  270 CALL QPCOPT(N,ME,M,NADIM,A,B,X,MJ,JS,CS,OPTM,ZERO)
      IF(OPTM) THEN
        IRTN=0
        DO 275 J=1,N
        C(J)=C(J)*OBJSCL
        DO 275 I=1,N
  275   G(I,J)=G(I,J)*OBJSCL
        DO 280 I=1,M
        B(I)=B(I)*SCL(I)
        DO 280 J=1,N
  280   A(J,I)=A(J,I)*SCL(I)
        DO 285 I=1,M
  285   V(I)=V(I)*OBJSCL/SCL(I)
        FMIN=F0
        DO 290 I=1,N
        FMIN=FMIN+C(I)*X(I)
        DO 290 J=1,N
  290   FMIN=FMIN+0.5D0*G(I,J)*X(I)*X(J)
        IF(MSGLVL.GE.1)
     %    CALL QPOUT(N,ME,M,NADIM,NGDIM,A,B,FMIN,C,G,X,V,RES,IT)
        RETURN
      ENDIF
C--- UPDATE THE CURRENT SOLUTION
  310 IT=IT+1
      CALL QPUPD(N,ME,M,NADIM,A,B,X,V,MJ,JSET,NJ,R,N1,N2,AS,D1,D2,ZV,
     %            RV,JS,CS,JOPT,FEASBL,ZERO)
      IF(.NOT.FEASBL) GOTO 910
      IF(MSGLVL.GE.2)
     %   CALL QPPRNT(N,M,NADIM,NGDIM,A,B,F00,C,G,X,V,RES,JSET,NJ,
     %               OBJSCL,SCL,IT)
      IF(IT.GT.ITMAX) GOTO 900
      IF(JOPT) GOTO 270
      GOTO 310
C
  900 IRTN=4
      IF(MSGLVL.GE.0) WRITE(MSGFL,905) ITMAX
  905 FORMAT(' (SUBR.QPDUAL) ITERATION EXCEEDED THE LIMIT',I7)
      RETURN
  910 IRTN=3
      IF(MSGLVL.GE.0) WRITE(MSGFL,915)
  915 FORMAT(' (SUBR.QPDUAL) Problem is not feasible.')
      RETURN
  920 IRTN=2+1000*I
      IF(MSGLVL.GE.0) WRITE(MSGFL,925) I
  925 FORMAT(' (SUBR.QPDUAL)',I4,'th COLUMN OF A IS ZERO')
      RETURN
  930 IRTN=IRTINI
      RETURN
      END
C--------------------- QPINIT ----------------------------------
      SUBROUTINE QPINIT(N,ME,M,NADIM,NGDIM,A,B,C,G,X,V,MJ,JSET,NJ,R,
     %      N1,N2,L,W,Q,IRTN,ZERO)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(NADIM,M),B(M),C(N),G(NGDIM,N),L(N,N),W(N),
     %       Q(N,N),R(N,N),N1(N,N),N2(N,N),X(N),V(M)
      INTEGER JSET(M),MJ(M)
      LOGICAL TRANS
      INTEGER MSGLVL,MSGFL
      COMMON/QPDUALCM/MSGLVL,MSGFL
C
      IRTN=0
C--- CHOLESKI DECOMPOSITION OF G
      CALL QPCHDC(N,NGDIM,G,L,IRTN1,ZERO)
      IF(IRTN1.NE.0) GOTO 940
C--- INITIALIZE THE INDEX SET J
      NJ=ME
      DO 210 I=1,M
  210 MJ(I)=0
      DO 220 I=1,ME
      JSET(I)=I
  220 MJ(I)=1
C--- COMPUTE BsubJ=(inverse of L)*AsubJ
      DO I=1,NJ
        DO J=1,N
          W(J)=A(J,I)
	  ENDDO
        TRANS=.FALSE.
        CALL QPFWSB(N,N,L,TRANS,W)
        DO J=1,N
          R(J,I)=W(J)
	  ENDDO
      ENDDO
C--- QR decomposition of matrix BsubJ
      CALL QPQRDC(N,NJ,R,Q,JRANK,ZERO)
      IF(JRANK.NE.0) GOTO 950
C--- Compute matrices N1 and N2
      DO 290 I=1,NJ
      DO 270 J=1,N
  270 W(J)=Q(J,I)
      TRANS=.TRUE.
      CALL QPBWSB(N,N,L,TRANS,W)
      DO 280 J=1,N
  280 N1(J,I)=W(J)
  290 CONTINUE
      DO 320 I=1,N-NJ
      DO 300 J=1,N
  300 W(J)=Q(J,NJ+I)
      TRANS=.TRUE.
      CALL QPBWSB(N,N,L,TRANS,W)
      DO 310 J=1,N
  310 N2(J,I)=W(J)
  320 CONTINUE
C--- INITIAL SOLUTION X
      IF(NJ.EQ.0) THEN
        DO 330 J=1,N
  330   X(J)=0
        GOTO 360
      ENDIF
      DO 340 I=1,NJ
  340 W(I)=B(I)
      TRANS=.TRUE.
      CALL QPFWSB(NJ,N,R,TRANS,W)
      DO 350 J=1,N
      X(J)=0
      DO 350 I=1,NJ
  350 X(J)=X(J)+N1(J,I)*W(I)
  360 DO 370 I=1,N-NJ
      W(I)=0
      DO 370 J=1,N
  370 W(I)=W(I)+N2(J,I)*C(J)
      DO 380 J=1,N
      DO 380 I=1,N-NJ
  380 X(J)=X(J)-N2(J,I)*W(I)
C--- Initial multipliers V
      DO 390 I=1,M
  390 V(I)=0
      IF(NJ.EQ.0) RETURN
      DO 400 I=1,NJ
  400 V(I)=B(I)
      TRANS=.TRUE.
      CALL QPFWSB(NJ,N,R,TRANS,V)
      TRANS=.FALSE.
      CALL QPBWSB(NJ,N,R,TRANS,V)
      DO 410 I=1,NJ
      W(I)=0
      DO 410 J=1,N
  410 W(I)=W(I)+N1(J,I)*C(J)
      TRANS=.FALSE.
      CALL QPBWSB(NJ,N,R,TRANS,W)
      DO 420 I=1,NJ
  420 V(I)=V(I)+W(I)
      RETURN
C
  940 IRTN=1+IRTN1*1000
      IF(MSGLVL.GE.0) WRITE(MSGFL,945)
  945 FORMAT(' (SUBR.QPINIT) Matrix G not positive definite.')
      RETURN
  950 IRTN=2+JRANK
      IF(MSGLVL.GE.0) WRITE(MSGFL,955) JRANK
  955 FORMAT(' (SUBR.QPINIT) Rank deficiency at column',I4,' of A.')
      RETURN
      END
C------------------------- QPCHDC -----------------------------------
      SUBROUTINE QPCHDC(N,NGDIM,G,GL,IRTN,ZERO)
C Choleski decomposition  G=GL*(transpose of GL)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION G(NGDIM,N),GL(N,N)
      DO 240 J=1,N
      GL(J,J)=G(J,J)
      DO 210 K=1,J-1
  210 GL(J,J)=GL(J,J)-GL(J,K)**2
      IF(GL(J,J).LT.ZERO) GOTO 900
      GL(J,J)=SQRT(GL(J,J))
      DO 230 I=J+1,N
      GL(I,J)=G(I,J)
      DO 220 K=1,J-1
  220 GL(I,J)=GL(I,J)-GL(I,K)*GL(J,K)
      GL(I,J)=GL(I,J)/GL(J,J)
  230 CONTINUE
  240 CONTINUE
      RETURN
  900 IRTN=J
      RETURN
      END
C------------------------- QPQRDC -----------------------------------
      SUBROUTINE QPQRDC(N,M,R,Q,JRANK,ZERO)
C QR decomposition of N*M matrix.  (N>M).  Givens transformation.
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION R(N,N),Q(N,N)
      LOGICAL JZERO
C
      JN=MIN(M,N-1)
      JRANK=0
      DO 220 I=1,N
      DO 210 J=1,N
  210 Q(I,J)=0
  220 Q(I,I)=1
      DO 260 I=1,JN
      JZERO=.TRUE.
      DO 250 J=I+1,N
      GAMMA=R(I,I)**2+R(J,I)**2
      IF(GAMMA.LT.ZERO) GOTO 250
      JZERO=.FALSE.
      GAMMA=SQRT(GAMMA)
      C=R(I,I)/GAMMA
      S=R(J,I)/GAMMA
      DO 230 L=I,M
      V=R(I,L)
      W=R(J,L)
      R(I,L)=C*V+S*W
  230 R(J,L)=-S*V+C*W
      DO 240 K=1,N
      V=Q(K,I)
      W=Q(K,J)
      Q(K,I)=C*V+S*W
  240 Q(K,J)=-S*V+C*W
  250 CONTINUE
      IF(JZERO) THEN
        JRANK=I
        RETURN
      ENDIF
  260 CONTINUE
      RETURN
      END
C------------------------- QPFWSB ----------------------------------
      SUBROUTINE QPFWSB(N,NDIM,D,TRANS,W)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION D(NDIM,N),W(N)
      LOGICAL TRANS
C
      IF(N.EQ.0) RETURN
      IF(TRANS) GOTO 230
      DO 220 I=1,N
      DO 210 J=1,I-1
  210 W(I)=W(I)-D(I,J)*W(J)
  220 W(I)=W(I)/D(I,I)
      RETURN
  230 DO 250 I=1,N
      DO 240 J=1,I-1
  240 W(I)=W(I)-D(J,I)*W(J)
  250 W(I)=W(I)/D(I,I)
      RETURN
      END
C------------------------- QPBWSB ----------------------------------
      SUBROUTINE QPBWSB(N,NDIM,D,TRANS,W)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION D(NDIM,N),W(N)
      LOGICAL TRANS
C
      IF(N.EQ.0) RETURN
      IF(TRANS) GOTO 230
      DO 220 I=N,1,-1
      DO 210 J=I+1,N
  210 W(I)=W(I)-D(I,J)*W(J)
  220 W(I)=W(I)/D(I,I)
      RETURN
  230 DO 250 I=N,1,-1
      DO 240 J=I+1,N
  240 W(I)=W(I)-D(J,I)*W(J)
  250 W(I)=W(I)/D(I,I)
      RETURN
      END
C------------------------- QPCOPT ----------------------------------
      SUBROUTINE QPCOPT(N,ME,M,NADIM,A,B,X,MJ,JS,CS,OPTM,ZERO)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(NADIM,M),B(M),X(N)
      INTEGER MJ(M)
      LOGICAL OPTM
C
      JS=0
      CS=-ZERO
      DO 220 I=ME+1,M
      IF(MJ(I).NE.0) GOTO 220
      RES=-B(I)
      DO 210 J=1,N
  210 RES=RES+A(J,I)*X(J)
      IF(RES.LT.CS) THEN
        CS=RES
        JS=I
      ENDIF
  220 CONTINUE
      IF(JS.EQ.0) OPTM=.TRUE.
      RETURN
      END
C------------------------- QPUPD ----------------------------------
      SUBROUTINE QPUPD(N,ME,M,NADIM,A,B,X,V,MJ,JSET,NJ,R,N1,N2,
     %  AS,D1,D2,ZV,RV,JS,CS,JOPT,FEASBL,ZERO)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(NADIM,M),B(M),R(N,N),N1(N,N),N2(N,N),
     %   X(N),V(M),AS(N),D1(N),D2(N),ZV(N),RV(M)
      INTEGER JSET(M),MJ(M)
      LOGICAL TRANS,FEASBL,JOPT
      REAL*8 LARGE/1D20/
      INTEGER MSGLVL,MSGFL
      COMMON/QPDUALCM/MSGLVL,MSGFL
C
      DO 210 J=1,N
  210 AS(J)=A(J,JS)
      DO 220 I=1,NJ
      D1(I)=0
      DO 220 J=1,N
  220 D1(I)=D1(I)+N1(J,I)*AS(J)
      DO 230 I=1,N-NJ
      D2(I)=0
      DO 230 J=1,N
  230 D2(I)=D2(I)+N2(J,I)*AS(J)
      DO 240 J=1,N
      ZV(J)=0
      DO 240 I=1,N-NJ
  240 ZV(J)=ZV(J)+N2(J,I)*D2(I)
      DO 250 I=1,NJ
  250 RV(I)=D1(I)
      TRANS=.FALSE.
      CALL QPBWSB(NJ,N,R,TRANS,RV)
      ZMIN=0
      DO 260 J=1,N
  260 ZMIN=MAX(ZMIN,ABS(ZV(J)))
      IF(ZMIN.LT.ZERO) GOTO 350
C--- Case (a): ZV.ne.0
      JK=0
      T1=LARGE
      DO 270 I=ME+1,NJ
      L=JSET(I)
      IF(RV(I).LT.ZERO) GOTO 270
      RATIO=V(L)/RV(I)
      IF(RATIO.LT.T1) THEN
        T1=RATIO
        JK=L
        JP=I
      ENDIF
  270 CONTINUE
      TS=0
      DO 280 J=1,N
  280 TS=TS+AS(J)*ZV(J)
      T2=-CS/TS
      IF(T1.LT.T2) GOTO 310
C--- Case (a1):  T1>=T2
      DO 290 J=1,N
  290 X(J)=X(J)+T2*ZV(J)
      DO 300 I=1,NJ
  300 V(JSET(I))=V(JSET(I))-T2*RV(I)
      V(JS)=V(JS)+T2
      IF(MSGLVL.GE.3) WRITE(MSGFL,305) JS
  305 FORMAT(' (Add constraint',I4,': full step)')
      CALL QPADD(N,M,NJ,MJ,JSET,R,N1,N2,D1,D2,JS,ZERO)
      JOPT=.TRUE.
      RETURN
C--- Case (a2): T1<T2
  310 DO 320 J=1,N
  320 X(J)=X(J)+T1*ZV(J)
      DO 330 I=1,NJ
  330 V(JSET(I))=V(JSET(I))-T1*RV(I)
      V(JS)=V(JS)+T1
      CS=-B(JS)
      DO 340 J=1,N
  340 CS=CS+AS(J)*X(J)
      IF(MSGLVL.GE.3) WRITE(MSGFL,345) JK
  345 FORMAT(' (Drop constraint',I4,': partial step)')
      CALL QPDROP(N,M,NJ,MJ,JSET,R,N1,N2,JK,JP,ZERO)
      JOPT=.FALSE.
      RETURN
C--- Case (b): ZV=0
  350 JK=0
      T=LARGE
      DO 360 I=ME+1,NJ
      L=JSET(I)
      IF(RV(I).LT.ZERO) GOTO 360
      RATIO=V(L)/RV(I)
      IF(RATIO.LT.T) THEN
        JK=L
        JP=I
        T=RATIO
      ENDIF
  360 CONTINUE
      IF(JK.NE.0) GOTO 370
C--- Case (b1)
      FEASBL=.FALSE.
      IF(MSGLVL.GE.0) WRITE(MSGFL,365)
  365 FORMAT(' --- INFEASIBLE')
      RETURN
C--- Case (b2)
  370 DO 380 I=1,NJ
  380 V(JSET(I))=V(JSET(I))-T*RV(I)
      V(JS)=T
      IF(MSGLVL.GE.3) WRITE(MSGFL,345) JK
      CALL QPDROP(N,M,NJ,MJ,JSET,R,N1,N2,JK,JP,ZERO)
      JOPT=.FALSE.
      RETURN
      END
C------------------------- QPADD ----------------------------------
      SUBROUTINE QPADD(N,M,NJ,MJ,JSET,R,N1,N2,D1,D2,JS,ZERO)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 R(N,N),N1(N,N),N2(N,N),D1(N),D2(N)
      INTEGER JSET(M),MJ(M)
C
      NJ1=NJ+1
      DO 210 J=1,NJ
  210 R(J,NJ1)=D1(J)
      DO 220 J=1,N-NJ
  220 R(NJ+J,NJ1)=D2(J)
      DO 240 J=NJ+2,N
      GAMMA=R(NJ1,NJ1)**2+R(J,NJ1)**2
      IF(GAMMA.LT.ZERO) GOTO 240
      GAMMA=SQRT(GAMMA)
      C=R(NJ1,NJ1)/GAMMA
      S=R(J,NJ1)/GAMMA
      R(NJ1,NJ1)=GAMMA
      R(J,NJ1)=0
      DO 230 K=1,N
      V=N2(K,1)
      W=N2(K,J-NJ)
      N2(K,1)=C*V+S*W
  230 N2(K,J-NJ)=-S*V+C*W
  240 CONTINUE
      DO 250 J=1,N
  250 N1(J,NJ1)=N2(J,1)
      DO 260 I=1,N-NJ1
      DO 260 J=1,N
  260 N2(J,I)=N2(J,I+1)
      JSET(NJ1)=JS
      MJ(JS)=1
      NJ=NJ1
      RETURN
      END
C------------------------- QPDROP ---------------------------------
      SUBROUTINE QPDROP(N,M,NJ,MJ,JSET,R,N1,N2,JK,JP,ZERO)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 R(N,N),N1(N,N),N2(N,N)
      INTEGER JSET(M),MJ(M)
C
      if(nj.eq.0) return
c         added but do not know correct or not
      IF(JP.EQ.NJ) THEN
        DO 210 I=N-NJ,1,-1
        DO 210 J=1,N
  210   N2(J,I+1)=N2(J,I)
        DO 220 J=1,N
  220   N2(J,1)=N1(J,NJ)
        IF(JK.NE.0) MJ(JK)=0
c           "if" added
        NJ=NJ-1
        RETURN
      ENDIF
C
      DO 230 I=JP,NJ-1
      DO 230 J=1,N
  230 R(J,I)=R(J,I+1)
      DO 260 I=JP,NJ-1
      I1=I+1
      GAMMA=R(I,I)**2+R(I1,I)**2
      IF(GAMMA.LT.ZERO) GOTO 260
      GAMMA=SQRT(GAMMA)
      C=R(I,I)/GAMMA
      S=R(I1,I)/GAMMA
      DO 240 L=I,NJ-1
      V=R(I,L)
      W=R(I1,L)
      R(I,L)=C*V+S*W
  240 R(I1,L)=-S*V+C*W
      DO 250 J=1,N
      V=N1(J,I)
      W=N1(J,I1)
      N1(J,I)=C*V+S*W
  250 N1(J,I1)=-S*V+C*W
  260 CONTINUE
      DO 270 I=N-NJ,1,-1
      DO 270 J=1,N
  270 N2(J,I+1)=N2(J,I)
      DO 280 J=1,N
  280 N2(J,1)=N1(J,NJ)
      DO 290 I=JP,NJ-1
  290 JSET(I)=JSET(I+1)
      IF(JK.NE.0) MJ(JK)=0
c           "if" added
      NJ=NJ-1
      RETURN
      END
C------------------------- QPPRNT ---------------------------------
      SUBROUTINE QPPRNT(N,M,NADIM,NGDIM,A,B,F00,C,G,X,V,RES,JSET,NJ,
     %                  OBJSCL,SCL,IT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(NADIM,M),B(M),C(N),G(NGDIM,N),X(N),V(M),RES(N+M),
     %       SCL(M)
      INTEGER JSET(M)
      INTEGER MSGLVL,MSGFL
      COMMON/QPDUALCM/MSGLVL,MSGFL
C
      Z=0
      DO 210 I=1,N
      Z=Z+C(I)*X(I)
      DO 210 J=1,N
  210 Z=Z+0.5D0*G(I,J)*X(I)*X(J)
      WRITE(MSGFL,220) IT,(Z+F00)*OBJSCL
  220 FORMAT(' +++ Iteration',I5,' +++  Objective value=',1PD12.5)
      WRITE(MSGFL,230) (X(J),J=1,N)
  230 FORMAT(' Values of variables X= ',1P,3(D13.5,:),/,5(D13.5,:))
      WRITE(MSGFL,240) (JSET(I),I=1,NJ)
  240 FORMAT(' Current active set  J= ',10I5,:,/,(15I5,:))
      WRITE(MSGFL,250) (V(I)*OBJSCL/SCL(I),I=1,M)
  250 FORMAT(' Values of multiplierV= ',1P,3(D13.5,:),/,5(D13.5,:))
      IF(MSGLVL.LE.2) RETURN
      DO 340 J=1,N
      RES(J)=C(J)
      DO 320 I=1,N
  320 RES(J)=RES(J)+G(J,I)*X(I)
      DO 330 I=1,M
  330 RES(J)=RES(J)-V(I)*A(J,I)
  340 CONTINUE
      DO 360 I=1,M
      I1=I+N
      RES(I1)=-B(I)
      DO 360 J=1,N
  360 RES(I1)=RES(I1)+A(J,I)*X(J)
      WRITE(MSGFL,400) (RES(J)*SCL(J),J=1,N)
  400 FORMAT(' Residual Kuhn-Tucker = ',1P,3(D13.5,:),/,5(D13.5,:))
      WRITE(MSGFL,420) (RES(I+N)*SCL(I),I=1,M)
  420 FORMAT(' Residual of constraints =',1P,3(D13.5,:),/,5(D13.5,:))
      RETURN
      END
C------------------------- QPOUT ---------------------------------
      SUBROUTINE QPOUT(N,ME,M,NADIM,NGDIM,A,B,FMIN,C,G,X,V,RES,IT)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(NADIM,M),B(M),C(N),G(NGDIM,N),X(N),V(M),RES(N+M)
      INTEGER MSGLVL,MSGFL
      COMMON/QPDUALCM/MSGLVL,MSGFL
C
      MI=M-ME
      DO 340 J=1,N
      RES(J)=C(J)
      DO 320 K=1,N
  320 RES(J)=RES(J)+G(J,K)*X(K)
      DO 330 I=1,M
  330 RES(J)=RES(J)-A(J,I)*V(I)
  340 CONTINUE
      DO 360 I=1,M
      I1=I+N
      RES(I1)=-B(I)
      DO 360 J=1,N
  360 RES(I1)=RES(I1)+A(J,I)*X(J)
C
      WRITE(MSGFL,400) FMIN,IT
  400 FORMAT(' ++++++ RESULTS ++++ F=',1PD12.5,
     %  '  after',I5,' iterations')
      WRITE(MSGFL,410) (J,X(J),RES(J),J=1,N)
  410 FORMAT(2('      Variables        (Res.K-T)  '),/,
     %    2('  X(',I2,')=',1PD12.5,' (',1PD11.4,')',:))
      IF(ME.NE.0) WRITE(MSGFL,430) (I,RES(I+N),V(I),I=1,ME)
  430 FORMAT('  Equality Constraints:  Lagrange Mult.',/,
     % 2('  C(',I2,')=',1PD12.5,' V=',1PD12.5,:))
      IF(MI.NE.0) WRITE(MSGFL,480) (I,RES(I+N),V(I),I=ME+1,M)
  480 FORMAT('  Inequality Constraints:  Lagrange Mult.',/,
     % 2('  C(',I2,')=',1PD12.5,' V=',1PD12.5,:))
      RETURN
      END

