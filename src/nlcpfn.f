      FUNCTION NLCPFN(KK,K,NPH,XI,LAMBDA,X)
      IMPLICIT NONE
      INTEGER KK,K,NPH
      REAL*8 NLCPFN,XI,LAMBDA,X
      INTEGER L,L1,J,J1,IY,IY1,N,LP
      REAL*8 XISQ1,VN,V,F(5),PJ,PJ1,PL,PL1,Y,F0,F1,PY,PY1
      INCLUDE 'include/nllsrcm.h'
      INCLUDE 'include/nlcpcm.h'
	INCLUDE 'include/ctrlcm.h'
      REAL*8 FF,WGT
      FF(K,N,IY,J,L)=R8(IPFF+K-1+2*(N-1+MPH*(IY
     %       +(MY+1)*(J+(MXI+1)*(L-1)))))
C
      NLCPFN=0
      XISQ1=XI**2
      IF(KK.LE.0.OR.KK.GE.3) GOTO 900
      IF(XI.LT.0) GOTO 930
      IF(LAMBDA.LT.0) GOTO 940
      IF(X.LT.0.OR.X.GT.1) GOTO 950
      NLCPFN=0
      IF(X.EQ.0.OR.X.EQ.1) RETURN
      IF(KK.EQ.1) THEN
        IF(K.LE.0.OR.K.GE.6) GOTO 910
        LP=0
        IF(K.GE.3) LP=1
        CALL NLCPFN0(LP,NPH,XISQ1,LAMBDA,X,F)
        NLCPFN=F(K)
      ELSE
        IF(K.NE.1.AND.K.NE.2) GOTO 910
        IF(XISQ1.GT.XISQMX) GOTO 930
        IF(LAMBDA.GT.LMMAX) GOTO 940
        V=X/(1-X)
        VN=NPH*LAMBDA/(1+XISQ1)
        Y=V/VN
        IF(Y.GT.1) RETURN
        PJ=XISQ1/XISQMX*MXI
        J=MIN(MXI-1,INT(PJ))
        PJ=PJ-J
        PJ1=1-PJ
        J1=J+1
        PL=LAMBDA/LMMAX*MLM
        L=MIN(MLM-1,INT(PL))
        L1=L+1
        PL1=1-PL
        PY=Y*MY
        IY=MIN(MY-1,INT(PY))
        PY=PY-IY
CC        IY=IY1-1
        IY1=IY+1
        PY1=1-PY
        WGT=WGTCP(IY)*PY1+WGTCP(IY1)*PY
        F0=PL1*(PJ1*FF(K,NPH,IY,J,L)+PJ*FF(K,NPH,IY,J1,L))
     %     +PL*(PJ1*FF(K,NPH,IY,J,L1)+PJ*FF(K,NPH,IY,J1,L1))
        F1=PL1*(PJ1*FF(K,NPH,IY1,J,L)+PJ*FF(K,NPH,IY1,J1,L))
     %     +PL*(PJ1*FF(K,NPH,IY1,J,L1)+PJ*FF(K,NPH,IY1,J1,L1))
        NLCPFN=(PY1*F0+PY*F1)*(1+V)**2/VN/WGT
      ENDIF
      RETURN
 900  WRITE(MSGFL,905)
 905  FORMAT(' (FUNC.NLCPFN) The first argument must be 1 or 2.')
      RETURN
 910  WRITE(MSGFL,915)
 915  FORMAT(' (FUNC.NLCPFN) Bad combination of the first two ',
     %   'arguments.')
      RETURN
 930  WRITE(MSGFL,935)
 935  FORMAT(' (FUNC.NLCPFN) Invalid XI parameter.')
      RETURN
 940  WRITE(MSGFL,945)
 945  FORMAT(' (FUNC.NLCPFN) Invalid LAMBDA parameter.')
      RETURN
 950  WRITE(MSGFL,955)
 955  FORMAT(' (FUNC.NLCPFN) Invalid X parameter.')
      RETURN
      END

