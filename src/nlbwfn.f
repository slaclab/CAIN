      FUNCTION NLBWFN(KK,K,NPH,XI,ETA,X)
      IMPLICIT NONE
      INTEGER KK,K,NPH
      REAL*8 NLBWFN,XI,ETA,X
      INTEGER K1,L,L1,L2,J,J1,IY,IY1,N,LP
      REAL*8 XISQ1,Q,UN,U,G(5),PJ,PJ1,PL,PL1,XRANGE,Y,G0,G1,PY,PY1,
     %     WGT
      INCLUDE 'include/nllsrcm.h'
      INCLUDE 'include/nlbwcm.h'
	INCLUDE 'include/ctrlcm.h'
      REAL*8 GG
      GG(K,N,IY,J,L)=R8(IPGG+K-1+2*(N-1+MPH*(IY
     %       +(MY+1)*(J+(MXI+1)*(L-1)))))
C
      NLBWFN=0
      XISQ1=XI**2
      IF(KK.LE.0.OR.KK.GE.3) GOTO 900
      IF(XI.LT.0) GOTO 930
      IF(ETA.LT.0) GOTO 940
      IF(X.LT.0.OR.X.GT.1) GOTO 950
      NLBWFN=0
      IF(X.EQ.0.OR.X.EQ.1) RETURN
      Q=ETA/(1+XISQ1)
      UN=NPH*Q
      IF(UN.LT.1) RETURN
      IF(KK.EQ.1) THEN
        IF(K.LE.0.OR.K.GE.5) GOTO 910
        U=0.25D0/(X*(1-X))
        LP=0
        IF(K.EQ.2.OR.K.EQ.4) LP=1
        CALL NLBWFN0(LP,NPH,XISQ1,Q,X,G)
        NLBWFN=G(K)
      ELSE
        IF(K.NE.1.AND.K.NE.3) GOTO 910
        IF(XISQ1.GT.XISQMX) GOTO 930
        IF(Q.GT.QMAX) GOTO 940
        K1=1
        IF(K.EQ.3) K1=2
        PJ=XISQ1/XISQMX*MXI
        J=MIN(MXI-1,INT(PJ))
        PJ=PJ-J
        PJ1=1-PJ
        J1=J+1
        L=1
        L1=MQ
 200    L2=(L+L1)/2
        IF(Q.GE.QQ(L2)) THEN
          L=L2
        ELSE
          L1=L2
        ENDIF
        IF(L1-L.NE.1) GOTO 200
        L1=L+1
        PL=(Q-QQ(L))/(QQ(L1)-QQ(L))
        PL1=1-PL
        XRANGE=0.5D0*SQRT(MAX(0D0,1-1/UN))
        Y=(X-0.5D0)/XRANGE
        PY=ABS(Y)*MY
        IY=MIN(MY-1,INT(PY))
        PY=PY-IY
CC        IY=IY1-1
        IY1=IY+1
        PY1=1-PY
        WGT=WGTBW(IY)*PY1+WGTBW(IY1)*PY
        G0=PL1*(PJ1*GG(K1,NPH,IY,J,L)+PJ*GG(K1,NPH,IY,J1,L))
     %     +PL*(PJ1*GG(K1,NPH,IY,J,L1)+PJ*GG(K1,NPH,IY,J1,L1))
        G1=PL1*(PJ1*GG(K1,NPH,IY1,J,L)+PJ*GG(K1,NPH,IY1,J1,L))
     %     +PL*(PJ1*GG(K1,NPH,IY1,J,L1)+PJ*GG(K1,NPH,IY1,J1,L1))
        NLBWFN=(PY1*G0+PY*G1)/XRANGE/WGT
      ENDIF
      RETURN
 900  WRITE(MSGFL,905)
 905  FORMAT(' (FUNC.NLBWFN) The first argument must be 1 or 2.')
      RETURN
 910  WRITE(MSGFL,915)
 915  FORMAT(' (FUNC.NLBWFN) Bad combination of the first two ',
     %   'arguments.')
      RETURN
 930  WRITE(MSGFL,935)
 935  FORMAT(' (FUNC.NLBWFN) Invalid XI parameter.')
      RETURN
 940  WRITE(MSGFL,945)
 945  FORMAT(' (FUNC.NLBWFN) Invalid ETA parameter.')
      RETURN
 950  WRITE(MSGFL,955)
 955  FORMAT(' (FUNC.NLBWFN) Invalid X parameter.')
      RETURN
      END

