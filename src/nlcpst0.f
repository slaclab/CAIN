      SUBROUTINE NLCPST0(MY,MPH,MXI,MLM,YY,XISQ,LM,
     %   BES,FF,FINT,FALL,FALLMX,WGTCP)
C  Create table for nonlinear Compton (circular pol)
C  FF(1,*), FF(2,*) correspond to D1s, D2s in Tsai's paper
C  divided by (8*Xi**2).
      IMPLICIT NONE
      INTEGER MY,MPH,MXI,MLM
      REAL*8 YY(0:MY),XISQ(0:MXI),LM(0:MLM),
     %   BES(3,MPH,0:MY,0:MXI),
     %   FF(2,MPH,0:MY,0:MXI,0:MLM),FINT(2,MPH,0:MY,0:MXI,0:MLM),
     %   FALL(2,0:MXI,0:MLM),FALLMX(2),WGTCP(0:MY)
      INTEGER I,J,K,L,N
      REAL*8 BJ,DBJ,DY,C,VMAX,V,Z,Z1,FF1
C
	CALL CPUTIM('NLCPST0',1)
      DY=1D0/MY
      DO 300 I=0,MY
        DO 280 J=0,MXI
          Z1=SQRT(4*YY(I)*(1-YY(I))*XISQ(J)/(1+XISQ(J)))
          DO 260 N=1,MPH
            Z=Z1*N
            CALL BESJDJ(N,Z,BJ,DBJ)
            BES(1,N,I,J)=BJ**2
            IF(Z.NE.0) THEN
              BES(2,N,I,J)=2*(DBJ**2-(1-(N/Z)**2)*BJ**2)
              BES(3,N,I,J)=4*N/Z*BJ*DBJ
            ELSE
              IF(N.EQ.1) THEN
                BES(2,N,I,J)=1
                BES(3,N,I,J)=1
              ELSE
                BES(2,N,I,J)=0
                BES(3,N,I,J)=0
              ENDIF
            ENDIF
C    BES(1)=J(n)**2
C    BES(2)=J(n-1)**2+J(n+1)**2-2*J(n)**2
C    BES(3)=J(n-1)**2-J(n+1)**2
 260      CONTINUE
 280    CONTINUE
 300  CONTINUE
      DO 400 N=1,MPH
        DO 380 J=0,MXI
          DO 360 L=0,MLM
            VMAX=N*LM(L)/(1+XISQ(J))
            DO 340 I=0,MY
              V=VMAX*YY(I)
              C=VMAX/(1+V)**2*WGTCP(I)
              FF1=0.5D0*(1+V**2/(2*(1+V)))*BES(2,N,I,J)
              IF(XISQ(J).NE.0) THEN
                FF1=FF1-BES(1,N,I,J)/XISQ(J)
              ELSEIF(N.EQ.1) THEN
                FF1=FF1-YY(I)*(1-YY(I))
              ENDIF
              FF(1,N,I,J,L)=C*FF1
              FF(2,N,I,J,L)=C*(0.5D0-YY(I))*V*(1+V/2D0)/(1+V)
     %                *BES(3,N,I,J)
 340        CONTINUE
 360      CONTINUE
 380    CONTINUE
 400  CONTINUE
      DO 500 N=1,MPH
        DO 480 J=0,MXI
          DO 460 L=0,MLM
            DO 440 K=1,2
              FINT(K,N,0,J,L)=0
              DO 420 I=1,MY
                FINT(K,N,I,J,L)=FINT(K,N,I-1,J,L)
     %               +0.5D0*DY*(FF(K,N,I-1,J,L)+FF(K,N,I,J,L))
 420          CONTINUE
 440        CONTINUE
 460      CONTINUE
 480    CONTINUE
 500  CONTINUE
      FALLMX(1)=0
      FALLMX(2)=0
      DO 600 J=0,MXI
        DO 580 L=0,MLM
          DO 560 K=1,2
            FALL(K,J,L)=0
            DO 540 N=1,MPH
              FALL(K,J,L)=FALL(K,J,L)+FINT(K,N,MY,J,L)
 540        CONTINUE
            FALLMX(K)=MAX(FALLMX(K),ABS(FALL(K,J,L)))
 560      CONTINUE
 580    CONTINUE
 600  CONTINUE
	CALL CPUTIM('NLCPST0',2)
      RETURN
      END
