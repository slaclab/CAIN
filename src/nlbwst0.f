      SUBROUTINE NLBWST0(MY,MPH,MXI,MQ,NPHMIN,YY,XISQ,QQ,
     %    LN0,NL0,GG,GINT,GALL,GALLMX,WGTBW)
C  Create table for nonliner Breit-Wheeler
      IMPLICIT NONE
      INTEGER MY,MPH,MXI,MQ,NPHMIN,LN0(MPH),NL0(MQ)
      REAL*8 YY(0:MY),XISQ(0:MXI),QQ(MQ),
     %    GG(2,MPH,0:MY,0:MXI,MQ),GINT(2,MPH,0:MY,0:MXI,MQ),
     %    GALL(2,MPH,0:MXI,MQ),GALLMX(2),WGTBW(0:MY)
      INTEGER I,J,K,L,N
      REAL*8 UN,XRANGE,X,DY,G(4)
C
	CALL CPUTIM('NLBWST0',1)
      DO 300 N=NPHMIN,MPH
        DO 280 J=0,MXI
          DO 260 L=LN0(N),MQ
            UN=N*QQ(L)
            XRANGE=0.5D0*SQRT(MAX(0D0,1-1/UN))
            DY=1/DFLOAT(MY)
            DO 240 I=0,MY
              X=0.5D0-XRANGE*DFLOAT(I)/MY
              CALL NLBWFN0(0,N,XISQ(J),QQ(L),X,G)
              GG(1,N,I,J,L)=WGTBW(I)*XRANGE*G(1)
              GG(2,N,I,J,L)=WGTBW(I)*XRANGE*G(3)
              IF(I.NE.0) THEN
                DO 220 K=1,2
                  GINT(K,N,I,J,L)=GINT(K,N,I-1,J,L)
     %             +0.5D0*DY*(GG(K,N,I-1,J,L)+GG(K,N,I,J,L))
 220            CONTINUE
              ELSE
                GINT(1,N,I,J,L)=0
                GINT(2,N,I,J,L)=0
              ENDIF
 240        CONTINUE
 260      CONTINUE
 280    CONTINUE
 300  CONTINUE
C
      DO 400 K=1,2
        GALLMX(K)=0
        DO 380 J=0,MXI
          DO 360 L=1,MQ
            GALL(K,MPH,J,L)=GINT(K,MPH,MY,J,L)
            GALLMX(K)=MAX(GALLMX(K),ABS(GALL(K,MPH,J,L)))
            IF(NL0(L).LT.MPH) THEN
              DO 340 N=MPH-1,NL0(L),-1
                GALL(K,N,J,L)=GALL(K,N+1,J,L)+GINT(K,N,MY,J,L)
                GALLMX(K)=MAX(GALLMX(K),ABS(GALL(K,N,J,L)))
 340          CONTINUE
            ENDIF
 360      CONTINUE
 380    CONTINUE
 400  CONTINUE
	CALL CPUTIM('NLBWST0',2)
      RETURN
      END

        
