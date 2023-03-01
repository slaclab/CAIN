      SUBROUTINE NLBWST(MY0,MPH0,MXI0,MQ0,XIMAX,ETAMAX,LWGTBW,WGTBWF,
     %     IRTN)
      IMPLICIT NONE
      INTEGER MY0,MPH0,MXI0,MQ0,LWGTBW,IRTN
      REAL*8 XIMAX,ETAMAX,WGTBWF
      EXTERNAL WGTBWF
      INCLUDE 'include/nllsrcm.h'
      INCLUDE 'include/nlbwcm.h'
	INCLUDE 'include/ctrlcm.h'
      INTEGER N,I,J,L
      REAL*8 DQ0,DQ,DELTAQ
C
      IF(MY0.GT.MMY) GOTO 910
      IF(MPH.GT.MMPH) GOTO 920
      IF(MXI0.GT.MMXI) GOTO 930
      IF(MQ0.GT.MMQ) GOTO 940
      CALL NLLSR0(2)
      MY=MY0
      MXI=MXI0
      MPH=MPH0
      XISQMX=XIMAX**2
      QMAX=ETAMAX
      NPHMIN=INT(1/QMAX)+1
      IF(NPHMIN.GT.MPH) GOTO 910
      DQ0=(QMAX-QMIN)/MQ0
      MQ=1
      DO 240 N=MPH,NPHMIN,-1
        IF(N.NE.NPHMIN) THEN
          DELTAQ=1/DFLOAT(N*(N-1))
        ELSE
          DELTAQ=QMAX-1/DFLOAT(N)
        ENDIF
        L=MAX(1,NINT(DELTAQ/DQ0))
        DQ=DELTAQ/L
        IF(MQ+L.GE.MMQ) GOTO 900
        LN0(N)=MQ
        DO 220 I=1,L
          QQ(MQ)=1/DFLOAT(N)+DQ*(I-1)
          NL0(MQ)=N
          MQ=MQ+1
 220    CONTINUE
 240  CONTINUE
      QQ(MQ)=QMAX
      NL0(MQ)=NPHMIN
      DO 260 I=0,MY
        YY(I)=DFLOAT(I)/DFLOAT(MY)
        IF(LWGTBW.GE.1) THEN
          WGTBW(I)=WGTBWF(YY(I))
          IF(WGTBW(I).LT.1) GOTO 960
        ELSE
          WGTBW(I)=1
        ENDIF
 260  CONTINUE
      DO 280 J=0,MXI
        XISQ(J)=DFLOAT(J)*XISQMX/MXI
 280  CONTINUE
c      print *,' nphmin=',nphmin,'  mph=',mph,'  mq=',mq
c      WRITE(MSGFL,998) (ln0(i),i=1,mph)
c 998  format(' LN0=',10I3)
c      WRITE(MSGFL,999) (l,qq(l),l=1,mq)
c 999  format(4(' QQ(',i2,')=',F9.5,:))
c      WRITE(MSGFL,997) (l,nl0(l),l=1,mq)
c 997  format(4(' LN0(',i2,')=',I4,4X,:))
C
      IPINIT(2)=IP
      IPGG=IPINIT(2)
      IPGINT=IPGG+2*MPH*(1+MY)*(1+MXI)*MQ
      IPGALL=IPGINT+2*MPH*(1+MY)*(1+MXI)*MQ
      IP=IPGALL+2*MPH*(1+MXI)*MQ
      IPLAST(2)=IP-1
      IF(IPLAST(2).GT.MW) GOTO 900
      CALL NLBWST0(MY,MPH,MXI,MQ,NPHMIN,YY,XISQ,QQ,
     %   LN0,NL0,R8(IPGG),R8(IPGINT),R8(IPGALL),GALLMX,WGTBW)
      IRTN=0
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905) IPLAST(2),MW,MQ+L,MMQ
 905  FORMAT(' (SUBR.NLBWST) Memory insufficient.  IPLAST(2),MW= ',2I10,
     %      ' MQ+L,MMQ= ',2I10) 
      GOTO 990
 910  IRTN=1001
      WRITE(MSGFL,915) MMY
 915  FORMAT(' (SUBR.NLBWST) MY too large. Must be <=',I4)
      GOTO 990
 920  IRTN=1002
      WRITE(MSGFL,925) MMPH
 925  FORMAT(' (SUBR.NLBWST) MPH too large. Must be <=',I4)
      GOTO 990
 930  IRTN=1003
      WRITE(MSGFL,935) MMXI
 935  FORMAT(' (SUBR.NLBWST) MXI too large. Must be <=',I4)
      GOTO 990
 940  IRTN=1004
      WRITE(MSGFL,945) MMQ
 945  FORMAT(' (SUBR.NLBWST) MQ too large. Must be <=',I4)
      GOTO 990
C 950  IRTN=1005
C      WRITE(MSGFL,955)
C 955  FORMAT(' (SUBR.NLBWST) Laser pair creation impossible for ',
C     %   'given ETAMAX and NPH')
C      GOTO 990
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.NLBWST) Weight function <1.')
      GOTO 990
 990  IREADY(2)=0
      RETURN
      END
