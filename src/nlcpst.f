      SUBROUTINE NLCPST(LCIRCLIN,MY0,MPH0,MXI0,MLM0,MPHI0,
     %     XIMAX,LMMAX0,LWGTCP,WGTCPF,IRTN)
      IMPLICIT NONE
      INTEGER LCIRCLIN,MY0,MPH0,MXI0,MLM0,MPHI0,LWGTCP,IRTN
      REAL*8 XIMAX,LMMAX0,WGTCPF
      EXTERNAL WGTCPF
      INCLUDE 'include/nllsrcm.h'
      INCLUDE 'include/nlcpcm.h'
	INCLUDE 'include/ctrlcm.h'
      INTEGER I,J,L
C
      IF(MY0.GT.MMY) GOTO 910
      IF(MPH.GT.MMPH) GOTO 920
      IF(MXI0.GT.MMXI) GOTO 930
      IF(MLM0.GT.MMLM) GOTO 940
      MY=MY0
      MXI=MXI0
      MLM=MLM0
      MPH=MPH0
      XISQMX=XIMAX**2
      LMMAX=LMMAX0
	LSRQEDPOL(1)=LCIRCLIN
	IF(LCIRCLIN.EQ.1) THEN
        CALL NLLSR0(1)
        IPINIT(1)=IP
        IPBES=IPINIT(1)
        IPFF=IPBES+3*MPH*(1+MY)*(1+MXI)
        IPFINT=IPFF+2*MPH*(1+MY)*(1+MXI)*(1+MLM)
        IPFALL=IPFINT+2*MPH*(1+MY)*(1+MXI)*(1+MLM)
        IP=IPFALL+2*(1+MXI)*(1+MLM)
        IPLAST(1)=IP-1
        IF(IPLAST(1).GT.MW) GOTO 900
        DO 200 I=0,MY
          YY(I)=DFLOAT(I)/DFLOAT(MY)
          IF(LWGTCP.GE.1) THEN
            WGTCP(I)=WGTCPF(YY(I))
            IF(WGTCP(I).LT.1) GOTO 950
            IF(ABS(WGTCP(I)-1).LE.1D-3) WGTCP(I)=1
          ELSE
            WGTCP(I)=1
          ENDIF
 200    CONTINUE
        DO 210 J=0,MXI
          XISQ(J)=DFLOAT(J)*XISQMX/MXI
 210    CONTINUE
        DO 220 L=0,MLM
          LM(L)=DFLOAT(L)*LMMAX/MLM
 220    CONTINUE
        CALL NLCPST0(MY,MPH,MXI,MLM,YY,XISQ,LM,
     %     R8(IPBES),R8(IPFF),R8(IPFINT),R8(IPFALL),FALLMX,WGTCP)
        IRTN=0
	ELSEIF(LCIRCLIN.EQ.2) THEN

!      Begin Change, March.7.2003(by Li Dongguo)
        call NLCPLST4(Mph0,My0,Mphi0,Mxi0,MLM0,LMMAX0,Ximax,IRTN)
!      End Change, March.7.2003(by Li Dongguo)
!       Before Change
!	  CALL NLCPLST(MY0,MPH0,MXI0,MLM0,MPHI0,
!     %     XIMAX,LMMAX0,LWGTCP,WGTCPF,IRTN)



	  IF(IRTN.NE.0) GOTO 960
	  IREADY(1)=IREADY0
	ENDIF
      RETURN
 900  IRTN=1000
c     23456789012345678901234567890123456789012345678901234567890123456789012
c            10        20        30        40        50        60        70    
      WRITE(MSGFL,905) IPLAST(1),MW
 905  FORMAT(' (SUBR.NLCPST) Memory insufficient.  IPLAST(1),MW= ',2I10)
      GOTO 990
 910  IRTN=1001
      WRITE(MSGFL,915) MMY
 915  FORMAT(' (SUBR.NLCPST) MY too large. Must be <=',I7)
      GOTO 990
 920  IRTN=1002
      WRITE(MSGFL,925) MMPH
 925  FORMAT(' (SUBR.NLCPST) MPH too large. Must be <=',I7)
      GOTO 990
 930  IRTN=1003
      WRITE(MSGFL,935) MXI0,MMXI
 935  FORMAT(' (SUBR.NLCPST) MXI0= ',I7,' too large. Must be <=',I7)
      GOTO 990
 940  IRTN=1004
      WRITE(MSGFL,945) MMLM
 945  FORMAT(' (SUBR.NLCPST) MLAM too large. Must be <=',I7)
      GOTO 990
 950  IRTN=1100
      WRITE(MSGFL,955) YY(I)
 955  FORMAT(' (SUBR.NLCPST) Weight function <1 at y=',0PF10.6)
      GOTO 990
 960  IRTN=1006
      WRITE(MSGFL,965)
 965  FORMAT(' (SUBR.NLCPST) Error in NLCPLST.')
      GOTO 990
 990  IREADY(1)=0
      RETURN
      END
