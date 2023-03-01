      SUBROUTINE NLBWGN0(XISQ,Q,DT,WG,HL,HG,PMAX,ISPIN1,
     %     NPH,XELE,HELE,HPOS,PROB,WGT,IRTN,
     %     MPH,MY,MXI,MQ,XISQMX,QMIN,QMAX,QQ,LN0,NL0,
     %     GG,GINT,GALL,GALLMX,WGTBW)
      IMPLICIT NONE
      INTEGER ISPIN1,NPH,IRTN
      REAL*8 XISQ,Q,DT,WG,HL,HG,PMAX,XELE,HELE,HPOS,PROB,WGT
      INTEGER MPH,MY,MXI,MQ,LN0(MPH),NL0(MQ)
      REAL*8 XISQMX,QMIN,QMAX,QQ(MQ),GG(2,MPH,0:MY,0:MXI,MQ),
     %   GINT(2,MPH,0:MY,0:MXI,MQ),GALL(2,MPH,0:MXI,MQ),GALLMX(2),
     %   WGTBW(0:MY)
      REAL*8 FAC/9.65650270D15/
C        FAC= (fine_str_const)*(electron rest energy)
C              /(Compton wavelength) in units of eV/meter
C
      INTEGER J,L,J1,L1,L2,N,I,IY,IY1,N0
      REAL*8 PJ,PJ1,PL,PL1,PJL,PJ1L,PJL1,PJ1L1,
     %   HLG,FAC1,R1,R10,GNALL(2),HG0,
     %   GGX,GN,GN0,GN1,GG0,GG1,GG10,C1,PY,Y,UN,G(5)
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
      REAL*8 EMASS/0.511D6/
	INCLUDE 'include/ctrlcm.h'
C
      NPH=0
      IRTN=0
      IF(Q.LE.QMIN) RETURN
      IF(Q.GT.QMAX) GOTO 910
      IF(XISQ.GT.XISQMX) GOTO 900
C Decide to emit or not to emit
      HG0=HG
      HLG=HL*HG0
      FAC1=FAC*DT/WG*XISQ*2
C           factor 2 for -1<y<1. 
      R1=RANDCAIN()
cc      PROB=FAC1*(GALLMX(1)+ABS(HLG)*GALLMX(2))
cc      IF(PROB.LE.PMAX.AND.R1.GE.PMAX) RETURN
      PJ=XISQ/XISQMX*MXI
      J=MIN(MXI-1,INT(PJ))
      PJ=PJ-J
      PJ1=1-PJ
      J1=J+1
C
      L=1
      L1=MQ
 200  L2=(L+L1)/2
      IF(Q.GE.QQ(L2)) THEN
        L=L2
      ELSE
        L1=L2
      ENDIF
      IF(L1-L.NE.1) GOTO 200
      L1=L+1
      PL=(Q-QQ(L))/(QQ(L1)-QQ(L))
      PL1=1-PL
      PJL=PJ*PL
      PJL1=PJ*PL1
      PJ1L=PJ1*PL
      PJ1L1=PJ1*PL1
      N0=NL0(L)
      DO 220 I=1,2
        GNALL(I)=PJ1L1*GALL(I,N0,J,L)+PJL1*GALL(I,N0,J1,L)
     %          +PJ1L*GALL(I,N0,J,L1)+PJL*GALL(I,N0,J1,L1)
 220  CONTINUE
      GN=GNALL(1)+HLG*GNALL(2)
      PROB=FAC1*GN
      IF(PROB.GE.PMAX) GOTO 920
C        Helicity change for the case of no event
      HG=MAX(-1D0,MIN(1D0,
     %     (HG0*(1-FAC1*GNALL(1))-HL*FAC1*GNALL(2))/(1-PROB)))
      IF(R1.GE.PROB) RETURN
C Decide how many laser photons to absorb
      IF(N0.LT.MPH) THEN
        DO 230 N=MPH,N0+1,-1
          GN=PJ1L1*(GALL(1,N,J,L)+HLG*GALL(2,N,J,L))
     %     +PJL1*(GALL(1,N,J1,L)+HLG*GALL(2,N,J1,L))
     %     +PJ1L*(GALL(1,N,J,L1)+HLG*GALL(2,N,J,L1))
     %      +PJL*(GALL(1,N,J1,L1)+HLG*GALL(2,N,J1,L1))
          GGX=GN*FAC1
          NPH=N
          IF(R1.LE.GGX) GOTO 240
 230    CONTINUE
      ENDIF
      NPH=N0
C Decide the photon energy and angle
C        (one-to-one because nph is given)
 240  N=NPH
      R10=2*RANDCAIN()-1
      R1=ABS(R10)
      GN=PJ1L1*(GINT(1,N,MY,J,L)+HLG*GINT(2,N,MY,J,L))
     %   +PJL1*(GINT(1,N,MY,J1,L)+HLG*GINT(2,N,MY,J1,L))
     %   +PJ1L*(GINT(1,N,MY,J,L1)+HLG*GINT(2,N,MY,J,L1))
     %    +PJL*(GINT(1,N,MY,J1,L1)+HLG*GINT(2,N,MY,J1,L1))
      GN1=0
      DO 300 I=1,MY-1
        GN0=GN1
        GN1=PJ1L1*(GINT(1,N,I,J,L)+HLG*GINT(2,N,I,J,L))
     %      +PJL1*(GINT(1,N,I,J1,L)+HLG*GINT(2,N,I,J1,L))
     %      +PJ1L*(GINT(1,N,I,J,L1)+HLG*GINT(2,N,I,J,L1))
     %       +PJL*(GINT(1,N,I,J1,L1)+HLG*GINT(2,N,I,J1,L1))
        GN1=GN1/GN
        IY1=I
        IF(R1.LE.GN1) GOTO 320
 300  CONTINUE
C       inverse interpolation
      IY1=MY
      GN0=GN1
      GN1=1
 320  IY=IY1-1
      GG0=PJ1L1*(GG(1,N,IY,J,L)+HLG*GG(2,N,IY,J,L))
     %    +PJL1*(GG(1,N,IY,J1,L)+HLG*GG(2,N,IY,J1,L))
     %    +PJ1L*(GG(1,N,IY,J,L1)+HLG*GG(2,N,IY,J,L1))
     %     +PJL*(GG(1,N,IY,J1,L1)+HLG*GG(2,N,IY,J1,L1))
      GG1=PJ1L1*(GG(1,N,IY1,J,L)+HLG*GG(2,N,IY1,J,L))
     %    +PJL1*(GG(1,N,IY1,J1,L)+HLG*GG(2,N,IY1,J1,L))
     %    +PJ1L*(GG(1,N,IY1,J,L1)+HLG*GG(2,N,IY1,J,L1))
     %     +PJL*(GG(1,N,IY1,J1,L1)+HLG*GG(2,N,IY1,J1,L1))
      GG0=GG0/(GN*MY*(GN1-GN0))
      GG1=GG1/(GN*MY*(GN1-GN0))
      GG10=GG1-GG0
C        sum of GG0 and GG1 must be 2
      R1=(R1-GN0)/(GN1-GN0)
      C1=0.25D0*GG10+2*R1-1
      PY=C1/(1+SQRT(1+GG10*C1))+0.5D0
      Y=(IY+PY)/MY
      IF(R10.LT.0) Y=-Y
      UN=N*Q
      XELE=0.5D0-0.5D0*SQRT(MAX(0D0,1-1/UN))*Y
      IF(ISPIN1.GE.1) THEN
        CALL NLBWFN0(1,NPH,XISQ,Q,XELE,G)
        HELE=(HL*G(2)+HG0*G(4))/(G(1)+HLG*G(3))
        HPOS=(-HL*G(2)+HG0*G(5))/(G(1)+HLG*G(3))
      ELSE
        HELE=0
        HPOS=0
      ENDIF
      IF(WGTBW(IY).EQ.1.AND.WGTBW(IY1).EQ.1) THEN
        WGT=1
      ELSE
        WGT=1/(WGTBW(IY)*(1-PY)+WGTBW(IY1)*PY)
      ENDIF
      IRTN=0
!!!!!!!!!!!
	IF(IDBGFLG(1).NE.0) THEN
	  WRITE(MSGFL,400) NPH,WGT
400     FORMAT('(NLBWGN0) Pair created: NPH=',I3,'  WGT=',1PD10.3)
	ENDIF
!!!!!!!!!!!
      RETURN
 900  IRTN=1001
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.NLBWGN0) Xi out of range.')
      RETURN
 910  IRTN=1002
      WRITE(MSGFL,915)
 915  FORMAT(' (SUBR.NLBWGN0) ETA out of range.')
      RETURN
 920  IRTN=100
c      WRITE(MSGFL,925)
c 925  FORMAT(' (SUBR.NLBWGN0) Pair creation rate too large.')
      RETURN
      END
