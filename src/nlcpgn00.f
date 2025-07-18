      SUBROUTINE NLCPGN00(XI,LAM,DT,EEFF,HL,HEI,PMAX,ISPIN1,
     %     NPH,X,HEF,HG,PROB,WGT,IRTN,
     %     MPH,MY,MXI,MLM,XISQMX,LMMAX,FALLMX,FF,FINT,FALL,BES,
     %     WGTCP)
C      Name changed NLCPGN0 --> NLCPGN00 (Jun.24.2011)
      IMPLICIT NONE
      INTEGER ISPIN1,NPH,IRTN
      REAL*8 XI,LAM,DT,EEFF,HL,HEI,PMAX,X,HEF,HG,PROB,WGT
      INTEGER MPH,MY,MXI,MLM
      REAL*8 XISQMX,LMMAX,FALLMX(2),FF(2,MPH,0:MY,0:MXI,0:MLM),
     %   FINT(2,MPH,0:MY,0:MXI,0:MLM),FALL(2,0:MXI,0:MLM),
     %   BES(3,MPH,0:MY,0:MXI),WGTCP(0:MY)
      REAL*8 FAC/9.65650270D15/
C        FAC= (fine_str_const)*(electron rest energy)
C              /(Compton wavelength) in units of eV/meter
C
      INTEGER J,L,J1,L1,N,I,IY,IY1
      REAL*8 XISQ,PJ,PJ1,PL,PL1,PJL,PJ1L,PJL1,PJ1L1,
     %   HLE,FAC1,R1,FFX1,FFX2,
     %   FFX,FN,FN0,FN1,FF0,FF1,FF10,C1,PY,Y,V,VMAX,F(5)
      REAL*8 RANDCAIN
	EXTERNAL RANDCAIN
        REAL*8 EMASS/0.511D6/
        INTEGER NOUTR/0/
        SAVE NOUTR
	INCLUDE 'include/ctrlcm.h'
C
      NPH=0
      IRTN=0
      XISQ=XI**2
C Decide to emit or not to emit
      IF(ISPIN1.EQ.0) THEN
        HLE=0
      ELSE
        HLE=HL*HEI
      ENDIF
      FAC1=FAC*DT/EEFF*XISQ
      R1=RANDCAIN()
      IF(ISPIN1.EQ.0) THEN
        PROB=FAC1*(FALLMX(1)+ABS(HLE)*FALLMX(2))
        IF(PROB.LE.PMAX.AND.R1.GE.PMAX) RETURN
C           Rejection at this level is possible when ISPIN1=0.
C           When polarization effect is needed, FALL must be
C           calculated even when the event is rejected.
      ENDIF
      PJ=XISQ/XISQMX*MXI
      J=INT(PJ)
      IF(J.GE.MXI) GOTO 900
      PJ=PJ-J
      PJ1=1-PJ
      J1=J+1
      PL=LAM/LMMAX*MLM
      L=INT(PL)
      IF(L.GE.MLM) GOTO 910
      PL=PL-L
      PL1=1-PL
      L1=L+1
      PJL=PJ*PL
      PJL1=PJ*PL1
      PJ1L=PJ1*PL
      PJ1L1=PJ1*PL1
      FFX1=PJ1L1*FALL(1,J,L)+PJL1*FALL(1,J1,L)
     %    +PJ1L*FALL(1,J,L1)+PJL*FALL(1,J1,L1)
      FFX2=PJ1L1*FALL(2,J,L)+PJL1*FALL(2,J1,L)
     %    +PJ1L*FALL(2,J,L1)+PJL*FALL(2,J1,L1)
      FFX=FFX1+HLE*FFX2
      PROB=FAC1*FFX
      IF(PROB.GE.PMAX) GOTO 920
C Rejected case
      IF(R1.GE.PROB) THEN
        IF(ISPIN1.NE.0) HEI=MAX(-1D0,MIN(1D0,
     %           (HEI*(1-FAC1*FFX1)-FAC1*FFX2*HL)/(1-PROB)))
        RETURN
      ENDIF
C Decide how many laser photons to absorb
      FFX=0
      DO 200 N=1,MPH
        FN=PJ1L1*(FINT(1,N,MY,J,L)+HLE*FINT(2,N,MY,J,L))
     %     +PJL1*(FINT(1,N,MY,J1,L)+HLE*FINT(2,N,MY,J1,L))
     %     +PJ1L*(FINT(1,N,MY,J,L1)+HLE*FINT(2,N,MY,J,L1))
     %      +PJL*(FINT(1,N,MY,J1,L1)+HLE*FINT(2,N,MY,J1,L1))
        FFX=FFX+FN*FAC1
        NPH=N
        IF(R1.LE.FFX) GOTO 240
 200  CONTINUE
C Decide the photon energy and angle
C        (one-to-one because nph is given)
 240  VMAX=NPH*LAM/(1+XISQ)
      R1=RANDCAIN()
      FN1=0
      DO 300 I=1,MY-1
        FN0=FN1
        FN1=PJ1L1*(FINT(1,N,I,J,L)+HLE*FINT(2,N,I,J,L))
     %      +PJL1*(FINT(1,N,I,J1,L)+HLE*FINT(2,N,I,J1,L))
     %      +PJ1L*(FINT(1,N,I,J,L1)+HLE*FINT(2,N,I,J,L1))
     %       +PJL*(FINT(1,N,I,J1,L1)+HLE*FINT(2,N,I,J1,L1))
        FN1=FN1/FN
        IY1=I
        IF(R1.LE.FN1) GOTO 320
 300  CONTINUE
C       inverse interpolation
      IY1=MY
      FN0=FN1
      FN1=1
 320  IY=IY1-1
      FF0=PJ1L1*(FF(1,N,IY,J,L)+HLE*FF(2,N,IY,J,L))
     %    +PJL1*(FF(1,N,IY,J1,L)+HLE*FF(2,N,IY,J1,L))
     %    +PJ1L*(FF(1,N,IY,J,L1)+HLE*FF(2,N,IY,J,L1))
     %     +PJL*(FF(1,N,IY,J1,L1)+HLE*FF(2,N,IY,J1,L1))
      FF1=PJ1L1*(FF(1,N,IY1,J,L)+HLE*FF(2,N,IY1,J,L))
     %    +PJL1*(FF(1,N,IY1,J1,L)+HLE*FF(2,N,IY1,J1,L))
     %    +PJ1L*(FF(1,N,IY1,J,L1)+HLE*FF(2,N,IY1,J,L1))
     %     +PJL*(FF(1,N,IY1,J1,L1)+HLE*FF(2,N,IY1,J1,L1))
      FF0=FF0/(FN*MY*(FN1-FN0))
      FF1=FF1/(FN*MY*(FN1-FN0))
      FF10=FF1-FF0
C        sum of FF0 and FF1 must be 2
      R1=(R1-FN0)/(FN1-FN0)
      C1=0.25D0*FF10+2*R1-1
      PY=C1/(1+SQRT(1+FF10*C1))+0.5D0
      Y=(IY+PY)/MY
      V=VMAX*Y
      X=V/(1+V)
      IF(WGTCP(IY).EQ.1.AND.WGTCP(IY1).EQ.1) THEN
        WGT=1
      ELSE
        WGT=1/((1-PY)*WGTCP(IY)+PY*WGTCP(IY1))
      ENDIF
      IF(ISPIN1.GE.1) THEN
        CALL NLCPFN0(1,NPH,XISQ,LAM,X,F)
        HG=(HL*F(3)+HEI*F(4))/(F(1)+HLE*F(2))
        HEF=(HEI*(F(1)+F(5))+HL*F(2))/(F(1)+HLE*F(2))
        IF(WGT.LT.1) HEI=MAX(-1D0,MIN(1D0,
     %           (HEI*(1-FAC1*FFX1)-FAC1*FFX2*HL)/(1-PROB)))
      ELSE
        HG=0
        HEF=0
      ENDIF
      IRTN=0
      RETURN
 900  IRTN=1001
      WRITE(MSGFL,905) XISQ
 905  FORMAT(' (SUBR.NLCPGN00) Xi out of range. Xi^2=',1PD11.4)
      NOUTR=NOUTR+1
      IF(NOUTR.GT.10000) STOP
      RETURN
 910  IRTN=1002
      WRITE(MSGFL,915) LAM
 915  FORMAT(' (SUBR.NLCPGN00) Lambda out of range. Lambda=',1PD11.4)
      RETURN
 920  IRTN=100
c      WRITE(MSGFL,925)
c 925  FORMAT(' (SUBR.NLCPGN00) Radiation rate too large.')
      RETURN
      END
