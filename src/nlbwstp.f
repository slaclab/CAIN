      SUBROUTINE NLBWSTP
      IMPLICIT NONE
      INCLUDE 'include/nllsrcm.h'
      INCLUDE 'include/nlbwcm.h'
      CALL NLBWSTP1(MY,MPH,MXI,MQ,NPHMIN,YY,XISQ,QQ,
     %     LN0,NL0,R8(IPGINT))
      CALL NLBWSTP2(MY,MPH,MXI,MQ,NPHMIN,YY,XISQ,QQ,
     %     LN0,NL0,R8(IPGG))
      RETURN
      END
      SUBROUTINE NLBWSTP1(MY,MPH,MXI,MQ,NPHMIN,YY,XISQ,QQ,
     %     LN0,NL0,GINT)
      IMPLICIT NONE
      INTEGER MY,MPH,MXI,MQ,NPHMIN,LN0(MPH),NL0(MQ)
      REAL*8 YY(0:MY),XISQ(0:MXI),QQ(MQ),GINT(2,MPH,0:MY,0:MXI,MQ)
      INTEGER N,L,M,K
      REAL*8 FMAX,FMIN,F1
      INTEGER NXPL,NYPL,MM
      PARAMETER (NXPL=3,NYPL=2,MM=NXPL*NYPL)
      INTEGER J(MM),IX,IY
      REAL*8 XWL00/0D0/,XWH00/13D0/,YWL00/0D0/,YWH00/10D0/
      REAL*8 XW,YW,XWL,XWH,YWL,YWH,XWL0,XWH0,YWL0,YWH0
      INTEGER KK(2)/1,-1/
      CHARACTER*1 LKK(2)/'+','-'/
      INCLUDE 'include/topdraw.h'
C
      DO 200 M=1,MM
        J(M)=MXI-MM+M
 200  CONTINUE
      XWL0=XWL00+1.4
      XWH0=XWH00-0.1
      YWL0=YWL00+1.0
      YWH0=YWH00-1.0
      XW=(XWH0-XWL0)/NXPL
      YW=(YWH0-YWL0)/NYPL
      DO 500 K=1,2
        FMAX=0
        FMIN=0
        DO 240 N=NPHMIN,MPH
          DO 230 M=1,MM
            DO 220 L=LN0(N),MQ
              F1=GINT(1,N,MY,J(M),L)+KK(K)*GINT(2,N,MY,J(M),L)
              FMAX=MAX(FMAX,F1)
              FMIN=MIN(FMIN,F1)
 220        CONTINUE
 230      CONTINUE
 240    CONTINUE
        FMAX=1.05*FMAX
        FMIN=1.05*FMIN
        WRITE(8,300) LKK(K)
 300    FORMAT(' NEWFRAME; SET FONT DUPLEX',/,
     %     ' SET TICKS SIZE 0.05',/,
     %   ' TITLE 3.0 9.6 SIZE 2.0 ',2H'',/,
     %   ' MORE ',1H','Integral of G01n1',A1,'G03n1',1H',/,
     %   ' CASE ',1H','             X  X',1X,' X  X',1H',/,
     %   ' TITLE 6.0 0.5 SIZE 2.0 ',2H'',/,
     %   ' MORE ',1H','q=H/(1+X223)',1H',/,
     %   ' CASE ',1H','  G    GX X ',1H')
        IX=NXPL-1
        IY=NYPL
        DO 460 M=1,MM
          IX=IX+1
          IF(IX.GE.NXPL) THEN
            IX=0
            IY=IY-1
          ENDIF
          XWL=XWL0+XW*IX
          XWH=XWL+XW
          YWL=YWL0+YW*IY
          YWH=YWL+YW
          WRITE(8,320) XWL,XWH,YWL,YWH,QQ(1),QQ(MQ),FMIN,FMAX
 320      FORMAT(
     %     ' SET WINDOW X FROM ',F6.3,' TO ',F6.3,/,
     %     ' SET WINDOW Y FROM ',F6.3,' TO ',F6.3,/,
     %     ' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %     ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4,/,
     %     ' SET LABELS ALL OFF')
          IF(IX.EQ.0) WRITE(8,340) 'LEFT'
          IF(IY.EQ.0) WRITE(8,340) 'BOTTOM'
 340      FORMAT(' SET LABELS ',A,' ON')
          WRITE(8,350) XWL+0.2,YWH-0.2,XISQ(J(M))
 350      FORMAT(' TITLE ',2F7.3,' SIZE 1.5 ',2H'',/,
     %       ' MORE ',1H','X223=',0PF6.3,1H',/,
     %       ' CASE ',1H','GX X ',6X,    1H')
          DO 400 N=NPHMIN,MPH
            WRITE(8,360) (QQ(L),GINT(1,N,MY,J(M),L)
     %           +KK(K)*GINT(2,N,MY,J(M),L),L=LN0(N),MQ)
 360        FORMAT(3(1PD11.4,D12.4,';'))
            WRITE(8,380) PATTRN(MOD(N-1,MLMODE)+1)
 380        FORMAT(' SET PATTERN ',A,';JOIN 1 PATTERNED')
 400      CONTINUE
 460    CONTINUE
 500  CONTINUE
      RETURN
      END
      SUBROUTINE NLBWSTP2(MY,MPH,MXI,MQ,NPHMIN,YY,XISQ,QQ,
     %     LN0,NL0,GG)
      IMPLICIT NONE
      INTEGER MY,MPH,MXI,MQ,NPHMIN,LN0(MPH),NL0(MQ)
      REAL*8 YY(0:MY),XISQ(0:MXI),QQ(MQ),GG(2,MPH,0:MY,0:MXI,MQ)
      INTEGER N,L,K,J,I,L1
      REAL*8 FMAX,FMIN,F1,XRANGE
      INTEGER NXPL,NYPL
      PARAMETER (NXPL=4,NYPL=3)
      INTEGER IX,IY
      REAL*8 XWL00/0D0/,XWH00/13D0/,YWL00/0D0/,YWH00/10D0/
      REAL*8 XW,YW,XWL,XWH,YWL,YWH,XWL0,XWH0,YWL0,YWH0
      INTEGER KK(2)/1,-1/
      CHARACTER*1 LKK(2)/'+','-'/
      INCLUDE 'include/topdraw.h'
C
      J=MXI
      XWL0=XWL00+1.4
      XWH0=XWH00-0.1
      YWL0=YWL00+1.0
      YWH0=YWH00-1.0
      XW=(XWH0-XWL0)/NXPL
      YW=(YWH0-YWL0)/NYPL
      IX=NXPL-1
      IY=NYPL
      DO 500 L=2,MQ
        IX=IX+1
        IF(IX.GE.NXPL) THEN
          IX=0
          IY=IY-1
          IF(IY.LT.0) THEN
            IX=0
            IY=NYPL-1
          ENDIF
        ENDIF
        XWL=XWL0+XW*IX
        XWH=XWL+XW
        YWL=YWL0+YW*IY
        YWH=YWL+YW
        IF(IX.EQ.0.AND.IY.EQ.NYPL-1) THEN
          WRITE(8,200) XISQ(J)
 200      FORMAT(' NEWFRAME; SET FONT DUPLEX',/,
     %       ' SET TICKS SIZE 0.05',/,
     %       ' TITLE 3.0 9.6 SIZE 2.0 ',2H'',/,
     %       ' MORE ',1H','G01n1UG03n1   X223=',F6.3,1H',/,
     %       ' CASE ',1H',' X  XM X  X   GX X ',6X,  1H',/,
     %       ' TITLE 6.0 0.4 ',1H','E0e1/W0G1',1H',/,
     %       '          CASE ',1H',' X X FXGX',1H')
        ENDIF
        IF(IX.EQ.0) THEN
          FMAX=0
          FMIN=0
          DO 260 L1=L,MIN(MQ,L+NXPL-1)
            DO 250 K=1,2
              DO 240 N=NL0(L1),MPH
                XRANGE=0.5D0*SQRT(MAX(0D0,1-1/(N*QQ(L1))))
                IF(XRANGE.EQ.0) GOTO 240
                DO 220 I=0,MY
                  F1=(GG(1,N,I,J,L1)+KK(K)*GG(2,N,I,J,L1))/XRANGE
                  FMAX=MAX(FMAX,F1)
                  FMIN=MIN(FMIN,F1)
 220            CONTINUE
 240          CONTINUE
 250        CONTINUE
 260      CONTINUE
          FMAX=1.05*FMAX
          FMIN=1.05*FMIN
        ENDIF
        WRITE(8,320) XWL,XWH,YWL,YWH,0D0,0.5D0,FMIN,FMAX
 320    FORMAT(
     %   ' SET WINDOW X FROM ',F6.3,' TO ',F6.3,/,
     %   ' SET WINDOW Y FROM ',F6.3,' TO ',F6.3,/,
     %   ' SET LIMIT X FROM ',1PD11.4,' TO ',D11.4,/,
     %   ' SET LIMIT Y FROM ',1PD11.4,' TO ',D11.4,/,
     %   ' SET LABELS ALL OFF')
        IF(IX.EQ.0) WRITE(8,340) 'LEFT'
        IF(IY.EQ.0) WRITE(8,340) 'BOTTOM'
 340    FORMAT(' SET LABELS ',A,' ON')
        WRITE(8,350) XWL+0.2,YWH-0.2,QQ(L)
 350    FORMAT(' TITLE ',2F7.3,' SIZE 1.5 ',2H'',/,
     %    ' MORE ',1H','q=',0PF6.3,1H',/,
     %    ' CASE ',1H','  ',6X,    1H')
        DO 400 N=NL0(L),MPH
          XRANGE=0.5D0*SQRT(MAX(0D0,1-1/(N*QQ(L))))
          IF(XRANGE.EQ.0) GOTO 400
          DO 390 K=1,2
            WRITE(8,360) QQ(L),N,LKK(K)
 360        FORMAT(' ( Q=',0PF7.3,' Nph=',I2,'   G1',A1,'G2')
            WRITE(8,370) (0.5D0-XRANGE*YY(I),(GG(1,N,I,J,L)
     %        +KK(K)*GG(2,N,I,J,L))/XRANGE,I=0,MY)
 370        FORMAT(3(1PD11.4,D12.4,';'))
            WRITE(8,380) PATTRN(MOD(N-1,MLMODE)+1)
 380        FORMAT(' SET PATTERN ',A,';JOIN 1 PATTERNED')
 390      CONTINUE
 400    CONTINUE
 500  CONTINUE
      RETURN
      END
