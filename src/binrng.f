      SUBROUTINE BINRNG(NP,NL,FLG,WGT,XY,XYRM,OUTMAX,LLR,
     %   WGTL,XYC,XYR)
C  Determine bin range for beam L=1 and 2
C  Input:
C    FLG    0: ingore, 1: beam #1,  2: beam #2
C    NL     1: beam#1 only, 2:beam#1 and #2
C    WGT    weight
C    XY     (x,y) coordinate
C    XYRM(1,IXY,L),XYRM(2,IXY,L)   possible size of the
C           bin range. (min and max)
C    OUTMAX   maximum fraction of particles outside the range.
C             (Not exactly obeyed)
C    LLR    flag to keep the vertical/horizontal ratio.
C           When LLR=1, XYRM(I,1,L)/XYRM(I,2,L) must
C           all be the same for I=1,2, L=1,2.
C  Output
C    WGTL(L)      sum og WGT in beam #L
C    XYC(IXY,L)   center of bin range
C    XYR(IXY,L)   full size of the bin range
C                  bin min(max)=XYC +(-)XYR/2
C
      IMPLICIT NONE
      INTEGER NP,NL,FLG(NP),LLR
      REAL*8 WGT(NP),XY(2,NP),XYRM(2,2,NL),OUTMAX,WGTL(NL),
     %     XYC(2,NL),XYR(2,NL)
      INTEGER IXY,L,N,I,J,JJ(2,2),I1,IXY1
      REAL*8 XYMM(2,2),SUM,DD(2),OMAX
      INTEGER MEX
      PARAMETER (MEX=5)
      REAL*8 OUT(MEX,2,2)
C
      DO 200 L=1,NL
        WGTL(L)=0
        DO 180 IXY=1,2
          XYC(IXY,L)=0
 180    CONTINUE
 200  CONTINUE
      DO 220 N=1,NP
        L=MIN(NL,FLG(N))
        IF(L.LE.0) GOTO 220
        WGTL(L)=WGTL(L)+WGT(N)
        DO 210 IXY=1,2
          XYC(IXY,L)=XYC(IXY,L)+XY(IXY,N)*WGT(N)
 210    CONTINUE
 220  CONTINUE
      DO 250 IXY=1,2
        DO 230 L=1,NL
          IF(WGTL(L).NE.0) XYC(IXY,L)=XYC(IXY,L)/WGTL(L)
 230    CONTINUE
 250  CONTINUE
C
      DO 560 L=1,NL
        DO 300 IXY=1,2
          XYR(IXY,L)=XYRM(1,IXY,L)
 300    CONTINUE
        IF(WGTL(L).EQ.0) GOTO 560
        IF(XYRM(2,1,L).EQ.XYRM(1,1,L)) GOTO 560
        DO 360 IXY=1,2
          XYMM(1,IXY)=XYC(IXY,L)-XYR(IXY,L)/2
          XYMM(2,IXY)=XYC(IXY,L)+XYR(IXY,L)/2
          DD(IXY)=(XYRM(2,IXY,L)-XYRM(1,IXY,L))/(2*MEX)
          DO 340 J=1,MEX
            OUT(J,1,IXY)=0
            OUT(J,2,IXY)=0
 340      CONTINUE
 360    CONTINUE
        DO 400 N=1,NP
          IF(MIN(NL,FLG(N)).NE.L) GOTO 400
          DO 380 IXY=1,2
            IF(XY(IXY,N).LE.XYMM(1,IXY)) THEN
              J=MIN(MEX,INT((XYMM(1,IXY)-XY(IXY,N))/DD(IXY))+1)
              OUT(J,1,IXY)=OUT(J,1,IXY)+WGT(N)
            ELSEIF(XY(IXY,N).GE.XYMM(2,IXY)) THEN
              J=MIN(MEX,INT((XY(IXY,N)-XYMM(1,IXY))/DD(IXY))+1)
              OUT(J,2,IXY)=OUT(J,2,IXY)+WGT(N)
            ENDIF
 380      CONTINUE
 400    CONTINUE
        SUM=0
        DO 440 IXY=1,2
          DO 430 I=1,2
            DO 420 J=MEX,1,-1
              OUT(J,I,IXY)=OUT(J,I,IXY)/WGTL(L)
              IF(J.NE.MEX) OUT(J,I,IXY)=OUT(J,I,IXY)+OUT(J+1,I,IXY)
 420        CONTINUE
            SUM=SUM+OUT(1,I,IXY)
 430      CONTINUE
 440    CONTINUE
        IF(SUM.LE.OUTMAX) GOTO 560
        DO 450 IXY=1,2
          JJ(1,IXY)=0
          JJ(2,IXY)=0
 450    CONTINUE
        IF(LLR.GE.1) THEN
          J=0
 460      J=J+1
          DO 480 IXY=1,2
            I=1
            IF(OUT(JJ(1,IXY)+1,1,IXY).LT.OUT(JJ(2,IXY)+1,2,IXY)) I=2
            JJ(I,IXY)=JJ(I,IXY)+1
            IF(JJ(I,IXY).EQ.MEX) THEN
              SUM=SUM-OUT(JJ(I,IXY),I,IXY)
            ELSE
              SUM=SUM-(OUT(JJ(I,IXY),I,IXY)-OUT(JJ(I,IXY)+1,I,IXY))
            ENDIF
 480      CONTINUE
          IF(SUM.GT.OUTMAX.AND.J.LT.MEX) GOTO 460
        ELSE
 500      OMAX=0
          DO 520 I1=1,2
            DO 510 IXY1=1,2
              IF(OUT(JJ(I1,IXY1)+1,I1,IXY1).GE.OMAX) THEN
                I=I1
                IXY=IXY1
                OMAX=OUT(JJ(I,IXY)+1,I,IXY)
              ENDIF
 510        CONTINUE
 520      CONTINUE
          JJ(I,IXY)=JJ(I,IXY)+1
          IF(JJ(I,IXY).EQ.MEX) THEN
            SUM=SUM-OUT(JJ(I,IXY),I,IXY)
          ELSE
            SUM=SUM-(OUT(JJ(I,IXY),I,IXY)-OUT(JJ(I,IXY)+1,I,IXY))
          ENDIF
          IF(SUM.GT.OUTMAX.AND.JJ(1,IXY)+JJ(2,IXY).LT.MEX) GOTO 500
        ENDIF
        DO 550 IXY=1,2
          XYMM(1,IXY)=XYC(IXY,L)-XYRM(1,IXY,L)/2-DD(IXY)*JJ(1,IXY)
          XYMM(2,IXY)=XYC(IXY,L)+XYRM(1,IXY,L)/2+DD(IXY)*JJ(2,IXY)
          XYC(IXY,L)=(XYMM(2,IXY)+XYMM(1,IXY))/2
          XYR(IXY,L)=XYMM(2,IXY)-XYMM(1,IXY)
 550    CONTINUE
 560  CONTINUE
      RETURN
      END

