      SUBROUTINE BBBIN(T,IS)
	USE BEAMCM
	USE BBCOM
C  Binning for beam-beam force
      IMPLICIT NONE
      INTEGER IS
      REAL*8 T
C      INCLUDE 'include/beamcm.h'
C      INCLUDE 'include/bbcom.h'
      INTEGER IXY,L,N,I,IJ(2),I00,I10,I01,I11,I000(2)
      REAL*8 PXY(2),PSIZE1,WCHG,WOUT(2),WTOT(2)
      REAL*8 CHG(3)/0D0,-1D0,1D0/
C
      CALL CPUTIM('BBBIN',1)
      DO 180 L=1,2
        WOUT(L)=0
        WTOT(L)=0
 180  CONTINUE
      I000(1)=0
      I000(2)=NXY(1)*NXY(2)
      DO 200 I=1,NXY(1)*NXY(2)*2
        BBQ(I)=0
 200  CONTINUE
      PSIZE1=1-PSIZE
      DO 300 N=1,NP
        IF(ISBIN(N).NE.IS) GOTO 300
        IF(KIND(N).EQ.1) GOTO 300
	  IF(LOST(N).NE.0) GOTO 300
        IF(PNAME(N).NE.'    ') GOTO 300
        IF(TXYS(0,N).GT.T) GOTO 300
C          exclude those created at time>T.
        IF(EP(3,N).GE.0) THEN
          L=1
        ELSE
          L=2
        ENDIF
        IF(BBFLG(L).EQ.0) GOTO 300
        DO 210 IXY=1,2
          PXY(IXY)=TXYS(IXY,N)+EP(IXY,N)/EP(0,N)*(T-TXYS(0,N))
          PXY(IXY)=(PXY(IXY)-XYMIN(IXY,L))/BBDXY(IXY,L)+1D0
          IJ(IXY)=INT(PXY(IXY))
          IF(IJ(IXY).LE.0.OR.IJ(IXY).GT.NXY(IXY)) THEN
            WOUT(L)=WOUT(L)+WGT(N)
            GOTO 300
          ENDIF
          PXY(IXY)=PXY(IXY)-IJ(IXY)
          WTOT(L)=WTOT(L)+WGT(N)
 210    CONTINUE
        WCHG=WGT(N)*CHG(KIND(N))
        IF(PSIZE.LE.0.OR.
     %     (PXY(1).GE.PSIZE.AND.PXY(1).LE.PSIZE1.AND.
     %      PXY(2).GE.PSIZE.AND.PXY(2).LE.PSIZE1)) THEN
          I=I000(L)+IJ(1)+NXY(1)*(IJ(2)-1)
          BBQ(I)=BBQ(I)+WCHG
        ELSE
          DO 240 IXY=1,2
            IF(PXY(IXY).LT.0.5D0) THEN
              IF(IJ(IXY).NE.1) THEN
                IJ(IXY)=IJ(IXY)-1
                PXY(IXY)=0.5D0-PXY(IXY)/PSIZE
              ELSE
                PXY(IXY)=1
              ENDIF
            ELSE
              IF(IJ(IXY).EQ.NXY(IXY)) THEN
                IJ(IXY)=IJ(IXY)-1
                PXY(IXY)=0
              ELSE
                PXY(IXY)=0.5D0+(1-PXY(IXY))/PSIZE
              ENDIF
            ENDIF
 240      CONTINUE
          I00=I000(L)+IJ(1)+NXY(1)*(IJ(2)-1)
          I10=I00+1
          I01=I00+NXY(1)
          I11=I01+1
          BBQ(I00)=BBQ(I00)+WCHG*PXY(1)*PXY(2)
          BBQ(I10)=BBQ(I10)+WCHG*(1-PXY(1))*PXY(2)
          BBQ(I01)=BBQ(I01)+WCHG*PXY(1)*(1-PXY(2))
          BBQ(I11)=BBQ(I11)+WCHG*(1-PXY(1))*(1-PXY(2))
        ENDIF
 300  CONTINUE
      DO 320 L=1,2
        WTOT(L)=WTOT(L)+WOUT(L)
        IF(WTOT(L).NE.0) THEN
          WOUT(L)=WOUT(L)/WTOT(L)
          WGTOUT(L)=MAX(WGTOUT(L),WOUT(L))
        ENDIF
 320  CONTINUE
      CALL CPUTIM('BBBIN',2)
      RETURN
      END
