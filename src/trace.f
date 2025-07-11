C******************** TRACE ********************************************
      SUBROUTINE TRACE(IX,IY,KXY,KD,F,IC,MX,NX,NY,FV,IR,CO,SI,X,Y,
     %    ID,MP,NP,LP,IRTN)
      IMPLICIT NONE
      INTEGER IX,IY,KXY,KD,IC,MX,NX,NY,IR,MP,ID(MP),NP,IRTN
      REAL*8 F(IC,0:MX,0:NY),X(MP),Y(MP),CO,SI,FV
      LOGICAL*1 LP(0:NX,0:NY,2)
      INTEGER IX1,IY1,KXY1,KD1,KK,IEDGE
      REAL*8 FVLIN1,F1
      INTEGER IDX(4)/1,0,0,-1/,IDY(4)/-1,1,0,0/
	INCLUDE 'include/ctrlcm.h'
C
      NP=NP+1
c      print *, " trace point 001 irtn= ",irtn
      IF(NP.GT.MP) GOTO 900
      IX1=IX
      IY1=IY
      KXY1=KXY
      KD1=KD
c      print *, " trace point 002 irtn= ",irtn
      F1=FVLIN1(F(1,IX1,IY1),IC,IR,CO,SI)
      IF(KXY1.EQ.2) THEN
        X(NP)=IX1
        Y(NP)=IY1-(FV-F1)/(FVLIN1(F(1,IX1,IY1-1),IC,IR,CO,SI)-F1)
      ELSE
        X(NP)=IX1-(FV-F1)/(FVLIN1(F(1,IX1-1,IY1),IC,IR,CO,SI)-F1)
        Y(NP)=IY1
      ENDIF
      ID(NP)=MOD(KD1+2,4)+1
      IF(KD1.EQ.2.AND.(IY1.NE.0.AND.IY1.NE.NY)) ID(NP)=5
  200 DO 220 KK=1,3
      IX1=IX1+IDX(KD1)
      IY1=IY1+IDY(KD1)
      KXY1=3-KXY1
      IF(LP(IX1,IY1,KXY1)) GOTO 240
      KD1=MOD(KD1,4)+1
  220 CONTINUE
      GOTO 920
  240 NP=NP+1
c      print *, " trace point 003 irtn= ",irtn
      IF(NP.GT.MP) GOTO 900
      IEDGE=0
      F1=FVLIN1(F(1,IX1,IY1),IC,IR,CO,SI)
      IF(KXY1.EQ.2) THEN
        X(NP)=IX1
        Y(NP)=IY1-(FV-F1)/(FVLIN1(F(1,IX1,IY1-1),IC,IR,CO,SI)-F1)
        IF(IX1.EQ.0)  IEDGE=4
        IF(IX1.EQ.NX) IEDGE=2
      ELSE
        X(NP)=IX1-(FV-F1)/(FVLIN1(F(1,IX1-1,IY1),IC,IR,CO,SI)-F1)
        Y(NP)=IY1
        IF(IY1.EQ.0) IEDGE=1
        IF(IY1.EQ.NY) IEDGE=3
      ENDIF
c      print *, " trace point 004 irtn= ",irtn
      ID(NP)=0
      LP(IX1,IY1,KXY1)=.FALSE.
      IF(IEDGE.NE.0) THEN
        ID(NP)=-IEDGE
        GOTO 320
      ENDIF
c      print *, " trace point 005 irtn= ",irtn
      IF(IX1.EQ.IX.AND.IY1.EQ.IY.AND.KXY1.EQ.KXY) THEN
        ID(NP)=-5
        GOTO 320
      ENDIF
c      print *, " trace point 006 irtn= ",irtn
      KD1=MOD(KD1+2,4)+1
      GOTO 200
  320 LP(IX,IY,KXY)=.FALSE.
c      print *, " trace point 007 irtn= ",irtn
      IRTN=0
      RETURN
  900 CONTINUE
c      print *, " trace point 008 irtn= ",irtn
      NP=NP-1
      LP(IX,IY,KXY)=.FALSE.
      IRTN=1
      print *, " trace np= ",np," mp= ",mp
      RETURN
  920 WRITE(MSGFL,930)
  930 FORMAT(' (SUBR.TRACE) PROGRAM ERROR.')
      IRTN=2
      RETURN
      END
