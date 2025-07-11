      SUBROUTINE ADDONE(N,KIN,GN,PNAM,ISB,WT,TXY,EP1,SP,
     %    LBBF,BFL,IRTN)
	USE BEAMCM
C Add one maco-particle in the list.
C If N=0, append at the end.
C    1<=N<=NP, replace N-th particle by the new data.
      IMPLICIT NONE
      INTEGER N,KIN,GN,ISB,LBBF,IRTN
      CHARACTER*4 PNAM
      REAL*8 WT,TXY(0:3),EP1(0:3),SP(3),BFL(3,2)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/beamcm.h'
      INTEGER I,N1
C
      IF(N.LT.0.OR.N.GT.NP) GOTO 900
      IF(N.EQ.0) THEN        
        IF(NP.GE.MP) GOTO 910
        NP=NP+1
        N1=NP
      ELSE
        N1=N
      ENDIF
      KIND(N1)=KIN
      GEN(N1)=GN
      PNAME(N1)=PNAM
	LOST(N1)=0
      ISBIN(N1)=ISB
      WGT(N1)=WT
      LBBFIN(N1)=LBBF
c      write(msgfl,49) np,kin,gn,pnam,isb,wt,ep1(0),ep1(3)
c 49   format(' addone np=',i8,' kin= ',i3,' gn= ',i8,
c     &     ' pnam= ',a4,' isb= ',i4,' wt= ',(1pd11.3),
c     &     ' ep(0)= ',(1pd11.3),' ep(3)= ',(1pd11.3)) 
      DO 200 I=0,3
        TXYS(I,N1)=TXY(I)
        EP(I,N1)=EP1(I)
 200  CONTINUE
      DO 220 I=1,3
        SPIN(I,N1)=SP(I)
 220  CONTINUE
      DO 240 I=1,3
        FLD(I,1,N1)=BFL(I,1)
        FLD(I,2,N1)=BFL(I,2)
 240  CONTINUE
      IRTN=0
      RETURN
 900  IRTN=1000
      IF(MSGFL.GE.1) WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.ADDONE) Invalid first argument.')
      RETURN
 910  IRTN=1001
      print *, " addone np,mp= ",np,mp
      RETURN
      END
