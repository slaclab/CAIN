      SUBROUTINE BBCOUL(NX,NY,DX,DY,X00,MX,MY,Q,
     %    NCX,NCY,DSUM,DCX,DCY,CF,NP,IFLG,FXY)
C Direct Coulomb force
C  Input FXY is the (x,y) coordinate from the mesh region center.
      IMPLICIT NONE
      INTEGER NX,NY,NCX,NCY,MX,MY,NP,IFLG(NP)
      REAL*8 DX,DY,X00(2),DSUM,DCX,DCY,CF(2,0:NCX,0:NCY),Q(MX,MY),
     %   FXY(2,NP)
      INTEGER N,I,J,I1,J1,K
      REAL*8 X1,Y1,X10,Y10,RR,FXY1(2),PX,PY
C
      CALL CPUTIM('BBCOUL',1)
      DO 300 N=1,NP
      IF(IFLG(N).NE.1) GOTO 300
      X10=FXY(1,N)+X00(1)
      Y10=FXY(2,N)+X00(2)
      FXY(1,N)=0
      FXY(2,N)=0
      DO 280 I=1,NX
        X1=X10-I*DX
        DO 260 J=1,NY
          Y1=Y10-J*DY
          IF(Q(I,J).NE.0) THEN
            IF(ABS(X1).LE.DCX.AND.ABS(Y1).LE.DCY) THEN
              PX=ABS(X1)/DCX*NCX
              PY=ABS(Y1)/DCY*NCY
              I1=MIN(NCX-1,INT(PX))
              J1=MIN(NCY-1,INT(PY))
              PX=PX-I1
              PY=PY-J1
              DO 220 K=1,2
                FXY1(K)=(1-PY)*((1-PX)*CF(K,I1,J1)+PX*CF(K,I1+1,J1))
     %                 +PY*((1-PX)*CF(K,I1,J1+1)+PX*CF(K,I1+1,J1+1))
                FXY1(K)=FXY1(K)/DSUM*Q(I,J)
 220          CONTINUE
              IF(X1.LT.0) FXY1(1)=-FXY1(1)
              IF(Y1.LT.0) FXY1(2)=-FXY1(2)
              FXY(1,N)=FXY(1,N)+FXY1(1)
              FXY(2,N)=FXY(2,N)+FXY1(2)
            ELSE
              RR=X1**2+Y1**2
              FXY(1,N)=FXY(1,N)-Q(I,J)*X1/RR
              FXY(2,N)=FXY(2,N)-Q(I,J)*Y1/RR
            ENDIF
          ENDIF
 260    CONTINUE
 280  CONTINUE
 300  CONTINUE
      CALL CPUTIM('BBCOUL',2)
      RETURN
      END
