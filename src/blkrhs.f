      SUBROUTINE BLKRHS(TEXT,X,MX,NX,GSTRRD,NGSTRRD,IRTN)
C Evaluate each term in the form
C        a,b,c...
C and store their values in X(i).
C If the whole expression is enclosed by a pair of
C either () or [] or {}, the pair is eliminated.
C If an item is missing (i.e. like "a,,c"), the
C corresponding component of X remains unchanged.
C NX is the last element of X that has changed.
C  When X(i)%L==2, the character string is stored in
C   GSTRRD(X(i)%C(1):X(i)%C(2))
C  Note that NGSTRRD is not cleared.
	USE FLCHTYP
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER MX,NX,NGSTRRD,IRTN
      TYPE(FLCHTYPE) X(MX)
      CHARACTER(*) TEXT,GSTRRD
      INCLUDE 'include/ctrlcm.h'
      INTEGER IDARR,ID1,I,NC,N1,N2,N,NN,K,CATCOD,C(2)
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR
C
      NX=0
      IRTN=0
      NC=LEN(TEXT)
	CALL IFARRAY(TEXT,IDARR)
	IF(IDARR.NE.0) THEN
	  ID1=ABS(IDARR)
	  NX=1
	  IF(ARR(ID1)%RANK.GT.0) THEN
	    DO I=1,ARR(ID1)%RANK
	      NX=NX*ARR(ID1)%DIM(3,I)
	    ENDDO
	  ENDIF
	  NX=MIN(MX,NX)
	  IF(IDARR.GT.0) THEN
	    DO I=1,NX
	      X(I)%L=1
	      X(I)%X=ARR(ID1)%VAL(I)
	      X(I)%C(1)=1
	      X(I)%C(2)=0
	    ENDDO
        ELSE
	    DO I=1,NX
	      C(1:2)=ARR(ID1)%LC(1:2,I)
	      CALL FLCHSET3(GSTR(C(1):C(2)),X(I),GSTRRD,NGSTRRD,ERR)
	      IF(ERR.NE.' ') GOTO 920
	    ENDDO
	  ENDIF
	ENDIF
	IF(NX.NE.0) RETURN
      DO 200 I=1,NC
        IF(TEXT(I:I).NE.' ') THEN
          N1=I
          K=CATCOD(TEXT(I:I))-10
          IF(K.GE.1.AND.K.LE.3) GOTO 220   !  opening parens
          N2=NC
          GOTO 260
        ENDIF
 200  CONTINUE
      RETURN
!!!       Change on Sep.18.2003
 220  DO 240 I=NC,N1+1,-1
        IF(TEXT(I:I).NE.' ') THEN
	    IF(CATCOD(TEXT(I:I))-20.NE.K) THEN
	      N2=I
		GOTO 260
	    ENDIF
!            closing parens
	    N1=N1+1
c 220  N1=N1+1
c      DO 240 I=NC,N1+1,-1
c        IF(TEXT(I:I).NE.' ') THEN
c          IF(CATCOD(TEXT(I:I))-20.NE.K) GOTO 900
c!            closing parens
          N2=I-1
          GOTO 260
        ENDIF
 240  CONTINUE
      GOTO 900
 260  N=N1
 300  CALL FNDCH2(TEXT,N,N2,',',NN,IRTN)
      IF(IRTN.NE.0) NN=N2+1
      IF(NN.GT.N) THEN
        NX=NX+1
        IF(TEXT(N:NN-1).NE.' ') THEN
          CALL EVAL0(TEXT(N:NN-1),FC,ERR)
          IF(ERR.NE.' ') GOTO 910
	    IF(FC%L.EQ.2) THEN
	      CALL FLCHSET3(GSTR2(EVALLAST)(FC%C(1):FC%C(2)),X(NX),
     %              GSTRRD,NGSTRRD,ERR)
            IF(ERR.NE.' ') GOTO 920
	    ELSE
            X(NX)=FC
	    ENDIF
        ENDIF
      ELSE
        IF(NN.LE.N2) NX=NX+1
      ENDIF
      IF(NX.LT.MX) THEN
        N=NN+1
        IF(N.LE.N2) GOTO 300
      ENDIF
      IRTN=0
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905) TEXT
 905  FORMAT(' (SUBR.BLKRHS) Invalid syntax ',/,
     %  ' "',A,'".')
      RETURN
 910  IRTN=1001
      WRITE(MSGFL,915) TEXT(N:NN-1),ERR
 915  FORMAT(' (SUBR.BLKRHS) Invalid expression ',/,
     %  ' "',A,'".',/,5X,A)
 920  IRTN=1002
	WRITE(MSGFL,925) ERR
 925  FORMAT(A)
	CALL STOPCAIN(100)
      RETURN
 930  IRTN=1003
	WRITE(MSGFL,935)
 935  FORMAT(' **** Character buffer full *****')
	CALL STOPCAIN(100)
      RETURN
      END

      
