      SUBROUTINE ARRAYSTR(NAM,NIND,N,M,IND,TEXT,NC)
C  Create a string for array with index like 
C  'abc(2,4,6)' (N=1) or 'a(1:2,3:4)' (N=2)
	IMPLICIT NONE
	INTEGER NIND,N,M,IND(M,NIND),NC
	CHARACTER(*) NAM,TEXT
	INTEGER NN,MC,I,N1,K,J,L
	CHARACTER(1) NUM(0:9)/'0','1','2','3','4','5','6','7','8','9'/

      NN=MAX(1,MIN(2,N))
	IF(M.LT.NN) RETURN
	MC=LEN(TEXT)
	NC=MIN(MC,LEN(NAM))
	TEXT=NAM(1:NC)
	IF(NC.GE.MC.OR.NIND.LE.0) RETURN
	NC=NC+1
	TEXT(NC:NC)='('
	DO I=1,NIND
	  DO J=1,NN
	    IF(J.GE.2) THEN
	      IF(NC.GE.MC) RETURN
	      NC=NC+1
	      TEXT(NC:NC)=':'
	    ENDIF
	    N1=IND(J,I)
	    K=10
          DO WHILE(K.LE.N1)
	      K=K*10
	    ENDDO
	    DO WHILE(K.GT.1)
	      K=K/10
	      L=N1/K
	      N1=N1-K*L
	      IF(NC.GE.MC) RETURN
	      NC=NC+1
	      TEXT(NC:NC)=NUM(L)
	    ENDDO
	    IF(I.NE.NIND.AND.J.EQ.NN) THEN
	      IF(NC.GE.MC) RETURN
	      NC=NC+1
	      TEXT(NC:NC)=','
	    ENDIF
	  ENDDO
	ENDDO
	IF(NC.GE.MC) RETURN
	NC=NC+1
	TEXT(NC:NC)=')'
	RETURN
	END

	  