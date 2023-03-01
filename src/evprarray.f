      SUBROUTINE EVPRARRAY(LONG,IFL)
C  Print array
C   LONG=0    array name, index range only
C       =1    array contents too
	USE ARRAYMOD
	IMPLICIT NONE
	INTEGER LONG,IFL
	INTEGER I,J,N,NC,MAXRANK,MAXIND,MAXNC,K,NCFMT,K1,ITY,ISTAT,
     %    NL,NCVAL,NCL,N1,NC1,NC2
	CHARACTER(200) TEXT
	CHARACTER(50) FMT
	INTEGER IND(20),IRTN
	CHARACTER(80) ERR
	CHARACTER(9) LTYPE(2)/'Floating','Character'/
	CHARACTER(1) CH
	INTEGER, PARAMETER:: MCL=1024
	CHARACTER(MCL), ALLOCATABLE:: PAGE(:)

	IF(NARRAY.LE.0) GOTO 900
	DO ITY=1,2
	  N=0
	  DO I=1,NARRAY
	    IF(ARR(I)%TYPE.EQ.ITY) N=N+1
	  ENDDO
	  IF(N.EQ.0) CYCLE
	  WRITE(IFL,200) LTYPE(ITY),N
200     FORMAT(' ++++ List of ',A,' type Arrays  ',I5,
     %            ' arrays defined ++++')
	  DO I=1,NARRAY
	    IF(ARR(I)%TYPE.NE.ITY) CYCLE
	    IF(ARR(I)%RANK.GE.1) THEN
	      CALL ARRAYSTR(ARR(I)%NAME(1:ARR(I)%NC),ARR(I)%RANK,2,3,
     %         ARR(I)%DIM,TEXT,NC)
	    ELSE
	      NC=ARR(I)%NC
	      TEXT=ARR(I)%NAME
	    ENDIF
	    WRITE(IFL,'(2X,A)') TEXT(1:NC)
	  ENDDO
	  NCL=0
	  IF(LONG.EQ.0) CYCLE
  
	  MAXRANK=0
	  MAXIND=0
	  MAXNC=0
	  NCVAL=1
	  IF(ITY.EQ.1) NCVAL=15
	  NL=0
	  DO I=1,NARRAY
	    IF(ARR(I)%TYPE.EQ.ITY) THEN
	      MAXNC=MAX(MAXNC,ARR(I)%NC)
	      MAXRANK=MAX(MAXRANK,ARR(I)%RANK)
	      N1=1
	      IF(ARR(I)%RANK.GE.1) THEN
	        DO J=1,ARR(I)%RANK
	          MAXIND=MAX(MAXIND,ARR(I)%DIM(2,J))
	          IF(ARR(I)%DIM(1,J).LT.0) THEN
	            MAXIND=MAX(MAXIND,-10*ARR(I)%DIM(1,J))
	          ENDIF
	        ENDDO
	        DO J=1,ARR(I)%RANK
	          N1=N1*ARR(I)%DIM(3,J)
	        ENDDO
	      ENDIF
	      NL=NL+N1
	      IF(ITY.EQ.2) THEN
	        DO J=1,N1
	          NCVAL=MAX(NCVAL,ARR(I)%LC(2,J)-ARR(I)%LC(1,J)+1)
	        ENDDO
	      ENDIF
	    ENDIF
	  ENDDO
	  K=10
	  NC=1
	  DO WHILE (K.LE.MAXIND)
	    K=K*10
	    NC=NC+1
	  ENDDO
	  NCL=MAXNC+1+MAXRANK*(NC+1)+1+1+NCVAL
	  IF(NCL.GT.MCL) GOTO 900
	  ALLOCATE(PAGE(NL),STAT=ISTAT)
	  IF(ISTAT.NE.0) GOTO 910
	  N=0
	  DO I=1,NARRAY
	    IF(ARR(I)%TYPE.NE.ITY) CYCLE
	    N1=1
	    CH=' '
	    NC1=ARR(I)%NC
	    IF(ARR(I)%RANK.GE.1) THEN
	      CH='('
	      NC1=NC1+1
	      DO J=1,ARR(I)%RANK
	        N1=N1*ARR(I)%DIM(3,J)
	      ENDDO
	    ENDIF
	    DO K=1,N1
	      PAGE(N+K)=ARR(I)%NAME(1:ARR(I)%NC)//CH
	    ENDDO
	    IF(ARR(I)%RANK.GE.1) THEN
	      DO J=1,ARR(I)%RANK
	        MAXIND=MAX(1,ARR(I)%DIM(2,J))
	        IF(ARR(I)%DIM(1,J).LT.0) THEN
	          MAXIND=MAX(MAXIND,-10*ARR(I)%DIM(1,J))
	        ENDIF
		      K=10
	        NC=1
	        DO WHILE (K.LE.MAXIND)
	          K=K*10
	          NC=NC+1
	        ENDDO
	        IF(J.EQ.1) THEN
	          WRITE(FMT,300) NC
300             FORMAT("(I",I5,")")
                NC2=NC1+NC
              ELSE
	          WRITE(FMT,310) NC
310             FORMAT("(',',I",I5,")")
                NC2=NC1+NC+1
              ENDIF
	        DO K=1,N1
	          CALL ARRN2IND(IND,ARR(I)%RANK,ARR(I)%DIM,K,IRTN)
	          WRITE(PAGE(N+K)(NC1+1:NC2),FMT) IND(J)
	        ENDDO
	        NC1=NC2
	      ENDDO
	      NC1=NC1+1
		    DO K=1,N1
	        PAGE(N+K)(NC1:NC1)=')'
	      ENDDO
	    ENDIF
	    NC1=NC1+1
		  DO K=1,N1
	      PAGE(N+K)(NC1:NC1)='='
	    ENDDO
	    NC2=0
	    IF(ITY.EQ.1) NC2=15
	    DO K=1,N1
	      IF(ITY.EQ.1) THEN
	        WRITE(PAGE(N+K)(NC1+1:),'(1PD15.7)') ARR(I)%VAL(K)
	      ELSE
	        IF(ARR(I)%LC(1,K).LE.ARR(I)%LC(2,K)) THEN
	          WRITE(PAGE(N+K)(NC1+1:),'(A)') 
     %             GSTR(ARR(I)%LC(1,K):ARR(I)%LC(2,K))
	          NC2=MAX(NC2,ARR(I)%LC(2,K)-ARR(I)%LC(1,K)+1)
	        ENDIF
	      ENDIF
	    ENDDO
	    NC1=NC1+NC2
	    WRITE(IFL,'(A)') (PAGE(K+N)(1:NC1),K=1,N1)
	    N=N+N1
	  ENDDO
	  IF(NCL.NE.0) DEALLOCATE(PAGE,STAT=ISTAT)
	ENDDO
	RETURN
900	WRITE(IFL,905)
905   FORMAT(' Warning: No array defined.')
	RETURN
910	WRITE(IFL,915)
915   FORMAT(' (SUBR.EVPRARRAY)  Allocation error')
	RETURN
	END
  
  
  
  