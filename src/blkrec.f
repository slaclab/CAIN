      SUBROUTINE BLKREC(LN,LINE,NCHAR,TEXT,NC,IRTN,MSGFL)
C  Construct a character string given the start LN(*,1) and end LN(*,2)
C  line-columns. Eliminate preceeding and trailing blanck
C  characters.
C    IRTN=0 : normal.  string TEXT(1:NC)
C      1000 : length > length of TEXT
      IMPLICIT NONE
      INTEGER LN(2,2),NCHAR(*),NC,IRTN,MSGFL
      CHARACTER*(*) LINE(*),TEXT
      INTEGER M,L0,N0,L1,N1,L2,N2,L3,N3,L,N
      CHARACTER*1 CH
C
      L0=LN(1,1)
      N0=LN(2,1)
      L3=LN(1,2)
      N3=LN(2,2)
      L=L0
      N=N0
 200  CH=LINE(L)(N:N)
      IF(CH.NE.' ') THEN
        L1=L
        N1=N
        GOTO 220
      ENDIF
      CALL NXTPOS(L,N,L3,N3,NCHAR,IRTN)
      IF(IRTN.EQ.0) GOTO 200
      NC=0
      GOTO 300
 220  L=L3
      N=N3
 240  CH=LINE(L)(N:N)
      IF(CH.NE.' ') THEN
        L2=L
        N2=N
        GOTO 260
      ENDIF
      CALL PRVPOS(L,N,L0,N0,NCHAR,IRTN)
      GOTO 240
 260  M=LEN(TEXT)
      L=L1
      N=N1
      NC=0
 280  NC=NC+1
      TEXT(NC:NC)=LINE(L)(N:N)
      IF(L.LT.L2.OR.(L.EQ.L2.AND.N.LT.N2)) THEN
        IF(NC.GE.M) GOTO 900
        CALL NXTPOS(L,N,L2,N2,NCHAR,IRTN)
        GOTO 280
      ENDIF
 300  IRTN=0
      RETURN
 900  IRTN=1000
      WRITE(MSGFL,905)
 905  FORMAT(' (SUBR.BLKREC) Following block is too long.')
      CALL PRECHO(L0,N0,L3,N3,LINE,NCHAR)
      RETURN
      END
