      SUBROUTINE RDHEAD(LN,LINE,NCHAR,IRTN)
	USE FLCHTYP
	USE READMOD
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER LN(2,3),NCHAR(*),IRTN
      CHARACTER*(*) LINE(*)
      INCLUDE 'include/ctrlcm.h'
C      INCLUDE 'include/readcm.h'
      INCLUDE 'include/headcm.h'
      INTEGER LNX(2,2,2),REL1,NC,NCTTL,L1,N1,L2,N2,L3,N3,IRTN1,LONG
	TYPE(FLCHTYPE) FC
	CHARACTER(80) ERR
C
      JOBTTL=' '
      L1=LN(1,2)
      N1=LN(2,2)
      L3=LN(1,3)
      N3=LN(2,3)
      NCTTL=0
      LONG=0
      DO WHILE(L1.NE.0)
        CALL NXTBLK(L1,N1,L3,N3,LINE,NCHAR,LNX,REL1,L2,N2,IRTN1)
        IF(IRTN1.NE.0) GOTO 800
        IF(REL1.NE.0) GOTO 900
        CALL BLKREC(LNX,LINE,NCHAR,TEXT,NC,IRTN1,MSGFL)
        IF(NC.GE.1) THEN
	    CALL EVAL0(TEXT(1:NC),FC,ERR)
	    IF(ERR.NE.' '.OR.FC%L.NE.2) GOTO 900
	    NC=FC%C(2)-FC%C(1)+1
          IF(NC.GT.MCTTL0-NCTTL) THEN
            IF(MSGLVL.GE.0.AND.LONG.EQ.0) WRITE(MSGFL,200)
 200        FORMAT(' ***Warning: Header string too long.',
     %             ' Truncated.')
            NC=MCTTL0-NCTTL
            LONG=1
          ENDIF
          IF(NC.GE.1) THEN
            JOBTTL(NCTTL+1:NCTTL+NC)=GSTR2(EVALLAST)(FC%C(1):FC%C(2))
            NCTTL=NCTTL+NC
          ENDIF
        ENDIF
        L1=L2
        N1=N2
      ENDDO
 800  IRTN=0
      RETURN
 900  IRTN=100
      IF(MSGLVL.GE.0) WRITE(MSGFL,910)
 910  FORMAT(' (SUBR.RDHEAD) Illegal header string.')
      RETURN
C 990  IRTN=101
C      RETURN
      END

