C------------------ EVREDCC ------------------------------------------
      SUBROUTINE EVREDCC(N,N00,MP,IP,PL,BOP,J,ERR)
C  Compile version of EVREDC
C  Evaluate the expression
C   X(0)  BOP(1)  X(1)  BOP(2) .... BOP(N)  X(N)
C  as far as possible from the right end,
C  where X's are numbers and BOP's are binary operators (+-*/^).
C  Next to the right end, the operator J exists so that, if the
C  priority of J is higher than that of BOP(N), no calculation
C  can be done.  If J=0, no operator is assumed at the end.
C  Note that the priority order
C   POP(BOP(1)) < POP(BOP(2)) < .... < POP(BOP(N))
C  is assumed, which is ensured by the algorithm of the calling
C  routine EVAL0.
      IMPLICIT NONE
      INTEGER N,N00,MP,IP,PL(3,MP),BOP(N),J
      CHARACTER*(*) ERR
      INTEGER POP(0:13)/0,5,5,7,7,8, 3, 3, 4, 4, 4, 4, 2, 1/
C                         + - * / ^ == /=  <  > <= >= && ||
C      POP: priority of binary operators.
C
      ERR=' '
 1    IF(N.EQ.0) RETURN
      IF(POP(BOP(N)).GE.POP(J)) THEN
        IF(IP.GE.MP) THEN
          ERR='(EVREDCC) Compile buffer full.'
          RETURN
        ENDIF
        IP=IP+1
	  IF(BOP(N).LE.5) THEN
          PL(1,IP)=BOP(N)+3
	  ELSE
	    PL(1,IP)=BOP(N)+4
	  ENDIF
        PL(2,IP)=N-1+N00
        PL(3,IP)=N+N00
      ELSE
        RETURN
      ENDIF
      N=N-1
      GOTO 1
      END
