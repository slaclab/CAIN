C------------------ EVREDC ------------------------------------------
      RECURSIVE SUBROUTINE EVREDC(N,X,BOP,J,ERR)
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
	USE FLCHTYP
	USE ARRAYMOD
      IMPLICIT NONE
      INTEGER N,BOP(N),J,K
      CHARACTER*(*) ERR
      TYPE(FLCHTYPE) X(0:N)
	CHARACTER(2) LOP(0:13)/'  ','+','-','*','/','^','==','/=',
     %   '<','>','<=','>=','&&','||'/
      INTEGER POP(0:13)/0,5,5,7,7,8, 3, 3, 4, 4, 4, 4, 2, 1/
C                         + - * / ^ == /=  <  > <= >= && ||
C      POP: priority of binary operators.
C
      ERR=' '
 1    IF(N.EQ.0) RETURN
	     
      IF(POP(BOP(N)).GE.POP(J)) THEN
        GOTO (10,20,30,40,50,60,70,80,90,100,110,120,130), BOP(N)
C              +  -  *  /  ^ == /=  <  >  <=  >=  &&  ||
 10     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          X(N-1)%X=X(N-1)%X+X(N)%X
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.2) THEN
	    CALL FLCHSET2(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2))//
     %                  GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2)),X(N-1),ERR)
	    IF(ERR.NE.' ') RETURN
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 20     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          X(N-1)%X=X(N-1)%X-X(N)%X
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 30     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          X(N-1)%X=X(N-1)%X*X(N)%X
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.1) THEN
	    CALL FLCHSETN(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2)),
     %           NINT(X(N)%X),X(N-1),ERR)
	    IF(ERR.NE.' ') RETURN
	  ELSEIF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.2) THEN
	    CALL FLCHSETN(GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2)),
     %           NINT(X(N-1)%X),X(N-1),ERR)
	    IF(ERR.NE.' ') RETURN
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 40     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N)%X.EQ.0) THEN
            ERR='Zero divide.   (EVREDC)'
            RETURN
          ENDIF
          X(N-1)%X=X(N-1)%X/X(N)%X
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 50     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N)%X.EQ.0) THEN
            X(N-1)%X=1
          ELSEIF(X(N-1)%X.EQ.0) THEN
            IF(X(N)%X.LT.0) THEN
              ERR='Negative power of zero.   (EVREDC)'
              RETURN
            ENDIF
          ELSEIF(X(N-1)%X.LT.0) THEN
            K=NINT(X(N)%X)
            IF(ABS(DFLOAT(K)-X(N)%X).LE.1D-12) THEN
              X(N-1)%X=X(N-1)%X**K
            ELSE
              ERR='Fractional power of negative number.   (EVREDC)'
              RETURN
            ENDIF
          ELSE
            X(N-1)%X=X(N-1)%X**X(N)%X
          ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 60     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.EQ.X(N)%X) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.2) THEN
	    X(N-1)%L=1
	    IF(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2)).EQ.
     %       GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2))) THEN
	      X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 70     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.NE.X(N)%X) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.2) THEN
	    X(N-1)%L=1
	    IF(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2)).NE.
     %       GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2))) THEN
	      X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 80     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.LT.X(N)%X) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.2) THEN
	    X(N-1)%L=1
	    IF(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2)).LT.
     %       GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2))) THEN
	      X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
 90     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.GT.X(N)%X) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.2) THEN
	    X(N-1)%L=1
	    IF(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2)).GT.
     %       GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2))) THEN
	      X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
100     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.LE.X(N)%X) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.2) THEN
	    X(N-1)%L=1
	    IF(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2)).LE.
     %       GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2))) THEN
	      X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
110     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.GE.X(N)%X) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSEIF(X(N-1)%L.EQ.2.AND.X(N)%L.EQ.2) THEN
	    X(N-1)%L=1
	    IF(GSTR2(EVALNEST)(X(N-1)%C(1):X(N-1)%C(2)).GE.
     %       GSTR2(EVALNEST)(X(N)%C(1):X(N)%C(2))) THEN
	      X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
120     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.NE.0.AND.X(N)%X.NE.0) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
130     IF(X(N-1)%L.EQ.1.AND.X(N)%L.EQ.1) THEN
          IF(X(N-1)%X.NE.0.OR.X(N)%X.NE.0) THEN
            X(N-1)%X=1
	    ELSE
	      X(N-1)%X=0
	    ENDIF
	  ELSE
	    GOTO 900
	  ENDIF
        GOTO 200
      ELSE
        RETURN
      ENDIF
200   N=N-1
      GOTO 1
900   ERR='Invalid float/char operation for '//LOP(BOP(N))
      RETURN
      END
