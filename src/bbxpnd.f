      SUBROUTINE BBXPND(L,MX,MY,Q,LVEC)
	USE BBPKCM
      IMPLICIT NONE
C      INCLUDE 'include/bbpkcm.h'
      INTEGER L,MX,MY,LVEC
      REAL*8 Q(MX,MY,2)
      INTEGER J
      INTEGER NCALL/0/
C
      NCALL=NCALL+1
      CALL CPUTIM('BBXPND',1)
      IF(NMOM.GE.0) THEN
c        IF(LVEC.EQ.0) THEN
          CALL BBQMOM(L,MX,MY,Q(1,1,L))
c        ELSE
c          CALL BBQMMV(L,MX,MY,Q(1,1,L))
c        ENDIF
      ELSE
        DO 360 J=1,2
          XYCM(J,L)=XYMIN(J,L)+NXY(J)*DXY(J,L)*0.5D0
 360    CONTINUE
      ENDIF
      CALL CPUTIM('BBXPND',2)
      RETURN
      END
