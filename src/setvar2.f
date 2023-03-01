      SUBROUTINE SETVAR2(KN,GN,TXYS,EP,SPIN,WGT,PNAM)
C Set particle parameters into EVAL parameter list.
      IMPLICIT NONE
      INTEGER KN,GN
      REAL*8 TXYS(0:3),EP(0:3),SPIN(3),WGT
	CHARACTER(*) PNAM
      INCLUDE 'include/ctrlcm3.h'
      INTEGER I
      REAL*8 RSVP(MRSVP4)
	CHARACTER(80) ERR
C
      DO 200 I=0,3
        RSVP(I+IPTXYS)=TXYS(I)
        RSVP(I+IPEP)=EP(I)
 200  CONTINUE
      IF(KN.EQ.1) THEN
	  DO 220 I=1,3
          RSVP(I+IPSXYS-1)=0
          RSVP(I+IPX123-1)=SPIN(I)
 220    CONTINUE
      ELSE
        DO 240 I=1,3
          RSVP(I+IPSXYS-1)=SPIN(I)
          RSVP(I+IPX123-1)=0
 240    CONTINUE
      ENDIF
      RSVP(IPKIND)=KN
      RSVP(IPGEN)=GN
	RSVP(IPWGT)=WGT
	IF(PNAM(1:1).EQ.'I') THEN
	  RSVP(IPINCP)=1
	ELSE
	  RSVP(IPINCP)=0
	ENDIF
      CALL EVDEFP2(IPINCP,IDRSVP,RSVP)
C        IPINCP is the last item of the particle variables
	CALL EVARRCSET(IDRSVP(IPPNAME),0,1,PNAM,ERR)
      RETURN
      END

