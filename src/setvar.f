      SUBROUTINE SETVAR(N,IRTN)
C Set particle parameters into EVAL parameter list.
	USE BEAMCM
      IMPLICIT NONE
      INTEGER N,IRTN
C      INCLUDE 'include/beamcm.h'
      INCLUDE 'include/ctrlcm3.h'
      INTEGER I
      REAL*8 RSVP(MRSVP4)
	CHARACTER(80) ERR
C
      IF(N.LE.0.OR.N.GT.NP) THEN
        IRTN=1000
        RETURN
      ENDIF
      DO 200 I=0,3
        RSVP(I+IPTXYS)=TXYS(I,N)
        RSVP(I+IPEP)=EP(I,N)
 200  CONTINUE
      IF(KIND(N).EQ.1) THEN
        DO 220 I=1,3
          RSVP(I+IPSXYS-1)=0
          RSVP(I+IPX123-1)=SPIN(I,N)
 220    CONTINUE
      ELSE
        DO 240 I=1,3
          RSVP(I+IPSXYS-1)=SPIN(I,N)
          RSVP(I+IPX123-1)=0
 240    CONTINUE
      ENDIF
      RSVP(IPKIND)=KIND(N)
      RSVP(IPGEN)=GEN(N)
	RSVP(IPWGT)=WGT(N)
	IF(PNAME(N)(1:1).EQ.'I') THEN
	  RSVP(IPINCP)=1
	ELSE
	  RSVP(IPINCP)=0
	ENDIF
      CALL EVDEFP2(IPINCP,IDRSVP,RSVP)
C        IPINCP is the last item of the particle variables
      CALL EVARRCSET(IDRSVP(IPPNAME),0,1,PNAME(N),ERR)
      IRTN=0
      RETURN
      END

