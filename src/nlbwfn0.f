      SUBROUTINE NLBWFN0(LP,NPH,XISQ,Q,X,G)
      IMPLICIT NONE
      INTEGER LP,NPH
      REAL*8 XISQ,Q,X,G(5)
      REAL*8 UN,U,U1,Z1,Z,BJ,DBJ,B0,B1,B2,B3
C
      G(1)=0
      G(3)=0
      IF(LP.NE.0) THEN
        G(2)=0
        G(4)=0
        G(5)=0
      ENDIF
      IF(XISQ.LT.0.OR.Q.LE.0) RETURN
      IF(X.LE.0.OR.X.GE.1) RETURN
      UN=NPH*Q
      IF(UN.LE.1) RETURN
      U=0.25D0/(X*(1-X))
      U1=U/UN
      IF(U1.LT.0.OR.U1.GT.1) RETURN
      Z1=(2*NPH)**2*U1*MAX(0D0,1-U1)/(1+XISQ)
      Z=SQRT(XISQ*Z1)
      CALL BESJDJ(NPH,Z,BJ,DBJ)
      IF(Z.EQ.0) THEN
        B0=0
        IF(NPH.EQ.1) B0=0.5D0
      ELSE
        B0=BJ/Z
      ENDIF
      B1=B0**2*Z1
      B2=2*(DBJ**2-(Z**2-NPH**2)*B0**2)
      B3=4*NPH*B0*DBJ
C    B0=J(n,z)/z
C    B1=J(n,z)**2/Xi**2
C    B2=J(n-1,z)**2+J(n+1,z)**2-2*J(n,z)**2
C    B3=J(n-1,z)**2-J(n+1,z)**2
      G(1)=B1+(U-0.5D0)*B2
      G(3)=-(U-0.5D0)*(1-2*U1)*B3
      IF(LP.GE.1) THEN
        G(2)=(1-2*X)*U*(1-2*U1)*B3
        G(4)=B1/X-U*(1-2*X)*B2
        G(5)=B1/(1-X)+U*(1-2*X)*B2
      ENDIF
      RETURN
      END
