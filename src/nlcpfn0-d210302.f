      SUBROUTINE NLCPFN0(LP,NPH,XISQ,LAMBDA,X,F)
      IMPLICIT NONE
      INTEGER LP,NPH
      REAL*8 XISQ,LAMBDA,X,F(5)
      REAL*8 V,VN,Y,Z1,Z,BJ,DBJ,B0,B1,B2,B3
C
      V=X/(1-X)
      VN=NPH*LAMBDA/(1+XISQ)
      Y=V/VN
      Z1=(2*NPH)**2*Y*(1-Y)/(1+XISQ)
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
      F(1)=-B1+0.5D0*(1+V*X/2)*B2
      F(2)=(0.5D0-Y)*X*(1+V/2)*B3
      IF(LP.NE.0) THEN
        F(3)=(0.5D0-Y)*(1+V*X/2)*B3
        F(4)=X*(-B1+0.5D0*(1+V/2)*B2)
        F(5)=-B1*V*X
      ENDIF
      RETURN
      END
