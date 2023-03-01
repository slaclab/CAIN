C  Functions for nonlinear Compton scat with linear laser polarization
      Subroutine functionA(n,Xi,alfaP1,alfaP2,AF)
c       Where i=0,1,2 is the function Ai, n is the nth order nonlinear Compton scattering
c          or the number photon be absorbed for each one electron 
c       An(i)=The integral:
c          Cos^i(phi)*exp{i[n*Phi-alfa1*Sin(Phi)+(alfa2/2)*Sin(2Phi)]}*D(Phi)/Pi 
c           The region of Phi is [0,Pi].
c       alfaP1=alfa1/Xi  and alfaP2=alfa2/Xi^2
      Implicit none
      real(8) PI/3.141592653589793238d0/, error/1.E-6/,err1
	real(8) cs(2)
	real(8) d,sum,c,s,aa,a,b,cs1,cs2,Xi,alfaP1,alfaP2
	real*8 Int1,Int2,AF(0:2)
	integer i,n,j,Npoint,irtn
      real*8 factor
        do j=0,2
	    AF(j)=0.0
	  enddo  

	if(Xi.le.20000.01) then
	   call functionA3(N,alfaP1*Xi,alfaP2*Xi**2,IRTN,AF)
!	   if(Xi.eq.0) print *,'Xi=0'
	   AF(0)=AF(0)/Xi
	   return
	else 

      err1=error
      
	do i=0,2
	if(i.eq.0) err1=err1*Min(Xi,1.0)
	   Npoint=4
	   d=Pi/Npoint
   
	   AF(i)=0.0
	   c=Cos(d)
	   s=sin(d)
		  cs(1)=1.0
	   cs(2)=0.0
	   aa=0.0
	   a=Xi*alfaP1
	   b=-0.5*Xi**2*alfaP2
         sum=0.0
	   do j=0,Npoint
	     factor=1.0
	     if(j.eq.0.or.j.eq.Npoint) factor=0.5
c       if(cs(1).lt.0.or.cs(2).lt.0) print *, 'cs(1), cs(2)',cs(1),cs(2) 
		 sum=sum+factor*(cs(1)**i)*cos((a+2*b*Cs(1))*cs(2)-n*j*d)
	     cs1=c*cs(1)-s*cs(2)
	     cs2=s*cs(1)+c*cs(2)
		    cs(1)=cs1
	  cs(2)=cs2
	   enddo
         Int2=sum*d/Pi 
            
100	   Int1=Int2
	   Npoint=2*Npoint 
	   d=d/2.0
	   c=Cos(2*d)
	   s=sin(2*d)
	   cs(1)=cos(d)
	   cs(2)=sin(d)
      	  do 120 j=1,Npoint/2
		    sum=sum+(cs(1)**i)*cos((a+2*b*Cs(1))*cs(2)-n*(2.*j-1.)*d)
		    cs1=c*cs(1)-s*cs(2)
		    cs2=s*cs(1)+c*cs(2)
	     cs(1)=cs1
		    cs(2)=cs2
120   	  continue
      	  Int2=Sum*d/Pi
	   if(Abs(Int1-Int2).gt.err1) goto 100 
	   AF(i)=Int2
		  if(i.eq.0) AF(i)=AF(i)/Xi
	 enddo
        endif
130	   return
     	 end

C---------------------------------------------------------------------------
	SUBROUTINE FunctionA3(N,c1,c2,IRTN,A)
c	   call functionA3(N,alfaP1*Xi,alfaP2*Xi**2,IRTN,AF)

C
C   A(s,n) = integral cos(phi)^s exp[i(n*phi-c1*sin(phi)+c2/2*sin(2*phi)] /(2*pi) dphi
C                               over  0<phi<2*pi,   n>=1
C      C2>0, NMAX>0
C   Bessel function expansion
	IMPLICIT NONE
	INTEGER N,IRTN
	REAL*8 A(0:2),C1,C2
	
	INTEGER M,MMAX,M1,J,ISTAT
	REAL*8 C11,C21,F1,F2
	REAL*8 EPS/1D-8/
	REAL*8, ALLOCATABLE:: JA1(:),JA2(:)
	
	DO 160 J=0,2
	  A(J)=0
160   CONTINUE
	IF(C2.EQ.0) THEN
	  IF(C1.NE.0) THEN
	    IRTN=10
	    RETURN
	  ENDIF
C             C1 must also be 0 in this case
	  IF(N.EQ.1) A(1)=0.5D0
	  IF(N.EQ.2) A(2)=0.25D0
	  RETURN
	ENDIF
	  
C  Find the truncation point of Bessel function series
	MMAX=(N+1)/2
	IF(C1.NE.0) THEN
	  C11=(C1/2)**2
	  C21=(C2/4)**2
	  F1=C11**(2*MMAX-N)*C21**MMAX
	  DO 200 M=1,MMAX
	    F1=F1/M
200     CONTINUE
220     IF(F1.GE.EPS) THEN
	    MMAX=MMAX+1
	    F1=F1*C11**2*C21/(2*MMAX-N)/MMAX
	    GOTO 220
	  ENDIF
	ENDIF
	
	ALLOCATE(JA2(0:MMAX), STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IRTN=1000
	  GOTO 500
	ENDIF
	CALL J0TON(C2/2,MMAX,JA2)
	IF(C1.EQ.0) THEN
	  IF(MOD(N,2).EQ.1) THEN
	    A(1)=0.5D0*(JA2((N-1)/2)-JA2((N+1)/2))
	    IF(MOD((N-1)/2,2).NE.0) A(1)=-A(1)
	  ENDIF
	  GOTO 500
	ENDIF

	ALLOCATE(JA1(0:2*MMAX+N), STAT=ISTAT)
	IF(ISTAT.NE.0) THEN
	  IRTN=1000
	  GOTO 500
	ENDIF
	CALL J0TON(C1,2*MMAX+N,JA1)
	
	DO 300 M=-MMAX,MMAX
	  M1=2*M+N
	  F1=JA1(ABS(M1))
	  IF(M1.LT.0.AND.MOD(M1,2).NE.0) F1=-F1
	  F2=JA2(ABS(M))
	  IF(M.LT.0.AND.MOD(M,2).NE.0) F2=-F2
	  A(0)=A(0)+F1*F2
	  A(1)=A(1)+M1/C1*F1*F2
	  A(2)=A(2)+(2*M+C2)/(2*C2)*F1*F2
300   CONTINUE
	IRTN=0

500	DEALLOCATE(JA1, STAT=ISTAT)
	DEALLOCATE(JA2, STAT=ISTAT)
	RETURN
	END

	SUBROUTINE CALA012X(A,N,C1,C2,IRTN)
C
C   A(s,n) = integral cos(phi)^s exp[i(n*phi-c1*sin(phi)+c2/2*sin(2*phi)] /(2*pi) dphi
C                               over  0<phi<2*pi,   n>=1
C      C2>0, NMAX>0
C  Numerical integration
	IMPLICIT NONE
	INTEGER N,IRTN
	REAL*8 A(0:2),C1,C2
	INTEGER ND,II,I,I0,J
	REAL*8 A0(0:2),DPHI,CO0,SI0,CO1,SI1,CO2,AA,ER,F1
	REAL*8 PI/3.141592653589793238D0/
	INTEGER MSG/0/

	ND=8
	II=1
	I0=0
	DO 100 J=0,2
	  A(J)=0
100   CONTINUE
200	DPHI=PI/ND
	DO 220 J=0,2
	  A0(J)=A(J)
	  A(J)=0
220   CONTINUE
	CO0=COS(I0*DPHI)
	SI0=SIN(I0*DPHI)
	CO1=COS(II*DPHI)
	SI1=SIN(II*DPHI)
	DO 260 I=I0,ND,II
	  F1=COS(N*I*DPHI-SI0*(C1-C2*CO0))
	  IF(I.EQ.0.OR.I.EQ.ND) F1=0.5D0*F1
	  CO2=1
	  DO 240 J=0,2
	    A(J)=A(J)+CO2*F1
	    CO2=CO2*CO0
240     CONTINUE
	  CO2=CO0
	  CO0=CO2*CO1-SI0*SI1
	  SI0=SI0*CO1+CO2*SI1
260   CONTINUE
	ER=0
	AA=0
	DO 280 J=0,2
	  A(J)=A(J)*II/ND
	  IF(I0.NE.0) A(J)=(A(J)+A0(J))/2
	  ER=ER+ABS(A(J)-A0(J))
	  AA=AA+ABS(A(J))
280   CONTINUE
	ER=ER/AA
	IF(MSG.GT.0) THEN
	  WRITE(6,300) ND,(A(J),J=0,2),ER
300     FORMAT(' ND=',I6,'  A=',1P3D17.10,'  ER=',1PD10.3)
	ENDIF
	IF(I0.EQ.0.OR.ER.GE.1D-8) THEN
	  I0=1
	  II=2
	  ND=ND*2
	  IF(ND.GT.1024) GOTO 900
	  GOTO 200
	ENDIF
	IRTN=0
	RETURN
900   IRTN=10
	WRITE(6,910)
910   FORMAT(' (SUBR.CALA012X) Numerical integration failed.')
	RETURN
	END
