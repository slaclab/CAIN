C******************** J0TON ***********************************                 
      SUBROUTINE J0TON(X,N0,F)                                                  
C  REAL*8 BESSEL FUNCTIONS OF INTEGER ORDER FROM J(0,X) TO J(N0,X).             
C  IF N0>=0, F(1)=J(0,X),F(2)=J(1,X),....,F(N0+1)=J(N0,X)                       
C  IF N0< 0, F(1)=J(0,X),F(2)=J(-1,X),....,F(-N0+1)=J(N0,X)                     
C  DIMENSION OF F MUST BE >=ABS(N0)+1                                           
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      COMMON/LIBCOM/LIBRTN,LIBMSGLVL                                                 
      DIMENSION F(*)                                                            
      REAL*4 XA                                                                 
      REAL*8 X1/1D-77/
      REAL*4 X2/2E-5/,X3/1E0/,X4/1E1/,X5/1E2/,X6/3E4/                 
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/,                             
     %     T0/1D-75/,UNDR1/1D-55/,UNDR2/1D-22/,OVER/1D55/                       
      LOGICAL*1 ON  
	INCLUDE 'include/ctrlcm.h'                                                          
C                                                                               
C      CALL OVERFL(KJ)                                                           
      N=ABS(N0)  
      N1=N+1  
      XA=ABS(X)                                        
      IF(N.GE.30000) GO TO 900                                                  
      IF(XA.GE.X4) GO TO 10                                                     
      IF(XA.GE.X3) GO TO 70                                                     
      IF(XA.GE.X2) GO TO 60                                                     
      GO TO 20                                                                  
   10 IF(XA.LT.X5) GO TO 80                                                     
      IF(XA.GE.X6) GO TO 920                                                    
      GO TO 90                                                                  
C------- TAYLOR EXPANSION FOR SMALL X -------                                   
   20 IF(XA.GE.X1) GO TO 40                                                     
      F(1)=ONE                                                                  
      IF(N.EQ.0) GO TO 200                                                      
      DO 30 I=2,N1                                                              
   30 F(I)=ZERO                                                                 
      GO TO 200                                                                 
   40 T1=HALF*X                                                                 
      T2=T1*T1                                                                  
      F(1)=ONE-T2                                                               
      IF(N.EQ.0) GO TO 200                                                      
      T3=ONE  
      T4=ONE                                                         
      DO 50 I=2,N1                                                              
      T4=T4*T1/T3                                                               
      T3=T3+ONE                                                                 
      F(I)=T4*(ONE-T2/T3)                                                       
   50 CONTINUE                                                                  
C------- SELECT THE STARTING POINT -------                                      
   60 L=15                                                                      
      GO TO 100                                                                 
   70 L=INT(1.4*XA)+15                                                          
      GO TO 100                                                                 
   80 L=INT(0.27*XA)+28                                                         
      GO TO 100                                                                 
   90 L=INT(0.073*XA)+48                                                        
      GO TO 100                                                                 
  100 NM=MAX(N,INT(XA))+L                                                       
C------- START RECURSION FORMULA -------                                        
      Z=TWO/X                                                                   
      T3=ZERO  
      T2=T0  
      S=ZERO  
      ON=.FALSE.                                  
      DO 140 I=1,NM                                                             
      K=NM-I+1                                                                  
      TSAVE=T2                                                                  
      T2=DFLOAT(K)*Z*T2-T3                                                      
      T3=TSAVE                                                                  
      IF(ON) GO TO 110                                                         
      IF(K.GT.N1) GO TO 120                                                     
      ON=.TRUE.                                                                   
  110 F(K)=T2                                                                   
  120 IF(MOD(K,2).EQ.0) GO TO 140                                               
      S=S+T2                                                                    
      IF(ABS(S).LT.OVER) GO TO 140                                              
      S=S*UNDR1  
      T2=T2*UNDR1  
      T3=T3*UNDR1                                 
      IF(.NOT.ON) GO TO 140                                                    
      DO 130 J=K,N1                                                             
      IF(ABS(F(J)).LT.UNDR2) GO TO 125                                          
      F(J)=F(J)*UNDR1                                                           
      GO TO 130                                                                 
  125 F(J)=ZERO                                                                 
  130 CONTINUE                                                                  
  140 CONTINUE                                                                  
C------- ADJUST THE OVER-ALL FACTOR -------                                     
      S=ONE/(S+S-T2)                                                            
      DO 150 J=1,N1                                                             
  150 F(J)=F(J)*S                                                               
      IF(N0.GE.0) GO TO 200                                                     
      DO 160 J=2,N1,2                                                           
  160 F(J)=-F(J)                                                                
  200 RETURN                                                                    
C------- ERROR MESSAGE -------                                                  
  900 IF(LIBMSGLVL.GE.0) WRITE(MSGFL,910) N0                                           
  910 FORMAT(' (SUBR.J0TON) TOO LARGE ORDER ',I8,'  NO CALCULATION')            
      GO TO 950                                                                 
  920 IF(LIBMSGLVL.GE.0) WRITE(MSGFL,930) X                                            
  930 FORMAT(' (SUBR.J0TON) TOO LARGE ARGUMENT ',D15.7,                         
     %       '  NO CALCULATION.')                                               
  950 LIBRTN=1000                                                                 
      RETURN                                                                    
      END                                                                       
