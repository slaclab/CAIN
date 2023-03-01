C   24/05/93 305241356  MEMBER NAME  GAMMA    *.FORT        E2FORT              
C******************** DGAMMA ***********************************                
      FUNCTION DGAMMA(X)                                                        
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      REAL*8 BB(16)/                                                            
     1   0.152382215394074161922833649588868D+08,                               
     2   0.691472268851313067108395250775672D+06,                               
     3   0.361087712537249893571732652192422D+05,                               
     4   0.219310333333333333333333333333333D+04,                               
     5   0.156848284626002017306365132452089D+03,                               
     6   0.134028640441683919944789510006901D+02,                               
     7   0.139243221690590111642743221690590D+01,                               
     8   0.179644372368830573164938490015889D+00,                               
     9   0.295506535947712418300653594771242D-01,                               
     O   0.641025641025640944306005521966553D-02,                               
     1   0.191752691752691752691752691752692D-02,                               
     2   0.841750841750841750841750841750841D-03,                               
     3   0.595238095238095238095238095238095D-03,                               
     4   0.79365079365079365079365079365079365D-03,                             
     5   0.27777777777777777777777777777777778D-02,                             
     6   0.83333333333333333333333333333333333D-01/                             
C        BB(17-N)=B(N)/(2*N*(2*N-1))  (B(N)=BERNOULLI NUMBER)                   
      DATA XMAX/57.5744D0/,                                                     
     %   PI/3.14159 26535 89793 23846 26433 83279 5028D0/,                      
     %     CC/0.918938533204672741780329736405536D+00/ 
	INCLUDE 'include/ctrlcm.h'                         
C                                                                               
      XX=ABS(X)                                                                 
      IF(XX.GE.XMAX) GOTO 900                                                   
      NX=NINT(XX)                                                               
      FR=XX-DFLOAT(NX)                                                          
      IF(ABS(FR).LE.1D-30) GOTO 500                                             
      X1=XX                                                                     
      IF(NX.LE.21) X1=22D0+FR                                                   
      Y=-1D0/X1**2                                                              
      S=0D0                                                                     
      DO 100 I=1,16                                                             
  100 S=BB(I)+S*Y                                                               
      S=(X1-0.5D0)*LOG(X1)-X1+CC+S/X1                                           
      DGAMMA=EXP(S)                                                             
      IF(NX.GE.22) GOTO 140                                                     
      S=1D0                                                                     
      DO 120 I=0,21-NX                                                          
  120 S=S*(DFLOAT(I)+XX)                                                        
      DGAMMA=DGAMMA/S                                                           
  140 IF(X.GE.0D0) RETURN                                                       
      S=SIN(PI*X)                                                               
      IF(ABS(S).LE.1D-14) GOTO 920                                              
      IF(ABS(S).LE.1D-9) WRITE(MSGFL,180) X                                         
  180 FORMAT(' (FUNC.DGAMMA) X=',1PD25.18,'   CLOSE TO AN ',                    
     %  'NON-POSITIVE INTEGER.  DGAMMA MAY NOT BE ACCURATE.')                   
      DGAMMA=-PI/(X*S*DGAMMA)                                                   
      RETURN                                                                    
  500 IF(X.LE.0D0.OR.NX.EQ.0) GOTO 920                                          
      DGAMMA=1D0                                                                
      IF(NX.EQ.1) RETURN                                                        
      DO 520 I=1,NX-1                                                           
  520 DGAMMA=DGAMMA*DFLOAT(I)                                                   
      RETURN                                                                    
  900 WRITE(MSGFL,910) X,XMAX                                                       
  910 FORMAT(' (FUNC.DGAMMA) X=',1PD25.18,'   ABS(X) MUST BE ',                 
     % '<',0PF7.4,'.')                                                          
      IF(X.GE.0D0) DGAMMA=1D75                                                  
      IF(X.LE.0D0) DGAMMA=0.0                                                   
      RETURN                                                                    
  920 WRITE(MSGFL,930) X                                                            
  930 FORMAT(' (FUNC.DGAMMA) X=',1PD25.18,'  TOO CLOSE TO AN ',                 
     %  'NON-POSITIVE INTEGER.')                                                
      DGAMMA=1D75                                                               
      RETURN                                                                    
      END                                                                       
