C**********************************************************************
      SUBROUTINE LNBWPE(GA,E,V,P1,P2,P3,P4,PE1,PE2,PE3,PE4,MSG)
      IMPLICIT NONE
      INTEGER MSG
      REAL*8  GA,E,V(3),P1(3),P2(3),P3(3),P4(3),
     %   PE1(3),PE2(3),PE3(3),PE4(3) 
      INTEGER I
      REAL*8  GA2,E3,GA3,RHO3(3),RHO31,RHO32,RHO33,RHO34,RHO35(3)
      REAL*8  E4,GA4,RHO4(3),RHO41,RHO42,RHO43,RHO44,RHO45(3)
      REAL*8  M/0.510999062D+6/
      GA2=E/M
      E3=SQRT(P3(1)**2+P3(2)**2+P3(3)**2+M**2)
      GA3=E3/M
      IF(MSG.GE.2) PRINT *,'GA2.GA3=',GA2,GA3
 
      RHO3(1)=GA*(V(2)*P1(3)-V(3)*P1(2))/M
      RHO3(2)=GA*(V(3)*P1(1)-V(1)*P1(3))/M
      RHO3(3)=GA*(V(1)*P1(2)-V(2)*P1(1))/M
      RHO31=(1+GA)*(1+GA3)*(1+GA2)
      RHO32=RHO3(1)**2+RHO3(2)**2+RHO3(3)**2
      RHO33=RHO3(1)*PE1(1)+RHO3(2)*PE1(2)+RHO3(3)*PE1(3)
      RHO34=(1+GA+GA3+GA2)
      RHO35(1)=PE1(2)*RHO3(3)-PE1(3)*RHO3(2)
      RHO35(2)=PE1(3)*RHO3(1)-PE1(1)*RHO3(3)
      RHO35(3)=PE1(1)*RHO3(2)-PE1(2)*RHO3(1)
 
      E4=SQRT(P4(1)**2+P4(2)**2+P4(3)**2+M**2)
      GA4=E4/M
      IF(MSG.GE.2) PRINT *,'GA4=',GA4
 
      RHO4(1)=GA*(V(2)*P2(3)-V(3)*P2(2))/M
      RHO4(2)=GA*(V(3)*P2(1)-V(1)*P2(3))/M
      RHO4(3)=GA*(V(1)*P2(2)-V(2)*P2(1))/M
      RHO41=(1+GA)*(1+GA4)*(1+GA2)
      RHO42=RHO4(1)**2+RHO4(2)**2+RHO4(3)**2
      RHO43=RHO4(1)*PE2(1)+RHO4(2)*PE2(2)+RHO4(3)*PE2(3)
      RHO44=(1+GA+GA4+GA2)
      RHO45(1)=PE2(2)*RHO4(3)-PE2(3)*RHO4(2)
      RHO45(2)=PE2(3)*RHO4(1)-PE2(1)*RHO4(3)
      RHO45(3)=PE2(1)*RHO4(2)-PE2(2)*RHO4(1)
 
      DO 110 I=1,3
         PE3(I)=((RHO31-RHO32)*PE1(I)+RHO3(I)*RHO33+RHO34*RHO35(I))
     %            /RHO31
         PE4(I)=((RHO41-RHO42)*PE2(I)+RHO4(I)*RHO43+RHO44*RHO45(I))
     %            /RHO41
         IF(MSG.GE.2) PRINT *,'PE3=',PE3(I)
         IF(MSG.GE.2) PRINT *,'PE4=',PE4(I)
 110  CONTINUE   
      RETURN
      END
