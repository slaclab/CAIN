      SUBROUTINE JOBDAT(JOBTIM)
C  Date and time of job start by 20byte string
      CHARACTER*20 JOBTIM
	CHARACTER*10 TIM

	CALL DATE_AND_TIME(JOBTIM(1:8),TIM)
	JOBTIM(9:9)='('
	JOBTIM(10:15)=TIM(1:6)
	JOBTIM(16:20)=')    '
c      CALL TIME(JOBTIM(1:8))
c      JOBTIM(9:9)='('
c      CALL DATE(JOBTIM(10:18))
c      JOBTIM(19:20)=') '
      RETURN
      END
  
