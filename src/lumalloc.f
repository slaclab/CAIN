	SUBROUTINE LUMALLOC(MLUMMESH1,IRTN)
	USE LUMCOM
	IMPLICIT NONE
	INTEGER MLUMMESH1,IRTN

	INTEGER,PARAMETER:: MEX=11
	INTEGER I,M,N

	N=2
	M=MEX
	DO I=1,MEX-1
	  M=I
	  IF(N.GE.MLUMMESH1) EXIT
	  N=2*N
	ENDDO
	MLUMMESH1=N
	MMM=M
	MDD=2**MMM
	MMDIM=(4*MDD**2-1)/3
	
	ALLOCATE(NDD(0:MMM),NDD2(0:MMM),IPDD(0:MMM),KIJDD(0:MDD-1),
     %    IPBN(0:MDD**2-1,3,2),IPBNV(0:MDD**2-1,2),
     %    DIST(0:MMDIM-1,3,2),VDIST(0:MMDIM-1,3,2), STAT=IRTN)

	CALL LUMINI(0)

      RETURN
	END
      
	SUBROUTINE LUMDEALLOC
	USE LUMCOM
	IMPLICIT NONE
	INTEGER ISTAT

	DEALLOCATE(NDD,NDD2,IPDD,KIJDD,IPBN,IPBNV,DIST,VDIST,STAT=ISTAT)
	RETURN
	END
