
         Function ScalarP4(A,B)
             Implicit Double Precision (A-H,k,L,O-Z)
             real(8) A(0:3),B(0:3)
	       ScalarP4=A(0)*B(0)
	       do j=1,3
	       ScalarP4=ScalarP4-A(j)*B(j)
	       enddo
	     return
	   end


