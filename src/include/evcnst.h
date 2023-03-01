      INTEGER MPAR0
      PARAMETER (MPAR0=3)
      CHARACTER*(MCHAR) NAMPAR0(MPAR0)/'Pi','E','Euler'/
C   MCHAR defined in nameleng.h
C   MFUN defined in funlis.h
      REAL*8 VPAR0(MPAR0)/
     %  3.14159 26535 89793 238D0, 2.71828 18284 59045 235D0,
     %  0.57721 56649 01532 8606D0/
C  Pre-defined function list
      CHARACTER*(MCHAR) NAMFUN0(MFUN)/
     %  '+','-','Int','Nint','Sgn','Step','Abs',
     %  'Frac','Sqrt','Exp','Log','Log10',
     %  'Cos','Sin','Tan','ArcCos','ArcSin','ArcTan',
     %  'Cosh','Sinh','Tanh','ArcCosh','ArcSinh','ArcTanh',
     %  'Gamma',
     %  'Mod','Atan2','Min','Max'/
C         If you change the above list, change also setcnst.f in Cain
      INTEGER NVFUN0(MFUN)
      DATA NVFUN0/
     %   25*1,
     %   2,2,1000,1000/
C  Keep in mind to change evload.f if you change this table,
C  you have also to change evfun.f and evload.f.
