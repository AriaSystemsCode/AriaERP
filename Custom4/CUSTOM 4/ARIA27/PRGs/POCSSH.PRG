*E301085,1 WAM 12/07/98 Generate cost sheet automatically

*PARAMETERS lcPoNo
*DO (gcAppHome+'MFCSSH') WITH 'I',IIF(TYPE('lcPoNo')='C',lcPoNo,'')
PARAMETERS lcPoNo,llAutoGen
DO (gcAppHome+'MFCSSH') WITH 'I',IIF(TYPE('lcPoNo')='C',lcPoNo,''),llAutoGen