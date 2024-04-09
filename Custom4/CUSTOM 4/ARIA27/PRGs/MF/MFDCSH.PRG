*:************************************************************************
*: Program file  : MFDCSH.PRG
*: Program desc. : Dye Cost Sheet
*: For screen    : MFCSSH.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Manufactoring (MF).
*:      Developer: AMM - Ahmed Mohamed Ibrahim
*:           Date: 07/25/1999
*:      Reference: *C200080,1
*:************************************************************************
*: Passed Parameters  : Program type   => lcPoNo,llAutoGen
*:************************************************************************
*: Modification :
*:************************************************************************

PARAMETERS lcPoNo,llAutoGen
DO (gcAppHome+'MFCSSH') WITH 'D',IIF(TYPE('lcPoNo')='C',lcPoNo,''),llAutoGen
