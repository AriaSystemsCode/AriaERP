*:************************************************************************
*: Program file  : MFDCSH.PRG
*: Program desc. : Dye Cost Sheet
*: For screen    : MFCSSH.SCX
*:         System: ARIA 4XP
*:         Module: Manufactoring (MF).
*:      Developer: AHMED MAHER (AMH)
*:           Date: 12/21/2003
*:      Reference: *N119813,1
*:************************************************************************
*: Passed Parameters  : Program type   => lcPoNo,llAutoGen
*:************************************************************************
*: Modification :
*:************************************************************************

LPARAMETERS lcPoNo,llAutoGen
lcPoNo = IIF(TYPE('lcPoNo')='C',lcPoNo,'')
LOCAL lcAutoGen
lcAutoGen = IIF(llAutoGen,'.T.','.F.')
=oAriaApplication.DoProgram('AWRMFCSSH','"D","'+lcPoNo+'",'+lcAutoGen,.F.,'MF')
