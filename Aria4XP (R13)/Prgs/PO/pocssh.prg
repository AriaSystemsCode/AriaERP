*:************************************************************************
*: Program file  : POCSSH.PRG
*: Program desc. : PO Cost Sheet
*: For screen    : MFCSSH.SCX
*:         System: ARIA 4XP
*:         Module: Style Purchase Order (PO).
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
*=oAriaApplication.DoProgram('AWRMFCSSH','"I","'+lcPoNo+'",'+lcAutoGen,.F.,'PO')
DO (oAriaApplication.ProgramHome+'MFCSSH.FXP') WITH 'I',lcPoNo,llAutoGen
