*:************************************************************************
*: Program file  : MACSSH.PRG
*: Program desc. : Material Manufacturing Order Cost Sheet
*: For screen    : MFCSSH.SCX
*:         System: ARIA 4XP
*:         Module: Material (MA).
*:      Developer: AHMED MAHER (AMH)
*:           Date: 12/21/2003
*:      Reference: *N119813,1
*:************************************************************************
*: Passed Parameters  : Program type   => lcPoNo,llAutoGen
*:************************************************************************
*: Modification :
*: N037637,1 MMT 09/30/2012 Convert Manufacturing Order Cost Sheet[T20110914.0019]
*:************************************************************************

LPARAMETERS lcPoNo,llAutoGen
lcPoNo = IIF(TYPE('lcPoNo')='C',lcPoNo,'')
LOCAL lcAutoGen
lcAutoGen = IIF(llAutoGen,'.T.','.F.')

*B999999,1 AMH Fix bug of blank screen open [Start]
*=oAriaApplication.DoProgram('AWRMFCSSH','"T","'+lcPoNo+'",'+lcAutoGen,.F.,'MA')
DO (oAriaApplication.ProgramHome+'MFCSSH.FXP') WITH 'T',lcPoNo,llAutoGen
*B999999,1 AMH [End]
