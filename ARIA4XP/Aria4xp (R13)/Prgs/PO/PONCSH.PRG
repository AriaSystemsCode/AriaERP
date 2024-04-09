*:************************************************************************
*: Program file  : PONCSH.PRG
*: Program desc. : Inter-Location PO Cost Sheet
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

*wael
IF ALLTRIM(gfGetMemVar('M_WAREHOUS',oAriaApplication.ActiveCompanyID))<>'Y'
  *--The system has not been setup to use multiple locations. Cannot proceed.
  =gfModalGen('TRM42054B42001','DIALOG')
  RETURN .F.
ENDIF
*wael

*B999999,1 AMH Fix bug of screen not open if use is operatior [Start]
*=oAriaApplication.DoProgram('AWRMFCSSH','"N","'+lcPoNo+'",'+lcAutoGen,.F.,'PO')
DO (oAriaApplication.ProgramHome+'MFCSSH.FXP') WITH 'N',lcPoNo,llAutoGen
*B999999,1 AMH [End]
