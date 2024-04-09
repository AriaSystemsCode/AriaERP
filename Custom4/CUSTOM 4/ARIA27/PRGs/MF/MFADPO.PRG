*:************************************************************************
*: Program file  : MFDCSH.PRG
*: Program desc. : Dye Order
*: For screen    : POSTYLE.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Manufactoring (MF).
*:      Developer: AMM - Ahmed Mohamed Ibrahim
*:           Date: 07/25/1999
*:      Reference: *C200080,1
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Modification :
*:************************************************************************
IF !gfGetMemvar('M_BOMVAR')
  *-- Message "System hasn't been setup to use variant cost sheet, Cannot proceed."
  =gfModalGen('TRM00353B00000','DIALOG')
  glQuitting = .T.
  RETURN
ELSE
  DO (gcAppHome+'POSTYLE') WITH 'A'
ENDIF