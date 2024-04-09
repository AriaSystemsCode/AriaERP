*:************************************************************************
*: Program file  : POADPO.PRG
*: Program desc. : Adornment Order
*: For screen    : POSTYLE.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*! For Module    : Style Purchase Order - (PO)
*:      Developer: Ahmed Maher (AMH)
*:           Date: 05/24/2001
*:      Reference: *C102357,1
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