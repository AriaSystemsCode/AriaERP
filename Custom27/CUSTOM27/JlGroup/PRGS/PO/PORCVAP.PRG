*:************************************************************************
*: Program file  : PORCVAP.PRG
*: Program desc. : Issue Adornment order
*: For screen    : POSTREC.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*! For Module    : Style Purchase Order - (PO)
*:      Developer: Ahmed Maher (AMH)
*:           Date: 05/24/2001
*:      Reference: C102358,1
*:************************************************************************
*: Passed Parameters  :
*:************************************************************************
*: Modification :
*:************************************************************************

IF !gfGetMemvar('M_BOMVAR')
  *-- Message "System hasn't been setup to use variant cost sheet, Cannot proceed."
  =gfModalGen('TRM00353B00000','DIALOG')
  glQuitting = .T.
  RETURN
ELSE
  *--Issue Inter Company P/O.
  DO (gcAppHome+'POSTREC') WITH 'A'
ENDIF