*:************************************************************************
*: Program file  : MFRCVAP.PRG
*: Program desc. : Issue Adornment order
*: For screen    : POSTREC.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Manufactoring (MF).
*:      Developer: AMM - Ahmed Mohamed Ibrahim
*:           Date: 08/04/1999
*:      Reference: *E301291,4
*:************************************************************************
*: Passed Parameters  : Program type   => lcPoNo,llAutoGen
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