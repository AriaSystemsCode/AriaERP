*:************************************************************************
*: Program file  : MFRCVAR.PRG
*: Program desc. : Issue Adornment order
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Manufactoring (MF).
*:      Developer: TAK
*:           Date: 10/27/1999
*:      Reference: *B603237
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
  *--Reveive adornment order for Fresh Produce.
  DO (gcAppHome+'POSTREC') WITH 'E'
ENDIF