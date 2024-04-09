*--Style Inventory Transfer.

*--Transfer inventory available only in multi warehouse setting.
IF (gfGetMemVar('M_WareHouse')='Y')
  DO FORM (oAriaApplication.ScreenHome+"IC\ICINVTA.scx") WITH 3
ELSE
  *--The system has not been setup to use multiple locations. Cannot proceed.
  =gfModalGen('TRM42054B42001','DIALOG')
ENDIF
