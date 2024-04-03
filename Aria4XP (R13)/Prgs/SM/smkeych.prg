*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMKEYCH.Prg
*:  Module      : System Manager
*:  Desc.       : Global Changes screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/28/2013
*:  Reference   : *E303425,1 TMI 10/21/2013 []
*:************************************************************************
PARAMETERS lcParam
*B610613,3 TMI 12/30/2013 16:01 [Start] get the local program
*DO (oAriaapplication.applicationhome + 'SM\SMFLDCH.FXP') WITH lcParam
lcRunPrg = lfGetPrg('SM\SMFLDCH.FXP')
DO (lcRunPrg) WITH lcParam
*B610613,3 TMI 12/30/2013 16:01 [End  ] 

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
*B610613,3 TMI 12/30/2013 16:06 [Start] 
************************************************************
FUNCTION lfGetPrg
PARAMETERS lcPrg
LOCAL lcRunPrg
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientApplicationHome+lcPrg)
  lcRunPrg = oAriaApplication.clientApplicationHome+lcPrg
ELSE
  lcRunPrg = oAriaApplication.ApplicationHome+lcPrg
ENDIF
RETURN lcRunPrg
 *- End of lfGetScx.
