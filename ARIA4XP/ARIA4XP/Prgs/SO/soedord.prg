***********************************************************************
*:   Program file: socont.PRG
*:  Program desc.: Temporary EDI Sales Order
*:         System: Aria 4XP
*:      Developer: Wael Ali Mohamed WAM
*:           Date: 12/21/2003
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*: Modifications      :
*! B608338,1 WAM 10/31/2007 Fix the calling method to sales order screen
*:************************************************************************

*B608338,1 WAM 10/31/2007 Fix the calling method to sales order screen
*DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleId+"\SOORD") WITH 'T'
lcExeKeyValue = "'T'"
oAriaApplication.DoProgram("AWRSOORD",lcExeKeyValue,.F.,'SO')
*B608338,1 WAM 10/31/2007 (End)




