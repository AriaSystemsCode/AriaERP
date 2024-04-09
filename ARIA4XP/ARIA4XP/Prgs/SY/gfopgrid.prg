*!*****************************************************************************************
*! Name      : GfOpGrid
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/15/2002 10:04:47 PM
*! Purpose   : Option Grid screen
*! Entry no. : N000398,1 - Build Aria4 Option Grid
*!*****************************************************************************************
*! Parameters: lcOGPrgName - Mandatory, SYDREPRT.CREP_ID (Dictionary ID)
*!           : llProgram   - Optional,  Required if called as program 
*!           : lcRunDirct  - Optional,  Report run direction (Valid values described later)
*!           : llNoShow    - Optional,  Don't Show the OG.
*!****************************************************************************************
*! Returns   : Report Expression or .F.
*!****************************************************************************************
*! lcRunDirct Valid Values :
*! R : Run report or Program.
*! X : User-Defined View Mode.
*! T : User-Defined Edit Mode.
*! M : OG called from menu.
*! P : OG Called from toolbar print button.
*! B : Called from Batch Printing (lcOGPrtDir either V for preview or other to print.)
*!****************************************************************************************
*!Modifications:
*! E037233,2 MAH, 01/14/2004  Show report icons in the list view.
*! B037825,1 MAH 02/18/2004 Copy option grid ranges cursor to calling program data session.
*! B037901,1 MAH 04/06/2004 OGSYS18() function returns the name of the VAR as property
*! B037902,1 MAH 04/06/2004 Variables are declared private in option grid, 
*!               so when close the option grid we loss the changes done to the value 
*!               of these variables
*! B037905,1 MAH 04/06/2004 Option grid returns .F. instead of '.T.'
*! B037919,1 MAH 04/06/2004 Error (Invalid subscript reference) when Run / close the O.G.
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover
*! B038249,1 SMM 07/28/2004 Missing function
*! B038236,1 SMM 07/11/2004 Add a parameter to indicate whether the return value is 
*!                          Property or variable.
*! N038218,1 SMM Define all Form Variables to be accessible all over the OG
*! N038460,1 SMM 09/01/2004 Add a unique index to the ITEM file 
*! N037249,1 SMM 09/29/2004 Handle user defined fields
*! B038633,1 SMM 10/19/2004 Bug in lfmacrosub
*! B128052,1 MAH 06/05/2005 Takes a long time to log in.
*!*****************************************************************************************
#include r:\aria4xp\prgs\sy\gfopgrid.h

* B037902,1 MAH Add Undeclate variables parameter
*-- PARAMETERS lcOGPrgName,llProgram,lcRunDirct, llNoShow

* B037901,1 MAH Add read variable without property parameter
*! N037249,1 SMM 09/29/2004 Add a parameter for user defined fields [Start]
*PARAMETERS lcOGPrgName,llProgram,lcRunDirct, llNoShow, ;
*           llUndeclareVariables, llDonnotReadVariableAsProp
*040078,1 ASM 18/1/2005 Developing Company Informnation Screen in Aria4 [Start]
*PARAMETERS lcOGPrgName,llProgram,lcRunDirct, llNoShow, ;
			llUndeclareVariables, llDonnotReadVariableAsProp, laUserFields, lcBaseFile,lcScrTitle
PARAMETERS lcOGPrgName,llProgram,lcRunDirct, llNoShow, ;
			llUndeclareVariables, llDonnotReadVariableAsProp, laUserFields, lcBaseFile,lcScrTitle, lcFieldsFile, lcShowFunction			
*040078,1 ASM 18/1/2005 Developing Company Informnation Screen in Aria4 [End]
*! N037249,1 SMM 09/29/2004 Add a parameter for user defined fields [END]

*! B128052,1 MAH 06/05/2005 [BEGIN]
= gfAddPerfLog("OPTIONGRID", "OPEN", "START", "Name = " + IIF(TYPE('lcOGPrgName') = 'C' .AND. !EMPTY(lcOGPrgName), lcOGPrgName, ''), .T.)
*! B128052,1 MAH 06/05/2005 [END]

*-- Validate llUndeclareVariables
llDonnotReadVariableAsProp = IIF(TYPE("llDonnotReadVariableAsProp") # 'L' .OR. ;
                             ISNULL(llDonnotReadVariableAsProp), ;
                             .F., ;
                             llDonnotReadVariableAsProp)

PRIVATE llDonnotReadVariableAsProp
* B037901,1 MAH End

*-- Validate llUndeclareVariables
llUndeclareVariables = IIF(TYPE("llUndeclareVariables") # 'L' .OR. ;
                           ISNULL(llUndeclareVariables), ;
                           .F., ;
                           llUndeclareVariables)
* B037902,1 MAH End

*-- Used to find out the global functions...
*-- Validate the only mandatory Parameter.
IF (VARTYPE(lcOGPrgName) != "C") OR EMPTY(lcOGPrgName)
  RETURN .F.
ENDIF
LOCAL lnOldDataSession
lnOldDataSession = SET("Datasession")

*-- Initialize all private variables ... BEGIN
PRIVATE lcOGPrtdir, lcRepTmpNm, llOGAddMode, llOGEditMode, llOGViewMode,lcOGRepID,;
        lnRepIDLen, lcOGManRep, llOGFilter, lcOGReadW, lcOGreadV, lcOGReadS,lnOGSeting,;
        lcOGWinTitl, lcOGOrder, lcOGOrderV, lcOGFormV, llOGBatchOk, llOGVrFlt,lcLogFile,;
        lcOGFlt_ID, lcOGTmpForm, lcOGLastForm, lcRepMode, lcRepAvlM, lcOGPlatForm,;
        lcOGSortID, lcRpExp, llOGFltCh, lcOGPrvRun, lcRpFrxMod, gcCmpModules,;
        laOGObjType, laOGObjCnt, llOGRefForm, llOGStyleCh,;
        llOGSysData, laRangeInfo, R_WIDTH, llFrxForm, lcOptProg,;
        gcContCode, llRpPrUSel

DECLARE laOGFxFlt[1,8], laOGHdFlt[1,8], laOGVrFlt[1,8], laOGSeting[1,2],;
        laOGObjType[1,3], laOGObjCnt[1], laRangeInfo[1,2]

STORE "" TO lcOGPrtdir, lcRepTmpNm, lcOGManRep,lcOGReadW, lcOGreadV,;
            lcOGReadS, laOGFxFlt, laOGHdFlt, laOGVrFlt,lcOGRepID,lcLogFile,;
            lcOGWinTitl, lcOGOrder, lcOGOrderV, lcOGFormV,;
            lcOGFlt_ID, lcOGTmpForm, lcOGLastForm, lcRepMode, lcRepAvlM,;
            lcOGPlatForm,lcOGSortID, lcRpExp, llOGFltCh, lcOGPrvRun, lcRpFrxMod,;
            laOGObjType, lcOGTmpForm, lcOptProg

R_WIDTH = "N"    && @X,Y SAY Report design width.

*! B608184,1 MMT 07/30/2007 Fix bug of Print Selection criteria when save filter[Start]
*llRpPrUSel  = .T.   && Print report criteria.
llRpPrUSel  = .F.   && Print report criteria.
*! B608184,1 MMT 07/30/2007 Fix bug of Print Selection criteria when save filter[END]

llFrxForm   = .F.
laOGObjCnt  = .F.            
llOGRefForm = .T.
STORE .T. TO llOGStyleCh,llOGSysData

*-- MAB - Create Aria27 global images... BEGIN
*-- IF any one face a new global used in reports
*-- it should be added to this list.
*-- this is to avoid changing report code.
WITH oAriaApplication
  gcCmpModules = .CompanyInstalledModules
  gcBaseCurr = .BaseCurrency
  gcCom_Name = .ActiveCompanyName
  gdSysDate  = .SystemDate
  gcContCode = .DefaultCountry                                          
ENDWITH 
*-- MAB - Create Aria27 global images... END


STORE .F. TO llOGAddMode, llOGEditMode, llOGViewMode,;
             llOGFilter, llOGBatchOk, llOGVrFlt

STORE 0 TO lnRepIDLen, lnOGSeting
*-- Initialize all private variables ... END


*-- 037231,1 HFK 12/22/2003 To Set The G/L Account Mask  [Begin]
STORE SPACE(0) TO lcAcMask    && Account mask
*AMH
=lfSetGLMsk()
*-- 037231,1 HFK 12/22/2003 To Set The G/L Account Mask  [Begin] [End]

*-- Instantiate the OG objects ..... BEGIN
PRIVATE loOGScroll

LOCAL lcClassDir, lcScreenDir, oOptionGrid
lcClassDir  = ADDBS(oAriaApplication.ClassDir)
*--   lcScreenDir = ADDBS(ALLTRIM(oAriaApplication.ScreenHome)) + "SY\"
*--   DO FORM (lcScreenDir + "SyOPGrid") NAME oOptionGrid NOSHOW 
oOptionGrid = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll  = oOptionGrid.OptionGrid.oHost

*-- Initialize OG Scroll area.
*AMH
IF NOT loOGScroll.InitScroll(lcOGPrgName,llProgram,lcRunDirct)
  *! B128052,1 MAH 06/05/2005 [BEGIN]
  = gfAddPerfLog("OPTIONGRID", "OPEN", "ABNORMAL_END1",  "")
  *! B128052,1 MAH 06/05/2005 [END]

  oOptionGrid = .NULL.
  RETURN .F.
ENDIF
*-- Instantiate the OG objects ..... END

WITH loOGScroll
  *-- B037825,1 MAH Assing the data session before calling option grid.
  .lnOldDataSession = lnOldDataSession
  *-- MAH End
  gcSysHome  = .gcSysHome
  gcDataDir  = .gcDataDir
  gcWorkdir  = .gcWorkdir
  gcRepHome  = .gcRepHome
  gcAct_Appl = .gcAct_Appl
  gcAct_Comp = .gcAct_Comp
  gcScrHome  = .gcScrHome
  gcUser_ID  = .gcUser_ID
  lcOGTmpForm = .lcOGTmpForm
ENDWITH 

*-- All OG Variables should be shown to this program.
IF !EMPTY(loOGScroll.cRepVariables)
  LOCAL lcOGVariables

  * MAH T20080413.0001 10/15/2008 BEGIN
  lcOGVariables = ALLTRIM(loOGScroll.cRepVariables)
  
  LOCAL lcOGVariables1
  lcOGVariables1 = ""
  
  TRY
    lcOGVariables1 = gfGetOptionGridVars()
  CATCH
  
  ENDTRY
  
  IF !EMPTY(lcOGVariables1)
    PRIVATE &lcOGVariables1.
    STORE "" TO &lcOGVariables1.
    = gfInitOptionGridVars()
  ENDIF   
  
  * MAH T20080413.0001 10/15/2008 BEGIN
  
  * B037902,1 MAH Undeclare variables if required
  IF !llUndeclareVariables
  * B037902,1 MAH End
    PRIVATE &lcOGVariables.
    STORE "" TO &lcOGVariables.    && Initial all report OG level variables.
  * B037902,1 MAH un declare variables if required
  ENDIF
  * B037902,1 MAH End
ENDIF 

*! N038218,1 SMM Define all Form Variables to be accessible all over the OG [START]  
IF !EMPTY(loOGScroll.cFormVariables)
  LOCAL lcOGFormVariables, lcFormVar
  lcOGFormVariables = ALLTRIM(loOGScroll.cFormVariables)  
  PRIVATE &lcOGFormVariables.
  STORE "" TO &lcOGFormVariables.    && Initial all report OG level variables.
  LOCAL lnItems
  FOR lnItems = 1 TO ALEN(loOGScroll.aFormVariables,1)   	
    IF !EMPTY(loOGScroll.aFormVariables[lnItems,2])   
  	  lcFormVar = loOGScroll.aFormVariables[lnItems,1] 
	    &lcFormVar. = loOGScroll.aFormVariables[lnItems,2]
      && Check data types of the variables
    ENDIF  
  ENDFOR
ENDIF 
*! N038218,1 SMM Define all Form Variables to be accessible all over the OG [END]  

*! E037249,1 SMM Call AddUDFFilters In case of user defined fields [START] 
*!*	loOGScroll.DefineObjects()           && Define OG Objects... 
IF TYPE('laUserFields') # 'C' 
  *AMH
  loOGScroll.DefineObjects()           && Define OG Objects... 
ELSE
   *040078,1 ASM 18/1/2005 Developing Company Informnation Screen in Aria4 [Start]
  IF TYPE('lcFieldsFile')<>'C'
    loOGScroll.GetUserFilters()
  ENDIF
  loOGScroll.mAddUDFFilters('FX',lcFieldsFile)
  IF !EMPTY(lcShowFunction)
    loOGScroll.lcOGReadS = lcShowFunction
    loOGScroll.DoShow()
  ENDIF
  *040078,1 ASM 18/1/2005 Developing Company Informnation Screen in Aria4 [End]

  loOGScroll.GetUserFilters
  loOGScroll.mAddUDFFilters('FX')
EndIF
*! E037249,1 SMM Call AddUDFFilters In case of user defined fields [END]

*-- Adjust OG Title and startup position.
oOptionGrid.OptionGrid.Caption = lcOGWinTitl
oOptionGrid.OptionGrid.AutoCenter = .T.

LOCAL lvReturnValue
*-- Show the OG
IF llNoShow
  lvReturnValue = oOptionGrid.OptionGrid   && Return the Option Grid form.

  *! B128052,1 MAH 06/05/2005 [BEGIN]
   = gfAddPerfLog("OPTIONGRID", "OPEN", "END2",  "")
   *! B128052,1 MAH 06/05/2005 [END]
ELSE
  IF loOGScroll.lShowOG    && Option Grid in the Show mode.
    *-- E037233,2 01/14/2004 Start Flag that option grid is running
    PUBLIC glOptionGridIsRuning
    glOptionGridIsRuning = .T.

   *! B128052,1 MAH 06/05/2005 [BEGIN]
   = gfAddPerfLog("OPTIONGRID", "OPEN", "END1",  "")
   *! B128052,1 MAH 06/05/2005 [END]

    *-- E037233,2 01/14/2004 End
    oOptionGrid.OptionGrid.Show(1)  && Show as a modal form.
    *-- E037233,2 01/14/2004 Start Flag that option grid is not running
    glOptionGridIsRuning = .F.
    *-- E037233,2 01/14/2004 End
    *=lfTempFunc()   
  ENDIF
  * B037905,1 MAH Return '.T.' in case of empty
  *-- lvReturnValue = IIF(EMPTY(lcRpExp) OR UPPER(ALLTRIM(lcRpExp))=".T.",.F.,lcRpExp)
  lvReturnValue = IIF(EMPTY(lcRpExp), ".T.", lcRpExp)
  * B037905,1 MAH End
ENDIF   
  
* B037919,1 MAH Destroy option grid
loOGScroll  = .NULL.
oOptionGrid = .NULL.
* B037919,1 MAH End

ON KEY  && No Key assigned to.
SET DATASESSION TO lnOldDataSession
ON KEY  && No Key assigned to.
RETURN lvReturnValue  && Return value.
*-- END OF PROGRAM CODE.

*!*****************************************************************************************
*!*****************************************************************************************
*!*****************************************************************************************
*!*****************************  OG Global Functions Section ******************************  
*!*****************************************************************************************
*!*****************************************************************************************
*!*****************************************************************************************


*!*****************************************************************************************
*! Name      : OGTrapKey
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 09/30/2002 10:13:24 AM
*! Purpose   : Trap OG Keys...(HOME, END, PGUP, and PGDN)
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: cPressedKey - Pressed key
*!****************************************************************************************
*!
FUNCTION OGTrapKey
  LPARAMETERS cPressedKey
  cPressedKey = ALLTRIM(cPressedKey)
  RETURN loOGScroll.OGTrapKey(cPressedKey)
ENDFUNC 
*-- end of OGTrapKey.

*!*****************************************************************************************
*! Name      : OGSys18
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 09/30/2002 10:13:24 AM
*! Purpose   : Simulate Old SYS(18) function.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Returns   : Object reference to active object.
*!****************************************************************************************
*!Modifications :
*! B038236,1 SMM 07/11/2004 Add a parameter to indicate whether the return value is 
*!                          Property or variable.
*!****************************************************************************************
FUNCTION OGSys18
* B038236,1 SMM Add parameter [START]
*--  RETURN OGVarRead()
  LPARAMETERS llGetAsVar
  IF TYPE("llGetAsVar") # 'L' .OR. ISNULL(llGetAsVar)
    RETURN OGVarRead(.F.)
  ELSE
    RETURN OGVarRead(llGetAsVar)  
  ENDIF 	
* B038236,1 SMM Add parameter [END]
ENDFUNC
*-- end of OGSys18.

*!*****************************************************************************************
*! Name      : OGVarRead
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 09/30/2002 10:13:24 AM
*! Purpose   : Simulate Old VARREAD function.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Returns   : Object reference to active object.
*!****************************************************************************************
*!Modifications:
*! B037901,1 MAH 04/06/2004 OGSYS18() function returns the name of the VAR as property
*! B038236,1 SMM 07/11/2004 Add a parameter to indicate whether the return value is 
*!                          Property or variable.
*!*****************************************************************************************
*!
FUNCTION OGVarRead
  * B038236,1 SMM Add parameter [START]
  LPARAMETERS llGetAsVar
  * B038236,1 SMM Add parameter [END]
  LOCAL lcFocusControl, loActiveControl, lcVariableRef
  lcFocusControl = loOGScroll.FocusControl
  IF VARTYPE(lcFocusControl) != "C" OR EMPTY(lcFocusControl)
    RETURN ".NULL."
  ENDIF 
  lcFocusControl  = ALLTRIM(lcFocusControl)
  IF (TYPE("loOGScroll." + lcFocusControl) != "O")
    RETURN ".NULL."
  ENDIF 
  
  loActiveControl = loOGScroll.&lcFocusControl.
  IF ISNULL(loActiveControl)
    RETURN ".NULL."
  ENDIF

  *-- If active object has a GetValue method (All OG objects have.)
  IF PEMSTATUS(loActiveControl.Parent,"GetVariable",5)
    lcVariableRef = UPPER(ALLTRIM(loActiveControl.Parent.GetVariable()))

    *-- Return a reference to the control's variable.
    * B037901,1 MAH Remove loOGScroll refrence
    *-- RETURN STRTRAN(lcVariableRef,"THISFORM.OHOST.","loOGScroll.")
	* B038236,1 SMM Change the condition to use the llGetAsVar [START]
	*-- IF llDonnotReadVariableAsProp
    IF (llDonnotReadVariableAsProp) .OR. (llGetAsVar)
	* B038236,1 SMM Change the condition to use the llGetAsVar [END]
      RETURN STRTRAN(lcVariableRef,"THISFORM.OHOST.","")
    ELSE
      RETURN STRTRAN(lcVariableRef,"THISFORM.OHOST.","loOGScroll.")
    ENDIF
    * B037901,1 MAH End
  ELSE
    RETURN ".NULL."
  ENDIF   

ENDFUNC
*-- end of OGVarRead.

*!*****************************************************************************************
*! Name      : GetObjectRef
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 09/30/2002 10:19:28 AM
*! Purpose   : Get an object reference of a specific OG ask at run time variable, or filter.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: cAssociatedVar - Object OG variable.
*!****************************************************************************************
*! Returns   : Object Reference or NULL
*!****************************************************************************************
*!
FUNCTION GetObjectRef
  LPARAMETERS cAssociatedVar
  *-- If Mandatory parameter not passed.
  IF (VARTYPE(cAssociatedVar) != "C") OR EMPTY(cAssociatedVar)
    RETURN .NULL.
  ENDIF 
  cAssociatedVar = ALLTRIM(cAssociatedVar)
  
  *-- If the passed parameter not a defined variable.
  IF TYPE(cAssociatedVar) = "U"
    RETURN .NULL.
  ENDIF 
  
  RETURN loOGScroll.GetObjectRef(cAssociatedVar)  && Return the object reference.
  
ENDFUNC
*-- end of GetObjectRef.

*!*****************************************************************************************
*! Name      : ClearRead
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 09/30/2002 10:21:10 AM
*! Purpose   : Simulates the Old style CLEAR READ command, to refresh the OG scroll area.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION ClearRead
  *-- Clear the read cycle.
  loOGScroll.ClearRead()
ENDFUNC 
*-- end of ClearRead.

*-- Interface functions    ............ BEGIN
*-- NB. MAB - I didn't write all interface function, I wrote them as they required
*--           But I put a wait window to know that this function are required now.

*!*****************************************************************************************
*! Name      : lfClearRep
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/27/2002 01:17:30 AM
*! Purpose   : Stub function when closing the report.
*!*****************************************************************************************
*!
FUNCTION lfClearRep
  *-- Stub function to be included in some reports.
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfModeVld
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/27/2002 01:17:30 AM
*! Purpose   : Stub function to validate report mode change (From Dos to Window and Via Versa)
*!*****************************************************************************************
*!
FUNCTION lfModeVld
  *-- Stub function to validate report mode change (From Dos to Window and Via Versa)
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfRepPltFr
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/27/2002 01:17:30 AM
*! Purpose   : Adjust the report active platform, and its variables.
*!*****************************************************************************************
*!
FUNCTION lfRepPltFr
  LPARAMETERS lcRepFrmName
  RETURN loOGScroll.GetPlatForm(lcRepFrmName)
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfVViewRep
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:10:38 PM
*! Purpose   : Preview the report
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfVViewRep
  loOGScroll.ReportPreview()
ENDFUNC 
*-- end of lfVViewRep.

*!*****************************************************************************************
*! Name      : lfvRunRep
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:10:38 PM
*! Purpose   : Run the report (O/P to printer, File, and Batch)
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfvRunRep
  loOGScroll.ReportPrint()
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfOGShowGet
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:10:38 PM
*! Purpose   : Enable/Disable OG object.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfOGShowGet
  LPARAMETERS lcVarName
  loOGScroll.lfOGShowGet(lcVarName)
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfGetForm
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:10:38 PM
*! Purpose   : Get the report form.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfGetForm
  LPARAMETER lcSpcID
  RETURN loOGScroll.GetForm(lcSpcID)
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfIsApparl
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 10:10:38 PM
*! Purpose   : Check if the active report form is an apparell form or not.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfIsApparl
  *-- cFormID : Should passed by reference.
  LPARAMETERS cFormID
  RETURN loOGScroll.IsApparell(@cFormID)
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfOptProg
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 12:30:32 PM
*! Purpose   : Run Optional Program.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfOptProg
  loOGScroll.DoOptionalProgram()
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfUsrVldFn
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 12:30:32 PM
*! Purpose   : Do user defined function inside custom program.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfUsrVldFn
  *-- NB:- Parameters are Self-Descriptive except vFunctionProgram
  *-- vFunctionProgram:- This parameters is a new feature in 
  *--                    ARIA4, you can pass either the program contains 
  *--                    the function, or an object reference contains a method to run.
  *--                    
 
  LPARAMETERS cFunctionName,vFunctionProgram,cFunctionParameter
  RETURN loOGScroll.DoUserValidFunction(cFunctionName,;
                                        vFunctionProgram,;
                                        cFunctionParameter)
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfChangeGrid
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 12:30:32 PM
*! Purpose   : Called to change the overall grid to another report ID.
*!           : Not implemented yet.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfChangeGrid
  WAIT WINDOW "lfChangeGrid"
ENDFUNC 

*!*****************************************************************************************
*! Name      : lfOGShow
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 12:30:32 PM
*! Purpose   : Not implemented yet.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfOGShow
  WAIT WINDOW "lfOGShow"
ENDFUNC
*-- Interface functions    ............ END


*-- ARIA27 Substitute function (Compatability issues) ... BEGIN
*!*****************************************************************************************
* B123663,1 SMM Change gfMover to lfOGMover
*-- *! Name      : gfMover
*! Name      : lfOGMover
* B123663,1 SMM End
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/10/2002 05:28:10 PM
*! Purpose   : Option grid mover functionality
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION lfOGMover
  LPARAMETERS aSource,aTarget,cWidTitle, lEditMode,cVldFunc,;
              lMV1By1,lReMV1By1,oHost,cVldRemFunc

  loOGScroll.DoMover(@aSource,@aTarget,cWidTitle, lEditMode,cVldFunc,;
                            lMV1By1,lReMV1By1,oHost,cVldRemFunc)  
ENDFUNC 
*-- Interface functions    ............ END

*!*****************************************************************************************
*! Name      : FileExist
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 01/26/2003 11:53:32 AM
*! Purpose   : Check the existance of a file in a disk.
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION FileExist
  LPARAMETERS cFileToCheckFor
  RETURN loOGScroll.FileExist(cFileToCheckFor)
ENDFUNC 
*-- END OF FileExist.

*!*****************************************************************************************
*! Name      : OGInRang
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/10/2002 05:28:10 PM
*! Purpose   : Option grid In-Range functionality
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION OGInRang
  LPARAMETERS lcBrwFile   , lcBrwFields , lcBrwFltExp , lcClrRngSel ,;
             lcSetFunc   , lcSetParm   , lcBrwSelFld , lcTmpCursr  ,;
             lcSeekFlag  , lcPict , lcIndxExpr, lcBrowTitle

  LOCAL lnAlias,llChngIt
  llChngIt = .F.

  lnAlias = SELECT(0)              && Save the old alias



  *! B038633,1 SMM 10/19/2004 Check if the lcBrwFile has & [START]
  * lcBrwFile   = lfMacroSub(EVALUATE(&lcBrwFile))
  IF AT('&',lcBrwSelFld) > 0 
  *	lcBrwSelFld = lfMacroSub(lcBrwSelFld)
    lcBrwSelFld = EVALUATE(SUBSTR(lcBrwSelFld , 2 , LEN(lcBrwSelFld )-1))
  ENDIF

  IF AT('&',&lcBrwFile) > 0 
*	lcBrwFile   = lfMacroSub(&lcBrwFile)
    lcBrwFile = &lcBrwFile      
	lcBrwFile = EVALUATE(SUBSTR(lcBrwFile, 2 , LEN(lcBrwFile)-1))
  ELSE
	lcBrwFile   = lfMacroSub(EVALUATE(&lcBrwFile))
  ENDIF 	
  *! B038633,1 SMM 10/19/2004 Check if the lcBrwFile has & [END]	
  
  *! B999999,1 SMM Allow mBrwFld to Accept a variable instead of Fields [START]
  *lcBrwFields = IIF(EMPTY(&lcBrwFields) , .F. , lfMacroSub(&lcBrwFields))
  IF AT('&',&lcBrwFields) = 1
    lcBrwFields = EVALUATE(SUBSTR(&lcBrwFields,2))
    lcBrwFields = IIF(EMPTY(lcBrwFields) , .F. , lfMacroSub(lcBrwFields))
  ELSE
    lcBrwFields = IIF(EMPTY(&lcBrwFields) , .F. , lfMacroSub(&lcBrwFields))
  ENDIF
  *! B999999,1 SMM Allow mBrwFld to Accept a variable instead of Fields [START]

  lcBrwFltExp = IIF(EMPTY(&lcBrwFltExp) , .F. , lfMacroSub(&lcBrwFltExp))
 
  lcSetFunc   = ALLTRIM(lcSetFunc)

  *-- If we are to clear the old user selection
  IF !EMPTY(lcClrRngSel) .AND. EVALUATE(lcClrRngSel)
    &lcClrRngSel = .F.
    
    *-- If the In Range temp. cursor is opened
    IF USED(lcTmpCursr)
      USE IN &lcTmpCursr
    ENDIF    && End of IF USED(lcTmpCursr)
    
  ENDIF    && End of IF !EMPTY(lcClrRngSel) .AND. EVALUATE(lcClrRngSel)

  *-- If we are to call the Set function to set the relations
  IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 1 , 1)) = 'Y'
    =&lcSetFunc.('S')
  ENDIF    && End of IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 1 , 1)) = 'Y'

  SELECT (lcBrwFile)
  *! N038460,1 SMM 09/01/2004 Add a unique index to the ITEM file [START]
  IF UPPER(ALLTRIM(lcBrwFile)) == 'ITEM' && .AND. TAGCOUNT() = 1
		IF TAGCOUNT() = 1
		  =CURSORSETPROP("Buffering",3,lcBrwFile)
      INDEX ON &lcBrwSelFld TAG lcTag UNIQUE 
    ENDIF
    SET ORDER TO lcTag 
  ENDIF
  *! N038460,1 SMM 09/01/2004 Add a unique index to the ITEM file [END]

  *-- If we are going to use the Set function as a valid function for
  *-- the select process in the In Range screen
  IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 3 , 1)) = 'Y'
    
    *-- If the option type is not "Expression"
    IF !EMPTY(lcIndxExpr)
      &lcIndxExpr = SUBSTR(KEY() , 1 , AT(UPPER(ALLTRIM(lcBrwSelFld)) ,;
                           KEY()) - 1)
      
      &lcIndxExpr = SUBSTR(&lcIndxExpr , 1 , RAT('+' , &lcIndxExpr) - 1)
    ENDIF    && End of IF !EMPTY(lcIndxExpr)
    
    llChngIt = gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp ,;
               lcSetFunc + '("V")' , lcSeekFlag  , lcPict, lcBrowTitle)

  ELSE    && Else [If there will be no validation for the select process]
    
    *-- If the option type is not "Expression"
    IF !EMPTY(lcIndxExpr)
      &lcIndxExpr = SUBSTR(KEY() , 1 , AT(UPPER(ALLTRIM(lcBrwSelFld)) ,;
                          KEY()) - 1)
      
      &lcIndxExpr = SUBSTR(&lcIndxExpr , 1 , RAT('+' , &lcIndxExpr) - 1)
    ENDIF    && End of IF !EMPTY(lcIndxExpr)
    
    llChngIt = gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp , '' ,;
               lcSeekFlag  , lcPict, lcBrowTitle)
    
  ENDIF    && End of IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 3 , 1)) = 'Y'

  *-- If we are to call the Set function to clear the relations
  IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 2 , 1)) = 'Y'
    =&lcSetFunc.('R')
  ENDIF    && End of IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 2 , 1)) = 'Y'

  SELECT (lnAlias)              && Restore the old alias

  loOGScroll.llOGFltCh = loOGScroll.llOGFltCh OR llChngIt
ENDFUNC 
*-- END OF OGInRang.

*!*	*!*************************************************************
*!*	*! Name      : lfMacroSub
*!*	*! Developer : Haytham El_Sheltawi
*!*	*! Date      : 08/11/1999
*!*	*! Purpose   : Function to perform the macro substitution within
*!*	*!             any giving string.
*!*	*!*************************************************************
*!*	*! Called from : lfDefVarPart()
*!*	*!*************************************************************
*!*	*! Calls       : None
*!*	*!*************************************************************
*!*	*! Passed Parameters : 1) lcString, Any string with macros
*!*	*!                        that need to be substituted.
*!*	*!*************************************************************
*!*	*! Return      : The string after performing the macro substitution.
*!*	*!*************************************************************
*!*	*
*!*	FUNCTION lfMacroSub

*!*	  LPARAMETERS lcString

*!*	  LOCAL lcReturStr , lnNoOfMacr , lnCount , lnMacroEnd , lcMacroStr

*!*	  *-- Get the number of occurrences of the macro character "&" in the string
*!*	  lnNoOfMacr = OCCURS('&' , lcString)

*!*	  *-- If the string has no macro character "&" return the string as it is.
*!*	  IF lnNoOfMacr = 0
*!*	    RETURN lcString
*!*	  ENDIF    && End of IF lnNoOfMacr = 0

*!*	  lcReturStr = ''        && Variable to hold the returned string

*!*	  *-- For loop to perform the necessary macro substitution
*!*	  FOR lnCount = 1 TO lnNoOfMacr
*!*	    
*!*	    *-- Add the part of the string that is ahead of the macro character "&"
*!*	    *-- to the returned string
*!*	    lcReturStr = lcReturStr + SUBSTR(lcString , 1 , AT('&' , lcString) - 1)
*!*	    
*!*	    *-- Remove the part of the string that is ahead of the macro character "&"
*!*	    *-- from the original string
*!*	    lcString   = SUBSTR(lcString , AT('&' , lcString))
*!*	    
*!*	    *-- Get the end position of the macro
*!*	    lnMacroEnd = MIN(IIF(AT('.' , lcString) = 0 ,;
*!*	                         LEN(lcString) , AT('.' , lcString)) ,;
*!*	                     IIF(AT(' ' , lcString) = 0 ,;
*!*	                         LEN(lcString) , AT(' ' , lcString) - 1) ,;
*!*	                     IIF(AT('&' , lcString , 2) = 0 ,;
*!*	                         LEN(lcString) , AT('&' , lcString , 2) - 1))
*!*	    
*!*	    *-- Get the part of the string that belongs to the macro substitution
*!*	    lcMacroStr = SUBSTR(lcString , 1 , lnMacroEnd)
*!*	    
*!*	    *-- Remove the part of the string that belongs to the macro substitution
*!*	    *-- from the original string
*!*	    lcString   = SUBSTR(lcString , lnMacroEnd + 1)
*!*	    
*!*	    *-- Perform the macro substitution and add the result to the returned
*!*	    *-- string
*!*	    lcReturStr = lcReturStr + [&lcMacroStr]
*!*	  ENDFOR    && End of FOR lnCount = 1 TO lnNoOfMacr

*!*	  *-- Add any string left from the original string that doesn't have a
*!*	  *-- macro character "&"
*!*	  lcReturStr = lcReturStr + lcString

*!*	  RETURN lcReturStr
*!*	ENDFUNC 
*!*	*-- END OF lfMacroSub.
*!*************************************************************
*! Name      : lfMacroSub
*! Developer : Haytham El_Sheltawi
*! Date      : 08/11/1999
*! Purpose   : Function to perform the macro substitution within
*!             any giving string.
*!*************************************************************
*! Called from : lfDefVarPart()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) lcString, Any string with macros
*!                        that need to be substituted.
*!*************************************************************
*! Return      : The string after performing the macro substitution.
*!*************************************************************
*
FUNCTION lfMacroSub

  LPARAMETERS lcString

  LOCAL lcReturStr , lnNoOfMacr , lnCount , lnMacroEnd , lcMacroStr

  *-- Get the number of occurrences of the macro character "&" in the string
  lnNoOfMacr = OCCURS('&' , lcString)

  *-- If the string has no macro character "&" return the string as it is.
  IF lnNoOfMacr = 0
    RETURN lcString
  ENDIF    && End of IF lnNoOfMacr = 0

  lcReturStr = ''        && Variable to hold the returned string

  *-- For loop to perform the necessary macro substitution
  FOR lnCount = 1 TO lnNoOfMacr
    
    *-- Add the part of the string that is ahead of the macro character "&"
    *-- to the returned string
    lcReturStr = lcReturStr + SUBSTR(lcString , 1 , AT('&' , lcString) - 1)
    
    *-- Remove the part of the string that is ahead of the macro character "&"
    *-- from the original string
    lcString   = SUBSTR(lcString , AT('&' , lcString))
    
    *-- Get the end position of the macro
    lnMacroEnd = MIN(IIF(AT('.' , lcString) = 0 ,;
                         LEN(lcString) , AT('.' , lcString)) ,;
                     IIF(AT(' ' , lcString) = 0 ,;
                         LEN(lcString) , AT(' ' , lcString) - 1) ,;
                     IIF(AT('&' , lcString , 2) = 0 ,;
                         LEN(lcString) , AT('&' , lcString , 2) - 1))
    
    *-- Get the part of the string that belongs to the macro substitution
    lcMacroStr = SUBSTR(lcString , 1 , lnMacroEnd)
    
    *-- Remove the part of the string that belongs to the macro substitution
    *-- from the original string
    lcString   = SUBSTR(lcString , lnMacroEnd + 1)
    
    *-- Perform the macro substitution and add the result to the returned
    *-- string
    lcReturStr = lcReturStr + [&lcMacroStr]
  ENDFOR    && End of FOR lnCount = 1 TO lnNoOfMacr

  *-- Add any string left from the original string that doesn't have a
  *-- macro character "&"
  lcReturStr = lcReturStr + lcString

  RETURN lcReturStr
ENDFUNC 
*-- END OF lfMacroSub.

*!*****************************************************************************************
*! Name      : OGAFIELDS
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/06/2002 01:08:09 PM
*! Purpose   : AFIELDS are in the old structure.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: lcOGArray
*!****************************************************************************************
*!
FUNCTION OGAFIELDS
  LPARAMETERS laFileArray
  IF TYPE("laFileArray[1]") = "U"
    RETURN 0
  ENDIF 
  
  LOCAL lnFields, laTmpFields[1,1], lnField, lnColumns
  lnFields  = AFIELDS(laTmpFields)
  lnColumns = ALEN(laTmpFields,2)
  DECLARE laFileArray[lnFields, 4]
  FOR lnField = 1 TO lnFields
    ACOPY(laTmpFields,laFileArray,((lnField - 1) * lnColumns) + 1,4,((lnField - 1) * 4) + 1)
  ENDFOR
  RETURN lnFields
ENDFUNC 

*!*****************************************************************************************
*! Name      : gfOpenFile
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/06/2002 01:08:09 PM
*! Purpose   : Compatability with Aria27 version
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! NB.       : Support all easy cases only (90% of the cases.)
*!*****************************************************************************************
*!
*--   FUNCTION gfOpenFile
*--     LPARAMETERS cFileFullName,lcIndex,lcOpenMode,lcAliasNam,llForceOp
*--     LOCAL lcFileName, lcAliasName
*--     
*--     lcFileName = UPPER(ALLTRIM(JUSTFNAME(cFileFullName)))
*--     lcAliasNam = UPPER(IIF(VARTYPE(lcAliasNam)="C",ALLTRIM(lcAliasNam),""))
*--     IF EMPTY(lcAliasNam)
*--       lcAliasNam = lcFileName
*--     ENDIF 
*--     IF !USED(lcAliasNam)
*--       IF !EMPTY(lcIndex)
*--         lcIndex = JUSTFNAME(ALLTRIM(lcIndex))
*--       ENDIF 
*--       loOGScroll.OpenTable(lcFileName, lcIndex, lcAliasNam)
*--     ENDIF  
*--   ENDFUNC 

*!*****************************************************************************************
*! Name      : RPT_HDR1
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/24/2002 11:50:40 PM
*! Purpose   : Print Report Header
*! Entry no. : 
*!*****************************************************************************************
*!
PROCEDURE RPT_HDR1
  LPARAMETER XPROG,lcRptTitle,lcRptType,lcOptTitle
  DO RPT_HDR WITH XPROG,lcRptTitle,lcRptType,lcOptTitle
ENDPROC 

*!*****************************************************************************************
*! Name      : RPT_HDR
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/24/2002 11:50:40 PM
*! Purpose   : Print Report Header
*! Entry no. : 
*!*****************************************************************************************
*!
PROCEDURE RPT_HDR
  PARAMETER XPROG,lcRptTitle,lcRptType,lcOptTitle
  IF TYPE('lcOptTitle') != 'C'
    lcOptTitle = ''
  ENDIF

  lnRepLen  = IIF(lcRptType='N',80,IIF(lcRptType='W',132,187))  && Report Width
  lnLftRght = 32     && Right and Left Titles (15 + 17)

  *-- Variables to be printed. [Begin
  lnCenWidth = IIF(lcRptType='N',40,80)              && Center title max. len
  lcRepComp  = ALLTRIM(PADR(gcCom_Name,lnCenWidth))  && Company Name
  lcOptTitle = ALLTRIM(PADR(lcOptTitle,lnCenWidth))  && Optional Title
  lcRptTitle = ALLTRIM(PADR(lcRptTitle,lnCenWidth))  && Report Title
  *-- Variables to be printed. [End

  *-- Positions at them we start print Pre-defined variables [Begin
  lnPrnComp  = 17 + INT((lnRepLen - lnLftRght - LEN(lcRepComp))/2)
  lnPrnOpt   = IIF(EMPTY(lcOptTitle),0, 17 +;
                INT((lnRepLen - lnLftRght - LEN(lcOptTitle))/2))
  lnPrnTitle = INT((lnRepLen - LEN(lcRptTitle))/2)
  *-- Positions at it we start print Pre-defined variables [End..

  *-- Print Header Code [Begin


  *-- Line # 1 in Header[Begin]
  @ 01,000          SAY 'By   : ' + ALLTRIM(gcUser_Id)
  @ 01,lnPrnComp    SAY lcRepComp
  @ 01,lnRepLen-15  SAY 'Date : ' + ALLTRIM(DTOC(gdSysDate))
  *-- Line # 1 in Header[End  ]

  *-- Line # 2 in Header[Begin]
  @ 02,000          SAY 'Time : ' + TIME()
  IF !EMPTY(lcOptTitle)
    @ 02,lnPrnOpt    SAY lcOptTitle
  ENDIF  
  @ 02,lnRepLen-15  SAY 'Page : ' + ALLTRIM(STR(PAGENO))
  *-- Line # 2 in Header[End  ]

  *-- Line # 3 in Header[Begin]
  @ 03,lnPrnTitle   SAY lcRptTitle
  *-- Line # 3 in Header[End  ]

  *-- Line # 4 in Header[Begin]
  @ 04,00 SAY REPLICATE('=',lnRepLen)
  *-- Line # 4 in Header[End  ]

  *-- Print Header Code [End..
ENDPROC 
*-- end of RPT_HDR.

*!*****************************************************************************************
*! Name      : ENDREPORT
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/24/2002 11:50:40 PM
*! Purpose   : Ending the reporting task
*! Entry no. : 
*!*****************************************************************************************
*!
PROCEDURE ENDREPORT
  IF oAriaApplication.gcDevice = 'SCREEN'
     = gfEndPrn()
  ELSE
    LOCAL lcConsoleSet
    lcConsoleSet = SET("Console")
    SET CONSOLE OFF
    =gfToPrintr()
    SET CONSOLE &lcConsoleSet.
    SET PRINTER TO NULL
    SET PRINTER TO
  ENDIF
  RETURN .T.
ENDPROC
*-- end of ENDREPORT.  

*!*****************************************************************************************
*! Name      : gfToPrintr
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/24/2002 11:50:40 PM
*! Purpose   : Send o/p text to the printer or file.
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION gfToPrintr
  lcPrntTool = GETENV('ARIAPRN')

  llError=.F.
  lcOnErr=ON('ERROR')
  ON ERROR llError=.T.
  lcSetSafe=SET('SAFE')
  SET SAFE OFF

  IF oAriaApplication.gcDevice = "PRINTER"
    lcPdDriv = _PDRIVER
    lcPDSetup = _PDSETUP
    gcLoclPrt = oAriaApplication.gcPrnt_Port
    *SET PRINTER TO
    *SET PRINTER TO (oAriaApplication.gcPrnt_Port)
    =SetPrinterTo(oAriaApplication.gcPrnt_Port)

    DO WHILE SYS(13)='OFFLINE'
      IF gfModalGen("QRM00054B00015",'ALERT') = 2
        llError=.T.
        EXIT
      ENDIF
    ENDDO 

  ENDIF

  IF FileExist(oAriaApplication.gcOutFile) AND !llError

    IF oAriaApplication.gcDevice = "PRINTER"
      =lfSetEscSeq()
    ENDIF
    
    DO CASE
      CASE lcPrntTool = '???' OR lcPrntTool = '??' OR lcPrntTool = '?'
        lcTmpCur = gfTempName() && temprory cursor
        lcWorkA = SELECT()      && curr. work area
        CREATE CURS &lcTmpCur (mprnfile M) && create temp. cursor
        APPEND BLANK 
        *-- append memo with the output file
        APPEND MEMO mprnfile FROM (oAriaApplication.gcOutFile) 
        IF oAriaApplication.glPrnt_Lan                && if it is a lan option
          lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(oAriaApplication.gnPrnt_No))+'\S='+ALLTRIM(oAriaApplication.gcServ_Nam);
                      +'\Q='+ALLTRIM(oAriaApplication.gcQueu_nam);
                      +IIF(_pCopies>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                      +IIF(oAriaApplication.glBaner,'\B='+oAriaApplication.gcBaner_H,'\N')
          SET PRINTER TO &lcPrintSet        
        ELSE
          SET PRINTER TO (gcLoclPrt)
        ENDIF       
        
        SET PRINTER ON
        lnMemoWid = SET('MEMOW')
        llWrapStat = _WRAP
        lnIndent = _INDENT
        lnRMargin = _RMARGIN
        lnLMargin = _LMARGIN
        SET MEMOW TO 65              
        _WRAP = .T.
        _RMARGIN = 255
        _LMARGIN = 0
        _INDENT = 2
        FOR lnNoCopy = 1 TO _PCOPIES
          IF lcPrntTool == '???'
            &lcPrntTool ALLTRM(mprnfile)
          ELSE
            &lcPrntTool MPRNFILE FUNCTION 'V253'
          ENDIF  
        ENDFOR
        SET PRINTER OFF
        SET PRINTER TO
        SET MEMOW TO lnMemoWid
        _WRAP = llWrapStat
        _INDENT = lnIndent 
        _RMARGIN = lnRMargin 
        _LMARGIN = lnLMargin 
                
        *-- close the temp. cursor and select the curr. work area
        USE IN &lcTmpCur
        SELECT (lcWorkA)          

      CASE lcPrntTool="COPY"
        IF oAriaApplication.gcDevice = "PRINTER"
          lcHeadSet = SET('HEADING')
          lnMemoWid = SET('MEMOW')
          llWrapStat = _WRAP
          lnRMargin = _RMARGIN
          lnLMargin = _LMARGIN
          lcAdvance = _PADVANCE
          SET MEMOW TO 255              
          _WRAP = .F.
          _RMARGIN  = IIF(R_Width='XW',240,IIF(R_Width='W',132,80))      
          _LMARGIN = 0
          _PADVANCE = 'LINEFEED'
          SET HEADING OFF
          SET PRINTER TO (gcLoclPRT)  
          
          FOR lnNoCopy = 1 TO _PCOPIES
            !COPY (oAriaApplication.gcOutFile) &gcLoclPRT
          ENDFOR  

          SET HEADING &lcHeadSet
          SET MEMOW TO lnMemoWid
          _WRAP = llWrapStat
          _RMARGIN = lnRMargin 
          _LMARGIN = lnLMargin 
          _PADVANCE = lcAdvance
        ENDIF  

      CASE lcPrntTool="LIB"
*-- URGENT TO ASK MAN
*--           IF FILE("TXTSPOOL.FLL") AND FILE("PRTWIN.EXE") 
*--             SET LIBRARY TO TXTSPOOL.FLL ADDITIVE
*--             lcWinPrt  = "IBM Proprinter on " + ALLTRIM(gcLoclPRT)
*--             lcOrgPrnt = PRTWIN(lcWinPrt)
*--             FOR lnNoCopy = 1 TO _PCOPIES
*--               =TXTSPOOL(oAriaApplication.gcOutFile,"ARIA SYSTEMS")
*--             ENDFOR  
*--             = PRTWIN(lcOrgPrnt)
*--             RELEASE LIBRARY TXTSPOOL.FLL
*--           ENDIF
      CASE lcPrntTool = 'TYPE'
        lcHeadSet = SET('HEADING')
        lnMemoWid = SET('MEMOW')
        llWrapStat = _WRAP
        lnRMargin = _RMARGIN
        lnLMargin = _LMARGIN
        lcAdvance = _PADVANCE
        SET MEMOW TO 255              
        _WRAP = .F.
        _RMARGIN  = IIF(R_Width='XW',240,IIF(R_Width='W',132,80))      
        _LMARGIN = 0
        _PADVANCE = 'LINEFEED'
        SET HEADING OFF
        lcTempPr=_PDSETUP
        SET PRINTER TO (oAriaApplication.gcPrnt_Port)
        FOR lnNoCopy = 1 TO _PCOPIES
          TYPE (oAriaApplication.gcOutFile) TO PRINTER
        ENDFOR  
        SET PRINTER TO 
        SET HEADING &lcHeadSet
        SET MEMOW TO lnMemoWid
        _PDSETUP = lcTempPr
        _WRAP = llWrapStat
        _RMARGIN = lnRMargin 
        _LMARGIN = lnLMargin 
        _PADVANCE = lcAdvance
      OTHERWISE
        FOR lnNoCopy = 1 TO _PCOPIES
          COPY FILE (oAriaApplication.gcOutFile) TO (oAriaApplication.gcPrnt_Port)
        ENDFOR  
    ENDCASE

    SET PRINTER OFF

    IF oAriaApplication.gcDevice = "PRINTER"
      _PDSETUP = lcPDSetup
    ENDIF  
    
  ENDIF
  ON ERROR &lcOnErr
  SET SAFE &lcSetSafe    

  IF oAriaApplication.gcDevice = "PRINTER"
    ERASE (oAriaApplication.gcOutFile)

  ELSE  && Else if device is File relase file from printer port (i.e. user can read it)
    SET PRINTER TO

  ENDIF

ENDFUNC 
*-- end of gfToPrintr.

*!*****************************************************************************************
*! Name      : lfSetEscSeq
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/24/2002 11:50:40 PM
*! Purpose   : Set the printer escape sequence
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION lfSetEscSeq
  PRIVATE lcTempFile

  lcHeadSet = SET('HEADING')
  llWrapStat = _WRAP
  lnRMargin = _RMARGIN
  lnLMargin = _LMARGIN
  lcAdvance = _PADVANCE

  _WRAP = .F.
  _RMARGIN  = IIF(R_Width='XW',240,IIF(R_Width='W',132,80))      
  _LMARGIN = 0
  _PADVANCE = 'LINEFEED'
  SET HEADING OFF

  lcTempFile = gcWorkDir+gfTempName()+'.TXT'
  SET PRINTER TO (lcTempFile)
  SET PRINTER ON
  IF _PEJECT = "NONE"
    _PADVANCE = 'LINEFEED'
  ELSE
    _PADVANCE = 'FORMFEED'
    IF INLIST(_PEJECT ,"BEFORE",'BOTH')
      EJECT
    ENDIF  
  ENDIF  

  PRINTJOB
    TYPE (oAriaApplication.gcOutFile) TO PRINTER
  ENDPRINTJOB  
  ERASE (oAriaApplication.gcOutFile)

  SET PRINTER OFF
  SET PRINTER TO
  oAriaApplication.gcOutFile = lcTempFile
  SET HEADING &lcHeadSet.
  _WRAP     = llWrapStat 
  _RMARGIN  = lnRMargin 
  _LMARGIN  = lnLMargin 
  _PADVANCE = lcAdvance 

ENDFUNC 
*-- end of lfSetEscSeq.

*!*****************************************************************************************
*! Name      : gfSetDevice
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/24/2002 11:50:40 PM
*! Purpose   : Set the device 
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION gfSetDevice
  DO CASE
    CASE oAriaApplication.gcDevice = "PRINTER" 
      _RMARGIN  = IIF(R_Width='XW',240,IIF(R_Width='W',132,80))
      oAriaApplication.gcOutFile = gcWorkDir+gfTempName()+'.TXT'
      SET PRINTER TO (oAriaApplication.gcOutFile)
      SET DEVICE TO PRINTER
      
    CASE oAriaApplication.gcDevice = "SCREEN" 
      oAriaApplication.gcOutFile = gcWorkDir+gfTempName()+'.TXT'
      SET PRINTER TO (oAriaApplication.gcOutFile)

     CASE oAriaApplication.gcDevice = "FILE"     
       IF loOGScroll.lAdditive
         SET PRINTER TO (oAriaApplication.gcOutFile) ADDITIVE 
       ELSE
         SET PRINTER TO (oAriaApplication.gcOutFile)
       ENDIF  
       SET DEVICE TO PRINTER   
  ENDCASE
ENDFUNC 
*-- end of gfSetDevice.

*!*****************************************************************************************
*! Name      : gfRepCur
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/24/2002 11:50:40 PM
*! Purpose   : Set a report exchange rate
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION gfRepCur
  PARAMETERS llCalledFromAdv, lcRepCurr, ldExRtDate, lcTmpFlName 
  PRIVATE lcRepCurr, ldExRtDate, lcTmpFlName
  LOCAL loRepCurrency, lcClassDir
  lcClassDir  = ADDBS(oAriaApplication.ClassDir)
  loRepCurrency = NEWOBJECT("repcurrency",lcClassDir+"optiongrid.vcx","","lcRepCurr", "ldExRtDate", "lcTmpFlName")
  loRepCurrency.Show(1)
ENDFUNC 
*-- ARIA27 Substitute function (Compatability issues) ... END

*!*****************************************************************************************
*! Name      : Optional Message Interface
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/10/2002 04:23:47 PM
*! Purpose   : 
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION gfOptMsg
  PARAMETERS lcArrayNam , lcTitle

  *IF The first parameter is not of type Character [The Array name] or
  *The first element of the Array is not of type Character
  IF TYPE('lcArrayNam') <> 'C' .OR. TYPE(lcArrayNam) <> 'C'
    RETURN
  ENDIF

  LOCAL lnRowNumb , lnLineWid , lnMaxWid ,;
        loOptionalMsg, lcClassDir

  lcClassDir  = ADDBS(oAriaApplication.ClassDir)
  loOptionalMsg = NEWOBJECT("optionalmessage",lcClassDir+"optiongrid.vcx")
  IF VARTYPE(loOptionalMsg) != "O"
    RETURN 
  ENDIF 

  IF (VARTYPE(lcTitle) = "C") AND !EMPTY(lcTitle)
    loOptionalMsg.Caption = ALLTRIM(lcTitle)
  ENDIF 

  IF (TYPE(lcArrayNam + '[1,2]') = "N") AND &lcArrayNam.[1 , 2] > 0
    lnLineWid = &lcArrayNam.[1 , 2]
  ELSE
    lnLineWid = 50
  ENDIF 
  lnMaxWid = lnLineWid          && Variable to hold the Max line length

  WITH loOptionalMsg
    *IF (lcArrayNam) is not an Array
    IF TYPE(lcArrayNam + '[1,1]') = 'U'
      DECLARE .aLabels[1,2]
      .aLabels[1,1] = &lcArrayNam.
      .aLabels[1,2] = REPLICATE('X' , lnLineWid)
    ELSE    && Else
      
      *FOR Loop to scan the Array (lcArrayNam) [IF The Array is more than 5 rows
      * scan the First 5 only]
      FOR lnRowNumb = 1 TO MIN(5 , ALEN(&lcArrayNam , 1))
        
        *IF The First column of the current row is not of type Character
        * OR if it is empty OR its not holding the name of a Variable of type
        *Character
        IF TYPE(lcArrayNam + '[' + STR(lnRowNumb , 1) + ',1]') <> 'C' .OR.;
           EMPTY(&lcArrayNam.[lnRowNumb , 1]) .OR.;
           TYPE(&lcArrayNam.[lnRowNumb , 1]) <> 'C'
          
          EXIT
        ELSE    && Else
          DECLARE .aLabels[lnRowNumb , 2]
          .aLabels[lnRowNumb,1] = EVALUATE(&lcArrayNam.[lnRowNumb , 1])
          
          .aLabels[lnRowNumb,2] = IIF(TYPE(lcArrayNam + '[' + STR(lnRowNumb ,;
                                    1) + ',3]') <> 'C' .OR.;
                                    EMPTY(&lcArrayNam.[lnRowNumb , 3]) .OR.;
                                    ALEN(&lcArrayNam , 2) < 3 ,;
                                    REPLICATE('X' , lnLineWid) ,;
                                    &lcArrayNam.[lnRowNumb , 3])
        
          lnMaxWid = MAX(lnMaxWid , IIF(TYPE(lcArrayNam + '[' + STR(lnRowNumb ,;
                         1) + ',2]') <> 'N' .OR. ALEN(&lcArrayNam , 2) < 2 ,;
                         0 , &lcArrayNam.[lnRowNumb , 2]))
        ENDIF    && End of IF
      ENDFOR    && End of FOR Loop
    ENDIF    && End of IF

    .AdjustLabels()  && Adjust form labels.
    
  ENDWITH
  loOptionalMsg.Show()          && Show the form.
  loOptionalMsg = .NULL.        && Release the form reference.

ENDFUNC 
*-- end of gfOptMsg.

*!*****************************************************************************************
*! Name      : BeforeOpenDataFiles
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 02/27/2003 02:20:34 PM
*! Purpose   : Event occurs before open data files.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: laOGTables - Data Tables array passed By Ref (@)
*!****************************************************************************************
*!
FUNCTION BeforeOpenDataFiles
  LPARAMETERS laOGTables
  IF TYPE("laOGTables[1]") != "C"
    RETURN .F.
  ENDIF 

  *-- Stub Event occurs before openning data files.

  RETURN .T.
ENDFUNC 
*-- end of before Open Data Files.

*!*****************************************************************************************
*! Name      : AfterOpenDataFiles
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 02/27/2003 02:20:34 PM
*! Purpose   : Event occurs after open data files.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: laOGTables - Data Tables array passed By Ref (@)
*!****************************************************************************************
*!
FUNCTION AfterOpenDataFiles
  LPARAMETERS laOGTables
  IF TYPE("laOGTables[1]") != "C"
    RETURN .F.
  ENDIF 

  *-- Stub Event occurs after openning data files.

  RETURN .T.
ENDFUNC 
*-- end of After Open Data Files.


*!*****************************************************************************************
*! Name      : SetPrinterTo
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 05/28/2003 06:20:34 PM
*! Purpose   : Set printer to a specified port
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: lcPort2Use - Printer Port.
*!****************************************************************************************
*!
FUNCTION SetPrinterTo
  LPARAMETERS lcPort2Use
  IF (VARTYPE(lcPort2Use) != "C") OR EMPTY(lcPort2Use)
    RETURN .F.
  ENDIF
  LOCAL laPrinters[1,2], lnPrinters, lnPrinter, lnPortLen, lcMapPort2, lcMapCommand, lcPortString

  lcPort2Use = UPPER(ALLTRIM(lcPort2Use))
  lnPortLen  = LEN(lcPort2Use) + 2
  lnPrinters = APRINTERS(laPrinters)
  IF lnPrinters = 0 
    RETURN .F.
  ENDIF
  FOR lnPrinter = 1 TO lnPrinters
    lcPortString = UPPER(ALLTRIM(laPrinters[lnPrinter,2]))
    IF LEN(lcPortString) <= lnPortLen
      LOOP 
    ENDIF
    
    *-- Find the port you want
    IF LEFT(lcPortString,lnPortLen) == lcPort2Use + ":,"
      lcMapPort2 = SUBSTR(lcPortString,lnPortLen+1)
      lcMapCommand = [NET USE ] + lcPort2Use + " /DELETE"
      !/N7 &lcMapCommand.    && Delete old mapping.
      
      lcMapCommand = [NET USE ] + lcPort2Use + " " + lcMapPort2
      !/N7 &lcMapCommand.    && Map the Printer
      EXIT 
    ENDIF
  ENDFOR
  SET PRINTER TO NULL
  SET PRINTER TO 
  SET PRINTER TO (lcPort2Use)
ENDFUNC
*-- end of SetPrinterTo.


*!*************************************************************
*! Name      : lfSetGLMsk
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : This function is to load the GL account mask and width
*!						 to variables that will be used in all the screens that
*!             display any GL account.Function to fill the lcRpStatFr variable for the filter.
*!******************************************************************************************
*! 037231,1 HFK 12/22/2003 Function to get the GL Account MAsk
*!********************************************************************************************
FUNCTION lfSetGLMsk
PRIVATE lnAlias

lnAlias    = SELECT()
llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',gcAct_Comp)))
IF llGlLink
  IF lcLinkWith $ "AO"
    lcSelectCommand =[SELECT * FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
		lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
		IF lnResult>=1
		  lcPthToUse = oAriaApplication.DataDir
		  IF !EMPTY(SYCCOMP.cCompPrnt)
				lcSelectCommand=[SELECT * FROM SYCCOMP WHERE Ccomp_id=']+SYCCOMP.cCompPrnt+[']
  			lnResult1= oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
	  		IF lnResult1>=1
	    		lcPthToUse = ALLTRIM(SYCCOMP.cCom_DDir)
				ENDIF
    	ENDIF 
		ENDIF
    USE (lcPthToUse + "ACCOD") IN 0 AGAIN ALIAS CodeStr ORDER AccSegNo
    SELECT CodeStr
    GOTO TOP
    lcRep     = IIF(lcLinkWith = "A", "9", "X")
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    USE IN CodeStr
  ELSE
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , gcAct_Comp))
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', gcAct_Comp))
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    USE (lcAcntStrc) IN 0 AGAIN ALIAS AcntStrc ORDER SegID
    SELECT AcntStrc
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    USE IN AcntStrc
  ENDIF
ENDIF
SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvGLAccnt
*! Developer : HEBA FATHI (HFK)
*! Date      : 12/22/2003
*! Purpose   : This is a global AR function to validate any enterd
*! 						 GL account on any AR screen, it will update the object
*!						 that is called from, and refresh it on the screen.
*!******************************************************
*! 037231,1 HFK 12/22/2003 Function to browse GL Account
*!**************************************************************************

Function lfvGLAccnt
PRIVATE lcEmpty,llEmpty, lcBrFields, lcToGet, lcNoThing
PRIVATE lnAlias,lcObjName , lcObjVal , laRetVal, lcFile_Ttl
lcAcntFld  = SPACE(0)         && Account field that will be validated.
lcAcntDesF = SPACE(0)         && Account Description.
lnAlias    = SELECT()
lcObjName  = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal   = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
lcNoThing  = SPACE(0)
lcAccDscOb = IIF(TYPE("lcAccDscOb")#"C", "lcNoThing", lcAccDscOb)
lcNoThing  = IIF(TYPE("lcRefFunNm")#"C", SPACE(0)   , lcRefFunNm)
llFrmBrow  = IIF(TYPE("llFrmBrow" )#"L", .F.			  , llFrmBrow )
lcEmpty    = STRTRAN(lcAcMask,"9",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"!",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"X",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"#",SPACE(1))
llEmpty    = EMPTY(lcObjVal) OR (lcObjVal = lcEmpty)
lcRefFunNm = IIF(TYPE("lcRefFunNm")#"C", SPACE(0)   , lcRefFunNm)
lnAcLen    = LEN(ALLTRIM(lcAcMask))

llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',gcAct_Comp)))

*!*	lcFile_Ttl = "Chart of Accounts" 
lcFile_Ttl = LANG_Gfopgrid_ChrAcc 
IF llGlLink
  IF lcLinkWith $ "AO"
		lcSelectCommand =[SELECT * FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
		lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
		IF lnResult>=1
		  lcPthToUse = oAriaApplication.DataDir
		  IF !EMPTY(SYCCOMP.cCompPrnt)
				lcSelectCommand=[SELECT * FROM SYCCOMP WHERE Ccomp_id=']+SYCCOMP.cCompPrnt+[']
  			lnResult1= oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
	  		IF lnResult1>=1
	    		lcPthToUse = ALLTRIM(SYCCOMP.cCom_DDir)
				ENDIF
    	ENDIF 
		ENDIF
		USE (lcPthToUse + "GLACCHAR")IN 0 AGAIN ORDER ACCTCODE 
 		IF llFrmBrow .OR. ((!llEmpty) .AND. !SEEK(lcObjVal,'GLACCHAR')) 
			*--lcBrFields =[cAcctCode:H='Account Code',cAccNlDes:H='Description']
			lcBrFields =[cAcctCode:H=LANG_Gfopgrid_AccCode,cAccNlDes:H=LANG_Gfopgrid_Desc ]
	   	lcAcntFld  = "cAcctCode"
	 		lcAcntDesF = "cAccNlDes"
			SELECT GLACCHAR
  	  LOCATE 
   	  DECLARE laRetVal[2]
   		llFrmBrow  = .F.
  		lcToGet    = lcAcntFld + "," + lcAcntDesF
    	IF gfBrows('',lcToGet,'laRetVal',lcFile_Ttl)
  		  &lcObjName  = laRetVal[1]
    		&lcAccDscOb = laRetVal[2]
	  	ELSE
	    	&lcObjName = SPACE(lnAcLen)
	    	&lcAccDscOb = SPACE(0)
	  	ENDIF
			IF !EMPTY(lcRefFunNm)
        DO &lcRefFunNm
  		ENDIF
		  LcRpGlAcc = &lcObjName
			USE IN GLACCHAR
			SELECT(lnAlias)
		ENDIF
  ELSE
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , gcAct_Comp))
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', gcAct_Comp))
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
   	IF !USED("lcLinkChar")
   	  USE (lcAcntChrt) IN 0 ORDER GlAcnt ALIAS lcLinkChar  
	  ENDIF
	  IF llFrmBrow .OR. ((!llEmpty) .AND. !SEEK(lcObjVal,'lcLinkChar')) 
			*!*lcBrFields =[glAcnt+:H='Account Code',glDesc:H='Description']
  		lcBrFields =[glAcnt:H=LANG_Gfopgrid_AccCode,glDesc:H=LANG_Gfopgrid_Desc ]
			lcAcntFld  = "glAcnt"
  	  lcAcntDesF = "glDesc"
  		SELECT GLACCHAR
  	  LOCATE 
   	  DECLARE laRetVal[2]
   		llFrmBrow  = .F.
  		lcToGet    = lcAcntFld + "," + lcAcntDesF
    	IF gfBrows('',lcToGet,'laRetVal',lcFile_Ttl)
  		  &lcObjName  = laRetVal[1]
    		&lcAccDscOb = laRetVal[2]
	  	ELSE
	    	&lcObjName = SPACE(lnAcLen)
	    	&lcAccDscOb = SPACE(0)
	  	ENDIF
		  IF !EMPTY(lcRefFunNm)
        DO &lcRefFunNm
  		ENDIF
		  LcRpGlAcc = &lcObjName
			USE IN GLACCHAR
			SELECT(lnAlias)
  	ENDIF
  ENDIF
ENDIF

*!*****************************************************************************************
*! Name      : lfChPprSz
*! Developer : SMM 
*! Date      : 07/28/2004
*! Purpose   : Adjust the Paper size of a form according to the Setting (Use A4) - 
*!			   to Letter or A4
*! Entry no. : N038249,1 Missing function
*!*****************************************************************************************
*! Parameters: 
*!   lcOGTmpForm: Form name

*!*****************************************************************************************
FUNCTION lfChPprSz
PARAMETERS lcOGTmpForm

IF TYPE("lcOGTmpForm") # 'C' .OR. ISNULL(lcOGTmpForm) .OR. EMPTY(lcOGTmpForm)
  RETURN
ENDIF

lcOGTmpForm = ALLTRIM(lcOGTmpForm)

LOCAL llUSePapA4
*-- Don't print all UK on A4 papper, add seting to print on A4 Paper. [Begin]
llUSePapA4 = gfGetMemVar('M_LLUSEPA4')
llUSePapA4 = IIF(TYPE('llUSePapA4') = 'C', .T., llUSePapA4)

IF ALLTRIM(oAriaApplication.DefaultCountry) == "ENG" .AND. llUSePapA4
  LOCAL llUsLcOg ,lcAChPpAls, lnRecPPr, lcSaveErr   
  llUsLcOg =.F.
  IF !USED(lcOGTmpForm)
    *-- Disable error handler before use this file and then restore it again
    lcSaveErr = ON('ERROR')
    ON ERROR
   
	*-- Fix bug file does not exist
    IF FILE(oAriaApplication.WorkDir + lcOGTmpForm + '.FRX')
      USE(oAriaApplication.WorkDir  + lcOGTmpForm + '.FRX') IN 0
    ELSE
      IF FILE(oAriaApplication.WorkDir + lcOGTmpForm + '.LBX')
        USE(oAriaApplication.WorkDir + lcOGTmpForm + '.LBX') IN 0
      ELSE
        RETURN
      ENDIF
    ENDIF
   
    ON ERROR &lcSaveErr.

    llUsLcOg = .T.  
  ELSE
    lnRecPPr = RECNO(lcOGTmpForm)  
  ENDIF  
  lcAChPpAls = ALIAS()
    
  IF lcOGPlatForm = "WINDOWS" 
    SELECT (lcOGTmpForm)

	*-- Get the correct record to change page size
    LOCATE FOR OBJTYPE = 1 .AND. OBJCODE = 53 AND PLATFORM = "WINDOWS" .AND. ASC(SUBSTR(tag2, 47, 47)) = 1
    IF FOUND()
      REPLACE TAG2 WITH stuff(TAG2, 47, 1, CHR(9))
    ENDIF
    
  ENDIF
  IF !EMPTY(lcAChPpAls)
    SELECT (lcAChPpAls)
  ENDIF  
   
  IF llUslcOg   
    USE IN (lcOGTmpForm)
  ELSE
    IF BETWEEN(lnRecPPr,1,RECNO(lcOGTmpForm))
      GOTO lnRecPPr IN (lcOGTmpForm)  
    ENDIF  
  ENDIF
ENDIF   


*B608106,1 SSH 5-31-2007 Indicate that we click on printer button.
*!*****************************************************************************************
*! Name      : gf2Printer
*! Developer : SSH - Ahmed Salah shalaby
*! Date      : 5-31-2007
*! Purpose   : Set printer to a specified port
*! Entry no. : B608106,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: lcPort2Use - Printer Port.
*!****************************************************************************************
*!
FUNCTION gf2Printer

loOGScroll.ll2printer = .T.

*B608106,1 SSH 5-31-2007 Indicate that we click on printer button.
*!*****************************************************************************************
*! Name      : SetPrinterTo
*! Developer : SSH - Ahmed Salah Shalaby
*! Date      : 5-31-2007
*! Purpose   : Set printer to a specified port
*! Entry no. : B608106,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: lcPort2Use - Printer Port.
*!****************************************************************************************
*!
FUNCTION gfFrx2Printer
*B608106,1 SSH 5-31-2007 Indicate that we click on printer button.
loOGScroll.ll2printer = (SYS(2040)='2')

* MAH T20080413.0001 10/15/2008 BEGIN
*!*	FUNCTION gfGetOptionGridVars
*!*	RETURN ""

*!*	FUNCTION gfInitOptionGridVars
* MAH T20080413.0001 10/15/2008 END