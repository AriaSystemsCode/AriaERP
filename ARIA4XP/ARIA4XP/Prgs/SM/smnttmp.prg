*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMNTTMP.Prg
*:  Module      : System Manager 
*: Program desc. : NotePad Templates
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/18/2013
*:  Reference   : *E303355,1 
*:************************************************************************

lcRunScx = lfGetScx("SM\SMNTTMP.scx")
DO FORM (lcRunScx) 

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF   
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet
loFormSet.AddProperty('lcProgName','SMNTTMP')


=lfDefineVars(loFormSet)

*- set row Source for the companies popup
WITH loFormSet.Ariaform1
  .laComp.RowSource = 'Thisformset.laComp'
   
   *- set fields with
   .lcTmpCod.MaxLength = 20
   .lcTmpCod.MaxLength = 35
ENDWITH 
loFormSet.Ariaform1.laComp.OldValue = ' '
loFormSet.Ariaform1.laComp.Value = oAriaApplication.ActiveCompanyID
=lfvPuComp(loFormset,loFormSet.Ariaform1.laComp)

*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/18/2013
*! Purpose   : Define screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.AddProperty('lcScFields','')
loFormSet.AddProperty('lcTmpFl','')
loFormSet.AddProperty('lcPrevComp','')
loFormSet.AddProperty('lcNotTmpFl','')
loFormSet.AddProperty('lcBrow_Ttl','')
loFormSet.AddProperty('lcWindTitl','')
loFormSet.AddProperty('lnArLoop','')
loFormSet.AddProperty('lcCurComp','')
loFormSet.AddProperty('lcOldVal','')
loFormSet.AddProperty('lcKeyType','')

STORE 0 TO loFormSet.Ariaform1.laComp.ListIndex
loFormSet.AddProperty('lcTmpStat' , 'DISABLE')

loFormSet.AddProperty('laComp[1]')

*-- Holds the save button to use lpSavScr Procdure
*-- Create temporary file
=lfCrTmpFl(loFormSet)

*-- fields list to be scattered to LADATA array
*lcScFields = 'Capp_id,Cdlobjtyp,Cdlobjid,Mdlobj'
*-- lcKeyType = 'T' For templates.
*-- lcKeyType = 'X' For Account General purpose letters.
*-- lcKeyType = 'Y' For  Vendor General purpose letters.
IF (TYPE('lcKeyType') $ 'UL')
  loFormSet.lcKeyType = 'T'
ENDIF

*-- lcKeyType = 'T' For templates.
*-- lcKeyType = 'X' For Account General purpose letters.
*-- lcKeyType = 'Y' For  Vendor General purpose letters.
*-- Fill viarables according to the paramter.
*lcBrow_Ttl = 'Templates'
*lcWindTitl = 'Notepad Templates'   

loFormSet.lcBrow_Ttl = IIF(loFormSet.lcKeyType = 'T','Templates','General Purpose Letters')
DO CASE
  CASE loFormSet.lcKeyType = 'T'
    loFormSet.lcWindTitl = 'Notepad Templates'   
  CASE loFormSet.lcKeyType = 'X'
    loFormSet.lcWindTitl = 'Customer General Purpose Letters'
  CASE loFormSet.lcKeyType = 'Y'
    loFormSet.lcWindTitl = 'Vendor General Purpose Letters' 
ENDCASE

loFormSet.Ariaform1.Caption = loFormSet.lcWindTitl

*-- The first time we run the screen, 
  IF !lfMakArry()  
    RETURN .F.
  ENDIF  
  *-- Initialize some variables
  *-- Create window names
  loFormSet.lcNotTmpFl = gfTempName()  
    llNoShow  = .F.

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet  
oGrd = loFormSet.Ariaform1.grdSMNTTMP
WITH oGrd
  .RecordSource = ''
  .RecordSource = loFormSet.lcNotTmpFl
ENDWITH

lcNotTmpFl = loFormSet.lcNotTmpFl
lfSetColumnsProp('1',"&lcNotTmpFl..Key",'Template'       ,200 ,oGrd)
lfSetColumnsProp('2',"&lcNotTmpFl..cDesc","Description"  ,300 ,oGrd)
oGrd.ReadOnly = .T.

SELECT (loFormSet.lcNotTmpFl)
lcKeyType = loFormSet.lcKeyType
lcFilt = "TYPE = '&lcKeyType'"
SET FILTER TO &lcFilt

*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS lcCol,lcSrc,lcHeader,lnWidth,loGrd
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH loGrd
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH 
*- End of lfSetColumnsProp.

************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/20/2013
*! Purpose   : grid interactive 
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loFormSet
lcNotTmpFl = loFormSet.lcNotTmpFl
WITH loFormset.AriaForm1
  .lcTmpCod.Value = &lcNotTmpFl..Key
  .lcTmpDesc.Value = &lcNotTmpFl..cDesc
  .Refresh()
ENDWITH   

************************************************************
*! Name      : lpSavScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/23/2013
*! Purpose   : Save function 
*************************************************************--------------------------------------------------------------------------*
FUNCTION lpSavScr
PARAMETERS loFormSet
*N000682,,1 TMI 04/23/2013 [Start] define lnSlct
LOCAL lnSlct
lnSlct = sele(0)
*N000682,1 TMI 04/23/2013 [End  ]

SELECT (loFormSet.lcNotTmpFl)
TABLEUPDATE(.T.)
SELECT(lnSlct)
*- End of lpSavScr.


************************************************************
*! Name      : lpShow
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/23/2013
*! Purpose   : mode change function
*************************************************************--------------------------------------------------------------------------*
FUNCTION lpShow
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN 
ENDIF 

WITH loFormSet.Ariaform1
  .pbNew.Enabled = .F.
  .pbRemove.Enabled = .F.
  .pbNotes.Enabled = .F.
  .lcTmpCod.Enabled = .F.
  .lcTmpDesc.Enabled = .F.
ENDWITH 

DO CASE
  *-- If select mode
  CASE loFormSet.ActiveMode = 'S'
    loFormSet.Ariaform1.grdSMNTTMP.RecordSource = ''

    SELECT (loFormSet.lcTmpFl)
    ZAP
    WITH loFormSet.Ariaform1
      .laComp.ListIndex = 0
      .laComp.Enabled = .T.
      .laComp.SetFocus()
      KEYBOARD '{ALT+DNARROW}'
    
      .lcTmpCod.Value = SPACE(20)
      .lcTmpCod.Value=SPACE(35)
    
      .pbNew.Enabled = .F.
      .pbRemove.Enabled = .F.
      .pbNotes.Enabled = .F.
      .lcTmpDesc.Enabled = .F.
    ENDWITH 

 *-- If view mode
  CASE loFormSet.ActiveMode = 'V'
   
    =lfSetGridDataSource(loFormSet)

    LOCATE 
    =lfFormAfterRowColumnChange(loFormSet)
    
    WITH loFormSet.Ariaform1
      .lcTmpCod.Enabled = .F.   && DISABLE

      .pbNotes.Enabled = .T.
    ENDWITH 
  
  CASE loFormSet.ActiveMode = 'E'
    WITH loFormSet.Ariaform1
      .laComp.Enabled = .F.
      .pbNew.Enabled = .T.
      .pbRemove.Enabled = .T.
      .lcTmpDesc.Enabled = .T.
    ENDWITH   
ENDCASE
*- End of lpShow.

*!*************************************************************
*! Name      : lfNew
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Adding a New Record
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfNew()
*!*************************************************************

FUNCTION lfvNew
PARAMETERS loFormSet
*--Allows the addition of a new record in the notepad file 	
*-------------------------------------------------------------------------*
WITH loFormSet.Ariaform1
  .lcTmpCod.Value  = SPACE(20)
  .lcTmpCod.Value = SPACE(35)
  .lcTmpCod.Enabled = .T.
  .lcTmpCod.SetFocus()

  .lcTmpDesc.Enabled = .F.
  .lcTmpDesc.Value = ''
ENDWITH   

*!*************************************************************
*! Name      : lfRemove
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Deleting the current record
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   : lfRemove()
*!*************************************************************

FUNCTION lfvRemove
PARAMETERS loFormSet
*--Allows the deletion of a current selected record in the notepad file 	
*-------------------------------------------------------------------------*
IF gfModalGen("QRM00007B00007","ALERT") = 1
*-- Marks records as deleted when pressing remove button
 IF !EOF()
   DELETE
   SKIP
   IF EOF()
     LOCATE 
   ENDIF 
   *-- Clears the data in two variables 
   lcNotTmpFl = loFormSet.lcNotTmpFl
   loFormSet.Ariaform1.lcTmpCod.Value  = &lcNotTmpFl..key
   loFormSet.Ariaform1.lcTmpDesc.Value = &lcNotTmpFl..cdesc
   loFormset.AriaForm1.grdSMNTTMP.Refresh()
 ENDIF 
ENDIF
*-- IF KEY IS EMPTY (BROWSE WINDOW IS EMPTY)
IF EMPTY(key)
  lcTmpStat = 'DISABLE'
  loFormSet.Ariaform1.pbRemove.Enabled = .F.
  loFormSet.Ariaform1.pbNotes.Enabled = .F.
  loFormSet.Ariaform1.lcTmpDesc.Enabled = .F.
ENDIF    

*-------------------------------------------------------------------------*
*!*************************************************************
*! Name      : lfCrTmpFl
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Creates an Empty Template File
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfCrTmpFl()
*!*************************************************************

FUNCTION lfCrTmpFl
PARAMETERS loFormSet
*-->> When entering the prog
*-- Create a temp File in the work dir(oAriaApplication.WorkDir)	
*--Using the global function gfTempName 
*-- Creates an index for the file
*-- Sets the table order to this index 
loFormSet.lcTmpFl = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lcTmpFl) (TYPE C(1),;
       KEY C(20),CDESC C(35),MNOTES M(10))
INDEX ON TYPE+KEY TAG (loFormSet.lcTmpFl) ADDITIVE
SET ORDER TO TAG (loFormSet.lcTmpFl)  
  
*-------------------------------------------------------------------------*    
*!*************************************************************
*! Name      : lfDlTmpFl
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Deletes Template file,its(.CDX) & (.FPT) Files
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfDlTmpFl()
*!*************************************************************

 FUNCTION lfDlTmpFl
 PARAMETERS loFormSet
 *-->> When quiting the prog
 *--Closes temp file to delete it before quitting the system
 *-- Delete also the index(.CDX) file
 *-- Delete memo file (.FPT) file
IF USED (loFormSet.lcTmpFl)
  USE IN (loFormSet.lcTmpFl) 
ENDIF
ERASE (oAriaApplication.WorkDir + loFormSet.lcTmpFl + '.DBF')
ERASE (oAriaApplication.WorkDir + loFormSet.lcTmpFl + '.FPT')
ERASE (oAriaApplication.WorkDir + loFormSet.lcTmpFl + '.CDX')  
  
*-------------------------------------------------------------------------*    
*!*************************************************************
*! Name      : lfMakArry
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Makes the Array to fill the Company PopUp with.
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfMakArry()
*!*************************************************************
FUNCTION lfMakArry
*-- Opens the file needed to fill our array (laComp) with 3 fields from it
*-- Then using this array (laComp) to fill the puComp popup 
PRIVATE lnCurAlias, llRetVal
lnCurAlias = SELECT(0)   && save current work ara
=gfOpenFile(oAriaApplication.SysPath+'SYccomp', 'Ccomp_ID', 'SH')
SELECT 	Ccomp_Id + " - " + Ccom_Name,Ccom_Ddir,cComp_ID ;
  FROM 	SYccomp      ;
  INTO 	ARRAY loFormSet.laComp ;
 ORDER  BY Ccom_Name
IF _TALLY = 0
  *-- Message 
  *** No companies available. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00189B00000","DIALOG","companies")
  llRetVal = .F.
ELSE
  llRetVal = .T.
ENDIF  
SELECT (lnCurAlias)
RETURN llRetVal

*!*************************************************************
*! Name      : lfvPuComp
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Checks the value of the Company PopUp
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfvPuComp()
*!*************************************************************
FUNCTION lfvPuComp
PARAMETERS loFormSet,loFld
*-- Checks that selected company is different then the last one & not("Select a Company")
*IF lcPrevComp <> loFormSet.laComp[puComp,1] AND puComp <> 1
IF !EMPTY(loFld.Value) AND !(loFld.Value == loFld.OldValue)
  =lfOpenDbf(loFormSet)
ENDIF
*!*************************************************************
*! Name      : lfOpenDbf
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Opens the Dbf of the Selected Company
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfOpenDbf
PARAMETERS loFormSet

*-- Passes to a variable the directory of the dbfs of 
*--The selected company file name (NOTEPAD)
IF loFormSet.Ariaform1.laComp.ListIndex>0
  lcNtPth  = gfGetDataDir(ALLTRIM (loFormSet.laComp[loFormSet.Ariaform1.laComp.ListIndex,2])) +"NotePad"
  IF USED(loFormSet.lcNotTmpFl)
    USE IN (loFormSet.lcNotTmpFl)
  ENDIF
  lcNotTmpFl = loFormSet.lcNotTmpFl
  =gfOpenFile(lcNtPth, "NOTEPAD", "SH", @lcNotTmpFl, .T.)

  SELECT (lcNotTmpFl)
  CURSORSETPROP("Buffering",5)
  
  loFormSet.ChangeMode('V')
ENDIF 
*!*	*---------------------------------------------------------------------------*
*!*	FUNCTION lfwCoName
*!*	*-- passses the value of the previously selected company to variable(lcPrevComp)
*!*	lcPrevComp = loFormSet.laComp[puComp,1]
*!*************************************************************
*! Name      : lfvDesc
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Updates the cDesc field in NotePad file.
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvDesc
PARAMETERS loFormSet,loFld
PRIVATE lnCurAlias
*IF loFormSet.Ariaform1.lcTmpCod.Value <> lcOldVal

IF !(loFld.Value == loFld.OldValue)
  loFormSet.Ariaform1.lcTmpDesc.Value = PADR(ALLTRIM(loFormSet.Ariaform1.lcTmpDesc.Value), 35)
  *-- When quitting the description variable
  lnCurAlias = SELECT(0)
  SELECT (loFormSet.lcNotTmpFl)
  REPLACE cDesc WITH loFormSet.Ariaform1.lcTmpDesc.Value
  loFormset.AriaForm1.grdSMNTTMP.Refresh()
  SELECT (lnCurAlias)
ENDIF
 
*---------------------------------------------------------------------------* 
*!*************************************************************
*! Name      : lfvNotes
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Opens the NotePad prg with the Memos of the selected Record
*!*************************************************************
*! Calls     :  NotePad Prg
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvNotes
PARAMETERS loFormSet

IF EMPTY(loFormSet.Ariaform1.lcTmpCod.Value)
  RETURN 
ENDIF   

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

lcNotTmpFl = loFormSet.lcNotTmpFl

lcNotes = &lcNotTmpFl..MNOTES    

lcToDo = oAriaApplication.ScreenHome + "SY\NOTEPAD.SCX"

lcKey = loFormSet.Ariaform1.lcTmpCod.Value
DO FORM (lcToDo) WITH loFormSet.lcKeyType, lcKey,,lcNotes
SELECT (lnCurAlias)
*---------------------------------------------------------------------------* 

*!*************************************************************
*! Function  : lfViewRLog
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/05/2005
*! Purpose   : Preview rebalance log file (ReBalLog.txt).
*!*************************************************************
FUNCTION lfViewRLog

LOCAL lnAlias
lnAlias = SELECT(0)

  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (oAriaApplication.WorkDir + 'ReBalLog.txt') OVERWRITE
  SCATTER MEMVAR MEMO
  APPEND BLANK
  GATHER MEMVAR MEMO 
  LOCATE
  
  REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
                       LANG_SMREBAL_REBVERI + CHR(13) +;
                       REPLICATE('*',68) + CHR(13) + ' ' + CHR(13) + ' ' +;
                       mStrRep

  DO FORM (oAriaApplication.ScreenHome + 'SM\SMSTRREP')
  USE IN TMPSTR
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvTmpCod
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Validates the Key of the newly added records
*!*************************************************************
*! Calls     :  None
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvTmpCod
PARAMETERS loFormSet,loFld

loFormSet.Ariaform1.lcTmpCod.Value = PADR(ALLTRIM(loFormSet.Ariaform1.lcTmpCod.Value), 20)
IF !EMPTY(loFormSet.Ariaform1.lcTmpCod.Value)
  *-- Validates key   
  IF !SEEK(loFormSet.lcKeyType+loFormSet.Ariaform1.lcTmpCod.Value)
    *-- Inserts the newly added records in the temp file 
    INSERT INTO (loFormSet.lcNotTmpFl);
     	    (TYPE, Key);
  	  VALUES(loFormSet.lcKeyType, loFormSet.Ariaform1.lcTmpCod.Value)
  	  
    loFormSet.Ariaform1.lcTmpCod.Enabled = .F.
      lcTmpStat = 'ENABLE'
      loFormSet.Ariaform1.pbRemove.Enabled = .T.
      loFormSet.Ariaform1.pbNotes.Enabled = .T.
      loFormSet.Ariaform1.lcTmpDesc.Enabled = .T.
      loFormSet.Ariaform1.grdSMNTTMP.Refresh()
  ELSE 
*--Message terminate an already existing key
   =gfModalGen("TRM00352B00000","DIALOG",ALLTRIM(loFormSet.lcKeyType))
 *--Empty the Code & Descrip.
    loFormSet.Ariaform1.lcTmpCod.Value = SPACE(20)
    loFormSet.Ariaform1.lcTmpCod.Value=KEY
  ENDIF
ELSE
 
ENDIF   