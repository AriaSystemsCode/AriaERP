*:************************************************************************
*:  Program File: ARIA4XP\PRGS\PW\PWSHIFT.FXP
*:  Module      : PIECE WORK
*:  Desc.       : Shifts Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/24/2012
*:  Reference   : E303113,1   ( SQL system files : E303118.exe )
*:              :             ( FOX system files : E303132.exe )
*:************************************************************************
* Modifications
*B609978,1 TMI 25/06/2012 Allow shift to save shift range from 12:00 am to 1:00 am , it currently does not [T20120624.0001] 
*B610064,1 TMI 08/29/2012 Can not save time from 12:00 pm to 05:00pm in shift scr
*:************************************************************************


lcProg = JUSTSTEM(SYS(16))

*- Call the screen
lcRunScx = lfGetScx("PW\&lcProg..scx")
DO FORM (lcRunScx) 

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
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
*! Date      : 04/24/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet
lcProg = JUSTSTEM(SYS(16))
loFormSet.AddProperty('lcProgName',lcProg)

lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'PW\PWGLB.FXP') ADDITIVE &&  all these functions will be copied to the ARIAGLB later


*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)


*** Load program base file 
loFormSet.AddProperty('lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "SQL"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CSHIFT_ID"
  .cBrowseIndexFields     = "CSHIFT_ID"
  .cBrowseIndexName       = "PESHIFT"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	  = ALLTRIM(sydObjct.CPRGLNAME)
  *.AriaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea,oAriaApplication.cAria4SysPath)
  .AriaBrFields.edtBrowseFields.Value = "CSHIFT_ID  :H='Shift ID',"+;
                                        "CDESC      :H='Description',"+;
                                        "CPLANT_ID  :H='Plant ID',"+;
                                        "CSHIFT_STR :H='Start Time',"+;
                                        "CSHIFT_FNS :H='Finish Time'"
ENDWITH 


*- set the input mask of the keyfield 
WITH loFormSet
  .Ariaform1.AriaKeyField1.KeyTextbox.InputMask = REPLICATE('!',FSIZE(.cBrowseIndexFields,.lcBaseFile))
  .Ariaform1.txtDesc.MaxLength = FSIZE('CDESC',.lcBaseFile)
ENDWITH   

loFormset.HasMemo = .F.

*- Define the Plant popup source 
loFormSet.AddProperty('laPlant[1,2]','')
SELECT PEPLANT
gfSeek('')
SELECT CPNAME,CPLANT_ID FROM PEPLANT INTO ARRAY loFormSet.laPlant
loFormset.Ariaform1.cboPlant.RowSource = 'Thisformset.laPlant'
lfColumnWidthes(loFormset.Ariaform1.cboPlant)

loFormset.ChangeMode('S')

*- End of lfFormInit.


************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/14/2012
*! Purpose   : Form Activate
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate.

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
*! Purpose   : Change Mode
************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

IF TYPE('loFormSet.lcBaseFile')='U'
  RETURN 
ENDIF   

loFormSet.Ariaform1.AriaKeyField1.Enabled = .F.

DO CASE 
CASE loFormSet.ActiveMode = 'S'
  WITH loFormSet.Ariaform1
    .AriaKeyField1.Enabled = .T.
    .AriaKeyField1.KeyTextbox.Setfocus()
    .spnStartTime.Olecontrol1.Object.Value  = DTOT(DATE())
    .spnFinishTime.Olecontrol1.Object.Value = DTOT(DATE())
  ENDWITH 
  

CASE loFormSet.ActiveMode = 'V'
    SELECT PESHIFT 
    lcID = PESHIFT.CSHIFT_ID
    =gfSeek(lcID,'PESHIFT')
    WITH loFormset.Ariaform1
      .AriaKeyField1.Keytextbox.Value = CSHIFT_ID
      .txtDesc.Value =     CDESC      
      .cboPlant.Value = CPLANT_ID  
      .spnStartTime.Olecontrol1.Object.Value  = IIF(EMPTY(CTOT(CSHIFT_STR)),DATE(),CTOT(CSHIFT_STR))
      .spnFinishTime.Olecontrol1.Object.Value = IIF(EMPTY(CTOT(CSHIFT_FNS)),DATE(),CTOT(CSHIFT_FNS))
    ENDWITH 

CASE loFormSet.ActiveMode = 'E'

CASE loFormSet.ActiveMode = 'A'

ENDCASE 
SELECT (lnSlct)

*- End of lfChangeMode.


************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
*! Purpose   : Save process
************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT (loFormset.lcBaseFile)
IF loFormSet.ActiveMode = 'A'
  APPEND BLANK 
ENDIF

gfAdd_Info(loFormset.lcBaseFile)
WITH loFormset.Ariaform1
  lcSTime = TTOC(.spnStartTime.Olecontrol1.Object.Value)
  lnPos = AT(' ',lcSTime)+1  
  *lcSTime = SUBSTR(lcSTime,lnPos,5)+" "+RIGHT(lcSTime,2)
  lcSTime = SUBSTR(lcSTime,lnPos)
  
  lcFTime = TTOC(.spnFinishTime.Olecontrol1.Object.Value )
  *lcFTime = SUBSTR(lcFTime,lnPos,5)+" "+RIGHT(lcFTime,2)
  lcFTime = SUBSTR(lcFTime,lnPos)
  REPLACE CSHIFT_ID  WITH .AriaKeyField1.Keytextbox.Value ;
          CDESC      WITH .txtDesc.Value ;
          CPLANT_ID  WITH .cboPlant.Value ;
          CSHIFT_STR WITH lcSTime ;
          CSHIFT_FNS WITH lcFTime
ENDWITH 
=gfReplace('')
=gfTableUpdate()

SELECT (lnSlct)

*- End of lfFormSavefiles.
************************************************************
*! Name      : lfFormDelete
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
*! Purpose   : lfFormDelete
************************************************************
FUNCTION lfFormDelete
PARAMETERS loFormSet
LOCAL lnSlct
lnSlct = SELECT(0)

llDel = .T.
llDel = llDel AND lfChkPlantID_InFile('EMPSHIFT','Shift is assigned to employee(s), cannot delete.')

IF llDel 
  SELECT (loFormset.lcBaseFile)
  gfDelete()
  gfTableUpdate()
ELSE
  SELECT(lnSlct)
  RETURN .F.
ENDIF   

SELECT(lnSlct)

*- End of lfFormDelete.

************************************************************
*! Name      : lfChkPlantID_InFile
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 06/05/2012
*! Purpose   : lfChkPlantID_InFile
************************************************************
FUNCTION lfChkPlantID_InFile
PARAMETERS lcFile,lcMsg
SELECT &lcFile
gfSeek('')
LOCATE FOR CSHIFT_ID = loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value 
IF FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
  RETURN .F.
ENDIF   

*- End of lfChkPlantID_InFile.

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
*! Purpose   : Before Save
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet

WITH loFormset.Ariaform1
  IF EMPTY(.txtDesc.Value)
    =gfModalGen('INM38267B00000','DIALOG','Shift Name field')
    .txtDesc.SetFocus()
    RETURN .F.
  ENDIF 
  
  IF EMPTY(.cboPlant.Value)
    =gfModalGen('INM38267B00000','DIALOG','Plant ID')
    .cboPlant.Setfocus()
    RETURN .F.
  ENDIF 

  lcSTime = TTOC(.spnStartTime.Olecontrol1.Object.Value)
  lnPos = AT(' ',lcSTime)+1  
  *lcSTime = SUBSTR(lcSTime,lnPos,5)+" "+RIGHT(lcSTime,2)
  lcFTime = TTOC(.spnFinishTime.Olecontrol1.Object.Value )
  *lcFTime = SUBSTR(lcFTime,lnPos,5)+" "+RIGHT(lcFTime,2)
  
  *B609978,1 TMI 25/06/2012 [Start] Allow shift to save shift range from 12:00 am to 1:00 am , it currently does not
  lcSTime = SUBSTR(lcSTime,AT(':',lcSTime)-2)
  lcFTime = SUBSTR(lcFTime,AT(':',lcFTime)-2)
  *B610064,1 TMI 08/29/2012 [Start] remove the 12 in the two cases AM and PM
  *lcSTime = IIF(LEFT(lcSTime,2)='12' AND RIGHT(lcSTime,2)='AM','00'+SUBSTR(lcSTime,3),lcSTime)
  lcSTime = IIF(LEFT(lcSTime,2)='12','00'+SUBSTR(lcSTime,3),lcSTime)
  *B610064,1 TMI 08/29/2012 [End  ] 
  *B609978,1 TMI 25/06/2012 [End  ] 
  IF RIGHT(lcSTime,2) = RIGHT(lcFTime,2) AND  lcSTime >= lcFTime 
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Cannot save this shift range')
    .spnStartTime.Olecontrol1.Setfocus()
    RETURN .F.
  ENDIF 

ENDWITH 


*- End of lfFormBeforeSave.

************************************************************
*! Name      : lfvShifts
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/01/2012
*! Purpose   : lfvShifts
************************************************************
FUNCTION lfvShifts
PARAMETERS loFormset,loFld
LOCAL lnSlct
lnSlct = SELECT(0)

WITH loFld.Keytextbox
.Value     = ALLTRIM(.Value)
.Value     = PADR(.Value,LEN(.InputMask))
ENDWITH 
lcFile_Ttl = loFormSet.BrowseTitle
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value 
llView = .F.
lcBaseFile = loFormSet.lcBaseFile
llBrowse = loFld.Selectedfrombrowse 

SELECT (lcBaseFile)
IF llBrowse .OR. !gfSEEK(loFld.KeyTextBox.VALUE) .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
  IF llBrowse .OR. ATC("?",loFld.KeyTextBox.VALUE) > 0  
    IF loFormSet.oToolBar.cmdFind.Click()
      llView = .T.
    ELSE
      loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
    ENDIF
  ELSE
    lnOption  = gfModalGen('QRM00001B00001','Dialog',;
                   +ALLTRIM(loFld.KeyTextBox.VALUE))  
      
    DO CASE
      CASE lnOption = 1
        IF loFormSet.oToolBar.cmdFind.Click()
          llView = .T.
        ELSE
          loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        ENDIF
      CASE lnOption = 2
        loFormSet.CHangeMode('A')
        RETURN
        
      CASE lnOption = 3
        loFld.KeyTextBox.VALUE = loFld.KeyTextBox.OldValue
        RETURN .F.
    ENDCASE
  ENDIF
ELSE
  loFld.KeyTextBox.VALUE = &lcBaseFile..CPLANT_ID
*  loFormSet.CHangeMode('V')
  llView = .T.
ENDIF

IF llView = .T.
  loFormSet.CHangeMode('V')
  =lfChkNav()
ENDIF 

SELECT (lnSlct)

*- End of lfvShifts.
