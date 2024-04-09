*:************************************************************************
*:  Program File: ARIA4XP\PRGS\PW\PWRKCTR.FXP
*:  Module      : PIECE WORK
*:  Desc.       : Work Centers Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/24/2012
*:  Reference   : E303113,1   ( SQL system files : E303118.exe )
*:              :             ( FOX system files : E303132.exe )
*:************************************************************************

*N000682,1 HES 12/25/2012 Globalization changes[Start]
#INCLUDE R:\Aria4xp\Screens\PW\PWRKCTR.h
*N000682,1 HES 12/25/2012 Globalization changes[End  ]

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
loFormSet.AddProperty('CalendID',0)

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
  .cBrowseIndexExpression = "CWORKCENT"
  .cBrowseIndexFields     = "CWORKCENT"
  .cBrowseIndexName       = "PEWCNTR"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	  = ALLTRIM(sydObjct.CPRGLNAME)
  *.AriaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea,oAriaApplication.cAria4SysPath)  &&( should work but needs to be traced )
  .AriaBrFields.edtBrowseFields.Value = "CWORKCENT :H='Work Center ID',"+;
                                        "CDESC     :H='Work Center Name',"+;
                                        "CPLANT_ID :H='Plant ID',"+;
                                        "CWCNTR_BS :H='Capacity Based',"+;
                                        "CWCNTR_AVL:H='Available Number',"+;
                                        "CWCNTR_LOD:H='Load',"+;
                                        "CWCNTR_TYP:H='Work Center Type',"+;
                                        "NCALID    :H='Calender ID ',"+;
                                        "NMAN_MCH  :H='Man Per Machine',"+;
                                        "NOVR_RATE :H='Overhead Rate',"+;
                                        "NSTAND_RAT:H='Standard Rate'"

ENDWITH 

*- set the input mask of the keyfield 
WITH loFormSet
  .Ariaform1.AriaKeyField1.KeyTextbox.InputMask = REPLICATE('!',FSIZE(.cBrowseIndexFields,.lcBaseFile))
  .Ariaform1.txtDesc.MaxLength = FSIZE('CDESC',.lcBaseFile)
ENDWITH   

WITH loFormset
  DIMENSION .laPanelObj[1,6]
  *--Object link & style picture.
  .laPanelObj[1,1]= "cmdPicture"
  .laPanelObj[1,2]= oAriaApplication.BitmapHome+"RELATE.BMP"
  .laPanelObj[1,3]= "mvObjLink"
  .laPanelObj[1,4]= "Object Links"
  .laPanelObj[1,5]= "Object Links"
  .laPanelObj[1,6]= "VAE"
  .AddProperty('lcObjType',"H")
ENDWITH
*!*	"B" Plant
*!*	"H" work center
*!*	"E" Employee

loFormset.HasMemo = .F.

*WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1.Thumbnail1
WITH loFormset.Ariaform1.cntThumbnail
  .cType = "B"
  .cObjectKey = ""  
ENDWITH   

=lfCalender(loFormSet,loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1,'W')

*- Define the Plant popup source 
loFormSet.AddProperty('laPlant[1,2]','')
SELECT PEPLANT
gfSeek('')
SELECT CPNAME,CPLANT_ID FROM PEPLANT INTO ARRAY loFormSet.laPlant
o = loFormset.Ariaform1.cboPlant
o.RowSource = 'Thisformset.laPlant'
lfColumnWidthes(o)

*- Define the Type popup source 
loFormSet.AddProperty('laType[2,2]','')
loFormSet.laType[1,1] = 'Actual'
loFormSet.laType[1,2] = 'A'
loFormSet.laType[2,1] = 'Forecast'
loFormSet.laType[2,2] = 'F'
o = loFormset.Ariaform1.cboType
o.RowSource = 'Thisformset.laType'
lfColumnWidthes(o)

*- Define the "Capacity Based on" popup source 
loFormSet.AddProperty('laCapacityBased[2,2]','')
loFormSet.laCapacityBased[1,1] = 'Humans'
loFormSet.laCapacityBased[1,2] = 'H'
loFormSet.laCapacityBased[2,1] = 'Machines'
loFormSet.laCapacityBased[2,2] = 'M'
o = loFormset.Ariaform1.Ariapageframe1.Ariapage1.cboCapacityBased
o.RowSource = 'Thisformset.laCapacityBased'
lfColumnWidthes(o)

*- Define Human grid
lfgrdHuman(loformset)

*- Define Machines grid
lfgrdMachines(loFormSet)


WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
    .spnAvailableNumber.SpinnerHighValue = 999999 
    .spnAvailableNumber.SpinnerLowValue = 0

    .spnLoad.SpinnerHighValue = 99
    .spnLoad.SpinnerLowValue = 0

    .spnManPerMachine.SpinnerHighValue = 99.99   
    .spnManPerMachine.SpinnerLowValue = 0

    .spnOverheadRate.SpinnerHighValue = 99.99  
    .spnOverheadRate.SpinnerLowValue = 0

    .spnStandardRate.SpinnerHighValue = 99.99 
    .spnStandardRate.SpinnerLowValue = 0
    *E303114,1 TMI 31/05/2012 [Start] set input mask
    .spnAvailableNumber.InputMask = "999999"
    .spnLoad.InputMask = '99'
    .spnManPerMachine.InputMask = '99.99'
    .spnOverheadRate.InputMask = '99.99'
    .spnStandardRate.InputMask = '99.99'
    *E303114,1 TMI 31/05/2012 [End  ] 
ENDWITH 

loFormset.ChangeMode('S')

*- End of lfFormInit.



************************************************************
*! Name      : lfgrdHuman
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/05/2012
*! Purpose   : lfgrdHuman
*************************************************************- Define Human grid
FUNCTION lfgrdHuman
PARAMETERS loFormset
WITH loFormset.Ariaform1.Ariapageframe1.Ariapage3.grdHuman
  .RecordSource = ''
  .RecordSource = 'PEPERSON'
  .ColumnCount = 3
  
  .column1.header1.Caption = 'Employee ID'
  .column1.ControlSource = 'PEPERSON.CPERSON_ID'
  .column1.Width = 100  
  
  .column2.header1.Caption = 'Employee Name'
  .column2.ControlSource = 'PEPERSON.CNAME'
  .column2.Width = 200  

  .column3.header1.Caption = 'Cost Center'
  .column3.ControlSource = 'PEPERSON.CCOSTCENT'
  .column3.Width = 100  

  .ReadOnly = .T.
ENDWITH   
*- End of lfgrdHuman.

************************************************************
*! Name      : lfgrdMachines
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/05/2012
*! Purpose   : lfgrdMachines
*************************************************************- Define Machines grid
FUNCTION lfgrdMachines
PARAMETERS loFormset

WITH loFormset.Ariaform1.Ariapageframe1.Ariapage4.grdMachines
  .RecordSource = ''
  .RecordSource = 'PWMACHIN'
  .ColumnCount = 2
  .column1.header1.Caption = 'Machine ID'
  .column1.ControlSource = 'PWMACHIN.CMACHINEID'
  .column1.Width = 200

  .column2.header1.Caption = 'Machine Description'
  .column2.ControlSource = 'PWMACHIN.CDESC'
  .column2.Width = 200
  .ReadOnly = .T.
ENDWITH
*- End of lfgrdMachines.

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

*- Set calender mode
loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.formmode = IIF(loFormset.ActiveMode $ 'AE' , 'E' , 'V' )
loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.grdCalendar.RecordSource = loFormSet.DataFile

DO CASE 
CASE loFormSet.ActiveMode = 'S'
  loFormset.Ariaform1.Ariapageframe1.ActivePage = 1
  loFormSet.Ariaform1.AriaKeyField1.Enabled = .T.
  loFormSet.Ariaform1.AriaKeyField1.KeyTextbox.Setfocus()

*  WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1.Thumbnail1
  WITH loFormset.Ariaform1.cntThumbnail
  .cType = loFormset.lcObjType
  .cObjectKey = ""
  ENDWITH 

  SELECT (loFormSet.DataFile)
  ZAP 
  
  loFormSet.Ariaform1.cboType.Value = ''
  loFormset.Ariaform1.Ariapageframe1.Ariapage3.grdHuman.RecordSource = ''
  loFormset.Ariaform1.Ariapageframe1.Ariapage4.grdMachines.RecordSource = ''
  

CASE loFormSet.ActiveMode = 'V'
    SELECT PEWCNTR
    lcID = PEWCNTR.CWORKCENT 
    =gfSeek(lcID,'PEWCNTR')
    loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value = CWORKCENT 
    *loFormset.Ariaform1.Ariapageframe1.ActivePage = 1    
    
    WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
    loFormset.Ariaform1.AriaKeyField1.KeyTextbox.Value = PEWCNTR.CWORKCENT  
    loFormset.Ariaform1.txtDesc.Value = PEWCNTR.CDESC      
    loFormset.Ariaform1.cboPlant.Value = PEWCNTR.CPLANT_ID  
    loFormset.Ariaform1.cboType.Value = PEWCNTR.CWCNTR_TYP 
    .cboCapacityBased.Value = PEWCNTR.CWCNTR_BS  
    .spnAvailableNumber.Value = PEWCNTR.CWCNTR_AVL 
    .spnLoad.Value = PEWCNTR.CWCNTR_LOD 
    .spnManPerMachine.Value = PEWCNTR.NMAN_MCH   
    .spnOverheadRate.Value = PEWCNTR.NOVR_RATE  
    .spnStandardRate.Value = PEWCNTR.NSTAND_RAT 
    *PEWCNTR.ACCOUNT    WITH '' 
    *PEWCNTR.NCALID     WITH 0
    ENDWITH 
    

*  WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1.Thumbnail1
  WITH loFormset.Ariaform1.cntThumbnail
  .cType = loFormset.lcObjType
  .cObjectKey = PEWCNTR.CWORKCENT 
  ENDWITH 

  SELECT (loFormSet.DataFile)
  ZAP 
  
    lnW = 10

   
    gfSeek( PEWCNTR.CPLANT_ID ,'PEPLANT')
    =gfSeek(STR(PEPLANT.NCALID,lnW),'SCCALDTL')
    SELECT SCCALDTL
    SCAN
      SCATTER MEMVAR MEMO
      m.LLOCK = .T.
      m.crectyp = 'W'
      SELECT (loFormSet.DataFile)
      APPEND BLANK
      GATHER MEMVAR MEMO    
    ENDSCAN   
  
    *=gfSeek(STR(PEWCNTR.NCALID,lnW),'SCCALHDR')
    =gfSeek(STR(PEWCNTR.NCALID,lnW),'SCCALDTL')
    SELECT SCCALDTL
    SCAN
      SCATTER MEMVAR MEMO
      m.LLOCK = .F.      
      SELECT (loFormSet.DataFile)
      APPEND BLANK
      GATHER MEMVAR MEMO    
    ENDSCAN 
    
    IF recc(loFormSet.DataFile)>0
      loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()  
    ENDIF 

  *- Fill humans grid
  =gfSeek(PEWCNTR.CWORKCENT,'PEPERSON')
*!*	  loFormset.Ariaform1.Ariapageframe1.Ariapage3.grdHuman.RecordSource = 'PEPERSON'
*!*	  loFormset.Ariaform1.Ariapageframe1.Ariapage3.grdHuman.Readonly = .T.
  
  =gfSeek(PEWCNTR.CWORKCENT,'PWMACHIN')
*!*	  loFormset.Ariaform1.Ariapageframe1.Ariapage4.grdMachines.RecordSource = 'PWMACHIN'
*!*	  loFormset.Ariaform1.Ariapageframe1.Ariapage4.grdMachines.Readonly = .T.

*- Define Human grid
lfgrdHuman(loformset)

*- Define Machines grid
lfgrdMachines(loFormSet)


  loFormSet.CheckNavigation()
  loFormSet.oToolBar.Navrefresh()

CASE loFormSet.ActiveMode = 'E'
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Enabled = .T.
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()  

CASE loFormSet.ActiveMode = 'A'
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Enabled = .T.
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()  
  loFormSet.Ariaform1.cboType.Value = 'A'

  loFormset.Ariaform1.Ariapageframe1.Ariapage3.grdHuman.RecordSource = ''
  loFormset.Ariaform1.Ariapageframe1.Ariapage4.grdMachines.RecordSource = ''
  
  *E303114,1 TMI 12/05/2012 [Start] 
  WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
    .cboCapacityBased.Value = loFormSet.laCapacityBased[2,2]
    .spnAvailableNumber.Value = 1
    .spnManPerMachine.Value  = 1
  ENDWITH 
  *E303114,1 TMI 12/05/2012 [End  ] 


ENDCASE 
*loFormset.Ariaform1.Ariapageframe1.ActivePage = 1
loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.grdCalendar.Refresh()



loFormset.Ariaform1.Ariapageframe1.Ariapage1.cmdHumans.Enabled = loFormset.ActiveMode $ 'VE' 
loFormset.Ariaform1.Ariapageframe1.Ariapage1.cmdMachines.Enabled = loFormset.ActiveMode $ 'VE' 

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
LOCAL oF,oP
WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
REPLACE CWORKCENT  WITH loFormset.Ariaform1.AriaKeyField1.KeyTextbox.Value ;
        CDESC      WITH loFormset.Ariaform1.txtDesc.Value ;
        CPLANT_ID  WITH loFormset.Ariaform1.cboPlant.Value ;
        CWCNTR_TYP WITH loFormset.Ariaform1.cboType.Value ;
        CWCNTR_BS  WITH .cboCapacityBased.Value ;
        CWCNTR_AVL WITH .spnAvailableNumber.Value ;
        CWCNTR_LOD WITH .spnLoad.Value ;
        NMAN_MCH   WITH .spnManPerMachine.Value ;
        NOVR_RATE  WITH .spnOverheadRate.Value ;
        NSTAND_RAT WITH .spnStandardRate.Value ;
        ACCOUNT    WITH ''
ENDWITH 

lnSeq = PEWCNTR.NCALID
SELECT (loFormSet.DataFile)
SET FILTER TO 
LOCATE
LOCATE FOR !LLOCK
IF FOUND() 
  
  IF lnSeq = 0

    SELECT SCCALHDR
    APPEND BLANK
    *B610136,2 [T20121021.0030] TMI 11/22/2012 [Start] in case that the NCALID does not exist in the SEQUENCE table
    DO WHILE lnSeq = 0
      *B610136,2 [T20121021.0030] TMI 11/22/2012 [End  ] 
      lnSeq = INT(VAL(gfSequence('NCALID')))
      *B610136,2 [T20121021.0030] TMI 11/22/2012 [Start] 
    ENDDO 
    *B610136,2 [T20121021.0030] TMI 11/22/2012 [End  ] 
    REPLACE NCALID WITH lnSeq
    gfAdd_info('SCCALHDR')
    =gfReplace('')
  
    SELECT SCCALHDR
    =gfTableUpdate()
  
    SELECT (loFormset.lcBaseFile)
    REPLACE NCALID WITH lnSeq
  
  ELSE
  
    SELECT SCCALDTL
    gfSeek(STR(lnSeq,10))
    SCAN
      gfDELETE()
    ENDSCAN 
    gfTableUpdate()
    
    
  ENDIF 

  SELECT (loFormSet.DataFile)
  LOCATE 
  SCAN FOR !LLOCK
    SCATTER MEMVAR MEMO
    m.NCALID = lnSeq
    m.Rec_no = ''
    SELECT SCCALDTL
    APPEND BLANK
    GATHER MEMVAR MEMO
    gfAdd_info('SCCALDTL')
  ENDSCAN
  
  SELECT SCCALDTL
  SCAN   
   =gfReplace('')
  ENDSCAN

  SELECT SCCALDTL
  =gfTableUpdate()

ENDIF   
  
SELECT (loFormset.lcBaseFile)
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

*E303114,1 TMI 06/06/2012 [Start] check if key id in other tables
lcID = loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value
lcFld = loFormSet.cBrowseIndexExpression
IF !lfCheckifKeyUsed(lcID, lcFld , 'PEWCNTR ')
  RETURN .F.
ENDIF   
*E303114,1 TMI 06/06/2012 [End  ] 

SELECT (loFormset.lcBaseFile)
gfDelete()
gfTableUpdate()

*E303114,4 TMI 11/06/2012 [Start]   *- Delete the related Calenders
lcBaseFile = loFormSet.lcBaseFile
lfDelCalender(&lcBaseFile..NCALID)
*E303114,4 TMI 11/06/2012 [End  ] 
SELECT(lnSlct)

*- End of lfFormDelete.

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/24/2012
*! Purpose   : Before Save
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet
LOCAL lnSlct,llRet
lnSlct = SELECT(0)

IF EMPTY(loFormset.Ariaform1.cboType.Value)
  =gfModalGen('INM38267B00000','DIALOG','Type field')
  loFormset.Ariaform1.cboType.SetFocus()
  RETURN .F.
ENDIF 
IF EMPTY(loFormset.Ariaform1.txtDesc.Value)
  =gfModalGen('INM38267B00000','DIALOG','Name field')
  loFormset.Ariaform1.txtDesc.SetFocus()
  RETURN .F.
ENDIF 
IF EMPTY(loFormset.Ariaform1.cboPlant.Value)
  =gfModalGen('INM38267B00000','DIALOG','Plant field')
  loFormset.Ariaform1.cboPlant.SetFocus()
  RETURN .F.
ENDIF 

*E303114,1 TMI 31/05/2012 [Start] 
*llRet = lfCalChk(loFormSet,.T.)
loCalGrd = loFormset.Ariaform1.Ariapageframe1.Ariapage2
llRet = lfCalChk(loFormSet,.T.,loCalGrd)
*E303114,1 TMI 31/05/2012 [End  ] 


SELECT (lnSlct)
RETURN llRet
*- End of lfFormBeforeSave.

************************************************************
*! Name      : lfvWorkCenter
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/01/2012
*! Purpose   : lfvWorkCenter
************************************************************
FUNCTION lfvWorkCenter
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
        lfMode('A')
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
  lfMode('V')
  =lfChkNav()
ENDIF 

SELECT (lnSlct)

*- End of lfvWorkCenter.


************************************************************
*! Name      : lfGetBrow
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/05/2012
*! Purpose   : lfGetBrow
************************************************************
FUNCTION lfGetBrow
PARAMETERS lcKeyValue,lcFile
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT (lcFile)
=gfSeek(lcKeyValue,lcFile)
DO case
CASE lcFile = 'PEPERSON'
  lcBrFields = "CPERSON_ID :H='Emp. ID',CNAME     :H='Emp. Name',CWORKCENT :H='Work Center'"

CASE lcFile = 'PWMACHIN'
  lcBrFields = "CMACHINEID :H='Machine ID',CDESC  :H='Machine Description'"
ENDCASE 
=gfBrowse(lcBrFields)
SELECT(lnSlct)
*- End of lfGetBrow.


