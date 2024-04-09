*:************************************************************************
*:  Program File: ARIA4XP\PRGS\PW\PWPLANT.FXP
*:  Module      : PIECE WORK
*:  Desc.       : Plants Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/24/2012
*:  Reference   : E303113,1   ( SQL system files : E303118.exe )
*:              :             ( FOX system files : E303132.exe )
*:************************************************************************
* Modifications
*B610136,2 [T20121021.0030] TMI 11/22/2012 fix problems in the screen while the R13 test 
*:************************************************************************

*N000682,1 HES 12/20/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\Screens\PW\PWPLANT.h
*N000682,1 HES 12/20/2012 Globalization changes[END]

lcProg = JUSTSTEM(SYS(16))
*- Call the screen
lcRunScx = lfGetScx("PW\&lcProg..scx")
*E303113,1 TMI 04/24/2012 [End  ]
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
  .cBrowseIndexExpression = "CPLANT_ID"
  .cBrowseIndexFields     = "CPLANT_ID"
  .cBrowseIndexName       = "PEPLANT"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	  = ALLTRIM(sydObjct.CPRGLNAME)
  .AriaBrFields.edtBrowseFields.Value = "CPLANT_ID :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PlantID,loFormSet.GetHeaderText("LANG_PlantID",loFormSet.HeaderAlias))+"',CPNAME :H='"+IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_PlantName,loFormSet.GetHeaderText("LANG_PlantName",loFormSet.HeaderAlias))+"'"
ENDWITH

*- set the input mask of the keyfield
WITH loFormSet
  .Ariaform1.AriaKeyField1.KeyTextbox.InputMask = REPLICATE('!',FSIZE(.cBrowseIndexFields,.lcBaseFile))
  .Ariaform1.CPNAME.InputMask = REPLICATE('X',FSIZE('CPNAME',.lcBaseFile))
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
  .AddProperty('lcObjType',"B")
ENDWITH
*!*	"B" Plant
*!*	"H" work center
*!*	"E" Employee


loFormset.HasMemo = .F.

WITH loFormset.Ariaform1.cntThumbnail
  .cType = "B"
  .cObjectKey = ""  &&*gfGetStyleImageInfo("K", ThisFormSet.lcStyleKey)
ENDWITH

** put phone format
WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
  LOCAL lcPhneTem
  lcPhneTem = gfPhoNeTem(.F., .Address1.cboCountry.Value)
  .txtPhone1.Inputmask = lcPhneTem
  .txtPhone2.Inputmask = lcPhneTem
ENDWITH

=lfCalender(loFormset,loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1,'P')

WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
  .Address1.txtAdd1.MaxLength = FSIZE('CPADDRESS1','PEPLANT')
  .Address1.txtAdd2.MaxLength = FSIZE('CPADDRESS2','PEPLANT')
  .Address1.txtAdd3.MaxLength = FSIZE('CPADDRESS3','PEPLANT')
  .Address1.txtAdd4.MaxLength = FSIZE('CPADDRESS4','PEPLANT')
  .Address1.txtAdd5.MaxLength = FSIZE('CPADDRESS5','PEPLANT')
  .Address1.txtAdd6.MaxLength = FSIZE('CPADDRESS6','PEPLANT')
  .txtEmail.MaxLength = FSIZE('CE_MAIL','PEPLANT')
  .txtPhone1.MaxLength = FSIZE('CPHONE','PEPLANT')
  .txtPhone2.MaxLength = FSIZE('CPHONE2','PEPLANT')
ENDWITH


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

*!*	WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1.cntThumbnail
*!*	  .cType = loFormset.lcObjType
*!*	  .cObjectKey = loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value
*!*	ENDWITH


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

  WITH loFormset.Ariaform1.cntThumbnail
  .cType = loFormset.lcObjType
  .cObjectKey = ""
  ENDWITH

  SELECT (loFormSet.DataFile)
  ZAP
  *B610136,2 [T20121021.0030] TMI 11/22/2012 [Start] 
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()
  *B610136,2 [T20121021.0030] TMI 11/22/2012 [End  ] 

CASE loFormSet.ActiveMode = 'V'

  WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
    SELECT PEPLANT
    lcID = PEPLANT.CPLANT_ID
    =gfSeek(lcID,'PEPLANT')
    loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value = CPLANT_ID
    loFormset.Ariaform1.CPNAME.value = CPNAME
    .Address1.txtAdd1.Value = CPADDRESS1
    .Address1.txtAdd2.Value = CPADDRESS2
    .Address1.txtAdd3.Value = CPADDRESS3
    .Address1.txtAdd4.Value = CPADDRESS4
    .Address1.txtAdd5.Value = CPADDRESS5
    .Address1.txtAdd6.Value = CPADDRESS6
    .txtEmail.Value = CE_MAIL
    .txtPhone1.Value = CPHONE
    .txtPhone2.Value = CPHONE2
    ** NCALID
  ENDWITH

*  WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1.cntThumbnail
  WITH loFormset.Ariaform1.cntThumbnail
  .cType = loFormset.lcObjType
  .cObjectKey = PEPLANT.CPLANT_ID
  ENDWITH

  SELECT (loFormSet.DataFile)
  ZAP

  IF PEPLANT.NCALID > 0
    lnW = 10
    =gfSeek(STR(PEPLANT.NCALID,lnW),'SCCALHDR')
    =gfSeek(STR(PEPLANT.NCALID,lnW),'SCCALDTL')
    SELECT SCCALDTL
    SCAN
      SCATTER MEMVAR MEMO
      SELECT (loFormSet.DataFile)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
    *B610136,2 [T20121021.0030] TMI 11/22/2012 [Start] 
    *loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()
    *B610136,2 [T20121021.0030] TMI 11/22/2012 [END] 
  ENDIF
  *B610136,2 [T20121021.0030] TMI 11/22/2012 [Start] call it regardless there is a calender or not
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()  
  *B610136,2 [T20121021.0030] TMI 11/22/2012 [End  ] 

  loFormSet.CheckNavigation()
  loFormSet.oToolBar.Navrefresh()

CASE loFormSet.ActiveMode = 'E'
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Enabled = .T.
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()

CASE loFormSet.ActiveMode = 'A'
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Enabled = .T.
  loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.Create()

ENDCASE

*loFormset.Ariaform1.Ariapageframe1.ActivePage = 1
loFormset.Ariaform1.Ariapageframe1.Ariapage2.Calendar1.grdCalendar.Refresh()


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
WITH loFormset.Ariaform1.Ariapageframe1.Ariapage1
replace CPLANT_ID   WITH  loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value ;
   		CPNAME      WITH  loFormset.Ariaform1.CPNAME.value ;
   		CPADDRESS1  WITH  .Address1.txtAdd1.Value ;
   		CPADDRESS2  WITH  .Address1.txtAdd2.Value ;
   		CPADDRESS3  WITH  .Address1.txtAdd3.Value ;
   		CPADDRESS4  WITH  .Address1.txtAdd4.Value ;
   		CPADDRESS5  WITH  .Address1.txtAdd5.Value ;
   		CPADDRESS6  WITH  .Address1.txtAdd6.Value ;
   		CE_MAIL     WITH  .txtEmail.Value ;
   		CPHONE      WITH  .txtPhone1.Value ;
   		CPHONE2     WITH  .txtPhone2.Value
ENDWITH

*- save the calender data

lnSeq = PEPLANT.NCALID
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
  SCAN
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
LOCAL lnSlct,llDel
lnSlct = SELECT(0)

llDel = .T.
llDel = llDel AND lfChkPlantID_InFile('PEWCNTR',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_AsstoWC,loFormSet.GetHeaderText("LANG_AsstoWC",loFormSet.HeaderAlias)))
llDel = llDel AND lfChkPlantID_InFile('PEPERSON',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_AsstoEmp,loFormSet.GetHeaderText("LANG_AsstoEmp",loFormSet.HeaderAlias)))
llDel = llDel AND lfChkPlantID_InFile('PESHIFT',IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_AsstoShf,loFormSet.GetHeaderText("LANG_AsstoShf",loFormSet.HeaderAlias)))

IF llDel
  SELECT (loFormset.lcBaseFile)
  gfDelete()
  gfTableUpdate()

  *E303113,4 TMI 11/06/2012 [Start] *- Delete the related Calenders
  lcBaseFile = loFormSet.lcBaseFile
  lfDelCalender(&lcBaseFile..NCALID)
  *E303113,4 TMI 11/06/2012 [End  ]
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
LOCATE FOR CPLANT_ID = loFormset.Ariaform1.AriaKeyField1.Keytextbox.Value
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
LOCAL lnSlct,llRet
lnSlct = SELECT(0)

IF EMPTY(loFormset.Ariaform1.CPNAME.Value)
  =gfModalGen('INM38267B00000','DIALOG', IIF(oAriaApplication.oactiveLang.clang_id = "EN",LANG_NameField,loFormSet.GetHeaderText("LANG_NameField",loFormSet.HeaderAlias)))
  loFormset.Ariaform1.CPNAME.SetFocus()
  RETURN .F.
ENDIF

*E303113,1 TMI 31/05/2012 [Start] select the calendar
*llRet = lfCalChk(loFormSet)
loCalGrd = loFormset.Ariaform1.Ariapageframe1.Ariapage2
llRet = lfCalChk(loFormSet,.F.,loCalGrd)
*E303113,1 TMI 31/05/2012 [End  ]

SELECT (lnSlct)
RETURN llRet
*- End of lfFormBeforeSave.
************************************************************
*! Name      : lfvPlant
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/29/2012
*! Purpose   : Valid function for Plant_ID
************************************************************
FUNCTION lfvKeyField
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
*- End of lfvPlant.
