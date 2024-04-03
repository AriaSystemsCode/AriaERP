*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMFRMCD.Prg
*:  Module      : System Manager
*: Program description : Form Codes
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 02/18/2013
*:  Reference   : *E303353,1
*:************************************************************************

*- Get the screen , call it

#INCLUDE R:\ARIA4XP\PRGS\SM\smfrmcd.H

lcRunScx = lfGetScx("SM\SMFRMCD.scx")
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

loFormSet.AddProperty('lcProgName','SMFRMCD')

*!*	*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
=lfOpenPRGFILES(loFormSet.lcProgName)

*** Load program base file
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

=lfDefineVars(loFormSet)

*- set row Source for the companies popup
WITH loFormSet.Ariaform1
  .laCompany.RowSource = 'Thisformset.laCompany'
ENDWITH

*- Grid
=lfSetGridDataSource(loFormset)

*- start the screen with the view mode, as in A4xp selecting a company to start aria is mandatory
loFormSet.ChangeMode('V')

*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/18/2013
*! Purpose   : Define screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.AddProperty('laCompany[1,4]')
loFormSet.AddProperty('laForms[1]')

loFormSet.AddProperty('laSettings[3,2]','')
loFormSet.AddProperty('lc_FrmCdHd','')
loFormSet.AddProperty('puCompName','')
loFormSet.AddProperty('lcFrmName','')
loFormSet.AddProperty('lcFrmDes','')
loFormSet.AddProperty('lcDataDir','')
loFormSet.AddProperty('lnArLoop','')
loFormSet.AddProperty('lcCurComp','')
loFormSet.AddProperty('lcFormMaj','')
loFormSet.AddProperty('lcFrmDtl','')
loFormSet.AddProperty('lcFrmHdr','')
loFormSet.AddProperty('lcPrevComp','')
loFormSet.AddProperty('cMarker','')
loFormSet.AddProperty('llPrntComp','')
loFormSet.AddProperty('llFactor','')
loFormSet.AddProperty('llPrntIns','')
loFormSet.AddProperty('lcPuCompName','')


*N000682,1 04/17/2013 RAS Globalization[START]
*!*	lcBrowTtl = "Forms Codes"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrowTtl = LANG_Form_Code
lcBrowTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Form_Code,loFormSet.GetHeaderText("LANG_Form_Code",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
STORE SPACE(0) TO loFormSet.laForms
*N000682,1 04/17/2013 RAS Globalization[START]
*!*	loFormSet.lcFrmName  = "Form Name"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcFrmName  = LANG_Form_Name
loFormSet.lcFrmName  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Form_Name,loFormSet.GetHeaderText("LANG_Form_Name",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]

lnVarNum = 0
llOpRep = gfOpenFile(oAriaApplication.SysPath+'SYREPUVR','Fld_Name','SH')
=SEEK('SMFRMCD')

SCAN WHILE cRep_Id = 'SMFRMCD' FOR lAskRunT .AND. lDispOG

  IF !EMPTY(CVER) AND CVER <> 'A27'
    LOOP
  ENDIF
  lnVarNum = lnVarNum + 1
  DIMENSION loFormSet.laSettings[lnVarNum,2]
  loFormSet.laSettings[lnVarNum,1] = SYREPUVR.mFld_Name

  DO CASE
    CASE SYREPUVR.cData_typ = 'L'
      STORE .F. TO (loFormSet.laSettings[lnVarNum,1])
    CASE SYREPUVR.cData_typ = 'C'
      STORE SPACE(0) TO (loFormSet.laSettings[lnVarNum,1])
    CASE SYREPUVR.cData_typ = 'D'
      STORE {} TO (loFormSet.laSettings[lnVarNum,1])
    CASE SYREPUVR.cData_typ = 'N'
      STORE 0 TO (loFormSet.laSettings[lnVarNum,1])
  ENDCASE
ENDSCAN

IF llOpRep .AND. USED('SYREPUVR')
  USE IN SYREPUVR
ENDIF
RELEASE lnVarNum
RELEASE llOpRep

*-- Temporary form codes files
loFormSet.lc_FrmCdHd = gfTempName()			&& Cursor name for a temporary FORMCDHD
loFormSet.lcFrmDtl   = gfTempName()		    && Alias name for FORMCDDT
loFormSet.lcFrmHdr   = gfTempName()			&& Alias name for FORMCDHD

IF !lfReadComp()
  glQuitting = .T.
  RETURN .F.
ELSE
  *-- Temporary file created
  CREATE CURSOR (loFormSet.lc_FrmCdHd);
    (cFormMaj C(6), cFormMjDes C(30), cFormCdDes C(30), cCurForm C(2)   ,;
     mFormSets M  , cStatus C(1)    , nRecNo N(4)     , CADD_USER C(10) ,;
     DADD_DATE D(8) , CADD_TIME C(11) , LLOK_STAT L , CLOK_USER C(10)   ,;
     DLOK_DATE D(8) , CLOK_TIME C(8) , CFORMID C(2) , LDISPLAY L)

  loFormset.AriaForm1.laCompany.Value = oAriaApplication.ActiveCompanyID
  loFormSet.lcDataDir = oAriaApplication.DataDir

  =lfOpenDbf(loFormSet)
  *Check if there are custom forms added to the system files
  =lfchkCFrm(loFormSet)
  llNoShow     = .F.
  loFormSet.ChangeMode('V')
ENDIF


************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet
oGrd = loFormSet.Ariaform1.grdSMFRMCD
WITH oGrd
  .RecordSource = ''
  .RecordSource = loFormSet.lc_FrmCdHd
ENDWITH
lc_FrmCdHd = loFormSet.lc_FrmCdHd

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	lfSetColumnsProp('1',"&lc_FrmCdHd..cFormMjDes",'Form Name'  ,200,oGrd)
*!*	lfSetColumnsProp('2',"&lc_FrmCdHd..cCurForm"  ,"Form Code"  ,70 ,oGrd)
*!*	lfSetColumnsProp('3',"&lc_FrmCdHd..cFormCdDes","Description",200,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('1',"&lc_FrmCdHd..cFormMjDes",LANG_Form_Name ,200,oGrd)
lfSetColumnsProp('1',"&lc_FrmCdHd..cFormMjDes",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Form_Name,loFormSet.GetHeaderText("LANG_Form_Name",loFormSet.HeaderAlias)) ,200,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('2',"&lc_FrmCdHd..cCurForm"  ,LANG_Form_Code ,70 ,oGrd)
lfSetColumnsProp('2',"&lc_FrmCdHd..cCurForm"  ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Form_Code,loFormSet.GetHeaderText("LANG_Form_Code",loFormSet.HeaderAlias)) ,70 ,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('3',"&lc_FrmCdHd..cFormCdDes",LANG_Description ,200,oGrd)
lfSetColumnsProp('3',"&lc_FrmCdHd..cFormCdDes",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,loFormSet.GetHeaderText("LANG_Description",loFormSet.HeaderAlias)) ,200,oGrd)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]

oGrd.ReadOnly = .T.

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

DIMENSION loFormSet.laForms[1]
loFormSet.laForms = ' '
lc_FrmCdHd = loFormSet.lc_FrmCdHd
lcCurForm  = &lc_FrmCdHd..cFormMaj
lcFormId   = &lc_FrmCdHd..CFORMID

SELECT cFormCode + " - " + cFormCdDes,  cFormCode,cFormMaj ;
  FROM (loFormSet.lcFrmDtl) ;
  WHERE cFormMaj+cFormCode = lcCurForm ;
  AND CFORMID = PADR(lcFormId,2);
  INTO ARRAY loFormSet.laForms

WITH loFormSet.Ariaform1
  .laForms.RowSource = 'Thisformset.laForms'
ENDWITH

lc_FrmCdHd = loFormSet.lc_FrmCdHd
WITH loFormSet.Ariaform1
  .lcFrmName.Value = &lc_FrmCdHd..cFormMjDes
  .laForms.Value = &lc_FrmCdHd..cCurForm
  .Refresh()
ENDWITH

*- End of lfFormAfterRowColumnChange.

*!*************************************************************
*! Name      : lpShow
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Handle The special case for screen modes
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpShow
*!*************************************************************

PROCEDURE lpShow
PARAMETERS loFormSet
*-- Called from gpCtrlShow which is the SHOW procedure of the READ CYCLE,
*-- Added by the generated code of the tool bar to the SPR

IF TYPE('loFormSet.lcProgName')='U'
  RETURN
ENDIF

DO CASE
  CASE loFormSet.ActiveMode = 'S' && Select mode
    SELECT (loFormSet.lc_FrmCdHd)
    BLANK ALL
    DELETE ALL
    *N000682,1 04/17/2013 RAS Globalization[START]
*!*	    loFormSet.lcFrmName  = "Form Name"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcFrmName  = LANG_Form_Name
loFormSet.lcFrmName  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Form_Name,loFormSet.GetHeaderText("LANG_Form_Name",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/17/2013 RAS Globalization[End  ]
    loFormSet.Ariaform1.laForms.ListIndex = 0
    WITH loFormSet.Ariaform1.laCompany
      .Enabled = .T.
      .Value = ' '
      .Refresh()
      .SetFocus()
      KEYBOARD '{ALT+DNARROW}'
    ENDWITH

  CASE loFormSet.ActiveMode = 'V' && View Mode
    SELECT (loFormSet.lc_FrmCdHd)
    BLANK ALL
    DELETE ALL

    SELECT (loFormSet.lcFrmHdr)
    SCAN
      SCATTER MEMVAR MEMO
      m.nRecNo = RECNO()
      SELECT (loFormSet.lcFrmDtl)
      m.cFormCdDes = IIF(SEEK(m.cFormMaj+m.cCurForm), cFormCdDes, "")
      m.cStatus    = 'S'
      *-- Add The selected values to cursor
      INSERT INTO (loFormSet.lc_FrmCdHd) FROM MEMVAR
    ENDSCAN
    *-- U Can browse the selected through the cursor
    SELECT (loFormSet.lc_FrmCdHd)
    GOTO TOP
    =lfWhenBrow(loFormSet)
    =lfFormAfterRowColumnChange(loFormSet)
    loFormSet.oToolbar.cmdSelect.Enabled = .T.
    loFormSet.oToolbar.cmdEdit.Enabled = .T.
    loFormset.AriaForm1.cmdLayout.Enabled = .T.

  CASE loFormSet.ActiveMode = 'E' && Edit mode
    SELECT (loFormSet.lc_FrmCdHd)
    BLANK ALL
    DELETE ALL

    SELECT (loFormSet.lcFrmHdr)
    SCAN
      SCATTER MEMVAR MEMO
      m.nRecNo = RECNO()

      SELECT (loFormSet.lcFrmDtl)
      m.cFormCdDes = IIF(SEEK(m.cFormMaj+m.cCurForm), cFormCdDes, "")
      m.cStatus    = 'S'
      INSERT INTO (loFormSet.lc_FrmCdHd) FROM MEMVAR
    ENDSCAN

    SELECT (loFormSet.lc_FrmCdHd)
    GOTO TOP
    =lfWhenBrow(loFormSet)

    loFormSet.Ariaform1.laCompany.Enabled = .F.

ENDCASE

loFormSet.Ariaform1.grdSMFRMCD.ReadOnly = .T.

loFormSet.Ariaform1.Refresh()

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfWhenBrow
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Handles The Movement of the arow keys inside the
*!             Browse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfWhenBrow()
*!*************************************************************

FUNCTION lfWhenBrow
PARAMETERS loFormSet
PRIVATE lcCurForm , lcFormId

DIMENSION loFormSet.laForms[1]

loFormSet.laForms[1] = SPACE(0)
lc_FrmCdHd = loFormSet.lc_FrmCdHd
lcCurForm  = &lc_FrmCdHd..cFormMaj
*--HDM E301251,1 [Start] Read the special setting variable and filter to get its form

lcFormId   = &lc_FrmCdHd..CFORMID

*-- Add CformMaj to the array as 3rd Element

SELECT cFormCode + " - " + cFormCdDes,  cFormCode,cFormMaj ;
  FROM (loFormSet.lcFrmDtl) ;
  WHERE cFormMaj+cFormCode = lcCurForm ;
  AND CFORMID = PADR(lcFormId,2);
  INTO ARRAY loFormSet.laForms
WITH loFormSet.Ariaform1
  .laForms.RowSource = 'Thisformset.laForms'
ENDWITH

lc_FrmCdHd = loFormSet.lc_FrmCdHd
IF ASCAN(loFormSet.laForms,&lc_FrmCdHd..cCurForm) # 0
  loFormSet.Ariaform1.laForms.ListIndex = ASUBSCRIPT(loFormSet.laForms, ASCAN(loFormSet.laForms,&lc_FrmCdHd..cCurForm), 1)
ELSE
  loFormSet.Ariaform1.laForms.ListIndex = 1
ENDIF

loFormSet.lcFrmName  = ALLTRIM(&lc_FrmCdHd..cFormMjDes)
lnRecNo   = RECNO()


*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfvCompany
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Gets the selected company Data Files Path AND
*!             opens them
*!*************************************************************
*! Calls     : lfOpenDbf
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvCompany()
*!*************************************************************

FUNCTION lfvCompany
PARAMETERS loFormset, loFld

*-- When a company is selected, switch to view mode:
IF loFld.Value <> loFld.OldValue
  loFormSet.ChangeMode('V')
  IF loFld.Value <> oAriaApplication.ActiveCompanyID
    *-- When Any other item is selected just get it's data source
    FOR lnArLoop2 = 1 To ALEN(loFormSet.laCompany, 1)
      IF ALLTRIM(loFormSet.laCompany[lnArLoop2,3]) = loFld.Value
        loFormSet.lcDataDir        = ALLTRIM(loFormSet.laCompany[lnArLoop2,2])
        EXIT
      ENDIF
    ENDFOR
    = lfOpenDbf(loFormSet)

    =lfchkCFrm(loFormSet)

  ENDIF
ELSE
  loFormSet.ChangeMode('S')
ENDIF
*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfReadComp
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Reads the available companies from SYCCOMP
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfReadComp()
*!*************************************************************

FUNCTION lfReadComp
PRIVATE llRetVal

*-- Fill the Companies array from SYCCOMP
SELECT cComp_ID +' - ' + cCom_Name,PADR(gfGetDataDir(ALLT(cCom_DDir)),LEN(cCom_dDir)),cComp_ID;
  FROM (oAriaApplication.SysPath + 'SYCCOMP');
  INTO ARRAY loFormSet.laCompany;
 ORDER BY cComp_ID

*-- Check if any company exists at all
IF _TALLY = 0
  *-- Message
  *** No companies available. ***
  *** <  Ok  > ***

  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  =gfModalGen("TRM00189B00000","DIALOG","companies")
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00189B00000","DIALOG",LANG_companies)
=gfModalGen("TRM00189B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_companies,loFormSet.GetHeaderText("LANG_companies",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  llRetVal = .F.
ELSE
  llRetVal = .T.
ENDIF
RETURN llRetVal


*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfOpenDbf
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Open the Data Files
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfOpenDbf()
*!*************************************************************

FUNCTION lfOpenDbf
PARAMETERS loFormSet

*-- Open The database file
IF USED(loFormSet.lcFrmDtl)
  USE IN (loFormSet.lcFrmDtl)
ENDIF

IF USED(loFormSet.lcFrmHdr)
  USE IN (loFormSet.lcFrmHdr)
ENDIF

USE (loFormSet.lcDataDir + 'FORMCDHD')  AGAIN ALIAS (loFormSet.lcFrmHdr);
	IN 0 ORDER TAG FORMCDHD

USE (loFormSet.lcDataDir + 'FORMCDDT') AGAIN ALIAS (loFormSet.lcFrmDtl);
	IN 0 ORDER TAG FORMCDDT

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfvFormCode
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Update the cursor with the selected code
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvFormCode()
*!*************************************************************

FUNCTION lfvFormCode
PARAMETERS loFormSet,loFld

puForms = loFormSet.Ariaform1.laForms.ListIndex
IF puForms > 0
  SELECT (loFormSet.lc_FrmCdHd)
  REPLACE cCurForm   WITH loFormSet.laForms[loFormSet.Ariaform1.laForms.ListIndex,2],;
   	    cFormCdDes WITH SUBSTR(loFormSet.laForms[puForms,1], 6) ,;
   	    cStatus    WITH 'M'

  *-- Update the cursor with the audit values
  = gfAdd_Info(loFormSet.lc_FrmCdHd)
ENDIF

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfwCoName
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Sets a variable with the previouse company
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfwCoName()
*!*************************************************************

FUNCTION lfwCoName
lcPrevComp = puCompName

* --------------------------------------------------
*!*************************************************************
*! Name      : lfGoToNav
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Moves the Focus to the Settings Window
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGoToNav()
*!*************************************************************

FUNCTION lfGoToNav

IF SYS(18) = UPPER('pbLayout')
  =lfGoToFlds()
ENDIF
* --------------------------------------------------
*!*************************************************************
*! Name      : lfGoToFlds
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Moves the Focus to the Company Selection Window
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGoToFlds()
*!*************************************************************

FUNCTION lfGoToFlds


* --------------------------------------------------
*!*************************************************************
*! Name      : lpSavScr
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : My own Saving
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************

PROCEDURE lpSavScr
PARAMETERS loFormSet
=gftmp2Mast(loFormSet.lcFrmHdr ,loFormSet.lc_FrmCdHd)

* --------------------------------------------------
*!*************************************************************
*! Name      : lfGtDelBar
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : To get the Delete BAR Number to disable it
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGtDelBar()
*!*************************************************************

FUNCTION lfGtDelBar
PARAMETERS lcBarPrmpt

*-- Open the system menu file.
USE (oAriaApplication.SysPath+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
IF SEEK("GFCPDELETE", "MenuFile")
  lcDelPadNm = UPPER(ALLTRIM(MenuFile.cMstr_Nam))
  lnRetVal   = VAL(MenuFile.cBar_Pos)
  lcBarPrmpt = ALLTRIM(MenuFile.cSub_Prpt)
ELSE
  *-- The default values.
  lcDelPadNm = "P03PU03"
  lnRetVal   = 10
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  lcBarPrmpt = '\<Delete'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBarPrmpt = '\<'+LANG_Delete+"'"
lcBarPrmpt = '\<'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Delete,loFormSet.GetHeaderText("LANG_Delete",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
ENDIF
USE IN MenuFile
RETURN (lnRetVal)
* --------------------------------------------------
*!*************************************************************
*! Name      : lfVSetings
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Generate the option grid with the saved values
*!*************************************************************
*! Calls     : lfSavDefVr AND lfGetDefVr
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfVSetings()
*!*************************************************************

FUNCTION lfVSetings
PARAMETERS loFormSet

SELECT (loFormSet.lc_FrmCdHd)
REPLACE cStatus WITH 'M'

lc_FrmCdHd = loFormSet.lc_FrmCdHd
lcFrmDtl   = loFormSet.lcFrmDtl
lcFrmHdr   = loFormSet.lcFrmHdr

PRIVATE lnCount
LOCAL lcVar
DO msg
SELECT (loFormSet.lc_FrmCdHd)
lc_FrmCdHd = loFormSet.lc_FrmCdHd
IF !EMPTY(&lc_FrmCdHd..mFormSets)
  RESTORE FROM MEMO mFormSets ADDITIVE
  IF TYPE('laSettings')='U'
    BLANK FIELDS &lc_FrmCdHd..mFormSets
    lfVSetings(loFormSet)
    RETURN
  ENDIF
  *- in case that an option is added or removed, base on the loFormSet.laSettings dimension
  lnLen = ALEN(laSettings,1)
  IF lnLen <> ALEN(loFormSet.laSettings,1)
    IF ALEN(loFormSet.laSettings,1)>lnLen
      DIMENSION laSettings[ALEN(loFormSet.laSettings,1),2]
    ENDIF
    FOR lnCnt = lnLen+1 TO ALEN(loFormSet.laSettings,1)
      laSettings[lnCnt,1] = loFormSet.laSettings[lnCnt,1]
      laSettings[lnCnt,2] = loFormSet.laSettings[lnCnt,2]
    ENDFOR
  ENDIF
ELSE
  *- in case user runs the form settings for the first time
  DIMENSION laSettings[ALEN(loFormSet.laSettings,1),ALEN(loFormSet.laSettings,2)]
  =ACOPY(loFormSet.laSettings,laSettings)
ENDIF
FOR lnCount = 1 TO ALEN(laSettings,1)
  lcVar = laSettings[lnCount, 1]
  &lcVar = laSettings[lnCount, 2]
ENDFOR

*-- Call the option grid Function

SCATTER MEMVAR

lcResult=gfOpGrid("SMFRMCD",.T.,.F.,.F.,.T.,.T.)
IF EVALUATE(lcResult)
  *= lfSavDefVr()
  PRIVATE lnCount
  LOCAL lcVar
  FOR lnCount = 1 TO ALEN(laSettings,1)
    lcVar = laSettings[lnCount, 1]
    laSettings[lnCount, 2] = &lcVar
  ENDFOR

  *-- Save the settings into the memo field inside the DBF
  SELECT (loFormSet.lc_FrmCdHd)
  SAVE TO MEMO mFormSets;
       ALL LIKE laSettings

  DIMENSION laSettings[ALEN(loFormSet.laSettings,1),ALEN(loFormSet.laSettings,2)]
  ACOPY(laSettings,loFormSet.laSettings)
ENDIF

* --------------------------------------------------
*!*************************************************************
*! Name      : lfvLayout
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Run the layout preview considering the selected form
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvLayout()
*!*************************************************************
FUNCTION lfvLayout
PARAMETERS loFormSet

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT (loFormSet.lcFrmDtl)
*E301065,1 AMM if there is not layout exist, don't display an empty report
lc_FrmCdHd = loFormSet.lc_FrmCdHd
lcFrmDtl = loFormSet.lcFrmDtl
=SEEK(&lc_FrmCdHd..cFormMaj + &lc_FrmCdHd..cCurForm)
IF EMPTY(&lcFrmDtl..gfrmBmp)
  *-- Message
  *-- Invoice form layout is not available.
  *--          < OK >
  =gfModalGen("INM00341B00000","DIALOG",ALLTRIM(&lc_FrmCdHd..cFormMjDes))
ELSE
  REPORT FORM (oAriaApplication.clientreporthome+oAriaApplication.activemoduleid+"\SMFRMCD.FRX")       ;
       FOR                                             ;
         &lc_FrmCdHd..cFormMaj + &lc_FrmCdHd..cCurForm ;
       = &lcFrmDtl..cFormMaj   + &lcFrmDtl..cFormCode  ;
       PREVIEW

  SELECT (lnCurAlias)
ENDIF
* --------------------------------------------------

*!*************************************************************
*! Name      : lfchkCFrm
*! Developer : Ahmed Ibrahim
*! Date      : 11/22/1998
*! Purpose   : Check if there is any custom forms added to the system files
*! Reference : *E301065,1
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfchkCFrm()
*!*************************************************************
FUNCTION lfchkCFrm
PARAMETERS loFormSet

PRIVATE laSyFrmCdd , lnAlias

lcTmpFDT = gfTempName()
llOpSYREP = gfOpenFile(oAriaApplication.SysPath+'SYDREPRT','CREP_ID','SH')

IF USED('SYDREPRT')
  SET FILTER TO SYDREPRT.cVer<>"A40"
ENDIF

USE (oAriaApplication.SysPath+'SYFRMCDD') ORDER TAG UPGRDLVL AGAIN ALIAS (lcTmpFDT) IN 0
lcTmpFHD = gfTempName()
USE (oAriaApplication.SysPath+'SYFRMCDH') ORDER TAG Formcdhd AGAIN ALIAS (lcTmpFHD) IN 0
SELECT (lcTmpFDT)
SET RELATION TO cformmaj+cformcode INTO (loFormSet.lcFrmDtl) ADDITIVE
SET RELATION TO cRep_Id INTO SYDREPRT ADDITIVE
SET RELATION TO cformmaj+cformID INTO (lcTmpFHD) ADDITIVE

=AFIELDS(laSyFrmCdd)
SCAN
  IF SEEK(loFormset.Ariaform1.laCompany.Value,'SYCCOMP') .AND. !EOF('SYDREPRT')
    lcInstMod = SYCCOMP.mComp_Mdl
    IF !EMPTY(SYDREPRT.mCallMods)
      lnRel = OCCURS('|',SYDREPRT.mCallMods)
      DIMENSION laCalMdls[lnRel+1]
      laCalMdls = ''
      =gfSubStr(SYDREPRT.mCallMods,@laCalMdls,",")
      FOR lnC=1 TO ALEN(laCalMdls)
        IF laCalMdls[lnC] $ lcInstMod
          SCATTER MEMVAR MEMO

          IF EOF(loFormSet.lcFrmDtl)
            *-- we 've replaced Append From instead of Insert into (for the General Field) to be copied
            lnAlias = SELECT()
            SELECT (loFormSet.lcFrmDtl)
            APPEND FROM (oAriaApplication.SysPath+'SYFRMCDD') FOR cFormMaj + cFormCode = m.cFormMaj+m.cFormCode
            SELECT (lnAlias)
          ELSE
            =RLOCK()
            *-- For loop around all fields of SyFrmcdd
            lcFrmDtl = loFormSet.lcFrmDtl
            FOR lnC = 1 TO ALEN(laSyFrmCdd,1)
              *-- if Type of field is Char , Numeric , Logical , Memo
              IF laSyFrmCdd[lnC,2] $ 'CNLM'
                REPLACE &lcFrmDtl..&laSyFrmCdd[lnC,1] WITH m.&laSyFrmCdd[lnC,1]
              ENDIF
              *-- Endif of Type of field is Char , Numeric , Logical , Memo
            ENDFOR
            UNLOCK
          ENDIF

          IF !EOF(lcTmpFHD) .AND. !SEEK(&lcTmpFHD..cformmaj+&lcTmpFHD..cformID,loFormSet.lcFrmHdr)
            SELECT (lcTmpFHD)
            SCATTER MEMVAR MEMO
            INSERT INTO (loFormSet.lcFrmHdr) FROM MEMVAR
          ENDIF
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
ENDSCAN

IF USED(lcTmpFDT)
  USE IN (lcTmpFDT)
ENDIF
IF llOpSYREP .AND. USED('SYDREPRT')
  USE IN SYDREPRT
ENDIF
IF USED(lcTmpFHD)
  USE IN (lcTmpFHD)
ENDIF
