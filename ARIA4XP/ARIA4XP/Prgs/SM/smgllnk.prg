*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMGLLNK.Prg
*:  Module      : System Manager 
*:  Desc.       : Link Code Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/03/2012 
*:  Reference   : *E303316,1 
*:************************************************************************
*B610249,1 TMI 02/17/2013 1.Remove the company combobox, 
*                         2. allow the screen to show all link types regardless the selected module
*:************************************************************************
PARAMETERS lcSetupCom,lcSULnkVer

*- Get the screen , call it 
lcRunScx = lfGetScx("SM\SMGLLNK.scx")
*DO FORM (lcRunScx) WITH lcSetupCom,lcSULnkVer
IF !EMPTY(lcSetupCom)
  PRIVATE oScr
  DO FORM (lcRunScx) WITH lcSetupCom,lcSULnkVer NAME oScr NOSHOW 
  IF TYPE('oScr')='O' AND !ISNULL(oScr)
    oScr.Show(1)
  ENDIF
ELSE 
  DO FORM (lcRunScx) WITH lcSetupCom,lcSULnkVer
ENDIF   

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

loFormSet.AddProperty('lcProgName','SMGLLNK')

*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE 
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE 

*- Open tables 
=lfOpenPRGFILES(loFormSet.lcProgName)


IF !lfGL(loFormset)
  RETURN .F.
ENDIF 

*** Load program base file 
=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))
*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile 
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "LINKTYPE+LINK_CODE"
  .cBrowseIndexFields     = "LINKTYPE,LINK_CODE"  
  .cBrowseIndexName       = "GL_LINK1"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle            = 'GL Link Code'
  .ariaBrFields.edtBrowseFields.Value = "LINK_CODE :H='GL Link Code',"+;
                                        "LINKTYPE :H='Link Type',"+;
                                        "LINKDESC :H='Link Description'"
  .Ariaform1.lcAcSegDes.Caption = .lcAcSegDes  
ENDWITH


SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN .F.
ENDIF

*** check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE
IF EOF()
  *** The chart of accounts is empty. You have to create. ***
  *** the chart of accounts first...
  *** <  Ok  > ***
  =gfModalGen("TRM02215B00000","DIALOG")
  glQuitting = .T.
  RETURN .F.
ENDIF

lfDefineVars(loFormSet)

*X* original code **

CREATE TABLE (oAriaApplication.WorkDir + loFormSet.lcKeyCat);
 (Link_Code C(6),LinkType C(2),LinkDesc C(30),CatgKey C(3),CatgDesc C(30),;
  GLAcnt C(24),GLAccDesc C(40),nRecNo N(4),cStatus C(1))

*-- lfTypeArr() is a function to prepare the Link Type arry according
*   to the calling module
llNoThing = lfTypeArr(loFormSet)
*-- lfGLFiles() is a function to open the GL_Link file account chart
*   file according to the GL version(ARIA,SBT,Others) and to the active
*   company

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF   
=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID','SH')
IF !lfGLFiles(loFormSet)
  glQuitting = .T.
  RETURN .F.
ENDIF
  
*-- This is to switch screen mode to add mode if the program is called
*   from the setup
IF loFormSet.llSetUp
  *STORE .F. TO laScrMode
  *loFormSet.ActiveMode = 'A' = .T.
  loFormSet.ChangeMode('A')
  llNoShow = .F.
ENDIF    &&IF llSetUp

IF !oAriaApplication.ActiveModuleID == 'SM'  
  WITH loFormSet.Ariaform1
    *B610249,1 TMI 02/17/2013 [Start] remove the lnCOmp combobox
    *.lnComp.Visible = .F. 
    *B610249,1 TMI 02/17/2013 [End  ] 
    
*!*	    FOR lnI =  1 TO .ControlCount
*!*	      o = .Controls(lnI)
*!*	      o.Top = o.Top - 25
*!*	    ENDFOR 
*!*	    .grdLINKCODE.Height = .grdLINKCODE.Height + 25
	  ENDWITH 
ENDIF 

*- set decription caption background
loFormset.Ariaform1.lcTypDesc.BackColor = loFormset.Ariaform1.BackColor

*- set input mask
WITH loFormSet.Ariaform1
  .laData2.KeyTextbox.InputMask = REPLICATE('!',FSIZE('Link_Code','GL_LINK'))
  .lcTypeDesc.MaxLength = FSIZE('LINKDESC','GL_LINK')
ENDWITH   

*- relocate description field
WITH loFormset.Ariaform1
  .lcAccnlDes.Left = .lcAcctcode.Left + .lcAcctcode.Width + 10
ENDWITH   

loFormSet.ChangeMode('S')  
*!*	PUSH KEY                                      && To save the the current on key label
*!*	ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)  && To activate the browse screen when pressing ALT+B
*!*	DO (gcScrDir + gcWinAppl + '\SMGLLNK.SPR')                 && calling SMGLLNK screen
*!*	RELEASE WINDOW (lcBrTtl)                      && Release the Browse Window
*!*	POP KEY                                       && To Restore the previous assignments for on key label

RETURN .T.
*- End of lfFormInit.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/23/2012
*! Purpose   : Define screen variables 
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

*-- llSetup Variable that showes if the program called from setup or from menu
loFormSet.AddProperty('llSetup' , !EMPTY(loFormSet.lcSetupCom))

*-- loFormSet.laData      array that hold the data of the key screens and header folder
*-- laScrMode   array that hold the screen mode
*-- laKeyField  array that hold the key of the screen 
*-- loFormSet.laType      array that hold the link types
*-- laComp      Array that hold companies list

lcScFields = 'LinkType,Link_Code,LinkDesc' 
loFormSet.AddProperty('lcScFields',lcScFields)
lcLn = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcLn.]') 

loFormSet.AddProperty('laKeyField[2,4]')
loFormSet.AddProperty('laType[5]','')
loFormSet.AddProperty('laComp[1,3]','')

*-- lcTypeDesc  variable that hold link type description
*-- lcGLAcc     variable that hold G/L. Account
*-- lcGLDesc    variable that hold G/L. description
*-- lcGLChrTag  variable that hold the name of the account chart tag
*-- lcGLDir     variable that hold the directory of the GL_Link file
*-- lcFile      variable that hold the name of the account chart file
*-- lcGLCo      variable that hold the name of the G/L. company
*-- lcCodeFld   variable that hold the name of the GL account code filed
*               in account chart file
*-- lcDescFld   variable that hold the name of the GL account description 
*               filed in account chart file
*-- lcSysDir    variable that hold the system files directory of the used
*               G/L. version
*-- lcGLVer     variable that hold the version of the G/L. Link
*-- lcGLCo      variable that hold Company ID which the system is linked
*               to its GL.
*-- lcOldVal    variable that hold the original value of any field befor modify it
*-- lcPrntCo    variable that hold the parent company ID
*-- lcPrSysDir  variable that hold the system files directory of the parent
*               company 
*-- lcPrGLVer   variable that hold the version of the parent company G/L. Link
*-- lcPrGLCo    variable that hold parent Company ID which the system is linked
*               to its GL.
*-- lcPrGLDir   variable that hold the directory of the GL_Link file of the 
*               parent company
*-- lcPrFile    variable that hold the name of the account chart file of the 
*               parent company
*-- lcCpyLink   variable that hold the source GL link code to copy information
*               from it
*-- lcGlLink1   variable that hold temp name to be used for defining the
*               key fields window
*-- lcGlLink2   variable that hold temp name to be used for defining the
*               browse window
*-- lcGlLink3   variable that hold temp name to be used for defining the
*               edit rigion window
*-- lcKeyCat    variable that hold the temp name of dbf file which used for 
*               holding link code category keys
*-- lcBaseGL    variable that hold the name of the alias which GL_Link file
*               will be used with it and showes the detail records only
*               (do not include records that have linktype = '00')
*-- lcAllGL     variable that hold the name of the alias which GL_Link file
*               will be used with it and showes all file records
*-- lcGLChart   variable that hold the name of the alias which chart of 
*               Account GL_Link file will be used with it
*-- lcGLMask    variable that hold the name of the alias which Account code
*--             file will be used with it
*-- lcGLAccStr  variable that hold the name of the alias which Account structure 
*--             file will be used with it
*-- lcEmptyMas  Variable that hold the account picture with the seperator
*--             only (no picture characters)

lcVars = 'lcTypeDesc,lcGLAcc,lcGLDesc,lcGLChrTag,'+;
         'lcGLDir,lcFile,lcGLCo,lcCodeFld,lcDescFld,'+;
         'lcGLVer,lcPrntCo,'+;
         'lcPrGLDir,lpopcPrFile,lcCpyLink,'+;
         'lcAcMask,'+;
         'lcEmptyMas'
=lfAddProp(loFormSet,lcVars,'')                  
*-- lcGLDscStat variable that hold the eanabling status of the G/L. description
*               field
*-- lcTypeStat  variable that hold the eanabling status of the link type field
*-- lcLinkStat  variable that hold the eanabling status of the link code field
lcVars =  'lcGLDscStat,lcTypeStat,lcLinkStat'
=lfAddProp(loFormSet,lcVars,'DISABLE')

*-- llOtherVer  variable that showes if the system is linked to ARIA,SBT or
*               Others GL versions to be used in the screen SMGLLNK3 for
*               GL Account field and its push button (#OBJDISP llOtherVer)
*-- llChldComp  variable that showes if the company is a chield company
*-- llBrowse    variable that used to show if any browse button is clicked
*-- llNoShow    variable that allow screen to call lpshow procedure once 
*--             the program is run

lcVars = 'llOtherVer,llChldComp,llBrowse'
=lfAddProp(loFormSet,lcVars,.F.)

*-- lnBrRecNo   variable that hold the current record of the category keys
*               browse
*-- lnComp      variable that hold the number of the cuurent companies
*               popup value
*-- lnLinkTxt   variable that hold the number of the cuurent Link type
*               popup value
lcVars = 'lnBrRecNo,lnComp,lnLinkTxt,lnAcLen'
=lfAddProp(loFormSet,lcVars,0)

*-- llSMMOD     variable that showes if the parogram called from SM module
*               or any other module
loFormSet.AddProperty('llSMMOD' , oAriaApplication.ActiveModuleID == "SM" )

*-- lcTypDesc   To hold the description of the GL Link Category Type + Code 
*				Description.
loFormSet.AddProperty('lcTypDesc' , "Description" )


*-- lcBrTtl variable that hold the browse title
loFormSet.AddProperty('lcBrTtl' , 'Key Categories' )
 
loFormSet.laKeyField[1,1] = "laData[1]"
loFormSet.laKeyField[1,2] = .F.
loFormSet.laKeyField[1,3] = "GL_Link1"
loFormSet.laKeyField[1,4] = 1

loFormSet.laKeyField[2,1] = "laData[2]"
loFormSet.laKeyField[2,2] = .T.
loFormSet.laKeyField[2,3] = "GL_Link1"
loFormSet.laKeyField[2,4] = 2

*!*	*-- this is to open the SMGLLNK Screen as modal window if the program
*!*	*   is called from setup
*!*	IF loFormSet.llSetUp
*!*	  lcModal' = 'WITH (GCBASEWIND), (lcBrTtl)'
*!*	ELSE
*!*	  lcModal' = ''
*!*	ENDIF     &&IF llSetUp

*-- This is to get the GL_Link file browse fields 
loFormSet.AddProperty('lcBrFields' , gfDbfField('GL_LINK') )
*-- This is to get the GL_Link browse title
  
PRIVATE llSydFOpn
llSydFOpn = gfOpenFile(oAriaApplication.Syspath+'SYDFILES','Cfile_nam','SH')
  
loFormSet.AddProperty('lcFile_Ttl' , IIF(SEEK('GL_LINK', 'SYDFILES'),SYDFILES.cFile_Ttl, 'G/L Link codes')  )
  
= lfCloseFile('SYDFILES')
  
loFormSet.AddProperty('lcGlLink1'  , gfTempName() )
loFormSet.AddProperty('lcGlLink2'  , gfTempName() )
loFormSet.AddProperty('lcGlLink3'  , gfTempName() )
    
loFormSet.AddProperty('lcKeyCat'   , gfTempName() )
  
*loFormSet.AddProperty('lcBaseGL'   , gfTempName() )
loFormSet.AddProperty('lcBaseGL'   , gfTempName() )

loFormSet.AddProperty('lcAllGL'    , gfTempName() )
loFormSet.AddProperty('lcGLChart'  , gfTempName() )
loFormSet.AddProperty('lcSysTmp'   , gfTempName() )
loFormSet.AddProperty('lcGLMask'   , gfTempName() )
loFormSet.AddProperty('lcGLAccStr' , gfTempName() )

*- End of lfDefineVars.

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet

oGrd = loFormset.Ariaform1.grdLINKCODE
oGrd.ColumnCount = 4

oGrd.RecordSource = ''
oGrd.RecordSource = loFormSet.lcKeyCat
lcKeyCat = loFormSet.lcKeyCat

lfSetColumnsProp(oGrd,'1',"&lcKeyCat..CatgKey"  ,'Category Key',100)
lfSetColumnsProp(oGrd,'2',"&lcKeyCat..CatgDesc" ,'Description' ,160)
lfSetColumnsProp(oGrd,'3',"&lcKeyCat..Glacnt" ,'GL. Account'   ,100)
oGrd.Column3.Text1.InputMask = loFormSet.lcAcsMask

*!*	oGrd.Column1.Readonly = .T.
*!*	oGrd.Column2.Readonly = .T.
*!*	oGrd.Column3.ReadOnly = !loFormSet.ActiveMode $ 'AE'

IF loFormSet.llOtherVer
  lfSetColumnsProp(oGrd,'4',"&lcKeyCat..GlaccDesc" ,'GL. Description'   ,160)
  *oGrd.Column4.ReadOnly = !(Thisformset.lcGLVer = 'O' AND loformset.ActiveMode $ 'AE')
ELSE 
  lcDescFld = loFormSet.lcDescFld
  lcSrc = [IIF(SEEK(&lcKeyCat..Glacnt,'GLACCHAR'),GLACCHAR.&lcDescFld,'')]
  lfSetColumnsProp(oGrd,'4',IIF(!EMPTY(lcDescFld),lcSrc,''),'Description'   ,160)
  *oGrd.Column4.ReadOnly = .T.
ENDIF 

oGrd.Readonly = .T.

*- Refresh the bottom of the screen
lfFormAfterRowColumnChange(loFormSet)
oGrd.Refresh()

*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS oGrd,lcCol,lcSrc,lcHeader,lnWidth
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH oGrd
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH 
*- End of lfSetColumnsProp.

************************************************************
*! Name      : lfFormdestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Destroy function
************************************************************
FUNCTION lfFormdestroy
PARAMETERS loFormSet


*- End of lfFormdestroy.
************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : after row col changes in the grid
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loFormSet

lcKeyCat = loFormset.lcKeyCat
WITH loFormset.Ariaform1
  .lcAcctcode.Keytextbox.Value = &lcKeyCat..Glacnt
  
  IF loFormSet.llOtherVer
    .lcAccnlDes.Value = &lcKeyCat..GlaccDesc
  ELSE 
    lcDescFld = loFormSet.lcDescFld
    .lcAccnlDes.Value = IIF(SEEK(&lcKeyCat..Glacnt,'GLACCHAR'),GLACCHAR.&lcDescFld,'')
  ENDIF 
  
ENDWITH   

*- End of lfFormAfterRowColumnChange.
************************************************************
*! Name      : lfAccKey
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/05/2012
*! Purpose   : if user keyed in up/dn arrow then do skip in the table
************************************************************
FUNCTION lfAccKey
PARAMETERS loformset,nKeyCode, nShiftAltCtrl
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT (loFormSet.lcKeyCat)
DO case
CASE nKeyCode = 5 && up
  SKIP -1
  IF BOF()
    GO top
  ENDIF 
CASE nKeyCode = 24 && down
  SKIP
  IF EOF()
    GO bottom
  ENDIF 
ENDCASE 
lfFormAfterRowColumnChange(loFormSet)
loFormset.Ariaform1.grdLINKCODE.Refresh()
SELECT(lnSlct)
*- End of lfAccKey.


************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Activate the screen
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate.

************************************************************
*! Name      : lfSetControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/04/2012
*! Purpose   : Set the source of controls on the screen
************************************************************
FUNCTION lfSetControlSource
PARAMETERS loFormSet

WITH loFormSet.Ariaform1
  *B610249,1 TMI 02/17/2013 [Start]  remove the lnCOmp combobox
  *.lnComp.ControlSource = 'ThisformSet.laComp'
  *B610249,1 TMI 02/17/2013 [End  ] 
  .lnLinkTxt.RowSource = 'ThisformSet.laType'
  .lnLinkTxt.ControlSource = 'ThisformSet.lnLinkTxt'
  .laData2.Keytextbox.ControlSource = 'ThisformSet.laData[2]'
  .lcTypDesc.ControlSource = 'ThisformSet.lcTypDesc'
  .lcTypeDesc.ControlSource = 'ThisformSet.lcTypeDesc'
  .laData3.ControlSource   = 'ThisformSet.laData[3]'
  .lcAcctcode.Keytextbox.ControlSource = 'Thisformset.lcGLAcc'
ENDWITH 

*- End of lfSetControlSource.

*!*************************************************************
*! Name      : lfvComp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Validat the selected company
*!*************************************************************
FUNCTION lfvComp
PARAMETERS loFormSet,loFld


IF loFormSet.laComp[loFormSet.lnComp,2] <> 'NOCOMP'
  llNothing = lfGLFiles(loFormSet,ALLTRIM(loFormSet.laComp[loFormSet.lnComp,2]))
ENDIF  

*!*************************************************************
*! Name      : lfvLinkTxt
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Validat the link type .
*!*************************************************************
FUNCTION lfvLinkTxt
PARAMETERS loFormSet,loFld

*!*	LOCAL lcFlt
*!*	SELECT(loFormSet.lcBaseGL)
*!*	lcFlt = FILTER()
*!*	SELECT (loFormSet.lcBaseFile)
*!*	SET FILTER TO &lcFlt 
*!*	LOCATE

*loFormSet.laData[1] = loFormSet.laType[loFormSet.lnLinkTxt,2]
loFormSet.laData[1] = loFormSet.lnLinkTxt

*-- this means that 'Select Link type' is found in the popup
*   and it is not the selected item
IF ASCAN(loFormSet.laType,'00') <> 0 AND !(loFormSet.lnLinkTxt == '00')
  DIMENSION loFormSet.laType[ALEN(loFormSet.laType,1)-1,2]
ENDIF   &&IF ASCAN(laType,'00') <......

IF loFormSet.llSetUp
  SELECT(loFormSet.lcKeyCat)
  SET FILTER TO LinkType = loFormSet.laData[1]
  SHOW GET pbGlAcc ENABLE && will not be changed until the SM module has been converted
  SHOW GET lcGLAcc ENABLE
  GO TOP
ENDIF     &&IF llSetUp

*!*************************************************************
*! Name      : lfvData_2
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Validat the screen key laData[2] .
*!*************************************************************
FUNCTION lfvData_2
PARAMETERS loFormSet,loFld
PRIVATE llOption,lcLinkType,lcFltExp

lcFile_Ttl = loFormSet.BrowseTitle            
loFld.Refresh()
loFormSet.llBrowse = loFld.Selectedfrombrowse
llOption = .F. 
lcLinkType = ''
lcBaseGL = loFormSet.lcBaseGL

*- Validate that a link code is not less than 6 characters
IF LEN(ALLTRIM(loFormSet.laData[2])) < 6 AND !loFormSet.llBrowse AND !("?" $ loFormSet.laData[2])
  *-- "GL Link code coud not be less than 6 characters"
  *-- <Browse> , <Reenter>
  IF gfModalGen("QRM00346B00014","Dialog") = 1
    loFormSet.llBrowse = .T.
  ELSE
    loFormSet.laData[2] = ''
    *_CUROBJ = _CUROBJ      
  ENDIF
ENDIF       
IF loFormSet.llBrowse OR '?' $ loFormSet.laData[2] OR (!EMPTY(loFormSet.laData[2]) AND loFormSet.lnLinkTxt=='00')
  llNothing = lfGLLinkBrw(loFormSet)
  loFormSet.laData[1] = IIF(llNothing,&lcBaseGL..LinkType ,loFormSet.laData[1])  
  loFormSet.laData[2] = IIF(llNothing,&lcBaseGL..Link_Code,'')
  loFormSet.llBrowse  = .F.
  *IF !llNothing
  *  _CUROBJ = _CUROBJ
  *ENDIF
ENDIF       &&IF loFormSet.llBrowse OR '?' $ laData[2] OR......

lcLinkType = loFormSet.laData[1]

IF !EMPTY(loFormSet.laData[2])
  *x
  IF !SEEK(loFormSet.laData[1]+loFormSet.laData[2],loFormSet.lcBaseFile)
    **** \!\<Browse;\<Add;\?\<Reenter
    lnResp = gfModalGen('INM00001B02004','DIALOG','Link Type\Link Code :'+loFormSet.laData[2])
    DO case
    CASE lnResp = 1
      IF !loFormSet.oToolBar.cmdFind.Click()
        loFormSet.laData[2] = ' '
        llOption = lcLinkType <> '00'
      ENDIF 
    CASE lnResp = 2  
      loFormSet.ChangeMode('A')
      llOption = .T.
    CASE lnResp = 3
      loFormSet.laData[2] = ' '
      llOption = .T.
    ENDCASE 
  ELSE
    *- the key is there, go to view mode
    loFormSet.ChangeMode('V')
  ENDIF 
  *x
ENDIF     &&!EMPTY(loFormSet.laData[2])

*lnLastKey = LASTKEY()

IF EMPTY(loFormSet.laData[2]) OR llOption 

  loFormSet.laData[1] = lcLinkType

  IF EMPTY(loFormSet.laData[2])
    IF EMPTY(oAriaApplication.ActiveCompanyID)
      loFormSet.lnComp = ALEN(loFormSet.laComp,1)
      STORE 'DISABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat
    ELSE
      STORE 'ENABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat
    ENDIF      &&IF EMPTY(oAriaApplication.ActiveCompanyID)
     *SHOW GET loFormSet.lnLinkTxt &lcTypeStat
    loFormSet.Ariaform1.lnLinkTxt.Enabled = loFormSet.lcTypeStat = 'ENABLE'
    *SHOW GET loFormSet.laData[2] &lcLinkStat
    loFormSet.Ariaform1.laData2.Enabled = loFormSet.lcTypeStat = 'ENABLE'
    *loFormSet.Ariaform1.laData2.Keytextbox.setfocus()  
    RETURN .F.
    *_CUROBJ = OBJNUM(loFormSet.laData[2])
  ELSE
    *_CUROBJ = OBJNUM(lcTypeDesc)
    *loFormSet.Ariaform1.lcTypeDesc.Setfocus()
  ENDIF
ENDIF       &&IF EMPTY(laData[2]) AND LASTKEY() = 13

loFormSet.lcTypDesc = loFormSet.Ariaform1.lnLinkTxt.DisplayValue + ' ' + ALLTRIM(loFormSet.laData[2])
loFormSet.Ariaform1.Refresh()

*!*************************************************************
*! Name      : lfGLCpyScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Copying information from another GL.
*!*************************************************************
FUNCTION lfGLCpyScr
PARAMETERS loFormSet
*-- lcWName  variable that hold the SMGLLNKC Screen name
lcWName  = "SMGLCPY"
*-- lcWTitl  variable that hold the SMGLLNKC Screen Title
lcWTitl  = "Copy Information From G/L Link code"
loFormSet.llBrowse = .F.

DO FORM (oariaapplication.ScreenHome + 'SM\SMGLLNKC.scx') WITH loFormSet

*!*************************************************************
*! Name      : lfvCpyLink
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Validate the Gl Link code (lcGLCpyLink Field)
*!             in Copying information screen 
*!*************************************************************
FUNCTION lfvCpyLink
PARAMETERS loFormSet

lcBaseGL = loFormSet.lcBaseGL
lnCurAlias = SELECT(0)
IF loFormSet.llBrowse OR !SEEK(loFormSet.laData[1]+loFormSet.lcCpyLink,loFormSet.lcBaseGL)
  llPckCopy = .T.
  loFormSet.llBrowse  = .F.
  loFormSet.lcCpyLink = IIF(lfGLLinkBrw(loFormSet),&lcBaseGL..Link_Code,SPACE(6))
ENDIF      &&IF loFormSet.llBrowse OR !SEEK(loFormSet.laData[1]+.........
SELECT(lnCurAlias)

*!*	*!*************************************************************
*!*	*! Name      : lfGLCpyAct
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 12/4/2012
*!*	*! Purpose   : Trap the keys in screen SMGLLNKC
*!*	*!*************************************************************
*!*	FUNCTION lfGLCpyAct

*!*	*-- This function called in activate snippet for screen packScr
*!*	*-- which use to copy data from another pack id

*!*	ON KEY LABEL ESCAPE DO lfGLCpyEsc

*!*	*!*************************************************************
*!*	*! Name      : lfGLCpyEsc
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 12/4/2012
*!*	*! Purpose   : Trap the Esacpe Key in screen SMGLLNKC
*!*	*!*************************************************************
*!*	FUNCTION lfGLCpyEsc

*!*	ON KEY LABEL ESCAPE

*!*	_CUROBJ = OBJNUM(pbCancel)
*!*	KEYBOARD "{ENTER}" CLEAR PLAIN

*!*************************************************************
*! Name      : lfGLLinkBrw
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Browse  existing link codes
*!*************************************************************
FUNCTION lfGLLinkBrw
PARAMETERS loFormSet

lcBaseGL = loFormSet.lcBaseGL

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "linktype,link_code"

lcBrFields = "Type = IIF(ASCAN(loFormSet.laType,&lcBaseGL..LinkType)>0,loFormSet.laType[ASUBSCRIPT(loFormSet.laType,ASCAN(loFormSet.laType,&lcBaseGL..LinkType),1),1],'') :H='Type':15,"+;
             "Link_Code    :H='Link code',"+;
             "Description=&lcBaseGL..LinkDesc :H='Description'"

lcFile_Ttl  = 'Link codes'

SELECT (lcBaseGL)
LOCATE
DECLARE laTemp[1]

DO CASE
  CASE oAriaApplication.ActiveModuleID = "AR"
    lcBrFor = "FOR INLIST(LinkType,'01','02') AND !DELETED()"
  CASE oAriaApplication.ActiveModuleID = "SO"
    lcBrFor = "FOR INLIST(LinkType,'01','02') AND !DELETED()"
  CASE oAriaApplication.ActiveModuleID = "PO"
    lcBrFor = "FOR INLIST(LinkType,'05') AND !DELETED()"
  CASE oAriaApplication.ActiveModuleID = "MA"
    lcBrFor = "FOR INLIST(LinkType,'04','05') AND !DELETED()"
  CASE oAriaApplication.ActiveModuleID = "MF"
    lcBrFor = "FOR INLIST(LinkType,'05') AND !DELETED()"
  CASE oAriaApplication.ActiveModuleID = "IC"
    lcBrFor = "FOR INLIST(LinkType,'03') AND !DELETED()"
  CASE oAriaApplication.ActiveModuleID = "SM"
    lcBrFor = "FOR INLIST(LinkType,'01','02','03','04','05') AND !DELETED()"
ENDCASE    

llReturn  = gfBrows(IIF(loFormSet.laData[1]='00' OR EMPTY(loFormSet.laData[1]),lcBrFor,"loFormSet.laData[1]"), 'linktype', 'laTemp',lcFile_Ttl)

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvTypeDesc
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Validate the link type Account description
*!*************************************************************
FUNCTION lfvTypeDesc
PARAMETERS loFormSet,loFld

PRIVATE lnRecNo

SELECT(loFormSet.lcKeyCat)
lnRecNo = RECNO(loFormSet.lcKeyCat)
REPLACE ALL LinkDesc WITH loFormSet.lcTypeDesc,;
            cStatus  WITH IIF(nRecNo=0,'A','M')

IF lnRecNo<=RECCOUNT(loFormSet.lcKeyCat)
  GOTO lnRecNo
ENDIF  

*!*********************************************************
*! Name      : lfvGLAcc
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Validate the GL Account
*!*************************************************************
FUNCTION lfvGLAcc
PARAMETERS loFormSet,loFld
lcKeyCat = loFormSet.lcKeyCat
lcOldVal = loFld.Keytextbox.OldValue

loFormSet.llBrowse = loFld.Selectedfrombrowse 

IF !loFormSet.llOtherVer AND (loFormSet.ActiveMode = 'E' OR loFormSet.ActiveMode = 'A')
  *loFormSet.lcGLAcc = &lcKeyCat..GLAcnt
  loFormSet.lcGLAcc = loFld.KeyTextBox.Value

  *llEmptyAcc = loFormSet.lcEmptyMas = loFormSet.lcGLAcc
  llEmptyAcc = VAL(CHRTRAN(loFormSet.lcGLAcc,'-',''))=0 AND !ISALPHA(LEFT(loFormSet.lcGLAcc,1)) and LEFT(loFormSet.lcGLAcc,1)<>'?'
  IF llEmptyAcc
    loFormSet.lcGLAcc = ''
  ENDIF

  IF !loFormSet.llOtherVer AND ((!llEmptyAcc AND !SEEK(loFormSet.lcGLAcc,loFormSet.lcGLChart)) OR loFormSet.llBrowse )
    loFormSet.llBrowse  = .F.
    SELECT (loFormSet.lcGLChart)
    GO TOP
    IF EOF()
      *-- There are no records to browse.
      *-- OK
      = gfModalGen("INM44032B00000","Dialog")  
      RETURN .F.
      *_CUROBJ = _CUROBJ
    ELSE
      llNoThing = lfGLChrBrw(loFormSet)
      IF !llNothing 
        SELECT (loFormSet.lcKeyCat)
        loFormSet.lcGLAcc = lcOldVal
        REPLACE GLAcnt WITH lcOldVal
        RETURN .F.
        *_CUROBJ = _CUROBJ
      ENDIF
    ENDIF
  
  ENDIF    && IF !llOtherVer AND (loFormSet.llBrowse OR (!EMPTY(lcGLAcc) AND.........

  IF !(loFormSet.lcGLAcc == lcOldVal)
    *= gfUpdate()
      SELECT (loFormSet.lcKeyCat)
      IF &lcKeyCat..nRecNo = 0
        REPLACE &lcKeyCat..cStatus   WITH 'A'      ,;
                &lcKeyCat..GLAcnt    WITH loFormSet.lcGLAcc  ,;
                &lcKeyCat..LinkDesc  WITH loFormSet.lcTypeDesc
      ELSE
        REPLACE &lcKeyCat..cStatus   WITH 'M',;
                &lcKeyCat..GLAcnt    WITH loFormSet.lcGLAcc
      ENDIF      

  ENDIF  &&IF !(lcGLAcc == lcOldVal)
ELSE

  SELECT (loFormSet.lcKeyCat)
  IF &lcKeyCat..nRecNo = 0
    REPLACE &lcKeyCat..cStatus   WITH 'A'
  ELSE
    REPLACE &lcKeyCat..cStatus   WITH 'M'
  ENDIF

ENDIF &&IF !llOtherVer  

*- Refresh the bottom of the screen
lfFormAfterRowColumnChange(loFormSet)


*!*************************************************************
*! Name      : lfGLChrBrw
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Browse account chart
*!*************************************************************
FUNCTION lfGLChrBrw
PARAMETERS loFormSet

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.
DIMENSION laTemp[1]

lnCurAlias = SELECT(0)

lcFields    = loFormSet.lcCodeFld+','+loFormSet.lcDescFld

lcBrFields  = loFormSet.lcCodeFld + ":H='GL Account'," + loFormSet.lcDescFld + ":H='Description'"

lcFile_Ttl  = 'GL Accounts'

SELECT (loFormSet.lcGLChart)

llReturn = AriaBrow(.F.,lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,loFormSet.lcGLChart,.F.)

IF llReturn
  loFormSet.lcGLAcc = laBrow[1,1]
  loFormSet.lcGlDesc= laBrow[1,2]
ENDIF

SELECT(lnCurAlias)

RETURN llReturn

*!*	*!*************************************************************
*!*	*! Name      : lfCatBrow
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      12/4/2012
*!*	*! Purpose   : Category keys for a link code
*!*	*!*************************************************************
*!*	*! Calls     : 
*!*	*!             Procedures : ....
*!*	*!             Functions  : ....
*!*	*!*************************************************************
*!*	*! Passed Parameters  : NONE
*!*	*!*************************************************************
*!*	*! Returns            : NONE
*!*	*!*************************************************************
*!*	*! Example   : =lfCatBrow()
*!*	*!*************************************************************

*!*	FUNCTION lfCatBrow

*!*	lnCurAlias = SELECT(0)
*!*	SELECT (lcKeyCat)
*!*	GO TOP

*!*	lnBrRecNo = RECNO()

*!*	IF llOtherVer
*!*	  BROWSE FIELDS cMarker =IIF(RECNO()=lnBrRecNo,'>',' ')  :H=' ':R:1:W=.F.,;
*!*	                CatgKey  :W=.F.:R   :H="Category Key",;
*!*	                CatgDesc :W=.F.:R:28:H="Description" ,;
*!*	                Glacnt   :W=(loFormSet.ActiveMode = 'E' OR loFormSet.ActiveMode = 'A') AND lfOldValue():P=lcAcMask:H="GL. Account" :V=lfvGLAcc() :F,;
*!*	                GlaccDesc:W=(loFormSet.ActiveMode = 'E' OR loFormSet.ActiveMode = 'A') AND loFormSet.lcGLVer= 'O':40:H="Description"  ;
*!*	         SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
*!*	         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfVBrow()    ;         
*!*	         WINDOW (lcGlLink2) IN WINDOW (gcBaseWind)         
*!*	ELSE
*!*	  BROWSE FIELDS cMarker =IIF(RECNO()=lnBrRecNo,'>',' ')  :H=' ':R:1:W=.F.,;
*!*	                CatgKey  :W=.F.:R   :H="Category Key",;
*!*	                CatgDesc :W=.F.:R:28:H="Description" ,;
*!*	                Glacnt   :W=(loFormSet.ActiveMode = 'E' OR loFormSet.ActiveMode = 'A') AND lfOldValue():P=lcAcMask:H="GL. Account" :V=lfvGLAcc() :F,;
*!*	                GLAccDsc=IIF(loFormSet.ActiveMode = 'S',SPACE(40),IIF(SEEK(&lcKeyCat..Glacnt,lcGLChart),;
*!*	                             EVAL(lcGLChart+'.'+loFormSet.lcDescFld),''));
*!*	                             :R:40:H="Description" ;
*!*	         SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
*!*	         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfVBrow()    ;         
*!*	         WINDOW (lcGlLink2) IN WINDOW (gcBaseWind)         
*!*	ENDIF

*!*	= lfwBrow()

*!*	*!*************************************************************
*!*	*! Name      : lfOldValue
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      12/4/2012
*!*	*! Purpose   : To save the old value befor modifying it
*!*	*!*************************************************************
*!*	*! Calls     : 
*!*	*!             Procedures : ....
*!*	*!             Functions  : ....
*!*	*!*************************************************************
*!*	*! Passed Parameters  : NONE
*!*	*!*************************************************************
*!*	*! Returns            : NONE
*!*	*!*************************************************************
*!*	*! Example   : =lfOldValue()
*!*	*!*************************************************************

*!*	FUNCTION lfOldValue

*!*	lcOldVal = EVAL(VARREAD())

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : change mode function
************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN 
ENDIF 
LOCAL lnCurAlias
lnCurAlias = SELECT(0)

lcScFields = loFormSet.lcScFields
IF loFormSet.ActiveMode $ 'VE'
  SELECT (loFormSet.lcBaseFile)
  SCATTER FIELDS &lcScFields TO loFormSet.laData
  SELECT (loFormSet.lcBaseGL)
  =SEEK(loformset.ladata[1]+loformset.ladata[2])
ENDIF 
loFormSet.Ariaform1.lcAcctcode.Enabled =  loFormSet.ActiveMode $ 'AE'


PRIVATE lcFilToUse,lcFltExp
lcFilToUse = IIF(loFormSet.ActiveMode = 'V',loFormSet.lcBaseGL, loFormSet.lcKeyCat)


lcAllGL = loFormSet.lcAllGL
 
DO CASE
  *-- Select mode
  CASE loFormSet.ActiveMode = 'S'  
    SELECT (loFormSet.lcBaseFile)
    SCATTER FIELDS &lcScFields TO loFormSet.laData BLANK 

    IF loFormSet.llSetUp
      glQuitting = .T.
      loFormSet.release()
      *CLEAR READ
    ELSE
      CREATE TABLE (oAriaApplication.WorkDir + loFormSet.lcKeyCat);
       (Link_Code C(6),LinkType C(2),LinkDesc C(30),CatgKey C(3),;
        CatgDesc C(30),GLAcnt C(24),GLAccDesc C(40),nRecNo N(4),cStatus C(1))
    
      STORE SPACE(0) TO loFormSet.lcTypeDesc,loFormSet.lcGLAcc,loFormSet.lcGLDesc,loFormSet.lcCpyLink
      STORE "DISABLE" TO loFormSet.lcGLDscStat,loFormSet.lcTypeStat,loFormSet.lcLinkStat
      STORE .F. TO loFormSet.llChldComp
      STORE 0 To loFormSet.lnBrRecNo

      loFormSet.laData[2] = ' '
      loFormSet.lcTypDesc = 'Description'      

      IF EMPTY(oAriaApplication.ActiveCompanyID)
        STORE 'DISABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat
      ELSE
        STORE 'ENABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat
      ENDIF    &&IF EMPTY(oAriaApplication.ActiveCompanyID)
    
      = lfTypeArr(loFormSet)
      IF EMPTY(oAriaApplication.ActiveCompanyID)
        = lfGLFiles(loFormSet)
      ENDIF   &&IF EMPTY(oAriaApplication.ActiveCompanyID) OR llSetUp
    ENDIF   &&IF llSetUp    
    IF oAriaApplication.ActiveModuleID = 'SM'
      IF loFormSet.llSetUp
        *_CUROBJ = OBJNUM(loFormSet.lnLinkTxt)
        loFormSet.Ariaform1.lnLinkTxt.Enabled = .T.
        loFormSet.Ariaform1.lnLinkTxt.Setfocus()
      ELSE
        *_CUROBJ = OBJNUM(loFormSet.lnComp)
        *B610249,1 TMI 02/17/2013 [Start]  remove the lnCOmp combobox
        *loFormSet.Ariaform1.lnComp.Enabled = .T.
        *loFormSet.Ariaform1.lnComp.Setfocus()
        *B610249,1 TMI 02/17/2013 [End  ] 
      ENDIF
    ELSE
      *_CUROBJ = OBJNUM(loFormSet.lnLinkTxt)
      loFormSet.Ariaform1.lnLinkTxt.Enabled = .T.
      loFormSet.Ariaform1.lnLinkTxt.Setfocus()
    ENDIF

  *-- View mode
  CASE loFormSet.ActiveMode = 'V'
    
    loFormSet.lcTypeDesc = loFormSet.laData[3]
    loFormSet.laData[3]  = &lcALLGL..LinkDesc
	*loFormSet.lnLinkTxt  = ASUBSCRIPT(loFormSet.laType,ASCAN(loFormSet.laType,loFormSet.laData[1]),1)
	loFormSet.lnLinkTxt  = loFormSet.laData[1]
    STORE "DISABLE" TO loFormSet.lcGLDscStat,loFormSet.lcTypeStat,loFormSet.lcLinkStat
    
  *-- Edit mode
  CASE loFormSet.ActiveMode = 'E'
    
    loFormSet.lcTypeDesc = loFormSet.laData[3]
    loFormSet.laData[3]  = &lcALLGL..LinkDesc
    loFormSet.lcGLDscStat  = IIF(loFormSet.lcGLVer=='O','ENABLE','DISABLE')
    STORE "DISABLE" TO loFormSet.lcTypeStat,loFormSet.lcLinkStat

  *-- Add mode
  CASE loFormSet.ActiveMode = 'A'
    IF loFormSet.llSetup
      loFormSet.laData[2] = 'DEFDEF'
      STORE 'Default G/L. link code' TO loFormSet.laData[3],loFormSet.lcTypeDesc
      STORE "ENABLE"  TO loFormSet.lcTypeStat
      STORE "DISABLE" TO loFormSet.lcLinkStat
    ELSE
      STORE "DISABLE" TO loFormSet.lcTypeStat,loFormSet.lcLinkStat

      SELECT (loFormSet.lcBaseGL)
      lcFltExp = SET('FILTER')
      SET FILTER TO 
      IF SEEK('00'+loFormSet.laData[2])
        loFormSet.laData[3]  = &lcALLGL..LinkDesc
        *SHOW GET loFormSet.laData[3] DISABLE
        loFormSet.Ariaform1.laData3.Enabled = .F.
      ENDIF
      SET FILTER TO &lcFltExp

    ENDIF   &&IF llSetup
    
    *-- "Do you wish to copy from another G/L. Link code."
    *-- <YES>, <NO>
    IF !loFormSet.llSetUp AND SEEK(loFormSet.laData[1],loFormSet.lcBaseGL) AND gfModalGen("INM00295B00006","Dialog") = 1
      = lfGLCpyScr(loFormSet)
    ENDIF
    
    loFormSet.lcGLDscStat  = IIF(loFormSet.lcGLVer=='O','ENABLE','DISABLE')

ENDCASE

*SHOW GET loFormSet.lnLinkTxt &lcTypeStat*X*
loFormSet.Ariaform1.lnLinkTxt.Enabled = loFormSet.lcTypeStat = 'ENABLE'
loFormSet.Ariaform1.laData2.Enabled = 'ENABLE' = loFormSet.lcLinkStat

lcBaseGL = loFormSet.lcBaseGL
IF loFormSet.ActiveMode = 'V' OR loFormSet.ActiveMode = 'A'
  IF !loFormSet.llSetUp
    lcLinkCode  = IIF(EMPTY(loFormSet.lcCpyLink),loFormSet.laData[2],loFormSet.lcCpyLink)
    lcSelCode   = IIF(EMPTY(loFormSet.lcCpyLink),'&lcBaseGL..Link_Code','loFormSet.laData[2]')
    lcRecNoExp  = IIF(EMPTY(loFormSet.lcCpyLink),'RECNO()','0')
    lcStatusExp = IIF(EMPTY(loFormSet.lcCpyLink),"'S'","'A'")
    SET ORDER TO 0 IN (lcBaseGL)

    SELECT EVAL(lcSelCode) AS Link_Code,LinkType,LinkDesc,CatgKey,CatgDesc,;
           GLAcnt,GLAccDesc,EVAL(lcRecNoExp) AS nRecNo,EVAL(lcStatusExp) AS cStatus ;
      FROM (lcBaseGL) INTO DBF (oAriaApplication.WorkDir + loFormSet.lcKeyCat);
      WHERE LinkType+Link_Code = loFormSet.laData[1]+lcLinkCode;
    UNION ;
    SELECT loFormSet.laData[2],loFormSet.laData[1],loFormSet.lcTypeDesc,CatgKey,CatgDesc,;
           SPACE(24),SPACE(40),0,'S';
      FROM SyGlCatg;
      WHERE !EMPTY(CatgType) ;
      AND   CatgType = loFormSet.laData[1] AND CatgKey NOT IN;
            (SELECT CatgKey FROM (lcBaseGL);
             WHERE LinkType+Link_Code = loFormSet.laData[1]+lcLinkCode)
  ELSE

    SELECT loFormSet.laData[2] AS Link_Code,CatgType AS LinkType,;
           loFormSet.lcTypeDesc AS LinkDesc,CatgKey,CatgDesc,;
           SPACE(24) AS GLAcnt,SPACE(40) AS GLAccDesc,0 AS nRecNo,;
           'S' AS cStatus ;
      FROM SyGLCatg INTO DBF (oAriaApplication.WorkDir + loFormSet.lcKeyCat) ;
      WHERE !EMPTY(CatgType)

    SELECT (loFormSet.lcKeyCat)
    SET FILTER TO LinkType = loFormSet.laData[1]
    SHOW GET pbGlAcc DISABLE && will not be changed until the SM module has been converted
    SHOW GET lcGLAcc DISABLE
  ENDIF      &&IF !llSetUp
  SET ORDER TO GL_Link1 IN (lcBaseGL)
  
  SELECT (loFormSet.lcKeyCat)
  LOCATE 
ENDIF   &&IF loFormSet.ActiveMode = 'V' OR loFormSet.ActiveMode = 'A'

SELECT(loFormSet.lcKeyCat)

*= lfCatBrow()

*- Set screen control Sources
=lfSetControlSource(loFormSet)

*- update grid data source
=lfSetGridDataSource(loFormSet)

loFormSet.Ariaform1.Refresh()
SELECT (lnCurAlias)

*- End of lfChangeMode.

************************************************************
*! Name      : lfFormBeforeSave
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : Before save validation 
************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet

PRIVATE llContinue
llContinue = .T.

SELECT(loFormSet.lcKeyCat)
SET FILTER TO
lcKeyCat = loFormSet.lcKeyCat
SCAN 
  IF EMPTY(GLAcnt) AND ;
     (LinkType <> '05' OR (LinkType = '05' AND !(CatgKey $ '022,023,024,025,026')))
    llContinue = .F.
    *-- GL Account for any category can not be left empty
    *-- <OK>
    = gfModalGen("INM00294B00000","Dialog")  
    loFormSet.laData[1] = &lcKeyCat..LinkType
    *loFormSet.lnLinkTxt = ASUBSCRIPT(loFormSet.laType,ASCAN(loFormSet.laType,loFormSet.laData[1]),1)
    loFormSet.lnLinkTxt = loFormSet.laData[1]
    SET FILTER TO LinkType = loFormSet.laData[1]
    EXIT
  ENDIF
  IF !loFormSet.llOtherVer AND !SEEK(ALLTRIM(GLAcnt),loFormSet.lcGLChart)
    llContinue = .F.
    *-- Account x not found in chart of account.
    *-- <OK>
    = gfModalGen("INM00316B00000","Dialog",ALLTRIM(GLAcnt))
    loFormSet.laData[1] = &lcKeyCat..LinkType
    *loFormSet.lnLinkTxt = ASUBSCRIPT(loFormSet.laType,ASCAN(loFormSet.laType,loFormSet.laData[1]),1)
    loFormSet.lnLinkTxt = loFormSet.laData[1]
    SET FILTER TO LinkType = loFormSet.laData[1]
    EXIT
  ENDIF
ENDSCAN

RETURN llContinue
*- End of lfFormBeforeSave.

*!*************************************************************
*! Name      : lpSavscr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : To make local save.
*!*************************************************************
FUNCTION lpSavscr
PARAMETERS loFormSet

*IF llContinue
SELECT (loFormSet.lcBaseGL)
lcActFltr = SET('FILTER')
SET FILTER TO
SET ORDER TO GL_Link1 IN (loFormSet.lcBaseGL)
IF !SEEK('00'+loFormSet.laData[2],loFormSet.lcBaseGL)
  APPEND BLANK
ENDIF
lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData FIELDS &lcScFields
REPLACE LinkType WITH "00"

SET ORDER TO GL_Link IN (loFormSet.lcBaseGL)
=gfTmp2Mast(loFormSet.lcBaseGL,loFormSet.lcKeyCat,'','')
SET ORDER TO GL_Link1 IN (loFormSet.lcBaseGL)
=SEEK(loFormSet.laData[1]+loFormSet.laData[2],loFormSet.lcBaseGL)
SET FILTER TO &lcActFltr.
*ENDIF  &&IF llContinue

SELECT (loFormSet.lcBaseGL)
gfTableUpdate()

*llCSave = llContinue
*- End of lpSavscr

*!*************************************************************
*! Name      : lpDelScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : To make local delete.
*!*************************************************************
FUNCTION lpDelScr
PARAMETERS loFormSet

PRIVATE lnCurAlias,lcTag,lnCurRec,llOpen,lnI,llContinue

llContinue = .T.

lnCurAlias = SELECT(0)

IF ALLTRIM(loFormSet.laData[2]) == 'DEFDEF'
  *-- Cannot delete the default link code.
  *-- <OK>
  = gfModalGen("INM00300B00000","Dialog")  
  llContinue = .F.
ELSE
  *-- this part for checking the existence of the link code in the
  *   mentioned files in laGLFiles array

  *-- Deleting this code requires checking its checking its existence
  *-- on some files. This process may take a few minutes.
  *--<Check>,<Ignore cheking>
  
*!*	  you need to write a function that check for all files in the system file either in A27 or A4xp
*!*	  FOR A27 you use the LOCATE command 
*!*	  FOR A4xp , you use the sqlrun function TO return the lines that has the certian link code
*!*	  let this function at the end of the work
  
  
  IF gfModalGen("INM00296B00032","Dialog") = 1    
    *- fox case
    IF USED('SYDFLFLD')
      USE IN SYDFLFLD
    ENDIF 
    =gfOpenFile(oAriaApplication.SysPath+'SYDFLFLD','','SH')
    SELECT SYDFLFLD
    LOCAL lnI
    lnI = 0
    SCAN FOR CFLD_NAME = 'LINK_CODE ' OR CFLD_NAME = 'GL_LINK   ' 
      IF CFILE_NAM = 'GL_LINK'
        LOOP
      ENDIF
      lnI = lnI + 1    
      DIMENSION laGLFiles[lnI,3]
      laGLFiles[lnI,1] = ALLTRIM(SYDFLFLD.CFILE_NAM)
      laGLFiles[lnI,2] = ALLTRIM(SYDFLFLD.CFLD_NAME )
      laGLFiles[lnI,3] = 'FOX'
    ENDSCAN
    USE IN SYDFLFLD
    =gfOpenFile(oAriaApplication.cAria4syspath+'SYDFLFLD','','SH')
    SCAN FOR CFLD_NAME = 'LINK_CODE ' OR CFLD_NAME = 'GL_LINK   ' 
      lnI = lnI + 1    
      DIMENSION laGLFiles[lnI,3]
      laGLFiles[lnI,1] = ALLTRIM(SYDFLFLD.CFILE_NAM)
      laGLFiles[lnI,2] = ALLTRIM(SYDFLFLD.CFLD_NAME )
      laGLFiles[lnI,3] = 'SQL'
    ENDSCAN
    USE IN SYDFLFLD


    FOR lnI = 1 TO ALEN(laGLFiles,1)
      IF laGLFiles[lnI,3] = 'FOX'
        IF FILE(oAriaApplication.DataDir+laGLFiles[lnI,1]+'.DBF')
          llOpen = gfOpenTable(laGLFiles[lnI,1])
          SELECT(laGLFiles[lnI,1])
          lcFld = laGLFiles[lnI,2]
          LOCATE FOR &lcFld. = loFormSet.laData[2]
          
          IF FOUND(laGLFiles[lnI,1])
            = gfCloseTable(laGLFiles[lnI,1])
            llContinue = .F.
            EXIT
          ELSE
            = gfCloseTable(laGLFiles[lnI,1])
          ENDIF      &&IF FOUND(laGLFiles[lnI])
        ENDIF
      ELSE
        
        lcSql = "Select "+laGLFiles[lnI,2] +" From "+laGLFiles[lnI,1] + " where " + laGLFiles[lnI,2] + "='"+loFormSet.laData[2]+"'"
        gfOpenTable(laGLFiles[lnI,1],'','SH')
        =gfSqlRun(lcSql,laGLFiles[lnI,1],.T.,"Tempfl")        
        SELECT Tempfl
        LOCATE
        IF !EOF()
          llContinue = .F.
          USE IN Tempfl
          EXIT
        ELSE
          USE IN Tempfl
        ENDIF
        gfCloseTable(laGLFiles[lnI,1])
      ENDIF       
    ENDFOR     &&FOR lnI = 1 TO ALEN(laGLFiles)
    
    IF !llContinue
      *-- This Link code is used in one or more of the data files,
      *-- cannot delete.
      *-- <OK>
      = gfModalGen("INM00297B00000","Dialog")  
    ENDIF 

  ENDIF    &&IF gfModalGen("INM00296B00032","Dialog") = 1
ENDIF

IF llContinue
  SELECT (loFormSet.lcBaseGL)
  lcTag = ORDER(loFormSet.lcBaseGL)

  SET ORDER TO 0 
  lnCurRec = RECNO(loFormSet.lcBaseGL)
  SCAN FOR LinkType+Link_Code = loFormSet.laData[1]+loFormSet.laData[2]
    BLANK  
    DELETE
  ENDSCAN
  IF lnCurRec<=RECCOUNT(loFormSet.lcBaseGL)
    GOTO lnCurRec
  ENDIF    &&IF lnCurRec<=RECCOUNT(lcBaseGL)

  SET ORDER TO GL_Link IN (loFormSet.lcBaseGL)
  IF SEEK(loFormSet.laData[2],loFormSet.lcBaseGL)
    LOCATE REST WHILE Link_Code = loFormSet.laData[2] FOR !(LinkType=='00')
    IF !FOUND(loFormSet.lcBaseGL)
      = SEEK(loFormSet.laData[2],loFormSet.lcBaseGL)
      BLANK
      DELETE
    ENDIF
  ENDIF
  
  SELECT (loFormSet.lcBaseGL)
  gfTableUpdate()
   
  SET ORDER TO lcTag IN (loFormSet.lcBaseGL)
  SELECT(loFormSet.lcKeyCat)
  DELETE ALL
  *-- Return to "SELECT" mode
  *laScrMode        = .F.
  *loFormSet.ActiveMode = 'S'     = .T.
  loFormSet.ChangeMode('S')
  SELECT(lnCurAlias)
ENDIF  &&IF llContinue

SELECT (loFormSet.lcBaseFile)
RETURN llContinue
*- End of lpDelScr

*!*	*!*************************************************************
*!*	*! Name      : lfTrap
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 12/4/2012
*!*	*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*	*!*************************************************************
*!*	FUNCTION lfTrap

*!*	*-- THIS is function is called in deactivate snippet of the screen
*!*	*-- if the screen on top is the browse screen assign fuction to the key

*!*	IF WONTOP()  = lcBrTtl
*!*	  glFromBrow = .T.
*!*	  ON KEY LABEL TAB     DO lfBrTab
*!*	  ON KEY LABEL BACKTAB DO lfBrBack
*!*	ENDIF

*!*	*!*************************************************************
*!*	*! Name      : lfClrTrap
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 12/4/2012
*!*	*! Purpose   : Clearing the previous trapping
*!*	*!*************************************************************
*!*	FUNCTION lfClrTrap

*!*	*-- THIS is function is called in activate snippet of the screen
*!*	*-- if the screen on top is not the browse screen restore 
*!*	*-- the previous on key label 

*!*	IF glFromBrow
*!*	  =gfStopBrow()
*!*	ENDIF  

*!*	ON KEY LABEL TAB
*!*	ON KEY LABEL BACKTAB

*!*	*!*************************************************************
*!*	*! Name      : lfBrTab
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 12/4/2012
*!*	*! Purpose   : Trap the Tab Key
*!*	*!*************************************************************
*!*	FUNCTION lfBrTab

*!*	ON KEY LABEL TAB
*!*	ACTIVATE WINDOW ('gwcContrl1')
*!*	_CUROBJ = OBJNUM(pbTop)

*!*	*!*************************************************************
*!*	*! Name      : lfBrBack
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 12/4/2012
*!*	*! Purpose   : Trap the BackTab Key
*!*	*!*************************************************************
*!*	FUNCTION lfBrBack

*!*	ON KEY LABEL BACKTAB

*!*	DO CASE
*!*	  CASE loFormSet.ActiveMode = 'S'
*!*	    IF loFormSet.llSMMOD AND oAriaApplication.ActiveCompanyID = "SM"
*!*	      _CUROBJ = OBJNUM(loFormSet.lnComp)
*!*	    ELSE  
*!*	      _CUROBJ = OBJNUM(loFormSet.laData[2])
*!*	    ENDIF  
*!*	  CASE loFormSet.ActiveMode = 'V'
*!*	    ACTIVATE WINDOW ('gwcContrl1')
*!*	    _CUROBJ = OBJNUM(pbCls)
*!*	  CASE loFormSet.ActiveMode = 'E'
*!*	    IF loFormSet.llSMMOD AND oAriaApplication.ActiveCompanyID = "SM"
*!*	      _CUROBJ = OBJNUM(loFormSet.lnComp)
*!*	    ELSE  
*!*	      _CUROBJ = OBJNUM(loFormSet.laData[3])
*!*	    ENDIF  
*!*	  CASE loFormSet.ActiveMode = 'A'
*!*	    IF loFormSet.llSMMOD AND oAriaApplication.ActiveCompanyID = "SM"
*!*	      _CUROBJ = OBJNUM(loFormSet.lnComp)
*!*	    ELSE  
*!*	      _CUROBJ = OBJNUM(loFormSet.laData[3])
*!*	    ENDIF  
*!*	ENDCASE    

*!*	*!*************************************************************
*!*	*! Name      : lfwBrow
*!*	*! Developer : Ahmed Amer (AHM)
*!*	*! Date      : 08/28/97
*!*	*! Purpose   : adjust the label of pbsel button
*!*	*!*************************************************************
*!*	*! Calls     : 
*!*	*!             Procedures : ....
*!*	*!             Functions  : ....
*!*	*!*************************************************************
*!*	*! Passed Parameters  : None
*!*	*!*************************************************************
*!*	*! Returns            : None
*!*	*!*************************************************************
*!*	*! Example   : = lfwBrow()
*!*	*!*************************************************************

*!*	FUNCTION lfwBrow

*!*	lnBrRecNo = RECNO(lcKeyCat)
*!*	SHOW WINDOW (lcBrTtl) REFRESH SAME

*!*	*!*************************************************************
*!*	*! Name      : lfvBrow
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 12/4/2012
*!*	*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*	*!*************************************************************
*!*	FUNCTION lfvBrow

*!*	  =gfStopBrow()

*!*************************************************************
*! Name      : lfTypeArr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : prepare the Link Type arry according to the calling module
*!*************************************************************
FUNCTION lfTypeArr
PARAMETERS loFormSet

DO CASE
  CASE oAriaApplication.ActiveModuleID = "AR"
    DIMENSION loFormSet.laType[3,2]
    loFormSet.laType[1,1] = "Customer"
    loFormSet.laType[1,2] = "01"
    loFormSet.laType[2,1] = "Sales"
    loFormSet.laType[2,2] = "02"
    loFormSet.laType[3,1] = "Select link type"
    loFormSet.laType[3,2] = "00"
  CASE oAriaApplication.ActiveModuleID = "SO"
    DIMENSION loFormSet.laType[3,2]
    loFormSet.laType[1,1] = "Customer"
    loFormSet.laType[1,2] = "01"
    loFormSet.laType[2,1] = "Sales"
    loFormSet.laType[2,2] = "02"
    loFormSet.laType[3,1] = "Select link type"
    loFormSet.laType[3,2] = "00"
  CASE oAriaApplication.ActiveModuleID = "PO"
    DIMENSION loFormSet.laType[2,2]
    loFormSet.laType[1,1] = "Work in process"
    loFormSet.laType[1,2] = "05"    
    loFormSet.laType[2,1] = "Select link type"
    loFormSet.laType[2,2] = "00"
  CASE oAriaApplication.ActiveModuleID = "MA"
    DIMENSION loFormSet.laType[3,2]
    loFormSet.laType[1,1] = "Material"
    loFormSet.laType[1,2] = "04"    
    loFormSet.laType[2,1] = "Work in process"
    loFormSet.laType[2,2] = "05"    
    loFormSet.laType[3,1] = "Select link type"
    loFormSet.laType[3,2] = "00"
  CASE oAriaApplication.ActiveModuleID = "MF"
    DIMENSION loFormSet.laType[2,2]
    loFormSet.laType[1,1] = "Work in process"
    loFormSet.laType[1,2] = "05"    
    loFormSet.laType[2,1] = "Select link type"
    loFormSet.laType[2,2] = "00"
  CASE oAriaApplication.ActiveModuleID = "IC"
    DIMENSION loFormSet.laType[2,2]
    loFormSet.laType[1,1] = "Style"
    loFormSet.laType[1,2] = "03"    
    loFormSet.laType[2,1] = "Select link type"
    loFormSet.laType[2,2] = "00"
  CASE oAriaApplication.ActiveModuleID = "SM"
    DIMENSION loFormSet.laType[6,2]
    loFormSet.laType[1,1] = "Customer"
    loFormSet.laType[1,2] = "01"    
    loFormSet.laType[2,1] = "Sales"
    loFormSet.laType[2,2] = "02"
    loFormSet.laType[3,1] = "Style"        
    loFormSet.laType[3,2] = "03"            
    loFormSet.laType[4,1] = "Material"            
    loFormSet.laType[4,2] = "04"                
    loFormSet.laType[5,1] = "Work in process"    
    loFormSet.laType[5,2] = "05"        
    loFormSet.laType[6,1] = "Select link type"
    loFormSet.laType[6,2] = "00"
ENDCASE

*B610249,1 TMI 02/17/2013 [Start] allow the screen to show all link types regardless the selected module
DIMENSION loFormSet.laType[6,2]
loFormSet.laType[1,1] = "Customer"
loFormSet.laType[1,2] = "01"    
loFormSet.laType[2,1] = "Sales"
loFormSet.laType[2,2] = "02"
loFormSet.laType[3,1] = "Style"        
loFormSet.laType[3,2] = "03"            
loFormSet.laType[4,1] = "Material"            
loFormSet.laType[4,2] = "04"                
loFormSet.laType[5,1] = "Work in process"    
loFormSet.laType[5,2] = "05"        
loFormSet.laType[6,1] = "Select link type"
loFormSet.laType[6,2] = "00"
*B610249,1 TMI 02/17/2013 [End  ] 

*loFormSet.lnLinkTxt = ASUBSCRIPT(loFormSet.laType,ASCAN(loFormSet.laType,'00'),1)
loFormSet.lnLinkTxt = '00'
*SHOW GET loFormSet.lnLinkTxt &lcTypeStat
loFormSet.Ariaform1.lnLinkTxt.Enabled = loFormSet.lcTypeStat = 'ENABLE'

*!*************************************************************
*! Name      : lfGLFiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/4/2012
*! Purpose   : open the GL_Link file account chart file according to 
*!             the GL version(ARIA,SBT,Others) and to the active company
*!*************************************************************
FUNCTION lfGLFiles
PARAMETERS loFormSet,lcSelComp

PRIVATE llContinue,lcComp,lcSelComp
lcAllGL = loFormSet.lcAllGL
lcSysTmp = loFormSet.lcSysTmp
llContinue = .T.

=lfCloseFile(loFormSet.lcGLChart)

=lfCloseFile(loFormSet.lcBaseGl)

=lfCloseFile(loFormSet.lcAllGL)
  
IF loFormSet.llSetUp
  STORE 'DISABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat
  laCtrStat[10]  =  "DISABLE" 
  SHOW GET pbBrws  DISABLE  && will not be changed until the SM module has been converted
ELSE
  
  IF EMPTY(oAriaApplication.ActiveCompanyID)
    STORE 'DISABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat,lcpbBrwsSt
  ELSE
    STORE 'ENABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat,lcpbBrwsSt
  ENDIF
  *laCtrStat[10]  = lcpbBrwsSt   
  *SHOW GET pbBrws  &lcpbBrwsSt  &&*x*
  
ENDIF  &&IF EMPTY(lcCompID) OR !llGL_Link

lcComp  = IIF(TYPE('lcSelComp')=='C',lcSelComp,IIF(loFormSet.llSetUp,lcSetupCom,oAriaApplication.ActiveCompanyID))

IF oAriaApplication.ActiveModuleID == 'SM' 
  SELECT SycComp.cComp_ID+"-"+SycComp.cCom_Name,SycComp.cComp_ID;
   FROM SycComp INTO ARRAY loFormSet.laComp

  DIMENSION loFormSet.laComp[ALEN(loFormSet.laComp,1)+1,2]
  loFormSet.laComp[ALEN(loFormSet.laComp,1),1] = 'Select a company'
  loFormSet.laComp[ALEN(loFormSet.laComp,1),2] = 'NOCOMP'
ENDIF  &&IF oAriaApplication.ActiveModuleID == 'SM' AND !llSetUp

IF !EMPTY(lcComp)
  llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
  IF !loFormSet.llSetUp AND !llGL_Link 
    *-- System has not been linked to gl_link yet
    *-- <OK>
    = gfModalGen("INM00292B00000","Dialog")  
    llContinue = .F.
    lcBaseFile = ''
    IF oAriaApplication.ActiveModuleID == 'SM' 
      loFormSet.lnComp = ASUBSCRIPT(loFormSet.laComp,ASCAN(loFormSet.laComp,'NOCOMP'),1)
      STORE 'DISABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat
      *SHOW GET loFormSet.lnLinkTxt &loFormSet.lcTypeStat
      *SHOW GET loFormSet.laData[2] &lcLinkStat
      loFormSet.Ariaform1.lnLinkTxt.Enabled = loFormSet.lcTypeStat = 'ENABLE'
      loFormSet.Ariaform1.laData2.Enabled = 'ENABLE' = loFormSet.lcLinkStat
    ENDIF
  ELSE
    STORE 'ENABLE' TO loFormSet.lcTypeStat,loFormSet.lcLinkStat
    *SHOW GET loFormSet.lnLinkTxt &lcTypeStat
    *SHOW GET loFormSet.laData[2] &lcLinkStat
    loFormSet.Ariaform1.lnLinkTxt.Enabled = loFormSet.lcTypeStat = 'ENABLE'
    loFormSet.Ariaform1.laData2.Enabled = 'ENABLE' = loFormSet.lcLinkStat
  ENDIF
ELSE
  DIMENSION loFormSet.laData[3]
  STORE SPACE(0) TO loFormSet.laData
  IF oAriaApplication.ActiveModuleID == 'SM'
    loFormSet.lnComp = ASUBSCRIPT(loFormSet.laComp,ASCAN(loFormSet.laComp,'NOCOMP'),1)
  ENDIF
ENDIF     &&IF !EMPTY(oAriaApplication.ActiveCompanyID)


lcGLChart = loFormSet.lcGLChart
lcGLMask = loFormSet.lcGLMask
lcGLAccStr = loFormSet.lcGLAccStr

IF llContinue AND !EMPTY(lcComp)
  
  loFormSet.llChldComp = SEEK(lcComp,'SycComp') AND !EMPTY(SycComp.cCompPrnt)
  lcDataDir  = IIF(SEEK(lcComp,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
  loFormSet.lcPrntCo   = IIF(loFormSet.llChldComp,ALLTRIM(SycComp.cCompPrnt),'')
  loFormSet.lcGLVer    = ALLTRIM(UPPER(gfGetMemVar('M_GL_VERS',lcComp)))
  loFormSet.lcGLCo     = ALLTRIM(UPPER(gfGetMemVar('M_GL_CO',lcComp)))
  
  IF loFormSet.lcGLVer <> 'S'
    loFormSet.lcGLCo = IIF(loFormSet.llChldComp,ALLTRIM(SyCComp.cCompPrnt),lcComp)
  ENDIF

  loFormSet.lcGLVer = IIF(loFormSet.llSetUp,lcSULnkVer,loFormSet.lcGLVer)
  
  DO CASE
    *-- GL Version is SBT
    CASE loFormSet.lcGLVer = 'S'
      lcSBTGLDir = ALLTRIM(UPPER(gfGetMemVar('M_SYS_DIR',lcComp)))
      
      =gfOpenFile(lcSBTGLDir+'SYSDATA','','SH',@lcSysTmp,.T.)
        
      SELECT (lcSysTmp)
      LOCATE FOR SYSID = "GL" + loFormSet.lcGLCo
      IF !FOUND()
        *--lcInfoMsg = 'Company not found !!!'
        =gfModalGen('INM00269B00000','DIALOG')
        llContinue = .F.
      ELSE  &&FOUND
        *-- Get path for gl data and company name
        loFormSet.lcGLDir    = ALLTRIM(SUBSTR(DRIVE,61,30))         && DATA DIRECTORY PATH
        loFormSet.lcFile     = "GLACNT"+loFormSet.lcGLCo
        loFormSet.lcPrGLDir  = loFormSet.lcGLDir
      ENDIF
      
      =lfCloseFile(lcSysTmp)
      
      loFormSet.lcCodeFld   = 'GLACNT'       
      loFormSet.lcDescFld   = 'GLDESC'
      loFormSet.lcGLDscStat = "DISABLE"
      loFormSet.llOtherVer  = .F.

    *-- GL Version is ARIA
    CASE loFormSet.lcGLVer  = 'A'
      loFormSet.lcGLDir     = IIF(SEEK(IIF(loFormSet.llChldComp,loFormSet.lcPrntCo,lcComp),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      loFormSet.lcFile      = "GLACCHAR"
      loFormSet.lcPrGLDir   = IIF(SEEK(loFormSet.lcPrntCo,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      loFormSet.lcCodeFld   = 'CACCTCODE'       
      loFormSet.lcDescFld   = 'CACCNLDES'
      loFormSet.lcGLDscStat = "DISABLE"
      loFormSet.llOtherVer  = .F.

    *-- Other type of GL version
    OTHERWISE
      loFormSet.lcGLDir     = IIF(SEEK(IIF(loFormSet.llChldComp,loFormSet.lcPrntCo,lcComp),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      loFormSet.lcFile      = ''      
      loFormSet.lcCodeFld   = ''       
      loFormSet.lcDescFld   = ''
      loFormSet.lcGLDscStat = "ENABLE"
      loFormSet.llOtherVer  = .T.

  ENDCASE
  
  IF loFormSet.lcGLVer <> 'O'
    IF !FILE(IIF(loFormSet.llChldComp,loFormSet.lcPrGLDir,loFormSet.lcGLDir)+loFormSet.lcFile+'.DBF')
      *-- Chart of account file for this company not found !!!
      *-- <OK>
      = gfModalGen("INM00293B00000","Dialog")  
    ELSE
      PRIVATE lcChrtFile
      lcChrtFile = IIF(loFormSet.llChldComp,loFormSet.lcPrGLDir,loFormSet.lcGLDir)+loFormSet.lcFile
      =gfOpenFile(lcChrtFile,IIF(loFormSet.lcGLVer = 'S','GLACNT',IIF(loFormSet.lcGLVer = 'A','ACCTCODE','')),'SH',@lcGLChart,.T.)
      DO CASE
        CASE loFormSet.lcGLVer = 'S'
          SET ORDER TO GLACNT IN (lcGLChart)
        CASE loFormSet.lcGLVer = 'A'
          SET ORDER TO ACCTCODE IN (lcGLChart)        
      ENDCASE
    ENDIF    &&IF !FILE(IIF(loFormSet.llChldComp,loFormSet.lcPrGLDir,lcGLDir)+......
  ENDIF   &&IF loFormSet.lcGLVer <> 'O'
  lcBaseGL = loFormSet.lcBaseGL
  *=gfOpenFile(lcDataDir+'GL_Link','GL_Link1','SH',@lcBaseGL,.T.)
  =gfOpenFile(lcDataDir+'GL_Link','GL_Link1','SH',@lcBaseGL,.T.)
  
  SELECT (loFormSet.lcBaseGL)
  
  lcArr = ""
  FOR lnI = 1 TO ALEN(loFormSet.laType,1)
    IF loFormSet.laType[lnI,2] <> '00'
      lcArr = lcArr + IIF(EMPTY(lcArr),"'"+loFormSet.laType[lnI,2]+"'",','+"'"+loFormSet.laType[lnI,2]+"'")
    ENDIF
  ENDFOR
  lcFltExp = "INLIST(LinkType," + lcArr + ")"

  SET FILTER TO &lcFltExp.
  *E303316,1 TMI 12/05/2012 [Start] 
  SELECT (loFormSet.lcBaseFile)
  SET FILTER TO &lcFltExp.
  *E303316,1 TMI 12/05/2012 [End  ] 
    
  *=gfOpenFile(lcDataDir+'GL_Link','GL_Link1','SH',@lcALLGL,.T.)
  =gfOpenFile(lcDataDir+'GL_Link','GL_Link1','SH',@lcALLGL,.T.)
  
  SELECT (loFormSet.lcBaseGL)
  SET RELATION TO '00'+Link_Code INTO (lcALLGL)

  lcBaseFile = loFormSet.lcBaseGL
  lcScFields = "LinkType,Link_Code,LinkDesc"
  SCATTER FIELDS &lcScFields TO loFormSet.laData BLANK
  *SHOW GET loFormSet.laData[2] &lcLinkStat
  loFormSet.Ariaform1.laData2.Enabled = 'ENABLE' = loFormSet.lcLinkStat

  IF loFormSet.lcGLVer $ 'AO'
    =gfOpenFile(loFormSet.lcGLDir+"ACCOD","AccSegNo",'SH',@lcGLMask,.T.)
    SELECT (lcGLMask)
    GOTO TOP
    lcRep     = IIF(loFormSet.lcGLVer = "A", "9", "X")
    loFormSet.lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    loFormSet.lnAcLen   = LEN(ALLTRIM(loFormSet.lcAcMask))
    =lfCloseFile(lcGLMask)
  ELSE
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + loFormSet.lcGLCo + ".DBF"
    loFormSet.lcAcMask   = SPACE(0)
    
    =gfOpenFile(lcAcntStrc,'','SH',@lcGLAccStr,.T.)
    SET ORDER TO SegID IN (lcGLAccStr)
    
    SELECT (lcGLAccStr)
    SCAN FOR SegLen > 0
      loFormSet.lcAcMask = loFormSet.lcAcMask + IIF(EMPTY(loFormSet.lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    loFormSet.lnAcLen   = LEN(ALLTRIM(loFormSet.lcAcMask))
    =lfCloseFile(lcGLAccStr)
  ENDIF

  loFormSet.lcEmptyMas = loFormSet.lcAcMask
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,'A',' ')
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,'X',' ')
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,'9',' ')
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,'#',' ')
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,'!',' ')
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,'*',' ')
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,'.',' ')
  loFormSet.lcEmptyMas = STRTRAN(loFormSet.lcEmptyMas,';',' ')
  *-- Padding with 24 char. refers to glaccount field lenght
  loFormSet.lcEmptyMas = PADR(loFormSet.lcEmptyMas,24)

  IF loFormSet.llSetUp
    loFormSet.lnComp = ASUBSCRIPT(loFormSet.laComp,ASCAN(loFormSet.laComp,lcComp),1)
  ELSE
    IF oAriaApplication.ActiveModuleID == 'SM'
      loFormSet.lnComp = ASUBSCRIPT(loFormSet.laComp,IIF(EMPTY(lcComp),;
                                     ASCAN(loFormSet.laComp,'NOCOMP'),;
                                     ASCAN(loFormSet.laComp,lcComp)),1)
    ENDIF
  ENDIF
  
  IF TYPE("lcSelComp") = 'C'
    *= lfCatBrow()
  ENDIF

ENDIF

RETURN llContinue

************************************************************
*! Name      : lfCloseFile
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/04/2012
*! Purpose   : Close a table
************************************************************
FUNCTION lfCloseFile
PARAMETERS lcAlias
IF USED(lcAlias)
  USE IN &lcAlias
ENDIF 
*- End of lfCloseFile.


*!*************************************************************
*! Name        : gfSqlRun
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Locate Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : SQL Select Statement, Cursor Receiving Results, Ignore Creating Index
*!*************************************************************
*! Returns     :  False if Falied to Create the Cursor
*!*************************************************************
FUNCTION gfSqlRun
LPARAMETERS lcSqlStatement, lcAlias, llNoIndex, lcTempAlias
LOCAL lnTable

lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lcTempAlias = IIF(TYPE('lcTempAlias')='C',lcTempAlias,lcAlias)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  RETURN oAriaApplication.laRemoteTable[lnTable].SqlRun(lcSqlStatement, lcTempAlias, llNoIndex)
ELSE
  RETURN .F.
ENDIF

RETURN
*--end of SqlRun
*!*************************************************************
*! Name        : gfGetRemoteTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Searches the AriaApplication laRemoteTable Array for an Existing Object
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : DataSession, Alias Name
*!*************************************************************
*! Returns            :  0  ----> If Remote Table Object Not Found in the laRemoteTable Array
*!                       n  ----> Element No. of laRemoteTable in which the Remote Table Exists
*!*************************************************************
FUNCTION gfGetRemoteTable
LPARAMETERS lnDataSessionID, lcAlias
LOCAL lnC, lnL, lnV

lnV=0
lnL = ALEN(oAriaApplication.laRemoteTable)
FOR lnC=1 TO lnL
  IF TYPE('oAriaApplication.laRemoteTable[lnC]')='O' AND ;
      oAriaApplication.laRemoteTable[lnC].lnDataSession == lnDataSessionID AND ;
      UPPER(oAriaApplication.laRemoteTable[lnC].lcCursorView)==UPPER(lcAlias)
     lnV = lnC
     EXIT
  ENDIF
NEXT

RETURN lnV
*--end of gfGetRemoteTable
