*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLSGVLU.PRG
*:  Module      : General Ledger
*:  Desc.       : Segment Values screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/05/2012
*:  Reference   : *E303107,1
*:************************************************************************
PARAMETERS pcSegNum,pcSegValue  && If calling from the account validation.
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\gl\GLSGVLU.H
*N000682,1 MMT 11/22/2012 Globalization changes[END]
*- Call the screen
lcRunScx = lfGetScx("GL\GLSGVLU.scx")
IF !EMPTY(pcSegNum)
  PRIVATE oGLSGVLU
  DO FORM (lcRunScx) WITH pcSegNum,pcSegValue NAME oGLSGVLU NOSHOW
  IF TYPE('oGLSGVLU')='O' AND !ISNULL(oGLSGVLU)
    oGLSGVLU.Show(1)
  ENDIF
ELSE
  DO FORM (lcRunScx) WITH pcSegNum,pcSegValue
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

loFormSet.AddProperty('lcProgName','GLSGVLU')

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
*N000682,1 12/21/12 TMI Globlization changes[Start] add the "Account Code Segment Value" title *x*
*lcFile_Ttl = LOOKUP(SYDFILES.CFILE_TTL,loFormset.lcBaseFile,SYDFILES.CFILE_NAM)
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account_Code_Segment_Value,loFormSet.GetHeaderText("LANG_Account_Code_Segment_Value",loFormSet.HeaderAlias))
*N000682,1 12/21/12 TMI Globlization changes[End  ] 
loFormSet.lcFile_Ttl = lcFile_Ttl

*- initializations
WITH loFormSet
  .cbrowsetabledbengine   = "NATIVE"
  .nWorkArea                            = .lcBaseFile
  .otoolbar.nWorkArea                   = .lcBaseFile
  .DataEnvironment.InitialSelectedAlias = .lcBaseFile
  .cBrowseFileName        = .lcBaseFile
  .cBrowseIndexExpression = "CACSSEGNO+CSEGVALUE"
  .cBrowseIndexFields     = "CACSSEGNO,CSEGVALUE"
  .cBrowseIndexName       = "ACSSEGVAL"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  .BrowseTitle 		  	    = loFormSet.lcFile_Ttl
  .ariaBrFields.edtBrowseFields.Value = gfDbfField(.nWorkArea)
  *.Ariaform1.lcAcSegDes.Caption = .lcAcSegDes
ENDWITH

*** To know if the prog. calling from the menu or account validation.
llSegMd = IIF(TYPE("loFormSet.pcSegNum") $ "UL",.F.,.T.)
loFormSet.Addproperty('llSegMd',llSegMd)

DECLARE laSegSize  [1,2]  && Array to have seg. size & desc.
DECLARE laObjdisp  [6]    && Array to control the display of seg. Des. in Child

*** this defination of these arrays is temporary till it be ***
*** data driven from screen object file ...

*** The status popup array ***
DECLARE laTStatus [2,2]
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTStatus [1,1] = "Active"
laTStatus [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACTIVE,loFormSet.GetHeaderText("LANG_ACTIVE",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTStatus [1,2] = "A"
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTStatus [2,1] = "Inactive"
laTStatus [2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INACTIVE,loFormSet.GetHeaderText("LANG_INACTIVE",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTStatus [2,2] = "I"
=lfDefineArray(loFormSet,'laTStatus')

*** The categry popup array ***
DECLARE laTCatgry [4,2]
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTCatgry [1,1] = "Cash"
laTCatgry [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASH,loFormSet.GetHeaderText("LANG_CASH",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTCatgry [1,2] = "C"
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTCatgry [2,1] = "Non cash"
laTCatgry [2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NONCASH ,loFormSet.GetHeaderText("LANG_NONCASH",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTCatgry [2,2] = "N"
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTCatgry [3,1] = "Depreciation"
laTCatgry [3,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DEPREC,loFormSet.GetHeaderText("LANG_DEPREC",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTCatgry [3,2] = "D"
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTCatgry [4,1] = "Amortization"
laTCatgry [4,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AMORT,loFormSet.GetHeaderText("LANG_AMORT",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTCatgry [4,2] = "A"
=lfDefineArray(loFormSet,'laTCatgry')

*** The term popup array ***
DECLARE laTTerm   [2,2]
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTTerm   [1,1] = "Long term"
laTTerm   [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LONGTERM,loFormSet.GetHeaderText("LANG_LONGTERM",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTTerm   [1,2] = "L"
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
*laTTerm   [2,1] = "Current"
laTTerm   [2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRENT,loFormSet.GetHeaderText("LANG_CURRENT",loFormSet.HeaderAlias))
*N000682,1 MMT 12/03/2012 Globalization changes[end]
laTTerm   [2,2] = "C"
=lfDefineArray(loFormSet,'laTTerm')

=lfDefineVars(loFormSet)

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
SELECT GLSEGVAL

loFormSet.Addproperty('lc_TempAc', gfTempName())
lcScFields = 'CACSSEGNO,CSEGVALUE,CSEGSHDES,CSEGLNDES,CSEGACTIV,CSEGTERM,CSEGACCAT,CTYPECODE,'+;
             'NSEGSEQN,CSEGRATIO,CSEGCAFLO,CSEGALPOS,CSEGAUTDS,CSEGCOCAC,NSEGCONPR,CSTANDARD'
loFormSet.Addproperty('lcScFields',lcScFields)
SCATTER FIELDS &lcScFields TO laData BLANK
DIMENSION loFormSet.laData[ALEN(laData)]
=ACOPY(laData,loFormSet.laData)

gcDataDir = oAriaApplication.DataDir
  *** Select the segment size & desc. in laSegSize array. ***
  SELECT nacssize,cAcsshdes FROM &gcDataDir.accod ;
         WHERE !EMPTY(nacssize) ;
         INTO ARRAY laSegSize
  =lfDefineArray(loFormSet,'laSegSize')

  *** Locate Segments controls On Screen
  =lfLocateSgOnScr(loFormSet)


   loFormSet.Ariaform1.puRatio.Value = '00'
   loFormSet.Ariaform1.puCash.Value = '000'

loFormSet.llDispFlg = .T.
lcTNonApp = loFormSet.lcTNonApp

IF llSegMd
  *** If calling from account validation. ***
  loFormSet.llDispFlg = (pcSegNum = "1")

  lnSegNo   = VAL(pcSegNum)
  laData[1] = pcSegNum

  lcSegVlu  = "lcSeg" + pcSegNum
  loFormSet.&lcSegVlu = pcSegValue
  laData[2] = pcSegValue

  lcSegStat = "DISABLE"

  STORE "DISABLE" TO lcPopTerm,lcPopCatg,lcPopRato,lcPopCash,lcPopStat

  IF laData[1] = "1"

    *** Get the type code & desc. for this segment. ***
    SELECT GLTYPES
    GO TOP
    LOCATE FOR BETWEEN(VAL(pcSegValue),VAL(cTyplacno),VAL(cTypuacno))
    IF FOUND()
      loFormSet.lcTypCode    = GLTYPES.cTypeCode
      loFormSet.lcTypDesc    = GLTYPES.cTypeDesc
      laData[8]    = loFormSet.lcTypCode
    ELSE
      *** This value does not fall within any range. ***
      *** Please enter another value. ***
      *** < Ok > ***
      *N000682,1 MMT 11/22/2012 Globalization changes[Start]
      *lcMsg = "The segment value &pcSegValue does not fall within any range. Please, enter another one."
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcMsg = LANG_MSG1
      lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MSG1,loFormSet.GetHeaderText("LANG_MSG1",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 MMT 11/22/2012 Globalization changes[END]
      =gfModalGen("TRM0000B00000",.F.,.F.,.F.,lcMsg)
      RETURN .F.
    ENDIF

    lcPopTerm = IIF(LEFT(laData[8],1) $ 'AL',"ENABLE","DISABLE")
    laData[6] = IIF(LEFT(laData[8],1) $ 'AL',loFormset.laTTerm[1,2]," ")

    lcPopCatg = IIF(LEFT(laData[8],1) = 'A',"ENABLE","DISABLE")
    laData[7] = IIF(LEFT(laData[8],1) = 'A',loFormset.laTCatgry[1,2]," ")

    lcPopStat = "ENABLE"
    laData[5] = laTStatus[1,2]

    SELECT GLRACOD
    SET FILTER TO

    SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
    CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " " ;
    INTO ARRAY loFormSet.laRatio

    SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash

    lcPopRato  = IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE")
    loFormSet.ariaform1.puRatio.Value = '00'

    lcPopCash  = IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE")
    loFormSet.ariaform1.puCash.Value = '000'

    *** Set defaults if 1st segment. ***
    loFormSet.ariaform1.cbPost.Value     = 1
    laData[12] = "Y"

  ENDIF

  DIMENSION loFormset.laData[ALEN(laData)]
  =ACOPY(laData,loFormset.laData)

  WITH loFormSet.Ariaform1
    .ibPopTerm.Enabled = ( lcPopTerm = 'ENABLE' )
    .ibPopCatg.Enabled = ( lcPopCatg = 'ENABLE' )
    .ibPopStat.Enabled = ( lcPopStat = 'ENABLE' )
    .puRatio.Enabled = ( lcPopRato = 'ENABLE' )
    .puCash.Enabled  = ( lcPopCash = 'ENABLE' )
  ENDWITH

  SELECT GLSEGVAL

  =lfDefControlSource(loFormSet)

  loFormSet.ChangeMode('A')


ELSE

  STORE "DISABLE" TO lcPopTerm,lcPopCatg,lcPopRato,;
                       lcPopCash,lcPopStat,lcActMajr

  IF laData[1] = '1'
    lcActMajr = IIF(loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V',"DISABLE",;
                IIF(VAL(laData[1]) = 1,"ENABLE","DISABLE"))
    lcPopTerm = IIF(loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V',"DISABLE",;
                IIF(LEFT(laData[8],1) $ 'AL',"ENABLE","DISABLE"))
    lcPopCatg = IIF(loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V',"DISABLE",;
                IIF(LEFT(laData[8],1) = 'A',"ENABLE","DISABLE"))
    lcPopRato = IIF(loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V',"DISABLE",;
                IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE"))
    lcPopCash = IIF(loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V',"DISABLE",;
                IIF(LEFT(laData[8],1) <> 'Y',"ENABLE","DISABLE"))
    lcPopStat = IIF(loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V',"DISABLE","ENABLE")

    DIMENSION loFormSet.laData[ALEN(laData)]
    =ACOPY(laData,loFormset.laData)

    cbPost = IIF(laData[12] = "Y",1,0)

  ENDIF


    SELECT GLRACOD
    SET FILTER TO

    SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
    CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " " ;
    INTO ARRAY loFormSet.laRatio

    SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash


  *- apply enable/disable
  WITH loFormSet.Ariaform1
    .ibPopTerm.Enabled = ( lcPopTerm = 'ENABLE' )
    .ibPopCatg.Enabled = ( lcPopCatg = 'ENABLE' )
    .ibPopStat.Enabled = ( lcPopStat = 'ENABLE' )
    .puRatio.Enabled = ( lcPopRato = 'ENABLE' )
    .puCash.Enabled  = ( lcPopCash = 'ENABLE' )
  ENDWITH

  =lfDefControlSource(loFormSet)

  loFormSet.ChangeMode('S')

  WITH loFormset.ariaform1
    .cmdOk.Visible = .F.
    .cmdCancel.Visible = .F.
  ENDWITH

ENDIF

*- Hide controls not needed in case of segment type <> '1'

IF TYPE('loFormSet.pcSegNum') = 'C'
  *=lfHideCtrls(loFormSet)
  loFormSet.llDispFlg = .F.
ENDIF

*- Clear the filter in the GLRACOD if left the screen.
SELECT GLRACOD
SET FILTER TO

RETURN
*-End of lfFormInit.

************************************************************
*! Name      : lfDefControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/12/2012
*! Purpose   : Define Control Source
************************************************************
FUNCTION lfDefControlSource
PARAMETERS loFormSet
WITH loFormSet.Ariaform1

  .lcSeg1.txtSeg.ControlSource = 'Thisformset.lcSeg1'
  .lcSeg2.txtSeg.ControlSource = 'Thisformset.lcSeg2'
  .lcSeg3.txtSeg.ControlSource = 'Thisformset.lcSeg3'
  .lcSeg4.txtSeg.ControlSource = 'Thisformset.lcSeg4'
  .lcSeg5.txtSeg.ControlSource = 'Thisformset.lcSeg5'
  .lcSeg6.txtSeg.ControlSource = 'Thisformset.lcSeg6'

  .lcTypCode.ControlSource = 'Thisformset.lcTypCode'
  .lcTypDesc.ControlSource = 'Thisformset.lcTypDesc'
  .txtSDesc.ControlSource = 'Thisformset.laData[3]'
  .txtLDesc.ControlSource = 'Thisformset.laData[4]'

  .txtSDesc.InputMask = REPLICATE('X',FSIZE('CSEGSHDES','GLSEGVAL'))
  .txtLDesc.InputMask = REPLICATE('X',FSIZE('CSEGLNDES','GLSEGVAL'))

  .ibPopTerm.ControlSource = 'Thisformset.laData[6]'
  .ibPopCatg.ControlSource = 'Thisformset.laData[7]'
  .ibPopStat.ControlSource = 'Thisformset.laData[5]'

  **.cbPost.ControlSource = 'Thisformset.laData[8]'
  .cbPost.Value = IIF(loFormSet.laData[12]='Y',1,0)

  .puRatio.ControlSource = 'Thisformset.laData[10]'
  .puCash.ControlSource = 'Thisformset.laData[11]'

  .kbAutoDistCode.KeyTextBox.ControlSource = 'Thisformset.laData[13]'
  .kbAutoDistCode.KeyTextBox.InputMask = REPLICATE('!',FSIZE('CSEGAUTDS','glsegval'))

  .txtConsCoAct.ControlSource = 'Thisformset.laData[14]'
  .txtConsCoAct.ControlSource = 'Thisformset.laData[14]'
  .txtConsCoAct.InputMask = REPLICATE('X', FSIZE('CSEGCOCAC','glsegval') )

  .txtRepSeqNo.ControlSource = 'Thisformset.laData[9]'
  .txtConsPrcnt.ControlSource = 'Thisformset.laData[15]'

  *- adjust the popup width for combo boxes
  .ibPopTerm.RowSource = 'Thisformset.laTTerm'
  lfColumnWidthes( .ibPopTerm )
  .ibPopCatg.RowSource = 'Thisformset.laTCatgry'
  lfColumnWidthes( .ibPopCatg )
  .ibPopStat.RowSource = 'Thisformset.laTStatus'
  lfColumnWidthes( .ibPopStat )
  .puRatio.RowSource = 'Thisformset.laRatio'
  lfColumnWidthes( .puRatio )
  .puCash.RowSource = 'Thisformset.laCash'
  lfColumnWidthes( .puCash )
ENDWITH
*- End of lfDefControlSource.

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/11/2012
*! Purpose   : Define Vars
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

loFormSet.Addproperty('laData[1]'  , "" )    && laData
loFormSet.Addproperty('lc_TempAc'  , "" )    && Name for the temp. file hold accounts upon deletion.
loFormSet.Addproperty('llDispFlg'  , .F.)    && to display all obj. if 1st seg.
loFormSet.Addproperty('llNumeric'  , .F.)    && To know if 1st seg. has any char. or not.
loFormSet.Addproperty('llBrowse'   , .F.)
loFormSet.Addproperty('lnSegNo'    , 1  )    && Have the segment no.
loFormSet.Addproperty('lnTotlen'   , 0  )
loFormSet.Addproperty('lnRitPos'   , 2  )
loFormSet.Addproperty('lcOldAuto'  , "" )    && The old automatic distribution.

loFormSet.Addproperty('lcRatioDes' , "" )       && Ratio group say var.
loFormSet.Addproperty('puRatio'    , "" )
loFormSet.Addproperty('puRatioGrp' , "" )
loFormSet.Addproperty('lcCashFDes' , "" )       && Cash flow say var.
loFormSet.Addproperty('puCash'     , "" )
loFormSet.Addproperty('puCashFlow' , "" )
loFormSet.Addproperty('lcTerm'     , "" )    && Term say var.
loFormSet.Addproperty('lcCatgry'   , "" )    && Categry say var.
loFormSet.Addproperty('lcStatus'   , "" )    && Status say var.

loFormSet.Addproperty('lcActMajr' , " ")
loFormSet.Addproperty('lcPopTerm' , " ")
loFormSet.Addproperty('lcPopCatg' , " ")
loFormSet.Addproperty('lcPopRato' , " ")
loFormSet.Addproperty('lcPopCash' , " ")
loFormSet.Addproperty('lcPopStat' , " ")
loFormSet.Addproperty('lcSegStat' , " ")

lcVars = 'lcSeg1,lcSeg2,lcSeg3,lcSeg4,lcSeg5,lcSeg6'  && Seg. values
=lfAddProp(loFormSet,lcVars,'')

loFormSet.Addproperty('laObjdisp'  , .F.)    && Default Display all lines in child screen

loFormSet.Addproperty('lnBoxSize'  , 19 )
loFormSet.Addproperty('cbPost'     , 1  )    && Default value forcheck box.
loFormSet.Addproperty('lcTypCode'  , '   ' ) && The type code.
loFormSet.Addproperty('lcTypDesc'  , '' )    && The type description.
loFormSet.Addproperty('lctActMajr' , '' )    && Veriabel to hold the child window titel

loFormSet.Addproperty('llChild'    , .F.)

*** These variables defined before in the screen object file. ***
loFormSet.Addproperty('lcTAtBrTit' , "" )
loFormSet.Addproperty('lcTitleAct' , "" )

loFormSet.Addproperty('llSegMd' , IIF(TYPE("pcSegNum") $ "UL",.F.,.T.) )
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	loFormSet.Addproperty('LCTACTMAJR', "Account Major options")
*!*	loFormSet.Addproperty('LCTFLDTXT', "Field")
*!*	loFormSet.Addproperty('LCTATBRTIT', "Automatic Distribution")
*!*	loFormSet.Addproperty('LCTPRCNTG', "Percentage")
*!*	loFormSet.Addproperty('LCTITLEACT', "Accounts")
*!*	loFormSet.Addproperty('lcTNonApp', "N/A")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Addproperty('LCTACTMAJR', LANG_ACCOUNTMAJOR)
loFormSet.Addproperty('LCTACTMAJR', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNTMAJOR,loFormSet.GetHeaderText("LANG_ACCOUNTMAJOR",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Addproperty('LCTFLDTXT', LANG_FIELD )
loFormSet.Addproperty('LCTFLDTXT', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FIELD,loFormSet.GetHeaderText("LANG_FIELD",loFormSet.HeaderAlias)) )
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Addproperty('LCTATBRTIT', LANG_AUTOMATIC)
loFormSet.Addproperty('LCTATBRTIT', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOMATIC,loFormSet.GetHeaderText("LANG_AUTOMATIC",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Addproperty('LCTPRCNTG', LANG_PERCENT )
loFormSet.Addproperty('LCTPRCNTG', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERCENT,loFormSet.GetHeaderText("LANG_PERCENT",loFormSet.HeaderAlias)) )
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Addproperty('LCTITLEACT', LANG_ACCOUNTS)
loFormSet.Addproperty('LCTITLEACT', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNTS,loFormSet.GetHeaderText("LANG_ACCOUNTS",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Addproperty('lcTNonApp', LANG_NA)
loFormSet.Addproperty('lcTNonApp', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NA,loFormSet.GetHeaderText("LANG_NA",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
*- Define two arrays to hold the Ration & Cash row source
loFormSet.Addproperty('laRatio[1,2]'  , ''   )
loFormSet.Addproperty('laCash[1,2]'  , ''   )


*- End of lfDefineVars.

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/14/2012
*! Purpose   : Form Activate
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- Hide the toolbar in the MODAL mode
IF TYPE('loFormSet.pcSegNum') = 'C'
  loFormSet.oToolBar.Visible =.F.
  loFormSet.Ariaform1.ControlBox = .F.
ENDIF

*- End of lfFormActivate.
************************************************************
*! Name      : lfFormUndo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/14/2012
*! Purpose   : Form Undo
************************************************************
FUNCTION lfFormUndo
PARAMETERS loFormSet

IF TYPE('loFormSet.pcSegNum') = 'C'
  loFormSet.ariaform1.Release
ENDIF

*- End of lfFormUndo.

************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/12/2012
*! Purpose   : Change Mode FUNCTION
************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
IF TYPE('loFormSet.lcTypCode')='U'
  RETURN
ENDIF

DIMENSION laData[ALEN(loFormset.laData)]
=ACOPY(loFormSet.laData,laData)

loFormSet.llDispFlg = laData[1]= '1'

lcTNonApp = loFormSet.lcTNonApp

DO CASE
  CASE loFormSet.ActiveMode='S'

    lcScFields = loFormset.lcScFields
    SELECT glsegval
    SCATTER FIELDS &lcScFields TO laData BLANK

    *** Select mode ***
    loFormSet.lcTypCode = "   "
    loFormSet.lcTypDesc = ""

    *** Set the pic. for all segment values. ***
    FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      loFormSet.&lcObjNam = SPACE(loFormSet.laSegSize[lnCount,1])
      loFormSet.ariaform1.&lcObjNam..txtSeg.Enabled = .T.
    ENDFOR
    lfEnDsMajSgDetCtrls(loFormSet,.F.)

  CASE loFormSet.ActiveMode='V'

    *** View mode ***
    SELECT GLSEGVAL
    lcScFields = loFormset.lcScFields
    SCATTER FIELDS &lcScFields TO laData
    =ACOPY(laData,loFormSet.laData)

    *** Get type code & desc. ***
    IF laData[1] ="1"
      loFormSet.lcTypCode = laData[8]
      loFormSet.lcTypDesc = LOOKUP(GLTYPES.cTypedesc,laData[8],GLTYPES.cTypecode,'Typecode')
    ELSE
      loFormSet.lcTypCode = "   "
      loFormSet.lcTypDesc = ""
    ENDIF

    *** Set the pic. for all segment values. & disable all the segments. ***
    lcObjNam   = 'lcSeg' + ALLTRIM(laData[1])
    &lcObjNam  = laData[2]

    IF laData[1] = '1' .AND. !EMPTY(laData[2])
      *** If 1st segment, enable account major button. ***
      lfEnDsMajSgDetCtrls(loFormSet,.T.)
    ELSE
      lfEnDsMajSgDetCtrls(loFormSet,.F.)
    ENDIF

  CASE loFormSet.ActiveMode='E' .OR. loFormSet.ActiveMode='A'
    *** Edit mode - Add mode ***
    *** Disable all the segment ***

    FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      loFormSet.&lcObjNam = IIF( VAL(laData[1]) = lnCount , laData[2] , '')
      loFormSet.Ariaform1.&lcObjNam..Enabled = .F.
    ENDFOR

    IF laData[1] = '1' .AND. !EMPTY(laData[2])
      lfEnDsMajSgDetCtrls(loFormSet,.T.)
    ELSE
      loFormSet.lcTypCode = "   "
      loFormSet.lcTypDesc = ""
      lfEnDsMajSgDetCtrls(loFormSet,.F.)
    ENDIF
ENDCASE

=lfValShow(loFormSet)  && Show function of the child screen. ***
=ACOPY(laData,loFormSet.laData)

*- End of lfChangeMode.
************************************************************
*! Name      : lfEnDsMajSgDetCtrls
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/12/2012
*! Purpose   : Enable/Disable Major Segment Details Controls
************************************************************
FUNCTION lfEnDsMajSgDetCtrls
PARAMETERS loFormSet,llEn
WITH loFormSet.Ariaform1
  .ibPopTerm.Enabled = llEn
  .ibPopCatg.Enabled = llEn
  .ibPopStat.Enabled = llEn
  .puRatio.Enabled = llEn
  .puCash.Enabled  = llEn
ENDWITH

*- End of lfEnDsMajSgDetCtrls.

*!**************************************************************************
*!
*!      FUNCTION: lfValShow
*!
*!**************************************************************************
FUNCTION lfValShow
PARAMETERS loFormSet
*** Show function of the child screen. ***

DO CASE

  CASE loFormSet.ActiveMode='S'
    *** Set the default values for all the popups. ***
    STORE lcTNonApp TO lcRatioDes,lcCashFDes,lcTNonApp,lcCatgry,;
                       lcStatus,puRatio,puCash
    WITH loFormSet.Ariaform1
    .ibPopTerm.Value  = loFormSet.laTTerm[1,2]
    .ibPopCatg.Value  = loFormSet.laTCatgry[1,2]
    .ibPopStat.Value  = loFormSet.laTStatus[1,2]
    .cbPost.Value     = 1
    ENDWITH

  CASE loFormSet.ActiveMode='V'

    *** Set the pic. for all segment values. ***
    FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      loFormSet.ariaform1.&lcObjNam..txtSeg.Enabled = .F.
      loFormSet.&lcObjNam = SPACE(loFormSet.laSegSize[lnCount,1])
      IF VAL(laData[1]) = lnCount
        loFormSet.&lcObjNam = glsegval.CSEGVALUE
      ENDIF
    ENDFOR

    *** Set the popups values according to its fields. ***
    WITH loFormSet.Ariaform1
    .ibPopTerm.Value = laData[6]
    .ibPopStat.Value = laData[5]
    .ibPopCatg.Value = laData[7]

    IF laData[12] = "Y"
      .cbPost.Value  = 1
    ELSE
      .cbPost.Value  = 0
    ENDIF

    *** Get the ratio group value from GLRACOD. ***
    lcRatioDes = ''

    SELECT CRATDESC,CSEGRATIO FROM GLRACOD INTO ARRAY loFormSet.laRatio
    .puRatio.RowSource = 'Thisformset.laRatio'

    .puRatio.Value = laData[10]
    *.puRatio.DisplayValue = lcRatioDes

    *** Get the cash flow value from GLCFITEM. ***
    .puCash.Value = laData[11]
    *.puCash.DisplayValue = puCash

    .kbAutoDistCode.Keytextbox.Value = laData[13]
    .txtConsCoAct.Value = laData[14]
    .txtRepSeqNo.Value = laData[9]
    .txtConsPrcnt.Value = laData[15]

    .Refresh()
    ENDWITH

  CASE loFormSet.ActiveMode='E'

    *** Set filter in the GLRACOD according to current type code. ***
    SELECT GLRACOD
    SET FILTER TO
    SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
    CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " " ;
    INTO ARRAY loFormSet.laRatio
    loFormSet.Ariaform1.puRatio.RowSource = 'Thisformset.laRatio'

    SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash

    SELECT GLSEGVAL
    WITH loFormSet.Ariaform1
    IF LEFT(laData[8],1) = 'A' .OR. LEFT(laData[8],1) = 'L'
      .ibPopTerm.Enabled = .T.
    ELSE
      lcTerm = lcTNonApp
      .ibPopTerm.DisplayValue = lcTNonApp
      .ibPopTerm.Enabled = .F.
    ENDIF

    IF LEFT(laData[8],1) = 'A'
      *SHOW GET ibPopCatg ENABLE
      .ibPopCatg.Enabled = .T.
    ELSE
      lcCatgry = lcTNonApp
      .ibPopCatg.DisplayValue = lcTNonApp
      .ibPopCatg.Enabled = .F.
    ENDIF

    IF LEFT(laData[8],1) <> 'Y'   && If type code is standard.
      .puCash.Enabled = .T.
      .puRatio.Enabled = .T.
    ELSE
      STORE lcTNonApp TO lcRatioDes,puRatio,lcCashFDes,puCash
      .puRatio.Enabled = .F.
      .puCash.Enabled = .F.
    ENDIF
    ENDWITH

  CASE loFormSet.ActiveMode='A'
    WITH loFormSet.Ariaform1
    IF laData[1] = "1"
      *** Set filter in the GLRACOD according to current type code. ***
      SELECT GLRACOD
      SET FILTER TO
      DIMENSION loFormSet.laRatio[1,2]
      loFormSet.laRatio = ''
      SELECT CRATDESC,CSEGRATIO FROM GLRACOD WHERE ;
      CRATTYPE = LEFT(laData[8],1) .OR. CRATTYPE = " " ;
      INTO ARRAY loFormSet.laRatio
      .puRatio.RowSource = 'Thisformset.laRatio'

      SELECT cCfidesc,CSEGCAFLO FROM GLCFITEM INTO ARRAY loFormSet.laCash

      SELECT GLSEGVAL

      *** If 1st seg. set default for status & allow posting. ***
      .ibPopStat.Value = loFormSet.laTStatus [1,2]
      laData[5]  = loFormSet.laTStatus [1,2]
      laData[10] = '00'
      laData[11] = '000'
      laData[12] = "Y"
      laData[16] = IIF(SUBSTR(laData[8],1,1) = 'Y' , 'N' , 'Y')

      IF LEFT(laData[8],1) = 'A' .OR. LEFT(laData[8],1) = 'L'
        laData[6] = loFormSet.laTTerm [1,2]
        .ibPopTerm.Enabled = .T.
      ELSE
        .ibPopTerm.DisplayValue = lcTNonApp
        .ibPopTerm.Enabled = .F.
      ENDIF

      IF LEFT(laData[8],1) = 'A'
        laData[7] = loFormSet.laTCatgry [1,2]
        .ibPopCatg.Enabled = .T.
      ELSE
        .ibPopCatg.DisplayValue = lcTNonApp
        .ibPopCatg.Enabled = .F.
      ENDIF

      STORE lcTNonApp TO lcRatioDes,puRatio,lcCashFDes,puCash
      IF LEFT(laData[8],1) <> 'Y'
        .puRatio.Enabled = .T.
        .puCash.Enabled = .T.
      ELSE
        .puRatio.Enabled = .F.
        .puCash.Enabled = .F.
      ENDIF
    ENDIF
    ENDWITH
ENDCASE

WITH loFormSet.Ariaform1
IF loFormSet.ActiveMode='S' .OR. loFormSet.ActiveMode='V'
  .ibPopTerm.Enabled = .F.
  .ibPopCatg.Enabled = .F.
  .ibPopStat.Enabled = .F.
  .cbPost.Enabled = .F.
  .puRatio.Enabled = .F.
  .puCash.Enabled = .F.
  .kbAutoDistCode.Enabled = .F.
  .txtConsCoAct.Enabled = .F.
  .txtRepSeqNo.Enabled = .F.
  .txtConsPrcnt.Enabled = .F.

  IF loFormSet.ActiveMode='S'
    FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      loFormSet.Ariaform1.&lcObjNam..Enabled = .T.
    ENDFOR
    try
    loFormSet.Ariaform1.lcSeg1.txtSeg.SetFocus()
    CATCH
    ENDTRY
  ENDIF
ELSE
  .ibPopStat.Enabled = .T.
  .cbPost.Enabled = .T.

  .puRatio.Enabled = .T.
  .puCash.Enabled = .T.
  .kbAutoDistCode.Enabled = .T.
  .txtConsCoAct.Enabled = .T.
  .txtRepSeqNo.Enabled = .T.
  .txtConsPrcnt.Enabled = .T.

  TRY
  .txtSDesc.SetFocus()
  CATCH
  ENDTRY
ENDIF
ENDWITH

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
FUNCTION lfvData_2
PARAMETERS loFormSet,lcSegVlu,lnSegmenNo
lnSegNo = lnSegmenNo

*** Valid function for segment value. ***
lcExacStat = SET("EXACT")
DIMENSION laData[ALEN(loFormSet.laData)]
=ACOPY(loFormSet.laData,laData)

lcScFields = loFormSet.lcScFields

llRet = .T.
WITH loFormset.Ariaform1
IF !EMPTY(lcSegVlu.Value)  && .AND. LASTKEY() = 13
  laData[1] = ALLTRIM(STR(lnSegNo))

  IF lcSegVlu.Value <> "?"
    lcSegVlu.Value = RIGHT('000000000000000'+ALLTRIM(lcSegVlu.Value),loFormSet.laSegSize[lnSegNo,1])
  ENDIF

  laData[2] = ALLTRIM(lcSegVlu.Value)
  IF ALLTRIM(lcSegVlu.Value) = "?"
    *** Browse the segments according to the seg. no. that have "?". ***
    FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      .&lcObjNam..txtSeg.Value = SPACE(loFormSet.laSegSize[lnCount,1])
      .&lcObjNam..txtSeg.Enabled = .T.
    ENDFOR
*X*
    lcFile_Ttl = loFormSet.lcFile_Ttl
    SELECT (loFormset.lcBaseFile)

    lcOldSegVl = laData[2]
    =gfBrows('laData[1]',lcScFields,'laData',lcFile_Ttl)
    IF type('laData[2]')<>'U'
      IF (lcOldSegVl <> laData[2]) .AND. !EMPTY(laData[2])
        =ACOPY(laData,loFormset.laData)
        loFormSet.ChangeMode('V')
      ENDIF
    ENDIF

    lcObjNam = 'lcSeg' + ALLTRIM(STR(lnSegNo))

    llRet = .F.

  ELSE
    *** Check if segment value has no char. ***
    llNumeric=lfDigit(laData[2],.T.)
    IF !llNumeric
      FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
        lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
        .&lcObjNam..txtSeg.Value = SPACE(loFormSet.laSegSize[lnCount,1])
        .&lcObjNam..txtSeg.Enabled = .T.
      ENDFOR
      *** Only numeric value is accepted here !!! ***
      *** < Ok > ***
      = gfModalGen("TRM02005B00000","DIALOG")
      lcObjNam = SYS(18)

      llRet = .F.

    ELSE
      *** If it is the first segment, check that it is not a zero
      IF lnSegNo = 1 .AND. VAL(laData[2]) = 0
        FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
          lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
          .&lcObjNam..txtSeg.Value = SPACE(loFormSet.laSegSize[lnCount,1])
          .&lcObjNam..txtSeg.Enabled = .T.
        ENDFOR
        *** Segment no. 1 cannot be zeros. ***
        *** <  Ok  > ***
        =gfModalGen("TRM02217B00000","DIALOG")
        lcObjNam = SYS(18)

        llRet = .F.

      ELSE
        SET EXACT ON
        IF SEEK(laData[1]+laData[2])
          SET EXACT &lcExacStat
          *** If The segment found in the file. ***
          SCATTER FIELDS &lcScFields MEMO TO laData
          loFormSet.ChangeMode('V')
        ELSE
           SET EXACT &lcExacStat
          *** If not found in the segment file. ***
          IF laData[1] = "1"
            SET EXACT &lcExacStat
            *** If its 1st segment check in GLTYPES file ***
            *** if this segment between any ranges...... ***
            SELECT GLTYPES
            SET ORDER TO TAG TYPLACNO
            GO TOP
            LOCATE FOR BETWEEN(VAL(laData[2]),VAL(GLTYPES.cTyplacno),;
                               VAL(GLTYPES.cTypuacno))
            IF !FOUND()
              *** This value does not fall within any range. ***
              *** Please enter another value. ***
              *** < Ok > ***
              = gfModalGen("TRM02004B00000","DIALOG")
              loFormSet.ChangeMode('S')
              llRet = .F.
            ELSE
              =lfSelOptn(loFormSet)     && Func. have buttons to select action.
            ENDIF
            SELECT GLTYPES
            SET ORDER TO TAG TYPECODE
            SELECT GLSEGVAL
          ELSE
            =lfSelOptn(loFormSet)       && Func. have buttons to select action.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ELSE
  *** Set segments picture to spaces. ***
  FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
    lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
    .&lcObjNam..txtSeg.Value = SPACE(loFormSet.laSegSize[lnCount,1])
    .&lcObjNam..txtSeg.Enabled = .T.
  ENDFOR
  .txtSDesc.Enabled = .F.
  .txtLDesc.Enabled = .F.
  lfEnDsMajSgDetCtrls(loFormSet,.F.)
ENDIF
ENDWITH
SET EXACT &lcExacStat

RETURN llRet

*!**************************************************************************
*!
*!      Function: lfSelOptn
*!
*!**************************************************************************
FUNCTION lfSelOptn
PARAMETERS loFormset

IF glAutoAdd         && If automatic add, option = < add >
  lnOption = 2
ELSE
  *** {Field} is not found in the data file. ***
  *** <Browse> <Add> <Reenter> ***
  lnOption = gfModalGen('QRM00001B00001','Dialog',loFormSet.lcTFldTxt+" "+laData[1]+laData[2])
ENDIF

WITH loFormset.Ariaform1
DO CASE
  CASE lnOption = 1         && Browse.
    FOR lnCount = 1 TO ALEN(loFormSet.laSegSize,1)
      lcObjNam  = 'lcSeg' + ALLTRIM(STR(lnCount))
      .&lcObjNam..txtSeg.Value = SPACE(loFormSet.laSegSize[lnCount,1])
      .&lcObjNam..txtSeg.Enabled = .T.
    ENDFOR

    *** Browse according to segment no. ***
    SELECT GLSEGVAL

    IF gfBrows('laData[1]',lcScFields,'laData')
      loFormSet.ChangeMode('V')
    ENDIF

    lcObjNam = 'lcSeg' + ALLTRIM(STR(lnSegNo))

    llRet = .F.
    RETURN
  CASE lnOption = 2         && Add
    *** If 1st segment get type code & desc. ***
    IF laData[1] = "1"
      loFormSet.lcTypCode  = GLTYPES.cTypeCode
      loFormSet.lcTypDesc  = GLTYPES.cTypeDesc
      laData[8]  = loFormSet.lcTypCode
    ENDIF

    =ACOPY(laData,loFormset.laData)
    *** Go to add mode. ***
    loFormSet.ChangeMode('A')
  CASE lnOption = 3         && Reenter
    *** Go back to select mode again. ***
    loFormSet.ChangeMode('S')
    llRet = .F.
ENDCASE
ENDWITH


*!**************************************************************************
*!
*!      Function: lfvData_12
*!
*!**************************************************************************
FUNCTION lfvData_12
PARAMETERS loformset,loFld

*** Valid func. for allow posting check box. ***
loFormSet.laData[12] = IIF( loFld.Value = 0 , "N" , "Y" )

*!**************************************************************************
*!
*!      Function: lfvData_13
*!
*!**************************************************************************
FUNCTION lfvData_13
PARAMETERS loFormSet,loFld
*** Valid func. for automatic distribution. ***

DECLARE laAutoInfo[1,1]
llBrowse = loFld.Selectedfrombrowse
IF !EMPTY(loFormSet.laData[13]) .OR. llBrowse

    IF SEEK("D","GLAUTHD")

      lcCurFile = ALIAS()
      SELECT GLAUTHD
      *** If found update the user name from automatic dist. file

      IF !SEEK("D"+loFormSet.laData[13],"GLAUTHD") .OR. llBrowse
        IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
            GO RECNO(0)
        ELSE
          GO TOP
        ENDIF
        *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	        lcBrFields     =  "cautcode :H='Distribution code',cautref :H='Distribution reference',"+;
*!*	                          "cautdes  :H='Distribution description'"
*!*	        lcFile_Ttl     = "Automatic Distribution"
        lcBrFields     =  "cautcode :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DISTCODE,loFormSet.GetHeaderText("LANG_DISTCODE",loFormSet.HeaderAlias))+"',cautref :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DISTREF,loFormSet.GetHeaderText("LANG_DISTREF",loFormSet.HeaderAlias))+"',"+;
"cautdes  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DISTDESC,loFormSet.GetHeaderText("LANG_DISTDESC",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl     = LANG_AUTOMATIC
lcFile_Ttl     = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOMATIC,loFormSet.GetHeaderText("LANG_AUTOMATIC",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000682,1 MMT 11/22/2012 Globalization changes[END]

        laAutoInfo [1] = loFormSet.laData[13]

        =gfBrows("'D'","cautcode" ,"laAutoInfo")

        *** If cancel from the brow you will come back  with the same id
        IF loFormSet.laData[13] = laAutoInfo[1]
          loFormSet.laData[13]  = loFld.KeyTextbox.OldValue

        ELSE
          loFormSet.laData[13]  = laAutoInfo[1]
          loFormSet.Ariaform1.kbAutoDistCode.KeyTextBox.Refresh()

          SELECT GLAUTDT
          SET ORDER TO TAG TYPCODACC
          SELECT GLACCHAR
          SET ORDER TO TAG ACCTCODE
          SELECT GLAUTDT
          SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE

          SELECT GLAUTDT
          IF SEEK("D"+loFormSet.laData[13],"GLAUTDT")
            llAutcd_Ok = .T.
            SCAN REST WHILE GLAUTDT.cAutcode = loFormSet.laData[13]
              IF GLACCHAR.cSegactiv = "I"
                *** This  automatic  distribution has an ***
                *** inactive account.  You cannot select ***
                *** this automatic distribution code. ***
                *** <  Ok  > ***
                =gfModalGen("TRM02194B00000","DIALOG")
                llAutcd_Ok = .F.
                EXIT
              ENDIF

              IF GLACCHAR.cStandard <> loFormSet.laData[16]
                *** This automatic distribution code ***
                *** has a { standard : statistical } ***
                *** account.  You cannot select this ***
                *** automatic distribution code.
                *** <  Ok  > ***
                *N000682,1 MMT 11/22/2012 Globalization changes[Start]
                *lcStandStr = IIF(GLACCHAR.cStandard = "Y","standard","statistical")
                *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcStandStr = IIF(GLACCHAR.cStandard = "Y",LANG_STANDARD,LANG_STATISTICAL)
lcStandStr = IIF(GLACCHAR.cStandard = "Y",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STANDARD,loFormSet.GetHeaderText("LANG_STANDARD",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATISTICAL,loFormSet.GetHeaderText("LANG_STATISTICAL",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                *N000682,1 MMT 11/22/2012 Globalization changes[END]
                =gfModalGen("TRM02195B00000","DIALOG",lcStandStr)
                llAutcd_Ok = .F.
                EXIT
              ENDIF

              SELECT GLAUTDT
            ENDSCAN
            IF !llAutcd_Ok
              loFormSet.laData[13]  = lcOldAuto
            ENDIF
          ENDIF

          SELECT GLAUTDT
          SET RELATION TO
        ENDIF
      ENDIF
      SELECT (lcCurFile)
    ELSE

      *** There is no records to display. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00052B00000","DIALOG")
      loFormSet.laData[13] = SPACE(8)
    ENDIF
ENDIF

llBrowse = .F.

*** The report seq. no. ***
*!**************************************************************************
*!
*!      Function: lfvData_9
*!
*!**************************************************************************
FUNCTION lfvData_9
PARAMETERS loFormSet,loFld

IF loFld.Value < 0
  *** Negative values are not allowed. ***
  =gfModalGen("TRM02036B00000","DIALOG")
  loFld.Value = loFld.OldValue
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_15
*!
*!**************************************************************************
FUNCTION lfvData_15
PARAMETERS loFormSet,loFld
*- When the user presses ESC to close the screen , the value in this fields turned into character type
IF TYPE('loFld.Value')<>'N'
  RETURN
ENDIF

*** Valid func. for consolidation per. ***
DO CASE
  CASE loFld.Value > 100
    *** Total percentage cannot exceed 100 %. ***
    =gfModalGen("TRM02017B00000","DIALOG",loFormSet.lcTPrcntg)
    loFld.Value = loFld.OldValue
    RETURN .F.
  CASE loFld.Value < 0
    *** Negative values are not allowed. ***
    =gfModalGen("TRM02036B00000","DIALOG")
    loFld.Value = loFld.OldValue
    RETURN .F.
ENDCASE
************************************************************
*! Name      : lpSavScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : save function
************************************************************
FUNCTION lpSavScr
PARAMETERS loFormSet

lcScFields = loFormSet.lcScFields
DIMENSION laData[ALEN(loFormSet.laData)]
=ACOPY(loFormSet.laData,laData)

laData[12] = IIF(loFormSet.Ariaform1.cbPost.Value=1,'Y','N')

SELECT GLSEGVAL

IF laData[1] = "1"
  *** If 1st segment put the type standard or statistical. ***
  laData[16] = IIF(SUBSTR(laData[8],1,1) = 'Y' , 'N' , 'Y')
ENDIF

IF loFormSet.ActiveMode='A'
  APPEND BLANK
ENDIF

GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info('GLSEGVAL')

SELECT GLSEGVAL
=gfTableUpdate()

*- Release the form if it is in MODAL mode
IF TYPE('loFormSet.pcSegNum') = 'C'
  loFormSet.ariaform1.Release
ENDIF
*- End of lpSavScr.

************************************************************
*! Name      : lpDelScr
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/14/2012
*! Purpose   : Delete function
************************************************************
FUNCTION lpDelScr
PARAMETERS loFormSet

DIMENSION laData[ALEN(loFormSet.laData)]
=ACOPY(loFormSet.laData,laData)

SELECT ACCOD

lnConter   = 1
lnStartPos = 1

GO TOP
IF !EOF()
  lnAryLenth = NACSNOSEG

  DECLARE laSegments[lnAryLenth,2]

  SKIP
  SCAN REST
    laSegments [lnConter,1] = lnStartPos
    laSegments [lnConter,2] = NACSSIZE
    lnStartPos              = lnStartPos + NACSSIZE + 1

    lnConter                = lnConter + 1
  ENDSCAN
ENDIF

lcTmpAcCd   = REPLICATE('_',loFormSet.lnAcsSegSz)

lcTmpAcCd   = stuff(lcTmpAcCd,laSegments[VAL(laData[1]),1],;
                    laSegments[VAL(laData[1]),2],;
                    ALLTRIM(laData[2]))

*** Check that there is no Accounts will be without ***
*** segments if this deletion happen....

gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir
lcScFields = loFormset.lcScFields
SELECT GLACCHAR

lc_TempAc = loformset.lc_TempAc
SELECT CACCTCODE,CACCNSDES ;
     FROM &gcDataDir.GLACCHAR ;
     WHERE CACCTCODE LIKE lcTmpAcCd;
     INTO DBF &gcWorkDir.&lc_TempAc

SELECT (lc_TempAc)
IF _TALLY > 0

  *** There is one or more accounts using this segment value. ***
  *** You cannot delete this segment value.  Do you want to ***
  *** display these accounts ? ***
  *** <  Yes  > <  No  > ***
  IF gfModalGen("QRM02169B00006","DIALOG") = 1

    lcBrFields = "CACCTCODE:H='"+ALLTRIM(LOOKUP(SYDFIELD.cFld_Head,'CACCTCODE',SYDFIELD.cFld_Name,'CFLD_NAME'))+"',"+;
                 "CACCNSDES:H='"+ALLTRIM(LOOKUP(SYDFIELD.cFld_Head,'CACCNSDES',SYDFIELD.cFld_Name,'CFLD_NAME'))+"'"


    =gfBrows('',lcScFields,'laData')

  ENDIF
  SELECT GLSEGVAL
  RETURN .F.
ELSE
  SELECT GLSEGVAL

  DELETE
  =gfTableUpdate()

  loFormSet.ChangeMode('S')
  RETURN .T.
ENDIF
*- End of lpDelScr.

***********************************************************************
*!
*!      FUNCTION: lfvSegOk
*!
*!**************************************************************************
FUNCTION lfvSegOK
PARAMETERS loFormset

lcTSegVal = loFormset.lcTSegVal
lcTSegVal = lcTSegVal+" "+ALLTRIM(loFormSet.laData[2])
WAIT lcTSegVal WINDOW TIMEOUT 2

loFormset.savefiles()
loFormset.Release()
RETURN




************************************************************
*! Name      : lfHideCtrls
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/15/2012
*! Purpose   : Hide some controls based on which the screen is opened on fly or from the menu
************************************************************
FUNCTION lfHideCtrls
PARAMETERS loFormSet

IF loFormSet.laData[1]<>'1'
  WITH loFormSet.Ariaform1
    FOR i=7 TO 21
      k = ALLTRIM(STR(i))
      IF TYPE('loFormSet.Ariaform1.Label&k')='O'
      .Label&k..Visible = .F.
      ENDIF
    ENDFOR
    .ibPopStat.Visible = .F.
    .ibPopCatg.Visible = .F.
    .ibPopTerm.Visible = .F.
    .puCash.Visible = .F.
    .puRatio.Visible = .F.
    .cbPost.Visible = .F.
    .kbAutoDistCode.Visible = .F.
    .txtConsCoAct.Visible = .F.
    .txtRepSeqNo.Visible = .F.
    .txtConsPrcnt.Visible = .F.
    .Ariashape3.Visible = .F.

    .Height = .Height - .Ariashape3.Height - 30

    .cmdOk.Top = .cmdOk.Top - .Ariashape3.Height
    .cmdCancel.Top = .cmdCancel.Top - .Ariashape3.Height
  ENDWITH
ENDIF
*- End of lfHideCtrls.
