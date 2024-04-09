*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\GL\GLCASHF.PRG
*:  Module      : General Ledger
*:  Desc.       : Source Journal screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/30/2012
*:  Reference   : *E303275,1
*:************************************************************************
*- Get the screen , call it
*N000682,1 MMT 12/03/2012 Globalization changes[Start]
#INCLUDE R:\Aria4xp\prgs\gl\glcashf.h
*N000682,1 MMT 12/03/2012 Globalization changes[end]
lcPrg = JUSTSTEM(SYS(16))
lcRunScx = lfGetScx("GL\&lcPrg..scx")
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

loFormSet.AddProperty('lcProgName',lcPrg)

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
  .cBrowseIndexExpression = "CSEGCAFLO"
  .cBrowseIndexFields     = "CSEGCAFLO"
  .cBrowseIndexName       = "CFICODE"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = ""
  *N000682,1 MMT 12/03/2012 Globalization changes[Start]
*!*	  .BrowseTitle 		  	  = 'Cash Flow Item'

*!*	  .ariaBrFields.edtBrowseFields.Value = "CSEGCAFLO :H='Cash flow code',"+;
*!*	                                        "CCFIDESC  :H='Item description',"+;
*!*	                                        "CCFIADJ   :H='Net income adj. type'"
  .BrowseTitle 		  	  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHFLOWITEM,loFormSet.GetHeaderText("LANG_CASHFLOWITEM",loFormSet.HeaderAlias))
  .ariaBrFields.edtBrowseFields.Value = "CSEGCAFLO :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHFLOWCODEBR,loFormSet.GetHeaderText("LANG_CASHFLOWCODEBR",loFormSet.HeaderAlias))+"',"+;
                                        "CCFIDESC  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ITEMDESC,loFormSet.GetHeaderText("LANG_ITEMDESC",loFormSet.HeaderAlias))+"',"+;
                                        "CCFIADJ   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NETINCOME,loFormSet.GetHeaderText("LANG_NETINCOME",loFormSet.HeaderAlias))+"'"
  *N000682,1 MMT 12/03/2012 Globalization changes[ENd]                                        
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

*- Define needed variables.
=lfDefineVars(loFormSet)

*- Get select mode
loFormSet.ChangeMode('S')

*- End of lfFormInit.

************************************************************
*! Name      : lfSetControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/05/2012
*! Purpose   : Set the Controls data Source
************************************************************
FUNCTION lfSetControlSource
PARAMETERS loFormSet
WITH loFormSet.Ariaform1
  .lacfgroup.RowSource = 'Thisformset.lacfgroup'
  .lacfgroup.ControlSource = 'ThisFormSet.lcCfGroup'
  .lcCfGroup.ControlSource = 'ThisFormSet.lcCfGroup'
  .rbAdj.ControlSource = 'ThisFormSet.rbAdj'
  .laData1.Keytextbox.ControlSource = 'Thisformset.laData[1]'
  .laData2.ControlSource = 'Thisformset.laData[2]'
  .laData3.ControlSource = 'Thisformset.laData[3]'
ENDWITH
*- End of lfSetControlSource.

************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/07/2012
*! Purpose   : lfFormActivate method
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet

*- End of lfFormActivate

************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/23/2012
*! Purpose   : Define screen variables
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir

loFormSet.AddProperty('laKeyField[1,4]')
loFormSet.AddProperty('laCfGroup[3,2]')
loFormSet.AddProperty('laPhIName[1]')

loFormSet.laKeyField[1,1] = 'laData[1]'
loFormSet.laKeyField[1,2] =.T.
loFormSet.laKeyField[1,3] = 'CFICODE'
loFormSet.laKeyField[1,4] = 1

*** Array to hold the activity groups name
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	loFormSet.laCfGroup[1,1] = 'Operating activities'
*!*	loFormSet.laCfGroup[1,2] = 'O'
*!*	loFormSet.laCfGroup[2,1] = 'Investment activities'
*!*	loFormSet.laCfGroup[2,2] = 'I'
*!*	loFormSet.laCfGroup[3,1] = 'Financing activities'
*!*	loFormSet.laCfGroup[3,2] = 'F'
loFormSet.laCfGroup[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN" or TYPE('loFormSet.HeaderAlias')<>'C',LANG_OPERACT,loFormSet.GetHeaderText("LANG_OPERACT",loFormSet.HeaderAlias)) 
loFormSet.laCfGroup[1,2] = 'O'
loFormSet.laCfGroup[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN" or TYPE('loFormSet.HeaderAlias')<>'C',LANG_INVACT,loFormSet.GetHeaderText("LANG_INVACT",loFormSet.HeaderAlias)) 
loFormSet.laCfGroup[2,2] = 'I'
loFormSet.laCfGroup[3,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN" or TYPE('loFormSet.HeaderAlias')<>'C',LANG_FINACT,loFormSet.GetHeaderText("LANG_FINACT",loFormSet.HeaderAlias)) 
loFormSet.laCfGroup[3,2] = 'F'
*N000682,1 MMT 11/22/2012 Globalization changes[END]
*** group character  _ the first character of cash flow id
loFormSet.AddProperty('lcCfGroup'      , 'O')
loFormSet.AddProperty('rbAdj',  1 )


lcScFields = 'CSEGCAFLO,CCFIDESC,MCFINOTES,CCFIADJ'
loFormSet.Addproperty('lcScFields',lcScFields)
lcCnt = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcCnt.]')

SELECT (loformSet.lcbasefile)
SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

loFormSet.rbAdj     = IIF(loFormSet.laData[4] = "D" , 1 , 2 )
puCfgroup = IIF(AT(loFormSet.lcCfGroup,"OIF") > 0 , AT(loFormSet.lcCfGroup,"OIF"),0)
loFormSet.AddProperty('puCfGroup',puCfGroup)

*- End of lfDefineVars.

*!**************************************************************************
*!
*!      Procedure: lfChangeMode
*!
*!**************************************************************************
PROCEDURE lfChangeMode
PARAMETERS loFormSet
IF TYPE('loFormSet.lcProgName')='U'
  RETURN
ENDIF

*- Set the Controls data Source
lfSetControlSource(loFormSet)
lcScFields = loFormSet.lcScFields

SELECT (loFormSet.lcBaseFile)
DO CASE
  *** Select mode
  CASE loFormSet.ActiveMode = 'S'
    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
    loFormSet.puCfGroup = 1            && IIF(BETWEEN(puCfGroup,1,3),puCfGroup,1)
    loFormSet.lcCfGroup = "O"          && laCfGroup[puCfGroup,2]
    loFormSet.laData[4] = "D"
    loFormSet.rbAdj     = 1

    WITH loFormset.Ariaform1
    .lacfgroup.Value = loFormSet.lcCfGroup
    .lcCfGroup.Value = loFormSet.lcCfGroup
    .lacfgroup.Enabled = .T.
    .laData1.Enabled = .T.
    .laData2.Enabled = .F.
    .rbAdj.Enabled = .F.
    .laData3.ReadOnly = .T.

    .lacfgroup.SetFocus()
    .Refresh()
    ENDWITH

  *** View mode
  CASE loFormSet.ActiveMode = 'V' .OR. loFormSet.ActiveMode = 'E'
    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData

    loFormSet.rbAdj     = IIF(loFormSet.laData[4] = "D" , 1 , 2 )
    loFormSet.laData[1] = glcfitem.csegcaflo  && store the Original ID into laData[1]
    loFormSet.lcCfGroup = LEFT(loFormSet.laData[1],1)   && Divid the Original ID into Group character
    loFormSet.laData[1] = SUBSTR(loFormSet.laData[1],2) && And the number
    loFormSet.puCfGroup = 0                   && rest the popup bottom

    ***  Search for the group character to deispaly the popup bottom
    FOR lnCount  = 1 TO ALEN(loFormSet.laCfGroup,1)
      IF loFormSet.laCfGroup [lnCount,2] == loFormSet.lcCfGroup
        loFormSet.puCfGroup = lnCount
        EXIT
      ENDIF
    ENDFOR
    IF loFormSet.ActiveMode = 'V'

      WITH loFormset.Ariaform1
        .lacfgroup.Enabled = .F.
        .laData1.Enabled = .F.
        .laData2.Enabled = .F.
        .rbAdj.Enabled = .F.
        .laData3.ReadOnly = .T.
      ENDWITH
    ELSE
      WITH loFormset.Ariaform1
        .lacfgroup.Enabled = .F.
        .laData1.Enabled = .F.
        .laData3.ReadOnly = .F.
      ENDWITH
      IF VAL(loFormSet.laData[1]) <= 50

        loFormset.Ariaform1.rbAdj.Enabled = .F.
      ELSE

        loFormset.Ariaform1.rbAdj.Enabled = .T.
      ENDIF
    ENDIF

  CASE loFormSet.ActiveMode = 'A'
    WITH loFormset.Ariaform1
      .lacfgroup.Enabled = .F.
      .laData1.Enabled = .F.
      .laData3.ReadOnly = .F.
    ENDWITH
    IF VAL(SUBSTR(loFormSet.laData[1],2)) <= 50
      loFormset.Ariaform1.rbAdj.Enabled = .F.
    ELSE
      loFormset.Ariaform1.rbAdj.Enabled = .T.
    ENDIF
ENDCASE

*** check if the ID Smaller than 50  -- From 1 to 50 reseved by the system
IF VAL(loFormSet.laData[1]) <= 50
  *&& disable the delete bottom
  loFormSet.otoolbar.cmddelete.Enabled = .F.
ELSE
  IF loFormSet.ActiveMode = 'V'
    *&& Enable the delete bottom
    loFormSet.otoolbar.cmddelete.Enabled = .T.
  ENDIF
ENDIF

loFormSet.Ariaform1.cmdZoom.Enabled = loFormSet.ActiveMode <> 'S'

*!**************************************************************************
*!
*!      Function : lfvCfGroup
*!
*!**************************************************************************
FUNCTION lfvCfGroup
PARAMETERS loFormSet,loFld
loFormSet.Ariaform1.lcCfGroup.Value = loFormSet.Ariaform1.laCfGroup.Value
loFormSet.Refresh()
loFormSet.Ariaform1.laData1.Setfocus()


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
PARAMETERS loFormset,loFld
lcBrFields = loFormSet.ariaBrFields.edtBrowseFields.Value

IF !EMPTY(loFormSet.laData[1]) OR loFld.Selectedfrombrowse
  IF ATC("?",loFormSet.laData[1]) > 0 OR loFld.Selectedfrombrowse
    *=gfBrows()
    IF loFormSet.oToolBar.cmdFind.Click()
      loFormSet.ChangeMode('V')
    ENDIF
  ELSE
    IF ISDIGIT(loFormSet.laData[1])
      loFormSet.laData[1] = ALLTRIM(loFormSet.lcCfGroup+loFormSet.laData[1]) && make the whole Id
      IF !SEEK(loFormSet.laData[1],loFormSet.lcBaseFile) AND !BETWEEN(VAL(SUBSTR(loFormSet.laData[1],2)),51,99)
        loFormSet.laData[1] = SPACE(FSIZE(csegcaflo))
        *** Available lines for the ð activities section from 51 to 99
        =gfModalGen("INM02170B00000",'DIALOG',;
                      LOWER(loFormSet.laCfGroup[loFormSet.puCfGroup,1])+"|"+"51"+"|"+"99")
        loFormSet.laData[1] = ''
        RETURN .F.
      ENDIF

      IF !SEEK(loFormSet.laData[1],loFormSet.lcBaseFile)
        **** \!\<Browse;\<Add;\?\<Reenter
        *N000682,1 MMT 11/22/2012 Globalization changes[Start]
        *lnResp = gfModalGen('INM00001B02004','DIALOG','Cash Flow Code:'+loFormSet.laData[1])        
        lnResp = gfModalGen('INM00001B02004','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHFLOW,loFormSet.GetHeaderText("LANG_CASHFLOW",loFormSet.HeaderAlias))+loFormSet.laData[1])
        *N000682,1 MMT 11/22/2012 Globalization changes[END]
        DO case
        CASE lnResp = 1
          IF !loFormSet.oToolBar.cmdFind.Click()
            loFormSet.laData[1] = ' '
            RETURN .F.
          ENDIF
        CASE lnResp = 2
          loFormSet.ChangeMode('A')
          loFormSet.laData[1] = SUBSTR(loFormSet.laData[1],2)
        CASE lnResp = 3
          loFormSet.laData[1] = ' '
          RETURN .F.
        ENDCASE

      ELSE
        loFormSet.ChangeMode('V')
      ENDIF
    ELSE
      ** You have to enter the item line number
      =gfModalGen("INM02186B00000",'DIALOG')   		
      loFormSet.laData[1] = " "
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      PROCEDURE lfvAdj
*!
*!**************************************************************************
*
FUNCTION lfvAdj
PARAMETERS loFormset,loFld

loFormSet.laData[4] = IIF(loFld.Value = 1 , "D" , "A" )

*!**************************************************************************
*!
*!      PROCEDURE lpSavScr
*!
*!**************************************************************************
* Save the informaation
PROCEDURE lfFormBeforeSave
PARAMETERS loFormSet

IF EMPTY(loFormSet.laData[2])
  *** you have to enter ð description
  =gfModalGen("INM02171B00000","DIALOG",IIF(loFormSet.puCfGroup=0,''," "+loFormSet.laCfGroup[loFormSet.puCfGroup,1])+" ")
  llcSave = .F.
  loFormSet.Ariaform1.laData2.Setfocus()
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      PROCEDURE lpSavScr
*!
*!**************************************************************************
* Save the informaation
PROCEDURE lpSavScr
PARAMETERS loFormSet

*** delete any space character
loFormSet.laData[1] = ALLTRIM(loFormSet.lcCfGroup+loFormSet.laData[1])

IF loFormSet.ActiveMode = 'A'
  APPEND BLANK
ENDIF

lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData FIELDS &lcScFields MEMO
=gfAdd_Info(loFormSet.lcBaseFile)
gfTableUpdate()

*!**************************************************************************
*!
*!      PROCEDURE: lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lfFormDelete
PARAMETERS loFormset

llUsed     = .F.
lnNOfFiles = 0

SELECT SYDFLFLD
gcSysHome = oAriaApplication.SysPath
gcDataDir = oAriaApplication.DataDir

SELECT cFile_nam , .F. AS ln FROM SYDFLFLD WHERE ;
       cFld_name = "CSEGCAFLO" AND ;
       cFile_nam <> "GLCFITEM.DBF" AND ;
       cFile_nam <> "GLCFITEM" ;
       GROUP BY cFile_nam ;
       INTO ARRAY laFiles

*** Store the number of array
lnNOfRec = _TALLY

*** Check if the record information used by another files
FOR lnCount = 1 TO lnNOfRec
  IF EMPTY(laFiles[lnCount])
    LOOP
  ENDIF
  *** Store the alias of the file
  laFiles[lnCount] = ALLTRIM(laFiles[lnCount])

  laFiles[lnCount] = IIF(AT(".",laFiles[lnCount])>0,;
                     SUBSTR(laFiles[lnCount],1,AT(".",laFiles[lnCount])-1),;
                     laFiles[lnCount])

  IF NOT USED(laFiles[lnCount])
    SELECT 0

    IF LEFT(laFiles[lnCount],2) = 'SY'
      llUsed = gfOpenFile(oAriaApplication.SysPath+laFiles[lnCount],'','SH')
    ELSE
      llUsed = gfOpenFile(oAriaApplication.DataDir+laFiles[lnCount],'','SH')
    ENDIF

    SELECT (laFiles[lnCount])
  ENDIF

  SELECT (laFiles[lnCount])

  *** Search for the record which match the record information
  LOCATE FOR CSEGCAFLO = ALLTRIM(loFormSet.lcCfGroup+loFormSet.laData[1])
  *- If the code found in the file.
  IF FOUND()
    lnNOfFiles = lnNOfFiles + 1  && the number of files that share info.

    IF laFiles[lnCount] = "GLACCHAR"
      *** Store the account code that used the cash flow code
      lcAccNum =laFiles[lnCount]+".CACCTCODE"
      lcAccNum = "account "+ALLTRIM(&lcAccNum)+" in the chart of account"
    ELSE
      llUsedFil = .F.
      IF NOT USED("SYDFILES")
        SELECT 0
        USE &gcSysHome.SYDFILES
        llUsedFil = .T.
      ENDIF

      SELECT SYDFILES
      DECLARE laPhIName[1]
      SELECT CFILE_TTL ;
      FROM SYDFILES ;
      WHERE CFILE_NAM = (laFiles[lnCount]);
      INTO ARRAY laPhiName

      IF llUsedFil
        SELECT SYDFILES
        USE
      ENDIF
      *N000682,1 MMT 11/22/2012 Globalization changes[Start]
      *lcAccNum = "file "+laPhiName[1]
      lcAccNum = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FILE,loFormSet.GetHeaderText("LANG_FILE",loFormSet.HeaderAlias)) +laPhiName[1]
      *N000682,1 MMT 11/22/2012 Globalization changes[eND]
    ENDIF
    EXIT
  ENDIF
  *B600435,1 If the file was not used from before, close it.
  IF llUsed
    USE IN laFiles[lnCount]
    llUsed = .F.
  ENDIF
ENDFOR

IF NOT USED("GLCFITEM")
  USE &gcDataDir.GLCFITEM IN SELECT(1)
ENDIF

SELECT GLCFITEM   && select the original file
*** Check if there is any file share information
IF lnNOfFiles > 0
  *** Cash flow code ð is used by account ð in the chart of accounts file
  =gfModalGen("INM02173B00000","DIALOG",loFormSet.lcCfGroup+loFormSet.laData[1]+"|"+ALLTRIM(lcAccNum ))
  loFormSet.ChangeMode('V')
ELSE
  DELETE
  gfTableUpdate()

  loFormSet.ChangeMode('S')
ENDIF
