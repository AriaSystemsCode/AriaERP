*****************************************************************************
*: Program file        : GLCPGRP.PRG
*: Program desc.       : Copy Group Entry
*: Module              : General Ledger (GL)
*: System		       : Aria Apparel System (A4xp)
*: Developer	       : Saber Saber [SAB]
*: Tracking Job Number : E303272.EXE
*: Date                : 10/09/2012
*****************************************************************************
*Modifications :
*****************************************************************************
*: N000682,1 HES Globalization Changes [Globalization Project]

llNoContrl  = .T.

lcSGrpDt    = " "
lcTmpGridSrc   = " "    && Temp file name for selected accounts
lc_Temp1    = " "    && Temp file name for group lines
lc_Temp2    = " "    && Temp file name for selected criteria
lcAlTypes   = " "    && Alias name to GLTYPES  in source company
lcAlAcCha   = " "    && Alias name to GLACCHAR in source company
lnNoOfAcc   = 0      && var. to store number of selected accounts
lnRecNum    = 0
lnAction    = 0
lnRecNo     = 0
lc_Types    = " "
lcGrpCode   = " "
lcBut1Stat  = "DISABLE"
lcBut2Stat  = "DISABLE"
lc_Title    = "Group accounts"
lcPrompt    = "\<Select"
lsTypes     = 1
puPopcom    = 1
lcList_Str  = " "
lcAccSeg    = " "
lcTmpGridSrc   = " "
lcAlTypes   = " "
lcAlAcCha   = " "
lcGrpHd     = " "
lcSGrPDt    = " "
lcVGrpDt    = " "
lcGrpDt     = " "
*: N000682,1 HES Globalization Changes [Start]
*!*	lcEmptyTxt  = 'No company Selected'
*!*	laCtrStat   = 'DISABLE' && To disable the browse pad in the menu
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcEmptyTxt  = LANG_No_Company
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcEmptyTxt  = LANG_No_Company
lcEmptyTxt  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Company,loFormSet.GetHeaderText("LANG_No_Company",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCtrStat   = LANG_DISABLE && To disable the browse pad in the menu
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCtrStat   = LANG_DISABLE && To disable the browse pad in the menu
laCtrStat   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DISABLE,loFormSet.GetHeaderText("LANG_DISABLE",loFormSet.HeaderAlias)) && To disable the browse pad in the menu
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

*: N000682,1 HES Globalization Changes [End  ]

LSDETAIL    = ' '



*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#INCLUDE R:\Aria4xp\PRGs\GL\GLCOPY.H

SET PROCEDURE TO ADDBS(oAriaApplication.ProgramHome)+'GL\GL.FXP' ADDITIVE

** lc_SelBuHd : Temp file name for selected budgets   ====> lcTmpGridSrc
** lc_SelBuDt : Temp file name for budget lines
** lc_ViewDt  : Temp file name for view budget details
** lcAlBudHd  : Alias name for budget header file
** lcAlBudDt  : Alias name for budget detail file
** lcAlAcCha  : Alias name for accounts chart file
** lc_BudCode : Variable to hold budget code
** lc_BudYear : Variable to hold budget year

** lnReCopied : Variable to hold number of copied records.
** lnlistLine : Variable to hold record pointer in the list
** lnHdRecNo  : Variable to hold record number in the header file
** lnSelected :
** lnTotalRecs:
** llValdCopy : Variable to hold copy status

STORE " " TO lcAccSeg, lcTmpGridSrc, lc_SelBuDt, lc_ViewDt, lcAlBudHd, lcAlBudDt, lcAlAcCha, lcBud, lcList_Str, lc_BudCode, lc_BudYear
STORE 0   TO lnReCopied, lnlistLine, lsBud, lnHdRecNo, lnSelected, lnTotalRec
STORE .T. TO llNoContrl, llValdCopy

PRIVATE lcTmpGridSrc, lcAutDt, lcAutHd, lcAlTypes, lcAlAcCha, lcTAutDt
STORE '' TO lcTmpGridSrc, lcAutDt, lcAutHd, lcAlTypes, lcAlAcCha

*- Create the Name for the temporary file
lcTmpGridSrc = gfTempName()  && lc_SelGrp
lcAlTypes    = gfTempName()
lcAlAcCha    = gfTempName()
lcGrpHd      = gfTempName()
lcSGrPDt     = gfTempName()
lcVGrpDt     = gfTempName()
lcGrpDt      = gfTempName()
lc_ViewDt    = gfTempName()
*- Open used files and Create temp files
=lfPrepFls()

*- Check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE
IF EOF()
  * The chart of accounts is empty. You have to create the chart of accounts first.
  *                  < Ok >
  =gfModalGen("TRM02215B00000","DIALOG")
  RETURN
ENDIF

PRIVATE laComLst
DIMENSION laComLst[1,1]

*- Func. in the GL.PRG to see if there is other companies have the same account code strucure.
IF !lfSamAcode()
  glQuitting=.T.
  RETURN
ENDIF

SELECT (lcTmpGridSrc)

DO FORM (oAriaApplication.ScreenHome+"\GL\GLCOPY.SCX")


*!*************************************************************
*! Name      : lfFormLoad
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Called from the Screen Load Method
*!*************************************************************
FUNCTION lfFormLoad
PARAMETERS loFormSet

SET MULTILOCKS ON

*- Add property
=lfAddProp(loFormSet,'lcTmpGridSrc', lcTmpGridSrc)  && lc_SelBuHd
=lfAddProp(loFormSet,'lc_ViewDt', lc_ViewDt)
=lfAddProp(loFormSet,'lcAlTypes', lcAlTypes)
=lfAddProp(loFormSet,'lcAlAcCha', lcAlAcCha)
=lfAddProp(loFormSet,'lcGrpHd', lcGrpHd)
=lfAddProp(loFormSet,'lcGrpDt', lcGrpDt)
=lfAddProp(loFormSet,'lcSGrPDt', lcSGrPDt)
=lfAddProp(loFormSet,'lcVGrpDt', lcVGrpDt)

ENDFUNC
*- End of lfFormLoad


*!*************************************************************
*! Name      : lfFormInit
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

*- Fill To Company textboxes
loFormSet.AriaForm1.txtToCompanyCode.Value = oAriaApplication.ActiveCompanyID
loFormSet.AriaForm1.txtToCompanyName.Value = oAriaApplication.ActiveCompanyName
loFormSet.AriaForm1.txtToCompanyCode.Enabled = .F.
loFormSet.AriaForm1.txtToCompanyName.Enabled = .F.

*- Fill cboFromCompany Combobox
WITH loFormSet.AriaForm1.cboFromCompany
  DIMENSION .aSourceArray[ALEN(laComLst,1)+1, 2]
  *: N000682,1 HES Globalization Changes [Start]
*!*	  .aSourceArray[1,1]    = 'No company selected'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.aSourceArray[1,1]    = LANG_No_Company
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*.aSourceArray[1,1]    = LANG_No_Company
.aSourceArray[1,1]    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Company,loFormSet.GetHeaderText("LANG_No_Company",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *: N000682,1 HES Globalization Changes [End  ]
  .aSourceArray[1,2]    = '*'
  ACOPY(laComLst, .aSourceArray, 1, -1, 3)
  .Value = '*'
ENDWITH

*- Copy Option Button Caption
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdCopyOption.Caption = LANG_View_Budget_Details
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdCopyOption.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_View_Budget_Details,loFormSet.GetHeaderText("LANG_View_Budget_Details",loFormSet.HeaderAlias))
loFormSet.AriaForm1.cmdCopyOption.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_View_Budget_Details,loFormSet.GetHeaderText("LANG_View_Budget_Details",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_View_Budget_Details",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]


*- Adjust Grid Control Source
WITH loFormSet.AriaForm1.grdCopyRecords
  LOCAL lcTmpGridSrc
  lcTmpGridSrc = loFormSet.lcTmpGridSrc
  .ColumnCount = 4
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpGridSrc
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.AriaForm1.grdCopyRecords

  *Inc, cGrpCode,SUBSTR(cGrpLnHed,1,38), CopyStat
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..Inc", '')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cGrpCode", LANG_GLCPGRP_Group_Code, 70)                  &&"Group Code"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cGrpCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPGRP_Group_Code,loFormSet.GetHeaderText("LANG_GLCPGRP_Group_Code",loFormSet.HeaderAlias)),
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cGrpCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPGRP_Group_Code,loFormSet.GetHeaderText("LANG_GLCPGRP_Group_Code",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPGRP_Group_Code",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cGrpLnHed, 1, 38)", LANG_GLCPGRP_Group_Desc, 250) &&"Group Description"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cGrpLnHed, 1, 38)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPGRP_Group_Desc,loFormSet.GetHeaderText("LANG_GLCPGRP_Group_Desc",loFormSe
=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cGrpLnHed, 1, 38)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPGRP_Group_Desc,loFormSet.GetHeaderText("LANG_GLCPGRP_Group_Desc",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPGRP_Group_Desc",loFormSe
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,LANG_GLCPGRP_CopyStat, 150)                &&"Copy Status"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPGRP_CopyStat,loFormSet.GetHeaderText("LANG_GLCPGRP_CopyStat",loFormSet.HeaderAlias)),
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPGRP_CopyStat,loFormSet.GetHeaderText("LANG_GLCPGRP_CopyStat",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPGRP_CopyStat",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]


  *- Remove extra columns
  FOR lnX = lnI+1 TO oGrid.ColumnCount
    lcX = ALLTRIM(STR(lnX))
    oGrid.RemoveObject("Column&lcX")
  ENDFOR

  .READONLY = .T.
  .Refresh()
  .DoScroll(2)
ENDWITH

loFormSet.AriaForm1.cboFromCompany.Valid()

ENDFUNC
*- End of lfFormInit


*!*************************************************************
*! Name      : lfFormDestroy
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/03/2012
*! Purpose   : Called from the Screen Destroy Method
*!*************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet


=gfCloseTable('GLGRPHD')
=gfCloseTable('GLGRPDT')
=gfCloseTable('GLTYPES')
=gfCloseTable('GLACCHAR')
=gfCloseTable('ACCOD')
=gfCloseTable('SYDFIELD')

RETURN .T.

ENDFUNC
*- End of lfFormDestroy


*!*************************************************************
*! Name      : lfAddProp
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : A function to add properties to the object that passed as a parameter
*!*************************************************************
FUNCTION lfAddProp
PARAMETERS loObj,lcPropName,PropValue
*- check if lcPropname is not just a variable, but a list of variables, then create a loop to walk around
LOCAL lnI,lnLen,lcPropToCreate
lcPropName = lcPropName + ','
lnLen = OCCURS(',',lcPropName)
FOR lnI = 1 TO lnLen
  lcPropToCreate = ALLTRIM(SUBSTR(lcPropName,1,AT(',',lcPropName)-1))
  IF TYPE('loObj.&lcPropToCreate')='U'
    loObj.AddProperty(lcPropToCreate,PropValue)
  ENDIF
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR

ENDFUNC
*- End of lfAddProp


*!*************************************************************
*! Name      : lfAddColumn
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : A function to add columns to the passed grid object
*!*************************************************************
FUNCTION lfAddColumn
LPARAMETERS lnI,oGrid,lcFld,lcTitle, lnWidth
LOCAL lcI
lnI = lnI + 1
lcI = ALLTRIM(STR(lnI))
WITH oGrid
  .Column&lcI..ControlSource    = lcFld
  .Column&lcI..Header1.Caption  = lcTitle
  IF TYPE('lnWidth') = 'N' .AND. lnWidth > 0
    .Column&lcI..Width  = lnWidth
  ENDIF
ENDWITH
ENDFUNC
*- End of lfAddColumn.


*!**************************************************************************************************************************
*!                                                   Custome Code for Each Screen
*!**************************************************************************************************************************
*!*************************************************************
*! Name      : lfPrepFls
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Create Screen Temp Files and Open used Files
*!*************************************************************
FUNCTION lfPrepFls

*- Open Tables
=gfOpenTable('GLGRPHD','GRPCODE','SH')      && CGRPCODE
=gfOpenTable('GLGRPDT','GRCODACC','SH')     && CGRPCODE+CACCTCODE
=gfOpenTable('GLTYPES','TYPECODE','SH')     && CTYPECODE
=gfOpenTable('GLACCHAR','ACCTCODE','SH')    && CACCTCODE
=gfOpenTable('ACCOD','ACCSEGNO','SH')       && ALLTRIM(STR(NACSSEGNO))
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYDFIELD','CFLD_NAME','SH')   && CFLD_NAME

*- Create empty temp file (lcTmpGridSrc) with the same structure of GLTYPES + Inc, CopyStat,cStatus.
SELECT GLGRPHD
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2, 18]

laFileStru[lnFileStru+1,1] = 'Inc'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'CopyStat'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 14
laFileStru[lnFileStru+2,4] = 0

FOR lnLoop = 1 to 2
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop, 7] ,laFileStru[lnFileStru+lnLoop, 8],;
                laFileStru[lnFileStru+lnLoop, 9] ,laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11] ,laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13] ,laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15] ,laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO  laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR

CREATE TABLE ADDBS(oAriaApplication.Workdir)+(lcTmpGridSrc)+'.DBF' FROM ARRAY laFileStru

SELECT (lcTmpGridSrc)
INDEX ON cGrpCode TAG CGRPCODE

*- Create empty temp file (lc_ViewDt) with the same structure of GLGRPDT + account description.
SELECT GLGRPDT

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1, 18]

laFileStru[lnFileStru+1, 1] = 'caccnldes'
laFileStru[lnFileStru+1, 2] = 'C'
laFileStru[lnFileStru+1, 3] = 40
laFileStru[lnFileStru+1, 4] = 0

FOR lnLoop = 1 to 1
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop, 7] ,laFileStru[lnFileStru+lnLoop, 8],;
                laFileStru[lnFileStru+lnLoop, 9] ,laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11] ,laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13] ,laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15] ,laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO  laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR

CREATE TABLE ADDBS(oAriaApplication.Workdir)+(lc_ViewDt)+'.DBF' FROM ARRAY laFileStru

ENDFUNC
*-End of lfPrepFls


*!*************************************************************
*! Name      : lfOpenSrcFiles
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Open files of the source company to copy from
*!*************************************************************
FUNCTION lfOpenSrcFiles
LPARAMETERS loFormSet

LOCAL lcTmpGridSrc, lc_ViewDt, lcAlBudDt, lcAlAcCha, lcComPath
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lc_ViewDt    = loFormSet.lc_ViewDt
lcAlTypes    = loFormSet.lcAlTypes
lcSGrpDt     = loFormSet.lcSGrpDt
lcAlAcCha    = loFormSet.lcAlAcCha
lcGrpHd      = loFormSet.lcGrpHd
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))


*- Select (lcTmpGridSrc) and removes all records
SELECT (lcTmpGridSrc)
ZAP

*- Select (lc_ViewDt) and removes all records
SELECT (lc_ViewDt)
ZAP

IF USED(lcAlTypes)
  SELECT (lcAlTypes)
ELSE
  SELECT 0
ENDIF
*- Use GLTypes from the source company in the path lcComPath and set index to order tag TYPECODE
USE &lcComPath.GLTypes  AGAIN ALIAS  &lcAlTypes ORDER TAG TYPECODE

IF USED(lcSGrpDt)
  SELECT (lcSGrpDt)
ELSE
  SELECT 0
ENDIF
*- Use glGrpDt from the source company in the path lcComPath
USE &lcComPath.GLGRPDT AGAIN ALIAS &lcSGrpDt ORDER TAG GRCODACC

IF USED(lcAlAcCha)
  SELECT (lcAlAcCha)
ELSE
  SELECT 0
ENDIF
*- Use glGrpDt from the source company in the path lcComPath
USE &lcComPath.GLACCHAR AGAIN ALIAS &lcAlAcCha ORDER TAG ACCTCODE

SELECT ACCOD
IF !EOF()
  lcAccSeg = cAcSegDes
ELSE
  lcAccSeg = ""
ENDIF

IF USED(lcGrpHd)
  SELECT (lcGrpHd)
ELSE
  SELECT 0
ENDIF
*- use glGrpHd from the source company in the path lcComPath
USE &lcComPath.GLGRPHD AGAIN ALIAS &lcGrpHd ORDER TAG GRPCODE

lnTotalRec = RECCOUNT()

IF RECNO() <= RECCOUNT()
  GO TOP
ENDIF

IF EOF()
  USE
  SELECT (lcTmpGridSrc)
  =gfModalGen("TRM02119B00000","Dialog","group")
  RETURN .F.
ENDIF

lnSelected  = 0

SELECT (lcGrpHd)
SCAN
  SCATTER MEMO MEMVAR
  m.Inc      = .F.
  m.CopyStat = SPACE(14)
  SELECT(lcTmpGridSrc)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN

SELECT(lcTmpGridSrc)
INDEX ON cGrpCode TAG CGRPCODE
SET ORDER TO TAG CGRPCODE
SET RELATION TO cGrpCode INTO &lcGrpHd ADDITIVE

SELECT (lcTmpGridSrc)
LOCATE
loFormSet.AriaForm1.grdCopyRecords.Refresh()

ENDFUNC
*- End of lfOpenSrcFiles


*!*************************************************************
*! Name      : lfClearVar
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Call the Option Screen (Change from
*!             a Copy entry to another
*!*************************************************************
FUNCTION lfClearVar
LPARAMETERS loFormSet


ENDFUNC
*-End of lfClearVar


*!*************************************************************
*! Name      : lfvCopyOption
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Call the Option Screen (Change from
*!             a Copy entry to another
*!*************************************************************
FUNCTION lfvCopyOption
LPARAMETERS loFormSet

=lfvView(loFormSet)

ENDFUNC


*!*************************************************************
*! Name      : lfvCopy
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Copy selected records from the grid to active Company
*!*************************************************************
FUNCTION  lfvCopy
LPARAMETERS loFormSet

PRIVATE lcTmpGridSrc, lc_ViewDt, lcSGrpDt, lcGrpDt, lcAlAcCha, lcComPath, lcWorkDir, lcDataDir
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lc_ViewDt    = loFormSet.lc_ViewDt
lcSGrpDt     = loFormSet.lcSGrpDt
lcGrpDt      = loFormSet.lcGrpDt
lcAlAcCha    = loFormSet.lcAlAcCha
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))
lcWorkDir    = oAriaApplication.WorkDir
lcDataDir    = oAriaApplication.DataDir

lnTotalRecs = 0
SELECT (lcTmpGridSrc)
lnRecNum = RECNO()

*- Count the number of included groups to the variable lnTotRecs
COUNT FOR (Inc) TO lnTotalRecs

*- Initialize no. of copied groups , current type variable.
lnCopyTypes = 0               && init. the no of copied groups.
lnCurRec    = 0               && init. the current type variable.
lnSelected  = 0

*- Check if there is any selected groups to copied
IF lnTotalRecs = 0
  *- case there is no selected groups to copyied
  *** Message "No included groups to copy."
  ***              < Ok >
  =gfModalGen("TRM02115B00000","DIALOG")
  RETURN
ENDIF

SELECT (lcSGrpDt)
SET ORDER TO TAG ACCTCODE

SELECT &lcSGrpDt..*,SPACE(16) AS 'CopyStat',&lcAlAcCha..cTypeCode;
  FROM &lcSGrpDt,&lcAlAcCha,&lcTmpGridSrc;
  WHERE  &lcTmpGridSrc..cGrpCode = &lcSGrpDt..cGrpCode .AND. &lcTmpGridSrc..Inc;
  .AND. &lcSGrpDt..cAcctCode = &lcAlAcCha..cAcctCode;
  INTO DBF &lcWorkDir.&lcGrpDt

  INDEX ON cGrpCode TAG GrpCode
  SET ORDER TO GrpCode

SELECT (lcSGrpDt)
SET ORDER TO GRCODACC

SELECT (lcTmpGridSrc)
INDEX ON Inc TAG Inc
SET ORDER TO TAG Inc
SET RELATION TO cGrpCode INTO &lcGrpDt ADDITIVE

lnTotalRecs = lnTotalRecs + 1

SCAN FOR !EMPTY(Inc)
  SELECT (lcGrpHd)
  IF EOF()
    SELECT (lcTmpGridSrc)
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTmpGridSrc..CopyStat WITH 'Source deleted',;
*!*	            &lcTmpGridSrc..Inc      WITH .F.
    REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Source_Deleted,loFormSet.GetHeaderText("LANG_Source_Deleted",loFormSet.HeaderAlias)),;
            &lcTmpGridSrc..Inc      WITH .F.
    *: N000682,1 HES Globalization Changes [End  ]
  ELSE
    SELECT GLGRPHD
    *- Cheking if the copied type Already exists
    IF SEEK (&lcTmpGridSrc..cGrpCode, "GLGRPHD")
      SELECT (lcTmpGridSrc)
      lcGrpCode = "Group "+&lcTmpGridSrc..cGrpCode
      lnAction = gfModalGen("TRM02114B02015","Dialog",lcGrpCode)
      DO CASE
        CASE lnAction = 1      && Overwrite
          IF lfChkDt()
            SELECT glGrpHd
            IF gfObj_Lock(.T.)
              =gfDelete("FOR GLGRPHD.cGrpCode = &lcTmpGridSrc..cGrpCode")
              SELECT GLGRPDT
              =gfDelete("FOR GLGRPDT.cGrpCode = &lcTmpGridSrc..cGrpCode")
              SELECT (lcTmpGridSrc)
              *- Start copy selected group headers
              =lfCopyHd()
            ELSE
              SELECT (lcTmpGridSrc)
              *: N000682,1 HES Globalization Changes [Start]
*!*	              REPLACE &lcTmpGridSrc..CopyStat WITH  'Overwrite err.' ,;
*!*	                      &lcTmpGridSrc..Inc      WITH  .F.
              REPLACE &lcTmpGridSrc..CopyStat WITH  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_over_Error,loFormSet.GetHeaderText("LANG_over_Error",loFormSet.HeaderAlias)) ,;
                      &lcTmpGridSrc..Inc      WITH  .F.
              *: N000682,1 HES Globalization Changes [End  ]
            ENDIF
          ELSE
            SELECT (lcTmpGridSrc)
            *: N000682,1 HES Globalization Changes [Start]
*!*	            REPLACE &lcTmpGridSrc..CopyStat WITH 'Copy Error',;
*!*	                    &lcTmpGridSrc..Inc      WITH .F.
            REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Error,loFormSet.GetHeaderText("LANG_Copy_Error",loFormSet.HeaderAlias)),;
                    &lcTmpGridSrc..Inc      WITH .F.
            *: N000682,1 HES Globalization Changes [End  ]
          ENDIF
        CASE lnAction = 2   && Resume
          *: N000682,1 HES Globalization Changes [Start]
*!*	          REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists',;
*!*	                  &lcTmpGridSrc..Inc      WITH .F.
          REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias)),;
                  &lcTmpGridSrc..Inc      WITH .F.
          *: N000682,1 HES Globalization Changes [End  ]
        CASE lnAction = 3
          *: N000682,1 HES Globalization Changes [Start]
*!*	          REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists',;
*!*	                  &lcTmpGridSrc..Inc      WITH .F.
          REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias)),;
                  &lcTmpGridSrc..Inc      WITH .F.
          *: N000682,1 HES Globalization Changes [End  ]
          EXIT
        ENDCASE
    ELSE
      IF lfChkDt()
        =lfCopyHd()
      ELSE
        SELECT (lcTmpGridSrc)
        *: N000682,1 HES Globalization Changes [Start]
*!*	        REPLACE &lcTmpGridSrc..CopyStat WITH 'Copy Error',;
*!*	                &lcTmpGridSrc..Inc      WITH .F.
        REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Error,loFormSet.GetHeaderText("LANG_Copy_Error",loFormSet.HeaderAlias)),;
                &lcTmpGridSrc..Inc      WITH .F.
        *: N000682,1 HES Globalization Changes [End  ]
      ENDIF
    ENDIF
  ENDIF
  SELECT (lcTmpGridSrc)
ENDSCAN

SELECT (lcTmpGridSrc)
SET ORDER TO TAG CGRPCODE

IF lnRecNum <= RECCOUNT()
  GOTO lnRecNum
ENDIF


SELECT GLGRPHD
=gfTableUpdate()
SELECT GLGRPDT
=gfTableUpdate()

*** message with no of groups copied.
*WAIT ALLTRIM(STR(lnCopyTypes))+" Groups copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02237","NOWAIT",ALLTRIM(STR(lnCopyTypes)))


loFormSet.AriaForm1.cboFromCompany.Value = '*'
loFormSet.AriaForm1.cboFromCompany.Valid()

ENDFUNC
*-End of lfvCopy


*!*************************************************************
*! Name      : lfCopyHd
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to copy header record
*!*************************************************************
FUNCTION  lfCopyHd

SELECT (lcTmpGridSrc)
*- Update GLGRPHD
*- Collecting groups from temporary file to memory variables
SCATTER MEMVAR
SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
*- inserting selected groups into "GLGRPHD" file
INSERT INTO &lcDataDir.GLGRPHD FROM MEMVAR

SELECT GLGRPHD
*=gfObj_Lock(.T.)
=gfAdd_Info("GLGRPHD")
=gfReplace('')

*- Adjusting copied groups counter
lnCopyTypes = lnCopyTypes + 1

SELECT (lcGrpDt)
SCAN REST WHILE &lcGrpDt..cGrpCode = &lcTmpGridSrc..cGrpCode
  *- Update GLGRPDT
  *- Collecting groups from temporary file to memory variables
  SCATTER MEMVAR
  SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
  *- Inserting selected groups into "GLGRPDT" file
  INSERT INTO &lcDataDir.GLGRPDT FROM MEMVAR
  =gfAdd_Info("glGrpDt")
  =gfReplace('')
ENDSCAN

SELECT GLGRPHD
*=gfObj_Lock(.F.)

*- Replacing copystat with proper variable
SELECT (lcTmpGridSrc)

*: N000682,1 HES Globalization Changes [Start]
*!*	REPLACE &lcTmpGridSrc..CopyStat WITH 'Copied',;
*!*	        &lcTmpGridSrc..Inc      WITH .F.
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copied,loFormSet.GetHeaderText("LANG_Copied",loFormSet.HeaderAlias)),;
        &lcTmpGridSrc..Inc      WITH .F.
*: N000682,1 HES Globalization Changes [End  ]

ENDFUNC
*- End of lfCopyHd


*!*************************************************************
*! Name      : lfChkDt
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to check the detail record
*!*************************************************************
FUNCTION  lfChkDt

llCopyValid = .T.

SELECT (lcGrpDt)

lnDtNo=RECNO()

SCAN REST WHILE &lcGrpDt..cGrpCode = &lcTmpGridSrc..cGrpCode
  SELECT GLACCHAR
  IF !SEEK(&lcGrpDt..cAcctCode)
    SELECT (lcGrpDt)
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcGrpDt..CopyStat WITH 'Account not found'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcGrpDt..CopyStat WITH LANG_Account_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcGrpDt..CopyStat WITH LANG_Account_NotFound
REPLACE &lcGrpDt..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account_NotFound,loFormSet.GetHeaderText("LANG_Account_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
    llCopyValid=.F.
    LOOP
  ELSE
    *- Checking that the selected type range is not overlaping with any other ranges in the target company file
    SELECT GLTYPES
    IF SEEK(&lcGrpDt..cTypeCode)
      LOCATE FOR BETWEEN(VAL(&lcGrpDt..cAcctCode) , VAL(GLTYPES.cTypLacno) , VAL(GLTYPES.cTypUacno))
      *** checking that the selected type is not a primary one otherwise bypassing range checking.
      IF !FOUND()
        SELECT (lcGrpDt)
        *: N000682,1 HES Globalization Changes [Start]
*!*	        REPLACE &lcGrpDt..CopyStat WITH 'Type Mismatch'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcGrpDt..CopyStat WITH LANG_Type_Mismatch
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcGrpDt..CopyStat WITH LANG_Type_Mismatch
REPLACE &lcGrpDt..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_Mismatch,loFormSet.GetHeaderText("LANG_Type_Mismatch",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

        *: N000682,1 HES Globalization Changes [End  ]
        llCopyValid=.F.
        LOOP
      ENDIF
    ELSE
      SELECT (lcGrpDt)
      *: N000682,1 HES Globalization Changes [Start]
*!*	      REPLACE &lcGrpDt..CopyStat WITH 'Type not found'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcGrpDt..CopyStat WITH LANG_Type_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcGrpDt..CopyStat WITH LANG_Type_NotFound
REPLACE &lcGrpDt..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_NotFound,loFormSet.GetHeaderText("LANG_Type_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
      llCopyValid=.F.
      LOOP
    ENDIF
  ENDIF
  SELECT (lcGrpDt)
ENDSCAN

SELECT (lcGrpDt)

IF RECCOUNT() <> 0
  GOTO lnDtNo
ENDIF

SELECT (lcTmpGridSrc)

RETURN llCopyValid == .T.

ENDFUNC
*- End of lfChkDt


*!*************************************************************
*! Name      : lfvView
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to open GLDETAL screen with Budget Detail
*!*************************************************************
FUNCTION  lfvView
LPARAMETERS loFormSet

PRIVATE lc_ViewDt, lcSGrpDt, lcAlAcCha, lcWorkDir
lc_ViewDt = loFormSet.lc_ViewDt
lcSGrpDt  = loFormSet.lcSGrpDt
lcAlAcCha = loFormSet.lcAlAcCha
lcWorkDir = oAriaApplication.WorkDir

SELECT (lcSGrpDt)

PRIVATE lcGrpCode
lcGrpCode = &lcTmpGridSrc..CGRPCODE

