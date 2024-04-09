*****************************************************************************
*: Program file        : GLCPBUD.PRG
*: Program desc.       : Copy Budget Entry
*: Module              : General Ledger (GL)
*: System		       : Aria Apparel System (A4xp)
*: Developer	       : Saber Saber [SAB]
*: Tracking Job Number : E303272.EXE
*: Date                : 10/09/2012
*****************************************************************************
*Modifications :
*****************************************************************************
*: N000682,1 HES Globalization Changes [Globalization Project]

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

PRIVATE lcTmpGridSrc, lcAutDt, lcAutHd, lcAlTypes, lcAlAcHar, lcTAutDt
STORE '' TO lcTmpGridSrc, lcAutDt, lcAutHd, lcAlTypes, lcAlAcHar

*- Create the Name for the temporary file
lc_ViewDt    = gfTempName()  && Temp file name for view budget details
lcTmpGridSrc = gfTempName()  && Temp file name for budget header
lc_SelBuDt   = gfTempName()  && Temp file name for budget details
lcAlBudDt    = gfTempName()  && Alias name for budget details file
lcAlAcCha    = gfTempName()  && Alias name for accounts chart file
lcAlBudHd    = gfTempName()  && Alias name for budget header  file

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
=lfAddProp(loFormSet,'lc_SelBuDt', lc_SelBuDt)
=lfAddProp(loFormSet,'lc_ViewDt', lc_ViewDt)
=lfAddProp(loFormSet,'lcAlBudHd', lcAlBudHd)
=lfAddProp(loFormSet,'lcAlBudDt', lcAlBudDt)
=lfAddProp(loFormSet,'lcAlAcCha', lcAlAcCha)

=lfAddProp(loFormSet,'lc_BudCode', lc_BudCode)
=lfAddProp(loFormSet,'lc_BudYear', lc_BudYear)

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
  .ColumnCount = 5
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpGridSrc
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.AriaForm1.grdCopyRecords

  *Inc, cBudCode, cBudYear, SUBSTR(cBudDes,1,31), CopyStat
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..Inc", '')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cBudCode", LANG_GLCPBUD_Budget_Code, 70)                &&"Budget Code"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cBudCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_Budget_Code,loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Code",loFormSet.HeaderAlias)
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cBudCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_Budget_Code,loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Code",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Code",loFormSet.HeaderAlias)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cBudYear", LANG_GLCPBUD_Budget_Year, 40)                &&"Budget Year"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cBudYear", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_Budget_Year,loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Year",loFormSet.HeaderAlias)
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cBudYear", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_Budget_Year,loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Year",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Year",loFormSet.HeaderAlias)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cBudDes, 1, 31)", LANG_GLCPBUD_Budget_Desc, 250) &&"Automatic Description"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cBudDes, 1, 31)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_Budget_Desc,loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Desc",loFormSe
=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cBudDes, 1, 31)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_Budget_Desc,loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Desc",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPBUD_Budget_Desc",loFormSe
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,LANG_GLCPBUD_CopyStat, 150)               &&"Copy Status"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_CopyStat,loFormSet.GetHeaderText("LANG_GLCPBUD_CopyStat",loFormSet.HeaderAlias)),
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_CopyStat,loFormSet.GetHeaderText("LANG_GLCPBUD_CopyStat",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPBUD_CopyStat",loFormSet.HeaderAlias))
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

=gfCloseTable('GLBUDHD')
=gfCloseTable('GLBUDDT')
=gfCloseTable('GLSEGVAL')
=gfCloseTable('GLTYPES')
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
=gfOpenTable('GLBUDHD','BDYRCOD','SH')      && CBUDYEAR+CBUDCODE
=gfOpenTable('GLBUDDT','CDYRPRACC','SH')    && CBUDCODE+CBUDYEAR+CBUDPRD+CACCTCODE
=gfOpenTable('GLSEGVAL','ACSSEGVAL','SH')   && CACSSEGNO+CSEGVALUE
=gfOpenTable('GLTYPES','TYPECODE','SH')     && CTYPECODE
=gfOpenTable('ACCOD','ACCSEGNO','SH')       && ALLTRIM(STR(NACSSEGNO))
=gfOpenTable('GLACCHAR','ACCTCODE','SH')    && CACCTCODE
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYDFIELD','CFLD_NAME','SH')   && CFLD_NAME

*- Create empty temp file (lcTmpGridSrc) with the same structure of GLBudHd + Inc, CopyStat.
lcTmpGridSrc = gfTempName()
SELECT GLBUDHD
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]

laFileStru[lnFileStru+1,1] = 'Inc'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'CopyStat'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 17
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
INDEX ON  CBUDCODE+CBUDYEAR TAG CODEYEAR
SET ORDER TO TAG CODEYEAR

*- Create empty temp file (lc_ViewDt) with the same structure of GLBUDDT + account description.
SELECT GLBUDDT

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
lc_ViewDt  = loFormSet.lc_ViewDt
lcAlBudHd  = loFormSet.lcAlBudHd
lcAlBudDt  = loFormSet.lcAlBudDt
lcAlAcCha  = loFormSet.lcAlAcCha
lcComPath  = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))


*- Select (lcTmpGridSrc) and removes all records. and refresh the list window
SELECT (lcTmpGridSrc)
ZAP
*- Select (lc_ViewDt) and removes all records. and refresh the list window
SELECT (lc_ViewDt)
ZAP

IF USED(lcAlBudHd)
  SELECT (lcAlBudHd)
ELSE
  SELECT 0
ENDIF
*- use GLBudHd from the source company in the path lcComPath and set index to order tag BDCODYR
USE &lcComPath.GLBudHd AGAIN ALIAS &lcAlBudHd ORDER TAG BDCODYR

*- check if this there no budgets in budget header file if empty ----> display message
*!                "There are no budgets to copy from this company."
GO TOP
IF EOF()
  USE
  SELECT (lcTmpGridSrc)
  =gfModalGen("TRM02119B00000","Dialog","bugdets")
  RETURN .F.
ENDIF

*- case there is at least one budget in the header file in this case select all budgets from source company
*  into lcTmpGridSrc and set relation between lcTmpGridSrc and master header file (lcAlBudHd) to check if every
*  budget still exist in the master file.
SELECT (lcAlBudHd)
SCAN
  SCATTER MEMO MEMVAR
  m.Inc      = .F.
  m.CopyStat = SPACE(15)
  SELECT (lcTmpGridSrc)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN

SELECT(lcTmpGridSrc)
SET RELATION TO &lcTmpGridSrc..cBudCode+&lcTmpGridSrc..cBudYear INTO &lcAlBudHd

IF USED(lcAlBudDt)
  SELECT (lcAlBudDt)
ELSE
  SELECT 0
ENDIF
*- use GLBudDt from the source company in the path lcComPath and set index to order tag CDYRPRACC
USE &lcComPath.GLBudDt AGAIN ALIAS &lcAlBudDt ORDER TAG CDYRPRACC

IF USED(lcAlAcCha)
  SELECT (lcAlAcCha)
ELSE
  SELECT 0
ENDIF
*** use GLAccHar from the source company in the path lcComPath
USE &lcComPath.GLAccHar AGAIN ALIAS &lcAlAcCha

SELECT ACCOD
GO TOP
IF !EOF()
  lcAccSeg = cAcSegDes
ELSE
  lcAccSeg = ""
ENDIF

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

PRIVATE lcTmpGridSrc, lc_SelBuDt, lc_ViewDt, lcAlBudHd, lcAlBudDt, lcAlAcCha, lcComPath, lcWorkDir, lcDataDir
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lc_SelBuDt   = loFormSet.lc_SelBuDt
lc_ViewDt    = loFormSet.lc_ViewDt
lcAlBudHd    = loFormSet.lcAlBudHd
lcAlBudDt    = loFormSet.lcAlBudDt
lcAlAcCha    = loFormSet.lcAlAcCha
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))
lcWorkDir    = oAriaApplication.WorkDir
lcDataDir    = oAriaApplication.DataDir

*- Start copy selected budgets
SELECT (lcTmpGridSrc)
lnCurRec   = 0          && init. the current budget variable.
lnReCopied = 0          && init. the no of copied records.
lnHdRecNo  = RECNO(lcTmpGridSrc)

*- Count the number of included budgets to the variable lnTotRecs
COUNT FOR Inc TO lnTotRecs

*- check if there is any selected budgets to copyied case there is no selected budgets to copyied
IF lnTotRecs = 0
  *** Message "No included budgets to copy."
  ***              < Ok >
  =gfModalGen("TRM02121B00000","DIALOG")
  SELECT (lcTmpGridSrc)
  LOCATE
  IF lnHdRecNo <= RECCOUNT()
    GOTO lnHdRecNo
  ENDIF

  RETURN .F.
ENDIF

*- Select all budget details information and the type for every account from source company into lc_SelBuDt
lnTotRecs  = lnTotRecs + 1
lnSelected = 0
lnTotalRec = RECCOUNT(lcAlBudDt)

SET STEP ON
SELECT &lcAlBudDt..*,SPACE(20) AS 'DetalStat',&lcAlAcCha..cTypeCode;
  FROM &lcAlBudDt,&lcTmpGridSrc,&lcAlAcCha                ;
  WHERE  (&lcTmpGridSrc..Inc)                       ;
  .AND.  &lcTmpGridSrc..CBUDCODE  = &lcAlBudDt..CBUDCODE  ;
  .AND.  &lcTmpGridSrc..CBUDYEAR  = &lcAlBudDt..CBUDYEAR  ;
  .AND.  &lcAlBudDt..cAcctCode  = &lcAlAcCha..cAcctCode ;
  INTO DBF &lcWorkDir.&lc_SelBuDt

SELECT (lcTmpGridSrc)
SCAN FOR Inc
  lnHdRecNo = RECNO(lcTmpGridSrc)   && store CP --> lcTmpGridSrc
  lnCurRec = lnCurRec + 1

  SELECT (lcAlBudHd)
  && check if this budget deleted.... replace lcTmpGridSrc..CopyStat WITH 'Source deleted'
  IF EOF()
    SELECT (lcTmpGridSrc)
    IF lnHdRecNo <= RECCOUNT()
      GOTO lnHdRecNo
    ENDIF
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTmpGridSrc..CopyStat WITH 'Source deleted'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Source_Deleted
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Source_Deleted
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Source_Deleted,loFormSet.GetHeaderText("LANG_Source_Deleted",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
    REPLACE &lcTmpGridSrc..Inc WITH .F.
  ELSE
    SELECT GLBUDHD
    SET ORDER TO TAG BDCODYR
    *- check if this budget exist in the source company display message
    IF SEEK (&lcTmpGridSrc..cBudCode+&lcTmpGridSrc..cBudYear)
      *** budget Already exists do you want to overwrite
      *** < Overwrite >  <Resume>  <Cancel>
      *: N000682,1 HES Globalization Changes [Start]
*!*	      lnAction=gfModalGen("TRM02114B02015","Dialog", "Budget"+' ' +ALLTRIM(&lcTmpGridSrc..cBudCode)+ '\' +ALLTRIM(&lcTmpGridSrc..cBudYear) + ' ')
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnAction=gfModalGen("TRM02114B02015","Dialog", LANG_Budget+' ' +ALLTRIM(&lcTmpGridSrc..cBudCode)+ '\' +ALLTRIM(&lcTmpGridSrc..cBudYear) + ' ')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnAction=gfModalGen("TRM02114B02015","Dialog", LANG_Budget+' ' +ALLTRIM(&lcTmpGridSrc..cBudCode)+ '\' +ALLTRIM(&lcTmpGridSrc..cBudYear) + ' ')
lnAction=gfModalGen("TRM02114B02015","Dialog", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Budget,loFormSet.GetHeaderText("LANG_Budget",loFormSet.HeaderAlias))+' ' +ALLTRIM(&lcTmpGridSrc..cBudCode)+ '\' +ALLTRIM(&lcTmpGridSrc..cBudYear) + ' ')
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
      DO CASE
        CASE lnAction = 1   &&  Overwrite this budget
          *- check details for this budget(if valid to copy ..)
          IF lfChkDt()
            SELECT GLBUDHD
            IF gfObj_Lock(.T.)
              *- delete old budget form active company (Budget header + Details)
              =gfDelete("FOR GLBUDHD.cBudCode + GLBUDHD.cBudYear = &lcTmpGridSrc..cBudCode + &lcTmpGridSrc..cBudYear")
              SELECT GLBUDDT
              =gfDelete("FOR GLBUDHD.cBudCode + GLBUDHD.cBudYear = &lcTmpGridSrc..cBudCode + &lcTmpGridSrc..cBudYear")
              *- Copy cureent budget header and detail to the actice copmany..
              =lfCopyHdDt()

              SELECT GLBUDHD
              =gfObj_Lock(.F.)
            ELSE
              SELECT (lcTmpGridSrc)
              IF lnHdRecNo <= RECCOUNT()
                GOTO lnHdRecNo
              ENDIF
              *: N000682,1 HES Globalization Changes [Start]
*!*	              REPLACE &lcTmpGridSrc..CopyStat WITH 'Overwrite err.',;
*!*	                      &lcTmpGridSrc..Inc      WITH .F.
              REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_over_Error,loFormSet.GetHeaderText("LANG_over_Error",loFormSet.HeaderAlias)),;
                      &lcTmpGridSrc..Inc      WITH .F.
              *: N000682,1 HES Globalization Changes [End  ]
            ENDIF
          ELSE
            *- If this budget not valid to copy to active company.
            SELECT (lcTmpGridSrc)
            IF lnHdRecNo <= RECCOUNT()
              GOTO lnHdRecNo
            ENDIF
            *- Repalce copy status with 'Copy Error'
            *: N000682,1 HES Globalization Changes [Start]
*!*	            REPLACE &lcTmpGridSrc..CopyStat WITH 'Copy Error' ,;
*!*	                    &lcTmpGridSrc..Inc      WITH .F.
            REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Error,loFormSet.GetHeaderText("LANG_Copy_Error",loFormSet.HeaderAlias)) ,;
                    &lcTmpGridSrc..Inc      WITH .F.
            *: N000682,1 HES Globalization Changes [End  ]
          ENDIF

        CASE lnAction = 2      &&  <Resume>
          *- In this case repalce copy status with 'Already exists'
          SELECT (lcTmpGridSrc)
          IF lnHdRecNo <= RECCOUNT()
            GOTO lnHdRecNo
          ENDIF
          *: N000682,1 HES Globalization Changes [Start]
*!*	          REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists' ,;
*!*	                  &lcTmpGridSrc..Inc      WITH .F.
          REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias)) ,;
                  &lcTmpGridSrc..Inc      WITH .F.
          *: N000682,1 HES Globalization Changes [End  ]

        CASE lnAction = 3      &&  <Cancel>
          *- In this case repalce copy status with 'Already exists' and terminat copy procedure.
          SELECT (lcTmpGridSrc)
          IF lnHdRecNo <= RECCOUNT()
            GOTO lnHdRecNo
          ENDIF

          *: N000682,1 HES Globalization Changes [Start]
*!*	          REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists' ,;
*!*	                  &lcTmpGridSrc..Inc      WITH .F.
          REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias)) ,;
                  &lcTmpGridSrc..Inc      WITH .F.
          *: N000682,1 HES Globalization Changes [End  ]
          EXIT

       ENDCASE

    ELSE
      *- case active budget not exist in active company in this case check budget details ..
      *  and if return .T. then copy budget header and details to the active company.
      IF lfChkDt()
        =lfCopyHdDt()
      ELSE
        SELECT (lcTmpGridSrc)
        IF lnHdRecNo <= RECCOUNT()
          GOTO lnHdRecNo
        ENDIF
        *- repalce copy status with 'Copy Error'
        *: N000682,1 HES Globalization Changes [Start]
*!*	        REPLACE &lcTmpGridSrc..CopyStat WITH 'Copy Error' ,;
*!*	                &lcTmpGridSrc..Inc      WITH .F.
        REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Error,loFormSet.GetHeaderText("LANG_Copy_Error",loFormSet.HeaderAlias)) ,;
                &lcTmpGridSrc..Inc      WITH .F.
        *: N000682,1 HES Globalization Changes [End  ]
      ENDIF
    ENDIF
  ENDIF

  SELECT (lcTmpGridSrc)
  IF lnHdRecNo <= RECCOUNT()
    GOTO lnHdRecNo
  ENDIF

ENDSCAN

SELECT GLBUDHD
=gfTableUpdate()
SELECT GLBUDDT
=gfTableUpdate()

*WAIT ALLTRIM(STR(lnReCopied))+" Budgets copied from source company..." ;
   WINDOW NOWAIT
=gfWait("02236","NOWAIT",ALLTRIM(STR(lnReCopied)))


loFormSet.AriaForm1.cboFromCompany.Value = '*'
loFormSet.AriaForm1.cboFromCompany.Valid()

ENDFUNC
*-End of lfvCopy


*!*************************************************************
*! Name      : lfCopyHdDt
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to copy header record
*!*************************************************************
FUNCTION  lfCopyHdDt

*- Copy budget header and details to target company.
SELECT (lcTmpGridSrc)
SCATTER MEMVAR
SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
INSERT INTO &lcDataDir.GLBUDHD FROM MEMVAR
SELECT GLBUDHD
*=gfObj_Lock(.T.)
=gfAdd_Info("glBudHd")
=gfReplace('')

SELECT (lc_SelBuDt)
SCAN FOR &lc_SelBuDt..cBudCode+&lc_SelBuDt..cBudYear = &lcTmpGridSrc..cBudCode+&lcTmpGridSrc..cBudYear
  SCATTER MEMVAR
  SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
  INSERT INTO &lcDataDir.GLBUDDT FROM MEMVAR
  =gfAdd_Info("glBudDt")
  SELECT (lc_SelBuDt)
  =gfReplace('')
ENDSCAN

lnReCopied = lnReCopied + 1   && increase number of copyied budgets
SELECT  (lcTmpGridSrc)
*: N000682,1 HES Globalization Changes [Start]
*!*	REPLACE &lcTmpGridSrc..CopyStat WITH 'Copied' ,;
*!*	        &lcTmpGridSrc..Inc      WITH .F.
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copied,loFormSet.GetHeaderText("LANG_Copied",loFormSet.HeaderAlias)) ,;
        &lcTmpGridSrc..Inc      WITH .F.
*: N000682,1 HES Globalization Changes [End  ]
SELECT GLBUDHD
*=gfObj_Lock(.F.)

ENDFUNC
*- End of lfCopyHdDt


*!*************************************************************
*! Name      : lfChkDt
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to check the detail record
*!*************************************************************
FUNCTION  lfChkDt

*- Check every account belong to active budget..
llValdCopy = .T.
SELECT (lc_SelBuDt)

SCAN FOR &lc_SelBuDt..cBudCode+&lc_SelBuDt..cBudYear+&lc_SelBuDt..cBudPrd = &lcTmpGridSrc..cBudCode+&lcTmpGridSrc..cBudYear+'01'
  SELECT GLACCHAR
  *- check if this account exist in the target company
  IF !SEEK(&lc_SelBuDt..cAcctCode)
    SELECT (lc_SelBuDt)
    *-  repalce detail status with 'Account not found'
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lc_SelBuDt..DetalStat WITH 'Account not found'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lc_SelBuDt..DetalStat WITH LANG_Account_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lc_SelBuDt..DetalStat WITH LANG_Account_NotFound
REPLACE &lc_SelBuDt..DetalStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account_NotFound,loFormSet.GetHeaderText("LANG_Account_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
    llValdCopy = .F.
    LOOP

  ELSE
    SELECT GLTYPES
    *- Check if the type for active account exist in the target company (GLTYPES)
    IF SEEK(&lc_SelBuDt..cTypeCode)
      *- check if this type between the values of lower limit (GLTYPES.cTyplacno) and upper limit (GLTYPES.cTypuacno)
      LOCATE FOR BETWEEN( VAL(&lc_SelBuDt..cAcctCode) , VAL(glTypes.cTypLacno) , VAL(glTypes.cTypUacno) )

      IF ! FOUND()
        SELECT (lc_SelBuDt)
        *- case the type is out of range ... repalce detail status with 'Out of range'
        *: N000682,1 HES Globalization Changes [Start]
*!*	        REPLACE &lc_SelBuDt..DetalStat WITH 'Out of range'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lc_SelBuDt..DetalStat WITH LANG_Out_ofrange
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lc_SelBuDt..DetalStat WITH LANG_Out_ofrange
REPLACE &lc_SelBuDt..DetalStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Out_ofrange,loFormSet.GetHeaderText("LANG_Out_ofrange",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

        *: N000682,1 HES Globalization Changes [End  ]
        llValdCopy = .F.
        LOOP
      ENDIF

    ELSE
      SELECT (lc_SelBuDt)
      *- Repalce detail status with 'Type not found
      *: N000682,1 HES Globalization Changes [Start]
*!*	      REPLACE &lc_SelBuDt..DetalStat WITH 'Type not found'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lc_SelBuDt..DetalStat WITH LANG_Type_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lc_SelBuDt..DetalStat WITH LANG_Type_NotFound
REPLACE &lc_SelBuDt..DetalStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_NotFound,loFormSet.GetHeaderText("LANG_Type_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
      llValdCopy = .F.
      LOOP

    ENDIF
  ENDIF
  SELECT (lc_SelBuDt)
ENDSCAN

IF llValdCopy
  Return .T.    && This budget details valid to copy
ELSE
  Return .F.    && This budget details not valid to copy
ENDIF

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

PRIVATE lc_ViewDt, lcAlBudDt, lcAlAcCha, lcWorkDir
lc_ViewDt = loFormSet.lc_ViewDt
lcAlBudDt = loFormSet.lcAlBudDt
lcAlAcCha = loFormSet.lcAlAcCha
lcWorkDir = oAriaApplication.WorkDir

SELECT (lcAlBudDt)

PRIVATE lc_BudCode, lc_BudYear
lc_BudCode = &lcTmpGridSrc..CBUDCODE
lc_BudYear = &lcTmpGridSrc..CBUDYEAR

*- select account code and description from source company where budget code = lc_BudCode budget year = lc_BudYear(active budget)
SELECT DISTINCT &lcAlBudDt..cAcctCode,&lcAlAcCha..cAccnlDes;
   FROM &lcAlBudDt,&lcAlAcCha;
   INTO DBF &lcWorkDir.&lc_ViewDt ;
   WHERE &lcAlBudDt..CBUDCODE  = lc_BudCode;
   .AND. &lcAlBudDt..CBUDYEAR  = lc_BudYear;
   .AND. &lcAlBudDt..cAcctCode = &lcAlAcCha..cAcctCode

PRIVATE lcFormTitle, lcGridSource, laGridColumns
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFormTitle  = LANG_GLCPBUD_GLDETAL_Title
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFormTitle  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_GLDETAL_Title,loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Title",loFormSet.HeaderAlias))
lcFormTitle  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_GLDETAL_Title,loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Title",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Title",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

lcGridSource = lc_ViewDt
DIMENSION laGridColumns[2, 3]
laGridColumns[1, 1] = "cAcctCode"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laGridColumns[1, 2] = LANG_GLCPBUD_GLDETAL_Code
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laGridColumns[1, 2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_GLDETAL_Code,loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Code",loFormSet.HeaderAlias))
laGridColumns[1, 2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_GLDETAL_Code,loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Code",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Code",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

laGridColumns[1, 3] = 50

laGridColumns[2, 1] = "cAccnlDes"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laGridColumns[2, 2] = LANG_GLCPBUD_GLDETAL_Desc
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laGridColumns[2, 2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_GLDETAL_Desc,loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Desc",loFormSet.HeaderAlias))
laGridColumns[2, 2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPBUD_GLDETAL_Desc,loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Desc",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPBUD_GLDETAL_Desc",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

laGridColumns[2, 3] = 300

DO FORM (oAriaApplication.ScreenHome+"\GL\GLDETAL.SCX")

SELECT (lc_ViewDt)

ENDFUNC
*- End of lfvView
