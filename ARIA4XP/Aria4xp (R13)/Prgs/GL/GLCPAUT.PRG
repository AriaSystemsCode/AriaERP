*****************************************************************************
*: Program file        : GLCPAUT.SCX
*: Program desc.       : Copy Automatic Entry
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

PRIVATE lcTmpGridSrc, lcAutDt, lcAutHd, lcAlTypes, lcAlAcHar, lcTAutDt
STORE '' TO lcTmpGridSrc, lcAutDt, lcAutHd, lcAlTypes, lcAlAcHar

lcAutDt   = gfTempName()
lcAutHd   = gfTempName()
lcAlTypes = gfTempName()
lcAlAcHar = gfTempName()
lcTAutDt  = gfTempName()

*- Open used files and Create temp files
=lfPrepFls()

*- Check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE
IF EOF()
  * The chart of accounts is empty. You have to create
  * the chart of accounts first.
  * < Ok >
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
=lfAddProp(loFormSet,'lcTmpGridSrc', lcTmpGridSrc)
=lfAddProp(loFormSet,'lcAutDt', lcAutDt)
=lfAddProp(loFormSet,'lcAutHd', lcAutHd)
=lfAddProp(loFormSet,'lcAlTypes', lcAlTypes)
=lfAddProp(loFormSet,'lcAlAcHar', lcAlAcHar)
=lfAddProp(loFormSet,'lcTAutDt', lcTAutDt)
=lfAddProp(loFormSet,'lnAlloc', 0)
=lfAddProp(loFormSet,'lnTempl', 0)
=lfAddProp(loFormSet,'lnDistr', 0)
=lfAddProp(loFormSet,'lnRecur', 0)


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
*loFormSet.AriaForm1.cmdCopyOption.Caption = LANG_Select_Auto_Types
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdCopyOption.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Auto_Types,loFormSet.GetHeaderText("LANG_Select_Auto_Types",loFormSet.HeaderAlias))
loFormSet.AriaForm1.cmdCopyOption.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Auto_Types,loFormSet.GetHeaderText("LANG_Select_Auto_Types",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_Select_Auto_Types",loFormSet.HeaderAlias))
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

  *Inc, cAutType, cAutCode, SUBSTR(cAutDes,1,29), CopyStat
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..Inc", '')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAutType", LANG_GLCPAUT_AutType, 40)                   &&"Automatic Type"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAutType", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_AutType,loFormSet.GetHeaderText("LANG_GLCPAUT_AutType",loFormSet.HeaderAlias)), 40)
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAutType", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_AutType,loFormSet.GetHeaderText("LANG_GLCPAUT_AutType",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPAUT_AutType",loFormSet.HeaderAlias)), 40)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAutCode", LANG_GLCPAUT_AutCode, 70)                   &&"Automatic Code"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAutCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_AutCode,loFormSet.GetHeaderText("LANG_GLCPAUT_AutCode",loFormSet.HeaderAlias)), 70)
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cAutCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_AutCode,loFormSet.GetHeaderText("LANG_GLCPAUT_AutCode",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPAUT_AutCode",loFormSet.HeaderAlias)), 70)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAutDes, 1, 29)", LANG_GLCPAUT_AutDesc, 250)    &&"Automatic Description"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAutDes, 1, 29)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_AutDesc,loFormSet.GetHeaderText("LANG_GLCPAUT_AutDesc",loFormSet.Header
=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAutDes, 1, 29)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_AutDesc,loFormSet.GetHeaderText("LANG_GLCPAUT_AutDesc",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPAUT_AutDesc",loFormSet.Header
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,LANG_GLCPAUT_CopyStat, 150)              &&"Copy Status"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_CopyStat,loFormSet.GetHeaderText("LANG_GLCPAUT_CopyStat",loFormSet.HeaderAlias)),
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPAUT_CopyStat,loFormSet.GetHeaderText("LANG_GLCPAUT_CopyStat",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPAUT_CopyStat",loFormSet.HeaderAlias))
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

=gfCloseTable('GLAUTHD')
=gfCloseTable('GLAUTDT')
=gfCloseTable('GLACCHAR')
=gfCloseTable('GLTYPES')
=gfCloseTable('ACCOD')
=gfCloseTable('SYCCOMP')
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
=gfOpenTable('GLAUTHD','TYPECODE','SH')    && CAUTTYPE+CAUTCODE
=gfOpenTable('GLAUTDT','TYPCODACC','SH')   && CAUTTYPE+CAUTCODE+CACCTCODE
=gfOpenTable('GLACCHAR','ACCTCODE','SH')   && CACCTCODE
=gfOpenTable('GLTYPES','TYPECODE','SH')    && CTYPECODE
=gfOpenTable('ACCOD','ACCSEGNO','SH')      && ALLTRIM(STR(NACSSEGNO))
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYCCOMP','CCOMP_ID','SH')     && CCOMP_ID
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYDFIELD','CFLD_NAME','SH')   && CFLD_NAME

*- Create empty temp file (lcTmpGridSrc) with the same structure of GLTYPES + Inc, CopyStat,cStatus.
lcTmpGridSrc = gfTempName()
SELECT GLAUTHD
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

LOCAL lcAutHd, lcAutDt, lcAlTypes, lcAlAcHar, lcComPath
lcAutHd   = loFormSet.lcAutHd
lcAutDt   = loFormSet.lcAutDt
lcAlTypes = loFormSet.lcAlTypes
lcAlAcHar = loFormSet.lcAlAcHar
lcComPath = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))

IF USED(lcAlTypes)
  SELECT (lcAlTypes)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.GLTypes  AGAIN ALIAS  &lcAlTypes ORDER TAG TYPECODE

IF USED(lcAutDt)
  SELECT (lcAutDt)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.GLAutDt AGAIN ALIAS &lcAutDt ORDER TYPCODACC

IF USED(lcAlAcHar)
  SELECT (lcAlAcHar)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.glAcchar AGAIN ALIAS &lcAlAcHar ORDER TAG ACCTCODE

IF USED(lcAutHd)
  SELECT (lcAutHd)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.GLAutHd AGAIN ALIAS &lcAutHd ORDER TYPECODE

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

loFormSet.lnAlloc = 0
loFormSet.lnTempl = 0
loFormSet.lnDistr = 0
loFormSet.lnRecur = 0

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

LOCAL lcSelAutTyp, lcOldSelAutTyp
lcOldSelAutTyp = IIF(loFormSet.lnAlloc = 1, 'A', '') + IIF(loFormSet.lnTempl = 1, 'T', '') + ;
                 IIF(loFormSet.lnDistr = 1, 'D', '') + IIF(loFormSet.lnRecur = 1, 'R', '')

DO FORM (oAriaApplication.ScreenHome+"\GL\GLCHTYP.SCX") WITH loFormSet

lcSelAutTyp = IIF(loFormSet.lnAlloc = 1, 'A', '') + IIF(loFormSet.lnTempl = 1, 'T', '') + ;
              IIF(loFormSet.lnDistr = 1, 'D', '') + IIF(loFormSet.lnRecur = 1, 'R', '')

IF !(lcSelAutTyp == lcOldSelAutTyp)
  SELECT (loFormSet.lcTmpGridSrc)
  ZAP

  SELECT (loFormSet.lcAutHd)
  SCAN FOR CAUTTYPE $ lcSelAutTyp
    SCATTER MEMO MEMVAR
    SELECT (loFormSet.lcTmpGridSrc)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDSCAN

  SELECT (loFormSet.lcTmpGridSrc)
  LOCATE
  loFormSet.AriaForm1.grdCopyRecords.Refresh()
ENDIF

ENDFUNC


*!*************************************************************
*! Name      : lfvCopy
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Copy selected records from the grid to active Company
*!*************************************************************
FUNCTION  lfvCopy
LPARAMETERS loFormSet

PRIVATE lcTmpGridSrc, lcAutHd, lcAutDt, lcAlTypes, lcAlAcHar, lcTAutDt, lcComPath, lcWorkDir, lcDataDir
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lcAutHd      = loFormSet.lcAutHd
lcAutDt      = loFormSet.lcAutDt
lcAlTypes    = loFormSet.lcAlTypes
lcAlAcHar    = loFormSet.lcAlAcHar
lcTAutDt     = loFormSet.lcTAutDt
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))
lcWorkDir    = oAriaApplication.WorkDir
lcDataDir    = oAriaApplication.DataDir

lnTotalRecs = 0
SELECT (lcTmpGridSrc)
lnRecNum = RECNO()

*- Count the number of included groups to the variable lnTotRecs
COUNT FOR !EMPTY(Inc) TO lnTotalRecs

IF RECNO() <= RECCOUNT()
  GO TOP
ENDIF

*** initialize no. of copied groups , current type variable.
lnCopyTypes = 0               && init. the no of copied groups.
lnCurRec    = 0               && init. the current type variable.
lnSelected  = 0
lnAction    = 0

*** check if there is any selected groups to copied
IF lnTotalRecs = 0
  *** case there is no selected groups to copyied
  *** Message "No automatic entry to copy."
  ***      < Ok >
  =gfModalGen("TRM02144B00000","DIALOG")
  RETURN
ENDIF

SET STEP ON
SELECT &lcAutDt..*, SPACE(17) AS 'CopyStat', &lcAlAchar..cTypeCode;
  FROM &lcAutDt, &lcAlAchar, &lcTmpGridSrc;
  WHERE &lcTmpGridSrc..cAutType+&lcTmpGridSrc..cAutCode = &lcAutDt..cAutType+&lcAutDt..cAutCode .AND. &lcTmpGridSrc..Inc;
  .AND. &lcAutDt..cAcctCode = &lcAlAchar..cAcctCode;
  INTO DBF &lcWorkDir.&lcTAutDt

  INDEX ON cAutType+cAutCode TAG TypCode
  SET ORDER TO TypCode

SELECT (lcTmpGridSrc)
SET RELATION TO cAutType+cAutCode INTO &lcTAutDt ADDITIVE
SET RELATION TO cAutType+cAutCode INTO &lcAutHd ADDITIVE

lnTotalRecs = lnTotalRecs + 1

SELECT (lcTmpGridSrc)

ln_Curr = RECNO()

SCAN FOR (&lcTmpGridSrc..Inc)
  SELECT (lcAutHd)
  IF EOF()
    SELECT (lcTmpGridSrc)
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTmpGridSrc..CopyStat WITH 'Source deleted',
     REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Source_Deleted,loFormSet.GetHeaderText("LANG_Source_Deleted",loFormSet.HeaderAlias)),;
    *: N000682,1 HES Globalization Changes [End  ]
    &lcTmpGridSrc..Inc WITH .F.
  ELSE
    SELECT GLAUTHD
    *** cheking if the copied type Already exists
    IF SEEK (&lcTmpGridSrc..cAutType+&lcTmpGridSrc..cAutCode,"GLAUTHD")
      SELECT (lcTmpGridSrc)
      *: N000682,1 HES Globalization Changes [Start]
*!*	      lcEntType = IIF(cAutType == 'A','Allocation', IIF(cAutType == 'D', 'Distribution', IIF(cAutType = 'R' , 'Recurring', 'Template')))
*!*	      lcAutCode = "Automatic entry of Type " + lcEntType+ " and code " + ALLTRIM(&lcTmpGridSrc..cAutCode)+ " "
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcEntType = IIF(cAutType == 'A',LANG_Allocation, IIF(cAutType == 'D', LANG_Distribution, IIF(cAutType = 'R' , LANG_Recurring, LANG_Template)))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcEntType = IIF(cAutType == 'A',LANG_Allocation, IIF(cAutType == 'D', LANG_Distribution, IIF(cAutType = 'R' , LANG_Recurring, LANG_Template)))
lcEntType = IIF(cAutType == 'A',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Allocation,loFormSet.GetHeaderText("LANG_Allocation",loFormSet.HeaderAlias)), IIF(cAutType == 'D', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Distribution,loFormSet.GetHeaderText("LANG_Distribution",loFormSet.HeaderAlias)), IIF(cAutType = 'R' , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Recurring,loFormSet.GetHeaderText("LANG_Recurring",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Template,loFormSet.GetHeaderText("LANG_Template",loFormSet.HeaderAlias)))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcAutCode = LANG_Aut_Entry + lcEntType+ LANG_And_Code + ALLTRIM(&lcTmpGridSrc..cAutCode)+ " "
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcAutCode = LANG_Aut_Entry + lcEntType+ LANG_And_Code + ALLTRIM(&lcTmpGridSrc..cAutCode)+ " "
lcAutCode = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Aut_Entry,loFormSet.GetHeaderText("LANG_Aut_Entry",loFormSet.HeaderAlias)) + lcEntType+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_And_Code,loFormSet.GetHeaderText("LANG_And_Code",loFormSet.HeaderAlias)) + ALLTRIM(&lcTmpGridSrc..cAutCode)+ " "
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
      lnAction  = gfModalGen("TRM02114B02015","Dialog", lcAutCode)
      DO CASE
        CASE lnAction = 1                && OverWrite
          SET STEP ON
          IF lfChkDt()
            SELECT GLAUTHD
            IF gfObj_Lock(.T.)
              =gfDelete("FOR GLAUTHD.cAutType+GLAUTHD.cAutCode = &lcTmpGridSrc..cAutType+&lcTmpGridSrc..cAutCode")
              SELECT GLAUTDT
              =gfDelete("FOR GLAUTDT.cAutType+GLAUTDT.cAutCode = &lcTmpGridSrc..cAutType+&lcTmpGridSrc..cAutCode")

              SELECT (lcTmpGridSrc)
              IF RECNO() <= RECCOUNT()
                GOTO RECNO()
              ENDIF
              *** start copy selected group headers
              =lfCopyHd()
              SELECT GLAUTHD
              =gfObj_Lock(.F.)
            ELSE
              SELECT (lcTmpGridSrc)
              *: N000682,1 HES Globalization Changes [Start]
*!*	              REPLACE &lcTmpGridSrc..CopyStat WITH 'Overwrite err.'
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_over_Error
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_over_Error
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_over_Error,loFormSet.GetHeaderText("LANG_over_Error",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

              *: N000682,1 HES Globalization Changes [End  ]
            ENDIF
          ELSE
            SELECT (lcTmpGridSrc)
            *: N000682,1 HES Globalization Changes [Start]
*!*	            REPLACE &lcTmpGridSrc..CopyStat WITH 'Copy Error'
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH 'LANG_Copy_Error'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH 'LANG_Copy_Error'
REPLACE &lcTmpGridSrc..CopyStat WITH 'IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Error',loFormSet.GetHeaderText("LANG_Copy_Error'",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

            *: N000682,1 HES Globalization Changes [End  ]
          ENDIF
        CASE lnAction = 2                && Resume (Skip)
          *: N000682,1 HES Globalization Changes [Start]
*!*	          REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists'
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Exists
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Exists
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

          *: N000682,1 HES Globalization Changes [End  ]
        CASE lnAction = 3                && Cancel
          *: N000682,1 HES Globalization Changes [Start]
*!*	          REPLACE &lcTmpGridSrc..CopyStat WITH 'Already exists'
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Exists
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Exists
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

          *: N000682,1 HES Globalization Changes [End  ]
          EXIT
        ENDCASE
    ELSE
      IF lfChkDt()
        =lfCopyHd()
      ELSE
        SELECT (lcTmpGridSrc)
        *: N000682,1 HES Globalization Changes [Start]
*!*	        REPLACE &lcTmpGridSrc..CopyStat WITH 'Copy Error'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Copy_Error
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Copy_Error
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Error,loFormSet.GetHeaderText("LANG_Copy_Error",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

        *: N000682,1 HES Globalization Changes [End  ]
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

SELECT GLAUTHD
=gfTableUpdate()
SELECT GLAUTDT
=gfTableUpdate()

SELECT (lcTmpGridSrc)

IF lnRecNum <= RECCOUNT()
  GO lnRecNum
ENDIF

SELECT (lcTmpGridSrc)

REPLACE ALL INC WITH .F. FOR !EMPTY(CopyStat)

IF lnRecNum <= RECCOUNT()
  GOTO lnRecNum
ENDIF

*** message with no of groups copied.
*WAIT ALLTRIM(STR(lnCopyTypes))+" Automatic entries copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02234","NOWAIT",ALLTRIM(STR(lnCopyTypes)))

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
*- Update GLTYPES
*- collecting groups from temporary file to memory variables
SCATTER MEMVAR
SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
*- Inserting selected groups into "gltypes" file
m.DautlgDat = {}
INSERT INTO &lcDataDir.GLAUTHD FROM MEMVAR

*- Adding audit information to "GLTYPES" files
SELECT GLAUTHD
=gfAdd_Info("GLAUTHD")
=gfReplace('')

*- adjusting copied groups counter
lnCopyTypes = lnCopyTypes + 1

SELECT (lcTAutDt)
ln_DtNo = RECNO()

SCAN REST WHILE &lcTAutDt..cAutType+&lcTAutDt..cAutCode = &lcTmpGridSrc..cAutType+&lcTmpGridSrc..cAutCode
  *- Update gltypes
  *- Collecting groups from temporary file to memory variables
  SCATTER MEMVAR
  SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
  *- Inserting selected groups into "gltypes" file
  INSERT INTO &lcDataDir.GLAUTDT FROM MEMVAR

  *- adding audit information to "gltypes" files
  =gfAdd_Info("GLAUTDT")

  SELECT GLAUTDT
  =gfReplace('')
ENDSCAN

SELECT (lcTAutDt)

IF ln_DtNo <= RECCOUNT()
  GOTO ln_DtNo
ENDIF

*- replacing copystat with proper variable
SELECT (lcTmpGridSrc)
*: N000682,1 HES Globalization Changes [Start]
*!*	REPLACE &lcTmpGridSrc..CopyStat WITH 'Copied',;
*!*	        &lcTmpGridSrc..Inc WITH .F.
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copied,loFormSet.GetHeaderText("LANG_Copied",loFormSet.HeaderAlias)),;
        &lcTmpGridSrc..Inc WITH .F.
*: N000682,1 HES Globalization Changes [End  ]


SELECT GLAUTHD
=gfObj_Lock(.F.)

ENDFUNC
*- End of lfCopyHd


*!*************************************************************
*! Name      : lfChkDt
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to check the detail record
*!*************************************************************
FUNCTION  lfChkDt

llCopyValid  = .T.

SELECT (lcTmpGridSrc)

IF cAutType = "A"
  SELECT GLACCHAR
  IF !SEEK(&lcTmpGridSrc..cActCode)
    SELECT (lcTmpGridSrc)
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTmpGridSrc..CopyStat WITH 'Account not found'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Account_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Account_NotFound
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account_NotFound,loFormSet.GetHeaderText("LANG_Account_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
    llCopyValid=.F.
  ELSE
    *- Checking that the selected type range is not overlaping with any other ranges in the target company file
    SELECT GLTYPES
    IF SEEK(GLACCHAR.cTypeCode)
      LOCATE FOR BETWEEN(VAL(&lcTmpGridSrc..cActCode) , VAL(GLTYPES.cTypLacno) , VAL(GLTYPES.cTypUacno) )
      *- Checking that the selected type is not a primary one otherwise bypassing range checking.
      IF !FOUND()
        SELECT (lcTmpGridSrc)
        *: N000682,1 HES Globalization Changes [Start]
*!*	        REPLACE &lcTmpGridSrc..CopyStat WITH 'Type Mismatch'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Type_Mismatch
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Type_Mismatch
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_Mismatch,loFormSet.GetHeaderText("LANG_Type_Mismatch",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

        *: N000682,1 HES Globalization Changes [End  ]
        llCopyValid=.F.
      ENDIF
    ELSE
      SELECT (lcTmpGridSrc)
      *: N000682,1 HES Globalization Changes [Start]
*!*	      REPLACE &lcTmpGridSrc..CopyStat WITH 'Type not found'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Type_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Type_NotFound
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_NotFound,loFormSet.GetHeaderText("LANG_Type_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
      llCopyValid=.F.
    ENDIF
  ENDIF
ENDIF

SELECT (lcTAutDt)
lnDtNo = RECNO()

SCAN REST WHILE &lcTAutDt..cAutType+&lcTAutDt..cAutCode = &lcTmpGridSrc..cAutType+&lcTmpGridSrc..cAutCode
  SELECT GLACCHAR
  IF !SEEK(&lcTAutDt..cAcctCode)
    SELECT (lcTAutDt)
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTAutDt..CopyStat WITH 'Account not found'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTAutDt..CopyStat WITH LANG_Account_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTAutDt..CopyStat WITH LANG_Account_NotFound
REPLACE &lcTAutDt..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account_NotFound,loFormSet.GetHeaderText("LANG_Account_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
    llCopyValid=.F.
    LOOP
  ELSE
    *- Checking that the selected type range is not overlaping with any other ranges in the target company file
    SELECT GLTYPES
    IF SEEK(&lcTAutDt..cTypeCode)
      LOCATE FOR BETWEEN(VAL(&lcTAutDt..cAcctCode) , VAL(GLTYPES.cTypLacno) , VAL(GLTYPES.cTypUacno))
      *- Checking that the selected type is not a primary one otherwise bypassing range checking.
      IF !FOUND()
        SELECT (lcTAutDt)
        *: N000682,1 HES Globalization Changes [Start]
*!*	        REPLACE &lcTAutDt..CopyStat WITH 'Type Mismatch'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTAutDt..CopyStat WITH LANG_Type_Mismatch
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTAutDt..CopyStat WITH LANG_Type_Mismatch
REPLACE &lcTAutDt..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_Mismatch,loFormSet.GetHeaderText("LANG_Type_Mismatch",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

        *: N000682,1 HES Globalization Changes [End  ]
        llCopyValid=.F.
        LOOP
      ENDIF
    ELSE
      SELECT (lcTAutDt)
      *: N000682,1 HES Globalization Changes [Start]
*!*	      REPLACE &lcTAutDt..CopyStat WITH 'Type not found'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTAutDt..CopyStat WITH LANG_Type_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTAutDt..CopyStat WITH LANG_Type_NotFound
REPLACE &lcTAutDt..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_NotFound,loFormSet.GetHeaderText("LANG_Type_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
      llCopyValid=.F.
      LOOP
    ENDIF
  ENDIF
ENDSCAN

SELECT (lcTAutDt)

IF lnDtNo <= RECCOUNT()
  GO lnDtNo
ENDIF

RETURN llCopyValid == .T.

ENDFUNC
*- End of lfChkDt
