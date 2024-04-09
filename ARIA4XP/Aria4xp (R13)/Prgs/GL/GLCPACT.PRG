*****************************************************************************
*: Program file        : GLCPACT.SCX
*: Program desc.       : Copy Account Entry
*: Module              : General Ledger (GL)
*: System		       : Aria Apparel System (A4xp)
*: Developer	       : Saber Saber [SAB]
*: Tracking Job Number : E303272.EXE
*: Date                : 10/09/2012 
*****************************************************************************
*Modifications :
*****************************************************************************
*: N000682,1 HES Globalization Changes [Globalization Project]
*: B612109,1 Es 03/30/2020 GL Account copy screen does not copy accounts even if the user selected accounts in accounts grid. [T20200309.0004]
#INCLUDE d:\Aria4xp\PRGs\GL\GLCOPY.H

SET PROCEDURE TO ADDBS(oAriaApplication.ProgramHome)+'GL\GL.FXP' ADDITIVE
** lcTmpGridSrc : Temp file name for selected accounts   && lc_SelAcct
** lc_TSegVal   : Temp file name for selected accounts
** lc_Temp1     : Temp file name for group lines
** lc_Temp2     : Temp file name for selected criteria
** lcAlTypes    : Alias name to GLTYPES  in source company
** lcAlAcHar    : Alias name to GLACCHAR in source company
** lcAlAcSeg    : Alias name to GLSEGVAL in source company
** lcAccts      :

** lnNoOfAcc    : Var. to store number of selected accounts
** laBalRecs    : Array of balance periods
** lnAcsSegSz   : Account Segment Size

PRIVATE lcTmpGridSrc, lc_TSegVal, lc_Temp1, lc_Temp2, lcAlTypes, lcAlAcHar, lcAlAcSeg, lcAccts, lnNoOfAcc, lnlistLine, lnAction
STORE " " TO lcTmpGridSrc, lc_TSegVal, lc_Temp1, lc_Temp2, lcAlTypes, lcAlAcHar, lcAlAcSeg, lcAccts
STORE 0   TO lnNoOfAcc, lnlistLine, lnAction


*- Create the Name for the temporary file
lcTmpGridSrc = gfTempName()  && Temp file name for selected accounts
lc_TSegVal   = gfTempName()  && Temp file name for segment values
lc_Temp1     = gfTempName()  && Temp file name for group lines
lc_Temp2     = gfTempName()  && Temp file name for selected criteria
lcAlTypes    = gfTempName() && Alias name to GLTYPES  in source company
lcAlAcHar    = gfTempName() && Alias name to GLACCHAR in source company
lcAlAcSeg    = gfTempName() && Alias name to GLSEGVAL in source company


*- Open used files and Create temp files
=lfPrepFls()

*-Get Account Segment Size
PRIVATE lnAcsSegSz
SELECT ACCOD
LOCATE
lnAcsSegSz = ACCOD.nAcsSegSz

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

*- Select window years and the accounts periods for the active company from SYCFSPRD into array laBalRecs
PRIVATE laBalRecs
DIMENSION laBalRecs[1,1]
=gfOpenTable('FSPRD','','SH')
SELECT FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
  FROM FSPRD;
  ORDER BY FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
  INTO ARRAY laBalRecs
=gfCloseTable('FSPRD')

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
=lfAddProp(loFormSet,'lc_TSegVal', lc_TSegVal)
=lfAddProp(loFormSet,'lc_Temp1', lc_Temp1)
=lfAddProp(loFormSet,'lc_Temp2', lc_Temp2)
=lfAddProp(loFormSet,'lcAlTypes', lcAlTypes)
=lfAddProp(loFormSet,'lcAlAcHar', lcAlAcHar)
=lfAddProp(loFormSet,'lcAlAcSeg', lcAlAcSeg)
=lfAddProp(loFormSet,'lcAccts', lcAccts)
=lfAddProp(loFormSet,'lnNoOfAcc', lnNoOfAcc)
=lfAddProp(loFormSet,'laBalRecs', laBalRecs)
=lfAddProp(loFormSet,'lnAcsSegSz', lnAcsSegSz)

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
*!*	  .aSourceArray[1,1]    = "No company selected"
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
*loFormSet.AriaForm1.cmdCopyOption.Caption = LANG_Select_Accounts
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdCopyOption.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Accounts,loFormSet.GetHeaderText("LANG_Select_Accounts",loFormSet.HeaderAlias))
loFormSet.AriaForm1.cmdCopyOption.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Accounts,loFormSet.GetHeaderText("LANG_Select_Accounts",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_Select_Accounts",loFormSet.HeaderAlias))
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

  *Inc, cTypeCode, SUBSTR(cAcctCode,1,lnAcsSegSz), SUBSTR(cAccnlDes,1,30-lnAcsSegSz), CopyStat
  =lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..Inc", '')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cTypeCode", LANG_GLCPACT_Acount_Type, 40)                               &&"Account Type"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cTypeCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Acount_Type,loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Type",loFormSet.HeaderAlias
*N000682,1 TMI 04/22/13 [Start] 
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cTypeCode", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Acount_Type,loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Type",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Type",loFormSet.HeaderAlias
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..cTypeCode",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Acount_Type,loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Type",loFormSet.HeaderAlias)))
*N000682,1 TMI 04/22/13 [End  ] 
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAcctCode, 1, lnAcsSegSz)", LANG_GLCPACT_Acount_Code, 70)        &&"Account Code"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAcctCode, 1, lnAcsSegSz)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Acount_Code,loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Code
*N000682,1 TMI 04/22/13 [Start] 
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAcctCode, 1, lnAcsSegSz)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Acount_Code,loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Code",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Acount_Code,loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Code",loFormSet.HeaderAlias))
=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAcctCode, 1, lnAcsSegSz)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Acount_Code,loFormSet.GetHeaderText("LANG_GLCPACT_Acount_Code",loFormSet.HeaderAlias)))
*N000682,1 TMI 04/22/13 [End  ] 
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAccnlDes, 1, 30-lnAcsSegSz)", LANG_GLCPACT_Account_Desc, 250)   &&"Account Description"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAccnlDes, 1, 30-lnAcsSegSz)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Account_Desc,loFormSet.GetHeaderText("LANG_GLCPACT_Account
*N000682,1 TMI 04/22/13 [Start] 
*=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAccnlDes, 1, 30-lnAcsSegSz)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Account_Desc,loFormSet.GetHeaderText("LANG_GLCPACT_Account_Desc",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Account,loFormSet.GetHeaderText("LANG_GLCPACT_Account",loFormSet.HeaderAlias))
=lfAddColumn(@lnI, oGrid, "SUBSTR(&lcTmpGridSrc..cAccnlDes, 1, 30-lnAcsSegSz)", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_Account_Desc,loFormSet.GetHeaderText("LANG_GLCPACT_Account_Desc",loFormSet.HeaderAlias)))
*N000682,1 TMI 04/22/13 [End  ] 
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,LANG_GLCPACT_CopyStat, 150)                               &&"Copy Status"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_CopyStat,loFormSet.GetHeaderText("LANG_GLCPACT_CopyStat",loFormSet.HeaderAlias)),
*N000682,1 TMI 04/22/13 [Start] 
*=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_CopyStat,loFormSet.GetHeaderText("LANG_GLCPACT_CopyStat",loFormSet.HeaderAlias)),loFormSet.GetHeaderText("LANG_GLCPACT_CopyStat",loFormSet.HeaderAlias))
=lfAddColumn(@lnI, oGrid, "&lcTmpGridSrc..CopyStat"    ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GLCPACT_CopyStat,loFormSet.GetHeaderText("LANG_GLCPACT_CopyStat",loFormSet.HeaderAlias)))
*N000682,1 TMI 04/22/13 [End  ] 
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

=gfCloseTable('GLACCHAR')
=gfCloseTable('GLTYPES')
=gfCloseTable('GLACBALS')
=gfCloseTable('GLCFITEM')
=gfCloseTable('GLSEGVAL')
=gfCloseTable('GLAUTHD')
=gfCloseTable('SYDFIELD')
=gfCloseTable('ACCOD')

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
=gfOpenTable('GLACCHAR','ACCTCODE','SH')     && CACCTCODE
=gfOpenTable('GLTYPES','TYPECODE','SH')      && CTYPECODE
=gfOpenTable('GLACBALS','ACCYRPRD','SH')     && CACCTCODE+CFISFYEAR+CFSPPRDID
=gfOpenTable('GLCFITEM','CFICODE','SH')      && CSEGCAFLO
=gfOpenTable('GLSEGVAL','ACSSEGVAL','SH')    && CACSSEGNO+CSEGVALUE
=gfOpenTable('GLAUTHD','TYPECODE','SH')      && CAUTTYPE+CAUTCODE
=gfOpenTable('ACCOD','ACCSEGNO','SH')      && ALLTRIM(STR(NACSSEGNO))
=gfOpenTable(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYDFIELD','CFLD_NAME','SH')   && CFLD_NAME

*- Create empty temp file (lcTmpGridSrc) with the same structure of GLTYPES + Inc, CopyStat,cStatus.
lcTmpGridSrc = gfTempName()
SELECT GLACCHAR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3, 18]

laFileStru[lnFileStru+1, 1] = 'Inc'
laFileStru[lnFileStru+1, 2] = 'L'
laFileStru[lnFileStru+1, 3] = 1
laFileStru[lnFileStru+1, 4] = 0

laFileStru[lnFileStru+2, 1] = 'CopyStat'
laFileStru[lnFileStru+2, 2] = 'C'
laFileStru[lnFileStru+2, 3] = 17
laFileStru[lnFileStru+2, 4] = 0

laFileStru[lnFileStru+3, 1] = 'CSTATUS'
laFileStru[lnFileStru+3, 2] = 'C'
laFileStru[lnFileStru+3, 3] = 1
laFileStru[lnFileStru+3, 4] = 0

FOR lnLoop = 1 to 3
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop, 7] ,laFileStru[lnFileStru+lnLoop, 8],;
                laFileStru[lnFileStru+lnLoop, 9] ,laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11] ,laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13] ,laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15] ,laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO  laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR

CREATE TABLE ADDBS(oAriaApplication.Workdir)+(lcTmpGridSrc)+'.DBF' FROM ARRAY laFileStru
INDEX ON  cAcctCode TAG cAcctCode
SET ORDER TO TAG cAcctCode


*- Create empty temp file (lc_TSegVal) with the same structure of GLSEGVAL
SELECT GlSegVal

=AFIELDS(laFileStru)
CREATE TABLE ADDBS(oAriaApplication.Workdir)+(lc_TSegVal)+'.DBF' FROM ARRAY laFileStru

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

LOCAL lcTmpGridSrc, lcAlTypes, lcAlAcHar, lcAlAcSeg, lcComPath
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lcAlTypes = loFormSet.lcAlTypes
lcAlAcHar = loFormSet.lcAlAcHar
lcAlAcSeg = loFormSet.lcAlAcSeg
lcComPath = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))


IF USED(lcAlAcHar)
  SELECT (lcAlAcHar)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.GLACCHAR AGAIN ALIAS  &lcAlAcHar ORDER TAG ACCTCODE

IF USED(lcAlTypes)
  SELECT (lcAlTypes)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.GLTypes  AGAIN ALIAS  &lcAlTypes ORDER TAG TYPECODE

IF USED(lcAlAcSeg)
  SELECT (lcAlAcSeg)
ELSE
  SELECT 0
ENDIF
USE &lcComPath.GLSegVal AGAIN ALIAS  &lcAlAcSeg  ORDER TAG ACSSEGVAL

*- set relation between cAcctCode in the temp file and GlAcChar (account chart) in the source company path.
SELECT (lcTmpGridSrc)
SET RELATION TO &lcTmpGridSrc..cAcctCode INTO &lcAlAcHar

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

lc_Temp1     = loFormSet.lc_Temp1
lc_Temp2     = loFormSet.lc_Temp2
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lnNoOfAcc    = loFormSet.lnNoOfAcc
lcAlTypes    = loFormSet.lcAlTypes
lcAlAchar    = loFormSet.lcAlAchar
lnNoOfAcc    = RECCOUNT(lcTmpGridSrc)

DO FORM (oAriaApplication.ScreenHome+"\GL\GLSLACT.SCX") WITH lc_Temp1, lc_Temp2, lcTmpGridSrc, lnNoOfAcc, lcAlTypes, lcAlAchar, .T., .T.

SELECT (loFormSet.lcTmpGridSrc)
LOCATE
loFormSet.AriaForm1.grdCopyRecords.Refresh()

ENDFUNC


*!*************************************************************
*! Name      : lfvCopy
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to Copy selected records from the grid to active Company
*!*************************************************************
FUNCTION  lfvCopy
LPARAMETERS loFormSet

SET STEP ON

PRIVATE lcTmpGridSrc, lcAlTypes, lcAlAcHar, lcAlAcSeg, lcComPath, lcWorkDir, lcDataDir
lcTmpGridSrc = loFormSet.lcTmpGridSrc
lcAlTypes = loFormSet.lcAlTypes
lcAlAcHar = loFormSet.lcAlAcHar
lcAlAcSeg = loFormSet.lcAlAcSeg
lcComPath    = ADDBS(ALLTRIM(loFormSet.AriaForm1.cboFromCompany.Value))
lcWorkDir    = oAriaApplication.WorkDir
lcDataDir    = oAriaApplication.DataDir

PRIVATE lnAction

lnTotalRecs = 0
SELECT (lcTmpGridSrc)
lnRecNum = RECNO()

*- Start copy selected accounts
SELECT (lcTmpGridSrc)
llExitCopy = .F.

*- Count the number of included accounts to the variable lnTotRecs check if there is any selected accounts to copyied
*B612109,1 Es 03/30/2020 GL Account copy screen does not copy accounts even if the user selected accounts in accounts grid. [Start]
*COUNT FOR !(Inc) TO lnTotRecs
 COUNT FOR (Inc) TO lnTotRecs
*B612109,1 Es 03/30/2020 GL Account copy screen does not copy accounts even if the user selected accounts in accounts grid.  [End]

*- Case there is no selected accounts to copyied
IF lnTotRecs = 0
  *** Message "No included accounts to copy."
  =gfModalGen("TRM02103B00000","DIALOG","copy")
  RETURN .F.
ENDIF

lnAction = gfModalGen("TRM02192B00006","Dialog")

lnCopyActs = 0               && init. the no of copied accounts.
lnCurRec   = 0               && init. the current account variable.

lc_Balance = gfTempName()    && Temp file name for balances.

*- Create empty temp file (lc_Balance) with the same structure of GLACBALS.
SELECT GLACBALS
COPY STRUCTURE TO &lcWorkDir.&lc_Balance

lcTime = gfGetTime()
ldDate = oAriaApplication.SystemDate

SELECT GLACBALS
*- For every account we need to create a number of records matchs the number of periods for fiscal years with 0 values
FOR lnCount = 1 TO ALEN(laBalRecs,1)
  INSERT INTO &lcWorkDir.&lc_Balance ;
           (cAcctCode, cFisfYear, cFspPrdid, nacbptddr, nacbptdcr, nacbytddr, nacbytdcr, nacbopbal, nacbclbal, cAdd_User, dAdd_Date, cAdd_Time);
    VALUES ('00', laBalRecs[lnCount,1], laBalRecs[lnCount,2] , 0, 0, 0, 0, 0, 0, oAriaApplication.User_ID, ldDate, lcTime)
ENDFOR

SELECT (lcTmpGridSrc)

*- Start copying all selected accounts only
SCAN FOR (Inc) .AND. !llExitCopy
lnCurRec = lnCurRec + 1
  *- Searches the current dbf (GLACCHAR) if the current account number in the dbf (lcTmpGridSrc) is Already exists
  IF SEEK (cAcctCode,"GLACCHAR")
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTmpGridSrc..CopyStat WITH 'Already Exists'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Exists
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Exists
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Exists,loFormSet.GetHeaderText("LANG_Exists",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
  ELSE
    *- Check the account number type and if the accounte value lies between the values of any type in dbf (GLTYPES).
    IF lfCheckAct()
      *- Update dbf GLACCHAR and add the audit information.
      SELECT (lcTmpGridSrc)
      SCATTER MEMVAR
      SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
      INSERT INTO &lcDataDir.GLACCHAR FROM MEMVAR
      =gfAdd_Info('GLACCHAR')
      *- Increment the number of copyied accounts by 1
      lnCopyActs = lnCopyActs + 1

      SELECT (lc_Balance)
      *- Replace the account number in the dbf (lc_balance) with the current account number in the dbf (lcTmpGridSrc)
      REPLACE ALL &lc_Balance..cAcctCode WITH &lcTmpGridSrc..cAcctCode
      *- Update GLBALANCE.
      SELECT GLACBALS
      APPEND FROM &lcWorkDir.&lc_Balance
      *: N000682,1 HES Globalization Changes [Start]
*!*	      REPLACE &lcTmpGridSrc..CopyStat WITH 'Copied'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Copied
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Copied
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copied,loFormSet.GetHeaderText("LANG_Copied",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

      *: N000682,1 HES Globalization Changes [End  ]
    ENDIF
  ENDIF

  SELECT (lcTmpGridSrc)
ENDSCAN

SELECT GLACCHAR
=gfTableUpdate()
SELECT GLACBALS
=gfTableUpdate()

*- Close temp file (lc_Balance) then erase this file.
IF USED(lc_Balance)
  USE IN ALIAS(lc_Balance)
ENDIF
ERASE &lcWorkdir.&lc_Balance..DBF
ERASE &lcWorkdir.&lc_Balance..CDX

SELECT (lcTmpGridSrc)
REPLACE ALL  Inc WITH .F.

*WAIT ALLTRIM(STR(lnCopyActs))+" Accounts copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02233","NOWAIT",ALLTRIM(STR(lnCopyActs)))

loFormSet.AriaForm1.cboFromCompany.Value = '*'
loFormSet.AriaForm1.cboFromCompany.Valid()

ENDFUNC
*-End of lfvCopy


*!*************************************************************
*! Name      : lfCheckAct
*! Developer : Saber A.Razek (SAB)
*! Date      : 10/09/2012
*! Purpose   : Function to check if the type for the current account is exist in dbf (GLTYPES)
*!*************************************************************
FUNCTION  lfCheckAct

*- Check if the type for the current account is exist in dbf (GLTYPES)
IF !SEEK(cTypeCode, "GLTYPES")
  *: N000682,1 HES Globalization Changes [Start]
*!*	  REPLACE &lcTmpGridSrc..CopyStat WITH 'Type not found'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Type_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Type_NotFound
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Type_NotFound,loFormSet.GetHeaderText("LANG_Type_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *: N000682,1 HES Globalization Changes [End  ]
  RETURN .F.
ELSE
  *- case found the type in the dbf (GLTYPES) check if this type between the values of (lower limit, upper limit) GLTYPES.cTyplacno, GLTYPES.cTypuacno
  IF !BETWEEN(VAL(cAcctCode),VAL(GLTYPES.cTyplacno),VAL(GLTYPES.cTypuacno))
    *: N000682,1 HES Globalization Changes [Start]
*!*	    REPLACE &lcTmpGridSrc..CopyStat WITH 'Out of range'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_out_ofRange
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_out_ofRange
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_out_ofRange,loFormSet.GetHeaderText("LANG_out_ofRange",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

    *: N000682,1 HES Globalization Changes [End  ]
    RETURN .F.
  ENDIF
ENDIF

*- Removes all records from a dbf (lc_TSegVal) and calling global function (gfSubStr) with 3 parameters
*   1 st parameter the current account code.
*   2 nd parameter array name.
*   3 rd parameter separeator for the account code (-).
*- This function extracts and returns every segment value into array (laAcctSegs)
SELECT (lc_TSegVal)
ZAP
DECLARE laAcctSegs[1]
SELECT (lcTmpGridSrc)
=gfSubStr(cAcctCode,@laAcctSegs,'-')

*- Check if this account code still exist in the master file (lcAlAcHar) for the source company ..
IF !EOF(lcAlAcHar)
  *- Case account code exist in source company..
  IF lnAction = 1
    SELECT (lcTmpGridSrc)
    IF !EMPTY(&lcTmpGridSrc..cSegCaFlo)
      SELECT GLCFITEM
      SET ORDER TO cFICode
      IF !SEEK(&lcTmpGridSrc..cSegCaFlo, "GLCFITEM")
        SELECT (lcTmpGridSrc)
        *: N000682,1 HES Globalization Changes [Start]
*!*	        lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|C.F. code'+ '|C.F. code: '+ALLTRIM(&lcTmpGridSrc..cSegCaFlo))
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|'+LANG_CF_Code+ '|'+LANG_CF_Code+ALLTRIM(&lcTmpGridSrc..cSegCaFlo))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|'+LANG_CF_Code+ '|'+LANG_CF_Code+ALLTRIM(&lcTmpGridSrc..cSegCaFlo))
lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CF_Code,loFormSet.GetHeaderText("LANG_CF_Code",loFormSet.HeaderAlias))+ '|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CF_Code,loFormSet.GetHeaderText("LANG_CF_Code",loFormSet.HeaderAlias))+ALLTRIM(&lcTmpGridSrc..cSegCaFlo))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

        *: N000682,1 HES Globalization Changes [End  ]
        DO CASE
          CASE lnCopyMode = 1    && Intialize fields to blank
            REPLACE &lcTmpGridSrc..cSegCaFlo WITH ''

          CASE lnCopyMode = 3    && Ignore copying this account
            *: N000682,1 HES Globalization Changes [Start]
*!*	            REPLACE &lcTmpGridSrc..CopyStat WITH 'C.F. not found'
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_CF_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_CF_NotFound
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CF_NotFound,loFormSet.GetHeaderText("LANG_CF_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

            *: N000682,1 HES Globalization Changes [End  ]
            RETURN .F.

          CASE lnCopyMode = 4    && Exit
            llExitCopy = .T.
            RETURN .F.

        ENDCASE
      ENDIF
    ENDIF

    IF !EMPTY(&lcTmpGridSrc..cSegAutDs)
      SELECT GLAUTHD
      SET ORDER TO TypeCode
      IF !SEEK('D'+&lcTmpGridSrc..cSegAutDs, "GLAUTHD")
        SELECT (lcTmpGridSrc)
        *** Account ð has ð, which deos not exist in the target company.
        *** You can copy the account without the ð (blank), copy it
        *** <as it is>, ignore copying the account, or stop copying the
        *** rest of accounts.In case of copying <as it is> you are
        *** responsable for adding this ð.

        *: N000682,1 HES Globalization Changes [Start]
*!*	        lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|A.D. code'+ '|A.D. code: '+ALLTRIM(&lcTmpGridSrc..cSegAutDs))
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|'+LANG_AD_Code+ '|'+LANG_AD_Code+ALLTRIM(&lcTmpGridSrc..cSegAutDs))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|'+LANG_AD_Code+ '|'+LANG_AD_Code+ALLTRIM(&lcTmpGridSrc..cSegAutDs))
lnCopyMode = gfModalGen("TRM02193B02018","Dialog", ALLTRIM(cAcctCode)+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AD_Code,loFormSet.GetHeaderText("LANG_AD_Code",loFormSet.HeaderAlias))+ '|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AD_Code,loFormSet.GetHeaderText("LANG_AD_Code",loFormSet.HeaderAlias))+ALLTRIM(&lcTmpGridSrc..cSegAutDs))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

        *: N000682,1 HES Globalization Changes [End  ]

        DO CASE
          CASE lnCopyMode = 1    && Intialize fields to blank
            REPLACE &lcTmpGridSrc..cSegAutDs WITH ''

          CASE lnCopyMode = 3    && Ignore copying this account
            *: N000682,1 HES Globalization Changes [Start]
*!*	            REPLACE &lcTmpGridSrc..CopyStat WITH 'A.D. not found'
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_AD_NotFound
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_AD_NotFound
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AD_NotFound,loFormSet.GetHeaderText("LANG_AD_NotFound",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

            *: N000682,1 HES Globalization Changes [End  ]
            RETURN .F.

          CASE lnCopyMode = 4    && Exit
            llExitCopy = .T.
            RETURN .F.

        ENDCASE
      ENDIF
    ENDIF

  ENDIF

  FOR lnCount = 1 TO ALEN(laAcctSegs,1)
    *- For every segment in array (laAcctSegs) check if this segment value is exist in the target company (GLSEGVAL)
    IF !SEEK(STR(lnCount,1)+laAcctSegs[lnCount],'GLSEGVAL')
      *- Case segment value not exist ....,  copy this segment values record from source company (lcAlAcSeg) into dbf (lc_TSegVal), and add audit information.
      SELECT (lcAlAcSeg)
      SEEK STR(lnCount,1)+laAcctSegs[lnCount]
      SCATTER MEMVAR
      INSERT INTO &lcWorkDir.&lc_TSegVal FROM MEMVAR
      =gfAdd_Info(lc_TSegVal)
    ENDIF
  ENDFOR
ELSE
  *- Case account code deleted from source company.., replace the copy status field for this account with 'Source deleted' and return false.
  *: N000682,1 HES Globalization Changes [Start]
*!*	  REPLACE &lcTmpGridSrc..CopyStat WITH 'Source deleted'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Source_Deleted
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*REPLACE &lcTmpGridSrc..CopyStat WITH LANG_Source_Deleted
REPLACE &lcTmpGridSrc..CopyStat WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Source_Deleted,loFormSet.GetHeaderText("LANG_Source_Deleted",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

  *: N000682,1 HES Globalization Changes [End  ]
  RETURN .F.
ENDIF

*- check if there is any record in the temp (lc_TSegVal) if found .. append all this new segment values from the
*  temp file (lc_TSegVal) to the master file (GLSEGVAL) in the target company.
IF RECCOUNT(lc_TSegVal) > 0
  SELECT GLSEGVAL
  APPEND FROM &lcWorkDir.&lc_TSegVal
ENDIF

ENDFUNC
*- End of lfCheckAct
