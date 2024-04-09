***********************************************************************
*:  Program File: APAPRPA.prg
*:  Desc.       : Manual check, Non Check, Cash payment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 01/01/2012
*:  Reference   : E303014,1
*:************************************************************************
*! B609857,1 SAB 03/11/2012 Fix AP Media Problems reported by Mariam [T20120304.00004]
*! B609890,1 SAB 04/17/2012 Fix Error in Displaying Currency [T20120304.0004]
*! B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1
*! B609953,1 MMT 06/06/2012 Non-Check Payemnt screen dispalys correct Invoice amount[T20120530.0008]
*! B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[T20120607.0033]
*! B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021]
*! B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072]
*! B610088,1 SAB 09/17/2012 Update the correct vendor balance when paying a payment [T20120719.0020]
*! B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [T20120719.0020]
*! B610172,1 SAB 12/11/2012 Fix problem of not changing the exchange rate when change payment date [T20121203.0003]
*! B610172,2 SAB 20/11/2012 Fix problem of not amend the exchnage rate that is added by the user [T20121203.0003]
*! E303342, HES 01/05/2012 Disable the OG while working except from the scope button [T20121123.0002]
*! B610257,1 HES 02/25/2013 Release lock from invoices if press UNDO before saving [T20130114.0002]
*! B610268,1 TMI 03/14/2013 fix a problem that the checking bank is not initialized  when there is no defaul bank in the APVENDOR table [T20130305.0036]
*! B610305,1 HIA 04/15/2013 T20130308.0009 - Check register summary. On the first page there is no checking account
*! B610311,1 TMI 04/17/2013 [T20130305.0036] fix a problem that the screen does not filter invoices by payment type
*! B610319,1 SAB 04/28/2013 get the correct exchange difference account code [T20130416.0004]
*! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments
*! B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012]
*! B611744,1 Es 03/14/2019 Non-Check/Cash/Manual Payment screens does not check the invoice status (paid field) before locking it to be paid when user select or select all [T20190213.0009]
*! B612057,1 MMT 02/19/2020 Invert button allow overwriting locked invoice in another sesstion[T20200212.0008]
*! B612189,1 Es 07/29/2020 Total Payment/ Year is not update correctly in APVENHST table when invoice is paid using Non-Check/Manual/Cash Payment screens. [T20200709.0009]
*:************************************************************************
PARAMETER lnPyChMN         && Refere where the program is called from
PRIVATE laBankObjs
DECLARE laSelect[1,2], laWndObj[4,3], laBankObjs[2,3]

#INCLUDE R:\aria4xp\PRGS\ap\apmnchp.H
DO FORM (oAriaApplication.ScreenHome+"\AP\APMNCHP.SCX") WITH lnPyChMN

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet,lnPyChMN
=lfAddProp(loFormSet,'lnPyChMN',lnPyChMN)

*!B609890,1 SAB 04/17/2012 Fix Error in Displaying Currency [T20120304.0004][Start]
loFormSet.Ariaform1.kbAprCurrCode.VISIBLE = (lnPyChMN = 3)
loFormSet.Ariaform1.lblAprCurCod.VISIBLE  = (lnPyChMN = 3)
*!B609890,1 SAB 04/17/2012 Fix Error in Displaying Currency [T20120304.0004][End]
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
SET PROCEDURE TO (oAriaApplication.ApplicationHome +'AP\APMAIN.FXP') ADDITIV
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]

SET MULTILOCKS ON
*- Open tables
=gfOpenTable('APINVHDR','INVVEND','SH')
=gfOpenTable('APVENDOR','VENCODE','SH')
=gfOpenTable('APVENHST','VENDYEAR','SH')
=gfOpenTable('APCHECKS','BANKCHECK','SH')
=gfOpenTable('APDIV','DIVISION','SH')
=gfOpenTable('CODES','IDRLTFNAME','SH')
*B610311,1 TMI 04/17/2013 [Start] open APBANKS via fox
*=gfOpenTable('APBANKS','BANKCODE','SH')
=gfOpenFile(oAriaApplication.DataDir+'APBANKS','BANKCODE','SH')
*B610311,1 TMI 04/17/2013 [End  ]
=gfOpenTable('SYCCURR','CCURRCODE','SH')
=gfOpenTable('APSETUP','APSETUP','SH')
=gfOpenTable('APDIST','INVVEND','SH')   && CINVNO+CVENDCODE+CAPDTRTYP

=gfOpenTable('APPAYMNT','TYPMETHNO','SH')

*- initializations
WITH loFormSet
  .nWorkArea = 'APINVHDR'
  .otoolbar.nWorkArea = 'APINVHDR'
  .DATAENVIRONMENT.INITIALSELECTEDALIAS = 'APINVHDR'
ENDWITH

*- Define variables
=lfDefineVars(loFormset)  && actually , has not been used

*-- Define custom tool bar buttons
DECLARE loFormSet.lapanelobj[1,6]
STORE '' TO loFormSet.lapanelobj
*-- Scope Button
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.BMP"
loFormSet.laPanelObj[1,3] = 'lfGetData'    && the loFormSet is sent by default as a parameter
loFormSet.laPanelObj[1,4] = "Option Grid"
loFormSet.laPanelObj[1,5] = "Option Grid"
*loFormSet.laPanelObj[1,6] = 'V'
loFormSet.laPanelObj[1,6] = 'S'

*- Create temp files
=lfCrTmpFls()

*- add property to create in it the class to call the vendor/ap invoice  screen
=lfAddProp(loFormSet,'clsVendCall' ,'')
loFormSet.clsVendCall = CREATEOBJECT('clsVendCall',loFormSet.lcAPINVHDR)

WITH loFormSet.ariaform1.grdAPINVHDR
  LOCAL lcAPINVHDR
  lcAPINVHDR = loFormSet.lcAPINVHDR
  .RECORDSOURCE = ''
  *.ColumnCount = IIF(loFormset.llMultiCr,14,11)
  .RECORDSOURCE = loFormSet.lcAPINVHDR
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.ariaform1.grdAPINVHDR

  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..LSELECT'    ,'')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..CINVNO'      ,LANG_APMNCHP_Inv_No     )  &&"Inv. No."
  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..CINVNO'      ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Inv_No,loFormSet.GetHeaderText("LANG_APMNCHP_Inv_No",loFormSet.HeaderAlias))     )  &&"Inv. No."
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  =lfAddColumn(@lnI,oGrid,'','')    &&"Command button"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..CVENPRIOR'   ,LANG_APMNCHP_Priority   )  &&"P"
  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..CVENPRIOR'   ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Priority,loFormSet.GetHeaderText("LANG_APMNCHP_Priority",loFormSet.HeaderAlias))   )  &&"P"
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'lfGetPayMeth(Thisformset)',LANG_APMNCHP_Pay_Meth   )  &&"  Pay.  Meth  "
  *B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[Start]
  *=lfAddColumn(@lnI,oGrid,'lfGetPayMeth(Thisformset)',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Pay_Meth,loFormSet.GetHeaderText("LANG_APMNCHP_Pay_Meth",loFormSet.HeaderAlias))   )  &&"  Pay.  Meth  "
  =lfAddColumn(@lnI,oGrid,'Thisformset.lfGetPAYPayMeth()',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Pay_Meth,loFormSet.GetHeaderText("LANG_APMNCHP_Pay_Meth",loFormSet.HeaderAlias))   )  &&"  Pay.  Meth  "  
  *B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[End]
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ', ;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Open_Amount,loFormSet.GetHeaderText("LANG_APMNCHP_Open_Amount",loFormSet.HeaderAlias))    )  &&"Open amount"

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NINVAMTAP'   ,LANG_APMNCHP_Appr_to_pay)  &&"Appr. to pay"
  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NINVAMTAP'   ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Appr_to_pay,loFormSet.GetHeaderText("LANG_APMNCHP_Appr_to_pay",loFormSet.HeaderAlias)))  &&"Appr. to pay"
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  IF loFormset.llMultiCr
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..nInvFAAp'  ,LANG_APMNCHP_Apr_foreign_amt  )&&"Apr foreign amt"
    =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..nInvFAAp'  ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Apr_foreign_amt,loFormSet.GetHeaderText("LANG_APMNCHP_Apr_foreign_amt",loFormSet.HeaderAlias))  )&&"Apr foreign amt"
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NINVDISAP'   ,LANG_APMNCHP_Appr_Disc)    &&"Appr. Disc."
  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NINVDISAP'   ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Appr_Disc,loFormSet.GetHeaderText("LANG_APMNCHP_Appr_Disc",loFormSet.HeaderAlias)))    &&"Appr. Disc."
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NINVADJAP'   ,LANG_APMNCHP_Appr_adj   )  &&       "Appr. adj."
  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NINVADJAP'   ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Appr_adj,loFormSet.GetHeaderText("LANG_APMNCHP_Appr_adj",loFormSet.HeaderAlias))   )  &&       "Appr. adj."
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..DINVDUDAT'   ,LANG_APMNCHP_Due_Date   )  &&"Due Date"
  =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..DINVDUDAT'   ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Due_Date,loFormSet.GetHeaderText("LANG_APMNCHP_Due_Date",loFormSet.HeaderAlias))   )  &&"Due Date"
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  IF loFormset.llMultiCr
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..CCURRCODE' ,LANG_APMNCHP_Curr_code  )  &&       "Curr. code"
    =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..CCURRCODE' ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Curr_code,loFormSet.GetHeaderText("LANG_APMNCHP_Curr_code",loFormSet.HeaderAlias))  )  &&       "Curr. code"
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NEXRATE'   ,LANG_APMNCHP_Curr_rate  )  &&    "Curr. rate"
    =lfAddColumn(@lnI,oGrid,'&lcAPINVHDR..NEXRATE'   ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Curr_rate,loFormSet.GetHeaderText("LANG_APMNCHP_Curr_rate",loFormSet.HeaderAlias))  )  &&    "Curr. rate"
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=lfAddColumn(@lnI,oGrid,'gfCodDes(&lcAPINVHDR..CDIVISION,"CDIVISION")' , LANG_APMNCHP_Division)  &&    "Curr. rate"
  =lfAddColumn(@lnI,oGrid,'gfCodDes(&lcAPINVHDR..CDIVISION,"CDIVISION")' , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Division,loFormSet.GetHeaderText("LANG_APMNCHP_Division",loFormSet.HeaderAlias)))  &&    "Curr. rate"
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  *- Remove extra columns
  FOR lnX = lnI+1 TO oGrid.COLUMNCOUNT
    lcX = ALLTRIM(STR(lnX))
    oGrid.REMOVEOBJECT("Column&lcX")
  ENDFOR

  BINDEVENT(.Column3.Command1,"Click",loFormSet.clsVendCall,"lfInvoice")
  .READONLY = .T.
  .REFRESH()
  .DOSCROLL(2)
ENDWITH

=lfAddProp(loFormSet,'lcRpListFor','')

*- Add flag lf1stRun
=lfAddProp(loFormSet,'lf1stRun',.T.)

*- fill up the temp file and collect data
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
*!*	IF ! loFormSet.lf1stRun
*!*	  =lfGetData(loFormset)
*!*	ENDIF
=lfGetData(loFormSet,.T.)
loFormSet.ChangeMode('S')
*- End of lfFormInit
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End]

******************************************************************************
*Name      : lfAfterRowColChange
*Developer : TMI - Tarek Mohammed Ibrahim
*Date      : 1/3/2012
*Purpose   : After Row Col Change of the Grid
******************************************************************************
FUNCTION lfAfterRowColChange
PARAMETERS loGrid
LOCAL lcAPINVHDR
lcAPINVHDR = loGrid.RECORDSOURCE
WITH loGrid.PARENT
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *.cmdSelect.Caption = IIF(&lcAPINVHDR..LSELECT,'Un Se\<lect','Se\<lect')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.cmdSelect.Caption = IIF(&lcAPINVHDR..LSELECT,LANG_APMNCHP_UNSELECT,LANG_APMNCHP_SELECT)
  .cmdSelect.CAPTION = IIF(&lcAPINVHDR..LSELECT,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_UNSELECT,;
    loGrid.PARENT.PARENT.GetHeaderText("LANG_APMNCHP_UNSELECT",loGrid.PARENT.PARENT.HeaderAlias)),;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_SELECT,;
    loGrid.PARENT.PARENT.GetHeaderText("LANG_APMNCHP_SELECT",loGrid.PARENT.PARENT.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]
  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  .cmdSelect.REFRESH
ENDWITH
*- End of lfAfterRowColChange.

*!*************************************************************
*! Name      : lfAddColumn
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303014,1 TMI 01/03/2011
*! Purpose   : A function to add columns to the passed grid object
*!*************************************************************
FUNCTION lfAddColumn
LPARAMETERS lnI,oGrid,lcFld,lcTitle
LOCAL lcI
lnI = lnI + 1
lcI = ALLTRIM(STR(lnI))
WITH oGrid
  .COLUMN&lcI..CONTROLSOURCE    = lcFld
  .COLUMN&lcI..Header1.CAPTION  = lcTitle
ENDWITH
*- End of lfAddColumn.

************************************************************************************
*Name : lfFormActivate
*Date:*E303014,1 TMI 01/08/2012
*Purpose: Run lfGetData in the activate method for the first time the screen runs
************************************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet
IF loFormSet.lf1stRun
  loFormSet.lf1stRun = .F.
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
  *=lfGetData(loFormset)
  =lfGetData(loFormSet,.T.)
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]
ENDIF
*- End of lfFormActivate.

********************************************************************************
*Name : lfCrTmpFls
*E303014,1 TMI 01/06/2012
*Purpose:*- Create the temp table that holds the invoices that are ready to be approved
********************************************************************************
FUNCTION lfCrTmpFls

=lfAddProp(loFormSet,'lcAPINVHDR',gfTempName())
=lfAddProp(loFormSet,'lcAPCHECKS',gfTempName())
=lfAddProp(loFormSet,'lcAPPAYMNT',gfTempName())
=lfAddProp(loFormSet,'lcAPVENDOR',gfTempName())
=lfAddProp(loFormSet,'lcAPVENHST',gfTempName())
=lfAddProp(loFormSet,'lcAPDIST'  ,gfTempName())

LOCAL laStru
DIMENSION laStru[1,18]
SELECT APINVHDR
=AFIELDS(laStru)

*- add fields
lnArrLen = ALEN(laStru,1)
DIMENSION laStru[lnArrLen+2 ,18]
laStru[lnArrLen+1 ,1] = 'LSELECT'
laStru[lnArrLen+1 ,2] = 'L'
laStru[lnArrLen+1 ,3] = 1
laStru[lnArrLen+1 ,4] = 0

laStru[lnArrLen+2 ,1] = 'LUPDATED'
laStru[lnArrLen+2 ,2] = 'L'
laStru[lnArrLen+2 ,3] = 1
laStru[lnArrLen+2 ,4] = 0

*- update other array fields
FOR lnI = lnArrLen+1 TO ALEN(laStru,1)
  STORE .F. TO laStru[lnI,5],laStru[lnI,6]
  FOR lnJ = 7 TO 16
    laStru[lnI,lnJ] = ""
  ENDFOR
  STORE 0 TO laStru[lnI,17],laStru[lnI,18]
ENDFOR

DIMENSION laIndex[1,2]
laIndex[1,1] = 'CINVNO+CVENDCODE'
laIndex[1,2] = 'INVVEND'
=gfCrtTmp(loFormSet.lcAPINVHDR,@laStru,@laIndex,loFormSet.lcAPINVHDR)
SELECT (loFormSet.lcAPINVHDR)
SET ORDER TO INVVEND

DIMENSION laStru[1,18]
SELECT APCHECKS
=AFIELDS(laStru)
DIMENSION laIndex[1,2]
laIndex[1,1] = 'CBNKCODE+CCHKACCT'
laIndex[1,2] = 'BANKCHECK'
=gfCrtTmp(loFormSet.lcAPCHECKS,@laStru,@laIndex,loFormSet.lcAPCHECKS)

DIMENSION laStru[1,18]
SELECT APPAYMNT
=AFIELDS(laStru)
DIMENSION laIndex[1,2]
laIndex[1,1] = 'CBNKCODE+CCHKACCT+CPAYTYPE+CPAYMETH+CPAYDOCNO'
laIndex[1,2] = 'CHKTMNO'
=gfCrtTmp(loFormSet.lcAPPAYMNT,@laStru,@laIndex,loFormSet.lcAPPAYMNT)

DIMENSION laStru[1,18]
SELECT APVENDOR
=AFIELDS(laStru)
DIMENSION laIndex[1,2]
laIndex[1,1] = 'CVENDCODE'
laIndex[1,2] = 'VENCODE'
=gfCrtTmp(loFormSet.lcAPVENDOR,@laStru,@laIndex,loFormSet.lcAPVENDOR)

DIMENSION laStru[1,18]
SELECT APVENHST
=AFIELDS(laStru)
DIMENSION laIndex[1,2]
laIndex[1,1] = 'CVENDCODE+CFISFYEAR'
laIndex[1,2] = 'VENDYEAR'
=gfCrtTmp(loFormSet.lcAPVENHST,@laStru,@laIndex,loFormSet.lcAPVENHST)

DIMENSION laStru[1,18]
SELECT APDIST
=AFIELDS(laStru)
DIMENSION laIndex[1,2]
laIndex[1,1] = 'CINVNO+CVENDCODE+CAPDTRTYP'
laIndex[1,2] = 'INVVEND'
=gfCrtTmp(loFormSet.lcAPDIST,@laStru,@laIndex,loFormSet.lcAPDIST)
*- End of lfCrTmpFls.

*<> try to put this function in a global program to not rewrite it many times
*!*************************************************************
*! Name      : lfAddProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/17/2011
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
    loObj.ADDPROPERTY(lcPropToCreate,PropValue)
  ENDIF
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR

*<> try to put this function in a global program to not rewrite it many times
************************************************************************************************
* Name         : lfPopPropArray
* Developer    : tmi - Tarek Mohamed Ibrahim
* Date         : 12/21/2011
* Purpose      : Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*-             : In the start of each function that changes the APINVHDR file initialize all the variables.
************************************************************************************************
FUNCTION lfPopPropArray
LPARAMETERS loObj,lcArrPropName,lcVarsString,InitVal
LOCAL lnLen,lnOldLen,lnI,lnCommaPos

lcVarsString = lcVarsString+','
lnLen = OCCURS(',',lcVarsString)
lnOldLen = ALEN(loObj.&lcArrPropName,1)
DIMENSION loObj.&lcArrPropName.[lnOldLen+lnLen,2]
FOR lnI = 1 TO lnLen
  lnCommaPos = AT(',',lcVarsString)
  loObj.&lcArrPropName.[lnOldLen+lnI,1] = ALLTRIM(SUBSTR(lcVarsString,1,lnCommaPos-1))
  loObj.&lcArrPropName.[lnOldLen+lnI,2] = InitVal
  lcVarsString = SUBSTR(lcVarsString,lnCommaPos+1)
ENDFOR

*- End of lfPopPropArray.

***************************************************************************************
* Name      : clsVendCall
* Developer : Tmi Trek mohammed Ibfrahim
* Date      : 12/21/2011
* Purpose   : Define a class to bind its method to the button added to the grdApprove to call the vendor and appyinv screens
***************************************************************************************
DEFINE CLASS clsVendCall AS CUSTOM
  lcApinvhdr = ''
  PROCEDURE INIT
  PARAMETERS lcApinvhdr
  THIS.lcApinvhdr = lcApinvhdr
  ENDPROC

  *!*    PROCEDURE lfVendor
  *!*      *call the vendor screen here for vendor# :CVENDCODE
  *!*      LOCAL lcCVENDCODE
  *!*      lcCVENDCODE = EVALUATE(this.lcAPINVHDR+".CVENDCODE")
  *!*      oAriaApplication.DoProgram("AWRAPVENDR",'"&lcCVENDCODE"',.F.,"")
  *!*    ENDPROC

  PROCEDURE lfInvoice
  *call the Invoice screen here for Inv# :CINVNO
  LOCAL lcCVENDCODE,lcCINVNO
  lcCVENDCODE = EVALUATE(THIS.lcAPINVHDR+".CVENDCODE")
  lcCINVNO    = EVALUATE(THIS.lcAPINVHDR+".CINVNO")
  *!B609857,1 SAB 03/11/2012 Solve Media reported Problems[Start]
  *oAriaApplication.DoProgram("AWRAPPYINV",'"&lcCVENDCODE.","&lcCINVNO."',.F.,"")
  oAriaApplication.DoProgram("AWRAPPYINV",'"&lcCVENDCODE.","&lcCINVNO."',.F.,"AP")
  *!B609857,1 SAB 03/11/2012 Solve Media reported Problems[End]
  ENDPROC

ENDDEFINE

***********************************************************************************
*Name    : lfGetData
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/24/2011
*Purpose : called from the AfterRowColChange event in the grdApprove on the screen
***********************************************************************************
FUNCTION lfGetData
*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][Start]

*! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [Begin]
*PARAMETERS loFormSet,llDontShowOG
PARAMETERS loFormSet, llDontShowOG, llNotShowMSG
*! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [End]

*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][End]
*-Clear data from temp files
WITH loFormSet
  SELECT (.lcAPINVHDR)
  ZAP
  SELECT (.lcAPCHECKS)
  ZAP
  SELECT (.lcAPVENDOR)
  ZAP
  SELECT (.lcAPVENHST)
  ZAP
  SELECT (.lcAPPAYMNT)
  ZAP
  SELECT (.lcAPDIST)
  ZAP
ENDWITH

*- Call the "APMNCHP" OG
LOCAL lcExpr
llMultiCr = loFormset.llMultiCr
lnPyChMN  = loFormset.lnPyChMN
DECLARE laOgFxFlt[1,2],laOgVrFlt[1,2]    && define filter arrays to use after the OG is closed
* X   remove the apinvhdr.cvendcode from the lcExpr
lcRpListFor = loFormSet.lcRpListFor
*B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][Start]
*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][Start]
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]

STORE '' TO lcOgVendCode, lcOgBNKCODE, lcOgCHKACCT
llFound = .F.
WITH loFormSet.Ariaform1
  IF !EMPTY(ALLTRIM(.KBVendCode.Keytextbox.VALUE))
    lcVendCode = APVENDOR.CVENDCODE
    IF SEEK(lcVendCode,'APVENDOR','VENCODE')
      llFound = .T.
    ENDIF
    *!*	    ELSE
    *!*	      IF !EMPTY(ALLTRIM(.kbVendPhone.Keytextbox.VALUE)) OR !EMPTY(ALLTRIM(.kbVendCompany.Keytextbox.VALUE))
    *!*	        SELECT APVENDOR
    *!*	        LOCATE FOR IIF(!EMPTY(ALLTRIM(.kbVendPhone.Keytextbox.VALUE)),CPHONENO = ALLTRIM(.kbVendPhone.Keytextbox.VALUE),CVENCOMP = ALLTRIM(.kbVendCompany.Keytextbox.VALUE))
    *!*	        IF FOUND()
    *!*
    *!*	        ENDIF
    *!*	      ENDIF
  ENDIF
  lcOgVendCode = APVENDOR.CVENDCODE
  lcOgBNKCODE = APVENDOR.CBNKCODE
  lcOgCHKACCT = APVENDOR.CCHKACCT
  lcOgCURRCODE = APVENDOR.CCURRCODE
  *B610268,1 TMI 03/14/2013 [Start] fix a problem that the checking bank is not initialized  when there is no defaul bank in the APVENDOR table
  lnPyChMN = loFormSet.lnPyChMN
  =SEEK(APVENDOR.CDIVISION,'APDIV')
  GO TOP IN APSETUP
  =lfGetBnkChk(@lcOgBNKCODE,@lcOgCHKACCT,lcOgVendCode)
  *B610268,1 TMI 03/14/2013 [End  ] fix a problem that the checking bank is not initialized  when there is no defaul bank in the APVENDOR table
ENDWITH
IF llDontShowOG
  WITH loFormSet.Ariaform1
    IF  llFound
      *!*	        .KBVendCode.Keytextbox.VALUE = lcOgVendCode
      *!*	        .kbVendCompany.Keytextbox.VALUE = APVENDOR.CVENCOMP
      *!*	        .kbVendPhone.Keytextbox.VALUE = APVENDOR.CPHONENO
      .BankChk.kbBanks.Keytextbox.VALUE      = IIF(EMPTY(ALLTRIM(.BankChk.kbBanks.Keytextbox.VALUE)),lcOgBNKCODE,.BankChk.kbBanks.Keytextbox.VALUE)
      .BankChk.kbChkAccount.Keytextbox.VALUE = IIF(EMPTY(ALLTRIM(.BankChk.kbChkAccount.Keytextbox.VALUE)),lcOgCHKACCT,.BankChk.kbChkAccount.Keytextbox.VALUE)
      .kbAprCurrCode.Keytextbox.VALUE        = IIF(EMPTY(ALLTRIM(.kbAprCurrCode.Keytextbox.VALUE)),IIF(loFormSet.llMultiCr,APVENDOR.CCURRCODE,oAriaApplication.BaseCurrency),.kbAprCurrCode.Keytextbox.VALUE)
      *B610311,1 TMI 04/17/2013 [Start] add the filter expression based on it get the acurate invoices
      lcExprsion = '(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ <> 0)'
      IF loFormSet.lnPyChMN = 3 && If the program called from cash payments bar
        lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = '"+.kbAprCurrCode.Keytextbox.VALUE+"'"
      ELSE
        lcExprsion = lcExprsion + ".AND. CVENPMETH <> 'H' .AND." +"CBNKCODE+CCHKACCT='"+.BankChk.kbBanks.Keytextbox.VALUE+"'+'"+.BankChk.kbChkAccount.Keytextbox.VALUE+"'"
      ENDIF
      *B610311,1 TMI 04/17/2013 [End  ]
    ENDIF
  ENDWITH


  IF llFound
    SELECT APINVHDR
    lcSvOrd = ORDER()
    SET ORDER TO VENDINV   && CVENDCODE+CINVNO

    *- fill the temp file with the data
    =SEEK(lcVendCode,'APINVHDR')
    *B610311,1 TMI 04/17/2013 [Start] add the filter expression
    *SCAN REST WHILE APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendCode ;
    FOR (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
    CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'
    SCAN REST WHILE APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendCode ;
        FOR (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
        CVENPRIOR<> '0' .AND. CINVSTAT<> 'V' AND EVALUATE(lcExprsion)
      *B610311,1 TMI 04/17/2013 [End  ]
      SCATTER MEMVAR
      m.COWNER = ''  && use it later in the save process
      SELECT (loFormSet.lcAPINVHDR)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
    SELECT APINVHDR
    SET ORDER TO &lcSvOrd

    =SEEK(lcVendCode,'APVENDOR','VENCODE')   && CVENDCODE

    SELECT APVENDOR
    SCATTER MEMVAR MEMO
    INSERT INTO (loFormSet.lcAPVENDOR) FROM MEMVAR

    =SEEK(lcVendCode,'APVENHST','VENDYEAR')   && CVENDCODE+CFISFYEAR

    SELECT APVENHST
    SCAN REST WHILE CVENDCODE+CFISFYEAR = lcVendCode
      SCATTER MEMVAR MEMO
      INSERT INTO (loFormSet.lcAPVENHST) FROM MEMVAR
    ENDSCAN

    SELECT APCHECKS

    =SEEK(lcOgBNKCODE+lcOgCHKACCT,'APCHECKS')
    SELECT (loFormSet.lcAPINVHDR)
    LOCATE
    IF loFormSet.lnPyChMN = 1  AND !EOF(loFormSet.lcAPINVHDR)&& Calling from manual check.
      SELECT APCHECKS
      IF (ALLTRIM(APCHECKS.cLok_User) == ALLTRIM(oAriaApplication.User_ID)) .OR. gfObj_lock(.T.) && lock the pointed record.
      ELSE
        =gfModalGen("TRM04055B00000","DIALOG",ALLTRIM(lcOgBNKCODE)+'-'+ALLTRIM(lcOgCHKACCT))
        WITH loFormSet
          SELECT (.lcAPINVHDR)
          ZAP
          SELECT (.lcAPCHECKS)
          ZAP
          SELECT (.lcAPVENDOR)
          ZAP
          SELECT (.lcAPVENHST)
          ZAP
          SELECT (.lcAPPAYMNT)
          ZAP
          SELECT (.lcAPDIST)
          ZAP
        ENDWITH
        loFormSet.ChangeMode('S')
      ENDIF
    ENDIF

    SCATTER MEMVAR MEMO
    INSERT INTO (loFormSet.lcAPCHECKS) FROM MEMVAR

    SELECT (loFormSet.lcAPINVHDR)
    LOCATE
    IF EOF()

      *! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [Begin]
      IF !llNotShowMSG
        =gfModalGen("TRM04035B00000","DIALOG",ALLTRIM(lcVendCode))
      ENDIF
      *! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [End]

      loFormSet.ChangeMode('S')
    ELSE
      loFormSet.ChangeMode('E')
    ENDIF
  ENDIF
ENDIF
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]

IF !llDontShowOG
  *!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][END]
  LOCAL lcSetProcD
  lcSetProcD = SET("Procedure")
  *B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][End]
  lcExpr = gfOpGrid('APMNCHP',.T. ,.F. ,.F. ,.T. ,.T.)
  *B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][Start]
  SET PROCEDURE TO &lcSetProcD.
  *B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][END]
  *!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][Start]
ELSE
  lcExpr = '.F.'
ENDIF
*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][END]
*- if cancel clicked , go to Select Mode
IF lcExpr == '.F.'
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
  *loFormSet.ChangeMode('S')
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End]
  RETURN
ENDIF

loFormSet.lcRpListFor = lcRpListFor

*- Set the comparison operator based on if this is a Debit memo, Invoice or both
LOCAL lcCmpOpr
lcCmpOpr = IIF(lcRpListFor='I',">",IIF(lcRpListFor='D',"<","<>"))

lcCurrCode = IIF(loFormSet.llMultiCr,APVENDOR.CCURRCODE,oAriaApplication.BaseCurrency)
lcVendCode = lfGetCrVal("APINVHDR.CVENDCODE")
lcBankCode = lfGetCrVal("APINVHDR.CBNKCODE")
lcCheckCode = lfGetCrVal("APINVHDR.CCHKACCT")
*!B609890,1 SAB 04/17/2012 Fix Error in Displaying Currency [T20120304.0004][Start]
*lcCurrCode = lfGetCrVal("APINVHDR.CCURRCODE")
lcCurrCode = lfGetCrVal("APINVHDR.CAPRCURCOD")
*!B609890,1 SAB 04/17/2012 Fix Error in Displaying Currency [T20120304.0004][End]

=SEEK(lcVendCode,'APVENDOR','VENCODE')

WITH loFormSet.Ariaform1
  .KBVendCode.Keytextbox.VALUE = APVENDOR.CVENDCODE
  .kbVendCompany.Keytextbox.VALUE = APVENDOR.CVENCOMP
  .kbVendPhone.Keytextbox.VALUE = APVENDOR.CPHONENO
  .BankChk.kbBanks.Keytextbox.VALUE = lcBankCode
  .BankChk.kbChkAccount.Keytextbox.VALUE = lcCheckCode
  .kbAprCurrCode.Keytextbox.VALUE = lcCurrCode
ENDWITH

lcDivCode = lfGetCrVal("APINVHDR.CDIVISION")
=lfRepExpr(@lcExpr,"INLIST(","APINVHDR.CDIVISION",lcDivCode)

lcExprsion = '(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ &lcCmpOpr 0)'
IF loFormSet.lnPyChMN = 3 && If the program called from cash payments bar
  lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = '&lcCurrCode'"
ELSE
  lcExprsion = lcExprsion + ".AND. CVENPMETH <> 'H' .AND." +"CBNKCODE+CCHKACCT='&lcBankCode'+'&lcCheckCode'"
ENDIF

SELECT APINVHDR
lcSvOrd = ORDER()
SET ORDER TO VENDINV   && CVENDCODE+CINVNO

**-
*- fill the temp file with the data
=SEEK(lcVendCode,'APINVHDR')
SCAN REST WHILE APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendCode ;
    FOR (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
    CVENPRIOR<> '0' .AND. CINVSTAT<> 'V' .AND.;
    EVALUATE(lcExprsion) AND EVALUATE(lcExpr)

  SCATTER MEMVAR
  m.COWNER = ''  && use it later in the save process
  SELECT (loFormSet.lcAPINVHDR)
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT APINVHDR
SET ORDER TO &lcSvOrd

*!B610088,1 SAB 09/17/2012 Update the correct vendor balance when paying a payment [Start]
=SEEK(lcVendCode,'APVENDOR','VENCODE')   && CVENDCODE
*!B610088,1 SAB 09/17/2012 Update the correct vendor balance when paying a payment [End]
**-
SELECT APVENDOR
SCATTER MEMVAR MEMO
INSERT INTO (loFormset.lcAPVENDOR) FROM MEMVAR

*!B610088,1 SAB 09/17/2012 Update the correct vendor balance when paying a payment [Start]
=SEEK(lcVendCode,'APVENHST','VENDYEAR')   && CVENDCODE+CFISFYEAR
*!B610088,1 SAB 09/17/2012 Update the correct vendor balance when paying a payment [End]
**-
SELECT APVENHST
SCAN REST WHILE CVENDCODE+CFISFYEAR = lcVendCode
  SCATTER MEMVAR MEMO
  INSERT INTO (loFormset.lcAPVENHST) FROM MEMVAR
ENDSCAN

**-
SELECT APCHECKS
*!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
=SEEK(lcBankCode+lcCheckCode,'APCHECKS')
SELECT (loFormSet.lcAPINVHDR)
LOCATE
IF loFormset.lnPyChMN = 1  AND !EOF(loFormSet.lcAPINVHDR)&& Calling from manual check.
  SELECT APCHECKS
  IF (ALLTRIM(APCHECKS.cLok_User) == ALLTRIM(oAriaApplication.User_ID)) .OR. gfObj_lock(.T.) && lock the pointed record.
  ELSE
    =gfModalGen("TRM04055B00000","DIALOG",ALLTRIM(lcBankCode)+'-'+ALLTRIM(lcCheckCode))
    WITH loFormSet
      SELECT (.lcAPINVHDR)
      ZAP
      SELECT (.lcAPCHECKS)
      ZAP
      SELECT (.lcAPVENDOR)
      ZAP
      SELECT (.lcAPVENHST)
      ZAP
      SELECT (.lcAPPAYMNT)
      ZAP
      SELECT (.lcAPDIST)
      ZAP
    ENDWITH
    loFormSet.ChangeMode('S')
  ENDIF
ENDIF
*!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[End]
SCATTER MEMVAR MEMO
INSERT INTO (loFormSet.lcAPCHECKS) FROM MEMVAR


*- check if there is any collected lines or not
SELECT (loFormSet.lcAPINVHDR)
LOCATE
IF EOF()
  *! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [Begin]
  IF !llNotShowMSG
    =gfModalGen("TRM04035B00000","DIALOG",ALLTRIM(lcVendCode))
  ENDIF
  *! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [End]
  loFormSet.ChangeMode('S')
ELSE
  *- go to Edit mode when data is selected
  loFormSet.ChangeMode('E')
ENDIF
*- End of lfGetData.

************************************************************************************************
* Name        : lfRepExpr
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 10/03/2011
* Purpose     : to remove a part of the filter from the lcRpExp
************************************************************************************************
FUNCTION lfRepExpr
PARAMETERS lcExp,lcSucr,lcRmv,lcPut
LOCAL lnPos,lcRight
lcRight = ")"
lnPos = AT(lcSucr+lcRmv,lcExp)
IF lnPos>0
  *: B609857,1 SAB 03/11/2012 Fix AP Media Problems reported by Mariam [Start]
  *lnPranth = AT(lcRight,SUBSTR(lcExp,lnAndPos))
  lnPranth = AT(lcRight,SUBSTR(lcExp,lnPos))
  *: B609857,1 SAB 03/11/2012 Fix AP Media Problems reported by Mariam [End]
  lcExp = STUFF(lcExp,lnPos, lnPranth,lcRmv + '$' + lcPut)
ENDIF
*- End of lfRepExpr.

************************************************************************************
*Name      : lfGetCrVal
*Developer :*E303014,1 TMI
*Date      :01/05/2012 [Start]
*Purpose   :Get the criteria value from the filter arrays
************************************************************************************
FUNCTION lfGetCrVal
PARAMETERS lcFltFld
LOCAL lnPos
lnPos = ASCAN(laOgFxFLt,lcFltFld)
IF lnPos > 0
  lnPOS = ASUBSCRIPT(laOgFxFLt,lnPos,1)
  RETURN laOgFxFLt[lnPos,6]
ENDIF
lnPos = ASCAN(laOgVrFLt,lcFltFld)
IF lnPos > 0
  lnPOS = ASUBSCRIPT(laOgVrFLt,lnPos,1)
  RETURN laOgVrFLt[lnPos,6]
ENDIF
*- End of lfGetCrVal.


*!*************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen Change mode Method
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
LOCAL llEnable
llEnable = .F.

DO CASE
CASE loFormSet.ActiveMode = 'S'

  llEnable = .F.

  oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','ENABLED')
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
  WITH loFormSet.Ariaform1
    .kbVendPhone.ENABLED = .T.
    .kbVendCompany.ENABLED = .T.
    .KBVendCode.ENABLED = .T.
    .kbAprCurrCode.ENABLED = llEnable
    .BankChk.kbBanks.ENABLED = llEnable
    .BankChk.kbChkAccount.ENABLED = llEnable
  ENDWITH
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]


CASE loFormSet.ActiveMode = 'E'

  llEnable = .T.

  oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','DISABLED')
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
  WITH loFormSet.Ariaform1
    .kbVendPhone.ENABLED = .F.
    .kbVendCompany.ENABLED = .F.
    .KBVendCode.ENABLED = .F.
    .kbAprCurrCode.ENABLED = llEnable
    .BankChk.kbBanks.ENABLED = llEnable
    .BankChk.kbChkAccount.ENABLED = llEnable
  ENDWITH
  *!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]

ENDCASE

WITH loFormSet.AriaForm1
  .cmdSelect.ENABLED = llEnable
  .cmdSelNon.ENABLED = llEnable
  .cmdSelAll.ENABLED = llEnable
  .cmdInvert.ENABLED = llEnable
  .cmdPay.ENABLED    = llEnable
  .cmdVendor.ENABLED = llEnable
ENDWITH

WITH loFormSet.oToolBar
  .cmdSelect.ENABLED = .F.
  .cmdFind.ENABLED = .F.
  .cmdTop.ENABLED = .F.
  .cmdPrev.ENABLED = .F.
  .cmdNext.ENABLED = .F.
  .cmdEnd.ENABLED = .F.
  .cmdEdit.ENABLED = .F.
ENDWITH
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
*!*	WITH loFormSet.Ariaform1
*!*	  .KBVendCode.Enabled = .F.
*!*	  .kbVendCompany.Enabled = .F.
*!*	  .kbVendPhone.Enabled = .F.
*!*	  .BankChk.kbBanks.Enabled = .F.
*!*	  .BankChk.kbChkAccount.Enabled = .F.
*!*	  .kbAprCurrCode.Enabled = .F.
*!*	ENDWITH
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [END]
loFormSet.Ariaform1.grdAPINVHDR.READONLY = .T.


*- End of lfChangeMode.

*****************************************************************
*Name       : lfFormBeforeSave
*Developer  : TMI
*Date       : <Dt>
*Purpose    : check if there is any data to save
*****************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet
LOCAL lnRec,lcFlt
SELECT (loFormSet.lcAPINVHDR)
lnRec = RECNO()
lcFlt = FILTER()
SET FILTER TO
LOCATE
LOCATE FOR LUPDATED
IF !FOUND()
  SET FILTER TO &lcFlt
  GO lnRec
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'No invoices have been paid to save')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_APMNCHP_NOINVOICESPAID)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_NOINVOICESPAID,loFormSet.GetHeaderText("LANG_APMNCHP_NOINVOICESPAID",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  RETURN .F.
ENDIF

*! B610305,1 HIA 04/15/2013 T20130308.0009 - Check register summary. On the first page there is no checking account

LOCAL lcCHKBankCode, lcCHKCheckAcc

lcCHKBankCode = loFormSet.Ariaform1.BankChk.kbBanks.Keytextbox.VALUE
lcCHKCheckAcc = loFormSet.Ariaform1.BankChk.kbChkAccount.Keytextbox.VALUE

IF EMPTY(lcCHKBankCode)
  gfModalGen('INM04074B00000','DIALOG','Bank code')
  RETURN .F.
ENDIF

IF EMPTY(lcCHKCheckAcc)
  gfModalGen('INM04074B00000','DIALOG','Checking Account code')
  RETURN .F.
ENDIF
*! B610305,1 HIA 04/15/2013 T20130308.0009 - Check register summary. On the first page there is no checking account



*- End of lfFormBeforeSave.



*!*************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/13/2011
*! Purpose   : Save data
*!*************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loFormSet

*-1
SELECT (loFormset.lcAPPAYMNT)
LOCATE
SCAN
  SCATTER MEMVAR MEMO
  SELECT APPAYMNT
  APPEND BLANK
  GATHER MEMVAR MEMO
  =lfAdd_Info(.T.)
ENDSCAN

*-2
IF loFormset.lnPyChMN = 1
  SELECT (loFormSet.lcAPCHECKS)
  lcAPCHECKS = loFormSet.lcAPCHECKS
  SCATTER MEMVA
  SELECT APCHECKS
  =SEEK(&lcAPCHECKS..CBNKCODE+&lcAPCHECKS..CCHKACCT,'APCHECKS')
  *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
  =gfObj_lock(.F.)
  SELECT APCHECKS
  *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[END]
  REPLACE DCHKLMDAT WITH m.DCHKLMDAT ;
    NCHKLMAMT WITH m.NCHKLMAMT ;
    NCHKNXTMN WITH m.NCHKNXTMN
  =lfAdd_Info()
  *  =gfObj_lock(.F.)
ENDIF

*-3
lcAPVENDOR = loFormset.lcAPVENDOR
SELECT (loFormset.lcAPVENDOR)
SCATTER MEMVAR
SELECT APVENDOR
=SEEK(&lcAPVENDOR..CVENDCODE,'APVENDOR','VENCODE')
REPLACE DVENLPAYD WITH m.DVENLPAYD ;
  NVENLPAYA WITH m.NVENLPAYA ;
  NVEN1099B WITH m.NVEN1099B ;
  NVENOPNDR WITH m.NVENOPNDR ;
  NVENBAL   WITH m.NVENBAL   ;
  NVENCPAY  WITH m.NVENCPAY  ;
  CVENLPAYN WITH m.CVENLPAYN
=lfAdd_Info()

*-4
lcAPVENHST = loFormSet.lcAPVENHST
SELECT (loFormSet.lcAPVENHST)
LOCATE
SCAN
  SCATTER MEMVAR
  SELECT APVENHST
  =SEEK(&lcAPVENHST..CVENDCODE+&lcAPVENHST..CFISFYEAR)
  lcField="NVNHPAY"+ALLTRIM(STR(VAL(loFormSet.lcPeriod)))
  REPLACE NVNHDISTKN WITH  m.NVNHDISTKN ;
    NVNHTOTPA  WITH  m.NVNHTOTPA  ;
    NVNHADJ    WITH  m.NVNHADJ    ;
    NVNHMCHKP  WITH  m.NVNHMCHKP  ;
    NVNHNCHKP  WITH  m.NVNHNCHKP  ;
    NVNHCASHP  WITH  m.NVNHCASHP  ;
    &lcField   WITH  m.&lcField.
  =lfAdd_Info()
ENDSCAN

*-5
SELECT (loFormset.lcAPDIST)
LOCATE
SCAN
  SCATTER MEMVAR MEMO
  SELECT APDIST
  APPEND BLANK
  GATHER MEMVAR MEMO
  =lfAdd_Info(.T.)
ENDSCAN

*-6
lcAPINVHDR = loFormSet.lcAPINVHDR
SELECT (loFormSet.lcAPINVHDR)
SET FILTER TO
LOCATE
SCAN FOR LUPDATED
  REPLACE LUPDATED WITH .F.

  SCATTER MEMVAR MEMO
  SELECT APINVHDR
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
  =gfObj_lock(.F.)
  SELECT APINVHDR
  *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[END]

  REPLACE NINVPAID   WITH m.NINVPAID   ;
    NINVDISTK  WITH m.NINVDISTK  ;
    NINVADJ    WITH m.NINVADJ    ;
    NINV1099A  WITH m.NINV1099A  ;
    CBNKCODE   WITH "",;
    CCHKACCT   WITH "",;
    CCHKGLACC  WITH "",;
    NINVAMTAP  WITH 0,;
    NINVDISAP  WITH 0,;
    NINVADJAP  WITH 0,;
    NINVA1099  WITH 0,;
    CAPRCURCOD WITH '',;
    NAPREXRAT  WITH 0,;
    NAPRCURUNT WITH 0,;
    DCHKDATE   WITH m.DCHKDATE   ;
    CCHKNO     WITH m.CCHKNO     ;
    nInvFAAp   WITH 0
  =lfAdd_Info()
  *  =gfObj_lock(.F.)
ENDSCAN

*- Update tables
SELECT APINVHDR
=gfTableUpdate()
SELECT APCHECKS
=gfTableUpdate()
SELECT APPAYMNT
=gfTableUpdate()
SELECT APVENDOR
=gfTableUpdate()
SELECT APVENHST
=gfTableUpdate()
SELECT APDIST
=gfTableUpdate()

*- Call a new session of data collection
*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][Start]
*=lfGetData(loFormset)

*! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [Begin]
*=lfGetData(loFormset,.T.)
=lfGetData(loFormset,.T., .T.)
*! B610373,1 HIA 06/10/2013 T20130305.0039	Disbursing non-check payments [End]

*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
loFormSet.ChangeMode('S')
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]

*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][End]
*- Refresh
loFormset.Ariaform1.grdAPINVHDR.REFRESH()

*- End of lfFormSavefiles

********************************************************************************
*Name       : lfAdd_Info
*Developer  : TMI
*Date       : <Dt>
*Purpose    : user information
********************************************************************************
FUNCTION lfAdd_Info
PARAMETERS llAdd
IF llAdd
  REPLACE cAdd_User  WITH oAriaApplication.User_ID ,;
    dAdd_Date  WITH DATE()    ,;
    cAdd_Time  WITH gfGetTime(),;
    cAdd_Ver   WITH oAriaApplication.cShortVersion
ELSE && Modified Record
  REPLACE cEdit_User  WITH oAriaApplication.User_ID ,;
    dEdit_Date  WITH DATE()    ,;
    cEdit_Time  WITH gfGetTime(),;
    cEdt_Ver    WITH oAriaApplication.cShortVersion
ENDIF

*- Clear lock fields
REPLACE LLOK_STAT WITH .F. ;
  CLOK_USER WITH '' ;
  DLOK_DATE WITH {} ;
  CLOK_TIME WITH ''

*- End of lfAdd_Info.

************************************************************************************************
*Name       : lfFormBeforealtermode
*Developer  : TMI
*Date       : <Dt>
*Purpose    : check before undo
************************************************************************************************
FUNCTION lfFormBeforealtermode
PARAMETERS loFormSet
LOCAL lnRec,lcFil
IF TYPE('loFormSet.lcAPINVHDR')='U'
  RETURN
ENDIF

SELECT (loFormSet.lcAPINVHDR)
lnRec = RECNO()
lcFil = FILTER()
SET FILTER TO
LOCATE
LOCATE FOR LUPDATED
IF FOUND()
  * Are you sure you want to ð?
  * \!\<Yes;\?\<No
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *IF gfModalGen('QRM40169B00006','DIALOG','lose all your changes?') <> 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *IF gfModalGen('QRM40169B00006','DIALOG',LANG_APMNCHP_LOSECHANGES) <> 1
  IF gfModalGen('QRM40169B00006','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_LOSECHANGES,loFormSet.GetHeaderText("LANG_APMNCHP_LOSECHANGES",loFormSet.HeaderAlias))) <> 1
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    SELECT (loFormSet.lcAPINVHDR)
    SET FILTER TO &lcFil
    LOCATE
    GO lnRec
    RETURN .F.
  ENDIF
ENDIF

*- End of lfFormBeforealtermode             .

************************************************************************************************
*Name : lfFormUndo
*Developer :TMI - Tarek Mohamed Ibrahim
*Date:12/24/2011
*Purpose:undo procedure
************************************************************************************************
FUNCTION lfFormUndo
PARAMETERS loFormSet

*!B609953,1 MMT 06/06/2012 Non-Check Payemnt screen dispalys correct Invoice amount[Start]
lcAPINVHDR = loFormSet.lcAPINVHDR
SELECT (lcAPINVHDR)

*!* B610257,1 HES 02/25/2013 Release lock from invoices if press UNDO before saving [Start]
SET FILTER TO
*!* B610257,1 HES 02/25/2013 Release lock from invoices if press UNDO before saving [End  ]

LOCATE
*!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [Start]
*SCAN
SCAN FOR &lcAPINVHDR..LSELECT
  *!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [End]
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  SELECT APINVHDR
  =gfObj_lock(.F.)
  SELECT (lcAPINVHDR)
ENDSCAN
*!B609953,1 MMT 06/06/2012 Non-Check Payemnt screen dispalys correct Invoice amount[End]
*!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
IF loFormset.lnPyChMN = 1  && Calling from manual check.
  WITH loFormSet.ariaform1
    lcBankCode  = .BankChk.kbBanks.Keytextbox.VALUE
    lcCheckCode = .BankChk.kbChkAccount.Keytextbox.VALUE
  ENDWITH
  SELECT APCHECKS
  =SEEK(lcBankCode+lcCheckCode,'APCHECKS')
  =gfObj_lock(.F.) && lock the pointed record.
ENDIF
*!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[ENd]
*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][Start]
*=lfGetData(loFormset)
=lfGetData(loFormset,.T.)
*!B610071,1 MMT 09/06/2012 Modify AP Payment screens to no open OG after Saving and Undo[T20120730.0072][END]
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
=loFormSet.ChangeMode('S')
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]

*- End of lfFormUndo

****************************************************************************************
*Name      : lfDefineVars
*Developer : TMI - TAREK MOHAMED IBRAHIM
*Date      : 1/1/2012
* Purpose   : Define all variables to be used in the screen
***************************************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormset

*---  Variables used
** lcVenSum     : Vendor summary window name
** lcInvSum     : Invoice summary window name
** lcSelect     : Variable to hold sign if the record selected.
** lcDivDisc    : Variable to hold division description in browse.
** lcFuncName   : Variable to hold Refresh window.
** lcBrowName   : Variable to hold Brows Window name.
** lcReadNam1   : Variable to hold Read Window name.
** lcReadNam2   : Variable to hold Read Window name.
** lcPayNum     : Variable to hold Payment number.
** lcExprsion   : Variable to hold filter expresion.
** lcOldExpr    : Variable to hold old filter expresion.
** lcPeriod     : Variable to hold First period.
** lcFisPrd     : Variable to hold First period.
** lcYear       : Variable to hold first year.
** lcFisYer     : Variable to hold first year.
** lcTAprAmnt   : The Approve amount in approve message.
** lcVendCode   : Variable to hold vendor code.
** lcOldVndcd   : Variable to hold old vendor code.
** lcVendComp   : Variable to hold vendor company.
** lcVPhone     : Variable to hold vendor phone.
** lcDivision   : Variable to hold division description.
** lcCInvRef    : Variable to hold current Inv. Ref.
** lcPriority   : Variable to hold Paroirity.
** lcPaymeth    : Variable to hold pay method.
** lcOVendCod   : Variable to hold old vendor code.
** lcOldVnCmp   : Variable to hold old vendor company.
** lcOldDivDs   : Variable to hold old division description.
** lcOldDvCod   : Variable to hold old division code.
** lcOldPrty    : Variable to hold old Paroirity.
** lcOldpymth   : Variable to hold old pay method.
** lcOMethod    : Variable to hold old method.
** lcTThrou     : Variable to hold word through.
** lcTFrom      : Variable to hold word From.
** lcTBlnkCode  : Variable to hold text not applicable.
** lcTInvNo     : Variable to hold text
** lcTPrior     : Variable to hold text
** lcTPayMeth   : Variable to hold text
** lcTAprPay    : Variable to hold text
** lcTAprDisc   : Variable to hold text
** lcTAprAdj    : Variable to hold text
** lcTInvDate   : Variable to hold text
** lcTDivDes    : Variable to hold text
** lcTOpnAmnt   : Variable to hold text
** lcTCurrCode  : Variable to hold text
** lcTCurRate   : Variable to hold text
** lcSesNum     : Variable to hold session number.
** lcCurYear    : Variable to hold current year.
** lcBrowStr    : Variable to hold browse string.
** lcOldVal     : Variable to hold old vendor code, company and phone.
** lcObjectSt   : Variable to hold object status enable or disable.
** lcObjDisp    : Variable to hold object display status.
** lcInvoice    : Variable to hold invoice window title.
** lcBrTtl      : Variable to hold Browse Window Title.
** lcBankCode   : Variable to hold bank code.
** lcOldBank    : Variable to hold old bank code.
** lcCheckcode  : Variable to hold check code.
** lcOldCheck   : Variable to hold old check code.
** lcInvRef     : Variable to hold Inv. Ref.
** lcOldInvRf   : Variable to hold old Inv. Ref.
** lcEscTrp     : Variable to hold escape trap.
** lcColPair    : Variable to hold
** lcColor      : Variable to hold
** lcVendTtl    : Variable to hold vendor window title.
** lcDivCode    : Variable to hold division code.
** lcTAll       : Variable to hold text all.
** lcCodeFilt   : Variable to hold code filter.
** lcBrowFile   : Vairable to hold the Browse File.
** lcCurrCode   : Variable to hold the currency code.
** lcOldCurr    : Variable to hold the old currency.
*Currency equation sign variables. (Begin)
** in case of exchange currency from invoice currency to company base currency.
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
** in case of exchange currency from invoice currency to approved currency.
** lcExSin3             Variable to hold the first sign in the equation.
** lcExSin4             Variable to hold the second sign in the equation.
** in case of exchange currency from approved currency to company base currency.
** lcExSin5             Variable to hold the first sign in the equation.
** lcExSin7             Variable to hold the second sign in the equation.
** lcCurrSmbl   : Variable to hold the approved currency symbol.
** lcBaseSmbl   : Variable to hold the base currency symbol.

** lnOScope     : Variable to hold old rb Scope.
** lnArlen      : Variable to hold Select array length.
** lnkeyno      : Variable to Select order in vendor browse.
** lnDivDes     : Variable to hold pu Division description.
** lnMethod     : Variable to hold ib Method of payment.
** lnVnTotPay   : Variable to hold vendor history tot pay.
** lnTotPay     : Variable to hold Total Payment.
** lnTotDisc    : Variable to hold Total Discount.
** lnTotAdj     : Variable to hold Total Adjustment.
** lnTot1099    : Variable to hold Total 1099.
** lnAprToPay   : Variable to hold approve to pay amount.
** lnAprDisc    : Variable to hold approve discount.
** lnAprAdj     : Variable to hold approve adjustment.
** lnOldAprTPy  : Variable to hold old approve to pay amount.
** lnOldAprDisc : Variable to hold old approve discount.
** lnOldAprAdj  : Variable to hold old approve adjustment.
** lnBrRecNo    : Browse record number
** lnChkNum     : Variable to hold Check number.
** lnGnChkNm    : Variable to hold old check number.
** lnOChkNum    : Variable to hold old check number.
** lnTime       : Variable to hold time to consider duble click.

** puDiv        : Variable to hold pop up division.
** puDivDes     : Variable to hold pu Division Description.
** puDivision   : Variable to hold pop division.

** ldOldPdat    : Variable to hold old payment date.
** ldFrDueDat   : Variable to hold from due date.
** ldToDueDat   : Variable to hold to due date.
** ldFrDisDat   : Variable to hold from discount date.
** ldToDisDat   : Variable to hold to discount date.
** ldOFrDueDt   : Variable to hold old from due date.
** ldOToDueDt   : Variable to hold old to due date.
** ldOFrDisDt   : Variable to hold old from discount date.
** ldOToDisDt   : Variable to hold old to discount date.
** ldPayDat     : Variable to hold Payment Date.

** llBrowse     : Variable to hold flag in case of activate browse by mouse.
** llAprPart    : Variable to hold parameter to branch between appr. fully and appr. part.
** llScope      : Variable to hold if you want scope to be done.
** llVendExst   : If Vendor object is not exist in scope screen.
** llDisBro     : If refresh window in display function.
** llChkCan     : Variable to hold
** llFromTrap   : Variable to hold if call model screen from browse trap.
** llDispbr     : Flag to show browse refresh if vendor or bank or checks changed only.
** llNoContrl   : Variable to hold there is not Control panel.
** llproceed    : Variable to hold flag to proceed.

** rbScope      : Variable to hold rb Scope.
** rbDuDsDt     : Variable to hold rb due date and discount date.
** rbODuDsDt    : Variable to hold rb due date and discount date.
** rbScopeOn    : Variable to hold rb Scope On in rb Scope.
** ibmethod     : Variable to hold ib Method of payment.

** laCtrStat    : To disable the browse pad in the menu

*- Create a property to hold the AP class as a reference
=lfAddProp(loFormSet,'AP1' ,'')
loFormSet.ap1 = CREATEOBJECT('AP')

*- Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*- In the start of each function that changes the APINVHDR file initialize all the variables.
=lfAddProp(loFormSet,'laPropArr[1]' ,'Temp')

lcVars = "llNoContrl , llproceed"
=lfPopPropArray(loFormSet,'laPropArr',lcVars, .T.    )

lcVars = "llBrowse   , llAprPart   , llScope    , llVendExst ,"+;
  "llDisBro   , llChkCan    , llFromTrap , llDispbr"
=lfPopPropArray(loFormSet,'laPropArr',lcVars, .F.    )

lcVars = "lnBrRecNo  , lnChkNum    , lnGnChkNm  , lnOChkNum   "
=lfPopPropArray(loFormSet,'laPropArr',lcVars, 0      )

*!*	lcVars = "lnVnTotPay , lnTotPay    , lnTotDisc  , lnTotAdj   ,"+;
*!*	         "lnTot1099  , lnAprToPay  , lnAprDisc  , lnAprAdj   ,"+;
*!*	         "lnOldAprTPy, lnOldAprDisc, lnOldAprAdj "
*!*	=lfPopPropArray(loFormSet,'laPropArr',lcVars, 0.00   )

lcVars = "rbScope    , rbDuDsDt    , rbODuDsDt  , puDivDes   ,"+;
  "ibmethod   , lnOScope    , lnArlen    , lnkeyno    ,"+;
  "lnDivDes   , lnMethod"
=lfPopPropArray(loFormSet,'laPropArr',lcVars, 1      )

lcVars = "rbScopeOn"
=lfPopPropArray(loFormSet,'laPropArr',lcVars, 3      )

lcVars = "ldPayDat    "
=lfPopPropArray(loFormSet,'laPropArr',lcVars, oAriaApplication.systemdate )

lcVars = "ldOldPdat  , ldFrDueDat  , ldToDueDat , ldFrDisDat ,"+;
  "ldToDisDat , ldOFrDueDt  , ldOToDueDt , ldOFrDisDt ,"+;
  "ldOToDisDt"
=lfPopPropArray(loFormSet,'laPropArr',lcVars, {}     )

lcVars = "lcVenSum   , lcInvSum    , lcSelect   , lcDivDisc  ,"+;
  "lcFuncName , lcBrowName  , lcReadNam1 , lcReadNam2 ,"+;
  "lcPayNum   , lcExprsion  , lcOldExpr  , lcPeriod   ,"+;
  "lcFisPrd   , lcYear      , lcFisYer   , lcTAprAmnt ,"+;
  "lcVendCode , lcOldVndcd  , lcVendComp , lcVPhone   ,"+;
  "lcDivision , lcCInvRef   , lcPriority , lcPaymeth  ,"+;
  "lcOVendCod , lcOldVnCmp  , lcOldDivDs , lcOldDvCod ,"+;
  "lcOldPrty  , lcOldpymth  , lcOMethod  , lcTThrou   ,"+;
  "lcTFrom    , lcTBlnkCode , lcTInvNo   , lcTPrior   ,"+;
  "lcTPayMeth , lcTAprPay   , lcTAprDisc , lcTAprAdj  ,"+;
  "lcTInvDate , lcTDivDes   , lcTOpnAmnt , "+;
  "lcBrowStr   , lcOldVal   , lcObjectSt ,"+;
  "lcObjDisp  , puDiv       , puDivision , lcMarker   ,"+;
  "lcTCurrCode, lcTCurRate  , lcCurrCode , lcOldCurr  ,"+;
  "lcExSin1   , lcExSin2    , lcExSin3   , lcExSin4   ,"+;
  "lcExSin5   , lcExSin6    , lcCurrSmbl , lcBaseSmbl"
=lfPopPropArray(loFormSet,'laPropArr',lcVars, " "    )

lcVars = "lcVendrCod , lcCurCod"
=lfPopPropArray(loFormSet,'laPropArr',lcVars, " "    )

=lfPopPropArray(loFormSet,'laPropArr','lcCurrCode', oAriaApplication.BaseCurrency)
=lfPopPropArray(loFormSet,'laPropArr','lnTime',_DBLCLICK+SECONDS() )
=lfPopPropArray(loFormSet,'laPropArr','lcDivCode', "*"          )
=lfPopPropArray(loFormSet,'laPropArr','lcTAll            ', "All"        )
=lfPopPropArray(loFormSet,'laPropArr','lcVendTtl', "Vendor"    )
=lfPopPropArray(loFormSet,'laPropArr','laCtrStat   ', "DISABLE"    )
=lfPopPropArray(loFormSet,'laPropArr','lcBrowFile', "APINVHDR"   )
=lfPopPropArray(loFormSet,'laPropArr','lcCodeFilt', "CDIVISION " )
=lfPopPropArray(loFormSet,'laPropArr','lcColPair,lcColor', SCHEME(1,2)  )
=lfPopPropArray(loFormSet,'laPropArr','lcEscTrp    ', ""           )
=lfPopPropArray(loFormSet,'laPropArr','lcBankCode,lcOldBank', SPACE(8)     )
=lfPopPropArray(loFormSet,'laPropArr','lcCheckcode,lcOldCheck', SPACE(12)    )
=lfPopPropArray(loFormSet,'laPropArr','lcInvRef,lcOldInvRf', SPACE(15)    )
=lfPopPropArray(loFormSet,'laPropArr','lcBrTtl', "Invoices"   )
=lfPopPropArray(loFormSet,'laPropArr','lcInvoice', "Invoice"   )
=lfPopPropArray(loFormSet,'laPropArr','lcBnkCod', SPACE(8)     )    &&<> this variable is used only in the lcExpression,
&&   which would be not more used , instead, append
&&   to the temp file when we in the edit mode
=lfPopPropArray(loFormSet,'laPropArr','lcChekCod', SPACE(12)    )   && same as lcBnkCod

*- Get multi currency settings
lcVars = "llMultiCr, llEditEx, llAddRate"
=lfAddProp(loFormSet,lcVars,.F.)

lnOldVal = 0
WITH loFormSet
  IF gfGetMemVar('LLMulCurr')
    .llMultiCr   = .T.
    .llEditEx    = gfGetMemVar('LLEDITEXRA')
    .llAddRate   = gfGetMemVar('LLEXCHRATE')
  ELSE
    STORE .F. TO .llEditEx, .llMultiCr, .llAddRate
  ENDIF
ENDWITH

*E303014,1 TMI 01/03/2012 [Start] do not use this technique in browse, instead call the gfBrows directly
*** Array laFields is used by lfSekVnd() and formed as follows :
*** A row for every field to be browsed
*** Column 1 : the object name corresponding to the field
*** Column 2 : the physical field name
*** Column 3 : the tag to be used in ssearching

*Old : laInvAdt[1]
=lfAddProp(loFormSet,'laFields[1]')
WITH loFormSet
  DECLARE .laFields[3,3]
  .laFields[1,1] = 'lcVendCode'         &&'lcVendCode'
  .laFields[1,2] = 'cVendCode'
  .laFields[1,3] = 'VENCODE'
  .laFields[2,1] = 'lcVendComp'         && 'lcVenComp'
  .laFields[2,2] = 'cVenComp'
  .laFields[2,3] = 'VENCOMP'
  .laFields[3,1] = 'lcVPhone'           &&'lcPhoneNo'
  .laFields[3,2] = 'cPhoneNo'
  .laFields[3,3] = 'VENPHONE'
ENDWITH
*E303014,1 TMI 01/03/2012 [End  ]

*** Get current year form parent company.
lcCurYear = ' '
SELECT SYCCOMP
LOCATE FOR CCOMP_ID = oAriaapplication.PRntcompanyid
IF FOUND()
  lcCurYear = CCURR_YER
ENDIF
=lfAddProp(loFormSet,'lcCurYear',lcCurYear)

*<>
*** Get payment methods array
=lfAddProp(loFormSet,'laPayMeth[1]' ,'')   && Array to hold payment methods
DIMENSION laPayMeth[1,2]
lnPayMLen = gfGetVld('CVENPMETH',@laPayMeth,.T.)

lnAryPos = ASCAN(laPayMeth,'C',1)
IF lnAryPos > 0
  =ADEL(laPayMeth,ASUBSCRIPT(laPayMeth, lnAryPos, 1))
  DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]
ENDIF

IF loFormSet.lnPyChMN <> 3 && If the program not called from cash payments bar
  lnAryPos = ASCAN(laPayMeth,'H',1)
  IF lnAryPos > 0
    =ADEL(laPayMeth,ASUBSCRIPT(laPayMeth, lnAryPos, 1))
    DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]
  ENDIF
ENDIF

DIMENSION loFormset.laPayMeth[ALEN(laPayMeth,1),ALEN(laPayMeth,2)]
=ACOPY(laPayMeth,loFormset.laPayMeth)

=lfPopPropArray(loFormSet,'laPropArr','lcPayMeth', laPayMeth[1,1] ) && Variable to hold method.
=lfPopPropArray(loFormSet,'laPropArr','lcMethod' , laPayMeth[1,2] ) && Variable to hold method code.
=lfAddProp(loFormSet,'lcTBlnkCode' ,gfCodDes(' ',' '))

*-Old : lcPayMeth = laPayMeth[1,1]  && Variable to hold method.
*-Old : lcMethod  = laPayMeth[1,2]  && Variable to hold method code.


*-Old : IF _WINDOWS


*-Old :   puDiv = CODES.cdiscrep
*-Old :   DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
*-Old :   FROM 9.00,12.88 TO 13.75,43.15;
*-Old :   MESSAGE gfObj_msg()


*-Old :   ON SELECTION POPUP puDivision DO lfvDivision
*-Old : ENDIF

*<> from here

*** Get names to Brawse and Read window ***

*E303014,1 TMI 01/03/2012 [Start] comment this , collect data into a temp file using the OG filtering
*<<	IF !WEXIST(gcBaseWind)
*<<
*<<	  lcObjectSt  = 'DISABLE' *<>

*<<	  *E300643,1 Change this line for the changes we have made
*<<	  *          to (gfCodDes) [Begin]
*<<	  *lcTBlnkCode = gfCodDes(' ')
*<<	  lcTBlnkCode = gfCodDes(' ' , ' ')
*<<	  *E300643,1 Change this line [End]
*<<
*<<	  *** Get the active session number. ***

*<<	  *E300663,1 Change this line for the changes we have
*<<	  *          made to (gfSequence) [Begin]
*<<	  *lcSesNum=PADL(gfSequence("APSESS",1,gcAct_Comp),8,'0')
=lfAddProp(loFormSet,'lcSesNum' ,gfSequence('CAPSESSNO'))
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*  loFormSet.Ariaform1.Caption = IIF(loFormSet.lnPyChMN=1,'Manual Check Payment',;
*!*                                IIF(loFormSet.lnPyChMN=2,'Non-Check Payment',;
*!*                                                         'Cash Payment'))+;
*!*                                '      Session : '+loFormSet.lcSesNum
loFormSet.Ariaform1.CAPTION = IIF(loFormSet.lnPyChMN=1,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_MANUALCHECKPAY,loFormSet.GetHeaderText("LANG_APMNCHP_MANUALCHECKPAY",loFormSet.HeaderAlias)) ,;
  IIF(loFormSet.lnPyChMN=2,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_NONCHECKPAYMENT,loFormSet.GetHeaderText("LANG_APMNCHP_NONCHECKPAYMENT",loFormSet.HeaderAlias)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_CASHPAYMENT,loFormSet.GetHeaderText("LANG_APMNCHP_CASHPAYMENT",loFormSet.HeaderAlias))))+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_SESSION,loFormSet.GetHeaderText("LANG_APMNCHP_SESSION",loFormSet.HeaderAlias)) +loFormSet.lcSesNum
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
*<<	  *E300663,1 Change this line [End]
*<<
*B610311,1 TMI 04/17/2013 [Start] no need for this property, a variable assigned on fly replaces its purpose
*** Variable holding Filter expresion ***
*=lfAddProp(loFormSet,'lcExprsion','(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ<>0)')
*WITH loFormSet
*  IF .lnPyChMN = 3 && If the program called from cash payments bar
*    .lcExprsion = .lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
*  ELSE
*    .lcExprsion = .lcExprsion + ".AND. CVENPMETH <> 'H' .AND." +"CBNKCODE+CCHKACCT=lcBnkCod+lcChekCod"
*  ENDIF
*ENDWITH
*B610311,1 TMI 04/17/2013 [End  ]
*<<	    *B601217,1 Change this line to use the new varibles [End]

*<<	    *** Prepare an array to hold bank objects to be used for
*<<	    *** global bank and checking accounts validations as follows :
*<<	    *** One row for every object, such that
*<<	    *** row no. 1 holds bank object names,
*<<	    *** row no. 2 holds checking account object names
*<<	    *** row no. 3 holds the corresponding G/L account object names,(optional)
*<<	    *** Columns are ordered as follows :
*<<	    *** Column no. 1 : invisible button name for corresponding object
*<<	    *** Column no. 2 : object name (e.g. bank object name)
*<<	    *** Column no. 3 : object description name(if required)
*<<
*<<	    laBankObjs      = ' '
*<<	    laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
*<<	    laBankObjs[1,2] = 'lcBankCode'     && Bank code
*<<	    laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
*<<	    laBankObjs[2,2] = 'lcCheckCode'    && Checking account
*<<
*<<	  ENDIF
*<<
*<<	  SELECT APINVHDR
*<<	  SET FILTER TO CVENDCODE + CINVNO = ' *' && Filter return no record.
*<<	  lcBrowName = gfTempName()
*<<	  lcReadNam1 = gfTempName()
*<<	  lcReadNam2 = gfTempName()
*<<	  lcVenSum   = gfTempName()
*<<	  lcVenSum   ="CWR"+SUBSTR(lcVenSum,4)
*<<	  lcInvSum   = gfTempName()
*<<	  lcInvSum   ="CWR"+SUBSTR(lcInvSum,4)

*<<	ELSE && If window exist
*<<	  SELECT APINVHDR
*<<	  lcSavOrd = ORDER()
*<<	  SET ORDER TO 0

*<<	  *B601217,1 Change this line to use the new varible lcVendrCod [Begin]
*<<	  *SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendCode .AND.;
*<<	  *              (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
*<<	  *              CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'.AND.;
*<<	  *              EVALUATE(lcExprsion)

*<<	  SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendrCod  .AND.;
*<<	                (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
*<<	                CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'.AND.;
*<<	                EVALUATE(lcExprsion)

*<<	  *B601217,1 Change this line to use the new varible lcVendrCod [End]

*<<	  SET ORDER TO &lcSavOrd

*<<	  IF _DOS && Return the popup values
*<<	    lcPayMeth = lcOldpymth
*<<	    lcDivision = lcOldDivDs
*<<	  ENDIF
*<<	  lcMethod   = lcOMethod
*<<	  rbScope    = lnOScope
*<<	  IF rbScope = 1
*<<	    ***Exprsion in apinvhdr file to be filtered.***
*<<	    lcExprsion = "(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ<> 0)"
*<<	    IF lnPyChMN = 3 && If the program called from cash payments bar
*<<	      lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
*<<	    ELSE
*<<	      lcExprsion = lcExprsion + ".AND. CVENPMETH <> 'H' .AND." +;
*<<	                   "CBNKCODE+CCHKACCT=lcBnkCod+lcChekCod"
*<<	    ENDIF
*<<	  ELSE
*<<	    =lfvOk() && returning the existing browse filter.
*<<	  ENDIF
*<<	ENDIF
*E303014,1 TMI 01/03/2012 [End  ]


*** fill the array holding frist and last object in base window and each children
*!*	laWndObj [1,1] = lcReadNam1
*!*	laWndObj [1,2] = "ibVend"
*!*	laWndObj [1,3] = "pbClose"

*!*	laWndObj [2,1] = lcReadNam2
*!*	laWndObj [2,2] = "ibVend"
*!*	laWndObj [2,3] = "pbClose"

*!*	laWndObj [3,1] = lcVenSum
*!*	laWndObj [3,2] = "PBVECLOSE"
*!*	laWndObj [3,3] = "PBVECLOSE"

*!*	laWndObj [4,1] = lcInvSum
*!*	laWndObj [4,2] = "PBINCLOSE"
*!*	laWndObj [4,3] = "PBINCLOSE"

*** store scape trap.
*-Old : lcEscTrp=ON("KEY","ESC")

*** Trab some keys before browse. ***
*-Old : =lfPushKey()

*** Get Codes from codes file.

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
=lfAddProp(loFormSet,'lcCodeFilt','CDIVISION ')
lcCodeFilt = loFormSet.lcCodeFilt
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'&lcCodeFilt') OR;
  (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A') OR;
  (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'ALL')
*E300789,4  AMM end


*** Set relation between invoice header, vendor, and division file ***
SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APDIV
SET ORDER TO TAG DIVISION

SELECT APVENHST
SET ORDER TO TAG VENDYEAR

SELECT APINVHDR

SET RELATION TO APINVHDR.CVENDCODE INTO APVENDOR ADDITIVE
SET RELATION TO APINVHDR.CDIVISION INTO APDIV ADDITIVE
SET RELATION TO APINVHDR.CVENDCODE+ APINVHDR.CFISFYEAR INTO APVENHST ADDITIVE

*E300683,1 Call *.SPR from screens directory
* DO APMNCHP.SPR
*-Old : DO (gcScrDir + gcWinAppl + '\APMNCHP.SPR') *<>
*E300683,1 end

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
*-Old : SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*-Old : SET FILTER TO

*-Old : SELECT APINVHDR
*** Clear relation ***
*-Old : SET FILTER TO
*-Old : SET RELATION TO

*** restores on key labels that were placed on the stack with push key. ***
*-Old : POP KEY

*-Old : RELEASE PAD _BROWSE OF _MSYSMENU
***Release the browse window.
*-Old : RELEASE WINDOW (lcBrTtl)

*-Old : IF glQuitting
*-Old :   HIDE WINDOW(gcBaseWind)
*-Old :   RELEASE WINDOW (lcVenSum)
*-Old :   RELEASE WINDOW (lcInvSum)
*-Old :   RELEASE POPUPS puDivision
*-Old : ENDIF

*-Old : glFromBrow  = .F.


lcVars = "lnVnTotPay , lnTotPay    , lnTotDisc  , lnTotAdj   ,"+;
  "lnTot1099  , lnAprToPay  , lnAprDisc  , lnAprAdj   ,"+;
  "lnOldAprTPy, lnOldAprDisc, lnOldAprAdj "
=lfAddProp(loFormSet,lcVars, 0.00 )

=lfAddProp(loFormSet,"lcPeriod,lcYear", " " )

*- End of lfDefineVars.
*!**************************************************************************
*!
*!      Function: lfGetPayMeth
*!
*!**************************************************************************
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[Start]
*FUNCTION lfGetPayMeth
FUNCTION lfGetPAYPayMeth
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[End]
PARAMETER loFormSet
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][Start]
SET DATASESSION TO loFormSet.DataSessionID
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][End]

lcAPINVHDR = loFormSet.lcAPINVHDR
lcPayCode = &lcAPINVHDR..CVENPMETH


lnposition= ASCAN(loFormSet.laPayMeth,lcPayCode,1)
IF lnposition <>  0
  RETURN loFormSet.laPayMeth[ASUBSCRIPT(loFormSet.laPayMeth, lnposition, 1),1]
ELSE
  RETURN " "
ENDIF
*!**************************************************************************
*Name       : lfvSelect
*Developer  : TMI
*Date       : <Dt>
*Purpose    : Select or unselect certain invoice.
*!**************************************************************************
FUNCTION lfvSelect
PARAMETERS loFormSet
LOCAL lnSlct,lcAPINVHDR
lcAPINVHDR = loFormSet.lcAPINVHDR
lnSlct = SELECT(0)
=SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')

*! B611744,1 Es 03/14/2019 Non-Check/Cash/Manual Payment screens does not check the invoice status (paid field) before locking it to be paid when user select or select all [Start]
IF !&lcAPINVHDR..LSELECT 
  IF (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ = 0) 
    =gfModalGen("TRM04075B00000","DIALOG",ALLTRIM(&lcAPINVHDR..CINVNO)+'|'+ALLTRIM(&lcAPINVHDR..CVENDCODE))
	    SELECT APINVHDR
	    SCATTER MEMO MEMVAR 
	    SELECT (loFormSet.lcAPINVHDR)
	    GATHER MEMO MEMVAR
    RETURN 
  ENDIF
  IF !(ABS(APINVHDR.NINVAMTAP)+ABS(APINVHDR.NINVDISAP)+ABS(APINVHDR.NINVADJAP)>0)
    =gfModalGen("TRM04084B00000","DIALOG","/ "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Invoice,loFormSet.GetHeaderText("LANG_APMNCHP_Invoice",loFormSet.HeaderAlias))+": "+ALLTRIM(&lcAPINVHDR..CVENDCODE)+"/"+ALLTRIM(&lcAPINVHDR..CINVNO))
	    SELECT APINVHDR
	    SCATTER MEMO MEMVAR 
	    SELECT (loFormSet.lcAPINVHDR)
	    GATHER MEMO MEMVAR	    
    RETURN 
  ENDIF
ENDIF
*! B611744,1 Es 03/14/2019 Non-Check/Cash/Manual Payment screens does not check the invoice status (paid field) before locking it to be paid when user select or select all [End]


IF !&lcAPINVHDR..LSELECT
  SELECT APINVHDR
  *!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [Start]
  *IF ALLTRIM(cLok_User) == ALLTRIM(oAriaApplication.User_ID) .OR. gfObj_lock(.T.) && lock the pointed record.
  IF lfObj_lock(.T.) && lock the pointed record.
    *!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [End]
    SELECT(loFormSet.lcAPINVHDR)
    REPLACE LSELECT WITH .T.
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
  ENDIF
ELSE
  SELECT APINVHDR
  gfObj_lock(.F.)
  SELECT (loFormSet.lcAPINVHDR)
  REPLACE LSELECT WITH .F.
  SKIP -1
  IF BOF()
    GO TOP
  ENDIF
ENDIF
WITH loFormSet.Ariaform1.grdAPINVHDR
  .AFTERROWCOLCHANGE()
  .REFRESH()
ENDWITH
SELECT(lnSlct)
*- End of lfvSelect
*!**************************************************************************
*!
*!      Function: lfvSelAll
*!
*!**************************************************************************
* Select all invoices.
*
FUNCTION lfvSelAll
PARAMETERS loFormSet
LOCAL lnRec,lnSlct
lnSlct = SELECT(0)
lcAPINVHDR = loFormSet.lcAPINVHDR
SELECT (loFormSet.lcAPINVHDR)
lnRec = RECNO()
LOCATE
SCAN FOR !LSELECT
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  
*! B611744,1 Es 03/14/2019 Non-Check/Cash/Manual Payment screens does not check the invoice status (paid field) before locking it to be paid when user select or select all [Start]
    IF (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ = 0) 
	    =gfModalGen("TRM04075B00000","DIALOG",ALLTRIM(&lcAPINVHDR..CINVNO)+'|'+ALLTRIM(&lcAPINVHDR..CVENDCODE))
	    SELECT APINVHDR
	    SCATTER MEMO MEMVAR 
	    SELECT (loFormSet.lcAPINVHDR)
	    GATHER MEMO MEMVAR
	    LOOP 
	  ENDIF
	  IF !(ABS(APINVHDR.NINVAMTAP)+ABS(APINVHDR.NINVDISAP)+ABS(APINVHDR.NINVADJAP)>0)
	    =gfModalGen("TRM04084B00000","DIALOG","/ "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Invoice,loFormSet.GetHeaderText("LANG_APMNCHP_Invoice",loFormSet.HeaderAlias))+" "+ALLTRIM(&lcAPINVHDR..CVENDCODE)+"/"+ALLTRIM(&lcAPINVHDR..CINVNO))
	    SELECT APINVHDR
	    SCATTER MEMO MEMVAR 
	    SELECT (loFormSet.lcAPINVHDR)
	    GATHER MEMO MEMVAR	    
	    LOOP  
	  ENDIF
*! B611744,1 Es 03/14/2019 Non-Check/Cash/Manual Payment screens does not check the invoice status (paid field) before locking it to be paid when user select or select all [End]
  
  SELECT APINVHDR
  *!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [Start]
  *IF gfObj_lock(.T.)
  IF lfObj_lock(.T.) && lock the pointed record.
    *!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [End]
    SELECT (loFormSet.lcAPINVHDR)
    REPLACE LSELECT WITH .T.
  ENDIF
ENDSCAN
GO lnRec
SELECT(lnSlct)
*- End of lfvSelAll.
*!**************************************************************************
*!
*!      Function: lfvSelNon
*!
*!**************************************************************************
* Unselect all selected invoices.
*Reviewed
FUNCTION lfvSelNon
PARAMETERS loFormSet
LOCAL lnRec,lnSlct
lnSlct = SELECT(0)
lcAPINVHDR = loFormSet.lcAPINVHDR
SELECT (loFormSet.lcAPINVHDR)
lnRec = RECNO()
LOCATE
SCAN FOR LSELECT
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  SELECT APINVHDR
  =gfObj_lock(.F.)
  SELECT (loFormSet.lcAPINVHDR)
  REPLACE LSELECT WITH .F.
ENDSCAN
GO lnRec
SELECT(lnSlct)
*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
* Select unselected invoices and unselect selected invoices.
FUNCTION lfvInvert
PARAMETERS loFormSet
LOCAL lnRec,lnSlct
lnSlct = SELECT(0)
lcAPINVHDR = loFormSet.lcAPINVHDR
SELECT (loFormSet.lcAPINVHDR)
lnRec = RECNO()
LOCATE
SCAN
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  IF LSELECT
    SELECT APINVHDR
    =gfObj_lock(.F.)
    SELECT (loFormSet.lcAPINVHDR)
    REPLACE LSELECT WITH .F.
  ELSE
    SELECT APINVHDR
    *! B612057,1 MMT 02/19/2020 Invert button allow overwriting locked invoice in another sesstion[T20200212.0008][Start]
    *IF gfObj_lock(.T.)
    IF lfObj_lock(.T.)
    *! B612057,1 MMT 02/19/2020 Invert button allow overwriting locked invoice in another sesstion[T20200212.0008][End]
      SELECT (loFormSet.lcAPINVHDR)
      REPLACE LSELECT WITH .T.
    ENDIF
  ENDIF
ENDSCAN
GO lnRec
SELECT(lnSlct)
*- End of lfvInvert.
*!**************************************************************************
*!
*!      Function: lfvPySlInv
*!
*!**************************************************************************
*Activate the Pay selected invoices screen.
FUNCTION lfvPySlInv
PARAMETERS loFormSet
LOCAL lnSlct,lnRec
lnSlct = SELECT(0)
lnRec = RECNO(loFormSet.lcAPINVHDR)

SELECT (loFormSet.lcAPINVHDR)
LOCATE
LOCATE FOR LSELECT

*- if there is no selected checks then don't do anything
IF EOF()
  *** Message :" No selected invoices to pay. "
  *** Choice  :"            < Ok >            "
  =gfModalGen("TRM04045B00000","DIALOG")
  GO lnRec IN (loFormSet.lcAPINVHDR)
  RETURN
ENDIF

WITH loFormSet.ariaform1
  lcBankCode  = .BankChk.kbBanks.Keytextbox.VALUE
  lcCheckCode = .BankChk.kbChkAccount.Keytextbox.VALUE
  lcVendCode =  .KBVendCode.Keytextbox.VALUE
ENDWITH

IF loFormset.lnPyChMN = 1  && Calling from manual check.
  ***Get the next manual check number from apchecks***
  SELECT APCHECKS
  IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
    *B800072,1 We do not need to lock the record because it is already lo
    *IF gfObj_lock(.T.) && lock the pointed record.
    *B132275,1  TMI [Start] use the local version of "gfObj_lock" function
    *IF (ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)) .OR. gfObj_lock(.T.) && lock the pointed record.
    *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
    *IF (ALLTRIM(cLok_User) == ALLTRIM(oAriaApplication.User_ID)) .OR. gfObj_lock(.T.) && lock the pointed record.
    IF .T.
      *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[END]
      SELECT (loFormSet.lcAPCHECKS)
      lcAPCHECKS = loFormSet.lcAPCHECKS
      lnOChkNum  = &lcAPCHECKS..NCHKNXTMN
      lnGnChkNm  = &lcAPCHECKS..NCHKNXTMN+1
      lnChkNum = lnOChkNum
      SELECT (loFormSet.lcAPCHECKS)
      REPLACE NCHKNXTMN WITH lnGnChkNm
      *Old : =gfAdd_Info()  && Add the audit information to the record.
    ELSE

      *B601553,1 Change this line to change the choice [Begin]
      *  *** Message :" The checking account lcBankCode-lcCheckCode is being edited "
      *  ***         :" Cannot generate the next check number. "
      *  *** Choice  :"                  < Retry >      < Cancel >            "
      *=gfModalGen("TRM04055B00015","DIALOG",ALLTRIM(lcBankCode)+'-'+ALLTRIM(lcCheckCode))

      *** Message :" The checking account lcBankCode-lcCheckCode is being edited "
      ***         :" Cannot generate the next check number. "
      *** Choice  :"                   < OK >               "
      =gfModalGen("TRM04055B00000","DIALOG",ALLTRIM(lcBankCode)+'-'+ALLTRIM(lcCheckCode))

      *B601553,1 Change this line to change the choice [End]

      *B601553,1 Add this line to return if the lock didnt work [Begin]
      SELECT APINVHDR
      RETURN
      *B601553,1 Add this line to return if the lock didnt work [End]

    ENDIF

  ENDIF
  SELECT APINVHDR
ENDIF

lnTotPay  = 0
lnTotDisc = 0
lnTotAdj  = 0
lnTot1099 = 0
=lfvTot(loFormSet) && Sun Selected Totamoount, tot_disc, tot_Agj,tot_1099


*oLD : IF lnArlen=1
*oLD :   *** Message :" No selected invoices to pay. "
*oLD :   *** Choice  :"            < Ok >            "
*oLD :   =gfModalGen("TRM04045B00000","DIALOG")
*oLD : ELSE

*B601553,1 Add this lines to check that the total approved payment is
*not greater than zero if so give the user a message and return [Begin]

*B601553,1 IF Statment to check if the total approved payment is not
*greater than zero
IF !lnTotPay > 0

  *B601553,4 Change these line to give the user the capability to
  *approves a negative amount [Begin]

  *** Message: " Approved payment for vendor ð must be greater than zero."
  *** Choices: "                           < Ok >                        "
  *=gfModalGen("INM04084B00000" , "DIALOG" , ALLTRIM(lcVendCode))

  *** Message: "Approved payment for vendor ð is less than or equal zero."
  *** Choices: "                < Ok >       < Cancel >                  "
  lnUserRep = gfModalGen("INM04084B04004" , "DIALOG" ,;
    ALLTRIM(lcVendCode))

  *B601553,4 Change these line to give the user the capability [End]

  *B601553,4 Add these IF condition to give the user the capability to
  *approves a negative amount [Begin]

  *B601553,4 If the user selected to cancel the process
  IF lnUserRep = 2

    *B601553,4 Add these IF condition to give the user the capability [End]

    IF loFormset.lnPyChMN = 1 .AND. !EOF('APCHECKS')
      lcSavAlias = ALIAS()
      SELECT APCHECKS
      =gfObj_lock(.F.)
      SELECT (lcSavAlias)
    ENDIF
    RETURN

    *B601553,4 Add these IF condition to give the user the capability to
    *approves a negative amount [Begin]
  ENDIF    && End of IF lnUserRep = 2
  *B601553,4 Add these IF condition to give the user the capability [End]

ENDIF    && End of IF
*B601553,1 Add this lines [End]

*E300296,1 M.H 02/04/96 Define a new variable to hold the symbol of the approved amount.
lcAPINVHDR = loFormSet.lcAPINVHDR
lcCurrSmbl = LOOKUP(SYCCURR.cCurrSmbl,&lcAPINVHDR..CAPRCURCOD,SYCCURR.cCurrCode,'CCURRCODE')
lcBaseSmbl = PADR(ALLTRIM(SET('CURRENCY','TO')),3,' ')
*E300296,1 M.H 02/04/96 End.

*B601013,1 Default the exchange rate with the payment date rate.
lnChkExUnt = 1
ldPayDat = oAriaApplication.systemdate
IF &lcAPINVHDR..CAPRCURCOD <> oAriaApplication.BaseCurrency
  IF EMPTY(ldPayDat)
    lnChkExRat  = 0
    lcRateDisp  = 'DISABLE'
  ELSE
    lcRateDisp  = IIF(loFormSet.llEditEx , 'ENABLE', 'DISABLE')
    lnChkExRat  = gfChkRate('lnChkExUnt',;
      &lcAPINVHDR..CAPRCURCOD,ldPayDat)
  ENDIF
ELSE
  lcRateDisp  = 'DISABLE'
  lnChkExRat  = 1
ENDIF
*B601013,1 Initialize old payment date.
ldOldPDat = {}
*B601013,1 end.

*** Release trap before branching to another screen. ***
*oLD : ON KEY
*oLD : RELEASE PAD _BROWSE OF _MSYSMENU
*oLD : RELEASE WINDOW (lcBrTtl)

*E300683,1 Call *.SPR from screens directory
* DO APYSLINV.SPR
DO FORM (oAriaApplication.ScreenHome+"\AP\APYSLINV.SCX") WITH loFormSet
*E300683,1 end

*** Push the same keys again ***
* Old : =lfPushKey()
* Old : =lfDispBrow()
* Old : IF _DOS AND !(llFromTrap)  &&if we came from brows window no need to define browse pad.
* Old :   =lfDefinePad()
* Old : ENDIF

*B801208,4 AMM start
*GO TOP              && Activate the browse
LOCATE
*B801208,4 AMM end

*oLD : lnBrRecNo  = RECNO('APINVHDR')
*oLD : IF ASCAN(laSelect,APINVHDR.CINVNO)<> 0
*oLD :   SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
*oLD : ELSE
*oLD :   SHOW GET pbSelect,1 PROMPT 'Se\<lect'
*oLD : ENDIF
*oLD : =lfShowDs(.F.,.T.)  && Disable object if there is no more approve records.
loFormSet.Ariaform1.grdAPINVHDR.AFTERROWCOLCHANGE()
loFormSet.Ariaform1.grdAPINVHDR.REFRESH()
*ENDIF

*B601553,1 Add this lines to unlock the APCHECKS record [Begin]
*!*  IF loFormset.lnPyChMN = 1 .AND. !EOF('APCHECKS')
*!*    lcSavAlias = ALIAS()
*!*    SELECT APCHECKS
*!*    =gfObj_lock(.F.)
*!*    SELECT (lcSavAlias)
*!*  ENDIF
*B601553,1 Add this lines to unlock the APCHECKS record [End]
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
=loFormSet.ChangeMode('E')
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]

*!**************************************************************************
*!
*!      Function: lfvProceed
*!
*!**************************************************************************
FUNCTION lfvProceed
PARAMETERS loPayFormSet
LOCAL loFormSet,lcAPINVHDR
loFormSet = loPayFormSet.loCallingForm

lcAPINVHDR = loFormSet.lcAPINVHDR
lcAPCHECKS = loFormSet.lcAPCHECKS

* Set a filter on lcAPINVHDR to not show paid invoices

llProcOk  = .T.
*lnArCount = ALEN(laSelect,1)

IF EMPTY(ldPayDat)
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *=gfModalGen("TRM04066B00000","DIALOG",'payment date')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen("TRM04066B00000","DIALOG",LANG_APMNCHP_PAYDATE)
  =gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_PAYDATE,loFormSet.GetHeaderText("LANG_APMNCHP_PAYDATE",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  _CUROBJ = OBJNUM(ldPayDat)
  loPayFormSet.Ariaform1.DtAppDate.Text1.SETFOCUS
  RETURN .F.
ELSE
  *B601013,1 Validate the date if it has not been validated before
  *!B610172,2 SAB 20/11/2012 Fix problem of not amend the exchnage rate that is added by the user [Start]
  *=lfvPayDate(loPayFormSet)
  *!B610172,2 SAB 20/11/2012 Fix problem of not amend the exchnage rate that is added by the user [End]
  IF EMPTY(ldPayDat)
    RETURN .F.
  ENDIF
ENDIF
*B601013,1 end.

*!*	*E301077,79 AMM Open file
*!*	=gfOpenFile(gcDataDir+'APPAYMNT','TYPMETHNO','SH')


** In case of calling bar is manual check payment or non check payment.
IF loFormSet.lnPyChMN <>  1
  IF EMPTY(lcPayNum)
    **  Message: "You have to enter The ð.       "
    **  Choices: "              ® Ok ¯           "
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *=gfModalGen("TRM04066B00000","DIALOG",'payment number')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen("TRM04066B00000","DIALOG",LANG_APMNCHP_MSG_PAYNUMBER)
    =gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_MSG_PAYNUMBER,loFormSet.GetHeaderText("LANG_APMNCHP_MSG_PAYNUMBER",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    *Old: _CUROBJ = OBJNUM(lcPayNum)
    loPayFormSet.Ariaform1.txtPayNum.SETFOCUS()
    RETURN  .F. && <-- quit function.
  ELSE
    SELECT APPAYMNT
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
    *IF SEEK('P'+'H'+SPACE(8)+SPACE(12)+lcPayNum) .OR.;
    SEEK('P'+'N'+lcBankCode+lcCheckCode+lcPayNum)
    IF SEEK('P'+'H'+SPACE(8)+SPACE(12)+PADR(ALLTRIM(lcPayNum),8),'APPAYMNT') .OR.;
        SEEK('P'+'N'+lcBankCode+lcCheckCode+PADR(ALLTRIM(lcPayNum),8),'APPAYMNT') .OR.;
        SEEK(SPACE(8)+SPACE(12)+'P'+'H'+PADR(ALLTRIM(lcPayNum),8),loFormSet.lcAPPAYMNT) .OR.;
        SEEK(lcBankCode+lcCheckCode+'P'+'N'+PADR(ALLTRIM(lcPayNum),8),loFormSet.lcAPPAYMNT)
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
      **  Message: " The payment number already exists. "
      **  Choices: "                 ® Ok ¯             "
      =gfModalGen("TRM04051B00000","DIALOG")
      *Old: _CUROBJ=OBJNUM(lcPayNum)
      loPayFormSet.Ariaform1.txtPayNum.SETFOCUS()
      RETURN .F. && <--- quit function.
    ENDIF
    SELECT APINVHDR
  ENDIF
ELSE  && in case of manual check.
  IF EMPTY(lnChkNum)
    **  Message: " You have to enter The ð.  "
    **  Choices: "             ® Ok ¯        "
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *=gfModalGen("TRM04066B00000","DIALOG",'Check number')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen("TRM04066B00000","DIALOG",LANG_APMNCHP_MSG_CHECKNUMBER)
    =gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_MSG_CHECKNUMBER,loFormSet.GetHeaderText("LANG_APMNCHP_MSG_CHECKNUMBER",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    lnChkNum=lnGnChkNm
    loPayFormSet.Ariaform1.txtPayNum.SETFOCUS()
    *Old: SHOW GET lnChkNum
    *Old: _CUROBJ = OBJNUM(lnChkNum)
    RETURN .F. && <--- quit function.
  ELSE
    IF lnChkNum < 0
      **  Message: " Negative values are not allowed. "
      **  Choices: "              ® Ok ¯              "
      =gfModalGen("TRM04087B00000","DIALOG")
      lnChkNum=lnOChkNum
      *Old: SHOW GET lnChkNum
      loPayFormSet.Ariaform1.txtPayNum.SETFOCUS()
      RETURN .F. && <--- quit function.
    ELSE
      SELECT APPAYMNT
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
      *IF SEEK('P'+'M'+lcBankCode+lcCheckCode+PADL(lnChkNum,8,'0'))
      IF SEEK('P'+'M'+lcBankCode+lcCheckCode+PADL(lnChkNum,8,'0'),'APPAYMNT') .OR.;
          SEEK(lcBankCode+lcCheckCode+'P'+'M'+PADL(lnChkNum,8,'0'),loFormSet.lcAPPAYMNT)
        *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
        **  Message: " A manual check already exists with this number."
        **           " You can not have duplicate manual check numbers"
        **           " for the same checking account.                 "
        **  Choices: "                        ® Ok ¯                  "
        =gfModalGen("TRM04053B00000","DIALOG")
        loPayFormSet.Ariaform1.txtPayNum.SETFOCUS()
        *Old: _CUROBJ=OBJNUM(lnChkNum)
        RETURN .F.  && <--- quit function.
      ELSE
        ** If check number changed manually add one to next check number.
        *Old : SELECT APCHECKS
        SELECT (loFormSet.lcAPCHECKS)
        IF lnChkNum <> lnOChkNum  && If generated check no changed.
          REPLACE NCHKNXTMN WITH lnChkNum+1
          *=gfAdd_Info()  && Add the audit information to the record.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

*B601013,1 Check if the exchange rate is 0.
*E300296,1 M.H Add the currency to the AP module.
*E300296,1 M.H Check the Checking account currency rate with oAriaApplication.BaseCurrency with the proceed date.
*lnChkExUnt = 0
*lnChkExRat = 0
*IF gfGetMemVar('LLMULCURR')
*  lnChkExRat = gfChkRate('lnChkExUnt',APINVHDR.CAPRCURCOD,ldPayDat,.T.,.F.)
IF lnChkExRat = 0
  lnChkExRat  = gfChkRate('lnChkExUnt',;
    &lcAPINVHDR..CAPRCURCOD,ldPayDat, .T.,;
    oAriaapplication.Activecompanyid, oAriaApplication.BaseCurrency, .T.)
  IF lnChkExRat = 0
    IF !loFormset.llAddRate
      ** MESSAGE : " A valid ð to ð exchange rate could not "
      **           " be found on ð.                         "
      **           "                  ® Ok ¯                "
      =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(&lcAPINVHDR..CAPRCURCOD)+'|'+ALLTRIM(oAriaApplication.BaseCurrency)+'|'+DTOC(ldPayDat))
    ENDIF
    llProcOk  = .F.
    *IF lnPyChMN <> 3
    *_CUROBJ=OBJNUM(ldPayDat)
    loPayFormset.Ariaform1.DtAppDate.Text1.SETFOCUS()
    *ELSE
    *_CUROBJ=OBJNUM(lcCurrCode)
    *ENDIF
    RETURN .F.
  ENDIF
  *Old: SHOW GET lnChkExRat
ENDIF
*ELSE
*  lnChkExUnt = 1
*  lnChkExRat = 1
*ENDIF
*E300296,1 M.H End.
*B601013,1

LOCAL lnCntSlscted
lnCntSlscted = 0
SELECT (loFormSet.lcAPINVHDR)
LOCATE
COUNT FOR LSELECT TO lnCntSlscted
LOCATE

SELECT (loFormset.lcAPINVHDR)
SCAN FOR LSELECT
  *Old: FOR lnRecCount= 1 TO lnArCount-1
  *Old:   GO laSelect[lnRecCount,2] && go to the seleced record.
  ** If invoice date is greater than the payment date. **
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  =SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')

  *B604453,1 Amin, Change the validation to include the posting date [Start]
  *IF APINVHDR.DINVDATE > ldPayDat
  IF &lcAPINVHDR..DINVDATE > ldPayDat OR &lcAPINVHDR..DPOSTDATE > ldPayDat
    *B604453,1 Amin, Change the validation to include the posting date [End]

    **  Message: " Invoice xxxx date is greater than the payment date.  "
    **             All selected invoices should have dates less than or "
    **             equal to payment date.                               "
    **  Choices: "                          ® Ok ¯                      "
    lcMessage = ALLTRIM(&lcAPINVHDR..CINVNO)

    *B604453,1 Amin, Change the Message [Start]
    *=gfModalGen("TRM04046B00000","DIALOG",lcMessage)
    =gfModalGen("TRM04198B00000","DIALOG",lcMessage)
    *B604453,1 Amin, Change the Message [End]
    llProcOk  = .F.
    *Old: _CUROBJ=OBJNUM(ldPayDat)
    loPayFormset.Ariaform1.DtAppDate.Text1.SETFOCUS()
    EXIT
    *B600580,1 start
    *B600580,1 no need to lock again as the record is alredy locked
    *  ELSE && if invoice date is smaller than or equal the payment date.
    *    IF ! gfObj_lock(.T.) && lock the pointed record.
    *      llProcOk =.F.
    *      EXIT
    *    ENDIF
    *B600580,1 end
  ENDIF
  *Old: ENDFOR
ENDSCAN

*B604733,1 Get the equivalent exchange rate. [Begin]
PRIVATE lnExchUnit , lnExchRate
STORE 0 TO lnExchUnit , lnExchRate
*Old: IF gfGetMemVar('LLMULCURR')
IF loFormSet.llMultiCr
  lnExchRate = gfChkRate('lnExchUnit',ApInvHdr.CAPRCURCOD,ldPayDat,.T.,.F.)
  IF lnExchRate = 0
    STORE 1 TO lnExchUnit , lnExchRate
  ENDIF
ELSE
  STORE 1 TO lnExchUnit , lnExchRate
ENDIF
*B604733,1 Get the equivalent exchange rate. [End]

IF llProcOk && If valid date and locking ok.
  lcAPINVHDR = loFormset.lcAPINVHDR
  SELECT (loFormset.lcAPINVHDR)
  LOCATE FOR LSELECT

  SELECT (loFormset.lcAPPAYMNT)
  APPEND BLANK
  *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
  *REPLACE CPAYTYPE   WITH "P",;
  DPAYDATE   WITH ldPayDat,;
  CFISFYEAR  WITH loFormSet.lcYear,;
  CFSPPRDID  WITH loFormSet.lcPeriod,;
  CPAYCLNO   WITH lcVendCode,;
  NPAYAMNT   WITH 0,;
  NPAYDISC   WITH 0,;
  NPAYADJ    WITH 0,;
  NINV1099A  WITH 0,;
  CPAYRECST  WITH 'O';
  CBNKCODE   WITH lcBankCode,;
  CCHKACCT   WITH lcCheckCode,;
  CPAYMETH   WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
  CPAYCOMP   WITH IIF(lnCntSlscted = 1,&lcAPINVHDR..COUTCOMP,APVENDOR.CVENCOMP),;
  CPAYDOCNO  WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
  CCURRCODE  WITH &lcAPINVHDR..CAPRCURCOD,;
  NEXRATE    WITH lnChkExRat,;
  NCURRUNIT  WITH lnExchUnit
  REPLACE CPAYTYPE   WITH "P",;
    DPAYDATE   WITH ldPayDat,;
    CFISFYEAR  WITH loFormSet.lcYear,;
    CFSPPRDID  WITH loFormSet.lcPeriod,;
    CPAYCLNO   WITH lcVendCode,;
    NPAYAMNT   WITH 0,;
    NPAYDISC   WITH 0,;
    NPAYADJ    WITH 0,;
    NINV1099A  WITH 0,;
    CPAYRECST  WITH 'O';
    CBNKCODE   WITH lcBankCode,;
    CCHKACCT   WITH lcCheckCode,;
    CPAYMETH   WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
    CPAYCOMP   WITH IIF(lnCntSlscted = 1,&lcAPINVHDR..COUTCOMP,APVENDOR.CVENCOMP),;
    CPAYDOCNO  WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADR(ALLTRIM(lcPayNum),8)),;
    CCURRCODE  WITH &lcAPINVHDR..CAPRCURCOD,;
    NEXRATE    WITH lnChkExRat,;
    NCURRUNIT  WITH lnExchUnit
  *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[End]

  IF loFormSet.lnPyChMN = 1     && Case calling bar is Manual check payment.
    SELECT (loFormSet.lcAPCHECKS)
    REPLACE DCHKLMDAT WITH ldPayDat,;
      NCHKLMAMT WITH 0
  ENDIF

  *Old: =gfAdd_Info()  && Add the audit information to the record.

  SELECT (loFormSet.lcAPVENDOR)
  REPLACE NVENLPAYA WITH 0  && Init the last payment amount.


  ** Do process for all selected invoices. **
  *Old: FOR lnRecCount= 1 TO lnArCount-1
  *Old:   GO laSelect[lnRecCount,2] && go to the seleced record.
  oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
  oProgress.TotalProgress = lnCntSlscted
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *  oProgress.lblFirstLabel.CAPTION = 'Approve All'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *oProgress.lblFirstLabel.CAPTION = LANG_APMNCHP_APPROVE_ALL
  oProgress.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_APPROVE_ALL,loFormSet.GetHeaderText("LANG_APMNCHP_APPROVE_ALL",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  lnThermNo = 0
  oProgress.SHOW()

  SELECT (loFormSet.lcAPINVHDR)
  SCAN FOR LSELECT
    =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
    =SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')

    lnAprPayB  = 0
    lnAprDisB  = 0
    lnAprAdjB  = 0
    lnApr1099B = 0

    *E300316,1 HISH  11/30/95. Got the equation signs. (Begin)
    *E300316,1 to exchange currency from invoice currency to company base currency.
    *E300316,1 HISH 01/08/96  Passed pointer parameter to get Unit sgin. (Begin)
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,cCurrCode)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')

    *E300316,1 to exchange currency from invoice currency to approved currency.
    lcExSin4 = ' '
    lcExSin6 = ' '
    lcExSin5 = gfGetExSin(@lcExSin6,cAprCurCod)
    *lcExSin6 = IIF(lcExSin5 = '*','/','*')
    *E300316,1 (End)

    *Old: =gfThermo(lnArCount-1,lnRecCount,"Paying selected invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
    lnAprPayB  = ROUND(&lcAPINVHDR..NINVFAAP &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    *B601526,1 Change this line [End]

    *lnAprDisB  = ROUND(APINVHDR.NINVDISAP * APINVHDR.NEXRATE/APINVHDR.NCURRUNIT,2)
    lnAprDisB  = ROUND(&lcAPINVHDR..NINVDISAP &lcExSin1 &lcAPINVHDR..NEXRATE &lcExSin2 &lcAPINVHDR..NCURRUNIT,2)

    *lnAprAdjB  = ROUND(APINVHDR.NINVADJAP * APINVHDR.NEXRATE/APINVHDR.NCURRUNIT,2)
    lnAprAdjB  = ROUND(&lcAPINVHDR..NINVADJAP &lcExSin1 &lcAPINVHDR..NEXRATE &lcExSin2 &lcAPINVHDR..NCURRUNIT,2)

    *lnApr1099B = ROUND(APINVHDR.NINVA1099 * APINVHDR.NEXRATE/APINVHDR.NCURRUNIT,2)
    lnApr1099B = ROUND(&lcAPINVHDR..NINVA1099 &lcExSin1 &lcAPINVHDR..NEXRATE &lcExSin2 &lcAPINVHDR..NCURRUNIT,2)
    *E300316,1 (End)

    lcAPPAYMNT = loFormSet.lcAPPAYMNT
    REPLACE &lcAPPAYMNT..NPAYAMNT  WITH &lcAPPAYMNT..NPAYAMNT  +;
      ROUND(&lcAPINVHDR..NINVFAAP , 2),;
      &lcAPPAYMNT..NPAYDISC  WITH &lcAPPAYMNT..NPAYDISC  + lnAprDisB,;
      &lcAPPAYMNT..NPAYADJ   WITH &lcAPPAYMNT..NPAYADJ   + lnAprAdjB,;
      &lcAPPAYMNT..NINV1099A WITH &lcAPPAYMNT..NINV1099A + lnApr1099B

    *B601526,1 Change this line [End]

    *E300316,1 (End)

    *Old: =gfAdd_Info('APPAYMNT')  && Add the audit information to the record.

    **  Updating records in vendor file.  **
    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations.
    lcAPVENDOR = loFormset.lcAPVENDOR
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
    *REPLACE &lcAPVENDOR..DVENLPAYD WITH ldPayDat,;
    &lcAPVENDOR..NVENLPAYA WITH &lcAPVENDOR..NVENLPAYA + lnAprPayB,;
    &lcAPVENDOR..NVEN1099B WITH &lcAPVENDOR..NVEN1099B + lnApr1099B,;
    &lcAPVENDOR..NVENOPNDR WITH &lcAPVENDOR..NVENOPNDR + ROUND(IIF(&lcAPINVHDR..NINVAMNT < 0,;
    &lcAPINVHDR..NINVAMTAP + &lcAPINVHDR..NINVDISAP + &lcAPINVHDR..NINVADJAP;
    &lcExSin1 &lcAPINVHDR..NEXRATE &lcExSin2 &lcAPINVHDR..NCURRUNIT,0),2),;
    &lcAPVENDOR..NVENBAL   WITH &lcAPVENDOR..NVENBAL   -;
    ROUND((&lcAPINVHDR..NINVAMTAP+&lcAPINVHDR..NINVDISAP+&lcAPINVHDR..NINVADJAP);
    &lcExSin1 &lcAPINVHDR..NEXRATE &lcExSin2 &lcAPINVHDR..NCURRUNIT,2),;
    &lcAPVENDOR..NVENCPAY  WITH &lcAPVENDOR..NVENCPAY  + lnAprPayB,;
    &lcAPVENDOR..CVENLPAYN WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,FSIZE('CVENLPAYN','APVENDOR'),'0'),PADL(lcPayNum,FSIZE('CVENLPAYN','APVENDOR'),'0'))

    REPLACE &lcAPVENDOR..DVENLPAYD WITH ldPayDat,;
      &lcAPVENDOR..NVENLPAYA WITH &lcAPVENDOR..NVENLPAYA + lnAprPayB,;
      &lcAPVENDOR..NVEN1099B WITH &lcAPVENDOR..NVEN1099B + lnApr1099B,;
      &lcAPVENDOR..NVENOPNDR WITH &lcAPVENDOR..NVENOPNDR + ROUND(IIF(&lcAPINVHDR..NINVAMNT < 0,;
      &lcAPINVHDR..NINVAMTAP + &lcAPINVHDR..NINVDISAP + &lcAPINVHDR..NINVADJAP;
      &lcExSin1 &lcAPINVHDR..NEXRATE &lcExSin2 &lcAPINVHDR..NCURRUNIT,0),2),;
      &lcAPVENDOR..NVENBAL   WITH &lcAPVENDOR..NVENBAL   -;
      ROUND((&lcAPINVHDR..NINVAMTAP+&lcAPINVHDR..NINVDISAP+&lcAPINVHDR..NINVADJAP);
      &lcExSin1 &lcAPINVHDR..NEXRATE &lcExSin2 &lcAPINVHDR..NCURRUNIT,2),;
      &lcAPVENDOR..NVENCPAY  WITH &lcAPVENDOR..NVENCPAY  + lnAprPayB,;
      &lcAPVENDOR..CVENLPAYN WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,FSIZE('CVENLPAYN','APVENDOR'),'0'),PADR(ALLTRIM(lcPayNum),8))
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
    *E300316,1 (End)    .

    *Old: =gfAdd_Info('APVENDOR')  && Add the audit information to the record.

    ** Updating records in apchecks file. **
    IF loFormSet.lnPyChMN = 1        && Case calling bar is Manual check payment.

      SELECT(loFormSet.lcAPCHECKS)
      REPLACE NCHKLMAMT WITH;
        NCHKLMAMT + ROUND(&lcAPINVHDR..NINVFAAP , 2)

      *B601526,1 Change this line [End]

      *E300316,1 (End)
      *Old: =gfAdd_Info('APCHECKS')  && Add the audit information to the record.
    ENDIF

    *B131145,1 EIH 03/20/2006 Fix bug of saving in wrong record of APVENHST file when we change
    *B131145,1 EIH 03/20/2006 The payment date to the next year.[Begin]
    IF !EMPTY(loFormSet.lcYear)
      =SEEK(&lcAPINVHDR..cvendcode+loFormSet.lcYear,'APVENHST')
    ENDIF
    *B131145,1 EIH 03/20/2006 [End]

    *    REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + APINVHDR.NINVDISAP,;
    APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + APINVHDR.NINVAMTAP,;
    APVENHST.NVNHADJ    WITH APVENHST.NVNHADJ    + APINVHDR.NINVADJAP
    lcAPVENHST = loFormSet.lcAPVENHST
    *B612189,1 Es 07/29/2020 Total Payment/ Year is not update correctly in APVENHST table when invoice is paid using Non-Check/Manual/Cash Payment screens. [T20200709.0009][Start]
 IF !EMPTY(loFormSet.lcYear)
      =SEEK(&lcAPINVHDR..cvendcode+loFormSet.lcYear,lcAPVENHST)
    ENDIF    
    *B612189,1 Es 07/29/2020 Total Payment/ Year is not update correctly in APVENHST table when invoice is paid using Non-Check/Manual/Cash Payment screens. [T20200709.0009][End]

    REPLACE &lcAPVENHST..NVNHDISTKN WITH &lcAPVENHST..NVNHDISTKN + lnAprDisB,;
      &lcAPVENHST..NVNHTOTPA  WITH &lcAPVENHST..NVNHTOTPA  + lnAprPayB,;
      &lcAPVENHST..NVNHADJ    WITH &lcAPVENHST..NVNHADJ    + lnAprAdjB

    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations.
    IF loFormSet.lnPyChMN = 1        && Case calling bar is Manual check payment.
      *REPLACE APVENHST.NVNHMCHKP WITH APVENHST.NVNHMCHKP + APINVHDR.NINVAMTAP
      REPLACE &lcAPVENHST..NVNHMCHKP WITH &lcAPVENHST..NVNHMCHKP + lnAprPayB
    ELSE
      IF loFormSet.lnPyChMN = 2      && Case calling bar is Non check payment.
        *REPLACE APVENHST.NVNHNCHKP WITH APVENHST.NVNHNCHKP + APINVHDR.NINVAMTAP
        REPLACE &lcAPVENHST..NVNHNCHKP WITH &lcAPVENHST..NVNHNCHKP + lnAprPayB
      ELSE                 && Case calling bar is Cash payment.
        *REPLACE APVENHST.NVNHCASHP WITH APVENHST.NVNHCASHP + APINVHDR.NINVAMTAP
        REPLACE &lcAPVENHST..NVNHCASHP WITH &lcAPVENHST..NVNHCASHP + lnAprPayB
      ENDIF
    ENDIF

    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations.
    IF !EMPTY(loFormSet.lcPeriod)
      lcfield="NVNHPAY"+ALLTRIM(STR(VAL(loFormSet.lcPeriod)))
      *REPLACE APVENHST.&lcfield WITH APVENHST.&lcfield + APINVHDR.NINVAMTAP
      REPLACE &lcAPVENHST..&lcfield WITH &lcAPVENHST..&lcfield + lnAprPayB
    ENDIF

    *Old: =gfAdd_Info('APVENHST')  && Add the audit information to the record.

    ** Add to Distrebution File **
    *E301077,79 AMM Open file
    *SELECT APDIST
    *Old: =gfOpenFile(gcDataDir+'APDIST','INVVEND','SH')
    *E301077,79 AMM end
    ** Add 1st record in distrebution file. **
    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations.

    SELECT (loFormSet.lcAPDIST)
    APPEND BLANK

    *E300296,1 M.H Add the currency to the AP module.
    * Add the Eq. (APINVHDR.NINVAMTAP * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT + lnAprDisB + lnAprAdjB )
    * Add the inv Currency,Rate,Unit
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
    *REPLACE CVENDCODE WITH lcVendCode,;
    CINVNO    WITH &lcAPINVHDR..CINVNO,;
    DAPDTRDAT WITH ldPayDat,;
    LAPDPOST  WITH .F.,;
    CAPDGLACT WITH &lcAPINVHDR..CAPACCT,;
    NAPDAMNT  WITH (&lcAPINVHDR..NINVAMTAP+&lcAPINVHDR..NINVDISAP+&lcAPINVHDR..NINVADJAP),;
    CAPDACTID WITH "A",;
    CFISFYEAR WITH loFormSet.lcYear,;
    CFSPPRDID WITH loFormSet.lcPeriod,;
    CAPSESSNO WITH loFormSet.lcSesNum,;
    CBNKCODE  WITH lcBankCode,;
    CCHKACCT  WITH lcCheckcode,;
    CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
    CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
    CCURRCODE WITH &lcAPINVHDR..CCURRCODE,;
    NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
    NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
    NEQVAMNT  WITH ROUND((&lcAPINVHDR..NINVAMTAP &lcExSin1 &lcAPINVHDR..NEXRATE;
    &lcExSin2 &lcAPINVHDR..NCURRUNIT + lnAprDisB + lnAprAdjB ),2)
    *B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][Start]
    *!*	    REPLACE CVENDCODE WITH lcVendCode,;
    *!*	            CINVNO    WITH &lcAPINVHDR..CINVNO,;
    *!*	            DAPDTRDAT WITH ldPayDat,;
    *!*	            LAPDPOST  WITH .F.,;
    *!*	            CAPDGLACT WITH &lcAPINVHDR..CAPACCT,;
    *!*	            NAPDAMNT  WITH (&lcAPINVHDR..NINVAMTAP+&lcAPINVHDR..NINVDISAP+&lcAPINVHDR..NINVADJAP),;
    *!*	            CAPDACTID WITH "A",;
    *!*	            CFISFYEAR WITH loFormSet.lcYear,;
    *!*	            CFSPPRDID WITH loFormSet.lcPeriod,;
    *!*	            CAPSESSNO WITH loFormSet.lcSesNum,;
    *!*	            CBNKCODE  WITH lcBankCode,;
    *!*	            CCHKACCT  WITH lcCheckcode,;
    *!*	            CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
    *!*	            CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(ALLTRIM(lcPayNum),8)),;
    *!*	            CCURRCODE WITH &lcAPINVHDR..CCURRCODE,;
    *!*	            NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
    *!*	            NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
    *!*	            NEQVAMNT  WITH ROUND((&lcAPINVHDR..NINVAMTAP &lcExSin1 &lcAPINVHDR..NEXRATE;
    *!*	                           &lcExSin2 &lcAPINVHDR..NCURRUNIT + lnAprDisB + lnAprAdjB ),2)
    REPLACE CVENDCODE WITH lcVendCode,;
      CINVNO    WITH &lcAPINVHDR..CINVNO,;
      DAPDTRDAT WITH ldPayDat,;
      LAPDPOST  WITH .F.,;
      CAPDGLACT WITH &lcAPINVHDR..CAPACCT,;
      NAPDAMNT  WITH (&lcAPINVHDR..NINVAMTAP+&lcAPINVHDR..NINVDISAP+&lcAPINVHDR..NINVADJAP),;
      CAPDACTID WITH "A",;
      CFISFYEAR WITH loFormSet.lcYear,;
      CFSPPRDID WITH loFormSet.lcPeriod,;
      CAPSESSNO WITH loFormSet.lcSesNum,;
      CBNKCODE  WITH lcBankCode,;
      CCHKACCT  WITH lcCheckcode,;
      CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
      CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
      CCURRCODE WITH &lcAPINVHDR..CCURRCODE,;
      NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
      NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
      NEQVAMNT  WITH ROUND((&lcAPINVHDR..NINVAMTAP &lcExSin1 &lcAPINVHDR..NEXRATE;
      &lcExSin2 &lcAPINVHDR..NCURRUNIT + lnAprDisB + lnAprAdjB ),2)
    *B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][End]
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
    IF &lcAPINVHDR..NINVA1099 <> 0

      *B601957,1 Change this line to add the paid 1099 amount [End]
      SELECT (loFormSet.lcAPDIST)
      APPEND BLANK

      *E300296,1 M.H Add the currency to the AP module.
      * Add the Eq. lnApr1099B
      * Add the inv Currency,Rate,Unit
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
      *REPLACE CVENDCODE WITH lcVendCode,;
      CINVNO    WITH &lcAPINVHDR..CINVNO,;
      DAPDTRDAT WITH ldPayDat,;
      LAPDPOST  WITH .F.,;
      CAPDSTAT  WITH "V",;
      CAPDGLACT WITH " ",;
      NAPDAMNT  WITH &lcAPINVHDR..NINVA1099,;
      CAPDACTID WITH "B",;
      CFISFYEAR WITH loFormSet.lcYear,;
      CFSPPRDID WITH loFormSet.lcPeriod,;
      CAPSESSNO WITH loFormset.lcSesNum,;
      CBNKCODE  WITH lcBankCode,;
      CCHKACCT  WITH lcCheckcode,;
      CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
      CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
      CCURRCODE WITH &lcAPINVHDR..CCURRCODE,;
      NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
      NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
      NEQVAMNT  WITH &lcAPINVHDR..NINVA1099

      REPLACE CVENDCODE WITH lcVendCode,;
        CINVNO    WITH &lcAPINVHDR..CINVNO,;
        DAPDTRDAT WITH ldPayDat,;
        LAPDPOST  WITH .F.,;
        CAPDSTAT  WITH "V",;
        CAPDGLACT WITH " ",;
        NAPDAMNT  WITH &lcAPINVHDR..NINVA1099,;
        CAPDACTID WITH "B",;
        CFISFYEAR WITH loFormSet.lcYear,;
        CFSPPRDID WITH loFormSet.lcPeriod,;
        CAPSESSNO WITH loFormset.lcSesNum,;
        CBNKCODE  WITH lcBankCode,;
        CCHKACCT  WITH lcCheckcode,;
        CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
        CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
        CCURRCODE WITH &lcAPINVHDR..CCURRCODE,;
        NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
        NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
        NEQVAMNT  WITH &lcAPINVHDR..NINVA1099
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
      *Old: =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF

    ** Add 3rd record in distrebution file. **
    SELECT (loFormSet.lcAPDIST)
    APPEND BLANK
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
    *REPLACE CVENDCODE WITH lcVendCode,;
    CINVNO    WITH &lcAPINVHDR..CINVNO,;
    DAPDTRDAT WITH ldPayDat,;
    LAPDPOST  WITH .F.,;
    CAPDGLACT WITH &lcAPINVHDR..CCHKGLACC,;
    NAPDAMNT  WITH -&lcAPINVHDR..NINVFAAP,;
    CAPDACTID WITH "C",;
    CFISFYEAR WITH loFormSet.lcYear,;
    CFSPPRDID WITH loFormSet.lcPeriod,;
    CAPSESSNO WITH loFormSet.lcSesNum,;
    CBNKCODE  WITH lcBankCode,;
    CCHKACCT  WITH lcCheckcode,;
    CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
    CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
    CCURRCODE WITH &lcAPINVHDR..CAPRCURCOD,;
    NEXRATE   WITH lnChkExRat,;
    NCURRUNIT WITH lnChkExUnt,;
    NEQVAMNT  WITH -lnAprPayB
    *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
    *REPLACE CVENDCODE WITH lcVendCode,;
    CINVNO    WITH &lcAPINVHDR..CINVNO,;
    DAPDTRDAT WITH ldPayDat,;
    LAPDPOST  WITH .F.,;
    CAPDSTAT  WITH "V",;
    CAPDGLACT WITH " ",;
    NAPDAMNT  WITH &lcAPINVHDR..NINVA1099,;
    CAPDACTID WITH "B",;
    CFISFYEAR WITH loFormSet.lcYear,;
    CFSPPRDID WITH loFormSet.lcPeriod,;
    CAPSESSNO WITH loFormset.lcSesNum,;
    CBNKCODE  WITH lcBankCode,;
    CCHKACCT  WITH lcCheckcode,;
    CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
    CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
    CCURRCODE WITH &lcAPINVHDR..CCURRCODE,;
    NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
    NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
    NEQVAMNT  WITH &lcAPINVHDR..NINVA1099
    *B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][Start]
    *!*	     REPLACE CVENDCODE WITH lcVendCode,;
    *!*	            CINVNO    WITH &lcAPINVHDR..CINVNO,;
    *!*	            DAPDTRDAT WITH ldPayDat,;
    *!*	            LAPDPOST  WITH .F.,;
    *!*	            CAPDGLACT WITH &lcAPINVHDR..CCHKGLACC,;
    *!*	            NAPDAMNT  WITH -&lcAPINVHDR..NINVFAAP,;
    *!*	            CAPDACTID WITH "C",;
    *!*	            CFISFYEAR WITH loFormSet.lcYear,;
    *!*	            CFSPPRDID WITH loFormSet.lcPeriod,;
    *!*	            CAPSESSNO WITH loFormSet.lcSesNum,;
    *!*	            CBNKCODE  WITH lcBankCode,;
    *!*	            CCHKACCT  WITH lcCheckcode,;
    *!*	            CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
    *!*	            CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8)),;
    *!*	            CCURRCODE WITH &lcAPINVHDR..CAPRCURCOD,;
    *!*	            NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
    *!*	            NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
    *!*	            NEQVAMNT  WITH -lnAprPayB
    *!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [Start]
    *REPLACE CVENDCODE WITH lcVendCode,;
    CINVNO    WITH &lcAPINVHDR..CINVNO,;
    DAPDTRDAT WITH ldPayDat,;
    LAPDPOST  WITH .F.,;
    CAPDGLACT WITH &lcAPINVHDR..CCHKGLACC,;
    NAPDAMNT  WITH -&lcAPINVHDR..NINVFAAP,;
    CAPDACTID WITH "C",;
    CFISFYEAR WITH loFormSet.lcYear,;
    CFSPPRDID WITH loFormSet.lcPeriod,;
    CAPSESSNO WITH loFormSet.lcSesNum,;
    CBNKCODE  WITH lcBankCode,;
    CCHKACCT  WITH lcCheckcode,;
    CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
    CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
    CCURRCODE WITH &lcAPINVHDR..CAPRCURCOD,;
    NEXRATE   WITH &lcAPINVHDR..NEXRATE,;
    NCURRUNIT WITH &lcAPINVHDR..NCURRUNIT,;
    NEQVAMNT  WITH -lnAprPayB
    REPLACE CVENDCODE WITH lcVendCode,;
      CINVNO    WITH &lcAPINVHDR..CINVNO,;
      DAPDTRDAT WITH ldPayDat,;
      LAPDPOST  WITH .F.,;
      CAPDGLACT WITH &lcAPINVHDR..CCHKGLACC,;
      NAPDAMNT  WITH -&lcAPINVHDR..NINVFAAP,;
      CAPDACTID WITH "C",;
      CFISFYEAR WITH loFormSet.lcYear,;
      CFSPPRDID WITH loFormSet.lcPeriod,;
      CAPSESSNO WITH loFormSet.lcSesNum,;
      CBNKCODE  WITH lcBankCode,;
      CCHKACCT  WITH lcCheckcode,;
      CAPDTRTYP WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
      CAPDREF   WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
      CCURRCODE WITH &lcAPINVHDR..CAPRCURCOD,;
      NEXRATE   WITH lnChkExRat,;
      NCURRUNIT WITH lnChkExUnt,;
      NEQVAMNT  WITH -lnAprPayB
    *!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [End]
    *B610004,1 MMT 07/16/2012 Fix the problem of incorrect Payment# format saved in APDIST table[T20120702.0021][End]
    *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[END]
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]

    *- If there is a discount
    IF &lcAPINVHDR..NINVDISAP <> 0

      lcApdGlAct=IIF(loFormSet.lnPyChMN = 3,;
        IIF(SEEK(&lcAPINVHDR..CDIVISION,'APDIV');
        AND !EMPTY(APDIV.CDISCACCT),;
        APDIV.CDISCACCT,APSETUP.CDISCACCT),;
        IIF(SEEK(&lcAPINVHDR..CBNKCODE+&lcAPINVHDR..CCHKACCT,'APCHECKS');
        AND !EMPTY (&lcAPCHECKS..CDISCACCT),;
        &lcAPCHECKS..CDISCACCT,;
        IIF(SEEK(&lcAPINVHDR..CDIVISION,'APDIV') AND !EMPTY(APDIV.CDISCACCT),;
        APDIV.CDISCACCT,APSETUP.CDISCACCT)))

      SELECT (loFormSet.lcAPDIST)
      APPEND BLANK
      *E300296,1 M.H Add the currency to the AP module.
      * Add the Eq. lnAprDisB
      * Add the inv Currency,Rate,Unit
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
      *REPLACE CVENDCODE  WITH lcVendCode,;
      CINVNO     WITH &lcAPINVHDR..CINVNO,;
      DAPDTRDAT  WITH ldPayDat,;
      LAPDPOST   WITH .F.,;
      NAPDAMNT   WITH -&lcAPINVHDR..NINVDISAP,;
      CAPDACTID  WITH "S",;
      CFISFYEAR  WITH loFormSet.lcYear,;
      CFSPPRDID  WITH loFormSet.lcPeriod,;
      CAPSESSNO  WITH loFormSet.lcSesNum,;
      CBNKCODE   WITH lcBankCode,;
      CCHKACCT   WITH lcCheckcode,;
      CAPDTRTYP  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
      CAPDREF    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
      CAPDGLACT  WITH lcApdGlAct ,;
      CCURRCODE  WITH &lcAPINVHDR..CCURRCODE,;
      NEXRATE    WITH &lcAPINVHDR..NEXRATE,;
      NCURRUNIT  WITH &lcAPINVHDR..NCURRUNIT,;
      NEQVAMNT   WITH -lnAprDisB
      REPLACE CVENDCODE  WITH lcVendCode,;
        CINVNO     WITH &lcAPINVHDR..CINVNO,;
        DAPDTRDAT  WITH ldPayDat,;
        LAPDPOST   WITH .F.,;
        NAPDAMNT   WITH -&lcAPINVHDR..NINVDISAP,;
        CAPDACTID  WITH "S",;
        CFISFYEAR  WITH loFormSet.lcYear,;
        CFSPPRDID  WITH loFormSet.lcPeriod,;
        CAPSESSNO  WITH loFormSet.lcSesNum,;
        CBNKCODE   WITH lcBankCode,;
        CCHKACCT   WITH lcCheckcode,;
        CAPDTRTYP  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
        CAPDREF    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
        CAPDGLACT  WITH lcApdGlAct ,;
        CCURRCODE  WITH &lcAPINVHDR..CCURRCODE,;
        NEXRATE    WITH &lcAPINVHDR..NEXRATE,;
        NCURRUNIT  WITH &lcAPINVHDR..NCURRUNIT,;
        NEQVAMNT   WITH -lnAprDisB
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]

    ENDIF


    *- If there is an adjustment
    IF &lcAPINVHDR..NINVADJAP <> 0

      lcApdGlAct=IIF(loFormSet.lnPyChMN = 3,;
        IIF(SEEK(&lcAPINVHDR..CDIVISION,'APDIV');
        AND !EMPTY(APDIV.CADJACCT),;
        APDIV.CADJACCT,APSETUP.CADJACCT),;
        IIF(SEEK(&lcAPINVHDR..CBNKCODE+&lcAPINVHDR..CCHKACCT,'APCHECKS');
        AND !EMPTY (&lcAPCHECKS..CADJACCT),;
        &lcAPCHECKS..CADJACCT,;
        IIF(SEEK(&lcAPINVHDR..CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT),;
        APDIV.CADJACCT,APSETUP.CADJACCT)))

      SELECT (loFormSet.lcAPDIST)
      APPEND BLANK

      *E300296,1 M.H Add the currency to the AP module.
      * Add the Eq. lnAprAdjB
      * Add the inv Currency,Rate,Unit
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
      *REPLACE CVENDCODE  WITH lcVendCode,;
      CINVNO     WITH &lcAPINVHDR..CINVNO,;
      DAPDTRDAT  WITH ldPayDat,;
      LAPDPOST   WITH .F.,;
      NAPDAMNT   WITH -&lcAPINVHDR..NINVADJAP,;
      CAPDACTID  WITH "J",;
      CFISFYEAR  WITH loFormSet.lcYear,;
      CFSPPRDID  WITH loFormSet.lcPeriod,;
      CAPSESSNO  WITH loFormSet.lcSesNum,;
      CBNKCODE   WITH lcBankCode,;
      CCHKACCT   WITH lcCheckcode,;
      CAPDTRTYP  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
      CAPDREF    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
      CAPDGLACT  WITH lcApdGlAct ,;
      CCURRCODE  WITH &lcAPINVHDR..CCURRCODE,;
      NEXRATE    WITH &lcAPINVHDR..NEXRATE,;
      NCURRUNIT  WITH &lcAPINVHDR..NCURRUNIT,;
      NEQVAMNT   WITH -lnAprAdjB
      REPLACE CVENDCODE  WITH lcVendCode,;
        CINVNO     WITH &lcAPINVHDR..CINVNO,;
        DAPDTRDAT  WITH ldPayDat,;
        LAPDPOST   WITH .F.,;
        NAPDAMNT   WITH -&lcAPINVHDR..NINVADJAP,;
        CAPDACTID  WITH "J",;
        CFISFYEAR  WITH loFormSet.lcYear,;
        CFSPPRDID  WITH loFormSet.lcPeriod,;
        CAPSESSNO  WITH loFormSet.lcSesNum,;
        CBNKCODE   WITH lcBankCode,;
        CCHKACCT   WITH lcCheckcode,;
        CAPDTRTYP  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
        CAPDREF    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
        CAPDGLACT  WITH lcApdGlAct ,;
        CCURRCODE  WITH &lcAPINVHDR..CCURRCODE,;
        NEXRATE    WITH &lcAPINVHDR..NEXRATE,;
        NCURRUNIT  WITH &lcAPINVHDR..NCURRUNIT,;
        NEQVAMNT   WITH -lnAprAdjB
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
      *Old: =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *lnExchDiff = lnAprPayB - ROUND(APINVHDR.NINVAMTAP * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2)
    lnExchDiff = lnAprPayB - ROUND(&lcAPINVHDR..NINVAMTAP &lcExSin1 &lcAPINVHDR..NEXRATE;
      &lcExSin2 &lcAPINVHDR..NCURRUNIT,2)
    *E300316,1 (End)
    *E300296,1 M.H Add the currency to the AP module.
    * Add the Eq. lnExchDiff
    * Add the base Currency,Rate = 1,Unit= 1
    IF lnExchDiff <> 0

      *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
      *B610319,1 SAB 04/28/2013 [Start] get the correct exchange difference account code
      *lcApdGlAct=IIF(loFormSet.lnPyChMN = 3,;
      *  IIF(SEEK(&lcAPINVHDR..CDIVISION,'APDIV');
      *  AND !EMPTY(APDIV.CADJACCT),;
      *  APDIV.CADJACCT,APSETUP.CADJACCT),;
      *  IIF(SEEK(&lcAPINVHDR..CBNKCODE+&lcAPINVHDR..CCHKACCT,'APCHECKS');
      *  AND !EMPTY (&lcAPCHECKS..CADJACCT),;
      *  &lcAPCHECKS..CADJACCT,;
      *  IIF(SEEK(&lcAPINVHDR..CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT),;
      *  APDIV.CADJACCT,APSETUP.CADJACCT)))
      IF !USED('GLSETUP')
        gfOpenFile(oAriaApplication.DataDir+'GLSETUP','','SH')
      ENDIF
      lcApdGlAct  = IIF(loFormSet.Ap1.llApGlLink, ;
        IIF(EMPTY(GLSETUP.CSETEXMJ), loFormSet.Ap1.lcEmptyAcc, ALLTRIM(GLSETUP.CSETEXMJ) + STRTRAN(SUBSTR(loFormSet.Ap1.lcApsAcMas,AT("-",loFormSet.Ap1.lcApsAcMas)),"9","0")),;
        gfGetMemVar('LNEXRATACC', oAriaApplication.ActiveCompanyID))
      *B610319,1 SAB 04/28/2013 [End  ]
      *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[END]

      SELECT (loFormSet.lcAPDIST)
      APPEND BLANK
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
      *REPLACE CVENDCODE  WITH lcVendCode,;
      CINVNO     WITH &lcAPINVHDR..CINVNO,;
      DAPDTRDAT  WITH ldPayDat,;
      LAPDPOST   WITH .F.,;
      NAPDAMNT   WITH lnExchDiff,;
      CAPDACTID  WITH "J",;
      CFISFYEAR  WITH loFormSet.lcYear,;
      CFSPPRDID  WITH loFormSet.lcPeriod,;
      CAPSESSNO  WITH loFormSet.lcSesNum,;
      CBNKCODE   WITH lcBankCode,;
      CCHKACCT   WITH lcCheckcode,;
      CAPDTRTYP  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
      CAPDREF    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
      CAPDGLACT  WITH lcExDifAcc,;
      CCURRCODE  WITH oAriaApplication.BaseCurrency,;
      NEXRATE    WITH 1,;
      NCURRUNIT  WITH 1,;
      NEQVAMNT   WITH lnExchDiff,;
      nApdLinNo  WITH 1
      *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
      *REPLACE CVENDCODE  WITH lcVendCode,;
      CINVNO     WITH &lcAPINVHDR..CINVNO,;
      DAPDTRDAT  WITH ldPayDat,;
      LAPDPOST   WITH .F.,;
      NAPDAMNT   WITH lnExchDiff,;
      CAPDACTID  WITH "J",;
      CFISFYEAR  WITH loFormSet.lcYear,;
      CFSPPRDID  WITH loFormSet.lcPeriod,;
      CAPSESSNO  WITH loFormSet.lcSesNum,;
      CBNKCODE   WITH lcBankCode,;
      CCHKACCT   WITH lcCheckcode,;
      CAPDTRTYP  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
      CAPDREF    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
      CAPDGLACT  WITH lcExDifAcc,;
      CCURRCODE  WITH oAriaApplication.BaseCurrency,;
      NEXRATE    WITH 1,;
      NCURRUNIT  WITH 1,;
      NEQVAMNT   WITH lnExchDiff,;
      nApdLinNo  WITH 1
      REPLACE CVENDCODE  WITH lcVendCode,;
        CINVNO     WITH &lcAPINVHDR..CINVNO,;
        DAPDTRDAT  WITH ldPayDat,;
        LAPDPOST   WITH .F.,;
        NAPDAMNT   WITH lnExchDiff,;
        CAPDACTID  WITH "J",;
        CFISFYEAR  WITH loFormSet.lcYear,;
        CFSPPRDID  WITH loFormSet.lcPeriod,;
        CAPSESSNO  WITH loFormSet.lcSesNum,;
        CBNKCODE   WITH lcBankCode,;
        CCHKACCT   WITH lcCheckcode,;
        CAPDTRTYP  WITH IIF(loFormSet.lnPyChMN = 1,'M',IIF(loFormSet.lnPyChMN = 2,'N','H')),;
        CAPDREF    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
        CAPDGLACT  WITH lcApdGlAct,;
        CCURRCODE  WITH oAriaApplication.BaseCurrency,;
        NEXRATE    WITH 1,;
        NCURRUNIT  WITH 1,;
        NEQVAMNT   WITH lnExchDiff,;
        nApdLinNo  WITH 1
      *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[END]
      *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
      *B601589,1 Change this line [End]


      *Old: =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF
    *E300296,1 M.H Add the currency to the AP module.
    * Clear The aproved Unit,Rate
    SELECT (loFormSet.lcAPINVHDR)
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
    *REPLACE NINVPAID   WITH NINVPAID  + NINVAMTAP,;
    NINVDISTK  WITH NINVDISTK + NINVDISAP,;
    NINVADJ    WITH NINVADJ   + NINVADJAP,;
    NINV1099A  WITH NINV1099A + NINVA1099,;
    CBNKCODE   WITH "",;
    CCHKACCT   WITH "",;
    CCHKGLACC  WITH "",;
    NINVAMTAP  WITH 0,;
    NINVDISAP  WITH 0,;
    NINVADJAP  WITH 0,;
    NINVA1099  WITH 0,;
    CAPRCURCOD WITH '',;
    NAPREXRAT  WITH 0,;
    NAPRCURUNT WITH 0,;
    DCHKDATE  WITH ldPayDat,;
    CCHKNO    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
    nInvFAAp  WITH 0
    REPLACE NINVPAID   WITH NINVPAID  + NINVAMTAP,;
      NINVDISTK  WITH NINVDISTK + NINVDISAP,;
      NINVADJ    WITH NINVADJ   + NINVADJAP,;
      NINV1099A  WITH NINV1099A + NINVA1099,;
      CBNKCODE   WITH "",;
      CCHKACCT   WITH "",;
      CCHKGLACC  WITH "",;
      NINVAMTAP  WITH 0,;
      NINVDISAP  WITH 0,;
      NINVADJAP  WITH 0,;
      NINVA1099  WITH 0,;
      CAPRCURCOD WITH '',;
      NAPREXRAT  WITH 0,;
      NAPRCURUNT WITH 0,;
      DCHKDATE  WITH ldPayDat,;
      CCHKNO    WITH IIF(loFormSet.lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADR(ALLTRIM(lcPayNum),8)),;
      nInvFAAp  WITH 0
    *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
    *B601526,1 Add the new field (nInvFAAp) [Begin]

    *B600970,1 M.H End.

    *!*	    lnAprPayB  = 0
    *!*	    lnAprDisB  = 0
    *!*	    lnAprAdjB  = 0
    *!*	    lnApr1099B = 0

    *Old: =gfAdd_Info('APINVHDR')  && Add the audit information to the record.
    *=gfObj_lock(.F.) && unlock the pointed record.

    lnThermNo = lnThermNo + 1
    oProgress.CurrentProgress(lnThermNo)
    oProgress.lblFirstLabel.CAPTION = APINVHDR.CVENDCODE

    *- update the LUPDATED field
    SELECT (loFormSet.lcAPINVHDR)
    REPLACE LUPDATED WITH .T.

  ENDSCAN

  oProgress = NULL
  ***Remove paid record from selected array.
  DIMENSION laSelect[1,2]
  laSelect = " "
  lnArLen  = 1
  *Old: CLEAR READ


  SELECT(loFormSet.lcAPINVHDR)
  SET FILTER TO !LUPDATED
  LOCATE

  *!*  ELSE && if date is invalid or can not lock any record.
  *** Release locking. ***
  *  do this in the save , undo
  *!*	  SELECT APINVHDR
  *!*	  FOR lnRecCount= 1 TO lnArCount-1
  *!*	    GO laSelect[lnRecCount,2] && go to the seleced record.
  *!*	    =gfObj_lock(.F.) && unlock the pointed record.
  *!*	  ENDFOR
ENDIF
RETURN .T.
*!**************************************************************************
*!
*!      Function: lfvTot
*!
*!**************************************************************************
* Get total all selected approve amount,discount,adjustment,1099. ***
* calling from pay selected invoices and vendor screens.***

FUNCTION lfvTot
PARAMETERS loFormSet
SELECT (loFormSet.lcAPINVHDR)
lnRecNo   = RECNO() && get the record no.
LOCATE
SCAN FOR LSELECT

  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,cCurrCode)

  lcExSin4 = ' '
  *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[Start]
  *!*	    lnTotPay  = lnTotPay  + ROUND(APINVHDR.NINVFAAP , 2)
  *!*	    lnTotDisc = lnTotDisc + ROUND(APINVHDR.NINVDISAP &lcExSin1;
  *!*	                            APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)

  *!*	    lnTotAdj  = lnTotAdj  + ROUND(APINVHDR.NINVADJAP &lcExSin1 APINVHDR.NEXRATE;
  *!*	                            &lcExSin2 APINVHDR.NCURRUNIT,2)

  *!*	    lnTot1099 = lnTot1099 + ROUND(APINVHDR.NINVA1099 &lcExSin1;
  *!*	                            APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
  lnTotPay  = lnTotPay  + ROUND(EVALUATE(loFormSet.lcAPINVHDR+'.NINVFAAP') , 2)
  lnTotDisc = lnTotDisc + ROUND(EVALUATE(loFormSet.lcAPINVHDR+'.NINVDISAP') &lcExSin1;
    EVALUATE(loFormSet.lcAPINVHDR+'.NEXRATE') &lcExSin2 EVALUATE(loFormSet.lcAPINVHDR+'.NCURRUNIT'),2)
  lnTotAdj  = lnTotAdj  + ROUND(EVALUATE(loFormSet.lcAPINVHDR+'.NINVADJAP') &lcExSin1 EVALUATE(loFormSet.lcAPINVHDR+'.NEXRATE');
    &lcExSin2 EVALUATE(loFormSet.lcAPINVHDR+'.NCURRUNIT'),2)
  lnTot1099 = lnTot1099 + ROUND(EVALUATE(loFormSet.lcAPINVHDR+'.NINVA1099') &lcExSin1;
    EVALUATE(loFormSet.lcAPINVHDR+'.NEXRATE') &lcExSin2 EVALUATE(loFormSet.lcAPINVHDR+'.NCURRUNIT'),2)
  *!B609947,1 SAB 05/29/2012 Fix AP problems reported to AP Conversion project issue1[END]
ENDSCAN

IF lnRecNo <= RECCOUNT()  && be sure that the is not empty.
  GO lnRecNo      && go to the last record pointed.
ENDIF

*!*  WITH loFormSet
*!*    .lnTotPay  = lnTotPay
*!*    .lnTotDisc = lnTotDisc
*!*    .lnTotAdj  = lnTotAdj
*!*    .lnTot1099 = lnTot1099
*!*  ENDWITH

*=lfRefresh()
*- End of lfvTot
*!*************************************************************
*! Name      : lfvPayDate                             *B601013,1
*! Developer : RENEE - Renee Ezzat
*! Date      : 04/16/1996
*! Purpose   : Valid function for ldPayDat field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPayDate()
*!*************************************************************
FUNCTION lfvPayDate
PARAMETERS loPayFormSet
LOCAL loFormSet
loFormSet = loPayFormSet.loCallingForm
lcAPINVHDR = loFormSet.lcAPINVHDR
*B601013,1 Validate the payment date if a new one , or if changed.
IF EMPTY(loPayFormSet.Ariaform1.DtAppDate.Text1.VALUE)
  loPayFormSet.Ariaform1.DtAppDate.Text1.VALUE = loPayFormSet.Ariaform1.DtAppDate.Text1.OldValue
ENDIF
* Old : IF ldPayDat <> ldOldPDat
STORE " " TO lcFisPrd,lcFisYer
*IF loPayFormSet.Ariaform1.DtAppDate.Text1.Value <> loPayFormSet.Ariaform1.DtAppDate.Text1.OldValue

IF lfVDtMsg(oAriaApplication.prntcompanyid ,@lcFisPrd,@lcFisYer,ldPayDat)
  loFormSet.lcPeriod  = lcFisPrd
  loFormSet.lcYear    = lcFisYer
  ldOldPDat = ldPayDat

  *B601013,1 Upon selecting a valid date, get the exchange rate
  *B601013,1 for the currency.
  lnChkExUnt = 1
  IF &lcAPINVHDR..CAPRCURCOD <> oAriaApplication.BaseCurrency
    lnChkExRat  = gfChkRate('lnChkExUnt',;
      &lcAPINVHDR..CAPRCURCOD,ldPayDat, .T.,;
      oAriaapplication.Activecompanyid, oAriaApplication.BaseCurrency, .T.)
    IF lnChkExRat = 0 .AND. !loFormSet.llAddRate
      *B601013,1 MESSAGE : " A valid ð to ð exchange rate could not "
      *B601013,1           " be found for ð.                        "
      *B601013,1           "                  ® Ok ¯                "
      =gfModalGen("QRM04157B00000","DIALOG",;
        ALLTRIM(APINVHDR.CAPRCURCOD)+'|'+ALLTRIM(oAriaApplication.BaseCurrency)+'|'+DTOC(ldPayDat))
    ENDIF
    *Old: IF loFormSet.llEditEx
    *Old:  SHOW GET lnChkExRat ENABLE
    *Old: ELSE
    *Old:   SHOW GET lnChkExRat DISABLE
    *Old: ENDIF
    loPayFormSet.Ariaform1.txtExRate.ENABLED = loFormSet.llEditEx
    *!B610172,1 SAB 12/11/2012 Fix problem of not changing the exchange rate when change payment date [Start]
    loPayFormSet.Ariaform1.txtExRate.VALUE = lnChkExRat
    *!B610172,1 SAB 12/11/2012 Fix problem of not changing the exchange rate when change payment date [End]
  ELSE
    lnChkExRat  = 1
    SHOW GET lnChkExRat DISABLE
    loPayFormSet.Ariaform1.txtExRate.ENABLED = .F.
  ENDIF
  *B601013,1 end
ELSE
  *Old: ldPayDat  = ldOldPDat
  loPayFormSet.Ariaform1.DtAppDate.Text1.VALUE = loPayFormSet.Ariaform1.DtAppDate.Text1.OldValue
  IF EMPTY(ldPayDat)
    lnChkExRat = 0
    loPayFormSet.Ariaform1.txtExRate.ENABLED = .F.
    *Old: SHOW GET lnChkExRat DISABLE
  ENDIF
  *Old: _CUROBJ   = OBJNUM(ldPayDat)
  RETURN .F.
ENDIF
*ENDIF
*ENDIFldOldPDat

*!*************************************************************
*! Name      : lfvExRate                             *B601013,1
*! Developer : RENEE - Renee Ezzat
*! Date      : 04/16/1996
*! Purpose   : Valid function for lnChkExRat field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvExRate()
*!*************************************************************
*E300296,4 Added this function as a validation for the new
*E300296,4 field, lnChkExRat
FUNCTION lfvExRate
PARAMETERS loPayFormSet,loTxt

*E300296,4 if the entered value is not greater than zero,
*E300296,4 Message :  "   ð should be greater than ð.   "
*E300296,4                 ®   OK   ¯
*Old: IF lnChkExRat <> lnOldVal .AND. lnChkExRat < 0 .AND. ;
gfModalGen("TRM04072B00000","DIALOG", lcTExRateMsg) > 0
IF loTxt.VALUE <> loTxt.OldValue .AND. loTxt.VALUE < 0 .AND. ;
    gfModalGen("TRM04072B00000","DIALOG", lcTExRateMsg) > 0
  *E300296,4 Old adjustment amount field is also filled from the
  *E300296,4 when function of this field, to save a variable.
  *E300296,4 i.e. At this point it actually contains the old exchange
  *E300296,4 rate.
  *Old: lnChkExRat = lnOldVal
  *Old: _CUROBJ = _CUROBJ
  loTxt.VALUE = loTxt.OldValue
ENDIF
*- End of lfvExRate
*!**************************************************************************
*!
*!      Function: lfBnkCode
*!
*!**************************************************************************
FUNCTION lfBnkCode

PRIVATE lcRetVal

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias
SELECT APSETUP
lcRetVal   = APSETUP.CBNKCODE

llRpGlLink = IIF(APSETUP.CAPSGLLINK='Y',.T.,.F.)
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
  ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink
  SELECT SYCCOMP
  =SEEK(oAriaApplication.ActiveCompanyID)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = oAriaApplication.DataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(oAriaApplication.ActiveCompanyID)
  ENDIF
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName)
  &lcOGVarName=lcRetVal
ENDIF

RETURN REPLI('!',8)

*!**************************************************************************
*!
*!      Function: lfChkAct
*!
*!**************************************************************************
FUNCTION lfChkAct
PARAMETERS llFirsTime

PRIVATE lcRetVal

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias

IF llFirsTime
  SELECT APSETUP
  lcRetVal = APSETUP.CCHKACCT
ELSE
  SELECT APCHECKS
  gfSetOrder('BANKCHECK')
  =gfSeek(laOGVrFlt[2,6])
  lcRetVal = APCHECKS.CCHKACCT
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName) AND llFirsTime
  &lcOGVarName=lcRetVal
ENDIF
RETURN IIF(llFirsTime,REPL('!',12),lcRetVAl)

************************************************************************************
*Name : lfvOGVend
*Date:*E303014,1 TMI 01/06/2012
*Purpose: valid function called from OG for Vendor
************************************************************************************
FUNCTION lfvOGVend
PRIVATE lcVendCode
loOgScroll.ACTIVECONTROL.VALUE = PADR(ALLTRIM(loOgScroll.ACTIVECONTROL.VALUE),FSIZE('CVENDCODE','APINVHDR'))
lcVendCode = loOgScroll.ACTIVECONTROL.VALUE
SELECT APVENDOR

IF !SEEK(lcVendCode,'APVENDOR','VENCODE')

  DIMENSION laTemp[4]
  laTemp = '' && fill the array.
  *** Save the old fields and title.***
  *!*    lcSavBrFld=lcBrFields
  *!*    lcSavTitle=lcFile_Ttl

  *** Get new fields name and title.***
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *!*    lcBrFields="CVENDCODE :H= 'Vendor',"       +;
  *!*               "CVENCOMP  :H= 'Company',"      +;
  *!*               "CPHONENO  :H= 'Phone',"        +;
  *!*               "CBNKCODE  :H= 'Bank ',"        +;
  *!*               "CCHKACCT  :H= 'Checking Acc.',"+;
  *!*               "CCURRCODE :H= 'Currency '"

  *!*    lcFile_Ttl="Vendors"
  lcBrFields="CVENDCODE :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Vendor,loFormSet.GetHeaderText("LANG_APMNCHP_Vendor",loFormSet.HeaderAlias))+"',"       +;
    "CVENCOMP  :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_COMPANY,loFormSet.GetHeaderText("LANG_APMNCHP_COMPANY",loFormSet.HeaderAlias))+"',"      +;
    "CPHONENO  :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_PHONE,loFormSet.GetHeaderText("LANG_APMNCHP_PHONE",loFormSet.HeaderAlias)) +"',"        +;
    "CBNKCODE  :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Bank,loFormSet.GetHeaderText("LANG_APMNCHP_Bank",loFormSet.HeaderAlias))+"',"        +;
    "CCHKACCT  :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_Checking_Account,loFormSet.GetHeaderText("LANG_APMNCHP_Checking_Account",loFormSet.HeaderAlias))  +"',"+;
    "CCURRCODE :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_CURRENCY,loFormSet.GetHeaderText("LANG_APMNCHP_CURRENCY",loFormSet.HeaderAlias))+"'"
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcFile_Ttl=LANG_APMNCHP_VENDORS
  lcFile_Ttl=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_VENDORS,loFormSet.GetHeaderText("LANG_APMNCHP_VENDORS",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  *** Browse ***
  =gfBrows(.F.,'CVENDCODE,CBNKCODE,CCHKACCT,CCURRCODE','laTemp')
  *** Get the old fields name and title.***
  *!*    lcBrFields=lcSavBrFld
  *!*    lcFile_Ttl=lcSavTitle
  *** if not empty of browse. ***

  lnVenPos = lfGetVrPos("laOGVrFlt","APINVHDR.CVENDCODE")
  lcVendCode = ''
  IF !EMPTY(laTemp[1])
    loOgScroll.laOGVrFlt[ lnVenPos , 6 ]  = laTemp[1]
    lcVendCode = laTemp[1]
  ELSE                      && if empty
    loOgScroll.laOGVrFlt[ lnVenPos , 6 ] = loOgScroll.ACTIVECONTROL.OldValue && get the old vendor code.
    RETURN
  ENDIF

ENDIF

lcBankCode = APVENDOR.CBNKCODE
lcCheckCode= APVENDOR.CCHKACCT
=SEEK(APVENDOR.CDIVISION,'APDIV')
GO TOP IN APSETUP
=lfGetBnkChk(@lcBankCode,@lcCheckCode,lcVendCode)
IF lnPyChMN <>  3
  loOgScroll.laOGVrFlt[ lfGetVrPos("laOGVrFlt","APINVHDR.CBNKCODE") , 6 ]   = lcBankCode
  loOgScroll.laOGVrFlt[ lfGetVrPos("laOGVrFlt","APINVHDR.CCHKACCT") , 6 ]   = lcCheckCode
ELSE
  IF llMultiCr
    loOgScroll.laOGVrFlt[ lfGetVrPos("laOGFxFlt","APINVHDR.CCURRCODE ") , 6 ] = APVENDOR.CCURRCODE
  ENDIF
ENDIF

*- End of lfvOGVend.


******************************************************************************************
*Name :  lfGetBnkChk
*Date:*E303014,1 TMI 01/08/2012
*Purpose:Get Default Bank code and checking account
******************************************************************************************
FUNCTION  lfGetBnkChk
PARAMETERS lcBankCode,lcCheckCode,lcVendCode
*!*  DO CASE
*!*  CASE !EMPTY(APINVHDR.CBNKCODE) && if found values in invoice header file.
*!*    lcBankCode = APINVHDR.CBNKCODE
*!*    lcCheckCode= APINVHDR.CCHKACCT
*!*  CASE !EMPTY(APVENDOR.CBNKCODE) && if found values in vendor file.
*!*    lcBankCode = APVENDOR.CBNKCODE
*!*    lcCheckCode= APVENDOR.CCHKACCT
*!*  CASE !EMPTY(APDIV.CBNKCODE)    && if found values in division file.
*!*    lcBankCode = APDIV.CBNKCODE
*!*    lcCheckCode= APDIV.CCHKACCT
*!*  OTHERWISE                      && get value from setup file.
*!*    lcBankCode = APSETUP.CBNKCODE
*!*    lcCheckCode= APSETUP.CCHKACCT
*!*  ENDCASE

*** set default bank and check ***
IF lnPyChMN <>  3 && if not calling from cash payment
  IF !EMPTY(lcVendCode)
    DO CASE
    CASE SEEK(lcVendCode,'APVENDOR') .AND. !EMPTY(APVENDOR.CBNKCODE) && if found values in vendor file.
      lcBankCode = APVENDOR.CBNKCODE
      lcCheckCode= APVENDOR.CCHKACCT
    CASE !EMPTY(APDIV.CBNKCODE)    && if found values in division file.
      lcBankCode = APDIV.CBNKCODE
      lcCheckCode= APDIV.CCHKACCT
    OTHERWISE                      && get value from setup file.
      lcBankCode = APSETUP.CBNKCODE
      lcCheckCode= APSETUP.CCHKACCT
    ENDCASE
  ENDIF
ENDIF

*- End of  lfGetBnkChk.


***********************************************************************************************************************
*Name : lfGetVrPos
*Date:*E303014,1 TMI 01/06/2012 [Start]
*Purpose: Get variable position
***********************************************************************************************************************
FUNCTION lfGetVrPos
PARAMETERS lcVrArr,lcFld
LOCAL lnPos
lnPOS = ASCAN(loOgScroll.&lcVrArr.,lcFld)
IF lnPos > 0
  lnPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
ENDIF
RETURN lnPos
*- End of lfGetVrPos.

*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
FUNCTION lfvBank
* x
*!*  IF EMPTY(loOgScroll.ActiveControl.Value)
*!*    RETURN .F.
*!*  ENDIF

LOCAL loFld
loFld = loOgScroll.ACTIVECONTROL

IF loFld.OldValue = loFld.VALUE
  RETURN
ENDIF
DECLARE laRpRetFld(1)
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields    = 'CBnkCode:H="'+LANG_APMNCHP_BANKCODE +'",CBNKLNDES:H="'+LANG_APMNCHP_DESC+'"'
lcBrFields    = 'CBnkCode:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_BANKCODE,loFormSet.GetHeaderText("LANG_APMNCHP_BANKCODE",loFormSet.HeaderAlias)) +'",CBNKLNDES:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_DESC,loFormSet.GetHeaderText("LANG_APMNCHP_DESC",loFormSet.HeaderAlias))+'"'
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

SELECT APBANKS
gfSetOrder('BANKCODE')
IF loFld.OldValue <> loFld.VALUE

  lnPOS = ASCAN(loOgScroll.laOGVrFlt,"APINVHDR.CBNKCODE")
  IF lnPos > 0
    lnPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
  ENDIF

  *** Search for the current Group code
  loFld.VALUE = PADR(ALLTRIM(loFld.VALUE),8)
  IF !EMPTY(loFld.VALUE) AND ('?' $ loFld.VALUE .OR.!SEEK(loFld.VALUE))
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *=gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfBrows([],'CBnkCode',"laRpRetFld",LANG_APMNCHP_BANKTITLE,.F.)
    =gfBrows([],'CBnkCode',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_BANKTITLE,loFormSet.GetHeaderText("LANG_APMNCHP_BANKTITLE",loFormSet.HeaderAlias)),.F.)
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    IF !EMPTY(laRpRetFld[1])
      loFld.VALUE = laRpRetFld[1]
    ELSE
      loFld.VALUE = loFld.OldValue
    ENDIF
    loFld.REFRESH
  ENDIF

  loOgScroll.laOGVrFlt[lnPOS,6] = loFld.VALUE

  lnChkPos = ASCAN(loOgScroll.laOGVrFlt,"APINVHDR.CCHKACCT")
  IF lnChkPos > 0
    lnChkPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnChkPos,1)
  ENDIF
  IF EMPTY(loFld.VALUE)
    loOgScroll.laOGVrFlt[lnChkPos,6] = ''
  ELSE
    IF !gfSeek(loFld.VALUE+loOgScroll.laOGVrFlt[lnChkPos,6],'APCHECKS')
      loOgScroll.laOGVrFlt[lnChkPos,6] = ''
    ENDIF
  ENDIF
ENDIF

IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

*!**************************************************************************
*!
*!      Function: lfvChkAct
*!
*!**************************************************************************
FUNCTION lfvChkAct
* x
IF loOgScroll.ACTIVECONTROL.OldValue = loOgScroll.ACTIVECONTROL.VALUE
  RETURN
ENDIF
*!*  IF EMPTY(loOgScroll.ActiveControl.Value)
*!*    RETURN .F.
*!*  ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

lnChkPos = ASCAN(loOgScroll.laOGVrFlt,"APINVHDR.CCHKACCT")
IF lnChkPos > 0
  lnChkPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnChkPos,1)
ENDIF


SELECT APCHECKS
gfSetOrder('BANKCHECK')
IF EMPTY(loOgScroll.laOgVrFlt[lnChkPos,6])
  gfSeek('')
ELSE
  gfSeek(PADR(loOgScroll.laOgVrFlt[lnChkPos,6],8))
ENDIF
LOCATE

lcRpCurFld      = loOgScroll.ACTIVECONTROL.VALUE
&& Check If year field is empty
IF loOgScroll.ACTIVECONTROL.OldValue <> loOgScroll.ACTIVECONTROL.VALUE
  *** Search for the current Group code
  IF !SEEK(laOGVrFlt[lnChkPos,6]+loOgScroll.ACTIVECONTROL.VALUE)
    =gfBrows('','CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)

    IF EMPTY(laRpRetFld[1])
      loOgScroll.ACTIVECONTROL.VALUE = ''
      loOgScroll.laOGVrFlt[lnChkPos,6] = ''
    ELSE
      IF loOgScroll.ACTIVECONTROL.VALUE <> laRpRetFld[1]
        loOgScroll.ACTIVECONTROL.VALUE = laRpRetFld[1]
        loOgScroll.laOGVrFlt[lnChkPos,6] = laRpRetFld[1]
      ENDIF
    ENDIF
    loOgScroll.ACTIVECONTROL.REFRESH
  ENDIF

  lnPOS = ASCAN(loOgScroll.laOGVrFlt,"APINVHDR.CBNKCODE")
  IF lnPos > 0
    lnPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
  ENDIF

  IF !EMPTY(loOgScroll.ACTIVECONTROL.VALUE)
    IF EMPTY(loOgScroll.laOGVrFlt[lnPOS,6] ) OR loOgScroll.laOGVrFlt[lnPOS,6]<>APCHECKS.CBNKCODE
      loOgScroll.laOGVrFlt[lnPOS,6] = APCHECKS.CBNKCODE
    ENDIF
  ENDIF
ENDIF


IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

************************************************************************************************
*Name      : lfBeforeRev
*Date      : *E303014,1 TMI 01/05/2012 [Start]
*Purpose   : Valid function for the OG
************************************************************************************************
FUNCTION lfOgValidFn
LOCAL lcVendCode,lcCurrCode
lcVendCode = lfGetCrVal("APINVHDR.CVENDCODE")
IF EMPTY(lcVendCode)
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *gfModalGen('INM04074B00000','DIALOG','Vendor code')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *gfModalGen('INM04074B00000','DIALOG',LANG_APMNCHP_VENDCODE)
  gfModalGen('INM04074B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_VENDCODE,loFormSet.GetHeaderText("LANG_APMNCHP_VENDCODE",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  RETURN .F.
ENDIF

IF lnPyChMN = 3

  lcCurrCode = lfGetCrVal("APINVHDR.CAPRCURCOD")
  IF llMultiCr AND EMPTY( lcCurrCode )
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *gfModalGen('INM04074B00000','DIALOG','Currency code')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *gfModalGen('INM04074B00000','DIALOG',LANG_APMNCHP_CURRENCYCODE)
    gfModalGen('INM04074B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_CURRENCYCODE,loFormSet.GetHeaderText("LANG_APMNCHP_CURRENCYCODE",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    RETURN .F.
  ENDIF

ELSE

  LOCAL lcBankCode,lcCheckAcc
  lcBankCode = lfGetCrVal("APINVHDR.CBNKCODE")
  lcCheckAcc = lfGetCrVal("APINVHDR.CCHKACCT")
  IF EMPTY(lcBankCode)
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *gfModalGen('INM04074B00000','DIALOG','Bank code')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *gfModalGen('INM04074B00000','DIALOG',LANG_APMNCHP_BANKCODE)
    gfModalGen('INM04074B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_BANKCODE,loFormSet.GetHeaderText("LANG_APMNCHP_BANKCODE",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    RETURN .F.
  ENDIF
  IF EMPTY(lcCheckAcc)
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *gfModalGen('INM04074B00000','DIALOG','Checking Account code')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *gfModalGen('INM04074B00000','DIALOG',LANG_APMNCHP_CHKACCCODE)
    gfModalGen('INM04074B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_CHKACCCODE,loFormSet.GetHeaderText("LANG_APMNCHP_CHKACCCODE",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    RETURN .F.
  ENDIF

ENDIF
*- End of lfOgValidFn.

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
FUNCTION lfvCurCode
LOCAL loFld
loFld = loOgScroll.ACTIVECONTROL

IF EMPTY(loFld.VALUE) .OR. !SEEK(loFld.VALUE,'SYCCURR') .OR. ATC("?",loFld.VALUE) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *!*    lcFile_Ttl = "Currency"
  *!*    lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
  *!*                 "CCURRDESC :R :H= 'Description',  " +;
  *!*                 "CCURRSMBL :R :H= 'Symbol'"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcFile_Ttl = LANG_APMNCHP_CURRENCY
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_CURRENCY,loFormSet.GetHeaderText("LANG_APMNCHP_CURRENCY",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lcBrFields = "CCURRCODE :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_CURRENCYCODE,loFormSet.GetHeaderText("LANG_APMNCHP_CURRENCYCODE",loFormSet.HeaderAlias)) +"'," +;
    "CCURRDESC :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_DESC,loFormSet.GetHeaderText("LANG_APMNCHP_DESC",loFormSet.HeaderAlias))+"',  " +;
    "CCURRSMBL :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_SYMBOL,loFormSet.GetHeaderText("LANG_APMNCHP_SYMBOL",loFormSet.HeaderAlias))+"'"
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
  *lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCURRCODE")
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CAPRCURCOD")
  *!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[END]
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
  ENDIF
  IF EMPTY(laTemp[1])
    loFld.VALUE = loFld.OldValue
    loOgScroll.laOGFxFlt[lnPOS,6] = loFld.OldValue
  ELSE
    loFld.VALUE = laTemp[1]
    loOgScroll.laOGFxFlt[lnPOS,6] = laTemp[1]
  ENDIF
ENDIF
SELECT APINVHDR
*- End of lfvCurCode.

*!*************************************************************
*! Name      : lfVDtMsg
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 7/26/2004
*! Purpose   : Validation function for date by giving messages.
*!*************************************************************
*! Parameters: The company that you want to validate from its periods.
*!           : Logical parameter to accept the date in a locked period.
*!*************************************************************
*! Returns   : True or False
*!*************************************************************
FUNCTION lfVDtMsg
PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer


PRIVATE llLockStat, lcCompYer, lcCompPrd
PRIVATE lnYear, lcYerPer, lcMessage, lcMessgSel
PRIVATE lcCurYear, lcCurPrd, llVaildDate

lcCompYer  = ' '   && Variable to hlod the current year of the company.
lcCompPrd  = ' '   && Variable to hlod the current period of the company.

lcCurYear  = ' '
lcCurPrd   = ' '

llVaildDate= .F.   && Variable to indicate if the date is valid or not

lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)

llLockPer  = IIF(llLockPer,llLockPer,.F.)

lnYear     = 0     && Variable to hold the year No.

llLockStat = .F.   && Variable to hold the lock stat of the period.

ldDateTVal = IIF(TYPE('ldDateTVal')='O',ldDateTVal.Text1.VALUE,ldDateTVal)

IF lfVlDate(lcCompany,lcCrPrd,lcCrYer,ldDateTVal)  && Call the date validation function.

  lcCrPrd  = lcCurPrd
  lcCrYer  = lcCurYear
  lcYerPer = lcCurPrd+'-'+lcCurYear


  lnYear   = (INT(VAL(lcCurYear))-INT(VAL(lcCompYer))) + 3


  lnYear   = IIF(lnYear <= 0,1,lnYear)

  IF lnYear = 3
    lnYear  = lnYear + SIGN(VAL(lcCrPrd) - VAL(oAriaApplication.CurrentPeriod))
  ENDIF  && End Period Validation

  DO CASE
  CASE llLockStat


    =gfModalGen("TRM00134B00000","DIALOG",lcYerPer)
    llVaildDate = .F.

  CASE lnYear > 0
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *lcMessage  = 'history prior   current  future  '
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcMessage  = LANG_APMNCHP_HISTORYPERIOR
    lcMessage  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_HISTORYPERIOR,loFormSet.GetHeaderText("LANG_APMNCHP_HISTORYPERIOR",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    lcMessgSel = ALLTRIM(SUBSTR(lcMessage,(lnYear*8)-7,8))


    IF lnYear <> 3
      =gfModalGen("TRM00136B00000","DIALOG",lcMessgSel+"|"+lcYerPer)
    ENDIF

    IF lnYear = 1   && If the year = 1 we are not going to accept.
      llVaildDate = .F.
    ELSE
      llVaildDate = .T.
    ENDIF
  ENDCASE
ELSE

  =gfModalGen("TRM00133B00000","DIALOG")

  llVaildDate = .F.
ENDIF

RETURN llVaildDate

*--end of lfVDtMsg


*!*************************************************************
*! Name      : lfVlDate
*! Developer : Ahmad Shoukry Mohammed (ASM)
*! Date      : 7/26/2004
*! Purpose   : To Validate dates from the periods file.
*!*************************************************************
*! Parameters: The company that you want to validate from its periods.
*!           : Logical parameter to accept the date in a locked period.
*!*************************************************************
*! Returns   : True or False
*!*************************************************************
FUNCTION lfVlDate
PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer
LOCAL ap1

PRIVATE llOpenPrd, lcSavOrdPr, llValidDate, lcExactStat

IF TYPE('lcCurYear') <> 'C' .AND. TYPE('lcCurPrd') <> 'C'
  PRIVATE lcCurYear, lcCurPrd, lLockStat

  lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

  lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)
ENDIF

llLockPer  = IIF(llLockPer,llLockPer,.F.)

llValidDate= .F.      && Variable to return with from the function.

lcCurYear  = ' '      && Variable to hold the current year.
lcCurPrd   = ' '      && Varibale to hold the current period.
lcExactStat= ' '      && Variable to hold the SET EXACT status.
lLockStat  = .F.      && Variable to hold the lock stat of the record.

lcSavSelct = ALIAS()  && Variable to save the currently selected file.

llOpenPrd  = .F.      && Variable to indicate that the there is a file;
&& OPEN BY the FUNCTION.

ap1 = CREATEOBJECT("ap")

*PRIVATE llOpAPS, llOpcomp

*llOpcomp = gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','Ccomp_id','SH')  &&lower
*llOpAPS  = gfOpenFile(oAriaApplication.DataDir+'APSETUP','','SH')  &&lower

lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)

lcCompYer  = oAriaApplication.CurrentYear
lcCompPrd  = oAriaApplication.CurrentPeriod



lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)


lcCompYer  = oAriaApplication.CurrentYear
lcCompPrd  = oAriaApplication.CurrentPeriod

IF !USED('FSPRD')   && Check if the period file is open or not.
  llOpenPrd = .T.      && Indicates that the file is open by the function.
  SELECT 0             && Select an empty work area.
  *lcDataDir = ALLTRIM(IIF(SEEK(oAriaApplication.PrntCompanyID, 'SYCCOMP'), gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), oAriaApplication.DataDir))  &&lower  &&lower
  lcdatadir = ap1.lcDataDir

  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
  *USE &lcDataDir.fsprd ORDER TAG COMFYRPRDI
  =gfOpenTable('fsprd','COMFYRPRDI','SH')
  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]

ELSE
  SELECT FSPRD      && The file is open so we are going to select

  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
  *!*    lcSavOrdPr = SYS(22) && Save the file order
  *!*    SET ORDER TO TAG COMFYRPRDI   && Change the order
  lcSavOrdPr = ORDER() && Save the file order
  =gfSetOrder('COMFYRPRDI')
  *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
ENDIF
*IF USED('SYCCOMP') .AND. llOpcomp
*=gfCloseFile('SYCCOMP')
*ENDIF
*IF USED('APSETUP') .AND. llOpAPS
*=gfCloseFile('APSETUP')
*ENDIF

IF TYPE('ldDateTVal') <> 'D'
  ldDate = IIF(TYPE('_screen.ActiveForm.ActiveControl.Value')='D',_SCREEN.ACTIVEFORM.ACTIVECONTROL.VALUE,{})
ELSE
  ldDate = ldDateTVal
ENDIF
*ldDate = ldDateTVal

lcExactStat = SET('EXACT')
SET EXACT OFF
GO TOP

SET EXACT &lcExactStat


LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
IF FOUND() .AND. BETWEEN(ldDate,ap1.ldPyBgDate,ap1.ldNyEnDate)

  llLockStat = lFspLocks  && Assign the variable with the period lock stat.
  lcCurYear  = cFisFYear  && Assign the variable with fiscal year of the period.
  lcCurPrd   = cFspprdid  && Assign the variable with the period no.
  lcCrYer    = cFisFYear  && Assign the variable with fiscal year of the period.
  lcCrPrd    = cFspprdid  && Assign the variable with the period no.
  llLockPer  = lFspLocks  && Assign the variable with the period lock stat.
  llValidDate= .T.        && Assign the variable with .T.

  IF USED('FSPRD') .AND. llOpenPrd
    llOpenPrd = .F.
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *USE IN FSPRD
    =gfCloseTable('FSPRD')
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[END]
  ELSE
    SELECT FSPRD
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *SET ORDER TO &lcSavOrdPr
    =gfSetOrder(lcSavOrdPr)
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
  ENDIF

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF
ELSE
  IF USED('FSPRD') .AND. llOpenPrd
    llOpenPrd = .F.
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *USE IN FSPRD
    =gfCloseTable('FSPRD')
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
  ELSE
    SELECT FSPRD
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[Start]
    *SET ORDER TO &lcSavOrdPr
    =gfSetOrder(lcSavOrdPr)
    *! E302623,1 MMT 08/26/2009 Convert Payable invoice screen to use SQL tables[End]
  ENDIF

  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF
ENDIF

RETURN llValidDate

*--end of lfVlDate


*!*************************************************************
*! Name      : lfCallDetFrm
*! Developer : Mariam MAzhar(MMT)
*! Date      : 10/26/2009
*! Purpose   : Debit/Invoice Summary Screen call
*!*************************************************************
*
FUNCTION lfCallDetFrm
LPARAMETERS lcType,loFormSet
DO FORM (oAriaApplication.ScreenHome+"\AP\APAPLDI.SCX") WITH lcType,loFormSet
*!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[Start]
*!*************************************************************
*! Name      : lfVSelCan
*! Developer : Mariam MAzhar(MMT)
*! Date      : 06/25/2012
*! Purpose   : Cancel Pay selected invoice screen
*!*************************************************************
FUNCTION lfVSelCan
LPARAMETERS loFormSet
lnCurAls = SELECT()
IF loFormset.lnPyChMN = 1  && Calling from manual check.
  SELECT (loFormSet.lcAPCHECKS)
  REPLACE NCHKNXTMN WITH NCHKNXTMN-1
ENDIF
SELECT(lnCurAls)
*!B609975,1 MMT 06/25/2012 Manual Check payment screen displays incorrect Check#[End]


*!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [Start]
*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Saber A Razek
*! Date      : 10/01/2012
*! Purpose   : To object lock any record in any file
*!*************************************************************
FUNCTION lfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag

PRIVATE lnOldrpSt

lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

IF !USED('SYUUSER')
  =gfOpenFile(ADDBS(ADDBS(oAriaApplication.ClientA27Path)+'SysFiles')+'SYUUSER','CUSER_ID','SH','SYUUSER')   && CUSER_ID
ENDIF
IF !USED('SYUSTATC')
  =gfOpenFile(ADDBS(oAriaApplication.cAria4SysPath)+'SYUSTATC','CUSER_ID','SH','SYUSTATC')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
ENDIF
SELECT  APINVHDR
DO WHILE .T.
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
    llLocked = RLOCK()
    IF DELETED()
      UNLOCK
      =gfModalGen('INM00095B00000','ALERT')
      laScrMode     = .F.
      laScrMode [1] = .T.
      SHOW GETS
      RETURN .F.
    ENDIF
  ENDIF

  *** Chek if the record is in use by another user
  IF lLok_Set
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      IF !EMPTY(cLok_User)
        lnOldrpSt = SET('REPROCESS')
        SET REPROCESS TO 1
        IF SEEK ('INI'+'OLDVARS'+cLok_User,'SyuStatc')
          UNLOCK
          lnSavRec   = IIF(RECNO('SYUUSER')>RECCOUNT('SYUUSER'),0, RECNO('SYUUSER'))
          lcLok_User = ALLTRIM(PROPER(LOOKUP(syuUser.cUsr_name,cLok_User, syuUser.cUser_id,'cUser_id')))
          IF lnSavRec > 0
            GO lnSavRec IN SYUUSER
          ENDIF

          *** Record is in use by user ????
          *N000682,1 MMT 11/22/2012 Globalization changes[Start]
          *lcRtyCncMs = "Invoice "+ALLTRIM(APINVHDR.CINVNO)+" for vendor "+ ALLTRIM(APINVHDR.CVendCode)+" is being edited by user " + lcLok_User+"."
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *lcRtyCncMs = LANG_APMNCHP_INVOICE_S+ALLTRIM(APINVHDR.CINVNO)+LANG_APMNCHP_FORVENDOR+ ALLTRIM(APINVHDR.CVendCode)+LANG_APMNCHP_BEINGEDITED + lcLok_User+"."
          lcRtyCncMs = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_INVOICE_S,loFormSet.GetHeaderText("LANG_APMNCHP_INVOICE_S",loFormSet.HeaderAlias))+ALLTRIM(APINVHDR.CINVNO)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_FORVENDOR,loFormSet.GetHeaderText("LANG_APMNCHP_FORVENDOR",loFormSet.HeaderAlias))+ ALLTRIM(APINVHDR.CVendCode)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_APMNCHP_BEINGEDITED,loFormSet.GetHeaderText("LANG_APMNCHP_BEINGEDITED",loFormSet.HeaderAlias)) + lcLok_User+"."
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/22/2012 Globalization changes[END]
          IF  gfModalGen("INM00274B00015","ALERT",lcRtyCncMs) = 1
            LOOP
          ENDIF
          lLok_It    = .F.
          lRet_Flag  = .F.
        ELSE
          lLok_It    = .T.
        ENDIF
        SET REPROCESS TO  lnOldrpSt
      ELSE
        *** Display the message "Record is in use by another"
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
        TYPE ('dLok_Date') <> "U" .AND. ;
        TYPE ('cLok_Time') <> "U"

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;
        cLok_User WITH ""  , ;
        dLok_Date WITH {}  , ;
        cLok_Time WITH ""
      lRet_Flag  = .T.
    ENDIF
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
IF lLok_It
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
      TYPE ('dLok_Date') <> "U" .AND. ;
      TYPE ('cLok_Time') <> "U"
    *** Lock the record for this user with date and time
    WAIT WINDOW NOWAIT ' ... Locking ... '
    REPLACE lLok_Stat WITH .T. ,;
      cLok_User WITH oAriaApplication.User_ID ,;
      dLok_Date WITH DATE() ,;
      cLok_Time WITH gfGetTime()
    =gfTableUpdate()
    WAIT CLEAR
    lRet_Flag  = .T.
  ENDIF
ENDIF

UNLOCK

RETURN lRet_Flag
*!B610105,1 SAB 10/01/2012 Fix problem of adding 2 payments for the same user at the same time [End]
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
************************************************************************************
*Name      : lfSetCrVal
*Developer :Hesham Elmasry(HES)
*Date      :01/03/2013 [Start]
*Purpose   :Set the criteria value to the filter arrays
************************************************************************************
FUNCTION lfSetCrVal
PARAMETERS lcFltFld, lcValue


LOCAL lnPos
lnPos = ASCAN(laOgFxFlt,lcFltFld)
IF lnPos > 0
  lnPos = ASUBSCRIPT(laOgFxFlt,lnPos,1)
  laOgFxFlt[lnPos,6] = lcValue
ENDIF
lnPos = ASCAN(laOgVrFlt,lcFltFld)
IF lnPos > 0
  lnPos = ASUBSCRIPT(laOgVrFlt,lnPos,1)
  laOgVrFlt[lnPos,6] = lcValue
ENDIF
*- End of lfSetCrVal.
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]

*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [Start]
*!*************************************************************
*! Name      : lfOgWhen
*! Developer : Hesham Elmasry(HES)
*! Date      : 01/02/2012
*! Purpose   : Option grid when function
*!*************************************************************
FUNCTION lfOgWhen
=lfSetCrVal("APINVHDR.CVENDCODE",lcOgVendCode)
=lfSetCrVal("APINVHDR.CBNKCODE",lcOgBNKCODE)
=lfSetCrVal("APINVHDR.CCHKACCT",lcOgCHKACCT)
=lfSetCrVal("APINVHDR.CAPRCURCOD",lcOgCURRCODE)
*End function lfOgWhen
*!* E303342, HES 01/05/2012 Disable the OG while working except from the scope button [End  ]
