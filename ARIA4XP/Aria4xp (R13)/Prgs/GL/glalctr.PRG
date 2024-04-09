*:************************************************************************
*:  Program File: ARIA4XP\PRGS\GL\GLALCTR.PRG
*:  Module      : General Ledger
*:  Desc.       : Allocation Entry
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/30/2012
*:  Reference   : *E303280
*:************************************************************************
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\gl\GLALCTR.h
*N000682,1 MMT 11/22/2012 Globalization changes[End]
*- Get the screen , call it
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
  .cBrowseIndexExpression = "CAUTTYPE+CAUTCODE"
  .cBrowseIndexFields     = "CAUTTYPE,CAUTCODE"
  .cBrowseIndexName       = "TYPECODE"
  .cBrowseAliasName       = .lcBaseFile
  .cBrowseTableName       = .lcBaseFile
  .cBrowseFilter          = "CAUTTYPE='A'"
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *.BrowseTitle            = 'Allocation Entry'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.BrowseTitle            = LANG_ALLOCENTRY
.BrowseTitle            = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALLOCENTRY,loFormSet.GetHeaderText("LANG_ALLOCENTRY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
  .Ariaform1.lcAcSegDes.Caption  = .lcAcSegDes
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  .ariaBrFields.edtBrowseFields.Value = "CAUTCODE  :H='Automatic entry code',"+;
*!*	                                        "CAUTREF   :H='Reference',"+;
*!*	                                        "CAUTDES   :H='Description',"+;
*!*	                                        "CACTCODE  :H='Allocation account code',"+;
*!*	                                        "CAUTBASE  :H='Automatic entry base',"+;
*!*	                                        "DAUTBGDAT :H='Recurring begin date',"+;
*!*	                                        "DAUTENDAT :H='Recurring end date',"+;
*!*	                                        "DAUTLGDAT :H='Last recurring gen. date',"+;
*!*	                                        "DAUTNGDAT :H='Next recurring gen. date',"+;
*!*	                                        "CAUTFRQUN :H='Frequency unit',"+;
*!*	                                        "NAUTFRQNO :H='Frequency number',"+;
*!*	                                        "CAUTREV   :H='Reverse transaction',"+;
*!*	                                        "CSRCJRNL  :H='Source journal entry'"
  .ariaBrFields.edtBrowseFields.Value = "CAUTCODE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOMAICENTRYCODE,loFormSet.GetHeaderText("LANG_AUTOMAICENTRYCODE",loFormSet.HeaderAlias))  +"',"+;
                                        "CAUTREF   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REFERENCE,loFormSet.GetHeaderText("LANG_REFERENCE",loFormSet.HeaderAlias)) +"',"+;
                                        "CAUTDES   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESCRIPTION,loFormSet.GetHeaderText("LANG_DESCRIPTION",loFormSet.HeaderAlias))+"',"+;
                                        "CACTCODE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALLOCATIONACCOUNTCODE,loFormSet.GetHeaderText("LANG_ALLOCATIONACCOUNTCODE",loFormSet.HeaderAlias)) +"',"+;
                                        "CAUTBASE  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUOMATICENTRYBASE,loFormSet.GetHeaderText("LANG_AUOMATICENTRYBASE",loFormSet.HeaderAlias))+"',"+;
                                        "DAUTBGDAT :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECURRBEGINDATE,loFormSet.GetHeaderText("LANG_RECURRBEGINDATE",loFormSet.HeaderAlias)) +"',"+;
                                        "DAUTENDAT :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECURRENDDATE,loFormSet.GetHeaderText("LANG_RECURRENDDATE",loFormSet.HeaderAlias))+"',"+;
                                        "DAUTLGDAT :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LASTRECURR,loFormSet.GetHeaderText("LANG_LASTRECURR",loFormSet.HeaderAlias)) +"',"+;
                                        "DAUTNGDAT :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NEXTRECURR,loFormSet.GetHeaderText("LANG_NEXTRECURR",loFormSet.HeaderAlias)) +"',"+;
                                        "CAUTFRQUN :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FREQUENCYUNIT,loFormSet.GetHeaderText("LANG_FREQUENCYUNIT",loFormSet.HeaderAlias)) +"',"+;
                                        "NAUTFRQNO :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FREQUENCYNUMBER,loFormSet.GetHeaderText("LANG_FREQUENCYNUMBER",loFormSet.HeaderAlias))+"',"+;
                                        "CAUTREV   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REVERSETRANSACTION,loFormSet.GetHeaderText("LANG_REVERSETRANSACTION",loFormSet.HeaderAlias))+"',"+;
"CSRCJRNL  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOURCEJOURNAL,loFormSet.GetHeaderText("LANG_SOURCEJOURNAL",loFormSet.HeaderAlias)) +"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
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

*- Define Grid
=lfSetGridDataSource(loFormSet)

loFormSet.ChangeMode('S')

*- Adjust columns width in the popups listed
lfColumnWidthes(loFormSet.Ariaform1.puType)

*- End of lfFormInit.
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


loFormSet.AddProperty('lcObjState','ENABLE')
loFormSet.AddProperty('laTSrcJrnl[1,2]')
loFormSet.AddProperty('laType[2,2]')

lcVars =     'laType     , lcTDfRef   , lcTDfDesc  , lcTAllcPrc ,'+;
             'lcTAllcTrs , lcTDrOrCr  , lcTAllPrcs , lcTTotal1  ,'+;
             'lcTTotal2  , lcTDebit   , lcTCredit  , lcTAlloc   ,'+;
             'lcType     , lcSrcJrnl  , lcDSrcJrn  , lcTTotals  ,'+;
             'lcFieldStr , lc_TmpAlc  , lcAStamp   , lcOldAcct  ,'+;
             'lcAccDesc1 , lcAccDesc2 , lcCodeType , lcTyp_Stat ,'+;
             'lcAct_Stat , lcDeb_Stat , lcCrd_Stat , lcPer_Stat ,'+;
             'lcNew_Stat , lcRem_Stat,  LCTTMPSAV  , LCTTMPDEL'
=lfAddProp(loFormSet,lcVars,'')
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*loFormSet.laType[1,1] = "Amounts"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laType[1,1] = LANG_AMOUNTS
loFormSet.laType[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AMOUNTS,loFormSet.GetHeaderText("LANG_AMOUNTS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[End]

loFormSet.laType[1,2] = "A"

*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*loFormSet.laType[2,1] = "Percent"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laType[2,1] = LANG_PERCENT
loFormSet.laType[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERCENT,loFormSet.GetHeaderText("LANG_PERCENT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[End]

loFormSet.laType[2,2] = "P"

*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	loFormSet.LCTDFDESC = 'Created by '
*!*	loFormSet.LCTDFREF = 'On'
*!*	loFormSet.LCTALLCPRC = 'allocated percentages '
*!*	loFormSet.LCTALLCTRS = 'allocation transaction|allocation transaction '
*!*	loFormSet.LCTDRORCR = 'either debit or credit '
*!*	loFormSet.LCTALLPRCS = 'the allocation percentage '
*!*	loFormSet.LCTTOTAL1 = 'Totals : '
*!*	loFormSet.LCTTOTAL2 = 'Total : '
*!*	loFormSet.LCTDEBIT = 'Debit'
*!*	loFormSet.LCTCREDIT = 'Credit'
*!*	loFormSet.LCTALLOC = 'Allocation % '
*!*	loFormSet.LCTTMPSAV = 'Saving allocation entry'
*!*	loFormSet.LCTTMPDEL = 'Deleting allocation entry'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTDFDESC = LANG_CREATEDBY
loFormSet.LCTDFDESC = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CREATEDBY,loFormSet.GetHeaderText("LANG_CREATEDBY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTDFREF = LANG_ON
loFormSet.LCTDFREF = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ON,loFormSet.GetHeaderText("LANG_ON",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTALLCPRC = LANG_ALLOCATEDPERCNTAGES
loFormSet.LCTALLCPRC = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALLOCATEDPERCNTAGES,loFormSet.GetHeaderText("LANG_ALLOCATEDPERCNTAGES",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTALLCTRS = LANG_ALLOCATETRANS
loFormSet.LCTALLCTRS = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALLOCATETRANS,loFormSet.GetHeaderText("LANG_ALLOCATETRANS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTDRORCR = LANG_EITHERDEBITCREDIT
loFormSet.LCTDRORCR = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_EITHERDEBITCREDIT,loFormSet.GetHeaderText("LANG_EITHERDEBITCREDIT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTALLPRCS = LANG_THEALLOCPERCENT
loFormSet.LCTALLPRCS = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_THEALLOCPERCENT,loFormSet.GetHeaderText("LANG_THEALLOCPERCENT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTTOTAL1 = LANG_TOTALS
loFormSet.LCTTOTAL1 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TOTALS,loFormSet.GetHeaderText("LANG_TOTALS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTTOTAL2 = LANG_TOTAL1
loFormSet.LCTTOTAL2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TOTAL1,loFormSet.GetHeaderText("LANG_TOTAL1",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTDEBIT = LANG_DEBIT
loFormSet.LCTDEBIT = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DEBIT,loFormSet.GetHeaderText("LANG_DEBIT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTCREDIT = LANG_CREDIT
loFormSet.LCTCREDIT = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CREDIT,loFormSet.GetHeaderText("LANG_CREDIT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTALLOC = LANG_ALLOCPERCENT
loFormSet.LCTALLOC = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALLOCPERCENT,loFormSet.GetHeaderText("LANG_ALLOCPERCENT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTTMPSAV = LANG_SAVING
loFormSet.LCTTMPSAV = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SAVING,loFormSet.GetHeaderText("LANG_SAVING",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.LCTTMPDEL = LANG_DELETEING
loFormSet.LCTTMPDEL = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DELETEING,loFormSet.GetHeaderText("LANG_DELETEING",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[End]
*** Variables declaration
loFormSet.AddProperty('lcOldWnTyp' , "A" )

loFormSet.AddProperty('lcAcctCode' , REPLICATE ("0",loFormSet.lnAcsSegSz)  )

*** list totals string ( amount mode ) ***
loFormSet.AddProperty('lcLsTotal1', "IIF(loformset.lnTmpRcCnt > 0,"+;
                 "'Totals : '+STR(loformset.lnTotDr,15,2)+' '+"+;
                 "STR(loformset.lnTotCr,15,2),SPACE(40))" )
*** list totals string ( percent mode ) ***
loFormSet.AddProperty('lcLsTotal2',  "IIF(loformset.lnTmpRcCnt > 0,"+;
                 "SPACE(1)+'Total : '+SPACE(25)+STR(loformset.lnTotCr,6,2),"+;
                 "SPACE(42))")

loFormSet.AddProperty('lcLsTotal', loFormSet.lcLsTotal1  )
loFormSet.AddProperty('lcTTotals', loFormSet.lcTTotal1   )

lcVars = 'lcAutDtTg  , lcAutDtEx'
=lfAddProp(loFormSet,lcVars,'')

lcVars =    'lnTmpRcCnt , lnOldDr    , lnOldCr    ,'+;
            'lnOldPer   , lnDebit    , lnCredit   , lnAllocPer ,'+;
            'lnTotdr    , lnTotcr    , lnSrcJrnl  , lnDSrcJrn  ,'+;
            'lnAutDtFld '
=lfAddProp(loFormSet,lcVars,0)

lcVars  = 'llFromBton , llBrowse'
=lfAddProp(loFormSet,lcVars,.F.)

loFormSet.AddProperty('llNewDet', .F.)

loFormSet.AddProperty('puSrcJrnl', 1)

=gfOpenFile(gcDataDir+'GLTYPES',gcDataDir+'Typecode','SH')
loFormSet.AddProperty('lnSpaces'   , 1)

loFormSet.AddProperty('lcCurrSign' , gfGetCurSmbl(oAriaApplication.BaseCurrency))        && currency sign

lcUserName = oAriaApplication.User_ID
loFormSet.AddProperty('lcCurDate'  , IIF(SET('CENTURY')='ON',DTOC(oAriaApplication.SystemDate),;
                 LEFT(DTOC(oAriaApplication.SystemDate),6)+STR(YEAR(oAriaApplication.SystemDate),4)) )

lcScFields = 'CAUTTYPE,CAUTCODE,CAUTREF,CAUTDES,CAUTBASE,CACTCODE,CSRCJRNL'
loFormSet.AddProperty('lcScFields',lcScFields)
lcLn = ALLTRIM(STR(OCCURS(',',lcScFields)+1))
loFormSet.AddProperty('laData[&lcLn.]')
SELECT(loFormSet.lcBaseFile)
SCATTER FIELDS &lcScFields TO laData BLANK
loFormSet.laData[1]  = 'A'
loFormSet.laData[6]  = REPLICATE ("0",loFormSet.lnAcsSegSz)
  *** Create a name for the temporary file
loFormSet.lc_TmpAlc = gfTempName()
lc_TmpAlc = loFormSet.lc_TmpAlc

SELECT GLAUTDT

loFormSet.lcAutDtTg = SYS(22)

SET ORDER TO TAG TYPCODACC

*** Current tag expression is cAutType + cAutCode + cAcctCode
loFormSet.lcAutDtEx = SYS(14,VAL(SYS(21)))

*** Create the temporary file with the appropriate fields ,
*** (in this case all fields of file "GLAUTDT" + 2 more fields
*** for the record number and status('M'odify,'D'elete,'A'dd,'S'ame)

DIMENSION laAutDtFld[1,18]
=AFIELDS(laAutDtFld)
lnAutDtFld = ALEN(laAutDtFld,1)
DIMENSION laAutDtFld[lnAutDtFld+2,18]

laAutDtFld[lnAutDtFld+1,1] = 'nRecNo'
laAutDtFld[lnAutDtFld+1,2] = 'N'
laAutDtFld[lnAutDtFld+1,3] = 10
laAutDtFld[lnAutDtFld+1,4] = 0

laAutDtFld[lnAutDtFld+2,1] = 'cStatus'
laAutDtFld[lnAutDtFld+2,2] = 'C'
laAutDtFld[lnAutDtFld+2,3] = 1
laAutDtFld[lnAutDtFld+2,4] = 0

FOR i= lnAutDtFld+1 TO ALEN(laAutDtFld,1)
  STORE .F. TO laAutDtFld[i,5],laAutDtFld[i,6]
  STORE '' TO laAutDtFld[i,7],laAutDtFld[i,8],laAutDtFld[i,9],laAutDtFld[i,10],laAutDtFld[i,11],laAutDtFld[i,12],laAutDtFld[i,13],laAutDtFld[i,14],laAutDtFld[i,15],laAutDtFld[i,16]
  STORE 0 TO laAutDtFld[i,17],laAutDtFld[i,18]
ENDFOR

CREATE TABLE &gcWorkDir.&lc_TmpAlc;
    FROM ARRAY laAutDtFld

** Create source journal array for the source journals popup
=lfSrcJrnl(loFormSet)

loFormSet.lcType    = loFormSet.laType[1,2]
loFormSet.laData[5] = 'A'
loFormSet.laData[7] = loFormSet.lcSJ_Def

SELECT GLAUTHD

SET FILTER TO
*** Filter on allocation transactions records only
SET FILTER TO cAutType="A"
*** Activate filter

*- End of lfDefineVars.

************************************************************
*! Name      : lfEnDsVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/06/2012
*! Purpose   : Define Enable,Disable vars
************************************************************
FUNCTION lfEnDsVars
PARAMETERS loFormSet

IF loFormSet.ActiveMode = 'S' .OR. loFormSet.ActiveMode = 'V'
  STORE "DISABLE" TO loFormSet.lcTyp_Stat,loFormSet.lcAct_Stat,loFormSet.lcDeb_Stat,loFormSet.lcCrd_Stat,;
                     loFormSet.lcPer_Stat,loFormSet.lcNew_Stat,loFormSet.lcRem_Stat
ELSE
  loFormSet.lcTyp_Stat = IIF(loFormSet.lnTmpRcCnt = 0 , "ENABLE" , "DISABLE")
  loFormSet.lcAct_Stat = IIF(loFormSet.lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")

  IF loFormSet.laData[5]='A'
    loFormSet.lcDeb_Stat = IIF(loFormSet.lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
    loFormSet.lcCrd_Stat = IIF(loFormSet.lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
    loFormSet.lcPer_Stat = "DISABLE"
  ELSE
    loFormSet.lcDeb_Stat = "DISABLE"
    loFormSet.lcCrd_Stat = "DISABLE"
    loFormSet.lcPer_Stat = IIF(loFormSet.lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
  ENDIF

  IF loFormSet.lnTmpRcCnt = 0
    loFormSet.lcNew_Stat = IIF(VAL(STRTRAN(loFormSet.laData[6],'-','')) = 0 ,"DISABLE","ENABLE")
  ELSE
    loFormSet.lcNew_Stat = IIF(loFormSet.llNewDet OR (loFormSet.laData[5] = "P" AND loFormSet.lnTotCr=100) OR ;
                 (VAL(STRTRAN(loFormSet.laData[6],'-','')) = 0) ,"DISABLE","ENABLE")
  ENDIF

  loFormSet.lcRem_Stat = IIF(loFormSet.lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")

  loFormSet.lnSrcJrnl = ASCAN(loFormSet.laTSrcJrnl,loFormSet.laData[7])
  puSrcJrnl = IIF(loFormSet.lnSrcJrnl > 0,ASUBSCRIPT(loFormSet.laTSrcJrnl,loFormSet.lnSrcJrnl,1),loFormSet.lnDSrcJrn)
ENDIF

*- End of lfEnDsVars.

************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/23/2012
*! Purpose   : destroy method function
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

  *** reset tag of GLAUTDT
  SELECT GLAUTDT
  IF !EMPTY(loFormSet.lcAutDtTg)
    SET ORDER TO TAG (loFormSet.lcAutDtTg)
  ELSE
    SET ORDER TO
  ENDIF

  *** Close open temporary files.
  IF USED(loFormSet.lc_TmpAlc)
    USE IN ALIAS(loFormSet.lc_TmpAlc)
  ENDIF
  gcWorkDir = oAriaApplication.WorkDir
  lc_TmpAlc = loFormSet.lc_TmpAlc
  ERASE &gcWorkDir.&lc_TmpAlc..dbf

*- End of lfFormDestroy.

************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loFormSet
LOCAL lc_TmpAlc,lcCr,lnWid,lcLn

lcCr = loFormSet.lcCurrSign
lc_TmpAlc = loFormSet.lc_TmpAlc
lnWid = IIF(loFormSet.laData[5]='A',260,310)

WITH loFormSet.Ariaform1.grdGLALCTR
  .RecordSource = ''
  .RecordSource = loFormSet.lc_TmpAlc
  .ColumnCount = IIF( loFormSet.laData[5]='A' , 4 ,3 )
ENDWITH
lcLn = ALLTRIM(STR(loFormSet.lnAcsSegSz))
lfSetColumnsProp('1',"SUBSTR(&lc_TmpAlc..cAcctcode,1,&lcLn)"   ,loFormSet.lcAcSegDes,100)
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*lfSetColumnsProp('2',"LOOKUP(GLACCHAR.cAccnldes,&lc_TmpAlc..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')",'Description'   ,lnWid)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('2',"LOOKUP(GLACCHAR.cAccnldes,&lc_TmpAlc..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')",LANG_DESCRIPTION    ,lnWid)
lfSetColumnsProp('2',"LOOKUP(GLACCHAR.cAccnldes,&lc_TmpAlc..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESCRIPTION,loFormSet.GetHeaderText("LANG_DESCRIPTION",loFormSet.HeaderAlias))    ,lnWid)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[end]
IF loFormSet.laData[5]='A'
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  lfSetColumnsProp('3',"IIF(&lc_TmpAlc..cDrOrCr='D',&lc_TmpAlc..nAmount,0.00)"       ,'Debit '+lcCr,70)
*!*	  lfSetColumnsProp('4',"IIF(&lc_TmpAlc..cDrOrCr='C',&lc_TmpAlc..nAmount,0.00)"       ,'Credit '+lcCr,70)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('3',"IIF(&lc_TmpAlc..cDrOrCr='D',&lc_TmpAlc..nAmount,0.00)"       ,LANG_S_DEBIT +lcCr,70)
lfSetColumnsProp('3',"IIF(&lc_TmpAlc..cDrOrCr='D',&lc_TmpAlc..nAmount,0.00)"       ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_DEBIT,loFormSet.GetHeaderText("LANG_S_DEBIT",loFormSet.HeaderAlias)) +lcCr,70)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('4',"IIF(&lc_TmpAlc..cDrOrCr='C',&lc_TmpAlc..nAmount,0.00)"       ,LANG_S_CREDIT+lcCr,70)
lfSetColumnsProp('4',"IIF(&lc_TmpAlc..cDrOrCr='C',&lc_TmpAlc..nAmount,0.00)"       ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_CREDIT,loFormSet.GetHeaderText("LANG_S_CREDIT",loFormSet.HeaderAlias))+lcCr,70)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
ELSE
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *lfSetColumnsProp('3',"&lc_TmpAlc..nAmount"       ,'Allocation %',70)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lfSetColumnsProp('3',"&lc_TmpAlc..nAmount"       ,LANG_S_ALLOCPERCENT,70)
lfSetColumnsProp('3',"&lc_TmpAlc..nAmount"       ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_ALLOCPERCENT,loFormSet.GetHeaderText("LANG_S_ALLOCPERCENT",loFormSet.HeaderAlias)),70)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[End]
ENDIF
loFormSet.Ariaform1.grdGLALCTR.ReadOnly = .T.
*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS lcCol,lcSrc,lcHeader,lnWidth
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH loFormSet.Ariaform1.grdGLALCTR
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH
*- End of lfSetColumnsProp.

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
*! Name      : lfSetControlSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/05/2012
*! Purpose   : Set the Controls data Source
************************************************************
FUNCTION lfSetControlSource
PARAMETERS loFormSet
WITH loFormSet.Ariaform1
  .laData2.Keytextbox.ControlSource = 'Thisformset.laData[2]'
  .laData2.Keytextbox.MaxLength = FSIZE('CAUTCODE','glautdt')

  .laData3.ControlSource = 'Thisformset.laData[3]'
  .laData3.MaxLength = LEN(loformset.laData[3])

  .laData4.ControlSource = 'Thisformset.laData[4]'
  .laData4.MaxLength = LEN(loformset.laData[4])

  .puType.RowSource = 'Thisformset.laType'
  .puType.ControlSource = 'Thisformset.laData[5]'

  .laData6.Keytextbox.ControlSource = 'Thisformset.laData[6]'
  .lcAccDesc1.ControlSource = 'ThisFormSet.lcAccDesc1'

  .laTSrcJrnl.ControlSource = 'Thisformset.laData[7]'
  .laTSrcJrnl.RowSource = 'Thisformset.laTSrcJrnl'

  .lnDebit.ControlSource = 'ThisFormSet.lnDebit'
  .lnCredit.ControlSource = 'ThisFormSet.lnCredit'
  .lnAllocPer.ControlSource = 'ThisFormSet.lnAllocPer'

  .lcAcctCode.Keytextbox.ControlSource = 'Thisformset.lcAcctCode'
  .lcAccDesc2.ControlSource = 'Thisformset.lcAccDesc2'

ENDWITH
*- End of lfSetControlSource.

************************************************************
*! Name      : lfFormAfterRowColumnChange
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 07/25/2012
*! Purpose   : adjust data value when line changes in the Grid
************************************************************
FUNCTION lfFormAfterRowColumnChange
PARAMETERS loFormSet
lc_TmpAlc = loFormSet.lc_TmpAlc

WITH loFormSet.Ariaform1
  .grdGLALCTR.Refresh()
  IF loFormSet.laData[5] = 'A'
    .lnDebit.Value = IIF(&lc_TmpAlc..CDRORCR = 'D',&lc_TmpAlc..NAMOUNT, 00.0 )
    .lnCredit.Value = IIF(&lc_TmpAlc..CDRORCR = 'C',&lc_TmpAlc..NAMOUNT, 00.0 )
  ELSE
    .lnAllocPer.Value = &lc_TmpAlc..NAMOUNT
  ENDIF
  .lcAcctCode.KeyTextbox.Value = IIF(!EOF(lc_TmpAlc),&lc_TmpAlc..CACCTCODE,REPLICATE ("0",loFormSet.lnAcsSegSz))
  .lcAccDesc2.Value = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpAlc..cAcctcode,GLACCHAR.cAcctcode,'ACCTCODE')
  .lcLsTotal.Value = EVALUATE(loformset.lcLsTotal)
ENDWITH
*- End of lfFormAfterRowColumnChange.

************************************************************
*! Name      : lfCrTemp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/07/2012
*! Purpose   : Create temp file for the Grid
************************************************************
FUNCTION lfCrTemp
PARAMETERS loFormSet
LOCAL lnSlct,laStru[1],lnLen
lnSlct = SELECT(0)
SELECT GLAUTDT
AFIELDS(laStru)

lnLen = ALEN(laStru,1)
DIMENSION laStru[lnLen+2,18]
laStru[lnLen+1,1] = 'nRecNo'
laStru[lnLen+1,2] = 'N'
laStru[lnLen+1,3] = 6
laStru[lnLen+1,4] = 0

laStru[lnLen+2,1] = 'cStatus'
laStru[lnLen+2,2] = 'C'
laStru[lnLen+2,3] = 1
laStru[lnLen+2,4] = 0

FOR lnI = lnLen+1 TO ALEN(laStru,1)
  STORE .F. TO laStru[lnI,5],laStru[lnI,6]
  FOR lnJ = 7 TO 16
    laStru[lnI,lnJ] = ""
  ENDFOR
  STORE 0 TO laStru[lnI,17],laStru[lnI,18]
ENDFOR

CREATE TABLE (oAriaApplication.WorkDir+loFormSet.lc_TmpAlc) FROM ARRAY laStru

SELECT (lnSlct)
*- End of lfCrTemp.

*!**************************************************************************
*!
*!      FUNCTION : lpShow
*!
*!**************************************************************************
*
************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/01/2012
*! Purpose   : Change mode action
************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet

IF TYPE('loFormSet.lcProgName')='U'
  RETURN
ENDIF

SELECT (loFormSet.lcBaseFile)
lcScFields = loFormSet.lcScFields
IF loFormSet.ActiveMode $ 'VE'
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData
ELSE
  lcNewCode = loFormSet.laData[2]
  SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK
  loFormSet.laData[2] = lcNewCode
ENDIF

lfSetControlSource(loFormSet)

gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir
lc_TmpAlc = loFormSet.lc_TmpAlc
lcAutDtEx = loFormSet.lcAutDtEx

loFormSet.laData[1] = 'A'

WITH loFormSet.Ariaform1
  .laData2.Enabled = .F.
ENDWITH

=lfEnDsVars(loFormSet)
DO CASE
  *** "Select" mode (
  CASE loFormSet.ActiveMode = 'S'
    
    *B610269,1 media fixing problems TMI 03/16/2013 [Start] empty ThisFormset.Ariaform1.lcAccDesc1.Value
    loFormset.Ariaform1.lcAccDesc1.Value = ' '
    *B610269,1 media fixing problems TMI 03/16/2013 [End  ] 

    SELECT (loFormSet.lcBaseFile)
    GO TOP

    SCATTER FIELDS &lcScFields MEMO TO loFormSet.laData BLANK

    SELECT (lc_TmpAlc)

    *** Delete old data ( if any )
    ZAP

    *** Initialize variables for display
    loFormSet.lnTmpRcCnt = 0       && No nonempty records in the temporary file

    *** Reset defaults :
    loFormSet.laData[5]  = 'A'      && Initialise to 'A'mounts
    loFormSet.laData[6]  = REPLICATE ("0",loFormSet.lnAcsSegSz)
                            && corresponding to allocation accout code
    loFormSet.laData[7]  = loFormSet.lcSJ_Def
    loFormSet.lcType     = loFormSet.laType[1,2]       && Initialise to 'A'mounts

    loFormSet.lcAcctCode = REPLICATE ("0",loFormSet.lnAcsSegSz)
    lcAccDesc1 = SPACE(60)
    loFormSet.lcAccDesc2 = SPACE(60)
    lcOldAcct  = ''
    loFormSet.lcOldWnTyp = " "

    loFormSet.lnCredit = 0
    loFormSet.lnDebit = 0
    loFormSet.lnAllocPer = 0
    loFormSet.lnTotDr = 0
    loFormSet.lnTotCr = 0
    loFormSet.lnOldDr = 0
    loFormSet.lnOldCr = 0
    loFormSet.lnOldPer = 0

    loFormSet.laData[5] = loFormSet.laType[1,2]

    puSrcJrnl  = loFormSet.lnDSrcJrn

    WITH loFormSet.Ariaform1
      .laData2.Enabled = .T.
    ENDWITH

    *** Choose window 1 as a default
    DO lpSetWinds WITH 1

  *** "View" mode (loFormSet.ActiveMode = 'V'=.T.), or "Edit" mode (loFormSet.ActiveMode = 'E'=.T.)
  CASE loFormSet.ActiveMode = 'V' .OR. loFormSet.ActiveMode = 'E'
    loFormSet.lnDebit    = 0
    loFormSet.lnCredit   = 0
    llFirstRec = .T.
    loFormSet.lnSrcJrnl  = ASCAN(loFormSet.laTSrcJrnl,loFormSet.laData[7])

    puSrcJrnl = IIF(loFormSet.lnSrcJrnl > 0,;
                   ASUBSCRIPT(loFormSet.laTSrcJrnl,loFormSet.lnSrcJrnl,1),loFormSet.lnDSrcJrn)

    loFormSet.lcAccDesc1 = LOOKUP(GLACCHAR.cAccnlDes,loFormSet.laData[6],;
                        GLACCHAR.cAcctCode,'ACCTCODE')

    *** Get the type of the allocation account
    IF loFormSet.ActiveMode = 'E'
      loFormSet.lcCodeType  = IIF(LEFT(LTRIM(LOOKUP(GLACCHAR.cTypeCode,loFormSet.laData[6],;
                       GLACCHAR.cAcctCode,'ACCTCODE')),1)="Y","S","T")
    ENDIF

    loFormSet.lcAStamp = IIF(loFormSet.ActiveMode = 'V',GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                   +GLAUTHD.cAdd_Time,loFormSet.lcAStamp)
    IF loFormSet.ActiveMode = 'V' .OR. loFormSet.lcAStamp <> GLAUTHD.cAdd_User+;
                    DTOC(GLAUTHD.dAdd_Date)+GLAUTHD.cAdd_Time
      loFormSet.lnTotDr = 0
      loFormSet.lnTotCr = 0


      SELECT &lc_TmpAlc
      ZAP

      SELECT GLAUTDT
      =SEEK("A"+loFormSet.laData[2])

      SCAN REST WHILE CAUTTYPE+CAUTCODE+CACCTCODE = "A"+loFormSet.laData[2]

        =lfTotals(AT(loFormSet.laData[5],'AP'))

        SCATTER MEMVAR memo
        m.nRecNo = RECNO()
        m.cStatus = 'S'
        INSERT INTO (gcWorkDir+lc_TmpAlc) FROM MEMVAR
      ENDSCAN

    ENDIF

    SELECT (lc_TmpAlc)
    LOCATE

    *** Get the number of records currently in the temporary file
    loFormSet.lnTmpRcCnt = RECCOUNT(lc_TmpAlc)


    *** Considering the fact that there has to be at least one record
    *** in the temporary file,( due to the fact that we cannot "Save"
    *** an empty transaction,there seems to be no reason for checking
    *** for the number of records in the file,or to initialize variables
    *** to spaces,hence,

    loFormSet.lcAcctCode = cAcctCode
    loFormSet.lcAccDesc2 = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpAlc..cAcctCode,;
                       GLACCHAR.cAcctCode,'ACCTCODE')

    IF loFormSet.laData[5] = "A"
      IF cDrorCr = "D"
        loFormSet.lnDebit  = nAmount
      ELSE
        loFormSet.lnCredit = nAmount
      ENDIF
    ELSE
      loFormSet.lnAllocPer = nAmount
    ENDIF
    lfShowAmntFlds(loFormSet)

    IF !EMPTY(loFormSet.laData[5])
      DO lpSetWinds WITH AT(loFormSet.laData[5],'AP')
    ENDIF


    *** Prepare push buttons
    loFormSet.lcObjState = IIF(loFormSet.ActiveMode = 'E',"ENABLE","DISABLE")

    WITH loFormSet.Ariaform1
      .laData6.Enabled = loFormSet.lcObjState = 'ENABLE'
      .pbNew.Enabled = loFormSet.lcObjState = 'ENABLE'
      .pbRem.Enabled = loFormSet.lcObjState = 'ENABLE'
      .lcAcctCode.Enabled = loFormSet.lcObjState = 'ENABLE'
      .lnDebit.Enabled = loFormSet.lcObjState = 'ENABLE'
      .lnCredit.Enabled = loFormSet.lcObjState = 'ENABLE'
      .lnAllocPer.Enabled = loFormSet.lcObjState = 'ENABLE'
    ENDWITH

    loFormSet.lcObjState = IIF(loFormSet.ActiveMode = 'V'.OR.(loFormSet.ActiveMode = 'E'.AND.loFormSet.laData[5]='P';
                    .AND. loFormSet.lnTotCr=100),"DISABLE","ENABLE")
    loFormSet.Ariaform1.pbNew.Enabled =  loFormSet.lcObjState = 'ENABLE'

    loFormSet.Ariaform1.puType.Enabled = .F.

  *** "Add" mode (loFormSet.ActiveMode = 'A'=.T.)
  CASE loFormSet.ActiveMode = 'A'
    *** Prepare defaults
    *** Remember that lcCurDate's length depends upon the century setting.
    lcDfRef    = loFormSet.lcTDfRef+' '+loFormSet.lcCurDate
    loFormSet.laData[3]  = lcDfRef+SPACE(FSIZE('cAutRef','GLAUTHD')-LEN(lcDfRef))
    loFormSet.laData[4]  = SUBSTR(loFormSet.lcTDfDesc+' '+oAriaApplication.User_ID,1,;
                       FSIZE('cAutDes','GLAUTHD'))

    loFormSet.laData[5] = 'A'
    loFormSet.laData[6]  = REPLICATE ("0",loFormSet.lnAcsSegSz)
    loformset.laData[7] = loFormSet.lcSJ_Def

    SELECT (lc_TmpAlc)
    ZAP

    WITH loFormSet.Ariaform1
      .laData6.Enabled = .T.
      .lnDebit.Enabled = .F.
      .lnCredit.Enabled = .F.
      .lnAllocPer.Enabled = .F.
      .lcAcctCode.Enabled = .F.
      .pbNew.Enabled = .F.
      .pbRem.Enabled = .F.
    ENDWITH
ENDCASE

*- Reset grid columns
=lfSetGridDataSource(loformSet)
=lfFormAfterRowColumnChange(loformSet)

loFormSet.Ariaform1.Refresh()

SELECT GLAUTHD
*- End of lfChangeMode.

*!**************************************************************************
*!
*!      Function : lfTotals
*!
*!*************************************************************************
*    This function gets the total debits and credits,called
*    while SELECT_SQL command is executed.
*
FUNCTION lfTotals
PARAMETERS lnType

*** If Dollars screen and account is Debit,add it to debits totals
*** If Dollars screen and account is Credit,add it to credits totals
*** If it is percentage screen,add amounts field.The same field
*** of credits totals (lnTotCr) is used for optimization.
IF lnType = 1 .AND. cDrOrCr = "D"
  loFormSet.lnTotDr = loFormSet.lnTotDr + nAmount
ELSE
  loFormSet.lnTotCr = loFormSet.lnTotCr + nAmount
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*  VALID function for the get field "laData[2]" corresponding
*  to field GLAUTHD.cAutCode that constitutes a part of a
*  two field primary key : GLAUTHD.cAutType + GLAUTHD.cAutCode
*
FUNCTION lfvData_2
PARAMETERS loFormSet,loFld
llBrowse = loFld.Selectedfrombrowse

loFormSet.laData[1] = 'A'
IF llBrowse .OR. !EMPTY(loFormSet.laData[2])
  IF llBrowse
    IF loFormSet.oToolBar.cmdFind.Click()
      *loFormSet.ChangeMode('V')
    ELSE
      loFormSet.laData[2] = ' '
      RETURN .F.
    ENDIF
  ELSE
    loFormSet.laData[2] = ALLTRIM(loFormSet.laData[2])
    loFormSet.laData[2] = IIF(ISDIGIT(LEFT(loFormSet.laData[2],1)),;
                   PADL(loFormSet.laData[2], FSIZE('cAutCode','GLAUTHD'),'0'),;
                   PADR(loFormSet.laData[2], FSIZE('cAutCode','GLAUTHD'),SPACE(1)))

    IF !SEEK(loFormSet.laData[1]+loFormSet.laData[2],loFormSet.lcBaseFile)
      **** \!\<Browse;\<Add;\?\<Reenter
      *N000682,1 MMT 11/22/2012 Globalization changes[Start]
      *lnResp = gfModalGen('INM00001B02004','DIALOG','Automatic Entry Code:'+loFormSet.laData[2])
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnResp = gfModalGen('INM00001B02004','DIALOG',LANG_S_AUTO+loFormSet.laData[2])
*B610269,1 media fixing problems TMI 03/16/2013 [Start] if '?' in laData[2] then browse immediatly
IF !'?' $ loFormSet.laData[2]
  *B610269,1 media fixing problems TMI 03/16/2013 [End  ] 
  lnResp = gfModalGen('INM00001B02004','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_AUTO,loFormSet.GetHeaderText("LANG_S_AUTO",loFormSet.HeaderAlias))+loFormSet.laData[2])
  *B610269,1 media fixing problems TMI 03/16/2013 [Start] 
ELSE
  lnResp = 1
ENDIF   
*B610269,1 media fixing problems TMI 03/16/2013 [End  ]   
  
  *N000682,1 11/20/2012 MMT Globlization changes[End] 
      *N000682,1 MMT 11/22/2012 Globalization changes[End]
      
      DO case
      CASE lnResp = 1
        IF !loFormSet.oToolBar.cmdFind.Click()
          RETURN .F.
        ENDIF
      CASE lnResp = 2
        loFormSet.ChangeMode('A')
      CASE lnResp = 3
        loFormSet.laData[2] = ' '
        RETURN .F.
      ENDCASE
    ELSE
      *- the key is there, go to view mode
      loFormSet.ChangeMode('V')
    ENDIF
  ENDIF
ENDIF
*- End of lfvData_2.


*!**************************************************************************
*!
*!      Function: lfvType
*!
*!**************************************************************************
*
FUNCTION lfvType
PARAMETERS loFormSet,loFld
lfShowAmntFlds(loFormSet)
lfSetGridDataSource(loFormSet)

IF !EMPTY(loFormSet.laData[5])
  DO lpSetwinds WITH AT(loFormSet.laData[5],'AP')
ENDIF

************************************************************
*! Name      : lfShowAmntFlds
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/07/2012
*! Purpose   : Show appropriate fields
************************************************************
FUNCTION lfShowAmntFlds
PARAMETERS loformSet
LOCAL llShow
llShow = ( loFormSet.laData[5] = 'A' )

WITH loFormSet.Ariaform1
  .lnAllocPer.Visible = !llShow
  .lnDebit.Visible = llShow
  .lnCredit.Visible = llShow

  .lblAllocPer.Visible = !llShow
  .lblDebit.Visible = llShow
  .lblCredit.Visible = llShow
ENDWITH

*- End of lfShowAmntFlds.

*!**************************************************************************
*!
*!      Function: lfvSrcJrnl
*!
*!**************************************************************************
*
FUNCTION lfvSrcJrnl
PARAMETERS loFormSet,loFld


*!**************************************************************************
*!
*!      FUNCTION : lpSetWinds
*!
*!**************************************************************************
*  This FUNCTION handles switching between the two screen layouts,
*  according to the base type of the allocation account,whether it
*  is in Amount (Dollars) or Percent
*
FUNCTION lpSetwinds
PARAMETERS lnOption
PRIVATE lcStr,lcI
lcI = ALLTRIM(STR(lnOption))
IF loFormSet.laData[5]<>loFormSet.lcOldWnTyp
  loFormSet.lcOldWnTyp = loFormSet.laData[5]

  loFormSet.lcTTotals  = loFormSet.lcTTotal&lcI

  loFormSet.lcLsTotal  = loFormSet.lcLsTotal&lcI

ENDIF

*!**************************************************************************
*!
*!      Function: lswAlcDet
*!
*!**************************************************************************
FUNCTION lswAlcDet

*** IF There are no records in the list,
*** prohibit selection from list.
lc_TmpAlc = loFormSet.lc_TmpAlc

IF  loFormSet.lnTmpRcCnt = 0
  RETURN .F.
ELSE
  *** If current record has no account code entry nor an amount entry,
  *** prohibit selection from list.
  IF VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0 ;
        .OR. (loFormSet.laData[5] = 'A'.AND. loFormSet.lnDebit = 0 .AND. loFormSet.lnCredit = 0);
        .OR. (loFormSet.laData[5] = 'P'.AND. loFormSet.lnAllocPer = 0)
    RETURN .F.
  ENDIF
ENDIF

SELECT (lc_TmpAlc)

*** Refresh get fields with current contents
loFormSet.lcAcctCode = cAcctCode
loFormSet.lcAccDesc2 = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpAlc..cAcctCode,;
                   GLACCHAR.cAcctCode,'ACCTCODE')
IF loFormSet.laData[5] = "A"
  IF cDrOrCr = "D"
    loFormSet.lnDebit  = nAmount
    loFormSet.lnCredit = 0
  ELSE
    loFormSet.lnCredit = nAmount
    loFormSet.lnDebit  = 0
  ENDIF
ELSE
  loFormSet.lnAllocPer = nAmount
ENDIF

IF loFormSet.laData[5] = 'A'
  SHOW GET lnDebit
  SHOW GET lnCredit
ELSE
  SHOW GET lnAllocPer
ENDIF

SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function: lfwActBrw1
*!
*!**************************************************************************
FUNCTION lfwActBrw1

*!**************************************************************************
*!
*!      Function: lfwActBrw2
*!
*!**************************************************************************
FUNCTION lfwActBrw2


*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*    VALID function for get fields "lcAcctCode" , and loFormSet.laData[6]
*
FUNCTION lfvAccCode
PARAMETERS loFormSet,loFld,lnAcctPos

*PARAMETERS lcAcct,lnAcctPos

LOCAL lcOldAct
*N000682,1 03/16/2013 TMI Globlization changes[Start] 
loFld.KeyTextbox.OldValue = iif(VAL(STRTRAN(loFld.KeyTextbox.OldValue,'-','')) <> 0,loFld.KeyTextbox.OldValue ,REPLICATE ("0",loFormSet.lnAcsSegSz))
loFld.KeyTextbox.Value = IIF(loFld.Selectedfrombrowse , '?'+SUBSTR(loFld.KeyTextbox.Value,2),loFld.KeyTextbox.Value )
*N000682,1 03/16/2013 TMI Globlization changes[End  ] 
lcOldAct = loFld.KeyTextbox.OldValue
lcOldAct = IIF(EMPTY(lcOldAct),REPLICATE ("0",loFormSet.lnAcsSegSz),lcOldAct)
lcAcct = loFld.KeyTextbox.Value


PRIVATE lcCodeT,lcAccnlDes,lcCurrObj

lc_TmpAlc = loFormSet.lc_TmpAlc
lcCurrObj = SYS(18)

IF LEFT(LTRIM(lcAcct),1)<>'?' AND !ISDIGIT(LTRIM(lcAcct))
  =gfModalGen("TRM02061B00000","Dialog")
  loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
  RETURN .F.
ENDIF

IF loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
  *** No need for validation
  RETURN
ELSE
  *** This condition is true only if the account code had an old entry
  *** and now it is emptied,just ignore the entry.
  IF LEFT(LTRIM(lcAcct),1)<>'?'.AND.VAL(STRTRAN(lcAcct,'-','')) = 0
    loFld.KeyTextbox.Value = loFld.KeyTextbox.OldValue
    RETURN
  ELSE
    lcCodeT    = ''
    lcAccnlDes = ''
    *** If there are no record in the temporary file (list),
    *** the account type of the upper account code may be changed.
    *** This condition applies for the following cases :
    *** a. A new transaction ( Add mode )
    *** b. Removing all records in the list.
    IF loFormSet.lnTmpRcCnt = 0
      loFormSet.lcCodeType  = "A"
    ENDIF

    IF lfVldAccnt(loFormSet,loFld,loFormSet.lcCodeType,"C","L",.T.,@lcAccnlDes,@lcCodeT,"") AND !EMPTY(CHRTRAN(loFld.Keytextbox.Value,'0-',''))

      IF loFormSet.lcCodeType = "A"
        loFormSet.lcCodeType  = IIF(LEFT(LTRIM(lcCodeT),1)="Y","S","T")
      ENDIF
      IF lnAcctPos  = 2

        SELECT (lc_TmpAlc)

        *** If previously modified,"M---->M"
        *** If a new entry,        "A---->A"
        *** else                   "S---->M"
        lcStatus = SUBSTR("MAM",AT(cStatus,"MAS"),1)

        REPLACE cAcctcode WITH loFormSet.lcAcctCode ,;
                cStatus   WITH lcStatus

        *** Refresh objects
        loFormSet.lcAccDesc2 = lcAccnlDes

        SELECT GLAUTHD

        loFormSet.lcObjState = IIF(VAL(STRTRAN(loFormSet.laData[6],'-','')) > 0 .AND.;
                     ((loFormSet.laData[5]='A' .AND.( loFormSet.lnDebit>0 .OR. loFormSet.lnCredit>0));
                     .OR. (loFormSet.laData[5]='P' .AND. loFormSet.lnAllocPer>0.AND. loFormSet.lnTotCr<100)),;
                     "ENABLE","DISABLE")
        loFormSet.llNewDet   = IIF(VAL(STRTRAN(loFormSet.laData[6],'-','')) > 0 .AND.;
                     ((loFormSet.laData[5]='A' .AND.( loFormSet.lnDebit>0 .OR. loFormSet.lnCredit>0));
                     .OR. (loFormSet.laData[5]='P' .AND. loFormSet.lnAllocPer>0.AND. loFormSet.lnTotCr<100)),;
                     .F.,.T.)

        WITH loFormSet.Ariaform1
        .pbNew.Enabled = (loFormSet.lcObjState = 'ENABLE')
        IF loFormSet.laData[5] = 'A'
          .lnDebit.Enabled = .T.
          .lnCredit.Enabled = .T.
        ELSE
          .lnAllocPer.Enabled = .T.
        ENDIF
        ENDWITH
      ELSE
        *** Modifying upper account code :
        IF loFormSet.lnTmpRcCnt = 0
           loFormSet.llNewDet = .F.
           loFormSet.Ariaform1.pbNew.Enabled = .T.
        ENDIF
        loFormSet.lcAccDesc1 = lcAccnlDes
      ENDIF
      *B610269,1 media fixing problems TMI 03/16/2013 [Start] 
      loFormset.Ariaform1.lcAccDesc1.Value = lcAccnlDes
      *B610269,1 media fixing problems TMI 03/16/2013 [End  ] 
      
    ELSE

      loFld.KeyTextbox.Value = lcOldAct
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
lfFormAfterRowColumnChange(loformset)
RETURN
*!**************************************************************************
*!
*!      Function:  lfvDebit
*!
*!
*!**************************************************************************
*    VALID function for get field "lnDebit".
*    lnTotDr field is used for totals
FUNCTION lfvDebit
PARAMETERS loFormSet,loFld
lc_TmpAlc = loFormSet.lc_TmpAlc
*IF loFormSet.lnDebit <> lnOldDr

WITH loFormSet.Ariaform1
  .lnDebit.refresh()
  .lnCredit.Refresh()
  .lnAllocPer.refresh()
ENDWITH

IF loFormSet.lnDebit <> loFld.OldValue
  *** Reject negative or zero entries
  IF loFormSet.lnDebit < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    loFormSet.lnDebit = loFld.OldValue
    RETURN .F.
  ELSE

    SELECT (lc_TmpAlc)

    IF loFormSet.lnDebit > 0 .AND. cDrOrCr = "C"

      *** If there is a Credit entry for the same record,ignore it,
      *** blank it and adjust Credit totals.
      loFormSet.lnCredit = 0
      loFormSet.lnTotCr  = loFormSet.lnTotCr - nAmount
      loFormSet.lnTotDr  = loFormSet.lnTotDr + loFormSet.lnDebit

    ELSE
      loFormSet.lnTotDr  = loFormSet.lnTotDr - nAmount + loFormSet.lnDebit
    ENDIF
    lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH loFormSet.lnDebit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "D"

    SELECT GLAUTHD

  ENDIF
ENDIF

loFormSet.lcObjState = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 ;
                .AND.(loFormSet.lnDebit>0 .OR. loFormSet.lnCredit>0),"ENABLE","DISABLE")
loFormSet.llNewDet   = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 ;
                .AND.(loFormSet.lnDebit>0 .OR. loFormSet.lnCredit>0),.F.,.T.)

loFormSet.Ariaform1.pbNew.Enabled = loFormSet.lcObjState = 'ENABLE'
lfFormAfterRowColumnChange(loformset)

*!**************************************************************************
*!
*!      Function:  lfvCredit
*!
*!**************************************************************************
*    VALID function for get field "lnCredit".
*    lnTotCr field is used for totals
FUNCTION lfvCredit
PARAMETERS loFormSet,loFld
lc_TmpAlc = loFormSet.lc_TmpAlc

WITH loFormSet.Ariaform1
  .lnDebit.refresh()
  .lnCredit.Refresh()
  .lnAllocPer.refresh()
ENDWITH
IF loFld.Value <> loFld.OldValue

  *** Reject negative or zero entries
  IF loFormset.lnCredit < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    loFormset.lnCredit = loFld.OldValue
    RETURN .F.

  ELSE

    SELECT (lc_TmpAlc)

    DO CASE
      CASE loFormset.lnCredit > 0 .AND. cDrOrCr = "D"
        *** If there is a Debit entry for the same record,ignore it,
        *** blank it and adjust Debit totals.
        loFormSet.lnDebit = 0
        loFormSet.lnTotDr = loFormSet.lnTotDr - nAmount
        loFormSet.lnTotCr = loFormSet.lnTotCr + loFormset.lnCredit

      OTHERWISE
        loFormSet.lnTotCr = loFormSet.lnTotCr - nAmount + loFormset.lnCredit
    ENDCASE
    lcStatus = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH loFormset.lnCredit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "C"

    SELECT GLAUTHD
  ENDIF
ENDIF

loFormSet.lcObjState = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 .AND.;
                (loFormSet.lnDebit > 0 .OR. loFormSet.lnCredit > 0),"ENABLE","DISABLE")

loFormSet.llNewDet = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 .AND.;
              (loFormSet.lnDebit > 0 .OR. loFormSet.lnCredit > 0),.F.,.T.)

loFormSet.Ariaform1.pbNew.Enabled = loFormSet.lcObjState = 'ENABLE'
lfFormAfterRowColumnChange(loformset)

*!**************************************************************************
*!
*!      Function:  lfvAllocPr
*!
*!**************************************************************************
*    VALID function for get field "lnAllocPer"
*    Note that lnTotCr field is used for Allocation Percentage totals
FUNCTION lfvAllocPr
PARAMETERS loFormSet,loFld
PRIVATE lcModalWin

lc_TmpAlc = loFormSet.lc_TmpAlc

WITH loFormSet.Ariaform1
  .lnDebit.refresh()
  .lnCredit.Refresh()
  .lnAllocPer.refresh()
ENDWITH

IF loFormSet.lnAllocPer <> loFld.OldValue
  *** Reject negative or zero entries
  IF loFormSet.lnAllocPer < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    loFormSet.lnAllocPer = loFld.OldValue
    RETURN .F.
  ELSE
    SELECT (lc_TmpAlc)

    *** If the user inputs an entry in this field,
    IF (loFormSet.lnTotCr - nAmount + loFormSet.lnAllocPer) > 100
      *** If this entry caused the total of the allocation percentage
      *** to exceed 100 %, do not accept the entry,and present a message
      =gfModalGen("TRM02017B00000","DIALOG",loFormSet.lcTAllcPrc)

      *** Return to field
      loFormSet.lnAllocPer = loFld.OldValue
      RETURN .F.
    ELSE
      loFormSet.lnTotCr    = loFormSet.lnTotCr-nAmount+loFormSet.lnAllocPer
      lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
      REPLACE nAmount  WITH loFormSet.lnAllocPer ;
              cStatus  WITH lcStatus

      SELECT GLAUTHD
    ENDIF
  ENDIF
ENDIF

loFormSet.lcObjState = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 .AND.;
                 loFormSet.lnAllocPer > 0 .AND. loFormSet.lnTotCr < 100 , "ENABLE" , "DISABLE")

loFormSet.llNewDet   = IIF(VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) > 0 .AND.;
                 loFormSet.lnAllocPer > 0 .AND. loFormSet.lnTotCr < 100 , .F. , .T.)

loFormSet.Ariaform1.pbNew.Enabled = loFormSet.lcObjState = 'ENABLE'
lfFormAfterRowColumnChange(loformset)

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*    VALID function for push button "New" (pbNew).
FUNCTION lfvNew
PARAMETERS loFormSet

*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled by the user ,
*** initializing it with type ("D"),distribution code and status="A"
*** ( for addition )

gcWorkDir = oAriaApplication.WorkDir
lc_TmpAlc = loFormSet.lc_TmpAlc

SELECT (lc_TmpAlc)

LOCATE FOR EMPTY(cAutType)
IF FOUND()
  REPLACE cAutType WITH loFormSet.laData[1],;
          cAutCode WITH loFormSet.laData[2],;
          cStatus  WITH 'A'
ELSE
  INSERT INTO &gcWorkDir.&lc_TmpAlc.;
              (cAutType,cAutCode,cStatus);
         VALUES (loFormSet.laData[1],loFormSet.laData[2],"A")
ENDIF

*** Add Audit Information to the newly created record
=gfAdd_Info(lc_TmpAlc)

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving
loFormSet.lcAcctCode = REPLICATE ("0",loFormSet.lnAcsSegSz)
loFormSet.lcAccDesc2 = SPACE(60)
IF loFormSet.laData[5] = "A"
  loFormSet.lnDebit  = 0.00
  loFormSet.lnCredit = 0.00
ELSE
  loFormSet.lnAllocPer = 0
ENDIF

*** Increase number of records in temporary file
loFormSet.lnTmpRcCnt = loFormSet.lnTmpRcCnt + 1

*** Select the new record from the list
*sAlcDet   = loFormSet.lnTmpRcCnt

WITH loFormSet.Ariaform1
   .lcAcctCode.Enabled = .T.
  *** Disable numeric fields until an account is entered.
  IF loFormSet.laData[5] = "A"
    .lnDebit.Enabled = .F.
    .lnCredit.Enabled = .F.
  ELSE
    .lnAllocPer.Enabled = .F.
  ENDIF

  IF loFormSet.lnTmpRcCnt > 0
    .pbRem.Enabled = .T.
    loFormSet.Ariaform1.puType.Enabled = .F.
  ENDIF

  *** Disable New button until a valid account is ebtered
  .pbNew.Enabled = .F.

ENDWITH

loFormSet.llNewDet = .T.

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (lcAcctCode object)
lfFormAfterRowColumnChange(loformset)
WITH loFormSet.Ariaform1.lcAcctCode
  .Setfocus()
  .Keytextbox.Value = REPLICATE ("0",loFormSet.lnAcsSegSz)
ENDWITH
*** Always return to the original work aria
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*    VALID function for push button "Remove" (pbRem).
*
FUNCTION lfvRem
PARAMETERS loFormSet

*** Confirm Removing of the record
lc_TmpAlc = loFormSet.lc_TmpAlc
IF gfModalGen("QRM00007B00007","ALERT") = 1
  SELECT (lc_TmpAlc)

  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D"
  ***   delete it
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Adjust totals
  IF cDrOrCr = "D"
    loFormSet.lnTotDr  = loFormSet.lnTotDr - nAmount
  ELSE
    loFormSet.lnTotCr  = loFormSet.lnTotCr - nAmount
  ENDIF

  *** Decrement number of records in list
  loFormSet.lnTmpRcCnt = loFormSet.lnTmpRcCnt - 1

  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top
  DELETE
  SKIP
  IF EOF(lc_TmpAlc)
    GO TOP
  ENDIF

  *** Refresh objects with contents of the current record,or spaces
  *** if the list is empty
  loFormSet.lcAcctCode = IIF(loFormSet.lnTmpRcCnt=0,REPLICATE ("0",loFormSet.lnAcsSegSz),cAcctCode)
  loFormSet.lcAccDesc2 = IIF(loFormSet.lnTmpRcCnt=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                  &lc_TmpAlc..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))

  IF loFormSet.laData[5] = "A"
    loFormSet.lnDebit  = IIF(loFormSet.lnTmpRcCnt = 0 .OR. cDrOrCr = "C", 0 , nAmount)
    loFormSet.lnCredit = IIF(loFormSet.lnTmpRcCnt = 0 .OR. cDrOrCr = "D", 0 , nAmount)
  ELSE
    loFormSet.lnAllocPer = IIF(loFormSet.lnTmpRcCnt = 0 , 0 , nAmount)
  ENDIF

  *** Adjust controls
  loFormSet.lcObjState = IIF(loFormSet.lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")

  WITH loFormSet.Ariaform1
    .lcAcctCode.Enabled = loFormSet.lcObjState = 'ENABLE'
    .pbRem.Enabled = loFormSet.lcObjState = 'ENABLE'
    .pbNew.Enabled = .T.

    loFormSet.llNewDet = .F.

    IF loFormSet.laData[5] = "A"
      .lnDebit.Enabled = loFormSet.lcObjState = 'ENABLE'
      .lnCredit.Enabled = loFormSet.lcObjState = 'ENABLE'
    ELSE
      .lnAllocPer.Enabled = loFormSet.lcObjState = 'ENABLE'
    ENDIF

    IF loFormSet.lnTmpRcCnt = 0
      loFormSet.Ariaform1.puType.Enabled = .T.
    ENDIF
  ENDWITH

  WITH loFormSet.Ariaform1
  IF .lcAcctCode.Enabled
    .lcAcctCode.Setfocus()
  ELSE
    .pbNew.Setfocus()
  ENDIF
  ENDWITH
  lfFormAfterRowColumnChange(loformset)
  SELECT GLAUTHD

ENDIF


*!**************************************************************************
*!
*!      PROCEDURE lpSavScr
*!
*!**************************************************************************
* Save the informaation
PROCEDURE lfFormBeforeSave
PARAMETERS loFormSet
LOCAL llCSave
llCSave = .T.

WITH loFormSet.Ariaform1
DO CASE
  *** If the file is empty,i.e. there are no allocation transactions,
  *** "Cannot save an empty transaction"
  CASE (loFormSet.lnTmpRcCnt = 0)
    =gfModalGen("TRM02035B00000","Dialog",loFormSet.lcTAllcTrs)
    llCSave = .F.
    .pbNew.Setfocus()

  CASE VAL(STRTRAN(loFormSet.laData[6],'-','')) = 0
    =gfModalGen("TRM02040B00000","DIALOG")
    llCSave = .F.
    .laData6.Setfocus()

  CASE VAL(STRTRAN(loFormSet.lcAcctCode,'-','')) = 0
    =gfModalGen("TRM02022B00000","DIALOG")
    llCSave = .F.
    .lcAcctCode.Setfocus()

  CASE loFormSet.laData[5] = 'A' .AND. loFormSet.lnDebit = 0 .AND. loFormSet.lnCredit = 0
    =gfModalGen("TRM02033B00000","DIALOG",loFormSet.lcTDrORCR)
    llCSave = .F.
    .lnDebit.Setfocus()

  *** Present a message with percent=0 not allowed
  CASE loFormSet.laData[5] = 'P' .AND. loFormSet.lnAllocPer = 0
    =gfModalGen("TRM02033B00000","DIALOG",loFormSet.lcTAllPrcs)
    llCSave = .F.
    .lnAllocPer.Setfocus()

  CASE loFormSet.lnTotDr = loFormSet.lnTotCr
    *** Total debits and credits must not balance
    =gfModalGen("TRM02032B00000","DIALOG")
    llCSave = .F.

ENDCASE
ENDWITH
RETURN llCSave

*- End of lfFormBeforeSave.

*!**************************************************************************
*!
*!      FUNCTION: lpSavScr
*!
*!*******************************************************************************
*    This FUNCTION handles saving,instead of the global FUNCTION.
*    "Save" option corresponds to the ninth position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global saving FUNCTION.
*      The flag llCSave is a global flag that is to have the value
*    of "FALSE" (.F.) if the record(s) is/are not to be saved.
*
FUNCTION lpSavScr
PARAMETERS loFormSet

SELECT GLAUTHD

*** If adding a new record,append a blank one
IF loFormSet.ActiveMode = 'A'
  APPEND BLANK
  =gfAdd_Info('GLAUTHD')
  RLOCK()
ENDIF

*** Store laData values in the current record
lcScFields = loFormSet.lcScFields
GATHER FROM loFormSet.laData fields &lcScFields

*** Now save the data in the temporary file using the following global
*** function which performs correct translation from the temporary
*** file lc_TmpAlc,and the main file GLAUTDT
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	=gfTmp2Mast("GLAUTDT",loFormSet.lc_TmpAlc,;
*!*	            'Saving allocation entry '+loFormSet.laData[2]+' ...')
=gfTmp2Mast("GLAUTDT",loFormSet.lc_TmpAlc,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_SAVING,loFormSet.GetHeaderText("LANG_S_SAVING",loFormSet.HeaderAlias)) +loFormSet.laData[2]+' ...')
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
SELECT GLAUTHD
gfTableUpdate()
SELECT GLAUTDT
gfTableUpdate()

SELECT GLAUTHD
IF loFormSet.ActiveMode = 'A'
  UNLOCK
ENDIF
*- End of lpsavscr

*!**************************************************************************
*!
*!      FUNCTION: lpDelScr
*!
*!*******************************************************************************
*    This FUNCTION handles deletion,instead of the global FUNCTION.
*    "Delete" option corresponds to the seventh position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global delete FUNCTION.
*
FUNCTION lpDelScr
PARAMETERS loFormSet
PRIVATE lcAutType, lcAutCode

lc_TmpAlc = loFormSet.lc_TmpAlc
lcAutDtEx = loFormSet.lcAutDtEx

*** Check if this record is already deleted by another user from
*** a different station. If it is, the record pointer is no longer
*** on the viewed record which is now actually out of scope if
*** SET('DELETED')='ON'

IF GLAUTHD.cAutType + GLAUTHD.cAutCode <> loFormSet.laData[1]+loFormSet.laData[2]
  *** If the record is already deleted, present the following message,
  *** and go to 'Select' mode
  *** Message : "

  =gfModalGen("TRM00095B00000","ALERT")

  *** Go back to 'Select' mode
  loFormSet.ChangeMode('S')

  RETURN
ENDIF

lnTotRec   = RECCOUNT(lc_TmpAlc)+1

*** Delete records belonging to the current header from the master
*** file (GLAUTDT)
*** The temporary file lc_TmpAlc is zapped in 'Select' mode
SELECT GLAUTDT
SCATTER MEMVAR MEMO BLANK
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	REPLACE cAutType  WITH m.cAutType,;
*!*	        cAutCode  WITH m.cAutCode,;
*!*	        cAcctCode WITH m.cAcctCode ;
*!*	    FOR &lcAutDtEx. = loFormSet.laData[1]+loFormSet.laData[2] .AND. ;
*!*	        lfThermo('Deleting allocation entry '+loFormSet.laData[2]+' ...')
*B610269,1 media fixing problems TMI 03/16/2013 [Start] should be in one statement
*REPLACE cAutType  WITH m.cAutType,;
*        cAutCode  WITH m.cAutCode,;
*        cAcctCode WITH m.cAcctCode ;
*    FOR &lcAutDtEx. = loFormSet.laData[1]+loFormSet.laData[2] .AND. ;
*        *N000682,1 11/20/2012 MMT Globlization changes[Start]
**lfThermo(LANG_S_DELETING+loFormSet.laData[2]+' ...')
*lfThermo(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_DELETING,loFormSet.GetHeaderText("LANG_S_DELETING",loFormSet.HeaderAlias))+loFormSet.laData[2]+' ...')
REPLACE cAutType  WITH m.cAutType,;
        cAutCode  WITH m.cAutCode,;
        cAcctCode WITH m.cAcctCode ;
    FOR &lcAutDtEx. = loFormSet.laData[1]+loFormSet.laData[2] .AND. ;
        lfThermo(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_S_DELETING,loFormSet.GetHeaderText("LANG_S_DELETING",loFormSet.HeaderAlias))+loFormSet.laData[2]+' ...')
*B610269,1 media fixing problems TMI 03/16/2013 [End  ] 
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[End]
DELETE FOR &lcAutDtEx. = m.cAutType + m.cAutCode
=gfTableUpdate()

SELECT GLAUTHD

*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE
=gfTableUpdate()

*!*	IF lnThermRec < lnTotRec
*!*	  =gfThermo(lnTotRec,lnTotRec,;
*!*	        'Deleting allocation entry '+loFormSet.laData[2]+' ...','')
*!*	ENDIF

*** Return to "SELECT" mode
*laScrMode    = .F.
*laScrMode[1] = .T.
loFormSet.ChangeMode('S')
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function : lfThermo
*!
*!*******************************************************************************
*  This function calls global function gfThermo (thermometer)
*  It takes as a parameter a counter to be incremented and tested
*  at every call. The thermometer is called at increments of 13
*  instead of 1 for faster processing.
*
FUNCTION lfThermo
PARAMETERS lcThermStr

WAIT WINDOW NOWAIT lcThermStr

*!**************************************************************************
*!
*!      Function: lfSrcJrnl
*!
*!**************************************************************************
*  Creates an array of Source journals for the Source Journals popup
*
FUNCTION lfSrcJrnl
PARAMETERS loFormSet
gcDataDir = oAriaApplication.DataDir
gcWorkDir = oAriaApplication.WorkDir

*** Create an array holding avialable Source Journals from
*** GLSORJOR.DBF to be displayed in popup (puSrcJrnl/ibSrcJrnl)
SELECT ALLTRIM(CSRCJRNL)+'-'+CJORLNDES,CSRCJRNL;
       FROM &gcDataDir.GLSUBJOR;
       INTO ARRAY loFormSet.laTSrcJrnl


*** Get the row number of the array element corresponding to
*** the default source journal if the array is not empty.
loFormSet.lnDSrcJrn =  ASCAN(loFormSet.laTSrcJrnl,loFormSet.lcSJ_Def)

*** If the array is empty (_TALLY=0), add the default Source Journal
*** value (global variable loFormSet.lcSJ_Def) to the array
*** Else, look for the default Source journal value in the array, if
*** found, use it as a default in 'Select' mode and 'Add' mode, else
*** add the default source journal values (global variable loFormSet.lcSJ_Def)
*** to the array.
IF loFormSet.lnDSrcJrn = 0
  loFormSet.lnDSrcJrn = ALEN(loFormSet.laTSrcJrnl,1)

  DIMENSION loFormSet.laTSrcJrnl[loFormSet.lnDSrcJrn,2]
  loFormSet.laTSrcJrnl[loFormSet.lnDSrcJrn,1] = loFormSet.lcSJ_Def+'-'+''

  loFormSet.laTSrcJrnl[loFormSet.lnDSrcJrn,2] = loFormSet.lcSJ_Def

ELSE
  loFormSet.lnDSrcJrn = ASUBSCRIPT(loFormSet.laTSrcJrnl,loFormSet.lnDSrcJrn,1)
ENDIF

