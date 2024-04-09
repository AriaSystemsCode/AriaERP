*:***************************************************************************
*: Program file  : ARSJOUR
*: Program desc. : Custom Sales Journal Report for FLO09
*: Module        : Accounts receivable (AR)
*: Developer     : Mariam Mazhar(MMT)
*: Tracking Job Number: C201635 {T20140814.0021}
*: Date          : 08/25/2014
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfCodDes,gfGetMemVar,gfOptMsg,gfBrows,gfCrtTmp.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : 1- All IF llFrTime Blocks executed one time in the option grid seasson.
*:         :    and if user selection opens any temp. files that may be used later
*:         :    in another selection I take this file open to use untill user choice
*:         :    is to press < Close > button, to save more time.
*:         : 2- Any variable start by (llCh) means that some thing in
*:         :    selected critria was changed, you must collect data again.
*:***************************************************************************
*: Example : DO ARSJOUR
*:***************************************************************************
*:Modifications : ....
*:**************************************************************************************************
*----------------------- Report Code Begin -----------------------------
#INCLUDE R:\Aria4xp\reports\ar\arsjour.H

lcStTime   = TIME()    && Time in which we start collect data.
llNoIndex = .F.        && I don't make index for file.
*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON

*-- Use variable llOGFltCh that detect OG filter changes.[Begin]
IF llClearFn OR loOGScroll.llOGFltCh
  STORE .F. TO llChAcc,llChTrnTyp,llClearFn
  lcLastExpr = lcRpExp
ENDIF
*-- Use variable llOGFltCh that detect OG filter changes.[End]
*-- Show messages in status bar when collecting data. [begin]

*Include void invoices amount and ship amount if upper invoice date is less than void invoice date. [Begin]

lnInvPos = lfItmPos('INVHDR.INVDATE')
STORE {  /  /  } TO ldStrtDate , ldEndDate
*Fill the date variables.[START]

IF lcRpDateTP = "P"
  STORE {  /  /  } TO ldStrtDPst , ldEndDPst
  lnPostDate = lfItmPos('DPOSTDATE')
  *Variable Hold true If you print Void invoices only. [Begin]
  IF EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
  *Variable Hold true If you print Void invoices only.[End]
    ldStrtDPst = CTOD(PADR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])-1))
    ldEndDPst  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])+1))
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst)) OR (!BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))))]
  ENDIF
ENDIF
*Fill the date variables.[END]
*Don't get to this code in case posted date.[START]
IF lcRpDateTP # "P"
  IF EMPTY(loOGScroll.laOGFxFlt[lnInvPos,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
    ldStrtDate = CTOD(PADR(loOGScroll.laOGFxFlt[lnInvPos,6],ATC('|',loOGScroll.laOGFxFlt[lnInvPos,6])-1))
    ldEndDate  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnInvPos,6],ATC('|',loOGScroll.laOGFxFlt[lnInvPos,6])+1))
    * Add Void between to Range.
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(INVDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(INVDATE,ldStrtDate,ldEndDate))))]
  ENDIF
ENDIF
*Don't get to this code in case posted date.[END]
*Include void invoices amount and ship amount if upper invoice date is less than [End]

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  *-- lcDetExp   : Detail band expression.
  *-- lcCurrExp  : Currency group footer expression.
  *-- lcGrpExp   : Variable group footer expression.
  *-- lcGrandExp : Summary band expression.

  * Display HST Tax amount for Canada. [Begin]
  lcDetExp = [IIF(llMultCurr AND (lcRpCurr != 'F') AND (lcRpSortBy !='U'),']+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))+['+']+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))+;
              ['+' : ' + cCurrCode + " , ",'')] +;
              [+ IIF(llRpRepPrn,']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
              ['+' ' + Rep1 +  " " +']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+['+" =  "  +;
              TRANSFORM(lnRepComm,"9999999.99") ," ")] +;
              [+IIF(llCanada, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstAmount,oAriaApplication.GetHeaderText("LANG_Arsjour_PstAmount",AHEADERFILE))+[' +]+;
              [TRANSFORM(lnPstAmt,"99999999.99")+;
              SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstAmount,oAriaApplication.GetHeaderText("LANG_Arsjour_HstAmount",AHEADERFILE))+[' + ;
              TRANSFORM(lnHstAmt,"99999999.99")," ")]
  lcCurrExp  = [IIF(llRpRepPrn, SPACE(10) +']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Total,oAriaApplication.GetHeaderText("LANG_Arsjour_Total",AHEADERFILE))+;
               [' +TRANSFORM(lnCommCur,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_PstTotal",AHEADERFILE))+;
               [' +TRANSFORM(lnPstAmtCr,"99999999.99")+SPACE(10) +']+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_HstTotal",AHEADERFILE))+[' + ;
               TRANSFORM(lnHstAmtCr,"99999999.99")," ")]
  lcGrpExp = [IIF(llRpRepPrn, SPACE(10) +']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
             ['+' '+']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
             ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Total,oAriaApplication.GetHeaderText("LANG_Arsjour_Total",AHEADERFILE))+;
             ['  +TRANSFORM(lnCommGrp,"9999999.99") ," ")] +;
             [+IIF(llCanada, SPACE(10) +']+ ;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_PstTotal",AHEADERFILE))+[' +;
             TRANSFORM(lnPstAmtGp,"99999999.99")+SPACE(10) + ']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_HstTotal",AHEADERFILE))+[' + ;
             TRANSFORM(lnHstAmtGp,"99999999.99")," ")]
  lcGrandExp = [IIF(llRpRepPrn, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
               ['+' '+']+iIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Grand,oAriaApplication.GetHeaderText("LANG_Arsjour_Grand",AHEADERFILE))+[' +;
               TRANSFORM(lnCommTot,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) +']+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstGrand,oAriaApplication.GetHeaderText("LANG_Arsjour_PstGrand",AHEADERFILE))+[' +;
               TRANSFORM(lnPstAmtot,"99999999.99")+;
               SPACE(10) +']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstGrand,oAriaApplication.GetHeaderText("LANG_Arsjour_HstGrand",AHEADERFILE))+[' + ;
                               TRANSFORM(lnHstAmtot,"99999999.99")," ")]
  * Display HST Tax amount for Canada. [End]

  *-- Create temporary file that holding order InvHdr data. [begin]

  lcWorkFile = gfTempName()
  DIMENSION laTempStru[1,18]
  laTempStru = ''

  SELECT INVHDR
  =AFIELD(laTempStru)
  
  lnTempStru=ALEN(laTempStru,1) + 3
  DIMENSION laTempStru[lnTempStru,18]
  
  laTempStru[lnTempStru-1,1] = 'cstname'
  laTempStru[lnTempStru-1,2] = 'C'
  laTempStru[lnTempStru-1,3] = 30
  laTempStru[lnTempStru-1,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-1,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-1,17], laTempStru[lnTempStru-1,18]

  laTempStru[lnTempStru-2,1] = 'Priority'
  laTempStru[lnTempStru-2,2] = 'C'
  laTempStru[lnTempStru-2,3] = 3
  laTempStru[lnTempStru-2,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-2,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-2,17], laTempStru[lnTempStru-2,18]
  
  *-- cTempKey : field used in most sort by case as the master key.
  *--          : note that field width is dependent on number of sort
  *--          : case he make.
  laTempStru[lnTempStru,1] = 'cTempKey'
  laTempStru[lnTempStru,2] = 'C'
  laTempStru[lnTempStru,3] = 62
  laTempStru[lnTempStru,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru,17], laTempStru[lnTempStru,18]

  *-- Create temporary file that holding order InvHdr data. [End]

  llFrTime = .F.  && After this time all of your variables have been defined,  you do not need to go to any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.

*-- Create temporary cursors from structure array. [begin]
IF EMPTY(lcWorkFile) OR !USED(lcWorkFile)
  *-- System Setting for report [begin]
  lcSetHour = SET('HOURS')
  SET HOURS TO 24
  *-- System Setting for report [end]
  =lfCreatCur(lcWorkFile)  && Create work cursor.
ENDIF


*-- If user change report critria, Collect report data.
*-- lcLastExpr : Last <Run> OR <Preview> lcRpExp.
*-- llChFactor : .T. if user change Factored/Non Factored/Both selection, which is hidden filter.
*-- llChInv    : .T. if user change Invoces/Void Invoices/Both selection, which is hidden filter.
*Use variable llOGFltCh that detect OG filter changes.[Begin]
IF llClearFn OR loOGScroll.llOGFltCh
*Use variable llOGFltCh that detect OG filter changes.[End]
  llClearFn = .F.
  *-- If the file already have data, clear it.
  IF RECCOUNT(lcWorkFile) > 0
    USE IN (lcWorkFile)
    =lfCreatCur(lcWorkFile)  && Create work cursor again.
    llNoIndex = .T.
  ENDIF  && end If the file already have data, clear it.
  *-- If User Change Index tag due to change sort by.
  IF llNoIndex OR (lcLastTag != lcIndexTg)
    = lfUserChTg()
  ENDIF    && end if User Change Index tag.

  *-- Note that : Optimized expression shape is [ACCOUNT+INVOICE = lcAccVal]
  *-- lcAccOp   : Direct Operator of optimized expression.
  *-- lcAccVal  : Value of optimized expression.
  *-- lcOperator: True Operator of optimized expression.
  STORE '' TO lcAccOp,lcAccVal,lcOperator
  STORE .T. TO llOpLogic  && Logic of operator (if it's .F. its logic is NOT)

  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.

  =lfCollData()  && Scan around invHdr master file to collect specific critria records.

  *-- Calculate From and To dates.
  *-- If you print Void invoices only.
  IF llVoidOnly
    lcInvDateF = PADR(SUBSTR(loOGScroll.laOGFxFlt[2,6],1,ATC('|',loOGScroll.laOGFxFlt[2,6])-1),10)
    lcInvDateT = PADL(SUBSTR(loOGScroll.laOGFxFlt[2,6],ATC('|',loOGScroll.laOGFxFlt[2,6])+1),10)
  ELSE && else Print either (Invoices) or (invoices and void ).
    lcInvDateF = PADR(SUBSTR(loOGScroll.laOGFxFlt[1,6],1,ATC('|',loOGScroll.laOGFxFlt[1,6])-1),10)
    lcInvDateT = PADL(SUBSTR(loOGScroll.laOGFxFlt[1,6],ATC('|',loOGScroll.laOGFxFlt[1,6])+1),10)
  ENDIF

  STORE .F. TO llChFactor,llChInv    && Unrise all hidden Critria variables.

ELSE  &&  user does not change report critria, print from the same data.

  *-- If User Change Index tag due to change sort by.
  IF lcLastTag != lcIndexTg
    =lfUserChTg()
  ENDIF    && end if User Change Index tag.

ENDIF       && end If user change report critria, Collect report data.

*-- Select Master report file.
SELECT (lcWorkFile)
*-- Relation Section [begin]
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
SET RELATION TO cWareCode INTO Warehous ADDITIVE
SET RELATION TO Rep1 INTO Salesrep ADDITIVE
*HMA(Case of Multi Currency)[Begin]
IF llMultCurr
  SET RELATION TO ccurrcode INTO Syccurr ADDITIVE
ENDIF
*HMA(Case of Multi Currency)[End]
*-- Relation Section [End]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SelectMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SelectMsg",AHEADERFILE)) +' '+ ;
            ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_RecInMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_RecInMsg",AHEADERFILE)) + ;
            ALLTRIM(STR(lnInterval,6,2)) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SecondMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SecondMsg",AHEADERFILE)) TIMEOUT 2

loogScroll.cCROrientation = 'P'

DO gfDispRe WITH EVAL('lcRpForm')

SET STATUS BAR &lcStatusBr    && Restore previous status bar status.


*----------------------- Report Code End -----------------------------


*-- Function Section
*-------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : 1- Get state title.
*!           : 2- Know in which country we are.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Country state title.
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfStitle

lcWorkArea = SELECT()
lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult >= 1
  IF !USED('SYCINT')
    =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')
  ENDIF
   lcSelectCommand1=[SELECT CCONT_CODE,CPART4LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=SYCCOMP.CCONT_CODE]
   lnResult1 = oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult1 >= 1
    llCanada  = 'CAN' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
    llEngland = 'ENG' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
  SELECT (lcWorkArea)
  RETURN (SYCINT.CPART4LAB)
  ENDIF
ENDIF
SELECT (lcWorkArea)
*-- End of lfStitle.

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Option Grid When function
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'

  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.

  PRIVATE lcThAlias
  lcThAlias = ALIAS()
  *-- Compute Start of variable filter to control its apperance [begin]
  *-- in option grid.
  lnVarbEnd = 0
  *-- Calculate length of variables appears in option grid.
  *-- and items that we enable and disable.
  PRIVATE lcSelectCommand2,lnResult2

  lcSelectCommand2=[SELECT * FROM SYREPUVR WHERE Crep_id="ARSJOUR" AND cExpType="V" AND nVarPos!=0 AND (EMPTY(CVER) OR CVER = "A40")]

  lnResult2  = oAriaApplication.remotesystemdata.execute(lcSelectCommand2,"","TMPREPUVR","",oAriaApplication.cAria4SysFiles,3,"",SET("DATASESSION"))

  IF lnResult2 >= 1
    lnVarbEnd = lnVarbEnd + 1
  ENDIF

  SELECT (lcThAlias)

  IF EMPTY(laRpFltVal)
    *-- laRpFltVal : Array to hold D for disable and E for enable, to control
    *--              Fixed filter appearance in option grid.
    *-- laRpVarNow : Array to hold .T. or .F., to control variables
    *--              appearance in option grid.
    DECLARE laRpFltVal[ALEN(loOGScroll.laOGFxFlt,1)],laRpVarNow[lnVarbEnd]
    STORE 'E' TO laRpFltVal
    STORE .T. TO laRpVarNow

    llTaxes =gfGetMemVar('M_TAX')='Y'  && .T., if company use taxes.

    IF llMultCurr
      IF !USED('SYCCURR')
        =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
      ENDIF
      SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    ENDIF
  ENDIF
ENDIF  && END IF you first time enter when function.
*Disable the Sort#2 Option When the sort#1 is by invoice [Begin]
lnSortPos=lfVarPos('lcRpSort2')
DO lpShowObj
*-- note that disable and enable is according to value of laRpFltVal.
*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lfVarPos
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : OG when function
*!***************************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(loOGScroll.laOGObjType,UPPER(lcItmInFlt))
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfVarPos.

*!*************************************************************
*! Name      : lfFltState
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
*! Parameters: 1- Filter Object Number
*!             2- 'E' for enable, 'D' for disable
*!*************************************************************
*! Returns   : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfFltState
PARAMETERS lnObjNum,lcObjState

IF lcObjState = 'D' AND !EMPTY(loOGScroll.laOGFxFlt[lnObjNum,6])
  loOGScroll.laOGFxFlt[lnObjNum,6] = ''
ENDIF
LAOGOBJCNT[LNOBJNUM + LNVARBEND] = (LCOBJSTATE = 'E')
= LFOGSHOWGET('loOGScroll.LAOGFXFLT[' + ALLTRIM(STR(LNOBJNUM)) + ',6]')  && ENABLE / DISABLE OBJECT .
*-- End of lfFltState.

*!*************************************************************
*! Name      : lfFormName
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Function to get the Form name
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : Form name
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfFormName

RETURN 'ARSJRFL'
*-- End of lfFormName.

*!*************************************************************
*! Name      : lfUserChTg
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Change Work file Index Tag.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfUserChTg
SELECT (lcWorkFile)
INDEX ON &lcIndexTg TAG (lcWorkFile)
IF llNoIndex
  llNoIndex = .F.
ELSE
  lcLastTag = lcIndexTg
ENDIF
*-- End of lfUserChTg.

*!*************************************************************
*! Name      : lfCollData
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Make optimized expression then Collecting data
*!             from InvHdr file into Work file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollData

PRIVATE lcOptimize
lcOptimize = ''

lnSelect=SELECT()
SELECT INVHDR
lcNewExp=""
IF LEN(loOGScroll.lcRpExp) > 1
  lcExp = loOGScroll.lcRpExp
  lnOccur = OCCURS(' AND',lcExp)
  IF lnOccur > 0
    FOR lnCount = 1 TO lnOccur + 1
      lnStart = IIF(lnCount = 1 , 1 , ATC(' AND',lcExp,lnCount-1) + 5)
      lnEnd = IIF(lnCount = lnOccur + 1,LEN(lcExp),ATC(' AND',lcExp,lnCount))
      lnLength = lnEnd - lnStart +IIF(lnCount = lnOccur + 1,1,0)
      lcTake = SUBSTR(lcExp,lnStart,lnLength)
      lnoccurs=ATC('INLIST',lcTake)
      lnSeaOcurr=ATC('SEASON',lcTake)
      lnDivOcurr=ATC('CDIVISION',lcTake)
      lnTrmOcurr=ATC('CTERMCODE',lcTake)
      lnLocOcurr=ATC('CWARECODE',lcTake)

      *WSH [Start]
      lnAccOcurr=ATC('ACCOUNT',lcTake)
      lnCurOcurr=ATC('CCURRCODE',lcTake)
      lnAdrOcurr=ATC('CADDRESS4',lcTake)
      lnRepOcurr=ATC('REP1',lcTake)
      *WSH [End]

      IF lnoccurs > 0

        IF (lnSeaOcurr > 0 OR lnDivOcurr > 0 OR lnTrmOcurr > 0 OR lnLocOcurr > 0 OR;
            lnAccOcurr > 0 OR lnCurOcurr > 0 OR lnAdrOcurr > 0 OR lnRepOcurr > 0)
          lcTake = ""
        ELSE
          lcTake = SUBSTR(lcExp,lnStart,lnLength)
        ENDIF
      ENDIF
      IF !EMPTY(lcNewExp)
        IF !EMPTY(lcTake)
          lcNewExp = lcNewExp + ' .AND. '+ lcTake
        ENDIF
      ELSE
        lcNewExp = lcTake
      ENDIF
    ENDFOR
  ENDIF
ENDIF
lcRpExp = IIF(EMPTY(lcNewExp),loOGScroll.lcRpExp,lcNewExp)
SELECT(lnSelect)
*--Create temp files for Division,Season,Location,payment terms [BEGIN]
*--Make Temp File For Selected invoice Divisions

lcDivCursor = lfOGTempCurs("INVHDR.CDIVISION", "CDIVISION", "CDIVISION", "loOGScroll.laOgVrFlt", .T.)
lcSeaCursor = lfOGTempCurs("INVHDR.SEASON", "SEASON", "SEASON", "loOGScroll.laOgVrFlt", .T.)
lcLocCursor = lfOGTempCurs("INVHDR.CWARECODE", "CWARECODE", "CWARECODE", "loOGScroll.laOgVrFlt", .T.)
lcTrmCursor = lfOGTempCurs("INVHDR.CTERMCODE", "CTERMCODE", "CTERMCODE", "loOGScroll.laOgVrFlt", .T.)
lcStatCursor = lfOGTempCurs("CUSTOMER.CADDRESS4", "Address", "Address", "loOGScroll.laOgVrFlt", .T.)
lcAcctCursor = lfOGTempCurs("INVHDR.ACCOUNT", "ACCOUNT", "ACCOUNT", "loOGScroll.laOgVrFlt", .F.)
lcCurrCursor = lfOGTempCurs("INVHDR.CCURRCODE", "CCURRCODE", "CCURRCODE", "loOGScroll.laOgFxFlt", .F.)
lcRep1Cursor = lfOGTempCurs("INVHDR.REP1", "Rep1", "Rep1", "loOGScroll.laOgVrFlt", .F.)
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")
*Check for the posted filter also
IF !EMPTY(loOGScroll.laOGFxFlt[lnInvPos,6]) OR IIF(lcRpDateTP = "P" , !EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6]) , .F.)

  *-- If Void Only.
  IF lcRpList = "V"
    lcRpExp = STRTRAN(lcRpExp,"INVDATE","VDATE")
  ENDIF

  *Check for the posted filter also.[START]
  IF lcRpList = "V" AND lcRpDateTP = "P"
    lcRpExp = STRTRAN(lcRpExp,"DPOSTDATE","VDATE")
  ENDIF
  *Check for the posted filter also.[END]

  *-- If Print Both Active and Void invoices.
  IF lcRpList = "B"
   *change the filter expr 'lcRpExp' to set the date range
   *In the same level [start]
   *The report gives error when using the transaction date.[START]
   *Add check for date,change or to and in LCRPEXP.   [Start]

   *Check for the posted filter also.[START]
    IF (!EMPTY(ldStrtDate) OR !EMPTY(ldEndDate)) OR IIF(lcRpDateTP = "P" , (!EMPTY(ldStrtDPst) OR !EMPTY(ldEndDPst)) , .F.)
   *Check for the posted filter also.[END]

      *Change AND to OR. [Begin]
      DO lpGenExpr
      *Change AND to OR. [End]

    ENDIF
    *Add check for date,change or to and in LCRPEXP.[End]
    *The report gives error when using the transaction date.[END]
    *in the same level[end]
  ENDIF
ENDIF
*Add void date to filter expression  [End  ]

SELECT INVHDR
SET ORDER TO   && Activate rushmore optimizing.
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer

IF !USED('ORDHDR')
  =gfOpenTable('ORDHDR','ORDHDR','SH')
ENDIF

SELECT INVHDR
SET RELATION TO 'O'+ Order INTO ORDHDR ADDITIVE 

*-- cTempKey : Field in work file that contains Country code, plus State code,
*--          : plus region code.
*--          : this field serves in sort by [Country,State,Region]


*When running the report to show both active and void invoices and for a specific
*division - the report prints the void invoices from all divisions.[START]
lcDivsion = ".T."
IF  "V" $ lcRpList OR "B" $ lcRpList
  STORE 0 TO lnDataDiv
  lnDataDiv = ASCAN(loOGScroll.laOGVrFlt,'INVHDR.CDIVISION')
  IF lnDataDiv > 0
    lnDataDiv  = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnDataDiv,1)
    lcDivsion = IIF(!EMPTY(loOGScroll.laOGVrFlt[lnDataDiv,6]) , "INVHDR.CDIVISION $ laOgVrFlt[lnDataDiv,6]", ".T.")
  ENDIF
ENDIF
*division - the report prints the void invoices from all divisions.[END]

*When running the report to show both active and void invoices and for a specific
*division - the report prints the void invoices from all divisions.[START]

*Replace the filter in case the posted date.[START]
IF lcRpDateTP = "P"
  lcRpExp = STRTRAN(lcRpExp,"INVDATE","DPOSTDATE")
  lcRpExp = STRTRAN(lcRpExp,"ldStrtDate","ldStrtDPst")
  lcRpExp = STRTRAN(lcRpExp,"ldEndDate","ldEndDPst")
ENDIF
*Replace the filter in case the posted date.[END]

SELECT INVHDR
SCAN FOR &lcRpExp AND &lcDivsion

  IF !EMPTY(lcAcctCursor) AND !SEEK(Account, lcAcctCursor)
    LOOP
  ENDIF

  IF !EMPTY(lcCurrCursor) AND !SEEK(cCurrCode, lcCurrCursor)
    LOOP
  ENDIF

  IF !EMPTY(lcRep1Cursor) AND !SEEK(Rep1, lcRep1Cursor)
    LOOP
  ENDIF

  IF !EMPTY(lcDivCursor)
    IF !SEEK(CDIVISION,lcDivCursor)
      LOOP
    ENDIF
  ENDIF
  IF !EMPTY(lcSeaCursor)
    IF !SEEK(SEASON,lcSeaCursor)
      LOOP
    ENDIF
  ENDIF
  IF !EMPTY(lcLocCursor)
    IF !SEEK(CWARECODE,lcLocCursor)
      LOOP
    ENDIF
  ENDIF
  IF !EMPTY(lcTrmCursor)
    IF !SEEK(CTERMCODE,lcTrmCursor)
      LOOP
    ENDIF
  ENDIF

  IF !EMPTY(lcStatCursor)




    IF !SEEK(LEFT(Customer.CADDRESS4,6), lcStatCursor)

      LOOP
    ENDIF
  ENDIF



*When running the report to show both active and void invoices and for a specific
*division - the report prints the void invoices from all divisions.[END]

  *Add wait window to show collecting data [Begin.]
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
            Invoice NOWAIT
  *Add wait window to show collecting data [End.]

  SCATTER MEMVAR MEMO
  *Empty value for the region in case any transaction for stores.[START]
  m.cTempKey = PADR(CUSTOMER.CADDRESS6,20)+PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))
  *Empty value for the region in case any transaction for stores.[END]

  m.cstname=CUSTOMER.btname
  m.priority =Ordhdr.Priority

  *Add the code that use the posted date.[START]
  IF lcRpDateTP = "P"
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND;
                                      !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))
      = lfNegValue() && Negative void values.
    ENDIF
  ELSE
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                                     !BETWEEN(INVDATE,ldStrtDate,ldEndDate))
      = lfNegValue() && Negative void values.
    ENDIF
  ENDIF
  *Add the code that use the posted date.[END]

  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN
*-- End of lfCollData.

*!*************************************************************
*! Name      : lfGetOpExp
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Make optimized expression then Collecting data
*!             from InvHdr file into Work file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfGetOpExp
lcOptimize = ''
PRIVATE lcAlias

lnAccPos = CEILING(ASCAN(loOGScroll.laOGVrFlt,'INVHDR.ACCOUNT')/7) && Position of Account in variable filter.
*-- if you find [Account is <In List>] in the variable filter section.
IF lnAccPos != 0
  llOpLogic  = loOGScroll.laOGVrFlt[lnAccPos,4]
  lcAccOp    = loOGScroll.laOGVrFlt[lnAccPos,5]
  *Modify the Optimized expression due to change the Account Screen From INLIST into INRANGE [Begin]
  IF loOGScroll.laOGVrFlt[lnAccPos,7]="V"
    lcAccVal   = loOGScroll.laOGVrFlt[lnAccPos,6]
  ELSE         &&loOGScroll.laOGVrFlt[lnAccPos,7]="R"       Case of using a cursor holding Selected Data
    lcAccVal = ""
    lcAlias  = Alias()
    lcCursor = loOGScroll.laOGVrFlt[lnAccPos,6]
    IF !EMPTY(lcCursor) AND USED(lcCursor)
      SELECT (lcCursor)
      SCAN
        IF EMPTY(lcAccVal)
          lcAccVal = Account
        ELSE
          lcAccVal=lcAccVal+"|"+Account
        ENDIF
      ENDSCAN
    ENDIF
    SELECT (lcAlias)
  ENDIF
  *Modify the Optimized expression due to change the Account Screen From INLIST into INRANGE [End]
  lcOperator = ''

  *-- If user assign values for account selection.
  IF !EMPTY(lcAccVal)

    *-- if Logic of account filter "Is"
    IF llOpLogic

      DO CASE
        CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Like
          lcOperator = '='
        CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatThan
          lcOperator = '>'
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessThan
          lcOperator = '<'
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatOrEql
          lcOperator = '>='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessOrEql
          lcOperator = '<='
      ENDCASE
    ELSE  && else Logic of account filter "Is Not"

      DO CASE
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Like
          lcOperator = '!='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatThan
          lcOperator = '<='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessThan
          lcOperator = '>='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatOrEql
          lcOperator = '<'
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessOrEql
          lcOperator = '>'
      ENDCASE

    ENDIF  && end if Logic of account filter "Is"

    *-- if Operator is [Contains or Between or InList]
    IF EMPTY(lcOperator)
      lcTrueFals = IIF(llOpLogic,'','!')  && Logic shape mask.
      IF ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Contain
        lcOptimize = [&lcTrueFals.(lcAccVal $ ACCOUNT+INVOICE)]
      ELSE  && Operator is either BETWEEN or INLIST.
        IF OCCUR('|',lcAccVal) = 0
          lcOptimize = [ACCOUNT+INVOICE &lcTrueFals.= lcAccVal]
        ELSE
		  lcOperator = IIF(ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Between,LANG_Arsjour_Between,LANG_Arsjour_InList)
          lcOptimize = [&lcTrueFals.&lcOperator(ACCOUNT+INVOICE,] + lfMakeOpt(lcAccVal) + [)]
        ENDIF

      ENDIF

    ELSE  && else Operator is some thing rather than [Contains or Between or InList]
      lcOptimize = [ACCOUNT+INVOICE &lcOperator. lcAccVal]
    ENDIF  && end if Operator is [Contains or Between or InList]

  ENDIF  && end If user assign values for account selection.

ENDIF  && end if you find [Account is <In List>] in the variable filter section.
*-- When there is no optimize expression [user delete Account from OG]
*-- or does not assign any values for account filter,
*-- INVOICE Index is faster than ACCOUNT+INVOICE Index in collecting data.
lcOptimize = IIF(EMPTY(lcOptimize),[INVOICE = ''],lcOptimize)
*-- End of lfGetOpExp.

*!*************************************************************
*! Name      : lfMakeOpt
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Make mask for optimize expression if operator
*!           : is either BETWEEN or INLIST.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : Optimized mask.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfMakeOpt
PARAMETERS lcString
PRIVATE lnPipeNo,lcExpr

lnPipeNo = OCCUR('|',lcString)

lcExpr = ''
FOR lnI = 1 TO lnPipeNo
  lcExpr    = IIF(EMPTY(lcExpr),"'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'",;
              lcExpr + "," + "'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'")
  lcString      = SUBSTR(lcString,ATC('|',lcString)+1)
ENDFOR
RETURN (lcExpr + "," + "'" + lcString + "'")
*-- End of lfMakeOpt.

*!*************************************************************
*! Name      : lfvOptMsg
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 65                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- End of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value
*-- End of lfwOldVal.
*!*************************************************************
*! Name      : lfGetRepVr
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : 1- Put both index and group expressions for all sort cases.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfGetRepVr
LOCAL lnResult3,lcSelectCommand3
llSortGrp = llMultCurr AND (lcRpCurr = 'F') AND (lcRpSortBy !='U')
lcCurrGrp = IIF(llSortGrp,[CCURRCODE],'')

DO CASE
  CASE lcRpSortBy = 'I'  && Sort by invoice
    IF lcRpKind = 'D'
      lcIndexTg  = [INVOICE]    && Index expression.
    ELSE
      lcIndexTg  = [CCURRCODE + INVOICE]    && Index expression.
    ENDIF


    lcSubTitle = [Invoice]    && Sub Title
    lcGroup    = ''           && Report Group
    lcGrpFoot  = ['']         && Group title

  CASE lcRpSortBy = 'A'  && Sort by account
    IF llSortGrp
      lcIndexTg  = [ACCOUNT + CCURRCODE + INVOICE]
    ELSE
      lcIndexTg  = [ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Account]
    lcGroup    = [ACCOUNT]
    lcGrpFoot  = [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))+;
                 ['+'# ' + ACCOUNT + " - " + ALLTRIM(CUSTOMER.BTNAME)]
    

  CASE lcRpSortBy = 'L'  && Sort by location
    IF llSortGrp
      lcIndexTg  = [CWARECODE + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [CWARECODE + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Location]
    lcGroup    = [CWARECODE]
    lcGrpFoot  = [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Location,oAriaApplication.GetHeaderText("LANG_Arsjour_Location",AHEADERFILE))+;
                ['+'# ' + cWareCode + " - " + ALLTRIM(WAREHOUS.CDESC)]
    

  CASE lcRpSortBy = 'R'  && Sort by primary sales rep.
    IF llSortGrp
      lcIndexTg  = [REP1 + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [REP1 + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [Primary Rep]
    lcGroup    = [REP1]
    lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+'# ' + Rep1 + " - " + ALLTRIM(SALESREP.NAME)]
  CASE lcRpSortBy = 'C'  && Sort by country

    IF llSortGrp
      lcIndexTg  = [LEFT(cTempKey,20) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [LEFT(cTempKey,20) + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Country]
    lcGroup    = [LEFT(cTempKey,20)]
	lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))+':  ' + ALLTRIM(CUSTOMER.cAddress6)]

  CASE lcRpSortBy = 'S'  && Sort by state
    IF llSortGrp
      lcIndexTg  = [SUBSTR(cTempKey,21,36)+ CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [SUBSTR(cTempKey,21,36) + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = SUBSTR(lcSTitle,1,8)

    lcGroup    = [SUBSTR(cTempKey,21,36)]


    IF !USED('SYCINT')
      =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')
    ENDIF
    
    
    lcSelectCommand3=[SELECT CCONT_CODE,CPART4LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=']+CUSTOMER.CCONT_CODE+[']
    
    lnResult3= oAriaApplication.remotesystemdata.execute(lcSelectCommand3,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))

    IF lnResult3 >= 1
      lcGrpFoot= [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))+['+': ' + LEFT(cTempKey,20) + '  ' +;
                  IIF(!EMPTY(ALLTRIM(SYCINT.CPART4LAB)), ALLTRIM(SYCINT.CPART4LAB) + ': ','') +;
                  ALLTRIM(SUBSTR(cTempKey,27,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,27,30)),'STATE')]
    ELSE
     lcGrpFoot= [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))+': ' + LEFT(cTempKey,20) + '  ' +ALLTRIM(SUBSTR(cTempKey,27,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,27,30)),'STATE')]
    ENDIF

  CASE lcRpSortBy = 'G'  && Sort by region
    IF llSortGrp
      lcIndexTg  = [RIGHT(cTempKey,6) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [RIGHT(cTempKey,6) + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Region]
    lcGroup    = [RIGHT(cTempKey,6)]
	lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Region,oAriaApplication.GetHeaderText("LANG_Arsjour_Region",AHEADERFILE))+': ' + gfCodDes(RIGHT(cTempKey,6),'REGION')]



  CASE lcRpSortBy = 'D'  && Sort by division
    IF llSortGrp
      lcIndexTg  = [cDIVISION + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cDIVISION + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Division]
    lcGroup    = [cDIVISION]
    lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Division,oAriaApplication.GetHeaderText("LANG_Arsjour_Division",AHEADERFILE))+'# ' + cDIVISION + '  ' + gfCodDes(cDIVISION,'CDIVISION')]
  CASE lcRpSortBy = 'T'  && Sort by terms
    IF llSortGrp
      lcIndexTg  = [cTermCode + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cTermCode + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [P. Terms]
    lcGroup    = [cTermCode]
	lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Terms,oAriaApplication.GetHeaderText("LANG_Arsjour_Terms",AHEADERFILE))+':  ' + gfCodDes(cTermCode,'CTERMCODE')]


  CASE lcRpSortBy = 'U'  && Sort by currency
    lcIndexTg  = [CCURRCODE + ACCOUNT + INVOICE]
    lcSubTitle = [Currency]
    lcGroup    = [CCURRCODE]
	lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))+'# ' + CCURRCODE + ' - ' + SYCCURR.cCurrDesc]
    lcCurrGrp  = ''
ENDCASE

* Add the second sort by option. [Begin]
IF lcRpSort2 <> 'N'
  PRIVATE lcAddSort , lc2ndTitle
  DO CASE
    CASE lcRpSort2 = 'I'  && Sort by invoice
      lcAddSort  = [INVOICE]
      lc2ndTitle = [Invoice]
    CASE lcRpSort2 = 'A'  && Sort by account
      lcAddSort  = [ACCOUNT]
      lc2ndTitle = [Account]
    CASE lcRpSort2 = 'L'  && Sort by location
      lcAddSort  = [CWARECODE]
      lc2ndTitle = [Location]
    CASE lcRpSort2 = 'R'  && Sort by primary sales rep.
      lcAddSort  = [REP1]
      lc2ndTitle = [Primary Rep]
    CASE lcRpSort2 = 'C'  && Sort by country
      lcAddSort  = [LEFT(cTempKey,20)]
      lc2ndTitle = [Country]
    CASE lcRpSort2 = 'S'  && Sort by state
      lcAddSort  = [SUBSTR(cTempKey,21,36)]
      lc2ndTitle = SUBSTR(lcSTitle,1,8)
    CASE lcRpSort2 = 'G'  && Sort by region
      lcAddSort  = [RIGHT(cTempKey,6)]
      lc2ndTitle = [Region]
    CASE lcRpSort2 = 'D'  && Sort by division
      lcAddSort  = [CDIVISION]
      lc2ndTitle = [Division]
    CASE lcRpSort2 = 'T'  && Sort by terms
      lcAddSort  = [CTERMCODE]
      lc2ndTitle = [P. Terms]
    CASE lcRpSort2 = 'U'  && Sort by currency
      lcAddSort  = [CCURRCODE]
      lc2ndTitle = [Currency]
  ENDCASE

  IF lcRpSortBy = 'I'
    lcIndexTg = lcIndexTg + [ + ] + lcAddSort
  ELSE
    lcIndexTg = SUBSTR(lcIndexTg,1,AT('+',lcIndexTg)) + [ ] + lcAddSort + [ + ] + SUBSTR(lcIndexTg,AT('+',lcIndexTg)+1)
  ENDIF
  lcSubTitle = lcSubTitle + [+] + lc2ndTitle
ENDIF
*Add the second sort by option. [End]
*-- End of lfGetRepVr.

*!*************************************************************
*! Name      : lfvChFact
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : 1- Change Factored/Non Factored logical variable To recollect data.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvChFact

llChFactor = .T.
*-- End of lfvChFact.

*!*************************************************************
*! Name      : lfvList
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : 1- Change Invoices/Void Invoices logical variable To recollect data.
*!           : 2- Enable and disable Invoice date or Void date due to user selection.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvList
*!*  _screen.Visible =.T.

llChInv = .T.  && Logical invoice variable.
llVoidOnly = (lcRpList='V')  && To use it in .FRX
*-- End of lfvList.

*!*************************************************************
*! Name      : lfvCurDisp
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
= lfGetRepVr()
*-- End of lfvCurDisp.

*!*************************************************************
*! Name      : lfvCurr
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Validate Currency code in SYCCURR file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp

lcVar = OGSYS18()                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(OGSYS18())      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SYCCURR'))

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = [CCURRCODE:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))+[' ,    ;
                CCURRSMBL:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Symbol,oAriaApplication.GetHeaderText("LANG_Arsjour_Symbol",AHEADERFILE))+[' ,      ;
                CCURRDESC:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Description,oAriaApplication.GetHeaderText("LANG_Arsjour_Description",AHEADERFILE))+[' , ;
				NCURRUNIT:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Units,oAriaApplication.GetHeaderText("LANG_Arsjour_Units",AHEADERFILE))+[' ]

  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currencies,oAriaApplication.GetHeaderText("LANG_Arsjour_Currencies",AHEADERFILE))

  = gfBrows('','CCURRCODE','laTemp')

  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF

ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- End of lfvCurr.

*!*************************************************************
*! Name      : lfsrAcc
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfCollTime
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Parameters: Start collection date,End collection date
*!*************************************************************
*! Returns   : Spent time.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!*************************************************************
*! Name      : lfCreatCur
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Create cursor
*!*************************************************************
*! Parameters: Cursor Name
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCreatCur
PARAMETERS lcCurName

gfCrtTmp(lcCurName,@laTempStru,"lcIndexTg",lcWorkFile,.F.)
*-- End of lfCreatCur.

*!*************************************************************
*! Name      : lfClearRep
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.    && If you run filter you must create cursor again.

*-- Close temp. opended files, if it used.

*-- Delete temporary work file.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF

*-- if user change setting [enter report <Preview> or <Run>]
IF !llFrTime
  *SET CENTURY &lcCentury
  SET HOURS TO &lcSetHour
ENDIF  && end if user change setting [enter report <Preview> or <Run>].
*-- End of lfClearRep.

*!*************************************************************
*! Name      : lfPreRun
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Pre_Preview Function To call Temporary .FRX again
*!           : to evaluate #OBJDISP objects again.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!           : 2- While this function has one line of code calls another function
*!           :    I write it for other reasons and for any other to add any
*!           :    enhancement code.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfPreRun
= lfGetRepVr()      && Get Report variables such as groups and index.
RETURN .T.
*-- End of lfPreRun.

*!*************************************************************
*! Name      : lfSortDumy
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Fill Sort Array
*!*************************************************************
*! Parameters: ...
*!*************************************************************
*! Returns   : ...
*!*************************************************************
*! Modification : ....
*!*************************************************************
*-- Fill Sort Arrays.
FUNCTION lfSortDumy
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 8
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDesc[lnArrElmnt,1],laSortVal[lnArrElmnt,1]
laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))
laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))
laSortDesc[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalRepresent,oAriaApplication.GetHeaderText("LANG_Arsjour_SalRepresent",AHEADERFILE))
laSortDesc[4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))
laSortDesc[5] = lcSTitle       && State variable Title
laSortDesc[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Region,oAriaApplication.GetHeaderText("LANG_Arsjour_Region",AHEADERFILE))
laSortDesc[7] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Division,oAriaApplication.GetHeaderText("LANG_Arsjour_Division",AHEADERFILE))
laSortDesc[8] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PaymentTerms,oAriaApplication.GetHeaderText("LANG_Arsjour_PaymentTerms",AHEADERFILE))

laSortVal[1] = 'I'
laSortVal[2] = 'A'
laSortVal[3] = 'R'
laSortVal[4] = 'C'
laSortVal[5] = 'S'
laSortVal[6] = 'G'
laSortVal[7] = 'D'
laSortVal[8] = 'T'

IF llMultLoc
  =AINS(laSortDesc,3)
  laSortDesc[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Location,oAriaApplication.GetHeaderText("LANG_Arsjour_Location",AHEADERFILE))
  =AINS(laSortVal,3)
  laSortVal[3] = 'L'
ENDIF

IF llMultCurr
  laSortDesc[ALEN(laSortDesc,1)] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))
  laSortVal[ALEN(laSortDesc,1)]  = 'U'
ENDIF
*-- End of lfSortDumy.

*!*************************************************************
*! Name      : lfItmPos
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Parameters: ...
*!*************************************************************
*! Returns   : Position
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(loOGScroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!*************************************************************
*! Name      : lfNegValue
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : -Ve Void values
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfNegValue
PRIVATE lnFldsCnt , lcMemField
lcMemField = ''
lnFldsCnt = 0
FOR lnFldsCnt = 1 TO FCOUNT()
  IF TYPE(FIELD(lnFldsCnt)) = "N"
    lcMemField = "m." + FIELD(lnFldsCnt)
    &lcMemField = -1 * &lcMemField
  ENDIF
ENDFOR
*-- End of lfNegValue.

*!*************************************************************
*! Name      : lfRegion
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : To get the data from the store fields.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfRegion
PRIVATE lcRegion , lnCUSRec


lcAlias = SELECT(0)
SELECT CUSTOMER
lnCUSRec = EVAL(KEY())

IF SEEK('M'+CUSTOMER.Account)
  lcRegion = PADR(CUSTOMER.REGION,6)
ENDIF

=SEEK(lnCUSRec , 'CUSTOMER')

SELECT(lcAlias)
RETURN lcRegion
*-- End of lfRegion.
*!**************************************************************************
*! Name      : lfSortDum2
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Fill second sort bu option
*!**************************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!**************************************************************************
FUNCTION lfSortDum2
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 9
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDes2[lnArrElmnt,1],laSortVal2[lnArrElmnt,1]
laSortDes2[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_None,oAriaApplication.GetHeaderText("LANG_Arsjour_None",AHEADERFILE))
laSortDes2[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))
laSortDes2[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))
laSortDes2[4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalRepresent,oAriaApplication.GetHeaderText("LANG_Arsjour_SalRepresent",AHEADERFILE))
laSortDes2[5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))
laSortDes2[6] = lcSTitle       && State variable Title
laSortDes2[7] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Region,oAriaApplication.GetHeaderText("LANG_Arsjour_Region",AHEADERFILE))
laSortDes2[8] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Division,oAriaApplication.GetHeaderText("LANG_Arsjour_Division",AHEADERFILE))
laSortDes2[9] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PaymentTerms,oAriaApplication.GetHeaderText("LANG_Arsjour_PaymentTerms",AHEADERFILE))

laSortVal2[1] = 'N'
laSortVal2[2] = 'I'
laSortVal2[3] = 'A'
laSortVal2[4] = 'R'
laSortVal2[5] = 'C'
laSortVal2[6] = 'S'
laSortVal2[7] = 'G'
laSortVal2[8] = 'D'
laSortVal2[9] = 'T'

IF llMultLoc
  =AINS(laSortDes2,4)
  laSortDes2[4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Location,oAriaApplication.GetHeaderText("LANG_Arsjour_Location",AHEADERFILE))
  =AINS(laSortVal2,4)
  laSortVal2[4] = 'L'
ENDIF

IF llMultCurr
  laSortDes2[ALEN(laSortDes2,1)] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))
  laSortVal2[ALEN(laSortDes2,1)]  = 'U'
ENDIF
*-- End of lfSortDum2.

*!**************************************************************************
*! Name      : lfvSort
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Valid function for both 1st , 2nd Sort by option.
*!**************************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!**************************************************************************
FUNCTION lfvSort
DO lpShowObj
IF lcRpSortBy = lcRpSort2
  lcRpSort2 = 'N'
loOGScroll.EnableObject('lcRpSort2', lcRpSortBy <> 'I')
ENDIF
*-- End of lfvSort.

*!***************************************************************************
*! Name      : lpShowObj
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Enable/Disable all options in OG
*!***************************************************************************
*! Example   : DO lpShowObj
*!***************************************************************************
PROCEDURE lpShowObj
PRIVATE llShwInRng

IF lnSortPos > 0
  IF lcRpSortBy = 'I'
    lcRpSort2 = 'N'
  ELSE
  ENDIF
  loOGScroll.EnableObject('lcRpSort2',lcRpSortBy <> 'I')
ENDIF
*-- End of lShowObj.

*!***************************************************************************
*! Name      : lpGenExpr
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Generate Expression.
*!***************************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!**************************************************************************
*Function to add the void expr. after the invoice date expr.
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX , lcInvExp , lcVoidExp

lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1]
=ACOPY(loOGScroll.laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.

*-- Define new Fixed filter array to hold one expression only.
DIMENSION loOGScroll.laOGFxFlt[1,7]
*-- Copy only Month Range expression to laOGFxFlt.
*Modify the array to use the posted date filter.[START]
IF lcRpDateTP = "P"
  =ACOPY(laTempExpr,loOGScroll.laOGFxFlt,AELEMENT(laTempExpr,lnPostDate,1),7)
ELSE
  =ACOPY(laTempExpr,loOGScroll.laOGFxFlt,AELEMENT(laTempExpr,lnInvPos,1),7)
ENDIF
*Modify the array to use the posted date filter.[END]

*-- Generate expression for Month Range.
lcInvExp = gfGenFlt('loOGScroll.laOGFxFlt',.T.,.T.)
lcInvExp = STRTRAN(lcInvExp,"INVHDR.","")

lcVoidExp = "( " + lcInvExp + " OR BETWEEN(IIF(STATUS='V',VDATE,INVDATE),ldStrtDate,ldEndDate) )"

*-- Remove Style Group from lcRpExp.
lcRpExp = STRTRAN(lcRpExp,lcInvExp,lcVoidExp)
*-- If user selected Style.

*-- Restore original laOGFxFlt.
DIMENSION loOGScroll.laOGFxFlt[1]
=ACOPY(laTempExpr,loOGScroll.laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!*************************************************************
*! Name      : lfClrRead
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Function used to suppressing the field in the grid.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modifications:  ....
*!*************************************************************
* Refresh the option grid in case the posted date.[START]
FUNCTION lfClrRead
ClearRead()
* Refresh the option grid in case the posted date.[END]
*-- End of lfClrRead.
*!*************************************************************
*! Name      : lfOGTempCurs
*: Developer : Mariam Mazhar(MMT)
*: Date      : 08/25/2014
*! Purpose   : Function used to create Temp Cursor from OG Expr.
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Return    : Cursor Name
*!*************************************************************
FUNCTION lfOGTempCurs
LPARAMETERS lcExpr, lcFldName, lcIndexExp, laOgArray, llInList

LOCAL lnAlias, lcCursor, lnPos, lcValue, lnStart, lnEnd
lnAlias  = SELECT(0)
lnPos    = ASCAN(&laOgArray, lcExpr)
lnPos    = IIF(lnPos > 0, ASUBSCRIPT(&laOgArray, lnPos, 1), 0)
lcCursor = ""

IF lnPos > 0
  lcValue = &laOgArray.[lnPos,6]

  IF !llInList
    RETURN lcValue
  ENDIF

  IF !EMPTY(lcValue)
    lcCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Divisions
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1] = lcFldName
    laTempacstru[1,2] = 'C'
    laTempacstru[1,3] = 6
    laTempacstru[1,4] = 0
    gfCrtTmp(lcCursor, @laTempacstru, lcIndexExp, lcCursor, .T.)
    IF !EMPTY(lcValue)
      lnStart = 1
      lnEnd   = AT('|', lcValue)
      DO WHILE lnEnd <> 0
        SELECT (lcCursor)
        APPEND BLANK
        REPLACE &lcFldName WITH SUBSTR(lcValue, lnStart, lnEnd - 1)

        lcValue = STUFF(lcValue, lnStart, lnEnd, "")
        lnEnd   = AT('|', lcValue)
      ENDDO
      IF lnEnd = 0
        SELECT (lcCursor)
        APPEND BLANK
        REPLACE &lcFldName WITH lcValue
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(lcCursor)
  SET ORDER TO TAG (lcCursor)
ENDIF

SELECT (lnAlias)
RETURN IIF(!EMPTY(lcCursor) AND USED(lcCursor), lcCursor, "")
