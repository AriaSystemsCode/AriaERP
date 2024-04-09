*:***************************************************************************
*: Program file  : ARPLETT
*: Program desc. : Customer/Vendor Letters Report
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable/Payable (AR/AP)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARPLETT
*:***************************************************************************
*: This Report Program is due to E301119,1 ...
*:***************************************************************************
*: Modifications :
*: E500341,1 BWA 05/29/2000 Modify inrange screen with the criteria of the style
*: E500341,1                season division fabric and style group to give the
*: E500341,1                user the ability to select the color.
*: B803344,1 BWA 06/18/2000 1) Fix the bug of variable [lcOrdCanc] not found.
*: B803344,1                the record hold the temp name of [lcOrdCanc] not found 
*: B803344,1                in the syrepuvr file.
*: B603716,1 BWA 06/29/2000 Fix the bug of alias name not found in case the user select 
*: B603716,1                all records and then select color.
*: B603748,1 BWA 07/13/2000 Fix the bug of alias not found.
*: B804205,1 AME 06/10/2001 Fix the bug of No records to display when choosing 
*: B804205,1 AME            All styles then all styles/colors.
*: B604402,1 AME 08/26/2001 cancel the selected lines from the ordline if 
*: B804386,1 SSE 09/18/2001 Fix bug of not updating the right warehouse in StyDye file.
*: B606686,1 ABD 12/01/2002 Fix bug of The report shows colors of styles that were not Chosen. 
*: E302174,1 ALB 06/04/2003 Add Vendor Letter report to AP Modual
*: B607389,1 ALB 08/05/2003 Fix Bugs Relate to E302174
*: E119114,1 ASH 08/14/2003 Display the WIP on the selected styles browse.
*: B121840,1 NNA 03/17/2004 Fix bug that if we don't have 'MF' and using [Customer Letter] in the sales
*: B121840,1 NNA            order module and try to Run then an error massege (cuttktH.dbf) is missing
*:***************************************************************************
*
*-- Nothing rather than functions.
*

*!*************************************************************
*! Name      : lfWRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : OG when function.
*!*************************************************************
*
FUNCTION lfWRepWhen
laOGHdFlt[1,6] = ALLTRIM(laOGHdFlt[1,6])

IF EMPTY(lcRpLetTyp)
  lcRpLetTyp = laLettVal[1,1]
ENDIF
=lfOGShowGet('lcRpLetTyp')

*-- if Default setting.
IF lnOGSeting = 1
  lcRpBott = lfDefaText()
  =lfOGShowGet('lcRpBott')
ENDIF

lcActivObj = lfActivObj() 

*-- Get Object Positions for good optimized filteration. [Begin]
*lnAccPos  = lfItmPos('CUSTOMER.ACCOUNT')
lnProfile = lfItmPos('PROFILE.CPRO_CODE')
lnProfVal = lfItmPos('PROFILE.CPRO_VALUE')
lnStyPos   = lfItmPos('STYLE.CSTYMAJOR')
lnFabPos   = lfItmPos('STYLE.FABRIC')
lnRepPos   = lfItmPos('ORDHDR.REP1')
lnGrpPos   = lfItmPos('STYLE.CSTYGROUP')
lnSeaPos   = lfItmPos('STYLE.SEASON')
lnDivPos   = lfItmPos('STYLE.CDIVISION')
lnDecDtPos = lfItmPos('ORDHDR.DECL_DATE')
*-- Get Object Positions for good optimized filteration. [End  ]

=lfvProFile()
DO CASE
  CASE "Ext" $ lcActivObj
    lcRpRepFrm = "AREXTRPA"
    lcRpLetFrm = "AREXTLTA"
  
  CASE "Can" $ lcActivObj
    lcRpRepFrm = "ARCANRPA"
    lcRpLetFrm = "ARCANLTA"

  CASE "Dec" $ lcActivObj
    lcRpRepFrm = "ARDECRPA"
    lcRpLetFrm = "ARDECLTA"

  CASE "Gen" $ lcActivObj
    lcRpRepFrm = ""
    *E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
    *lcRpLetFrm = "ARGENLTA"
    lcRpLetFrm = IIF(lcLetrTo = "V","APGENLTA","ARGENLTA")
    *E302174,1 ALB Add Vendor Letter report to AP Modual [End]

ENDCASE
*E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
IF !USED('APVENDOR')
  =gfOpenFile(gcDataDir+"APVENDOR","VENCODE","SH")
ENDIF
*E302174,1 ALB Add Vendor Letter report to AP Modual [end]

=lfObjState()
=lfvCancOrd()

*-- end of lfWRepWhen.

*!*************************************************************
*! Name      : lfCanProcd
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Fill Letters type array and detect that we can proceed or no
*!*************************************************************
*
FUNCTION lfCanProcd
PARAMETERS lcLetterTo
PRIVATE lcMessage , llCanProcd , lnI
llCanProcd = .T.
lcLetrTo = lcLetterTo

SELECT LETTERS
SET ORDER TO LetterTo
llNoLetter = !SEEK(lcLetrTo)

IF llNoLetter
  *-- if Customer Letters.
  IF lcLetrTo = "C"
    *Message : 40167 => No XXX letters found, Do you want to print report only?
    *Button  : 40000 => <Yes> <No>
    IF gfModalGen('QRM40167B40000','Dialog',"Customer") = 2
      llCanProcd = .F.
    ENDIF
  ELSE  && else if vendor letters.
    *Message : 04192 => No XXX letters found, Cannot proceed!
    *Button  : 00000 => <Ok>
    = gfModalGen('TRM04192B00000','Dialog',"Vendor")
    llCanProcd = .F.
  ENDIF  
ENDIF

lcFrstItem = IIF(lcLetrTo="C","Extension","General Purpose")

IF llCanProcd
  IF llNoLetter
    lnAryItems = 3
  ELSE
    lnAryItems = IIF(lcLetrTo="C",4,1)
  ENDIF

  SET ORDER TO
  DIMENSION laLettDesc[lnAryItems,1] , laLettVal[lnAryItems,1]
  lnI = 0
  FOR lnI = 1 TO lnAryItems
    DO CASE
      CASE lnI = 1
        laLettDesc[lnI,1] = lcFrstItem

      CASE lnI = 2
        laLettDesc[lnI,1] = "Cancellation"
      
      CASE lnI = 3
        laLettDesc[lnI,1] = "Decline"
      
      CASE lnI = 4
        laLettDesc[lnI,1] = "General Purpose"

    ENDCASE
    laLettVal[lnI,1] = "lf" + LEFT(laLettDesc[lnI,1],3) + "Obj()"
    
    *-- Fill a letter array.    
    =lfFillAray(lcLetrTo+LEFT(laLettDesc[lnI,1],1),LEFT(laLettDesc[lnI,1],3))
  ENDFOR

  =lfTranTemp()  && Create Transaction Temp. Cursor

ELSE
  llOgTrmnat = .T.
  CLEAR READ
ENDIF
*-- end of lfCanProcd.

*!*************************************************************
*! Name      : lfFillAray
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Fill a specific array values.
*!*************************************************************
*
FUNCTION lfFillAray
PARAMETERS lcWhereVal , lcArrayNam
PRIVATE lcWhereVal , lcArrayNam , lcDesArray , lcValArray , lnItems , lnJ , lnStart
lcDesArray = "la" + lcArrayNam + "Lettr"
DIMENSION &lcDesArray[1,1]

SELECT cLetterId + ' - ' + cLetShDes;
 FROM Letters;
 WHERE (casdefault+cleterto+cletertype+cletterid = " "+lcWhereVal) OR ;
       (casdefault+cleterto+cletertype+cletterid = "D"+lcWhereVal) ;
 ORDER BY Letters.cletterid ;
 INTO ARRAY &lcDesArray

lnItems = _TALLY
lcValArray = "la" + lcArrayNam + "Val"

IF lnItems = 0
  DIMENSION &lcDesArray[1,1] , &lcValArray[1, 1]
  &lcDesArray[1,1] = "N/A"
  &lcValArray[1,1] = ""

ELSE
  
  lnStart = 1 
  IF lnItems=1
    DIMENSION &lcValArray[1,1]
  ELSE
    DIMENSION &lcDesArray[lnItems+1,1] , &lcValArray[lnItems+1,1]
    =AINS(&lcDesArray,1)
    &lcDesArray[1,1] = "Select Per " + IIF(lcLetrTo = "C","Account","Vendor")
    &lcValArray[1,1] = ""
    lnStart = 2
  ENDIF  
  
  lnJ = 0
  FOR lnJ = lnStart TO ALEN(&lcDesArray,1)
    &lcValArray[lnJ,1] = PADR(&lcDesArray[lnJ,1],10)
  ENDFOR
ENDIF
*-- end of lfFillAray.

*!*************************************************************
*! Name      : lfTranTemp
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Create Transaction Temp. Cursor
*!*************************************************************
*
FUNCTION lfTranTemp
IF USED(lcTempMast)
  USE IN (lcTempMast)
ENDIF
*-- IF customer letter
IF lcLetrTo = "C"
  CREATE CURSOR (lcTempMast) (Account C(5) , cDefaLett C(10), cOldLetter C(10))
  SELECT (lcTempMast)
  INDEX ON Account + cDefaLett TAG (lcTempMast) OF (gcWorkDir+lcTempMast+'.CDX')
ELSE  && else if vendor letter.
  CREATE CURSOR (lcTempMast) (cVendCode C(8) , cDefaLett C(10), cOldLetter C(10))
  SELECT (lcTempMast)
  INDEX ON cVendCode + cDefaLett TAG (lcTempMast) OF (gcWorkDir+lcTempMast+'.CDX')
ENDIF
ZAP
*-- end of lfTranTemp.


*!*************************************************************
*! Name      : lfvLetrTyp
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter type validation.
*!*************************************************************
*
FUNCTION lfvLetrTyp
=RECCOUNT(lcTempMast) > 0 AND lfTranTemp()
llClrTrans = .T.
laOrdTemp  = ''
CLEAR READ  && Apply suppress expression.
*-- end of lfvLetrTyp.

*!*************************************************************
*! Name      : lfExtObj
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Deal with extension type (Name save in lcRpLetTyp)
*!*************************************************************
*
FUNCTION lfExtObj

*-- if you does not have any collected data for this report.
IF !lfRepoData([ AND DTOS(Complete) < DTOS(ldRpComplt)])
  RETURN .F.
ENDIF

*-- if user want to update new complete date.
*-- Message : Are you sure you want to 
*--         : update the order(s) with new completion date?
*-- Button  : <Yes> <No>
lcUpdtMsg = "update the order(s) with new completion date"
IF llUpdatNow AND llRpUpComp AND gfModalGen('QRM40169B40000','Dialog',lcUpdtMsg) = 1
  SET ORDER TO ORDLINE IN ORDLINE
  SET ORDER TO ORDHDR IN ORDHDR

  SELECT (lcOrdTemp)
  SET ORDER TO WithHdr
  *-- Scan Temp. File to update Master files.
  SCAN
    IF SEEK(cOrdType+Order,"ORDHDR")
      SELECT ORDHDR
      =RLOCK()
      REPLACE Complete WITH ldRpComplt
      UNLOCK
      IF SEEK(cOrdType+Order,"ORDLINE")
        SELECT ORDLINE
        SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = OrdHdr.cOrdType + OrdHdr.Order
          =RLOCK()
          REPLACE Complete WITH ldRpComplt
          UNLOCK
        ENDSCAN
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lcOrdTemp)
  ZAP  && Now No Selected records for current Filter.
ENDIF
*-- end of lfExtObj.

*!*************************************************************
*! Name      : lfCanObj
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Deal with Cancellation type (Name save in lcRpLetTyp)
*!*************************************************************
*
FUNCTION lfCanObj
*-- if you does not have any collected data for this report.
IF !lfRepoData("")
  RETURN .F.
ENDIF

*-- if user want to cancel all the line items from the orders.
*-- Message : Are you sure you want to 
*--         : cancel all the line item(s) from the selected order(s)?
*-- Button  : <Yes> <No>
lcCancMsg = "cancel all the line item(s) from the selected order(s)"
IF llUpdatNow AND llRpCanSel AND gfModalGen('QRM40169B40000','Dialog',lcCancMsg) = 1
  *Comment it till finishing
  *B604402,1 AME [Start]
  =lfCancLine()
  *B604402,1 AME [End]
ENDIF
*-- end of lfCanObj.

*!*************************************************************
*! Name      : lfDecObj
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Deal with DecLine type (Name save in lcRpLetTyp)
*!*************************************************************
*
FUNCTION lfDecObj
PRIVATE laCodes
IF llOGFltCh
  PRIVATE lcHiddFilt,lcProVExp,lcProfExp,lcDecExpr,;
          lcRepExp,lcProFExpr,lcOrdHExpr,laBrTmpFlt

  STORE '' TO lcHiddFilt,lcProVExp,lcProfExp,lcDecExpr,;
          lcRepExp,lcProFExpr,lcOrdHExpr,laBrTmpFlt
          
  IF !USED(lcHdrTemp) OR RECCOUNT(lcHdrTemp) > 0
    =lfHdrTemp()  && Create Temp. Order Header file.
  ENDIF
  lcHiddFilt = ["DECL"$Approval AND Status # "C"]

  DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
  =ACOPY(laBrFldFlt,laBrTmpFlt)
  FOR lnI = 1 TO ALEN(laBrFldFlt,1)
    IF "LAOGFXFLT" $ UPPER(laBrFldFlt[lnI,5])
      laBrFldFlt[lnI,5]  = "laTempExpr:1"
    ENDIF
  ENDFOR

  IF lnProfile > 0
    lcProfExp = lfItemFilt(lnProfile)
    =lfAddTo(@lcProFExpr,lcProfExp)
  ENDIF  

  IF lnProfVal > 0
    lcProVExp = lfItemFilt(lnProfVal)
    IF "laTempExpr[1,6]" $ lcProVExp
      lcProVExp = STRTRAN(lcProVExp,"laTempExpr[1,6]","laOGFxFlt[lnProfVal,6]")
    ENDIF
    =lfAddTo(@lcProFExpr,lcProVExp)
  ENDIF  

  IF lnDecDtPos > 0
    lcDecExpr = lfItemFilt(lnDecDtPos)
    =lfAddTo(@lcOrdHExpr,lcDecExpr)
  ENDIF

  IF lnRepPos > 0
    lcRepExp  = lfItemFilt(lnRepPos)
    =lfAddTo(@lcOrdHExpr,lcRepExp)
  ENDIF  

  =ACOPY(laBrTmpFlt,laBrFldFlt)

  *-- Collect decline data.
  =lfDecData()
ENDIF

IF RECCOUNT(lcHdrTemp) = 0
  RETURN .F.
ENDIF

DIMENSION laCodes[2,3]
laCodes[1,2] = "DECL_CODE"
laCodes[2,2] = "CTERMCODE"

SELECT (lcHdrTemp)
SET RELATION TO IIF(EMPTY(Store),"M","S")+Account+Store INTO CUSTOMER

DO lpPrnLettr  && Print Procedure.

SELECT (lcHdrTemp)
SET RELATION TO

IF USED(lcWithMe)
  USE IN (lcWithMe)
ENDIF
*-- end of lfDecObj.

*!*************************************************************
*! Name      : lfGenObj
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Deal with General purpose type (Name save in lcRpLetTyp)
*!*************************************************************
*
FUNCTION lfGenObj

    
    *lcRpLetFrm = "ARGENLTA"
    lcRpLetFrm = IIF(lcLetrTo = "V","APGENLTA","ARGENLTA")



IF !EOF(lcTempMast)
  *E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
  *lcRpExp = lcRpExp + [ AND SEEK(CUSTOMER.ACCOUNT,lcTempMast)]
  lcRpExp = lcRpExp + IIF(lcLetrTo = "V",[ AND SEEK(APVENDOR.CVENDCODE,lcTempMast)],;
                                         [ AND SEEK(CUSTOMER.ACCOUNT,lcTempMast)])
  *E302174,1 ALB Add Vendor Letter report to AP Modual [end]
ENDIF
  
*-- Set Customer Profile relation.
SELECT CUSTOMER
*E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
IF lcLetrTo = "V"
  SELECT APVENDOR
ENDIF
*E302174,1 ALB Add Vendor Letter report to AP Modual [end]

IF !EMPTY(laOGFxFlt[lnProfile,6]) OR !EMPTY(laOGFxFlt[lnProfVal,6])

  *E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
  *SET RELATION TO "C"+Account+SPACE(8) INTO ProFile
  IF lcLetrTo = "V"
    SET RELATION TO "V"+CVENDCODE+SPACE(8) INTO ProFile
  ELSE
    SET RELATION TO "C"+Account+SPACE(8) INTO ProFile
  ENDIF
  *E302174,1 ALB Add Vendor Letter report to AP Modual [end]
  SET SKIP TO Profile
ENDIF

LOCATE FOR &lcRpExp
IF !FOUND()
  SELECT CUSTOMER

  *E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
  IF lcLetrTo = "V"
    SELECT APVENDOR
  ENDIF
  *E302174,1 ALB Add Vendor Letter report to AP Modual [end]

  SET RELATION TO
  RETURN .F.
ENDIF  

DO lpPrnLettr       && Print Procedure.

*-- Reset Profile relation.
SELECT CUSTOMER
*E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
IF lcLetrTo = "V"
  SELECT APVENDOR
ENDIF
*E302174,1 ALB Add Vendor Letter report to AP Modual [end]

SET RELATION TO

*-- end of lfGenObj.

*!*************************************************************
*! Name      : lfDecData
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Decline data collection.
*!*************************************************************
*
FUNCTION lfDecData
PRIVATE lcOrdForEx

GO TOP IN (lcTempMast)
lcOrdForEx = lcHiddFilt
=lfAddTo(@lcOrdForEx,lcOrdHExpr)
lcOrdForEx = [FOR ] + lcOrdForEx
lcOrdForEx = STRTRAN(lcOrdForEx,"ORDHDR.","")
SELECT ORDHDR

IF EOF(lcTempMast)
  SET ORDER TO
  =lfOrdHdr("")

ELSE
  
  SET ORDER TO Ordacct
  SELECT (lcTempMast)
  SCAN
    IF SEEK(Account,"ORDHDR")
      =lfOrdHdr("REST WHILE Account+cOrdType+Order = " + "'" + Account + "'")
    ENDIF  
  ENDSCAN
ENDIF
*-- end of lfDecData.

*!*************************************************************
*! Name      : lfOrdHdr
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Collect ordhdr records.
*!*************************************************************
*
FUNCTION lfOrdHdr
PARAMETERS lcWhileExp
PRIVATE lcWhileExp
lcWhileExp = LTRIM(lcWhileExp + " " + lcOrdForEx)
lcProFExpr = STRTRAN(lcProFExpr,"PROFILE.","")

SELECT ORDHDR
SCAN &lcWhileExp
  IF EMPTY(lcProFExpr) OR lfProFOk(Account,Store)
    SCATTER MEMVAR MEMO
    INSERT INTO (lcHdrTemp) FROM MEMVAR
  ENDIF   
ENDSCAN
*-- end of lfOrdHdr.

*!*************************************************************
*! Name      : lfRepoData
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Report / Letter Data.
*!*************************************************************
*
FUNCTION lfRepoData
PARAMETERS lcAddedFlt

IF llOGFltCh
  PRIVATE lcAddedFlt,lcHiddFilt,lcSeaExp,lcDivExp,lcGrpExp,lcProVExp,lcProfExp,;
          lcRepExp,lcFabExp,lcStyExp,lcStyleExp,lcProFExpr,lcOrdHExpr,laBrTmpFlt

  STORE '' TO lcHiddFilt,lcSeaExp,lcDivExp,lcGrpExp,lcProVExp,lcProfExp,;
              lcRepExp,lcFabExp,lcStyExp,lcStyleExp,lcProFExpr,lcOrdHExpr,laBrTmpFlt

  *B603748,1 BWA 07/13/2000 Fix the bug of alias not found.[START]
  *-- if Cancellation letter.
  IF "Can" $ lcActivObj
    *-- Apply Cancellation process.
    IF USED(lcOrdCanc)
      USE IN (lcOrdCanc)
    ENDIF

    CREATE CURSOR (lcOrdCanc) (Order C(6) , Open N(12) , mPikTkt M(10))
    ZAP
    INDEX ON Order TAG (lcOrdCanc) OF (gcWorkDir+lcOrdCanc+".CDX")
  ENDIF
  *B603748,1 [END]
  
  IF !USED(lcOrdTemp) OR RECCOUNT(lcOrdTemp) > 0
    =lfOrdTemp()  && Create Temp. Order Line file.
  ENDIF
  lcHiddFilt = [TotQty > 0 AND OrdHdr.Status $ "OH"] + lcAddedFlt

  
  DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
  =ACOPY(laBrFldFlt,laBrTmpFlt)
  FOR lnI = 1 TO ALEN(laBrFldFlt,1)
    IF "LAOGFXFLT" $ UPPER(laBrFldFlt[lnI,5])
      laBrFldFlt[lnI,5]  = "laTempExpr:1"
    ENDIF
  ENDFOR

  IF lnSeaPos > 0
    lcSeaExp  = lfItemFilt(lnSeaPos)
    IF "laTempExpr[1,6]" $ lcSeaExp
      lcSeaExp = STRTRAN(lcSeaExp,"laTempExpr[1,6]","laOGFxFlt[lnSeaPos,6]")
    ENDIF
    =lfAddTo(@lcStyleExp,lcSeaExp)
  ENDIF

  IF lnDivPos > 0
    lcDivExp  = lfItemFilt(lnDivPos)
    IF "laTempExpr[1,6]" $ lcDivExp
      lcDivExp = STRTRAN(lcDivExp,"laTempExpr[1,6]","laOGFxFlt[lnDivPos,6]")
    ENDIF
    =lfAddTo(@lcStyleExp,lcDivExp)
  ENDIF  

  IF lnGrpPos > 0
    lcGrpExp  = lfItemFilt(lnGrpPos)
    IF "laTempExpr[1,6]" $ lcGrpExp
      lcGrpExp = STRTRAN(lcGrpExp,"laTempExpr[1,6]","laOGFxFlt[lnGrpPos,6]")
    ENDIF
    =lfAddTo(@lcStyleExp,lcGrpExp)
  ENDIF  

  IF lnFabPos > 0
    lcFabExp  = lfItemFilt(lnFabPos)
    =lfAddTo(@lcStyleExp,lcFabExp)
  ENDIF  

  IF lnProfile > 0
    lcProfExp = lfItemFilt(lnProfile)
    =lfAddTo(@lcProFExpr,lcProfExp)
  ENDIF  

  IF lnProfVal > 0
    lcProVExp = lfItemFilt(lnProfVal)
    IF "laTempExpr[1,6]" $ lcProVExp
      lcProVExp = STRTRAN(lcProVExp,"laTempExpr[1,6]","laOGFxFlt[lnProfVal,6]")
    ENDIF
    =lfAddTo(@lcProFExpr,lcProVExp)
  ENDIF  

  IF lnRepPos > 0
    lcRepExp  = lfItemFilt(lnRepPos)
    =lfAddTo(@lcOrdHExpr,lcRepExp)
  ENDIF  

  IF lnStyPos > 0
    lcStyExp  = lfItemFilt(lnStyPos)
  ENDIF  

  =ACOPY(laBrTmpFlt,laBrFldFlt)

  *-- Collecting data.
  =lfFillData()
ENDIF

IF RECCOUNT(lcOrdTemp) = 0
  RETURN .F.
ENDIF

SELECT (lcOrdTemp)
SET RELATION TO IIF(EMPTY(Store),"M","S")+Account+Store INTO CUSTOMER

DO lpPrnLettr  && Print Procedure.

SELECT (lcOrdTemp)
SET RELATION TO

IF USED(lcWithMe)
  USE IN (lcWithMe)
ENDIF
*-- end of lfRepoData.

*!*************************************************************
*! Name      : lpPrnLettr
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Print Report/Letter Procedure.
*!*************************************************************
*
PROCEDURE lpPrnLettr
PARAMETERS lcPrintExp
PRIVATE lnPosInTyp , lcTitle , lcPrintExp , lcRepExpr , lcLetExpr , lnCurrAls
lnCurrAls = SELECT (0)

llUpdatNow = ("PRINT"$gcDevice)  && Update master files only if you print a letter.

lcTitle = ''
*-- Assign Report/Step Step Variable
*-- Step 1 : Report
*-- Step 2 : Letter
lnRepStep = IIF("Gen" $ lcActivObj,2,IIF(lcRpPrint="L",2,1))

lnPosInTyp = ASCAN(laLettDesc,SUBSTR(lcActivObj,5,3))
IF lnPosInTyp > 0
  lcTitle = laLettDesc[lnPosInTyp]
ENDIF

STORE '' TO lcTopSalut , lcBotSalut
IF lnRepStep = 1

  llEndGroup = .F.

  IF !("Gen"$lcRpLetTyp)
    IF "Ext"$lcRpLetTyp
      =lfChngTag([Style + cOrdType + Order + STR(LineNo,6)])
    ENDIF  
  ENDIF

  llGetSalut = .F.
  lcRepTitle = lcTitle + " Report"
  WAIT WINDOW "Print " + lcRepTitle TIMEOUT 2

  *lcRepExpr = EVAL('lcRpForm') + " " + lcPrintExp
  SELECT (lnCurrAls)
  
  *DO gfDispRe WITH lcRepExpr
  DO gfDispRe WITH EVALUATE('lcRpForm')
  
  IF lcRpPrint = "B"
    lnRepStep = lnRepStep + 1
    lcRepTitle = ''
  ENDIF
ENDIF

IF lnRepStep = 2

  llDetalHed = .F.
  llEndGroup = .F.
  IF !("Gen"$lcRpLetTyp)

    IF "Ext"$lcRpLetTyp
      =lfChngTag([Account + Store + cOrdType + Order + STR(LineNo,6)])
    ENDIF  

    *-- Open Alias again and make proper relation.
    DO lpAnothAls WITH IIF("Dec"$lcRpLetTyp,lcHdrTemp,lcOrdTemp)
    SET RELATION TO Account INTO (lcWithMe) ADDITIVE

  ENDIF

  llGetSalut = .T.
  WAIT WINDOW "Print " + lcTitle + " Letter" TIMEOUT 2

  IF lcRpPrint = "B"
    lcRpForm = lcRpLetFrm
    =gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)  && Create Temp. file for new form.
    = lfRepPltFr(lcRpForm)
  ENDIF
  
  *lcLetExpr = EVAL('lcRpForm') + " " + lcPrintExp
  SELECT (lnCurrAls)
  *DO gfDispRe WITH lcLetExpr
  IF "Gen"$lcRpLetTyp
    lcRpExp = STRTRAN(lcRpExp,"CUSTOMER.","")
    DO gfDispRe WITH EVALUATE('lcRpForm'), "FOR " + lcRpExp  && Have totals filter.
  ELSE
    DO gfDispRe WITH EVALUATE('lcRpForm')
  ENDIF  

  IF lcRpPrint = "B"
    lcRpForm = lcRpRepFrm
    = lfRepPltFr(lcRpForm)
  ENDIF
  
ENDIF
WAIT CLEAR
*-- end of lpPrnLettr.

*!*************************************************************
*! Name      : lfChngTag
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Change Tag if applicable.
*!*************************************************************
*
FUNCTION lfChngTag
PARAMETERS lcToChange
PRIVATE lcToChange , lcVariable
GO TOP
lcVariable = &lcToChange
IF !SEEK(lcVariable)
  REPLACE ALL cTempKey WITH &lcToChange
ENDIF
*-- end of lfChngTag.

*!*************************************************************
*! Name      : lpAnothAls
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Open the same file in another alias.
*!*************************************************************
*
PROCEDURE lpAnothAls
PARAMETERS lcTempAlas

IF USED(lcWithMe)
  SET ORDER TO (lcTempAlas) IN (lcWithMe)

ELSE

  PRIVATE lcFullPath , lcCurDir , lnCurrAls
  lnCurrAls = SELECT(0)

  SELECT (lcTempAlas)
  lcFullPath = SET('FULLPATH')
  SET FULLPATH ON 
  lcCurDir   = FULLPATH(DBF())  && Get Cursor full path.
  SET FULLPATH &lcFullPath
  USE (lcCurDir) IN 0 AGAIN ALIAS (lcWithMe)  ORDER (lcTempAlas)  && Use the same file in another alias
  SELECT (lnCurrAls)

ENDIF
GO TOP IN (lcWithMe)
*-- end of lpAnothAls.

*!*************************************************************
*! Name      : lfOrdTemp
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Create Temp. Order Line file.
*!*************************************************************
*
FUNCTION lfOrdTemp
PRIVATE lnAllFlds

*B603748,1 BWA 07/13/2000 Comment this lines because I creat the file before the function.[START]
*-- Apply Cancellation process.
*IF USED(lcOrdCanc)
*  USE IN (lcOrdCanc)
*ENDIF

*-- if Cancellation letter.
*IF "Can" $ lcActivObj
*  CREATE CURSOR (lcOrdCanc) (Order C(6) , Open N(12) , mPikTkt M(10))
*  ZAP
*  INDEX ON Order TAG (lcOrdCanc) OF (gcWorkDir+lcOrdCanc+".CDX")
*ENDIF
*B603748,1 [END]



IF USED(lcOrdTemp)
  USE IN (lcOrdTemp)
ENDIF

*-- if you didn't declare array before.
IF TYPE("laOrdTemp[1,1]") $ "UL" OR EMPTY(laOrdTemp[1,1])
  DIMENSION laOrdTemp[1,4]
  SELECT ORDLINE
  lnAllFlds = AFIELDS(laOrdTemp)
  
  *B604402,1 AME [Start] Add new field to indicate records to be deleted
  *DIMENSION laOrdTemp[lnAllFlds+2,4]
  DIMENSION laOrdTemp[lnAllFlds+3,4]
  *B604402,1 AME [End]
  
  laOrdTemp[lnAllFlds+1,1] = "Status"
  laOrdTemp[lnAllFlds+1,2] = "C"
  laOrdTemp[lnAllFlds+1,3] = 1
  laOrdTemp[lnAllFlds+1,4] = 0

  laOrdTemp[lnAllFlds+2,1] = "cTempKey"
  laOrdTemp[lnAllFlds+2,2] = "C"
  laOrdTemp[lnAllFlds+2,3] = 32
  laOrdTemp[lnAllFlds+2,4] = 0
  
  *B604402,1 AME [Start] Add new field to indicate records to be deleted
  laOrdTemp[lnAllFlds+3,1] = "llSel"
  laOrdTemp[lnAllFlds+3,2] = "L"
  laOrdTemp[lnAllFlds+3,3] = 1
  laOrdTemp[lnAllFlds+3,4] = 0
  *B604402,1 AME [End]

ENDIF

CREATE CURSOR (lcOrdTemp) ;
   FROM ARRAY laOrdTemp

SELECT (lcOrdTemp)
ZAP

INDEX ON cOrdType + Order TAG WithHdr OF ;
    (gcWorkDir+lcOrdTemp+'.CDX') UNIQUE

INDEX ON cTempKey TAG (lcOrdTemp) OF ;
    (gcWorkDir+lcOrdTemp+'.CDX')
*-- end of lfOrdTemp.

*!*************************************************************
*! Name      : lfHdrTemp
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Create Temp. Order Header file.
*!*************************************************************
*
FUNCTION lfHdrTemp
PRIVATE lnAllFlds
IF USED(lcHdrTemp)
  USE IN (lcHdrTemp)
ENDIF

*-- if you didn't declare array before.
IF TYPE("laOrdTemp[1,1]") $ "UL" OR EMPTY(laOrdTemp[1,1])
  DIMENSION laOrdTemp[1,4]
  SELECT ORDHDR
  lnAllFlds = AFIELDS(laOrdTemp)
ENDIF

CREATE CURSOR (lcHdrTemp) ;
   FROM ARRAY laOrdTemp
SELECT (lcHdrTemp)
ZAP

INDEX ON Account + Store + cOrdType + Order TAG (lcHdrTemp) OF ;
    (gcWorkDir+lcHdrTemp+'.CDX')
*-- end of lfHdrTemp.

*!*************************************************************
*! Name      : lfItemFilt
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Item Filter.
*!*************************************************************
*
FUNCTION lfItemFilt
PARAMETERS lnCurrFilt
PRIVATE lnCurrFilt , lnNoOfCols , laTempExpr , lcFilter
lnNoOfCols = ALEN(laOGFxFlt,2)
DIMENSION laTempExpr[1,lnNoOfCols]
lnCurrFilt = (lnCurrFilt - 1) * lnNoOfCols + 1
=ACOPY(laOGFxFlt,laTempExpr,lnCurrFilt,lnNoOfCols,1)
lcFilter = gfGenFlt('laTempExpr',.T.,.T.)
RETURN lcFilter
*-- end of lfItemFilt.

*!*************************************************************
*! Name      : lfAddTo
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Create Expression.
*!*************************************************************
*
FUNCTION lfAddTo
PARAMETERS lcExpr , lcAddedExp
PRIVATE lcExpr , lcAddedExp
IF !EMPTY(lcExpr) AND !EMPTY(lcAddedExp)
  lcExpr = lcExpr + [ AND ]
ENDIF  
lcExpr = lcExpr + lcAddedExp
*-- end of lfAddTo.

*!*************************************************************
*! Name      : lfFillData
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Collect Extension/Cancellation data.
*!*************************************************************
*
FUNCTION lfFillData
PRIVATE lcStyForEx,lcOrdForEx,llStyRel
lcStyForEx = ''
lcOrdForEx = ''
llStyRel = .F.

SELECT ORDLINE
SET RELATION TO cOrdType + Order INTO Ordhdr

lcOrdForEx = lcHiddFilt
=lfAddTo(@lcOrdForEx,lcOrdHExpr)
lcOrdForEx = [FOR ] + lcOrdForEx
SET ORDER TO ORDLINES IN ORDLINE

*E500341,1 BWA 05/29/2000 Calling Inrange style color screen.[START]
=LFVSTYLE()
*E500341,1 [END]

*-- if there is no style filter.
IF EMPTY(lcStyExp)

  *-- Apply Order Criteria.
  SELECT ORDLINE
  GO TOP
  IF !EMPTY(lcStyleExp)
    lcOrdForEx = lcOrdForEx + [ AND ] + lcStyleExp
    SET RELATION TO Style INTO STYLE ADDITIVE
    llStyRel = .T.
  ENDIF  
  =lfApplySty('')
  IF llStyRel
    SET RELATION OFF INTO STYLE
  ENDIF

ELSE  && Have Style Filter.

  IF !EMPTY(lcStyleExp)
    lcStyForEx = [FOR ] + STRTRAN(lcStyleExp,"STYLE.","")
  ENDIF  
  
  *E500341,1 Add string "for" to the filter that scan on the ordline.[START]
  lcStyExp = [FOR ] + lcStyExp
  *E500341,1 [END] 
  
  SET ORDER TO Style IN Style

  *-- Scan Style In Range File.
  *B803344,1 .[START]
  *SELECT (laOGFxFlt[lnStyPos,6])
  IF EMPTY(lcStyColor)
    SELECT (laOGFxFlt[lnStyPos,6])
  ELSE
    SELECT (lcTempStyle)
    
    *B603716,1 BWA 06/29/2000 Fix the bug of alias name not found in case the user select 
    *B603716,1                all records and then select color.[START]
    *B606686,1 ABD - Fix bug of The report shows colors of styles that were not Chosen, 
    *B606686,1 ABD - we will create the index on the style field as all. [Begin] 
    *INDEX ON LEFT(Style,lnStPos) TAG (lcTmpIndx2) UNIQUE
    INDEX ON Style TAG (lcTmpIndx2)
    *B606686,1 ABD - [End]
    *B603716,1 [END]
    
  ENDIF  
  *B803344,1 [END]
  *B804205,1 AME [Start]  Remove Seek in lcTempStyle from lcStyExp
  
  *B606686,1 ABD - Fix bug of The report shows colors of styles that were not Chosen,
  *B606686,1 ABD - I will create a new file to scan on it , and didn't scan on the 
  *B606686,1 ABD - file that the expression seek into it, because it sends the pointer to EOF. [Begin] 
  lnOldAlias = SELECT (0)
  lcTempScan = gfTempName()
  lnFildLen = AFIELDS(laTempStru)
  CREATE TABLE (lcTempScan) FROM ARRAY laTempStru
  SELECT (lcTempScan)
  *-- Fix Cursor bug [Begin]
  ZAP
  SELECT(lnOldAlias)
  LOCATE
  SCAN
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTempScan) FROM MEMVAR
  ENDSCAN
  SELECT(lcTempScan)
  LOCATE

  *IF !EMPTY(lcStyColor)
  *  llSekSty = AT([SEEK(STYLE.STYLE , "]+lcTempStyle+[")],lcStyExp) > 0  && This condition to restore the expr. if we change it.
  *  IF llSekSty 
  *    lcOldSExp = lcStyExp
  *    lcStyExp  = STRTRAN(lcStyExp,[SEEK(STYLE.STYLE , "]+lcTempStyle+[")],'.T.')
  *  ENDIF
  *ENDIF
  *B606686,1 ABD - [End]
  
  *B804205,1 AME [End]
  SCAN
    *B803344,1 [START]
    *lcMajorVal = PADR(cStyMajor,lnMajorLen)
    IF EMPTY(lcStyColor)
      lcMajorVal = PADR(cStyMajor,lnMajorLen)
    ELSE
      *B606686,1 ABD - Scan an all the style in case we have color not part from the style
      *B606686,1 ABD - else the report we collect data 2 times. [Begin]
      *lcMajorVal = PADR(Style,lnMajorLen)
      lcMajorVal = Style
      *B606686,1 ABD - [End]
    ENDIF  
    *B803344,1 [END]
    
        
    IF SEEK(lcMajorVal,"STYLE")
      SELECT STYLE
      *-- Scan for Style Non Majors.
      
      *E500341,1 BWA 05/29/2000 Use the returned expression from the Inrange screen in 
      *E500341,1                scanning in the ordline.dbf.
      
      *SCAN REST WHILE Style = lcMajorVal &lcStyForEx
      SCAN REST  WHILE Style = lcMajorVal &lcStyExp
      *E500341,1 [END]

        IF SEEK(Style,"ORDLINE")
          *-- Apply Order Criteria.
          SELECT ORDLINE
          =lfApplySty(Style.Style)
        ENDIF
      ENDSCAN
      *-- end Scan for Style Non Majors.
    ENDIF
  ENDSCAN  
  *-- end Scan Style In Range File.
  
  *B804205,1 AME [start] Restore the old expr. if we had changed it.
  *B606686,1 ABD - Remark the next line and erase the file from the work directory.[Begin]
  *IF !EMPTY(lcStyColor) .AND.  llSekSty 
  *  lcStyExp = lcOldSExp 
  *ENDIF
  IF USED(lcTempScan)
    USE IN (lcTempScan)
    ERASE (gcWorkDir+lcTempScan+".DBF")
  ENDIF
  *B606686,1 ABD - [End]
  
  *B804205,1 AME [End]
ENDIF

SELECT ORDLINE
SET RELATION TO

*-- end of lfFillData.

*!*************************************************************
*! Name      : lfApplySty
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Apply Style in Ordline filter.
*!*************************************************************
*
FUNCTION lfApplySty
PARAMETERS lcApplySty

PRIVATE lcApplySty
lcProFExpr = STRTRAN(lcProFExpr,"PROFILE.","")

SCAN REST WHILE Style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
                lcApplySty &lcOrdForEx

  GO TOP IN (lcTempMast)
  WAIT WINDOW "Compiling Style : " + Style + " in Order : " + Order NOWAIT

  *-- Apply Account and Profile filters. [Begin]
  IF (EOF(lcTempMast)   OR SEEK(Account,lcTempMast)) AND ;
     (EMPTY(lcProFExpr) OR lfProFOk(Account,OrdLine.Store))

    SCATTER MEMVAR MEMO
    IF (lcRpPrint = "L") OR ("Can" $ lcActivObj)
      m.cTempKey = Account + Store + cOrdType + Order + STR(LineNo,6)
    ELSE
      m.cTempKey = Style + cOrdType + Order + STR(LineNo,6)
    ENDIF  

    m.Status = ORDHDR.Status
    *B604402,1 AME [Start] Field indicates records to be canceled in case of 
    *B604402,1             cancelation letter by default all records are to be canceled
    m.llSel = .T.
    *B604402,1 AME [End]
    INSERT INTO (lcOrdTemp) FROM MEMVAR

    *-- if Cancellation letter, Update Cancellation Table. [Begin]
    IF "Can" $ lcActivObj
      
      IF SEEK(m.Order,lcOrdCanc)
        REPLACE &lcOrdCanc..Open WITH &lcOrdCanc..Open + m.TotQty
      ELSE
        INSERT INTO (lcOrdCanc) (Order,Open) VALUES (m.Order,m.TotQty)
      ENDIF

      *-- if you have a piktkt
      IF m.Picked AND m.PikTkt <> "******"
        lcPickFld  = UPPER(ALLTRIM(&lcOrdCanc..mPikTkt))
        lnFindPick = ATC(m.PikTkt,lcPickFld)

        *-- if pick ticket not found.
        IF lnFindPick = 0
          lcPickStr = m.PikTkt+",1"
          lcPickFld = IIF(EMPTY(&lcOrdCanc..mPikTkt),lcPickStr,;
                                                     &lcOrdCanc..mPikTkt+"|"+lcPickStr)
        ELSE  && else U find the same pick ticket from another line.
          lcPickStr = SUBSTR(lcPickFld,lnFindPick,ATC("|",lcPickFld)-1)
          lcNewStr  = m.PikTkt + "," + ALLTRIM(STR(VAL(RIGHT(lcPickStr,1)) + 1))
          lcPickFld = STRTRAN(lcPickFld,lcPickStr,lcNewStr)
        ENDIF
        REPLACE &lcOrdCanc..mPikTkt WITH lcPickFld

      ENDIF  && end if you have a piktkt
      
    ENDIF  
    *-- if Cancellation letter, Update Cancellation Table. [End  ]
    
  ENDIF
  *-- Apply Account and Profile filters. [End  ]
          
ENDSCAN
WAIT CLEAR
*-- end of lfApplySty.

*!*************************************************************
*! Name      : lfProFOk
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Apply Profile filter.
*!*************************************************************
*
FUNCTION lfProFOk
PARAMETERS lcProAcc , lcStore
PRIVATE lcProAcc , lcStore , llProFOk , lnCurrAlis
lnCurrAlis = SELECT (0)
llProFOk = .F.
IF SEEK("C"+lcProAcc,"PROFILE")
  SELECT PROFILE
  SCAN REST WHILE cconttype+ccont_id+store+cpro_code+cpro_value = "C"+lcProAcc+lcStore ;
            FOR &lcProFExpr
    llProFOk = .T.
    EXIT
  ENDSCAN
ENDIF
SELECT (lnCurrAlis)
RETURN llProFOk
*-- end of lfProFOk.

*!*************************************************************
*! Name      : lfObjState
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Object State Function.
*!*************************************************************
*
FUNCTION lfObjState
PRIVATE lcSeekVal , lcArrayNam , lnInType , lnPosInAry , llPrnType

lnInType = ASCAN(laLettVal,lcRpLetTyp)
IF lnInType > 0
  lnInType   = ASUBSCRIPT(laLettVal,lnInType,1)
  lcArrayNam = "la" + LEFT(laLettDesc[lnInType,1],3) + "Val"

  lnPosInAry = ASCAN(laOGObjType,lcActivObj)
  IF lnPosInAry > 0
    lnPosInAry = ASUBSCRIPT(laOGObjType,lnPosInAry,1)

    IF ALEN(&lcArrayNam,1) = 1

      laOGObjCnt[lnPosInAry] = .F.
      &lcActivObj = &lcArrayNam[1,1]
      
      IF ALEN(&lcArrayNam,1) = 1 AND EMPTY(&lcArrayNam[1,1])
        lcRpPrint = IIF("Gen"$lcActivObj,"","R")  && if General print Nothing Else default to Report.
        llPrnType = .F.
      ELSE
        lcRpPrint = IIF("Gen"$lcActivObj,"L",lcRpPrint)  && if General print letter only Else default to Both.
        llPrnType = .T.
      ENDIF
      
    ELSE
    
      laOGObjCnt[lnPosInAry] = .T.
      IF EMPTY(&lcActivObj)
        lcSeekVal = "D"+lcLetrTo+LEFT(laLettDesc[lnInType,1],1)
        SELECT LETTERS
        SET ORDER TO Leterdefa
        &lcActivObj = IIF(SEEK(lcSeekVal),cLetterId,"")
      ENDIF  
      lcRpPrint = IIF("Gen"$lcActivObj,"L",lcRpPrint)  && if General print letter only Else default to Both.
      llPrnType = .T.

    ENDIF

    IF (lcRpPrint = "R")
      laOGObjCnt[lnPosInAry] = .F.
    ENDIF
    =lfOGShowGet(lcActivObj)  && Show get current letter object
    
    *-- Show Get Print Object.
    lnPosInAry = ASCAN(laOGObjType,"lcRpPrint")
    IF lnPosInAry > 0
      lnPosInAry = ASUBSCRIPT(laOGObjType,lnPosInAry,1)
      laOGObjCnt[lnPosInAry] = llPrnType
    ENDIF
    =lfOGShowGet("lcRpPrint")
  ENDIF  
  
ENDIF

*-- if print a report.
IF lcRpPrint = "L"
  lcRpForm = lcRpLetFrm
ELSE  && else Print letter
  lcRpForm = lcRpRepFrm
ENDIF
= lfRepPltFr(lcRpForm)

*-- Show Style detail on letter Object.
=lfEnDisObj("llRpStyDet")

*-- Update Ord. Complete date Object.
=lfEnDisObj("llRpUpComp")

*-- Cancel selectd ord. lines Object.
=lfEnDisObj("llRpCanSel")
*-- end of lfObjState.

*!*************************************************************
*! Name      : lfEnDisObj
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Account Range Function.
*!*************************************************************
*!
FUNCTION lfEnDisObj
PARAMETERS lcObjToShw
PRIVATE lcObjToShw
lnPosInAry = ASCAN(laOGObjType,lcObjToShw)
IF lnPosInAry > 0
  lnPosInAry = ASUBSCRIPT(laOGObjType,lnPosInAry,1)
  laOGObjCnt[lnPosInAry] = (lcRpPrint<>"R")
ENDIF
&lcObjToShw = IIF(lcRpPrint="R",.F.,&lcObjToShw)
=lfOGShowGet(lcObjToShw)
*-- end of lfEnDisObj.

*!*************************************************************
*! Name      : lfsrvAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Account Range Function.
*!*************************************************************
*E301119,1
FUNCTION lfsrvAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = "S"
    
    *-- if user want to default by Transaction.
    IF lcRpPrint != "R" AND EMPTY(&lcActivObj) AND RECCOUNT(lcTempMast) > 0
      *Message : Skip pre-defaulted XXX.
      *Button  : <Yes> <No>
      lcMessage  = IIF(lcLetrTo = "C","Customer","Vendor") + " Letters"
      llSkipDefa = (gfModalGen('QRM40168B40000',"Dialog",lcMessage)=1)
    ENDIF
    GO TOP IN CUSTOMER 

    SET ORDER TO CUSTOMER IN CUSTOMER
    SET ORDER TO Profile IN PROFILE
*    ON KEY LABEL CTRL+S DO lpSkipNext
    
  CASE lcParm = "R"
    
    STORE .F. TO llClrTrans , llVldSkip , llSkipDefa
*    ON KEY LABEL CTRL+F3
    
  CASE lcParm = "V"

*    IF !llVldSkip
*      ON KEY LABEL CTRL+F3 DO lpSkipNext
*    ENDIF  
     
    IF lcRpPrint="R"
      RETURN .T.
    ENDIF
   
    lcSeekVal = IIF(lcLetrTo = "C",Customer.Account,ApVendor.cVendCode)
    lcRepVar  = IIF(lcLetrTo = "C","m.Account","m.cVendCode")
    IF EMPTY(&lcActivObj)
      
      IF llVldSkip
        RETURN .T.
      ENDIF
      
      llSeekIt = SEEK(lcSeekVal,lcTempMast)
      IF !(llSeekIt AND llSkipDefa)
        
        lcArrayNam = "la" + STRTRAN(lcActivObj,"lcRp","")
        lnInSource = IIF(llSeekIt,ASCAN(&lcArrayNam,&lcTempMast..cDefaLett) - 1,0)

        lcArrayNam = STRTRAN(lcArrayNam,"Val","Lettr")
        
        DIMENSION laSourcAry[ALEN(&lcArrayNam,1)-1] , laTargtAry[1]
        =ACOPY(&lcArrayNam,laSourcAry,2,ALEN(laSourcAry),1)
        IF lnInSource <= 0
          laTargtAry = ''
        ELSE
          laTargtAry = laSourcAry[lnInSource]
        ENDIF  
  
        *Call validation screen
        *lcMoverMsg = "Assign letter to Acct# " + Customer.Account + ", <CTRL+S> to skip next validations. "
        lcMoverMsg = "Assign letter to Acct# " + Customer.Account
        = gfMover(@laSourcAry,@laTargtAry,lcMoverMsg,.T.,'lfvAddItem',.T.,.F.)
        
        *-- if no letters was selected for this Transaction do not select this record.
        IF EMPTY(laTargtAry)
          RETURN .F.
        ENDIF
        
        IF SEEK(lcSeekVal,lcTempMast)
          SELECT (lcTempMast)
          REPLACE cDefaLett WITH PADR(laTargtAry[1],10)
        ELSE
          m.cDefaLett = PADR(laTargtAry[1],10)
          &lcRepVar   = lcSeekVal
          INSERT INTO (lcTempMast) FROM MEMVAR
        ENDIF
      
      ENDIF

    ELSE

      IF SEEK(lcSeekVal,lcTempMast)
        SELECT (lcTempMast)
        REPLACE cDefaLett WITH &lcActivObj
      ELSE
        m.cDefaLett = &lcActivObj
        &lcRepVar   = lcSeekVal
        INSERT INTO (lcTempMast) FROM MEMVAR
      ENDIF
    ENDIF

ENDCASE
*-- end of lfsrvAcc.

PROCEDURE lpSkipNext
llVldSkip = .T.
*-- end of lpSkipNext

*!*************************************************************
*! Name      : lfvAddItem
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Mover Add item validation.
*!*************************************************************
*
FUNCTION lfvAddItem
PARAMETERS lnBttn
PRIVATE lnBttn
*-- if Move or Move all.
IF lnBttn <= 2
  IF !EMPTY(laTarget)
    WAIT WINDOW "You cannot assign more than one letter !"
    RETURN .F.
  ENDIF
ENDIF
*-- end of lfvAddItem.

*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfsrRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Rep. Validation.
*!*************************************************************
*
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- end of lfsrRep.

*!*************************************************************
*! Name      : lfActivObj
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Get Current Active object Variable.
*!*************************************************************
*
FUNCTION lfActivObj
PRIVATE lcActivVar , lnInType
lcActivVar = ''
lnInType = ASCAN(laLettVal,lcRpLetTyp)
IF lnInType > 0
  lnInType   = ASUBSCRIPT(laLettVal,lnInType,1)
  lcActivVar = "lcRp" + LEFT(laLettDesc[lnInType,1],3) + "Val"
ENDIF
RETURN lcActivVar
*-- end of lfActivObj.

*!*************************************************************
*! Name      : lfvProFile
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Profile Validation.
*!*************************************************************
*
FUNCTION lfvProFile
PRIVATE lcOurFiltr , lnOurFiltr
IF EMPTY(lcRpProfil) OR lcRpProfil <> laOGFxFlt[lnProfile,6]
  laOGFxFlt[lnProfVal,6] = ""
ENDIF
lcRpProfil = laOGFxFlt[lnProfile,6]

SET ORDER TO Profile IN ARPRFCOD
lcOurFiltr = 'laOGFxFlt[' + ALLTRIM(STR(lnProfVal)) + ',6]'
lnOurFiltr =  ASCAN(laOGObjType,lcOurFiltr)
IF lnOurFiltr > 0
  lnOurFiltr = ASUBSCRIPT(laOGObjType,lnOurFiltr,1)
  laOGObjCnt[lnOurFiltr] = SEEK(lcRpProfil,"ARPRFCOD")
  = lfOGShowGet(lcOurFiltr)  && Show get Object .
ENDIF
*-- end of lfvProFile.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Postion in fixed filter array.
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.


*!*************************************************************
*! Name      : lfCompInfo
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Company Information.
*!*************************************************************
*
FUNCTION lfCompInfo
DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laDivLName[1,2]
laCompAdd = ''                     && Array to hold the Company address
laSoldTo  = ''                     && Array to hold the Sold To address

laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

*-- Get company Address [begin].
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompPhon = TRANSFORM(cCom_Phon,lcPhonPict)
lcCompFax  = TRANSFORM(cCom_Fax,lcPhonPict)
*-- Load Company address.
=gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)
= lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.

llHaveLogo = SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')

lcOldCent = SET("CENTURY")
SET CENTURY ON
lcDetOnLet = lcStyMajor + " Detail on Letter"
SELECT SALUTES
SET ORDER TO Saldefa
*-- Opening salutation Default
*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
IF IIF(lcLetrTo = "V",SEEK("DTV"),SEEK("DTC"))
*IF SEEK("DT")
*B607389,1 ALB Fix Bugs Relate to E302174 [END]
  lcTopDefa = cSalDesc
ELSE
  *E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
  *lcTopDefa = "Dear Customer :"
  lcTopDefa = IIF(lcLetrTo = "V","Dear Vendor :","Dear Customer :")
  *E302174,1 ALB Add Vendor Letter report to AP Modual [end]
ENDIF

*-- Concluding salutation Default
*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
IF IIF(lcLetrTo = "V",SEEK("DBV"),SEEK("DBC"))
*IF SEEK("DB")
*B607389,1 ALB Fix Bugs Relate to E302174 [END]
  lcBotDefa = cSalDesc
ELSE
  lcBotDefa = "Sincerely,"
ENDIF
SET ORDER TO Saluteid
*-- end of lfCompInfo.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Shift an address array.
*!*************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
PRIVATE lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam,1)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam,1)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfDefaText
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Bottom text default value.
*!*************************************************************
*
FUNCTION lfDefaText
RETURN IIF("Gen"$lcRpLetTyp,lcCompName,"")
*-- end of lfDefaText.

*!*************************************************************
*! Name      : lfDefaType
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter Type default value.
*!*************************************************************
*
FUNCTION lfDefaType
RETURN IIF(TYPE("laLettVal[1,1]")$"UL","",laLettVal[1,1])
*-- end of lfDefaType.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Object Old Value.
*!*************************************************************
*
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvNewDate
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : New date validation.
*!*************************************************************
*
FUNCTION lfvNewDate
PARAMETERS llGoOnly
PRIVATE llGoOnly

IF !llGoOnly AND MDOWN()
  RETURN
ENDIF

IF EMPTY(ldRpComplt)
  =gfModalGen('QRM04074B00000','DIALOG',"New complete date")
  IF !llGoOnly
    ldRpComplt = lcOldVal
    SHOW GET ldRpComplt
  ENDIF  
  _CUROBJ = OBJNUM(ldRpComplt)
ENDIF  
*-- end of lfvNewDate.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Clear report function.
*!*************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .T.
SET CENTURY &lcOldCent

*-- Called by Rest Button or user want default setting
IF (UPPER(SYS(18)) != "LNOGSETING") OR (lnOGSeting = 1)
  SET PROCEDURE TO &lcOldProc
ENDIF  

IF USED("STYDYE")
  USE IN STYDYE
ENDIF  

IF USED("PIKTKT")
  USE IN PIKTKT
ENDIF  
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Detect end of a group function.
*!*************************************************************
*
FUNCTION lfEndGroup
llEndGroup = .T.
RETURN ''
*-- end of lfEndGroup.

*!*************************************************************
*! Name      : lfHdrFootS
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Print Header in Letter Form Called from Memo field Printing
*!*************************************************************
*
FUNCTION lfHdrFootS
llEndGroup = .F.
IF llRpStyDet
  llDetalHed = .T.
ENDIF  
RETURN ''
*-- end of lfHdrFoot.

*!*************************************************************
*! Name      : lfHdrFootR
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Reset Print Header in Letter Form Called from Memo field Printing
*!*************************************************************
*
FUNCTION lfHdrFootR
llDetalHed = .F.
RETURN ""
*-- end of lfHdrFoot.

*!*************************************************************
*! Name      : lfFillResn
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Fill reason arrays.
*!*************************************************************
*
FUNCTION lfFillResn
PRIVATE lnI
DIMENSION laReason[1,1]
SELECT cCode_No+ " - " + ALLTRIM(cDiscrep) ;
 FROM Codes;
 WHERE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = "NCCANCRESON";
 ORDER BY Codes.ccode_no ;
 INTO ARRAY laReason

DIMENSION laReasonVl[ALEN(laReason,1),1]
IF EMPTY(laReason[1,1])
  laReason[1,1] = "N/A"
  laReasonVl[1,1] = "N/A"
ELSE
  lnI = 0
  FOR lnI = 1 TO ALEN(laReason,1)
    laReasonVl[lnI,1] = PADR(laReason[lnI,1],6)
  ENDFOR
ENDIF  
*-- end of lfFillResn.

*!*************************************************************
*! Name      : lfDefaResn
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Default Reason.
*!*************************************************************
*
FUNCTION lfDefaResn
PRIVATE lcDefaVal
SET ORDER TO Ccode_no IN CODES
IF SEEK("DCCANCRESON","CODES")
  lcDefaVal = Codes.cCode_No
ELSE
  lcDefaVal = laReasonVl[1]
ENDIF
RETURN lcDefaVal
*-- end of lfDefaResn.

*!*************************************************************
*! Name      : lfvCancOrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Enable / Disable cancellation reason.
*!*************************************************************
*
FUNCTION lfvCancOrd
PRIVATE lnPosInAry
lnPosInAry = ASCAN(laOGObjType,"lcRpCanRsn")
IF lnPosInAry > 0
  lnPosInAry = ASUBSCRIPT(laOGObjType,lnPosInAry,1)
  laOGObjCnt[lnPosInAry] = llRpCanSel
ENDIF
=lfOGShowGet("lcRpCanRsn")
*-- end of lfvCancOrd.


*!*************************************************************
*! Name      : lfCancLine
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Cancellation function.
*!*************************************************************
*
FUNCTION lfCancLine

*B604402,1 AME [Start]

*PRIVATE lnCancQty , lnCancAmt
PRIVATE lnCancQty , lnCancAmt ,lnCounter , lcOrdNum , llCancel


*Don't open CutPick unless PO or MF or PS installed.
*fag to indicate whether we can use cutpick or not
llOpenCut = ('PO' $ gcCmpModules OR 'MF' $ gcCmpModules OR 'PS' $ gcCmpModules)

*=lfNewCanc()
*RETURN

DECLARE laSetups[1,2] 
laSetups[1,1]  = 'M_CANAFTER'       && Number of deplation days ((days needed to consider cancelation - in So module setup -))
=gfGetMemVar(@laSetups,gcAct_Comp)

*------Get the previlige on the cut ticket screen
llUsrCPrv = gfUserPriv('MF','MFCUTKT')

STORE .F. TO llSndLnToC,llSndOrToC,llUpdOrdCa

*DO lpAnothAls
DO lpAnothAls WITH lcOrdTemp

*B604402,1 AME [End]



SET ORDER TO ORDLINE IN ORDLINE
SET ORDER TO ORDHDR IN ORDHDR
SET ORDER TO STYLE IN STYLE

IF !USED("STYDYE")
  =gfOpenFile(gcDataDir+"STYDYE","STYDYE","SH")
ENDIF  

IF !USED("PIKTKT")
  =gfOpenFile(gcDataDir+"PIKTKT","Ordpik","SH")
ENDIF  

*B604402,1 AME [Start]
IF llOpenCut
  = gfOpenFile(gcDataDir + 'CutPick' ,'CutPick','SH')
ENDIF
*B604402,1 AME [End]


*B604402,1 AME [Start]
STORE .F. TO llOkCancel , llUpdPoCt

*B604402,4 AME [Start] add variable to check piked message
*STORE .F. TO llAlocMsg  , llAcordMsg 
STORE .F. TO llAlocMsg  , llAcordMsg  , llPikMsg
*B604402,4 AME [End]
lnCounter = 0
lcOrdNum = ''
*B604402,1 AME [End]

SELECT (lcOrdTemp)
SET ORDER TO (lcOrdTemp)
*B604402,1 AME [Start]
=lfChkAlloc()
LOCATE
*-- Scan Temp. File to update Master files.
SCAN FOR llSel
  = SEEK(&lcOrdTemp..cOrdType+&lcOrdTemp..Order+STR(&lcOrdTemp..LineNo,6),"ORDLINE")
  = SEEK(&lcOrdTemp..cOrdType+&lcOrdTemp..Order,"ORDHDR")
  IF lcOrdNum  <> ORDER
    lnCounter = 0
  ENDIF
  lcOrdNum  = ORDER
  IF lnCounter = 0  
    lnRecno = RECNO()
    SUM &lcOrdTemp..TOTQTY  TO lnTCan FOR &lcOrdTemp..ORDER = lcOrdNum  .AND. llSel
    IF BETWEEN(lnRecno,1,RECCOUNT())
      GO lnRecno
    ENDIF
    *-Flag to Update OrdCanLn when removing line (or more but not all order)
    llSndLnToC = ORDHDR.STATUS <> 'B' AND (gdSysDate > ORDHDR.ENTERED + laSetups[1,2])
    *-Flag to Update OrdCanLn when removing the whole  order
    llSndOrToC = (ORDHDR.Open = lnTCan) AND ORDHDR.SHIP > 0
    *-Main flag
    llUpdOrdCa = llSndOrToC OR (llSndLnToC AND ORDHDR.Open <> lnTCan)
    *--Check the cancelled quantity < order Quantity
    IF lnTCan >= ORDHDR.OPEN AND ORDHDR.SHIP <= 0
      llCancel = .F.
    ELSE
      llCancel = .T.
    ENDIF
  ENDIF 
  lnCounter = lnCounter + 1
  
  *???? uptill now the next lines are useless
  *SELECT (lcWithMe)
  *COUNT TO lnSelected for ORDER = lcOrdNum 
  *SELECT ORDLINE
  *COUNT TO lnRecCount FOR ORDLINE.ORDER = lcOrdNum
  *SELECT (lcOrdTemp)
  * ?? end of useless lines

  
  SELECT (lcOrdTemp)
  SCATTER MEMVAR MEMO 		&&to update files from memo
  
  
  
  *---- Step No 1  release So allocation
  IF llOpenCut
    SELECT CUTPICK
    *-- Release SO Allocation
    lcCutPkOrd = ORDER()
    SET ORDER TO TAG CUTORD
    IF SEEK('1' + M.ORDER ,'CutPick') .OR. SEEK('2' + M.ORDER ,'CutPick')
      = lfRelAllOC()
    ENDIF
    SELECT CUTPICK
    SET ORDER TO TAG (lcCutPkOrd)
  ENDIF
  lnCanc = M.QTY1 + M.QTY2 + M.QTY3 + M.QTY4 + M.QTY5 + M.QTY6 + M.QTY7 + M.QTY8    
    
  *---- Step No 2  make proper changes in OrdHdr Qty
  SELECT ORDHDR
  = SEEK(&lcOrdTemp..cOrdType+&lcOrdTemp..Order)
  REPLACE OPEN      WITH (OPEN     - lnCanc)           ;
          CANCEL    WITH CANCEL    + (M.TotQty)        ;
          CANCELAMT WITH CANCELAMT + (lnCanc * M.PRICE);
          OPENAMT   WITH OPENAMT   - (lnCanc * M.PRICE);
          FOR ORDER = M.ORDER
  


  *---- Step No 3  add line in ordcancel If need and make changes to ord qty in style and stydye
  IF SEEK(&lcOrdTemp..cOrdType+&lcOrdTemp..Order+STR(&lcOrdTemp..LineNo,6),"ORDLINE") ;
         .AND. ORDLINE.TOTQTY > 0 .AND. llUpdOrdCa
    IF !SEEK(&lcOrdTemp..cOrdType+&lcOrdTemp..Order+STR(&lcOrdTemp..LineNo,6),"ORDCANLN")
      SELECT  ORDCANLN
      APPEND  BLANK
      REPLACE CORDTYPE  WITH M.CordType                        ;
              ORDER     WITH M.ORDER                           ;
              LINENO    WITH M.LINENO                          ;
              QTY1      WITH M.QTY1                            ;
              QTY2      WITH M.QTY2                            ;
              QTY3      WITH M.QTY3                            ;
              QTY4      WITH M.QTY4                            ;
              QTY5      WITH M.QTY5                            ;
              QTY6      WITH M.QTY6                            ;
              QTY7      WITH M.QTY7                            ;
              QTY8      WITH M.QTY8                            ;
              TOTQTY    WITH M.TOTQTY                          ;
              CANCELLED WITH gdSysDate                         ;
              CCANCRESON WITH lcRpCanRsn                       ;
              STYLE     WITH m.Style                          ,;
              Account   WITH m.Account                        ,;
              Store     WITH m.Store                          ,;
              Dyelot    WITH m.Dyelot                         ,;
              Price     WITH m.Price
      = gfAdd_Info()
    ENDIF  
  ENDIF  
  
  
  SELECT STYLE
  SET ORDER TO TAG STYLE
  SEEK &lcOrdTemp..STYLE
  *--Start Updatin Style File
  REPLACE ORD1   WITH ORD1   - M.QTY1   ;
          ORD2   WITH ORD2   - M.QTY2   ;
          ORD3   WITH ORD3   - M.QTY3   ;
          ORD4   WITH ORD4   - M.QTY4   ;
          ORD5   WITH ORD5   - M.QTY5   ;
          ORD6   WITH ORD6   - M.QTY6   ;
          ORD7   WITH ORD7   - M.QTY7   ;
          ORD8   WITH ORD8   - M.QTY8   ;
          TOTORD WITH TOTORD - M.TOTQTY
  = gfAdd_Info()        
  
  SELECT STYDYE
  SET ORDER TO TAG STYDYE
  
  *B804386,1 Update the right warehouse. [Begin]
  *SEEK &lcOrdTemp..STYLE
  SEEK &lcOrdTemp..Style + &lcOrdTemp..cWareCode
  *B804386,1 Update the right warehouse. [End]

  *--Start Updating StyDye File
  REPLACE ORD1   WITH ORD1   - M.QTY1   ;
          ORD2   WITH ORD2   - M.QTY2   ;
          ORD3   WITH ORD3   - M.QTY3   ;
          ORD4   WITH ORD4   - M.QTY4   ;
          ORD5   WITH ORD5   - M.QTY5   ;
          ORD6   WITH ORD6   - M.QTY6   ;
          ORD7   WITH ORD7   - M.QTY7   ;
          ORD8   WITH ORD8   - M.QTY8   ;
          TOTORD WITH TOTORD - M.TOTQTY
  = gfAdd_Info()
  
  *---- Step No 4  make proper changes in OrdLine Qty in case of not cancelling the whole order
  IF llCancel
    SELECT ORDLINE
    = SEEK(&lcOrdTemp..cOrdType+&lcOrdTemp..Order+STR(&lcOrdTemp..LineNo,6))
    REPLACE QTY1      WITH QTY1 - M.QTY1 ;
            QTY2      WITH QTY2 - M.QTY2 ;
            QTY3      WITH QTY3 - M.QTY3 ;
            QTY4      WITH QTY4 - M.QTY4 ;
            QTY5      WITH QTY5 - M.QTY5 ;
            QTY6      WITH QTY6 - M.QTY6 ;
            QTY7      WITH QTY7 - M.QTY7 ;
            QTY8      WITH QTY8 - M.QTY8 ;
            TOTQTY    WITH TOTQTY - M.TOTQTY ;
    FOR ORDER = M.ORDER .AND. LINENO = M.LINENO
  ENDIF
  
  *---- Step No 5  make proper changes in OrdHdr status
  SELECT ORDHDR
  = SEEK(&lcOrdTemp..cOrdType+&lcOrdTemp..Order)
  IF BOOK - (CANCEL + Ship) = 0
    REPLACE STATUS WITH IIF(SHIP > 0 , 'C' , 'X')
    IF SHIP <= 0 
      REPLACE cCancReson WITH  lcRpCanRsn
    ENDIF
  ENDIF
ENDSCAN
*B604402,1 AME [End]

*B604402,1 AME [Start] remove the old scan loop
*SCAN

  *IF SEEK(cOrdType+Order,"ORDHDR")
    *STORE 0 TO lnCancQty , lnCancAmt
    *IF SEEK(cOrdType+Order,"ORDLINE")
      *SELECT ORDLINE
      *SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = OrdHdr.cOrdType + OrdHdr.Order ;
                *FOR   SEEK(Account + Store + cOrdType + Order + STR(LineNo,6),lcWithMe)    

        *lnCancQty = lnCancQty + TotQty
        *lnCancAmt = lnCancAmt + TotQty * Price
        
        *-- 1- Update StyDye Table <Dyelot Record>.
        *= !EMPTY(Dyelot) AND SEEK(Style+cWareCode+Dyelot,"STYDYE") AND lfCancQty("STYDYE")

        *-- 2- Update StyDye Table <Location Record>.
        *= SEEK(Style+cWareCode+SPACE(10),"STYDYE") AND lfCancQty("STYDYE")

        *-- 3- Update Style Table.
        *= SEEK(Style,"STYLE") AND lfCancQty("STYLE")

        *-- 4- Update PikTkt Table
        *IF Picked AND PikTkt <> "******" AND SEEK(Order + PikTkt , "PIKTKT")
          *SELECT PIKTKT
          *=RLOCK()
          *BLANK
          *DELETE
          *UNLOCK
        *ENDIF

        *-- 5- Update OrdLine While Updating.
        *=RLOCK()
        *REPLACE Qty1 WITH 0,;
                Qty2 WITH 0,;
                Qty3 WITH 0,;
                Qty4 WITH 0,;
                Qty5 WITH 0,;
                Qty6 WITH 0,;
                Qty7 WITH 0,;
                Qty8 WITH 0,;
              TotQty WITH 0
        
        *IF Picked 
          *REPLACE PIK1 WITH 0,;
                  PIK2 WITH 0,;
                  PIK3 WITH 0,;
                  PIK4 WITH 0,;
                  PIK5 WITH 0,;
                  PIK6 WITH 0,;
                  PIK7 WITH 0,;
                  PIK8 WITH 0,;
               TOTPIK  WITH 0,;
               PICKED  WITH .F.,;
               PIKTKT  WITH SPACE(6),;
               PIKDATE WITH {}
        *ENDIF          
        *UNLOCK
        *= gfAdd_Info()
        
      *ENDSCAN
      
    *ENDIF  

    *-- 6- Update OrdHdr Table.
    *SELECT ORDHDR
    *SCATTER MEMVAR
    *m.Open      = MAX(m.Open - lnCancQty,0)
    *m.OpenAmt   = MAX(m.OpenAmt - lnCancAmt,0)
    *m.Cancel    = m.Cancel + lnCancQty
    *m.CancelAmt = m.CancelAmt + lnCancAmt
    *m.Flag      = SPACE(1)
    
    *IF m.Open = 0
      *IF m.Ship = 0
        *m.Status = "X"
        *m.Cancelled  = gdSysDate
        *m.cCancReson = lcRpCanRsn
      *ELSE
        *m.Status = "C"
      *ENDIF
    *ENDIF
    *=RLOCK()
    *GATHER MEMVAR
    *UNLOCK
    *= gfAdd_Info()
  *ENDIF
*ENDSCAN

*-- Restore Flags
STORE .F. TO llOkCancel , llUpdPoCt

*B604402,4 AME [Start]
*STORE .F. TO llAlocMsg  , llAcordMsg
STORE .F. TO llAlocMsg  , llAcordMsg , llPikMsg
*B604402,4 AME [End]

*B604402,1 AME [End]

IF USED(lcWithMe)
  USE IN (lcWithMe)
ENDIF

SELECT (lcOrdTemp)
ZAP
*-- end of lfCancLine.

*!*************************************************************
*! Name      : lfCancQty
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Update Cancelled quantity (Style/StyDye)
*!*************************************************************
*
FUNCTION lfCancQty
PARAMETERS lcPassAlis
PRIVATE lcPassAlis , lnCurrAls
lnCurrAls = SELECT (0)

SELECT (lcPassAlis)
=RLOCK()
REPLACE Ord1 WITH MAX(Ord1 - OrdLine.Qty1,0) ,;
        Ord2 WITH MAX(Ord2 - OrdLine.Qty2,0) ,;
        Ord3 WITH MAX(Ord3 - OrdLine.Qty3,0) ,;
        Ord4 WITH MAX(Ord4 - OrdLine.Qty4,0) ,;
        Ord5 WITH MAX(Ord5 - OrdLine.Qty5,0) ,;
        Ord6 WITH MAX(Ord6 - OrdLine.Qty6,0) ,;
        Ord7 WITH MAX(Ord7 - OrdLine.Qty7,0) ,;
        Ord8 WITH MAX(Ord8 - OrdLine.Qty8,0) ,;
      TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8

IF OrdLine.Picked
  REPLACE Alo1 WITH MAX(Alo1 - OrdLine.Pik1,0) ,;
          Alo2 WITH MAX(Alo2 - OrdLine.Pik2,0) ,;
          Alo3 WITH MAX(Alo3 - OrdLine.Pik3,0) ,;
          Alo4 WITH MAX(Alo4 - OrdLine.Pik4,0) ,;
          Alo5 WITH MAX(Alo5 - OrdLine.Pik5,0) ,;
          Alo6 WITH MAX(Alo6 - OrdLine.Pik6,0) ,;
          Alo7 WITH MAX(Alo7 - OrdLine.Pik7,0) ,;
          Alo8 WITH MAX(Alo8 - OrdLine.Pik8,0) ,;
        TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6 + Alo7 + Alo8

ENDIF
UNLOCK
= gfAdd_Info()

SELECT (lnCurrAls)
*-- end of lfCancQty.

*!*************************************************************
*! Name      : lfCodes
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Get Account codes
*!*************************************************************
*
FUNCTION lfCodes
laCodes[1,1] = DECL_CODE
laCodes[2,1] = CTERMCODE
=gfCodDes(@laCodes)
RETURN ''
*-- end of lfCodes.

*!*************************************************************
*! Name      : lfEntryDef
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Return Default Letter.
*!*************************************************************
*
FUNCTION lfEntryDef
PARAMETERS lcLetSymbl
PRIVATE lcLetSymbl
SELECT LETTERS
SET ORDER TO Leterdefa
RETURN IIF(SEEK("D"+lcLetrTo+lcLetSymbl),cLetterId,"")
*-- end of lfEntryDef.

*!*************************************************************
*! Name      : lfvAcctFlt
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Account Filter.
*!*************************************************************
* Either Direct Call to gfRange Function or To Select Letter Screen.
*
FUNCTION lfvAcctFlt
PRIVATE lcReplace
lcReplace = ''

*-- if user want to print Report, or print Letter or Both for specific Letter.
IF (lcRpPrint = "R") OR !EMPTY(&lcActivObj)
  PRIVATE llPressOk
  llPressOk = .F.

  *-- gfRange Calling
  IF lcLetrTo = "C"
    lcBrowFlds = [account   :H= 'Acct#' :10,]    + ;
                 [btName    :H= 'Name' :35 ,]    + ;
                 [Phone1    :H = 'Phone' :16,]   + ;
                 [cAddress6 :H = 'Country' :30,] + ;
                 [netBal  :H = 'Balance' ]       
    SELECT CUSTOMER
    =SEEK("M")
    llPressOk = gfrange(lcBrowFlds,lcTempMast,"Account",['M'],"","","@! XXXXX")

  ELSE
    *E302174,1 ALB Add Vendor Letter report to AP Modual [Begin]
    lcBrowFlds = [cvendcode   :H=  'Ven#'  :10,]   + ;
                 [CVENCOMP    :H=  'Name' :35 ,]   + ;
                 [CPHONENO    :H = 'Phone' :16,]   + ;
                 [CCONT_CODE  :H = 'Country' :30,] + ;
                 [NVENBAL     :H = 'Balance' ]       
    SELECT APVENDOR
    llPressOk = gfrange(lcBrowFlds,lcTempMast,"cvendcode",[''],"","","@! XXXXX")
    *E302174,1 ALB Add Vendor Letter report to AP Modual [end]
  ENDIF                 

  lcReplace = [cDefaLett WITH &lcActivObj , cOldLetter WITH &lcActivObj]
  
ELSE && else User want to print Letter or Both and select a letter Per Account.
  *-- Custom Select Letter per Account screen.
  IF lcLetrTo = "C"
    llPressOk = lfvCustLtr()

  ELSE
    llPressOk = lfvVendLtr()
  ENDIF  
  lcReplace = [cOldLetter WITH cDefaLett]

ENDIF

*-- Save Selection.
SELECT (lcTempMast)
*-- if user press Ok Button.
IF llPressOk
  REPLACE ALL &lcReplace
  llOGFltCh = .T.
ELSE  && else user press Cancel Button
  REPLACE ALL cDefaLett WITH cOldLetter
  DELETE FOR EMPTY(cDefaLett)
ENDIF
*-- end of lfvAcctFlt.

*!*************************************************************
*! Name      : lfvCustLtr
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Custom select Letters Screen.
*!*************************************************************
*
FUNCTION lfvCustLtr
PRIVATE lcLetrType , lnActvAls , lcLettOrd
lnActvAls = SELECT(0)
lcLettOrd = ORDER("LETTERS")

lcLetrType = SUBSTR(lcActivObj,5,1)

SELECT CUSTOMER 
SET RELATION TO Account INTO (lcTempMast) ADDITIVE
=SEEK("M")
IF EOF()
  STORE "DISABLE" TO lcSelNonSt,lcInvertSt,lcSelSt,lcSelAllSt
ELSE
  STORE "ENABLE" TO lcSelNonSt,lcInvertSt,lcSelSt,lcSelAllSt
ENDIF  

SELECT LETTERS
INDEX ON cleterto+cletertype+cletterid TAG (lcLetrTag) OF (gcWorkDir+lcLetrTag+".CDX")

DO CASE
  CASE lcLetrType = "E"
    lcLetrBrow = "Extension Letters"
  CASE lcLetrType = "C"
    lcLetrBrow = "Cancellation Letters"
  CASE lcLetrType = "D"
    lcLetrBrow = "Decline Letters"
  CASE lcLetrType = "G"
    lcLetrBrow = "General purpose Letters"
ENDCASE
lcWhatLetr = "Assign " + lcLetrBrow

llOkPress = .F.

DIMENSION laScrMode[4]
laScrMode[2] = .T.
lnLtrBrRec   = 0
lnCusBrRec   = 0

lcLetrBase = gfTempName()
lcChLet1   = gfTempName()
lcChLet2   = gfTempName()
lcChLet3   = gfTempName()
lcChLet4   = gfTempName()
lcCustBrow = "Customers"

PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcLetrBrow)

*-- Customer Range window
ON KEY LABEL ALT+C ACTIVATE WINDOW (lcCustBrow)
ON KEY LABEL Alt+L DO lfvAccSel WITH "SEL_UNSEL"
ON KEY LABEL Alt+A DO lfvAccSel WITH "SEL_ALL"
ON KEY LABEL Alt+N DO lfvAccSel WITH "SEL_NON"
ON KEY LABEL Alt+I DO lfvAccSel WITH "INVERT"
ON KEY LABEL Alt+A DO lfvClose              && 	Cancel Button
ON KEY LABEL ESC DO lfvClose                && 	Cancel Button
ON KEY LABEL Alt+O DO lfvClose WITH .T.     && Ok Button

DO (gcRepHome+"ARDFLTR.SPX")

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcLetrBrow)
RELEASE WINDOW (lcCustBrow)

SELECT LETTERS
CLOSE INDEXES
ERASE (gcWorkDir+lcLetrTag+".CDX")

SELECT CUSTOMER 
SET RELATION TO

SET ORDER TO &lcLettOrd IN LETTERS
SELECT (lnActvAls)

RETURN llOkPress
*-- end of Main Function Code.
*-- end of lfvCustLtr.

*!*************************************************************
*! Name      : lfvVendLtr
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Custom select Letters Screen.
*!*************************************************************
*
FUNCTION lfvVendLtr
PRIVATE lcLetrType , lnActvAls , lcLettOrd
lnActvAls = SELECT(0)
lcLettOrd = ORDER("LETTERS")

lcLetrType = SUBSTR(lcActivObj,5,1)

SELECT APVendor
SET RELATION TO cVendCode INTO (lcTempMast) ADDITIVE
GO TOP
IF EOF()
  STORE "DISABLE" TO lcSelNonSt,lcInvertSt,lcSelSt,lcSelAllSt
ELSE
  STORE "ENABLE" TO lcSelNonSt,lcInvertSt,lcSelSt,lcSelAllSt
ENDIF  

SELECT LETTERS
INDEX ON cleterto+cletertype+cletterid TAG (lcLetrTag) OF (gcWorkDir+lcLetrTag+".CDX")

DO CASE
  CASE lcLetrType = "E"
    lcLetrBrow = "Extension Letters"
  CASE lcLetrType = "C"
    lcLetrBrow = "Cancellation Letters"
  CASE lcLetrType = "D"
    lcLetrBrow = "Decline Letters"
  CASE lcLetrType = "G"
    lcLetrBrow = "General purpose Letters"
ENDCASE
lcWhatLetr = "Assign " + lcLetrBrow

llOkPress = .F.

DIMENSION laScrMode[4]
laScrMode[2] = .T.
lnLtrBrRec   = 0
lnCusBrRec   = 0

lcLetrBase = gfTempName()
lcChLet1   = gfTempName()
lcChLet2   = gfTempName()
lcChLet3   = gfTempName()
lcChLet4   = gfTempName()
lcCustBrow = "Vendors"

PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcLetrBrow)

*-- Customer Range window
ON KEY LABEL ALT+C ACTIVATE WINDOW (lcCustBrow)
ON KEY LABEL Alt+L DO lfvAccSel WITH "SEL_UNSEL"
ON KEY LABEL Alt+A DO lfvAccSel WITH "SEL_ALL"
ON KEY LABEL Alt+N DO lfvAccSel WITH "SEL_NON"
ON KEY LABEL Alt+I DO lfvAccSel WITH "INVERT"
ON KEY LABEL Alt+A DO lfvClose              && 	Cancel Button
ON KEY LABEL ESC DO lfvClose                && 	Cancel Button
ON KEY LABEL Alt+O DO lfvClose WITH .T.     && Ok Button

DO (gcRepHome+"ARDFLTR.SPX")

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcLetrBrow)
RELEASE WINDOW (lcCustBrow)

SELECT LETTERS
CLOSE INDEXES
ERASE (gcWorkDir+lcLetrTag+".CDX")

SELECT APVENDOR
SET RELATION TO

SET ORDER TO &lcLettOrd IN LETTERS
SELECT (lnActvAls)

RETURN llOkPress
*-- end of Main Function Code.
*-- end of lfvCustLtr.

*!*************************************************************
*! Name      : lfLetrBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter Browse Fn.
*!*************************************************************
*
FUNCTION lfLetrBrow
*-- Release browse window if found.
IF WEXIST(lcLetrBrow)
  RELEASE WINDOW (lcLetrBrow)
ENDIF

SELECT LETTERS
lnLtrBrRec  = RECNO()

lcBrowFild = "cMarker=IIF(RECNO()=lnLtrBrRec , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "CLETTERID :R :H= 'Letter ID'   :13  ,"+;
             "cletshdes :R :H= 'Description' :40"

BROWSE KEY lcLetrTo+lcLetrType;
       FIELDS &lcBrowFild     ;
       WINDOW (lcChLet1)      ;
       WHEN lfwLtrBrow()      ;
       VALID :F lfvLtrBrow()  ;
       IN WINDOW (lcLetrBase) ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcLetrBrow
*-- end of lfLetrBrow.

*!*************************************************************
*! Name      : lfwLtrBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter Browse When Fn.
*!*************************************************************
*
FUNCTION lfwLtrBrow
lnLtrBrRec  = RECNO("LETTERS")
=lfLetrShow()
*-- end of lfwLtrBrow.

*!*************************************************************
*! Name      : lfLetrShow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter Browse Show Fn.
*!*************************************************************
*
FUNCTION lfLetrShow
SHOW WINDOW (lcLetrBrow) REFRESH
SHOW GET letters.mletrbody 
*-- end of lfLetrShow.

*!*************************************************************
*! Name      : lfvLtrBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter Browse Valid Fn.
*!*************************************************************
*
FUNCTION lfvLtrBrow
IF !WONTOP(lcLetrBrow)
  =gfStopBrow()
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcLetrBrow)
ENDIF
*-- end of lfvLtrBrow.


***********************************************************

*!*************************************************************
*! Name      : lfCustBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Customer Browse Fn.
*!*************************************************************
*
FUNCTION lfCustBrow
*-- Release browse window if found.
IF WEXIST(lcCustBrow)
  RELEASE WINDOW (lcCustBrow)
ENDIF

*B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
IF lcLetrTo = "C"
  SELECT CUSTOMER
ELSE
  SELECT APVENDOR
ENDIF
lnCusBrRec  = RECNO()
IF lcLetrTo = "C"
  lcCustFild = [cMarker=IIF(RECNO()=lnCusBrRec , '>' , ' ') :1 :H= ' ' :W= .F. ,]+;
               [cDefa = &lcTempMast..cDefaLett :R :H="Assigned Letter " ,]+;
               [ACCOUNT :R :H= 'Acct #' , BTNAME :R :H= 'Name' ,]+;
               [PHONE1 :R :H= 'Phone' , CADDRESS6 :R :H= 'Country' , NETBAL :R :H= 'Balance'] 
ELSE
  lcCustFild = [cMarker=IIF(RECNO()=lnCusBrRec , '>' , ' ') :1 :H= ' ' :W= .F. ,]+;
               [cDefa = &lcTempMast..cDefaLett :R :H="Assigned Letter " ,]+;
               [CVENDCODE :R :H= 'VENDOR #' , CVENCOMP :R :H= 'Name' ,]+;
               [CPHONENO :R :H= 'Phone' , CADDRESS6 :R :H= 'Country' , NVENBAL :R :H= 'Balance'] 

ENDIF

IF lcLetrTo = "C"
  BROWSE KEY "M" FIELDS &lcCustFild ;
         WINDOW (lcChLet3)      ;
         WHEN lfwCusBrow()      ;
         VALID :F lfvCusBrow()  ;
         IN WINDOW (lcLetrBase) ;
         LOCK 0                 ;
         NOAPPEND               ;
         NOCLEAR                ;
         NODELETE               ;
         NOWAIT                 ;
         NOEDIT                 ;
         NOMENU                 ;
         SAVE                   ;
         TITLE lcCustBrow
ELSE
  BROWSE FIELDS &lcCustFild     ;
         WINDOW (lcChLet3)      ;
         WHEN lfwCusBrow()      ;
         VALID :F lfvCusBrow()  ;
         IN WINDOW (lcLetrBase) ;
         LOCK 0                 ;
         NOAPPEND               ;
         NOCLEAR                ;
         NODELETE               ;
         NOWAIT                 ;
         NOEDIT                 ;
         NOMENU                 ;
         SAVE                   ;
         TITLE lcCustBrow

ENDIF
*B607389,1 ALB Fix Bugs Relate to E302174 [END]
*-- end of lfCustBrow.

*!*************************************************************
*! Name      : lfwCusBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Customer Browse When Fn.
*!*************************************************************
*
FUNCTION lfwCusBrow
lnCusBrRec  = RECNO("CUSTOMER")
=lfvpbSel()
SHOW WINDOW (lcCustBrow) REFRESH
*-- end of lfwLtrBrow.

*!*************************************************************
*! Name      : lfvCusBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Customer Browse Valid Fn.
*!*************************************************************
*
FUNCTION lfvCusBrow
IF !WONTOP(lcCustBrow)
  =gfStopBrow()
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+C ACTIVATE WINDOW (lcCustBrow)
ENDIF
*-- end of lfvCusBrow.


*!*************************************************************
*! Name      : lfUnTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : UnTrap Keys (Activate ).
*!*************************************************************
*
FUNCTION lfUnTrap
*IF The window on top is not the Browse and coming from the Browse
IF !(WONTOP(lcLetrBrow) AND WONTOP(lcCustBrow)) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcLetrBrow)
  ON KEY LABEL ALT+C ACTIVATE WINDOW (lcCustBrow)
ENDIF    && End of IF
*--

*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Trap Keys (Deactivate ).
*!*************************************************************
*
FUNCTION lfTrap
*-- IF The window on top is the Browse
IF WONTOP(lcLetrBrow) OR WONTOP(lcCustBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  IF WONTOP(lcLetrBrow)
    ON KEY LABEL TAB DO lpLtTraps WITH "TAB" , "L"
    ON KEY LABEL BACKTAB DO lpLtTraps WITH "BACKTAB" , "L"
    ON KEY LABEL ALT+B 
  ELSE
    ON KEY LABEL TAB DO lpLtTraps WITH "TAB" , "C"
    ON KEY LABEL BACKTAB DO lpLtTraps WITH "BACKTAB" , "C"
    ON KEY LABEL ALT+C 
  ENDIF  
ENDIF    && End of IF
*--

*!*************************************************************
*! Name      : lfLtTraps
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : Trap (Tab / Backtab) Keys from letter Browse.
*!*************************************************************
*
PROCEDURE lpLtTraps
PARAMETERS lcLtTrap , lcFromWhat
PRIVATE lcLtTrap , lcFromWhat , lcActWind
IF (lcFromWhat = "L" AND lcLtTrap = "TAB") OR (lcFromWhat = "C" AND lcLtTrap = "BACKTAB")
  ACTIVATE WINDOW (lcChLet2) TOP
  _CUROBJ = OBJNUM(letters.mletrbody)
ELSE
  ACTIVATE WINDOW (lcChLet4) TOP
  _CUROBJ = IIF(lcFromWhat = "L",OBJNUM(pbOk),OBJNUM(pbSelect))
ENDIF
*-- end of lpLtTraps.

*!*************************************************************
*! Name      : lfwIbLtBw
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter Invisable Button Validation.
*!*************************************************************
*
FUNCTION lfwIbLtBw
*IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbLtBw.

*!*************************************************************
*! Name      : lfwIbCuBw
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Letter Invisable Button Validation.
*!*************************************************************
*
FUNCTION lfwIbCuBw
*IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+C}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbCuBw.

*!*************************************************************
*! Name      : lfvAccSel
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Handle <Select>, <Select All>, <Select None> and <Invert> 
*!           : Buttons pressed.
*!*************************************************************
*! Passed Parameters  : Apply Method which will be one of the following
*!                    : 1- SEL_UNSEL :=> Select / UnSelect Line.
*!                    : 2- SEL_ALL   :=> Select All lines.
*!                    : 3- SEL_NON   :=> UnSelect All lines.
*!                    : 4- INVERT    :=> Invert Select / UnSelect Lines.
*!*************************************************************
*!
FUNCTION lfvAccSel
PARAMETERS lcApplyMth
*-- if you have records and you are at specific record and 
*-- if your choice is <Select All> its button must be enable and
*-- if your choice is <Select None> its button must be enable.
*-- this is because you have hot keys.

PRIVATE lcScanExpr , lnCurrAls
lnCurrAls = SELECT(0)

IF lcApplyMth = "SEL_UNSEL"
  =lfSelUnSel()

ELSE  && Sel All, Sel None, Or Invert

  DO CASE
    *B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
    *CASE lcApplyMth = "SEL_ALL"                     && Select All Case
    CASE lcApplyMth = "SEL_ALL"  AND lcLetrTo = "C"  && Select All Case
    *B607389,1 ALB Fix Bugs Relate to E302174 [END]
      SELECT CUSTOMER
      =SEEK("M")
      SCAN REST WHILE type+account+store = "M"
        
        *-- Record Not Found Before.
        IF EOF(lcTempMast)
          INSERT INTO (lcTempMast) (Account , cDefaLett , cOldLetter) ;
            VALUES (Customer.Account, Letters.cLetterId , SPACE(10))
        ELSE
          SELECT (lcTempMast)
          REPLACE cDefaLett WITH Letters.cLetterId
        ENDIF
      ENDSCAN
    
    *B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
    CASE lcApplyMth = "SEL_ALL"  AND lcLetrTo = "V"  && Select All Case
      SELECT APVENDOR
      LOCATE
      SCAN
        *-- Record Not Found Before.
        IF EOF(lcTempMast)
          INSERT INTO (lcTempMast) (cVendCode , cDefaLett , cOldLetter) ;
            VALUES (APVENDOR.cVendCode, Letters.cLetterId , SPACE(10))
        ELSE
          SELECT (lcTempMast)
          REPLACE cDefaLett WITH Letters.cLetterId
        ENDIF
      ENDSCAN
    *B607389,1 ALB Fix Bugs Relate to E302174 [END]
    
    CASE lcApplyMth = "SEL_NON"  && Select None Case
    
      SELECT (lcTempMast)
      REPLACE ALL cDefaLett WITH SPACE(10)

    CASE lcApplyMth = "INVERT"  && Invert Case
    
      SELECT CUSTOMER
      =SEEK("M")
      SCAN REST WHILE type+account+store = "M"
        =lfSelUnSel()
      ENDSCAN  

  ENDCASE
  GO lnCusBrRec IN CUSTOMER

ENDIF
SELECT (lnCurrAls)

*-- Apply Select Function.
=lfvpbSel()
SHOW WINDOW (lcCustBrow) REFRESH
*-- end of lfvAccSel.

*!*************************************************************
*! Name      : lfSelUnSel
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Select / Unselect Record.
*!*************************************************************
*!
FUNCTION lfSelUnSel
PRIVATE llFindIt
llFindIt = !EOF(lcTempMast)

IF llFindIt AND !EMPTY(&lcTempMast..cDefaLett)
  SELECT (lcTempMast)
  REPLACE cDefaLett WITH SPACE(10)

ELSE

  *-- empty letter.
  IF llFindIt
    SELECT (lcTempMast)
    REPLACE cDefaLett WITH Letters.cLetterId
  ELSE
    *B607389,1 ALB Fix Bugs Relate to E302174 [BEGIN]
    IF lcLetrTo = "C"
      INSERT INTO (lcTempMast) (Account , cDefaLett , cOldLetter) ;
        VALUES (Customer.Account, Letters.cLetterId , SPACE(10))
    ELSE
      INSERT INTO (lcTempMast) (cvendcode , cDefaLett , cOldLetter) ;
        VALUES (APVENDOR.cVENDCODE, Letters.cLetterId , SPACE(10))
    ENDIF
    *B607389,1 ALB Fix Bugs Relate to E302174 [END]
  ENDIF

ENDIF
*
*GO RECNO("CUSTOMER") IN CUSTOMER
*-- end of lfSelUnSel.

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : if Current status is Select prompt is Un-Select and vice versa
*!*************************************************************
*
FUNCTION lfvpbSel
IF EMPTY(&lcTempMast..cDefaLett)
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'    &lcSelSt
ELSE    && Else
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'  &lcSelSt
ENDIF
*-- end of lfvpbSel.

*!*************************************************************
*! Name      : lfvClose
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Ok / Cancel Screen.
*!*************************************************************
*
FUNCTION lfvClose
PARAMETERS llKeyPress
PRIVATE llKeyPress
llOkPress = llKeyPress
ACTI WINDOW (lcChLet4) TOP
CLEAR READ
*-- end of lfvClose.

*-- lcOrdCanc : Cancellation Table Name.



*: E500341,1 BWA 05/29/2000 Modify inrange screen with the criteria of the style
*: E500341,1                season division fabric and style group to give the
*: E500341,1                user the ability to select the color.
*!*************************************************************
*! Name      : LFVSTYLE
*! Developer : BASSEM RAFAAT 
*! Date      : 05/29/2000
*! Purpose   : Brow the style inrange and modify the filter.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = LFVSTYLE()
*!*************************************************************
FUNCTION LFVSTYLE

*B603716,1 BWA 06/29/2000 Fix the bug of alias name not found in case the user select 
*B603716,1                all records and then select color.[START]
*PRIVATE lcAliasSty , lcStyColor
PRIVATE lcAliasSty
*B603716,1 [END]


lcAliasSty = ALIAS()
STORE SPACE(0) TO lcExpsty , lcStyColor
STORE 0 TO lnStyColor

*B803344,1 .[START]
*lcTempStyle = gfTempName()
*B803344,1 [END]

*B603716,1 BWA 06/29/2000 Fix the bug of alias name not found in case the user select 
*B603716,1                all records and then select color.[START]
DIMENSION laStySeg[1]
STORE SPACE(0) TO laStySeg

=gfItemMask(@laStySeg)
FOR lnCount = 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCount,1]='F'
    lnStPos = LEN(laStySeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR
*B603716,1 [END]

CREATE CURSOR (lcTempStyle) (Style C(19))
ZAP
INDEX ON Style TAG (lcTempStyle) OF (gcWorkDir+lcTempStyle+'.CDX')
*E119114,1 ASH 08/14/2003 (Begin) Display the WIP&Stock on the selected styles browse.
*lcBrowFlds = [STYLE     :H = 'Style'       :25 ,]    + ;
             [DESC      :H = 'Description' :25 ,]    + ;
             [SEASON    :H = 'Season'      :15 ,]    + ;
             [CDIVISION :H = 'Division'    :15 ,]    + ;
             [PRICEA    :H = 'Price'       :10 ]

lcBrowFlds = [STYLE     :H = 'Style'       :20 ,]    + ;
             [DESC      :H = 'Description' :25 ,]    + ;
             [SEASON    :H = 'Season'      :10 ,]    + ;
             [CDIVISION :H = 'Division'    :10 ,]    + ;
             [TOTWIP    :H = 'WIP'         :10 ,]    + ;
             [TOTSTK    :H = 'Stock'       :10 ,]    + ;     
             [TOTORD    :H = 'Order'       :10 ,]    + ;     
             [PRICEA    :H = 'Price'       :10 ]    
*E119114,1 ASH 08/14/2003 (End)
*--THE TEMPORARY INDEX.
lcTmpIndx = gfTempName()
SELECT STYLE
INDEX ON  CSTYMAJOR TAG lcMmajour OF (gcWorkDir + lcTmpIndx+ '.CDX') 
SET ORDER TO TAG lcMmajour OF (gcWorkDir +lcTmpIndx  + '.CDX')

*B803344,1 BWA 06/18/2000 check if the lcstyexp is empty we preview for .T. [START]
*lcExpsty = 'FOR ' + lcStyExp + IIF(!EMPTY(lcStyleExp),' AND ' + lcStyleExp , "")
lcExpsty = 'FOR ' + IIF(!EMPTY(lcStyExp),lcStyExp,".T.") + IIF(!EMPTY(lcStyleExp),' AND ' + lcStyleExp , "")
*B803344,1 [END]

=gfrange(lcBrowFlds,lcTempStyle,"Style",(lcExpsty),"","","@! XXXXXXXXXXXXXXXXXXX")


DIMENSION laStylFle[1,7]
laStylFle[1,1]= "STYLE.STYLE"
laStylFle[1,2]= "F"
laStylFle[1,3]= "C"
laStylFle[1,4]= .T.
laStylFle[1,5]= "In List             "
laStylFle[1,6]= lcTempStyle
laStylFle[1,7]= "R"

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laBrFldFlt[2,ALEN(laBrTmpFlt,2)]
laBrFldFlt = ""
laBrFldFlt[2,1] = "STYLE"
laBrFldFlt[2,2] = lcBrowFlds
laBrFldFlt[2,3] = ""
laBrFldFlt[2,4] = ""
laBrFldFlt[2,5] = "laStylFle:1"

lcStyColor = gfGenFlt('laStylFle',.T.,.T.)
DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]
=ACOPY(laBrTmpFlt,laBrFldFlt)

*B803344,1 BWA 06/18/2000 Check if the lcstycolor is empty we add space(0) to the filter. [START]
*lcStyExp = lcStyExp + " AND " + lcStyColor
lcStyExp = lcStyExp + IIF(!EMPTY(lcStyExp),IIF(!EMPTY(lcStyColor), " AND " + lcStyColor,""), lcStyColor )
*B803344,1 [END]

SELECT STYLE
CLOSE INDEX
ERASE (gcWorkDir +lcTmpIndx  + '.CDX')

SELECT (lcAliasSty)
*E500341,1 [END]
                     *-- End Function LFVSTYLE --*



*!**************************************************************************
*! Name      : lfChkAlloc
*! Developer : Ahmed Mohamed Elanwer (AME)
*! Date      : 08/28/2001
*! Purpose   : Function to Check the allocation status
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return      : Allocation Message Flag
*!***************************************************************************
*!This Function Due to B604402
*!***************************************************************************
*B604402,1
FUNCTION lfChkAlloc

lnSelect = SELECT()
IF !USED('POSHDR')
  =gfOpenFile(gcDataDir+"POSHDR","POSHDR","SH")
ENDIF
*B121840,1 NNA 03/17/2004 (Begin) Check if the MF Module installed or not to open or not open the file
IF 'MF' $ gcComp_mdl
*B121840,1 NNA (End)
  IF !USED('CUTTKTH')
    =gfOpenFile(gcDataDir+"CUTTKTH","CUTTKTH","SH")  
  ENDIF
*B121840,1 NNA 03/17/2004 (Begin) End if statment
ENDIF
*B121840,1 NNA (End)
IF !USED('POSLN')
  =gfOpenFile(gcDataDir+"POSLN","POSLN","SH")
ENDIF

*B121840,1 NNA 03/17/2004 (Begin) Check if the MF Module installed or not to open or not open the file
IF 'MF' $ gcComp_mdl
*B121840,1 NNA (End)
  IF !USED('CUTTKTL')
    =gfOpenFile(gcDataDir+"CUTTKTL","Cutlin","SH")  
  ENDIF
*B121840,1 NNA 03/17/2004 (Begin) End if statment
ENDIF
*B121840,1 NNA (End)

SELECT (lcOrdTemp)
SCAN FOR LLSEL = .T.
*B604402,4 AME [Start] don't delete lines with piked Qtys
IF &lcOrdTemp..Picked AND !EMPTY(&lcOrdTemp..PikTkt)
  REPLACE &lcOrdTemp..llSel WITH .F.
  IF !llPikMsg
    *lcMsg = 'One or More Sales Order Line(s) have Picked Quantitys These Line(s) will be Ignored'
    = gfModalGen('INM32094B00000','ALERT')
    llPikMsg = .T.
  ENDIF
  LOOP
ENDIF
*B604402,4 AME [End]
  IF llOpenCut
    SELECT CUTPICK
    lcCutPkOrd = ORDER()
    SET ORDER TO TAG CUTORD
    SCAN FOR ORDER  = &lcOrdTemp..ORDER .AND. ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(&lcOrdTemp..LINENO))
      IF !llAlocMsg
        IF llUsrCPrv
          lcMsg = 'These Sales Order lines have allocated quantities,'+;
                  'canceling these lines will release the allocation, would you like to continue?'
          lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
        ELSE
          lcMsg = 'These Sales Order lines have allocated quantities,'+;
                  'canceling these lines will release the allocation, but access to do this is denied, Cannot proceed.'
          =gfModalGen('TRM32059B00000','DIALOG',lcMsg)
          lnRespons = 2
        ENDIF
        llAlocMsg    = .T.
        IF lnRespons = 1
          llOkCancel = .T.
        ELSE
          llOkCancel = .F.
        ENDIF
      ENDIF
      REPLACE &lcOrdTemp..LLSEL WITH llOkCancel

      *--Check for CT/PO updates only if on Hold
      IF !llUpdPoCt .AND. !llAcordMsg AND llOkCancel
        IF CUTPICK.TRANCD = '1'
          IF SEEK(CUTPICK.CTKTNO,'CUTTKTH') .AND. CUTTKTH.STATUS = 'H'
            lcMsg = 'CT/PO is Hold, Would You like to update the generated CT/PO accordingly?'
            lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
            llAcordMsg = .T.
            IF lnRespons = 1
              llUpdPoCt  = .T.
            ELSE
              llUpdPoCt  = .F.
            ENDIF
          ENDIF
        ELSE
          IF (SEEK('P'+CUTPICK.CTKTNO,'POSHDR') .OR. SEEK('C'+CUTPICK.CTKTNO,'POSHDR')) .AND. POSHDR.STATUS = 'H'
            lcMsg = 'CT/PO is Hold, Would You like to update the generated CT/PO accordingly?'
            lnRespons = gfModalGen('TRM32059B32000','DIALOG',lcMsg)
            llAcordMsg = .T.
            IF lnRespons = 1
              llUpdPoCt  = .T.
            ELSE
              llUpdPoCt  = .F.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lnSelect)
*!**************************************************************************
*! Name      : lfRelAllOc
*! Developer : Ahmed Mohamed Elanwer (AME)
*! Date      : 08/27/2001
*! Purpose   : Function to Check the allocation status
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return      : NONE
*!***************************************************************************
*!This Function Due to B604402
*!***************************************************************************
*B604402,1
FUNCTION lfRelAllOc

IF llOpenCut
  SELECT CUTPICK
  lcCutPkOrd = ORDER()
  SET ORDER TO TAG CUTORD
  LOCATE
  SCAN FOR ORDER = &lcOrdTemp..ORDER .AND. ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(&lcOrdTemp..LINENO))
    IF TRANCD = '1'
      IF SEEK(CUTPICK.CTKTNO,'CUTTKTH')
        IF CUTTKTH.STATUS = 'H'
          IF llUpdPoCt
            PRIVATE lcStyOdr ,lcDyOrd
            *--Update Cut Ticket
            SELECT CUTTKTL
            lcOldCTOrd = ORDER()
            SET ORDER TO TAG Cutlin
            IF SEEK (CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1')
              WAIT WINDOW 'Updating CT# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
              REPLACE QTY1   WITH QTY1   - CUTPICK.QTY1,;
                      QTY2   WITH QTY2   - CUTPICK.QTY2,;
                      QTY3   WITH QTY3   - CUTPICK.QTY3,;
                      QTY4   WITH QTY4   - CUTPICK.QTY4,;
                      QTY5   WITH QTY5   - CUTPICK.QTY5,;
                      QTY6   WITH QTY6   - CUTPICK.QTY6,;
                      QTY7   WITH QTY7   - CUTPICK.QTY7,;
                      QTY8   WITH QTY8   - CUTPICK.QTY8,;
                      TOTQTY WITH TOTQTY - CUTPICK.TOTQTY
              SELECT STYLE
              lcStyOdr = ORDER()
              SET ORDER TO STYLE
              IF SEEK(CUTTKTL.STYLE,'STYLE')
                =RLOCK()
                REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                        WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                        WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                        WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                        WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                        WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                        WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                        WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                        TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                        NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                        NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                        NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                        NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                        NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                        NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                        NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                        NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                        NTOTWO WITH NTOTWO - M.TOTQTY

                UNLOCK
                SET ORDER TO (lcStyOdr)
                    
                SELECT STYDYE
                lcDyOrd = ORDER()
                SET ORDER TO STYDYE
                IF SEEK(CUTTKTL.STYLE+CUTTKTL.CWARECODE+PADR(CUTTKTL.DYELOT,10),'STYDYE')
                      
                  =RLOCK()
                  REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                          WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                          WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                          WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                          WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                          WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                          WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                          WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                          TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                          NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                          NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                          NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                          NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                          NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                          NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                          NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                          NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                          NTOTWO WITH NTOTWO - M.TOTQTY
                  UNLOCK
                ENDIF
                SET ORDER TO (lcDyOrd)
                SELECT CUTTKTL
              ENDIF
            ENDIF
            SET ORDER TO TAG &lcOldCTOrd
            *--Update cutting ticket header file

            SELECT CUTTKTH
            = SEEK (CUTPICK.CTKTNO)
            lnRestOpn = PCS_OPN - CUTPICK.TOTQTY
            =RLOCK()
            REPLACE PCS_OPN WITH IIF(PCS_OPN > 0 , PCS_OPN - CUTPICK.TOTQTY , PCS_OPN),;
                    TOTORD  WITH IIF(TOTORD > 0 , TOTORD - CUTPICK.TOTQTY , TOTORD),;
                    PCS_CAN WITH PCS_CAN + CUTPICK.TOTQTY
                
            REPLACE nEst_Cost1 WITH (CUTTKTL.nCost1 * lnRestOpn),;
                    nEst_Cost2 WITH (CUTTKTL.nCost2 * lnRestOpn),;
                    nEst_Cost3 WITH (CUTTKTL.nCost3 * lnRestOpn),;
                    nEst_Cost4 WITH (CUTTKTL.nCost4 * lnRestOpn),;
                    nEst_Cost5 WITH (CUTTKTL.nCost5 * lnRestOpn)
                
            IF PCS_OPN = 0
              REPLACE STATUS WITH IIF(PCS_REC > 0 , 'C' , 'X')
            ENDIF
            UNLOCK
          ENDIF
        ENDIF
      
        *--Releas The Allocation
        SELECT ORDHDR
        =SEEK (M.CordType+M.ORDER)
        REPLACE TOTCUT WITH TOTCUT - CutPick.TotQty
        
        SELECT ORDLINE
            
            
        = SEEK('O' + CutPick.Order + CutPick.CORDLINE)
        REPLACE CUT1   WITH 0,;
                CUT2   WITH 0,;
                CUT3   WITH 0,;
                CUT4   WITH 0,;
                CUT5   WITH 0,;
                CUT6   WITH 0,;
                CUT7   WITH 0,;
                CUT8   WITH 0,;
                TOTCUT WITH 0
          
        *-- Releas the allocated QTYs From CutTktl file
        SELECT CUTTKTL
        lcOldCTOrd = ORDER()
        SET ORDER TO TAG Cutlin
        IF SEEK (CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1')
          REPLACE ORD1   WITH 0,;
                  ORD2   WITH 0,;
                  ORD3   WITH 0,;
                  ORD4   WITH 0,;
                  ORD5   WITH 0,;
                  ORD6   WITH 0,;
                  ORD7   WITH 0,;
                  ORD8   WITH 0,;
                  TOTORD WITH 0
        ENDIF
        SET ORDER TO TAG &lcOldCTOrd
        SELECT CUTPICK
        BLANK
        DELETE
      ENDIF
    ENDIF
  
  
  
  
    IF TRANCD = '2'
      IF SEEK('P' + CUTPICK.CTKTNO,'POSHDR') OR SEEK('C' + CUTPICK.CTKTNO,'POSHDR')
        IF POSHDR.STATUS = 'H'
          IF llUpdPoCt AND (SEEK ('P' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN' ) OR SEEK('C' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN'))
            *--Update PO
            WAIT WINDOW 'Updating PO# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
            SELECT POSLN
            REPLACE QTY1  WITH QTY1   - CUTPICK.QTY1,;
                    QTY2  WITH QTY2   - CUTPICK.QTY2,;
                    QTY3  WITH QTY3   - CUTPICK.QTY3,;
                    QTY4  WITH QTY4   - CUTPICK.QTY4,;
                    QTY5  WITH QTY5   - CUTPICK.QTY5,;
                    QTY6  WITH QTY6   - CUTPICK.QTY6,;
                    QTY7  WITH QTY7   - CUTPICK.QTY7,;
                    QTY8  WITH QTY8   - CUTPICK.QTY8,;
                    TOTQTY WITH TOTQTY- CUTPICK.TOTQTY

            PRIVATE lcStyOdr ,lcDyOrd
            SELECT STYLE
            lcStyOdr = ORDER()
            SET ORDER TO STYLE
            IF SEEK(POSLN.STYLE,'STYLE')
              SELECT STYLE
              =RLOCK()
              REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                      WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                      WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                      WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                      WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                      WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                      WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                      WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                      TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                      NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                      NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                      NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                      NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                      NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                      NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                      NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                      NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                      NTOTWO WITH NTOTWO - M.TOTQTY
              UNLOCK
              SET ORDER TO (lcStyOdr)
              SELECT STYDYE
              lcDyOrd = ORDER()
              SET ORDER TO STYDYE

              IF SEEK(POSLN.STYLE+POSLN.CWARECODE+PADR(POSLN.DYELOT,10),'STYDYE')
                =RLOCK()
                REPLACE WIP1   WITH WIP1   - CUTPICK.QTY1  ,;
                        WIP2   WITH WIP2   - CUTPICK.QTY2  ,;
                        WIP3   WITH WIP3   - CUTPICK.QTY3  ,;
                        WIP4   WITH WIP4   - CUTPICK.QTY4  ,;
                        WIP5   WITH WIP5   - CUTPICK.QTY5  ,;
                        WIP6   WITH WIP6   - CUTPICK.QTY6  ,;
                        WIP7   WITH WIP7   - CUTPICK.QTY7  ,;
                        WIP8   WITH WIP8   - CUTPICK.QTY8  ,;
                        TOTWIP WITH TOTWIP - CUTPICK.TOTQTY,;
                        NWO1   WITH NWO1   - CUTPICK.QTY1  ,;
                        NWO2   WITH NWO2   - CUTPICK.QTY2  ,;
                        NWO3   WITH NWO3   - CUTPICK.QTY3  ,;
                        NWO4   WITH NWO4   - CUTPICK.QTY4  ,;
                        NWO5   WITH NWO5   - CUTPICK.QTY5  ,;
                        NWO6   WITH NWO6   - CUTPICK.QTY6  ,;
                        NWO7   WITH NWO7   - CUTPICK.QTY7  ,;
                        NWO8   WITH NWO8   - CUTPICK.QTY8  ,;
                        NTOTWO WITH NTOTWO - M.TOTQTY
                UNLOCK
              ENDIF
              SET ORDER TO (lcDyOrd)
              SELECT POSLN
            ENDIF

            SELECT POSHDR
            IF SEEK ('P' + CUTPICK.CTKTNO) OR SEEK(('C' + CUTPICK.CTKTNO))
              =RLOCK()
              REPLACE OPEN   WITH (OPEN - CUTPICK.TOTQTY),;
                      CANCEL WITH (CANCEL+ CUTPICK.TOTQTY)
              IF OPEN <= 0
                REPLACE STATUS WITH IIF(RECEIVE > 0 , 'C' , 'X')
              ENDIF
            ENDIF
      
            REPLACE nICost1 WITH (POSLN.nCost1 * POSHDR.OPEN),;
                    nICost2 WITH (POSLN.nCost2 * POSHDR.OPEN),;
                    nICost3 WITH (POSLN.nCost3 * POSHDR.OPEN),;
                    nICost4 WITH (POSLN.nCost4 * POSHDR.OPEN),;
                    nICost5 WITH (POSLN.nCost5 * POSHDR.OPEN)

            UNLOCK
          *--B602671,1 HDM [End]
          ENDIF
        ENDIF
        SELECT POSHDR
        *-- RELEASE The allocation from poshdr File
        IF SEEK ('P' + CUTPICK.CTKTNO) OR SEEK(('C' + CUTPICK.CTKTNO))
          REPLACE TOTORD WITH 0
        ENDIF
        IF SEEK ('P' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN' ) OR SEEK('C' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.CtktLineNo + '1','POSLN')
          WAIT WINDOW 'Updating PO# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
          *-- Releas the allocated QTYs From POSLN file
          SELECT POSLN
          REPLACE ORD1   WITH 0,;
                  ORD2   WITH 0,;
                  ORD3   WITH 0,;
                  ORD4   WITH 0,;
                  ORD5   WITH 0,;
                  ORD6   WITH 0,;
                  ORD7   WITH 0,;
                  ORD8   WITH 0,;
                  TOTORD WITH 0
        ENDIF
      ENDIF

      *--Releas The Allocation
      SELECT ORDHDR
      =SEEK (M.CordType+M.ORDER)
      REPLACE TOTCUT WITH TOTCUT - CutPick.TotQty

      
      SELECT ORDLINE
      = SEEK('O' + CutPick.Order + CutPick.CORDLINE)
      REPLACE CUT1   WITH 0,;
              CUT2   WITH 0,;
              CUT3   WITH 0,;
              CUT4   WITH 0,;
              CUT5   WITH 0,;
              CUT6   WITH 0,;
              CUT7   WITH 0,;
              CUT8   WITH 0,;
              TOTCUT WITH 0
      SELECT CUTPICK
      BLANK
      DELETE
    ENDIF
  ENDSCAN
  SELECT CUTPICK
  SET ORDER TO TAG (lcCutPkOrd)
ENDIF
  
