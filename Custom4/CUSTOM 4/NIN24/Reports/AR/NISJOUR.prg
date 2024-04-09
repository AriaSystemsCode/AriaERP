*:***************************************************************************
*: Program file  : NISJOUR
*: Program desc. : Custom Sales Journal Report
*: Module        : Accounts receivable (AR)
*: Developer     : Saber A Razek  (SAB)
*: Tracking Job Number: [C201494]
*: Date          : 06/25/2012
*:***************************************************************************
*:Modifications : ....
*C202243,1 Es 01/16/2018 'T20181228.0004'
*:************************************************ Report Code Begin  ********************************************************

#INCLUDE R:\Aria4XP\Reports\AR\NISJOUR.H

lcStTime   = TIME()    && Time in which we start collect data.
llNoIndex = .F.        && I don't make index for file.
*-- Show messages in status bar when collecting data.
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON

*-- Use variable llOGFltCh that detect OG filter changes.
IF llClearFn OR loOGScroll.llOGFltCh
  STORE .F. TO llChAcc,llChTrnTyp,llClearFn
  lcLastExpr = lcRpExp
ENDIF 

*-- Include void invoices amount and ship amount if upper invoice date is less than void invoice date.
lnInvPos = lfItmPos('INVHDR.INVDATE')
STORE {  /  /  } TO ldStrtDate , ldEndDate

*-- Fill the date variables.
 
IF lcRpDateTP = "P"
  STORE {  /  /  } TO ldStrtDPst , ldEndDPst
  lnPostDate = lfItmPos('DPOSTDATE')
  *-- Variable Hold true If you print Void invoices only.
  IF EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
    ldStrtDPst = CTOD(PADR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])-1))
    ldEndDPst  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])+1))
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst)) OR (!BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))))]
  ENDIF
ENDIF

*-- Don't get to this code in case posted date.
IF lcRpDateTP # "P"
  IF EMPTY(loOGScroll.laOGFxFlt[lnInvPos,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
    ldStrtDate = CTOD(PADR(loOGScroll.laOGFxFlt[lnInvPos,6],ATC('|',loOGScroll.laOGFxFlt[lnInvPos,6])-1))
    ldEndDate  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnInvPos,6],ATC('|',loOGScroll.laOGFxFlt[lnInvPos,6])+1))
    *-- Add Void between to Range.
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(INVDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(INVDATE,ldStrtDate,ldEndDate))))]
  ENDIF
ENDIF

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  *-- lcDetExp   : Detail band expression.
  *-- lcCurrExp  : Currency group footer expression.
  *-- lcGrpExp   : Variable group footer expression.
  *-- lcGrandExp : Summary band expression.

  *-- Display HST Tax amount for Canada.
  lcDetExp = [IIF(llMultCurr AND (lcRpCurr != 'F') AND (lcRpSortBy !='U'),;
                 LANG_Arsjour_Invoice+LANG_Arsjour_Currency+' : ' + cCurrCode + " , ",'')] +;
             [+ IIF(llRpRepPrn,LANG_Arsjour_SalesRep+' ' + Rep1 +  " " +LANG_Arsjour_CommPayable+" =  "  +;
                                 TRANSFORM(lnRepComm,"9999999.99") ," ")] +;
             [+IIF(llCanada, SPACE(10) + LANG_Arsjour_PstAmount +;
                              TRANSFORM(lnPstAmt,"99999999.99")+;
                              SPACE(10) + LANG_Arsjour_HstAmount + ;
                              TRANSFORM(lnHstAmt,"99999999.99")," ")]
  lcCurrExp  = [IIF(llRpRepPrn, SPACE(10) +LANG_Arsjour_SalesRep+' '+LANG_Arsjour_CommPayable+' '+LANG_Arsjour_Total +;
                                TRANSFORM(lnCommCur,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + LANG_Arsjour_PstTotal +;
                               TRANSFORM(lnPstAmtCr,"99999999.99")+;
                               SPACE(10) + LANG_Arsjour_HstTotal + ;
                               TRANSFORM(lnHstAmtCr,"99999999.99")," ")]
  lcGrpExp = [IIF(llRpRepPrn, SPACE(10) + LANG_Arsjour_SalesRep+' '+LANG_Arsjour_CommPayable+' '+LANG_Arsjour_Total  +;
                              TRANSFORM(lnCommGrp,"9999999.99") ," ")] +;
             [+IIF(llCanada, SPACE(10) + LANG_Arsjour_PstTotal +;
                              TRANSFORM(lnPstAmtGp,"99999999.99")+;
                              SPACE(10) + LANG_Arsjour_HstTotal + ;
                              TRANSFORM(lnHstAmtGp,"99999999.99")," ")]
  lcGrandExp = [IIF(llRpRepPrn, SPACE(10) + LANG_Arsjour_SalesRep+' '+LANG_Arsjour_CommPayable+' '+LANG_Arsjour_Grand +;
                                TRANSFORM(lnCommTot,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + LANG_Arsjour_PstGrand +;
                               TRANSFORM(lnPstAmtot,"99999999.99")+;
                               SPACE(10) + LANG_Arsjour_HstGrand + ;
                               TRANSFORM(lnHstAmtot,"99999999.99")," ")]

  *-- Create temporary file that holding order InvHdr data.
  lcWorkFile = gfTempName()
  DIMENSION laTempStru[1,18]
  laTempStru = ''

  SELECT INVHDR
  =AFIELD(laTempStru)
  lnTempStru=ALEN(laTempStru,1) + 1
  DIMENSION laTempStru[lnTempStru,18]

  *-- cTempKey : field used in most sort by case as the master key.
  *--          : note that field width is dependent on number of sort case he make.
  laTempStru[lnTempStru,1] = 'cTempKey'
  laTempStru[lnTempStru,2] = 'C'
  laTempStru[lnTempStru,3] = 62
  laTempStru[lnTempStru,4] = 0
  
  FOR  lnInc=7 TO 16 
    STORE SPACE(1) TO laTempStru[lnTempStru,lnInc]
  ENDFOR 
  STORE 0  TO laTempStru[lnTempStru,17], laTempStru[lnTempStru,18]
  
  lnTempStru=ALEN(laTempStru,1) + 1
  DIMENSION laTempStru[lnTempStru,18]
  laTempStru[lnTempStru,1] = 'ReltInv'
  laTempStru[lnTempStru,2] = 'C'
  laTempStru[lnTempStru,3] = 6
  laTempStru[lnTempStru,4] = 0
  
  FOR  lnInc=7 TO 16 
    STORE SPACE(1) TO laTempStru[lnTempStru,lnInc]
  ENDFOR 
  STORE 0  TO laTempStru[lnTempStru,17], laTempStru[lnTempStru,18]
   
  * C202243,1 Es 01/16/2018 'T20181228.0004'[Start]
  lnTempStru=ALEN(laTempStru,1) + 1
  DIMENSION laTempStru[lnTempStru,18]
  laTempStru[lnTempStru,1] = 'Csh_Rec_no'
  laTempStru[lnTempStru,2] = 'C'
  laTempStru[lnTempStru,3] = 6
  laTempStru[lnTempStru,4] = 0
  FOR  lnInc=7 TO 16 
    STORE SPACE(1) TO laTempStru[lnTempStru,lnInc]
  ENDFOR 
  STORE 0  TO laTempStru[lnTempStru,17], laTempStru[lnTempStru,18]
  * C202243,1 Es 01/16/2018 'T20181228.0004' [End] 
   
   
  llFrTime = .F.  && After this time all of your variables have been defined,  you do not need to go to any llFrTime block again.
ENDIF

*-- Create temporary cursors from structure array
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

*-- Use variable llOGFltCh that detect OG filter changes.
IF llClearFn OR loOGScroll.llOGFltCh
  llClearFn = .F.
  *-- If the file already have data, clear it.
  IF RECCOUNT(lcWorkFile) > 0
    USE IN (lcWorkFile)
    =lfCreatCur(lcWorkFile)  && Create work cursor again.
    llNoIndex = .T.
  ENDIF

  *-- If User Change Index tag due to change sort by.
  IF llNoIndex OR (lcLastTag != lcIndexTg)
    = lfUserChTg()
  ENDIF

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
  ENDIF

ENDIF

*-- Select Master report file.
SELECT (lcWorkFile)
*-- Relation Section [begin]
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
SET RELATION TO cWareCode INTO Warehous ADDITIVE
SET RELATION TO Rep1 INTO Salesrep ADDITIVE
IF llMultCurr
  SET RELATION TO ccurrcode INTO Syccurr ADDITIVE
ENDIF

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW LANG_Arsjour_SelectMsg +' '+ ALLTRIM(STR(RECCOUNT(lcWorkFile))) + LANG_Arsjour_RecInMsg + ALLTRIM(STR(lnInterval,6,2)) + LANG_Arsjour_SecondMsg TIMEOUT 2

*-- Call Report [lcRpForm = 'NISJOUR.FRX']
loogScroll.cCROrientation = 'P'

DO gfDispRe WITH EVAL('lcRpForm')

SET STATUS BAR &lcStatusBr    && Restore previous status bar status.

*:************************************************ Report Code End ********************************************************


*!*************************************************************
*! Name      : lfStitle
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : 1- Get state title.
*!           : 2- Know in which country we are.
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

ENDFUNC && lfStitle

   
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Option Grid When function
*!*************************************************************
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control reset case which does not rise llFirsttime, 
*-- but restore initial value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'
  
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.

  PRIVATE lcThAlias
  lcThAlias = ALIAS()
  *-- Compute Start of variable filter to control its apperance in option grid.
  lnVarbEnd = 0
  *-- Calculate length of variables appears in option grid and items that we enable and disable.
  PRIVATE lcSelectCommand2,lnResult2  
  lcSelectCommand2=[SELECT * FROM SYREPUVR WHERE Crep_id="ARSJOUR" AND cExpType="V" AND nVarPos!=0 AND (EMPTY(CVER) OR CVER = "A40")] 
  lnResult2  = oAriaApplication.remotesystemdata.execute(lcSelectCommand2,"","TMPREPUVR","",oAriaApplication.cAria4SysFiles,3,"",SET("DATASESSION")) 

  IF lnResult2 >= 1 
    lnVarbEnd = lnVarbEnd + 1  
  ENDIF

  SELECT (lcThAlias)

  IF EMPTY(laRpFltVal)
    *-- laRpFltVal : Array to hold D for disable and E for enable, to control Fixed filter appearance in option grid.
    *-- laRpVarNow : Array to hold .T. or .F., to control variables appearance in option grid.
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
ENDIF
*-- Disable the Sort#2 Option When the sort#1 is by invoice
lnSortPos=lfVarPos('lcRpSort2')
DO lpShowObj
*-- note that disable and enable is according to value of laRpFltVal.

ENDFUNC && lfwRepWhen


*!***************************************************************************
*! Name      : lfVarPos
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : OG when function
*!***************************************************************************
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(loOGScroll.laOGObjType,UPPER(lcItmInFlt))
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos

ENDFUNC && lfVarPos


*!*************************************************************
*! Name      : lfFltState
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
FUNCTION lfFltState
PARAMETERS lnObjNum,lcObjState

IF lcObjState = 'D' AND !EMPTY(loOGScroll.laOGFxFlt[lnObjNum,6])
  loOGScroll.laOGFxFlt[lnObjNum,6] = ''
ENDIF
LAOGOBJCNT[LNOBJNUM + LNVARBEND] = (LCOBJSTATE = 'E')
= LFOGSHOWGET('loOGScroll.LAOGFXFLT[' + ALLTRIM(STR(LNOBJNUM)) + ',6]')  && ENABLE / DISABLE OBJECT .

ENDFUNC && lfFltState


*!*************************************************************
*! Name      : lfFormName
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Function to get the Form name
*!*************************************************************
FUNCTION lfFormName

RETURN 'NISJOUR'

ENDFUNC && lfFormName

*!*************************************************************
*! Name      : lfUserChTg
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Change Work file Index Tag.
*!*************************************************************
FUNCTION lfUserChTg
SELECT (lcWorkFile)
INDEX ON &lcIndexTg TAG (lcWorkFile)
IF llNoIndex
  llNoIndex = .F.
ELSE
  lcLastTag = lcIndexTg
ENDIF

ENDFUNC && lfUserChTg

*!*************************************************************
*! Name      : lfCollData
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Make optimized expression then Collecting data
*!             from InvHdr file into Work file.
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
      
      lnAccOcurr=ATC('ACCOUNT',lcTake)
      lnCurOcurr=ATC('CCURRCODE',lcTake)
      lnAdrOcurr=ATC('CADDRESS4',lcTake)
      lnRepOcurr=ATC('REP1',lcTake)
      
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

*-- Create temp files for Division,Season,Location,payment terms
lcDivCursor = lfOGTempCurs("INVHDR.CDIVISION", "CDIVISION", "CDIVISION", "loOGScroll.laOgVrFlt", .T.)
lcSeaCursor = lfOGTempCurs("INVHDR.SEASON", "SEASON", "SEASON", "loOGScroll.laOgVrFlt", .T.)
lcLocCursor = lfOGTempCurs("INVHDR.CWARECODE", "CWARECODE", "CWARECODE", "loOGScroll.laOgVrFlt", .T.)
lcTrmCursor = lfOGTempCurs("INVHDR.CTERMCODE", "CTERMCODE", "CTERMCODE", "loOGScroll.laOgVrFlt", .T.)
lcStatCursor = lfOGTempCurs("CUSTOMER.CADDRESS4", "Address", "Address", "loOGScroll.laOgVrFlt", .T.)
lcAcctCursor = lfOGTempCurs("INVHDR.ACCOUNT", "ACCOUNT", "ACCOUNT", "loOGScroll.laOgVrFlt", .F.)
lcCurrCursor = lfOGTempCurs("INVHDR.CCURRCODE", "CCURRCODE", "CCURRCODE", "loOGScroll.laOgFxFlt", .F.)
lcRep1Cursor = lfOGTempCurs("INVHDR.REP1", "Rep1", "Rep1", "loOGScroll.laOgVrFlt", .F.)

lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")


*-- Add void date to filter expression

*-- Check for the posted filter also.
IF !EMPTY(loOGScroll.laOGFxFlt[lnInvPos,6]) OR IIF(lcRpDateTP = "P" , !EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6]) , .F.)
  *-- If Void Only.
  IF lcRpList = "V"
    lcRpExp = STRTRAN(lcRpExp,"INVDATE","VDATE")
  ENDIF

  IF lcRpList = "V" AND lcRpDateTP = "P"
    lcRpExp = STRTRAN(lcRpExp,"DPOSTDATE","VDATE")
  ENDIF

  *-- If Print Both Active and Void invoices, change the filter expr 'lcRpExp' to set the date range In the same level
  IF lcRpList = "B"   
    IF (!EMPTY(ldStrtDate) OR !EMPTY(ldEndDate)) OR IIF(lcRpDateTP = "P" , (!EMPTY(ldStrtDPst) OR !EMPTY(ldEndDPst)) , .F.)
      *-- Change AND to OR.
      DO lpGenExpr
    ENDIF
  ENDIF
ENDIF

SELECT INVHDR
SET ORDER TO   && Activate rushmore optimizing.
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer

*-- cTempKey : Field in work file that contains Country code, plus State code, plus region code.
*            : this field serves in sort by [Country,State,Region]

*-- When running the report to show both active and void invoices and for a specific
*   division - the report prints the void invoices from all divisions.[START]
lcDivsion = ".T."
IF  "V" $ lcRpList OR "B" $ lcRpList
  STORE 0 TO lnDataDiv
  lnDataDiv = ASCAN(loOGScroll.laOGVrFlt,'INVHDR.CDIVISION')
  IF lnDataDiv > 0
    lnDataDiv  = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnDataDiv,1)
    lcDivsion = IIF(!EMPTY(loOGScroll.laOGVrFlt[lnDataDiv,6]) , "INVHDR.CDIVISION $ laOgVrFlt[lnDataDiv,6]", ".T.")
  ENDIF
ENDIF

*-- Replace the filter in case the posted date.
IF lcRpDateTP = "P"
  lcRpExp = STRTRAN(lcRpExp,"INVDATE","DPOSTDATE")
  lcRpExp = STRTRAN(lcRpExp,"ldStrtDate","ldStrtDPst")
  lcRpExp = STRTRAN(lcRpExp,"ldEndDate","ldEndDPst")
ENDIF
 
*C202243,1 Es 01/16/2018 'T20181228.0004'[Start]
IF !USED('CREDIT')
  =gfOpenTable('CREDIT', 'CREDIT', 'SH')
ENDIF  

IF !USED('ARHIST')
  =gfOpenTable('ARHIST', 'ARHIST', 'SH')
ENDIF  
*C202243,1 Es 01/16/2018 'T20181228.0004'[End]


SELECT INVHDR
SCAN FOR &lcRpExp AND &lcDivsion AND IIF(llRpExZeroTax, (Tax_Amt <> 0), .T.)
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

  WAIT WINDOW LANG_Arsjour_ColDataMsg + Invoice NOWAIT

  SCATTER MEMVAR MEMO
  m.cTempKey = PADR(CUSTOMER.CADDRESS6,20)+PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))  
  m.ReltInv = INVHDR.Invoice
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
 
  *C202243,1 Es 01/16/2018 'T20181228.0004'[Start]
  =gfSeek(m.Account , 'CREDIT')
  SELECT Credit 
  LOCATE resT WHILE  Account+TRAN+DTOS(TRANDATE) =m.Account FOR TranType ='4' AND ALLTRIM(Reference) = m.Order
  IF FOUND()
  	m.Csh_Rec_no= Credit.Tran
  ELSE
    =gfSeek(m.Account,'ARHIST')
    SELECT ARHIST
    LOCATE REST WHILE ACCOUNT+HISTORY = m.Account FOR TranType ='4' AND ALLTRIM(Reference) = m.Order
    IF FOUND()
      m.Csh_Rec_no= Credit.Tran      
    ELSE
      m.Csh_Rec_no= ''
    ENDIF	
  Endif
  *C202243,1 Es 01/16/2018 'T20181228.0004'[End]
  
  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN


*- Collect Credit Memo data [Start]
IF !USED('RETHDR')
  =gfOpenTable('RETHDR', 'RETHDR', 'SH')   && CWARECODE
ENDIF

SELECT RETHDR
=gfSeek('')
REPLACE Status WITH 'C' FOR EMPTY(Status)
SET ORDER TO   && Activate rushmore optimizing.
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
SELECT RETHDR
LOCAL lcCrExp
lcCrExp = STRTRAN(lcRpExp, 'INVDATE', 'CRDATE')
SCAN FOR &lcCrExp AND &lcDivsion AND IIF(llRpExZeroTax, (Tax_Amt <> 0), .T.) 
  IF !EMPTY(lcAcctCursor) AND !SEEK(Account, lcAcctCursor)
    LOOP
  ENDIF
  IF !EMPTY(lcCurrCursor) AND !SEEK(cCurrCode, lcCurrCursor)
    LOOP
  ENDIF
  IF !EMPTY(lcRep1Cursor) AND !SEEK(SalesRep1, lcRep1Cursor)
    LOOP
  ENDIF
  IF !EMPTY(lcDivCursor)
    IF !SEEK(CDIVISION,lcDivCursor)
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

  WAIT WINDOW LANG_Arsjour_ColDataMsg + CrMemo NOWAIT

  SCATTER MEMVAR MEMO
  m.Invoice   = RETHDR.CrMemo
  m.ReltInv   = RETHDR.Invoice
  m.Ship      = -1 * RETHDR.Pieces
  m.VShip     = -1 * RETHDR.VPieces
  m.ShipAmt   = -1 * RETHDR.Amount
  m.VShipAmt  = -1 * RETHDR.VAmount
  m.nCharges  = -1 * RETHDR.Other
  m.nVCharges = -1 * RETHDR.VOther
  m.Discount  = -1 * RETHDR.Disc_Amt
  m.VDiscount = -1 * RETHDR.VDisc_Amt
  m.Tax_Amt   = -1 * IIF(STATUS = 'V', 0, RETHDR.Tax_Amt)  &&&&&&
  m.VTax_Amt  = -1 * IIF(STATUS = 'V', RETHDR.Tax_Amt, 0)  &&&&&&
  m.TotalChg  = -1 * RETHDR.TotCredit
  m.VTotalChg = -1 * RETHDR.VTotCredit
  m.Status    = IIF(EMPTY(RETHDR.Status), 'C', RETHDR.Status)
  m.Rep1      = RETHDR.SalesRep1
  m.Rep2      = RETHDR.SalesRep2
  m.InvDate   = RETHDR.crDate
  *x 
*  M.TAX_RATE = IIF(RETHDR.Tax_Amt <> 0 and RETHDR.TotCredit<>0,Round((100 * RETHDR.Tax_Amt)/RETHDR.TotCredit,3),0)
  M.TAX_RATE = IIF(RETHDR.Tax_Amt <> 0 and RETHDR.Amount<>0,Round((100 * RETHDR.Tax_Amt)/RETHDR.Amount,3),0)
  *x
  *CommAmt1, VCommAmt1, CommAmt2, VCommAmt2, nPstAmt, nVPstAmt, nHstAmt, nVHstAmt has the same name

  LOCAL lnAlias, lcKey
  lnAlias = SELECT(0)
  IF !EMPTY(RETHDR.Invoice) AND gfSeek(RETHDR.Invoice, 'INVHDR', 'INVHDR')
    SELECT Customer
    lcKey = EVALUATE(KEY())
    =gfSeek(IIF(EMPTY(INVHDR.Store),'M','S') + INVHDR.Account + INVHDR.Store,'Customer')
  ENDIF
  
  m.cTempKey = PADR(CUSTOMER.CADDRESS6,20)+PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))
  IF !EMPTY(lcKey)
    =gfSeek(lcKey, 'Customer')
  ENDIF
  SELECT (lnAlias)
  IF lcRpDateTP = "P"
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND;
                                      !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))
      m.Ship      = -1 * m.Ship
      m.VShip     = -1 * m.VShip
      m.ShipAmt   = -1 * m.ShipAmt
      m.VShipAmt  = -1 * m.VShipAmt
      m.nCharges  = -1 * m.nCharges
      m.nVCharges = -1 * m.nVCharges
      m.Discount  = -1 * m.Discount
      m.VDiscount = -1 * m.VDiscount
      m.Tax_Amt   = -1 * m.Tax_Amt
      m.VTax_Amt  = -1 * m.VTax_Amt
      m.TotalChg  = -1 * m.TotalChg
      m.VTotalChg = -1 * m.VTotalChg
    ENDIF
  ELSE
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                                     !BETWEEN(INVDATE,ldStrtDate,ldEndDate))
      m.Ship      = -1 * m.Ship
      m.VShip     = -1 * m.VShip
      m.ShipAmt   = -1 * m.ShipAmt
      m.VShipAmt  = -1 * m.VShipAmt
      m.nCharges  = -1 * m.nCharges
      m.nVCharges = -1 * m.nVCharges
      m.Discount  = -1 * m.Discount
      m.VDiscount = -1 * m.VDiscount
      m.Tax_Amt   = -1 * m.Tax_Amt
      m.VTax_Amt  = -1 * m.VTax_Amt
      m.TotalChg  = -1 * m.TotalChg
      m.VTotalChg = -1 * m.VTotalChg
    ENDIF
  ENDIF

  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN
*- Collect Credit Memo data [End]

ENDFUNC && lfCollData


*!*************************************************************
*! Name      : lfGetOpExp
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Make optimized expression then Collecting data
*!             from InvHdr file into Work file.
*!*************************************************************
FUNCTION lfGetOpExp
lcOptimize = ''
PRIVATE lcAlias
 
lnAccPos = CEILING(ASCAN(loOGScroll.laOGVrFlt,'INVHDR.ACCOUNT')/7) && Position of Account in variable filter.
*-- if you find [Account is <In List>] in the variable filter section.
IF lnAccPos != 0
  llOpLogic  = loOGScroll.laOGVrFlt[lnAccPos,4]
  lcAccOp    = loOGScroll.laOGVrFlt[lnAccPos,5]
  *-- Modify the Optimized expression due to change the Account Screen From INLIST into INRANGE [Begin]
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

    ENDIF  

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
    ENDIF  

  ENDIF  

ENDIF  

*-- When there is no optimize expression [user delete Account from OG] or does not assign any values for account filter,
*-- INVOICE Index is faster than ACCOUNT+INVOICE Index in collecting data.
lcOptimize = IIF(EMPTY(lcOptimize),[INVOICE = ''],lcOptimize)

ENDFUNC && lfGetOpExp

*!*************************************************************
*! Name      : lfMakeOpt
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Make mask for optimize expression if operator
*!           : is either BETWEEN or INLIST.
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

ENDFUNC && lfMakeOpt


*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 65                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.

ENDFUNC && lfvOptMsg


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : When function to get the Old value
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

ENDFUNC && lfwOldVal


*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : 1- Put both index and group expressions for all sort cases.
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
    lcGrpFoot  = [LANG_Arsjour_Account+'# ' + ACCOUNT + " - " + ALLTRIM(CUSTOMER.BTNAME)]

  CASE lcRpSortBy = 'L'  && Sort by location
    IF llSortGrp
      lcIndexTg  = [CWARECODE + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [CWARECODE + ACCOUNT + INVOICE]
    ENDIF    

    lcSubTitle = [Location]
    lcGroup    = [CWARECODE]
    lcGrpFoot  = [LANG_Arsjour_Location+'# ' + cWareCode + " - " + ALLTRIM(WAREHOUS.CDESC)]

  CASE lcRpSortBy = 'R'  && Sort by primary sales rep.
    IF llSortGrp
      lcIndexTg  = [REP1 + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [REP1 + ACCOUNT + INVOICE]
    ENDIF
    
    lcSubTitle = [Primary Rep]
    lcGroup    = [REP1]
    lcGrpFoot  = [LANG_Arsjour_SalesRep+'# ' + Rep1 + " - " + ALLTRIM(SALESREP.NAME)]

  CASE lcRpSortBy = 'C'  && Sort by country  
    IF llSortGrp
      lcIndexTg  = [LEFT(cTempKey,20) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [LEFT(cTempKey,20) + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [Country]
    lcGroup    = [LEFT(cTempKey,20)]
    lcGrpFoot  = [LANG_Arsjour_Country+':  ' + ALLTRIM(CUSTOMER.cAddress6)]

  CASE lcRpSortBy = 'S'  && Sort by state
    IF llSortGrp
      lcIndexTg  = [SUBSTR(cTempKey,21,36)+ CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [SUBSTR(cTempKey,21,36) + ACCOUNT + INVOICE]
    ENDIF
    
    lcSubTitle = SUBSTR(lcSTitle,1,8)
    lcGroup    = [SUBSTR(cTempKey,21,36)]
    *-- Get lcGrpFoot
    IF !USED('SYCINT')
      =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')   
    ENDIF
    lcSelectCommand3=[SELECT CCONT_CODE,CPART4LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=']+CUSTOMER.CCONT_CODE+[']
    lnResult3= oAriaApplication.remotesystemdata.execute(lcSelectCommand3,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 

    IF lnResult3 >= 1  
      lcGrpFoot= [LANG_Arsjour_Country+': ' + LEFT(cTempKey,20) + '  ' +;
                  IIF(!EMPTY(ALLTRIM(SYCINT.CPART4LAB)), ALLTRIM(SYCINT.CPART4LAB) + ': ','') +;
                  ALLTRIM(SUBSTR(cTempKey,27,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,27,30)),'STATE')]
    ELSE
      lcGrpFoot= [LANG_Arsjour_Country+': ' + LEFT(cTempKey,20) + '  ' +ALLTRIM(SUBSTR(cTempKey,27,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,27,30)),'STATE')]       
    ENDIF 

  CASE lcRpSortBy = 'G'  && Sort by region
    IF llSortGrp
      lcIndexTg  = [RIGHT(cTempKey,6) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [RIGHT(cTempKey,6) + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [Region]
    lcGroup    = [RIGHT(cTempKey,6)]
    lcGrpFoot  = [LANG_Arsjour_Region+': ' + gfCodDes(RIGHT(cTempKey,6),'REGION')]

  CASE lcRpSortBy = 'D'  && Sort by division
    IF llSortGrp
      lcIndexTg  = [cDIVISION + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cDIVISION + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [Division]
    lcGroup    = [cDIVISION]
    lcGrpFoot  = [LANG_Arsjour_Division+'# ' + cDIVISION + '  ' + gfCodDes(cDIVISION,'CDIVISION')]

  CASE lcRpSortBy = 'T'  && Sort by terms
    IF llSortGrp
      lcIndexTg  = [cTermCode + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cTermCode + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [P. Terms]
    lcGroup    = [cTermCode]
    lcGrpFoot  = [LANG_Arsjour_Terms+':  ' + gfCodDes(cTermCode,'CTERMCODE')]

  CASE lcRpSortBy = 'U'  && Sort by currency
    lcIndexTg  = [CCURRCODE + ACCOUNT + INVOICE]
    lcSubTitle = [Currency]
    
    lcGroup    = [CCURRCODE]
    lcGrpFoot  = [LANG_Arsjour_Currency+'# ' + CCURRCODE + ' - ' + SYCCURR.cCurrDesc]
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

ENDFUNC && lfGetRepVr


*!*************************************************************
*! Name      : lfvChFact
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : 1- Change Factored/Non Factored logical variable To recollect data.
*!*************************************************************
FUNCTION lfvChFact

llChFactor = .T.

ENDFUNC && lfvChFact


*!*************************************************************
*! Name      : lfvList
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : 1- Change Invoices/Void Invoices logical variable To recollect data.
*!           : 2- Enable and disable Invoice date or Void date due to user selection.
*!*************************************************************
FUNCTION lfvList

llChInv = .T.  && Logical invoice variable.
llVoidOnly = (lcRpList='V')  && To use it in .FRX

ENDFUNC && lfvList


*!***********************************************************************************
*! Name      : lfvCurDisp
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Activate currency display screen to get user selection for currencies.
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
= lfGetRepVr()

ENDFUNC && lfvCurDisp


*!*************************************************************
*! Name      : lfvCurr
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Validate Currency code in SYCCURR file.
*!*************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp

lcVar = OGSYS18()                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(OGSYS18())      && Varible to hold the current field value

*-- IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SYCCURR'))

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = [CCURRCODE:H= LANG_Arsjour_Currency ,    ;
                CCURRSMBL:H= LANG_Arsjour_Symbol ,      ;
                CCURRDESC:H= LANG_Arsjour_Description , ;
                NCURRUNIT:H= LANG_Arsjour_Units ]

  lcFile_Ttl = LANG_Arsjour_Currencies
  = gfBrows('','CCURRCODE','laTemp')

  *-- IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF

ENDIF
&lcVar = lcObj   && Update the field

ENDFUNC && lfvCurr


*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Rise change account flag, in range browse screen.
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

ENDFUNC && lfsrAcc


*!*************************************************************
*! Name      : lfCollTime
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

ENDFUNC && lfCollTime


*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Create cursor
*!*************************************************************
FUNCTION lfCreatCur
PARAMETERS lcCurName

gfCrtTmp(lcCurName,@laTempStru,"lcIndexTg",lcWorkFile,.F.)

ENDFUNC && lfCreatCur


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Function that we call when Close the option grid.
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
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Pre_Preview Function To call Temporary .FRX again
*!           : to evaluate #OBJDISP objects again.
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!           : 2- While this function has one line of code calls another function
*!           :    I write it for other reasons and for any other to add any
*!           :    enhancement code.
*!*************************************************************
FUNCTION lfPreRun
= lfGetRepVr()      && Get Report variables such as groups and index.
RETURN .T.
*-- End of lfPreRun.


*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Fill Sort Array
*!*************************************************************
*-- Fill Sort Arrays.
FUNCTION lfSortDumy
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 8
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDesc[lnArrElmnt,1],laSortVal[lnArrElmnt,1]
laSortDesc[1] = LANG_Arsjour_Invoice
laSortDesc[2] = LANG_Arsjour_Account
laSortDesc[3] = LANG_Arsjour_SalRepresent
laSortDesc[4] = LANG_Arsjour_Country
laSortDesc[5] = lcSTitle       && State variable Title
laSortDesc[6] = LANG_Arsjour_Region
laSortDesc[7] = LANG_Arsjour_Division
laSortDesc[8] = LANG_Arsjour_PaymentTerms

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
  laSortDesc[3] = LANG_Arsjour_Location

  =AINS(laSortVal,3)
  laSortVal[3] = 'L'
ENDIF

IF llMultCurr
  laSortDesc[ALEN(laSortDesc,1)] = LANG_Arsjour_Currency 
  laSortVal[ALEN(laSortDesc,1)]  = 'U'
ENDIF
*-- End of lfSortDumy.


*!*************************************************************
*! Name      : lfItmPos
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Evaluate fixed filter position within array.
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
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : -Ve Void values
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
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : To get the data from the store fields.
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
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Fill second sort bu option
*!**************************************************************************
FUNCTION lfSortDum2
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 9
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDes2[lnArrElmnt,1],laSortVal2[lnArrElmnt,1]
laSortDes2[1] = LANG_Arsjour_None
laSortDes2[2] = LANG_Arsjour_Invoice 
laSortDes2[3] = LANG_Arsjour_Account
laSortDes2[4] = LANG_Arsjour_SalRepresent
laSortDes2[5] = LANG_Arsjour_Country  
laSortDes2[6] = lcSTitle       && State variable Title
laSortDes2[7] = LANG_Arsjour_Region
laSortDes2[8] = LANG_Arsjour_Division
laSortDes2[9] = LANG_Arsjour_PaymentTerms

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
  laSortDes2[4] = LANG_Arsjour_Location

  =AINS(laSortVal2,4)
  laSortVal2[4] = 'L'
ENDIF

IF llMultCurr
  laSortDes2[ALEN(laSortDes2,1)] = LANG_Arsjour_Currency
  laSortVal2[ALEN(laSortDes2,1)]  = 'U'
ENDIF
*-- End of lfSortDum2.


*!**************************************************************************
*! Name      : lfvSort
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Valid function for both 1st , 2nd Sort by option.
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
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Enable/Disable all options in OG 
*!***************************************************************************
PROCEDURE lpShowObj
PRIVATE llShwInRng

IF lnSortPos > 0
  IF lcRpSortBy = 'I'
    lcRpSort2 = 'N'
*    llShwInRng = .F.
*    loOGScroll.laOGObjCnt[lnSortPos] = .F.
  ELSE 
*    llShwInRng = .T.
*    loOGScroll.laOGObjCnt[lnSortPos] = .T.
  ENDIF 
*  =lfOGShowGet('lcRpSort2')  && Show get Object .
  loOGScroll.EnableObject('lcRpSort2',lcRpSortBy <> 'I')
ENDIF 
*-- End of lShowObj.


*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Generate Expression.
*!***************************************************************************
*Function to add the void expr. after the invoice date expr.
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX , lcInvExp , lcVoidExp

lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1]
=ACOPY(loOGScroll.laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.

*-- Define new Fixed filter array to hold one expression only.
DIMENSION loOGScroll.laOGFxFlt[1,7]
*ssh
*loOGScroll.laOGFxFlt = ""

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
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Function used to suppressing the field in the grid.
*!*************************************************************
* Refresh the option grid in case the posted date.[START]
FUNCTION lfClrRead
ClearRead()
* Refresh the option grid in case the posted date.[END]
*-- End of lfClrRead.


*!*************************************************************
*! Name      : lfOGTempCurs
*! Developer : Saber A Razek (SAB) 
*! Date      : 06/25/2012
*! Purpose   : Function used to create Temp Cursor from OG Expr.
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