*:***************************************************************************
*: Program file  : RMCMRET.PRG
*: Program desc. : CREDIT MEMO FOR RETURNS.
*: Date          : 14/01/2003
*: System        : Aria Advantage Series.
*: Module        : RETURN MERCHANDISE (RM)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: E302103
*:
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO RMCMEM
*:***************************************************************************
*: Modification:
*:B607314,1 ALB 06/15/2003 Adjust the FRX to void problem with A4 paper size.
*:B123885,1 NNA 09/21/2004 Fix bug that when you Choose to display the Active and Voided Credit Memo
*:B123885,1 NNA            you will find a diffrence in the Tax_Amt between the Summary and the Detail
*:B123885,1 NNA            Form because in the summary Form it Sum the Tax Amount for the Voided Credit
*:B126261,1 NNA 01/26/2005 Fix bug that when you Choose to display the Active and Voided Credit Memo
*:B126261,1 NNA            with the Equivalent Mode you will find a diffrence in the Tax_Amt between 
*:B126261,1 NNA            the Summary and the Detail Form because in the summary Form it Sum the 
*:B126261,1 NNA            Tax Amount for the Voided Credit
*:B607797,1 TMI 10/08/2006 Make report width 130 chars only , frx changed also (T20060831.0032)
*:E302378,1 TMI 03/29/2007 1) Add state filter  ( T20070215.0014 )
*:E302378,1 TMI            2) Add sort by state , Tran. Date
*****************************************************************************

lcStTime   = TIME()    && Time in which we start collect data.
STORE '' TO lcGrpBY    && hold group variable
STORE '' TO lcGrpFoot  && hold group footer
STORE '' TO ldStrtDate , ldEndDate

*--Fill the variables of the posted date.
llPostDate = .F.
IF IIF(ALLTRIM(gfGetMemVar('M_LINK_GL')) = 'Y', .T. , .F.)
  STORE {  /  /  } TO ldStrtDPst , ldEndDPst
  lnPostDate = lfItmPos('DPOSTDATE')
  IF !EMPTY(laOGFxFlt[lnPostDate,6])
    ldStrtDPst = CTOD(PADR(laOGFxFlt[lnPostDate,6],ATC('|',laOGFxFlt[lnPostDate,6])+1))
    ldEndDPst  = CTOD(SUBSTR(laOGFxFlt[lnPostDate,6],ATC('|',laOGFxFlt[lnPostDate,6])+1))
    llPostDate = .T.
  ENDIF
ENDIF

*--get group variable and group footer.
=lfGetGrp()

IF USED('SYCINT')
  SET ORDER TO TAG Ccontcode IN SYCINT
ENDIF
llVoidOnly = (lcRpStatus = "V")

*-- Include void invoices amount and ship amount if upper invoice date is less than [Begin]
llDateRang = .F.
lnCRPos    = lfItmPos('RETHDR.CRDATE')
IF !EMPTY(laOGFxFlt[lnCRPos,6])
  ldStrtDate = CTOD(PADR(laOGFxFlt[lnCRPos,6],ATC('|',laOGFxFlt[lnCRPos,6])+1))
  ldEndDate  = CTOD(SUBSTR(laOGFxFlt[lnCRPos,6],ATC('|',laOGFxFlt[lnCRPos,6])+1))
  IF EMPTY(ldStrtDate) AND EMPTY(ldEndDate)
    STORE '' TO ldStrtDate , ldEndDate
  ELSE
    llDateRang = .T.  
  ENDIF
ENDIF

IF llDateRang
  *-- Add Void between to Range.
  lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(CRDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(CRDATE,ldStrtDate,ldEndDate))))]
ELSE
  lcVoidExpr = [llVoidOnly]
ENDIF

*-- if user change last filter .
IF llOGFltCh
  llClearFn  = .F.
  llChStatus = .F.
  lcLastTag  = ''     && to fill index field with its corresponding data.
  *-- if you have previous data clear workfile then recreate it. 
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    =lfWorkFile()
  ENDIF
  STORE '' TO lcHiddFilt , lcFixdFilt , lcVarbFilt
  IF !EMPTY(laOGHdFlt[1,1])
    lcHiddFilt = gfGenFlt('laOGHdFlt',.T.)
  ENDIF
  IF !EMPTY(laOGVrFlt[1,1])
    lcVarbFilt = gfGenFlt('laOGVrFlt',.T.)
  ENDIF
  IF llDateRang
    DO CASE 
      CASE lcRpStatus = "V"
        lcFixdFilt = [Status = "V" AND BETWEEN(VDATE,ldStrtDate,ldEndDate)]
      CASE EMPTY(lcRpStatus)
        lcFixdFilt = [Status = " " AND BETWEEN(CRDATE,ldStrtDate,ldEndDate)]
      OTHERWISE
        lcFixdFilt = [(BETWEEN(CRDATE,ldStrtDate,ldEndDate) OR ] +;
                     [BETWEEN(VDATE,ldStrtDate,ldEndDate))]
    ENDCASE
  ELSE
    DO CASE 
      CASE lcRpStatus = "V"
        lcFixdFilt = [Status = "V"]
      CASE EMPTY(lcRpStatus)
        lcFixdFilt = [EMPTY(Status)]
    ENDCASE
  ENDIF
  lnCurrPos  = lfItmPos('RETHDR.CCURRCODE')
  IF !EMPTY(laOGFxFlt[lnCurrPos,6])
    IF !EMPTY(lcFixdFilt)
      lcFixdFilt = lcFixdFilt + [ AND ]
    ENDIF
    lcFixdFilt = lcFixdFilt + "(cCurrCode $ laOGFxFlt[lnCurrPos,6])"
  ENDIF

  *--To add the posting date to the filter.
  IF llPostDate
    IF !EMPTY(lcFixdFilt)
      lcFixdFilt = lcFixdFilt + [ AND ]
    ENDIF
    lcFixdFilt = lcFixdFilt + "BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst)"
  ENDIF

  lcRepExpr = ALLTRIM(lcHiddFilt)
  IF !EMPTY(lcRepExpr) AND !EMPTY(lcVarbFilt)
    lcRepExpr = lcRepExpr + [ AND ]
  ENDIF
  lcRepExpr = lcRepExpr + ALLTRIM(lcVarbFilt)
  IF !EMPTY(lcRepExpr) AND !EMPTY(lcFixdFilt)
    lcRepExpr = lcRepExpr + [ AND ]
  ENDIF
  lcRepExpr = lcRepExpr + ALLTRIM(lcFixdFilt)
  lcRepExpr = STRTRAN(lcRepExpr,"RETHDR.","")
  IF !(EMPTY(lcRepExpr) OR ("CRMEMO"$lcRepExpr))
    lcRepExpr = [AND ] + lcRepExpr
  ENDIF
  IF LEFT(lcRepExpr,3) = "AND"
    lcRepExpr = [CrMemo = '' ] + lcRepExpr
  ENDIF

  *-- Control report expression [Begin]
  *-- Scan to fill Temp. File with filtered data.
  lcRepExpr = IIF(EMPTY(lcRepExpr),'.T.',lcRepExpr)
  DO CASE
    CASE lcRpFormat = "D"
      =lfGetDetal()
    CASE lcRpFormat = "S" 
      IF llMultCurr AND lcRpCurr<>"F" 
        =lfGetSumry()
      ELSE
        =lfGetSumfg()
      ENDIF
  ENDCASE
ENDIF  && end if user change last filter.

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SELECT (lcWorkFile)
*-- ReIndex work file if first time collect data or user change sort By.
IF !(lcRpIndTag == lcLastTag)
  lcLastTag = lcRpIndTag
  REPLACE ALL cTempKey WITH EVALUATE(lcRpIndTag)
ENDIF

LOCATE
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER,SALESREP1 INTO SALESREP
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*-- Call Report [BEGIN]
IF llRpDec  && case print decimal
  lcHedLin1 = IIF(llTaxes,'','Order '+SPACE(1)) + 'Factor' + ' ' + 'Div   '  + ' ' + ;
              ' Pieces ' + '  Gross Amt ' + '  Discount ' + ' Merch. Amt ' + ;
              '     Other' + IIF(llTaxes,'    Tax Amt '+'       Total','     Total') 
   
ELSE   && case print no decimal
  *T20060831.0032 TMI [Start] 
  *lcHedLin1 = IIF(llTaxes,'','Order '+SPACE(1)) + 'Factor' + ' ' + 'Div   '  + ' ' + ;
            ' Pieces ' + '  Gross Amt' + ' Discount ' + 'Merch. Amt ' + ;
            '     Other    ' + IIF(llTaxes,'Tax Amt '+'       Total','     Total') 
  lcHedLin1 = IIF(llTaxes,'','Order '+SPACE(1)) + 'Factor' + ' ' + 'Div   '  + ' ' + ;
            ' Pieces ' + '  Gross Amt' + ' Discount ' + 'Merch. Amt ' + ;
            '     Other    ' + IIF(llTaxes,'Tax Amt '+'      Total','     Total') 
  *T20060831.0032 TMI [End  ] 
ENDIF

lnEmptyLen = IIF(llTaxes,0,7) + 14
lnDashLen  = 89 -   lnEmptyLen        && Length of calculated fields
lcDashed1  = SPACE(lnEmptyLen) + REPLICATE('-',lnDashLen+2)  && Group Line ['------']
lcDashed2  = SPACE(lnEmptyLen) + REPLICATE('=',lnDashLen)  && Grand Line ['======']

*-- Data printed on group band [begin]
IF llRpDec && case print decimal
  lcGrpLine = [SPACE(lnEmptyLen) +] + ;
              [TRAN(lnGrPieces,'9999999')+' '+;
              TRAN(lnGrGrsAmt,'99999999.99')+' '+;
              TRAN(-1 * lnGrDisc,'9999999.99')+' '+;
              TRAN(lnGrAmt,'99999999.99')+' '+;
              TRAN(lnGrOther,'9999999.99')+' ' +] +;
              [IIF(llTaxes,TRAN(lnGrTax,'9999999.99')+SPACE(1),'')+;
              TRAN(lnGrTotal,'999999999.99')+' ']

ELSE && case not print decimal
  lcGrpLine = [SPACE(lnEmptyLen) +] + ;
              [TRAN(lnGrPieces,'9999999')+SPACE(1)+;
              TRAN(lnGrGrsAmt,'99999999999')+SPACE(1)+;
              TRAN(-1 * lnGrDisc,'99999999')+SPACE(1)+;
              TRAN(lnGrAmt,'9999999999')+SPACE(1)+;
              TRAN(lnGrOther,'9999999999')+SPACE(1) +] +;
              [IIF(llTaxes,TRAN(lnGrTax,'9999999999')+SPACE(1),SPACE(11))+;
              TRAN(lnGrTotal,'99999999999')+' ']
ENDIF 
*-- Data printed on group band [end  ]
*-- Data printed on summary band (Grand data) [begin]
IF llRpDec && case print decimal
  lcGndLine =  [SPACE(lnEmptyLen) + ] + ;
               [TRAN(lnGdPieces,'9999999')+' '+;
               TRAN(lnGdGrsAmt,'99999999.99')+' '+;
               TRAN(-1 * lnGdDisc,'9999999.99')+' '+;
               TRAN(lnGdAmt,'99999999.99')+' '+;
               TRAN(lnGdOther,'9999999.99')+' ' +] +;
               [IIF(llTaxes,TRAN(lnGdTax,'9999999.99')+'  ',' ')+;
               TRAN(lnGdTotal,'999999999.99')+' ']

ELSE && case not print decimal
  lcGndLine =  [SPACE(lnEmptyLen) +] + ;
               [TRAN(lnGdPieces,'9999999')+SPACE(1)+;
               TRAN(lnGdGrsAmt,'99999999999')+SPACE(1)+;
               TRAN(-1 * lnGdDisc,'99999999')+ SPACE(1)+;
               TRAN(lnGdAmt,'99999999999')+ SPACE(1)+;
               TRAN(lnGdOther,'9999999999')+SPACE(1)+] +;
               [IIF(llTaxes,TRAN(lnGdTax,'9999999999')+SPACE(1),SPACE(11))+;
               TRAN(lnGdTotal,'99999999999')+' ']
ENDIF
*-- Data printed on summary band (Grand data) [end  ]
*-- lcDetLine : Data printed on detail line.
*-- Note that :in the following lines TRAN is TRANSFORM function but 
*--            because line in fox has specific length I forced to write TRAN.
IF llRpDec && case print decimal
  lcDLine2D = [TRAN(PIECES,'9999999')+' '       +;
              TRAN(GROSS_AMT,'99999999.99')+' ' +;
              TRAN(-1 * DISC_AMT,'9999999.99')+' '  +;
              TRAN(AMOUNT,'99999999.99')+' '   +;
              TRAN(OTHER,'9999999.99')+' '+]

  lcDLine2V = [TRAN(VPIECES,'9999999')+' '      +;
              TRAN(VGROSS_AMT,'9999999.99')+' '+;
              TRAN(-1 * VDISC_AMT,'9999999.99')+' ' +;
              TRAN(VAMOUNT,'99999999.99')+' '  +;
              TRAN(VOTHER,'9999999.99')+' ' +]
ELSE && case not print decimal
  lcDLine2D = [TRAN(PIECES,'9999999')+SPACE(1)+;
              TRAN(GROSS_AMT,'99999999999')+space(1)+;
              TRAN(-1 * DISC_AMT,'99999999 ')+;
              TRAN(AMOUNT,'9999999999')+space(1)+;
              TRAN(OTHER,'9999999999')+' '+]

  lcDLine2V =  [TRAN(VPIECES,'9999999')+' '      +;
               TRAN(VGROSS_AMT,'99999999999')+' '+;
               TRAN(-1 * VDISC_AMT,'9999999')+SPACE(1) +;
               TRAN(VAMOUNT,'99999999999')+SPACE(1)  +;
               TRAN(VOTHER,'9999999999')+' ' +]
ENDIF

IF llRpDec && case print decimal
  lcDetLineV = [IIF(llTaxes,'',Order+SPACE(1)) +] + [cFacCode + ' '+cDivision+' ' +] + lcDLine2V +;
               [IIF(llTaxes,TRAN(TAX_AMT,'9999999.99')+' ','')                                  +;
               TRAN(VTOTCREDIT,'99999999.99')+' ']
  
   lcDetLineD = [IIF(llTaxes,'',Order+SPACE(1)) +] + [cFacCode + ' '+cDivision+' ' +] + lcDLine2D +;
                [IIF(llTaxes,TRAN(IIF(STATUS='V',0.00,TAX_AMT),'9999999.99')+'  ',' ')+;
                TRAN(TOTCREDIT,'99999999.99')+' ']            

  lcDetLine  =  [IIF(EVALUATE(lcVoidExpr),EVALUATE(lcDetLineV),EVALUATE(lcDetLineD))]

ELSE   && case not printing decimal
  lcDetLineV = [IIF(llTaxes,'',Order+SPACE(1)) +] + [cFacCode + ' '+cDivision+' ' +] + lcDLine2V +;
               [IIF(llTaxes,TRAN(TAX_AMT,'9999999999')+'  ',' ')                                 +;
               TRAN(VTOTCREDIT,'99999999999')+' '] 
    
  lcDetLineD = [IIF(llTaxes,'',Order+SPACE(1)) +] + [cFacCode + ' '+cDivision+' ' +] + lcDLine2D +;
               [IIF(llTaxes,TRAN(IIF(STATUS='V',0.00,TAX_AMT),'9999999999'),' ')+;
               TRAN(TOTCREDIT,'999999999999')+' ']

  lcDetLine  =  [IIF(EVALUATE(lcVoidExpr),EVALUATE(lcDetLineV),EVALUATE(lcDetLineD))]
ENDIF

DO gfDispRe WITH EVAL('lcRpForm')
SET RELATION TO

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfEvalVars
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Fill Default values used in both OG and Report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfGetMemVar,gfOpenFile,lfvSortBy
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEvalVars()
*!*************************************************************
FUNCTION lfEvalVars

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
llTaxes     = gfGetMemVar('M_TAX') = 'Y'  && .T., if company use taxes.
llCanada    = 'CAN' $ ALLTRIM(UPPER(gcContCode))  && Country is canada.

*-- if multi currency evaluate currency arrays [Begin]
IF llMultCurr
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF
  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*-- if multi currency evaluate currency arrays [Begin]
*-- Fill default sort options... [Begin]
lcRpSortBy = 'C'
=lfvSortBy()
*-- Fill default sort options... [End]

*--End of lfSortDumy.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- if it's first time to run the report.
IF TYPE('lcLastTag') = 'N'
  R_WIDTH='W'
  SET ORDER TO CUSTOMER IN CUSTOMER
  SET ORDER TO Codes    IN CODES   
  SET ORDER TO RETHDR   IN RETHDR
  SET ORDER TO Salesrep IN Salesrep
  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT RETHDR
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 11
  laTempStru[ALEN(laTempStru,1)  ,4] = 0
  *E302378,1 TMI [Start] Add the state field to the temp file
  lnLen = ALEN(laTempStru,1) + 1
  DIMENSION laTempStru[lnLen,4]
  laTempStru[lnLen,1] = 'STATE'
  laTempStru[lnLen,2] = 'C'
  laTempStru[lnLen,3] = 6
  laTempStru[lnLen,4] = 0
  *E302378,1 TMI [End  ] 
  =lfWorkFile()

  *E302378,1 TMI [Start] Fill the states array 
  DIMENSION laRpSStats[1],laRpTStats[1]
  lcRpStates = ''
  
  SELECT CCODE_NO + '-'+CDISCREP ;
    FROM CODES ;
    WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNSTATE  ' ;
    ORDER BY CCODE_NO ;
    INTO ARRAY laRpSStats
  *E302378,1 TMI [End  ] 

ENDIF  && END IF you first time enter when function.

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfWorkFile
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Create work cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWorkFile()
*!*************************************************************
FUNCTION lfWorkFile

CREATE CURSOR (lcWorkFile) FROM ARRAY laTempStru
SELECT (lcWorkFile)
*AMH - Fix bug invalid path or file name [Start]
*INDEX ON cTempKey TAG (lcWorkFile)
INDEX ON cTempKey TAG (lcWorkFile) OF (lcWorkFile)
*AMH - [End]

DO CASE
  CASE lcRpSortBy = "C"
    *AMH - Fix bug invalid path or file name [Start]
    *INDEX ON cCurrcode TAG "TempIndx"
    INDEX ON cCurrcode TAG "TempIndx" OF (lcWorkFile)
    *AMH - [End]
  CASE lcRpSortBy = "A"
    *AMH - Fix bug invalid path or file name [Start]
    *INDEX ON ACCOUNT+cCurrcode TAG "TempIndx" 
    INDEX ON ACCOUNT+cCurrcode TAG "TempIndx"  OF (lcWorkFile)
    *AMH - [End]
  CASE lcRpSortBy = "S"
    *AMH - Fix bug invalid path or file name [Start]
    *INDEX ON SALESREP1+cCurrcode TAG "TempIndx" 
    INDEX ON SALESREP1+cCurrcode TAG "TempIndx" OF (lcWorkFile)
    *AMH - [End]
  *E302378,1 TMI [Start] Add two sorting by State / Transaction Date
  CASE lcRpSortBy = "T"
    INDEX ON STATE+cCurrcode TAG "TempIndx" OF (lcWorkFile)
  CASE lcRpSortBy = "D"
    INDEX ON DTOS(CRDATE)+cCurrcode TAG "TempIndx" OF (lcWorkFile)
  *E302378,1 TMI [End  ] 
ENDCASE
SET ORDER TO TAG (lcWorkFile)

*--End of lfWorkFile.
*!*************************************************************
*! Name      : lfvSortBy
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Rise change index flag to reindex temp cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid, lfEvalVars
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy

DO CASE
  CASE lcRpSortBy = 'C'		&& Sort by Credit Memo Case
    lcRpIndTag = [CRMEMO]
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcRpIndTag = [ccurrcode+CRMEMO]
    ENDIF
  CASE lcRpSortBy = 'A'		&& Sort by Account Case
    lcRpIndTag = [ACCOUNT + CRMEMO]
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcRpIndTag = [ACCOUNT +ccurrcode+CRMEMO]
    ENDIF
  CASE lcRpSortBy = 'S'		&& Sort by Primary Sales Rep. Case
    lcRpIndTag = [SALESREP1 + CRMEMO]
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcRpIndTag = [SALESREP1 +ccurrcode+ CRMEMO]
    ENDIF
  *E302378,1 TMI [Start] Add two sorting by State / Transaction Date
  CASE lcRpSortBy = 'T'		&& Sort by State
    lcRpIndTag = [STATE + CRMEMO]
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcRpIndTag = [STATE +ccurrcode+ CRMEMO]
    ENDIF
  
  CASE lcRpSortBy = 'D'		&& Sort by Transaction Date
    lcRpIndTag = [DTOS(CRDATE) + CRMEMO]
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcRpIndTag = [DTOS(CRDATE) +ccurrcode+ CRMEMO]
    ENDIF
  *E302378,1 TMI [End  ] 
ENDCASE

*E302378,1 TMI [Start] recollect data if sort by is changed
llOgFltCh = .T.
*E302378,1 TMI [End  ] 

*--End of lfvSortBy.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*--End of lfwOldVal.
*!*************************************************************
*! Name      : lfvAcc
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Validate function for the Customer Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc
PRIVATE lcItsName , lcItsVal , llObjRet
lcItsName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal  = EVALUATE(SYS(18))     && Varible to hold  the value of the current GET field

IF USED('SYCINT')
  SET ORDER TO TAG Ccontcode IN SYCINT
ENDIF

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF

*--End of lfvAcc.
*!*************************************************************
*! Name      : lfCollTime
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

*--End of lfCollTime.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

llOGFltCh = .T.
*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfvFormat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Valid function called when user change report format
*!*************************************************************
*! Called from : [Option Grid]
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFormat()
*!*************************************************************
FUNCTION lfvFormat

IF lcRpFormat = 'S'
  STORE .F. TO llRpNotes
ENDIF
IF lcOldVal!=lcRpFormat
  lcOldVal  = lcRpFormat
  llOGFltCh = .T.
ENDIF
CLEAR READ

*--End of lfvFormat.
*!*************************************************************
*! Name      : lfvCrMemo
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Validation function for the CrMemo field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCrMemo()
*!*************************************************************
FUNCTION lfvCrMemo
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*--IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND !SEEK(lcObj , 'RETHDR')))
  SELECT RETHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "CRMEMO    :R :H= 'CrMemo#' , "    +;
               "CRSTATUS= IIF(STATUS = 'V' , 'Voided' , 'Credit Memo') :R :H= 'STATUS' , " +;
               "PRINTED = IIF(Flag = 'Y' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "CRDATE    :R :H= 'Credit Date',"+;
               "VDATE     :R :H= 'Void Date',"+;
               "REASON    :R :H= 'Reason' ,"     +;
               "CDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "PIECES    :R :H= 'Pieces' ,"    +;
               "AMOUNT    :R :H= 'Amount ' ,"    +; 
               "OTHER     :R :H= 'Other ',"  +; 
               "TAX_AMT   :R :H= 'Tax Amount' "
  
  lcFile_Ttl = "Credit Memo ..."
  PRIVATE lcStatFlt
  lcStatFlt = ''
  DO CASE
    CASE EMPTY(lcRpStatus)
      lcStatFlt = [EMPTY(Status)]
    CASE lcRpStatus = "V"
      lcStatFlt = 'STATUS = "V"'
    OTHERWISE
      lcStatFlt = ''
  ENDCASE
  lcBrowCond = IIF(EMPTY(lcStatFlt),'',[FOR ] + lcStatFlt)
  = gfBrows(lcBrowCond,'CRMEMO','laTemp')
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
  
ENDIF
&lcVar = lcObj      && Update the field

*--End of lfvCrMemo.
*!*************************************************************
*! Name      : lfvRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Validation function for the sales rep. field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRep()
*!*************************************************************
FUNCTION lfvRep
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*--IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
  SELECT SALESREP
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
  
ENDIF
&lcVar = lcObj      && Update the field

*--End of lfvRep.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*--End of lfItmPos.
*!*************************************************************
*! Name      : lfNegValue
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : -Ve Void values
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfNegValue()
*!*************************************************************
FUNCTION lfNegValue
PRIVATE lnFldsCnt , lcMemField

lcMemField = ''
lnFldsCnt  = 0
FOR lnFldsCnt = 1 TO FCOUNT()
  IF TYPE(FIELD(lnFldsCnt)) = "N"
    lcMemField = "m." + FIELD(lnFldsCnt)
    &lcMemField = -1 * &lcMemField
  ENDIF
ENDFOR

*--End of lfNegValue.
*!*************************************************************
*! Name      : lfFillVars
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  

IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End]
ENDIF

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfCalAmt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : to compute some values in case forign currancy
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : .
*!*************************************************************
*! Example     : =lfCalAmt()
*!*************************************************************
FUNCTION lfCalAmt
*--lcOldArea to hold old area
*--lnRecNo to hold the current postion in old area
PRIVATE lcOldArea,lnRecNo
lcOldArea = SELECT()
lnRecNo   = RECNO()

IF M.GROSS_AMT !=0
  M.GROSS_AMT = gfAmntDisp(GROSS_AMT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")    
ENDIF

IF M.VGROSS_AMT !=0
  M.VGROSS_AMT = gfAmntDisp(M.VGROSS_AMT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.DISC_AMT !=0
  M.DISC_AMT = gfAmntDisp(M.DISC_AMT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.VDISC_AMT !=0
  M.VDISC_AMT = gfAmntDisp(M.VDISC_AMT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.AMOUNT !=0
  M.AMOUNT = gfAmntDisp(M.AMOUNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.VAMOUNT !=0
  M.VAMOUNT = gfAmntDisp(M.VAMOUNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.OTHER !=0
  M.OTHER = gfAmntDisp(M.OTHER,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.VOTHER !=0
  M.VOTHER = gfAmntDisp(M.VOTHER,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.TOTCREDIT !=0
  M.TOTCREDIT = gfAmntDisp(M.TOTCREDIT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.VTOTCREDIT !=0
  M.VTOTCREDIT = gfAmntDisp(M.VTOTCREDIT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.COMMAMT1 !=0
   M.COMMAMT1 = gfAmntDisp(M.COMMAMT1,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.VCOMMAMT1 !=0
  M.VCOMMAMT1 = gfAmntDisp(M.VCOMMAMT1,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.COMMAMT2 !=0
  M.COMMAMT2 = gfAmntDisp(M.COMMAMT2,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

IF M.VCOMMAMT2 !=0
  M.VCOMMAMT2 = gfAmntDisp(M.VCOMMAMT2,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF

*B126261,1 NNA 01/26/2005 (Begin) if the Credit Memo is void I make M.Tax_Amt = Zero
IF m.Status = 'V' .AND. !llVoidOnly
  STORE 0 TO M.Tax_Amt
ENDIF
*B126261,1 NNA (End)

IF M.TAX_AMT !=0
  M.TAX_AMT = gfAmntDisp(M.TAX_AMT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"RETHDR")
ENDIF  

SELECT(lcOldArea)
GOTO lnRecNo
RETURN

*--End function lfCalAmt
*!*************************************************************
*! Name      : lfGetGrp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : get group variable and group footer variable
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetGrp()
*!*************************************************************
FUNCTION lfGetGrp

DO CASE
  CASE lcRpSortBy = 'C'		&& Sort by Credit Memo Case
    STORE '' TO lcGrpBy
    STORE '' TO lcGrpFoot
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcGrpBy   = "ccurrcode"
      lcGrpFoot = " 'Currency.:' +'('+ ccurrcode+ ')' " 
    ENDIF
    IF llMultCurr AND lcRpCurr<>"F" AND  lcRpFormat="S" 
      STORE '' TO lcGrpFoot
    ENDIF
  CASE lcRpSortBy = 'A'		&& Sort by Account Case
    lcGrpBy    ="ACCOUNT"
    lcGrpFoot  =" 'Acct :'+ ACCOUNT +' '        +SUBSTR(CUSTOMER.BTNAME,1,10) "
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcGrpBy="ACCOUNT+ccurrcode"
      lcGrpFoot  =" 'Acct :'+ ACCOUNT +' ' +SUBSTR(CUSTOMER.BTNAME,1,10) +'('+ ccurrcode+ ')' "
    ENDIF
    IF llMultCurr AND lcRpCurr<>"F" AND  lcRpFormat="S" 
      STORE '' TO lcGrpFoot
    ENDIF 
  CASE lcRpSortBy = 'S'		&& Sort by Primary Sales Rep. Case
    lcGrpBy    ="SALESREP1"
    lcGrpFoot  =" 'Rep.: '+SALESREP1+' '+ SUBSTR(SALESREP.NAME,1,10) "
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcGrpBy="SALESREP1+ccurrcode"
      lcGrpFoot  =" 'Rep.: ' + SALESREP1 +' ' + SUBSTR(SALESREP.NAME,1,10) +'('+ ccurrcode+ ')' "
    ENDIF
    IF llMultCurr AND lcRpCurr<>"F" AND  lcRpFormat="S" 
      STORE '' TO lcGrpFoot
    ENDIF
    *E302378,1 TMI [Start] get group description for State and Trans.Date
  CASE lcRpSortBy = 'T'		&& Sort by State case
    lcGrpBy    ="STATE"
    lcGrpFoot  =" 'State: '+STATE+' '+ SUBSTR(gfCodDes(STATE,'STATE'),1,10) "
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcGrpBy="STATE+ccurrcode"
      lcGrpFoot  =" 'State: '+SUBSTR(STATE,1,3)+ SUBSTR(gfCodDes(STATE,'STATE'),1,10) +'('+ ccurrcode+ ')' "
    ENDIF
    IF llMultCurr AND lcRpCurr<>"F" AND  lcRpFormat="S" 
      STORE '' TO lcGrpFoot
    ENDIF
  CASE lcRpSortBy = 'D'		&& Sort by Primary Sales Rep. Case
    lcGrpBy    ="DTOS(CRDATE)"
    lcGrpFoot  =" 'Tran. Date: '+DTOC(CRDATE) "
    IF llMultCurr .OR. lcRpCurr<> "F"  
      lcGrpBy="DTOS(CRDATE)+ccurrcode"
      lcGrpFoot  =" 'Tran.Date: '+DTOC(CRDATE) +' ('+ ccurrcode+ ')' "
    ENDIF
    IF llMultCurr AND lcRpCurr<>"F" AND  lcRpFormat="S" 
      STORE '' TO lcGrpFoot
    ENDIF
    *E302378,1 TMI [End  ] 
ENDCASE

*--End of lfGetGrp
*!*************************************************************
*! Name      : lfGetFlChg
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : To detect when filter change
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : .
*!*************************************************************
*! Example     : =lfGetFlChg()
*!*************************************************************
FUNCTION lfGetFlChg

IF lcOldVal!=lcRpCurr
  lcOldVal=lcRpCurr
  llOGFltCh=.T.
ENDIF
RETURN

*--End of lfGetFlChg.
*!*************************************************************
*! Name      : lfGetDetal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : to collect data  when detail mode
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : .
*!*************************************************************
*! Example     : =lfGetDetal()
*!*************************************************************
FUNCTION lfGetDetal
PRIVATE lcOldArea

lcOldArea = SELECT()
SELECT RETHDR
SET ORDER TO RETHDR
SCAN FOR &lcRepExpr
  *E302378,1 TMI [Start] if state is not included in the selected list
  =SEEK('M'+RETHDR.ACCOUNT,'CUSTOMER')
  M.STATE = SUBSTR(CUSTOMER.CADDRESS4,1,6)
  IF !EMPTY(laRpTStats[1])
    IF !M.STATE $ lcRpStates
      LOOP
    ENDIF
  ENDIF
  *E302378,1 TMI [End  ] 
  SCATTER MEMVAR MEMO
  IF llVoidOnly OR (llDateRang AND STATUS = 'V' AND ;
                   BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                   !BETWEEN(CRDATE,ldStrtDate,ldEndDate))
    *--negative in void range only values  
    = lfNegValue() && Negative void values.
  ENDIF
  IF llMultCurr AND lcRpCurr<>"F" 
    =lfCalAmt()      
  ENDIF
  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN  && end Scan to fill Temp. File with filtered data.
SELECT(lcOldArea)
RETURN

*--End of lfGetDetal.
*!*************************************************************
*! Name      : lfGetSumry
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : to collect data  when summary and use equivlant mode
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : .
*!*************************************************************
*! Example     : =lfGetSumry()
*!*************************************************************
FUNCTION lfGetSumry

PRIVATE lnpieces,lnvpieces,lngrossamt,lnvgrosamt,lndiscamt,lnvdiscamt,lnAmount,lnVAmount
PRIVATE lnOther,lnvOther,lnTotCrd,lnvTotCrd
PRIVATE lcOldArea
STORE 0 TO lnpieces,lnvpieces,lngrossamt,lnvgrosamt,lndiscamt,lnvdiscamt,lnAmount,lnVAmount,;
           lnOther,lnvOther,lnTotCrd,lnvTotCrd

PRIVATE lnTax_Amt           
lnTax_Amt = 0

lcOldArea = SELECT()
SELECT RETHDR
SET ORDER TO RETHDR

SCAN FOR &lcRepExpr
  *E302378,1 TMI [Start] exclude not selected states
  =SEEK('M'+RETHDR.ACCOUNT,'CUSTOMER')
  M.STATE = SUBSTR(CUSTOMER.CADDRESS4,1,6)
  IF !EMPTY(laRpTStats[1])
    IF !M.STATE $ lcRpStates
      LOOP
    ENDIF
  ENDIF
  *E302378,1 TMI [End  ]   
  SCATTER MEMVAR MEMO
  IF llVoidOnly OR (llDateRang AND STATUS = 'V' AND ;
                   BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                   !BETWEEN(CRDATE,ldStrtDate,ldEndDate))
    *--negative in void range only values  
    = lfNegValue() && Negative void values.
  ENDIF
  =lfCalAmt()      
  lnpieces   = lnpieces  +M.pieces
  lnvpieces  = lnvpieces +M.vpieces
  lngrossamt = lngrossamt+M.GROSS_AMT
  lnvgrosamt = lnvgrosamt+M.VGROSS_AMT
  lndiscamt  = lndiscamt +M.DISC_AMT
  lnvdiscamt = lnvdiscamt+M.VDISC_AMT
  lnAmount   = lnAmount  +M.AMOUNT
  lnVAmount  = lnVAmount +M.VAMOUNT
  lnOther    = lnOther   +M.OTHER
  lnvOther   = lnvOther  +M.VOTHER
  lnTotCrd   = lnTotCrd  +M.TOTCREDIT
  lnvTotCrd  = lnvTotCrd +M.VTOTCREDIT
  lnTax_Amt  = lnTax_Amt + m.Tax_Amt

ENDSCAN  
INSERT INTO (lcWorkFile) FROM MEMVAR

SELECT (lcWorkFile)
REPLACE pieces     WITH  lnpieces    ,;
        vpieces    WITH  lnvpieces   ,;
        GROSS_AMT  WITH  lngrossamt  ,;
        VGROSS_AMT WITH  lnvgrosamt  ,;
        DISC_AMT   WITH  lndiscamt   ,;
        VDISC_AMT  WITH  lnvdiscamt  ,;
        AMOUNT     WITH  lnAmount    ,;
        VAMOUNT    WITH  lnVAmount   ,;
        OTHER      WITH  lnOther     ,;
        VOTHER     WITH  lnvOther    ,; 
        TOTCREDIT  WITH  lnTotCrd    ,; 
        VTOTCREDIT WITH  lnvTotCrd   ,;
        Tax_Amt    WITH lnTax_Amt

SELECT(lcOldArea)        
RETURN

*--End of lfGetSumry.
*!*************************************************************
*! Name      : lfGetSumfg
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : to collect data  when summary and use forign mode
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : .
*!*************************************************************
*! Example     : =lfGetSumfg()
*!*************************************************************
FUNCTION lfGetSumfg
PRIVATE lcOldArea,lcOrder,lcExp
STORE ' ' TO lcExp
DIMENSION laCurr[1]
lcOldArea = SELECT()
DO CASE
  CASE lcRpSortBy = "C"
    lcExp = "RETHDR.ccurrcode"    
  CASE lcRpSortBy = "A"
    lcExp = "RETHDR.ACCOUNT+RETHDR.ccurrcode"     
  CASE lcRpSortBy = "S"
    lcExp = "RETHDR.SALESREP1+RETHDR.ccurrcode"     
  *E302378,1 TMI [Start] Add sort by State/Trans.Date
  CASE lcRpSortBy = "T"
    lcExp = "M.STATE+RETHDR.ccurrcode"     
  CASE lcRpSortBy = "D"
    lcExp = "DTOS(RETHDR.CRDATE)+RETHDR.ccurrcode"     
  *E302378,1 TMI [End  ] 
ENDCASE

SELECT(lcWorkFile)
lcOrder = ORDER()
SET ORDER TO TAG "TempIndx"
SELECT RETHDR
SET ORDER TO RETHDR
SCAN FOR &lcRepExpr
  *E302378,1 TMI [Start] exclude not selected states
  =SEEK('M'+RETHDR.ACCOUNT,'CUSTOMER')
  M.STATE = SUBSTR(CUSTOMER.CADDRESS4,1,6)
  IF !EMPTY(laRpTStats[1])
    IF !M.STATE $ lcRpStates
      LOOP
    ENDIF
  ENDIF
  *E302378,1 TMI [End  ]   
  SCATTER MEMVAR MEMO 
  IF llVoidOnly OR (llDateRang AND STATUS = 'V' AND ;
                   BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                   !BETWEEN(CRDATE,ldStrtDate,ldEndDate))
    *--negative in void range only values  
    = lfNegValue() && Negative void values.
  ENDIF

  SELECT (lcWorkFile)
  IF !SEEK(EVAL(lcExp))
    *B123885,1 NNA 09/21/2004 (Begin) if the Credit Memo is void I make M.Tax_Amt = Zero
    IF m.Status = 'V' .AND. !llVoidOnly
      STORE 0 TO M.Tax_Amt
    ENDIF
    *B123885,1 NNA (End)

    m.Status = "" 
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ELSE 
    *B123885,1 NNA 09/21/2004 (Begin) Sum only the Tax_Amt for the Active Credit Memo (not the Voided)
    *REPLACE pieces     WITH pieces+m.pieces        ,;
            vpieces    WITH vpieces+M.vpieces       ,;
            GROSS_AMT  WITH GROSS_AMT+M.GROSS_AMT   ,;
            VGROSS_AMT WITH VGROSS_AMT+M.VGROSS_AMT ,;
            DISC_AMT   WITH DISC_AMT+M.DISC_AMT     ,;
            VDISC_AMT  WITH VDISC_AMT+M.VDISC_AMT   ,;
            AMOUNT     WITH AMOUNT +M.AMOUNT        ,;
            VAMOUNT    WITH VAMOUNT+M.VAMOUNT       ,;
            OTHER      WITH OTHER+M.OTHER           ,;
            VOTHER     WITH VOTHER+M.VOTHER         ,;
            TOTCREDIT  WITH TOTCREDIT+M.TOTCREDIT   ,;
            VTOTCREDIT WITH VTOTCREDIT+M.VTOTCREDIT ,;
            Tax_Amt    WITH Tax_Amt + M.Tax_Amt
    REPLACE pieces     WITH pieces+m.pieces         ,;
            vpieces    WITH vpieces+M.vpieces       ,;
            GROSS_AMT  WITH GROSS_AMT+M.GROSS_AMT   ,;
            VGROSS_AMT WITH VGROSS_AMT+M.VGROSS_AMT ,;
            DISC_AMT   WITH DISC_AMT+M.DISC_AMT     ,;
            VDISC_AMT  WITH VDISC_AMT+M.VDISC_AMT   ,;
            AMOUNT     WITH AMOUNT +M.AMOUNT        ,;
            VAMOUNT    WITH VAMOUNT+M.VAMOUNT       ,;
            OTHER      WITH OTHER+M.OTHER           ,;
            VOTHER     WITH VOTHER+M.VOTHER         ,;
            TOTCREDIT  WITH TOTCREDIT+M.TOTCREDIT   ,;
            VTOTCREDIT WITH VTOTCREDIT+M.VTOTCREDIT ,;
            Tax_Amt    WITH Tax_Amt + IIF(M.STATUS='V' .AND. !llVoidOnly, 0.00,M.Tax_Amt)

    *B123885,1 NNA (End)
    
  ENDIF
  SELECT RETHDR
ENDSCAN
SELECT(lcWorkFile)
SET ORDER TO TAG &lcOrder
SELECT(lcOldArea)  
RETURN

*--End of lfGetSumfg.
*!*************************************************************
*! Name      : lfGetSumfg
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/01/2003
*! Purpose   : To save the old value.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : .
*!*************************************************************
*! Example     : =lfGetSumfg()
*!*************************************************************
FUNCTION lfWSortBY

lcOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*--End of lfWSortBY.

*:**************************************************************************X
*:* Name        : lfvState
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/06/2007
*:* Purpose     : Valid function for the selected states
*:***************************************************************************
*E302378,1 TMI
FUNCTION lfvState
PRIVATE lcOStates,lcState

lcOStates = lcRpStates
=gfMover(@laRpSStats,@laRpTStats,'States',.T.,'')  && call mover function.

lcRpStates = '|'
IF !EMPTY(laRpTStats[1])
  FOR lnI = 1 TO ALEN(laRpTStats,1)
    lcState = LEFT(laRpTStats[lnI],6) + '|'
    lcRpStates = lcRpStates + lcState
    lcOStates = STRTRAN(lcOStates,lcState,'')
  ENDFOR  
  lcOStates = CHRTRAN(lcOStates,'|','')
ENDIF

IF !EMPTY(lcOStates)
  llOgFltCh = .T.
ENDIF
*-- end of lfvState.