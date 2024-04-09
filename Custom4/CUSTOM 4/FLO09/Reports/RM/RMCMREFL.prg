*:***************************************************************************
*: Program file  : RMCMRET.PRG
*: Program desc. : CREDIT MEMO FOR RETURNS.For FLO09
*: Date          : 10/09/2007
*: System        : Aria 4XP
*: Module        : RETURN MERCHANDISE (RM)
*: Developer     : Mariam Mazhar (MMT)
*: Tracking      : (C200871)[T20070920.0033]
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
*: T20060928.0015 ,MMT 11/01/2006 Convert report layout to graphics (E302320)
*****************************************************************************
lcStTime   = TIME()    && Time in which we start collect data.
STORE '' TO lcGrpBY    && hold group variable
STORE '' TO lcGrpFoot  && hold group footer
STORE '' TO ldStrtDate , ldEndDate

*--T20060928.0015 ,MMT 11/01/2006 Convert report layout to graphics[Start]
loogScroll.cCROrientation = 'P'
*--T20060928.0015 ,MMT 11/01/2006 Convert report layout to graphics[End]

*--Fill the variables of the posted date.
llPostDate = .F.
IF IIF(ALLTRIM(gfGetMemVar('M_LINK_GL')) = 'Y', .T. , .F.)
  STORE {  /  /  } TO ldStrtDPst , ldEndDPst
  lnPostDate = lfItmPos('DPOSTDATE')
  IF !EMPTY(laOGFxFlt[lnPostDate,6])
  *ldStrtDPst = CTOD(PADR(laOGFxFlt[lnPostDate,6],ATC('|',laOGFxFlt[lnPostDate,6])+1))
    ldStrtDPst = CTOD(SUBSTR(laOGFxFlt[lnPostDate,6],1,ATC('|',laOGFxFlt[lnPostDate,6])-1))
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
*  ldStrtDate = CTOD(PADR(laOGFxFlt[lnCRPos,6],ATC('|',laOGFxFlt[lnCRPos,6])+1))
  ldStrtDate = CTOD(SUBSTR(laOGFxFlt[lnCRPos,6],1,ATC('|',laOGFxFlt[lnCRPos,6])-1))
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
  
  llUseSeason  = .F.
  lnSeaPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
  IF lnSeaPos > 0 
    lnSeaPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnSeaPos,1)
    lcSeaSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnSeaPos,6]),loOgScroll.laOgFXFlt[lnSeaPos,6],'')
    IF !EMPTY(lcSeaSel) 
      lcSeaFile = loOGScroll.gfTempName()
      llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
    ENDIF   
  ENDIF   

  
  
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

*crmemo choosed
lcJoin =""
 lcCurName = lfCheckFilter(3, 'RETHDR.CRMEMO')    
  IF !EMPTY(lcCurName)
    SELECT &lcCurName    
    llFound = ( RECCOUNT() > 0) 
    IF llFound
      lcSQLCur = loOgScroll.gfSQLTempName('','CRMEMO C(6)',lcCurName,'CRMEMO') && SQL Temp File
      IF EMPTY(lcSQLCur)
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpMEMO on TmpMEMO.CRMEMO = RETHDR.CRMEMO "
    ENDIF
  ENDIF
*account selection
 lcCurName = lfCheckFilter(3, 'RETHDR.ACCOUNT')    
  IF !EMPTY(lcCurName)
    SELECT &lcCurName    
    llFound = ( RECCOUNT() > 0) 
    IF llFound
      lcSQLCur = loOgScroll.gfSQLTempName('','ACCOUNT C(6)',lcCurName,'ACCOUNT') && SQL Temp File
      IF EMPTY(lcSQLCur)
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpACCOUNT on TmpACCOUNT.ACCOUNT= RETHDR.ACCOUNT "
    ENDIF
  ENDIF
*sales rep selection
 lcCurName = lfCheckFilter(3, 'RETHDR.SALESREP1')    
  IF !EMPTY(lcCurName)
    SELECT &lcCurName    
    llFound = ( RECCOUNT() > 0) 
    IF llFound
      lcSQLCur = loOgScroll.gfSQLTempName('','SALESREP1 C(6)',lcCurName,'SALESREP1') && SQL Temp File
      IF EMPTY(lcSQLCur)
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpSALESREP1 on TmpSALESREP1.SALESREP1= RETHDR.SALESREP1 "
    ENDIF
  ENDIF
 lcSQLStmt  = "Select * From RETHDR(INDEX=RETHDR) " + lcJoin 
  lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'RETHDR' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
  IF lnResult = 1
    SELECT RETHDR 
    =CURSORSETPROP("Buffering" ,3)
    INDEX on crmemo TAG RETHDR 
 ELSE
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF  
  
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
  lcHedLin1 = IIF(llTaxes,'','Order '+SPACE(1)) + 'Factor' + ' ' + 'Div   '  + ' ' + ;
            ' Pieces ' + '  Gross Amt' + ' Discount ' + 'Merch. Amt ' + ;
            '     Other    ' + IIF(llTaxes,'Tax Amt '+'       Total','     Total') 
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

=gfOpenTable('RETLINE','RETLINE','SH','RETLINE')
=gfOpenTable('STYLE','STYLE','SH','STYLE')

IF TYPE('lcLastTag') = 'N'
  R_WIDTH='W'
  SET ORDER TO CUSTOMER IN CUSTOMER
  SET ORDER TO Codes    IN CODES   
  loDBFRetH    = CreateObject("RemoteTable","RETHDR","RETHDR","RETHDR_a",SET("DATASESSION"),.T.)
  SET ORDER TO Salesrep IN Salesrep
  DIMENSION laTempStru[1,18]
  laTempStru = ''
  SELECT RETHDR_a
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 18]
  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 11
  laTempStru[ALEN(laTempStru,1)  ,4] = 0
  =lfWorkFile()
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

LOCAL lnField, lnCol

FOR lnField = 1 TO ALEN(laTempStru,1)
  FOR lnCol = 7 TO 16
    laTempStru[lnField, lnCol] = ""
  ENDFOR  
  STORE 0 TO laTempStru[lnField, 17],laTempStru[lnField, 18]
ENDFOR

CREATE CURSOR (lcWorkFile) FROM ARRAY laTempStru
SELECT (lcWorkFile)
INDEX ON cTempKey TAG (lcWorkFile) OF (lcWorkFile)
DO CASE
  CASE lcRpSortBy = "C"
    INDEX ON cCurrcode TAG "TempIndx" OF (lcWorkFile)
  CASE lcRpSortBy = "A"
    INDEX ON ACCOUNT+cCurrcode TAG "TempIndx"  OF (lcWorkFile)
  CASE lcRpSortBy = "S"
    INDEX ON SALESREP1+cCurrcode TAG "TempIndx" OF (lcWorkFile)
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
ENDCASE

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
IF m.Status = 'V' .AND. !llVoidOnly
  STORE 0 TO M.Tax_Amt
ENDIF
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

  llFound = .F.
  IF llUseSeason  
    IF gfSeek(RETHDR.CRMEMO,'RETLINE')
      SELECT RETLINE
      SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = RETHDR.CRMEMO FOR ;
            gfSeek(RETLINE.Style,'Style') AND SEEK(STYLE.SEASON,lcSeaFile)
        llFound = .T.    
      ENDSCAN 
    ENDIF 
  ELSE
    llFound = .T.
  ENDIF 
  
  SELECT RETHDR
  IF !llFound 
    LOOP 
  ENDIF
  
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


 llFound = .F.
  IF llUseSeason  
    IF gfSeek(RETHDR.CRMEMO,'RETLINE')
      SELECT RETLINE
      SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = RETHDR.CRMEMO FOR ;
            gfSeek(RETLINE.Style,'Style') AND SEEK(STYLE.SEASON,lcSeaFile)
        llFound = .T.    
      ENDSCAN 
    ENDIF 
  ELSE
    llFound = .T.
  ENDIF 
  
  SELECT RETHDR
  IF !llFound 
    LOOP 
  ENDIF

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
ENDCASE

SELECT(lcWorkFile)
lcOrder = ORDER()
SET ORDER TO TAG "TempIndx"
SELECT RETHDR
SET ORDER TO RETHDR
SCAN FOR &lcRepExpr


 llFound = .F.
  IF llUseSeason  
    IF gfSeek(RETHDR.CRMEMO,'RETLINE')
      SELECT RETLINE
      SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = RETHDR.CRMEMO FOR ;
            gfSeek(RETLINE.Style,'Style') AND SEEK(STYLE.SEASON,lcSeaFile)
        llFound = .T.    
      ENDSCAN 
    ENDIF 
  ELSE
    llFound = .T.
  ENDIF 
  
  SELECT RETHDR
  IF !llFound 
    LOOP 
  ENDIF

  SCATTER MEMVAR MEMO 
  IF llVoidOnly OR (llDateRang AND STATUS = 'V' AND ;
                   BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                   !BETWEEN(CRDATE,ldStrtDate,ldEndDate))
    *--negative in void range only values  
    = lfNegValue() && Negative void values.
  ENDIF

  SELECT (lcWorkFile)
  IF !SEEK(EVAL(lcExp))
    IF m.Status = 'V' .AND. !llVoidOnly
      STORE 0 TO M.Tax_Amt
    ENDIF
    m.Status = "" 
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ELSE 
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
  ENDIF
  SELECT RETHDR
ENDSCAN
SELECT(lcWorkFile)
SET ORDER TO TAG &lcOrder
SELECT(lcOldArea)  
RETURN

*--End of lfGetSumfg.
*!*************************************************************
*! Name      : lfWSortBY
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

*********************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected and returns the 
*!          Cursor name if In Range and the expression if mover 
*********************************************************************
FUNCTION lfCheckFilter()
  LPARAMETERS lnArrayType, lcFilter
  LOCAL lcReturn, lnPOS   
  DO CASE
  CASE lnArrayType = 1 
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
      IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
        lcReturn = ""     
      ENDIF
  CASE lnArrayType = 2  
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
      IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
        lcReturn = ""     
      ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
      IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
        lcReturn = ""     
      ENDIF
  OTHERWISE :
    lcReturn = ""
  ENDCASE  
  RETURN lcReturn


*************************************************************************************************
*************************************************************************************************






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
*!*  FUNCTION lfvCrMemo
*!*  PRIVATE lcVar , lcObj , laTemp

*!*  lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
*!*  lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
*!*  lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*!*  *--IF Statment to check if we are going to Browse
*!*  IF !EMPTY(lcObj) AND !SEEK(lcObj , 'RETHDR')
*!*    SELECT RETHDR
*!*    DIMENSION laTemp[1]
*!*    laTemp = ''      && Array to hold the Selected value
*!*    lcBrFields = "CRMEMO    :R :H= 'CrMemo#' , "    +;
*!*                 "CRSTATUS= IIF(STATUS = 'V' , 'Voided' , 'Credit Memo') :R :H= 'STATUS' , " +;
*!*                 "PRINTED = IIF(Flag = 'Y' , 'Yes' , 'No') :R :H= 'Printed' , " +;
*!*                 "ACCOUNT   :R :H= 'Account' ,"    +;
*!*                 "STORE     :R :H= 'Store' ,"      +;
*!*                 "CRDATE    :R :H= 'Credit Date',"+;
*!*                 "VDATE     :R :H= 'Void Date',"+;
*!*                 "REASON    :R :H= 'Reason' ,"     +;
*!*                 "CDIVISION :R :H= 'Division' ,"   +;
*!*                 "CTERMCODE :R :H= 'Terms' ,"      +;
*!*                 "PIECES    :R :H= 'Pieces' ,"    +;
*!*                 "AMOUNT    :R :H= 'Amount ' ,"    +; 
*!*                 "OTHER     :R :H= 'Other ',"  +; 
*!*                 "TAX_AMT   :R :H= 'Tax Amount' "
*!*    
*!*    lcFile_Ttl = "Credit Memo ..."
*!*    PRIVATE lcStatFlt
*!*    lcStatFlt = ''
*!*    DO CASE
*!*      CASE EMPTY(lcRpStatus)
*!*        lcStatFlt = [EMPTY(Status)]
*!*      CASE lcRpStatus = "V"
*!*        lcStatFlt = 'STATUS = "V"'
*!*      OTHERWISE
*!*        lcStatFlt = ''
*!*    ENDCASE
*!*    lcBrowCond = IIF(EMPTY(lcStatFlt),'',[FOR ] + lcStatFlt)
*!*    = gfBrows(lcBrowCond,'CRMEMO','laTemp')
*!*    IF !EMPTY(laTemp[1])
*!*      lcObj = laTemp[1]
*!*    ELSE
*!*      lcObj = laOldVal
*!*    ENDIF
*!*    
*!*  ENDIF
*!*  &lcVar = lcObj      && Update the field



*--End of lfvCrMemo.
*!*  *!*************************************************************
*!*  *! Name      : lfvRep
*!*  *! Developer : BASSEM RAFAAT ERNEST (BWA)
*!*  *! Date      : 14/01/2003
*!*  *! Purpose   : Validation function for the sales rep. field
*!*  *!*************************************************************
*!*  *! Called from : Order field [Option Grid]
*!*  *!*************************************************************
*!*  *! Calls       : gfBrows()
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  *! Example     : = lfvRep()
*!*  *!*************************************************************
*!*  FUNCTION lfvRep
*!*  PRIVATE lcVar , lcObj , laTemp

*!*  lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
*!*  lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*!*  *--IF Statment to check if we are going to Browse
*!*  IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
*!*    SELECT SALESREP
*!*    DIMENSION laTemp[1]
*!*    laTemp = ''      && Array to hold the Selected value

*!*    lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
*!*                 "NAME      :R :H= 'Name' ,"    +;
*!*                 "cAddress6 :R :H= 'Country' ," +;
*!*                 "PHONE     :R :H= 'Phone' ,"   +;
*!*                 "BALANCE   :R :H= 'Balance' "
*!*    
*!*    lcFile_Ttl = "Sales Representative ..."
*!*    = gfBrows('','REPCODE','laTemp')
*!*      
*!*    *--IF The user selected a record
*!*    IF !EMPTY(laTemp[1])
*!*      lcObj = laTemp[1]
*!*    ELSE
*!*      lcObj = laOldVal
*!*    ENDIF
*!*    
*!*  ENDIF
*!*  &lcVar = lcObj      && Update the field

*!*  *--End of lfvRep.




*!*  *!*************************************************************
*!*  *! Name      : lfvAcc
*!*  *! Developer : BASSEM RAFAAT ERNEST (BWA)
*!*  *! Date      : 14/01/2003
*!*  *! Purpose   : Validate function for the Customer Account field
*!*  *!*************************************************************
*!*  *! Called from : Account field [Option Grid]
*!*  *!*************************************************************
*!*  *! Calls       : CusBrowM()
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  *! Example   : = lfvAcc()
*!*  *!*************************************************************
*!*  FUNCTION lfvAcc
*!*  PRIVATE lcItsName , lcItsVal , llObjRet
*!*  lcItsName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
*!*  lcItsVal  = EVALUATE(SYS(18))     && Varible to hold  the value of the current GET field

*!*  IF USED('SYCINT')
*!*    SET ORDER TO TAG Ccontcode IN SYCINT
*!*  ENDIF

*!*  *--IF The user want to Browse or if the Account he entered is not in the file
*!*  IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
*!*    llObjRet = CusBrowM(@lcItsVal , '' , 'M')
*!*    lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
*!*    &lcItsName = lcItsVal
*!*  ENDIF

*--End of lfvAcc.

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 10/09/2007
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile

lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  CASE  ALLTRIM(lcFieldName) = 'SEASON'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6 
    laTempacstru[1,4]= 0
ENDCASE 


= gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

