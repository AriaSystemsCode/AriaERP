*:----------------------------------------------------------------
*: Program file        : ARCSHRSV
*: Program description : Saving the Cash Receipt transaction
*: For screen          : 
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Account Receivable - (AR)
*: Developer Name      : Ahmed Amer
*: Tracking Job Number : E301521,1
*:----------------------------------------------------------------
*: Calls               : 
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO lfSavCashR IN ARCSHR.PRG
*:----------------------------------------------------------------

*:----------------------------------------------------------------
*: Function            : lfSavCashR
*: Program description : Saving the Cash Receipt transaction
*: For screen          : 
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Account Receivable - (AR)
*: Developer Name      : Ahmed Amer
*: Tracking Job Number : E301521,1
*:----------------------------------------------------------------
*: Calls               : 
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO lfSavCashR IN ARCSHR.PRG
*:----------------------------------------------------------------

FUNCTION lfSavCashR
PARAMETERS llFromEDI

PRIVATE lnCurAls

*E301521,1 (Start) Open the needed tables for NC module use
llEdiAccount = .F.
lcSysType = gfGetMemVar('M_SYSTYPE',gcAct_Comp)
IF !llFromEdi AND 'NC' $ gcCmpModules AND lcSysType = 'P'
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  SELECT CODES
  SET ORDER TO TAG Idrltfname
  =SEEK('NYCSITEID')
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  IF FOUND()
    lcSiteId = Codes.cCode_No
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = lcSiteId
    IF FOUND('EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'812','EDIPD')
      llEdiAccount = .T.
    ENDIF
  ENDIF
ENDIF    
*E301521,1 (End)

SELECT (lcTmpCredt)
LOCATE FOR AMOUNT = 0 AND !DELETED()
IF FOUND()
  *-- One or more of the transaction(s) has zero amount, Transaction(s)
  *-- with zero amount will be ignored.
  *-- < Modify > < Continue >
  IF gfModalGen("QRM40071B40009") = 1
    = lfwBrow()
    _CUROBJ = OBJNUM(lnAmount)
    RETURN
  ENDIF
ENDIF

*-- Validate if there are no transactions...
SELECT (lcTmpCredt)
COUNT ALL FOR Amount <> 0 TO lnTotTrans
IF lnTotTrans = 0
  *-- No transactions entered, cannot proceed.
  *-- < ok >
  = gfModalGen("TRM40070B00000")
  RETURN
ENDIF

*-- Checking the actual number of payments with the tape count.
IF lnTapCount <> lnActCount
  *-- The actual number of payments (999) is not equal to the 
  *-- tape count (999).
  *-- < Modify > < Continue >
  lcStr1 = "The actual number of payments ("+ALLTRIM(STR(lnActCount))+")"
  lcStr2 = "the tape count ("+ ALLTRIM(STR(lnTapCount))+")"
  IF gfModalGen("QRM40060B40009","ALERT",lcStr1+"|"+lcStr2) = 1
    GOTO TOP IN (lcTmpCredt)
    KEYBOARD "{CTRL+B}" PLAIN
    RETURN
  ENDIF  
ENDIF

*-- Checking the actual amount with the tape total.
IF lnTapTotal <> lnActTotal
  *-- The actual amount (999.99) is not equal to the tape total (999.99).
  *-- < Modify > < Continue >
  lcStr1 = "The actual amount (" + ALLTRIM(STR(lnActTotal,10,2)) + ")"
  lcStr2 = "the tape total ("    + ALLTRIM(STR(lnTapTotal,10,2)) + ")"
  IF gfModalGen("QRM40060B40009","ALERT",lcStr1+"|"+lcStr2) = 1
    GOTO TOP IN (lcTmpCredt)
    KEYBOARD "{CTRL+B}" PLAIN
    RETURN
  ENDIF  
ENDIF

IF llApLink 
  IF EMPTY(lcBankCode)
    *-- You have to enter the Bank Code.
    *-- < ok >
    = gfModalGen("TRM00250B00000", "ALERT", "Bank Code")
    _CUROBJ = OBJNUM(lcBankCode)
    RETURN
  ENDIF

  IF EMPTY(lcChkAcnt) 
    *-- You have to enter the Checking Account.
    *-- < ok >
    = gfModalGen("TRM00250B00000", "ALERT", "Checking Account")
    _CUROBJ = OBJNUM(lcChkAcnt)
    RETURN
  ENDIF
ENDIF

IF llMltCur 
  *-- Be sure that the currencey code field is not empty if the system
  *-- is setup to use multiable currencies.
  IF EMPTY(lcCurCode)
    *-- You have to enter the Currency Code.
    *-- < ok >
    = gfModalGen("TRM00250B00000", "ALERT", "Currency Code")
    _CUROBJ = IIF(llApLink, OBJNUM(lcChkAcnt), OBJNUM(lcCurCode))
    RETURN
  ENDIF  

  *-- The rate will never be less that one because it is validated
  *-- either from the valid rate function which is called from the 
  *-- object if the rate is editable, or it is validated from the 
  *-- currency code object or the bank code or checking account, but 
  *-- there is only one case that might let the rate equal to zero, 
  *-- this case is to enter the rate as zero and do not press enter
  *-- on the field but press escape, at that time the global escape 
  *-- trapping will be excuted without excuting the object valid
  *-- function.
  IF lnExRate <= 0
    *-- Currency exchange rate should not be less than one.
    *-- <  ok >
    = gfModalGen("INM40061B00000")
    IF llEditExRt
      _CUROBJ = OBJNUM(lnExRate)
    ENDIF  
    RETURN
  ENDIF
ENDIF

IF llGlLink AND !llApLink AND EMPTY(lcGLAcct2)
  *-- You have to enter the Cash Receipt Account.
  *-- < ok >
  = gfModalGen("TRM00250B00000", "ALERT", "Cash Receipt Account")
  _CUROBJ = OBJNUM(lcGLAcct2)
  RETURN
ENDIF

IF llGlLink
  SELECT (lcTmpCredt)
  LOCATE FOR EMPTY(STRTRAN(cARGLAcc,"-",SPACE(1)))
  IF FOUND()
    = lfwBrow()

    *-- You have to enter the AR Account.
    *-- < ok >
    lcStr = "correct " + IIF(lNonAr, "Non ", "") + "AR payment type"
    = gfModalGen("TRM00250B00000", "ALERT", lcStr)
    _CUROBJ = IIF(lNonAr, OBJNUM(lnNArPayT), OBJNUM(lnArPayT))
    RETURN
  ENDIF
ENDIF

SELECT (lcTmpCredt)
LOCATE FOR !lNonAr AND EMPTY(Account)
IF FOUND()
  = lfwBrow()
  *-- You have to enter the Account Code.
  *-- < ok >
  = gfModalGen("TRM00250B00000", "ALERT", "Account Code")
  _CUROBJ = OBJNUM(lcAccount)
  RETURN
ENDIF

*-- Start updating the master files.
lnCurAls   = SELECT(0)
llOpSeqn   = gfOpenFile(gcDataDir+"Sequence", "cSeq_Type","SH")
llOpnHis   = gfOpenFile(gcDataDir+"ARCUSHST", "Acthst"   ,"SH")

*-- Update the uncomplete session file with the object name.
llNoThing  = lfUpdUnCmS("Open", "pbSav", .T.)

lcBatSeqNo = IIF(EMPTY(lcBatSeqNo), gfSequence("BATCH", gcAct_Comp), lcBatSeqNo)
llNoThing  = lfUpdVars()

lnCurRec = 0
lcMsg1   = "Updating master credit file"

SELECT (lcTmpCredt)
SCAN FOR Amount <> 0
  lnCurRec   = lnCurRec + 1
  lcTrnSeqNo = gfSequence("TRAN", gcAct_Comp)
  lcMsg2     = "Transaction Number : " + lcTrnSeqNo
  lcPayComp  = &lcTmpCredt..Reference
  llNoThing  = IIF(&lcTmpCredt..lNonAr, .T., lfUpdCust())
  llNoThing  = IIF(&lcTmpCredt..lNonAr, .T., lfUpdCusHs())
  llNoThing  = IIF(llApLink, lfUpdApPay(), .T.)
  llNoThing  = lfUpdTmp()
  llNoThing  = gfThermo(lnTotTrans, lnCurRec, lcMsg1 ,lcMsg2)

  *E301521,1 (Start) Add new record in the EDITrans table for NC module using
  IF llEdiAccount
    INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,cStatus,lInterComp) VALUES ;
    ('812',&lcTmpCredt..TranType+&lcTmpCredt..Tran,'A',EdiAcPrt.cPartner,'N',EdiAcPrt.lInterComp)
    =gfAdd_Info('EDITRANS')
  ENDIF
  *E301521,1 (End)
  
ENDSCAN

*B603091,1 Start, Added.
IF gcAct_Appl='PS' AND ;
  ( EMPTY(gfGetMemVar("M_PRTRCET")) OR gfGetMemVar("M_PRTRCET") $ 'AI' ) 
  *--Do you wish to print the case receipt? Y/N
  =lfPrtRectpt()
ENDIF
*B603091,1 End.

llNoThing = IIF(llGlLink, lfUpdGL(), .T.)

SELECT Credit
APPEND FROM (gcWorkDir+lcTmpCredt) FOR Amount <> 0

*-- Update the uncomplete session file.
llNoThing = lfUpdUnCmS("Complete", SPACE(0), .F.)
laScrMode = "V"
*B603639,1 (Start) to Add new record in the UnCMSESS file
lnUnCmSeRc=0
*B603639,1 (End) to Add new record in the UnCMSESS file

*-- Generated batch number is : XXXXXXXX
*-- < ok >
lnNoThing = gfModalGen("INM40064B00000", "DIALOG", lcBatSeqNo)

USE IN IIF(llOpSeqn, "Sequence" , 0) 
USE IN IIF(llOpnHis, "ARCUSHST" , 0)

SELECT(lnCurAls)

IF !llFromEdi
laScrMode    = .F.
laScrMode[2] = .T.
SHOW GETS
ENDIF

*:----------------------------------------------------------------
FUNCTION lfUpdUnCmS
PARAMETERS lcStatus, lcCurObj, llSetCon
PRIVATE lnAlias

*-- If there is an incomplete session record for this session..
IF lnUnCmSeRc <> 0
  lnAlias  = SELECT(0)
  lcStatus = UPPER(LEFT(lcStatus,1))

  *-- Go and update it..
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  UNLOCK
  REPLACE cCurrObj WITH lcCurObj ,;
          Status   WITH lcStatus
  llNoThing = RLOCK()

  *-- If you are asked to unlock the record, please do..
  IF !llSetCon
    UNLOCK 
  ENDIF

  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
FUNCTION lfUpdVars
PRIVATE lnAlias

*-- Build the files names string.
lcFiles = "lcTmpCredt," + lcTmpCredt + ", ;"
*-- Go and update the incomplete session record.
llNoThing = gfSavSess(lcProgID, lcFiles, @laVars, lcSession)

* ----------------------------------------------------------------

FUNCTION lfUpdCust

IF &lcTmpCredt..nSteps < 1
  IF SEEK("M" + &lcTmpCredt..Account, "Customer")
    lcPayComp = Customer.BTName
    lnEqvAmnt = ROUND(&lcTmpCredt..Amount &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
    SELECT Customer
    llNoThing = RLOCK()
    REPLACE OpenCr WITH OpenCr + lnEqvAmnt ,;
            NetBal WITH NetBal + lnEqvAmnt
    UNLOCK
    
    *E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245 function to update nHgWtrMark Field with NETBAL field [End.]

  ENDIF  
  = lfUpdStp(lcTmpCredt, 1)
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfUpdCusHs

IF &lcTmpCredt..nSteps < 2
  IF BETWEEN(VAL(lcGLPeriod), 1, 13)
    lcPeriod  = PADL(ALLTRIM(lcGLPeriod), 2, "0" )
    lnEqvAmnt = ROUND(&lcTmpCredt..Amount &lcExRSin lnExRate &lcUntSin lnCurrUnit, 2)
    lnEqvAmnt = ABS(lnEqvAmnt)
    IF SEEK(&lcTmpCredt..Account+lcGLFYear, "ARCusHst")
      SELECT ARCusHst
      llNoThing = RLOCK()
      REPLACE nPayment&lcPeriod WITH nPayment&lcPeriod + lnEqvAmnt ,;
              nPayment          WITH nPayment          + lnEqvAmnt
      UNLOCK
    ENDIF
  ENDIF
  = lfUpdStp(lcTmpCredt, 2)
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfUpdApPay

IF &lcTmpCredt..nSteps < 3
  
  *E301077,52 YMA 03/03/99 Start - Open the ApPaymnt file.
  = gfOpenFile (gcDataDir+"ApPayMnt","","SH")
  *E301077,52 YMA 03/03/99 End.
  
  SELECT ApPaymnt
  APPEND BLANK
  llNoThing = RLOCK()
  llNoThing = gfAdd_Info()

  *E301266,1 AKA (Start)
  * Assign generated batch no.  to ApPaymnt.Batch
  *REPLACE cPayType  WITH "A"                              ,;
          cPayMeth  WITH IIF(&lcTmpCredt..lNonAr,'N','A') ,;
          cBnkCode  WITH lcBankCode                       ,;
          cChkAcct  WITH lcChkAcnt                        ,;
          dPayDate  WITH ldBatDate                        ,;
          cFisFYear WITH lcGLFYear                        ,;
          cFspprdid WITH lcGLPeriod                       ,;
          cPayDocNo WITH &lcTmpCredt..Store               ,;
          cPayClNo  WITH &lcTmpCredt..Account             ,;
          cPayComp  WITH lcPayComp                        ,;
          nPayAmnt  WITH &lcTmpCredt..Amount              ,;
          cCurrCode WITH lcCurCode                        ,;
          nExRate   WITH lnExRate                         ,;
          nCurrUnit WITH lnCurrUnit                       ,;
          cPayRecSt WITH "O"
  
          
  REPLACE cPayType  WITH "A"                              ,;
          cPayMeth  WITH IIF(&lcTmpCredt..lNonAr,'N','A') ,;
          cBnkCode  WITH lcBankCode                       ,;
          cChkAcct  WITH lcChkAcnt                        ,;
          dPayDate  WITH ldBatDate                        ,;
          cFisFYear WITH lcGLFYear                        ,;
          cFspprdid WITH lcGLPeriod                       ,;
          cPayDocNo WITH &lcTmpCredt..Store               ,;
          cPayClNo  WITH &lcTmpCredt..Account             ,;
          cPayComp  WITH lcPayComp                        ,;
          nPayAmnt  WITH &lcTmpCredt..Amount              ,;
          cCurrCode WITH lcCurCode                        ,;
          nExRate   WITH lnExRate                         ,;
          nCurrUnit WITH lnCurrUnit                       ,;
          cPayRecSt WITH "O"                              ,;
          Batch     WITH lcBatSeqNo  
          
  *E301266,1 AKA (End)
  
  UNLOCK
  = lfUpdStp(lcTmpCredt, 3)
ENDIF        
* ----------------------------------------------------------------

FUNCTION lfUpdTmp

IF &lcTmpCredt..nSteps < 4
  SELECT (lcTmpCredt)
  llNoThing = RLOCK()
  llNoThing = gfAdd_Info()
  
  *B603056,1 Replace Credit description with the description of the
  *B603056,1 payment type
  *REPLACE cAdjAcct   WITH lcGLAcct2                       ,;
          cBnkCode   WITH lcBankCode                      ,;
          cChkAcct   WITH lcChkAcnt                       ,;
          TranDate   WITH ldBatDate                       ,;
          TranType   WITH "4"                             ,;
          Tran       WITH lcTrnSeqNo                      ,;
          Desc       WITH "PAYMENT-THANK YOU"             ,;
          Batch      WITH lcBatSeqNo                      ,;
          cCurrCode  WITH lcCurCode                       ,;
          nCurrUnit  WITH lnCurrUnit                      ,;
          nExRate    WITH lnExRate
  REPLACE cAdjAcct   WITH lcGLAcct2                       ,;
          cBnkCode   WITH lcBankCode                      ,;
          cChkAcct   WITH lcChkAcnt                       ,;
          TranDate   WITH ldBatDate                       ,;
          TranType   WITH "4"                             ,;
          Tran       WITH lcTrnSeqNo                      ,;
          Desc       WITH gfCodDes(cArPType,'CARPTYPE')   ,;
          Batch      WITH lcBatSeqNo                      ,;
          cCurrCode  WITH lcCurCode                       ,;
          nCurrUnit  WITH lnCurrUnit                      ,;
          nExRate    WITH lnExRate
  *B603056,1 end
  UNLOCK
  = lfUpdStp(lcTmpCredt, 4)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfUpdGL

GOTO BOTTOM IN (lcTmpCredt)
IF &lcTmpCredt..nSteps < 5
  lcTmpGLD   = gfTempName()
  TmpCrdtGL  = gfTempName()
  llOpGlDist = gfOpenFile(gcDataDir+"GLDIST" ,"GLDISTAC","SH")  

  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcTmpGLD)
  = gfOpenFile(gcWorkDir+lcTmpGLD ,"","EX")

*--MAN Start
*  SELECT (lcTmpCredt)
*  SELECT *, SUM(Amount) AS nAmt ;
    FROM (lcTmpCredt) ;
    GROUP BY cARGlAcc, cAdjAcct, Link_Code ; 
  INTO DBF (gcWorkDir+TmpCrdtGL)
  
*  SELECT (TmpCrdtGL)
*  SCAN 
*    DO GLDIST WITH &TmpCrdtGL..Link_Code,'002',ABS(&TmpCrdtGL..nAmt)      ,;
                   'CR',lcBatSeqNo,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &TmpCrdtGL..cAdjAcct,lcCurCode,lnCurrUnit,lnExRate
*    DO GLDIST WITH &TmpCrdtGL..Link_Code,'001',-ABS(&TmpCrdtGL..nAmt)     ,;
                   'CR',lcBatSeqNo,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &TmpCrdtGL..cARGlAcc,lcCurCode,lnCurrUnit,lnExRate
*  ENDSCAN 
    
*  USE IN (TmpCrdtGL)
    
  SELECT (lcTmpCredt)
  SCAN 
    DO GLDIST WITH &lcTmpCredt..Link_Code,'002',ABS(&lcTmpCredt..Amount)      ,;
                   'CR',&lcTmpCredt..Tran,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &lcTmpCredt..cAdjAcct,lcCurCode,lnCurrUnit,lnExRate
    DO GLDIST WITH &lcTmpCredt..Link_Code,'001',-ABS(&lcTmpCredt..Amount)     ,;
                   'CR',&lcTmpCredt..Tran,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &lcTmpCredt..cARGlAcc,lcCurCode,lnCurrUnit,lnExRate
  ENDSCAN 
*-- MAN End

  lcGLSeqNo = gfSequence("GLSESSION", gcAct_Comp)
  SELECT (lcTmpGLD)
  REPLACE ALL GLSESSION WITH lcGLSeqNo
  USE IN (lcTmpGLD)
    
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcTmpGLD)
  
  USE IN IIF(llOpGlDist, "GLDIST" , 0) 
  
  = lfUpdStp(lcTmpCredt, 5)
ENDIF


*:----------------------------------------------------------------
FUNCTION lfUpdStp
PARAMETERS lcFileName, lnStepNo
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcFileName)
*-- Update the needed file with the new step number..
llNoThing  = RLOCK()
REPLACE nSteps WITH lnStepNo
UNLOCK
SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!*************************************************************************
*E301245,1
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.

