*:************************************************************************
*: Program file  : PSACTVY.PRG
*: Program desc. : Activity Report.
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: AR.
*:      Developer: TAK - Timour Abdalla Khalil 
*:           Date: 06/10/98
*:************************************************************************
*B602829,1 SSH 22/04/99 Fix the bug of print zero total payed
*B602956,1 TAK 05/31/99 Fix wrong refund figure for exchange record.
*B603687,1 ABD 06/272000 Fix bug that the header of the report print always 
*B603687,1 ABD           "Timanad"  in case of printing the company name.
*B603687,1 ABD           Fix bug in the FRX
*:************************************************************************
*--Initialize report variabels.
lnDtRow = ASCAN(laOgFxFlt,'POSTRAN.TRANDATE')
lcLDate = SUBSTR(laOgFxFlt[lnDtRow,6],1,AT('|',laOgFxFlt[lnDtRow,6])-1)
lcHDate = SUBSTR(laOgFxFlt[lnDtRow,6],AT('|',laOgFxFlt[lnDtRow,6])+1,10)

*--Read The existant payments.
DIME laPayCodes[1,3]
lnI=0
*B#602829 SSH 22/04/99 Commented out to because of changing codes file structure.
*SELECT CODES
*SEEK gcAct_Comp+'CTERMCODE'
*SCAN WHILE cComp_id+cfld_name=gcAct_Comp+'CTERMCODE' FOR cRltField='N'
SELECT CODES
=SEEK ('N'+'CTERMCODE')
SCAN WHILE cDefCode+cfld_name='N'+'CTERMCODE' FOR cRltField='N'
*B#602829 SSH (END)
  lnI=lnI+1
  DIME laPayCodes[lnI,3]
  laPayCodes[lnI,1] = cCode_No
  laPayCodes[lnI,2] = cDiscRep
  laPayCodes[lnI,3] = 0
ENDSCAN


STORE 0.00 TO lnSales,lnReturn,lnCredit
SELECT POSTRAN
lcTmpTran = gfTempName()
CREATE CURSOR (lcTmpTran) (cPosSeq C(6),Tran C(6),Account C(5),cType C(17),;
                           cPay C(17),nAmt N(8,2),nPaid N(8,2),cRpTyp C(1))
INDEX ON cRpTyp+cPosSeq TAG (lcTmpTran)

lcRpPrint = IIF(EMPTY(lcRpPrint),'A',lcRpPrint)
lnAmount  = 0
IF lcRpPrint $ 'ASR'
  SELECT POSTRAN
  IF !EMPTY(lcRPAcnt)
    SET ORDER TO TAG Postrna
    SEEK lcRPAcnt
    lcWhlCnd="WHILE Account=lcRPAcnt"
  ELSE
    SET ORDER TO
    lcWhlCnd=''
  ENDIF
  SCAN &lcWhlCnd FOR &lcRpExp AND !lFlag AND cType = IIF(lcRpPrint='A','',lcRpPrint)
    SELECT (lcTmpTran)
    IF !SEEK('1'+POSTRAN.cPosSeq)
      APPEND BLANK
      REPLACE cPosSeq WITH POSTRAN.cPosSeq,;
              cType   WITH IIF(POSTRAN.cType='S','Sales','Customer Return'),;
              Account WITH POSTRAN.Account,;
              cRpTyp  WITH '1' 
    ELSE
      *--Case of multiple payments.
      IF POSTRAN.TranType = '4' AND nPaid<>0
        lcTran = Tran
        lcType = cType
        APPEND BLANK
        REPLACE cPosSeq WITH POSTRAN.cPosSeq,;
                Tran    WITH lcTran ,;
                cType   WITH lcType ,;
                cRpTyp  WITH '1'    ,; 
                Account WITH POSTRAN.Account 
      ENDIF
    ENDIF  
    *--Replace Amount.
    IF (POSTRAN.TranType = '1' AND POSTRAN.cType='S') OR ;
       (POSTRAN.TranType = '0' AND POSTRAN.cType='R')
      REPLACE Tran WITH POSTRAN.Tran,;
              nAmt WITH nAmt + POSTRAN.nAmount
      lnAmount = POSTRAN.nAmount
      =lfAcomulate()
    ELSE
      IF POSTRAN.TranType = '4'
*B602956,1 Take the refund that done in exchange into consederation.
*        REPLACE cPay  WITH POSTRAN.Desc1,;
*                nPaid WITH nPaid + IIF(POSTRAN.cType='R',-ABS(POSTRAN.nAmount),ABS(POSTRAN.nAmount))
*        lnAmount = IIF(POSTRAN.cType='R',-ABS(POSTRAN.nAmount),ABS(POSTRAN.nAmount))
        REPLACE cPay  WITH POSTRAN.Desc1,;
                nPaid WITH nPaid + IIF(POSTRAN.cType='R',-ABS(POSTRAN.nAmount),-(POSTRAN.nAmount))
        lnAmount = IIF(POSTRAN.cType='R',-ABS(POSTRAN.nAmount),-(POSTRAN.nAmount))
*B602956,1 End.
        =lfAcomulate()
      ENDIF
    ENDIF

  ENDSCAN
ENDIF
IF lcRpPrint $ 'AC'
  SELECT CREDIT
  IF !EMPTY(lcRPAcnt)
    SET ORDER TO TAG Credit
    SEEK lcRPAcnt
    lcWhlCnd="WHILE Account=lcRPAcnt"
  ELSE
    SET ORDER TO
    lcWhlCnd=''
  ENDIF
  SCAN &lcWhlCnd FOR !(TranType $ '067') AND BETWEEN(TranDate,CTOD(lcLDate),CTOD(lcHDate))
    SELECT (lcTmpTran)
    APPEND BLANK
    REPLACE cPosSeq WITH CREDIT.Batch,;
            Tran    WITH CREDIT.Tran ,;
            cType   WITH 'Customer Payment',;
            cRpTyp  WITH '1',; 
            Account WITH CREDIT.Account ,; 
            nPaid   WITH ABS(CREDIT.Amount)
    lnCredit = lnCredit + nPaid
  ENDSCAN

  *--Keyed off Payments (Out of Poit of sale)
  SELECT POSTRAN
  SET ORDER TO TAG Postrna
  SELECT ARHIST
  IF !EMPTY(lcRPAcnt)
    SET ORDER TO TAG Arhistt
    SEEK lcRPAcnt
    lcWhlCnd="WHILE Account=lcRPAcnt"
  ELSE
    SET ORDER TO
    lcWhlCnd=''
  ENDIF
  SCAN &lcWhlCnd FOR TranType $ '4567' AND BETWEEN(TranDate,CTOD(lcLDate),CTOD(lcHDate))
    IF !SEEK(ARHIST.Account+ARHIST.Tran,'POSTRAN')
      SELECT (lcTmpTran)
      APPEND BLANK
      REPLACE cPosSeq WITH ARHIST.History,;
              Tran    WITH ARHIST.Tran ,;
              cType   WITH 'Customer Payment',;
              cRpTyp  WITH '1',; 
              Account WITH ARHIST.Account ,; 
              nPaid   WITH ABS(ARHIST.Amount)
      lnCredit = lnCredit + nPaid
    ENDIF
  ENDSCAN

ENDIF


*-Total Payments.
SELECT (lcTmpTran)
lnTotalPay = 0
llFrst     = .T.
IF !EMPTY(laPayCodes[1,1])
FOR lnI=1 TO ALEN(laPayCodes,1)
  IF laPayCodes[lnI,3] <> 0
    lnTotalPay = lnTotalPay + laPayCodes[lnI,3]
    APPEND BLANK
    REPLACE cPay   WITH laPayCodes[lnI,2],;
            cRpTyp WITH '2',; 
            nPaid  WITH laPayCodes[lnI,3]
    IF llFrst
      REPLACE cPosSeq WITH 'Totals',;
              cType   WITH 'By Payment type'
      llFrst = .F.
    ENDIF
  ENDIF
ENDFOR
ENDIF
lnTotalPay = lnTotalPay + lnCredit

lnDebit = 0
IF lcRpPrint = 'A'
  SELECT DEBIT
  IF !EMPTY(lcRPAcnt)
    SET ORDER TO TAG Debit
    SEEK lcRPAcnt
    lcWhlCnd="WHILE Account=lcRPAcnt"
  ELSE
    SET ORDER TO
    lcWhlCnd=''
  ENDIF
  SUM REST Amount &lcWhlCnd FOR !(TranType $ '13') AND BETWEEN(TranDate,CTOD(lcLDate),CTOD(lcHDate)) ;
    TO lnDebit 

  *--Keyed off Payments (Out of Poit of sale)
  SELECT POSTRAN
  SET ORDER TO TAG Postrna
  SELECT ARHIST
  IF !EMPTY(lcRPAcnt)
    SET ORDER TO TAG Arhistt
    SEEK lcRPAcnt
    lcWhlCnd="WHILE Account=lcRPAcnt"
  ELSE
    SET ORDER TO
    lcWhlCnd=''
  ENDIF
  SCAN &lcWhlCnd FOR TranType $ '23' AND BETWEEN(TranDate,CTOD(lcLDate),CTOD(lcHDate))
    IF !SEEK(ARHIST.Account+ARHIST.Tran,'POSTRAN')
      lnDebit = lnDebit + ARHIST.Amount
    ENDIF
  ENDSCAN
ENDIF

*--Point of the current site location.
SELECT WAREHOUS
GO TOP
IF gfGetMemVar('M_SYSTYPE') = 'P' AND (OCCURS('CM',gcCmpModules)<>0)
  LOCATE FOR cSiteId = gcCurSite
ENDIF


SELECT (lcTmpTran)
GO TOP
IF EOF()
  *--No Transactions was done! Nothing to print.
  =gfDialog("I",'No Transactions was done! Nothing to print.')
ELSE
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
SELECT (lcTmpTran)
USE
SELECT POSTRAN
SET ORDER TO TAG POSTRAN
RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Timour A. K.
*! Date      : 06/10/98
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*--Start date range is today date.
lnDtRow = ASCAN(laOgFxFlt,'POSTRAN.TRANDATE')
IF EMPTY(laOgFxFlt[lnDtRow,6])
  laOgFxFlt[lnDtRow,6] = DTOC(gdSysDate)+"|"+DTOC(gdSysDate)
ENDIF

*--Open report files.
= gfOpenFile(gcDataDir+'CODES','CCODE_NO','SH')
= gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
= gfOpenFile(gcDataDir+'CREDIT','CREDIT','SH')
= gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
RETURN


*!*************************************************************
*! Name      : lfAcomulate
*! Developer : Timour A. K.
*! Date      : 06/10/98
*! Purpose   : Accomulate totals.
*!*************************************************************
*! Example   : =lfAcomulate()
*!*************************************************************
FUNCTION lfAcomulate

DO CASE
  CASE (POSTRAN.TranType = '0' AND POSTRAN.cType='R')
     lnReturn= lnReturn + lnAmount
  CASE (POSTRAN.TranType = '1' AND POSTRAN.cType='S')
     lnSales = lnSales + lnAmount
  CASE POSTRAN.TranType = '4'
     lnArElm = ASCAN(laPayCodes,POSTRAN.TranCode)
     IF lnArElm<>0
       laPayCodes[lnArElm+2] = laPayCodes[lnArElm+2] + lnAmount
     ENDIF
  CASE !(POSTRAN.TranType $ '014')
     lnCredit = lnCredit + lnAmount
ENDCASE
RETURN


*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN



*!*************************************************************
*! Name      : lfvCust
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate customers.
*!*************************************************************
FUNCTION lfvCust
lcRPAcnt = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcRPAcnt = lcOldValue
  RETURN
ENDIF
lcAcctName = SYS(18)
IF !EMPTY(lcRPAcnt) AND !SEEK('M'+lcRPAcnt,'CUSTOMER')
  xAccount = lcRPAcnt
  DO CUSBROWM WITH xAccount
  lcRPAcnt = xAccount
  &lcAcctName = IIF(!EMPTY(lcRPAcnt),lcRPAcnt,lcOldValue)
ENDIF
RETURN
