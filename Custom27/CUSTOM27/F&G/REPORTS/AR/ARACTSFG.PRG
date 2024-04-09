*:***************************************************************************
*: Program file  : ARACTSFG.PRG
*: Program desc. : CUSTOMIZED ACTIVITY SUMMARY REPORT FOR F&G.
*! Date          : 06/08/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C200118
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARACTSFG
*:****************************************************************************
*: The DEBIT and CREDIT Type.
*:      Type  Meaning
*:      0     Returns (Credit Memo).
*:      1     Invoice, Direct Invoice.
*:      2     Debit Adjustment.                              DEBIT
*:      3     Charge Back (Debit On Account).                DEBIT
*:      4     Payment.                                       CREDIT
*:      5     Credit Adjustment.                             CREDIT
*:      6     Credit On Account.                             CREDIT
*:      7     Allowance (Credit Adjustment in this program). 
*:      8     Charge Back (Only in the ARHIST).
*:      9     Credit On Account (Only in the ARHIST).
*************************************************************************
*: Modifications :
*: B603984,1 BWA 11/23/2000 Add the KEYOFF transactions to the report.
*: B604415,1 BWA 05/02/2001 Incorrect balance .[START]
*:***************************************************************************

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*--The transaction manoth must be fill with a date to get from it the month.
IF ldRpFMon = {}
  =gfModalGen('TRM00250B00000','DIALOG', 'Transaction Month') 
  RETURN
ENDIF

*-- Get the Variables its Initial value
STORE SPACE(0) TO lcFilter , lcTerms , lcDivision , lcRegion , lcShipvia , lcSpeIns , lcCusClass

=lfChngDat()
=lfDatHdr()
=lfCreatTmp()
=lfRelation()
=lfvCrATVar()
=lfColctDat()

SELECT (lclines)
SET ORDER TO TAG SALESREP
SET RELATION TO &lclines..SALESREP INTO SALESREP ADDITIVE
LOCATE

*-- Comment the check of the CashAmnt due to Hkaled.
*DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + "&lclines..TotNet <> 0 AND &lclines..CashAmnt <> 0 "
DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + "&lclines..TotNet <> 0 "
SET CENTURY &lcCentury

*!*************************************************************
*! Name      : lfChngDat
*! Developer : Bassem Rafaat (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Change Date to the first day of the
*!           : month.
*!*************************************************************
*! Called from : ARACTSFG.PRGT
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfChngDat

*--Change the date to be look like the report in months not in days.
*--The change in the from only to be between the first of the month
*--and the date of the system date if the user didn't open the option.
IF DAY(ldRpFMon) # 01
  IF SET('DATE') = "BRITISH"
    ldRpFMon = CTOD('01'+'/'+STR(MONTH(ldRpFMon))+'/'+STR(YEAR(ldRpFMon)))
  ELSE
    ldRpFMon = CTOD(STR(MONTH(ldRpFMon))+'/01/'+STR(YEAR(ldRpFMon)))
  ENDIF  
ENDIF

*-- End of lfChngDat.
*!*************************************************************
*! Name      : lfDatHdr
*! Developer : Bassem Rafaat (BWA)
*! Date      : 06/08/2000
*! Purpose   : 1) Function to get Date Header.
*!           : 2) Make array to use it in filtering the date. 
*!*************************************************************
*! Called from : ARACTSFG.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfDatHdr

*--Make the arrays dimension 13 to use the laFltrDate array in the do 
*--case in collecting the data.

DIMENSION laDateTitl[13],laYearTitl[13],laNoOfDays[13] ,laFltrDate[13]
STORE {} TO laDateTitl , laFltrDate

laNoOfDays = '31'
STORE '30' TO laNoOfDays[4],laNoOfDays[6],laNoOfDays[9],laNoOfDays[11]

lcPrdMonth = ALLTRIM(STR(MONTH(ldRpFMon)))
lnPrdMonth = INT(VAL(lcPrdMonth))
lcPrdYear  = ALLTRIM(STR(YEAR(ldRpFMon)))

IF SET('DATE') = "BRITISH"
  FOR I=1 TO 13
    laDateTitl[I] = ALLTRIM(CMONTH(CTOD('01'+"/"+lcPrdMonth+"/"+lcPrdYear)))
    laYearTitl[I] = ALLTRIM(lcPrdYear)
    laNoOfDays[2] = IIF(MOD(YEAR(CTOD(lcPrdMonth+'/01/'+lcPrdYear)),4)=0 ,'29' ,'28' )
    ldNewDate     = (CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)+1)
    laFltrDate[I] = CTOD('01'+'/'+lcPrdMonth+'/'+lcPrdYear)
    lcPrdMonth    = ALLTRIM(STR(MONTH(ldNewDate)))
    lnPrdMonth    = INT(VAL(lcPrdMonth))
    lcPrdYear     = ALLTRIM(STR(YEAR(ldNewDate)))
  ENDFOR
ELSE
  FOR I=1 TO 13
    laDateTitl[I] = ALLTRIM(CMONTH(CTOD(lcPrdMonth+'/01/'+lcPrdYear)))
    laYearTitl[I] = ALLTRIM(lcPrdYear)
    laNoOfDays[2] = IIF(MOD(YEAR(CTOD(lcPrdMonth+'/01/'+lcPrdYear)),4)=0 ,'29' ,'28' )
    ldNewDate     = (CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)+1)
    laFltrDate[I] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)
    lcPrdMonth    = ALLTRIM(STR(MONTH(ldNewDate)))
    lnPrdMonth    = INT(VAL(lcPrdMonth))
    lcPrdYear     = ALLTRIM(STR(YEAR(ldNewDate)))
  ENDFOR
ENDIF

*-- End of lfDatHdr.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

*--Creat table for the trasactions month.
*Nets --> Net Sales    from 1 --> 12
*NetR --> Net Receipts from 1 --> 12
*Net  --> Net (Sales-Receipts) from 1 --> 12

CREATE CURSOR (lclines) (Account C(5)  , Salesrep C(3) , Crlimit N(15) , Disc N(5,2)    , TotNtSal N(15,2) , TotRecpt N(15,2),;
                         Nets1 N(15,2) , Nets2 N(15,2) , Nets3 N(15,2) , Nets4 N(15,2)  , Nets5 N(15,2)    , Nets6 N(15,2)    ,;
                         Nets7 N(15,2) , Nets8 N(15,2) , Nets9 N(15,2) , Nets10 N(15,2) , Nets11 N(15,2)   , Nets12 N(15,2)   ,;
                         NetR1 N(15,2) , NetR2 N(15,2) , NetR3 N(15,2) , NetR4 N(15,2)  , NetR5 N(15,2)    , NetR6 N(15,2)    ,;
                         NetR7 N(15,2) , NetR8 N(15,2) , NetR9 N(15,2) , NetR10 N(15,2) , NetR11 N(15,2)   , NetR12 N(15,2)   ,;
                         Net1 N(15,2)  , Net2 N(15,2)  , Net3 N(15,2)  , Net4 N(15,2)   , Net5 N(15,2)     , Net6 N(15,2)     ,;
                         Net7 N(15,2)  , Net8 N(15,2)  , Net9 N(15,2)  , Net10 N(15,2)  , Net11 N(15,2)    , Net12 N(15,2)    ,;
                         TotNet N(15,2), CashAmnt N(15,2) , Btname C(30) ,nCount N(6), nFrstPrd N(15,2))
ZAP
INDEX ON SALESREP+Account TAG SALESREP OF (gcWorkDir+lcLines)
INDEX ON Account          TAG Account  OF (gcWorkDir+lcLines)

*-- End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to prepare the relations of the program.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT CUSTOMER
SET RELATION TO Customer.Account INTO Credit ADDITIVE
SET RELATION TO Customer.Account INTO Debit  ADDITIVE
SET RELATION TO Customer.Account INTO Invhdr ADDITIVE
SET RELATION TO Customer.Account INTO Rethdr ADDITIVE
SET RELATION TO Customer.Account INTO Arhist ADDITIVE

*-- End of lfRelation.
*!*************************************************************
*! Name      : lfFillAll
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to fill Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillAll

DIMENSION laRpSource[1,1],laRpTarget[1,1],laRpSDivi[1,1],laRpTDivi[1,1],laRpSRegn[1,1],;
          laRpTRegn[1,1],laRpSShpv[1,1],laRpTShpv[1,1],laRpSSpIn[1,1],laRpTSPIn[1,1],;
          laRpSCLass[1,1],laRpTCLass[1,1],laRpSItmTp[1,1],laRpTItmTp[1,1],laRpSAdRsn[1,1],;
          laRpTAdRsn[1,1],laRpSDtRsn[1,1],laRpTDtRsn[1,1]

STORE '' TO laRpSource,laRpTarget,laRpSDivi,laRpTDivi,laRpSRegn,laRpTRegn,laRpSShpv,;
            laRpTShpv,laRpSSpIn,laRpTSPIn,laRpSCLass,laRpTCLass,laRpSItmTp,laRpTItmTp,;
            laRpSAdRsn,laRpTAdRsn,laRpSDtRsn,laRpTDtRsn

*--The Terms.
SELECT CODES
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CTERMCODE ' AND crltField = 'N';
                                        INTO ARRAY laRpSource

*--The Division.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CDIVISION ' AND crltField = 'N';
                                        INTO ARRAY laRpSDivi

*--The Region.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'REGION    ' AND crltField = 'N';
                                        INTO ARRAY laRpSRegn

*--The Ship Via.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SHIPVIA   ' AND crltField = 'N';
                                        INTO ARRAY laRpSShpv

*--The Special Instruction.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SPCINST   ' AND crltField = 'N';
                                        INTO ARRAY laRpSSpIn

*--The Customer Classification.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CLASS     ' AND crltField = 'N';
                                        INTO ARRAY laRpSCLass

*--The Payment Type.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CARPTYPE  ' AND crltField = 'N';
                                        INTO ARRAY laRpSItmTp

*--The Credit Adjustment Reason.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CCREDITCOD' AND crltField = 'N';
                                        INTO ARRAY laRpSAdRsn

*--The Debit Adjustment Reason.
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'TRANCODE  ' AND crltField = 'N';
                                        INTO ARRAY laRpSDtRsn

*-- End of lfFillAll.
*!*************************************************************
*! Name      : Header for the functions calling the mover.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to call mover Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : =lfvTerm(),
*!*************************************************************

*--The Terms.
FUNCTION lfvTerm
= gfMover(@laRpSource,@laRpTarget,'Terms ',.T.,'')

*--The Division.
FUNCTION lfvDvsion
= gfMover(@laRpSDivi,@laRpTDivi,'Division',.T.,'')

*--The Region.
FUNCTION lfvRegion
= gfMover(@laRpSRegn,@laRpTRegn,'Region',.T.,'')

*--The Ship Via.
FUNCTION lfvShpVia
= gfMover(@laRpSShpv,@laRpTShpv,'Ship Via',.T.,'')

*--The Special Instruction.
FUNCTION lfvSpcIns
= gfMover(@laRpSSpIn,@laRpTSPIn,'Special Instruction',.T.,'')

*--The Customer Classification.
FUNCTION lfvCustom
= gfMover(@laRpSCLass,@laRpTCLass,'Customer Classification',.T.,'')

*--The Payment Type.
FUNCTION lfvItmTyp
= gfMover(@laRpSItmTp,@laRpTItmTp,'Payment Type',.T.,'')

*--The Credit Adjustment Reason.
FUNCTION lfvCrdtTyp
= gfMover(@laRpSAdRsn,@laRpTAdRsn,'Credit Adjustment Reason',.T.,'')

*--The Debit Adjustment Reason.
FUNCTION lfvDbtTyp
= gfMover(@laRpSDtRsn,@laRpTDtRsn,'Debit Adjustment Reason',.T.,'')

*-- End of Calling Mover for all codes.
*!*************************************************************
*! Name      : lfvCrATVar
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

*--Salesrep
lnDataPos = ASCAN(laOGFxFlt,'SALESREP.REPCODE')
IF lnDataPos > 0
  LCRPEXP = STRTRAN(LCRPEXP ,'SALESREP.REPCODE','CUSTOMER.SALESREP' )
ENDIF

*--The filter
lcFilter = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

*--The dISCOUNT
lcFilter = lcFilter  + '.AND.' + IIF(lnRpDiscf = 0 .AND. lnRpDiscT <> 0 , 'CUSTOMER.DISC <= lnRpDiscT' ,;
                                 IIF(lnRpDiscf = 0 .AND. lnRpDiscT  = 0 , '.T.'     ,;
                                 'BETWEEN(CUSTOMER.DISC,lnRpDiscf,lnRpDiscT)'))

*--The Terms.
FOR lnInd = 1 TO ALEN(laRpTarget)
  lcTerms = lcTerms + PADR(laRpTarget[lnInd],6) + ' | '
ENDFOR
lcTerms = IIF(ALLTRIM(lcTerms) = '|','',lcTerms)

IF !EMPTY(lcTerms)
  lcFilter = lcFilter + ' AND CUSTOMER.ctermcode $ lcTerms'
ENDIF
  
*--The Division.
FOR lnInd = 1 TO ALEN(laRpTDivi)
  lcDivision = lcDivision + PADR(laRpTDivi[lnInd],6) + ' | '
ENDFOR
lcDivision = IIF(ALLTRIM(lcDivision) = '|','',lcDivision)

IF !EMPTY(lcDivision)
  lcFilter = lcFilter + ' AND CUSTOMER.cDivision $ lcDivision'
ENDIF

*--The Region.
FOR lnInd = 1 TO ALEN(laRpTRegn)
  lcRegion = lcRegion + PADR(laRpTRegn[lnInd],6) + ' | '
ENDFOR
lcRegion = IIF(ALLTRIM(lcRegion) = '|','',lcRegion)

IF !EMPTY(lcRegion)
  lcFilter = lcFilter + ' AND CUSTOMER.Region $ lcRegion'
ENDIF

*--The Ship Via.
FOR lnInd = 1 TO ALEN(laRpTShpv)
  lcShipvia = lcShipvia + PADR(laRpTShpv[lnInd],6) + ' | '
ENDFOR
lcShipvia = IIF(ALLTRIM(lcShipvia) = '|','',lcShipvia)

IF !EMPTY(lcShipvia)
  lcFilter = lcFilter + ' AND CUSTOMER.Shipvia $ lcShipvia'
ENDIF

*--The Special Instruction.
FOR lnInd = 1 TO ALEN(laRpTSPIn)
  lcSpeIns = lcSpeIns + PADR(laRpTSPIn[lnInd],6) + ' | '
ENDFOR
lcSpeIns = IIF(ALLTRIM(lcSpeIns) = '|','',lcSpeIns)

IF !EMPTY(lcSpeIns)
  lcFilter = lcFilter + ' AND CUSTOMER.Spcinst $ lcSpeIns'
ENDIF

*--The Customer Classification.
FOR lnInd = 1 TO ALEN(laRpTCLass)
  lcCusClass = lcCusClass + PADR(laRpTCLass[lnInd],6) + ' | '
ENDFOR
lcCusClass = IIF(ALLTRIM(lcCusClass) = '|','',lcCusClass)

IF !EMPTY(lcCusClass)
  lcFilter = lcFilter + ' AND CUSTOMER.Class $ lcCusClass'
ENDIF

*--This conditions will be modify in the scan of the arhist and credit.
*--
*--The Payment Type.
FOR lnInd = 1 TO ALEN(laRpTItmTp)
  lcItemTyp = lcItemTyp + PADR(laRpTItmTp[lnInd],6) + ' | '
ENDFOR
lcItemTyp = IIF(ALLTRIM(lcItemTyp) = '|','',lcItemTyp)

*--The Credit Adjustment Reason.
FOR lnInd = 1 TO ALEN(laRpTAdRsn)
  lcAdjRsn = lcAdjRsn + PADR(laRpTAdRsn[lnInd],6) + ' | '
ENDFOR
lcAdjRsn = IIF(ALLTRIM(lcAdjRsn) = '|','',lcAdjRsn)

*--This condition will be modify in the scan of the debit.
*--
*--The Debit Adjustment Reason.
FOR lnInd = 1 TO ALEN(laRpTDtRsn)
  lcDbtRsn = lcDbtRsn + PADR(laRpTDtRsn[lnInd],6) + ' | '
ENDFOR
lcDbtRsn = IIF(ALLTRIM(lcDbtRsn) = '|','',lcDbtRsn)

*-- End of lfvCrATVar.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the date from the credit and 
*!           : the debit file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias,lcAccount

lcAlias = ALIAS()
XACCOUNT= SPACE(0)

SELECT CUSTOMER
=SEEK('M')
lcFilter = IIF(EMPTY(lcFilter) , '.T.' , lcFilter)

SCAN WHILE Type+Account+Store ='M' FOR &lcFilter
  lcAccount=ACCOUNT
  
  *--The First period balance
  =lfFrstPrd()
  
  *--The net sales = Invoices-Credit memos
  *--Invocies     --> Invhdr file
  =lfNetSals()
  *--Credit memos --> RetHdr file
  =lfCrdtMem()
  
  *--The net receipts = (Cash receipt + customer credit adjustment) - customer debit adjustment
  *--Cash receipt & credit adjustment from credit and Arhist file. [Type 4 , 5 , 7]
  =lfCashRct()        && In Credit, Cash receipt take type  [4,5]
  =lfArhist()         && In Arhist, Cash receipt take type  [4] ,credit adjustment take type  [5,7]
                      &&            Debit adjustment take type  [2]
  *--Debit adjustment from Debit file. [Type 2]
  *--Charge Back (Debit On Account).   [Type 3]
  =lfDbtAdj()
  
  *--The under collection checks.
  =lfChkDate()

  *--Function callculate the net.
  =lfNetBlnc()

ENDSCAN

SELECT (lcAlias)

*-- End of lfcolctDat.
*!*************************************************************
*! Name      : lfFrstPrd
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the first period balance.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*The net sales = invoices - credit memos
*The net receipts = (cash receipt + credit adjustment) - debit adjustment
*The Amount balance from the first day till the day before the user select
*in the from transaction month.
* Net sales - Net receipts = [invoices - credit memos] - [(cash receipt + credit adjustment) - debit adjustment]
* Net sales - Net receipts =  invoices - credit memos  -   cash receipt - credit adjustment  + debit adjustment

FUNCTION lfFrstPrd
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT (lclines)
APPEND BLANK
REPLACE Crlimit  WITH CUSTOMER.Crlimit  ;
        Disc     WITH CUSTOMER.Disc     ;
        Btname   WITH CUSTOMER.Btname   ;
        Salesrep WITH CUSTOMER.Salesrep ;
        Account  WITH lcAccount

*--The invoice +ve value

SELECT INVHDR
IF SEEK(lcAccount)
  SELECT INVHDR
  SCAN REST WHILE ACCOUNT+INVOICE = lcAccount ;
            FOR Invdate < ldRpFMon            ;
            .AND. INVHDR.STATUS <> 'V'

    *--Fill the totnetsales field with the total charge from the invhdr.
    REPLACE &lclines..nFrstPrd WITH &lclines..nFrstPrd + INVHDR.TOTALCHG ;
            &lclines..TotNet WITH &lclines..TotNet +INVHDR.TOTALCHG

  ENDSCAN
ENDIF

*--The credit memo -ve value
SELECT RETHDR
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+CRMEMO = lcAccount ;
            FOR CRDATE < ldRpFMon            ;
            .AND. RETHDR.STATUS <> 'V'

    *--Subtract the totnetsales field with the total charge from the Rethdr.
    REPLACE &lclines..nFrstPrd WITH &lclines..nFrstPrd - RETHDR.TOTCREDIT ;
            &lclines..TotNet WITH &lclines..TotNet - RETHDR.TOTCREDIT

  ENDSCAN
ENDIF


*--The Cash receipt -ve value
SELECT CREDIT
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount ;
            FOR TRANDATE < ldRpFMon                       ; 
            .AND. IIF(EMPTY(lcItemTyp),CREDIT.CARPTYPE = "" , CREDIT.CARPTYPE   $ lcItemTyp ) ;
            .AND. IIF(EMPTY(lcAdjRsn) ,CREDIT.CCREDITCOD = "" , CREDIT.CCREDITCOD $ lcAdjRsn )  ;
            .AND. CREDIT.TranType $ '45'

    *--Fill the totnetsales field with the total charge from the CREDIT.
    REPLACE &lclines..nFrstPrd WITH &lclines..nFrstPrd + CREDIT.AMOUNT ;
            &lclines..TotNet WITH &lclines..TotNet + CREDIT.AMOUNT

  ENDSCAN
ENDIF


*--In Arhist, Cash receipt take type  [4] ,credit adjustment take type  [5,7]
*--           Debit adjustment take type  [2]
SELECT ARHIST

IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+HISTORY+TranType+Tran+Cinstalno = lcAccount     ;
            FOR TRANDATE < ldRpFMon                                       ;
            .AND. IIF(EMPTY(lcItemTyp),ARHIST.CARPTYPE = "",ARHIST.CARPTYPE $ lcItemTyp) ;
            .AND. IIF(EMPTY(lcAdjRsn),ARHIST.TRANCODE = "",ARHIST.TRANCODE $ lcAdjRsn );
            .AND. ARHIST.TranType $ '4572'

    *--Fill the totnetsales field with the total charge from the ARHIST.
    REPLACE &lclines..nFrstPrd WITH &lclines..nFrstPrd + ARHIST.AMOUNT ;
            &lclines..TotNet WITH &lclines..TotNet + ARHIST.AMOUNT

  ENDSCAN
ENDIF


*--Debit adjustment from Debit file. [Type 2]
SELECT DEBIT

IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+TRAN+Cinstalno+DTOS(TranDate) = lcAccount ;
            FOR TRANDATE < ldRpFMon                                 ; 
            .AND. IIF(EMPTY(lcDbtRsn),DEBIT.TRANCODE = "" ,DEBIT.TRANCODE $ lcDbtRsn);
            .AND. DEBIT.TranType $ '23'

    *--Fill the totnetsales field with the total charge from the DEBIT.
    REPLACE &lclines..nFrstPrd WITH &lclines..nFrstPrd + DEBIT.AMOUNT ;
            &lclines..TotNet WITH &lclines..TotNet + DEBIT.AMOUNT

  ENDSCAN
ENDIF

SELECT (lcAlias)

*-- End of lfFrstPrd.
*!*************************************************************
*! Name      : lfNetSals
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the data from the invhdr.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfNetSals
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR

IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+INVOICE = lcAccount      ;
            FOR BETWEEN(Invdate,ldRpFMon,ldRpTMon) ;
            .AND. INVHDR.STATUS <> 'V'

    *--Fill the totnetsales field with the total charge from the invhdr.
    REPLACE &lclines..TotNtSal WITH &lclines..TotNtSal + INVHDR.TOTALCHG ;
            &lclines..TotNet   WITH &lclines..TotNet + INVHDR.TOTALCHG
    DO CASE
      *--- Period # 1
      CASE BETWEEN(Invdate,ldRpFMon,laFltrDate[2]-1)
        REPLACE &lclines..Nets1 WITH &lclines..Nets1 + INVHDR.TOTALCHG

      *--- Period # 2
      CASE BETWEEN(Invdate,laFltrDate[2],laFltrDate[3]-1)
        REPLACE &lclines..Nets2 WITH &lclines..Nets2 + INVHDR.TOTALCHG

      *--- Period # 3
      CASE BETWEEN(Invdate,laFltrDate[3],laFltrDate[4]-1)
        REPLACE &lclines..Nets3 WITH &lclines..Nets3 + INVHDR.TOTALCHG

      *--- Period # 4
      CASE BETWEEN(Invdate,laFltrDate[4],laFltrDate[5]-1)
        REPLACE &lclines..Nets4 WITH &lclines..Nets4 + INVHDR.TOTALCHG

      *--- Period # 5
      CASE BETWEEN(Invdate,laFltrDate[5],laFltrDate[6]-1)
        REPLACE &lclines..Nets5 WITH &lclines..Nets5 + INVHDR.TOTALCHG

      *--- Period # 6
      CASE BETWEEN(Invdate,laFltrDate[6],laFltrDate[7]-1)
        REPLACE &lclines..Nets6 WITH &lclines..Nets6 + INVHDR.TOTALCHG

      *--- Period # 7
      CASE BETWEEN(Invdate,laFltrDate[7],laFltrDate[8]-1)
        REPLACE &lclines..Nets7 WITH &lclines..Nets7 + INVHDR.TOTALCHG

      *--- Period # 8
      CASE BETWEEN(Invdate,laFltrDate[8],laFltrDate[9]-1)
        REPLACE &lclines..Nets8 WITH &lclines..Nets8 + INVHDR.TOTALCHG

      *--- Period # 9
      CASE BETWEEN(Invdate,laFltrDate[9],laFltrDate[10]-1)
        REPLACE &lclines..Nets9 WITH &lclines..Nets9 + INVHDR.TOTALCHG

      *--- Period # 10
      CASE BETWEEN(Invdate,laFltrDate[10],laFltrDate[11]-1)
        REPLACE &lclines..Nets10 WITH &lclines..Nets10 + INVHDR.TOTALCHG

      *--- Period # 11
      CASE BETWEEN(Invdate,laFltrDate[11],laFltrDate[12]-1)
        REPLACE &lclines..Nets11 WITH &lclines..Nets11 + INVHDR.TOTALCHG

      *--- Period # 12
      CASE BETWEEN(Invdate,laFltrDate[12],laFltrDate[13]-1)
        REPLACE &lclines..Nets12 WITH &lclines..Nets12 + INVHDR.TOTALCHG
          
    ENDCASE
  ENDSCAN
ENDIF

*B603984,1 BWA 11/23/2000 Add the KEYOFF transactions to the report.[START]

*B604415,1 BWA 05/02/2001 Comment this lines. [Incorrect balance] .[START]
*SELECT ARHIST
*IF SEEK(lcAccount)
*  SCAN REST WHILE ACCOUNT+HISTORY+TranType+Tran+Cinstalno = lcAccount ;
*            FOR BETWEEN(TRANDATE,ldRpFMon,ldRpTMon)                   ;
*            .AND. IIF(EMPTY(lcItemTyp),ARHIST.CARPTYPE = "",ARHIST.CARPTYPE $ lcItemTyp );
*            .AND. IIF(EMPTY(lcAdjRsn+lcDbtRsn),ARHIST.TRANCODE = "",ARHIST.TRANCODE $ lcAdjRsn+lcDbtRsn);
*            .AND. ARHIST.TranType $ '1'

*    *--Fill the totnetsales field with the total charge from the ARHIST.
*    REPLACE &lclines..TotNtSal WITH &lclines..TotNtSal + ARHIST.AMOUNT ;
*            &lclines..TotNet   WITH &lclines..TotNet + ARHIST.AMOUNT

*    DO CASE
*      *--- Period # 1
*      CASE BETWEEN(TRANDATE,ldRpFMon,laFltrDate[2]-1)
*        REPLACE &lclines..Nets1 WITH &lclines..Nets1 + ARHIST.AMOUNT

*      *--- Period # 2
*      CASE BETWEEN(TRANDATE,laFltrDate[2],laFltrDate[3]-1)
*        REPLACE &lclines..Nets2 WITH &lclines..Nets2 + ARHIST.AMOUNT

*      *--- Period # 3
*      CASE BETWEEN(TRANDATE,laFltrDate[3],laFltrDate[4]-1)
*        REPLACE &lclines..Nets3 WITH &lclines..Nets3 + ARHIST.AMOUNT

*      *--- Period # 4
*      CASE BETWEEN(TRANDATE,laFltrDate[4],laFltrDate[5]-1)
*        REPLACE &lclines..Nets4 WITH &lclines..Nets4 + ARHIST.AMOUNT

*      *--- Period # 5
*      CASE BETWEEN(TRANDATE,laFltrDate[5],laFltrDate[6]-1)
*        REPLACE &lclines..Nets5 WITH &lclines..Nets5 + ARHIST.AMOUNT

*      *--- Period # 6
*      CASE BETWEEN(TRANDATE,laFltrDate[6],laFltrDate[7]-1)
*        REPLACE &lclines..Nets6 WITH &lclines..Nets6 + ARHIST.AMOUNT

*      *--- Period # 7
*      CASE BETWEEN(TRANDATE,laFltrDate[7],laFltrDate[8]-1)
*        REPLACE &lclines..Nets7 WITH &lclines..Nets7 + ARHIST.AMOUNT

*      *--- Period # 8
*      CASE BETWEEN(TRANDATE,laFltrDate[8],laFltrDate[9]-1)
*        REPLACE &lclines..Nets8 WITH &lclines..Nets8 + ARHIST.AMOUNT

*      *--- Period # 9
*      CASE BETWEEN(TRANDATE,laFltrDate[9],laFltrDate[10]-1)
*        REPLACE &lclines..Nets9 WITH &lclines..Nets9 + ARHIST.AMOUNT

*      *--- Period # 10
*      CASE BETWEEN(TRANDATE,laFltrDate[10],laFltrDate[11]-1)
*        REPLACE &lclines..Nets10 WITH &lclines..Nets10 + ARHIST.AMOUNT

*      *--- Period # 11
*      CASE BETWEEN(TRANDATE,laFltrDate[11],laFltrDate[12]-1)
*        REPLACE &lclines..Nets11 WITH &lclines..Nets11 + ARHIST.AMOUNT

*      *--- Period # 12
*      CASE BETWEEN(TRANDATE,laFltrDate[12],laFltrDate[13]-1)
*        REPLACE &lclines..Nets12 WITH &lclines..Nets12 + ARHIST.AMOUNT

*    ENDCASE
*  ENDSCAN
*ENDIF

*B604415,1 [end]
*B603984,1 [END]

SELECT (lcAlias)

*-- End of lfNetSals.
*!*************************************************************
*! Name      : lfCrdtMem
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the data from the rethdr file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrdtMem
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT RETHDR
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+CRMEMO = lcAccount      ;
            FOR BETWEEN(CRDATE,ldRpFMon,ldRpTMon) ;
            .AND. RETHDR.STATUS <> 'V'

    *--Subtract the totnetsales field with the total charge from the Rethdr.
    REPLACE &lclines..TotNtSal WITH &lclines..TotNtSal - RETHDR.TOTCREDIT ;
            &lclines..TotNet   WITH &lclines..TotNet - RETHDR.TOTCREDIT
    DO CASE
      *--- Period # 1
      CASE BETWEEN(CRDATE,ldRpFMon,laFltrDate[2]-1)
        REPLACE &lclines..Nets1 WITH &lclines..Nets1 - RETHDR.TOTCREDIT 

      *--- Period # 2
      CASE BETWEEN(CRDATE,laFltrDate[2],laFltrDate[3]-1)
        REPLACE &lclines..Nets2 WITH &lclines..Nets2 - RETHDR.TOTCREDIT

      *--- Period # 3
      CASE BETWEEN(CRDATE,laFltrDate[3],laFltrDate[4]-1)
        REPLACE &lclines..Nets3 WITH &lclines..Nets3 - RETHDR.TOTCREDIT

      *--- Period # 4
      CASE BETWEEN(CRDATE,laFltrDate[4],laFltrDate[5]-1)
        REPLACE &lclines..Nets4 WITH &lclines..Nets4 - RETHDR.TOTCREDIT

      *--- Period # 5
      CASE BETWEEN(CRDATE,laFltrDate[5],laFltrDate[6]-1)
        REPLACE &lclines..Nets5 WITH &lclines..Nets5 - RETHDR.TOTCREDIT

      *--- Period # 6
      CASE BETWEEN(CRDATE,laFltrDate[6],laFltrDate[7]-1)
        REPLACE &lclines..Nets6 WITH &lclines..Nets6 - RETHDR.TOTCREDIT

      *--- Period # 7
      CASE BETWEEN(CRDATE,laFltrDate[7],laFltrDate[8]-1)
        REPLACE &lclines..Nets7 WITH &lclines..Nets7 - RETHDR.TOTCREDIT

      *--- Period # 8
      CASE BETWEEN(CRDATE,laFltrDate[8],laFltrDate[9]-1)
        REPLACE &lclines..Nets8 WITH &lclines..Nets8 - RETHDR.TOTCREDIT

      *--- Period # 9
      CASE BETWEEN(CRDATE,laFltrDate[9],laFltrDate[10]-1)
        REPLACE &lclines..Nets9 WITH &lclines..Nets9 - RETHDR.TOTCREDIT

      *--- Period # 10
      CASE BETWEEN(CRDATE,laFltrDate[10],laFltrDate[11]-1)
        REPLACE &lclines..Nets10 WITH &lclines..Nets10 - RETHDR.TOTCREDIT

      *--- Period # 11
      CASE BETWEEN(CRDATE,laFltrDate[11],laFltrDate[12]-1)
        REPLACE &lclines..Nets11 WITH &lclines..Nets11 - RETHDR.TOTCREDIT

      *--- Period # 12
      CASE BETWEEN(CRDATE,laFltrDate[12],laFltrDate[13]-1)
        REPLACE &lclines..Nets12 WITH &lclines..Nets12 - RETHDR.TOTCREDIT
          
    ENDCASE
  ENDSCAN
ENDIF

*B603984,1 BWA 11/23/2000 Add the KEYOFF transactions to the report.[START]

*B604415,1 BWA 05/02/2001 Comment this lines. [Incorrect balance] .[START]
*SELECT ARHIST
*IF SEEK(lcAccount)
*  SCAN REST WHILE ACCOUNT+HISTORY+TranType+Tran+Cinstalno = lcAccount ;
*            FOR BETWEEN(TRANDATE,ldRpFMon,ldRpTMon)                   ;
*            .AND. IIF(EMPTY(lcItemTyp),ARHIST.CARPTYPE = "",ARHIST.CARPTYPE $ lcItemTyp );
*            .AND. IIF(EMPTY(lcAdjRsn+lcDbtRsn),ARHIST.TRANCODE = "",ARHIST.TRANCODE $ lcAdjRsn+lcDbtRsn);
*            .AND. ARHIST.TranType $ '0'

*    *--Fill the totnetsales field with the total charge from the ARHIST.
*    REPLACE &lclines..TotNtSal WITH &lclines..TotNtSal - ARHIST.AMOUNT ;
*            &lclines..TotNet   WITH &lclines..TotNet - ARHIST.AMOUNT

*    DO CASE
*      *--- Period # 1
*      CASE BETWEEN(TRANDATE,ldRpFMon,laFltrDate[2]-1)
*        REPLACE &lclines..Nets1 WITH &lclines..Nets1 - ARHIST.AMOUNT

*      *--- Period # 2
*      CASE BETWEEN(TRANDATE,laFltrDate[2],laFltrDate[3]-1)
*        REPLACE &lclines..Nets2 WITH &lclines..Nets2 - ARHIST.AMOUNT

*      *--- Period # 3
*      CASE BETWEEN(TRANDATE,laFltrDate[3],laFltrDate[4]-1)
*        REPLACE &lclines..Nets3 WITH &lclines..Nets3 - ARHIST.AMOUNT

*      *--- Period # 4
*      CASE BETWEEN(TRANDATE,laFltrDate[4],laFltrDate[5]-1)
*        REPLACE &lclines..Nets4 WITH &lclines..Nets4 - ARHIST.AMOUNT

*      *--- Period # 5
*      CASE BETWEEN(TRANDATE,laFltrDate[5],laFltrDate[6]-1)
*        REPLACE &lclines..Nets5 WITH &lclines..Nets5 - ARHIST.AMOUNT

*      *--- Period # 6
*      CASE BETWEEN(TRANDATE,laFltrDate[6],laFltrDate[7]-1)
*        REPLACE &lclines..Nets6 WITH &lclines..Nets6 - ARHIST.AMOUNT

*      *--- Period # 7
*      CASE BETWEEN(TRANDATE,laFltrDate[7],laFltrDate[8]-1)
*        REPLACE &lclines..Nets7 WITH &lclines..Nets7 - ARHIST.AMOUNT

*      *--- Period # 8
*      CASE BETWEEN(TRANDATE,laFltrDate[8],laFltrDate[9]-1)
*        REPLACE &lclines..Nets8 WITH &lclines..Nets8 - ARHIST.AMOUNT

*      *--- Period # 9
*      CASE BETWEEN(TRANDATE,laFltrDate[9],laFltrDate[10]-1)
*        REPLACE &lclines..Nets9 WITH &lclines..Nets9 - ARHIST.AMOUNT

*      *--- Period # 10
*      CASE BETWEEN(TRANDATE,laFltrDate[10],laFltrDate[11]-1)
*        REPLACE &lclines..Nets10 WITH &lclines..Nets10 - ARHIST.AMOUNT

*      *--- Period # 11
*      CASE BETWEEN(TRANDATE,laFltrDate[11],laFltrDate[12]-1)
*        REPLACE &lclines..Nets11 WITH &lclines..Nets11 - ARHIST.AMOUNT

*      *--- Period # 12
*      CASE BETWEEN(TRANDATE,laFltrDate[12],laFltrDate[13]-1)
*        REPLACE &lclines..Nets12 WITH &lclines..Nets12 - ARHIST.AMOUNT

*    ENDCASE
*  ENDSCAN
*ENDIF
*B604415,1 [END]
*B603984,1 [END]

SELECT (lcAlias)

*-- End of lfCrdtMem.
*!*************************************************************
*! Name      : lfCashRct
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the data from the credit file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCashRct
PRIVATE lcAlias

lcAlias = ALIAS()

*-- The first time we scan for the Cash receipt.
SELECT CREDIT
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount ;
            FOR BETWEEN(TRANDATE,ldRpFMon,ldRpTMon)       ; 
            .AND. IIF(EMPTY(lcItemTyp),CREDIT.CARPTYPE = "",CREDIT.CARPTYPE  $ lcItemTyp);
            .AND. CREDIT.TranType $ '4'

    *--Fill the totnetRecpt field with the total net from the CREDIT.
    REPLACE &lclines..TotRecpt WITH &lclines..TotRecpt - CREDIT.AMOUNT ;
            &lclines..TotNet   WITH &lclines..TotNet + CREDIT.AMOUNT
    DO CASE
      *--- Period # 1
      CASE BETWEEN(TRANDATE,ldRpFMon,laFltrDate[2]-1)
        REPLACE &lclines..NetR1 WITH &lclines..NetR1 - CREDIT.AMOUNT 

      *--- Period # 2
      CASE BETWEEN(TRANDATE,laFltrDate[2],laFltrDate[3]-1)
        REPLACE &lclines..NetR2 WITH &lclines..NetR2 - CREDIT.AMOUNT

      *--- Period # 3
      CASE BETWEEN(TRANDATE,laFltrDate[3],laFltrDate[4]-1)
        REPLACE &lclines..NetR3 WITH &lclines..NetR3 - CREDIT.AMOUNT

      *--- Period # 4
      CASE BETWEEN(TRANDATE,laFltrDate[4],laFltrDate[5]-1)
        REPLACE &lclines..NetR4 WITH &lclines..NetR4 - CREDIT.AMOUNT

      *--- Period # 5
      CASE BETWEEN(TRANDATE,laFltrDate[5],laFltrDate[6]-1)
        REPLACE &lclines..NetR5 WITH &lclines..NetR5 - CREDIT.AMOUNT

      *--- Period # 6
      CASE BETWEEN(TRANDATE,laFltrDate[6],laFltrDate[7]-1)
        REPLACE &lclines..NetR6 WITH &lclines..NetR6 - CREDIT.AMOUNT

      *--- Period # 7
      CASE BETWEEN(TRANDATE,laFltrDate[7],laFltrDate[8]-1)
        REPLACE &lclines..NetR7 WITH &lclines..NetR7 - CREDIT.AMOUNT

      *--- Period # 8
      CASE BETWEEN(TRANDATE,laFltrDate[8],laFltrDate[9]-1)
        REPLACE &lclines..NetR8 WITH &lclines..NetR8 - CREDIT.AMOUNT

      *--- Period # 9
      CASE BETWEEN(TRANDATE,laFltrDate[9],laFltrDate[10]-1)
        REPLACE &lclines..NetR9 WITH &lclines..NetR9 - CREDIT.AMOUNT

      *--- Period # 10
      CASE BETWEEN(TRANDATE,laFltrDate[10],laFltrDate[11]-1)
        REPLACE &lclines..NetR10 WITH &lclines..NetR10 - CREDIT.AMOUNT

      *--- Period # 11
      CASE BETWEEN(TRANDATE,laFltrDate[11],laFltrDate[12]-1)
        REPLACE &lclines..NetR11 WITH &lclines..NetR11 - CREDIT.AMOUNT

      *--- Period # 12
      CASE BETWEEN(TRANDATE,laFltrDate[12],laFltrDate[13]-1)
        REPLACE &lclines..NetR12 WITH &lclines..NetR12 - CREDIT.AMOUNT

    ENDCASE
  ENDSCAN
ENDIF

*-- The first time we scan for the credit adjustment. [CREDIT.TranType $ '5']
*-- Once agian due to the credit reason.[IIF(EMPTY(lcAdjRsn) ,CREDIT.CCREDITCOD = "",CREDIT.CCREDITCOD $ lcAdjRsn )]
SELECT CREDIT
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount ;
            FOR BETWEEN(TRANDATE,ldRpFMon,ldRpTMon)       ; 
            .AND. IIF(EMPTY(lcItemTyp),CREDIT.CARPTYPE = "",CREDIT.CARPTYPE  $ lcItemTyp);
            .AND. IIF(EMPTY(lcAdjRsn) ,CREDIT.CCREDITCOD = "",CREDIT.CCREDITCOD $ lcAdjRsn );
            .AND. CREDIT.TranType $ '5'

    *--Fill the totnetsales field with the total charge from the CREDIT.
    REPLACE &lclines..TotRecpt WITH &lclines..TotRecpt - CREDIT.AMOUNT ;
            &lclines..TotNet   WITH &lclines..TotNet + CREDIT.AMOUNT
    DO CASE
      *--- Period # 1
      CASE BETWEEN(TRANDATE,ldRpFMon,laFltrDate[2]-1)
        REPLACE &lclines..NetR1 WITH &lclines..NetR1 - CREDIT.AMOUNT 

      *--- Period # 2
      CASE BETWEEN(TRANDATE,laFltrDate[2],laFltrDate[3]-1)
        REPLACE &lclines..NetR2 WITH &lclines..NetR2 - CREDIT.AMOUNT

      *--- Period # 3
      CASE BETWEEN(TRANDATE,laFltrDate[3],laFltrDate[4]-1)
        REPLACE &lclines..NetR3 WITH &lclines..NetR3 - CREDIT.AMOUNT

      *--- Period # 4
      CASE BETWEEN(TRANDATE,laFltrDate[4],laFltrDate[5]-1)
        REPLACE &lclines..NetR4 WITH &lclines..NetR4 - CREDIT.AMOUNT

      *--- Period # 5
      CASE BETWEEN(TRANDATE,laFltrDate[5],laFltrDate[6]-1)
        REPLACE &lclines..NetR5 WITH &lclines..NetR5 - CREDIT.AMOUNT

      *--- Period # 6
      CASE BETWEEN(TRANDATE,laFltrDate[6],laFltrDate[7]-1)
        REPLACE &lclines..NetR6 WITH &lclines..NetR6 - CREDIT.AMOUNT

      *--- Period # 7
      CASE BETWEEN(TRANDATE,laFltrDate[7],laFltrDate[8]-1)
        REPLACE &lclines..NetR7 WITH &lclines..NetR7 - CREDIT.AMOUNT

      *--- Period # 8
      CASE BETWEEN(TRANDATE,laFltrDate[8],laFltrDate[9]-1)
        REPLACE &lclines..NetR8 WITH &lclines..NetR8 - CREDIT.AMOUNT

      *--- Period # 9
      CASE BETWEEN(TRANDATE,laFltrDate[9],laFltrDate[10]-1)
        REPLACE &lclines..NetR9 WITH &lclines..NetR9 - CREDIT.AMOUNT

      *--- Period # 10
      CASE BETWEEN(TRANDATE,laFltrDate[10],laFltrDate[11]-1)
        REPLACE &lclines..NetR10 WITH &lclines..NetR10 - CREDIT.AMOUNT

      *--- Period # 11
      CASE BETWEEN(TRANDATE,laFltrDate[11],laFltrDate[12]-1)
        REPLACE &lclines..NetR11 WITH &lclines..NetR11 - CREDIT.AMOUNT

      *--- Period # 12
      CASE BETWEEN(TRANDATE,laFltrDate[12],laFltrDate[13]-1)
        REPLACE &lclines..NetR12 WITH &lclines..NetR12 - CREDIT.AMOUNT

    ENDCASE
  ENDSCAN
ENDIF

SELECT (lcAlias)

*-- End of lfCashRct.
*!*************************************************************
*! Name      : lfArhist
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the data from the Arhist file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfArhist
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT ARHIST

IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+HISTORY+TranType+Tran+Cinstalno = lcAccount ;
            FOR BETWEEN(TRANDATE,ldRpFMon,ldRpTMon)                   ;
            .AND. IIF(EMPTY(lcItemTyp),ARHIST.CARPTYPE = "",ARHIST.CARPTYPE $ lcItemTyp );
            .AND. IIF(EMPTY(lcAdjRsn+lcDbtRsn),ARHIST.TRANCODE = "",ARHIST.TRANCODE $ lcAdjRsn+lcDbtRsn);
            .AND. ARHIST.TranType $ '4572'

    *--Fill the totnetsales field with the total charge from the ARHIST.
    REPLACE &lclines..TotRecpt WITH &lclines..TotRecpt - ARHIST.AMOUNT ;
            &lclines..TotNet   WITH &lclines..TotNet + ARHIST.AMOUNT
    DO CASE
      *--- Period # 1
      CASE BETWEEN(TRANDATE,ldRpFMon,laFltrDate[2]-1)
        REPLACE &lclines..NetR1 WITH &lclines..NetR1 - ARHIST.AMOUNT 

      *--- Period # 2
      CASE BETWEEN(TRANDATE,laFltrDate[2],laFltrDate[3]-1)
        REPLACE &lclines..NetR2 WITH &lclines..NetR2 - ARHIST.AMOUNT

      *--- Period # 3
      CASE BETWEEN(TRANDATE,laFltrDate[3],laFltrDate[4]-1)
        REPLACE &lclines..NetR3 WITH &lclines..NetR3 - ARHIST.AMOUNT

      *--- Period # 4
      CASE BETWEEN(TRANDATE,laFltrDate[4],laFltrDate[5]-1)
        REPLACE &lclines..NetR4 WITH &lclines..NetR4 - ARHIST.AMOUNT

      *--- Period # 5
      CASE BETWEEN(TRANDATE,laFltrDate[5],laFltrDate[6]-1)
        REPLACE &lclines..NetR5 WITH &lclines..NetR5 - ARHIST.AMOUNT

      *--- Period # 6
      CASE BETWEEN(TRANDATE,laFltrDate[6],laFltrDate[7]-1)
        REPLACE &lclines..NetR6 WITH &lclines..NetR6 - ARHIST.AMOUNT

      *--- Period # 7
      CASE BETWEEN(TRANDATE,laFltrDate[7],laFltrDate[8]-1)
        REPLACE &lclines..NetR7 WITH &lclines..NetR7 - ARHIST.AMOUNT

      *--- Period # 8
      CASE BETWEEN(TRANDATE,laFltrDate[8],laFltrDate[9]-1)
        REPLACE &lclines..NetR8 WITH &lclines..NetR8 - ARHIST.AMOUNT

      *--- Period # 9
      CASE BETWEEN(TRANDATE,laFltrDate[9],laFltrDate[10]-1)
        REPLACE &lclines..NetR9 WITH &lclines..NetR9 - ARHIST.AMOUNT

      *--- Period # 10
      CASE BETWEEN(TRANDATE,laFltrDate[10],laFltrDate[11]-1)
        REPLACE &lclines..NetR10 WITH &lclines..NetR10 - ARHIST.AMOUNT

      *--- Period # 11
      CASE BETWEEN(TRANDATE,laFltrDate[11],laFltrDate[12]-1)
        REPLACE &lclines..NetR11 WITH &lclines..NetR11 - ARHIST.AMOUNT

      *--- Period # 12
      CASE BETWEEN(TRANDATE,laFltrDate[12],laFltrDate[13]-1)
        REPLACE &lclines..NetR12 WITH &lclines..NetR12 - ARHIST.AMOUNT

    ENDCASE
  ENDSCAN
ENDIF

SELECT (lcAlias)

*-- End of lfArhist.
*!*************************************************************
*! Name      : lfDbtAdj
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect the data from the Debit file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfDbtAdj
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT DEBIT

IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+TRAN+Cinstalno+DTOS(TranDate) = lcAccount ;
            FOR BETWEEN(TRANDATE,ldRpFMon,ldRpTMon)       ; 
            .AND. IIF(EMPTY(lcDbtRsn),DEBIT.TRANCODE = "",DEBIT.TRANCODE $ lcDbtRsn);
            .AND. DEBIT.TranType $ '23'
            
    *--Fill the totnetsales field with the total charge from the DEBIT.
    REPLACE &lclines..TotRecpt WITH &lclines..TotRecpt - DEBIT.AMOUNT ;
            &lclines..TotNet   WITH &lclines..TotNet + DEBIT.AMOUNT
    DO CASE
      *--- Period # 1
      CASE BETWEEN(TRANDATE,ldRpFMon,laFltrDate[2]-1)
        REPLACE &lclines..NetR1 WITH &lclines..NetR1 - DEBIT.AMOUNT

      *--- Period # 2
      CASE BETWEEN(TRANDATE,laFltrDate[2],laFltrDate[3]-1)
        REPLACE &lclines..NetR2 WITH &lclines..NetR2 - DEBIT.AMOUNT

      *--- Period # 3
      CASE BETWEEN(TRANDATE,laFltrDate[3],laFltrDate[4]-1)
        REPLACE &lclines..NetR3 WITH &lclines..NetR3 - DEBIT.AMOUNT

      *--- Period # 4
      CASE BETWEEN(TRANDATE,laFltrDate[4],laFltrDate[5]-1)
        REPLACE &lclines..NetR4 WITH &lclines..NetR4 - DEBIT.AMOUNT

      *--- Period # 5
      CASE BETWEEN(TRANDATE,laFltrDate[5],laFltrDate[6]-1)
        REPLACE &lclines..NetR5 WITH &lclines..NetR5 - DEBIT.AMOUNT

      *--- Period # 6
      CASE BETWEEN(TRANDATE,laFltrDate[6],laFltrDate[7]-1)
        REPLACE &lclines..NetR6 WITH &lclines..NetR6 - DEBIT.AMOUNT

      *--- Period # 7
      CASE BETWEEN(TRANDATE,laFltrDate[7],laFltrDate[8]-1)
        REPLACE &lclines..NetR7 WITH &lclines..NetR7 - DEBIT.AMOUNT

      *--- Period # 8
      CASE BETWEEN(TRANDATE,laFltrDate[8],laFltrDate[9]-1)
        REPLACE &lclines..NetR8 WITH &lclines..NetR8 - DEBIT.AMOUNT

      *--- Period # 9
      CASE BETWEEN(TRANDATE,laFltrDate[9],laFltrDate[10]-1)
        REPLACE &lclines..NetR9 WITH &lclines..NetR9 - DEBIT.AMOUNT

      *--- Period # 10
      CASE BETWEEN(TRANDATE,laFltrDate[10],laFltrDate[11]-1)
        REPLACE &lclines..NetR10 WITH &lclines..NetR10 - DEBIT.AMOUNT

      *--- Period # 11
      CASE BETWEEN(TRANDATE,laFltrDate[11],laFltrDate[12]-1)
        REPLACE &lclines..NetR11 WITH &lclines..NetR11 - DEBIT.AMOUNT

      *--- Period # 12
      CASE BETWEEN(TRANDATE,laFltrDate[12],laFltrDate[13]-1)
        REPLACE &lclines..NetR12 WITH &lclines..NetR12 - DEBIT.AMOUNT

    ENDCASE
  ENDSCAN
ENDIF

SELECT (lcAlias)

*-- End of lfDbtAdj.
*!*************************************************************
*! Name      : lfChkDate
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Collect cash receipt for the option
*!           : under collection checks.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfChkDate
PRIVATE lcAlias

lcAlias = ALIAS()

SELECT CREDIT
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+TRAN+DTOS(TranDate) = lcAccount ;
            FOR IIF(ldRpFchk <> {} ,TRANDATE >= ldRpFchk ,;
                IIF(ldRpTchk ={} .AND. ldRpFchk={},.T.   ,;  
                BETWEEN(TRANDATE,ldRpFchk,ldRpTchk)))     ; 
            .AND. CREDIT.TranType $ '45'

    *--Fill the CashAmnt field with the total charge from the CREDIT.
    REPLACE &lclines..CashAmnt WITH &lclines..CashAmnt - CREDIT.AMOUNT

  ENDSCAN
ENDIF

SELECT ARHIST
IF SEEK(lcAccount)
  SCAN REST WHILE ACCOUNT+HISTORY+TranType+Tran+Cinstalno = lcAccount ;
            FOR IIF(ldRpFchk <> {} ,TRANDATE >= ldRpFchk ,;
                IIF(ldRpTchk ={} .AND. ldRpFchk={},.T.   ,;  
                BETWEEN(TRANDATE,ldRpFchk,ldRpTchk)))     ; 
            .AND. ARHIST.TranType = '4'

    *--Fill the CashAmnt field with the total charge from the ARHIST.
    REPLACE &lclines..CashAmnt WITH &lclines..CashAmnt - ARHIST.AMOUNT

  ENDSCAN
ENDIF

SELECT (lcAlias)

*-- End of lfChkDate.
*!*************************************************************
*! Name      : lfNetBlnc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to Callculate the net balance.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfNetBlnc
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT (lcLines)

IF SEEK(lcAccount)
  REPLACE &lclines..Net1  WITH &lclines..NetS1  - &lclines..NetR1  + &lclines..nFrstPrd
  REPLACE &lclines..Net2  WITH &lclines..NetS2  - &lclines..NetR2  + &lclines..Net1
  REPLACE &lclines..Net3  WITH &lclines..NetS3  - &lclines..NetR3  + &lclines..Net2
  REPLACE &lclines..Net4  WITH &lclines..NetS4  - &lclines..NetR4  + &lclines..Net3
  REPLACE &lclines..Net5  WITH &lclines..NetS5  - &lclines..NetR5  + &lclines..Net4
  REPLACE &lclines..Net6  WITH &lclines..NetS6  - &lclines..NetR6  + &lclines..Net5
  REPLACE &lclines..Net7  WITH &lclines..NetS7  - &lclines..NetR7  + &lclines..Net6
  REPLACE &lclines..Net8  WITH &lclines..NetS8  - &lclines..NetR8  + &lclines..Net7
  REPLACE &lclines..Net9  WITH &lclines..NetS9  - &lclines..NetR9  + &lclines..Net8
  REPLACE &lclines..Net10 WITH &lclines..NetS10 - &lclines..NetR10 + &lclines..Net9
  REPLACE &lclines..Net11 WITH &lclines..NetS11 - &lclines..NetR11 + &lclines..Net10
  REPLACE &lclines..Net12 WITH &lclines..NetS12 - &lclines..NetR12 + &lclines..Net11
                    
ENDIF

SELECT (lcAlias)

*-- End of lfNetBlnc.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

lcTitle = 'Transaction Month'

DO (gcRepHome + gcAct_Appl + '\Monthrng.SPR')

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvDisc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Showes Discount range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDisc()
*!*************************************************************
FUNCTION lfvDisc

lnRpDiscf = IIF(lnRpDiscT = 0 , 0 , lnRpDiscf)
lcTitle = 'Discount %'

DO (gcRepHome + gcAct_Appl + '\Discrng.SPR')
lnRpDiscF = IIF(lnRpDiscT = 0 AND lnRpDiscF <> 0 , 0 , lnRpDiscF)

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvChekF
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Validate date from range screen's to get it 01 
*!           : the first day in the month .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvChekF()
*!*************************************************************
FUNCTION lfvChekF
PRIVATE lcMonth,lcYear

lcMonth = ALLTRIM(STR(MONTH(ldRpFMon)))
lcYear  = ALLTRIM(STR(YEAR(ldRpFMon)))

IF DAY(ldRpFMon) # 01
  IF SET('DATE') = "BRITISH"
    ldRpFMon = CTOD('01'+'/'+lcMonth+'/'+lcYear)
  ELSE
    ldRpFMon = CTOD(lcMonth+'/01/'+lcYear)
  ENDIF
ENDIF

*-- End of lfvChekF.
*!*************************************************************
*! Name      : lfvChekTo
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Validate date from range screen's to get it 01 
*!           : the first day in the month .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvChekTo()
*!*************************************************************
FUNCTION lfvChekTo
PRIVATE lcMonth,lcYear

lcMonth = ALLTRIM(STR(MONTH(ldRpTMon)))
lcYear  = ALLTRIM(STR(YEAR(ldRpTMon)))

IF DAY(ldRpTMon) # 01
  IF SET('DATE') = "BRITISH"
    ldRpTMon = CTOD('01'+'/'+lcMonth+'/'+lcYear)
  ELSE
    ldRpTMon = CTOD(lcMonth+'/01/'+lcYear)
  ENDIF
ENDIF

*-- End of lfvChekTo.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Validate Transaction Month screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************
FUNCTION lfvPbOk

IF ldRpFMon > ldRpTMon
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldRpFMon)
ELSE
  CLEAR READ
ENDIF

*-- End of lfvPbOk.
*!*************************************************************
*! Name      : lfvpDisc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Validate Discount % screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpDisc()
*!*************************************************************
FUNCTION lfvpDisc

IF lnRpDiscF > lnRpDiscT
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(lnRpDiscF)
ELSE
  CLEAR READ
ENDIF

*-- End of lfvpDisc.
*!*************************************************************
*! Name      : lfvCheck
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Showes Inrange date for the check date.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDscont()
*!*************************************************************
FUNCTION lfvCheck

lcTitle = 'Check Date'

DO (gcRepHome + gcAct_Appl + '\Chekrng.SPR')   && Run the advance payment screen 

*-- End of lfvCheck.
*!*************************************************************
*! Name      : lfvpbokDis
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Validate Check Date screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbokDis()
*!*************************************************************
FUNCTION lfvpChek

IF ldRpFchk > ldRpTchk
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldRpFchk)
ELSE
  CLEAR READ
ENDIF

*-- End of lfvpChek.
*!*************************************************************
*! Name      : lfsrRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/08/2000
*! Purpose   : Sales rep. in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
ENDCASE

*-- End of lfsrRep.
*!*************************************************************
*! Name      : lfAccount 
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 06/08/2000
*! Purpose   : Account in range browse screen .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAccount ()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfAccount
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
ENDCASE

*-- End of lfAccount.
