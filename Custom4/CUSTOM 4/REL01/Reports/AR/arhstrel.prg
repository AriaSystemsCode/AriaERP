*:***************************************************************************
*: Program file  : ARHSTREL.PRG
*: Program desc. : CUSTOMIZED CUSTOMER HISTORY REPORT FOR RELIQ.
*: Date          : 12/13/2006
*: System        : Aria 4XP
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : AYMAN MAHMOUD AHMED(AYM)
*: AutoTask      : T20060927.0069
*: TRACK #       : C200727
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARHSTREL
*:***************************************************************************

*--Note that the transaction types used to update the "ARHIST", "DEBIT" and "CREDIT" files are..
*--     Type  Meaning
*--      0     Returns (Credit Memo).
*--      1     Invoice, Direct Invoice.
*--      2     Debit Adjustment.
*--      3     Charge Back (Debit On Account).
*--      4     Payment.
*--      5     Credit Adjustment.
*--      6     Credit On Account.
*--      7     Allowance (Credit Adjustment in this program).
*--      8     Charge Back (Only in the ARHIST).
*--      9     Credit On Account (Only in the ARHIST).

*!*	_screen.Visible = .T.
*!*	ACTIVATE WINDOW trace
*!*	SUSPEND

IF !USED(lcArhist_B)
  =gfOpenTABLE(oAriaApplication.DataDir + "ARHIST" , "ARHISTHT",'SH', @lcArhist_B, .T.)
ENDIF

=lfCreatTmp()  && Create work cursor.
=lfCollData()  && Collect the data.

SELECT (lcHdrRel)
SET RELATION TO
SET ORDER TO TAG CustUnq IN (lcHdrRel)
SET RELATION TO CustCode INTO (lclinsRel) ADDITIVE
SET SKIP TO (lclinsRel)

IF RECCOUNT(lcHdrRel) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
ELSE
  DO gfDispRe WITH EVAL('lcRpFormNa')
ENDIF

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/01/2004
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ARHSTREL.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData

SELECT (lcHdrRel)
SET RELATION TO CustCode + InvoiceNo INTO (lclinsRel) ADDITIVE
=gfOpenTABLE(oAriaApplication.DataDir+ "CUSTOMER" , "CUSTOMER",'SH', , .T.)
=gfOpenTABLE(oAriaApplication.DataDir + "INVHDR" , "INVHDRA",'SH', , .T.)
=gfOpenTABLE(oAriaApplication.DataDir + "RETHDR" , "RETHDRA",'SH', , .T.)


STORE " .T. " TO lcCustExp
STORE " .T. " TO lcHdrExp

*Customer Filter
lcCustFile = ''
lcCustFile = lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
llAccFltr   = !EMPTY(lcCustFile ) AND USED(lcCustFile ) AND RECCOUNT(lcCustFile ) > 0
IF llAccFltr   
  SELECT (lcCustFile )
  INDEX ON ACCOUNT  TAG (lcCustFile )
  lcCustExp=lcCustExp+" AND SEEK(ACCOUNT,'"+lcCustFile +"')"
ELSE
  IF TYPE("lcCustFile ") = "C" AND USED(lcCustFile )
    USE IN (lcCustFile )
  ENDIF
  lcCustFile = ''
ENDIF

* SalesRep Filter
lcRepFltr= ''
lcRepFltr= lfCheckFilter(1, 'INVHDR.REP1')
llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
IF llRepFltr   
  SELECT (lcRepFltr)
  INDEX ON REPCODE TAG (lcRepFltr)
  lcHdrExp=lcHdrExp+" AND (SEEK( INVHDR.REP1,'"+lcRepFltr+"') .OR. SEEK( INVHDR.REP2,'"+lcRepFltr+"'))"
ELSE
  IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
    USE IN (lcRepFltr)
  ENDIF
  lcRepFltr= ''
ENDIF
lnInvPos = lfItmPos('INVHDR.INVDATE')

IF !EMPTY(laOGFxFlt[lnInvPos ,6])
 ldStrtDPst = CTOD(SUBSTR(laOGFxFlt[lnInvPos ,6],1, ATC('|',laOGFxFlt[lnInvPos ,6])-1))
 ldEndDPst  = CTOD(SUBSTR(laOGFxFlt[lnInvPos ,6],   ATC('|',laOGFxFlt[lnInvPos ,6])+1))
 lcHdrExp=lcHdrExp+" AND BETWEEN(INVDATE,ldStrtDPst,ldEndDPst) "
ENDIF 
  
SELECT CUSTOMER
=gfseek('M')

SCAN REST WHILE TYPE ='M' FOR &lcCustExp
  WAIT WINDOW 'Selecting Records For Customer ...' + Customer.Account NOWAIT
  SELECT INVHDR    &&--Get the data from the Invhdr file.
  IF GFSEEK(CUSTOMER.ACCOUNT , "INVHDR")
    SCAN REST WHILE ACCOUNT + INVOICE = CUSTOMER.ACCOUNT FOR &lcHdrExp AND INVHDR.STATUS <> "V"
      INSERT INTO (lcHdrRel) (CustCode       , InvoiceNo      , CustName  ) ;
                     VALUES  (INVHDR.ACCOUNT , INVHDR.INVOICE , ALLTRIM(CUSTOMER.BTNAME))

      INSERT INTO (lclinsRel) (CustCode       , InvoiceNo      , InvDate        , OpnBlncVal      , Store        ,;
                               TranValu);
                   VALUES     (INVHDR.ACCOUNT , INVHDR.INVOICE , INVHDR.INVDATE , INVHDR.TOTALCHG , INVHDR.STORE ,;
                               INVHDR.TOTALCHG)
    ENDSCAN
  ENDIF
ENDSCAN


*--Get the data from the Arhist file.
IF RECCOUNT(lcHdrRel) > 0
  SELECT (lcHdrRel)
  LOCATE
  SCAN
    IF GFSEEK(CustCode + InvoiceNo , "ARHIST")
      STORE 0 TO lnBalance
      lnBalance = EVAL(lclinsRel+'.OpnBlncVal')
      SELECT ARHIST
      SCAN REST WHILE ACCOUNT + TRAN + CINSTALNO = EVAL(lcHdrRel+'.CustCode') + EVAL(lcHdrRel+'.InvoiceNo')
        IF TRANTYPE = "1"
          lcHistory = ARHIST.HISTORY
          IF GFSEEK(EVAL(lcHdrRel+'.CustCode') + ARHIST.HISTORY , lcArhist_B)
            SELECT (lcArhist_B)
            SCAN REST WHILE ACCOUNT + HISTORY + TRANTYPE + TRAN + CINSTALNO = EVAL(lcHdrRel+'.CustCode') + lcHistory
              IF TRANTYPE = "0"  
                lcCusCrm= EVAL(lcHdrRel+'.CustCode') + EVAL(lcArhist_B+'.TRAN')
                IF GFSEEK(lcCusCrm, "RETHDR") AND RETHDR.INVOICE == EVAL(lcHdrRel+'.InvoiceNo')
                  lnBalance = lnBalance + EVAL(lcArhist_B+'.AMOUNT')
                  INSERT INTO (lclinsRel) (CustCode                    , TranValu                   , CrdtMmo                  ,OpnBlncVal , RANo        , InvoiceNo  );
                               VALUES     (EVAL(lcArhist_B+'.ACCOUNT') , EVAL(lcArhist_B+'.AMOUNT') , EVAL(lcArhist_B+'.TRAN') ,lnBalance  , RETHDR.RANo , EVAL(lcHdrRel+'.INVOICENO'))
                ENDIF
              ENDIF

              IF TRANTYPE = "4"
                lnBalance = lnBalance + EVAL(lcArhist_B+'.AMOUNT')
                INSERT INTO (lclinsRel) (CustCode                    , ChkPymnt                  , TranValu                     ,;
                                         OpnBlncVal                  , PaymntRcvd                , DatPymnt                     , InvoiceNo) ;
                             VALUES     (EVAL(lcArhist_B+'.ACCOUNT') , EVAL(lcArhist_B+'.STORE') , EVAL(lcArhist_B+'.AMOUNT')   ,;
                                         lnBalance                   , EVAL(lcArhist_B+'.AMOUNT'), EVAL(lcArhist_B+'.TRANDATE') , EVAL(lcHdrRel+'.INVOICENO'))
              ENDIF

              IF TRANTYPE $ "2356789"
                IF TRANTYPE $ "3689"
                  lnBalance = lnBalance - EVAL(lcArhist_B+'.AMOUNT')
                ELSE &&"257"
                  lnBalance = lnBalance + EVAL(lcArhist_B+'.AMOUNT')
                ENDIF
                INSERT INTO (lclinsRel) (CustCode                    , TranValu                     , OpnBlncVal                  ,;
                                         PaymntRcvd                  , DatPymnt                     , InvoiceNo)                   ;
                             VALUES     (EVAL(lcArhist_B+'.ACCOUNT') , EVAL(lcArhist_B+'.AMOUNT')   , lnBalance                   ,;
                                         EVAL(lcArhist_B+'.AMOUNT')  , EVAL(lcArhist_B+'.TRANDATE') , EVAL(lcHdrRel+'.INVOICENO'))
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
WAIT CLEAR

*--End of lfCollData.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/01/2004
*! Purpose   : Create Temp. file that hold the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE lcAlias

lcAlias = ALIAS()

CREATE CURSOR (lcHdrRel) (CustCode C(5) , InvoiceNo C(6) , CustName C(30))
INDEX ON CustCode UNIQUE TAG 'CustUnq' of (oAriaApplication.WorkDir + lcHdrRel + ".CDX")
INDEX ON CustCode + InvoiceNo TAG CustInvo of (oAriaApplication.WorkDir + lcHdrRel + ".CDX")


CREATE CURSOR (lclinsRel) (CustCode C(5)    , InvoiceNo C(6)     , OpnBlncVal N(14,2) ,;
                           TranValu N(14,2) , PaymntRcvd N(14,2) , DatPymnt D         ,;
                           ChkPymnt C(8)    , RANo C(6)          , CrdtMmo C(6)       ,;
                           InvDate D        , Store C(8))

INDEX ON CustCode + InvoiceNo TAG CustCode of (oAriaApplication.WorkDir+ lclinsRel + ".CDX")

SELECT(lcAlias)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfSetAcct
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/01/2004
*! Purpose   : Customer account in range browse screen.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSetAcct()
*!*************************************************************
*! Note      : S symbol is [S,Set] ,R symbol is [R ,Reset]
*!*************************************************************
FUNCTION lfSetAcct
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SET ORDER TO CUSTOMER IN CUSTOMER
    GO TOP IN CUSTOMER
ENDCASE

*--End of lfSetAcct.
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : BASSEM RAAFAT ERNEST
*! Date      : 09/01/2004
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP 
    SET ORDER TO 
ENDCASE

*--End of lfSeTSRep.


*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn


  *!*************************************************************
*! Name      : lfItmPos
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
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
  


