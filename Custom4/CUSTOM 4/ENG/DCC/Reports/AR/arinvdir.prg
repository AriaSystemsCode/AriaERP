*:***************************************************************************
*: Program file  : ARPINVDIR.PRG
*: Program desc. : Customized Consolidated Invoice Report for [DIR03]
*: Date          : 11/11/2008
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Tarek Mohammed Ibrahim 
*: Tracking Job Number : C201064
*: Ticket No. T20070214.0007   
*:**************************************************************************
*:Modification:
*tmi 08/02/2009
*B609037,1 TMI 10/12/2009 base the lnVat on the tax rate of the invoice not of the style [T20080807.0002   ]
*! B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024]
*B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024]
*B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[T20130109.0024]
*B611201,1 MAA 12/10/2016 Fix report to display Pick tiket, order, and customer PO numbers [T20161003.0016]
*:**************************************************************************
PRIVATE lcCentury

*- Define report as LandScape
loogScroll.cCROrientation = 'L'

*--Set Date's Year to (yyyy) to print a complete date on the report
STORE '' TO lcCentury
lcCentury = SET('CENTURY')
SET CENTURY ON
DECLARE laCompAdd[5,1] , laSoldTo[6,1] 
llPrntComp = .T.
llRpinvnot=.T.
laCompAdd  = ''        && Array to hold the Company address
laSoldTo   = ''        && Array to hold the Sold To address
llEndGroup = .F.       && Flag to know if we are at the end of the Group
lcTaxRefDs = ALLTRIM(gfGetMemVar('M_TAX_REFE'))

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)

lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)

*- use the style 'zzzz ... ' to be added to the charges lines
lcChrgStyle = REPLICATE('z',lnstylewid)

lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxraT'

lcKeySeek = ''
DO CASE
CASE lcRpFormN = 'S'
  lcKeySeek = 'Store'
CASE lcRpFormN = 'D'
  lcKeySeek = 'Piktkt'
CASE lcRpFormN = 'E'
  lcKeySeek = 'EMPLOYEE'
ENDCASE

=lfCrtTmp()
=lfColDataS()

=lfPrnHdr()
=lfSetRela()

IF llRpToExcel
  =lfToExcel()
ELSE
  DO gfDispRe WITH EVAL('lcRpForm')  
ENDIF  

SET CENTURY &lcCentury

*:**************************************************************************
*:* Name        : lfWhenRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/13/2008
*:* Purpose     : create a temp index to filter the report on only to the invoices with CONSOL = 'Y' and STATUS <> 'V'
*:***************************************************************************
FUNCTION lfWhenRep

*!*	SELECT INVHDR
*!*	INDEX ON INVOICE FOR CONSOL='Y' AND STATUS<>'V' TO (gcWorkDir+lcTmpIdx) 

*-- end of lfWhenRep.

*:**************************************************************************
*:* Name        : lfCrtTmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2008
*:* Purpose     : Function to Creat the temp. File for form ARPINVDC
*:***************************************************************************
FUNCTION lfCrtTmp
*--Create lcTmpDate file only if Consolidate invoice type =Sort by site because, Date Range found in this form only
*--Create lcTmpCstPo file to hold the Custpo in there is only one custpo applies to all Stores or Piktkt

CREATE TABLE (gcWorkDir + lcSumTemp);
             (Invoice C(6),STORE c(8), Piktkt C(6) ,dShipDate D(8),;
              Style C(lnstylewid), Desc C(30) , Price N(12,2) , QTY N(7) , ;
              NetVal N(10,2) , Vat N(12,5) , Gross N(12,5) , Type C(1), ;
              LPRINT C(1),CCONTREF c(30),ORDER C(6),EMPLOYEE C(12))
INDEX ON INVOICE+&lcKeySeek.+STYLE+STR(PRICE,12,2)+ORDER TAG &lcSumTemp
INDEX ON INVOICE+STYLE TAG CHARGS

*- Explicitly set the order to (lcSumTemp)
SET ORDER TO &lcSumTemp

CREATE TABLE (gcWorkDir + lcTmpDate)(Invoice C(6),FDate D(8) , ToDate D(8))
INDEX ON (INVOICE) TAG (lcTmpDate) OF (lcTmpDate)

CREATE TABLE (gcWorkDir + lcTmpCstPo)(Invoice C(6),Account C(5),CustPo C(15) , llPrint L(1), InvDate D(8))
INDEX ON (INVOICE) TAG (lcTmpCstPo) OF (lcTmpCstPo)

*--End function lfCrtTmp.
*!*************************************************************
*! Name        : lfColDataS
*! Developer   : Nader Nabil (NNA)
*! Date        : 02/12/2004
*! Purpose     : Function to Collect data for Report ARINVDIS
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColDataS()
*!*************************************************************
FUNCTION lfColDataS
PRIVATE lcCustPo , lcInvoice , lnCustPo
STORE '' TO lcCustPo , lcInvoice 
STORE 0 TO lnCustPo
set step on
LOCAL lcKeyFld
lcKeyFld = ''
DO CASE
CASE lcRpFormN = 'S'
  lcKeyFld = 'Consinvl.Store'
CASE lcRpFormN = 'D'
  lcKeyFld = 'Consinvh.Piktkt'
CASE lcRpFormN = 'E'
  lcKeyFld = 'ORDLINE.EMPLOYEE'
ENDCASE
SELECT &lcSumTemp

lnInvPos = ASCAN(loOgScroll.laOgVrFlt,'INVHDR.INVOICE')
lnInvPos = ASUBSCRIPT(loOgScroll.laOgVrFlt,lnInvPos,1)
lcInvCurs = loOgScroll.laOgVrFlt[lnInvPos,6]
lnSlcted = 0
IF !EMPTY(lcInvCurs) AND TYPE('lcInvCurs') = 'C' AND USED(lcInvCurs)
  SELECT &lcInvCurs
  COUNT TO lnSlcted
ENDIF

lcInvCurs = IIF(lnSlcted>0,loOgScroll.laOgVrFlt[lnInvPos,6],'INVHDR')

SELECT &lcInvCurs
SCAN
  IF lnSlcted>0
    =SEEK(&lcInvCurs..INVOICE,'INVHDR')
  ENDIF
  IF INVHDR.STATUS='V' AND INVHDR.CONSOL<>'Y'
    LOOP
  ENDIF  
  
  SELECT INVHDR  

  STORE {} TO ldFromDate , ldToDate
  IF SEEK(INVHDR.INVOICE,'Consinvh')
    SELECT Consinvh
    *XX
*!*	    SCAN REST WHILE Invoice+Store+Order = INVHDR.INVOICE ;
*!*	                FOR SEEK(Consinvh.order+Consinvh.piktkt,'PIKTKT') AND ;
*!*	                    SEEK(Consinvh.order+Consinvh.Store+Consinvh.piktkt,'PACK_HDR')
    SCAN REST WHILE Invoice+Store+Order = INVHDR.INVOICE ;
                FOR SEEK(Consinvh.order+Consinvh.piktkt,'PIKTKT') 
       
        =SEEK(Consinvh.order+Consinvh.Store+Consinvh.piktkt,'PACK_HDR')
*XX
        SELECT PACK_HDR

        *--Get the Date range to show it on the report
        IF PACK_HDR.dShipDate<>{}
          IF EMPTY(ldFromDate) AND EMPTY(ldToDate)
            ldFromDate = PACK_HDR.dShipDate
            ldToDate   = PACK_HDR.dShipDate
          ELSE
            ldFromDate = IIF(PACK_HDR.dShipDate < ldFromDate , PACK_HDR.dShipDate , ldFromDate )
            ldToDate   = IIF(PACK_HDR.dShipDate > ldToDate   , PACK_HDR.dShipDate , ldToDate )
          ENDIF
        ENDIF

        *- Collecting Charges Data
        IF !SEEK(Consinvh.INVOICE+lcChrgStyle,lcSumTemp,'CHARGS')

          IF SEEK(Consinvh.INVOICE , 'INVCHRG')          
            SELECT INVCHRG
            lnCrgCnt = 0
            SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE
              SELECT (lcSumTemp)
              IF lcRpFormN $ 'SE' OR lnCrgCnt = 0                
                APPEND BLANK 
                *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[T20130109.0024][Start]
*!*                  REPLACE Invoice  WITH INVCHRG.Invoice                                         ,;
*!*                          Store    WITH INVCHRG.CSTORE                                          ,;
*!*                          Piktkt   WITH CHR(255)                                                ,;
*!*                          EMPLOYEE WITH CHR(255)                                                ,;
*!*                          STYLE    WITH lcChrgStyle                                             ,;
*!*                          Desc     WITH gfCodDes(INVCHRG.cchrgcode , 'CORDCHG')                 ,;
*!*                          Price    WITH 999999999.99                                            ,;            
*!*                          NetVal   WITH INVCHRG.nchrgamnt                                       ,;
*!*                          Vat      WITH IIF(INVCHRG.ntaxrate>0                                  ,;
*!*                                   (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)                ,;
*!*                          Gross    WITH NetVal + Vat                                            ,; 
*!*                          LPRINT   WITH 'T'   

*B611201,1 MAA 12/10/2016 Fix report to display Pick tiket, order, and customer PO numbers [T20161003.0016] [Start]

*!*	                REPLACE Invoice  WITH INVCHRG.Invoice                                         ,;
*!*	                        Store    WITH INVCHRG.CSTORE                                          ,;
*!*	                        Piktkt   WITH CHR(255)                                          ,;
*!*	                        EMPLOYEE WITH CHR(255)                                                ,;
*!*	                        STYLE    WITH lcChrgStyle                                             ,;
*!*	                        Desc     WITH gfCodDes(INVCHRG.cchrgcode , 'CORDCHG')                 ,;
*!*	                        Price    WITH 0.00                                            ,;            
*!*	                        NetVal   WITH INVCHRG.nchrgamnt                                       ,;
*!*	                        Vat      WITH IIF(INVCHRG.ntaxrate>0                                  ,;
*!*	                                 (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)                ,;
*!*	                        Gross    WITH NetVal + Vat                                            ,; 
*!*	                        LPRINT   WITH 'T'   
               *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[T20130109.0024][END]                    

				=SEEK('O'+INVCHRG.Order,'ORDHDR')

				REPLACE Invoice  WITH INVCHRG.Invoice                                 ,;
                        Store    WITH INVCHRG.CSTORE                                          ,;
                        Piktkt   WITH INVCHRG.Piktkt                                          ,;
                        EMPLOYEE WITH CHR(255)                                                ,;
                        STYLE    WITH lcChrgStyle                                             ,;
                        Desc     WITH gfCodDes(INVCHRG.cchrgcode , 'CORDCHG')                 ,;
                        Price    WITH 0.00                                                    ,;    
                        NetVal   WITH INVCHRG.nchrgamnt                                       ,;
                        Vat      WITH IIF(INVCHRG.ntaxrate>0                                  ,;
                                 (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)                ,;
                        Order	 WITH INVCHRG.Order											  ,;
                        Gross    WITH NetVal + Vat                                            ,; 
                        LPRINT   WITH 'F',;
                        CCONTREF  WITH ORDHDR.CCONTREF                        
*B611201,1 MAA 12/10/2016 Fix report to display Pick tiket, order, and customer PO numbers [T20161003.0016] [END]

              ELSE
                REPLACE NetVal   WITH NetVal + INVCHRG.nchrgamnt                              ,;
                        Vat      WITH Vat  + IIF(INVCHRG.ntaxrate>0                           ,;
                                 (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)                ,;
                        Gross    WITH NetVal + Vat                                            
              ENDIF
              lnCrgCnt = lnCrgCnt + 1
            ENDSCAN
          ENDIF          
        ENDIF  
        
        *- Add the detailed lines
        =SEEK(Consinvh.invoice+Consinvh.store+Consinvh.order,'CONSINVL')
        SELECT Consinvl
        SCAN REST WHILE invoice+store+order+style+STR(lineno,6) = ;
                  Consinvh.invoice+Consinvh.store+Consinvh.order FOR PIKTKT =Consinvh.PIKTKT 
          *XX        
          *IF  SEEK(PIKTKT.PIKTKT+PIKTKT.Order+STR(Consinvl.lineno,6),'PIKLINE')
          IF .T.
          *XX

            =SEEK(Consinvl.Style,'STYLE')
            
            *--Get the tax Rate for the current Style
            =gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
            *B609037,1 TMI 10/12/2009 02:01:32 PM [Start] base the lnVat on the tax rate of the invoice not of the style
            *lnVat = IIF(INVHDR.TAX_AMT<>0 AND lnTaxraT>0,(lnTaxraT * Consinvl.Price * Consinvl.TotQty)/100,0)            
            *X
            *XX
            IF lnTaxraT  = 0
              lnVat = 0
            Else
            *XX
            lnVat = IIF(INVHDR.TAX_AMT<>0 AND invhdr.tax_rate>0,(invhdr.tax_rate * Consinvl.Price * Consinvl.TotQty)/100,0)
            ENDIF            
            *lnVat = IIF(INVHDR.TAX_AMT<>0 ,((INVHDR.TAX_AMT*100/INVHDR.SHIPAMT) * Consinvl.Price * Consinvl.TotQty)/100,0)            
            *X
            *B609037,1 TMI 10/12/2009 02:02:29 PM [End  ] 

            SELECT (lcSumTemp)

            =SEEK('O'+CONSINVL.ORDER+CONSINVL.STORE+CONSINVL.STYLE+STR(CONSINVL.LINENO,6),'ORDLINE')

            IF SEEK(Consinvl.invoice+&lcKeyFld.+SUBSTR(Consinvl.Style,1,lnstylewid)+STR(Consinvl.Price,12,2)+Consinvl.Order,lcSumTemp)
              REPLACE Qty     WITH Qty + Consinvl.TotQty                       ;
                      NetVal  WITH NetVal + (Consinvl.Price * Consinvl.TotQty) ;
                      Vat     WITH Vat + lnVat        ;
                      Gross   WITH NetVal + Vat          
            ELSE

              =SEEK('O'+CONSINVL.ORDER,'ORDHDR')
    
              APPEND BLANK
              REPLACE Invoice   WITH Consinvh.Invoice                    ;
                      dShipDate WITH Pack_Hdr.dShipDate                  ;
                      PikTkt    WITH Consinvh.Piktkt                     ;
                      Store     WITH Consinvh.Store                      ;
                      Style     WITH SUBSTR(Consinvl.Style,1,lnstylewid) ;
                      Desc      WITH STYLE.Desc                          ;
                      Price     WITH Consinvl.Price                      ;
                      Qty       WITH Consinvl.TotQty                     ;
                      NetVal    WITH (Consinvl.Price * Consinvl.TotQty)  ;
                      Vat       WITH lnVat                               ;
                      Gross     WITH NetVal + Vat                        ;
                      ORDER     WITH ORDHDR.ORDER                        ;
                      CCONTREF  WITH ORDHDR.CCONTREF                     ;
                      EMPLOYEE  WITH ORDLINE.EMPLOYEE                    ;
                      LPRINT    WITH 'F'
            ENDIF
          ENDIF
        ENDSCAN
        SELECT (lcTmpCstPo)
        IF !(lcInvoice == INVHDR.Invoice)
          lcInvoice = INVHDR.Invoice
          lnCustPo  = 0
          APPEND BLANK
        ENDIF
        IF !(lcCustPo == Consinvh.CustPo)
          lcCustPo = Consinvh.CustPo
          lnCustPo = lnCustPo + 1   && to know if there are multiple CUSTPO's
          *--if there is only one CUSTPO applies to all CONSINVH records then print it
          IF lnCustPo = 1      
            REPLACE INVOICE WITH INVHDR.Invoice        ,;
                    Account WITH INVHDR.Account        ,;
                    CUSTPO  WITH Consinvh.CustPo       ,;
                    InvDate WITH INVHDR.InvDate        ,;
                    llPrint WITH .T.
          ELSE  && if there are multiple CUSTPO's then don't print them
            REPLACE llPrint WITH .F.
          ENDIF
        ENDIF
    ENDSCAN
    SELECT (lcTmpDate)
    APPEND BLANK
    REPLACE INVOICE WITH INVHDR.INVOICE ,;
            FDATE   WITH ldFromDate     ,;
            TODATE  WITH ldToDate
  ENDIF
ENDSCAN
*--End function lfColDataS
*B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[Start]
IF lcRpFormN = 'D'
  SELECT &lcInvCurs
  LOCATE
  SCAN
    IF SEEK(&lcInvCurs..INVOICE, 'INVCHRG')  
      SELECT INVCHRG
      SELECT PIKTKT,CSTORE,ORDER,SUM(INVCHRG.nchrgamnt) as TotalChr,SUM(ROUND(nTaxRate*INVCHRG.nchrgamnt/100,2)) as TotalVat FROM INVCHRG WHERE;
             INVOICE+ORDER+CSTORE+PIKTKT+CCHRGCODE = &lcInvCurs..INVOICE GROUP BY INVOICE,ORDER,CSTORE,PIKTKT INTO CURSOR 'SumChrg'
      IF _tally > 0         
        SELECT 'SumChrg'
        LOCATE 
        SCAN 
         =SEEK(&lcInvCurs..INVOICE+SumChrg.cSTORE+SumChrg.ORDER+SumChrg.PIKTKT,'CONSINVH','CONSINVH')
         =SEEK(Consinvh.order+Consinvh.Store+Consinvh.piktkt,'PACK_HDR')
         SELECT (lcSumTemp)
         APPEND BLANK
         REPLACE Invoice   WITH &lcInvCurs..INVOICE                 ;
                      dShipDate WITH Pack_Hdr.dShipDate                  ;
                      PikTkt    WITH SumChrg.Piktkt                     ;
                      Store     WITH SumChrg.CSTORE;
                      Style     WITH CHR(254)+"Carriage"                           ;
                      Desc      WITH "Carriage"                          ;
                      Price     WITH SumChrg.TotalChr ;
                      NetVal    WITH SumChrg.TotalChr ;
                      Vat       WITH SumChrg.TotalVat                           ;
                      Gross     WITH NetVal + Vat                        ;
                      ORDER     WITH SumChrg.ORDER                        ;
                      LPRINT    WITH 'F'
        ENDSCAN
      ENDIF 
    ENDIF
    SELECT &lcInvCurs 
  ENDSCAN    
ENDIF 
*B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[End]
*:**************************************************************************
*:* Name        : lfSetRel_1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2008
*:* Purpose     : Creat Relationship between files before collecting data.
*:***************************************************************************
FUNCTION lfSetRel_1
SELECT INVHDR
SET RELATION TO
SET RELATION TO INVOICE INTO CONSINVH ADDITIVE

SELECT CONSINVH
SET RELATION TO
SET RELATION TO Consinvh.invoice+Consinvh.store+Consinvh.order INTO Consinvl ADDITIVE
SET RELATION TO Consinvh.order+ Consinvh.piktkt INTO Piktkt ADDITIVE
SET RELATION TO 'S'+ Consinvh.account+ Consinvh.store INTO Customer ADDITIVE

SELECT CONSINVL
SET RELATION TO Consinvl.style INTO Style ADDITIVE
SET RELATION TO Consinvh.invoice INTO Invchrg ADDITIVE   

SELECT PIKTKT
SET RELATION TO
SET RELATION TO Piktkt.order INTO Pack_hdr ADDITIVE
SET RELATION TO PikTkt.piktkt INTO PikLine ADDITIVE
*--End function lfSetRel_1.

*!*************************************************************
*! Name        : lfSetRela
*! Developer   : Nader Nabil (NNA)
*! Date        : 02/12/2004
*! Purpose     : Function to Creat Relationship between files
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRela()
*!*************************************************************
FUNCTION lfSetRela

*!*	SELECT CONSINVH
*!*	SET RELATION TO
*!*	SELECT CONSINVL
*!*	SET RELATION TO
*!*	SELECT PIKTKT
*!*	SET RELATION TO

SELECT (lcSumTemp)
SET RELATION TO
SET RELATION TO INVOICE INTO (lcTmpCstPo) ADDITIVE

SELECT (lcTmpCstPo)
SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE

SELECT (lcSumTemp)
SET RELATION TO INVOICE INTO (lcTmpDate) ADDITIVE

SELECT (lcSumTemp)
LOCATE

*--End function lfSetRela.

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Tarek Mohammed Ibrahim 
*! Date      : 11/11/2008
*! Purpose   : Browse to select the needed invoices to report 
*!*************************************************************
*! Called from : OG
*!*************************************************************
FUNCTION lfvInvNo

PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  
  lcFile_Ttl = 'Receivable invoices' 
  
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE  
    &lcObjName = laOldVal
  ENDIF 
  
  SET RELATION OFF INTO CUSTOMER
ENDIF

IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN INVHDR
ELSE    
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF   

IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE    
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF   
*--End function lfvInvNo.

*!*************************************************************
*! Name      : lfSolToAdr
*! Developer : Nader Nabil (NNA)
*! Date      : 02/12/2004
*! Purpose   : Function to Get the Sold to Address 
*!*************************************************************
*! Called from : ARINVDIR.FRX
*!*************************************************************
*! Calls       : lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolToAdr

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec , lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
llEndGroup = .F.
SELECT SYCCOMP
SEEK oAriaapplication.activecompanyid
lcCompName = cCom_Name             && Variable to hold the Company Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompFax = cCom_Fax               && Variable to hold the Company Fax
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
lcCompPhon   = TRANSFORM(lcCompPhon , lcPhonPict)
lcCompFax    = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
=lfAdrShift('laCompAdd')

laSoldTo[1] = CUSTOMER.BTName 
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')
SELECT(lnOldAlias)
RETURN ''

*--End of function lfSolToAdr.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : TAREK MOHAMMED IBRAHEM
*! Date      : 11/11/2008
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARINVDIR.PRG , lfSolToAdr()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR 
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF  
ENDFOR   
*--End of function lfAdrShift.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Nader Nabil (NNA)
*! Date      : 02/12/2004
*! Purpose   : Function to Update the End of Group flag 
*!*************************************************************
*! Called from : ARINVDIR.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
FUNCTION lfEndGroup
llEndGroup = .T.
RETURN ''

*--End of function lfEndGroup.

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Tarek Mohammed Ibrahim 
*! Date      : 11/11/2008
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
FUNCTION lfInvSet
PARAMETERS lcParm

IF lcParm = 'S'
  lcInvHdTag = ORDER('INVHDR')
  lcCstmrTag = ORDER('CUSTOMER')
  SET ORDER TO TAG INVHDR IN INVHDR
  SET ORDER TO TAG CUSTOMER IN CUSTOMER
ELSE
  IF EMPTY(lcInvHdTag)
    SET ORDER TO 0 IN INVHDR
  ELSE    
    SET ORDER TO TAG (lcInvHdTag) IN INVHDR
  ENDIF    
  IF EMPTY(lcCstmrTag)
    SET ORDER TO 0 IN CUSTOMER
  ELSE    
    SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
  ENDIF   
  
ENDIF

*!*************************************************************
*! Name        : lfPrnHdr
*! Developer   : Tarek Mohammed  (TMI)
*! Date        : 02/12/2004
*! Purpose     : Replace lcSumTemp.Type with 'A' in the first
*!             : Record only to print invoice header in the 
*!             : first page only
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr
PRIVATE lcInvoice 
STORE '' TO lcInvoice

SELECT (lcSumTemp)
*!*	SET ORDER TO ('BY_'+lcRpFormN)

LOCATE
SCAN
  IF !(lcInvoice == EVAL(lcSumTemp+'.Invoice'))
    lcInvoice = EVAL(lcSumTemp+'.Invoice')
    REPLACE Type  WITH 'A'
  ELSE
    REPLACE Type  WITH 'B'
  ENDIF  
ENDSCAN

LOCATE

*--End FUNCTION of lfPrnHdr.

*:**************************************************************************
*:* Name        : lfvRpForm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2008
*:* Purpose     : Select the correct form to print
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvRpForm

DO CASE
CASE lcRpFormN='S'
  lcRpForm = 'ARINVDIS'
CASE lcRpFormN='D'
  lcRpForm = 'ARINVDIN'
CASE lcRpFormN='E'
  lcRpForm = 'ARINVDIE'
ENDCASE

= lfRepPltFr(lcRpForm)
CLEAR READ
*-- End Of Function lfvRpForm.

*:**************************************************************************
*:* Name        : lfRepSet
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2008
*:* Purpose     : Call the local fn lfRepSet to set my settings for the report
*:***************************************************************************
*:* Called from : arinvdis.frx, arinvdin.frx, arinvdie.frx ( the INIT event )
*:***************************************************************************
FUNCTION lfRepSet
SET CENTURY ON 
*-- end of lfRepSet.


*:**************************************************************************
*:* Name        : lfToExcel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/16/2008
*:* Purpose     : Export to Excel
*:***************************************************************************
FUNCTION lfToExcel

lcBy = ''
DO CASE
CASE lcRpFormN = 'S'
  lcBy = 'Site'
  lcKeySeek = 'STORE'
CASE lcRpFormN = 'D'
  lcBy = 'Delivery Note'
  lcKeySeek = 'PIKTKT'
CASE lcRpFormN = 'E'
  lcBy = 'Employee'
  lcKeySeek = 'EMPLOYEE'
ENDCASE

PRIVATE loExel
loExel = CREATEOBJECT('Excel.Application')

loExel.Workbooks.Add
*tmi 08/02/2009
TRY
*tmi 08/02/2009
  loExel.Sheets(3).Delete
  loExel.Sheets(2).Delete
  *tmi 08/02/2009
CATCH
ENDTRY
*tmi 08/02/2009
  
loExel.Sheets(1).name = "By " + lcBy 
loExel.ActiveSheet.PageSetup.Orientation = 2 

WITH loExel
.ActiveSheet.PageSetup.TopMargin = 0
.ActiveSheet.PageSetup.BottomMargin = 0
.ActiveSheet.PageSetup.HeaderMargin = 0
.ActiveSheet.PageSetup.FooterMargin = 0
ENDWITH

*- Format columns
=lfFormatColumns()

SELECT &lcSumTemp
LOCATE
lnRow = 0
lcKey = KEY()
lnPageCount = 0
lnInvPgCnt = 1
STORE 0 TO lnQtyCol,lnVatCol,lnNetValCol,lnGrossCol
STORE 0 TO lnInvQty,lnInvVat,lnInvNetVal,lnInvGross
lcInvoice = ''
DO WHILE !EOF(lcSumTemp)  
  *- If new invoice
  IF !(&lcSumTemp..INVOICE == lcInvoice)
    lnRow = lnRow  + 1      
    loExel.cells(lnRow,1).Activate
    IF lnRow > 1
      loexel.ActiveWindow.SelectedSheets.HPageBreaks.Add(loexel.ActiveCell)
    ENDIF      

    lcInvoice = &lcSumTemp..INVOICE 
  
    lnInvCell = 7
    WITH loExel
    .Cells(lnRow ,lnInvCell).Value = 'INVOICE'
    .Cells(lnRow ,lnInvCell).Font.name = 'Arial Black'
    .Cells(lnRow ,lnInvCell).Font.Size = 26    

    lnInvPgCnt = 1
    .Cells(lnRow ,12) = "Page 1"

    =lfSolToAdr() 
    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = lcCompName
    .Cells(lnRow,1).Font.FontStyle = "Bold"
    
    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = laCompAdd[1]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = laCompAdd[2]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = laCompAdd[3]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = laCompAdd[4]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = 'Tel   :'+lccompphone
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = 'Fax  :'+lcCompFax
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = 'VAT Registration Number   :'+lctaxrefds
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    
    lnRow = lnRow + 1
    .Cells(lnRow,1).Value = 'Sold to'
    .Cells(lnRow,1).Font.FontStyle = "Bold"
    
    .Cells(lnRow,8) = 'Invoice Number    :'+&lcSumTemp..INVOICE
    .Cells(lnRow,8).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value =     laSoldTo[1]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    .Cells(lnRow,8) = 'Invoice Date          :'+DTOC(&lcTmpCstPo..InvDate)
    .Cells(lnRow,8).Font.FontStyle = "Bold"
    

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value =     laSoldTo[2]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    .Cells(lnRow,8) = 'Account Number  :'+&lcTmpCstPo..ACCOUNT
    .Cells(lnRow,8).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value =     laSoldTo[3]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    .Cells(lnRow,8) = 'Customer Ref.       :'+IIF(EVAL(lcTmpCstPo+'.llPrint'),EVAL(lcTmpCstPo+'.CustPo'),'******************')
    .Cells(lnRow,8).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value =     laSoldTo[4]
    .Cells(lnRow,1).Font.FontStyle = "Bold"
    
    IF lcRpFormN <> 'D'
      .Cells(lnRow,8) = 'Invoice Number    :'+DTOC(&LCTMPDATE..FDATE) + ' - ' +DTOC(&LCTMPDATE..TODATE)
      .Cells(lnRow,8).Font.FontStyle = "Bold"
    ENDIF

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value =     laSoldTo[5]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    lnRow = lnRow + 1
    .Cells(lnRow,1).Value =     laSoldTo[6]
    .Cells(lnRow,1).Font.FontStyle = "Bold"

    ENDWITH
    
    *- Columns Headers
    =lfColumHeaders()
    
    STORE 0 TO lnInvQty,lnInvVat,lnInvNetVal,lnInvGross
  ENDIF 
  
  lcKeyVal = INVOICE+&lcKeySeek.
  STORE 0 TO lnQty,lnVat,lnNetVal,lnGross
  SCAN REST WHILE &lcKey = lcKeyVal

  *- increase line #
  =lfIncLineNo()

  WITH loExel

  lnCol = 0
  IF lcRpFormN = 'E'
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = &lcSumTemp..EMPLOYEE
  ENDIF

  IF lcRpFormN = 'D'
    lnCol = lnCol + 1
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[Start]
    *.Cells(lnRow,lnCol) = &lcSumTemp..PIKTKT
    .Cells(lnRow,lnCol) = IIF(&lcSumTemp..PIKTKT = CHR(255),'',&lcSumTemp..PIKTKT)
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[End]
    lnCol = lnCol + 1
    *! B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
    *.Cells(lnRow,lnCol) = &lcSumTemp..DSHIPDATE
    try
    .Cells(lnRow,lnCol) = IIF(&lcSumTemp..PIKTKT = CHR(255), '', &lcSumTemp..DSHIPDATE)
    Catch
    .Cells(lnRow,lnCol) = IIF(&lcSumTemp..PIKTKT = CHR(255), '',dtoc( &lcSumTemp..DSHIPDATE))
    endtry
    *! B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]    
  ENDIF
    
  IF lcRpFormN $ 'ES'
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = &lcSumTemp..Store
  ENDIF
    
  IF &lcSumTemp..Style<>'zzzzzzzzzzzz'
    
    WAIT WINDOW NOWAIT &lcSumTemp..invoice+'-'+&lcSumTemp..Style
    lnCol = lnCol + 1
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[Start]
    *.Cells(lnRow,lnCol) = &lcSumTemp..Style
    .Cells(lnRow,lnCol) = IIF(LEFT(&lcSumTemp..Style,1) = CHR(254),SUBSTR(&lcSumTemp..Style,2),&lcSumTemp..Style)
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[End]
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = &lcSumTemp..DESC
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = &lcSumTemp..Price
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = &lcSumTemp..Qty
    lnQtyCol = lnCol
  ELSE
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[Start]
    IF lcRpFormN = 'D'
      lnCol = lnCol + 1
    ELSE
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[End]
      lnCol = lnCol + 4
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[Start]
    ENDIF
    *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[End]
  ENDIF
  *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[Start]
  IF lcRpFormN = 'D' AND &lcSumTemp..Style='zzzzzzzzzzzz'
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = &lcSumTemp..DESC
    lnCol = lnCol + 2
  ENDIF
  *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[End]
  lnCol = lnCol + 1
  .Cells(lnRow,lnCol) = &lcSumTemp..NetVal
  lnCol = lnCol + 1
  .Cells(lnRow,lnCol) = &lcSumTemp..Vat
  lnCol = lnCol + 1
  .Cells(lnRow,lnCol) = &lcSumTemp..Gross
  lnCol = lnCol + 1
  .Cells(lnRow,lnCol) = &lcSumTemp..Order
  lnCol = lnCol + 1
  .Cells(lnRow,lnCol) = &lcSumTemp..CCONTREF
  
  lnQty = lnQty + &lcSumTemp..Qty
  lnVat = lnVat + &lcSumTemp..Vat
  lnNetVal = lnNetVal + &lcSumTemp..NetVal
  lnGross = lnGross + &lcSumTemp..Gross
  lnInvQty = lnInvQty + &lcSumTemp..Qty
  
  *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[Start]
  IF IIF(lcRpFormN = 'D',LEFT(EVAL(lcSumTemp+'.STYLE'),1)<> CHR(254),.T.)
  *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[End]
    lnInvVat = lnInvVat + &lcSumTemp..Vat
    lnInvNetVal = lnInvNetVal + &lcSumTemp..NetVal
    lnInvGross = lnInvGross + &lcSumTemp..Gross
  *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[Start]
  ENDIF
  *B610492,2 MMT 09/09/2013 Fix report to display the Charges code description in excel[END]
  ENDWITH  
  
  ENDSCAN

  WITH loExel
  =lfIncLineNo()
  .Cells(lnRow,lnQtyCol) = lnQty
  .Cells(lnRow,lnQtyCol).font.fontstyle = "Bold"
  .Cells(lnRow,lnVatCol) = lnVat
  .Cells(lnRow,lnVatCol).font.fontstyle = "Bold"  
  .Cells(lnRow,lnNetValCol) = lnNetVal
  .Cells(lnRow,lnNetValCol).font.fontstyle = "Bold"  
  .Cells(lnRow,lnGrossCol) = lnGross
  .Cells(lnRow,lnGrossCol).font.fontstyle = "Bold"  
  
  IF &lcSumTemp..INVOICE <> lcInvoice
    
    =lfIncLineNo()
    .Cells(lnRow,lnQtyCol) = lnInvQty
    .Cells(lnRow,lnQtyCol).font.fontstyle = "Bold"    
    .Cells(lnRow,lnVatCol) = lnInvVat
    .Cells(lnRow,lnVatCol).font.fontstyle = "Bold"    
    .Cells(lnRow,lnNetValCol) = lnInvNetVal
    .Cells(lnRow,lnNetValCol).font.fontstyle = "Bold"    
    .Cells(lnRow,lnGrossCol) = lnInvGross
    .Cells(lnRow,lnGrossCol).font.fontstyle = "Bold"    
    
  ENDIF
  ENDWITH
  
  
 
ENDDO

loExel.Visible = .T.

*-- end of lfToExcel.


*:**************************************************************************
*:* Name        : lfColumHeaders
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/16/2008
*:* Purpose     : put column headers
*:***************************************************************************
FUNCTION lfColumHeaders

    WITH loExel
    lnCol = 0
    IF lcRpFormN = 'E'
      lnCol = lnCol + 1
      .Cells(lnRow,lnCol) = 'Staff No'
      .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
      
    ENDIF

    IF lcRpFormN = 'D'
      lnCol = lnCol + 1
      .Cells(lnRow,lnCol) = 'Delivery' + CHR(10)+ CHR(13) + 'Note No.'
      .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
      
      lnCol = lnCol + 1
      .Cells(lnRow,lnCol) = 'Date'
      .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    ENDIF
    
    IF lcRpFormN $ 'ES'
      lnCol = lnCol + 1
      .Cells(lnRow,lnCol) = 'Store'
      .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    ENDIF
        
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Style'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Description'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Price'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Qty'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnQtyCol = lnCol
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Net  Value'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnNetValCol = lnCol
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Vat'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnVatCol = lnCol
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Gross'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnGrossCol = lnCol
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Order'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    lnCol = lnCol + 1
    .Cells(lnRow,lnCol) = 'Customer PO'
    .Cells(lnRow,lnCol).Font.FontStyle = "Bold"
    
    
    ENDWITH       
    
    lnPageCount = loExel.ActiveSheet.HPageBreaks.Count

*-- end of lfColumHeaders.


*:**************************************************************************
*:* Name        : lfIncLineNo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/16/2008
*:* Purpose     : increase line #
*:***************************************************************************
FUNCTION lfIncLineNo

    lnRow = lnRow + 1 
    loExel.Cells(lnRow,1) = ' '
    IF lnPageCount < loExel.ActiveSheet.HPageBreaks.Count
      lnRow = lnRow + 1 
      loExel.Cells(lnRow,1) = '    Invoice Number :'+&lcSumTemp..INVOICE
  
      lnInvPgCnt = lnInvPgCnt+1
      loExel.Cells(lnRow,10) = "Page "+ALLTRIM(STR(lnInvPgCnt))
  
      lnRow = lnRow + 1         
      =lfColumHeaders()
      
      lnRow = lnRow + 1
    ENDIF    

*-- end of lfIncLineNo.


*:**************************************************************************
*:* Name        : lfColumHeaders
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/16/2008
*:* Purpose     : put column headers
*:***************************************************************************
FUNCTION lfFormatColumns

    WITH loExel
    lnCol = 0
    IF lcRpFormN = 'E'
      lnCol = lnCol + 1  && Staff No
      .Columns(lnCol).NumberFormat = "@"
      .Columns(lnCol).HorizontalAlignment = -4131 && left aligned
    ENDIF

    IF lcRpFormN = 'D'
      lnCol = lnCol + 1
      *.Cells(lnRow,lnCol) = 'Delivery' + CHR(13) + 'Note No.'
      lnCol = lnCol + 1
      *.Cells(lnRow,lnCol) = 'Date'
    ENDIF
    
    IF lcRpFormN $ 'ES'
      lnCol = lnCol + 1  && Store
      .Columns(lnCol).NumberFormat = "@"
      .Columns(lnCol).HorizontalAlignment = -4131 && left aligned
    ENDIF
        
    * 'Style'
    lnCol = lnCol + 1
    .Columns(lnCol).NumberFormat = "@"
    .Columns(lnCol).HorizontalAlignment = -4131 && left aligned
    
    * 'Description'
    lnCol = lnCol + 1
    .Columns(lnCol).NumberFormat = "@"
    .Columns(lnCol).HorizontalAlignment = -4131 && left aligned
    
    *'Price'
    lnCol = lnCol + 1
    .Columns(lnCol).NumberFormat = "0.00"

    * 'Qty'
    lnCol = lnCol + 1
    .Columns(lnCol).NumberFormat = "0"
    
    * 'Net  Value'
    lnCol = lnCol + 1
    .Columns(lnCol).NumberFormat = "0.00"
    
    *'Vat'
    lnCol = lnCol + 1
    .Columns(lnCol).NumberFormat = "0.00"
        
    *'Gross'
    lnCol = lnCol + 1
    .Columns(lnCol).NumberFormat = "0.00"        
    
    ENDWITH       
    

