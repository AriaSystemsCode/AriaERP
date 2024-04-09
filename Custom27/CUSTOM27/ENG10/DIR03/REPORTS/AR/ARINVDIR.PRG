*:***************************************************************************
*: Program file  : ARPINVDIR.PRG
*: Program desc. : Customized Consolidated Invoice Report for [DIR03]
*: Date          : 02/12/2004
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Nader Nabil Abd Elmonam (NNA)
*: Tracking Job Number : C123950
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:**************************************************************************
*: Example : DO ARPINVDIR
*:**************************************************************************
*:Modification:
*:**************************************************************************
*:C127085,1 NNA 05/08/2005 Add atext Option to both Formats of the Report
*:C127805,1 MMR 06/15/2005 Adding order charges to the both forms.
PRIVATE lcCentury
*--Set Date's Year to (yyyy) to print a complete date on the report
STORE '' TO lcCentury
lcCentury = SET('CENTURY')
SET CENTURY ON
DECLARE laCompAdd[5,1] , laSoldTo[6,1] 
llPrntComp = .T.
llRpinvnot=.t.
laCompAdd  = ''        && Array to hold the Company address
laSoldTo   = ''        && Array to hold the Sold To address
llEndGroup = .F.       && Flag to know if we are at the end of the Group
lcTaxRefDs = ALLTRIM(gfGetMemVar('M_TAX_REFE'))
DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)

lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxraT'
*:C127805,1 MMR 06/15/2005 Open the invchrg table [Start]
 IF !USED('INVCHRG')
   =gfOpenFile(gcDataDir+'INVCHRG','INVCHRG','SH')
   SET ORDER TO Invchrg
 ENDIF
*:C127805,1 MMR [END]
DO CASE
  CASE lcRpFormN = 'S'      && Consolidate invoice type (Sort by Site)
    = lfSetRel_1()
    = lfCrtTmp()
    = lfColDataS()
    SELECT CONSINVH
    SET RELATION TO
    SELECT CONSINVL
    SET RELATION TO
    SELECT PIKTKT
    SET RELATION TO
    = lfSetRela(lcSumTemp)
    = lfPrntHdr(lcSumTemp)
    *C127085,1 NNA 05/08/2005 (Begin) Change report form with another way 
    *LCOGTMPFORM = ''
    *C127085,1 (End)
    SELECT (lcSumTemp)
    LOCATE
    *C127085,1 NNA 05/08/2005 (Begin) Change report form with another way 
    *DO gfDispRe WITH gcRepHome +gcAct_Appl+ '\ARINVDIS.FRX'
    DO gfDispRe WITH EVAL('lcRpForm')  
    *C127085,1 (End)
  CASE lcRpFormN = 'D'    && Consolidate invoice type (Sort by D/N)
    = lfSetRel_1()
    = lfCrtTmp()
    = lfColDataD()
    = lfSetRela(lcDetTemp)
    = lfPrntHdr(lcDetTemp)
    *C127085,1 NNA 05/08/2005 (Begin) Change report form with another way 
    *LCOGTMPFORM = ''
    *C127085,1 (End)
    SELECT (lcDetTemp)
    LOCATE
    *C127085,1 NNA 05/08/2005 (Begin) Change report form with another way 
    *DO gfDispRe WITH gcRepHome +gcAct_Appl+ '\ARINVDIN.FRX'
    DO gfDispRe WITH EVAL('lcRpForm')  
    *C127085,1 (End)
ENDCASE

SET CENTURY &lcCentury
*:C127805,1 MMR 06/15/2005 Close the invchrg table [Start]
SELECT INVCHRG
USE
*:C127805,1 MMR [End]
*!*************************************************************
*! Name        : lfCrtTmp
*! Developer   : Nader Nabil (NNA)
*! Date        : 02/12/2004
*! Purpose     : Function to Creat the temp. File for form ARPINVDC
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTmp()
*!*************************************************************
*!C123950,1
FUNCTION lfCrtTmp
*--Create lcTmpDate file only if Consolidate invoice type =Sort by site because
  *-Date Range found in this form only
*--Create lcTmpCstPo file to hold the Custpo in there is only one custpo applies
  *-to all Stores or Piktkt
IF lcRpFormN = 'S'
  *:C127805,1 MMR 06/15/2005 ADDING New Field To Print the Charges Per Store [Start]
  *CREATE TABLE (gcWorkDir + lcSumTemp)(Invoice C(6),STORE C(8) , Dpoint C(50) , ;
                *Style C(lnstylewid), Desc C(20) , Price N(12,2) , QTY N(7) , ;
                *NetVal N(10,2) , Vat N(12,5) , Gross N(12,5) , Type C(1))
  *INDEX ON (INVOICE+STORE+STYLE+ALLT(STR(PRICE))) TAG (lcSumTemp) OF (lcSumTemp)
  CREATE TABLE (gcWorkDir + lcSumTemp)(Invoice C(6),STORE C(8) , Dpoint C(50) , ;
                Style C(lnstylewid), Desc C(30) , Price N(12,2) , QTY N(7) , ;
                NetVal N(10,2) , Vat N(12,5) , Gross N(12,5) , Type C(1),lPrint C(1))
  INDEX ON (INVOICE+STORE+STYLE+ALLT(STR(PRICE))+lPrint) TAG (lcSumTemp) OF (lcSumTemp)
  *:C127805,1 MMR [End]
  CREATE TABLE (gcWorkDir + lcTmpDate)(Invoice C(6),FDate D(8) , ToDate D(8))
  INDEX ON (INVOICE) TAG (lcTmpDate) OF (lcTmpDate)
  *:C127805,1 MMR 06/15/2005 New Table including the charges per store [Start]
  CREATE TABLE (gcWorkDir + lcTmpChrgS)(Invoice C(6),STORE C(8) , Dpoint C(50) , ;
                Style C(lnstylewid), Desc C(30) , Price N(12,2)                , ;
                NetVal N(10,2) , Vat N(12,5) , Gross N(12,5) , lPrint C(1))
  INDEX ON (INVOICE+STORE) TAG (lcTmpChrgS) OF (lcTmpChrgS)
  *:C127805,1 MMR [End]
ELSE
  *:C127805,1 MMR 06/15/2005 ADDING new field to print the charges under the 1rst pik ticket only. [Start] 
  *CREATE TABLE (gcWorkDir + lcDetTemp)(Invoice C(6),CustRef C(15) , Piktkt C(6) ,dShipDate D(8),;
               *Style C(lnstylewid), Desc C(20) , Price N(12,2) , QTY N(7) , ;
               *NetVal N(10,2) , Vat N(12,5) , Gross N(12,5) , Type C(1))
  *INDEX ON (INVOICE+PIKTKT+STYLE+ALLT(STR(PRICE))) TAG (lcDetTemp) OF (lcDetTemp)
  CREATE TABLE (gcWorkDir + lcDetTemp)(Invoice C(6),CustRef C(15) , Piktkt C(6) ,dShipDate D(8),;
               Style C(lnstylewid), Desc C(30) , Price N(12,2) , QTY N(7) , ;
               NetVal N(10,2) , Vat N(12,5) , Gross N(12,5) , Type C(1),lPrint C(1))
  INDEX ON (INVOICE+PIKTKT+STYLE+ALLT(STR(PRICE))+lPrint) TAG (lcDetTemp) OF (lcDetTemp)
  *:C127805,1 MMR [End]
  *:C127805,1 MMR 06/15/2005 Create the temp file to hold charges for form by DN [Start]
  CREATE TABLE (gcWorkDir + lcTmpChrg)(Invoice C(6), Piktkt C(6),Desc C(30),Style C(lnstylewid), Price N(12,2),;
               NetVal N(10,2), Vat N(12,5), Gross N(12,5),lPrint C(1)) 
  INDEX ON (INVOICE+Piktkt) TAG (lcTmpChrg) OF (lcTmpChrg)
  *:C127805,1 MMR [End]
ENDIF
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
*!C123950,1
FUNCTION lfColDataS
*:C127805,1 MMR 06/15/2005 Adding new variable to hold the old alias[Start]
*PRIVATE lcStore , lcCustPo , lcInvoice , lnCustPo
PRIVATE lcStore , lcCustPo , lcInvoice , lnCustPo,lcAlias,lcAlias2
*:C127805,1 MMR [End]
STORE '' TO lcStore , lcCustPo , lcInvoice
STORE 0 TO lnCustPo
SELECT INVHDR
SCAN FOR &LCRPEXP 
  STORE {} TO ldFromDate , ldToDate
  SELECT Consinvh
  IF SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+Store+Order = INVHDR.INVOICE
      IF SEEK(Consinvh.order+Consinvh.piktkt,'PIKTKT') AND;
        SEEK(Consinvh.order+Consinvh.Store+Consinvh.piktkt,'PACK_HDR')
        SELECT PACK_HDR
        IF !(lcStore == Consinvh.Store)
          lcStore = Consinvh.Store
        ENDIF
        *--Get the Date range to show it on the report
        IF PACK_HDR.dShipDate<>{}
          IF ldFromDate={} AND ldToDate = {}
            ldFromDate = PACK_HDR.dShipDate
            ldToDate   = PACK_HDR.dShipDate
          ELSE
            IF PACK_HDR.dShipDate < ldFromDate
              ldFromDate = PACK_HDR.dShipDate
            ENDIF
            IF PACK_HDR.dShipDate > ldToDate
              ldToDate = PACK_HDR.dShipDate
            ENDIF                        
          ENDIF
        ENDIF
        SELECT Consinvl
        *:C127805,1 MMR 06/15/2005 Collecting Charges Data[Start]
        lcAlias = ALIAS()
        IF SEEK(Consinvh.INVOICE+Consinvh.store,'INVCHRG') AND !SEEK(Consinvh.INVOICE+Consinvh.store,lcTmpChrgS)  
          SELECT INVCHRG
          SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE+Consinvh.store 
            SELECT (lcTmpChrgS)
            lcChrDisc = gfCodDes(INVCHRG.cchrgcode , 'CORDCHG')
            APPEND BLANK 
             REPLACE Invoice  WITH INVCHRG.Invoice                                         ,;
                     Store    WITH Consinvh.Store                                          ,;
                     Dpoint   WITH ALLTRIM(Consinvh.Store) + ' '+IIF(!EMPTY(Consinvh.Store),;
                              ALLTRIM(CUSTOMER.STNAME),'')                                 ,;
                     STYLE    WITH 'zzzzzzz'                                               ,;
                     Desc     WITH lcChrDisc                                               ,;
                     Price    WITH 100000000.99                                            ,;            
                     NetVal   WITH INVCHRG.nchrgamnt                                       ,;
                     Vat      WITH IIF(INVCHRG.ntaxrate>0                                  ,;
                              (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)                ,;
                     Gross    WITH NetVal + Vat                                            ,; 
                     lPrint   WITH 'T'                                                     
          ENDSCAN
        ENDIF  
        SELECT (lcAlias)                              
        *:C127805,1 MMR [End]
        SCAN REST WHILE invoice+store+order+style+STR(lineno,6) = ;
                  Consinvh.invoice+Consinvh.store+Consinvh.order
          *NNA 07/02/2005 (Begin) Seek for the piktkt line to not duplicate the records
          IF SEEK(PIKTKT.PIKTKT+PIKTKT.Order+STR(Consinvl.lineno,6),'PIKLINE')
          *NNA (End)
    
            *--Get the tax Rate for the current Style
            =gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
            SELECT (lcSumTemp)
            IF SEEK(Consinvl.invoice+Consinvl.store+SUBSTR(Consinvl.Style,1,lnstylewid)+;
                   ALLTRIM(STR(Consinvl.Price)))
              REPLACE Price   WITH Consinvl.Price                                          ,;
                      Qty     WITH Qty + Consinvl.TotQty                                   ,;
                      NetVal  WITH NetVal + (Consinvl.Price * Consinvl.TotQty)             ,;
                      Vat     WITH Vat + IIF(INVHDR.TAX_AMT<>0 AND lnTaxraT>0              ,;
                               (lnTaxraT * Consinvl.Price * Consinvl.TotQty)/100,0)        ,;
                      Gross   WITH NetVal + Vat          
            ELSE
              APPEND BLANK
              REPLACE Invoice WITH Consinvh.Invoice                                        ,;
                      Store   WITH Consinvh.Store                                          ,;
                      Dpoint  WITH ALLTRIM(Consinvh.Store) + ' '+IIF(!EMPTY(Consinvh.Store),;
                                   ALLTRIM(CUSTOMER.STNAME),'')                            ,;
                      Style   WITH SUBSTR(Consinvl.Style,1,lnstylewid)                     ,;
                      Desc    WITH STYLE.Desc                                              ,;
                      Price   WITH Consinvl.Price                                          ,;
                      Qty     WITH Consinvl.TotQty                                         ,;
                      NetVal  WITH (Consinvl.Price * Consinvl.TotQty)                      ,;
                      Vat     WITH IIF(INVHDR.TAX_AMT<>0 AND lnTaxraT>0                    ,;
                               (lnTaxraT * Consinvl.Price * Consinvl.TotQty)/100,0)        ,;
                      Gross   WITH NetVal + Vat                                            
            ENDIF
          *NNA (Begin)
          ENDIF
          *NNA (End)
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
      ENDIF
    ENDSCAN
    SELECT (lcTmpDate)
    APPEND BLANK
    REPLACE INVOICE WITH INVHDR.INVOICE ,;
            FDATE   WITH ldFromDate     ,;
            TODATE  WITH ldToDate
  ENDIF
ENDSCAN
*:C127805,1 MMR 06/15/2005 Adding the new data for Charges to the orignal table[Start]
lcAlias2=ALIAS()
SELECT (lcSumTemp)
REPLACE ALL lPrint WITH 'F'
APPEND FROM (gcWorkDir + lcTmpChrgS)  
SELECT (lcAlias2)
*:C127805,1 MMR [End]
*--End function lfColDataS
*!*************************************************************
*! Name        : lfColDataD
*! Developer   : Nader Nabil (NNA)
*! Date        : 02/12/2004
*! Purpose     : Function to Collect data for form ARPINVDN
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColDataD()
*!*************************************************************
*!C123950,1
FUNCTION lfColDataD
*:C127805,1 MMR 06/15/2005 Adding new variable to hold the old alias[Start]
*PRIVATE lcPiktkt , lcCustPo , lcInvoice , lnCustPo
PRIVATE lcPiktkt , lcCustPo , lcInvoice , lnCustPo,lcAlias,lcAlias2
*:C127805,1 MMR [End]
STORE '' TO lcPiktkt , lcCustPo , lcInvoice
STORE 0 TO lnCustPo
SELECT INVHDR
SCAN FOR &LCRPEXP 
  SELECT Consinvh
  IF SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+Store+Order = INVHDR.INVOICE
      IF SEEK(Consinvh.order+Consinvh.piktkt,'PIKTKT') AND;
        SEEK(Consinvh.order+Consinvh.Store+Consinvh.piktkt,'PACK_HDR')
        SELECT PACK_HDR
        IF !(lcPiktkt == ConsInvh.Piktkt)
          lcPiktkt = ConsInvh.Piktkt
        ENDIF
        SELECT Consinvl
        *:C127805,1 MMR 06/15/2005 Collecting Charges Data[Start]
        lcAlias = ALIAS()
        IF SEEK(Consinvh.INVOICE,'INVCHRG') AND !SEEK(Consinvh.INVOICE,lcTmpChrg)  
          SELECT INVCHRG
          SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE 
            SELECT (lcTmpChrg)
            lcChrDisc = gfCodDes(INVCHRG.cchrgcode , 'CORDCHG')
            APPEND BLANK 
            REPLACE Invoice    WITH INVCHRG.Invoice                                    ,;
                    Piktkt     WITH Consinvh.Piktkt                                    ,;
                    STYLE      WITH 'zzzzzzz'                                          ,;
                    Desc       WITH lcChrDisc                                          ,;
                    Price      WITH 100000000.99                                       ,;            
                    NetVal     WITH INVCHRG.nchrgamnt                                  ,;
                    Vat        WITH IIF(INVCHRG.ntaxrate>0                             ,;
                               (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;
                    Gross      WITH NetVal + Vat                                       ,; 
                    lPrint     WITH 'T'                                                     
          ENDSCAN
        ENDIF  
        SELECT (lcAlias)                              
        *:C127805,1 MMR [End]
        SCAN REST WHILE invoice+store+order+style+STR(lineno,6) = ;
                  Consinvh.invoice+Consinvh.store+Consinvh.order
          *NNA 07/02/2005 (Begin) Seek for the piktkt line to not duplicate the records
          IF SEEK(PIKTKT.PIKTKT+PIKTKT.Order+STR(Consinvl.lineno,6),'PIKLINE')
          *NNA (End)
           *--Get the tax Rate for the current Style
            =gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
            SELECT (lcDetTemp)
            IF SEEK(Consinvl.invoice+Consinvl.store+SUBSTR(Consinvl.Style,1,lnstylewid)+;
                  ALLTRIM(STR(Consinvl.Price)))
              REPLACE Qty     WITH Qty + Consinvl.TotQty                                     ,;
                      NetVal  WITH NetVal + (Consinvl.Price * Consinvl.TotQty)               ,;
                      Vat     WITH Vat + IIF(INVHDR.TAX_AMT<>0 AND lnTaxraT>0                ,;
                              (lnTaxraT * Consinvl.Price * Consinvl.TotQty)/100,0)           ,;
                      Gross   WITH NetVal + Vat          
            ELSE
              APPEND BLANK
              REPLACE Invoice   WITH Consinvh.Invoice                                        ,;
                      CustRef   WITH Consinvh.CustPo                                         ,;
                      dShipDate WITH Pack_Hdr.dShipDate                                      ,;
                      PikTkt    WITH Consinvh.Piktkt                                         ,;
                      Style     WITH SUBSTR(Consinvl.Style,1,lnstylewid)                     ,;
                      Desc      WITH STYLE.Desc                                              ,;
                      Price     WITH Consinvl.Price                                          ,;
                      Qty       WITH Consinvl.TotQty                                         ,;
                      NetVal    WITH (Consinvl.Price * Consinvl.TotQty)                      ,;
                      Vat       WITH IIF(INVHDR.TAX_AMT<>0 AND lnTaxraT>0                    ,;
                                 (lnTaxraT * Consinvl.Price * Consinvl.TotQty)/100,0)        ,;
                      Gross     WITH NetVal + Vat                                            
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
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
*:C127805,1 MMR 06/15/2005 Adding the new data for Charges to the orignal table[Start]
lcAlias2=ALIAS()
SELECT (lcDetTemp)
REPLACE ALL lPrint WITH 'F'
APPEND FROM (gcWorkDir + lcTmpChrg)  
SELECT (lcAlias2)
*:C127805,1 MMR [End]
*--End function lfColDataD.

*!*************************************************************
*! Name        : lfSetRel_1
*! Developer   : Nader Nabil (NNA)
*! Date        : 02/12/2004
*! Purpose     : Function to Creat Relationship between files
*!             : before collecting data.
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRel_1()
*!*************************************************************
*!C123950,1
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
*:C127805,1 MMR 06/15/2005 Create relation to the invchrg[Start]
SET RELATION TO Consinvh.invoice INTO Invchrg ADDITIVE   
*:C127805,1 MMR [End]
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
*!C123950,1
FUNCTION lfSetRela
PARAMETER lcUsedFile
  SELECT (lcUsedFile)
  SET RELATION TO
  SET RELATION TO INVOICE INTO (lcTmpCstPo) ADDITIVE
  SELECT (lcTmpCstPo)
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
IF lcRpFormN = 'S'
  SELECT (lcUsedFile)
  SET RELATION TO INVOICE INTO (lcTmpDate) ADDITIVE
ENDIF
*--End function lfSetRela.
*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Nader Nabil (NNA)
*! Date      : 02/12/2004
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*C123950,1
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
  ELSE    && Else
    &lcObjName = laOldVal
  ENDIF    && End of IF
  
  SET RELATION OFF INTO CUSTOMER
ENDIF    && End of IF

IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN INVHDR
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF

IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*--End function lfvInvNo.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Nader Nabil (NNA)
*! Date      : 02/12/2004
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
*C123950,1
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*--End of function lfwOldVal.
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
SEEK gcAct_Comp
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
*! Developer : Nader Nabil (NNA)
*! Date      : 02/12/2004
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARINVDIR.PRG , lfSolToAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*C123950,1
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

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
*C123950,1
FUNCTION lfEndGroup
llEndGroup = .T.
RETURN ''

*--End of function lfEndGroup.

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Nader Nabil (NNA)
*! Date      : 02/12/2004
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*C123950
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
  ELSE    && Else
    SET ORDER TO TAG (lcInvHdTag) IN INVHDR
  ENDIF    && End of IF
  IF EMPTY(lcCstmrTag)
    SET ORDER TO 0 IN CUSTOMER
  ELSE    && Else
    SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
  ENDIF    && End of IF
  
ENDIF

*!*************************************************************
*! Name        : lfPrntHdr
*! Developer   : Nader Nabil (NNA)
*! Date        : 02/12/2004
*! Purpose     : Replace lcSumTemp.Type with 'A' in the first
*!             : Record only to print invoice header in the 
*!             : first page only
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrntHdr()
*!*************************************************************
*!C123950,1
FUNCTION lfPrntHdr
PARAMETER lcUsedFile
PRIVATE lcInvoice , llPrntHdr
STORE '' TO lcInvoice
STORE .F. TO llPrntHdr
SELECT (lcUsedFile)
LOCATE
SCAN
  IF !(lcInvoice == EVAL(lcUsedFile+'.Invoice'))
    lcInvoice = EVAL(lcUsedFile+'.Invoice')
    llPrntHdr = .T.
  ELSE
    llPrntHdr = .F.  
  ENDIF
  REPLACE Type  WITH IIF(llPrntHdr ,'A','B')
ENDSCAN
*--End FUNCTION of lfPrntHdr.
*!*************************************************************
*! Name        : lfvRpForm
*! Developer   : Nader Nabil (NNA)
*! Date        : 05/08/2005
*! Purpose     : Control Form name
*!*************************************************************
*! Calls       : Procedures : ....
*!               Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvRpForm()
*!*************************************************************
*!C127085,1
FUNCTION lfvRpForm
lcRpForm = IIF(lcRpFormN='S','ARINVDIS','ARINVDIN')
= lfRepPltFr(lcRpForm)
CLEAR READ
*-- End Of Function lfvRpForm.
