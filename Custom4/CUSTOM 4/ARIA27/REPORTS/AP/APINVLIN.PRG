*:************************************************************************************
*: Program file       : APINVLIN.PRG
*: Program desc.      : AP Detial Report,To show Transactions detail from AP invoice lines.
*: System             : Aria Advantage Series.
*: Module             : Accounts Payable (AP)
*: Developer          : RANIA ABDEL RAZIK (RAE)
*: Tracking Job Number: E500300
*:************************************************************************************
*: Calls :
*:    Functions : lfGetTyps() , lfvCrTemp() , lfCollData() , lfUpdFltVar()
*:                lfTmpFltr() , lfDocType() , lfPrntCTyp()     
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Notes   : ....
*:************************************************************************************
*: Example : DO APINVLIN.PRG
*:************************************************************************************
*: Modifications :
*:B606382,1 ALB 10/20/2002 Display shipment invoices
*:B606554,1 ALB 10/30/2002 Display contributed line only with contributed option YES
*:B606811,1 ALB 10/30/2002 Display contributed line in the same invoice
*:B607173,1 ALB 04/29/2003 Refresh the Docement# after change the Docement Type
*:B038431,1 NNA 09/04/2004 fix bug that when you Preview then return to Select an invoice Range
*:B038431,1 NNA            you will get an error massage
*:B124857,1 NNA 22/11/2004 Fix bug that when you print the report for Document Type(Cutting Ticket)
*:B124857,1 NNA            it Show the Cost type as (Misc.) Even these cutkts cost type is Mfg.Operation
**************************************************************************************

lcStTime = TIME()
*--- Start Valriable declaration.
STORE '' TO lcDocName , lcCstTypFlt , lcCostName , lcDocTyp
STORE SPACE(08) TO lcVendCode , lcInvAlias , lcOperList
STORE SPACE(03) TO lcCurrCode
llSelByInv = .F.

STORE {} TO ldStrDat , ldEndDat

=lfGetTyps()
=lfvCrTemp()
=lfUpdFltVar()
IF lfCollData() .OR. lfTmpFltr()
  *-- 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
ELSE
  DO gfDispRe WITH EVALUATE('lcRpForm')  
ENDIF

*!*************************************************************
*! Name      : lfGetTyps
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Get the Cost types codes.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfGetTyps()
*!*************************************************************

FUNCTION lfGetTyps

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*-- Get the Document Type.
DO CASE
  CASE lcRpDocTyp = 'I'
    lcDocTyp = 'I'
  CASE lcRpDocTyp = 'R'
    lcDocTyp = 'R'
  CASE lcRpDocTyp = 'S'
    lcDocTyp = 'S'
  CASE lcRpDocTyp = 'M'
    lcDocTyp = 'M'
  CASE lcRpDocTyp = 'L'
    lcDocTyp = 'L'
  CASE lcRpDocTyp = 'F'
    lcDocTyp = 'F'   
  CASE lcRpDocTyp = 'T'
    lcDocTyp = 'T'
  CASE lcRpDocTyp = 'G'
    lcDocTyp = 'G'
ENDCASE
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvCrTemp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Create Temp File.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvCrTemp()
*!*************************************************************

FUNCTION lfvCrTemp

*-- Create temporary file to collect the data.

CREATE TABLE (gcWorkDir+lcTempFile) (Vendor C(8)   , Ven_name C(24) , Currency C(3)  ,;
                                     Invoice C(12) , Inv_Date D(8)  , Doc_Type C(40) ,;
                                     DocNo C(6)    , Cost_Type C(1) , Operation C(30),;
                                     Style C(19)   , Size1 N(6)     , Size2 N(6)     ,;
                                     Size3 N(6)    , Size4 N(6)		, Size5 N(6)     ,;
                                     Size6 N(6)    , Size7 N(6)     , Size8 N(6)     ,;
                                     TotQty N(11,3), Price N(13,3)  , Amount N(13,3) ,;
                                     Session C(6)  , Codes C(6)     , Bom_Type C(1)  ,;
                                     LotNo C(2)    , VilNo C(4)     , CostName C(20) ,;
                                     DocTyp C(1)   , Color C(6))
INDEX ON Codes     TAG (lcTempF2)
INDEX ON Session   TAG (lcTempF3)
INDEX ON LotNo     TAG (lcTempF4)
INDEX ON Cost_Type TAG (lcTempF5)
INDEX ON Vendor + Currency + Invoice + Doc_Type TAG (lcTempFile)

*!*************************************************************
*! Name      : lfOldCollData
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Data Collection.
*!*************************************************************
*! Calls     : lfDocType() , lfPrntCTyp() , gfAmntDisp()
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfOldCollData()
*!*************************************************************
*
FUNCTION lfOldCollData

PRIVATE llCallGfAm
llCallGfam = EMPTY(laOGFxFlt[1,27]) .OR. (!EMPTY(laOGFxFlt[1,27]) .AND. lcRpCurr <> 'F')

SELECT APINVHDR
SET ORDER TO
SELECT APVINVDT
SET ORDER TO
SELECT APINVTKT

*-- If include contributed is Yes - APINVTKT - IS USED
IF lcRpIncCont ='Y'
  *-- If Invoice(s) is selected
  IF llSelByInv
    SELECT (lcInvAlias)
    SCAN
      m.Invoice = CINVNO
      SELECT APINVHDR
      SCAN FOR cinvno+cvendcode = m.Invoice .AND. ccurrcode+cvendcode+cinvno = IIF(!EMPTY(lcCurrCode),lcCurrCode,"") .AND.;
               cvendcode+cinvno = IIF(!EMPTY(lcVendCode),lcVendCode,"").AND.;
               BETWEEN(DTOS(apinvhdr.dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
        m.Vendor   = APINVHDR.CVENDCODE
        m.Ven_Name = APINVHDR.COUTCOMP
        m.Currency = APINVHDR.CCURRCODE
        m.Inv_Date = APINVHDR.DINVDATE
        WAIT WINDOW "Validating vendor "+m.Vendor+" Transactions"   NOWAIT
        SELECT APVINVDT

        *B606382,1 ALB Display shipment invoices [Begin]
        lcForCond = ''
        IF lcDocTyp = 'S'
          lcForCond = 'cimtyp+SHIPNO = lcDocTyp +lcRpDocNo'
        ELSE
          lcForCond = 'cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo'
        ENDIF
        *SCAN FOR cvendcode+capinvno+capvilno = m.Vendor + m.Invoice .AND.;
                 cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo
        SCAN FOR cvendcode+capinvno+capvilno = m.Vendor + m.Invoice .AND. &lcForCond
        *B606382,1 ALB Display shipment invoices [end]
        
          WAIT WINDOW "Validating vendor "+m.Vendor+" Transactions"   NOWAIT
          m.VilNo     = APVINVDT.CAPVILNO
          m.Cost_Type = APVINVDT.CCOSTTYPE
          m.Bom_Type  = APVINVDT.CBOMTYPE
          m.Operation = gfCodDes(APVINVDT.COPRCODE,PADR('MFGCODE',10))
          IF m.Operation = 'All'
            m.Operation = ''
          ENDIF
          m.Codes     = APVINVDT.COPRCODE
          m.LotNo     = APVINVDT.CLOTNO
          m.DocNo     = APVINVDT.CTKTNO
          SELECT APINVTKT
          *B606554,1 ALB Display contributed line only with contributed option YES [Begin]
          *SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor+m.Invoice+m.VilNo
          SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor+m.Invoice+m.VilNo AND !EMPTY(crsession)
          *B606554,1 ALB Display contributed line only with contributed option YES [end]
            WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT                            
            m.DocTyp    = APINVTKT.CIMTYP
            m.CostName  = lfPrntCTyp()
            m.Doc_Type  = lfDocType()              
            m.Style     = APINVTKT.ITEM
            m.Color     = APINVTKT.COLOR
            m.Session   = APINVTKT.CRSESSION
            m.Size1     = APINVTKT.NAPAPLQTY1
            m.Size2     = APINVTKT.NAPAPLQTY2
            m.Size3     = APINVTKT.NAPAPLQTY3
            m.Size4     = APINVTKT.NAPAPLQTY4
            m.Size5     = APINVTKT.NAPAPLQTY5
            m.Size6     = APINVTKT.NAPAPLQTY6
            m.Size7     = APINVTKT.NAPAPLQTY7
            m.Size8     = APINVTKT.NAPAPLQTY8
            m.TotQty    = APINVTKT.NAPTAPLQTY
            m.Price     = APINVTKT.NAPAPLPRIC            
            SELECT APINVHDR
            m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
            m.Price     = m.Amount / m.TotQty
            INSERT INTO (gcWorkDir+lcTempFile) FROM MEMVAR
          ENDSCAN
          *E500300,4
          lcCurrCode = ''
          *E500300,4
        ENDSCAN
      ENDSCAN
    ENDSCAN
  ELSE
    SELECT APINVHDR
    SCAN FOR cvendcode+cinvno = IIF(!EMPTY(lcVendCode),lcVendCode,"").AND. ccurrcode+cvendcode+cinvno = IIF(!EMPTY(lcCurrCode),lcCurrCode,"") .AND.;
             BETWEEN(DTOS(apinvhdr.dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
      m.Invoice   = APINVHDR.CINVNO
      m.Vendor    = APINVHDR.CVENDCODE
      m.Ven_Name  = APINVHDR.COUTCOMP
      m.Currency  = APINVHDR.CCURRCODE
      m.Inv_Date  = APINVHDR.DINVDATE
      WAIT WINDOW "Validating vendor "+m.Vendor+" Transactions"   NOWAIT
      SELECT APVINVDT
      *B606382,1 ALB Display shipment invoices [Begin]
      lcForCond = ''
      
      IF lcDocTyp = 'S'
        lcForCond = ' .AND. SHIPNO = lcRpDocNo'
      ENDIF
      *SCAN FOR cvendcode+capinvno+capvilno = m.Vendor+m.Invoice
      SCAN FOR cvendcode+capinvno+capvilno = m.Vendor+m.Invoice &lcForCond.
      *B606382,1 ALB Display shipment invoices [end]
        m.VilNo = APVINVDT.CAPVILNO
        m.Cost_Type = APVINVDT.CCOSTTYPE
        m.Bom_Type  = APVINVDT.CBOMTYPE
        m.Operation = gfCodDes(APVINVDT.COPRCODE,PADR('MFGCODE',10))
        IF m.Operation = 'All'
          m.Operation = ''
        ENDIF       
        m.Codes     = APVINVDT.COPRCODE
        m.LotNo     = APVINVDT.CLOTNO
        m.DocNo     = APVINVDT.CTKTNO
        
        SELECT APINVTKT
        *B606382,1 ALB Display shipment invoices [Begin]
        *SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor + m.Invoice + m.VilNo .AND.;
                 cimtyp+ctktno+STR(lineno,6)+crsession = lcDocTyp + lcRpDocNo
        SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor + m.Invoice + m.VilNo .AND.;
                 cimtyp+ctktno+STR(lineno,6)+crsession = lcDocTyp AND !EMPTY(crsession)

        *B606382,1 ALB Display shipment invoices [end]
          WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
          m.DocTyp    = APINVTKT.CIMTYP
          m.CostName  = lfPrntCTyp()          
          m.Doc_Type  = lfDocType()
          m.VilNo     = APINVTKT.CAPVILNO
          m.Style     = APINVTKT.ITEM
          m.Color     = APINVTKT.COLOR
          m.Session   = APINVTKT.CRSESSION
          m.Size1     = APINVTKT.NAPAPLQTY1
          m.Size2     = APINVTKT.NAPAPLQTY2
          m.Size3     = APINVTKT.NAPAPLQTY3
          m.Size4     = APINVTKT.NAPAPLQTY4
          m.Size5     = APINVTKT.NAPAPLQTY5
          m.Size6     = APINVTKT.NAPAPLQTY6
          m.Size7     = APINVTKT.NAPAPLQTY7
          m.Size8     = APINVTKT.NAPAPLQTY8
          m.TotQty    = APINVTKT.NAPTAPLQTY
          m.Price     = APINVTKT.NAPAPLPRIC
          SELECT APINVHDR
          m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
          m.Price     = m.Amount / m.TotQty
          INSERT INTO (gcWorkDir+lcTempFile) FROM MEMVAR
        ENDSCAN
        *E500300,4
        lcCurrCode = ''
        *E500300,4
      ENDSCAN
    ENDSCAN
  ENDIF
ELSE
  *-- If include contributed is No - APVINVDT - IS USED
  *-- If Invoice(s) is selected
  IF llSelByInv
    SELECT (lcInvAlias)
    SCAN
      m.Invoice = CINVNO
      WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
      SELECT APINVHDR
      SCAN FOR cinvno+cvendcode = m.Invoice .AND. BETWEEN(DTOS(apinvhdr.dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat))) .AND.;
               cvendcode+cinvno = IIF(!EMPTY(lcVendCode),lcVendCode,"") .AND. ccurrcode+cvendcode+cinvno = IIF(!EMPTY(lcCurrCode),lcCurrCode,"")
        m.Vendor    = APINVHDR.CVENDCODE
        m.Ven_Name  = APINVHDR.COUTCOMP
        m.Currency  = APINVHDR.CCURRCODE
        m.Inv_Date  = APINVHDR.DINVDATE
        WAIT WINDOW "Validating Vendor  : " + m.Vendor+" Transactions" NOWAIT
        SELECT APVINVDT
        *B606382,1 ALB Display shipment invoices [Begin]
        lcForCond = ''
        IF lcDocTyp = 'S'
          lcForCond = 'cimtyp+SHIPNO = lcDocTyp +lcRpDocNo'
        ELSE
          lcForCond = 'cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo'
        ENDIF
        *SCAN FOR cvendcode+capinvno+capvilno = m.Vendor + m.Invoice .AND.;
                 cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo
        SCAN FOR cvendcode+capinvno+capvilno = m.Vendor + m.Invoice .AND. &lcForCond
        *B606382,1 ALB Display shipment invoices [end]
          WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
          m.DocTyp    = APVINVDT.CIMTYP
          m.Doc_Type  = lfDocType()
          m.DocNo     = APVINVDT.CTKTNO
          m.Cost_Type = APVINVDT.CCOSTTYPE          
          m.Bom_Type  = APVINVDT.CBOMTYPE
          m.CostName  = lfPrntCTyp()
          m.Operation = gfCodDes(APVINVDT.COPRCODE,PADR('MFGCODE',10))
          IF m.Operation = 'All'
            m.Operation = ''
          ENDIF
          m.Codes     = APVINVDT.COPRCODE
          m.LotNo     = APVINVDT.CLOTNO
          m.Style     = APVINVDT.ITEM
          m.Color     = APVINVDT.COLOR
          m.Size1     = APVINVDT.NAPQTY1
          m.Size2     = APVINVDT.NAPQTY2
          m.Size3     = APVINVDT.NAPQTY3
          m.Size4     = APVINVDT.NAPQTY4
          m.Size5     = APVINVDT.NAPQTY5
          m.Size6     = APVINVDT.NAPQTY6
          m.Size7     = APVINVDT.NAPQTY7
          m.Size8     = APVINVDT.NAPQTY8
          m.TotQty    = APVINVDT.NAPTOTQTY
          m.Price     = APVINVDT.NAPPRICE
          SELECT APINVHDR
          m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
          m.Price     = m.Amount / m.TotQty
          INSERT INTO (gcWorkDir+lcTempFile) FROM MEMVAR
        ENDSCAN
        *E500300,4
        lcCurrCode = ''
        *E500300,4
      ENDSCAN
    ENDSCAN
  ELSE
    SELECT APINVHDR
    SCAN FOR cvendcode+cinvno = IIF(!EMPTY(lcVendCode),lcVendCode,"").AND. ;
             ccurrcode+cvendcode+cinvno = IIF(!EMPTY(lcCurrCode),lcCurrCode,"") .AND.;
             BETWEEN(DTOS(apinvhdr.dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
      m.Invoice   = APINVHDR.CINVNO
      m.Vendor    = APINVHDR.CVENDCODE
      m.Ven_Name  = APINVHDR.COUTCOMP
      m.Currency  = APINVHDR.CCURRCODE
      m.Inv_Date  = APINVHDR.DINVDATE
      WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
      *lcCurrCode = ''
      SELECT APVINVDT
      *B606382,1 ALB Display shipment invoices [Begin]
      lcForCond = ''
      IF lcDocTyp = 'S'
        lcForCond = 'cimtyp+SHIPNO = lcDocTyp +lcRpDocNo'
      ELSE
        lcForCond = 'cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo'
      ENDIF
      *SCAN FOR cvendcode+capinvno+capvilno = m.Vendor + m.Invoice .AND.;
               cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo
      SCAN FOR cvendcode+capinvno+capvilno = m.Vendor + m.Invoice .AND. &lcForCond
      *B606382,1 ALB Display shipment invoices [end]

        WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
        m.DocTyp    = APVINVDT.CIMTYP
        m.Doc_Type  = lfDocType()
        m.DocNo     = APVINVDT.CTKTNO
        m.Cost_Type = APVINVDT.CCOSTTYPE
        m.Bom_Type  = APVINVDT.CBOMTYPE
        m.CostName  = lfPrntCTyp()        
        m.Operation = gfCodDes(APVINVDT.COPRCODE,PADR('MFGCODE',10))
        IF m.Operation = 'All'
          m.Operation = ''
        ENDIF       
        m.Codes     = APVINVDT.COPRCODE
        m.LotNo     = APVINVDT.CLOTNO
        m.Style     = APVINVDT.ITEM
        m.Color     = APVINVDT.COLOR
        m.Size1     = APVINVDT.NAPQTY1
        m.Size2     = APVINVDT.NAPQTY2
        m.Size3     = APVINVDT.NAPQTY3
        m.Size4     = APVINVDT.NAPQTY4
        m.Size5     = APVINVDT.NAPQTY5
        m.Size6     = APVINVDT.NAPQTY6
        m.Size7     = APVINVDT.NAPQTY7
        m.Size8     = APVINVDT.NAPQTY8
        m.TotQty    = APVINVDT.NAPTOTQTY
        m.Price     = APVINVDT.NAPPRICE
        SELECT APINVHDR
        m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
        m.Price     = m.Amount / m.TotQty
        INSERT INTO (gcWorkDir+lcTempFile) FROM MEMVAR
      ENDSCAN
      *E500300,4
      lcCurrCode = ''
      *E500300,4
    ENDSCAN
  ENDIF
ENDIF
SELECT (lcTempFile)
GO TOP
RETURN(EOF())

*!*************************************************************
*! Name      : lfCollData
*! Developer : Albert Raif (ALB)
*! Date      : 12/31/2002
*! Purpose   : Data Collection.
*!*************************************************************
*! Calls     : lfDocType() , lfPrntCTyp() , gfAmntDisp()
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
*B606811,1ALB Display shipment invoices and control controbuted/non cont. invoice
FUNCTION lfCollData

*B038431,1 NNA 09/04/2004 (Begin) add New varaible to hold order of the APINVHDR file
*PRIVATE llCallGfAm
PRIVATE llCallGfAm , lnOldOrder
*B038431,1 NNA (End)

llCallGfam = EMPTY(laOGFxFlt[1,27]) .OR. (!EMPTY(laOGFxFlt[1,27]) .AND. lcRpCurr <> 'F')
SELECT APINVHDR

*B038431,1 NNA 09/04/2004 (Begin) hold order of the APINVHDR file
lnOldOrder = ORDER()
*B038431,1 NNA (End)

SET ORDER TO
SELECT APVINVDT
SET ORDER TO
SELECT APINVTKT
*-- If Invoice(s) is selected
IF llSelByInv
  SELECT (lcInvAlias)
  SCAN
    m.Invoice = CINVNO
    SELECT APINVHDR
    SCAN FOR cinvno+cvendcode = m.Invoice .AND. ccurrcode+cvendcode+cinvno = IIF(!EMPTY(lcCurrCode),lcCurrCode,"") .AND.;
             cvendcode+cinvno = IIF(!EMPTY(lcVendCode),lcVendCode,"").AND.;
             BETWEEN(DTOS(apinvhdr.dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
      m.Vendor   = APINVHDR.CVENDCODE
      m.Ven_Name = APINVHDR.COUTCOMP
      m.Currency = APINVHDR.CCURRCODE
      m.Inv_Date = APINVHDR.DINVDATE
      WAIT WINDOW "Validating vendor "+m.Vendor+" Transactions"   NOWAIT
      SELECT APVINVDT
      lcForCond = ''
      IF lcDocTyp = 'S'
        lcForCond = 'cimtyp+SHIPNO = lcDocTyp +lcRpDocNo'
      ELSE
        lcForCond = 'cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo'
      ENDIF
      SCAN FOR cvendcode+capinvno+capvilno = m.Vendor + m.Invoice .AND. &lcForCond
      
        WAIT WINDOW "Validating vendor "+m.Vendor+" Transactions"   NOWAIT
        m.VilNo     = APVINVDT.CAPVILNO
        m.Cost_Type = APVINVDT.CCOSTTYPE
        m.Bom_Type  = APVINVDT.CBOMTYPE
        m.Operation = gfCodDes(APVINVDT.COPRCODE,PADR('MFGCODE',10))
        IF m.Operation = 'All'
          m.Operation = ''
        ENDIF
        m.Codes     = APVINVDT.COPRCODE
        m.LotNo     = APVINVDT.CLOTNO
        m.DocNo     = IIF(lcDocTyp = 'S',APVINVDT.SHIPNO,APVINVDT.CTKTNO)
        SELECT APINVTKT
        *khalid
        *SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor+m.Invoice+m.VilNo;
                 AND IIF(lcRpIncCont ='Y',!EMPTY(crsession),EMPTY(crsession))
        SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor+m.Invoice+m.VilNo;
                 AND IIF(lcRpIncCont ='Y',.T.,EMPTY(crsession))
        *khalid
          WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT                            
          m.DocTyp    = APINVTKT.CIMTYP
          m.CostName  = lfPrntCTyp()
          m.Doc_Type  = lfDocType()              
          m.Style     = APINVTKT.ITEM
          m.Color     = APINVTKT.COLOR
          m.Session   = APINVTKT.CRSESSION
          m.Size1     = APINVTKT.NAPAPLQTY1
          m.Size2     = APINVTKT.NAPAPLQTY2
          m.Size3     = APINVTKT.NAPAPLQTY3
          m.Size4     = APINVTKT.NAPAPLQTY4
          m.Size5     = APINVTKT.NAPAPLQTY5
          m.Size6     = APINVTKT.NAPAPLQTY6
          m.Size7     = APINVTKT.NAPAPLQTY7
          m.Size8     = APINVTKT.NAPAPLQTY8
          m.TotQty    = APINVTKT.NAPTAPLQTY
          m.Price     = APINVTKT.NAPAPLPRIC            
          SELECT APINVHDR
          m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
          m.Price     = m.Amount / m.TotQty
          INSERT INTO (gcWorkDir+lcTempFile) FROM MEMVAR
        ENDSCAN
        lcCurrCode = ''
      ENDSCAN
    ENDSCAN
  ENDSCAN
ELSE
  SELECT APINVHDR
  SCAN FOR cvendcode+cinvno = IIF(!EMPTY(lcVendCode),lcVendCode,"").AND. ccurrcode+cvendcode+cinvno = IIF(!EMPTY(lcCurrCode),lcCurrCode,"") .AND.;
           BETWEEN(DTOS(apinvhdr.dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
    m.Invoice   = APINVHDR.CINVNO
    m.Vendor    = APINVHDR.CVENDCODE
    m.Ven_Name  = APINVHDR.COUTCOMP
    m.Currency  = APINVHDR.CCURRCODE
    m.Inv_Date  = APINVHDR.DINVDATE
    WAIT WINDOW "Validating vendor "+m.Vendor+" Transactions"   NOWAIT
    SELECT APVINVDT
    lcForCond = ''
      IF lcDocTyp = 'S'
        lcForCond = 'cimtyp+SHIPNO = lcDocTyp +lcRpDocNo'
      ELSE
        lcForCond = 'cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp+lcRpDocNo'
      ENDIF
    SCAN FOR cvendcode+capinvno+capvilno = m.Vendor+m.Invoice AND &lcForCond.
      m.VilNo = APVINVDT.CAPVILNO
      m.Cost_Type = APVINVDT.CCOSTTYPE
      m.Bom_Type  = APVINVDT.CBOMTYPE
      m.Operation = gfCodDes(APVINVDT.COPRCODE,PADR('MFGCODE',10))
      IF m.Operation = 'All'
        m.Operation = ''
      ENDIF       
      m.Codes     = APVINVDT.COPRCODE
      m.LotNo     = APVINVDT.CLOTNO
      m.DocNo     = IIF(lcDocTyp = 'S',APVINVDT.SHIPNO,APVINVDT.CTKTNO)
       
      SELECT APINVTKT
      *khalid
      *SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor + m.Invoice + m.VilNo .AND.;
               cimtyp+ctktno+STR(lineno,6)+crsession = lcDocTyp ;
               AND IIF(lcRpIncCont ='Y',!EMPTY(crsession),EMPTY(crsession))

      SCAN FOR cvendcode+capinvno+capvilno+crsession = m.Vendor + m.Invoice + m.VilNo .AND.;
               cimtyp+ctktno+STR(lineno,6)+crsession = lcDocTyp ;
               AND IIF(lcRpIncCont ='Y',.T.,EMPTY(crsession))
      *khalid
        WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
        m.DocTyp    = APINVTKT.CIMTYP
        m.CostName  = lfPrntCTyp()          
        m.Doc_Type  = lfDocType()
        m.VilNo     = APINVTKT.CAPVILNO
        m.Style     = APINVTKT.ITEM
        m.Color     = APINVTKT.COLOR
        m.Session   = APINVTKT.CRSESSION
        m.Size1     = APINVTKT.NAPAPLQTY1
        m.Size2     = APINVTKT.NAPAPLQTY2
        m.Size3     = APINVTKT.NAPAPLQTY3
        m.Size4     = APINVTKT.NAPAPLQTY4
        m.Size5     = APINVTKT.NAPAPLQTY5
        m.Size6     = APINVTKT.NAPAPLQTY6
        m.Size7     = APINVTKT.NAPAPLQTY7
        m.Size8     = APINVTKT.NAPAPLQTY8
        m.TotQty    = APINVTKT.NAPTAPLQTY
        m.Price     = APINVTKT.NAPAPLPRIC
        SELECT APINVHDR
        m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
        m.Price     = m.Amount / m.TotQty
        INSERT INTO (gcWorkDir+lcTempFile) FROM MEMVAR
      ENDSCAN
      lcCurrCode = ''
    ENDSCAN
  ENDSCAN
ENDIF

*B038431,1 NNA 09/04/2004 (Begin) Return to the previous order of the APINVHDR file
SELECT APINVHDR
SET ORDER TO TAG &lnOldOrder
*B038431,1 NNA 09/04/2004 (End)

SELECT (lcTempFile)
GO TOP
RETURN(EOF())

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Get the values of filter Exp. variables.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfUpdFltVar()
*!*************************************************************

FUNCTION lfUpdFltVar

FOR lnInd = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'APVENDOR.CVENDCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Vendor Code
      lcVendCode = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,8))

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'APINVHDR.CCURRCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Currency Code
      lcCurrCode = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,3))

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'APINVHDR.CINVNO' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Invoice Range
      lcInvAlias = laOgFxFlt[lnInd,6]
      llSelByInv = (USED(lcInvAlias) .AND. RECCOUNT(lcInvAlias) <> 0)

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'APINVHDR.DINVDATE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Invoice Dates Range
      ldStrDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldEndDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'APVINVDT.COPRCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Operation List
      lcOperList = laOgFxFlt[lnInd,6]
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfTmpFltr
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Filter the temp file.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfTmpFltr()
*!*************************************************************

FUNCTION lfTmpFltr

lcFinalFlt = ".T."
IF !EMPTY(lcOperList)
  lcFinalFlt = lcFinalFlt + ".AND. Codes $ lcOperList"
ENDIF
*E500300,4
IF lcRpIncCont ='Y'
*E500300,4
  IF !EMPTY(lcRpSess)
    lcRpSess = ALLTRIM(lcRpSess)
    lcFinalFlt = lcFinalFlt + " .AND. Session = lcRpSess"
  ENDIF
ENDIF
IF !EMPTY(lcRpLot)
  lcRpLot = ALLTRIM(lcRpLot)
  lcFinalFlt = lcFinalFlt + " .AND. LotNo = lcRpLot"
ENDIF
IF !EMPTY(laRpCTgt[1])
  FOR lnCout = 1 TO ALEN(laRpCTgt,1)
    lcCstTypFlt = lcCstTypFlt + SUBSTR(laRpCTgt[lnCout],1,1) + '|'
  ENDFOR
  lcFinalFlt = lcFinalFlt + " .AND. Cost_Type $ lcCstTypFlt"
ENDIF
SELECT (lcTempFile)
DELETE ALL FOR !(&lcFinalFlt)
PACK 
GO TOP
RETURN(EOF())

*!*************************************************************
*! Name      : lfDocType
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Get the name of Document Type to be printed.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : lcDocName
*!*************************************************************
*! Example            : =lfDocType()
*!*************************************************************

FUNCTION lfDocType

IF lcRpIncCont ='Y'
  DO CASE
    CASE APINVTKT.CIMTYP = 'I'
      lcDocName = 'Style Purchase Order'
    CASE APINVTKT.CIMTYP = 'R'
      lcDocName = 'Style Purchase Order Return'
    CASE APINVTKT.CIMTYP = 'S'
      lcDocName = 'Shipment'
    CASE APINVTKT.CIMTYP = 'M'
      lcDocName = 'Cutting Ticket'
    CASE APINVTKT.CIMTYP = 'L'
      lcDocName = 'Material PO'
    CASE APINVTKT.CIMTYP = 'F'
      lcDocName = 'Material PO Return'
    CASE APINVTKT.CIMTYP = 'T'
      lcDocName = 'Material MFG Order' 
  ENDCASE
ELSE
  DO CASE
    CASE APVINVDT.CIMTYP = 'I'
      lcDocName = 'Style Purchase Order'
    CASE APVINVDT.CIMTYP = 'R'
      lcDocName = 'Style Purchase Order Return'
    CASE APVINVDT.CIMTYP = 'S'
      lcDocName = 'Shipment'
    CASE APVINVDT.CIMTYP = 'M'
      lcDocName = 'Cutting Ticket'
    CASE APVINVDT.CIMTYP = 'L'
      lcDocName = 'Material PO'
    CASE APVINVDT.CIMTYP = 'F'
      lcDocName = 'Material PO Return'
    CASE APVINVDT.CIMTYP = 'T'
      lcDocName = 'Material MFG Order' 
    CASE APVINVDT.CIMTYP = 'G'
      lcDocName = 'General Expense' 
  ENDCASE
ENDIF
RETURN lcDocName

*!*************************************************************
*! Name      : lfPrntCTyp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Get the name of Cost Type to be printed.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : lcCostName
*!*************************************************************
*! Example            : =lfPrntCTyp()
*!*************************************************************

FUNCTION lfPrntCTyp

IF m.DocTyp $'IRS'
  STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,;
            M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5
  =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5',gcAct_Comp)

  FOR lnCout = 1 TO 5
    IF m.Cost_Type = EVAL('M_CITYPE'+STR(lnCout,1))
      lcCostName = EVAL('M_CISLBL'+STR(lnCout,1))
    ENDIF
  ENDFOR
ENDIF

IF m.DocTyp $'MT'
  STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,;
              M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5
  =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5',gcAct_Comp)         

  FOR lnCout = 1 TO 5
    IF m.Cost_Type = EVAL('M_CMTYPE'+STR(lnCout,1))

      *B124857,1 NNA 22/11/2004 (Begin) Check for APVINVDT.CBOMTYPE to get the cost Type Because
      *B124857,1 NNA            if it (4) then the Cost Name is 'MFG. Operation' and if it (5)
      *B124857,1 NNA            then the cost name is 'Misc.'
      *B124857,1 NNA            this bug Occurs because the two types save as 'M' in the APVINVDT.cCostType
      IF m.Bom_Type = STR(lnCout,1)
      *B124857,1 (End)

        lcCostName = EVAL('M_CMSLBL'+STR(lnCout,1))

      *B124857,1 (Begin)
      ENDIF
      *B124857,1 (End)
    ENDIF
  ENDFOR
ENDIF

IF m.DocTyp $'LF'
  DO CASE
    CASE m.Cost_Type = 'P' .AND. m.Bom_Type = '1'
      lcCostName = 'Purchase Price'
    CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '2'
      lcCostName = 'Freight'
    CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '3'
      lcCostName = 'Tax'
    CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '4'
      lcCostName = 'Quota'
  ENDCASE
ENDIF
RETURN lcCostName

*!*************************************************************
*! Name      : lfGetVend
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Valid Function of Vendor Code.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfGetVend()
*!*************************************************************

FUNCTION lfGetVend

lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)

IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor 

*!*************************************************************
*! Name      : lfvCostType
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Valid function of Cost Type.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfvCostType()
*!*************************************************************

FUNCTION lfvCostType

=gfMover(@laRpCSr,@laRpCTgt,'Cost Type',.T.,'')

*!*************************************************************
*! Name      : lfRepShow
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Fill the Cost types array.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfRepShow()
*!*************************************************************

FUNCTION lfRepShow

PRIVATE lnDataPos

*B607173,1 ALB Refresh the Docement# after change the Docement Type [Begin]
STORE '' TO lcRpDocNo
*B607173,1 ALB Refresh the Docement# after change the Docement Type [End]

lnDataPos = 0
lnDataPos = ASCAN(laOgFxFlt,'APINVHDR.CCURRCODE')
*E500300,4
*IF lnDataPos > 0
*  lnDataPos  = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
*  laOGFxFlt[lnDataPos,6]= gcBaseCurr
*ENDIF
*E500300,4
IF llCostTyp
  DIMENSION laRpCSr[1] , laRpCTgt[1] , laOldVal[1]
  =lfFillCstA()
  llCostTyp = .F.
ELSE
  RETURN  
ENDIF

*!*************************************************************
*! Name      : lfFillCstA
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Fill the Cost type array.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfFillCstA()
*!*************************************************************

FUNCTION lfFillCstA

  IF lcRpDocTyp $ 'IRS'
    STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,;
                M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5
    =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5',gcAct_Comp)
    DIMENSION laICstType[3,1]
    DIMENSION laRpCSr[3,1]
      
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    lnElements=0
    lnElements=0
    FOR lnCount = 1 TO 5
      IF INLIST(EVAL('M_CITYPE'+STR(lnCount,1)),'P','D','M')
        lnElements = lnElements+1
        DIMENSION laICstType[lnElements,1]
        laICstType[lnElements,1] = EVAL('M_CITYPE'+STR(lnCount,1))+' - '+EVAL('M_CISLBL'+STR(lnCount,1))
      ENDIF
    ENDFOR
    DIMENSION laRpCSr[lnElements,1]
    =ACOPY(laICstType,laRpCSr)
  ENDIF

  IF lcRpDocTyp $'MT'
    STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,;
                M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5
    =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5',gcAct_Comp)         

    DIMENSION laICstType[2,1]
    DIMENSION laRpCSr[2,1]  
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    lnElements=0
    FOR lnCount = 1 TO 5
      IF INLIST(EVAL('M_CMTYPE'+STR(lnCount,1)),'M')
        lnElements = lnElements+1
        DIMENSION laICstType[lnElements,1]
        laICstType[lnElements,1] = EVAL('M_CMTYPE'+STR(lnCount,1))+' - '+EVAL('M_CMSLBL'+STR(lnCount,1))
      ENDIF
    ENDFOR
    DIMENSION laRpCSr[lnElements,1]
    =ACOPY(laICstType,laRpCSr)
  ENDIF

  IF lcRpDocTyp $'LF'
    DIMENSION laICstType[4,1]
    DIMENSION laRpCSr[4,1]  
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    laICstType[1,1] = 'P - Purchase Price'
    laICstType[2,1] = 'D - Freight'
    laICstType[3,1] = 'D - Tax'
    laICstType[4,1] = 'D - Quota'  
    =ACOPY(laICstType,laRpCSr)
  ENDIF 

*!*************************************************************
*! Name      : lfclearrd
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Clear the Option Grid files.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfclearrd()
*!*************************************************************

FUNCTION lfclearrd

CLEAR READ
llCostTyp = .F.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Valid function to display the Currency.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfvCurDisp()
*!*************************************************************

FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*E500300,4
*-- If forgin currency you must be sure invoice currency has value.
*IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[1,27])
*  laOGFxFlt[1,27]= gcBaseCurr
*ENDIF
*SHOW GET laOGFxFlt[1,27]
*E500300,4

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/03/2002
*! Purpose   : Valid function of Currency Code.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfvCurCode()
*!*************************************************************

FUNCTION lfvCurCode

PRIVATE lcSavBrFld,lcSavTitle,lcBrFields,lcFile_Ttl,lnAlias
STORE ' ' TO lcSavBrFld,lcSavTitle,lcBrFields,lcFile_Ttl
STORE 0 TO lnAlias
lnAlias = SELECT(0)

IF EMPTY(laOGFxFlt[1,27]) .AND. lcRpCurr <> 'F'
  RETURN
ENDIF
IF !SEEK(laOGFxFlt[1,27],'SYCCURR') .OR. ATC("?",laOGFxFlt[1,27]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    laOGFxFlt[1,27] = lcOldCurr
  ELSE
    laOGFxFlt[1,27] = laTemp[1]
  ENDIF
ENDIF
SHOW GET laOGFxFlt[1,27]
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/22/2002
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwCurCode()
*!*************************************************************

FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,27]

*!*************************************************************
*! Name      : lfvDoc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Valid function of Document types popup. Change
*!             the cost type mover according to Document type.
*!*************************************************************
*! Called from : SYREPUVR
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvDoc()
*!*************************************************************

FUNCTION lfvDoc

CLEAR READ
IF lcRpDocTyp $ 'IRS'
  STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,;
              M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5

  =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5',gcAct_Comp)

  DIMENSION laICstType[3,1]
  DIMENSION laRpCSr[3,1]
  DECLARE laRpCTgt[1]
  laRpCTgt = ''
  lnElements=0
  FOR lnCount = 1 TO 5
    IF INLIST(EVAL('M_CITYPE'+STR(lnCount,1)),'P','D','M')
      lnElements = lnElements+1
      DIMENSION laICstType[lnElements,1]
      lCCount = STR(lnCount,1)
      laICstType[lnElements,1] = EVAL('M_CITYPE'+STR(lnCount,1))+' - '+EVAL('M_CISLBL'+STR(lnCount,1))
    ENDIF
  ENDFOR
  DIMENSION laRpCSr[lnElements,1]
  =ACOPY(laICstType,laRpCSr)
ENDIF

IF lcRpDocTyp $'MT'
  STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,;
              M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5
  =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5',gcAct_Comp)         

  DIMENSION laICstType[2,1]
  DIMENSION laRpCSr[2,1]  
  DECLARE laRpCTgt[1]
  laRpCTgt = ''
  lnElements=0
  FOR lnCount = 1 TO 5
    IF INLIST(EVAL('M_CMTYPE'+STR(lnCount,1)),'M')
      lnElements = lnElements+1
      DIMENSION laICstType[lnElements,1]
      laICstType[lnElements,1] = EVAL('M_CMTYPE'+STR(lnCount,1))+' - '+EVAL('M_CMSLBL'+STR(lnCount,1))
    ENDIF
  ENDFOR
  DIMENSION laRpCSr[lnElements,1]
  =ACOPY(laICstType,laRpCSr)
ENDIF

IF lcRpDocTyp $'LF'
  DIMENSION laICstType[4,1]
  DIMENSION laRpCSr[4,1]  
  DECLARE laRpCTgt[1]
  laRpCTgt = ''
  laICstType[1,1] = 'P - Purchase Price'
  laICstType[2,1] = 'D - Freight'
  laICstType[3,1] = 'D - Tax'
  laICstType[4,1] = 'D - Quota'  
  =ACOPY(laICstType,laRpCSr)
ENDIF

IF lcRpDocTyp $'G'
  CLEAR READ
ENDIF
  
*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Set function for the invoice number option in 
*!             case of In Range.
*!*************************************************************
*! Called from : SYREPUVR
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!*************************************************************
*! Return      : NONE
*!*************************************************************
*! Example     : =lfvInvNo()
*!*************************************************************

FUNCTION lfvInvNo

PARAMETERS lcParm
IF lcParm = 'S'
  SELECT APINVHDR
  GO TOP
ENDIF

*!*************************************************************
*! Name      : lfvTicket
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 04/05/2002
*! Purpose   : Validate Tickets
*!*************************************************************
*! Calls     : POSBrow()    , ARIABROW()    , CutBrow()
*!             POFBROW()    , lfChkCstCur()
*!*************************************************************
*! Passed Parameters  :  lcDocTyp
*!*************************************************************
*! Returns            :  NONE
*!*************************************************************
*! Example            :  =lfvTicket()
*!*************************************************************
FUNCTION lfvTicket

PARAMETERS lcDocTyp
PRIVATE lcBrFields,lnAlias

lnAlias = SELECT(0)
=lfGetTyps()

*-- Case of Style PO and Style Return PO
IF lcDocTyp = 'I' .OR. lcDocTyp = 'R'
  =gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
  IF !EMPTY(lcRpDocNo) .AND. !SEEK(IIF(lcDocTyp='I','P','R')+lcRpDocNo,'POSHDR')
    =POSBrow(@lcRpDocNo,'',IIF(lcDocTyp='I','P','R'))
  ENDIF
ENDIF

*-- Case of Shipment
IF lcDocTyp = 'S'
  =gfOpenFile(gcDataDir+'SHPMTHDR','SHPMTHDR','SH')
  IF !EMPTY(lcRpDocNo) .AND. !SEEK(lcRpDocNo,'SHPMTHDR')
    lcBrFields = "ShipNo    :R :H='Shipment #',"+;
                 "Status    :R :H='S',"+;
                 "Entered   :R :H='Entered',"+;
                 "Cartons   :R :H='Cartons',"+;
                 "AirWayB   :R :H='Air-Way Bill #',"+;
                 "ETA       :R :H='E.T.A.',"+;
    	         "TotQty    :R :H='In-Transit',"+;
    	         "Recv_Stk  :R :H='Received',"+;
        	     "Recv_Dam  :R :H='Damaged',"+;
        	     "Recv_Can  :R :H='Canceled',"+;
    	         "cVessel   :R :H='Airline/Vessel',"+;
       	         "Reference :R :H='Reference'"
    SELECT SHPMTHDR
    lcRpDocNo = IIF(ARIABROW('','Style Purchase Orders Shipments',gnBrHSRow1,;
                gnBrHSCol1,gnBrHSRow2,gnBrHSCol2,'','','ShipNo',;
                'laBrowArr'),ShipNo,SPACE(6))
  ENDIF
ENDIF  

*-- Case of Cutting Ticket
IF lcDocTyp = 'M'
  =gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
  IF !EMPTY(lcRpDocNo) .AND. !SEEK(lcRpDocNo,'CUTTKTH'))
    =CutBrow(@lcRpDocNo)
  ENDIF
ENDIF

*-- Case of Material PO and MAterial Return PO
IF lcDocTyp $'LF'
    =gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
    IF !EMPTY(lcRpDocNo) AND !SEEK(IIF(lcDocTyp='F','R','P')+lcRpDocNo,'POFHDR')
      =POFBROW(IIF(lcDocTyp='F','R','P'),'',@lcRpDocNo)
    ENDIF
ENDIF

*-- Case of MFG Order
IF lcDocTyp = 'T'
    =gfOpenFile(gcDataDir+'MMFGORDH','MMFGORDH','SH')
    IF !EMPTY(lcRpDocNo) .AND. !SEEK(lcRpDocNo,'MMFGORDH')
      =gfMFGOrdBr(@lcRpDocNo)
    ENDIF
ENDIF
*E500300,4
lcRpDocNo = IIF(EMPTY(lcRpDocNo),"",lcRpDocNo)
*E500300,4
SELECT (lnAlias)