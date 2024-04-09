*:***************************************************************************
*: Program file  : SOALE30
*: Program desc. : SALES ORDER PURCHASE REQUIREMENTS FORM for ALENA
*: For Report    : SOPURREQ.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOPURREQ
*:***************************************************************************
*: This Program is due to C200242 ...
*:***************************************************************************
*:Modifications:...
*:******************
*:B605657,1 RAE 03/21/2002 Fix the problem of trim colors are not as the style
*:                         color on style cost sheet, only the first PO is printed.
*:C200309,1 TMI 04/03/2002 Get OrdLine.item_no instead of style.cvensty (see C200311)
*:B605749,1 TMI 04/08/2002 Solve unsolved rest problems of Alena.
*:***************************************************************************

STORE 0 TO lnRequed
STORE '' TO lcSizes, lnSize
lcCentury  = SET('CENTURY')
SET CENTURY ON
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 200

*-- if the user change filter criteria then we must collect the data again.

IF llOGFltCh
  *--Check for he records.
  SELECT ORDHDR
  LOCATE FOR &lcRpexp AND Status $"OH"
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF

  *--Open the custom fiel MAPOALO
  IF !USED('MAPOALO')
    =gfOpenFile(gcDataDir+'MAPOALO', gcDataDir + 'MAPOSO','SH')
    *B605749,1 TMI [START] Open 'MAPOSO' agian
    USE (gcDataDir+'MAPOALO') AGAIN IN 0 ORDER TAG MAPOALO ALIAS (lcMaPOAlo2)
    *B605749,1 TMI [END  ] Open 'MAPOSO' agian
  ENDIF

  *--collect Item_Types
  IF !USED(lcTrmTmp)
    =lfCrtTrm()
  ENDIF  
  *--Custom index
  =lfTmpIndex()
  *--create temp files  
  =lfCreatTmp()
  *--Function to make the relations between the files and function to collect the data.
  =lfRelation()
  =lfColctDat()

  =lfGetSoPo2()
  SELECT (lcHeader)
  GOTO BOTTOM
  IF RECCOUNT() = 0 .OR. EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  LOCATE

ENDIF
*-200242,3
IF USED(lcDetail)
  SELECT (lcDetail)
  SET RELATION TO cOrder INTO (lcHeader) ADDITIVE
  SET RELATION TO cOrder+TYP INTO (lcTmpSoPo2) ADDITIVE
  LOCATE
ELSE
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF  

DO gfDispRe WITH EVALUATE('lcRpForm')
IF USED('MAPOALO')
  SELECT MAPOALO
  SET RELATION TO
ENDIF

IF USED(lcDetail)
  SELECT (lcDetail)
  SET RELATION TO
ENDIF  
SET CENTURY &lcCentury
SET MEMOWIDTH TO lnOldMemW
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
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

*--Creat table for the trasactions.

CREATE CURSOR (lcHeader) (cComp_Name C(30) , cOrder C(6)    , cBtname C(30) ,;    
                          cStyle C(12)     , nOrdQty N(8)   , dCompDate D(8),;
                          dFactDate D(8)   , cVendor C(8)   , cDesc C(20)    ,;
                          cFabric C(20)    , cDescVen1 C(29), cDescVen2 C(29),;
                          cDescVen3 C(29)  , cDescVen4 C(29), cCustPoNo C(15))

ZAP

INDEX ON cOrder  TAG lcHeader OF (gcWorkDir+lcHeader)

*--temp file for details only
CREATE CURSOR (lcDetail) (cFabric C(7) , cFabDesc C(20)    , cFabVen C(8)  ,;    
                          cFabClr C(6) , cFabItem C(19)    , cFabPatt C(10),;
                          nRequired N(15,3) , nOrdered N(15,3) , nHousQty N(11,3),;
                          cPo C(6)     , dOrdDate D(8)     , dDueDate D(8) ,;
                          cOrder C(6)  , cStyle C(12)      ,nSequanc N(2)  ,;
                          cItem_Type C(6) , TYP C(1))
ZAP

INDEX ON cOrder + cFabric +cFabClr + cPo TAG lcDetail OF (gcWorkDir+lcDetail)
*--index on Sequance Number
*-200242,3
*INDEX ON cOrder + TYP +STR(nSequanc,2) + cItem_Type TAG SeqType OF (gcWorkDir+lcDetail)
*B605749,4 TMI [START] Add PO# to the index
*INDEX ON cOrder + TYP +STR(nSequanc,2) + cItem_Type + cFabric +cFabClr TAG SeqType OF (gcWorkDir+lcDetail)
INDEX ON cOrder + TYP +STR(nSequanc,2) + cItem_Type + cFabric +cFabClr + cPo TAG SeqType OF (gcWorkDir+lcDetail)
*B605749,4 TMI [END  ] Add PO# to the index
*-200242,3

*--temp file for Po Sales order
*B6057489,1 TMI [START] Index tmpSoPo file on Order
*CREATE CURSOR (lcTmpSoPo) (cOrders C(100) ,cOrder C(6), TYP C(1) , cPoNo C(6))
*ZAP
*INDEX ON cPoNo +TYP  TAG lcTmpSoPo OF (gcWorkDir+lcTmpSoPo)
CREATE CURSOR (lcTmpSoPo) (cOrder C(6), cPoNo C(6),cOrders C(6) , TYP C(1) ,;
                           CITMMASK C(19),ITEM C(19),COLOR C(6))
INDEX ON cOrder+cPoNo+cOrders+TYP  TAG lcTmpSoPo OF (gcWorkDir+lcTmpSoPo)
*B6057489,1 TMI [END  ] Index tmpSoPo file on Order

*--temp file for Po Sales order 2
CREATE CURSOR (lcTmpSoPo2) (cOrder C(6) , TYP C(1) ,Fabmemo M(200))
ZAP
INDEX ON cOrder + TYP TAG lcTmpSoPo2 OF (gcWorkDir+lcTmpSoPo2)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelation
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to create the relations of the program.
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

SELECT ORDHDR
SET RELATION TO "O" + ORDHDR.ORDER INTO ORDLINE ADDITIVE
SET RELATION TO IIF(ORDHDR.STORE = SPACE(8),'M'+ ORDHDR.ACCOUNT,;
                'S'+ ORDHDR.ACCOUNT+ORDHDR.STORE) INTO CUSTOMER ADDITIVE

SELECT ORDLINE 
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO "2" + ORDHDR.ORDER + LEFT(STYLE,12) INTO CUTPICK ADDITIVE

SELECT CUTPICK
SET RELATION TO "I"+ CUTPICK.ctktno INTO CTKTBOM ADDITIVE

SELECT STYLE
SET RELATION TO cStyMajor INTO BOM ADDITIVE
SET RELATION TO Vendor    INTO APVENDOR ADDITIVE

SELECT MAPOALO
IF !('"P"+MAPOALO.POMAT' $ SET('RELATION'))
  SET RELATION TO "P"+MAPOALO.POMAT INTO POFHDR ADDITIVE
  SET RELATION TO MAPOALO.FABRIC + MAPOALO.COLOR + "P" + MAPOALO.POMAT INTO POFLN ADDITIVE
ENDIF  

*--End of lfRelation.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to Collect the data.
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
PRIVATE lcAlias

lcAlias = ALIAS()
STORE SPACE(0) TO lcStyMajEn , lcOrderEn , lcStBomItm , lcStyNnMaj

SELECT (lcDetail)
SET ORDER TO lcDetail

SELECT ORDHDR
SCAN FOR &lcRpexp AND Status $"OH"
  m.cOrder     = Order
  m.cComp_Name = GcCom_Name
  m.cBtName    = Customer.BtName
  m.dCompDate  = OrdHdr.Complete
  m.dFactDate  = OrdHdr.Complete - 7
  m.nOrdQty    = OrdHdr.Open
  m.cCustPoNo  = OrdHdr.CustPo

  SELECT ORDLINE
  m.cStyle   = LEFT(ORDLINE.STYLE,12)
  m.cDesc    = Style.Desc
  m.cVendor  = ApVendor.cVenComp
  *200242,3 get fabric description
  *=SEEK(Style.Fabric+SUBSTR(ORDLINE.STYLE,14,6),'FABRIC')
  =SEEK(Style.Fabric,'FABRIC')
  m.cFabric  = Fabric.Desc
  *200242,3m

  INSERT INTO (lcHeader) FROM MEMVAR

  *200242,3m
  *--Get style color description
  *C200309,1 TMI [Start] Define variables to Get Item_no from ordline file.
  *=lfGetClr()
  STORE SPACE(0) TO lcDescVen1 , lcDescVen2 ,lcDescVen3 , lcDescVen4 
  STORE 0 TO lnCount
  *C200309,1 TMI [End  ] Define variables to Get Item_no from ordline file.
  *200242,3m

  SCAN REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6)= "O" + ORDHDR.ORDER
    
    WAIT WINDOW "Collecting data for order# : " + Order NOWAIT
    *--Get style color description
    *=lfGetClr()
    *C200309,1 TMI [Start] Get the 4 color + item_no descriptions .
    IF lnCount < 5
      lnCount = lnCount + 1
      lcCount = STR(lnCount,1)
      lcDescVen&lcCount = SUBSTR(gfCodDes(SUBSTR(ORDLINE.STYLE,14,6),'COLOR'),1,10)+' '+;
                          IIF(EMPTY(ORDLINE.ITEM_NO),STYLE.CVENSTY,ORDLINE.ITEM_NO)
    ENDIF    
    *C200309,1 TMI [End  ] Get the 4 color + item_no descriptions .    
    *--Function to collect the lines.
    =lfColLine()
    lcOrderEn  = ORDLINE.ORDER
  ENDSCAN
  *C200309,1 TMI [Start] Update the descriptions in the header file.  
  SELECT (lcHeader)
  REPLACE cDescVen1 WITH lcDescVen1 ,;
          cDescVen2 WITH lcDescVen2 ,;
          cDescVen3 WITH lcDescVen3 ,;
          cDescVen4 WITH lcDescVen4 
  *C200309,1 TMI [End  ] Update the descriptions in the header file.  
ENDSCAN

SELECT (lcDetail)
SET ORDER TO SeqType

SELECT(lcAlias)

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfColLine
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to Collect the lines.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfColLine

STORE SPACE(0) TO LcBomItem , lnStockEn
lcAlias = ALIAS()

SELECT BOM
IF SEEK(PADR(LEFT(Style.cStyMajor,12),19,' '))
  *-- Include the Trims. [Begin]
  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = PADR(LEFT(Style.cStyMajor,12),19,' ') FOR ccatgtyp $ 'FT'
    DO CASE
      CASE SUBSTR(citmmask,14,6) = "******"  AND Bom.IClr = "******" 
           **Same AS
           ** Using the color of Order line while searching the BOM file.
           m.IClr = SUBSTR(ORDLINE.STYLE,14,6) 
           
      CASE SUBSTR(citmmask,14,6) = "******"  AND Bom.IClr <> "******" 
           ** Constant
          * m.IClr = SUBSTR(ORDLINE.STYLE,14,6) 
          m.IClr = Bom.IClr
                  
      CASE SUBSTR(citmmask,14,6) <> "******"  AND Bom.IClr <> "******"       
           ** Different 
           ** Using the color of the BOM file "Bom.IClr" while collecting.
           IF SUBSTR(ORDLINE.STYLE,14,6)  <> SUBSTR(citmmask,14,6)
             LOOP
           ENDIF 
           m.IClr = Bom.IClr           
    ENDCASE
    
    LcBomItem = BOM.ITEM
    =SEEK(LEFT(Item,7)+ALLTRIM(m.IClr),'FABRIC')    
    *B605657,1 RAE [BEGIN]
    IF SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + m.IClr, 'MAPOALO')
      lcAlias = SELECT(0)
      SELECT MAPOALO
      SCAN REST WHILE order+fabric+color = ORDHDR.ORDER + ;
                      LEFT(LcBomItem,7) + m.IClr
        *B605657,1 RAE [END]
        *m.IClr = IIF(Bom.IClr = "******" , SUBSTR(ORDLINE.STYLE,14,6) , Bom.IClr)
        
        m.cPo  = lfPoNo()
        
        *B605657,1 RAE [BEGIN]
        *m.cFabric  = Item
        m.cFabric   = lcBomItem
        *B605657,1 RAE [END]
        
        m.cFabDesc  = FABRIC.desc
        m.cFabPatt  = FABRIC.PATTERN
        =SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT,'POFLN')    
        =SEEK("P" + MAPOALO.POMAT,'POFHDR')
        m.cFabVen   = POFLN.Vendor
        m.cFabClr   = m.IClr
        m.cFabItem  = BOM.Item
        m.TYP       = BOM.TYP
        *B605657,1 RAE [BEGIN]
        *SELECT (lcAlias)  && TMI No need to  this
        *B605657,1 RAE [END]
        m.nRequired = lfRequred()
        m.nOrdered  = lfOtoS()
        m.nHousQty  = MAPOALO.nhousqty
        m.dOrdDate  = POFHDR.ENTERED
        m.dDueDate  = POFHDR.COMPLETE
        m.cOrder    = ORDLINE.ORDER
        m.cStyle    = PADR(LEFT(Style.cStyMajor,12),19,' ')
        =SEEK(FABRIC.Item_Type,lcTrmTmp)
        IF BOM.TYP = '3'
          IF !EOF(lcTrmTmp)
            m.nSequanc  = &lcTrmTmp..nSequanc
            m.cItem_Type= &lcTrmTmp..cItem_Type
          ELSE
            LOOP
          ENDIF
        ELSE
          m.nSequanc  = 0
          m.cItem_Type= ''
        ENDIF
        *--If found any material then accumelate quantity

        IF SEEK(OrdLine.Order + LEFT(m.cFabric,7) +m.cFabClr+m.cPo,lcDetail)
          *--mhm2000
          *REPLACE &lcDetail..nRequired WITH &lcDetail..nRequired +m.nRequired ,;
          *       &lcDetail..nOrdered WITH &lcDetail..nOrdered +m.nOrdered
          REPLACE &lcDetail..nRequired WITH &lcDetail..nRequired + m.nRequired
        ELSE          
          INSERT INTO (lcDetail) FROM MEMVAR
        ENDIF
        *200242,3
        lcPoNoEn = m.cPo
        SELECT MAPOALO
        
        *B605657,1 RAE [Begin] ==> Commented by TMI <B605749,1>
        **IF SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6) )
        **IF SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + BOM.iclr )
        *IF .T.
        *  SCAN REST WHILE Order+Fabric+Color = Ordline.Order+LEFT(m.cFabric,7)+ALLTRIM(m.cFabClr) FOR  PoMat <> lcPoNoEn
        *    m.cPo  =PoMat
        *    =SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT,'POFLN')    
        *    =SEEK("P" + MAPOALO.POMAT,'POFHDR')
        *    m.cFabVen   = POFLN.Vendor
        *    m.cFabItem  = BOM.Item
        *    m.TYP       = BOM.TYP
        *    m.nRequired = lfRequred()
        *    m.nOrdered  = lfOtoS()
        *    m.nHousQty  = MAPOALO.nhousqty
        *    m.dOrdDate  = POFHDR.ENTERED
        *    m.dDueDate  = POFHDR.COMPLETE
        *    m.cOrder    = ORDLINE.ORDER
        *    m.cStyle    = PADR(LEFT(Style.cStyMajor,12),19,' ')
        *    IF SEEK(OrdLine.Order + LEFT(m.cFabric,7) +m.cFabClr+m.cPo,lcDetail)
        *      *--mhm2000
        *      *REPLACE &lcDetail..nRequired WITH &lcDetail..nRequired +m.nRequired ,;
        *      *      &lcDetail..nOrdered WITH &lcDetail..nOrdered +m.nOrdered
        *      REPLACE &lcDetail..nRequired WITH &lcDetail..nRequired +m.nRequired 
        *    ELSE
        *      INSERT INTO (lcDetail) FROM MEMVAR
        *    ENDIF  
        *    =lfGetSoPo(lcPoNoEn,bom.item)
        *  ENDSCAN
        *ENDIF
        *B605657,1 RAE [End]
      ENDSCAN
    ENDIF            
  ENDSCAN
ENDIF

SELECT (lcAlias)

*--End of lfColLine.
*!*************************************************************
*! Name      : lfRequred
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to collect the Required quantity.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRequred

STORE 0 TO lnReqedEn
lcAlias = ALIAS()

*B605749,1  TMI [START] Comment the following code 
* IF SUBSTR(BOM.cItmmask,14,6) = "******"
*   *--The condition of the CTKTBOM.issue_qty = 0 if no didn't get this line and if there is no cutpick record we get the lines.
*   IF (SEEK('I'+CUTPICK.ctktno+PADR(LEFT(BOM.ITEM,12),19,' ')+ SUBSTR(ORDLINE.STYLE,14,6) , 'CTKTBOM') AND CTKTBOM.issue_qty = 0 )  OR EOF('CUTPICK') OR !EOF('CUTPICK')
*     IF EOF('CUTPICK')
*     
*       *B605657,1 RAE [START]
*       *lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
*       SELECT BOM
*       SCATTER MEMV MEMO
*       IF !EMPTY(m.Msizes)
*         lcSizes = m.Msizes
*         lcSizes = SUBSTR(m.Msizes,5,LEN(m.Msizes))
*         FOR lnCount = 1 TO LEN(lcSizes)-1 STEP 2
*           lnSize = SUBSTR(lcSizes,lnCount,1)
*           SELECT ORDLINE
*           lnRequed = lnRequed + ORDLINE.QTY&lnSize
*         ENDFOR
*         lnReqedEn = lnRequed * BOM.NBOMTOTQTY
*         STORE 0 TO lnRequed
*       ELSE
*         lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
*       ENDIF
*       *B605657,1 RAE [END]
*       
*     ELSE
*       IF EOF('CTKTBOM')
*        lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
*       ELSE
*         lnReqedEn = CTKTBOM.REQ_QTY
*       ENDIF
*     ENDIF
*       
*   ENDIF
* ELSE
*   *--The condition of the CTKTBOM.issue_qty = 0 if no didn't get this line and if there is no cutpick record we get the lines.
*   IF (SEEK('I'+CUTPICK.ctktno+PADR(LEFT(BOM.ITEM,12),19,' ')+ SUBSTR(ORDLINE.STYLE,14,6) , 'CTKTBOM') AND CTKTBOM.issue_qty = 0 )  OR EOF('CUTPICK') OR !EOF('CUTPICK')
*     IF EOF('CUTPICK')
* 
*       *B605657,1 RAE [START]
*       *lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
*       SELECT BOM
*       SCATTER MEMV MEMO
*       IF !EMPTY(m.Msizes)
*         lcSizes = m.Msizes
*         lcSizes = SUBSTR(m.Msizes,5,LEN(m.Msizes))
*         FOR lnCount = 1 TO LEN(lcSizes)-1 STEP 2
*           lnSize = SUBSTR(lcSizes,lnCount,1)
*           SELECT ORDLINE
*           lnRequed = lnRequed + ORDLINE.QTY&lnSize
*         ENDFOR
*         lnReqedEn = lnRequed * BOM.NBOMTOTQTY
*         STORE 0 TO lnRequed
*         *B605657,1 RAE [END]
*       ELSE
*         lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
*       ENDIF
*     ELSE
*       IF EOF('CTKTBOM')
*        lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
*       ELSE
*         lnReqedEn = CTKTBOM.REQ_QTY
*       ENDIF
*     ENDIF
*   ENDIF
* ENDIF
PRIVATE lnCount,lcCnt
*--If there is a cutpick record for the sales order then print from the cutpick only where no issued
*  quantities, if there is no cutpick record for the sales order then calculate quanity required
*  from the style bom records relating to the sales order
IF !EOF('CUTPICK') AND SEEK('I'+CUTPICK.ctktno+BOM.ITEM+m.cFabClr,'CTKTBOM')
  IF CTKTBOM.issue_qty = 0
    *B605749,4 TMI [START] Make a sub-function to calculate required.
    *lnReqedEn = CTKTBOM.REQ_QTY
    lnReqedEn = lfSzTot(CTKTBOM.UNTQTY)
    *B605749,4 TMI [END  ] Make a sub-function to calculate required.
  ENDIF  
ELSE
  *B605749,4 TMI [START] Make a sub-function to calculate required.
  *IF BOM.LBASONSIZ 
  *  FOR lnCount = 1 TO 8
  *    lcCnt = STR(lnCount,1)
  *    lnReqedEn = lnReqedEn + IIF( lcCnt $ BOM.MSIZES , ORDLINE.QTY&lcCnt , 0 ) * BOM.NBOMTOTQTY
  *  ENDFOR
  *ELSE
  *  lnReqedEn = ORDLINE.TOTQTY*BOM.NBOMTOTQTY    
  *ENDIF
  lnReqedEn = lfSzTot(BOM.NBOMTOTQTY)
  *B605749,4 TMI [START] Make a sub-function to calculate required.  
ENDIF
*B605749,1 TMI [END  ]  

SELECT (lcAlias)
RETURN lnReqedEn

*--End of lfRequred.

*:**************************************************************************
*:* Name        : lfSzTot
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/16/2002
*:* Purpose     : Get requirement based on sizes
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSzTot()
*:* Refer To    : B605749,4
*:***************************************************************************
FUNCTION lfSzTot
PARAMETERS lnUntQty
PRIVATE lnReqedEn
lnReqedEn = 0
IF BOM.LBASONSIZ 
  FOR lnCount = 1 TO 8
    lcCnt = STR(lnCount,1)
    lnReqedEn = lnReqedEn + IIF( lcCnt $ BOM.MSIZES , ORDLINE.QTY&lcCnt , 0 ) * lnUntQty
  ENDFOR
ELSE
  lnReqedEn = ORDLINE.TOTQTY*BOM.NBOMTOTQTY    
ENDIF
RETURN lnReqedEn
*-- end of lfSzTot.

*!*************************************************************
*! Name      : lfPoNo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to get the PoNo from the custom file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPoNo

PRIVATE lcPoNoEn
STORE SPACE(0) TO lcPoNoEn
lcAlias = ALIAS()

SELECT MAPOALO
*set order to MAPOSO
*200242,4
*IF SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6) )
*B605657,1 RAE [START]
*IF SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7)+IIF(Bom.IClr = "******" , SUBSTR(ORDLINE.STYLE,14,6) , Bom.IClr))
*B605657,1 RAE [END]
*200242,4
  lcPoNoEn = MAPOALO.PoMat
  =lfGetSoPo(lcPoNoEn,bom.item)
*B605657,1 RAE [START]
*ENDIF
*B605657,1 RAE [END]

*200242,4
*=SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6) )
*--- SSH
*=SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7)+IIF(Bom.IClr = "******" OR EMPTY(Bom.IClr), SUBSTR(ORDLINE.STYLE,14,6) , Bom.IClr))
*200242,4

SELECT (lcAlias)
*set order to mapoalo
RETURN lcPoNoEn

*--End of lfPoNo.
*!*************************************************************
*! Name      : lfOtoS
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to get open to sell.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOtoS

lcAlias = ALIAS()
STORE 0 TO lnPoOtoS
SELECT POFLN
=SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT)
SCAN REST WHILE fabric+color+cmattype+pomat+trancd = MAPOALO.FABRIC + MAPOALO.COLOR + "P" + MAPOALO.POMAT
  IF Trancd = '1'
    lnPoOtoS = lnPoOtoS + POFLN.nfabtotqty*Fabric.Conv
*  ELSE
*   IF Trancd = '2'
*      lnPoOtoS = lnPoOtoS - POFLN.nfabtotqty*Fabric.Conv
*   ENDIF
  ENDIF
ENDSCAN

SELECT (lcAlias)
RETURN lnPoOtoS

*--End of lfOtoS.
*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : The program
*!**************************************************************************
*! Calls     : None()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
FUNCTION lfClearRep

IF USED('MAPOALO')
  USE IN MAPOALO
  *B605749,1 TMI [START] Close lcMaPOAlo2
  USE IN (lcMaPOAlo2)
  *B605749,1 TMI [END  ] Close lcMaPOAlo2  
ENDIF

IF USED(lcHeader)
  USE IN (lcHeader)
ENDIF
ERASE (gcWorkDir+lcHeader+".DBF")
ERASE (gcWorkDir+lcHeader+".CDX")

IF USED(lcDetail)
  USE IN (lcDetail)
ENDIF
ERASE (gcWorkDir+lcDetail+".DBF")
ERASE (gcWorkDir+lcDetail+".CDX")

IF USED(lcTmpSoPo)
  USE IN (lcTmpSoPo)
ENDIF
ERASE (gcWorkDir+lcTmpSoPo+".DBF")
ERASE (gcWorkDir+lcTmpSoPo+".CDX")

IF USED(lcTmpSoPo2)
  USE IN (lcTmpSoPo2)
ENDIF
ERASE (gcWorkDir+lcTmpSoPo2+".DBF")
ERASE (gcWorkDir+lcTmpSoPo2+".CDX")

IF FILE(gcWorkDir + lcCutPick + '.CDX')
  SELECT CUTPICK
  CLOSE INDEX
  ERASE (gcWorkDir + lcCutPick + '.CDX')
ENDIF
*--End of lfClearRep.

*!**************************************************************************
*! Name      : lfSROrder
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 11/15/2001
*! Purpose   : Go top in the ORDHDR IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSROrder()
*!**************************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    LOCATE
  CASE lcParm = 'R'
    SELECT ORDHDR
ENDCASE
*-- end of lfSROrder.
*!*************************************************************
*! Name      : lfCrtTrm
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to creat the temp. file hold Trims.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrtTrm

CREATE CURSOR (lcTrmTmp) (nSequanc N(2) , cItem_Type C(6) )
ZAP
INDEX ON cItem_Type + STR(nSequanc,2) TAG lcTrmTmp OF (gcWorkDir+lcTrmTmp)

M.nSequanc = 0
SELECT (lcTrmTmp)

*--ITEM_TYPE1
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000046"
GATHER MEMVAR MEMO

*--ITEM_TYPE2
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000065"
GATHER MEMVAR MEMO

*--ITEM_TYPE3
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000068"
GATHER MEMVAR MEMO

*--ITEM_TYPE4
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000042"
GATHER MEMVAR MEMO

*--ITEM_TYPE5
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000043"
GATHER MEMVAR MEMO

*--ITEM_TYPE6
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000044"
GATHER MEMVAR MEMO

*--ITEM_TYPE7
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000062"
GATHER MEMVAR MEMO

*--ITEM_TYPE8
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000053"
GATHER MEMVAR MEMO

*--ITEM_TYPE9
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000059"
GATHER MEMVAR MEMO

*--ITEM_TYPE10
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000056"
GATHER MEMVAR MEMO

*--ITEM_TYPE11
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000070"
GATHER MEMVAR MEMO

*--ITEM_TYPE12
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000048"
GATHER MEMVAR MEMO

*--ITEM_TYPE13
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000071"
GATHER MEMVAR MEMO

*--ITEM_TYPE14
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000052"
GATHER MEMVAR MEMO

*--ITEM_TYPE15
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000069"
GATHER MEMVAR MEMO

*--ITEM_TYPE16
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000049"
GATHER MEMVAR MEMO

*--ITEM_TYPE17
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000001"
GATHER MEMVAR MEMO

*--ITEM_TYPE18
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000045"
GATHER MEMVAR MEMO

*--ITEM_TYPE19
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000047"
GATHER MEMVAR MEMO

*--ITEM_TYPE20
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000060"
GATHER MEMVAR MEMO


*--ITEM_TYPE21
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000057"
GATHER MEMVAR MEMO

*--ITEM_TYPE22
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000063"
GATHER MEMVAR MEMO

*--ITEM_TYPE23
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000051"
GATHER MEMVAR MEMO

*--ITEM_TYPE24
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000064"
GATHER MEMVAR MEMO

*--ITEM_TYPE25
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000050"
GATHER MEMVAR MEMO

*--ITEM_TYPE26
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000066"
GATHER MEMVAR MEMO

*--ITEM_TYPE27
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000054"
GATHER MEMVAR MEMO

*--ITEM_TYPE28
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000067"
GATHER MEMVAR MEMO

*--ITEM_TYPE29
APPEND BLANK
M.nSequanc   = M.nSequanc +1 
M.cItem_Type = "000055"
GATHER MEMVAR MEMO

*!*************************************************************
*! Name      : lfGetClr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to get the 4 colors description.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetClr

STORE SPACE(0) TO lcDescVen1 , lcDescVen2 ,lcDescVen3 , lcDescVen4 
STORE SPACE(0) TO lcStyNnMaj , lcStyMajEn
STORE 0 TO lnCountr

lcAlias = ALIAS()

lcStyMajEn = LEFT(ORDLINE.STYLE,12)
SELECT STYLE
SCAN REST WHILE LEFT(STYLE.STYLE,12) = lcStyMajEn
  lnCountr = lnCountr +1
  lcCountr = STR(lnCountr,1)
  IF lnCountr < 5
    lcStyNnMaj = SUBSTR(STYLE.STYLE,14,6)
    lcDescVen&lcCountr=SUBSTR(gfCodDes(lcStyNnMaj , 'COLOR     '),1,10)+' '+STYLE.CVENSTY
  ELSE
    EXIT
  ENDIF    
  
ENDSCAN
REPLACE &lcHeader..cDescVen1 WITH lcDescVen1 ,;
        &lcHeader..cDescVen2 WITH lcDescVen2 ,;
        &lcHeader..cDescVen3 WITH lcDescVen3 ,;
        &lcHeader..cDescVen4 WITH lcDescVen4 
SELECT STYLE
=SEEK(ORDLINE.STYLE)        
SELECT (lcAlias)
*!*************************************************************
*! Name      : lfGetSoPo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to get sales order for selected Po.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetSoPo
PARA lcPoNo ,lcFabric
PRIVATE lcOldOrd

*B605749,1 TMI [START] Comment all following lines and put my Code
* SELECT MAPOALO
* *--- SSH
* *lnRecNo = RECNO()
* *lcOldOrd = SET('ORDER')
* *SET ORDER TO Mapoalo
* 
* *IF SEEK(lcPoNo+LEFT(lcFabric,7))
* *  SCAN REST WHILE PoMat+Fabric+Color+Order = lcPoNo
*     SELECT (lcTmpSoPo)
*     IF SEEK(lcPoNo + BOM.TYP,lcTmpSoPo)
*       lcOrdNo = MAPOALO.Order
*       lcOrdrsNo = &lcTmpSoPo..cOrders
*       *IF OCCURS(lcOrdNo,lcOrdrsNo)=0
*       IF OCCURS(lcOrdNo,lcOrdrsNo)=0 AND OrdLine.Order<> MAPOALO.Order
*         REPLACE &lcTmpSoPo..cOrders  WITH  ALLTRIM(lcOrdrsNo) + '/' + MAPOALO.Order
*       ENDIF  
*     ELSE
*       IF OrdLine.Order<> MAPOALO.Order
*         APPEND BLANK
*         REPLACE &lcTmpSoPo..cOrders WITH MAPOALO.Order  ,;
*                 &lcTmpSoPo..cPoNo   WITH lcPoNo,;
*                 &lcTmpSoPo..TYP     WITH BOM.TYP,;
*                 &lcTmpSoPo..cOrder   WITH OrdLine.Order
*       ENDIF
*     ENDIF  
* 
* *  ENDSCAN
* *ENDIF  
* *--Fabmemo
* *SELECT MAPOALO
* *SET ORDER TO &lcOldOrd
* *GOTO lnRecNo

PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcMaPOAlo2)
IF SEEK(lcPoNo+LEFT(lcFabric,7),(lcMaPOAlo2))
  SCAN REST WHILE PoMat+Fabric+Color+Order = lcPoNo+LEFT(lcFabric,7) ;
              FOR &lcMaPOAlo2..ORDER <> ORDHDR.ORDER
    IF !SEEK(ORDHDR.ORDER+lcPoNo+&lcMaPOAlo2..ORDER+BOM.TYP , lcTmpSoPo)  && KEY : cOrder+cPoNo+cOrders+TYP
      INSERT INTO (lcTmpSoPo) VALUES (ORDHDR.Order,lcPoNo,&lcMaPOAlo2..Order,BOM.TYP,;
                                      BOM.CITMMASK,&lcMaPOAlo2..FABRIC,&lcMaPOAlo2..COLOR)
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)
*B605749,1 TMI [END  ]

*!*************************************************************
*! Name      : lfGetSoPo2
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/18/2001
*! Purpose   : Function to get sales order for selected Po.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetSoPo2

*B605749,4 TMI [START] Create intermediate cursor order_po_typ
PRIVATE lcIntrmdt
lcIntrmdt = gfTempName()
CREATE CURSOR &lcIntrmdt (CORDER C(6),CPONO C(6),TYP C(1),CORDERS C(200))
INDEX ON CORDER+CPONO+TYP TAG &lcIntrmdt
*B605749,4 TMI [END  ] Create intermediate cursor order_po_typ

SELECT (lcTmpSoPo)
LOCATE
*B605749,1 TMI [START] Save Po#
PRIVATE lcPoNo 
lcPoNo = &lcTmpSoPo..cPoNo
*B605749,1 TMI [END  ] Save Po#

SCAN 
  *B605749,1 TMI [START] Collect So's for Po's
  *SELECT (lcTmpSoPo2)
  *IF SEEK(&lcTmpSoPo..cOrder + &lcTmpSoPo..TYP )
  *  REPLACE &lcTmpSoPo2..Fabmemo WITH &lcTmpSoPo..cPoNo+'                        '+&lcTmpSoPo..CORDERS +CHR(13) ADDITIVE
  *ELSE
  *  APPEND BLANK
  *  REPLACE &lcTmpSoPo2..Fabmemo WITH &lcTmpSoPo..cPoNo+'                        '+&lcTmpSoPo..CORDERS + +CHR(13),;
  *          &lcTmpSoPo2..cOrder  WITH &lcTmpSoPo..cOrder,;
  *          &lcTmpSoPo2..TYP     WITH &lcTmpSoPo..TYP
  *ENDIF    
  
  WAIT WINDOW NOWAIT "Collecting Other Hous No's included Purchase Orders"
  *-- If the fabric is not icluded in the bom of the style then do not include the SO 
  *-- of this style in the "Other House No's on the PO' in the report
  llItmFound = .F.
  =SEEK('O'+&lcTmpSoPo..CORDERS,'ORDLINE')
  SELECT ORDLINE
  SCAN REST WHILE CORDTYPE+ORDER = 'O'+&lcTmpSoPo..CORDERS
    SELECT BOM
    IF SEEK(PADR(LEFT(ORDLINE.STYLE,12),19),'BOM')
      SCAN REST WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = PADR(LEFT(ORDLINE.STYLE,12),19)
        DO CASE
          CASE SUBSTR(BOM.CITMMASK,14,6) = "******" AND BOM.ICLR = "******"  && Same as
            IF &lcTmpSoPo..ITEM = BOM.ITEM
              llItmFound = .T.
              EXIT
            ENDIF
          CASE BOM.ICLR # "******"  && Constant
            IF &lcTmpSoPo..ITEM = BOM.ITEM AND &lcTmpSoPo..COLOR = BOM.ICLR
              llItmFound = .T.
              EXIT
            ENDIF
        ENDCASE
      ENDSCAN
      IF llItmFound
        EXIT
      ENDIF
    ENDIF
  ENDSCAN
  
  IF !llItmFound
    LOOP
  ENDIF

  *B605749,4 TMI [START] Add lines to &lcIntrmdt cursor
  *IF !SEEK(&lcTmpSoPo..cOrder+&lcTmpSoPo..TYP , lcTmpSoPo2)
  * INSERT INTO (lcTmpSoPo2) VALUES (&lcTmpSoPo..cOrder,&lcTmpSoPo..TYP, ;
  *                                   &lcTmpSoPo..cPoNo+SPACE(23)+&lcTmpSoPo..CORDERS )
  *ELSE
  *  IF lcPoNo = &lcTmpSoPo..cPoNo
  *    REPLACE &lcTmpSoPo2..Fabmemo WITH '/'+&lcTmpSoPo..CORDERS ADDITIVE
  *  ELSE
  *    REPLACE &lcTmpSoPo2..Fabmemo WITH CHR(13)+&lcTmpSoPo..cPoNo+SPACE(23)+&lcTmpSoPo..CORDERS ADDITIVE
  *  ENDIF
  *ENDIF
  *lcPoNo = &lcTmpSoPo..cPoNo
  IF !SEEK(&lcTmpSoPo..cOrder+&lcTmpSoPo..CPONO+&lcTmpSoPo..TYP,lcIntrmdt)
    INSERT INTO &lcIntrmdt VALUES ;
                (&lcTmpSoPo..cOrder,&lcTmpSoPo..CPONO,&lcTmpSoPo..TYP,&lcTmpSoPo..CORDERS)
  ELSE
    IF !&lcTmpSoPo..CORDERS $ &lcIntrmdt..CORDERS
      REPLACE &lcIntrmdt..CORDERS WITH ALLTRIM(&lcIntrmdt..CORDERS)+'/'+&lcTmpSoPo..CORDERS
    ENDIF
  ENDIF
ENDSCAN  

SELECT &lcIntrmdt
LOCATE
SCAN
  IF !SEEK(&lcIntrmdt..cOrder+&lcIntrmdt..TYP , lcTmpSoPo2)
    INSERT INTO (lcTmpSoPo2) VALUES (&lcIntrmdt..cOrder,&lcIntrmdt..TYP, ;
                                     &lcIntrmdt..cPoNo+SPACE(23)+&lcIntrmdt..CORDERS )
  ELSE
    REPLACE &lcTmpSoPo2..Fabmemo WITH CHR(13)+&lcIntrmdt..cPoNo+SPACE(23)+&lcIntrmdt..CORDERS ADDITIVE
  ENDIF
*B605749,4 TMI [END  ] Add lines to &lcIntrmdt cursor  
*B605749,1 TMI [END  ] Collect So's for Po's  
ENDSCAN
*B605749,4 TMI [START] Close &lcIntrmdt cursor  
USE IN &lcIntrmdt
*B605749,4 TMI [END  ] Close &lcIntrmdt cursor  

*!*************************************************************
*! Name      : lfTmpIndex
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to creat temp. index for the cutpick file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfTmpIndex

SELECT CUTPICK
lcOrderPik = ORDER()
IF !FILE(gcWorkDir + lcCutPick + '.CDX')
  INDEX ON TRANCD+ORDER+STYLE TAG cStyle OF (gcWorkDir + lcCutPick + '.CDX')
ELSE
  SET ORDER TO TAG cStyle
ENDIF

*--End of lfTmpIndex.
