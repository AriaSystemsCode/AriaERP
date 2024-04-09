*:****************************************************************************************
*: Program desc. : Main Program.
*:****************************************************************************************
*C200404,1 HBG 09/03/2002 - Update new cPckAcc field in ORDLINE with the
*C200404,1                  Pack account 'Generic or Account code'
*C200404,1                - If the qty sent is not evenly divisible by
*C200404,1                  the pack qty so reject the PO [Begin]
*C200410,1 HBG 09/23/2002 Update new nPackNo field in ORDLINE
*C200464,1 HBG 12/16/2002 Retrieve the name of the out bound file from ediNet.c856File
*C200464,1                in case of sending 856
*C200479,1 HBG 08/01/2003 USE fields lRange & lPckPrPiec from ORDLINE & INVLINE
*B606785,1 HBG 12/17/2002 Reject the PO if the Price of the pack <> the send Price ,not only give an error
*B606892,1 HBG 19/01/2003 Fix bug of always reject for “Prices sent doesn’t match…”
*B606892,1                even if the prices match
*B606913,1 HBG 27/01/2003 Use the Full Pack ID when updating EDIORDLN
*B607024,1 HBG 10/03/2003 Fix bug of wrong Qty in conflict Screen
*C200510,1 WLD 03/12/2003 Add SKUs# in Order Status Report (Report)
*C200526,1 WLD 03/26/2003 Use Pack-Color-Size in class send832 (EdiSc.Do)
*C200530,1 WLD 04/07/2003  Dept error (EdiPo)
*C200530,1 WLD 04/07/2003 If Dept not sent display Err in "Check Errors" and Prevent updating
*B607260,1 SSE 05/14/2003 Fix bug of not generating SO# when order is updated from Temp to Real.
*B607378,1 HBG 24/06/2003 Fix bug of not updating 860 correctly because it not using the full Pack Key
*C200566,1 SSE 06/02/2003 Custom add triggers for GMA to enhance consolidated invoice.
*C200579,1 HBG 06/29/2003 Calculate the check digit added to UCB field in sending invoice and ASN
*B607460,1 HBG 07/09/2003 Fix bug of when updating 860 the SuRetPri not stored
*B607478,1 TMI 08/07/2003 The 870 is building with 0 quantities in the PO1
*B607479,1 HBG 08/10/2003 Wrong update for pack version
*C200592,1 HBG 17/08/2003 Modify 810 & 856 Mapping for Kohl's to Send using Assortment Pack
*B120476,1 TMI 11/05/2003 Add quotations to seeked file name In funtion lfgmainv
*I119999,1 TMI 02/05/2004 Print the stub label "XXK" for GMA.
*B120999,1 HBG 02/17/2004 Fix bug of sending UOM as AS even if there is no pack detai
*B121104,1 HBG 15/02/2004 Fix bug of sending colored packs in 832 as Assort
*B120352,1 HBG 03/03/2004 Fix bug of rejecting the PO in Case 'CA' because the price mismatch
*B037982,1 HBG 4/18/2004 Get the Carton # form the new file EDICRTSQ instead of calculating it
*B122766,1 SSE 05/25/2004 Add the fix for B607282,1 from 810 class.
*C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty while sending ASN
*B038247,1 SSE 06/29/2004 Fix bug of adding cSizes field to the lcTmpLines file.
*B122272,1 WLD 10/05/2004 Incorrect price at SLN segment.
*E124848,1 WAM 11/07/2004 Update release order with bulk order configuration
*E124849,1 WAM 11/07/2004 consolidate same style with different configurations
*E124648,1 WLD 02/06/2005 Get the UPC of PO1 for the partner WLM 4030
*:****************************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*:****************************************************************************************
*! Name      : lfPRPack
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : Process Range Pack For GMA
*:****************************************************************************************
*! Called from : Class "ProcessPo"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfPRPack
  *!* lRange_Process from Spck_HDR.lRange from lfChkRng fn.
  *!* cBlkOrd from ConflctL.cBlkOrd from lfChkRng fn.
  *!* As per A27 Range pack means pack with QTY 1 for each componant.
  IF lRange_Process AND !EMPTY(cBlkOrd)
    *&& Seek For Bulk Order.
    SELECT ORDHDR
    lnStQty_Hld = lnQty
    IF SEEK(PADR(ALLTRIM(MACCOUNT),5)+'O'+cBlkOrd,'ORDHDR','ORDACCT')
      lminFlag = .F.
      *  SCAN REST WHILE Account+cOrdType+ORDER = PADR(ALLTRIM(MACCOUNT),5)+'O'+cBlkOrd
      *&& Scan For Bulk Order Lines.
      SELECT OrdLine
      = SEEK('O'+cBlkOrd,'OrdLine','OrdLine')
      SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6) = "O"+cBlkOrd
        *&& If This Line Still Have Qty Open, Get This Qty To Use In Coming EDI Order.
        *IF TotBook>0
        IF TotBook>0  AND SEEK(MTYPE+MKEY+OrdLine.STYLE,'Spck_Lin','Spck_LinVr')
          *&&H Use OrdLine Again Alias "BlkOrdLn"
          *&& Sum The Temparary Booked Qty From This Bulk Order Line.
          STORE 0 TO nTotBook_Sum,Book1_Sum,Book2_Sum,Book3_Sum,Book4_Sum,Book5_Sum,Book6_Sum,Book7_Sum,Book8_Sum
          SELECT BlkOrdLn
          = SEEK('T'+cBlkOrd+STR(OrdLine.LINENO,6),'BlkOrdLn','OrdBulk')
          SUM REST  TotBook    ,Book1      ,Book2      ,Book3      ,Book4      ,Book5      ,Book6      ,Book7      ,Book8    ;
            TO     nTotBook_Sum,Book1_Sum,Book2_Sum,Book3_Sum,Book4_Sum,Book5_Sum,Book6_Sum,Book7_Sum,Book8_Sum;
            WHILE cOrdType+ORDER+STR(LINENO,6) = 'T'+cBlkOrd+STR(OrdLine.LINENO,6)
          SELECT EDIORDLN
          lnTotUsdQty = 0
          STORE 0 TO lnUsedQty1,lnUsedQty2,lnUsedQty3,lnUsedQty4,lnUsedQty5,lnUsedQty6,lnUsedQty7,lnUsedQty8

          SUM  Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQTY  TO lnUsedQty1,lnUsedQty2,lnUsedQty3,lnUsedQty4,;
            lnUsedQty5,lnUsedQty6,lnUsedQty7,lnUsedQty8,lnTotUsdQty ;
            FOR EDIORDLN.STYLE +EDIORDLN.Pack_id = OrdLine.STYLE+SPCK_LIN.Pack_id
          Book1_Sum = Book1_Sum + lnUsedQty1
          Book2_Sum = Book2_Sum + lnUsedQty2
          Book3_Sum = Book3_Sum + lnUsedQty3
          Book4_Sum = Book4_Sum + lnUsedQty4
          Book5_Sum = Book5_Sum + lnUsedQty5
          Book6_Sum = Book6_Sum + lnUsedQty6
          Book7_Sum = Book7_Sum + lnUsedQty7
          Book8_Sum = Book8_Sum + lnUsedQty8
          SELECT OrdLine
          nAvlblQty   = OrdLine.TotBook - nTotBook_Sum - lnTotUsdQty

          IF nAvlblQty > 0
            *&&h Do While IIf(nAvlblQty < lnStQty,.T.,.F.)
            *&&h To End ALL the line Value
            lminFlag = IIF(nAvlblQty > lnStQty_Hld,.T.,.F.)
            *******************************************************************
            XSTYLE = OrdLine.STYLE
            XPACK  = SPCK_LIN.Pack_id
            =SEEK(XSTYLE,'STYLE')
            *B803897,1 Update GL Link Codes
            m.gl_Sales = lcSlsLink+STYLE.cSlsGlLink
            m.gl_Sales = IIF(llLinkToGl AND SEEK('02'+m.gl_Sales,'GL_LINK'),m.gl_Sales,'DEFDEF')
            *B803897,1 (End)
            xSeason   = STYLE.Season
            xWareCode = IIF(EMPTY(lcDefWare),STYLE.cDefWare,lcDefWare)
            XScale    = STYLE.SCALE
            SELECT EDIORDLN
            *B606913,1 HBG 27/01/2003 Seek using the Full Pack ID [Begin]
            lcPack = XPACK+PADR(SPCK_LIN.cPkColor,6)+PADR(SPCK_LIN.cPckSize,3)+PADR(SPCK_LIN.cPkVersion,4)
            *IF !SEEK(PADR(lcDumStore , 8) + XSTYLE + XPACK)
            IF !SEEK(PADR(lcDumStore , 8) + XSTYLE + lcPack)
              *B606913,1 [End]
              MLINE = MLINE + 1
              APPEND BLANK
              *E124848,1 WAM 11/07/2004 Update release order with bulk order configuration
              REPLACE Account    WITH MACCOUNT  ,;
                CustPO     WITH MCustPo   ,;
                STORE      WITH lcDumStore   ,;
                STYLE      WITH XSTYLE    ,;
                Pack_id    WITH XPACK     ,;
                ORDER      WITH MORDER    ,;
                LINENO     WITH MLINE     ,;
                START      WITH MSTART    ,;
                COMPLETE   WITH MCOMPLETE ,;
                SCALE      WITH XScale    ,;
                cLineStat  WITH MLNSTATUS ,;
                Season     WITH xSeason   ,;
                DESC1      WITH IIF(EMPTY(MDESC1),STYLE.DESC1,MDESC1)    ,;
                cWareCode  WITH IIF(EMPTY(lcDefWare),STYLE.cDefWare,lcDefWare) ,;
                nSugRetPri WITH MSRP ,;
                gl_Sales   WITH m.gl_Sales,;
                cFromOrder WITH cBlkOrd,;
                BulkLineNo WITH OrdLine.LINENO,;
                cPkColor   WITH lcPack_Clr,;
                cPckSize   WITH lcPack_Sz,;
                cPkVersion WITH lcPack_Vrn,;
                nPkSlPrice WITH MPck_Price,;
                Dyelot     WITH OrdLine.Dyelot
              *E124848,1 WAM 11/07/2004 (End)
              *C200479,1 HBG 8/1/2003 Updates new fields lRange & lPckPrPiec [Begin]
              REPLACE lRange     WITH llPack_Rng,;
                lPckPrPiec WITH llPack_PrPk
              *C200479,1 [End]
              IF XPoId = 'PC' AND !EMPTY(MLNSTATUS)
                REPLACE DESC1 WITH 'SIZES MODIFIED:'
              ENDIF
              IF llCatDanial
                REPLACE cPack WITH MPACK
              ENDIF
            ENDIF
            IF XPoId = 'PC' AND !EMPTY(MLNSTATUS)
              lcSizesMod = ''
              FOR lnCount = 1 TO 8
                lcCount = STR(lnCount,1)
                IF SPCK_LIN.Qty&lcCount <> 0
                  lcSizesMod = lcSizesMod + 'SIZE'+lcCount + ','
                ENDIF
              ENDFOR
              REPLACE DESC1 WITH ALLTRIM(DESC1)+lcSizesMod
            ENDIF
            *!*     lFillFlag = .T.
            IF lminFlag
              FOR lnCount = 1 TO 8
                lcCount = STR(lnCount,1)
                lnDiff  = (OrdLine.Book&lcCount - Book&lcCount._Sum )
                *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[Begin]
                *IF lnDiff > lnStQty_Hld
                IF lnDiff < lnStQty_Hld
                  *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[End]
                  REPLACE Qty&lcCount    WITH  Qty&lcCount  +lnDiff;
                    Book&lcCount   WITH  Book&lcCount +lnDiff
                  lnStQty_Hld = lnStQty_Hld - lnDiff
                ELSE
                  REPLACE Qty&lcCount    WITH  Qty&lcCount  + lnStQty_Hld;
                    Book&lcCount   WITH  Book&lcCount + lnStQty_Hld
                  *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[Begin]
                  *lnStQty_Hld = 0
                  *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[End]
                  EXIT
                ENDIF
              ENDFOR
              *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[Begin]
              *!*                  REPLACE  TOTQty   WITH TOTQty  + (lnStQty - lnStQty_Hld),;
              *!*                    PRICE    WITH PRICE   + IIF(lnPrice=0,PRICE,lnPrice)* (lnStQty - lnStQty_Hld) ,;
              *!*                    NOTE_MEM WITH MLNOTES + CHR(13) ADDI
              *!*                  REPLACE  TotBook WITH TotBook + (lnQty - lnStQty_Hld)
              *!*                  MBOOK    = MBOOK + (lnQty - lnStQty_Hld)
              *!*                  MBOOKAMT = MBOOKAMT +PRICE * (lnQty - lnStQty_Hld)
              REPLACE  TotQTY   WITH TotQTY  + lnStQty_Hld,;
                PRICE    WITH PRICE   + IIF(lnPrice=0,PRICE,lnPrice),;
                NOTE_MEM WITH MLNOTES + CHR(13) ADDI
              REPLACE  TotBook WITH TotBook + lnStQty_Hld
              MBOOK    = MBOOK + lnStQty_Hld
              MBOOKAMT = MBOOKAMT +PRICE * lnStQty_Hld
              lnStQty_Hld = 0
              *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[End]
            ELSE
              *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[Begin]
              *!*                  REPLACE Qty1    WITH  Qty1   +(OrdLine.Book1-Book1_Sum),;
              *!*                    Qty2    WITH  Qty2   + (OrdLine.Book2-Book2_Sum),;
              *!*                    Qty3    WITH  Qty3   + (OrdLine.Book3-Book3_Sum),;
              *!*                    Qty4    WITH  Qty4   + (OrdLine.Book4-Book4_Sum),;
              *!*                    Qty5    WITH  Qty5   + (OrdLine.Book5-Book5_Sum),;
              *!*                    Qty6    WITH  Qty6   + (OrdLine.Book6-Book6_Sum),;
              *!*                    Qty7    WITH  Qty7   + (OrdLine.Book7-Book7_Sum),;
              *!*                    Qty8    WITH  Qty8   + (OrdLine.Book8-Book8_Sum),;
              *!*                    TOTQty  WITH  TOTQty + nTotBook_Sum,;
              *!*                    PRICE   WITH PRICE + IIF(lnPrice=0,PRICE,lnPrice)*nTotBook_Sum ,;
              *!*                    NOTE_MEM WITH MLNOTES + CHR(13) ADDI
              *!*                  *&&H ModiFy Price in Above Line
              *!*                  REPLACE Book1   WITH Book1   + (OrdLine.Book1-Book1_Sum),;
              *!*                    Book2   WITH Book2   + (OrdLine.Book2-Book2_Sum),;
              *!*                    Book3   WITH Book3   + (OrdLine.Book3-Book3_Sum),;
              *!*                    Book4   WITH Book4   + (OrdLine.Book4-Book4_Sum),;
              *!*                    Book5   WITH Book5   + (OrdLine.Book5-Book5_Sum),;
              *!*                    Book6   WITH Book6   + (OrdLine.Book6-Book6_Sum),;
              *!*                    Book7   WITH Book7   + (OrdLine.Book7-Book7_Sum),;
              *!*                    Book8   WITH Book8   + (OrdLine.Book8-Book8_Sum),;
              *!*                    TotBook WITH TotBook + nTotBook_Sum
              *!*                  MBOOK    = MBOOK + nTotBook_Sum
              *!*                  MBOOKAMT = MBOOKAMT +PRICE * nTotBook_Sum
              REPLACE Qty1    WITH  Qty1   +(OrdLine.Book1-Book1_Sum),;
                Qty2    WITH  Qty2   + (OrdLine.Book2-Book2_Sum),;
                Qty3    WITH  Qty3   + (OrdLine.Book3-Book3_Sum),;
                Qty4    WITH  Qty4   + (OrdLine.Book4-Book4_Sum),;
                Qty5    WITH  Qty5   + (OrdLine.Book5-Book5_Sum),;
                Qty6    WITH  Qty6   + (OrdLine.Book6-Book6_Sum),;
                Qty7    WITH  Qty7   + (OrdLine.Book7-Book7_Sum),;
                Qty8    WITH  Qty8   + (OrdLine.Book8-Book8_Sum),;
                TotQTY  WITH  TotQTY + nAvlblQty,;
                PRICE   WITH PRICE + IIF(lnPrice=0,PRICE,lnPrice) ,;
                NOTE_MEM WITH MLNOTES + CHR(13) ADDI
              REPLACE Book1   WITH Book1   + (OrdLine.Book1-Book1_Sum),;
                Book2   WITH Book2   + (OrdLine.Book2-Book2_Sum),;
                Book3   WITH Book3   + (OrdLine.Book3-Book3_Sum),;
                Book4   WITH Book4   + (OrdLine.Book4-Book4_Sum),;
                Book5   WITH Book5   + (OrdLine.Book5-Book5_Sum),;
                Book6   WITH Book6   + (OrdLine.Book6-Book6_Sum),;
                Book7   WITH Book7   + (OrdLine.Book7-Book7_Sum),;
                Book8   WITH Book8   + (OrdLine.Book8-Book8_Sum),;
                TotBook WITH TotBook + nAvlblQty
              MBOOK    = MBOOK + nAvlblQty
              MBOOKAMT = MBOOKAMT +PRICE * nAvlblQty
              lnStQty_Hld = lnStQty_Hld - nAvlblQty
              *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[End]
            ENDIF

            IF TotQTY = 0
              Delete
            ENDIF

            IF lminFlag
              EXIT
            ENDIF
            *******************************************************************
          ENDIF
        ENDIF
      ENDSCAN
      *ENDSCAN
      *&& Write to Log error ,Bulk Order Quantity Not Enough.
      IF !lminFlag AND lnStQty_Hld > 0
        *=FPUTS(lnErorFile
        =FPUTS(EReport, 'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
          ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id+;
          ' have not added all ,as Bulk Order no '+cBlkOrd +' Have not enough Quantity.')
      ENDIF
    ENDIF
  ELSE
    * =lfPPack(lnQty , lcDumStore)
    IF !lMVersion
      = oObject.processPack(lnQty,lcDumStore)
    ENDIF
  ENDIF
ENDFUNC
*:****************************************************************************************
*! Name      : lfAddV
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : Adjust Price And Fill Custom Fields For GMA
*:****************************************************************************************
*! Called from : Class "ProcessPo"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfAddV

  * C200215 Process Pack 04/22/2002 [Begin]
  *B606050,1 Varible 'lcCust_View' Not Found Hassan[Begin]
  LOCAL lcCust_View , lcGma_View
  STORE "NA" TO lcCust_View , lcGma_View
  *B606050,1 Varible 'lcCust_View' Not Found Hassan[End]
  *B606131,1 No Change In Qty in case Of Range Pack  Hassan 06/19/2002[Begin]
  *C200404,1 HBG 09/03/2002 IReset the flag which use to reject the PO if the qty sent is
  *C200404,1                not evenly divisible by the pack qty [Begin]
  llReject = .F.
  *C200404,1 [End]
  *=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
  =SEEK('P'+MKEY,'Spck_Hdr','Spck_HdrVr')

  IF !Spck_HDR.lRange
    *B606131,1 No Change In Qty in case Of Range Pack  Hassan 06/19/2002[End]
    DO CASE
      *C102713,1 Hassan ,Replace MPck_Price = lnPrice with new field !Spck_Hdr.LQtyPcs 11/14/2002 [Begin]
      *CASE  Mord = 3 AND MPck_Price = lnPrice AND INLIST(MUOM,'EA')
    CASE  Mord = 3 AND !Spck_HDR.LQtyPcs AND INLIST(MUOM,'EA')
      *C102713,1 Hassan [End  ]
      lcCust_View = "Pack"
      lcGma_View  = "Pack"
      *C102713,1 Hassan ,Replace MPck_Price = lnPrice with new field !Spck_Hdr.LQtyPcs 11/14/2002 [Begin]
      *CASE  Mord = 3 AND MPck_Price <> lnPrice AND INLIST(MUOM,'EA')
    CASE  Mord = 3 AND Spck_HDR.LQtyPcs AND INLIST(MUOM,'EA')
      *C102713,1 Hassan [End  ]
      IF lFlag = .T.
        *C200404,1 HBG 09/03/2002 If the qty sent is not evenly divisible by
        *C200404,1                the pack qty , reject the PO [Begin]
        *lnQty = ROUND(lnQty/MPckQty,0)
        lnQty = lnQty/MPckQty
        IF MOD(lnQty,1) <> 0
          llReject = .T.
        ENDIF
        *C200404,1 [End]
      ENDIF
      &lcGmaQty. = lnQty
      *C102713,1 Hassan ,Replace "Style" by  "Pieces" 11/14/2002 [Begin]
      *lcCust_View = "Style"
      lcCust_View = "Pieces"
      *C102713,1 Hassan [End  ]
      lcGma_View  = "Pack"

    CASE Mord = 3 AND INLIST(MUOM,'CA','CT')
      lcCust_View = "Pack"
      lcGma_View  = "Pack"
      IF lFlag = .T.
        IF MPri <> 0
          IF MPo4Qs<>0 OR MPo4Qi<>0
            MPo4Qs = IIF(MPo4Qs=0,1,MPo4Qs)

            *C200404,1 HBG 09/03/2002 If the qty sent is not evenly divisible by
            *C200404,1                the pack qty , reject the PO [Begin]
            *lnQty = ROUND(MPo4Qi*MPo4Qs*lnQty/MPckQty,0)
            lnQty = MPo4Qi*MPo4Qs*lnQty/MPckQty
            IF MOD(lnQty,1) <> 0
              llReject = .T.
            ENDIF
            *C200404,1 [End]

            &lcGmaQty. = lnQty
          ELSE
            *As ...1
            *C102713,1 Hassan ,Replace MPck_Price = lnPrice with new field !Spck_Hdr.LQtyPcs [Begin]
            *IF MPck_Price <> lnPrice
            IF Spck_HDR.LQtyPcs
              *C102713,1 Hassan [End  ]
              *C200404,1 HBG 09/03/2002 If the qty sent is not evenly divisible by
              *C200404,1                the pack qty , reject the PO [Begin]
              *lnQty = ROUND(lnQty/MPckQty,0)
              lnQty = lnQty/MPckQty
              IF MOD(lnQty,1) <> 0
                llReject = .T.
              ENDIF
              *C200404,1 [End]
              &lcGmaQty. = lnQty
              *C102713,1 Hassan ,Replace "Style" by  "Pieces" 11/14/2002 [Begin]
              *lcCust_View = "Style"
              lcCust_View = "Pieces"
              *C102713,1 Hassan [End  ]
              lcGma_View  = "Pack"
            ENDIF
          ENDIF
        ELSE
          *C200404,1 HBG 09/03/2002 If the qty sent is not evenly divisible by
          *C200404,1                the pack qty , reject the PO [Begin]
          *lnQty = ROUND(lnQty*MInerPack*MPackUnit/MPckQty,0)
          lnQty = lnQty*MInerPack*MPackUnit/MPckQty
          IF MOD(lnQty,1) <> 0
            llReject = .T.
          ENDIF
          *C200404,1 [End]
          &lcGmaQty. = lnQty
        ENDIF
      ENDIF
      *B606050,1 Varible 'lcCust_View' Not Found Hassan[Begin]
      *CASE Mord = 1 AND PRICE = lnPrice
    CASE INLIST(Mord,1,2,5) AND PRICE = lnPrice AND INLIST(MUOM,'EA')
      *B606050,1 Varible 'lcCust_View' Not Found Hassan[End]
      lcCust_View = "Style"
      lcGma_View  = "Style"

      *B606050,1 Varible 'lcCust_View' Not Found Hassan[Begin]
      *CASE Mord=1 AND PRICE <> lnPrice
    CASE INLIST(Mord,1,2,5) AND PRICE <> lnPrice AND INLIST(MUOM,'EA')
      *B606050,1 Varible 'lcCust_View' Not Found Hassan[End]
      lcCust_View = "NA"
      lcGma_View  = "Style"

    CASE INLIST(Mord,1,2,5) AND INLIST(MUOM,'CT','CA')
      *B606050,1 Varible 'lcCust_View' Not Found Hassan[End]
      lcCust_View = "NA"
      lcGma_View  = "Style"

    ENDCASE
    *B606131,1 No Change In Qty in case Of Range Pack  Hassan 06/19/2002[End]
  ENDIF
  *B606131,1 No Change In Qty in case Of Range Pack  Hassan 06/19/2002[End]

  IF lFlag = .F.
    IF Mord = 3
      *C200404,1 HBG 09/03/2002 If the qty sent is not evenly divisible by
      *C200404,1                the pack qty so reject the PO [Begin]
      IF !llReject
        *C200404,1 HBG 09/03/2002 Update new cPckAcc field in ORDLINE with the
        *C200404,1                Pack account 'Generic or Account code'
        *!*            REPLACE cCust_View WITH lcCust_View;
        *!*                     cGMA_View  WITH lcGma_View;
        *!*                     cPkColor   WITH lcPack_Clr;
        *!*                     cPckSize   WITH lcPack_Sz;
        *!*                     cPkVersion WITH lcPack_Vrn;
        *!*                     nPkSlPrice WITH MPck_Price
        *C200410,1 HBG 09/03/2002 Update new nPackNo field in ORDLINE [Begin]
        *!*            REPLACE cCust_View WITH lcCust_View ;
        *!*                     cGMA_View  WITH lcGma_View  ;
        *!*                     cPkColor   WITH lcPack_Clr  ;
        *!*                     cPckSize   WITH lcPack_Sz   ;
        *!*                     cPkVersion WITH lcPack_Vrn  ;
        *!*                     nPkSlPrice WITH MPck_Price  ;
        *!*                     cPckAcc    WITH SUBSTR(MKEY,1,5)
        *C102713,1 Hassan,Price sent(Ps)empty put Pack Prie(Pp)=0,If Pp=Ps put Pp with Ps 11/14/2002 [Begin]
        *!*            REPLACE cCust_View WITH lcCust_View ;
        *!*              cGMA_View  WITH lcGma_View  ;
        *!*              cPkColor   WITH lcPack_Clr  ;
        *!*              cPckSize   WITH lcPack_Sz   ;
        *!*              cPkVersion WITH lcPack_Vrn  ;
        *!*              nPkSlPrice WITH MPck_Price  ;
        *!*              cPckAcc    WITH SUBSTR(MKEY,1,5);
        *!*              nPackNo    WITH lnQty
        *C200479,1 HBG 8/1/2003 Updates new fields lRange & lPckPrPiec [Begin]
        *!*	      *REPLACE cCust_View WITH lcCust_View ;
        *!*	      cGMA_View  WITH lcGma_View  ;
        *!*	      cPkColor   WITH lcPack_Clr  ;
        *!*	      cPckSize   WITH lcPack_Sz   ;
        *!*	      cPkVersion WITH lcPack_Vrn  ;
        *!*	      cPckAcc    WITH SUBSTR(MKEY,1,5);
        *!*	      nPackNo    WITH lnQty
        REPLACE cCust_View WITH lcCust_View ;
          cGMA_View  WITH lcGma_View  ,;
          cPkColor   WITH lcPack_Clr  ,;
          cPckSize   WITH lcPack_Sz   ,;
          cPkVersion WITH lcPack_Vrn  ,;
          cPckAcc    WITH SUBSTR(MKEY,1,5),;
          nPackNo    WITH lnQty,;
          lRange     WITH llPack_Rng,;
          lPckPrPiec WITH llPack_PrPk
        *C200479,1 [End]

        IF lnPrice=0
          REPLACE nPkSlPrice WITH 0
        ELSE
          *B606785,1 WLD as specs of C102713 12/17/2002 [Begin]
          *IF MPck_Price != lnPrice
          *B120352,1 HBG 3/3/2004 Fix bug of rejecting in Case 'CA' because the price mismatch [Begin]
          *IF MPck_Price = lnPrice
          lnPri = IIF(MUOM = 'EA',lnPrice,lnPrice*MPckQty)
          IF MPck_Price = lnPri
            *B120352,1 [End]
            *B606785,1 WLD 12/17/2002 [End  ]
            REPLACE nPkSlPrice WITH MPck_Price
          ELSE
            *B606785,1 HBG  12/17/2002 Reject the PO if the Price of the pack <> the send Price ,not only give an error [Begin]
            *B607460,1 HBG 07/09/2003 when process 860 the change of the price is not updated [Begin]
            *IF RFLAG
            IF RFLAG AND EDILIBDT.cEDITrnTyp <> '860'
              *B607460,1 [End]
              *B606785,1 [ End]
              =FPUTS(EReport ,'File Code#: '+lcFileCode)
              =FPUTS(EReport ,'ACCOUNT#: '  + MACCOUNT)
              =FPUTS(EReport ,'CUST. PO#: ' + ALLTRIM(MCustPo))
              IIF(!EMPTY(&lcStores..STORE),FPUTS(EReport ,'Store #: ' + ALLTRIM(&lcStores..STORE)),.T.)
              IIF(!EMPTY(MUPC),FPUTS(EReport ,'UPC : ' + ALLTRIM(MUPC)),.T.)
              *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
              *IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle)),.T.)
              IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle+MSiz)),.T.)
              *HBG [End]
              =FPUTS(EReport ,'Pack ID : ' + ALLTRIM(Spck_HDR.Pack_id))
              *B606785,1 HBG  12/17/2002 Reject the PO if the Price of the pack <> the send Price ,not only give an error [Begin]
              *=FPUTS(EReport ,'Price sent does match pack price for pack : ' +;
              * ALLTRIM(MKEY)+ Space(1) + ALLTRIM(Spck_HDR.cPkColor) + Space(1) +;
              * ALLTRIM(Spck_HDR.cPkSize) + Space(1) + ALLTRIM(Spck_HDR.cPkVersion) + '.')
              =FPUTS(EReport ,'Price sent does match pack price for pack : ' + ALLTRIM(MKEY) + '.')
              RFLAG = .F.
              *B607460,1 HBG 07/09/2003 when process 860 the change of the price is not updated [Begin]
            ELSE
              IF EDILIBDT.cEDITrnTyp = '860'
                REPLACE nPkSlPrice WITH lnPrice
              ENDIF
              *B607460,1 [End]
            ENDIF
            *B606785,1 [End]
          ENDIF
        ENDIF
        *C102713,1 Hassan [End  ]
        *C200410,1 [End]
        *C200404,1 [End]

        *C102713,1 Hassan   replace MPck_Price = lnPrice with new field !Spck_Hdr.LQtyPcs 11/14/2002 [Begin]
        *IF MPck_Price = lnPrice
        *HBG 24/02/2003 Fix bug of The system updates the style price with the price sent [Begin]
        *IF !Spck_HDR.LQtyPcs
        *HBG [End]
        *C102713,1 Hassan [End  ]
        *E124848,1 WAM 11/07/2004 Update configuration with pack configuration
        REPLACE PRICE  WITH SPCK_LIN.nPck_Price ,;
          Dyelot WITH SPCK_LIN.Dyelot
        *E124848,1 WAM (End)

        *HBG 24/02/2003 Fix bug of The system updates the style price with the price sent [Begin]
        *ENDIF
        *HBG [End]
        *C200404,1 HBG 09/03/2002 End If the qty sent is not evenly divisible by
        *C200404,1                the pack qty so reject the PO [Begin]
      ENDIF
      *C200404,1 [End]
    ELSE
      REPLACE cCust_View WITH lcCust_View ;
        cGMA_View  WITH lcGma_View
    ENDIF
  ENDIF
ENDFUN
*:****************************************************************************************
*! Name      : lfOPNFLS
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : OPen Files For Gma Using In Process Edi File.
*:****************************************************************************************
*! Called from : Class "ProcessPo" Method 'Init'
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfOPNFLS

  lnArrLenR = ALEN(oObject.FilesToOpen , 1)
  lnArrLenC = ALEN(oObject.FilesToOpen , 2)
  DIMENSION oObject.FilesToOpen[lnArrLenR+5,lnArrLenC]

  oObject.FilesToOpen[6,5] = 'SPCK_HDRVR'
  oObject.FilesToOpen[7,5] = 'SPCK_LINVR'

  lnArrLenR = lnArrLenR + 1
  oObject.FilesToOpen[lnArrLenR,1] = 'D'
  oObject.FilesToOpen[lnArrLenR,2] = 'ORDLINE'
  oObject.FilesToOpen[lnArrLenR,3] = ''
  oObject.FilesToOpen[lnArrLenR,4] = 'BlkORDLN'
  oObject.FilesToOpen[lnArrLenR,5] = 'OrdBulk'
  oObject.FilesToOpen[lnArrLenR,7] = 1

  lnArrLenR = lnArrLenR + 1
  oObject.FilesToOpen[lnArrLenR,1] = 'D'
  oObject.FilesToOpen[lnArrLenR,2] = 'ConflctH'
  oObject.FilesToOpen[lnArrLenR,3] = ''
  oObject.FilesToOpen[lnArrLenR,4] = 'ConflctH'
  oObject.FilesToOpen[lnArrLenR,5] = 'ConflctH'
  oObject.FilesToOpen[lnArrLenR,7] = 1

  lnArrLenR = lnArrLenR + 1
  oObject.FilesToOpen[lnArrLenR,1] = 'D'
  oObject.FilesToOpen[lnArrLenR,2] = 'ConflctL'
  oObject.FilesToOpen[lnArrLenR,3] = ''
  oObject.FilesToOpen[lnArrLenR,4] = 'ConflctL'
  oObject.FilesToOpen[lnArrLenR,5] = 'ConflctL'
  oObject.FilesToOpen[lnArrLenR,7] = 1

  lnArrLenR = lnArrLenR + 1
  oObject.FilesToOpen[lnArrLenR,1] = 'D'
  oObject.FilesToOpen[lnArrLenR,2] = 'CnfHdrAr'
  oObject.FilesToOpen[lnArrLenR,3] = ''
  oObject.FilesToOpen[lnArrLenR,4] = 'CnfHdrAr'
  oObject.FilesToOpen[lnArrLenR,5] = ''
  oObject.FilesToOpen[lnArrLenR,7] = 1

  lnArrLenR = lnArrLenR + 1
  oObject.FilesToOpen[lnArrLenR,1] = 'D'
  oObject.FilesToOpen[lnArrLenR,2] = 'CnfLinAr'
  oObject.FilesToOpen[lnArrLenR,3] = ''
  oObject.FilesToOpen[lnArrLenR,4] = 'CnfLinAr'
  oObject.FilesToOpen[lnArrLenR,5] = 'ConflctL'
  oObject.FilesToOpen[lnArrLenR,7] = 1

ENDFUN
****************************************************************************************
*! Name      : lfBLKBRW
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : Browse For Gma
*:****************************************************************************************
*! Called from : Class "ProcessPo"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfBLKBRW

  PUBLIC llgma_Browse
  llgma_Browse = .T.
  *C200349,1 Hassan [Begin]
  *!*    *If MESSAGEBOX('Are you sure you do not want to apply this order to any bulk order?',4+32,_SCREEN.CAPTION)=6
  *!*    do While .T.
  *!*      =gfBrowse("Order:H='Order#',Account,CustPo :H='Customer PO#',Style:H='Style',"+;
  *!*              "Start:H='Start date',Complete:H='Completion date',pack_id:H='Pack Id',"+;
  *!*              "cPkColor:H='Pack color',cPckSize:H='Pack size',cPkVersion:H='Pack version',"+;
  *!*              "price :H='Unit Price',totqty :H='Total Qty',cfromorder:H='Bulk Order'",;
  *!*              'Order Lines',lcOrdLine,.T.,.T.,'FFTF',.T.)
  *!*
  *!*      Select lcOrdLine
  *!*      lIn =.F.
  *!*      Scan for Empty(cfromorder)
  *!*        lIn = .T.
  *!*        Exit
  *!*      endscan
  *!*      If lIn
  *!*        if MESSAGEBOX('One or some lines have no Bulk Order to deplete from'+chr(13)+'Are You Sure You Want to Continue?',4+32,_SCREEN.CAPTION)=6
  *!*          exit
  *!*        endif
  *!*      else
  *!*        exit
  *!*      EndIF
  *!*    *=gfBrowse([Order:H='Order#',Account,CustPo :H='Customer PO#',;
  *!*                Start:H='Start date',Complete:H='Completion date',pack_id:H='Pack Id',;
  *!*                price:H='Unit Price',totqty :H='Total Qty',;
  *!*                cfromorder:H='Bulk Order' ],;
  *!*                'Order Lines',lcOrdLine,;
  *!*                .T.,.T.,'FFTF',.T.)
  *!*    Enddo
  DO FORM oAriaApplication.ScreenHome+"EBGmaAdp" WITH lcOrdLine
  *C200349,1 Hassan [End]
  Release llgma_Browse
ENDFUN
*:****************************************************************************************
*! Name      : lfADJBRW
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : Adjust Browse For Gma
*:****************************************************************************************
*! Called from : Screen 'Browse'
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfADJBRW

  IF llgma_Browse
    FOR lnCount = 1 TO (oBrowse.COLUMNCOUNT -1)
      oBrowse.COLUMNS(lnCount).VISIBLE = .T.
      oBrowse.COLUMNS(lnCount).ADDOBJECT('TEXT2','GRIDCOLUMN')
      oBrowse.COLUMNS(lnCount).TEXT2.VISIBLE = .T.
      oBrowse.COLUMNS(lnCount).CURnRENTCONTROL ='TEXT2'
    ENDFOR
    oBrowse.COLUMNS(lnCount).VISIBLE = .T.
    oBrowse.COLUMNS(lnCount).ADDOBJECT('TEXT2','BRWTXT')
    oBrowse.COLUMNS(lnCount).TEXT2.VISIBLE = .T.
    oBrowse.COLUMNS(lnCount).CURRENTCONTROL ='TEXT2'
  ENDIF
ENDFUN
*:****************************************************************************************
*! Name      : lfEDTBRW
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : Enable Browse to be Editable For Gma
*:****************************************************************************************
*! Called from : Screen 'Browse'
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfEDTBRW

  BROWSE FIELDS &tcBrowseFields ;
    WINDOW (PcWinName) IN WINDOW (oObject.NAME) ;
    NOWAIT SAVE NODELE NOAPP  ;
    NAME oBrowse TITLE oObject.BrowseTitle ;
    &lcKey &lcFor
ENDFUN
*:****************************************************************************************
*! Name      : lfUPCSKU
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : Seek By UPC Or SKU For Gma
*:****************************************************************************************
*! Called from : Class 'ProcessPo' Method 'ProcessPo'
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfUPCSKU

  *C102713,1 Hassan ,process item by UPC , SKU or Both [Begin]
  *lcPartSetup = ['UO', 'SO','US','SU'] mean {"UPC ONLY","SKU ONLY","UPC/SKU", "SKU/UPC"}
  lcPartSetup = IIF(EMPTY(EdiAcPrt.cPoRcvLine),'US',EdiAcPrt.cPoRcvLine)
  *C102713,1 Hassan [End  ]

  DO CASE
    *1* Case UPC Search For Style With Confirmation Of SKU Exist In DB If Sent.

    *C102713,1 Hassan   Types of process the item UPC,SKU,BOTH 11/14/2002 [Begin]
    *!*    CASE llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') AND;
    *!*      !StyleUpc.lUPCPack .AND. (EMPTY(MStyle) OR SEEK('S' + MKey , 'Spck_Lin'))
  CASE llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') AND;
      !StyleUpc.lUPCPack .AND. (INLIST(lcPartSetup,'UO') OR EMPTY(MStyle) OR SEEK('S' + MKEY , 'Spck_Lin')) ;
      AND INLIST(lcPartSetup,'UO','US','SU')
    *C102713,1 Hassan [End  ]

    Mord = 5

    *2* Case UPC Search  For Pack With Confirmation Of SKU Exist In DB If Sent.
    * Note Generic  Pack  Has UPC Only,But No SKU
    * Note Specific Pack  Has SKU Only,But No UPC
    *C102713,1 Hassan   Types of process the item UPC,SKU,BOTH 11/14/2002 [Begin]
    *!*    CASE llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') .AND. StyleUpc.lUPCPack .AND. ;
    *!*      SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
  CASE llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') .AND. StyleUpc.lUPCPack .AND. ;
      SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr') ;
      AND INLIST(lcPartSetup,'UO','US','SU')
    *C102713,1 Hassan [End  ]

    *B606306,1 As Per Emad GMA Hassan [Begin]
    *!*      IF EMPTY(MStyle) OR;
    *!*          (SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr')AND;
    *!*          SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
    *!*          Spck_HDR.CSKUTYPE="P")
    *!*        =SEEK(PADR(MUPC,13),'StyleUpc')
    *!*        =SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
    *!*        lcPack_Id  = Spck_HDR.Pack_Id
    *!*        lcPack_Sz  = Spck_HDR.cPckSize
    *!*        lcPack_Clr = Spck_HDR.cPkColor
    *!*        lcPack_Vrn = Spck_HDR.cPkVersion
    *!*        SELECT SPCK_LIN
    *!*        lcOrder = SET('Order')
    *!*        *SET ORDER TO Spck_linvr
    *!*        MKey  = '*****' + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
    *!*        MType = 'P'
    *!*        Mord  = 3
    *!*        =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
    *!*        *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
    *!*        SUM REST TOTQty  TO MPckQty  WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
    *!*        =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
    *!*        *SET ORDER TO SPCK_LIN
    *!*        MPck_Price = Spck_HDR.nPkSlPrice
    *!*      ELSE
    *!*        =FPUTS(EReport , ' SKU Send #:'+ALLTRIM(MStyle)+ 'is not in our DB.')
    *!*      ENDIF
    IF !EMPTY(MStyle) AND INLIST(lcPartSetup,'US','SU')
      *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
      *IF (SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr')AND;
      *    SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
      *    Spck_HDR.CSKUTYPE="P")
      IF (SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle + MSiz),16),'Spck_Lin','Spck_LinVr')AND;
          SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
          Spck_HDR.CSKUTYPE="P")
        *HBG [End]
        *BXXX Hassan 08/27/2002[Begin]
        *=SEEK(PADR(MUPC,13),'StyleUpc')
        *=SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
        =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
        *BXXX Hassan 08/27/2002 [End  ]
        lcPack_Id   = Spck_HDR.Pack_id
        lcPack_Sz   = Spck_HDR.cPckSize
        lcPack_Clr  = Spck_HDR.cPkColor
        lcPack_Vrn  = Spck_HDR.cPkVersion
        *C200479,1 HBG 8/1/2003 Get the value of fields lRange & lPckPrPiec [Begin]
        llPack_Rng  = Spck_HDR.lRange
        llPack_PrPk = Spck_HDR.lPckPrPiec
        *C200479,1 [End]

        SELECT SPCK_LIN
        lcOrder = SET('Order')
        *BXXX Hassan 08/27/2002 [Begin ]
        *SET ORDER TO Spck_linvr
        *MKey  = '*****' + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
        MKEY  = PADR(ALLTRIM(MACCOUNT),5) + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
        *BXXX Hassan 08/27/2002 [End  ]
        MTYPE = 'P'
        Mord  = 3
        *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
        IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
            AND !EMPTY(ConflctL.cPkVersion)
          MKEY = PADR(ALLTRIM(MACCOUNT),5)+ lcPack_Id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
          *B607479,1 HBG 08/10/2003 Get the selected version in conflict screen [Begin]
          lcPack_Vrn = ConflctL.cPkVersion
          *B607479,1 [End]
        ENDIF
        *C200404,1 [End]
        =SEEK('P' + MKEY,'Spck_Lin','Spck_linvr')
        *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
        SUM REST TotQTY  TO MPckQty  WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P' + MKEY
        =SEEK('P' + MKEY,'Spck_Lin','Spck_linvr')
        *SET ORDER TO SPCK_LIN
        *B606892,1 HBG 19/01/2003 Fix bug of always reject for “Prices sent doesn’t match…”
        *B606892,1                even if the prices match [Begin]
        =SEEK('P' + MKEY,'Spck_HDR','Spck_hdrvr')
        *B606892,1 [End]
        MPck_Price = Spck_HDR.nPkSlPrice
      ELSE
        *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
        *=FPUTS(EReport , ' SKU Send #:'+ALLTRIM(MStyle)+ 'is not in our DB.')
        =FPUTS(EReport , ' SKU Send #:'+ALLTRIM(MStyle+MSiz)+ 'is not in our DB.')
        *HBG [End]
        *C102713,1 Hassan   If Pack does not exist 11/14/2002 [Begin]
        =FPUTS(EReport ,'Pack does not exist. ')
        *C102713,1 Hassan [End  ]
      ENDIF
    ELSE
      IF llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') .AND. StyleUpc.lUPCPack .AND. ;
          SEEK('P' + PADR(ALLTRIM(MACCOUNT),5) +PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
        lcPack_Id  = Spck_HDR.Pack_id
        lcPack_Sz  = Spck_HDR.cPckSize
        lcPack_Clr = Spck_HDR.cPkColor
        lcPack_Vrn = Spck_HDR.cPkVersion
        *C200479,1 HBG 8/1/2003 Get the value of fields lRange & lPckPrPiec [Begin]
        llPack_Rng  = Spck_HDR.lRange
        llPack_PrPk = Spck_HDR.lPckPrPiec
        *C200479,1 [End]
        SELECT SPCK_LIN
        lcOrder = SET('Order')
        *SET ORDER TO Spck_linvr
        MKEY  = PADR(ALLTRIM(MACCOUNT),5) + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
        MTYPE = 'P'
        Mord  = 3
        *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
        IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
            AND !EMPTY(ConflctL.cPkVersion)
          MKEY = PADR(ALLTRIM(MACCOUNT),5)+ lcPack_Id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
          *B607479,1 HBG 08/10/2003 Get the selected version in conflict screen [Begin]
          lcPack_Vrn = ConflctL.cPkVersion
          *B607479,1 [End]
        ENDIF
        *C200404,1 [End]
        =SEEK('P' + MKEY,'Spck_Lin','Spck_linvr')
        *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
        SUM REST TotQTY  TO MPckQty  WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P' + MKEY
        =SEEK('P' + MKEY,'Spck_Lin','Spck_linvr')
        *SET ORDER TO SPCK_LIN
        *B606892,1 HBG 19/01/2003 Fix bug of always reject for “Prices sent doesn’t match…”
        *B606892,1                even if the prices match [Begin]
        =SEEK('P' + MKEY,'Spck_HDR','Spck_hdrvr')
        *B606892,1 [End]
        MPck_Price = Spck_HDR.nPkSlPrice
      ELSE
        IF llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') .AND. StyleUpc.lUPCPack .AND. ;
            SEEK('P' + '*****' +PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
          lcPack_Id  = Spck_HDR.Pack_id
          lcPack_Sz  = Spck_HDR.cPckSize
          lcPack_Clr = Spck_HDR.cPkColor
          lcPack_Vrn = Spck_HDR.cPkVersion
          *C200479,1 HBG 8/1/2003 Get the value of fields lRange & lPckPrPiec [Begin]
          llPack_Rng  = Spck_HDR.lRange
          llPack_PrPk = Spck_HDR.lPckPrPiec
          *C200479,1 [End]
          SELECT SPCK_LIN
          lcOrder = SET('Order')
          *SET ORDER TO Spck_linvr
          MKEY  = '*****' + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
          MTYPE = 'P'
          Mord  = 3
          *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
          IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
              AND !EMPTY(ConflctL.cPkVersion)
            MKEY = '*****'+ lcPack_Id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
            *B607479,1 HBG 08/10/2003 Get the selected version in conflict screen [Begin]
            lcPack_Vrn = ConflctL.cPkVersion
            *B607479,1 [End]
          ENDIF
          *C200404,1 [End]
          =SEEK('P' + MKEY,'Spck_Lin','Spck_linvr')
          *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
          SUM REST TotQTY  TO MPckQty  WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P' + MKEY
          =SEEK('P' + MKEY,'Spck_Lin','Spck_linvr')
          *SET ORDER TO SPCK_LIN
          *B606892,1 HBG 19/01/2003 Fix bug of always reject for “Prices sent doesn’t match…”
          *B606892,1                even if the prices match [Begin]
          =SEEK('P' + MKEY,'Spck_HDR','Spck_hdrvr')
          *B606892,1 [End]
          MPck_Price = Spck_HDR.nPkSlPrice
          *C102713 WLD ,write in error log if Pack does not exist 11/14/2002 [Begin]
        ELSE
          =FPUTS(EReport ,'Pack does not exist. ')
          *C102713 WLd 11/14/2002 [End  ]
        ENDIF
      ENDIF
    ENDIF
    *B606306,1 As Per Emad GMA Hassan [End]
    *CXXXX As Per Emad GMA Hassan [Begin]

    *4* Case SKU Search  For Pack For GMA
    * Note Generic  Pack  Has UPC Only,But No SKU
    * Note Specific Pack  Has SKU Only,But No UPC
    *!*    CASE SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Hdr','Spck_HdrVr')AND Spck_HDR.CSKUTYPE="P"
    *!*      lcPack_Id  = ALLTRIM(Spck_HDR.STYLE)
    *!*      lcPack_Sz  = Spck_HDR.cPckSize
    *!*      lcPack_Clr = Spck_HDR.cPkColor
    *!*
    *!*      =SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
    *!*      lcPack_Id  = Spck_HDR.Pack_Id
    *!*      lcPack_Vrn = Spck_HDR.cPkVersion
    *!*      SELECT SPCK_LIN
    *!*      *SET ORDER TO Spck_linvr
    *!*      MKey  = MACCOUNT + Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn
    *!*      MType = 'P'
    *!*      Mord  = 3
    *!*      =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
    *!*      *SUM REST TOTQty,nPck_Price TO MPckQty,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
    *!*      SUM REST TOTQty TO MPckQty WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
    *!*      =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
    *!*      *SELECT SPCK_LIN
    *!*      *SET ORDER TO SPCK_LIN
    *!*      MPck_Price = Spck_Hdr.nPkSlPrice
    *C102713,1 Hassan   Types of process the item UPC,SKU,BOTH 11/14/2002 [Begin]
    *!*    CASE SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr')AND;
    *!*      SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
    *!*      Spck_HDR.CSKUTYPE="P"
    *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
    *CASE SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr')AND;
    *    SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
    *    Spck_HDR.CSKUTYPE="P" AND INLIST(lcPartSetup,'SO','US','SU')
  CASE SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle + MSiz),16),'Spck_Lin','Spck_LinVr')AND;
      SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
      Spck_HDR.CSKUTYPE="P" AND INLIST(lcPartSetup,'SO','US','SU')
    *HBG [End]
    *C102713,1 Hassan [End  ]

    lcPack_Id  = ALLTRIM(Spck_HDR.STYLE)
    lcPack_Sz  = Spck_HDR.cPckSize
    lcPack_Clr = Spck_HDR.cPkColor

    =SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
    lcPack_Id  = Spck_HDR.Pack_id
    lcPack_Vrn = Spck_HDR.cPkVersion
    *C200479,1 HBG 8/1/2003 Get the value of fields lRange & lPckPrPiec [Begin]
    llPack_Rng  = Spck_HDR.lRange
    llPack_PrPk = Spck_HDR.lPckPrPiec
    *C200479,1 [End]
    SELECT SPCK_LIN
    *SET ORDER TO Spck_linvr
    *ahmed (Start)
    *MKey  = MACCOUNT + Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn
    MKEY  = PADR(ALLTRIM(MACCOUNT),5) + Spck_HDR.Pack_id+lcPack_Clr+lcPack_Sz+lcPack_Vrn
    *ahmed (End)

    MTYPE = 'P'
    Mord  = 3
    *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
    IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(Spck_HDR.Pack_id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
        AND !EMPTY(ConflctL.cPkVersion)
      MKEY = PADR(ALLTRIM(MACCOUNT),5)+ Spck_HDR.Pack_id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
      lcPack_Vrn= ConflctL.cPkVersion
      *C200479,1 HBG 8/1/2003 Get the value of fields lRange & lPckPrPiec [Begin]
      =SEEK('P'+MKEY,'Spck_Hdr','Spck_HdrVr')
      llPack_Rng  = Spck_HDR.lRange
      llPack_PrPk = Spck_HDR.lPckPrPiec
      *C200479,1 [End]
    ENDIF
    *C200404,1 [End]

    =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
    *SUM REST TOTQty,nPck_Price TO MPckQty,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
    SUM REST TotQTY TO MPckQty WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P' + MKEY
    =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
    *SELECT SPCK_LIN
    *SET ORDER TO SPCK_LIN
    *B606892,1 HBG 19/01/2003 Fix bug of always reject for “Prices sent doesn’t match…”
    *B606892,1                even if the prices match [Begin]
    =SEEK('P' + MKEY,'Spck_HDR','Spck_hdrvr')
    *B606892,1 [End]
    MPck_Price = Spck_HDR.nPkSlPrice

    *3* Case SKU Search  For Style
    *C102713,1 Hassan   Types of process the item UPC,SKU,BOTH 11/14/2002 [Begin]
    *!*    CASE SEEK('S' + MKey , 'Spck_Lin')
  CASE SEEK('S' + MKEY , 'Spck_Lin') AND INLIST(lcPartSetup,'SO','US','SU')
    *C102713,1 Hassan [End  ]
    MTYPE   = 'S'
    MPrepck = (SPCK_LIN.TotQTY = 0)
    Mord    = IIF(SPCK_LIN.TotQTY=0,2,1)
    *CXXXX As Per Emad GMA Hassan [End]
    *C102713 WLD ,write in error log if UPC does not exist 11/14/2002 [Begin]
  OTHERWISE
    *HBG Remove this block of code because it dublicate the error report [Begin]
    *IF llUPCUse .AND. !SEEK(PADR(MUPC,13),'StyleUpc') AND INLIST(lcPartSetup,'UO','US','SU')
    *  =FPUTS(EReport ,' UPC does not exist. ')
    *ENDIF
    *C102713 WLD [End  ]
    **xxxx wld
    *2*
    *IF llUPCUse .AND. (!SEEK(PADR(MUPC,13),'StyleUpc')  or ;
    *      !SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')) ;
    *      AND INLIST(lcPartSetup,'UO','US','SU')
    *   =FPUTS(EReport ,' UPC does not exist. ')
    *ENDIF
    *4*
    *IF (!SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr') or ;
    *    !SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst'))AND ;
    *       INLIST(lcPartSetup,'SO','US','SU')
    *   =FPUTS(EReport ,' SKU does not exist. ')
    *ENDIF
    *3*
    *IF !SEEK('S' + MKEY , 'Spck_Lin') AND INLIST(lcPartSetup,'SO','US','SU')
    *   =FPUTS(EReport ,' SKU does not exist. ')
    *ENDif
    **xxxx wld
    *HBG [End]
  ENDCASE
ENDFUN
*:****************************************************************************************
*! Name      : lfErr
*! Developer : Hassan Ali (HIA)
*! Date      : 04/30/2002
*! Purpose   : Seek By UPC Or SKU For Gma
*:****************************************************************************************
*! Called from : Class 'ProcessPo' Method 'ProcessPo'
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfErr

  *1* No UPC Or SKU Send.
  IF EMPTY(MUPC) AND EMPTY(MStyle)
    *=FPUTS(EReport , 'Account: '+MACCOUNT+' UPC# And SKU# not sent in the EDI file.')
    =FPUTS(EReport , ' UPC#: is not sent in the EDI file.')
    =FPUTS(EReport , ' SKU#: is not sent in the EDI file.')

  ENDIF
  *2* No UPC But SKU Send.
  IF EMPTY(MUPC) AND !EMPTY(MStyle)
    * =FPUTS(EReport , 'Account: '+MACCOUNT+' UPC# not sent in the EDI file and SKU# sent is not in our DB.')
    =FPUTS(EReport , ' UPC#: is not sent in the EDI file.')
    *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
    *=FPUTS(EReport , ' SKU#: '+ALLTRIM(MStyle))
    =FPUTS(EReport , ' SKU#: '+ALLTRIM(MStyle+MSiz))
    *HBG [End]
  ENDIF
  *3* UPC But No SKU Send.
  IF !EMPTY(MUPC) AND EMPTY(MStyle)
    *=FPUTS(EReport , 'Account: '+MACCOUNT+' SKU# not sent in the EDI file and UPC# sent is not in our DB.')
    =FPUTS(EReport , ' UPC#: '+ALLTRIM(MUPC))
    =FPUTS(EReport , ' SKU#: is not sent in the EDI file.')
  ENDIF
  *4* UPC And SKU Are Sent , But Not In Our DB.
  IF !EMPTY(MUPC) AND !EMPTY(MStyle)
    *=FPUTS(EReport , 'Account: '+MACCOUNT+' UPC# and SKU# sent are not in our DB.')
    =FPUTS(EReport , ' UPC#: '+ALLTRIM(MUPC))
    *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
    *=FPUTS(EReport , ' SKU#: '+ALLTRIM(MStyle))
    =FPUTS(EReport , ' SKU#: '+ALLTRIM(MStyle+MSiz))
    *HBG [End]
  ENDIF

  IF !EMPTY(MSTYL)
    =FPUTS(EReport , '  Style: '+ALLTRIM(MSTYL))
  ENDIF
  IF !EMPTY(MCOLOR)
    =FPUTS(EReport , '  Color: '+ALLTRIM(MCOLOR))
  ENDIF
  IF !EMPTY(MSIZE)
    =FPUTS(EReport , '  Size : '+ALLTRIM(MSIZE))
  ENDIF
  =FPUTS(EReport , '  Quantity Ordered: '+ALLTRIM(STR(MQTY,10))+' '+MUOM)
  IF MInerPack*MPackUnit <> 1
    =FPUTS(EReport , '  No. of inner packs: '+ALLTRIM(STR(MInerPack,10)))
    =FPUTS(EReport , '  No. of pieces per pack: '+ALLTRIM(STR(MPackUnit,10)))
  ENDIF
  RFLAG = .F.

ENDFUN
*!* C200504 HBG This function is commented because it is changed completly
*!*	*:****************************************************************************************
*!*	*! Name      : lfChkRng
*!*	*! Developer : Hassan Ali (HIA)
*!*	*! Date      : 04/30/2002
*!*	*! Purpose   : Seek By UPC Or SKU For Gma
*!*	*:****************************************************************************************
*!*	*! Called from : Class 'ProcessPo' Method 'ProcessPo'
*!*	*:****************************************************************************************
*!*	*! Calls       :
*!*	*:****************************************************************************************
*!*	*! Return      : None
*!*	*:****************************************************************************************
*!*	FUNCTION lfChkRng

*!*	  lRange_Process = .F.
*!*	  cBlkOrd        = ""
*!*	  IF Mord = 3
*!*	    *=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
*!*	    =SEEK('P'+MKEY ,'Spck_Hdr','Spck_HdrVr')
*!*	    IF Spck_HDR.lRange
*!*	      *&&H Check  'ConflictLin' File Is Used
*!*	      *&& Check If The Line In The Conflict File
*!*	      IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
*!*	        *&&Check If The Conflict Resolve
*!*	        IF !EMPTY(ConflctL.cBlkOrd)
*!*	          *C200404,1 HBG 09/03/2002 reject the PO if the qty sent is not evenly
*!*	          *C200404,1                divisible by the pack qty [Begin]
*!*	          IF llReject
*!*	            IF RFLAG
*!*	              =FPUTS(EReport ,'File Code#: '+lcFileCode)
*!*	              =FPUTS(EReport ,'ACCOUNT#: '  + MACCOUNT)
*!*	              =FPUTS(EReport ,'CUST. PO#: ' + ALLTRIM(MCustPo))
*!*	              IIF(!EMPTY(&lcStores..STORE),FPUTS(EReport ,'Store #: ' + ALLTRIM(&lcStores..STORE)),.T.)
*!*	              IIF(!EMPTY(MUPC),FPUTS(EReport ,'UPC : ' + ALLTRIM(MUPC)),.T.)
*!*	              *B606419,1 Hassan [Begin]
*!*	              *IIF(!EMPTY(MSKU),FPUTS(EReport ,'SKU : ' + ALLTRIM(MSKU)),.T.)
*!*	              *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
*!*	              *IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle)),.T.)
*!*	              IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle+MSIZ)),.T.)
*!*	              *HBG [End]
*!*	              *B606419,1 Hassan [End  ]
*!*	              =FPUTS(EReport ,'Pack ID : ' + ALLTRIM(MKEY))
*!*	              =FPUTS(EReport ,'Qty Ordered: '+;
*!*	                IIF(EMPTY(&lcStores..STORE),ALLTRIM(STR(MQTY)),ALLTRIM(STR(&lcStores..nQuantity*MSlnQty))))
*!*	              =FPUTS(EReport ,'Pack Qty : ' + ALLTRIM(STR(MPckQty)))
*!*	              =FPUTS(EReport ,'Rejection Reasone : Qty ordered is not divisable by the Pack qty.')
*!*	              RFLAG = .F.
*!*	            ENDIF
*!*	            RETURN
*!*	          ENDIF
*!*	          *C200404,1 [End]
*!*	          *&& Move this line to 'ConfLinAr'
*!*	          lcAlias = SELECT()
*!*	          SELECT ConflctL
*!*	          *&& Proceed To Process Range Pack Order(1)
*!*	          lRange_Process = .T.
*!*	          cBlkOrd = ConflctL.cBlkOrd
*!*	          *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[Begin]
*!*	          *!*SCATTER TO MEMVAR MEMO
*!*	          *!*SELECT CnfLinAr
*!*	          *!*APPEND BLANK
*!*	          *!*GATHER FROM MEMVAR
*!*	          *!*SELECT ConflctL
*!*	          *!*DELETE
*!*	          DO CASE
*!*	            *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
*!*	            *Case nCounter = 1
*!*	            CASE ConflctL.nCounter = 1
*!*	              *C200404,1 [End]
*!*	              SCATTER TO MEMVAR MEMO
*!*	              SELECT CnfLinAr
*!*	              APPEND BLANK
*!*	              GATHER FROM MEMVAR
*!*	              SELECT ConflctL
*!*	              DELETE
*!*	              *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
*!*	              *Case nCounter > 1
*!*	              *Replace nCounter With (nCounter -1) in ConflctL
*!*	            CASE ConflctL.nCounter > 1
*!*	              lcPrvAls = ALIAS()
*!*	              SELECT ConflctL
*!*	              REPLACE ConflctL.nCounter WITH (ConflctL.nCounter -1)
*!*	              SELECT (lcPrvAls)
*!*	              *C200404,1 [End]
*!*	          ENDCASE
*!*	          *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[End  ]
*!*	          *&& If This Line Is The Last Line Related with Header,Move Header Line
*!*	          *hbg
*!*	          *IF !SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
*!*	          IF !SEEK( lcFileCode + PADR(MCustPo,15),'ConflctL','ConflctL')
*!*	          *hbg
*!*	            SELECT ConflctH
*!*	            SCATTER TO MEMVAR MEMO
*!*	            SELECT CnfHdrAr
*!*	            APPEND BLANK
*!*	            GATHER FROM MEMVAR
*!*	            SELECT ConflctH
*!*	            DELETE
*!*	          ENDIF &&!Seek( lcFileCode + MCustPo +SPCK_HDR.Pack_Id,'ConflctL','ConflctL')
*!*	          SELECT(lcAlias)
*!*	        ELSE
*!*	          *&& Increase 'nCounter'
*!*	          RFLAG = .F.
*!*	          REPLACE ConflctL.nCounter WITH ConflctL.nCounter + 1
*!*	        ENDIF
*!*	      ELSE
*!*	        *&&H Put The Line In The Conflict File.
*!*	        RFLAG = .F.
*!*	        lcAlias = SELECT()
*!*	        SELECT ConflctH
*!*	        IF !SEEK(lcFileCode+MCustPo,'ConflctH','ConflctH')
*!*	          APPEND BLANK
*!*	          REPLACE cFileCode  WITH lcFileCode ;
*!*	            CustPO     WITH MCustPo;
*!*	            Account    WITH MACCOUNT ;
*!*	            RcveDate   WITH EdiLibDt.dDate;
*!*	            RcveTime   WITH EdiLibDt.cTime;
*!*	            ProcessDat WITH oAriaApplication.SystemDate;
*!*	            ProcessTim WITH gfGetTime();
*!*	            cAdd_User  WITH oAriaApplication.User_ID
*!*	        ENDIF
*!*	        SELECT ConflctL
*!*	        APPEND BLANK
*!*	        *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
*!*		    *!* REPLACE cFileCode  WITH lcFileCode ;
*!*			*!*	cPack      WITH Spck_HDR.Pack_id+'-'+Spck_HDR.cPkColor+'-'+Spck_HDR.cPckSize ;
*!*			*!*	CustPO     WITH MCustPo;
*!*			*!*	lRange     WITH Spck_HDR.lRange  ;
*!*			*!*	cUpc       WITH MUPC;
*!*			*!*	cSku       WITH MStyle;
*!*			*!*	cVendrNum  WITH MVend;
*!*			*!*	nPCKQty    WITH MQTY;
*!*			*!*	nPkSlPrice WITH MPri;
*!*			*!*	cPkVersion WITH Spck_HDR.cPkVersion;
*!*			*!*	nCounter   WITH 1;
*!*			*!* cAdd_User  WITH oAriaApplication.User_ID
*!*	        REPLACE cFileCode  WITH lcFileCode ;
*!*	          cPack      WITH Spck_HDR.Pack_id+'-'+Spck_HDR.cPkColor+'-'+Spck_HDR.cPckSize ;
*!*	          CustPO     WITH MCustPo;
*!*	          lRange     WITH Spck_HDR.lRange  ;
*!*	          cUpc       WITH MUPC;
*!*	          cSku       WITH MStyle+MSiz;
*!*	          cVendrNum  WITH MVend;
*!*	          nPCKQty    WITH MQTY;
*!*	          nPkSlPrice WITH MPri;
*!*	          cPkVersion WITH Spck_HDR.cPkVersion;
*!*	          nCounter   WITH 1;
*!*	          cAdd_User  WITH oAriaApplication.User_ID
*!*	        *HBG [End]
*!*	        *B607024,1 HBG 10/03/2003 Update the new field conflctL File with # of packs [Begin]
*!*	        REPLACE nPckNum WITH MPckQty
*!*	        *B607024,1 [End]
*!*	        SELECT(lcAlias)
*!*	        *&& Put This Line In The Error Log.
*!*	        * =FPUTS(lnErorFile
*!*	        =FPUTS(EReport ,'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
*!*	          ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id+;
*!*	          ' Need to get bulk order to deplet from.')
*!*	      ENDIF &&Seek( lcFileCode + MCustPo + SPCK_HDR.Pack_Id,'ConflctL','ConflctL')
*!*	    ELSE
*!*	      *&& Proceed To Version Check.
*!*	      ****************************************************
*!*	      SELECT Spck_HDR
*!*	      *SET ORDER TO Spck_HdrVr
*!*	      *=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
*!*	      *C200404,1 HBG 09/03/2002 Seek for the pack without specifing a version [Begin]
*!*	      *=SEEK('P'+Mkey,'Spck_Hdr','Spck_HdrVr')
*!*	      lMAccount = SUBSTR(MKEY,1,5)
*!*	      =SEEK('P'+PADR(ALLTRIM(lMAccount),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
*!*	      *C200404,1 [End]
*!*	      *&&H Scan Rest While Pack_Id + Color +...
*!*	      *lcPack_Id  = Spck_HDR.Pack_Id
*!*	      *lcPack_Sz  = Spck_HDR.cPckSize
*!*	      *lcPack_Clr = Spck_HDR.cPkColor
*!*	      *lcPack_Ver = Spck_HDR.cPkVersion
*!*	      lMVersion = .F.
*!*	      *BXXX Hassan [Begin]
*!*	      lMAccount = SUBSTR(MKEY,1,5)
*!*	      SCAN REST WHILE  TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion=;
*!*	          'P'+lMAccount+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz
*!*	        *SCAN REST WHILE  TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion= 'P'+Mkey
*!*	        IF lcPack_Vrn <> Spck_HDR.cPkVersion
*!*	          lMVersion = .T.
*!*	          EXIT
*!*	        ENDIF
*!*	      ENDSCAN
*!*	      *wab
*!*	      *=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
*!*	      =SEEK('P'+MKEY,'Spck_Hdr','Spck_HdrVr')
*!*	      *wab
*!*	      IF !lMVersion
*!*	        *C200404,1 HBG 09/03/2002 reject the PO if the qty sent is not evenly
*!*	        *C200404,1                divisible by the pack qty [Begin]
*!*	        IF llReject
*!*	          IF RFLAG
*!*	            =FPUTS(EReport ,'File Code#: '+lcFileCode)
*!*	            =FPUTS(EReport ,'ACCOUNT#: '  + MACCOUNT)
*!*	            =FPUTS(EReport ,'CUST. PO#: ' + ALLTRIM(MCustPo))
*!*	            IIF(!EMPTY(&lcStores..STORE),FPUTS(EReport ,'Store #: ' + ALLTRIM(&lcStores..STORE)),.T.)
*!*	            IIF(!EMPTY(MUPC),FPUTS(EReport ,'UPC : ' + ALLTRIM(MUPC)),.T.)
*!*	            *B606419,1 Hassan [Begin]
*!*	            *IIF(!EMPTY(MSKU),FPUTS(EReport ,'SKU : ' + ALLTRIM(MSKU)),.T.)
*!*	            *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
*!*	            *IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle)),.T.)
*!*	            IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle+MSiz)),.T.)
*!*	            *HBG [End]
*!*	            *B606419,1 Hassan [End  ]
*!*	            =FPUTS(EReport ,'Pack ID : ' + ALLTRIM(MKEY))
*!*	            =FPUTS(EReport ,'Qty Ordered: '+;
*!*	              IIF(EMPTY(&lcStores..STORE),ALLTRIM(STR(MQTY)),ALLTRIM(STR(&lcStores..nQuantity*MSlnQty))))
*!*	            =FPUTS(EReport ,'Pack Qty : ' + ALLTRIM(STR(MPckQty)))
*!*	            =FPUTS(EReport ,'Rejection Reasone : Qty ordered is not divisable by the Pack qty.')
*!*	            RFLAG = .F.
*!*	          ENDIF
*!*	          RETURN
*!*	        ENDIF
*!*	        *C200404,1 [End]
*!*	        *&& Process Order(2)
*!*	      ELSE
*!*	        IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
*!*	          *&&Check If The Conflict Resolve
*!*	          IF !EMPTY(ConflctL.cPkVersion)
*!*	            *C200404,1 HBG 09/03/2002 reject the PO if the qty sent is not evenly
*!*	            *C200404,1                divisible by the pack qty [Begin]
*!*	            IF llReject
*!*	              IF RFLAG
*!*	                =FPUTS(EReport ,'File Code#: '+lcFileCode)
*!*	                =FPUTS(EReport ,'ACCOUNT#: '  + MACCOUNT)
*!*	                =FPUTS(EReport ,'CUST. PO#: ' + ALLTRIM(MCustPo))
*!*	                IIF(!EMPTY(&lcStores..STORE),FPUTS(EReport ,'Store #: ' + ALLTRIM(&lcStores..STORE)),.T.)
*!*	                IIF(!EMPTY(MUPC),FPUTS(EReport ,'UPC : ' + ALLTRIM(MUPC)),.T.)
*!*	                *B606419,1 Hassan [Begin]
*!*	                *IIF(!EMPTY(MSKU),FPUTS(EReport ,'SKU : ' + ALLTRIM(MSKU)),.T.)
*!*	                *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
*!*	                *IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle)),.T.)
*!*	                IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle+MSiz)),.T.)
*!*	                *HBG [End]
*!*	                *B606419,1 Hassan [End  ]
*!*	                =FPUTS(EReport ,'Pack ID : ' + ALLTRIM(MKEY))
*!*	                =FPUTS(EReport ,'Qty Ordered: '+;
*!*	                  IIF(EMPTY(&lcStores..STORE),ALLTRIM(STR(MQTY)),ALLTRIM(STR(&lcStores..nQuantity*MSlnQty))))
*!*	                =FPUTS(EReport ,'Pack Qty : ' + ALLTRIM(STR(MPckQty)))
*!*	                =FPUTS(EReport ,'Rejection Reasone : Qty ordered is not divisable by the Pack qty.')
*!*	                RFLAG = .F.
*!*	              ENDIF
*!*	              RETURN
*!*	            ENDIF
*!*	            *C200404,1 [End]

*!*	            lMVersion = .F.
*!*	            lcPack_Vrn = ConflctL.cPkVersion
*!*	            MKEY = PADR(ALLTRIM(MACCOUNT),5)+ lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
*!*	            *C200479,1 HBG 8/1/2003 Get the value of fields lRange & lPckPrPiec [Begin]
*!*	            =SEEK('P'+MKEY,'Spck_Hdr','Spck_HdrVr')
*!*	            llPack_Rng  = Spck_HDR.lRange
*!*	            llPack_PrPk = Spck_HDR.lPckPrPiec
*!*	            *C200479,1 [End]
*!*	            *&& Move this line to 'ConfLinAr'
*!*	            lcAlias = SELECT()
*!*	            *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[Begin]
*!*	            *!*SELECT ConflctL
*!*	            *!*SCATTER TO MEMVAR MEMO
*!*	            *!*SELECT CnfLinAr
*!*	            *!*APPEND BLANK
*!*	            *!*GATHER FROM MEMVAR
*!*	            *!*SELECT ConflctL
*!*	            *!*DELETE
*!*	            DO CASE
*!*	                *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
*!*	                *Case nCounter = 1
*!*	              CASE ConflctL.nCounter = 1
*!*	                *C200404,1 [End]
*!*	                SELECT ConflctL
*!*	                SCATTER TO MEMVAR MEMO
*!*	                SELECT CnfLinAr
*!*	                APPEND BLANK
*!*	                GATHER FROM MEMVAR
*!*	                SELECT ConflctL
*!*	                DELETE
*!*	                *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
*!*	                *Case nCounter > 1
*!*	                *Replace nCounter With (nCounter -1) in ConflctL
*!*	              CASE ConflctL.nCounter > 1
*!*	                lcPrvAls = ALIAS()
*!*	                SELECT ConflctL
*!*	                REPLACE ConflctL.nCounter WITH (ConflctL.nCounter -1)
*!*	                SELECT (lcPrvAls)
*!*	                *C200404,1 [End]
*!*	            ENDCASE
*!*	            *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[End  ]
*!*	            *&& If This Line Is The Last Line Related with Header,Move Header Line
*!*	            *!*	            *IF !SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
*!*	            IF !SEEK( lcFileCode + PADR(MCustPo,15),'ConflctL','ConflctL')
*!*	            *hbg
*!*	              SELECT ConflctH
*!*	              SCATTER TO MEMVAR MEMO
*!*	              SELECT CnfHdrAr
*!*	              APPEND BLANK
*!*	              GATHER FROM MEMVAR
*!*	              SELECT ConflctH
*!*	              DELETE
*!*	            ENDIF
*!*	            SELECT(lcAlias)
*!*	          ELSE
*!*	            *&& Increase 'nCounter'
*!*	            RFLAG = .F.
*!*	            REPLACE ConflctL.nCounter WITH ConflctL.nCounter + 1
*!*	          ENDIF
*!*	        ELSE
*!*	          *&& Reject This Line
*!*	          *&& Put This Line In The Error Log.
*!*	          *&&H Put The Line In The Conflict File.
*!*	          RFLAG = .F.
*!*	          lcAlias = SELECT()
*!*	          SELECT ConflctH
*!*	          IF !SEEK(lcFileCode+MCustPo,'ConflctH','ConflctH')
*!*	            APPEND BLANK
*!*	            REPLACE cFileCode  WITH lcFileCode ;
*!*	              CustPO     WITH MCustPo;
*!*	              Account    WITH MACCOUNT ;
*!*	              RcveDate   WITH EdiLibDt.dDate;
*!*	              RcveTime   WITH EdiLibDt.cTime;
*!*	              ProcessDat WITH oAriaApplication.SystemDate;
*!*	              ProcessTim WITH gfGetTime();
*!*	              cAdd_User  WITH oAriaApplication.User_ID
*!*	          ENDIF
*!*	          SELECT ConflctL
*!*	          APPEND BLANK
*!*	          *HBG 08/04/2003 Add MSiz To MStyle to Get the all SKU code [Begin]
*!*			  *!* REPLACE cFileCode  WITH lcFileCode ;
*!*			  *!*   cPack      WITH Spck_HDR.Pack_id+'-'+Spck_HDR.cPkColor+'-'+Spck_HDR.cPckSize ;
*!*			  *!*   CustPO     WITH MCustPo;
*!*			  *!*   lRange     WITH Spck_HDR.lRange  ;
*!*			  *!*   cUpc       WITH MUPC;
*!*			  *!*	cSku       WITH MStyle;
*!*			  *!*   cVendrNum  WITH MVend;
*!*			  *!*   nPCKQty    WITH MQTY;
*!*			  *!*   nPkSlPrice WITH MPri;
*!*			  *!*   nCounter   WITH 1;
*!*			  *!*   cAdd_User  WITH oAriaApplication.User_ID
*!*	          REPLACE cFileCode  WITH lcFileCode ;
*!*	            cPack      WITH Spck_HDR.Pack_id+'-'+Spck_HDR.cPkColor+'-'+Spck_HDR.cPckSize ;
*!*	            CustPO     WITH MCustPo;
*!*	            lRange     WITH Spck_HDR.lRange  ;
*!*	            cUpc       WITH MUPC;
*!*	            cSku       WITH MStyle+MSiz;
*!*	            cVendrNum  WITH MVend;
*!*	            nPCKQty    WITH MQTY;
*!*	            nPkSlPrice WITH MPri;
*!*	            nCounter   WITH 1;
*!*	            cAdd_User  WITH oAriaApplication.User_ID
*!*	          *HBG [End]
*!*	          *B607024,1 [Begin]
*!*	          REPLACE nPckNum WITH MPckQty
*!*	          *B607024,1 [End]

*!*	          SELECT(lcAlias)
*!*	          * =FPUTS(lnErorFile ,
*!*	          =FPUTS(EReport , 'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
*!*	            ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id+;
*!*	            ' Pack has multi Version And user should specify pack verion.')
*!*	        ENDIF
*!*	      ENDIF
*!*	      ****************************************************
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDFUN

*:****************************************************************************************
*! Name      : lfChkRng
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/16/2003
*! Purpose   : Check for conflict packs (Range & version)
*:****************************************************************************************
*! Called from : Class 'ProcessPo' Method 'ProcessPo'
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfChkRng

  *Sameh on 11/05/2004 Helpdesk issue # 122865. [Begin]
  lcCurAlias = ALIAS()
  IF !USED(lcTmpError)
    oTmp  = CREATEOBJECT("GetTempName")
    lcTmpError = oTmp.DO()
    CREATE TABLE (oAriaApplication.WorkDir+lcTmpError) (cCode C(240))
    INDEX ON cCode TAG (lcTmpError)
    Release oTmp
  ENDIF
  IF !EMPTY(lcCurAlias)
    SELECT (lcCurAlias)
  ENDIF
  *Sameh on 11/05/2004 Helpdesk issue # 122865. [End]

  lRange_Process = .F.
  cBlkOrd        = ""
  IF Mord = 3
    lMVersion = .F.
    =SEEK('P'+MKEY ,'Spck_Hdr','Spck_HdrVr')
    IF !Spck_HDR.lRange
      SELECT Spck_HDR
      lMAccount = SUBSTR(MKEY,1,5)
      =SEEK('P'+PADR(ALLTRIM(lMAccount),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
      lMVersion = .F.
      lMAccount = SUBSTR(MKEY,1,5)
      SCAN REST WHILE  TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion=;
          'P'+lMAccount+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz
        IF lcPack_Vrn <> Spck_HDR.cPkVersion
          lMVersion = .T.
          EXIT
        ENDIF
      ENDSCAN
      =SEEK('P'+MKEY,'Spck_Hdr','Spck_HdrVr')
      IF !lMVersion
        IF llReject
          IF RFLAG
            =FPUTS(EReport ,'File Code#: '+lcFileCode)
            =FPUTS(EReport ,'ACCOUNT#: '  + MACCOUNT)
            =FPUTS(EReport ,'CUST. PO#: ' + ALLTRIM(MCustPo))
            IIF(!EMPTY(&lcStores..STORE),FPUTS(EReport ,'Store #: ' + ALLTRIM(&lcStores..STORE)),.T.)
            IIF(!EMPTY(MUPC),FPUTS(EReport ,'UPC : ' + ALLTRIM(MUPC)),.T.)
            IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle+MSiz)),.T.)
            =FPUTS(EReport ,'Pack ID : ' + ALLTRIM(MKEY))
            =FPUTS(EReport ,'Qty Ordered: '+;
              IIF(EMPTY(&lcStores..STORE),ALLTRIM(STR(MQTY)),ALLTRIM(STR(&lcStores..nQuantity*MSlnQty))))
            =FPUTS(EReport ,'Pack Qty : ' + ALLTRIM(STR(MPckQty)))
            =FPUTS(EReport ,'Rejection Reasone : Qty ordered is not divisable by the Pack qty.')
            RFLAG = .F.
          ENDIF
          RETURN
        ENDIF
      ENDIF
    ENDIF
    IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
      llCheck = IIF(Spck_HDR.lRange,!EMPTY(ConflctL.cBlkOrd),!EMPTY(ConflctL.cPkVersion))
      IF llCheck
        IF llReject
          IF RFLAG
            =FPUTS(EReport ,'File Code#: '+lcFileCode)
            =FPUTS(EReport ,'ACCOUNT#: '  + MACCOUNT)
            =FPUTS(EReport ,'CUST. PO#: ' + ALLTRIM(MCustPo))
            IIF(!EMPTY(&lcStores..STORE),FPUTS(EReport ,'Store #: ' + ALLTRIM(&lcStores..STORE)),.T.)
            IIF(!EMPTY(MUPC),FPUTS(EReport ,'UPC : ' + ALLTRIM(MUPC)),.T.)
            IIF(!EMPTY(MStyle),FPUTS(EReport ,'SKU : ' + ALLTRIM(MStyle+MSiz)),.T.)
            =FPUTS(EReport ,'Pack ID : ' + ALLTRIM(MKEY))
            =FPUTS(EReport ,'Qty Ordered: '+;
              IIF(EMPTY(&lcStores..STORE),ALLTRIM(STR(MQTY)),ALLTRIM(STR(&lcStores..nQuantity*MSlnQty))))
            =FPUTS(EReport ,'Pack Qty : ' + ALLTRIM(STR(MPckQty)))
            =FPUTS(EReport ,'Rejection Reasone : Qty ordered is not divisable by the Pack qty.')
            RFLAG = .F.
          ENDIF
          RETURN
        ENDIF
        lcAlias = SELECT()
        SELECT ConflctL
        IF Spck_HDR.lRange
          lRange_Process = .T.
          cBlkOrd = ConflctL.cBlkOrd
        ELSE
          lMVersion = .F.
          lcPack_Vrn = ConflctL.cPkVersion
          MKEY = PADR(ALLTRIM(MACCOUNT),5)+ lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
          =SEEK('P'+MKEY,'Spck_Hdr','Spck_HdrVr')
          llPack_Rng  = Spck_HDR.lRange
          llPack_PrPk = Spck_HDR.lPckPrPiec
        ENDIF
        SELECT(lcAlias)
        REPLACE ConflctL.nCounter WITH 0
      ELSE
        RFLAG = .F.

        *Sameh on 11/05/2004 Helpdesk issue # 122865. [Begin]
        *IF ConflctL.nCounter = 1 AND !llAddedNew
        IF !SEEK('File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id,(lcTmpError))
          INSERT INTO (lcTmpError) (cCode) VALUES ("File Code#: "+lcFileCode+ ", ACCOUNT#: " + MACCOUNT+" CUST. PO#: " + ALLTRIM(MCustPo)+", Pack Id: "+Spck_HDR.Pack_id)
          *Sameh on 11/05/2004 Helpdesk issue # 122865. [End]

          REPLACE ConflctL.nCounter WITH ConflctL.nCounter + 1
          llAddedNew = .F.
          IF Spck_HDR.lRange
            =FPUTS(EReport ,'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
              ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id+;
              ' Need to get bulk order to deplet from.')
          ELSE
            =FPUTS(EReport , 'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
              ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id+;
              ' Pack has multi Version And user should specify pack verion.')
          ENDIF
        ENDIF
      ENDIF
    ELSE
      IF Spck_HDR.lRange OR lMVersion
        RFLAG = .F.
        lcAlias = SELECT()
        SELECT ConflctH
        IF !SEEK(lcFileCode+MCustPo,'ConflctH','ConflctH')
          APPEND BLANK
          REPLACE cFileCode  WITH lcFileCode ;
            CustPO     WITH MCustPo;
            Account    WITH MACCOUNT ;
            RcveDate   WITH EDILIBDT.dDate;
            RcveTime   WITH EDILIBDT.cTime;
            ProcessDat WITH oAriaApplication.SystemDate;
            ProcessTim WITH gfGetTime();
            cAdd_User  WITH oAriaApplication.User_ID
        ENDIF
        SELECT ConflctL
        APPEND BLANK
        REPLACE cFileCode  WITH lcFileCode ;
          cPack      WITH Spck_HDR.Pack_id+'-'+Spck_HDR.cPkColor+'-'+Spck_HDR.cPckSize ;
          CustPO     WITH MCustPo;
          lRange     WITH Spck_HDR.lRange  ;
          cUpc       WITH MUPC;
          cSku       WITH MStyle+MSiz;
          cVendrNum  WITH MVend;
          nPCKQty    WITH MQTY;
          nPkSlPrice WITH MPri;
          cPkVersion WITH IIF(Spck_HDR.lRange,Spck_HDR.cPkVersion,"");
          nCounter   WITH 1;
          cAdd_User  WITH oAriaApplication.User_ID
        REPLACE nPckNum WITH MPckQty
        llAddedNew = .T.
        SELECT(lcAlias)

        *Sameh on 11/05/2004 Helpdesk issue # 122865. [Begin]
        IF !SEEK('File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id,(lcTmpError))
          INSERT INTO (lcTmpError) (cCode) VALUES ("File Code#: "+lcFileCode+ ", ACCOUNT#: " + MACCOUNT+" CUST. PO#: " + ALLTRIM(MCustPo)+", Pack Id: "+Spck_HDR.Pack_id)
        ENDIF
        *Sameh on 11/05/2004 Helpdesk issue # 122865. [End]

        IF Spck_HDR.lRange
          =FPUTS(EReport ,'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
            ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id+;
            ' Need to get bulk order to deplet from.')
        ELSE
          =FPUTS(EReport , 'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
            ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_id+;
            ' Pack has multi Version And user should specify pack verion.')
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDFUNC

*:****************************************************************************************
*! Name      : lfGmaInv
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/07/2002
*! Purpose   : Send Custom GMA Invoice.
*:****************************************************************************************
*! Called from : Class 'SEND810' Method 'DO'
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
*--C200215,5
FUNCTION lfGmaInv
  PRIVATE     lcTmpInvL , llWriteDoc

  oGetMemVar     = CREATEOBJECT("GetMemVar")
  oGetRltdFld    = CREATEOBJECT("GetRelatedFields")
  oField2Seg     = CREATEOBJECT("FIELD2SEG")
  oGetCheckDigit = CREATEOBJECT("GetCheckDigit")
  *B122766,1 The varible lcMANUFID not found  06/09/2004 [Begin]
  *MVEND= oGetMemVar.DO("XMANUFID",OAriaApplication.ActiveCompanyId)
  lcMANUFID = oGetMemVar.DO("XMANUFID",oAriaApplication.ActiveCompanyId)
  *B122766,1 The varible lcMANUFID not found  06/09/2004 [End]
  llUPCUse  = oGetMemVar.DO("M_UPC_USE",oAriaApplication.ActiveCompanyId)='Y'
  MTAXREFE  = oGetMemVar.DO("M_TAX_REFE",oAriaApplication.ActiveCompanyId)
  MDUNS     = oGetMemVar.DO("XDUNS",oAriaApplication.ActiveCompanyId)
  XDUNS = MDUNS
  lcGenColor= PADR(oGetMemVar.DO("MCLRASSCOD",oAriaApplication.ActiveCompanyId),6)
  lcTmpInvL = oGetTempName.DO()
  *** EDDIE 11/5/2003 [begin]
  DO WHILE FILE(oAriaApplication.WorkDir+lcTmpInvL + '.DBF')
    lcTmpInvL = oGetTempName.DO()
  ENDDO
  *** EDDIE 11/5/2003 [end]
  *E130163,1 WLD Commented Out ,Speed up collecting data [Begin]
  *!* lcOrdSum  = oGetTempName.DO()
  *E130163,1 WLD Commented Out ,Speed up collecting data [End]

  Release oGetMemVar
  WAIT 'Sending Invoice# '+lcInv WINDOW NOWAIT
  *HBG Add "nPackNo" field to the temp file fix bug of not getting number of Packs from INVLINE.nPAckNo [Begin]
  *CREATE TABLE (oAriaApplication.WorkDir+lcTmpInvL) ;
  *(ORDER C(6),Account C(5),STYLE C(19),Pack_id C(16),cPkColor C(6),cPckSize C(3),cPkVersion C(4), LINENO N(6),PRICE N(9,2), SCALE C(3), Edi_Pk_amt N(12,10),;
  *Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
  *Qty8 N(6),TotQTY N(7) ,Ord1 N(6),Ord2 N(6),Ord3 N(6),Ord4 N(6),Ord5 N(6),;
  *Ord6 N(6),Ord7 N(6),Ord8 N(6),TotOrd N(7),PrePak C(1),nSugRetPri N(8,2),nSugRetAmt N(12,10),nPkSlPrice N(8,2))
  *C200479,1 HBG 8/1/2003 Add fields lRange & lPckPrPiec [Begin]
  *CREATE TABLE (oAriaApplication.WorkDir+lcTmpInvL) ;
  *(ORDER C(6),Account C(5),STYLE C(19),Pack_id C(16),cPkColor C(6),cPckSize C(3),cPkVersion C(4), LINENO N(6),PRICE N(9,2), SCALE C(3), Edi_Pk_amt N(12,10),;
  *Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
  *Qty8 N(6),TotQTY N(7) ,Ord1 N(6),Ord2 N(6),Ord3 N(6),Ord4 N(6),Ord5 N(6),;
  *Ord6 N(6),Ord7 N(6),Ord8 N(6),TotOrd N(7),PrePak C(1),nSugRetPri N(8,2),nSugRetAmt N(12,10),nPkSlPrice N(8,2),nPackNo N(6))
  CREATE TABLE (oAriaApplication.WorkDir+lcTmpInvL) ;
    (ORDER C(6),Account C(5),STYLE C(19),Pack_id C(16),cPkColor C(6),cPckSize C(3),cPkVersion C(4), LINENO N(6),PRICE N(9,2), SCALE C(3), Edi_Pk_amt N(12,10),;
    Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
    Qty8 N(6),TotQTY N(7) ,Ord1 N(6),Ord2 N(6),Ord3 N(6),Ord4 N(6),Ord5 N(6),;
    Ord6 N(6),Ord7 N(6),Ord8 N(6),TotOrd N(7),PrePak C(1),nSugRetPri N(8,2),;
    nSugRetAmt N(12,2),nPkSlPrice N(8,2),nPackNo N(6),lRange L(1),lPckPrPiec L(1))
  *C200479,1 [End]
  *HBG [END]

  INDEX ON Pack_id+cPkColor+cPckSize+cPkVersion+STYLE TAG (lcTmpInvL)
  *E130163,1 WLD Send consolidated invoices
  lcTmpStores = oGetTempName.DO()
  CREATE TABLE (oAriaApplication.WorkDir+lcTmpStores) ;
    (STYLE C(19), Pack_id C(16),cPkColor C(6),cPckSize C(3),cPkVersion C(4), STORE C(8), Qty1 N(6), Qty2 N(6), Qty3 N(6), Qty4 N(6),;
    Qty5 N(6), Qty6 N(6), Qty7 N(6), Qty8 N(6), TotQTY N(7))
  INDEX ON Pack_id+cPkColor+cPckSize+cPkVersion+STYLE+STORE TAG (lcTmpStores)
  *E130163,1 WLD (End)

  MINV_SEQ = lnTran
  XInvoice = LEFT(lcInv,6)
  DECLARE laTRltFld[5,2]
  STORE '' TO MEOM
  STORE 0  TO MTERMS_DAY,MTERMS_DDAY,MTERMS_DSC,lnEOMDay
  laTRltFld[1,1] = 'EOM'
  laTRltFld[1,2] = 'MEOM'
  laTRltFld[2,1] = 'NTERDUED'
  laTRltFld[2,2] = 'MTERMS_DAY'
  laTRltFld[3,1] = 'NTERDISCD'
  laTRltFld[3,2] = 'MTERMS_DDAY'
  laTRltFld[4,1] = 'NTERDISCR'
  laTRltFld[4,2] = 'MTERMS_DSC'
  laTRltFld[5,1] = 'EOMDAY'
  laTRltFld[5,2] = 'lnEomDay'    && End Of Month Day
  DECLARE laDRltFld[1,2]
  laDRltFld[1,1] = 'DIVLNAME'
  laDRltFld[1,2] = 'MDIVDISC'
  MDIVDISC = ''
  *-- Check if document has been written successfuly
  llWriteDoc = .T.
  lcPartner = ALLTRIM(EdiAcPrt.cPartner)
  MShpTODuns= ALLTRIM(EdiAcPrt.Duns)
  MFOB      = EdiAcPrt.cEdiFOB
  MREMIT    = EdiAcPrt.cRemit
  MUSERID   = ALLTRIM(EdiAcPrt.cCmpISAId)
  llInvUnShip = EdiAcPrt.lInvUnShip
  *C200592,1 HBG 17/08/2003 Get the value of the flag Send using Assortment Pack [Begin]
  =SEEK(EdiAcPrt.cPartCode,'EDIPH','PARTNER')
  *B120999,1 HBG 02/17/2004 Fix bug of sending UOM as AS even if there is no pack detai [Begin]
  *MUOM = IIF(EdiPh.cSnPkD810 = 'Y',"AS","EA")
  llPckDtl = (EdiPh.cSnPkD810 = 'Y')
  *B120999,1 [End]
  *C200592,1 [End]

  =SEEK(lcPartner,'SYCFACT')
  XTIME     = XTIME()
  STORE 0 TO MCTT
  IF !SEEK(XInvoice,'InvHdr') OR !SEEK(XInvoice,'Invline')
    RETURN(.F.)
  ENDIF
  =SEEK(InvHdr.cFacCode,'SYCFACT')
  lcFactor   = InvHdr.cFacCode
  MFACTNAME  = ALLTRIM(SYCFACT.cFacComp)
  MFACTADDR1 = ALLTRIM(SYCFACT.cAddress1)
  MFACTADDR2 = ALLTRIM(SYCFACT.cAddress2)
  MFACTCITY  = ALLTRIM(SYCFACT.cAddress3)
  MFACTSTATE = ALLTRIM(SYCFACT.cAddress4)
  MFACTZIP   = ALLTRIM(SYCFACT.cAddress5)
  =SEEK(InvHdr.cWareCode,'WareHous')
  MWareCode = InvHdr.cWareCode
  MShipName = ALLTRIM(WareHous.cDesc)
  MAddress1 = ALLTRIM(WareHous.cAddress1)
  MAddress2 = ALLTRIM(WareHous.cAddress2)
  MCity     = ALLTRIM(WareHous.cAddress3)
  MState    = ALLTRIM(WareHous.cAddress4)
  MZip      = ALLTRIM(WareHous.cAddress5)
  XACCOUNT  = InvHdr.Account
  MORDER    = InvHdr.ORDER
  MINVDTE   = InvHdr.INVDATE
  MCustPo   = InvHdr.CustPO
  MDEPT     = InvHdr.DEPT

  *C200566,1 Consolidated Invoice case to DC. [Begin]
  *MSTORE    = InvHdr.STORE
  *HBG 12/08/2003 Change the check to make a safe code [Begin]
  *IF InvHdr.Consol = 'Y'
  *  IF SEEK(InvHdr.Invoice,'ConsInvH')
  *    MSTORE = ConsInvH.STORE
  *  ENDIF
  IF InvHdr.CONSOL = 'Y' AND SEEK(InvHdr.Invoice,'ConsInvH')
    MSTORE = ConsInvH.STORE
    *HBG [End]
  ELSE
    MSTORE = InvHdr.STORE
  ENDIF
  *C200566,1 Consolidated Invoice case to DC. [End]

  MSHIPDATE = InvHdr.ShipDate
  MSHIPQTY  = InvHdr.SHIP
  MWEIGHT   = InvHdr.WEIGHT
  MDUEDATE  = InvHdr.DUEDATE
  MCARTONS  = InvHdr.CARTONS
  MCONSOL   = InvHdr.CONSOL
  MPIKTKT   = InvHdr.PIKTKT
  XMERDISC  = InvHdr.DiscPcnt
  MTRDEDISC = InvHdr.Trde_Disc
  MBOL      = InvHdr.BOL_NO
  XSHIPAMT  = InvHdr.SHIPAMT
  XTOTALCHG = InvHdr.TOTALCHG
  MFREIGHT  = InvHdr.FREIGHT+InvHdr.COD+InvHdr.INSUR+InvHdr.nCharges
  xCod      = InvHdr.COD
  xTax      = InvHdr.Tax_Amt+InvHdr.nPstAmt
  xGSTTax   = InvHdr.Tax_Amt
  xInsur    = InvHdr.INSUR
  xDisc     = InvHdr.Discount
  xFreight  = InvHdr.FREIGHT
  xCharge   = MFREIGHT + xTax
  XCARTON   = InvHdr.CARTONS
  MPURPOSE  = IIF(EMPTY(InvHdr.dSentDate),'00','07')
  MCARRIER  = ''
  MCACODE   = ''
  MLOGFAC   = ''

  *B122766,1 SSE 05/25/2004 Add the fix for B607282,1 from 810 class. [Begin]
  *IF 'AS' $ oAriaApplication.CompanyInstalledModules
  *  IF EMPTY(MBOL)
  *    SELECT Pack_Hdr
  *    =SEEK(MORDER+MSTORE)
  *    SCAN REST WHILE ORDER+STORE+PACK_NO = MORDER+MSTORE
  *      MBOL = Pack_Hdr.Bill_Ladg
  *    ENDSCAN
  *  ENDIF
  *  =SEEK(MBOL,'BOL_HDR')

  *  MUCB = PADL(ALLTRIM(MVend) , 7 , '0') + PADL(ALLTRIM(MBOL) , 9 , '0')
  *C200579,1 HBG 29/06/2003 Change the calculation of the check digit added to UCB field[End]
  *MUCB = MUCB + oGetCheckDigit.DO(MUCB)
  *  =lfChkDigit()
  *  *C200579,1 [End]
  *  RELEASE oGetCheckDigit
  *  MRMT_COD = Bol_Hdr.RMT_COD
  *  IF EMPTY(Bol_Hdr.ShipVia)
  *    MCARRIER = Bol_Hdr.Carrier
  *  ELSE
  *    =SEEK('N'+Bol_Hdr.ShipVia+'N'+'SHIPVIA','CODES','CODES')
  *    MCARRIER = CODES.cDiscRep
  *  ENDIF
  *  MCACODE = Bol_Hdr.CarrierCod
  *  MLOGFAC = Bol_Hdr.STORE
  *ENDIF

  MUCB      = ''
  MRMT_COD  = ''
  MTRANMTHD = ''

  IF 'AS' $ oAriaApplication.CompanyInstalledModules
    IF EMPTY(MBOL)
      IF InvHdr.CONSOL='Y'
        IF SEEK(ConsInvH.ORDER+ConsInvH.STORE,'Pack_Hdr','ORDERPCK') AND SEEK(ConsInvH.ORDER+Pack_Hdr.PACK_NO,"BOL_LIN","ORD_BOL")
          MBOL = Bol_lin.BOL_NO
        ENDIF
      ELSE
        IF SEEK(MORDER+MSTORE,'Pack_Hdr') AND SEEK(MORDER+Pack_Hdr.PACK_NO,"BOL_LIN","ORD_BOL")
          MBOL = Bol_lin.BOL_NO
        ENDIF
      ENDIF
    ENDIF
    IF SEEK(MBOL,'BOL_HDR')
      *B607362,1 Use new varible instead of mvend  Hassan 06/18/2003 [Begin]
      *MUCB = PADL(ALLTRIM(MVEND) , 7 , '0') + PADL(ALLTRIM(MBOL) , 9 , '0')
      MUCB = PADL(ALLTRIM(lcMANUFID) , 7 , '0') + PADL(ALLTRIM(MBOL) , 9 , '0')
      *B607362,1 Use new varible instead of mvend  Hassan 06/18/2003 [End  ]

      *B607474,1 WAM Wrong calculation of check digit
      *MUCB = MUCB + oGetCheckDigit.DO(MUCB)
      *MUCB = MUCB + oGetCheckDigit.DO(MUCB,'E')
      =lfChkDigit()
      *B607474,1 WAM (End)

      MRMT_COD = Bol_Hdr.RMT_COD
      IF EMPTY(Bol_Hdr.ShipVia)
        MCARRIER = Bol_Hdr.Carrier
      ELSE
        =SEEK('N'+Bol_Hdr.ShipVia+'N'+'SHIPVIA','CODES','CODES')
        MCARRIER = CODES.cDiscRep
      ENDIF
      MTRANMTHD  = Bol_Hdr.cTranMthd
      MCACODE = Bol_Hdr.CarrierCod
      MLOGFAC = Bol_Hdr.STORE
    ENDIF
    *B607282,1 Initialize varibles HIA 05/20/2003[End  ]
  ENDIF
  *B122766,1 SSE 05/25/2004 Add the fix for B607282,1 from 810 class. [End]

  =SEEK('M'+XACCOUNT,'Customer')
  MFACTACCT = Customer.FACTACCT
  MFACTOR   = Customer.cFacCode
  MVENDNO   = Customer.cCusVend

  =SEEK(IIF(EMPTY(MSTORE),'M'+XACCOUNT,'S'+XACCOUNT+MSTORE),'Customer')
  MTELE    = ALLTRIM(Customer.PHONE1)
  MBTNAME  = ALLTRIM(Customer.BTNAME)
  MBTADDR1 = ALLTRIM(Customer.cAddress12)
  MBTADDR2 = ALLTRIM(Customer.cAddress22)
  MBTCITY  = ALLTRIM(Customer.cAddress32)
  MBTSTATE = ALLTRIM(Customer.cAddress42)
  MBTZIP   = ALLTRIM(Customer.cAddress52)
  MSTNAME  = ALLTRIM(Customer.STNAME)
  MSTADDR  = ALLTRIM(Customer.cAddress1)
  MSTADDR2 = ALLTRIM(Customer.cAddress2)
  MSTCITY  = ALLTRIM(Customer.cAddress3)
  MSTSTATE = ALLTRIM(Customer.cAddress4)
  MSTZIP   = ALLTRIM(Customer.cAddress5)
  *-- Check Stores GLN
  MSTOREGLN= ALLTRIM(Customer.cStoreGln)
  =SEEK('O'+MORDER,'OrdHdr')
  MVend    = IIF(EMPTY(ORDHDR.INT_VEND),MVENDNO,ORDHDR.INT_VEND)
  MVENDNO  = MVend
  MDIV     = ORDHDR.cDIVISION
  =oGetRltdFld.DO(ORDHDR.cDIVISION,@laDRltFld,'CDIVISION ')
  MEVENT   = ORDHDR.EVENT_COD
  MBILL    = ORDHDR.BILLNO
  MENTERED = ORDHDR.ENTERED
  MCLASS   = ORDHDR.cCLASS
  MBLANK_ORD = ORDHDR.BLANK_ORD
  MMERC_TYPE = ORDHDR.MERC_TYPE

  IF EMPTY(MLOGFAC)
    MLOGFAC = IIF(EMPTY(ORDHDR.DISTRB_NO),;
      IIF(InvHdr.CONSOL='Y',lfGetLogFac(XACCOUNT,XInvoice),;
      Customer.Dist_Ctr),ORDHDR.DISTRB_NO)
  ENDIF
  MLOGFAC = IIF(EMPTY(MLOGFAC),MSTORE,MLOGFAC)
  =SEEK('S'+XACCOUNT+MLOGFAC,'Customer')
  MDCNAME  = ALLTRIM(Customer.STNAME)
  MDCADDR1 = ALLTRIM(Customer.cAddress1)
  MDCADDR2 = ALLTRIM(Customer.cAddress2)
  MDCCITY  = ALLTRIM(Customer.cAddress3)
  MDCSTATE = ALLTRIM(Customer.cAddress4)
  MDCZIP   = ALLTRIM(Customer.cAddress5)
  *-- Check Stores GLN
  MLOGFACGLN= ALLTRIM(Customer.cStoreGln)
  MTERMCODE = InvHdr.CTERMCODE
  =oGetRltdFld.DO(InvHdr.CTERMCODE,@laTRltFld,'CTERMCODE')
  *-- Get EOM Days
  lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
  =SEEK('N' + InvHdr.CTERMCODE + 'N' + 'CTERMCODE' , 'CODES' , 'CODES')
  MTERMS_DAT  = CODES.cDiscRep
  MTERMS_TYP  = IIF(MTERMS_DSC=0,'05','01')
  MTERMS_DDUE = 0
  IF MEOM='Y'
    lnDays = IIF(DAY(InvHdr.INVDATE) > lnEOMDay,GOMONTH(InvHdr.INVDATE,2),GOMONTH(InvHdr.INVDATE,1))
    MTERMS_DDAY = lnDays - InvHdr.INVDATE - DAY(InvHdr.INVDATE)+10+MTERMS_DAY
    MTERMS_TYP  = IIF(MTERMS_DSC=0,'05',IIF('8/10' $ MTERMS_DAT,'12','02'))
    MTERMS_DAY  = MTERMS_DDAY
  ENDIF
  =SEEK('N'+InvHdr.ShipVia+'N'+'SHIPVIA','CODES','CODES')
  MSHIPVIA = CODES.cDiscRep
  MSHIPVIA = IIF('UPS' $ MSHIPVIA,'UPS',MSHIPVIA)
  llWriteDoc = IIF(oField2Seg.DO(lnFilePoint,'03','SycEdiFS',lcMapS),llWriteDoc,.F.)

  *E130163,1 WLD Commented Out ,Speed up collecting data [Begin]
  *!*	  *HBG 12/08/2003 Change the check to make a safe code [Begin]
  *!*	  *IF MCONSOL = 'Y'
  *!*	  *B120476 TMI [START] Add quotations to seeked file name
  *!*	  *IF MCONSOL = 'Y' AND SEEK(lcInv,CONSINVH)
  *!*	  IF MCONSOL = 'Y' AND SEEK(lcInv,'CONSINVH')
  *!*	  *B120476 TMI [END  ] Add quotations to seeked file name
  *!*	  *HBG [End]
  *!*	    SELECT 'O'+CONSINVH.ORDER FROM CONSINVH WHERE CONSINVH.INVOICE = lcInv INTO ARRAY laOrders
  *!*	    SELECT ORDER,STORE,STYLE,SUM(Book1) AS nBook1,SUM(Book2) AS nBook2,SUM(Book3) AS nBook3,SUM(Book4) AS nBook4,;
  *!*	      SUM(Book5) AS nBook5,SUM(Book6) AS nBook6,SUM(Book7) AS nBook7,SUM(Book8) AS nBook8,SUM(TotBook) AS nTotBook ;
  *!*	      FROM OrdLine WHERE ASCAN(laOrders,cOrdType+ORDER)<>0 GROUP BY STYLE INTO CURSOR (lcOrdSum)
  *!*	    SELECT (lcOrdSum)
  *!*	    SCAN
  *!*	      IF !SEEK(STYLE+XInvoice,'INVLINE','Invlines') AND !llInvUnShip
  *!*	        LOOP
  *!*	      ENDIF
  *!*	      lcKey = INVLINE.INVOICE+STORE+ORDER
  *!*	      =SEEK(lcKey,'ConsInvH')
  *!*	      SELECT INVLINE
  *!*	      SCATTER MEMVAR
  *!*	      *B127392 AAH DEFINE SUGEST RETEIL PRICE VARIABLE 04/13/2005 [BEGIN]
  *!*	      =SEEK(m.Style,'Style')
  *!*	      m.nSugRetPri = IIF(OrdLine.nSugRetPri=0,STYLE.nSugRetPri,OrdLine.nSugRetPri)
  *!*	      *B127392 AAH  04/13/2005 [END]
  *!*	      m.Order = CONSINVH.ORDER
  *!*	      IF EMPTY(m.Pack_id)
  *!*	        lcGenClrSty = SUBSTR(m.Style,1,lnClrPos-1)+lcGenColor+SUBSTR(m.Style,lnClrPos+lnCLrLen)
  *!*	        IF llUPCUse .AND. SEEK(lcGenClrSty,'Style') .AND. ;
  *!*	            !SEEK(m.Style,'StyleUpc') AND SEEK(lcGenClrSty,'StyleUpc')
  *!*	          IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+lcGenClrSty,lcTmpInvL)
  *!*	            m.Edi_Pk_amt = m.TotQTY*m.PRICE
  *!*	            m.Style = lcGenClrSty
  *!*	            INSERT INTO (lcTmpInvL) FROM MEMVAR
  *!*	          ELSE
  *!*	            REPLACE Qty1       WITH Qty1 + m.Qty1 ,;
  *!*	                    Qty2       WITH Qty2 + m.Qty2 ,;
  *!*	                    Qty3       WITH Qty3 + m.Qty3 ,;
  *!*	                    Qty4       WITH Qty4 + m.Qty4 ,;
  *!*	                    Qty5       WITH Qty5 + m.Qty5 ,;
  *!*	                    Qty6       WITH Qty6 + m.Qty6 ,;
  *!*	                    Qty7       WITH Qty7 + m.Qty7 ,;
  *!*	                    Qty8       WITH Qty8 + m.Qty8 ,;
  *!*	                    TotQTY     WITH TotQTY+m.TotQTY      ,;
  *!*	                    Edi_Pk_amt WITH Edi_Pk_amt + m.TotQTY*m.PRICE ,;
  *!*	                    PRICE      WITH Edi_Pk_amt/TotQTY IN (lcTmpInvL)
  *!*	          ENDIF
  *!*	        ELSE
  *!*	          *E124849,1 WAM 11/07/2004 consolidate same style with different configurations
  *!*	          *INSERT INTO (lcTmpInvL) FROM MEMVAR
  *!*	          IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+m.Style,lcTmpInvL)
  *!*	            m.Edi_Pk_amt = m.TotQTY*m.PRICE
  *!*	            m.nSugRetAmt = m.TotQTY*m.nSugRetPri
  *!*	            INSERT INTO (lcTmpInvL) FROM MEMVAR
  *!*	          ELSE
  *!*	            REPLACE Qty1       WITH Qty1 + m.Qty1 ,;
  *!*	                    Qty2       WITH Qty2 + m.Qty2 ,;
  *!*	                    Qty3       WITH Qty3 + m.Qty3 ,;
  *!*	                    Qty4       WITH Qty4 + m.Qty4 ,;
  *!*	                    Qty5       WITH Qty5 + m.Qty5 ,;
  *!*	                    Qty6       WITH Qty6 + m.Qty6 ,;
  *!*	                    Qty7       WITH Qty7 + m.Qty7 ,;
  *!*	                    Qty8       WITH Qty8 + m.Qty8 ,;
  *!*	                    TotQTY     WITH TotQTY+m.TotQTY      ,;
  *!*	                    Edi_Pk_amt WITH Edi_Pk_amt + m.TotQTY*m.PRICE ,;
  *!*	                    nSugRetAmt WITH nSugRetAmt + m.TotQTY*m.nSugRetPri ,;
  *!*	                    PRICE      WITH Edi_Pk_amt/TotQTY ,;
  *!*	                    nSugRetPri WITH nSugRetAmt/TotQTY IN (lcTmpInvL)
  *!*	          ENDIF
  *!*	          *E124849,1 WAM 11/07/2004 (End)
  *!*	        ENDIF
  *!*	      ELSE
  *!*	        IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+SPACE(19),lcTmpInvL)
  *!*	          m.Style = ''
  *!*	          m.Edi_Pk_amt = m.TotQTY*m.PRICE
  *!*	          INSERT INTO (lcTmpInvL) FROM MEMVAR
  *!*	          REPLACE &lcTmpInvL..PRICE  WITH INVLINE.nPkSlPrice
  *!*	        ELSE
  *!*	          REPLACE Qty1       WITH Qty1+m.Qty1,;
  *!*	                  Qty2       WITH Qty2+m.Qty2,;
  *!*	                  Qty3       WITH Qty3+m.Qty3,;
  *!*	                  Qty4       WITH Qty4+m.Qty4,;
  *!*	                  Qty5       WITH Qty5+m.Qty5,;
  *!*	                  Qty6       WITH Qty6+m.Qty6,;
  *!*	                  Qty7       WITH Qty7+m.Qty7,;
  *!*	                  Qty8       WITH Qty8+m.Qty8,;
  *!*	                  TotQTY     WITH TotQTY+m.TotQTY,;
  *!*	                  Edi_Pk_amt WITH Edi_Pk_amt+(m.TotQTY*m.PRICE) IN (lcTmpInvL)
  *!*	          REPLACE &lcTmpInvL..PRICE WITH &lcTmpInvL..PRICE +(m.TotQTY*m.PRICE)
  *!*	        ENDIF
  *!*	      ENDIF
  *!*	      IF llInvUnShip
  *!*	        REPLACE Ord1   WITH Ord1 + &lcOrdSum..nBook1 ,;
  *!*	          Ord2   WITH Ord2 + &lcOrdSum..nBook2 ,;
  *!*	          Ord3   WITH Ord3 + &lcOrdSum..nBook3 ,;
  *!*	          Ord4   WITH Ord4 + &lcOrdSum..nBook4 ,;
  *!*	          Ord5   WITH Ord5 + &lcOrdSum..nBook5 ,;
  *!*	          Ord6   WITH Ord6 + &lcOrdSum..nBook6 ,;
  *!*	          Ord7   WITH Ord7 + &lcOrdSum..nBook7 ,;
  *!*	          Ord8   WITH Ord8 + &lcOrdSum..nBook8 ,;
  *!*	          TotOrd WITH TotOrd + &lcOrdSum..nTotBook IN (lcTmpInvL)
  *!*	      ENDIF
  *!*	    ENDSCAN
  *!*	    USE IN (lcOrdSum)
  IF MCONSOL = 'Y'
    SET ORDER TO TAG ORDLINST IN OrdLine
    SELECT ConsInvH
    =SEEK(XInvoice)
    SCAN REST WHILE Invoice+STORE+ORDER = XInvoice
      SELECT OrdLine
      =SEEK('O'+ConsInvH.ORDER+ConsInvH.STORE)
      SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+ConsInvH.ORDER+ConsInvH.STORE
        IF SEEK(ORDER+STR(LINENO,6)+XInvoice,'Consinvl','CINVLINE')
          SELECT Consinvl
          SCATTER MEMVAR
        ELSE
          IF llInvUnShip
            SCATTER MEMVAR
            STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQTY,m.PRICE
          ELSE
            LOOP
          ENDIF
        ENDIF
        SELECT Consinvl
        =SEEK(m.Style,'Style')
        m.nSugRetPri = IIF(OrdLine.nSugRetPri=0,STYLE.nSugRetPri,OrdLine.nSugRetPri)

        IF EMPTY(m.Pack_id)
          m.Pack_id = SPACE(16)
          lcGenClrSty = SUBSTR(m.Style,1,lnClrPos-1)+lcGenColor+SUBSTR(m.Style,lnClrPos+lnCLrLen)
          IF llUPCUse .AND. SEEK(lcGenClrSty,'Style') .AND. ;
              !SEEK(m.Style,'StyleUpc') AND SEEK(lcGenClrSty,'StyleUpc')
            m.Style = lcGenClrSty
            IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+lcGenClrSty,lcTmpInvL)
              m.Edi_Pk_amt = m.TotQTY*m.PRICE
              m.Style = lcGenClrSty
              INSERT INTO (lcTmpInvL) FROM MEMVAR
            ELSE
              REPLACE Qty1       WITH Qty1 + m.Qty1 ,;
                Qty2       WITH Qty2 + m.Qty2 ,;
                Qty3       WITH Qty3 + m.Qty3 ,;
                Qty4       WITH Qty4 + m.Qty4 ,;
                Qty5       WITH Qty5 + m.Qty5 ,;
                Qty6       WITH Qty6 + m.Qty6 ,;
                Qty7       WITH Qty7 + m.Qty7 ,;
                Qty8       WITH Qty8 + m.Qty8 ,;
                TotQTY     WITH TotQTY+m.TotQTY      ,;
                Edi_Pk_amt WITH Edi_Pk_amt + m.TotQTY*m.PRICE ,;
                PRICE      WITH Edi_Pk_amt/TotQTY IN (lcTmpInvL)
            ENDIF
          ELSE
            *Consolidate same style with different configurations
            IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+m.Style,lcTmpInvL)
              m.Edi_Pk_amt = m.TotQTY*m.PRICE
              m.nSugRetAmt = m.TotQTY*m.nSugRetPri
              INSERT INTO (lcTmpInvL) FROM MEMVAR
            ELSE
              REPLACE Qty1       WITH Qty1 + m.Qty1 ,;
                Qty2       WITH Qty2 + m.Qty2 ,;
                Qty3       WITH Qty3 + m.Qty3 ,;
                Qty4       WITH Qty4 + m.Qty4 ,;
                Qty5       WITH Qty5 + m.Qty5 ,;
                Qty6       WITH Qty6 + m.Qty6 ,;
                Qty7       WITH Qty7 + m.Qty7 ,;
                Qty8       WITH Qty8 + m.Qty8 ,;
                TotQTY     WITH TotQTY+m.TotQTY      ,;
                Edi_Pk_amt WITH Edi_Pk_amt + m.TotQTY*m.PRICE ,;
                nSugRetAmt WITH nSugRetAmt + m.TotQTY*m.nSugRetPri ,;
                PRICE      WITH Edi_Pk_amt/TotQTY ,;
                nSugRetPri WITH nSugRetAmt/TotQTY IN (lcTmpInvL)
            ENDIF
          ENDIF
        ELSE
          IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+SPACE(19),lcTmpInvL)
            m.Style = SPACE(19)
            m.Edi_Pk_amt = m.TotQTY*m.PRICE
            INSERT INTO (lcTmpInvL) FROM MEMVAR
            REPLACE &lcTmpInvL..PRICE  WITH INVLINE.nPkSlPrice
          ELSE
            REPLACE Qty1       WITH Qty1+m.Qty1,;
              Qty2       WITH Qty2+m.Qty2,;
              Qty3       WITH Qty3+m.Qty3,;
              Qty4       WITH Qty4+m.Qty4,;
              Qty5       WITH Qty5+m.Qty5,;
              Qty6       WITH Qty6+m.Qty6,;
              Qty7       WITH Qty7+m.Qty7,;
              Qty8       WITH Qty8+m.Qty8,;
              TotQTY     WITH TotQTY+m.TotQTY,;
              Edi_Pk_amt WITH Edi_Pk_amt+(m.TotQTY*m.PRICE) IN (lcTmpInvL)
            REPLACE &lcTmpInvL..PRICE WITH &lcTmpInvL..PRICE +(m.TotQTY*m.PRICE)
          ENDIF
        ENDIF
        IF llInvUnShip
          REPLACE Ord1   WITH Ord1 + OrdLine.Book1 ,;
            Ord2   WITH Ord2 + OrdLine.Book2 ,;
            Ord3   WITH Ord3 + OrdLine.Book3 ,;
            Ord4   WITH Ord4 + OrdLine.Book4 ,;
            Ord5   WITH Ord5 + OrdLine.Book5 ,;
            Ord6   WITH Ord6 + OrdLine.Book6 ,;
            Ord7   WITH Ord7 + OrdLine.Book7 ,;
            Ord8   WITH Ord8 + OrdLine.Book8 ,;
            TotOrd WITH TotOrd + OrdLine.TotBook IN (lcTmpInvL)
        ENDIF
      ENDSCAN
    ENDSCAN
    SET ORDER TO TAG OrdLine IN OrdLine
    *E130163,1 WLD Commented Out ,Speed up collecting data [Begin]
  ELSE
    SELECT OrdLine
    =SEEK('O'+MORDER+MSTORE)
    SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)='O'+MORDER+MSTORE
      IF !SEEK(XInvoice+STR(LINENO,6),'INVLINE') AND !llInvUnShip
        LOOP
      ENDIF
      SELECT INVLINE
      SCATTER MEMVAR
      =SEEK(m.Style,'Style')
      m.nSugRetPri = IIF(OrdLine.nSugRetPri=0,STYLE.nSugRetPri,OrdLine.nSugRetPri)
      IF EMPTY(m.Pack_id)
        lcGenClrSty = SUBSTR(m.Style,1,lnClrPos-1)+lcGenColor+SUBSTR(m.Style,lnClrPos+lnCLrLen)
        IF llUPCUse .AND. SEEK(lcGenClrSty,'Style') .AND. ;
            !SEEK(m.Style,'StyleUpc') AND SEEK(lcGenClrSty,'StyleUpc')
          IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize + m.cPkVersion + lcGenClrSty,lcTmpInvL)
            m.Edi_Pk_amt = m.TotQTY*m.PRICE
            m.nSugRetAmt = m.TotQTY*m.nSugRetPri
            m.Style = lcGenClrSty
            INSERT INTO (lcTmpInvL) FROM MEMVAR
          ELSE
            REPLACE Qty1       WITH Qty1 + m.Qty1 ,;
              Qty2       WITH Qty2 + m.Qty2 ,;
              Qty3       WITH Qty3 + m.Qty3 ,;
              Qty4       WITH Qty4 + m.Qty4 ,;
              Qty5       WITH Qty5 + m.Qty5 ,;
              Qty6       WITH Qty6 + m.Qty6 ,;
              Qty7       WITH Qty7 + m.Qty7 ,;
              Qty8       WITH Qty8 + m.Qty8 ,;
              TotQTY     WITH TotQTY+m.TotQTY      ,;
              Edi_Pk_amt WITH Edi_Pk_amt + m.TotQTY*m.PRICE ,;
              nSugRetAmt WITH nSugRetAmt + m.TotQTY*m.nSugRetPri ,;
              PRICE      WITH Edi_Pk_amt/TotQTY ,;
              nSugRetPri WITH nSugRetAmt/TotQTY IN (lcTmpInvL)
          ENDIF
        ELSE
          *E124849,1 WAM 11/07/2004 consolidate same style with different configurations
          *INSERT INTO (lcTmpInvL) FROM MEMVAR
          IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+m.Style,lcTmpInvL)
            m.Edi_Pk_amt = m.TotQTY*m.PRICE
            m.nSugRetAmt = m.TotQTY*m.nSugRetPri
            INSERT INTO (lcTmpInvL) FROM MEMVAR
          ELSE
            REPLACE Qty1       WITH Qty1 + m.Qty1 ,;
              Qty2       WITH Qty2 + m.Qty2 ,;
              Qty3       WITH Qty3 + m.Qty3 ,;
              Qty4       WITH Qty4 + m.Qty4 ,;
              Qty5       WITH Qty5 + m.Qty5 ,;
              Qty6       WITH Qty6 + m.Qty6 ,;
              Qty7       WITH Qty7 + m.Qty7 ,;
              Qty8       WITH Qty8 + m.Qty8 ,;
              TotQTY     WITH TotQTY+m.TotQTY      ,;
              Edi_Pk_amt WITH Edi_Pk_amt + m.TotQTY*m.PRICE ,;
              nSugRetAmt WITH nSugRetAmt + m.TotQTY*m.nSugRetPri ,;
              PRICE      WITH Edi_Pk_amt/TotQTY ,;
              nSugRetPri WITH nSugRetAmt/TotQTY IN (lcTmpInvL)
          ENDIF
          *E124849,1 WAM 11/07/2004 (End)
        ENDIF
      ELSE
        IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion+SPACE(19),lcTmpInvL)
          m.Style = ''
          m.Edi_Pk_amt = m.TotQTY*m.PRICE
          m.nSugRetAmt = m.TotQTY*m.nSugRetPri
          INSERT INTO (lcTmpInvL) FROM MEMVAR
          REPLACE &lcTmpInvL..PRICE  WITH INVLINE.nPkSlPrice
        ELSE
          REPLACE Qty1       WITH Qty1+m.Qty1,;
            Qty2       WITH Qty2+m.Qty2,;
            Qty3       WITH Qty3+m.Qty3,;
            Qty4       WITH Qty4+m.Qty4,;
            Qty5       WITH Qty5+m.Qty5,;
            Qty6       WITH Qty6+m.Qty6,;
            Qty7       WITH Qty7+m.Qty7,;
            Qty8       WITH Qty8+m.Qty8,;
            TotQTY     WITH TotQTY+m.TotQTY,;
            nSugRetAmt WITH nSugRetAmt+(m.TotQTY*m.nSugRetPri) ,;
            Edi_Pk_amt WITH Edi_Pk_amt+(m.TotQTY*m.PRICE) IN (lcTmpInvL)
          REPLACE &lcTmpInvL..PRICE WITH &lcTmpInvL..PRICE +(m.TotQTY*m.PRICE)
        ENDIF
      ENDIF
      IF llInvUnShip
        REPLACE Ord1   WITH Ord1 + OrdLine.Book1 ,;
          Ord2   WITH Ord2 + OrdLine.Book2 ,;
          Ord3   WITH Ord3 + OrdLine.Book3 ,;
          Ord4   WITH Ord4 + OrdLine.Book4 ,;
          Ord5   WITH Ord5 + OrdLine.Book5 ,;
          Ord6   WITH Ord6 + OrdLine.Book6 ,;
          Ord7   WITH Ord7 + OrdLine.Book7 ,;
          Ord8   WITH Ord8 + OrdLine.Book8 ,;
          TotOrd WITH TotOrd + OrdLine.TotBook IN (lcTmpInvL)
      ENDIF
    ENDSCAN
  ENDIF
  lnTotQty = 0
  lnTotPcs = 0
  lnTOtAmnt = 0
  SELECT (lcTmpInvL)

  SCAN FOR IIF(llInvUnShip,.T.,TotQTY <> 0)
    *B120999,1 HBG 02/17/2004 Fix bug of sending UOM as AS even if there is no pack detai [Begin]
    MUOM = IIF(!EMPTY(Pack_id) AND llPckDtl,'AS','EA')
    *B120999,1 [End]
    MORDER = ORDER
    lcStyle= STYLE
    MLINENO= LINENO
    =SEEK('O'+MORDER+STR(MLINENO,6),'ORDLINE','ORDLINE')
    MStyle = SUBSTR(STYLE,1,LEN(lcMjrPic))
    MCOLOR = IIF(llColor,SUBSTR(STYLE,lnClrPos,lnCLrLen),'')
    MSCALE = SCALE
    MSIZE  = ''
    =SEEK(lcStyle,'Style')
    MSTYDESC = STYLE.DESC
    XSKU   = lcStyle
    IF llColor AND SUBSTR(STYLE,lnClrPos,lnCLrLen) <> lcGenColor
      =SEEK('N'+SUBSTR(STYLE,lnClrPos,lnCLrLen)+'N'+'COLOR','CODES','CODES')
      MCOLOR = CODES.cDiscRep
    ENDIF
    STORE SPACE(1) TO xUpc,xUpc1,xUpc2,xUpc3,xUpc4,xUpc5,xUpc6,xUpc7,xUpc8
    STORE 1 TO XPPTOT
    SELECT (lcTmpInvL)
    lcUpcStyle = STYLE
    IF llCatDanial
      lcUpcStyle = ALLTRIM(SUBSTR(STYLE,1,LEN(lcMjrPic)))
      lcUpcStyle = PADR(SUBSTR(lcUpcStyle,1,LEN(lcUpcStyle)-2),LEN(lcMjrPic))
      lcUpcStyle = lcUpcStyle + SUBSTR(STYLE,LEN(lcMjrPic)+1)
      lcUpcStyle = IIF(SEEK(lcUpcStyle,'STYLE'),lcUpcStyle,STYLE)
    ENDIF
    IF llUPCUse AND SEEK(lcUpcStyle,'StyleUpc')
      XSKU = '**BY SIZE**'
      SELECT StyleUpc
      SCAN REST WHILE STYLE = lcUpcStyle
        lcSize = ALLTRIM(SIZE)
        xUpc&lcSize = cUpcNum1 + cUpcNum2 + cUpcNum3
      ENDSCAN
    ENDIF
    SELECT (lcTmpInvL)
    =SEEK('S'+MSCALE,'Scale')
    MKEY = Account+STYLE
    IF EMPTY(Pack_id)
      STORE STYLE TO XSKU1,XSKU2,XSKU3,XSKU4,XSKU5,XSKU6,XSKU7,XSKU8
      SELECT SPCK_LIN
      IF SEEK('S'+MKEY)
        XSKU  ='**BY SIZE**'
        SCAN REST WHILE TYPE+Account+STYLE = 'S'+MKEY
          IF lfOrgSku(ALLTRIM(Pack_id))
            IF TotQTY = 0
              XSKU = SKU
              =SEEK('P'+MSCALE+&lcTmpInvL..PrePak,'Scale','Scale')
              XPPTOT = SCALE.PPTOT
              =SEEK('S'+MSCALE,'Scale')
            ELSE
              FOR lnCount = 1 TO SCALE.CNT
                lcCount = STR(lnCount,1)
                IF Qty&lcCount = 1
                  *C200464,1 HBG 6/1/2003 Fix bug of sent SKU with extra characters [Begin]
                  *XSKU&lcCount = Pack_id + "-"+&lcTmpInvL..cPkColor+"-"+&lcTmpInvL..cPckSize
                  XSKU&lcCount = Pack_id
                  *C200464,1 [End]
                  EXIT
                ENDIF
              ENDFOR
            ENDIF
          ENDIF
        ENDSCAN
      ELSE
        IF XSKU <> '**BY SIZE**' AND (SEEK('P'+MKEY) OR SEEK('P'+'*****'+&lcTmpInvL..STYLE))
          *C200464,1 HBG 6/1/2003 Fix bug of sent SKU with extra characters [Begin]
          *XSKU = Pack_id + "-"+&lcTmpInvL..cPkColor+"-"+&lcTmpInvL..cPckSize
          XSKU = Pack_id
          *C200464,1 [End]
        ENDIF
      ENDIF
    ELSE
      IF SEEK('S'+&lcTmpInvL..Account+PADR(&lcTmpInvL..Pack_id,19)+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize,'SPCK_LIN','SPCK_LINST')
        XSKU = SPCK_LIN.Pack_id
      ELSE
        lcScaleDesc = IIF(SEEK('S'+SUBSTR(&lcTmpInvL..cPckSize,1,1),'Scale','Scale'),EVAL("SCALE.sz" + ALLTRIM(STR(VAL(RIGHT(&lcTmpInvL..cPckSize,2))))),"")
        XSKU = ALLTRIM(Pack_id) + "-"+ALLTRIM(&lcTmpInvL..cPkColor)+"-"+lcScaleDesc
      ENDIF
    ENDIF
    lnPackQty = 1
    SELECT (lcTmpInvL)
    IF XSKU = '**BY SIZE**'
      FOR lnCount = 1 TO SCALE.CNT
        lcCount = STR(lnCount,1)
        IF IIF(llInvUnShip,Ord&lcCount > 0,Qty&lcCount > 0)
          XTOTQTY = Qty&lcCount
          XSKU    = XSKU&lcCount
          xUpc    = ALLTRIM(xUpc&lcCount)
          MSIZE   = SCALE.SZ&lcCount
          XPRICE  = PRICE
          XSugRetPri = nSugRetPri
          XDiscPri = XPRICE*(100-XMERDISC)/100
          llWriteDoc = IIF(oField2Seg.DO(lnFilePoint,'04','SycEdiFS',lcMapS),llWriteDoc,.F.)
          MCTT = MCTT+1
          lnTotQty = lnTotQty + XTOTQTY
          lnTotPcs = lnTotPcs + XTOTQTY
          lnTOtAmnt=lnTOtAmnt+XTOTQTY*XPRICE
        ENDIF
      ENDFOR
    ELSE
      IF IIF(llInvUnShip,.T.,TotQTY > 0)
        XTOTQTY = TotQTY
        XPRICE  = PRICE
        XSugRetPri = nSugRetPri
        lnTotPcs = lnTotPcs + XTOTQTY
        XDiscPri = XPRICE*(100-XMERDISC)/100
        SELECT Spck_HDR
        SET ORDER TO TAG Spck_HdrVr
        SELECT SPCK_LIN
        SET ORDER TO TAG Spck_linvr
        lcActKey = 'P'+&lcTmpInvL..Account+&lcTmpInvL..Pack_id+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion
        lcGenKey = 'P'+'*****'+&lcTmpInvL..Pack_id+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion
        lc2Key   = &lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion
        IF SEEK(lcActKey) OR SEEK(lcGenKey)
          lcPackAcc = Account
          *B606211,1 Send UPC in Invoice Hassan [Begin]
          lcccPack_Id = PADR(Pack_id,19)
          *B606211,1 Send UPC in Invoice Hassan [End  ]
          =SEEK('P'+lcPackAcc+PADR(LEFT(Pack_id,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lc2Key,'SPCK_HDR')
          MStyle = Pack_id + "-"+&lcTmpInvL..cPkColor+"-"+&lcTmpInvL..cPckSize
          MCOLOR = ''
          MSIZE  = ''
          MSTYDESC  = Spck_HDR.DESC
          SUM REST TotQTY TO lnPackQty ;
            WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion+STYLE = 'P'+lcPackAcc+&lcTmpInvL..Pack_id+lc2Key
          *HBG Get number of Packs from INVLINE.nPAckNo [Begin]
          *XPCKQTY = IIF(lnPackQty<>0, XTOTQTY/lnPackQty, XTOTQTY)
          XPCKQTY = IIF(lnPackQty<>0, &lcTmpInvL..nPackNo, XTOTQTY)
          *HBG [END]
          *B606211,1 Send UPC in Invoice Hassan [Begin]
          *IF llUPCUse .AND. SEEK(XSKU+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize,'StyleUpc') AND StyleUpc.lUPCPack
          *  xUpc = ALLTRIM(StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3)
          *ENDIF
          *IF INT(XPCKQTY) <> XPCKQTY OR (!EMPTY(xUpc) AND llGMA)
          IF EMPTY(lcccPack_Id)
            IF llUPCUse .AND. SEEK(XSKU+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize,'StyleUpc') AND StyleUpc.lUPCPack
              xUpc = ALLTRIM(StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3)
            ENDIF
          ELSE
            IF llUPCUse .AND. SEEK('*****'+lcccPack_Id+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize,'StyleUpc','GMAPkUPc') AND StyleUpc.lUPCPack
              xUpc = ALLTRIM(StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3)
            ENDIF
          ENDIF
          *B606665,1 HBG 18/11/2002 Fix bug of send pack Qty = 1 in case there is a UPC for this pack[Begin]
          *IF INT(XPCKQTY) <> XPCKQTY OR !EMPTY(xUpc)
          =SEEK(lcActKey,'SPCK_HDR') OR SEEK(lcGenKey,'SPCK_HDR')
          IF INT(XPCKQTY) <> XPCKQTY
            *B606665,1 [End]
            *B606211,1 Send UPC in Invoice Hassan [End  ]
            XPCKQTY   = 1
            *lnPackQty = 1
            *B606328,1 Adjust the qty in case of pack Hassan [Begin]
            *B606665,1 HBG 18/11/2002 Fix bug of send pack Qty = 1 in case there is a UPC for this pack[Begin]
            *XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 ,XTOTQTY,XPCKQTY)
            *C200479,1 HBG 8/1/2003 Use fields lRange & lPckPrPiec from INVLINE[Begin]
            *XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 OR SPCK_HDR.lRange OR SPCK_HDR.lpckprpiec,;
            *              XTOTQTY,XPCKQTY)
            XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 OR &lcTmpInvL..lRange OR &lcTmpInvL..lPckPrPiec,;
              XTOTQTY,XPCKQTY)
            *C200479,1 [End]
            *B606665,1 [End]
            *B606328,1 Adjust the qty in case of pack Hassan [End   ]
          ELSE
            *B606328,1 Adjust the qty in case of pack Hassan [Begin]
            *XTOTQTY = XPCKQTY
            *B606665,1 HBG 18/11/2002 Fix bug of send pack Qty = 1 in case there is a UPC for this pack[Begin]
            *XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 ,XTOTQTY,XPCKQTY)
            *C200479,1 HBG 8/1/2003 Use fields lRange & lPckPrPiec from INVLINE[Begin]
            *XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 OR SPCK_HDR.lRange OR SPCK_HDR.lpckprpiec,;
            *              XTOTQTY,XPCKQTY)
            XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 OR &lcTmpInvL..lRange OR &lcTmpInvL..lPckPrPiec,;
              XTOTQTY,XPCKQTY)
            *C200479,1 [End]
            *B606665,1 [End]
            *B606328,1 Adjust the qty in case of pack Hassan [End
          ENDIF
          *--Adel --Initiliaze Packs No
          lnPackQty =1
          *--Adel --Initiliaze Packs No
          *B606328,1 Adjust the qty in case of pack Hassan [Begin]
          *XPRICE      = &lcTmpInvL..nPkSlPrice
          XPRICE = IIF(XTOTQTY=0,XPRICE,&lcTmpInvL..Edi_Pk_amt/XTOTQTY)
          *B606665,1 HBG 18/11/2002 Fix bug of send pack Qty = 1 in case there is a UPC for this pack[Begin]
          *XPRICE = IIF(&lcTmpInvL..nPkSlPrice = 0 ,XPRICE,&lcTmpInvL..nPkSlPrice)
          *C200479,1 HBG 8/1/2003 Use fields lRange & lPckPrPiec from INVLINE[Begin]
          *XPRICE = IIF(&lcTmpInvL..nPkSlPrice = 0 OR SPCK_HDR.lRange OR SPCK_HDR.lpckprpiec,;
          *             XPRICE,&lcTmpInvL..nPkSlPrice)
          XPRICE = IIF(&lcTmpInvL..nPkSlPrice = 0 OR &lcTmpInvL..lRange OR &lcTmpInvL..lPckPrPiec,;
            XPRICE,&lcTmpInvL..nPkSlPrice)
          *C200479,1 [End]
          *B606665,1 [End]
          *B606328,1 Adjust the qty in case of pack Hassan [End   ]
          XSugRetPri = IIF(XTOTQTY=0,XSugRetPri,&lcTmpInvL..nSugRetAmt/XTOTQTY)
          lnTOtAmnt  = lnTOtAmnt+ROUND(XTOTQTY*lnPackQty*ROUND(XPRICE/lnPackQty,2),2)
        ENDIF
        SET ORDER TO TAG SPCKLINS
        SELECT (lcTmpInvL)
        llWriteDoc = IIF(oField2Seg.DO(lnFilePoint,'04','SycEdiFS',lcMapS),llWriteDoc,.F.)
        *C200592,1 HBG 17/08/2003 Send using Assortment Pack [Begin]
        IF !USED('SPCK_LIN1')
          USE (oAriaApplication.DataDir+ 'SPCK_LIN') IN 0 ORDER TAG 'Spcklins' SHARED ALIAS SPCK_LIN1 AGAIN
        ELSE
          SELECT SPCK_LIN1
          SET ORDER TO TAG SPCKLINS
        ENDIF
        SELECT SPCK_LIN
        SET ORDER TO TAG Spck_linvr
        IF TYPE('lcPackAcc')<>"U" AND SEEK('P'+lcPackAcc+&lcTmpInvL..Pack_id+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion)
          STORE 0 TO MSLNCOUNTER
          SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion+STYLE =;
              'P'+lcPackAcc+&lcTmpInvL..Pack_id+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion
            STORE ' ' TO MSLNColor,MSLNSTYDESC
            =SEEK(SPCK_LIN.STYLE,'Style')
            MSLNSTYDESC = STYLE.DESC
            =SEEK('S'+STYLE.SCALE,'Scale')
            *B122272,1 WLD  Incorrect price at SLN segment [Begin]
            =SEEK(SPCK_LIN.STYLE+XInvoice,'InvLine','INVLINES')
            *B122272,1 WLD Incorrect price at SLN segment [End  ]
            MSLNPRICE = INVLINE.PRICE
            MSLNColor = IIF(llColor,SUBSTR(SPCK_LIN.STYLE,lnClrPos,lnCLrLen),'')
            FOR LNSLNCOUNT1 = 1 TO SCALE.CNT
              STORE ' ' TO MSLNSKU,MSLNUPC,MSLNSIZE
              STORE 0 TO MSlnQty
              LCSLNCOUNT1 =STR(LNSLNCOUNT1,1)
              SELECT SPCK_LIN
              IF Qty&LCSLNCOUNT1. <> 0
                MSlnQty     = Qty&LCSLNCOUNT1.
                MSLNSIZE    = SCALE.SZ&LCSLNCOUNT1.
                MSLNCOUNTER = MSLNCOUNTER + 1
                MSLNSIZE    = SCALE.SZ&LCSLNCOUNT1.
                SELECT SPCK_LIN1
                =SEEK('S'+lcPackAcc+SPCK_LIN.STYLE,'Spck_Lin1','SPCKLINS')
                SCAN REST WHILE TYPE+Account+STYLE+Pack_id = 'S'+lcPackAcc+SPCK_LIN.STYLE
                  FOR LNSLNCOUNT2 = 1 TO 8
                    LCSLNCOUNT2 =STR(LNSLNCOUNT2,1)
                    IF Qty&LCSLNCOUNT2. = 1 AND LNSLNCOUNT2 = LNSLNCOUNT1
                      MSLNSKU = Pack_id
                      EXIT
                    ENDIF
                  ENDFOR
                ENDSCAN
                IF SEEK(SPCK_LIN.STYLE+LCSLNCOUNT1,'STYLEUPC','STYLEUPC')
                  MSLNUPC=StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
                ENDIF
                llWriteDoc = IIF(oField2Seg.DO(lnFilePoint,'06','SycEdiFS',lcMapS),llWriteDoc,.F.)
              ENDIF
            ENDFOR
            SELECT SPCK_LIN
          ENDSCAN
        ENDIF
        SELECT SPCK_LIN1
        USE
        SELECT SPCK_LIN
        SET ORDER TO TAG SPCKLINS
        SELECT (lcTmpInvL)
        *C200592,1 [End]
        lnTotQty = lnTotQty + XTOTQTY
        MCTT = MCTT+1
      ENDIF
    ENDIF
  ENDSCAN
  MGROSSAMNT = MGROSSAMNT + InvHdr.TOTALCHG
  MDISCAMNT  = MDISCAMNT  + InvHdr.TOTALCHG*InvHdr.Trde_Disc/100
  MNETAMNT   = MNETAMNT   + InvHdr.TOTALCHG*(100-InvHdr.Trde_Disc)/100
  llWriteDoc = IIF(oField2Seg.DO(lnFilePoint,'05','SycEdiFS',lcMapS),llWriteDoc,.F.)
  =RLOCK('INVHDR')
  REPLACE dSentDate WITH oAriaApplication.SystemDate IN InvHdr
  UNLOCK IN InvHdr
  IF SEEK(lcPartner,'SYCFACT')
    REPLACE BatchNo WITH IIF(MBATCHNO=99,0,MBATCHNO) IN SYCFACT
  ENDIF
  USE IN (lcTmpInvL)
  ***  EDDIE 11/05/2003, file access denied [begin]
  *ERASE (oAriaApplication.WorkDir+lcTmpInvL+'.DBF')
  *ERASE (oAriaApplication.WorkDir+lcTmpInvL+'.CDX')
  IF NOT USED(lcTmpInvL)
    ERASE (oAriaApplication.WorkDir+lcTmpInvL+'.DBF')
    ERASE (oAriaApplication.WorkDir+lcTmpInvL+'.CDX')
  ENDIF
  ***  EDDIE 11/05/2003 [begin]
  Release oGetRltdFld
  Release oField2Seg
  WAIT CLEAR

  RETURN llWriteDoc
ENDFUN

*:****************************************************************************************
*! Name      : lfUccPck
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process Ucc Labels Pack For GMA
*:****************************************************************************************
*! Called from : Class "SendUccLabels"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfUccPck

  LOCAL lnTotQty, lnTotPcs, lnRow, llFound

  lnTotPcs  = Pack_Lin.TotQTY
  llFound   = .F.

  lcPkColor = OrdLine.cPkColor
  lcPckSize = OrdLine.cPckSize
  lcPkVrson = OrdLine.cPkVersion
  *C123073,1 HBG Get the Full Pack Key[Begin]
  lcPackIdval = lcPackId+lcPkColor+lcPckSize
  *C123073,1 [End]
  IF !SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson  , 'Spck_Lin', 'Spck_LinVr') AND;
      !SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson  , 'Spck_Lin', 'Spck_LinVr')
    oObject.ProcessSizes()
    RETURN
  ENDIF
  *C123073,1 HBG USE new array to print packs and style[End]
  *!*	  FOR lnRow = 1 TO lnLblInfo
  *!*	    *C123073,1 HBG check for the Full Pack Key[Begin]
  *!*	    *IF laLblInfo[lnRow, 4]  = lcPackId ;
  *!*	    *    .AND. LEFT(laLblInfo[lnRow, 1], 2) = "$#"
  *!*	    IF laLblInfo[lnRow, 4]  = lcPackIdval ;
  *!*	        .AND. LEFT(laLblInfo[lnRow, 1], 2) = "$#"
  *!*	    *C123073,1 [End]
  *!*	      laLblInfo[lnRow, 7] = laLblInfo[lnRow, 7] + lnTotPcs
  *!*	      llFound                 = .T.
  *!*	      *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [Begin]
  *!*	      IF SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr') OR;
  *!*	          SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr')

  *!*	        laLblInfo[lnLblInfo, 8] = laLblInfo[lnLblInfo, 8] + SPCK_LIN.TotQTY
  *!*	      ENDIF
  *!*	      *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [End]
  *!*	      EXIT
  *!*	    ENDIF
  *!*	  ENDFOR

  *!*	  IF !llFound
  *!*	    lnLblInfo = lnLblInfo + 1

  *!*	    *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [Begin]
  *!*	    *DIMENSION laLblInfo[lnLblInfo , 7]
  *!*	    DIMENSION laLblInfo[lnLblInfo , 8]

  *!*	    IF SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr') OR;
  *!*	        SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr')
  *!*	      laLblInfo[lnLblInfo, 8] = SPCK_LIN.TotQTY
  *!*	    ELSE
  *!*	      laLblInfo[lnLblInfo, 8] = 0
  *!*	    ENDIF
  *!*	    *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [End]

  *!*	    laLblInfo[lnLblInfo, 1] = "$#" + Spck_HDR.Account
  *!*	    laLblInfo[lnLblInfo, 2] = 0
  *!*	    laLblInfo[lnLblInfo, 3] = ""
  *!*	    laLblInfo[lnLblInfo, 4] = lcPackId+lcPkColor+lcPckSize
  *!*	    laLblInfo[lnLblInfo, 5] = ""
  *!*	    laLblInfo[lnLblInfo, 6] = 0
  *!*	    laLblInfo[lnLblInfo, 7] = lnTotPcs

  *!*	    IF llUPCUse .AND.;
  *!*	        ((SEEK(MACCOUNT + lcPackId+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack) .OR. ;
  *!*	        (SEEK('*****'  + lcPackId+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack))

  *!*	      laLblInfo[lnLblInfo,5] = StyleUpc.cUpcNum1 + StyleUpc.cUpcNum2 + ;
  *!*	        StyleUpc.cUpcNum3
  *!*	    ENDIF
  *!*	  ENDIF

  FOR lnRow = 1 TO lnTmpLbIfo
    IF laTmpLbIfo[lnRow, 4]  = lcPackIdval ;
        .AND. LEFT(laTmpLbIfo[lnRow, 1], 2) = "$#"
      laTmpLbIfo[lnRow, 7] = laTmpLbIfo[lnRow, 7] + lnTotPcs
      llFound                 = .T.
      IF SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr') OR;
          SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr')

        laTmpLbIfo[lnTmpLbIfo, 8] = laTmpLbIfo[lnTmpLbIfo, 8] + SPCK_LIN.TotQTY
      ENDIF
      EXIT
    ENDIF
  ENDFOR

  IF !llFound
    lnTmpLbIfo = lnTmpLbIfo + 1

    DIMENSION laTmpLbIfo[lnTmpLbIfo, 9]

    IF SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr') OR;
        SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr')
      laTmpLbIfo[lnTmpLbIfo, 8] = SPCK_LIN.TotQTY
    ELSE
      laTmpLbIfo[lnTmpLbIfo, 8] = 0
    ENDIF


    laTmpLbIfo[lnTmpLbIfo, 1] = "$#" + Spck_HDR.Account
    laTmpLbIfo[lnTmpLbIfo, 2] = 0
    laTmpLbIfo[lnTmpLbIfo, 3] = ""
    laTmpLbIfo[lnTmpLbIfo, 4] = lcPackId+lcPkColor+lcPckSize
    laTmpLbIfo[lnTmpLbIfo, 9] = lcPackId+lcPkColor+lcPckSize
    laTmpLbIfo[lnTmpLbIfo, 5] = ""
    laTmpLbIfo[lnTmpLbIfo, 6] = 0
    laTmpLbIfo[lnTmpLbIfo, 7] = lnTotPcs

    *B038400,1 WAM 10/28/2004 Get the UPC record
    *IF llUPCUse .AND.;
((SEEK(MACCOUNT + lcPackId+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack) .OR. ;
(SEEK('*****'  + lcPackId+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack))
    IF llUPCUse .AND.;
        ((SEEK(MACCOUNT + PADR(lcPackId,19)+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack) .OR. ;
        (SEEK('*****'   + PADR(lcPackId,19)+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack))
      *B038400,1 WAM (End)

      laTmpLbIfo[lnTmpLbIfo,5] = StyleUpc.cUpcNum1 + StyleUpc.cUpcNum2 + ;
        StyleUpc.cUpcNum3
    ENDIF
  ENDIF
  *C123073,1 HBG USE new array to print packs and style[End]
ENDFUN
*:****************************************************************************************
*! Name      : lfClcPck
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process Ucc Labels Pack For GMA
*:****************************************************************************************
*! Called from : Class "SendUccLabels"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfClcPck
  LOCAL lcPackId, lnRow, lnTotPackQty, lcAccount , lcOldOrder

  SELECT SPCK_LIN
  lcOldOrder = ORDER()
  SET ORDER TO Spck_linvr

  FOR lnRow = 1 TO lnLblInfo
    IF LEFT(laLblInfo[lnRow, 1], 2) = "$#"
      lcPackId            = laLblInfo[lnRow, 4]
      lcAccount           = SUBSTR(laLblInfo[lnRow, 1], 3, 5)
      laLblInfo[lnRow, 1] = ''
      = SEEK('P' + lcAccount + lcPackId, 'Spck_Lin')
      SUM TotQTY REST;
        WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion+STYLE = 'P' + lcAccount + lcPackId;
        TO lnTotPackQty
      laLblInfo[lnRow, 2] = IIF(lnTotPackQty > 0 , laLblInfo[lnRow,7] / lnTotPackQty , 0)
    ENDIF
  ENDFOR

  *-- Restore the old order in Spck_Lin
  IF EMPTY(lcOldOrder)
    SET ORDER TO
  ELSE
    SET ORDER TO (lcOldOrder)
  ENDIF
ENDFUN
*:****************************************************************************************
*! Name      : lfCrnInf
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process Ucc Labels Pack For GMA
*:****************************************************************************************
*! Called from : Class "SendUccLabels"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfCrnInf

  *LPARAMETER lnCartNo, lcUCC9, lcUCCCheck
  PRIVATE lnAlias
  lnAlias = ALIAS()
  SELECT (lcTmpCrtninfo)
  APPEND BLANK
  *C123073,1 [End] Fix Bug of variables lnCartNo,lcUCC9,lcUCCCheck Not found[Begin]
  *!*	  REPLACE BOL_NO    WITH MBOL      ,;
  *!*	    PACK_NO   WITH MPACKNO   ,;
  *!*	    CART_NO   WITH lnCartNo  ,;
  *!*	    lPlt      WITH EDIPH.lPltShp ,;
  *!*	    ASN_VER   WITH IIF(EDIPH.lPltShp,EDIPH.CPLTLBL,IIF(Bol_Hdr.cToStOrCn='C' OR Pack_Hdr.cToStOrCn='C',EDIPH.cAsnLbl2,EDIPH.cAsnLbl1)),;
  *!*	    VND_NAME  WITH MShipName ,;
  *!*	    VND_ADDR1 WITH MAddress1 ,;
  *!*	    VND_ADDR2 WITH MAddress2 ,;
  *!*	    VND_CITY  WITH MCity     ,;
  *!*	    VND_STATE WITH MState    ,;
  *!*	    VND_ZIP   WITH MZip      ,;
  *!*	    SHP_NAME  WITH MShToName ,;
  *!*	    SHP_ADDR1 WITH MShToAdd1 ,;
  *!*	    SHP_ADDR2 WITH MShToAdd2 ,;
  *!*	    SHP_CITY  WITH MShtoCity ,;
  *!*	    SHP_STATE WITH MShToStat ,;
  *!*	    SHP_ZIP   WITH MShToZip  ,;
  *!*	    CustPO    WITH MCustPo   ,;
  *!*	    DEPT      WITH MDEPT
  *!*	  REPLACE cStStore  WITH lcStore   ,;
  *!*	    cStName   WITH lcStName  ,;
  *!*	    cStAddr1  WITH lcStAddr1 ,;
  *!*	    cStAddr2  WITH lcStAddr2 ,;
  *!*	    cStCity   WITH lcStCity  ,;
  *!*	    cStState  WITH lcStState ,;
  *!*	    cStZip    WITH lcStZip   ,;
  *!*	    MANUF_ID  WITH MMVEND    ,;
  *!*	    UCC9      WITH lcUCC9    ,;
  *!*	    UCC_CHECK WITH lcUCCCheck,;
  *!*	    EVENT_COD WITH MEVENT_COD,;
  *!*	    ShipVia   WITH MSHIPVIA  ,;
  *!*	    STORE     WITH MSHIP_CODE,;
  *!*	    INT_VEND  WITH MVend     ,;
  *!*	    Note1     WITH MORDNOTE1 ,;
  *!*	    Note2     WITH MORDNOTE2 ,;
  *!*	    Cancelled WITH MORDCANC
  *!*	  REPLACE STYLE      WITH lcStyle    ,;
  *!*	    cClrDesc   WITH lcClrDesc  ,;
  *!*	    cSizeDesc  WITH lcSizeDesc ,;
  *!*	    CARTONS    WITH lnCartons  ,;
  *!*	    TotQTY     WITH lnQtyPCrtn ,;
  *!*	    Pack_id    WITH lcCrtnSku  ,;
  *!*	    cPkColor   WITH lcPkColor  ,;
  *!*	    cPckSize   WITH lcPckSize  ,;
  *!*	    cPkVersion WITH lcPkVrson  ,;
  *!*	    cUpc       WITH lcUpc      ,;
  *!*	    cAdd_User  WITH oAriaApplication.User_ID    ,;
  *!*	    dAdd_Date  WITH oAriaApplication.SystemDate ,;
  *!*	    cAdd_Time  WITH TIME()

  REPLACE BOL_NO    WITH MBOL      ,;
    PACK_NO   WITH MPACKNO   ,;
    CART_NO   WITH lnCartNum  ,;
    lPlt      WITH EdiPh.lPltShp ,;
    ASN_VER   WITH IIF(EdiPh.lPltShp,EdiPh.CPLTLBL,IIF(Bol_Hdr.cToStOrCn='C' OR Pack_Hdr.cToStOrCn='C',EdiPh.cAsnLbl2,EdiPh.cAsnLbl1)),;
    VND_NAME  WITH MShipName ,;
    VND_ADDR1 WITH MAddress1 ,;
    VND_ADDR2 WITH MAddress2 ,;
    VND_CITY  WITH MCity     ,;
    VND_STATE WITH MState    ,;
    VND_ZIP   WITH MZip      ,;
    SHP_NAME  WITH MShToName ,;
    SHP_ADDR1 WITH MShToAdd1 ,;
    SHP_ADDR2 WITH MShToAdd2 ,;
    SHP_CITY  WITH MShtoCity ,;
    SHP_STATE WITH MShToStat ,;
    SHP_ZIP   WITH MShToZip  ,;
    CustPO    WITH MCustPo   ,;
    DEPT      WITH MDEPT

  REPLACE cStStore  WITH lcStore   ,;
    cStName   WITH lcStName  ,;
    cStAddr1  WITH lcStAddr1 ,;
    cStAddr2  WITH lcStAddr2 ,;
    cStCity   WITH lcStCity  ,;
    cStState  WITH lcStState ,;
    cStZip    WITH lcStZip   ,;
    MANUF_ID  WITH MMVEND    ,;
    UCC9      WITH lcUCC9No  ,;
    UCC_CHECK WITH lcUCCChkNO,;
    EVENT_COD WITH MEVENT_COD,;
    ShipVia   WITH MSHIPVIA  ,;
    STORE     WITH MSHIP_CODE,;
    INT_VEND  WITH MVend     ,;
    Note1     WITH MORDNOTE1 ,;
    Note2     WITH MORDNOTE2 ,;
    Cancelled WITH MORDCANC


  REPLACE STYLE      WITH lcStyle    ,;
    cClrDesc   WITH lcClrDesc  ,;
    cSizeDesc  WITH lcSizeDesc ,;
    CARTONS    WITH lnCartons  ,;
    TotQTY     WITH lnQtyPCrtn ,;
    Pack_id    WITH lcCrtnSku  ,;
    cUpc       WITH lcUpc      ,;
    cAdd_User  WITH oAriaApplication.User_ID    ,;
    dAdd_Date  WITH oAriaApplication.SystemDate ,;
    cAdd_Time  WITH TIME()

  =lfSAVPCK()
  *C123073,1 [End]
  =lfClcPck()
  SELECT (lcTmpCrtninfo)

  IF TYPE("laLblInfo[1,4]") = "C" .AND. !EMPTY(laLblInfo[1,4])
    SAVE TO MEMO MLBLINFO ALL LIKE laLblInfo
  ENDIF
  SELECT (lnAlias)
  *:****************************************************************************************
  *! Name      : lfCrtons
  *! Developer : Hassan Ali (HIA)
  *! Date      : 05/07/2002
  *! Purpose   : Process Ucc Labels Pack For GMA
  *:****************************************************************************************
  *! Called from : Class "SendUccLabels"
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfCrtons
  *LPARAMETERS lcPackNo,lcAccount
  PRIVATE lnAlias, lnCount, lcCount
  LOCAL lcTemp
  lnAlias = SELECT()
  ZAP IN (lcCartons)

  SELECT Pack_Lin
  = SEEK(lcPackNo)

  SCAN REST WHILE PACK_NO = lcPackNo
    = SEEK('O' + Pack_Hdr.ORDER + Pack_Hdr.STORE + STYLE + STR(nOrdLineNo,6),'ORDLINE','ORDLINST')
    lcPackId  = OrdLine.Pack_id
    lcPkColor = OrdLine.cPkColor
    lcPckSize = OrdLine.cPckSize
    lcPkVrson = OrdLine.cPkVersion

    IF EMPTY(lcPackId) AND ;
        ( SEEK('P' + lcAccount + STYLE, 'Spck_Lin') OR SEEK('P' + '*****' + STYLE,'Spck_Lin'))
      lcPackId = SPCK_LIN.Pack_id
    ENDIF
    lcSize = ' '
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF Qty&lcCount = TotQTY
        lcSize = lcCount
      ENDIF
    ENDFOR
    lcStyle = SUBSTR(Pack_Lin.STYLE, 1, LEN(lcMjrPic))
    IF !SEEK(STR(Pack_Lin.nPltNo,4) + STR(Pack_Lin.No_Cart,4),lcCartons)
      INSERT INTO (lcCartons) (nPltNo          , No_Cart          , STYLE          , Major   , SIZE   ,Pack_id  ,cPkColor , cPckSize, cPkVersion , TotQTY          );
        VALUES           (Pack_Lin.nPltNo , Pack_Lin.No_Cart , Pack_Lin.STYLE , lcStyle , lcSize ,lcPackId ,lcPkColor,lcPckSize, lcPkVrson  , Pack_Lin.TotQTY )
    ELSE
      IF laSetups[1,2]
        IF LEFT(STYLE,LEN(STYLE)-3) = LEFT(Pack_Lin.STYLE,LEN(Pack_Lin.STYLE)-3)
          lcTemp = STYLE
        ELSE
          lcTemp = ''
        ENDIF
        REPLACE STYLE      WITH lcTemp ,;
          Major      WITH IIF(Major=PADR(lcStyle,19),Major,'') ,;
          SIZE       WITH IIF(SIZE=lcSize,SIZE,'') ,;
          Pack_id    WITH IIF(Pack_id=lcPackId, lcPackId, '') ,;
          cPkColor   WITH lcPkColor,;
          cPckSize   WITH lcPckSize,;
          cPkVersion WITH lcPkVrson,;
          TotQTY     WITH TotQTY + Pack_Lin.TotQTY IN (lcCartons)
      ELSE
        REPLACE STYLE      WITH IIF(STYLE = Pack_Lin.STYLE,STYLE,'') ,;
          Major      WITH IIF(Major=PADR(lcStyle,19),Major,'') ,;
          SIZE       WITH IIF(SIZE=lcSize,SIZE,'') ,;
          Pack_id    WITH IIF(Pack_id=lcPackId, lcPackId, '') ,;
          cPkColor   WITH lcPkColor,;
          cPckSize   WITH lcPckSize,;
          cPkVersion WITH lcPkVrson,;
          TotQTY     WITH TotQTY + Pack_Lin.TotQTY IN (lcCartons)
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lnAlias)
  *:****************************************************************************************
  *! Name      : lfCreate
  *! Developer : Hassan Ali (HIA)
  *! Date      : 05/07/2002
  *! Purpose   : Process Ucc Labels Pack For GMA
  *:****************************************************************************************
  *! Called from : Class "SendUccLabels"
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfCreate

  CREATE TABLE (oAriaApplication.WorkDir+lcCartons) ;
    (nPltNo N(4), No_Cart N(4),STYLE C(19),Major C(19),SIZE C(1), TotQTY N(6),Pack_id C(16),cPkColor C(6),cPckSize C(3),cPkVersion C(4))
  INDEX ON STR(nPltNo,4)+STR(No_Cart,4) TAG (lcCartons)
ENDFUN
*:****************************************************************************************
*! Name      : lfUCCSEK
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process Ucc Labels Pack For GMA
*:****************************************************************************************
*! Called from : Class "SendUccLabels"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfUCCSEK

  IF !EMPTY(Pack_id) .AND.;
      SEEK(MACCOUNT + PADR(Pack_id , 19) + cPkColor+LEFT(cPckSize,1), 'StyleUpc' , 'GMAPKUPC') .AND.;
      StyleUpc.lUPCPack
    lcUpc = StyleUpc.cUpcNum1 + StyleUpc.cUpcNum2 +StyleUpc.cUpcNum3
  ENDIF
ENDFUN
*:****************************************************************************************
*! Name      : lfAsnCrt
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process ASN Cartons For GMA
*:****************************************************************************************
*! Called from : Class "Send856"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfAsnCrt

  *LPARAMETERS lcPackNo,lcAccount
  PRIVATE lnAlias,lnCount, lcCount

  lnAlias = SELECT()
  SET ORDER TO TAG SPCKLINS IN SPCK_LIN
  ZAP IN (lcTmpPackLine)
  SET ORDER TO TAG 'STANDARD' IN (lcTmpPackLine)

  SELECT Pack_Lin
  =SEEK(lcPackNo)

  SCAN REST WHILE PACK_NO = lcPackNo
    =SEEK(lcPackNo,'Pack_Hdr')

    *B804154,1 HS 05/02/2001 Use another index tag that doesn't contain the Store and
    *                        Style fields [Begin]
    *=SEEK('O'+Pack_Hdr.Order+Pack_Hdr.Store+Style+STR(nOrdLineNo,6),'ORDLINE','ORDLINST')
    *B606350 Hassan [Begin]
    *=SEEK('O' + Pack_Hdr.ORDER + STR(Pacl_lin.nOrdLineNo,6) , 'ORDLINE' , 'ORDLINE')
    =SEEK('O' + Pack_Hdr.ORDER + STR(Pack_Lin.nOrdLineNo,6) , 'ORDLINE' , 'ORDLINE')
    *B606350 Hassan [End]
    *B804154,1 HS 05/02/2001 Use another index tag that doesn't contain... [End]

    lcGenSty = SUBSTR(STYLE,1,lnClrPos-1)+lcGenColor+SUBSTR(STYLE,lnClrPos+lnCLrLen)
    IF llUPCUse AND SEEK(lcGenSty,'Style') AND !SEEK(STYLE,'StyleUpc') .AND. ;
        SEEK(lcGenSty,'StyleUpc')
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        IF Qty&lcCount > 0 AND ;
            SEEK(lcGenSty+lcCount,'StyleUpc')
          *C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty[Begin]
          *IF !SEEK(PADR(StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,16)+;
          *    STR(nPltNo,4)+STR(No_Cart,4),lcTmpPackLine)
          IF !SEEK(PADR(StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,29)+;
              STR(nPltNo,4)+STR(No_Cart,4),lcTmpPackLine)
            *C123073,1 [End]
            INSERT INTO (lcTmpPackLine) (nPltNo,No_Cart,SKU) VALUES ;
              (Pack_Lin.nPltNo,Pack_Lin.No_Cart,;
              StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3)
          ENDIF

          *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [Begin]
          lnShipQty  = 0
          lcOrdLinNo = STR(OrdLine.LINENO,6)
          IF SEEK(Pack_Hdr.ORDER + lcOrdLinNo , "InvLine" , "INVLINEO")
            SELECT INVLINE
            SUM Qty&lcCount WHILE ORDER + STR(LINENO,6) = Pack_Hdr.ORDER + lcOrdLinNo TO lnShipQty
            SELECT Pack_Lin
          ENDIF
          *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [End]

          *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold the shipped
          *             quantity, a new field "lAltStyle" to know if any alternative
          *             styles were used and a new field "nOpenQty" to hold the Open
          *             quantity [Begin]
          *REPLACE TotQty WITH TotQty + Pack_Lin.Qty&lcCount ,;
          *        OrgQty WITH OrgQty + OrdLine.Book&lcCount ,;
          *        Weight WITH Weight + Pack_Lin.Weight/Pack_Lin.TotQty*Pack_Lin.Qty&lcCount IN (lcTmpPackLine)
          REPLACE TotQTY    WITH TotQTY   + Pack_Lin.Qty&lcCount ,;
            OrgQty    WITH OrgQty   + OrdLine.Book&lcCount ,;
            nShipQty  WITH nShipQty + lnShipQty            ,;
            nOpenQty  WITH nOpenQty + OrdLine.Qty&lcCount  ,;
            WEIGHT    WITH WEIGHT   + Pack_Lin.WEIGHT/Pack_Lin.TotQTY*Pack_Lin.Qty&lcCount,;
            lAltStyle WITH IIF(OrdLine.STYLE <> Pack_Lin.STYLE , .T. , lAltStyle) IN (lcTmpPackLine)
          *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold... [End]
        ENDIF
      ENDFOR
      LOOP
    ENDIF
&&H
    *B606328,1 Use New Index Hassan [Begin]
    *SPCK_LINST
    *type+account+style+cpkcolor+cpcksize+pack_id+cpkversion
    * IF !EMPTY(OrdLine.Pack_Id) .AND. ;
    *  ( SEEK('P'+lcAccount+STYLE+OrdLine.Pack_Id+OrdLine.cPkColor+OrdLine.cPckSize,'SPCK_LIN') OR ;
    *   SEEK('P'+'*****'+STYLE+OrdLine.Pack_Id+OrdLine.cPkColor+OrdLine.cPckSize,'SPCK_LIN') )
    *C200522,1 HBG 07/04/2003 Fix bug of not seeking Correctly in Spck_Lin and not checking if this
    *C200522,1                Pack is Range or Price per Piece[Begin]
    lcPrvLnOrd = ORDER('SPCK_LIN')
    SET ORDER TO SPCK_LINST IN SPCK_LIN
    *IF !EMPTY(OrdLine.Pack_id) .AND. ;
    *    ( SEEK('P'+lcAccount+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') OR ;
    *      SEEK('P'+'*****'+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') )
    *HBG 04/06/2003 Remove the check of lPckPrPiec [Begin]
    *IF !EMPTY(OrdLine.Pack_id) .AND. ;
    *    (SEEK('P'+lcAccount+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') OR ;
    *    SEEK('P'+'*****'+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') ) AND;
    *    !OrdLine.lPckPrPiec AND !OrdLine.lRange
    IF !EMPTY(OrdLine.Pack_id) .AND. ;
        (SEEK('P'+lcAccount+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') OR ;
        SEEK('P'+'*****'+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST'))
      *HBG [End]
      SET ORDER TO &lcPrvLnOrd IN SPCK_LIN
      *C200522,1 [End]
      *B606328,1 Use New Index Hassan [End  ]

      *HBG 04/06/2003 Fix bug of not sending the SKU [Begin]
      *IF !SEEK(OrdLine.Pack_id+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
      *  *C200404,1 Pick SKU for the pack instead of pack_ID HASSAN 09/04/2002[Begin]
      *  *INSERT INTO (lcTmpPackLine) ;
      *  *(nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
      *  *(Pack_Lin.nPltNo,Pack_Lin.No_Cart,OrdLine.Pack_Id,Pack_Lin.nOrdLineNo)

      *  *type+account+style+cpkcolor+cpcksize+pack_id+cpkversion
      *  IF SEEK('S'+lcAccount+OrdLine.Pack_id+SPACE(3)+OrdLine.cPkColor+OrdLine.cPckSize,'SPCK_LIN','SPCK_LINST')
      *    INSERT INTO (lcTmpPackLine) ;
      *      (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
      *      (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.Pack_id,Pack_Lin.nOrdLineNo)
      *
      *    = SEEK('P'+lcAccount+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') OR ;
      *      SEEK('P'+'*****'+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST')
      *
      *  ELSE
      *    INSERT INTO (lcTmpPackLine) ;
      *      (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
      *      (Pack_Lin.nPltNo,Pack_Lin.No_Cart,OrdLine.Pack_id,Pack_Lin.nOrdLineNo)
      *  ENDIF
      *  *C200404,1 Pick SKU for the pack instead of pack_ID HASSAN 09/04/2002[End  ]
      *ENDIF
      *HBG 07/09/2003 Fix bug of not sending UPC [Begin]
      lcPrvOrd = ORDER('STYLEUPC')
      SET ORDER TO GMAPKUPC IN StyleUpc
      lcUpc = IIF(llUPCUse .AND. ;
        (SEEK(lcAccount+PADR(OrdLine.Pack_id,19)+OrdLine.cPkColor+OrdLine.cPckSize,'StyleUpc','Gmapkupc') OR;
        SEEK('*****'+PADR(OrdLine.Pack_id,19)+OrdLine.cPkColor+OrdLine.cPckSize,'StyleUpc','Gmapkupc')),;
        StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,SPACE(13))
      SET ORDER TO &lcPrvOrd IN StyleUpc
      *HBG [End]
      IF SEEK('S'+lcAccount+OrdLine.Pack_id+SPACE(3)+OrdLine.cPkColor+OrdLine.cPckSize,'SPCK_LIN','SPCK_LINST')
        *C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty[Begin]
        *IF !SEEK(SPCK_LIN.Pack_id+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
        IF !SEEK(PADR(SPCK_LIN.Pack_id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,29)+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
          *C123073,1 [End]
          *HBG 07/09/2003 Fix bug of not sending UPC [Begin]
          *INSERT INTO (lcTmpPackLine) ;
          *  (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
          *  (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.PACK_ID,Pack_Lin.nOrdLineNo)
          *C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty[Begin]
          *INSERT INTO (lcTmpPackLine) ;
          *  (nPltNo,No_Cart,SKU,nOrdLineNo,cUpc) VALUES ;
          *  (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.PACK_ID,Pack_Lin.nOrdLineNo,lcUpc)
          INSERT INTO (lcTmpPackLine) ;
            (nPltNo,No_Cart,SKU,nOrdLineNo,cUpc) VALUES ;
            (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.Pack_id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,Pack_Lin.nOrdLineNo,lcUpc)
          *C123073,1 [End]
          *HBG [END]
        ENDIF
        = SEEK('P'+lcAccount+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') OR ;
          SEEK('P'+'*****'+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST')
      ELSE
        *C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty[Begin]
        *IF !SEEK(OrdLine.Pack_id+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
        IF !SEEK(PADR(OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize,29)+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
          *C123073,1 [End]
          *HBG 07/09/2003 Fix bug of not sending UPC [Begin]
          *INSERT INTO (lcTmpPackLine) ;
          *  (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
          *  (Pack_Lin.nPltNo,Pack_Lin.No_Cart,OrdLine.Pack_id,Pack_Lin.nOrdLineNo)
          *C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty[Begin]
          *INSERT INTO (lcTmpPackLine) ;
          *  (nPltNo,No_Cart,SKU,nOrdLineNo,cUpc) VALUES ;
          *  (Pack_Lin.nPltNo,Pack_Lin.No_Cart,OrdLine.Pack_id,Pack_Lin.nOrdLineNo,lcUPC)
          INSERT INTO (lcTmpPackLine) ;
            (nPltNo,No_Cart,SKU,nOrdLineNo,cUpc) VALUES ;
            (Pack_Lin.nPltNo,Pack_Lin.No_Cart,OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize,Pack_Lin.nOrdLineNo,lcUpc)
          *C123073,1 [End]
          *HBG [End]

        ENDIF

      ENDIF
      *HBG [End]

      *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [Begin]
      lnShipQty  = 0
      lcOrdLinNo = STR(OrdLine.LINENO,6)
      IF SEEK(Pack_Hdr.ORDER + lcOrdLinNo , "InvLine" , "INVLINEO")
        SELECT INVLINE
        SUM TotQTY WHILE ORDER + STR(LINENO,6) = Pack_Hdr.ORDER + lcOrdLinNo TO lnShipQty
        SELECT Pack_Lin
      ENDIF
      *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [End]

      *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold the shipped
      *             quantity, a new field "lAltStyle" to know if any alternative
      *             styles were used and a new field "nOpenQty" to hold the Open
      *             quantity [Begin]
      *REPLACE TotQty with TotQty + Pack_Lin.TotQty ,;
      *        OrgQty with OrgQty + Ordline.TotBook ,;
      *        Weight WITH Weight + Pack_Lin.Weight IN (lcTmpPackLine)
      REPLACE TotQTY    WITH TotQTY   + Pack_Lin.TotQTY ,;
        OrgQty    WITH OrgQty   + OrdLine.TotBook ,;
        nShipQty  WITH nShipQty + lnShipQty       ,;
        nOpenQty  WITH nOpenQty + OrdLine.TotQTY  ,;
        WEIGHT    WITH WEIGHT   + Pack_Lin.WEIGHT ,;
        lAltStyle WITH IIF(OrdLine.STYLE <> Pack_Lin.STYLE , .T. , lAltStyle) IN (lcTmpPackLine)
      *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold... [End]
    ELSE
      llPerSize = .F.
      *C200180,1 Get Cathey Danial account style
      lcUpcStyle = Pack_Lin.STYLE
      *C200180,1 (End)
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        IF Pack_Lin.Qty&lcCount > 0
          lcUpc = IIF(llUPCUse .AND. ;
            SEEK(lcUpcStyle+lcCount,'StyleUpc','StyleUpc'),;
            StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,SPACE(13))
          SELECT SPCK_LIN
          =SEEK('S'+lcAccount+Pack_Lin.STYLE)
          LOCATE REST ;
            WHILE TYPE+Account+STYLE+Pack_id = 'S'+lcAccount+Pack_Lin.STYLE ;
            FOR   (Qty&lcCount = 1 OR TotQTY = 0) AND oObject.OrgSku(ALLTRIM(Pack_id))
          *C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty[Begin]
          *lcSku = IIF(FOUND(),Pack_id,lcUpc)
          lcSku = IIF(FOUND(),PADR(Pack_id+cPkColor+cPckSize,29),PADR(lcUpc,29))
          *C123073,1 [End]
          IF !EMPTY(lcSku)
            llPerSize = .T.
            IF !SEEK(lcSku+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
              INSERT INTO (lcTmpPackLine) ;
                (nPltNo,No_Cart,SKU,nOrdLineNo,cUpc) ;
                VALUES (Pack_Lin.nPltNo,Pack_Lin.No_Cart,lcSku,Pack_Lin.nOrdLineNo,lcUpc)
            ENDIF

            *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [Begin]
            lnShipQty  = 0
            lcOrdLinNo = STR(OrdLine.LINENO,6)
            IF SEEK(Pack_Hdr.ORDER + lcOrdLinNo , "InvLine" , "INVLINEO")
              SELECT INVLINE
              SUM Qty&lcCount WHILE ORDER + STR(LINENO,6) = Pack_Hdr.ORDER + lcOrdLinNo TO lnShipQty
            ENDIF
            *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [End]

            SELECT (lcTmpPackLine)
            *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold the shipped
            *             quantity, a new field "lAltStyle" to know if any alternative
            *             styles were used and a new field "nOpenQty" to hold the Open
            *             quantity [Begin]
            *REPLACE TotQty WITH TotQty + Pack_Lin.Qty&lcCount ,;
            *        OrgQty WITH OrgQty + OrdLine.Book&lcCount ,;
            *        Weight WITH Weight + Pack_Lin.Weight/Pack_Lin.TotQty*Pack_Lin.Qty&lcCount
            REPLACE TotQTY    WITH TotQTY   + Pack_Lin.Qty&lcCount ,;
              OrgQty    WITH OrgQty   + OrdLine.Book&lcCount ,;
              nShipQty  WITH nShipQty + lnShipQty            ,;
              nOpenQty  WITH nOpenQty + OrdLine.Qty&lcCount  ,;
              WEIGHT    WITH WEIGHT   + Pack_Lin.WEIGHT/Pack_Lin.TotQTY*Pack_Lin.Qty&lcCount,;
              lAltStyle WITH IIF(OrdLine.STYLE <> Pack_Lin.STYLE , .T. , lAltStyle)
            *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold... [End]
          ENDIF
        ENDIF
      ENDFOR
      llFound = llPerSize
&&H
      IF !llPerSize
        *B803743,1 Wael  Fix Variable lcKey not Found
        *IF SEEK('P'+lcKey) OR SEEK('P'+'*****'+Style)
        IF SEEK('P'+lcAccount+Pack_Lin.STYLE,'Spck_Lin') OR SEEK('P'+'*****'+Pack_Lin.STYLE,'Spck_Lin')
          *B803743,1 Wael  (End)
          *C123073,1 HBG 05/27/2004 Fix bug of Adding the 2 packs into 1 line and sum the Qty[Begin]
          *IF !SEEK(SPCK_LIN.Pack_id+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
          *  INSERT INTO (lcTmpPackLine) ;
          *    (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
          *    (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.Pack_id,Pack_Lin.nOrdLineNo)
          IF !SEEK(PADR(SPCK_LIN.Pack_id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,29)+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
            INSERT INTO (lcTmpPackLine) ;
              (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
              (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.Pack_id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,Pack_Lin.nOrdLineNo)
          ENDIF
          *C123073,1 [End]
          *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [Begin]
          lnShipQty  = 0
          lcOrdLinNo = STR(OrdLine.LINENO,6)
          IF SEEK(Pack_Hdr.ORDER + lcOrdLinNo , "InvLine" , "INVLINEO")
            SELECT INVLINE
            SUM TotQTY WHILE ORDER + STR(LINENO,6) = Pack_Hdr.ORDER + lcOrdLinNo TO lnShipQty
          ENDIF
          *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [End]

          *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold the shipped
          *             quantity, a new field "lAltStyle" to know if any alternative
          *             styles were used and a new field "nOpenQty" to hold the Open
          *             quantity [Begin]
          *REPLACE TotQty with TotQty + Pack_Lin.TotQty ,;
          *        OrgQty with OrgQty + Ordline.TotBook ,;
          *        Weight WITH Weight + Pack_Lin.Weight IN (lcTmpPackLine)
          REPLACE TotQTY    WITH TotQTY   + Pack_Lin.TotQTY ,;
            OrgQty    WITH OrgQty   + OrdLine.TotBook ,;
            nShipQty  WITH nShipQty + lnShipQty       ,;
            nOpenQty  WITH nOpenQty + OrdLine.TotQTY  ,;
            WEIGHT    WITH WEIGHT   + Pack_Lin.WEIGHT ,;
            lAltStyle WITH IIF(OrdLine.STYLE <> Pack_Lin.STYLE , .T. , lAltStyle) IN (lcTmpPackLine)
          *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold... [End]
          llFound = .T.
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  SET ORDER TO TAG SPCK_LIN IN SPCK_LIN
  SELECT (lnAlias)
  *:****************************************************************************************
  *! Name      : lfAsnDoA
  *! Developer : Hassan Ali (HIA)
  *! Date      : 05/07/2002
  *! Purpose   : Process ASN Cartons For GMA
  *:****************************************************************************************
  *! Called from : Class "Send856"
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfAsnDoA
  SELECT SPCK_LIN
  ** OrdLine.cPkColor,cPckSize,cPkVersion
  *SPCK_LINVR
  *Type+account+pack_id+cpkcolor+cpcksize+cpkversion+style
  *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
  *lcTest = OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
  lcTest = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
  *HBG 21/08/2003 [End]
  *B606328,1 Use New Index Hassan [Begin]
  SET ORDER TO Spck_linvr
  *B606328,1 Use New Index Hassan [End  ]
  *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
  *IF SEEK('P'+MACCOUNT+lcItem+lcTest) OR SEEK('P'+'*****'+lcItem+lcTest)
  IF SEEK('P'+MACCOUNT+lcTest) OR SEEK('P'+'*****'+lcTest)
    *HBG 21/08/2003 [End]

    MUOM = 'PK'
    *C200592,1 HBG 17/08/2003 Get the value of the flag Send using Assortment Pack [Begin]
    =SEEK(EdiAcPrt.cPartCode,'EDIPH','PARTNER')
    MLASSORT = (EdiPh.cSnPkD856 = 'Y')
    *C200592,1 [End]

    MQtyPPack = 0
    lcPackAcc = Account
    *B606328,1 Use New Index Hassan [Begin]
    *Type+account+pack_id+cpkcolor+cpcksize+cpkversion
    *=SEEK('P'+lcPackAcc+PADR(LEFT(lcItem,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr')
    *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
    *=SEEK('P'+lcPackAcc+PADR(LEFT(lcItem,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr','SPCK_HDRVR')
    =SEEK('P'+lcPackAcc+lcTest,'Spck_Hdr','SPCK_HDRVR')
    *HBG 21/08/2003 [End]
    *B606328,1 Use New Index Hassan [End  ]
    MSTYDESC = Spck_HDR.DESC
    *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
    *SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P'+lcPackAcc+lcItem+lcTest
    SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P'+lcPackAcc+lcTest
      *HBG 21/08/2003 [End]
      MQtyPPack = MQtyPPack + TotQTY
      MPO401    = TotQTY
      =SEEK(STYLE,'Style')
      IF INLIST(lcPriLevel,'A','B','C')
        MPri = MPri + STYLE.PRICE&lcPriLevel * TotQTY
      ENDIF
    ENDSCAN
    *HBG 21/08/2003 Fix bug of not sending UPC [Begin]
    *IF llUPCUse AND SEEK(lcItem,'StyleUpc') AND StyleUpc.lUPCPack
    SET ORDER TO GMAPKUPC IN StyleUpc
    IF llUPCUse AND (SEEK(lcPackAcc+PADR(OrdLine.Pack_id,19)+OrdLine.cPkColor+OrdLine.cPckSize,'StyleUpc') OR;
        SEEK('*****'+PADR(OrdLine.Pack_id,19)+OrdLine.cPkColor+OrdLine.cPckSize,'StyleUpc')) AND StyleUpc.lUPCPack
      *HBG 21/08/2003 [End]
      MUPC = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
    ENDIF
    *MInPckPCrt = &lcTmpPackLine..TotQty
    *MQtyPCrt   = MQtyPPack
    IF (!EMPTY(MUPC) AND llGMA) OR (INT(MQTY/MQtyPPack) <> MQTY/MQtyPPack)
      MQtyPPack  = 1
      MInPckPCrt = 1                   && # Inner packs per carton
      MQtyPCrt   = &lcTmpPackLine..TotQTY && Quantity per carton
    ELSE
      MQTY       = MQTY/MQtyPPack
      MORGQTY    = MORGQTY/MQtyPPack
      MInPckPCrt = &lcTmpPackLine..TotQTY
      MQtyPCrt   = MQtyPPack
    ENDIF
  ELSE
    IF SEEK('S'+MACCOUNT+lcItem+lcTest)
      *B606328,1 Use New Index Hassan [Begin]
      *=SEEK('S'+MACCOUNT+PADR(LEFT(lcItem,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr')
      =SEEK('S'+MACCOUNT+PADR(LEFT(lcItem,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr','SPCK_HDRVR')
      *B606328,1 Use New Index Hassan [End  ]
      MSTYDESC = Spck_HDR.DESC
      =SEEK(STYLE,'Style')
      IF INLIST(lcPriLevel,'A','B','C')
        MPri = STYLE.PRICE&lcPriLevel
      ENDIF
      IF TotQTY = 1
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          IF Qty&lcCount = 1
            lcSize  = lcCount
            EXIT
          ENDIF
        ENDFOR
      ELSE
        =SEEK('P'+STYLE.SCALE+OrdLine.PrePak,'Scale','Scale')
        XPPTOT = SCALE.PPTOT
        =SEEK('S'+STYLE.SCALE,'Scale','Scale')
      ENDIF
      MUPC   = &lcTmpPackLine..cUpc
      MStyle = SUBSTR(STYLE,1,LEN(lcMjrPic))
      MCOLOR = SUBSTR(STYLE,lnClrPos,lnCLrLen)
      IF SEEK(STYLE,'Style') ;
          .AND. !EMPTY(lcSize) .AND. SEEK('S'+STYLE.SCALE,'SCALE')
        MSIZE = SCALE.SZ&lcSize
      ENDIF
      MUOM = 'EA'
      *C200592,1 HBG 17/08/2003 Get the value of the flag Send using Assortment Pack [Begin]
      MLASSORT = .F.
      *C200592,1 HBG 17/08/2003 [End]
      MQtyPPack  = 1
      MPO401     = 1
      MInPckPCrt = 1                      && # Inner packs per carton
      MQtyPCrt   = &lcTmpPackLine..TotQTY && Quantity per carton
    ELSE
      IF llUPCUse AND SEEK(ALLTRIM(lcItem),'StyleUpc','StyUpcN')
        MUPC  = lcItem
        MSKU  = ''
        *C200180,1 Get Cathey Danial account style
        lcUpcStyle = StyleUpc.STYLE
        IF llCatDanial
          lcUpcStyle = SUBSTR(StyleUpc.STYLE,1,LEN(lcMjrPic))
          lcUpcStyle = PADR(ALLTRIM(lcUpcStyle)+EdiAcPrt.cAccCode,LEN(lcMjrPic))
          lcUpcStyle = lcUpcStyle + SUBSTR(StyleUpc.STYLE,LEN(lcMjrPic)+1)
          lcUpcStyle = IIF(SEEK(lcUpcStyle,'STYLE'),lcUpcStyle,StyleUpc.STYLE)
        ENDIF
        *MSTYLE = SUBSTR(StyleUpc.Style,1,LEN(lcMjrPic))
        *MColor = SUBSTR(StyleUpc.Style,lnClrPos,lnClrLen)
        *=SEEK(StyleUpc.Style,'Style')
        MStyle = SUBSTR(lcUpcStyle,1,LEN(lcMjrPic))
        MCOLOR = SUBSTR(lcUpcStyle,lnClrPos,lnCLrLen)
        =SEEK(lcUpcStyle,'Style')
        *C200180,1 (End)
        IF INLIST(lcPriLevel,'A','B','C')
          MPri = STYLE.PRICE&lcPriLevel
        ENDIF
        MSTYDESC = STYLE.DESC
        =SEEK('S'+STYLE.SCALE,'SCALE')
        lcSize = ALLTRIM(StyleUpc.SIZE)
        MSIZE  = IIF(EMPTY(lcSize),'',SCALE.SZ&lcSize)
        MUOM      = 'EA'
        *C200592,1 HBG 17/08/2003 Get the value of the flag Send using Assortment Pack [Begin]
        MLASSORT = .F.
        *C200592,1 HBG 17/08/2003 [End]
        MQtyPPack = 1
        MPO401    = 1
        MInPckPCrt= 1                      && # Inner packs per carton
        MQtyPCrt  = &lcTmpPackLine..TotQTY && Quantity per carton
      ENDIF
    ENDIF
  ENDIF
  *B606328,1 Use New Index Hassan [Begin]
  SELECT SPCK_LIN
  SET ORDER TO SPCK_LIN
  *B606328,1 Use New Index Hassan [End]

ENDFUN
*:****************************************************************************************
*! Name      : lfASNOPT
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process ASN Cartons For GMA
*:****************************************************************************************
*! Called from : Class "Send856"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfASNOPT
  lcAlias =SELECT ()
  SELECT Spck_HDR
  SET ORDER TO Spck_HdrVr
  SELECT SPCK_LIN
  SET ORDER TO Spck_linvr
  SELECT(lcAlias)
ENDFUN
*:****************************************************************************************
*! Name      : lfASNDOB
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process ASN Cartons For GMA
*:****************************************************************************************
*! Called from : Class "Send856"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfASNDOB
  =SEEK('O'+lcOrder+STR(&lcTmpPackLine..nOrdLineNo,6),'ORDLINE','ORDLINE')
  *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
  *lcTest = OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
  *IF SEEK('P'+MACCOUNT+PADR(&lcTmpPackLine..SKU,16)+lcTest) OR ;
  *   SEEK('P'+'*****'+PADR(&lcTmpPackLine..SKU,16)+lcTest)
  lcTest = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
  IF SEEK('P'+MACCOUNT+lcTest) OR SEEK('P'+'*****'+lcTest)
    *HBG 21/08/2003 [End]
    lcPackAcc = Account
    *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
    *SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = ;
    *    'P'+lcPackAcc+PADR(&lcTmpPackLine..SKU,16)+lcTest
    SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = ;
        'P'+lcPackAcc+lcTest
      *HBG 21/08/2003 [End]
      =SEEK(STYLE,'Style')
      MQtyPPack  = MQtyPPack + TotQTY
      MWghtPPack = MWghtPPack + TotQTY*STYLE.nstyweight
    ENDSCAN
  ENDIF
ENDFUN
*:****************************************************************************************
*! Name      : lfASNDOC
*! Developer : Hassan Ali (HIA)
*! Date      : 05/07/2002
*! Purpose   : Process ASN Cartons For GMA
*:****************************************************************************************
*! Called from : Class "Send856"
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
FUNCTION lfASNDOC
  *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
  *lcTest = OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
  lcTest = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
  *HBG 21/08/2003 [End]
  *type+account+pack_id+cpkcolor+cpcksize+cpkversion
  SELECT SPCK_LIN
  *B606328,1 Use New Index Hassan [Begin]
  SET ORDER TO Spck_linvr
  *B606328,1 Use New Index Hassan [End]
  *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
  *IF SEEK('P'+MACCOUNT+PADR(MSKU,16)+lcTest) OR SEEK('P'+'*****'+PADR(MSKU,16)+lcTest)
  IF SEEK('P'+MACCOUNT+lcTest) OR SEEK('P'+'*****'+lcTest)
    *HBG 21/08/2003 [End]
    MUOM = 'PK'
    *C200592,1 HBG 17/08/2003 Get the value of the flag Send using Assortment Pack [Begin]
    =SEEK(EdiAcPrt.cPartCode,'EDIPH','PARTNER')
    MLASSORT = (EdiPh.cSnPkD856 = 'Y')
    *C200592,1 [End]
    MQtyPPack = 0
    lcPackAcc = Account
    *B606328,1 Use New Index Hassan [Begin]
    *=SEEK('P'+lcPackAcc+PADR(LEFT(MSKU,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr')
    *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
    *=SEEK('P'+lcPackAcc+PADR(LEFT(MSKU,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr','SPCK_HDRVR')
    =SEEK('P'+lcPackAcc+lcTest,'Spck_Hdr','SPCK_HDRVR')
    *HBG 21/08/2003 [End]
    *B606328,1 Use New Index Hassan [End]
    MSTYDESC = Spck_HDR.DESC
    *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
    *SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P'+lcPackAcc+PADR(MSKU,16)+lcTest
    SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P'+lcPackAcc+lcTest
      *HBG 21/08/2003 [End]
      MQtyPPack = MQtyPPack + TotQTY
      MPO401 = TotQTY
      =SEEK(STYLE,'Style')
      IF INLIST(lcPriLevel,'A','B','C')
        MPri = MPri + STYLE.PRICE&lcPriLevel * TotQTY
      ENDIF
    ENDSCAN
    *HBG 21/08/2003 Fix bug of not sending SKU [Begin]
    *IF llUPCUse AND SEEK(PADR(MSKU,13),'StyleUpc') AND StyleUpc.lUPCPack
    SET ORDER TO GMAPKUPC IN StyleUpc
    IF llUPCUse AND (SEEK(lcPackAcc+PADR(OrdLine.Pack_id,19)+OrdLine.cPkColor+OrdLine.cPckSize,'StyleUpc') OR;
        SEEK('*****'+PADR(OrdLine.Pack_id,19)+OrdLine.cPkColor+OrdLine.cPckSize,'StyleUpc')) AND StyleUpc.lUPCPack
      *HBG 21/08/2003 [End]
      MUPC = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
    ENDIF
    *MInPckPCrt = &lcTmpPackLine..TotQty
    *MQtyPCrt   = MQtyPPack
    *lnQtyPCrtn = lnQtyPCrtn + MQtyPPack*MInPckPCrt
    IF (!EMPTY(MUPC) AND llGMA) OR (INT(MQTY/MQtyPPack) <> MQTY/MQtyPPack)
      MQtyPPack  = 1
      MInPckPCrt = 1
      MPO401     = 1
      MQtyPCrt   = &lcTmpPackLine..TotQTY
      lnQtyPCrtn = lnQtyPCrtn + MQtyPCrt
    ELSE
      lnQtyPCrtn = lnQtyPCrtn + &lcTmpPackLine..TotQTY
      MQTY = MQTY/MQtyPPack
      MORGQTY = MORGQTY/MQtyPPack
      MInPckPCrt = &lcTmpPackLine..TotQTY
      MQtyPCrt   = MQtyPPack
    ENDIF
  ELSE
    IF SEEK('S'+MACCOUNT+PADR(MSKU,16))
      =SEEK('S'+MACCOUNT+PADR(LEFT(MSKU,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16),'Spck_Hdr')
      MSTYDESC = Spck_HDR.DESC
      =SEEK(STYLE,'Style')
      IF INLIST(lcPriLevel,'A','B','C')
        MPri = STYLE.PRICE&lcPriLevel
      ENDIF
      IF TotQTY = 1
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          IF Qty&lcCount = 1
            lcSize = lcCount
            EXIT
          ENDIF
        ENDFOR
      ELSE
        =SEEK('P'+STYLE.SCALE+OrdLine.PrePak,'Scale','Scale')
        XPPTOT = SCALE.PPTOT
        =SEEK('S'+STYLE.SCALE,'Scale','Scale')
      ENDIF
      MUPC   = &lcTmpPackLine..cUpc
      MStyle = SUBSTR(STYLE,1,LEN(lcMjrPic))
      MCOLOR = SUBSTR(STYLE,lnClrPos,lnCLrLen)
      IF SEEK(STYLE,'Style') AND !EMPTY(lcSize) AND SEEK('S'+STYLE.SCALE,'SCALE')
        MSIZE = SCALE.SZ&lcSize
      ENDIF
      MUOM = 'EA'
      *C200592,1 HBG 17/08/2003 Get the value of the flag Send using Assortment Pack [Begin]
      MLASSORT = .F.
      *C200592,1 HBG 17/08/2003 [End]
      MQtyPPack  = 1
      MInPckPCrt = 1
      MPO401     = 1
      MQtyPCrt   = &lcTmpPackLine..TotQTY
      lnQtyPCrtn = lnQtyPCrtn + MQtyPCrt
    ELSE
      IF llUPCUse .AND. SEEK(PADR(MSKU,13),'StyleUpc','StyUpcN')
        MUPC   = PADR(MSKU,13)
        MSKU   = ''
        *C200180,1 Get Cathey Danial account style
        lcUpcStyle = StyleUpc.STYLE
        IF llCatDanial
          lcUpcStyle = SUBSTR(StyleUpc.STYLE,1,LEN(lcMjrPic))
          lcUpcStyle = PADR(ALLTRIM(lcUpcStyle)+EdiAcPrt.cAccCode,LEN(lcMjrPic))
          lcUpcStyle = lcUpcStyle + SUBSTR(StyleUpc.STYLE,LEN(lcMjrPic)+1)
          lcUpcStyle = IIF(SEEK(lcUpcStyle,'STYLE'),lcUpcStyle,StyleUpc.STYLE)
        ENDIF
        *MSTYLE = SUBSTR(StyleUpc.Style,1,LEN(lcMjrPic))
        *MColor = SUBSTR(StyleUpc.Style,lnClrPos,lnClrLen)
        *=SEEK(StyleUpc.Style,'Style')
        MStyle = SUBSTR(lcUpcStyle,1,LEN(lcMjrPic))
        MCOLOR = SUBSTR(lcUpcStyle,lnClrPos,lnCLrLen)
        =SEEK(lcUpcStyle,'Style')
        *C200180,1 (End)
        IF INLIST(lcPriLevel,'A','B','C')
          MPri = STYLE.PRICE&lcPriLevel
        ENDIF
        MSTYDESC = STYLE.DESC
        =SEEK('S'+STYLE.SCALE,'SCALE')
        lcSize = ALLTRIM(StyleUpc.SIZE)
        MSIZE  = IIF(EMPTY(lcSize),'',SCALE.SZ&lcSize)
        MUOM   = 'EA'
        *C200592,1 HBG 17/08/2003 Get the value of the flag Send using Assortment Pack [Begin]
        MLASSORT = .F.
        *C200592,1 HBG 17/08/2003 [End]

        MQtyPPack  = 1
        MInPckPCrt = 1
        MPO401     = 1
        MQtyPCrt   = &lcTmpPackLine..TotQTY
        lcCrtnSku= ''
        lnQtyPCrtn = lnQtyPCrtn + MQtyPCrt
      ENDIF
    ENDIF
  ENDIF
  *B606328,1 Use New Index Hassan [Begin]
  SELECT SPCK_LIN
  SET ORDER TO SPCK_LIN
  *B606328,1 Use New Index Hassan [End]


ENDFUN
*:****************************************************************************************
*! Name      : lfOrgSku
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/16/2002
*! Purpose   : Get sku
*:****************************************************************************************
*! Called from : This program
*:****************************************************************************************
*! Calls       :
*:****************************************************************************************
*! Return      : None
*:****************************************************************************************
*--C200215,5
FUNCTION lfOrgSku
  LPARAMETERS lcSku
  RETURN(ATCLINE('SKU~',OrdLine.NOTE_MEM)=0 OR ;
    ATCLINE('SKU~'+lcSku,OrdLine.NOTE_MEM)>0)

  *:****************************************************************************************
  *! Name      : lfGetLogFac
  *! Developer : Adel Mohammed El Gazzar (ADEL)
  *! Date      : 05/16/2002
  *! Purpose   : Return distribution center
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfGetLogFac
  LPARAMETERS lcAccount,lcInvoice
  PRIVATE lnAlias,lcDistCntr,lnCusRecNo

  lcDistCntr = SPACE(8)
  lnAlias    = SELECT()
  lnCusRecNo = RECNO('Customer')
  SELECT ConsInvH
  IF SEEK(lcInvoice) .AND. SEEK('S'+lcAccount+STORE,'Customer') .AND. ;
      SEEK('S'+lcAccount+Customer.Dist_Ctr,'Customer')
    lcDistCntr = Customer.STORE
    SCAN REST WHILE Invoice = lcInvoice
      IF !SEEK('S'+lcAccount+STORE,'Customer') OR ;
          !SEEK('S'+lcAccount+Customer.Dist_Ctr,'Customer') OR Customer.STORE <> lcDistCntr
        lcDistCntr = SPACE(8)
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  IF BETWEEN(lnCusRecNo,1,RECCOUNT('Customer'))
    GOTO lnCusRecNo IN 'Customer'
  ENDIF
  SELECT(lnAlias)
  RETURN(lcDistCntr)

  *!**************************************************************************


  *:****************************************************************************************
  *! Name      : lfCrtTmp
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 09/15/2002
  *! Purpose   : Create temp file
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfCrtTmp

  IF FILE(oAriaApplication.WorkDir+lcTmpLines+'.DBF')
    USE IN (lcTmpLines)
    ERASE (oAriaApplication.WorkDir+lcTmpLines+'.*')
  ENDIF

  *B038247,1  SSE 06/29/2004 Add the cSizes field to lcTmpLines. [Begin]
  *CREATE TABLE (oAriaApplication.WorkDir+lcTmpLines) ;
  *  (CustPO C(15), ORDER C(6), STORE C(8), cWareCode C(6),STYLE C(19),Pack_id C(16), PRICE N(9,2),;
  *  Edi_Pk_amt N(12,10),Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
  *  Qty8 N(6),TotQTY N(7) ,Ord1 N(6),Ord2 N(6),Ord3 N(6),Ord4 N(6),Ord5 N(6),;
  *  Ord6 N(6),Ord7 N(6),Ord8 N(6),TotOrd N(7),cShpStatus C(2),DATE D,Reason C(6),;
  *  cPkColor C(6),cPckSize C(3),cPkVersion C(4))
  *B122620,1 SSE  04/22/2004 Add the line no. [Begin]
  *!*	  CREATE TABLE (oAriaApplication.WorkDir+lcTmpLines) ;
  *!*	    (CustPO C(15), ORDER C(6), STORE C(8), cWareCode C(6),STYLE C(19),Pack_id C(16), PRICE N(9,2),;
  *!*	    Edi_Pk_amt N(12,10),Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
  *!*	    Qty8 N(6),TotQTY N(7) ,Ord1 N(6),Ord2 N(6),Ord3 N(6),Ord4 N(6),Ord5 N(6),;
  *!*	    Ord6 N(6),Ord7 N(6),Ord8 N(6),TotOrd N(7),cShpStatus C(2),DATE D,Reason C(6),cSizes C(8),;
  *!*	    cPkColor C(6),cPckSize C(3),cPkVersion C(4))

  CREATE TABLE (oAriaApplication.WorkDir+lcTmpLines) ;
    (CustPO C(15),  ORDER C(6), STORE C(8), cWareCode C(6),STYLE C(19),Pack_id C(16), PRICE N(9,2),;
    Edi_Pk_amt N(12,10),Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
    Qty8 N(6),TotQTY N(7) ,Ord1 N(6),Ord2 N(6),Ord3 N(6),Ord4 N(6),Ord5 N(6),;
    Ord6 N(6),Ord7 N(6),Ord8 N(6),TotOrd N(7),cShpStatus C(2),DATE D,Reason C(6),cSizes C(8),;
    cPkColor C(6),cPckSize C(3),cPkVersion C(4),LINENO N(6))
  *B122620,1 SSE 04/22/2004 Add the line no. [End]
  *B038247,1 SSE 06/29/2004 Add the cSizes field to lcTmpLines. [End]

  INDEX ON CustPO+ORDER+STORE+cWareCode+Pack_id+cPkColor+cPckSize+cPkVersion+STYLE TAG (lcTmpLines)

  *:****************************************************************************************
  *! Name      : lfGtPkInf
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 09/15/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfGtPkInf

  lcPkLnOrd = ORDER('SPCK_LIN')
  SET ORDER TO Spck_linvr IN SPCK_LIN
  lcPkhdOrd = ORDER('SPCK_HDR')
  SET ORDER TO Spck_HdrVr IN Spck_HDR
  lcSUPCOrd = ORDER('StyleUpc')
  SET ORDER TO GMAPKUPC IN StyleUpc

  SELECT SPCK_LIN
  IF SEEK('P'+MACCOUNT+&lcTmpLines..Pack_id+&lcTmpLines..cPkColor+&lcTmpLines..cPckSize+&lcTmpLines..cPkVersion) OR ;
      SEEK('P'+'*****'+&lcTmpLines..Pack_id+&lcTmpLines..cPkColor+&lcTmpLines..cPckSize+&lcTmpLines..cPkVersion)
    lcPackAcc = Account
    =SEEK('P'+lcPackAcc+PADR(LEFT(Pack_id,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)++cPkColor+cPckSize+cPkVersion,'SPCK_HDR')
    MSTYDESC  = Spck_HDR.DESC
    *B607478 TMI [START] Correct the sum rest while expression to get correct lnPackQty
    *SUM REST TotQTY TO lnPackQty ;
WHILE TYPE+Account+Pack_id++cPkColor+cPckSize+cPkVersion='P'+lcPackAcc+&lcTmpLines..Pack_id+;
&lcTmpLines..cPckSize+&lcTmpLines..cPkVersion
    SUM REST TotQTY TO lnPackQty ;
      WHILE TYPE+Account+Pack_id++cPkColor+cPckSize+cPkVersion='P'+lcPackAcc+&lcTmpLines..Pack_id+;
      &lcTmpLines..cPkColor+&lcTmpLines..cPckSize+&lcTmpLines..cPkVersion
    *B607478 TMI [END  ] Correct the sum rest while expression to get correct lnPackQty
    XPCKQTY = IIF(lnPackQty<>0, XTOTQTY/lnPackQty, XTOTQTY)
    IF INT(XPCKQTY) = XPCKQTY
      XTOTQTY = XPCKQTY
      XTOTORD = IIF(lnPackQty<>0, XTOTORD/lnPackQty, XTOTORD)
      XPRICE  = IIF(XTOTQTY<>0,&lcTmpLines..Edi_Pk_amt/XTOTQTY,XPRICE)
    ELSE
      lnPackQty = 1
    ENDIF
    IF llUPCUse .AND. SEEK(lcPackAcc+&lcTmpLines..Pack_id+&lcTmpLines..cPckSize,'StyleUpc','Gmapkupc');
        AND StyleUpc.lUPCPack
      xUpc = ALLTRIM(StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3)
    ENDIF
  ENDIF

  SET ORDER TO &lcPkLnOrd IN SPCK_LIN
  SET ORDER TO &lcPkhdOrd IN Spck_HDR
  SET ORDER TO &lcSUPCOrd IN StyleUpc

  *:****************************************************************************************
  *! Name      : lfGetSKU
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 09/15/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfGetSKU


  lcPkLnOrd = ORDER('SPCK_LIN')
  SET ORDER TO SPCK_LINST IN SPCK_LIN

  IF SEEK('S'+MACCOUNT+PADR(&lcTmpLines..Pack_id,19)+&lcTmpLines..cPkColor+&lcTmpLines..cPckSize,'SPCK_LIN') OR ;
      SEEK('S'+'*****'+PADR(&lcTmpLines..Pack_id,19)+&lcTmpLines..cPkColor+&lcTmpLines..cPckSize,'SPCK_LIN')
    XSKU = SPCK_LIN.Pack_id
  ENDIF

  SET ORDER TO &lcPkLnOrd IN SPCK_LIN

  *:****************************************************************************************
  *! Name      : lfUpdPck
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 09/15/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfUpdPck

  REPLACE &lcTmpLines..cPkColor   WITH OrdLine.cPkColor;
    &lcTmpLines..cPckSize   WITH OrdLine.cPckSize;
    &lcTmpLines..cPkVersion WITH OrdLine.cPkVersion

  *:****************************************************************************************
  *! Name      : lfChkErr
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 11/14/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : CheckError of EdiProcessPo of EdiPo.vcx
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
FUNCTION lfChkErr
  *Hassan 11/21/2002 [Begin]
  *Waleed 11/21/2002 [Begin]
  *!*    *type+account+style+pack_id
  *!*    =SEEK("S"+OrdLine.Account+OrdLine.STYLE+OrdLine.Pack_id,'spck_Hdr','SKU_Style')
  *!*    IF !Spck_HDR.LRang AND Spck_HDR.nPkSlPrice <> 0
  *!*    && Original Price
  *!*      REPLACE PRICE      WITH  Spck_HDR.nPck_Price ,;
  *!*      Gros_Price WITH lnGrsPrice ,;
  *!*      Disc_Pcnt  WITH lnDiscount
  *!*    ENDIF
  *Waleed 11/21/2002 [End  ]

  *C200479,1 HBG 8/1/2003 Use fields lRange & NPKSLPRICE from ORDLINE[Begin]
  *IF !EMPTY(OrdLine.Pack_id) AND;
  *   (SEEK('P'+OrdLine.Account+OrdLine.Pack_id+Ordline.cPkColor+Ordline.cPkSize+ORDLINE.CPKVERSION,'Spck_Hdr','Spck_hdrvr') or;
  *    SEEK('P'+"*****"+OrdLine.Pack_id+Ordline.cPkColor+Ordline.cPkSize+ORDLINE.CPKVERSION,'Spck_Hdr','Spck_hdrvr'        )) And;
  *    !(Spck_hdr.lRange) AND Spck_hdr.NPKSLPRICE <> 0
  IF !EMPTY(OrdLine.Pack_id) AND !(OrdLine.lRange) AND OrdLine.nPkSlPrice <> 0
    *C200479,1 [End]

    lMsgGma = .T.
    *type+account+pack_id+cpkcolor+cpcksize+cpkversion+style
    IF SEEK('P'+OrdLine.Account+OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPkSize+OrdLine.cPkVersion+OrdLine.STYLE,"Spck_lin","SPCK_LINVR") OR;
        SEEK('P'+"*****"+OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPkSize+OrdLine.cPkVersion+OrdLine.STYLE,"Spck_lin","SPCK_LINVR")

      REPLACE OrdLine.PRICE      WITH SPCK_LIN.nPck_Price ,;
        OrdLine.Gros_Price WITH lnGrsPrice          ,;
        OrdLine.Disc_Pcnt  WITH lnDiscount
    ENDIF
  ELSE
    lMsgGma = .F.
  ENDIF
  *Hassan 11/21/2002 [End  ]
  *:****************************************************************************************
  *! Name      : lfSetWar
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 11/19/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200442,1
FUNCTION lfSetWar

  FOR lnI = 1 TO oGMAObj.AriaForm1.cboLocations.LISTCOUNT
    IF oGMAObj.AriaForm1.cboLocations.LIST(lnI,2) = "MAIN  "
      EXIT
    ENDIF
  ENDFOR
  oGMAObj.AriaForm1.cboLocations.LISTINDEX = lnI


  *:****************************************************************************************
  *! Name      : lfGetFile
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 12/16/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200464,1
FUNCTION lfGetFile

  IF TYPE('lcEdiTrnTyp') = 'U'
    RETURN
  ENDIF

  IF lcEdiTrnTyp = "856"
    lcOutFlNam = IIF(EMPTY(EDINET.c856File),EDINET.cOutFile,EDINET.c856File)
  ENDIF

  *:****************************************************************************************
  *! Name      : lfCHKFLDRD
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 12/16/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200464,1
FUNCTION lfCHKFLDRD

  IF lnLockFile < 0 OR INLIST(lnLockFile,1,3,5,7)
    lcUser = ''
    lnUserFile = FOPEN(ALLTRIM(oAriaApplication.EDIPath) + "OUTBOX\" + ;
      ALLTRIM(SUBSTR(lcOutFlNam , 1 , RAT('.' , lcOutFlNam))) + "usr")
    IF lnUserFile > 0
      lcUser = ALLTRIM(FGETS(lnUserFile))
      =FCLOSE(lnUserFile)
    ENDIF
    llLockNet = .F.
    *-- Outgoing file is in use by another user/read only. All the transactions that
    *-- should be sent in this file will be ignored.
    =oAriaApplication.MESSAGEBOX('TRM00000B00000','ALERT',.F.,.F.,;
      'Outgoing file is in use by another user, or it is Read only.'+;
      'All the transactions that should be sent in this file will be ignored.')
  ENDIF


  *:****************************************************************************************
  *! Name      : lfINCRSRHC
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 12/16/2002
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200464,1
FUNCTION lfINCRSRHC

  lcValue    = _SCREEN.ACTIVEFORM.ACTIVECONTROL.VALUE
  lcFileName = _SCREEN.ACTIVEFORM.FileName
  lcFld      = _SCREEN.ACTIVEFORM.FIELD
  lcISKey    = _SCREEN.ACTIVEFORM.ISKEY
  lcFilt     = _SCREEN.ACTIVEFORM.lcFilter

  IF EMPTY(lcFilt)
    IF lcISKey
      =SEEK(ALLT(lcValue),lcFileName)
    ELSE
      lcField = lcFld+'="'+ALLT(lcValue)+'"'
      SELECT (lcFileName)
      LOCATE FOR &lcField
    ENDIF
  ELSE
    lcField = lcFilt+' AND '+lcFld+'="'+ALLT(lcValue)+'"'
    SELECT (lcFileName)
    LOCATE FOR &lcField
  ENDIF

  IF TYPE('_SCREEN.ACTIVEFORM.GRIDTORefresh')='O'
    _SCREEN.ACTIVEFORM.GRIDTORefresh.REFRESH()
  ENDIF

  *:****************************************************************************************
  *! Name      : lfCHNGINDX
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 27/01/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:B606913,1
FUNCTION lfCHNGINDX

  INDEX ON STORE + STYLE + Pack_id + cPkColor + cPckSize + cPkVersion TAG EDIORDPK
  SET ORDER TO EDIORDPK

  *:****************************************************************************************
  *! Name      : lfSETKEY
  *! Developer : Hend Ghanem  (GMA)
  *! Date      : 27/01/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : This program
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:B606913,1
FUNCTION lfSETKEY

  lcPack = IIF(Mord = 3,XPACK+PADR(SPCK_LIN.cPkColor,6)+PADR(SPCK_LIN.cPckSize,3)+PADR(SPCK_LIN.cPkVersion,4),XPACK)

  *:****************************************************************************************
  *! Name      : lfGTHPCK
  *! Developer : Waleed Hamed  (WLD)
  *! Date      : 23/02/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : senducclabels class
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200497,1
FUNCTION lfGTHPCK
  lcCurrAlis = ALIAS()
  *I119999,1  TMI [Start] print the stub label "XXK" for GMA.
  *C123073,1 HBG Fix bug of build the ASN into one item and the quantities are added.[Begin]
  *IF TYPE('ll_Use_XXK')='L' .AND. ll_Use_XXK = .T.
  *  =IIF(!EMPTY(PACK_LIN.PACK_ID),;
  *                  oObject.ProcessPacks(),;
  *                  oObject.ProcessSizes())
  IF TYPE('ll_Use_XXK')='L'
    IF ll_Use_XXK = .T.
      =IIF(!EMPTY(Pack_Lin.Pack_id),;
        lfUccPck(),;
        oObject.ProcessSizes())
      *C123073,1 [End]
    ELSE
      *I119999,1  TMI [End  ]
      =oObject.ProcessSizes()
      *I119999,1  TMI [Start] Close the above If statment
    ENDIF
    *C123073,1 HBG Fix bug of build the ASN into one item and the quantities are added.[Begin]
  ELSE
    =IIF(!EMPTY(Pack_Lin.Pack_id),;
      lfUccPck(),;
      oObject.ProcessSizes())
  ENDIF
  *C123073,1 [End]
  *I119999,1  TMI [End  ]
  SELECT (lcCurrAlis)


  *:****************************************************************************************
  *! Name      : lfSAVPCK
  *! Developer : Waleed Hamed  (WLD)
  *! Date      : 23/02/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : senducclabels class
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200497,1
FUNCTION lfSAVPCK

  lcCurrAlis = ALIAS()

  SELECT (lcTmpCrtninfo)
  REPLACE Pack_id WITH &lcCartons..Pack_id,;
    cSku       WITH &lcCartons..cSku,;
    cPckAcc    WITH &lcCartons..cPckAcc  ,;
    cPkColor   WITH &lcCartons..cPkColor ,;
    cPckSize   WITH &lcCartons..cPckSize ,;
    cPkVersion WITH &lcCartons..cPkVersion

  SELECT (lcCurrAlis)

  *:****************************************************************************************
  *! Name      : lfCLMCRT
  *! Developer : Waleed Hamed  (GMA)
  *! Date      : 23/02/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : senducclabels class
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200497,1
FUNCTION lfCLMCRT

  lcCurrAlis = ALIAS()

  IF USED(lcCartons)
    ALTER TABLE (lcCartons) ADD COLUMN cPckAcc    C(5)
    ALTER TABLE (lcCartons) ADD COLUMN cPkColor   C(6)
    ALTER TABLE (lcCartons) ADD COLUMN cPckSize   C(3)
    ALTER TABLE (lcCartons) ADD COLUMN cPkVersion C(4)
    ALTER TABLE (lcCartons) ADD COLUMN cSku       C(16)
  ENDIF
  SELECT (lcCurrAlis)
  *:****************************************************************************************
  *! Name      : lfGETCRT
  *! Developer : Waleed Hamed  (WLD)
  *! Date      : 23/02/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : senducclabels class
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200497,1
FUNCTION lfGETCRT
  LPARAMETERS lcPackNo,lcAccount
  PRIVATE lnAlias, lnCount, lcCount


  LOCAL lcTemp
  lnAlias = SELECT()
  ZAP IN (lcCartons)
  SELECT Pack_Lin
  = SEEK(MPACKNO)
  SCAN REST WHILE PACK_NO = MPACKNO
    MSKU = " "
    = SEEK('O' + Pack_Hdr.ORDER + Pack_Hdr.STORE + STYLE + STR(nOrdLineNo,6),'ORDLINE','ORDLINST')
    lcPackId = OrdLine.Pack_id
    lcSize = ' '
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF Qty&lcCount = TotQTY
        lcSize = lcCount
      ENDIF
    ENDFOR
    lcStyle = SUBSTR(Pack_Lin.STYLE, 1, LEN(lcMjrPic))
    IF EMPTY(OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion)
      IF !EMPTY(lcSize) AND SEEK('S'+MACCOUNT+OrdLine.STYLE,'spck_lin','Spcklins')
        SELECT SPCK_LIN
        SCAN REST WHILE TYPE+Account+STYLE = 'S'+MACCOUNT+OrdLine.STYLE
          IF SPCK_LIN.Qty&lcSize = 1
            EXIT
          ENDIF
        ENDSCAN
        MSKU = SPCK_LIN.Pack_id
        SELECT Pack_Lin
      ENDIF
    ELSE
      IF SEEK('S'+MACCOUNT+PADR(OrdLine.Pack_id,19)+OrdLine.cPkColor+OrdLine.cPckSize,'spck_lin','spck_linSt')
        MSKU=SPCK_LIN.Pack_id
      ENDIF
    ENDIF
    IF !SEEK(STR(Pack_Lin.nPltNo,4) + STR(Pack_Lin.No_Cart,4),lcCartons)
      INSERT INTO (lcCartons) (nPltNo          , No_Cart          , STYLE          ,;
        Major   , SIZE   , Pack_id  , TotQTY          , cPckAcc         ,;
        cPkColor         , cPckSize         , cPkVersion,cSku);
        VALUES           (Pack_Lin.nPltNo , Pack_Lin.No_Cart , Pack_Lin.STYLE , lcStyle ,;
        lcSize , lcPackId , Pack_Lin.TotQTY , OrdLine.cPckAcc ,;
        OrdLine.cPkColor , OrdLine.cPckSize , OrdLine.cPkVersion,MSKU)
    ELSE
      IF laSetups[1,2]
        IF LEFT(STYLE,LEN(STYLE)-3) = LEFT(Pack_Lin.STYLE,LEN(Pack_Lin.STYLE)-3)
          lcTemp = STYLE
        ELSE
          lcTemp = ''
        ENDIF
        llSame = (&lcCartons..Pack_id=lcPackId AND &lcCartons..cPkColor = OrdLine.cPkColor AND;
          &lcCartons..cPckSize = OrdLine.cPckSize AND &lcCartons..cPkVersion = OrdLine.cPkVersion)

        REPLACE STYLE   WITH lcTemp ,;
          Major      WITH IIF(Major=PADR(lcStyle,19),Major,'') ,;
          SIZE       WITH IIF(SIZE=lcSize,SIZE,'') ,;
          Pack_id    WITH IIF(llSame,lcPackId, '') ,;
          TotQTY     WITH TotQTY + Pack_Lin.TotQTY,;
          cPkColor   WITH IIF(llSame , OrdLine.cPkColor, '') ,;
          cPckSize   WITH IIF(llSame , OrdLine.cPckSize, '') ,;
          cPkVersion WITH IIF(llSame , OrdLine.cPkVersion, ''),;
          cSku       WITH IIF(IIF(EMPTY(lcPackId),!EMPTY(lcTemp),llSame),MSKU,'')  IN (lcCartons)
      ELSE
        llSame = (&lcCartons..Pack_id=lcPackId AND &lcCartons..cPkColor = OrdLine.cPkColor AND;
          &lcCartons..cPckSize = OrdLine.cPckSize AND &lcCartons..cPkVersion = OrdLine.cPkVersion)
        REPLACE STYLE   WITH IIF(STYLE = Pack_Lin.STYLE,STYLE,'') ,;
          Major      WITH IIF(Major=PADR(lcStyle,19),Major,'') ,;
          SIZE       WITH IIF(SIZE=lcSize,SIZE,'') ,;
          Pack_id    WITH IIF(llSame,lcPackId, '') ,;
          TotQTY     WITH TotQTY + Pack_Lin.TotQTY ,;
          cPkColor   WITH IIF(llSame , OrdLine.cPkColor, '') ,;
          cPckSize   WITH IIF(llSame , OrdLine.cPckSize, '') ,;
          cPkVersion WITH IIF(llSame , OrdLine.cPkVersion, '') ,;
          cSku       WITH IIF(IIF(EMPTY(lcPackId),STYLE = Pack_Lin.STYLE,llSame),MSKU, '')  IN (lcCartons)
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lnAlias)

  *:****************************************************************************************
  *! Name      : lfPrcesPck
  *! Developer : Hend Ghanem (HBG)
  *! Date      : 23/02/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : lfGTHPCK()
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200497,1
FUNCTION lfPrcesPck

  LOCAL lnTotQty, lnTotPcs, lnRow, llFound
  lnTotPcs = Pack_Lin.TotQTY
  llFound = .F.

  FOR lnRow = 1 TO lnLblInfo
    IF laLblInfo[lnRow, 4]  = lcPackId ;
        .AND. LEFT(laLblInfo[lnRow, 1], 2) = "$#"
      laLblInfo[lnRow, 7] = laLblInfo[lnRow, 7] + lnTotPcs
      llFound = .T.
      IF SEEK('P' + MACCOUNT +  OrdLine.Pack_id + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion+ Pack_Lin.STYLE , 'Spck_Lin', 'Spck_LinVr') OR;
          SEEK('P' + '*****'  +  OrdLine.Pack_id + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion+ Pack_Lin.STYLE , 'Spck_Lin', 'Spck_LinVr')
        laLblInfo[lnLblInfo, 8] = laLblInfo[lnLblInfo, 8] + SPCK_LIN.TotQTY
      ENDIF
      EXIT
    ENDIF
  ENDFOR

  lcSku = ""
  IF SEEK('S' + MACCOUNT +  PADR(OrdLine.Pack_id,19) + OrdLine.cPkColor + OrdLine.cPckSize , 'Spck_Lin', 'Spck_LinSt') OR;
      SEEK('S' + '*****'  +  PADR(OrdLine.Pack_id,19) + OrdLine.cPkColor + OrdLine.cPckSize , 'Spck_Lin', 'Spck_LinSt')
    lcSku   = SPCK_LIN.Pack_id
  ENDIF

  IF !llFound
    lnLblInfo = lnLblInfo + 1
    DIMENSION laLblInfo[lnLblInfo , 8]
    IF SEEK('P' + MACCOUNT +  OrdLine.Pack_id + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion+ Pack_Lin.STYLE , 'Spck_Lin', 'Spck_LinVr') OR;
        SEEK('P' + '*****'  +  OrdLine.Pack_id + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion+ Pack_Lin.STYLE , 'Spck_Lin', 'Spck_LinVr')
      laLblInfo[lnLblInfo, 8] = SPCK_LIN.TotQTY
    ELSE
      laLblInfo[lnLblInfo, 8] = 0
    ENDIF
    laLblInfo[lnLblInfo, 1] = lcSku
    laLblInfo[lnLblInfo, 2] = 0
    laLblInfo[lnLblInfo, 3] = ""
    laLblInfo[lnLblInfo, 4] = ALLTRIM(OrdLine.Pack_id)+'-'+ALLTRIM(OrdLine.cPkColor)+'-'+ALLTRIM(OrdLine.cPckSize)+'-'+ALLTRIM(OrdLine.cPkVersion)
    laLblInfo[lnLblInfo, 5] = ""
    laLblInfo[lnLblInfo, 6] = 0
    laLblInfo[lnLblInfo, 7] = lnTotPcs

    IF llUPCUse .AND.;
        ((SEEK(MACCOUNT +  PADR(OrdLine.Pack_id,19) + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion, 'StyleUPC', 'Gmapkupc') .AND. StyleUpc.lUPCPack) .OR. ;
        (SEEK('*****'  +  PADR(OrdLine.Pack_id,19) + OrdLine.cPkColor + OrdLine.cPckSize + OrdLine.cPkVersion, 'StyleUPC', 'Gmapkupc') .AND. StyleUpc.lUPCPack))
      laLblInfo[lnLblInfo,5] = StyleUpc.cUpcNum1 + StyleUpc.cUpcNum2 + ;
        StyleUpc.cUpcNum3
    ENDIF
  ENDIF
  *:****************************************************************************************
  *! Name      : lfORDRADDSKU
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 03/12/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : Print method of EbOrdRep screen
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200510,1
FUNCTION LFORDRADDSKU

  oObject.AddArrayLine('Sku1','C',16)
  oObject.AddArrayLine('Sku2','C',16)
  oObject.AddArrayLine('Sku3','C',16)
  oObject.AddArrayLine('Sku4','C',16)
  oObject.AddArrayLine('Sku5','C',16)
  oObject.AddArrayLine('Sku6','C',16)
  oObject.AddArrayLine('Sku7','C',16)
  oObject.AddArrayLine('Sku8','C',16)

  *:****************************************************************************************
  *! Name      : lfORDRGETSKU
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 03/12/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : Print method of EbOrdRep screen
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200510,1
FUNCTION LFORDRGETSKU
  STORE '' TO m.Sku1,m.Sku2,m.Sku3,m.Sku4,m.Sku5,m.Sku6,m.Sku7,m.Sku8
  IF SEEK(ORDREPLN.Account+PADR(ORDREPLN.STORE,8)+ORDREPLN.ORDER+STR(ORDREPLN.LINENO,6),'ORDLINE','OrdLAcc')
    IF !EMPTY(OrdLine.Pack_id)
      IF SEEK('S'+OrdLine.Account+PADR(OrdLine.Pack_id,16)+OrdLine.cPkColor+OrdLine.cPckSize,'Spck_Lin','Spck_LinVr')
        m.Sku1 = ALLTRIM(SPCK_LIN.Pack_id )
      ENDIF
    ELSE
      lcSize = '1 '
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        IF OrdLine.Book&lcCount <> 0
          lcSize = lcCount
          IF !EMPTY(lcSize) AND SEEK('S'+ORDREPLN.Account+ORDREPLN.STYLE,'spck_lin','Spcklins')
            SELECT SPCK_LIN
            LOCATE REST WHILE TYPE+Account+STYLE = 'S'+ORDREPLN.Account+ORDREPLN.STYLE FOR Qty&lcSize = 1
            IF FOUND()
              m.SKU&lcSize = SPCK_LIN.Pack_id
            ENDIF
            SELECT ORDREPLN
          ENDIF
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  STORE ' ' TO m.CORDINQNO,m.CTRNPURPOS
  STORE { } TO m.TRAN_DATE
  IF SEEK(ORDREPLN.CORDREPNO,'ORDREPHD')
    m.CORDINQNO = ORDREPHD.CORDINQNO
    m.TRAN_DATE = ORDREPHD.TRAN_DATE
    m.CTRNPURPOS = oObject.laPurpose[VAL(ORDREPHD.CTRNPURPOS)]
  ENDIF
  INSERT INTO (tmpOrdRepLn) FROM MEMVAR

  *:****************************************************************************************
  *! Name      : lfORDRSTRP
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 03/12/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : Print method of EbOrdRep screen
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200510,1
FUNCTION LFORDSTRP
  oObjAriaRep.ReportFileName = oAriaApplication.ReportHome + 'EB\GMAOrdRp.RPT'


  *:****************************************************************************************
  *! Name      : lfSLCATPCK
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 03/26/2003
  *! Purpose   : Use Pack-Color-Size in class send832 (EdiSc.Do)
  *:****************************************************************************************
  *! Called from : CLASS EDISC FROM SEND832.DO (SALES CATALOG)
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:*C200526,1
FUNCTION LFSLCATPCK

  lcCurrAlis = ALIAS()

  lcPkClrCat = &lcSelCodes..cPkColor
  lcPkSzCat  = &lcSelCodes..cPkSize
  *B121104,1 HBG 15/02/2004 Set the correct index for SPCK_HDR file [Begin]
  lcOldOrd = ORDER('SPCK_HDR')
  SET ORDER TO Spck_HdrVr IN Spck_HDR
  *B121104,1 [End]
  IF SEEK('P'+StyleUpc.Account+lcPack+lcPkClrCat+lcPkSzCat,'SPCK_HDR','SPCK_HDRVR')
    IF !EMPTY(Spck_HDR.cPckSize)
      lcscale= LEFT(Spck_HDR.cPckSize,1)
      MSizeNrf  = ''                            && Size NRF code
      MSizeDesc = ''                            && Size description
      =SEEK('S'+lcscale,'scale','scale')
      FOR lnCount = 1 TO 8
        IF STR(lnCount,1) = RIGHT(Spck_HDR.cPckSize,1)
          lcSize   = STR(lnCount,1)
          EXIT
        ENDIF
      ENDFOR
      IF !EMPTY(lcscale) AND INLIST(ALLTRIM(lcSize),'1','2','3','4','5','6','7','8') AND ;
          SEEK('S'+lcscale,'Scale')
        MSizeNrf  = SCALE.cNrfCode&lcSize                 && Size NRF code
        MSizeDesc = SCALE.SZ&lcSize+ ALLTRIM(SCALE.cScl_Desc)  && Size description
      ELSE
        lcSize    = ''
        MSizeNrf  = ''                && Size NRF code
        MSizeDesc = ''                && Size description
      ENDIF
    ENDIF
    IF llColor AND !EMPTY(Spck_HDR.cPkColor)
      lcColor=Spck_HDR.cPkColor
      =oRelatedFlds.DO(lcColor,@laCRltFld,'COLOR')
      =SEEK('N'+lcColor+'N'+'COLOR','CODES','CODES')
      MClrDesc = CODES.cDiscRep
    ENDIF
  ENDIF
  *B121104,1 HBG 15/02/2004 Set the correct index for SPCK_HDR file [Begin]
  SET ORDER TO (lcOldOrd) IN Spck_HDR
  *B121104,1 [End]

  SELECT (lcCurrAlis)
  *:****************************************************************************************
  *! Name      : lfSLCATTRN
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 03/26/2003
  *! Purpose   : Use Pack-Color-Size in class send832 (EdiSc.Do)
  *:****************************************************************************************
  *! Called from : CLASS EDISC FROM SEND832.DO (SALES CATALOG)
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:*C200526,1
FUNCTION lfSLCATTRN

  lcCurrAlis = ALIAS()
  =SEEK(TYPE+cPartner+cSelcCode+cUpc,'EDICATGD','UPCNUM')
  *B607263,1 HBG 14/05/2003 Locate for the correct line according to the status [Begin]
  lcKey = TYPE+cPartner+cSelcCode+cUpc
  SELECT EDICATGD
  LOCATE REST FOR TYPE+cPartner+cSelcCode+cUpc = lcKey AND cEdiDStat = lcEdiStatus
  *B607263,1 [End]
  SELECT (lcCurrAlis)


  *:****************************************************************************************
  *! Name      : lfErrDpt
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 04/07/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : lfErrDptU()
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200530,1
FUNCTION lfERRDPT

  lcCurrAlis = ALIAS()

  IF llFrmUp OR llMultiUp
    IF !EMPTY(ORDHDR.DEPT) .AND. !SEEK(ORDHDR.Account + ORDHDR.DEPT , 'CUSTDEPT')
      llError  = .T.
      MESSAGEBOX("Cannot update the temp order# " + ALLTRIM(EDILIBDT.cEdiTranNo) + " untill department is created.",16,_SCREEN.CAPTION)
      IF !SEEK('D' + ALLTRIM(ORDHDR.DEPT),lcErrsF)
        =FPUTS(lnErFl , ' Dept '+ ALLTRIM(ORDHDR.DEPT) + ' does not exist in the system. ')
        INSERT INTO (lcErrsF) (cType,cCode) VALUES ('D',ALLTRIM(ORDHDR.DEPT))
      ENDIF
    ENDIF
  ELSE
    IF !EMPTY(ORDHDR.DEPT)
      IF !SEEK(ORDHDR.Account+ORDHDR.DEPT,'CUSTDEPT')
        llError  = .T.
        IF !SEEK('D' + ALLTRIM(ORDHDR.DEPT),lcErrsF)
          =FPUTS(lnErFl , ' Dept '+ ALLTRIM(ORDHDR.DEPT) + ' does not exist in the system. ')
          INSERT INTO (lcErrsF) (cType,cCode) VALUES ('D',ALLTRIM(ORDHDR.DEPT))
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  SELECT (lcCurrAlis)
  *:****************************************************************************************
  *! Name      : lfErrDptP
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 04/07/2003
  *! Purpose   :
  *:****************************************************************************************
  *! Called from : lfErrDptP()
  *:****************************************************************************************
  *! Calls       :
  *:****************************************************************************************
  *! Return      : None
  *:****************************************************************************************
  *:C200530,1
FUNCTION lfERRDPTP

  lcCurrAlis = ALIAS()

  IF !EMPTY(MDEPT)
    IF !SEEK(MACCOUNT+MDEPT,'CUSTDEPT')
      llError  = .T.
      =FPUTS(lnErFl , ' Dept '+ ALLTRIM(MDEPT) + ' does not exist in the system. ')
    ENDIF
  ENDIF
  *E126512 Request to show missing dept# & desc and not allow to update AAH 03/06/2005 [begin]
  IF !EMPTY(MDEPT) AND !EMPTY(MDEPTDESC) AND SEEK(MACCOUNT+MDEPT,'CUSTDEPT')
    IF ALLTRIM(CUSTDEPT.CDEPTDESC) #  ALLTRIM(MDEPTDESC)
      llError  = .T.
      =FPUTS(lnErFl , 'Income Dept. description  does not match exist dept. description in the system. ')
      RFLAG = .F.
    ENDIF
  ENDIF
  *E126512 Request to show missing dept# & desc and not allow to update AAH 03/06/2005 [begin]

  SELECT (lcCurrAlis)

  *:****************************************************************************************
  *! Name      : lfAssgnOrd
  *! Developer : Sameh Saiid Ezzat (SSE)
  *! Date      : 05/14/2003
  *! Purpose   : Assign Sales Order # the same web order number.
  *:****************************************************************************************
  *! Called from : SOUPDATE in Prgs\EB
  *:****************************************************************************************
  *B607260,1
FUNCTION lfAssgnOrd

  IF llFromEDI
    IF TYPE('TmpWebOrd') <> 'U'
      laData[1] = TmpWebOrd.ORDER
    ELSE
      laData[1] = IIF(TYPE('lcEdiOrder') <> 'U',lcEdiOrder,laData[1])
    ENDIF
  ENDIF
  *-- End of lfAssgnOrd

  *:****************************************************************************************
  *! Name      : lfShViaSt
  *! Developer : Hend Ghanem
  *! Date      : 06/09/2003
  *! Purpose   : Add popup to partner setup screen
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200504,1
FUNCTION lfShViaSt

  *C200592,1 HBG 17/08/2003 Add popup to partner setup screen to update send pack detail field [Begin]
  *!*	oFormSet.ariaform1.pageFrame.page2.AddObject('lblShipVia','ARIALABEL')
  *!*	WITH oFormSet.ariaform1.pageFrame.page2.lblShipVia
  *!*	    .Caption = 'Ship Via'
  *!*	    .Left    = 302
  *!*	    .Top     = 222
  *!*	    .Visible = .T.
  *!*	ENDWITH

  *!*	oFormSet.ariaform1.pageFrame.page2.AddObject('CMBSHVIAST','CMBOBOX')
  *!*	WITH oFormSet.ariaform1.pageFrame.page2.cmbShViaSt
  *!*	    .Left    = 444
  *!*	    .Top     = 219
  *!*		.Visible = .T.
  *!*		.Enabled = .T.
  *!*	ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('lblShipVia','ARIALABEL')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.lblShipVia
    .CAPTION = 'Ship Via'
    .LEFT    = 15
    .TOP     = 251
    .VISIBLE = .T.
  ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('CMBSHVIAST','CMBOBOX')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.cmbShViaSt
    .LEFT    = 157
    .TOP     = 247
    .VISIBLE = .T.
    .ENABLED = .T.
    .WIDTH   = 122
  ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('ShpPacks','ARIASHAPE')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.ShpPacks
    .LEFT    = 397
    .TOP     = 217
    .VISIBLE = .T.
    .WIDTH   = 180
    .HEIGHT  = 59
  ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('lblSnPkD','ARIALABEL')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.lblSnPkD
    .CAPTION = 'Send pack details : '
    .LEFT    = 302
    .TOP     = 224
    .VISIBLE = .T.
  ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('lblSnPkD810','ARIALABEL')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.lblSnPkD810
    .CAPTION = '810 : '
    .LEFT    = 402
    .TOP     = 224
    .VISIBLE = .T.
  ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('cmbSnPkD810','ARIACOMBOBOX')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.cmbSnPkD810
    .LEFT    = 444
    .TOP     = 219
    .VISIBLE = .T.
    .ENABLED = .T.
    .BOUNDCOLUMN = 2
    .COLUMNCOUNT = 2
    .COLUMNLINES = .F.
    .COLUMNWIDTHS = "130,0"
    .CONTROLSOURCE = "EDIPH.cSnPkD810"
    .ROWSOURCE  = "Yes,Y,No,N"
    .ROWSOURCETYPE = 1
    .STYLE = 2
    .VALUE = 'N'
    .WIDTH = 122
  ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('lblSnPkD856','ARIALABEL')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.lblSnPkD856
    .CAPTION = '856 : '
    .LEFT    = 402
    .TOP     = 251
    .VISIBLE = .T.
  ENDWITH

  oFormSet.AriaForm1.PAGEFRAME.page2.ADDOBJECT('cmbSnPkD856','ARIACOMBOBOX')
  WITH oFormSet.AriaForm1.PAGEFRAME.page2.cmbSnPkD856
    .LEFT    = 444
    .TOP     = 247
    .VISIBLE = .T.
    .ENABLED = .T.
    .BOUNDCOLUMN = 2
    .COLUMNCOUNT = 2
    .COLUMNLINES = .F.
    .COLUMNWIDTHS = "130,0"
    .CONTROLSOURCE = "EDIPH.cSnPkD856"
    .ROWSOURCE  = "Yes,Y,No,N"
    .ROWSOURCETYPE = 1
    .STYLE = 2
    .VALUE = 'N'
    .WIDTH = 122
  ENDWITH
  *C200592,1[End]

  *:****************************************************************************************
  *! Name      : lfShpRfrsh
  *! Developer : Hend Ghanem
  *! Date      : 06/09/2003
  *! Purpose   : Refresh ship via popup in partner setup screen
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200504,1
FUNCTION lfShpRfrsh

  oFormSet.AriaForm1.PAGEFRAME.page2.cmbShViaSt.ENABLED = INLIST(oFormSet.ActiveMode,'A','E')

  *C200592,1 HBG 17/08/2003 Refresh popup of send pack detail [Begin]
  oFormSet.AriaForm1.PAGEFRAME.page2.cmbSnPkD810.ENABLED = INLIST(oFormSet.ActiveMode,'A','E')
  oFormSet.AriaForm1.PAGEFRAME.page2.cmbSnPkD856.ENABLED = INLIST(oFormSet.ActiveMode,'A','E')
  *C200592,1[End]

  *:****************************************************************************************
  *! Name      : lfUpdShpv
  *! Developer : Hend Ghanem
  *! Date      : 06/09/2003
  *! Purpose   : Update Ship Via field in EDIPH
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200504,1
FUNCTION lfUpdShpv

  lcAlis = ALIAS()
  SELECT EdiPh
  REPLACE cShViaSt WITH 'S'
  *C200592,1 HBG 17/08/2003 Update send pack detail field [Begin]
  REPLACE cSnPkD810 WITH 'N',;
    cSnPkD856 WITH 'N'
  *C200592,1[End]
  SELECT (lcAlis)

  *:****************************************************************************************
  *! Name      : lfUpOrdShV
  *! Developer : Hend Ghanem
  *! Date      : 06/09/2003
  *! Purpose   : Update Ship Via field in ORDHDR
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200504,1
FUNCTION lfUpOrdShV

  REPLACE ShipVia WITH IIF(EdiPh.cShViaSt = 'S','*',lcShipVia)
  *B127835,1 WAM remove PO conflect
  SET ORDER TO ConflctL IN ConflctL
  SET ORDER TO ConflctH IN ConflctH
  IF SEEK(EDILIBDT.cFileCode+MCustPo,'CONFLCTL')
    SELECT ConflctL
    Delete REST WHILE cFileCode+CustPO+cPack = EDILIBDT.cFileCode+MCustPo
    SELECT ConflctH
    IF SEEK(EDILIBDT.cFileCode+MCustPo)
      Delete
    ENDIF
    SELECT ORDHDR
  ENDIF
  *B127835,1 WAM (End)

  *:****************************************************************************************
  *! Name      : lfBatchPrc
  *! Developer : Hend Ghanem
  *! Date      : 06/09/2003
  *! Purpose   : Update Ship Via field in ORDHDR
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200504,1
FUNCTION lfBatchPrc

  oObj = CREATEOBJECT('EDIMAIN')
  llBatch = IIF(TYPE('lnSelecet')="U",llBatch ,lnSelecet > 1)
  IF TYPE('lnSelecet')="U"
    llFrstPo = .T.
  ENDIF
  DO WHILE cFileCode+cPartCode+cIntchgSeq+cGroupSeq+cEDITrnTyp = PADR(lcFileCode,6)
    lcGroupAcc = cFileCode+cPartCode
    =SEEK(cPartCode+'850','EDIPD','PARTTRANS')
    =SEEK(cPartCode,'EDIPH')
    =SEEK(cPartCode,'EdiAcPrt','PARTNER')
    STORE .F. TO llDefAccLoc

    DO WHILE cFileCode+cPartCode+cIntchgSeq+cGroupSeq+cEDITrnTyp = lcGroupAcc
      lcGroupKey = cFileCode+cPartCode+cIntchgSeq+cGroupSeq+cEDITrnTyp
      SCAN REST WHILE cFileCode+cPartCode+cIntchgSeq+cGroupSeq+cEDITrnTyp = lcGroupKey ;
          FOR   cEDITrnSt <> 'R' .AND. cStatus <> 'U' .AND. &lcFilter
        IF llMultiWare AND !llDefFileLoc AND !llDefAccLoc
          IF llFrstPo
            DO FORM oAriaApplication.ScreenHome + 'EB\ebShpWar.scx' ;
              WITH llBatch,ALLTRIM(EDILIBDT.cEdiRef),EDILIBDT.cPartCode TO lcWareSet
            llFrstPo = .F.
            lcWareHous = lcWareSet
          ENDIF
          IF EMPTY(lcWareHous)
            LOOP
          ENDIF
          oObject.Location = LEFT(lcWareHous,6)
          llDefAccLoc   = SUBSTR(lcWareHous,7,1)='Y'
          llDefFileLoc  = SUBSTR(lcWareHous,8,1)='Y'
        ENDIF
        =oObj.PROCESS()
        llReturn = oObject.ProcessPo(llBatch)
        =oObject.CloseEdiFile()
        lcTran  = EDILIBDT.mTranText
        lnTransNo = lnTransNo + VAL(SUBSTR(lcTran,AT('|LENGTH|',lcTran)+8))
        SET ORDER TO TAG FILEISAGS IN EDILIBDT
      ENDSCAN
    ENDDO
  ENDDO


  *:****************************************************************************************
  *! Name      : lfCrTmpOrd
  *! Developer : Hend Ghanem
  *! Date      : 06/09/2003
  *! Purpose   : Creat Temo Order
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *B607378,1
FUNCTION lfCrTmpOrd

  DO CASE
    *-- Case of Change of Dates
  CASE cLineStat = 'CT'
    IF SEEK('O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE ,'ORDLINE1','ORDLINST')
      llFound=.T.
      IF OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion # Ordline1.Pack_id+Ordline1.cPkColor+Ordline1.cPckSize+Ordline1.cPkVersion
        llFound = .F.
        SELECT Ordline1
        SCAN REST WHILE cOrdType+ORDER+STORE+STYLE = 'O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE FOR;
            Pack_id+cPkColor+cPckSize+cPkVersion = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
          llFound=.T.
          EXIT
        ENDSCAN
        SELECT OrdLine
      ENDIF
      IF llFound
        IF !SEEK('O' + Ordline1.ORDER + STR(Ordline1.LINENO , 6),lcOrdLine)
          APPEND BLANK IN (lcOrdLine)
        ENDIF
        SELECT Ordline1
        SCATTER MEMVAR MEMO
        m.Flag     = 'M'
        m.Start    = IIF(!EMPTY(OrdLine.START) , OrdLine.START , m.Start)
        m.Complete = IIF(!EMPTY(OrdLine.COMPLETE) , OrdLine.COMPLETE ,m.Complete)
        SELECT (lcOrdLine)
        GATHER MEMVAR MEMO
      ENDIF
    ENDIF
  CASE INLIST(cLineStat,'QI','QD')
    IF SEEK('O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE ,'ORDLINE1','ORDLINST')
      llFound=.T.
      IF OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion # Ordline1.Pack_id+Ordline1.cPkColor+Ordline1.cPckSize+Ordline1.cPkVersion
        llFound = .F.
        SELECT Ordline1
        SCAN REST WHILE cOrdType+ORDER+STORE+STYLE = 'O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE FOR;
            Pack_id+cPkColor+cPckSize+cPkVersion = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
          llFound=.T.
          EXIT
        ENDSCAN
        SELECT OrdLine
      ENDIF
      IF llFound
        IF !SEEK('O' + Ordline1.ORDER + STR(Ordline1.LINENO , 6),lcOrdLine)
          APPEND BLANK IN (lcOrdLine)
        ENDIF
        SELECT Ordline1
        SCATTER MEMVAR MEMO
        m.Flag   = 'M'
        m.Qty1   = MAX(m.Qty1 + OrdLine.Qty1 , 0)
        m.Qty2   = MAX(m.Qty2 + OrdLine.Qty2 , 0)
        m.Qty3   = MAX(m.Qty3 + OrdLine.Qty3 , 0)
        m.Qty4   = MAX(m.Qty4 + OrdLine.Qty4 , 0)
        m.Qty5   = MAX(m.Qty5 + OrdLine.Qty5 , 0)
        m.Qty6   = MAX(m.Qty6 + OrdLine.Qty6 , 0)
        m.Qty7   = MAX(m.Qty7 + OrdLine.Qty7 , 0)
        m.Qty8   = MAX(m.Qty8 + OrdLine.Qty8 , 0)
        m.TotQTY = m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8

        SELECT (lcOrdLine)
        GATHER MEMVAR MEMO
        IF m.TotQTY = 0
          REPLACE cLineStat WITH 'DI'
        ENDIF
      ENDIF
    ENDIF
    *-- Case of Quantity Increased/Decreased
  CASE cLineStat = 'PQ'
    IF SEEK('O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE ,'ORDLINE1','ORDLINST')
      llFound=.T.
      IF OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion # Ordline1.Pack_id+Ordline1.cPkColor+Ordline1.cPckSize+Ordline1.cPkVersion
        llFound = .F.
        SELECT Ordline1
        SCAN REST WHILE cOrdType+ORDER+STORE+STYLE = 'O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE FOR;
            Pack_id+cPkColor+cPckSize+cPkVersion = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
          llFound=.T.
          EXIT
        ENDSCAN
        SELECT OrdLine
      ENDIF
      IF llFound
        IF !SEEK('O' + Ordline1.ORDER + STR(Ordline1.LINENO , 6),lcOrdLine)
          APPEND BLANK IN (lcOrdLine)
        ENDIF
        SELECT Ordline1
        SCATTER MEMVAR MEMO
        m.Flag   = 'M'
        m.Qty1   = IIF(OrdLine.Qty1=0 AND !('SIZE1' $ OrdLine.DESC1),m.Qty1,OrdLine.Qty1)
        m.Qty2   = IIF(OrdLine.Qty2=0 AND !('SIZE2' $ OrdLine.DESC1),m.Qty2,OrdLine.Qty2)
        m.Qty3   = IIF(OrdLine.Qty3=0 AND !('SIZE3' $ OrdLine.DESC1),m.Qty3,OrdLine.Qty3)
        m.Qty4   = IIF(OrdLine.Qty4=0 AND !('SIZE4' $ OrdLine.DESC1),m.Qty4,OrdLine.Qty4)
        m.Qty5   = IIF(OrdLine.Qty5=0 AND !('SIZE5' $ OrdLine.DESC1) ,m.Qty5,OrdLine.Qty5)
        m.Qty6   = IIF(OrdLine.Qty6=0 AND !('SIZE6' $ OrdLine.DESC1),m.Qty6,OrdLine.Qty6)
        m.Qty7   = IIF(OrdLine.Qty7=0 AND !('SIZE7' $ OrdLine.DESC1),m.Qty7,OrdLine.Qty7)
        m.Qty8   = IIF(OrdLine.Qty8=0 AND !('SIZE8' $ OrdLine.DESC1),m.Qty8,OrdLine.Qty8)
        m.TotQTY = m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8

        SELECT (lcOrdLine)
        m.DESC1 = OrdLine.DESC1
        GATHER MEMVAR MEMO
      ENDIF
    ENDIF

    *-- Case of Delete Item from the order
  CASE cLineStat = 'DI'
    IF SEEK('O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE ,'ORDLINE1','ORDLINST')
      llFound=.T.
      IF OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion # Ordline1.Pack_id+Ordline1.cPkColor+Ordline1.cPckSize+Ordline1.cPkVersion
        llFound = .F.
        SELECT Ordline1
        SCAN REST WHILE cOrdType+ORDER+STORE+STYLE = 'O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE FOR;
            Pack_id+cPkColor+cPckSize+cPkVersion = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
          llFound=.T.
          EXIT
        ENDSCAN
        SELECT OrdLine
      ENDIF
      IF llFound
        IF !SEEK('O' + Ordline1.ORDER + STR(Ordline1.LINENO , 6),lcOrdLine)
          SELECT Ordline1
          SCATTER MEMVAR MEMO
          STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQTY
          m.Flag = 'M'
          m.cLineStat = 'DI'
          INSERT INTO (lcOrdLine) FROM MEMVAR
        ENDIF
      ENDIF
    ENDIF
    *-- Case of Price Changed
  CASE cLineStat = 'PC'

    *B607460,1 HBG 07/09/2003 when updating 860 the SuRetPri not stored [Begin]
    *IF EMPTY(ORDLINE.Store) .AND. ORDHDR.Multi = "Y"
    IF EMPTY(OrdLine.STORE) .AND. ORDHDR.MULTI <> "Y"
      *B607460,1 [End]
      SELECT Ordline1
      =SEEK('O' + ORDHDR1.ORDER)
      SCAN REST WHILE cOrdType + ORDER + STORE + STYLE + STR(LINENO,6) = 'O' + ORDHDR1.ORDER;
          FOR STYLE = OrdLine.STYLE

        IF !SEEK('O' + Ordline1.ORDER + STR(Ordline1.LINENO , 6) , lcOrdLine)
          APPEND BLANK IN (lcOrdLine)
        ENDIF

        SCATTER MEMVAR MEMO

        *Sameh Modifications for Dillard Mappings
        *m.Price     = ORDLINE.Price
        m.PRICE     = IIF(OrdLine.PRICE=0,m.PRICE,OrdLine.PRICE)

        m.Flag      = 'M'
        m.Disc_Pcnt = IIF(m.Gros_Price = 0 , 0 ,100 - m.PRICE * 100 / m.Gros_Price)
        *B607460,1 HBG 07/09/2003 when updating 860 the SuRetPri not stored [Begin]
        m.nPkSlPrice = OrdLine.nPkSlPrice
        m.nSugRetPri = OrdLine.nSugRetPri
        *B607460,1 [End]
        SELECT (lcOrdLine)
        GATHER MEMVAR MEMO

        SELECT Ordline1
      ENDSCAN
    ELSE
      *B607460,1 HBG 07/09/2003 when update 860 the change of the price is not updated [Begin]
      lcPrevOrd = ORDER('ORDLINE1')
      IF !EMPTY(OrdLine.Pack_id)
        SET ORDER TO Ordblkst IN Ordline1
        IF SEEK('O' + ORDHDR1.ORDER + OrdLine.STYLE + OrdLine.Pack_id + OrdLine.cPkColor+ ;
            OrdLine.cPckSize + OrdLine.cPkVersion + OrdLine.STORE,'ORDLINE1','Ordblkst')
          IF !SEEK('O' + Ordline1.ORDER + STR(Ordline1.LINENO , 6),lcOrdLine)
            APPEND BLANK IN (lcOrdLine)
          ENDIF
          SELECT Ordline1
          SCATTER MEMVAR MEMO

          *m.Price     = ORDLINE.Price
          m.PRICE     = IIF(OrdLine.PRICE=0,m.PRICE,OrdLine.PRICE)

          m.Flag      = 'M'
          m.Disc_Pcnt = IIF(m.Gros_Price = 0 , 0 ,100 - m.PRICE * 100 / m.Gros_Price)

          m.nPkSlPrice = OrdLine.nPkSlPrice
          m.nSugRetPri = OrdLine.nSugRetPri

          SELECT (lcOrdLine)
          GATHER MEMVAR MEMO
        ENDIF
      ELSE
        *B607460,1 [End]
        SET ORDER TO ORDLINST IN Ordline1
        IF SEEK('O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE ,'ORDLINE1','ORDLINST')
          IF !SEEK('O' + Ordline1.ORDER + STR(Ordline1.LINENO , 6),lcOrdLine)
            APPEND BLANK IN (lcOrdLine)
          ENDIF
          SELECT Ordline1
          SCATTER MEMVAR MEMO

          *m.Price     = ORDLINE.Price
          m.PRICE     = IIF(OrdLine.PRICE=0,m.PRICE,OrdLine.PRICE)

          m.Flag      = 'M'
          m.Disc_Pcnt = IIF(m.Gros_Price = 0 , 0 ,100 - m.PRICE * 100 / m.Gros_Price)

          *B607460,1 HBG 07/09/2003 when updating 860 the SuRetPri not stored [Begin]
          m.nPkSlPrice = OrdLine.nPkSlPrice
          m.nSugRetPri = OrdLine.nSugRetPri
          *B607460,1 [End]

          SELECT (lcOrdLine)
          GATHER MEMVAR MEMO
        ENDIF
        *B607460,1 HBG 07/09/2003 End if this is a Pack [Begin]
      ENDIF
      *B607460,1 [End]
      SET ORDER TO &lcPrevOrd
    ENDIF

    *-- Case of Add Item to the order
  CASE cLineStat = 'AI'
    IF SEEK('O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE ,'ORDLINE1','ORDLINST') AND ;
        PRICE = Ordline1.PRICE .AND. OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion =;
        Ordline1.Pack_id+Ordline1.cPkColor+Ordline1.cPckSize+Ordline1.cPkVersion
      SELECT Ordline1
      SCATTER MEMVAR MEMO
      SELECT OrdLine
      m.Flag   = 'M'
      m.Qty1   = IIF(Qty1=0,m.Qty1,Qty1)
      m.Qty2   = IIF(Qty2=0,m.Qty2,Qty2)
      m.Qty3   = IIF(Qty3=0,m.Qty3,Qty3)
      m.Qty4   = IIF(Qty4=0,m.Qty4,Qty4)
      m.Qty5   = IIF(Qty5=0,m.Qty5,Qty5)
      m.Qty6   = IIF(Qty6=0,m.Qty6,Qty6)
      m.Qty7   = IIF(Qty7=0,m.Qty7,Qty7)
      m.Qty8   = IIF(Qty8=0,m.Qty8,Qty8)
      m.TotQTY = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    ELSE
      SCATTER MEMVAR MEMO
      m.Flag     = 'N'
      IF !EOF('Ordline1')
        SELECT Ordline1
        SCAN REST WHILE cOrdType+ORDER+STORE+STYLE = 'O' + ORDHDR1.ORDER + OrdLine.STORE + OrdLine.STYLE FOR;
            Pack_id+cPkColor+cPckSize+cPkVersion = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
          SCATTER MEMVAR MEMO
          m.Flag     = 'M'
          EXIT
        ENDSCAN
        SELECT OrdLine
      ENDIF
    ENDIF
    INSERT INTO (lcOrdLine) FROM MEMVAR
    *-- Case of Dept. Change
  CASE cLineStat = 'CA'
    REPLACE DEPT WITH ORDHDR.DEPT IN (lcOrdHdr)
  ENDCASE

  *:****************************************************************************************
  *! Name      : lfGtChkDgt
  *! Developer : Hend Ghanem
  *! Date      : 29/06/2003
  *! Purpose   : Get Check Digit
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200579,1
FUNCTION lfChkDigit

  LOCAL      lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
  STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

  MUCB = SUBSTR(MUCB,1,16)
  lnTop = LEN(MUCB)

  FOR lnCount = 1 TO lnTop STEP 2
    lnSumOdd  = lnSumOdd  + VAL(SUBSTR(MUCB,lnCount     , 1))
    lnSumEven = lnSumEven + VAL(SUBSTR(MUCB,lnCount + 1 , 1))
  ENDFOR

  lnChkDigit = MOD(lnSumOdd + (lnSumEven*3) , 10)

  MUCB = MUCB + IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1))

  *:****************************************************************************************
  *! Name      : lfAssPck
  *! Developer : Hend Ghanem
  *! Date      : 29/06/2003
  *! Purpose   : Send Pack Details in SLN segment in 856
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200592,1
FUNCTION lfAssPck


  SELECT SPCK_LIN
  lcTest = OrdLine.Pack_id+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
  SET ORDER TO Spck_linvr
  IF SEEK('P'+MACCOUNT+lcTest) OR SEEK('P'+'*****'+lcTest)
    lcPackAcc = Account
    IF !USED('SPCK_LIN1')
      USE (oAriaApplication.DataDir+ 'SPCK_LIN') IN 0 ORDER TAG 'Spcklins' SHARED ALIAS SPCK_LIN1 AGAIN
    ELSE
      SELECT SPCK_LIN1
      SET ORDER TO TAG SPCKLINS
    ENDIF
    STORE 0 TO MSLNCOUNTER
    lcStyOrd = ORDER('Style')
    lcSclOrd = ORDER('Scale')
    SET ORDER TO STYLE IN STYLE
    SET ORDER TO SCALE IN SCALE
    SCAN REST WHILE TYPE+Account+Pack_id+cPkColor+cPckSize+cPkVersion = 'P'+lcPackAcc+lcTest
      =SEEK(SPCK_LIN.STYLE,'Style') AND SEEK('S'+STYLE.SCALE,'Scale')
      FOR LNSLNCOUNT1 = 1 TO SCALE.CNT
        STORE ' ' TO MSLNSKU,MSLNUPC
        STORE 0 TO MSlnQty
        LCSLNCOUNT1 =STR(LNSLNCOUNT1,1)
        SELECT SPCK_LIN
        IF Qty&LCSLNCOUNT1. <> 0
          MSlnQty     = Qty&LCSLNCOUNT1.
          MSLNCOUNTER = MSLNCOUNTER + 1
          MSLNSIZE    = SCALE.SZ&LCSLNCOUNT1.
          SELECT SPCK_LIN1
          =SEEK('S'+lcPackAcc+SPCK_LIN.STYLE,'Spck_Lin1','SPCKLINS')
          SCAN REST WHILE TYPE+Account+STYLE+Pack_id = 'S'+lcPackAcc+SPCK_LIN.STYLE
            FOR LNSLNCOUNT2 = 1 TO 8
              LCSLNCOUNT2 =STR(LNSLNCOUNT2,1)
              IF Qty&LCSLNCOUNT2. = 1 AND LNSLNCOUNT2 = LNSLNCOUNT1
                MSLNSKU = Pack_id
                EXIT
              ENDIF
            ENDFOR
          ENDSCAN
          IF SEEK(SPCK_LIN.STYLE+LCSLNCOUNT1,'STYLEUPC','STYLEUPC')
            MSLNUPC=StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
          ENDIF
          llWriteDoc = IIF(oField2Seg.DO(lnFilePoint,'061','SycEdiSH',lcMapS),llWriteDoc,.F.)
        ENDIF
      ENDFOR
    ENDSCAN
    SELECT SPCK_LIN1
    USE
    SET ORDER TO &lcStyOrd IN STYLE
    SET ORDER TO &lcSclOrd IN SCALE
    SELECT SPCK_LIN

  ENDIF

  *:****************************************************************************************
  *! Name      : lfUpdConf
  *! Developer : Hend Ghanem
  *! Date      : 29/06/2003
  *! Purpose   : Update conflct files
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *C200504,1
FUNCTION lfUpdConf

  SET ORDER TO ConflctL IN ConflctL
  SET ORDER TO ConflctH IN ConflctH
  IF SEEK(EDILIBDT.cFileCode+PADR(EDILIBDT.cEdiRef,15),'CONFLCTL')
    SELECT ConflctL
    SCAN REST WHILE cFileCode+CustPO+cPack = EDILIBDT.cFileCode+PADR(EDILIBDT.cEdiRef,15)
      IF nCounter > 0
        RETURN
      ENDIF
    ENDSCAN
    Delete FOR cFileCode+CustPO+cPack = EDILIBDT.cFileCode+PADR(EDILIBDT.cEdiRef,15)
    SELECT ConflctH
    Delete FOR cFileCode+CustPO = EDILIBDT.cFileCode+PADR(EDILIBDT.cEdiRef,15)
  ENDIF


  *:****************************************************************************************
  *! Name      : lfUpdConf
  *! Developer : Hend Ghanem
  *! Date      : 29/06/2003
  *! Purpose   : Update conflct files
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *B121104,1
FUNCTION lfUpdPkInf

  REPLACE cPkColor WITH StyleUpc.cPkColor,;
    cPkSize  WITH StyleUpc.cPkSize

  *:****************************************************************************************
  *! Name      : lfGetUccNO
  *! Developer : Hend Ghanem
  *! Date      : 29/06/2003
  *! Purpose   : Eet the UCC9 # from the new file EDICRTSQ
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *:B037982,1
FUNCTION lfGetUccNO

  lcAlias = ALIAS()
  IF !USED('EDICRTSQ')
    USE (oAriaApplication.DataDir+'EDICRTSQ') ORDER TAG PCKCRTSQ IN 0
  ENDIF
  IF SEEK(MPACKNO+STR(lnCarton,6),'EDICRTSQ')
    SELECT EDICRTSQ
    REPLACE Ship_date WITH Bol_Hdr.Ship_date
    lcUCC9 = EDICRTSQ.UCC9
  ENDIF
  SELECT (lcAlias)

  *:****************************************************************************************
  *! Name      : lfUpdUccNo
  *! Developer : Noha Mohamed
  *! Date      : 04/27/2004
  *! Purpose   :
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *:B037982,1

FUNCTION lfUpdUccNo
  LOCAL lcAlias, lcAccount

  IF oFormSet.AriaForm1.cboAccType.VALUE ='A'
    lcAlias   = ALIAS()
    lcAccount = PADR(oFormSet.AriaForm1.keyCustomerCode.Keytextbox.VALUE,5)
    IF !USED('EDICRTSQ')
      USE (oAriaApplication.DataDir+'EDICRTSQ') ORDER TAG EDICRTSQ IN 0
    ENDIF
    IF SEEK('A'+lcAccount,'EDIACPRT','Accfact') AND EMPTY(EdiAcPrt.UCC9)
      SELECT MAX(EDICRTSQ.UCC9) AS UCC9 FROM EDICRTSQ WHERE Account = lcAccount INTO CURSOR lcTempUcc
      SELECT lcTempUcc
      LOCATE
      IF !EOF()
        SELECT EdiAcPrt
        REPLACE EdiAcPrt.UCC9 WITH lcTempUcc.UCC9
      ELSE
        SELECT EdiAcPrt
        REPLACE EdiAcPrt.UCC9 WITH '000000000'
      ENDIF
    ENDIF
    SELECT (lcAlias)
  ENDIF

  *:****************************************************************************************
  *! Name      : lfCrtFile
  *! Developer : hend GhaNEM
  *! Date      : 05/27/2004
  *! Purpose   :
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *:C123073
FUNCTION lfCrtFile

  CREATE TABLE (oAriaApplication.WorkDir+lcTmpPackLine) ;
    (nPltNo N(4), No_Cart N(4), SKU C(29), TotQTY N(6), OrgQty N(6), nShipQty N(6), nOpenQty N(6),;
    nOrdLineNo N(6), WEIGHT N(10,2), cUpc C(13), lAltStyle L, cCrtnVlTyp C(6), cCarrCtnId C(25))


  *:****************************************************************************************
  *! Name      : lfCrtFile
  *! Developer : hend GhaNEM
  *! Date      : 05/27/2004
  *! Purpose   :
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *:C123073
FUNCTION lfCHKARRY
  FOR lnRow = 1 TO lnTmpLbIfo
    *B1233480,1 WAM 07/18/2004 Fix label duplicate lines
    *IF laTmpLbIfo[lnRow, 9] = Pack_Lin.Style+Pack_Lin.Pack_Id+Pack_LIn.cPkColor+PACK_LIN.cPckSize .AND. ;
    *  laTmpLbIfo[lnRow, 6]  = lnCount
    IF laTmpLbIfo[lnRow, 9] = Pack_Lin.STYLE+Pack_Lin.Pack_id+Pack_Lin.cPkColor+Pack_Lin.cPckSize .AND. ;
        laTmpLbIfo[lnRow, 6]  = lnSizeCnt
      *B1233480,1 WAM (End)
      laTmpLbIfo[lnRow, 2] = laTmpLbIfo[lnRow, 2] + Pack_Lin.Qty&lcSz
      llFound              = .T.
      EXIT
    ENDIF
  ENDFOR

  *:****************************************************************************************
  *! Name      : lfCrtFile
  *! Developer : hend GhaNEM
  *! Date      : 05/27/2004
  *! Purpose   :
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *:C123073
FUNCTION lfADDPCKKEY

  laTmpLbIfo[lnTmpLbIfo,9] = Pack_Lin.STYLE+Pack_Lin.Pack_id+Pack_Lin.cPkColor+Pack_Lin.cPckSize


  *:****************************************************************************************
  *! Name      : lfCrtFile
  *! Developer : hend GhaNEM
  *! Date      : 05/27/2004
  *! Purpose   :
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *:C123073
FUNCTION lfASSINDEX


  lcIndex = 'PACKSTYPK'

  *:****************************************************************************************
  *! Name      : lfCrtFile
  *! Developer : hend GhaNEM
  *! Date      : 05/27/2004
  *! Purpose   :
  *:****************************************************************************************
  *! Called from :
  *:****************************************************************************************
  *:C123073
FUNCTION lfSETINDEX

  SET ORDER TO PACKSTYPK
  *:****************************************************************************************
  *! Name      : lfGTPO1UPC
  *! Developer : Waleed Hamed (WLD)
  *! Date      : 02/06/2005
  *! Purpose   : Get the UPC of PO1 for the partner WLM 4030
  *:****************************************************************************************
  *! Called from : UCC128 Label the screen (EBSHPLBL.SCX)
  *:****************************************************************************************
  *E124648,1
FUNCTION lfGTPO1UPC

  IF mVer='WLM'
    =SEEK(EdiAcPrt.cPartCode+'850','EDIPD','PARTTRANS')
    m.cUpc = LFREAD(9,ALLTRIM(Asn_Ship.CustPO)+ MSKU,'Detail','850','PO1')
  ENDIF

ENDFUNC

*:****************************************************************************************
*! Name      : LFCSTDIV
*! Developer : Amgad EL Halawany  (AEH)
*! Date      : 05/19/2005
*! Purpose   : REPLACE THE ORDHDR.CDIVISION WITH THE MCUST_DIV
*:****************************************************************************************
FUNCTION lfCstDiv
  REPLACE CCUSTDIV WITH MCUST_DIV
ENDFUNC
*:****************************************************************************************
*! Name      : lfGETCSKU
*! Developer : Abdelaziz Awdallah (AAH)
*! Date      : 03/14/2006
*! Purpose   : Get the SKU of Packs which store into ASN_SHIP.CSKU
*:****************************************************************************************
*! Called from : Visual UCC128 Label the screen (EBVSHPLB.SCX)
*:****************************************************************************************
*B131412,AAH,adjust variable of SKU ,03/08/2006 [Begin]
FUNCTION lfGETCSKU
  MSKU=M.cSku
ENDFUNC
*B131412,AAH,adjust variable of SKU ,03/08/2006 [End]
*****************************************************************************************