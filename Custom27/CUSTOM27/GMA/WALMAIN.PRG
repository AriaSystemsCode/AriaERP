*:****************************************************************************************
*: Program desc. : Main Program.
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
IF lRange_Process AND !EMPTY(cBlkOrd)
  *&& Seek For Bulk Order.
	  SELECT ORDHDR
  lnStQty_Hld = lnQty
  IF SEEK(PADR(ALLTRIM(MACCOUNT),5)+'O'+cBlkOrd,'ORDHDR','ORDACCT')
    lminFlag = .F.
    *  SCAN REST WHILE Account+cOrdType+ORDER = PADR(ALLTRIM(MACCOUNT),5)+'O'+cBlkOrd
    *&& Scan For Bulk Order Lines.
    SELECT OrdLine
    =SEEK('O'+cBlkOrd,'OrdLine','OrdLine')
    SCAN REST WHILE cOrdType+ORDER+STR(LINENO,6) = "O"+cBlkOrd
      *&& If This Line Still Have Qty Open, Get This Qty To Use In Coming EDI Order.
      *wab
      *IF TotBook>0  
      IF TotBook>0  AND SEEK(MTYPE+MKEY+Ordline.Style,'Spck_Lin','Spck_LinVr')
      *wab
        *&&H Use OrdLine Again Alias "BlkOrdLn"
        *&& Sum The Temparary Booked Qty From This Bulk Order Line.
        STORE 0 TO nTotBook_Sum,Book1_Sum,Book2_Sum,Book3_Sum,Book4_Sum,Book5_Sum,Book6_Sum,Book7_Sum,Book8_Sum
        SELECT BlkOrdLn
        =SEEK('T'+cBlkOrd+STR(OrdLine.LINENO,6),'BlkOrdLn','OrdBulk')
        SUM REST  TotBook    ,Book1      ,Book2      ,Book3      ,Book4      ,Book5      ,Book6      ,Book7      ,Book8    ;
        TO     nTotBook_Sum,Book1_Sum,Book2_Sum,Book3_Sum,Book4_Sum,Book5_Sum,Book6_Sum,Book7_Sum,Book8_Sum;
        WHILE cOrdType+ORDER+STR(LINENO,6) = 'T'+cBlkOrd+STR(OrdLine.LINENO,6)

        *wab
        SELECT EDIORDLN
        lnTotUsdQty = 0
        STORE 0 TO lnUsedQty1,lnUsedQty2,lnUsedQty3,lnUsedQty4,lnUsedQty5,lnUsedQty6,lnUsedQty7,lnUsedQty8

        SUM  Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQTY  TO lnUsedQty1,lnUsedQty2,lnUsedQty3,lnUsedQty4,;
             lnUsedQty5,lnUsedQty6,lnUsedQty7,lnUsedQty8,lnTotUsdQty ;
             FOR EDIORDLN.Style +EDIORDLN.Pack_id = OrdLine.STYLE+SPCK_LIN.Pack_Id
        
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
        *wab

        IF nAvlblQty > 0
          *&&h Do While IIf(nAvlblQty < lnStQty,.T.,.F.)
          *&&h To End ALL the line Value
          lminFlag = IIF(nAvlblQty > lnStQty_Hld,.T.,.F.)
          *******************************************************************
          XSTYLE = OrdLine.STYLE
          XPACK  = SPCK_LIN.Pack_Id
          =SEEK(XSTYLE,'STYLE')
          *B803897,1 Update GL Link Codes
          m.gl_Sales = lcSlsLink+STYLE.cSlsGlLink
          m.gl_Sales = IIF(llLinkToGl AND SEEK('02'+m.gl_Sales,'GL_LINK'),m.gl_Sales,'DEFDEF')
          *B803897,1 (End)
          xSeason   = STYLE.Season
          xWareCode = IIF(EMPTY(lcDefWare),STYLE.cDefWare,lcDefWare)
          XScale    = STYLE.SCALE
          SELECT EDIORDLN
          IF !SEEK(PADR(lcDumStore , 8) + XSTYLE + XPACK)
            MLINE = MLINE + 1
            APPEND BLANK
            REPLACE Account    WITH MACCOUNT  ,;
            CustPO     WITH MCustPo   ,;
            STORE      WITH lcDumStore   ,;
            STYLE      WITH XSTYLE    ,;
            Pack_Id    WITH XPACK     ,;
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
            BulkLineNo WITH OrdLine.LINENO;
            cPkColor   WITH lcPack_Clr;
            cPckSize   WITH lcPack_Sz;
            cPkVersion WITH lcPack_Vrn;
            nPkSlPrice WITH MPck_Price

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
            *!*	              REPLACE  TOTQty   WITH TOTQty  + (lnStQty - lnStQty_Hld),;
            *!*	                PRICE    WITH PRICE   + IIF(lnPrice=0,PRICE,lnPrice)* (lnStQty - lnStQty_Hld) ,;
            *!*	                NOTE_MEM WITH MLNOTES + CHR(13) ADDI
            *!*	              REPLACE  TotBook WITH TotBook + (lnQty - lnStQty_Hld)
            *!*	              MBOOK    = MBOOK + (lnQty - lnStQty_Hld)
            *!*	              MBOOKAMT = MBOOKAMT +PRICE * (lnQty - lnStQty_Hld)
            REPLACE  TOTQty   WITH TOTQty  + lnStQty_Hld,;
            PRICE    WITH PRICE   + IIF(lnPrice=0,PRICE,lnPrice),;
            NOTE_MEM WITH MLNOTES + CHR(13) ADDI
            REPLACE  TotBook WITH TotBook + lnStQty_Hld

            MBOOK    = MBOOK + lnStQty_Hld
            MBOOKAMT = MBOOKAMT +PRICE * lnStQty_Hld
            lnStQty_Hld = 0
            *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[End]
          ELSE
            *B606131,1 Adjust Qty in case Of Range Pack  Hassan 06/19/2002[Begin]
            *!*	              REPLACE Qty1    WITH  Qty1   +(OrdLine.Book1-Book1_Sum),;
            *!*	                Qty2    WITH  Qty2   + (OrdLine.Book2-Book2_Sum),;
            *!*	                Qty3    WITH  Qty3   + (OrdLine.Book3-Book3_Sum),;
            *!*	                Qty4    WITH  Qty4   + (OrdLine.Book4-Book4_Sum),;
            *!*	                Qty5    WITH  Qty5   + (OrdLine.Book5-Book5_Sum),;
            *!*	                Qty6    WITH  Qty6   + (OrdLine.Book6-Book6_Sum),;
            *!*	                Qty7    WITH  Qty7   + (OrdLine.Book7-Book7_Sum),;
            *!*	                Qty8    WITH  Qty8   + (OrdLine.Book8-Book8_Sum),;
            *!*	                TOTQty  WITH  TOTQty + nTotBook_Sum,;
            *!*	                PRICE   WITH PRICE + IIF(lnPrice=0,PRICE,lnPrice)*nTotBook_Sum ,;
            *!*	                NOTE_MEM WITH MLNOTES + CHR(13) ADDI
            *!*	              *&&H ModiFy Price in Above Line

            *!*	              REPLACE Book1   WITH Book1   + (OrdLine.Book1-Book1_Sum),;
            *!*	                Book2   WITH Book2   + (OrdLine.Book2-Book2_Sum),;
            *!*	                Book3   WITH Book3   + (OrdLine.Book3-Book3_Sum),;
            *!*	                Book4   WITH Book4   + (OrdLine.Book4-Book4_Sum),;
            *!*	                Book5   WITH Book5   + (OrdLine.Book5-Book5_Sum),;
            *!*	                Book6   WITH Book6   + (OrdLine.Book6-Book6_Sum),;
            *!*	                Book7   WITH Book7   + (OrdLine.Book7-Book7_Sum),;
            *!*	                Book8   WITH Book8   + (OrdLine.Book8-Book8_Sum),;
            *!*	                TotBook WITH TotBook + nTotBook_Sum

            *!*	              MBOOK    = MBOOK + nTotBook_Sum
            *!*	              MBOOKAMT = MBOOKAMT +PRICE * nTotBook_Sum
            REPLACE Qty1    WITH  Qty1   +(OrdLine.Book1-Book1_Sum),;
            Qty2    WITH  Qty2   + (OrdLine.Book2-Book2_Sum),;
            Qty3    WITH  Qty3   + (OrdLine.Book3-Book3_Sum),;
            Qty4    WITH  Qty4   + (OrdLine.Book4-Book4_Sum),;
            Qty5    WITH  Qty5   + (OrdLine.Book5-Book5_Sum),;
            Qty6    WITH  Qty6   + (OrdLine.Book6-Book6_Sum),;
            Qty7    WITH  Qty7   + (OrdLine.Book7-Book7_Sum),;
            Qty8    WITH  Qty8   + (OrdLine.Book8-Book8_Sum),;
            TOTQty  WITH  TOTQty + nAvlblQty,;
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
          *WAB
          IF TotQty = 0
            DELETE
          ENDIF
          *wab
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
      ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_Id+;
      ' have not added all ,as Bulk Order no '+cBlkOrd +' Have not enough Quantity.')
    ENDIF
  ENDIF
ELSE
  * =lfPPack(lnQty , lcDumStore)
  IF !lMVersion
    = oObject.processPack(lnQty,lcDumStore)
  ENDIF
ENDIF
ENDFUN
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
*wab
*=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
=SEEK('P'+Mkey,'Spck_Hdr','Spck_HdrVr')
*wab
IF !Spck_HDR.lRange
  *B606131,1 No Change In Qty in case Of Range Pack  Hassan 06/19/2002[End]

  DO CASE
  CASE  Mord = 3 AND MPck_Price = lnPrice AND INLIST(MUOM,'EA')
    lcCust_View = "Pack"
    lcGma_View  = "Pack"

  CASE  Mord = 3 AND MPck_Price <> lnPrice AND INLIST(MUOM,'EA')
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
    lcCust_View = "Style"
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
          IF MPck_Price <> lnPrice

            *C200404,1 HBG 09/03/2002 If the qty sent is not evenly divisible by 
            *C200404,1                the pack qty , reject the PO [Begin]
            *lnQty = ROUND(lnQty/MPckQty,0)
            lnQty = lnQty/MPckQty
            IF MOD(lnQty,1) <> 0
              llReject = .T.
            ENDIF
            *C200404,1 [End]

            &lcGmaQty. = lnQty
            lcCust_View = "Style"
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
      *REPLACE cCust_View WITH lcCust_View;
              cGMA_View  WITH lcGma_View;
              cPkColor   WITH lcPack_Clr;
              cPckSize   WITH lcPack_Sz;
              cPkVersion WITH lcPack_Vrn;
              nPkSlPrice WITH MPck_Price
      REPLACE cCust_View WITH lcCust_View ;
              cGMA_View  WITH lcGma_View  ;
              cPkColor   WITH lcPack_Clr  ;
              cPckSize   WITH lcPack_Sz   ;
              cPkVersion WITH lcPack_Vrn  ;
              nPkSlPrice WITH MPck_Price  ;
              cPckAcc    WITH SUBSTR(MKEY,1,5)
      *C200404,1 [End]
      IF MPck_Price = lnPrice
        REPLACE PRICE WITH SPCK_LIN.nPck_Price
      ENDIF
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
*Parameters oObject

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

*CnfHdrAr ,CnfLinAr

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
*!*	*If MESSAGEBOX('Are you sure you do not want to apply this order to any bulk order?',4+32,_SCREEN.CAPTION)=6
*!*	do While .T.
*!*	  =gfBrowse("Order:H='Order#',Account,CustPo :H='Customer PO#',Style:H='Style',"+;
*!*	          "Start:H='Start date',Complete:H='Completion date',pack_id:H='Pack Id',"+;
*!*	          "cPkColor:H='Pack color',cPckSize:H='Pack size',cPkVersion:H='Pack version',"+;
*!*	          "price :H='Unit Price',totqty :H='Total Qty',cfromorder:H='Bulk Order'",;
*!*	          'Order Lines',lcOrdLine,.T.,.T.,'FFTF',.T.)
*!*
*!*	  Select lcOrdLine
*!*	  lIn =.F.
*!*	  Scan for Empty(cfromorder)
*!*	    lIn = .T.
*!*	    Exit
*!*	  endscan
*!*	  If lIn
*!*	    if MESSAGEBOX('One or some lines have no Bulk Order to deplete from'+chr(13)+'Are You Sure You Want to Continue?',4+32,_SCREEN.CAPTION)=6
*!*	      exit
*!*	    endif
*!*	  else
*!*	    exit
*!*	  EndIF
*!*	*=gfBrowse([Order:H='Order#',Account,CustPo :H='Customer PO#',;
*!*	            Start:H='Start date',Complete:H='Completion date',pack_id:H='Pack Id',;
*!*	            price:H='Unit Price',totqty :H='Total Qty',;
*!*	            cfromorder:H='Bulk Order' ],;
*!*	            'Order Lines',lcOrdLine,;
*!*	            .T.,.T.,'FFTF',.T.)
*!*	Enddo
DO FORM oAriaApplication.ScreenHome+"EBGmaAdp" WITH lcOrdLine
*C200349,1 Hassan [End]
RELEASE llgma_Browse
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
*Parameters oObject
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
DO CASE
  *1* Case UPC Search For Style With Confirmation Of SKU Exist In DB If Sent.
CASE llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') AND;
  !StyleUpc.lUPCPack .AND. (EMPTY(MStyle) OR SEEK('S' + MKey , 'Spck_Lin'))

  Mord = 5

  *2* Case UPC Search  For Pack With Confirmation Of SKU Exist In DB If Sent.
  * Note Generic  Pack  Has UPC Only,But No SKU
  * Note Specific Pack  Has SKU Only,But No UPC
CASE llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') .AND. StyleUpc.lUPCPack .AND. ;
  SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
*B606306,1 As Per Emad GMA Hassan [Begin]    
*!*	  IF EMPTY(MStyle) OR;
*!*	      (SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr')AND;
*!*	      SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
*!*	      Spck_HDR.CSKUTYPE="P")

*!*	    =SEEK(PADR(MUPC,13),'StyleUpc')
*!*	    =SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
*!*	    lcPack_Id  = Spck_HDR.Pack_Id
*!*	    lcPack_Sz  = Spck_HDR.cPckSize
*!*	    lcPack_Clr = Spck_HDR.cPkColor
*!*	    lcPack_Vrn = Spck_HDR.cPkVersion

*!*	    SELECT SPCK_LIN
*!*	    lcOrder = SET('Order')
*!*	    *SET ORDER TO Spck_linvr
*!*	    MKey  = '*****' + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
*!*	    MType = 'P'
*!*	    Mord  = 3
*!*	    =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
*!*	    *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
*!*	    SUM REST TOTQty  TO MPckQty  WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
*!*	    =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
*!*	    *SET ORDER TO SPCK_LIN
*!*	    MPck_Price = Spck_HDR.nPkSlPrice
*!*	  ELSE
*!*	    =FPUTS(EReport , ' SKU Send #:'+ALLTRIM(MStyle)+ 'is not in our DB.')
*!*	  ENDIF
  IF !EMPTY(MStyle) 
    If (SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr')AND;
        SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
        Spck_HDR.CSKUTYPE="P")
      *BXXX Hassan 08/27/2002[Begin]
      *=SEEK(PADR(MUPC,13),'StyleUpc')
      *=SEEK('P' + '*****'+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
      
      =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
      *BXXX Hassan 08/27/2002 [End  ]
      lcPack_Id  = Spck_HDR.Pack_Id
      lcPack_Sz  = Spck_HDR.cPckSize
      lcPack_Clr = Spck_HDR.cPkColor
      lcPack_Vrn = Spck_HDR.cPkVersion
      
      SELECT SPCK_LIN
      lcOrder = SET('Order')
      *BXXX Hassan 08/27/2002 [Begin ]
      *SET ORDER TO Spck_linvr
      *MKey  = '*****' + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
      MKey  = PADR(ALLTRIM(MACCOUNT),5) + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
      *BXXX Hassan 08/27/2002 [End  ]
      MType = 'P'
      Mord  = 3
      *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
      IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
         AND !EMPTY(ConflctL.cPkVersion)
         MKey = PADR(ALLTRIM(MACCOUNT),5)+ lcPack_Id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
      ENDIF
      *C200404,1 [End]
      =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
      *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
      SUM REST TOTQty  TO MPckQty  WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
      =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
      *SET ORDER TO SPCK_LIN
      MPck_Price = Spck_HDR.nPkSlPrice
    ELSE
      =FPUTS(EReport , ' SKU Send #:'+ALLTRIM(MStyle)+ 'is not in our DB.')
    ENDIF
  Else
    If llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') .AND. StyleUpc.lUPCPack .AND. ;
      SEEK('P' + PADR(ALLTRIM(MACCOUNT),5) +PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')
      
      lcPack_Id  = Spck_HDR.Pack_Id
      lcPack_Sz  = Spck_HDR.cPckSize
      lcPack_Clr = Spck_HDR.cPkColor
      lcPack_Vrn = Spck_HDR.cPkVersion
      
      SELECT SPCK_LIN
      lcOrder = SET('Order')
      *SET ORDER TO Spck_linvr
      MKey  = PADR(ALLTRIM(MACCOUNT),5) + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
      MType = 'P'
      Mord  = 3
      *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
      IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
         AND !EMPTY(ConflctL.cPkVersion)
         MKey = PADR(ALLTRIM(MACCOUNT),5)+ lcPack_Id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
      ENDIF
      *C200404,1 [End]
      =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
      *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
      SUM REST TOTQty  TO MPckQty  WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
      =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
      *SET ORDER TO SPCK_LIN
      MPck_Price = Spck_HDR.nPkSlPrice
    Else
     If llUPCUse .AND. SEEK(PADR(MUPC,13),'StyleUpc') .AND. StyleUpc.lUPCPack .AND. ;
        SEEK('P' + '*****' +PADR(ALLTRIM(StyleUpc.STYLE),16)+StyleUpc.cPkColor+StyleUpc.cPkSize,'Spck_Hdr','Spck_hdrvr')      
        
      lcPack_Id  = Spck_HDR.Pack_Id
      lcPack_Sz  = Spck_HDR.cPckSize
      lcPack_Clr = Spck_HDR.cPkColor
      lcPack_Vrn = Spck_HDR.cPkVersion
      
      SELECT SPCK_LIN
      lcOrder = SET('Order')
      *SET ORDER TO Spck_linvr
      MKey  = '*****' + lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
      MType = 'P'
      Mord  = 3
      *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
      IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
         AND !EMPTY(ConflctL.cPkVersion)
         MKey = '*****'+ lcPack_Id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
      ENDIF
      *C200404,1 [End]
      =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
      *SUM REST TOTQty ,nPck_Price TO MPckQty ,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
      SUM REST TOTQty  TO MPckQty  WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
      =SEEK('P' + MKey,'Spck_Lin','Spck_linvr')
      *SET ORDER TO SPCK_LIN
      MPck_Price = Spck_HDR.nPkSlPrice
      
     EndIF   
    EndIF  
  EndIF  
*B606306,1 As Per Emad GMA Hassan [End]

  *CXXXX As Per Emad GMA Hassan [Begin]
  *4* Case SKU Search  For Pack For GMA
  * Note Generic  Pack  Has UPC Only,But No SKU
  * Note Specific Pack  Has SKU Only,But No UPC
  *!*	CASE SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Hdr','Spck_HdrVr')AND Spck_HDR.CSKUTYPE="P"

  *!*	  lcPack_Id  = ALLTRIM(Spck_HDR.STYLE)
  *!*	  lcPack_Sz  = Spck_HDR.cPckSize
  *!*	  lcPack_Clr = Spck_HDR.cPkColor
  *!*
  *!*	  =SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
  *!*	  lcPack_Id  = Spck_HDR.Pack_Id
  *!*	  lcPack_Vrn = Spck_HDR.cPkVersion
  *!*	  SELECT SPCK_LIN
  *!*	  *SET ORDER TO Spck_linvr
  *!*	  MKey  = MACCOUNT + Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn
  *!*	  MType = 'P'
  *!*	  Mord  = 3
  *!*	  =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
  *!*	  *SUM REST TOTQty,nPck_Price TO MPckQty,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
  *!*	  SUM REST TOTQty TO MPckQty WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
  *!*	  =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
  *!*	  *SELECT SPCK_LIN
  *!*	  *SET ORDER TO SPCK_LIN
  *!*	  MPck_Price = Spck_Hdr.nPkSlPrice
CASE SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+PADR(ALLTRIM(MStyle ),16),'Spck_Lin','Spck_LinVr')AND;
  SEEK('S' + PADR(ALLTRIM(MACCOUNT),5)+SPCK_LIN.STYLE+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize,'Spck_Hdr','Spck_Hdrst')AND;
  Spck_HDR.CSKUTYPE="P"

  lcPack_Id  = ALLTRIM(Spck_HDR.STYLE)
  lcPack_Sz  = Spck_HDR.cPckSize
  lcPack_Clr = Spck_HDR.cPkColor

  =SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
  lcPack_Id  = Spck_HDR.Pack_Id
  lcPack_Vrn = Spck_HDR.cPkVersion
  SELECT SPCK_LIN
  *SET ORDER TO Spck_linvr
  *ahmed (Start)
  
  *MKey  = MACCOUNT + Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn
  MKey  = PADR(ALLTRIM(MACCOUNT),5) + Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn
  *ahmed (End)
  
  MType = 'P'
  Mord  = 3
  *C200404,1 HBG 09/03/2002 Get the version which selected in confilct screen [Begin]
  IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(Spck_HDR.Pack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL');
    AND !EMPTY(ConflctL.cPkVersion)
    MKey = PADR(ALLTRIM(MACCOUNT),5)+ Spck_HDR.Pack_Id +lcPack_Clr+lcPack_Sz+ConflctL.cPkVersion
    lcPack_vrn= ConflctL.cPkVersion
  ENDIF
  *C200404,1 [End]
  
  =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
  *SUM REST TOTQty,nPck_Price TO MPckQty,MPck_Price WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
  SUM REST TOTQty TO MPckQty WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P' + MKey
  =SEEK('P' + PADR(ALLTRIM(MACCOUNT),5)+Spck_HDR.Pack_Id+lcPack_Clr+lcPack_Sz+lcPack_Vrn,'Spck_Lin','Spck_linvr')
  *SELECT SPCK_LIN
  *SET ORDER TO SPCK_LIN
  MPck_Price = Spck_HDR.nPkSlPrice

  *3* Case SKU Search  For Style
CASE SEEK('S' + MKey , 'Spck_Lin')
  MType   = 'S'
  MPrepck = (SPCK_LIN.TOTQty = 0)
  Mord    = IIF(SPCK_LIN.TOTQty=0,2,1)
  *CXXXX As Per Emad GMA Hassan [End]
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
  =FPUTS(EReport , ' SKU#: '+ALLTRIM(MStyle))
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
  =FPUTS(EReport , ' SKU#: '+ALLTRIM(MStyle))
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
*:****************************************************************************************
*! Name      : lfChkRng
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
FUNCTION lfChkRng
lRange_Process = .F.
cBlkOrd        = ""
IF Mord = 3
  *wab
  *=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
  =SEEK('P'+Mkey ,'Spck_Hdr','Spck_HdrVr')
  *wab
  IF Spck_HDR.lRange
    *&&H Check  'ConflictLin' File Is Used
    *&& Check If The Line In The Conflict File
    IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
      *&&Check If The Conflict Resolve
      IF !EMPTY(ConflctL.cBlkOrd)
        *&& Move this line to 'ConfLinAr'
        lcAlias = SELECT()
        SELECT ConflctL
        *&& Proceed To Process Range Pack Order(1)
        lRange_Process = .T.
        cBlkOrd = ConflctL.cBlkOrd
        *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[Begin]
        *!*SCATTER TO MEMVAR MEMO
        *!*SELECT CnfLinAr
        *!*APPEND BLANK
        *!*GATHER FROM MEMVAR
        *!*SELECT ConflctL
        *!*DELETE
        Do Case
        *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
        *Case nCounter = 1 
        Case ConflctL.nCounter = 1 
        *C200404,1 [End]
          SCATTER TO MEMVAR MEMO
          SELECT CnfLinAr
          APPEND BLANK
          GATHER FROM MEMVAR
          SELECT ConflctL
          DELETE
        *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
        *Case nCounter > 1 
          *Replace nCounter With (nCounter -1) in ConflctL
        Case ConflctL.nCounter > 1 
          lcPrvAls = ALIAS()
          SELECT ConflctL
          Replace ConflctL.nCounter With (ConflctL.nCounter -1)
          SELECT (lcPrvAls)
        *C200404,1 [End]
        EndCase
        *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[End  ]
        *&& If This Line Is The Last Line Related with Header,Move Header Line
        IF !SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
          SELECT ConflctH
          SCATTER TO MEMVAR MEMO
          SELECT CnfHdrAr
          APPEND BLANK
          GATHER FROM MEMVAR
          SELECT ConflctH
          DELETE
        ENDIF &&!Seek( lcFileCode + MCustPo +SPCK_HDR.Pack_Id,'ConflctL','ConflctL')
        SELECT(lcAlias)
      ELSE
        *&& Increase 'nCounter'
        RFLAG = .F.
        REPLACE ConflctL.nCounter WITH ConflctL.nCounter + 1
      ENDIF
    ELSE
      *&&H Put The Line In The Conflict File.
      RFLAG = .F.
      lcAlias = SELECT()
      SELECT ConflctH
      IF !SEEK(lcFileCode+MCustPo,'ConflctH','ConflctH')
        APPEND BLANK
        REPLACE cFileCode  WITH lcFileCode ;
        CustPO    WITH MCustPo;
        Account    WITH MACCOUNT ;
        RcveDate   WITH EdiLibDt.dDate;
        RcveTime   WITH EdiLibDt.cTime;
        ProcessDat WITH oAriaApplication.SystemDate;
        ProcessTim WITH gfGetTime();
        cAdd_User  WITH oAriaApplication.User_ID
      ENDIF
      SELECT ConflctL
      APPEND BLANK
      REPLACE cFileCode  WITH lcFileCode ;
      cPack      WITH Spck_HDR.Pack_Id+'-'+Spck_HDR.cPkColor+'-'+Spck_HDR.cPckSize ;
      CustPO    WITH MCustPo;
      lRange     WITH Spck_HDR.lRange  ;
      cUpc       WITH MUPC;
      cSku       WITH MStyle;
      cVendrNum  WITH MVend;
      nPCKQty    WITH MQTY;
      nPkSlPrice WITH MPri;
      cPkVersion WITH Spck_HDR.cPkVersion;
      nCounter   WITH 1;
      cAdd_User  WITH oAriaApplication.User_ID

      SELECT(lcAlias)
      *&& Put This Line In The Error Log.
      * =FPUTS(lnErorFile
      =FPUTS(EReport ,'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
      ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_Id+;
      ' Need to get bulk order to deplet from.')
    ENDIF &&Seek( lcFileCode + MCustPo + SPCK_HDR.Pack_Id,'ConflctL','ConflctL')
  ELSE
    *&& Proceed To Version Check.
    ****************************************************
    SELECT Spck_HDR
    *SET ORDER TO Spck_HdrVr
    *wab
    *=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
    *C200404,1 HBG 09/03/2002 Seek for the pack without specifing a version [Begin]
    *=SEEK('P'+Mkey,'Spck_Hdr','Spck_HdrVr')    
    lMAccount = SubStr(MKey,1,5)
    =SEEK('P'+PADR(ALLTRIM(lMAccount),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
    *C200404,1 [End]
    *wab
    *&&H Scan Rest While Pack_Id + Color +...
    *lcPack_Id  = Spck_HDR.Pack_Id
    *lcPack_Sz  = Spck_HDR.cPckSize
    *lcPack_Clr = Spck_HDR.cPkColor
    *lcPack_Ver = Spck_HDR.cPkVersion

    lMVersion = .F.
    *wab
    *BXXX Hassan [Begin]
    lMAccount = SubStr(MKey,1,5)
    SCAN REST WHILE  TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion=;
      'P'+lMACCOUNT+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz
    *SCAN REST WHILE  TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion= 'P'+Mkey
    *wab
      IF lcPack_Vrn <> Spck_HDR.cPkVersion
        lMVersion = .T.
        EXIT
      ENDIF
    ENDSCAN
    *wab
    *=SEEK('P'+PADR(ALLTRIM(MACCOUNT),5)+PADR(lcPack_Id,16)+lcPack_Clr+lcPack_Sz,'Spck_Hdr','Spck_HdrVr')
    =SEEK('P'+Mkey,'Spck_Hdr','Spck_HdrVr')
    *wab
    IF !lMVersion
      *&& Process Order(2)
    ELSE
      IF SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
        *&&Check If The Conflict Resolve
        IF !EMPTY(ConflctL.cPkVersion)
          *C200404,1 HBG 09/03/2002 reject the PO if the qty sent is not evenly 
          *C200404,1                divisible by the pack qty [Begin]
          IF llReject 
            IF RFLAG 
              =FPUTS(EReport ,'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
               ' CUST. PO#: ' + ALLTRIM(MCustPo)+;
               ' Rejected because the Qty sent for Pack Id: '+Mkey+;
               ' not divisable by the Pack qty.')
              RFLAG = .F.
            ENDIF   
            RETURN 
          ENDIF     
          *C200404,1 [End]

          lMVersion = .F.
          lcPack_Vrn = ConflctL.cPkVersion
          MKey = PADR(ALLTRIM(MACCOUNT),5)+ lcPack_Id +lcPack_Clr+lcPack_Sz+lcPack_Vrn
          *&& Move this line to 'ConfLinAr'
          lcAlias = SELECT()
          *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[Begin]
          *!*SELECT ConflctL
          *!*SCATTER TO MEMVAR MEMO
          *!*SELECT CnfLinAr
          *!*APPEND BLANK
          *!*GATHER FROM MEMVAR
          *!*SELECT ConflctL
          *!*DELETE
          Do Case
          *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
          *Case nCounter = 1
          Case ConflctL.nCounter = 1
          *C200404,1 [End]
            SELECT ConflctL
            SCATTER TO MEMVAR MEMO
            SELECT CnfLinAr
            APPEND BLANK
            GATHER FROM MEMVAR
            SELECT ConflctL
            DELETE
          *C200404,1 HBG 09/03/2002 Use nCounter field in ConflctL file [Begin]
          *Case nCounter > 1 
            *Replace nCounter With (nCounter -1) in ConflctL
          Case ConflctL.nCounter > 1 
            lcPrvAls = ALIAS()
            SELECT ConflctL
            Replace ConflctL.nCounter With (ConflctL.nCounter -1)
            SELECT (lcPrvAls)
          *C200404,1 [End]
          EndCase
          *B606350,1 Modifications In Conflict Mangment Hassan 08/08/2002[End  ]
          *&& If This Line Is The Last Line Related with Header,Move Header Line
          IF !SEEK( lcFileCode + PADR(MCustPo,15) +PADR(lcPack_Id,16)+'-'+lcPack_Clr+'-'+lcPack_Sz,'ConflctL','ConflctL')
            SELECT ConflctH
            SCATTER TO MEMVAR MEMO
            SELECT CnfHdrAr
            APPEND BLANK
            GATHER FROM MEMVAR
            SELECT ConflctH
            DELETE
          ENDIF
          SELECT(lcAlias)
        ELSE
          *&& Increase 'nCounter'
          RFLAG = .F.
          REPLACE ConflctL.nCounter WITH ConflctL.nCounter + 1
        ENDIF
      ELSE
        *&& Reject This Line
        *&& Put This Line In The Error Log.
        *&&H Put The Line In The Conflict File.
        RFLAG = .F.
        lcAlias = SELECT()
        SELECT ConflctH
        IF !SEEK(lcFileCode+MCustPo,'ConflctH','ConflctH')
          APPEND BLANK
          REPLACE cFileCode  WITH lcFileCode ;
          CustPO    WITH MCustPo;
          Account    WITH MACCOUNT ;
          RcveDate   WITH EdiLibDt.dDate;
          RcveTime   WITH EdiLibDt.cTime;
          ProcessDat WITH oAriaApplication.SystemDate;
          ProcessTim WITH gfGetTime();
          cAdd_User  WITH oAriaApplication.User_ID
        ENDIF
        SELECT ConflctL
        APPEND BLANK
        REPLACE cFileCode  WITH lcFileCode ;
        cPack      WITH Spck_HDR.Pack_Id+'-'+Spck_HDR.cPkColor+'-'+Spck_HDR.cPckSize ;
        CustPO    WITH MCustPo;
        lRange     WITH Spck_HDR.lRange  ;
        cUpc       WITH MUPC;
        cSku       WITH MStyle;
        cVendrNum  WITH MVend;
        nPCKQty    WITH MQTY;
        nPkSlPrice WITH MPri;
        nCounter   WITH 1;
        cAdd_User  WITH oAriaApplication.User_ID

        SELECT(lcAlias)
        * =FPUTS(lnErorFile ,
        =FPUTS(EReport , 'File Code#: '+lcFileCode+ ', ACCOUNT#: ' + MACCOUNT+;
        ' CUST. PO#: ' + ALLTRIM(MCustPo)+', Pack Id: '+Spck_HDR.Pack_Id+;
        ' Pack has multi Version And user should specify pack verion.')
      ENDIF
    ENDIF
    ****************************************************
  ENDIF
ENDIF
ENDFUN
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
MVend     = oGetMemVar.DO("XMANUFID",oAriaApplication.ActiveCompanyId)
llUPCUse  = oGetMemVar.DO("M_UPC_USE",oAriaApplication.ActiveCompanyId)='Y'
MTAXREFE  = oGetMemVar.DO("M_TAX_REFE",oAriaApplication.ActiveCompanyId)
MDUNS     = oGetMemVar.DO("XDUNS",oAriaApplication.ActiveCompanyId)
XDUNS = MDUNS
lcGenColor= PADR(oGetMemVar.DO("MCLRASSCOD",oAriaApplication.ActiveCompanyId),6)
lcTmpInvL = oGetTempName.DO()
lcOrdSum  = oGetTempName.DO()
RELEASE oGetMemVar
WAIT 'Sending Invoice# '+lcInv WINDOW NOWAIT
CREATE TABLE (oAriaApplication.WorkDir+lcTmpInvL) ;
(ORDER C(6),Account C(5),STYLE C(19),Pack_Id C(16),cPkColor C(6),cPckSize C(3),cPkVersion C(4), LINENO N(6),PRICE N(9,2), SCALE C(3), Edi_Pk_amt N(12,10),;
Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
Qty8 N(6),TOTQty N(7) ,Ord1 N(6),Ord2 N(6),Ord3 N(6),Ord4 N(6),Ord5 N(6),;
Ord6 N(6),Ord7 N(6),Ord8 N(6),TotOrd N(7),PrePak C(1),nSugRetPri N(8,2),nSugRetAmt N(12,10),nPkSlPrice N(8,2) )
INDEX ON Pack_Id+cPkColor+cPckSize+cPkVersion+STYLE TAG (lcTmpInvL)
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
lcPartner = ALLTRIM(EDIACPRT.cPartner)
MShpTODuns= ALLTRIM(EDIACPRT.Duns)
MFOB      = EDIACPRT.cEdiFOB
MREMIT    = EDIACPRT.cRemit
MUSERID   = ALLTRIM(EDIACPRT.cCmpISAId)
llInvUnShip = EDIACPRT.lInvUnShip

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
MSTORE    = InvHdr.STORE
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
IF 'AS' $ oAriaApplication.CompanyInstalledModules
  IF EMPTY(MBOL)
    SELECT Pack_Hdr
    =SEEK(MORDER+MSTORE)
    SCAN REST WHILE ORDER+STORE+PACK_NO = MORDER+MSTORE
      MBOL = Pack_Hdr.Bill_Ladg
    ENDSCAN
  ENDIF
  =SEEK(MBOL,'BOL_HDR')
  MUCB = PADL(ALLTRIM(MVend) , 7 , '0') + PADL(ALLTRIM(MBOL) , 9 , '0')
  MUCB = MUCB + oGetCheckDigit.DO(MUCB)
  RELEASE oGetCheckDigit
  MRMT_COD = Bol_Hdr.RMT_COD
  IF EMPTY(Bol_Hdr.ShipVia)
    MCARRIER = Bol_Hdr.Carrier
  ELSE
    =SEEK('N'+Bol_Hdr.ShipVia+'N'+'SHIPVIA','CODES','CODES')
    MCARRIER = CODES.cDiscRep
  ENDIF
  MCACODE = Bol_Hdr.CarrierCod
  MLOGFAC = Bol_Hdr.STORE
ENDIF

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

IF MCONSOL = 'Y'
  SELECT 'O'+CONSINVH.ORDER FROM CONSINVH WHERE CONSINVH.INVOICE = lcInv INTO ARRAY laOrders
  SELECT ORDER,STORE,STYLE,SUM(Book1) AS nBook1,SUM(Book2) AS nBook2,SUM(Book3) AS nBook3,SUM(Book4) AS nBook4,;
  SUM(Book5) AS nBook5,SUM(Book6) AS nBook6,SUM(Book7) AS nBook7,SUM(Book8) AS nBook8,SUM(TotBook) AS nTotBook ;
  FROM OrdLine WHERE ASCAN(laOrders,cOrdType+ORDER)<>0 GROUP BY STYLE INTO CURSOR (lcOrdSum)
  SELECT (lcOrdSum)
  SCAN
    IF !SEEK(STYLE+XInvoice,'INVLINE','Invlines') AND !llInvUnShip
      LOOP
    ENDIF
    lcKey = INVLINE.INVOICE+STORE+ORDER
    =SEEK(lcKey,'ConsInvH')
    SELECT INVLINE
    SCATTER MEMVAR
    m.Order = CONSINVH.ORDER
    IF EMPTY(m.Pack_Id)
      lcGenClrSty = SUBSTR(m.Style,1,lnClrPos-1)+lcGenColor+SUBSTR(m.Style,lnClrPos+lnCLrLen)
      IF llUPCUse .AND. SEEK(lcGenClrSty,'Style') .AND. ;
        !SEEK(m.Style,'StyleUpc') AND SEEK(lcGenClrSty,'StyleUpc')
        IF !SEEK(m.Pack_Id+m.cPkColor+m.cPckSize+m.cPkVersion+lcGenClrSty,lcTmpInvL)
          m.Edi_Pk_amt = m.TOTQty*m.PRICE
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
          TOTQty     WITH TOTQty+m.TOTQty      ,;
          Edi_Pk_amt WITH Edi_Pk_amt + m.TOTQty*m.PRICE ,;
          PRICE      WITH Edi_Pk_amt/TOTQty IN (lcTmpInvL)
        ENDIF
      ELSE
        INSERT INTO (lcTmpInvL) FROM MEMVAR
      ENDIF
    ELSE
      IF !SEEK(m.Pack_Id+m.cPkColor+m.cPckSize+m.cPkVersion+SPACE(19),lcTmpInvL)
        m.Style = ''
        m.Edi_Pk_amt = m.TOTQty*m.PRICE
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
        TOTQty     WITH TOTQty+m.TOTQty,;
        Edi_Pk_amt WITH Edi_Pk_amt+(m.TOTQty*m.PRICE) IN (lcTmpInvL)
        REPLACE &lcTmpInvL..PRICE WITH &lcTmpInvL..PRICE +(m.TOTQty*m.PRICE)
      ENDIF
    ENDIF
    IF llInvUnShip
      REPLACE Ord1   WITH Ord1 + &lcOrdSum..nBook1 ,;
      Ord2   WITH Ord2 + &lcOrdSum..nBook2 ,;
      Ord3   WITH Ord3 + &lcOrdSum..nBook3 ,;
      Ord4   WITH Ord4 + &lcOrdSum..nBook4 ,;
      Ord5   WITH Ord5 + &lcOrdSum..nBook5 ,;
      Ord6   WITH Ord6 + &lcOrdSum..nBook6 ,;
      Ord7   WITH Ord7 + &lcOrdSum..nBook7 ,;
      Ord8   WITH Ord8 + &lcOrdSum..nBook8 ,;
      TotOrd WITH TotOrd + &lcOrdSum..nTotBook IN (lcTmpInvL)
    ENDIF
  ENDSCAN
  USE IN (lcOrdSum)
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
    IF EMPTY(m.Pack_Id)
      lcGenClrSty = SUBSTR(m.Style,1,lnClrPos-1)+lcGenColor+SUBSTR(m.Style,lnClrPos+lnCLrLen)
      IF llUPCUse .AND. SEEK(lcGenClrSty,'Style') .AND. ;
        !SEEK(m.Style,'StyleUpc') AND SEEK(lcGenClrSty,'StyleUpc')
        IF !SEEK(m.cPckSize + m.cPkVersion + lcGenClrSty,lcTmpInvL)
          m.Edi_Pk_amt = m.TOTQty*m.PRICE
          m.nSugRetAmt = m.TOTQty*m.nSugRetPri
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
          TOTQty     WITH TOTQty+m.TOTQty      ,;
          Edi_Pk_amt WITH Edi_Pk_amt + m.TOTQty*m.PRICE ,;
          nSugRetAmt WITH nSugRetAmt + m.TOTQty*m.nSugRetPri ,;
          PRICE      WITH Edi_Pk_amt/TOTQty ,;
          nSugRetPri WITH nSugRetAmt/TOTQty IN (lcTmpInvL)
        ENDIF
      ELSE
        INSERT INTO (lcTmpInvL) FROM MEMVAR
      ENDIF
    ELSE
      IF !SEEK(m.Pack_Id+m.cPkColor+m.cPckSize+m.cPkVersion+SPACE(19),lcTmpInvL)
        m.Style = ''
        m.Edi_Pk_amt = m.TOTQty*m.PRICE
        m.nSugRetAmt = m.TOTQty*m.nSugRetPri
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
        TOTQty     WITH TOTQty+m.TOTQty,;
        nSugRetAmt WITH nSugRetAmt+(m.TOTQty*m.nSugRetPri) ,;
        Edi_Pk_amt WITH Edi_Pk_amt+(m.TOTQty*m.PRICE) IN (lcTmpInvL)
        REPLACE &lcTmpInvL..PRICE WITH &lcTmpInvL..PRICE +(m.TOTQty*m.PRICE)
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
SCAN FOR IIF(llInvUnShip,.T.,TOTQty <> 0)
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
  MKey = Account+STYLE
  IF EMPTY(Pack_Id)
    STORE STYLE TO XSKU1,XSKU2,XSKU3,XSKU4,XSKU5,XSKU6,XSKU7,XSKU8
    SELECT SPCK_LIN
    IF SEEK('S'+MKey)
      XSKU  ='**BY SIZE**'
      SCAN REST WHILE TYPE+Account+STYLE = 'S'+MKey
        IF lfOrgSku(ALLTRIM(Pack_Id))
          IF TOTQty = 0
            XSKU = SKU
            =SEEK('P'+MSCALE+&lcTmpInvL..PrePak,'Scale','Scale')
            XPPTOT = SCALE.PPTOT
            =SEEK('S'+MSCALE,'Scale')
          ELSE
            FOR lnCount = 1 TO SCALE.CNT
              lcCount = STR(lnCount,1)
              IF Qty&lcCount = 1
                XSKU&lcCount = Pack_Id + "-"+&lcTmpInvL..cPkColor+"-"+&lcTmpInvL..cPckSize
                EXIT
              ENDIF
            ENDFOR
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
      IF XSKU <> '**BY SIZE**' AND (SEEK('P'+MKey) OR SEEK('P'+'*****'+&lcTmpInvL..STYLE))
        XSKU = Pack_Id + "-"+&lcTmpInvL..cPkColor+"-"+&lcTmpInvL..cPckSize
      ENDIF
    ENDIF
  ELSE
    IF SEEK('S'+&lcTmpInvL..Account+PADR(&lcTmpInvL..Pack_Id,19)+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize,'SPCK_LIN','SPCK_LINST')
      XSKU = SPCK_LIN.Pack_Id
    ELSE
      lcScaleDesc = IIF(SEEK('S'+SUBSTR(&lcTmpInvL..cPckSize,1,1),'Scale','Scale'),EVAL("SCALE.sz" + ALLTRIM(STR(VAL(RIGHT(&lcTmpInvL..cPckSize,2))))),"")
      XSKU = ALLTRIM(Pack_Id) + "-"+ALLTRIM(&lcTmpInvL..cPkColor)+"-"+lcScaleDesc
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
    IF IIF(llInvUnShip,.T.,TOTQty > 0)
      XTOTQTY = TOTQty
      XPRICE  = PRICE
      XSugRetPri = nSugRetPri
      lnTotPcs = lnTotPcs + XTOTQTY
      XDiscPri = XPRICE*(100-XMERDISC)/100

      SELECT Spck_HDR
      SET ORDER TO TAG Spck_HdrVr
      SELECT SPCK_LIN
      SET ORDER TO TAG Spck_linvr
      lcActKey = 'P'+&lcTmpInvL..Account+&lcTmpInvL..Pack_Id+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion
      lcGenKey = 'P'+'*****'+&lcTmpInvL..Pack_Id+&lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion
      lc2Key   = &lcTmpInvL..cPkColor+&lcTmpInvL..cPckSize+&lcTmpInvL..cPkVersion

      IF SEEK(lcActKey) OR SEEK(lcGenKey)
        lcPackAcc = Account
        *B606211,1 Send UPC in Invoice Hassan [Begin]
        lcccPack_Id = PADR(Pack_Id,19)
        *B606211,1 Send UPC in Invoice Hassan [End  ]
        =SEEK('P'+lcPackAcc+PADR(LEFT(Pack_Id,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lc2Key,'SPCK_HDR')
        MStyle = Pack_Id + "-"+&lcTmpInvL..cPkColor+"-"+&lcTmpInvL..cPckSize
        MCOLOR = ''
        MSIZE  = ''
        MSTYDESC  = Spck_HDR.DESC
        SUM REST TOTQty TO lnPackQty ;
        WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion+STYLE = 'P'+lcPackAcc+&lcTmpInvL..Pack_Id+lc2Key

        XPCKQTY = IIF(lnPackQty<>0, XTOTQTY/lnPackQty, XTOTQTY)
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
        IF INT(XPCKQTY) <> XPCKQTY OR !EMPTY(xUpc)
          *B606211,1 Send UPC in Invoice Hassan [End  ]
          XPCKQTY   = 1
          *lnPackQty = 1
          *B606328,1 Adjust the qty in case of pack Hassan [Begin]
          XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 ,XTOTQTY,XPCKQTY)
          *B606328,1 Adjust the qty in case of pack Hassan [End   ]
        ELSE
          *B606328,1 Adjust the qty in case of pack Hassan [Begin]
          *XTOTQTY = XPCKQTY
          XTOTQTY = IIF(&lcTmpInvL..nPkSlPrice = 0 ,XTOTQTY,XPCKQTY)
          *B606328,1 Adjust the qty in case of pack Hassan [End   
        ENDIF
        *--Adel --Initiliaze Packs No
        lnPackQty =1
        *--Adel --Initiliaze Packs No
        *B606328,1 Adjust the qty in case of pack Hassan [Begin]
        *XPRICE      = &lcTmpInvL..nPkSlPrice
        XPRICE = IIF(XTOTQTY=0,XPRICE,&lcTmpInvL..EDI_PK_AMT/XTOTQTY)
        XPRICE = IIF(&lcTmpInvL..nPkSlPrice = 0 ,XPRICE,&lcTmpInvL..nPkSlPrice)
        *B606328,1 Adjust the qty in case of pack Hassan [End   ]
        XSugRetPri = IIF(XTOTQTY=0,XSugRetPri,&lcTmpInvL..nSugRetAmt/XTOTQTY)
        lnTOtAmnt  = lnTOtAmnt+ROUND(XTOTQTY*lnPackQty*ROUND(XPRICE/lnPackQty,2),2)
      ENDIF
      SET ORDER TO TAG SPCKLINS
      SELECT (lcTmpInvL)
      llWriteDoc = IIF(oField2Seg.DO(lnFilePoint,'04','SycEdiFS',lcMapS),llWriteDoc,.F.)
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
ERASE (oAriaApplication.WorkDir+lcTmpInvL+'.DBF')
ERASE (oAriaApplication.WorkDir+lcTmpInvL+'.CDX')
RELEASE oGetRltdFld
RELEASE oField2Seg
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

lnTotPcs  = Pack_Lin.TOTQty
llFound   = .F.

lcPkColor = OrdLine.cPkColor
lcPckSize = OrdLine.cPckSize
lcPkVrson = OrdLine.cPkVersion

IF !SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson  , 'Spck_Lin', 'Spck_LinVr') AND;
  !SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson  , 'Spck_Lin', 'Spck_LinVr')
  oObject.ProcessSizes()
  RETURN
ENDIF

FOR lnRow = 1 TO lnLblInfo
  IF laLblInfo[lnRow, 4]  = lcPackId ;
    .AND. LEFT(laLblInfo[lnRow, 1], 2) = "$#"

    laLblInfo[lnRow, 7] = laLblInfo[lnRow, 7] + lnTotPcs
    llFound                 = .T.
    *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [Begin]
    IF SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr') OR;
      SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr')

      laLblInfo[lnLblInfo, 8] = laLblInfo[lnLblInfo, 8] + SPCK_LIN.TOTQty
    ENDIF
    *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [End]
    EXIT
  ENDIF
ENDFOR

IF !llFound
  lnLblInfo = lnLblInfo + 1

  *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [Begin]
  *DIMENSION laLblInfo[lnLblInfo , 7]
  DIMENSION laLblInfo[lnLblInfo , 8]

  IF SEEK('P' + MACCOUNT + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr') OR;
    SEEK('P' + '*****'  + lcPackId+lcPkColor+lcPckSize+lcPkVrson + Pack_Lin.STYLE , 'Spck_Lin', 'Spck_linvr')
    laLblInfo[lnLblInfo, 8] = SPCK_LIN.TOTQty
  ELSE
    laLblInfo[lnLblInfo, 8] = 0
  ENDIF
  *C102510 Hassan Increase "laLblInfo" Dimension to hold inner pack qty varible [End]

  laLblInfo[lnLblInfo, 1] = "$#" + Spck_HDR.Account
  laLblInfo[lnLblInfo, 2] = 0
  laLblInfo[lnLblInfo, 3] = ""
  laLblInfo[lnLblInfo, 4] = lcPackId+lcPkColor+lcPckSize
  laLblInfo[lnLblInfo, 5] = ""
  laLblInfo[lnLblInfo, 6] = 0
  laLblInfo[lnLblInfo, 7] = lnTotPcs

  IF llUPCUse .AND.;
    ((SEEK(MACCOUNT + lcPackId+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack) .OR. ;
    (SEEK('*****'  + lcPackId+lcPkColor+lcPckSize , 'StyleUPC', 'GMAPKUPC') .AND. StyleUpc.lUPCPack))

    laLblInfo[lnLblInfo,5] = StyleUpc.cUpcNum1 + StyleUpc.cUpcNum2 + ;
    StyleUpc.cUpcNum3
  ENDIF
ENDIF
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
    SUM TOTQty REST;
    WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion+STYLE = 'P' + lcAccount + lcPackId;
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
REPLACE BOL_NO    WITH MBOL      ,;
PACK_NO   WITH MPACKNO   ,;
CART_NO   WITH lnCartNo  ,;
lPlt      WITH EDIPH.lPltShp ,;
ASN_VER   WITH IIF(EDIPH.lPltShp,EDIPH.CPLTLBL,IIF(Bol_Hdr.cToStOrCn='C' OR Pack_Hdr.cToStOrCn='C',EDIPH.cAsnLbl2,EDIPH.cAsnLbl1)),;
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
UCC9      WITH lcUCC9    ,;
UCC_CHECK WITH lcUCCCheck,;
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
TOTQty     WITH lnQtyPCrtn ,;
Pack_Id    WITH lcCrtnSku  ,;
cPkColor   WITH lcPkColor  ,;
cPckSize   WITH lcPckSize  ,;
cPkVersion WITH lcPkVrson  ,;
cUpc       WITH lcUpc      ,;
cAdd_User  WITH oAriaApplication.User_ID    ,;
dAdd_Date  WITH oAriaApplication.SystemDate ,;
cAdd_Time  WITH TIME()
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
  lcPackId  = OrdLine.Pack_Id
  lcPkColor = OrdLine.cPkColor
  lcPckSize = OrdLine.cPckSize
  lcPkVrson = OrdLine.cPkVersion
  IF EMPTY(lcPackId) AND ;
    ( SEEK('P' + lcAccount + STYLE, 'Spck_Lin') OR SEEK('P' + '*****' + STYLE,'Spck_Lin'))
    lcPackId = SPCK_LIN.Pack_Id
  ENDIF
  lcSize = ' '
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    IF Qty&lcCount = TOTQty
      lcSize = lcCount
    ENDIF
  ENDFOR
  lcStyle = SUBSTR(Pack_Lin.STYLE, 1, LEN(lcMjrPic))
  IF !SEEK(STR(Pack_Lin.nPltNo,4) + STR(Pack_Lin.No_Cart,4),lcCartons)
    INSERT INTO (lcCartons) (nPltNo          , No_Cart          , STYLE          , Major   , SIZE   ,Pack_Id  ,cPkColor , cPckSize, cPkVersion , TOTQty          );
    VALUES           (Pack_Lin.nPltNo , Pack_Lin.No_Cart , Pack_Lin.STYLE , lcStyle , lcSize ,lcPackId ,lcPkColor,lcPckSize, lcPkVrson  , Pack_Lin.TOTQty )
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
      Pack_Id    WITH IIF(Pack_Id=lcPackId, lcPackId, '') ,;
      cPkColor   WITH lcPkColor,;
      cPckSize   WITH lcPckSize,;
      cPkVersion WITH lcPkVrson,;
      TOTQty     WITH TOTQty + Pack_Lin.TOTQty IN (lcCartons)
    ELSE
      REPLACE STYLE      WITH IIF(STYLE = Pack_Lin.STYLE,STYLE,'') ,;
      Major      WITH IIF(Major=PADR(lcStyle,19),Major,'') ,;
      SIZE       WITH IIF(SIZE=lcSize,SIZE,'') ,;
      Pack_Id    WITH IIF(Pack_Id=lcPackId, lcPackId, '') ,;
      cPkColor   WITH lcPkColor,;
      cPckSize   WITH lcPckSize,;
      cPkVersion WITH lcPkVrson,;
      TOTQty     WITH TOTQty + Pack_Lin.TOTQty IN (lcCartons)
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
(nPltNo N(4), No_Cart N(4),STYLE C(19),Major C(19),SIZE C(1), TOTQty N(6),Pack_Id C(16),cPkColor C(6),cPckSize C(3),cPkVersion C(4))
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

IF !EMPTY(Pack_Id) .AND.;
  SEEK(MACCOUNT + PADR(Pack_Id , 19) + cPkColor+LEFT(cPckSize,1), 'StyleUpc' , 'GMAPKUPC') .AND.;
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
  =SEEK('O' + Pack_Hdr.ORDER + STR(PacK_lin.nOrdLineNo,6) , 'ORDLINE' , 'ORDLINE')
  *B606350 Hassan [End]
  *B804154,1 HS 05/02/2001 Use another index tag that doesn't contain... [End]

  lcGenSty = SUBSTR(STYLE,1,lnClrPos-1)+lcGenColor+SUBSTR(STYLE,lnClrPos+lnCLrLen)
  IF llUPCUse AND SEEK(lcGenSty,'Style') AND !SEEK(STYLE,'StyleUpc') .AND. ;
    SEEK(lcGenSty,'StyleUpc')
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF Qty&lcCount > 0 AND ;
        SEEK(lcGenSty+lcCount,'StyleUpc')
        IF !SEEK(PADR(StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,16)+;
          STR(nPltNo,4)+STR(No_Cart,4),lcTmpPackLine)
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
        REPLACE TOTQty    WITH TOTQty   + Pack_Lin.Qty&lcCount ,;
        OrgQty    WITH OrgQty   + OrdLine.Book&lcCount ,;
        nShipQty  WITH nShipQty + lnShipQty            ,;
        nOpenQty  WITH nOpenQty + OrdLine.Qty&lcCount  ,;
        WEIGHT    WITH WEIGHT   + Pack_Lin.WEIGHT/Pack_Lin.TOTQty*Pack_Lin.Qty&lcCount,;
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
  IF !EMPTY(OrdLine.Pack_Id) .AND. ;
    ( SEEK('P'+lcAccount+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_Id+Ordline.cpkversion,'SPCK_LIN','SPCK_LINST') OR ;
      SEEK('P'+'*****'+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_Id+Ordline.cpkversion,'SPCK_LIN','SPCK_LINST') )
  *B606328,1 Use New Index Hassan [End  ]
  
    IF !SEEK(OrdLine.Pack_Id+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
      *C200404,1 Pick SKU for the pack instead of pack_ID HASSAN 09/04/2002[Begin]
      *INSERT INTO (lcTmpPackLine) ;
      *(nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
      *(Pack_Lin.nPltNo,Pack_Lin.No_Cart,OrdLine.Pack_Id,Pack_Lin.nOrdLineNo)

      *type+account+style+cpkcolor+cpcksize+pack_id+cpkversion
      IF SEEK('S'+lcAccount+OrdLine.Pack_id+SPACE(3)+OrdLine.cPkColor+OrdLine.cPckSize,'SPCK_LIN','SPCK_LINST')
        INSERT INTO (lcTmpPackLine) ;
        (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
        (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.Pack_id,Pack_Lin.nOrdLineNo)

        = SEEK('P'+lcAccount+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST') OR ;
        SEEK('P'+'*****'+STYLE+OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.Pack_id+OrdLine.cPkVersion,'SPCK_LIN','SPCK_LINST')

      ELSE
        INSERT INTO (lcTmpPackLine) ;
        (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
        (Pack_Lin.nPltNo,Pack_Lin.No_Cart,OrdLine.Pack_id,Pack_Lin.nOrdLineNo)
      ENDIF
      *C200404,1 Pick SKU for the pack instead of pack_ID HASSAN 09/04/2002[End  ]
    ENDIF

    *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [Begin]
    lnShipQty  = 0
    lcOrdLinNo = STR(OrdLine.LINENO,6)
    IF SEEK(Pack_Hdr.ORDER + lcOrdLinNo , "InvLine" , "INVLINEO")
      SELECT INVLINE
      SUM TOTQty WHILE ORDER + STR(LINENO,6) = Pack_Hdr.ORDER + lcOrdLinNo TO lnShipQty
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
    REPLACE TOTQty    WITH TOTQty   + Pack_Lin.TOTQty ,;
    OrgQty    WITH OrgQty   + OrdLine.TotBook ,;
    nShipQty  WITH nShipQty + lnShipQty       ,;
    nOpenQty  WITH nOpenQty + OrdLine.TOTQty  ,;
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
        WHILE TYPE+Account+STYLE+Pack_Id = 'S'+lcAccount+Pack_Lin.STYLE ;
        FOR   (Qty&lcCount = 1 OR TOTQty = 0) AND oObject.OrgSku(ALLTRIM(Pack_Id))
        lcSku = IIF(FOUND(),Pack_Id,lcUpc)
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
          REPLACE TOTQty    WITH TOTQty   + Pack_Lin.Qty&lcCount ,;
          OrgQty    WITH OrgQty   + OrdLine.Book&lcCount ,;
          nShipQty  WITH nShipQty + lnShipQty            ,;
          nOpenQty  WITH nOpenQty + OrdLine.Qty&lcCount  ,;
          WEIGHT    WITH WEIGHT   + Pack_Lin.WEIGHT/Pack_Lin.TOTQty*Pack_Lin.Qty&lcCount,;
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
        IF !SEEK(SPCK_LIN.Pack_Id+STR(Pack_Lin.nPltNo,4)+STR(Pack_Lin.No_Cart,4),lcTmpPackLine)
          INSERT INTO (lcTmpPackLine) ;
          (nPltNo,No_Cart,SKU,nOrdLineNo) VALUES ;
          (Pack_Lin.nPltNo,Pack_Lin.No_Cart,SPCK_LIN.Pack_Id,Pack_Lin.nOrdLineNo)
        ENDIF

        *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [Begin]
        lnShipQty  = 0
        lcOrdLinNo = STR(OrdLine.LINENO,6)
        IF SEEK(Pack_Hdr.ORDER + lcOrdLinNo , "InvLine" , "INVLINEO")
          SELECT INVLINE
          SUM TOTQty WHILE ORDER + STR(LINENO,6) = Pack_Hdr.ORDER + lcOrdLinNo TO lnShipQty
        ENDIF
        *B804154,1 HS 05/02/2001 Get the shipped quantity for this line [End]

        *B804154,1 HS 05/02/2001 Add a new field "nShipQty" to hold the shipped
        *             quantity, a new field "lAltStyle" to know if any alternative
        *             styles were used and a new field "nOpenQty" to hold the Open
        *             quantity [Begin]
        *REPLACE TotQty with TotQty + Pack_Lin.TotQty ,;
        *        OrgQty with OrgQty + Ordline.TotBook ,;
        *        Weight WITH Weight + Pack_Lin.Weight IN (lcTmpPackLine)
        REPLACE TOTQty    WITH TOTQty   + Pack_Lin.TOTQty ,;
        OrgQty    WITH OrgQty   + OrdLine.TotBook ,;
        nShipQty  WITH nShipQty + lnShipQty       ,;
        nOpenQty  WITH nOpenQty + OrdLine.TOTQty  ,;
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
lcTest = OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
*B606328,1 Use New Index Hassan [Begin]
Set order to SPCK_LINVR
*B606328,1 Use New Index Hassan [End  ]
IF SEEK('P'+MACCOUNT+lcItem+lcTest) OR SEEK('P'+'*****'+lcItem+lcTest)
  MUOM = 'PK'
  MQtyPPack = 0
  lcPackAcc = Account
  *B606328,1 Use New Index Hassan [Begin]
  *Type+account+pack_id+cpkcolor+cpcksize+cpkversion
  *=SEEK('P'+lcPackAcc+PADR(LEFT(lcItem,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr')
  =SEEK('P'+lcPackAcc+PADR(LEFT(lcItem,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr','SPCK_HDRVR')
  *B606328,1 Use New Index Hassan [End  ]
  MSTYDESC = Spck_HDR.DESC
  SCAN REST WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P'+lcPackAcc+lcItem+lcTest
    MQtyPPack = MQtyPPack + TOTQty
    MPO401    = TOTQty
    =SEEK(STYLE,'Style')
    IF INLIST(lcPriLevel,'A','B','C')
      MPri = MPri + STYLE.PRICE&lcPriLevel * TOTQty
    ENDIF
  ENDSCAN
  IF llUPCUse AND SEEK(lcItem,'StyleUpc') AND StyleUpc.lUPCPack
    MUPC = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
  ENDIF
  *MInPckPCrt = &lcTmpPackLine..TotQty
  *MQtyPCrt   = MQtyPPack
  IF (!EMPTY(MUPC) AND llGMA) OR (INT(MQTY/MQtyPPack) <> MQTY/MQtyPPack)
    MQtyPPack  = 1
    MInPckPCrt = 1                   && # Inner packs per carton
    MQtyPCrt   = &lcTmpPackLine..TOTQty && Quantity per carton
  ELSE
    MQTY       = MQTY/MQtyPPack
    MORGQTY    = MORGQTY/MQtyPPack
    MInPckPCrt = &lcTmpPackLine..TOTQty
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
    IF TOTQty = 1
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
    MQtyPPack  = 1
    MPO401     = 1
    MInPckPCrt = 1                      && # Inner packs per carton
    MQtyPCrt   = &lcTmpPackLine..TOTQty && Quantity per carton
  ELSE
    IF llUPCUse AND SEEK(ALLTRIM(lcItem),'StyleUpc','StyUpcN')
      MUPC  = lcItem
      MSKU  = ''
      *C200180,1 Get Cathey Danial account style
      lcUpcStyle = StyleUpc.STYLE
      IF llCatDanial
        lcUpcStyle = SUBSTR(StyleUpc.STYLE,1,LEN(lcMjrPic))
        lcUpcStyle = PADR(ALLTRIM(lcUpcStyle)+EDIACPRT.cAccCode,LEN(lcMjrPic))
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
      MQtyPPack = 1
      MPO401    = 1
      MInPckPCrt= 1                      && # Inner packs per carton
      MQtyPCrt  = &lcTmpPackLine..TOTQty && Quantity per carton
    ENDIF
  ENDIF
ENDIF
*B606328,1 Use New Index Hassan [Begin]
Select Spck_Lin
Set Order to spck_LIN
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
lcTest = OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
IF SEEK('P'+MACCOUNT+PADR(&lcTmpPackLine..SKU,16)+lcTest) OR ;
  SEEK('P'+'*****' +PADR(&lcTmpPackLine..SKU,16)+lcTest)
  lcPackAcc = Account
  SCAN REST WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = ;
    'P'+lcPackAcc+PADR(&lcTmpPackLine..SKU,16)+lcTest
    =SEEK(STYLE,'Style')
    MQtyPPack  = MQtyPPack + TOTQty
    MWghtPPack = MWghtPPack + TOTQty*STYLE.nstyweight
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
lcTest = OrdLine.cPkColor+OrdLine.cPckSize+OrdLine.cPkVersion
*type+account+pack_id+cpkcolor+cpcksize+cpkversion
SELECT SPCK_LIN
*B606328,1 Use New Index Hassan [Begin]
Set Order to SPCK_LINVR
*B606328,1 Use New Index Hassan [End]
IF SEEK('P'+MACCOUNT+PADR(MSKU,16)+lcTest) OR SEEK('P'+'*****'+PADR(MSKU,16)+lcTest)
  MUOM = 'PK'
  MQtyPPack = 0
  lcPackAcc = Account
  *B606328,1 Use New Index Hassan [Begin]
  *=SEEK('P'+lcPackAcc+PADR(LEFT(MSKU,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr')
  =SEEK('P'+lcPackAcc+PADR(LEFT(MSKU,SKUTMPL.Len1+SKUTMPL.Len2+SKUTMPL.Len3),16)+lcTest,'Spck_Hdr','SPCK_HDRVR')
  *B606328,1 Use New Index Hassan [End]
  MSTYDESC = Spck_HDR.DESC
  SCAN REST WHILE TYPE+Account+Pack_Id+cPkColor+cPckSize+cPkVersion = 'P'+lcPackAcc+PADR(MSKU,16)+lcTest
    MQtyPPack = MQtyPPack + TOTQty
    MPO401 = TOTQty
    =SEEK(STYLE,'Style')
    IF INLIST(lcPriLevel,'A','B','C')
      MPri = MPri + STYLE.PRICE&lcPriLevel * TOTQty
    ENDIF
  ENDSCAN
  IF llUPCUse AND SEEK(PADR(MSKU,13),'StyleUpc') AND StyleUpc.lUPCPack
    MUPC = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
  ENDIF
  *MInPckPCrt = &lcTmpPackLine..TotQty
  *MQtyPCrt   = MQtyPPack
  *lnQtyPCrtn = lnQtyPCrtn + MQtyPPack*MInPckPCrt
  IF (!EMPTY(MUPC) AND llGMA) OR (INT(MQTY/MQtyPPack) <> MQTY/MQtyPPack)
    MQtyPPack  = 1
    MInPckPCrt = 1
    MPO401     = 1
    MQtyPCrt   = &lcTmpPackLine..TOTQty
    lnQtyPCrtn = lnQtyPCrtn + MQtyPCrt
  ELSE
    lnQtyPCrtn = lnQtyPCrtn + &lcTmpPackLine..TOTQty
    MQTY = MQTY/MQtyPPack
    MORGQTY = MORGQTY/MQtyPPack
    MInPckPCrt = &lcTmpPackLine..TOTQty
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
    IF TOTQty = 1
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
    MQtyPPack  = 1
    MInPckPCrt = 1
    MPO401     = 1
    MQtyPCrt   = &lcTmpPackLine..TOTQty
    lnQtyPCrtn = lnQtyPCrtn + MQtyPCrt
  ELSE
    IF llUPCUse .AND. SEEK(PADR(MSKU,13),'StyleUpc','StyUpcN')
      MUPC   = PADR(MSKU,13)
      MSKU   = ''
      *C200180,1 Get Cathey Danial account style
      lcUpcStyle = StyleUpc.STYLE
      IF llCatDanial
        lcUpcStyle = SUBSTR(StyleUpc.STYLE,1,LEN(lcMjrPic))
        lcUpcStyle = PADR(ALLTRIM(lcUpcStyle)+EDIACPRT.cAccCode,LEN(lcMjrPic))
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
      MQtyPPack  = 1
      MInPckPCrt = 1
      MPO401     = 1
      MQtyPCrt   = &lcTmpPackLine..TOTQty
      lcCrtnSku= ''
      lnQtyPCrtn = lnQtyPCrtn + MQtyPCrt
    ENDIF
  ENDIF
ENDIF
*B606328,1 Use New Index Hassan [Begin]
Select Spck_Lin
Set Order to spck_LIN
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
SELECT CONSINVH
IF SEEK(lcInvoice) .AND. SEEK('S'+lcAccount+STORE,'Customer') .AND. ;
  SEEK('S'+lcAccount+Customer.Dist_Ctr,'Customer')
  lcDistCntr = Customer.STORE
  SCAN REST WHILE INVOICE = lcInvoice
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
