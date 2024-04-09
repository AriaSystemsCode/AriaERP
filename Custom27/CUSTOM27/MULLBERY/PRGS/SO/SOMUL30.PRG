*:***************************************************************************
*: Program file  : ALMUL30.PRG
*: Program desc. : AUTOMATIC PACKING LIST FOR MULBERRY NECKWARE
*:                 FOR C'est Duo Int'l Ltd.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Sales Order Allocation (AL)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Mulberry NeckWare
*:***************************************************************************
*:C101625,1 KHM 11/10/99
*:B802927,1 ABD 04/24/2000 Made automatic packing list program rum from
*:B802927,1 ABD            Module Sales Order. 
*:B803540,1 HBG 08/01/2000 Fix bug of not Saving Pack Qty in OrdLine file 
*:***************************************************************************
*:B802927,1 ABD Made automatic packing list program rum from
*:B802927,1 ABD Module Sales Order. 
llModInst = ('AL' $ gcCmpModules  .OR. 'AS' $ gcCmpModules)
IF !llModInst 
  *B802927,1 Message No.    : 32081
  *B802927,1 Message Text   : XX modules are not installed. Cannot proceed.    
  *B802927,1 Button No.     :  00000   
  *B802927,1 Message Button : OK
  =gfModalGen('TRM32081B00000','DIALOG','Advanced Ship Notice and Sales Order Allocation')
  CLEAR READ
  RETURN
ENDIF
*:B802927,1 ABD [End]

*-- Initializing the necessary variables.
lcOrderNo  = SPACE(06)
lcAccount  = SPACE(05)
lcAccName  = SPACE(30)
lcCustPo   = SPACE(15)
lcDept     = SPACE(05)
lcPackNos  = ""
lcBrTit    = "Order Stores"
llBrowse   = .F.
llStatus   = .T.
TempPackHd = gfTempName()
TempPackH1 = gfTempName()
TempPackLn = gfTempName()
TempFile   = gfTempName()

IF !gfSetup()
  RETURN
ENDIF

*-- Opening the necessary files
=lfOpnFiles()

*:B802927,1 ABD [Begin]
*DO gcScrDir+'AL\ALMUL30A.SPX'
DO (gcScrDir+gcWinAppl+"\SOMUL30A.SPX")
*:B802927,1 ABD [End]

*!*************************************************************
*! Name      : lfOpnFiles
*! Developer : Khalid Mohi El-Din
*! Date      : 11/10/1999
*! Purpose   : To Open the databases files.
*!*************************************************************
*! Example            : =lfOpnFiles()
*!*************************************************************
FUNCTION lfOpnFiles

= gfOpenFile (gcDataDir+'OrdHdr','OrdHdr','SH')
= gfOpenFile (gcDataDir+'OrdLine','OrdLinSt','SH')
= gfOpenFile (gcDataDir+'Pack_Hdr','Pack_Hdr','SH')

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,4]
laFileStru[lnFileStru+1,1] = 'Flag'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cCustPo'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 15
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'nAmount'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 10
laFileStru[lnFileStru+3,4] = 2

CREATE TABLE (gcWorkDir+TempPackHd) FROM ARRAY laFileStru
INDEX ON Flag TAG (TempPackH1)
INDEX ON Order+Store TAG (TempPackHd)

= gfOpenFile (gcDataDir+'Pack_lin','Pack_Lin','SH')
COPY STRUCTURE TO (gcWorkDir+TempPackLn)
= gfOpenFile (gcWorkDir+TempPackLn,' ','EX')
INDEX ON STR(nOrdLineNo,6) TAG (TempPackLn)

= gfOpenFile (gcDataDir+'Customer','Customer','SH')
= gfOpenFile (gcDataDir+'Style','Style','SH')

IF ('EB' $ gcCmpModules)
  =gfOpenFile(gcDataDir+'EdiAcPrt','AccFact','SH')
  =gfOpenFile(gcDataDir+'EdiPh','Partner','SH')
ENDIF

*!*************************************************************
*! Name      : lfvOrderNo
*! Developer : Khalid Mohi El-Din
*! Date      : 11/10/1999
*! Purpose   : To validate the selected order number.
*!*************************************************************
*! Example            : =lfvOrderNo()
*!*************************************************************
FUNCTION lfvOrderNo

IF !llBrowse .AND. EMPTY(lcOrderNo)
  RETURN
ENDIF

IF llBrowse .OR. (!EMPTY(lcOrderNo) AND !SEEK('O'+lcOrderNo,'OrdHdr'))
  XAccount = ' '
  =ORDBROWO(@lcOrderNo)
ENDIF
llBrowse = .F.

=lfShow()
IF EMPTY(lcOrderNo)
  _CUROBJ = OBJNUM(pbClose)
ENDIF

*!*************************************************************
*! Name      : lfBrows
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : To browse the stores of the selected order
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfBrows
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (TempPackHd)
GOTO TOP
BROWSE FIELDS ;
   Flag            :H = ' '       :1 :R,;
   Store           :H = 'Store'   :8 :R,;       
   Customer.StName :H = 'Name'    :21:R,;
   Tot_Pcs         :H = 'Pieces'  :7 :R,;
   Tot_Wght        :H = 'Weight'     :R,;
   nAmount         :H = 'Amount'     :R;
   WINDOW    ALMUL30C  ;
   IN WINDOW ALMUL30A;
   NOMENU            ;         
   NOAPPEND          ;
   NODELETE          ;         
   NOWAIT            ;
   SAVE              ;
   NOCLEAR           ;
   NOEDIT            ;
   LOCK 0            ;
   WHEN lfwBrow()    ;
   FONT "FOXFONT",9  ;
   TITLE lcBrTit
SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfShow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : To show the related data of the order
*!*************************************************************
*! Example            :  lfShow()
*!*************************************************************
FUNCTION lfShow

IF EMPTY(lcOrderNo)    
  lcAccount = SPACE(05)
  lcAccName = SPACE(30)
  lcCustPo  = SPACE(15)
  lcDept    = SPACE(05)
  
  IF USED(TempPackHd)
    SELECT(TempPackHd)
    ZAP
  ENDIF
  IF USED(TempPackLn)
    SELECT(TempPackLn)
    ZAP
  ENDIF
  
  =lfBrows()
  SHOW GET pbUnSelect DISABLE
  SHOW GET pbSelNon   DISABLE
  SHOW GET pbSelect   DISABLE
  SHOW GET pbSelAll   DISABLE
  SHOW GET pbProceed  DISABLE
  SHOW GET pbClose,1 PROMPT '\<Close' 
  SHOW GET lcOrderNo ENABLE
  SHOW GET ibOrder   ENABLE
  _CUROBJ = OBJNUM(lcOrderNo)
  llStatus = .T.
ELSE
  =SEEK("M"+OrdHdr.Account,'Customer')
  lcAccName = Customer.StName  
  SELECT (TempPackHd)
  IF OrdHdr.Multi = "Y"    
    SET RELATION TO "S"+Account+Store INTO Customer
  ELSE
    IF !EMPTY(OrdHdr.Store)
      SET RELATION TO "S"+Account+Store INTO Customer
    ELSE
      SET RELATION TO "M"+Account INTO Customer
    ENDIF
  ENDIF  
  lcAccount = OrdHdr.Account
  lcCustPo  = OrdHdr.CustPo
  lcDept    = OrdHdr.Dept  
  =lfRefresh()
  =lfGetData()
  llStatus = .F.
  IF EOF(TempPackHd)
    llStatus = .T.
    RETURN
  ENDIF
  SHOW GET pbUnSelect ENABLE
  SHOW GET pbSelNon   ENABLE
  SHOW GET pbSelect   DISABLE
  SHOW GET pbSelAll   DISABLE
  SHOW GET pbProceed  ENABLE
  SHOW GET lcOrderNo  DISABLE
  SHOW GET ibOrder    DISABLE

  SHOW GET pbClose,1 PROMPT ' \<Cancel '
ENDIF
=lfRefresh()
*!*************************************************************
*! Name      : lfwBrow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : To be done whenever the pointer of the browse changed.
*!*************************************************************
*! Example            :  lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

IF OrdHdr.Multi = "Y" AND OrdHdr.MultiPo
  lcCustPo  = &TempPackHd..cCustPo
  =lfRefresh()  
ENDIF

IF !EMPTY(&TempPackHd..Flag)
  SHOW GET pbUnSelect ENABLE
  SHOW GET pbSelect   DISABLE
ELSE
  SHOW GET pbUnSelect DISABLE
  SHOW GET pbSelect   ENABLE
ENDIF

*!*************************************************************
*! Name      : lfGetData
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : To get the lines of the order
*!*************************************************************
*! Example            :  lfGetData()
*!*************************************************************
FUNCTION lfGetData

*llStandPk = ('E' $ qModule) AND SEEK(OrdHdr.Account,"EdiAcc") AND EdiAcc.StandCrtPk="Y"
llStandPk = ('EB' $ gcCmpModules) AND SEEK ('A'+OrdHdr.Account,'EdiAcPrt');
             AND SEEK (EdiAcPrt.cPartCode,'EdiPh') AND EdiPh.cCrtnType = "Y"
lcStore = ""

SELECT OrdLine
*=SEEK(lcOrderNo)
=SEEK('O'+lcOrderNo)

*DO WHILE Order = lcOrderNo
DO WHILE cOrdType+Order = 'O'+lcOrderNo
  lcStore = Store
  IF OrdHdr.Multi = "Y"
    WAIT WINDOW "Processing Store :" + Store NOWAIT
  ELSE
    WAIT WINDOW "Processing Order: " + lcOrderNo NOWAIT
  ENDIF   
  
  *SELECT Pack_Lin.*;
  FROM Pack_Hdr,Pack_Lin;
  WHERE Pack_Hdr.Order+Pack_Hdr.Store+Pack_Hdr.Pack_No=;
  lcOrderNo+lcStore AND Pack_Lin.Pack_No+STR(Pack_Lin.From_Crt,4)+;
  STR(Pack_Lin.To_Crt,4)+Pack_Lin.Style+Pack_Lin.Color=;
  Pack_hdr.Pack_No INTO TABLE (gcWorkDir+TempFile)
  
  *JICA Change Line_No to be 6 Chars [Begin]
  *SELECT Pack_Lin.*;
  *FROM Pack_Hdr,Pack_Lin;
  *WHERE Pack_Hdr.Order+Pack_Hdr.Store+Pack_Hdr.Pack_No=;
  *lcOrderNo+lcStore AND;
  *Pack_Lin.Pack_No+STR(Pack_Lin.Line_No,3)+Pack_Lin.Style = ;
  *Pack_hdr.Pack_No INTO TABLE (gcWorkDir+TempFile)
  *INDEX ON STR(nOrdLineNo,6) TAG (TempFile)

  SELECT Pack_Lin.*;
  FROM Pack_Hdr,Pack_Lin;
  WHERE Pack_Hdr.Order+Pack_Hdr.Store+Pack_Hdr.Pack_No=;
  lcOrderNo+lcStore AND;
  Pack_Lin.Pack_No+STR(Pack_Lin.Line_No,6)+Pack_Lin.Style = ;
  Pack_hdr.Pack_No INTO TABLE (gcWorkDir+TempFile)
  INDEX ON STR(nOrdLineNo,6) TAG (TempFile)
  *JICA Change Line_No to be 6 Chars [End  ]

  SELECT OrdLine
  *SCAN REST WHILE ORDER+STORE+STYLE+COLOR+STR(LINENO,6) = lcOrderNo+lcStore
  SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
                  'O'+lcOrderNo+lcStore
  
    *=SEEK(Style+Color,'Style')    
    =SEEK(Style,'Style')

    STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,;
               lnTotQty
    IF SEEK(STR(OrdLine.LineNo,6),TempFile)
       SELECT (TempFile)
       SUM REST WHILE STR(nOrdLineNo,6) = STR(OrdLine.LineNo,6);
       Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty;
       TO lnqty1,lnqty2,lnqty3,lnqty4,lnqty5,lnqty6,lnqty7,lnqty8,lnTotQty
     ENDIF     

     IF OrdLine.TotQty - lnTotQty > 0
       SELECT (TempPackLn)     
       APPEND BLANK
       REPLACE From_Crt   WITH 1               ,;
               To_Crt     WITH 1               ,;
               Style      WITH OrdLine.Style   ,;
               No_Cart    WITH 1               ,;
               PrePak     WITH OrdLine.PrePak  ,;
               Weight     WITH (OrdLine.TotQty-lnTotQty)*Style.nStyWeight,;
               nOrdLineNo WITH OrdLine.LineNo   ,; 
               Qty1       WITH OrdLine.Qty1-lnQty1,;
               Qty2       WITH OrdLine.Qty2-lnQty2,;
               Qty3       WITH OrdLine.Qty3-lnQty3,;
               Qty4       WITH OrdLine.Qty4-lnQty4,;
               Qty5       WITH OrdLine.Qty5-lnQty5,;
               Qty6       WITH OrdLine.Qty6-lnQty6,;
               Qty7       WITH OrdLine.Qty7-lnQty7,;
               Qty8       WITH OrdLine.Qty8-lnQty8,;
               TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

       SELECT (TempPackHd)
       IF !SEEK(lcOrderNo+lcStore)
         APPEND BLANK
         REPLACE Order     WITH lcOrderNo   ,;
                 Account   WITH OrdHdr.Account ,;
                 cCustPo   WITH IIF(OrdHdr.MultiPo,OrdLine.CustPo,OrdHdr.CustPo),;
                 cWareCode WITH OrdHdr.cWareCode    ,;
                 Store     WITH OrdLine.Store    ,;
                 ShipVia   WITH IIF(!EMPTY(OrdHdr.ShipVia),OrdHdr.ShipVia,;
                                   Customer.ShipVia),;          
                 Tot_Cart  WITH 1          ,;
                 nLastCart WITH 1          ,;
                 lStandCtn WITH llStandPk  ,;
                 cToStOrCn WITH 'S'        ,;
                 Flag      WITH 'û'
       ENDIF
       REPLACE Tot_Wght WITH Tot_Wght + (OrdLine.TotQty- lnTotQty)*Style.nStyWeight,;
               Tot_Pcs  WITH Tot_Pcs  + OrdLine.TotQty - lnTotQty,;
               nAmount  WITH nAmount  + (OrdLine.TotQty - lnTotQty)* OrdLine.Price
     ENDIF  
  ENDSCAN
ENDDO
WAIT CLEAR

SELECT (TempPackHd)
GOTO TOP
IF EOF()
  =gfModalGen("INM44087B00000","Dialog")
  
  STORE SPACE(6) TO lcOrderNo
  =lfShow()
ENDIF
SHOW WINDOW (lcBrTit) REFRESH

*!*************************************************************
*! Name      : lfvSelect
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : The valid function of select button.
*!*************************************************************
*! Example            :  lfvSelect()
*!*************************************************************
FUNCTION lfvSelect

SELECT (TempPackHd)
lcExp = Order+Store
REPLACE Flag WITH 'û'
SEEK lcExp

=lfShowPush()
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvUnSel
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : The valid function of UnSelect button.
*!*************************************************************
*! Example            :  lfvUnSel()
*!*************************************************************
FUNCTION lfvUnSel

SELECT (TempPackHd)
lcExp = Order+Store
REPLACE Flag WITH ' '
SEEK lcExp

=lfShowPush()
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : The valid function of select all button.
*!*************************************************************
*! Example            :  lfvSelAll()
*!*************************************************************
FUNCTION lfvSelAll

SELECT (TempPackHd)
lcExp = Order+Store
REPLACE ALL FLAG WITH 'û'
SEEK lcExp

=lfShowPush()
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : The valid function of select non button.
*!*************************************************************
*! Example            :  lfvSelNon()
*!*************************************************************
FUNCTION lfvSelNon

SELECT (TempPackHd)
lcExp = Order+Store
REPLACE ALL FLAG WITH SPACE(1)
SEEK lcExp

=lfShowPush()
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvProceed
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : To validate the procedd button.
*!*************************************************************
*! Example            :  lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

SELECT (TempPackHd)
SET ORDER TO TAG(TempPackH1)
IF !SEEK('û')
  IF OrdHdr.Multi = "Y"
    =gfModalGen("INM44088B00000","Dialog")
  ELSE
    IF !EMPTY(OrdHdr.Store)
      =gfModalGen("INM44089B00000","Dialog",'')
    ELSE
      =gfModalGen("INM44089B00000","Dialog",'main')
    ENDIF
  ENDIF
   STORE SPACE(6) TO lcOrderNo
  =lfShow()
  SET ORDER TO TAG (TempPackHd) IN (TempPackHd)
  RETURN
ENDIF
lcPackNos = ""
SCAN REST WHILE Flag = 'û'
  *lcPackNo = Sequence('PIKTKT')
  lcPackNo = gfSequence('PIKTKT')

  IF OrdHdr.Multi = "Y"
    =SEEK('S'+Account+Store,'Customer')
  ELSE
    =SEEK(IIF(!EMPTY(OrdHdr.Store),'S'+OrdHdr.Account+OrdHdr.Store,;
                                   'M'+OrdHdr.Account),'Customer')
  ENDIF

  lcPackNos = lcPackNos + lcPackNo
  
  SELECT Pack_Hdr
  APPEND BLANK
  REPLACE Pack_No   WITH lcPackNo             ,;
          Order     WITH &TempPackHd..Order   ,;
          Account   WITH &TempPackHd..Account ,;
          cWareCode WITH &TempPackHd..cWareCode     ,;
          Store     WITH &TempPackHd..Store   ,;
          Tot_Wght  WITH &TempPackHd..Tot_Wght,;
          Tot_Cart  WITH 1                    ,;
          Tot_Pcs   WITH &TempPackHd..Tot_Pcs ,;
          ShipVia   WITH IIF(!EMPTY(OrdHdr.ShipVia),OrdHdr.ShipVia,;
                             Customer.ShipVia),;          
          nLastCart WITH &TempPackHd1..nLastCart,;
          lStandCtn WITH &TempPackHd1..lStandCtn,;
          cToStOrCn WITH &TempPackHd1..cToStOrCn

  lnLastNo = 0  
  SELECT OrdLine
  *=SEEK(lcOrderNo+&TempPackHd..Store)
  =SEEK('O'+lcOrderNo+&TempPackHd..Store)

  *SCAN REST WHILE Order+Store = lcOrderNo+&TempPackHd..Store
  SCAN REST WHILE cOrdType+Order+Store = 'O'+lcOrderNo+&TempPackHd..Store

    *B803540,1 HBG 08/01/2000 Save Pack Qty in OrdLine file [Begin]     
    REPLACE nPck1  WITH nPck1  + &TempPackLn..Qty1,;
            nPck2  WITH nPck2  + &TempPackLn..Qty2,;
            nPck3  WITH nPck3  + &TempPackLn..Qty3,;
            nPck4  WITH nPck4  + &TempPackLn..Qty4,;
            nPck5  WITH nPck5  + &TempPackLn..Qty5,;
            nPck6  WITH nPck6  + &TempPackLn..Qty6,;
            nPck7  WITH nPck7  + &TempPackLn..Qty7,;
            nPck8  WITH nPck8  + &TempPackLn..Qty8,;
            nPWght WITH nPWght + (&TempPackLn..TotQty * Style.nStyWeight)
    *B803540,1 [End]    

    lnLastNo = lnLastNo + 1
    SELECT (TempPackLn)
    =SEEK(STR(OrdLine.LineNo,6))
    SCATTER MEMVAR
    INSERT INTO Pack_Lin FROM MEMVAR
    SELECT Pack_Lin
    REPLACE Pack_No WITH lcPackNo,;
            Line_No   WITH lnLastNo
  ENDSCAN  
  SELECT Pack_Hdr
  REPLACE nLastLNo  WITH lnLastNo
ENDSCAN
IF LEN(lcPackNos) > 7
  =gfModalGen("INM44090B00000","Dialog",LEFT(lcPackNos,6)+'|'+RIGHT(lcPackNos,6))
ELSE
  =gfModalGen("INM44036B00000","Dialog",lcPackNos)
ENDIF
SET ORDER TO TAG (TempPackHd) IN (TempPackHd)
lcOrderNo = SPACE(6)
=lfShow()

*!*************************************************************
*! Name      : lfvClose
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : The valid function of Close & Cancel buttons.
*!*************************************************************
*! Example            :  lfvClose()
*!*************************************************************
FUNCTION lfvClose

IF llStatus
  CLEAR READ
ELSE
  STORE SPACE(06) TO lcOrderNo
 =lfShow()
ENDIF

*!*************************************************************
*! Name      : lfShowPush
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : To control the buttons of the screen.
*!*************************************************************
*! Example            :  lfShowPush()
*!*************************************************************
FUNCTION lfShowPush

lcExp = &TempPackHd..Order+&TempPackHd..Store
SET ORDER TO TAG (TempPackH1) IN (TempPackHd)

IF SEEK('û',TempPackHd)
  SHOW GET pbSelNon   ENABLE
  SHOW GET pbUnSelect ENABLE
ELSE
  SHOW GET pbSelNon   DISABLE
  SHOW GET pbUnSelect DISABLE
ENDIF

IF SEEK(' ',TempPackHd)
  SHOW GET pbSelAll ENABLE
  SHOW GET pbSelect ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
  SHOW GET pbSelect DISABLE
ENDIF  
SET ORDER TO TAG (TempPackHd) IN (TempPackHd)
SEEK lcExp

*!*************************************************************
*! Name      : lfActivate
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Example            :  lfActivate()
*!*************************************************************
FUNCTION lfActivate

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC 
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 

*!*************************************************************
*! Name      : lfDeactiv
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : The READ Deactivate function of screen NAP500
*!*************************************************************
*! Example            :  lfDeactiv()
*!*************************************************************
FUNCTION lfDeactiv

IF WONTOP() = lcBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC DO lpTrapKey WITH 'ALMUL30D', 'PbClose', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH 'ALMUL30D', 'pbSelect'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'ALMUL30D', 'PbClose'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvActBrow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 11/10/1999
*! Purpose   : Valid function for the browsing button (ibOrder)
*!*************************************************************
*! Example            :  lfvActBrow()
*!*************************************************************
FUNCTION lfvActBrow
PARAMETERS lcObjName
llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)