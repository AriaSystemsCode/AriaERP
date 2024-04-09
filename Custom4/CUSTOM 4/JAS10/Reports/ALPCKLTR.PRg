*:***************************************************************************
*: Program file  : ALPCKLSJ.PRG
*: Program desc. : CUSTOMIZED PACKING LIST Form FOR JAS10.
*: Date          : 11-17-2011
*: System        : Aria Advantage Series.4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Saber A.Razek (SAB)
*: Tracking Job Number: (C201424.122 and C201423.exe) {T20110908.0032}
*:***************************************************************************
*! Modifications:
*! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [T20110908.0032]
*:***************************************************************************

*-SABER-------------------------------------------------------------------[Start]
lnPcnt    = 1
lcLastPack = SPACE(16)
*-SABER-------------------------------------------------------------------[End]

llALPakLst = .T.
llEndGroup = .F.
lnLastRec  = 0
STORE SPACE(16) TO lcPackId
IF loPack_Hdr.llnative
  SELECT(lcTempPack_Hdr)
  LOCATE 
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF
ENDIF 
*-- if user change filter criteria then you must collect data again 
IF llOGFltCh

  DIMENSION  laShipTo[5,1], laSoldTo[5,1]
  laSoldTo = ''           && Array to hold the Sold To address
  lcSolTName = ''         && Variable to hold the Sold to name
  *--selecting company information from syccomp remotely[Begin]

  *-- NotePad File  
  IF lcNotePad   = lcTempNotePad
    lcNotePad = loogScroll.gfTempName()
    SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
    =lfMakeIndex(lcNotePad)
  ELSE 
    IF USED(lcNotePad) 
      USE IN (lcNotePad)
      SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
      =lfMakeIndex(lcNotePad)
    ENDIF  
  ENDIF 
  *--customer file
  IF lcCustomer  = lcTempCustomer
    lcCustomer = loogScroll.gfTempName()
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
    =lfMakeIndex(lcCustomer)
  ELSE
    IF USED(lcCustomer)
      USE IN (lcCustomer)
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
    =lfMakeIndex(lcCustomer)
    ENDIF 
  ENDIF 
  *--OrdHdr File
  IF lcOrdhdr    = lcTempOrdhdr
    lcOrdhdr = loogScroll.gfTempName()
    SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
    =lfMakeIndex(lcOrdhdr)
  ELSE
    IF USED(lcOrdhdr)
      USE IN (lcOrdhdr)
      SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
      =lfMakeIndex(lcOrdhdr)
    ENDIF 
  ENDIF 
  *--Ordline File
  IF lcOrdLnTmp  = lcTempOrdline
    lcOrdLnTmp = loogScroll.gfTempName()
    SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
    =lfMakeIndex(lcOrdLnTmp)
  ELSE
    IF USED(lcOrdLnTmp)
      USE IN (lcOrdLnTmp)
      SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
      =lfMakeIndex(lcOrdLnTmp)
    ENDIF 
  ENDIF 
  *--style file
  IF lcStyleFile = lcTempStyle
    lcStyleFile = loogScroll.gfTempName()
    SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
    =lfMakeIndex(lcStyleFile)
  ELSE 
    IF USED(lcStyleFile)
      USE IN (lcStyleFile)
      SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
      =lfMakeIndex(lcStyleFile)
    ENDIF 
  ENDIF 
  *--scale file
  IF lcScaleFile = lcTempScale
    lcScaleFile = loogScroll.gfTempName()
    SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
    =lfMakeIndex(lcScaleFile)
  ELSE 
    IF USED(lcScaleFile)
      USE IN (lcScaleFile)
      SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
      =lfMakeIndex(lcScaleFile)
    ENDIF 
  ENDIF 
  *--WareHous File
  IF lcWareHous  = lcTempWareHous
    lcWareHous = loogScroll.gfTempName()
    SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
    =lfMakeIndex(lcWareHous)
  ELSE 
    IF USED(lcWareHous)
      USE IN (lcWareHous)
      SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
      =lfMakeIndex(lcWareHous)
    ENDIF 
  ENDIF 
  *-- Collecting Code...
  IF !USED(lcPackTmp) OR (RECCOUNT(lcPackTmp) > 0)
    =lfWorkFile()
  ENDIF

  =lfCollect()               && Collect Aging data.
  SELECT (lcPackTmp)
  SET STEP ON &&SABER
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO &lcCustomer ,;
                                      "O" + ORDER + STORE     INTO &lcOrdLnTmp,;
                                      "B" + ORDER             INTO &lcNotePad  ,;  
                                      PACK_NO                 INTO (lcPakLnTmp) ,;           
                                      "O" + ORDER             INTO &lcOrdHdr  ,;
                                      piktkt                  INTO &lcTempPikTkt ADDITIVE
ENDIF

*-- Asking if no records (Display message) otherwise print report 
SELECT (lcPackTmp)
IF RECCOUNT(lcPackTmp) = 0
 *---Text : 'No Record Selected for the report..!'
 =gfModalGen('TRM00052B00000','DIALOG')
 RETURN
ENDIF


*-SABER-------------------------------------------------------------------[Start]
llOnePack = .F.
lnPosPack = ASCAN(loOgScroll.laOgFxFlt,"PACK_HDR.PACK_NO")
IF lnPosPack > 0 
  lnPosPack = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPack,1)
  lcPackCursor = loOgScroll.laOgFxFlt[lnPosPack,6]
  IF !EMPTY(lcPackCursor)
    SELECT(lcPackCursor)
    LOCATE
    IF !EOF()
      COUNT FOR !DELETED() TO lnCntOrd
      IF lnCntOrd = 1 
        llOnePack = .T.
      ENDIF 
    ENDIF 
  ENDIF   
ENDIF 


lcFileOld   = oAriaApplication.gcoutFile 
lcOldDevice = oAriaApplication.gcDevice 
lcOldType   = loogscroll.cTextRepType 

oAriaApplication.gcDevice  = 'FILE'
oAriaApplication.gcoutFile = oAriaApplication.Workdir+loogscroll.gftempName()+'.PDF'
loogscroll.cTextRepType    = "PDF"
loogScroll.cCROrientation  = 'P'
loOGScroll.cCRPapersize    = 'LETTER'

llGetPage = .T.
SELECT (lcPackTmp)
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
IF llOnePack 
  SELECT (lcPackTmp)
  REPLACE ALL  nPageNo WITH _pageTotal  IN (lcPackTmp)
ENDIF
llGetPage = .F.
SELECT (lcPackTmp)

oAriaApplication.gcDevice  = lcOldDevice
oAriaApplication.gcoutFile = lcFileOld
loogscroll.cTextRepType    = lcOldType
*-SABER-------------------------------------------------------------------[End]

SELECT (lcPackTmp)

SELECT(lcSTYLEfile)
SET RELATION TO  "S" + scale  INTO (lcScaleFile)

SELECT (lcPackTmp)
GOTO TOP
*-- Sort by Carton No. Index
SET ORDER TO TAG PACKCRTN IN (lcPakLnTmp)

loogScroll.cCROrientation = 'P'

IF llALPakLst
  DO gfDispRe WITH EVAL('lcFormName')
  *- Realese Object   
  IF TYPE('loogScroll.UPCBAR') == 'O'
    *- lcF_name = UPPER('UPCBAR')
    loogScroll.RemoveObject('UPCBAR')    
  ENDIF  
ENDIF


*!**************************************************************************
*! Name      : lfGetBarCode
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Get Barcode for CustPO
*!**************************************************************************
FUNCTION lfGetBarCode
PARAMETERS  tcCUPC
SELECT(lcPackTmp) 
lcF_name = UPPER('UPCBAR')
WITH loogScroll && your form name
  IF TYPE('loogScroll.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name
  .&lcF_name..WIDTH         = 100
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]
  *.&lcF_name..HEIGHT        = 200
  .&lcF_name..HEIGHT        = 300
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
   SELECT(lcPackTmp) 
   APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
  .&lcF_name..REFRESH
  *-SABER-------------------------------------------------------------------[Start]
  *.&lcF_name..OBJECT.DataToEncode = tcCUPC && EVALUATE(SYCUPCDT.F_VALUE) The CUPC #
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]
  *.&lcF_name..OBJECT.DataToEncode = "9859"+tcCUPC && EVALUATE(SYCUPCDT.F_VALUE) The CUPC #
  .&lcF_name..OBJECT.DataToEncode = PADR("9859"+tcCUPC, 14)
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
  *-SABER-------------------------------------------------------------------[End]  
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]
  *.&lcF_name..OBJECT.Barheight    = 2
  .&lcF_name..OBJECT.Barheight    = 3
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
  *-SABER 12-20-2011 --------------- [Start]
  *.&lcF_name..OBJECT.SymbologyID  = 13
  .&lcF_name..OBJECT.SymbologyID  = 16
  *-SABER 12-20-2011 --------------- [End]
  .&lcF_name..OBJECT.showtext     = 0
  .&lcF_name..OBJECT.NarrowBarWidth = 0.04
  .&lcF_name..OBJECT.Wide2NarrowRatio = 3
  *adding Orientation to the barcode
  * Values are 0,90,180
  .&lcF_name..OBJECT.Orientation = 0
  .&lcF_name..OBJECT.TopMarginCm = 0
  .&lcF_name..OBJECT.LeftMarginCm = 0.1
  .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
  .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
ENDWITH

*!**************************************************************************
*! Name      : lfGetRetLoc
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Get Return loc.
*!**************************************************************************
FUNCTION lfGetRetLoc
*“Return Locations:”,
lcRetLoc = ''
*-SABER 11-28-2011 [Start]
*lnNotesLine = MEMLINES(eval(lcTempNotePad+'.mNotes'))
*FOR lnF = 1  TO lnNotesLine 
*  IF 'Return Locations:' $ MLINE(eval(lcTempNotePad+'.mNotes'),lnF)
*    lcRetLoc = ALLTRIM(SUBSTR(MLINE(eval(lcTempNotePad+'.mNotes'),lnF),;
*    		   AT('Return Locations:',MLINE(eval(lcTempNotePad+'.mNotes'),lnF))+LEN('Return Locations:')+1))
*    EXIT 
*  ENDIF 
*ENDFOR  
lnNotesLine = MEMLINES(eval(lcTempOrdLine+'.Note_mem'))
FOR lnF = 1  TO lnNotesLine 
  IF 'Return Locations:'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)
    lcRetLoc = ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),;
               AT('Return Locations:',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('Return Locations:')+1, 30))
    lcRetLoc = SUBSTR(lcRetLoc,1,AT("|",lcRetLoc,1)-1)
    EXIT
  ENDIF
ENDFOR

*-SABER 11-28-2011 [End]
RETURN lcRetLoc 

*!**************************************************************************
*! Name      : lfGetGiftMsg
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Get Gift Message
*!**************************************************************************
FUNCTION lfGetGiftMsg
lcGiftMsg = ''
lnNotesLine = MEMLINES(eval(lcTempOrdLine+'.Note_mem'))
FOR lnF = 1  TO lnNotesLine 
*! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]

**-SABER 11-28-2011 [Start]
**  IF 'Gift Wrap:'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)
**    lcGiftMsg= ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('Gift Wrap:',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('Gift Wrap:')+1))
*  IF 'WM|'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)
*    lcGiftMsg= ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('WM|',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('WM|')+1))
**-SABER 11-28-2011 [End]

*  IF 'Gift Wrap:'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)
*    lcGiftMsg= ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('Gift Wrap:',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('Gift Wrap:')+1))  
*! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
*    EXIT 
*  ENDIF 
  
  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [Start]
  IF 'Gift Wrap:'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)
    lcGiftMsg= ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('Gift Wrap:',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('Gift Wrap:')+1))  
    EXIT 
  ENDIF
  IF 'GIFTWRAP'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)
    lcGiftMsg= ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('GIFTWRAP',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('GIFTWRAP')+1))  
    EXIT 
  ENDIF

  *! C201423,1 SAB 01-02-2012 Fix bug in Barcode and GiftWrap [End]
ENDFOR  
RETURN lcGiftMsg

*!**************************************************************************
*! Name      : lfGetDPCI
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-20-2011
*! Purpose   : Get DPCI #
*!**************************************************************************
FUNCTION lfGetDPCI
lcDPCI = ''
lnNotesLine = MEMLINES(eval(lcTempOrdLine+'.Note_mem'))
FOR lnF = 1  TO lnNotesLine 
  *-SABER 01-05-2012 --------------- [Start]
  *IF '|CB|'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)    
  *  *-SABER 12-20-2011 --------------- [Start]
  *  *lcDPCI = ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('|CB|',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('|CB|')+1))
  *  lcDPCI = ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('|CB|',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('|CB|')))
  *  lcDPCI = SUBSTR(lcDPCI,1,AT("|",lcDPCI,1)-1)
  *  *-SABER 12-20-2011 --------------- [End]
  *  EXIT 
  *ENDIF   
  IF 'CB|'  $ MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF)    
    lcDPCI = ALLTRIM(SUBSTR(MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF),AT('CB|',MLINE(eval(lcTempOrdLine+'.Note_mem'),lnF))+LEN('CB|')))
    lcDPCI = SUBSTR(lcDPCI,1,AT("|",lcDPCI,1)-1)
    EXIT 
  ENDIF   
  *-SABER 01-05-2012 --------------- [End]
ENDFOR  
RETURN lcDPCI

*!**************************************************************************
*! Name      : lfGetOrderSt
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Get Order Status Message
*!**************************************************************************
FUNCTION lfGetOrderSt
lcOrderSt = ''
lnNotesLine = MEMLINES(eval(lcTempNotePad+'.mNotes'))
FOR lnF = 1  TO lnNotesLine 
  IF 'Order Status' $ MLINE(eval(lcTempNotePad+'.mNotes'),lnF)    
    *-SABER 12-20-2011 --------------- [Start]
    *lcOrderSt= ALLTRIM(SUBSTR(MLINE(eval(lcTempNotePad+'.mNotes'),lnF),AT('Gift Wrap:',MLINE(eval(lcTempNotePad+'.mNotes'),lnF))+LEN('Order Status:')+1))
    *EXIT 
    lcOrderSt= IIF(EMPTY(lcOrderSt),'',lcOrderSt+CHR(13)+CHR(10))+ALLTRIM(SUBSTR(MLINE(eval(lcTempNotePad+'.mNotes'),lnF),AT('Gift Wrap:',MLINE(eval(lcTempNotePad+'.mNotes'),lnF))+LEN('Order Status:')+1))
    *-SABER 12-20-2011 --------------- [End]
  ENDIF 
ENDFOR  
RETURN lcOrderSt

*!**************************************************************************
*! Name      : lfGetUpc
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Get Style UPC
*!**************************************************************************
FUNCTION lfGetUpc
IF TYPE('loStyleUpc') <> 'O'
  loStyleUpc = CreateObject("RemoteTable","StyleUpc","STYLEUPC",lcTempStyleUpc,SET("DATASESSION")) 
ENDIF 
IF loStyleUpc.Seek(m.Style)
  RETURN &lcTempStyleUpc..CUPCNUM1+&lcTempStyleUpc..CUPCNUM2+&lcTempStyleUpc..CUPCNUM3
ENDIF 
RETURN ''

*!**************************************************************************
*! Name      : lfGetPrior
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Get Priority
*!**************************************************************************
FUNCTION lfGetPrior
lcUPCTyp = SPACE(0)
DIMENSION laShipViaAry[1,2]
lcPriValu = ""
laShipViaAry[1,1] = "CUPS      "
laShipViaAry[1,2] = "lcUPCTyp"
*-- Fill the related GL information from the codes file.
llNoThing = gfRltFld(EVALUATE(lctempOrdHdr+'.SHIPVIA'), @laShipViaAry, "SHIPVIA   ")
IF !EMPTY(lcUPCTyp)
  lcPriValu = ALLTRIM(gfCodDes(lcUPCTyp,"CUPS      ",.F.))
ENDIF 
RETURN lcPriValu 

*!**************************************************************************
*! Name      : lfWOgWhen()    
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : To Set When running the Option Grid.
*!**************************************************************************
*! Called from : "When" Option Grid.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfWOgWhen()    
*!**************************************************************************
*
FUNCTION lfWOgWhen

IF !USED('OBJLINK_JA')
  =gfOpenTable('OBJLINK','OBJDEFA','SH','OBJLINK_JA')
ENDIF 

IF !USED('OBJECTS_JA')
  =gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS_JA')
ENDIF 

*--Pack_Hdr file
IF TYPE('loPack_Hdr') <> 'O'
  loPack_Hdr = CreateObject("RemoteTable","Pack_Hdr","Pack_Hdr",lcTempPack_Hdr,SET("DATASESSION")) 
ENDIF 

*--Pack_lin file
IF TYPE('loPack_Lin') <> 'O'
  loPack_Lin = CreateObject("RemoteTable","Pack_Lin","Pack_Lin",lcTempPack_Lin,SET("DATASESSION")) 
ENDIF 

*--Piktkt file
IF TYPE('loPikTkt') <> 'O'
   loPikTkt   = CreateObject("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,SET("DATASESSION"))
ENDIF

*--Customer file
IF TYPE('loCustomer') <> 'O'
  loCustomer = CreateObject("RemoteTable","Customer","Customer",lcTempCustomer,SET("DATASESSION"))  
  SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
  =lfMakeIndex(lcCustomer)
ENDIF   

*--Notepad file
IF TYPE('loNotePad') <> 'O'
  loNotePad  = CreateObject("RemoteTable","NotePad","NotePad",lcTempNotePad,SET("DATASESSION")) 
  SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
  =lfMakeIndex(lcNotePad)
ENDIF  

*--Style file
IF TYPE('loStyle') <> 'O' 
  loStyle    = CreateObject("RemoteTable","Style","Style",lcTempStyle,SET("DATASESSION")) 
  SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
  =lfMakeIndex(lcStyleFile)
ENDIF

*--Scale file
IF TYPE('loScale') <> 'O'
  loScale    = CreateObject("RemoteTable","Scale","Scale",lcTempScale,SET("DATASESSION")) 
  SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
  =lfMakeIndex(lcScaleFile)
ENDIF 
  
*--Ordhdr file  
IF TYPE('loOrdHdr') <> 'O'
  loOrdHdr   = CreateObject("RemoteTable","OrdHdr","OrdHdr",lcTempOrdHdr,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
  =lfMakeIndex(lcOrdhdr)
ENDIF

*--Ordline file 
IF TYPE('loOrdLine') <> 'O'  
  loOrdLine  = CreateObject("RemoteTable","OrdLine","ORDLINST",lcTempOrdLine,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
  =lfMakeIndex(lcOrdLnTmp)
ENDIF   

*--WareHous  file
IF TYPE('loWareHous') <> 'O'  
  loWareHous  = CreateObject("RemoteTable","WareHous","WareHous",lcTempWareHous,SET("DATASESSION")) 
  SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
  =lfMakeIndex(lcWareHous)
ENDIF   


*--Spck_lin file
IF TYPE('loSpck_Lin') <> 'O'  
  loSpck_Lin = CreateObject("RemoteTable","Spck_Lin","SPKLNSTCN",lcTempSpck_Lin,SET("DATASESSION")) 
ENDIF   

 
SELECT(lcTempPack_Hdr)
DIMENSION laTempStru[1,18]
lnFldLen = AFIELDS(laTempStru)
*-SABER-------------------------------------------------------------------[Start]
*DIMENSION laTempStru[lnFldLen + 4 , 18]
DIMENSION laTempStru[lnFldLen + 5 , 18]
*-SABER-------------------------------------------------------------------[End]

laTempStru[lnFldLen + 1 , 1] = 'upcBar'
laTempStru[lnFldLen + 1 , 2] = 'G'
laTempStru[lnFldLen + 1 , 3] = 1
laTempStru[lnFldLen + 1 , 4] = 0

laTempStru[lnFldLen + 2 , 1] = 'cPriority'
laTempStru[lnFldLen + 2 , 2] = 'C'
laTempStru[lnFldLen + 2 , 3] = 30
laTempStru[lnFldLen + 2 , 4] = 0

laTempStru[lnFldLen + 3 , 1] = 'MRetLoc'
laTempStru[lnFldLen + 3 , 2] = 'M'
laTempStru[lnFldLen + 3 , 3] = 2
laTempStru[lnFldLen + 3 , 4] = 0

laTempStru[lnFldLen + 4 , 1] = 'MOrdSt'
laTempStru[lnFldLen + 4 , 2] = 'M'
laTempStru[lnFldLen + 4 , 3] = 2
laTempStru[lnFldLen + 4 , 4] = 0

*-SABER-------------------------------------------------------------------[Start]
laTempStru[lnFldLen + 5 , 1] = 'nPageNo'
laTempStru[lnFldLen + 5 , 2] = 'N'
laTempStru[lnFldLen + 5 , 3] = 7
laTempStru[lnFldLen + 5 , 4] = 0
*-SABER-------------------------------------------------------------------[End]


*-SABER-------------------------------------------------------------------[Start]
*FOR lnLoop = 1 to 4
FOR lnLoop = 1 to 5
*-SABER-------------------------------------------------------------------[End]
  STORE ' ' TO  laTempStru[lnFldLen +lnLoop,7],laTempStru[lnFldLen +lnLoop,8],;
                laTempStru[lnFldLen +lnLoop,9],laTempStru[lnFldLen +lnLoop,10],;
                laTempStru[lnFldLen +lnLoop,11],laTempStru[lnFldLen +lnLoop,12],;
                laTempStru[lnFldLen +lnLoop,13],laTempStru[lnFldLen +lnLoop,14],;
                laTempStru[lnFldLen +lnLoop,15],laTempStru[lnFldLen +lnLoop,16]
  STORE 0 TO    laTempStru[lnFldLen +lnLoop,17] ,laTempStru[lnFldLen +lnLoop,18]
ENDFOR
 
 
SELECT(lcTempPack_Lin)
DIMENSION laPckLinSt[1,18]
=AFIELDS(laPckLinSt)

lnFldLnPck = AFIELDS(laPckLinSt)
*-SABER-------------------------------------------------------------------[Start]
*DIMENSION laPckLinSt[lnFldLnPck + 5 , 18]
DIMENSION laPckLinSt[lnFldLnPck + 9 , 18]
*-SABER-------------------------------------------------------------------[End]

  *-- Field for the printing lines.
  laPckLinSt[lnFldLnPck + 1 , 1] = 'llPrint'
  laPckLinSt[lnFldLnPck + 1 , 2] = 'L'
  laPckLinSt[lnFldLnPck + 1 , 3] = 1
  laPckLinSt[lnFldLnPck + 1 , 4] = 0

  *-- Field to change the group.
  laPckLinSt[lnFldLnPck + 2 , 1] = 'lcType'
  laPckLinSt[lnFldLnPck + 2 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 2 , 3] = 1
  laPckLinSt[lnFldLnPck + 2 , 4] = 0
 
  laPckLinSt[lnFldLnPck + 3 , 1] = 'SKU'
  laPckLinSt[lnFldLnPck + 3 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 3 , 3] = 16
  laPckLinSt[lnFldLnPck + 3 , 4] = 0
 
  laPckLinSt[lnFldLnPck + 4 , 1] = 'cStyUpc '
  laPckLinSt[lnFldLnPck + 4 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 4 , 3] = 13
  laPckLinSt[lnFldLnPck + 4 , 4] = 0
  
  laPckLinSt[lnFldLnPck + 5 , 1] = 'MGftMsg'
  laPckLinSt[lnFldLnPck + 5 , 2] = 'M'
  laPckLinSt[lnFldLnPck + 5 , 3] = 2
  laPckLinSt[lnFldLnPck + 5 , 4] = 0  
  
  *-SABER-------------------------------------------------------------------[Start]
  laPckLinSt[lnFldLnPck + 6 , 1] = 'cDPCINum'
  laPckLinSt[lnFldLnPck + 6 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 6 , 3] = 20
  laPckLinSt[lnFldLnPck + 6 , 4] = 0
  
  laPckLinSt[lnFldLnPck + 7 , 1] = 'MRetLoc'
  laPckLinSt[lnFldLnPck + 7 , 2] = 'M'
  laPckLinSt[lnFldLnPck + 7 , 3] = 2
  laPckLinSt[lnFldLnPck + 7 , 4] = 0
  
  *-SABER 12-20-2011 --------------- [Start]
  laPckLinSt[lnFldLnPck + 8 , 1] = 'cStyMajor'
  laPckLinSt[lnFldLnPck + 8 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 8 , 3] = 19
  laPckLinSt[lnFldLnPck + 8 , 4] = 0
  
  laPckLinSt[lnFldLnPck + 9 , 1] = 'cStyleDesc'
  laPckLinSt[lnFldLnPck + 9 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 9 , 3] = 60
  laPckLinSt[lnFldLnPck + 9 , 4] = 0
  *-SABER 12-20-2011 --------------- [End]

  *-SABER-------------------------------------------------------------------[End]
 
  *-SABER-------------------------------------------------------------------[Start]
  *FOR lnLoop = 1 to  5
  FOR lnLoop = 1 to  9
  *-SABER-------------------------------------------------------------------[End]
    STORE ' ' TO  laPckLinSt[lnFldLnPck +lnLoop,7],laPckLinSt[lnFldLnPck +lnLoop,8],;
                  laPckLinSt[lnFldLnPck +lnLoop,9],laPckLinSt[lnFldLnPck +lnLoop,10],;
                  laPckLinSt[lnFldLnPck +lnLoop,11],laPckLinSt[lnFldLnPck +lnLoop,12],;
                  laPckLinSt[lnFldLnPck +lnLoop,13],laPckLinSt[lnFldLnPck +lnLoop,14],;
                  laPckLinSt[lnFldLnPck +lnLoop,15],laPckLinSt[lnFldLnPck +lnLoop,16]
     STORE 0 TO   laPckLinSt[lnFldLnPck +lnLoop,17] ,laPckLinSt[lnFldLnPck +lnLoop,18]
  ENDFOR

  =lfWorkFile()  && Fill arrays then create files.

  *--
  DIMENSION laShipTo[5,1], laSoldTo[5,1]
  laSoldTo = ''           && Array to hold the Sold To address
  lcSolTName = ''         && Variable to hold the Sold to name



*-- End Of lfWOgWhen.
*!**************************************************************************
*! Name      : lfWorkFile
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Create work cursors.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfWorkFile()
*!**************************************************************************
*
FUNCTION lfWorkFile
*-- First Close all temporary files if it was opened before 
IF USED(lcPackTmp)
  USE IN (lcPackTmp)
ENDIF
*-- First Close all temporary files if it was opened before
=gfCrtTmp(lcPackTmp,@laTempStru,"PACK_NO",lcPackTmp,.T.)
SELECT (lcPackTmp)
*-- You Must Make A Zap To decoument The File After Cursor on The Hard Disk
ZAP
INDEX ON PACK_NO TAG (lcPackTmp)

*-- First Close all temporary files if it was opened before 
IF USED(lcPakLnTmp)
  USE IN (lcPakLnTmp)
ENDIF

*_Screen.Visible = .T.
*SET STEP ON &&SABER
=gfCrtTmp(lcPakLnTmp,@laPckLinSt,"PACK_NO+STYLE+STR(nOrdLineNO,6)",lcPakLnTmp,.T.)

SELECT (lcPakLnTmp)
ZAP
INDEX ON PACK_NO+STYLE+STR(nOrdLineNO,6) TAG (lcPakLnTmp) 

INDEX ON pack_no+STR(no_cart,4)+style TAG 'PACKCRTN' ADDITIVE
*-- End Of lfWorkFile.

*!**************************************************************************
*! Name      : lfAdrShift
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : used to shift the lashipto array
*!**************************************************************************
*! Passed Parameters  : lcArrayNam : Array hold The address.
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : = lfAdrShift()
*!**************************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End Of lfAdrShift

*!**************************************************************************
*! Name      : lfCollect
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : For Collecting Data In The First Time.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None.
*!**************************************************************************
*! Example   : = lfCollect()
*!**************************************************************************
FUNCTION lfCollect
  lnPosPackList = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.PACK_NO")

  loPack_Hdr.setorder("Pack_Hdr")

  IF lnPosPackList > 0 
	lnPosPackList = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPackList,1)
	lcPackLstSel =IIF(!EMPTY(laOgFxFlt[lnPosPackList,6]),laOgFxFlt[lnPosPackList,6],'')
	IF !EMPTY(lcPackLstSel)
	  SELECT(lcPackLstSel)
	  LOCATE
	  IF !EOF()
		SCAN 
		  WAIT WINDOW "Collect data for PACK_NO #" + &lcPackLstSel..PACK_NO  + ' ...' NOWAIT
		  loPack_hdr.Seek(&lcPackLstSel..PACK_NO)
		  SELECT(lcTempPack_Hdr)
		  SCATTER MEMVAR MEMO
		  loOrdHdr.Seek('O' + m.Order)
		  IF EMPTY(M.CWARECODE)
		    M.CWARECODE = &lctempOrdHdr..CWARECODE
		  ENDIF 		  
		  loNotepad.seek("B" + &lctempOrdHdr..ORDER)             
		  *-SABER 11-28-2011 [Start]
		  *m.MRetLoc = lfGetRetLoc()
		  *-SABER 11-28-2011 [End]
		  m.MOrdSt  = lfGetOrderSt()
	 	 m.cPriority = lfGetPrior()
		  INSERT INTO (lcPackTmp) FROM MEMVAR
	      SELECT(lcPackTmp)
	 	  IF !EMPTY(&lctempOrdHdr..CustPO)
	 	    lfGetBarCode(&lctempOrdHdr..CustPO)	    	
		  ENDIF   	  
    	ENDSCAN 
		WAIT CLEAR
		=lfGetLines()
		=lfGetData()
	  ELSE
		=lfNoPackSel()
	  ENDIF  
	ELSE
	  =lfNoPackSel()
	ENDIF 
  ENDIF    
*!**************************************************************************
*! Name      : lfHeadVar
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : 1 - Hold Address In Shipto Array
*!           : 2 - Get the division long name.
*!**************************************************************************
*! Called from : Report Group Header band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfHeadVar()
*!**************************************************************************
FUNCTION lfHeadVar

DIMENSION laShipTo[5,1]
laShipTo = ''

DIMENSION laSoldTo[6,1]
laSoldTo = ''

*! C201255,1 MMT Change the SOLD to address in JAS10 Custom Packing Slip[Start]
*!*	lcSolTName = &lcCustomer..BTName
*!*	laSoldTo[1] = gfGetAdr(lcCustomer , '' , '' , '' , 1 , '2')
*!*	laSoldTo[2] = gfGetAdr(lcCustomer , '' , '' , '' , 2 , '2')
*!*	laSoldTo[3] = gfGetAdr(lcCustomer , '' , '' , '' , 3 , '2')
*!*	laSoldTo[4] = gfGetAdr(lcCustomer , '' , '' , '' , 4 , '2')
*!*	laSoldTo[5] = gfGetAdr(lcCustomer , '' , '' , '' , 5 , '2')
*!*	laSoldTo[6] = gfGetAdr(lcCustomer , '' , '' , '' , 6 , '2')
loNotepad.seek("B" + &lcPackTmp..ORDER)              
lcSolTName = ''
lnNotesLine = MEMLINES(eval(lcTempNotePad+'.mNotes'))
lnStLnAdd = 0
FOR lnF = 1  TO lnNotesLine 
  IF 'SOLD TO:' $ MLINE(eval(lcTempNotePad+'.mNotes'),lnF)
    lnStLnAdd = lnF+1
    EXIT 
  ENDIF 
ENDFOR  
IF lnStLnAdd > 0
  lcSolTName = MLINE(eval(lcTempNotePad+'.mNotes'),lnStLnAdd)
  lnAddCnt = 1
  FOR lnCnt = lnStLnAdd+1 TO lnStLnAdd+4
    *-SABER 12-20-2011 --------------- [Start]
    IF 'SOLD TO' $ MLINE(eval(lcTempNotePad+'.mNotes'),lnCnt)
      EXIT 
    ENDIF
    *-SABER 12-20-2011 --------------- [End]
    laSoldTo[lnAddCnt] = MLINE(eval(lcTempNotePad+'.mNotes'),lnCnt)
    lnAddCnt = lnAddCnt + 1
  ENDFOR 
ENDIF 
*! C201255,1 MMT Change the SOLD to address in JAS10 Custom Packing Slip[End]
=lfAdrShift('laSoldTo')

PRIVATE lcDistCntr , lcAlasCust
lcAlasCust = SELECT(0)
SELECT(lcCustomer)

IF &lcOrdHdr..Alt_ShpTo
  DIMENSION laShipTo[6,1]
  laShipTo = ''
  laShipTo[1] = &lcOrdHdr..STName
  laShipTo[2] = &lcOrdHdr..cAddress1
  laShipTo[3] = &lcOrdHdr..cAddress2
  laShipTo[4] = &lcOrdHdr..cAddress3
  laShipTo[5] = &lcOrdHdr..cAddress4
  laShipTo[6] = &lcOrdHdr..cAddress5
  =lfAdrShift('laShipTo')
ELSE
  lnCUSRec = 0
  IF !EMPTY(&lcCustomer..Store) AND !EMPTY(&lcCustomer..Dist_ctr) AND !EVALUATE(lcOrdHdr+'.lStrDirct')
    lnCUSRec = IIF(!EOF(lcCustomer),RECNO(lcCustomer),0)
    =SEEK('S'+&lcCustomer..Account+&lcCustomer..Dist_ctr)
  ENDIF

  =gfGetAdr(lcCustomer , '' , '' , '' , @laShipTo)
  =lfAdrShift('laShipTo')

  DIMENSION laShipTo[6,1]
  =AINS(laShipTo,1)

  laShipTo[1,1] = &lcCustomer..StName
  =lfAdrShift('laShipTo')

  IF BETWEEN(lnCusRec , 1 , RECCOUNT(lcCUSTOMER))
    GOTO lnCusRec IN &lcCUSTOMER
  ENDIF
ENDIF
SELECT(lcAlasCust)


STORE "" TO lcScale , lcPackNo
RETURN ""
*-- END OF lfHeadVar

*!**************************************************************************
*! Name      : lfGrpSetes
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfGrpSetes()
*!**************************************************************************
FUNCTION lfGrpSetes
PARAMETERS llDosMode
lnGrdTotWg = 0

PRIVATE lcSkipExpr , lnCurAlias , lcSkipTag , lcCurRec
lnCurAlias = SELECT(0)

lcSkipExpr = ''
SET SKIP TO




SELECT (lcPakLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT(lcOrdLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT (lnCurAlias)

PRIVATE lcLocExpr

  SELECT (lcPakLnTmp)
  lcLinFile = lcPakLnTmp

  lcPackNo = pack_no
  lcLocExpr = ""
 
  lcSkipTag = ORDER()
  lcCurRec = EVALUATE(KEY())
  SET ORDER TO (lcSkipTag) DESC
  IF SEEK(lcPackNo)

    IF !EMPTY(lcLocExpr)    
      LOCATE REST WHILE &lcLocExpr
    ENDIF
    lnLastRec = RECNO()
  ENDIF
  SET ORDER TO (lcSkipTag) ASCE
  =SEEK(lcCurRec)
  SET RELATION TO  STYLE INTO &lcStyleFile ADDITIVE
SELECT (lnCurAlias)
lcSkipExpr = lcPakLnTmp
SET SKIP TO &lcSkipExpr
llEndGroup = .F.

RETURN ""
*-- END OF  lfGrpSetes

*!**************************************************************************
*! Name      : lfChngScle
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : To Print New Scale If Changed
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfChngScle()
*!**************************************************************************
*
FUNCTION lfChngScle
lcScale = &lcStyleFile..Scale
RETURN ''
*-- End of lfChngScle 


*!**************************************************************************
*! Name      : lfEndGroup
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Rise End Group Flag Which Control Page Footer Data.
*!**************************************************************************
*! Called from : Group fotter band.
*!**************************************************************************
*! Passed Parameters : None.
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfEndGroup()
*!**************************************************************************
FUNCTION lfEndGroup
*-- Set this variable .T. to don't print the word "CONTINUE NEXT PAGE"
*-- and then print Totals.
llEndGroup = .T.
lnLastRec = 0
RETURN ''
*-- End Of lfEndGroup.

*!**************************************************************************
*! Name      : lfScalHead
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Return The New Scales If The Scale Changed
*!**************************************************************************
*! Called from : Pomat Details Band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfScalHead()
*!**************************************************************************
*
FUNCTION lfScalHead
PRIVATE lcScalHead
lcScalHead = ""

IF lcScale != &lcStyleFile..Scale
  lcScale = &lcStyleFile..Scale
  lcScalHead = lcScale+" " + PADL(ALLTRIM(&lcScaleFile..SZ1),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ2),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ3),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ4),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ5),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ6),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ7),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ8),5)
ENDIF
RETURN lcScalHead
*-- End Of lfScalHead.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Close any opened files if user press OG <Close> Button
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .F.  

*-- Close Temporary Cursors [Begin]
IF USED (lcPackTmp )
 USE IN (lcPackTmp )
ENDIF
*-- Close Temporary Cursors [End]

*-SABER-------------------------------------------------------------------[Start]
*IF TYPE('loogScroll.lcf_name')='O'
*  loogScroll.lcf_name = ''
*ENDIF
*-SABER-------------------------------------------------------------------[End]
*-- End Of lfClearRep.
*!**************************************************************************
*! Name      : lfSRPack
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : To set the relations or To release the relations when browse
*!         packing list No.   
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRPack (S-> , R->)
*!**************************************************************************
FUNCTION lfSRPack
PARAMETERS lcParm
SET ORDER TO TAG Customer IN Customer

IF lcParm = "S"
  SELECT PACK_HDR
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  GO TOP
ELSE
  SELECT PACK_HDR
  SET RELATION TO
ENDIF


*!**************************************************************************
*! Name      : lfGetLines
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : to collect the detaile line from pack_line Into Temp File
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfGetLines()
*!**************************************************************************
FUNCTION lfGetLines
*  loPack_Lin = CreateObject("RemoteTable","Pack_Lin","Pack_Lin",lcTempPack_Lin,SET("DATASESSION")) 

*-SABER 12-20-2011 --------------- [Start]
LOCAL lcMajorPic, lnMajorLen
*-- Picture of the style major.
lcMajorPic = gfItemMask("PM")
*-- Lenght of the the style major.
lnMajorLen = LEN(lcMajorPic)
*-SABER 12-20-2011 --------------- [End]

loPack_Lin.SETORDER('PACKSTYLE')
SET ORDER TO TAG PACKCRTN IN (lcPakLnTmp)
SELECT (lcPackTmp)

SCAN 
  lnStyles = 0
  loPack_Lin.SEEK(&lcPackTmp..Pack_no)
  SELECT(lcTempPack_Lin)
  DO WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no
    lnCartonNO = no_cart
    llSameCarton = .F.
    IF lfSameCartn(&lcPackTmp..Pack_no,lnCartonNO,lnStyles)
      llSameCarton  = .T.
    ELSE  
      lnStyles = 0
    ENDIF
    SCAN REST WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no+STR(lnCartonNO,4)
      SCATTER MEMVAR MEMO
      IF llSameCarton
        SELECT (lcPakLnTmp)
        =SEEK(m.Pack_No+STR(lnCartonNO-1,4)+m.style)
        REPLACE To_Crt  WITH lnCartonNO  ,;
                No_Cart WITH lnCartonNO ,;
                Qty1    WITH Qty1 + m.Qty1 ,;
                Qty2    WITH Qty2 + m.Qty2 ,;
                Qty3    WITH Qty3 + m.Qty3 ,;
                Qty4    WITH Qty4 + m.Qty4 ,;
                Qty5    WITH Qty5 + m.Qty5 ,;
                Qty6    WITH Qty6 + m.Qty6 ,;
                Qty7    WITH Qty7 + m.Qty7 ,;
                Qty8    WITH Qty8 + m.Qty8 ,;
                TotQty  WITH TotQty + m.TotQty ,;
                Weight  WITH Weight + m.Weight
      ELSE  
        STORE m.No_Cart TO m.From_Crt,To_Crt,lnFromCrtn
        m.SKU = ''
  	    IF loSpck_Lin.SEEK('S' + EVAL(lcPackTmp+'.ACCOUNT') + m.Style)
		  m.SKU = EVAL(lcTempSpck_Lin+'.Pack_Id')
     	ENDIF   
	    m.cStyUpc = lfGetUpc()
        loOrdline.Seek('O'+&lcPackTmp..ORDER+STR(&lcTempPack_Lin..nordlineno,6),'Ordline')
        m.MGftMsg = lfGetGiftMsg() 
        *-SABER-------------------------------------------------------------------[Start]
        m.cDPCINum  = lfGetDPCI()
        m.MRetLoc   = lfGetRetLoc()        
        *-SABER 12-20-2011 --------------- [Start]
        m.cStyMajor =  SUBSTR(m.style,1,lnMajorLen)
        m.cStyleDesc = &lcTempOrdLine..Desc1
        *-SABER 12-20-2011 --------------- [End]
		*-SABER-------------------------------------------------------------------[End]
        INSERT INTO (lcPakLnTmp) FROM MEMVAR
        lnStyles = lnStyles + 1
      ENDIF
    ENDSCAN
  ENDDO
ENDSCAN

*!**************************************************************************
*! Name      : lfChkModls
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Cheak if moduls AL & AS instaled in the company.
*!**************************************************************************
*! Passed Parameters  : .........
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfChkModls()
*!**************************************************************************
FUNCTION lfChkModls
llModInst = .F.
llModInst = ('AL' $ oAriaApplication.CompanyInstalledModules  .OR. 'AS' $ oAriaApplication.CompanyInstalledModules  )
*-- llogtrmnat:- Variable Terminate The Option Grid.
IF !llModInst 
  =gfModalGen('TRM42083B00000','DIALOG','Advanced Shipment and Sales Order Allocation')
  llOgTrmnat = .T.
  CLEARREAD()
  RETURN
ENDIF
*-- End Of lfChkModls.
*!**************************************************************************
*! Name      : lfGetPack
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Retrieve Pack ID
*!**************************************************************************
*! Passed Parameters  : lcOrder,lcLineNo
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfGetPack()
*!**************************************************************************
FUNCTION lfGetPack
PARAMETERS lcOrder,lcStore,lcStyle,lcStyDyelot

PRIVATE lcAlias,lcCurTag

STORE "" TO lcPackId

lcAlias = ALIAS()

SELECT(lcOrdLnTmp)
lcCurTag = TAG()
IF lcOrdLnTmp  = lcTempOrdline
  SET ORDER TO ORDLINST
ELSE 
  SET ORDER TO TAG &lcOrdLnIndTmp
ENDIF 

IF llUse_config 
  IF SEEK("O"+ lcOrder + lcStore +lcStyle,lcOrdLnTmp) AND !EMPTY(Pack_Id) AND DYELOT = lcStyDyelot
    lcPackId = Pack_Id
  ENDIF
ELSE 
  IF SEEK("O"+ lcOrder + lcStore +lcStyle,lcOrdLnTmp) AND !EMPTY(Pack_Id)
    lcPackId = Pack_Id
  ENDIF
ENDIF 
SET ORDER TO &lcCurTag
SELECT &lcAlias
*!*************************************************************
*! Name      : lfMakeIndex
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMakeIndex
PARAMETERS lcTempName
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcCursor = lcTempName
lnBuffering = CURSORGETPROP("Buffering",lcCursor)
 =CURSORSETPROP("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
=lfCrtindex(lcCursor)
SELECT (lcCursor)
FOR lnI = 1 TO ALEN(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)

*!*************************************************************
*! Name      : lfCrtindex
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE

   *--temp. Customer File
   CASE UPPER(lcTable) = lcCustomer
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
     laIndex[1,2] = lcCustomer

   *--temp. ordhdr file
   CASE UPPER(lcTable) =  lcOrdHdr
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'CORDTYPE+ORDER'
     laIndex[1,2] = lcOrdHdr

   *--temp. ordline file
   CASE UPPER(lcTable) =  lcOrdLnTmp
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)'
      laIndex[1,2] = lcOrdLnIndTmp
      laIndex[2,1] = 'CORDTYPE+ORDER+STR(LINENO,6)'
      laIndex[2,2] = lcOrdLnTmp
      

   *--temp. pikline file
   CASE UPPER(lcTable) =  lcNotePad
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+key'
      laIndex[1,2] = lcNotePad

   CASE UPPER(lcTable) =  lcStyleFile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'Style'
      laIndex[1,2] = lcStyleFile

   CASE UPPER(lcTable) =  lcScalefile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+scale+prepak'
      laIndex[1,2] = lcScalefile

   CASE UPPER(lcTable) =  lcWareHous
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'cWareCode'
      laIndex[1,2] = lcWareHous
   *--lcPikTemp
   CASE UPPER(lcTable) =  lcPikTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'piktkt'
      laIndex[1,2] = lcPikTemp
   *--lcPckHdTemp
   CASE UPPER(lcTable) =  lcPckHdTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'pack_no'
      laIndex[1,2] = lcPckHdTemp


ENDCASE
*!*************************************************************
*! Name      : lfGetData
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : function to get the data from ordhdr and customer files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetData
SELECT (lcPackTmp)
LOCATE 
SCAN 
  =loCustomer.Seek('M'+ &lcPackTmp..Account)
  SELECT(lcTempCustomer)
  SCAN REST WHILE &lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store = 'M'+ &lcPackTmp..Account
    IF !SEEK(&lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcCustomer) FROM MEMVAR 
    ENDIF   
  ENDSCAN   
  =loCustomer.Seek('S'+ &lcPackTmp..Account)
  SELECT(lcTempCustomer)
  SCAN REST WHILE &lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store = 'S'+ &lcPackTmp..Account
    IF !SEEK(&lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcCustomer) FROM MEMVAR 
    ENDIF   
  ENDSCAN   
ENDSCAN 
SELECT (lcPackTmp)
LOCATE 
SCAN 
  loNotepad.seek("B" + ORDER)
  SELECT(lcTempNotepad)
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcNotePad) FROM MEMVAR 
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  looRDHDR.SEEK("O" + ORDER)
  SELECT(lcTempordhdr)
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcordhdr) FROM MEMVAR 
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  loOrdLine.Seek('O'+ &lcPackTmp..order + &lcPackTmp..Store)
  SELECT(lcTempOrdLine)
   SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ &lcPackTmp..order + &lcPackTmp..Store ;
             FOR  piktkt = &lcPackTmp..piktkt
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdLnTmp) FROM MEMVAR
  ENDSCAN 
ENDSCAN 

SELECT(lcPakLnTmp)
SCAN 
  loStyle.Seek(Style)
  SELECT(lcStyleFile)
  IF !SEEK(&lcTempStyle..Style,lcStyleFile)
    SELECT(lcTempStyle)
    SCATTER MEMO MEMVAR 
INSERT INTO (lcStyleFile) FROM MEMVAR 
  ENDIF
ENDSCAN 

SELECT(lcStyleFile)
SCAN 
  loScale.Seek('S'+ Scale)
  IF !SEEK('S'+ &lcTempScale..Scale,lcScalefile)
    SELECT(lcTempScale)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcScaleFile) FROM MEMVAR 
  ENDIF   
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  loWareHous.Seek(&lcPackTmp..cWareCode)
  IF !SEEK(&lcTempWareHous..cWareCode,lcWareHous)
    SELECT(lcTempWareHous)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcWareHous) FROM MEMVAR 
  ENDIF   
ENDSCAN 

*!*************************************************************
*! Name      : lfOpenSql
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfNoPackSel
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : function to be called in case of user does not
*!         select invoices
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfNoPackSel
IF loPack_hdr.llnative
  SELECT(lcTempPack_Hdr)
  SCAN FOR Status <> 'V'
    WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    SCATTER MEMVAR MEMO
	loOrdHdr.Seek('O' + m.Order)    
    IF EMPTY(M.CWARECODE)
      M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
    loNotepad.seek("B" + &lctempOrdHdr..ORDER)             
    m.MRetLoc = lfGetRetLoc()
    m.MOrdSt  = lfGetOrderSt()
    m.cPriority = lfGetPrior()
    INSERT INTO (lcPackTmp) FROM MEMVAR
    SELECT(lcPackTmp)
    IF !EMPTY(&lctempOrdHdr..CustPO)      
      lfGetBarCode(&lctempOrdHdr..CustPO)      
    ENDIF   	  
  ENDSCAN
  WAIT CLEAR
  =lfGetLines()
  lcCustomer  = lcTempCustomer
  lcNotePad   = lcTempNotePad
  lcOrdhdr    = lcTempOrdhdr
  lcOrdLnTmp  = lcTempOrdline
  lcStyleFile = lcTempStyle
  lcScaleFile = lcTempScale
  lcWareHous  = lcTempWareHous
ELSE
  lcSelFile =  "Pack_hdr"
  lcSelFld  = " * "
  lcSelCond = " Status <> 'V'"
  =lfOpenSql(lcSelFld ,lcSelFile ,lcPckHdTemp,lcSelCond)  
  SELECT(lcPckHdTemp)
  SCAN
    WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    SCATTER MEMVAR MEMO
	loOrdHdr.Seek('O' + m.Order)    
    IF EMPTY(M.CWARECODE)
       
       M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
    loNotepad.seek("B" + &lctempOrdHdr..ORDER)             
    m.MRetLoc = lfGetRetLoc()
    m.MOrdSt  = lfGetOrderSt()
    m.cPriority = lfGetPrior()
    
    INSERT INTO (lcPackTmp) FROM MEMVAR
    SELECT(lcPackTmp)
    IF !EMPTY(&lctempOrdHdr..CustPO)
      lfGetBarCode(&lctempOrdHdr..CustPO)      
    ENDIF   	  
    
  ENDSCAN 
  *--function to collect the lines 
  =lfGetLines()
  =lfGetData()
ENDIF 
*!**************************************************************************
*! Name      : lfSameCartn
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-17-2011
*! Purpose   : Check if the previous carton has the same information
*!**************************************************************************
*! Passed Parameters : lcPackNo    : Packinf list# 
*!                     lnCartnNo   : Carton #
*!                     lnSameLines : Previous carton total lines number
*!**************************************************************************
*! Return      : .T. If the previous carton has the same information
*!**************************************************************************
*! Example     : =lfSameCartn('000001',3,20)
*!**************************************************************************
FUNCTION lfSameCartn
PARAMETERS lcPackNo,lnCartnNo,lnSameLines
PRIVATE llSameCrtn,lnCrtnLines

llSameCrtn  = .T.
lnCrtnLines = 0

SELECT(lcTempPack_Lin)
SCAN REST WHILE pack_no+STR(no_cart,4)+style = lcPackNo+STR(lnCartnNo,4)
  =SEEK(lcPackNo+STR(lnCartnNo-1,4)+style,lcPakLnTmp)
  lnCrtnRange = &lcPakLnTmp..To_Crt - &lcPakLnTmp..From_Crt+1
  IF EOF(lcPakLnTmp) OR ;
     Qty1*lnCrtnRange  <> &lcPakLnTmp..Qty1 OR Qty2*lnCrtnRange <>  &lcPakLnTmp..Qty2 OR ;
     Qty3*lnCrtnRange  <> &lcPakLnTmp..Qty3 OR Qty4*lnCrtnRange <>  &lcPakLnTmp..Qty4 OR ;
     Qty5*lnCrtnRange  <> &lcPakLnTmp..Qty5 OR Qty6*lnCrtnRange <>  &lcPakLnTmp..Qty6 OR ;
     Qty7*lnCrtnRange  <> &lcPakLnTmp..Qty7 OR Qty8*lnCrtnRange <>  &lcPakLnTmp..Qty8 OR ;
     Weight*lnCrtnRange <> &lcPakLnTmp..Weight
    llSameCrtn = .F.
    EXIT
  ENDIF
  lnCrtnLines = lnCrtnLines + 1
ENDSCAN
llSameCrtn = llSameCrtn AND (lnCrtnLines=lnSameLines)
loPack_Lin.SEEK(lcPackNo+STR(lnCartnNo,4))
RETURN(llSameCrtn)


*!*************************************************************
*! Name      : lfpCount
*! Developer : Saber A.Razek (SAB)
*! Date      : 11-22-2011
*! Purpose   : Function to Calculate No. of Pages Per Packing List
*!*************************************************************      
FUNCTION lfpCount

lcAlis = SELECT(0)
SELECT (lcPackTmp)
lnRecNum = RECNO()
IF (lcLastPack <> &lcPackTmp..Pack_No)&&OrdHdr.Order) 
  IF !EMPTY(lcLastPack)
    REPLACE ALL nPageNo WITH lnPcnt  FOR Pack_No = lcLastPack IN (lcPackTmp)
  ENDIF 
  IF BETWEEN(lnRecNum ,1,RECCOUNT())
    GO RECORD lnRecNum IN (lcPackTmp)
  ENDIF
  lcLastPack = &lcPackTmp..Pack_No
  lnPcnt     = 1                
ELSE
  lnPcnt     = lnPcnt + 1
ENDIF
SELECT(lcAlis)

RETURN ''