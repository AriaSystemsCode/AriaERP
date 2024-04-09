*:****************************************************************************************
*: Program file  : ALBEL05.PRG (Copied from ALASI90.PRG)
*: Program desc. : Customized packing list program for Eagle's Eye
*: System        : Aria Advantage Series.
*: Module        : ALLOCATION (AL)
*: Developer     : Adel Mohhamed El Gazzar (ADEL)
*: Date          : 25/02/2001
*: Refer to      : C(102196)
*:****************************************************************************************
*:Modifications :
*:B604501,1 SSH Fix bug of print store instruction.
*:B604467,1 ABD 05/23/2001 Fix problem that the user cannot add the same style 
*:B604467,1 ABD            In the same Carton or in a new carton.
*:C102301,1 AAN 06/11/2001 Add a button to print a packing list direct from this screen.
*:C200205,1 SSH Update file ALPAKINF When saving the packing list.
*:B605734,1 ASH 03/27/2002 Fix the bug of losing size description after printing the packing slip.
*:B606639,1 ABD 12/18/2002 Update the number of carton with Carton No.  
*:****************************************************************************************
*-- Arrays to hold the folder,cartons,shipvia codes 
DECLARE lafoldwinds[2,2],laCartons[1,2],laShpVia[1],laCodInfo[1,10]

*-- Initializing the necessary variables for the program.
STORE '' TO lcEvenCode,lcVendNo,lcUccVer,lcWrName,lcWrAddr1,;
            lcWrAddr2,lcWrCity,lcWrState,lcWrZip,lcDistCtr,lcStName,;
            lcStAddr1,lcStAddr2,lcStCity,lcStState,lcStZip,lcDCName,;
            lcDCAddr1,lcDCAddr2,lcDCCity,lcDCState,lcDCZip,lcOldValue,;
            laShpVia,lcShipVia,lcPkChTxt,lcPkDsTxt
STORE SPACE(6) TO lcPackNo,lcOrderNo,lcBol,lcStyColor
STORE SPACE(5) TO lcAccount,lcDept
lcDivLName = ''
*-- Check the memo file existance.
*-- lcSndPort    Hold the name of the printing port.
llFoundP = FILE('&gcDataDir.ALASI90.MEM')
IF llFoundP
  RESTORE FROM &gcDataDir.ALASI90.MEM ADDITIVE
ELSE
  lcSndPort = "COM2"
ENDIF
llCancel = .F.
STORE 0 TO lnTotCart,lnTotPiece,lnTotWg,lnMarker,lnShpVia,laCodInfo,;
           lnToStorCn,laCartons
lcStore    = SPACE(08)
lcCustPo   = SPACE(10)
lcNotes    = SPACE(18)
lcSp_Inst1 = SPACE(78)
lcSp_Inst2 = SPACE(78)
lcPackChar = SPACE(05)
lcPackDesc = SPACE(07)

*-- Scan variables
STORE SPACE(12) TO lcUpc,lcStyle
lcSize     = SPACE(05)
STORE 0 TO lnQty1,lnQtyWg,lnStyWg,lnCartonNo,lnLastCart

*-- Variables to hold the non major segments.
STORE 0 TO lnClrPo,lnColorLen

*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))
*-- To get the non major segments.
=lfNonMaj()

llReBrow   = .T.
llUpdate   = .F.
lcScrMode  = 'S'
STORE .F. TO llBrowse,llEdiFound,llPkChrDes,llToStOrCn,llEdiExist
lcWarCode  = SPACE(10)
lcBrTit    = 'Cartons'

*-- Folder coordinations (starting row,col and ending row,col)
lnFolderCEnd = 97.60
lnFolderREnd = 2.00

*-- Number of folders
lnNoFld      =   2
lcfolder     = gfTempName()
lcwfoldchng  = '=lfActFolder()'
lnActFolder  = 1
lafoldwinds[1,1] = 'Header'
lafoldwinds[1,2] = 'albel052'
lafoldwinds[2,1] = 'Scan'
lafoldwinds[2,2] = 'albel054'
lcfoldprnt ='albel05'

*-- shipvia
laCodInfo[1,01] = "SHIPVIA"                 && Field Name
laCodInfo[1,02] = "laShpVia"                && Array Name
laCodInfo[1,03] = "lnShpVia"                && Popup Name
laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = "Pack_Hdr"                && Alternative File (For default val.)
laCodInfo[1,08] = "Pack_Hdr"                && Use this index for the Alternative file.
laCodInfo[1,09] = "lcShipVia"               && Seek this expretion.
laCodInfo[1,10] = "ShipVia"                 && Alternative Field Name
= gfwCodePop(@laCodInfo, "SHIPVIA", "N")

*-- Opening the necessary files
TmpPkLin   = gfTempName()
lcAsnLabel = gfTempName()
tmpAsnShip = gfTempName()
lcObjColor = ",RGB(,,,192,192,192),,,,,,,,RGB(100,100,100,192,192,192)"
lcBackClr  = ',,,,,,,,RGB(192,192,192,192,192,192),;
             RGB(192,192,192,192,192,192),'
lcPinBack  = 'RGB(192,192,192,192,192,192)'
lcTopLeft  = 'RGB(128,128,128,128,128,128)'
lcBotRgt   = 'RGB(255,255,255,255,255,255)'

IF !gfSetup()
  RETURN
ENDIF  

llEdiSys  = ('EB' $ gcComp_mdl)

DIMENSION laSetup[4,2]
laSetUp[1,1] = 'M_DYELOT'
laSetUp[2,1] = 'M_WareHouse'
laSetUp[3,1] = 'M_UPC_USE'
laSetUp[4,1] = 'XMANUFID'
=gfGetMemVar(@laSetUp,gcAct_Comp)

llDyelot  = ALLTRIM(laSetUp[1,2]) = 'Y'
llMulWare = ALLTRIM(laSetUp[2,2]) = 'Y'
llUseUpc  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcManufId = ALLTRIM(laSetUp[4,2])

*-- Checking if the system is using the UPC or not. IF its not then displays
*-- a message to inform the user that the system has not been setup to use
*-- UPC numbers. Cannot proceed
IF !llUseUpc
  =gfModalGen('INM44061B00000','DIALOG' )
  RETURN
ENDIF

=gfOpenFile(gcDataDir+'PIKTKT','PIKTKT','SH')
=gfOpenFile(gcDataDir+'CUSTDEPT','CUSTDEPT','SH')
=gfOpenFile(gcDataDir+'PACK_HDR','PACK_HDR','SH')
=gfOpenFile(gcDataDir+'PACK_LIN','PACK_CRT','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINST','SH')
=gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'CODES','CODES','SH')
=gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
=gfOpenFile(gcDataDir+'EdiAcPrt','AccFact','SH')
=gfOpenFile(gcDataDir+'EdiPh','Partner','SH')
=gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
=gfOpenFile(gcDataDir+'EdiPd','PARTTRANS','SH')
IF llEdiSys
  =gfOpenFile(gcDataDir+'BOL_HDR','BOL_HDR','SH')
  =gfOpenFile(gcDataDir+'BOL_LIN','BOL_LIN','SH')
  =gfOpenFile(gcDataDir+'Asn_Ship','Asn_Ship','SH')
  COPY STRUCTURE TO (gcWorkDir+tmpAsnShip)
  =gfOpenFile(gcWorkDir+tmpAsnShip,'','EX')

  INDEX ON PACK_NO+STR(CART_NO,6) TAG (tmpAsnShip)
  =gfOpenFile(gcSysHome+'SYCASNLB','ASNlbl','SH')    
ENDIF

IF !llMulWare
  GOTO TOP  IN WareHous
  lcWrName  = WareHous.cDesc
  lcWrAddr1 = WareHous.cAddress1
  lcWrAddr2 = WareHous.cAddress2
  lcWrCity  = ALLTRIM(WareHous.cAddress3)
  lcWrState = ALLTRIM(WareHous.cAddress4)
  lcWrZip   = ALLTRIM(WareHous.cAddress5)
ENDIF

SELECT STYLE
SET RELATION TO 'S'+Scale INTO SCALE ADDITIVE
SELECT PikTkt
*:C200205,1 SSH [Start] Change the relation to support the store.
*SET RELATION TO 'M'+ Account INTO Customer, 'O'+Order INTO OrdHdr ADDITIVE
SET RELATION TO IIF(EMPTY(Store),'M'+ Account ,'S'+ Account+Store ) INTO Customer, 'O'+Order INTO OrdHdr ADDITIVE
*:C200205,1 SSH [End..].
lcConfStat = SET('CONFIRM')
CREATE TABLE (gcWorkDir+TmpPkLin) ;
(CartonNo N(4),cUpc C(12),Style C(19),cSize C(5), Size C(1),;
 TotQty N(5),Weight N(10,2) ,OldCrtNo N(4), OrdLineNo N(6))
INDEX ON STR(OrdLineNo,6)+STR(CartonNo,4)+cUpc TAG OrdLineNo
INDEX ON STR(CartonNo,4)+cUpc+STR(OrdLineNo,6) TAG (TmpPkLin) ADDITIVE
SET ORDER TO TAG (TmpPkLin)

PUSH KEY
=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTit)
llFirst = .T.
DO gcScrDir+"AL\albel05.SPR"
ON KEY
POP KEY
RETURN

*!*************************************************************
*! Name      : lfActFolder
*! Purpose   : Function to be used when moving through folders
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*!*************************************************************
*! Example            :  lfActFolder()
*!*************************************************************
FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
    lcStatus = IIF(lcScrMode $ 'AE','ENABLE','DISABLE')
    SHOW GETS WINDOW 'albel056' DISABLE
    SHOW GETS WINDOW 'albel052' &lcStatus
    IF !llPkChrDes
      SHOW GET lcPackChar DISABLE
      SHOW GET lcPackDesc DISABLE
    ENDIF
    IF !llToStOrCn
      SHOW GET lnToStorCn DISABLE
    ENDIF
  CASE lnActFolder = 2
    SELECT (tmpPkLin)  
    =IIF(llReBrow,lfBrows(),.T.)
    SHOW GETS WINDOW 'albel056' DISABLE
    SHOW GET ibBrow ENABLE
    IF lcScrMode <> 'S' .AND. lnCartonNo > 0
      SHOW GET pbPrntLab ENABLE
    ENDIF
    IF INLIST(lcScrMode,'A','E')
      SHOW GET pbNewCart ENABLE
      SHOW GET ibBrow    ENABLE
      IF lnCartonNo > 0
        SHOW GET lnQty1    ENABLE
        SHOW GET pbPrntLab ENABLE
        SHOW GET pbNewUpc  ENABLE
        SHOW GET pbRemUpc  ENABLE
      ENDIF
    ENDIF
ENDCASE  

*!*************************************************************
*! Name      : lfvShipVia
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to validate the shipvia
*!*************************************************************
*! Example            :  lfvShipVia()
*!*************************************************************
FUNCTION lfvShipVia

lcShipVia = laShpVia[lnShpVia,2]

*!*************************************************************
*! Name      : lfGetUpc
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to add upc records in the TmpPkLin file
*!             in order to use it as a brows in the second folder 
*!*************************************************************
*! Example            :  lfGetUpc()
*!*************************************************************
FUNCTION lfGetUpc
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (TmpPkLin)
ZAP
STORE 0 TO lnLastCart,lnCartonNo
IF SEEK (lcPackNo,'PACK_LIN')
  SELECT Pack_Lin
  SCAN REST WHILE Pack_No = lcPackNo
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF Pack_Lin.Qty&lcCount <> 0 .AND. ;
        SEEK(Pack_Lin.Style+lcCount,'StyleUpc') AND SEEK(Pack_Lin.Style,'Style')
        SELECT (TmpPkLin)
        FOR lnCount2 = Pack_Lin.From_Crt TO Pack_Lin.To_Crt
          IF !SEEK(STR(lnCount2,4))
            lnLastCart = lnLastCart + 1
            DIMENSION laCartons[lnLastCart,2]
            STORE 0 TO laCartons[lnLastCart,1],laCartons[lnLastCart,2]
            lnCartonNo = lnLastCart
          ELSE
            lnCartonNo = lnCount2
          ENDIF  
          APPEND BLANK
          REPLACE cUpc      WITH StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3,;
                  Style     WITH Pack_Lin.Style                           ,;
                  cSize     WITH Scale.Sz&lcCount                         ,;
                  Size      WITH lcCount                                  ,;
                  Weight    WITH Pack_Lin.Qty&lcCount*Style.nStyWeight        ,; 
                  CartonNo  WITH lnCount2   							  ,;
                  OldCrtNo  WITH lnCount2   							  ,;
                  TotQty    WITH Pack_Lin.Qty&lcCount 					  ,;
                  OrdLineNo WITH Pack_Lin.nOrdLineNo
          laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + TotQty
          laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + Weight
        ENDFOR
      ENDIF
    ENDFOR
  ENDSCAN
  GO TOP IN (TmpPkLin)
  lnCartonNo = IIF(lnLastCart>0,1,0)
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfBrows
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to browse the upc
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfBrows

SELECT (TmpPkLin)
lnMarker = RECNO()
IF llFirst
  BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnMarker ,'>',' ') :R:1:W=.F.,;
         CartonNo :R,;
         cUpc     :R,;
         TotQty   :R;
         WINDOW    albel055;
         IN WINDOW albel054;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
  	     NOCLEAR           ;
  	     WHEN lfShowData() ;
         TITLE lcBrTit
ELSE
  BROWSE FIELDS ;
        cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,;
        CartonNo :H= 'Crtn#' :R,;
        Style      = SUBSTR(Style,1,lnMajLen):R,;
        Color      = SUBSTR(Style,lnClrPo,lnColorLen):R,;
        cSize    :H= 'Size'  :R,;
        TOtQty   :H= 'Qty. ' :R;
        WINDOW    albel055;
        IN WINDOW albel054;
        NOMENU            ;         
        NOAPPEND          ;
        NODELETE          ;         
        NOWAIT            ;
        SAVE              ;
        NOCLEAR           ;
 	    WHEN lfShowData() ;
        TITLE lcBrTit
  =lfShowData()
  llReBrow = .F.
ENDIF

*!*************************************************************
*! Name      : lfShowData
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to display upc data when moving through
*!             upc records in the brows
*!*************************************************************
*! Example            :  lfShowData()
*!*************************************************************
FUNCTION lfShowData

lcUpc      = &TmpPkLin..cUpc
lnQty1     = &TmpPkLin..TotQty
lcStyle    = SUBSTR(&TmpPkLin..Style,1,lnMajLen)
lcStyColor = SUBSTR(&TmpPkLin..Style,lnClrPo,lnColorLen)        
lcSize     = &TmpPkLin..cSize
=SEEK(&TmpPkLin..Style,'Style')
lnStyWg    = Style.nStyWeight
lnCartonNo = &TmpPkLin..CartonNo
SHOW GETS WINDOW 'albel056'
lnMarker = RECNO(TmpPkLin)
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvFirst
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to get the first pack slip
*!*************************************************************
*! Example            :  lfvFirst()
*!*************************************************************
FUNCTION lfvFirst

SELECT Pack_Hdr
GOTO TOP
lcPackNo = Pack_No
=LPSHOW()

*!*************************************************************
*! Name      : lfvPrev
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to get the Previous pack slip
*!*************************************************************
*! Example            :  lfvPrev()
*!*************************************************************
FUNCTION lfvPrev

SELECT Pack_Hdr
SKIP -1
IF BOF() 
  =gfModalGen('INM44063B00000','DIALOG' )
  GOTO TOP
ENDIF
lcPackNo = Pack_No
=LPSHOW()

*!*************************************************************
*! Name      : lfvNext
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to get the next pack slip
*!*************************************************************
*! Example            :  lfvNext()
*!*************************************************************
FUNCTION lfvNext

SELECT Pack_Hdr
SKIP
IF EOF() 
  =gfModalGen('INM44064B00000','DIALOG' )
  GO BOTTOM
ENDIF
lcPackNo = Pack_No
=lpShow()

*!*************************************************************
*! Name      : lfvLast
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to get the last pack slip
*!*************************************************************
*! Example            :  lfvLast()
*!*************************************************************
FUNCTION lfvLast

SELECT Pack_Hdr
GOTO BOTTOM
lcPackNo = Pack_No
=lpShow()

*!*************************************************************
*! Name      : lfvUpc
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to validate the upc number
*!*************************************************************
*! Example            :  lfvUpc()
*!*************************************************************
FUNCTION lfvUpc
PRIVATE lnRecNo,lnPicked,lnOrdered

*--- keyboard buffer as the scanner somtimes issue CHR(13) causing any popup disappered.
CLEAR TYPEAHEAD
SET ORDER TO TAG STYUPCN IN STYLEUPC

IF !EMPTY(lcUpc)
  IF !SEEK(lcUpc,'StyleUpc')
    *-- Upc ... not found.
    ?? CHR(7)
    =gfModalGen('INM44065B00000','DIALOG',lcUpc)
    lcUpc = SPACE(12)
    _CUROBJ = OBJNUM(lcUpc)
    RETURN
  ENDIF  
  SELECT OrdLine
  =SEEK('O'+lcOrderNo+lcStore+StyleUpc.Style)
  LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                    'O'+lcOrderNo+lcStore+StyleUpc.Style;
         FOR PikTkt = lcPackNo
  
  IF !FOUND()       
    *-- UPC ð is not found in piktkt# ð.
    ?? CHR(7)
    =gfModalGen('INM44066B00000','DIALOG',lcUpc+'|'+lcPackNo)
    lcUpc = SPACE(12)
    _CUROBJ = OBJNUM(lcUpc)
    SELECT (TmpPkLin)
    RETURN
  ENDIF
  llUpdate  = .T.
  SELECT (TmpPkLin)
  lnRecNo = RECNO()
  SUM TotQty TO lnPacked FOR cUPc = lcUpc
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  
  SELECT OrdLine
  STORE 0   TO lnPicked,lnOrdered
  =SEEK('O'+lcOrderNo+lcStore+StyleUpc.Style)
  SCAN REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                  'O'+lcOrderNo+lcStore+StyleUpc.Style FOR PikTkt = lcPackNo
    lnPicked  = lnPicked  + EVAL('PIK'+ALLTRIM(StyleUpc.Size))
    lnOrdered = lnOrdered + EVAL('QTY'+ALLTRIM(StyleUpc.Size))
    IF lnPacked+1 <= lnPicked
      EXIT
    ENDIF
  ENDSCAN

  *B604467,1 ABD - Fix problem that the user cannot add the same style 
  *B604467,1 ABD - In the same Carton or in a new carton , so we 
  *B604467,1 ABD - remark the next lines that check on the Qty. [Begain]
  *=SEEK(StyleUpc.Style,'Style')
  **-- Style\Color\Size ð is not included in this order. It cannot be packed.
  *IF lnPacked+1 > lnOrdered
  *  ?? CHR(7)
  *ENDIF  
  *IF lnPacked+1 > lnOrdered .AND. ;
  *  gfModalGen('INM44072B00000','DIALOG',StyleUpc.Style+'\'+EVAL('SCALE.SZ'+ALLTRIM(StyleUpc.Size)) ) = 1
  *  lcUpc = SPACE(12)
  *  _CUROBJ = OBJNUM(lcUpc)
  *  SELECT (TmpPkLin)
  *  RETURN
  *ENDIF
  *B604467,1 ABD - [End]
  
  IF lnPacked+1 > lnPicked
    *-- Packed quantity for ð exceeds picked quantity. Do you want to add in the packing list?
    ?? CHR(7)
    IF gfModalGen('QRM44073B44009','DIALOG',StyleUpc.Style+'\'+EVAL('SCALE.SZ'+ALLTRIM(StyleUpc.Size)) ) = 2     
      lcUpc = SPACE(12)
      _CUROBJ = OBJNUM(lcUpc)
     SELECT (TmpPkLin)
      RETURN
    ENDIF
    SELECT OrdLine
    =SEEK('O'+lcOrderNo+lcStore+StyleUpc.Style)
    LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                      'O'+lcOrderNo+lcStore+StyleUpc.Style;
           FOR PikTkt = lcPackNo
  ENDIF

  SELECT (TmpPkLin)
  =lfAddRec()
  SHOW WINDOW (lcBrTit) REFRESH SAME
  =lfClearFld()
  _CUROBJ = OBJNUM(lcUpc)
ELSE
  IF laCartons[lnCartonNo,1] > 0
  ELSE  
    GO TOP
    STORE 0 TO lnCartonNo,lnLastCart
    DO WHILE !EOF()
      lnLastCart = lnLastCart + 1
      lnCartonNo = lnLastCart
      DIMENSION laCartons[lnCartonNo,2]
      STORE 0 TO laCartons[lnCartonNo,1],laCartons[lnCartonNo,2]
      lnOldCarton = CartonNo
      SCAN REST WHILE STR(CartonNo,4)+cUpc = STR(lnOldCarton,4)
        REPLACE CartonNo WITH lnCartonNo
        laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + TotQty
        laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + Weight
      ENDSCAN
    ENDDO
    GO TOP
  ENDIF
  SHOW GET lcUpc DISABLE
  IF lnCartonNo > 0
    SHOW GET lnQty1    ENABLE
    SHOW GET pbNewUpc  ENABLE
    SHOW GET pbRemUpc  ENABLE
    SHOW GET pbPrntLab ENABLE
  ENDIF
  SET CONFIRM &lcConfStat
  =lfShowData()
ENDIF  
SET ORDER TO TAG STYLEUPC IN STYLEUPC

*!*************************************************************
*! Name      : lfClearFld
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to clear the scan variables
*!*************************************************************
*! Example            :  lfClearFld()
*!*************************************************************
FUNCTION lfClearFld

STORE SPACE(12) TO lcUpc, lcStyle
STORE 0 TO lnStyWg, lnQty1
lcStyColor = SPACE(6)
lcSize     = SPACE(5)

*!*************************************************************
*! Name      : lfvModify
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to modify an existent pack slip
*!             called from push buttom modify
*!*************************************************************
*! Example            :  lfvModify()
*!*************************************************************
FUNCTION lfvModify

lcScrMode = 'E'
=LPSHOW()

*!*************************************************************
*! Name      : lfNewCart
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to make a new carton
*!*************************************************************
*! Example            :  lfNewCart()
*!*************************************************************
FUNCTION lfNewCart

lnLastCart = lnLastCart + 1
lnCartonNo = lnLastCart
=lfClearFld()
DIMENSION laCartons[lnCartonNo,2]
STORE 0 TO laCartons[lnCartonNo,1],laCartons[lnCartonNo,2]
IF lnCartonNo <> 1
  =lfPrntLab(lnCartonNo-1)
ENDIF
SHOW GETS WINDOW 'albel056' DISABLE
SHOW GET lcUpc     ENABLE
SHOW GET pbNewCart ENABLE
SET CONFIRM ON
_CUROBJ = OBJNUM(lcUpc)

*!*************************************************************
*! Name      : lfvNewUpc
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to add new upc to a carton
*!*************************************************************
*! Calls     : lfClearFld
*!*************************************************************
*! Example            :  lfvNewUpc()
*!*************************************************************
FUNCTION lfvNewUpc

=lfClearFld()
SHOW GETS WINDOW 'albel056' DISABLE
SHOW GET lcUpc     ENABLE
SHOW GET pbNewCart ENABLE
SET CONFIRM ON
_CUROBJ = OBJNUM(lcUpc)

*!*************************************************************
*! Name      : lfWQty
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : When Function for upc quantity field
*!*************************************************************
*! Example            :  lfWQty()
*!*************************************************************
FUNCTION lfWQty
lcOldValue = lnQty1

*!*************************************************************
*! Name      : lfvQty
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Valid Function for upc quantity field
*!*************************************************************
*! Example            :  lfvQty()
*!*************************************************************
FUNCTION lfvQty

*B604467,1 ABD Fix problem that the user can add Negative Qty. [Begin]
*IF (TYPE("lnQty1") # "N") OR (lnQty1 > 99999)
IF (TYPE("lnQty1") # "N") .OR. (lnQty1 > 99999) .OR. (lnQty1 <= 0)
*:B604467,1 ABD [End]


  lnQty1 = lcOldValue
  SHOW GET lnQty1
  _CUROBJ = _CUROBJ
  RETURN 
ENDIF
SELECT (tmpPkLin)
REPLACE TotQty WITH TotQty - lcOldValue + lnQty1 ,;
        Weight WITH TotQty*lnStyWg
laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] - lcOldValue + lnQty1
laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] - (lcOldValue-lnQty1)*lnStyWg
lnTotWg    = lnTotWg    - (lcOldValue-lnQty1)*lnStyWg
lnTotPiece = lnTotPiece - lcOldValue + lnQty1
=lfRefresh('albel056')
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvRemUpc
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Remove UPC from a carton
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Example            :  lfvRemUpc()
*!*************************************************************
FUNCTION lfvRemUpc

*IF gfDialog('?','Are you sure you want to remove UPC# '+lcUpc+' from carton# '+ALLTRIM(STR(lnCartonNo,4))+'.','\<Remove;\!\?\<Cancel')=1
IF gfModalGen("QRM44067B44008","Dialog",lcUpc+'|'+ALLTRIM(STR(lnCartonNo,4))) = 1
  SELECT (tmpPkLin)
  laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] - TotQty
  laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] - Weight
  lnTotWg    = lnTotWg    - Weight
  lnTotPiece = lnTotPiece - TotQty
  DELETE
  llUpdate  = .T.
  IF !SEEK(STR(lnCartonNo,4))
    WAIT 'Rearranging cartons...' WINDOW NOWAIT
    IF llEdiSys AND SEEK(lcPackNo+STR(lnCartonNo,4),tmpAsnShip)
      SELECT (tmpAsnShip)
      DELETE
    ENDIF
    SELECT (tmpPkLin)
    GO TOP
    STORE 0 TO lnCartonNo,lnLastCart
    DO WHILE !EOF()
      lnLastCart = lnLastCart + 1
      lnCartonNo = lnLastCart
      DIMENSION laCartons[lnCartonNo,2]
      STORE 0 TO laCartons[lnCartonNo,1],laCartons[lnCartonNo,2]
      lnOldCarton = CartonNo
      SCAN REST WHILE STR(CartonNo,4)+cUpc = STR(lnOldCarton,4)
        REPLACE CartonNo WITH lnCartonNo
        laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + TotQty
        laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + Weight
      ENDSCAN
    ENDDO
    WAIT CLEAR
    GO TOP
    IF lnCartonNo = 0
      =lfClearFld()
      SHOW GETS WINDOW 'albel056' DISABLE
      SHOW GET ibBrow    ENABLE
      SHOW GET pbNewCart ENABLE
      _CUROBJ = OBJNUM(pbNewCart)
    ENDIF
  ENDIF
  SHOW WINDOW (lcBrTit) REFRESH SAME
  =lfRefresh('albel056')
ENDIF

*!*************************************************************
*! Name      : lpShow
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Screen show function
*!*************************************************************
*! Calls     : lfchngfolder,lfReIntiVr,lfvPackInfo,lfGetUpc,lfBrows
*!*************************************************************
*! Example            :  lpShow()
*!*************************************************************
FUNCTION lpShow
DO CASE
  CASE lcScrMode = 'S'
    IF lnactfolder <> 1
      lnlastfold = lnactfolder
      lnactfolder=1
      =lfchngfolder(lnactfolder)
    ENDIF
    =lfReIntiVr()
    llReBrow = .T.
    SHOW GET pbPackNo   ENABLE
    SHOW GET lcPackNo   ENABLE
    SHOW GET pbAccount  ENABLE
    SHOW GET lcAccount  ENABLE
    SHOW GET pbStore    DISABLE
    SHOW GET lcStore    DISABLE
    SHOW GET pbOrder    ENABLE
    SHOW GET lcOrderNo  ENABLE
    SHOW GET lcCustPo   ENABLE
    SHOW GET lcDept     DISABLE
    =lfRefresh('albel051')
    SHOW GETS WINDOW 'albel052' DISABLE
    SHOW GETS WINDOW 'albel053' DISABLE
    SHOW GET pbClose,1 ENABLE PROMPT '\<Close' 
    _CUROBJ = OBJNUM(lcPackNo)
  CASE lcScrMode = 'V'
    =lfvPackInfo()
    SHOW GETS WINDOW 'albel051' DISABLE
    SHOW GETS WINDOW 'albel052' DISABLE
    SHOW GETS WINDOW 'albel053' ENABLE
    SHOW GETS WINDOW 'albel056' DISABLE
    SHOW GET lcPackChar DISABLE COLOR &lcObjColor
    SHOW GET lcPackDesc DISABLE COLOR &lcObjColor
    SHOW GET lnToStorCn DISABLE    
    SHOW GET pbClose,1 ENABLE PROMPT '\<Close' 
    SHOW GET pbSave DISABLE
    =lfGetUpc()
    IF lnActFolder=2
      IF lnCartonNo > 0
        SHOW GET pbPrntLab ENABLE
      ENDIF
      =lfBrows()
      SHOW GET ibBrow ENABLE
    ENDIF
  CASE lcScrMode = 'E'
    IF lnActFolder = 1
      SHOW GETS WINDOW 'albel052' ENABLE
    ELSE
      IF lnCartonNo > 0
        SHOW GET lnQty1    ENABLE
        SHOW GET pbPrntLab ENABLE
        SHOW GET pbNewUpc  ENABLE
        SHOW GET pbRemUpc  ENABLE
      ENDIF
      =lfBrows()
      SHOW GET pbNewCart ENABLE
      SHOW GET ibBrow    ENABLE
    ENDIF
    SHOW GETS WINDOW 'albel053' DISABLE
    SHOW GET pbSave ENABLE
    SHOW GET pbClose,1 ENABLE PROMPT '\<Cancel'
  CASE lcScrMode = 'A'
    SHOW GETS WINDOW 'albel051' DISABLE 
    IF lnActFolder = 1
      SHOW GETS WINDOW 'albel052' ENABLE
      IF !llPkChrDes
        SHOW GET lcPackChar DISABLE
        SHOW GET lcPackDesc DISABLE
      ELSE
        SHOW GET lcPackChar ENABLE
        SHOW GET lcPackDesc ENABLE
      ENDIF  
    ELSE
      SHOW GETS WINDOW 'albel056' ENABLE
    ENDIF
    SHOW GETS WINDOW 'albel053' DISABLE
    SHOW GET pbSave ENABLE
    SHOW GET pbClose,1 ENABLE PROMPT '\<Cancel'
    lnLastCart = 0
ENDCASE

*!*************************************************************
*! Name      : lfvClose
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to cancel or close
*!*************************************************************
*! Calls     : gfclsdata,lpShow
*!*************************************************************
*! Example            :  lfvClose()
*!*************************************************************
FUNCTION lfvClose

IF lcScrMode $ 'SV'
  *=gfclsdata()
  CLEAR READ
  RETURN
ELSE
  *IF llUpdate .AND. ;
    gfDialog('?','Are you sure? you will lose all changes.','\<Yes;\!\?\<No')= 2
  IF llUpdate .AND. ;
    gfModalGen("QRM00031B44009","Dialog")= 2
    RETURN
  ENDIF
  lcScrMode = IIF(lcScrMode='E','V','S')
  SELECT (TmpPkLin)
  ZAP
  IF llEdiSys
    SELECT (tmpAsnShip)
    ZAP
  ENDIF  
  =lpShow()
ENDIF

*!*************************************************************
*! Name      : lfReIntiVr
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to reintializing the global variables
*!*************************************************************
*! Example            :  lfReIntiVr()
*!*************************************************************
FUNCTION lfReIntiVr

lcPackNo   = SPACE(06)
lcOrderNo  = SPACE(06)
lcAccount  = SPACE(05)
lcStore    = SPACE(08)
lcCustPo   = SPACE(10)
lcDept     = SPACE(05)
lcDivLName =''
lnTotCart  = 0
lcBol      = SPACE(06)
lnTotPiece = 0
lnTotWg    = 0
lcShipVia  = ''
lnShipVia  = 0
lcNotes    = SPACE(18)
lcPackChar = SPACE(05)
lcPackDesc = SPACE(07)
lnToStorCn = 0
lcUpc      = SPACE(12)
lnQty1     = 0
lcStyle    = SPACE(12)
lcStyColor = SPACE(06)
lcSize     = SPACE(05)
lcSp_Inst1 = SPACE(78)
lcSp_Inst2 = SPACE(78)
lnQtyWg    = 0
lnCartonNo = 0
lnLastCart = 0
= gfwCodePop(@laCodInfo, "SHIPVIA", "N")
STORE .F. TO llPkChrDes,llUpdate,llReBrow,llToStOrCn
STORE '' TO lcPkChTxt,lcPkDsTxt
SELECT (TmpPkLin)
ZAP
IF llEdiSys
  SELECT (tmpAsnShip)
  ZAP
ENDIF

*!*************************************************************
*! Name      : lfAddRec
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to Add record in the TmpPkLin file
*!*************************************************************
*! Example            :  lfAddRec()
*!*************************************************************
FUNCTION lfAddRec

IF SEEK(StyleUpc.Style,'Style')
  lcStySize = ALLTRIM(StyleUpc.Size)
  SELECT (TmpPkLin)
  IF !SEEK (STR(lnCartonNo,4)+lcUpc+STR(OrdLine.LineNo,6))
    INSERT INTO (TmpPkLin) (CartonNo,Style,cSize,Size,cUpc,OrdLineNo) VALUES ;
    (lnCartonNo,Style.Style,Scale.Sz&lcStySize,ALLTRIM(StyleUpc.Size),lcUpc,OrdLine.LineNo)
  ENDIF
  REPLACE TotQty  WITH TotQty+1 ,;
          Weight  WITH Weight+Style.nStyWeight
  lnTotWg    = lnTotWg    + Style.nStyWeight
  lnTotPiece = lnTotPiece + 1
  laCartons[lnCartonNo,1] = laCartons[lnCartonNo,1] + 1
  laCartons[lnCartonNo,2] = laCartons[lnCartonNo,2] + Style.nStyWeight
  =lfRefresh('albel056')
ENDIF

*!*************************************************************
*! Name      : lfvAccount
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Validate account
*!*************************************************************
*! Calls     : CUSBrowM
*!*************************************************************
*! Example            :  lfvAccount()
*!*************************************************************
FUNCTION lfvAccount
IF llBrowse OR (!EMPTY(lcAccount) AND !SEEK('M'+lcAccount,'CUSTOMER'))
  DO CUSBrowM WITH lcAccount
ENDIF
llBrowse = .F.
IF !EMPTY(lcAccount)
  SHOW GET lcStore ENABLE
  SHOW GET pbStore ENABLE
ELSE
  SHOW GET lcStore DISABLE
  SHOW GET pbStore DISABLE
ENDIF
=lfRefresh('albel051')

*!*************************************************************
*! Name      : lfvStore
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Validate store
*!*************************************************************
*! Calls     : CUSBROWS
*!*************************************************************
*! Example            :  lfvStore()
*!*************************************************************
FUNCTION lfvStore
PRIVATE xStore
IF llBrowse OR (!EMPTY(lcStore) AND !SEEK('S'+lcAccount+lcStore,'CUSTOMER'))
  xStore   = lcStore
  IF !CUSBROWS(lcAccount,.T.)
    STORE SPACE(8) TO xStore
  ENDIF
  lcStore = xStore
ENDIF
llBrowse = .F.
=lfRefresh('albel051')

*!*************************************************************
*! Name      : lfvOrder
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Validate Order#
*!*************************************************************
*! Calls     : ORDBROWO,lfvPikTkt
*!*************************************************************
*! Example            :  lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE xStore,xOrder,xAccount
xAccount = lcAccount
xStore   = lcStore
xOrder   = lcOrderNo
IF EMPTY(lcAccount)
  IF llBrowse OR (!EMPTY(lcOrderNo) AND !SEEK('O'+lcOrderNo,'ORDHDR'))
    =ORDBROWO(@lcOrderNo)
  ENDIF
ELSE
  SET ORDER TO TAG ORDACCT IN ORDHDR
  IF llBrowse OR (!EMPTY(lcOrderNo) AND !SEEK(lcAccount+'O'+lcOrderNo,'ORDHDR'))
    llBrowse = .T.
    =ORDBROWA(lcAccount,.T.)
    lcOrderNo = xOrder
  ENDIF
  SET ORDER TO TAG ORDHDR IN ORDHDR
ENDIF
llBrowse = .F.
IF !EMPTY(lcOrderNo)
  lcAccount = OrdHdr.Account
  lcStore   = OrdHdr.Store
  lcCustPo  = OrdHdr.CustPo
  =lfvPikTkt()
ENDIF

*!*************************************************************
*! Name      : lfvCustPo
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Validate CustPo#
*!*************************************************************
*! Calls     : lfvPikTkt
*!*************************************************************
*! Example            :  lfvCustPo()
*!*************************************************************
FUNCTION lfvCustPo
IF !EMPTY(lcAccount) .OR. !EMPTY(lcCustPo)
  =lfvPikTkt()
ENDIF

*!*************************************************************
*! Name      : lfvPackNo
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Validate Pack#
*!*************************************************************
*! Calls     : lfvPikTkt
*!*************************************************************
*! Example            :  lfvPackNo()
*!*************************************************************
FUNCTION lfvPackNo
PRIVATE lnAlias
IF llBrowse .OR. !EMPTY(lcPackNo)
  =lfvPikTkt()
ENDIF
llBrowse = .F.

*!*************************************************************
*! Name      : lfvPikTkt
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Validate PikTkt#
*!*************************************************************
*! Calls     : gfModalGen,AriaBrow,lpShow
*!*************************************************************
*! Example            :  lfvPikTkt()
*!*************************************************************
FUNCTION lfvPikTkt
PRIVATE laBrowVal
PRIVATE laAddress
DIMENSION laBrowVal[1],laAddress[6,3]
STORE '' TO laBrowVal,laAddress
SELECT PikTkt
IF !SEEK(lcPackNo)
  lcBrFields  = [PikTkt:H='PACK No.',]+;
                [Order:H='Order#',]+;
                [Store:H='Store',]+;
                [OrdHdr.Complete:H='Complete',]+;
                [Date:H='PikDate',]+;
                [Account:H='Account',]+;
                [Customer.StName:H='Account Name']

  DO CASE
    CASE !EMPTY(lcOrderNo)
      SET ORDER TO TAG ORDPIK IN PIKTKT
      IF !SEEK(lcOrderNo)        
        =gfModalGen("INM44028B00000","Dialog",lcOrderNo)
      ELSE
        IF AriaBrow('lcOrderNo','Pick Tickets',gnBrFSRow1, gnBrFSCol1,;
           gnBrFSRow2, gnBrFSCol2,.F.,.F.,"PikTkt.PikTkt","laBrowVal")
          lcPackNo = laBrowVal[1]
        ELSE
          lcPackNo = SPASCE(6) 
        ENDIF
      ENDIF
      SET ORDER TO TAG PIKTKT IN PIKTKT
    OTHERWISE
      LOCATE FOR ACCOUNT = ALLTRIM(lcAccount) AND STORE = ALLTRIM(lcStore)
      IF !FOUND()
        DO CASE
          CASE EMPTY(lcAccount)
            =gfModalGen("INM44062B00000","Dialog")
          CASE EMPTY(lcStore)
            =gfModalGen("INM44043B00000","Dialog",lcAccount)
          CASE !EMPTY(lcStore)
            =gfModalGen("INM44043B00000","Dialog",lcStore)
        ENDCASE
        *adel 
        _CUROBJ = _CUROBJ
        RETURN
      ELSE
        IF AriaBrow('FOR ACCOUNT = ALLTRIM(lcAccount) AND;
           STORE = ALLTRIM(lcStore)','Pick Tickets',gnBrFSRow1, gnBrFSCol1,;
           gnBrFSRow2, gnBrFSCol2,.F.,.F.,"PikTkt.PikTkt","laBrowVal")
          lcPackNo =  laBrowVal[1]
        ELSE
          lcPackNo = SPACE(6)
        ENDIF             
      ENDIF
  ENDCASE
ENDIF

llShowInst = .F.
IF !EMPTY(lcPackNo)
  IF !SEEK(lcPackNo,'PACK_HDR')
    *IF gfDialog('?','Packing slips # '+lcPackNo+ ' not found.','\<Add;\<Reenter')=2
    IF gfModalGen("QRM44029B44004","Dialog",lcPackNo) = 2
      lcScrMode = 'S'
    ELSE
      llShowInst = .T.
      lcScrMode = 'A'
      lcOrderNo = PikTkt.Order
      lcAccount = PikTkt.Account
      lcStore   = PikTkt.Store
      =SEEK('O'+lcOrderNo,'OrdHdr')
      =SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
      lcDistCtr  = Customer.Dist_Ctr
      lcCustPo   = PikTkt.CustPo
      lcDept     = OrdHdr.Dept
      SET ORDER TO TAG CCODE_NO IN CODES
      =SEEK('N'+PADR('CDIVISION',10)+ORDHDR.CDIVISION+SPACE(30)+'DIVLNAME','CODES')
      lcDivLName = CODES.cRltd_Vlu 
      SET ORDER TO TAG CODES IN CODES
      lcWarCode  = PikTkt.cWareCode
      lcShipVia  = Customer.ShipVia
      =gfwCodePop(@laCodInfo, "SHIPVIA", "V,"+lcShipVia)
      llEdiFound = llEdiSys AND SEEK ('A'+lcAccount,'EdiAcPrt')
      llEdiExist = SEEK (lcAccount,'EdiPh')
      llPkChrDes = llEdiFound AND llEdiExist AND EdiAcPrt.lPkChrDes      
      llToStOrCn = llEdiFound AND llEdiExist AND EdiAcPrt.cMulUccLbl='Y'      
      lcPkChTxt  = IIF(llPkChrDes,'Pack Characteristic  :','')
      lcPkDsTxt  = IIF(llPkChrDes,'Pack Description     :','')
      lcPackChar = IIF(llPkChrDes,EdiAcPrt.cPckChCode,SPACE(5))
      lcPackDesc = IIF(llPkChrDes,EdiAcPrt.cPckDsCode,SPACE(7))
      lnToStorCn = 0    
      ** Labels Informations
      lcEvenCode = OrdHdr.Event_Cod
      lcVendNo   = Customer.cCusVend
           
      IF llMulWare .AND. SEEK(lcWarCode,'WareHous')
        lcWrName  = WareHous.cDesc 
        lcWrAddr1 = WareHous.cAddress1
        lcWrAddr2 = WareHous.cAddress2
        lcWrCity  = WareHous.cAddress3
        lcWrState = WareHous.cAddress4
        lcWrZip   = WareHous.cAddress5
      ENDIF  
      =SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
      lcDistCtr = Customer.Dist_Ctr 
      lcStName  = Customer.StName

      *ShipTo address
      =gfGetAdr('CUSTOMER','','','',1,'')
      lcStAddr1 = laAddress[1,2]
      lcStAddr2 = laAddress[2,2]
      lcStCity  = laAddress[3,2]
      lcStState = laAddress[4,2]
      lcStZip   = laAddress[5,2]

      =SEEK('S'+lcAccount+lcDistCtr,'Customer')
      *ShipTo address
      =gfGetAdr('CUSTOMER','','','',1,'')

      lcDCName  = Customer.StName
      lcDCAddr1 = laAddress[1,2]
      lcDCAddr2 = laAddress[2,2]
      lcDCCity  = laAddress[3,2]
      lcDCState = laAddress[4,2]
      lcDCZip   = laAddress[5,2]
      IF llEdiSys AND SEEK(EdiAcPrt.cPartCode+'856','EDIPD')
        SELECT BOL_HDR
        LOCATE FOR Account+Store+W_CODE+cDivision+ShipVia = ;
                   lcAccount+lcDistCtr+lcWarCode+OrdHdr.cDivision+lcShipVia AND Status <> 'C'
        IF !FOUND()
          lcBol = gfSequence('BOL_NO')
          INSERT INTO BOL_HDR ;
           (BOL_NO,ACCOUNT,STORE,STANCARTON,BOLDATE,W_CODE,cDivision,ShipVia) VALUES ;
           (lcBol,lcAccount,lcDistCtr,'N',gdSysDate,lcWarCode,OrdHdr.cDivision,lcShipVia)
        ELSE
          lcBol = BOL_HDR.BOL_NO
        ENDIF
        IF !SEEK(lcBol+lcOrderNo+lcPackNo,'BOL_LIN')
          INSERT INTO BOL_LIN (BOL_NO,ORDER,PACK_NO) VALUES (lcBol,lcOrderNo,lcPackNo)
        ENDIF
      ENDIF
    ENDIF
  ELSE
    lcScrMode = 'V'
    llShowInst = .T.
  ENDIF
ENDIF
IF llShowInst
  *--Show user the Shipping Instructions
  *B604501,1 SSH Fix bug of print store instruction.
  *=SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
  =SEEK('M'+lcAccount,'Customer')
  *B604501,1 SSH Fix bug of print store instruction.
  lcInst = CUSTOMER.mShpInst
  IF !EMPTY(lcInst)
    SELECT CUSTOMER
    =gfZoom("mShpInst")
  ENDIF  
  *B604501,1 SSH Fix bug of print store instruction.
  =SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
  *B604501,1 SSH Fix bug of print store instruction.
ENDIF
=lpShow()

*!*************************************************************
*! Name      : lfvPackInfo
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Get Pack informations
*!*************************************************************
*! Calls     : ,AriaBrow,lpShow
*!*************************************************************
*! Example            :  lfvPackInfo()
*!*************************************************************
FUNCTION lfvPackInfo
PRIVATE laAddress
DIMENSION laAddress[6,3]

llReBrow = .T.
SELECT PACK_HDR
=SEEK(lcPackNo)
=SEEK('O'+Order,'OrdHdr')
=SEEK(lcPackNo,'PikTKt')
lcOrderNo = Order
lcAccount = Account
lcStore   = Store
lcCustPo  = PikTkt.CustPo
lcDept    = OrdHdr.Dept

SET ORDER TO TAG CCODE_NO IN CODES
=SEEK('N'+PADR('CDIVISION',10)+ORDHDR.CDIVISION+SPACE(30)+'DIVLNAME','CODES')
lcDivLName = CODES.cRltd_Vlu 
SET ORDER TO TAG CODES IN CODES
lcNotes   = Note
lcSp_Inst1= Sp_Inst1
lcSp_Inst2= Sp_Inst2
lcWarCode = cWareCode
lcShipVia = ShipVia
=gfwCodePop(@laCodInfo, "SHIPVIA", "V,"+lcShipVia)
llEdiFound = llEdiSys AND SEEK ('A'+lcAccount,'EdiAcPrt')
llEdiExist = SEEK (lcAccount,'EdiPh')
llPkChrDes = llEdiFound AND llEdiExist AND EdiAcPrt.lPkChrDes      
llToStOrCn = llEdiFound AND llEdiExist AND EdiAcPrt.cMulUccLbl='Y'      
lcPkChTxt  = IIF(llPkChrDes,'Pack Characteristic:','')
lcPkDsTxt  = IIF(llPkChrDes,'Pack Description   :','')
lcPackChar = cpkChCode
lcPackDesc = cpkDsCode
lnToStorCn = IIF(cToStorCn='C',1,0)
lcBol      = Bill_Ladg
lnTotPiece = Tot_Pcs
lnTotWg    = Tot_Wght
lnTotCart  = Tot_Cart

** Labels Informations
=SEEK('O'+lcOrderNo,'ORDHDR')
lcEvenCode = OrdHdr.Event_Cod
lcVendNo  = Customer.cCusVend
IF llMulWare .AND. SEEK(lcWarCode,'WareHous')
  lcWrName  = WareHous.cDesc 
  lcWrAddr1 = WareHous.cAddress1
  lcWrAddr2 = WareHous.cAddress2
  lcWrCity  = ALLTRIM(WareHous.cAddress3)
  lcWrState = ALLTRIM(WareHous.cAddress4)
  lcWrZip   = ALLTRIM(WareHous.cAddress5)
ENDIF  
=SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore),'Customer')
lcDistCtr = Customer.Dist_Ctr 
lcStName  = Customer.StName

*ShipTo address
=gfGetAdr('CUSTOMER','','','',1,'')
lcStAddr1 = laAddress[1,2]
lcStAddr2 = laAddress[2,2]
lcStCity  = laAddress[3,2]
lcStState = laAddress[4,2]
lcStZip   = laAddress[5,2]

=SEEK('S'+lcAccount+lcDistCtr,'Customer')
lcDCName  = Customer.StName
*ShipTo address
=gfGetAdr('CUSTOMER','','','',1,'')
lcDCAddr1 = laAddress[1,2]
lcDCAddr2 = laAddress[2,2]
lcDCCity  = laAddress[3,2]
lcDCState = laAddress[4,2]
lcDCZip   = laAddress[5,2]

*!*************************************************************
*! Name      : lfDeact
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : READ Deactivate function of screen albel05
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDeact()
*!*************************************************************
FUNCTION lfDeact
IF WONTOP()=lcBrTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC     DO lpTab WITH 'albel053','pbClose',.T.
  ON KEY LABEL TAB     DO lpTab WITH 'albel056','lcUpc'
  ON KEY LABEL BACKTAB DO lpTab WITH 'albel053','pbClose'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTab
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Example   : DO lpTab WITH 'albel053','pbClose'
*!*************************************************************
PROCEDURE LPTAB
PARAMETERS lcWindName, lcObjName,llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfReadAct
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : READ Activate function of albel05
*!*************************************************************
*! Calls     : gfClearKey.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
IF llFirst
  llFirst = .F.
  BROWSE FIELDS ;
        cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,;
        CartonNo :H= 'Crtn#' :R,;
        Style      = SUBSTR(Style,1,lnMajLen):R,;
        Color      = SUBSTR(Style,lnClrPo,lnColorLen):R,;
        cSize    :H= 'Size'  :R,;
        TOtQty   :H = 'Qty.' :R;
        WINDOW    albel055;
        IN WINDOW albel054;
        NOMENU            ;         
        NOAPPEND          ;
        NODELETE          ;         
        NOWAIT            ;
        SAVE              ;
        NOCLEAR           ;
 	    WHEN lfShowData() ;
        TITLE lcBrTit
  llReBrow = .F.
ENDIF         

=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTit)

*!*************************************************************
*! Name      : lfPrntLab
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to Print the packing slip lable
*!*************************************************************
*! Calls     : lfPrnUcc128,lfCheckNo
*!*************************************************************
*! Example            :  lfPrntLab()
*!*************************************************************
FUNCTION lfPrntLab
PARAMETERS lnCarton
PRIVATE lcStyle,lcColor,lcSize


SELECT (tmpPkLin)
lnRecNo = RECNO()
=SEEK(STR(lnCarton,4))
lcStyle  = SUBSTR(Style,1,lnMajLen)
lcColor  = SUBSTR(Style,lnClrPo,lnColorLen)
lcUpdSty = Style
lcSize   = cSize
SCAN REST WHILE STR(CartonNo,4) = STR(lnCarton,4)
  lcStyle = IIF(lcStyle<>SUBSTR(Style,1,lnMajLen),SPACE(12),lcStyle)
  lcColor = IIF(EMPTY(lcStyle),SPACE(6),IIF(lcColor<>SUBSTR(Style,lnClrPo,lnColorLen),'MIXED',lcColor))
  lcSize  = IIF(EMPTY(lcStyle),SPACE(5),IIF(lcSize <>cSize,'MIXED',lcSize))
ENDSCAN
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo IN (tmpPkLin)
ENDIF
=SEEK ('A'+lcAccount,'EdiAcPrt') AND SEEK(EdiAcPrt.cPartCode,'EdiPH')
lcUccVer = IIF(lnToStorCn=0,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
lcUccVer = IIF(EMPTY(lcUccVer),'XXX',lcUccVer)
IF llEdiSys
  SELECT (tmpAsnShip)
  lcUCC9    = RIGHT(PADL(ALLTRIM(lcPackNo),6,'0'),5)+PADL(lnCarton,4,'0')
  lcCheckNo = lfCheckNo('000'+lcManufId+lcUcc9)
  IF !SEEK(lcPackNo+STR(lnCarton,6))
    APPEND BLANK
  ENDIF
  REPLACE Cart_No   WITH lnCarton  ,;
          VND_NAME  WITH lcWrName  ,;
          VND_ADDR1 WITH lcWrAddr1 ,;
          VND_ADDR2 WITH lcWrAddr2 ,;
          VND_CITY  WITH lcWrCity  ,;
          VND_STATE WITH lcWrState ,;
          VND_ZIP   WITH lcWrZip   ,;
          STORE     WITH IIF(EMPTY(lcDistCtr),lcStore  ,lcDistCtr) ,;
          SHP_NAME  WITH IIF(EMPTY(lcDistCtr),lcStName ,lcDCName)  ,;
          SHP_ADDR1 WITH IIF(EMPTY(lcDistCtr),lcStAddr1,lcDCAddr1) ,;
          SHP_ADDR2 WITH IIF(EMPTY(lcDistCtr),lcStAddr2,lcDCAddr2) ,;
          SHP_CITY  WITH IIF(EMPTY(lcDistCtr),lcStCity ,lcDCCity)  ,;
          SHP_STATE WITH IIF(EMPTY(lcDistCtr),lcStState,lcDCState) ,;
          SHP_ZIP   WITH IIF(EMPTY(lcDistCtr),lcStZip  ,lcDCZip)   ,;
          CUSTPO    WITH lcCustPo  ,;
          DEPT      WITH lcDept    ,;
          MANUF_ID  WITH lcManufId ,;
          UCC9      WITH lcUCC9    ,;
          UCC_CHECK WITH lcCheckNo ,;
          ASN_VER   WITH lcUccVer  ,;
          EVENT_COD WITH lcEvenCode
  REPLACE PACK_NO   WITH lcPackNo  ,;
          ShipVia   WITH lcShipVia ,;
          Carrier   WITH ALLTRIM(gfCodDes(lcShipVia,'SHIPVIA   ')),;
          Int_Vend  WITH lcVendNo  ,;
          Bol_No    WITH lcBol     ,; 
          cStStore  WITH lcStore   ,;
          cStName   WITH lcStName  ,;
          cStAddr1  WITH lcStAddr1 ,;
          cStAddr2  WITH lcStAddr2 ,;
          cStCity   WITH lcStCity  ,;
          cStState  WITH lcStState ,;
          cStZip    WITH lcStZip   ,;
          TotQty    WITH laCartons[lnCarton,1] ,;
          Cartons   WITH lnTotCart ,;
          Style     WITH lcUpdSty ,;
          cClrDesc  WITH IIF(lcColor='MIXED',lcColor,gfCodDes(lcColor,'COLOR     ')) ,;
          cSizeDesc WITH lcSize
  =lfPrnUcc128(lcUccVer)
ENDIF
SELECT (tmpPkLin)

*!*************************************************************
*! Name      : lfCheckNo
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Algorithm to Compute the modulus-10 Check digit for the 
*!             UCC 128 code
*!*************************************************************
*! Parameters: lcUccNo : UCC number without check digit
*!*************************************************************
*! Returns   : UCC Check digit
*!*************************************************************
*! Example   :  =lfCheckNo('1234567890123456789')
*!*************************************************************
FUNCTION lfCheckNo
PARAMETER lcUccNo
PRIVATE lnChkDigit,lnSumOdd,lnSumEven,lnCount

STORE 0 TO lnSumOdd,lnSumEven,lnChkDigit
FOR lnCount = 1 TO 9
  lnSumOdd  = lnSumOdd + VAL(SUBSTR(lcUccNo,lnCount*2-1,1))
  lnSumEven = lnSumEven+ VAL(SUBSTR(lcUccNo,lnCount*2,1))
ENDFOR
lnSumOdd   = lnSumOdd + VAL(SUBSTR(lcUccNo,19,1))
lnChkDigit = MOD(lnSumOdd*3+lnSumEven,10)
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

*!*************************************************************
*! Name      : lfPrnUcc128
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to Print the packing slip lable
*!*************************************************************
*! Example            :  lfPrnUcc128()
*!*************************************************************
FUNCTION lfPrnUcc128
PARAMETERS lcVersion
PRIVATE lnHandle, lcOutFile, lcString, lcData

WAIT 'Generating Shipping Label...Please stand by....' WINDOW NOWAIT
lcString  = SPACE(40)

SELECT SYCASNLB
*IF !SEEK(lcVersion+'H') .AND. gfDialog('I','Shipping Label Header does not exist. Cannot proceed.')=1
IF !SEEK(lcVersion+'H') .AND. gfModalGen('INM44068B00000','DIALOG' )=1
  RETURN
ENDIF
lcOutFile = gcWorkDir+lcAsnLabel+".TXT"
lnHandle  = FCREATE(lcOutFile,0)
=FSEEK(lnHandle,0,2)

SCAN WHILE 	cVer+cEdiType = lcVersion+'H'
  STORE DATA TO lcData
  lcString = &lcData 
  =FPUTS(lnHandle,lcString)
ENDSCAN	
lcString = SPACE(3)
=FPUTS(lnHandle,lcString)
SELECT (tmpAsnShip)
SCATTER MEMVAR
* Pickup Order Division long name, Department name and style description
m.DivLName = lcDivLName 
=SEEK(lcAccount+lcDept,'CUSTDEPT')
m.DeptDesc = CustDept.cDeptDesc
=SEEK(ALLTRIM(lcUpdSty),'Style')
m.StyDesc = Style.Desc

TMP_ADDR = ALLTRIM(VND_CITY) + ",  " +ALLTRIM(VND_STATE) + "   " + ALLTRIM(VND_ZIP)

SELECT SYCASNLB
=SEEK(lcVersion+'L')
SCAN WHILE cVer+cEdiType= lcVersion+'L'
  STORE DATA TO lcData
  lcString = &lcData 
  =FPUTS(lnHandle,lcString)
ENDSCAN
lcString = SPACE(3)
=FPUTS(lnHandle,lcString)
=FCLOSE(lnHandle)
*-- Remark the following sentence to show it if we will print (Ok button is pressed).
*! MODE COM1:24,N,8,1,P >>null
*! MODE LPT1:=COM1: >>null
*! PRINT /D:LPT1 /S:1 >>null
*****! TYPE &lcOutFile > COM2
IF !llFoundP
  *--If it is the first time to show the Ports screen.
  DO lpPortScr
  IF !llCancel
    *--If user clicked OK.
    SAVE ALL LIKE lcSndPort* TO &gcDataDir.ALASI90.MEM
    llFoundP = .T.
    WAIT 'Sending Shipping Labels to Printer...Please stand by....' WINDOW NOWAIT
    lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
    ! &lcCommand 
  ENDIF  
ELSE
  WAIT 'Sending Shipping Labels to Printer...Please stand by....' WINDOW NOWAIT
  lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
  ! &lcCommand 
ENDIF  
WAIT CLEAR
RETURN

*!*************************************************************
*! Name      : lfUpdate
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Check if changes occured
*!*************************************************************
*! Example            :  lfUpdate()
*!*************************************************************
FUNCTION lfUpdate
IF UPDATE()
  llUpdate  = .T.
ENDIF  

*!*************************************************************
*! Name      : lfvSave
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : Function to save the pack slip
*!*************************************************************
*! Example            :  lfvSave()
*!*************************************************************
FUNCTION lfvSave
PRIVATE lnLineNo

IF lnTotCart <> lnLastCart .AND. gfModalGen("QRM44070B44010","Dialog")= 2
  RETURN
ENDIF
llDelPack = .F.
lnChoise = 0
SET ORDER TO TAG ORDLINE   IN ORDLINE
SET ORDER TO TAG ORDLINENO IN (TmpPkLin)
IF SEEK('O'+lcOrderNo,'ORDLINE')
  SELECT ORDLINE
  LOCATE REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcOrderNo ;
         FOR PICKED .AND. PIKTKT=lcPackNo .AND. !SEEK(STR(LineNo,6),TmpPkLin)
  IF FOUND()
    *-- Some of the lines in the pick ticket are not packed. Do you want to release them?
    lnChoise= gfModalGen("QRM44071B44006","Dialog")
    DO CASE
      CASE lnChoise = 2
        *:B604467,4 SSH 05/23/2001 Restore the old index.
        SET ORDER TO TAG ORDLINST   IN ORDLINE
        *:B604467,4 SSH [END]
        RETURN
      CASE lnChoise = 1
        SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcOrderNo ;
             FOR PICKED .AND. PIKTKT=lcPackNo .AND. !SEEK(STR(LineNo,6),TmpPkLin)
          IF SEEK(Style,'Style')
            SELECT STYLE
            =RLOCK()
            REPLACE Alo1   WITH Alo1   - OrdLine.Pik1 ,;
                    Alo2   WITH Alo2   - OrdLine.Pik2 ,;
                    Alo3   WITH Alo3   - OrdLine.Pik3 ,;
                    Alo4   WITH Alo4   - OrdLine.Pik4 ,;
                    Alo5   WITH Alo5   - OrdLine.Pik5 ,;
                    Alo6   WITH Alo6   - OrdLine.Pik6 ,;
                    Alo7   WITH Alo7   - OrdLine.Pik7 ,;
                    Alo8   WITH Alo8   - OrdLine.Pik8 ,;
                    TotAlo WITH TotAlo - OrdLine.TotPik
            UNLOCK
          ENDIF
          *--  updating the stydye file.
          IF llMulWare .AND.;
             SEEK (OrdLine.Style+lcWarCode+SPACE(10),'StyDye')
            SELECT StyDye
            =RLOCK()
            REPLACE Alo1   WITH Alo1   - OrdLine.Pik1 ,;
                    Alo2   WITH Alo2   - OrdLine.Pik2 ,;
                    Alo3   WITH Alo3   - OrdLine.Pik3 ,;
                    Alo4   WITH Alo4   - OrdLine.Pik4 ,;
                    Alo5   WITH Alo5   - OrdLine.Pik5 ,;
                    Alo6   WITH Alo6   - OrdLine.Pik6 ,;
                    Alo7   WITH Alo7   - OrdLine.Pik7 ,;
                    Alo8   WITH Alo8   - OrdLine.Pik8 ,;
                    TotAlo WITH TotAlo - OrdLine.TotPik
            UNLOCK
          ENDIF  
          SELECT OrdLine
          =RLOCK()
          REPLACE PIK1   WITH 0 ,;
                  PIK2   WITH 0 ,;
                  PIK3   WITH 0 ,;
                  PIK4   WITH 0 ,;
                  PIK5   WITH 0 ,;
                  PIK6   WITH 0 ,;
                  PIK7   WITH 0 ,;
                  PIK8   WITH 0 ,;
                  TOTPIK WITH 0 ,;
                  Picked WITH .F.,;
                  PikTkt WITH SPACE(6)
          UNLOCK
          *:C200205,1 SSH [Start] Update file ALPAKINF When saving the packing list.
          *--- Start call functions to update the custom DBF "ALPAKINF"
          *--- Seek to refresh the relation between pikttkt ,OrdHdr and Customer.
          *=SEEK(lcPackNo,"PikTkt")
          *-=lfUPDPACK()
          *:C200205,1 SSH [End..] Update file ALPAKINF When saving the packing list.
        ENDSCAN
        SELECT ORDLINE
        =SEEK('O'+lcOrderNo,'ORDLINE')
        LOCATE REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcOrderNo;
               FOR PICKED .AND. PIKTKT=lcPackNo
       IF !FOUND() .AND. SEEK(lcPackNo,'PIKTKT')
         SELECT PIKTKT
         DELETE
         llDelPack = .T.
       ENDIF
    ENDCASE
  ELSE
    *--All lines are packed with whatever qtys. Check the final qty.
    IF !lfChkQty()
      *:B604467,4 SSH 05/23/2001 Restore the old index.
      SET ORDER TO TAG ORDLINST   IN ORDLINE
      *:B604467,4 SSH 05/23/2001 Restore the old index.
      RETURN
    ENDIF
  ENDIF
ENDIF
*:C200205,1 SSH Update file ALPAKINF When saving the packing list.
=lfRestQty()
*:C200205,1 SSH [End]
lnTotCart = lnLastCart
lnLineNo = IIF(SEEK(lcPackNo,'PACK_HDR'),PACK_HDR.nLastLNo,0)
SELECT (TmpPkLin)
SET DELETE OFF
GO TOP
DO WHILE !EOF()
  lcOrdLNo = STR(OrdLineNo,6)
  =SEEK(Style,'Style')
  IF llMulWare
    =SEEK(Style+lcWarCode,'StyDye')
  ENDIF
  STORE 0 TO lnPack1,lnPack2,lnPack3,lnPack4,lnPack5,lnPack6,lnPack7,lnPack8
  SCAN REST WHILE STR(OrdLineNo,6) = lcOrdLNo
  
    lcStySize = Size
    lnPack&lcStySize = lnPack&lcStySize + IIF(DELETED(),0,TotQty)
    DO CASE
      CASE !DELETED(TmpPkLin) .AND. OldCrtNo = 0
        SELECT PACK_LIN
        =SEEK(lcPackNo+STR(&TmpPkLin..CartonNo,4)+STR(&TmpPkLin..CartonNo,4)+;
        &TmpPkLin..Style)
        LOCATE REST WHILE pack_no+STR(from_crt,4)+STR(to_crt,4)+style =;
        lcPackNo+STR(&TmpPkLin..CartonNo,4)+STR(&TmpPkLin..CartonNo,4)+;
        &TmpPkLin..Style FOR nORDLINENO=&TmpPkLin..ORDLINENO
        IF !FOUND()
          lnLineNo = lnLineNo + 1

          *B606639,1 ABD - Update the number of carton with Carton No.  [Begin]
          *INSERT INTO 'PACK_LIN' ;
          *(PACK_NO,Line_No,Style,No_cart,From_Crt,To_Crt,nOrdLineNo) VALUES ;
          *(lcPackNo,lnLineNo,&TmpPkLin..Style,1,&TmpPkLin..CartonNo,;
          * &TmpPkLin..CartonNo,&TmpPkLin..OrdLineNo)
          INSERT INTO 'PACK_LIN' ;
          (PACK_NO,Line_No,Style,No_cart,From_Crt,To_Crt,nOrdLineNo) VALUES ;
          (lcPackNo,lnLineNo,&TmpPkLin..Style,&TmpPkLin..CartonNo,&TmpPkLin..CartonNo,;
           &TmpPkLin..CartonNo,&TmpPkLin..OrdLineNo)
           *B606639,1 ABD - [End]
           
        ENDIF
        REPLACE Qty&lcStySize WITH Qty&lcStySize + &TmpPkLin..TotQty ,;
                TotQty        WITH TotQty        + &TmpPkLin..TotQty ,;
                Weight        WITH TotQty*Style.nStyWeight
              
      CASE !DELETED(TmpPkLin) .AND. OldCrtNo <> 0 
        SELECT PACK_LIN
        =SEEK(lcPackNo+STR(&TmpPkLin..OldCrtNo,4)+STR(&TmpPkLin..OldCrtNo,4)+;
        &TmpPkLin..Style)        
        LOCATE REST WHILE pack_no+STR(from_crt,4)+STR(to_crt,4)+style = ;
        lcPackNo+STR(&TmpPkLin..OldCrtNo,4)+STR(&TmpPkLin..OldCrtNo,4)+;
        &TmpPkLin..Style FOR nORDLINENO=&TmpPkLin..ORDLINENO
        IF FOUND()
          REPLACE From_Crt      WITH &TmpPkLin..CartonNo ,;
                  To_Crt        WITH &TmpPkLin..CartonNo ,;
                  Qty&lcStySize WITH &TmpPkLin..TotQty   ,;
                  TotQty        WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
                  Weight        WITH TotQty*Style.nStyWeight

          *B606639,1 ABD - Update the number of carton with Carton No.  [Begin]
          REPLACE No_Cart       WITH &TmpPkLin..CartonNo                  
          *B606639,1 ABD - [End]
          
        ENDIF
      CASE DELETED(TmpPkLin) .AND. OldCrtNo <> 0
        SELECT PACK_LIN
        =SEEK(lcPackNo+STR(&TmpPkLin..OldCrtNo,4)+STR(&TmpPkLin..OldCrtNo,4)+;
        &TmpPkLin..Style)
        LOCATE REST WHILE pack_no+STR(from_crt,4)+STR(to_crt,4)+style =;
        lcPackNo+STR(&TmpPkLin..OldCrtNo,4)+STR(&TmpPkLin..OldCrtNo,4)+;
        &TmpPkLin..Style FOR nORDLINENO=&TmpPkLin..ORDLINENO
        IF FOUND()
          REPLACE Qty&lcStySize WITH 0 ,;
                  TotQty        WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
                  Weight        WITH TotQty*Style.nStyWeight
        ENDIF
        IF TotQty = 0
          BLANK
          DELETE
        ENDIF
    ENDCASE
  ENDSCAN
  =SEEK('O'+lcOrderNo+lcOrdLNo,'OrdLine')
  *--Don't release unless the user packed the line (entirely or partially) or chose to Release.
  IF lnPack1+lnPack2+lnPack3+lnPack4+lnPack5+lnPack6+lnPack7+lnPack8 > 0 OR lnChoise = 1
    SELECT Style
    =RLOCK()
    REPLACE ALO1   WITH ALO1 - OrdLine.Pik1 + lnPack1 ,;
            ALO2   WITH ALO2 - OrdLine.Pik2 + lnPack2 ,;
            ALO3   WITH ALO3 - OrdLine.Pik3 + lnPack3 ,;
            ALO4   WITH ALO4 - OrdLine.Pik4 + lnPack4 ,;
            ALO5   WITH ALO5 - OrdLine.Pik5 + lnPack5 ,;
            ALO6   WITH ALO6 - OrdLine.Pik6 + lnPack6 ,;
            ALO7   WITH ALO7 - OrdLine.Pik7 + lnPack7 ,;
            ALO8   WITH ALO8 - OrdLine.Pik8 + lnPack8 ,;
            TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
    UNLOCK
    IF llMulWare
      SELECT StyDye
      =RLOCK()
      REPLACE ALO1   WITH ALO1 - OrdLine.Pik1 + lnPack1 ,;
              ALO2   WITH ALO2 - OrdLine.Pik2 + lnPack2 ,;
              ALO3   WITH ALO3 - OrdLine.Pik3 + lnPack3 ,;
              ALO4   WITH ALO4 - OrdLine.Pik4 + lnPack4 ,;
              ALO5   WITH ALO5 - OrdLine.Pik5 + lnPack5 ,;
              ALO6   WITH ALO6 - OrdLine.Pik6 + lnPack6 ,;
              ALO7   WITH ALO7 - OrdLine.Pik7 + lnPack7 ,;
              ALO8   WITH ALO8 - OrdLine.Pik8 + lnPack8 ,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
    ENDIF
    SELECT OrdLine
    =RLOCK()
    REPLACE Pik1   WITH lnPack1 ,;
            Pik2   WITH lnPack2 ,;      
            Pik3   WITH lnPack3 ,;      
            Pik4   WITH lnPack4 ,;      
            Pik5   WITH lnPack5 ,;      
            Pik6   WITH lnPack6 ,;      
            Pik7   WITH lnPack7 ,;      
            Pik8   WITH lnPack8 ,;      
            TotPik WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8 ,;
            Picked WITH TotPik>0 ,;
            PikTkt WITH IIF(TotPik>0 ,PikTKt,'')
    UNLOCK
    *:C200205,1 SSH [Start] Update file ALPAKINF When saving the packing list.
    *--- Start call functions to update the custom DBF "ALPAKINF"
    *--- Seek to refresh the relation between pikttkt ,OrdHdr and Customer.
    =SEEK(lcPackNo,"PikTkt")
    =lfUPDPACK()
    *:C200205,1 SSH [End..] Update file ALPAKINF When saving the packing list.
  ENDIF
  SELECT (TmpPkLin)
ENDDO  
SET DELETE ON
IF llEdiSys AND SEEK ('A'+lcAccount,'EdiAcPrt') .AND. SEEK(EdiAcPrt.cPartCode+'856','EDIPD')
  SELECT BOL_HDR
  IF !SEEK(lcPackNo,'Pack_Hdr')
    REPLACE Tot_Wght WITH Tot_Wght + lnTotWg   ,;
            Tot_Cart WITH Tot_Cart + lnTotCart ,;
            Tot_Pcs  WITH Tot_Pcs  + lnTotPiece
  ELSE
    REPLACE Tot_Wght WITH Tot_Wght - Pack_Hdr.Tot_Wght + lnTotWg   ,;
            Tot_Cart WITH Tot_Cart - Pack_Hdr.Tot_Cart + lnTotCart ,;
            Tot_Pcs  WITH Tot_Pcs  - Pack_Hdr.Tot_Pcs  + lnTotPiece
  ENDIF
ENDIF  
SELECT Pack_Hdr
IF llDelPack
  IF SEEK(lcPackNo)
    DELETE
  ENDIF
  IF llEdiSys AND SEEK(lcBol+lcOrderNo+lcPackNo,'BOL_LIN')
    SELECT BOL_LIN
    DELETE
  ENDIF  
ELSE
  IF !SEEK(lcPackNo)
    APPEND BLANK
  ENDIF
  REPLACE Pack_No   WITH lcPackNo   ,;
          Order     WITH lcOrderNo  ,;
          Account   WITH lcAccount  ,;
          cWareCode WITH lcWarCode  ,;
          Store     WITH lcStore    ,;
          Bill_ladg WITH lcBol      ,;
          Tot_Wght  WITH lnTotWg    ,;
          Tot_Pcs   WITH lnTotPiece ,;
          Tot_Cart  WITH lnTotCart  ,;
          Note      WITH lcNotes    ,;
          ShipVia   WITH lcShipVia  ,;
          cPkChCode WITH lcPackChar ,;
          cPkDsCode WITH lcPackDesc ,;
          cToStorCn WITH IIF(lnToStorCn=0,'S','C') ,;
          Piktkt    WITH lcPackNo   ,;
          Sp_Inst1  WITH lcSp_Inst1 ,;
          Sp_Inst2  WITH lcSp_Inst2 ,;
          nLastLNo  WITH lnLineNo   ,;
          nLastCart WITH lnLastCart
ENDIF
IF llEdiSys
  SELECT (tmpAsnShip)
  SCAN
    SCATTER MEMVAR
    SELECT ASN_SHIP
    IF !SEEK(&tmpAsnShip..Bol_No+&tmpAsnShip..PACK_NO+STR(&tmpAsnShip..Cart_No,6))
      APPEND BLANK
    ENDIF
    GATHER MEMVAR
  ENDSCAN
  ZAP
ENDIF
SET ORDER TO TAG ORDLINST   IN ORDLINE
SET ORDER TO TAG (TmpPkLin) IN (TmpPkLin)
lcScrMode = 'S'
=lpShow()
*B605734,1 ASH 03/27/2002 (Begin) Save the relation to restore it after printing.
lcAlias=ALIAS()
SELECT PIKTKT
lcPikRel = SET('RELATION')
SELECT STYLE
lcStyRel = SET('RELATION')
SELECT (lcAlias)
*B605734,1 ASH 03/27/2002 (End)
*:C102301,1 AAN Show a message to print the packing list report or not[Begin].
IF gfModalGen('TRM44106B42002','DIALOG') = 1
  =lfvGrid()
ENDIF
*:C102301,1 AAN Show a message to print the packing list report or not[End].
*B605734,1 ASH 03/27/2002 (Begin) Restore the relation after printing.
SELECT PIKTKT
SET RELATION TO &lcPikRel 
SELECT STYLE
SET RELATION TO &lcStyRel 
*B605734,1 ASH 03/27/2002 (End)
*!*************************************************************
*! Name      : lfNonMaj
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 25/02/2001
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnNonMajPo = 0
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      llStopConc = .T.
    ENDIF
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''



*!*************************************************************
*! Name      : lpPortScr
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 01/20/2000
*! Purpose   : Handle the printing  labels screen setup
*!*************************************************************
*! Example   : = lpPortScr
*!*************************************************************
PROCEDURE lpPortScr
PRIVATE lcOldPort
lcOldPort = lcSndPort
llCancel  = .F.
DO (gcScrDir + gcWinAppl + '\ALPORTSC.SPX')
IF llCancel
  lcSndPort  = lcOldPort
ENDIF
*-- end of lpPortScr.

*!*************************************************************
*! Name      : lpChkQty
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 01/20/2000
*! Purpose   : Check the final Paclink list qty against the 
*!           : original PK qty
*!*************************************************************
*! Example   : = lpPortScr
*!*************************************************************
PROCEDURE lfChkQty

PRIVATE lnAlias
lnAlias = SELECT()

IF SEEK('O'+lcOrderNo,'ORDLINE')
  SELECT ORDLINE
  SUM TotQty TO lnPikQty WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcOrderNo ;
                         FOR PICKED .AND. PIKTKT=lcPackNo
  SELECT (TmpPkLin)
  SUM TotQty TO lnPacked
  lcMes = 'The quantity packed is less than the pick ticket quantity.'
  IF lnPacked < lnPikQty AND gfModalGen('QRM00000B44010',.F.,.F.,.F.,lcMes) = 2
      RETURN .F.    
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvGrid
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2001
*! Purpose   : Run th eoption grid of the packing list report
*! Refer to  : C#102301
*!*************************************************************
*! Example   : = lfvGrid
*!*************************************************************
FUNCTION lfvGrid
=gfStatic()                   && Save inviroment before terminate
PRIVATE lcRunRep , lcRepsDir
lcRunRep  = 'ALPKLS'
lcRepsDir = gcRepHome+gcAct_Appl+'REPORT'  && TO DO ALREPORT WITH 'ALPKLS'
DO &lcRepsDir WITH lcRunRep

=gfSetup()
*-- End of lfvGrid.


*:C200205,1 SSH [Start] Update file ALPAKINF When saving the packing list.
*:C200205,1 SSH         Get the following functions from bel02 main program
*:C200205,1 SSH         to this prg.
*!*****************************************************************
*! Name      : lfUPDPACK
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Update ALPAKINF file
*!*************************************************************
FUNCTION lfUPDPACK

PRIVATE lnAlias
lnAlias = SELECT(0)
=gfOpenFile(gcDataDir+'ALPAKINF',gcDataDir+'ALPAKINF','SH')
*:C200205,1 SSH [Start]Commented out since that there is relation.
*=IIF(EMPTY(laData[5]),SEEK('M'+laData[4],'Customer'),SEEK('S'+laData[4]+laData[5],'Customer'))
*--- No Need for SEEK since that there is relation.
*lcShipVia = laData[7]
lcShipVia  = IIF( ALLTRIM(OrdHdr.ShipVia) = '*' ,Customer.ShipVia,OrdHdr.ShipVia)
*:C200205,1 SSH [End..].
SELECT ALPAKINF
IF !SEEK(lcPackNo)
  APPEND BLANK
ELSE
  REPLACE CTOTAL WITH 0
ENDIF
*++NAD
lcAdd   = OrdHdr.CADDRESS3
lcCity  = SUBSTR(lcAdd,1,AT(',',lcAdd)-1)
lcMid   = AT(',',lcAdd,2) - AT(',',lcAdd) -1
lcState = SUBSTR(lcAdd,AT(',',lcAdd)+1,lcMid)
lcZip   = SUBSTR(lcAdd,AT(',',lcAdd,2)+1)
*++NAD
REPLACE PIKTKT     WITH lcPackNo,;
        ACCOUNT    WITH lcAccount,;
        STORE      WITH lcStore,;
        STNAME     WITH IIF(OrdHdr.Alt_ShpTo,OrdHdr.STNAME,CUSTOMER.STNAME)       ,;
        CADDRESS1  WITH IIF(OrdHdr.Alt_ShpTo,OrdHdr.CADDRESS1,CUSTOMER.CADDRESS1) ,;
        CADDRESS2  WITH IIF(OrdHdr.Alt_ShpTo,OrdHdr.CADDRESS2,CUSTOMER.CADDRESS2) ,;
        CADDRESS3  WITH IIF(OrdHdr.Alt_ShpTo,lcCity,CUSTOMER.CADDRESS3) ,;
        CADDRESS4  WITH IIF(OrdHdr.Alt_ShpTo,lcState,CUSTOMER.CADDRESS4) ,;
        CADDRESS5  WITH IIF(OrdHdr.Alt_ShpTo,lcZip,CUSTOMER.CADDRESS5) ,;
        PHONE1     WITH CUSTOMER.PHONE1    ,;
        CSHIPVIA   WITH lcShipVia          ,;
        CUSTPO     WITH IIF(SEEK('O'+lcOrderNo,'OrdHdr'),ORDHDR.CUSTPO,''),;
        ALT_SHPTO  WITH ORDHDR.ALT_SHPTO
=gfAdd_Info()     
=lfUPDTOT()
SELECT (lnAlias)


*!*****************************************************************
*! Name      : lfUPDTOT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Update Total shipment 
*!*************************************************************
FUNCTION lfUPDTOT
PARAMETER lcSigne1
*IF USED('ALPAKINF') AND SEEK(lcPackNo,'ALPAKINF')
REPLACE ALPAKINF.CTOTAL WITH ALPAKINF.CTOTAL + ORDLINE.TOTPIK*ORDLINE.PRICE
*ENDIF
*:C200205,1 SSH [End..] Update file ALPAKINF When saving the packing list.


FUNCTION lfRestQty
PRIVATE lnAlias
lnAlias = SELECT(0)
=gfOpenFile(gcDataDir+'ALPAKINF',gcDataDir+'ALPAKINF','SH')
SELECT ALPAKINF
IF SEEK(lcPackNo)
  REPLACE CTOTAL WITH 0
ENDIF
SELECT (lnAlias)