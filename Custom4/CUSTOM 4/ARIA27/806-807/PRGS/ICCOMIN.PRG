*:********************************+****************************
*: Program file   : ICCOMIN.PRG    Converted from A26 (ROB3800)
*: Program desc.  : Commercial Invoice (Add\Modify\Inquire)
*: For screen     : ICCOMIN
*:                : ICCOMINP
*:                : ICCOMIND
*:         System : Aria Apparel System
*:         Module : IC
*:      Developer : Ashraf Sherif Mohammad (ASH)
*:*************************************************************
*: Calls : 
*:         Procedures : None
*:         Functions  : lfvInvNo
*:                      lfvNew
*:                      lfvCons
*:                      lfvSave
*:                      lfvFirst
*:                      lfvLast
*:                      lfvNext
*:                      lfvPrev
*:                      lfvEdit
*:                      lfvDelete
*:                      lfvSelect
*:                      lfvBrowes
*:                      lfvClose
*:                      lfvZoom
*:                      lfvNotePad
*:                      lfvBrowInv
*:                      lfvStyPO
*:                      lfvPOnNo
*:                      lfvSlctdPO
*:                      lfvPOOk
*:                      lfvDetails
*:                      lfvRemv
*:                      lfvInsert
*:                      lfvItem
*:                      lfvQty
*:                      lfvPrice
*:                      lfvUnits
*:                      lfvAmount
*:                      lfvRef
*:                      lfvDesc
*:                      lfvNotes
*:                      lfvCatgTyp
*:                      lfvWCatTyp
*:                      lfvPO_No
*:                      lfvStyle
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************

EXTERNAL ARRAY laData 
DECLARE laCompAdd[6,1]
STORE SPACE(0) TO laCompAdd
laScrMode[1] = .T.
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
STORE .T. TO llAlowNew
IF !gfSetup()
  RETURN
ENDIF

Dimension laCatgTyp[4,2]
laCatgTyp[1,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE2')))
laCatgTyp[2,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE3')))
laCatgTyp[3,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE4')))
laCatgTyp[4,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE5')))
laCatgTyp[1,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL2')))
laCatgTyp[2,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL3')))
laCatgTyp[3,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL4')))
laCatgTyp[4,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL5')))

*=gfGetMemvar(@laCatgTyp,gcAct_Comp)

lcCatgDes = laCatgTyp[1,1]
lcCatgTyp = laCatgTyp[1,2]
lnCatgTyp = 1

=gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
SEEK gcAct_Comp

lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format
* Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)


lcOldCons  = SPACE(08)              && Old Consignee
lcConName  = SPACE(20)              && Consignee Name
lnSeqNo    =  0                     && Invoice sequentianl number 
llMousClic = .F.                    && Boolean for mouse click
glUpdated  = .F.                    && Boolean for field updated
llBrowse   = .F.                    && Controling the browse inv.button.
llDflage   = .T.                    && Controlling enabling/disabling Details button
llInsert   = .F.                    && Controlling insert mode on Details button
lnBrRecNo  = 0                      && Browes pointer
lcPattern  = SPACE(10)              && Pattern
lcOldItm   = SPACE(20)              && Old Item
lcOldVal   = SPACE(1)               && Old Value
lcBrowcTtl = 'Components'           && Browes Title
ldInvDate  = DATE()

SELECT CODES
SET ORDER TO TAG CCODE_NO

DIMENSION laData[23]



IF !WEXIST(gcBaseWind)

  lcTmpSty  = gfTempName()             && Temprary name for mover file
  lcTmpSty1 = gfTempName()             && Temprary name for po_no tag 
  lcTmpSlc  = gfTempName()             && Temprary file for selected po
  lcTmpSlc1 = gfTempName()             && Temprary name for po_no tag 
  lcTmpDtl  = gfTempName()             && Temprary name for details file
  lcTmpDtl2 = gfTempName()             && Temprary name for details file 2
  lcTmpDtlx = gfTempName()             && Temprary name for po_no+style+type+item tag 
  lcTmpBrow = gfTempName()             && Temprary file name for browse 

  DIMENSION laFileStru[4,4]
  laFileStru[1,1] = 'CPO_NO'
  laFileStru[1,2] = 'C'
  laFileStru[1,3] = 6
  laFileStru[1,4] = 0
  laFileStru[2,1] = 'STYLE'
  laFileStru[2,2] = 'C'
  laFileStru[2,3] = 19
  laFileStru[2,4] = 0
  laFileStru[3,1] = 'QTY'
  laFileStru[3,2] = 'C'
  laFileStru[3,3] = 7
  laFileStru[3,4] = 0
  laFileStru[4,1] = 'LFLAGE'
  laFileStru[4,2] = 'L'
  laFileStru[4,3] = 1
  laFileStru[4,4] = 0
  CREATE TABLE (gcDataDir+lcTmpSty) FROM ARRAY laFileStru
  CREATE TABLE (gcDataDir+lcTmpSlc) FROM ARRAY laFileStru
  SELECT (lcTmpSlc)
  INDEX ON CPO_NO TAG (lcTmpSlc1) UNIQUE
  SELECT (lcTmpSty)
  INDEX ON CPO_NO TAG (lcTmpSty1) UNIQUE
  INDEX ON CPO_NO+STYLE TAG (lcTmpSty)
  SET ORDER TO TAG (lcTmpSty)

  lcScFields = 'cComInvNo   , Complete   , Vendor     , cAddress12 ,' + ; 
               'cAddress22  , cAddress32 , cAddress42 , cAddress52 ,' + ;
               'cAtten      , Phone      , cShipVia   , Note1      ,' + ;
               'Note2       , nWTotal    , nNoBoxes   , nNoBags    ,' + ;
               'cShprName   , cAddress1  , cAddress2  , cAddress3  ,' + ;
               'cAddress4   , cAddress5  , Note_Mem '
  SELECT ComInvHd
  SET ORDER TO ComInv
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK               

 
ENDIF

*:***********************************************************************
*                         Sequence Screen
*:***********************************************************************

*-- Checking for the file carring Sequentinal number
PUSH KEY
DO (gcScrDir+gcWinAppl+"\ICCOMIN.SPX")
ON KEY
POP KEY


****
FUNCTION lpShow
DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]  && Select mode. 
    lnAlias=SELECT()
    SELECT ComInvHd
    lcConName = ' '
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    = lfRefresh()                       && Refresh SAY
    
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]  && View mode.
    SHOW GET laData[23]
    SHOW GET pbZoom    ENABLE     
    SHOW GET PbDetaile ENABLE
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]  && Edit mode. 

  ***--- A D D   M O D E ---***
  CASE laScrMode[4]  && Add mode. 
    laData[2] = DATE()
    SHOW GET laData[1] DISABLE
    SHOW GET pbDetaile DISABLE
    SHOW GET ibBrow    DISABLE
    SHOW GET laData[2] ENABLE
    =gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
    SEEK gcAct_Comp
    lcCompName = cCom_Name
    lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
    lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
    * Get the company addresses
    laData[17]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
    laData[17]    = IIF(EMPTY(laData[17]),SPACE(30),laData[17])
    laData[18]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
    laData[18]    = IIF(EMPTY(laData[18]),SPACE(30),laData[18])
    laData[19]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
    laData[19]    = IIF(EMPTY(laData[19]),SPACE(30),laData[19])
    laData[20]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
    laData[20]    = IIF(EMPTY(laData[20]),SPACE(30),laData[20])
    laData[21]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
    laData[21]    = IIF(EMPTY(laData[21]),SPACE(30),laData[21])
    laData[22]    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)    

ENDCASE

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To validate the invoice number.
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : AriaBrow
*!                          lfShow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvInvNo()
*!*************************************************************

FUNCTION lfvInvNo
PRIVATE lnAlias
lnAlias = SELECT()

SELECT ComInvHd
*-- Validate the entered invoice code from the ComInvHd file.
IF !EMPTY(laData[1]) AND !SEEK(laData[1],"ComInvHd")
  lcBrowTitl = "COMMERCIAL INVOICE"
  lcBrFields = [cComInvNo:8:H = "Invoice",Complete:9:H = "Invoice Date",;
                CSHPRNAME:30:H = "Shipper Name",cAddress1:30:H = "Shipper Address 1",;
                Vendor:8:H = "Consignee",CATTEN:30:H = "Attention",; 
                NWTOTAL:10:H = "Total Weight"]
  
  DECLARE laInv[1] 
  STORE '' TO laInv

  lcRetVal = 'cComInvNo'
  =gfBrows("",lcRetVal,'laInv')             
  IF !EMPTY(laInv[1])
    laData[1] = laInv[1]
  ELSE
    laData[1]= SPACE(6)
    _CUROBJ = _CUROBJ
    SHOW GET laData[1]
  ENDIF
ENDIF  

*-- Switch the screen mode to "View" if a selection was made.
IF !EMPTY(laData[1])
  laScrMode[1] = .F.
  laScrMode[2] = .T.
  SCATTER FIELDS &lcScFields TO laData
  laData[23] = Note_mem
  SHOW GETS
  SHOW GET PBDETAILE ENABLE
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvNew
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To enter a new commercial invoice 
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfShow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNew()
*!*************************************************************

FUNCTION lfvNew
PRIVATE lnAlias 

lnAlias = SELECT()
*-- Switch the screen mode to "Add".
glUpdated = .F.
IF USED(lcTmpDtl)
  SELECT(lcTmpDtl)
  ZAP
ENDIF
IF USED(lcTmpSlc)
  SELECT(lcTmpSlc)
  ZAP
ENDIF
laScrMode[4] = .T.
= lfShow()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwCons
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To store the old consignee value 
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwCons()
*!*************************************************************

FUNCTION lfwCons

lcOldCons = laData[3]

*!*************************************************************
*! Name      : lfvCons
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To validate consignee value 
*!*************************************************************
*! Calls     : 
*!             Procedures : VenBrow
*!             Functions  : gfAPVnBrow
*!                          lfRefresh
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCons()
*!*************************************************************

FUNCTION lfvCons
PRIVATE lnAlias

*-- Validate the entered invoice code from the VenFile file.
lnAlias = SELECT()
IF !EMPTY(laData[3]) AND !SEEK (laData[3],'ApVendor')
  SELECT ApVendor
  lcVendor = laData[3]
  = gfAPVnBrow(@lcVendor)
  laData[3] = lcVendor 
  *-- If the user ESC from browes show the value before he enters
  laData[3] = IIF(EMPTY(laData[3]),lcOldCons,laData[3])
ENDIF

*-- If the user changes consignee get the default from VenFile
IF !EMPTY(laData[3])
  laData[9]  = IIF(lcOldCons =laData[3],laData[9],SUBSTR(ApVendor.cVenCont,1,30))
  laData[10] = IIF(lcOldCons =laData[3],laData[10],SUBSTR(ApVendor.cPhoneNo,1,18))
  lcConName  = IIF(lcOldCons =laData[3],lcConName,SUBSTR(ApVendor.cVenComp,1,21)) 
  laData[4]  = IIF(lcOldCons =laData[3],laData[4],cAddress1)
  laData[5]  = IIF(lcOldCons =laData[3],laData[5],cAddress2)
  laData[6]  = IIF(lcOldCons =laData[3],laData[6],cAddress3)
  laData[7]  = IIF(lcOldCons =laData[3],laData[7],cAddress4)
  laData[8]  = IIF(lcOldCons =laData[3],laData[8],cAddress5)
ELSE
  laData[9]  = SPACE(30)
  laData[10] = SPACE(18)
  lcConName  = SPACE(21)
  laData[4]  = SPACE(30)
  laData[5]  = SPACE(30)
  laData[6]  = SPACE(15)
  laData[7]  = SPACE(03)
  laData[8]  = SPACE(10)
ENDIF

SHOW GET laData[3]
SHOW GET laData[4]
SHOW GET laData[5]
SHOW GET laData[6]
SHOW GET laData[7]
SHOW GET laData[8]
SHOW GET laData[9] 
SHOW GET laData[10] 

= lfRefresh()                       && Refresh SAY
SELECT(lnAlias)

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To save new values 
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfShow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpSavscr()
*!*************************************************************

PROCEDURE lpSavscr
PRIVATE lnAlias

IF laScrMode[4] AND (!USED(lcTmpSlc) OR EOF(lcTmpSlc))
  =gfModalGen("TRM42188B00000" , "DIALOG" , "PO selected. Cannot save.")
  =lfvStyPO()
  STORE .F. TO llShow,llCSave
  RETURN
ENDIF

IF (!USED(lcTmpDtl) OR EOF(lcTmpDtl)) AND laScrMode[4]
  =gfModalGen("TRM42188B00000" , "DIALOG" , "lines for this invoice. Cannot save.")
  _CUROBJ = OBJNUM(pbDetaile)
  STORE .F. TO llShow,llCSave
  RETURN
ELSE 
  IF EMPTY(laData[3])
    =gfModalGen("TRM42072B00000" , "DIALOG" , "Consignee field")
    _CUROBJ = OBJNUM(laData[3])
    STORE .F. TO llShow,llCSave
    RETURN
  ELSE 
    lnAlias = SELECT()
    SELECT ComInvHd
    
    *-- If Add append blank to replace in it and get sequentianl number and increment it
    IF laScrMode[4]
      laData[1]   = gfSequence('cComInvNo')
      llFlag3   = .F.
      DO WHILE !llFlag3
        IF SEEK(laData[1],"COMINVHD")
          laData[1] = PADL(ALLTRIM(STR(INT(VAL(laData[1])) + 1)),6,"0")
        ELSE
          llFlag3 = .T.
        ENDIF
      ENDDO
      SHOW GET laData[1] 
      APPEND BLANK
    ENDIF
    
    *-- Replacing old values with new one in ComInvHd File
    SELECT ComInvHd
    REPLACE cComInvNo  WITH laData[1]  ,;
            Complete   WITH laData[2]  ,;
            CSHPRNAME  WITH laData[17] ,;
            cAddress1  WITH laData[18] ,;
            cAddress2  WITH laData[19] ,;
            cAddress3  WITH laData[20] ,;
            cAddress4  WITH laData[21] ,;
            cAddress5  WITH laData[22] ,;
            Vendor     WITH laData[3]  ,;
            cAddress12 WITH laData[4]  ,;
            cAddress22 WITH laData[5]  ,;
            cAddress32 WITH laData[6]  ,;
            cAddress42 WITH laData[7]  ,;
            cAddress52 WITH laData[8]  ,;
            CATTEN     WITH laData[9]  ,;
            Phone      WITH laData[10] ,;
            CSHIPVIA   WITH laData[11] ,;
            Note1      WITH laData[12] ,;
            Note2      WITH laData[13] ,;
            NNOBOXES   WITH laData[15] ,;
            NNOBAGS    WITH laData[16] ,;
            NWTOTAL    WITH laData[14] ,;
            NOTE_MEM   WITH laData[23]

    IF USED(lcTmpDtl)
      *- Save cominvln Lines
      SELECT (lcTmpDtl)
      lcDlSet = SET('DELETE')
      SET DELETE OFF
      SCAN FOR cStatus <> "S"
        DO CASE
          CASE cStatus = "A"
            SELECT ComInvLn
            APPEND BLANK
            REPLACE cComInvNo  WITH laData[1]           ,;  
                    Complete   WITH laData[2]           ,;
                    PO         WITH &lcTmpDtl..cPO_No   ,;
                    STYLE      WITH &lcTmpDtl..STYLE    ,;
                    cItem      WITH &lcTmpDtl..Item     ,;
                    cType      WITH &lcTmpDtl..cCatgTyp ,;
                    Type       WITH &lcTmpDtl..Typ      ,;                  
                    cDesc      WITH &lcTmpDtl..Desc     ,;
                    cRef       WITH &lcTmpDtl..cRef     ,;
                    cNotes     WITH &lcTmpDtl..cNotes   ,;
                    nQty       WITH &lcTmpDtl..TotQty   ,;
                    nUnits     WITH &lcTmpDtl..nUnits   ,;
                    nPrice     WITH &lcTmpDtl..UntCost  ,;
                    nAmount    WITH &lcTmpDtl..Amount 
      
          CASE cStatus = "M"
            SELECT ComInvLn
            SET ORDER TO TAG COMINVL
            SEEK(laData[1]+&lcTmpDtl..cPO_No+&lcTmpDtl..Style+&lcTmpDtl..Typ+&lcTmpDtl..Item)
            REPLACE cComInvNo  WITH laData[1]           ,;  
                    Complete   WITH ldInvDate           ,;
                    PO         WITH &lcTmpDtl..cPO_No   ,;
                    STYLE      WITH &lcTmpDtl..STYLE    ,;
                    cItem      WITH &lcTmpDtl..Item     ,;
                    cType      WITH &lcTmpDtl..cCatgTyp ,;
                    Type       WITH &lcTmpDtl..Typ      ,;                   
                    cDesc      WITH &lcTmpDtl..Desc     ,;
                    cRef       WITH &lcTmpDtl..cRef     ,;
                    cNotes     WITH &lcTmpDtl..cNotes   ,;
                    nQty       WITH &lcTmpDtl..TotQty   ,;
                    nUnits     WITH &lcTmpDtl..nUnits   ,;
                    nPrice     WITH &lcTmpDtl..UntCost  ,;
                    nAmount    WITH &lcTmpDtl..Amount 
        
         CASE cStatus = "D"
           SELECT ComInvLn
           SET ORDER TO ComInvL
           IF SEEK(laData[1]+&lcTmpDtl..cPO_No+&lcTmpDtl..Style+&lcTmpDtl..Typ+&lcTmpDtl..Item)
             DELETE    
           ENDIF
        ENDCASE
      ENDSCAN
      SET DELETE &lcDlSet
      IF laScrMode[4]
        ** Invoice has been saves as.
        =gfModalGen('INM40005B00000','ALERT',laData[1])
      ENDIF
     
      *-- Zap temprary detailed file
      SELECT(lcTmpDtl)
      ZAP
    ENDIF

    IF USED(lcTmpSlc)
      SELECT(lcTmpSlc)
      ZAP
    ENDIF

    *-- Switch screen mode depend on current screen mode
    SELECT(lnAlias)
  ENDIF
ENDIF



*!*************************************************************
*! Name      : lpDelscr
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To delete the current record in ComInvHd File
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfShow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDelete()
*!*************************************************************

FUNCTION lpDelscr

SELECT ComInvLn
SET ORDER TO TAG COMINVL
IF SEEK (laData[1])
  DELETE REST WHILE cComInvNo = laData[1]
ENDIF
SELECT ComInvHd
IF SEEK (laData[1])
  = RLOCK()
  DELETE
  UNLOCK
ENDIF
IF USED(lcTmpDtl)
  SELECT(lcTmpDtl)
  ZAP
ENDIF
IF USED(lcTmpSlc)
  SELECT(lcTmpSlc)
  ZAP
ENDIF
laScrMode[1] = .T.



*!*************************************************************
*! Name      : lfvBrowes
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To browes from ComInvHd File
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfShow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowes()
*!*************************************************************

FUNCTION lfvBrowes
PRIVATE lnAlias

lnAlias = SELECT()
SELECT ComInvHd
lcBrowTitl = "COMMERCIAL INVOICE"
lcBrFields = [cComInvNo:8:H = "Invoice Number",   Complete:8:H = "Invoice Date"     ,;
              CSHPRNAME:30:H = "Shipper Name",     cAddress1:30:H = "Shipper Address 1",;
              cAddress2:30:H = "Shipper Address 2",cAddress3:15:H = "Shipper City"     ,;
              NWTOTAL:10:H = "Total Weight"]

IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','cComInvNo','laData')
  laData[1] = ComInvHd.cComInvNo
ENDIF
*-- Switch screen mode to view
IF !EMPTY(laData[1])
  IF USED(lcTmpDtl)
    SELECT(lcTmpDtl)
    ZAP
  ENDIF
  IF USED(lcTmpSlc)
    SELECT(lcTmpSlc)
    ZAP
  ENDIF
  laScrMode[2] = .T.
  = lfShow()
ENDIF

SELECT(lnAlias)


*!*************************************************************
*! Name      : lfvZoom
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Zooming comments
*!*************************************************************
*! Calls     : 
*!             Procedures : AriaZoom
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvZoom()
*!*************************************************************

FUNCTION lfvZoom

*IF laScrMode[2]
  Do gfZoom WITH "laData[23]",.F.
*ELSE
*  Do lfZoomWin WITH "laData[23]"
*ENDIF

*!*************************************************************
*! Name      : lfvNotePad
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Switch bettwen screen modes and load information as needed
*!*************************************************************
*! Calls     : 
*!             Procedures : NotePad
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNotePad()
*!*************************************************************

FUNCTION lfvNotePad

DO NotePad WITH "U",laData[1],.F.

*!*************************************************************
*! Name      : lfvBrowInv
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Show ComInv browes
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfvInvNo
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowInv()
*!*************************************************************

FUNCTION lfvBrowInv

laData[1] = "?"
= lfvInvNo()

*!*************************************************************
*! Name      : lfvStyPO
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/04/97
*! Purpose   : Show PO Dialoge
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfOpenFile
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyPO
*!*************************************************************

FUNCTION lfvStyPO

lcPONo   = SPACE(6)                 && PO number
lnPrvLen = 0                        && Previouse length of target array
DIMEN laTarget[1]                   && Mover Target array
DIMEN laSource[1]
laSource = ' '
laTarget = SPACE(1)
DO (gcScrDir+gcWinAppl+"\ICCOMINP.SPR")

*!*************************************************************
*! Name      : lfShow
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Switch bettwen screen modes and load information as needed
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfLoadInfo
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfShow()
*!*************************************************************

PROCEDURE lfShow
PRIVATE lnAlias
lnAlias = SELECT()
DO CASE
  CASE laScrMode[1]
    = lfLoadInfo("")
    SHOW GETS DISABLE
    SHOW GET laData[1] ENABLE
    SHOW GET ibBrow  ENABLE
    *SHOW GET pbNew   ENABLE
    *SHOW GET pbCls   ENABLE
    *lcCPrmt = lcClose
    *SHOW GET pbCls ,1 PROMPT (lcCPrmt)
    
  CASE laScrMode[2]
    = lfLoadInfo("F")
    SHOW GETS DISABLE
    *SHOW GET pbNew      ENABLE
    *SHOW GET pbCls      ENABLE
    SHOW GET pbDetaile  ENABLE
    *SHOW GET pbNotePad  ENABLE
    SHOW GET pbZoom     ENABLE
    *SHOW GET pbSlct     ENABLE
    *SHOW GET pbBrws     ENABLE
    *SHOW GET pbEdt      ENABLE
    *SHOW GET pbDlt      ENABLE
    *SHOW GET pbNxt      ENABLE
    *SHOW GET pbPrvs     ENABLE
    *SHOW GET pbTop      ENABLE
    *SHOW GET pbBtm      ENABLE
    *lcCPrmt = lcClose
    *SHOW GET pbCls ,1 PROMPT (lcCPrmt)
    = lfRefresh()
    IF SEEK ("U"+laData[1],"NOTEPAD") AND !EMPTY("NOTEPAD.MNOTES")
      lcNPPrmt = lcNotPadA
      SHOW GET pbNotePad ,1 PROMPT (lcNPPrmt)  
    ELSE
      lcNPPrmt = lcNotPad
      SHOW GET pbNotePad ,1 PROMPT (lcNPPrmt)
    ENDIF
    SELECT(lnAlias)
    
  CASE laScrMode[3]
    = lfLoadInfo("F")
    SHOW GETS ENABLE
    SHOW GET laData[1]    DISABLE
    SHOW GET ibBrow     DISABLE
    SHOW GET pbNew      DISABLE
    SHOW GET pbSlct     DISABLE
    SHOW GET pbBrws     DISABLE
    SHOW GET pbEdt      DISABLE
    SHOW GET pbNxt      DISABLE
    SHOW GET pbPrvs     DISABLE
    SHOW GET pbTop      DISABLE
    SHOW GET pbBtm      DISABLE
    lcCPrmt = lcCancel
    SHOW GET pbCls ,1 PROMPT (lcCPrmt)
    IF SEEK ("U"+laData[1],"NOTEPAD") AND !EMPTY("NOTEPAD.MNOTES")
      lcNPPrmt = lcNotPadA
      SHOW GET pbNotePad ,1 PROMPT (lcNPPrmt)  
    ELSE
      lcNPPrmt = lcNotPad
      SHOW GET pbNotePad ,1 PROMPT (lcNPPrmt)
    ENDIF
    SELECT(lnAlias)
    
  CASE laScrMode[4]
    = lfLoadInfo("")
    laData[17] = TRIM(lcCompName) 
    laData[18] = laCompAdd[1]
    laData[19] = laCompAdd[2]
    laData[20] = laCompAdd[3]
    laData[21] = laCompAdd[4]
    laData[22] = laCompAdd[5]
    SHOW GETS ENABLE
    SHOW GET laData[1]    DISABLE
    SHOW GET ibBrow     DISABLE
    SHOW GET pbNew      DISABLE
    SHOW GET pbDetaile  DISABLE
    SHOW GET pbSlct     DISABLE
    SHOW GET pbBrws     DISABLE
    SHOW GET pbEdt      DISABLE
    SHOW GET pbNxt      DISABLE
    SHOW GET pbPrvs     DISABLE
    SHOW GET pbTop      DISABLE
    SHOW GET pbBtm      DISABLE
    SHOW GET pbDlt      DISABLE
    lcCPrmt = lcCancel
    SHOW GET pbCls ,1 PROMPT (lcCPrmt)

    lcNPPrmt = lcNotPad
    SHOW GET pbNotePad ,1 PROMPT (lcNPPrmt) DISABLE

    SELECT(lnAlias)
    
ENDCASE

*!*************************************************************
*! Name      : lfLoadInfo
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : To load information or intialize variables
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfLoadInfo()
*!*************************************************************

FUNCTION lfLoadInfo
PARAMETERS lcFrom

IF EMPTY(lcFrom)
  laData[1]  = SPACE(6)
  ldInvDate  = DATE()
  laData[3]  = SPACE(8)
  lcConName  = SPACE(20)
  laData[4]  = SPACE(30)  
  laData[5]  = SPACE(30)  
  laData[6]  = SPACE(15) 
  laData[7]  = SPACE(03) 
  laData[8]  = SPACE(10) 
  laData[9]  = SPACE(30)
  laData[10] = SPACE(18)
  laData[11] = SPACE(20)
  laData[12] = SPACE(20)
  laData[13] = SPACE(30)
  laData[14] = 0
  laData[17] = SPACE(30)
  laData[18] = SPACE(30)
  laData[19] = SPACE(30)
  laData[20] = SPACE(30)
  laData[21] = SPACE(30)
  laData[22] = SPACE(10)
  laData[15] = 0
  laData[16] = 0
  laData[23] = SPACE(10)
ELSE
  laData[1]  = ComInvHd.cComInvNo
  laData[2]  = ComInvHd.Complete
  laData[3]  = ComInvHd.Vendor
  laData[17] = ComInvHd.CSHPRNAME
  laData[18] = ComInvHd.cAddress1
  laData[19] = ComInvHd.cAddress2
  laData[21] = ComInvHd.cAddress4
  laData[20] = ComInvHd.cAddress3
  laData[22] = ComInvHd.cAddress5
  
  =SEEK(laData[3],ApVendor)
  lcConName  = ApVendor.cVenComp
  laData[4]  = ComInvHd.cAddress12
  laData[5]  = ComInvHd.cAddress22
  laData[6]  = ComInvHd.cAddress32
  laData[7]  = ComInvHd.cAddress42
  laData[8]  = ComInvHd.cAddress52
  laData[9]  = ComInvHd.CATTEN
  laData[10] = ComInvHd.Phone
  laData[11] = ComInvHd.CSHIPVIA
  laData[12] = ComInvHd.Note1
  laData[13] = ComInvHd.Note2
  laData[15] = ComInvHd.NNOBOXES
  laData[16] = ComInvHd.NNOBAGS
  laData[14] = ComInvHd.NWTOTAL
  laData[23] = ComInvHd.Note_Mem
ENDIF  


*:***********************************************************************
*                         PO Screen
*:***********************************************************************


*!*************************************************************
*! Name      : lfvPONo
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/04/97
*! Purpose   : To Validate PO number from POSHDR 
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPONo()
*!*************************************************************

FUNCTION lfvPONo
PRIVATE lnAlias
lnAlias = SELECT()
SELECT POSHDR
SET FILTER TO !( STATUS $ "CSX" )
*-- Validate the entered PO code from the POSHdr file.
IF llBrowse OR (!EMPTY(lcPONo) AND !SEEK('P'+lcPONo,"POSHdr"))
  DIME laTemp[1]
  laTemp = ''
  lcBrowTitl = "PO STYLE HEADER"
  lcBrFields = [PO:8:H = "PO Number",VENDOR:11:H = "Vendor",Status:6:H = "Status",ShipVia:7:H = "Ship Via"]
  IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','PO','laTemp')
    lcPONo = laTemp[1]
  ELSE
    lcPONo = SPACE(6)
    _CUROBJ = _CUROBJ
    SHOW GET lcPONo
  ENDIF
ENDIF  
SELECT(lnAlias)
llBrowse = .F.
SET FILTER TO

*!*************************************************************
*! Name      : lfvSlctdPO
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/04/97
*! Purpose   : Browes Selected POs from lcTmpSty
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSlctdPO()
*!*************************************************************

FUNCTION lfvSlctdPO
PRIVATE lnAlias

lnAlias = SELECT()

*-- SHOWING THE USER THE SELECTED POs.
SELECT (lcTmpSlc)
SET ORDER TO TAG (lcTmpSlc1)
GO TOP
IF !EOF()
  lcBrowTitl = "SELECTED POs "
  lcBrFields = [CPO_NO:8:H = "PO Number",STYLE:14:H = "Style",QTY:8:H = "Qty"]
  DIME laTemp[1] 
  laTemp = ''
  IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','CPO_NO','laTemp')
    lcPONo = laTemp[1]
  ELSE
    lcPONo = SPACE(6)
    _CUROBJ = OBJNUM(lcPONo)
  ENDIF
  SHOW GET lcPONo
ELSE
  **No po selected.
  =gfModalGen("TRM42188B00000" , "DIALOG" , "PO selected.")
ENDIF
SELECT (lcTmpSty)
SET ORDER TO TAG (lcTmpSty)
SELECT(lnAlias) 

*!*************************************************************
*! Name      : lfvPOOk
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/05/97
*! Purpose   : Validate PO input then show the mover while storing 
*!             his slections in temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : 
*!                          gfMover()
*!                          SUBSTR()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPOOk()
*!*************************************************************

FUNCTION lfvPOOk


IF EMPTY(lcPONo)
  **No po selected.
  =gfModalGen("TRM42188B00000" , "DIALOG" , "PO selected, cannot proceed.")
  _CUROBJ = OBJNUM(lcPONo)
ELSE 
 CLEAR READ
 RELEASE WINDOW ("lwPOdialog")
 lnAlias = SELECT()
 SELECT POSLn
*-- geting open qty POs and qtyby PO number puting it in mover source array
 SELECT STYLE+"-"+STR(SUM(Totqty),7,0);
    FROM POSLN;
    WHERE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = 'P'+lcPONo  ;
    GROUP BY PO,STYLE ;
    HAVING SUM(IIF(trancd="1",totqty,IIF(trancd="3",0,-totqty)))>0 ;
    INTO ARRAY laSource
*-- geting POs and qty by PO number puting it in mover target array
 SELECT STYLE+"-"+Qty ;
    FROM (lcTmpSty);
    WHERE CPO_NO = lcPONo AND LFLAGE;
    INTO ARRAY laTarget
*-- Showing the mover

  IF gfMover(@laSource,@laTarget,"Select PO Lines",.T.)
    llDflage = .T.
    SELECT (lcTmpSty)
    = SEEK(lcPONo)
    IF EMPTY(laTarget[1])
      DELETE REST WHILE CPO_NO=lcPONo
      PACK
      SELECT (lcTmpSlc)
      = SEEK(lcPONo)
      DELETE REST WHILE CPO_NO=lcPONo
      PACK
      SELECT (lcTmpSty)
    ELSE
*-- Intialize all recordes in the file that have the lcPONo with .F.   
      REPLACE REST WHILE CPO_NO=lcPONo LFLAGE WITH .F.
      FOR lnInd = 1 TO ALEN(laTarget)
        lcSty = SUBSTR(laTarget[lnInd],1,12) 
        lcClr = SUBSTR(laTarget[lnInd],14,6) 
        lnQty = SUBSTR(laTarget[lnInd],21,7) 
        IF EMPTY(lcSty) AND EMPTY(lcClr)
          EXIT
        ENDIF     
*-- From Target array to lcTmpSty file
        IF !SEEK(lcPONo+lcSty+lcClr,lcTmpSty)
          APPEND BLANK
          REPLACE CPO_NO WITH lcPONo;  
                  STYLE  WITH lcSty;
                  Qty WITH lnQty;
                  LFLAGE WITH .T.
        ELSE
          REPLACE LFLAGE WITH .T.
        ENDIF
      ENDFOR 
      SELECT (lcTmpSty)
      GOTO TOP
      SCAN
        IF !SEEK(cPO_No,lcTmpSlc)
          SELECT (lcTmpSlc)
          APPEND BLANK
          REPLACE cPO_NO    WITH &lcTmpSty..cPO_No   ,;
                  STYLE     WITH &lcTmpSty..STYLE    ,;
                  Qty       WITH &lcTmpSty..Qty
        ENDIF
      ENDSCAN
      SHOW GET pbDetaile ENABLE LEVEL RDLEVEL()-1
    ENDIF
  ENDIF
ENDIF


*:***********************************************************************
*                         Details Screen
*:***********************************************************************

*!*************************************************************
*! Name      : lfvDetails
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Intialize variables & load information for detailes screen and show it 
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfGetInfo
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDetails()
*!*************************************************************

FUNCTION lfvDetails

*-- for controlling disabling/enabling
lcShowMode=IIF(laScrMode[2],'DISABLE','ENABLE')
*-- building the temprary file
ldDInvDat  = ldInvDate                    &&Date in details
lnDInvNo   = laData[1]                      &&invoice number in details
liclInsert   = .F.                          && flag for insert

=lfGetInfo()
SELECT (lcTmpDtl)
SCATTER MEMVAR BLANK
lcPattern  = SPACE(10)                     &&Pattern
*-Trapping
PUSH KEY
ON KEY
ON KEY LABEL ALT+B DO lfActBrWin
*-- Details screen
DO (gcScrDir+gcWinAppl+"\ICCOMIND.SPR")
POP KEY

*!*************************************************************
*! Name      : lfBrowse
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Showing the browes
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfBrowse()
*!*************************************************************

FUNCTION lfBrowse

SELECT (lcTmpDtl)
SET ORDER TO TAG (lcTmpDtlx)    
GO TOP
IF !EOF()
  *-- Loading information from temprary file in memory variables
  SCAN WHILE EMPTY(TYP)
    DELETE
  ENDSCAN
  SCATTER MEMVAR
  lcPattern  = STYLE.Pattern
  lnCatgTyp = VAL(TYP)-1
ENDIF
SHOW GETS
lcOldItm  = m.Item
lnBrRecNo = RECNO()
lcBrD1fld = [cMarker=IIF(lnBrRecNo=RECNO(),'',' '):1:H=' ':W=.F.,;
             Item:20:H='Item',cCatgTyp:4:H='Type',Desc:20:H='Description',;   
             totQty:H='QTY':5:R:P='99999',nUnits:H='Units':7:R:P='9999999',;      
          	 UntCost:H='Price':7:R:P='9999.999',Amount:H='Amount':9:R:P='999999.99'] 


*-- Showing the browes
BROWSE FIELDS &lcBrD1fld ;
       LOCK 0   ;
       NOAPPEND ;
       NOCLEAR  ;
  	   NODELETE ;
       NOMENU   ;
       NOEDIT   ;
  	   NOWAIT   ;
	   SAVE     ;
  	   TITLE lcBrowcTtl ;
       WINDOW IcComIn1 IN WINDOW lwCominvd ;
       WHEN lfwBrow()

ACTIVATE WINDOW IcComIn4
_CUROBJ = OBJNUM(pbInsert)
RETURN

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Controlls moveing in the browes geting information
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow()
*!*************************************************************

FUNCTION lfwBrow
PRIVATE lnAlias

lnAlias   = SELECT()
lnBrRecNo = RECNO()
*-- Loading the first record information into memvariables
SCATTER MEMVAR
lcPattern  = STYLE.Pattern
lnCatgTyp = VAL(TYP)-1

SHOW GETS
lcOldItm  = m.Item
SHOW WINDOW (lcBrowcTtl) REFRESH SAME
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfShowD
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Controlls disabling/enabling for details screen
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfShowD()
*!*************************************************************

FUNCTION lfShowD

DO CASE
  CASE laScrMode[2]
    SHOW GETS DISABLE    
  
  CASE laScrMode[3]
    SHOW GETS ENABLE
   
  CASE laScrMode[4]
    SHOW GETS ENABLE
    SHOW GET lnCatgTyp DISABLE

ENDCASE

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Build details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetInfo()
*!*************************************************************

FUNCTION lfGetInfo
PRIVATE lnAlias
lnAlias = SELECT()

IF laScrMode[4] OR ;
  (laScrMode[3] AND USED(lcTmpDtl) AND RECCOUNT(lcTmpDtl)<>0) OR ;
  (laScrMode[2] AND USED(lcTmpDtl) AND !EOF(lcTmpDtl))
  
  *-- Building first temprary file if not exists or make the secondary one then zap selected POs file
  IF !USED(lcTmpDtl) OR EOF(lcTmpDtl)
    SELECT (lcTmpSty)
    *-- Get PONo ,Style from lctmpsty file , and get item,type,desc.,
    *-- totqty,unit cost from BOM file and geting qty from lctmpsty X bom.nbomtotqty in nUnits field,
    *-- lctmp.qty X bom.totcost in Amount field , thats in condition that the po and from 
    *-- lctmpsty file equals theres in the bom file and category type be a fabric or trim

     
     SELECT BOM
     SET ORDER TO

     SELECT &lcTmpSty..CPO_No,&lcTmpSty..STYLE,BOM.ITEM+SPACE(8) AS ITEM,;
           BOM.cCatgTyp,BOM.Typ,BOM.DESC,SUM(BOM.nbomTOTQTY) AS TOTQTY,SUM(VAL(&lcTmpSty..Qty)*Bom.nbomtotQty) AS nUnits,;
           SUM(Bom.Untcost) AS UntCost,SUM(VAL(&lcTmpSty..Qty)*Bom.totcost) AS Amount,;
           BOM.Trim_Invt,SPACE(6) AS cRef,SPACE(42) AS cNotes,"A" AS cStatus;
     FROM (lcTmpSty),BOM ;
     WHERE .F. ;
     INTO DBF (gcDataDir+lcTmpDtl)

     SELECT (lcTmpDtl)
     INDEX ON CPO_No+STYLE+ITEM TAG (lcTmpDtl)
     INDEX ON CPO_No+STYLE+Typ+ITEM TAG (lcTmpDtlx) 
     SET ORDER TO TAG (lcTmpDtlx)

     llFirst = .F.
     SELECT (lcTmpSty)
     GOTO TOP
     lcFrstClr = STYLE
     lcFrstPO  = cPO_No
     SCAN
       IF lcFrstPO <> cPO_No
         lcFrstClr = STYLE
         lcFrstPO  = cPO_No
       ENDIF
       llFirst = (lcFrstClr = STYLE)
       
       SELECT BOM
       SCAN FOR CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = &lcTmpSty..STYLE AND ;
                TYP $ "2345"
         SELECT (lcTmpDtl)
         IF !SEEK(&lcTmpSty..CPO_No+&lcTmpSty..STYLE+BOM.Typ+BOM.Item)
           APPEND BLANK
         ENDIF

         REPLACE CPO_No    WITH &lcTmpSty..CPO_No,;
                 STYLE     WITH &lcTmpSty..STYLE,;
                 ITEM      WITH BOM.ITEM,;
                 cCatgTyp  WITH BOM.cCatgTyp,;
                 Typ       WITH BOM.Typ,;
                 DESC      WITH BOM.DESC,;
                 TOTQTY    WITH TOTQTY  + BOM.nbomTOTQTY,;
                 nUnits    WITH nUnits  + VAL(&lcTmpSty..Qty)*Bom.nbomtotQty ,;
                 UntCost   WITH UntCost + IIF(llFirst, Bom.Untcost, 0),;
                 Amount    WITH nUnits  * UntCost ,;
                 Trim_Invt WITH BOM.Trim_Invt,;
                 cRef      WITH SPACE(6) ,;
                 cNotes    WITH SPACE(42),;
                 cStatus   WITH "A"
       ENDSCAN
     ENDSCAN
     SELECT BOM
     SET ORDER TO TAG BOM
    SELECT (lcTmpDtl)

    *-- Relation to Pattern field
    SET RELATION TO STYLE INTO STYLE
    GOTO TOP
  ELSE
    SELECT (lcTmpSty)
    *-- Get PONo ,Style,from lctmpsty file , and get item,type,desc.,
    *-- totqty,unit cost from BOM file and geting qty from lctmpsty X bom.nbomtotqty in nUnits field,
    *-- lctmp.qty X bom.totcost in Amount field , thats in condition that the po from 
    *-- lctmpsty file equals theres in the bom file and category type be a fabric or trim

    SELECT &lcTmpSty..CPO_No,&lcTmpSty..STYLE,BOM.ITEM,;
           BOM.cCatgTyp,BOM.Typ,BOM.DESC,SUM(BOM.nbomTOTQTY) AS TOTQTY,SUM(VAL(&lcTmpSty..Qty)*Bom.nbomtotQty) AS nUnits,;
           SUM(Bom.Untcost) AS UntCost,SUM(VAL(&lcTmpSty..Qty)*Bom.totcost) AS Amount,SPACE(6) AS cRef,;
           SPACE(42) AS cNotes,"A" AS cStatus;
     FROM (lcTmpSty),BOM ;
     WHERE &lcTmpSty..STYLE=BOM.CITMMAJOR ;
           AND BOM.TYP $ "2345"; 
     GROUP BY &lcTmpSty..CPO_No,BOM.CITMMAJOR,BOM.Item;
     INTO DBF (gcDataDir+lcTmpDtl2)
 
    SELECT (lcTmpDtl)
    SET ORDER TO TAG (lcTmpDtl)
    SELECT (lcTmpDtl2)
    SCAN
    *- build our first temprary file from the second temprary file or from lafields array
      IF SEEK(&lcTmpDtl2..cPO_No+&lcTmpDtl2..Style+&lcTmpDtl2..Item,lcTmpDtl)
        SELECT (lcTmpDtl)
        REPLACE TotQty  WITH TotQty  + &lcTmpDtl2..TotQty   ;
                nUnits  WITH nUnits  + &lcTmpDtl2..nUnits   ;
                UntCost WITH UntCost + &lcTmpDtl2..UntCost ;
                Amount  WITH Amount + &lcTmpDtl2..Amount 
      ELSE
        SCATTER TO laFields
        SELECT (lcTmpDtl)
        APPEND BLANK
        GATHER FROM laFields
      ENDIF
    ENDSCAN
    SELECT (lcTmpDtl2)
    ZAP
  ENDIF
ELSE
  *- Transform the master file 'cominvln' to temprary file for that commercial invoice
  SELECT PO AS CPO_No,Style AS STYLE,cItem AS ITEM,;
         cType AS cCatgTyp,Type AS Typ,cDesc AS DESC,nQty AS TOTQTY,nUnits AS nUnits,;
         nPrice AS UntCost,nAmount AS Amount,cRef AS cRef,cNotes AS cNotes,"S" AS cStatus;
     FROM ComInvLn;
     WHERE cComInvNo = laData[1];
     INTO DBF (gcDataDir+lcTmpDtl)
  INDEX ON CPO_No+STYLE+ITEM TAG (lcTmpDtl)
  INDEX ON CPO_No+STYLE+Typ+ITEM TAG (lcTmpDtlx) 
  SET ORDER TO TAG (lcTmpDtl)
  *-- Relation to Pattern field
  SET RELATION TO STYLE INTO STYLE

  *-- Get PONo ,Style from lctmpsty file , and get item,type,desc.,
  *-- totqty,unit cost from BOM file and geting qty from lctmpsty X bom.nbomtotqty in nUnits field,
  *-- lctmp.qty X bom.totcost in Amount field , thats in condition that the po from 
  *-- lctmpsty file equals theres in the bom file and category type don't be 1
  *-- puting the result into temprary file   
  

   *-- compare between the temprary file from prevouise select and the 
   *-- temprary file from master file if found accumelate if not append 
   *-- in the temprary file from master file
ENDIF
SELECT (lcTmpSty)
ZAP
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvRemv
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Delete record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemv()
*!*************************************************************

FUNCTION lfvRemv
PRIVATE lnAlias
lnAlias = SELECT()

  SELECT(lcTmpDtl)
  REPLACE cStatus WITH "D"  
  DELETE
  GOTO TOP
  SHOW WINDOW (lcBrowcTtl) REFRESH
  IF EOF(lcTmpDtl)
    SHOW GETS DISABLE
    SHOW GET pbClose  ENABLE
    SHOW GET pbInsert ENABLE
  ELSE
    SCATTER MEMVAR
    lcPattern  = STYLE.Pattern
    lnCatgTyp = VAL(TYP)-1
    SHOW GETS
    lcOldItm  = m.Item
  ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvInsert
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Insert record to details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvInsert()
*!*************************************************************

FUNCTION lfvInsert
PRIVATE lnAlias
lnAlias = SELECT()
llInsert      = .T.
m.Style       = SPACE(12)
m.cPO_No      = SPACE(6)
lcPattern     = SPACE(10)
m.Item        = SPACE(20)
m.Desc        = SPACE(20)
m.cRef        = SPACE(6)
m.cNotes      = SPACE(42)
m.TotQty      = 1
m.nUnits      = 0
m.UntCost     = 0
m.Amount     = 0

SHOW GET m.Item
= lfShowInD()
_CUROBJ = OBJNUM(m.cPO_No)

*!*************************************************************
*! Name      : lfvItem
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify item field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvItem()
*!*************************************************************

FUNCTION lfvItem
PRIVATE lnAlias
lnAlias = SELECT()

DO CASE
  CASE llInsert
    IF !EMPTY(m.Item)
      SELECT (lcTmpDtl)
      APPEND BLANK
      REPLACE CPO_No   WITH m.cPO_NO ,; 
              STYLE    WITH m.Style ,;
              Item     WITH m.Item,;
              cStatus  WITH "A"     ,; 
              cCatgTyp WITH laCatgTyp[lnCatgTyp,2],;
              Typ      WITH ALLTRIM(STR(lnCatgTyp+1))
      lnBrRecNo = RECNO()
      SHOW GET m.Item
    ELSE
      **Item field cannot be empty.
      IF gfModalGen("TRM36002B36005" , "DIALOG" , "Item") = 2
        SELECT (lcTmpDtl)
        SCATTER MEMVAR
        lcPattern  = STYLE.Pattern
        lnCatgTyp = VAL(TYP)-1
        _CUROBJ = OBJNUM(pbClose)
        lcOldItm  = m.Item
      ELSE
        _CUROBJ = OBJNUM(m.Item)
        RETURN
      ENDIF
    ENDIF
    IF !EMPTY(lcOldItm)
      = lfShowInD()
    ENDIF

  CASE ! llInsert
    IF lcOldVal <> m.Item 
      IF EMPTY(m.Item)
        m.Item = lcOldItm       
      ENDIF
      SELECT (lcTmpDtl)
      REPLACE Item    WITH m.Item 
      REPLACE cStatus WITH IIF(cStatus="A","A","M")
      m.Desc = FABRIC.Desc
      SHOW GET m.Item
      SHOW GET m.Desc
    ENDIF
ENDCASE
SHOW WINDOW (lcBrowcTtl) REFRESH SAME
SHOW GET IBBROW1 ENABLE
llInsert = .F.
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvQty
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify TotQty field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvQty()
*!*************************************************************

FUNCTION lfvQty
PRIVATE lnAlias

IF m.TotQty < 0
  **Negative values are not allowed.
  =gfModalGen("TRM42000B00000" , "DIALOG")
  _CUROBJ = _CUROBJ
ELSE 
  IF lcOldVal <> m.TotQty
    lnAlias = SELECT()
    SELECT (lcTmpDtl)
    REPLACE TotQty  WITH m.TotQty 
    REPLACE cStatus WITH IIF(cStatus="A","A","M")      
    SHOW WINDOW (lcBrowcTtl) REFRESH
  ENDIF     
ENDIF


*!*************************************************************
*! Name      : lfvUnits
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify Units field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvUnits()
*!*************************************************************

FUNCTION lfvUnits
PRIVATE lnAlias

IF m.nUnits < 0
  **Negative values are not allowed.
  =gfModalGen("TRM42000B00000" , "DIALOG")
  _CUROBJ = _CUROBJ
ELSE 
  IF lcOldVal <> m.nUnits
    lnAlias = SELECT()
    SELECT (lcTmpDtl)
    REPLACE nUnits WITH m.nUnits 
    REPLACE cStatus WITH IIF(cStatus="A","A","M")    
    SHOW WINDOW (lcBrowcTtl) REFRESH
  ENDIF     
ENDIF
m.Amount = m.nUnits * m.UntCost

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify UnitCost field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPrice()
*!*************************************************************

FUNCTION lfvPrice
PRIVATE lnAlias
IF m.UntCost < 0
  **Negative values are not allowed.
  =gfModalGen("TRM42000B00000" , "DIALOG")
  _CUROBJ = _CUROBJ
ELSE 
  IF lcOldVal <> m.UntCost
    lnAlias = SELECT()
    SELECT (lcTmpDtl)
    REPLACE UntCost WITH m.UntCost 
    REPLACE cStatus WITH IIF(cStatus="A","A","M")
    SHOW WINDOW (lcBrowcTtl) REFRESH
  ENDIF     
ENDIF
m.Amount=m.nUnits * m.UntCost

*!*************************************************************
*! Name      : lfvAmount
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify Amount field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAmount()
*!*************************************************************

FUNCTION lfvAmount
PRIVATE lnAlias
IF m.Amount < 0
  **Negative values are not allowed.
  =gfModalGen("TRM42000B00000" , "DIALOG")
  _CUROBJ = _CUROBJ
ELSE 
  IF lcOldVal <> m.Amount
    lnAlias = SELECT()
    SELECT (lcTmpDtl)
    REPLACE Amount WITH m.Amount 
    REPLACE cStatus WITH IIF(cStatus="A","A","M")
    SHOW WINDOW (lcBrowcTtl) REFRESH
  ENDIF     
ENDIF

SHOW GET m.Amount

*!*************************************************************
*! Name      : lfvDesc
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify Desc. field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDesc()
*!*************************************************************

FUNCTION lfvDesc
PRIVATE lnAlias

lnAlias = SELECT()
IF lcOldVal <> m.Desc
  SELECT (lcTmpDtl)
  REPLACE Desc WITH m.Desc 
  REPLACE cStatus WITH IIF(cStatus="A","A","M")
  SHOW WINDOW (lcBrowcTtl) REFRESH
ENDIF     

*!*************************************************************
*! Name      : lfvRef
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify Ref field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRef()
*!*************************************************************

FUNCTION lfvRef
PRIVATE lnAlias

lnAlias = SELECT()
IF lcOldVal <> m.Desc
  SELECT (lcTmpDtl)
  REPLACE cRef WITH m.cRef 
  REPLACE cStatus WITH IIF(cStatus="A","A","M")
ENDIF     

*!*************************************************************
*! Name      : lfvNotes
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Modify Notes field in current record from details temprary file
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNotes()
*!*************************************************************

FUNCTION lfvNotes
PRIVATE lnAlias

lnAlias = SELECT()
IF lcOldVal <> m.Desc
  SELECT (lcTmpDtl)
  REPLACE cNotes WITH m.cNotes 
  REPLACE cStatus WITH IIF(cStatus="A","A","M")
ENDIF     

*!*************************************************************
*! Name      : lfvCatgTyp
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Controll in the category type Popup
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfActPop
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCatgTyp()
*!*************************************************************
 
FUNCTION lfvCatgTyp
*- Function for popup for DOS
*- Get the descreption of the type and array pointer to the type and number
lcCatgTyp = laCatgTyp[lnCatgTyp,2]
SHOW GET lnCatgTyp
SHOW GET m.Item    ENABLE
SHOW GET lnCatgTyp DISABLE  
SHOW GET m.Desc    ENABLE
SHOW GET m.cRef    ENABLE
SHOW GET m.cNotes    ENABLE
SHOW GET m.TotQty    ENABLE
SHOW GET m.nUnits    ENABLE
SHOW GET m.UntCost    ENABLE
SHOW GET m.Amount    ENABLE
_CUROBJ = OBJNUM(m.Item)
RETURN

*!*************************************************************
*! Name      : lfvWCatTyp
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Controll in the category type Popup
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvWCatTyp()
*!*************************************************************

 
FUNCTION lfvWCatTyp

*- Function for popup for WINDOWS
*- Get the descreption of the type and array pointer to the type and number
lcCatgTyp = laCatgTyp[lnCatgTyp,2]
SHOW GET lnCatgTyp
SHOW GET m.Item    ENABLE
SHOW GET lnCatgTyp DISABLE
SHOW GET m.Desc    ENABLE
SHOW GET m.cRef    ENABLE
SHOW GET m.cNotes    ENABLE
SHOW GET m.TotQty    ENABLE
SHOW GET m.nUnits    ENABLE
SHOW GET m.UntCost    ENABLE
SHOW GET m.Amount    ENABLE

RETURN


*!*************************************************************
*! Name      : lfvPO_No
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Validation for po in details screen
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPO_No()
*!*************************************************************

FUNCTION lfvPO_No
PRIVATE lnAlias
lnAlias = SELECT()
SELECT POSHDR
*-- Validate the entered PO code from the POSHdr file.
IF !EMPTY(m.cPO_No) AND !SEEK('P'+m.cPO_No,"POSHdr")
  DIME laTemp[1]
  laTemp = ''
  lcBrowTitl = "PO STYLE HEADER"
  lcBrFields = [PO:8:H = "PO Number",VENDOR:11:H = "Vendor",Status:6:H = "Status",ShipVia:7:H = "Ship Via"]
  IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','PO','laTemp')
    m.cPO_No = laTemp[1]
  ELSE
    m.cPO_No = SPACE(6)
    _CUROBJ  = _CUROBJ
    SHOW GET m.cPO_No
  ENDIF
ENDIF
  
IF laScrMode[4] OR laScrMode[3] 
  IF !EMPTY(m.cPO_No)
    SHOW GET m.Style ENABLE
  ENDIF
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Validation for the style in details screen
*!*************************************************************
*! Calls     : 
*!             Procedures : StyBrow
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle
PRIVATE lnAlias
lnAlias = SELECT()
IF !EMPTY(m.Style)
  IF EMPTY(m.cPO_No)
    DO StyBrow WITH m.Style,"*","*"
    IF !EMPTY(m.Style)
      lcPattern  = STYLE.Pattern
      SHOW GET lcPattern   
    ELSE
      m.Style   = SPACE(12)
      lcPattern = SPACE(10)
      _CUROBJ   = _CUROBJ
    ENDIF
  ELSE
    *- select all styles for that po showing qty 
    SELECT STYLE,SUM(Totqty) AS QTY;
      FROM POSLN;
       WHERE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD='P'+m.cPO_No ;
       GROUP BY PO,STYLE ;
       INTO DBF (gcDataDir+lcTmpBrow)
    SELECT (lcTmpBrow)
    INDEX ON Style TAG (lcTmpBrow)
    *- if the style dosen't belong to the po browes the selected file
    IF !SEEK(m.Style,lcTmpBrow)
      DIME laTemp[1]
      laTemp = ''
      lcBrowTitl = "PO STYLE HEADRE"
      lcBrFields = [Style:14:H = "Style",Qty:7:H = "QTY"]        
          
      IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','Style','laTemp')
        m.Style = laTemp[1]
        SELECT STYLE
        SEEK(m.Style)
        lcPattern = STYLE.Pattern
      ELSE
        m.Style   = SPACE(12)
        lcPattern = SPACE(10)
        _CUROBJ   = _CUROBJ
      ENDIF
      SHOW GET m.Style
      SHOW GET lcPattern
    ENDIF
  ENDIF
  SELECT STYLE
  SEEK(m.Style)
  lcPattern = STYLE.Pattern
  SHOW GET lcPattern
  _CUROBJ  = OBJNUM(lnCatgTyp)
ENDIF
IF !EMPTY(m.Style)
  SHOW GET lnCatgTyp ENABLE
ELSE
  ** You have to insert a style .. "\<Ok;\<Cancel"
  IF gfModalGen("TRM42213B00030" , "DIALOG" ) = 1
   _CUROBJ  = OBJNUM(m.Style)
  ELSE
    m.CPO_No = " "
    SHOW GET m.cPO_No DISABLE
    SHOW GET m.Style DISABLE   
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfShowInD
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : Switch bettwen screen modes in details as needed
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfShowInD()
*!*************************************************************
 
FUNCTION lfShowInD
DO CASE
  CASE laScrMode[4]
    SHOW GET m.cPO_No  ENABLE
    SHOW GET m.Style DISABLE
    SHOW GET lnCatgTyp DISABLE
    SHOW GET lcPattern DISABLE
    SHOW GET m.Item    DISABLE
    SHOW GET m.Desc    DISABLE
    SHOW GET m.TotQty  DISABLE
    SHOW GET m.nUnits  DISABLE 
    SHOW GET m.UntCost DISABLE
    SHOW GET m.Amount DISABLE
    SHOW GET m.cRef    DISABLE
    SHOW GET m.cNotes  DISABLE
    
  CASE laScrMode[3]
    SHOW GET m.cPO_No  DISABLE    
    SHOW GET m.Style   DISABLE
    SHOW GET lcPattern DISABLE
    SHOW GET lnCatgTyp DISABLE
    SHOW GET m.Item    ENABLE
    SHOW GET m.Desc    ENABLE
    SHOW GET m.TotQty  ENABLE
    SHOW GET m.nUnits  ENABLE
    SHOW GET m.UntCost ENABLE
    SHOW GET m.Amount ENABLE
    SHOW GET m.cRef    ENABLE
    SHOW GET m.cNotes  ENABLE
ENDCASE

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Rami Gamil Anis - (RGA)
*! Date      : 06/03/97
*! Purpose   : to keep field old value
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwOldVal()
*!*************************************************************

FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))


*:*************************************************************
*! Name      : lfTrapKy
*! Purpose   : Trap key.
*:*************************************************************
FUNCTION lfTrapKy
IF WONTOP() = lcBrowcTtl
  ON KEY LABEL TAB        DO lptab1
  ON KEY LABEL backtab    DO lpsfttp1
  ON KEY LABEL ESC        DO lpsfttp1
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfReadAct
*! Purpose   : READ Activate function.
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfActBrWin
*! Purpose   : key trapping procedure for activate browse.
*!*************************************************************
FUNCTION lfActBrWin 

IF !WONTOP(lcBrowcTtl)
  ACTIVATE WINDOW (lcBrowcTtl) TOP
ENDIF  


*!*************************************************************
*! Name      : lptab1
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
PROCEDURE lptab1

IF WONTOP(lcBrowcTtl)
  SHOW GET IBBROW1 DISABLE
  ACTI WINDOW IcComIn3
  _CUROBJ = OBJNUM(ITEM)
ENDIF  
RETURN 


*!*************************************************************
*! Name      : lpsfttp1
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
PROCEDURE lpsfttp1

IF WONTOP(lcBrowcTtl)
  ACTI WINDOW IcComIn4
  _CUROBJ = OBJNUM(pbClose)
ENDIF
RETURN

