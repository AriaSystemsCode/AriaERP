*:************************************************************************
*: PROGRAM   : SOORCNGM.PRG      (C101807)
*: DESC.     : Order confirmation Form (GM)(Conversion of ORD510Z.PRG FROM 2.6)
*: System    : Aria Apparel System    (A27)
*: Module    : SALES ORDER ALLOCATION (AL)
*: Developer : ABDOU ELGENDI        - (ABD)
*: DATE      : 05/04/2000
*: REFER TO  : (C101807)
*: Modified  : (C200335) Mohamed Shokry (MHM)
*:************************************************************************
*: Calls : 
*:         FUNCTION  : lfPrtONte , lfAddRec   , lfPrnConti , lfPrnLast
*:                   : lfPrnHdr  , lfPrnNotPd , lfGetPack  , lfGetOdHdr
*:                   : lfPrGrdTot.
*:         --------------------------------------------------------------
*:         PROCEDURE : 
*:************************************************************************
*: Example : DO SOOECNGM
*:************************************************************************
*: This Program is due to C101807 ...
*:************************************************************************
*:Modifications:
*:C200479,1 HBG 16/01/2003 If ordLine.range OR ordLine.price per piece OR ordLine.nPkSlPirce = 0 
*:C200479,1                change calculating the amount
*:B607387,1 SSE 29/06/2003 Fix numeric overflow when having more than 999 lines in Order Lines.
*:B119649,1 MHM 07/09/2003 Fix numeric overflow when lrange="T" and totqty=0 in ordline file .
*:B120495,1 MHM 11/06/2003 Print CUSTPO 15 chrs.
*:B123013,1 NNA 05/31/2004 Fix bug that the report does not print all Pack colors
*:********************************************************************************************

*-- Set the memory variables.

*-- llRpPack    --> Print the Pack_Id (.T.-->.F.).
*-- llRpPrTCon  --> Print consolidating all stores by Style/Color (.T.-->.F.).

*-- llRpOrdLNt  --> IF The Add notes to order lines seting is set to No
*-- llSQty      --> Print Sizes Qty (.T.-->.F.).

SET DEVICE TO SCREEN

STORE '' TO lcOrder,lcAccount,lcMulti,lcStore,laAddress,lcTermData,lcShipVia,lcDivison,;
            lcBtName
STORE 0 TO lnNum
STORE .F. To NOTELOOP , llYes

DECLARE laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
laSoldTo = ''                     && Array to hold the Sold To address
laShipTo = ''                     && Array to hold the Ship To address
lcDivLName = ''
laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

*-- If No records selected.
SELECT ORDHDR
SET SKIP TO
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  *-- Variable to do EndReport Or Don't.
  llNoRec = .T.
  RETURN
ELSE
  *-- Variable to do EndReport Or Don't.
  llNoRec = .F.
ENDIF

*-- Check if there is only one Order selected. [Begin]
PRIVATE lnOrderPos
lnOrderPos = lfItmPos('ORDHDR.ORDER')   && get Get Order Fixed filter Position
IF (UPPER(laOGVrFlt[lnOrderPos,5]) = "IN LIST" AND USED(laOGVrFlt[lnOrderPos,6]) AND RECCOUNT(laOGVrFlt[lnOrderPos,6])=1)
  IF lfvChkOrd()
    DO lpvGMAStor
  ENDIF  
ENDIF
*-- Check if there is only one Order selected. [End]

SELECT (lcTempOrd)

*-- Get the style and color
lcMajTitl  = ALLTRIM(gfItemMask('HI'))

IF llRpPrTCon &&  Print consolidating all stores by Style/Color

  lcTmpFile = gfTempName()
  lcTmpHdr  = gfTempName()

  *--If the user answered yes then cursor will be created to hold the totals.

  CREATE TABLE &lcTmpFile (Style C(19) ,nTotQty    N(7), nPrice N(8,2), cDesc C(20),;
                          pack  M(10) ,nSugRetPri N(8,2),pack_id  c(16) ,cPkColor C(6),;
                          cPckSize C(3), cPkVersion C(4),Store C(8),nQty1 N(6),nQty2 N(6),;
                          nQty3 N(6),nQty4 N(6),nQty5 N(6),nQty6 N(6),nQty7 N(6),nQty8 N(6))
                          
  
  IF !llRpPack
    INDEX ON Style TAG lcTmpFile 
  ELSE  
    IF llRpPrTCon
      INDEX ON Style + Pack_Id +cpkcolor+cpcksize+ cPkVersion  TAG lcTmpFile 
    ELSE
      INDEX ON Style +Store + Pack_Id +cpkcolor+cpcksize+ cPkVersion  TAG lcTmpFile 
    ENDIF
  ENDIF  

  CREATE TABLE &lcTmpHdr (Style C(19) ,nTotQty    N(7), nPrice N(8,2), cDesc C(20),;
                          nSugRetPri N(8,2),pack_id  c(16),cPkColor C(6),cPckSize C(3), cPkVersion C(4),Store C(8) , Note_Mem M(10))

  INDEX ON Style  + Pack_Id +cpkcolor+cpcksize+ cPkVersion TAG lcTmpHdr    
  
  llYes =.T.
  IF !USED(lcTmpFile)
    =gfOpenFile(gcWorkDir+lcTmpFile,'','EX')
  ENDIF
  *-- Open Header file as exclusive [Start]
  IF !USED(lcTmpHdr)
    =gfOpenFile(gcWorkDir+lcTmpHdr,'','EX')
  ENDIF 
ENDIF
*--add new table to get pack_id and SKU, UPC 
lcTmpDesc = gfTempName()

*--SKU1...8 fields is related to SKU per size
*--UPC1...8 fields is related to UPC per size
  
CREATE TABLE &lcTmpDesc (Style C(19) ,pack_id  c(16) ,cPkColor C(6),cPckSize C(3), cPkVersion C(4),PackSKU C(16),PackUPC C(16),;
                        SKU1 C(16),SKU2 C(16),SKU3 C(16),SKU4 C(16),SKU5 C(16),SKU6 C(16),SKU7 C(16),SKU8 C(16),;
                        UPC1 C(16),UPC2 C(16),UPC3 C(16),UPC4 C(16),UPC5 C(16),UPC6 C(16),UPC7 C(16),UPC8 C(16),;
                        Size1 C(5),Size2 C(5),Size3 C(5),Size4 C(5),Size5 C(5),Size6 C(5),Size7 C(5),Size8 C(5),;
                        llStyUPC L(1),llStySKU L(1))

IF !llRpPack
  INDEX ON  Style TAG lcTmpDesc 
ELSE  
  INDEX ON   Pack_Id +cpkcolor+cpcksize+ cPkVersion +Style TAG lcTmpDesc 
ENDIF

*-- add new file to check pack in case of No
IF !llRpPrTPak
  lcTmpPack = gfTempName()
  *C200479,1 HBG 16/01/2003 Add fields to Apply the check If ordLine.range OR ordLine.price 
  *C200479,1                per piece OR ordLine.nPkSlPirce = 0 in calculating the amount [Begin]
  *CREATE TABLE &lcTmpPack (pack_id  c(16) ,cPkColor C(6),cPckSize C(3), cPkVersion C(4),;
                          Store C(8),nTotQty N(7),nPackPrc N(9,2),PackUPC  C(30),PackSKU C(30),Note_Mem M(10))
  
  *B607387,1 SSE 29/06/2003 Increase field width. [Begin]
  *CREATE TABLE &lcTmpPack (pack_id  c(16) ,cPkColor C(6),cPckSize C(3), cPkVersion C(4),;
  *                        Store C(8),nTotQty N(7),nPackPrc N(9,2),PackUPC  C(30),PackSKU C(30),;
  *                        Note_Mem M(10),nAmount N(14,2),nPackSlPrc N(8,2),;
  *                        lRange L(1),lPckPrPiec L(1),nPkSlPrice N(8,2),nCount N(3),nPackQty N(7))
  CREATE TABLE &lcTmpPack (pack_id  c(16) ,cPkColor C(6),cPckSize C(3), cPkVersion C(4),;
                          Store C(8),nTotQty N(7),nPackPrc N(9,2),PackUPC  C(30),PackSKU C(30),;
                          Note_Mem M(10),nAmount N(14,2),nPackSlPrc N(8,2),;
                          lRange L(1),lPckPrPiec L(1),nPkSlPrice N(8,2),nCount N(7),nPackQty N(7))
  *B607387,1 SSE 29/06/2003 Increase field width. [End]

  *C200479,1 [End]
  INDEX ON Pack_Id +cpkcolor+cpcksize+ cPkVersion + Store  TAG lcTmpPack
ENDIF  


*--  Open needed files That didn't open before.
=gfOpenFile(gcDataDir+'Styleupc',gcDataDir+'styleupc','SH')
=gfOpenFile(gcDataDir+'Spck_Lin',gcDataDir+'Spck_Lin','SH')
=gfOpenFile(gcDataDir+'Spck_Hdr',gcDataDir+'Spck_Hdr','SH')
*--Initilize variables


*-- We Have Temp File Called lcTempOrd Fill Of All Data.
SELECT (lcTempOrd)


DO CASE 
  CASE lcRpSortBy = 'S'
    INDEX ON CORDTYPE + ORDER + STORE + STYLE + STR(LINENO,6) + Pack_Id +cpkcolor+cpcksize+ cPkVersion   TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)

  CASE lcRpSortBy = 'L'
    INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) + STYLE +  Pack_Id +cpkcolor+cpcksize+ cPkVersion   TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)

  CASE lcRpSortBy = 'P'
    IF llRpPrTCon
      INDEX ON CORDTYPE + ORDER + PACK_ID +cpkcolor+cpcksize+ cPkVersion  + STORE  + STYLE + STR(LINENO,6) TAG (lcTempOrd)
    ELSE
      INDEX ON CORDTYPE + ORDER + STORE  + Pack_Id +cpkcolor+cpcksize+ cPkVersion + STYLE + STR(LINENO,6)  TAG (lcTempOrd)
    ENDIF  
    SET ORDER TO TAG (lcTempOrd)

ENDCASE

A= REPLICATE('*',80)
*   ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
B='|  START  | COMPLETE |    TERMS      |   SHIPVIA     | SEASON   |REP1 REP2'
*   MM/DD/YY   MM/DD/YY  123456789012345 123456789012345 1234567890 123  123
C= 'STYLE        COLOR  DESCRIPTION             S.R.P      PRICE TOT. QTY     AMOUNT'
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    STYLE   CLR                                     PRICE   PIECES       AMOUNT
*123 1234567 123 12345678901234567890              1234.99   123456   123,456.99

*-------------------------------
* SETUP HEADINGS
*-------------------------------
*--Fill the company address variables.
*--Center the address
laCompAdd1 = ''
laCompAdd2 = SPACE(ROUND((80-LEN(laCompAdd[2]))/2,0))+laCompAdd[2]
laCompAdd3 = SPACE(ROUND((80-LEN(laCompAdd[3]))/2,0))+laCompAdd[3]
laCompAdd4 = SPACE(ROUND((80-LEN(laCompAdd[4]))/2,0))+laCompAdd[4]
laCompAdd[5] = SyCcomp.Ccom_phon
laCompAdd5 = SPACE(ROUND((80-LEN(ALLTRIM(laCompAdd[5])))/2,0))+laCompAdd[5]
MAXROW = 51
lcTmpPage = gfTempName()
CREATE CURSOR (lcTmpPage) (Order C(6), Store C(8),LastPage N(6))
INDEX ON order+STORE tag (lcTmpPage)
lnPageNo = 0
*--Get the pages or seleted orders.

SELECT (lcTempOrd)
IF llOneMulti AND !EMPTY(laStores)
  SET FILTER TO ASCAN(laStores,ALLTRIM(Store)) <> 0
  
  *-- Updating the Recap in case of Temp. EDI. [Begin]
  IF lcRpOrdTyp = "T"  
    SELECT (lcTempRcp)
    LOCATE
    REPLACE Qty1   WITH 0 , Qty2   WITH 0 , Qty3   WITH 0 , Qty4   WITH 0 ,;
            Qty5   WITH 0 , Qty6   WITH 0 , Qty7   WITH 0 , Qty8   WITH 0 ,;
            TotQty WITH 0 , Amount WITH 0
    
    SELECT (lcTempOrd)
    SCAN
      SCATTER MEMVAR MEMO
      SELECT (lcTempRcp)
      IF !SEEK(m.Account+m.Style)
        INSERT INTO (lcTempRcp) (Account,Style,nSRP,Scale) VALUES ;
                                (m.Account,m.Style,m.nSugRetPri,m.Scale)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Qty1 ,;
              Qty2   WITH Qty2   + m.Qty2 ,;
              Qty3   WITH Qty3   + m.Qty3 ,;
              Qty4   WITH Qty4   + m.Qty4 ,;
              Qty5   WITH Qty5   + m.Qty5 ,;
              Qty6   WITH Qty6   + m.Qty6 ,;
              Qty7   WITH Qty7   + m.Qty7 ,;
              Qty8   WITH Qty8   + m.Qty8 ,;
              TotQty WITH TotQty + m.TotQty ,;
              Amount WITH Amount + m.TotQty*m.Price
    ENDSCAN
  ENDIF
ELSE
  SET FILTER TO
ENDIF


STORE 1 TO lnNoteLn,lnSvNoteLn
llNrmPrt = .T.
*-- Print the Form.
SELECT Ordhdr
SET DEVICE TO PRINT
NEWDOC = .T.
GOTO TOP

SCAN REST WHILE INKEY()<> 32 FOR &lcRpExp
  WAIT WINDOW 'Printing Order #:...'+Order+'..<Space Bar> TO ABORT' NOWAIT
  *--Get the Order header information.
  =lfGetOdHdr()
  lnTotQty  = 0.00
  lnTotORd  = 0.00
  NOTELOOP  = .F.
  
  llRpPrTCon = (lcMulti = 'Y' .AND. llYes ) &&  Print consolidating all stores by Style/Color.
  
  *-- Start Printing  The header.
  SELECT (lcTempOrd)
  lcStore = Store
  =lfPrnHdr()

  *--- START PRINTING THE ORDER LINES
  ROW = 27
  NEWDOC  = .T.
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  *LN#   GRP STYLE           CLR                        PRICE   PIECES       AMOUNT
  *123456 X  123456789012345 123 12345678901234567890 1234.99   123456   123,456.99
  *            11111 22222 33333 44444 55555 66666 77777 88888
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
  *-- Main loop of printing 

  SCAN WHILE cOrdType+Order = ORDHDR.cOrdType+lcOrder    
    DO CASE
      CASE lcMulti = 'Y' .AND. Store # lcStore
        IF !llRpPrTCon    &&  Print consolidating all stores by Style/Color.

            *-- Print order notepade
            IF llRpOrdNot
              =lfPrnNotPd()
              ROW = ROW+1
            ENDIF  
            *-- Print page footer
            =lfPrnLast()
            lcStore = Store 
            *-- get page header for the new page, then print it.
            =lfGetOdHdr()            
            =lfPrnHdr()
        ENDIF
      CASE ROW >= MAXROW
        NEWDOC = .F.
        =lfPrnConti()
        =lfPrnHdr()
    ENDCASE
    lcStyDesc = Style.Desc

    IF !llRpPrTCon  &&  Print consolidating all stores by Style/Color
      IF llRpPrTPak .OR. EMPTY(Pack_id) .OR. !llRpPack
        @ ROW,00 SAY Style
        @ ROW,20 SAY lcStyDesc
        @ ROW,40 SAY &lcTempOrd..nSugRetPri PICTURE '999.99' 
        @ ROW,47 SAY Price                  PICTURE '999,999.99'
        @ ROW,60 SAY TotQty                 PICTURE '999999'
        lnAmount = TotQty   * Price
        lnTotORd = lnTotORd + lnAmount
        lnTotQty = lnTotQty + TotQty
        @ ROW,67 SAY lnAmount PICTURE '99,999,999.99'
        IF !EMPTY(&lcTempOrd..DESC1) OR !EMPTY(STYLE.DESC1)
          ROW = ROW + 1
          @ ROW,0 SAY IIF(EMPTY(&lcTempOrd..DESC1),STYLE.DESC1,&lcTempOrd..DESC1)
        ENDIF
        ROW = ROW + 1

        lcPack = lfGetPack()
        =lfPrtpack(lcPack,'')

      ELSE    
        =lfPrntPack()
      ENDIF
        IF ROW >= MAXROW
          NEWDOC = .F.
          =lfPrnConti()
          =lfPrnHdr()
        ENDIF
        IF  llRpOrdLnt .AND. !EMPTY(ORDHDR.Note1) .AND. SUBSTR(ALLTRIM(ORDHDR.Note1),1,1) <> '*'
          @ ROW,27 SAY  ALLTRIM(ORDHDR.Note1)

          IF MEMLINES(Note_Mem) <> 0 .AND. EMPTY(ORDHDR.Note2)
            ROW = ROW + 1
          ENDIF
        ENDIF        
        IF llRpOrdLnt .AND. !EMPTY(ORDHDR.Note2) .AND. SUBSTR(ALLTRIM(ORDHDR.Note2),1,1) <> '*'
          @ ROW,58 SAY SUBSTR(ALLTRIM(ORDHDR.Note2),1,22)
        
          IF MEMLINES(Note_Mem) <> 0 
            ROW = ROW + 1
          ENDIF
        ENDIF
        IF MEMLINES(Note_Mem) <> 0 AND llRpOrdLNt .AND. llRpPrTPak

         *-- Print order notepade lines.
          =lfPrtOnte()
        ENDIF  
        ROW = ROW + 2
    ELSE      
      IF llRpPrTPak .OR. EMPTY(Pack_id) .OR. !llRpPack
        =lfAddRec()
      ELSE    
        =lfPrntPack()
      ENDIF
    ENDIF
  ENDSCAN
  *------------------------ END PRINT LINE LOOP ----------------------*

  IF !llRpPrTCon

    IF llRpOrdNot
      =lfPrnNotPd()
      ROW = ROW+1
    ENDIF  
    *-- Print page footer
    =lfPrnLast()
  ENDIF  
  =lfPrGrdTot()    
ENDSCAN

SET DEVICE TO SCREEN

llRpPrTCon  = llYes

RETURN
*-- End OF Print.
*!*************************************************************
*! Name      : lfPrtONte
*! Developer : ABDOU ELGENDI
*! Date      : 05/04/2000
*! Purpose   : Function to print Order line notes.
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfPrtONte()
*!*************************************************************
*
FUNCTION lfPrtONte

*-- Set the memo width to our standard.
PRIVATE  lnOldMemW
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75

ROW = ROW-1
lnNoteLn = lnSvNoteLn
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= MaxRow - 1
    NewDoc   = .F.
    lnSvNoteLn = lnNoteLn
    *-- Restore the old memo wodth.
    SET MEMOWIDTH TO lnOldMemW

    RETURN(.F.)
  ELSE
    ROW = ROW + 1
    @ ROW,01 SAY MLINE(Note_Mem,lnNoteLn)
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  ROW = ROW + 1
ENDIF
lnSvNoteLn = 1
*-- Restore the old memo wodth.
SET MEMOWIDTH TO lnOldMemW

RETURN(.T.)

*-- End OF lfPrtONte.
*!*************************************************************
*! Name      : lfPrnConti
*! Developer : ABDOU ELGENDI
*! Date      : 05/04/2000
*! Purpose   : Function to print the continue if the order
*!           : Was more than one page
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None. 
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnConti()
*!*************************************************************
*
FUNCTION lfPrnConti

@ 53,00 SAY REPLICATE('-',80)
@ 54,13 SAY 'C O N T I N U E D ...'
@ 54,69    SAY '**********'
@ 55,00 SAY REPLICATE('-',80)

*--End Of lfPrnConti.
*!*************************************************************
*! Name      : lfPrnLast
*! Developer : ABDOU ELGENDI
*! Date      : 05/04/2000
*! Purpose   : Function to print the total
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnLast()
*!*************************************************************
*
FUNCTION lfPrnLast

lnMer_Disc = ORDHDR.DISC
lnMer_Amnt = (lnMer_Disc * lnTotORd)/100
lnNet_Amnt = lnTotORd - lnMer_Amnt
@ 52,00 SAY A
@ 53,10 SAY 'GROSS AMOUNT      =>'
@ 53,64 SAY lnTotORd PICTURE '9,999,999,999.99'
@ 54,10 SAY 'MERCHANDISE DISC. =>'
@ 54,40 SAY lnMer_Disc PICTURE '999.99' + " %"
@ 54,69 SAY lnMer_Amnt PICTURE '999,999.99'
@ 55,10 SAY 'NET AMOUNT        =>'
@ 55,55 SAY lnTotQty PICTURE '99999999'
@ 55,64 SAY lnNet_Amnt PICTURE '9,999,999,999.99'
@ 56,00 SAY A
@ 57,22 SAY '______________________________________   ____________'
@ 58,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
NEWDOC   = .F.
lnTotQty  = 0.00
lnTotORd  = 0.00

*-- End Of lfPrnLast.
*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : ABDOU ELGENDI
*! Date      : 05/04/2000
*! Purpose   : Function to print the header
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
*
FUNCTION lfPrnHdr

*-- Print the company address.
IF EMPTY(lcDivLName)
  @ 01,01 SAY SPACE(ROUND((80-LEN(gccom_name))/2,0))+gccom_name
ELSE
  @ 01,00 SAY SPACE(ROUND((80-LEN(ALLTRIM(lcDivLName)))/2,0)) + ALLTRIM(lcDivLName)
ENDIF
lnPageNo = lnPageNo +1

=SEEK(ORDHDR.ORDER+IIF(!llRpPrTCon,LCSTORE,''),lctmppage)

lnRemain = (&lctmppage..lastpage/25) - INT(&lctmppage..lastpage/25)
@ 01,66 SAY "Page# :" + ALLTRIM(STR(lnPageNo,3))+"/"+ALLTRIM(STR(INT(&lctmppage..lastpage/25)+IIF(lnRemain<>0 OR &lctmppage..lastpage = 0 ,1,0),3))
@ 02,01 SAY laCompAdd1
@ 03,01 SAY laCompAdd2
@ 04,01 SAY laCompAdd3
@ 05,01 SAY laCompAdd5

@ 09,03 SAY 'ORDER: '+OrdHdr.Order
@ 09,59 SAY 'DATE: '
@ 09,65 SAY OrdHdr.Entered

*B120495,1 MHM 11/06/2003 Increase CustPo to be 15[Start]
*@ 10,60 SAY 'PO #: '
*@ 10,66 SAY IIF(OrdHdr.MultiPO,IIF(llYes,"",LEFT(&lcTempOrd..CUSTPO,14)),LEFT(OrdHdr.CUSTPO,14))
@ 10,59 SAY 'PO #:'
@ 10,64 SAY IIF(OrdHdr.MultiPO,IIF(llYes,"",LEFT(&lcTempOrd..CUSTPO,15)),LEFT(OrdHdr.CUSTPO,15))
*B120495,1 MHM 11/06/2003 [End]


@ 12,04 SAY '........ SOLD TO ........'
@ 12,46 SAY '........ SHIP TO ........'           
@ 13,04 SAY OrdHdr.Account
=SEEK('M'+ACCOUNT,'CUSTOMER')
@ 13,10 SAY "Vendor #: " + Customer.cCusVend

@ 13,46 SAY IIF(llRpPrTCon,"",'STORE: ' + lcStore) + '  DEPT: ' + OrdHdr.Dept

*-- Print the Sold to and Shipto 
@ 14,04 SAY lcBtName
@ 14,46 SAY lcShptName
@ 15,04 SAY laSoldTo[1]
@ 15,46 SAY laShipTo[1]
@ 16,04 SAY laSoldTo[2]
@ 16,46 SAY laShipTo[2]
@ 17,04 SAY laSoldTo[3]
@ 17,46 SAY laShipTo[3]

@ 19,12 SAY IIF(Customer.lIn_Store,"IN STORE","")
@ 20,00 SAY A
@ 21,00 SAY B
@ 22,00 SAY A
@ 23,01 SAY OrdHdr.Start
@ 23,12 SAY OrdHdr.Complete
@ 23,22 SAY lcTermData
@ 23,38 SAY lcShipVia
@ 23,54 SAY lcSeason
@ 23,65 SAY OrdHdr.REP1
@ 23,70 SAY OrdHdr.REP2
@ 24,00 SAY A
IF !llRpPrTPak .AND. !EMPTY(Pack_id)
  @ 25,00 SAY lcMajTitl+'  DESCRIPTION                 AVG.PRICE TOT. QTY        AMOUNT'
ELSE
  @ 25,00 SAY lcMajTitl+'  DESCRIPTION          S.R.P      PRICE TOT. QTY        AMOUNT'
ENDIF
@ 26,00 SAY A
ROW = 27
*-- End Of lfPrnHdr.

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : ABDOU ELGENDI
*! Date      : 05/04/2000
*! Purpose   : Function to print the notepad
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
*
FUNCTION lfPrnNotPd
PRIVATE  lnAlias,lnOldMemW, lnMemLins, lnNotLine

lnAlias = SELECT()

IF llRpOrdNot && if Print NotePade. 
  lnNotLine = 1
  SELECT NotePad
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  IF SEEK ('B'+lcOrder)
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    DO WHILE lnNotLine <= lnMemLins
      IF Row > MAXROW
        =lfPrnConti()
        =lfPrnHdr()
      ENDIF          
      IF  LEFT(MLINE(MNOTES,lnNotLine),1) <> '*'
        @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
        ROW = ROW + 1
      ENDIF  
      lnNotLine = lnNotLine + 1
    ENDDO     
  ENDIF
  SET MEMOWIDTH TO lnMemWidth
ENDIF  
SELECT(lnAlias)

*-- End OF lfPrnNotPd.
*!***************************************************************************
*! Name      : lfGetOdHdr
*! Developer : ABDOU ELGENDI
*! Date      : 05/04/2000
*! Purpose   : Function to get the information of the ORDER header.
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetOdHdr()
*!***************************************************************************
*
FUNCTION lfGetOdHdr

PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""

lnPageNo = 0

lcAlias = ALIAS()

lcOrder = &lcTempOrd..Order
IF OrdHdr.Multi = 'Y' .AND. llYes
  llRpPrTCon  = .T.
ELSE
  llRpPrTCon  = .F.
ENDIF
lcMulti   = OrdHdr.Multi
lcAccount = Ordhdr.Account
SELECT (lcTempOrd)
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)  
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia
lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.

IF lcMulti = "Y" .AND. !llRpPrTCon

  SELECT CUSTOMER
  =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)
  lcBtName  = CUSTOMER.BTNAME
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
  
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    SELECT CUSTOMER
    =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF 
ELSE
  lcBtName  = CUSTOMER.BTNAME
  SELECT CUSTOMER
  
  = gfGetAdr('CUSTOMER','','','',@laSoldTo,'2')
  
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    SELECT CUSTOMER

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF 

ENDIF
laCompAdd1 = laCompAdd[1]

laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1

NOTELOOP = .F.
lnTotQty  = 0.00
lnTotORd  = 0.00

SELECT (lcAlias)

*-- End OF lfGetOdHdr.

*!*********************************************************************************************
*! Name      : lpCalcPage
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Procedure to calculate pages no for selected orders.
*!*********************************************************************************************
*! Calls     :  None.
*!*********************************************************************************************
*! Passed Parameters  : None.
*!*********************************************************************************************
*! Returns            : None.
*!*********************************************************************************************
*! Example            : DO lpCalcPage
*!*********************************************************************************************
* NOTE : This procedure runs through all the program functions below as if it were printing.
PROCEDURE lpCalcPage

STORE 1 TO lnNoteLn,lnSvNoteLn 
llNrmPrt = .T.
*-- Print the Form.
SELECT Ordhdr
SET DEVICE TO PRINT
NEWDOC = .T.
LOCATE
SCAN WHILE INKEY()<> 32 FOR &lcRpExp
  lnLastPage = 0
  *--Get the Order header information.
  =lfGetMeHd()
  lnTotQty  = 0.00
  lnTotORd  = 0.00
  NOTELOOP  = .F.
  
  llRpPrTCon = (lcMulti = 'Y' .AND. llYes ) &&  Print consolidating all stores by Style/Color.
  
  *-- Start Printing  The header.
  SELECT (lcTempOrd)
  lcStore = Store 
  ROW = 27
  NEWDOC  = .T.
  *-- Main loop of printing 
  SCAN WHILE cOrdType+Order = ORDHDR.cOrdType+lcOrder
    DO CASE
      CASE lcMulti = 'Y' .AND. Store # lcStore 

        IF !llRpPrTCon    &&  Print consolidating all stores by Style/Color.
       
           *-- Print order notepade
           IF llRpOrdNot
             =lfPrnMeNo()
             ROW = ROW+1
           ENDIF
           =lfGetMeHd()
           insert into (lctmppage) (order,Store,lastpage) values (ordhdr.order,lcStore,lnLastPage)
           lnLastPage = 0
           lcStore = Store 
        ENDIF  
      CASE ROW >= MAXROW
        NEWDOC = .F.
    ENDCASE
    lcStyDesc = Style.Desc

    IF !llRpPrTCon  &&  Print consolidating all stores by Style/Color
    
        IF !EMPTY(&lcTempOrd..DESC1) OR !EMPTY(STYLE.DESC1)
          ROW = ROW + 1
          lnLastPage = lnLastPage +1
        ENDIF
        ROW = ROW + 1
        lnLastPage = lnLastPage +1
        lcPack = lfGetmepack()
        =lfPrtmepack(lcPack)
        IF MEMLINES(Note_Mem) <> 0 AND llRpOrdLNt
          *-- Print order notepade lines.
          =lfPrtmeon() 
        ENDIF  
        ROW = ROW + 2
        lnLastPage = lnLastPage +2
    ELSE
      =lfAddme()
    ENDIF  
  ENDSCAN

  IF !llRpPrTCon

    IF llRpOrdNot
      =lfPrnMeNo()
    ENDIF
    ROW = ROW+1
    IF lcRpSortBy <> 'S'
      lnLastPage = lnLastPage +1
    ENDIF  
  ENDIF  
  *-- Print page footer
  =lfPrMyGrdT()    
  INSERT INTO (lctmppage) (order,Store,lastpage) values (ordhdr.order,lcStore,lnLastPage)
ENDSCAN
SELECT OrdHdr
set relation to order into (lctmppage) addi
SET DEVICE TO SCREEN

llRpPrTCon = llYes

RETURN
*-- End OF Print.

*!*********************************************************************************************
*! Name      : lfPrtmeon
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Calculate order notepad's lines
*!*********************************************************************************************
*! Calls     :  None.
*!*********************************************************************************************
*! Passed Parameters  : None.
*!*********************************************************************************************
*! Returns            : None.
*!*********************************************************************************************
*! Example            : =lfPrtmeon()
*!*********************************************************************************************
* 
FUNCTION lfPrtmeon

PRIVATE  lnOldMemW
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75

ROW = ROW-1
lnLastPage = lnLastPage -1
lnNoteLn = lnSvNoteLn
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= MaxRow - 1
    NewDoc   = .F.
    lnSvNoteLn = lnNoteLn
    SET MEMOWIDTH TO lnOldMemW
    RETURN(.F.)
  ELSE
    ROW = ROW + 1
    lnLastPage =  lnLastPage +1
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  ROW = ROW + 1
  lnLastPage = lnLastPage +1
ENDIF
lnSvNoteLn = 1
SET MEMOWIDTH TO lnOldMemW
RETURN(.T.)

*-- End OF lfPrtmeon.
*!*********************************************************************************************
*! Name      : lfAddme
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Add lines.
*!*********************************************************************************************
*! Calls     :  None.
*!*********************************************************************************************
*! Passed Parameters  : None.
*!*********************************************************************************************
*! Returns            : None.
*!*********************************************************************************************
*! Example            : =lfAddme()
*!*********************************************************************************************
* 
FUNCTION lfAddme
PRIVATE lnAlias
lcStyle = ''
lnAlias = SELECT()
lcStyle = &lcTempOrd..Style 
*--- Select the temp file that print accumolate styles/colors totals.
SELECT (lcTmpFile)
  
IF SEEK(lcStyle,lcTmpFile)  && if found the same style in the file.
  lcTPack =lfGetmepack()
  REPLACE  PACK WITH lfPackMerg(ALLTRIM(PACK),lcTPAck)
  *-- add Sizes Fields [Start]
  REPLACE  Nqty1 WITH &lcTempOrd..Qty1 +Nqty1;
           Nqty2 WITH &lcTempOrd..Qty2 +Nqty2;
           Nqty3 WITH &lcTempOrd..Qty3 +Nqty3;
           Nqty4 WITH &lcTempOrd..Qty4 +Nqty4;
           Nqty5 WITH &lcTempOrd..Qty5 +Nqty5;
           Nqty6 WITH &lcTempOrd..Qty6 +Nqty6;
           Nqty7 WITH &lcTempOrd..Qty7 +Nqty7;
           Nqty8 WITH &lcTempOrd..Qty8 +Nqty8 
  
ELSE  && else if didn't found the style in the file (New Style).
  APPEND BLANK
  REPLACE Style      WITH &lcTempOrd..Style          ,;
          nPrice     WITH &lcTempOrd..Price          ,;
          cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
          nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
          Pack       WITH lfGetmepack() ,;
          nTotQty    WITH &lcTempOrd..TotQty         
  *--add Sizes Fields [Start]
  REPLACE  Nqty1 WITH &lcTempOrd..Qty1;
           Nqty2 WITH &lcTempOrd..Qty2;
           Nqty3 WITH &lcTempOrd..Qty3;
           Nqty4 WITH &lcTempOrd..Qty4;
           Nqty5 WITH &lcTempOrd..Qty5;
           Nqty6 WITH &lcTempOrd..Qty6;
           Nqty7 WITH &lcTempOrd..Qty7;
           Nqty8 WITH &lcTempOrd..Qty8
  
ENDIF
GOTO TOP
SELECT(lnAlias)
*-- End Of  lfAddme.

*!*********************************************************************************************
*! Name      : lfPrnMeNo
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Calculate order notepad's lines
*!*********************************************************************************************
*! Calls     :  None.
*!*********************************************************************************************
*! Passed Parameters  : None.
*!*********************************************************************************************
*! Returns            : None.
*!*********************************************************************************************
*! Example            : =lfPrtmeon()
*!*********************************************************************************************
* C(102351)
FUNCTION lfPrnMeNo
PRIVATE  lnAlias,lnOldMemW, lnMemLins, lnNotLine

lnAlias = SELECT()

IF llRpOrdNot && if Print NotePade. 
  lnNotLine = 1
  SELECT NotePad
  lnMemWidth = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  IF SEEK ('B'+lcOrder)
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    DO WHILE lnNotLine <= lnMemLins
      IF  LEFT(MLINE(MNOTES,lnNotLine),1) <> '*'
        ROW = ROW + 1
        lnLastPage = lnLastPage +1
      ENDIF  
      lnNotLine = lnNotLine + 1
    ENDDO     
  ENDIF
  SET MEMOWIDTH TO lnMemWidth
ENDIF  
SELECT(lnAlias)

*-- End OF lfPrnMeNo.
*!***************************************************************************
*! Name      : lfGetmepack
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Update the field TMPFILE.pack with either pack_id from ORDLINE
*!           : file or the sku
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetmepack()
*!***************************************************************************
*
FUNCTION lfGetmepack

PRIVATE lcOrderset ,lcPackId
Store '' TO lcOrderset , lcPackId

IF llRpPack AND !EMPTY(&lcTempOrd..Pack_id)  && If not empty tempfile Get the Pack_Id from it.

  lcPackId = "PACK # : "+ &lcTempOrd..Pack_id
  IF SEEK(&lcTempOrd..Pack_id,"STYLEUPC")
    lcPackId = lcPackId + SPACE(2) + "PACK UPC#: " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
  ENDI
ENDIF
lcOrderset = ORDER("SPCK_LIN")
SET ORDER TO TAG SPCKLINS IN SPCK_LIN
IF SEEK(&lcTempOrd..Style,"STYLEUPC")
  FOR lnNum =1 TO 8
    lcNum = STR(lnNum,1)
    IF !EMPTY(&lcTempOrd..qty&lcNum) .AND. SEEK(&lcTempOrd..Style+lcNum,"STYLEUPC")
    
      IF llRpSQty
        lcPackId = IIF(EMPTY(lcPackId),"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum+SPACE(2)+'QTY'+SPACE(2)+STR(&lcTempOrd..Qty&lcNum,6),;
                          lcPackId+'|'+"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum+SPACE(2)+'QTY'+SPACE(2)+STR(&lcTempOrd..Qty&lcNum,6))
      ELSE  && No to print sizes Qty.
        *----  Add UPC # to lcPackId like "Pack # line|UPC # upc1 sz1 sz2 sz3 'Qty'  TotalQty"
        lcPackId = IIF(EMPTY(lcPackId),"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum,;
                          lcPackId+'|'+"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum)
      ENDIF
    ENDIF
  ENDFOR
ELSE &&   Elseif not found sku for the account-style get the UPC Number.

  IF llRpPack AND SEEK("S"+&lcTempOrd..Account+&lcTempOrd..Style,"SPCK_LIN")
  
    lcPackId = IIF(EMPTY(lcPackId),"SKU # : " + SPCK_LIN.Pack_id,;
                      lcPackId+'|'+"SKU # : " + SPCK_LIN.Pack_id)
  ENDIF
ENDIF
IF !EMPTY(lcOrderset)   && restore the ordrer for spck_lin
  SET ORDER TO TAG &lcOrderset IN SPCK_LIN
ENDIF 
RETURN lcPackId 
*-- End Of lfGetmepack.

*!***************************************************************************
*! Name      : lfGetMeHd
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to get the information of the ORDER header.
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetMeHd()
*!***************************************************************************
*
FUNCTION lfGetMeHd

PRIVATE lcAlias , lcCurrKey
lcCurrKey = ""
lcAlias = ALIAS()
lcOrder = &lcTempOrd..Order
IF OrdHdr.Multi = 'Y' .AND. llYes
  llRpPrTCon  = .T.
ELSE
  llRpPrTCon  = .F.
ENDIF 
lcMulti   = OrdHdr.Multi
lcAccount = Ordhdr.Account
SELECT (lcTempOrd)
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)  
STORE '' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia
lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.

IF lcMulti = "Y" .AND. !llRpPrTCon

  SELECT CUSTOMER
  =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)
  lcBtName  = CUSTOMER.BTNAME
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.
  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    SELECT CUSTOMER
    =SEEK('S'+ &lcTempOrd.. Account+ &lcTempOrd..Store)
    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF 
ELSE
  lcBtName  = CUSTOMER.BTNAME
  SELECT CUSTOMER
  = gfGetAdr('CUSTOMER','','','',@laSoldTo,'2')
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.
  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    SELECT CUSTOMER
    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    = gfGetAdr('CUSTOMER','','','',@laShipTo,'')
    = lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
  ENDIF 
ENDIF
laCompAdd1 = laCompAdd[1]
laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1
NOTELOOP = .F.
lnTotQty  = 0.00
lnTotORd  = 0.00
SELECT (lcAlias)

*-- End OF lfGetMeHd.
*!***************************************************************************
*! Name      : lfPrtmepack
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to print field Pack.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfPrtmepack(Pack)
*!***************************************************************************
*
FUNCTION lfPrtmepack
PARAMETER lcPack
IF !EMPTY(lcPack)
  IF '|' $ lcPack   && if string contains more than one line Print line by line cutting every line you  print.
    lnOcur = OCCURS('|',lcpack) && No of ocuurs = No of lines - 1
    FOR I = 1 TO lnOcur
      lcPack = STRTRAN(lcPack,SUBSTR(lcPack,1,AT('|',lcPack)),'') && then remove this line from string
      ROW = ROW + 1
      lnLastPage = lnLastPage +1
      IF ROW >= MAXROW
        IF I < lnOcur
*          lnLastPage = lnLastPage +25
          ROW = 27
        ENDIF  
      ENDIF
    ENDFOR
  ENDIF
  *--- Now we have one line string (from begining or after cutting prev. lines).
  *--- So just print it.
  ROW = ROW + 1
  lnLastPage = lnLastPage +1
ENDIF
*-- End Of lfPrpack

*!***************************************************************************
*! Name      : lfPackMerg
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to merge two PAck strings.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfPackMerg(Pack1,Pack2)
*!***************************************************************************
*
FUNCTION lfPackMerg
PARAMETERS PACK1,PACK2
*----- here we want to add second string info to first one info.


IF AT('Style UPC #', PACK2 ) = 0   && IF no upc in 2nd string no need to continue.
  RETURN  PACK1
ENDIF
IF AT('Style UPC #', PACK1 ) = 0    && If No upc in first string then return with second string
  IF AT('PACK #', PACK2) = 0
    IF AT('PACK #', PACK1) <> 0
      Pack2 = LEFT(Pack1,AT('|',PACK1))  + Pack2
      Pack1 = STRTRAN(PACK1,LEFT(Pack1,AT('|',PACK1)),'')
    ENDIF
  ENDIF
  PACK1 = PACK2
  RETURN PACK1
ENDIF
IF AT('PACK #', PACK1) = 0  && if no pack_id in 1st string
  IF AT('PACK #', PACK2) <> 0  && the second str. has pack_id
    Pack1 = LEFT(Pack2,AT('|',PACK2))  + Pack1         && add second string pack_id to 1st str.
    Pack2 = STRTRAN(PACK2,LEFT(Pack2,AT('|',PACK2)),'')  && remove pack_id from 2nd str.
  ENDIF
ENDIF

IF AT('PACK #', PACK2) <> 0  && the second str. has pack_id
  Pack2 = STRTRAN(PACK2,LEFT(Pack2,AT('|',PACK2)),'')  && remove pack_id from 2nd str.
ENDIF

Pack2 = Pack2+"|"  
IF !llRpSQty

  Pack1 = Pack1 + SPACE(ATC('|',Pack1,MAX(OCCURS('|',Pack1),1))+34-len(Pack1))
ENDIF
FOR I =1 TO OCCURS('|',Pack2)   && No of Upc's
  IF AT(LEFT(Pack2,AT('UPC',Pack2)+24),Pack1) <> 0 && if the size in both str's the sum qtys  

    IF llRpSQty    && Only if Print sizes qty (yes)

      lcTVal = STR(VAL(SUBSTR(Pack1,AT(LEFT(Pack2,AT('QTY',Pack2)),Pack1)+41,6))+VAL(RIGHT(LEFT(PACK2,AT("|",Pack2)),7)),6)+"|"
      Pack1 = STRTRAN(Pack1,SUBSTR(Pack1,AT(LEFT(Pack2,AT('QTY',Pack2)),Pack1),42),LEFT(Pack2,AT('QTY',Pack2))+'TY'+SPACE(2)+lcTVal)
    ENDIF
  ELSE   && if size is not exist in first string 
    IF RIGHT(Pack1 ,1) = "|"  
      Pack1 = Pack1 + LEFT(Pack2,AT('|',PACK2))  && add the size upc to 1st str as it.
    ELSE
      Pack1 = Pack1 + '|' + LEFT(Pack2,AT('|',PACK2))  
    ENDIF
  ENDIF
Pack2 = STRTRAN(Pack2,LEFT(Pack2,AT("|",Pack2)))  && remove this upc from 2nd str.
ENDFOR
IF RIGHT(Pack1 ,1) = "|"  
  Pack1 = LEFT(Pack1,LEN(Pack1)-1)  && clean up the string.
ENDIF

IF !llRpSQty

  Pack1 = Pack1 + SPACE(ATC('|',Pack1,MAX(OCCURS('|',Pack1),1))+34-len(Pack1))
ENDIF
Pack1 =lfStrArng(Pack1)
RETURN PACK1
*-- End Of lfPackMerg

*!***************************************************************************
*! Name      : lfStrArng
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to arrang sizes into PAck string.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfStrArng(Pack,40)
*!***************************************************************************
*
FUNCTION lfStrArng
PARAMETER PKUPC 
PRIVATE TempPack, lnTemp,llErDis
llErDis = .T.

DIMENSION laScal[OCCURS('Style UPC #',PKUPC)]
FOR I = 1 TO OCCURS('Style UPC #',PKUPC)
  FOR J = 1  TO 8
    Z = STR(J,1)
    IF SCALE.SZ&Z = SUBSTR(PKUPC,AT('Style UPC #',PKUPC,I)+29,5)
      laScal[I] = J
      EXIT
    ENDIF    
  ENDFOR
ENDFOR
DO WHILE llErDis 
  llErDis = .F.
  FOR I = 1 TO 8
    IF ASCAN(laScal,I) = 0
      FOR J = 1 TO ALEN(laScal,1)
        IF lascal[J] > I 
          llErDis = .T.
          laScal[J] = laScal[J]-1
        ENDIF
      ENDFOR
    ENDIF
  ENDFOR
ENDDO
IF ATC('Style UPC',PKUPC) <> 1
  TempPack = LEFT(PKUPC,ATC('Style UPC',PKUPC)-1)
ELSE
  TempPack = ''
ENDIF

FOR I =  1 TO ALEN(laScal,1)
  TempPack = TempPack+ SUBSTR(PKUPC,ATC('Style UPC #',PKUPC,ASCAN(laScal,I)),IIF(llRpSQty,42,29)+6)
ENDFOR

IF RIGHT(TempPack ,1) = "|"  
  TempPack = LEFT(TempPack,LEN(TempPack)-1)  && clean up the string.
ENDIF

RETURN TempPack

*-- End Of lfStrArng


*!***************************************************************************
*! Name      : lfvChkOrd
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to check if the user selects ONE multi store order.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  =lfvChkOrd()
*!***************************************************************************
*
FUNCTION lfvChkOrd

GO TOP IN (laOGVrFlt[lnOrderPos,6])
llOneMulti = (SEEK('O'+EVALUATE(laOGVrFlt[lnOrderPos,6]+'.Order'),'OrdHdr') OR ;
             SEEK('C'+EVALUATE(laOGVrFlt[lnOrderPos,6]+'.Order'),'OrdHdr') OR ;
             SEEK('T'+EVALUATE(laOGVrFlt[lnOrderPos,6]+'.Order'),'OrdHdr')) AND ;
             OrdHdr.Multi = 'Y'
IF !llOneMulti
  STORE '' TO laStores , laStorTar
ENDIF

RETURN llOneMulti
*-- End of lfvChkOrd.

*!***************************************************************************
*! Name      : lfPrMyGrdT
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to print the grand totals
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfPrMyGrdT()
*!***************************************************************************
*
FUNCTION lfPrMyGrdT
PRIVATE lnAlias

lnAlias = SELECT()
IF lcMulti = 'Y'

  IF llRpPrTCon  &&  Print consolidating all stores by Style/Color.
  
    SELECT (lcTmpFile)
    SCAN REST
      IF ROW > MAXROW
      ENDIF
      ROW = ROW + 1
      lnLastPage = lnLastPage +1
      =lfPrtmepack(ALLT(Pack))
      ROW = ROW +2
      lnLastPage = lnLastPage +2
    ENDSCAN
    *-- Function to print the order notepad 
    ROW = ROW + 1
    lnLastPage = lnLastPage +1
    =lfPrnMeNo()
    ROW = ROW + 1
    lnLastPage = lnLastPage +1
    SELECT (lcTmpFile)
    ZAP   
  ENDIF  
  NEWDOC = .T.
ENDIF  

SELECT(lnAlias)
*-- End Of lfPrMyGrdT.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/29/2001
*! Purpose   : To get the position of Variable filter in OG
*! Reference : B604784,1
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGVrFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGVrFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpvGMAStor
*! Developer : Sameh Saiid Ezzat (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to select order's stores.
*! Reference : B604784,1
*!**************************************************************************
*! Example   : =lpvGMAStor()
*!**************************************************************************
*
FUNCTION lpvGMAStor
PRIVATE lcOrder
DIMENSION laStores[1] , laStorScr[1]
laStorScr = ''

lcOrder   = EVALUATE(laOGVrFlt[lnOrderPos,6]+'.Order')
IF (SEEK('O'+lcOrder,'OrdHdr') AND SEEK('O'+lcOrder,'OrdLine')) OR ;
  (SEEK('C'+lcOrder,'OrdHdr') AND SEEK('C'+lcOrder,'OrdLine')) OR ;
  (SEEK('T'+lcOrder,'OrdHdr') AND SEEK('T'+lcOrder,'OrdLine'))
  SELECT OrdLine
  SCAN WHILE cOrdType+Order = OrdHdr.cOrdType+lcOrder
    IF ASCAN(laStorScr,ALLTRIM(Store)) = 0
      IF !EMPTY(laStorScr[ALEN(laStorScr,1)])
        DIMENSION laStorScr[ALEN(laStorScr,1)+1]
      ENDIF  
      laStorScr[ALEN(laStorScr,1)] = ALLTRIM(Store)      
    ENDIF
  ENDSCAN
ENDIF

IF !EMPTY(laStorScr)
  IF llOGFltCh
    DIMENSION laStorTar[1]
    laStorTar = .F.
  ENDIF  
  = gfMover(@laStorScr,@laStorTar,'Select Stores ',.T.)
  laStores = ""
  IF !EMPTY(laStorTar[1])
    =ACOPY(laStorTar,laStores)
  ENDIF
ENDIF
*-- End of lpvGMAStor.
*!*************************************************************
*! Name      : lfConsYsNo
*! Developer : Mohamed Shokry
*! Date      : 02/17/2002
*! Purpose   : Function to Validate consolidate
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfConsYsNo()
*!*************************************************************
*
FUNCTION lfConsYsNo
PARAMETER lcDummy

lcDummy = .T.
CLEAR READ

RETURN lcDummy

*!*************************************************************
*! Name      : lfPackYsNo
*! Developer : Mohamed Shokry
*! Date      : 02/17/2002
*! Purpose   : Function to Validate Pack_id
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfPackYsNo()
*!*************************************************************
*
FUNCTION lfPackYsNo
PARAMETER lcDummy

lcDummy = .T.
CLEAR READ

RETURN lcDummy

*!*************************************************************
*! Name      : lfAddRec
*! Developer : Mohamed Shokry
*! Date      : 02/17/2002
*! Purpose   : Function to accumolate the style/color totals
*!           : per order
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfAddRec()
*!*************************************************************
**
FUNCTION lfAddRec
PRIVATE lnAlias
lcStyle = ''
lnAlias = SELECT(0)
lcStyle = &lcTempOrd..Style
*--- Select the temp file that print accumolate styles/colors totals.

SELECT (lcTmpFile)
IF !llRpPack
  llCheck = SEEK(lcStyle,lcTmpFile)  && if found the same style in the file.
ELSE
  *--modify it as Alaa Ask me
  IF llRpPrTCon
    llCheck = SEEK(lcStyle +&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion ,lcTmpFile)  && if found the same style in the file.
  ELSE
    llCheck = SEEK(lcStyle +&lcTempOrd..Store +&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,lcTmpFile)  && if found the same style in the file.
  ENDIF
ENDIF
IF llCheck
  
  *B607387,1 SSE 29/06/2003 Fix Numeric overflow. [Begin]
  *lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)>0,(&lcTempOrd..TotQty + nTotQty),1)
  lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)<>0,(&lcTempOrd..TotQty + nTotQty),1)
  *B607387,1 SSE 29/06/2003 Fix Numeric overflow. [End]

  REPLACE nTotQty WITH nTotQty + &lcTempOrd..TotQty
  IF llRpPrTCon 
    REPLACE  Nqty1 WITH &lcTempOrd..Qty1+Nqty1;
             Nqty2 WITH &lcTempOrd..Qty2+Nqty2;
             Nqty3 WITH &lcTempOrd..Qty3+Nqty3;
             Nqty4 WITH &lcTempOrd..Qty4+Nqty4;
             Nqty5 WITH &lcTempOrd..Qty5+Nqty5;
             Nqty6 WITH &lcTempOrd..Qty6+Nqty6;
             Nqty7 WITH &lcTempOrd..Qty7+Nqty7;
             Nqty8 WITH &lcTempOrd..Qty8+Nqty8
   ENDIF          
  

  REPLACE  nPrice  WITH lnAvgPrice
  IF !SEEK(lcStyle +&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,lcTmpHdr)
      
    SELECT (lcTmpHdr)
    APPEND BLANK
    IF !llRpPack
      REPLACE Style      WITH &lcTempOrd..Style          ,;
              nPrice     WITH &lcTempOrd..Price          ,;
              cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
              nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
              nTotQty    WITH &lcTempOrd..TotQty  
    ELSE
        REPLACE Style      WITH &lcTempOrd..Style          ,;
                nPrice     WITH &lcTempOrd..Price          ,;
                cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
                nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
                Pack_id    WITH &lcTempOrd..Pack_id ,;
                nTotQty    WITH &lcTempOrd..TotQty
       REPLACE cpkcolor   WITH &lcTempOrd..cpkcolor ,; 
               cpcksize   WITH &lcTempOrd..cpcksize ,;
               cPkVersion WITH &lcTempOrd..cPkVersion,;
               Note_Mem   WITH &lcTempOrd..Note_Mem

    ENDIF
  ELSE
    SELECT (lcTmpHdr)
    
    *B607387,1 SSE 29/06/2003 Fix Numeric overflow. [Begin]
    *lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)>0,(&lcTempOrd..TotQty + nTotQty),1)
    lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)<>0,(&lcTempOrd..TotQty + nTotQty),1)
    *B607387,1 SSE 29/06/2003 Fix Numeric overflow. [End]
    
    REPLACE nTotQty WITH nTotQty + &lcTempOrd..TotQty,;
            nPrice  WITH lnAvgPrice
  ENDIF
ELSE  && else if didn't found the style in the file (New Style).
  IF !SEEK(lcStyle +&lcTempOrd..Pack_id +&lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion,lcTmpHdr)
    SELECT (lcTmpHdr)
    APPEND BLANK
    IF !llRpPack
      REPLACE Style      WITH &lcTempOrd..Style          ,;
              nPrice     WITH &lcTempOrd..Price          ,;
              cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
              nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
              nTotQty    WITH &lcTempOrd..TotQty         
    ELSE
      REPLACE Style      WITH &lcTempOrd..Style          ,;
              nPrice     WITH &lcTempOrd..Price          ,;
              cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
              nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
              Pack_id    WITH &lcTempOrd..Pack_id ,;
              nTotQty    WITH &lcTempOrd..TotQty         

       REPLACE cpkcolor   WITH &lcTempOrd..cpkcolor ,; 
               cpcksize   WITH &lcTempOrd..cpcksize ,;
               cPkVersion WITH &lcTempOrd..cPkVersion,;
               Note_Mem   WITH &lcTempOrd..Note_Mem


    ENDIF
  ELSE
    SELECT (lcTmpHdr)
    
    *B607387,1 SSE 29/06/2003 Fix Numeric overflow. [Begin]
    *lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)>0,(&lcTempOrd..TotQty + nTotQty),1)
    lnAvgPrice  = (nPrice * nTotQty +  &lcTempOrd..TotQty * &lcTempOrd..Price)/IIF((&lcTempOrd..TotQty + nTotQty)<>0,(&lcTempOrd..TotQty + nTotQty),1)
    *B607387,1 SSE 29/06/2003 Fix Numeric overflow. [End]
    
    REPLACE nTotQty WITH nTotQty + &lcTempOrd..TotQty,;
            nPrice  WITH lnAvgPrice
  ENDIF

  SELECT (lcTmpFile)
  APPEND BLANK

  IF !llRpPack
    REPLACE Style      WITH &lcTempOrd..Style          ,;
            nPrice     WITH &lcTempOrd..Price          ,;
            cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
            nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
            Pack       WITH lfGetPack()                ,;
            nTotQty    WITH &lcTempOrd..TotQty         ,;
            Store      WITH &lcTempOrd..Store            
  ELSE
    REPLACE Style      WITH &lcTempOrd..Style          ,;
            nPrice     WITH &lcTempOrd..Price          ,;
            cDesc      WITH SUBSTR(Style.Desc,1,19)    ,;
            nSugRetPri WITH &lcTempOrd..nSugRetPri     ,;
            Pack       WITH lfGetPack() ,;
            Pack_id    WITH &lcTempOrd..Pack_id        ,;
            nTotQty    WITH &lcTempOrd..TotQty         ,;
            Store      WITH &lcTempOrd..Store

    REPLACE cpkcolor   WITH &lcTempOrd..cpkcolor ,; 
            cpcksize   WITH &lcTempOrd..cpcksize ,;
            cPkVersion WITH &lcTempOrd..cPkVersion 

  ENDIF
  REPLACE  Nqty1 WITH &lcTempOrd..Qty1;
           Nqty2 WITH &lcTempOrd..Qty2;
           Nqty3 WITH &lcTempOrd..Qty3;
           Nqty4 WITH &lcTempOrd..Qty4;
           Nqty5 WITH &lcTempOrd..Qty5;
           Nqty6 WITH &lcTempOrd..Qty6;
           Nqty7 WITH &lcTempOrd..Qty7;
           Nqty8 WITH &lcTempOrd..Qty8
  
ENDIF
LOCATE
SELECT(lnAlias)

*-- End Of  lfAddRec.

*!***************************************************************************
*! Name      : lfPrGrdTot
*! Developer : Mohamed Shokry
*! Date      : 02/17/2002
*! Purpose   : Function to print the grand totals
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfPrGrdTot()
*!***************************************************************************
**-
FUNCTION lfPrGrdTot
PRIVATE lnAlias

lnAlias = SELECT(0)
IF lcMulti = 'Y'
  IF llRpPrTCon  &&  Print consolidating all stores by Style/Color.
    SELECT (lcTmphdr)
    SCAN 
      IF ROW > MAXROW
        =lfPrnConti()
        =lfPrnHdr()
      ENDIF
      @ ROW,00 SAY Style
      @ ROW,20 SAY cDesc
      @ ROW,40 SAY nSugRetPri PICTURE '999.99' 
      @ ROW,47 SAY nPrice PICTURE '999,999.99'
      @ ROW,60 SAY nTotQty PICTURE '999999'
      lnAmount = nTotQty * nPrice
      lnTotORd = lnTotORd + lnAmount
      lnTotQty = lnTotQty + nTotQty
      @ ROW,67 SAY lnAmount PICTURE '99,999,999.99'
      ROW = ROW + 1
      IF ROW > MAXROW
        =lfPrnConti()
        =lfPrnHdr()
      ENDIF

      IF !llRpPack
        =SEEK(STYLE,lcTmpFile)
      ELSE
        =SEEK(STYLE+Pack_Id +cPkColor+cPckSize+ cPkVersion,lcTmpFile)
      ENDIF

      SELECT (lcTmpFile)

      IF llRpPack 
        lcEVExp = "STYLE+Pack_Id +cPkColor+cPckSize+ cPkVersion = &lcTmpHdr..Style+&lcTmpHdr..Pack_Id +&lcTmpHdr..cPkColor+&lcTmpHdr..cPckSize+ &lcTmpHdr..cPkVersion"
      ELSE  
        lcEVExp = "STYLE = &lcTmpHdr..Style"
      ENDIF

      SCAN REST WHILE &lcEVExp
        =lfPrtpack(ALLT(Pack),STORE)
      ENDSCAN 
      IF !EMPTY(ORDHDR.Note1) .AND. SUBSTR(ALLTRIM(ORDHDR.Note1),1,1) <> '*'      
        @ ROW,27 SAY  ALLTRIM(ORDHDR.Note1)

        IF MEMLINES(&lcTmphdr..Note_Mem) <> 0 .AND. EMPTY(ORDHDR.Note2)
          ROW = ROW + 1
        ENDIF

      ENDIF        
      IF !EMPTY(ORDHDR.Note2) .AND. SUBSTR(ALLTRIM(ORDHDR.Note2),1,1) <> '*'
        @ ROW,58 SAY SUBSTR(ALLTRIM(ORDHDR.Note2),1,22)

        IF MEMLINES(&lcTmphdr..Note_Mem) <> 0 
          ROW = ROW + 1
        ENDIF

      ENDIF

      SELECT(lcTmphdr)     
      IF MEMLINES(Note_Mem) <> 0 AND llRpOrdLNt 
       *-- Print order notepade lines.
        =lfPrtOnte()
      ENDIF  

      ROW = ROW +2
    ENDSCAN
    *-- Function to print the order notepad 
    ROW = ROW + 1
    =lfPrnNotPd()
    ROW = ROW + 1
    =lfPrnLast()
    SELECT (lcTmpFile)
    ZAP
    SELECT (lcTmphdr)
    ZAP
  ENDIF
  NEWDOC = .T.
ENDIF

SELECT(lnAlias)
*-- End Of lfPrGrdTot.

*!*************************************************************
*! Name      : lfPrntPack
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/16/2002
*! Purpose   : Function to print Pack_id without details.
*!*************************************************************
*! Calls     : lfGetPack()
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfPrntPack()
*!*************************************************************
*! Due to C200343
*!*************************************************************
*--This function to modify the way we get UPC and SKU
FUNCTION lfPrntPack
PRIVATE lcPck_id , lnTotlQty , lnPrice , lnSugRet , lnAvgAmt , lnTotAmnt
STORE 0 TO lnTotlQty , lnPrice , lnSugRet , lnAvgAmt , lnTotAmnt
PRIVATE llScanLop,lcPackUPC,lcPackSKU,lnAlias
STORE .F. TO llScanLop
STORE '' TO lcPackUPC,lcPackSKU

lnAlias = SELECT(0)

SELECT (lcTmpPack)
ZAP

SELECT (lcTempOrd)
lnRecTo = RECNO()

SCAN FOR cOrdType+Order = ORDHDR.cOrdType+lcOrder AND IIF(!llRpPrTCon, lcStore=Store,.T.)
  IF !EMPTY(PACK_ID)
    lcPack = lfGetPack()
    lcPack2 = STRTRAN(lcPack,SUBSTR(lcPack,1,AT('|',lcPack)),'') && then remove this line from string

    *B607387,1 Prevent removing Pack SKU# from the Pack String. [Begin]
    *IF "PACK UPC#" $ SUBSTR(lcPack2,1,AT('|',lcPack2)-1)
    *  lcPackUPC = SUBSTR(lcPack2,1,AT('|',lcPack2)-1)
    *  lcPack3 = STRTRAN(lcPack2,SUBSTR(lcPack2,1,AT('|',lcPack2)),'') && then remove this line from string
    *  IF "PACK SKU #" $ SUBSTR(lcPack3,1,AT('|',lcPack3)-1)
    *    lcPackSKU = SUBSTR(lcPack3,1,AT('|',lcPack3)-1)
    *  ELSE
    *    lcPackSKU = ""
    *  ENDIF
    IF "PACK UPC#" $ lcPack2
      IF "PACK UPC#" $ SUBSTR(lcPack2,1,AT('|',lcPack2)-1)
        lcPackUPC = SUBSTR(lcPack2,1,AT('|',lcPack2)-1)
        lcPackSKU = STRTRAN(lcPack2,SUBSTR(lcPack2,1,AT('|',lcPack2)),'')
      ELSE
        lcPackUPC = lcPack2
      ENDIF
    *B607387,1 Prevent removing Pack SKU# from the Pack String. [End]

    ELSE  

      *B607387,1 Prevent removing Pack SKU# from the Pack String. [Begin]
      *IF "PACK SKU #" $ SUBSTR(lcPack2,1,AT('|',lcPack2)-1)
      *  lcPackSKU = SUBSTR(lcPack2,1,AT('|',lcPack2)-1) && then remove this line from string
      *ELSE
      *  lcPackSKU = ""
      *ENDIF
      IF "PACK SKU #" $ lcPack2
        IF "PACK SKU #" $ SUBSTR(lcPack2,1,AT('|',lcPack2)-1)
          lcPackSKU = SUBSTR(lcPack2,1,AT('|',lcPack2)-1)
        ELSE 
          lcPackSKU = lcPack2
        ENDIF  
      ENDIF
      *B607387,1 Prevent removing Pack SKU# from the Pack String. [End]

      lcPackUPC =""
    ENDIF    
    *B607387,1 Prevent removing Pack SKU# from the Pack String. [End]

    lcPck_id = SUBSTR(lcPack,10,16)
    lnCountr = 0
    lnLastRec = 0
    llCheckPck = .T.

    SCAN FOR PACK_ID=lcPck_id AND IIF(!llRpPrTCon, lcStore=Store,.T.) AND(cOrdType+Order = ORDHDR.cOrdType+lcOrder)
      IF lcStore<>Store AND llRpPrTCon
        llCheckPck = .T.
        lcStore = Store        
      ENDIF
      SCATTER MEMVAR MEMO
      IF llCheckPck = .T.
        IF llRpPrTCon
          llSeek = SEEK(Pack_Id+cPkColor+cPckSize+cPkVersion,lcTmpPack)
        ELSE
          llSeek = SEEK(Pack_Id+cPkColor+cPckSize+cPkVersion+Store,lcTmpPack)
        ENDIF
        IF llSeek 
          *C200479,1 HBG 16/01/2003 If ordLine.range OR ordLine.price per piece OR 
          *C200479,1                ordLine.nPkSlPirce = 0 then calculate the price [Begin]
          *REPLACE &lcTmpPack..nTotQty WITH &lcTmpPack..nTotQty + lfGetPkQty()
          *hbg
          *REPLACE &lcTmpPack..nPackQty WITH &lcTmpPack..nPackQty + lfGetPkQty()
          IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
            REPLACE &lcTmpPack..nPackQty WITH &lcTmpPack..nPackQty + &lcTempOrd..TotQty
          ELSE
            REPLACE &lcTmpPack..nPackQty WITH &lcTmpPack..nPackQty + lfGetPkQty()
          ENDIF
          *hbg
          
          IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
            *hbg
            *REPLACE &lcTmpPack..nPackPrc WITH &lcTmpPack..nPackPrc + &lcTempOrd..price 
            REPLACE &lcTmpPack..nPackPrc WITH &lcTmpPack..nPackPrc +;
                                             (&lcTempOrd..TotQty*&lcTempOrd..price ) 
            *hbg
          ENDIF  
          REPLACE &lcTmpPack..nCount  WITH &lcTmpPack..nCount + 1,;
                  &lcTmpPack..nTotQty WITH &lcTmpPack..nTotQty + &lcTempOrd..TotQty
          *C200479,1 [End]
        ELSE
          *C200479,1 HBG 16/01/2003 Add fields to Apply the check If ordLine.range OR ordLine.price 
          *C200479,1                per piece OR ordLine.nPkSlPirce = 0 in calculating the amount [Begin]
          *INSERT INTO (lcTmpPack) (pack_id,cPkColor,cPckSize,cPkVersion,Store,nTotQty,nPackPrc,PackUPC,PackSKU,Note_Mem) VALUES ;
                               (lcPck_id,&lcTempOrd..cPkColor,&lcTempOrd..cPckSize,&lcTempOrd..cPkVersion,;
                               IIF(llRpPrTCon,'',&lcTempOrd..Store),lfGetPkQty(),&lcTempOrd..nPkSlPrice,lcPackUPC,lcPackSKU,m.Note_Mem)
          *hbg
          *INSERT INTO (lcTmpPack) (pack_id,cPkColor,cPckSize,cPkVersion,Store,nTotQty,PackUPC,;
                                   PackSKU,Note_Mem,lRange,lPckPrPiec,nPkSlPrice,nPackQty) VALUES ;
                               (lcPck_id,&lcTempOrd..cPkColor,&lcTempOrd..cPckSize,&lcTempOrd..cPkVersion,;
                               IIF(llRpPrTCon,'',&lcTempOrd..Store),&lcTempOrd..TotQty,lcPackUPC,lcPackSKU,m.Note_Mem,;
                               &lcTempOrd..lRange,&lcTempOrd..lPckPrPiec,&lcTempOrd..nPkSlPrice,lfGetPkQty())
          INSERT INTO (lcTmpPack) (pack_id,cPkColor,cPckSize,cPkVersion,Store,nTotQty,PackUPC,;
                                   PackSKU,Note_Mem,lRange,lPckPrPiec,nPkSlPrice) VALUES ;
                               (lcPck_id,&lcTempOrd..cPkColor,&lcTempOrd..cPckSize,&lcTempOrd..cPkVersion,;
                               IIF(llRpPrTCon,'',&lcTempOrd..Store),&lcTempOrd..TotQty,lcPackUPC,lcPackSKU,m.Note_Mem,;
                               &lcTempOrd..lRange,&lcTempOrd..lPckPrPiec,&lcTempOrd..nPkSlPrice)
          *hbg                                          
          IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
            *hbg
            *REPLACE &lcTmpPack..nPackPrc WITH &lcTempOrd..price 
            REPLACE &lcTmpPack..nPackPrc WITH (&lcTempOrd..TotQty*&lcTempOrd..price)
            REPLACE &lcTmpPack..nPackQty WITH &lcTempOrd..TotQty            
            *hbg
          ELSE
            *hbg
            REPLACE &lcTmpPack..nPackQty WITH lfGetPkQty()
            *hbg
            REPLACE &lcTmpPack..nPackSlPrc WITH &lcTempOrd..nPkSlPrice  
            
            *B607387,1 SSE 07/30/2003 Add the Pack price. [Begin]
            REPLACE &lcTmpPack..nPackPrc WITH (&lcTempOrd..TotQty*&lcTempOrd..price)
            *B607387,1 SSE 07/30/2003 Add the Pack price. [End]

          ENDIF
          REPLACE &lcTmpPack..nCount WITH 1
          *C200479,1 [End]
        ENDIF
        llCheckPck = .F.
      ELSE 
        *C200479,1 HBG 16/01/2003 If ordLine.range OR ordLine.price per piece OR 
        *C200479,1                ordLine.nPkSlPirce = 0 then calculate the price [Begin]
        IF llRpPrTCon
          llSeek = SEEK(Pack_Id+cPkColor+cPckSize+cPkVersion,lcTmpPack)
        ELSE
          llSeek = SEEK(Pack_Id+cPkColor+cPckSize+cPkVersion+Store,lcTmpPack)
        ENDIF
        IF llSeek 
          IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
            *hbg
            *REPLACE &lcTmpPack..nPackPrc WITH &lcTmpPack..nPackPrc + &lcTempOrd..price 
            REPLACE &lcTmpPack..nPackPrc WITH &lcTmpPack..nPackPrc +;
                                             (&lcTempOrd..TotQty*&lcTempOrd..price ) 
            REPLACE &lcTmpPack..nPackQty WITH &lcTmpPack..nPackQty + &lcTempOrd..TotQty
            *hbg                                 
          ENDIF

          *B607387,1 SSE 07/30/2003 Add the Pack price. [Begin]
          *REPLACE &lcTmpPack..nPackPrc WITH &lcTmpPack..nPackPrc +;
          *                                 (&lcTempOrd..TotQty*&lcTempOrd..price)
          *B607387,1 SSE 07/30/2003 Add the Pack price. [End]
            
          REPLACE &lcTmpPack..nCount  WITH &lcTmpPack..nCount + 1,;
                  &lcTmpPack..nTotQty WITH &lcTmpPack..nTotQty + &lcTempOrd..TotQty

        *B123013,1 NNA 05/31/2004 (Begin) in case Pack has more color we Accumulate by Style-color...etc
        ELSE
          INSERT INTO (lcTmpPack) (pack_id,cPkColor,cPckSize,cPkVersion,Store,nTotQty,PackUPC,;
                                   PackSKU,Note_Mem,lRange,lPckPrPiec,nPkSlPrice) VALUES ;
                               (lcPck_id,&lcTempOrd..cPkColor,&lcTempOrd..cPckSize,&lcTempOrd..cPkVersion,;
                               IIF(llRpPrTCon,'',&lcTempOrd..Store),&lcTempOrd..TotQty,lcPackUPC,lcPackSKU,m.Note_Mem,;
                               &lcTempOrd..lRange,&lcTempOrd..lPckPrPiec,&lcTempOrd..nPkSlPrice)
          IF &lcTempOrd..lRange OR &lcTempOrd..lPckPrPiec OR &lcTempOrd..nPkSlPrice = 0
            REPLACE &lcTmpPack..nPackPrc WITH (&lcTempOrd..TotQty*&lcTempOrd..price)
            REPLACE &lcTmpPack..nPackQty WITH &lcTempOrd..TotQty            
          ELSE
            REPLACE &lcTmpPack..nPackQty WITH lfGetPkQty()
            REPLACE &lcTmpPack..nPackSlPrc WITH &lcTempOrd..nPkSlPrice  
            REPLACE &lcTmpPack..nPackPrc WITH (&lcTempOrd..TotQty*&lcTempOrd..price)
          ENDIF
          REPLACE &lcTmpPack..nCount WITH 1
        *B123013,1 NNA (End)
        ENDIF  
        *C200479,1 [End]
      ENDIF
      llScanLop = .T.
      lnCountr =RECNO()
      lnLastRec = RECNO()

    ENDSCAN
    IF llScanLop
      IF !llRpPrTCon
        IF BETWEEN(lnLastRec,1,RECCOUNT())
          GOTO lnLastRec 
        ENDIF
      ELSE
        IF EOF()
          GOTO lnCountr
        ELSE
          GOTO lnCountr+1
        ENDIF
        
      ENDIF  
    ENDIF
   
    lnPrice = lnTotAmnt / lnTotlQty
    lntotord = lntotord + lnTotAmnt
    lntotqty = lntotqty + lntotlqty

    STORE 0 TO lnTotlQty , lnPrice , lnAvgAmt , lnTotAmnt
    llScanLop = .F.
  ENDIF
ENDSCAN
IF !llRpPrTCon 
  IF EOF()
    GOTO lnRecTo 
    SCAN REST WHILE cOrdType+Order = ORDHDR.cOrdType+lcOrder AND lcStore=Store
    ENDSCAN
    SKIP-1
  ENDIF
ENDIF  
SELECT (lcTmpPack)
*C200479,1 HBG 16/01/2003 If ordLine.range OR ordLine.price per piece OR 
*C200479,1                ordLine.nPkSlPirce = 0 then calculate the price [Begin]
SCAN
 IF &lcTmpPack..lRange OR &lcTmpPack..lPckPrPiec OR &lcTmpPack..nPkSlPrice = 0
   *hbg
   *REPLACE &lcTmpPack..nPackSlPrc WITH (&lcTmpPack..nPackPrc / &lcTmpPack..nCount) * (&lcTmpPack..nTotQty/&lcTmpPack..nPackQty)
   
   *B119649,1 MHM 07/09/2003 to privent Numiric Over Fllow[Start] 
   *REPLACE &lcTmpPack..nPackSlPrc WITH (&lcTmpPack..nPackPrc / &lcTmpPack..nTotQty)
   REPLACE &lcTmpPack..nPackSlPrc WITH &lcTmpPack..nPackPrc / IIF(&lcTmpPack..nTotQty=0,1,&lcTmpPack..nTotQty)
   *B119649,1 MHM 07/09/2003 to privent Numiric Over Fllow[Start] 
   
   *B607387,1 Correct the Gross Amount value. [Begin]
   *REPLACE &lcTmpPack..nAmount WITH &lcTmpPack..nPackSlPrc * &lcTmpPack..nTotQty
   REPLACE &lcTmpPack..nAmount WITH &lcTmpPack..nPackPrc
   *B607387,1 Correct the Gross Amount value. [End]
   
 ELSE
   REPLACE &lcTmpPack..nAmount WITH &lcTmpPack..nPackSlPrc * &lcTmpPack..nPackQty  
   *hbg
 ENDIF
 
ENDSCAN
LOCATE
*C200479,1 [End]

SCAN
  *--calculate totales

  *B607387,1 Correct the Gross Amount value. [Begin]
  *lntotord = lntotord + nTotQty*nPackPrc
  IF &lcTmpPack..lRange OR &lcTmpPack..lPckPrPiec OR &lcTmpPack..nPkSlPrice = 0
    lntotord = lntotord + nPackPrc
  ELSE
    lntotord = lntotord + nAmount
  ENDIF
  *B607387,1 Correct the Gross Amount value. [End]

  lntotqty = lntotqty + nTotQty
   
  @ ROW ,0  SAY "PACK # : "+Pack_Id+'-'+cPkColor+'-'+lfGetGmSz(cPckSize)+'-'+cPkVersion
  *C200479,1 HBG 16/01/2003 Print the calculated Price and Pack Qty and Amount [Begin]
  *@ ROW ,47 SAY nPackPrc                  PICTURE '99,999.99'
  *@ ROW ,60 SAY nTotQty                   PICTURE '999999'
  *@ ROW ,67 SAY nTotQty*nPackPrc         PICTURE '99,999,999.99'
  @ ROW ,47 SAY nPackSlPrc   PICTURE '99,999.99'
  @ ROW ,60 SAY nPackQty     PICTURE '999999'
  @ ROW ,67 SAY nAmount      PICTURE '99,999,999.99'
  *C200479,1 [End]
  ROW = ROW + 1

  IF ROW >= MAXROW
    =lfPrnConti()
    =lfPrnHdr()
  ENDIF
      
  *hbg
  @ ROW ,0  SAY IIF(lPckPrPiec,'Price / Piece','Price / Pack')
  ROW = ROW + 1
  IF ROW >= MAXROW
    =lfPrnConti()
    =lfPrnHdr()
  ENDIF
  *hbg
  
  IF !EMPTY(PackUPC) AND OCCURS('UPC',PackUPC)>0
    @ ROW ,0  SAY PackUPC
    ROW = ROW + 1
    IF ROW >= MAXROW
      =lfPrnConti()
      =lfPrnHdr()
    ENDIF
  ENDIF  

  IF !EMPTY(PackSKU) AND OCCURS('SKU',PackSKU)>0
    @ ROW ,0  SAY PackSKU
    ROW = ROW + 1
    IF ROW >= MAXROW
      =lfPrnConti()
      =lfPrnHdr()
    ENDIF
  ENDIF  

  IF MEMLINES(Note_Mem) <> 0 AND llRpOrdLNt
    SCATTER MEMVAR MEMO
    *-- Print order notepade lines.
    =lfPrtOnte()
  ENDIF  
ENDSCAN
ROW = ROW + 1
SELECT(lnAlias)
SELECT (lcTempOrd)

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/09/2001
*! Purpose   : Function To get Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
*
FUNCTION lfGetGmSz
PARAMETER lcPackSize

PRIVATE lcNombr,lnScalePos
IF !EMPTY(lcPackSize)
  *--svae Scale Positon
  lnScalePos = RECNO('SCALE')
  =SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcNombr = RIGHT(lcPackSize,1)
  lcSize=EVAL('SCALE.SZ'+lcNombr)
  IF BETWEEN(lnScalePos,1,RECCOUNT('SCALE'))
    GOTO lnScalePos In SCALE
  ENDIF
  
ELSE
  lcSize ='*****'
ENDIF  

RETURN lcSize 

*!*************************************************************
*! Name      : lfGetPkQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/09/2001
*! Purpose   : Function To get Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetPkQty()
*!*************************************************************
*
FUNCTION lfGetPkQty

*--accourding to new modification we change getting No Of Packs

SELECT (lcTempOrd)
lnPackQty  = npackNo

RETURN lnPackQty


*--modify getpack function 
*!***************************************************************************
*! Name      : lfGetPack
*! Developer : ABDOU ELGENDI
*! Date      : 05/04/2000
*! Purpose   : Update the field TMPFILE.pack with either pack_id from ORDLINE
*!           : file or the sku
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetPack()
*!***************************************************************************
*
FUNCTION lfGetPack

PRIVATE lcOrderset ,lcPackId
PRIVATE lnAlias
lnAlias = SELECT()

Store '' TO lcOrderset , lcPackId
*--to get pack id And PackUPC AND SKU

IF llRpPack AND !EMPTY(&lcTempOrd..Pack_id)  && If not empty tempfile Get the Pack_Id from it.

  lcPackId = "PACK # : "+ &lcTempOrd..Pack_id+'-'+ &lcTempOrd..cpkcolor+'-'+lfGetGmSz(&lcTempOrd..cpcksize)+'-'+&lcTempOrd..cPkVersion  
  *--replace in new table for it
  SELECT (lcTmpDesc)
  
  IF !SEEK(&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+&lcTempOrd..Style)
    APPEND BLANK
    REPLACE Pack_id    WITH &lcTempOrd..Pack_id   ,;
            cpkcolor   WITH &lcTempOrd..cpkcolor  ,;
            cpcksize   WITH &lcTempOrd..cpcksize  ,;
            cPkVersion WITH &lcTempOrd..cPkVersion,;
            Style      WITH &lcTempOrd..Style
  ENDIF
  SELECT (lnAlias)
  PRIVATE lcStyUpcOrd
  lcStyUpcOrd = ORDER("STYLEUPC")
  SET ORDER TO TAG Gmapkupc IN STYLEUPC
  IF SEEK(&lcTempOrd..Account+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cpkcolor+&lcTempOrd..cPcksize,"STYLEUPC") OR;
     SEEK("*****"+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cpkcolor+&lcTempOrd..cPcksize,"STYLEUPC") 
    IF !EMPTY(lcStyUpcOrd) 
      SET ORDER TO TAG (lcStyUpcOrd) IN STYLEUPC
    ENDIF  

    lcPackId = lcPackId + SPACE(2) +'|'+ "PACK UPC#: " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
    SELECT (lcTmpDesc)
    REPLACE PackUPC WITH STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3
    SELECT (lnAlias)

  ENDIF
  IF !EMPTY(lcStyUpcOrd) 
    SET ORDER TO TAG (lcStyUpcOrd) IN STYLEUPC
  ENDIF  
  
  PRIVATE lcSpklnOrd
  lcSpklnOrd = ORDER("SPCK_LIN")
  SET ORDER TO TAG Spck_linst IN SPCK_LIN
  IF llRpPack AND (SEEK("S"+&lcTempOrd..Account+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cPkColor+;
                       &lcTempOrd..cPckSize,"SPCK_LIN") OR  ;
                  SEEK("S"+"*****"+PADR(&lcTempOrd..Pack_id,19)+&lcTempOrd..cPkColor+;
                       &lcTempOrd..cPckSize,"SPCK_LIN"))
    lcPackId = IIF(EMPTY(lcPackId),"PACK SKU # : " + SPCK_LIN.Pack_id,;
                      lcPackId+'|'+"PACK SKU # : " + SPCK_LIN.Pack_id)
    SELECT (lcTmpDesc)
    REPLACE PackSKU WITH SPCK_LIN.Pack_id
    SELECT (lnAlias)
  ENDIF
  IF !EMPTY(lcSpklnOrd) 
    SET ORDER TO TAG (lcSpklnOrd) IN SPCK_LIN
  ENDIF  
    
ENDIF

lcOrderset = ORDER("SPCK_LIN")
SET ORDER TO TAG SPCKLINS IN SPCK_LIN

IF SEEK(&lcTempOrd..Style,"STYLEUPC")
  IF llRpPack
    SELECT (lcTmpDesc)
    IF !SEEK(&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+&lcTempOrd..Style)
      APPEND BLANK
      REPLACE Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion,;
              Style      WITH &lcTempOrd..Style
    ENDIF
  ELSE
    SELECT (lcTmpDesc)
    IF !SEEK(&lcTempOrd..Style)
      APPEND BLANK
      REPLACE Style    WITH &lcTempOrd..Style
    ENDIF
  ENDIF  

  FOR lnNum =1 TO 8
    lcNum = STR(lnNum,1)
    IF !EMPTY(&lcTempOrd..qty&lcNum) .AND. SEEK(&lcTempOrd..Style+lcNum,"STYLEUPC")
      IF llRpSQty
      
        lcPackId = IIF(EMPTY(lcPackId),"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum+SPACE(2),;
                          lcPackId+'|'+"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum+SPACE(2))
        REPLACE UPC&lcNum   WITH STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3,;
                Size&lcNum  WITH SCALE.SZ&lcNum,;
                llStyUPC    WITH .T.
      ELSE  && No to print sizes Qty.
        *----  Add UPC # to lcPackId like "Pack # line|UPC # upc1 sz1 sz2 sz3 'Qty'  TotalQty"
        lcPackId = IIF(EMPTY(lcPackId),"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum,;
                          lcPackId+'|'+"Style UPC # : " +STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3+SPACE(2)+SCALE.SZ&lcNum)
        REPLACE UPC&lcNum   WITH STYLEUPC.Cupcnum1+STYLEUPC.Cupcnum2+STYLEUPC.Cupcnum3,;
                Size&lcNum  WITH SCALE.SZ&lcNum,;
                llStyUPC    WITH .T.
      ENDIF
    ENDIF
  ENDFOR
ENDIF

IF  SEEK("S"+&lcTempOrd..Account+&lcTempOrd..Style,"SPCK_LIN")  
  IF llRpPack
    SELECT (lcTmpDesc)
    IF !SEEK(&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+&lcTempOrd..Style)
      APPEND BLANK
      REPLACE Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion,;
              Style      WITH &lcTempOrd..Style
    ENDIF
  ELSE
    SELECT (lcTmpDesc)
    IF !SEEK(&lcTempOrd..Style)
      APPEND BLANK
      REPLACE Style    WITH &lcTempOrd..Style
    ENDIF
  ENDIF  
  lnNum = 1
  lcNum = STR(lnNum,1)
  SELECT SPCK_LIN
  SCAN REST WHILE Type+Account+Style+Pack_id = "S"+&lcTempOrd..Account+LEFT(ALLTRIM(&lcTempOrd..Style),LEN((&lcTempOrd..Style))-1)

    IF EMPTY(lcPackId)
      lcPackId = IIF(&lcTempOrd..Qty&lcNum = 0,'',"STYLE SKU # : " + SPCK_LIN.Pack_id+SCALE.SZ&lcNum+SPACE(2))
      FOR lnCount = 1 to 8
        lcCount = STR(lnCount,1)
        IF !EMPTY(SPCK_LIN.QTY&lcCount)
          SELECT (lcTmpDesc)
          REPLACE SKU&lcCount   WITH SPCK_LIN.Pack_id,;
                  Size&lcCount  WITH SCALE.SZ&lcCount,;
                  llStySku      WITH .T.
        ENDIF         
      ENDFOR          
    ELSE
      lcPackId = lcPackId+IIF(&lcTempOrd..Qty&lcNum=0,'','|'+"STYLE SKU # : " + SPCK_LIN.Pack_id+SCALE.SZ&lcNum+SPACE(2))
      FOR lnCount = 1 to 8
        lcCount = STR(lnCount,1)
        IF !EMPTY(SPCK_LIN.QTY&lcCount)
          SELECT (lcTmpDesc)
          REPLACE SKU&lcCount   WITH SPCK_LIN.Pack_id,;
                  Size&lcCount  WITH SCALE.SZ&lcCount,;
                  llStySku      WITH .T.
        ENDIF          
      ENDFOR            
    ENDIF
    lnNum = lnNum +1
    lcNum = STR(lnNum,1)
  ENDSCAN
ENDIF

*--if not found UPC or SKU
IF !SEEK("S"+&lcTempOrd..Account+&lcTempOrd..Style,"SPCK_LIN") AND !SEEK(&lcTempOrd..Style,"STYLEUPC")  
  IF llRpPack
    SELECT (lcTmpDesc)
    IF !SEEK(&lcTempOrd..Pack_id+ &lcTempOrd..cpkcolor+&lcTempOrd..cpcksize+&lcTempOrd..cPkVersion+&lcTempOrd..Style)
      APPEND BLANK
      REPLACE Pack_id    WITH &lcTempOrd..Pack_id ,;
              cpkcolor   WITH &lcTempOrd..cpkcolor,;
              cpcksize   WITH &lcTempOrd..cpcksize,;
              cPkVersion WITH &lcTempOrd..cPkVersion,;
              Style      WITH &lcTempOrd..Style
    ENDIF
  ELSE
    SELECT (lcTmpDesc)
    IF !SEEK(&lcTempOrd..Style)
      APPEND BLANK
      REPLACE Style    WITH &lcTempOrd..Style
    ENDIF
  ENDIF  

  FOR lnNum =1 TO 8
    lcNum = STR(lnNum,1)
    IF !EMPTY(&lcTempOrd..qty&lcNum) 
      REPLACE Size&lcNum  WITH SCALE.SZ&lcNum
    ENDIF
  ENDFOR
ENDIF

IF !EMPTY(lcOrderset)   && restore the ordrer for spck_lin
  SET ORDER TO TAG &lcOrderset IN SPCK_LIN
ENDIF 
SELECT(lnAlias)

RETURN lcPackId 

*-- End Of lfGetPack.

*!***************************************************************************
*! Name      : lfPrtpack
*! Developer : MOHAMED SHOKRY
*! Date      : 02/17/2002
*! Purpose   : Function to print field Pack.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfPrtpack(Pack)
*!***************************************************************************
*
FUNCTION lfPrtpack
PARAMETER lcPack ,lcLocStore

PRIVATE lnUPCOcc ,lnUPCPrnt
STORE 0 TO lnUPCOcc ,lnUPCPrnt

IF !llRpPack
  llFound = SEEK(STYLE,lcTmpDesc)
ELSE  
  llFound = SEEK(Pack_Id +cPkColor+cPckSize+ cPkVersion+STYLE,lcTmpDesc)
ENDIF

IF llFound
  IF llRpPrTPak OR EMPTY(Pack_id) OR !llRpPrTCon 
    IF EMPTY(Pack_id) AND !llRpPrTCon

      @ ROW ,0 SAY "PACK # :"  && print STORE
      @ ROW ,42 SAY LEFT(lcLocStore,6)
      ROW = ROW + 1
      IF ROW >= MAXROW
        =lfPrnConti()
        =lfPrnHdr()
      ENDIF
    ENDIF
    IF !EMPTY(Pack_id)
      @ ROW ,0 SAY "PACK # :"  && print STORE
      @ ROW ,9 SAY Pack_Id +cPkColor + lfGetGmSz(cpcksize)+ cPkVersion  && print STORE
      ROW = ROW + 1
      IF ROW >= MAXROW
        =lfPrnConti()
        =lfPrnHdr()
      ENDIF
      IF !EMPTY(&lcTmpDesc..PackUPC) AND !EMPTY(&lcTmpDesc..PackSKU)
          @ ROW ,0 SAY "PACK UPC # :"  
          @ ROW ,13 SAY &lcTmpDesc..PackUPC
          @ ROW ,30 SAY "PACK SKU # :"  
          @ ROW ,43 SAY &lcTmpDesc..PackSKU
      ENDIF
      IF !EMPTY(&lcTmpDesc..PackUPC) AND EMPTY(&lcTmpDesc..PackSKU)
          @ ROW ,0 SAY "PACK UPC # :"  
          @ ROW ,13 SAY &lcTmpDesc..PackUPC
      ENDIF

      IF EMPTY(&lcTmpDesc..PackUPC) AND !EMPTY(&lcTmpDesc..PackSKU)
          @ ROW ,0 SAY "PACK SKU # :"  
          @ ROW ,13 SAY &lcTmpDesc..PackSKU
      ENDIF
      ROW = ROW + 1
      IF ROW >= MAXROW
        =lfPrnConti()
        =lfPrnHdr()
      ENDIF
    ENDIF  
    *--both SKU and UPC Found
    IF &lcTmpDesc..llStyUPC AND &lcTmpDesc..llStySKU
      FOR lnCount =1 to 8 
        lcCount = STR(lnCount,1)
        IF llRpPrTCon
          llChkQty = IIF(EMPTY(nQty&lcCount),.F.,.T.)
        ELSE
          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
        ENDIF  
        IF llChkQty
          @ ROW ,0 SAY "STYLE UPC # :"  
          @ ROW ,14 SAY &lcTmpDesc..UPC&lcCount 

          IF llRpSQty 
            @ ROW ,32 SAY &lcTmpDesc..Size&lcCount 
            IF llRpPrTCon
              @ ROW ,37 SAY  nQty&lcCount
            ELSE
              @ ROW ,37 SAY  Qty&lcCount
            ENDIF  
          ENDIF  
          
          @ ROW ,45 SAY "STYLE SKU # :"  
          @ ROW ,58 SAY &lcTmpDesc..SKU&lcCount 
          ROW = ROW + 1
          IF ROW >= MAXROW
            =lfPrnConti()
            =lfPrnHdr()
          ENDIF
        ENDIF  
      ENDFOR
    ENDIF

    *--UPC only Found
    IF &lcTmpDesc..llStyUPC AND !&lcTmpDesc..llStySKU
      FOR lnCount =1 to 8 
        lcCount = STR(lnCount,1)
        IF llRpPrTCon
          llChkQty = IIF(EMPTY(nQty&lcCount),.F.,.T.)
        ELSE
          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
        ENDIF  
        IF llChkQty
          @ ROW ,0 SAY "STYLE UPC # :"  
          @ ROW ,14 SAY &lcTmpDesc..UPC&lcCount 

          IF llRpSQty 
            @ ROW ,31 SAY &lcTmpDesc..Size&lcCount 
            IF llRpPrTCon
              @ ROW ,37 SAY  nQty&lcCount
            ELSE
              @ ROW ,37 SAY  Qty&lcCount
            ENDIF  
          ENDIF
          ROW = ROW + 1
          IF ROW >= MAXROW
            =lfPrnConti()
            =lfPrnHdr()
          ENDIF
        ENDIF  
      ENDFOR
    ENDIF

    *--SKU only Found
    IF !&lcTmpDesc..llStyUPC AND &lcTmpDesc..llStySKU
      FOR lnCount =1 to 8 
        lcCount = STR(lnCount,1)
        IF llRpPrTCon
          llChkQty = IIF(EMPTY(nQty&lcCount),.F.,.T.)
        ELSE
          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
        ENDIF  
        IF llChkQty
          @ ROW ,0 SAY "STYLE SKU # :"  
          @ ROW ,14 SAY &lcTmpDesc..SKU&lcCount 

          IF llRpSQty 
            @ ROW ,31 SAY &lcTmpDesc..Size&lcCount 
            IF llRpPrTCon
              @ ROW ,37 SAY  nQty&lcCount
            ELSE
              @ ROW ,37 SAY  Qty&lcCount
            ENDIF  
          ENDIF
          ROW = ROW + 1
          IF ROW >= MAXROW
            =lfPrnConti()
            =lfPrnHdr()
          ENDIF
        ENDIF  
      ENDFOR
    ENDIF

    *--No SKU No UPC print only Size
    IF !&lcTmpDesc..llStyUPC AND !&lcTmpDesc..llStySKU AND llRpSQty
      FOR lnCount =1 to 8 
        lcCount = STR(lnCount,1)
        IF llRpPrTCon
          llChkQty = IIF(EMPTY(nQty&lcCount),.F.,.T.)
        ELSE
          llChkQty = IIF(EMPTY(Qty&lcCount),.F.,.T.)
        ENDIF  
        IF llChkQty
          @ ROW ,31 SAY &lcTmpDesc..Size&lcCount 
          IF llRpPrTCon
            @ ROW ,37 SAY  nQty&lcCount
          ELSE
            @ ROW ,37 SAY  Qty&lcCount
          ENDIF  
          ROW = ROW + 1
          IF ROW >= MAXROW
            =lfPrnConti()
            =lfPrnHdr()
          ENDIF
        ENDIF  
      ENDFOR
    ENDIF

    IF  !llRpPrTPak  AND !EMPTY(Pack_id)
      @ ROW ,42 SAY LEFT(lcLocStore,7) 
      @ ROW ,48 SAY nPkSlPrice                PICTURE '99,999.99'
      lnPakQty=lfGetPkQty()
      @ ROW ,60 SAY lnPakQty                  PICTURE '999999'
      @ ROW ,67 SAY lnPakQty*nPkSlPrice       PICTURE '99,999,999.99'
    ENDIF  
    ROW = ROW + 1
    IF ROW >= MAXROW
      =lfPrnConti()
      =lfPrnHdr()
    ENDIF
  ENDIF
ENDIF

*-- End Of lfPrpack
