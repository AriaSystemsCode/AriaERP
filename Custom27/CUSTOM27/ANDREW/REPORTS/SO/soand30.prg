*:***************************************************************************
*: Program file  : SOAND30.PRG
*: Program desc. : Customized Order Detail Report for Andrew Mark.
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*:***************************************************************************
*: Example : DO Sorddet
*:***************************************************************************
*: C101388,1 KHM 02/01/99
*:***************************************************************************
*E301249,1 IHB 06/16/1999 Make priority field C(3) in CUSTOMER 
*E301249,1 				  and ORDHDR files
*C101709,1 ADEL 22/11/1999 Add 'Bulk orders only' to the OG and do some modifications
*C101709,1                 to the subtotals. 
*:***************************************************************************

IF lcRpSelBy <> 'E'
  DO CASE
    CASE lcRpSelBy = 'A'
      lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
      lcSelcFile = laOGFxFlt[lnAccPos,6] 
    CASE lcRpSelBy = 'S'
      lnStyPos   = lfItmPos('STYLE.STYLE')
      lcSelcFile = laOGFxFlt[lnStyPos,6] 
    CASE lcRpSelBy = 'F'
      lnFabPos   = lfItmPos('FABRIC.FABRIC')
      lcSelcFile = laOGFxFlt[lnFabPos,6] 
  ENDCASE
  IF EMPTY(lcSelcFile) OR !USED(lcSelcFile) OR RECCOUNT(lcSelcFile) = 0
    =gfModalGen('TRM00052B00000','DIALOG' )
    SET DEVICE TO SCREEN
    RETURN(.F.)
  ELSE
    lcRpExp  = ALLTRIM(lcRpExp)
    lnStrPos = ATC("SEEK",lcRpExp)
    IF lnStrPos > 0
      lnEndPos = ATC(")",lcRpExp)
      lcRpExp  = STRTRAN(lcRpExp,SUBSTR(lcRpExp,lnStrPos ,lnEndPos),'')
      lcRpExp  = ALLTRIM(lcRpExp)
      IF !EMPTY(lcRpExp)
        lnFrstPos = ATC("AND",lcRpExp)
        IF lnFrstPos > 0
          lcRpExp   = STRTRAN(lcRpExp,SUBSTR(lcRpExp,lnFrstPos,3),'')
          lcRpExp  = ALLTRIM(lcRpExp)
        ENDIF
      ENDIF      
    ENDIF
    IF EMPTY(lcRpExp)
      lcRpExp = ".T."
    ENDIF
  ENDIF
ENDIF


*-- Initializing the necessary variables.
STORE SPACE(12) TO lcLStyRang,lcHStyRang
STORE {} TO ldLCanDate,ldHCanDate
lcGroup    = ""
TempFile   = gfTempName()
TempWork   = gfTempName()
lcTempCur  = gfTempName()

*-- To get the position of the style group option in the OG, then get the 
*-- contents of the selections.
lnOptionNo = ASCAN(laOgFxFlt,"STYLE.CSTYGROUP")
IF lnOptionNo > 0
  lnStatPos  = ASUBSCRIPT(laOgFxFlt,lnOptionNo,1)
  lcGroup    = ALLTRIM(laOgFxFlt[lnStatPos,6])
ENDIF

*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))

*-- To get all the order lines that have TotQty greater than 0
lcRpExp  = lcRpExp + " AND TotQty > 0"

*-- If prints lines in coordinate groups then added to the filter
IF lcRpCoorGr = 'Y'
  lcRpExp  = lcRpExp + ".AND. !EMPTY(Group)"
ENDIF
*C101709,1 (Begin) Get all bulk orders if the user wants that.
IF lcRpBulk
  lcRpExp  = lcRpExp + " .AND. ORDHDR.BULK = 'Y' "
ENDIF
*C101709,1 (End)

*-- Copying the structure of the ordline file to the temporary file.
SELECT OrdLine
COPY STRUCTURE TO &gcWorkDir.&TempFile
= gfOpenFile (gcWorkDir+TempFile,' ','EX')
INDEX ON Style+DTOS(OrdHdr->Complete)+Order+STR(LineNo,6) TAG (TempFile)

*-- lfPreParTm() is used to check the validity of the style,account
*-- and fabric.
*-- lfBldTemp() is used to build the temporary file that will be used in printing.
*-- lfPrnReprt() is used to print the report.
IF lfPreParTm() AND lfBldTemp()
  *-- To print the report
  =lfPrnReprt()
ENDIF  

*!*************************************************************
*! Name      : lfPreParTm
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/03/1999
*! Purpose   : To check the validity of the style,account and fabric.
*!*************************************************************
*! Example   :  =lfPreParTm()
*!*************************************************************
FUNCTION lfPreParTm

DO CASE
 *--Selecting records by Account
 CASE lcRpSelBy ='A'
   SELECT OrdHdr
   SET ORDER TO TAG OrdAcct
 
   SELECT OrdLine
   SET ORDER TO TAG OrdLine
   SELECT (lcSelcFile)
   SCAN
     lcAccNo = Account
     IF SEEK(lcAccNo+'O','OrdHdr')
       SELECT OrdHdr
       SCAN REST WHILE Account+cOrdType+Order = lcAccNo + 'O'
          WAIT WINDOW 'Account : '+Account+' Order# : '+Order NOWAIT
          SELECT OrdLine
          SEEK 'O'+OrdHdr.Order
          SCAN REST WHILE cOrdType+Order+STR(LineNo,6) =;
                          'O'+OrdHdr.Order FOR TotQty > 0
            SCATTER MEMVAR MEMO
            INSERT INTO (TempFile) FROM MEMVAR
            SELECT OrdLine
          ENDSCAN
          SELECT OrdHdr   
       ENDSCAN
     ENDIF
     SELECT (lcSelcFile)
   ENDSCAN    
   WAIT CLEAR

   *--Selecting records by Styles
   CASE lcRpSelBy ='S'
     SELECT OrdLine
     SET ORDER TO TAG OrdLineS
     SELECT (lcSelcFile)
     SCAN
       lcStyNo = ALLTRIM(cStyMajor)
       IF SEEK(lcStyNo,'OrdLine')
         SELECT OrdLine
         SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6)=;
                         lcStyNo FOR cOrdType = 'O'
           WAIT WINDOW 'Selecting orders for styles: '+ lcStyNo NOWAIT
           
           SCATTER MEMVAR MEMO
           INSERT INTO (TempFile) FROM MEMVAR

         ENDSCAN                          
       ENDIF
     ENDSCAN
     WAIT CLEAR
 *--Selecting records by Fabric
 CASE lcRpSelBy ='F'
   SELECT OrdLine
   SET ORDER TO TAG OrdLineS
   SELECT (lcSelcFile)
   SCAN
     lcFabNo = Fabric
     SELECT Style
     LOCATE ALL FOR Fabric = lcFabNo
     IF FOUND()
       SCAN FOR Fabric = lcFabNo
         lcKey = Style
         IF SEEK(lcKey,'OrdLine')
           SELECT OrdLine
           SCAN REST WHILE Style+DTOS(Complete)+cOrdType+Order+Store+STR(LineNo,6)=;
                           lcKey FOR cOrdType = 'O'
             WAIT WINDOW "Order# : "+Order NOWAIT
             SCATTER MEMVAR MEMO
             INSERT INTO (TempFile) FROM MEMVAR
           ENDSCAN
         ENDIF
       ENDSCAN     
     ENDIF
   ENDSCAN
   WAIT CLEAR
ENDCASE

*!*************************************************************
*! Name      : lfBldTemp
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To build the temporary file.
*!*************************************************************
*! Example            :  lfBldTemp()
*!*************************************************************
FUNCTION lfBldTemp

SELECT OrdHdr
SET ORDER TO TAG OrdHdr

SELECT &TempFile
IF lcRpSelBy  = 'E'
  USE
  SELECT OrdLine
  SET RELATION TO 'O'+Order INTO OrdHdr
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  GO TOP
  
  SET TALK ON
  SET TALK WINDOW ON
  LOCATE FOR &lcRpExp
  COPY REST TO &gcWorkDir.&TempFile FOR &lcRpExp
  SET TALK OFF
  SET TALK WINDOW OFF  
  WAIT CLEAR
  
  = gfOpenFile (gcWorkDir+TempFile,' ','EX')
  INDEX ON Style+DTOS(OrdHdr->Complete)+Order+STR(LineNo,6) TAG (TempFile)
  SET RELATION TO 'O'+Order INTO OrdHdr
ELSE
  SET RELATION TO 'O'+Order INTO OrdHdr
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SET FILTER TO &lcRpExp
ENDIF

SELECT &TempFile
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfPrnReprt
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To print the report.
*!*************************************************************
*! Example            :  lfPrnReprt()
*!*************************************************************
FUNCTION lfPrnReprt

PAGENO = 0
ROW    = 99

*-- Sub and Grand totals for Approved orders
STORE 0 TO lnSubAppr1,lnSubAppr2,lnSubAppr3,lnSubAppr4,lnSubAppr5,lnSubAppr6,;
lnSubAppr7,lnSubAppr8,;
lnGrdAppr1,lnGrdAppr2,lnGrdAppr3,lnGrdAppr4,lnGrdAppr5,lnGrdAppr6,;
lnGrdAppr7,lnGrdAppr8

*-- Sub and Grand totals for Hold orders
STORE 0 TO lnSubHold1,lnSubHold2,lnSubHold3,lnSubHold4,lnSubHold5,lnSubHold6,;
lnSubHold7,lnSubHold8,;
lnGrdHold1,lnGrdHold2,lnGrdHold3,lnGrdHold4,lnGrdHold5,lnGrdHold6,;
lnGrdHold7,lnGrdHold8

*-- Sub and Grand totals for Decline orders
STORE 0 TO lnSubDecl1,lnSubDecl2,lnSubDecl3,lnSubDecl4,lnSubDecl5,lnSubDecl6,;
lnSubDecl7,lnSubDecl8,;
lnGrdDecl1,lnGrdDecl2,lnGrdDecl3,lnGrdDecl4,lnGrdDecl5,lnGrdDecl6,;
lnGrdDecl7,lnGrdDecl8

*-- Sub and Grand totals for Resubmitted orders
STORE 0 TO lnSubRsub1,lnSubRsub2,lnSubRsub3,lnSubRsub4,lnSubRsub5,lnSubRsub6,;
lnSubRsub7,lnSubRsub8,;
lnGrdRsub1,lnGrdRsub2,lnGrdRsub3,lnGrdRsub4,lnGrdRsub5,lnGrdRsub6,;
lnGrdRsub7,lnGrdRsub8

*-- Sub and Grand totals for PrePaid orders
STORE 0 TO lnSubPrpd1,lnSubPrpd2,lnSubPrpd3,lnSubPrpd4,lnSubPrpd5,lnSubPrpd6,;
lnSubPrpd7,lnSubPrpd8,;
lnGrdPrpd1,lnGrdPrpd2,lnGrdPrpd3,lnGrdPrpd4,lnGrdPrpd5,lnGrdPrpd6,;
lnGrdPrpd7,lnGrdPrpd8

*-- Sub and Grand totals for Open orders
STORE 0 TO lnSubOpen1,lnSubOpen2,lnSubOpen3,lnSubOpen4,lnSubOpen5,lnSubOpen6,;
lnSubOpen7,lnSubOpen8,;
lnGrdOpen1,lnGrdOpen2,lnGrdOpen3,lnGrdOpen4,lnGrdOpen5,lnGrdOpen6,;
lnGrdOpen7,lnGrdOpen8

*-- Sub and Grand totals for In Stock Uncommitted
STORE 0 TO lnSubStk1,lnSubStk2,lnSubStk3,lnSubStk4,lnSubStk5,lnSubStk6,;
lnSubStk7,lnSubStk8,;
lnGrdStk1,lnGrdStk2,lnGrdStk3,lnGrdStk4,lnGrdStk5,lnGrdStk6,;
lnGrdStk7,lnGrdStk8

*-- Grand Total for availabe and required fields.
STORE 0 TO lnGrdAvQt1,lnGrdAvQt2,lnGrdAvQt3,lnGrdAvQt4,lnGrdAvQt5,lnGrdAvQt6,;
lnGrdAvQt7,lnGrdAvQt8,lnGrdReQt1,lnGrdReQt2,lnGrdReQt3,lnGrdReQt4,lnGrdReQt5,;
lnGrdReQt6,lnGrdReQt7,lnGrdReQt8,lnAvaiTot,lnReqTot

*C101709,1 (Begin) Qty and Ex. price subtotal for style/color ordsres AND approve orders.
STORE 0 TO lnSubQty,lnSubAmnt,lnAppAmnt
*C101709,1 (End)

R_TITLE = "ORDER DETAIL REPORT"

SET DEVICE TO PRINT
SELECT (TempFile)
GOTO TOP
lcLStyRang = SUBSTR(Style,1,lnMajLen)
ldLCanDate = OrdHdr.Cancelled
GOTO BOTTOM
lcHStyRang = SUBSTR(Style,1,lnMajLen)
ldHCanDate = OrdHdr.Cancelled
GOTO TOP

DO WHILE !EOF() AND INKEY() <>32
 lcStyle = SUBSTR(Style,1,lnMajLen)
 =IIF(Row > 56,lfPrntHdr(),.F.) 
 DO WHILE SUBSTR(Style,1,lnMajLen) = lcStyle
   lcStyClr = Style
   =SEEK(lcStyClr,'Style')
   =SEEK('S'+Style.Scale,'Scale')
   lnCol = 66
   FOR lnCounter = 1 TO Scale.Cnt
     lcCounter = STR(lnCounter,1)
     @ Row,lnCol SAY PADL(Scale.Sz&lcCounter,4)
     lnCol = lnCol + 5
   ENDFOR
   Row = Row + 1
   =IIF(Row > 56,lfPrntHdr(),.F.) 
   @ Row,00 SAY SUBSTR(Style,1,lnMajLen)
   @ Row,14 SAY SUBSTR(Style,lnNonMajSt,lnColorLen)
   
   lcColorDsc = gfCodDes(SUBSTR(Style,lnNonMajSt,lnColorLen),'COLOR')
   @ Row,22 SAY ALLTRIM(lcColorDsc)
   
   DO CASE
     CASE lcRpStkWp  = 'S'
       @ ROW,40 SAY 'STOCK :'
       lnCol = 66
       FOR lnCounter = 1 TO Scale.Cnt
         lcCounter = STR(lnCounter,1)
         @ Row,lnCol SAY Style.Stk&lcCounter PICTURE '@Z 9999'
         lnCol = lnCol + 5
       ENDFOR
       lnCol = lnCol + 5
       @ Row,108 SAY Style.TotStk PICTURE '@Z 99999'
     CASE lcRpStkWp  = 'W'
       @ ROW,40 SAY 'WIP + STOCK :'
       lnCol = 66
       FOR lnCounter = 1 TO Scale.Cnt
         lcCounter = STR(lnCounter,1)
         @ Row,lnCol SAY Style.Stk&lcCounter+Style.Wip&lcCounter PICTURE '@Z 9999'
         lnCol = lnCol + 5
       ENDFOR
       lnCol = lnCol + 5
       @ Row,108 SAY Style.TotStk+Style.TotWip PICTURE '@Z 99999'
     CASE lcRpStkWp  = 'I'
       @ ROW,40 SAY 'WIP :'
       lnCol = 66
       FOR lnCounter = 1 TO Scale.Cnt
         lcCounter = STR(lnCounter,1)
         @ Row,lnCol SAY Style.Wip&lcCounter PICTURE '@Z 9999'
         lnCol = lnCol + 5
       ENDFOR
       lnCol = lnCol + 5
       @ Row,108 SAY Style.TotWip PICTURE '@Z 99999'
    ENDCASE
    Row = Row + 1
    *-- To Calculate the unallocated qty.
    FOR lnCounter = 1 TO Scale.Cnt
      lcCounter = STR(lnCounter,1)
      lnSubStk&lcCounter = Style.Stk&lcCounter - Style.Alo&lcCounter
      lnGrdStk&lcCounter = lnGrdStk&lcCounter + lnSubStk&lcCounter 
    ENDFOR
    SCAN REST WHILE Style+DTOS(OrdHdr.Complete)+cOrdType+Order+Store+;
                    STR(LineNo,6)= lcStyClr
      WAIT WINDOW "Press space bar to abort. Selecting records. Order# :" + Order + "  Style/Color: " +;
                   ALLTRIM(Style) NOWAIT
      =IIF(Row > 56,lfPrntHdr(),.F.)
      @ Row,00 SAY Order
      @ Row,07 SAY Account
      @ Row,13 SAY IIF(OrdHdr.MultiPo,LEFT(CustPo,10),LEFT(OrdHdr.CustPo,10))
      @ Row,24 SAY Store
      @ Row,33 SAY Start
      @ Row,42 SAY Complete
      @ Row,51 SAY OrdHdr.Status

      *E301249,1 Make priority field C(3) [start]
      *@ Row,53 SAY OrdHdr.Priority
      @ Row,53 SAY ALLTRIM(OrdHdr.Priority)
      *E301249,1 [end]
      
      lcReason = gfCodDes(OrdHdr.Decl_Code,'DECL_CODE')
      @ Row,57 SAY LEFT(ALLTRIM(lcReason),6)
       lnCol = 66
       FOR lnCounter = 1 TO Scale.Cnt
         lcCounter = STR(lnCounter,1)
         @ Row,lnCol SAY Qty&lcCounter PICTURE '@Z 9999'
         lnCol = lnCol + 5
         *-- To accumolate the subtotals and grand totals per size
         =lfAccumlat()
       ENDFOR

       *C101709,1 (Begin) Calculate Qty and Ex. price subtotal for style/color ordsres.
       lnSubQty  = lnSubQty  + TotQty
       lnSubAmnt = lnSubAmnt + (TotQty*Price)
       *--Calculate Total amount for approved orders.
       lnAppAmnt = lnAppAmnt + IIF(OrdHdr.Status = 'O',(TotQty*Price),0)
       *C101709,1 (End)

       @ Row,108 SAY TotQty       PICTURE '@Z 99999'
       @ Row,113 SAY Price        PICTURE '@Z 99999.99'
       @ Row,123 SAY TotQty*Price PICTURE '@Z 999999.99'
       Row = Row + 1
    ENDSCAN
    Row = Row + 1
    
    *C101709,1 (Begin) Say Qty and Ex. price subtotal for style/color ordsres.
    @ Row,00 SAY "  Total for ( "+ lcStyClr +")"
    @ Row,108 SAY lnSubQty    PICTURE '@Z 99999'
    @ Row,123 SAY lnSubAmnt   PICTURE '@Z 999999.99'
    *--Reinitialize the two variables for a new style/color.
    STORE 0 TO lnSubQty,lnSubAmnt
    Row = Row + 1
    *C101709,1 (End)

    *-- To print the subtotals
    =lfPrnTotal('S')

    *-- To print the Style's POs.
    =lfPrnStyPo()
    =SEEK(lcStyClr,'Style')
    @ Row,00 SAY "Required  :"
    lnCol = 66    
    lnReqTot = 0
    FOR lnCounter = 1 TO Scale.Cnt
      lcCounter = STR(lnCounter,1)
      @ Row,lnCol SAY IIF((Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter < 0,;
                      (Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter,0) PICTURE "@Z 9999"
      lnReqTot = lnReqTot + IIF((Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter < 0,;
                      (Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter,0)
      lnGrdReQt&lcCounter = lnGrdReQt&lcCounter+IIF((Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter < 0,;
                      (Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter,0)
      lnCol = lnCol + 5      
    ENDFOR
    @ Row,108 SAY lnReqTot PICTURE "@Z 99999"
    Row = Row + 1
    =IIF(Row > 56,lfPrntHdr(),.F.)
    @ Row,00 SAY "Available :"
    lnCol = 66
    lnAvaiTot = 0    
    FOR lnCounter = 1 TO Scale.Cnt
      lcCounter = STR(lnCounter,1)
      @ Row,lnCol SAY IIF((Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter > 0,;
                      (Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter,0) PICTURE "@Z 9999"
      lnAvaiTot = lnAvaiTot + IIF((Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter > 0,;
                      (Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter,0)
      lnGrdAvQt&lcCounter = lnGrdAvQt&lcCounter+IIF((Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter > 0,;
                      (Style.Stk&lcCounter+Style.Wip&lcCounter)-;
                      Style.Ord&lcCounter,0)                     
      lnCol = lnCol + 5
    ENDFOR
    @ Row,108 SAY lnAvaiTot PICTURE "@Z 99999"   
    Row = Row + 1
    @ Row,00 SAY REPLICATE('-',132)
    Row = Row + 1
    =IIF(Row > 56,lfPrntHdr(),.F.)
  ENDDO
  
  IF !EOF()
    Row = Row + 1
  ENDIF  
ENDDO
WAIT CLEAR

*-- To print the grand totals.
=lfPrnTotal('G')

DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To print the report's header.
*!*************************************************************
*! Example            :  lfPrntHdr()
*!*************************************************************
FUNCTION lfPrntHdr

PAGENO = PAGENO + 1
@ 01,000 SAY "SOAND30"
@ 01,((130 - (LEN(TRIM(QCOMPANY))))/2) SAY gcCompName
@ 01,120 SAY DATE()
@ 01,129 SAY '~'
@ 02,000 SAY TIME()
@ 02,((130 - (LEN( R_TITLE)))/2) SAY R_TITLE 
@ 02,120 SAY 'PAGE#'
@ 02,126 SAY STR(PAGENO,4)
lc3rdHdr = '"'+"Cancel Date:"+DTOC(ldLCanDate)+"-to-"+DTOC(ldHCanDate)+'"'+" "+'"'+"Style:"+ALLTRIM(lcLStyRang)+"-to-"+ALLTRIM(lcHStyRang)+'"'+" "+'"'+"Group:"+lcGroup+'"'
@ 03,((130 - (LEN(lc3rdHdr)))/2) SAY lc3rdHdr
@ 04,((130 - (LEN(lcRpOptTit)))/2)  SAY lcRpOptTit
@ 05,00 SAY REPLICATE('-',132)
Row = 6
@ Row,00 SAY 'Order# Acct. CustPo     Store    Start    Complete S Pr. Dcl. Re.  (1)  (2)  (3)  (4)  (5)  (6)  (7)  (8) Tot Pcs   Price Ext. Price'
Row = Row + 1
@ Row,00 SAY REPLICATE('-',132)
Row = Row + 1

*!*************************************************************
*! Name      : lfAccumlat
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To accumolate the sub and grand totals
*!*************************************************************
*! Example            :  lfAccumlat()
*!*************************************************************
FUNCTION lfAccumlat

lnSubOpen&lcCounter = lnSubOpen&lcCounter + Qty&lcCounter
lnGrdOpen&lcCounter = lnGrdOpen&lcCounter + Qty&lcCounter

DO CASE
  CASE OrdHdr.Status = 'O'
    lnSubAppr&lcCounter = lnSubAppr&lcCounter + Qty&lcCounter
    lnGrdAppr&lcCounter = lnGrdAppr&lcCounter + Qty&lcCounter
  CASE OrdHdr.Status = 'H' AND EMPTY(OrdHdr.Approval) 
    lnSubHold&lcCounter = lnSubHold&lcCounter + Qty&lcCounter
    lnGrdHold&lcCounter = lnGrdHold&lcCounter + Qty&lcCounter
  CASE OrdHdr.Status = 'H' AND OrdHdr.Approval = 'DECLINE'
    lnSubDecl&lcCounter = lnSubDecl&lcCounter + Qty&lcCounter
    lnGrdDecl&lcCounter = lnGrdDecl&lcCounter + Qty&lcCounter
  CASE OrdHdr.Status = 'H' AND OrdHdr.Approval = 'RESUB.'
    lnSubRsub&lcCounter = lnSubRsub&lcCounter + Qty&lcCounter
    lnGrdRsub&lcCounter = lnGrdRsub&lcCounter + Qty&lcCounter
  CASE OrdHdr.Status = 'H' AND OrdHdr.Approval = 'PREPAID'
    lnSubPrpd&lcCounter = lnSubPrpd&lcCounter + Qty&lcCounter
    lnGrdPrpd&lcCounter = lnGrdPrpd&lcCounter + Qty&lcCounter
ENDCASE

*!*************************************************************
*! Name      : lfPrnTotal
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To print the sub or grand totals for a style/color.
*!*************************************************************
*! Example            :  lfPrnTotal()
*!*************************************************************
FUNCTION lfPrnTotal
PARAMETERS lcType

IF lcType = 'S'
  @ Row,00 SAY "SubTotals ==>"
  @ Row,30 SAY "Approve Orders"
  =lfPrnQty('lnSubAppr')

  @ Row,30 SAY "Hold Orders"
  =lfPrnQty('lnSubHold')

  @ Row,30 SAY "Decline Orders"
  =lfPrnQty('lnSubDecl')

  @ Row,30 SAY "Resubmitted Orders"
  =lfPrnQty('lnSubRsub')

  @ Row,30 SAY "Prepaid Orders"
  =lfPrnQty('lnSubPrpd')

  @ Row,30 SAY "Open Orders"
  =lfPrnQty('lnSubOpen')

  @ Row,30 SAY "In stock uncommitted"
  =lfPrnQty('lnSubStk')
  
ELSE
  @ Row,00 SAY "Grand Totals ==>"
  @ Row,30 SAY "Approve Orders"
  =lfPrnQty('lnGrdAppr')

  @ Row,30 SAY "Hold Orders"
  =lfPrnQty('lnGrdHold')

  @ Row,30 SAY "Decline Orders"
  =lfPrnQty('lnGrdDecl')

  @ Row,30 SAY "Resubmitted Orders"
  =lfPrnQty('lnGrdRsub')

  @ Row,30 SAY "Prepaid Orders"
  =lfPrnQty('lnGrdPrpd')

  @ Row,30 SAY "Open Orders"
  =lfPrnQty('lnGrdOpen')

  @ Row,30 SAY "In stock uncommitted"
  =lfPrnQty('lnGrdStk')


  @ Row,00 SAY "Required"
  =lfPrnQty('lnGrdReQt')

  @ Row,00 SAY "Available"  
  =lfPrnQty('lnGrdAvQt')
  
ENDIF

*!*************************************************************
*! Name      : lfPrnStyPo
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To print the style pos.
*!*************************************************************
*! Example            :  lfPrnStyPo()
*!*************************************************************
FUNCTION lfPrnStyPo
PRIVATE lnAlias,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8

lnAlias = SELECT(0)

CREATE CURSOR (lcTempCur) (cPo C(6),cVendor C(5), ldComp D, ldAvail D,;
                           cShipNo C(6),cTranCd C(1), nQty1 N(6),nQty2 N(6),;
                           nQty3 N(6),nQty4 N(6),nQty5 N(6),nQty6 N(6),;
                           nQty7 N(6),nQty8 N(6))
INDEX ON cPo+cShipNo TAG (lcTempCur) OF (lcTempCur)
                           
STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
IF SEEK(lcStyClr+'P','PosLn')
  lnCol = 66    
  @ Row,00 SAY "Open Purchase Orders:"
  =SEEK(lcStyClr,'Style')
  =SEEK('S'+Style.Scale,'Scale')
  SELECT PosLn
  FOR lnCounter = 1 TO Scale.Cnt
    lcCounter = STR(lnCounter,1)
    =SEEK(lcStyClr)
    SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+TranCd+STR(RECNO(),7) =;
                    lcStyClr+'P'
      IF SEEK('P'+Po,'PosHdr') AND !(PosHdr.Status $ "OH")
         LOOP
      ENDIF
      lnQty&lcCounter = lnQty&lcCounter+IIF(TranCd = "1",Qty&lcCounter,;
                                        IIF(TranCd $ "245",-Qty&lcCounter,0))
      SELECT (lcTempCur)
      IF !SEEK(PosLn.Po+PoSLn.ShipNo)
        APPEND BLANK
      ENDIF
      REPLACE cPo     WITH PosLn.Po,;
              cVendor WITH PosHdr.Vendor,;
              ldComp  WITH PosHdr.Complete,;
              ldAvail WITH PosHdr.Available,;
              cShipNo WITH PosLn.ShipNo,;              
              cTranCd WITH PosLn.TranCd,;
              nQty&lcCounter  WITH nQty&lcCounter+IIF(PosLn.TranCd = "1",PosLn.Qty&lcCounter ,;
                                 IIF(PosLn.TranCd $ "245",-PosLn.Qty&lcCounter ,PosLn.Qty&lcCounter ))
    ENDSCAN
    @ Row,lnCol SAY lnQty&lcCounter PICTURE "@Z 9999"
    lnCol = lnCol + 5
  ENDFOR
  @ Row,108 SAY lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8;
                PICTURE "@ 99999"
  Row = Row + 2
  =IIF(Row > 56,lfPrntHdr(),.F.)
  SELECT (lcTempCur)
  SCAN             
    @ Row,00 SAY cPo
    @ Row,08 SAY cVendor
    IF !EMPTY(cShipNo)
      =SEEK(cShipNo,'ShpmtHdr')
      @ Row,18 SAY "SHIPT"
    ELSE
      @ Row,18 SAY "OPEN"
    ENDIF
    @ Row,25 SAY ldComp
    @ Row,34 SAY IIF(!EMPTY(cShipNo),ShpmtHdr.Eta,ldAvail)
    lnCol = 66 
    FOR lnCounter = 1 TO Scale.Cnt      
      lcCounter = STR(lnCounter,1)      
      @ Row,lnCol SAY nQty&lcCounter PICTURE "@Z 9999"      
      lnCol = lnCol + 5
    ENDFOR
    @ Row,108 SAY nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8;
                  PICTURE "@Z 99999"
    Row = Row + 1
    =IIF(Row > 56,lfPrntHdr(),.F.)
  ENDSCAN          

ENDIF

SELECT(lnAlias)


*!*************************************************************
*! Name      : lfPrnQty
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To print the qty per size
*!*************************************************************
*! Example            :  lfPrnQty()
*!*************************************************************
FUNCTION lfPrnQty
PARAMETERS lcQtyPerSz

PRIVATE lnTotSub
lnTotSub = 0
lnCol = 66
FOR lnCounter = 1 TO Scale.Cnt
  lcCounter = STR(lnCounter,1)
  @ Row,lnCol SAY &lcQtyPerSz&lcCounter PICTURE "@Z 9999"
  lnTotSub = lnTotSub + &lcQtyPerSz&lcCounter
  &lcQtyPerSz&lcCounter = 0
  lnCol = lnCol + 5
ENDFOR
@ Row,108 SAY lnTotSub PICTURE "@Z 99999"
*C101709,1 (Begin)
@ Row,123 SAY lnAppAmnt PICTURE '@Z 999999.99'
*--Reinitialize lnAppAmnt variable.
lnAppAmnt = 0
*C101709,1 (End)
Row = Row + 1
=IIF(Row > 56,lfPrntHdr(),.F.)  

*!*************************************************************
*! Name      : lfvSelBy
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To validate the select by
*!*************************************************************
*! Example            :  lfvSelBy()
*!*************************************************************
FUNCTION lfvSelBy
llClearAcc = (lcRpSelBy  # 'A')
llClearSty = (lcRpSelBy  # 'S')
llClearFab = (lcRpSelBy  # 'F')
CLEAR READ

*!*************************************************************
*! Name      : lfvOrder
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To validate the select by
*!*************************************************************
*! Example            :  lfvOrder()
*!*************************************************************
FUNCTION lfvOrder

PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field


SELECT OrdHdr
lcFabOrder = ORDER()
SET ORDER TO OrdHdr

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('O'+lcObjVal , 'OrdHdr'))
  llObjRet = ORDBROWO(@lcObjVal )
  lcObjVal = IIF(llObjRet , lcObjVal , "")
  &lcObjName = lcObjVal
ENDIF    && End of IF

SELECT OrdHdr
SET ORDER TO &lcFabOrder
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/03/1999
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg = gfItemMask('SM')  && No. of major segments.

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.      
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTit = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
        
*-- Compute Free/Color Items in Style code Structure. [End] 

RETURN ''


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Example            :  lfsrvSty()
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Example            :  lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0
IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : selecting it in inlist function.
*!*************************************************************
*! Example            :  lfSRVFab()
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Example            :  lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Example            :  lfsrAcc()
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Example            :  lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : KHALD MOHI EL-DIN
*! Date      : 02/03/1999
*! Purpose   : To be done when the report is run.
*!*************************************************************
*! Example            :  lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH    = 'W'
STORE .T. TO llClearAcc,llClearSty,llClearFab
