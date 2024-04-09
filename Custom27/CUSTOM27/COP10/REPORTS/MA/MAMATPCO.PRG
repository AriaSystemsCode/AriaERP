*******************************************************************
* PROG : MAMATCO.PRG  (Converted Matreial PO FORM For COPY CATS).
* DATE : 06/06/2000
* DESC : PRINT FABRIC PURCHASE ORDER
* AUTH : Adel Mohammed El Gazzar (ADEL)
* Refer to (C101886)
****************************************************************************

*--Initialize some variables.
llOptMsg = (!EMPTY(lcRpMsg1) OR !EMPTY(lcRpMsg2) OR !EMPTY(lcRpMsg3))
lnCol1=(ROUND((80-LEN(laCompAdd[1]))/2,0))
lnCol2=(ROUND((80-LEN(laCompAdd[2]))/2,0))
lnCol3=(ROUND((80-LEN(laCompAdd[3]))/2,0))
lnCol4=(ROUND((80-LEN(laCompAdd[4]))/2,0))
lnCol5=(ROUND((80-LEN(laCompAdd[5]))/2,0))
lnMaxRow = IIF(llOptMsg,45,55)
lnPageNo=0
llNoRec  = .F.
A= REPLICATE('*',80)

*  .....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    ITEM    DESCRIPTION               PATTERN       WIDTH     WEIGHT                  && MODIFY BY SMH ON 05/23/93
*            CONTENTS                                                          && SMH 05/23/93 ADDED
*
*    COLOR   COLOR DESCRIPTION         TOT QTY  UOM  CURR      PRICE         AMOUNT'
*    XXXXXXX XXXXXXXXXXXXXXXXXXXXX     XXXXXXXXXX    XXXXXX    XXXXXXXXXXXXXXX                  && SMH 05/23/93 ADDED WIDTH AND WEIGHT
*            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                             && SMH 05/23/93 ADDED 
*    XXXXXXX XXXXXXXXXXXXXXXXXXXXX 9999999.999  XXX  XXX   999999.99  9999999999.99             && ARH 09/07/92 
C='  ITEM    DESCRIPTION               PATTERN       WIDTH    WEIGHT'                          && SMH 05/23/93  MODIFY
E='          CONTENTS'                                                                          && SMH 05/23/93  ADDED   
D='  COLOR   COLOR DESC PATTERN        TOT QTY  UOM  CURR     PRICE         AMOUNT'            && SMH 04/22/93 
*-------------------------------
* SETUP HEADINGS
*-------------------------------
IF !lfPrintRep()
  llNoRec = .T.
  RETURN
ENDIF
SET DEVICE TO SCREEN
RETURN

*****************************************************************************
*: Program file  : lfPrintRep.PRG
*: Program desc. : Print form. 
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 06/06/2000
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:EXAMPLE : DO lpPrint
*****************************************************************************
FUNCTION lfPrintRep


SET DEVICE TO PRINT
SELECT POFHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
   =gfModalGen('TRM00052B00000','DIALOG')
   SET DEVICE TO SCREEN
  RETURN .F.
ENDIF
lnRow = 27 
SET SKIP TO
SCAN WHILE INKEY() <>32 FOR &lcRpExp
  WAIT WINDOW 'Report printing ! press <SPACE BAR> to abort ' NOWAIT  
  *--Getting header variables.
  =lfHeadVar()
  lcPo = pomat
  *--Initialize header variables
  lnRate    = nPriceRat
  lnTax     = NCOST2
  lnFreight = NCOST3
  lnQuota   = NCOST4
  lcPriceSmb = PADL(ALLTRIM(IIF(llRpFCurr,IIF(SEEK(POFHDR.cPriceCur,'SYCCURR'),SycCurr.cCurrSmbl,''),IIF(SEEK(gcBaseCurr,'SYCCURR'),SycCurr.cCurrSmbl,''))),3)
  IF llRpFCurr
    lnTax    = NCOST2*nPriceRat                  && CONVERT USD AMOUNT TO FOREIGN
    lnFreight= NCOST3*nPriceRat
    lnQuota  = NCOST4*nPriceRat
  ENDIF
  lcPrevFab = SPACE(7)
  STORE .F. TO NOTELOOP,SNOTLOOP
  STORE 0   TO lnTotQty,lnTotOrd,XGTOTORD,lnGTotQty,lnPageNo
  *--Printing header
  DO lpHeader
  *-----------------------
  *--Printing Lines
  SELECT POFLN
  SCAN WHILE cmattype+pomat+fabric+color+trancd = POFHDR.cmattype+POFHDR.pomat FOR (TRANCD='1' .AND. nFabTotqty>0)
    IF lnRow>=lnMaxRow
      DO lpPrPgFoot
      DO lpHeader
    ENDIF
    *-Print subtotal
    =lfPrSubTot()
    *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
    *LN# GRP ITEM     CLR                                PRICE   PIECES       AMOUNT
    *123  X  1234567 123 12345678901234567890          1234.99   123456   123,456.99
    *            11111 22222 33333 44444 55555 66666 77777 88888
    *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
    IF FABRIC <> lcPrevFab
      @ lnRow,02 SAY FABRIC
      @ lnRow,10 SAY SUBSTR(Fabric.Desc,1,20)
      @ lnRow,36 SAY PATTERN
      @ lnRow,50 SAY WIDTH
      @ lnRow,59 SAY Fabric.cFabWeight
      lnRow=lnRow+1
      IF !EMPTY(Fabric.CONTENT)
        @ lnRow,10 SAY Fabric.CONTENT  
        lnRow = lnRow + 1
      ENDIF
      lcPrevFab = FABRIC
      *---------------------------------------------------
      * [NOTEPAD] COMMENT LINES
      *---------------------------------------------------
      IF llRpPrtMn
        =SEEK('G'+FABRIC,'NOTEPAD')
        =lfPrnNotes()
      ENDIF  
    ENDIF 
    IF lnRow>=lnMaxRow
      DO lpPrPgFoot
      DO lpHeader
    ENDIF
    lnRow = lnRow + 1
    @ lnRow ,02 SAY COLOR
    lcColorDesc = gfCodDes(COLOR , 'COLOR')    
    @ lnRow ,10 SAY SUBSTR(lcColorDesc ,1,10)
    @ lnRow,21 SAY Pattern
    @ lnRow,36 SAY nFabTotQty PICTURE '9999999' 
    @ lnRow,45 SAY FABRIC.UOMBUY
    @ lnRow,50 SAY lcPriceSmb
    IF llRpFCurr
      @ lnRow,54 SAY NCOST1 PICTURE '999999.999'
    ELSE
      IF lnRate <> 0
        @ lnRow,54 SAY (NCOST1/lnRate) PICTURE '999999.999'
      ELSE
        @ lnRow,54 SAY 'N/A'
      ENDIF  
    ENDIF
    IF llRpFCurr    
      lnAmount= nFABTOTQTY * NCOST1
    ELSE
      lnAmount= nFABTOTQTY * (NCOST1/lnRate)
    ENDIF
    IF lnRate <> 0
      @ lnRow,65 SAY lnAmount  PICTURE '9999999999.999'
    ELSE
      @ lnRow,65 SAY 'N/A'
      lnAmount = 0
    ENDIF  
    * CUM ORDER TOTALS
    lnTotQty  = lnTotQty+ nFabTOTQTY
    lnTotOrd  = lnTotOrd+ lnAmount
    lnRow = lnRow + 1
  ENDSCAN
  =lfPrSubTot()
  IF llRpPrtPn 
    =SEEK('M'+POFHDR.CMATTYPE+POFHDR.POMAT,'NOTEPAD')
    =lfPrnNotes(.T.)
  ENDIF  
  
  @ 56,00 SAY A
  @ 57,10 SAY 'TOTAL  =>' 
  @ 57,32 SAY lnGTotQty  PICTURE '9999999999'
  @ 57,65 SAY XGTOTORD  PICTURE '9999999999.999'
  lnRow = 58
  *--If the user wants to print costing cat.
  IF llRpCostCt
    IF lnFreight<>0
      @ lnRow,10 SAY 'ESTIMATED FREIGHT'
      @ lnRow,65 SAY lnFreight PICTURE '9999999999.999' 
      lnRow=lnRow+1
    ENDIF
    IF lnTax<>0
      @ lnRow,10 SAY 'ESTIMATED TAX'
      @ lnRow,65 SAY lnTax PICTURE '9999999999.999'
      lnRow=lnRow+1
    ENDIF
    IF lnQuota<>0
      @ lnRow,10 SAY 'ESTIMATED QUOTA'
      @ lnRow,65 SAY lnQuota PICTURE '9999999999.999'    
      lnRow=lnRow+1
    ENDIF
    XGAMT=XGTOTORD+lnFreight+lnTax+lnQuota 
  ELSE
    XGAMT=XGTOTORD 
  ENDIF
  @ lnRow,10 SAY 'TOTAL PURCHASE ORDER AMOUNT'
  @ lnRow,50 SAY lcPriceSmb
  @ lnRow,65 SAY XGAMT PICTURE '9999999999.999'
  lnRow = lnRow + 1
  @ lnRow,00 SAY REPLICATE('-',80)
  lnRow = lnRow + 1
  IF llOptMsg
    @ lnRow,3 SAY lcRpMsg1
    lnRow = lnRow + 1
    @ lnRow,3 SAY lcRpMsg2
    lnRow = lnRow + 1
    @ lnRow,3 SAY lcRpMsg3	
    lnRow = lnRow + 1
  ENDIF   
ENDSCAN

*****************************************************************************
*: Program file  : lpHeader.PRG
*: Program desc. : Print hesder. 
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 06/06/2000
*****************************************************************************
PROCEDURE lpHeader

PRIVATE lnAlias
lnAlias = SELECT()
SELECT POFHDR
@ 01,04 SAY IIF(cMatType = 'P','PO # : ',IIF(cMatType = 'R','Return # : ',  'Contract # : ')) + POMAT

@ 01,lnCol1 SAY laCompAdd[1]
@ 01,60 SAY 'DATE  :'
@ 01,68 SAY ENTERED

lnPageNo = lnPageNo +1
@ 02,lnCol2 SAY laCompAdd[2]
@ 02,60 SAY 'PAGE #: '
@ 02,68 SAY lnPageNo PICTURE '999'

@ 03,lnCol3 SAY laCompAdd[3]
laCompAdd[4] = STRTRAN(laCompAdd[4],'Phone# : ','')
@ 04,lnCol4 SAY laCompAdd[4]
laCompAdd[5] = STRTRAN(laCompAdd[5],'Phone# : ','')
@ 05,lnCol5 SAY laCompAdd[5]

@ 07,26 SAY IIF(cMatType = 'P','P u r c h a s e   O r d e r',IIF(cMatType = 'R','R e t u r n   P u r c h a s e   O r d e r',  '         C o n t r a c t'))


@ 09,03 SAY '........ VENDOR .........'
@ 09,47 SAY '........ SHIP TO ........'

@ 10,03 SAY laVendor[1]
@ 10,47 SAY laShipTo[1]

@ 11,03 SAY laVendor[2]
@ 11,47 SAY laShipTo[2]

@ 12,03 SAY laVendor[3]
@ 12,47 SAY laShipTo[3]

@ 13,03 SAY laVendor[4]
@ 13,47 SAY laShipTo[4]

@ 14,03 SAY laVendor[5]
@ 14,47 SAY laShipTo[5]

@ 15,03 SAY 'ATTN   : '+CONTACT
@ 16,3 SAY  'PHONE #: '
@ 16,12 SAY TRANSFORM(PHONE , lcPhonPict)
@ 16,46 SAY 'SHIP VIA     :'+lcShipVia

   
@ 17,03 SAY 'DIVISION   :'+lcDivDesc
@ 17,46 SAY "QUOTA CATG.  :"+ ALLT(QuotaCat)
            
      
@ 18,03 SAY 'COMPLETE   :'
@ 18,15 SAY COMPLETE
@ 18,46 SAY "START        :"
@ 18,60 SAY ENTERED

   
@ 19,03 SAY 'TERMS      :'+lcTerms
@ 19,46 SAY 'ORIGIN       :'+ORIGIN

@ 20,03 SAY 'F.O.B.     :'+CFOB
@ 20,46 SAY 'INSURANCE    :'+INSURANCE
@ 21,03 SAY 'REQUIS. FOR:'+cRequis
@ 21,46 SAY 'LETTER CREDIT:'+CFAB_LC
@ 22,00 SAY A
@ 23,00 SAY C
@ 24,00 SAY E
@ 25,00 SAY D
@ 26,00 SAY A
lnRow = 27 
SELECT (lnAlias)


*!*************************************************************
*! Name      : lfPrnNotes
*: DATE      : 06/06/2000
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To print the fabric's notepad.
*!*************************************************************
*! Example   :  lfPrnNotes()
*!*************************************************************
FUNCTION lfPrnNotes
PARAMETERS llSeek
PRIVATE lnAlias

lnAlias  = SELECT()
lnOldMemW = 0
lnMemLins = 0
lnNotLine = 0
lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 65
SELECT NotePad
lnMemLins = MEMLINES(NOTEPAD.MNOTES)
DO WHILE lnNotLine <= lnMemLins    
  IF !EMPTY(MLINE(MNOTES,lnNotLine))
    lnRow = lnRow +1
    IF lnRow>=lnMaxRow
      DO lpPrPgFoot
      DO lpHeader
    ENDIF 
    @ lnRow,02 SAY MLINE(MNOTES,lnNotLine)
  ENDIF
  lnNotLine = lnNotLine +1
ENDDO     
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfPrSubTot
*: DATE      : 06/06/2000
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To print the fabric's notepad.
*!*************************************************************
*! Example   :  lfPrnNotes()
*!*************************************************************
FUNCTION lfPrSubTot

*** PRINT FABRIC SUBTOTAL  ***
IF EOF() OR ((FABRIC<>lcPrevFab) AND !EMPTY(lcPrevFab)) OR (pomat <> lcPo)
  @ lnRow,10 SAY '*** SUB TOTAL ***'
  @ lnRow,36 SAY lnTotQty PICTURE '9999999'       
  @ lnRow,65 SAY lnTotOrd PICTURE '9999999999.999'
  lnRow=lnRow+2
  lnGTotQty=lnGTotQty+lnTotQty
  XGTOTORD=XGTOTORD+lnTotOrd
  lnTotQty=0
  lnTotOrd=0
ENDIF

*!*************************************************************
*! Name      : lfPrPgFoot
*: DATE      : 06/06/2000
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To print the page footer.
*!*************************************************************
*! Example   :  lfPrPgFoot()
*!*************************************************************
PROCEDURE lpPrPgFoot

IF llOptMsg
  @ 53,00 SAY REPLICATE('-',80)
  @ 54,13 SAY 'C O N T I N U E D ...'
  @ 55,00 SAY REPLICATE('-',80)
  @ 56,3 SAY lcRpMsg1
  @ 57,3 SAY lcRpMsg2
  @ 58,3 SAY lcRpMsg3	
ELSE
  @ 58,00 SAY REPLICATE('-',80)
  @ 59,13 SAY 'C O N T I N U E D ...'
  @ 59,69    SAY '**********'
  @ 60,00 SAY REPLICATE('-',80)
ENDIF
