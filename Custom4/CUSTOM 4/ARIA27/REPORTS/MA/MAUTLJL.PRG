*:************************************************************************
*: Program file  : MAUTLJL.PRG 
*: Program desc. : Material Utilization Report
*: For Report    : (@x,y SAY)
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MATERIALS (MA).
*:      Developer: IHB
*:************************************************************************
*: Calls         : 
*:    Procedures : None
*:    Functions  : lfwRepWhen, lfGetColor, lfvFabBet, lfvFabRng, 
*:                 lfvpbOk, lfvSortBy, lfvSeq
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO MAUTLJL
*:************************************************************************
*: This Report Program is due to E301084,1
*:************************************************************************
*: Modifications :
*:B802245,1 HDM 05/23/1999 Adjust report Fields position to increase
*:                         new balance field picture 1 digit
*:B603358,1 AAN 08/29/2000 fix the bug of Missing screen objrng.scx , so we add it to the Ma 
*:B603358,1 AAN            directory
*:B803660,1 HBG 10/01/2000 Fix bug of 'Unit Cost' & 'Item Cost' not Aligned
*:B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*:B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:B603381,1 ADEL 10/24/2000 Fix some bugs.
*:B602910,1 RAMY 02/11/2001 Add decimals to the totals fields
*:B604518,1 WAB  06/08/2001 fix the bug of the material utilization report does not show the 
*:B604518,1 WAB             entire style number in the reference field. 
*:B604518,1 WAB             it only shows 6 characters of style number
*:C200258,1 WAB 01/01/2002 print the dyelot fiedl after the refrence field
*:B605918,1 KHM 04/29/2002 Fix the problem of not returning the device to screen
*:B037177,1 ABD 10/14/2003 Fix the problem that the report does not 
*:B037177,1 ABD            consider the inventory Locking transactions
*:B122148,1 NNA 03/24/2004 Fix bug of incorrect result if we chose a date or sequence Rang
*:B038431,1 NNA 09/09/2004 Fix bug that if you didn't define item code structure you'll get error
*:B038431,1 NNA            [Variable 'laSeg' Not Found ] when you run this report 
*:B608374,1 WAM 12/05/2007 Fix error appears after close report after the No records message displayed
*:************************************************************************
*-- Option Grid Layout :
*-- Sort By (D)DAte (S)sequence    : D
*-- Activity Date / Sequence Range : .. to ..
*-- Item Type                      : ..
*-- Item Rnage                     : .. to ..
*-- Color                          : .. .. .. .. .. .. .. ..

*-- Note That using RANGE FUNCTION for selecting sequence in MATINVJL
*-- is not correct because starting period balance is calculated based  
*-- on selected fixed sequence (MATINVJL.CTRAN_SEQ) .
*-- We 'd used BETWEEN operator (similar to Item Range) and defined LCRPLSEQ, LCRPHSEQ 
*-- to hold the selected sequence .

*-- MAREPORT --> GFOPGRID --> MAUTLJL
*-- All needed dbfs. and indexes are initiated before calling MAUTLJL  

IF 'PO' $ gcCmpModules
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF
IF 'MF' $ gcCmpModules
  =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
ENDIF
*B603381,1 (Begin) Open Material Manf. Order detail file.
=gfOpenFile(gcDataDir+'mmfgordd',gcDataDir+'Mfgrec','SH')
*B603381,1 (End)

*C200258,1 WAB (Start) -- if the ma not used dyelot we should not print the dyelot column
IF !llDyelot  
  llRpPrtDy = .F.
ENDIF
*C200258,1 WAB (End)

*B803660,1 HBG 10/01/2000 Flag To Check If this is new page inside the 
*B803660,1                same fabric/color or not[Begin]
llBegin = .F.
*B803660,1 [End]

R_TITLE = 'MATERIAL UTILIZATION'   && report title
lcTempFile = gfTempName()    &&global function gives temporarily name for the workfile.

SELECT MATINVJL
SET RELATION TO CFABRIC+CCOLOR INTO FABRIC
GO TOP
*-- if user selected Item Range, modify lcRpExp  
IF !EMPTY(lcRpFrmFab)       && meaning both lcRpFrmFab and lcRpToFab have values 
  lcRpExp = lcRpExp + ' AND BETWEEN(MATINVJL.CFABRIC,lcRpFrmFab,lcRpToFab)'   
ELSE   && else, lcRpFrmFab is empty and lcRpToFab has value
  IF !EMPTY(lcRpToFab)
    lcRpExp = lcRpExp + ' AND MATINVJL.CFABRIC <= lcRpToFab'       
  ENDIF
ENDIF  && end if user selected Item Range, modify lcRpExp  
*-- if user selected Item Type, modify lcRpExp  
IF !EMPTY(lcRpItmTyp)
  lcRpExp = lcRpExp + '.AND. FABRIC.ITEM_TYPE = lcRpItmTyp'
ENDIF  && end if user selected Item Type, modify lcRpExp  

*B122148,1 NNA 03/24/2004 (Begin) add the transaction Date and the trans. Sequense to the filter
IF !EMPTY(hDate)
  lcRpExp = lcRpExp + '.AND. dTranDate<=hDATE'
ENDIF
IF !EMPTY(lcRphSeq)
  lcRpExp = lcRpExp + '.AND. VAL(cTrn_Seq) <= VAL(lcRphSeq)'
ENDIF
*B122148,1 NNA (End)

COPY REST TO (gcWorkDir+lcTempFile) FOR &lcRpExp    && appends all needed records.
*-- reset the relation between MATIVJL and FABRIC
SET RELATION TO
=gfOpenFile(gcWorkDir+lcTempFile,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
*-- if user selected Date Range, index lcTempFile 
IF lcrpSortby='D'
  INDEX ON cFabric+cColor+DTOS(dTranDate)+cTrn_Seq TAG (lcTempFile)
  *B122148,1 NNA 03/24/2004 (Begin) make a filtration to know if the (Fabric/Color) that I selected
  *B122148,1 NNA            Has transaction within my period range and if found delete Colors that not have.
  *-- IF the user Input a date Range, I'll delete the unneeded transaction that not have a date
  *-- within my Range , but if there is no date range I let the report to run normally to fix the 
  *-- problem of the speed
  IF !EMPTY(hDate)				
    *-- open the temp file again with another alias to make a relationship into itself
    SELECT 0
    IF !USED('lcTmp2')
      USE (gcWorkDir+lcTempFile) AGAIN ALIAS lcTmp2	
      SET ORDER TO TAG lcTempFile
    ELSE
      *-- Close the temp file and Reuse it to Recollecting data from the master temp file
      USE IN lcTmp2
      USE (gcWorkDir+lcTempFile) AGAIN ALIAS lcTmp2	
      SET ORDER TO TAG lcTempFile
    ENDIF
    SELECT (lcTempFile)
    SET RELATION TO (cFabric+cColor) INTO lcTmp2 ADDITIVE	
    *-- First search in the temp file if any Fabric/color Has a Tran. Date within my Range
    SET FILTER TO BETWEEN(dTranDate,LDATE,HDATE)
    LOCATE
    IF EOF()
      =gfModalGen('TRM00052B40011','ALERT')
       *B608374,1 WAM 12/05/2007 Fix error appears after close report after the No records message displayed
       USE IN lcTmp2
       *B608374,1 WAM 12/05/2007 (End)
       RETURN
    ELSE
      SET FILTER TO      
      SELECT (lcTempFile)
      SCAN 
        SELECT lcTmp2
        lltrue = .F.
        SCAN REST WHILE cFabric+cColor+DTOS(dTranDate)+cTrn_Seq = &lcTempFile..cFabric+&lcTempFile..cColor
          *-- IF the (fabric/Color) has a transaction date within my range leave it
          IF BETWEEN(dTranDate,LDATE,HDATE)	
            lltrue = .T.
            EXIT
          ENDIF
        ENDSCAN
        *-- IF the (fabric/Color) Has No transactions date within my range Delete it
        IF !lltrue
          SELECT (lcTempFile)
          DELETE
        ENDIF  
      ENDSCAN
    ENDIF
    SET FILTER TO
    *-- Close the relationship and close the new temp file Alias
    SET RELATION TO
    SELECT lcTmp2
    USE										
    SELECT (lcTempFile)
    *-- Delete any transaction have a date over my high date
    lnOldRec = Recno()
    DELETE FOR dTranDate>HDATE
    *-- Go to the first record (After the deleting operations) to start calculating and printing
    GOTO TOP
  ENDIF
  *B122148,1 NNA (End)

ELSE  && else user selected Sequence Range
  INDEX ON cFabric+cColor+cTrn_Seq TAG (lcTempFile)

  *B122148,1 NNA 03/24/2004 (Begin) make a filtration to know if the (Fabric/Color) that I selected
  *B122148,1 NNA            Has transactions within my Sequence range and if found delete Colors that not have.
    *-- IF the user Input a Sequense Range, I'll delete the unneeded transaction that not have a Number
    *-- within my Range , but if there is no Sequense range I let the report to run normally to fix the 
    *-- problem of the speed
  IF !EMPTY(lcRplSeq)				
    *-- open the temp file again with another alias to make a relationship into itself
    SELECT 0
    IF !USED('lcTmp2')
      USE (gcWorkDir+lcTempFile) AGAIN ALIAS lcTmp2	
      SET ORDER TO TAG lcTempFile
    ELSE
      *-- Close the temp file and Reuse it to Recollecting data from the master temp file
      USE IN lcTmp2
      USE (gcWorkDir+lcTempFile) AGAIN ALIAS lcTmp2	
      SET ORDER TO TAG lcTempFile
    ENDIF
    SELECT (lcTempFile)
    SET RELATION TO (cFabric+cColor) INTO lcTmp2 ADDITIVE	
     *-- First search in the temp file if any Fabric/color Has a Tran. Date within my Range
    SET FILTER TO BETWEEN(cTrn_Seq,lcRplSeq,lcRphSeq)
    GO TOP
    COUNT TO X
    GO TOP
    IF X = 0
  	  =gfModalGen('TRM00052B40011','ALERT')
      *B608374,1 WAM 12/05/2007 Fix error appears after close report after the No records message displayed
      USE IN lcTmp2
      *B608374,1 WAM 12/05/2007 (End)
      RETURN
    ELSE
      SET FILTER TO      
      SELECT (lcTempFile)
      SCAN 
        SELECT lcTmp2
        lltrue = .F.
        SCAN REST WHILE cFabric+cColor+DTOS(dTranDate)+cTrn_Seq = &lcTempFile..cFabric+&lcTempFile..cColor
          *-- IF the (fabric/Color) has a transaction date within my range leave it
          IF BETWEEN(cTrn_Seq,lcRplSeq,lcRphSeq)	
            lltrue = .T.
            EXIT
          ENDIF
        ENDSCAN
        *-- IF the (fabric/Color) Has No transactions date within my range Delete it
        IF !lltrue
          SELECT (lcTempFile)
          DELETE
        ENDIF  
      ENDSCAN
    ENDIF
    SET FILTER TO
    SET RELATION TO
    SELECT lcTmp2
    USE IN lcTmp2
    SELECT (lcTempFile)
    *-- Delete any transaction have a Sequence No. over my high Seq. Range
    DELETE FOR VAL(cTrn_Seq)>VAL(lcRphSeq)
    *-- Go to the first record (After the deleting operations) to start calculating and printing
    GO TOP									
    *-- Close the relationship and close the new temp file Alias
  ENDIF    
  *B122148,1 NNA (End)

ENDIF  && end if user selected Date Range
*-- if no records are inserted into lcTempFile
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')

  *B605918,1 KHM 04/29/2002 (Begin) Setting the device to screen
  SET DEVICE TO SCREEN
  *B605918,1 KHM 04/29/2002 (End)

  RETURN
ENDIF  && end if no records are inserted into lcTempFile
SET RELATION TO cTktNo INTO MMFGORDH ADDITIVE
IF 'PO' $ gcCmpModules
  SET RELATION TO 'P'+cTktNo INTO POSHDR ADDITIVE
ENDIF
IF 'MF' $ gcCmpModules
  SET RELATION TO cTktNo INTO CUTTKTH ADDITIVE
ENDIF

*-- XTOTCLR(1) : is an array element used to accumulate the PO-RECEIPTS . 
*-- in case of MATINVJL.cTranType='1' (PO-RECEIPT) 
*-- then we have XTOTCLR(1)= XTOTCLR(1)+nReceived .
*-- XTOTCLR(2) : is an array element used to accumulate the ADJUSTMENTS .
*-- in case of MATINVJL.cTranType='2' (Adjustment) 
*-- then we have XTOTCLR(2)=XTOTCLR(2)+IIF(nReceived>0,nReceived,IIF(nIssued > 0,-nIssued,nIssued)) 
*-- XTOTCLR(3) : is an array element used to accumulate the USAGE .
*-- in case of MATINVJL.cTranType='3' (Physical) 
*-- then we have XTOTCLR(3)=XTOTCLR(3)+nIssued
*-- XTOTFAB() : is similar to XTOTCLR() but these are totals by fabric
*-- and the other ones are totals by color.
DIMENSION XTOTCLR(3)
DIMENSION XTOTFAB(3)
STORE 0.00 TO XTOTCLR,XTOTFAB
ROW     = 99           && row no 
PAGENO  = 0            && page no
TIME    = TIME()     
XREPORT = 'MAUTLJL'    && report name
XTITLE  = ' '          && report title

SET DEVICE TO PRINT
GO TOP                 && in lcTempFile
XFABRIC  = cFabric
XCOLOR   = cColor
XFTOTFLG =.F.
*-- laContr : stores the contractors' values
DIMENSION laContr[3]
*-- major print loop
DO WHILE INKEY() <> 32
  *-- print the header
  *-- if new page (row is initially 99 and for new page also) 
  IF ROW > 53
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW=5
  ENDIF  && end if new page
  *-- XPRNT_ACT : is a flag saying that there is activity to print 
  *-- for this fabric/color 
  *-- totals by color
  IF (cFABRIC <> XFABRIC .OR. cCOLOR <> XCOLOR) .AND. XPRNT_ACT
    @ ROW,00 SAY REPLICATE('=',133)
    ROW = ROW + 1
    @ ROW,01 SAY XFABRIC+' '+ XCOLOR
    @ ROW,20 SAY 'SUB TOTAL'
    @ ROW,37 SAY 'PO-RECEIPTS :'
    
    *--B602910,1 RAMY 02/11/2001 Add decimals to the totals fields [sart]
    *@ ROW,52 SAY XTOTCLR(1) PICTURE '99999999'
    @ ROW,52 SAY XTOTCLR(1) PICTURE '99999999.99'
    *--B602910,1 RAMY [end] 
    @ ROW,72 SAY 'ADJUSTMENTS :'
    
    *--B602910,1 RAMY 02/11/2001 Add decimals to the totals fields [sart]
    *@ ROW,87 SAY XTOTCLR(2) PICTURE '99999999'
    @ ROW,87 SAY XTOTCLR(2) PICTURE '99999999.99'
    *--B602910,1 RAMY [end] 
    @ ROW,107 SAY 'USAGE : '
    
    *--B602910,1 RAMY 02/11/2001 Add decimals to the totals fields [sart]
    *@ ROW,116 SAY XTOTCLR(3) PICTURE '99999999'
    @ ROW,116 SAY XTOTCLR(3) PICTURE '99999999.99'
    *--B602910,1 RAMY [end]
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('=',133)
    XTOTCLR(1) = 0
    XTOTCLR(2) = 0
    XTOTCLR(3) = 0
  ENDIF  && end if TOTALS BY COLOR
  *-- totals by fabric
  IF cFABRIC <> XFABRIC .AND. XPRNT_ACT
    ROW = ROW + 1
    @ ROW,01 SAY XFABRIC
    @ ROW,20 SAY 'TOTAL'
    @ ROW,37 SAY 'PO-RECEIPTS :'

    *--B602910,1 RAMY 02/11/2001 Add decimals to the totals fields [sart]
    *@ ROW,51 SAY XTOTFAB(1) PICTURE '999999999'
    @ ROW,51 SAY XTOTFAB(1) PICTURE '999999999.99'
    *--B602910,1 RAMY [end]
    @ ROW,72 SAY 'ADJUSTMENTS :'

    *--B602910,1 RAMY 02/11/2001 Add decimals to the totals fields [sart]
    *@ ROW,86 SAY XTOTFAB(2) PICTURE '999999999'
    @ ROW,86 SAY XTOTFAB(2) PICTURE '999999999.99'
    *--B602910,1 RAMY [end]
    @ ROW,107 SAY 'USAGE : '

    *--B602910,1 RAMY 02/11/2001 Add decimals to the totals fields [sart]
    *@ ROW,115 SAY XTOTFAB(3) PICTURE '999999999'
    @ ROW,115 SAY XTOTFAB(3) PICTURE '999999999.99'
    *--B602910,1 RAMY [end]
    
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('=',133)
    XTOTFAB(1) = 0
    XTOTFAB(2) = 0
    XTOTFAB(3) = 0
  ENDIF  && end if TOTALS BY FABRIC
  *-- if eof(), exits print loop
  IF EOF()
    EXIT
  ENDIF  && end if eof()
  XFABRIC  = cFABRIC
  XCOLOR   = cCOLOR
  XBAL     = 0
  XNEWBAL  = 0
  XPRV_REC = 0
  XPRV_USG = 0
  *-- Calculate the starting inventory balance for this fabric/color .
  *-- If there's no starting activity date entered then start printing
  *-- the report with starting balance zero. Or if starting activity date 
  *-- is earlier than the first transaction date for this fabric/color.
  *-- if select by date 
  IF lcrpSortby='D'
    *-- entered date range is stored in lDate, hDate
    *-- if a date range is entered
    IF !EMPTY(lDate) .AND. dTranDate < lDate
      *-- scan lcTempFile for the same fabric and same color and transaction date less than the first entered date 
      SCAN WHILE cFABRIC+cCOLOR = XFABRIC+XCOLOR .AND. dTranDate < lDate
        *-- case transaction type  
        DO CASE
           CASE cTranType='1'   && po-receipt
             XBAL     = XBAL     + nReceived
             XPRV_REC = XPRV_REC + nReceived
           CASE cTranType='2'   && adjustment
             XBAL   = XBAL + (nReceived-nIssued)

             *B037177,1 ABD - Print the locking records as the phisical records. [Begin]
           *CASE cTranType='3'   && physical
           CASE cTranType $ '39'   && physical and Locking
             *B037177,1 ABD - [End]
             
             XBAL   = xbal+(nReceived-nIssued)
           CASE cTranType='4'   && issue/return fabric in cuttkt
             XBAL     = XBAL     -  IIF(nIssued <> 0 ,nIssued,-nReceived)
             XPRV_USG = XPRV_USG +  IIF(nIssued <> 0 ,nIssued,-nReceived)
        ENDCASE && end case transaction type  
      ENDSCAN  && end scan lcTempFile 
    ENDIF  && end if a date range is entered
  ELSE  && else select by sequence no (MATINVJL.CTRN_SEQ)
    *-- if user seleted range of sequence 
    IF !EMPTY(lcRplSeq) .AND. cTrn_Seq < lcRplSeq  
      *-- scan lcTempFile for the same fabric and same color and  transaction sequence no less than selected low sequence 
      SCAN WHILE (cFABRIC+cCOLOR = XFABRIC+XCOLOR) .AND. (cTrn_Seq < lcRplSeq)
        *-- case transaction type 
        DO CASE
           CASE cTranType='1'  && po-receipt
             XBAL     = XBAL     + nReceived
             XPRV_REC = XPRV_REC + nReceived
           CASE cTranType='2'  && adjustment
             XBAL   = XBAL +(nReceived-nIssued)

             *B037177,1 ABD - Print the locking records as the phisical records. [Begin]
           *CASE cTranType='3'  && physical
           CASE cTranType $ '39'  && physical and Locking
             *B037177,1 ABD - [End]
             
             XBAL    = xbal+(nReceived-nIssued)
           CASE cTranType='4'  && issue/return fabric in cuttkt
             XBAL     = XBAL     -  IIF(nIssued <> 0 ,nIssued,-nReceived)
             XPRV_USG = XPRV_USG +  IIF(nIssued <> 0 ,nIssued,-nReceived)
        ENDCASE  && end case transaction type 
      ENDSCAN  && end scan lcTempFile 
    ENDIF  && end if user seleted range of sequence 
  ENDIF  && end if select by date 
  XNEWBAL=XBAL                  && NEWBAL=BALANCE
  XPRNT_ACT=.T.                 && flag saying that there's activity to print
                                && for this fabric/color
  *-- if another fabric or another color
  IF cFABRIC+cCOLOR <> XFABRIC+XCOLOR
    XPRNT_ACT=.F.               && no more activity for this fabric/color
  ENDIF  && end if another fabric or another color
  *-- get some informations from the fabric file
  SELECT FABRIC
  SEEK XFABRIC+XCOLOR
  *-- if the fabric exists
  IF FOUND()
    XDESC     = DESC
    XPATTERN  = PATTERN
    XVENDOR   = VENDOR
    XLOC      = LOC
    XWIDTH    = WIDTH
  ELSE  && else fabric not exist
    XDESC     = ''
    XPATTERN  = ''
    XVENDOR   = ''
    XLOC      = ''
    XWIDTH    = ''
  ENDIF  && end if the fabric exists
  SELECT (lcTempFile)
  *-- print information about that fabric/color
  *B803660,1 HBG 10/01/2000 Return the flag to false because this is new fabric/color[Begin]
  llBegin = .F.
  *B803660,1 [End]
  ROW=ROW+1
  @ ROW,01 SAY 'ITEM   :'
  @ ROW,10 SAY XFABRIC
  @ ROW,18 SAY 'COLOR :'
  @ ROW,26 SAY XCOLOR
  @ ROW,36 SAY 'DESC :'
  @ ROW,43 SAY XDESC
  @ ROW,70 SAY 'LOC:'
  @ ROW,76 SAY XLOC
  @ ROW,104 SAY 'PREVIOUS RECEIPTS :'
  @ ROW,124 SAY XPRV_REC PICTURE '99999999'
  ROW=ROW+1
  @ ROW,01 SAY 'VENDOR :'
  @ ROW,10 SAY XVENDOR
  @ ROW,33 SAY 'PATTERN :'
  @ ROW,43 SAY XPATTERN
  @ ROW,68 SAY 'WIDTH :'
  @ ROW,76 SAY XWIDTH
  @ ROW,104 SAY 'PREVIOUS    USAGE :'
  @ ROW,124 SAY XPRV_USG PICTURE '99999999'
  ROW = ROW+1
  @ ROW,95 SAY 'STARTING INVENTORY BALANCE :'
  
  @ ROW,124 SAY XBAL PICTURE '99999999'
  
  ROW=ROW+1
  *-- if no more activity to print 
  IF .NOT. XPRNT_ACT
    @ ROW,000 SAY REPLICATE('-',133)
    LOOP
  ENDIF  && end if no more activity to print 
  *-- print the transactions between activity 
  @ ROW,000 SAY REPLICATE('-',133)
  ROW=ROW+1
  *-- if select by date
  IF lcrpSortby='D'

    *--HDM B802245,1[Start] re-position header
    *@ ROW,001 SAY 'DATE     TYPE         PO/CT#  VENDOR1  VENDOR2  VENDOR3 REFERENCE           UNT CST.  ITM CST     BALANCE RCVD/USED/ADJ     NEW BAL.'
    *B803660,1 HBG 10/01/2000 Moving the Position of  "UNT CST. & ITM CST " [Begin]
    *@ ROW,001 SAY 'DATE     TYPE       PO/CT#  VENDOR1  VENDOR2  VENDOR3 REFERENCE           UNT CST.  ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    *C200258,1 WAB (Start) insert the dyelot columns
     *@ ROW,001 SAY 'DATE     TYPE       PO/CT#  VENDOR1  VENDOR2  VENDOR3 REFERENCE          UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    IF !llRpPrtDy
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 VENDOR3 REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ELSE
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 VENDOR3 REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ENDIF
    *C200258,1 WAB (End)
    *B803660,1 [End]
    *--HDM B802245,1[End]

  ELSE  && else select by sequence no (MATINVJL.CTRN_SEQ)

    *--HDM B802245,1[Start] re-position header
    *@ ROW,001 SAY 'DATE     TYPE         PO/CT#  VENDOR1  VENDOR2  SEQ. NO REFERENCE           UNT CST.  ITM CST     BALANCE RCVD/USED/ADJ     NEW BAL.'
    *B803660,1 HBG 10/01/2000 Moving the Position of  "UNT CST. & ITM CST " [Begin]
    *@ ROW,001 SAY 'DATE     TYPE       PO/CT#  VENDOR1  VENDOR2  SEQ. NO REFERENCE           UNT CST.  ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    *C200258,1 WAB (Start) - insert the dyelot column
     *@ ROW,001 SAY 'DATE     TYPE       PO/CT#  VENDOR1  VENDOR2  SEQ. NO REFERENCE          UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    IF !llRpPrtDy
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 SEQ. NO REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ELSE
      @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 SEQ. NO REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
    ENDIF
    *C200258,1 WAB (End)
    *B803660,1 [End]
    *--HDM B802245,1[End]

  ENDIF  && end if select by date 
  ROW=ROW+1
  @ ROW,00 SAY REPLICATE('-',133)
  ROW=ROW+1
  *-- scan lcTempFlie for the same fabric and the same color
  SCAN WHILE cFABRIC+cCOLOR= XFABRIC+XCOLOR
    
    *-- if selected date and the transaction date is exceeding the selected high date
    IF !EMPTY(hDate) .AND. (dTranDate > hDate)
       EXIT
    ENDIF  && end if selected date and the transaction date is exceeding the selected high date 
    XBAL     = XNEWBAL
    XTOTCOST = 0
    *-- case transaction type
    DO CASE
       CASE cTranType='1'        && po receipt      
         *B603381,1 (Begin) If ciSession isn't empty, then it's issure returm PO, otherwise Receice PO.         
         *XNEWBAL    = XNEWBAL    + nReceived
         *XTOTCLR(1) = XTOTCLR(1) + nReceived
         *XTOTFAB(1) = XTOTFAB(1) + nReceived
         *XTOTCOST   =  nReceived * nUnitCost
         IF EMPTY(CiSession) && PO Received
           XNEWBAL    = XNEWBAL    + nReceived
           XTOTCLR(1) = XTOTCLR(1) + nReceived
           XTOTFAB(1) = XTOTFAB(1) + nReceived
           XTOTCOST   =  nReceived * nUnitCost
         ELSE && PO Return
           XNEWBAL    = XNEWBAL    + nIssued
           XTOTCLR(1) = XTOTCLR(1) + nIssued
           XTOTFAB(1) = XTOTFAB(1) + nIssued
           XTOTCOST   =  nIssued * nUnitCost
         ENDIF
         *B603381,1 (End)
       CASE cTranType='2'        && adjustment
         
         XNEWBAL=XNEWBAL         +nReceived-nIssued
         XTOTCLR(2)  = XTOTCLR(2)+nReceived-nIssued
         XTOTFAB(2) = XTOTFAB(2) +nReceived-nIssued

       *B037177,1 ABD - Print the locking records as the phisical records. [Begin]
       *CASE cTranType='3'        && physical
       CASE cTranType $ '39'      && physical and Locking
         *B037177,1 ABD - [End]
            
         XNEWBAL     =  XNEWBAL+nReceived-nIssued
         
       CASE cTranType='4'        && issue/return fabric in cuttkt
            
         XNEWBAL    = XNEWBAL       -  IIF(nIssued <> 0 ,nIssued,-nReceived)
         XTOTCLR(3) = XTOTCLR(3)    +  IIF(nIssued <> 0 ,nIssued,-nReceived)
         XTOTFAB(3) = XTOTFAB(3)    +  IIF(nIssued <> 0 ,nIssued,-nReceived)
         XTOTCOST   = IIF(nIssued <> 0 ,nIssued,-nReceived) *  nUnitCost
         
    ENDCASE  && end case transaction type
    SELECT (lcTempFile)
    *-- if page end, check for page breake
    IF ROW > 53
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
      *B803660,1 HBG 10/01/2000 Set the flag because this is new page inside the 
      *B803660,1                same fabric/color or not[Begin]
      llBegin = .T.
      *B803660,1 [End]
      ROW = 5
      IF lcrpSortby='D'
        *B803660,1 HBG 10/01/2000 Moving the Position of  "UNT CST. & ITM CST " [Begin]
        *@ ROW,001 SAY 'DATE     TYPE         PO/CT#  VENDOR1  VENDOR2  VENDOR3 REFERENCE           UNT CST.  ITM CST     BALANCE RCVD/USED/ADJ     NEW BAL.'
        *C200258,1 WAB (Start) - insert the dyelot column
        *@ ROW,001 SAY 'DATE     TYPE         PO/CT#  VENDOR1  VENDOR2  VENDOR3 REFERENCE          UNT CST.   ITM CST     BALANCE RCVD/USED/ADJ     NEW BAL.'
        IF !llRpPrtDy
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 VENDOR3 REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ELSE
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 VENDOR3 REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ENDIF
        *C200258,1 WAB (End)
        *B803660,1 [End]
      ELSE
        *B803660,1 HBG 10/01/2000 Moving the Position of  "UNT CST. & ITM CST " [Begin]
        *@ ROW,001 SAY 'DATE     TYPE         PO/CT#  VENDOR1  VENDOR2  SEQ. NO REFERENCE           UNT CST.  ITM CST     BALANCE RCVD/USED/ADJ     NEW BAL.'
        *C200258,1 WAB (Start) insert the dyelot column
        *@ ROW,001 SAY 'DATE     TYPE         PO/CT#  VENDOR1  VENDOR2  SEQ. NO REFERENCE          UNT CST.   ITM CST     BALANCE RCVD/USED/ADJ     NEW BAL.'
        IF !llRpPrtDy
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 SEQ. NO REFERENCE              UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ELSE
          @ ROW,000 SAY 'DATE     TYPE       PO/CT# VENDOR1 VENDOR2 SEQ. NO REFERENCE   DYELOT     UNT CST.   ITM CST       BALANCE RCVD/USED/ADJ     NEW BAL.'
        ENDIF
        *C200258,1 WAB (End)
        *B803660,1 [End]
      ENDIF  
      ROW=ROW+1
      @ ROW,000 SAY REPLICATE('-',133)
      ROW=ROW+1
    ENDIF  && end if page end
    *-- case transaction type
    DO CASE
       CASE cTranType = '1'
         
         *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
         *@ ROW,1 SAY dTranDate
         
         @ ROW,00 SAY dTranDate
         
         *C200258,1 WAB (End) 
         
         *B603381,1 (Begin) If iSession isn't empty, then it's issure returm PO, otherwise Receice PO.
         *@ ROW,10 SAY 'PO-RECEIPT'
         llMFO = (SEEK(crsession+cTran+cfabric+Ccolor+'2','mmfgordd'))
         
         *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
         *@ ROW,10 SAY IIF(!llMFO,IIF(EMPTY(CiSession),'PO-RECEIPT','PO-RETURN'),'MF-RECEIPT')
         
         @ ROW,09 SAY IIF(!llMFO,IIF(EMPTY(CiSession),'PO-RECEIPT','PO-RETURN'),'MF-RECEIPT')
         
         *C200258,1 WAB (End) 
         
         *B603381,1 (End)
         *-- get some informations from purchase order fabric line for the fabric/color  
         *-- if found record for that fabric/color in POFLN
         *B603381,1 (Begin) If iSession isn't empty, then it's issure returm PO, otherwise Receice PO.
         *IF SEEK(cFabric+cColor+'P'+cTran+'2','PofLn')
         IF SEEK(cFabric+cColor+IIF(EMPTY(CiSession),'P','R')+cTran+'2','PofLn')
         *B603381,1 (End)
           SELECT PofLn   
           LOCATE FOR &lcTempFile..cTrn_Seq=cRsession;
                  WHILE &lcTempFile..cFabric+&lcTempFile..cColor = Fabric+Color
           *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
           *@ ROW,21 SAY PofLn.POMAT
           *@ ROW,29 SAY PofLn.VENDOR
           *SELECT (lcTempFile)
           *@ ROW,48 SAY IIF (lcrpSortby='S',cTrn_Seq,'')           
           **B604518,1 WAB (Start) ALine the refrence field with the colun headr
           *@ ROW,54 SAY SUBSTR(PofLn.REFERENCE,1,15)
           *@ ROW,55 SAY SUBSTR(PofLn.REFERENCE,1,15)

           @ ROW,20 SAY PofLn.POMAT
           @ ROW,27 SAY PofLn.VENDOR
           SELECT (lcTempFile)
           @ ROW,43 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
           @ ROW,51 SAY SUBSTR(PofLn.REFERENCE,1,IIF(llRpPrtDy ,11,22))
           IF llRpPrtDy
             @ ROW,63 SAY PofLn.Dyelot
           ENDIF
           
           *C200258,1 WAB (End) 
           
           *:B604518,1 WAB (END)
         *B603381,1 (Begin) See if it's MFO
         ELSE
           IF SEEK(crsession+cTran+cfabric+Ccolor+'2','mmfgordd')
             *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
             *@ ROW,21 SAY mmfgordd.cMfgOrdNo
             **B604518,1 WAB (Start) ALine the refrence field with the colun headr
             *@ ROW,54 SAY SUBSTR(mmfgordd.REFERENCE,1,15)
             *@ ROW,55 SAY SUBSTR(mmfgordd.REFERENCE,1,15)

             @ ROW,20 SAY mmfgordd.cMfgOrdNo
             @ ROW,51 SAY SUBSTR(mmfgordd.REFERENCE,1,IIF(llRpPrtDy ,11,22))
             IF llRpPrtDy
               @ ROW,63 SAY mmfgordd.Dyelot
             ENDIF
             
             *C200258,1 WAB (End) 
             
             *B604518,1 WAB (End) 
           ENDIF
         *B603381,1 (End)
         ENDIF  && end if found record for that fabric/color in POFLN
         *B803660,1 HBG 10/01/2000 Moving the Position of  "UNT CST. & ITM CST " 
         *B803660,1                According to the flag of new page[Begin]
         *@ ROW,75 SAY nUnitCost PICTURE '9999.999'
         *@ ROW,82 SAY XTOTCOST PICTURE '999999.999'
         IF !llBegin
           @ ROW,74 SAY nUnitCost PICTURE '9999.999'
           @ ROW,81 SAY XTOTCOST PICTURE '999999.999'
         ELSE
           @ ROW,76 SAY nUnitCost PICTURE '9999.999'
           @ ROW,83 SAY XTOTCOST PICTURE '999999.999'
         ENDIF  
         *B803660,1 [End]  
         *--B802245,1 HDM [Start]
         *@ ROW,95 SAY XBAL PICTURE '9999999.999'
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
         *B603381,1 (Begin) If iSession isn't empty, then it's issure returm PO, otherwise Receice PO.         
         *@ ROW,107 SAY nReceived PICTURE '999999999.999'
         @ ROW,107 SAY IIF(EMPTY(CiSession),nReceived,nIssued) PICTURE '999999999.999'
         *B603381,1 (End)
         *--B802245,1 HDM [Start]
         *@ ROW,122 SAY XNEWBAL PICTURE '9999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
       CASE cTranType='2'
         *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
         *@ ROW,1 SAY dTranDate
         *@ ROW,10 SAY 'ADJUSTMENT'
         *@ ROW,30 SAY SUBSTR(REFERENCE,1,17)
         *@ ROW,48 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
         @ ROW,00 SAY dTranDate
         @ ROW,09 SAY 'ADJUSTMENT'
         @ ROW,27 SAY SUBSTR(REFERENCE,1,15)
         @ ROW,43 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
         IF llRpPrtDy
           @ ROW,63 SAY cDyelot
         ENDIF
         *C200258,1 WAB (End) 
         
         *--B802245,1 HDM[Start]
         *@ ROW,95 SAY XBAL PICTURE '9999999.999'
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
         @ ROW,108 SAY nReceived-nIssued PICTURE '99999999.999'
         *--B802245,1 HDM[Start]
         *@ ROW,122 SAY XNEWBAL PICTURE '9999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
         
       *B037177,1 ABD - Print the locking records as the phisical records. [Begin]
       *CASE cTranType='3'
       CASE cTranType $ '39'      && physical and Locking
         *B037177,1 ABD - [End]
       
         *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
         *@ ROW,1 SAY dTranDate
         *@ ROW,10 SAY '*PHYSICAL*'
         *@ ROW,29 SAY SUBSTR(REFERENCE,1,15)
         *@ ROW,48 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
         @ ROW,0 SAY dTranDate
         
         *B037177,1 ABD - Print the locking records as the phisical records. [Begin]
         *@ ROW,09 SAY '*PHYSICAL*'
         @ ROW,09 SAY IIF(cTranType='3','*PHYSICAL*','Locking')
           *B037177,1 ABD - [End]
         
         @ ROW,20 SAY SUBSTR(REFERENCE,1,17)
         @ ROW,43 SAY IIF (lcrpSortby='S',cTrn_Seq,'')
         IF llRpPrtDy
           @ ROW,63 SAY cDyelot
         ENDIF
         
         *C200258,1 WAB (End) 
         
         *--B802245,1 HDM[Start]
         *@ ROW,95 SAY XBAL PICTURE '9999999.999'
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
         @ ROW,107 SAY nReceived-nIssued PICTURE '999999999.999'
         *--B802245,1 HDM[Start]
         *@ ROW,122 SAY XNEWBAL PICTURE '9999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
       CASE cTranType='4'            && issue/return fabric for cuttkt/po 
         *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
         *@ ROW,1 SAY dTranDate
         @ ROW,0 SAY dTranDate
         *C200258,1 WAB (End) 
         *-- the saving procedure of the material po cost sheet 
         *-- in MATINVJL is the same like cuttkt cost sheet.
         *-- one of the solutions is to distinguish between them
         *-- by adding CIMTYP Character (01) (as in MFGOPRHD).
         *-- thus , we 'll use that column to distinguish between
         *-- the two states (instead of checking EMPTY(cTktNo))
         *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column 
         *@ ROW,10 SAY 'USAGE'
         *@ ROW,21 SAY cTktNo
         @ ROW,09 SAY 'USAGE'
         @ ROW,20 SAY cTktNo
         *C200258,1 WAB (End) 
         *-- a maximum of three contractors are printed .
         
         lnCurAlias = SELECT(0)
         SELECT MFGOPRHD
         *B603381,1 (Begin) Set order to sequence.
         SET ORDER TO Tktoper
         *B603381,1 (End)
         STORE SPACE(08) TO laContr
         lnInd  = 1
         =SEEK(&lcTempFile..CIMTYP+&lcTempFile..cTktNo)
         SCAN WHILE CIMTYP+CTKTNO = &lcTempFile..CIMTYP+&lcTempFile..cTktNo FOR !lInHouse
           laContr[lnInd] = MFGOPRHD.CCONTCODE
           lnInd = lnInd+1 
           IF lnInd > 3     && to limit printing 3 contractors only
             EXIT
           ENDIF  
         ENDSCAN
         SELECT (lnCurAlias)
         *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
         *@ ROW,29 SAY laContr[1]
         *@ ROW,38 SAY laContr[2]
         *@ ROW,48 SAY SUBSTR(IIF(lcrpSortby='D',laContr[3],cTrn_Seq),1,7)
         @ ROW,27 SAY laContr[1]
         @ ROW,34 SAY laContr[2]
         @ ROW,43 SAY SUBSTR(IIF(lcrpSortby='D',laContr[3],cTrn_Seq),1,7)
         *C200258,1 WAB (End) 
         IF INLIST(CIMTYP,'M','I','T')
           DO CASE
             CASE CIMTYP='M'
               lcFile='CUTTKTH'
             CASE CIMTYP='I'
              lcFile ='POSHDR'
             CASE CIMTYP='T'
              lcFile = 'MMFGORDH'
           ENDCASE
           *-- case status 
           *B604518,1 WAB (Start) print only the One character fro the status
           *DO CASE
           *  CASE &lcFile..STATUS='O'
           *    @ ROW,56 SAY 'OPEN'
           *  CASE &lcFile..STATUS='X'
           *    @ ROW,56 SAY 'CANCELLED'
           *  CASE &lcFile..STATUS='C'
           *    @ ROW,56 SAY 'COMPLETED'
           *  CASE &lcFile..STATUS='H'
           *    @ ROW,56 SAY 'HOLD'
           *  CASE &lcFile..STATUS='A'
           *    @ ROW,56 SAY 'ACTUALIZED'
           *  CASE &lcFile..STATUS='S'
           *    @ ROW,56 SAY 'CLOSED'
           *ENDCASE  && end case status 
           *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
           *@ ROW,55 SAY &lcFile..STATUS + "/"
           @ ROW,51 SAY &lcFile..STATUS + "/"
           *C200258,1 WAB (End) 
           *B604518,1 WAB (END)
         ENDIF  
         *-- if there's cuttkt, print cuttktkh.style 
         IF CIMTYP = 'M'
           *B604518,1 WAB (Start) Print the style ith maximum 16 character
           *@ ROW,67 SAY SUBSTR(CUTTKTH.STYLE,1,6)
           *C200258,1 WAB (Start) - moving the position because we have inserted the dyelot column
           *@ ROW,57  SAY SUBSTR(CUTTKTH.STYLE,1,16)
           @ ROW,53  SAY SUBSTR(CUTTKTH.STYLE,1,IIF(llRpPrtDy ,9,19))

           IF llRpPrtDy
             @ ROW,63 SAY cDyelot
           ENDIF
           *C200258,1 WAB (End) 
           *B604518,1 WAB (END)
         ENDIF  && end if there's cuttkt
         *B803660,1 HBG 10/01/2000 Moving the Position of  "UNT CST. & ITM CST " 
         *B803660,1                According to the flag of new page[Begin]
         *@ ROW,73 SAY nUnitCost PICTURE '9999.999'
         *@ ROW,82 SAY XTOTCOST PICTURE '999999.999'
         IF !llBegin
           @ ROW,74 SAY nUnitCost PICTURE '9999.999'
           @ ROW,81 SAY XTOTCOST PICTURE '999999.999'
         ELSE
           @ ROW,76 SAY nUnitCost PICTURE '9999.999'
           @ ROW,83 SAY XTOTCOST PICTURE '999999.999'
         ENDIF  
         *B803660,1[End]
         *--B802245,1 HDM[Start]
         *@ ROW,95 SAY XBAL PICTURE '9999999.999'
         @ ROW,94 SAY XBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
         @ ROW,107 SAY IIF(nReceived=0,-nIssued,nReceived) PICTURE '999999999.999'
         *--B802245,1 HDM[Start]
         *@ ROW,122 SAY XNEWBAL PICTURE '9999999.999'
         @ ROW,121 SAY XNEWBAL PICTURE '99999999.999'
         *--B802245,1 HDM[End]
    ENDCASE  && end case transaction type
    ROW=ROW+1
  ENDSCAN  && end scan lcTempFlie for the same fabric and the same color
  *-- IF FABRIC+COLOR=XFABRIC+XCOLOR THEN IT MEANS THAT WE REACHED THE ENDING
  *-- ACTIVITY DATE AND THERE ARE MORE TRANSACTIONS FOR THIS FAB/CLR. SO WE HAVE
  *-- TO SCAN UNITL FABRIC+COLOR<>XFABRIC+XCOLOR (i.e next fabric/color)
  SELECT (lcTempFile)
  IF CFABRIC+CCOLOR=XFABRIC+XCOLOR
     SCAN WHILE CFABRIC+CCOLOR=XFABRIC+XCOLOR
     ENDSCAN
  ENDIF
ENDDO  && end major print loop
DO ENDREPORT                   && end report 
SET DEVICE TO SCREEN           && return set device to screen 
RETURN                         && return to the caller which is gfopgrid (option grid program)
*-- end of report code
*--------------------- Functions' Section ---------------------
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : IHB
*! Date      : 12/02/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
R_WIDTH  = 'W'       && Width of report either N',W or XW
=lfGetColor()

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor
DIME laMajSeg[1,1]
*B038431,1 NNA 09/09/2004 (Begin) Check if there is item code structure or not
IF lfSegInfo()
*B038431,1 NNA (End)
 
  =gfItemMask(@laMajSeg)
  FOR lnCnt=1 TO ALEN(laMajSeg,1)
    *--Check for existance of color segment in style structure.
    IF laMajSeg[lnCnt,1]='C'
      *--Get the color length and width.
      lnClrSrt = laMajSeg[lnCnt,4]
      lnClrEnd = LEN(laMajSeg[lnCnt,3])
      EXIT
    ENDIF
  ENDFOR

*B038431,1 NNA (Begin)
ENDIF
*B038431,1 NNA (End)

RETURN

*!*************************************************************
*! Name      : lfvFabBet
*! Developer : IHB
*! Date      : 12/02/1998
*! Purpose   : Showes range screen for fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabBet()
*!*************************************************************
FUNCTION lfvFabBet
lcTitle = 'Item'
STORE lcRPFrmFab TO lcFromFab
STORE lcRPToFab  TO lcToFab
DO (gcRepHome + gcAct_Appl + '\FabRng.SPR')
lcRPFrmFab  = lcFromFab
lcRpToFab   = lcToFab
 
*!*************************************************************
*! Name      : lfvFabRng
*! Developer : IHB
*! Date      : 12/03/1998
*! Purpose   : To validate the fabric code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabRng()
*!*************************************************************
FUNCTION lfvFabRng
*-- this is the validation of from_item
lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.
IF !EMPTY(lcRngVarVa)
  SELECT FABRIC
  IF !SEEK(lcRngVarVa)
    DO FABROW WITH lcRngVarVa,'*'
    &lcRngVarNa = lcRngVarVa
  ENDIF
ENDIF
SHOW GET &lcRngVarNa

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : IHAB
*! Date      : 12/03/1998
*! Purpose   : Vailadet range screen's OK button 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*FUNCTION lfvpbOk
FUNCTION lfvpbFabOk
*B603955,1 ABD - [End]
*-- this is the validation of to_item
IF !EMPTY(lcFromFab) .OR. !EMPTY(lcToFab)
  IF EMPTY(lcToFab) 
    lcToFab = lcFromFab
  ENDIF
  IF lcFromFab > lcToFab
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFromFab)
  ELSE
    lcRPFrmFab = lcFromFab
    lcRPToFab  = lcToFab
    CLEAR READ
  ENDIF
ELSE
  *-- CLEAR READ exits the active READ, 
  *-- and returns control to the previous READ level if the active READ 
  *-- is nested.
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : IHB
*! Date      : 12/06/1998
*! Purpose   : To validate the sort by option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
CLEAR READ

*!*************************************************************
*! Name      : lfvSeq
*! Developer : IHB
*! Date      : 12/11/1998
*! Purpose   : Valid function of the Sequence field (MATINVJL.CTRN_SEQ).
*!*************************************************************
*! Called from : Option Grid [Sequence field option]
*!*************************************************************
*! Calls       : gfBrows().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSeq()
*!*************************************************************
FUNCTION lfvSeq

*******************************************************************
*******************************************************************
*-- THE FOLLOWING CODE IS COMMENTED DUE TO - IF NEEDED FOR BROWSING
*-- FROM MATINVJL FILE 
*PRIVATE lcVar, lcObj, lnCurTag
*lnCurTag = VAL(SYS(21))
*lcVar    = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
*lcObj    = ALLTRIM(EVALUATE(SYS(18)))   && Varible to hold the current field value
*SELECT MATINVJL
*SET ORDER TO TAG MTINVSEQ 
*-- If Statement to check if we are going to Browse
*IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'MATINVJL'))
*  DIMENSION laTemp[1]
*  laTemp = ''          && Array to hold the Selected value
*  lcBrFields = "CTRN_SEQ :R :H= 'Seq.#' , CFABRIC :R :H= 'Fabric' , CCOLOR :R :H= 'Color' ,CWARECODE :R :H= 'Location' , CDYELOT :R :H= 'Dyelot' "
*  lcFile_Ttl = "Activity Sequence"
*  lcBrowCond = ""
*  = gfBrows(lcBrowCond,'CTRN_SEQ','laTemp')
*  IF !EMPTY(laTemp[1])
*    &lcVar = ctrn_seq
*  ELSE
*    &lcVar = SPACE(06)
*  ENDIF
*  lcRplSeq  =  lcOGValueF
*  lcRphSeq  =  lcOGValueTo
*ENDIF
*IF EMPTY(lcOGValueF) AND !EMPTY(lcOGValueTo)
*  lcOGValueF = lcOGValueTo
*ENDIF
*SELECT MATINVJL
*SET ORDER TO (lnCurTag)
*******************************************************************
*******************************************************************

PRIVATE lcFrom,lcTo
STORE lcRplSeq  TO lcFrom,lcOldFrm
STORE lcRphSeq  TO lcTo,lcOldTo
STORE "@! X99999" TO lcPict
lcTitle  = 'Sequence Range'
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\ObjRng.SPR')
DO ObjRng.Spx
*B603955,1 ABD - [End]


*!*************************************************************
*! Name      : lfOldVal
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Saving old value
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOldVal()
*!*************************************************************
FUNCTION lfOldVal
lcObjNam = VARREAD()
DO CASE
  CASE UPPER(lcObjNam) = "LCFROM"
    lcOldFrm = &lcObjNam
  CASE UPPER(lcObjNam) = "LCTO"
    lcOldTo = &lcObjNam
ENDCASE    
 
*!*************************************************************
*! Name      : lfvObj
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate Account OR style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvObj()
*!*************************************************************
FUNCTION lfvObj
lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.
*AAN
*IF !EMPTY(lcRngVarVa) .AND. !SEEK(lcRngVarVa , 'MATINVJL')
*  RETURN .F.
lnAlias = SELECT()
SELECT MATINVJL
IF !EMPTY(lcRngVarVa) .AND. ('?' $ lcRngVarVa .OR. !SEEK(lcRngVarVa))
  DIMENSION laTemp[1]
  laTemp = ''          && Array to hold the Selected value
  lcBrFields = "CTRN_SEQ :R :H= 'Seq.#' , CFABRIC :R :H= 'Fabric' , CCOLOR :R :H= 'Color' ,CWARECODE :R :H= 'Location' , CDYELOT :R :H= 'Dyelot' "
  lcFile_Ttl = "Activity Sequence"
  lcBrowCond = ""
  = gfBrows(lcBrowCond,'CTRN_SEQ','laTemp')
  IF !EMPTY(laTemp[1])
    &lcRngVarNa = ctrn_seq
  ELSE
    &lcRngVarNa = SPACE(06)
    RETURN .F.
  ENDIF
ENDIF
SHOW GET lcRngVarNa
*!*************************************************************
*! Name      : lfvpbObjOk
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate range screen's ok button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjOk()
*!*************************************************************
FUNCTION lfvpbObjOk
*-- this is the validation of to_item
IF !EMPTY(lcFrom) .OR. !EMPTY(lcTo)
  IF EMPTY(lcTo) 
    lcTo = lcFrom
  ENDIF
  IF lcFrom > lcTo
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFrom)
  ELSE
    lcRplSeq  = lcFrom
    lcRphSeq  = lcTo
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfvpbObjCa
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate range screen's Cancel button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbObjCa()
*!*************************************************************
FUNCTION lfvpbObjCa
*-- this is the validation of to_item
STORE lcOldFrm TO lcFrom
STORE lcOldTo  TO lcTo
lcRplSeq  = lcFrom
lcRphSeq  = lcTo
CLEAR READ

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo
ldFrom = LDATE
LDTO   = HDATE
lcTitle = 'Date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*FUNCTION lfvpbDateOk
FUNCTION lfvpbOk
*B603955,1 ABD - [End]
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  LDate = ldFrom
  HDate = ldTo
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfChkSysDy
*! Developer : WAB - Walid A. Wahab
*! Date      : 12/27/1998
*! Purpose   : check if MA used dyeolt ( yes / no
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkSysDy()
*!*************************************************************
*C200258,1 WAB (Start) 
FUNCTION lfChkSysDy

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
*C200258,1 WAB (End)

*!*************************************************************
*! Name      : lfSegInfo
*! Developer : Nader Nabil (NNA)
*! Date      : 09/09/2004
*! Purpose   : Check if The item code Structure found or not.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns : False or True
*!*************************************************************
*! Example   : =lfSegInfo()
*!*************************************************************
*!038431 (Bld41)
FUNCTION lfSegInfo
lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  llOgtrmnat = .T.
  CLEAR READ  
  RETURN .F.
ENDIF
SELECT(lnalias)
RETURN
*-- End of function lfSegInfo.