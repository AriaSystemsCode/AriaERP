************************************************************************
*: Program file  : icStyUtl.PRG
*: Program desc. : Style Utilization Report
*:
*:         System: Aria Apparel System
*:      Developer: WAM - Wael Aly Mohamed
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*!*************************************************************
*:B602210,1 WAM 11/29/98 Change report header width and adjust data position
*:B602346,1 WAM 12/16/98 Consider that all issued transactions are stored
*:B602346,1              with nigative quantity
*:B801982,1 WAM 03/25/99 Get C/T contractor from MFGOPRHD file.
*:B802168,1 KHM 20/04/99 Fixed the bug of alias MFGOPRHD not found when
*:B802168,1              MF module is not installed.
*:E301248   AHM 06/03/99 Adding new Tran. type "I" in styinvjl file
*:E301248                for issunig style from PO/CT cost sheet
*:B802402,1 WAB - 07/25/99 - style filter needs to be changed to style browse 
*:B802402,1                  to enable select range 		
*:B802496,1 ABD 08/3/1999 Fixed The Bug alias (PosHdr) not Found When 
*:                        AP Module Is not Installed.
*:B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance
*:B803274,1 MHM 07/13/2000 change variable from reason to Creason according to table.
*:B604014,1 ABD 11/13/2000 Add warehouse option, Add option to the report selection grid.
*:E301800,1 BWA 01/21/2002 Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
*:B605419,1 BWA 01/28/2002 Fix the bug of the broken fields in case there is no records to display.
*:B605608,1 BWA 03/31/2002 Enhance the performance of the report.
*:B607409,1 ABD 07/21/2003 Fix error - that has no description - just tells you an error has occurred, 
*:B607409,1 ABD            This error happen because the program open file not exist, this file 
*:B607409,1 ABD            did't use at the current time.
*:B038431,1 NNA 08/21/2004 fix bug that if the 'RM' module not installed we will get an error massage
*:B038431,1 NNA            'Alias not found' because the [Rethdr] File not opened
*:B131768,1 TMI 04/25/2006 Fix the bug the Inventory does not coencide with the results of the Date sensitive report
*!*********************************************************************************

*--Open nessasary files.
*B802496,1 ABD [ BEGIN ] open nessasary files in some modules.
*llApVendor = 'AP' $ gcCmpModules AND gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VENCODE','SH')

*-- ApVendor Open With Modules AP,PS,PO,MA,MF
*-- PosHdr   Open With Modules PO,PS,RS
llApVendor = 'AP' $ gcCmpModules OR ;
             'PS' $ gcCmpModules OR ;
             'PO' $ gcCmpModules OR ;
             'MA' $ gcCmpModules OR ;
             'MF' $ gcCmpModules
llApVendor = llApVendor AND gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VENCODE','SH')
*B802496,1 ABD [ END ] open nessasary files in some modules.

llInvHdr   = 'AR' $ gcCmpModules AND gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')

*B802496,1 ABD [ BEGIN ] open nessasary files in some modules.
*llPosHdr  = 'PO' $ gcCmpModules AND gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
llPosHdr   = 'PO' $ gcCmpModules OR ;
             'PS' $ gcCmpModules OR ;
             'RS' $ gcCmpModules

llPosHdr = llPosHdr AND gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
*B802496,1 ABD [ END ] open nessasary files in some modules.

*B038431,1 NNA 08/21/2004 (Begin) OPen the [RetHdr] file if module [RM] or [PS] Installed
*llRetHdr   = 'RM' $ gcCmpModules AND gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
llRetHdr   = ('RM' $ gcCmpModules OR 'PS' $ gcCmpModules) AND gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
*B038431,1 NNA (End)

llCutTktH  = 'MF' $ gcCmpModules AND gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
*B801982,1 Get C/T contractor from MFGOPRHD file.
=IIF('MF' $ gcCmpModules,gfOpenFile(gcDataDir+'MFGOPRHD',gcDataDir+'TKTOPER','SH'),.T.)
*B801982,1 (End)

SELE STYLE
SET RELATION TO 'S'+Scale INTO Scale
lcActDates=laOgVrFlt[ASCAN(laOgVrFlt,'STYINVJL.DTRDATE')+5]
DO CASE 
  CASE EMPTY(lcActDates)
    *B801982,1 Get all activities when date range is empty
    STORE {}        TO ldLDate
    STORE gdSysDate TO ldHDate
    *STORE gdSysDate TO ldLDate,ldHDate
    **B602346,1 Add dates to filter
    *lcRpExp = lcRpExp + ' AND STYINVJL.DTRDATE=ldLDate'
    **B602346,1 (End)
    *B801982,1 (End)
  CASE AT('|',lcActDates) = 0
    STORE CTOD(lcActDates) TO ldLDate,ldHDate
  OTHERWISE
    STORE CTOD(SUBSTR(lcActDates,1,AT('|',lcActDates)-1) ) TO ldLDate
    STORE CTOD(SUBSTR(lcActDates,AT('|',lcActDates)+1))    TO ldHDate
ENDCASE  
lcStyUtil = gfTempName()
lcAccName = ' '
SELECT StyInvJL
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcStyUtil) FROM ARRAY laFileStru

*B605608,1 BWA 03/31/2002 Change the creation of the index to be when the temp file is empty.[START]
INDEX ON Style+cSession+DTOS(dTRDate)+cTrCode TAG (lcStyUtil)
*B605608,1 BWA 03/31/2002.[END]

*B131768,1  TMI [Start] Open the styinvjl with another alias to seek in for voided invoices 
lcSekInvJL = gfTempName()
USE (gcDataDir+'STYINVJL') AGAIN IN 0 ORDER MFGOPR ALIAS &lcSekInvJL
*B131768,1  TMI [End  ] 

SELECT StyInvJL
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET FILTER TO &lcRpExp
LOCATE
*B801982,1 Get all activities when date range is empty
STORE gdSysDate TO ldLowDate
*B801982,1 (End)

*B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
llAddTrn = .F.  && Flag to add Trnaction to lcStyUtil with Qty zero
*B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (End)

DO WHILE !EOF()
  lcKey    = Style

  *B605608,1 BWA 03/31/2002 Add a wait window to let the user check collecting the data.[START]
  WAIT WINDOW "Collecting Data for Style : " +  lcKey NOWAIT
  *B605608,1 BWA 03/31/2002.[END]

  lnFstInv = 0
  ldTrDt   = {}
  *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
  SCATTER MEMVAR BLANK
  *B604014,1 ABD Redifine this variable to void bug when inter one 
  *B604014,1 ABD Physical transaction with Zero qty. [Begin]
  llAddTrn = .F.
  *B604014,1 ABD [End]
  *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (End)

  *B605608,1 BWA 03/31/2002 Use the full index to activate the Rushmore.[START]
  *SCAN REST WHILE Style = lcKey FOR !INLIST(cTrType,'4','8')
  *B131768,1  TMI [Start] include all transactions 
  *SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcKey FOR !INLIST(cTrType,'4','8')
  SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcKey
  *B131768,1  TMI [End  ] 
  *B605608,1 BWA 03/31/2002.[END]

    *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
    IF llAddTrn .AND. Csession <> m.Csession
      IF m.Cirtype = 'R'
        SELECT (lcStyUtil)
        DELETE
        m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
        m.Cirtype = 'I'
        m.Cisession = m.Csession
        STORE '' TO m.Crsession , m.Cicacnt
        APPEND BLANK
        GATHER MEMVAR
        SELECT STYINVJL
      ENDIF
      m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
      m.Cirtype = 'R'
      m.Crsession = m.Csession
      STORE '' TO m.Cisession , m.Cicacnt
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        m.Nstk&lcCount = 0
      ENDFOR
      STORE 0 TO m.Ntotstk , Nstkval , Nprvsval , Lineno , Nprvsqty
      SELECT (lcStyUtil)
      APPEND BLANK
      GATHER MEMVAR
      llAddTrn = .F.
      SELECT STYINVJL
    ENDIF
    *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (End)
    
    *B801982,1 Get all activities when date range is empty
    ldLowDate = IIF(DTRDATE<ldLowDate,DTRDATE,ldLowDate)
    *B801982,1 (End)

    *B131768,1  TMI [Start] no need for this skipping since now all transactions are included
    *-* *Skip Voided Invoices
    *-* IF cTrType='3' AND SEEK(StyInvJl.cTrCode,'INVHDR') AND INVHDR.STATUS='V'
    *-*   LOOP
    *-* ENDIF
    *-* *Skip Voided Credit Memos
    *-* IF cTrType='7' AND SEEK(StyInvJl.cTrCode,'RETHDR') AND RETHDR.STATUS='V'
    *-*   LOOP
    *-* ENDIF
    *B131768,1  TMI [End  ] 
    
    IF llRpPCnInv .AND. cTrType='3' .AND. dTrDate = ldTrDt
      SELECT (lcStyUtil)
      GOTO lnFstInv
      =RLOCK()
      REPLACE nStk1 WITH nStk1+STYINVJL.nStk1,;
              nStk2 WITH nStk2+STYINVJL.nStk2,;
              nStk3 WITH nStk3+STYINVJL.nStk3,;
              nStk4 WITH nStk4+STYINVJL.nStk4,;
              nStk5 WITH nStk5+STYINVJL.nStk5,;
              nStk6 WITH nStk6+STYINVJL.nStk6,;
              nStk7 WITH nStk7+STYINVJL.nStk7,;
              nStk8 WITH nStk8+STYINVJL.nStk8,;   
            nTotStk WITH nTotStk+STYINVJL.nTotStk
      UNLOCK
    ELSE
      *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
      IF Cirtype = 'R' .AND. m.Cirtype = 'R' .AND. Csession = m.Csession
        SELECT (lcStyUtil)
        REPLACE Cirtype WITH 'I'
        SELECT STYINVJL
      ENDIF
      *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
      SCATTER MEMVAR
      m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
      SELECT (lcStyUtil)
      APPEND BLANK
      GATHER MEMVAR
      *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
      IF cTrType = '2'
        llAddTrn = !llAddTrn
      ENDIF
      *B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
    ENDIF
    IF llRpPCnInv .AND. &lcStyUtil..cTrType='3' .AND. &lcStyUtil..dTrDate <> ldTrDt
      lnFstInv= RECNO(lcStyUtil)
      ldTrDt  = &lcStyUtil..dTrDate
    ENDIF
  ENDSCAN
ENDDO

*B605608,1 BWA 03/31/2002 To clear the wait window.[START]
WAIT CLEAR
*B605608,1 BWA 03/31/2002.[END]

*B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (Start)
IF llAddTrn
      IF m.Cirtype = 'R'
        SELECT (lcStyUtil)
        DELETE
        m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
        m.Cirtype = 'I'
        m.Cisession = m.Csession
        STORE '' TO m.Crsession , m.Cicacnt
        APPEND BLANK
        GATHER MEMVAR
        SELECT STYINVJL
      ENDIF
      m.Csession = PADL(ALLTRIM(m.Csession),6,'0')
      m.Cirtype = 'R'
      m.Crsession = m.Csession
      STORE '' TO m.Cisession , m.Cicacnt
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        m.Nstk&lcCount = 0
      ENDFOR
      STORE 0 TO m.Ntotstk , Nstkval , Nprvsval , Lineno , Nprvsqty
      SELECT (lcStyUtil)
      APPEND BLANK
      GATHER MEMVAR
      llAddTrn = .F.
      SELECT STYINVJL
ENDIF
*B603706,1 AMH 07/4/2000 Fixed The Bug not Printing Physical Inv. with zero Balance (End)
SELECT StyInvJL
SET FILTER TO
SELECT (lcStyUtil)
IF RECCOUNT() = 0
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display.')  

  *B605419,1 BWA 01/28/2002 The page broken after this message because the device didn't return to its original status which is screen.[START]
  SET DEVICE TO SCREEN
  *B605419,1 BWA 01/28/2002.[END]

  RETURN
ENDIF

*B605608,1 BWA 03/31/2002 Comment the creation of the index to be before collecting the data.[START]
*INDEX ON Style+cSession+DTOS(dTRDate)+cTrCode TAG (lcStyUtil)
*B605608,1 BWA 03/31/2002.[END]

*--Printing portion.
R_Title = "STYLE UTILIZATION REPORT"
*B602210,1 Change report header width
R_WIDTH = 'W'
DO lpPrint
DO ENDREPORT
SET DEVICE TO SCREEN

IF llApVendor
  USE IN 'APVENDOR'
ENDIF
IF llInvHdr
  USE IN 'INVHDR'
ENDIF
IF llPosHdr
  USE IN 'POSHDR'
ENDIF
IF llRetHdr
  USE IN 'RETHDR'
ENDIF
IF llCutTktH 
  USE IN 'CUTTKTH'
ENDIF
USE IN (lcStyUtil)
ERASE (gcWorkDir+lcStyUtil+".DBF")
ERASE (gcWorkDir+lcStyUtil+".CDX")

*B131768,1  TMI [Start] close the ALIAS "lcSekInvJL"
IF USED(lcSekInvJL)
  USE IN &lcSekInvJL
ENDIF
*B131768,1  TMI [End  ] 

*!*************************************************************
*! Name      : lpPrint
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Print Report
*!*************************************************************
*! Calls     : RPT_HDR,lfTotals,lfGetNBal,lpTrTp,lfBrekSize
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Report LayOut :
*!0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!PROGID                                                           COMPANY NAME                                              DATE      
*!TIME                                                             PROGRAM NAME                                              PAGE#     
*!                                                                                                                                      
*!-------------------------------------------------------------------------------------------------------------------------------------
*! STYLE : XXXXXXXXXXXXXXXXXXX  DESC: XXXXXXXXXXXXXXXXXXXX   SEASON : XXXXXX  DIVISION : XXXXXX  PATTERN : XXXXXXXXXX GROUP : XXXXXX  
*!                              SCALE: XXX =>  XXXXX  XXXXX  XXXXX  XXXXX  XXXXX  XXXXX  XXXXX  XXXXX                FABRIC : XXXXXXX               
*!-------------------------------------------------------------------------------------------------------------------------------------        
*!DATE       TRANS          TRANS   CUST. CODE                                                                      REC/SHP  NEW    
*!CUST. NAME TYPE           NUMBER  /VENDOR      ADJUSTMENT REASON                                       BALANCE    /ADJ.    BALANCE 
*!-------------------------------------------------------------------------------------------------------------------------------------
*!MM/DD/YY   XXXXXXXXXXXXXX XXXXXX  XXXXX        XXXXXXXXXXXXXXXXXXXXXXXXX                                                            
*!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX              999999 999999 999999 999999 999999 999999 999999 999999   99999999  (9999999) 99999999
*!                               NEW BALANCE  999999 999999 999999 999999 999999 999999 999999 999999                              
*!=====================================================================================================================================
*! XXXXXXXX  XXX   SUB TOTAL   [ RECEIVED : 99999999 ]   [ ADJUSTMENTS : 99999999 ]  [ SHIPPED : 99999999 ]  [ RETURNED : 99999999 ]  
*!=====================================================================================================================================
*!
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpPrint()
*!*************************************************************
PROCEDURE lpPrint

SET DEVICE TO PRINT
CLEAR TYPEAHEAD               
GG=SYS(2002)

PAGENO = 0 
ROW    = 99
lcReport = ''

*B801982,1 Get all activities when date range is empty
*lcTitle  = 'DATE RANGE =>'+DTOC(ldLDate)+'  '+DTOC(ldHDate)
*B604014,1 ABD Change the title to print the location beside the date. [Begin]
*lcTitle  = 'DATE RANGE =>'+IIF(EMPTY(ldLDate),DTOC(ldLowDate ),DTOC(ldLDate))+'  '+DTOC(ldHDate)
IF  llMultiWH
  IF EMPTY(laOGFxFlt[lnLocPos,6])
    lcLocTitl = '   Location : ALL '
  ELSE
    lcLocTitl = '   Location : '+ laOGFxFlt[lnLocPos,6]
  ENDIF
ENDIF
lcTitle  = 'DATE RANGE =>'+IIF(EMPTY(ldLDate),DTOC(ldLowDate ),DTOC(ldLDate))+'  '+DTOC(ldHDate) +;
            IIF(llMultiWH,lcLocTitl,'')
*B604014,1 ABD [End]

*B801982,1 (End)

DIMENSION laSubTot(4),laGrnTot(4)  && Array to hold sub totals and totals.
STORE 0 TO laSubTot,laGrnTot       && Initialize sub and grand totals array.
llFirst=.T.
lcOLDKey=' '
lnNewBal=0
STORE 0 TO lnPreQty1,lnPreQty2,lnPreQty3,lnPreQty4,;
           lnPreQty5,lnPreQty6,lnPreQty7,lnPreQty8
SELECT (lcStyUtil)
LOCATE
lnIssue=0
llAddIssue=.T.
*B131768,1  TMI [Start] physical inventory change variable
lnPhyStk = 0
*B131768,1  TMI [End  ] 
SCAN WHILE INKEY() <> 32
  IF cTrType $ '29' AND cIRType = "I" 
    *B602346,1 Consider that issued transactions are stored with nigative quantity
    *lnNewBal = lnNewBal-nTotStk  
    *lnIssue  = lnIssue+nTotStk
    *FOR lnCount = 1 TO 8
    *  lcCount = STR(lnCount,1)
    *  lnPreQty&lcCount = lnPreQty&lcCount - NStk&lcCount
    *ENDFOR

    lnNewBal = lnNewBal + nTotStk  
    lnIssue  = lnIssue  - nTotStk
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      lnPreQty&lcCount = lnPreQty&lcCount + NStk&lcCount
    ENDFOR
    *B602346,1 (End)
    
    *B131768,1  TMI [Start] save the physical inv. to the variable lnPhyStk
    lnPhyStk = nTotStk
    *B131768,1  TMI [End  ] 

    LOOP
  ENDIF
      
  *--PRINT THE HEADER.
  IF ROW>=52
    PAGENO=PAGENO+1
    DO RPT_HDR WITH lcReport,lcTitle,R_WIDTH
    ROW=05
  ENDIF   
  lcKey=Style
  IF lcKey<>lcOLDKey
    llAddIssue=.F.
    IF ! llFirst
      *--Print Subtotals
      =lfTotals('S')
    ENDIF
    llFirst=.F.
    lcOLDKey=Style
    *--Calculating the previous balance for each style.
    lnNewBal=lfGetNBal()
    SELE STYLE
    =SEEK(lcKey)
    lcStyle = STYLE
    @ ROW,001 SAY 'STYLE  :'+lcStyle
    @ ROW,030 SAY 'DESC: '+STYLE.DESC
    @ ROW,059 SAY 'SEASON : '+STYLE.SEASON
    @ ROW,076 SAY 'DIVISION : '+STYLE.cDivision
    @ ROW,095 SAY 'PATTERN : '+STYLE.PATTERN
    @ ROW,116 SAY 'GROUP : '+STYLE.cStyGroup
    ROW=ROW+1
    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,030 SAY 'SCALE: '+STYLE.SCALE+' =>'
    *XCOL=45
    @ ROW,022 SAY 'SCALE: '+STYLE.SCALE+' =>'
    XCOL = 38
    *E301800,1 BWA 01/21/2002 (End)

    FOR lnCount = 1 TO SCALE.CNT
      lcCount=STR(lnCount,1)
      @ ROW,XCOL SAY PADL(ALLTRIM(SCALE.SZ&lcCount),5,' ')
      XCOL = XCOL + 8
    ENDFOR
    @ ROW,115 SAY 'FABRIC : '+STYLE.FABRIC
    ROW=ROW+1
    @ ROW,000 SAY REPLICATE('-',130)
    ROW=ROW+1
    @ ROW,000 SAY 'DATE       TRANS          TRANS   CUST. CODE                                                                      REC/SHP  NEW'
    ROW=ROW+1
    @ ROW,000 SAY 'CUST. NAME TYPE           NUMBER  /VENDOR      ADJUSTMENT REASON                                       BALANCE    /ADJ.    BALANCE '
    ROW=ROW+1
    @ ROW,000 SAY REPLICATE('-',130)
  ENDIF 
  SELECT (lcStyUtil)
  *--Initialize needed variables.
  FOR lnCount=1 TO SCALE.CNT
    lcCount=STR(lnCount,1)
    lnStk&lcCount=nStk&lcCount
  ENDFOR
  lnTotStk=nTotStk
  IF llAddIssue
    lnBalnce = lnNewBal + lnIssue
  ELSE
    llAddIssue=.T.
    lnBalnce = lnNewBal
  ENDIF
  lcT=0
  ROW=ROW+1
  DO lpTrTp WITH cTrType
  IF llRpPrntSz
    =lfBrekSize('C')
  ENDIF
  *--Balances/adj fields.
  @ ROW,102 SAY lnBalnce PICTURE '99999999'
  lnBalnce = lnBalnce - lnIssue
  lnIssue  = 0

  *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
  IF cTrType<>'3'
    *@ ROW,113 SAY lnTotStk PICTURE '9999999'
    @ ROW,112 SAY lnTotStk PICTURE '9999999999'
    *E301800,1 BWA 01/21/2002 (End)
    lnNewBal=lnBalnce+lnTotStk  
  ELSE
    @ ROW,112 SAY '('
    @ ROW,113 SAY lnTotStk PICTURE '9999999'
    @ ROW,120 SAY ')'
    *B602346,1 Consider that issued transactions are stored with nigative quantity
    *lnNewBal=lnBalnce-lnTotStk
    lnNewBal=lnBalnce+lnTotStk
    *B602346,1 (End)
  ENDIF
  @ ROW,122 SAY lnNewBal PICTURE '99999999'
  laSubTot(lcT)=laSubTot(lcT)+lnTotStk
  *B131768,1 TMI [START] Update the subtotal with the physical inventory Issue variable
  IF lnPhyStk<>0
    laSubTot(2)=laSubTot(2)+lnPhyStk
    lnPhyStk = 0
  ENDIF  
  *B131768,1 TMI [END  ]
  
  IF llRpPrntSz
    =lfBrekSize('N')
  ENDIF
ENDSCAN    
*--Print last Subtotals
=lfTotals('S')
*--Grand total.
=lfTotals('G')

GG=SYS(2002,1)
RETURN

*!*************************************************************
*! Name      : lfTotals
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Calculate Totals
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcSubGnd  'S' : SubTotal
*!                                 'G' : Grand Total
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTotals()
*!*************************************************************
FUNCTION lfTotals
PARAMETERS lcSubGnd

ROW=ROW+1
@ ROW,000 SAY REPLICATE('=',130)
ROW=ROW+1
IF lcSubGnd='S'
  @ ROW,001 SAY lcStyle
  ROW=ROW+1
  @ ROW,015 SAY 'SUB TOTAL'
  @ ROW,029 SAY '[ RECEIVED :'
  @ ROW,042 SAY STR(laSubTot(1),8)+' ]'
  @ ROW,055 SAY '[ ADJUSTMENTS :'
  @ ROW,071 SAY STR(laSubTot(2),8)+' ]'
  @ ROW,083 SAY '[ SHIPPED :'
  @ ROW,095 SAY STR(laSubTot(3),8)+' ]'
  @ ROW,107 SAY '[ RETURNED :'
  @ ROW,120 SAY STR(laSubTot(4),8)+' ]'

  laGrnTot(1)=laGrnTot(1)+laSubTot(1)
  laGrnTot(2)=laGrnTot(2)+laSubTot(2)
  laGrnTot(3)=laGrnTot(3)+laSubTot(3)
  laGrnTot(4)=laGrnTot(4)+laSubTot(4)            
  laSubTot = 0
  STORE 0 TO lnPreQty1,lnPreQty2,lnPreQty3,lnPreQty4,;
             lnPreQty5,lnPreQty6,lnPreQty7,lnPreQty8
ELSE
  @ ROW,015 SAY 'GRAND TOTAL'
  @ ROW,029 SAY '[ RECEIVED :'
  @ ROW,042 SAY STR(laGrnTot(1),8)+' ]'
  @ ROW,055 SAY '[ ADJUSTMENTS :'
  @ ROW,071 SAY STR(laGrnTot(2),8)+' ]'
  @ ROW,083 SAY '[ SHIPPED :'
  @ ROW,095 SAY STR(laGrnTot(3),8)+' ]'
  @ ROW,107 SAY '[ RETURNED :'
  @ ROW,120 SAY STR(laGrnTot(4),8)+' ]'
ENDIF
ROW=ROW+1
@ ROW,000 SAY REPLICATE('=',130)
ROW=ROW+1 
RETURN

*!*************************************************************
*! Name      : lpTrTp
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Get special fields from master files wich not found in
*!             Style inventory journal .
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcTrType : Transaction Type
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpTrTp()
*!*************************************************************
PROCEDURE lpTrTp
PARAMETERS lcTrType

@ ROW,000 SAY dTrDate
lcAcctvn  = ' '
lcAccName = ' '
lcReason  = ' '
DO CASE
  *E301248 (Start)
  *CASE lcTrType='1'
  CASE lcTrType $ '1I'
  *E301248 (End)
    lcTrTpDsc ='INVENTORY ADJ.'  
    lcReason  = &lcStyUtil..REFERENCE
    lcT=2
  CASE lcTrType='2'
    lcTrTpDsc='PHYSICAL INV.'
    lcT=2
    lcReason = &lcStyUtil..REFERENCE
  CASE lcTrType='3'
    =SEEK(&lcStyUtil..cTrCode,'INVHDR')
    lcTrTpDsc = IIF(!llRpPCnInv,'SHIPPED','TOTAL SHIP')
    lcAcctvn  = INVHDR.Account
    lcAccName = IIF(SEEK('M'+lcAcctvn,'Customer'),Customer.BtName,' ')
    lcT=3

    *B131768,1  TMI [Start] print the void invoice transaction
  CASE lcTrType='4'
    =SEEK(&lcStyUtil..cTrCode,'INVHDR')
    lcTrTpDsc = 'VOID INVOICE'
    lcAcctvn  = INVHDR.Account
    lcAccName = IIF(SEEK('M'+lcAcctvn,'Customer'),Customer.BtName,' ')
    lcT=3
    *B131768,1  TMI [End  ]     

  CASE lcTrType$'56'
    lcTrTpDsc='RECEIVED'
    IF lcTrType='5'
      *B801982,1 Get C/T contractor from MFGOPRHD file.
      *=SEEK(&lcStyUtil..cTrCode,'CUTTKTH')
      *lcAcctvn=CUTTKTH.Contr2

      *B802168,1 KHM 20/04/99 (Begin) Checking the existence of MF module
      *B802168,1              since MFGOPRHD file is opened when MF is installed.
      *IF SEEK('M'+&lcStyUtil..cTrCode,'MFGOPRHD')
      IF 'MF' $ gcCmpModules AND SEEK('M'+&lcStyUtil..cTrCode,'MFGOPRHD')
      *B802168,1 KHM 20/04/99 (End)
        SKIP IN 'MFGOPRHD'
      ENDIF
      
      *B802168,1 KHM 20/04/99 (Begin) Checking the existence of MF module
      *B802168,1              since MFGOPRHD file is opened when MF is installed.
      *IF MFGOPRHD.cIMTyp+MFGOPRHD.cTktNo='M'+&lcStyUtil..cTrCode
      IF 'MF' $ gcCmpModules AND MFGOPRHD.cIMTyp+MFGOPRHD.cTktNo='M'+&lcStyUtil..cTrCode
      *B802168,1 KHM 20/04/99 (End)
        
        lcAcctvn  = MFGOPRHD.cContCode
        lcAccName = MFGOPRHD.cContName
      ELSE
        STORE '' TO lcAcctvn,lcAccName
      ENDIF   
      *B801982,1 (End)
    ELSE

      *B802496,1 Check if The File Open or Not [ BEGIN ]
      IF llPosHdr
        =SEEK('P'+&lcStyUtil..cTrCode,'POSHDR')
        lcAcctvn  = POSHDR.Vendor
      ELSE  
        STORE '' TO lcAcctvn
      ENDIF  

      IF llApVendor
        lcAccName = IIF(SEEK(lcAcctvn,'ApVendor'),ApVendor.cVenComp,' ')
      ELSE
        STORE '' TO lcAccName
      ENDIF
      *B802496,1 Check if The File Open or Not [ END ]
      
    ENDIF
    lcReason = &lcStyUtil..REFERENCE
    lcT=1
  CASE lcTrType='7'
    lcTrTpDsc='RETURNED'
    =SEEK(&lcStyUtil..cTrCode,'RETHDR')
    lcAcctvn  = RETHDR.Account
    lcAccName = IIF(SEEK('M'+lcAcctvn,'Customer'),Customer.BtName,' ')
    lcT=4
    
    *B131768,1  TMI [Start] print the void credit memo transaction
  CASE lcTrType='8'
    lcTrTpDsc='VOID CRMEMO'
    =SEEK(&lcStyUtil..cTrCode,'RETHDR')
    lcAcctvn  = RETHDR.Account
    lcAccName = IIF(SEEK('M'+lcAcctvn,'Customer'),Customer.BtName,' ')
    lcT=4
    *B131768,1  TMI [End  ] 

  CASE lcTrType='9'
    lcTrTpDsc='INV. MARKDOWN'
    *B607409,1 ABD - Remark the next line and get the locking reson from the new file with a new way. [Begin]
    *=SEEK(DTOS(&lcStyUtil..dTrDate)+&lcStyUtil..Style,'TMPINVT')
    **B803274,1 MHM 07/13/2000 [Begin]
    **lcReason=TMPINVT.Reason
    *lcReason=TMPINVT.CReason
    **B803274,1 MHM 07/13/2000 [end]

    lnPrvAlias  = SELECT(0)
    IF SEEK(&lcStyUtil..Style,'MDINVNTL')
      SELECT MDINVNTL
      SCAN REST WHILE Style+Color+Dyelot+Clocation+cBattype+Clkbatch = &lcStyUtil..Style + SPACE(6) + &lcStyUtil..Cdyelot
        IF SEEK(cBatType+clkBatch,'MDINVNTH') .AND. &lcStyUtil..dTrDate = MDINVNTH.Date
          lcReason = MDINVNTL.CReason
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
    SELECT(lnPrvAlias)
    *B607409,1 ABD - [END]
    
    lcT=2
ENDCASE
SELECT (lcStyUtil)
@ ROW,11 SAY lcTrTpDsc
IF !(llRpPCnInv .AND. lcTrType='3' )
  @ ROW,26 SAY cTrCode
  @ ROW,34 SAY lcAcctvn
ENDIF
*E301248 (Start)
*IF INLIST(lcTrType,'1','2','9','6')
IF INLIST(lcTrType,'I','1','2','9','6')
*E301248 (End)
  @ ROW,47 SAY lcReason
  ROW=ROW+IIF(llRpPrntSz,1,0)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfBrekSize
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : print the new balance in sizes after each tranc.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : lcBal  'N' for new Balance.
*!                             'C' for current ADJ/REC/SHP. 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrekSize('N')
*!*************************************************************
FUNCTION lfBrekSize
PARAMETERS lcBal

*E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
*XCOL=044
XCOL = 037
*E301800,1 BWA 01/21/2002 (End)

ROW=ROW+1
IF lcBal='N'
  *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
  *@ ROW,031 SAY 'NEW BALANCE'
  @ ROW,023 SAY 'NEW BALANCE'
  *E301800,1 BWA 01/21/2002 (End)
  FOR lnCount=1 TO SCALE.CNT
    lcCount =STR(lnCount,1)
    *B602346,1 Consider that issued transactions are stored with nigative quantity
    *lnPreQty&lcCount=IIF(cTrType<>'3',(lnPreQty&lcCount+lnStk&lcCount),;
                         (lnPreQty&lcCount-lnStk&lcCount))
    lnPreQty&lcCount = lnPreQty&lcCount+lnStk&lcCount
    *B602346,1 (End)

    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,XCOL SAY lnPreQty&lcCount PICTURE '999999'
    *XCOL = XCOL + 7
    @ ROW,XCOL SAY lnPreQty&lcCount PICTURE '9999999'
    XCOL = XCOL + 8
    *E301800,1 BWA 01/21/2002 (End)

  ENDFOR
ELSE
  @ ROW,000 SAY lcAccName
  FOR lnCount=1 TO SCALE.CNT
    lcCount =STR(lnCount,1)
    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *@ ROW,XCOL SAY lnStk&lcCount PICTURE '999999'
    *XCOL = XCOL + 7
    @ ROW,XCOL SAY lnStk&lcCount PICTURE '9999999'
    XCOL = XCOL + 8
    *E301800,1 BWA 01/21/2002 (End)
  ENDFOR
ENDIF
RETURN        

*!*************************************************************
*! Name      : lfGetNBal
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Calculating the previous balance for each style.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetNBal()
*!*************************************************************
FUNCTION lfGetNBal

SELECT STYINVJL
=SEEK(lcKey)
lnNewBal=0

*B605608,1 BWA 03/31/2002 Use the full index to activate the Rushmore.[START]
*SCAN REST WHILE Style = lcKey FOR DTRDate<ldLDate
SCAN REST WHILE Style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcKey FOR DTRDate<ldLDate
*B605608,1 BWA 03/31/2002.[END]

  IF CIRType = 'I'
    *E301248 (Start)
    *IF CTRType='1'
    IF CTRType $ 'I1'
    *E301248 (End)
      lnPreQty1=lnPreQty1+NStk1
      lnPreQty2=lnPreQty2+NStk2
      lnPreQty3=lnPreQty3+NStk3
      lnPreQty4=lnPreQty4+NStk4
      lnPreQty5=lnPreQty5+NStk5
      lnPreQty6=lnPreQty6+NStk6
      lnPreQty7=lnPreQty7+NStk7
      lnPreQty8=lnPreQty8+NStk8
      lnNewBal=lnNewBal+NTotStk
    ELSE  
      *B602346,1 Consider that issued transactions are stored with nigative quantity
      *lnPreQty1=lnPreQty1-NStk1
      *lnPreQty2=lnPreQty2-NStk2
      *lnPreQty3=lnPreQty3-NStk3
      *lnPreQty4=lnPreQty4-NStk4
      *lnPreQty5=lnPreQty5-NStk5
      *lnPreQty6=lnPreQty6-NStk6
      *lnPreQty7=lnPreQty7-NStk7
      *lnPreQty8=lnPreQty8-NStk8
      *lnNewBal=lnNewBal-NTotStk

      lnPreQty1=lnPreQty1+NStk1
      lnPreQty2=lnPreQty2+NStk2
      lnPreQty3=lnPreQty3+NStk3
      lnPreQty4=lnPreQty4+NStk4
      lnPreQty5=lnPreQty5+NStk5
      lnPreQty6=lnPreQty6+NStk6
      lnPreQty7=lnPreQty7+NStk7
      lnPreQty8=lnPreQty8+NStk8
      lnNewBal=lnNewBal+NTotStk
      *B602346,1 (End)

    ENDIF
  ELSE
    lnPreQty1=lnPreQty1+NStk1
    lnPreQty2=lnPreQty2+NStk2
    lnPreQty3=lnPreQty3+NStk3
    lnPreQty4=lnPreQty4+NStk4
    lnPreQty5=lnPreQty5+NStk5
    lnPreQty6=lnPreQty6+NStk6
    lnPreQty7=lnPreQty7+NStk7
    lnPreQty8=lnPreQty8+NStk8
    lnNewBal=lnNewBal+NTotStk
  ENDIF
ENDSCAN
RETURN(lnNewBal)

*!*************************************************************
*! Name      : lfvstyle
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Validate Optional Grid selected Styles
*!*************************************************************
*! Calls     : gfStyBrw
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvstyle()
*!*************************************************************
FUNCTION lfvstyle
PRIVATE lcStyle
lcStyle = SYS(18)
IF !EMPTY(&lcStyle) AND !SEEK(&lcStyle,'Style')
  &lcStyle = gfStyBrw("M" , "" , "" , .F.)
ENDIF

*!*************************************************************
*! Name      : lfvFabric
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Validate Optional Grid selected Primary Fabric
*!*************************************************************
*! Calls     : gfStyBrw
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lcFabric,lcField
lcFabric = EVAL(SYS(18))
SET ORdER TO TAG FABRIC IN FABRIC
IF !EMPTY(lcFabric) .AND. !SEEK(lcFabric,'Fabric')
  lcFabric = IIF(FABROW(@lcFabric,'*',.T.),lcFabric,SPACE(7))
  lcField  = SYS(18)
  &lcField = lcFabric
ENDIF

*!*************************************************************
*! Name      : lfRptWhen
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Report When Function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRptWhen()
*!*************************************************************
FUNCTION lfRptWhen
lcMajPic = "@! " + gfItemMask("PM")

*B604014,1 ABD Get the potion of the field. [Begin]
lnLocPos   = lfItmPos('STYINVJL.CWARECODE')
*B604014,1 ABD [End]

*!*************************************************************
*! Name      : lfClrStat
*! Developer : WAM
*! Date      : 09/09/1998
*! Purpose   : Get Information about Item Segment
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrStat()
*!*************************************************************
FUNCTION lfClrStat
PRIVATE laItemSeg,lnMajSeg,lcFreeSep

STORE '' TO lcNonMjr,lcNonMjrH,lcNonMjrM,lcFreeSeg,lcFreeHdr,lcFreeMask,lcFreeSep
STORE 0  TO lnNonMjrP,lnFreePos
DECLARE laItemSeg[1]
lnMajSeg = gfItemMask('SM')  && No. of major segments.
=gfItemMask(@laItemSeg)
FOR lnCount = lnMajSeg +1 TO ALEN(laItemSeg,1)
  DO CASE
     CASE laItemSeg[lnCount,1]='C'
       lcNonMjr  = 'C'
       lcNonMjrH = laItemSeg[lnCount,2]
       lcNonMjrM = laItemSeg[lnCount,3]
       lnNonMjrP = laItemSeg[lnCount,4]
       STORE '' TO lcFreeSeg,lcFreeHdr,lcFreeMask
       STORE 0  TO lnFreePos
       EXIT
     CASE lcFreeSeg='F' AND laItemSeg[lnCount,1] <> 'F'
     CASE laItemSeg[lnCount,1]='F'
       lcFreeSeg  = 'F'
       lcFreeHdr = lcFreeHdr+lcFreeSep+laItemSeg[lnCount,2]
       lcFreeMask= lcFreeMask+lcFreeSep+laItemSeg[lnCount,3]
       lnFreePos = IIF(lnFreePos=0,laItemSeg[lnCount,4],lnFreePos)
       lcFreeSep = laItemSeg[lnCount,6]
  ENDCASE
ENDFOR
*B802402,1 - WAB - Add function for order the table when press button in range 
*B802402,1 		   at option  grad
*B802402,1 - WAB - Start
*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/25/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty  
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE

   *B605608,1 BWA 03/31/2002 Fix the bug of locating in another file not the style file.[START]
   *GO TOP 
   GO TOP IN STYLE
   *B605608,1 BWA 03/31/2002.[END]

  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE
*B802402,1 - WAB - END
*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Abdou ElGendi - (ABD)
*! Date      : 11/13/2000
*! Purpose   : Get any get field old value.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Called from : Option Grid, any Get Field.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWOldVal()
*!*************************************************************
*B604014,1
FUNCTION lfWOldVal
laOldVal = EVALUATE(SYS(18))

*- End Of lfWOldVal.
*!*************************************************************
*! Name      : lfvLoc
*! Developer : Abdou ElGendi - (ABD)
*! Date      : 11/13/2000
*! Purpose   : Valid function of the Location.
*!*************************************************************
*! Calls     : Functions  : gfBrowWare()
*!*************************************************************
*! Called from : Option Grid,Get Location Field.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvLoc()
*!*************************************************************
*B604014,1
FUNCTION lfvLoc

*-- Varible to hold  the name of the memory variable used to create the current GET field .
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field .
lcObjVal = EVALUATE(SYS(18))

IF !USED('WAREHOUS')
  USE (gcDataDir+'WAREHOUS') ORDER TAG Warehous IN 0 SHARED
ENDIF

*-- IF The user want to Browse or if the Warehouse he entered is not in the
*-- file.
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*- End OF lfvLoc.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Abdou ElGendi - (ABD)
*! Date      : 11/13/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : lnItmPos.
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*B604014,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*!*************************************************************
