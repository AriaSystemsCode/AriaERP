*:****************************************************************************
*: Program file        : SOIMPJL.PRG
*: Program description : Sales orders for John Lewis need to be imported into Aria from an 
*:                       ASCII file layout
*: Aria customer       : Charles Kirk  ( CHA26 )           
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SO2)
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : *C200575,1
*: Date developed      : 07/13/2003
*:****************************************************************************
* Modifications
*C200575,6 TMI 08/27/2003 Don't show the message called from SoUPdate that says "Order has been saved with # xxxxxx " for each order
*C200575,6                Only show one message after all imported orders
*C037287,1 MMM 11/30/2003 Some Modifications based on the customer request.
*B124704,1 ASH 10/07/2004 Get the sales rep from customer store record.
*:***************************************************************************



PRIVATE lcRpImpfil,lcOldValue,lcSlctdAcc,lnClrPos,lnClrLen

STORE '' TO lcRpImpfil,lcOldValue,lcT_BomVar
*C200575,5  TMI [Start] Initialise lcAccnt variable , Used for the whole imported file
STORE '     ' TO lcAccnt
*C200575,5  TMI [End  ] 
*- Get Color position and lenght
STORE 0 TO lnClrPos,lnClrLen
=lfGetClrD()

lcSlctdAcc = gfTempName()
lcT_BomVar = gfTempName()

*--Calling the option grid
lcExpr = gfOpGrid('SOIMPJL' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcExpr)
  RETURN
ENDIF

*-- Export data to ASCII file
=lfImpFrAsc()

*-- Start exporting orders


*:**************************************************************************
*:* Name        : lfwRpWhen() 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : When OG Function
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRpWhen() ()
*:***************************************************************************
FUNCTION lfwRpWhen

*-- if it is the default case
IF lnOgSeting = 1
  SHOW GET pbRun DISABLE
ENDIF
*C200575,5  TMI [Start] Run "lfvChkRnBt" function
=lfvChkRnBt()
*C200575,5  TMI [End  ] 
*-- end of lfwRpWhen.

*:**************************************************************************
*:* Name        : lfvImpFile
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/13/2003
*:* Purpose     : Valid fn for selecting a file to imort from 
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Example     :  = lfvSndFile()
*:***************************************************************************
FUNCTION lfvImpFile
DO WHILE .T.
  IF EMPTY(lcRpImpFil)
    EXIT
  ENDIF  
  IF lcOldValue # lcRpImpfil
    lcRpImpfil = ALLT(lcRpImpfil)
    lcRpImpfil = IIF('\'$lcRpImpfil,'',FULLPATH('')) + lcRpImpfil + IIF('.' $ lcRpImpfil , '' , '.CSV' )
    IF !FILE(lcRpImpfil)
      lcRpImpfil = GETFILE('CSV','Open csv file to import from.')
    ELSE
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO

=lfvChkRnBt()
*-- end of lfvSndFile.

*:**************************************************************************
*:* Name        : lfvChkRnBt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/13/2003
*:* Purpose     : Check Run button to be enabled
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvChkRnBt()
*:***************************************************************************
FUNCTION lfvChkRnBt
PRIVATE lcStat,lnAccPos,lnOrdRec,lnAccRec 

*C200575,5  TMI [Start] Check m.account not to be empty
*lcStat = IIF( !EMPTY(lcRpImpfil) , 'ENABLE','DISABLE')
lcStat = IIF( !EMPTY(lcRpImpfil) .AND. !EMPTY(lcAccnt) , 'ENABLE','DISABLE')
*C200575,5  TMI [End  ] 
SHOW GET pbRun &lcStat

*-- end of lfvChkRnBt.

*:**************************************************************************
*:* Name        : lfvSlctdAcc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2003
*:* Purpose     : Set/rest/valid funtion for Accounts Browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSlctdAcc()
*:***************************************************************************
FUNCTION lfvSlctdAc
PARAMETERS lcSR
PRIVATE lnAlias,lnAccPos
lnAlias = SELECT()
lnAccPos = ASUBSCRIPT( laOgFxFlt , ASCAN( laOgFxFlt , 'CUSTOMER.ACCOUNT' ) , 1 )
DO CASE
  CASE lcSR = 'S'
    GO TOP IN CUSTOMER

  CASE lcSR = 'R'
    *C200575,5  TMI [Start] 
    GO TOP IN (laOgFxFlt[lnAccPos,6])
    lcAccnt = EVAL(laOgFxFlt[lnAccPos,6]+'.ACCOUNT')
    *C200575,5  TMI [End  ] 
    =lfvChkRnBt()    
    SELECT (laOgFxFlt[lnAccPos,6])
    *C200575,3  TMI [Start] create the cdx explicitly later
    *COPY TO (gcWorkDir+lcSlctdAcc) WITH CDX 
    *C200575,5  TMI [Start] no need for this file if only one account is selected
    *COPY TO (gcWorkDir+lcSlctdAcc)
    *C200575,5  TMI [End  ] 
    *C200575,3  TMI [End  ] 
ENDCASE


SELECT (lnAlias)

*-- end of lfvSlctdAcc.

*:**************************************************************************
*:* Name        : lfImpFrAsc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2003
*:* Purpose     : Import orders from the selected csv file 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfImpFrAsc()
*   Note    : all checks must take place
*:***************************************************************************
FUNCTION lfImpFrAsc
PRIVATE lcTmpOrd,lnScaleLen,lcLogFile,lcOrdHdr,lcOrdLine,lcTmpOrd,;
        lcDefSesn,lcDefDiv,lcDefWH,lnErrCnt,;
        lcSkuTmpl,lnDime1,lnDime2,llBomVarnt

STORE 8 TO lnDime1,lnDime2

llBomVarnt = gfGetMemVar('M_BOMVAR' , gcAct_Comp)   && Check if user use variant cost sheet setup
llExtSS    = gfGetMemVar('M_USEEXSSC')              && if Comp. use extended size scale

*-- Assign temp names
lcLogFile = gfTempName()
lcOrdHdr  = gfTempName()
lcOrdLine = gfTempName()
lcTmpOrd  = gfTempName()

STORE ' ' TO lcDefSesn,lcDefDiv,lcDefWH
STORE 0 TO lnErrCnt

lnScaleLen = gfGetMemVar('M_EXTWIDTH')
 
*--Open Needed files
=lfOpenFls()

*--Create needed temp files
=lfCratTemp()

*-- Create temp file and populate it with the selected csv file
=lfTempCSV()

*-- Populate default for season ,div and WH
=lfPopDef()

*-- Get data from skutmpl file
=lfGtSkuTmpl()  

*-- Intialize variables
SELECT ORDHDR
SCATTER MEMVAR MEMO BLANK
SELECT ORDLINE
SCATTER MEMVAR MEMO BLANK
STORE 1 TO m.NCURRUNIT,m.NEXRATE

SELECT &lcTmpOrd
GO TOP
M.ORDER = &lcTmpOrd..ORDER
*C200575,5  TMI [Start] Get Store id
M.STORE = lfGtStore(&lcTmpOrd..DLVCODE)
*C200575,5  TMI [End  ] 
llErrFree = .T.

SELECT &lcTmpOrd
GO TOP
SCAN 
  *-- Update lcOrdhdr file and reInitialize memory variables if order number changed
  IF M.ORDER # &lcTmpOrd..ORDER 
    IF llErrFree
      INSERT INTO &lcOrdHdr FROM MEMVAR
    ENDIF
    
    SELECT ORDHDR
    SCATTER MEMVAR MEMO BLANK
    SELECT ORDLINE
    SCATTER MEMVAR MEMO BLANK
    STORE 1 TO m.NCURRUNIT,m.NEXRATE    
    *C200575,5  TMI [Start] Get Store id
    M.STORE = lfGtStore(&lcTmpOrd..DLVCODE)
    *C200575,5  TMI [End  ] 
    llErrFree = .T.
  ENDIF

  *-- Fill ordhdr fields
  *C200575,3  TMI [Start] Get upper case of account
  *m.ACCOUNT = &lcTmpOrd..ACCOUNT
  *C200575,5  TMI [Start] Only One account will be entered as per tony
  *m.ACCOUNT = UPPER(&lcTmpOrd..ACCOUNT)
  m.ACCOUNT = lcAccnt
  *C200575,5  TMI [End  ] 
  *C200575,3  TMI [End  ] 
  m.CORDTYPE = 'O'
  M.ORDER = &lcTmpOrd..ORDER
  m.STATUS = 'O'
  m.SEASON = lcDefSesn  
  m.CDIVISION = lcDefDiv  
  m.CWARECODE = lcDefWH
  
  *C037287,1 MMM 11/30/2003 Validating with the custpo field because the the order will change 
  *                         based on the sequence file [Start]  
  *IF SEEK('O'+M.ORDER,'ORDHDR')
  lcSlct= SELECT()
  SELECT ORDHDR
  lcOldIndx  = ORDER('ORDHDR')
  SET ORDER TO TAG Ordcust
  IF SEEK(UPPER(M.ACCOUNT)+UPPER(M.ORDER))  
  *C037287,1 MMM [End]
    =lfErr('ORDER')
    IF EOF(lcTmpOrd)
      EXIT
    ELSE
      *C200575,3  TMI [Start] Make the skip be in lcTmpOrd explicitly
      *SKIP -1
      SKIP -1 IN &lcTmpOrd
      *C200575,3  TMI [End  ] 
      LOOP
    ENDIF 
    *C037287,1 MMM 11/30/2003 Return the order to the old index. [Start]
    SET ORDER TO TAG &lcOldIndx 
    SELECT (lcSlct) 
    *C037287,1 MMM [End]    
  ENDIF
  
  
  IF SEEK('M'+m.ACCOUNT,'CUSTOMER')
    m.StName    = CUSTOMER.StName    
    m.Buyer     = CUSTOMER.Buyer     
    m.Phone     = CUSTOMER.Phone1    
    m.Disc      = CUSTOMER.Disc      
    m.cTermCode = CUSTOMER.cTermCode 
    m.ShipVia   = CUSTOMER.ShipVia   
    m.Spcinst   = CUSTOMER.Spcinst   
    m.Priority  = CUSTOMER.Priority  
    m.Link_Code = IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code) 
    m.GL_Sales  = IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)  
    m.Rep1 = CUSTOMER.SALESREP  
    m.Rep2 = CUSTOMER.REP2
    M.COMM1 = CUSTOMER.COMM
    M.COMM2 = CUSTOMER.COMM2
    M.PRIORITY = CUSTOMER.PRIORITY
    m.CCURRCODE = CUSTOMER.CCURRCODE
  ELSE
    =lfErr('ACCOUNT')             &&- if account not found skip this account orders   
    IF EOF(lcTmpOrd)
      EXIT
    ELSE
      *C200575,3  TMI [Start] Make the skip be in lcTmpOrd explicitly
      *SKIP -1
      SKIP -1 IN &lcTmpOrd
      *C200575,3  TMI [End  ] 
      LOOP
    ENDIF
  ENDIF
  
  *C200575,4  TMI [Start] Check if selected store exists  
  IF !EMPTY(M.STORE) .AND. !SEEK('S'+M.ACCOUNT+M.STORE,'CUSTOMER')
    *- if store not found skip this account orders
    =lfErr('STORE')             
    IF EOF(lcTmpOrd)
      EXIT
    ELSE
      SKIP -1 IN &lcTmpOrd
      LOOP
    ENDIF
  *B124704,1 ASH 10/07/04 (Begin) Get the sales rep from customer store record.
  ELSE
    M.REP1  = CUSTOMER.SALESREP
    M.REP2  = CUSTOMER.REP2
    M.COMM1 = CUSTOMER.COMM
    M.COMM2 = CUSTOMER.COMM2
  *B124704,1 ASH 10/07/04 (End)
  ENDIF
  *C200575,4  TMI [End  ] 
  
  *C200575,5  TMI [Start] if only one account will be selected then no need to this part of code.
  *IF USED(lcSlctdAcc) .AND. RECCOUNT(lcSlctdAcc) > 0 .AND. ;
  *  !SEEK(M.ACCOUNT,lcSlctdAcc)   && skip not selected accounts
  *  =lfErr('')
  *  IF EOF(lcTmpOrd)
  *    EXIT
  *  ELSE
  *    *C200575,3  TMI [Start] Make the skip be in lcTmpOrd explicitly
  *    *SKIP -1
  *    SKIP -1 IN &lcTmpOrd
  *    *C200575,3  TMI [End  ] 
  *    LOOP
  *  ENDIF    
  *ENDIF
  *C200575,5  TMI [End  ] 

  M.MULTI = 'N'  
  *C200575,4  TMI [Start] Convert date from string fromat to date format
  *M.ENTERED = &lcTmpOrd..ENTERED
  *M.START =  &lcTmpOrd..ENTERED
  *M.COMPLETE =  &lcTmpOrd..COMPLETE
  M.ENTERED = lfGtDt(&lcTmpOrd..ENTERED)
  
  *C037287,1 MMM 11/30/2003 The Start date amended to 7 days prior to the complete date [Start]
  *M.START =  lfGtDt(&lcTmpOrd..ENTERED)
  M.COMPLETE =  lfGtDt(&lcTmpOrd..COMPLETE)
  M.START    =  M.COMPLETE - 7
  *C037287,1 MMM [end]
  
  *C200575,4  TMI [End  ] 
  *C200575,3  TMI [Start] Check both entered and complete date not empty and 
  *-                      Be sure that the complete date is after the entered date
  IF EMPTY(M.ENTERED)
    =lfErr('ENTERED')
    IF EOF(lcTmpOrd)
      EXIT
    ELSE
      *C200575,3  TMI [Start] Make the skip be in lcTmpOrd explicitly
      *SKIP -1
      SKIP -1 IN &lcTmpOrd
      *C200575,3  TMI [End  ] 
      LOOP
    ENDIF    
  ENDIF
  IF EMPTY(M.COMPLETE)
    =lfErr('COMPLETE')
    IF EOF(lcTmpOrd)
      EXIT
    ELSE
      *C200575,3  TMI [Start] Make the skip be in lcTmpOrd explicitly
      *SKIP -1
      SKIP -1 IN &lcTmpOrd
      *C200575,3  TMI [End  ] 
      LOOP
    ENDIF    
  ENDIF
  IF M.ENTERED > M.COMPLETE
    =lfErr('DATEMISMATCH')
    IF EOF(lcTmpOrd)
      EXIT
    ELSE
      *C200575,3  TMI [Start] Make the skip be in lcTmpOrd explicitly
      *SKIP -1
      SKIP -1 IN &lcTmpOrd
      *C200575,3  TMI [End  ] 
      LOOP
    ENDIF    
  ENDIF  
  *C200575,3  TMI [End  ] 

  M.CINSUR = 'N'
  M.BULK = 'N'
  M.CREORDER = 'N'
  *------------------------------. Update lcOrdLine temp file .----------------------*
  STORE &lcTmpOrd..PRICE TO M.PRICE,M.GROS_PRICE
  
  *C037287,1 MMM 11/30/2003 store the new field disc_pcnt data to a variable  [Start]
  STORE &lcTmpOrd..Disc_Pcnt TO M.Disc_Pcnt 
  *C037287,1 MMM [End]
  *-- let all qty's be zero to update only the qty for the current sku
  STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.TOTQTY,;
             M.BOOK1,M.BOOK2,M.BOOK3,M.BOOK4,M.BOOK5,M.BOOK6,M.BOOK7,M.BOOK8,M.TOTBOOK
  =lfSku2Sty()

  *-- User ID , date and time
  M.CADD_USER = gcUser_ID
  M.CADD_TIME = Time()
  M.DADD_DATE = gdSysDate
  
  *-- Check reasons to not save this order
  IF M.STYLE = CHR(255)
    =lfErr('SKU')
    LOOP
  ENDIF    
  IF M.PRICE = 0
    =lfErr('PRICE')
    LOOP
  ENDIF
  IF M.TOTQTY = 0
    =lfErr('QTY')
    LOOP  
  ENDIF  
  IF STYLE.SEASON # lcDefSesn
    =lfErr('SEASON')
    LOOP
  ENDIF
  IF STYLE.CDIVISION  # lcDefDiv
    =lfErr('CDIVISION')
    LOOP
  ENDIF

  WAIT WINDOW NOWAIT 'Importing order:'+M.ORDER+' for Account:'+M.Account

  && KEY :: CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
  *C200575,7  TMI [Start] Seek by the new index respecting 'PackID' field
  *IF !SEEK('O'+M.ORDER+M.STORE+M.STYLE,lcOrdline)
  IF !SEEK('O'+M.ORDER+PADR(M.STORE,8)+M.STYLE+M.PACKID,lcOrdline)
    *C200575,7  TMI [End  ] 
    M.LINENO = M.LINENO + 1
    m.LASTLINE = M.LINENO    
    INSERT INTO &lcOrdLine FROM MEMVAR
  ELSE
    SELECT &lcOrdLine
    REPLACE QTY1  WITH QTY1  + M.QTY1  ,;
            QTY2  WITH QTY2  + M.QTY2  ,;
            QTY3  WITH QTY3  + M.QTY3  ,;
            QTY4  WITH QTY4  + M.QTY4  ,;
            QTY5  WITH QTY5  + M.QTY5  ,;
            QTY6  WITH QTY6  + M.QTY6  ,;
            QTY7  WITH QTY7  + M.QTY7  ,;
            QTY8  WITH QTY8  + M.QTY8  ,; 
            BOOK1 WITH BOOK1 + M.BOOK1 ,;
            BOOK2 WITH BOOK2 + M.BOOK2 ,;
            BOOK3 WITH BOOK3 + M.BOOK3 ,;
            BOOK4 WITH BOOK4 + M.BOOK4 ,;
            BOOK5 WITH BOOK5 + M.BOOK5 ,;
            BOOK6 WITH BOOK6 + M.BOOK6 ,;
            BOOK7 WITH BOOK7 + M.BOOK7 ,;
            BOOK8 WITH BOOK8 + M.BOOK8
  ENDIF
  
  *-- Update open qty and amt for the whole order
  
  SELECT &lcOrdLine
  REPLACE TOTQTY  WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8,;
          TOTBOOK WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8
          
  *C037287,1 MMM 11/30/2003 The Line discount placed in the line discount field and Update 
  *                         The Ordline.Desc1 with the style Description [Start]
  REPLACE Price WITH GROS_PRICE * (100 - disc_pcnt )/100
  lcSct= SELECT()
  SELECT STYLE
  =SEEK(m.Style)
  cDesc = STYLE.Desc1
  SELECT (lcSct) 
  REPLACE Desc1 WITH cDesc    
  *C037287,1 MMM [End]  
      
ENDSCAN

*-- Save last order
IF !EMPTY(M.ORDER) AND llErrFree
  INSERT INTO &lcOrdHdr FROM MEMVAR
ENDIF  

*B607492,1  TMI [Start] Chenge the order to SPCK_HDR instead of SKU_STYLE order
SET ORDER TO SPCK_HDR IN SPCK_HDR
*B607492,1  TMI [End  ] 

SELECT &lcOrdHdr
GO TOP
SCAN          &&*-- Update Open qty and Amt for the imported orders
  =SEEK(&lcOrdHdr..CORDTYPE+&lcOrdHdr..ORDER,lcOrdLine)
  SELECT &lcOrdLine
  SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) =  &lcOrdHdr..CORDTYPE+&lcOrdHdr..ORDER
    *- Check ICDESGN file
    IF llBomVarnt
      *- Get pack_id from spck_hdr file
      *B607492,1  TMI [Start] Use the saved field PackID instead
      *=SEEK('S'+&lcOrdLine..ACCOUNT+SUBSTR(&lcOrdLine..STYLE,1,lnClrPos+lnClrLen-1),'SPCK_HDR')          
      =SEEK('S'+&lcOrdLine..ACCOUNT+&lcOrdLine..PACKID,'SPCK_HDR')
      *B607492,1  TMI [End  ] 
      *C200575,3  TMI [Start] Use the style exits in lcOrdline file , not a memory variable
      *IF SEEK(SUBSTR(M.STYLE,1,lnClrPos+lnClrLen-1),'ICSTYPOS') .AND. ;
         SEEK(PADR(SPCK_HDR.PACK_ID,8),'ICDESIGN')
      IF SEEK(SUBSTR(&lcOrdLine..STYLE,1,lnClrPos+lnClrLen-1),'ICSTYPOS') .AND. ;
         SEEK(PADR(SPCK_HDR.PACK_ID,8),'ICDESIGN')
        *C200575,3  TMI [End  ]   
        *- Update lcT_BomVar temp file 
        SELECT &lcT_Bomvar
        SCATTER MEMVAR MEMO BLANK
        m.CIDTYPE   = 'SO'
        m.cCost_id  = &lcOrdHdr..ORDER
        m.LINENO    = &lcOrdLine..LINENO    
        m.CSTYLEPOS = ICSTYPOS.CSTYLEPOS 
        m.CDSGNCODE = ICDESIGN.CDSGNCODE 
        m.NPRICEADD = ICDESIGN.NDSGNPRC                  
        
        m.UNTCOST   = ICDESIGN.NDSGNCST  
        m.NBOMTOTQTY= 1
        m.TOTCOST   = m.UNTCOST * m.NBOMTOTQTY        
        m.CADD_VER  = 'A27'
        m.CSTATUS   = 'A'
        
        m.CADD_USER = gcUser_Id
        m.dAdd_Date = gdSysDate
        m.cAdd_Time = Time()
        
        INSERT INTO &lcT_Bomvar FROM MEMVAR

        *- Update lcOrdLine temp file
        REPLACE  &lcOrdLine..PRICE      WITH &lcOrdLine..PRICE      + ICDESIGN.NDSGNPRC,;
                 &lcOrdLine..GROS_PRICE WITH &lcOrdLine..GROS_PRICE + ICDESIGN.NDSGNPRC  
      
           
      ENDIF
    ENDIF

    
    *- Update Open,Book qty's and Amnts
    SELECT &lcOrdHdr
    
    *C037287,1 MMM 11/30/2003 The Line discount placed in the line discount field [Start]
    *REPLACE &lcOrdHdr..OPEN    WITH &lcOrdHdr..OPEN    + &lcOrdLine..TOTQTY,;
            &lcOrdHdr..OPENAMT WITH &lcOrdHdr..OPENAMT + &lcOrdLine..TOTQTY*&lcOrdLine..PRICE,;
            &lcOrdHdr..BOOK    WITH &lcOrdHdr..OPEN    ,;
            *&lcOrdHdr..BOOKAMT WITH &lcOrdHdr..OPENAMT 
            
    REPLACE &lcOrdHdr..OPEN    WITH &lcOrdHdr..OPEN    + &lcOrdLine..TOTQTY,;
            &lcOrdHdr..OPENAMT WITH &lcOrdHdr..OPENAMT + &lcOrdLine..TOTQTY*(&lcOrdLine..GROS_PRICE *(100 - &lcOrdLine..disc_pcnt)/100),;
            &lcOrdHdr..BOOK    WITH &lcOrdHdr..OPEN    ,;
            &lcOrdHdr..BOOKAMT WITH &lcOrdHdr..OPENAMT 
    *C037287,1 MMM [End] 
           
  ENDSCAN  
ENDSCAN

WAIT CLEAR

*- Handling errors
IF lnErrCnt>0 
  *-- Button  : 32013
  *-- Options : View the log file , continue , Cancel

  lnResp = 0
  lcErroSrc = gcWorkDir + 'IMPORT.LOG'
  *C200575,3  TMI [Start] Proceed button status 
  lcPrcStat = IIF(RECCOUNT(lcOrdHdr)>0,'ENABLE','DISABLE')  
  *C200575,3  TMI [End  ] 
  DO (gcScrDir +  gcAct_Appl + '\Soimerr.SPX')
  
  *-- If proceed then save orders with no errors
  IF lnResp = 1
    =lfSaveData()
  ENDIF  
  
  *-- if selected log file copy error to it
  IF !EMPTY(lcErroSrc)
    SELECT &lcLogFile  
    COPY TO (lcErroSrc) SDF
  ENDIF    
ELSE
  *-- no errors then save
  =lfSaveData()
ENDIF

*--Close temp files
=lfCloseFls()

*-- end of lfImpFrAsc.

*:**************************************************************************
*:* Name        : lfvOldVal
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/03
*:* Purpose     : Old value function
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvOldVal
lcOldValue = EVAL(SYS(18))
*-- end of lfvOldVal.

*:**************************************************************************
*:* Name        : lfGtSku
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2003
*:* Purpose     : Get Sku of the current style account from spck_lin file
*:***************************************************************************
*: Parametes    : Size # that with qty>0 ,passed as Character
*:* Called from : lfImpFrAsc
*:***************************************************************************
FUNCTION lfSku2Sty
PARAMETERS lcSKU
PRIVATE lnSz,lcSz
lnSlct = SELECT()
M.STYLE = CHR(255)

*-- since the sku id saved in the csv file do not contain the pack id so we locate only by 
*-- the sku part of the zise not the main part

SELECT SPCK_LIN
=SEEK('S'+M.ACCOUNT,'SPCK_LIN')
LOCATE REST WHILE TYPE+ACCOUNT+PACK_ID+STYLE = 'S'+M.ACCOUNT ;
            FOR ALLTRIM(&lcTmpOrd..SKU) = ;
                RIGHT(ALLTRIM(SPCK_LIN.PACK_ID),LEN(ALLTRIM(&lcTmpOrd..SKU)))
IF FOUND()
  *C200575,7  TMI [Start] Get PackID , save it to the new customized field Ordline.PACKID
  M.PACKID = SUBSTR(ALLTRIM(SPCK_LIN.PACK_ID),1,LEN(ALLTRIM(SPCK_LIN.PACK_ID))-LEN(ALLTRIM(&lcTmpOrd..SKU)) )
  *C200575,7  TMI [End  ] 
  =SEEK(SPCK_LIN.STYLE,'STYLE')
  =SEEK('S'+STYLE.SCALE,'SCALE')
  M.STYLE = STYLE.STYLE
  M.SCALE = SCALE.SCALE
  SELECT SPCK_LIN 
  FOR lnSz = 1 TO SCALE.CNT
    lcSz = STR(lnSz,1)
    IF SPCK_LIN.QTY&lcSz > 0
      M.QTY&lcSz = &lcTmpOrd..QTY
      M.BOOK&lcSz = &lcTmpOrd..QTY
    ENDIF    
  ENDFOR
    
ENDIF
M.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
SELECT (lnSlct)
*-- end of lfGtSku.

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/14/2003
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpenFls
*C200575,4  TMI [Start] Ask if the file is open befor opening any file
IF !USED('STYLE'    )
  =gfOpenFile(gcDataDir+'STYLE'    ,'STYLE','SH')
ENDIF  
IF !USED('SCALE'    )
  =gfOpenFile(gcDataDir+'SCALE'    ,'SCALE','SH')
ENDIF  
IF !USED('WAREHOUS' )
  =gfOpenFile(gcDataDir+'WAREHOUS' ,'WAREHOUS','SH')
ENDIF  
IF !USED('CUSTOMER' )
  =gfOpenFile(gcDataDir+'CUSTOMER' ,'CUSTOMER','SH')
ENDIF  
IF !USED('SALESREP' )
  =gfOpenFile(gcDataDir+'SALESREP' ,'SALESREP','SH')
ENDIF  
IF !USED('ORDHDR'   )
  =gfOpenFile(gcDataDir+'ORDHDR'   ,'ORDHDR','SH')
ENDIF  
IF !USED('ORDLINE'  )
  =gfOpenFile(gcDataDir+'ORDLINE'  ,'ORDLINST','SH')
ENDIF  
IF !USED('SYCCURR'  )
  =gfOpenFile(gcSysHome+'SYCCURR'  ,'CCURRCODE','SH')
ENDIF  
IF !USED('SYCEXCH'  )
  =gfOpenFile(gcSysHome+'SYCEXCH'  ,'CURRENCY','SH')
ENDIF  

IF !USED('UnCmSess' )
  =gfOpenFile(gcDataDir+'UnCmSess' ,gcDataDir+'TRANS','SH')
ENDIF  
IF !USED('STYDYE'   )
  =gfOpenFile(gcDataDir+'STYDYE'   ,gcDataDir+'STYDYE','SH')
ENDIF  
SET ORDER TO TAG CURRENCY DESC IN 'SYCEXCH'

IF !USED('SPCK_LIN' )
  =gfOpenFile(gcDataDir+'SPCK_LIN' ,'SPCK_LIN','SH')  && Key :: TYPE+ACCOUNT+PACK_ID+STYLE
ENDIF  
IF !USED('SPCK_HDR' )
  =gfOpenFile(gcDataDir+'SPCK_HDR' ,'SKU_STYLE','SH')  && Key :: TYPE+ACCOUNT+STYLE+PACK_ID
ENDIF  
IF !USED('CODES'    )
  =gfOpenFile(gcDataDir+'CODES'    ,'CCODE_NO','SH')  && Key :: CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
ENDIF  
IF !USED('SKUTMPL'  )
  =gfOpenFile(gcDataDir+'SKUTMPL'  ,'SKUTMPL','SH')
ENDIF  

*- If use variant cost sheet is Yes then open bomvar file
IF llBomVarnt
  IF !USED('BOMVAR'   )
    =gfOpenFile(gcDataDir+'BOMVAR'   ,'BOMVAR','SH')
  ENDIF  
  IF !USED('ICDESIGN' )
    =gfOpenFile(gcDataDir+'ICDESIGN' ,'ICDESIGN','SH')
  ENDIF  
  IF !USED('ICSTYPOS' )
    =gfOpenFile(gcDataDir+'ICSTYPOS' ,'ICSTYPOS','SH')
  ENDIF  
ENDIF
*C200575,4  TMI [End  ] 

*C037287,1 MMM 11/30/2003 Place the order notes in the notepad file [Start]
IF !USED('NOTEPAD' )
  =gfOpenFile(gcDataDir+'NOTEPAD' ,'NOTEPAD','SH')
ENDIF
*C037287,1 MMM [End]

*C200575,5  TMI [Start] no need to open this file since only one account is selected
*- *- Open the selected accounts file
*- IF FILE(gcWorkDir+lcSlctdAcc+'.DBF')
*-   =gfOpenFile(gcWorkDir+lcSlctdAcc,'','SH')
*-   *C200575,3  TMI [Start] create index on account field
*-   INDEX ON ACCOUNT TAG ACCOUNT
*-   *C200575,3  TMI [End  ] 
*- ENDIF
*C200575,5  TMI [End  ] 
*C200575,3  TMI [Start] Comment this line and create the index explicitly
*SET ORDER TO 1
*C200575,3  TMI [End  ] 
*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfCratTemp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/14/2003
*:* Purpose     : Create temp files for Ordhdr and Ordline
*:***************************************************************************
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex

SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

=gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+ORDER],lcOrdHdr)

SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lContract'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

*C200575,7  TMI [Start] Add a new index with "PackID" field
*DECLARE laIndex[3,2]
DECLARE laIndex[4,2]
*C200575,7  TMI [End  ] 
laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
*C200575,7  TMI [Start] Add new index with 'PackID' field
laIndex[4,1] = 'cOrdType+ORDER+STORE+STYLE+PACKID'
laIndex[4,2] = 'ORDLINPK'
*C200575,7  TMI [End  ] 
=gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)
*C200575,7  TMI [Start] Set order to the new index 'ORDLINPK'
*SET ORDER TO TAG 'ORDLINST' IN (lcOrdLine)
SET ORDER TO TAG 'ORDLINPK' IN (lcOrdLine)
*C200575,7  TMI [End  ] 
SCATTER MEMVAR BLANK MEMO

*-- Create error log file
CREATE TABLE (gcWorkDir+lcLogFile) (cErro C(120))
INSERT INTO (lcLogFile) VALUES ('-------------------------------------------------------------------------')
INSERT INTO (lcLogFile) VALUES ('Data imported from file '+ lcRpImpFil +' at Date:'+DTOC(DATE())+' Time:'+TIME())
INSERT INTO (lcLogFile) VALUES ('-------------------------------------------------------------------------')

IF llBomVarnt
  PRIVATE laBomVarSt
  SELECT BomVar
  =AFIELDS(laBomVarSt)
  lnBomVarSt = ALEN(laBomVarSt,1)
  DIMENSION laBomVarSt[lnBomVarSt + 2 , 4]
  laBomVarSt[lnBomVarSt+1,1] = 'nRecno'
  laBomVarSt[lnBomVarSt+1,2] = 'N'
  laBomVarSt[lnBomVarSt+1,3] = 10
  laBomVarSt[lnBomVarSt+1,4] = 0
  laBomVarSt[lnBomVarSt+2,1] = 'cStatus'
  laBomVarSt[lnBomVarSt+2,2] = 'C'
  laBomVarSt[lnBomVarSt+2,3] = 1
  laBomVarSt[lnBomVarSt+2,4] = 0
  =gfCrtTmp(lcT_BomVar,@laBomVarSt,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
  USE IN &lcT_BomVar
  =gfOpenFile(gcWorkDir+lcT_BomVar,lcT_BomVar,'EX')
ENDIF
*-- end of lfCratTemp.

*:**************************************************************************
*:* Name        : lfTempCSV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/14/2003
*:* Purpose     : Create temp file and populate it with the selected csv file
*:***************************************************************************
FUNCTION lfTempCSV
*C200575,3  TMI [Start] define new variables
PRIVATE lcDelStat,lcEmpAcc,lcEmpOrd
lcDelStat = SET('DELETED')
SET DELETED OFF
*C200575,3  TMI [End  ] 

*C200575,4  TMI [Start] 1)Let Entered and Complete fields be Chracters C(6),
*C200575,4              2)Remove the field "NUM" since no need to it in the final csv file format
*C200575,4              3)Let DLVCODE be 13 characters

*C037287,1 MMM 11/30/2003 Add 2 fields one for discount and other for notes [Start]
*CREATE TABLE &gcDataDir.&lcTmpOrd (ACCOUNT C(5),ORDER C(6),ENTERED D,DLVCODE C(6),;
                                   COMPLETE D,SKU C(16),QTY N(7),NUM N(1),PRICE N(9,3))
                                   
CREATE TABLE &gcDataDir.&lcTmpOrd (ACCOUNT C(5),ORDER C(6),ENTERED C(6),DLVCODE C(13),;
                                   COMPLETE C(6),SKU C(16),QTY N(7),PRICE N(9,3),;
                                   disc_pcnt N(6,2),mnotes C(254))
                                   
*C037287,1 MMM [End]
                                                                       
*C200575,4  TMI [End  ] 
APPEND FROM &lcRpImpFil DELIMITED

GO TOP
lcEmpAcc = ''
lcEmpOrd = ''
SCAN   
  IF EMPTY(ACCOUNT) OR EMPTY(ORDER)
    lcEmpAcc = lcEmpAcc + IIF(EMPTY(ACCOUNT), ','+ALLTRIM(STR(RECNO())) , '' )
    lcEmpOrd = lcEmpOrd + IIF(EMPTY(ORDER)  , ','+ALLTRIM(STR(RECNO())) , '' )
    DELETE    
  ELSE
    IF !EMPTY(ORDER)      
      REPLACE ACCOUNT WITH UPPER(ACCOUNT),; 
              ORDER WITH UPPER(PADL(ALLTRIM(ORDER),6,'0'))
      *C200575,3  TMI [Start] convert sku to upper case
      REPLACE SKU WITH UPPER(SKU)
      *C200575,3  TMI [End  ]         
      *C200575,4  TMI [Start] 
      REPLACE DLVCODE WITH UPPER(DLVCODE)
      *C200575,4  TMI [End  ] 
    ENDIF
  ENDIF
ENDSCA
SET DELETED &lcDelStat
*C200575,4  TMI [Start] make the index be on "order" field only
*INDEX ON ACCOUNT+ORDER TAG &lcTmpOrd
INDEX ON ORDER TAG &lcTmpOrd
*C200575,4  TMI [End  ] 


IF !EMPTY(lcEmpAcc)
  lcTxt = ' Lines are ignored from the csv file due to empty Account field:'+SUBSTR(lcEmpAcc,2)+'.'
  INSERT INTO (lcLogFile) VALUES (lcTxt)
  *--Increase error counter
  lnErrCnt = lnErrCnt + 1 
ENDIF
IF !EMPTY(lcEmpOrd)
  lcTxt = ' Lines are ignored from the csv file due to empty Order field:'+SUBSTR(lcEmpOrd,2)+'.'
  INSERT INTO (lcLogFile) VALUES (lcTxt)
  *--Increase error counter
  lnErrCnt = lnErrCnt + 1 
ENDIF
GO TOP
*-- end of lfTempCSV.

*:**************************************************************************
*:* Name        : lfPopDef
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/14/2003
*:* Purpose     : Get defaults for Div,Seas and WH
*:***************************************************************************
FUNCTION lfPopDef
PRIVATE lnSlct
lnSlct = SELECT()

IF SEEK('DSEASON    ','CODES')
  lcDefSesn = CODES.CCODE_NO
ENDIF

IF SEEK('DCDIVISION ','CODES')
  lcDefDiv = CODES.CCODE_NO
ENDIF

SELECT WAREHOUS
GO TOP
lcDefWH = WAREHOUS.CWARECODE 

SELECT (lnSlct)
*-- end of lfPopDef.

*:**************************************************************************
*:* Name        : lfErr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2003
*:* Purpose     : SKip current order
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErr()
*:***************************************************************************
FUNCTION lfErr
PARAMETERS lcReason
PRIVATE lcTxt,lnSlct
lnSlct = SELECT()

llErrFree = .F.

*-- Write to alternate file
IF !EMPTY(lcReason)
  DO CASE
    CASE lcReason = 'ACCOUNT'
      lcTxt = ' Account:'+m.Account+' not found.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
  
      *- Skip rest lines of this account
      SELECT &lcTmpOrd
      *C200575,4  TMI [Start] loop for the current order only
      *SCAN REST WHILE ACCOUNT+ORDER = M.ACCOUNT
      SCAN REST WHILE ORDER = M.ORDER
      ENDSCAN
      *C200575,4  TMI [End  ] 

    *C200575,4  TMI [Start] If store is not found in custmer file
    CASE lcReason = 'STORE'
      lcTxt = ' Store:'+m.Store+' not found.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
  
      *- Skip rest lines of this account
      SELECT &lcTmpOrd
      SCAN REST WHILE ORDER = M.ORDER
      ENDSCAN
    *C200575,4  TMI [End  ] 

    CASE lcReason = 'ORDER'
      lcTxt = ' Order:'+m.Order+' already exists.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

      *- Skip rest lines of this order
      SELECT &lcTmpOrd
      *C200575,4  TMI [Start] the key will be "Order" field only
      *SCAN REST WHILE ACCOUNT+ORDER = M.ACCOUNT+M.ORDER
      SCAN REST WHILE ORDER = M.ORDER
      ENDSCAN
      *C200575,4  TMI [End  ] 

    *C200575,3  TMI [Start] check case no entered or complete date exists , or entered is after complete
    CASE lcReason = 'ENTERED'
      lcTxt = 'No entered date exits for order '+M.ORDER+', Order skipped'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
      =lfSkpOrd()

    CASE lcReason = 'COMPLETE'
      lcTxt = 'No complete date exits for order '+M.ORDER+', Order skipped'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
      =lfSkpOrd()

    CASE lcReason = 'DATEMISMATCH'
      lcTxt = 'Entered date is after complete date, Order '+M.ORDER+' skipped.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
      =lfSkpOrd()
    *C200575,3  TMI [End  ] 

    CASE lcReason = 'SKU'
      lcTxt = ' Order:'+m.Order+' SKU:'+&lcTmpOrd..SKU+' is not assigned to any style for account '+M.Account
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'QTY'
      lcTxt = ' Order:'+m.Order+' SKU:'+&lcTmpOrd..SKU+' has zero qty.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'PRICE'
      lcTxt = ' Order:'+m.Order+' SKU:'+&lcTmpOrd..SKU+' has zero price.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'SEASON'
      lcTxt = ' Order:'+m.Order+' Style:'+m.STYLE+' season other than the default.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'CDIVISION'
      lcTxt = ' Order:'+m.Order+' Style:'+m.STYLE+' division other than the default.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

  ENDCASE
  *--Increase error counter
  lnErrCnt = lnErrCnt + 1 

ELSE

  *- Skip rest lines of this account
  SELECT &lcTmpOrd
  *C200575,4  TMI [Start] the key will be "Order" field only
  *SCAN REST WHILE ACCOUNT+ORDER = M.ACCOUNT
  SCAN REST WHILE ORDER = M.ORDER
  ENDSCAN
  *C200575,4  TMI [End  ] 

ENDIF

SELECT (lnSlct)
*-- end of lfErr.

*:**************************************************************************
*:* Name        : lfCloseFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2003
*:* Purpose     : Close temp files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCloseFls()
*:***************************************************************************
FUNCTION lfCloseFls
IF USED(lcOrdHdr)
  USE IN (lcOrdHdr)
ENDIF
ERASE (gcWorkDir+lcOrdHdr+'.DBF')
ERASE (gcWorkDir+lcOrdHdr+'.CDX')

IF USED(lcOrdLine)
  USE IN (lcOrdLine)
ENDIF
ERASE (gcWorkDir+lcOrdLine+'.DBF')
ERASE (gcWorkDir+lcOrdLine+'.CDX')

IF llBomVarnt
  IF USED(lcT_BomVar)
    USE IN (lcT_BomVar)
  ENDIF
  ERASE (gcWorkDir+lcT_BomVar+'.DBF')
  ERASE (gcWorkDir+lcT_BomVar+'.CDX')
ENDIF 

*-- end of lfCloseFls.

*:**************************************************************************
*:* Name        : lfSaveData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2003
*:* Purpose     : Save temp file to Actual orders
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSaveData()
*:***************************************************************************
FUNCTION lfSaveData

EXTERNAL ARRAY laData , laKeyField
PRIVATE llOrdSaved
STORE 'O'  TO lcOrdType

DECLARE laWareHouses[1,2]  , laSetups[6,2] , laKeyField[2,4] , laSeasons[1,2] ,;
        laCodes[1,10]      , laSource[1,2]        
STORE ''   TO laWareHouses , lcODefWare    , lcWareHouse     , lcScFields ,;
             laSeasons     , laCodes       , lcODefSes       , lcSeason             
STORE .F.  TO laSetups     , llFound1      , llFound2        , llBrowse   
             
STORE 1    TO lnWareHouse  , lnOrdStatus   , lnSeason
STORE 0    TO lnBook       , lnOpen
STORE 0.00 TO lnOpenAmt    , lnBookAmt     , lnTotAmt
             
*-- variables of SOUPDATE.PRG [Start]
DECLARE laVariables[6] , laScrMode[4]
*STORE .F. TO llContinue , llBomVarnt , llCDPerL
STORE .F. TO llContinue  , llCDPerL
STORE ''  TO lcFlToUpd  , lcSession  , lcFiles   , laVariables , lcGlYear , lcGlPeriod ,;
             lcExRsin   , lcUntSin   , lcODefDiv , lcScrMode   , lcCurrOrd
STORE {}  TO ldDefOrdDate
lcFlToUpd = gfTempName()
*-- variables of SOUPDATE.PRG [End]

laSetups[1,1]  = 'M_PACK'           && System has been steup to use packs
laSetups[2,1]  = 'M_STY_COM'        && Edit sales reps commissions at style level
laSetups[3,1]  = 'M_OR_NOTE'        && Edit order lines notepad
laSetups[4,1]  = 'M_LINK_GL'        && System has been linked to GL
laSetups[5,1]  = 'M_WareHouse'      && System has been steup to use multiple warehouses
laSetups[6,1]  = 'M_GenOrNum'       && Generate order number manually
=gfGetMemVar(@laSetups,gcAct_Comp)

laCodes[1,1] = 'SEASON'
laCodes[1,2] = 'laSeasons'
laCodes[1,3] = 'lnSeason'
laCodes[1,4] = ' '
laCodes[1,5] = .T.
laCodes[1,6] = .F.
laCodes[1,10] = 'SEASON'
=gfwCodePop(@laCodes,'SEASON','L')

lnSeason   = ASCAN('laSeasons',lcODefSes)
lnSeason   = IIF(lnSeason=0,1,ASUBSCRIPT(laSeasons,lnSeason,1))

*-- variables of SOUPDATE.PRG [Start]
laVariables[1] = 'ldDefOrdDate'
laVariables[2] = 'lcODefSes'  
laVariables[3] = 'lcODefDiv'
laVariables[4] = 'lcODefWare'
laVariables[5] = 'lcScrMode'
laVariables[6] = 'lcCurrOrd'
*-- variables of SOUPDATE.PRG [End]

lcScFields = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
             'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
             'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
             'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
             'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
             'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
             'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
             'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS,LFROMWEB'

SELECT WAREHOUS
SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
DIMENSION laFileName[1,4]
laFileName = ' '

=gfCloseFile('WAREHOUS')

lnWareHouse = ASCAN(laWareHouses,lcODefWare)
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))

laScrMode[4] = .T.
STORE .F. TO llMFDsPrc , llPoDsPrc
llOrdSaved = .F.
*--Loop throght the temp file lcOrdHdr to save data

SELECT (lcOrdHdr)
*C037287,1 MMM 11/30/2003 Use the next sequential order no from the sequence file instead of the no in the import file.[Start]  
SCAN
  lcOldOrderNO= &lcOrdHdr..order
  lcOrderNo= gfSequence('ORDER')
  REPLACE cOwner WITH Order
  REPLACE Order WITH lcOrderNo
  SELECT &lcOrdLine
  =SEEK('O'+ALLTRIM(&lcOrdHdr..cOwner))
  REPLACE REST cOwner WITH lcOrderNo WHILE CORDTYPE+ORDER = 'O'+ALLTRIM(&lcOrdHdr..cOwner)
  SELECT &lcT_Bomvar
  =SEEK('SO'+ALLTRIM(&lcOrdHdr..cOwner))
  REPLACE REST CCOST_ID WITH lcOrderNo WHILE cidtype+ccost_id+STR(lineno,6) = 'SO'+ALLTRIM(&lcOrdHdr..cOwner)
ENDSCAN
*C037287,1 MMM [End]

SELECT (lcOrdHdr)

*C200575,6  TMI [Start] 
GO TOP
STORE Order TO lcFrstOrd,lcLastOrd
*C200575,6  TMI [End  ] 

SCAN
  llOrdSaved = .T.  
  SCATTER FIELDS &lcScFields TO laData
  
  SELECT &lcOrdLine  
  SET FILTER TO CORDTYPE+ORDER = 'O'+ALLTRIM(&lcOrdHdr..cOwner)
  GO TOP
   
  SELECT &lcOrdHdr
 
  llContinue = .T. 
   
    
  *C200575,6  TMI [Start] Don't show the message "Order has been saved with # ?????? " for each one
  *DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
  lcLastOrd = Order
  
  *C037287,1 MMM 11/30/2003 Update ordhdr dept field with the first three digits of the sku
  *                         number and update the ordhdr.custpo with the orderno. [Start]
                              
  lcSlct = SELECT()
  SELECT &lcTmpOrd
  LOCATE FOR Order = ALLTRIM(&lcOrdHdr..cOwner)
  lcSku = &lcTmpOrd..Sku
  ladata[17] = substr(lcSku,1,3)
  SELECT(lcSlct)
  *ladata[4] = lcOldOrderNO
  ladata[4] = cOwner
  *C037287,1 MMM [End]
    
  DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP') WITH .F.,.T.
   
  *C037287,1 MMM 11/30/2003 Place the order notes in the notepad file [Start] 
  =gfAdd_Info('ORDHDR')
  lcSlct= SELECT() 
  lcNtTxt = 'Notes For Order Number'+ &lcOrdHdr..ORDER
  SELECT &lcTmpOrd
  LOCATE FOR &lcTmpOrd..Order = ALLTRIM(&lcOrdHdr..cOwner)
  lcNote = &lcTmpOrd..mNotes
  IF !EMPTY(lcNote)    
    REPLACE ORDHDR.lHasNotes WITH .T.
  ENDIF
  SELECT NOTEPAD
  INSERT INTO NOTEPAD (Type, Key, Cdesc, Mnotes) ;
    VALUES ('B', ORDHDR.ORDER, lcNtTxt, lcNote)
    
  SELECT (lcSlct)                                                 
  *C037287,1 MMM [End]
  
  *C200575,6  TMI [End] 
  
ENDSCAN
IF !llOrdSaved
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
  *C200575,6  TMI [Start] Show a message for all saved orders
ELSE
  =gfModalGen('TRM38134B00000','DIALOG','Orders|'+lcFrstOrd+'|'+lcLastOrd) 
    
  *C200575,6  TMI [End  ] 
  
ENDIF

*-- end of lfSaveData.

*:**************************************************************************
*:* Name        : lfvErrSrc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2003
*:* Purpose     : change the selected error file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvErrSrc()
*:***************************************************************************
FUNCTION lfvErrSrc
lcErroSrc = GETFILE('LOG', 'Log file: def.(Import.Log) ', 'Select',1)
SHOW GET lcErroSrc DISABLE

*-- end of lfvErrSrc.

*:**************************************************************************
*:* Name        : lfGtSkuTmpl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2003
*:* Purpose     : Get data from skutmpl file
*:***************************************************************************
FUNCTION lfGtSkuTmpl

*-- Get template dimensions
=SEEK('M'+ORDLINE.ACCOUNT,'Customer')
lcSkuTmpl = IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
   lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
   lnDime2 = SkuTmpl.Len4
ELSE
   lnDime1 = 8
   lnDime2 = 8
ENDIF
*-- end of lfGtSkuTmpl.

*:**************************************************************************
*:* Name        : lfVPrnt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/15/2003
*:* Purpose     : valid function of print button in the log screen
*:***************************************************************************
FUNCTION lfVPrnt
PRIVATE lnSlct
lnSlct = SELECT()
SELECT (lcLogFile)

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY TO (gcOutFile) TYPE DELIMITED
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

SELECT (lnSlct)

*:**************************************************************************
*:* Name        : lfGetClrD                                   
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/28/2003
*:* Purpose     : Get color width for David luck
*:***************************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF  
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfSkpOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/05/2003
*:* Purpose     : Skip rest lines of this order
*:***************************************************************************
*C200575,3
FUNCTION lfSkpOrd
PRIVATE lnSlct
lnSlct = SELECT()
SELECT &lcTmpOrd
*C200575,4  TMI [Start]  the key will be "Order" field only
*SCAN REST WHILE ACCOUNT+ORDER = M.ACCOUNT+M.ORDER
SCAN REST WHILE ORDER = M.ORDER
ENDSCAN
*C200575,4  TMI [End  ] 

SELECT (lnSlct)
*-- end of lfSkpOrd.

*:**************************************************************************
*:* Name        : lfGtDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 18/08/2003
*:* Purpose     : Get date type from a character string
*:***************************************************************************
FUNCTION lfGtDt
PARAMETER lcDt
PRIVATE ldRetdat
ldRetdat = SUBSTR(lcDt,1,2)+'/'+;
           SUBSTR(lcDt,3,2)+'/'+;
           IIF(VAL(SUBSTR(lcDt,5,2))>50,'19','20')+SUBSTR(lcDt,5,2)
RETURN CTOD(ldRetdat)
*-- end of lfGtDt.

*:**************************************************************************
*:* Name        : lfGtStore
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/19/2003
*:* Purpose     : Get store depending on custom field CUSTOMER.STORELOCNO
*:***************************************************************************
*C200575,5  
FUNCTION lfGtStore
PARAMETERS lcLoc
PRIVATE lnSlct,lcRetStor
lnSlct = SELECT()

lcLoc = UPPER(lcLoc)
lcRetStor = ''
IF !EMPTY(lcLoc) .AND. SEEK('S'+lcAccnt,'CUSTOMER')   
  SELECT CUSTOMER  
  LOCATE REST WHILE TYPE+ACCOUNT+STORE = 'S'+lcAccnt ;
              FOR   STORELOCNO = lcLoc
  IF FOUND()
    lcRetStor = CUSTOMER.STORE
  ENDIF
  =SEEK('M'+lcAccnt,'CUSTOMER')
ENDIF

RETURN lcRetStor
SELECT (lnSlct)
*-- end of lfGtStore.