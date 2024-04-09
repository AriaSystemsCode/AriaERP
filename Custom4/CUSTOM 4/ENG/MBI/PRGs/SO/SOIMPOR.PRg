*:************************************************************************
*: Program file  : SOIMPOR.PRG    
*: Program desc. : Import Orders from CSV file(11/20/2007)
*:         System: ARIA 4.0 XP
*:         Module: SO [T20071029.0002]
*:      Developer: Mariam Mazhar (MMT) (C200892)(C200893)
*:************************************************************************
*: Modifications:
*:MMT,11/27/2007 Fix bug reported by customer
*:C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [T20090624.0001]
*:B609003,1 TMI 9/10/2009 The program does not import decimals [T20090624.0001]
*:************************************************************************
lcLogFile = '' 
lcImpDirPath  = gfGetMemVar('M_IMWBDR')
lcHistDirPath  = gfGetMemVar('M_WBHIST')


IF EMPTY(lcImpDirPath) OR (NOT DIRECTORY(lcImpDirPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Import Web SO Directory")
  RETURN 
ENDIF 

IF EMPTY(lcHistDirPath) OR (NOT DIRECTORY(lcHistDirPath))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Web SO History Directory")
  RETURN 
ENDIF 


lcTempHead = ""
lcTempLine = ""
opross = Null

lcOldDefaPath = SET("Default")
SET DEFAULT TO (lcImpDirPath)

DIMENSION laFileToImport[1]

IF ADIR(laFileToImport,"*.CSV") = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"There is no CSV files to import")
  SET DEFAULT TO (lcOldDefaPath)
  RETURN 
ELSE
  opross = CREATEOBJECT('ariaprogressbar')  
  oPross.TotalProgress = ALEN(laFileToImport,1)  
  oPross.AutoCenter = .T.
  oPross.Show()
  oPross.Caption = 'Checking Files to be Imported'
  
  lcTempHead = gfTempName()
  lcTempLine = gfTempName()
  lcLogFile = gfTempName()
  
  lfCreatTemp()


  lnPrepRec = 0
  FOR lnI = 1 TO ALEN(laFileToImport,1)  
  
    lnPrepRec = lnPrepRec + 1            
    oPross.CurrentProgress(lnPrepRec)
    
    STORE FOPEN(laFileToImport[lnI,1]) TO gnFileHandle    && Open the file
    STORE FSEEK(gnFileHandle, 0, 2) TO gnEnd    && Move pointer to EOF
    STORE FSEEK(gnFileHandle, 0) TO gnTop     && Move pointer to BOF
    lcString = FGETS(gnFileHandle,gnEnd)
    llFirstLine = .T.
    DO WHILE !(EMPTY(lcString))
      IF llFirstLine
        SELECT(lcTempHead) 
        APPEND BLANK 
        REPLACE CfileName WITH laFileToImport[lnI,1]
        IF !EMPTY(lcString)
          lnStart=1 
          lnFielCnt = 1
          lnEnd=AT(',',lcString)
          DO WHILE lnEnd <> 0
            IF lnFielCnt = 1
              REPLACE Account  WITH SUBSTR(lcString,lnStart,lnEnd-1)
              lnFielCnt = lnFielCnt + 1
            ELSE
              IF lnFielCnt = 2
                REPLACE Store WITH SUBSTR(lcString,lnStart,lnEnd-1)
                lnFielCnt = lnFielCnt + 1
              ELSE
                IF lnFielCnt = 3
                  REPLACE Caddress1 WITH SUBSTR(lcString,lnStart,lnEnd-1)
                  lnFielCnt = lnFielCnt + 1
                ELSE
                  IF lnFielCnt = 4
                    REPLACE Caddress2 WITH SUBSTR(lcString,lnStart,lnEnd-1)
                    lnFielCnt = lnFielCnt + 1
                  ELSE
                    IF lnFielCnt = 5
                      REPLACE Caddress3 WITH SUBSTR(lcString,lnStart,lnEnd-1)
                      lnFielCnt = lnFielCnt + 1
                    ELSE
                      IF lnFielCnt = 6
                        REPLACE Caddress4 WITH SUBSTR(lcString,lnStart,lnEnd-1)
                        lnFielCnt = lnFielCnt + 1
                      ELSE
                        IF lnFielCnt = 7
                          REPLACE Caddress5 WITH SUBSTR(lcString,lnStart,lnEnd-1)
                          lnFielCnt = lnFielCnt + 1
                        ELSE
                          IF lnFielCnt = 8
                            REPLACE WORDER WITH SUBSTR(lcString,lnStart,lnEnd-1)
                            lnFielCnt = lnFielCnt + 1
                          ELSE
                            IF lnFielCnt = 9
                              REPLACE entered WITH CTOD(SUBSTR(lcString,lnStart,lnEnd-1))
                              lnFielCnt = lnFielCnt + 1
                            ELSE
                              IF lnFielCnt = 10
                                REPLACE start WITH CTOD(SUBSTR(lcString,lnStart,lnEnd-1))
                                lnFielCnt = lnFielCnt + 1
                              ELSE
                                IF lnFielCnt = 11
                                  REPLACE Complete WITH CTOD(SUBSTR(lcString,lnStart,lnEnd-1))
                                  lnFielCnt = lnFielCnt + 1
                                ELSE
                                  IF lnFielCnt = 12
                                    REPLACE Order WITH SUBSTR(lcString,lnStart,lnEnd-1) && need to be changed so we save the WEB ORDER #
                                    lnFielCnt = lnFielCnt + 1
                                  ELSE
                                    IF lnFielCnt = 13
                                      REPLACE ccurrcode WITH SUBSTR(lcString,lnStart,lnEnd-1)
                                      lnFielCnt = lnFielCnt + 1
                                      *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [start]
                                    ELSE                                      
                                      IF lnFielCnt = 14
                                        REPLACE NMERAMNT WITH VAL(SUBSTR(lcString,lnStart,lnEnd-1))
                                        lnFielCnt = lnFielCnt + 1
                                      ELSE
                                        IF BETWEEN(lnFielCnt,15,24)
                                          REPLACE Mnotes WITH  Mnotes + Chr(13) + SUBSTR(lcString,lnStart,lnEnd-1)
                                          lnFielCnt = lnFielCnt + 1
                                        ENDIF
                                      ENDIF
                                      *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [end]
                                    ENDIF 
                                  ENDIF 
                                ENDIF 
                              ENDIF 
                            ENDIF 
                          ENDIF 
                        ENDIF 
                      ENDIF   
                    ENDIF 
                  ENDIF 
                ENDIF               
              ENDIF 
            ENDIF   
            lcString= STUFF(lcString,lnStart,lnEnd,"") 
            lnEnd=AT(',',lcString)
          ENDDO 
          IF lnEnd = 0
            SELECT(lcTempHead)
            REPLACE Mnotes   WITH Mnotes + CHR(13) + lcString
            lnFielCnt = lnFielCnt + 1
          ENDIF 
        ENDIF 
        llFirstLine = .F.
      ELSE
        lnStart=1 
        lnFielCnt = 1
        lnEnd=AT(',',lcString)
        SELECT(lcTempLine)
        *IF !SEEK(SUBSTR(lcString,lnStart,lnEnd-1),lcTempLine)
        APPEND BLANK 
        REPLACE WOrder WITH  &lcTempHead..WOrder
        *ENDIF   
        DO WHILE lnEnd <> 0
          IF lnFielCnt = 1
            REPLACE Style WITH SUBSTR(lcString,lnStart,lnEnd-1)
            lnFielCnt = lnFielCnt + 1
          ELSE
            IF lnFielCnt = 2
              REPLACE SclCnt WITH VAL(SUBSTR(lcString,lnStart,lnEnd-1))
              lnFielCnt = lnFielCnt + 1
            ELSE
              IF lnFielCnt = 3 
               IF  BETWEEN(SclCnt ,1,8)
                  lcScnt = ALLTRIM(STR(SclCnt,1))
                  REPLACE Qty&lcScnt. WITH Qty&lcScnt. + VAL(SUBSTR(lcString,lnStart,lnEnd-1))
               ENDIF    
               lnFielCnt = lnFielCnt + 1
              ELSE
               IF lnFielCnt = 4
                 REPLACE Price WITH VAL(SUBSTR(lcString,lnStart,lnEnd-1))
                 lnFielCnt = lnFielCnt + 1
                 *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [start]
               ELSE
               IF lnFielCnt = 5
                   REPLACE CWBPRODREF WITH SUBSTR(lcString,lnStart,lnEnd-1)
                 lnFielCnt = lnFielCnt + 1
               ELSE
               IF lnFielCnt = 6
                   REPLACE CPERSON WITH SUBSTR(lcString,lnStart,lnEnd-1)
                 lnFielCnt = lnFielCnt + 1
               ENDIF
               ENDIF
                 *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [end]             
               ENDIF    
              ENDIF 
            ENDIF 
          ENDIF 
          lcString= STUFF(lcString,lnStart,lnEnd,"") 
          lnEnd=AT(',',lcString)
        ENDDO 
        IF lnEnd = 0 AND !EMPTY(lcString)
          REPLACE Price WITH VAL(lcString)
          *VAL(SUBSTR(lcString,lnStart,lnEnd-1))
          lnFielCnt = lnFielCnt + 1
        ENDIF 
      ENDIF 
      lcString = FGETS(gnFileHandle,gnEnd)
      lcString = lcString + IIF(!EMPTY(lcString ) , ',' , '')
      lnFielCnt = 1
    ENDDO 
    FCLOSE(gnFileHandle) 
  ENDFOR
  opross = Null
ENDIF 


SET DEFAULT TO (lcOldDefaPath)


IF (!EMPTY(lcTempHead) AND USED(lcTempHead)) AND (!EMPTY(lcTempLine) AND USED(lcTempLine))
  =gfOpenTable('ORDLINE','ORDLINE','SH')
  =gfOpenTable('ORDHDR','ORDHDR','SH')
  =gfOpenTable('STYLE','STYLE','SH','STYLE')
  =gfOpenTable('NOTEPAD','NOTEPAD','SH')
  =gfOpenTable('CUSTOMER','CUSTOMER','SH')  
  =gfOpenTable('WAREHOUS','WAREHOUS','SH')  
  SELECT WAREHOUS
  gfSeek('')
  LOCATE FOR warehous.ldefware
  
  opross = CREATEOBJECT('ariaprogressbar')  
  oPross.TotalProgress = RECCOUNT(lcTempHead)
  oPross.AutoCenter = .T.
  oPross.Show()
  lnPrepRec =0
 
   
  *Check if all orders can be Imported 
  SELECT (lcTempHead)
  SCAN
    llvalidQty = .T.
    llValidStyle = .T.
    llValidAcc = .T.
    llValidStore = .T.
    llValidCnt  = .T.
    llValidPrice = .T.
    
    *:MMT,11/27/2007 Fix bug reported by customer[Start]
    llValidCurr = .T.
    llValidRate = .T.
    *:MMT,11/27/2007 Fix bug reported by customer[End]
    
    lnPrepRec = lnPrepRec + 1            
    oPross.CurrentProgress(lnPrepRec)
    oPross.Caption = 'Processing Order No.: ' + WOrder
    llValidAcc = gfSeek('M'+Account,'CUSTOMER','CUSTOMER')
    IF !llValidAcc 
      lfFillLog("Invalid Account Code",&lcTempHead..WOrder,.T.)
    ENDIF 
    llValidStore = IIF(!EMPTY(Store),gfSeek('S'+Account+Store,'CUSTOMER','CUSTOMER'),.T.)
    IF !llValidStore
      lfFillLog("Invalid Store Code",&lcTempHead..WOrder,.T.)
    ENDIF 
    
    *:MMT,11/27/2007 Fix bug reported by customer[Start]
    IF EMPTY(Ccurrcode) AND gfSeek(IIF(!EMPTY(Store),"S","M")+Account+IIF(!EMPTY(Store),Store,""),'CUSTOMER','CUSTOMER')
      REPLACE Ccurrcode  WITH customer.Ccurrcode 
    ENDIF 
    IF Ccurrcode <> oAriaApplication.BaseCurrency
      =gfOpenFile(oAriaApplication.SysPath + 'SYCCURR','CCURRCODE','SH')
      SELECT (lcTempHead)
      IF NOT SEEK(Ccurrcode,'SYCCURR')
        llValidCurr = .F.
        lfFillLog("Invalid Currency Code "+Ccurrcode,&lcTempHead..WOrder,.T.)
      ENDIF 
      lnUnit = 0
      lnNexrate   = 0
      lnNexrate  = gfChkRate('lnUnit' , Ccurrcode , entered  , .F.) 
      SELECT (lcTempHead)
      IF lnNexrate = 0
        llValidRate = .F.
        lfFillLog("Invalid Currency Exchange Rate",&lcTempHead..WOrder,.T.)
      ENDIF 
    ENDIF 
    *:MMT,11/27/2007 Fix bug reported by customer[End]
    
    
    =SEEK(&lcTempHead..WOrder,lcTempLine)
    SELECT (lcTempLine)
    SCAN REST WHILE WORDER+Style = &lcTempHead..WOrder
    
      *:MMT,11/27/2007 Fix bug reported by customer[Start]
      *llValidStyle =  gfSeek(Style,'Style','Style')
      llValidStyle = llValidStyle AND gfSeek(Style,'Style','Style')
      *:MMT,11/27/2007 Fix bug reported by customer[End]
      
      IF !llValidStyle 
        lfFillLog("Invalid Style No."+Style,&lcTempHead..WOrder,.F.)
      ENDIF 
      
      IF SclCnt < 1 OR SclCnt > 8
        lfFillLog("Size Bucket "+STR(SclCnt,1)+" is Invalid ",&lcTempHead..WOrder,.F.)
        llValidCnt = .F.
      ENDIF 
      
      *FOR lnCnt = 1 TO 8
      IF BETWEEN(&lcTempLine..SclCnt,1,8)
        lcCnt = STR(&lcTempLine..SclCnt,1)
        IF Qty&lcCnt. < 1
          lfFillLog("Quantity "+lcCnt+" is less than 1",&lcTempHead..WOrder,.F.)
          llvalidQty = .F.
        ENDIF 
      ENDIF   
      *ENDFOR 
      IF price < 0.01
        lfFillLog("Price is less than 0.01",&lcTempHead..WOrder,.F.)
        llValidPrice = .F.
      ENDIF 
    ENDSCAN 
    *Record Has Invalid Information
    
    *:MMT,11/27/2007 Fix bug reported by customer[Start]
    *IF !llValidQty OR !llValidStyle OR !llValidAcc OR  !llValidStore OR !llValidCnt  OR !llValidPrice
    IF !llValidQty OR !llValidStyle OR !llValidAcc OR  !llValidStore OR !llValidCnt  OR !llValidPrice OR !llValidCurr OR !llValidRate 
    *:MMT,11/27/2007 Fix bug reported by customer[End]
    
      SELECT (lcTempHead)
      DELETE 
      LOOP 
    ELSE
      *Record is Valid
      STORE '' TO m.cclass,M.Ccurrcode ,m.Cdivision,m.Cordtype,m.ctermcode ,m.CustPO ,;
                 m.Order,m.CwareCode,m.GL_Sales,m.LINK_Code, m.Multi,m.Note1,;
                 m.priority,m.Season,m.shipvia,m.spcinst ,m.Status ,m.StName ,lcOrder
                 
                             
      *:MMT,11/27/2007 Fix bug reported by customer[Start]
      *store .F. TO m.alt_shpto ,m.lhasnotes
      store .F. TO m.alt_shpto ,m.lhasnotes,m.MultiPO,m.lfromweb
      STORE '' TO m.Bulk,m.creorder,m.Rep1,m.Rep2,m.Buyer,m.Phone
      STORE 0 TO m.comm1,m.comm2,m.Nexrate,m.Disc,m.Appramt
      *:MMT,11/27/2007 Fix bug reported by customer[End]
      
      STORE 0 TO  m.Book,m.BookAmt,lnLastLNo,m.lastline,m.nExRate,m.NcurrUnit,m.OPenAmt,m.OPen,;
                  m.TotAmnt 
                  
      SELECT (lcTempHead)
      SCATTER MEMO MEMVAR 
      M.Ccurrcode = IIF(EMPTY(m.Ccurrcode),Customer.ccurrcode ,m.Ccurrcode)
          
      *:MMT,11/27/2007 Fix bug reported by customer[Start]
      IF M.Ccurrcode <> oAriaApplication.BaseCurrency
        lnUnit = 0
        m.Nexrate  = gfChkRate('lnUnit' , M.Ccurrcode , m.entered  , .F.) 
        m.NcurrUnit = lnUnit
      ELSE
        m.NcurrUnit = 1
        m.Nexrate  = 1
      ENDIF 
      
      m.lfromweb = .T.
      m.Rep1 = Customer.SalesRep
      m.comm1 = Customer.Comm
      m.comm2 = Customer.Comm2
      m.Rep2 = Customer.Rep2
      *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [start]
      *m.Disc = 0
      m.Disc = 0.00
      *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [end]
      m.Buyer = customer.buyer
      m.Phone = Customer.Phone1
      m.Appramt = 0
      *:MMT,11/27/2007 Fix bug reported by customer[End]
      
      
      =SEEK(&lcTempHead..WOrder,lcTempLine)
      =gfSeek(&lcTempLine..Style,'Style','Style')
      
      m.Cdivision = style.cdivision 
      m.Season = style.Season 
      
      SELECT(lcTempLine)
      COUNT REST WHILE WORDER+Style = &lcTempHead..WORDER TO lnLastLNo 
      
      =SEEK(&lcTempHead..WOrder,lcTempLine)
      
      SELECT(lcTempLine)
      SUM Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,;
         (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)* Price ;
          REST WHILE WORDER+Style = &lcTempHead..WORDER TO M.qty1,M.qty2,;
          m.Qty3,m.Qty4 ,m.Qty5 ,m.Qty6 ,m.Qty7 ,m.Qty8 ,m.TotAmnt 
      
      SELECT (lcTempHead)      
      
      *:MMT,11/27/2007 Fix bug reported by customer[Start]
      m.MultiPO  = .F.
      m.Bulk = 'N'
      m.creorder = 'N'
      *:MMT,11/27/2007 Fix bug reported by customer[End]
      
      m.Cordtype = 'T'
      
      m.CustPO = m.Worder
      m.Order = gfSequence('ORDER','','',m.cDivision)
      m.CwareCode = WAREHOUS.CwareCode
      = gfSeek('M'+m.Account,'CUSTOMER','CUSTOMER')
      m.ctermcode = Customer.ctermcode 
      m.GL_Sales = Customer.cslsgllink
      m.LINK_Code = Customer.LINK_Code
      m.Multi = 'N'
      *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [start]
      *m.Note1 = "Web Order"
      m.Note1 = "Web Order No = " + &lcTempHead..Order 
      *C201184 AHS 08/06/2009 Amendments to Import Sales Orders from Web [end]
      m.Priority = Customer.priority 
      m.shipvia = Customer.shipvia 
      m.spcinst = Customer.spcinst 
      m.Status = 'B'
     
      *:MMT,11/27/2007 Fix bug reported by customer[Start]
      =gfSeek(IIF(!EMPTY(m.Store),"S","M")+m.Account+IIF(!EMPTY(m.Store),m.Store,""),'CUSTOMER','CUSTOMER')
      m.StName = IIF(!EMPTY(m.Store),Customer.StName,"")
      *:MMT,11/27/2007 Fix bug reported by customer[End]
      
      IF !EMPTY(m.Caddress1)
        m.alt_shpto = .T.
      ENDIF 
      m.lhasnotes = IIF(!EMPTY(mNotes),.T.,.F.)
      m.Book = m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
      m.BookAmt = m.TotAmnt 
      m.lastline  = lnLastLNo 
      
      *:MMT,11/27/2007 Fix bug reported by customer[Start]
      *m.NcurrUnit = 1 
      *m.nExRate  = 1
      m.Flag = 'N'
      *:MMT,11/27/2007 Fix bug reported by customer[End]
      
      m.OPen = m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
      m.OPenAmt =  m.TotAmnt 
      SELECT ORDHDR
      gfAppend('ORDHDR',.T.)
      
      *:MMT,11/27/2007 Fix bug reported by customer[Start]
      =gfAdd_Info('ORDHDR')               
      *:MMT,11/27/2007 Fix bug reported by customer[End]
      
      *NotePad
      IF !EMPTY(m.Mnotes)
        m.Type = 'B'
        m.Key = m.Order 
        m.Cdesc = "Notes For Order Number : "+ m.Order 
        SELECT NOTEPAD
        gfAppend('NOTEPAD',.T.)  
        
        *:MMT,11/27/2007 Fix bug reported by customer[Start]
        =gfAdd_Info('NOTEPAD')               
        *:MMT,11/27/2007 Fix bug reported by customer[End]

      ENDIF 

      
      lcOrder = m.Order
      *Inserting Lines in Ordline File
      SELECT(lcTempLine)
      =SEEK(&lcTempHead..WOrder,lcTempLine)
      lnLine = 1
      SCAN REST WHILE WORDER+Style = &lcTempHead..WORDER 
        STORE 0 TO m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8
        
        *:MMT,11/27/2007 Fix bug reported by customer[Start]
        STORE 0 TO m.comm1,m.comm2
        *:MMT,11/27/2007 Fix bug reported by customer[end]
        
        SCATTER MEMO MEMVAR 
        m.Account = &lcTempHead..Account
        m.Order   = lcOrder         
        m.CwareCode = WAREHOUS.CwareCode
        m.store  = &lcTempHead..store
        m.Cordtype = 'T'
        m.CustPo = m.Worder
        =gfSeek(&lcTempLine..Style,'Style','Style')
        m.Gros_price = M.Price
        m.Cost = Style.TOTCOST
        m.Desc1 = Style.Desc1
        m.Flag = 'N'
        m.Gl_Cost = Style.Link_Code
        m.GL_Sales = Customer.cslsgllink + Style.CSLSGLLINK
        m.Scale = Style.Scale
        m.Season = Style.Season
        m.Complete = &lcTempHead..Complete
        m.Start =  &lcTempHead..Start
        m.Book1 = m.Qty1 
        m.Book2 = m.Qty2
        m.Book3 = m.Qty3
        m.Book4 = m.Qty4
        m.Book5 = m.Qty5
        m.Book6 = m.Qty6 
        m.Book7 = m.Qty7
        m.Book8 = m.Qty8
        m.totbook =  m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
        m.totQty =  m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
        m.lineno = lnLine 
        lnLine = lnLine + 1
        SELECT ORDLINE
        gfAppend('ORDLINE',.T.)  
        
        *:MMT,11/27/2007 Fix bug reported by customer[Start]
        =gfAdd_Info('ORDLINE')               
        *:MMT,11/27/2007 Fix bug reported by customer[End]
        
      ENDSCAN
    ENDIF 
  ENDSCAN 
  SELECT Ordhdr
  gfTableUpdate()
  SELECT OrdLINE
  gfTableUpdate()
  SELECT NOTEPAD
  gfTableUpdate()
  =gfCloseTable('ORDLINE')
  =gfCloseTable('ORDHDR')
  =gfCloseTable('STYLE')
  =gfCloseTable('NOTEPAD')
  =gfCloseTable('CUSTOMER')  
  =gfCloseTable('WAREHOUS')  
ENDIF 



lcSetDelet = SET("Deleted")
SET DELETED OFF 
SELECT (lcTempHead)
lcImpDirPath = ADDBS(lcImpDirPath)
lcHistDirPath = ADDBS(lcHistDirPath)
lnCount = 0
SCAN FOR !DELETED()
  IF FILE(lcImpDirPath + ALLTRIM(CfileName))
    COPY FILE (lcImpDirPath + ALLTRIM(CfileName)) TO  (lcHistDirPath + ALLTRIM(CfileName))
    ERASE (lcImpDirPath + ALLTRIM(CfileName)) 
  ENDIF 
  lnCount = lnCount + 1
ENDSCAN 
SET DELETED &lcSetDelet.

IF !EMPTY(lcLogFile) AND USED(lcLogFile)
  SELECT(lcLogFile)
  LOCATE 
  IF !EOF(lcLogFile)
    lfPrint()
  ELSE
    IF lnCount > 0
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,ALLTRIM(STR(lnCount,3))+" Order(s) Imported")
      RETURN   
    ENDIF    
  ENDIF
ENDIF

*!*****************************************************************************************
*! Name      : lfCreatTemp
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 11/19/2007
*! Purpose   : Create Temp File to Collect data in it
*!*****************************************************************************************
FUNCTION lfCreatTemp

DIMENSION laTempStru[17,4]

laTempStru[1,1] = 'Account' 
laTempStru[1,2] = "C"
laTempStru[1,3] = 5
laTempStru[1,4] = 0


laTempStru[15,1] = 'CfileName' 
laTempStru[15,2] = "C"
laTempStru[15,3] = 50
laTempStru[15,4] = 0


laTempStru[2,1] = 'Store' 
laTempStru[2,2] = "C"
laTempStru[2,3] = 8
laTempStru[2,4] = 0

laTempStru[3,1] = 'caddress1' 
laTempStru[3,2] = "C"
laTempStru[3,3] = 30
laTempStru[3,4] = 0

laTempStru[4,1] = 'caddress2' 
laTempStru[4,2] = "C"
laTempStru[4,3] = 30
laTempStru[4,4] = 0

laTempStru[5,1] = 'caddress3' 
laTempStru[5,2] = "C"
laTempStru[5,3] = 30
laTempStru[5,4] = 0

laTempStru[6,1] = 'caddress4' 
laTempStru[6,2] = "C"
laTempStru[6,3] = 30
laTempStru[6,4] = 0

laTempStru[7,1] = 'caddress5' 
laTempStru[7,2] = "C"
laTempStru[7,3] = 30
laTempStru[7,4] = 0

*C201184 AHS 08/06/2009 changing the field from 6 to 12 [start]
*!*	laTempStru[8,1] = 'WORDER' 
*!*	laTempStru[8,2] = "C"
*!*	laTempStru[8,3] = 6
*!*	laTempStru[8,4] = 0

laTempStru[8,1] = 'WORDER' 
laTempStru[8,2] = "C"
laTempStru[8,3] = 12
laTempStru[8,4] = 0
*C201184 AHS 08/06/2009 changing the field from 6 to 12 [end]

laTempStru[9,1] = 'entered' 
laTempStru[9,2] = "D"
laTempStru[9,3] = 8
laTempStru[9,4] = 0

laTempStru[14,1] = 'Start' 
laTempStru[14,2] = "D"
laTempStru[14,3] = 8
laTempStru[14,4] = 0


laTempStru[10,1] = 'complete' 
laTempStru[10,2] = "D"
laTempStru[10,3] = 8
laTempStru[10,4] = 0

laTempStru[11,1] = 'cordercat' 
laTempStru[11,2] = "C"
laTempStru[11,3] = 6
laTempStru[11,4] = 0

laTempStru[12,1] = 'ccurrcode' 
laTempStru[12,2] = "C"
laTempStru[12,3] = 3
laTempStru[12,4] = 0

laTempStru[13,1] = 'Mnotes' 
laTempStru[13,2] = "M"
laTempStru[13,3] = 10
laTempStru[13,4] = 0

*C201184 AHS 08/06/2009 Adding new fields [start]
laTempStru[16,1] = 'Order' 
laTempStru[16,2] = "C"
laTempStru[16,3] = 10
laTempStru[16,4] = 0

laTempStru[17,1] = 'NMERAMNT' 
laTempStru[17,2] = "N"
laTempStru[17,3] = 10
*B609003 , TMI [start] define the decimal place 
*laTempStru[17,4] = 0
laTempStru[17,4] = 2
*B609003 , TMI [end  ] define the decimal place 

*C201184 AHS 08/06/2009 Adding new fields [end]

= gfCrtTmp(lcTempHead ,@laTempStru,"WORDER" ,lcTempHead ,.T.)

DIMENSION laTempStrul[14,4]
*C201184 AHS 08/06/2009 changing the field width to 12 [start]
*!*	laTempStrul[1,1] = 'WORDER' 
*!*	laTempStrul[1,2] = "C"
*!*	laTempStrul[1,3] = 6
*!*	laTempStrul[1,4] = 0

laTempStrul[1,1] = 'WORDER' 
laTempStrul[1,2] = "C"
laTempStrul[1,3] = 12
laTempStrul[1,4] = 0
*C201184 AHS 08/06/2009 changing the field width to 12 [end]

laTempStrul[2,1] = 'Style' 
laTempStrul[2,2] = "C"
laTempStrul[2,3] = 19
laTempStrul[2,4] = 0

laTempStrul[3,1] = 'SclCnt' 
laTempStrul[3,2] = "N"
laTempStrul[3,3] = 1
laTempStrul[3,4] = 0


laTempStrul[4,1] = 'Qty1' 
laTempStrul[4,2] = "N"
laTempStrul[4,3] = 6
laTempStrul[4,4] = 0

laTempStrul[5,1] = 'Qty2' 
laTempStrul[5,2] = "N"
laTempStrul[5,3] = 6
laTempStrul[5,4] = 0

laTempStrul[6,1] = 'Qty3' 
laTempStrul[6,2] = "N"
laTempStrul[6,3] = 6
laTempStrul[6,4] = 0

laTempStrul[7,1] = 'Qty4' 
laTempStrul[7,2] = "N"
laTempStrul[7,3] = 6
laTempStrul[7,4] = 0

laTempStrul[8,1] = 'Qty5' 
laTempStrul[8,2] = "N"
laTempStrul[8,3] = 6
laTempStrul[8,4] = 0

laTempStrul[9,1] = 'Qty6' 
laTempStrul[9,2] = "N"
laTempStrul[9,3] = 6
laTempStrul[9,4] = 0

laTempStrul[10,1] = 'Qty7' 
laTempStrul[10,2] = "N"
laTempStrul[10,3] = 6
laTempStrul[10,4] = 0

laTempStrul[11,1] = 'Qty8' 
laTempStrul[11,2] = "N"
laTempStrul[11,3] = 6
laTempStrul[11,4] = 0

laTempStrul[12,1] = 'Price' 
laTempStrul[12,2] = "N"
laTempStrul[12,3] = 12
laTempStrul[12,4] = 2

*C201184 AHS 08/06/2009 Adding new fields [start]
laTempStrul[13,1] = 'CWBPRODREF' 
laTempStrul[13,2] = "C"
laTempStrul[13,3] = 30
laTempStrul[13,4] = 0

laTempStrul[14,1] = 'CPERSON' 
laTempStrul[14,2] = "C"
laTempStrul[14,3] = 5
laTempStrul[14,4] = 0
*C201184 AHS 08/06/2009 Adding new fields [end]

= gfCrtTmp(lcTempLine  ,@laTempStrul,"WORDER+Style" ,lcTempLine ,.T.)

DIMENSION laLogStr[9,4]
*C201184 AHS 08/06/2009 changing the field width to 12 [start]
*!*	laLogStr[1,1] = "Worder"
*!*	laLogStr[1,2] = "C"
*!*	laLogStr[1,3] = 6 
*!*	laLogStr[1,4] = 0

laLogStr[1,1] = "Worder"
laLogStr[1,2] = "C"
laLogStr[1,3] = 12
laLogStr[1,4] = 0
*C201184 AHS 08/06/2009 changing the field width to 12 [end]

laLogStr[2,1] = "Entered"
laLogStr[2,2] = "D"
laLogStr[2,3] = 8
laLogStr[2,4] = 0

laLogStr[3,1] = "Account"
laLogStr[3,2] = "C"
laLogStr[3,3] = 5
laLogStr[3,4] = 0

laLogStr[4,1] = "Store"
laLogStr[4,2] = "C"
laLogStr[4,3] = 8
laLogStr[4,4] = 0

laLogStr[5,1] = "Style"
laLogStr[5,2] = "C"
laLogStr[5,3] = 19
laLogStr[5,4] = 0

laLogStr[6,1] = "szcnt"
laLogStr[6,2] = "N"
laLogStr[6,3] = 1
laLogStr[6,4] = 0

laLogStr[7,1] = "TOTQty"
laLogStr[7,2] = "N"
laLogStr[7,3] = 10
laLogStr[7,4] = 0

laLogStr[8,1] = "PRICE"
laLogStr[8,2] = "N"
laLogStr[8,3] = 7
laLogStr[8,4] = 2

laLogStr[9,1] = "DESC"
laLogStr[9,2] = "M"
laLogStr[9,3] = 10
laLogStr[9,4] = 0

= gfCrtTmp(lcLogFile,@laLogStr,"WORDER" ,lcLogFile,.F.)



*!*************************************************************
*! Name      : lfPrint
*! Developer : MAriam Mazhar[MMT]
*! Date      : 11/20/2007
*! Purpose   : To print the report
*!*************************************************************
*! Parameters: cFileToCheckFor 
*!*************************************************************
FUNCTION lfPrint

lcDefa = FULLPATH('')
PRIVATE loOGScroll

*-- Create a Dummy Object of the Option Grid...
lnDataSess = SET("Datasession")
lcClassDir   = ADDBS(oAriaApplication.ClassDir)
oOptionGrid  = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
loOGScroll   = oOptionGrid.OptionGrid.oHost
lcOGPlatForm = ''
loOgScroll.lcOGPlatForm  = ''
loOgScroll.lcOGLastForm  = 'SOIMPLOG'
loOGScroll.llPrintPDF = .F.
LoOGScroll.llCrystal = .T.
loOGScroll.cCROrientation='L'
loOGScroll.lUsePDFViewer  = .F.
loogscroll.lcogwintitl = 'Web Orders Import Error Log'

DIMENSION loOGScroll.laSelFile[1,3]
loOGScroll.laSelFile = ''

SELECT (lcLogFile)
LOCATE 
USE IN (lcLogFile)

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[2,2]

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcLogFile+ ".DBF"
  
loOgScroll.laCRParams[1,1] = 'dsysDate'
loOgScroll.laCRParams[1,2]= oAriaApplication.SYSTEMDATE

=gfDispRe('SOIMPLOG')

SET DATASESSION TO (lnDataSess)

oOptionGrid = .NULL.
LoOGScroll = null

SET DEFAULT TO (lcDefa)
SET DEVICE TO SCREEN
RETURN 

*!*************************************************************
*! Name      : lfcleanPrg
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 08/26/2006
*! Purpose   : To return the existance of the file
*!*************************************************************
FUNCTION lfCleanPrg

lcBaseWind='AAREBRCVCN'
oariaapplication.SETHlpfl(oariaapplication.activemoduleid)

SELECT syuStatc
IF SEEK ('WIN'+ALLTRIM(lcBaseWind)+oariaapplication.User_ID+oariaapplication.Station) 
  *** Save the name of module from which the window was activated
  SCATTER MEMVAR MEMO BLANK
  GATHER  MEMVAR MEMO
  DELETE 
  =TABLEUPDATE(.T.)
ENDIF   

*** Clear the temp record in static file
IF !USED('SYDSUPRG')
  SELECT 0 
  USE (oariaapplication.SysPath+'SYDSUPRG')
ELSE
  SELECT SYDSUPRG
ENDIF

SET ORDER TO TAG PRGCOMP

*** Check if the program was single user clear the locking
IF SEEK(SUBSTR(lcBaseWind,4)+oariaapplication.ActiveCompanyID,'SYDSUPRG')
  UNLOCK IN SYDSUPRG
ENDIF
glNoLog    = .F.

*!*****************************************************************************************
*! Name      : lfFillLog
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 11/20/2007
*! Purpose   : Fill log file
*!*****************************************************************************************
FUNCTION lfFillLog
PARAMETERS lcMessage,lcOrderNum,llHeader
lcOldAlias = SELECT()
SELECT(lcLogFile)
APPEND BLANK 
IF llHeader
  REPLACE Worder WITH lcOrderNum,;
          Account WITH &lcTempHead..Account,;
          Store   With &lcTempHead..Store,;
          Entered With &lcTempHead..Entered 
ELSE
  lcScnt = ALLTRIM(STR(&lcTempLine..SclCnt,1))
  REPLACE Worder  WITH lcOrderNum,;
          Account WITH &lcTempHead..Account,;
          Store   With &lcTempHead..Store,;
          Entered With &lcTempHead..Entered ,;
          style   WITH &lcTempLine..Style,;
          szcnt  WITH &lcTempLine..SclCnt,;
          totqty WITH IIF(BETWEEN(&lcTempLine..SclCnt,1,8), &lcTempLine..QTY&lcScnt.,0) ,;
          PRICE  with &lcTempLine..PRICE
ENDIF   
REPLACE Desc WITH lcMessage
SELECT(lcOldAlias)