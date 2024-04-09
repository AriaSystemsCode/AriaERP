*:****************************************************************
*: Program file  : SOIMPRT.PRG
*: Program desc. : Import Sales orders from Excel files
*: System        : Aria Apparel System - Version 4XP
*: Module        : Sales Order(SO)
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 09/11/2006. (C200678)
***************************************************
*:* Modifications:
*!* C202001,1 AHH 11/04/2017 changing warehouse location to JWE INC [T20170411.0002].
*!* B611315,1 MMT 05/16/2017 Issue#18 - JWE Import SO program does not import zip code of order[P20140306.0001]
*!* B611318,1 AHH 05/21/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001]
*!* B611328,1 AHH 01/06/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001]
*!* B611785,1 Es 06/12/2019 When import Sales Order from excel file ,No lines added in Orderline. [T20190531.0003]
*:****************************************************************
DO FORM (oAriaApplication.ScreenHome+ oAriaApplication.ActiveModuleID+ '\SOIMPRT.SCX')

*!*************************************************************
*! Name      : lfvgetFile
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 09/11/2006.
*! Purpose   : Function to get the Excel file Path
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfvgetFile()
*!*************************************************************
*!B608550,1 5/11/2008, AKA Fix error while importing Sales order from excel. The scren impacted with the fix too
*!*************************************************************
*!B611271,1 26/02/2017, AH, Fix error Incorrect SO warehouse and shipping address.. The scren impacted with the fix too [T20170208.0075]
*!*************************************************************
FUNCTION lfvgetFile
PRIVATE lcOld_Path

lcOld_Path = FULLPATH('')

lcPathName  = GETFILE('XLS', 'Excel sheet Path : ','Select')

SET DEFAULT TO &lcOld_Path

IF EMPTY(lcPathName)
  RETURN ''
ELSE
  RETURN lcPathName
ENDIF

*-- End Of lfvgetFile

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Saeed Mohammed (SMM)
*! Date      : 12/13/2004
*! Purpose   : To import the excel file
*!*************************************************************
*! Example            :  lfvProceed()
*!*************************************************************
FUNCTION lfvProceed
PARAMETERS lcPathName,LOFORMSET

IF LEN(ALLTRIM(lcPathName)) = 1
  lcPathName = ALLTRIM(lcPathName)
ENDIF
IF EMPTY(lcPathName)
  *Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  RETURN
ENDIF

*-- Check if File Is .XLS file.
IF !(".XLS" $ lcPathName)
  = gfModalGen('INM00389B00000','F','ALERT')
  RETURN
ENDIF

IF !FILE(lcPathName)
  * "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  RETURN
ENDIF

*-- Creating a cursor that will hold the imported data from the
*-- excel file in order to append it in the master database file.
PRIVATE lcLogFile , lError, lOrdErr
lcLogFile = gfTempName()
lError = .F.

DIMENSION laFileStru[2,4]

laFileStru[1,1] = 'nRecNo'
laFileStru[1,2] = 'N'
laFileStru[1,3] = 5
laFileStru[1,4] = 0

laFileStru[2,1] = 'cError'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 200
laFileStru[2,4] = 0

=gfCrtTmp(lcLogFile,@laFileStru,'NRECNO','LogRecno',.F.)


INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'--------------------------------------------------------------------')
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'Data imported from file '+lcPathName+' at Date:'+DTOC(DATE())+' Time:'+TIME())
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'--------------------------------------------------------------------')

PRIVATE lcErrStr, llImpErr, lcDefDir, lcXLS,lcAlias

*--Open Needed files
=lfOpenFls()
*--Create needed temp files

*!B611271,1 26/02/2017, AH, Fix error Incorrect SO warehouse and shipping address.. The scren impacted with the fix too [Begin]
lcldefware = ""
SELECT cDesc,cWareCode FROM WAREHOUS WHERE WAREHOUS.ldefware=.T. INTO ARRAY laWareHouses1
IF ALEN(laWareHouses1,1) >0 AND !(ALLTRIM(laWareHouses1[1,2] )== "")
  lcldefware = laWareHouses1[1,2]
ENDIF
*!B611271,1 26/02/2017, AH, Fix error Incorrect SO warehouse and shipping address.. The scren impacted with the fix too [end]



PRIVATE lcOrdHdr, lcOrdLine,lcT_BomVar

*--lnOrdCount : to hold the numbers of orders that have been generated
*--laOrders   : array to hold the orders numbers.
*--lcNotes    : hold the order's notes.
*--llLoop     : to skip the customer PO order if imported before or it has a style not found in the style file
PRIVATE lcCustPO,lcStyle,lnOrdCount , lcNotes , llLoop , lcOldOrder , lnOldAlias
DIMENSION laOrders[1]
STORE '' TO laOrders , lcNotes , lcOldOrder
STORE 0 TO lnOrdCount , lnBooked , lnBookAmt , lnOpened , lnOpenAmt , lnOldAlias
STORE .F. TO llLoop

lcCustPO = ''
lcStyle  = ''
lcDefWare = ''

lcOrdHdr   = gfTempName()
lcOrdLine  = gfTempName()
lcT_BomVar = gfTempName()
lcXLS = 'SHEET'
IF !USED(lcXLS)
  SELECT 0
  WAIT WINDOW NOWAIT 'Opening Excel file....'
  lcErrStr = ON('ERROR')
  llImpErr = .F.
  ON ERROR llImpErr = .T.
  lcDefDir = FULLPATH('')               && Save current default dir
  SET DEFAULT TO (oAriaApplication.WorkDir)

  *!* C202001,1 AHH 11/04/2017 changing warehouse location to JWE INC. [Begin][T20170411.0002]
  lcPathNameNew = ADDBS(oAriaApplication.WorkDir) + JUSTFNAME(lcPathName )
  COPY FILE  (lcPathName) TO (lcPathNameNew)
  lcPathName = lcPathNameNew


  *!* C202001,1 AHH 11/04/2017 changing warehouse location to JWE INC. [End][T20170411.0002]

  IMPORT FROM (lcPathName) TYPE XLS
  lcXLS = DBF()
  lcAlias = ALIAS()
  USE (lcXLS) IN 0 AGAIN ALIAS 'SHEET'
  SELECT (lcAlias)
  USE
  lcXLS = 'SHEET'
  SELECT (lcXLS)
  SET DEFAULT TO &lcDefDir
  ON ERROR &lcErrStr
  WAIT CLEAR
  IF llImpErr
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Excel file format!')
    =lfCloseFls()
    RETURN
  ENDIF
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is another file with the same name of '+;
    'the selected Excel file , Please rename.')
  =lfCloseFls()
  RETURN
ENDIF
SELECT (lcXLS)
GO TOP
DELETE && Delete the first row which hold the Fields headings
DELETE ALL FOR EMPTY(A) && Delete all empty records

DELETE ALL FOR IIF(TYPE('&lcXLS..V')='C',VAL(ALLTRIM(&lcXLS..V)),&lcXLS..V) + ;
  IIF(TYPE('&lcXLS..X')='C',VAL(ALLTRIM(&lcXLS..X)),&lcXLS..X) = 0
PACK

SET DELETED ON

*--Index on Order_ID + Item_Style + Size.
INDEX ON ALLTRIM(N)+ALLTRIM(T)+ALLTRIM(R) TAG CUSTPO
*--in Column B,C or D if the Code Wrote as (003), this Code importes in the Fox as (3) so I Give this Code the two(00) thas cut form it
SCAN
  IF LEN(ALLTRIM(B))<3
    REPLACE B WITH PADL(ALLTRIM(B),3,'000')
  ENDIF

  IF LEN(ALLTRIM(C))<3
    REPLACE C WITH PADL(ALLTRIM(C),3,'000')
  ENDIF

  IF LEN(ALLTRIM(D))<3
    REPLACE D WITH PADL(ALLTRIM(D),3,'000')
  ENDIF
ENDSCAN

LOCATE
PRIVATE lcStore,lcScFields,lcStyClr,lcSz,lcAcc,lcCustPO,lnPrice,lcPriceLvl,lcPrice
*EXTERNAL ARRAY laData


=lfCratTemp()


DO WHILE !EOF()
  lOrdErr   = .F.
  lcCustPO  = ALLTRIM(N)
  lcAcc     = ALLTRIM(A)
  lcStyle   = ALLTRIM(T)
  SELECT (lcOrdHdr)
  IF SEEK(PADR(lcAcc,5)+PADR(lcCustPO,15)+'O')  && Check the existence of the Cust PO

    IF gfModalGen('QRM00000B32018',.F.,ALLTRIM(lcCustPO),.F.,'Customer PO ð has been already entered. Do you want'+;
        ' to overwrite it or ignore?') = 2
      INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' + ALLTRIM(STR(RECNO(lcXLS))) + ', Customer PO '+ALLTRIM(lcCustPO) +;
        ' has been already entered.')
      lError = .T.
      SELECT (lcXLS)
      SKIP
      LOOP
    ELSE

      *--Insert the Recno into the cursor to sort on it
      INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
        ALLTRIM(STR(RECNO(lcXLS))) + ', Customer PO '+ALLTRIM(lcCustPO) +;
        ' has been already entered, and the user chose to overwrite.')
      lError = .T.
    ENDIF
  ENDIF


  SELECT (lcOrdHdr)

  SCATTER MEMVAR MEMO BLANK
  SELECT (lcXLS)
  IF !lfValRec() && Validate Record
    lError = .T.
    lOrdErr = .T.
    SKIP
    LOOP
  ENDIF
  lcPriceLvl = ALLTRIM(m.Account)
  SELECT(lcXLS)
  m.ORDER     = ''
  m.Account   = UPPER(A)
  m.CUSTPO = ALLTRIM(N)
  m.Rep1   = RIGHT(ALLTRIM(&lcXLS..B),3)
  m.NCURRUNIT = 1
  m.NEXRATE   = 1
  m.CCURRCODE = 'USD'
  m.CORDTYPE  = 'O'
  m.STATUS    = 'O'
  *--Make Stname hold only Name1
  m.Stname    = ALLTRIM(E)
  m.Start     = IIF(TYPE('O')<>'D',CTOD(ALLTRIM(O)),O)
  m.Complete  = IIF(TYPE('O')<>'D',CTOD(ALLTRIM(O)),O)
  m.Entered   = DATE()
  m.Flag      = 'N'
  m.Bulk      = 'N'
  m.DirectInv = .F.
  m.LContract = .F.
  m.CREORDER  = 'N'
  m.MULTI     = 'N'
  m.Cinsur    = 'Y'
  m.LERRORS   = .F.         && Define that this order is error free
  m.caddress6= ' '
  SELECT (lcXLS)
  *--STORE = 'T' + Terrirtory c(3) + 'R' + Region C(3)
  lcStore = 'T'+RIGHT(ALLTRIM(&lcXLS..C),3)+'R'+RIGHT(ALLTRIM(&lcXLS..D),3)
  IF !gfSEEK('S'+PADR(ALLTRIM(&lcXLS..A),5)+lcStore,'CUSTOMER')
    INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
      ALLTRIM(STR(RECNO(lcXLS))) + ', Store ' + ALLTRIM(lcStore) +' is not found')
    llLoop = .T.
    =gfModalGen("INM00000B00000","DIALOG",'','',"Store ( " + lcStore + " ) not found for Customer ( "+;
      ALLTRIM(&lcXLS..A)+ " ). order " + lcCustPO + " will be Skipped")

    lError = .T.
    lOrdErr = .T.
    SKIP
    LOOP
  ELSE
    IF CUSTOMER.STATUS <> 'A'
      INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
        ALLTRIM(STR(RECNO(lcXLS))) + ', Customer '+ ALLTRIM(CUSTOMER.Account) +' is not active ')
      lError = .T.
      lOrdErr = .T.
      SELECT (lcXLS)
      SKIP
      LOOP
    ENDIF
    m.Buyer     = CUSTOMER.Buyer
    m.Phone     = CUSTOMER.Phone1
    m.Disc      = CUSTOMER.Disc
    m.Rep1      = CUSTOMER.SalesRep
    m.Comm1     = CUSTOMER.COMM
    m.REP2      = CUSTOMER.REP2
    m.Comm2     = CUSTOMER.Comm2
    m.Priority  = CUSTOMER.Priority
    m.Link_Code = IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code) && ?????
    m.GL_Sales  = IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)  && ?????
    m.Store     = lcStore
    =lfAddress()
    IF m.cAddress1 <> CUSTOMER.cAddress1 OR m.cAddress2 <> CUSTOMER.cAddress2 OR m.cAddress3 <> CUSTOMER.cAddress3 OR ;
        M.cAddress4 <> CUSTOMER.cAddress4 OR m.cAddress5 <> CUSTOMER.cAddress5 OR m.caddress6 <> CUSTOMER.caddress6
      m.Alt_SHPTO = .T.
    ENDIF
  ENDIF
  lnLINENO = 0
  m.cDivision = ''
  SELECT(lcXLS)

  SCAN REST WHILE ALLTRIM(N)+ALLTRIM(T)+ALLTRIM(R) = lcCustPO && All records with cust PO
    *--If the customer PO has imported before then skip this order.
    lcOldOrder = ORDER('ORDHDR')
    lnOldAlias = SELECT(0)
    SELECT ORDHDR
    gfSetOrder('ORDCUST')
    IF gfSEEK(ALLTRIM(m.Account) + lcCustPO ,'ORDHDR')
      SELECT ORDHDR
      SCAN REST WHILE Account+UPPER(CUSTPO)+CORDTYPE+ORDER = ALLTRIM(m.Account) + lcCustPO FOR STATUS<>'X'
        =gfModalGen("INM00000B00000","DIALOG",'','',"Customer order# " + lcCustPO + " has imported before . "+;
          "this order will be Skipped")
        INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
          ALLTRIM(STR(RECNO(lcXLS))) + ', Customer Order# '+ALLTRIM(lcCustPO) +' has been imported before.')
        llLoop = .T.
        EXIT
      ENDSCAN
      gfSetOrder(lcOldOrder)
      *SET ORDER TO lcOldOrder IN ORDHDR
      IF llLoop
        EXIT
      ENDIF
      SELECT(lnOldAlias)
    ENDIF
    SELECT CUSTOMER
    LOCATE FOR Account=PADR(ALLTRIM(&lcXLS..A),5) AND SalesRep=RIGHT(ALLTRIM(&lcXLS..B),3) ;
      AND STORE = lcStore
    IF !FOUND()
      =gfModalGen("INM00000B00000","DIALOG",'','',"Sales Rep. ( " + RIGHT(ALLTRIM(&lcXLS..B),3) + " ) not found for Customer/Store ( "+;
        ALLTRIM(&lcXLS..A)+ " / " + lcStore + " ). order " + lcCustPO + " will be Skipped")
      INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
        ALLTRIM(STR(RECNO(lcXLS))) + ', Sales Rep ' + RIGHT(ALLTRIM(&lcXLS..B),3) +' is not found')
      llLoop = .T.
      EXIT
    ENDIF
    SELECT(lnOldAlias)
    *--If the current style not found in the style file then skip this order at all.
    IF !gfSEEK(UPPER(ALLTRIM(T)),[STYLE]) && Check for style
      =gfModalGen("INM00000B00000","DIALOG",'','',"Style : " + ALLTRIM(T) +" in Customer PO# " +;
        ALLTRIM(N) + " Not found in Aria's Style File . this order will be Skipped")
      INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' + ;
        ALLTRIM(STR(RECNO(lcXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +' is not found')
      llLoop = .T.
      EXIT
    ENDIF

    SELECT (lcXLS)

    =gfSEEK(UPPER(ALLTRIM(T)),[STYLE])
    lcStore = 'T'+RIGHT(ALLTRIM(&lcXLS..C),3)+'R'+RIGHT(ALLTRIM(&lcXLS..D),3)
    IF !gfSEEK('S'+PADR(ALLTRIM(&lcXLS..A),5)+lcStore,'CUSTOMER')
      =gfModalGen("INM00000B00000","DIALOG",'','',"Store ( " + lcStore + " ) not found for Customer ( "+;
        ALLTRIM(&lcXLS..A)+ " ). order " + lcCustPO + " will be Skipped")
      INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
        ALLTRIM(STR(RECNO(lcXLS))) + ', Store ' + ALLTRIM(lcStore) +' is not found')
      llLoop = .T.
      EXIT
    ENDIF

    SELECT (lcOrdLine)
    IF !SEEK(ALLTRIM(lcCustPO)+ALLTRIM(STYLE.STYLE)+ALLTRIM(lcStore))
      INSERT INTO (lcOrdLine) (ORDER,STYLE,SCALE,SEASON,cWareCode,CORDTYPE,Account,CUSTPO,STORE) VALUES ;
        ('',STYLE.STYLE, STYLE.SCALE,STYLE.SEASON,STYLE.cDefWare,'O',m.Account,lcCustPO,lcStore)
      IF EMPTY(m.cDivision)
        m.cDivision = STYLE.cDivision
      ELSE
        IF m.cDivision <> STYLE.cDivision
          INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
            ALLTRIM(STR(RECNO(lcXLS))) + ', The syles for this order have different divisions')
          lError = .T.
          lOrdErr = .T.
          LOOP
        ENDIF
      ENDIF
      lnPrice = IIF(TYPE('&lcXLS..W')='C',VAL(ALLTRIM(&lcXLS..W)),&lcXLS..W)
      lcPrice = 'Style.Price' + lcPriceLvl
      IF lcPriceLvl <> 'Q' AND (lnPrice <> EVAL(lcPrice))
        IF gfModalGen('QRM00000B32017',.F.,ALLTRIM(STYLE.CSTYMAJOR),.F.,'There is a mismatch between the transmitted'+ ;
            ' price and the original price of Style ð. Which price would you like to add in the'+ ;
            ' Sales Order?') = 1
          INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Record Number : ' +;
            ALLTRIM(STR(RECNO(lcXLS))) + ', There is a mismatch between the imported'+ ;
            ' price and the original price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+  ;
            ', the imported price was selected for the order.')
          lError = .T.
          REPLACE PRICE WITH lnPrice
          SELECT(lcXLS)

          IF IIF(TYPE('X') = 'C',VAL(ALLTRIM(X)),X)  <> IIF(TYPE('V') = 'C',VAL(ALLTRIM(V)),V)*IIF(TYPE('W') = 'C',VAL(ALLTRIM(W)),W)
            INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
              ALLTRIM(STR(RECNO(lcXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..T) +;
              ' total amount does not match the price amount.')
            lError = .T.
            lOrdErr = .T.
            LOOP
          ENDIF
          SELECT (lcOrdLine)
        ELSE
          INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Record Number : ' +;
            ALLTRIM(STR(RECNO(lcXLS))) + ', There is a mismatch between the imported'+ ;
            ' price and the original price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+ ;
            ', the original price was selected for the order.')
          lError = .T.
          REPLACE PRICE WITH EVAL(lcPrice)
        ENDIF
      ELSE
        IF lcPriceLvl ='Q'
          INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Record Number : ' +;
            ALLTRIM(STR(RECNO(lcXLS))) + ", This customers price level is 'at qty level', "+ ;
            'the imported price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+ ' was selected for the order.')
          lError = .T.
          SELECT (lcXLS)
          IF IIF(TYPE('X') = 'C',VAL(ALLTRIM(X)),X)  <> IIF(TYPE('V') = 'C',VAL(ALLTRIM(V)),V)*IIF(TYPE('W') = 'C',VAL(ALLTRIM(W)),W)
            INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
              ALLTRIM(STR(RECNO(lcXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..T) +;
              ' total amount does not match the price amount.')

            lError = .T.
            lOrdErr = .T.
            LOOP
          ENDIF
        ENDIF
        SELECT (lcOrdLine)
        REPLACE PRICE WITH lnPrice
      ENDIF
      lnLINENO = lnLINENO + 1
      REPLACE LINENO WITH lnLINENO
      

*!* B611785,1 Es 06/12/2019 When import Sales Order from excel file ,No lines added in Orderline. [Start]
 REPLACE flag WITH 'N'
*!* B611785,1 Es 06/12/2019 When import Sales Order from excel file ,No lines added in Orderline. [End]
    ENDIF



    SELECT (lcXLS)
    IF gfSEEK(UPPER(ALLTRIM(T)),'STYLE')
      lcSz = '&lcOrdLine..QTY'+lfGetSize(ALLTRIM(R))
      IF !(lcSz == '&lcOrdLine..QTY')
        IF &lcSz > 0
          INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
            ALLTRIM(STR(RECNO(lcXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +;
            ', size '+ALLTRIM(&lcXLS..R)+' was entered before.')
          lError  = .T.
          lOrdErr = .T.
          SELECT(lcXLS)
          LOOP
        ENDIF
        REPLACE &lcSz      WITH IIF(TYPE('&lcXLS..V')='C',INT(VAL(ALLTRIM(V))),V)
        lcSz = '&lcOrdLine..BOOK' + lfGetSize(ALLTRIM(R))
        REPLACE &lcSz      WITH IIF(TYPE('&lcXLS..V')='C',INT(VAL(ALLTRIM(V))),V)
      ENDIF

      SELECT (lcOrdLine)
      REPLACE TotBook    WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8
      REPLACE TotQty     WITH Qty1  + Qty2  + Qty3  + Qty4  + Qty5  + Qty6  + Qty7  + Qty8
      REPLACE GROS_PRICE WITH EVAL(lcOrdLine+'.PRICE')
    ENDIF
    lcNotes = lcNotes +IIF(!EMPTY(lcNotes),CHR(13),'')+ ALLTRIM(&lcXLS..Y)
    m.LASTLINE  = lnLINENO - 1
    SELECT (lcXLS)
  ENDSCAN

  SELECT (lcXLS)
  IF llLoop = .T.
    DO WHILE ALLTRIM(N) = lcCustPO
      SKIP
    ENDDO
    SELECT (lcOrdLine)
    DELETE ALL FOR CUSTPO = lcCustPO
    SELECT (lcOrdHdr)
    DELETE ALL FOR CUSTPO = lcCustPO
    llLoop = .F.
  ENDIF
  SELECT (lcOrdLine)
  SCAN
    m.BOOK      = m.BOOK + TotBook
    m.BookAmt   = m.BookAmt + (PRICE * TotQty)
    m.OpenAmt   = m.BookAmt
    m.CancelAmt = 0
    m.OPEN      = m.BOOK
  ENDSCAN
  SELECT (lcXLS)

  IF (m.BOOK > 0 OR m.OPEN > 0) AND !lOrdErr

    m.CBCODE   = ALLTRIM(M)
    m.CEmail   = ALLTRIM(P)
    m.CSupTerr = ALLTRIM(B)
    PRIVATE lcStySea
    SELECT (lcOrdLine)
    GO TOP
    lcStySea = ALLTRIM(UPPER(SEASON))
    lcStore  = ALLTRIM(UPPER(STORE))
    SELECT SEASON FROM (lcOrdLine) WHERE ALLTRIM(UPPER(SEASON)) <> lcStySea INTO ARRAY laX
    IF _TALLY <> 0
      m.SEASON = '*'
    ELSE
      m.SEASON = lcStySea
    ENDIF
    SELECT DISTINCT STORE FROM (lcOrdLine) INTO ARRAY laY
    IF _TALLY > 1
      m.Store     = '*Multi*'
      m.MULTI     = 'Y'
      m.ShipVia   = '*'
      m.cAddress1 = ''
      m.cAddress2 = ''
      m.cAddress3 = ''
      m.cAddress4 = ''
      m.cAddress5 = ''
      m.caddress6 = ''
      m.Alt_SHPTO = .F.
    ENDIF
    *!B611271,1 26/02/2017, AH, Fix error Incorrect SO warehouse and shipping address.. The scren impacted with the fix too [Begin]
    m.cWareCode = lcldefware
    *!B611271,1 26/02/2017, AH, Fix error Incorrect SO warehouse and shipping address.. The scren impacted with the fix too [End]


    SELECT (lcXLS)
    =lfAddUsrDt(lcOrdHdr)
    =lfAddUsrDt(lcOrdLine)
    INSERT INTO (lcOrdHdr) FROM MEMVAR
    SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
    =lfSaveData()
  ENDIF
  SELECT (lcXLS)

  STORE 0 TO lnBooked , lnBookAmt , lnOpened , lnOpenAmt
  SET ORDER TO TAG 'ORDLN' IN (lcOrdLine)

ENDDO
IF lError
  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'There were some errors and warning, Do you want to view the log report?') = 1
    lcWinTitl  = "Log Report"
    DIMENSION laFileStru[1,4]

    laFileStru[1,1] = 'mStrRep'
    laFileStru[1,2] = 'M'
    laFileStru[1,3] = 120
    laFileStru[1,4] = 0

    =gfCrtTmp('TMPSTR',@laFileStru,.F.)

    *	CREATE CURSOR TMPSTR (mStrRep M(120))
    SELECT TMPSTR
    APPEND BLANK
    SELECT (lcLogFile)
    GO TOP
    SCAN
      SELECT TMPSTR
      REPLACE mStrRep WITH mStrRep + CHR(10) + CHR(13)+ &lcLogFile..cError
      SELECT (lcLogFile)
    ENDSCAN
    SELECT TMPSTR
    DO FORM (oAriaApplication.ScreenHome+ 'SM\SMSTRREP.SCX')
  ENDIF
ENDIF


DO CASE
CASE lnOrdCount=1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is saved as' + laOrders[1])
CASE lnOrdCount > 1
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from ' + laOrders[1] + ' to ' + laOrders[lnOrdCount]+ ' have been generated successfully')
CASE lnOrdCount = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ENDCASE


USE IN &lcXLS
*!**************************************************************************
*!* Name        : lfOpenFls
*!* Developer   : SMM - SAEED MOHAMED
*!* Date        : 12/14/2004
*!* Purpose     : Open needed files
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfOpenFls()
*!***************************************************************************
FUNCTION lfOpenFls
=gfOpenTable(oAriaApplication.DataDir+'BomVar',oAriaApplication.DataDir+'BomVar','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
=gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
=gfOpenTable(oAriaApplication.DataDir+'WAREHOUS','WAREHOUS','SH')
=gfOpenTable(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
=gfOpenTable(oAriaApplication.DataDir+'SALESREP','SALESREP','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR','ORDHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDLINE','ORDLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'UnCmSess',oAriaApplication.DataDir+'TRANS','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
=gfOpenTable(oAriaApplication.DataDir+'NOTEPAD','NOTEPAD','SH')

=gfOpenTable(oAriaApplication.SysPath+'SycCurr','cCurrCode','SH')
=gfOpenTable(oAriaApplication.SysPath+'SYCEXCH','CURRENCY','SH')
=gfOpenTable(oAriaApplication.SysPath+'SYCINT','CCONTCODE','SH')


SET ORDER TO TAG CURRENCY DESC IN 'SYCEXCH'
*-- end of lfOpenFls.

*!**************************************************************************
*!* Name        : lfCloseFls
*!* Developer   : Saeed Mohammed (SMM)
*!* Date        : 12/13/2004
*!* Purpose     : Close temp files
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  =lfCloseFls()
*!***************************************************************************
FUNCTION lfCloseFls
IF USED(lcOrdHdr)
  USE IN (lcOrdHdr)
ENDIF
ERASE (oAriaApplication.WorkDir+lcOrdHdr+'.DBF')
ERASE (oAriaApplication.WorkDir+lcOrdHdr+'.CDX')

IF USED(lcOrdLine)
  USE IN (lcOrdLine)
ENDIF
ERASE (oAriaApplication.WorkDir+lcOrdLine+'.DBF')
ERASE (oAriaApplication.WorkDir+lcOrdLine+'.CDX')

IF USED(lcXLS)
  USE IN (lcXLS)
ENDIF
ERASE (oAriaApplication.WorkDir+lcXLS+'.DBF')

*!*************************************************************
*!* Name      : lfCratTemp
*!* Developer : Saeed Mohammed (SMM)
*!* Date      : 12/13/2004
*!* Purpose   : Create temp files
*!*************************************************************
*!* Calls     : gfCrtTmp
*!*************************************************************
*!* Passed Parameters  :  None
*!*************************************************************
*!* Returns            :  None
*!*************************************************************
*!* Example            :  =lfCratTemp()
*!*************************************************************
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex,lnAlias
lnAlias = SELECT()
SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'LERRORS'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

FOR lnLoop = 1 TO  2
  STORE ' ' TO  laFileStru[lnFileStru +lnLoop,7],laFileStru[lnFileStru +lnLoop,8],;
    laFileStru[lnFileStru +lnLoop,9],laFileStru[lnFileStru +lnLoop,10],;
    laFileStru[lnFileStru +lnLoop,11],laFileStru[lnFileStru +lnLoop,12],;
    laFileStru[lnFileStru +lnLoop,13],laFileStru[lnFileStru +lnLoop,14],;
    laFileStru[lnFileStru +lnLoop,15],laFileStru[lnFileStru +lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru +lnLoop,17] ,laFileStru[lnFileStru +lnLoop,18]
ENDFOR

DECLARE laIndex[2,2]
laIndex[1,1] = 'ACCOUNT+CUSTPO'
laIndex[1,2] = 'CUSTPO'
laIndex[2,1] = 'cOrdType+ORDER'
laIndex[2,2] = '&lcOrdHdr'
*laIndex
=gfCrtTmp(lcOrdHdr,@laFileStru,@laIndex)
*=gfCrtTmp(lcOrdHdr,@laFileStru,'ACCOUNT+CUSTPO','CUSTPO')
SELECT(lcOrdHdr)
INDEX ON 'cOrdType+ORDER' TAG (lcOrdHdr)
SET ORDER TO TAG 'CUSTPO' IN (lcOrdHdr)


SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lContract'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

FOR lnLoop = 1 TO  2
  STORE ' ' TO  laFileStru[lnFileStru +lnLoop,7],laFileStru[lnFileStru +lnLoop,8],;
    laFileStru[lnFileStru +lnLoop,9],laFileStru[lnFileStru +lnLoop,10],;
    laFileStru[lnFileStru +lnLoop,11],laFileStru[lnFileStru +lnLoop,12],;
    laFileStru[lnFileStru +lnLoop,13],laFileStru[lnFileStru +lnLoop,14],;
    laFileStru[lnFileStru +lnLoop,15],laFileStru[lnFileStru +lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru +lnLoop,17] ,laFileStru[lnFileStru +lnLoop,18]
ENDFOR
=gfCrtTmp(lcOrdLine,@laFileStru,'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)','ORDLINST',.F.)
*CREATE TABLE (gcWorkDir+lcOrdLine) FROM ARRAY laFileStru
SELECT (lcOrdLine)
*INDEX ON cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) TAG ORDLINST
INDEX ON CORDTYPE+ORDER+STYLE+STORE+STR(LINENO,6) TAG ORDLINES
INDEX ON CORDTYPE+ORDER+STR(LINENO,6) TAG ORDLINE
INDEX ON ALLTRIM(CUSTPO)+ALLTRIM(STYLE)+ALLTRIM(STORE) TAG ORDLN
SELECT(lcOrdLine)
USE IN (lcOrdLine)
=gfOpenTable(oAriaApplication.WorkDir+lcOrdLine,'ORDLN','EX')

SCATTER MEMVAR BLANK MEMO

SELECT BomVar
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]


laFileStru[lnFileStru+1,1] = 'nRecno'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cStatus'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
FOR lnLoop = 1 TO  2
  STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
    laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
    laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
    laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
    laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR
IF USED(lcT_BomVar)
  ZAP IN (lcT_BomVar)
ELSE
  =gfCrtTmp(lcT_BomVar,@laFileStru,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
ENDIF


SELECT(lnAlias)
*-- end of lfCratTemp.

*!**************************************************************************
*!* Name        : lfValRec
*!* Developer   : SMM - SAEED MOHAMED
*!* Date        : 12/14/2004
*!* Purpose     : Validate record
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfValRec()
*!***************************************************************************
FUNCTION lfValRec
PRIVATE llValid

llValid = .T.
IF EMPTY(ALLTRIM(&lcXLS..A))
  INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
    ALLTRIM(STR(RECNO(lcXLS))) + ', Missing Account Number')
  llValid = .F.
ENDIF

IF EMPTY(ALLTRIM(&lcXLS..A)) OR EMPTY(ALLTRIM(&lcXLS..B)) OR EMPTY(ALLTRIM(&lcXLS..C)) OR EMPTY(ALLTRIM(&lcXLS..D))
  INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
    ALLTRIM(STR(RECNO(lcXLS))) + ', Missing Data')
  llValid = .F.
ENDIF

* Validate existence of Account
IF !SEEK('M'+PADR(ALLTRIM(&lcXLS..A),5),'CUSTOMER')
  INSERT INTO (lcLogFile) (NRECNO,cError) VALUES (RECNO(lcXLS),'Error in Record Number : ' +;
    ALLTRIM(STR(RECNO(lcXLS))) + ', Customer ' + ALLTRIM(&lcXLS..A) +' is not found')
  llValid = .F.
ELSE
  m.cTermCode = CUSTOMER.cTermCode
  m.ShipVia   = CUSTOMER.ShipVia
  m.Spcinst   = CUSTOMER.Spcinst
  m.Account  = ALLTRIM(IIF(!EMPTY(CUSTOMER.PriceLvl),CUSTOMER.PriceLvl,'A'))
ENDIF

RETURN llValid

*!**************************************************************************
*!* Name      : lfAddress
*!* Developer : Saeed Mohammed (SMM)
*!* Date      : 12/13/2004
*!* Purpose   :
*!**************************************************************************
FUNCTION lfAddress
PRIVATE lnCount,lcFldNam,lcFldVal,lcCountr
SELECT (lcXLS)
IF EOF()
  LOCATE
  lcCountr = IIF (EMPTY(L),oAriaApplication.DefaultCountry,ALLTRIM(UPPER(L)))
  SKIP
ELSE
  lcCountr = IIF (EMPTY(L),oAriaApplication.DefaultCountry,ALLTRIM(UPPER(L)))
ENDIF
SELECT SYCINT
=gfSEEK(lcCountr)
FOR lnCount = 1 TO 6
  lcFldNam = 'CPART'+ ALLTRIM(STR(lnCount))+'LAB'
  lcFldVal = EVALUATE(lcFldNam)
  IF lnCount=1
    lcFldVal = 'Address1'
  ENDIF
  IF lnCount=2 AND EMPTY(lcFldVal)
    lcFldVal = 'Address2'
  ENDIF
  lcFldNam ='CAddress'+ALLTRIM(STR(lnCount))
  DO CASE
  CASE UPPER(lcFldVal)='ADDRESS1'
    m.&lcFldNam = ALLTRIM(&lcXLS..G)
  CASE UPPER(lcFldVal)='ADDRESS2'
    m.&lcFldNam = ALLTRIM(&lcXLS..H)
  CASE UPPER(lcFldVal)='CITY'
    m.&lcFldNam = ALLTRIM(&lcXLS..I)
  CASE UPPER(lcFldVal)='STATE'
    m.&lcFldNam = ALLTRIM(&lcXLS..J)
  CASE UPPER(lcFldVal)='ZIP'
    m.&lcFldNam = ALLTRIM(&lcXLS..K)
  CASE UPPER(lcFldVal)='COUNTRY'
    m.&lcFldNam = ALLTRIM(&lcXLS..L)
  ENDCASE
NEXT

*!**************************************************************************
*!* Name        : lfGetSize
*!* Developer   : NADER NABIL (NNA)
*!* Date        : 02/12/2005
*!* Purpose     : Get Size No from the scale file
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfAddUsrDt()
*!***************************************************************************
*C126257,1
FUNCTION lfGetSize
PARAMETERS lcSZCode
STORE '' TO lcSizNo
lnOldAlias=SELECT(0)
IF gfSEEK('S'+STYLE.SCALE,'SCALE')
  SELECT SCALE
  FOR I = 1 TO 8
    LCI = ALLTRIM(STR(I))
    IF ALLTRIM(SCALE.SZ&LCI) = lcSZCode
      lcSizNo = LCI
      EXIT
    ENDIF
  ENDFOR
ENDIF
SELECT(lnOldAlias)
RETURN lcSizNo

*-- End of lfGetSize.
*!**************************************************************************
*!* Name        : lfAddUsrDt
*!* Developer   : Saeed Mohammed (SMM)
*!* Date        : 12/13/2004
*!* Purpose     : Add user id ,date  and time to files08/01/2002
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfAddUsrDt()
*!***************************************************************************
FUNCTION lfAddUsrDt
PARAMETERS lcAlias
PRIVATE lcCurrAls
lcCurrAls = SELECT()
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
REPLACE ALL cAdd_User WITH oAriaApplication.User_ID,;
  dAdd_Date WITH oAriaApplication.SystemDate,;
  cAdd_Time WITH TIME()

SELECT (lcCurrAls)
*-- end of lfAddUsrDt.

*!**************************************************************************
*!* Name        : lfSaveData
*!* Developer   : Saeed Mohammed (SMM)
*!* Date        : 12/13/2004
*!* Purpose     : Save temp file to Actual orders
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfSaveData()
*!***************************************************************************
FUNCTION lfSaveData

*EXTERNAL ARRAY laData , laKeyField
PRIVATE llOrdSaved
lnAlias = SELECT()

STORE 'O'  TO lcOrdType
DECLARE laWareHouses[1,2]  , laSetups[6,2] , laKeyField[2,4] , laSeasons[1,2] ,;
  laCodes[1,10]      , laSource[1,2] , laOrdStatus [1]

DECLARE laWareHouses1[1,2]

STORE ''   TO laWareHouses , lcODefWare    , lcWareHouse     , lcScFields ,;
  laSeasons     , laCodes       , lcODefSes       , lcSeason   ,laOrdStatus
STORE .F.  TO laSetups     , llFound1      , llFound2        , llBrowse
STORE 1    TO lnWareHouse  , lnOrdStatus   , lnSeason
STORE m.Stname TO lcShipName
STORE m.cAddress1 TO lcShipAdd1
STORE m.cAddress2 TO lcShipAdd2
STORE PADR(m.cAddress3,15,' ')+','+m.cAddress4+','+m.cAddress5 TO lcShipAdd3
STORE m.caddress6 TO lcShipAdd4
STORE ' ' TO lcShipAdd5

STORE 0    TO lnBook    , lnOpen
STORE 0.00 TO lnOpenAmt , lnBookAmt , lnTotAmt

*-- variables of SOUPDATE.PRG [Start]
DECLARE laVariables[6] , laScrMode[4]
STORE .F. TO llContinue , llBomVarnt , llCDPerL
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
lnWareHouse = ASCAN(laWareHouses,lcODefWare)
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))

STORE .F. TO llMFDsPrc , llPoDsPrc
llOrdSaved = .F.
*--Loop throght the temp file lcOrdHdr to save data
SELECT (lcOrdHdr)
SET ORDER TO TAG &lcOrdHdr
SCAN FOR !LERRORS
  llOrdSaved = .T.
  SCATTER FIELDS &lcScFields MEMVAR
  * m.cOrdType = 'O'
  *TO laData
  *---
  SELECT ORDHDR
  *!* B611318,1 AHH 05/21/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][Begin]
  *!*	  gfappend()
  *!*	  Gath  Fields &lcScFields Memvar
  *!* B611318,1 AHH 05/21/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][Begin]

  REPLACE CORDTYPE WITH 'O'
  gfreplace()
  *---
  *
  SELECT (lcOrdLine)
  SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
  GO TOP
  SELECT (lcOrdHdr)
  llContinue = .T.

  *!B608550,1 5/11/2008, AKA Fix error while importing Sales order from excel  [Start]
  *DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
  WITH .F., 'A', lcOrdLine,.F.,.F.,lcT_BomVar,LOFORMSET
  DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
    WITH .F., 'A', lcOrdHdr,lcOrdLine,.F.,.F.,lcT_BomVar,LOFORMSET
  *!B608550,1 5/11/2008, AKA Fix error while importing Sales order from excel  [End]


  *,This.lcAlocated ,This.lcOrdCanLn ,This.lcT_BomVar,THISFORMSET

  *DO lfSavScr WITH .F.,.T. IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.prg')

ENDSCAN

*--Delele Records of the immediately saved order
SELECT (lcOrdLine)
DELETE ALL
PACK
*mmt
SELECT (lcOrdHdr)
DELETE ALL
PACK
*mmt
IF llOrdSaved
  PRIVATE lcCBCODE,lcAttn,lcEmail,lcSt,lcName2
  SELECT (lcXLS)
  IF EOF()
    GO BOTTOM
    lcCBCODE = ALLTRIM(M)
    lcEmail  = ALLTRIM(P)
    lcSt     = ALLTRIM(B)
    lcName2  = ALLTRIM(F)
    SKIP
  ELSE
    IF !BOF()
      lnRecNo = RECNO(lcXLS)
      SKIP -1
    ENDIF
    lcCBCODE = ALLTRIM(M)
    lcEmail  = ALLTRIM(P)
    lcSt     = ALLTRIM(B)
    lcName2  = ALLTRIM(F)
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcXLS))
      GOTO lnRecNo IN &lcXLS
    ENDIF
  ENDIF

  SELECT ORDHDR
  *!* B611318,1 AHH 05/21/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][Begin]

  *!*	  GFREPLACE("CBCODE    With'"+lcCBCode+"'")
  *!*	  GFREPLACE("CEmail    With '"+lcEmail+"'")
  *!*	  GFREPLACE("CSupTerr  With '"+lcST+"'")
  *!*	  GFREPLACE("Caddress5 WITH '"+IIF(ALT_SHPTO,lcEmail,Caddress5)+"'")
  *!*	  GFREPLACE("Caddress4 WITH '"+IIF(ALT_SHPTO,ALLTRIM(Caddress3)+','+ALLTRIM(Caddress4),Caddress4)+"'")
  *!*	  GFREPLACE("Caddress3 WITH '"+IIF(ALT_SHPTO,Caddress2,Caddress3)+"'")
  *!*	  GFREPLACE("Caddress2 WITH '"+IIF(ALT_SHPTO,Caddress1,Caddress2)+"'")
  *!*	  GFREPLACE("Caddress1 WITH '"+IIF(ALT_SHPTO,lcName2,Caddress1)+"'")

  gfreplace([CBCODE    With "]+lcCBCODE+["])
  gfreplace([CEmail    With "]+lcEmail+["])
  gfreplace([CSupTerr  With "]+lcSt+["])
  gfreplace([Caddress5 WITH "]+IIF(Alt_SHPTO,lcEmail,cAddress5)+["])
  gfreplace([Caddress4 WITH "]+IIF(Alt_SHPTO,ALLTRIM(cAddress3)+[,]+ALLTRIM(cAddress4),cAddress4)+["])
  gfreplace([Caddress3 WITH "]+IIF(Alt_SHPTO,cAddress2,cAddress3)+["])
  gfreplace([Caddress2 WITH "]+IIF(Alt_SHPTO,cAddress1,cAddress2)+["])
  gfreplace([Caddress1 WITH "]+IIF(Alt_SHPTO,lcName2,cAddress1)+["])


  *!* B611318,1 AHH 05/21/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][End]
  *!* B611315,1 MMT 05/16/2017 Issue#18 - JWE Import SO program does not import zip code of order[P20140306.0001][Start]
  *!* B611318,1 AHH 05/21/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][Begin]
  IF Alt_SHPTO
    *!*	    REPLACE cAddress1  WITH lcShipAdd1,;
    *!*	      cAddress2  WITH lcShipAdd2,;
    *!*	      cAddress3  WITH lcShipAdd3,;
    *!*	      cAddress4  WITH lcShipAdd4,;
    *!*	      cAddress5  WITH lcShipAdd5
    *!*	    gfreplace('')
    gfreplace([cAddress2 With "]+lcShipAdd1+["])
    gfreplace([cAddress3 With "]+lcShipAdd2+["])
    gfreplace([cAddress4 With "]+lcShipAdd3+["])
    gfreplace([cAddress5 With "]+lcShipAdd4+["])
    *!* B611328,1 AHH 01/06/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][Begin]
      gfreplace([stname With "]+ALLTRIM(stname)+" "+lcName2+["])
    *!* B611328,1 AHH 01/06/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][End]

  ENDIF
  *!* B611318,1 AHH 05/21/2017 Issue#18 - Change warehouse location to JWE Inc.[P20140306.0001][End]
  *!* B611315,1 MMT 05/16/2017 Issue#18 - JWE Import SO program does not import zip code of order[P20140306.0001][End]

  lnOrdCount = lnOrdCount + 1
  DIMENSION laOrders[lnOrdCount]
  laOrders[lnOrdCount] = ORDHDR.ORDER
  *-- Save the order's Notes (Comments)
  SELECT NOTEPAD
  IF !gfSEEK('B'+ORDHDR.ORDER)
    GFAPPEND()
    REPLACE TYPE 	WITH 'B'			,;
      KEY 	WITH ORDHDR.ORDER	,;
      MNOTES  WITH lcNotes
    gfreplace('')
  ENDIF
ENDIF
STORE '' TO lcNotes

lfSavefiles()
SELECT(lnAlias)


*-- End of lfSaveData.
*!*************************************************************
*! Name      : lfSavefiles
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 09/11/2006.
*! Purpose   : Function to save
*!*************************************************************
FUNCTION lfSavefiles
*E039415,1 ASM [Start]
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen 
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == LOFORMSET.DATASESSIONID
    IF !oAriaApplication.laRemoteTable[lnCounter].TABLEUPDATE(lcTranCode)
      lnUpdated=lnCounter
      EXIT
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  MESSAGEBOX('Saving Process is Rolled Back')
  THISFORMSET.UNDO()
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*E039415,1 ASM [End]
