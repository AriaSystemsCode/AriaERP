*!****************************************************************************
*! Program file        : SOIMPRT.PRG
*! Program description : Import Sales orders from Excel files
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : Sales Order - (SO)
*! Developer Name      : SMM - SAEED MOHAMMED MOSTAFA
*! Date completed      : 12/14/2004
*! Tracking Job Number : C125116
*!****************************************************************************
* This program reads from an Excel file , import data to a Fox2 file , reads from this file
* appends Sales Orders data to ordhdr , ordline files

**********************************
* Excel sheet Format :
**********************************
* A --> Field Holds ACCOUNT NUMBER
* B --> Field Holds SUPER TERRITORY
* C --> Field Holds TERRITORY
* D --> Field Holds REGION 
* E --> Field Holds NAME1          -> FIRST NAME
* F --> Field Holds NAME2          -> LAST NAME 
* G --> Field Holds ATTENTION
* H --> Field Holds ADDRESS1       -> SHIP_TO_ADDRESS1
* I --> Field Holds ADDRESS2       -> SHIP_TO_ADDRESS2
* J --> Field Holds CITY           -> SHIP_TO_CITY
* K --> Field Holds STATE          -> SHIP_TO_STATE
* L --> Field Holds ZIP            -> SHIP_TO_ZIP
* M --> Field Holds COUNTRY        -> SHIP_TO_COUNTRY
* N --> Field Holds BUDGET CODE    
* O --> Field Holds ORDER_ID       -> CUSTOMER PO
* P --> Field Holds ORDER_DATE     -> START & COMPLETE DATE
* Q --> Field Holds EMAIL_ADDR
* R --> Field Holds TOTAL_AMOUNT
* S --> Field Holds ITEM_STYLE     -> STYLE
* T --> Field Holds SIZE           -> SIZE
* U --> Field Holds QTY            -> QTY * SIZE
* V --> Field Holds UNIT_PRICE
* W --> Field Holds ITEM_NAME      -> NOT USED
* X --> Field Holds COMMENT        -> NOTES
*!****************************************************************************
*! Modifications :
*! C126257,1 NNA 02/12/2005 Add and Modify Some Columns in the excel Sheet As follow :
*! C126257,1 NNA            1 - Delete Column (G) --> Attention
*! C126257,1 NNA            2 - Insert new Field in Column (Q) --> Uniform Type
*! C126257,1 NNA            3 - Move Column(X)-->Comments To Column(Y)
*! C126257,1 NNA            4 - Move Column(R)-->Total Amount To Column(X) 
*! C126257,1 NNA 			5 - Move Column(T)-->Size To Column(R)
*! C126257,1 NNA 			6 - Move Column(S)-->Item Style To Column(T)
*! C126257,1 NNA 			7 - Insert New Field in Column(S)-->Type
*! C126257,1 NNA 			8 - Move Column(W)-->Item Name To Column(U)
*! C126257,1 NNA 			9 - Move Column(U)-->Quantity To Column(V)
*! C126257,1 NNA 			9 - Move Column(V)-->Unit Price To Column(W)
*! C127687,1 NNA 05/26/2005 Get the default warehouse for the order
*!****************************************************************************

Private laSeason,laDivision
DIMENSION laSeason[1,1],laDivision[1,1]
STORE ' ' TO laSeason[1,1],laDivision[1,1]
DO (gcScrDir+gcWinAppl+"\SOIMPRT.SPX")

*!*************************************************************
*! Name      : lfvgetFile
*! Developer : Saeed Mohammed (SMM)
*! Date      : 12/13/2004
*! Purpose   : Function to get the Excel file Path
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfvgetFile()
*!*************************************************************
FUNCTION lfvgetFile

lcPathName  = GETFILE('XLS', 'Excel sheet Path : ','Select') 

IF EMPTY(lcPathName)
  SHOW GET PbProceed DISABLE && Disable the Proceed Button
ELSE
  SHOW GET PbProceed ENABLE  && Enable the Proceed Button
ENDIF

SHOW GET lcPathName

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

IF LEN(ALLTRIM(lcPathName)) = 1
  lcPathName = ALLTRIM(lcPathName)
ENDIF
IF EMPTY(lcPathName)
  *Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  _CUROBJ = OBJNUM(lcPathName)
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
  _CUROBJ = OBJNUM(lcPathName)
  RETURN
ENDIF

*-- Creating a cursor that will hold the imported data from the 
*-- excel file in order to append it in the master database file.
Private lcLogFile , lError, lOrdErr
lcLogFile = gfTempName()
lError = .F.

*C126257,1 NNA 02/12/2005 (BEGIN) Add new field to the Cursor to sort on it
*CREATE TABLE (gcWorkDir+lcLogFile) (cError C(200))
*INSERT INTO (lcLogFile) VALUES ('--------------------------------------------------------------------')
*INSERT INTO (lcLogFile) VALUES ('Data imported from file '+lcPathName+' at Date:'+DTOC(DATE())+' Time:'+TIME())
*INSERT INTO (lcLogFile) VALUES ('--------------------------------------------------------------------')
CREATE TABLE (gcWorkDir+lcLogFile) (nRecNo N(5) , cError C(200))
INDEX ON NRECNO TAG LogRecno
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'--------------------------------------------------------------------')
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'Data imported from file '+lcPathName+' at Date:'+DTOC(DATE())+' Time:'+TIME())
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'--------------------------------------------------------------------')
*C126257,1 NNA (END)

Private lcErrStr, llImpErr, lcDefDir, lcXLS,lcAlias

*--Open Needed files
=lfOpenFls()
*--Create needed temp files
Private lcOrdHdr, lcOrdLine

*C126257,1 NNA 02/12/2005 (BEGIN) Define Some new Variables
*PRIVATE lcCustPO,lcStyle
*--lnOrdCount : to hold the numbers of orders that have been generated
*--laOrders   : array to hold the orders numbers.
*--lcNotes    : hold the order's notes.
*--llLoop     : to skip the customer PO order if imported before or it has a style not found in the style file
PRIVATE lcCustPO,lcStyle,lnOrdCount , lcNotes , llLoop , lcOldOrder , lnOldAlias
DIMENSION laOrders[1]
STORE '' TO laOrders , lcNotes , lcOldOrder 
STORE 0 TO lnOrdCount , lnBooked , lnBookAmt , lnOpened , lnOpenAmt , lnOldAlias
STORE .F. TO llLoop
*C126257,1 NNA (END)

lcCustPO = ''
lcStyle  = ''
*C127687,1 NNA 05/26/2005 (Begin) Define a new Variable to hold the WareHouse
lcDefWare = ''
*C127687,1 NNA (End)

lcOrdHdr   = gfTempName()
lcOrdLine  = gfTempName()
lcXls = 'SHEET'
If !Used(lcXLS)
  Select 0
  WAIT WINDOW NOWAIT 'Opening Excel file....'                               
  lcErrStr = ON('ERROR')
  llImpErr = .F.
  ON ERROR llImpErr = .T.
  lcDefDir = FULLPATH('')               && Save current default dir
  SET DEFA TO (gcWorkDir)
  IMPORT FROM (lcPathName) TYPE XLS
  lcXLS = DBF()
  lcAlias = ALIAS()  
  Use (lcXLS) In 0 Again ALIAS 'SHEET'
  Select (lcALIAS)
  Use 
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

*C126257,1 NNA 02/12/2005 (BEGIN) Delete records that don't have qty. or Total Amount than pack
DELETE ALL FOR IIF(TYPE('&lcXLS..V')='C',VAL(ALLTRIM(&lcXLS..V)),&lcXLS..V) + ;
               IIF(TYPE('&lcXLS..X')='C',VAL(ALLTRIM(&lcXLS..X)),&lcXLS..X) = 0
PACK
*C126257,1 NNA (END)

SET DELETED ON

*C126257,1 NNA 02/12/2005 (BEGIN) Change Columns (O-->N , S-->T , T-->R)
*INDEX ON O+S+T TAG CUSTPO 
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
*C126257,1 NNA (END)
LOCATE
PRIVATE lcStore,lcScFields,lcStyClr,lcSz,lcAcc,lcCustPo,lnPrice,lcPriceLvl,lcPrice 
EXTERNAL ARRAY laData

*C126257,1 NNA 02/12/2005 (BEGIN) move this function to here to work on time only
=lfCratTemp()
*C126257,1 NNA (END)
DO WHILE !EOF()
  lOrdErr   = .F.

  *C126257,1 NNA 02/12/2005 (BEGIN) Change Columns (O-->N , S-->T , T-->R)
  *lcCustPo  = O
  *lcAcc     = A
  *=lfCratTemp()
  *SELECT ORDHDR
  *SET ORDER TO ORDCUST   && ACCOUNT+UPPER(CUSTPO)+CORDTYPE+ORDER  
  *IF SEEK(PADR(lcAcc,5)+PADR(lcCustPO,15)+'O') AND OrdHdr.Status = 'O' && Check the existence of the Cust PO
  lcCustPo  = ALLTRIM(N)
  lcAcc     = ALLTRIM(A)
  lcStyle   = ALLTRIM(T)
  SELECT (lcOrdHdr)
  IF SEEK(PADR(lcAcc,5)+PADR(lcCustPO,15)+'O')  && Check the existence of the Cust PO
  *C126257,1 NNA (END)

    IF gfModalGen('QRM00000B32018',.F.,ALLTRIM(lcCustPO),.F.,'Customer PO ð has been already entered. Do you want'+;
    			  ' to overwrite it or ignore?') = 2      
      *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
      *INSERT INTO (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Customer PO '+ALLTRIM(lcCustPO) +;
      								  ' has been already entered.')
      INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Customer PO '+ALLTRIM(lcCustPO) +;
      								  ' has been already entered.')
      *C126257,1 NNA (END)
      lError = .T.
	  SELECT (lcXLS)
      SKIP
      LOOP
    ELSE
    
    *C126257,1 NNA 02/12/2005 (BEGIN) comment these line because they're not correct
    *  SELECT ORDLINE
    *  DELETE ALL FOR ALLTRIM(CustPO) = ALLTRIM(lcCustPO)
    *  SELECT ORDHDR
    *  DELETE ALL FOR ALLTRIM(CustPO) = ALLTRIM(lcCustPO)
    *  INSERT INTO (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Customer PO '+ALLTRIM(lcCustPO) +;
      								  ' has been already entered, and the user chose to overwrite.')
      *--Insert the Recno into the cursor to sort on it
      INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;
                               ALLTRIM(STR(RECNO(LCXLS))) + ', Customer PO '+ALLTRIM(lcCustPO) +;
							  ' has been already entered, and the user chose to overwrite.')
    *C126257,1 NNA (END)

      lError = .T.
    ENDIF
  ENDIF

  *C126257,1 NNA 02/12/2005 (BEGIN) inside the program we deel with Temp files until saving
  *Select ORDHDR
  *SET ORDER TO ORDHDR   && CORDTYPE+ORDER 
  SELECT (lcOrdHdr)
  *C126257,1 NNA (END)

  SCATTER MEMVAR MEMO BLANK
  SELECT (lcXLS)
  IF !lfValRec() && Validate Record
    lError = .T.
    lOrdErr = .T.
    SKIP	
    LOOP
  ENDIF
  lcPricelvl = Alltrim(m.Account)
  SELECT(lcXLS)
  m.ORDER     = ''
  m.Account   = UPPER(A)
  *C126257,1 NNA 02/12/2005 (BEGIN) change Column O to N
  *m.CustPo    = O
  m.CustPo = ALLTRIM(N)
  m.Rep1   = RIGHT(ALLTRIM(&lcXLS..B),3)
  *C126257,1 NNA (END)

  m.NCURRUNIT = 1
  m.NEXRATE   = 1
  m.CCURRCODE = 'USD'
  m.CORDTYPE  = 'O'
  m.STATUS    = 'O'
  
  *C126257,1 NNA 02/12/2005 (BEGIN) change Column P to O
  *m.Stname    = ALLTRIM(E)+SPACE(1)+ ALLTRIM(F)
  *m.Start     = CTOD(ALLTRIM(P))
  *m.Complete  = CTOD(ALLTRIM(P))
  *--Make Stname hold only Name1
  m.Stname    = ALLTRIM(E)
 
  *--NNA 11/22/2005
  *m.Start     = CTOD(ALLTRIM(O))
  *m.Complete  = CTOD(ALLTRIM(O))
  m.Start     = IIF(TYPE('O')<>'D',CTOD(ALLTRIM(O)),O)
  m.Complete  = IIF(TYPE('O')<>'D',CTOD(ALLTRIM(O)),O)
  *--NNA 11/22/2005
  
  *C126257,1 NNA (END)
  
  m.Entered   = DATE()
  m.Flag      = 'N' 
  m.Bulk      = 'N'
  m.DirectInv = .F.
  m.LContract = .F.
  m.CREORDER  = 'N'
  m.MULTI     = 'N'
  m.Cinsur    = 'Y'
  M.LERRORS   = .F.         && Define that this order is error free 
  m.caddress6= ' '
  SELECT (lcXLS)
  *C126257,1 NNA 02/12/2005 (BEGIN) Get the last three Chr. for Column C and D to get the store 
  *--STORE = 'T' + Terrirtory c(3) + 'R' + Region C(3) 
  lcStore = 'T'+RIGHT(ALLTRIM(&lcXLS..C),3)+'R'+RIGHT(ALLTRIM(&lcXLS..D),3) 
  *C126257,1 NNA (END)
  IF !SEEK('S'+PADR(ALLTRIM(&lcXLS..A),5)+lcStore,'CUSTOMER')
    *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
    *INSERT INTO (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Store ' + ALLTRIM(lcStore) +' is not found')    
    INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;
                ALLTRIM(STR(RECNO(LCXLS))) + ', Store ' + ALLTRIM(lcStore) +' is not found')        
    llLoop = .T.
    =gfModalGen("INM00000B00000","DIALOG",'','',"Store ( " + lcStore + " ) not found for Customer ( "+;
                ALLTRIM(&lcXLS..A)+ " ). order " + lccustpo + " will be Skipped")
    *C126257,1 NNA (END)
    
    lError = .T.
    lOrdErr = .T.
    SKIP	
    LOOP
  ELSE
    IF CUSTOMER.Status <> 'A' 
      *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
      *INSERT INTO (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Customer '+ ALLTRIM(CUSTOMER.ACCOUNT) +' is not active ')    
      INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;
                  ALLTRIM(STR(RECNO(LCXLS))) + ', Customer '+ ALLTRIM(CUSTOMER.ACCOUNT) +' is not active ')    
      *C126257,1 NNA (END)
      lError = .T.
      lOrdErr = .T.
      SELECT (lcXLS)
      SKIP	
      LOOP
    ENDIF
    m.Buyer     = CUSTOMER.Buyer     
    m.Phone     = CUSTOMER.Phone1    
    m.Disc      = CUSTOMER.Disc      
    m.REP1      = CUSTOMER.SalesRep
    m.Comm1     = CUSTOMER.Comm
    
    *C126257,1 NNA 02/12/2005 (Begin) get the Correct Filed for REP2
    *m.REP2      = CUSTOMER.SalesRep
    m.REP2      = CUSTOMER.REP2
    *C126257,1 NNA (End)
    
    m.Comm2     = CUSTOMER.Comm2
    m.Priority  = CUSTOMER.Priority  
    m.Link_Code = IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code) && ?????
    m.GL_Sales  = IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)  && ?????
    m.Store     = lcStore
    =lfAddress()
    IF m.cAddress1 <> Customer.cAddress1 OR m.cAddress2 <> Customer.cAddress2 OR m.cAddress3 <> Customer.cAddress3 OR ;
       m.cAddress4 <> Customer.cAddress4 OR m.cAddress5 <> Customer.cAddress5 OR m.cAddress6 <> Customer.cAddress6
       m.Alt_SHPTO = .T.
    ENDIF	    
  ENDIF
  lnLINENO = 0
  m.cDivision = ''
  SELECT(lcXLS)

  *C126257,1 NNA 02/12/2005 (BEGIN) Change Columns (O-->N,S-->T,T-->R)
  *SCAN REST WHILE O+S+T = lcCustPo && All records with cust PO 
    *IF !SEEK(UPPER(ALLTRIM(S)),[STYLE]) && Check for style
      *INSERT INTo (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +' is not found')    
      *lError = .T.
      *lOrdErr = .T.
      *LOOP
  SCAN REST WHILE ALLTRIM(N)+ALLTRIM(T)+ALLTRIM(R) = lcCustPo && All records with cust PO 
    *--If the customer PO has imported before then skip this order.
    lcOldOrder = ORDER('ORDHDR')
    lnOldAlias = SELECT(0)
    SET ORDER TO ORDCUST IN ORDHDR
    IF SEEK(ALLTRIM(m.Account) + lcCustPo ,'ORDHDR')
      SELECT ORDHDR    
      SCAN REST WHILE Account+UPPER(Custpo)+Cordtype+Order = ALLTRIM(m.Account) + lcCustPo FOR STATUS<>'X'
        =gfModalGen("INM00000B00000","DIALOG",'','',"Customer order# " + lcCustPo + " has imported before . "+;
                   "this order will be Skipped")
        INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;
                    ALLTRIM(STR(RECNO(LCXLS))) + ', Customer Order# '+ALLTRIM(lcCustPO) +' has been imported before.')
        llLoop = .T.
        EXIT
      ENDSCAN
      SET ORDER TO lcOldOrder IN ORDHDR
      IF llLoop
        EXIT
      ENDIF
      SELECT(lnOldAlias)
    ENDIF
    SELECT CUSTOMER
    LOCATE FOR ACCOUNT=PADR(ALLTRIM(&lcXLS..A),5) AND SALESREP=RIGHT(ALLTRIM(&lcXLS..B),3) ;
        AND STORE = lcStore
    IF !FOUND()
      =gfModalGen("INM00000B00000","DIALOG",'','',"Sales Rep. ( " + RIGHT(ALLTRIM(&lcXLS..B),3) + " ) not found for Customer/Store ( "+;
                ALLTRIM(&lcXLS..A)+ " / " + lcStore + " ). order " + lccustpo + " will be Skipped")
      INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;
                  ALLTRIM(STR(RECNO(LCXLS))) + ', Sales Rep ' + RIGHT(ALLTRIM(&lcXLS..B),3) +' is not found')        
      llLoop = .T.
      EXIT
    ENDIF
    SELECT(lnOldAlias)
    *--If the current style not found in the style file then skip this order at all. 
    IF !SEEK(UPPER(ALLTRIM(T)),[STYLE]) && Check for style
      =gfModalGen("INM00000B00000","DIALOG",'','',"Style : " + ALLTRIM(T) +" in Customer PO# " +;
                 ALLTRIM(N) + " Not found in Aria's Style File . this order will be Skipped")
      INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' + ;
             ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +' is not found')    
      llLoop = .T.
      EXIT
    *C126257,1 NNA (END)

    ENDIF

    *C126257,1 NNA 02/12/2005 (BEGIN) comment this code because there is no need to it
    *SELECT STYLE
    *SKIP 
    *IF ALLTRIM(cStyMajor) == UPPER(ALLTRIM(&lcXLS..S))
    *INSERT INTo (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +' has more than one color')    
    *lError = .T.
    *lOrdErr = .T.
    *LOOP
    *ENDIF
    *C126257,1 NNA (END)

    SELECT (lcXLS)

    *C126257,1 NNA 02/12/2005 (BEGIN) Change column S to T
    *=SEEK(UPPER(ALLTRIM(S)),[STYLE])
    *lcStore = 'T'+ALLTRIM(&lcXLS..C)+'R'+ALLTRIM(&lcXLS..D)
    =SEEK(UPPER(ALLTRIM(T)),[STYLE])
    lcStore = 'T'+RIGHT(ALLTRIM(&lcXLS..C),3)+'R'+RIGHT(ALLTRIM(&lcXLS..D),3)
    IF !SEEK('S'+PADR(ALLTRIM(&lcXLS..A),5)+lcStore,'CUSTOMER')
      =gfModalGen("INM00000B00000","DIALOG",'','',"Store ( " + lcStore + " ) not found for Customer ( "+;
                ALLTRIM(&lcXLS..A)+ " ). order " + lccustpo + " will be Skipped")
      INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;
                  ALLTRIM(STR(RECNO(LCXLS))) + ', Store ' + ALLTRIM(lcStore) +' is not found')        
      llLoop = .T.
      EXIT
    ENDIF
    *C126257,1 NNA (END)

    SELECT (lcOrdLine)
    IF !SEEK(ALLTRIM(lcCustPO)+ALLTRIM(STYLE.Style)+ALLTRIM(lcStore))
      INSERT into (lcOrdLine) (ORDER,STYLE,SCALE,SEASON,CWARECODE,CORDTYPE,ACCOUNT,CustPO,Store) Values ;
      ('',Style.Style, Style.Scale,Style.Season,Style.cDefWare,'O',m.Account,lcCustPO,lcStore)
	  IF EMPTY(m.cDivision)
        m.cDivision = Style.cDivision
      ELSE
        IF m.cDivision <> Style.cDivision
          *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
          *NSERT INTo (lcLogFile) VALUES ('Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', The syles for this order have different divisions')             
          INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;
                      ALLTRIM(STR(RECNO(LCXLS))) + ', The syles for this order have different divisions')             
          *C126257,1 NNA (END)
          lError = .T.
          lOrdErr = .T.      
          LOOP
        ENDIF
      ENDIF

       *C126257,1 NNA 02/12/2005 (BEGIN) Change Column V-->W
       *Check for price
       *lnPrice = IIF(TYPE('&lcXLS..V')='C',VAL(ALLTRIM(&lcXLS..V)),&lcXLS..V)
       lnPrice = IIF(TYPE('&lcXLS..W')='C',VAL(ALLTRIM(&lcXLS..W)),&lcXLS..W)
       *C126257,1 NNA (END)

       lcPrice = 'Style.Price' + lcPriceLvl
       IF lcPriceLvl <> 'Q' AND (lnPrice <> EVAL(lcPrice)) 
         IF gfModalGen('QRM00000B32017',.F.,ALLTRIM(STYLE.CSTYMAJOR),.F.,'There is a mismatch between the transmitted'+ ;
         			   ' price and the original price of Style ð. Which price would you like to add in the'+ ;
         			   ' Sales Order?') = 1      
           *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
           *INSERT INTo (lcLogFile) VALUES ('Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', There is a mismatch between the imported'+ ;
                              			   ' price and the original price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+', the imported price was selected for the order.')   
           INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Record Number : ' +;
                       ALLTRIM(STR(RECNO(LCXLS))) + ', There is a mismatch between the imported'+ ;
           			   ' price and the original price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+  ;
           			   ', the imported price was selected for the order.')   
           *C126257,1 NNA (END)
           lError = .T.
           REPLACE PRICE WITH lnPrice  
           SELECT(lcXLS)

           *C126257,1 NNA 02/12/2005 (BEGIN) Change Column R-->X & U-->V & V-->W
           *IF IIF(TYPE('R') = 'C',VAL(ALLTRIM(R)),R)  <> IIF(TYPE('U') = 'C',VAL(ALLTRIM(U)),U)*IIF(TYPE('V') = 'C',VAL(ALLTRIM(V)),V)
             *INSERT INTo (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +' total amount does not match the price amount.')    		     		
           IF IIF(TYPE('X') = 'C',VAL(ALLTRIM(X)),X)  <> IIF(TYPE('V') = 'C',VAL(ALLTRIM(V)),V)*IIF(TYPE('W') = 'C',VAL(ALLTRIM(W)),W)
             INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;             
                         ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..T) +;
                         ' total amount does not match the price amount.')    		     		
           *C126257,1 NNA (END)

             lError = .T.
             lOrdErr = .T.
             Loop               
           ENDIF
         SELECT (lcOrdLine)       
         ELSE
           *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
           *INSERT INTo (lcLogFile) VALUES ('Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', There is a mismatch between the imported'+ ;
                             			   ' price and the original price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+', the original price was selected for the order.')   
           INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Record Number : ' +;             
                       ALLTRIM(STR(RECNO(LCXLS))) + ', There is a mismatch between the imported'+ ;
           			   ' price and the original price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+ ;
           			   ', the original price was selected for the order.')   
           *C126257,1 NNA (END)
		  lError = .T.	
          REPLACE PRICE WITH Eval(lcPrice)
		 ENDIF                 
       ELSE
         IF lcPriceLvl ='Q'
           *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
		   *INSERT INTo (lcLogFile) VALUES ('Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ", This customers price level is 'at qty level', "+ ;
                             			   'the imported price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+ ' was selected for the order.')              
           INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Record Number : ' +;             
                       ALLTRIM(STR(RECNO(LCXLS))) + ", This customers price level is 'at qty level', "+ ;
           			   'the imported price of Style '+ALLTRIM(STYLE.CSTYMAJOR)+ ' was selected for the order.')              
           *C126257,1 NNA (END)
 		   lError = .T.	
		   SELECT (lcXLS)	

           *C126257,1 NNA 02/12/2005 (BEGIN) Change Column R-->X & U-->V & V-->W
           *IF IIF(TYPE('R') = 'C',VAL(ALLTRIM(R)),R)  <> IIF(TYPE('U') = 'C',VAL(ALLTRIM(U)),U)*IIF(TYPE('V') = 'C',VAL(ALLTRIM(V)),V)
             *INSERT INTo (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +' total amount does not match the price amount.')    		     		
           IF IIF(TYPE('X') = 'C',VAL(ALLTRIM(X)),X)  <> IIF(TYPE('V') = 'C',VAL(ALLTRIM(V)),V)*IIF(TYPE('W') = 'C',VAL(ALLTRIM(W)),W)
             INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;             
                         ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..T) +;
                         ' total amount does not match the price amount.')    		     		
           *C126257,1 NNA (END)

             lError = .T.
             lOrdErr = .T.
             Loop               
           ENDIF
         ENDIF
         SELECT (lcOrdLine)
         REPLACE PRICE WITH lnPrice  
       ENDIF
      lnLINENO = lnLINENO + 1
      REPLACE LINENO WITH lnLINENO
    ENDIF
    SELECT (lcXLS)

    *C126257,1 NNA 02/12/2005 (BEGIN) get Size code from the Scale file and change some columns
    *lcSZ = '&lcOrdLine..QTY' + ALLTRIM(T)
    *IF &lcSZ > 0 
      *INSERT INTo (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +', size '+ALLTRIM(&lcXLS..T)+' was entered before.')    
      *lError  = .T.
      *lOrdErr = .T.
      *LOOP
    *ENDIF
    IF SEEK(UPPER(ALLTRIM(T)),'STYLE')
      lcSZ = '&lcOrdLine..QTY'+lfGetSize(ALLTRIM(R))
      IF !(lcSZ == '&lcOrdLine..QTY')
        IF &lcSZ > 0 
          *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
          *INSERT INTo (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +', size '+ALLTRIM(&lcXLS..R)+' was entered before.')    
          INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;             
                      ALLTRIM(STR(RECNO(LCXLS))) + ', STYLE ' + ALLTRIM(&lcXLS..S) +;
                      ', size '+ALLTRIM(&lcXLS..R)+' was entered before.')    
          *C126257,1 NNA (END)
          lError  = .T.
          lOrdErr = .T.
          SELECT(lcXLS)
          LOOP
        ENDIF
        *Replace &lcSZ      WITH IIF(TYPE('&lcXLS..V')='C',INT(VAL(ALLTRIM(U))),U)
        *lcSZ = '&lcOrdLine..BOOK' + ALLTRIM(T)
        *Replace &lcSZ      WITH IIF(TYPE('&lcXLS..V')='C',INT(VAL(ALLTRIM(U))),U)
        Replace &lcSZ      WITH IIF(TYPE('&lcXLS..V')='C',INT(VAL(ALLTRIM(V))),V)
        lcSZ = '&lcOrdLine..BOOK' + lfGetSize(ALLTRIM(R))
        Replace &lcSZ      WITH IIF(TYPE('&lcXLS..V')='C',INT(VAL(ALLTRIM(V))),V)
      ENDIF
      *C126257,1 NNA (END)
    
      SELECT (lcOrdLine)
      Replace TotBook    WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8
      REPLACE TotQty     WITH Qty1  + Qty2  + Qty3  + Qty4  + Qty5  + Qty6  + Qty7  + Qty8
    *C126257,1 NNA 02/12/2005 (BEGIN) change column X-->Y
      *REPLACE GROS_PRICE WITH Price * TotQty
      REPLACE GROS_PRICE WITH EVAL(lcOrdLine+'.PRICE')
    ENDIF
    *REPLACE NOTE_MEM   WITH ALLTRIM(&lcXLS..X)
    *m.BOOK      = TotBook
    *m.BookAmt   = Price * TotQty
    *m.OpenAmt   = Price * TotQty
    *m.CancelAmt = 0
    *m.OPEN      = m.Book - m.Cancel - m.Ship
    lcNotes = lcNotes +IIF(!EMPTY(lcNotes),CHR(13),'')+ ALLTRIM(&lcXLS..Y)
    *C126257,1 NNA (END)

    m.LASTLINE  = lnLINENO - 1
    SELECT (lcXLS)
  ENDSCAN
  
  *C126257,1 NNA 02/12/2005 (Begin) if the customer Po has imported before or the style not found
  *C126257,1 NNA            in the style file then llLoop =.T. and that mean skip this order at all.
  SELECT (lcXLS)
  IF llLoop = .T.
    DO WHILE ALLTRIM(N) = lcCustPo
      SKIP
    ENDDO
    SELECT (lcOrdLine)            
    DELETE ALL FOR CUSTPO = lcCustPo
    SELECT (lcOrdHdr)            
    DELETE ALL FOR CUSTPO = lcCustPo
    llLoop = .F.
  ENDIF
  SELECT (lcOrdLine)            
  SCAN
    m.BOOK      = m.BOOK + TotBook
    m.BookAmt   = m.BookAmt + (Price * TotQty)
    m.OpenAmt   = m.BookAmt
    m.CancelAmt = 0
    m.OPEN      = m.Book
  ENDSCAN
  SELECT (lcXLS)
  *C126257,1 NNA (End)

  IF (m.BOOK > 0 OR m.Open > 0) And !lOrdErr

    *C126257,1 NNA 02/12/2005 (BEGIN) Remove column G and Change N-->M & Q-->P
    *m.CBCODE   = ALLTRIM(N) 
    *m.CAttn    = ALLTRIM(G) 
    *m.CEmail   = ALLTRIM(Q)
    m.CBCODE   = ALLTRIM(M) 
    m.CEmail   = ALLTRIM(P)
    *C126257,1 NNA (END)

    m.CSupTerr = ALLTRIM(B)
    Private lcStySea
    SELECT (lcOrdLine)            
    Go Top
    lcStySea = ALLTRIM(UPPER(Season))
    lcStore  = ALLTRIM(UPPER(Store))
    Select Season from (lcOrdLine) where ALLTRIM(UPPER(season)) <> lcStySea into Array laX
    IF _TALLY <> 0
      m.Season = '*'
    ELSE
      m.Season = lcStySea
    ENDIF
    *C126257,1 NNA 02/12/2005 (BEGIN) If there are more one store then this order is multi
    *Select Store from (lcOrdLine) where ALLTRIM(UPPER(Store)) <> lcStore into Array laY
    *IF _TALLY <> 0
    SELECT DISTINCT Store FROM (lcOrdLine) INTO ARRAY laY
    IF _TALLY > 1
      m.Store     = '*Multi*'
    *C126257,1 NNA (END)
      m.MULTI     = 'Y'
      m.ShipVia   = '*'
      m.cAddress1 = ''
      m.cAddress2 = ''
      m.cAddress3 = ''
      m.cAddress4 = ''
      m.cAddress5 = ''
      m.cAddress6 = ''
      m.Alt_SHPTO = .F.
    ENDIF
    *C127687,1 NNA 05/26/2005 (Begin) Get the style Default Warehouse
    m.cWareCode = WAREHOUS.CWareCode
    *C127687,1 NNA (End)

    SELECT (lcXLS)
    =lfAddUsrDt(lcOrdHdr)
    =lfAddUsrDt(lcOrdLine)
    INSERT INTO (lcOrdHdr) FROM MEMVAR
    SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
    =lfSaveData()
  ENDIF
  SELECT (lcXLS)
 
  *C126257,1 NNA 02/12/2005 (Begin) Empty the Next Variables
  STORE 0 TO lnBooked , lnBookAmt , lnOpened , lnOpenAmt
  SET ORDER TO TAG 'ORDLN' IN (lcOrdLine)
  *C126257,1 NNA (End)

ENDDO
IF lError
  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'There were some errors and warning, Do you want to view the log report?') = 1
    lcWinTitl  = "Log Report"
	CREATE CURSOR TMPSTR (mStrRep M(120))
    SELECT TMPSTR 
    APPEND BLANK	
	SELECT (lcLogFile) 
	GO TOP
	SCAN
      SELECT TMPSTR 
      Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ &lcLogFile..cError
      SELECT (lcLogFile)
    ENDSCAN
    SELECT TMPSTR
    DO (gcScrDir + 'SM\SMSTRREP.SPR')
  ENDIF
ENDIF

*C126257,1 NNA 02/12/2005 (BEGIN) Massages that show to the user after generating orders or not
DO CASE 
  CASE lnOrdCount=1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is saved as' + laOrders[1])
  CASE lnOrdCount > 1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from ' + laOrders[1] + ' to ' + laOrders[lnOrdCount]+ ' have been generated successfully')
  CASE lnOrdCount = 0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ENDCASE
*C126257,1 NNA (END)

use in &lcXls

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
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
=gfOpenFile(gcDataDir+'SALESREP','SALESREP','SH')
=gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
=gfOpenFile(gcSysHome+'SYCCURR','CCURRCODE','SH')
=gfOpenFile(gcSysHome+'SYCEXCH','CURRENCY','SH')
=gfOpenFile(gcDataDir+'UnCmSess',gcDataDir+'TRANS','SH')
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
=gfOpenFile(gcSysHome +'SYCINT','CCONTCODE','SH')
*C126257,1 NNA 02/12/2005 (Begin) open the NotePad File to save the order's comments
=gfOpenFile(gcDataDir+'NOTEPAD','NOTEPAD','SH')
*C126257,1 NNA (End)
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
ERASE (gcWorkDir+lcOrdHdr+'.DBF')
ERASE (gcWorkDir+lcOrdHdr+'.CDX')

IF USED(lcOrdLine)
  USE IN (lcOrdLine)
ENDIF
ERASE (gcWorkDir+lcOrdLine+'.DBF')
ERASE (gcWorkDir+lcOrdLine+'.CDX')

IF USED(lcXls)
  USE IN (lcXls)
ENDIF
ERASE (gcWorkDir+lcXls+'.DBF')

*-- End of Function lfCloseFls.
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
  *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
  *INSERT INTO (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Missing Account Number')      	
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;             
              ALLTRIM(STR(RECNO(LCXLS))) + ', Missing Account Number')      	
  *C126257,1 NNA (END)
  llValid = .F.
ENDIF

IF EMPTY(ALLTRIM(&lcXLS..A)) OR EMPTY(ALLTRIM(&lcXLS..B)) OR EMPTY(ALLTRIM(&lcXLS..C)) OR EMPTY(ALLTRIM(&lcXLS..D))
  *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
  *INSERT INTO (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Missing Data')      	
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;             
              ALLTRIM(STR(RECNO(LCXLS))) + ', Missing Data')      	
  *C126257,1 NNA (END)
  llValid = .F.
ENDIF

* Validate existence of Account
IF !SEEK('M'+PADR(ALLTRIM(&lcXLS..A),5),'CUSTOMER')
  *C126257,1 NNA 02/12/2005 (BEGIN) Insert the Recno into the cursor to sort on it
  *INSERT INTO (lcLogFile) VALUES ('Error in Record Number : ' + ALLTRIM(STR(RECNO(LCXLS))) + ', Customer ' + ALLTRIM(&lcXLS..A) +' is not found')    
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),'Error in Record Number : ' +;             
              ALLTRIM(STR(RECNO(LCXLS))) + ', Customer ' + ALLTRIM(&lcXLS..A) +' is not found')    
  *C126257,1 NNA (END)
  llValid = .F. 
ELSE
  m.cTermCode = CUSTOMER.cTermCode 
  m.ShipVia   = CUSTOMER.ShipVia   
  m.Spcinst   = CUSTOMER.Spcinst   
  m.Account  = ALLTRIM(IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A'))
ENDIF

RETURN llValid

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

EXTERNAL ARRAY laData , laKeyField
PRIVATE llOrdSaved
lnAlias = SELECT()

STORE 'O'  TO lcOrdType
DECLARE laWareHouses[1,2]  , laSetups[6,2] , laKeyField[2,4] , laSeasons[1,2] ,;
        laCodes[1,10]      , laSource[1,2] , laOrdStatus [1]       
STORE ''   TO laWareHouses , lcODefWare    , lcWareHouse     , lcScFields ,;
             laSeasons     , laCodes       , lcODefSes       , lcSeason   ,laOrdStatus          
STORE .F.  TO laSetups     , llFound1      , llFound2        , llBrowse   
STORE 1    TO lnWareHouse  , lnOrdStatus   , lnSeason
STORE m.Stname TO lcShipName
STORE m.Caddress1 TO lcShipAdd1
STORE m.Caddress2 TO lcShipAdd2
STORE PADR(m.Caddress3,15,' ')+','+m.Caddress4+','+m.Caddress5 TO lcShipAdd3
STORE m.Caddress6 TO lcShipAdd4
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

laScrMode[4] = .T.
STORE .F. TO llMFDsPrc , llPoDsPrc
llOrdSaved = .F.
*--Loop throght the temp file lcOrdHdr to save data
SELECT (lcOrdHdr)

*C126257,1 NNA 02/12/2005 (BEGIN) change order of the Temp file to (Cordtype+Order)
SET ORDER TO TAG &lcOrdHdr
*C126257,1 NNA (END)

SCAN FOR !LERRORS
  llOrdSaved = .T.
  SCATTER FIELDS &lcScFields TO laData
  SELECT (lcOrdLine)
  GO TOP
  SELECT (lcOrdHdr)
  llContinue = .T.
  
  *C126257,1 NNA 02/12/2005 (BEGIN) use Saving Function in Soupdate with Parameter to not Show 
  *C126257,1 NNA            the Saving Massage
  *DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.prg')
  DO lfSavScr WITH .F.,.T. IN (gcAppHome + 'SO\SOUPDATE.prg')
  *C126257,1 NNA (END)

ENDSCAN
*C126257,1 NNA 02/12/2005 (BEGIN) Comment the Follwing massage and Change some Columns
*C126257,1 NNA            N-->M & Q-->P and Remove Column G(Attention)
*--Delele Records of the immediately saved order
SELECT (lcOrdLine)
DELETE ALL
PACK
*IF !llOrdSaved
*  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
*ELSE
*  Private lcCBCODE,lcAttn,lcEmail,lcSt
*  SELECT SHEET
*  IF EOF()
*    Go Bott
*    lcCBCODE = ALLTRIM(N)
*    lcAttn   = ALLTRIM(G)
*    lcEmail  = ALLTRIM(Q)
*    lcSt     = ALLTRIM(B)
*    Skip
*  ELSE
*    lcCBCODE = ALLTRIM(N)
*    lcAttn   = ALLTRIM(G)
*    lcEmail  = ALLTRIM(Q)
*    lcSt     = ALLTRIM(B)
*  ENDIF
*  Select ORDHDR	
*  REPLACE CBCODE   With lcCBCode 
*  REPLACE CATTN    With lcAttn
*  REPLACE CEmail   With lcEmail
*  REPLACE CSupTerr With lcST 

IF llOrdSaved
  Private lcCBCODE,lcAttn,lcEmail,lcSt,lcName2
  SELECT (lcXls)
  IF EOF()
    Go Bott
    lcCBCODE = ALLTRIM(M)
    lcEmail  = ALLTRIM(P)
    lcSt     = ALLTRIM(B)
    lcName2  = ALLTRIM(F)
    Skip
  ELSE
    IF !BOF()
      lnRecNo = RecNo(lcXls)
      SKIP -1
    ENDIF
    lcCBCODE = ALLTRIM(M)
    lcEmail  = ALLTRIM(P)
    lcSt     = ALLTRIM(B)
    lcName2  = ALLTRIM(F)
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcXls))
      GOTO lnRecNo IN &lcXls
    ENDIF
  ENDIF
  Select ORDHDR	
  REPLACE CBCODE    With lcCBCode 
  REPLACE CEmail    With lcEmail
  REPLACE CSupTerr  With lcST 
  REPLACE Caddress5 WITH IIF(ALT_SHPTO,lcEmail,Caddress5)
  REPLACE Caddress4 WITH IIF(ALT_SHPTO,ALLTRIM(Caddress3)+','+ALLTRIM(Caddress4),Caddress4)
  REPLACE Caddress3 WITH IIF(ALT_SHPTO,Caddress2,Caddress3)
  REPLACE Caddress2 WITH IIF(ALT_SHPTO,Caddress1,Caddress2)
  REPLACE Caddress1 WITH IIF(ALT_SHPTO,lcName2,Caddress1)

  lnOrdCount = lnOrdCount + 1
  DIMENSION laOrders[lnOrdCount]
  laOrders[lnOrdCount] = ORDHDR.ORDER
  *-- Save the order's Notes (Comments)
  SELECT NOTEPAD
  IF !SEEK('B'+ORDHDR.ORDER)
    APPEND BLANK
    REPLACE TYPE 	WITH 'B'			,;
    		KEY 	WITH ORDHDR.ORDER	,;
    		MNOTES  WITH lcNotes
  ENDIF
ENDIF
STORE '' TO lcNotes
*C126257,1 NNA (END)

SELECT(lnAlias)

*-- End of lfSaveData.
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
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'LERRORS'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

*C126257,1 NNA 02/12/2005 (Begin) add an additional index to the Temp file to check for Account/Custpo
*=gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+ORDER],lcOrdHdr)
DECLARE laIndex[2,2]
laIndex[1,1] = 'ACCOUNT+CUSTPO'
laIndex[1,2] = 'CUSTPO'
laIndex[2,1] = 'cOrdType+ORDER'
laIndex[2,2] = '&lcOrdHdr'
=gfCrtTmp(lcOrdHdr,@laFileStru,@laIndex)
SET ORDER TO TAG 'CUSTPO' IN (lcOrdHdr)
*C126257,1 NNA (End)

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

*C126257,1 NNA 02/12/2005 (Begin) use (Create Table) instead of GfcrtTmp to use file as exclusive
*C126257,1 NNA             and add Store to the index of (ordln)
*DECLARE laIndex[4,2]
*laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
*laIndex[1,2] = 'ORDLINST'
*laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
*laIndex[2,2] = 'ORDLINES'
*laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
*laIndex[3,2] = 'ORDLINE'
*laIndex[4,1] = 'ALLTRIM(CUSTPO)+ALLTRIM(STYLE)'
*laIndex[4,2] = 'ORDLN'
*=gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)
*SET ORDER TO TAG 'ORDLN' IN (lcOrdLine)
CREATE TABLE (gcWorkDir+lcOrdLine) FROM ARRAY laFileStru
INDEX ON cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) TAG ORDLINST
INDEX ON cOrdType+ORDER+STYLE+STORE+STR(LINENO,6) TAG ORDLINES
INDEX ON cOrdType+ORDER+STR(LINENO,6) TAG ORDLINE
INDEX ON ALLTRIM(CUSTPO)+ALLTRIM(STYLE)+ALLTRIM(STORE) TAG ORDLN
SELECT(lcOrdLine)
USE IN (lcOrdLine)
=gfOpenFile(gcWorkDir+lcOrdLine,'ORDLN','EX')
*C126257,1 NNA (End)

SCATTER MEMVAR BLANK MEMO
SELECT(lnAlias)
*-- end of lfCratTemp.

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
REPLACE cAdd_User WITH gcUser_ID,;
        dAdd_Date WITH gdSysDate,;
        cAdd_Time WITH Time()
        
SELECT (lcCurrAls)        
*-- end of lfAddUsrDt.

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
  Locate

  *C126257,1 NNA 02/12/2005 (BEGIN) Change Column M-->L
  *lcCountr = IIF (EMPTY(M),gcContCode,ALLTRIM(UPPER(M)))
  lcCountr = IIF (EMPTY(L),gcContCode,ALLTRIM(UPPER(L)))
  *C126257,1 NNA (END)

  Skip
ELSE

  *C126257,1 NNA 02/12/2005 (BEGIN) Change Column M-->L
  *lcCountr = IIF (EMPTY(M),gcContCode,ALLTRIM(UPPER(M)))
  lcCountr = IIF (EMPTY(L),gcContCode,ALLTRIM(UPPER(L)))
  *C126257,1 NNA (END)

ENDIF
SELECT SYCINT
SEEK(lcCountr)
FOR lnCount = 1 TO 6
  lcFldNam = 'CPART'+ ALLTRIM(STR(lnCount))+'LAB'
  lcFldVal = EVALUATE(lcFldNam)
  *C126257,1 NNA 02/12/2005 (BEGIN) GET Address2
  IF lnCount=1 
    lcFldVal = 'Address1'
  ENDIF
  IF lnCount=2 AND EMPTY(lcFldVal)
    lcFldVal = 'Address2'
  ENDIF
  *C126257,1 NNA (END)
  lcFldNam ='CAddress'+ALLTRIM(STR(lnCount))
  DO CASE 
    *C126257,1 NNA 02/12/2005 (BEGIN) Remove Column G and shift Columns from H--L one column
    *CASE UPPER(lcFldVal)='ADDRESS' OR UPPER(lcFldVal)='ADDRESS1'
    *  m.&lcFldNam = ALLTRIM(&lcXLS..H)   
    *CASE UPPER(lcFldVal)='ADDRESS2'
    *  m.&lcFldNam = ALLTRIM(&lcXLS..I)   
    *CASE UPPER(lcFldVal)='CITY'
    *  m.&lcFldNam = ALLTRIM(&lcXLS..J)   
    *CASE UPPER(lcFldVal)='STATE'
    *  m.&lcFldNam = ALLTRIM(&lcXLS..K)   
    *CASE UPPER(lcFldVal)='ZIP'
    *  m.&lcFldNam = ALLTRIM(&lcXLS..L)         
    *CASE UPPER(lcFldVal)='COUNTRY'
    *  m.&lcFldNam = ALLTRIM(&lcXLS..M)         
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
    *C126257,1 NNA (END)

  ENDCASE
NEXT

*!**************************************************************************
*!* Name      : lfPrnt
*!* Developer : Saeed Mohammed (SMM)
*!* Date      : 12/13/2004
*!* Purpose   : Print the 2nd Rep commission difference report.
*!**************************************************************************
*!* Example   : = lfPrnt()
*!**************************************************************************
*
FUNCTION lfvPrnt
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*-- End of lfvPrnt.
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
lnOldAlias=select(0)
IF SEEK('S'+STYLE.SCALE,'SCALE')
  SELECT SCALE
  FOR I = 1 TO 8
    LCI = ALLTRIM(STR(I))
    IF ALLTRIM(SCALE.SZ&LCI) = lcSZCode
      lcSizNo = LCI 
      EXIT
    ENDIF
  ENDFOR
ENDIF
select(lnOldAlias)
RETURN lcSizNo

*-- End of lfGetSize.