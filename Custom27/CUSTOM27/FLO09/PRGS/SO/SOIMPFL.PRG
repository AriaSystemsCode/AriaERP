*!****************************************************************************
*! Program file        : SOIMPFL.PRG
*! Program description : Import Sales orders from Excel For Flower By Zoe (FLO09)
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : Sales Order - (SO)
*! Developer Name      : NNA - NADER NABIL ABD-ALMONEIM
*! Date completed      : 07/18/2005
*! Tracking Job Number : C128703
*!****************************************************************************
*! This program reads from an Excel file , import data to a Fox2 file , reads from this file
*! appends Sales Orders data to ordhdr , ordline files
*! The Excel sheet File must be saved as Excel Ver.4
**********************************
* Excel sheet Format :
**********************************
* A --> Field Holds CUSTOMER CODE
* B --> Field Holds SUPER TERRITORY
* C --> Field Holds STORE
* D --> Field Holds ADDRESS1       		-> SHIP_TO_ADDRESS1
* E --> Field Holds ADDRESS2       		-> SHIP_TO_ADDRESS2
* F --> Field Holds ADDRESS3       		-> SHIP_TO_ADDRESS3
* G --> Field Holds TELP
* H --> Field Holds CUSTOMER ORDER_ID   -> CUSTOMER PO
* I --> Field Holds ORDER_DATE     		-> ENTERED DATE
* J --> Field Holds SALES REP           -> REP1
* K --> Field Holds TOTAL_AMOUNT        -> BOOKAMT AND OPENAMT
* L --> Field Holds STYLE      			-> STYLE MAJOR
* M --> Field Holds COLOR		        -> STYLE COLOR / NONMAJOR
* N --> Field Holds SABLON				-> SABLON NAME    
* O --> Field Holds NEW COLOR CODE      -> SABLON COLOR
* P --> Field Holds ACCESSORIES COLOR   -> SABLON ACCESSORIES
* Q --> Field Holds QTY            		-> QTY * SIZE
* R --> Field Holds SIZE           		-> SIZE_X
* S --> Field Holds UNIT_PRICE			-> STYLE PRICE
* T --> Field Holds STYLE NAME     		-> NOT USED
* U --> Field Holds START DATE          -> ORDER START DATE
* V --> Field Holds FINISH DATE			-> ORDER COMPLETE DATE
*!****************************************************************************
*!Modifications :
*!
*!****************************************************************************
Private laSeason,laDivision,lcXLS,lcStore,lcPriceLvl, lnLogLnNo , lcSablon
DIMENSION laSeason[1,1],laDivision[1,1]
STORE '' TO laSeason[1,1],laDivision[1,1],lcXls,lcStore,lcPriceLvl,lcSablon
STORE 0 TO lnClrLen , lnClrStPos , lnStyLen , lnStyStPos , lnScaLen , lnScaStPos,lnLogLnNo
= lfChkStrct()
DO (gcScrDir+gcWinAppl+"\SOIMPFL.SPX")
*!*************************************************************
*! Name        : lfvgetFile
*! Developer   : NNA - NADER NABIL 
*! Date        : 07/18/2005
*! Purpose     : Function to get the Excel file Path
*!*************************************************************
*! Called From : SOIMPFL.SPR
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : = lfvgetFile()
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
*!*************************************************************************
*!* Name        : lfvProceed
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/18/2005
*!* Purpose     : To import the excel file 
*!***************************************************************************
*!* Called from : SOIMPFL.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvProceed()
*!***************************************************************************
FUNCTION lfvProceed

*--lnOrdCount : to hold the numbers of orders that have been generated
*--laOrders   : array to hold the orders numbers.
*--lcNotes    : hold the order's notes.
*--llLoop     : to skip the customer PO order if imported before or it has a style not found in the style file

Private lcLogFile, lError, lOrdErr, lcErrStr, llImpErr, lcDefDir , lcAlias , lcScFields,;
        lcOrdHdr , lcOrdLine ,lcCustPO ,lcStyle ,lnOrdCount ,lcNotes ,llLoop ,lcOldOrder ,lnOldAlias ,;
        lcStyClr,lcSz,lcAcc,lnPrice,lcPrice,llContinue
STORE '' TO laOrders , lcNotes , lcOldOrder ,lcCustPO ,lcStyle ,lcDefWare
STORE 0 TO lnOrdCount , lnBooked , lnBookAmt , lnOpened , lnOpenAmt , lnOldAlias
STORE .F. TO llLoop , llImpErr ,lError, llContinue

EXTERNAL ARRAY laData
DIMENSION laOrders[1]
lcLogFile  = gfTempName()
lcOrdHdr   = gfTempName()
lcOrdLine  = gfTempName()
lcPathName = ALLTRIM(lcPathName)
*-- Check if File Is .XLS file. and every this is ok to start importing
llContinue = lfChkFile()
IF !llContinue
  RETURN
ENDIF
*--Open Needed files
= lfOpenFls()
= lfCrtTmps()
SELECT (lcXLS) 
GO TOP 
DELETE && Delete the first row which hold the Fields headings
DELETE ALL FOR EMPTY(A) && Delete all records that has no Customer code
LOCATE
DELETE ALL FOR EMPTY(ALLTRIM(L)) OR EMPTY(ALLTRIM(M)) OR EMPTY(ALLTRIM(R)) OR VAL(ALLTRIM(Q)) = 0
PACK
SET DELETED ON

*EIH 12/12/2005 Update index to accumulate the same style+color+size+sablon [Begin].
*INDEX ON ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(R) TAG CUSTPO 
INDEX ON ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(M)+ALLTRIM(R)+ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P) TAG CUSTPO 
*EIH 12/12/2005 [End].

LOCATE
DO WHILE !EOF()
  lOrdErr   = .F.
  lcCustPo  = PADR(ALLTRIM(H),15,' ')
  lcAcc     = ALLTRIM(A)
  lcStyle   = lfGetStyle()
 
  SELECT (lcOrdHdr)
  IF SEEK(PADR(lcAcc,5)+lcCustPO+'O')  && Check the existence of the Cust PO
    IF gfModalGen('QRM00000B32018',.F.,ALLTRIM(lcCustPO),.F.,'Customer PO ð has been already entered. Do you want'+;
    			  ' to overwrite it or ignore?') = 2      
      = lfFillLog('8','',2)
      lError = .T.
	  SELECT (lcXLS)
      SKIP
      LOOP
    ELSE
      *--Insert the Recno into the cursor to sort on it
      = lfFillLog('9','',2)
      lError = .T.
    ENDIF
  ENDIF

  SELECT (lcOrdHdr)
  SCATTER MEMVAR MEMO BLANK
  SELECT (lcXLS)
  IF !lfValRec() && Validate Record
    lError = .T.
    lOrdErr = .T.
    llLOOP = .T.
  ENDIF
  SELECT (lcXLS)
  m.ORDER      = ''
  m.Account    = UPPER(A)
  m.CustPo     = ALLTRIM(H)
  m.Rep1       = RIGHT(ALLTRIM(&lcXLS..J),3)
  m.NCURRUNIT  = 1
  m.NEXRATE    = 1
  m.CCURRCODE  = 'USD'
  m.CORDTYPE   = 'O'
  m.STATUS     = 'O'
  m.Flag       = 'N' 
  m.Bulk       = 'N'
  m.DirectInv  = .F.
  m.LContract  = .F.
  m.CREORDER   = 'N'
  m.MULTI      = 'N'
  M.LERRORS    = .F.         && Define that this order is error free 
  m.Dept       = ''
  m.cordercat  = ''
  m.lineno     = 0
  m.caddress6  = ' '
  =lfVldDate()

  SELECT (lcXLS)
  *-- Get the customer needed information.
  IF !lfCustInfo()
    *EIH 12/12/2005 To get all errors in the current record [Begin].
    *SKIP	
    *EIH 12/12/2005 [End].
    llLoop = .T.
  ELSE
    llLoop = .F.
  ENDIF  
  lnLINENO = 0
  m.cDivision = ''
  SELECT(lcXLS)
  SCAN REST WHILE PADR(ALLTRIM(H),15,' ')+ALLTRIM(L)+ALLTRIM(R) = lcCustPo && All records with cust PO 
    *--If llLoop raised to true for any previous error like customer not found or store not found
    IF llLoop
      EXIT
    ENDIF
    *--If the customer PO has imported before then skip this order.
    lcOldOrder = ORDER('ORDHDR')
    lnOldAlias = SELECT(0)
    SET ORDER TO ORDCUST IN ORDHDR
    IF SEEK(ALLTRIM(m.Account) + lcCustPo ,'ORDHDR')
      SELECT ORDHDR 
      SCAN REST WHILE Account+UPPER(Custpo)+Cordtype+Order = ALLTRIM(m.Account) + lcCustPo FOR STATUS<>'X'
        
        *EIH 12/12/2005 Insert displayed message in log file [Begin].
        *= lfFillLog('4','')
        = lfFillLog('4',"Customer order# " + ALLTRIM(lcCustPo) + " has imported before . with Aria's order No." + ALLTRIM(ORDHDR.ORDER) + " this order Skipped",1)
        *EIH 12/12/2005 [End].
        
        llLoop = .T.
        EXIT
      ENDSCAN
      SET ORDER TO lcOldOrder IN ORDHDR
      IF llLoop
        EXIT
      ENDIF
      SELECT(lnOldAlias)
    ENDIF
    SELECT(lnOldAlias)
    lcStyle   = lfGetStyle()

    *--If the current style not found in the style file then skip this order at all. 
    IF !SEEK(UPPER(lcStyle),'STYLE') && Check for style
      *EIH 12/12/2005 Insert displayed message in log file [Begin].
      *= lfFillLog('5','')    
      = lfFillLog('5',"Style : " +lcStyle+" in Customer PO# " + ALLTRIM(&LCXLS..H) + " Not found in Aria's Style File . this order will be Skipped",2)    
      *EIH 12/12/2005 [End].
      
      llLoop = .T.
      *EIH 12/12/2005 To get all errors in the current record [Begin].
      *EXIT
      *EIH 12/12/2005 [End].
    ENDIF

    *--Check If the Finished Date is less than the Start date then skip this order at all. 
    IF CTOD(ALLTRIM(U)) > CTOD(ALLTRIM(V))
      = lfFillLog('10'," Starting date for order " + lccustpo + " precede finishing date",2)    
      llLoop = .T.
      
      *EIH 12/12/2005 To get all errors in the current record [Begin].
      *EXIT
      *EIH 12/12/2005 [End].
      
    ENDIF

    SELECT (lcXLS)
    lcStyle = lfGetStyle()
    =SEEK(UPPER(lcStyle),'STYLE')
    SELECT (lcOrdLine)
    
    *EIH 12/12/2005 If we found the style+color+sablon in the ordline file so we don't insert [Begin].
    *IF !SEEK(ALLTRIM(lcCustPO)+ALLTRIM(STYLE.Style)+ALLTRIM(lcStore)) 
    IF !SEEK(ALLTRIM(lcCustPO)+ALLTRIM(STYLE.Style)+ALLTRIM(lcStore)+ALLTRIM(SUBSTR(&lcXLS..N,1,6))+ALLTRIM(SUBSTR(&lcXLS..O,1,6))+ALLTRIM(SUBSTR(&lcXLS..P,1,6)));
        AND !EMPTY(lcStyle) AND !llloop 
    *EIH 12/12/2005 [End].
    
      INSERT into (lcOrdLine) (ORDER,STYLE,SCALE,SEASON,CWARECODE,CORDTYPE,ACCOUNT,CustPO,Store,lineno) Values ;
      ('',Style.Style, Style.Scale,Style.Season,Style.cDefWare,'O',m.Account,lcCustPO,lcStore,m.lineno)
	  IF EMPTY(m.cDivision)
        m.cDivision = Style.cDivision
      ELSE
        IF m.cDivision <> Style.cDivision
          = lfFillLog(''," The syles for this order have different divisions",2)             
          lError = .T.
          lOrdErr = .T.      
          *EIH 12/12/2005 To get all errors in the current record [Begin].
          *LOOP
          *EIH 12/12/2005 [End].
        ENDIF
      ENDIF
       lnPrice = IIF(TYPE('&lcXLS..S')='C',VAL(ALLTRIM(&lcXLS..S)),&lcXLS..S)
       lcPrice = 'Style.Price' + lcPriceLvl
       IF lcPriceLvl <> 'Q' AND (lnPrice <> EVAL(lcPrice)) 
         IF gfModalGen('QRM00000B32017',.F.,ALLTRIM(STYLE.CSTYMAJOR),.F.,'There is a mismatch between the transmitted'+ ;
         			   ' price and the original price of Style ð. Which price would you like to add in the'+ ;
         			   ' Sales Order?') = 1      
           lError = .T.
           REPLACE PRICE WITH lnPrice  
           = lfFillLog(''," There is a mismatch between the imported" + ;
   			    " price and the original price of Style " + ALLTRIM(STYLE.CSTYMAJOR) + ", the imported price was selected for the order.",2)
           SELECT (lcOrdLine)       
         ELSE
           = lfFillLog(''," There is a mismatch between the imported" + ;
   			    " price and the original price of Style " + ALLTRIM(STYLE.CSTYMAJOR) + ", the original price was selected for the order.",2)
		  lError = .T.	
          REPLACE PRICE WITH Eval(lcPrice)
		 ENDIF                 
       ELSE
         IF lcPriceLvl ='Q'
           = lfFillLog(''," This customers price level is at qty level ," + " the imported price of Style "+ALLTRIM(STYLE.CSTYMAJOR)+ " was selected for the order.",2)
 		   lError = .T.	
		   SELECT (lcXLS)	
         ENDIF
         SELECT (lcOrdLine)
         REPLACE PRICE WITH lnPrice  
       ENDIF
      
      lnLINENO = lnLINENO + 1
      
      SELECT (lcOrdLine)
      REPLACE 	LINENO 		WITH 	lnLINENO 					,;
				CSBLNAME	WITH	ALLTRIM(UPPER(&lcXLS..N))	,;
				CSBLCOLOR	WITH	ALLTRIM(UPPER(&lcXLS..O))	,;
				CTRMCOLOR	WITH	ALLTRIM(UPPER(&lcXLS..P))
    ENDIF
    
    SELECT (lcXLS)

    
    
    IF SEEK(UPPER(lcStyle),'STYLE')
      lcSZ = '&lcOrdLine..QTY'+lfGetSize(ALLTRIM(R))
      IF !(lcSZ == '&lcOrdLine..QTY')
        IF &lcSZ > 0 
          
          *EIH 12/12/2005 accumulate the same style+color+size+sablon [Begin].
          *IF !(ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P)==lcSablon)
          *  lcSablon = ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P)
          IF !(ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(M)+ALLTRIM(R)+ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P)==lcSablon)
            lcSablon = ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(M)+ALLTRIM(R)+ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P)
          *EIH 12/12/2005 [End].
          
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
            lcSZ = '&lcOrdLine..BOOK' + lfGetSize(ALLTRIM(R))
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
          ELSE
            
            *EIH 12/12/2005 accumulate the same style+color+size+sablon [Begin].
            *= lfFillLog(''," STYLE " + lcStyle + ", size " + ALLTRIM(&lcXLS..R) + " was entered before.")
            *lError  = .T.
            *lOrdErr = .T.
            *SELECT(lcXLS)
            *LOOP  
            
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
            lcSZ = '&lcOrdLine..BOOK' + lfGetSize(ALLTRIM(R))
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
                     
            *EIH 12/12/2005 [End].
          ENDIF
        ELSE
          lcSablon = ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(M)+ALLTRIM(R)+ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P)          
          Replace &lcSZ      WITH IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
          lcSZ = '&lcOrdLine..BOOK' + lfGetSize(ALLTRIM(R))
          Replace &lcSZ      WITH IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
        ENDIF
      ENDIF

      SELECT (lcOrdLine)
      Replace TotBook    WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8
      REPLACE TotQty     WITH Qty1  + Qty2  + Qty3  + Qty4  + Qty5  + Qty6  + Qty7  + Qty8
      REPLACE GROS_PRICE WITH EVAL(lcOrdLine+'.PRICE')
      m.LASTLINE  = lnLINENO - 1
    ENDIF
    SELECT (lcXLS)
  ENDSCAN
  SELECT (lcXLS)
  IF llLoop = .T.
    lError = .T.
    DO WHILE PADR(ALLTRIM(H),15,' ') = lcCustPo
      IF !EOF()
        SKIP
      ELSE
        EXIT  
      ENDIF  
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

  IF (m.BOOK > 0 OR m.Open > 0) And !lOrdErr
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
    SELECT DISTINCT Store FROM (lcOrdLine) INTO ARRAY laY
    IF _TALLY > 1
      m.Store     = '*Multi*'
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
    m.cWareCode = WAREHOUS.CWareCode

    SELECT (lcXLS)
    =gfAdd_Info(lcOrdHdr)
    =gfAdd_Info(lcOrdLine)
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
	CREATE CURSOR TMPSTR (mStrRep M(120))
    SELECT TMPSTR 
    APPEND BLANK	
	SELECT (lcLogFile) 
	GO TOP
	lnLineSt = 1
	SCAN
      SELECT TMPSTR 
      IF &lcLogFile..NRECNO <> 0
        lnLogLnNo = lnLogLnNo + 1
        IF &lcLogFile..NFIRST<>lnLineSt
          lnLineSt = &lcLogFile..NFIRST
          Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ SPACE(20)+'***************************'
        ENDIF
        Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ ALLTRIM(STR(lnLogLnNo)) + ' - ' + &lcLogFile..cError
      ELSE
        Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ &lcLogFile..cError
      ENDIF
      SELECT (lcLogFile)
    ENDSCAN
    SELECT TMPSTR
    DO (gcScrDir + 'SM\SMSTRREP.SPR')
    
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

IF USED(lcXls)
  USE IN (lcXls)
ENDIF
ERASE (gcWorkDir+lcXls+'.DBF')
ERASE (gcWorkDir+lcXls+'.CDX')

*-- End of Function lfvProceed.
*!*************************************************************************
*!* Name        : lfChkFile
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/20/2005
*!* Purpose     : Check the excel sheet file 
*!***************************************************************************
*!* Called from : Soimpfl.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChkFile()
*!***************************************************************************
FUNCTION lfChkFile
IF !(".XLS" $ lcPathName)
    = lfFillLog ('1','',2)
    RETURN .F.
ENDIF
IF !FILE(lcPathName)   
  *-- "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  _CUROBJ = OBJNUM(lcPathName)
  RETURN .F.
ENDIF
lcXls = 'SHEET'

If !Used(lcXLS)
  Select 0
  WAIT WINDOW NOWAIT 'Opening Excel file....'                               
  lcErrStr = ON('ERROR')
  llImpErr = .F.
  ON ERROR llImpErr = .T.
  lcDefDir = FULLPATH('')               && Save current default dir
  SET DEFA TO (gcWorkDir)
  STORE '' TO lcCommLine
  =lfImprtVFP()
  WAIT WINDOW NOWAIT '  Please wait...' 
  *--Loop until memo file be erased from VFP to be sure that the dbf file has physically Copied to the H.D.D
  DO WHILE FILE(lcCommLine)
    WAIT WINDOW NOWAIT '  Please wait...'
  ENDDO
  
  USE (gcWorkDir + 'SHEET' ) EXCLUSIVE
  lcXLS = 'SHEET'
  SELECT (lcXLS)
  SET DEFAULT TO &lcDefDir
  ON ERROR &lcErrStr
  WAIT CLEAR
  IF llImpErr
    = lfFillLog ('2','',2)
    RETURN .F.
  ENDIF
ELSE
  = lfFillLog ('3','',2)
  RETURN .F.
ENDIF
RETURN .T.

*-- End of Function lfChkFile.
*!**************************************************************************
*!* Name        : lfOpenFls
*!* Developer   : NNA - NADER NABIL 
*!* Date        : 07/18/2005
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
=gfOpenFile(gcDataDir+'NOTEPAD','NOTEPAD','SH')
=gfOpenFile(gcDataDir+'SOCODES','SOCODES','SH')
=gfOpenFile(gcDataDir+'CODES','CCODE_NO','SH')

SET ORDER TO TAG CURRENCY DESC IN 'SYCEXCH'
*-- end of lfOpenFls.

*!*************************************************************************
*!* Name        : lfCrtTmps
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/18/2005
*!* Purpose     : Create Needed tmp. files
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCrtTmps()
*!***************************************************************************
FUNCTION lfCrtTmps
PRIVATE laFileStru,lnFileStru,laIndex,lnAlias
lnAlias = SELECT()

*-- Create Temp file that will hold any error occurs while importing.
CREATE TABLE (gcWorkDir+lcLogFile) (nRecNo N(5) , cError C(200),nFirst N(1))
INDEX ON ALLTRIM(STR(nfirst))+padl(nRecNo,fsize('nRecNo')) TAG LogRecno
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'Data imported from file '+lcPathName+' at Date:'+DTOC(DATE())+' Time:'+TIME(),0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)


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

DECLARE laIndex[2,2]
laIndex[1,1] = 'ACCOUNT+CUSTPO'
laIndex[1,2] = 'CUSTPO'
laIndex[2,1] = 'cOrdType+ORDER'
laIndex[2,2] = '&lcOrdHdr'
=gfCrtTmp(lcOrdHdr,@laFileStru,@laIndex)
SET ORDER TO TAG 'CUSTPO' IN (lcOrdHdr)

SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+5,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'lContract'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

laFileStru[lnFileStru+3,1] = 'cSblName'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 0

laFileStru[lnFileStru+4,1] = 'cSblColor'
laFileStru[lnFileStru+4,2] = 'C'
laFileStru[lnFileStru+4,3] = 6
laFileStru[lnFileStru+4,4] = 0

laFileStru[lnFileStru+5,1] = 'cTrmColor'
laFileStru[lnFileStru+5,2] = 'C'
laFileStru[lnFileStru+5,3] = 6
laFileStru[lnFileStru+5,4] = 0

CREATE TABLE (gcWorkDir+lcOrdLine) FROM ARRAY laFileStru
INDEX ON cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) TAG ORDLINST
INDEX ON cOrdType+ORDER+STYLE+STORE+STR(LINENO,6) TAG ORDLINES
INDEX ON cOrdType+ORDER+STR(LINENO,6) TAG ORDLINE

*EIH 12/12/2005 Update index to seek for the style+color+sablon in the ordline file [Begin].
*INDEX ON ALLTRIM(CUSTPO)+ALLTRIM(STYLE)+ALLTRIM(STORE) TAG ORDLN
INDEX ON ALLTRIM(CUSTPO)+ALLTRIM(STYLE)+ALLTRIM(STORE)+ALLTRIM(CSBLNAME)+ALLTRIM(CSBLCOLOR)+ALLTRIM(CTRMCOLOR) TAG ORDLN 
*EIH 12/12/2005 [End].

SCATTER MEMVAR BLANK MEMO
SELECT(lnAlias)

*-- End of Function lfCrtTmps.
*!**************************************************************************
*!* Name        : lfCloseFls
*!* Developer   : NNA - NADER NABIL 
*!* Date        : 07/18/2005
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
*!* Developer   : NNA - NADER NABIL 
*!* Date        : 07/18/2005
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
  = lfFillLog('',", Missing Account Code",2)
  llValid = .F.
ENDIF
IF !SEEK('M'+PADR(ALLTRIM(&lcXLS..A),5),'CUSTOMER')
  = lfFillLog('',"Customer " + ALLTRIM(&lcXLS..A)+' for order# ' + ALLTRIM(&LCXLS..H) +' is not found , this order '+;
             'will be skipped',1)
  llValid = .F. 
ENDIF
RETURN llValid

*-- End of Function lfValRec.
*!**************************************************************************
*!* Name        : lfSaveData
*!* Developer   : NNA - NADER NABIL 
*!* Date        : 07/18/2005
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

SET ORDER TO TAG &lcOrdHdr
SCAN FOR !LERRORS
  llOrdSaved = .T.
  SCATTER FIELDS &lcScFields TO laData
  SELECT (lcOrdLine)
  GO TOP
  SELECT (lcOrdHdr)
  llContinue = .T.
  DO lfSavScr WITH .F.,.T. IN (gcAppHome + 'SO\SOUPDATE.prg')
ENDSCAN
*-- Save Sablon code and Color
=lfSavSabln()

*--Delele Records of the immediately saved order
SELECT (lcOrdLine)
DELETE ALL
PACK
IF llOrdSaved
  SELECT (lcXls)
  lnOrdCount = lnOrdCount + 1
  DIMENSION laOrders[lnOrdCount]
  laOrders[lnOrdCount] = ORDHDR.ORDER
ENDIF
SELECT(lnAlias)

*-- End of lfSaveData.
*!*************************************************************************
*!* Name        : lfAddress
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/18/2005
*!* Purpose     : 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAddress()
*!***************************************************************************
FUNCTION lfAddress

*-- End of Function lfAddress.
PRIVATE lnCount,lcFldNam,lcFldVal,lcCountr
SELECT (lcXLS)
SELECT SYCINT
SEEK(gcContCode)
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
      m.&lcFldNam = ALLTRIM(&lcXLS..R)   
    CASE UPPER(lcFldVal)='ADDRESS2'
      m.&lcFldNam = ALLTRIM(&lcXLS..S)   
    CASE UPPER(lcFldVal)='CITY'
      m.&lcFldNam = ALLTRIM(&lcXLS..T)   
    CASE UPPER(lcFldVal)='STATE'
      m.&lcFldNam = ALLTRIM(&lcXLS..U)   
    CASE UPPER(lcFldVal)='ZIP'
      m.&lcFldNam = ALLTRIM(&lcXLS..V)         
    CASE UPPER(lcFldVal)='COUNTRY'
      m.&lcFldNam = gcContCode       
  ENDCASE
NEXT

*!**************************************************************************
*!* Name      : lfPrnt
*!* Developer : NNA - NADER NABIL ABD-ALMONAM
*!* Date      : 07/18/2005
*!* Purpose   : Print the 2nd Rep commission difference report.
*!**************************************************************************
*!* Example   : = lfPrnt()
*!**************************************************************************
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
*!* Developer   : NNA - NADER NABIL ABD-ALMONEIM
*!* Date        : 07/18/2005
*!* Purpose     : Get Size No from the scale file
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetSize()
*!***************************************************************************
FUNCTION lfGetSize
PARAMETERS lcSZCode
STORE '' TO lcSizNo
lnOldAlias=select(0)
IF SEEK('S'+STYLE.SCALE,'SCALE')
  SELECT SCALE
  FOR I = 1 TO 8
    LCI = ALLTRIM(STR(I))
    IF ALLTRIM(UPPER(SCALE.SZ&LCI)) == ALLTRIM(UPPER(lcSZCode))
      lcSizNo = LCI 
      EXIT
    ENDIF
  ENDFOR
ENDIF
select(lnOldAlias)
RETURN lcSizNo

*-- End of lfGetSize.
*!*************************************************************************
*!* Name        : lfCustInfo
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/20/2005
*!* Purpose     : Get the Customer information .
*!***************************************************************************
*!* Called from : Soimpfl.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCustInfo()
*!***************************************************************************
FUNCTION lfCustInfo
lcStore = ALLTRIM(C)
IF SEEK('M'+PADR(ALLTRIM(&lcXLS..A),5),'CUSTOMER')
  IF UPPER(ALLTRIM(CUSTOMER.STNAME)) == UPPER(lcStore)
    lcStore = ''
  ENDIF
ENDIF
IF !EMPTY(lcStore) AND !SEEK('S'+PADR(ALLTRIM(&lcXLS..A),5)+lcStore,'CUSTOMER')
  = lfFillLog('6',"Store ( " + lcStore + " ) not assigned to Customer ( " +ALLTRIM(&lcXLS..C) + " ). order# " + lccustpo + " Skipped",2)        
  llLoop = .T.
  lError = .T.
  lOrdErr = .T.
  RETURN .F.
ELSE
  IF CUSTOMER.Status <> 'A' 
    = lfFillLog('7'," Customer " + ALLTRIM(CUSTOMER.ACCOUNT) +' for order# '+ALLTRIM(&LCXLS..H) +' is not active ',1)        
    lError = .T.
    lOrdErr = .T.
    SELECT (lcXLS)
    RETURN .F.
  ENDIF
  m.Buyer     = CUSTOMER.Buyer     
  m.Phone     = CUSTOMER.Phone1    
  m.Disc      = CUSTOMER.Disc      
  m.REP1      = CUSTOMER.SalesRep
  m.Comm1     = CUSTOMER.Comm
  m.REP2      = CUSTOMER.REP2
  m.Stname    = CUSTOMER.STNAME
  m.Cinsur    = CUSTOMER.Cinsur
  m.ctermcode = CUSTOMER.CtermCode
  m.Shipvia   = CUSTOMER.ShipVia
  m.Spcinst   = CUSTOMER.Spcinst   
  m.Comm2     = CUSTOMER.Comm2
  m.Priority  = CUSTOMER.Priority  
  m.Link_Code = IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code)
  m.GL_Sales  = IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)
  m.Store     = lcStore
  lcPriceLvl  = ALLTRIM(IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A'))
  m.Alt_SHPTO = .F.
  IF UPPER(ALLTRIM(&lcXLS..D)) <> UPPER(ALLTRIM(Customer.cAddress1)) OR UPPER(ALLTRIM(&lcXLS..E)) <> UPPER(ALLTRIM(Customer.cAddress2))
    m.Alt_SHPTO = .T.
    m.cAddress1 = ALLTRIM(&lcXLS..D)
    m.cAddress2 = ALLTRIM(&lcXLS..E)
    m.cAddress3 = ALLTRIM(&lcXLS..F)
  ENDIF	    
  RETURN .T.
ENDIF
*-- End of Function lfCustInfo.
*!*************************************************************************
*!* Name        : lfFillLog
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/20/2005
*!* Purpose     : Insert record in the log file with the error occurred
*!*             : or generate an error massage
*!***************************************************************************
*!* Called from : Soimpfl.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfFillLog()
*!***************************************************************************
FUNCTION lfFillLog
PARAMETERS lcNo,lcLogMsg,lnSection
PRIVATE lcMessage
STORE '' TO lcMessage
DO CASE
  CASE lcNo == '1'
    lcMessage = "This file can not be selected you must select a file of .Xls type."
  CASE lcNo == '2'
    lcMessage = "Invalid Excel file format!,you have to save your excel file as excel sheet ver.4"
  CASE lcNo == '3'
    lcMessage = "There is another file with the same name of the selected Excel file , Please rename."
  CASE lcNo == '4'
    lcMessage = "Customer order# " + lcCustPo + " has imported before . with Aria's order No." + ORDHDR.ORDER + " this order will be Skipped"
  CASE lcNo == '5'
    lcMessage = "Style : " +lcStyle+" in Customer PO# " + ALLTRIM(&LCXLS..H) + " Not found in Aria's Style File . this order will be Skipped"
  CASE lcNo == '6'
    lcMessage = "Store ( " + lcStore + " ) not found for Customer ( " +ALLTRIM(&lcXLS..C) + " ). order " + lccustpo + "will be Skipped"
  CASE lcNo == '7'
    lcMessage = " Customer " + ALLTRIM(CUSTOMER.ACCOUNT) +" is not active "
  CASE lcNo == '8'
    lcMessage = " Customer PO " + ALLTRIM(lcCustPO) + " has been already entered before. "
  CASE lcNo == '9'
    lcMessage = " Customer PO " + ALLTRIM(lcCustPO) + " has been already entered, and the user chose to overwrite."
  CASE lcNo == '10'
    lcMessage = " Starting date for order " + lccustpo + " precede finishing date. this order Skipped"
  ENDCASE
IF !EMPTY(lcLogMsg)
  *-- nFirst : this field to use it in making two sections in the log screen 
  INSERT INTO (lcLogFile) (NRECNO,CERROR,nFirst) VALUES ;
         (RECNO(LCXLS)+1 ,IIF(lnSection<>1,'Error in Record Number : '+ALLTRIM(STR((RECNO(LCXLS)+1)))+' , ','') + ;
         lcLogMsg,IIF(lnSection<>0,lnSection,2))
ENDIF
IF !EMPTY(lcMessage)
  =gfModalGen("INM00000B00000","DIALOG",'','',lcMessage)
ENDIF
*-- End of Function lfFillLog.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : Nader Nabil (NNA)
*! Date      : 04/18/2005
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from        : SOACJERE.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLen  = LEN(laItemSeg[lnCount,3])
    lnStyStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLen   = LEN(laItemSeg[lnCount,3])
    lnScaStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--End of lfChkStrct.

*!*************************************************************************
*!* Name        : lfGetStyle
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/20/2005
*!* Purpose     : Combine the style Parts from fields L,M and R
*!***************************************************************************
*!* Called from : Soimpfl.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfGetStyle()
*!***************************************************************************
FUNCTION lfGetStyle
Private lcReturn,lnOldAlias,lcScale,llExit
STORE .F. TO llExit
STORE '' TO lcReturn,lcScale
lnOldAlias = SELECT(0)
SELECT SCALE
LOCATE
SCAN REST WHILE Type+Scale+Prepak = 'S'
  FOR I = 1 TO 8
    LCI=ALLTRIM(STR(I))
    IF ALLTRIM(UPPER(SCALE.SZ&LCI)) == ALLTRIM(UPPER(&LCXLS..R))
      lcScale = SCALE.SCALE
      llExit = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llExit
    EXIT
  ENDIF
ENDSCAN
IF !EMPTY(lcScale)
  lcReturn = PADR(ALLTRIM(&lcXls..L),lnStyLen) + '-' + PADR(ALLTRIM(&lcXls..M),lnClrLen)+ '-' + PADR(lcScale,lnScaLen)
*EIH 12/12/2005 Display scale not found message in log file [Begin].
ELSE
  *Scale not found
  = lfFillLog(''," This scale  " + ALLTRIM(UPPER(&LCXLS..R)) + " not found in scale file. this order will be Skipped" ,2)
  lcReturn = PADR(ALLTRIM(&lcXls..L),lnStyLen) + '-' + PADR(ALLTRIM(&lcXls..M),lnClrLen)
  lError = .T.
  llloop = .T.
*EIH 12/12/2005 [End].  
ENDIF
SELECT(lnOldAlias)
RETURN lcReturn
*-- End of Function lfGetStyle.

*!*************************************************************************
*!* Name        : lfSavSabln
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 07/20/2005
*!* Purpose     : Save the Sablon Code and Color for the order
*!***************************************************************************
*!* Called from : Soimpfl.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSavSabln()
*!***************************************************************************
FUNCTION lfSavSabln
PRIVATE lcOrder,lnOldAlias,lcSblName,lcSblColor,lcTrmColor,lcSblNcode,lcSblCcode,lcTrmCcode
STORE '' TO lcSblName,lcSblColor,lcTrmColor,lcSblNcode,lcSblCcode,lcTrmCcode
lnOldAlias = SELECT(0)
SELECT (LCORDLINE)
lcOrder = ORDER(LCORDLINE)
SET ORDER TO TAG ORDLINE
SCAN
  lcSblName  = CSBLNAME
  lcSblColor = CSBLCOLOR
  lcTrmColor = CTRMCOLOR
  SELECT CODES
  *--Get the Code for Sablon name
  IF !EMPTY(lcSblName)
    = SEEK('N'+'CSBLNAME')
    SCAN REST WHILE cDefcode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'N'+'CSBLNAME'
      IF ALLTRIM(UPPER(CDISCREP))=ALLTRIM(UPPER(lcSblName))
        lcSblNcode = ALLTRIM(cCode_No)
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    = SEEK('D'+'CSBLNAME')
    lcSblNcode = ALLTRIM(cCode_No)
  ENDIF  
  *--Get the Code for Sablon color
  IF !EMPTY(lcSblColor)
    = SEEK('N'+'CSBLCOLOR')
    SCAN REST WHILE cDefcode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'N'+'CSBLCOLOR'
      IF ALLTRIM(UPPER(CDISCREP))=ALLTRIM(UPPER(lcSblColor))
        lcSblCcode = ALLTRIM(cCode_No)
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    = SEEK('D'+'CSBLCOLOR')
    lcSblCcode = ALLTRIM(cCode_No)
  ENDIF  
  *--Get the Code for Sablon Accessories color
  IF !EMPTY(lcTrmColor)
    = SEEK('N'+'CTRMCOLOR')
    SCAN REST WHILE cDefcode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'N'+'CTRMCOLOR'
      IF ALLTRIM(UPPER(CDISCREP))=ALLTRIM(UPPER(lcTrmColor))
        lcTRMCcode = ALLTRIM(cCode_No)
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    = SEEK('D'+'CTRMCOLOR')
    lcTRMCcode = ALLTRIM(cCode_No)
  ENDIF  
  SELECT SOCODES
  IF !SEEK('O'+&lcOrdHdr..ORDER+STR(&lcOrdLine..lineno,6))
    APPEND BLANK
    REPLACE 	CORDTYPE		WITH 'O'				,;
    			ORDER			WITH OrdHdr.Order   	,;
    			LINENO			WITH &lcOrdline..Lineno	,;
    			CSBLNAME		WITH lcSblNcode			,;
    			CSBLCOLOR		WITH lcSblCcode			,;
    			CTRMCOLOR		WITH lcTrmCcode
    =gfAdd_Info('SOCODES')
  ENDIF
ENDSCAN
SET ORDER TO TAG &lcOrder IN (LCORDLINE)
SELECT(lnOldAlias)
*-- End of Function lfSavSabln.

*!*************************************************************************
*!* Name        : lfVldDate
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 08/25/2005
*!* Purpose     : Validate the Entered,Start and Complete date for the order
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfVldDate()
*!***************************************************************************
FUNCTION lfVldDate
PRIVATE llLogError
llLogError=.F.
IF !EMPTY(ALLTRIM(I))
  m.Entered  = CTOD(ALLTRIM(I))
ELSE
  m.Entered  = gdSysDate
  llLogError = .T.
ENDIF
IF !EMPTY(ALLTRIM(U))
  m.Start    = CTOD(ALLTRIM(U))
ELSE
  m.Start    = IIF(!EMPTY(CTOD(ALLTRIM(V))) AND CTOD(ALLTRIM(V))>= gdSysDate,gdSysDate,;
               IIF(!EMPTY(CTOD(ALLTRIM(V))) AND CTOD(ALLTRIM(V))< gdSysDate,CTOD(ALLTRIM(V)),gdSysDate))
  llLogError = .T.
ENDIF
IF !EMPTY(ALLTRIM(V))
  m.Complete = CTOD(ALLTRIM(V))
ELSE
  m.Complete = IIF(!EMPTY(CTOD(ALLTRIM(U))) AND CTOD(ALLTRIM(U))<= gdSysDate,gdSysDate,;
               IIF(!EMPTY(CTOD(ALLTRIM(U))) AND CTOD(ALLTRIM(U))> gdSysDate,CTOD(ALLTRIM(U)),gdSysDate))
  llLogError = .T.
ENDIF
IF llLogError 
  = LFFILLLOG('',"One or more date (Enter,Start or Complete) for Order " + lccustpo + " is Empty so it saved as the current date",2)
ENDIF

*-- End of Function lfVldDate.
*!*************************************************************
*! Name        : lfImprtVFP
*! Developer   : Nader Nabil (NNA)
*! Date        : 08/25/2005
*! Purpose     : Import excel sheet with Ver97 to a foxpro DBF
*!             : that because Fox2.6 import only Excel4 and this 
*!             : Ver hold only about 16000 row filled with data
*!*************************************************************
*! Called from : Prg. Code
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfImprtVFP()
*!*************************************************************
FUNCTION lfImprtVFP

WAIT WINDOW NOWAIT 'Please wait...'
lcTempMemo = gfTempName()
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 
lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcAppHome+'SO\'+[SOIMPVFP.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
WAIT CLEAR
*-- End of Function lfImprtVFP.
