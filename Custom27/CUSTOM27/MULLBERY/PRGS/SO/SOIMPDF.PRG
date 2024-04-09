*!****************************************************************************
*! Program file        : SOIMPDF.PRG
*! Program description : Import Sales orders from Excel For Mullbery (FLO09)
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : Sales Order - (SO)
*! Developer Name      : TMI - Tarek Ibrahim
*! Date completed      : 07/18/2005
*! Tracking Job Number : N039433,1 
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
* J --> Field Holds SALES REP           -> REP1
* K --> Field Holds TOTAL_AMOUNT        -> BOOKAMT AND OPENAMT
* L --> Field Holds STYLE      			-> STYLE MAJOR
* M --> Field Holds COLOR		        -> STYLE COLOR / NONMAJOR
* N --> Field Holds SABLON				-> SABLON NAME    
* O --> Field Holds NEW COLOR CODE      -> SABLON COLOR
* P --> Field Holds ACCESSORIES COLOR   -> SABLON ACCESSORIES
* Q --> Field Holds QTY            		-> QTY * SIZE

* J --> Field Holds ORDER_DATE     		-> ENTERED  DATE
* K --> Field Holds STARTSHIP     		-> START    DATE
* L --> Field Holds COMPLETE     		-> COMPLETE DATE
* R --> Field Holds ADDRESS1       		-> SHIP_TO_ADDRESS1
* S --> Field Holds ADDRESS2       		-> SHIP_TO_ADDRESS2
* T --> Field Holds ADDRESS3       		-> SHIP_TO_ADDRESS3
* U --> Field Holds ADDRESS4       		-> SHIP_TO_ADDRESS4
* V --> Field Holds ADDRESS5       		-> SHIP_TO_ADDRESS5
*!****************************************************************************
*!Modifications :
*!
*!****************************************************************************
*!***************************************************************************
*B040008,1 MMR 12/13/2005 1)Show both the Regected Orders with the reason and the accepted Order .
*B040008,1 MMR            2)Call all the functions in the SOUPDATE.
*!***************************************************************************
Private laSeason,laDivision,lcXLS,lcStore,lcPriceLvl
DIMENSION laSeason[1,1],laDivision[1,1]
STORE '' TO laSeason[1,1],laDivision[1,1],lcXls,lcStore,lcPriceLvl,lcClrSpr,lcStySpr
STORE 0 TO lnClrLen , lnClrStPos , lnStyLen , lnStyStPos , lnScaLen , lnScaStPos

*- Get setup ( Default SO status to Hold )
lcGtDefSt = gfGetMemVar('M_HOLD_ORD')
lcDefStats = IIF( lcGtDefSt = 'Y' , 'H' , 'O' )

= lfChkStrct()
DO (gcScrDir+gcWinAppl+"\SOIMPDF.SPX")

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
PRIVATE lcStatus
lcPathName  = GETFILE('XLS', 'Excel sheet Path : ','Select') 
lcStatus = IIF(EMPTY(lcPathName),'DISABLE','ENABLE')
SHOW GET PbProceed &lcStatus
_CUROBJ = OBJNUM(PbProceed)
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
*B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order .
*DIMENSION laOrders[1]
DIMENSION laOrders[1,2]
*B040008,1.[End]
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

*- Create needed temp files
= lfCrtTmps()

*- 
SELECT (lcXLS)
GO TOP 
*- Delete the first row which hold the Fields headings
DELETE  
*B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
*WAIT WINDOW NOWAIT 'Delete all records that has no Customer code ... '
*DELETE ALL FOR EMPTY(&lcXLS..C) 

*LOCATE
*WAIT WINDOW NOWAIT 'Deleting lines with empty Division'
*DELETE ALL FOR EMPTY(L) OR EMPTY(R) OR EMPTY(AW)
*B040008,1 MMR.[End]
SET DELETED ON
INDEX ON PADR(ALLTRIM(A),10)+PADR(ALLTRIM(AW),6) TAG CDOFFORDER
SELECT (lcXLS)
LOCATE
DO WHILE !EOF(lcXLS)
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order .
  llAcc = .F.
  llLoop = .F.
  *B040008,1 MMR.[End]
  lOrdErr   = .F.
  lcOrderNo = UPPER(ALLTRIM(A))
  lcAcc     = UPPER(ALLTRIM(C))
  lcStyle   = lfGetStyle()

  SELECT (lcOrdHdr) 
  SCATTER MEMVAR MEMO BLANK

  SELECT (lcXLS) 
  IF !lfValRec() && Validate Record
    lError = .T.
    lOrdErr = .T.
    *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
    *SKIP
    *LOOP
    llAcc=.T.
    *B040008,1 MMR,[End]
  ENDIF
  
  SELECT (lcXLS)
  m.CORDTYPE   = 'O'
  m.ORDER      = ''
  m.Account    = PADR(ALLTRIM(UPPER(&lcXLS..C)),5)
  m.NCURRUNIT  = 1
  m.NEXRATE    = 1
  m.CCURRCODE  = 'USD'
  m.STATUS     = lcDefStats 
  m.Entered    = CTOD(ALLTRIM(&lcXLS..J))
  m.Start      = CTOD(ALLTRIM(&lcXLS..K))
  m.Complete   = CTOD(ALLTRIM(&lcXLS..L))
  m.Flag       = 'N' 
  m.Bulk       = 'N'
  m.DirectInv  = .F.
  m.LContract  = .F.
  m.CREORDER   = 'N'
  m.MULTI      = 'N'
  M.LERRORS    = .F.         && Define that this order is error free 
  m.Dept       = ''
  m.lineno     = 0
  m.caddress6  = ' '
  m.Note1      = ALLTRIM(&lcXls..W)
  m.Note2      = 'Digital Order Form# '+ ALLTRIM(&lcXLS..A)
  m.DEPT       = ALLTRIM(&lcXls..X)
  m.CustPo     = ALLTRIM(&lcXls..Y)
  m.CORDERCAT  = ALLTRIM(&lcXls..AC)  
  
  SELECT (lcXLS)
  *-- Get the customer needed information.
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  IF llAcc=.F. 
  *B040008,1 MMR.[End]
    IF !lfCustInfo()
      *SKIP
      *LOOP
    *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
    llAcc=.T.
    *B040008,1 MMR.[End]
    ENDIF
  ENDIF
  *B040008,1 MMR.[End]
  lnLINENO = 0
  m.CDOFFORDER = PADR(ALLTRIM(&lcXLS..A),10)
  m.CDIVISION  = PADR(ALLTRIM(&lcXls..AW),6)
  SELECT(lcXLS) 
  SCAN REST WHILE PADR(ALLTRIM(&lcXls..A),10)+PADR(ALLTRIM(&lcXls..AW),6) = m.CDOFFORDER + m.CDIVISION
    *--If the customer PO has imported before then skip this order.
    lcOldOrder = ORDER('ORDHDR')
    lnOldAlias = SELECT(0)
    SET ORDER TO CDOFFORDER IN ORDHDR
    IF SEEK(m.CDOFFORDER + m.CDIVISION ,'ORDHDR') .AND. ORDHDR.STATUS <> 'X'
      = lfFillLog('4','')
      llLoop = .T.
      SET ORDER TO lcOldOrder IN ORDHDR
      SELECT(lnOldAlias)
     *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
      *EXIT
      llAcc=.T.
      lError = .T.	
     *B040008,1 MMR.[End] 
    ENDIF    
    SELECT(lnOldAlias)
    
    *--If the current style not found in the style file then skip this order at all. 
    lcStyle = lfGetStyle()
    IF !SEEK(lcStyle,'STYLE') 
      *N039433,3  TMI [Start] 
      *= lfFillLog('5','')    
      lError = .T.	
      = lfFillLog('',"  Style " +lcStyle+" Not found. Order Skipped")    
      *N039433,3  TMI [End  ]       
      llLoop = .T.
      *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order .
      *EXIT
      llAcc=.T.
      *B040008,1 MMR.[End]
    ENDIF
    *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order .
    IF !llAcc
    *B040008,1 MMR.[End]
      *- Get store
      lcStore = PADR(ALLTRIM(&lcXls..E),8) 
    
      SELECT (lcOrdLine) 
      IF !SEEK(m.CDOFFORDER+lcStore+PADR(STYLE.Style,19)+STR(m.Lineno),lcOrdLine) 

        INSERT INTO (lcOrdLine) ;
        (ORDER,STYLE,SCALE,SEASON,CWARECODE,CORDTYPE,ACCOUNT,CustPO,Store,lineno,CDOFFORDER,Start,Complete) Values ;
        ('',Style.Style, Style.Scale,Style.Season,Style.cDefWare,'O',m.Account,m.CustPO,lcStore,m.lineno,m.CDOFFORDER,m.Start,m.Complete) 

	    IF EMPTY(m.cDivision)
          m.cDivision = Style.cDivision
     *-*ELSE
          *-* IF m.cDivision <> Style.cDivision
          *-*   = lfFillLog(''," The syles for this order have different divisions")             
          *-*   lError = .T.
          *-*   lOrdErr = .T.      
          *-*   LOOP
          *-* ENDIF
        ENDIF
      
        lnPrice = IIF(TYPE('&lcXLS..AI')='C',VAL(ALLTRIM(&lcXLS..AI)),&lcXLS..AI)
        lcPrice = 'Style.Price' + lcPriceLvl
        IF lcPriceLvl <> 'Q' AND (lnPrice <> EVAL(lcPrice)) 
          IF gfModalGen('QRM00000B32017',.F.,ALLTRIM(STYLE.CSTYMAJOR),.F.,'There is a mismatch between the transmitted'+ ;
        			     ' price and the original price of Style ð. Which price would you like to add in the'+ ;
        			     ' Sales Order?') = 1
            *N039433,3  TMI [Start] 
            *lError = .T.
            *N039433,3  TMI [End  ] 
            REPLACE PRICE WITH lnPrice  
            SELECT(lcXLS)
            
            SELECT (lcOrdLine)       
          ELSE
            = lfFillLog(''," There is a mismatch between the imported" + ;
   		      " price and the original price of Style " + ALLTRIM(STYLE.CSTYMAJOR) + ", the original price was selected for the order.")
            lError = .T.	
            REPLACE PRICE WITH Eval(lcPrice)
		  ENDIF                 
        ELSE
          IF lcPriceLvl ='Q'
            = lfFillLog(''," This customers price level is at qty level ," + " the imported price of Style "+ALLTRIM(STYLE.CSTYMAJOR)+ " was selected for the order.")
            lError = .T.	
            SELECT (lcXLS)	
            ENDIF
          SELECT (lcOrdLine)
          REPLACE PRICE WITH lnPrice  
        ENDIF
      
        lnLINENO = lnLINENO + 1
        SELECT (lcOrdLine)
        REPLACE 	LINENO 	    WITH    lnLINENO                  ;
        CSBLNAME    WITH    ALLTRIM(UPPER(&lcXLS..N))	          ;
                  CSBLCOLOR   WITH    ALLTRIM(UPPER(&lcXLS..O)) ;
                  CTRMCOLOR   WITH    ALLTRIM(UPPER(&lcXLS..P))
        =lfAddUsrDt(lcOrdLine)
      ENDIF
    
      SELECT (lcXLS)
      IF SEEK(lcStyle,'STYLE')
        SELECT (lcOrdLine)
        REPLACE &lcOrdLine..QTY1  WITH IIF(TYPE('&lcXLS..AL')='C',VAL(ALLTRIM(&lcXLS..AL)),&lcXLS..AL),;
                &lcOrdLine..QTY2  WITH IIF(TYPE('&lcXLS..AM')='C',VAL(ALLTRIM(&lcXLS..AM)),&lcXLS..AM),;
                &lcOrdLine..QTY3  WITH IIF(TYPE('&lcXLS..AN')='C',VAL(ALLTRIM(&lcXLS..AN)),&lcXLS..AN),;
                &lcOrdLine..QTY4  WITH IIF(TYPE('&lcXLS..AO')='C',VAL(ALLTRIM(&lcXLS..AO)),&lcXLS..AO),;
                &lcOrdLine..QTY5  WITH IIF(TYPE('&lcXLS..AP')='C',VAL(ALLTRIM(&lcXLS..AP)),&lcXLS..AP),;
                &lcOrdLine..QTY6  WITH IIF(TYPE('&lcXLS..AQ')='C',VAL(ALLTRIM(&lcXLS..AQ)),&lcXLS..AQ),;
                &lcOrdLine..QTY7  WITH IIF(TYPE('&lcXLS..AR')='C',VAL(ALLTRIM(&lcXLS..AR)),&lcXLS..AR),;
                &lcOrdLine..QTY8  WITH IIF(TYPE('&lcXLS..AS')='C',VAL(ALLTRIM(&lcXLS..AS)),&lcXLS..AS)
        REPLACE &lcOrdLine..BOOK1 WITH IIF(TYPE('&lcXLS..AL')='C',VAL(ALLTRIM(&lcXLS..AL)),&lcXLS..AL),;
                &lcOrdLine..BOOK2 WITH IIF(TYPE('&lcXLS..AM')='C',VAL(ALLTRIM(&lcXLS..AM)),&lcXLS..AM),;
                &lcOrdLine..BOOK3 WITH IIF(TYPE('&lcXLS..AN')='C',VAL(ALLTRIM(&lcXLS..AN)),&lcXLS..AN),;
                &lcOrdLine..BOOK4 WITH IIF(TYPE('&lcXLS..AO')='C',VAL(ALLTRIM(&lcXLS..AO)),&lcXLS..AO),;
                &lcOrdLine..BOOK5 WITH IIF(TYPE('&lcXLS..AP')='C',VAL(ALLTRIM(&lcXLS..AP)),&lcXLS..AP),;
                &lcOrdLine..BOOK6 WITH IIF(TYPE('&lcXLS..AQ')='C',VAL(ALLTRIM(&lcXLS..AQ)),&lcXLS..AQ),;
                &lcOrdLine..BOOK7 WITH IIF(TYPE('&lcXLS..AR')='C',VAL(ALLTRIM(&lcXLS..AR)),&lcXLS..AR),;
                &lcOrdLine..BOOK8 WITH IIF(TYPE('&lcXLS..AS')='C',VAL(ALLTRIM(&lcXLS..AS)),&lcXLS..AS)
        REPLACE TotBook    WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ;
                TotQty     WITH Qty1 +Qty2 +Qty3 +Qty4 +Qty5 +Qty6 +Qty7 +Qty8  ;
                GROS_PRICE WITH EVAL(lcOrdLine+'.PRICE')
            
      ENDIF
      SELECT (lcXLS)
    *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order .
    ENDIF
    *B040008,1 MMR.[End]
  ENDSCAN
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order .
  IF !llAcc
  *B040008,1 MMR.[End]
    SELECT (lcXLS)
    IF llLoop = .T.
      *- Go beyond current doff order plus division 
      SCAN REST WHILE PADR(ALLTRIM(A),10)+PADR(ALLTRIM(AW),6) = m.CDOFFORDER + m.CDIVISION
      ENDSCAN
    
      SELECT (lcOrdLine)            
      DELETE ALL FOR CDOFFORDER = m.CDOFFORDER
      SELECT (lcOrdHdr)            
      DELETE ALL FOR CDOFFORDER = m.CDOFFORDER
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

      INSERT INTO (lcOrdHdr) FROM MEMVAR  
      =lfAddUsrDt(lcOrdHdr)

      SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
      =lfSaveData()
    ENDIF
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order .
  ENDIF
  *B040008,1 MMR.[End]
  SELECT (lcXLS)
 
  STORE 0 TO lnBooked , lnBookAmt , lnOpened , lnOpenAmt
  SET ORDER TO TAG 'ORDLN' IN (lcOrdLine)
ENDDO
*B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
*IF lError
IF lError OR lnOrdCount<>0
  *IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'There are some errors, Do you want to view the log report?') = 1
  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'Do you want to view the log report?') = 1
*B040008,1 MMR.[End]
    PRIVATE lmStrRep,lnLogLnNo
    lnLogLnNo = 0
    lcWinTitl  = "Log Report"

	CREATE CURSOR TMPSTR (mStrRep M(120))
    SELECT TMPSTR 
    APPEND BLANK	
	SELECT (lcLogFile) 
	GO TOP
	SCAN
      lmStrRep = CHR(13)
      IF nRecNo = 0
        lmStrRep = lmStrRep + ALLTRIM(cError)
      ELSE
        lnLogLnNo = lnLogLnNo + 1
        lmStrRep = lmStrRep + PADL(lnLogLnNo,3)+'- '+PADL(nRecNo,5)+'-'+ALLTRIM(cError)
      ENDIF                                     
      SELECT TMPSTR 
      REPLACE mStrRep WITH lmStrRep ADDITIVE
    ENDSCAN
    SELECT TMPSTR 
    *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
    lnLogLnNo=0 
    IF lnOrdCount<> 0
      lmStrRep = CHR(13)
      lmStrRep=lmStrRep+' '
      REPLACE mStrRep WITH lmStrRep ADDITIVE
      lmStrRep = CHR(13)
      lmStrRep = lmStrRep + '*****************************'
      REPLACE mStrRep WITH lmStrRep ADDITIVE
      lmStrRep = CHR(13)
      lmStrRep = lmStrRep + ALLTRIM('Generated Orders')
      REPLACE mStrRep WITH lmStrRep ADDITIVE
      lmStrRep = CHR(13)
      lmStrRep = lmStrRep + '*****************************'
      REPLACE mStrRep WITH lmStrRep ADDITIVE
      FOR lnOrdr = 1 TO ALEN(laOrders,1)
       lmStrRep = CHR(13)
       cDofOrdr = laOrders[lnOrdr,2]
       cAriaOrdr= laOrders[lnOrdr,1]
       lnLogLnNo = lnLogLnNo + 1
       lmStrRep = lmStrRep + PADL(lnLogLnNo,3)+'- '+"New Order# "+ cAriaOrdr+" Generated From Dof Order# "+cDofOrdr
       SELECT TMPSTR 
       REPLACE mStrRep WITH lmStrRep ADDITIVE
       ENDFOR
     ENDIF  
    *B040008,1 MMR.[End]
    DO (gcScrDir + 'SM\SMSTRREP.SPR')
  ENDIF
ENDIF

DO CASE 
  CASE lnOrdCount=1
    *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
    *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is saved as ' + laOrders[1])
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is saved as ' + laOrders[1,1])
    *B040008,1 MMR.[End]
  CASE lnOrdCount > 1
    *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
    *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from ' + laOrders[1] + ' To ' + laOrders[lnOrdCount]+ ' have been generated successfully')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from ' + laOrders[1,1] + ' To ' + laOrders[lnOrdCount,1]+ ' have been generated successfully')
    *B040008,1 MMR.[End]
  CASE lnOrdCount = 0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ENDCASE
lcPathName = ''
SHOW GET lcPathName
SHOW GET pbProceed DISABLE

USE in &lcXls
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
    = lfFillLog ('1','')
    RETURN .F.
ENDIF
IF !FILE(lcPathName)   
  *-- "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  _CUROBJ = OBJNUM(lcPathName)
  RETURN .F.
ENDIF
lcXls = 'SHEET'
*N039433,3  TMI [Start] 
IF USED(lcXls)
  USE IN &lcXls
ENDIF
*N039433,3  TMI [End  ] 
If !Used(lcXLS)
  *N039433,3  TMI [Start] 
  lcSafe = SET('SAFE')
  SET SAFE OFF
  *N039433,3  TMI [End  ] 
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
  *N039433,3  TMI [Start] 
  SET SAFE &lcSafe 
  *N039433,3  TMI [End  ] 
  
  IF llImpErr
    = lfFillLog ('2','')
    RETURN .F.
  ENDIF

  SELECT (lcXLS)  
  IF TYPE('AW')='U'
    = lfFillLog ('2','')
    RETURN .F.
  ENDIF
ELSE
  = lfFillLog ('3','')
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
IF !USED('STYLE'   )
  =gfOpenFile(gcDataDir+'STYLE'   ,'STYLE','SH')
ENDIF  
IF !USED('SCALE'   )
  =gfOpenFile(gcDataDir+'SCALE'   ,'SCALE','SH')
ENDIF  
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
ENDIF  
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF  
IF !USED('SALESREP')
  =gfOpenFile(gcDataDir+'SALESREP','SALESREP','SH')
ENDIF  
IF !USED('ORDHDR'  )
  =gfOpenFile(gcDataDir+'ORDHDR'  ,'CDOFFORDER','SH')
ENDIF  
IF !USED('ORDLINE' )
  =gfOpenFile(gcDataDir+'ORDLINE' ,'ORDLINE','SH')
ENDIF  
IF !USED('SYCCURR' )
  =gfOpenFile(gcSysHome+'SYCCURR' ,'CCURRCODE','SH')
ENDIF  
IF !USED('SYCEXCH' )
  =gfOpenFile(gcSysHome+'SYCEXCH' ,'CURRENCY','SH')
ENDIF  
IF !USED('UnCmSess')
  =gfOpenFile(gcDataDir+'UnCmSess','TRANS','SH')
ENDIF  
IF !USED('STYDYE'  )
  =gfOpenFile(gcDataDir+'STYDYE'  ,'STYDYE','SH')
ENDIF  
IF !USED(+'SYCINT' )
  =gfOpenFile(gcSysHome +'SYCINT' ,'CCONTCODE','SH')
ENDIF  
IF !USED('NOTEPAD' )
  =gfOpenFile(gcDataDir+'NOTEPAD' ,'NOTEPAD','SH')
ENDIF  
IF !USED('CODES'   )
  =gfOpenFile(gcDataDir+'CODES'   ,'CCODE_NO','SH')
ENDIF  

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
CREATE TABLE (gcWorkDir+lcLogFile) (nRecNo N(5) , cError C(200))
INDEX ON NRECNO TAG LogRecno

INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'--------------------------------------------------------------------')
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'Data imported from file '+lcPathName+' at Date:'+DTOC(DATE())+' Time:'+TIME())
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'--------------------------------------------------------------------')
INSERT INTO (lcLogFile) (NRECNO,cError ) VALUES (0,'Sn.-xls.Ln-Description')

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
laIndex[1,1] = 'cOrdType+ORDER'
laIndex[1,2] = lcOrdHdr
laIndex[2,1] = 'CDOFFORDER+CDIVISION'
laIndex[2,2] = 'CDOFFORDER'
=gfCrtTmp(lcOrdHdr,@laFileStru,@laIndex)
SET ORDER TO TAG CDOFFORDER IN (lcOrdHdr)

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
INDEX ON ALLTRIM(CUSTPO)+ALLTRIM(STYLE)+ALLTRIM(STORE) TAG ORDLN
INDEX ON CDOFFORDER+Store+STYLE+STR(Lineno) TAG DOFFORD

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
PRIVATE llValid,lnSlct
lnSlct = SELECT()
llValid = .T.
IF EMPTY(ALLTRIM(&lcXLS..C)) 
  = lfFillLog('',", Missing Account Code")
  llValid = .F.
ENDIF
*B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
IF !EMPTY(&lcXLS..C)
*B040008,1 MMR.[End]
  IF !SEEK('M'+PADR(ALLTRIM(&lcXLS..C),5),'CUSTOMER')
    = lfFillLog('',', Customer ' + ALLTRIM(&lcXLS..C) +' is not found')
    llValid = .F. 
  ENDIF
*B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
ENDIF
*B040008,1 MMR.[End]
SELECT (lnSlct)
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
               'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS,LFROMWEB,LSENDACK,CACKSTATUS,CCONTREF,'+;
               'CDOFFORDER'

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
  *B040008,1 MMR 12/13/2005 2)Call all the functions in the SOUPDATE.
  *DO lfSavScr WITH .F.,.T. IN (gcAppHome + 'SO\SOUPDATE.prg')
  DO lfSavScr WITH .F.,.T.
  *B040008,1 MMR.[End]
ENDSCAN
*--Delele Records of the immediately saved order
SELECT (lcOrdLine)
DELETE ALL
PACK
IF llOrdSaved
  SELECT (lcXls)
  lnOrdCount = lnOrdCount + 1
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  *DIMENSION laOrders[lnOrdCount]
  *laOrders[lnOrdCount] = ORDHDR.ORDER
  DIMENSION laOrders[lnOrdCount,2]
  laOrders[lnOrdCount,1] = ORDHDR.ORDER
  laOrders[lnOrdCount,2] = ORDHDR.cdofforder
  *B040008,1 MMR.[End]
ENDIF
SELECT(lnAlias)

*-- End of lfSaveData.

*!**************************************************************************
*!* Name        : lfAddUsrDt
*!* Developer   : NNA - NADER NABIL 
*!* Date        : 07/18/2005
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
lcStore = PADR(ALLTRIM(&lcXls..E),8)
*IF SEEK('M'+PADR(ALLTRIM(&lcXLS..C),5),'CUSTOMER')
*  IF UPPER(ALLTRIM(CUSTOMER.STNAME)) == UPPER(lcStore)
*    lcStore = ''
*  ENDIF
*ENDIF
IF !EMPTY(lcStore) AND !SEEK('S'+PADR(ALLTRIM(&lcXLS..C),5)+lcStore,'CUSTOMER')
  = lfFillLog('6','')        
  llLoop = .T.
  lError = .T.
  lOrdErr = .T.
  RETURN .F.
ELSE
  IF CUSTOMER.Status <> 'A' 
    = lfFillLog('7','')        
    lError = .T.
    lOrdErr = .T.
    SELECT (lcXLS)
    RETURN .F.
  ENDIF
  
  m.Buyer     = IIF(!EMPTY(&lcXls..H),ALLTRIM(&lcXls..H),CUSTOMER.Buyer)
  m.Phone     = IIF(!EMPTY(ALLTRIM(&lcXls..AD)),ALLTRIM(&lcXls..AD),CUSTOMER.Phone1 )
  m.Disc      = CUSTOMER.Disc      

  m.Stname    = CUSTOMER.STNAME
  m.Cinsur    = CUSTOMER.Cinsur
  m.ctermcode = IIF(!EMPTY(ALLTRIM(&lcXLS..F)),ALLTRIM(&lcXLS..F),CUSTOMER.CtermCode)

  m.Rep1      = IIF(!EMPTY(&lcXLS..I),ALLTRIM(&lcXLS..I),CUSTOMER.SALESREP)
  m.Comm1     = IIF(!EMPTY(&lcXLS..I),0,CUSTOMER.COMM)

  m.Shipvia   = IIF(!EMPTY(&lcXLS..G),ALLTRIM(&lcXLS..G),CUSTOMER.ShipVia)
  m.Spcinst   = CUSTOMER.Spcinst   

  m.Priority  = CUSTOMER.Priority  
  m.Link_Code = IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code)
  m.GL_Sales  = IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)
  m.Store     = lcStore
  lcPriceLvl  = ALLTRIM(IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A'))

  m.Alt_SHPTO = .T.
  m.cAddress1 = ALLTRIM(&lcXLS..R)
  m.cAddress2 = ALLTRIM(&lcXLS..S)
  m.cAddress3 = ALLTRIM(&lcXLS..T)
  m.cAddress4 = ALLTRIM(&lcXLS..U)
  m.cAddress5 = ALLTRIM(&lcXLS..V)
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
PARAMETERS lcNo,lcLogMsg
PRIVATE lcMessage,lcDoffOrd
*lcDoffOrd = ALLTRIM(m.CDOFFORDER) + "/" + ALLTRIM(m.CDIVISION)
STORE '' TO lcMessage
DO CASE  
CASE lcNo == '1'
  lcMessage = "This file can not be selected you must select a file of .Xls type."
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  =gfModalGen("INM00000B00000","DIALOG",'','',lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '2'
  lcMessage = "Invalid Excel file format, You have to save your excel file as excel sheet Ver 4.0"
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  =gfModalGen("INM00000B00000","DIALOG",'','',lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '3'
  lcMessage = "There is another file with the same name of the selected Excel file , Please rename."
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  =gfModalGen("INM00000B00000","DIALOG",'','',lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '4'  
  lcDoffOrd = ALLTRIM(m.CDOFFORDER) + "/" + ALLTRIM(m.CDIVISION)
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  *lcMessage = "Doff order# &lcDoffOrd has been imported before. Order Skipped"
  lcMessage = ", Doff order# &lcDoffOrd has been imported before. Order Skipped"
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '5'
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  *lcMessage = "Style " +lcStyle+" Not found. Order Skipped"
  lcMessage = ", Style " +lcStyle+" Not found. Order Skipped"
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '6'   
  lcDoffOrd = ALLTRIM(m.CDOFFORDER) + "/" + ALLTRIM(m.CDIVISION)
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  *lcMessage = "Store " + ALLTRIM(lcStore) + " is not found for Customer " +ALLTRIM(&lcXLS..C) + " ). order &lcDoffOrd will be Skipped"
  lcMessage = ", Store " + ALLTRIM(lcStore) + " is not found for Customer " +ALLTRIM(&lcXLS..C) + " ). order &lcDoffOrd will be Skipped"
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '7'
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  *lcMessage = " Customer " + ALLTRIM(CUSTOMER.ACCOUNT) +" is not active "
  lcMessage = ", Customer " + ALLTRIM(CUSTOMER.ACCOUNT) +" is not active "
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '8'  
  lcDoffOrd = ALLTRIM(m.CDOFFORDER) + "/" + ALLTRIM(m.CDIVISION)
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  *lcMessage = "Doff PO &lcDoffOrd has been already entered before."
  lcMessage = ", Doff PO &lcDoffOrd has been already entered before."
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),lcMessage)
  *B040008,1 MMR.[End]
CASE lcNo == '9'  
  lcDoffOrd = ALLTRIM(m.CDOFFORDER) + "/" + ALLTRIM(m.CDIVISION)
  lcMessage = "Doff Order# &lcDoffOrd has been already entered, and the user chose to overwrite."
  *B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  =gfModalGen("INM00000B00000","DIALOG",'','',lcMessage)
  *B040008,1 MMR.[End]
ENDCASE
IF !EMPTY(lcLogMsg)
  INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),lcLogMsg)
ENDIF
*B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
*IF !EMPTY(lcMessage)
*B040008,1 MMR,[End]
  *INSERT INTO (lcLogFile) (NRECNO,CERROR) VALUES (RECNO(LCXLS),lcMessage)
*B040008,1 MMR 12/13/2005 Show both the Regected Orders with the reason and the accepted Order.
  *=gfModalGen("INM00000B00000","DIALOG",'','',lcMessage)
*ENDIF
*B040008,1 MMR.[End]
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
  DO CASE
  CASE laItemSeg[lnCount,1]='F'
    lnStyLen  = LEN(laItemSeg[lnCount,3])
    lnStyStPos = laItemSeg[lnCount,4]
    lcStySpr = ALLT(laItemSeg[lnCount,6])

  CASE laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLTRIM(laItemSeg[lnCount,6])

  CASE laItemSeg[lnCount,1]='S'
    lnScaLen   = LEN(laItemSeg[lnCount,3])
    lnScaStPos = laItemSeg[lnCount,4]

  ENDCASE
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
Private lcReturn,lcScale
STORE '' TO lcReturn,lcScale
lcScale = ALLTRIM(&LCXLS..AT)
IF !EMPTY(lcScale)
*  lcReturn = PADR(ALLTRIM(&lcXls..AU),lnStyLen) + lcStySpr + PADR(ALLTRIM(&lcXls..AV),lnClrLen) + lcClrSpr + PADR(lcScale,lnScaLen)
  lcReturn = PADR(ALLTRIM(&lcXls..AU),lnStyLen) + lcStySpr + PADR(ALLTRIM(&lcXls..AV),lnClrLen)
  lcReturn = PADR(UPPER(lcReturn),19)
ENDIF
RETURN lcReturn
*-- End of Function lfGetStyle.

*!*************************************************************
*! Name      : lfGtOrder
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Read order number manually
*!*************************************************************
*! Calls     : ARORDER.SPX
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  Order#
*!*************************************************************
*! Example            :  =lfGtOrder()
*!*************************************************************
FUNCTION lfGtOrder
PRIVATE lcOrderNo

STORE SPACE(6) TO lcOrderNo
*B802231,1 Consider order type when enter order# manualy
*DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo'
DO (gcScrDir+"ARORDER.SPX") WITH 'O','lcOrderNo',lcOrdType
*B802231,1 (End)

RETURN(lcOrderNo)

*B040008,1 MMR 12/13/2005 2)Call all the functions in the SOUPDATE.[Start]
*!*************************************************************
*! Name        : lfSavScr
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Save new or modified order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSavScr()
*!*************************************************************
FUNCTION lfSavScr
*C102818,1  TMI [Start] Add a new parameter to prevent the message box of saved order being shown
*PARAMETERS llFromEDI
PARAMETERS llFromEDI,llNoMsgBx
*C102818,1  TMI [End  ] 

*-- llFromEDI  :- Variable hold true or false in case call save from EDI module.
*-- lnCanBulk  :- Variable hold cancel qty from Depleted bulk orders Quantity.
*-- lnCancel   :- Variable hold cancel qty from Depleted bulk orders Quantity.
*-- lnCancelAmt:- Variable hold cancel amount from Depleted bulk orders Quantity.
*-- lnLineCount:- Variable hold number of lines in order line.
*-- lcHdrAlo   :- Variable hold CutTktH Or PosHdr File if we have alocated qty.
*-- lcDetAlo   :- Variable hold CutTktL Or PosLn File if we have alocated qty.

llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)
IF llFromEDI
  llMFDsPrc = .F.
  llPODsPrc = .F.
ENDIF
*wab
PRIVATE lnCanBulk,lnCancel,lnCancelAmt,lnLineCount,lcHdrAlo,lcDetAlo,;
        lcBulkYear,lcBulkPrd

STORE ' ' TO lcBulkYear,lcBulkPrd

*--B605198,1 MHM add new var to chak if we change warehouse[Start]
STORE .F. TO llChgWh
*--B605198,1 MHM [End]

lnMessage = 0
IF SEEK('M'+laData[2],'Customer') .AND. Customer.Status = 'P' 
  IF laData[5] <> 'B'
    lcMessage = ALLTRIM(laOrdStatus[lnOrdStatus])+' while the customer is Potential. If you keep the order as '+;
                ALLTRIM(laOrdStatus[lnOrdStatus])
    lnMessage = gfModalGen("QRM32085B32008","DIALOG",lcMessage)
    IF lnMessage =1  
      =SEEK('M'+laData[2],'Customer')
      SELECT Customer
      IF gfObj_Lock(.T.)
        REPLACE Customer.Status WITH "A"
        =gfObj_Lock(.F.)
      ELSE
        =gfModalGen("INM32087B00000","DIALOG",'account')
        laData[5] = 'B'
      ENDIF
    ELSE
      laData[5] = 'B'
    ENDIF
  ENDIF  
ENDIF

*E301621,1 Check the CRM setup . [Begin]
IF &lcOrdHdr..lFromWeb  AND 'CR' $ gcCmpModules 
  lcConfMail = gfGetMemVar('M_CONFMAIL')  
  lcSOAPR    = gfGetMemVar('M_SOAPR')
  lcSOCAN    = gfGetMemVar('M_SOCAN')
  
  IF OrdHdr.Status $ 'HO' AND  OrdHdr.cOrdType = 'O'  AND laData[5] = 'X'  
    IF EMPTY(lcConfMail) 
      * Error message to User  
      =gfModalGen('TRM32090B00000','DIALOG')
      llcSave = .F.
      SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
      RETURN
    ENDIF 
    * If the User Cancelled the Web Order     
    IF EMPTY(lcSOCAN) 
      IF gfModalGen('QRM32092B32000','DIALOG') =2
        llcSave = .F.
        SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
        RETURN
      ENDIF
    ENDIF   
  ENDIF
  
  IF OrdHdr.Status = 'B' AND  OrdHdr.cOrdType = 'T' AND laData[5] <> 'B'
    IF EMPTY(lcConfMail) 
      * Error message to User  
      =gfModalGen('TRM32090B00000','DIALOG')
      llcSave = .F.
      SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
      RETURN
    ENDIF 
   
    * If the User approved(Open/Hold) the Web Order   
    IF EMPTY(lcSOAPR) AND laData[5] <> 'X'  
      IF gfModalGen('QRM32091B32000','DIALOG') =2
        llcSave = .F.
        SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
        RETURN
      ENDIF
    ENDIF
  ENDIF
ENDIF
*E301621,1 Check the CRM setup . [End]

SELECT(lcOrdLine)
GOTO TOP
SCAN
  *C200311,1 TMI [START] if no change in lcOrdline.item_no get style.cvensty value.
  IF ASCAN(laEvntTrig , PADR('UPDITMNO',10)) <> 0
    PRIVATE llOrdLn
    llOrdLn = .F.
    =gfDoTriger('SOORD',PADR('UPDITMNO',10))
  ENDIF
  *C200311,1 TMI [end  ] if no change in lcOrdline.item_no get style.cvensty value.
  IF !EMPTY(STORE) .AND. SEEK('S'+Account+Store,'Customer') ;
                   .AND. Customer.Status = 'P'
    IF laData[5] <> 'B'
      lcMessage = ALLTRIM(laOrdStatus[lnOrdStatus])+' while some stores are Potential. If you keep the order as '+;
                  ALLTRIM(laOrdStatus[lnOrdStatus])+', the Potential Stores will be changed to Active. Otherwise, Keep the Potential Stores'
      IF lnMessage = 0
        lnMessage = gfModalGen("QRM32086B32009","DIALOG",lcMessage)
      ENDIF
      IF lnMessage =1
        =SEEK('S'+Account+Store,'Customer')
        SELECT Customer
        IF gfObj_Lock(.T.)
          REPLACE Customer.Status WITH "A"
          =gfObj_Lock(.F.)
        ELSE
          =gfModalGen("INM32087B00000","DIALOG",'store')
          laData[5] = 'B'
        ENDIF
      ELSE
        laData[5] = 'B'
      ENDIF
    ENDIF  
  ENDIF
ENDSCAN 

*B802670,1 AMM If total open quantity doesn't equal zero and there are no shipped quantity, don't accept
*IF !SEEK(lcOrdType,lcOrdLine) .OR. laData[41] = 0
IF !SEEK(lcOrdType,lcOrdLine) .OR. (laData[41] = 0  .AND. laData[37] = 0)
  *B802670,1 AMM end
  *E300420,1 Message : 32044
  *E300420,1 No lines entered! No updates performed.
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32044B00000','DIALOG')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF

*-- don't update in case the exchange rate less than or equal  zero.
IF laData[34] <=0
  *E300420,1 Message : 32028
  *E300420,1 The exchange rate must be greater than zero
  *E300420,1 Button : 00000 
  *E300420,1 Ok
  =gfModalGen('TRM32028B00000','ALERT')
  llcSave = .F.
  SELECT (IIF(lnActFolder=2,lcOrdLine,lcOrdHdr))
  RETURN
ENDIF
*B607436,1  TMI [Start] Overcome the error "Numeric overflow"
IF lfDivByZr()
  llcSave = .F.
  RETURN
ENDIF   
*B607436,1  TMI [End  ] 
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. !CHECKPRD(laData[8],'lcGlYear','lcGlPeriod','',.T.)
  STORE SPACE(4) TO lcGlYear
  STORE SPACE(2) TO lcGlPeriod
ENDIF
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[4] .AND. SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')
  =CHECKPRD(OrdHdr.Entered,'lcBulkYear','lcBulkPrd','',.T.)
ENDIF
SET ORDER TO TAG 'ORDHDR' IN 'ORDHDR'
SET ORDER TO TAG 'ORDLINE' IN 'ORDLINE'


*E301288,1 Reham On 07/20/1999   *** Begin ***
*E301288,1 Reham On 07/20/1999   *** End   ***

IF ASCAN(laEvntTrig , PADR('ORD_SAV',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('ORD_SAV',10))
  IF !llReturn
    llCSave = .F.
    RETURN .F.
  ENDIF
ENDIF     

IF ASCAN(laEvntTrig , PADR('ALO_RES',10)) <> 0
  llReturn = gfDoTriger('SOORD',PADR('ALO_RES',10))
ENDIF
IF ASCAN(laEvntTrig , PADR('COMPLDAT',10)) <> 0
  PRIVATE llReturn , ldCompDate , lcPack
  ldCompDate = laData[10]
  lcPack = '' 
  llReturn = gfDoTriger('SOORD',PADR('COMPLDAT',10))
  IF !llReturn
    RETURN .F.
  ENDIF
  
  *-- If Empty (Complete date not found in Packs file.)
  IF EMPTY(lcPack)
    llCSave = .F.
    RETURN .F.
  ELSE
    PRIVATE lcOldAlias 
    lcOldAlias = ALIAS()                           && Save current alias.
    
    SELECT (lcOrdLine)
    REPLACE ALL cPack WITH lcPack
    
    SELECT (lcOldAlias)                            && Restore current alias.
  ENDIF
ENDIF     

*C200236,1 MHM (Begin) Save for MBI.
IF ASCAN(laEvntTrig , PADR('SAVSCR',10)) <> 0
  llReturn =gfDoTriger('SOORD',PADR('SAVSCR',10))
  IF llReturn
    llCSave = .T.
    RETURN 
  ENDIF
ENDIF
*C200236,1 (End)

*- If we are in the edit mode.
IF laScrMode[3]
  DO CASE
    CASE laData[5] <> OrdHdr.STATUS .AND. laData[5] = "O" .AND. ASCAN(laEvntTrig , PADR('CHG_OPN',10)) <> 0
      llReturn = gfDoTriger('SOORD',PADR('CHG_OPN',10))
    CASE laData[5] <> OrdHdr.STATUS .AND. laData[5] = "H" .AND. ASCAN(laEvntTrig , PADR('CHG_HLD',10)) <> 0
      llReturn = gfDoTriger('SOORD',PADR('CHG_HLD',10))
    OTHERWISE
      DECLARE laOrdInfo[1]
      =gfSubStr(ALLTRIM(lcScFields) , @laOrdInfo , ',')
      llModOrd = .F.
      SELECT OrdHdr
      FOR lnOrdCnt = 1 TO ALEN(laOrdInfo)
        IF EVALUATE(laOrdInfo[lnOrdCnt]) <> laData[lnOrdCnt]
          llModOrd = .T.
          EXIT
        ENDIF
      ENDFOR
      IF llModOrd .AND. ASCAN(laEvntTrig , PADR('MOD_ORD',10)) <> 0
        llReturn = gfDoTriger('SOORD',PADR('MOD_ORD',10))
      ENDIF
  ENDCASE
ENDIF
*E500377,1 Generate numbers automatically for EDI orders
*laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
            IIF(laSetups[6,2]='Y',lfGtOrder(),gfSequence('ORDER','','',laData[15])))
laData[1] = IIF((llContinue AND !EMPTY(laData[1])) OR laScrMode[3],laData[1],;
            IIF(laSetups[6,2]='Y' AND !llFromEDI,lfGtOrder(),gfSequence('ORDER','','',laData[15])))
*E500377,1 (End)
            
lcCurrOrd = laData[1]

*C123847,1  TMI [Start] Add a trigger to update the order charge file , 
*                       also update the styhist file with the used employee units
*-IF laScrMode[4] .AND. ASCAN(laEvntTrig , PADR('CHRGORD',10)) <> 0
IF ASCAN(laEvntTrig , PADR('CHRGORD',10)) <> 0
  =gfDoTriger('SOORD',PADR('CHRGORD',10))
ENDIF
*C123847,1  TMI [End  ] 

*E301288,1 Reham On 07/20/1999   *** Begin ***
*E301288,1 Replace the order # in all the adornment order lines.
IF llBomVarnt .AND. laScrMode[4] 
  SELECT (lcOrdLine)
  lnRecNo = RECNO()
  GO TOP

  *C102231,1 If Customer J&L replace Order no in Temp BomVar with laData[1]. [Begin]
  *SCAN
  *  SELECT (lcT_BomVar)
  *  REPLACE ALL cCost_Id WITH laData[1] ;
  *  FOR cIdType+cCost_ID+STR(LineNo,6)="SO"+ &lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
  *ENDSCAN
  IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0
    SELECT (lcT_BomVar)
    REPLACE ALL cCost_Id WITH laData[1]
  ELSE
    SCAN
      SELECT (lcT_BomVar)
      REPLACE ALL cCost_Id WITH laData[1] ;
      FOR cIdType+cCost_ID+STR(LineNo,6)="SO"+ &lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
    ENDSCAN  
  ENDIF  
  *C102231,1 If Customer J&L replace Order no in Temp BomVar with laData[1]. [End]

  IF BETWEEN(lnRecNo, 1, RECCOUNT())
    GO lnRecNo
  ENDIF
  *-delete any record without order no and pack The temp file 
  SELECT (lcT_BomVar)
  DELETE FOR EMPTY(cCost_Id)
  PACK
ENDIF
*E301288,1 Reham On 07/20/1999   *** End   ***

IF !llFromEDI
  =gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
  SELECT UnCmSess
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)

  REPLACE cCurrObj WITH 'pbSav'
  
ENDIF    && End of IF !llFromEDI

SELECT (lcOrdHdr)
lcEdiOrd = Order
=RLOCK()
REPLACE Order WITH laData[1]
UNLOCK
SHOW GET laData[1] DISABLE
*-- update the allocated quantity in case we come in edit mode.
*C037957,1 MHM 10/10/2004 Custom Amend to CutPick File[Start]
IF ASCAN(laEvntTrig , PADR('SOCHGSTY',10)) <> 0
  =gfDoTriger('SOORD',PADR('SOCHGSTY',10))
ENDIF
*C037957,1 MHM [End]
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. USED(lcAlocated)
    *C037957,1 MHM 10/10/2004 Custom Amend to CutPick File[Start]
  IF ASCAN(laEvntTrig , PADR('SOUPSTY',10)) <> 0
    =gfDoTriger('SOORD',PADR('SOUPSTY',10))
  ELSE
  *C037957,1 MHM [End]
    SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
    SELECT (lcAlocated)
    GO TOP
    IF !EOF()
      WAIT 'Updating alocated quantity...' WINDOW NOWAIT
      IF 'PO' $ gcCmpModules  && if we have style purchase order.
        =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
        =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
      ENDIF
      IF 'MF' $ gcCmpModules && if we have munf. module.
        =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
        =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
      ENDIF
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPKORD','SH')
    
      *B804386,1 Change the order of OrdLine File. [Begin]
      PRIVATE lcOrdLnOrd , lnOrdLnRec
      SELECT (lcOrdLine)
      lnOrdLnRec = RECNO()
      lcOrdLnOrd = TAG()
      SET ORDER TO TAG OrdLine    
      *B804386,1 Change the order of OrdLine File. [End]
    
      SELECT (lcAlocated)
      DO WHILE !EOF()
        lcKey = TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE
        SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE = lcKey
          lcHdrAlo = IIF(TranCd='1','CutTktH','PosHdr')
          lcDetAlo = IIF(TranCd='1','CutTktL','PosLn')
          =SEEK(lcKey,'CUTPICK')
          SCATTER MEMVAR
          SELECT (lcHdrAlo)
          =SEEK(IIF(m.TranCd='1',m.CTKTNO,'P'+m.CTKTNO))
          =RLOCK()
          IF &lcAlocated..nSteps < 1
            REPLACE TotOrd WITH TotOrd - CUTPICK.TotQty + m.TotQty
            SELECT (lcAlocated)
            =RLOCK()
            REPLACE nSteps WITH 1
            UNLOCK
            SELECT (lcHdrAlo)
          ENDIF
          IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 2
            *B604394,1 Get the Qtys that need to be updated in PO/CT Header. [Begin]
            *IF m.TranCd = '1'
            *  REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty + m.TotQty ,;
            *          Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty + m.TotQty
            *ELSE
            *  REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty + m.TotQty ,;
            *          Open      WITH Open  - CUTPICK.TotQty + m.TotQty
            *ENDIF
          
            PRIVATE lnQtyUpd , lnLoop , lcLoopStr , lnUpdSNo
            lnQtyUpd = CutPick.TotQty  
            lnUpdSNo = LEN(ALLTRIM(&lcAlocated..cUpdSizes))
            FOR lnLoop = 1 TO lnUpdSNo 
              lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
              lcLoopStr = ALLTRIM(lcLoopStr)
              lnQtyUpd = lnQtyUpd - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
            ENDFOR
            IF m.TranCd = '1'
              REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty + lnQtyUpd ,;
                      Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty + lnQtyUpd
            ELSE
              REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty + lnQtyUpd ,;
                      Open      WITH Open  - CUTPICK.TotQty + lnQtyUpd
            ENDIF
            *B604394,1 Get the Qtys that need to be updated in PO/CT Header. [End]
        
            *B604394,1 Update the WIP & WO in Style file. [Begin]
            IF SEEK(&lcAlocated..Style,'Style')
              PRIVATE lnLoop , lcLoopStr
              FOR lnLoop = 1 TO lnUpdSNo
                lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
                lcLoopStr = ALLTRIM(lcLoopStr)
                SELECT Style
                =RLOCK()
                REPLACE WIP&lcLoopStr WITH WIP&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                        TOTWIP WITH TOTWIP - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                        NWO&lcLoopStr WITH NWO&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ; 
                        NTOTWO WITH NTOTWO - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
                UNLOCK
              ENDFOR
            ENDIF  
            *B604394,1 Update the WIP & WO in Style file. [End]
          
            *B604540,1 Update the WIP & WO in StyDye file. [Begin]
           
            *B804386,1 Seek with Warehouse from Order Line instead of header. [Begin]
            *IF SEEK(&lcAlocated..Style + laData[31]  ,'StyDye')
            *B605681,1 TMI [START] Remove the Str() Function 
            *IF SEEK(lcOrdType+&lcAlocated..Order+STR(ALLTRIM(&lcAlocated..cOrdLine),6),lcOrdLine) AND ;
               SEEK(&lcAlocated..Style + &lcOrdLine..cWareCode  ,'StyDye')
            IF SEEK(lcOrdType+&lcAlocated..Order+PADL(ALLTRIM(&lcAlocated..cOrdLine),6),lcOrdLine) AND ;
               SEEK(&lcAlocated..Style + &lcOrdLine..cWareCode  ,'StyDye')
            *B605681,1 TMI [END  ] Remove the Str() Function 
            *B804386,1 Seek with Warehouse from Order Line instead of header. [End]
              PRIVATE lnLoop , lcLoopStr
              FOR lnLoop = 1 TO lnUpdSNo
                lcLoopStr = SUBSTR(&lcAlocated..cUpdSizes,lnLoop,1)
                lcLoopStr = ALLTRIM(lcLoopStr)
                SELECT StyDye
                =RLOCK()
                REPLACE WIP&lcLoopStr WITH WIP&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                        TOTWIP WITH TOTWIP - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ;
                        NWO&lcLoopStr WITH NWO&lcLoopStr - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr , ; 
                        NTOTWO WITH NTOTWO - CutPick.Qty&lcLoopStr + &lcAlocated..Qty&lcLoopStr
                UNLOCK
              ENDFOR
            ENDIF  
            *B604540,1 Update the WIP & WO in Style file. [End]
  
            SELECT (lcAlocated)
            =RLOCK()
            REPLACE nSteps WITH 2
            UNLOCK
            SELECT (lcHdrAlo)
          ENDIF
          UNLOCK
          SELECT (lcDetAlo)
          =SEEK(IIF(m.TranCd='2','P','')+m.CTKTNO+m.Style+m.cTktLineNo+'1')
          =RLOCK()
          IF &lcAlocated..nSteps < 3
            REPLACE ORD1   WITH ORD1   - CUTPICK.Qty1   + m.Qty1 ,;
                    ORD2   WITH ORD2   - CUTPICK.Qty2   + m.Qty2 ,;
                    ORD3   WITH ORD3   - CUTPICK.Qty3   + m.Qty3 ,;
                    ORD4   WITH ORD4   - CUTPICK.Qty4   + m.Qty4 ,;
                    ORD5   WITH ORD5   - CUTPICK.Qty5   + m.Qty5 ,;
                    ORD6   WITH ORD6   - CUTPICK.Qty6   + m.Qty6 ,;
                    ORD7   WITH ORD7   - CUTPICK.Qty7   + m.Qty7 ,;
                    ORD8   WITH ORD8   - CUTPICK.Qty8   + m.Qty8 ,;
                    TOTORD WITH TOTORD - CUTPICK.TotQty + m.TotQty
            SELECT (lcAlocated)
            =RLOCK()
            REPLACE nSteps WITH 3
            UNLOCK
            SELECT (lcDetAlo)
          ENDIF
          IF !EMPTY(&lcAlocated..cUpdSizes) .AND. &lcAlocated..nSteps < 4
          
            *B604394,1 Update TotQty in PO Lines/Ct Lines. [Begin]
            *REPLACE Qty1   WITH IIF('1' $ &lcAlocated..cUpdSizes,Qty1-CUTPICK.Qty1+m.Qty1,Qty1) ,;
            *        Qty2   WITH IIF('2' $ &lcAlocated..cUpdSizes,Qty2-CUTPICK.Qty2+m.Qty2,Qty2) ,;
            *        Qty3   WITH IIF('3' $ &lcAlocated..cUpdSizes,Qty3-CUTPICK.Qty3+m.Qty3,Qty3) ,;
            *        Qty4   WITH IIF('4' $ &lcAlocated..cUpdSizes,Qty4-CUTPICK.Qty4+m.Qty4,Qty4) ,;
            *        Qty5   WITH IIF('5' $ &lcAlocated..cUpdSizes,Qty5-CUTPICK.Qty5+m.Qty5,Qty5) ,;
            *        Qty6   WITH IIF('6' $ &lcAlocated..cUpdSizes,Qty6-CUTPICK.Qty6+m.Qty6,Qty6) ,;
            *        Qty7   WITH IIF('7' $ &lcAlocated..cUpdSizes,Qty7-CUTPICK.Qty7+m.Qty7,Qty7) ,;
            *        Qty8   WITH IIF('8' $ &lcAlocated..cUpdSizes,Qty8-CUTPICK.Qty8+m.Qty8,Qty8) ,;
            *        TotQty WITH TotQty - CUTPICK.TotQty + m.TotQty
           
            REPLACE Qty1   WITH IIF('1' $ &lcAlocated..cUpdSizes,Qty1-CUTPICK.Qty1+m.Qty1,Qty1) ,;
                    Qty2   WITH IIF('2' $ &lcAlocated..cUpdSizes,Qty2-CUTPICK.Qty2+m.Qty2,Qty2) ,;
                    Qty3   WITH IIF('3' $ &lcAlocated..cUpdSizes,Qty3-CUTPICK.Qty3+m.Qty3,Qty3) ,;
                    Qty4   WITH IIF('4' $ &lcAlocated..cUpdSizes,Qty4-CUTPICK.Qty4+m.Qty4,Qty4) ,;
                    Qty5   WITH IIF('5' $ &lcAlocated..cUpdSizes,Qty5-CUTPICK.Qty5+m.Qty5,Qty5) ,;
                    Qty6   WITH IIF('6' $ &lcAlocated..cUpdSizes,Qty6-CUTPICK.Qty6+m.Qty6,Qty6) ,;
                    Qty7   WITH IIF('7' $ &lcAlocated..cUpdSizes,Qty7-CUTPICK.Qty7+m.Qty7,Qty7) ,;
                    Qty8   WITH IIF('8' $ &lcAlocated..cUpdSizes,Qty8-CUTPICK.Qty8+m.Qty8,Qty8) ,;
                    TotQty WITH TotQty - CUTPICK.TotQty + lnQtyUpd                  
            *B604394,1 Update TotQty in PO Lines/Ct Lines. [End]
  
            SELECT (lcAlocated)
            =RLOCK()
            REPLACE nSteps WITH 4
            UNLOCK
            SELECT (lcDetAlo)
          ENDIF
          UNLOCK
          SELECT CUTPICK
          =RLOCK()
          IF m.TOTQTY = 0
            DELETE
          ELSE
            =RLOCK()
            GATHER MEMVAR
            UNLOCK
          ENDIF
          UNLOCK
        ENDSCAN
      ENDDO
      =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
      =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
      =gfCloseFile('CUTPICK')

      *B804386,1 Change the order of OrdLine File. [Begin]
      SELECT (lcOrdLine)
      SET ORDER TO TAG &lcOrdLnOrd
      IF BETWEEN(RECNO(),1,RECCOUNT())
        GOTO lnOrdLnRec
      ENDIF
      *B804386,1 Change the order of OrdLine File. [End]
    ENDIF
 *C037957,1 MHM 10/10/2004 Custom Amend to CutPick File[Start]
 ENDIF
 *C037957,1 MHM [ENd]
ENDIF
=gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
=IIF('AL' $ gcCmpModules,gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'PikTkt','SH'),.T.)
=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH') 
IF lcOrdType='O' AND laScrMode[4] AND !EMPTY(&lcOrdHdr..cFromOrder) AND ;
  ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
ENDIF

IF ASCAN(laEvntTrig , PADR('SAV_PAY',10)) <> 0
  =gfDoTriger('SOORD',PADR('SAV_PAY',10))
ENDIF

IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3]
  SELECT (lcOrdLine)
  SCAN
    IF Picked .AND. SEEK(PikTkt,'PikTkt')
      SELECT PikTkt
      SET ORDER TO TAG Ordpik
      SCAN FOR Order+Piktkt = EVAL(lcOrdHdr+'.Order')
        =RLOCK() 
        REPLACE CustPo WITH laData[4]
        UNLOCK
      ENDSCAN  
      SET ORDER TO TAG Piktkt
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcOrdLine)

*-B605198,1 MHM if warehouse changed then check if warehouse assigend or not [start]
LOCATE FOR TotPik > 0 OR TotCut > 0  .OR.  &lcOrdHdr..ship >0
llChgWh = IIF( FOUND() , .F., .T.) 
IF llChgWh
  PRIVATE lnAlias
  lnAlias = SELECT()
  SELECT (lcOrdLine)
  SCAN
    IF laSetups[5,2]='Y' .AND. !SEEK(&lcOrdLine..Style+laData[31]+SPACE(10),'StyDye')
      *E300408,1 Message : 40012
      *E300408,1 Style/color xxx is not assigned to warehouse xxx
      *E300408,1 Button : 40002
      *E300408,1 Add Reenter
      DO gpAdStyWar WITH &lcOrdLine..Style,SPACE(10),laData[31]
    ENDIF
    REPLACE &lcOrdLine..cwarecode WITH laData[31],;
            &lcOrdLine..Flag      WITH 'M'
  ENDSCAN  
  SELECT(lnAlias)
ENDIF  
*-B605198,1 MHM [End]

SELECT ORDLINE
SET RELATION TO
SELECT (lcOrdLine)
SET DELETE OFF
SET ORDER TO TAG 0
GO TOP
WAIT 'Updating order lines...' WINDOW NOWAIT
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

STORE 0 TO lnCancel,lnCancelAmt,lnCanBulk
lnLineCount=IIF(SEEK(lcOrdType+laData[1],'ORDHDR'),ORDHDR.LastLine,0)

SCAN FOR IIF(OrdHdr.Status='B',.T.,Flag='M' .OR. (Flag='N' .AND. !DELETED() .AND. TotQty > 0))

  *-- Decrease Depleted bulk orders from warehouse Ordered Quantity 
  IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. !DELETED() .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 1 .AND. ;
    SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine') .AND.;
    SEEK(OrdLine.Style+OrdLine.cWareCode+SPACE(10),'StyDye')
    SELECT StyDye
    =RLOCK('StyDye')
    REPLACE Ord1 WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2 WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3 WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4 WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5 WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6 WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7 WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8 WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK IN 'StyDye'
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF

  *-- Update Warehouse Ordered Quantity
  *B804386,1 Change order header warehouse to Order Line warehouse. [Begin]
  *IF lcOrdType='O' .AND. laData[5]<>'B' .AND. nSteps < 2 .AND. ;
  *   SEEK(Style+laData[31]+SPACE(10),'StyDye')
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. nSteps < 2 .AND. ;
     SEEK(Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
  *B804386,1 Change order header warehouse to Order Line warehouse. [End]

    =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

    llStyChgd = (&lcOrdLine..Style<>OrdLine.Style)
    
    *B605198,1 MHM if ware house changed then call update[Star]
    *IF llStyChgd 
    *B607411,1  TMI [Start] Use variable "llChgWh" only if line is not deleted
    *IF llStyChgd OR llChgWh
    IF llStyChgd OR ( llChgWh .AND. !DELETED() )
    *B607411,1  TMI [End  ] 

    *B605198,1 MHM if ware house changed then call update[End]
    
      =lfOrdQUpd('STYDYE')
    ELSE
      
      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT StyDye                           
      =RLOCK('StyDye')
      REPLACE Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty1,0),;
              Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty2,0),;
              Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty3,0),;
              Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty4,0),;
              Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty5,0),;
              Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty6,0),;
              Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty7,0),;
              Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty8,0),;
              TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)+;
                                   IIF(!DELETED(lcOrdLine),&lcOrdLine..TotQty,0)
      UNLOCK IN 'StyDye'
    ENDIF
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF

  *-- Decrease Depleted bulk orders from Style Ordered Quantity 
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. !DELETED() .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. nSteps < 3 .AND.;
     SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'ORDLINE') .AND. ;
     SEEK(OrdLine.Style,'Style')
    SELECT Style 
    =RLOCK('Style')
    REPLACE Ord1   WITH Ord1 - MIN(OrdLine.Qty1,&lcOrdLine..Qty1),;
            Ord2   WITH Ord2 - MIN(OrdLine.Qty2,&lcOrdLine..Qty2),;
            Ord3   WITH Ord3 - MIN(OrdLine.Qty3,&lcOrdLine..Qty3),;
            Ord4   WITH Ord4 - MIN(OrdLine.Qty4,&lcOrdLine..Qty4),;
            Ord5   WITH Ord5 - MIN(OrdLine.Qty5,&lcOrdLine..Qty5),;
            Ord6   WITH Ord6 - MIN(OrdLine.Qty6,&lcOrdLine..Qty6),;
            Ord7   WITH Ord7 - MIN(OrdLine.Qty7,&lcOrdLine..Qty7),;
            Ord8   WITH Ord8 - MIN(OrdLine.Qty8,&lcOrdLine..Qty8),;
            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
    UNLOCK IN 'Style'
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

  *-- Update Style Ordered Quantity 
  IF lcOrdType='O' AND laData[5]<>'B' AND nSteps < 4 AND SEEK(Style,'Style')
    =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

    llStyChgd = (&lcOrdLine..Style<>OrdLine.Style)
    IF llStyChgd
      =lfOrdQUpd('STYLE')
    ELSE
      *-- For modified order line : Substract order original quantity and add order new quantity
      *-- For deleted lines       : Substract order original quantity
      *-- For orders that was bid : Add order quantity 
      SELECT Style
      =RLOCK('Style')
      REPLACE  Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty1,0),;
               Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty2,0),;
               Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty3,0),;
               Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty4,0),;
               Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty5,0),;
               Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty6,0),;
               Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty7,0),;
               Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..Qty8,0),;
               TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)+;
                                    IIF(!DELETED(lcOrdLine),&lcOrdLine..TotQty,0)
      UNLOCK IN 'Style'
    ENDIF  
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF

  *-- Cancel Depleted bulk orders Quantity 
  IF lcOrdType='O' .AND. laData[5] <> 'B' .AND. ;
     (laScrMode[4] .OR. OrdHdr.Status='B') .AND. !DELETED() .AND. ;
     SEEK(lcOrdType+cFromOrder+STR(BulkLineNo,6),'OrdLine')
    lnCanBulk = MIN(OrdLine.Qty1,Qty1)+MIN(OrdLine.Qty2,Qty2)+;
                MIN(OrdLine.Qty3,Qty3)+MIN(OrdLine.Qty4,Qty4)+;
                MIN(OrdLine.Qty5,Qty5)+MIN(OrdLine.Qty6,Qty6)+;
                MIN(OrdLine.Qty7,Qty7)+MIN(OrdLine.Qty8,Qty8)
    lnCancel    = lnCancel    + lnCanBulk
    lnCancelAmt = lnCancelAmt + lnCanBulk*OrdLine.Price
    *B606257,1 SSH (Start), Fix Numeric Overflow problem.
    laData[50] = IIF(laData[50]=0,1,laData[50])
    *B606257,1 SSH (END).
    lnOpmAmnt = lnCanBulk*OrdLine.Price &lcExRSin laData[34] &lcUntSin laData[50]

    *-- Decrease Depleted bulk orders Quantity & Amount in the Style History file
    IF SEEK(Style+lcBulkYear,'icStyHst') .AND. &lcOrdLine..nSteps < 5
      lnOrdAmt = lnOpmAmnt &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCanBulk ,;
              nOrdQty           WITH nOrdQty           - lnCanBulk ,;
              nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt  ,;
              nOrdAmt           WITH nOrdAmt           - lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    
    *-- Update Bulk order line open and book quantity
    IF nSteps < 6
      =RLOCK('ORDLINE')
      SELECT ORDLINE
      =RLOCK()
      REPLACE Qty1   WITH MAX(Qty1-&lcOrdLine..Qty1,0),;
              Qty2   WITH MAX(Qty2-&lcOrdLine..Qty2,0),;
              Qty3   WITH MAX(Qty3-&lcOrdLine..Qty3,0),;
              Qty4   WITH MAX(Qty4-&lcOrdLine..Qty4,0),;
              Qty5   WITH MAX(Qty5-&lcOrdLine..Qty5,0),;
              Qty6   WITH MAX(Qty6-&lcOrdLine..Qty6,0),;
              Qty7   WITH MAX(Qty7-&lcOrdLine..Qty7,0),;
              Qty8   WITH MAX(Qty8-&lcOrdLine..Qty8,0),;
              Totqty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      REPLACE Book1   WITH MAX(Book1-&lcOrdLine..Book1,0),;
              Book2   WITH MAX(Book2-&lcOrdLine..Book2,0),;
              Book3   WITH MAX(Book3-&lcOrdLine..Book3,0),;
              Book4   WITH MAX(Book4-&lcOrdLine..Book4,0),;
              Book5   WITH MAX(Book5-&lcOrdLine..Book5,0),;
              Book6   WITH MAX(Book6-&lcOrdLine..Book6,0),;
              Book7   WITH MAX(Book7-&lcOrdLine..Book7,0),;
              Book8   WITH MAX(Book8-&lcOrdLine..Book8,0),;
              TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
      UNLOCK IN 'ORDLINE'
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
  ENDIF
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[3] .AND. ;
     Picked .AND. SEEK(PikTkt,'PikTkt')
    SELECT PikTkt
    =RLOCK() 
    REPLACE Store  WITH &lcOrdLine..Store ,;
            CustPo WITH &lcOrdLine..CustPo
    UNLOCK
    SELECT (lcOrdLine)
  ENDIF
  =SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')

  *-- Update orders Quantity & Amount in the Style History file
  IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(Style+lcGlYear,'icStyHst') .AND. ;
     &lcOrdLine..nSteps < 7
    
    *B606257,1 SSH (Start), Fix Numeric Overflow problem.
    IF OrdHdr.nCurrUnit = 0 .OR. laData[50] = 0
      PRIVATE lnPreAls
      lnPreAls = SELECT(0)
      SELECT OrdHdr
      REPLACE nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit)
      FLUSH
      laData[50] = IIF(laData[50]=0,1,laData[50])
      SELECT (lnPreAls)
    ENDIF
    *B606257,1 SSH (END).
        
    lnOrdAmt = ;
    IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]) - ;
    IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdLine.TotQty*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)

    SELECT icStyHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - ;
            IIF(OrdHdr.Status='B',0,OrdLine.TotQty) + ;
            IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdQty WITH nOrdQty- IIF(OrdHdr.Status='B',0,OrdLine.TotQty) + ;
            IIF(DELETED(lcOrdLine),0,&lcOrdLine..TotQty) ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
            nOrdAmt WITH nOrdAmt + lnOrdAmt
    UNLOCK
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 7
    UNLOCK
  ENDIF
  *E300956,1 Update Order lines canceled quantity
  IF laScrMode[3] AND &lcOrdLine..nSteps < 8 AND SEEK(lcOrdType+Order+STR(LineNo,6),lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCATTER TO laCanRec
    INSERT INTO ORDCANLN FROM ARRAY laCanRec
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF
  *E300956,1 (End)

  SELECT (lcOrdLine)
  IF lcOrdType='O' AND laScrMode[4] AND 'MF' $ gcCmpModules AND ;
     !EMPTY(&lcOrdHdr..cFromOrder) .AND. &lcOrdLine..nSteps < 9
    SCATTER FIELDS ;
            Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
    SELECT CUTPICK
    =SEEK('1'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6))
    SCAN REST ;
         WHILE TRANCD+ORDER+CORDLINE='1'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) ;
         AND laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
             laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0
      m.CtktLineNo = CtktLineNo
      m.CtktNo = CtktNo
      m.Cut1 = Qty1
      m.Cut2 = Qty2
      m.Cut3 = Qty3
      m.Cut4 = Qty4
      m.Cut5 = Qty5
      m.Cut6 = Qty6
      m.Cut7 = Qty7
      m.Cut8 = Qty8
      =RLOCK()            
      REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
              Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
              Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
              Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
              Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
              Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
              Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
              Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK 
      IF TotQty = 0
        DELETE
      ENDIF
      m.Cut1 = MIN(m.Cut1,laDistQty[1])
      m.Cut2 = MIN(m.Cut2,laDistQty[2])
      m.Cut3 = MIN(m.Cut3,laDistQty[3])
      m.Cut4 = MIN(m.Cut4,laDistQty[4])
      m.Cut5 = MIN(m.Cut5,laDistQty[5])
      m.Cut6 = MIN(m.Cut6,laDistQty[6])
      m.Cut7 = MIN(m.Cut7,laDistQty[7])
      m.Cut8 = MIN(m.Cut8,laDistQty[8])
      m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
      lnRecNo  = RECNO('CUTPICK')
      SELECT CUTPICK
      SET ORDER TO TAG CUTPKORD
      
      *B604301,1 Correcting cTktLintNo to be cTktLineNo. [Begin]   
      *IF !SEEK('1'+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6))
      *  INSERT INTO 'CUTPICK' ;
      *  (CTKTNO,CTKTLINTNO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
      *  (m.CTKTNO,m.cTktLineNo,'1',laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
      *ENDIF   
      IF !SEEK('1'+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6))
        INSERT INTO 'CUTPICK' ;
        (CTKTNO,CTKTLINENO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
        (m.CTKTNO,m.cTktLineNo,'1',laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
      ENDIF   
      *B604301,1 Correcting cTktLintNo to be cTktLineNo. [End]


      REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
              Qty2   WITH Qty2   + m.Cut2 ,;
              Qty3   WITH Qty3   + m.Cut3 ,;
              Qty4   WITH Qty4   + m.Cut4 ,;
              Qty5   WITH Qty5   + m.Cut5 ,;
              Qty6   WITH Qty6   + m.Cut6 ,;
              Qty7   WITH Qty7   + m.Cut7 ,;
              Qty8   WITH Qty8   + m.Cut8 ,;
              TotQty WITH TotQty + m.TotCut
      SET ORDER TO TAG CUTORD
      GOTO lnRecNo
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE Cut1   WITH Cut1+m.Cut1 ,;
              Cut2   WITH Cut2+m.Cut2 ,;
              Cut3   WITH Cut3+m.Cut3 ,;
              Cut4   WITH Cut4+m.Cut4 ,;
              Cut5   WITH Cut5+m.Cut5 ,;
              Cut6   WITH Cut6+m.Cut6 ,;
              Cut7   WITH Cut7+m.Cut7 ,;
              Cut8   WITH Cut8+m.Cut8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      UNLOCK
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotCut
      UNLOCK
      IF SEEK('O'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
        SELECT ORDLINE
        =RLOCK()
        REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
        UNLOCK
        IF SEEK('O'+&lcOrdHdr..cFromOrder,'ORDHDR')
          SELECT ORDHDR
          =RLOCK()
          REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
          UNLOCK
        ENDIF
      ENDIF
      laDistQty[1] = laDistQty[1] - m.Cut1
      laDistQty[2] = laDistQty[2] - m.Cut2
      laDistQty[3] = laDistQty[3] - m.Cut3
      laDistQty[4] = laDistQty[4] - m.Cut4
      laDistQty[5] = laDistQty[5] - m.Cut5
      laDistQty[6] = laDistQty[6] - m.Cut6
      laDistQty[7] = laDistQty[7] - m.Cut7
      laDistQty[8] = laDistQty[8] - m.Cut8
    ENDSCAN
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 9
    UNLOCK
  ENDIF
  SELECT (lcOrdLine)
  IF lcOrdType='O' AND laScrMode[4] AND 'PO' $ gcCmpModules .AND. ;
     !EMPTY(&lcOrdHdr..cFromOrder) AND &lcOrdLine..nSteps < 10
    SCATTER FIELDS ;
            Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laDistQty
    SELECT CUTPICK
    =SEEK('2'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6))
    SCAN REST ;
         WHILE TRANCD+ORDER+CORDLINE='2'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6) ;
         AND laDistQty[1]+laDistQty[2]+laDistQty[3]+laDistQty[4]+;
             laDistQty[5]+laDistQty[6]+laDistQty[7]+laDistQty[8] >0
      m.CtktLineNo = CtktLineNo
      m.CtktNo = CtktNo
      m.Cut1 = Qty1
      m.Cut2 = Qty2
      m.Cut3 = Qty3
      m.Cut4 = Qty4
      m.Cut5 = Qty5
      m.Cut6 = Qty6
      m.Cut7 = Qty7
      m.Cut8 = Qty8
      =RLOCK()            
      REPLACE Qty1   WITH MAX(Qty1-laDistQty[1],0) ,;
              Qty2   WITH MAX(Qty2-laDistQty[2],0) ,;
              Qty3   WITH MAX(Qty3-laDistQty[3],0) ,;
              Qty4   WITH MAX(Qty4-laDistQty[4],0) ,;
              Qty5   WITH MAX(Qty5-laDistQty[5],0) ,;
              Qty6   WITH MAX(Qty6-laDistQty[6],0) ,;
              Qty7   WITH MAX(Qty7-laDistQty[7],0) ,;
              Qty8   WITH MAX(Qty8-laDistQty[8],0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      UNLOCK 
      IF TotQty = 0
        DELETE
      ENDIF
      m.Cut1 = MIN(m.Cut1,laDistQty[1])
      m.Cut2 = MIN(m.Cut2,laDistQty[2])
      m.Cut3 = MIN(m.Cut3,laDistQty[3])
      m.Cut4 = MIN(m.Cut4,laDistQty[4])
      m.Cut5 = MIN(m.Cut5,laDistQty[5])
      m.Cut6 = MIN(m.Cut6,laDistQty[6])
      m.Cut7 = MIN(m.Cut7,laDistQty[7])
      m.Cut8 = MIN(m.Cut8,laDistQty[8])
      m.TotCut = m.Cut1+m.Cut2+m.Cut3+m.Cut4+m.Cut5+m.Cut6+m.Cut7+m.Cut8
      lnRecNo  = RECNO('CUTPICK')
      SELECT CUTPICK
      SET ORDER TO TAG CUTPKORD

      IF !SEEK('2'+m.CtktNo+m.cTktLineNo+laData[1]+&lcOrdLine..STYLE+STR(&lcOrdLine..LINENO,6))
        INSERT INTO 'CUTPICK' ;
        (CTKTNO,CTKTLINENO,TRANCD,ORDER,CORDLINE,STYLE) VALUES ;
        (m.CTKTNO,m.cTktLineNo,'2',laData[1],STR(&lcOrdLine..LineNo,6),&lcOrdLine..STYLE)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Cut1 ,;
              Qty2   WITH Qty2   + m.Cut2 ,;
              Qty3   WITH Qty3   + m.Cut3 ,;
              Qty4   WITH Qty4   + m.Cut4 ,;
              Qty5   WITH Qty5   + m.Cut5 ,;
              Qty6   WITH Qty6   + m.Cut6 ,;
              Qty7   WITH Qty7   + m.Cut7 ,;
              Qty8   WITH Qty8   + m.Cut8 ,;
              TotQty WITH TotQty + m.TotCut
      SET ORDER TO TAG CUTORD
      GOTO lnRecNo
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE Cut1   WITH Cut1+m.Cut1 ,;
              Cut2   WITH Cut2+m.Cut2 ,;
              Cut3   WITH Cut3+m.Cut3 ,;
              Cut4   WITH Cut4+m.Cut4 ,;
              Cut5   WITH Cut5+m.Cut5 ,;
              Cut6   WITH Cut6+m.Cut6 ,;
              Cut7   WITH Cut7+m.Cut7 ,;
              Cut8   WITH Cut8+m.Cut8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
      UNLOCK
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotCut
      UNLOCK
      IF SEEK('O'+&lcOrdHdr..cFromOrder+STR(&lcOrdLine..BulkLineNo,6),'ORDLINE')
        SELECT ORDLINE
        =RLOCK()
        REPLACE Cut1   WITH MAX(Cut1-m.Cut1,0) ,;
                Cut2   WITH MAX(Cut2-m.Cut2,0) ,;
                Cut3   WITH MAX(Cut3-m.Cut3,0) ,;
                Cut4   WITH MAX(Cut4-m.Cut4,0) ,;
                Cut5   WITH MAX(Cut5-m.Cut5,0) ,;
                Cut6   WITH MAX(Cut6-m.Cut6,0) ,;
                Cut7   WITH MAX(Cut7-m.Cut7,0) ,;
                Cut8   WITH MAX(Cut8-m.Cut8,0) ,;
                TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8
        UNLOCK
        IF SEEK('O'+&lcOrdHdr..cFromOrder,'ORDHDR')
          SELECT ORDHDR
          =RLOCK()
          REPLACE TotCut WITH MAX(TotCut - m.TotCut,0)
          UNLOCK
        ENDIF
      ENDIF
      laDistQty[1] = laDistQty[1] - m.Cut1
      laDistQty[2] = laDistQty[2] - m.Cut2
      laDistQty[3] = laDistQty[3] - m.Cut3
      laDistQty[4] = laDistQty[4] - m.Cut4
      laDistQty[5] = laDistQty[5] - m.Cut5
      laDistQty[6] = laDistQty[6] - m.Cut6
      laDistQty[7] = laDistQty[7] - m.Cut7
      laDistQty[8] = laDistQty[8] - m.Cut8
    ENDSCAN
    SELECT (lcOrdLine)
    =RLOCK()
    REPLACE nSteps WITH 10
    UNLOCK
  ENDIF
   
  SCATTER MEMVAR MEMO

  m.Start    = laData[9]
  m.Complete = IIF(!llCDPerL .OR. EMPTY(m.Complete),laData[10],m.Complete )
  m.Flag     = SPACE(1)

  m.Order    = laData[1]
  *B121559,1 NNA 02/18/2004 (Begin) Save the new GL_sales Code in the order lines
  *B125564,1 NNA 12/29/2004 (BEGIN) USE Alltrim for (laData[53]) and Alltrim for (Style.cSlsGlLink)
  *m.Gl_Sales = ALLTRIM(laData[53]+Style.cSlsGlLink)  
  m.Gl_Sales = ALLTRIM(laData[53])+ALLTRIM(Style.cSlsGlLink)  
  *B125564,1 NNA (END)
  *B121559,1 NNA (End)
  DO CASE
    CASE !llFromEDI AND !DELETED() .AND. !SEEK(lcOrdType+laData[1]+STR(LineNo,6),'OrdLine')
      lnLineCount=lnLineCount+1
      m.LineNo = lnLineCount
      INSERT INTO ORDLINE FROM MEMVAR
      IF SEEK(lcOrdType+laData[1],'ORDHDR')
        SELECT ORDHDR
        REPLACE LastLine WITH lnLineCount
      ENDIF
    CASE (llFromEDI .AND. SEEK('T'+Order+STR(LineNo,6),'OrdLine') ) .OR. ;
         (!llFromEDI AND !DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine'))
      m.cOrdType = IIF(llFromEDI,'O',m.cOrdType)
      lnLineCount = IIF(llFromEDI,m.LineNo,lnLineCount)
      SELECT ORDLINE
      
      IF llMFDsPrc .OR. llPODsPrc
        lnOldPrc = Price 
        lnOldQty = TotQty
        GATHER MEMVAR MEMO
        lnOldQty = IIF(llUpdAlo,TotQty,lnOldQty)
        =lfUpdtPoCT(Order,Style,STR(LineNo,6),TotQty,Price,lnOldPrc,lnOldQty,.F.)
      ELSE
        GATHER MEMVAR MEMO
      ENDIF
      
    CASE DELETED() .AND. SEEK(lcOrdType+Order+STR(LineNo,6),'OrdLine')
      SELECT ORDLINE

      lcOrder  = Order
      lclineNo = STR(LineNo,6)
      lcStyle  = Style
      lnTotQty = 0
      lnPrice  = 0
      lnOldPrc = Price
      lnOldQty = TotQty
      DELETE
      IF llMFDsPrc .OR. llPODsPrc
        =lfUpdtPoCT(lcOrder,lcStyle,lclineNo,lnTotQty,lnPrice,lnOldPrc,lnOldQty,.T.)      
      ENDIF
  ENDCASE
  
  SELECT (lcOrdLine)
  =RLOCK()
  REPLACE Order  WITH laDAta[1] ,;
          LineNo WITH m.LineNo
  UNLOCK
ENDSCAN

*C200311,1 TMI [START] if no change in lcOrdline.item_no get style.cvensty value.
IF ASCAN(laEvntTrig , PADR('UPDITMNO',10)) <> 0
  llOrdLn = .T.
  =gfDoTriger('SOORD',PADR('UPDITMNO',10))
ENDIF
*C200311,1 TMI [end  ] if no change in lcOrdline.item_no get style.cvensty value.


  SELECT ORDLINE
  IF SEEK(lcOrdType+laData[1])
           
    *B604577,1 Saving Cust PO in OrdLine if not Multi Store. [Begin]
    *REPLACE REST Start WITH laData[9] , Complete WITH IIF(llCDPerL,Complete,laData[10]) ;
    *       WHILE cOrdType+Order=lcOrdType+laData[1]
    
    IF laData[7]
      REPLACE REST Start WITH laData[9] , Complete WITH IIF(llCDPerL,Complete,laData[10]) ;
                   WHILE cOrdType+Order=lcOrdType+laData[1]
    ELSE                
      REPLACE REST Start WITH laData[9] , Complete WITH IIF(llCDPerL,Complete,laData[10]) , ;
                   CustPo WITH laData[4] WHILE cOrdType+Order=lcOrdType+laData[1]
    ENDIF               
    *B604577,1 Saving Cust PO in OrdLine if not Multi Store. [End]
    
    
    IF ASCAN(laEvntTrig , PADR('COMPLDAT',10)) <> 0
      = SEEK(lcOrdType+laData[1])
      REPLACE REST cPack WITH lcPack WHILE cOrdType+Order=lcOrdType+laData[1]
    ENDIF     
    
    *C102255,1 If Customer J & L replace Temp Ordline with cLevel , cLabel. [Begin] 
    IF ASCAN(laEvntTrig , PADR('SOJLMER',10)) <> 0
      = gfDoTriger('SOORD',PADR('SOJLMER',10))
    ENDIF     
    *C102255,1 If Customer J & L replace Temp Ordline with cLevel , cLabel. [End]

    *C200424,1 If GMA [Begin] 
    IF ASCAN(laEvntTrig , PADR('SOCUSTPO',10)) <> 0
      = gfDoTriger('SOORD',PADR('SOCUSTPO',10))
    ENDIF     
    *C200424,1  [End]
  ENDIF

*E301288,1 Reham On 07/20/1999   *** Begin ***
*E301288,1 Call global function to update the bomvar file.

IF llBomVarnt
  *C102231,1 If Customer J&L save data to BomVar & Custom thread color file. [Begin]
  *=gfTmp2Mast('BOMVAR' , lcT_BomVar , 'Update the style positions for the order lines...')
  IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0  
    PRIVATE lcDelete 
    lcDelete = SET('DELETE')  
    SET DELETE ON 
    =gfDoTriger('SOORD',PADR('DOSCRN',10)) 
    SET DELETE &lcDelete
  ELSE
    *-- The following line is used to save to Standard BomVar file.
    =gfTmp2Mast('BOMVAR' , lcT_BomVar , 'Update the style positions for the order lines...')  
  ENDIF  
  *C102231,1 If Customer J&L save data to BomVar & Custom thread color file. [End]
ENDIF
 
*E301288,1 Reham On 07/20/1999   *** End   ***

=gfCloseFile('icStyHst')
=gfCloseFile('ORDCANLN')
=IIF('AL' $ gcCmpModules,gfCloseFile('PikTkt'),.T.)
IF lcOrdType='O' AND laScrMode[4] AND !EMPTY(&lcOrdHdr..cFromOrder) AND ;
  ('PO' $ gcCmpModules .OR. 'MF' $ gcCmpModules)
  =gfCloseFile('CUTPICK')
ENDIF

SET DELETE ON
WAIT 'Updating order header...' WINDOW NOWAIT

=gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')

SELECT ORDHDR
*wam
*IF !SEEK(lcOrdType+laData[1])
*  INSERT INTO ORDHDR (cOrdType,Order) VALUES (lcOrdType,laData[1])
*ENDIF

*B606257,1 SSH [Start] Set OrdHdr and order line index to the correct tag.
SELECT ORDLINE
SET ORDER TO ORDLINE
SELECT ORDHDR
SET ORDER TO ORDHDR
LOCATE
*--- Try to fix the record pointer if SEEK is failed and we are in the EDIT mode.
PRIVATE llRecFnd
IF SEEK(lcOrdType+laData[1])
  llRecFnd = .T.
ELSE
  llRecFnd =  laScrMode[3]
  IF llRecFnd
    llRecFnd =  lfFxPointr(lcOrdType,laData[1],.T.)
    IF !llFromEDI .AND. !llRecFnd
      =gfModalGen('INM00000B00000','DIALOG','','', 'A problem is detected in the Order Header Index. Please update the file indices for file “ORDHDR.DBF”.')
    ENDIF
  ENDIF
ENDIF
*B606257,1 SSH [End].

*B606257,1 SSH [Start]. Commented out to add condition to be sure that the order does not exist.
*IF !llFromEDI .AND. !SEEK(lcOrdType+laData[1])
IF !llFromEDI .AND. !llRecFnd .AND. !SEEK(lcOrdType+laData[1]) 
*B606257,1 SSH [End].

  INSERT INTO ORDHDR (cOrdType,Order) VALUES (lcOrdType,laData[1])
  *B605641,1 SSH Fix the bug that SEEK command does not work.
  FLUSH
  *B605641,1 SSH [END]
ENDIF
IF llFromEDI .AND. SEEK('T'+lcEdiOrd)
  REPLACE cOrdType WITH 'O' ,;
          Order    WITH laData[1]
ENDIF
*B605641,1 SSH Fix the bug that SEEK command do not work.
PRIVATE lnHdrRecNo
lnHdrRecNo = RECNO()
*B605641,1 SSH [END]

*wam

IF ASCAN(laEvntTrig , PADR('SAV_CUST',10)) <> 0
  =gfDoTriger('SOORD',PADR('SAV_CUST',10))
ENDIF


*-- Update orders Quantity & Amount in the Customer History file
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. SEEK(laData[2]+lcGlYear,'arCusHst') .AND. ;
   &lcOrdHdr..nSteps < 1

  *B606257,1 SSH (Start), Fix Numeric Overflow problem.
  IF OrdHdr.nCurrUnit = 0 .OR. laData[50] = 0
    PRIVATE lnPreAls
    lnPreAls = SELECT(0)
    SELECT OrdHdr
    REPLACE nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit)
    FLUSH
    laData[50] = IIF(laData[50]=0,1,laData[50])
    SELECT (lnPreAls)
  ENDIF
  *B606257,1 SSH (END).
  
  lnOrdAmt = laData[42] &lcExRSin laData[34] &lcUntSin laData[50] - ;
  IIF(laScrMode[4] OR OrdHdr.Status='B',0,OrdHdr.OpenAmt &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit)
  SELECT arCusHst
  =RLOCK()
  REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod -;
            IIF(OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdQty WITH nOrdQty -;
            IIF(OrdHdr.Status='B',0,OrdHdr.Open) + laData[41],;
          nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
          nOrdAmt WITH nOrdAmt + lnOrdAmt
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
ENDIF


IF lcOrdType='O' .AND. laData[5]<>'B' .AND. laScrMode[4] .AND. ;
   lfFxPointr(lcOrdType,&lcOrdHdr..cFromOrder)
*B605641,1 SSH [END]
  *-- Add bulk orders approve Amount in the header of new order from bulk order .
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE APPRAMT  WITH MIN(&lcOrdHdr..OPENAMT,ORDHDR.APPRAMT)
  UNLOCK

  *-- Decrease bulk orders approve Amount in the header of bulk order.
  SELECT ORDHDR
  =RLOCK()
  REPLACE APPRAMT  WITH MAX(0,APPRAMT - &lcOrdHdr..OPENAMT)
  UNLOCK
ENDIF
*B605641,1 SSH Fix the bug that SEEK command do not work.
*IF lcOrdType='O' .AND. laData[5]<>'B' .AND. (laScrMode[4] .OR. OrdHdr.Status='B') .AND. ;
   SEEK(lcOrdType+&lcOrdHdr..cFromOrder,'OrdHdr')
IF lcOrdType='O' .AND. laData[5]<>'B' .AND. (laScrMode[4] .OR. OrdHdr.Status='B') .AND. ;
   lfFxPointr(lcOrdType,&lcOrdHdr..cFromOrder)
*B605641,1 SSH [END]

  *-- Decrease bulk orders depleted Quantity & Amount in the Customer History file
  IF &lcOrdHdr..nSteps < 2 .AND. SEEK(laData[2]+lcBulkYear,'arCusHst')

    *B606257,1 SSH (Start), Fix Numeric Overflow problem.
    laData[50] = IIF(laData[50]=0,1,laData[50])
    *B606257,1 SSH (END).

    lnOrdAmt = lnCancelAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcBulkPrd WITH nOrdQty&lcBulkPrd - lnCancel ,;
            nOrdQty           WITH nOrdQty           - lnCancel ,;
            nOrdAmt&lcBulkPrd WITH nOrdAmt&lcBulkPrd - lnOrdAmt ,;
            nOrdAmt           WITH nOrdAmt           - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  IF &lcOrdHdr..nSteps < 3
    SELECT ORDHDR
    =RLOCK('ORDHDR')

    IF BULK = 'Y'
      REPLACE Book      WITH Book      - lnCancel   ,;
              BookAmt   WITH BookAmt   - lnCancelAmt,;
              Open      WITH Book      - Cancel     ,;
              Openamt   WITH Bookamt   - Cancelamt  ,;
              Status    WITH IIF(OrdHdr.Open = 0 ,'X',OrdHdr.Status)
   
    ELSE
      REPLACE Cancel    WITH Cancel    + lnCancel   ,;
              Cancelamt WITH Cancelamt + lnCancelAmt,;
              Open      WITH Book      - Cancel     ,;
              Openamt   WITH Bookamt   - Cancelamt  ,;
              Status    WITH IIF(OrdHdr.Open = 0 ,'X',OrdHdr.Status)
    ENDIF
    UNLOCK IN 'ORDHDR'
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF  
  IF OrdHdr.Status='X' .AND. &lcOrdHdr..nSteps < 4 .AND. ;
    SEEK('M'+OrdHdr.Account,'Customer')
    SELECT Customer
    =RLOCK('Customer')
    REPLACE nBulk WITH nBulk - 1
    UNLOCK IN 'Customer'
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
ENDIF
=gfCloseFile('arCusHst')
IF lcOrdType='O' AND laData[5]<>'B' AND (laScrMode[4] OR OrdHdr.Status='B') AND ;
   &lcOrdHdr..Bulk='Y' AND &lcOrdHdr..nSteps < 5 AND SEEK('M'+laData[2],'Customer')
  SELECT Customer
  =RLOCK()
  REPLACE nBulk WITH nBulk + 1
  UNLOCK
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE nSteps WITH 5
  UNLOCK
ENDIF
SELECT ORDHDR
*B605641,1 SSH Fix the bug that SEEK command do not work.
*=SEEK(lcOrdType+laData[1])
=lfFxPointr(lcOrdType,laData[1])
*B605641,1 SSH [END]
=RLOCK()
*B802670,1 AMM If there is shipped quantity, change status to complete
laData[5] = IIF(laData[41] = 0  ,IIF(laData[37]>0,'C','X'),laData[5])
*B802670,1 AMM end
GATHER FROM laData FIELDS &lcScFields

IF ASCAN(laEvntTrig , PADR('COMPLDAT',10)) <> 0    
  PRIVATE lnUsrField
  lnUsrField = ASCAN(laUsrFields,"CPACK")
  IF lnUsrField > 0
    lnUsrField = ASUBSCRIPT(laUsrFields,lnUsrField,1)
    laUsrFields[lnUsrField,6] = lcPack
  ENDIF  
ENDIF     

REPLACE cOrdType   WITH lcOrdType ,;
        LastLine   WITH lnLineCount  ,;
        cFromOrder WITH &lcOrdHdr..cFromOrder ,;
        StName     WITH IIF(laData[51],lcShipName,SPACE(30)) ,;
        cAddress1  WITH IIF(laData[51],lcShipAdd1,SPACE(30)) ,;
        cAddress2  WITH IIF(laData[51],lcShipAdd2,SPACE(30)) ,;
        cAddress3  WITH IIF(laData[51],lcShipAdd3,SPACE(30)) ,;
        cAddress4  WITH IIF(laData[51],lcShipAdd4,SPACE(30)) ,;
        cAddress5  WITH IIF(laData[51],lcShipAdd5,SPACE(30)) ,;
        TotCut     WITH &lcOrdHdr..TotCut                    ,;
        APPRAMT    WITH &lcOrdHdr..APPRAMT 



UNLOCK

IF !llFromEDI
  SELECT unCmSess
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
 
  REPLACE STATUS WITH 'C'
  UNLOCK
  llContinue = .F.
  UNLOCK 
  
ENDIF    && End of IF !llFromEDI

WAIT CLEAR 
IF laScrMode[4] AND !llFromEDI
  *E300420,1 Message : 32045
  *E300420,1 Order has been saved as xxxxxx
  *E300420,1 Button : 00000 
  *E300420,1 Ok

  *C102818,1  TMI [Start] Add a new parameter to prevent the message box of saved order being shown
  *=gfModalGen('INM32045B00000','DIALOG',IIF(lcOrdType='C','Contract','Order')+'|'+laData[1])
  =IIF(llNoMsgBx , .T., gfModalGen('INM32045B00000','DIALOG',IIF(lcOrdType='C','Contract','Order')+'|'+laData[1]) )
  *C102818,1  TMI [End  ] 

ENDIF
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)

*E301086,1 Add new 855 record in the EDI transaction file to be send
IF !llFromEDI AND lcOrdType='O' AND 'EB' $ gcCmpModules
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  IF SEEK('A'+laData[2],'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'855','EDIPD')
    SELECT EDITRANS
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('855'+PADR(laData[1],20)+'A'+laData[2])
    IF !SEEK('855'+PADR(laData[1],40)+'A'+laData[2])
    *E037853,1 [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('855',laData[1],'A',laData[2])
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
  *CP102517 Hassan Add new 850 record in the EDI transaction file to be send  02/04/2002 [Begin]
  IF SEEK('A'+laData[2],'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'850','EDIPD') And EDIPD.cTranType = "S"
    SELECT EDITRANS
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('850'+PADR(laData[1],20)+'A'+laData[2])
    IF !SEEK('850'+PADR(laData[1],40)+'A'+laData[2])
    *E037853,1 [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('850',laData[1],'A',laData[2])
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
  *CP102517 Hassan Add new 850 record in the EDI transaction file to be send  02/04/2002 [End]
  *CP102517 Hassan Add new 850 record in the EDI transaction file to be send  02/04/2002 [Begin]
  IF SEEK('F'+OrdHdr.cFacCode,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'850','EDIPD') And EDIPD.cTranType = "S" 
    SELECT EDITRANS
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('850'+PADR(laData[1],20)+'F'+OrdHdr.cFacCode)
    IF !SEEK('850'+PADR(laData[1],40)+'F'+OrdHdr.cFacCode)
    *E037853,1 [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('850',laData[1],'F',OrdHdr.cFacCode)
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
  *CP102517 Hassan Add new 850 record in the EDI transaction file to be send  02/04/2002 [End]
  *CP102786 Hassan Add new 855 record in the EDI transaction file to be send  03/12/2003 [Begin]
  IF SEEK('F'+OrdHdr.cFacCode,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'855','EDIPD') And;
    INLIST(EDIPD.cTranType ,"S" ,"B")
    SELECT EDITRANS
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF !SEEK('855'+PADR(laData[1],20)+'F'+OrdHdr.cFacCode)
    IF !SEEK('855'+PADR(laData[1],40)+'F'+OrdHdr.cFacCode)
    *E037853,1 [End]
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('855',laData[1],'F',OrdHdr.cFacCode)
    ENDIF
    REPLACE cStatus WITH 'N'
    =gfAdd_Info('EDITRANS')
  ENDIF 
  *CP102786 Hassan Add new 855 record in the EDI transaction file to be send  03/12/2002 [End]
  

  =gfCloseFile('EDIACPRT')
  =gfCloseFile('EDIPD')
  =gfCloseFile('EDITRANS')
ENDIF
*E301086,1 (End)
*E302162,1 Add new 855 Acknowledgement record in the EDI transaction file to be send
IF !llFromEDI AND lcOrdType='T' AND LADATA[62]
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  IF SEEK('A'+laData[2],'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'855','EDIPD')
    SELECT EDITRANS
    lcStatus = 'N'
    *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
    *IF SEEK('855'+PADR(laData[1]+'T',20)+'A'+laData[2])
    IF SEEK('855'+PADR(laData[1]+'T',40)+'A'+laData[2])
    *E037853,1 [End]
      *E302162,1 Message : 32119
      *E302162,1 855 Acknowledgement has already been sent. Do you want to resend it?.
      *E302162,1 Button : 32000
      *E302162,1 Yes/No
      IF cStatus = 'S' AND gfModalGen('QRM32119B32000','ALERT') =2
        lcStatus = 'S'
      ENDIF
    ELSE
      INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) VALUES ;
      ('855',laData[1]+'T','A',laData[2])
    ENDIF
    REPLACE cStatus WITH lcStatus
    =gfAdd_Info('EDITRANS')
  ENDIF 
ENDIF
*E302162,1 End)

*B604825,1 Add flag to check for CRM update. [Begin]
PRIVATE llChanged
llChanged = .F.
*B604825,1 Add flag to check for CRM update. [End]

IF &lcOrdHdr..lFromWeb AND 'CR' $ gcCmpModules AND &lcOrdHdr..cOrdType = "T" AND &lcOrdHdr..Status $ 'HO' AND lcOldStats = 'B'
  IF SEEK("T"+&lcOrdHdr..Order,'OrdLine')
    SELECT OrdLine

    *B604825,1 loop till Order with Order Type "T" is replaced with "O". [Begin]
    *SCAN REST WHILE cOrdType + Order + STR(LineNo,6) = "T" + &lcOrdHdr..Order
    *  =RLOCK()
    *  REPLACE cOrdType WITH "O"
    *  UNLOCK
    *ENDSCAN
    DO WHILE SEEK("T"+&lcOrdHdr..Order,'OrdLine')
      =RLOCK()
      REPLACE cOrdType WITH "O"
      UNLOCK      
    ENDDO
    *B604825,1 loop till Order with Order Type "T" is replaced with "O". [End]
    
    IF SEEK('T'+&lcOrdHdr..Order,'OrdHdr')
      SELECT OrdHdr
      =RLOCK()
      REPLACE cOrdType WITH "O"
      
      *B604825,1 Rise the flag with true. [Begin]
      llChanged = .T.
      *B604825,1 Rise the flag with true. [End]
      
      UNLOCK
    ENDIF
  ENDIF    

  *-- Get the Memory Variables without checking for the CRM module.
  =gfOpenFile(gcDataDir+'CRMesag',gcDataDir+'TransType','SH')

  *B606257,1 SSH [Start] Only to be sure that the APPEND BLANK work on the Correct Alias.
  SELECT CRMesag
  *B606257,1 SSH[End]

  APPEND BLANK
  =gfAdd_Info('CRMesag')
  REPLACE cTransType WITH 'O' , cTransNo WITH &lcOrdHdr..Order , lApprove WITH .T. , ;
          cMailFrom WITH gfGetMemVar('M_NOTEMAIL') , dTransDate WITH gdSysDate, cMailTo WITH ;
          IIF(SEEK('M'+&lcOrdHdr..Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'') , ;
          cMsgSubjct WITH "Your Sales Order# " + &lcOrdHdr..Order + ' has been approved.' , ; 
          cLetterId WITH gfGetMemVar('M_SOAPR') , lSent WITH .F.
  =gfCloseFile('CRMesag')  
ENDIF
*E301621,1 Update the CRMesag file. [End]

*C102433,2 If customer is Cathy Daniel export Sales Order to Old System. [Begin]
IF ASCAN(laEvntTrig , PADR('EXPCTOLD',10)) <> 0
  =gfDoTriger('SOORD',PADR('EXPCTOLD',10))
ENDIF     
*C102433,2 If customer is Cathy Daniel export Sales Order to Old System. [End]

*B604825,1 Return to Select Mode after updating CRM module. [Begin]
IF llChanged
  llWebOrdUp = .T.
ENDIF  
*B604825,1 Return to Select Mode after updating CRM module. [End]
*C102497,1 MHM 01/22/2002 trigger to save [start]
IF ASCAN(laEvntTrig , PADR('SAVLABEL',10)) <> 0
  =gfDoTriger('SOORD',PADR('SAVLABEL',10))
ENDIF
*C102497,1 MHM [start]

*C123847,1  TMI [Start] Call alordal.prg with the order# to allocate

*- note : you should prevent this screen to appear if the user selects the option not to allocate from the mbimain trigger

IF ASCAN(laEvntTrig , PADR('SODIRALO',10)) <> 0 
  =gfDoTriger('SOORD',PADR('SODIRALO',10))
ENDIF
*C123847,1  TMI [End  ] 
*B126657,1 WAM 05/12/2005 Add FLUSH and end of saving Function[Start]
FLUSH
*B126657,1 WAM 05/12/2005 [End]

SELECT ORDHDR
RETURN

*!*************************************************************
*! Name      : lfDelScr
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDelScr()
*!*************************************************************
*!Modifications
*!
*!B801945,1 WAM 02/09/1999 Check if allocation module is installed before
*!B801945,1                use piktkt file.
*!*************************************************************
FUNCTION lfDelScr

PARAMETERS llFromEDI
*C037892,1 MHM Check for OrdBom record Found[Start]
IF ASCAN(laEvntTrig , PADR('SODELBOM',10)) <> 0
  IF !gfDoTriger('SOORD',PADR('SODELBOM',10)) 
    RETURN
  ENDIF
ENDIF
*C037892,1 MHM [End]

*C102323,1 Check if customer is SADIMARA. [Begin]
IF ASCAN(laEvntTrig , PADR('ORDCHECK',10)) <> 0
  IF SEEK(OrdHdr.Order,'InvHdrS')
    *-- Message < This Order has a Pre_Billed invoice, Cannot edit. >
    *-- Buttons <                         OK                        >
    =gfModalGen('INM00000B00000','DIALOG','','', 'This Order has a Pre_Billed invoice, Cannot cancel.')
    RETURN
  ENDIF  
ENDIF  
*C102323,1 Check if customer is SADIMARA. [End]

PRIVATE llDisSdMsg
llDisSdMsg = .F.

llFromEDI = IIF(TYPE('llFromEDI') = 'L' , llFromEDI , .F.)

PRIVATE lnTktCnt


*B127381,1 EIH 05/26/2005 Fix bug of can't cancel orders which not in current period .
*we comment these lines to can cancel orders which not in current period .
*IF lcOrdType='O' &&.AND. !CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod ','',.T.)
*  RETURN
*ENDIF

*B127381,1 EIH 05/26/2005 [End].


*E301621,1 Check the CRM setup . [Begin]
IF OrdHdr.lFromWeb  AND 'CR' $ gcCmpModules 
  lcConfMail = gfGetMemVar('M_CONFMAIL')  
  lcSOAPR    = gfGetMemVar('M_SOAPR')
  lcSOCAN    = gfGetMemVar('M_SOCAN')  

  IF OrdHdr.Status $ 'HOB'
    IF EMPTY(lcConfMail) 
      * Error message to User  
      =gfModalGen('TRM32090B00000','DIALOG')
      RETURN
    ENDIF 
    * If the User Cancelled the Web Order     
    IF EMPTY(lcSOCAN) 
      IF gfModalGen('QRM32092B32000','DIALOG') =2
        RETURN
      ENDIF
    ENDIF   
  ENDIF
  
  * Do Uncancel the Web Order 
  IF OrdHdr.Status = 'X'
    IF EMPTY(lcConfMail) 
      * Error message to User  
      =gfModalGen('TRM32090B00000','DIALOG')
      RETURN
    ENDIF 
    * If the User approved(Open/Hold) the Web Order   
    IF EMPTY(lcSOAPR) 
      IF gfModalGen('QRM32091B32000','DIALOG') =2
        RETURN
      ENDIF
    ENDIF
  ENDIF
ENDIF  
*E301621,1 Check the CRM setup . [End]

lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])
*ahmed
*-- Open the order cancellation table if we have shipped qty.
IF ORDHDR.SHIP > 0
  =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH') 
ENDIF
*ahmed


*-- Uncancel order
IF OrdHdr.Status='X'
  IF lcOrdType='O' .AND. !SEEK(lcOrdType+laData[1],'ORDLINE')
    *E300420,1 Message : 32000
    *E300420,1 The lines for this order are missing! Cannot update cut & sold.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  
  IF ASCAN(laEvntTrig,PADR('DELORD',10)) <> 0
    llReturn = gfDoTriger('SOORD',PADR('DELORD',10))
    llDisSdMsg = llReturn
    IF !llReturn
      RETURN 
    ENDIF
  ENDIF   
 
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR  IN ORDHDR  
  IF !llContinue
    IF !llDisSdMsg 
      *E300420,1 Message : 32002
      *E300420,1 Order is canceled. Uncancel?
      *E300420,1 Button : 32000
      *E300420,1 Yes/No
      IF gfModalGen('QRM32002B32000','ALERT',IIF(lcOrdType='C','Contract','Order')) =2
        RETURN
      ENDIF
    ENDIF
    IF OrdHdr.Cancel = 0 AND OrdHdr.Bulk <> 'Y'
      SELECT ORDHDR
      =RLOCK()
      REPLACE Status     WITH 'B' ,;
              cCancReson WITH SPACE(6) ,;
              Cancelled  WITH {} ,;
              Flag       WITH SPACE(1)
      UNLOCK
      SCATTER FIELDS &lcScFields TO laData

      *B037515,1 MHM 01/22/2004 Change places of Both Functions to void change 
      *B037515,1                mode from laScrMode[2] to laScrMode[1] before get Info [Start]
      *E301621,1 Update the CRMesag file. [Begin]
      *DO lpUpdMesag
      *E301621,1 Update the CRMesag file. [End]
      *=lfGetInfo()           
      =lfGetInfo()           
      DO lpUpdMesag
      
      *B037515,1 MHM 01/22/2004 [End]
      
      RETURN
    ENDIF  
    SELECT *, 00 AS nSteps FROM ORDHDR WHERE cordtype+order=lcOrdType+laData[1];
    INTO DBF (gcWorkDir+lcOrdHdr)
    SELECT *, 00 AS nSteps FROM ORDLINE WHERE cordtype+order+STR(lineno,6)=;
    lcOrdType+laData[1] INTO DBF (gcWorkDir+lcOrdLine)
    
    SELECT 'UNCMSESS'
   
    *B606625,1 NAD (Start)
    *IF SEEK('I')
    lnReprocesX = SET('REPROCESS')
    SET REPROCESS TO 1
    IF SEEK('I') AND RLOCK()
    *B606625,1 NAD (END)    
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    *B606625,1 NAD (Start)
    SET REPROCESS TO lnReprocesX 
    =RLOCK()
    *B606625,1 NAD (END)  
    REPLACE Status     WITH 'O'       ,;
            cUTranType WITH 'SOORD'   ,;
            cUserId    WITH gcUser_id ,;
            cSession   WITH lcSession ,;
            cProgram   WITH 'SOORD'   ,;
            cCurrScr   WITH 'SOORD'   ,;
            cCurrObj   WITH 'PBDLT'   ,;
            dTranDate  WITH gdSysDate ,;
            cTranTime  WITH TIME()
    *B606625,1 NAD (Start)  
    *=RLOCK()
    *B606625,1 NAD (END)  
    lcCurrOrd = laData[1]
    lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
              'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
    =gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
  ENDIF
  IF lcOrdType='O'
    WAIT 'Uncancelling and updating Cut & Sold...' WINDOW NOWAIT
  ENDIF
  STORE 0 TO lnNewOpen,lnNewOAmt
  =gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
  SELECT (lcOrdLine)
  SCAN
    *-- Increase warehouse ordered quantity
    
    *B804386,1 Change order header warehouse to Order Line warehouse. [Begin] 
    *IF lcOrdType='O' .AND. nSteps < 1 .AND. SEEK(Style+laData[31]+SPACE(10),'StyDye')
    IF lcOrdType='O' .AND. nSteps < 1 .AND. SEEK(Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
    *B804386,1 Change order header warehouse to Order Line warehouse. [End]

      SELECT StyDye
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + &lcOrdLine..QTY1 ,;
              ORD2   WITH ORD2 + &lcOrdLine..QTY2 ,;
              ORD3   WITH ORD3 + &lcOrdLine..QTY3 ,;
              ORD4   WITH ORD4 + &lcOrdLine..QTY4 ,;
              ORD5   WITH ORD5 + &lcOrdLine..QTY5 ,;
              ORD6   WITH ORD6 + &lcOrdLine..QTY6 ,;
              ORD7   WITH ORD7 + &lcOrdLine..QTY7 ,;
              ORD8   WITH ORD8 + &lcOrdLine..QTY8 ,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    *-- Increase style ordered quantity
    IF lcOrdType='O' .AND. nSteps < 2 .AND. SEEK(STYLE,'Style')
      SELECT STYLE
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + &lcOrdLine..QTY1 ,;
              ORD2   WITH ORD2 + &lcOrdLine..QTY2 ,;
              ORD3   WITH ORD3 + &lcOrdLine..QTY3 ,;
              ORD4   WITH ORD4 + &lcOrdLine..QTY4 ,;
              ORD5   WITH ORD5 + &lcOrdLine..QTY5 ,;
              ORD6   WITH ORD6 + &lcOrdLine..QTY6 ,;
              ORD7   WITH ORD7 + &lcOrdLine..QTY7 ,;
              ORD8   WITH ORD8 + &lcOrdLine..QTY8 ,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    *-- Increase ordered quantity and amount in the style history file
    IF lcOrdType='O' .AND. nSteps < 3 .AND. SEEK(Style+lcGlYear,'icStyHst')
      *B606257,1 SSH (Start), Fix Numeric Overflow problem.
      laData[50] = IIF(laData[50]=0,1,laData[50])
      *B606257,1 SSH (END).
      lnOrdAmt = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + &lcOrdLine..TotQty ,;
              nOrdQty            WITH nOrdQty            + &lcOrdLine..TotQty ,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt ,;
              nOrdAmt            WITH nOrdAmt            + lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    lnNewOpen = lnNewOpen+ TotQty
    lnNewOAmt = lnNewOAmt+ TotQty*Price
  ENDSCAN
  =gfCloseFile('icStyHst')
  *-- Increase number of bulk orders for this account
  IF laData[23]='Y' .AND. &lcOrdHdr..nSteps < 1 .AND. SEEK('M'+laData[2],'Customer')
    SELECT Customer
    =RLOCK()
    REPLACE nBulk WITH nBulk + 1
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  *-- Update order header cancel and open quantity and amount
  IF &lcOrdHdr..nSteps < 2
    SELECT ORDHDR
    =RLOCK()
    
    *B607262,1 KHM 05/26/2003 (Begin) When canceling the SO get the status from the SO setup
    *REPLACE Status     WITH 'O'      ,;
            cCancReson WITH SPACE(6) ,;
            Cancelled  WITH {} ,;
            Cancel     WITH IIF(BULK='Y',Cancel,Cancel-lnNewOpen) ,;
            CancelAmt  WITH IIF(BULK='Y',CancelAmt,CancelAmt-lnNewOAmt) ,;
            Open       WITH lnNewOpen ,;
            OpenAmt    WITH lnNewOAmt ,;
            Flag       WITH SPACE(1)
    REPLACE Status     WITH IIF(laSetups[7,2] $ 'YF' ,'H','O'),;
            cCancReson WITH SPACE(6) ,;
            Cancelled  WITH {} ,;
            Cancel     WITH IIF(BULK='Y',Cancel,Cancel-lnNewOpen) ,;
            CancelAmt  WITH IIF(BULK='Y',CancelAmt,CancelAmt-lnNewOAmt) ,;
            Open       WITH lnNewOpen ,;
            OpenAmt    WITH lnNewOAmt ,;
            Flag       WITH SPACE(1)
    *B607262,1 KHM 05/26/2003 (End)

    UNLOCK

    IF laData[23]='Y'
      =RLOCK()
      REPLACE BOOK    WITH  Ship + Cancel   + lnNewOpen ,;
              BOOKAMT WITH  ShipAmt + CancelAmt+ lnNewOAmt 
      UNLOCK    
    ENDIF
    
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  *-- Update ordered quantity and amount in the customer history file 
  IF lcOrdType='O' .AND. &lcOrdHdr..nSteps < 3 
    =gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
    =SEEK(laData[2]+lcGlYear,'arCusHst')
    *B606257,1 SSH (Start), Fix Numeric Overflow problem.
    laData[50] = IIF(laData[50]=0,1,laData[50])
    *B606257,1 SSH (END).
    lnOrdAmt = lnNewOAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + lnNewOpen ,;
            nOrdQty            WITH nOrdQty            + lnNewOpen ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + lnOrdAmt  ,;
            nOrdAmt            WITH nOrdAmt            + lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

ELSE
  *C200149,1  ABD 02/20/2001 Add new triger. [Begin]
  IF ASCAN(laEvntTrig , PADR('DLEALOSO',10)) <> 0
    llReturn  =gfDoTriger('SOORD',PADR('DLEALOSO',10))
    IF !llReturn
      RETURN 
    ENDIF
  ENDIF  
  *C200149,1  ABD [End]
  *-- Cancel order
  *-- Do not cancel shipped complete order
  IF ORDHDR.STATUS='C'
    *E300420,1 Message : 32003
    *E300420,1 Order has been shipped complete! Cannot cancel.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32003B00000','ALERT')
    RETURN
  ENDIF
  *-- Do not cancel order with no lines
  IF !(lcOrdType='T' AND ORDHDR.Mon_Flg='L') AND !SEEK(lcOrdType+laData[1],'ORDLINE')
  
    *E300420,1 Message : 32000
    *E300420,1 The lines for this order are missing! Cannot update cut & sold.
    *E300420,1 Button : 00000 
    *E300420,1 Ok
    =gfModalGen('TRM32000B00000','ALERT')
    RETURN
  ENDIF
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR  IN ORDHDR  
  IF !llContinue
    *B801945,1 Check if allocation module is installed.
    IF 'AL' $ gcCmpModules
      =gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH')
       
      *-- Do not cancel picked orders 
      IF SEEK(OrdHdr.Order,'PikTkt')
        SELECT PikTkt
        LOCATE REST WHILE Order = OrdHdr.Order FOR Status $ 'OHP'
        IF FOUND()
          IF ASCAN(laEvntTrig , PADR('TRMTE_CANL',10)) <> 0
            =gfDoTriger('SOORD',PADR('TRMTE_CANL',10))
            RETURN
          ELSE          
            *E300420,1 Message : 32004
            *E300420,1 Some order lines hve been picked.
            *E300420,1 Button : 00000 
            *E300420,1 Ok
            =gfModalGen('INM32004B00000','ALERT')
          ENDIF
        ENDIF
      ENDIF
      =gfOpenFile(gcDataDir+'PIKLINE',gcDataDir+'PIKLINE','SH') 
      SET ORDER TO TAG 'PIKTKT' IN 'PIKTKT'
    ENDIF

    IF ORDHDR.TotCut > 0
      *E300420,1 Message : 32054
      *E300420,1 This order has quantity allocated.
      *E300420,1 Canceling this order will release this allocation.
      *E300420,1 Would you like to continue ? 
      *E300420,1 Button : 32000
      *E300420,1 Yes  No
      IF gfModalGen('QRM32054B32000','ALERT')=2
        RETURN
      ENDIF
      IF 'PO' $ gcCmpModules
        =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
        =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
      ENDIF
      IF 'MF' $ gcCmpModules
        =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
        =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTLIN','SH')
      ENDIF
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
    ENDIF
    
      
    *E300420,1 Message : 32005
    *E300420,1 Cancel all open items on this order?
    *E300420,1 Button : 320000
    *E300420,1 Yes/No
    IF gfModalGen('QRM32005B32000','ALERT',IIF(lcOrdType='C','contract','order')) = 2
      RETURN
    ENDIF
    IF ASCAN(laEvntTrig , PADR('DELSOFT',10)) <> 0
      =gfDoTriger('SOORD',PADR('DELSOFT',10))
    ENDIF     


    IF ORDHDR.STATUS='B'
      SELECT ORDHDR
      =RLOCK()
      REPLACE STATUS     WITH 'X' ,;
              cCancReson WITH lfCanReason() ,;
              Cancelled  WITH gdSysDate ,;
              Flag       WITH SPACE(1)
      UNLOCK
      SCATTER FIELDS &lcScFields TO laData

      *B037515,1 MHM 01/22/2004 Change places of Both Functions to void change 
      *B037515,1                mode from laScrMode[2] to laScrMode[1] before get Info [Start]
      *E301621,1 Update the CRMesag file. [Begin]
      *DO lpUpdMesag
      *E301621,1 Update the CRMesag file. [End]
      *=lfGetInfo()           
      =lfGetInfo()           
      DO lpUpdMesag
      *B037515,1 MHM 01/22/2004 [End]
      
      RETURN
    ENDIF
    lnTktCnt = 0
    DIMENSION laTickets[1,2]

    SELECT *, 00 AS nSteps FROM ORDHDR WHERE cordtype+order=lcOrdType+laData[1];
    INTO DBF (gcWorkDir+lcOrdHdr)
    m.cCancReson = lfCanReason()
    REPLACE cCancReson WITH m.cCancReson
    SELECT *, 00 AS nSteps FROM ORDLINE WHERE cordtype+order+STR(lineno,6)=;
    lcOrdType+laData[1] INTO DBF (gcWorkDir+lcOrdLine)
    *!C037816,1 MHM 04/06/2004 Define New variables[Start]
    IF ASCAN(laEvntTrig,PADR("DLCANORD",10)) <> 0 
      =gfDoTriger("SOORD",PADR("DLCANORD",10))
    ENDIF
    *!C037816,1 MHM [End]
    
    
    SELECT 'UNCMSESS'
    
    *B606625,1 NAD (Start)
    *IF SEEK('I')
    lnReprocesX = SET('REPROCESS')
    SET REPROCESS TO 1
    IF SEEK('I') AND RLOCK()
    *B606625,1 NAD (END)  
      BLANK
    ELSE
      APPEND BLANK
    ENDIF
    *B606625,1 NAD (Start)
    SET REPROCESS TO lnReprocesX 
    =RLOCK()
    *B606625,1 NAD (END)
    REPLACE Status     WITH 'O'       ,;
            cUTranType WITH 'SOORD'   ,;
            cUserId    WITH gcUser_id ,;
            cSession   WITH lcSession ,;
            cProgram   WITH 'SOORD'   ,;
            cCurrScr   WITH 'SOORD'   ,;
            cCurrObj   WITH 'PBDLT'   ,;
            dTranDate  WITH gdSysDate ,;
            cTranTime  WITH TIME()
    *B606625,1 NAD (Start)
    *=RLOCK()
    *B606625,1 NAD (END)
    lcCurrOrd = laData[1]
    lcFiles = 'lcOrdHdr,'+lcOrdHdr+','+lcOrdHdr+';'+;
              'lcOrdLine,'+lcOrdLine+',lcOrdLine;'
    =gfSavSess('SOORD', lcFiles, @laVariables,lcSession)
  ENDIF
  WAIT 'Cancelling and updating Cut & Sold...' WINDOW NOWAIT 
  =gfOpenFile(gcDataDir+'icStyHst',gcDataDir+'Styhst','SH') 
  SELECT (lcOrdLine)
  STORE 0 TO lnOpen,lnOpenAmt,lnBook,lnBookAmt
  SCAN
    *-- Decrease warehouse ordered quantity with order open quantity
    IF lcOrdType='O' .AND. SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
      IF &lcOrdLine..nSteps < 1
        SELECT StyDye
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-&lcOrdLine..Qty1,0) ,;
                Ord2   WITH MAX(Ord2-&lcOrdLine..Qty2,0) ,;
                Ord3   WITH MAX(Ord3-&lcOrdLine..Qty3,0) ,;
                Ord4   WITH MAX(Ord4-&lcOrdLine..Qty4,0) ,;
                Ord5   WITH MAX(Ord5-&lcOrdLine..Qty5,0) ,;
                Ord6   WITH MAX(Ord6-&lcOrdLine..Qty6,0) ,;
                Ord7   WITH MAX(Ord7-&lcOrdLine..Qty7,0) ,;
                Ord8   WITH MAX(Ord8-&lcOrdLine..Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
      *-- Decrease warehouse allocated quantity with order picked quantity
      IF &lcOrdLine..PICKED .AND. &lcOrdLine..nSteps < 2
        SELECT StyDye
        =RLOCK()
        REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
                Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
                Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
                Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
                Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
                Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
                Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
                Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 2
        UNLOCK
      ENDIF
    ENDIF
    *-- Decrease style ordered quantity with order open quantity
    IF lcOrdType='O' .AND. SEEK(&lcOrdLine..STYLE,'Style')
      IF &lcOrdLine..nSteps < 3
        SELECT Style
        =RLOCK()
        REPLACE Ord1   WITH MAX(Ord1-&lcOrdLine..Qty1,0) ,;
                Ord2   WITH MAX(Ord2-&lcOrdLine..Qty2,0) ,;
                Ord3   WITH MAX(Ord3-&lcOrdLine..Qty3,0) ,;
                Ord4   WITH MAX(Ord4-&lcOrdLine..Qty4,0) ,;
                Ord5   WITH MAX(Ord5-&lcOrdLine..Qty5,0) ,;
                Ord6   WITH MAX(Ord6-&lcOrdLine..Qty6,0) ,;
                Ord7   WITH MAX(Ord7-&lcOrdLine..Qty7,0) ,;
                Ord8   WITH MAX(Ord8-&lcOrdLine..Qty8,0) ,;
                TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 3
        UNLOCK
      ENDIF
      *-- Decrease style allocated quantity with order picked quantity
      IF &lcOrdLine..PICKED .AND. &lcOrdLine..nSteps < 4
        SELECT Style
        =RLOCK()
        REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
                Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
                Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
                Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
                Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
                Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
                Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
                Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
        UNLOCK
        SELECT (lcOrdLine)
        =RLOCK()
        REPLACE nSteps WITH 4
        UNLOCK
      ENDIF
    ENDIF
    *B602418,1 Update dyelot allocated quantity
    IF lcOrdType='O' AND &lcOrdLine..nSteps < 5 AND &lcOrdLine..PICKED AND !EMPTY(&lcOrdLine..Dyelot) AND ;
       SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+&lcOrdLine..Dyelot,'StyDye')
      SELECT StyDye
      =RLOCK()
      REPLACE Alo1   WITH MAX(Alo1-&lcOrdLine..Pik1,0) ,;
              Alo2   WITH MAX(Alo2-&lcOrdLine..Pik2,0) ,;
              Alo3   WITH MAX(Alo3-&lcOrdLine..Pik3,0) ,;
              Alo4   WITH MAX(Alo4-&lcOrdLine..Pik4,0) ,;
              Alo5   WITH MAX(Alo5-&lcOrdLine..Pik5,0) ,;
              Alo6   WITH MAX(Alo6-&lcOrdLine..Pik6,0) ,;
              Alo7   WITH MAX(Alo7-&lcOrdLine..Pik7,0) ,;
              Alo8   WITH MAX(Alo8-&lcOrdLine..Pik8,0) ,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    *B602418,1 (End)

    *-- Decrease ordered quantity & amount in the style history with order open quantity 
    IF lcOrdType='O' .AND. &lcOrdLine..nSteps < 6 .AND. SEEK(Style+lcGlYear,'icStyHst')
      *B606257,1 SSH (Start), Fix Numeric Overflow problem.
      laData[50] = IIF(laData[50]=0,1,laData[50])
      *B606257,1 SSH (END).
      lnOrdAmt = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34] &lcUntSin laData[50]
      SELECT icStyHst
      =RLOCK()
      REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - &lcOrdLine..TotQty ,;
              nOrdQty            WITH nOrdQty            - &lcOrdLine..TotQty ,;
              nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt ,;
              nOrdAmt            WITH nOrdAmt            - lnOrdAmt
      UNLOCK
      SELECT (lcOrdLine)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
    *-- Release pick ticket and zero out order picked quantity
    =SEEK(lcOrdType+&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6),'ORDLINE')
    IF &lcOrdLine..Picked
      IF SEEK(&lcOrdLine..PikTkt,'PIKTKT')
        SELECT PIKTKT
        =RLOCK()
        REPLACE STATUS WITH 'X'
        UNLOCK
      ENDIF
      SELECT ORDLINE
      IF !SEEK(PikTkt+Order+STR(LineNo,6),'PIKLINE')      
        SCATTER TO laPikLine
        INSERT INTO PIKLINE FROM ARRAY laPikLine
      ENDIF
      SELECT ORDLINE
      =RLOCK()
      REPLACE PIK1   WITH 0 ,;
              PIK2   WITH 0 ,;
              PIK3   WITH 0 ,;
              PIK4   WITH 0 ,;
              PIK5   WITH 0 ,;
              PIK6   WITH 0 ,;
              PIK7   WITH 0 ,;
              PIK8   WITH 0 ,;
              TOTPIK WITH 0 ,;
              PICKED WITH .F. ,;
              PIKTKT WITH SPACE(6) ,;
              PIKDATE WITH {}
      IF SEEK(AltStyle,'STYLE') .AND. SEEK('S'+STYLE.Scale,'SCALE')
        REPLACE Style     WITH AltStyle,;
                AltStyle  WITH SPACE(19),;
                Scale     WITH SCALE.Scale,;
                cWareCode WITH OrdHdr.cWareCode
        REPLACE Qty1  WITH IIF(Scale.Cnt>=1,Qty1,0)  ,;
                Book1 WITH IIF(Scale.Cnt>=1,Book1,0) ,;
                Qty2  WITH IIF(Scale.Cnt>=2,Qty2,0)  ,;
                Book2 WITH IIF(Scale.Cnt>=2,Book2,0) ,;
                Qty3  WITH IIF(Scale.Cnt>=3,Qty3,0)  ,;
                Book3 WITH IIF(Scale.Cnt>=3,Book3,0) ,;
                Qty4  WITH IIF(Scale.Cnt>=4,Qty4,0)  ,;
                Book4 WITH IIF(Scale.Cnt>=4,Book4,0) ,;
                Qty5  WITH IIF(Scale.Cnt>=5,Qty5,0)  ,;
                Book5 WITH IIF(Scale.Cnt>=5,Book5,0) ,;
                Qty6  WITH IIF(Scale.Cnt>=6,Qty6,0)  ,;
                Book6 WITH IIF(Scale.Cnt>=6,Book6,0) ,;
                Qty7  WITH IIF(Scale.Cnt>=7,Qty7,0)  ,;
                Book7 WITH IIF(Scale.Cnt>=7,Book7,0) ,;
                Qty8  WITH IIF(Scale.Cnt>=8,Qty8,0)  ,;
                Book8 WITH IIF(Scale.Cnt>=8,Book8,0) ,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
                TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
      ENDIF
      UNLOCK
    ENDIF
    *-- Release order allocated quantity to any cutting ticket order style purchae order
    IF ORDHDR.TotCut > 0
      SELECT CutPick
      DO WHILE SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6))
        SELECT IIF(CutPick.TranCd='1','CutTktH','PosHdr')
        =SEEK(IIF(CutPick.TranCd='1','','P')+CutPick.CTKTNO)
        =RLOCK()
        REPLACE TotOrd WITH MAX(TotOrd-CUTPICK.TotQty,0)
        UNLOCK
        SELECT IIF(CutPick.TranCd='1','CutTktL','PosLn')
        =SEEK(IIF(CutPick.TranCd='2','P','')+;
                  CutPick.CTKTNO+CutPick.Style+CutPick.cTktLineNo+'1')
        =RLOCK()
        REPLACE ORD1   WITH MAX(ORD1-CUTPICK.Qty1,0) ,;
                ORD2   WITH MAX(ORD2-CUTPICK.Qty2,0) ,;
                ORD3   WITH MAX(ORD3-CUTPICK.Qty3,0) ,;
                ORD4   WITH MAX(ORD4-CUTPICK.Qty4,0) ,;
                ORD5   WITH MAX(ORD5-CUTPICK.Qty5,0) ,;
                ORD6   WITH MAX(ORD6-CUTPICK.Qty6,0) ,;
                ORD7   WITH MAX(ORD7-CUTPICK.Qty7,0) ,;
                ORD8   WITH MAX(ORD8-CUTPICK.Qty8,0) ,;
                TOTORD WITH MAX(TOTORD-CUTPICK.TotQty,0)
        UNLOCK
        *E300420,1 Message : 32006
        *E300420,1 Cuttkt# xxxxxx status is HOLD. Do you wish to update the generated 
        *E300420,1 cuttkt quantities accordingly?
        *E300420,1 Button : 32000 
        *E300420,1 Yes/No

        *E300420,1 Message : 32007
        *E300420,1 Purchase order# xxxxxx status is HOLD. Do you wish to update the generated 
        *E300420,1 Purchase order quantities accordingly?
        *E300420,1 Button : 32000 
        *E300420,1 Yes/No
  
        *E300420,1 Message : 32008
        *E300420,1 Do you wish to update Purchase order# 999999 quantities accordingly?
        *E300420,1 Button : 32000 
        *E300420,1 Yes/No
        lnTicket=ASCAN(laTickets,CutPick.CTKTNO)
        IF lnTicket = 0
          *llUpdate= (CutPick.TranCd='1' .AND. CUTTKTH.Status='H' .AND. ;
            gfModalGen('QRM32006B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
            (CutPick.TranCd='2' .AND. ( ( POSHDR.Status='H' .AND. ;
             gfModalGen('QRM32007B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
            ( POSHDR.Status $ 'OA' .AND. !laSetups[13,2] .AND. ;
              gfModalGen('QRM32008B32000','ALERT',CutPick.CTKTNO)=1) ))
          llUpdate= (CutPick.TranCd='1' .AND. CUTTKTH.Status='H' .AND. ;
            gfModalGen('QRM32006B32000','ALERT',CutPick.CTKTNO)=1) .OR. ;
                    (CutPick.TranCd='2' .AND. POSHDR.Status='H'  .AND. ;
            gfModalGen('QRM32007B32000','ALERT',CutPick.CTKTNO)=1)

          lnTktCnt=lnTktCnt+1
          DIMENSION laTickets[lnTktCnt,2]
          laTickets[lnTktCnt,1]=CutPick.CTKTNO
          laTickets[lnTktCnt,2]=IIF(llUpdate,'Y','N')
        ELSE
          llUpdate = (laTickets[lnTicket+1]='Y')
        ENDIF
        *-- Update hold cutting tickets order purchase orders budget quantity
        IF llUpdate
          SELECT IIF(CutPick.TranCd='1','CutTktH','PosHdr')
          =RLOCK()
          IF CutPick.TranCd = '1'
            REPLACE Pcs_Bud WITH Pcs_Bud - CUTPICK.TotQty ,;
                    Pcs_Opn WITH Pcs_Opn - CUTPICK.TotQty
          ELSE
            REPLACE nStyOrder WITH nStyOrder - CUTPICK.TotQty ,;
                    Open      WITH Open  - CUTPICK.TotQty
          ENDIF
          UNLOCK
          SELECT IIF(CutPick.TranCd='1','CutTktL','PosLn')
          =RLOCK()
          REPLACE Qty1   WITH Qty1-CUTPICK.Qty1 ,;
                  Qty2   WITH Qty2-CUTPICK.Qty2 ,;
                  Qty3   WITH Qty3-CUTPICK.Qty3 ,;
                  Qty4   WITH Qty4-CUTPICK.Qty4 ,;
                  Qty5   WITH Qty5-CUTPICK.Qty5 ,;
                  Qty6   WITH Qty6-CUTPICK.Qty6 ,;
                  Qty7   WITH Qty7-CUTPICK.Qty7 ,;
                  Qty8   WITH Qty8-CUTPICK.Qty8 ,;
                  TotQty WITH TotQty - CUTPICK.TotQty
          UNLOCK
        ENDIF
        SELECT CUTPICK
        DELETE
      ENDDO
    ENDIF

    *B604482,1 BWA 07/17/2002 Check if the user wants to delete the cuttkt quantities or not 
    *B604482,1 BWA 07/17/2002 and show this quantities in the WIP & Wo folders in the Cut&sold screen in the style screen.[SATRT]
    IF llUpdate
      IF lcOrdType='O' .AND. SEEK(&lcOrdLine..Style+&lcOrdLine..cWareCode+SPACE(10),'StyDye')
        SELECT StyDye
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1-&lcOrdLine..Qty1,0) ,;
                WIP2   WITH MAX(WIP2-&lcOrdLine..Qty2,0) ,;
                WIP3   WITH MAX(WIP3-&lcOrdLine..Qty3,0) ,;
                WIP4   WITH MAX(WIP4-&lcOrdLine..Qty4,0) ,;
                WIP5   WITH MAX(WIP5-&lcOrdLine..Qty5,0) ,;
                WIP6   WITH MAX(WIP6-&lcOrdLine..Qty6,0) ,;
                WIP7   WITH MAX(WIP7-&lcOrdLine..Qty7,0) ,;
                WIP8   WITH MAX(WIP8-&lcOrdLine..Qty8,0) ,;
                TotWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8

        REPLACE NWO1   WITH MAX(NWO1-&lcOrdLine..Qty1,0) ,;
                NWO2   WITH MAX(NWO2-&lcOrdLine..Qty2,0) ,;
                NWO3   WITH MAX(NWO3-&lcOrdLine..Qty3,0) ,;
                NWO4   WITH MAX(NWO4-&lcOrdLine..Qty4,0) ,;
                NWO5   WITH MAX(NWO5-&lcOrdLine..Qty5,0) ,;
                NWO6   WITH MAX(NWO6-&lcOrdLine..Qty6,0) ,;
                NWO7   WITH MAX(NWO7-&lcOrdLine..Qty7,0) ,;
                NWO8   WITH MAX(NWO8-&lcOrdLine..Qty8,0) ,;
                NTotWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8

        UNLOCK
      ENDIF
      *-- Decrease style ordered quantity with order open quantity
      IF lcOrdType='O' .AND. SEEK(&lcOrdLine..STYLE,'Style')
        SELECT Style
        =RLOCK()
        REPLACE WIP1   WITH MAX(WIP1-&lcOrdLine..Qty1,0) ,;
                WIP2   WITH MAX(WIP2-&lcOrdLine..Qty2,0) ,;
                WIP3   WITH MAX(WIP3-&lcOrdLine..Qty3,0) ,;
                WIP4   WITH MAX(WIP4-&lcOrdLine..Qty4,0) ,;
                WIP5   WITH MAX(WIP5-&lcOrdLine..Qty5,0) ,;
                WIP6   WITH MAX(WIP6-&lcOrdLine..Qty6,0) ,;
                WIP7   WITH MAX(WIP7-&lcOrdLine..Qty7,0) ,;
                WIP8   WITH MAX(WIP8-&lcOrdLine..Qty8,0) ,;
                TotWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8

        REPLACE NWO1   WITH MAX(NWO1-&lcOrdLine..Qty1,0) ,;
                NWO2   WITH MAX(NWO2-&lcOrdLine..Qty2,0) ,;
                NWO3   WITH MAX(NWO3-&lcOrdLine..Qty3,0) ,;
                NWO4   WITH MAX(NWO4-&lcOrdLine..Qty4,0) ,;
                NWO5   WITH MAX(NWO5-&lcOrdLine..Qty5,0) ,;
                NWO6   WITH MAX(NWO6-&lcOrdLine..Qty6,0) ,;
                NWO7   WITH MAX(NWO7-&lcOrdLine..Qty7,0) ,;
                NWO8   WITH MAX(NWO8-&lcOrdLine..Qty8,0) ,;
                NTotWO WITH NWO1+NWO2+NWO3+NWO4+NWO5+NWO6+NWO7+NWO8

        UNLOCK
      ENDIF
    ENDIF
    *B604482,1 BWA 07/17/2002.[END] 

    SELECT ORDLINE
    *-- Release any cutting ticket order.
    =RLOCK()
    REPLACE Cut1   WITH 0 ,;
            Cut2   WITH 0 ,;
            Cut3   WITH 0 ,;
            Cut4   WITH 0 ,;
            Cut5   WITH 0 ,;
            Cut6   WITH 0 ,;
            Cut7   WITH 0 ,;
            Cut8   WITH 0 ,;
            TotCut WITH 0
    UNLOCK
    lnOpen    = lnOpen    + OrdLine.TotQty
    lnOpenAmt = lnOpenAmt + OrdLine.TotQty*OrdLine.Price
    lnBook    = lnBook    + OrdLine.TotBook
    lnBookAmt = lnBookAmt + OrdLine.TotBook*OrdLine.Price

    *ahmed
    IF ORDHDR.Ship > 0 .AND. ORDLINE.TOTQTY > 0
      *B606402,1 TMI [Start] The user may cancel some of qty in one session, and cancel the rest of qty when decide to cancel to whole order
      *B606402,1             This seek expression deny adding a new line to ordcanln with the rest of qty , so remove it
      *IF !SEEK(lcOrdType+Order+STR(LineNo,6),'ORDCANLN')
      *B606402,1 TMI [END  ]       

        *INSERT INTO ('ORDCANLN') ;
          (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY) ;
          VALUES ;
          (lcOrdType,laData[1],OrdLine.LineNo,gdSysDate,&lcOrdHdr..cCancReson,OrdLine.Qty1,OrdLine.Qty2,;
          OrdLine.Qty3,OrdLine.Qty4,OrdLine.Qty5,OrdLine.Qty6,OrdLine.Qty7,OrdLine.Qty8,;
          OrdLine.TOTQty)

        *B604500,1 update Price field in Order Cancellation file with OrdLine price. [Begin]
        *INSERT INTO ('ORDCANLN') ;
        *  (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY, ;
        *   ACCOUNT,STYLE,STORE,DYELOT) ;
        *  VALUES ;
        *  (lcOrdType,laData[1],OrdLine.LineNo,gdSysDate,&lcOrdHdr..cCancReson,OrdLine.Qty1,OrdLine.Qty2,;
        *  OrdLine.Qty3,OrdLine.Qty4,OrdLine.Qty5,OrdLine.Qty6,OrdLine.Qty7,OrdLine.Qty8,;
        *  OrdLine.TOTQty,ORDLINE.ACCOUNT,ORDLINE.STYLE,ORDLINE.STORE,ORDLINE.DYELOT)
        INSERT INTO ('ORDCANLN') ;
          (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY, ;
           ACCOUNT,STYLE,STORE,DYELOT,Price) ;
          VALUES ;
          (lcOrdType,laData[1],OrdLine.LineNo,gdSysDate,&lcOrdHdr..cCancReson,OrdLine.Qty1,OrdLine.Qty2,;
          OrdLine.Qty3,OrdLine.Qty4,OrdLine.Qty5,OrdLine.Qty6,OrdLine.Qty7,OrdLine.Qty8,;
          OrdLine.TOTQty,ORDLINE.ACCOUNT,ORDLINE.STYLE,ORDLINE.STORE,ORDLINE.DYELOT,OrdLine.Price)
        *B604500,1 update Price field in Order Cancellation file with OrdLine price. [End]

      *B606402,1 TMI [Start] Comment 
      *ENDIF
      *B606402,1 TMI [END  ] 

      REPLACE QTY1   WITH 0 ,;
              QTY2   WITH 0 ,;
              QTY3   WITH 0 ,;
              QTY4   WITH 0 ,;
              QTY5   WITH 0 ,;
              QTY6   WITH 0 ,;
              QTY7   WITH 0 ,;
              QTY8   WITH 0 ,;
              TOTQTY WITH 0 

    ENDIF
    *ahmed end
    
  ENDSCAN
    *B605455,1 MHM 02/26/2002 get qty from ORDCANLN [Start]
  *-- llOrdCan ----------> check if ORDCANLN is Open Or Not
  llOrdCan  = !USED('ORDCANLN') .AND. gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
  IF SEEK(lcOrdType+laData[1],'ORDCANLN')
    SELECT ORDCANLN
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = lcOrdType+laData[1]
      IF SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
        LOOP
      ENDIF
      lnBook    = lnBook    + ORDCANLN.TotQty
      lnBookAmt = lnBookAmt + ORDCANLN.TotQty*ORDCANLN.Price
    ENDSCAN  
  ENDIF  
  IF llOrdCan  
    =gfCloseFile('ORDCANLN')
  ENDIF  
  *B605455,1 MHM [End]
  =gfCloseFile('icStyHst')
  *-- Decrease number of bulk orders for this account
  IF laData[23]='Y' .AND. &lcOrdHdr..nSteps < 1 .AND. SEEK('M'+laData[2],'Customer')
    SELECT CUSTOMER
    =RLOCK()
    REPLACE nBulk WITH nBulk - 1
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
  *-- Decrease orders quantity and amount in the customerr history file
  IF lcOrdType='O' .AND. &lcOrdHdr..nSteps < 2 
    =gfOpenFile(gcDataDir+'arCusHst',gcDataDir+'Acthst','SH')
    =SEEK(laData[2]+lcGlYear,'arCusHst')
    *B606257,1 SSH (Start), Fix Numeric Overflow problem.
    laData[50] = IIF(laData[50]=0,1,laData[50])
    *B606257,1 SSH (END).
    lnOrdAmt = OrdHdr.OpenAmt &lcExRSin laData[34] &lcUntSin laData[50]
    SELECT arCusHst
    =RLOCK()
    REPLACE nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod - OrdHdr.Open ,;
            nOrdQty            WITH nOrdQty            - OrdHdr.Open ,;
            nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod - lnOrdAmt    ,;
            nOrdAmt            WITH nOrdAmt            - lnOrdAmt
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ENDIF
  =IIF('AL' $ gcCmpModules,gfCloseFile('PIKLINE') AND gfCloseFile('PIKTKT'),.T.)
  IF ORDHDR.TotCut > 0
    =IIF('PO' $ gcCmpModules,gfCloseFile('POSHDR')  AND gfCloseFile('POSLN'),.T.)
    =IIF('MF' $ gcCmpModules,gfCloseFile('CUTTKTH') AND gfCloseFile('CUTTKTL'),.T.)
    =gfCloseFile('CUTPICK')
  ENDIF
  
  *-- Update order open, Cancel and book quantity and amount and order status
  IF &lcOrdHdr..nSteps < 3
    SELECT ORDHDR
    =RLOCK()
    *B602410,1 Update canceled date with system date.

    REPLACE STATUS     WITH IIF(Ship > 0,'C','X') ,;
            cCancReson WITH &lcOrdHdr..cCancReson ,;
            Cancelled  WITH gdSysDate ,;
            Cancel     WITH IIF(laData[23]='Y',Cancel ,Cancel+lnOpen),;
            CancelAmt  WITH IIF(laData[23]='Y',CancelAmt ,CancelAmt+lnOpenAmt),;
            Open       WITH 0,;
            OpenAmt    WITH 0,;
            TotCut     WITH 0,;
            Book       WITH IIF(laData[23]='Y',Ship + Cancel,lnBook)    ,;
            BookAmt    WITH IIF(laData[23]='Y',ShipAmt + CancelAmt,lnBookAmt) ,;
            Flag       WITH SPACE(1)
    UNLOCK
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF
ENDIF
=gfCloseFile('arCusHst')

IF !llFromEDI
  SELECT unCmSess
  =SEEK('O'+PADR('SOORD',10)+gcUser_id+lcSession)
  
  REPLACE STATUS WITH 'C'
  UNLOCK
  
ENDIF    && End of IF !llFromEDI

llContinue = .F.

WAIT CLEAR  
SELECT ORDHDR
SCATTER FIELDS &lcScFields TO laData

*B037515,1 MHM 01/22/2004 Change places of Both Functions to void change 
*B037515,1                mode from laScrMode[2] to laScrMode[1] before get Info [Start]
*E301621,1 Update the CRMesag file. [Begin]
*DO lpUpdMesag
*E301621,1 Update the CRMesag file. [End]
*=lfGetInfo()           
=lfGetInfo()           
DO lpUpdMesag
*B037515,1 MHM 01/22/2004 [End]

*ahmed
=gfCloseFile('ORDCANLN')
*ahmed end

*605026,1 Update version one with cancelled orders. [Begin]
IF ASCAN(laEvntTrig , PADR('EXPCTOLD',10)) <> 0
  =gfDoTriger('SOORD',PADR('EXPCTOLD',10))
ENDIF     
*605026,1 Update version one with cancelled orders. [End]

STORE .T. TO laScrMode[1]
SHOW GETS

RETURN

*!*************************************************************
*! Name      : lfGet_Info
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGet_Info()
*!*************************************************************
FUNCTION lfGet_Info

PRIVATE llMove
llMove = .F.

=SEEK('M'+laData[2],'Customer')
llConsAcc  = Customer.CONSOL='Y'
lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
=SEEK(IIF(EMPTY(laData[3]),'M'+laData[2],'S'+laData[2]+laData[3]),'Customer')
lnShipAddr = IIF(laData[51],2,1)
STORE '' TO  lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
llMultiSt = (laData[6]='Y')  && cheak if order is multi store.

IF llMultiSt
  STORE 'At Store Level' TO lcBillName
ELSE
  lcBillName = Customer.BtName
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR
ENDIF
IF lnShipAddr = 2
  lcShipName = IIF(laScrMode[2],OrdHdr.StName,&lcOrdHdr..StName)
  lcShipAdd1 = IIF(laScrMode[2],OrdHdr.cAddress1,&lcOrdHdr..cAddress1)
  lcShipAdd2 = IIF(laScrMode[2],OrdHdr.cAddress2,&lcOrdHdr..cAddress2)
  lcShipAdd3 = IIF(laScrMode[2],OrdHdr.cAddress3,&lcOrdHdr..cAddress3)
  lcShipAdd4 = IIF(laScrMode[2],OrdHdr.cAddress4,&lcOrdHdr..cAddress4)
  lcShipAdd5 = IIF(laScrMode[2],OrdHdr.cAddress5,&lcOrdHdr..cAddress5)
  IF laScrMode[2]
    SHOW GET lcShipName DISABLE
    SHOW GET lcShipAdd1 DISABLE
    SHOW GET lcShipAdd2 DISABLE
    SHOW GET lcShipAdd3 DISABLE
    SHOW GET lcShipAdd4 DISABLE
    SHOW GET lcShipAdd5 DISABLE
  ELSE
    SHOW GET lcShipName ENABLE
    SHOW GET lcShipAdd1 ENABLE
    SHOW GET lcShipAdd2 ENABLE
    SHOW GET lcShipAdd3 ENABLE
    SHOW GET lcShipAdd4 ENABLE
    SHOW GET lcShipAdd5 ENABLE
  ENDIF
ELSE
  IF llMultiSt
    STORE 'At Store Level' TO lcShipName
  ELSE
    IF !EMPTY(Customer.Dist_Ctr)
      llMove = .T.
      lcESeek = Customer.Store
      =SEEK("S"+Customer.Account+Customer.Dist_ctr,"Customer")
    ENDIF
    lcShipName =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
    IF llMove
      llMove = .F.
      =SEEK("S"+Customer.Account+lcESeek,"Customer")
    ENDIF
  ENDIF
  SHOW GET lcShipName DISABLE
  SHOW GET lcShipAdd1 DISABLE
  SHOW GET lcShipAdd2 DISABLE
  SHOW GET lcShipAdd3 DISABLE
  SHOW GET lcShipAdd4 DISABLE
  SHOW GET lcShipAdd5 DISABLE
ENDIF  
*B607342,1 ABD - The user should not cancel the order in the edit mode. [Begin]
IF !(laScrMode[3])
  *B607342,1 ABD - [End]
  lcDelMesag = 'cancel'
  DO CASE
    CASE laData[5] = 'B'
      lnOrdStatus = 1
      SHOW GET pbDlt,1 PROMPT lcCancel
    CASE laData[5] = 'O'
      lnOrdStatus = 2
       SHOW GET pbDlt,1 PROMPT lcCancel
    CASE laData[5] = 'H'
      lnOrdStatus = 3
      SHOW GET pbDlt,1 PROMPT lcCancel
    CASE laData[5] = 'X'
      lnOrdStatus = 4
      SHOW GET pbDlt,1 PROMPT lcUnCancel
      lcDelMesag    = 'uncancel'
    CASE laData[5] = 'C'
      lnOrdStatus = 5
      SHOW GET pbDlt,1 PROMPT lcCancel
  ENDCASE
*B607342,1 ABD - Endif for if statment. [Begin]
ENDIF
*B607342,1 ABD - [End]
IF ASCAN(laEvntTrig , PADR('NOMODI',10)) <> 0
  =gfDoTriger('SOORD',PADR('NOMODI',10))
ENDIF     

llInsur   = (laData[22]='Y')
llBulk    = (laData[23]='Y')
llReorder = (laData[24]='Y')
=gfwCodePop(@laCodes,'CTERMCODE','T')
=gfwCodePop(@laCodes,'SHIPVIA','T')
*B607300,1 ABD - Add word At Store Level in case the order is multi store order
*B607300,1 ABD - In edit mode not add mode. [Begin]
IF laScrMode[3]
  IF llMultiSt
    laCodes[2,5] = .T.
  ELSE
    laCodes[2,5] = .F.
  ENDIF
ENDIF
*B607300,1 ABD - [End]

IF llMultiSt
  =lfChMSHV()
ENDIF

=gfwCodePop(@laCodes,'SPCINST','T')
=gfwCodePop(@laCodes,'SEASON','T')
=gfwCodePop(@laCodes,'CDIVISION','T')
=gfwCodePop(@laCodes,'CORDERCAT','T')

lnWareHouse = ASCAN(laWareHouses,laData[31])
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
SHOW GET lnOrdStatus
SHOW GET lnWareHouse
SHOW GET lnShipAddr
SHOW GET llMultiSt
llReBrowse = .T.


*!*************************************************************
*! Name      : lfOrdQUpd                         
*! Developer : Timour A. K.
*! Date      : 05/30/1999
*! Purpose   : Update style orderd quantity in case of order
*!             line style was changed.
*!*************************************************************
*! Calls     : lfSavScr
*!*************************************************************
*! Parameters: lcFlToUpd-> Alias to update (STYLE or STYDYE).
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfOrdQUpd
PARA lcFlToUpd

*--Update New Style.
SELECT (lcFlToUpd)
=RLOCK()
REPLACE Ord1   WITH Ord1   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty1),;
        Ord2   WITH Ord2   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty2),;
        Ord3   WITH Ord3   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty3),;
        Ord4   WITH Ord4   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty4),;
        Ord5   WITH Ord5   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty5),;
        Ord6   WITH Ord6   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty6),;
        Ord7   WITH Ord7   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty7),;
        Ord8   WITH Ord8   + IIF(OrdHdr.Status='B',0,&lcOrdLine..Qty8),;
        TotOrd WITH TotOrd + IIF(OrdHdr.Status='B',0,&lcOrdLine..TotQty)
UNLOCK

*--Update Old Style.
*B804386,1 Change order header warehouse to Order Line warehouse. [Begin]
*=SEEK(OrdLine.Style+IIF(lcFlToUpd='STYLE','',laData[31]+SPACE(10)),lcFlToUpd)
*B125564,1 NNA 12/29/2004 (Begin) in some cases OrdLine.Style be empty because it's EOF()
*B125564,1 NNA            so it'll be also EOF() for the style file so it dosen't get the 
*B125564,1 NNA            Correct (Style.cSlsGlLink) for B#121559
*=SEEK(OrdLine.Style+IIF(lcFlToUpd='STYLE','',OrdLine.cWareCode+SPACE(10)),lcFlToUpd)
=SEEK(IIF(!EMPTY(OrdLine.Style),OrdLine.Style,EVAL(lCOrdLine+'.STYLE'))+;
      IIF(lcFlToUpd='STYLE','',OrdLine.cWareCode+SPACE(10)),lcFlToUpd)
*B125564,1 NNA (End)
*B804386,1 Change order header warehouse to Order Line warehouse. [End]

=RLOCK()
REPLACE Ord1   WITH Ord1   - IIF(OrdHdr.Status='B',0,OrdLine.Qty1),;
        Ord2   WITH Ord2   - IIF(OrdHdr.Status='B',0,OrdLine.Qty2),;
        Ord3   WITH Ord3   - IIF(OrdHdr.Status='B',0,OrdLine.Qty3),;
        Ord4   WITH Ord4   - IIF(OrdHdr.Status='B',0,OrdLine.Qty4),;
        Ord5   WITH Ord5   - IIF(OrdHdr.Status='B',0,OrdLine.Qty5),;
        Ord6   WITH Ord6   - IIF(OrdHdr.Status='B',0,OrdLine.Qty6),;
        Ord7   WITH Ord7   - IIF(OrdHdr.Status='B',0,OrdLine.Qty7),;
        Ord8   WITH Ord8   - IIF(OrdHdr.Status='B',0,OrdLine.Qty8),;
        TotOrd WITH TotOrd - IIF(OrdHdr.Status='B',0,OrdLine.TotQty)
UNLOCK
RETURN

*!*************************************************************
*! Name      : lfStyPos
*! Developer : Reham Al-Allamy
*! Date      : 07/20/1999
*! Purpose   : Function to check if there is style position for 
*!           : any order line to be sure it has been filled.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfStyPos()
*!*************************************************************
FUNCTION lfStyPos
PRIVATE lcCurAlias

*-- Save the current alias.
lcCurAlias = ALIAS()

*-- Flag to know if valid to save or not.
llSvBomVar = .T.

*-- Open the style positions file to Check if there is positions for the selected style.
=gfOpenFile(gcDataDir+'IcStyPos',gcDataDir+'IcStyPos','SH')

*-- Scan all the order lines to check if there is style position entered for the lines 
*-- that have to have records in the BomVar file.
SELECT (lcOrdLine)
SCAN FOR !DELETED()
  *-- Check if there is positions for the style in the current order line.
  IF SEEK(&lcOrdLine..Style , "IcStyPos")
    *-- Check if there is style position entered for the current line.
    IF SEEK("SO" + laData[1] + STR(&lcOrdLine..LineNo,6) , lcT_BomVar)
      *-- Check that at least one of the name drop or the design code has entered for 
      *-- all the style positions of the current order line.
      SELECT (lcT_BomVar)
      SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = "SO"+laData[1] + STR(&lcOrdLine..LineNo,6)
        IF EMPTY(&lcT_BomVar..cNDrpId) .AND. EMPTY(&lcT_BomVar..cDsgnCode)
          *** You cannot leave both name-drop and design code empty for style: {&lcOrdLine..Style}. ***
          *** <  OK  > ***
          =gfModalGen("TRM32071B00000" , "DIALOG" , &lcOrdLine..Style)
          llSvBomVar = .F.
        ENDIF
      ENDSCAN
      SELECT (lcOrdLine)
    ELSE
      *** You have to enter style position for Style: {&lcOrdLine..Style}. ***
      *** <  OK  > ***
      =gfModalGen("TRM32070B00000" , "DIALOG" , &lcOrdLine..Style)
      llSvBomVar = .F.
    ENDIF
  ENDIF
ENDSCAN

*-- Restore the previous alias.
SELECT (lcCurAlias)

*-- Return with the returned flag.
RETURN llSvBomVar


*!*************************************************************
*! Name      : lfUpdtPoCT
*! Developer : WAB - WALID A. WAHAB
*! Date      : 09/26/1999
*! Purpose   : update selling price in po/ct lines
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcOrder	---> sales order no
*!             LcStyle  ---> Style COde
*!             lcLineNo ---> sales order line no
*!             lnTotQty ---> new qty
*!             lnPrice  ---> new price
*!             lnOldPrc ---> old price 
*!             lnOldQty ---> old qty
*!			   llRecDel ---> record is deleted (remove line)
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdtPoCT()
*!*************************************************************
FUNCTION lfUpdtPoCT
PARAMETER  lcOrder,LcStyle,lcLineNo,lnQty,lnPrice,lnOldPrc,lnOldQty,llRecDel

=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
=gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')

*B121451,1 NNA 02/19/2004 (Begin) open (cuttktl.dbf) file only if we have 'MF' module
IF 'MF' $ gcComp_mdl
  =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTLS','SH')
ENDIF
*B121451,1 NNA (End)
SELECT CUTPICK
IF SEEK('2'+lcOrder+lcLineNo)
  *-- there are a po generated from curr. sales order
  SCAN REST WHILE TranCd+Order+Cordline = '2'+LcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percent
    lnCtktNo   = CtktNo					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)     && new generated qty 
    IF SEEK('P'+lnCtktNo+lcStyle,'POSLN')	
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated 
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty));
                   /(POSLN.TotQty-lngenqty)
      ELSE
        lnNewPrc = ((POSLN.nSelPrice*POSLN.TotQty)-(lnOldPrc*lnGenQty)+;
                    (lnPrice*lnTotQty)) / (POSLN.TotQty-lngenqty+lnTotQty)
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      *lnTotCost= nEcost1+nEcost2+nEcost3+nEcost4+nEcost5
      lnTotCost= POSLN.nEcost1+POSLN.nEcost2+POSLN.nEcost3+POSLN.nEcost4+POSLN.nEcost5
      lnRotSub    = IIF(llStyMark,lnTotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - lnTotCost)/lnRotSub)*100)
      REPLACE POSLN.nSelPrice  WITH lnNewPrc ,;
              POSLN.nGrosMrgn  WITH lnGrosMrgn
    ENDIF
  ENDSCAN  
ENDIF
SELECT CUTPICK
IF SEEK('1'+lcOrder+lcLineNo)
  *-- there are a CT generated from curr. sales order
  SCAN REST WHILE TranCd+Order+Cordline = '1'+LcOrder+lcLineNo
    lnGenQty   = TotQty					&& generated qty
    lnGenPerc  = TotQty/lnOldQty  		&& genereted percentg
    lnCtktNo   = CtktNo					&& PO number
    lnTotQty   = INT(lnQty*lnGenPerc)   && new generated qty 
    IF SEEK(lcStyle+lnCtktNo+'1','CUTTKTL')
      *-- new selling price = --> curr selling price * total qty
      *--                     --> - oldprice * old qty generated 
      *--					  --> + newprice * new generated qty
      *-- 					  --> / total qty - old qty generated + new generated qty
      IF llRecDel
        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty));
                   /(CUTTKTL.TotQty-lngenqty)
      ELSE
        lnNewPrc = ((CUTTKTL.nSelPrice*CUTTKTL.TotQty)-(lnOldPrc*lnGenQty)+;
                   (lnPrice*lnTotQty)) / (CUTTKTL.TotQty-lngenqty+lnTotQty)
      ENDIF
      lnNewPrc = IIF(BETWEEN(lnNewPrc,-999999999.99,999999999.99),lnNewPrc,0)
      =SEEK(STYLE,'STYLE')
      lnRotSub   = IIF(llStyMark,Style.TotCost,lnNewPrc)
      lnGrosMrgn = IIF(lnRotSub=0,0,((lnNewPrc - Style.TotCost)/lnRotSub)*100)
      REPLACE CUTTKTL.nSelPrice  WITH lnNewPrc ,;
              CUTTKTL.nGrosMrgn  WITH lnGrosMrgn
    ENDIF
  ENDSCAN
ENDIF


*!**************************************************************************
*! Name      : lpUpdMesag
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/02/2001
*! Purpose   : Update the Message file in CRM Module.
*!**************************************************************************
*! Reference : E301621,1
*!**************************************************************************
*! Example   : DO lpUpdMesag
*!**************************************************************************
*
PROCEDURE lpUpdMesag
IF OrdHdr.lFromWeb AND 'CR' $ gcCmpModules
  PRIVATE lcAlias , lcOrder , llTypeChng , lcType
  lcAlias = ALIAS()
  llTypeChng = .F.
  lcType = OrdHdr.cOrdType
  lcOrder = OrdHdr.Order  
  
  IF SEEK(OrdHdr.cOrdType + OrdHdr.Order,'OrdLine') AND OrdHdr.cOrdType = "T"
    SELECT OrdLine
    
    DO WHILE SEEK("T"+lcOrder,'OrdLine')
      =RLOCK()
      REPLACE cOrdType WITH "O"
      UNLOCK      
    ENDDO
    =SEEK('O' + lcOrder,'OrdLine')
    
    IF SEEK('T'+lcOrder,'OrdHdr')
      SELECT OrdHdr
      =RLOCK()
      llTypeChng = .T.
      REPLACE cOrdType WITH "O"
      lcType = OrdHdr.cOrdType
      UNLOCK
    ENDIF
  ENDIF  

  *-- Get the Memory Variables without checking for the CRM module.
  IF !USED('CRMesag')
    USE (gcDataDir+'CRMesag') IN 0 ORDER TAG TransType DESCENDING
  ENDIF
  
  SELECT CRMesag
  =SEEK('O' + lcOrder,'CRMesag')
  IF lSent OR EOF()
    APPEND BLANK
  ENDIF  

  =gfAdd_Info('CRMesag')
  REPLACE cTransType WITH 'O' , cTransNo WITH OrdHdr.Order , ;
          cMailFrom WITH gfGetMemVar('M_NOTEMAIL') , dTransDate WITH gdSysDate, cMailTo WITH ;
          IIF(SEEK('M'+OrdHdr.Account,'Customer'),EVALUATE('Customer.' + gfGetMemVar('M_CONFMAIL')),'') , ;
          lSent WITH .F.
  IF OrdHdr.Status = "X"
    REPLACE lApprove WITH .F. , cMsgSubjct WITH "Your Sales Order# " + OrdHdr.Order + ' has been cancelled.' , ; 
            cLetterId WITH gfGetMemVar('M_SOCAN') 
  ELSE
    REPLACE lApprove WITH .T. , cMsgSubjct WITH "Your Sales Order# " + OrdHdr.Order + ' has been approved.' , ; 
            cLetterId WITH gfGetMemVar('M_SOAPR')   
            
  ENDIF
          
  IF USED('CRMesag')
    USE IN CRMesag
  ENDIF
  
  IF llTypeChng
    PRIVATE lcOldFilt
    lcOldFilt = SET('FILTER')
    SELECT OrdHdr
    SET FILTER TO
    IF SEEK(lcType + lcOrder)
      =gfObj_Lock(.F.)
    ENDIF
    SET FILTER TO &lcOldFilt
    STORE .F. TO laScrMode
    laScrMode[1] = .T.
  ENDIF  
  
  SELECT (lcAlias)  
ENDIF
*-- End of lpUpdMesag.
**************************************************************

*******************Function to correct OrdHdr pointer.
*B605641,1 SSH Fix the bug that SEEK command do not work.

*B606257,1 SSH Add new parameter in to indicate if we are in EDIT mode.
FUNCTION lfFxPointr
*PARAMETER lcFxTyp,lcFOrd
PARAMETER lcFxTyp,lcFOrd,llEditMod
*B606257,1 SSH [End].

PRIVATE lnFAls , ll2Return , lcOldOrd
lnFAls = SELECT(0)
SELECT ORDHDR
lcOldOrd = ORDER()
SET ORDER TO
LOCATE FOR cOrdType+Order = lcFxTyp+lcFOrd
IF !FOUND() .OR. OrdHdr.Order <> lcFOrd

  *B606257,1 SSH [Start] If we are not in EDIT mode.
  IF !llEditMod
  *B606257,1 SSH [End].

    IF BETWEEN(lnHdrRecNo, 1, RECCOUNT())
      GO lnHdrRecNo
      ll2Return = (OrdHdr.Order = lcFOrd)
    ELSE
      WAIT WINDOW "Unable to locate order# "+lcFOrd
      ll2Return = .F.
    ENDIF

  *B606257,1 SSH [Start] Else If EDIT mode.
  ELSE
    *--- We cannot use rushmore code since that if there is problem with the CDX the locate will
    *--- return faulse.
    WAIT WINDOW "Locate order# "+lcFOrd NOWAIT
    LOCATE FOR Order = lcFOrd .AND. cOrdType = lcFxTyp
    ll2Return = (FOUND() .AND. (cOrdType+Order = lcFxTyp+lcFOrd))
  ENDIF
  *B606257,1 SSH [End] Else If EDIT mode.
ELSE
  ll2Return = (OrdHdr.Order = lcFOrd)
ENDIF
SET ORDER TO &lcOldOrd
SELECT(lnFAls)
RETURN(ll2Return)
*B605641,1 SSH [END]

*:**************************************************************************
*:* Name        : lfDivByZr                                      *B607436,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/30/2003
*:* Purpose     : Overcome the error "Division by 0 when updating the field nOrdAmt
* Note          : this error is difficult to generate so this function is only to be sure
*               : that this type of error will not happen.
*:***************************************************************************
FUNCTION lfDivByZr
PRIVATE llDivByZro,lnOrdAmt,lcUntSin,lcExRSin,lnSlct
lnSlct = SELECT()
llDivByZro = .F.
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, laData[33])

laData[50] = IIF(laData[50] = 0 , 1 , laData[50] )

IF !llFromEDI AND laScrMode[3]
  SELECT ORDHDR
  IF ORDHDR.nCurrUnit = 0
    REPLACE ORDHDR.nCurrUnit WITH 1
  ENDIF
  IF ORDHDR.NEXRATE = 0   
    REPLACE ORDHDR.NEXRATE WITH 1
  ENDIF  
ENDIF  

SELECT &lcOrdLine
lnRecno = RECNO()
GO TOP
SCAN
  STORE 0 TO lnAmt1,lnAmt2
  lnAmt1 = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin laData[34]     &lcUntSin laData[50]
  IF !llFromEDI AND laScrMode[3]
    lnAmt2 = &lcOrdLine..TotQty*&lcOrdLine..Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit 
  ENDIF
  IF '*' $ TRANSFORM(lnAmt1+lnAmt2,REPL('9',15))
    =gfModalGen('INM32121B00000','ALERT')
    llDivByZro = .T.
    EXIT
  ENDIF
ENDSCAN

IF BETWEEN(lnRecno,1,RECCOUNT(lcOrdLine))
  GOTO (lnRecno)
ENDIF  

SELECT (lnSlct)
RETURN llDivByZro
*-- end of lfDivByZr.
*B040008,1 MMR.[End]
