*:****************************************************************
*: Program file  : soimpfl.PRG
*: Program desc. : Import Sales order form Excel
*: System        : Aria Apparel System - Version 4XP.
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar
*: Date          : 07/09/2007
*: T20070625.0007  - C200812
*:****************************************************************
*B610785,1 MMT 07/31/2014 Error while opening custom import program for FLO09[T20140725.0004]
*B610968,1 MMT 03/22/2015 Custom Import sales order for FLO09 gives error[T20150212.0009]
*:****************************************************************
STORE '' TO lcPathName , lcImprtFil
*B610785,1 MMT 07/31/2014 Error while opening custom import program for FLO09[Start]
*DO FORM (oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+"\soimpfl.SCX")
DO FORM (oAriaApplication.ClientScreenHome+oAriaApplication.ActiveModuleID+"\soimpfl.SCX")
*B610785,1 MMT 07/31/2014 Error while opening custom import program for FLO09[End]
*:*************************************************************
*: Name      : lfvgetFile
*: Developer : Mariam Mazhar[MMT]
*: Date      : 07/09/2007
*: Purpose   : Function to get the Excel file data Dir & File.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : FormSet
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvgetFile ()
*:*************************************************************
*:
FUNCTION lfvgetFile
PARAMETERS loFormSet
PRIVATE lcOldPath

lcOldPath = FULLPATH('')
lcPathName  = GETFILE('XLS', 'Excel sheet Path : ','Select') 
SET DEFA TO &lcOldPath
loFormSet.lcName =lcPathName

*!*************************************************************************
*!* Name        : lfvProceed
*:    Developer : Mariam Mazhar[MMT]
*:    Date      : 07/09/2007
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
PARAMETERS lcPathName,LOFORMSET

LOFORMSET.lcdeposittemp = gfTempName()


Private laSeason,laDivision,lcXLS,lcStore,lcPriceLvl, lnLogLnNo , lcSablon
DIMENSION laSeason[1,1],laDivision[1,1]
STORE '' TO laSeason[1,1],laDivision[1,1],lcXls,lcStore,lcPriceLvl,lcSablon
STORE 0 TO lnClrLen , lnClrStPos , lnStyLen , lnStyStPos , lnScaLen , lnScaStPos,lnLogLnNo
=lfChkStrct()


*--lnOrdCount : to hold the numbers of orders that have been generated
*--laOrders   : array to hold the orders numbers.
*--lcNotes    : hold the order's notes.
*--llLoop     : to skip the customer PO order if imported before or it has a style not found in the style file

Private lcLogFile, lError, lOrdErr, lcErrStr, llImpErr, lcDefDir , lcAlias , lcScFields,;
        lcOrdHdr , lcOrdLine ,lcCustPO ,lcStyle ,lnOrdCount ,lcNotes ,llLoop ,lcOldOrder ,lnOldAlias ,;
        lcStyClr,lcSz,lcAcc,lnPrice,lcPrice,llContinue,lcT_BomVar 
STORE '' TO laOrders , lcNotes , lcOldOrder ,lcCustPO ,lcStyle ,lcDefWare,lcXLS
STORE 0 TO lnOrdCount , lnBooked , lnBookAmt , lnOpened , lnOpenAmt , lnOldAlias
STORE .F. TO llLoop , llImpErr ,lError, llContinue

*EXTERNAL ARRAY laData
DIMENSION laOrders[1]
lcLogFile  = gfTempName()
lcOrdHdr   = gfTempName()
lcOrdLine  = gfTempName()
lcT_BomVar = gfTempName()
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

INDEX ON ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(M)+ALLTRIM(R)+ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P) TAG CUSTPO 
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
    SELECT ORDHDR
    =gfSetOrder('ORDCUST')
    IF gfSEEK(ALLTRIM(m.Account) + lcCustPo ,'ORDHDR')
      SELECT ORDHDR 
      SCAN REST WHILE Account+UPPER(Custpo)+Cordtype+Order = ALLTRIM(m.Account) + lcCustPo FOR STATUS<>'X'
        = lfFillLog('4',"Customer order# " + ALLTRIM(lcCustPo) + " has imported before . with Aria's order No." + ALLTRIM(ORDHDR.ORDER) + " this order Skipped",1)
        llLoop = .T.
        EXIT
      ENDSCAN
      =gfSetOrder(lcOldOrder)
      
      IF llLoop
        EXIT
      ENDIF
      SELECT(lnOldAlias)
    ENDIF
    SELECT(lnOldAlias)
    lcStyle   = lfGetStyle()

    *--If the current style not found in the style file then skip this order at all. 
    IF !gfSEEK(UPPER(lcStyle),'STYLE') && Check for style
      = lfFillLog('5',"Style : " +lcStyle+" in Customer PO# " + ALLTRIM(&LCXLS..H) + " Not found in Aria's Style File . this order will be Skipped",2)    
      llLoop = .T.
    ENDIF

    *--Check If the Finished Date is less than the Start date then skip this order at all. 
    IF CTOD(ALLTRIM(U)) > CTOD(ALLTRIM(V))
      = lfFillLog('10'," Starting date for order " + lccustpo + " precede finishing date",2)    
      llLoop = .T.
    ENDIF

    SELECT (lcXLS)
    lcStyle = lfGetStyle()
    =gfSEEK(UPPER(lcStyle),'STYLE')
    SELECT (lcOrdLine)
    IF !SEEK(ALLTRIM(lcCustPO)+ALLTRIM(STYLE.Style)+ALLTRIM(lcStore)+ALLTRIM(SUBSTR(&lcXLS..N,1,6))+ALLTRIM(SUBSTR(&lcXLS..O,1,6))+ALLTRIM(SUBSTR(&lcXLS..P,1,6)));
        AND !EMPTY(lcStyle) AND !llloop 
      INSERT into (lcOrdLine) (ORDER,STYLE,SCALE,SEASON,CWARECODE,CORDTYPE,ACCOUNT,CustPO,Store,lineno) Values ;
      ('',Style.Style, Style.Scale,Style.Season,Style.cDefWare,'O',m.Account,lcCustPO,lcStore,m.lineno)
	  IF EMPTY(m.cDivision)
        m.cDivision = Style.cDivision
      ELSE
        IF m.cDivision <> Style.cDivision
          = lfFillLog(''," The syles for this order have different divisions",2)             
          lError = .T.
          lOrdErr = .T.      
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

    
    
    IF gfSEEK(UPPER(lcStyle),'STYLE')
      lcSZ = '&lcOrdLine..QTY'+lfGetSize(ALLTRIM(R))
      IF !(lcSZ == '&lcOrdLine..QTY')
        IF &lcSZ > 0 
          
          IF !(ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(M)+ALLTRIM(R)+ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P)==lcSablon)
            lcSablon = ALLTRIM(H)+ALLTRIM(L)+ALLTRIM(M)+ALLTRIM(R)+ALLTRIM(N)+ALLTRIM(O)+ALLTRIM(P)
          
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
            lcSZ = '&lcOrdLine..BOOK' + lfGetSize(ALLTRIM(R))
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
          ELSE
            
           
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
            lcSZ = '&lcOrdLine..BOOK' + lfGetSize(ALLTRIM(R))
            Replace &lcSZ      WITH &lcSZ + IIF(TYPE('&lcXLS..Q')='C',INT(VAL(ALLTRIM(Q))),Q)
                     
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
	DIMENSION laFileStru[1,4]
	laFileStru[1,1] = 'mStrRep'
	laFileStru[1,2] = 'M'
	laFileStru[1,3] = 120
	laFileStru[1,4] = 0
	=gfCrtTmp('TMPSTR',@laFileStru,.F.)
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
    DO FORM (oAriaApplication.ScreenHome+ 'SM\SMSTRREP.SCX')
  ENDIF
ENDIF

DO CASE 
  CASE lnOrdCount=1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is saved as ' + laOrders[1])
  CASE lnOrdCount > 1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from ' + laOrders[1] + ' to ' + laOrders[lnOrdCount]+ ' have been generated successfully')
  CASE lnOrdCount = 0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ENDCASE

IF USED(lcXls)
  USE IN (lcXls)
ENDIF
ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')
ERASE (oAriaApplication.WorkDir+lcXls+'.CDX')

*-- End of Function lfvProceed.
*!*************************************************************************
*!* Name        : lfChkFile
*: Developer    : Mariam Mazhar[MMT]
*: Date         : 07/09/2007
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
  RETURN .F.
ENDIF
lcXls = 'SHEET'

If !USED(lcXLS)
  Select 0
  WAIT WINDOW NOWAIT 'Opening Excel file....'                               
  lcErrStr = ON('ERROR')
  llImpErr = .F.
  ON ERROR llImpErr = .T.
  lcDefDir = FULLPATH('')               && Save current default dir
  SET DEFA TO (oAriaApplication.WorkDir)
  *MMT
  WAIT WINDOW NOWAIT '  Please wait...'
  IMPORT FROM (lcPathName) TYPE XLS
  lcXLS = DBF()
*!*	  lcAlias = ALIAS()  
  USE (lcXLS) EXCLUSIVE ALIAS 'SHEET'
  lcXLS = 'SHEET'
*!*	  Select (lcALIAS)
  SET DEFAULT TO &lcDefDir
  ON ERROR &lcErrStr
  WAIT CLEAR

  *MMT
  
*!*	  =lfImprtVFP()
*!*	  WAIT WINDOW NOWAIT '  Please wait...' 
*!*	  *--Loop until memo file be erased from VFP to be sure that the dbf file has physically Copied to the H.D.D
*!*	  DO WHILE FILE(lcCommLine)
*!*	    WAIT WINDOW NOWAIT '  Please wait...'
*!*	  ENDDO
  
*!*	  USE (gcWorkDir + 'SHEET' ) EXCLUSIVE
*!*	  lcXLS = 'SHEET'
*!*	  SELECT (lcXLS)
*!*	  SET DEFAULT TO &lcDefDir
*!*	  ON ERROR &lcErrStr
*!*	  WAIT CLEAR
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
*!* Developer   : Mariam Mazhar[MMT]
*!* Date        : 07/09/2007
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
=gfOpenTable(oAriaApplication.DataDir+'WareHous',oAriaApplication.DataDir+'WareHous','SH')
=gfOpenTable(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
=gfOpenTable(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CCODE_NO','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYLE',oAriaApplication.DataDir+'STYLE','SH')
=gfOpenTable(oAriaApplication.DataDir+'CUSTOMER',oAriaApplication.DataDir+'CUSTOMER','SH')
=gfOpenTable(oAriaApplication.DataDir+'SALESREP',oAriaApplication.DataDir+'SALESREP','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR',oAriaApplication.DataDir+'ORDHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDLINE',oAriaApplication.DataDir+'ORDLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'SYCCURR',oAriaApplication.DataDir+'CCURRCODE','SH')
=gfOpenTable(oAriaApplication.DataDir+'SYCEXCH',oAriaApplication.DataDir+'CURRENCY','SH')
=gfOpenTable(oAriaApplication.DataDir+'SYCINT',oAriaApplication.DataDir+'CCONTCODE','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
=gfOpenTable(oAriaApplication.DataDir+'NOTEPAD',oAriaApplication.DataDir+'NOTEPAD','SH')
=gfOpenTable(oAriaApplication.DataDir+'SOCODES',oAriaApplication.DataDir+'SOCODES','SH')

SET ORDER TO TAG CURRENCY DESC IN 'SYCEXCH'

*-- end of lfOpenFls.

*!*************************************************************************
*!* Name        : lfCrtTmps
*!* Developer   : Mariam Mazhar
*!* Date        : 07/09/2007
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

DIMENSION laFileStruct[3,4]

laFileStruct[1,1]= 'nRecNo'
laFileStruct[1,2]= 'N'
laFileStruct[1,3]= 5
laFileStruct[1,4]= 0

laFileStruct[2,1]= 'cError'
laFileStruct[2,2]= 'C'
laFileStruct[2,3]= 200
laFileStruct[2,4]= 0

laFileStruct[3,1]= 'nFirst'
laFileStruct[3,2]= 'N'
laFileStruct[3,3]= 1
laFileStruct[3,4]= 0

=gfCrtTmp(lcLogFile,@laFileStruct,"ALLTRIM(STR(nfirst))+padl(nRecNo,fsize('nRecNo'))",'LogRecno',.F.)

INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'Data imported from file '+lcPathName+' at Date:'+DTOC(DATE())+' Time:'+TIME(),0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)


SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0


STORE .F. TO laFileStru[lnFileStru+1,5], laFileStru[lnFileStru+1,6]
STORE 0 TO laFileStru[lnFileStru+1,17], laFileStru[lnFileStru+1,18]

FOR lnI = 7 TO 16
  STORE "" TO laFileStru[lnFileStru+1,lnI]
ENDFOR



laFileStru[lnFileStru+2,1] = 'LERRORS'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0


STORE .F. TO laFileStru[lnFileStru+2,5], laFileStru[lnFileStru+2,6]
STORE 0 TO laFileStru[lnFileStru+2,17], laFileStru[lnFileStru+2,18]

FOR lnI = 7 TO 16
  STORE "" TO laFileStru[lnFileStru+2,lnI]
ENDFOR



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
DIMENSION laFileStru[lnFileStru+5,18]
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

FOR lnC = 1 TO 5
  STORE .F. TO laFileStru[lnFileStru+lnC,5], laFileStru[lnFileStru+lnC,6]
  STORE 0 TO laFileStru[lnFileStru+lnC,17], laFileStru[lnFileStru+lnC,18]

  FOR lnI = 7 TO 16
    STORE "" TO laFileStru[lnFileStru+lnC,lnI]
  ENDFOR
ENDFOR  

=gfCrtTmp(lcOrdLine,@laFileStru,"cOrdType+ORDER+STR(LINENO,6)",'ORDLINE',.F.)

SELECT (lcOrdLine)
INDEX ON cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) TAG ORDLINST
INDEX ON cOrdType+ORDER+STYLE+STORE+STR(LINENO,6) TAG ORDLINES
INDEX ON ALLTRIM(CUSTPO)+ALLTRIM(STYLE)+ALLTRIM(STORE)+ALLTRIM(CSBLNAME)+ALLTRIM(CSBLCOLOR)+ALLTRIM(CTRMCOLOR) TAG ORDLN 


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
FOR lnLoop = 1 to  2
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

*-- End of Function lfCrtTmps.
*!**************************************************************************
*!* Name        : lfCloseFls
*!* Developer   : Mariam Mazhar
*!* Date        : 07/09/2007
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
ERASE (oAriaApplication.WorkDir   +lcOrdHdr+'.DBF')
ERASE (oAriaApplication.WorkDir   +lcOrdHdr+'.CDX')

IF USED(lcOrdLine)
  USE IN (lcOrdLine)
ENDIF
ERASE (oAriaApplication.WorkDir   +lcOrdLine+'.DBF')
ERASE (oAriaApplication.WorkDir   +lcOrdLine+'.CDX')

IF USED(lcXls)
  USE IN (lcXls)
ENDIF
ERASE (oAriaApplication.WorkDir+lcXls+'.DBF')

*-- End of Function lfCloseFls.
*!**************************************************************************
*!* Name        : lfValRec
*!* Developer   : Mariam Mazhar[MMT]
*!* Date        : 07/09/2007
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
IF !gfSEEK('M'+PADR(ALLTRIM(&lcXLS..A),5),'CUSTOMER')
  = lfFillLog('',"Customer " + ALLTRIM(&lcXLS..A)+' for order# ' + ALLTRIM(&LCXLS..H) +' is not found , this order '+;
             'will be skipped',1)
  llValid = .F. 
ENDIF
RETURN llValid

*-- End of Function lfValRec.
*!**************************************************************************
*!* Name        : lfSaveData
*!* Developer   : Mariam Mazhar [MMT]
*!* Date        : 07/09/2007
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
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

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
=gfSeek('')
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
  SELECT (lcOrdLine)
  GO TOP
  SELECT (lcOrdHdr)
  llContinue = .T.
  
  *MMT
  *B610968,1 MMT 03/22/2015 Custom Import sales order for FLO09 gives error[T20150212.0009][Start]
*!*	  SELECT Ordhdr 
*!*	  =gfAppend()
*!*	  GATHER FIELDS &lcScFields MEMVAR 
*!*	  REPLACE cOrdType WITH 'O'
*!*	  
*!*	  =gfReplace()

*!*	  DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
*!*	  WITH .F., 'A', lcOrdLine,.F.,.F.,lcT_BomVar,LOFORMSET,.T. 
  DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
  WITH .F., 'A', lcOrdHdr,lcOrdLine,.F.,.f.,lcT_BomVar,loFormSet
  *B610968,1 MMT 03/22/2015 Custom Import sales order for FLO09 gives error[T20150212.0009][End]
  *MMT
  SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)

ENDSCAN
*-- Save Sablon code and Color
=lfSavSabln()

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
  SELECT (lcXls)
  lnOrdCount = lnOrdCount + 1
  DIMENSION laOrders[lnOrdCount]
  laOrders[lnOrdCount] = ORDHDR.ORDER
ENDIF
lfSavefiles() 
SELECT(lnAlias)

*-- End of lfSaveData.
*!*************************************************************************
*!* Name        : lfAddress
*!* Developer   : Mariam Mazhar
*!* Date        : 07/09/2007
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
*!* Developer : Mariam Mazhar
*!* Date      : 07/09/2007
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
*!* Developer   : Mariam Mazhar [MMT]
*!* Date        : 07/09/2007
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
IF gfSEEK('S'+STYLE.SCALE,'SCALE')
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
*!* Developer   : Mariam Mazhar
*!* Date        : 07/09/2007
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
IF gfSEEK('M'+PADR(ALLTRIM(&lcXLS..A),5),'CUSTOMER')
  IF UPPER(ALLTRIM(CUSTOMER.STNAME)) == UPPER(lcStore)
    lcStore = ''
  ENDIF
ENDIF
IF !EMPTY(lcStore) AND !gfSEEK('S'+PADR(ALLTRIM(&lcXLS..A),5)+lcStore,'CUSTOMER')
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
*!* Developer   : Mariam Mazhar
*!* Date        : 07/09/2007
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
    lcMessage = "Invalid Excel file format! , you have to save your excel file as excel sheet ver.4"
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
*! Developer : Mariam Mazhar
*! Date      : 07/09/2007
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
*!* Developer   : Mariam Mazhar
*!* Date        : 07/09/2007
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
=gfSeek('S')
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
ELSE
  = lfFillLog(''," This scale  " + ALLTRIM(UPPER(&LCXLS..R)) + " not found in scale file. this order will be Skipped" ,2)
  lcReturn = PADR(ALLTRIM(&lcXls..L),lnStyLen) + '-' + PADR(ALLTRIM(&lcXls..M),lnClrLen)
  lError = .T.
  llloop = .T.
ENDIF
SELECT(lnOldAlias)
RETURN lcReturn
*-- End of Function lfGetStyle.
*!*************************************************************************
*!* Name        : lfSavSabln
*!* Developer   : Mariam Mazhar[MMT]
*!* Date        : 07/09/2007
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
    = gfSEEK('N'+'CSBLNAME')
    SCAN REST WHILE cDefcode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'N'+'CSBLNAME'
      IF ALLTRIM(UPPER(CDISCREP))=ALLTRIM(UPPER(lcSblName))
        lcSblNcode = ALLTRIM(cCode_No)
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    = gfSEEK('D'+'CSBLNAME')
    lcSblNcode = ALLTRIM(cCode_No)
  ENDIF  
  *--Get the Code for Sablon color
  IF !EMPTY(lcSblColor)
    = gfSEEK('N'+'CSBLCOLOR')
    SCAN REST WHILE cDefcode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'N'+'CSBLCOLOR'
      IF ALLTRIM(UPPER(CDISCREP))=ALLTRIM(UPPER(lcSblColor))
        lcSblCcode = ALLTRIM(cCode_No)
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    = gfSEEK('D'+'CSBLCOLOR')
    lcSblCcode = ALLTRIM(cCode_No)
  ENDIF  
  *--Get the Code for Sablon Accessories color
  IF !EMPTY(lcTrmColor)
    = gfSEEK('N'+'CTRMCOLOR')
    SCAN REST WHILE cDefcode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'N'+'CTRMCOLOR'
      IF ALLTRIM(UPPER(CDISCREP))=ALLTRIM(UPPER(lcTrmColor))
        lcTRMCcode = ALLTRIM(cCode_No)
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    = gfSEEK('D'+'CTRMCOLOR')
    lcTRMCcode = ALLTRIM(cCode_No)
  ENDIF  
  SELECT SOCODES
  IF !gfSEEK('O'+&lcOrdHdr..ORDER+STR(&lcOrdLine..lineno,6))
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

SET ORDER TO (lcOrder) IN (LCORDLINE)
SELECT(lnOldAlias)
*-- End of Function lfSavSabln.

*!*************************************************************************
*!* Name        : lfVldDate
*!* Developer   : Mariam Mazhar[MMT]
*!* Date        : 07/09/2007
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
  m.Entered  = oAriaApplication.SystemDate
  llLogError = .T.
ENDIF
IF !EMPTY(ALLTRIM(U))
  m.Start    = CTOD(ALLTRIM(U))
ELSE
  m.Start    = IIF(!EMPTY(CTOD(ALLTRIM(V))) AND CTOD(ALLTRIM(V))>= oAriaApplication.SystemDate,oAriaApplication.SystemDate,;
               IIF(!EMPTY(CTOD(ALLTRIM(V))) AND CTOD(ALLTRIM(V))< oAriaApplication.SystemDate,CTOD(ALLTRIM(V)),oAriaApplication.SystemDate))
  llLogError = .T.
ENDIF
IF !EMPTY(ALLTRIM(V))
  m.Complete = CTOD(ALLTRIM(V))
ELSE
  m.Complete = IIF(!EMPTY(CTOD(ALLTRIM(U))) AND CTOD(ALLTRIM(U))<= oAriaApplication.SystemDate,oAriaApplication.SystemDate,;
               IIF(!EMPTY(CTOD(ALLTRIM(U))) AND CTOD(ALLTRIM(U))> oAriaApplication.SystemDate,CTOD(ALLTRIM(U)),oAriaApplication.SystemDate))
  llLogError = .T.
ENDIF
IF llLogError 
  = LFFILLLOG('',"One or more date (Enter,Start or Complete) for Order " + lccustpo + " is Empty so it saved as the current date",2)
ENDIF

*-- End of Function lfVldDate.
*!*************************************************************
*! Name        : lfImprtVFP
*! Developer   : Mariam Mazhar
*! Date        : 07/09/2007
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

*!*************************************************************
*! Name      : lfSavefiles
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 07/09/2007.
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
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == loFormSet.DataSessionId
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  MESSAGEBOX('Saving Process is Rolled Back')
  ThisFormSet.Undo()
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*E039415,1 ASM [End]
