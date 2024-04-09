*:****************************************************************************
*: Program file        : SOIMPEX.PRG
*: Program description : Import Sales orders from Excel files
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SO)
*: Developer Name      : Walid A. Wahab (WAB)
*: Date completed      : 02/18/2003
*: Tracking Job Number : C200461
*:****************************************************************************
* This program reads from an Excel file , import data to a Fox2 file , reads from this file
* appends Sales Orders data to ordhdr , ordline files
* 
* Excel sheet fields meaning
* B --> Field Holds Style Major
* E --> Field Holds Color
* F,G,...,O  Fields Hold Sizes
* P --> Field Holds TOTAL
*
*:****************************************************************************
*: Modifications      :
*:B121529,1 NNA 02/22/2004 Fix bug of that when creating the ordline data from the spread sheet it updates
*:B121529,1 NNA            GL_Sales With the first part of the sales linkcode only without the style part
*:B122297,1 ASH 03/30/2004 Some styles are not imported from the excel sheet.
*:B608638,1 MHM 07/31/2008 Fix bug of can not import more than one Excel sheet at the same time.
*:****************************************************************************
PRIVATE lcOldValue,lcRpXlsFil,lcRpSeason,lcRpDiv,lcRpWarh,laSeason,laDivision,lcCodName,;
        lcOrdHdr,lcOrdLine,lnErrCnt,lcXls,lcLogFile,lcErroSrc
STORE '' TO lcOldValue,lcRpXlsFil,lcRpSeason,lcRpDiv,lcRpWarh,lcCodName
STORE 0 TO lnErrCnt        && Increased by 1 for every error occured in data imported
lcOrdHdr   = gfTempName()
lcOrdLine  = gfTempName()
lcLineQty  = gfTempName()
lcErroSrc = gcWorkDir+'Import.Log'
lcLogFile = gfTempName()
DIMENSION laSeason[1,1],laDivision[1,1]
STORE ' ' TO laSeason[1,1],laDivision[1,1]

*--If code not setuped , warn the user
lcCodName = lFfillcode()
IF !EMPTY(lcCodName)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcCodName + ;
  ' codes have not been set up. Please set up and try again.')
  RETURN 
ENDIF

*--Open all the files 
=lfOpenFls()
*--Initialise  all the variable used in the option grid
lcOldVal = ""
lcHoldOrder = gfGetMemVar('M_HOLD_ORD',gcAct_Comp)       && Default new order status to hold
lnCompDate  = gfGetMemVar('M_COMPDATE',gcAct_Comp)       && Number of days need to calculate order completion date

lcRpAcc     = ''						&& *--Account
lcRpCurr    = ''						&& *--Curr Code
lcRPSalRep  = ''						&& *--Sales Rep
lnRPComss   = ''						&& *--Sales Rep Commisssion 
ldRpCtPoNo  = ''						&& *--Customer Po No
lcRpStore   = ''						&& *--Store code 	
ldRpEntDat  = {}						&& *--entered date
ldRpStDate  = {}						&& *--start date 
ldRpComDat  = {}						&& *--complete date
ldRpNote1   = ''						&& *--Note one
ldRpNote2   = ''						&& *--Note 2

*--Call the option grid
lcExpr = gfOpGrid('SOIMPEX' , .T.)


*--IF close button pressed , exit
IF EMPTY(lcExpr)
  RETURN
ENDIF

*--Import Confirmation message.
IF gfModalGen('INM00000B32000',.F.,.F.,.F.,;
   'Are you sure you want to import data from the selected XLS file?') = 2
  RETURN
ENDIF
*--Importing  data from  Excel to temp fox file then to aria27 files
=lfOrdImprt()
*:B608638,1 MHM 07/31/2008 Fix bug of can not import more than one Excel sheet at the same time.[Start]
IF USED('W25')
  USE IN ('W25')
ENDIF
*:B608638,1 End

*:**************************************************************************
*:* Name        : lfOrdImprt
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Importing  data from  Excel to temp fox file then to aria27 files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOrdImprt()
*:***************************************************************************
FUNCTION lfOrdImprt

PRIVATE lnCnt,lcCnt,lnSzPos,lcStyClr,lnScaleLen,laExtSz,lnQty,;
        lnK,lcSz,lnOpn,laSize,lcDefDir,laFlds,lcDele,llAllZero
lnScaleLen = gfGetMemVar('M_EXTWIDTH')
DIMENSION laSize[10,2]         &&Hold sizes names
laSize = ''
For lnCount = 1 TO 10
  laSize[lnCount,2] = 0
ENDFOR


CREATE TABLE (gcWorkDir+lcLogFile) (cErro C(120))
INSERT INTO (lcLogFile) VALUES ('-------------------------------------------------------------------------')
INSERT INTO (lcLogFile) VALUES ('Data imported from file '+lcRpXlsFil+' at Date:'+DTOC(DATE())+' Time:'+TIME())
INSERT INTO (lcLogFile) VALUES ('-------------------------------------------------------------------------')

*--Create needed temp files
=lfCratTemp()
*--Import data from the excel file to a temp file 
SELECT 0
lcXls = SUBSTR(lcRpXlsFil,RAT('\',lcRpXlsFil)+1,;
                               RAT('.',lcRpXlsFil)-1-RAT('\',lcRpXlsFil))
IF USED(lcXls)  
  USE IN (lcXls)

ENDIF
WAIT WINDOW NOWAIT 'Opening Excel file....'                               
lcErrStr = ON('ERROR')
llImpErr = .F.
ON ERROR llImpErr = .T.
lcDefDir = FULLPATH('')               && Save current default dir
SET DEFA TO (gcWorkDir)
IMPORT FROM (lcRpXlsFil) TYPE XLS
=lfChkXls()
SET DEFAULT TO &lcDefDir
ON ERROR &lcErrStr
WAIT CLEAR
IF llImpErr
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Excel file format!')      
  =lfCloseFls()
  RETURN
ENDIF

WAIT WINDOW NOWAIT 'Removing unneeded records....'
=AFIELDS(laFlds)
M.REP = lcRPSalRep

*-- Locate the first record to get data from
LOCATE
*--delete frist record
IF !(B ="Style No.")
  DELETE
ENDIF
LOCATE

m.Account = lcRpAcc
DO WHILE !EOF()
  *--if this a new style we have to collect the scale to be in array lasize
  *B122297,1 ASH 03/30/2004 (Begin) Fix bug of some styles are not imported from the excel sheet.
  *IF (B ="Style No." OR UPPER(P) = 'TOTAL')
  IF (B ="Style No." OR UPPER(P) = 'TOTAL' OR UPPER(Q) = 'TOTAL' OR EMPTY(E))
  *B122297,1 ASH 03/30/2004 (End)
    =lfGetScale()
    SKIP
    lcStyMajor = B
    *B122297,1 ASH 03/30/2004 (Begin) Fix bug of some styles are not imported from the excel sheet.
    *SCAN REST WHILE (UPPER(P) <> 'TOTAL') AND (EMPTY(B) OR !("Style" $ B ))
    SCAN REST WHILE (UPPER(Q) <> 'TOTAL' AND UPPER(P) <> 'TOTAL') AND (EMPTY(B) OR !("Style" $ B ))
    *B122297,1 ASH 03/30/2004 (End)
      lcStyMajor = IIF(!EMPTY(B),B,lcStyMajor)
      WAIT WINDOW 'Style : ' + lcStyMajor NOWAIT
      =lfGetQty()

      *--i found some empty records in the excel sheet so i have to check if the color field is empty 
      *--skip the line

      IF EMPTY(E)
        *B122297,1 ASH 03/30/2004 (Begin) 
        *LOOP
        EXIT
        *B122297,1 ASH 03/30/2004 (End)
      ENDIF
      lcStyColor = PADR(lcStyMajor,12)+'-'+PADR(E,3)
      
      *--if the style/color (without the scale) is exist in the style file wo we will take line by line
      *--and compare the size desc from the excel sheet with the style/color/scale in the style file
      *--because the excel sheet contain the style/color and the sizes for all its  scales
      
      IF SEEK(lcStyColor,'STYLE')
        *B122297,1 ASH 03/30/2004 (Begin) I added this check becasue sometimes some sizes are not included in the heading.
        IF !lfChecksz()
          m.Style = lcStyColor
          =lfaddError(.F.,'SCL')   
        *B122297,1 ASH 03/30/2004 (End)
        ENDIF
        SELECT STYLE
        SCAN REST WHILE Style = lcStyColor
          IF !lfCheckQty() 
            EXIT
          ENDIF
          lcStyle = Style.Style
          =lfAdNewRec()
        ENDSCAN
      ELSE
        m.Style = lcStyColor
        =lfaddError(.F.,'STY')                    
      ENDIF
    ENDSCAN
  ELSE
    SKIP
  ENDIF
ENDDO


*--Initialize memory vairables
SELECT ORDHDR
SCATTER MEMVAR MEMO BLANK
SELECT ORDLINE
SCATTER MEMVAR MEMO BLANK
SELECT ORDHDR
m.CCURRCODE = PADR(lcRpCurr,3)
m.ACCOUNT   = PADR(lcRpAcc,5)
m.Store     = PADR(lcRpStore,8)
m.CORDTYPE = 'O'
m.STATUS = IIF(lcHoldOrder $ 'YF','H','O')
m.SEASON = PADR(lcRpSeason,6)
m.CDIVISION = PADR(lcRpDiv ,6)
m.CWARECODE = PADR(lcRpWarh,6) 
IF SEEK(m.CCURRCODE,'SYCCURR')
  m.NCURRUNIT = SYCCURR.NCURRUNIT
  IF SEEK(gcBaseCurr+m.CCURRCODE,'SYCEXCH')
    m.NEXRATE = SYCEXCH.NEXRATE
  ENDIF
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
ENDIF  
m.REP1     = PADR(lcRPSalRep,3)
m.COMM1    = lnRPComss
m.ENTERED  = ldRpEntDat
m.START    = ldRpStDate
m.COMPLETE = ldRpComDat
m.NOTE1    = ldRpNote1
m.NOTE2    = ldRpNote2
m.CREORDER = 'N'
m.MULTI    = 'N'
m.Cinsur   = 'Y'
m.CustPo   = ldRpCtPoNo
m.Bulk     = 'N'
*-- check if there is any order for the same customer or not (specs)
** order ordCust******account+UPPER(custpo)+cordtype+order
SELECT OrdHdr
lcOrder = ORDER()
SET ORDER TO OrdCUst

IF SEEK(m.Account+PADR(m.CustPo,15)+'O') 
  llFound = .T. 
  IF Status $ 'XC'
    LOCATE REST WHILE Account+CustPo+'O'=m.Account+PADR(m.CustPo,15)+'O' FOR (Status $'HOB')
    IF !FOUND()
      llFound = .F.
    ENDIF
  ENDIF  
  IF llFound
    lcMessage = 'There is a'
    lcStatus  = IIF(STATUS='O','n Open',IIF(STATUS='H',' Hold','Bid'))
    lcMessage = lcMessage + lcStatus
    lcMessage = lcMessage + ' order already placed for that Account Code, Season, Division and Customer PO.'
    lnResp = gfModalGen('INM00000B00023',.F.,.F.,.F.,lcMessage)
    IF lnResp = 2
      =lfCloseFls()
      RETURN
    ENDIF
 ENDIF
ENDIF
SET ORDER TO &lcOrder

*--Create the order lines    
SELECT (lcLineQty)  
LOCATE
m.LINENO = 0
m.cordtype = 'O'
SCAN  
  IF  TOTQTY <> 0
    m.STYLE = Style
    IF SEEK(m.Style,'Style')
      m.Scale      = Style.Scale
      m.GROS_PRICE = IIF(m.CCURRCODE =gcBaseCurr,Style.PriceA,;
                       gfStyPrice(m.Style,'A',m.CCURRCODE))
      *B121529,1 NNA 02/22/2004 (Begin) Add the style sales linkcode to GL_Sales code	
	  m.GL_Sales  = Left(m.GL_Sales,3) + IIF(EMPTY(style.cSlsGlLink),'DEF',Style.cSlsGlLink)  
      *B121529,1 NNA (End)	  
      SELECT (lcLineQty)  
      m.Price      = m.GROS_PRICE
      m.Desc1      = STYLE.Desc1
    ENDIF
    IF STYLE.SEASON = PADL(lcRpSeason,6) AND STYLE.CDIVISION = PADL(lcRpDiv,6)
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Totqty,;
            m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8,m.TotBook
      FOR lnCount = 1 TO 8
        lcStr = STR(lnCount,1)
        m.Book&lcStr = Qty&lcStr
        m.Qty&lcStr  = Qty&lcStr
        m.Totbook    = m.Totbook + Qty&lcStr
        m.Totqty     = m.Totqty  + Qty&lcStr
        m.BOOK       = m.BOOK    + Qty&lcStr
        m.OPEN       = m.OPEN    + Qty&lcStr
        m.BOOKAMT    = m.BOOKAMT + m.Price*Qty&lcStr
        m.OPENAMT    = m.OPENAMT + m.Price*Qty&lcStr
      ENDFOR
      m.LINENO   = m.LINENO + 1
      m.LASTLINE = m.LASTLINE + 1
      INSERT INTO (lcOrdLine) FROM MEMVAR
      =lfAddUsrDt(lcOrdLine)
    ELSE
      =lfaddError(.F.,'SDV')                
    ENDIF
  ELSE
  *  =lfaddError(.F.,'QTY')
  ENDIF    
ENDSCAN    
INSERT INTO (lcOrdHdr) FROM MEMVAR
=lfAddUsrDt(lcOrdHdr)
        
WAIT CLEAR
SELECT (lcOrdHdr)

IF lnErrCnt>0 
   llExit = .F.
   DO WHILE !llExit
    *-- Button  : 32013
    *-- Options : View the log file , continue , Cancel
    lnResp = gfModalGen('INM00000B32013',.F.,.F.,.F.,'Some problems are detected.')
    DO CASE
      CASE lnResp = 1
        =lfErrFle()    
      CASE lnResp = 2
        =lfSaveData()
        llExit = .T.
      CASE lnResp = 3
        llExit = .T.
        =lfCloseFls()
        RETURN
    ENDCASE    
  ENDDO
ELSE
  =lfSaveData()
ENDIF

*--Close temp files
=lfCloseFls()

*-- end of lfImport.

*:**************************************************************************
*:* Name        : lfaddError
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : SKip current order
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSkip()
*:***************************************************************************
FUNCTION lfaddError
PARAMETERS lcExpr,lcReason
PRIVATE lcTxt,lnCnt

*B121529,1 NNA 02/22/2004 (Begin) in this case there is a massage appear (Alias 'The excel file name' not found)
*SELECT (lcXls)
*-- Write to alternate file
*lcTxt = 'LINE#('+PADL(RECNO(lcXls),6)+') '+'Acc:'+M.ACCOUNT+' '
lcTxt = 'LINE#('+PADL(RECNO(),6)+') '+'Acc:'+M.ACCOUNT+' '
*B121529,1 NNA (End)

IF !EMPTY(lcReason)
  DO CASE
    CASE lcReason = 'STY'
      lcTxt = lcTxt + 'Style '+M.STYLE+' not found.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'SCL'
      lcTxt = lcTxt + 'Style '+M.STYLE+' Wrong Scale/Size or Scale/Size not found.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
      
    CASE lcReason = 'SDV'
      lcTxt = lcTxt + 'Style '+M.STYLE+' Season or Division not match the selected.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
    *B122297,1 ASH 03/30/2004 (Begin) Add line to text file if qty = 0
    CASE lcReason = 'QTY'
      lcTxt = lcTxt + 'Style '+STYLE+' has ZERO quantity.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
    *B122297,1 ASH 03/30/2004 (End)   
  ENDCASE
  *--Increase error counter
  lnErrCnt = lnErrCnt + 1
  M.LERRORS = .T.
ENDIF

*-- end of lfSkip.


*:**************************************************************************
*:* Name        : lfChkXls
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Check structure of the Excel file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkXls()
*:***************************************************************************
*When importing from excel file and some fields do not contain data , the file
*created in the Import process , will not contain a field corresponding to that
*column .
*Moreover if there  is a refrence to that field in the code then a error occurs
*Here a field  is added for each non existing column with empty data

FUNCTION lfChkXls
IF llImpErr
  RETURN
ENDIF

PRIVATE laFlds,lcAlias,lnCnt,lcTempName
lcAlias = ALIAS()
lcDbf   = DBF()
=AFIELDS(laFlds)
=AFIELDS(laFlds2)
=ADEL(laFlds,2,2)             && Remove the column that contains the field type
*in same case if the any excle column is empty the import will ignore this  column so that will cause
*--a probem  " Variable foes notexist so that we have to add the misssing columns "
FOR lnCnt = 65 TO 88          && loop from "A" to "X"
  IF ASCAN(laFlds,CHR(lnCnt)) = 0
    lnLn = ALEN(laFlds2,1)+1
    DIMENSION laFlds2[lnLn,4]
    laFlds2[lnLn,1] = CHR(lnCnt)
    laFlds2[lnLn,2] = 'C'
    laFlds2[lnLn,3] = 10
    laFlds2[lnLn,4] = 0    
  ENDIF
ENDFOR
IF ALEN(laFlds,1) # ALEN(laFlds2,1)
  lcTempName = gfTempName()
  CREATE TABLE (gcWorkDir+lcTempName) FROM ARRAY laFlds2
  APPEND FROM (lcDbf)
  USE IN (lcAlias)
  USE (gcWorkDir+lcTempName) ALIAS (lcAlias)
ENDIF
*--end of lfChkXls.

*:**************************************************************************
*:* Name        : lfErrFle
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Open error log file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErrFle()
*:***************************************************************************
FUNCTION lfErrFle
PRIVATE lnHndl
DO (gcScrDir +  gcAct_Appl + '\Soimperr.SPX')

SELECT (lcLogFile)
DO WHILE .T.
  IF FILE(lcErroSrc)
    lnHndl = FOPEN(lcErroSrc)
    IF lnHndl = -1
      IF gfModalGen('QRM00000B02011',.F.,.F.,.F.,'Can not create the error log file! Close the log file and try again.') = 1
        LOOP
      ELSE
       EXIT
      ENDIF
    ELSE
      =FCLOSE(lnHndl)  
    ENDIF  
  ENDIF
  COPY TO (lcErroSrc) TYPE DELIMITED
  EXIT
ENDDO


*-- end of lfErrFle.

*:**************************************************************************
*:* Name        : lfwRpWhen() 
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : When OG Function
*:***************************************************************************
*:* Called from : OG
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
  SELECT WAREHOUS
  LOCATE
  lcRpWarh = WAREHOUS.CWARECODE
  =lfGetDefault()
ENDIF

*--Disable/Enable pbRun button via lfChkFlds() Fn.
=lfChkFlds()

*-- end of lfwRpWhen.

*:**************************************************************************
*:* Name        : lfvXlsFl
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Select Excel (*.xls) file to import Data from
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvXlsFl()
*:***************************************************************************
FUNCTION lfvXlsFl

lcRpXlsFil = SUBSTR(lcRpXlsFil,1,RAT('.',lcRpXlsFil)+3)
IF !EMPTY(lcRpXlsFil) .AND. !FILE(lcRpXlsFil)
  lcRpXlsFil = GETFILE('XLS','Select XLS file')
  IF EMPTY(lcRpXlsFil)
    lcRpXlsFil = IIF(!FILE(lcOldValue),'',lcOldValue)
  ENDIF
ENDIF  

*-- end of lfvXlsFl.

*:**************************************************************************
*:* Name        : lfwXlsFl
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : When function for Get Excel (*.xls) file
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwXlsFl()
*:***************************************************************************
FUNCTION lfwXlsFl
PARAMETERS lcVarName
lcOldValue = &lcVarName

*-- end of lfwXlsFl.

*:**************************************************************************
*:* Name        : lfChkFlds
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Check all fields since All fields are mandatory 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkFlds()
*:***************************************************************************
FUNCTION lfChkFlds
PRIVATE llStat,lnCnt,lcToChekc
lcStat = IIF(!EMPTY(lcRpXlsFil) AND !EMPTY(lcRpWarh) AND !EMPTY(lcRpACc) AND !EMPTY(lcRpSeason);
         AND !EMPTY(lcRpDiv) AND !EMPTY(lcRpCurr) AND !EMPTY(lcRPSalRep) ;
         AND !EMPTY(ldRpEntDat) AND !EMPTY(ldRpStDate) AND !EMPTY(ldRpComDat) ;
         ,'ENABLE','DISABLE')

SHOW GET pbRun &lcStat
*-- end of lfChkFlds.


*:**************************************************************************
*:* Name        : lffillcode
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Fill code 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lffillcode()
*:***************************************************************************
FUNCTION lffillcode
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES','IDRLTFNAME','SH')  && Key : CDEFCODE+CRLTFIELD+CFLD_NAME
ENDIF  

SELECT CODES
LOCATE
SELECT CCODE_NO+' - '+CDISCREP FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNSEASON    ' ;
INTO ARRAY laSeason
IF EMPTY(laSeason)
  RETURN 'Season'
ENDIF

LOCATE
SELECT CCODE_NO+' - '+CDISCREP FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCDIVISION ' ;
INTO ARRAY laDivision
IF EMPTY(laDivision)
  RETURN 'Division'
ENDIF

RETURN ''
*-- end of lffillcode.

*:**************************************************************************
*:* Name        : lfAddUsrDt
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Add user id ,date  and time to files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAddUsrDt()
*:***************************************************************************
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

*:**************************************************************************
*:* Name        : lfDefCod
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Get the default code
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDefCod()
*:***************************************************************************
FUNCTION lfDefCod
PARAMETERS lcFldName
PRIVATE lcAlias,lcOrder,lcRet
lcAlias = SELECT()
lcRet = ''
SELECT CODES
lcOrder = ORDER()
SET ORDER TO CCODE_NO 

IF SEEK('D'+lcFldName,'CODES')
  lcRet = CODES.CCODE_NO + ' - ' + CODES.CDISCREP
ENDIF

SET ORDER TO &lcOrder
SELECT (lcAlias)
RETURN lcRet
*-- end of lfDefCod.

*!*************************************************************
*! Name      : lfCratTemp
*! Developer : Walid A. Wahab (WAB)
*! Date      : 02/18/2003
*! Purpose   : Create temp files
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCratTemp()
*!*************************************************************
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex

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

DECLARE laIndex[3,2]
laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'

=gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)

SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
SCATTER MEMVAR BLANK MEMO

=gfCrtTmp(lcLineQty,@laFileStru,'STYLE+STR(LINENO,6)',lcLineQty)

*-- end of lfCratTemp.

*:**************************************************************************
*:* Name        : lfvWarhous
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Get a location
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvWarhous()
*:***************************************************************************
FUNCTION lfvWarhous
PRIVATE lcWH
SELECT WAREHOUS
lcWH = PADR(ALLTRIM(lcRpWarh),6)
IF !EMPTY(lcWH)
  IF SEEK(lcWH,'WAREHOUS')
    lcRpWarh = WAREHOUS.CWARECODE
  ELSE
    =gfBrowWare(.F.)
    lcRpWarh = WAREHOUS.CWARECODE
  ENDIF  
ENDIF
*-- end of lfvWarhous.

*:**************************************************************************
*:* Name        : lfSaveData
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
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
STORE ''   TO laWareHouses , lcODefWare    , lcScFields ,laSeasons   ,;
              laCodes       , lcODefSes       , lcSeason             
STORE .F.  TO laSetups     , llFound1      , llFound2        , llBrowse   
             
STORE 1    TO lnWareHouse  , lnOrdStatus   , lnSeason
STORE 0    TO lnBook       , lnOpen
STORE 0.00 TO lnOpenAmt    , lnBookAmt     , lnTotAmt
             
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

laCodes[1,1] = 'SEASON'
laCodes[1,2] = 'laSeasons'
laCodes[1,3] = 'lnSeason'
laCodes[1,4] = ' '
laCodes[1,5] = .T.
laCodes[1,6] = .F.
laCodes[1,10] = 'SEASON'
*B121529,1 NNA 02/22/2004 (Begin) fix bug of message appear ( Alias already in use)
IF !USED('CODES')
  =gfwCodePop(@laCodes,'SEASON','L')
ELSE
  SELECT CODES
  lnSeason   = ASCAN('laSeasons',lcODefSes)
  lnSeason   = IIF(lnSeason=0,1,ASUBSCRIPT(laSeasons,lnSeason,1))
ENDIF
*B121529,1 NNA (End)

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
  *B121529,1 NNA 02/22/2004 (Begin) fix bug of message appear ( Alias already in use)
  *SELECT cDesc,cWareCode FROM (gcDatadir+'WAREHOUS') INTO ARRAY laWareHouses
  laWareHouses[1,1]=WAREHOUS.cDesc
  laWareHouses[1,2]=WAREHOUS.cWareCode
  *B121529,1 NNA 02/22/2004 (End)
  =gfCloseFile('WAREHOUS')

lnWareHouse = ASCAN(laWareHouses,lcODefWare)
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))

laScrMode[4] = .T.
STORE .F. TO llMFDsPrc , llPoDsPrc
llOrdSaved = .F.
*--Loop throght the temp file lcOrdHdr to save data
SELECT (lcOrdHdr)
LOCATE
llOrdSaved = .T.
SCATTER FIELDS &lcScFields TO laData
SELECT &lcOrdLine
LOCATE
SELECT &lcOrdHdr
llContinue = .T.
lnActFolder = 1
DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
IF !llOrdSaved
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ENDIF

*-- end of lfSaveData.

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Open needed files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOpenFls()
*:***************************************************************************
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
SET ORDER TO TAG CURRENCY DESC IN 'SYCEXCH'
*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfCloseFls
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
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

IF USED(lcXls)
  USE IN (lcXls)
ENDIF

IF USED(lcLineQty)
  USE IN (lcLineQty)
ENDIF

ERASE (gcWorkDir+lcXls+'.DBF')

IF USED(lcLogFile)
  USE IN (lcLogFile)
ENDIF
ERASE (gcWorkDir)+(lcLogFile)+".DBF"

*-- end of lfCloseFls.


*:**************************************************************************
*:* Name        : lfvErrSrc
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : Change the output error file
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


*:**************************************************************************
*:* Name        : lfvAccount
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : valid function of the account filed 
*:***************************************************************************
*:* Called from : Option Grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvAccount()
*:***************************************************************************
FUNCTION lfvAccount
PRIVATE lcItsName , lcItsVal , llObjRet , lnAlias
lnAlias   = SELECT()
SELECT CUSTOMER
lcItsName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal  = EVALUATE(SYS(18))     && Varible to hold  the value of the current GET field

IF USED('SYCINT')
  SET ORDER TO TAG Ccontcode IN SYCINT
ENDIF

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal ,.T.,'M' )
  IF !(lcItsVal == lcoldval)
    lcRpStore = ''
  ENDIF
  lcItsVal = IIF(llObjRet , lcItsVal , lcoldval)
  &lcItsName = lcItsVal
ENDIF
=lfAccDeflt()
=lfShowgets('lcRpStore')
=lfShowgets('lcRpCurr')
=lfShowgets('lcRPSalRep')
=lfShowgets('lnRPComss')
=lfChkFlds()
SELECT (lnAlias)

*--End of lfvAcc.
*:**************************************************************************
*:* Name        : lfvStore
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : valid function of the Store filed 
*:***************************************************************************
*:* Called from : Option Grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvStore()
*:***************************************************************************
FUNCTION lfvStore
PRIVATE lcItsName , lcItsVal , llObjRet , lnAlias
lnAlias   = SELECT()
SELECT CUSTOMER

lcItsName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal  = EVALUATE(SYS(18))     && Varible to hold  the value of the current GET field

IF USED('SYCINT')
  SET ORDER TO TAG Ccontcode IN SYCINT
ENDIF

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('S' + lcRpAcc + lcItsVal , 'CUSTOMER'))
  XSTORE   = " "
  llObjRet = CusBrowS(lcRpAcc ,.T.)
  lcItsVal = IIF(llObjRet , XSTORE , lcItsVal)
  &lcItsName = lcItsVal
ENDIF

SELECT (lnAlias)

*:**************************************************************************
*:* Name        : lfwOldVal
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : when function of the accoutn field
*:***************************************************************************
*:* Called from : Option Grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwOldVal()
*:***************************************************************************
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))      && Varible to hold the old value


*:**************************************************************************
*:* Name        : lfShowGets
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : to show get a field in the option grid
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfShowGets()
*:***************************************************************************
FUNCTION lfShowGets
PARAMETERS lcObjName

lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,lcObjName),1)
laOGObjCnt[lnPos] = .T.
=lfOGShowGet(lcObjName)
=lfChkFlds()


*:**************************************************************************
*:* Name        : lfvSalesRep
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : valid function of the salesrep field in the option grid
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSalesRep()
*:***************************************************************************
FUNCTION lfvSalesRep

lcRepCode = VARREAD()

IF !EMPTY(&lcRepCode) AND ('?' $ &lcRepCode OR !SEEK(&lcRepCode , 'SALESREP'))
    XREPCODE = &lcRepCode
    DO REPCHK WITH XREPCODE, .T.
    &lcRepCode = IIF(EMPTY(XREPCODE),lcOldVal,XREPCODE)
    IF !(&lcRepCode == CUSTOMER.SalesRep  OR &lcRepCode == CUSTOMER.Rep2 )
      lnRPComss  = SalesRep.comm
    ELSE
      lnRPComss  = IIF(&lcRepCode == CUSTOMER.SalesRep,Customer.Comm,Customer.Comm2)
    ENDIF
    =lfShowgets('lnRPComss')
ENDIF
=lfChkFlds()
*ELSE
*  &lcRepCode = lcoldValue

*:**************************************************************************
*:* Name        : lfGetDefault
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : get the defautl value to the start,entered,complete date fields
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetDefault()
*:***************************************************************************
FUNCTION lfGetDefault
ldRpEntDat = gdSysDate
ldRpStDate = gdSysDate
ldRpComDat = gdSysDate + lnCompDate


*:**************************************************************************
*:* Name        : lfvCurr
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : valid function of the Cuurency field in the option grid
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCurr()
*:***************************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SYCCURR'))

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CCURRCODE :R :H= 'Currency' , "   +;
               "CCURRSMBL :R :H= 'Symbol' ,"      +;
               "CCURRDESC :R :H= 'Description' ," +;
               "NCURRUNIT :R :H= 'Units' "
  
  lcFile_Ttl = "Currencies ..."
  = gfBrows('','CCURRCODE','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = lcOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
=lfChkFlds()
*-- end of lfvCurr.

*:**************************************************************************
*:* Name        : lfAccDeflt
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : get the default salesrep and comission and currency 
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAccDeflt()
*:***************************************************************************
FUNCTION lfAccDeflt
lcRpCurr   = CUSTOMER.ccurrcode
lcRPSalRep = CUSTOMER.salesrep
lnRPComss  = CUSTOMER.comm
=lfShowgets('lcRpCurr')
=lfShowgets('lcRPSalRep')
=lfShowgets('lnRPComss')


*:**************************************************************************
*:* Name        : lfGetScale
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : get the scale from the tempfile to lasize
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetScale()
*:***************************************************************************
FUNCTION lfGetScale
DIMENSION laSize[10,2]         &&Hold sizes names
laSize = ''
For lnCount = 1 TO 10
  laSize[lnCount,2] = 0
ENDFOR

FOR lnCnt = 1 TO 10
  lcSz = EVAL(CHR(69+lnCnt))
  IF !EMPTY(lcSz)
    lcSz = IIF(TYPE('lcSz')='N',STR(lcSz),lcSz)
    laSize[lnCnt,1] = ALLTRIM(lcSz)
  ENDIF
ENDFOR

*:**************************************************************************
*:* Name        : lfGetQty
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : get qty from tempfile to lasize
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetQty()
*:***************************************************************************
FUNCTION lfGetQty
FOR lnCnt = 1 TO 10
  lcSz = EVAL(CHR(69+lnCnt))
  IF !EMPTY(lcSz)
    lcSz = IIF(TYPE('lcSz')='N',STR(lcSz),lcSz)
    laSize[lnCnt,2] = VAL(lcSz)
  ENDIF
ENDFOR

*:**************************************************************************
*:* Name        : lfAdNewRec
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : add new record to the temp file 
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAdNewRec()
*:***************************************************************************
FUNCTION lfAdNewRec
PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcLineQty)
APPEND BLANK
REPLACE STYLE WITH lcStyle 
IF SEEK('S'+RIGHT(ALLTRIM(STYLE),3),'SCALE')
  FOR lnCount = 1 TO 8
    lcStr  = ALLTRIM(STR(lnCount))
    lnSize = ASCAN(laSize,ALLTRIM(SCALE.SZ&lcStr))
    IF lnSize > 0
      lnSize = ASUBSCRIPT(laSize,lnSize,1)
      REPLACE Qty&lcStr WITH laSize[lnSize,2]
      laSize[lnSize,2] = 0
    ENDIF
  ENDFOR
  REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
ELSE
  m.Style = lcStyle
  =lfaddError(.F.,'SCL')                    
ENDIF

*:**************************************************************************
*:* Name        : lfCheckQty
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : valid function of the salesrep field in the option grid
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCheckQty()
*:***************************************************************************
FUNCTION lfCheckQty
lnQty = 0
FOR lnCount = 1 TO 10
  lnQty = lnQty + laSize[lnCount,2]
ENDFOR
RETURN (lnQty > 0) 

*:**************************************************************************
*:* Name        : lfVPrnt
*:* Developer   : Walid A. Wahab (WAB)
*:* Date        : 02/18/2003
*:* Purpose     : valid function of print button in the log screen
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVPrnt()
*:***************************************************************************
FUNCTION lfVPrnt
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY TO (gcOutFile) TYPE DELIMITED
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*:**************************************************************************
*:* Name        : lfChecksz                          B122297,1 
*:* Developer   : Ashraf Sherif (ASH)
*:* Date        : 04/18/2004
*:* Purpose     : Function to check that all sizes are included in the heading scale line
*:***************************************************************************
*:* Called from : from option grid
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChecksz()
*:***************************************************************************
FUNCTION lfChecksz
FOR lnCount = 1 TO 10
  IF laSize[lnCount,2] <> 0 AND EMPTY(laSize[lnCount,1])
    RETURN .F.
  ENDIF
ENDFOR
RETURN .T.
