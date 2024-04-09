*:****************************************************************************
*: Program file        : SOXLSTY.PRG
*: Program description : Import Sales orders from Excel files
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SO)
*: Developer Name      : TMI - TAREK MOHAMED IBRAHIM
*: Date completed      : 08/01/2002
*: Tracking Job Number : C200378
*:****************************************************************************
* This program reads from an Excel file , import data to a Fox2 file , reads from this file
* appends Sales Orders data to ordhdr , ordline files
* For this to done successfully , all conditions must be satisfied , i.e. Currency exists,
* Account exists , sales reps exists , Styles exists .
* 
* Excel sheet fields meaning
* A --> Field Holds CURRENCY CODE
* B --> Field Holds ACCOUNT NUMBER
* C --> Field Holds ACCOUNT NAME
* D --> Field Holds REP. CODE	
* E --> Field Holds COMMISS RATE
* F --> Field Holds ORDER NUMBER
* G --> Field Holds DELIVERY DATE
* H --> Field Holds Reference
* I --> Field Holds Ref.2
* J --> Field Holds STYLE
* K --> Field Holds COLOUR
* L,M,...,U  Fields Hold Sizes
* V --> Field Holds TOTAL
* W --> Field Holds PRICE
* X --> Field Holds DESCRIPTION
*
*:****************************************************************************
PRIVATE lcOldValue,lcRpXlsFil,lcRpSeason,lcRpDiv,lcRpWarh,laSeason,laDivision,lcCodName,;
        lcOrdHdr,lcOrdLine,lnErrCnt,lcXls,lcLogFile,lcErroSrc
STORE '' TO lcOldValue,lcRpXlsFil,lcRpSeason,lcRpDiv,lcRpWarh,lcCodName
STORE 0 TO lnErrCnt        && Increased by 1 for every error occured in data imported
lcOrdHdr   = gfTempName()
lcOrdLine  = gfTempName()
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

*--Call the option grid
lcExpr = gfOpGrid('SOXLSTY' , .T.)

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

*:**************************************************************************
*:* Name        : lfOrdImprt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
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

DIMENSION laSize[10]         &&Hold sizes names
laSize = ''

CREATE TABLE (gcWorkDir+lcLogFile) (cErro C(120))
INSERT INTO (lcLogFile) VALUES ('-------------------------------------------------------------------------')
INSERT INTO (lcLogFile) VALUES ('Data imported from file '+lcRpXlsFil+' at Date:'+DTOC(DATE())+' Time:'+TIME())
INSERT INTO (lcLogFile) VALUES ('-------------------------------------------------------------------------')

*--Open Needed files
=lfOpenFls()

*--Create needed temp files
=lfCratTemp()

*--Import data from the excel file to a temp file 
SELECT 0
lcXls = SUBSTR(lcRpXlsFil,RAT('\',lcRpXlsFil)+1,;
                               RAT('.',lcRpXlsFil)-1-RAT('\',lcRpXlsFil))
IF !USED(lcXls)  
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
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There is another file with the same name of '+;
                                           'the selected Excel file , Please rename.')
  =lfCloseFls()
  RETURN
ENDIF

WAIT WINDOW NOWAIT 'Removing unneeded records....'
=AFIELDS(laFlds)
GO TOP
lcDele = SET('DELETED')
SET DELETED OFF

M.REP1 = D
M.ORDER = F

SCAN
  *llDelete = .T.
  *FOR lnCnt = 1 TO ALEN(laFlds,1)
  *  IF !EMPTY(EVAL(laFlds(lnCnt,1)))
  *    llDelete = .F.
  *    EXIT
  *  ENDIF  
  *ENDFOR
  *IF llDelete
  IF EMPTY(J) AND EMPTY(L)
    DELETE
  ELSE    
    *--Fill gaps with rep code and order #
    IF EMPTY(D) 
      REPLACE D WITH M.REP1
    ENDIF
    M.REP1 = D

    IF EMPTY(F)
      REPLACE F WITH M.ORDER
    ENDIF
    M.ORDER = F  
  ENDIF
ENDSCAN

SET DELETED ON
GO TOP

*-- Locate the first record to get data from

LOCATE FOR !EMPTY(L) AND !('SIZE'$UPPER(L))

*--Fill laSize array - start from from column L to U / 10-size scale
laSize = ''
FOR lnCnt = 1 TO 10
  lcSz = EVAL(CHR(75+lnCnt))
  IF !EMPTY(lcSz)
    lcSz = IIF(TYPE('lcSz')='N',STR(lcSz),lcSz)
    laSize[lnCnt] = PADR(ALLT(lcSz),5)
  ENDIF
ENDFOR

*--Locate first line in the order
LOCATE REST FOR !EMPTY(A)

DO WHILE !EOF(lcXls)
  *--Initialize memory vairables
  SELECT ORDHDR
  SCATTER MEMVAR MEMO BLANK
  SELECT ORDLINE
  SCATTER MEMVAR MEMO BLANK
  STORE 1 TO m.NCURRUNIT,m.NEXRATE
  
  SELECT (lcXls)
  m.CCURRCODE = PADR(ALLT(A),3)
  m.ACCOUNT = PADR(ALLT(B),5)
  m.CORDTYPE = 'O'
  m.STATUS = 'O'
  m.SEASON = PADR(lcRpSeason,6)
  m.CDIVISION = PADR(lcRpDiv ,6)
  m.CWARECODE = PADR(lcRpWarh,6) 
   
  IF SEEK(m.CCURRCODE,'SYCCURR')
    m.NCURRUNIT = SYCCURR.NCURRUNIT
    IF SEEK(gcBaseCurr+m.CCURRCODE,'SYCEXCH')
      m.NEXRATE = SYCEXCH.NEXRATE
    ENDIF
  ELSE
    =lfSkip('EMPTY(A)','CUR')          &&- if currency not found skip this account orders
    LOOP
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
  ELSE    
    =lfSkip('EMPTY(A)','ACC')             &&- if account not found skip this account orders
    LOOP
  ENDIF  

  llAccount = .T.
    
  DO WHILE llAccount .AND. !EOF(lcXls)
  
    *--Populate order variables
    m.ORDER = PADL(ALLT(F),6,'0')
    m.REP1 = PADR(ALLT(D),3)
    m.COMM1 = VAL(E)
    STORE lfCTOD(G) TO m.ENTERED,m.START,m.COMPLETE
    m.NOTE1 = ALLTRIM(H)
    m.NOTE2 = ALLTRIM(I)
    m.CREORDER = 'N'
    m.MULTI = 'N'
    m.Cinsur = 'Y'
    M.LERRORS = .F.         && Define that this order is error free 
    
    IF !SEEK(m.REP1,'SALESREP')
      =lfSkip([PADL(ALLT(F),6,'0') = M.ORDER],'REP')             &&- if sales rep is not defined skip this order
      llAccount = EMPTY(B)
      LOOP  
    ENDIF  
    
    WAIT WINDOW NOWAIT 'Adding Order# ' + m.Order + ' For Account# '+ m.Account

    DO WHILE PADL(ALLT(F),6,'0') = M.ORDER AND !EOF(lcXls)
      lcStyClr = PADR(ALLT(J),12) + '-' + PADR(ALLT(K),3)             && Style major + Clr      
      IF SEEK(lcStyClr,'STYLE')      
        lcScl = SUBSTR(STYLE.STYLE,17)
        SELECT Scale,CNT,SZ1,0,SZ2,0,SZ3,0,SZ4,0,SZ5,0,SZ6,0,SZ7,0,SZ8,0,0 ;
        FROM SCALE ;
               WHERE Type+Scale='S'+SUBSTR(lcScl,1,lnScaleLen) ;
               ORDER BY 1 ;
               INTO ARRAY laExtSz
        
        llAllZero = .T.  && Check if at least one qty > 0 
        llSkipFor = .F.        
        FOR lnCnt = 1 TO 10
          lnVal = EVAL(CHR(75+lnCnt))
          lnVal = IIF(TYPE('lnVal')='N',lnVal,VAL(lnVal))
          
          IF lnVal > 0
            llAllZero = .F.
            lnPos = IIF(EMPTY(laSize[lnCnt]), 0 , ;
                        ASCAN(laExtSz,laSize[lnCnt]))
            IF lnPos>0
              laExtSz[lnPos+1] = lnVal
              lnRow = ASUBSCRIPT(laExtSz,lnPos,1)
              laExtSz[lnRow,19] = laExtSz[lnRow,19] + lnVal
            ELSE
              llSkipFor = .T.
              EXIT
            ENDIF
          ENDIF
        ENDFOR
        M.STYLE = lcStyClr
        IF llSkipFor
          =lfSkip(.F.,'SCL')
          LOOP
        ENDIF
        IF llAllZero
          =lfSkip(.F.,'QTY')
          LOOP
        ENDIF
        
        m.GROS_PRICE = VAL(W)
        m.Price = VAL(W)

        FOR lnCnt = 1 TO ALEN(laExtSz,1)
          IF laExtSz[lnCnt,19]>0
            m.Style = lcStyClr + laExtSz[lnCnt,1]
            m.Scale = laExtSz[lnCnt,1]
            IF SEEK(m.Style,'STYLE')
              IF STYLE.SEASON = PADL(lcRpSeason,6) AND STYLE.CDIVISION = PADL(lcRpDiv,6)
                STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Totqty,;
                           m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8,;
                           m.TotBook
                FOR lnK = 1 TO laExtSz[lnCnt,2]
                  lcK = STR(lnK,1)
                  lnQty = laExtSz[lnCnt,lnK*2+2]
                  IF lnQty>0
                    m.Book&lcK = lnQty
                    m.Qty&lcK = lnQty
                    m.Totbook = m.Totbook + lnQty
                    m.Totqty = m.Totqty + lnQty
                    m.BOOK = m.BOOK + lnQty
                    m.OPEN = m.OPEN + lnQty
                    m.BOOKAMT = m.BOOKAMT + m.Price*lnQty
                    m.OPENAMT = m.OPENAMT + m.Price*lnQty
                  ENDIF
                ENDFOR
                m.Desc1 = STYLE.Desc1
                m.LINENO = m.LINENO + 1
                m.LASTLINE = m.LASTLINE + 1
                INSERT INTO (lcOrdLine) FROM MEMVAR
                =lfAddUsrDt(lcOrdLine)
              ELSE
                =lfSkip(.F.,'SDV',.T.)                
              ENDIF
            ELSE              
              =lfSkip(.F.,'STY',.T.)
            ENDIF
          ENDIF
        ENDFOR        
        
        =lfSkip(.F.,'')  && Normal skip
        
      ELSE        
      
        *-- if style is not defined in the system skip this line
        M.STYLE = lcStyClr
        =lfSkip(.F.,'STY')
      
      ENDIF
      
    ENDDO  && For order

    *--Not to add previously added order
    IF m.OPEN > 0 AND !SEEK('O'+M.ORDER,'ORDHDR')
      INSERT INTO (lcOrdHdr) FROM MEMVAR
      =lfAddUsrDt(lcOrdHdr)
    ENDIF        
    
    STORE 0  TO m.BOOK,m.OPEN,m.BOOKAMT,m.OPENAMT
    
    llAccount = EMPTY(B)
  ENDDO
    
ENDDO

WAIT CLEAR
SET DELETED &lcDele
SELECT (lcOrdHdr)

IF lnErrCnt>0 
  *-- Button  : 32013
  *-- Options : View the log file , continue , Cancel
  lnResp = gfModalGen('INM00000B32013',.F.,.F.,.F.,'Some problems are detected.')
  DO CASE
    CASE lnResp = 1
      =lfErrFle()    
    CASE lnResp = 2
      =lfSaveData()
    CASE lnResp = 3
      RETURN
  ENDCASE    
ELSE
  =lfSaveData()
ENDIF

*--Close temp files
=lfCloseFls()

*-- end of lfImport.

*:**************************************************************************
*:* Name        : lfSkip
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : SKip current order08/01/2002
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSkip()
*:***************************************************************************
FUNCTION lfSkip
PARAMETERS lcExpr,lcReason,llNoSkip
PRIVATE lcTxt,lnCnt
SELECT (lcXls)

*-- Write to alternate file
*lcTxt = 'LINE#('+PADL(RECNO(lcXls),6)+') '
*lcTxt = 'Acc/Order '+M.ACCOUNT+'/'+PADL(ALLT(F),6,'0')+' '
lcTxt = 'LINE#('+PADL(RECNO(lcXls),6)+') '+'Acc/Order '+M.ACCOUNT+'/'+ALLT(F)+' '
IF !EMPTY(lcReason)
  DO CASE
    CASE lcReason = 'CUR'
      lcTxt = lcTxt + 'Currency '+m.CCURRCODE+' not found.'  &&, all the order is ignored'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'ACC'
      lcTxt = lcTxt + 'Account not found.'     &&, all the order is ignored.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
      
    CASE lcReason = 'REP'
      lcTxt = lcTxt + 'Sales rep '+m.Rep1+' not found.'      &&, all the order is ignored.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'STY'
      lcTxt = lcTxt + 'Style '+M.STYLE+' not found.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)

    CASE lcReason = 'SCL'
      lcTxt = lcTxt + 'Style '+M.STYLE+' Wrong Scale/Size or Scale/Size not found.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
      
    CASE lcReason = 'QTY'
      lcTxt = lcTxt + 'Style '+M.STYLE+' All quantities in this line are Zeros!'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
   
    CASE lcReason = 'SDV'
      lcTxt = lcTxt + 'Style '+M.STYLE+' Season or Division not match the selected.'
      INSERT INTO (lcLogFile) VALUES (lcTxt)
  ENDCASE
  *--Increase error counter
  lnErrCnt = lnErrCnt + 1
  M.LERRORS = .T.
ENDIF

*--Make normal skip
IF !llNoSkip
  SKIP
ENDIF
IF EOF()
  RETURN
ENDIF

lnRecno = 0
IF !EMPTY(lcExpr)
  WAIT WINDOW NOWAIT 'Skip Order# '+ F
  lc255 = REPL(CHR(255),3)
  LOCATE REST FOR A=lc255 WHILE EVAL(lcExpr)
  IF EOF()
    RETURN
  ENDIF
  lnRecno = RECNO(lcXls)
  DO WHILE !EMPTY(J) 
    SKIP -1
  ENDDO
ENDIF

*--Check new scale/size
IF EMPTY(J)
  laSize = ''        
  FOR lnCnt = 1 TO 10
    lcSz = EVAL(CHR(75+lnCnt))
    IF !EMPTY(lcSz)
      lcSz = IIF(TYPE('lcSz')='N',STR(lcSz),lcSz)
      laSize[lnCnt] = PADR(ALLT(lcSz),5)
    ENDIF
  ENDFOR
  SKIP          
  IF BETWEEN(lnRecno,1,RECCOUNT(lcXls)) 
    GOTO (lnRecno)
  ENDIF
ENDIF
*-- end of lfSkip.


*:**************************************************************************
*:* Name        : lfChkXls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2002
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
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2002
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
DO (gcScrDir +  gcAct_Appl + '\Soimeror.SPX')

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

IF USED(lcLogFile)
  USE IN (lcLogFile)
ENDIF
ERASE (gcWorkDir)+(lcLogFile)+".DBF"

*-- end of lfErrFle.

*:**************************************************************************
*:* Name        : lfwRpWhen() 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : When OG Function08/01/2002
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
ENDIF

*--Disable/Enable pbRun button via lfChkFlds() Fn.
=lfChkFlds()

*-- end of lfwRpWhen.

*:**************************************************************************
*:* Name        : lfvXlsFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : Select Excel (*.xls) file to import Data from08/01/2002
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
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : When function for Get Excel (*.xls) file08/01/2002
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
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : Check all fields since All fields are mandatory 08/01/2002
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
lcStat = IIF(!EMPTY(lcRpXlsFil) AND !EMPTY(lcRpWarh),'ENABLE','DISABLE')
SHOW GET pbRun &lcStat
*-- end of lfChkFlds.

*:**************************************************************************
*:* Name        : lffillcode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : Fill code fn08/01/2002
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
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : Add user id ,date  and time to files08/01/2002
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
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/01/2002
*:* Purpose     : Get the default code08/01/2002
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
*! Developer : TMI
*! Date      : 08/01/2002
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

*-- end of lfCratTemp.

*:**************************************************************************
*:* Name        : lfvWarhous
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/16/2002
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
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/06/2002
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
=gfCloseFile('WAREHOUS')

lnWareHouse = ASCAN(laWareHouses,lcODefWare)
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))

laScrMode[4] = .T.
STORE .F. TO llMFDsPrc , llPoDsPrc
llOrdSaved = .F.
*--Loop throght the temp file lcOrdHdr to save data
SELECT (lcOrdHdr)
SCAN FOR !LERRORS
  llOrdSaved = .T.
  SCATTER FIELDS &lcScFields TO laData
  SELECT &lcOrdLine
  SET FILTER TO CORDTYPE+ORDER = 'O'+&lcOrdHdr..ORDER
  GO TOP
  SELECT &lcOrdHdr
  llContinue = .T.
  DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
ENDSCAN
IF !llOrdSaved
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ENDIF

*-- end of lfSaveData.

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/07/2002
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
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/07/2002
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
ERASE (gcWorkDir+lcXls+'.DBF')

*-- end of lfCloseFls.


*:**************************************************************************
*:* Name        : lfCTOD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/07/2002
*:* Purpose     : Solve Y2K problem for dates
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCTOD()
*:***************************************************************************
FUNCTION lfCTOD
PARAMETERS lcDate
*-Date setting is DD/MM/YYYY
PRIVATE lcDay,lcMonth,lcYear,ldRetDate
STORE  '' TO lcDay,lcMonth,lcYear

lcDay = SUBSTR(lcDate,1,AT('/',lcDate)-1) 
lcMonth = SUBSTR(lcDate,AT('/',lcDate)+1,AT('/',lcDate,2)-AT('/',lcDate)-1)
lcYear = SUBSTR(lcDate,AT('/',lcDate,2) )
lcYear = RIGHT(ALLT(lcYear),2)
IF VAL(lcYear)>=30
  lcYear = '19'+lcYear
ELSE
  lcYear = '20'+lcYear
ENDIF
ldRetDate = CTOD(lcDay+'/'+lcMonth+'/'+lcYear)
RETURN ldRetDate
*-- end of lfCTOD.

*:**************************************************************************
*:* Name        : lfvErrSrc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/18/2002
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

