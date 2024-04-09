*:****************************************************************************
*: Program file        : SOXLIMP.PRG
*: Program description : EXCEL Orders Upload program 
*: Aria customer       : Falcon Bay Sportswear  ( FAL03 )
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SO2)
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : *C102818,1
*: Date developed      : 08/10/2003
*:****************************************************************************
* Modifications
*C102818,5 TMI 08/26/2003 reject order with style that has zero price
*B119833,1 TMI 10/13/2003 Include the commesion of NWB association
*:***************************************************************************

*--Calling the option grid
PRIVATE lnCsvRecs
lnCsvRecs = 26   && starting line in the csv file where order detail to be found

STORE ' ' TO lcOldValue,lcRpArcFld,lcRpImpFld

lcExpr = gfOpGrid('SOXLIMP' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcExpr)
  RETURN
ENDIF

*- Save selected valued of csv and archive folders
SAVE ALL LIKE lcRp???Fld TO (gcDataDir+'ImpFldr.MEM')

=lfImport()

*:**************************************************************************
*:* Name        : lfwRpWhen() 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : When OG Function
*:***************************************************************************
FUNCTION lfwRpWhen

*-- if it is the default case
IF lnOgSeting = 1
  IF FILE(gcDataDir+'ImpFldr.MEM')
    RESTORE FROM (gcDataDir+'ImpFldr.MEM') ADDITIVE
  ENDIF  
  =lfvChkRnBt()
ENDIF

*-- end of lfwRpWhen.

*:**************************************************************************
*:* Name        : lfwOldValue
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/10/2003
*:* Purpose     : Old saved name of EXCEL CSV order files folder
*:***************************************************************************
*:* Called from : OG. When fn of "lcRpImpFld"
*:***************************************************************************
FUNCTION lfwOldValue
lcOldValue = EVAL(SYS(18))

*-- end of lfwOldFldr.

*:**************************************************************************
*:* Name        : lfvImFlFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/10/2003
*:* Purpose     : Valid function of EXCEL CSV order files folder
*:***************************************************************************
*:* Called from : OG. Valid fn of "lcRpImpFld","lcRpArcFld" variables in OG
*:***************************************************************************
FUNCTION lfvImFlder
PARAMETERS lcFldVar
PRIVATE lcTmpFl
DO WHILE .T.
  IF EMPTY(&lcFldVar)
    EXIT
  ENDIF  
  IF ALLTRIM(lcOldValue) # ALLTRIM(&lcFldVar)
    &lcFldVar = &lcFldVar + IIF( RIGHT(ALLTRIM(&lcFldVar),1) = '\' , '' , '\' )
    lcTmpFl = gfTempName()
    lnHndl = FCREATE(&lcFldVar+lcTmpFl)
    IF lnHndl > 0
      =FCLOSE(lnHndl)
      ERASE (&lcFldVar+lcTmpFl)
      EXIT
    ELSE
      &lcFldVar = GETDIR()
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO

=lfvChkRnBt()

*-- end of lfvImFlFld.
*:**************************************************************************
*:* Name        : lfvChkRnBt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/13/2003
*:* Purpose     : Check Run button to be enabled
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
FUNCTION lfvChkRnBt
PRIVATE lcStat
lcStat = IIF( !EMPTY(lcRpImpFld) .AND. ;
              !EMPTY(lcRpArcFld) .AND. ;
              lcRpImpFld <> lcRpArcFld .AND. ;
              lfChkCsv() , 'ENABLE','DISABLE')
SHOW GET pbRun &lcStat

*-- end of lfvChkRnBt.

*:**************************************************************************
*:* Name        : lfChkCsv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/08/2003
*:* Purpose     : Check that there is files in the Excel CSV Import folder
*:***************************************************************************
*:* Called from : lfvChkRnBt
*:***************************************************************************
FUNCTION lfChkCsv
PRIVATE laCsvFls
DIMENSION laCsvFls[1,5]
STORE '' TO laCsvFls
=ADIR(laCsvFls,lcRpImpFld+'*.csv')
RETURN !EMPTY(laCsvFls[1])
*-- end of lfChkCsv.

*:**************************************************************************
*:* Name        : lfImport
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/10/2003
*:* Purpose     : Import orders from the Excel files located in the selected Folder
*:***************************************************************************
FUNCTION lfImport
PRIVATE laCsvFiles,lcDefWH,lcDefDiv,lnCnt,lcString,m.Ord1st,m.OrdLst,lcSafe,lcSTYLE,lnMajLen

*- lcString : Holds the contents of the csv opened file
*- lcDefWH  : Default Warehous
*- lcDefDiv : Default Division
*- m.Ord1st : First saved order#
*- m.OrdLst : Last saved order#

*- Get the setup optins
lcHOLD_ORD = gfGetMemVar('M_HOLD_ORD')  && "Default SO status to Hold"
lnCOMPDATE = gfGetMemVar('M_COMPDATE')  && "Default SO comp. days    "

*- Major lenght of the style
lnMajLen = LEN(gfItemMask("PM"))

*- Current import session 
m.CIMPSESSID = gfSequence('CIMPSESSID')

DIMENSION laCsvFiles[1,5]
STORE '' TO laCsvFiles
STORE '' TO lcDefWH,lcDefDiv,m.Ord1st,m.OrdLst

*-- Assign temp names
lcOrdHdr  = gfTempName()
lcOrdLine = gfTempName()
lcHdr     = gfTempName()
lcLine    = gfTempName()
lcTmpOrd  = gfTempName()
lcXLIMPSES= gfTempName()

*- Open needed files
=lfOpenFls()

*B119833,1  TMI [Start] Get Northwest Buyers assoc value , here it is '511' for FAL03
lcNWB = ''
lcNWB = gfGetMemVar('M_NWB')  
IF !SEEK(PADR(lcNWB,3),'SALESREP')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The Northwest Buyers Assoc. code in SR module setup is either empty '+;
                                           'or not found in the SalesRep file, '+;
                                           'Pls check SR module setup.')
  RETURN                                           
ENDIF
*B119833,1  TMI [End  ] 
 
*- Populate default for season ,div and WH
=lfPopDef()

*- Create temp files for Ordhdr and Ordline
=lfCratTemp()

*- Get files in the selected folder
=ADIR(laCsvFiles,lcRpImpFld+'*.CSV')

*- ----------------- --------------------------- ----------------- -*
*- ----------------- -Porcessing Import process- ----------------- -*
*- ----------------- --------------------------- ----------------- -*
FOR lnCnt = 1 TO ALEN(laCsvFiles,1)
  
  *- Get the data from the csv file
  =lfAppFrCsv()
  
  *-- Intialize memory variables for fields
  SELECT &lcHdr
  ZAP
  SCATTER MEMVAR MEMO BLANK
  SELECT &lcLine
  ZAP
  SCATTER MEMVAR MEMO BLANK
 
  *- Get the csv file data
  m.SEASON    = PADR(lfReadCell('C3'),6)
  m.ENTERED   = lfReadCell('N6','D',gdSysDate)
  m.REP1      = lfReadCell('R7')
  m.ACCOUNT   = lfReadCell('N8')
  m.ALT_SHPTO = !EMPTY(lfReadCell('C9'))
  IF m.ALT_SHPTO
    M.STNAME    = lfReadCell('B9')
    M.CADDRESS1 = lfReadCell('B10')
    *C102818,4  TMI [Start] No field labled B12, it is H11 for zip code  
    *M.CADDRESS3 = lfReadCell('B11') +','+ lfReadCell('B12')    
    M.CADDRESS3 = lfReadCell('B11') +','+ lfReadCell('H11')    
    *C102818,4  TMI [End  ] 
  ENDIF
  M.DEPT     = lfReadCell('H12')
  M.CUSTPO   = lfReadCell('H13')
  M.START    = lfReadCell('K13','D',gdSysDate)
  M.COMPLETE = lfReadCell('N13','D',gdSysDate+lnCOMPDATE)
  
  *-- Get other fields data
  
  *- Account data
  IF SEEK('M'+m.ACCOUNT,'CUSTOMER')
    m.StName    = IIF( m.ALT_SHPTO , m.StName , CUSTOMER.StName )
    m.Buyer     = CUSTOMER.Buyer
    m.Phone     = CUSTOMER.Phone1
    m.Disc      = CUSTOMER.Disc
    m.cTermCode = CUSTOMER.cTermCode
    m.ShipVia   = CUSTOMER.ShipVia
    m.Spcinst   = CUSTOMER.Spcinst   
    m.Priority  = CUSTOMER.Priority  
    m.Link_Code = IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code) 
    m.GL_Sales  = IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)  
    M.PRIORITY  = CUSTOMER.PRIORITY
    m.CCURRCODE = CUSTOMER.CCURRCODE
    m.cFacCode  = CUSTOMER.cFacCode
    *B119833,1  TMI [Start] Calculate the rep commession, taking in considration the commession of BWN group.
    *m.COMM1     = IIF(M.REP1=CUSTOMER.SALESREP,CUSTOMER.COMM,0)
    
    DO CASE    
      CASE M.REP1 = CUSTOMER.SALESREP
        M.COMM1 = CUSTOMER.COMM
        IF lcNWB = CUSTOMER.REP2 .AND. M.REP1 <> lcNWB
          M.REP2 = lcNWB
          M.COMM2 = CUSTOMER.COMM2
        ENDIF        

      CASE M.REP1 = CUSTOMER.REP2
        M.REP2 = M.REP1
        M.COMM2 = CUSTOMER.COMM2
        M.REP1 = '   '
        IF lcNWB = CUSTOMER.SALESREP .AND. M.REP2 <> lcNWB
          M.REP1 = lcNWB
          M.COMM1 = CUSTOMER.COMM
        ENDIF
    
      OTHERWISE

        IF SEEK(PADR(M.REP1,3),'SALESREP')
          M.COMM1 = SALESREP.COMM
          IF lcNWB = CUSTOMER.SALESREP .AND. M.REP1<> lcNWB
            M.REP2 = CUSTOMER.SALESREP
            M.COMM2 = CUSTOMER.COMM
          ENDIF
          IF lcNWB = CUSTOMER.REP2 .AND. M.REP1<> lcNWB
            M.REP2 = CUSTOMER.REP2
            M.COMM2 = CUSTOMER.COMM2
          ENDIF
        ELSE
          IF lcNWB = CUSTOMER.SALESREP
            M.REP1 = CUSTOMER.SALESREP
            M.COMM1 = CUSTOMER.COMM
          ENDIF
          IF lcNWB = CUSTOMER.REP2
            M.REP1 = CUSTOMER.REP2
            M.COMM1 = CUSTOMER.COMM2
          ENDIF
        ENDIF           
    ENDCASE
    
    *- if only second rep comes,let it be rep1
    IF EMPTY(M.REP1) .AND. !EMPTY(M.REP2)
      M.REP1 = M.REP2
      M.COMM1 = M.COMM2
      M.REP2 = '   '
      M.COMM2 = 0
    ENDIF
    *B119833,1  TMI [End  ] 
    
  ENDIF  

  *- Commesion
  *B119833,1  TMI [Start] Comment out these lines
  *IF M.COMM1 = 0 .AND. SEEK(m.REP1,'SALESREP')
  *  M.COMM1 = SALESREP.COMM
  *ENDIF
  *B119833,1  TMI [End  ] 

  STORE 1 TO m.NCURRUNIT,m.NEXRATE  
  STORE 'N' TO M.BULK, M.CREORDER, M.MULTI
  STORE 'Y' TO M.CINSUR
  m.CORDTYPE = 'O'
  m.STATUS = IIF(lcHOLD_ORD $ 'Y'+IIF(!EMPTY(m.cFacCode),'F','') ,'H','O')
  m.CDIVISION = lcDefDiv
  m.CWARECODE = lcDefWH
  
  *-- User ID , date and time
  M.CADD_USER = gcUser_ID
  M.CADD_TIME = Time()
  M.DADD_DATE = gdSysDate
  M.CADD_VER  = 'A27'

  *------------------------------      Get order details      ------------------------------*
  
  *- if no lines in the csv file let the temp file be reach eof
  SELECT &lcTmpOrd
  IF RECCOUNT(lcTmpOrd) < lnCsvRecs
    GO BOTTOM
    IF !EOF()
      SKIP
    ENDIF
  ELSE
    GOTO lnCsvRecs
  ENDIF
  
  M.LINENO = 0
  SCAN REST FOR !DELETED()
    STORE 0 TO M.QTY1 ,M.QTY2 ,M.QTY3 ,M.QTY4 ,M.QTY5 ,M.QTY6 ,M.QTY7 ,M.QTY8 ,M.TOTQTY ,;
               M.BOOK1,M.BOOK2,M.BOOK3,M.BOOK4,M.BOOK5,M.BOOK6,M.BOOK7,M.BOOK8,M.TOTBOOK
    
    lcSTYLE = PADR( ALLTRIM(m.SEASON) + '-' + ALLTRIM(&lcTmpOrd..A) , lnMajLen ) + IIF(!EMPTY(&lcTmpOrd..C), '-'+PADR(&lcTmpOrd..C,6) ,'')
    lcSTYLE = UPPER(lcSTYLE)
    M.STYLE = IIF( SEEK(lcSTYLE,'STYLE') , STYLE.STYLE , lcSTYLE )

    M.SCALE = STYLE.SCALE
    M.DESC1 = STYLE.DESC1
    
    STORE VAL(CHRTRAN(&lcTmpOrd..E,'$','')) TO M.PRICE,M.GROS_PRICE
    STORE VAL(CHRTRAN(&lcTmpOrd..J,'$','')) TO M.QTY1,M.BOOK1
    STORE VAL(CHRTRAN(&lcTmpOrd..K,'$','')) TO M.QTY2,M.BOOK2
    STORE VAL(CHRTRAN(&lcTmpOrd..L,'$','')) TO M.QTY3,M.BOOK3
    STORE VAL(CHRTRAN(&lcTmpOrd..M,'$','')) TO M.QTY4,M.BOOK4
    STORE VAL(CHRTRAN(&lcTmpOrd..N,'$','')) TO M.QTY5,M.BOOK5
    STORE VAL(CHRTRAN(&lcTmpOrd..O,'$','')) TO M.QTY6,M.BOOK6
    STORE VAL(CHRTRAN(&lcTmpOrd..P,'$','')) TO M.QTY7,M.BOOK7
    STORE VAL(CHRTRAN(&lcTmpOrd..Q,'$','')) TO M.QTY8,M.BOOK8
    M.TOTQTY = M.QTY1+M.QTY2+M.QTY3+M.QTY4+M.QTY5+M.QTY6+M.QTY7+M.QTY8
    M.TOTBOOK = M.TOTQTY         
    M.nExcelLine = RECNO(lcTmpOrd)

    INSERT INTO &lcLine FROM MEMVAR

    M.OPEN    = M.OPEN    + M.TOTQTY
    M.OPENAMT = M.OPENAMT + M.TOTQTY*M.PRICE
    M.BOOK    = M.OPEN
    M.BOOKAMT = M.OPENAMT
    
  ENDSCAN  
  
  INSERT INTO &lcHdr FROM MEMVAR
  
  *- Check order errors and if error free accept this order
  m.MERR = lfErrFree()
  IF EMPTY(m.MERR)
    SELECT &lcHdr
    SCATT MEMVAR MEMO
    M.ORDER = gfSequence('ORDER')
    INSERT INTO &lcOrdHdr FROM MEMVAR

    SELECT &lcLine
    REPLACE ORDER WITH M.ORDER ALL
    SELECT &lcOrdLine 
    APPEND FROM (gcWorkDir+lcLine)
    
    *- Update lines # in lcOrdLine and lcOrdHdr
    =SEEK('O'+M.ORDER,lcOrdLine)
    lcSvOrd = ORDER(lcOrdLine)
    SET ORDER TO '' IN &lcOrdLine
    
    M.LINENO = 0
    M.LASTLINE = 0
    REPLACE &lcOrdHdr..LASTLINE WITH M.LASTLINE
    SELECT &lcOrdLine
    GO TOP
    SCAN FOR &lcOrdLine..ORDER = M.ORDER
      M.LINENO = M.LINENO + 1
      M.LASTLINE = M.LINENO
      REPLACE &lcOrdLine..LINENO  WITH M.LINENO
      REPLACE &lcOrdHdr..LASTLINE WITH M.LASTLINE
    ENDSCAN
    SET ORDER TO (lcSvOrd) IN &lcOrdLine
  ENDIF
  
  *- Update the Import sessions temp file "lcXLIMPSES" file in both cases ( there is error or no errors )
  m.CXLSFILE  = SUBSTR(laCsvFiles[lnCnt,1] , 1 , AT('.',laCsvFiles[lnCnt,1]) - 1 )
  m.DSESSDATE = gdSysDate
  m.CSESSTIME = TIME()
  m.CSTATUS   = IIF(EMPTY(M.MERR) , 'A' , 'R' )
  INSERT INTO &lcXLIMPSES FROM MEMVAR
  
  *- Copy the imported csv file from the csv folder to the Archive folder and erase it from 
  *- the csv folder
  lcSafe = SET('SAFE')
  SET SAFE OFF  
  IF m.CSTATUS = 'A'    
    COPY FILE (lcRpImpFld+laCsvFiles[lnCnt,1]) TO (lcRpArcFld+laCsvFiles[lnCnt,1])
    ERASE (lcRpImpFld+laCsvFiles[lnCnt,1])
    *B119833,1  TMI [Start] Warn if the file can not be deleted 
    IF FILE(lcRpImpFld+laCsvFiles[lnCnt,1])
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The file '+lcRpImpFld+laCsvFiles[lnCnt,1]+;
                            ' is successfully uploaded and can not be deleted, pls Delete it manually.')
    ENDIF
    *B119833,1  TMI [End  ] 
  ENDIF
  SET SAFE &lcSafe  
     
ENDFOR

SELECT &lcOrdHdr
GOTO TOP
*- if no order accepted then save else show a message
IF EOF()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No orders saved.')
ELSE 
  DO lpSavOrdr
ENDIF  

*- View and print report showing the status of each of the order processed during that session
=lfViewRep()

*- Update the XLIMPSES file
SELECT &lcXLIMPSES
SCAN
  SCATT MEMVAR MEMO
  INSERT INTO XLIMPSES FROM MEMVAR
ENDSCAN

*- Close opened temp files
=lfCloseFls()

*-- end of lfImport.

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/14/2003
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpenFls
*C102818,5  TMI [Start] Ask for any file if it is opened befor opening it
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE',    'STYLE','SH')
ENDIF  
IF !USED('SCALE')
  =gfOpenFile(gcDataDir+'SCALE',    'SCALE','SH')
ENDIF  
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS', 'WAREHOUS','SH')
ENDIF  
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER', 'CUSTOMER','SH')
ENDIF  
IF !USED('SALESREP')
  =gfOpenFile(gcDataDir+'SALESREP', 'SALESREP','SH')
ENDIF  
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR',   'ORDHDR','SH')
ENDIF  
IF !USED('ORDLINST')
  =gfOpenFile(gcDataDir+'ORDLINE',  'ORDLINST','SH')
ENDIF  
IF !USED('CCURRCODE')
  =gfOpenFile(gcSysHome+'SYCCURR',  'CCURRCODE','SH')
ENDIF  
IF !USED('CURRENCY')
  =gfOpenFile(gcSysHome+'SYCEXCH',  'CURRENCY','SH')
ENDIF  
IF !USED('TRANS')
  =gfOpenFile(gcDataDir+'UnCmSess', 'TRANS','SH')
ENDIF  
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE',   'STYDYE','SH')
ENDIF  

IF !USED('XLIMPSES')
  =gfOpenFile(gcDataDir+'XLIMPSES', 'XLIMPSES','SH')
ENDIF  
IF !USED('CUSTDEPT')
  =gfOpenFile(gcDataDir+'CUSTDEPT', 'CUSTDEPT','SH')
ENDIF  
IF !USED('SPCK_LIN')
  =gfOpenFile(gcDataDir+'SPCK_LIN', 'SPCK_LIN','SH')   && Key :: TYPE+ACCOUNT+PACK_ID+STYLE
ENDIF  
IF !USED('SKU_STYLE')
  =gfOpenFile(gcDataDir+'SPCK_HDR', 'SKU_STYLE','SH')  && Key :: TYPE+ACCOUNT+STYLE+PACK_ID
ENDIF  
IF !USED('CCODE_NO')
  =gfOpenFile(gcDataDir+'CODES',    'CCODE_NO','SH')   && Key :: CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
ENDIF  
IF !USED('SKUTMPL')
  =gfOpenFile(gcDataDir+'SKUTMPL',  'SKUTMPL','SH')
ENDIF  
*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfCratTemp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/10/2003
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

*- Create header temp file
=gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+ORDER],lcOrdHdr)
*- Create header temp file for each order
CREATE TABLE (gcWorkDir+lcHdr) FROM ARRAY laFileStru

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

*- Create line temp file
=gfCrtTmp(lcOrdLine,@laFileStru,@laIndex)
SET ORDER TO TAG 'ORDLINST' IN (lcOrdLine)

*- Create line temp file for each orderline
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'nEXCELLINE'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 4
laFileStru[lnFileStru+1,4] = 0
CREATE TABLE (gcWorkDir+lcLine) FROM ARRAY laFileStru

*- Create temp file to append the csv file 
DIMENSION laFileStru[20,4]
FOR I=1 TO 20
  laFileStru[I,1] = CHR(I+64)
  laFileStru[I,2] = 'C'
  laFileStru[I,3] = 30
  laFileStru[I,4] = 0
ENDFOR  
CREATE TABLE (gcWorkDir+lcTmpOrd) FROM ARRAY laFileStru

*- Create error log file
SELECT XLIMPSES
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcXLIMPSES) FROM ARRAY laFileStru
INDEX ON CSTATUS TAG &lcXLIMPSES

*-- end of lfCratTemp.

*:**************************************************************************
*:* Name        : lfReadCell
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/11/2003
*:* Purpose     : Reads a cell from the excell csv file
*:***************************************************************************
FUNCTION lfReadCell
PARAMETERS lcCell,lcType,leDefault
PRIVATE lnSlct,lnRcno
lnSlct = SELECT()

SELECT &lcTmpOrd
lcType = IIF(EMPTY(lcType),'C',lcType)

lnRcno = VAL(SUBSTR(lcCell,2))
IF BETWEEN(lnRcno,1,RECCOUNT(lcTmpOrd))
  GOTO lnRcno IN &lcTmpOrd  
ELSE
  GO BOTTOM
  IF !EOF()
    SKIP  
  ENDIF
ENDIF

lcRetVal = ALLTRIM(EVAL( lcTmpOrd+'.'+SUBSTR(lcCell,1,1) ))

*- Let lcRetVal be with the passed type
IF !EMPTY(lcType)
  DO CASE
    CASE lcType = 'C'
      lcRetVal = UPPER(ALLTRIM(lcRetVal))
        
    CASE lcType = 'D'
      lcRetVal = lfC2D(lcRetVal)
        
    CASE lcType = 'N'
      lcRetVal = VAL(lcRetVal)
  ENDCASE  
ENDIF
*- If a default value passed assign it to return value if it is empty
IF !EMPTY(leDefault)
  lcRetVal = IIF( !EMPTY(lcRetVal) , lcRetVal , leDefault )
ENDIF

SELECT (lnSlct)  
RETURN lcRetVal
*-- end of lfReadCell.

*:**************************************************************************
*:* Name        : lfC2D
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/13/2003
*:* Purpose     : Get the date from given string in the date format
* NOTE : the funtion CTOD gets the year 190x , this fn. resolves this problem
*:***************************************************************************
FUNCTION lfC2D
PARAMETERS lcDtStr
PRIVATE ldRet
ldRet = CTOD(lcDtStr)
IF !EMPTY(ldRet) .AND. YEAR(ldRet) < 1950
  lcMonth = STR(MONTH(ldRet))
  lcDay   = STR(DAY(ldRet))
  lcYear  = '200'+RIGHT(lcDtStr,1)   && This code is value till 12/31/2009
  ldRet   = CTOD(lcMonth+'/'+lcDay+'/'+lcYear)
ENDIF  
RETURN ldRet
*-- end of lfC2D.
*:**************************************************************************
*:* Name        : lfErrFree
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/11/2003
*:* Purpose     : Collect errors for current csv file 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErrFree()
*:***************************************************************************
FUNCTION lfErrFree
PRIVATE lnSlct,lcRetErrs,lcErrs,lcCR,lnI,lcI,lnRecno,lcClr
lnSlct = SELECT()

STORE '' TO lcRetErrs,lcErrs
lcCR = CHR(13)

*-- Processing to get error if there is [START]
SELECT &lcHdr
GO TOP

*- Check season
IF EMPTY(&lcHdr..SEASON)
  DO lpAddErrLn WITH 'Empty Season field.'
ELSE
  IF !SEEK('NSEASON    '+&lcHdr..SEASON,'CODES')
    DO lpAddErrLn WITH 'Season "'+&lcHdr..SEASON+'" does not exists in Codes file.'
  ENDIF
ENDIF  

*- Check sales rep
IF EMPTY(&lcHdr..REP1)
  DO lpAddErrLn WITH 'Empty Sales Rep. field.'
ELSE
  IF !SEEK(&lcHdr..REP1,'SALESREP')
    DO lpAddErrLn WITH 'Sales Rep. "'+&lcHdr..REP1+'" does not exists in SALESREP file.'
  ELSE
    *IF &lcHdr..COMM1 = 0
    *  DO lpAddErrLn WITH 'Sales Rep. "'+&lcHdr..REP1+'" has zero commesion.'
    *ENDIF
  ENDIF
ENDIF

*- Check Account code
IF EMPTY(&lcHdr..ACCOUNT)
  DO lpAddErrLn WITH 'Empty ACCOUNT field.'
ELSE
  IF SEEK('M'+&lcHdr..ACCOUNT,'CUSTOMER')
    IF CUSTOMER.STATUS $ 'XP'
      DO lpAddErrLn WITH 'ACCOUNT "'+&lcHdr..ACCOUNT+'" has a '+IIF(CUSTOMER.STATUS='X','Cancelled','Potential') +' status.'
    ENDIF
    *- if Department not found in custdept file , do not update it, but the order will not rejected due to this
    IF !SEEK(&lcHdr..ACCOUNT+&lcHdr..DEPT,'CUSTDEPT')
      REPLACE &lcHdr..DEPT WITH ' '
    ENDIF    
  ELSE
    DO lpAddErrLn WITH 'ACCOUNT "'+&lcHdr..ACCOUNT+'" not found in the Customer file.'
  ENDIF  
ENDIF

*- Completion date can not be less than start date
IF &lcHdr..COMPLETE < &lcHdr..START
  DO lpAddErrLn WITH 'Completion date {'+DTOC(&lcHdr..COMPLETE)+'} can not be less than start date {'+DTOC(&lcHdr..START)+'}.'
ENDIF

*DO lpAddErrLn WITH '---------'

*C102818,4  TMI [Start] Remove 'HEADER' word from the memo field
*IF !EMPTY(lcErrs)
*  lcErrs = 'HEADER ' + lcCR + lcErrs 
*ENDIF
*C102818,4  TMI [End  ] 

lcRetErrs = lcErrs
lcErrs = ''
*---------------------------------- Check lines ----------------------------------*
SELECT &lcLine
GO TOP

llDetail = .F.
SCAN REST FOR !DELETED()
  llDetail = .T.
  IF EMPTY(&lcLine..STYLE)
    DO lpAddErrLn WITH 'Empty Style field.',.T.
  ENDIF  
  
  IF !SEEK(&lcLine..STYLE,'STYLE')
    IF !SEEK(SUBSTR(&lcLine..STYLE,1,lnMajLen),'STYLE')
      DO lpAddErrLn WITH 'Style "'+&lcLine..STYLE+'" not found in STYLE file.',.T.
    ELSE
      lcClr = SUBSTR(&lcLine..STYLE,lnMajLen+2,6)
      DO lpAddErrLn WITH 'Color code "'+lcClr+'" not found in this style colors.',.T.
      IF !SEEK( 'NCOLOR     '+lcClr, 'CODES')
        DO lpAddErrLn WITH 'Color "'+lcClr+'" does not exists in Codes file.'
      ENDIF
    ENDIF
  ENDIF

  IF SEEK(SUBSTR(&lcLine..STYLE,1,lnMajLen),'STYLE')
    lnRecno = RECNO()
    IF !EMPTY(SUBSTR(&lcLine..STYLE,lnMajLen+2)) 
      IF SEEK(&lcLine..STYLE,'STYLE')
        *- The goal of this is to reposition to pointer to the correct style in style file
      ELSE
        GO lnRecno
      ENDIF
    ENDIF
    
    *- Check scale
    IF EMPTY(STYLE.SCALE)
      DO lpAddErrLn WITH 'Empty scale field for style '+&lcLine..STYLE+'.',.T.
    ENDIF
    
    IF !SEEK('S'+STYLE.SCALE,'SCALE')
      DO lpAddErrLn WITH 'Scale "'+&lcLine..SCALE+'" for style "'+&lcLine..STYLE +'" not found in SCALE file.',.T.
    ENDIF

    *- Check style status
    IF STYLE.STATUS = 'X'
      DO lpAddErrLn WITH 'Style "'+&lcLine..STYLE+'" is cancelled.',.T.
    ENDIF

    *- Check style season    
    IF &lcHdr..SEASON # '*' .AND. STYLE.SEASON # &lcHdr..SEASON
      DO lpAddErrLn WITH 'Season of Style "'+&lcLine..STYLE+'" does not match order season.',.T.
    ENDIF
  
    *- Check style division
    IF STYLE.CDIVISION # lcDefDiv 
      DO lpAddErrLn WITH 'Style "'+&lcLine..STYLE+'" division is not the default.',.T.
    ENDIF
    
    *- Check price
    *C102818,5  TMI [Start] Reject order if it has lines with zero prices
    IF &lcLine..PRICE = 0
      DO lpAddErrLn WITH 'Style '+ALLTRIM(&lcLine..Style)+' has zero price.',.T.
    ELSE
      *C102818,5  TMI [End  ]         
      IF &lcLine..PRICE # STYLE.PRICEA .AND. &lcLine..PRICE # STYLE.PRICEB 
        DO lpAddErrLn WITH 'Price ('+ALLTRIM(STR(&lcLine..PRICE,10,2))+') does not match '+;
                      'priceA ('+ALLTRIM(STR(STYLE.PRICEA,10,2))+') or priceB ('+ALLTRIM(STR(STYLE.PRICEB,10,2))+')',.T.
      ENDIF
    *C102818,5  TMI [Start] Close the above if statement
    ENDIF
    *C102818,5  TMI [End  ] 

    lnSzCnt = 8
    FOR lnI = 8 TO 1 STEP -1
      lcI = STR(lnI,1)
      IF !EMPTY(&lcLine..QTY&lcI)
        EXIT
      ELSE
        lnSzCnt = lnSzCnt - 1
      ENDIF
    ENDFOR
      
    IF SCALE.CNT < lnSzCnt
      DO lpAddErrLn WITH 'Count of Sizes populated ('+ALLTRIM(STR(lnSzCnt))+')'+;
                            'is greater than scale sizes count('+ALLTRIM(STR(SCALE.CNT))+')',.T.
    ENDIF

  ENDIF
  
    
  *- Check Qty not to be 0
  IF &lcLine..TOTQTY = 0 
    DO lpAddErrLn WITH 'Style "'+&lcLine..STYLE+'" has zero qty.',.T.
  ENDIF
  
ENDSCAN

IF llDetail
  *lcErrs = 'ORDER DETAIL REJECTION REASONS' + lcCR + 'LINE# REJ. REASON'+ lcCR + lcErrs
  *C102818,4  TMI [Start]   remove 'LINE#' from the memo field
  *IF !EMPTY(lcErrs)
  *  lcErrs = 'LINE# REJ. REASON'+ lcCR + lcErrs
  *ENDIF
  *C102818,4  TMI [END  ]   
ELSE
  DO lpAddErrLn WITH 'NO LINES FOUND IN THE DETAIL PART OF THE ORDER.'
ENDIF

lcRetErrs = lcRetErrs + lcErrs
*-- Processing to get error if there is [END  ]

SELECT (lnSlct) 
RETURN lcRetErrs
*-- end of lfErrFree.

*:**************************************************************************
*:* Name        : lpAddErrLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/14/2003
*:* Purpose     : Add a line to MERR memo field in error sessions file
*:***************************************************************************
FUNCTION lpAddErrLn
PARAMETERS lcNewLn, llLnNo
lcErrs = lcErrs + IIF(!EMPTY(llLnNo),PADR(&lcLine..nExcelLine,6),'')+lcNewLn + lcCR
*-- end of lpAddErrLn.
*:**********************************************************************************************
*:* Name        : lpSavOrdr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/11/2003
*:* Purpose     : Save temp file to Actual orders
*:**********************************************************************************************
PROCEDURE lpSavOrdr

EXTERNAL ARRAY laData , laKeyField
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
*C102818,4  TMI [Start] Define AltShip variables used in order save
STORE '' TO  lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5
*C102818,4  TMI [End  ] 

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
*--Loop throght the temp file lcOrdHdr to save data
SELECT (lcOrdHdr)
GO TOP
m.Ord1st = &lcOrdHdr..ORDER
SCAN  
  SCATTER FIELDS &lcScFields TO laData
  *C102818,4  TMI [Start] Update "alternate ShipTo" variables to save the order.
  IF laData[51]   &Check AltShip to field
    lcShipName = M.STNAME
    lcShipAdd1 = M.CADDRESS1 
    lcShipAdd3 = M.CADDRESS3
    lcShipAdd4 = 'USA'
  ENDIF
  *C102818,4  TMI [End  ] 
  SELECT &lcOrdLine
  SET FILTER TO CORDTYPE+ORDER = 'O'+&lcOrdHdr..ORDER 
  GO TOP
  SELECT &lcOrdHdr
  m.OrdLst = &lcOrdHdr..ORDER  
  llContinue = .T.  
  DO lfSavScr WITH .F. , .T. IN (gcAppHome + 'SO\SOUPDATE.FXP')
ENDSCAN
=gfModalGen('TRM38134B00000','DIALOG','Orders|'+m.Ord1st+'|'+m.OrdLst)

*-- end of lpSavOrdr

*:**************************************************************************
*:* Name        : lfPopDef
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/11/2003
*:* Purpose     : Get defaults for Div,Seas and WH
*:***************************************************************************
FUNCTION lfPopDef
PRIVATE lnSlct
lnSlct = SELECT()

IF SEEK('DCDIVISION ','CODES')
  lcDefDiv = CODES.CCODE_NO
ENDIF

SELECT WAREHOUS
GO TOP
lcDefWH = WAREHOUS.CWARECODE 

SELECT (lnSlct)
*-- end of lfPopDef.


*:**************************************************************************
*:* Name        : lfCloseFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/12/2003
*:* Purpose     : Close open temp files
*:***************************************************************************
FUNCTION lfCloseFls
IF USED(lcHdr)
  USE IN (lcHdr)
ENDIF
ERASE (gcWorkDir+lcHdr+'.DBF')
ERASE (gcWorkDir+lcHdr+'.CDX')
ERASE (gcWorkDir+lcHdr+'.FPT')
  
IF USED(lcLine)
  USE IN (lcLine)
ENDIF
ERASE (gcWorkDir+lcLine+'.DBF')
ERASE (gcWorkDir+lcLine+'.CDX')
ERASE (gcWorkDir+lcLine+'.FPT')
  
IF USED(lcOrdHdr)
  USE IN (lcOrdHdr)
ENDIF
ERASE (gcWorkDir+lcOrdHdr+'.DBF')
ERASE (gcWorkDir+lcOrdHdr+'.CDX')
ERASE (gcWorkDir+lcOrdHdr+'.FPT')
  
IF USED(lcOrdLine)
  USE IN (lcOrdLine)
ENDIF
ERASE (gcWorkDir+lcOrdLine+'.DBF')
ERASE (gcWorkDir+lcOrdLine+'.CDX')
ERASE (gcWorkDir+lcOrdLine+'.FPT')
  
IF USED(lcTmpOrd)
  USE IN (lcTmpOrd)
ENDIF
ERASE (gcWorkDir+lcTmpOrd+'.DBF')
ERASE (gcWorkDir+lcTmpOrd+'.CDX')
ERASE (gcWorkDir+lcTmpOrd+'.FPT')
  
IF USED(lcXLIMPSES)
  USE IN (lcXLIMPSES)
ENDIF
ERASE (gcWorkDir+lcXLIMPSES+'.DBF')
ERASE (gcWorkDir+lcXLIMPSES+'.CDX')
ERASE (gcWorkDir+lcXLIMPSES+'.FPT')
  
  
*-- end of lfCloseFls.


*:**************************************************************************
*:* Name        : lfViewRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/13/2003
*:* Purpose     : View and print report showing the status of each of the order 
*:*             : processed during that session
*:***************************************************************************
FUNCTION lfViewRep
PRIVATE lnSlct,lcWorkFile,lnResp
lnSlct = SELECT()
*- B42017 :<Preview> ; <Print> ; <Cancel>
DO WHILE .T.
  lnResp = gfModalGen('INM00000B42017',.F.,.F.,.F.,'Print the Imported Excel order status report?')
  IF lnResp = 3
    EXIT
  ENDIF
  IF INLIST(lnResp,1,2)
    SELECT &lcXLIMPSES
    lcWorkFile = lcXLIMPSES
    llOgFltCh = .F.
    lcRpForm = 'SOFALXLS'
    lcOGPlatForm = 'WINDOW'
    gcDevice = IIF(lnResp = 1,'SCREEN','PRINTER')
    DO (gcRepHome+gcWinAppl+'\SOFALXLS.PRG')
  ENDIF  
ENDDO
SELECT (lnSlct)
*-- end of lfViewRep.

*:**************************************************************************
*:* Name        : lfAppFrCsv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/13/2003
*:* Purpose     : Get the data from the csv file
*:***************************************************************************
FUNCTION lfAppFrCsv
PRIVATE lnSlct,lcRecStr,lnI,lcErrHnd,llError,lcDeleted
lnSlct = SELECT()

SELECT &lcTmpOrd 
ZAP  

lcErrHnd = ON('ERROR')
llError = .F.
ON ERROR llError = .T.
APPEND FROM (lcRpImpFld+laCsvFiles[lnCnt,1]) DELIMITED
ON ERROR &lcErrHnd
IF llError
  *-
ENDIF

*- if no lines in the csv file let the temp file be reach eof
IF RECCOUNT(lcTmpOrd) < lnCsvRecs
  GO BOTTOM
  IF !EOF()
    SKIP
  ENDIF
ELSE
  GOTO lnCsvRecs
ENDIF

*- The following loop delete wholly empty records from Dtl. part of the file
lcDeleted = SET('DELETED')
SET DELETED OFF
SCAN REST

  *-Delete lines starts with '.'  
  IF ALLTRIM(&lcTmpOrd..A) = '.'
    DELETE
    LOOP
  ENDIF
  
  lcRecStr = ''
  FOR lnI = 1 TO 20
    lcFldVal = ALLTRIM(EVALUATE(lcTmpOrd+'.'+CHR(lnI+64)))
    lcRecStr = lcRecStr + IIF(lcFldVal='.','',lcFldVal)
  ENDFOR
  IF EMPTY(lcRecStr)
    DELETE
  ENDIF
  
ENDSCAN
SET DELETED &lcDeleted
GO TOP

SELECT (lnSlct)
*-- end of lfAppFrCsv.



*:**************************************************************************
*:* Name        : lfvRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/13/2003
*:* Purpose     : Browse Sales Reps.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvRep()
*:***************************************************************************
FUNCTION lfvRep

SELECT SALESREP
IF ATC("?",laData[1]) > 0
  =gfBrows()  
ELSE
  IF !EMPTY(laData[1]) .AND. LASTKEY()= 13 
    =gfSeekRec()
  ENDIF
ENDIF

llBrowse  = .F.
