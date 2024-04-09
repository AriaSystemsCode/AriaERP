*:***************************************************************************
*: Program file  	  	: SRCOMM.PRG
*: Program desc. 		  : Print customer history report
*: Module        	    : Account Receivable (AR)
*: Developer     	    : Tarek Mohammed Ibrahim
*! Tracking Job Number: *C201317,1 TMI 03/23/2011 [T20101213.0028 ] 
*! C201318.122 ( a27 )
*! Title  : Custom Sales Rep Commission report for Carol Wren
Note : this program is a copy of ARHIST.PRG report , I made the needed changes for the customer
*:***************************************************************************
*: Calls :
*:    Programs    		: ....
*:    Screens     		: ....
*:    Global Functions  : gfCodDes

*:***************************************************************************
*: Called From: 
*:**********************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARHIST
*:***************************************************************************
*: Modifications:
*B609776,1 MMT 12/15/2011 Fix bug of wrong comm. rate in custom SR Comm. Report[T20111114.0019]
*:***************************************************************************
#INCLUDE R:\Aria4xp\reports\ar\arhist.H
*-- Variable used to print the account one time per history ID
IF USED(lcArhist)
  USE IN (lcArhist)
  SELECT * FROM &lcTempArhist WHERE .F. INTO CURSOR &lcArhist READWRITE 
  =lfMakeIndex(lcArhist)
ENDIF    
IF lcCustomer = lcTempCustomer
  lcCustomer = loOgScroll.gfTempName()
  SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
  =lfMakeIndex(lcCustomer)
ELSE
  IF USED(lcCustomer)
    USE IN (lcCustomer)
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
    =lfMakeIndex(lcCustomer)
  ENDIF 
ENDIF   
IF lcCodes = lcTempCodes
  lcCodes = loOgScroll.gfTempName()
  SELECT * FROM &lcTempCodes WHERE .F. INTO CURSOR &lcCodes READWRITE 
  =lfMakeIndex(lcCodes)
ELSE 
  IF USED(lcCodes)  
    USE IN (lcCodes)
    SELECT * FROM &lcTempCodes WHERE .F. INTO CURSOR &lcCodes READWRITE 
    =lfMakeIndex(lcCodes)
  ENDIF
ENDIF    
IF USED(lcFnlArHist)
  USE IN (lcFnlArHist)
ENDIF 

*-- Collecting data
=lfCollectData()

llAccPrtd = .F.
*-- Variable hold the date period which to be printed on the .FRX

lcDatPrd  = SPACE(0)
lnDatapos = ASCAN(loOGScroll.laOGFxFlt,'ARHIST.HISTDATE')
*-- Check if The user entered a date and get the date period to be printed.
IF lnDatapos > 0
    lnDatapos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatapos,1)
    *-- The user whether entered the two date or the end date only.
    IF !EMPTY(STRTRAN(loOGScroll.laOGFxFlt[lnDatapos,6],'|'))
      IF EMPTY(CTOD(LEFT(loOGScroll.laOGFxFlt[lnDatapos,6],10)))
            *C201317,1 TMI 03/23/2011 [Start] 
            *lcDatPrd =LANG_Arhist_PerTo + LEFT(loOGScroll.laOGFxFlt[lnDatapos,6],10)
            lcDatPrd ='Keyed-off To:' + LEFT(loOGScroll.laOGFxFlt[lnDatapos,6],10)
            *C201317,1 TMI 03/23/2011 [End  ] 
    ELSE
            *C201317,1 TMI 03/23/2011 [Start] 
            *lcDatPrd =LANG_Arhist_Per + LEFT(loOGScroll.laOGFxFlt[lnDatapos,6],10) + " - "+RIGHT(laOGFxFlt[lnDatapos,6],10)
            lcDatPrd ='Keyed-off From: '  + LEFT(loOGScroll.laOGFxFlt[lnDatapos,6],10) + "  To: "+RIGHT(laOGFxFlt[lnDatapos,6],10)
            *C201317,1 TMI 03/23/2011 [End  ] 

       ENDIF
   ENDIF
ENDIF

=lfCreatNdx()
IF llOGFltCh
  IF lcRpFrmt = 'A'
    *-- Get the maximum transaction # in each selected history number less than the chargeback (8)
    lcSelCond = " VAL(trantype)<8 " + IIF(lcRpList = 'Y',''," AND &lcFnlArHist..TRANTYPE<>'I' AND &lcFnlArHist..TRANTYPE<>'R'")
    SELECT HISTORY,ACCOUNT,MAX(VAL(trantype)) AS NMAXTR FROM &lcFnlArHist WHERE &lcSelCond  GROUP BY history,ACCOUNT INTO CURSOR (lcTmpMaxN)
    SELECT (lcTmpMaxN)
    INDEX ON ACCOUNT+HISTORY TAG (lcTmpMaxN)
  ENDIF
ENDIF
SELECT(lcFnlArHist)
*-- set relation to get the related field ALLOW_TYPE directly from file
*B609776,1 MMT 12/15/2011 Fix bug of wrong comm. rate in custom SR Comm. Report[Start]
*!*  SET RELATION TO 'M'+ACCOUNT INTO &lcCUSTOMER,;
*!*     'N'+IIF(TRANTYPE='7','CCREDITCOD',IIF(TRANTYPE='2','TRANCODE  ',''))+&lcFnlArHist..trancode+SPACE(30)+'ALLOW_TYPE' INTO &lcCODES
SET RELATION TO IIF(EMPTY(Store) OR !SEEK('S' + Account + Store,lcCUSTOMER),'M' + Account,'S' + Account + Store) INTO &lcCUSTOMER,;
   'N'+IIF(TRANTYPE='7','CCREDITCOD',IIF(TRANTYPE='2','TRANCODE  ',''))+&lcFnlArHist..trancode+SPACE(30)+'ALLOW_TYPE' INTO &lcCODES
*B609776,1 MMT 12/15/2011 Fix bug of wrong comm. rate in custom SR Comm. Report[END]
*C201317,1 TMI 03/23/2011 [Start] 
SET RELATION TO REPCODE INTO SALESREP ADDITIVE
*C201317,1 TMI 03/23/2011 [End  ]    
IF lcRpFrmt='A'
  SET RELATION TO ACCOUNT+HISTORY INTO (lcTmpMaxN) ADDITIVE
ENDIF
LOCATE 

*--To make paper printed as portrait not landscape 
loogScroll.cCROrientation = 'P'
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
ELSE
  =gfDispRe(lcRpFormNa)
ENDIF
*-- end of main code.

*!*************************************************************
*! Name      : lfwGrid
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : When OG Fuction.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfwGrid

*--Arhist file
IF TYPE('loArhist') <> 'O'
   loArhist  = CreateObject("RemoteTable","Arhist","ARHISTT",lcTempArhist,SET("DATASESSION"))
   SELECT * FROM &lcTempArhist WHERE .F. INTO CURSOR &lcArhist READWRITE 
   =lfMakeIndex(lcArhist)
ENDIF

*--Customer file
IF TYPE('loCustomer') <> 'O'
  loCustomer = CreateObject("RemoteTable","Customer","Customer",lcTempCustomer,SET("DATASESSION"))  
  SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
  =lfMakeIndex(lcCustomer)
ENDIF   

*--Notepad file
IF TYPE('loCodes') <> 'O'
  loCodes  = CreateObject("RemoteTable","Codes","CCODE_NO",lcTempCodes,SET("DATASESSION")) 
  SELECT * FROM &lcTempCodes WHERE .F. INTO CURSOR &lcCodes READWRITE 
  =lfMakeIndex(lcCodes)
ENDIF  


=lfvFormat()
*-- end of lfwGrid.

*!*************************************************************
*! Name      : lfvFormat
*! Developer : Ashraf Medhat
*! Date      : 08/04/1999
*! Purpose   : Report format validation
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvFormat

PRIVATE lnFormPos
LNFORMPOS = ASCAN(LAOGOBJTYPE,'LCRPFORM')
IF lnFormPos > 0
  *-- If Enable the Detail/Summary setting if form B only.
    LNFORMPOS = ASUBSCRIPT(LAOGOBJTYPE,LNFORMPOS,1)
    LAOGOBJCNT[LNFORMPOS] = (LCRPFRMT = "B")
    =LFOGSHOWGET('LCRPFORM')
ENDIF

IF lcRpFrmt="A"
  lcRpForm="D"
ENDIF 

*lcRpFormNa = "ARHIST" + lcRpFrmt
= lfRepPltFr(lcRpFormNa)
*-- set llOGFltCh=.T. to change from format B to Format A 
llOGFltCh  = .T. 
*-- end of lfvFormat.
*

FUNCTION lfSetAcct
PARAMETERS lcParm
SET ORDER TO CUSTOMER IN CUSTOMER
GO TOP IN CUSTOMER

*-- end of lfSetAcct.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Noha Mohammed Mostafa
*! Date      : 01/05/2004
*! Purpose   : Close files when closing the option grid
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfClearRep

*-- Close the cursor if any
IF USED(lcTmpMaxN)
  USE IN (lcTmpMaxN)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfCreatNdx
*! Developer : Noha Mohammed Mostafa
*! Date      : 01/05/2004
*! Purpose   : Create a custom index on the ARHIST file
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfCreatNdx
SELECT(lcFnlArHist)

  INDEX ON ACCOUNT+HISTORY+TRANTYPE+TRANCODE TAG cAHTTTag 

  SET ORDER TO TAG cAHTTTag 
*-- end of lfCreatNdx.

*!*************************************************************
*! Name      : lfAccPrtd
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 08/04/1999
*! Purpose   : to Print the customer one time per history ID
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfAccPrtd
PARAMETERS lcCallFrm
llAccPrtd = (lcCallFrm = 'D')

*!*************************************************************
*! Name      : lfGetCodDes
*! Developer :
*! Date      :
*! Purpose   :Function to return the Code Desc. 
*!*************************************************************
*! Passed Parameters : lcTranCode
*!*************************************************************
*! Return      : lcRetVal
*!*************************************************************

FUNCTION lfGetCodDes
PARAMETERS lcTranCode
PRIVATE lcRetVal

lcRetVal = ''
DO CASE
  CASE TranType $ "579"
    lcRetVal = gfCodDes(lcTranCode,'CCREDITCOD',.T.)
  CASE TranType $ "28"
    lcRetVal = gfCodDes(lcTranCode,'TRANCODE',.T.)
  CASE TranType = '4'
    lcRetVal = ALLTRIM(Store)+IIF(!EMPTY(Store),'-','')+Desc
  OTHERWISE
    lcRetVal = Desc
ENDCASE

RETURN lcRetVal
*-- end of lfGetCodDes.
*!*************************************************************
*! Name      : lfOpenFox
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenFox
LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial

LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")


LOCAL lnConNum
lnConNum = 0
lnConnectionHandlar = loOGScroll.oRDA.SqlRun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                      'SAVE',SET("DATASESSION"),.F.,@lnConNum)
IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
  =CURSORSETPROP("Buffering",5,lcCursor)
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE

   *--temp. Customer File
   CASE UPPER(lcTable) = lcCustomer
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
     laIndex[1,2] = lcCustomer
  
  *--Temp arhist file
  CASE UPPER(lcTable) = lcArHist
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'ACCOUNT+HISTORY'
     laIndex[1,2] = lcArHist

 *--Temp codes file
  CASE UPPER(lcTable) = lcCodes
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM'
     laIndex[1,2] = lcCodes

  CASE UPPER(lcTable) = lcFnlArHist
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'ACCOUNT+HISTORY'
     laIndex[1,2] = lcFnlArHist

ENDCASE 

*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/16/2005 
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMakeIndex
PARAMETERS lcTempName
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcCursor = lcTempName
lnBuffering = CURSORGETPROP("Buffering",lcCursor)
 =CURSORSETPROP("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
=lfCrtindex(lcCursor)
SELECT (lcCursor)
FOR lnI = 1 TO ALEN(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)

*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/16/2005 
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) 
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfCollectData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/27/2005 
*! Purpose   : function to Collect data 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCollectData


lnAccpos = ASCAN(loOGScroll.laOGFxFlt,'ARHIST.ACCOUNT')
*-- Check if The user entered a date and get the date period to be printed.
IF lnAccpos > 0
  lnAccpos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnAccpos,1)
  lcAccountSel =IIF(!EMPTY(laOgFxFlt[lnAccpos,6]),laOgFxFlt[lnAccpos,6],'')

  *C201317,1 TMI 03/23/2011 [Start] check if sales reps are selected
  lnRepPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'SALESREP.REPCODE '),1)
  lcSelRep = laOgFxFlt[lnRepPos,6]
  llSelRep = .F.
  IF !EMPTY(lcSelRep) AND USED(lcSelRep)
    SELECT &lcSelRep
    LOCATE 
    llSelRep = !EOF()
  ENDIF
  
  *- if selected then filter also the customers' related
  lcAccountAdded = ' '
  IF llSelRep
    IF EMPTY(lcAccountSel)
      lcAccountSel = loOGScroll.gfTempName()      
    ENDIF
    IF !USED(lcAccountSel)
      CREATE TABLE (oAriaApplication.WorkDir+ lcAccountSel ) ( KEYEXP C(5),ACCOUNT C(5) )
      INDEX ON KEYEXP TAG KEYEXP
    ENDIF
    SELECT &lcAccountSel
    LOCATE 
    IF EOF()
      SELECT CUSTOMER
      SCAN FOR SEEK(SALESREP,lcSelRep)
        IF !SEEK(ACCOUNT,lcAccountSel)
          INSERT INTO &lcAccountSel VALUES (CUSTOMER.ACCOUNT,CUSTOMER.ACCOUNT)
          lcAccountAdded = lcAccountAdded + '|' + CUSTOMER.ACCOUNT
        ENDIF
      ENDSCAN      
    ENDIF
  ENDIF
  *C201317,1 TMI 03/23/2011 [End  ] 
  
  IF !EMPTY(lcAccountSel)
    SELECT(lcAccountSel)
    LOCATE
    IF !EOF()
      SCAN 
        loArHist.Seek(&lcAccountSel..ACCOUNT)
        SELECT(lcTempArHist)
        SCAN REST WHILE &lcTempArHist..ACCOUNT+TRAN+CINSTALNO = &lcAccountSel..ACCOUNT
          SCATTER MEMO MEMVAR 
          INSERT INTO (lcArHist) FROM MEMVAR 
        ENDSCAN   
      ENDSCAN     
      lcSelcCond =  IIF(lcRpList = 'Y',''," &lcArHist..TRANTYPE<>'I' AND &lcArHist..TRANTYPE<>'R'")
      *-- Check if user wants to include void invoices or not 
      lcSlecFiles = lcArHist
      lcSelcFlds = lcArHist+".*"
      lnDatepos = ASCAN(loOGScroll.laOGFxFlt,'ARHIST.HISTDATE')
      *-- Check if The user entered a date and get the date period to be printed.
      IF lnDatepos > 0
        lnDatepos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatepos,1)
        SDATE = SUBSTR(laOgFxFlt[lnDatepos,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnDatepos,6],12,20)
        IF !EMPTY(EDATE) AND !EMPTY(SDATE)
          lcSelcCond = lcSelcCond + IIF(EMPTY(lcSelcCond),""," AND ")+ "BETWEEN(&lcArHist..HISTDATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF 
      SELECT(lcArHist)
      LOCATE 
      IF !EMPTY(lcSelcCond)
        *C201317,1 TMI 03/22/2011 [Start] Allow the cursor be read/write
        *SELECT &lcSelcFlds FROM &lcSlecFiles WHERE &lcSelcCond INTO CURSOR &lcFnlArHist 
        SELECT &lcSelcFlds FROM &lcSlecFiles WHERE &lcSelcCond INTO CURSOR &lcFnlArHist   READWRITE
        *C201317,1 TMI 03/22/2011 [End  ] 
      ELSE
        *C201317,1 TMI 03/22/2011 [Start] Allow the cursor be read/write
        *SELECT &lcSelcFlds FROM &lcSlecFiles INTO CURSOR &lcFnlArHist      
        SELECT &lcSelcFlds FROM &lcSlecFiles INTO CURSOR &lcFnlArHist        READWRITE
        *C201317,1 TMI 03/22/2011 [End  ] 
      ENDIF 
      =lfCodeCustomer()
    ELSE
      IF loARhist.llNative
        lcSelCond = IIF(lcRpList = 'Y',''," &lcTempArHist..TRANTYPE<>'I' AND &lcTempArHist..TRANTYPE<>'R'")
        lcSleFiles = lcTempArHist
        lcSelFlds = lcTempArHist+".*"
        lnDatepos = ASCAN(loOGScroll.laOGFxFlt,'ARHIST.HISTDATE')
        *-- Check if The user entered a date and get the date period to be printed.
        IF lnDatepos > 0
          lnDatepos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatepos,1)
          SDATE = SUBSTR(laOgFxFlt[lnDatepos,6],1,10)
          EDATE = SUBSTR(laOgFxFlt[lnDatepos,6],12,20)
          IF !EMPTY(EDATE) AND !EMPTY(SDATE)
            lcSelCond = lcSelCond + IIF(EMPTY(lcSelCond),""," AND ")+ "BETWEEN(&lcTempArHist..HISTDATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
          ENDIF 
        ENDIF 
        SELECT(lcTempArHist)
        LOCATE 
        IF !EMPTY(lcSelCond)
          *C201317,1 TMI 03/22/2011 [Start] Allow the cursor be read/write
          *SELECT &lcSelFlds FROM &lcSleFiles WHERE &lcSelCond INTO CURSOR &lcFnlArHist
          SELECT &lcSelFlds FROM &lcSleFiles WHERE &lcSelCond INTO CURSOR &lcFnlArHist  READWRITE
          *C201317,1 TMI 03/22/2011 [End  ] 
        ELSE
          *C201317,1 TMI 03/22/2011 [Start] Allow the cursor be read/write
          *SELECT &lcSelFlds FROM &lcSleFiles INTO CURSOR &lcFnlArHist      
          SELECT &lcSelFlds FROM &lcSleFiles INTO CURSOR &lcFnlArHist        READWRITE
          *C201317,1 TMI 03/22/2011 [End  ] 
        ENDIF 
        lcCodes= lcTempCodes
        lcCustomer = lcTempCustomer
        SELECT(lcCodes)
        SET ORDER to CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM 
      ELSE
        lcSelSqlCond = IIF(lcRpList = 'Y',''," ArHist.TRANTYPE<>'I' AND ArHist.TRANTYPE<>'R'")
        lcSleSqlFiles ="ArHist"
        lcSelSqlFlds = "ArHist.*"
        lnDatepos = ASCAN(loOGScroll.laOGFxFlt,'ARHIST.HISTDATE')
        *-- Check if The user entered a date and get the date period to be printed.
        IF lnDatepos > 0
          lnDatepos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatepos,1)
          SDATE = SUBSTR(laOgFxFlt[lnDatepos,6],1,10)
          EDATE = SUBSTR(laOgFxFlt[lnDatepos,6],12,20)
          IF !EMPTY(EDATE) AND !EMPTY(SDATE)
            lcSelSqlCond = lcSelSqlCond + IIF(EMPTY(lcSelSqlCond ),""," AND ")+ "ArHist.HISTDATE BETWEEN  "+CTOD(SDATE)+" AND "+CTOD(EDATE)
          ENDIF 
        ENDIF 
        =lfOpenSql(lcSelSqlFlds ,lcSleSqlFiles,lcFnlArHist,lcSelSqlCond )
        =lfCodeCustomer()
      ENDIF 
    ENDIF 
  ELSE  
    IF loARhist.llNative
      lcSelCond = IIF(lcRpList = 'Y','',"&lcTempArHist..TRANTYPE <> 'I' AND &lcTempArHist..TRANTYPE <> 'R'")
      lcSleFiles = lcTempArHist
      lcSelFlds = lcTempArHist+".*"
      lnDatepos = ASCAN(loOGScroll.laOGFxFlt,'ARHIST.HISTDATE')
      *-- Check if The user entered a date and get the date period to be printed.
      IF lnDatepos > 0
        lnDatepos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatepos,1)
        SDATE = SUBSTR(laOgFxFlt[lnDatepos,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnDatepos,6],12,20)
        IF !EMPTY(EDATE) AND !EMPTY(SDATE)
          lcSelCond = lcSelCond + IIF(EMPTY(lcSelCond),""," AND ")+ "BETWEEN(&lcTempArHist..HISTDATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        ENDIF 
      ENDIF 
      SELECT(lcTempArHist)
      LOCATE 
      IF !EMPTY(lcSelCond)
        *C201317,1 TMI 03/22/2011 [Start] Allow the cursor be read/write
        *SELECT &lcSelFlds FROM &lcSleFiles WHERE &lcSelCond INTO CURSOR &lcFnlArHist 
        SELECT &lcSelFlds FROM &lcSleFiles WHERE &lcSelCond INTO CURSOR &lcFnlArHist   READWRITE
        *C201317,1 TMI 03/22/2011 [End  ] 
      ELSE
        *C201317,1 TMI 03/22/2011 [Start] Allow the cursor be read/write
        *SELECT &lcSelFlds FROM &lcSleFiles INTO CURSOR &lcFnlArHist      
        SELECT &lcSelFlds FROM &lcSleFiles INTO CURSOR &lcFnlArHist        READWRITE
        *C201317,1 TMI 03/22/2011 [End  ] 
      ENDIF 
      lcCodes= lcTempCodes
      lcCustomer = lcTempCustomer
      SELECT(lcCodes)
      SET ORDER to CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM 
    ELSE
      lcSelSqlCond = IIF(lcRpList = 'Y',''," ArHist.TRANTYPE<>'I' AND ArHist.TRANTYPE<>'R'")
      lcSleSqlFiles ="ArHist"
      lcSelSqlFlds = "ArHist.*"
      lnDatepos = ASCAN(loOGScroll.laOGFxFlt,'ARHIST.HISTDATE')
      *-- Check if The user entered a date and get the date period to be printed.
      IF lnDatepos > 0
        lnDatepos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatepos,1)
        SDATE = SUBSTR(laOgFxFlt[lnDatepos,6],1,10)
        EDATE = SUBSTR(laOgFxFlt[lnDatepos,6],12,20)
        IF !EMPTY(EDATE) AND !EMPTY(SDATE)
          lcSelSqlCond = lcSelSqlCond + IIF(EMPTY(lcSelSqlCond ),""," AND ")+ "ArHist.HISTDATE BETWEEN  "+CTOD(SDATE)+" AND "+CTOD(EDATE)
        ENDIF 
      ENDIF 
      =lfOpenSql(lcSelSqlFlds ,lcSleSqlFiles,lcFnlArHist,lcSelSqlCond )
      =lfCodeCustomer()
    ENDIF 
  ENDIF  
ENDIF 

*C201490,1 HES Fix the bug of considering voided original amounts [BEGIN]
IF !USED('Rethdr')
  =gfOpenTable('Rethdr','Rethdr')
ENDIF
lcDelStat = SET("Deleted")
SET DELETED OFF 
*C201490,1 HES Fix the bug of considering voided original amounts [END  ]

*C201317,1 TMI 03/22/2011 [Start] *- Update the repcode and do a filter on
SELECT (lcFnlArHist)
IF TYPE('REPCODE')='U'
  ALTER TABLE &lcFnlArHist ADD COLUMN REPCODE C(3)
ENDIF
SET RELATION TO TRAN INTO INVHDR
SCAN 
  IF TRANTYPE = '1'
    *C201490,1 HES Fix the bug of considering voided original amounts [BEGIN]
    IF INVHDR.Status = 'V'
      DELETE
      LOOP 
    ENDIF
    *C201490,1 HES Fix the bug of considering voided original amounts [END  ]  
    REPLACE REPCODE WITH INVHDR.REP1
  ENDIF
  *C201490,1 HES Fix the bug of considering voided original amounts [BEGIN]
  IF TRANTYPE = '0'
    IF gfSeek(TRAN,'RETHDR') AND RETHDR.Status = 'V'
      DELETE
    ENDIF  
  ENDIF  
  *C201490,1 HES Fix the bug of considering voided original amounts [END  ] 
ENDSCAN

*C201490,1 HES Fix the bug of considering voided original amounts [BEGIN]
SET DELETED &lcDelStat.
SELECT (lcFnlArHist)
*C201490,1 HES Fix the bug of considering voided original amounts [END  ]

INDEX ON HISTORY+REPCODE TAG REPCODE DESC
LOCATE
DO WHILE !EOF()
  lcHist = HISTORY
  lcRep  = REPCODE
  REPLACE REPCODE WITH lcRep REST WHILE HISTORY+REPCODE = lcHist
ENDDO

SELECT (lcFnlArHist)
LOCATE 
DELETE FOR EMPTY(REPCODE) OR IIF(llSelRep,!SEEK(REPCODE,lcSelRep),.F.)
INDEX ON REPCODE + ACCOUNT + HISTORY TAG REP_HIST

*- Deselect the 
IF !EMPTY(lcAccountAdded)
  SELECT (lcAccountSel)
  DELETE FOR ACCOUNT $ lcAccountAdded
ENDIF
*C201317,1 TMI 03/22/2011 [End  ] 

*!*************************************************************
*! Name      : lfCodeCustomer
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/27/2005 
*! Purpose   : function to Collect data 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCodeCustomer

SELECT(lcFnlArHist)
SCAN
  loCustomer.Seek('M'+&lcFnlArHist..ACCOUNT)
  SELECT(lcTempCustomer)
  IF !SEEK(&lcTempCustomer..TYPE+&lcTempCustomer..ACCOUNT+&lcTempCustomer..STORE,lcCustomer)
    SELECT(lcTempCustomer)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcCustomer) FROM MEMVAR 
  ENDIF   
  *B609776,1 MMT 12/15/2011 Fix bug of wrong comm. rate in custom SR Comm. Report[Start]
  loCustomer.Seek('S'+&lcFnlArHist..ACCOUNT)
  SELECT(lcTempCustomer)
  SCAN REST WHILE TYPE+ACCOUNT+STORE = 'S'+&lcFnlArHist..ACCOUNT
    IF !SEEK(&lcTempCustomer..TYPE+&lcTempCustomer..ACCOUNT+&lcTempCustomer..STORE,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcCustomer) FROM MEMVAR 
    ENDIF   
  ENDSCAN 
  *B609776,1 MMT 12/15/2011 Fix bug of wrong comm. rate in custom SR Comm. Report[END]
ENDSCAN  

SELECT(lcFnlArHist)
SCAN 
  loCodes.Seek('N'+IIF(TRANTYPE='7','CCREDITCOD',IIF(TRANTYPE='2','TRANCODE  ',''))+&lcFnlArHist..trancode+SPACE(30)+'ALLOW_TYPE')
  SELECT(lcTempCodes)
  IF !SEEK(&lcTempCodes..CDEFCODE+&lcTempCodes..CFLD_NAME+&lcTempCodes..CCODE_NO+&lcTempCodes..CDISCREP+&lcTempCodes..CRLTD_NAM,lcCodes)
    SELECT(lcTempCodes)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcCodes) FROM MEMVAR 
  ENDIF 
ENDSCAN   
SELECT(lcCodes)
SET ORDER TO &lcCodes
