*:***************************************************************************
*: Program file  : SOREVRG.PRG 
*: Program desc. : Daily Order Register (for Revue)
*: For Report    : (SOREVRG.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar[MMT]
*: Date          : 10/21/2008
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: ..C201021,1
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[T20090727.0031]
*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without condition[T20090727.0031]
*:***************************************************************************
*--If the user changes companies, recollect data.
PARAMETERS lcRequestID, lcXMLFileName

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.Description = "Opening Data Files..."
loAgent.UpdateObjectProgress(lcRequestID, loProgress)

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)

oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

oAriaEnvironment.Report.gcAct_Appl = "SO"
oariaenvironment.activeModuleID = 'SO' 


PUBLIC gcAct_Appl 
gcAct_Appl = "SO"


IF LEFT(gcDevice, 7) = "PRINTER"
  oAriaEnvironment.gcDevice = "PRINTER"
ELSE
  oAriaEnvironment.gcDevice = "FILE"
ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ENDIF
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

lcTempFile = oAriaEnvironment.Cursors.GetCursorTempName()
IF !FILE(oAriaEnvironment.WorkDir +'DEFCOMP.MEM')
  *--Initialize needed variables
  STORE '' TO laRpCmpCod,lcRpFrom,lcRpTo
  *--Get all companies information.
  DO lpGetComp
ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  lcTempFile = gfTempName()
  IF !FILE(oAriaApplication.WorkDir +'DEFCOMP.MEM')
    *--Initialize needed variables
    STORE '' TO laRpCmpCod,lcRpFrom,lcRpTo
    *--Get all companies information.
    DO lpGetComp
  ENDIF  
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*loogscroll.llOGFltCh = loogscroll.llOGFltCh OR (lcRpCmpExp <> lcOldComp) OR (lcOldComp <> lcRpCmpExp)
lcOldComp = lcRpCmpExp
*--Prepaire filter.
lcRpExp = lcRpExp + " AND cordtype+order = 'O' AND INLIST(STATUS,'O','H')"
*--Save default companies.

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

lcStyTitle = oAriaEnvironment.itemmask.DO('HI')
*lcStyTitle = gfItemMask('HI')

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  lcStyTitle = gfItemMask('HI')
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

IF !EMPTY(lcRpCmpExp)
  DIMENSION laRpCmpCod[1]
  IF AT(',',lcRpCmpExp) = 0
    laRpCmpCod[1] = lcRpCmpExp
  ELSE
    lcValuesToConvert = lcRpCmpExp
    lnStart=1 
    lnEnd=AT(',',lcValuesToConvert )
    DO WHILE lnEnd <> 0
      IF EMPTY(laRpCmpCod[1])
        laRpCmpCod[1] =  SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
      ELSE
        DIMENSION laRpCmpCod[ALEN(laRpCmpCod)+1]   
        laRpCmpCod[ALEN(laRpCmpCod)] =  SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
      ENDIF 
      
      lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
      lnEnd=AT(',',lcValuesToConvert )
    ENDDO 
    IF lnEnd = 0
      DIMENSION laRpCmpCod[ALEN(laRpCmpCod)+1]   
      laRpCmpCod[ALEN(laRpCmpCod)] =  lcValuesToConvert 
    ENDIF 
  ENDIF 
ENDIF 

*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without condition[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without condition[End]

*--Create the temp file.
DO lpCrtTemp
lfColData()

*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without condition[Start]
ELSE 
  IF llOGFltCh
    DO lpCrtTemp
    lfColData()    
  ENDIF 
ENDIF 
*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without condition[End]

SELECT (lcTempLine)
LOCATE 
DELETE FOR Total = 0
LOCATE 

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

oAriaEnvironment.report.OGLastForm = 'SOREVRG'

loProgress.Percent = 0.9
loProgress.Description = "Printing Report..."
loAgent.UpdateObjectProgress(lcRequestID, loProgress)

PRIVATE loProxy
loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

IF loProxy.GetRequest(lcRequestID).Status = 3
  oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
  loProgress.Percent = 1.0
  loProgress.Description = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress)
ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  DO gfDispRe 
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*!***************************************************************************
*! Name      : lfCmpExpr
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : - Evaluate Company expression.
*!***************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!***************************************************************************
*! Called from : lfvCategory(),lfvCompany(),lfwRepWhen()
*!***************************************************************************
*! Passed Parameters  : ....
*!***************************************************************************
*! Returns            : ....
*!***************************************************************************
*! Example   : = lfCmpExpr()
*!***************************************************************************
*E301294,1  AKA 08/23/99  
*!***************************************************************************

FUNCTION lfCmpExpr
PRIVATE laTarget

IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF
= ASORT(laTarget)
lcOldComp = lcRpCmpExp
lcRpCmpExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR

IF LEN(lcRpCmpExp) > 2
  llNComp = .T.
ELSE
  llNComp = .F.
ENDIF
*CLEARREAD()
*-- end of lfCmpExpr.

*!**************************************************************************
*! Name      : lpCrtTemp
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : Create temp file.
*!**************************************************************************
*! Example   : DO lpCrtTemp
*!**************************************************************************
*..
PROCEDURE lpCrtTemp
DIMENSION laStrArr[16,4]

laStrArr[1,1] = 'CompId '
laStrArr[1,2] = 'C'
laStrArr[1,3] = 2
laStrArr[1,4] = 0

laStrArr[2,1] = 'Rep1'
laStrArr[2,2] = 'C'
laStrArr[2,3] = 3
laStrArr[2,4] = 0

laStrArr[3,1] = 'CompNm'
laStrArr[3,2] = 'C'
laStrArr[3,3] = 30
laStrArr[3,4] = 0

laStrArr[4,1] = 'Account'
laStrArr[4,2] = 'C'
laStrArr[4,3] = 5
laStrArr[4,4] = 0

laStrArr[5,1] = 'Name'
laStrArr[5,2] = 'C'
laStrArr[5,3] = 30
laStrArr[5,4] = 0

laStrArr[6,1] = 'Order'
laStrArr[6,2] = 'C'
laStrArr[6,3] = 6
laStrArr[6,4] = 0

laStrArr[7,1] = 'PO'
laStrArr[7,2] = 'C'
laStrArr[7,3] = 15
laStrArr[7,4] = 0

laStrArr[8,1] = 'Season'
laStrArr[8,2] = 'C'
laStrArr[8,3] = 6
laStrArr[8,4] = 0

laStrArr[9,1] = 'Style'
laStrArr[9,2] = 'C'
laStrArr[9,3] = 19
laStrArr[9,4] = 0

laStrArr[10,1] = 'Qty'
laStrArr[10,2] = 'N'
laStrArr[10,3] = 4
laStrArr[10,4] = 0

laStrArr[11,1] = 'Price'
laStrArr[11,2] = 'N'
laStrArr[11,3] = 6
laStrArr[11,4] = 2

laStrArr[12,1] = 'Total'
laStrArr[12,2] = 'N'
laStrArr[12,3] = 8
laStrArr[12,4] = 2

laStrArr[13,1] = 'Terms'
laStrArr[13,2] = 'C'
laStrArr[13,3] = 30
laStrArr[13,4] = 0

laStrArr[14,1] = 'MarkUp'
laStrArr[14,2] = 'N'
laStrArr[14,3] = 5
laStrArr[14,4] = 2

laStrArr[15,1] = 'SlsRepN'
laStrArr[15,2] = 'C'
laStrArr[15,3] = 24
laStrArr[15,4] = 0

laStrArr[16,1] = 'LineNo'
laStrArr[16,2] = 'N'
laStrArr[16,3] = 6
laStrArr[16,4] = 0

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

=oAriaEnvironment.Cursors.createcursor(lcTempLine,@laStrArr,"CompId+Rep1+Account+Order+Style+PO+STR(lineno,6)",lcTempLine,.F.)

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  =gfCrtTmp(lcTempLine,@laStrArr,"CompId+Rep1+Account+Order+Style+PO+STR(lineno,6)",lcTempLine,.F.)
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*!**************************************************************************
*! Name      : lfColData
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : Collect data.
*!**************************************************************************
*! Example   : =lfColData()
*!**************************************************************************
*..
FUNCTION lfColData

ldStart = {}
ldEnd = {}
llDataSelect = .F.
lnDatePos = ASCAN(laOGFxFlt,'ORDHDR.ENTERED')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF !EMPTY(laOGFxFlt[lnDatePos,6])
	ldStart = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))
	ldEnd = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21))
	llDataSelect = .T.
	lcRpFrom   = DTOC(ldStart)
    lcRpTo     = DTOC(ldEnd)
  ENDIF 
ENDIF  
IF USED('CODES')
  USE IN CODES
ENDIF

IF !USED('SYCCOMP_L')

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

    =oAriaEnvironment.remotetableaccess.OpenFile(oAriaEnvironment.SystemFilesPath+'SYCCOMP', 'Ccomp_id','SH','SYCCOMP_L')
    
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  =gfOpenTABLE(oAriaApplication.DATADIR+'SYCCOMP',oAriaApplication.DATADIR+'Ccomp_id','SH','SYCCOMP_L') 
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
    
ENDIF
SELECT SYCCOMP_L
SET ORDER TO TAG Ccomp_id

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

lcOldDataDir = oAriaEnvironment.DataDir
lcOldComp = oAriaEnvironment.ActiveCompanyId

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  lcOldDataDir = oAriaApplication.DataDir
  lcOldComp    = oAriaApplication.ActiveCompanyId
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

FOR lnAllComp = 1 TO ALEN(laRpCmpCod,1)
 
  DIMENSION laFileName [1,2]      
  laFileName = ''
  lcComp_ID  = PADR(laRpCmpCod[lnAllComp],2)
  IF lcComp_ID $ lcRpCmpExp
    IF SEEK (lcComp_ID ,'SYCCOMP_L')
      lcFilePath = ALLTRIM(SYCCOMP_L.Ccom_ddir)
      lcCompName = ALLTRIM(SYCCOMP_L.Ccom_Name)
    ENDIF
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
    
    oAriaEnvironment.GetCompanyInformation(lcComp_ID)
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      oAriaApplication.GetCompanyInformation(lcComp_ID)
      *1234
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]        
    
    *--Open Company Files
    DO lpOpenFile
    *--Get records
    * " AND cordtype+order = 'O' AND INLIST(STATUS,'O','H')"
    SELECT ORDHDR
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]      
    
    =oAriaEnvironment.remotetableaccess.SeekRecord('O')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSeek('O')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]      
    
    LOCATE REST WHILE cordtype+order = 'O' For INLIST(STATUS,'O','H') AND;
    				  IIF(llDataSelect,BETWEEN(ORDHDR.ENTERED,ldStart,ldEnd),.T.)
    IF EOF()
      LOOP
    ENDIF

    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   
    
    =oAriaEnvironment.remotetableaccess.SeekRecord('O')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSeek('O')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
    
    COUNT REST WHILE cordtype+order = 'O' For INLIST(STATUS,'O','H') AND;
    				  IIF(llDataSelect,BETWEEN(ORDHDR.ENTERED,ldStart,ldEnd),.T.) TO lnCntOrder 				  
    
    lnOrdCnt = 1
    SELECT ORDHDR
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
    
    =oAriaEnvironment.remotetableaccess.SeekRecord('O')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSeek('O')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]           
    

    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   

    SCAN REST WHILE cordtype+order = 'O' For INLIST(STATUS,'O','H') AND;
    				  IIF(llDataSelect ,BETWEEN(ORDHDR.ENTERED,ldStart,ldEnd),.T.) AND oAriaEnvironment.remotetableaccess.SeekRecord(CORDTYPE+ORDER,'ORDLINE')
      
      lnOrdCnt = lnOrdCnt +  1
      lnPerCent = lnOrdCnt/lnCntOrder 
      IF MOD(lnOrdCnt,CEILING(lnCntOrder / 10)) = 0
        loProgress.Percent = lnPerCent * 0.9
	    loProgress.Description = "Collecting Data For Company:"+lcComp_ID  
  	    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      ENDIF

				  
      SELECT ORDLINE
      SCAN REST WHILE CORDTYPE+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
        DO lpInsert
      ENDSCAN
    ENDSCAN
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      SCAN REST WHILE cordtype+order = 'O' For INLIST(STATUS,'O','H') AND;
      				  IIF(llDataSelect ,BETWEEN(ORDHDR.ENTERED,ldStart,ldEnd),.T.) AND gfSeek(CORDTYPE+ORDER,'ORDLINE')
			
	    *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without condition [Start]
	    WAIT WINDOW NOWAIT "Collecting Data For Company:"+lcComp_ID 
	    *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without condition [End]
	    
        SELECT ORDLINE
        SCAN REST WHILE CORDTYPE+order+STR(lineno,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
          DO lpInsert
        ENDSCAN
      ENDSCAN      
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
    
  ENDIF
  IF USED('CODES')
  
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
  
    oAriaEnvironment.remotetableaccess.CloseTable('CODES')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfCloseTable('CODES')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
    
  ENDIF
ENDFOR

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   

oAriaEnvironment.ActiveCompanyId = lcOldComp  
oAriaEnvironment.GetCompanyInformation(lcOldComp)

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  oAriaApplication.ActiveCompanyId = lcOldComp  
  oAriaApplication.GetCompanyInformation(lcOldComp)
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   

RETURN RECCOUNT(lcTempLine) >0

*!**************************************************************************
*! Name      : lpOpenFile
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : Open compnay files.
*!**************************************************************************
*! Example   : DO lpCrtTemp
*!**************************************************************************
*..
PROCEDURE lpOpenFile

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   

IF USED('ORDHDR')
  =oAriaEnvironment.remotetableaccess.CloseTable('ORDHDR')
ENDIF  
IF USED('ORDLINE')
  =oAriaEnvironment.remotetableaccess.CloseTable('ORDLINE')
ENDIF  
IF USED('Style')
  =oAriaEnvironment.remotetableaccess.CloseTable('Style')
ENDIF
IF USED('SalesRep')
  =oAriaEnvironment.remotetableaccess.CloseTable('SalesRep')
ENDIF
IF USED('CUSTOMER')
  =oAriaEnvironment.remotetableaccess.CloseTable('CUSTOMER')
ENDIF

=oAriaEnvironment.remotetableaccess.OpenTable('ORDHDR','ORDHDR','SH')  
=oAriaEnvironment.remotetableaccess.OpenTable('Style','Style','SH')  
=oAriaEnvironment.remotetableaccess.OpenTable('SalesRep','SalesRep','SH')  
=oAriaEnvironment.remotetableaccess.OpenTable('ORDLINE','ORDLINE','SH')  
=oAriaEnvironment.remotetableaccess.OpenTable('CUSTOMER','CUSTOMER','SH')  

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  IF USED('ORDHDR')
    =gfCloseTable('ORDHDR')
  ENDIF  
  IF USED('ORDLINE')
    =gfCloseTable('ORDLINE')
  ENDIF  
  IF USED('Style')
    =gfCloseTable('Style')
  ENDIF
  IF USED('SalesRep')
    =gfCloseTable('SalesRep')
  ENDIF
  IF USED('CUSTOMER')
    =gfCloseTable('CUSTOMER')
  ENDIF

  =gfOpenTABLE(oAriaApplication.DATADIR+'ORDHDR',oAriaApplication.DATADIR+'ORDHDR','SH')  
  =gfOpenTABLE(oAriaApplication.DATADIR+'Style',oAriaApplication.DATADIR+'Style','SH')  
  =gfOpenTABLE(oAriaApplication.DATADIR+'SalesRep',oAriaApplication.DATADIR+'SalesRep','SH')    
  =gfOpenTABLE(oAriaApplication.DATADIR+'ORDLINE',oAriaApplication.DATADIR+'ORDLINE','SH')     
  =gfOpenTABLE(oAriaApplication.DATADIR+'CUSTOMER',oAriaApplication.DATADIR+'CUSTOMER','SH')  
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   

*!**************************************************************************
*! Name      : lpInsert
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : Update Records.
*!**************************************************************************
*! Example   : DO lpInsert
*!**************************************************************************
*..
PROCEDURE lpInsert

IF !SEEK(lcComp_ID+Account+Order+Style+PO+STR(lineno,6),lcTempLine)

  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  

  lcAccName  = IIF(oAriaEnvironment.remotetableaccess.SeekRecord('M'+ORDLINE.ACCOUNT,'CUSTOMER'),CUSTOMER.STNAME,'')
  lcTermData = oAriaEnvironment.codes.getcodedescription(OrdHdr.CTERMCODE , 'CTERMCODE')
  lcSlsName  = IIF(oAriaEnvironment.remotetableaccess.SeekRecord(ordhdr.Rep1,'SalesRep'),salesrep.NAME,'')
  =oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.STYLE,'STYLE')
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    lcAccName  = IIF(gfSeek('M'+ORDLINE.ACCOUNT,'CUSTOMER'),CUSTOMER.STNAME,'')
    lcTermData = gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE')
    lcSlsName  = IIF(gfSeek(ordhdr.Rep1,'SalesRep'),salesrep.NAME,'')
    =gfSeek(ORDLINE.STYLE,'STYLE')    
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
  
  lnMarkUp   = IIF(Price = 0,0,((ROUND(Price,2)-ROUND(style.Totcost,2))/ROUND(Price,2))*100)
  INSERT INTO (lcTempLine) (CompId,Rep1,CompNm,Account,Name,Order,PO,Season,Style,;
                            Qty,Price,Total,Terms,MarkUp,SlsRepN,LineNo);
                    VALUES (lcComp_ID,ordhdr.Rep1,lcCompName,ORDLINE.ACCOUNT,lcAccName,Ordline.Order,;
                            Ordline.CustPo,OrdLine.Season,OrdLine.Style,OrdLine.TotQty,OrdLine.Price,;
                            OrdLine.TotQty*OrdLine.Price,lcTermData,lnMarkUp,lcSlsName,OrdLine.LineNo)
ENDIF

*!**************************************************************************
*! Name      : lpGetComp
*: Developer : Mariam Mazhar[MMT]
*: Date      : 10/21/2008
*! Purpose   : Get all companies information in case of running the program
*!           : from outsode Aria.
*!**************************************************************************
*! Example   : DO lpGetComp
*!**************************************************************************
*..
PROCEDURE lpGetComp

IF USED('SYCCOMP')

  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

   =oAriaEnvironment.remotetableaccess.OpenFile(oAriaEnvironment.SystemFilesPath+'SYCCOMP', 'Ccomp_id','SH')
   
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    =gfOpenTABLE(oAriaApplication.DATADIR+'SYCCOMP',oAriaApplication.DATADIR+'Ccomp_id','SH') 
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   
   
ENDIF

DECLARE laRpCmpCod[1,3]
STORE '' TO lcRpCmpExp
*-- Collect all companies
SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
  FROM SYCCOMP                            ;
  INTO ARRAY laRpCmpCod                   ;
  ORDER BY 1
DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
FOR lnI = 1 TO ALEN(laRpCmpCod,1)
  STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
ENDFOR
=lfCmpExpr()
USE IN SYCCOMP
lcRpFrom   = DTOC(DATE())
lcRpTo     = DTOC(DATE())
lcRpExp = "BETWEEN(DTOS(ORDHDR.ENTERED),ALLTRIM(DTOS({  "+DTOC(DATE())+"  })),ALLTRIM(DTOS({  "+DTOC(DATE())+"  }))) AND CORDTYPE = 'O' AND INLIST(STATUS,'O','H')"
SAVE ALL LIKE l?Rp* TO oAriaApplication.WorkDir +"DEFCOMP.MEM"

