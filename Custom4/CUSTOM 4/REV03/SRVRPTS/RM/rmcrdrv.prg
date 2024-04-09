*!*****************************************************************************************
*! Name      : RMCRDRV.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 10/21/2008
*! Purpose   : CUSTOMIZED DAILY RETURN REGISTER FOR REVUE.
*! Entry no. : C201055
*!*****************************************************************************************
*! Modification
*! C201196,1 HES call request builder fxp to collect data [T20090727.0031]
*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[T20090727.0031]
*!*****************************************************************************************
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

oAriaEnvironment.Report.gcAct_Appl = "RM"
oariaenvironment.activeModuleID = 'RM' 


PUBLIC gcAct_Appl 
gcAct_Appl = "RM"


IF LEFT(gcDevice, 7) = "PRINTER"
  oAriaEnvironment.gcDevice = "PRINTER"
ELSE
  oAriaEnvironment.gcDevice = "FILE"
ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ENDIF
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

lcsttime = time()

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

lcTempFile = oAriaEnvironment.Cursors.GetCursorTempName()

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  lcTempFile = gfTempName()
ENDIF
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]

IF !EMPTY(lcRpCmpExp)
  DIMENSION laRpCompT[1]
  IF AT(',',lcRpCmpExp) = 0
    laRpCompT[1] = lcRpCmpExp
  ELSE
    lcValuesToConvert = lcRpCmpExp
    lnStart=1 
    lnEnd=AT(',',lcValuesToConvert )
    DO WHILE lnEnd <> 0
      IF EMPTY(laRpCompT[1])
        laRpCompT[1] =  SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
      ELSE
        DIMENSION laRpCompT[ALEN(laRpCompT)+1]   
        laRpCompT[ALEN(laRpCompT)] =  SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
      ENDIF 
      
      lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
      lnEnd=AT(',',lcValuesToConvert )
    ENDDO 
    IF lnEnd = 0
      DIMENSION laRpCompT[ALEN(laRpCompT)+1]   
      laRpCompT[ALEN(laRpCompT)] =  lcValuesToConvert 
    ENDIF 
  ENDIF 
ENDIF 

*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
IF TYPE('lcXMLFileName') = 'C'
  =lfvCrTemF()
  =lfvColData() 
ELSE 
  IF llOGFltCh
    =lfvCrTemF()
    =lfvColData()     
  ENDIF
ENDIF 
*!*	=lfvCrTemF()
*!*	=lfvColData() 
*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[End]

SELECT(lcTempFile)

IF RECCOUNT(lcTempFile) = 0
  USE IN (lcTempFile)
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  RETURN 
  
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
  ELSE 
    *-- Message <There are no records to display>
    *-- Buttons <               OK              >
    gfModalGen('TRM00052B00000','DIALOG' )
    RETURN    
  ENDIF 
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[End]    
  
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

oAriaEnvironment.report.OGLastForm = 'RMCRDRV'

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
  DO gfdispre WITH EVAL('lcRpForm')
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
*:***************************************************************************
*: Name              : lfvCrTemF
*! Developer         : Mariam Mazhar Tawfik [MMT]
*! Date              : 07/02/2008
*: Purpose           : Function to create temporary file to collect the data 
*:                     that will be printed.
*:***************************************************************************
*: Called from       : RMCRDRV.PRG
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvCrTemF()
*:***************************************************************************

FUNCTION lfvCrTemF

*-- Create temporary file to collect the data
DIMENSION laArrStruct[12,4]

laArrStruct[1,1] = 'cCompID' 
laArrStruct[1,2] = "C"
laArrStruct[1,3] = 2
laArrStruct[1,4] = 0

laArrStruct[2,1] = 'Ccom_name' 
laArrStruct[2,2] = "C"
laArrStruct[2,3] = 30
laArrStruct[2,4] = 0

laArrStruct[3,1] = 'Season' 
laArrStruct[3,2] = "C"
laArrStruct[3,3] = 6
laArrStruct[3,4] = 0

laArrStruct[4,1] = 'Crmemo' 
laArrStruct[4,2] = "C"
laArrStruct[4,3] = 6
laArrStruct[4,4] = 0

laArrStruct[5,1] = 'Style' 
laArrStruct[5,2] = "C"
laArrStruct[5,3] = 19
laArrStruct[5,4] = 0

laArrStruct[6,1] = 'Totqty' 
laArrStruct[6,2] = "N"
laArrStruct[6,3] = 4
laArrStruct[6,4] = 0

laArrStruct[7,1] = 'Price' 
laArrStruct[7,2] = "N"
laArrStruct[7,3] = 6
laArrStruct[7,4] = 2

laArrStruct[8,1] = 'CtermDesc' 
laArrStruct[8,2] = "C"
laArrStruct[8,3] = 30
laArrStruct[8,4] = 0

laArrStruct[9,1] = 'ReasonDesc' 
laArrStruct[9,2] = "C"
laArrStruct[9,3] = 30
laArrStruct[9,4] = 0

laArrStruct[10,1] = 'cRepCode' 
laArrStruct[10,2] = "C"
laArrStruct[10,3] = 3
laArrStruct[10,4] = 0

laArrStruct[11,1] = 'cRepName' 
laArrStruct[11,2] = "C"
laArrStruct[11,3] = 24
laArrStruct[11,4] = 0

laArrStruct[12,1] = 'Exten' 
laArrStruct[12,2] = "N"
laArrStruct[12,3] = 9
laArrStruct[12,4] = 2

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

oAriaEnvironment.Cursors.createcursor(lcTempFile,@laArrStruct,"cCompId + cRepCode + Crmemo",lcTempFile,.F.)

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data [Start]
ELSE 
  =gfCrtTmp(lcTempFile,@laArrStruct,"cCompId + cRepCode + Crmemo",lcTempFile,.F.)
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data [End]

*:***************************************************************************
*: Name              : lfvColData
*! Developer         : Mariam Mazhar Tawfik [MMT]
*! Date              : 07/02/2008
*: Purpose           : Function to collect the data that will be printed.           
*:***************************************************************************
*: Called from       : RMCRDRV.PRG
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvColData()
*:***************************************************************************

FUNCTION lfvColData
PRIVATE lnCount, lcCompID, lcComDir

ldRpFDate = DATE()
ldRpSDate = DATE()


IF llRnFrmOg
  llDateSelect = .F.
  lnDatePos = ASCAN(laOGFxFlt,'INVTADJ.DATE')
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  llDateSelect = !EMPTY(laOGFxFlt[lnDatePos,6])
  IF llDateSelect 
    ldRpFDate = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))
    ldRpSDate = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21))
  ENDIF 
ENDIF




DIMENSION laTemp[1]
IF EMPTY(laRpCompT)
  IF EMPTY(laRpCompS)
    DIMENSION laRpCompS[1]
    lfvArray(.T.)
  ENDIF 
  =ACOPY(laRpCompS, laTemp)
ELSE
  =ACOPY(laRpCompT, laTemp)
ENDIF

IF USED('CODES')
  USE IN CODES
ENDIF
  


  
  
FOR lnArrCount=1 TO ALEN(laTemp)





  lcCompID = laTemp[lnArrCount]
  lcCompID = SUBSTR(lcCompID,1,2)
  M.cCompID= ALLTRIM(lcCompID)

  
    
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
  
  lcConnStr  = ''
  IF SEEK (lcCompID,'SYCCOMP_L')
    lcComDir = ALLTRIM(SYCCOMP_L.Ccom_ddir)
    lcConnStr = "Driver={SQL Server};server="+ALLTRIM(SYCCOMP_L.CCONSERVER)+";DATABASE="+ALLTRIM(SYCCOMP_L.CCONDBNAME)+;
                ";uid="+ALLTRIM(SYCCOMP_L.CCONUSERID)+";pwd="+ALLTRIM(SYCCOMP_L.CCONPASWRD)
  ELSE
    LOOP 
  ENDIF
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  lcCurrConn = oAriaApplication.ReadConnectionString(lcCompID)
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    lcCurrConn = oAriaApplication.mreadconstr(lcCompID)
  ENDIF
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  

  
  
  lnConn = oAriaApplication.remotecompanydata.SqlRun("Select * From RETLINE[INDEX = RETLINES]" +IIF(llDateSelect,"WHERE CRDATE BETWEEN '"+ ;
                                DToC(ldRpFDate)+"' AND '"+DTOC(ldRpSDate)+"'",""),"RETLINE", ;
   "RETLINE",lcCurrConn  ,3,'SAVE',SET("Datasession" ))

   IF lnConn  >= 1
      lnHdr =oAriaApplication.remotecompanydata.SqlRun("Select * From RETHDR[INDEX = RETHDR]","RETHDR", ;
   					"RETHDR",lcCurrConn  ,3,'SAVE',SET("Datasession" ))
      IF lnHdr > 0
        SELECT  RETHDR
        lnBuffering = CURSORGETPROP("Buffering",'Rethdr')
        =CURSORSETPROP("Buffering",3,'Rethdr')
        INDEX ON crmemo TAG 'RETHDR'
        SET ORDER TO 'RETHDR'
      ENDIF 
   ELSE
   LOOP    
  ENDIF 
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  lcOldComp = oAriaEnvironment.ActiveCompanyId
  oAriaEnvironment.GetCompanyInformation(lcCompID)
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    lcOldComp = oAriaApplication.ActiveCompanyId
    oAriaApplication.GetCompanyInformation(lcCompID)    
    *1234
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  IF !USED('STYLE')
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
  
    =oAriaEnvironment.remotetableaccess.OpenFile(oAriaEnvironment.DataDir+'STYLE', 'STYLE','SH','STYLE')
    
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE 
    =gfOpenTABLE(oAriaApplication.DATADIR+'STYLE',oAriaApplication.DATADIR+'STYLE','SH','STYLE',.T.)  
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

  ENDIF
 
  SELECT RETLINE
  
  lnCompCnt = RECCOUNT()
  
  SET RELATION TO Retline.crmemo INTO Rethdr ADDITIVE
  SCAN FOR IIF(llDateSelect  ,BETWEEN(cRDate,ldRpFDate,ldRpSDate),.T.) .AND. RETHDR.STATUS <> "V"
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
    
    lnPerCent = RECNO()/lnCompCnt 
    IF MOD(RECNO(),CEILING(lnCompCnt / 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
      loProgress.Description = "Collecting Data For Company:"+lcCompID 
      loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    ENDIF  
    
    =oAriaEnvironment.remotetableaccess.SeekRecord(Retline.style ,'Style','Style')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE 
      WAIT WINDOW NOWAIT "Collecting Data For Company:"+lcCompID 
      gfSeek(Retline.style ,'Style','Style')
    ENDIF
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
    
    m.Ccom_name  = SYCCOMP_L.Ccom_name
    m.season     = STYLE.SEASON
    *lcOldDataDir = goAriaEnvironment.DataDir
    *goAriaEnvironment.DataDir = ADDBS(lcComDir)
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
    
    m.CtermDesc  = oAriaEnvironment.codes.getcodedescription(RETHDR.Ctermcode,PADR('CTERMCODE',10))
    m.ReasonDesc = oAriaEnvironment.codes.getcodedescription(RETLINE.Reason,PADR('REASON',10))
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE 
      m.CtermDesc  = gfCodDes(RETHDR.Ctermcode,PADR('CTERMCODE',10))
      m.ReasonDesc = gfCodDes(RETLINE.Reason,PADR('REASON',10))
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
        
   * goAriaEnvironment.DataDir = lcOldDataDir 
    m.cRepCode   = IIF(!EMPTY(RETHDR.Salesrep1),RETHDR.Salesrep1,RETHDR.Salesrep2)
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
    
    lcOldDataDir = oAriaEnvironment.DataDir
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE 
      lcOldDataDir = oAriaApplication.dataDir
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
    
   * goAriaEnvironment.DataDir = ADDBS(lcComDir)
   
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
   
    =oAriaEnvironment.remotetableaccess.OpenTable(ADDBS(lcComDir)+'SALESREP', 'SALESREP','SH')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE 
      =gfOpenTABLE(oAriaApplication.DATADIR+'SALESREP',oAriaApplication.DATADIR+'SALESREP','SH')  
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
        
  *  goAriaEnvironment.DataDir = lcOldDataDir 
    SELECT SALESREP
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
    
    m.cRepName   = IIF(oAriaEnvironment.remotetableaccess.SeekRecord(m.cRepCode,'SALESREP') , SALESREP.Name , SPACE(24))
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE 
      m.cRepName   = IIF(gfSeek(m.cRepCode,'SALESREP') , SALESREP.Name , SPACE(24))
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
    
    oAriaEnvironment.remotetableaccess.CloseTable('SALESREP')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE 
      gfCloseTable('SALESREP')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
    
    m.crmemo     = RETLINE.crmemo
    m.Style      = RETLINE.Style
    m.Totqty     = RETLINE.Totqty
    m.Price      = RETLINE.Price
    m.Exten      = m.Totqty * m.Price
    INSERT INTO (lcTempFile) FROM MEMVAR
  ENDSCAN
  
  IF USED('RETLINE')
    USE IN RETLINE
  ENDIF
  IF USED('RETHDR')
    USE IN RETHDR
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
  
  IF USED('STYLE')
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
    
    oAriaEnvironment.remotetableaccess.CloseTable('STYLE')
    
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE 
    gfCloseTable('STYLE')
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]           
    
  ENDIF
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  oAriaEnvironment.GetCompanyInformation(lcOldComp) 
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE 
    oAriaApplication.GetCompanyInformation(lcOldComp)
    *1234
  ENDIF  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
ENDFOR


FUNCTION lfRestFromXML
LPARAMETERS lcXML

LOCAL lobjDOMDocument
lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
lobjDOMDocument.loadXML(lcXML)

LOCAL loRoot 
loRoot = lobjDOMDocument.childNodes(1).childNodes(0)


PUBLIC laOGHdFlt[1, 8]
PUBLIC laOGFxFlt[1, 8]
PUBLIC laOGVrFlt[1, 8]

LOCAL lnIndex
LOCAL loVariable, lcName, lcDataType, lcValue

FOR lnIndex = 0 TO loRoot.childNodes.Length - 1
  loVariable = loRoot.childNodes(lnIndex)
  lcDataType = loVariable.childNodes(0).text
  lcName     = loVariable.childNodes(1).text
  lcValue    = loVariable.childNodes(2).text

 DO CASE
  CASE UPPER(lcName) = UPPER('laOGHdFlt[')
    DIMENSION laOGHdFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue
  
  CASE UPPER(lcName) = UPPER('laOGFxFlt[')
    DIMENSION laOGFxFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue

  CASE UPPER(lcName) = UPPER('laOGVrFlt[')
    DIMENSION laOGVrFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue
  
  CASE UPPER(lcDataType) = UPPER('System.String')
    PUBLIC &lcName.
    &lcName. = lcValue
      
  CASE UPPER(lcDataType) = UPPER('System.Boolean')
    PUBLIC  &lcName.
    IF !EMPTY(lcValue)
      &lcName. = IIF(lcValue = 'true', .T., .F.)
    ENDIF
      
  CASE UPPER(lcDataType) = UPPER('System.Decimal')
    PUBLIC  &lcName.
    &lcName. = VAL(lcValue)
      
  CASE UPPER(lcDataType) = UPPER('System.Datetime')
    PUBLIC  &lcName.
    IF !EMPTY(lcValue)
      &lcName. = DATE(VAL(LEFT(lcValue, 4)), VAL(SUBSTR(lcValue, 5, 2)), VAL(SUBSTR(lcValue, 7, 2)))
    ENDIF

  CASE UPPER(lcDataType) = UPPER('System.Table')
    IF !EMPTY(lcValue) .AND. !EMPTY(lcName)
      LOCAL lnSelected 
      lnSelected = SELECT()
    
      LOCAL loXMLParase
      loXMLParase = NEWOBJECT("wwxml")
      
      DIMENSION laStruct[loVariable.childNodes(3).childNodes.length,4]
      
      FOR nCount = 0 TO loVariable.childNodes(3).childNodes.length -1 
        laStruct[nCount+1,1] = loVariable.childNodes(3).childNodes(nCount).childNodes (0).text
        laStruct[nCount+1,2] = loVariable.childNodes(3).childNodes(nCount).childNodes (1).text
        laStruct[nCount+1,3] = loVariable.childNodes(3).childNodes(nCount).childNodes (2).text
        laStruct[nCount+1,4] = loVariable.childNodes(3).childNodes(nCount).childNodes (3).text
      ENDFOR  
      
      CREATE CURSOR (lcName) FROM ARRAY laStruct
      loXMLParase.XMLToCursor(lcValue, lcName)
       
      INDEX ON KEYEXP TAG KEYEXP
      
      SELECT(lnSelected)
      
    ENDIF
   ENDCASE
ENDFOR

*!*	cCROrientation = "P"
*!*	llCrystal      = .T.
*!*	lcoglastform   = "ALSTYAL"
*!*	lcActiveMod    = "AL"


*!*	oAriaEnvironment.gcOutFile = IIF(!EMPTY(cTextRepType),gcOutFile,STRTRAN(UPPER(gcOutFile),'.TXT','.PDF'))
*!*	oAriaEnvironment.cTextRepType = IIF(!EMPTY(cTextRepType),cTextRepType,'PDF')
*!*	* MAH
*!*	*IF UPPER(gcDevice) = 'SCREEN' OR UPPER(gcDevice) = 'PRINTER' 
*!*	*  goAriaEnvironment.cTextRepType = 'PDF'
*!*	*ENDIF
*!*	*goAriaEnvironment.gcOutFile = IIF(goAriaEnvironment.cTextRepType = 'PDF' and !EMPTY(goAriaEnvironment.gcOutFile),FORCEEXT(gcOutFile,'PDF'),goAriaEnvironment.gcOutFile)
*!*	* MAH
*!*	goAriaEnvironment.User_ID  = User_ID


*!*	goAriaEnvironment.report.cCROrientation = cCROrientation
*!*	goAriaEnvironment.report.llCrystal = llCrystal
*!*	goAriaEnvironment.report.OGLastForm =lcoglastform
*!*	goAriaEnvironment.ActiveModuleID = lcActiveMod
*:***************************************************************************
*: Name              : lfvArray
*! Developer         : Mariam Mazhar Tawfik [MMT]
*! Date              : 07/02/2008
*: Purpose           : Function to initiate the target and sourece arrays
*:                     in Option Grid
*:***************************************************************************
*: Called from       : SYREPUVR
*:***************************************************************************
*: Calls             : None
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Return		     : None
*:***************************************************************************
*: Example           : = lfvArray()
*:***************************************************************************

FUNCTION lfvArray
PARAMETERS llFromMover


IF !llFromMover
  DIMENSION laRpCompS[1],laRpCompT[1]
  STORE '' TO laRpCompS , laRpCompT
ENDIF 

IF !USED('SYCCOMP_P')

  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End] 

  =oAriaEnvironment.remotetableaccess.OpenFile(oAriaEnvironment.SystemFilesPath+'SYCCOMP', 'Ccomp_id','SH','SYCCOMP_P')
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE 
    =gfOpenTABLE(oAriaApplication.DATADIR+'SYCCOMP',oAriaApplication.DATADIR+'Ccomp_id','SH','SYCCOMP_P')  
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   
  
ENDIF

SELECT SYCCOMP_P
SELECT DISTINCT Ccomp_id+' - '+Ccom_name ;
       FROM SYCCOMP_P INTO ARRAY laRpCompS
