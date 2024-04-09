*:***************************************************************************
*: Program file  : APPYINVS
*: Program desc. : Invocie Summary Report
*: System        : ARIA 4XP
*: Module        : Accounts Payable(AP)
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 07/29/2009                                                                                                                                            
*:***************************************************************************
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.Description = "Opening Data Files..."

*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]

SET STEP ON 
LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
loEnvironment.ClientID = ClientID
loEnvironment.ConnectionsRefresh()
*T20100512.0026 Hassan 2010 05 23 [END]

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID), ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

*MESSAGEBOX(lcXMLFileName)

oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

oAriaEnvironment.Report.gcAct_Appl = "AP"
oAriaEnvironment.report.cCROrientation = "L"
oariaenvironment.activeModuleID = 'AP'
oAriaEnvironment.report.cCRPapersize = 'A4'

PUBLIC gcAct_Appl 
gcAct_Appl = "AP"

IF LEFT(gcDevice, 7) = "PRINTER"
  oAriaEnvironment.gcDevice = "PRINTER"
ELSE
  oAriaEnvironment.gcDevice = "FILE"
ENDIF



*----
STORE '' TO lcTempFile
lcTempFile = oAriaEnvironment.Cursors.GetCursorTempName()

IF !USED('APINVHDR') 
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APINVHDR',oAriaApplication.DATADIR+'VENDINV','SH',.F.,.F.)
  SELECT APINVHDR
  oAriaEnvironment.remotetableaccess.Setorderto('VENDINV')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF

=lfvCrTemp()     && To create Temp File
=lfvColData()    && Data Collection

oAriaEnvironment.report.OGLastForm = lcRpForm

SELECT &lcTempFile
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  *= gfModalGen('TRM00052B00000','DIALOG' )
  *SET DEVICE TO SCREEN
  RETURN
ELSE
  loProgress.Percent = 0.9
  loProgress.Description = "Printing Report..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]

  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

  IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
    oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)

    loProgress.Percent = 1.0
    loProgress.Description = "Printing Report..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]
  ENDIF
ENDIF

*!**************************************************************************
*! Function      : lfvColData
*! Purpose       : Collection of data 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 07/19/2009
*!**************************************************************************
FUNCTION lfvColData

lnVendPos = lfGetPos('APINVHDR.CVENDCODE','laOgVrFlt')
lnCurrPos = lfGetPos('APINVHDR.CCURRCODE','laOgFxFlt')
lnMethPos = lfGetPos('APINVHDR.CVENPMETH','laOgVrFlt')
lnDivPos = lfGetPos('APINVHDR.CDIVISION','laOgVrFlt')
lnPriorPos = lfGetPos('APINVHDR.CVENPRIOR','laOgVrFlt')
lcCurr = laOGfxFlt[lnCurrPos,6]
lcVend = laOGvrFlt[lnVendPos,6]
lcMeth = laOGvrFlt[lnMethPos,6]
lcDiv = laOGvrFlt[lnDivPos,6]
lcPrior = laOGvrFlt[lnPriorPos,6]
lcPriorFrom = SUBSTR(lcPrior,1,1)
lnTo = AT('|',lcPrior)+1
lcPriorTo =SUBSTR(lcPrior,lnTo,1)

llVenSlct = .F.
IF !EMPTY(lcVend) AND USED(lcVend)
  SELECT &lcVend
  LOCATE 
  llVenSlct = !EOF()
ENDIF

IF llVenSlct
  SCAN
   gfseek(&lcVend..cVendCode,'APINVHDR')
   SELECT APINVHDR
   =lfInsLine()
 
  ENDSCAN  
ELSE 
  gfseek('','APINVHDR')
  SELECT APINVHDR
  =lfInsLine()  
ENDIF 


*!**************************************************************************
*! Function      : lfInsLine
*! Purpose       : Inserting lines in Temp File
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfInsLine

   SCAN 
    IF IIF(!EMPTY(laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ laOGfxFlt[lnCurrPos,6] , .T. )AND;
       IIF(!EMPTY(laOGvrFlt[lnMethPos,6]),APINVHDR.CVENPMETH $ laOGvrFlt[lnMethPos,6],.T.)AND;
       IIF(!EMPTY(laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ laOGvrFlt[lnDivPos,6],.T.)AND;
       IIF(!EMPTY(laOGvrFlt[lnPriorPos,6]),BETWEEN(APINVHDR.CVENPRIOR,lcPriorFrom,lcPriorTo),.T.)AND;
       IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT !='V')AND;
       IIF(lcRpStatus='A',.T.,IIF(lcRpStatus='O',APINVHDR.NINVPAID < APINVHDR.NINVAMNT,APINVHDR.NINVPAID > APINVHDR.NINVAMNT)AND;
       IIF(!EMPTY(lcRpRefnce),APINVHDR.CINVREF,.T.)AND;
       IIF(!EMPTY(lcRpSess),lfFltSess(),.T.))
       
    SCATTER MEMVAR memo 
    INSERT INTO &lcTempFile FROM MEMVAR 
    SELECT &lcTempFile
    replace CVENPMETH WITH SUBSTR(lcRpVldEnt,ATC('~',lcRpVldEnt,ATC(LEFT(cvenpmeth,1),lcRpVldVal))+1,(ATC('~',lcRpVldEnt,ATC(LEFT(cvenpmeth,1),lcRpVldVal)+1)-1)-(ATC('~',lcRpVldEnt,ATC(LEFT(cvenpmeth,1),lcRpVldVal))))
    replace CAGE WITH IIF((oAriaApplication.SystemDate-DINVDUDAT)+1<0,(oAriaApplication.SystemDate-DINVDUDAT)-1,(oAriaApplication.SystemDate-DINVDUDAT)+1) 
    replace INVAMNT1 WITH gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
    replace PAIDAMNT1 WITH IIF(APINVHDR.CINVSTAT='V',0,gfAmntDisp(APINVHDR.NINVPAID,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    replace DIS_TAK1 WITH gfAmntDisp(APINVHDR.NINVDISTK,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
    replace ADJ_AMNT1 WITH gfAmntDisp(APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
    replace OPEN_BAL1 WITH gfAmntDisp(APINVHDR.NINVAmnt-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
      
    ENDIF
   ENDSCAN 

*!**************************************************************************
*! Function      : lfGetPos
*! Purpose       : Getting the number of element from array 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfGetPos
PARAMETERS lcOpt,lcArray
LOCAL lnPos
lnPos = ASCAN(&lcArray,lcOpt)
lnPos = ASUBSCRIPT(&lcArray,lnPos,1)
RETURN lnPos

*!**************************************************************************
*! Function      : lfvCrTemp
*! Purpose       : Creating Temp file  
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfvCrTemp
DIMENSION laTempStru[18,4]

  laTempstru[1,1]='CVENDCODE'
  laTempstru[1,2]='C'
  laTempstru[1,3]= 8
  laTempstru[1,4]= 0
   
  laTempstru[2,1]='CVENCOMP'
  laTempstru[2,2]='C'
  laTempstru[2,3]= 20
  laTempstru[2,4]= 0

  laTempstru[3,1]='CINVNO'
  laTempstru[3,2]='C'
  laTempstru[3,3]= 8
  laTempstru[3,4]= 0

  laTempstru[4,1]='CINVREF'
  laTempstru[4,2]='C'
  laTempstru[4,3]= 6
  laTempstru[4,4]= 0
  
  laTempstru[5,1]='DINVDATE'
  laTempstru[5,2]='D'
  laTempstru[5,3]= 10
  laTempstru[5,4]= 0

  laTempstru[6,1]='DINVDUDAT'
  laTempstru[6,2]='D'
  laTempstru[6,3]= 10
  laTempstru[6,4]= 0
    
  laTempstru[7,1]='CFACCODE'
  laTempstru[7,2]='C'
  laTempstru[7,3]= 8
  laTempstru[7,4]= 0
    
  laTempstru[8,1]='CVENPRIOR'
  laTempstru[8,2]='C'
  laTempstru[8,3]= 1
  laTempstru[8,4]= 0
   
  laTempstru[9,1]='CVENPMETH'
  laTempstru[9,2]='C'
  laTempstru[9,3]= 30
  laTempstru[9,4]= 0
  
  laTempstru[10,1]='CAGE'
  laTempstru[10,2]='N'
  laTempstru[10,3]= 8
  laTempstru[10,4]= 0
  
  laTempstru[11,1]='INVAMNT1'
  laTempstru[11,2]='N'
  laTempstru[11,3]= 8
  laTempstru[11,4]= 0
  
  laTempstru[12,1]='PAIDAMNT1'
  laTempstru[12,2]='N'
  laTempstru[12,3]= 8
  laTempstru[12,4]= 0
  
  laTempstru[13,1]='DIS_TAK1'
  laTempstru[13,2]='N'
  laTempstru[13,3]= 8
  laTempstru[13,4]= 0
  
  laTempstru[14,1]='ADJ_AMNT1'
  laTempstru[14,2]='N'
  laTempstru[14,3]= 8
  laTempstru[14,4]= 0
  
  laTempstru[15,1]='OPEN_BAL1'
  laTempstru[15,2]='N'
  laTempstru[15,3]= 8
  laTempstru[15,4]= 0
  
  laTempstru[16,1]='CCURRCODE'
  laTempstru[16,2]='C'
  laTempstru[16,3]= 3
  laTempstru[16,4]= 0
  
  laTempstru[17,1]='CINVSTAT'
  laTempstru[17,2]='C'
  laTempstru[17,3]= 8
  laTempstru[17,4]= 0
  
  laTempstru[18,1]='COUTCOMP'
  laTempstru[18,2]='C'
  laTempstru[18,3]= 30
  laTempstru[18,4]= 0
      
IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF 


oAriaEnvironment.Cursors.createcursor(lcTempFile,@laTempStru,"CVENDCODE+CINVNO",lcTempFile,.T.)
*--End of function

*!**************************************************************************
*! Function      : lfwRepWhen
*! Purpose       : Creating Temp file  
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfwRepWhen


IF !USED('APVENDOR') 
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APVENDOR',oAriaApplication.DATADIR+'VENCODE','SH',.F.,.F.)
  SELECT APVENDOR

  oAriaEnvironment.remotetableaccess.Setorderto('VENCODE')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF



*!**************************************************************************
*!
*!      Function: lfFltSess
*!
*!**************************************************************************
*E300301,1 MAN To check if the one of distribution lines for the current
*E300301,1     invoice has the entered session number
FUNCTION lfFltSess
IF !USED('APDIST')
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APDIST',oAriaApplication.DATADIR+'INVVEND','SH',.F.,.F.)
ENDIF

lcOldFl = ALIAS()
SELECT APDIST

oAriaEnvironment.remotetableaccess.SeekRecord(APINVHDR.CINVNO + APINVHDR.CVENDCODE)

LOCATE REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = APINVHDR.CINVNO + APINVHDR.CVENDCODE ;
            FOR   cApSessNo = lcRpSess

IF EMPTY(lcOldFl)
  SELECT 0
ELSE  
  SELECT (lcOldFl)      
ENDIF
RETURN FOUND("APDIST")


*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
*B603131,1 Walid Abou El-Magd
FUNCTION lfClearRep

*AHS
*IF FILE(gcWorkDir +lcApInvHdr+ '.CDX')
IF FILE(oAriaApplication.WorkDir +lcApInvHdr+ '.CDX') 
  SELECT APINVHDR
  CLOSE INDEX
  *ERASE (gcWorkDir +lcApInvHdr+ '.CDX')
  ERASE (oAriaApplication.WorkDir +lcApInvHdr+ '.CDX')
ENDIF

*!**************************************************************************
*!
*!      Function: lfCreatNdx
*!
*!**************************************************************************
*B603131,1 Walid Abou El-Magd
FUNCTION lfCreatNdx
SELECT APINVHDR
* If index name is already defined 
IF TYPE("lcApInvHdr") <> 'U'
  * If index file is already creadted 

*AHS  
  *IF !FILE(gcWorkDir +lcApInvHdr+ '.CDX')
   IF !FILE(oAriaApplication.WorkDir +lcApInvHdr+ '.CDX') 
    lcApInvHdr = gfTempName()
    *INDEX ON cvendcode+ccurrcode+cinvno TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')
     INDEX ON cvendcode+ccurrcode+cinvno TAG cVenCurTag OF (oAriaApplication.WorkDir +  lcApInvHdr + '.CDX')
  ELSE
    *SET ORDER TO TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')  
     SET ORDER TO TAG cVenCurTag OF (oAriaApplication.WorkDir +  lcApInvHdr + '.CDX')   
  ENDIF
ELSE    && ELASE OF  "If index name is already defined "
    lcApInvHdr = gfTempName()          
    *INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')
     INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (oAriaApplication.WorkDir +  lcApInvHdr + '.CDX')       
ENDIF
