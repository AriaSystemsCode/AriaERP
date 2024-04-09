*!*****************************************************************************************
*! Name      : PSetup
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/15/2002 12:05:17 PM
*! Purpose   : Printer setup global functionality.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: llScreen,llBatch
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
PARAMETERS llScreen,llBatch, lnDataSessionID

IF VARTYPE(lnDataSessionID) != "N"
  IF TYPE("_SCREEN.ActiveForm.DATASESSIONID") = "N"
    lnDataSessionID = _SCREEN.ActiveForm.DATASESSIONID
  ELSE
    lnDataSessionID = 1  && Default data session ID
  ENDIF
ENDIF

LOCAL lnOldDataSessionID, lnActiveAlias

lnActiveAlias      = SELECT(0)
lnOldDataSessionID = SET("Datasession")
SET DATASESSION TO lnDataSessionID

PRIVATE lcRepMode
lcRepMode = IIF(TYPE('lcOGPlatForm') = 'C' , lcOGPlatForm , '')
SET PRINTER OFF
IF TYPE('lcOGPlatForm') = 'C'
  IF lcOGPlatForm = 'WINDOWS' AND oAriaApplication.gcDevice = 'FILE'
    oAriaApplication.gcDevice = 'PRINTER'
  ENDIF
ENDIF

IF lcRepMode = "WINDOWS"  && If Windows Report.
  *=SYS(1037)
  RETURN .T.
ENDIF

PRIVATE ALL LIKE l* EXCEPT lcOGPlatForm
llBatch = IIF(TYPE('llBatch')='L',llBatch,.F.)
llBatch = IIF(TYPE('llBatch')#'U',llBatch,.T.)

DECLARE laDevice[1],laServer[1,2],laOutput[1]
STORE "" TO lcBrTitle,laServer,laOutput,lcTo,lcCc,puOutput,puServer, lcFileName

IF llBatch
  lcPriority = 9
  =lfGetServ()
  llBatch  = !EMPTY(laServer[1,1])
  puServer = laServer[1,1]
ENDIF

DECLARE laPrtPorts[12]
laPrtPorts[1] = 'LPT1'
laPrtPorts[2] = 'LPT2'
laPrtPorts[3] = 'LPT3'
laPrtPorts[4] = 'LPT4'
laPrtPorts[5] = 'LPT5'
laPrtPorts[6] = 'LPT6'
laPrtPorts[7] = 'LPT7'
laPrtPorts[8] = 'LPT8'
laPrtPorts[9] = 'LPT9'
laPrtPorts[10] = 'COM1'
laPrtPorts[11] = 'COM2'
laPrtPorts[12] = 'COM3'

*** Check Whether the Screen Device Will Be Enable or not
rboutput = ATC(LEFT(oAriaApplication.gcDevice,1),'PFB')

lnActFolder = MIN(IIF(llBatch,3,2),rboutput)
lnActFolder = MAX(1,lnActFolder)

*** Initialization for the Device Screen Variables
****** Just added in 11/09/94
oAriaApplication.glPrnt_lan = .F.
rbLan        = 2
llBaner      = oAriaApplication.glBaner
lcBaner_H    = oAriaApplication.gcBaner_H
lcServ_Nam   = oAriaApplication.gcServ_Nam
lcQueu_Nam   = oAriaApplication.gcQueu_Nam
lnPrnt_no    = IIF(TYPE('oAriaApplication.gnPrnt_No')<>"N",0,oAriaApplication.gnPrnt_No)
lcPrnt_Port  = oAriaApplication.gcPrnt_Port

lcWinPrint = IIF(lcRepMode = "DOS",IIF(EMPTY(_PDSETUP),'None',_PDSETUP),"")
lnNumCop   = _PCOPIES
lnStPage   = _PBPAGE
lnEndPage  = _PEPAGE
cbBefore   = (_PEJECT $ "BEFORE,BOTH")
cbAfter    = (_PEJECT $ "AFTER,BOTH")
cbHeader   = oAriaApplication.glHeader

IF rbOutPut  = 2
  lcFileName = oAriaApplication.gcOutFile
ENDIF


*-- Create the pSetup object and call it.
LOCAL loPSetup, lcClassLib
lcClassLib = ADDBS(oAriaApplication.ClassDir) + "OptionGrid.VCX"

llPrint = .F.

loPSetup = NEWOBJECT("pSetup",lcClassLib)  && Create the printer setup object.
IF (VARTYPE(loPSetup) = "O") AND (ATC("form",loPSetup.BaseClass)>0)
  loPSetup.Show(1)    && Show as a modal form.
ENDIF

*-- Restore data environment.
SET DATASESSION TO lnOldDataSessionID
SELECT (lnActiveAlias)

RETURN llPrint
*-- end of pSetup main program code.
*----------------------------------------
*----------------------------------------


*!*****************************************************************************************
*! Name      : lfGetServ
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/15/2002 12:26:10 PM
*! Purpose   : Get server information.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfGetServ
  *-- Open the table
  LOCAL lnRemoteResult, lnCurArea, lcExtended
  lnCurArea = SELECT(0)
  *-- Get the cursor.
  lnRemoteResult = oAriaApplication.RemoteSystemData.Execute("Select * from SYUBSERV Order By cpServerID" ,'',;
                     "SYUBSERV","",oAriaApplication.SystemConnectionString,;
                     3,"",lnDataSessionID)

  IF lnRemoteResult > 0
    SELECT SYUBSERV
    LOCATE
  ELSE
    RETURN
  ENDIF

  *-- Fill the server arrays.
  DIMENSION laServer[1,2]
  STORE '' TO laServer
  SCAN
    IF !EMPTY(laServer[1,1])
      DIMENSION laServer[ALEN(laServer,1)+1,2]
    ENDIF
    lcExtended = ''
    *IF RLOCK()
    *    UNLOCK
    *    lcExtended = ' Idle Server'
    *ENDIF
    laServer[ALEN(laServer,1),1] = ALLT(cpServerId)+' '+ALLT(cServerDes)+lcExtended
    laServer[ALEN(laServer,1),2] = ALLTR(cpServerId)
  ENDSCAN
  *SET REPROCESS TO  lnRePro
  SELECT (lnCurArea)
ENDFUNC
*-- end of lfGetServ.


*!*****************************************************************************************
*! Name      : lfvServerID
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/15/2002 12:26:10 PM
*! Purpose   : Get Output information.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!
FUNCTION lfvServerID
  LOCAL lnSrvNo, lcServerID
  lnSrvNo = ASCAN(laServer,puServer)
  IF lnSrvNo>0
    lcServerID = laServer[ASUB(laServer,lnSrvNo,1),2]
    DIMENSION laoutput[1]
    STORE '' TO laOutPut
    SELECT SYUBSERV
    LOCATE FOR cpServerID = lcServerID
    IF FOUND()
      =gfSubStr(ALLTRIM(MOUTPUT),@laOutput,CHR(27))
      puoutput = laOutput[1]
    ENDIF
  ENDIF
ENDFUNC
*-- end of lfvServerID.
