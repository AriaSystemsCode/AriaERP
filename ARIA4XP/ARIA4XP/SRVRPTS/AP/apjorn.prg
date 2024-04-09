*:***************************************************************************
*: Program file  : APJORN
*: Program desc. : AP JOURNAL
*: System        : ARIA4XP
*: Module        : Accounts Payable(AP)
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 09/28/2009                                                                                                                                                 
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




*-------
IF !USED('APDIST') 
  *=gfOpenTABLE('APDIST','INVVEND','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('APDIST','INVVEND','SH',.F.,.F.)
  SELECT APDIST
  *=gfsetorder('INVVEND')
  oAriaEnvironment.remotetableaccess.Setorderto('INVVEND')
 * =gfseek('')
ENDIF
IF !USED('APSETUP') 
  *=gfOpenTABLE('APSETUP','APSETUP','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('APSETUP','APSETUP','SH',.F.,.F.)
  SELECT APSETUP
  *=gfsetorder('APSETUP')
  oAriaEnvironment.remotetableaccess.Setorderto('APSETUP')
 * =gfSeek('')
 oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF
IF !USED('SYCCOMP') 
  *=gfOpenTable(oAriaApplication.SysPath+"SYCCOMP",'CCOMP_ID','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('SYCCOMP','CCOMP_ID','SH',.F.,.F.)
  SELECT SYCCOMP
  *=gfsetorder('CCOMP_ID')
  oAriaEnvironment.remotetableaccess.Setorderto('CCOMP_ID')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF
IF !USED('APINVHDR') 
  *=gfOpenTABLE('APINVHDR','VENDINV','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('APINVHDR','VENDINV','SH',.F.,.F.)
  SELECT APINVHDR
  *=gfsetorder('VENDINV')
  oAriaEnvironment.remotetableaccess.Setorderto('VENDINV')
 * =gfseek('')
ENDIF
IF !USED('APVENDOR') 
  *=gfOpenTABLE('APVENDOR','VENCODE','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('APVENDOR','VENCODE','SH',.F.,.F.)
  SELECT APVENDOR
  *=gfsetorder('VENCODE')
  oAriaEnvironment.remotetableaccess.Setorderto('VENCODE')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF
IF !USED('GLACCHAR') 
  *=gfOpenTABLE('GLACCHAR','ACCTCODE','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('GLACCHAR','ACCTCODE','SH',.F.,.F.)
  SELECT GLACCHAR
 * =gfsetorder('ACCTCODE')
  oAriaEnvironment.remotetableaccess.Setorderto('ACCTCODE')
  *=gfSeek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF



IF llOGFltCh 
  =lfvCrTemp() &&To create Temp File
  =lfvColData() &&Data Collection
ENDIF 

*-- Choosing the frx according to the selected sort option
DO CASE
  CASE lcRpSort = 'G' &&GL Account
    lcRpForm = IIF(lcRpFormat='A','APJORN','APJORNB')
  CASE lcRpSort = 'T' &&Transaction
    lcRpForm = 'APJORNT'
  CASE lcRpSort = 'V' &&Vendor Invoice
    lcRpForm = 'APJORNV'
ENDCASE     

SELECT &lcTempFile
*DO gfDispRe WITH EVAL('lcRpForm')
oAriaEnvironment.report.print(lcRpForm)


*!**************************************************************************
*! Function      : lfvCrTemp
*! Purpose       : Creating Temp file  
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 09/28/2009
*!**************************************************************************
FUNCTION lfvCrTemp
DIMENSION laTempStru[14,4]

  laTempstru[1,1]='CVENDCODE'
  laTempstru[1,2]='C'
  laTempstru[1,3]= 8
  laTempstru[1,4]= 0
   
  laTempstru[2,1]='CAPDGLACT'
  laTempstru[2,2]='C'
  laTempstru[2,3]= 24
  laTempstru[2,4]= 0

  laTempstru[3,1]='CINVNO'
  laTempstru[3,2]='C'
  laTempstru[3,3]= 12
  laTempstru[3,4]= 0

  laTempstru[4,1]='CFSPPRDID'
  laTempstru[4,2]='C'
  laTempstru[4,3]= 2
  laTempstru[4,4]= 0
  
  laTempstru[5,1]='DAPDTRDAT'
  laTempstru[5,2]='D'
  laTempstru[5,3]= 10
  laTempstru[5,4]= 0

  laTempstru[6,1]='CAPDTRTYP'
  laTempstru[6,2]='C'
  laTempstru[6,3]= 1
  laTempstru[6,4]= 0
        
  laTempstru[7,1]='CAPDREF'
  laTempstru[7,2]='C'
  laTempstru[7,3]= 12
  laTempstru[7,4]= 0
   
  laTempstru[8,1]='CAPSESSNO'
  laTempstru[8,2]='C'
  laTempstru[8,3]= 8
  laTempstru[8,4]= 0
   
  laTempstru[9,1]='CBATCHNO'
  laTempstru[9,2]='C'
  laTempstru[9,3]= 6
  laTempstru[9,4]= 0
  
  laTempstru[10,1]='CTRNSLEDN'
  laTempstru[10,2]='C'
  laTempstru[10,3]= 8
  laTempstru[10,4]= 0
  
  laTempstru[11,1]='NEQVAMNT'
  laTempstru[11,2]='N'
  laTempstru[11,3]= 15
  laTempstru[11,4]= 2
  
  laTempstru[12,1]='CVENCOMP'
  laTempstru[12,2]='C'
  laTempstru[12,3]= 25
  laTempstru[12,4]= 0
  
  laTempstru[13,1]='CFISFYEAR'
  laTempstru[13,2]='C'
  laTempstru[13,3]= 4
  laTempstru[13,4]= 0
  
  laTempstru[14,1]='CDESCRIPTION'
  laTempstru[14,2]='C'
  laTempstru[14,3]= 30
  laTempstru[14,4]= 0
               
IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF 

*=gfCrtTmp(lcTempFile,@laTempStru,"CVENDCODE+CINVNO",lcTempFile,.T.)
oAriaEnvironment.Cursors.createcursor(lcTempFile ,@laTempStru,"CVENDCODE+CINVNO",lcTempFile ,.F.)
*--End of function
*!**************************************************************************
*! Function      : lfvColData
*! Purpose       : Collection of data 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfvColData

lnVendPos = lfGetPos('APDIST.CVENDCODE','laOgVrFlt')
lnGlAcctPos = lfGetPos('APDIST.CAPDGLACT','laOgVrFlt')
lnTrTypePos = lfGetPos('APDIST.CAPDTRTYP','laOgVrFlt')
lnSessNoPos = lfGetPos('APDIST.CAPSESSNO','laOgVrFlt')
lnTrDatePos = lfGetPos('APDIST.DAPDTRDAT','laOgFxFlt')
lcVend = laOGvrFlt[lnVendPos,6]
lcGlAcct = laOGvrFlt[lnGlAcctPos,6]
lcTrType = laOGvrFlt[lnTrTypePos,6]
lcTrDate = laOGfxFlt[lnTrDatePos,6]
lcSessNo = laOGvrFlt[lnSessNoPos,6]

lcPTrD = AT('|',lcTrDate)+1
lcTrDateFrom = SUBSTR(lcTrDate,1,lcPTrD-2)
lcTrDateTo = SUBSTR(lcTrDate,lcPTrD)

lcP = AT('|',lcSessNo)+1
lcSessFrom = SUBSTR(lcSessNo,1,lcP-2)
lcSessTo = SUBSTR(lcSessNo,lcP)
llVenSlct = .F.

lnVen = 0
IF !EMPTY(lcVend) AND USED(lcVend)
  SELECT &lcVend
  COUNT TO lnVen FOR !DELETED()
ENDIF
   
*!*	IF !EMPTY(lcVend) AND USED(lcVend)
*!*	  SELECT &lcVend
*!*	  LOCATE 
*!*	  llVenSlct = !EOF()
*!*	ENDIF
IF llVenSlct
  SCAN
   *gfseek('','APDIST')
   oAriaEnvironment.remotetableaccess.SeekRecord('','APDIST')
   =lfInsLine()
  ENDSCAN  
ELSE 
 * gfseek('','APDIST')
  oAriaEnvironment.remotetableaccess.SeekRecord('','APDIST')
  SELECT APDIST
  =lfInsLine()  
ENDIF

*!**************************************************************************
*! Function      : lfInsLine
*! Purpose       : Inserting lines in Temp File
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfInsLine
SELECT APDIST
  SCAN  
    IF IIF(!EMPTY(laOGVrFlt[lnGlAcctPos,6]),ALLTRIM(APDIST.CAPDGLACT) $ laOGVrFlt[lnGlAcctPos,6], .T. )AND;
       IIF(!EMPTY(laOGVrFlt[lnTrTypePos,6]),IIF(laOGVrFlt[lnTrTypePos,6] ='L',.T.,APDIST.CAPDTRTYP $ laOGVrFlt[lnTrTypePos,6]), .T. )AND;
       IIF(!EMPTY(lcSessFrom) AND !EMPTY(lcSessTo),BETWEEN(VAL(APDIST.CAPSESSNO),VAL(lcSessFrom),VAL(lcSessTo)),.T.)AND;
       IIF(!EMPTY(lcTrDateFrom) AND !EMPTY(lcTrDateTo),BETWEEN(APDIST.DAPDTRDAT,CTOD(lcTrDateFrom),CTOD(lcTrDateTo)),.T.)AND;
       IIF(!EMPTY(lcTrnNo),APDIST.CTRNSLEDN $ lcTrnNo,.T.)AND;
       IIF(lcRpRel='B',.T.,IIF(lcRpRel='R',APDIST.lApdPost,!APDIST.lApdPost))AND;
       IIF(llRpPrven,.T.,!EMPTY(APDIST.CVENDCODE)) AND;
       IIF(lnVen > 0,oAriaEnvironment.remotetableaccess.SeekRecord(APDIST.CVENDCODE,'&lcVend'),.T.)
       *IIF(lnVen > 0,gfseek(APDIST.CVENDCODE,'&lcVend'),.T.)

           
     SCATTER MEMVAR memo 
  
     SELECT &lcTempFile
     
     INSERT INTO &lcTempFile FROM MEMVAR 
     
     *-- Getting the company name from APVENDOR using the vendor code
     lcVenComp =IIF(SEEK(&lcTempFile..cVendCode,'APVENDOR'),APVENDOR.cVenComp,'')
     *gfreplace('cvencomp with lcVenComp')
     replace cvencomp WITH lcvencomp
     lcDesc = IIF(llApGlLink,ALLTRIM(LOOKUP(GLACCHAR.CACCNLDES,capdglact,GLACCHAR.CACCTCODE,"ACCTCODE")),' ')
    * gfreplace('cdescription with lcDesc')
     replace cdescription WITH lcdesc
          
    ENDIF 
  ENDSCAN 
*!**************************************************************************
*! Function      : lfGetPos
*! Purpose       : Getting the number of element from array 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 09/29/2009
*!**************************************************************************
FUNCTION lfGetPos
PARAMETERS lcOpt,lcArray
LOCAL lnPos
lnPos = ASCAN(&lcArray,lcOpt)
lnPos = ASUBSCRIPT(&lcArray,lnPos,1)
RETURN lnPos
*--End of function


************************************************************
*! Name      : lfvSession
*! Developer : Haytham El_Sheltawi
*! Date      : 11/27/1996
*! Purpose   : This function is to validate the AP Session from 
*!             the Option Grid 
************************************************************
*! Called from : The Option Grid
************************************************************
*! Calls       : None
************************************************************
*! Passed Parameters  :  None
************************************************************
*B800868,1 This function was added by HS
************************************************************


*!*	FUNCTION lfvSession

*!*	lcSession = VARREAD()          &&Varible to hold the name of the memory varible used to create the AP Session @ ... GET field   
*!*	IF EMPTY(&lcSession)
*!*	   RETURN
*!*	ENDIF
*!*	&lcSession = PADL(ALLTRIM(&lcSession),8,'0')





*!*************************************************************
*! Name      : lfvRelStat
*! Developer : Amin Khodary 
*! Date      : 08/01/1999
*! Purpose   : This function is used to determine wehther the user 
*!             wants only the released transactions or unreleased 
*!             transactions or both.
*!*************************************************************
*! Called from : APJORN.prg
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B802483,1  AKA  08/01/99 This function is used to soled bug no. 
*!*************************************************************
FUNCTION lfvRelStat
DO CASE
  CASE lcRpRel = 'B'
    * 'Both'  Nothing to do 
  CASE lcRpRel = 'R'
    * Only Rleased tran.  
    lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp) , '' , ' AND ' )  + " APDIST.lApdPost "
  CASE lcRpRel = 'U'
    * Only Unrleased tran.  
    lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp) , '' , ' AND ' )  + " !APDIST.lApdPost "
ENDCASE
RETURN 



*!*************************************************************
*! Name      : lfwApJour
*! Developer : Amin Khodary 
*! Date      : 08/01/1999
*! Purpose   : This function is used as a when function for this report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B802483,1  AKA  08/01/99 This function is used to solve bug no. 
*!*************************************************************
FUNCTION lfwApJour

IF !USED('APVENDOR') 
  *=gfOpenTABLE('APVENDOR','VENCODE','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('APVENDOR','VENCODE','SH',.F.,.F.)
  SELECT APVENDOR
  *=gfsetorder('VENCODE')
  oAriaEnvironment.remotetableaccess.Setorderto('VENCODE')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF

IF !USED('ACCOD') 
  *=gfOpenTable("ACCOD",'ACCSEGNO','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('ACCOD','ACCSEGNO','SH',.F.,.F.)
  SELECT ACCOD
 * =gfsetorder('ACCSEGNO')
  oAriaEnvironment.remotetableaccess.Setorderto('ACCSEGNO')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF

IF !USED('APSETUP') 
  *=gfOpenTABLE('APSETUP','APSETUP','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('APSETUP','APSETUP','SH',.F.,.F.)
  SELECT APSETUP
  *=gfsetorder('APSETUP')
  oAriaEnvironment.remotetableaccess.Setorderto('APSETUP')
  *=gfSeek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF

IF APSETUP.CAPSGLLINK = 'Y'
  llApGlLink = .T.
ELSE 
  llApGlLink = .F.
ENDIF     

*C101831,1 SSE 03/27/2000 Commented out [Begin]
*PRIVATE  lnTrnType

*IF TYPE('INVB(4)') <> 'N'
*  lcTrnNo = ''
*  SHOW GET lcTrnNo  DISABLE   
*ELSE
*  lnTrnType = INVB(4)
*  IF lnTrnType > 1 
*    SHOW GET lcTrnNo ENABLE
*    =lfvTrnNo(lnTrnType)
*  ELSE
*    lcTrnNo = ''  
*    SHOW GET lcTrnNo DISABLE
*  ENDIF
*ENDIF
*C101831,1 SSE 03/27/2000 [End]

*C101831,1 SSE 03/27/2000 To Enable/Disable Objects in OG [Begin]
lnTranPos  = lfVarPos('lcTrnNo')      && Transaction Number Var Position
lnFormtPos = lfVarPos('lcRpFormat')   && Report Format Var Position

lnTrnTyPos = ASCAN(laOGVrFlt,'APDIST.CAPDTRTYP')
IF lnTrnTyPos > 0
  lnTrnTyPos = ASUBSCRIPT(laOGVrFlt,lnTrnTyPos,1)
ENDIF

DO lpShowObj
*C101831,1 SSE 03/27/2000 [End]

RETURN 



*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
*!*	**B802483,1 AKA 
*!*	FUNCTION lfClearRep
*!*	IF FILE(gcWorkDir +lcJourTmp+ '.CDX') 
*!*	  SELECT APDIST
*!*	  CLOSE INDEX
*!*	  ERASE (gcWorkDir +lcJourTmp+ '.CDX')
*!*	ENDIF


*!**************************************************************************
*!
*!      FUNCTION: lfvGetTrTyp
*!
*!**************************************************************************
* *B802483,1 AKA 
FUNCTION lfvGetTrTyp
RETURN IIF(cApdTrTyp = 'A' , 'DM application' ,IIF(cApdTrTyp = 'B' , 'Bank adjustment', IIF(cApdTrTyp = 'H' , 'Cash payments',IIF(cApdTrTyp = 'I' , 'Invoice', IIF(cApdTrTyp = 'M' , 'Manual checks',IIF(cApdTrTyp = 'N' , 'Non check payment',IIF(cApdTrTyp = 'P' , 'Printed checks' ,'')))))))  




*!**************************************************************************
*!
*!      FUNCTION: lfRepshow
*!
*!**************************************************************************
* *B802483,1 AKA 
FUNCTION lfRepshow 
*lcTrnNo = ''
*SHOW GET lcTrnNo  DISABLE 



*!***************************************************************************
*! Name      : lpShowObj
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/26/2000
*! Purpose   : Enable/Disable Report Format option in OG 
*!***************************************************************************
*! Example   : DO lpShowObj
*!***************************************************************************
*C101831,1 SSE 03/27/2000
*
PROCEDURE lpShowObj

*-- If Sort By is GL Account
IF lcRpSort = 'G'
*  laOGObjCnt[lnFormtPos] = .T.     && Enable Report format
*  laOGObjCnt[lnTranPos] = .F.      && Disable Transaction Number
ELSE   && Else Sort By is Transaction
*  laOGObjCnt[lnFormtPos] = .F.     && Disable Report format
  lcRpFormat = "A"                 && Turn Report format to 'A'

  *-- If Transaction Type is Like (Array)
  IF laOGVrFlt[lnTrnTyPos,5] = "Like"

    *-- If Transaction Type is ALL
    IF EMPTY(laOGVrFlt[lnTrnTyPos,6])
     * laOGObjCnt[lnTranPos] = .F.      && Enable Transaction Number
    ELSE  && Transaction Type is any not ALL 
     * laOGObjCnt[lnTranPos] = .T.      && Enable Transaction Number    
    ENDIF  
    *-- EndIf of Transaction Type is ALL

  ELSE   && Transaction Type is In List (Mover)
    * laOGObjCnt[lnTranPos] = .F.      && Enable Transaction Number 
  ENDIF
ENDIF
*-- Endif of Sort By

*=lfOGShowGet('lcRpFormat')   && Show get Object .
*=lfOGShowGet('lcTrnNo')      && Show get Object .
*-- End of lpShowObj.

*!***************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/26/2000
*! Purpose   : get position of some variable in OG
*!***************************************************************************
*! Called from : OG When Function
*!***************************************************************************
*! Example   : = lfVarPos()
*!***************************************************************************
*
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfVarPos.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Abdou Elgendy (Abd)
*! Date      : 12/28/2003
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*B120507,1 ABD - [Begin]
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- end of lfItmPos.
*B120507,1 ABD - [End]
*!***************************************************************************



*!**************************************************************************
*!
*!  FUNCTION lfInputMask
*!
*!**************************************************************************
FUNCTION lfInputMask

IF !USED('APSETUP') 
  *=gfOpenTABLE('APSETUP','APSETUP','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('APSETUP','APSETUP','SH',.F.,.F.)
  SELECT APSETUP
  *=gfsetorder('APSETUP')
  oAriaEnvironment.remotetableaccess.Setorderto('APSETUP')
 * =gfSeek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF
IF !USED('ACCOD') 
  *=gfOpenTable("ACCOD",'ACCSEGNO','SH')
  oAriaEnvironment.remotetableaccess.OpenTable('ACCOD','ACCSEGNO','SH',.F.,.F.)
  SELECT ACCOD
  *=gfsetorder('ACCSEGNO')
  oAriaEnvironment.remotetableaccess.Setorderto('ACCSEGNO')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF
LOCAL lcApsAcMas
lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))

RETURN lcApsAcMas
*end of lfInputMask