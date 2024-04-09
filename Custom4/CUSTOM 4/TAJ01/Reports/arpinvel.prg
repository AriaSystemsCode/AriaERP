 *:***************************************************************************
*: Program file  : ARPINVTJ.PRG
*: Developer     : Esraa Ahmed	(ES)
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVTJ
*:***************************************************************************
*: Modifications:
*C612119,1 Es 05/05/2020  [P20200316.0002]
*C202434,1 MMT 02/27/2022 Custom Invoice form for Ely and Walker[T20220201.0002]
*:***************************************************************************
IF llRpPstDue
  IF !USED('DEBIT')
    =gfOpenTable ("Debit","Debit")
  ENDIF
  lcRpExp = lcRpExp + " AND invhdr.duedate < oAriaApplication.SystemDate and gfSEEK ('1'+INVHDR.INVOICE,'DEBIT','DRTRAN')"
ENDIF
*XX
*IF oAriaApplication.ProcessID = 'ARARDINV'
*IF 'ARDINV' $ UPPER(oAriaApplication.ProcessID)
IF ALLTRIM(UPPER(oAriaApplication.ProcessID)) <> 'ARPINV' 
  lcRpPrSt = ""
ENDIF
*XX
*XX
IF oAriaApplication.gcDevice = "FILE" AND "EXCEL" $ loogScroll.cTextRepType
  RETURN
ENDIF
*C202434,1 MMT 02/27/2022 Custom Invoice form for Ely and Walker[T20220201.0002][Start]
SET STEP ON 
SELECT INVHDR
LOCATE FOR &lcRpExp. AND INVHDR.CDIVISION='ELY'
IF FOUND()
  SELECT INVHDR
*  SET FILTER TO (&lcRpExp. AND INVHDR.CDIVISION='ELY')
  LOCATE
  lcFormName='ARPINVEL'
  loogscroll.lcFormName ='ARPINVEL'
  loOgScroll.lcOGLastForm = "ARPINVEL"
 
  lcOGTmpForm   = loogscroll.gfTempName()
 
  IF TYPE('lcXMLFileName') <> 'C'
    loOgScroll.lcRpFrxMod = "Graphics"
    = gfCrtFrm("ARPINVEL","",.T.)
 
    DO gfDispRe WITH EVAL('loogscroll.lcFormName') , 'FOR ' + lcRpExp +" AND CDIVISION='ELY'"
    

  ELSE
    loProgress.Percent = 0.9
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

    PRIVATE loProxy
    loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    loOGScroll   = oAriaEnvironment.report
    oAriaEnvironment.report.OGLastForm = loogscroll.lcFormName
    oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm, 'FOR ' +lcRpExp +" AND CDIVISION='ELY'")
    loProgress.Percent = 1.0
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
   
  ENDIF  
  lcFormName= "ARPINVTJ"
  loogscroll.lcFormName ='ARPINVTJ'
  loOgScroll.lcOGLastForm = "ARPINVTJ"
  lcOGTmpForm   = loogscroll.gfTempName()
  IF TYPE('lcXMLFileName') <> 'C'
    loOgScroll.lcRpFrxMod = "Graphics"
    = gfCrtFrm("ARPINVTJ","",.T.)
  ENDIF 
ENDIF
SELECT INVHDR
LOCATE FOR &lcRpExp. AND INVHDR.CDIVISION<>'ELY'
IF FOUND()
  IF TYPE('lcXMLFileName') <> 'C'
  lcFormName= "ARPINVTJ"
  loogscroll.lcFormName ='ARPINVTJ'
  loOgScroll.lcOGLastForm = "ARPINVTJ"
  lcOGTmpForm   = loogscroll.gfTempName()
  IF TYPE('lcXMLFileName') <> 'C'
    loOgScroll.lcRpFrxMod = "Graphics"
    = gfCrtFrm("ARPINVTJ","",.T.)
  ENDIF 
 
    DO gfDispRe WITH EVAL('loogscroll.lcFormName') , 'FOR ' + lcRpExp +" AND CDIVISION !='ELY'" 
  ELSE
    loProgress.Percent = 0.9
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

    PRIVATE loProxy
    loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    loOGScroll   = oAriaEnvironment.report
    oAriaEnvironment.report.OGLastForm = loogscroll.lcFormName
    oAriaEnvironment.report.lAdditive = .T.
    oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm, 'FOR ' +lcRpExp +" AND CDIVISION !='ELY'" )
    loProgress.Percent = 1.0
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
  ENDIF
ENDIF
llarpinv = .F.
*C202434,1 MMT 02/27/2022 Custom Invoice form for Ely and Walker[T20220201.0002][End]
