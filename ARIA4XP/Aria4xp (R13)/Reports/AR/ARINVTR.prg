*:***************************************************************************
*: Program file  : ARINVTR
*: Program desc. : EDI Invoice Transmission StatusReport
*: Module        : Accounts receivable (AR)
*: Developer     : Mariam Mazhar (MMT)
*: Tracking Job Number: E612450 (T20210718.0001)
*: Date : 08/16/2021
*:***************************************************************************
PARAMETERS lcRequestID, lcXMLFileName, ClientID
IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  LOCAL loEnvironment
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
 
  loEnvironment.ClientId = ClientId
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  lcActiveMod = 'AR'
  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'AR'
  oAriaEnvironment.RequestID = lcRequestID
  PUBLIC gcAct_Appl
  gcAct_Appl = lcActiveMod
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  gcBaseCurr = oAriaEnvironment.BaseCurrency 
  oAriaApplication.SysPath = oAriaEnvironment.systemfilespath 
  oAriaEnvironment.Report.cCROrientation = 'P'
  =gfOpenFile('Invhdr','Invhdr')
  =gfOpenFile('CUSTOMER','CUSTOMER')
ELSE
  loogScroll.cCROrientation = 'P'  
ENDIF

#INCLUDE R:\Aria4xp\reports\ar\arsjour.H

lcStTime   = TIME()    && Time in which we start collect data.
lcDetExp = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))

lcInvDateF = ''
lcInvDateT = ''
SET STEP ON 

ldInvDSt = {}
ldInvDEnd  = {}
llDateInvSelect = .F.
lnPosDate = ASCAN(laOGFXFlt,"INVHDR.INVDATE")
IF lnPosDate> 0 AND !EMPTY(laOGFxFlt[lnPosDate ,6])
  ldInvDSt =  IIF(EMPTY(SUBSTR(laOGFxFlt[lnPosDate ,6],1,10)),CTOD(""),CTOD(SUBSTR(laOGFxFlt[lnPosDate ,6],1,10)))
  ldInvDEnd  = IIF(EMPTY(SUBSTR(laOGFxFlt[lnPosDate ,6],12,21)),CTOD(""),CTOD(SUBSTR(laOGFxFlt[lnPosDate ,6],12,21)))
  lcInvDateF = DTOC(ldInvDSt)
  lcInvDateT = DTOC(ldInvDEnd)
ENDIF


DO CASE
  CASE lcRpSortBy = 'I'  && Sort by invoice
    lcIndexTg  = [INVOICE]    && Index expression.
    lcSubTitle = [Invoice]    && Sub Title
    lcGroup    = ''           && Report Group
    lcGrpFoot  = ['']         && Group title

  CASE lcRpSortBy = 'A'  && Sort by account
    lcIndexTg  = [ACCOUNT + INVOICE]
    lcSubTitle = [Account]
    lcGroup    = [ACCOUNT]
    lcGrpFoot  = [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))+;
                 ['+'# ' + ACCOUNT + " - " + ALLTRIM(CUSTOMER.BTNAME)]
  CASE lcRpSortBy = 'F'  && Sort by location
    lcIndexTg  = [CFACCODE+ INVOICE]
    lcSubTitle = [Factor]
    lcGroup    = [CFACCODE]
    lcGrpFoot  = [']+"Factor"+;&&IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Location,oAriaApplication.GetHeaderText("LANG_Arsjour_Location",AHEADERFILE))+
                ['+'# ' + CFACCODE+ " - " + ALLTRIM(SYCFACT.CFACCODE)]

ENDCASE

IF loOGScroll.llOGFltCh 
  lfCreateTmp ()
  lfCollectData()
ENDIF

SELECT(lcWorkFile)
LOCATE
IF EOF()
   IF TYPE('lcXMLFileName') <> 'C'
     = gfModalGen('TRM00052B00000','DIALOG' )
     SET DEVICE TO SCREEN
   ENDIF  
   RETURN
ENDIF
IF TYPE('lcXMLFileName') <> 'C'
  DO gfDispRe WITH EVAL('lcRpForm')
ELSE
  loProgress.Percent = 0.9
  loProgress.DESCRIPTION = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
  PRIVATE loProxy
  loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  loogScroll   = oAriaEnvironment.REPORT
  oAriaEnvironment.REPORT.OGLastForm = lcRpForm
  oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
  loProgress.Percent = 1.0
  loProgress.DESCRIPTION = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
ENDIF


*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/16/2021
*! Purpose   : Collect Data
*!*************************************************************
FUNCTION lfCollectData

IF !USED('EDITRANS')
  = gfopentable('EDITRANS', 'TYPEKEY')  && CEDITRNTYP+KEY+TYPE+CPARTNER
ENDIF
IF !USED('EDILIBDT')
  = gfopentable('EDILIBDT', 'ACKNOLEDG')  &&  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ
ENDIF
IF !USED('EDIACPRT')
  = gfopentable('EDIACPRT', 'ACCFACT')   && TYPE+CPARTNER
ENDIF
IF !USED('EDIPD')
  = gfopentable('EDIPD', 'PARTTRANS')   && CPARTCODE+CEDITRNTYP+CTRANACTV+CPARTID
ENDIF

lcCursorInvoice = ''
llInvSelect = .F.
lnPosInv = ASCAN(laOGFXFlt,"INVHDR.INVOICE")
IF lnPosInv > 0
  lnPosInv = ASUBSCRIPT(laOGFXFlt,lnPosInv ,1)
  lcCursorInvoice = laOGFXFlt[lnPosInv ,6]
  IF !EMPTY(lcCursorInvoice) AND USED(lcCursorInvoice)
    SELECT(lcCursorInvoice)
    LOCATE
    IF !EOF()
      llInvSelect = .T.
    ENDIF
  ENDIF
ENDIF

lcCursorAccount = ''
llAccSelect = .F.
lnPosAcc = ASCAN(laOGFXFlt,"INVHDR.ACCOUNT")
IF lnPosAcc > 0
  lnPosAcc = ASUBSCRIPT(laOGFXFlt,lnPosAcc ,1)
  lcCursorAccount = laOGFXFlt[lnPosAcc ,6]
  IF !EMPTY(lcCursorAccount ) AND USED(lcCursorAccount )
    SELECT(lcCursorAccount )
    LOCATE
    IF !EOF()
      llAccSelect = .T.
    ENDIF
  ENDIF
ENDIF

lcCursorFactor= ''
llFactorSelect = .F.
lnPosFactor = ASCAN(laOGFXFlt,"INVHDR.CFACCODE")
IF lnPosFactor > 0
  lnPosFactor = ASUBSCRIPT(laOGFXFlt,lnPosFactor ,1)
  lcCursorFactor= laOGFXFlt[lnPosFactor ,6]
  IF !EMPTY(lcCursorFactor) AND USED(lcCursorFactor)
    SELECT(lcCursorFactor)
    LOCATE
    IF !EOF()
      llFactorSelect = .T.
    ENDIF
  ENDIF
ENDIF

lnCntProg = 0
lnCntProgNo = 0
IF TYPE('lcXMLFileName')  = 'C'
   SELECT INVHDR
   =gfSeek('')
   COUNT FOR (!EMPTY(lcCursorAccount) AND SEEK(Account, lcCursorAccount)) AND;
          (!EMPTY(lcCursorInvoice) AND SEEK(Invoice, lcCursorInvoice)) AND ;
          ((!EMPTY(ldInvDSt) OR !EMPTY(ldInvDEnd)) AND BETWEEN(INVHDR.INVDATE,ldInvDSt,ldInvDEnd)) TO lnCntProg
  
ENDIF

IF llInvSelect
  SELECT INVHDR
  =gfSetOrder('INVHDR')
  SELECT(lcCursorInvoice)
  LOCATE 
  SCAN
    IF gfSeek(&lcCursorInvoice..KeyExp,'INVHDR','INVHDR')
      IF llFactorSelect AND (EMPTY(INVHDR.CFACCODE) OR !SEEK(INVHDR.CFACCODE,lcCursorFactor))
        SELECT (lcCursorInvoice)
        LOOP 
      ENDIF
      IF llAccSelect AND !SEEK(INVHDR.ACCOUNT,lcCursorAccount)
        SELECT (lcCursorInvoice)
        LOOP 
      ENDIF
      IF (!EMPTY(ldInvDSt) OR !EMPTY(ldInvDEnd)) AND !BETWEEN(INVHDR.INVDATE,ldInvDSt,ldInvDEnd)
        LOOP 
      ENDIF
      
      IF TYPE('lcXMLFileName') <> 'C'
        WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
            INVHDR.Invoice NOWAIT
      ELSE 
        IF lnCntProg > 0
          lnCntProgNo = lnCntProgNo + 1
          lnPerCent = lnCntProgNo/lnCntProg
          IF MOD(lnCntProgNo,CEILING(lnCntProg/ 10)) = 0
            loProgress.Percent = lnPerCent * 0.9
            loProgress.Description = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
                INVHDR.Invoice 
           loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
         ENDIF 
       ENDIF  
     ENDIF

      SCATTER MEMVAR MEMO
      m.cTempKey = ''
      =gfSeek('M'+INVHDR.ACCOUNT,'CUSTOMER','CUSTOMER')
      m.cstname=CUSTOMER.btname
      m.btname =CUSTOMER.btname
      IF gfSEEK('A'+INVHDR.Account,'EDIACPRT') AND gfSEEK(EDIACPRT.cPartCode+'810','EDIPD')  
        IF !gfSEEK('810'+PADR(INVHDR.Invoice,40)+'A'+INVHDR.Account,'EDITRANS','TYPEKEY') 
           m.cAccSnt = 'No'
           m.cAccAck ='N\A'   
        ELSE
          IF EDITRANS.cStatus ='N'
            m.cAccSnt = 'No'
            m.cAccAck = 'N\A' 
          ELSE
            m.cAccSnt = 'Yes' 
            IF gfSeek('S'+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
              SELECT EDILIBDT
              LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= 'S'+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
              IF FOUND()
                IF EDILIBDT.cackstatus = 'A'   
                  m.cAccAck = 'Yes'         
                ELSE
                IF EDILIBDT.cackstatus = 'R'   
                  m.cAccAck = 'Rejected' 
                ELSE
                  m.cAccAck = 'No'
                ENDIF
              ENDIF 
            ELSE
              IF gfSeek(' '+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                SELECT EDILIBDT
                LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= ' '+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                IF FOUND() 
    	            IF EDILIBDT.cackstatus = 'A'   
    	              m.cAccAck = 'Yes'          
    	            ELSE
	                  IF EDILIBDT.cackstatus = 'R'   
    	                m.cAccAck = 'Rejected'
    	              ELSE
    	                m.cAccAck = 'No'
    	              ENDIF
	                ENDIF 
                ENDIF  
              ENDIF   
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ELSE
      m.cAccSnt = 'N\A' 
      m.cAccAck = 'N\A'  
    ENDIF
    IF !EMPTY(INVHDR.cFacCode)
      IF gfSEEK('F'+INVHDR.cFacCode,'EDIACPRT') AND gfSEEK(EDIACPRT.cPartCode+'810','EDIPD')
        IF !gfSEEK('810'+PADR(INVHDR.Invoice,40)+'F'+INVHDR.cFacCode)
          m.cFacSnt = 'No' 
          m.cFacACK  = 'N\A' 
        ELSE
          IF EDITRANS.cStatus = 'N'
            m.cFacSnt = 'No' 
            m.cFacACK  = 'N\A' 
          ELSE
            m.cFacSnt = 'Yes' 
            IF gfSeek('S'+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
              SELECT EDILIBDT
              LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= 'S'+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
              IF FOUND()
                IF EDILIBDT.cackstatus = 'A'   
  	              m.cAccAck = 'Yes'         
  	            ELSE
  	              IF EDILIBDT.cackstatus = 'R'   
  	                m.cAccAck ='Rejected' 
  	              ELSE
  	                m.cAccAck ='No' 
  	              ENDIF
  	            ENDIF 
              ELSE
                IF gfSeek(' '+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                  SELECT EDILIBDT
                  LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= ' '+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                  IF FOUND() 
    	              IF EDILIBDT.cackstatus = 'A'   
    	                m.cAccAck ='Yes'      
    	              ELSE
    	                IF EDILIBDT.cackstatus = 'R'   
    	                  m.cAccAck = 'Rejected' 
    	                ELSE
    	                  m.cAccAck = 'No' 
    	                ENDIF
    	              ENDIF 
                  ENDIF  
                ENDIF 
              ENDIF
            ENDIF    
          ENDIF
        ENDIF
      ELSE
        m.cFacSnt ='N\A' 
        m.cFacACK ='N\A'     
      ENDIF
    ELSE
       m.cFacSnt = 'N\A'
       m.cFacACK  ='N\A'    
    ENDIF
    IF ((lcRpSentTo ='B' OR lcRpSentTo ='A') AND m.cAccSnt = 'No') OR ;
      ((lcRpSentTo ='B' OR lcRpSentTo ='F') AND m.cFacSnt = 'No') OR ;
     ((lcRpAckBy ='B' OR lcRpAckBy ='A') AND m.cAccAck = 'No') OR ;
     ((lcRpAckBy ='B' OR lcRpAckBy ='F') AND m.cFacACK  = 'No') 
      INSERT INTO (lcWorkFile) FROM MEMVAR
    ENDIF  
    ENDIF
  ENDSCAN
ELSE
  IF llAccSelect 
    SELECT INVHDR
    =gfSetOrder('INVHDRA')
    SELECT (lcCursorAccount )
    LOCATE
    SCAN
      SELECT INVHDR
      =gfSeek(&lcCursorAccount..KeyExp)
      SCAN REST WHILE  ACCOUNT+INVOICE = &lcCursorAccount..KeyExp 
        IF llFactorSelect AND (EMPTY(INVHDR.CFACCODE) OR !SEEK(INVHDR.CFACCODE,lcCursorFactor))
          LOOP 
        ENDIF
        IF (!EMPTY(ldInvDSt) OR !EMPTY(ldInvDEnd)) AND !BETWEEN(INVHDR.INVDATE,ldInvDSt,ldInvDEnd)
          LOOP 
        ENDIF
              
       IF TYPE('lcXMLFileName') <> 'C'
        WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
            INVHDR.Invoice NOWAIT
      ELSE 
        IF lnCntProg > 0
          lnCntProgNo = lnCntProgNo + 1
          lnPerCent = lnCntProgNo/lnCntProg
          IF MOD(lnCntProgNo,CEILING(lnCntProg/ 10)) = 0
            loProgress.Percent = lnPerCent * 0.9
            loProgress.Description = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
                INVHDR.Invoice 
           loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
         ENDIF 
       ENDIF  
     ENDIF

        
        SCATTER MEMVAR MEMO
        m.cTempKey = ''
        =gfSeek('M'+INVHDR.ACCOUNT,'CUSTOMER','CUSTOMER')
        m.cstname=CUSTOMER.btname
        m.btname =CUSTOMER.btname
        IF gfSEEK('A'+INVHDR.Account,'EDIACPRT') AND gfSEEK(EDIACPRT.cPartCode+'810','EDIPD')  
          IF !gfSEEK('810'+PADR(INVHDR.Invoice,40)+'A'+INVHDR.Account,'EDITRANS','TYPEKEY') 
             m.cAccSnt = 'No'
             m.cAccAck = 'N\A'    
          ELSE
            IF EDITRANS.cStatus ='N'
              m.cAccSnt = 'No'
              m.cAccAck = 'N\A' 
            ELSE
              m.cAccSnt = 'Yes' 
              IF gfSeek('S'+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                SELECT EDILIBDT
                LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= 'S'+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                IF FOUND()
                  IF EDILIBDT.cackstatus = 'A'   
                    m.cAccAck = 'Yes'         
                  ELSE
                  IF EDILIBDT.cackstatus = 'R'   
                    m.cAccAck = 'Rejected' 
                  ELSE
                    m.cAccAck = 'No'
                  ENDIF
                ENDIF 
              ELSE
                IF gfSeek(' '+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                  SELECT EDILIBDT
                  LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= ' '+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                  IF FOUND() 
                    IF EDILIBDT.cackstatus = 'A'   
                      m.cAccAck = 'Yes'          
                    ELSE
                      IF EDILIBDT.cackstatus = 'R'   
                        m.cAccAck = 'Rejected'
                      ELSE
                        m.cAccAck = 'No'
                      ENDIF
                    ENDIF 
                  ENDIF  
                ENDIF   
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        m.cAccSnt = 'N\A' 
        m.cAccAck = 'N\A'  
      ENDIF
      IF !EMPTY(INVHDR.cFacCode)
        IF gfSEEK('F'+INVHDR.cFacCode,'EDIACPRT') AND gfSEEK(EDIACPRT.cPartCode+'810','EDIPD')
          IF !gfSEEK('810'+PADR(INVHDR.Invoice,40)+'F'+INVHDR.cFacCode)
            m.cFacSnt = 'No' 
            m.cFacACK  = 'N\A'  
          ELSE
            IF EDITRANS.cStatus = 'N'
              m.cFacSnt = 'No' 
              m.cFacACK  = 'N\A'  
            ELSE
              m.cFacSnt = 'Yes' 
              IF gfSeek('S'+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                SELECT EDILIBDT
                LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= 'S'+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                IF FOUND()
                  IF EDILIBDT.cackstatus = 'A'   
                    m.cAccAck = 'Yes'         
                  ELSE
                    IF EDILIBDT.cackstatus = 'R'   
                      m.cAccAck ='Rejected' 
                    ELSE
                      m.cAccAck ='No' 
                    ENDIF
                  ENDIF 
                ELSE
                  IF gfSeek(' '+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                    SELECT EDILIBDT
                    LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= ' '+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                    IF FOUND() 
                      IF EDILIBDT.cackstatus = 'A'   
                        m.cAccAck ='Yes'      
                      ELSE
                        IF EDILIBDT.cackstatus = 'R'   
                          m.cAccAck = 'Rejected' 
                        ELSE
                          m.cAccAck = 'No' 
                        ENDIF
                      ENDIF 
                    ENDIF  
                  ENDIF 
                ENDIF
              ENDIF    
            ENDIF
          ENDIF
        ELSE
          m.cFacSnt ='N\A' 
          m.cFacACK ='N\A'     
        ENDIF
      ELSE
         m.cFacSnt = 'N\A'
         m.cFacACK  ='N\A'    
      ENDIF
      IF ((lcRpSentTo ='B' OR lcRpSentTo ='A') AND m.cAccSnt = 'No') OR ;
        ((lcRpSentTo ='B' OR lcRpSentTo ='F') AND m.cFacSnt = 'No') OR ;
       ((lcRpAckBy ='B' OR lcRpAckBy ='A') AND m.cAccAck = 'No') OR ;
       ((lcRpAckBy ='B' OR lcRpAckBy ='F') AND m.cFacACK  = 'No') 
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDIF  
      ENDSCAN 
    ENDSCAN 
  ELSE
    SELECT INVHDR
    =gfSetOrder('INVHDR')
    SELECT INVHDR
      =gfSeek('')
      SCAN 
        IF llFactorSelect AND (EMPTY(INVHDR.CFACCODE) OR !SEEK(INVHDR.CFACCODE,lcCursorFactor))
          LOOP 
        ENDIF
        IF (!EMPTY(ldInvDSt) OR !EMPTY(ldInvDEnd)) AND !BETWEEN(INVHDR.INVDATE,ldInvDSt,ldInvDEnd)
          LOOP 
        ENDIF
              
     IF TYPE('lcXMLFileName') <> 'C'
        WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
            INVHDR.Invoice NOWAIT
      ELSE 
        IF lnCntProg > 0
          lnCntProgNo = lnCntProgNo + 1
          lnPerCent = lnCntProgNo/lnCntProg
          IF MOD(lnCntProgNo,CEILING(lnCntProg/ 10)) = 0
            loProgress.Percent = lnPerCent * 0.9
            loProgress.Description = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
                INVHDR.Invoice 
           loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
         ENDIF 
       ENDIF  
     ENDIF

        
        SCATTER MEMVAR MEMO
        m.cTempKey = ''
        =gfSeek('M'+INVHDR.ACCOUNT,'CUSTOMER','CUSTOMER')
        m.cstname=CUSTOMER.btname
        m.btname =CUSTOMER.btname
        IF gfSEEK('A'+INVHDR.Account,'EDIACPRT') AND gfSEEK(EDIACPRT.cPartCode+'810','EDIPD')  
          IF !gfSEEK('810'+PADR(INVHDR.Invoice,40)+'A'+INVHDR.Account,'EDITRANS','TYPEKEY') 
             m.cAccSnt = 'No'
             m.cAccAck = 'N\A'    
          ELSE
            IF EDITRANS.cStatus ='N'
              m.cAccSnt = 'No'
              m.cAccAck = 'N\A' 
            ELSE
              m.cAccSnt = 'Yes' 
              IF gfSeek('S'+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                SELECT EDILIBDT
                LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= 'S'+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                IF FOUND()
                  IF EDILIBDT.cackstatus = 'A'   
                    m.cAccAck = 'Yes'         
                  ELSE
                  IF EDILIBDT.cackstatus = 'R'   
                    m.cAccAck = 'Rejected' 
                  ELSE
                    m.cAccAck = 'No'
                  ENDIF
                ENDIF 
              ELSE
                IF gfSeek(' '+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                  SELECT EDILIBDT
                  LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= ' '+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                  IF FOUND() 
                    IF EDILIBDT.cackstatus = 'A'   
                      m.cAccAck = 'Yes'          
                    ELSE
                      IF EDILIBDT.cackstatus = 'R'   
                        m.cAccAck = 'Rejected'
                      ELSE
                        m.cAccAck = 'No'
                      ENDIF
                    ENDIF 
                  ENDIF  
                ENDIF   
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        m.cAccSnt = 'N\A' 
        m.cAccAck = 'N\A'  
      ENDIF
      IF !EMPTY(INVHDR.cFacCode)
        IF gfSEEK('F'+INVHDR.cFacCode,'EDIACPRT') AND gfSEEK(EDIACPRT.cPartCode+'810','EDIPD')
          IF !gfSEEK('810'+PADR(INVHDR.Invoice,40)+'F'+INVHDR.cFacCode)
            m.cFacSnt = 'No' 
            m.cFacACK  = 'N\A' 
          ELSE
            IF EDITRANS.cStatus = 'N'
              m.cFacSnt = 'No' 
              m.cFacACK  = 'N\A'  
            ELSE
              m.cFacSnt = 'Yes' 
              IF gfSeek('S'+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                SELECT EDILIBDT
                LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= 'S'+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                IF FOUND()
                  IF EDILIBDT.cackstatus = 'A'   
                    m.cAccAck = 'Yes'         
                  ELSE
                    IF EDILIBDT.cackstatus = 'R'   
                      m.cAccAck ='Rejected' 
                    ELSE
                      m.cAccAck ='No' 
                    ENDIF
                  ENDIF 
                ELSE
                  IF gfSeek(' '+PADR(EDIACPRT.cPartCode,6)+'810','EDILIBDT')
                    SELECT EDILIBDT
                    LOCATE REST WHILE  CEDIFILTYP+CPARTCODE+CEDITRNTYP+CGROUPSEQ+CTRANSEQ= ' '+PADR(EDIACPRT.cPartCode,6)+'810' FOR edilibdt.ceditranno = INVHDR.Invoice
                    IF FOUND() 
                      IF EDILIBDT.cackstatus = 'A'   
                        m.cAccAck ='Yes'      
                      ELSE
                        IF EDILIBDT.cackstatus = 'R'   
                          m.cAccAck = 'Rejected' 
                        ELSE
                          m.cAccAck = 'No' 
                        ENDIF
                      ENDIF 
                    ENDIF  
                  ENDIF 
                ENDIF
              ENDIF    
            ENDIF
          ENDIF
        ELSE
          m.cFacSnt ='N\A' 
          m.cFacACK ='N\A'     
        ENDIF
      ELSE
         m.cFacSnt = 'N\A'
         m.cFacACK  ='N\A'    
      ENDIF
      IF ((lcRpSentTo ='B' OR lcRpSentTo ='A') AND m.cAccSnt = 'No') OR ;
        ((lcRpSentTo ='B' OR (lcRpSentTo ='F' AND !EMPTY(m.cFaccode))) AND m.cFacSnt = 'No') OR ;
       ((lcRpAckBy ='B' OR lcRpAckBy ='A') AND m.cAccAck = 'No') OR ;
       ((lcRpAckBy ='B' OR (lcRpAckBy ='F' AND !EMPTY(m.cFaccode))) AND m.cFacACK  = 'No') 
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDIF  
      ENDSCAN 
  ENDIF   
ENDIF

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Heba Amin (HMA)
*! Date      : 04/08/2004
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/16/2021
*! Purpose   : Option Grid When function
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwRepWhen
IF !('AS' $ oAriaApplication.CompanyInstalledModules) AND !('EB' $ oAriaApplication.CompanyInstalledModules) AND ;
   !('UP' $ oAriaApplication.CompanyInstalledModules) 
   RETURN .F.
ENDIF
lcGroup = [Invoice]

*!*************************************************************
*! Name      : lfCreateTmp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/16/2021
*! Purpose   : Create Temp cursor
*!*************************************************************
FUNCTION lfCreateTmp
SELECT INVHDR
=AFIELD(laTempStru)
lnTempStru=ALEN(laTempStru,1) + 7
DIMENSION laTempStru[lnTempStru,18]
laTempStru[lnTempStru-1,1] = 'cstname'
laTempStru[lnTempStru-1,2] = 'C'
laTempStru[lnTempStru-1,3] = 30
laTempStru[lnTempStru-1,4] = 0
FOR  lnInc=7 TO 16
  STORE SPACE(1) TO laTempStru[lnTempStru-1,lnInc]
ENDFOR
STORE 0  TO laTempStru[lnTempStru-1,17], laTempStru[lnTempStru-1,18]
  *-- cTempKey : field used in most sort by case as the master key.
  *--          : note that field width is dependent on number of sort
  *--          : case he make.
laTempStru[lnTempStru,1] = 'cTempKey'
laTempStru[lnTempStru,2] = 'C'
laTempStru[lnTempStru,3] = 62
laTempStru[lnTempStru,4] = 0
FOR  lnInc=7 TO 16
  STORE SPACE(1) TO laTempStru[lnTempStru,lnInc]
ENDFOR
STORE 0  TO laTempStru[lnTempStru,17], laTempStru[lnTempStru,18]
  

laTempStru[lnTempStru-2,1] = 'cAccSnt'
laTempStru[lnTempStru-2,2] = 'C'
laTempStru[lnTempStru-2,3] = 3
laTempStru[lnTempStru-2,4] = 0
FOR  lnInc=7 TO 16
  STORE SPACE(1) TO laTempStru[lnTempStru-2,lnInc]
ENDFOR
STORE 0  TO laTempStru[lnTempStru-2,17], laTempStru[lnTempStru-2,18]
  
laTempStru[lnTempStru-3,1] = 'cFacSnt'
laTempStru[lnTempStru-3,2] = 'C'
laTempStru[lnTempStru-3,3] = 3
laTempStru[lnTempStru-3,4] = 0
FOR  lnInc=7 TO 16
  STORE SPACE(1) TO laTempStru[lnTempStru-3,lnInc]
ENDFOR
  
STORE 0  TO laTempStru[lnTempStru-3,17], laTempStru[lnTempStru-3,18]
  
laTempStru[lnTempStru-4,1] = 'cAccAck'
laTempStru[lnTempStru-4,2] = 'C'
laTempStru[lnTempStru-4,3] = 8
laTempStru[lnTempStru-4,4] = 0

FOR  lnInc=7 TO 16
  STORE SPACE(1) TO laTempStru[lnTempStru-4,lnInc]
ENDFOR
STORE 0  TO laTempStru[lnTempStru-4,17], laTempStru[lnTempStru-4,18]
  
laTempStru[lnTempStru-5,1] = 'cFacACK'
laTempStru[lnTempStru-5,2] = 'C'
laTempStru[lnTempStru-5,3] = 8
laTempStru[lnTempStru-5,4] = 0
FOR  lnInc=7 TO 16
  STORE SPACE(1) TO laTempStru[lnTempStru-5,lnInc]
ENDFOR
STORE 0  TO laTempStru[lnTempStru-5,17], laTempStru[lnTempStru-5,18]

laTempStru[lnTempStru-6,1] = 'BTNAME'
laTempStru[lnTempStru-6,2] = 'C'
laTempStru[lnTempStru-6,3] =30
laTempStru[lnTempStru-6,4] = 0
FOR  lnInc=7 TO 16
  STORE SPACE(1) TO laTempStru[lnTempStru-6,lnInc]
ENDFOR
STORE 0  TO laTempStru[lnTempStru-6,17], laTempStru[lnTempStru-6,18]




DIMENSION laIndex[3,2]
laIndex[1,1] = "INVOICE"
laIndex[1,2] = lcWorkFile

laIndex[2,1] = "Account+Invoice"
laIndex[2,2] = 'AccSort'


laIndex[3,1] = "CFACCODE+INVOICE"
laIndex[3,2] = 'FacSort'
=gfCrtTmp(lcWorkFile,@laTempStru,@laIndex,lcWorkFile,.F.)

  
  