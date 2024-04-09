*C201844,1 RAS 2016-07-13 add trigger to IKEddi to check Discripencoes after updating 820 [T20160713.0015]
*!**************************************************************************
*! Name      : IKIMAIN.PRG
*! Developer : AEG (Abdelrahman Essam)
*! Date      : 01/19/2016
*! Purpose   : IKEE Custom Process Program.
*!  C200915  ==> for Aria4  attachments
*!  C200916  ==> for Aria27 attachments
*! Ticket id T20071119.0001
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
* Modifications
*:***************************************************************************

Parameter lcevntfun, lcfunpars
lcfunpars = Iif(Type('lcFunPars') = 'C', lcfunpars, '')
lcfuntorun = 'lf' + Alltrim(lcevntfun) + '(' + lcfunpars + ')'
llretvalue = Evaluate(lcfuntorun)
Return llretvalue
**
*:**************************************************************************





*:**************************************************************************
*:* Name        : GETSHRTC
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 01/19/2016
*:* Purpose     : Get shortcut for edipd class
*:**************************************************************************
FUNCTION lfikgetshrtc 

lnReturn = 4
lcString = "\<Process,\<Update,\<Reported Errors,\<Print"
lcStatus = IIF(EDILIBDT.cStatus<>'U',"T","F")+;
           IIF(EDILIBDT.cStatus='A',"T","F")+;
           IIF(INLIST(EDILIBDT.cStatus,'R','E'),"T","F")+;
           IIF(INLIST(EDILIBDT.cStatus,'A','U','E'),"T","F")

RETURN .T.


*:**************************************************************************
*:* Name        : SHRTCT
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 01/20/2016
*:* Purpose     : Get shortcut for edira class
*:**************************************************************************
FUNCTION lfikshrtct 

lnReturn = 4

*C201844,1 RAS 2016-07-13 add trigger to IKEddi to check Discripencoes after updating 820 [begin]

*!*	lcString = "\<Process,\<Update,\<Reported Errors,\<Print"
*!*	lcStatus = IIF(EDILIBDT.cStatus<>'U',"T","F")+;
*!*	           IIF(EDILIBDT.cStatus='A',"T","F")+;
*!*	           IIF(INLIST(EDILIBDT.cStatus,'R','E'),"T","F")+;
*!*	           IIF(INLIST(EDILIBDT.cStatus,'A','U','E'),"T","F")

lcString = "\<Process,\<Update,\<Reported Errors,\<Print,\<Check Discrepancies"
lcStatus = IIF(EDILIBDT.cStatus<>'U',"T","F")+;
           IIF(EDILIBDT.cStatus='A',"T","F")+;
           IIF(INLIST(EDILIBDT.cStatus,'R','E'),"T","F")+;
           IIF(INLIST(EDILIBDT.cStatus,'A','U','E'),"T","F")+;
           IIF(INLIST(EDILIBDT.cStatus,'A','U'),"T","F")
*C201844,1 RAS 2016-07-13 add trigger to IKEddi to check Discripencoes after updating 820 [end]

RETURN .T.

*:**************************************************************************
*:* Name        : ikAdStWh
*:* Developer   : RAS
*:* Date        : 02/28/2016
*:* Purpose     : Add store which came at 852 if it not exist
*:**************************************************************************
FUNCTION lfikAdStWh

lnCurAls = SELECT()
IF !EMPTY(IKEStore) AND !SEEK('S'+MACCOUNT+PADR(IKEStore,8),'Customer')AND SEEK('M'+MACCOUNT,'Customer')
  SELECT Customer
  SCATTER MEMO MEMVAR 
  m.TYPE ='S'
  m.Store = IKEStore
  m.StName = IKEStore
  APPEND BLANK
  GATHER MEMO MEMVAR 
  
  IF !SEEK(PADR(IKEStore,6),'WAREHOUS','WAREHOUS')
    SELECT WAREHOUS 
    APPEND BLANK 
    REPLACE CWARECODE  WITH IKEStore,;
           cDesc WITH IKEStore
  ENDIF   
 

  =TABLEUPDATE(0,.T.,'Customer')

     
ENDIF

SELECT(lnCurAls)
RETURN .T.  





*:**************************************************************************
*:* Name        : DOSEL
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 01/19/2016
*:* Purpose     : Do Selection function in edipd
*:**************************************************************************
FUNCTION lfikdosel 

DO CASE
  CASE lnChoice = 1
    RETURN loFormObj.Do(.F.,EDILIBDT.cFileCode,"cPartCode+cIntChgSeq+cGroupSeq+cTranSeq='"+EDILIBDT.cPartCode+EDILIBDT.cIntChgSeq+EDILIBDT.cGroupSeq+EDILIBDT.cTranSeq+"'")
  CASE lnChoice = 2
    loFormObj.Update()
  CASE lnChoice = 3
    lcErorText = EDILIBDT.MSTATUS
    lcPrdBgn=SUBSTR(EDILIBDT.cEdiRef,1,AT('|',EDILIBDT.cEdiRef,1)-1)
    lcPrdEnd=SUBSTR(EDILIBDT.cEdiRef,AT('|',EDILIBDT.cEdiRef,1)+1,AT('|',EDILIBDT.cEdiRef,2)-AT('|',EDILIBDT.cEdiRef,1)-1)
    oDisplayText = CREATEOBJECT('DisplayText',lcErorText,'Errors reported while process product activity',;
                  'Received from '+ IIF(SEEK(EDILIBDT.cPartCode,'EDIPH'),ALLTRIM(EDIPH.cPartName),'')+;
                  ' within period '+DTOC(CENDATE(lcPrdBgn))+'-'+DTOC(CENDATE(lcPrdEnd))+'.')
    oDisplayText = .NULL.
  CASE lnChoice = 4
    lcPrdBgn=SUBSTR(EDILIBDT.cEdiRef,1,AT('|',EDILIBDT.cEdiRef,1)-1)
    lcPrdEnd=SUBSTR(EDILIBDT.cEdiRef,AT('|',EDILIBDT.cEdiRef,1)+1,AT('|',EDILIBDT.cEdiRef,2)-AT('|',EDILIBDT.cEdiRef,1)-1)
    IF SEEK(EDILIBDT.CPARTCODE,'EDIACPRT','PARTNER') AND !EMPTY(lcPrdBgn) AND !EMPTY(lcPrdEnd)
      DO FORM (oAriaApplication.ScreenHome+ 'EB\EBSLSINV.SCX') WITH LEFT(EDIACPRT.CPARTNER,5),CENDATE(lcPrdBgn),CENDATE(lcPrdEnd)
    ENDIF  
ENDCASE

RETURN .T.





*:**************************************************************************
*:* Name        : DOSELEC
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 01/20/2016
*:* Purpose     : Do Selection function in edira
*:**************************************************************************
FUNCTION lfikdoselec 

DO CASE
  CASE lnChoice = 1
    RETURN loFormObjj.Do(.F.,EDILIBDT.cFileCode,"cPartCode+cEdiTrnTyp+cIntChgSeq+cGroupSeq+cTranSeq='"+;
                       EDILIBDT.cPartCode+EDILIBDT.cEdiTrnTyp+EDILIBDT.cIntChgSeq+EDILIBDT.cGroupSeq+EDILIBDT.cTranSeq+"'")
  CASE lnChoice = 2
  loFormObjj.Update()
  
  CASE lnChoice = 3
    lcErorText = EDILIBDT.MSTATUS
    oDisplayText = CREATEOBJECT('DisplayText',lcErorText,'Errors reported while process Order Payment/Remittance Advice',;
                  'Received from '+ IIF(SEEK(EDILIBDT.cPartCode,'EDIPH'),ALLTRIM(EDIPH.cPartName),'')+'.')
    oDisplayText = .NULL.
  CASE lnChoice = 4
    IF SEEK(EDILIBDT.CPARTCODE,'EDIACPRT','PARTNER')
      DO FORM (oAriaApplication.ScreenHome+ 'EB\ebPrtPay.SCX') WITH LEFT(EDIACPRT.CPARTNER,5),ALLTRIM(EDILIBDT.cEdiRef)
    ENDIF  
 
*C201844,1 RAS 2016-07-13 add trigger to IKEddi to check Discripencoes after updating 820 [begin]
 CASE lnChoice = 5
  loFormObjj.chckdisc()  
*C201844,1 RAS 2016-07-13 add trigger to IKEddi to check Discripencoes after updating 820 [end]
  
ENDCASE

RETURN .T.








*:**************************************************************************
*:* Name        : IKECHK
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 01/19/2016
*:* Purpose     : Chek if client is IKEE
*:**************************************************************************
FUNCTION lfikechk 
RETURN .T.


*C201844,1 RAS 2016-07-13 add trigger to IKEddi to check Discripencoes after updating 820 [begin]
*:**************************************************************************
*:* Name        : IKCKDSC
*:* Developer   : RAS - Rasha Adel
*:* Date        : 07/13/2016
*:* Purpose     : add option to client to Check Discrepancies
*:**************************************************************************
FUNCTION lfikckdsc
lcFileName = ALLTRIM(oAriaApplication.ActiveCompanyID)+ALLTRIM(EDILIBHD.CFILECODE)+".XLS"
lcEDIARCHIVEFolder =  ADDBS(STRTRAN(UPPER(oAriaApplication.clientapplicationhome),"\PRGS",""))+"EDI\INBOX\ARCHIVE\"
lcfullname=lcEDIARCHIVEFolder+lcFileName

set step on
IF EDILIBDT.cStatus='U'
 IF FILE(lcfullname)
   DECLARE INTEGER ShellExecute IN shell32.DLL ;
     INTEGER hndWin, STRING cAction,  STRING cFileName, STRING cParams, STRING cDir, INTEGER nShowWin
   ShellExecute(0 ,'open' ,lcfullname,"","",1)
 ELSE
   MESSAGEBOX("Couldn't find the Discrepancies report",64,'Missing Report')
 ENDIF
ELSE
  IF SEEK(EDILIBDT.cPartCode,'EdiAcPrt','PARTNER')
    lcSetClass = SET('CLASSLIB')
    lcProc = SET('PROCEDURE')
    lcDatasession = SET('Datasession') 
    lcorgAppPth=oariaapplication.applicationhome 
    lcorgDatadir=oariaapplication.datadir 
    lcorgSrnPth=oariaapplication.Screenhome 
    lcOldFullPath = FULLPATH('')
    lcOrgClientPrgPth=oariaapplication.clientapplicationhome
    ikegetdisc(ALLTRIM(EdiAcPrt.CPARTNER) , EDILIBHD.CFILECODE)
    SET DEFAULT TO (lcOldFullPath)
    SET CLASSLIB TO &lcSetClass. 
    SET PROCEDURE TO &lcProc.   
    SET DATASESSION TO (lcDatasession)    
    oariaapplication.applicationhome =lcorgAppPth
    oariaapplication.datadir =lcorgDatadir
    oariaapplication.Screenhome =lcorgSrnPth
    oariaapplication.clientapplicationhome=lcOrgClientPrgPth
  ENDIF
ENDIF 

RETURN .T.

*C201844,1 RAS 2016-07-13 add trigger to IKEddi to check Discripencoes after updating 820 [end]


*:**************************************************************************
*:* Name        : UPDTACTI  
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 01/19/2016
*:* Purpose     : Update 852 for Ikee
*:**************************************************************************
FUNCTION lfikupdtacti 
 
lcSetClass = SET('CLASSLIB')
lcProc = SET('PROCEDURE')
lcDatasession = SET('Datasession') 
lcorgAppPth=oariaapplication.applicationhome 
lcorgDatadir=oariaapplication.datadir 
lcorgSrnPth=oariaapplication.Screenhome 
lcOldFullPath = FULLPATH('')
lcOrgClientPrgPth=oariaapplication.clientapplicationhome
   
IF SEEK(EDILIBDT.cPartCode,'EdiAcPrt','PARTNER')
  IF IKESMARTINVOICES(ALLTRIM(EdiAcPrt.CPARTNER) , EDILIBHD.CFILECODE)
    SET DEFAULT TO (lcOldFullPath)
    SET CLASSLIB TO &lcSetClass. 
    SET PROCEDURE TO &lcProc.   
    SET DATASESSION TO (lcDatasession)    
    oariaapplication.applicationhome =lcorgAppPth
    oariaapplication.datadir =lcorgDatadir
    oariaapplication.Screenhome =lcorgSrnPth
    oariaapplication.clientapplicationhome=lcOrgClientPrgPth
    RETURN .T.
  ELSE
    SET DEFAULT TO (lcOldFullPath)
    SET CLASSLIB TO &lcSetClass.  
    SET PROCEDURE TO &lcProc.  
    SET DATASESSION TO (lcDatasession)    
    oariaapplication.applicationhome =lcorgAppPth
    oariaapplication.datadir =lcorgDatadir
    oariaapplication.Screenhome =lcorgSrnPth
    oariaapplication.clientapplicationhome=lcOrgClientPrgPth
    MESSAGEBOX("Failed to Generate AR SMART Invoice", 16 ,'SMART INVOICE')
  RETURN .F.
  ENDIF
ELSE
 MESSAGEBOX("Failed to Generate AR SMART Invoice", 16 ,'SMART INVOICE')
 RETURN .F.
ENDIF






*:**************************************************************************
*:* Name        : UPDTRA
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 01/20/2016
*:* Purpose     : Update 820 for Ikee
*:**************************************************************************
FUNCTION lfikupdtra    
lcSetClass = SET('CLASSLIB')
lcProc = SET('PROCEDURE')
lcDatasession = SET('Datasession') 
lcorgAppPth=oariaapplication.applicationhome 
lcorgDatadir=oariaapplication.datadir 
lcorgSrnPth=oariaapplication.Screenhome 
lcOldFullPath = FULLPATH('')
lcOrgClientPrgPth=oariaapplication.clientapplicationhome
IF SEEK(EDILIBDT.cPartCode,'EdiAcPrt','PARTNER')
  IF IKECREATEPAYMENT(ALLTRIM(EdiAcPrt.CPARTNER) , EDILIBHD.CFILECODE)
    SET DEFAULT TO (lcOldFullPath)
    SET CLASSLIB TO &lcSetClass. 
    SET PROCEDURE TO &lcProc.   
    SET DATASESSION TO (lcDatasession)    
    oariaapplication.applicationhome =lcorgAppPth
    oariaapplication.datadir =lcorgDatadir
    oariaapplication.Screenhome =lcorgSrnPth
    oariaapplication.clientapplicationhome=lcOrgClientPrgPth
    RETURN .T.
  ELSE   
    SET DEFAULT TO (lcOldFullPath)     
    SET CLASSLIB TO &lcSetClass. 
    SET PROCEDURE TO &lcProc.       
    SET DATASESSION TO (lcDatasession)     
    MESSAGEBOX("Failed to update remittance advice# "+ALLTRIM(EDILIBDT.cediref), 16 ,'SMART INVOICE')    
    oariaapplication.applicationhome =lcorgAppPth    
    oariaapplication.datadir =lcorgDatadir     
    oariaapplication.Screenhome =lcorgSrnPth     
    oariaapplication.clientapplicationhome=lcOrgClientPrgPth   
    RETURN .F.
  ENDIF
ELSE
 MESSAGEBOX("Failed to update remittance advice# "+ALLTRIM(EDILIBDT.cediref), 16 ,'SMART INVOICE')
 RETURN .F.
ENDIF


