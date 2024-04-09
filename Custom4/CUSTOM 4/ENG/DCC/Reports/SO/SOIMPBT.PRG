*- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001]

*- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientId

IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
 
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
 
  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
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
  lcActiveMod = 'SO'
  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'SO'
  oAriaEnvironment.RequestID = lcRequestID
  PUBLIC gcAct_Appl
  gcAct_Appl = lcActiveMod
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  oAriaEnvironment.Report.cCROrientation = 'P' 
ELSE
  loOGScroll.cCROrientation = 'P'
ENDIF
*- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]


= gfopentable('contact', 'email', 'SH')
SELECT contact
SET ORDER TO email
= gfopentable('ordhdr')
SELECT ordhdr
SET ORDER TO ordhdr

lcTmpOut = gftempname()
= lfbuildtmp()
lcrppath=oAriaApplication.DefaultPath+'BT\Import\'
lcrphist=oAriaApplication.DefaultPath+'BT\History\'
STORE 0 TO lnip
DIMENSION laprocessed[1]
lcskeleton = lcrppath+'??????????.xml' && 10 characters
lnfnumber = ADIR(lafiles, lcskeleton)
IF lnfnumber=0
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
  *= gfmodalgen('INM00000B00000', .F., .F., .F., "No Files to Process")
  IF TYPE('lcXMLFileName') <> 'C'
    = gfmodalgen('INM00000B00000', .F., .F., .F., "No Files to Process")
  ENDIF
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
  RETURN
ENDIF
ASORT(lafiles)
ore = CREATEOBJECT("VBScript.RegExp")
FOR ncount = 1 TO lnfnumber
  ore.PATTERN = "[0-9]{10}\.xml$" && 10 digits
  IF ore.test(LOWER(lafiles(ncount, 1)))
    lcfile = lcrppath+lafiles(ncount, 1)
    lcXML=FILETOSTR(lcfile)
    lnTotLines=ALINES(lcArray, lcXML)
    STORE SPACE(0) TO lcPOID, lcEmail, lcName, lcAddr, lcAddr1, lcAddr2, lcAddr3, lcitemx, lcPOord
    FOR lnI=1 TO lnTotLines
      IF ATC('POID>', lcArray(lnI))>0
        lnstart=ATC('>', lcArray(lnI))+1
        lnEnd=ATC('</POID>', lcArray(lnI))
        IF lnEnd>0
          lcPOID=ALLTRIM(SUBSTR(lcArray(lnI), lnstart, lnEnd-lnstart))
          *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
          *WAIT WINDOW NOWAIT lcPOID
          IF TYPE('lcXMLFileName') <> 'C'
            WAIT WINDOW NOWAIT lcPOID
          ENDIF
          *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
        ENDIF
        LOOP
      ENDIF
      IF UPPER(LEFT(ALLTRIM(lcArray(lnI)),10))=='<USERAREA>'
        IF ATC('<CONFIGID>', lcArray(lnI))>0
          lcLine=STRTRAN(ALLTRIM(lcArray(lnI)),'><','>'+CHR(13)+'<')
          lnUALines=ALINES(lcUAArray, lcLine)
          FOR lnJ=1 TO lnUALines
            IF ATC('<CONFIGID>', lcUAArray(lnJ))>0
              lnstart=ATC('>', lcUAArray(lnJ))+1
              lnEnd=ATC('</CONFIGID>', lcUAArray(lnJ))
              IF lnEnd>0
                lcPOord=ALLTRIM(SUBSTR(lcUAArray(lnJ), lnstart, lnEnd-lnstart))
                *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
                *WAIT WINDOW NOWAIT lcPOord
                IF TYPE('lcXMLFileName') <> 'C'
                  WAIT WINDOW NOWAIT lcPOord
                ENDIF
                *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
              ENDIF
              EXIT
            ENDIF
          ENDFOR
        ENDIF
      ENDIF
    ENDFOR



    SELECT (lcTmpOut)
    APPEND BLANK
    REPL FILE WITH JUSTFNAME(lcfile), POID WITH lcPOID, POord WITH lcPOord IN (lcTmpOut)
    DO CASE
      CASE EMPTY(lcPOID)
        REPL PROCESS WITH 'POID not in file', passed WITH .F. IN (lcTmpOut)
      CASE EMPTY(lcPOord)
        REPL PROCESS WITH 'No Order Number in file', passed WITH .F. IN (lcTmpOut)
      OTHERWISE
        IF !gfSeek('T'+lcPOord, 'ordhdr')
          REPL PROCESS WITH 'Order not found', passed WITH .F. IN (lcTmpOut)
        ELSE
          REPL passed WITH .T. IN (lcTmpOut)
        ENDIF
    ENDCASE
    lnip = lnip+1
    DIMENSION laprocessed[lnip]
    laprocessed[lnip] = lcfile

  ENDIF
ENDFOR

STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl, lnscalngl, lnscaposgl
= lfchkstrct()

= gfopentable('ordline')
= gfopentable('stydye')
= gfopentable('style')
= gfopentable('piktkt')

*- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
IF TYPE('lcXMLFileName') = 'C'
  USE IN SYCCOMP
ENDIF
*- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
= gfopentable('syccomp', 'ccomp_id', 'SH')
SELECT syccomp
SET ORDER TO ccomp_id
=gfSeek(oAriaApplication.activecompanyid,'syccomp')
lcDB=ALLTRIM(syccomp.cConDBName)
lcConnect="Driver={SQL Server};Server="+ALLTRIM(syccomp.cConServer)+";uid="+ALLTRIM(syccomp.cConUserID)+";pwd="+ALLTRIM(syccomp.cConPasWrd)+";database="+lcDB
STORE SQLSTRINGCONNECT(lcConnect) TO lnAria
IF lnAria < 1 && Error
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
  *= gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to open Aria SQL")
  IF TYPE('lcXMLFileName') <> 'C'
    = gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to open Aria SQL")
  ENDIF
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
  RETURN .F.
ENDIF

DIME laError[7]
lcStyHProc='#'+gftempname()
lcCreate='CREATE PROCEDURE '+lcStyHProc+' @account char(5), @store char(8), @employee char(12), @cstymajor char(19), @color char(6), @nused numeric(5,2), @order char(6)'
lcAs1=" AS UPDATE ["+lcDB+"].[dbo].[styHist] SET cedit_time=convert(char(11),getdate(),108), cedit_user='"+ALLTRIM(oAriaApplication.user_id)+"', dedit_date=getdate()"
lcAs1=lcAs1+", nused=nused+@nused, dlastOrd=getdate(), [order]=@order WHERE account=@account AND store=@store AND employee=@employee and cstymajor=@cstymajor and color=@color"
IF !lfSQLExec(lnAria, lcCreate+lcAs1)  
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
  *=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to create styHist proc")
  IF TYPE('lcXMLFileName') <> 'C'
    =gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to create styHist proc")
  ENDIF
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
  RETURN .F.
ENDIF

*!*		lcPikProc='#'+gfTempName()
*!*		lcCreate='CREATE PROCEDURE '+lcPikProc+' @piktkt char(6), @store char(8), @account char(5), @order char(6)'
*!*		lcSQL=" as INSERT INTO ["+lcDB+"].[dbo].[pickpack] ([piktkt] ,[pikdate] ,[cpstatus] ,[order] ,[cadd_user] ,[cadd_time] ,[dadd_date] ,[llok_stat] ,[clok_user] ,[dlok_date] ,[clok_time] ,[cedit_user]"
*!*		lcSQL=lcSQL+" ,[cadd_ver] ,[dedit_date] ,[cedt_ver] ,[cedit_time] ,[account] ,[store] ,[cpickby] ,[dpickby] ,[cpicktime] ,[lcomppick] ,[chkedby] ,[dchkby] ,[cchkbytime] ,[cdespby] ,[ddespby]"
*!*		lcSQL=lcSQL+" ,[cdesptime] ,[consgment] ,[action] ,[actiondate] ,[actiontime] ,[actionto] ,[acttomail] ,[notes] ,[noofcarton])"
*!*		lcSQL=lcSQL+" VALUES ('@piktkt' ,GETDATE() ,'Not Issued' ,'@Order' ,'"+ALLTRIM(oariaapplication.user_id)+"' ,'' ,GETDATE() ,0 ,'' ,'1901-01-01' ,'' ,'' ,'' ,'1901-01-01' ,'' ,''"
*!*		lcSQL=lcSQL+" ,'@account' ,'@store' ,'' ,'1901-01-01' ,'' ,0 ,'' ,'1901-01-01' ,'' ,'' ,'1901-01-01' ,'' ,'' ,'' ,'1901-01-01' ,'' ,'' ,'' ,'' ,0)"
*!*		IF !lfSQLExec(lnAria, lcCreate+lcSQL)
*!*			=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to create PickPack proc")
*!*			RETURN .F.
*!*		ENDIF

IF SQLSETPROP(lnAria, 'Transactions', 2)<0  && manual
  = SQLDISCONNECT(lnAria)  
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
  *= gfmodalgen('INM00000B00000', .F., .F., .F., 'Failed to set Transactions Aria SQL')
  IF TYPE('lcXMLFileName') <> 'C'
    = gfmodalgen('INM00000B00000', .F., .F., .F., 'Failed to set Transactions Aria SQL')
  ENDIF
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
  STORE 0 TO lnAria
  RETURN .F.
ENDIF

SELECT ordline
SET ORDER TO ordline
SELECT STYLE
SET ORDER TO STYLE
SELECT stydye
SET ORDER TO stydye
SELECT (lcTmpOut)
SCAN FOR passed
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
  *WAIT WINDOW NOWAIT &lcTmpOut..POord
  IF TYPE('lcXMLFileName') <> 'C'
    WAIT WINDOW NOWAIT &lcTmpOut..POord
  ENDIF
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
  IF !gfSeek('T'+&lcTmpOut..POord, 'ordhdr')
    REPL PROCESS WITH 'Unable to find Ordhdr' IN (lcTmpOut)
    LOOP
  ENDIF
  IF !gfSeek('T'+&lcTmpOut..POord, 'Ordline')
    REPL PROCESS WITH 'Unable to find Ordline' IN (lcTmpOut)
    LOOP
  ENDIF
  STORE .T. TO llPicked
  SELECT ordline
  SCAN REST FOR ORDER=&lcTmpOut..POord AND cOrdType='T'
    =gfSeek(ordline.STYLE,'style')
    =gfSeek(ordline.STYLE+'DCC','stydye')
    FOR lnI = 1 TO 8
      lci = STR(lnI, 1)
      lnqty=stydye.stk&lci-stydye.alo&lci
      IF ordline.book&lci>0
        IF lnqty<ordline.book&lci
          STORE .F. TO llPicked
        ENDIF
      ENDIF
    ENDFOR
  ENDSCAN
  lcorder = gfsequence('ORDER')
  IF llPicked
    lcPiktkt = gfsequence('PIKTKT')
  ENDIF
  SELECT ordhdr
  REPL ordhdr.ORDER WITH lcorder, ordhdr.STATUS WITH 'O', ordhdr.cOrdType WITH 'O', ordhdr.custpo WITH &lcTmpOut..POID, ordhdr.cContRef WITH &lcTmpOut..POID, ordhdr.approval WITH &lcTmpOut..POord
  STORE 0 TO lnlineno

  lcSQL="Begin Transaction"
  IF !lfSQLExec(lnAria, lcSQL)    
    *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
    *=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Begin Transaction")
    IF TYPE('lcXMLFileName') <> 'C'
      =gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Begin Transaction")
    ENDIF
    *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
    RETURN .F.
  ENDIF

  SELECT ordline
  *!*			go top
  =gfSeek('T'+&lcTmpOut..POord, 'Ordline')
  SELECT ordline
  SCAN REST FOR ORDER=&lcTmpOut..POord AND cOrdType='T'
    IF llPicked
      REPL ordline.piktkt WITH lcPiktkt, ordline.picked WITH .T., ordline.pikdate WITH DATE()
      =gfSeek(ordline.STYLE,'style')
      =gfSeek(ordline.STYLE+ordline.cWareCode,'stydye')
      FOR lnI = 1 TO 8
        lci = STR(lnI, 1)
        IF ordline.book&lci>0
          REPL stydye.alo&lci WITH stydye.alo&lci+ordline.book&lci, stydye.totalo WITH stydye.totalo+ordline.book&lci
          REPL STYLE.alo&lci WITH STYLE.alo&lci+ordline.book&lci, STYLE.totalo WITH STYLE.totalo+ordline.book&lci
          REPL stydye.ord&lci WITH stydye.ord&lci+ordline.book&lci, stydye.totord WITH stydye.totord+ordline.book&lci
          REPL STYLE.ord&lci WITH STYLE.ord&lci+ordline.book&lci, STYLE.totord WITH STYLE.totord+ordline.book&lci
          REPL ordline.pik&lci WITH ordline.book&lci, ordline.totpik WITH ordline.totpik+ordline.book&lci
        ENDIF
      ENDFOR
      REPL PROCESS WITH 'Order Allocated', orderNo WITH lcorder, poPik WITH lcPiktkt IN (lcTmpOut)
    ELSE
      FOR lnI = 1 TO 8
        lci = STR(lnI, 1)
        IF ordline.book&lci>0
          REPL stydye.ord&lci WITH stydye.ord&lci+ordline.book&lci, stydye.totord WITH stydye.totord+ordline.book&lci
          REPL STYLE.ord&lci WITH STYLE.ord&lci+ordline.book&lci, STYLE.totord WITH STYLE.totord+ordline.book&lci
        ENDIF
      ENDFOR
      REPL PROCESS WITH 'Order Created', orderNo WITH lcorder, poPik WITH '' IN (lcTmpOut)
    ENDIF
    lnlineno=lnlineno+1
    REPL ordline.ORDER WITH lcorder, ordline.cOrdType WITH 'O', ordline.LINENO WITH lnlineno

    lcExec='EXEC '+lcStyHProc+" '"+ordline.account+"', '"+ALLTRIM(ordline.STORE)+"' ,'"+ALLTRIM(ordline.employee)+"' ,'"+ALLTRIM(LEFT(ordline.STYLE,lnstylngl))+"' ,'"+ALLTRIM(SUBSTR(ordline.STYLE,lnclrposgl,lnclrlngl))+"'"
    lcExec=lcExec+","+ALLTRIM(STR(ordline.totbook))+",'"+lcorder+"'"

    IF !lfSQLExec(lnAria, lcExec)      
      *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
      *=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Update StyHist")
      IF TYPE('lcXMLFileName') <> 'C'
        =gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Update StyHist")
      ENDIF
      *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
      RETURN .F.
    ENDIF

  ENDSCAN
  SELECT ordhdr
  REPL ordhdr.lastline WITH lnlineno
  *!*			lsql="update styhist set && dragons
  IF llPicked

    *!*				lcExec='EXEC '+lcPikProc+" '"+lcpiktkt+"', '"+alltrim(ordhdr.store)+"' ,'"+ordhdr.account+"' ,'"+lcOrder+"'"
    lcSQL="INSERT INTO ["+lcDB+"].[dbo].[pickpack] ([piktkt] ,[pikdate] ,[cpstatus] ,[order] ,[cadd_user] ,[cadd_time] ,[dadd_date] ,[llok_stat] ,[clok_user] ,[dlok_date] ,[clok_time] ,[cedit_user]"
    lcSQL=lcSQL+" ,[cadd_ver] ,[dedit_date] ,[cedt_ver] ,[cedit_time] ,[account] ,[store] ,[cpickby] ,[dpickby] ,[cpicktime] ,[lcomppick] ,[chkedby] ,[dchkby] ,[cchkbytime] ,[cdespby] ,[ddespby]"
    lcSQL=lcSQL+" ,[cdesptime] ,[consgment] ,[action] ,[actiondate] ,[actiontime] ,[actionto] ,[acttomail] ,[notes] ,[noofcarton])"
    lcSQL=lcSQL+" VALUES ('"+lcPiktkt+"' ,GETDATE() ,'Not Issued' ,'"+lcorder+"' ,'"+ALLTRIM(oAriaApplication.user_id)+"' ,'' ,GETDATE() ,0 ,'' ,'1901-01-01' ,'' ,'' ,'' ,'1901-01-01' ,'' ,''"
    lcSQL=lcSQL+" ,'"+ordhdr.account+"' ,'"+ordhdr.STORE+"' ,'' ,'1901-01-01' ,'' ,0 ,'' ,'1901-01-01' ,'' ,'' ,'1901-01-01' ,'' ,'' ,'' ,'1901-01-01' ,'' ,'' ,'' ,'' ,0)"
    IF !lfSQLExec(lnAria, lcSQL)      
      *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
      *=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Insert PickPack")
      IF TYPE('lcXMLFileName') <> 'C'
        =gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Insert PickPack")
      ENDIF
      *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
      RETURN .F.
    ENDIF

    SELECT ordhdr
    SCATTER MEMO MEMVAR
    m.piktkt = lcPiktkt
    m.date = DATE()
    SELECT piktkt
    gfappend('PIKTKT', .T.)
    = gfadd_info('PIKTKT')
  ENDIF

  lcSQL="Commit Transaction"
  IF !lfSQLExec(lnAria, lcSQL)    
    *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
    *=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Commit Transaction")
    IF TYPE('lcXMLFileName') <> 'C'
      =gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Commit Transaction")
    ENDIF
    *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
    RETURN .F.
  ENDIF

  SELECT ordhdr
  gftableupdate()
  SELECT ordline
  gftableupdate()
  SELECT STYLE
  gftableupdate()
  SELECT stydye
  gftableupdate()
  SELECT piktkt
  gftableupdate()
ENDSCAN

=SQLDISCONNECT(lnAria)
STORE 0 TO lnAria

SELECT (lcTmpOut)
*- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
*DO gfdispre WITH EVALUATE('lcFormName')
IF TYPE('lcXMLFileName') <> 'C'
  DO gfdispre WITH EVALUATE('lcFormName')
ELSE
  oAriaEnvironment.Report.cCROrientation = 'P'
  oAriaEnvironment.REPORT.OGLastForm = 'SOIMPBT'
  loProgress.DESCRIPTION = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
ENDIF
*- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
COPY TO (oAriaApplication.DefaultPath+'BT\History\BT_'+TTOC(DATETIME(),1)+'.csv') DELIMITED

IF VARTYPE(laprocessed[1])<>'L'
  lnfnumber = ALEN(laprocessed, 1)
  FOR ncount = 1 TO lnfnumber
    IF  .NOT. EMPTY(laprocessed(ncount))
      lcName = ADDBS(lcrphist)+JUSTFNAME(laprocessed(ncount))+'x'
      lcfile = laprocessed(ncount)
      COPY FILE (laprocessed(ncount)) TO (lcName)
      ERASE (laprocessed(ncount))
    ENDIF
  ENDFOR
ENDIF

****
****
PROCEDURE lfbuildtmp
  DIMENSION latempstru[7, 18]
  STORE '' TO latempstru
  STORE 0 TO lnindex
  lnindex = lnindex+1
  latempstru[lnindex, 1] = 'File'
  latempstru[lnindex, 2] = 'C'
  latempstru[lnindex, 3] = 50
  latempstru[lnindex, 4] = 0
  lnindex = lnindex+1
  latempstru[lnindex, 1] = 'POID'
  latempstru[lnindex, 2] = 'C'
  latempstru[lnindex, 3] = 16
  latempstru[lnindex, 4] = 0
  lnindex = lnindex+1
  latempstru[lnindex, 1] = 'orderNo'
  latempstru[lnindex, 2] = 'C'
  latempstru[lnindex, 3] = 6
  latempstru[lnindex, 4] = 0
  lnindex = lnindex+1
  latempstru[lnindex, 1] = 'POord'
  latempstru[lnindex, 2] = 'C'
  latempstru[lnindex, 3] = 6
  latempstru[lnindex, 4] = 0
  lnindex = lnindex+1
  latempstru[lnindex, 1] = 'POpik'
  latempstru[lnindex, 2] = 'C'
  latempstru[lnindex, 3] = 6
  latempstru[lnindex, 4] = 0
  lnindex = lnindex+1
  latempstru[lnindex, 1] = 'passed'
  latempstru[lnindex, 2] = 'L'
  latempstru[lnindex, 3] = 1
  latempstru[lnindex, 4] = 0
  lnindex = lnindex+1
  latempstru[lnindex, 1] = 'Process'
  latempstru[lnindex, 2] = 'C'
  latempstru[lnindex, 3] = 100
  latempstru[lnindex, 4] = 0
  * --------------------------------------------------------
  = gfcrttmp(lcTmpOut, @latempstru, , "", .F.)
  * --------------------------------------------------------
ENDPROC
**
FUNCTION lfwRepWhen
ENDFUNC
**
FUNCTION lfgetCrt
ENDFUNC
**
FUNCTION lfSQLExec
  PARAMETER lnDB, lcCode
  lnRetVal = 0
  DO WHILE .T.
    lnRetVal = SQLEXEC(lnDB, lcCode)
    IF lnRetVal<>0
      EXIT
    ENDIF
    LOOP
  ENDDO
  IF lnRetVal < 0 && Error
    = AERROR(laError)
    SQLROLLBACK(lnAria)
    SQLDISCONNECT(lnAria)
    STORE 0 TO lnAria
    SELECT (lcTmpOut)
    COPY TO (oAriaApplication.DefaultPath+'BT\History\BT_'+TTOC(DATETIME(),1)+'.csv') DELIMITED
    lnFileHandle=FCREATE(ADDBS(oAriaApplication.workdir)+'soimpbt.sql')
    FPUTS(lnFileHandle, lcCode)
    FOR N = 1 TO ALEN(laError,1)  && Display all elements of the array
      IF VARTYPE(laError(N))$'N,C'
        FPUTS(lnFileHandle, IIF(VARTYPE(laError(N))='N',STR(laError(N)),laError(N)))
      ENDIF
    ENDFOR
    FCLOSE(lnFileHandle)
    RETURN .F.
  ENDIF
  RETURN .T.
ENDFUNC
****
PROCEDURE lfchkstrct
  DIMENSION laitemseg[1]
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][Start]
  *= gfitemmask(@laitemseg)
  IF TYPE('lcXMLFileName') == 'C'
    LOCAL loItemMask
    loItemMask = CREATEOBJECT("GetItemMask")
    loItemMask.Do(@laItemSeg)
  ELSE
    = gfitemmask(@laitemseg)
  ENDIF
  *- C201600,1 SAB 12/19/2013 Add custom Import BT Purchase Orders report to RB [T20131125.0001][End]
  FOR lncount = 1 TO ALEN(laitemseg, 1)
    DO CASE
      CASE laitemseg(lncount, 1)='C'
        lnclrlngl = LEN(laitemseg(lncount, 3))
        lnclrposgl = laitemseg(lncount, 4)
      CASE laitemseg(lncount, 1)='F'
        lnstylngl = LEN(laitemseg(lncount, 3))
        lnstyposgl = laitemseg(lncount, 4)
      CASE laitemseg(lncount, 1)='S'
        lnscalngl = LEN(laitemseg(lncount, 3))
        lnscaposgl = laitemseg(lncount, 4)
    ENDCASE
  ENDFOR
ENDPROC
*****
