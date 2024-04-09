*:***************************************************************************
*: Program file  : APPYINVD
*: Program desc. : Invoice Detail Report
*: System        : ARIA 4XP
*: Module        : Accounts Payable(AP)
*: Developer     : AHMED MOUSTAFA (AHS)
*: Date          : 08/09/2009                                                                                                                                                 
*:***************************************************************************
PARAMETERS lcRequestID, lcXMLFileName

PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.Description = "Opening Data Files..."
loAgent.UpdateObjectProgress(lcRequestID, loProgress)

SET STEP ON 
LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)

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

*------

* HES
*!*	STORE '' TO lcTempFile
*!*	lcTempFile = oAriaEnvironment.Cursors.GetCursorTempName()
* HES

IF !USED('APINVHDR') 
  *=gfOpenTABLE('APINVHDR','VENDINV','SH')
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APINVHDR','VENDINV','SH',.F.,.F.)
  SELECT APINVHDR
  *=gfsetorder('VENDINV')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.Setorderto('VENDINV')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF

IF !USED('APDIST') 
  *=gfOpenTABLE('APDIST','INVVEND','SH')
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APDIST','INVVEND','SH',.F.,.F.)
  SELECT APDIST
  *=gfsetorder('INVVEND')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.Setorderto('INVVEND')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF

IF !USED('GLACCHAR') 
  *=gfOpenTABLE(oAriaApplication.DATADIR+'GLACCHAR',oAriaApplication.DATADIR+'CACCTCODE','SH')
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'GLACCHAR','ACCTCODE','SH',.F.,.F.)
  SELECT GLACCHAR
  *=gfsetorder('CACCTCODE')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.Setorderto('ACCTCODE')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
ENDIF

IF !USED('APSETUP') 
  *=gfOpenTABLE(oAriaApplication.DATADIR+'APSETUP','','SH')
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APSETUP','','SH',.F.,.F.)
  *SELECT GLACCHAR
  *SELECT APSETUP
  *=gfseek('')
ENDIF

=lfvCrTemp()     && To create Temp File
=lfvColData()    && Data Collection

*DO gfDispRe WITH EVAL('lcRpForm')
oAriaEnvironment.report.print(lcRpForm)

*!**************************************************************************
*! Function      : lfvColData
*! Purpose       : Collection of data 
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfvColData

lnVendPos = lfGetPos('APDIST.CVENDCODE','laOgVrFlt')
lnCurrPos = lfGetPos('APINVHDR.CCURRCODE','laOgFxFlt')
lnMethPos = lfGetPos('APINVHDR.CVENPMETH','laOgVrFlt')
lnDivPos = lfGetPos('APINVHDR.CDIVISION','laOgVrFlt')
*lnPriorPos = lfGetPos('APINVHDR.CVENPRIOR','laOgVrFlt')
lcCurr = laOGfxFlt[lnCurrPos,6]
lcVend = laOGvrFlt[lnVendPos,6]
lcMeth = laOGvrFlt[lnMethPos,6]
lcDiv = laOGvrFlt[lnDivPos,6]
*lcPrior = loOGScroll.laOGvrFlt[lnPriorPos,6]
*lcPriorFrom = SUBSTR(lcPrior,1,1)
*lnTo = AT('|',lcPrior)+1
*lcPriorTo =SUBSTR(lcPrior,lnTo,1)

* HES
*-- Period Range
lnPrdPos = lfGetPos("apdist.cfisfyear +'-'+ apdist.cfspprdid",'laOgVrFlt')
lcPrd = laOGvrFlt[lnPrdPos,6]
STORE 0 TO lnCnt,lnFromYear,lnFromPrd,lnToYear,lnToPrd 

IF !EMPTY(lcPrd)
  SELECT(lcPrd)
  COUNT TO lnCnt FOR !DELETED()
ENDIF 

IF lnCnt > 0
  SELECT(lcPrd)
  GO TOP 
  lnFromYear = VAL(SUBSTR(KEYEXP,1,4))
  lnFromPrd = VAL(SUBSTR(KEYEXP,6,2))
  ldFrom = DATE(lnFromYear,lnFromPrd,1)

  GO BOTTOM
  lnToYear = VAL(SUBSTR(KEYEXP,1,4))
  lnToPrd = VAL(SUBSTR(KEYEXP,6,2)) 
  ldTo = DATE(lnToYear,lnToPrd,1)
ENDIF 
* HES

llVenSlct = .F.
IF !EMPTY(lcVend) AND USED(lcVend)
  SELECT &lcVend
  LOCATE 
  llVenSlct = !EOF()
ENDIF

IF llVenSlct
  SCAN
   
*   gfseek(&lcVend..cVendCode,'APINVHDR')
   oAriaEnvironment.remotetableaccess.SeekRecord(&lcVend..cVendCode,'APINVHDR')
   =lfInsLine()
  ENDSCAN  
ELSE 
  oAriaEnvironment.remotetableaccess.SeekRecord('','APINVHDR')
*  gfseek('','APINVHDR')
  =lfInsLine()  
ENDIF 

*!**************************************************************************
*! Function      : lfInsLine
*! Purpose       : Inserting lines in Temp File
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfInsLine
SELECT APINVHDR
lnVenCnt = RECCOUNT() 
  SCAN 
  
  * HES
*!*	    IF IIF(!EMPTY(laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ laOGfxFlt[lnCurrPos,6] , .T. )AND;
*!*	       IIF(!EMPTY(laOGvrFlt[lnMethPos,6]),APINVHDR.CVENPMETH $ laOGvrFlt[lnMethPos,6],.T.)AND;
*!*	       IIF(!EMPTY(laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ laOGvrFlt[lnDivPos,6],.T.)AND;
*!*	       IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT <> 'V')AND;
*!*	       IIF(lcRpStatus='A',.T.,IIF(lcRpStatus='O',APINVHDR.NINVPAID < APINVHDR.NINVAMNT,APINVHDR.NINVPAID > APINVHDR.NINVAMNT)AND;
*!*	       IIF(!EMPTY(lcRpRefnce),APINVHDR.CINVREF,.T.)AND;
*!*	       IIF(!EMPTY(lcRpSess),lfFltSess(),.T.))

    IF IIF(!EMPTY(laOGfxFlt[lnCurrPos,6]),APINVHDR.CCURRCODE $ laOGfxFlt[lnCurrPos,6] , .T. )AND;
       IIF(!EMPTY(laOGvrFlt[lnMethPos,6]),IIF(laOGvrFlt[lnMethPos,6] ='A',.T.,APINVHDR.CVENPMETH $ laOGvrFlt[lnMethPos,6]),.T.)AND;
       IIF(!EMPTY(laOGvrFlt[lnDivPos,6]),APINVHDR.CDIVISION $ laOGvrFlt[lnDivPos,6],.T.)AND;
       IIF(llRpIncInv,.T.,APINVHDR.CINVSTAT <> 'V')AND;
       IIF(lcRpStatus='A',.T.,IIF(lcRpStatus='O',APINVHDR.NINVPAID < APINVHDR.NINVAMNT,APINVHDR.NINVPAID > APINVHDR.NINVAMNT)AND;
       IIF(!EMPTY(lcRpRefnce),lcRpRefnce $ APINVHDR.CINVREF,.T.)AND;
       IIF(!EMPTY(lcRpSess),lfFltSess(),.T.))AND;
       IIF(!EMPTY(lnFromYear) AND !EMPTY(lnFromPrd),BETWEEN(DATE(VAL(apinvhdr.cfisfyear),VAL(apinvhdr.cfspprdid),1),ldFrom,ldTo),.T.)
   * HES
       
    lnPerCent = RECNO()/lnVenCnt
    loProgress.Percent = lnPerCent * 0.9
    loProgress.Description = "Collecting Data For Vendor:"+APINVHDR.CVENDCODE
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
       
    SCATTER MEMVAR memo
     
*    gfseek(APINVHDR.CINVNO+APDIST.CVENDCODE,'APDIST')
    oAriaEnvironment.remotetableaccess.SeekRecord(APINVHDR.CINVNO+APINVHDR.CVENDCODE,'APDIST')
    SELECT APDIST
      SCAN 
        M.CAPDSTAT = APDIST.CAPDSTAT
        M.CAPDACTID = APDIST.CAPDACTID
        M.CAPDGLACT = APDIST.CAPDGLACT
        *M.CACCNLDES = APDIST.CACCNLDES
        *M.CAPACCT = APINVHDR.CAPACCT
        
        SELECT GLACCHAR
*        gfseek(APDIST.CAPDGLACT,'GLACCHAR')
        oAriaEnvironment.remotetableaccess.SeekRecord(APDIST.CAPDGLACT,'GLACCHAR')
        
        SELECT &lcTempFile
        M.CVENPMETH = SUBSTR(lcRpVldEnt,ATC('~',lcRpVldEnt,ATC(LEFT(m.cvenpmeth,1),lcRpVldVal))+1,(ATC('~',lcRpVldEnt,ATC(LEFT(m.cvenpmeth,1),lcRpVldVal)+1)-1)-(ATC('~',lcRpVldEnt,ATC(LEFT(m.cvenpmeth,1),lcRpVldVal))))         
        M.INVAMNT1 = gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
        M.PAIDAMNT1 = IIF(APINVHDR.CINVSTAT='V',0,gfAmntDisp(APINVHDR.NINVPAID,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
        M.DIS_TAK1 = gfAmntDisp(APINVHDR.NINVDISTK,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
        M.ADJ_AMNT1 = gfAmntDisp(APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
        M.OPEN_BAL1 = gfAmntDisp(APINVHDR.NINVAmnt-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
        *M.CACCNLDES = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',ALLT(APDIST.CAPDGLACT)+' '+GLACCHAR.CACCNLDES,'')
        M.NAPDAMNT  = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',gfAmntDisp(APDIST.NAPDAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),0)                
        *M.CAPDGLACT = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',ALLT(APDIST.CAPDGLACT)+' '+GLACCHAR.CACCNLDES,'')
        M.NAPDLINNO = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',APDIST.NAPDLINNO,0)
        M.CGLACCNT = IIF(APDIST.CAPDSTAT<>'V' AND APDIST.CAPDACTID='D',ALLT(APDIST.CAPDGLACT)+' '+GLACCHAR.CACCNLDES,'')
        INSERT INTO &lcTempFile FROM MEMVAR 
      ENDSCAN 
    
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
*! Date          : 08/09/2009
*!**************************************************************************
FUNCTION lfvCrTemp


* HES
IF USED(lcTempFile) AND RECCOUNT(lcTempFile) > 0
  SELECT (lcTempFile)
  ZAP 
ENDIF
*-- Create File
IF !USED(lcTempFile)
* HES

DIMENSION laTempStru[23,4]

  laTempstru[1,1]='CVENDCODE'
  laTempstru[1,2]='C'
  laTempstru[1,3]= 8
  laTempstru[1,4]= 0
   
  laTempstru[2,1]='COUTCOMP'
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
        
  laTempstru[7,1]='CVENPRIOR'
  laTempstru[7,2]='C'
  laTempstru[7,3]= 1
  laTempstru[7,4]= 0
   
  laTempstru[8,1]='CVENPMETH'
  laTempstru[8,2]='C'
  laTempstru[8,3]= 30
  laTempstru[8,4]= 0
   
  laTempstru[9,1]='INVAMNT1'
  laTempstru[9,2]='N'
  laTempstru[9,3]= 8
  laTempstru[9,4]= 0
  
  laTempstru[10,1]='PAIDAMNT1'
  laTempstru[10,2]='N'
  laTempstru[10,3]= 8
  laTempstru[10,4]= 0
  
  laTempstru[11,1]='DIS_TAK1'
  laTempstru[11,2]='N'
  laTempstru[11,3]= 8
  laTempstru[11,4]= 0
  
  laTempstru[12,1]='ADJ_AMNT1'
  laTempstru[12,2]='N'
  laTempstru[12,3]= 8
  laTempstru[12,4]= 0
  
  laTempstru[13,1]='OPEN_BAL1'
  laTempstru[13,2]='N'
  laTempstru[13,3]= 8
  laTempstru[13,4]= 0
  
  laTempstru[14,1]='CCURRCODE'
  laTempstru[14,2]='C'
  laTempstru[14,3]= 3
  laTempstru[14,4]= 0
  
  laTempstru[15,1]='CINVSTAT'
  laTempstru[15,2]='C'
  laTempstru[15,3]= 8
  laTempstru[15,4]= 0
  
  laTempstru[16,1]='CAPDSTAT'
  laTempstru[16,2]='C'
  laTempstru[16,3]= 10
  laTempstru[16,4]= 0
  
  laTempstru[17,1]='CAPDACTID'
  laTempstru[17,2]='C'
  laTempstru[17,3]= 10
  laTempstru[17,4]= 0
  
  laTempstru[18,1]='NAPDAMNT'
  laTempstru[18,2]='N'
  laTempstru[18,3]= 8
  laTempstru[18,4]= 0
  
  laTempstru[19,1]='CAPDGLACT'
  laTempstru[19,2]='C'
  laTempstru[19,3]= 10
  laTempstru[19,4]= 0
  
  laTempstru[20,1]='NAPDLINNO'
  laTempstru[20,2]='N'
  laTempstru[20,3]= 8
  laTempstru[20,4]= 0
  
  laTempstru[21,1]='CACCNLDES'
  laTempstru[21,2]='C'
  laTempstru[21,3]= 30
  laTempstru[21,4]= 0
  
  laTempstru[22,1]='CGLACCNT'
  laTempstru[22,2]='C'
  laTempstru[22,3]= 30
  laTempstru[22,4]= 0
  
  laTempstru[23,1]='CAPACCT'
  laTempstru[23,2]='C'
  laTempstru[23,3]= 8
  laTempstru[23,4]= 0
        
* HES           
*!*	IF USED(lcTempFile)
*!*	  USE IN (lcTempFile)
*!*	ENDIF
* HES 

oAriaEnvironment.Cursors.createcursor(lcTempFile,@laTempStru,"CVENDCODE+CCURRCODE+CINVNO",lcTempFile,.T.)

* HES
ENDIF 
* HES
*--End of function

*!**************************************************************************
*! Function      : lfwRepWhen
*! Purpose       : Creating Temp file  
*! Developer     : AHMED MOUSTAFA (AHS)     
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfwRepWhen

IF !USED('APVENDOR') 
  *=gfOpenTABLE(oAriaApplication.DATADIR+'APVENDOR',oAriaApplication.DATADIR+'VENCODE','SH')
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APVENDOR',oAriaApplication.DATADIR+'VENCODE','SH',.F.,.F.)
  SELECT APVENDOR
  *=gfsetorder('VENCODE')
  *=gfseek('')
  oAriaEnvironment.remotetableaccess.Setorderto('VENCODE')
  oAriaEnvironment.remotetableaccess.SeekRecord('')
  
ENDIF
*!**************************************************************************
*!
*!      Function: lfFltSess
*!
*!**************************************************************************

FUNCTION lfFltSess
IF !USED('APDIST')
   *=gfOpenTABLE(oAriaApplication.DATADIR+'APDIST',oAriaApplication.DATADIR+'INVVEND','SH')
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaApplication.DATADIR+'APDIST',oAriaApplication.DATADIR+'INVVEND','SH',.F.,.F.)
ENDIF

lcOldFl = ALIAS()
SELECT APDIST
*=gfSeek(APINVHDR.CINVNO + APINVHDR.CVENDCODE)
oAriaEnvironment.remotetableaccess.SeekRecord(APINVHDR.CINVNO + APINVHDR.CVENDCODE)

LOCATE REST WHILE CINVNO+CVENDCODE+CAPDTRTYP = APINVHDR.CINVNO + APINVHDR.CVENDCODE ;
            FOR   cApSessNo = lcRpSess

IF EMPTY(lcOldFl)
  SELECT 0
ELSE  
  SELECT (lcOldFl)      
ENDIF
RETURN FOUND("APDIST")


*!*************************************************************
*! Name      : lfRepShow
*! Developer : Walid Abou El-Magd (WAM)
*! Date      : 08/17/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************

FUNCTION lfRepShow

*laOGFxFlt[1,6]= gcBaseCurr
laOGFxFlt[1,6]= oAriaApplication.BaseCurrency
*=lfOGShowGet("lnRepCurr")



*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
FUNCTION lfClearRep
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
FUNCTION lfCreatNdx
SELECT APINVHDR
IF TYPE("lcApInvHdr") <> 'U'

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


*:**************************************************************************
*:* Name        : lfGetInvH
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/05/2006
*:* Purpose     : if a vendor is selected then colloct the related lines from APINVHDR into a temp file with the alias APINVHDR to enhance speed
*:***************************************************************************
*T20061011.0006
FUNCTION lfGetInvH
PRIVATE lnVnPos, lnCurrPos, llRet, laFlds, lcVndr, lcSetToOrd, lcCurr
lcSvOrd  = ORDER('APINVHDR')
lcOpnTmp = gfTempName()

llRet = .F.
lnCurrPos = ASCAN(laOgFxFlt,'APINVHDR.CCURRCODE')
lnVnPos   = ASCAN(laOgVrFlt,'APDIST.CVENDCODE')

lcCurr = IIF( lnCurrPos>0 AND !EMPTY(laOgFxFlt[lnCurrPos,6]) , laOgFxFlt[lnCurrPos,6] , '' )

IF lnVnPos>0 .AND. !EMPTY(laOgVrFlt[lnVnPos,6])
  
  SELECT APINVHDR
  SET ORDER TO VENDINV
  
  COPY STRUCTURE TO (gcWorkDir+lcOpnTmp) WITH CDX
  USE (gcWorkDir+lcOpnTmp) IN 0
  
  lcCurrCrit = IIF( !EMPTY(lcCurr) , 'FOR CCURRCODE = "' + lcCurr + '"' , '' )
  
  lcVend = laOgVrFlt[lnVnPos,6] + '|'
  DO WHILE LEN(lcVend)>0
    lcVndr = SUBSTR(lcVend,1,8)
    lcVend = STRTRAN(lcVend,lcVndr+'|','')
    =SEEK(lcVndr,'APINVHDR')
    SCAN REST WHILE CVENDCODE+CINVNO = lcVndr &lcCurrCrit
      SCATTER MEMVAR
      WAIT WINDOW NOWAIT M.CVENDCODE 
      INSERT INTO (gcWorkDir+lcOpnTmp) FROM MEMVAR
    ENDSCAN  
  ENDDO  
  
  USE IN APINVHDR
  SELECT &lcOpnTmp
  USE (gcWorkDir+lcOpnTmp) ALIAS APINVHDR ORDER &lcSvOrd
  llRet = .T.
ENDIF

*- if no vendor is selected but a currency is there
IF !llRet AND !EMPTY(lcCurr)
  SELECT APINVHDR
  SET ORDER TO CURVENINV
  
  WAIT WINDOW NOWAIT lcCurr
  =SEEK(lcCurr,'APINVHDR')
  COPY REST WHILE CCURRCODE+CVENDCODE+CINVNO = lcCurr WITH CDX TO (gcWorkDir+lcOpnTmp)
  
  SELECT APINVHDR
  USE (gcWorkDir+lcOpnTmp) ALIAS APINVHDR ORDER &lcSvOrd
  llRet = .T.
ENDIF

WAIT CLEAR

RETURN llRet
*-- end of lfGetInvH.
