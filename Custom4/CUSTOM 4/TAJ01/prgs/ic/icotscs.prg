*:***************************************************************************
*: Program file  : ICOTSCS
*: Program desc. : Export Stock Availability to CSV
*: Tracking#     : C202432[T20211210.0001]      
*:        System : Aria 4 XP
*:        Module : IC
*:     Developer : Mariam Mazhar (MMT)
*:***************************************************************************
*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001]
*C202440,2 MMT 10/25/2022 Get UnAllocated Qty for location 01-LEB [T20221004.0001]
*:***************************************************************************
PARAMETERS lcRequestID, lcXMLFileName, ClientID
SET STEP ON 
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
  lcCurrentProcedure = loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()  
  SET DEFAULT TO &lcCurrentProcedure.
  
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

  oAriaEnvironment.REPORT.gcAct_Appl = 'IC'
  PUBLIC gcAct_Appl
  gcAct_Appl = 'IC'
  oAriaEnvironment.activeModuleID = 'IC'
  lfExpData()
ELSE
  lcExpr = gfOpGrid('ICOTSCS' , .T.)&&,.F.,.F.,.T.,.T.)
ENDIF
*!*************************************************************
*! Name      : local2ftp 
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/20/2022
*! Purpose   : OG when function
*!*************************************************************
FUNCTION lfwrepwhen

*!*************************************************************
*! Name      : lfExpData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/20/2022
*! Purpose   : Export Style data to CSV
*!*************************************************************
FUNCTION lfExpData
IF TYPE("gcRequestXMLFileName") = 'C'
  oAriaApplication.gcOutFile = FORCEEXT(oAriaApplication.gcOutFile,'CSV')
*!*    lcXml = loogscroll.ConvertVariablesTOXMl()
*!*    = STRTOFILE(lcXml, gcRequestXMLFileName)
  RETURN 
ENDIF
SET STEP ON 
*!*  IF !USED('SPCK_HDR')
*!*    =gfOpenTable('SPCK_HDR','SPCK_HDRST','SH')   && TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION
*!*  ENDIF
IF !USED('SPCK_HDR')
  =gfOpenTable('SPCK_HDR','SPCK_HDRST','SH')   && TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION
ENDIF

IF !USED('SPCK_LIN')
  =gfOpenTable('SPCK_LIN','SPCK_LINST','SH')   && TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION
ENDIF

*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][Start]
IF !USED('CODES')
  =gfOpenTable('CODES','CCODE_NO','SH')   && TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION
ENDIF
IF !USED('STYDYE')
  =gfOpenTable('STYDYE','STYDYE','SH')   && STYLE
ENDIF
*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][End]
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYLEUPC','SH')   && STYLE+SIZE
ENDIF

IF !USED('STYLE_A')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_A')   && STYLE
ENDIF

IF !USED('SCALE')
  =gfOpenTable('SCALE','SCALE','SH')   && TYPE+SCALE
ENDIF

IF !USED('STYLE')
  =gfOpenTable('STYLE','STYLE','SH')   && STYLE
ENDIF

CREATE CURSOR (lcTmpFile) (Inv_No C(28), Style C(19), UPC C(13), Qty N(7), Price N(12,2),SalPrice N(12,2),SizeNo C(1))
INDEX on Style+SizeNo  TAG (lcTmpFile) 

lnStyMaj = ASCAN(loOgScroll.laOgFxFlt,"STYLE.CSTYMAJOR")
lnStyMaj = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStyMaj ,1)
lcStyFlt= loOgScroll.laOgFxFlt[lnStyMaj ,6]
IF !EMPTY(lcStyFlt) AND USED(lcStyFlt) &&AND RECCOUNT(lcStyFlt)> 0
  SELECT(lcStyFlt)
  LOCATE 
  IF EOF()
    lcStyFlt = ''
  ENDIF
ENDIF 
*!*  SELECT STYLE_A
*!*  IF gfSeek('')
*!*    IF TYPE('lcXMLFileName') <> 'C'
*!*      oPross = CREATEOBJECT('ariaprogressbar')
*!*      oPross.lblFirstLabel.CAPTION = 'Collecting Style information....'
*!*      oPross.Show()
*!*    ELSE
*!*     loProgress.DESCRIPTION = 'Collecting Style information....'
*!*     loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId) 
*!*    ENDIF
*!*    LOCATE 
  *C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][Start]
  lcTempStyle = gfTempName()
  CREATE CURSOR (lcTempStyle) (Style C(19), CSIZES C(8))
  INDEX on STYLE TAG (lcTempStyle)
  *XXX
*!*    SELECT STYLE_A
*!*    SCAN FOR IIF(!EMPTY(lcStyFlt),SEEK(cStyMajor,lcStyFlt),.T.) AND  ALLTRIM(CDIVISION) ='ELY'
*!*      IF !SEEK(STYLE_A.STYLE,lcTempStyle,lcTempStyle)
*!*        INSERT INTO (lcTempStyle) (STYLE) VALUES (STYLE_A.STYLE)
*!*      ENDIF
*!*    ENDSCAN
SELECT SPCK_HDR
IF gfSeek('S26608')
  
  IF TYPE('lcXMLFileName') <> 'C'
    oPross = CREATEOBJECT('ariaprogressbar')
    oPross.lblFirstLabel.CAPTION = 'Collecting Style information....'
    oPross.Show()
  ELSE
   loProgress.DESCRIPTION = 'Collecting Style information....'
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId) 
  ENDIF
  *LOCATE    
  SCAN REST WHILE TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = 'S26608';
     FOR IIF(!EMPTY(lcStyFlt),SEEK(SUBSTR(Style,1,12),lcStyFlt),.T.) 
    IF gfSeek(SPCK_HDR.STYLE,'STYLE_A','STYLE') AND  ALLTRIM(STYLE_A.CDIVISION) ='ELY'
    ELSE
      SELECT SPCK_HDR
      LOOP   
    ENDIF 
    *XX
    lcSizes = ''
    DIMENSION laSz[1]
    laSz =''
    lnSzCount = 1
    *XX
    SELECT SPCK_LIN
    =gfSeek('S26608'+SPCK_HDR.STYLE)      
    SCAN REST WHILE TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = 'S26608'+SPCK_HDR.STYLE
      =gfSeek(SPCK_HDR.STYLE,'STYLE_A','STYLE')
      IF ALLTRIM(STYLE_A.CDIVISION) !='ELY'
        LOOP 
      ENDIF
     
      =gfSeek('S'+STYLE_A.SCALE,'SCALE','SCALE')
      FOR lnA = 1 TO Scale.Cnt
        lcA = STR(lnA,1)
        IF SPCK_LIN.Qty&lcA. > 0
          DIMENSION laSz[lnSzCount]
          laSz[lnSzCount]=lcA
          lnSzCount = lnSzCount +  1
        ENDIF
      ENDFOR
    ENDSCAN
    =ASORT(laSz)
    FOR lnS =1 TO ALEN(laSz,1)
      lcSizes = lcSizes + laSz[lnS]
    ENDFOR 
    IF !SEEK(SPCK_HDR.STYLE,lcTempStyle,lcTempStyle)
      INSERT INTO (lcTempStyle) (STYLE, CSIZES) VALUES (SPCK_HDR.STYLE,lcSizes)
    ENDIF
  ENDSCAN
  
ENDIF

 *XXX
  SELECT (lcTempStyle)
  LOCATE 
  IF !EOF()
    IF TYPE('lcXMLFileName')= 'C'
      lcClssFldr = STRTRAN(UPPER(oAriaApplication.ClassDir),"SRVCLSS","CLASSES")
      SET CLASSLIB TO (ADDBS(lcClssFldr )+"IC.VCX") ADDITIVE 
      SET CLASSLIB TO (ADDBS(STRTRAN(UPPER(lcClssFldr),"ARIA4XP","ARIA3EDI"))+"GLOBALS.VCX") ADDITIVE 
      IF USED('SYCCOMP')
        USE IN SYCCOMP
      ENDIF
      oAriaApplication.SYSPATH = oAriaApplication.systemfilespath 
    ENDIF
    
    loExportAdpt = CREATEOBJECT('ExportStyleQty')
    *C202440,2 MMT 10/25/2022 Get UnAllocated Qty for location 01-LEB [T20221004.0001][Start]
    *lcRetur = loExportAdpt.GetStyleQtys (lcTempStyle)    
    lcRetur = loExportAdpt.GetStyleQtys (lcTempStyle,'01-LEB')
    *C202440,2 MMT 10/25/2022 Get UnAllocated Qty for location 01-LEB [T20221004.0001][End]
    IF !EMPTY(lcRetur) AND USED(lcRetur)
      SELECT (lcRetur)
      LOCATE
      SCAN FOR !EMPTY(&lcRetur..UPC)
*!*          IF gfSeek(ALLTRIM(&lcRetur..VENDOR_STYLE),'STYLE_A','STYLE')
*!*            lnDiscPcnt = 0
*!*            lcDENDATE = ''
*!*            lcSTART = ''
*!*            IF !EMPTY(Style_A.cdisccode) AND Style_A.cdisccode != 'N\A'
*!*              DIMENSION laDiscRel[3,2]
*!*              laDiscRel[1,1] = 'DISCPCNT'      && Array to get the Division long name
*!*              laDiscRel[1,2] = 'lnDiscPcnt'
*!*              laDiscRel[2,1] = 'START'      && Array to get the Division long name
*!*              laDiscRel[2,2] = 'lcSTART'
*!*              laDiscRel[3,1] = 'DENDATE'      && Array to get the Division long name
*!*              laDiscRel[3,2] = 'lcDENDATE'
*!*              =gfRltFld(STYLE_A.cdisccode, @laDiscRel, 'CDISCCODE')
*!*              IF (!EMPTY(lcSTART) OR !EMPTY(lcDENDATE)) AND lnDiscPcnt > 0
*!*                IF !BETWEEN(DATE(),lcSTART,lcDENDATE)
*!*                  lnDiscPcnt = 0
*!*                ENDIF
*!*              ENDIF
*!*            ENDIF
          INSERT INTO (lcTmpFile) (Inv_No ,Style, UPC, Qty, Price, SalPrice,SizeNo) VALUES ;
                      ("EW-"+ALLTRIM(&lcRetur..Style_Major)+"-"+ALLTRIM(&lcRetur..Color_Code)+"-"+ALLTRIM(&lcRetur..Style_Size_Description),;
                        &lcRetur..VENDOR_STYLE,&lcRetur..UPC,&lcRetur..UnAllocated_Inventory,&lcRetur..UNIT_PRICE,;
                       (&lcRetur..UNIT_PRICE  * 2)+0.99,ALLTRIM(&lcRetur..Style_Size_Code))
              
*!*          ENDIF
      ENDSCAN
    ENDIF
  ENDIF  
  *C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][End] 
*!*    SCAN FOR IIF(!EMPTY(lcStyFlt),SEEK(cStyMajor,lcStyFlt),.T.) AND  ALLTRIM(CDIVISION) ='ELY'
*!*      lnDiscPcnt = 0
*!*      lcDENDATE = ''
*!*      lcSTART = ''
*!*      *IF gfSeek(SPCK_HDR.STYLE,'STYLE_A','STYLE')
*!*        IF !EMPTY(Style_A.cdisccode) AND Style_A.cdisccode != 'N\A'
*!*          DIMENSION laDiscRel[3,2]
*!*          laDiscRel[1,1] = 'DISCPCNT'      && Array to get the Division long name
*!*          laDiscRel[1,2] = 'lnDiscPcnt'
*!*          laDiscRel[2,1] = 'START'      && Array to get the Division long name
*!*          laDiscRel[2,2] = 'lcSTART'
*!*          laDiscRel[3,1] = 'DENDATE'      && Array to get the Division long name
*!*          laDiscRel[3,2] = 'lcDENDATE'
*!*          =gfRltFld(STYLE_A.cdisccode, @laDiscRel, 'CDISCCODE')
*!*          IF (!EMPTY(lcSTART) OR !EMPTY(lcDENDATE)) AND lnDiscPcnt > 0
*!*            IF !BETWEEN(DATE(),lcSTART,lcDENDATE)
*!*              lnDiscPcnt = 0
*!*            ENDIF
*!*          ENDIF
*!*        ENDIF
*!*      *ENDIF  
*!*  *!*      SELECT SPCK_LIN
*!*  *!*      =gfSeek('S26608'+SPCK_HDR.STYLE)      
*!*  *!*      SCAN REST WHILE TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = 'S26608'+SPCK_HDR.STYLE
*!*        =gfSeek('S'+STYLE_A.SCALE,'SCALE','SCALE')
*!*        FOR lnA = 1 TO Scale.Cnt
*!*          lcA = STR(lnA,1)
*!*          *IF SPCK_LIN.Qty&lcA. > 0
*!*          IF gfSeek(STYLE_A.STYLE+lcA,'STYLEUPC','STYLEUPC')
*!*            *IF ALLTRIM(STYLE_A.CDIVISION) ='ELY'
*!*              INSERT INTO (lcTmpFile) (Inv_No ,Style, UPC, Qty, Price, SalPrice,SizeNo) VALUES ;
*!*                    ("EW-"+ALLTRIM(STYLE_A.CSTYMAJOR)+"-"+ALLTRIM(SUBSTR(STYLE_A.STYLE,14,6))+"-"+SCALE.SZ&lcA.,;
*!*                      STYLE_A.STYLE,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,Style_A.STK&lcA. - STYLE_A.ORD&lcA.,STYLE_A.PRICEA,;
*!*                      IIF(lnDiscPcnt = 0,STYLE_A.PRICEA,(STYLE_A.PRICEA  *(1-(lnDiscPcnt /100)))),lcA)
*!*           * ENDIF    
*!*          ENDIF   
*!*         * ENDIF
*!*        ENDFOR
*!*  *!*      ENDSCAN
*!*    ENDSCAN
*!*  ENDIF

SELECT (lcTmpFile) 
LOCATE
IF EOF()
  oPross = Null
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records to display')
  RETURN
ENDIF
lcOutputFile = oAriaApplication.workdir+lcTmpFile+".CSV"

lcHeader = 'Inventory Number,Style Number,UPC,Quantity,Wholesale  Cost,Suggested Retail Price'+CHR(13)+CHR(10)
llBytesWrote = STRTOFILE(lcHeader ,lcOutputFile ,0)
SELECT (lcTmpFile)
lnRecNt = RECCOUNT(lcTmpFile)

IF TYPE('lcXMLFileName') <> 'C'  
  oPross = CREATEOBJECT('ariaprogressbar')
  oPross.lblFirstLabel.CAPTION = 'Exporting Style information....'
  oPross.TotalProgress = lnRecNt 
  oPross.AUTOCENTER = .T.
  oPross.SHOW()
ENDIF  
SELECT (lcTmpFile)
LOCATE
lnRecCntSty = 0
SCAN
  lnRecCntSty = lnRecCntSty + 1 
  IF TYPE('lcXMLFileName') <> 'C'
    oPross.CurrentProgress(lnRecCntSty)
    oPross.lblSecondLabel.CAPTION =ALLTRIM(Style)+"\"+ALLTRIM(+UPC)
  ELSE
    loProgress.Percent = lnRecCntSty/lnRecNt
    loProgress.DESCRIPTION = "Exporting Style:"+ALLTRIM(Style)+"\"+ALLTRIM(+UPC)
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  ENDIF  
  lcDetLine = ''+ALLTRIM(Inv_No)+','+STRTRAN(ALLTRIM(Style),' ','')+','+UPC+','+;
              ALLTRIM(STR(Qty,7,0))+','+ALLTRIM(STR(Price ,12,2))+','+ALLTRIM(STR(SalPrice ,12,2))+''+CHR(13)+CHR(10)
  llBytesWrote = STRTOFILE(lcDetLine ,lcOutputFile ,1)              
ENDSCAN
IF TYPE('lcXMLFileName') <> 'C'
  oPross = Null
ENDIF
IF FILE(lcOutputFile)
  lcFileName =  oAriaApplication.workdir+"FurmoStockAvailability"+".CSV"  && "ELY" + Date extracted + ".CSV" (i.e., ELY-01-16-2022.CSV)
  IF FILE(lcFileName) 
     ERASE (lcFileName) 
  ENDIF
  RENAME (lcOutputFile) TO  (lcFileName)
    
  IF FILE(lcFileName) 
    IF TYPE('lcXMLFileName') = 'C'
      loProgress.Percent = 0.9
      loProgress.Description = "Uploading file..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
      PRIVATE loProxy
      loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
      COPY FILE  (lcFileName) TO (gcOutFile) 
    ENDIF
*!*      IF FILE(("\\10.0.1.8\taj_ftp\FurmoGroup\"++"ELY-" +PADL(ALLTRIM(STR(MONTH(DATE()))),2,'0')+"-"+PADL(ALLTRIM(STR(DAY(DATE()))),2,'0')+"-"+ALLTRIM(STR(YEAR(DATE())))+".CSV")) 
*!*        ERASE ("\\10.0.1.8\taj_ftp\FurmoGroup\"++"ELY-" +PADL(ALLTRIM(STR(MONTH(DATE()))),2,'0')+"-"+PADL(ALLTRIM(STR(DAY(DATE()))),2,'0')+;
*!*      "-"+ALLTRIM(STR(YEAR(DATE())))+".CSV") 
*!*      ENDIF
    
    lfUploadFile (JUSTSTEM(lcFileName))
    IF TYPE('lcXMLFileName') = 'C'
      loProgress.Percent = 1.0
      loProgress.Description =  "Uploading file..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
    ENDIF  
  ENDIF
*!*    COPY FILE (lcOutputFile) TO (lcFileName) 
  ERASE (lcFileName) 
  IF TYPE('lcXMLFileName') <> 'C' 
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File:"+JUSTSTEM(lcFileName)+".CSV has been uploaded on ftp.")
  ENDIF  
ENDIF
*Inventory Number,Style Number,UPC,Quantity ,Wholesale  Cost,SALE/Pormotion Price 
*Ftp functions
*!*************************************************************
*! Name      : lfUploadFile
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/20/2022
*! Purpose   : Upload function
*!*************************************************************
FUNCTION lfUploadFile
PARAMETERS lcFileName

#DEFINE GENERIC_READ 2147483648 && &H80000000
#DEFINE GENERIC_WRITE 1073741824 && &H40000000

Local m.ftpServer, m.ftpUserName, m.ftpUserPass

PUBLIC hOpen, hFtpSession
DECLARE INTEGER InternetOpen IN wininet.dll;
STRING sAgent,;
INTEGER lAccessType,;
STRING sProxyName,;
STRING sProxyBypass,;
STRING lFlags

DECLARE INTEGER InternetCloseHandle IN wininet.dll;
INTEGER hInet

DECLARE INTEGER InternetConnect IN wininet.dll;
INTEGER hInternetSession,;
STRING sServerName,;
INTEGER nServerPort,;
STRING sUsername,;
STRING sPassword,;
INTEGER lService,;
INTEGER lFlags,;
INTEGER lContext

DECLARE INTEGER FtpOpenFile IN wininet.dll;
INTEGER hFtpSession,;
STRING sFileName,;
INTEGER lAccess,;
INTEGER lFlags,;
INTEGER lContext

DECLARE INTEGER InternetWriteFile IN wininet.dll;
INTEGER hFile,;
STRING @ sBuffer,;
INTEGER lNumBytesToWrite,;
INTEGER @ dwNumberOfBytesWritten
#DEFINE FTP_TRANSFER_TYPE_BINARY       2


m.ftpUserName = lcRPUser &&"Aria\TAJ01_A1" && ftp user Name 
m.ftpUserPass = lcRRPass &&"Support@2020" &&Ftp Password
m.ftpServer   = lcRPFtp &&"23.226.135.132"
lcTargetPath  = lcRPFTPPath &&"/taj_ftp/FurmoGroup/"


IF connect2ftp (m.ftpServer, m.ftpUserName, m.ftpUserPass)
  lcSourcePath = oAriaApplication.WorkDir && local folder   && remote folder (ftp server)

  lcSource = lcSourcePath + lcFileName+".CSV"
  lcTarget = lcTargetPath +lcFileName+".CSV"
 
  local2ftp (hFtpSession, lcSource, lcTarget)

  = InternetCloseHandle (hFtpSession)
  = InternetCloseHandle (hOpen)
ELSE
  RETURN .F.  
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : connect2ftp
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/20/2022
*! Purpose   : connect to ftp
*!*************************************************************
FUNCTION connect2ftp (strHost, strUser, strPwd)
** Open the access
sProxyName = CHR(0) &&... no proxy
sProxyBypass = CHR(0) &&... nothing to bypass
lFlags = 0 &&... no flags used
sAgent = 'vfp'
hOpen = InternetOpen (sAgent, 1,sProxyName, sProxyBypass, lFlags)

IF hOpen = 0
  RETURN .F.
ENDIF

** Connect to FTP.
hFtpSession = InternetConnect (hOpen, strHost, 0, strUser, strPwd, 1, 0,0)


IF hFtpSession = 0
** Close
= InternetCloseHandle (hOpen)
*? "FTP " + strHost + " not ready"
RETURN .F.
ELSE
*? "Connected to " + strHost + " as: [" + strUser + ", *****]"
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : local2ftp 
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/20/2022
*! Purpose   : Copy to ftp
*!*************************************************************
FUNCTION local2ftp (hConnect, lcSource, lcTarget)
** Upload local file to ftp server
hSource = FOPEN (lcSource)
IF (hSource = -1)
RETURN -1
ENDIF

** New file in ftp server
hTarget = FtpOpenFile(hConnect, lcTarget, GENERIC_WRITE, 2, 0)
IF hTarget = 0
= FCLOSE (hSource)
RETURN -2
ENDIF

lnBytesWritten = 0
lnChunkSize = 512 && 128, 512
DO WHILE Not FEOF(hSource)
lcBuffer = FREAD (hSource, lnChunkSize)
lnLength = Len(lcBuffer)
IF lnLength > 0
IF InternetWriteFile (hTarget, @lcBuffer, lnLength, @lnLength) =1
lnBytesWritten = lnBytesWritten + lnLength
*? lnBytesWritten
** Show Progress
ELSE
EXIT
ENDIF
ELSE
EXIT
ENDIF
ENDDO

= InternetCloseHandle (hTarget)
= FCLOSE (hSource)

RETURN lnBytesWritten 