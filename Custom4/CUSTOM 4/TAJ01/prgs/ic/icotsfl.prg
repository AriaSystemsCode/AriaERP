*:***************************************************************************
*: Program file  : ICOTSFL
*: Program desc. : Fly Clothing Inventory Availability
*: Tracking#     : C202433[T20211209.0001]      
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
  lfExpFLData()
ELSE
  lcExpr = gfOpGrid('ICOTSFL' , .T.)&&,.F.,.F.,.T.,.T.)
ENDIF
*!*************************************************************
*! Name      : local2ftp 
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/14/2022
*! Purpose   : OG when function
*!*************************************************************
FUNCTION lfwrepwhen

*!*************************************************************
*! Name      : lfExpFLData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/14/2022
*! Purpose   : Export Style data to xlsx
*!*************************************************************
FUNCTION lfExpFLData
IF TYPE("gcRequestXMLFileName") = 'C'
  oAriaApplication.gcOutFile = FORCEEXT(oAriaApplication.gcOutFile,'xlsx')
*!*    lcXml = loogscroll.ConvertVariablesTOXMl()
*!*    = STRTOFILE(lcXml, gcRequestXMLFileName)
  RETURN 
ENDIF
SET STEP ON 
IF !USED('SPCK_HDR')
  =gfOpenTable('SPCK_HDR','SPCK_HDRST','SH')   && TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION
ENDIF

IF !USED('SPCK_LIN')
  =gfOpenTable('SPCK_LIN','SPCK_LINST','SH')   && TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION
ENDIF

IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYLEUPC','SH')   && STYLE+SIZE
ENDIF

IF !USED('STYLE_A')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_A')   && STYLE
ENDIF

IF !USED('SCALE')
  =gfOpenTable('SCALE','SCALE','SH')   && TYPE+SCALE
ENDIF

*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][Start]
IF !USED('CODES')
  =gfOpenTable('CODES','CCODE_NO','SH')   && TYPE+SCALE
ENDIF
IF !USED('STYLE')
  =gfOpenTable('STYLE','STYLE','SH')   && STYLE
ENDIF
IF !USED('STYDYE')
  =gfOpenTable('STYDYE','STYDYE','SH')   && STYLE
ENDIF
*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][end]

CREATE CURSOR (lcTmpFile) (Style C(19), UPC C(13), Qty N(7), Price N(12,2),SizeNo C(1),SKU C(30),DataVal C(120))
INDEX on Style+SizeNo  TAG (lcTmpFile) 

lnStyMaj = ASCAN(loOgScroll.laOgFxFlt,"STYLE.CSTYMAJOR")
lnStyMaj = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStyMaj ,1)
lcStyFlt= loOgScroll.laOgFxFlt[lnStyMaj ,6]
IF !EMPTY(lcStyFlt) AND USED(lcStyFlt) AND RECCOUNT(lcStyFlt)> 0
  SELECT(lcStyFlt)
  LOCATE 
  IF EOF()
    lcStyFlt = ''
  ENDIF
ENDIF
*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][Start]
lcTempStyle = gfTempName()
CREATE CURSOR (lcTempStyle) (Style C(19), CSIZES C(8))
INDEX on STYLE TAG (lcTempStyle)
*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][End]
*!*  SELECT STYLE_A
*!*  IF gfSeek('')
SELECT SPCK_HDR
IF gfSeek('S04421')
  
  IF TYPE('lcXMLFileName') <> 'C'
    oPross = CREATEOBJECT('ariaprogressbar')
    oPross.lblFirstLabel.CAPTION = 'Collecting Style information....'
    oPross.Show()
  ELSE
   loProgress.DESCRIPTION = 'Collecting Style information....'
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId) 
  ENDIF
  *LOCATE    
  SCAN REST WHILE TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = 'S04421';
     FOR IIF(!EMPTY(lcStyFlt),SEEK(SUBSTR(Style,1,12),lcStyFlt),.T.) &&AND  ALLTRIM(CDIVISION) ='ELY'
*!*      lnDiscPcnt = 0
*!*      lcDENDATE = ''
*!*      lcSTART = ''
    IF gfSeek(SPCK_HDR.STYLE,'STYLE_A','STYLE') AND  ALLTRIM(STYLE_A.CDIVISION) ='ELY'
       
      
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
    =gfSeek('S04421'+SPCK_HDR.STYLE)      
    SCAN REST WHILE TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = 'S04421'+SPCK_HDR.STYLE
      =gfSeek(SPCK_HDR.STYLE,'STYLE_A','STYLE')
      IF ALLTRIM(STYLE_A.CDIVISION) !='ELY'
        LOOP 
      ENDIF
     
      =gfSeek('S'+STYLE_A.SCALE,'SCALE','SCALE')
      FOR lnA = 1 TO Scale.Cnt
        lcA = STR(lnA,1)
        IF SPCK_LIN.Qty&lcA. > 0
           *XX
           DIMENSION laSz[lnSzCount]
           laSz[lnSzCount]=lcA
           lnSzCount = lnSzCount +  1
           *lcSizes = lcSizes + lcA
           *XX
*!*	        IF gfSeek(STYLE_A.STYLE+lcA,'STYLEUPC','STYLEUPC')
*!*	          *IF ALLTRIM(STYLE_A.CDIVISION) ='ELY'
*!*	            INSERT INTO (lcTmpFile) (Style, UPC, Qty, Price,SizeNo,SKU,DataVal) VALUES ;
*!*	                  (STYLE_A.STYLE,STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3,Style_A.STK&lcA. - STYLE_A.ORD&lcA.,STYLE_A.PRICEA,lcA,;
*!*	                   ALLTRIM(STYLE_A.CSTYMAJOR)+"-"+ALLTRIM(SUBSTR(STYLE_A.STYLE,14,6))+"-"+SCALE.SZ&lcA.,;
*!*	                   ALLTRIM(STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3)+'|'+;
*!*	                   ALLTRIM(ALLTRIM(STYLE_A.CSTYMAJOR)+"-"+ALLTRIM(SUBSTR(STYLE_A.STYLE,14,6))+"-"+SCALE.SZ&lcA.)+'|'+;
*!*	                   ALLTRIM(STR(IIF((Style_A.STK&lcA. - STYLE_A.ORD&lcA.) > 0,(Style_A.STK&lcA. - STYLE_A.ORD&lcA.),0),7,0))+;
*!*	                  '|'+ALLTRIM(STR(STYLE_A.PRICEA ,12,2))+'|'+ALLTRIM(STR((STYLE_A.PRICEA*2)+0.99,12,2)))
*!*	         * ENDIF    
*!*	        ENDIF   
        ENDIF
      ENDFOR
    ENDSCAN
    =ASORT(laSz)
    FOR lnS =1 TO ALEN(laSz,1)
      lcSizes = lcSizes + laSz[lnS]
    ENDFOR 
   *C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][Start]
    IF !SEEK(SPCK_HDR.STYLE,lcTempStyle,lcTempStyle)
      INSERT INTO (lcTempStyle) (STYLE, CSIZES) VALUES (SPCK_HDR.STYLE,lcSizes)
    ENDIF
    *C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][End]
  ENDSCAN
  
ENDIF
*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][Start]
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
    SCAN
     INSERT INTO (lcTmpFile) (Style, UPC, Qty, Price,SizeNo,SKU,DataVal) VALUES ;
                  (ALLTRIM(&lcRetur..VENDOR_STYLE),ALLTRIM(&lcRetur..UPC),&lcRetur..UnAllocated_Inventory,&lcRetur..UNIT_PRICE,;
                   ALLTRIM(&lcRetur..Style_Size_Code),;
                   ALLTRIM(&lcRetur..Style_Major)+"-"+ALLTRIM(&lcRetur..Color_Code)+"-"+ALLTRIM(&lcRetur..Style_Size_Description),;
                   ALLTRIM(&lcRetur..UPC)+'|'+;
                   ALLTRIM(ALLTRIM(&lcRetur..Style_Major)+"-"+ALLTRIM(&lcRetur..Color_Code)+"-"+ALLTRIM(&lcRetur..Style_Size_Description))+'|'+;
                   ALLTRIM(STR(IIF((&lcRetur..UnAllocated_Inventory) > 0,(&lcRetur..UnAllocated_Inventory),0),7,0))+;
                  '|'+ALLTRIM(STR(&lcRetur..UNIT_PRICE ,12,2))+'|'+ALLTRIM(STR((&lcRetur..UNIT_PRICE*2)+0.99,12,2)))      
    ENDSCAN
  ENDIF
ENDIF 
*C202440,1 MMT 07/14/2022 Use Product Availability Adaptor to Get UnAllocated Qty[T20220406.0001][End]
SELECT (lcTmpFile) 
LOCATE
IF EOF()
  oPross = Null
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records to display')
  ENDIF  
  RETURN
ENDIF
lcOutputFile = oAriaApplication.workdir+lcTmpFile+".CSV"


lcHeader = 'UPC|SKU|Quantity|UNIT PRICE|Suggested Retail Price'+CHR(13)+CHR(10) &&SALE/Pormotion Price
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
IF TYPE('lcXMLFileName') <> 'C'  
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
  lcDetLine = ''+ALLTRIM(UPC)+'|'+ALLTRIM(SKU)+'|'+ALLTRIM(STR(IIF(Qty > 0,Qty,0),7,0))+;
              '|'+ALLTRIM(STR(Price ,12,2))+'|'+ALLTRIM(STR((Price*2)+0.99,12,2))+''+CHR(13)+CHR(10)
  llBytesWrote = STRTOFILE(lcDetLine ,lcOutputFile ,1)              
ENDSCAN
ELSE
  lcOutputFile = FORCEEXT(lcOutputFile,'XLSX')
  *lcOutputFile = addbs(fullp(''))+"work\"+gfTempName()+".XLS"
*  COPY to (lcOutputFile) type XLS field DataVal
  CopytoExcel(lcTmpFile,lcOutputFile,.F.,.F.,'DataVal',.F.)
  *COPY to (lcOutputFile) DELIMITED WITH "" WITH CHARACTER tab field DataVal
ENDIF
IF TYPE('lcXMLFileName') <> 'C'
  oPross = Null
ENDIF
IF FILE(lcOutputFile)

*!*    lcFileName =  oAriaApplication.workdir+"ELY-" +PADL(ALLTRIM(STR(MONTH(DATE()))),2,'0')+"-"+PADL(ALLTRIM(STR(DAY(DATE()))),2,'0')+;
*!*    "-"+ALLTRIM(STR(YEAR(DATE())))+".CSV"  && "ELY" + Date extracted + ".CSV" (i.e., ELY-01-16-2022.CSV)
  lcFileName = oAriaApplication.workdir+'Fly StockAvailability.xlsx'  
  IF FILE(lcFileName) 
     ERASE (lcFileName) 
  ENDIF
  IF TYPE('lcXMLFileName') <> 'C'
    OEXCEL = NEWOBJECT("Table2Excel",ADDBS(oAriaApplication.ref4.ClassDir)+'optiongrid.vcx',"",lcOutputFile)

    IF VARTYPE(OEXCEL) != "O" OR ISNULL(OEXCEL)
      WAIT CLEAR
      IF TYPE('lcXMLFileName') <> 'C'
        =GFMODALGEN("INM00406B00000","DIALOGE")
      ENDIF  
      RETURN .F.
   ENDIF
    OEXCEL.XLSFORMAT = 'XLSX'
    OEXCEL.SAVE()

    *OEXCEL.CLOSE()
    OEXCEL= .NULL.

  ELSE
   * OEXCEL = NEWOBJECT("Table2Excel",ADDBS(oAriaApplication.ClassDir)+'SY\ARIAMAIN.vcx',"",lcOutputFile)

*!*	    LOCAL lcOldErr, llError
*!*	    lcOldErr = ON("ERROR")
*!*	    ON ERROR llError = .T.
    *oExcelSheet  = GetObject(lcOutputFile,"excel.application")
*!*	    oXLApp1 = GetObject(lcOutputFile,"excel.application")
*!*	    oXLApp1.Workbooks.Open (lcOutputFile)
*!*	    oXLApp1.Workbooks(1).SaveAs (FORCEEXT(lcOutputFile,'XLSX'),51)
*!*		oXLApp1.Workbooks(1).Close (.t.)
*!*		oXLApp1 = null
*!*	    oExcel = CREATEOBJECT("Excel.Application")
*!*	    oExcel.visible = .F.
*!*	    oExcelSheet = oExcel.Application.Workbooks.Open(lcOutputFile)
*!*	    oExcelSheet.Sheets(1).Rows(1).Delete()
*!*	    IF llError
*!*	      RETURN .F.
*!*	    ENDIF
*!*	    lcTempFile = gfTempName()
*!*	    *oExcelApp = oExcelSheet.Application
*!*	    *oExcelApp.Windows(JUSTFNAME(cExcelFile)).Visible = .T.  && Make the window Visible
*!*	    oExcelSheet.SaveAs(oAriaApplication.WorkDir+lcTempFile+'.xlsx',51)
*!*	    oExcelSheet.Close(.T.)
*!*	    DELETE FILE (lcOutputFile)
*!*	    RENAME (oAriaApplication.WorkDir+lcTempFile+'.xlsx') TO (FORCEEXT(lcOutputFile,'XLSX'))
*!*	    ON ERROR &lcOldErr.
   * RETURN NOT llError
    *OEXCEL = NEWOBJECT("Table2Excel",ADDBS(STRTRAN(UPPER(oAriaApplication.ClassDir),'SRVCLSS','CLASSES'))+'optiongrid.vcx',"",lcOutputFile)
  ENDIF  
  lcOutputFile = FORCEEXT(lcOutputFile,'XLSX')
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
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"File:"+JUSTSTEM(lcFileName)+".xlsx has been uploaded on ftp.")
  ENDIF  
ENDIF
*Inventory Number,Style Number,UPC,Quantity ,Wholesale  Cost,SALE/Pormotion Price 
*Ftp functions
*!*************************************************************
*! Name      : lfUploadFile
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/14/2022
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

  lcSource = lcSourcePath + lcFileName+".xlsx"
  lcTarget = lcTargetPath +lcFileName+".xlsx"
 
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
*: Date      : 02/14/2022
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
*: Date      : 02/14/2022
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
*!*************************************************************
*! Name      : local2ftp 
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/14/2022
*! Purpose   : Create XLSX from VFP cursor
*!*************************************************************
FUNCTION  CopytoExcel
* Version 2.9.1
LPARAMETERS cCur,lcFileName,llHead,lnMaxIndexLen,lcFFields,llMemoAsComment
* Parameters
* cCur				name of the table / cursor
* lcFileName		name of the xlsx
* llHead			.T. first row of xlsx contain column names
* lnMaxIndexLen		optional, maximum length of the indexes. A value between 19 and 120. Default 60
* lcFFields			optional, list of fields to be outputed
* llMemoAsComment	optional, memo fields are converted into comments while the cell contains the word "Memo"
DECLARE INTEGER ShellExecute IN SHELL32.DLL INTEGER nWinHandle,STRING cOperation,STRING cFileName,STRING cParameters,STRING cDirectory,INTEGER nShowWindow
IF OS(3) < '6' && XP
	Declare INTEGER GetLocaleInfo in Win32API LONG Locale, LONG LCType, STRING @LpLCData, INTEGER cchData
ELSE
	Declare INTEGER GetLocaleInfoEx in Win32API String Locale, LONG LCType, STRING @LpLCData, INTEGER cchData
ENDIF
DECLARE Sleep IN kernel32 INTEGER

LOCAL lcMyPath,lcDir,loerr as Exception
LOCAL lnRowsNo,lnColsNo,lnCurRow,lnCurCol,lnTime,ltTime,lcSetDec,lnColsNoAll,laFieldsAll[1],lnLenStr,lnLenIdx,llMemos,lnII,lSetTalk,lnFFields,laFFields[1]
LOCAL cStrings,lnColChars,llCR,lcValue,lnDec,cMax,ldValue 
LOCAL lcCurr
LOCAL ldDat01,ldDat11,ldDat02,ldDat12,ldDat03,ltDat02
LOCAL lnFHStr,lnFHSh,lcLenStr,lcLenIdx,lnCountbefore,lcUnion,lcField,lnTotal,cTotal,lcCurRow,ofile,lcSource,lcZipFileName,oShell,oFolder,llBelow7
LOCAL laFields[1,6],lnType,lala[1],lcSetPoint,lnFHCo,lnFHDr,lnCurComm

IF PCOUNT() < 1 
	MESSAGEBOX("Nothing to export",48,"No xlsx generated")
	RETURN
ELSE
	IF VARTYPE(m.cCur) $ "CV"
		IF !USED(m.cCur)
			USE (m.cCur)
		ENDIF
	ELSE
		MESSAGEBOX("Not a cursor/table name",48,"No xlsx generated")
		RETURN
	ENDIF
ENDIF
IF PCOUNT() < 2 
	lcFileName = FORCEEXT(SYS(2015),"xlsx")
ELSE
	IF VARTYPE(m.lcFileName) $ "CV"
		lcFileName = FORCEEXT(m.lcFileName,"xlsx")
	ELSE
		lcFileName = FORCEEXT(SYS(2015),"xlsx")
	ENDIF
ENDIF
IF PCOUNT() < 3
	llHead = .F.
ELSE
	IF VARTYPE(m.llHead) <> "L"
		llHead = .F.
	ENDIF
ENDIF
IF PCOUNT()<4
	lnMaxIndexLen = 60
ELSE
	IF VARTYPE(m.lnMaxIndexLen) $ "NFYBI"
		lnMaxIndexLen = INT(m.lnMaxIndexLen)
	ELSE 
		lnMaxIndexLen = 60
	ENDIF
	lnMaxIndexLen = MIN(MAX(m.lnMaxIndexLen,19),120)
ENDIF

IF FILE(FORCEEXT(m.lcFileName,"xlsx"))
	IF MESSAGEBOX(FORCEEXT(m.lcFileName,"xlsx")+" already exist."+CHR(13)+"Overwrite?",4+48) = 7
		RETURN
	ELSE
		ERASE (FORCEEXT(m.lcFileName,"xlsx")) RECYCLE
	ENDIF
ENDIF
IF PCOUNT()<5
	lcFFields = ""
ELSE
	IF VARTYPE(m.lcFFields) <> "C"
		lcFFields = ""
	ELSE
		lnFFields = ALINES(laFFields,m.lcFFields,1+4,",")
	ENDIF
ENDIF
IF PCOUNT() < 6
	llMemoAsComment = .F.
ELSE
	IF VARTYPE(m.llMemoAsComment) <> "L"
		llMemoAsComment = .F.
	ENDIF
ENDIF


lSetTalk = SET("Talk")
SET TALK OFF 
lcSetPoint = SET("Point")
SET POINT TO "."
lnColsNoAll=AFIELDS(m.laFieldsAll,m.cCur)
lnColsNo = 0
lnLenStr = 19 && for datetime
llMemos = .F.
lcUnion = ""
cTotal = SYS(2015)
lnTotal = 0
ldDat01 = DATE(1900,3,1)
ldDat02 = DATE(1900,1,1)
ldDat03 = DATE(1900,2,28)
ldDat11 = m.ldDat01 - 61
ldDat12 = m.ldDat02 - 1
ltDat02 = DATETIME(1900,1,1,0,0,0)
lnColChars = 0

FOR lnCurCol = 1 TO m.lnColsNoAll
	IF m.laFieldsAll[m.lnCurCol,2] $ "NFYBIDTLCVM"
		IF !EMPTY(m.lcFFields)
			IF ASCAN(m.laFFields,laFieldsAll[m.lnCurCol,1],1,-1,-1,1+2+4)=0
				LOOP
			ENDIF
		ENDIF
		lnColsNo = m.lnColsNo + 1
		DIMENSION laFields[m.lnColsNo,6]
		laFields[m.lnColsNo,1] = laFieldsAll[m.lnCurCol,1]
		laFields[m.lnColsNo,2] = IIF(laFieldsAll[m.lnCurCol,2] $ "CV",1,;
								IIF(laFieldsAll[m.lnCurCol,2] $ "NF",2,;
								IIF(laFieldsAll[m.lnCurCol,2] == "I",3,;
								IIF(laFieldsAll[m.lnCurCol,2] == "D",4,;
								IIF(laFieldsAll[m.lnCurCol,2] == "T",5,;
								IIF(laFieldsAll[m.lnCurCol,2] == "L",6,;
								IIF(laFieldsAll[m.lnCurCol,2] == "Y",7,;
								IIF(laFieldsAll[m.lnCurCol,2] == "B",8,;
								IIF(laFieldsAll[m.lnCurCol,2] == "M",9,10)))))))))
		laFields[m.lnColsNo,6] = m.lnCurCol
		laFields[m.lnColsNo,3] = laFieldsAll[m.lnCurCol,3]
		laFields[m.lnColsNo,4] = laFieldsAll[m.lnCurCol,4]
		laFields[m.lnColsNo,5] = IIF(m.lnColsNo<=26,[],CHR(64+FLOOR((m.lnColsNo-1)/26)))+CHR(65+MOD(m.lnColsNo-1,26))
	ELSE
		LOOP
	ENDIF
	lcField = laFieldsAll[m.lnCurCol,1]
	IF m.laFieldsAll[m.lnCurCol,2] $ "CV"
		lnLenStr = MAX(m.lnLenStr, laFieldsAll[m.lnCurCol,3])
		IF !EMPTY(m.lcUnion)
			lcUnion = m.lcUnion + " UNION"
		ENDIF
		lcUnion = m.lcUnion + " SELECT DISTINCT CAST(RTRIM(" + m.lcField + ") AS V("+TRANSFORM(m.laFieldsAll[m.lnCurCol,3])+")) FROM " + m.cCur + " WHERE !ISNULL(" + m.lcField + ")"
		SELECT COUNT(*) as no FROM (m.cCur) WHERE ISNULL(&lcField) INTO CURSOR (cTotal)
		lnTotal = m.lnTotal + RECCOUNT(m.cCur) - &cTotal..no
	ENDIF
	IF m.laFieldsAll[m.lnCurCol,2] == "D"
		IF !EMPTY(m.lcUnion)
			lcUnion = m.lcUnion + " UNION"
		ENDIF
		lcUnion = m.lcUnion + " SELECT DISTINCT DTOC(" + m.lcField + ") FROM " + m.cCur + " WHERE " + m.lcField + " < m.ldDat02 "
		SELECT COUNT(*) as no FROM (m.cCur) WHERE &lcField < m.ldDat02 INTO CURSOR (cTotal)
		lnTotal = m.lnTotal + &cTotal..no
	ENDIF
	IF m.laFieldsAll[m.lnCurCol,2] == "T"
		IF !EMPTY(m.lcUnion)
			lcUnion = m.lcUnion + " UNION"
		ENDIF
		lcUnion = m.lcUnion + " SELECT DISTINCT TTOC(" + m.lcField + ") FROM " + m.cCur + " WHERE " + m.lcField + " < m.ltDat02 "
		SELECT COUNT(*) as no FROM (m.cCur) WHERE &lcField < m.ltDat02 INTO CURSOR (cTotal)
		lnTotal = m.lnTotal + &cTotal..no
	ENDIF
	IF m.laFieldsAll[m.lnCurCol,2] == "M"
		lnColChars = m.lnColChars +1
		llMemos = .T.
	ENDIF
NEXT
lnLenIdx = MIN(m.lnMaxIndexLen, m.lnLenStr)
lnTotal = m.lnTotal + m.lnColsNo

SELECT (m.cCur)
COUNT TO m.lnRowsNo
lnTotal = m.lnTotal + m.lnRowsNo * m.lnColChars
lnRowsNo=m.lnRowsNo+IIF(m.llHead,1,0)

cStrings = SYS(2015)
cMax = SYS(2015)
CREATE CURSOR (m.cStrings) (ii I AUTOINC NEXTVALUE 0,cStr V(m.lnLenStr),cM M)
IF m.llMemoAsComment and m.llMemos
	INSERT INTO (m.cStrings) (cStr) VALUES ("Memo")
ENDIF

IF !EMPTY(m.lcUnion)
	EXECSCRIPT("LPARAMETERS ldDat02,ltDat02"+CHR(13)+"INSERT INTO " + m.cStrings + " (cStr)" + m.lcUnion,m.ldDat02,m.ltDat02)
ENDIF

IF m.lnLenStr > 0
	IF m.lnLenIdx >= m.lnLenStr
		INDEX on cStr TAG cStr
	ELSE
		lcLenIdx = LTRIM(STR(m.lnLenIdx))
		INDEX on LEFT(cStr,&lcLenIdx) TAG cStr
	ENDIF
ENDIF
IF m.llMemos
	lcLenStr = LTRIM(STR(m.lnLenStr))
	INDEX on LEFT(cM,&lcLenStr) TAG cM
ENDIF
SET ORDER TO cStr

lcCurr = Getcurr()

lcMyPath=''
IF !EMPTY(JUSTPATH(m.lcFileName))
	lcMyPath=ADDBS(JUSTPATH(m.lcFileName)) 
	SET DEFAULT TO (m.lcMyPath)
ELSE
	lcMyPath = ADDBS(JUSTPATH(FULLPATH(m.lcFileName)))
ENDIF

lcDir=gen_dirs(m.llMemoAsComment and m.llMemos)
gen_Content_Types(m.lcDir,m.llMemoAsComment and m.llMemos)
gen_rels(ADDBS(m.lcDir+[_rels]))
gen_app(ADDBS(m.lcDir+[docProps]))
gen_core(ADDBS(m.lcDir+[docProps]))
gen_workbook(ADDBS(ADDBS(m.lcDir+[xl])+[_rels]),m.llMemoAsComment and m.llMemos)
gen_styles(ADDBS(m.lcDir+[xl]),m.lcCurr,m.llMemoAsComment and m.llMemos)
gen_workbook2(ADDBS(m.lcDir+[xl]))
IF m.llMemoAsComment and m.llMemos
	gen_workbook3(ADDBS(ADDBS(ADDBS(m.lcDir+[xl])+[worksheets])+[_rels]))
ENDIF 

* Begin sheet1
lnFHSh = FCREATE(ADDBS(ADDBS(m.lcDir+[xl])+[worksheets])+"sheet1.xml")
IF m.lnFHSh < 0
	MESSAGEBOX('Cannot create sheet1.xml',16,'Abort')
	RETURN
ENDIF
FWRITE(m.lnFHSh,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
FWRITE(m.lnFHSh,[<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ])
IF m.llMemoAsComment AND m.llMemos
  FWRITE(m.lnFHSh,[xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing" ])
ENDIF
  FWRITE(m.lnFHSh,[xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="x14ac" xmlns:x14ac="http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac"> ])
FWRITE(m.lnFHSh,[<dimension ref="A1:]+IIF(m.lnColsNo>26,CHR(64+FLOOR((m.lnColsNo-1)/26)),[])+CHR(65+MOD(m.lnColsNo-1,26))+TRANSFORM(m.lnRowsNo)+["/>])
*FWRITE(m.lnFHSh,[<sheetViews><sheetView tabSelected="1" workbookViewId="0"/></sheetViews>])
FWRITE(m.lnFHSh,[<sheetViews><sheetView workbookViewId="0"/></sheetViews>])
FWRITE(m.lnFHSh,[<sheetFormatPr defaultRowHeight="15" x14ac:dyDescent="0.25"/>])
FWRITE(m.lnFHSh,[<sheetData>])

IF m.llMemoAsComment AND m.llMemos
* Begin comments1
	lnFHCo = FCREATE(ADDBS(m.lcDir+[xl])+"comments1.xml")
	IF m.lnFHCo < 0
		MESSAGEBOX('Cannot create comments1.xml',16,'Abort')
		RETURN
	ENDIF
	FWRITE(m.lnFHCo,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	FWRITE(m.lnFHCo,[<comments xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">])
		FWRITE(m.lnFHCo,[<authors>])
			FWRITE(m.lnFHCo,[<author>CopyToXlsx</author>])
		FWRITE(m.lnFHCo,[</authors>])
		FWRITE(m.lnFHCo,[<commentList>])
* Begin vmlDrawing1.vml
	lnFHDr = FCREATE(ADDBS(ADDBS(m.lcDir+[xl])+[drawings])+"vmlDrawing1.vml")
	IF m.lnFHDr < 0
		MESSAGEBOX('Cannot create vmlDrawing1.vml',16,'Abort')
		RETURN
	ENDIF
	FWRITE(m.lnFHDr,[<?xml version="1.0"?>]+CHR(10))
	FWRITE(m.lnFHDr,[<xml xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:v="urn:schemas-microsoft-com:vml">])
		FWRITE(m.lnFHDr,[<o:shapelayout v:ext="edit">])
			FWRITE(m.lnFHDr,[<o:idmap v:ext="edit" data="1"/>])
		FWRITE(m.lnFHDr,[</o:shapelayout>])
		FWRITE(m.lnFHDr,[<v:shapetype path="m,l,21600r21600,l21600,xe" o:spt="202" coordsize="21600,21600" id="_x0000_t202">])
			FWRITE(m.lnFHDr,[<v:stroke joinstyle="miter"/>])
			FWRITE(m.lnFHDr,[<v:path o:connecttype="rect" gradientshapeok="t"/>])
		FWRITE(m.lnFHDr,[</v:shapetype>])
ENDIF

* Begin sharedStrings
lnFHStr = FCREATE(ADDBS(m.lcDir+[xl])+"sharedStrings.xml")
IF m.lnFHStr < 0
	MESSAGEBOX('Cannot create sharedStrings.xml',16,'Abort')
	RETURN
ENDIF
FWRITE(m.lnFHStr,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
FWRITE(m.lnFHStr,[<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="]+SPACE(40)) 

SELECT (m.cStrings)
SET ORDER TO 
SCAN NOOPTIMIZE
	FWRITE(m.lnFHStr,[<si><t>]+htmspec(cStr)+[</t></si>])
ENDSCAN
SET ORDER TO cStr

lnCurRow = 0
IF m.llHead
	lnCurRow = m.lnCurRow + 1
	lcCurRow = LTRIM(STR(m.lnCurRow))
	FWRITE(m.lnFHSh,[<row r="]+m.lcCurRow+[" spans="1:1" x14ac:dyDescent="0.25">])
	FOR lnCurCol = 1 TO m.lnColsNo
		FWRITE(m.lnFHSh,[<c r="]+IIF(m.lnCurCol>26,CHR(64+FLOOR((m.lnCurCol-1)/26)),[])+CHR(65+MOD(m.lnCurCol-1,26))+m.lcCurRow)
		lcValue = m.laFields[m.lnCurCol,1]
		SELECT (m.cStrings)
		IF m.lnLenIdx >= m.lnLenStr
			SET KEY TO m.lcValue
		ELSE
			SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
		ENDIF
		lnII = 0
		SCAN
			IF cStr == m.lcValue
				lnII = ii
				EXIT
			ENDIF
		ENDSCAN
		SET KEY TO
		IF m.lnII > 0
			FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(m.lnII))+[</v></c>])
		ELSE
			FWRITE(m.lnFHStr,[<si><t>]+htmspec(m.lcValue)+[</t></si>])
			INSERT INTO (m.cStrings) (cStr) VALUES (m.lcValue)
			SELECT MAX(ii) as ii FROM (m.cStrings) INTO CURSOR (m.cMax)
			SELECT (m.cMax)
			lnII = ii
			FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(m.lnII))+[</v></c>])
		ENDIF
	NEXT
	FWRITE(m.lnFHSh,[</row>]+CHR(10))
ENDIF

lcSetDec = SET("Decimals")
SET DECIMALS TO 13
IF m.llMemoAsComment AND m.llMemos
	lnCurComm = 0
ENDIF
SELECT (m.cCur)
DO CASE
CASE m.llMemos AND m.lnLenIdx < m.lnLenStr
	SCAN 
		lnCurRow = m.lnCurRow + 1
		lcCurRow = LTRIM(STR(m.lnCurRow))
		FWRITE(m.lnFHSh,[<row r="]+m.lcCurRow+[" spans="1:1" x14ac:dyDescent="0.25">])
		SCATTER MEMO TO lala
		FOR lnCurCol = 1 TO m.lnColsNo
			SET ORDER TO cStr IN (m.cStrings)
			lcValue = lala[m.laFields[m.lnCurCol,6]]
			IF ISNULL(m.lcValue)
				LOOP
			ENDIF
			lnType = m.laFields[m.lnCurCol,2]
			lnDec = m.laFields[m.lnCurCol,4]
			FWRITE(m.lnFHSh,[<c r="]+m.laFields[m.lnCurCol,5]+m.lcCurRow)
			IF m.lnType == 1
				lcValue = RTRIM(m.lcValue)
				IF EMPTY(m.lcValue)
					FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
				ELSE
					SELECT (m.cStrings)
					SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
					SCAN
						IF cStr == m.lcValue
							EXIT
						ENDIF
					ENDSCAN
					SET KEY TO
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
				ENDIF
				SELECT (m.cCur)
			ELSE
			IF m.lnType == 2
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,m.laFields[m.lnCurCol,3],m.lnDec))+[</v></c>])
			ELSE
			IF m.lnType == 3
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue))+[</v></c>])
			ELSE
			IF m.lnType == 4
				DO CASE
				CASE m.lcValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat11))+[</v></c>])
				CASE BETWEEN(m.lcValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat12))+[</v></c>])
				OTHERWISE
					lcValue = DTOC(m.lcValue)
					SELECT (m.cStrings)
					SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
					SCAN
						IF cStr == m.lcValue
							EXIT 
						ENDIF
					ENDSCAN
					SET KEY TO
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE
			IF m.lnType == 5
				ltTime = m.lcValue 
				ldValue = TTOD(m.lcValue)
				lnTime = (m.ltTime-DATETIME(YEAR(m.ltTime),MONTH(m.ltTime),DAY(m.ltTime),0,0,0))/(86400.0)
				DO CASE
				CASE m.ldValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat11))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				CASE BETWEEN(m.ldValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat12))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				OTHERWISE
					lcValue = TTOC(m.lcValue)
					SELECT (m.cStrings)
					SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
					SCAN
						IF cStr == m.lcValue
							EXIT
						ENDIF
					ENDSCAN
					SET KEY TO
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE
			IF m.lnType == 6
				FWRITE(m.lnFHSh,[" t="b"><v>]+IIF(m.lcValue ,[1],[0])+[</v></c>])
			ELSE
			IF m.lnType == 7
				FWRITE(m.lnFHSh,[" s="4"><v>]+LTRIM(STR(m.lcValue,21,4))+[</v></c>])
			ELSE
			IF m.lnType == 8
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,21,m.lnDec))+[</v></c>])
			ELSE
			IF m.lnType == 9
				lcValue = RTRIM(m.lcValue)
				IF m.llMemoAsComment
					lnCurComm = m.lnCurComm + 1
					IF EMPTY(m.lcValue)
						FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
					ELSE
						FWRITE(m.lnFHSh,IIF(m.llCR,[" s="3],[])+[" t="s"><v>0</v></c>]) && Type "Memo" in sheet1
						FWRITE(m.lnFHCo,[<comment authorId="0" ref="] + m.laFields[m.lnCurCol,5]+m.lcCurRow + [">]) && comments1
							FWRITE(m.lnFHCo,[<text>])
								FWRITE(m.lnFHCo,[<r>])
									FWRITE(m.lnFHCo,[<rPr>])
										FWRITE(m.lnFHCo,[<sz val="9"/>])
										FWRITE(m.lnFHCo,[<color indexed="81"/>])
										FWRITE(m.lnFHCo,[<rFont val="Tahoma"/>])
										FWRITE(m.lnFHCo,[<family val="2"/>])
										FWRITE(m.lnFHCo,[<charset val="238"/>])
									FWRITE(m.lnFHCo,[</rPr>])
									FWRITE(m.lnFHCo,[<t>] + m.lcValue + [</t>])
								FWRITE(m.lnFHCo,[</r>])
							FWRITE(m.lnFHCo,[</text>])
						FWRITE(m.lnFHCo,[</comment>])
						
						
						FWRITE(m.lnFHDr,[<v:shape id="_x] + PADL(LTRIM(STR(m.lnCurComm)),10,"0") + [" o:insetmode="auto" fillcolor="#ffffe1" style="position:absolute; margin-left:] + ;
								LTRIM(STR(59.25 + 48 * (m.lnCurCol - 1))) + [pt;margin-top:] + LTRIM(STR(1.5 + 15 * (m.lnCurRow - 1))) + [pt;width:108pt;height:59.25pt;z-index:] + ;
								LTRIM(STR(m.lnCurComm)) + [; visibility:hidden" type="#_x0000_t202">])
							FWRITE(m.lnFHDr,[<v:fill color2="#ffffe1"/>])
							FWRITE(m.lnFHDr,[<v:shadow obscured="t" color="black" on="t"/>])
							FWRITE(m.lnFHDr,[<v:path o:connecttype="none"/>])
							FWRITE(m.lnFHDr,[<v:textbox style="mso-direction-alt:auto">])
								FWRITE(m.lnFHDr,[<div style="text-align:left"/>])
							FWRITE(m.lnFHDr,[</v:textbox>])
							FWRITE(m.lnFHDr,[<x:ClientData ObjectType="Note">])
								FWRITE(m.lnFHDr,[<x:MoveWithCells/>])
								FWRITE(m.lnFHDr,[<x:SizeWithCells/>])
								FWRITE(m.lnFHDr,[<x:Anchor> ] + LTRIM(STR(m.lnCurRow)) + [, 15, ] + LTRIM(STR(m.lnCurRow - 1)) + [, 2, ] + LTRIM(STR(m.lnCurRow + 2)) + [, 31, ] + LTRIM(STR(m.lnCurRow + 3)) + [, 1</x:Anchor>])
								FWRITE(m.lnFHDr,[<x:AutoFill>False</x:AutoFill>])
								FWRITE(m.lnFHDr,[<x:Row>] + LTRIM(STR(m.lnCurRow - 1)) + [</x:Row>])
								FWRITE(m.lnFHDr,[<x:Column>] + LTRIM(STR(m.lnCurCol - 1)) + [</x:Column>])
							FWRITE(m.lnFHDr,[</x:ClientData>])
						FWRITE(m.lnFHDr,[</v:shape>])
					ENDIF
				ELSE
					IF EMPTY(m.lcValue)
						FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
					ELSE
						llCR=AT(CHR(13),m.lcValue)>0 or AT(CHR(10),m.lcValue)>0
						SELECT (m.cStrings)
						SET ORDER TO cM
						SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
						lnII = 0
						SCAN
							IF cM == m.lcValue
								lnII = ii
								EXIT
							ENDIF
						ENDSCAN
						SET KEY TO
						IF lnII > 0
							FWRITE(m.lnFHSh,IIF(m.llCR,[" s="3],[])+[" t="s"><v>]+LTRIM(STR(m.lnII))+[</v></c>])
						ELSE
							FWRITE(m.lnFHStr,[<si><t>]+htmspec(m.lcValue)+[</t></si>])
							INSERT INTO (m.cStrings) (cM) VALUES (m.lcValue)
							SELECT MAX(ii) as ii FROM (m.cStrings) INTO CURSOR (m.cMax)
							SELECT (m.cMax)
							lnII = ii
							FWRITE(m.lnFHSh,IIF(m.llCR,[" s="3],[])+[" t="s"><v>]+LTRIM(STR(m.lnII))+[</v></c>])
						ENDIF
					ENDIF
				ENDIF
				SELECT (m.cCur)
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
		NEXT
		FWRITE(m.lnFHSh,[</row>]+CHR(10))
	ENDSCAN
CASE m.llMemos AND m.lnLenIdx >= m.lnLenStr
	SCAN
		lnCurRow = m.lnCurRow + 1
		lcCurRow = LTRIM(STR(m.lnCurRow))
		FWRITE(m.lnFHSh,[<row r="]+m.lcCurRow+[" spans="1:1" x14ac:dyDescent="0.25">])
		SCATTER MEMO TO lala
		FOR lnCurCol = 1 TO m.lnColsNo
			SET ORDER TO cStr IN (m.cStrings)
			lcValue = lala[m.laFields[m.lnCurCol,6]]
			IF ISNULL(m.lcValue)
				LOOP
			ENDIF
			lnType = m.laFields[m.lnCurCol,2]
			lnDec = m.laFields[m.lnCurCol,4]
			FWRITE(m.lnFHSh,[<c r="]+m.laFields[m.lnCurCol,5]+m.lcCurRow)
			IF m.lnType = 1
				lcValue = RTRIM(m.lcValue)
				IF EMPTY(m.lcValue)
					FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
				ELSE
					SELECT (m.cStrings)
					SEEK m.lcValue
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
				ENDIF
				SELECT (m.cCur)
			ELSE
			IF m.lnType = 2
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,m.laFields[m.lnCurCol,3],m.lnDec))+[</v></c>])
			ELSE 
			IF m.lnType = 3
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue))+[</v></c>])
			ELSE 
			IF m.lnType = 4
				DO CASE
				CASE m.lcValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat11))+[</v></c>])
				CASE BETWEEN(m.lcValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat12))+[</v></c>])
				OTHERWISE
					lcValue = DTOC(m.lcValue)
					SELECT (m.cStrings)
					SEEK m.lcValue
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE 
			IF m.lnType = 5
				ltTime = m.lcValue 
				ldValue = TTOD(m.lcValue)
				lnTime = (m.ltTime-DATETIME(YEAR(m.ltTime),MONTH(m.ltTime),DAY(m.ltTime),0,0,0))/(86400.0)
				DO CASE
				CASE m.ldValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat11))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				CASE BETWEEN(m.ldValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat12))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				OTHERWISE
					lcValue = TTOC(m.lcValue)
					SELECT (m.cStrings)
					SEEK m.lcValue
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE 
			IF m.lnType = 6
				FWRITE(m.lnFHSh,[" t="b"><v>]+IIF(m.lcValue ,[1],[0])+[</v></c>])
			ELSE 
			IF m.lnType = 7
				FWRITE(m.lnFHSh,[" s="4"><v>]+LTRIM(STR(m.lcValue,21,4))+[</v></c>])
			ELSE 
			IF m.lnType = 8
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,21,m.lnDec))+[</v></c>])
			ELSE
			IF m.lnType = 9
				lcValue = RTRIM(m.lcValue)
				IF m.llMemoAsComment
					lnCurComm = m.lnCurComm + 1
					IF EMPTY(m.lcValue)
						FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
					ELSE
						FWRITE(m.lnFHSh,IIF(m.llCR,[" s="3],[])+[" t="s"><v>0</v></c>]) && Tyoe "Memo" in sheet1
						FWRITE(m.lnFHCo,[<comment authorId="0" ref="] + m.laFields[m.lnCurCol,5]+m.lcCurRow + [">]) && comments1
							FWRITE(m.lnFHCo,[<text>])
								FWRITE(m.lnFHCo,[<r>])
									FWRITE(m.lnFHCo,[<rPr>])
										FWRITE(m.lnFHCo,[<sz val="9"/>])
										FWRITE(m.lnFHCo,[<color indexed="81"/>])
										FWRITE(m.lnFHCo,[<rFont val="Tahoma"/>])
										FWRITE(m.lnFHCo,[<family val="2"/>])
										FWRITE(m.lnFHCo,[<charset val="238"/>])
									FWRITE(m.lnFHCo,[</rPr>])
									FWRITE(m.lnFHCo,[<t>] + m.lcValue + [</t>])
								FWRITE(m.lnFHCo,[</r>])
							FWRITE(m.lnFHCo,[</text>])
						FWRITE(m.lnFHCo,[</comment>])
						
						
						FWRITE(m.lnFHDr,[<v:shape id="_x] + PADL(LTRIM(STR(m.lnCurComm)),10,"0") + [" o:insetmode="auto" fillcolor="#ffffe1" style="position:absolute; margin-left:] + ;
								LTRIM(STR(59.25 + 48 * (m.lnCurCol - 1))) + [pt;margin-top:] + LTRIM(STR(1.5 + 15 * (m.lnCurRow - 1))) + [pt;width:108pt;height:59.25pt;z-index:] + ;
								LTRIM(STR(m.lnCurComm)) + [; visibility:hidden" type="#_x0000_t202">])
							FWRITE(m.lnFHDr,[<v:fill color2="#ffffe1"/>])
							FWRITE(m.lnFHDr,[<v:shadow obscured="t" color="black" on="t"/>])
							FWRITE(m.lnFHDr,[<v:path o:connecttype="none"/>])
							FWRITE(m.lnFHDr,[<v:textbox style="mso-direction-alt:auto">])
								FWRITE(m.lnFHDr,[<div style="text-align:left"/>])
							FWRITE(m.lnFHDr,[</v:textbox>])
							FWRITE(m.lnFHDr,[<x:ClientData ObjectType="Note">])
								FWRITE(m.lnFHDr,[<x:MoveWithCells/>])
								FWRITE(m.lnFHDr,[<x:SizeWithCells/>])
								FWRITE(m.lnFHDr,[<x:Anchor> ] + LTRIM(STR(m.lnCurRow)) + [, 15, ] + LTRIM(STR(m.lnCurRow - 1)) + [, 2, ] + LTRIM(STR(m.lnCurRow + 2)) + [, 31, ] + LTRIM(STR(m.lnCurRow + 3)) + [, 1</x:Anchor>])
								FWRITE(m.lnFHDr,[<x:AutoFill>False</x:AutoFill>])
								FWRITE(m.lnFHDr,[<x:Row>] + LTRIM(STR(m.lnCurRow - 1)) + [</x:Row>])
								FWRITE(m.lnFHDr,[<x:Column>] + LTRIM(STR(m.lnCurCol - 1)) + [</x:Column>])
							FWRITE(m.lnFHDr,[</x:ClientData>])
						FWRITE(m.lnFHDr,[</v:shape>])
					ENDIF
				ELSE
					IF EMPTY(m.lcValue)
						FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
					ELSE
						llCR=AT(CHR(13),m.lcValue)>0 or AT(CHR(10),m.lcValue)>0
						SELECT (m.cStrings)
						SET ORDER TO cM
						SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
						lnII = 0
						SCAN
							IF cM == m.lcValue
								lnII = ii
								EXIT
							ENDIF
						ENDSCAN
						SET KEY TO
						IF lnII > 0
							FWRITE(m.lnFHSh,IIF(m.llCR,[" s="3],[])+[" t="s"><v>]+LTRIM(STR(m.lnII))+[</v></c>])
						ELSE
							FWRITE(m.lnFHStr,[<si><t>]+htmspec(m.lcValue)+[</t></si>])
							INSERT INTO (m.cStrings) (cM) VALUES (m.lcValue)
							SELECT MAX(ii) as ii FROM (m.cStrings) INTO CURSOR (m.cMax)
							SELECT (m.cMax)
							lnII = ii
							FWRITE(m.lnFHSh,IIF(m.llCR,[" s="3],[])+[" t="s"><v>]+LTRIM(STR(m.lnII))+[</v></c>])
						ENDIF
					ENDIF
				ENDIF
				SELECT (m.cCur)
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
		NEXT
		FWRITE(m.lnFHSh,[</row>]+CHR(10))
	ENDSCAN
CASE !m.llMemos AND m.lnLenIdx < m.lnLenStr
	SET ORDER TO cStr IN (m.cStrings)
	SCAN 
		lnCurRow = m.lnCurRow + 1
		lcCurRow = LTRIM(STR(m.lnCurRow))
		FWRITE(m.lnFHSh,[<row r="]+m.lcCurRow+[" spans="1:1" x14ac:dyDescent="0.25">])
		SCATTER TO lala
		FOR lnCurCol = 1 TO m.lnColsNo
			lcValue = lala[m.laFields[m.lnCurCol,6]]
			IF ISNULL(m.lcValue)
				LOOP
			ENDIF
			lnType = m.laFields[m.lnCurCol,2]
			lnDec = m.laFields[m.lnCurCol,4]
			FWRITE(m.lnFHSh,[<c r="]+m.laFields[m.lnCurCol,5]+m.lcCurRow)
			IF m.lnType = 1
				lcValue = RTRIM(m.lcValue)
				IF EMPTY(m.lcValue)
					FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
				ELSE
					SELECT (m.cStrings)
					SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
					SCAN
						IF cStr == m.lcValue
							EXIT
						ENDIF
					ENDSCAN
					SET KEY TO
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
				ENDIF
				SELECT (m.cCur)
			ELSE
			IF m.lnType = 2
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,m.laFields[m.lnCurCol,3],m.lnDec))+[</v></c>])
			ELSE
			IF m.lnType = 3
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue))+[</v></c>])
			ELSE
			IF m.lnType = 4
				DO CASE
				CASE m.lcValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat11))+[</v></c>])
				CASE BETWEEN(m.lcValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat12))+[</v></c>])
				OTHERWISE
					lcValue = DTOC(m.lcValue)
					SELECT (m.cStrings)
					SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
					SCAN
						IF cStr == m.lcValue
							EXIT 
						ENDIF
					ENDSCAN
					SET KEY TO
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE
			IF m.lnType = 5
				ltTime = m.lcValue 
				ldValue = TTOD(m.lcValue)
				lnTime = (m.ltTime-DATETIME(YEAR(m.ltTime),MONTH(m.ltTime),DAY(m.ltTime),0,0,0))/(86400.0)
				DO CASE
				CASE m.ldValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat11))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				CASE BETWEEN(m.ldValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat12))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				OTHERWISE
					lcValue = TTOC(m.lcValue)
					SELECT (m.cStrings)
					SET KEY TO LEFT(m.lcValue,m.lnLenIdx)
					SCAN
						IF cStr == m.lcValue
							EXIT
						ENDIF
					ENDSCAN
					SET KEY TO
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE
			IF m.lnType = 6
				FWRITE(m.lnFHSh,[" t="b"><v>]+IIF(m.lcValue ,[1],[0])+[</v></c>])
			ELSE
			IF m.lnType = 7
				FWRITE(m.lnFHSh,[" s="4"><v>]+LTRIM(STR(m.lcValue,21,4))+[</v></c>])
			ELSE
			IF m.lnType = 8
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,21,m.lnDec))+[</v></c>])
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
		NEXT
		FWRITE(m.lnFHSh,[</row>]+CHR(10))
	ENDSCAN
CASE !m.llMemos AND m.lnLenIdx >= m.lnLenStr
	SET ORDER TO cStr IN (m.cStrings)
	SCAN 
		lnCurRow = m.lnCurRow + 1
		lcCurRow = LTRIM(STR(m.lnCurRow))
		FWRITE(m.lnFHSh,[<row r="]+m.lcCurRow+[" spans="1:1" x14ac:dyDescent="0.25">])
		SCATTER TO lala
		FOR lnCurCol = 1 TO m.lnColsNo
			lcValue = lala[m.laFields[m.lnCurCol,6]]
			IF ISNULL(m.lcValue)
				LOOP
			ENDIF
			lnType = m.laFields[m.lnCurCol,2]
			lnDec = m.laFields[m.lnCurCol,4]
			FWRITE(m.lnFHSh,[<c r="]+m.laFields[m.lnCurCol,5]+m.lcCurRow)
			IF m.lnType = 1
				lcValue = RTRIM(m.lcValue)
				IF EMPTY(m.lcValue)
					FWRITE(m.lnFHSh,[" t="s"></c>]) && Empty cell
				ELSE
					SELECT (m.cStrings)
					SEEK m.lcValue
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
				ENDIF
				SELECT (m.cCur)
			ELSE
			IF m.lnType = 2
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,m.laFields[m.lnCurCol,3],m.lnDec))+[</v></c>])
			ELSE
			IF m.lnType = 3
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue))+[</v></c>])
			ELSE
			IF m.lnType = 4
				DO CASE
				CASE m.lcValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat11))+[</v></c>])
				CASE BETWEEN(m.lcValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="1"><v>]+LTRIM(STR(m.lcValue - m.ldDat12))+[</v></c>])
				OTHERWISE
					lcValue = DTOC(m.lcValue)
					SELECT (m.cStrings)
					SEEK m.lcValue
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE
			IF m.lnType = 5
				ltTime = m.lcValue 
				ldValue = TTOD(m.lcValue)
				lnTime = (m.ltTime-DATETIME(YEAR(m.ltTime),MONTH(m.ltTime),DAY(m.ltTime),0,0,0))/(86400.0)
				DO CASE
				CASE m.ldValue >= m.ldDat01
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat11))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				CASE BETWEEN(m.ldValue,m.ldDat02,m.ldDat03)
					FWRITE(m.lnFHSh,[" s="2"><v>]+LTRIM(STR(m.ldValue - m.ldDat12))+SUBSTR(TRANSFORM(m.lnTime),2,14)+[</v></c>])
				OTHERWISE
					lcValue = TTOC(m.lcValue)
					SELECT (m.cStrings)
					SEEK m.lcValue
					FWRITE(m.lnFHSh,[" t="s"><v>]+LTRIM(STR(II))+[</v></c>])
					SELECT (m.cCur)
				ENDCASE
			ELSE
			IF m.lnType = 6
				FWRITE(m.lnFHSh,[" t="b"><v>]+IIF(m.lcValue ,[1],[0])+[</v></c>])
			ELSE
			IF m.lnType = 7
				FWRITE(m.lnFHSh,[" s="4"><v>]+LTRIM(STR(m.lcValue,21,4))+[</v></c>])
			ELSE
			IF m.lnType = 8
				FWRITE(m.lnFHSh,["><v>]+LTRIM(STR(m.lcValue,21,m.lnDec))+[</v></c>])
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
			ENDIF
		NEXT
		FWRITE(m.lnFHSh,[</row>]+CHR(10))
	ENDSCAN
ENDCASE

SET DECIMALS TO &lcSetDec 

* End sheet1
FWRITE(m.lnFHSh,[</sheetData>])
FWRITE(m.lnFHSh,[<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>])
IF m.llMemoAsComment AND m.llMemos
	FWRITE(m.lnFHSh,[<legacyDrawing r:id="rId6"/>])
ENDIF
FWRITE(m.lnFHSh,[</worksheet>])
FCLOSE(m.lnFHSh)

* End sharedStrings
FWRITE(m.lnFHStr,[</sst>])
FSEEK(m.lnFHStr,55+1+LEN([<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="]))
FWRITE(m.lnFHStr,LTRIM(STR(m.lnTotal))+[" uniqueCount="]+LTRIM(STR(RECCOUNT(m.cStrings)))+[">])
FCLOSE(m.lnFHStr)

IF m.llMemoAsComment AND m.llMemos
* End comment1
	FWRITE(m.lnFHCo,[</commentList>])
	FWRITE(m.lnFHCo,[</comments>])
	FCLOSE(m.lnFHCo)
* End comment1
	FWRITE(m.lnFHDr,[</xml>])
	FCLOSE(m.lnFHDr)
ENDIF

*****

lcSource = m.lcMyPath + m.lcDir &&"<< fully qualified path name to some folder >>"
lcZipFileName = m.lcMyPath + FORCEEXT(JUSTFNAME(m.lcFileName),'zip') &&"<< fully qualified path name to some zip file >>"

TRY
	IF FILE(m.lcZipFileName)
		ERASE (m.lcZipFileName)
	ENDIF
CATCH TO m.loerr
ENDTRY

TRY
	IF FILE(m.lcFileName)
		ERASE (m.lcFileName)
	ENDIF
CATCH TO m.loerr
ENDTRY

STRTOFILE(CHR( 80 )+CHR( 75 )+CHR( 5 )+CHR( 6 )+REPLICATE( CHR(0), 18 ), m.lcZipFileName)
oShell = CREATEOBJECT("shell.application")
oFolder = m.oShell.NameSpace( m.lcSource ).items

llBelow7 = OS(3)<'6' OR OS(3)='6' AND OS(4)<'1'
IF m.llBelow7 && Win XP
	TRY
		FOR EACH ofile IN m.oFolder
			lnCountbefore = m.oShell.NameSpace( m.lcSource ).items.count
			oShell.NameSpace( m.lcZipFileName ).movehere( m.ofile )
			sleep(100)
		ENDFOR
	CATCH TO loErr
	ENDTRY

	llErr = .T.
	DO WHILE llErr
		TRY
			llErr = .F.
			RENAME (m.lcZipFileName) TO (FORCEEXT(m.lcZipFileName,"xlsx"))
		CATCH
			llErr = .T.
			sleep(100)
		ENDTRY
	ENDDO
	DO cleanup WITH m.lcDir,m.llMemoAsComment AND m.llMemos
ELSE && WIN 7
	TRY
		FOR EACH ofile IN m.oFolder
			lnCountbefore = m.oShell.NameSpace( m.lcSource ).items.count
			oShell.NameSpace( m.lcZipFileName ).movehere( m.ofile )
			sleep(100)
			DO WHILE m.lnCountbefore = m.oShell.NameSpace( m.lcSource ).items.count
				sleep(100)
			ENDDO
		ENDFOR
	CATCH TO loErr
	ENDTRY

	TRY 
		RD (m.lcDir)
	CATCH TO m.loErr
	ENDTRY

	RENAME (m.lcZipFileName) TO (FORCEEXT(m.lcZipFileName,"xlsx"))
ENDIF

*ShellExecute(0,"Open",FORCEEXT(m.lcZipFileName,"xlsx"),"","",1)

SET TALK &lSetTalk
SET POINT TO lcSetPoint

**********************
* Special characters *
**********************
FUNCTION htmspec
	LPARAMETERS cStr
	LOCAL lni,lcStrF,lcChar,lnChar,lcStrF2
	lcStrF = m.cStr
	IF AT(CHR(38),m.lcStrF)>0
		lcStrF = STRTRAN(m.lcStrF,CHR(38),'&amp;')
	ENDIF
	IF AT('>',m.lcStrF)>0
		lcStrF = STRTRAN(m.lcStrF,'>','&gt;')
	ENDIF
	IF AT('<',m.lcStrF)>0
		lcStrF = STRTRAN(m.lcStrF,'<','&lt;')
	ENDIF
	IF AT('"',m.lcStrF)>0
		lcStrF = STRTRAN(m.lcStrF,'"','&quot;')
	ENDIF
	IF AT("'",m.lcStrF)>0
		lcStrF = STRTRAN(m.lcStrF,"'",'&apos;')
	ENDIF

	lcStrF2 = ''
	FOR lni=1 TO LEN(m.lcStrF)
		lcChar = SUBSTR(m.lcStrF,m.lni,1)
		lnChar = ASC(m.lcChar)
		lcStrF2 = m.lcStrF2 + IIF(m.lnChar < 128 , m.lcChar , [&#]+STR(m.lnChar,3)+[;])
	NEXT
	RETURN m.lcStrF2
ENDFUNC

*******************
* Currency symbol *
*******************
FUNCTION getcurr
LOCAL nretval,LpLCData,cchData,llLeftCurr,lcCurr,lni
llLeftCurr = SET("Currency")=="LEFT"
LpLCData = space(255)
cchData = LEN(LpLCData)
IF OS(3)<'6' && Win XP
	nretval = GetLocaleInfo(1024, 0x14, @LpLCData, cchData) && get symbol
	lcCurr = LEFT(ALLTRIM(m.LpLCData) , m.nretval - 1)
	nretval = GetLocaleInfo(1024, 0x1B, @LpLCData, cchData)
	LpLCData = LEFT(ALLTRIM(m.LpLCData) , m.nretval - 1) && get position
	IF m.lcCurr == CHR(128)
		lcCurr = [&quot;&#8364;&quot;]
	ELSE
		lcCurr = [&quot;]+htmspec(m.lcCurr)+[&quot;]
	ENDIF
ELSE && Win Vista +  
	nretval = GetLocaleInfoEx(Null, 0x14, @LpLCData, cchData) && get symbol
	lcCurr = [&quot;]
	FOR lni = 1 TO m.nretval - 1
		lcCurr = m.lcCurr + [&#x] + RIGHT(TRANSFORM(ASC(SUBSTR(m.LpLCData,2*m.lni)),"@0"),2) + RIGHT(TRANSFORM(ASC(SUBSTR(m.LpLCData,2*m.lni - 1)),"@0"),2) + [;]
	NEXT
	lcCurr = m.lcCurr + [&quot;]
	
	nretval = GetLocaleInfoEx(Null, 0x1B, @LpLCData, cchData) && get position
	LpLCData = LEFT(m.LpLCData,1)
ENDIF
DO CASE
CASE LpLCData = "0"
	lcCurr = m.lcCurr  + [#,##0.00]
CASE LpLCData = "1"
	lcCurr = [#,##0.00] + m.lcCurr
CASE LpLCData = "2"
	lcCurr = m.lcCurr + [\ #,##0.00]
CASE LpLCData = "3"
	lcCurr = [#,##0.00\ ] + m.lcCurr
ENDCASE
RETURN m.lcCurr

**********************
* Generate temp dirs *
**********************
FUNCTION gen_dirs
	LPARAMETERS llMemoAsComment
	LOCAL lcDir
	lcDir=ADDBS(SYS(2015))
	MD (m.lcDir)
	MD (ADDBS(m.lcDir+[_rels]))
	MD (ADDBS(m.lcDir+[docProps]))
	MD (ADDBS(m.lcDir+[xl]))
	MD (ADDBS(ADDBS(m.lcDir+[xl])+[_rels]))
	MD (ADDBS(ADDBS(m.lcDir+[xl])+[worksheets]))
	IF m.llMemoAsComment
		MD (ADDBS(ADDBS(m.lcDir+[xl])+[drawings]))
		MD (ADDBS(ADDBS(ADDBS(m.lcDir+[xl])+[worksheets])+[_rels]))
	ENDIF
RETURN m.lcDir

**********************
* For OS below Win 7 *
**********************
PROCEDURE cleanup
	LPARAMETERS lcDir,llMemoAsComment
	LOCAL lSetSafety
	lSetSafety = SET("Safety")
	SET SAFETY OFF
	ERASE (ADDBS(ADDBS(m.lcDir+[xl])+[_rels]) + "*.*")
	RD (ADDBS(ADDBS(m.lcDir+[xl])+[_rels]))
	IF m.llMemoAsComment
		ERASE (ADDBS(ADDBS(m.lcDir+[xl])+[drawings]) + "*.*")
		RD (ADDBS(ADDBS(m.lcDir+[xl])+[drawings]))
		ERASE (ADDBS(ADDBS(ADDBS(m.lcDir+[xl])+[worksheets])+[_rels]) + "*.*")
		RD (ADDBS(ADDBS(ADDBS(m.lcDir+[xl])+[worksheets])+[_rels]))
	ENDIF
	ERASE (ADDBS(ADDBS(m.lcDir+[xl])+[worksheets]) + "*.*")
	RD (ADDBS(ADDBS(m.lcDir+[xl])+[worksheets]))
	ERASE (ADDBS(m.lcDir+[xl]) + "*.*")
	RD (ADDBS(m.lcDir+[xl]))
	ERASE (ADDBS(m.lcDir+[docProps]) + "*.*")
	RD (ADDBS(m.lcDir+[docProps]))
	ERASE (ADDBS(m.lcDir+[_rels]) + "*.*")
	RD (ADDBS(m.lcDir+[_rels]))
	ERASE (m.lcDir + "*.*")
	RD (m.lcDir)
	SET SAFETY &lSetSafety
ENDPROC

********************************
* Generate [Content_Types].xml *
********************************
PROCEDURE gen_Content_Types
	LPARAMETERS lcDir,llMemoAsComment
	LOCAL lnF
	lnF = FCREATE(m.lcDir+"[Content_Types].xml")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create [Content_Types].xml',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>])
	FWRITE(m.lnF,[<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">])
	FWRITE(m.lnF,[<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>])
	FWRITE(m.lnF,[<Default Extension="xml" ContentType="application/xml"/>])
	IF m.llMemoAsComment
		FWRITE(m.lnF,[<Default ContentType="application/vnd.openxmlformats-officedocument.vmlDrawing" Extension="vml"/>])
		FWRITE(m.lnF,[<Override ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml" PartName="/xl/comments1.xml"/>])
	ENDIF
	FWRITE(m.lnF,[<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>])
	FWRITE(m.lnF,[<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>])
	FWRITE(m.lnF,[<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>])
	FWRITE(m.lnF,[<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>])
	FWRITE(m.lnF,[<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>])
	FWRITE(m.lnF,[<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>]+CHR(10))
	FWRITE(m.lnF,[</Types>])
	FCLOSE(m.lnF)
ENDPROC

***************************
* Generate _rels\rels.xml *
***************************
PROCEDURE gen_rels
	LPARAMETERS lcDir
	LOCAL lnF
	lnF = FCREATE(m.lcDir+".rels")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create .rels',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	FWRITE(m.lnF,[<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">])
	FWRITE(m.lnF,[<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>])
	FWRITE(m.lnF,[<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>])
	FWRITE(m.lnF,[<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>]+CHR(10))
	FWRITE(m.lnF,[</Relationships>])
	FCLOSE(m.lnF)
ENDPROC

*****************************
* Generate docProps\app.xml *
*****************************
PROCEDURE gen_app
	LPARAMETERS lcDir
	LOCAL lnF
	lnF = FCREATE(m.lcDir+"app.xml")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create app.xml',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	FWRITE(m.lnF,[<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">])
	FWRITE(m.lnF,[<Application>copytoxlsx</Application>])
	FWRITE(m.lnF,[<AppVersion>2.9100</AppVersion>])
	FWRITE(m.lnF,[</Properties>])
	FCLOSE(m.lnF)
ENDPROC

******************************
* Generate docProps\core.xml *
******************************
PROCEDURE gen_core
	LPARAMETERS lcDir
	LOCAL lnF
	lnF = FCREATE(m.lcDir+"core.xml")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create core.xml',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	FWRITE(m.lnF,[<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" ])
		FWRITE(m.lnF,[xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">])
	FWRITE(m.lnF,[<dc:creator>Vilhelm-Ion Praisach</dc:creator>])
	FWRITE(m.lnF,[<dcterms:created xsi:type="dcterms:W3CDTF">]+TTOC(DATETIME(),3)+[</dcterms:created>])
	FWRITE(m.lnF,[</cp:coreProperties>])
	FCLOSE(m.lnF)
ENDPROC

***************************************
* Generate xl\_rels\workbook.xml.rels *
***************************************
PROCEDURE gen_workbook
	LPARAMETERS lcDir,llMemoAsComment
	LOCAL lnF
	lnF = FCREATE(m.lcDir+"workbook.xml.rels")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create workbook.xml.rels',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	FWRITE(m.lnF,[<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">])
	FWRITE(m.lnF,[<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>])
	FWRITE(m.lnF,[<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>])
	IF m.llMemoAsComment
		FWRITE(m.lnF,[<Relationship Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments" Target="../comments1.xml"/>])
		FWRITE(m.lnF,[<Relationship Id="rId6" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/vmlDrawing" Target="../drawings/vmlDrawing1.vml"/>])
	ENDIF
	FWRITE(m.lnF,[<Relationship Id="rId4" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>]+CHR(10))
	FWRITE(m.lnF,[</Relationships>])
	FCLOSE(m.lnF)
ENDPROC

**************************
* Generate xl\styles.xml *
**************************
PROCEDURE gen_styles
	LPARAMETERS lcDir,lcCurr,llMemoAsComment
	LOCAL lnF
	lnF = FCREATE(m.lcDir+"styles.xml")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create styles.xml',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	FWRITE(m.lnF,[<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" ])
	  FWRITE(m.lnF,[mc:Ignorable="x14ac" xmlns:x14ac="http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac">])
	FWRITE(m.lnF,[<numFmts count="2">]) && currency
		FWRITE(m.lnF,[<numFmt numFmtId="164" formatCode="]+m.lcCurr+["/>])
		FWRITE(m.lnF,[<numFmt formatCode="dd/mm/yyyy\ hh:mm:ss" numFmtId="22"/>])
	FWRITE(m.lnF,[</numFmts>])
	IF m.llMemoAsComment
		FWRITE(m.lnF,[<fonts count="2" x14ac:knownFonts="1">])
	ELSE
		FWRITE(m.lnF,[<fonts count="1" x14ac:knownFonts="1">])
	ENDIF
		FWRITE(m.lnF,[<font><sz val="11"/>])
			FWRITE(m.lnF,[<name val="Calibri"/>])
		FWRITE(m.lnF,[</font>])
	IF m.llMemoAsComment
		FWRITE(m.lnF,[<font>])
			FWRITE(m.lnF,[<sz val="9"/>])
			FWRITE(m.lnF,[<color indexed="81"/>])
			FWRITE(m.lnF,[<name val="Tahoma"/>])
			FWRITE(m.lnF,[<family val="2"/>])
			FWRITE(m.lnF,[<charset val="238"/>])
		FWRITE(m.lnF,[</font>])
	ENDIF
	FWRITE(m.lnF,[</fonts>])
	FWRITE(m.lnF,[<fills count="1">])
		FWRITE(m.lnF,[<fill>])
			FWRITE(m.lnF,[<patternFill patternType="none"/>])
		FWRITE(m.lnF,[</fill>])
	FWRITE(m.lnF,[</fills>])
	FWRITE(m.lnF,[<borders count="1">])
		FWRITE(m.lnF,[<border>])
			FWRITE(m.lnF,[<left/><right/><top/><bottom/><diagonal/>])
		FWRITE(m.lnF,[</border>])
	FWRITE(m.lnF,[</borders>])
	FWRITE(m.lnF,[<cellXfs count="5">])
		FWRITE(m.lnF,[<xf numFmtId="0" fontId="0"/>]) && Number
		FWRITE(m.lnF,[<xf numFmtId="14" fontId="0" applyNumberFormat="1"/>]) && date
		FWRITE(m.lnF,[<xf numFmtId="22" fontId="0" applyNumberFormat="1"/>]) && time
		FWRITE(m.lnF,[<xf numFmtId="0" fontId="0" applyAlignment="1"><alignment wrapText="1"/></xf>]) && enter in memo
		FWRITE(m.lnF,[<xf numFmtId="164" fontId="0" applyNumberFormat="1"/>]) && currency
	FWRITE(m.lnF,[</cellXfs>])
	FWRITE(m.lnF,[</styleSheet>])
	FCLOSE(m.lnF)
ENDPROC

****************************
* Generate xl\workbook.xml *
****************************
PROCEDURE gen_workbook2
	LPARAMETERS lcDir
	LOCAL lnF
	lnF = FCREATE(m.lcDir+"workbook.xml")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create workbook.xml',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	FWRITE(m.lnF,[<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">])
		FWRITE(m.lnF,[<sheets>])
			FWRITE(m.lnF,[<sheet name="Sheet1" sheetId="1" r:id="rId1"/>])
		FWRITE(m.lnF,[</sheets>])
	FWRITE(m.lnF,[</workbook>])
	FCLOSE(m.lnF)
ENDPROC


************************************************
* Generate xl\worksheets\_rels\sheet1.xml.rels *
************************************************
PROCEDURE gen_workbook3
	LPARAMETERS lcDir
	LOCAL lnF
	lnF = FCREATE(m.lcDir+"sheet1.xml.rels")
	IF m.lnF < 0
		MESSAGEBOX('Cannot create sheet1.xml.rels',16,'Abort')
		RETURN TO MASTER
	ENDIF
	FWRITE(m.lnF,[<?xml version="1.0" encoding="UTF-8" standalone="yes"?>]+CHR(10))
	
	FWRITE(m.lnF,[<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">])
		FWRITE(m.lnF,[<Relationship Id="rId5" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments" Target="../comments1.xml"/>])
		FWRITE(m.lnF,[<Relationship Id="rId6" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/vmlDrawing" Target="../drawings/vmlDrawing1.vml"/>])
	FWRITE(m.lnF,[</Relationships>])
	FCLOSE(m.lnF)
ENDPROC
