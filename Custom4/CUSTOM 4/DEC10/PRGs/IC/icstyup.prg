*:***************************************************************************************************************************
*: Program file  : ICSTYUP.PRG
*: Program desc. : Update excel sheet with the current OTS information
*: Module        : IC
*: System        : Aria4XP
*: Developer     : Hesham Elmasry (HES)
*! Ticket #      : T20090415.0019
*! Tracking #    : *C201194
*!***************************************************************************************************************************
*: Passed Parameters  : None
*!***************************************************************************************************************************
* Modifications
*C201194,4 TMI 10/04/2009 Export all lines even if the SKU is not valid in aria, but the line is exported intacted
*B609099,1 HES 12/01/2009 entered SKU codes in a system but was not able to save and transfer this file Under “output sheet directory” [T20091023.0009]
*!***************************************************************************************************************************

*! Previewing The OG
lcExpr = gfOpGrid('ICSTYUP',.T. )
RETURN
STORE '' TO lcFileName , lcRpSphis , lcRpSpdir , lcCatSel 

*********************************************************************

FUNCTION lfMain 
*!*** Open tables
STORE 0 TO lnClrLen , lnClrPos 

IF !USED('Codes')
  =gfOpenTable(oAriaApplication.DataDir+'Codes','CCODE_NO','SH')
ENDIF

IF !USED('STYLE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF

IF !USED('STYDYE')
  =gfOpenTable(oAriaApplication.DataDir+'STYDYE','STYDYE','SH')
ENDIF

*C201194,1 TMI 09/09/2009 12:36:38 PM [Start] open spck_lin file
IF !USED('SPCK_LIN')
  *C201194,3 TMI 09/27/2009 11:48:05 AM [Start] open with another tag
  *=gfOpenTable(oAriaApplication.DataDir+'SPCK_LIN','SPCKLINS','SH')    && TYPE+ACCOUNT+STYLE+PACK_ID
  =gfOpenTable(oAriaApplication.DataDir+'SPCK_LIN','SPCK_LIN','SH')   && TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT
  *C201194,3 TMI 09/27/2009 11:48:07 AM [End  ] 
ENDIF
*C201194,1 TMI 09/09/2009 12:36:38 PM [End  ] 

*! Global variabls 
IF lfProcCsv()   
ENDIF 

RETURN .F.  && do not close the OG to allow to create new SO
*!--- end of main function

*!*************************************************************
*! Name      : lfProcCsv
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : Check xls Sheet For errors 
*!*************************************************************
*! Example   : =lfProcCsv()
*!*************************************************************
FUNCTION lfProcCsv

=lfCreaTemp()

SELECT(lcTmpXls)
APPEND FROM (lcCsvFile) DELIMITED WITH CHARACTER ';'

SELECT(lcTmpXls)
SET ORDER TO 
LOCATE 
*C201194,3 TMI 09/30/2009 01:53:16 PM [Start] trim the lcRpSpdir variable
lcRpSpdir = ALLTRIM(lcRpSpdir)
*C201194,3 TMI 09/30/2009 01:53:17 PM [End  ] 
IF !FILE(lcRpSpdir+ALLTRIM(lcRpOutNm)+'.CSV')
  lnH = FCREATE(lcRpSpdir+ALLTRIM(lcRpOutNm)+'.CSV') 
ELSE
  lnRes = MESSAGEBOX('The File '+ ALLTRIM(lcRpOutNm) +'.CSV Already Exists. Do you want to Replace the Existing File ?',561,'Already Exists!')
  IF lnRes = 2
    MESSAGEBOX('Process Cancelled by User.',576,'Cancelled.')
    RETURN
  ELSE 
    lnH = FCREATE(lcRpSpdir+ALLTRIM(lcRpOutNm)+'.CSV')
  ENDIF 
ENDIF   
  
llDone  = .F.
*C201194,1 TMI 09/09/2009 01:36:34 PM [Start] NOT NEEDED
*lnRecNo = 0
*C201194,1 TMI 09/09/2009 01:36:35 PM [End  ] 

*C201194,1 TMI 09/09/2009 01:36:11 PM [Start] 
*SCAN FOR RECNO() < 3
SCAN REST WHILE RECNO() < 3
  *C201194,1 TMI 09/09/2009 01:36:16 PM [End  ] 
  IF RECNO() = 1
    lcTx = ALLTRIM(PRODUCTID)     
    FPUTS(lnh,lcTx)
  ELSE
    *C201194,1 TMI 09/09/2009 11:51:22 AM [Start] move this code to a function
    *lcTx = ALLTRIM(PRODUCTID) +';'+ALLTRIM(PRODUCTCOD)+';'+ALLTRIM(PRODUCT)+';'+ALLTRIM(VARIANTID)+';' + ;
           ALLTRIM(VARIANTCOD)+';'+ALLTRIM(WEIGHT)    +';'+ALLTRIM(PRICE)  +';'+ALLTRIM(AVAIL)    +';' + ;
           ALLTRIM(DEFAULT)   +';'+ALLTRIM(IMAGE)     +';'+ALLTRIM(CLASS)  +';'+ALLTRIM(OPTION)   +';' + ;
           ALLTRIM(COLCODE)   
    *C201194,3 TMI 09/30/2009 01:42:29 PM [Start] put the header directly, do not use the lfGetTx function 
    *lcTx = lfGetTx()
    lcTx = '!PRODUCTID;!PRODUCTCODE;!PRODUCT;!VARIANTID;!VARIANTCODE;!WEIGHT;!PRICE;!AVAIL;!DEFAULT;!IMAGE;!CLASS;!OPTION'
    *C201194,3 TMI 09/30/2009 01:42:30 PM [End  ] 
    *C201194,1 TMI 09/09/2009 11:51:31 AM [End  ] 
             
    FPUTS(lnh,lcTx)
  ENDIF 
  *C201194,1 TMI 09/09/2009 01:36:47 PM [Start] NOT NEEDED
  *lnRecNo = RECNO() +1 
  *C201194,1 TMI 09/09/2009 01:36:49 PM [End  ] 
ENDSCAN 

*C201194,1 TMI 09/09/2009 01:36:52 PM [Start] NOT NEEDED
*GOTO lnRecNo
*C201194,1 TMI 09/09/2009 01:36:54 PM [End  ] 

*C201194,4 TMI 10/04/2009 04:42:19 PM [Start] define a variable to show a warning message if needed
llWarning = .F.
lnHW = FCREATE(lcRpSpdir+ALLTRIM(lcRpOutNm)+'_ErrorLog.CSV')
*C201194,4 TMI 10/04/2009 04:42:20 PM [End  ] 

SCAN REST FOR !EOF()

  IF EMPTY(PRODUCTCOD)
    LOOP
  ENDIF 

  llValid = .F.
  
  *C201194,3 TMI 09/27/2009 11:53:48 AM [Start] get the pack ID, no need to the style major
  *lcStyMaj = PADR(PRODUCTCOD,8)+ "-"
  *C201194,3 TMI 09/27/2009 11:54:06 AM [End  ] 
  
  *C201194,1 TMI 09/09/2009 12:44:45 PM [Start] Get the STYLE ID from the SPCK_LIN file
  *lcColor  = ALLTRIM(COLCODE)  
  *IF EMPTY(COLCODE)
  *  SKIP 
  *  IF EMPTY(PRODUCTCOD)
  *    lcColor = ALLTRIM(COLCODE)
  *    IF EMPTY(lcColor)
  *      LOOP 
  *    ENDIF 
  *    SKIP -1
  *  ELSE 
  *    SKIP -1
  *    LOOP
  *  ENDIF 
  *ENDIF
  *lcStyle = lcStyMaj + PADR(lcColor,10) 
  lcStyle = SPACE(19)
  lcSz = ' '
  =lfGetColor()  
  IF EMPTY(lcStyle) OR EMPTY(lcSz) 
    *C201194,4 TMI 10/04/2009 04:19:24 PM [Start] Get all lines, if some lines are invalid then show a warning message
    *  LOOP
    *ENDIF  
    llWarning = .T.
    FPUTS(lnHW,lfGetTx())
  ELSE
    *C201194,4 TMI 10/04/2009 04:19:26 PM [End  ] 
    *C201194,1 TMI 09/09/2009 12:45:05 PM [End  ]   
  
    SELECT STYLE
    gfSeek(lcStyle)
    IF FOUND()
      WAIT WINDOW NOWAIT 'Processing Style : ' + lcStyle
      *C201194,1 TMI 09/09/2009 01:09:21 PM [Start] get availabel by size
      *lnAvail = TotStk + TotWip - TotOrd
      lnAvail = Stk&lcSz + Wip&lcSz - Ord&lcSz
      *C201194,1 TMI 09/09/2009 01:09:43 PM [End  ] 
      IF lnAvail >= 5
        SELECT(lcTmpXls)
        replace &lcTmpXls..AVAIL WITH STR(lnAvail)
        llValid = .T.
      ENDIF 
      replace &lcTmpXls..Valid WITH llValid 
    ENDIF 
  
    *C201194,4 TMI 10/04/2009 04:44:24 PM [Start] close the above IF statement
  ENDIF
  *C201194,4 TMI 10/04/2009 04:44:24 PM [End  ] 
  
  *C201194,4 TMI 10/04/2009 04:52:45 PM [Start] re select the lcTmpXls
  SELECT(lcTmpXls)
  *C201194,4 TMI 10/04/2009 04:52:47 PM [End  ] 
  
  *C201194,4 TMI 10/04/2009 04:19:58 PM [Start] get all lines
  *IF llValid 
  *C201194,4 TMI 10/04/2009 04:19:59 PM [End  ] 
  
    llDone = .T.
    *C201194,1 TMI 09/09/2009 12:43:21 PM [Start] 
    *lcTx = ALLTRIM(PRODUCTID) +';'+ALLTRIM(PRODUCTCOD)+';'+ALLTRIM(PRODUCT)+';'+ALLTRIM(VARIANTID)+';' + ;
           ALLTRIM(VARIANTCOD)+';'+ALLTRIM(WEIGHT)    +';'+ALLTRIM(PRICE)  +';'+ALLTRIM(AVAIL)    +';' + ;
           ALLTRIM(DEFAULT)   +';'+ALLTRIM(IMAGE)     +';'+ALLTRIM(CLASS)  +';'+ALLTRIM(OPTION)   +';' + ;
           ALLTRIM(COLCODE)
    lcTx = lfGetTx()
    *C201194,1 TMI 09/09/2009 12:43:32 PM [End  ] 
         
    FPUTS(lnh,lcTx)
    SKIP 
    IF !EMPTY(PRODUCTCOD)
      SKIP -1
    ELSE 
      *C201194,1 TMI 09/09/2009 12:43:35 PM [Start] 
      *lcTx = ALLTRIM(PRODUCTID) +';'+ALLTRIM(PRODUCTCOD)+';'+ALLTRIM(PRODUCT)+';'+ALLTRIM(VARIANTID)+';' + ;
             ALLTRIM(VARIANTCOD)+';'+ALLTRIM(WEIGHT)    +';'+ALLTRIM(PRICE)  +';'+ALLTRIM(AVAIL)    +';' + ;
             ALLTRIM(DEFAULT)   +';'+ALLTRIM(IMAGE)     +';'+ALLTRIM(CLASS)  +';'+ALLTRIM(OPTION)   +';' + ;
             ALLTRIM(COLCODE)
      lcTx = lfGetTx()
      *C201194,1 TMI 09/09/2009 12:43:41 PM [End  ] 
             
      FPUTS(lnh,lcTx)
    ENDIF 
  
  *C201194,4 TMI 10/04/2009 04:20:23 PM [Start] Get All lines
  *ENDIF 
  *C201194,4 TMI 10/04/2009 04:20:26 PM [End  ] 
  
ENDSCAN

*C201194,1 TMI 09/09/2009 01:52:37 PM [Start] 
FCLOSE(lnH)                                             
*C201194,1 TMI 09/09/2009 01:52:40 PM [End  ] 

IF !llDone 
  MESSAGEBOX('No Styles in the CSV File Match the Aria Styles !!!',576,'Nothing Matched!')
  *C201194,1 TMI 09/09/2009 01:49:46 PM [Start] Remove the created file
  ERASE (lcRpSpdir+ALLTRIM(lcRpOutNm)+'.CSV')
  *C201194,1 TMI 09/09/2009 01:49:46 PM [End  ] 
ELSE 
  MESSAGEBOX(ALLTRIM(lcRpOutNm)+'.CSV File Created Successfully in the Determined Directory.',576,'Created Successfully.')   
ENDIF 

*C201194,4 TMI 10/04/2009 04:54:07 PM [Start] show warning log
=FCLOSE(lnHW)
IF llWarning
  MESSAGEBOX("The input file contains some invalid SKU's,pls check the file "+lcRpSpdir+ALLTRIM(lcRpOutNm)+'_ErrorLog.CSV',0,'Problems.')
ELSE
  ERASE(lcRpSpdir+ALLTRIM(lcRpOutNm)+'_ErrorLog.CSV')
ENDIF
*C201194,4 TMI 10/04/2009 04:54:07 PM [End  ] 

*C201194,1 TMI 09/09/2009 01:52:37 PM [Start] move above
*FCLOSE(lnH)                                             
*C201194,1 TMI 09/09/2009 01:52:40 PM [End  ] 

RETURN .T.
*! --  end of lfProcCsv

************************************************************
*! Name      : lfwrepwhen
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Called    : From OG
*!*************************************************************
*! Example   : =lfwrepwhen()
*!*************************************************************
FUNCTION lfwRepWhen

*C201194,3 TMI 09/27/2009 01:28:18 PM [Start] remove extra space from folder if any
*lcRpSpdir=gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID)
*lcRpSphis=gfGetMemVar('M_DIRPRO',oAriaApplication.ActiveCompanyID)
lcRpSpdir = ALLTRIM(gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID))
lcRpSphis = ALLTRIM(gfGetMemVar('M_DIRPRO',oAriaApplication.ActiveCompanyID))
*C201194,3 TMI 09/27/2009 01:28:42 PM [End  ] 

*!*************************************************************
*! Name      : lfGetXlsFile
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : Importing Excel Sheet 
*!*************************************************************
*! Example   : =lfGetXlsFile()
*!*************************************************************
FUNCTION lfGetXlsFile  
LOCAL lcSaveDir
*C201194,3 TMI 09/30/2009 01:45:37 PM [Start] check if the file does not there
*IF EMPTY(lcCsvFile) OR "?" $ lcCsvFile
IF EMPTY(lcCsvFile) OR "?" $ lcCsvFile OR !FILE(ALLTRIM(lcCsvFile))
*C201194,3 TMI 09/30/2009 01:45:47 PM [End  ] 
  lcSaveDir = FULLPATH("")
  CD (lcRpSpDir)
  lcCsvFile = GETFILE("CSV") 
  lcFileName = JUSTFNAME(lcCsvFile)
  CD (lcSaveDir)
ENDIF 
RETURN lcFileName 

*End of lfGetXlsFile.

*!*************************************************************
*! Name      : lfXLSDIR
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : select xls dir 
*!*************************************************************
*! Example   : =lfXLSDIR()
*!*************************************************************
FUNCTION lfXLSDIR 

LOCAL lcSaveDir
IF EMPTY(lcRpSpDir) OR "?" $ lcRpSpDir
  lcSaveDir = FULLPATH("")
  *C201194,3 TMI 09/27/2009 01:29:39 PM [Start] 
  *lcRpSpdir=gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID)
  lcRpSpdir=ALLTRIM(gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID))
  *C201194,3 TMI 09/27/2009 01:29:44 PM [End  ] 
  CD (lcRpSpDir)
  *B609099,1 HES 12/01/2009 entered SKU codes in a system but was not able to save [Start]
*!*	  lcRpSpDir = GETDIR()
  lcRpSpDir = GETDIR('', 'Output Sheet Directory','Select Folder',64) 
  *B609099,1 HES 12/01/2009 entered SKU codes in a system but was not able to save [End]
  CD (lcSaveDir)
ELSE 
  TRY
    CD (lcRpSpDir)
  CATCH  
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You Should Enter a Valid Output Directory !')
    lcSaveDir = FULLPATH("")
    *C201194,3 TMI 09/27/2009 01:30:31 PM [Start] 
    *lcRpSpdir=gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID)
    lcRpSpdir=ALLTRIM(gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID))
    *C201194,3 TMI 09/27/2009 01:30:36 PM [End  ] 
    CD (lcRpSpDir)
    *B609099,1 HES 12/01/2009 entered SKU codes in a system but was not able to save [Start]
*!*	    lcRpSpDir = GETDIR()
    lcRpSpDir = GETDIR('', 'Output Sheet Directory','Select Folder',64) 
    *B609099,1 HES 12/01/2009 entered SKU codes in a system but was not able to save [End]
    CD (lcSaveDir)
  ENDTRY 
ENDIF
RETURN lcRpSpDir 
*End of lfXLSDIR.

*!*************************************************************
*! Name      : lfCreaTemp  
*: Developer : Mostafa Eid(mos)
*: Date      : 03/04/2009
*! Purpose   : Create Temp file to collect data FOR excel sheet
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCreaTemp()
*!*************************************************************
FUNCTION lfCreaTemp   

*-- check If File is created or not
IF USED(lcTmpXls) AND RECCOUNT(lcTmpXls) > 0
 USE IN (lcTmpXls)
ENDIF

*-- Create File
IF !USED(lcTmpXls)
      
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRODUCTID'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 50 && For the CSV Header (1st Line)
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRODUCTCOD'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRODUCT'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 100
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'VARIANTID'
  laTempStru[lnI,2] = 'C'
  *C201194,1 TMI 09/09/2009 11:54:47 AM [Start] widen SKU feild
  *laTempStru[lnI,3] = 10
  laTempStru[lnI,3] = 60
  *C201194,1 TMI 09/09/2009 11:54:55 AM [End  ] 
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'VARIANTCOD'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 20
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'WEIGHT'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRICE'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'AVAIL'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'DEFAULT'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'IMAGE'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 50
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CLASS'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
        
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'OPTION'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 50
  laTempStru[lnI,4] = 0
  
  *C201194,3 TMI 09/30/2009 01:22:42 PM [Start] no need for this field
  *lnI = ALEN(laTempStru,1)+1
  *DIMENSION laTempStru[lnI,4]
  *laTempStru[lnI,1] = 'COLCODE'
  *laTempStru[lnI,2] = 'C'
  *laTempStru[lnI,3] = 10
  *laTempStru[lnI,4] = 0
  *C201194,3 TMI 09/30/2009 01:22:52 PM [End  ] 
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Valid'
  laTempStru[lnI,2] = 'L'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0
  
=gfCrtTmp(lcTmpXls,@laTempStru,'Valid')
ENDIF  

*-- end of lfCreaTemp  
*:**************************************************************************
*:* Name        : lfProcess
*:* Developer   : Mostsfa eid 
*:* Date        : 03/29/2009
*:* Purpose     : CHECKING THE INPUTS 
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfProcess 
*- checking if the there is a missing data
LOCAL lcMsg
lcMsg = ''
DO CASE

CASE EMPTY(ALLTRIM(lcRpOutNm))
  lcMsg = 'You Should Enter the Output File Name!'

CASE EMPTY(lcCsvFIle)
  lcMsg = 'You Should Select the CSV File!'

CASE !FILE(lcCsvFIle) AND !EMPTY(lcCsvFIle)
  lcMsg = 'The Determined CSV File Path Not Found!'

CASE EMPTY(lcRpSpdir)
  lcMsg = 'You Should Select the Output Folder!'

ENDCASE

IF !EMPTY(lcMsg)
  MESSAGEBOX(lcMsg,560,'Insufficient Data!!')
  RETURN .F.
ENDIF

*! --  Calling main functions 
IF llOGFltCh
  =lfMain()
ENDIF 
RETURN .F.

*:**************************************************************************
*:* Name        : lfOpenTbls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/01/2007
*:* Purpose     : open needed tables
*:***************************************************************************
FUNCTION lfOpenTbls

IF !USED('STYLE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF

*-- end of lfOpenTbls.

*!*	*!*************************************************************
*!*	*! Name      : lfvStyle
*!*	*! Developer : Hesham Elmasry
*!*	*! Date      : 19/08/2009
*!*	*! Purpose   : Validate style
*!*	*!*************************************************************
*!*	*! Called from : Option Grid
*!*	*!*************************************************************
*!*	*! Calls       : ....
*!*	*!*************************************************************
*!*	*! Passed Parameters : None
*!*	*!*************************************************************
*!*	*! Return      : None
*!*	*!*************************************************************
*!*	*! Example     : = lfvStyle()
*!*	*!*************************************************************
*!*	FUNCTION lfvStyle

*!*	lcStyle = VARREAD()

*!*	lcTag = ORDER('STYLE')

*!*	SET ORDER TO cStyle IN STYLE

*!*	IF LASTKEY() = 13 AND !MDOWN()
*!*	  IF SEEK(&lcStyle.,'Style') 
*!*	    &lcStyle = STYLE.cStyMajor
*!*	  ELSE
*!*	    &lcStyle = gfStyBrw('M',"","",.F.)
*!*	  ENDIF
*!*	ELSE
*!*	  &lcStyle = ''
*!*	ENDIF

*!*	SET ORDER TO lcTag IN STYLE
*!*	*!*************************************************************
*!*	*! Name      : lfMajPic
*!*	*! Developer : BASSEM RAAFAT ERNEST(BWA)
*!*	*! Date      : 04/18/2006
*!*	*! Purpose   : Get major seg. picture
*!*	*!*************************************************************
*!*	*! Called from : Option Grid
*!*	*!*************************************************************
*!*	*! Calls       : ....
*!*	*!*************************************************************
*!*	*! Passed Parameters : None
*!*	*!*************************************************************
*!*	*! Return      : None
*!*	*!*************************************************************
*!*	*! Example     : = lfMajPic()
*!*	*!*************************************************************
*!*	FUNCTION lfMajPic

*!*	lcMajPic = "@! " + gfItemMask("PM")

*!*	RETURN lcMajPic

*!*	*!*************************************************************
*!*	*! Name      : lfMajTtlGet
*!*	*! Developer : BASSEM RAAFAT ERNEST(BWA)
*!*	*! Date      : 04/18/2006
*!*	*! Purpose   : To get the style major segement title
*!*	*!*************************************************************
*!*	*! Called from : Option Grid
*!*	*!*************************************************************
*!*	*! Calls       : ....
*!*	*!*************************************************************
*!*	*! Passed Parameters : None
*!*	*!*************************************************************
*!*	*! Return      : None
*!*	*!*************************************************************
*!*	*! Example     : = lfMajTtlGet()
*!*	*!*************************************************************
*!*	FUNCTION lfMajTtGet

*!*	lcMajTtl = gfItemMask("HM")

*!*	RETURN lcMajTtl

*!*	*--End of lfMajTtGet.


*:**************************************************************************
*:* Name        : lfGetTx
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/09/2009
*:* Purpose     : Get lcTx value
*:***************************************************************************
FUNCTION lfGetTx
*C201194,3 TMI 09/30/2009 01:12:31 PM [Start] add the needed "'s as per the original csv file
*lcTx = ALLTRIM(PRODUCTID) +';'+ALLTRIM(PRODUCTCOD)+';'+ALLTRIM(PRODUCT)+';'+ALLTRIM(VARIANTID)+';' + ;
        ALLTRIM(VARIANTCOD)+';'+ALLTRIM(WEIGHT)    +';'+ALLTRIM(PRICE)  +';'+ALLTRIM(AVAIL)    +';' + ;
        ALLTRIM(DEFAULT)   +';'+ALLTRIM(IMAGE)     +';'+ALLTRIM(CLASS)  +';'+ALLTRIM(OPTION)   +;
        IIF(!EMPTY(COLCODE), ';' + ALLTRIM(COLCODE) , '' )
LOCAL lcSemiColon
lcSemiColon = ';'
lcTx = ALLTRIM(PRODUCTID)     +lcSemiColon+;
       lfAddQot('PRODUCTCOD') +lcSemiColon+;
       lfAddQot('PRODUCT')    +lcSemiColon+;
       ALLTRIM(VARIANTID)     +lcSemiColon+;
       lfAddQot('VARIANTCOD') +lcSemiColon+;
       ALLTRIM(WEIGHT)        +lcSemiColon+;
       ALLTRIM(PRICE)         +lcSemiColon+;
       ALLTRIM(AVAIL)         +lcSemiColon+;
       ALLTRIM(DEFAULT)       +lcSemiColon+;
       ALLTRIM(IMAGE)         +lcSemiColon+;
       lfAddQot('CLASS')      +lcSemiColon+;
       lfAddQot('OPTION')
*C201194,3 TMI 09/30/2009 01:13:40 PM [End  ] 
RETURN lcTx            
*-- end of lfGetTx.
           
*:**************************************************************************
*:* Name        : lfAddQot
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/30/2009
*:* Purpose     : add qotations
*:***************************************************************************
FUNCTION lfAddQot
LPARAMETERS lcFld
LOCAL lcRet
lcRet = IIF(!EMPTY(&lcFld),'"','')+ALLTRIM(&lcFld)+IIF(!EMPTY(&lcFld),'"','')           
RETURN lcRet
*-- end of lfAddQot.
           
*:**************************************************************************
*:* Name        : lfGetColor
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/09/2009
*:* Purpose     : get color 
*:***************************************************************************
FUNCTION lfGetColor
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT SPCK_LIN
*C201194,3 TMI 09/27/2009 11:54:35 AM [Start] 
*=gfSeek('S'+'WEB01'+lcStyMaj,'SPCK_LIN')   && TYPE+ACCOUNT+STYLE+PACK_ID
*LOCATE REST WHILE TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+'WEB01'+lcStyMaj ;
        FOR PACK_ID = '###'+ALLTRIM(&lcTmpXls..VARIANTID)
=gfSeek('S'+'WEB01'+UPPER(ALLTRIM(&lcTmpXls..VARIANTCOD)),'SPCK_LIN')   && TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT
*C201194,3 TMI 09/27/2009 11:54:44 AM [End  ]         
IF FOUND()        
  lcStyle = SPCK_LIN.STYLE
  lcSz = IIF(QTY1=1,'1','')+;
         IIF(QTY2=1,'2','')+;
         IIF(QTY3=1,'3','')+;
         IIF(QTY4=1,'4','')+;
         IIF(QTY5=1,'5','')+;
         IIF(QTY6=1,'6','')+;
         IIF(QTY7=1,'7','')+;
         IIF(QTY8=1,'8','')
ENDIF
SELECT (lnSlct)
*-- end of lfGetColor.

*:**************************************************************************
*:* Name        : lfRmoveExt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/30/2009
*:* Purpose     : remove extension
*:***************************************************************************
FUNCTION lfRmoveExt
lcRpOutNm = JUSTSTEM(ALLTRIM(lcRpOutNm))
*-- end of lfRmoveExt.
