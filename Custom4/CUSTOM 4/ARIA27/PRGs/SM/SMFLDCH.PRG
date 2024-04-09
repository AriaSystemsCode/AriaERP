*:************************************************************************
*: Program file  : SMFLDCH.PRG
*: Program desc. : Key Change Program & Field Change Program
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SM
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 10/13/1998
*: REF           : *300998,1
*:************************************************************************
*: Calls : 
*:    Procedures : lpShow(), lpClsScr(), lpSavScr()
*:    Functions  : lfvComp(), lfCrtFiles(), lfvFiles(), lfvFields()
*:                 lfvValues(), lfvFilter(), lfvAdd(), lfvRemove() 
*:                 lfvNewVal(), lfvOldVal(), lfBrows(), lfwBrWhen()
*:                 lfReadAct(), lfReadDeact(), lfTrapKey(), lfEscape()
*:                 lfwOd(), lfvPrcd(), lfOpClFile(), lfActFolder()
*:                 lfbrwOpn(), lfwbrwOpn(), lfbrwHst(), lfwbrwHst(), lfRdActOp()]
*:                 lfRdDctOP(), lfEsc(), lfMajor(), lfNonMaj(), lfBldTmp()
*:                 lfvScale(), lfScalBrow(),lfvCustm(),lfvKeyVal()
*:************************************************************************
*: Passed Parameters  : 'K' Key change program
*:                      'F' Field change program
*:************************************************************************
*: Example : DO SOCAN
*:************************************************************************
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301077,29 AMM 02/28/99 Enhance opening and closing files
*B602722,1 AMM  03/30/99 Fix Bugs of 'Variable MEMO not Found' & Syntax error
*B603725,1 AMM  07/02/99 Fix some bugs in the program
*C101837,1 SSE  04/05/2000 Add the Scale to the Key changes
*C101837,1 MHM  07/10/2000 Add the Scale to the Key changes
*B803576,1 RAMY 08/15/2000 Change the Customer account in the schedule file
*B604119,1 BWA  28/02/2001 Fix the bug of "Compiled Code For This Line Too Long" in case the the user select customer.
*B604438,1 HBG  28/05/2001 Fix Bug of Variabel 'cCont_Id' not found
*B605057,1 ADEL 10/25/2001 Accept new values that only not exited in the base file..
*C102427,1 RAE  12/23/2001 Key Change for Store Numbers
*C102592,1 SSE  04/23/2002 Custom to display an OG for Styles that need changes in its Non Major. 
*B606594,1 WAB  03/09/2003 Fix the bug of invalie memo variable file 
*B607026,1 WAB  03/11/2003 Fix the bug of not all the files in which the changed fabric is invloved get changed. 
*B607125,1 ABD  04/06/2003 Global change for style field, I add a record at 
*B607125,1 ABD             Sydfdchg.dbf File, to change field CITMMAJOR  at BOM File.
*B607346,1 ALB  06/29/2003 Fix Bug when Change keys value that have shared char.
*B125283,1 NNA  12/17/2004 Fix bug that when you try to change some values in the "Ship Via" field
*B125283,1 NNA             of the customer master data file you'll get an error "Table is not ordered"
*B040090,1 EIH  03/09/2006 Fix bug that not update notepad file when we change the account code.
*:********************************************************************************************************************
PARAMETERS lcKeyType

*C102592,1 Display variable for temp style cursor. [Begin]
PRIVATE lcTmpStyle , lcStyMajor , llNonMjExt , llColorExt , lnClrPo , lnColorLen
STORE 0 TO lnClrPo , lnColorLen
STORE .F. TO llNonMjExt , llColorExt
STORE '' TO lcTmpStyle , lcStyMajor
lcStyMajor = gfItemMask('HM')
llNonMjExt = !EMPTY(gfItemMask('HN'))
*C102592,1 Display variable for temp style cursor. [End]

*C102427,1 RAE [BEGIN] 
lcKeyVal=''         && Holds the value of primary key
lcPKey=''           && Holds the value of primary key that stored in lcKeyVal
lcOldKey=''         && Holds the old value of key that will be changed
lcNewKey=''         && Holds the new value of key that will be changed
lcKeyTemp=''        && Holds the Temporary value of lcKeyVal
lnkeys=1            && Holds the numbers of keys that stored in SYDFDVLD.mFull_exp
*lnKeyPic= ''
*Increase the dimension of laFields from 6 to 7         
*DIMENSION laComp[1], laFiles[1,2], laFields[1,6], laCodInfo[1,10], ;
          laCodInf[1,10],laOldVal[1,2], laNewVal[1,2],lafoldwinds[2,2],;
          laComp[1,3], laMajSeg[1,1]
DIMENSION laComp[1], laFiles[1,2], laFields[1,7], laCodInfo[1,10], ;
          laCodInf[1,10],laOldVal[1,2], laNewVal[1,2],lafoldwinds[2,2],;
          laComp[1,3], laMajSeg[1,1],laFullExp[lnKeys,4]
*C102427,1 RAE [END]         

*C101837,1 SSE 04/05/2000 Add Public variable for the Scale Picture [Begin]
*STORE SPACE(0) TO laComp, laFiles, laFields, lcMaj,lcNMjrPt,;
*                  lcValues,lcHistory,lcNValues,lcbrwOpen,lcbrwHst,lcDummyCom,;
*                  lcBrTtl,lcWinKey,lcWinHist, lcfolder, lcfoldprnt,;
*                  lcwfoldchng, laCodInfo,laCodInf,laOldVal, laNewVal, lcOldfil,;
*                  lcOldOrd,lcCust,lcCusdp,lcRep, lcOld
STORE SPACE(0) TO laComp, laFiles, laFields, lcMaj,lcNMjrPt,;
                  lcValues,lcHistory,lcNValues,lcbrwOpen,lcbrwHst,lcDummyCom,;
                  lcBrTtl,lcWinKey,lcWinHist, lcfolder, lcfoldprnt,;
                  lcwfoldchng, laCodInfo,laCodInf,laOldVal, laNewVal, lcOldfil,;
                  lcOldOrd,lcCust,lcCusdp,lcRep, lcOld, lcScalePic , laFullExp
*C101837,1 SSE 04/05/2000 [End]

*-- Company array LACOMP[X,3]
*-- laComp[X,1] = Company ID + Description
*-- laComp[X,2] = Company ID
*-- laComp[X,3] = Company data directory

*-- Files array LAFILES[x,2]
*-- laFiles[X,1] = File Title
*-- laFiles[X,2] = File Name

*-- Fields array LAFIELDS[X,7]
*-- laFields[X,1]  = Header of the field
*-- laFields[X,2]  = Field name
*-- laFields[X,3]  = Special function of the field (Special function of 
*--                  Old Value, New Value, Replace process)
*-- laFields[X,4]  = Field width
*-- laFields[X,5]  = Field picture if any.
*-- laFields[X,6]  = Logic variable equal true if the field is code field

*C102427,1 RAE [BEGIN]
*-- laFields[X,7]  = Full expression for multiple keys

*-- Full_Expression array LAFULLEXP[X,4]
*-- laFullExp[X,1] = Key Name
*-- laFullExp[X,2] = Key Header
*-- laFullExp[X,3] = Key Width
*-- laFullExp[X,4] = Key Pict
*C102427,1 RAE [END]

*C101837,1 SSE 04/05/2000 Add public flag for Extended size Scale [Begin]
*STORE .F. TO llBrowse, laDefProc[10], llNoShow
STORE .F. TO llBrowse, laDefProc[10], llNoShow , llMScale , llSclExist
*C101837,1 SSE 04/05/2000 [End]

STORE .T. TO llNew , llFstSr

*C101837,1 SSE 04/05/2000 Add Public variable for Scale Lenght [Begin]
*STORE 0 TO lnORecNo, lnOldRNo,lnMajSize,lnMajSeg,lnOldRecNo, lnActFolder,;
*           lnLastFold , lnFolderCEnd,lnFolderREnd
*STORE 0 TO lnORecNo , lnOldRNo , lnMajSize , lnMajSeg , lnOldRecNo , ;
*           lnActFolder , lnLastFold , lnFolderCEnd , lnFolderREnd , lnScaleLen
*C101837,1 MHM 07/19/2000 Add Public variable for Lenght [Begin]
STORE 0 TO lnORecNo , lnOldRNo , lnMajSize , lnMajSeg , lnOldRecNo , ;
           lnActFolder , lnLastFold , lnFolderCEnd , lnFolderREnd , lnLenght
*C101837,1 MHM 07/19/2000 Add Public variable for Lenght [Begin]
*C101837,1 SSE 04/05/2000 [End]

lcfdTit = 'Field'

*-- lnORecNo   && Variable to hold record number in the history browse
*-- lnOldRNo   && Variable to hold record number in the Open keys  browse
*-- lcMaj      && Style major picture
*-- lcNMjrPt   && Style Non Major picture
*-- lnMajSize  && Style Major length
*-- lnMajSeg   && Number of segments in the style code structure
*-- Array to hold information of style segments

IF !gfSetup()
  RETURN
ENDIF  

IF !WEXIST(gcBaseWind)
  lcValues     = gfTempName()
  lcHistory    = gfTempName()
  lcNValues    = gfTempName()
  
  *-- Title name of the browse of open keys and values
  lcbrwOpen    = 'Open '+IIF(lcKeyType='F','Fields','Keys')
  *-- Title name of the browse of History keys and values
  lcbrwHst     = 'History'
  lcDummyCom   = REPLICATE("~",2)
  lcBrTtl = 'Values'
  *-- Two names for the screens of the two folders
  lcWinKey  = gfTempName()
  lcWinHist = gfTempName()

  lafoldwinds[1,1] = 'Key'  
  lafoldwinds[1,2] = lcWinKey
  lafoldwinds[2,1] = 'History'
  lafoldwinds[2,2] = lcWinHist
  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && window parent name for the folder 
  STORE 1 TO lnActFolder,lnLastFold && active folder
  *-- Dimensions of the folder screen
  lnFolderCEnd = 83.00
  lnFolderREnd = 1.99
  *-- Number of folders
  lnNoFld      = 2
  lcwfoldchng  = '=lfActFolder()'  && function to control shows after change the folder

  *-- Enable the local save
  laDefProc[9] = .F.
  *E301077,29 AMM Open files, They are not opened by module.
  =gfOpenFile(gcSysHome+'SYDFIELD', 'cFld_Name','SH')
  =gfOpenFile(gcSysHome+'SYDFILES', 'cFile_Nam','SH')
  =gfOpenFile(gcSysHome+'SYDFLFLD', 'CFile_Nam','SH')
  =gfOpenFile(gcSysHome+'SYDFDCHG', 'SYDFDCHG','SH')
  =gfOpenFile(gcSysHome+'SYDFDVLD', 'SYDFDVLD','SH')
  =gfOpenFile(gcSysHome+'SYCCOMP','','SH')
  *E301077,29 AMM end
  GO TOP
  IF EOF()
    *** There is no companies available.  You have ***
    *** to enter companies first before you enter ***
    *** their codes...
    *** <  Ok  > ***
    =gfModalGen("TRM00223B00000","DIALOG")
    glQuitting = .T.
    RETURN 
  ENDIF

  *-- Get the companies of the system in an array
  *E301098,1 Hesham (Start)
  *SELECT CCOMP_ID + " - " + CCOM_NAME, CCOMP_ID, cCom_DDir ;
    FROM SYCCOMP INTO ARRAY laComp ORDER BY CCOMP_ID
  SELECT CCOMP_ID + " - " + CCOM_NAME, CCOMP_ID, PADR(gfGetDataDir(ALLT(cCom_DDir)),LEN(cCom_dDir)) ;
    FROM SYCCOMP INTO ARRAY laComp ORDER BY CCOMP_ID
  *E301098,1 Hesham (End)
  DIMENSION laComp[ALEN(laComp,1)+1,3]
  = AINS(laComp,1)
  laComp[1] = "Select a Company"
  laComp[2] = lcDummyCom
  laComp[3] = ""
  lnComp    = 1

  *-- Create the temporary file Based on the KEY_CHG file , to work in it.
  CREATE TABLE (gcWorkDir+lcValues) (cKeyType C(1), cKey_Chg C(10),Old_Key C(30),;
            New_Key C(30) , cFld_Head C(30), cStatus C(1) , ENTERED D(10), ;
            lKey L(1) , mSaveFilt M(10))
  INDEX ON cKeyType+cKey_Chg+Old_Key TAG (lcValues)

  *-- Create a temproray file to browse the history data from it.
  CREATE TABLE (gcWorkDir+lcHistory) (cKeyType C(1), cKey_Chg C(10),Old_Key C(30),;
            New_Key C(30) , cFld_Head C(30), ENTERED D(10), Date_Procd D(10),;
             lKey L(1) )
  INDEX ON cKeyType+cKey_Chg+Old_Key TAG (lcHistory)

  *-- Use it again with another alias to make two browses based on it.
  USE (gcWorkDir+lcValues) AGAIN ALIAS (lcNValues) ORDER TAG (lcValues) IN 0 

ENDIF

*-- Display the screen
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcbrwOpen)
DO (gcScrDir + gcWinAppl + '\SMFLDCH.SPR')
ON KEY LABEL ALT+B 

IF glQuitting
  *-- Close data and erase the temporary tables
  IF USED(lcNValues)
    USE IN (lcNValues)
  ENDIF
  IF USED(lcValues)
    USE IN (lcValues)
    ERASE (gcWorkDir+lcValues+'.DBF')
    ERASE (gcWorkDir+lcValues+'.CDX')
  ENDIF
  IF USED('KEY_CHG')
    USE IN KEY_CHG
  ENDIF
  IF USED(lcHistory)
    USE IN (lcHistory)
    ERASE (gcWorkDir+lcHistory+'.DBF')
    ERASE (gcWorkDir+lcHistory+'.DBF')
  ENDIF
  IF USED('SYDFDCHG')
    USE IN SYDFDCHG
  ENDIF
  IF USED('SYDFDVLD')
    USE IN SYDFDVLD
  ENDIF
  *B602722,1  AMM Comment out No need to open and close the codes file
  *IF !EMPTY(lcCodFil) .AND. USED('CODES')
    *USE IN CODES
    *=gfOpenFile(lcCodFil,'CODES','SH')
    *lcCodFil = ''
  *ENDIF
  *B602722,1  AMM end
ENDIF

*!*************************************************************
*! Name      : lfvComp
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the company POPUP
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : FUNCTIONS : lfCrtFiles(), gfItemMask()
*!               PROCEDURES: None.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvComp()
*!*************************************************************
FUNCTION lfvComp

*B602722,1  AMM Comment out not used
*STORE SPACE(0) TO lcCodFil
*B602722,1  AMM end
IF laComp[lnComp,2] = lcDummyCom
  *C102427,1 RAE [BEGIN] Increase the dimension of laFields from 6 to 7  
  *DIMENSION laFiles[1,2], laFields[1,6]
  DIMENSION laFiles[1,2], laFields[1,7]
  *C102427,1 RAE [END]
  STORE SPACE(0) TO laFiles 
  _CUROBJ = _CUROBJ
  RETURN
ELSE
  *-- the file of the required company.
  IF USED('CODES') .AND. DBF('CODES') # UPPER(ALLTRIM(laComp[lnComp,3])+'Codes.dbf')
    *B602722,1  AMM Comment out , not used
    *lcCodFil = DBF('CODES')
    *B602722,1  AMM end
    USE IN CODES
  ENDIF
  = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'Codes','Codes','SH')
  
  *-- Fill the pop up of files or  keys 
  *B602722,1  AMM If there are no files, return
  *=lfCrtFiles()
  IF !lfCrtFiles()
    RETURN
  ENDIF
  *B602722,1  AMM end
  IF FILE(ALLTRIM(laComp[lnComp,3])+'ICISTRU.DBF') 
    llICISTRU = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICISTRU','Segno','SH')
    IF SEEK('U1')
      *-- Get the major Picture of the style
      lcMaj     = gfItemMask('PM',ALLTRIM(laComp[lnComp,3]))   
      *-- Length of the major
      lnMajSize = LEN(lcMaj)
      *-- Picture of non major
      lcNMjrPt  = gfItemMask('PN',ALLTRIM(laComp[lnComp,3]))
      *-- Number of major segments
      lnMajSeg  = gfItemMask('SM',ALLTRIM(laComp[lnComp,3]))    && No. of major segments.
      = gfItemMask(@laMajSeg,ALLTRIM(laComp[lnComp,3]))
    ENDIF
    IF llICISTRU
      =gfCloseFile('ICISTRU')
    ENDIF
  ENDIF
  
  *C101837,1 SSE 04/05/2000 Get Extended Size Scale flag , Scale Picture 
  *C101837,1                and Scale Lenght [Begin]
  *-- Check for company Using Extended Size Scale.  
  llMScale   = gfGetMemVar('M_USEEXSSC',laComp[lnComp,2]) 
  *lnScaleLen = IIF(llMScale,gfGetMemVar('M_EXTWIDTH')+1,1)   && Extended size Scale ID Length.  
  lnLenght =  IIF(SEEK('SCALE','SYDFIELD'),SYDFIELD.nFld_Wdth,0)
  lcScalePic = REPLICATE('X',lnLenght)  && Extended size scale ID picture.
  *C101837,1 SSE 04/05/2000 [End]
  
  IF lcKeyType = 'F'
    STORE SPACE(0) TO lafields
    _CUROBJ = OBJNUM(lnFiles)
    SHOW GET lnFiles
    SHOW GET lnFields
  ELSE
    *-- Go to view mode
    STORE .F. TO laScrMode
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfCrtFiles
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To fill the POPUP of files or keys.
*!*************************************************************
*! Called from : lfvComp()
*!*************************************************************
*! Calls       : FUNCTIONS  : gfOpenFile(), lfBldTmp(), lfbrwOpn()
*!               Procedures : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtFiles()
*!*************************************************************
FUNCTION lfCrtFiles

IF USED('KEY_CHG')
  USE IN KEY_CHG
ENDIF
= gfOpenFile(ALLTRIM(laComp[lnComp,3])+'KEY_CHG', 'KEY_CHG','SH')

IF lcKeyType = 'F'
  *B602722,1  AMM Initialize the array
  DIMENSION laFiles[1,2]  
  STORE SPACE(0) TO laFiles
  *B602722,1  AMM end
  *-- Fill the files array
  SELECT DISTINCT SYDFILES.cFile_ttl, SYDFDVLD.cFile_nam  ;
      FROM SYDFDVLD, SYDFILES ;
      WHERE SYDFDVLD.cKeyType+SYDFDVLD.cFile_nam = 'F'+SYDFILES.cFile_nam ;
           .AND. FILE(ALLTRIM(laComp[lnComp,3])+SYDFDVLD.cFile_nam+'.DBF');
      INTO ARRAY laFiles
  IF _TALLY = 0
    WAIT WINDOW 'No files available in this company.'
    RETURN .F.
  ENDIF
  
  DIMENSION laFiles[ALEN(laFiles,1)+1,2]
  = AINS(laFiles,1)
  laFiles[1] = "Select a File"
  laFiles[2] = lcDummyCom
  lnFiles    = 1
ELSE
  *-- Fill the field keys array
  *C102427,1 RAE [BEGIN] Add the mFull_exp field to SYDFDVLD
  *SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
                         SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
                  SYDFIELD.NFld_wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry;
         FROM SYDFDVLD,SYDFIELD ;
         WHERE SYDFDVLD.cKeyType+SYDFDVLD.cFld_name = 'K'+SYDFIELD.cFld_Name ;
               .AND. FILE(ALLTRIM(laComp[lnComp,3])+SYDFDVLD.cMainFile+'.DBF');
         INTO ARRAY laFields
   *DIMENSION laFields[ALEN(laFields,1)+1,6]
   SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
                         SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
                  SYDFIELD.NFld_wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry,SYDFDVLD.mFull_exp;
         FROM SYDFDVLD,SYDFIELD ;
         WHERE SYDFDVLD.cKeyType+SYDFDVLD.cFld_name = 'K'+SYDFIELD.cFld_Name ;
               .AND. FILE(ALLTRIM(laComp[lnComp,3])+SYDFDVLD.cMainFile+'.DBF');
         INTO ARRAY laFields
   DIMENSION laFields[ALEN(laFields,1)+1,7]         
   *C102427,1 RAE [END]


  = AINS(laFields,1)
  laFields[1] = "All"
  laFields[2] = lcDummyCom
  lnFields    = 1
  *-- Build the temporary files based on the key_chg file
  =lfBldTmp()
  *-- Update the browse of open fields
  =lfbrwOpn()
ENDIF

*!*************************************************************
*! Name      : lfvFiles
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the files POPUP .
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFiles()
*!*************************************************************
FUNCTION lfvFiles

PRIVATE llKey
llKey = .F.
lnAlias = SELECT(0)
IF laComp[lnComp,2] = lcDummyCom
  *-- 'You have to select a company.'
  *--        <OK>
  =gfModalGen("INM00331B00000","DIALOG")
  RETURN
ENDIF
*B602722,1  AMM add a checking if there is file chosen
*IF laFiles[lnFiles,2] # lcDummyCom
IF !EMPTY(laFiles[lnFiles,2]) .AND. laFiles[lnFiles,2] # lcDummyCom
*B602722,1  AMM end
  *-- Open the chosen file
  IF USED(laFiles[lnFiles,2]) .AND. DBF(laFiles[lnFiles,2]) # UPPER(ALLTRIM(laComp[lnComp,3])+laFiles[lnFiles,2]+'.dbf')
    lcOldfil = DBF(laFiles[lnFiles,2])
    lcOldOrd = ORDER(laFiles[lnFiles,2])
    USE IN (laFiles[lnFiles,2])
  ELSE
    STORE SPACE(0) TO lcOldFil, lcOldOrd
  ENDIF
  =gfOpenFile(ALLTRIM(laComp[lnComp,3])+laFiles[lnFiles,2],'','SH')
  
  *-- Fill the pop up of the fields from the system file SYDFDVLD
  *C102427,1 RAE [BEGIN] Add the mFull_exp field to SYDFDVLD
  *SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
                  SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
                  SYDFIELD.NFld_wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry;
         FROM SYDFDVLD,SYDFIELD ;
         WHERE SYDFDVLD.cKeyType+SYDFDVLD.cFld_name = 'F'+SYDFIELD.cFld_Name ;
               .AND. SYDFDVLD.cFile_nam = laFiles[lnFiles,2];
         INTO ARRAY laFields

  *DIMENSION laFields[ALEN(laFields,1)+1,7]
  SELECT DISTINCT IIF(EMPTY(SYDFDVLD.cFld_Head),SYDFIELD.cFld_Head,SYDFDVLD.cFld_Head), ;
                  SYDFDVLD.CFld_name, SYDFDVLD.mSpcFunc,;
                  SYDFIELD.NFld_wdth, SYDFIELD.cPict_str, SYDFIELD.lVldEntry,SYDFDVLD.mFull_exp;
         FROM SYDFDVLD,SYDFIELD ;
         WHERE SYDFDVLD.cKeyType+SYDFDVLD.cFld_name = 'F'+SYDFIELD.cFld_Name ;
               .AND. SYDFDVLD.cFile_nam = laFiles[lnFiles,2];
         INTO ARRAY laFields

  DIMENSION laFields[ALEN(laFields,1)+1,7]
  
  *C102427,1 RAE [END]
  
  = AINS(laFields,1)
  laFields[1] = "All"
  laFields[2] = lcDummyCom
  lnFields    = 1
  *-- Build the temporary files based on the KEY_CHG file.
  =lfBldTmp()
  STORE .F. TO laScrMode
  laScrMode[2] = .T.
  SHOW GETS
  SELECT (laFiles[lnFiles,2])
ELSE
  DIMENSION laFileds[1,5]  
  STORE SPACE(0) TO laFields
  SHOW GET lnFields DISABLE
  SHOW GET cbValues DISABLE
ENDIF
=lfbrwOpn()
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvFields
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of Fields POPUP
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : FUNCTIONS  : lfbrwOpn()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFields()
*!*************************************************************
FUNCTION lfvFields
IF laFields[lnFields,2]=lcDummyCom
  SHOW GET cbValues DISABLE
ELSE
  DO CASE
    CASE laFields[lnFields,2] = 'CSTYMAJOR'
      laFields[lnFields,4] = lnMajSize
      laFields[lnFields,5] = lcMaj
    CASE laFields[lnFields,2] = 'STYLE'
      laFields[lnFields,4] = LEN(lcNMjrPt)
      laFields[lnFields,5] = lcNMjrPt
    
    *C101837,1 SSE 04/05/2000 Adjust the Old and New Values Fields to
    *C101837,1                hold the Scale Picture [Begin] 
    CASE laFields[lnFields,2] = 'SCALE'
      laFields[lnFields,4] = lnLenght
      laFields[lnFields,5] = lcScalePic
    *C101837,1 SSE 04/05/2000 [End] 
  ENDCASE
  
  SHOW GET cbValues ENABLE  
ENDIF
=lfbrwOpn()


*!*************************************************************
*! Name      : lfvValues
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the Values check box to open the 
*!             values screen
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfOpenFile(),gfwCodePop(),SMFLDVL.SPR,
*!                            lfBrwOpn()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvValues()
*!*************************************************************
FUNCTION lfvValues

lnAlias = SELECT(0)
*C102427,1 RAE [BEGIN] Declaration of lnCount 
*PRIVATE llFile
PRIVATE llFile , lnCount
*C102427,1 RAE [END]

llFile = .F.
STORE SPACE(0) TO lcPct, lcCode
IF laFields[lnFields,2] = lcDummyCom .OR. laFields[lnFields,4] = 0
  cbValues = .F.
  SHOW GET cbValues
  RETURN
ENDIF
*-- Open the master file of the field to browse from it.
IF SEEK(lcKeyType+laFields[lnFields,2],'SYDFDVLD')
   IF USED(SYDFDvld.cMainFile) .AND. DBF(SYDFDvld.cMainFile) # UPPER(ALLTRIM(laComp[lnComp,3])+ALLTRIM(SYDFDvld.cMainFile)+'.DBF')
     USE IN (SYDFDvld.cMainFile)
   ENDIF
   *B602722,1  AMM Remove the spaces from the file name string
   *llFile = gfOpenFile(ALLTRIM(laComp[lnComp,3])+SYDFDvld.cMainFile,SYDFDVLD.cFile_Tag,'SH')
   llFile = gfOpenFile(ALLTRIM(laComp[lnComp,3])+ALLTRIM(SYDFDvld.cMainFile) ,SYDFDVLD.cFile_Tag,'SH')
   *B602722,1  AMM end
   IF !llFile .AND. !USED(SYDFDvld.cMainFile)
     RETURN
   ELSE
     *wab --new release
     COUNT TO lnRecrdNo FOR !DELETED()
     IF lnRecrdNo = 0
       cbValues = .F.
       SHOW GET cbValues
       RETURN
     ENDIF
     *wab
   ENDIF
ELSE
  RETURN
ENDIF

lcCode   = laFields[lnFields,1]

*C102592,1 Display OG to filter on Styles. [Begin]
IF UPPER(lcCode) = "STYLE NON MAJOR" 
  IF TYPE('lnCount') = "N"
    PRIVATE lnOldCount
    lnOldCount = lnCount
  ENDIF 
  DO lpSelecSty
  IF TYPE('lnCount') = "N"
    lnCount = lnOldCount
  ENDIF   
ENDIF
*C102592,1 Display OG to filter on Styles. [End]

STORE SPACE(laFields[lnFields,4]) TO lcOldVal,lcNewVal
*-- If the field is a code field
IF laFields[lnFields,6]
  laCodInfo[1,01] = laFields[lnFields,2]      && Field Name
  laCodInfo[1,02] = "laOldVal"                && Array Name
  laCodInfo[1,03] = "lnOldPop"                && Popup Name
  laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .T.                       && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = lcValues                  && Alternative File (For default val.)
  laCodInfo[1,08] = lcValues                  && Use this index for the Alternative file.
  laCodInfo[1,09] = "PADR(laFields[lnFields,2],11)+lcOldVal"    && Seek this expression.
  laCodInfo[1,10] = "Old_Key"                  && Alternative Field Name

  laCodInf[1,01] = laFields[lnFields,2]      && Field Name
  laCodInf[1,02] = "laNewVal"                && Array Name
  laCodInf[1,03] = "lnNewPop"                && Popup Name
  laCodInf[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
  laCodInf[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
  laCodInf[1,06] = .T.                       && Include "ALL" (.T.->Yes,.F.,No)
  laCodInf[1,07] = lcValues                  && Alternative File (For default val.)
  laCodInf[1,08] = lcValues                  && Use this index for the Alternative file.
  laCodInf[1,09] = "PADR(laFields[lnFields,2],11)+lcOldVal"    && Seek this expression.
  laCodInf[1,10] = "New_Key"                  && Alternative Field Name

  = gfwCodePop(@laCodInfo, ALLTRIM(laFields[lnFields,2]) , "N",laComp[lnComp,2])
  = gfwCodePop(@laCodInf , ALLTRIM(laFields[lnFields,2]) , "N",laComp[lnComp,2])
ELSE
  *-- Picture of the field (Old value and New value)
  lcPct = laFields[lnFields,5]
ENDIF

*C102427,1 RAE [BEGIN] Initialization of laFullExp
IF !EMPTY(laFields[lnFields,7])
  IF !USED('SYDFIELD')
    =gfOpenFile(gcSysHome+'SYDFIELD','SYDFIELD','SH')
  ENDIF
  lnkeys=OCCURS('+',SYDFDVLD.mFull_Exp)+1
  DIMENSION laFullExp[lnKeys,4]
  SELECT SYDFDVLD
  lcKeyValues = SYDFDVLD.mFull_Exp
  FOR lnCount=1 TO lnKeys
    lcPlus=OCCURS('+',lcKeyValues)
    IF lcPlus > 0
      laFullExp[lnCount,1] = SUBSTR(lcKeyValues,1,AT('+',lcKeyValues)-1) 
      lcKeyValues = SUBSTR(lcKeyValues,LEN(laFullExp[lnCount,1])+2)
    ELSE
      laFullExp[lnCount,1] = lcKeyValues
    ENDIF
    
    SELECT SYDFIELD
    SET ORDER TO TAG Cfld_name 
    IF SEEK(laFullExp[lnCount,1],'SYDFIELD')
      laFullExp[lnCount,2] = Cfld_Head
      laFullExp[lnCount,3] = Nfld_Wdth
      laFullExp[lnCount,4] = cPict_str
    ENDIF   
  ENDFOR
ENDIF

*C102427,1 RAE [END]    
*WAB
lcKeyTemp = ''
lcKeyVal  = ''
*WAB
*-- Call the values screen
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
DO (gcScrDir + gcWinAppl + '\SMFLDVL.SPR')
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcbrwOpen)

*-- Refresh the Open keys browse
=lfBrwOpn()

cbValues = .F.
SHOW GET cbValues
KEYBOARD '{ALT+B}'

IF llFile .AND. SEEK(lcKeyType+laFields[lnFields,2],'SYDFDVLD')
  USE IN (ALLTRIM(SYDFDvld.cMainFile) )
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lpShow
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To enable or disable screen objects due to screen mode
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfOpenFile()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpShow()
*!*************************************************************
PROCEDURE lpShow

SHOW GET pbDlt  DISABLE
SHOW GET pbTop  DISABLE
SHOW GET pbPrvs DISABLE
SHOW GET pbNxt  DISABLE
SHOW GET pbBtm  DISABLE
SHOW GET pbBrws DISABLE
DO CASE
  *-- Select Mode
  CASE laScrMode[1]
    IF !EMPTY(lcOldfil)
      lcOldfil = ''
      lcOldOrd = ''
      USE IN (laFiles[lnFiles,2])
      =gfOpenFile(lcoldFil,lcOldOrd,'SH')      
    ENDIF
    *C102427,1 RAE [BEGIN] Increase the dimension of laFields from 6 to 7         
    *DIMENSION laFiles[1,2], laFields[1,6]
    DIMENSION laFiles[1,2], laFields[1,7]
    *C102427,1 RAE [END]
    
    STORE SPACE(0) TO laFiles,laFields
    llFstSR = .T.
    SHOW GET lnComp      ENABLE
    SHOW GET lnFiles     ENABLE
    SHOW GET lnFields    DISABLE
    SHOW GET cbValues    DISABLE
    SHOW GET ibFolder(2) DISABLE
    SHOW GET pbPrcd      DISABLE
    
  *-- View Mode
  CASE laScrMode[2]
    SHOW GET lnComp      DISABLE
    SHOW GET lnFiles     DISABLE
    SHOW GET lnFields    DISABLE
    SHOW GET cbValues    DISABLE
    SHOW GET ibFolder(2) ENABLE
    SHOW GET pbPrcd      ENABLE
    SHOW GET ibopTab     ENABLE
  *-- Edit Mode
  CASE laScrMode[3]
    SHOW GET lnComp      DISABLE
    SHOW GET lnFiles     DISABLE
    SHOW GET lnFields    ENABLE
    IF laFields[lnFields,2] = lcDummyCom
      cbValues = 0
      SHOW GET cbValues    DISABLE
    ELSE
      SHOW GET cbValues    ENABLE
    ENDIF
    SHOW GET ibFolder(2) ENABLE
    SHOW GET pbPrcd      DISABLE
ENDCASE

*!*************************************************************
*! Name      : lfvFilter
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To make a filter on the chosen table
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfDbfField(), =gfBFiltr()
*!               PROCEDURES : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFilter()
*!*************************************************************
FUNCTION lfvFilter

lcBaseFile = ALLTRIM(laFiles[lnFiles,2])
lcScFields = 'KEY_CHG.cKEY_CHG'
SELECT (lcValues)

lcBrFields = gfDbfField(lcBaseFile)
DIMENSION gaTmpArray[1,3]
gaTmpArray = ' '

SELECT DIST SYDFIELD.CFLD_NAME, SYDFIELD.cfld_head, SYDFLFLD.NFLD_POS;
   FROM SYDFLFLD, SYDFIELD;
   WHERE SYDFIELD.CFLD_NAME = SYDFLFLD.CFLD_NAME  .AND. ;
            sydflfld.cfile_nam == lcBaseFile          .AND. ;
            ALLTRIM(sydfield.cfld_name) $  lcBrFields ;
   ORDER BY SYDFLFLD.NFLD_POS;
   INTO ARRAY gaTmpArray

  **MAN Instead of SYDFLFLD.CFLD_NAME Put SYDFLFLD.CFLD_NAME+STR(SYDFLFLD.NFLD_POS)
SELECT SYDFIELD.CFLD_NAME,SPACE(10) ;
   FROM SYDFLFLD, SYDFIELD;
   WHERE SYDFIELD.CFLD_NAME = SYDFLFLD.CFLD_NAME   .AND. ;
         sydflfld.cfile_nam == lcBaseFile           .AND. ;
         ALLTRIM(sydfield.cfld_name) $  lcBrFields .AND.;
         !EMPTY (sydfield.mventries) ;
   INTO ARRAY laArrayes

gnArrayln = ALEN(gaTmpArray,1)

DECLARE laField_H [gnArrayln,1] ,laField_N [gnArrayln,1]

FOR lnElem = 1 TO gnArrayln
  laField_N[lnElem,1] = ALLTRIM(gaTmpArray[lnElem,1])
  
  laField_H[lnElem,1] = IIF(EMPTY(gaTmpArray[lnElem,2]),;
                ALLTRIM(gaTmpArray[lnElem,1]),ALLTRIM(gaTmpArray[lnElem,2]))
ENDFOR

RELEASE gaTmpArray,gnArrayln

IF !EMPTY(laArrayes[1,1])
  FOR lnNewAry = 1 TO ALEN(laArrayes,1)
    laArrayes[lnNewAry,2]  = "la"+PROPER(SUBSTR(SYS(2015),3,8))
    IF AT(laArrayes[lnNewAry,1],lcBrFields)>0
      lcAscanStr = 'cDummi'+ALLTRIM(STR(lnNewAry))+'=IIF(ASCAN(&laArrayes[lnNewAry,2],&laArrayes[lnNewAry,1])>0,&laArrayes[lnNewAry,2] [ASUBS(&laArrayes[lnNewAry,2],ASCAN(&laArrayes[lnNewAry,2],&laArrayes[lnNewAry,1]),1),1]," ")'
      lcBrFields = STRTRAN(lcBrFields ,laArrayes[lnNewAry,1],lcAscanStr)
    ENDIF 
  ENDFOR
ENDIF
lcCurFilter = ''
lcFltBefor  = SET('FILTER')
*SELECT key_chg
*SEEK 'F'+'SALESREP'
*IF !EMPTY(mSaveFilt)
*  RESTORE FROM MEMO mSaveFilt ADDITIVE
*ENDIF  
SELECT (lcBaseFile)
IF llNew
  STORE SPACE(0) TO laFixFltr
ENDIF
=gfBFiltr('laFixFltr',@laField_H,@laField_N,'F')
*lcGetFlt = ALLTRIM(gfGenFlt('laFixFltr', .T.))
*x=select(0)
*select key_chg
*seek 'F'+'SALESREP'
*select (x)
lcFltAftr   = SET('FILTER')
SELECT (lcValues)
IF lcFltBefor # lcFltAftr
  SAVE TO MEMO mSaveFilt;
     ALL LIKE laFixFltr
  REPLACE cStatus   WITH IIF(cStatus='S','M',cStatus)
ENDIF

*!*************************************************************
*! Name      : lpClsScr
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To Get the data from the original files and reject 
*!             the modifications of the user
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : lfBldTmp(), lfbrwOpn()
*!               PROCEDURES : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpClsScr()
*!*************************************************************
PROCEDURE lpClsScr

IF laScrMode[3]
  *-- Build the temporary files again
  =lfBldTmp()
  IF lnActFolder = 1
    *-- Refresh the browse
    =lfbrwOpn()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Saving the screens (local save)
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : None
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpSavScr()
*!*************************************************************
PROCEDURE lpSavScr

PRIVATE lcFldtmp, lcDel
lcFldtmp = ''
lcDel = SET('DELETE')
SET DELETED OFF
SELECT (lcValues)

SCAN FOR cStatus # 'S'
  *-- If old value equals new value, loop and don't do any thing
  IF Old_Key = New_Key 
    LOOP
  ENDIF
  DO CASE
    CASE cStatus = 'A'
      *-- If added record
      INSERT INTO KEY_CHG (cKeyType,cKey_chg,Old_Key,New_Key,keyStatus,ENTERED) ;
           VALUES (&lcValues..cKeyType,&lcValues..cKey_Chg,&lcValues..Old_Key,;
                           &lcValues..New_Key,'O',DATE())
    CASE cStatus = 'M'    
      *-- If modified record
      SET DELETED ON
      IF SEEK(&lcValues..cKeyType+PADR(EVAL(lcValues+'.cKey_Chg'),10) + ALLTRIM(&lcValues..Old_Key),'KEY_CHG')
        SELECT KEY_CHG
        *B603725,1  AMM there is no field called cnewval
        *REPLACE New_Key  WITH &lcValues..cNewVal,;
                ENTERED  WITH &lcValues..ENTERED
        REPLACE New_Key  WITH &lcValues..NEW_KEY,;
                ENTERED  WITH &lcValues..ENTERED
        *B603725,1  AMM end
      ELSE
        LOOP
      ENDIF
      SET DELETED OFF
    CASE cStatus = 'D'
      *-- If deleted record
      SET DELETED ON
      IF SEEK(&lcValues..cKeyType+PADR(EVAL(lcValues+'.cKey_Chg'),10) + ALLTRIM(&lcValues..Old_Key),'KEY_CHG')
        SELECT KEY_CHG    
        DELETE
      ENDIF
      SET DELETED OFF
    ENDCASE
  
ENDSCAN
SET DELETE &lcDel

*!*************************************************************
*! Name      : lfvAdd
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the add button
*!*************************************************************
*! Called from : SMFLDVL.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfwCodePop()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAdd()
*!*************************************************************
FUNCTION lfvAdd


*C102427 RAE [BEGIN] 
IF EMPTY(laFields[lnFields,7])
*C102427 RAE [END]
  STORE SPACE(laFields[lnFields,4]) TO lcOldVal, lcNewVal
  SHOW GET pbRemove DISABLE
  llNew    = .T.
  IF laFields[lnFields,6]
    = gfwCodePop(@laCodInfo, ALLTRIM(laFields[lnFields,2]) , "N",laComp[lnComp,2])
    = gfwCodePop(@laCodInf , ALLTRIM(laFields[lnFields,2]) , "N",laComp[lnComp,2])
    SHOW GET lnOldPop ENABLE
    SHOW GET lnNewPop ENABLE
    _CUROBJ = OBJNUM(lnOldPop)
  ELSE
    SHOW GET lcOldVal ENABLE
    SHOW GET ibOldVal ENABLE
    SHOW GET lcNewVal ENABLE
    SHOW GET ibNewVal ENABLE
    _CUROBJ = OBJNUM(lcOldVal)
  ENDIF
*C102427 RAE [BEGIN] 
ELSE    && !EMPTY(SYDFDVLD.MFulL_EXP)
  STORE SPACE(laFields[lnFields,4]) TO lcOldVal, lcNewVal
  FOR lnCount=1 TO lnKeys
    lcKeyPic = laFullExp[lnKeys,4]
    lcKeyTitl= laFullExp[lnCount,2]
    lcKeyVal=''
    DO (gcScrDir + gcWinAppl + '\smkeynam.SPR')   
    IF EMPTY(lcKeyTemp)
      EXIT
    ENDIF    
  ENDFOR
  IF !EMPTY(lcKeyTemp)
    STORE SPACE(laFields[lnFields,4]) TO lcOldVal, lcNewVal
    SHOW GET pbRemove DISABLE
    llNew   = .T.
    IF laFields[lnFields,6]
      = gfwCodePop(@laCodInfo, ALLTRIM(laFields[lnFields,2]) , "N",laComp[lnComp,2])
      = gfwCodePop(@laCodInf , ALLTRIM(laFields[lnFields,2]) , "N",laComp[lnComp,2])
      SHOW GET lnOldPop ENABLE
      SHOW GET lnNewPop ENABLE
      _CUROBJ = OBJNUM(lnOldPop)
    ELSE
      SHOW GET lcOldVal ENABLE
      SHOW GET ibOldVal ENABLE
      SHOW GET lcNewVal ENABLE
      SHOW GET ibNewVal ENABLE
      _CUROBJ = OBJNUM(lcOldVal)
    ENDIF
  ENDIF
  IF EMPTY(lcKeyVal)
    IF !EOF(lcValues)
      =lfwBrWhen()
    ENDIF
  ENDIF
ENDIF
*C102427 RAE [END]
*!*************************************************************
*! Name      : lfvRemove
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the remove button
*!*************************************************************
*! Called from : SMFLDVL.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : lfBrows()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRemove()
*!*************************************************************
FUNCTION lfvRemove

*-- Message : Are you sure you want to delete this record ?
*-- Button     <Remove>    <Cancel>
IF gfModalGen("QRM00002B00007","DIALOG","remove") = 1
  SELECT (lcValues)
  REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
  DELETE
  =lfBrows()
  KEYBOARD '{ALT+B}'
  SHOW GETS
  *=lfwBrWhen()
ENDIF

*!*************************************************************
*! Name      : lfvNewVal
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To get some codes descriptions
*!*************************************************************
*! Called from : SMFLDVL.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : lfVldBrw(), 
*!                          &lcSpecFn Special function of the field if any.
*!               PROCEDURES : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvNewVal()
*!*************************************************************
FUNCTION lfvNewVal
PRIVATE  llRet, lnAlias

llRet = .T.
lnAlias = SELECT(0)
SELECT (lcValues)
IF laFields[lnFields,6]
  lcNewVal = laNewVal[lnNewPop,2]
ENDIF
*B606594,1 WAB (Start) -- in case of Sales representive we need to browse the new val
llChgValue = (UPPER(laFields[lnFields,1]) ="SALES REPRESENTATIVE")     
*B606594,1 WAB (END)   
IF EMPTY(lcNewVal) .AND. !llBrowse
  *B605057,1 (Begin) Don't browse.
  *KEYBOARD '{ALT+B}'
 
  *B606594,1 WAB (Start) -- in case of Sales representive we need to browse the new val
  *RETURN
  IF llChgValue
    KEYBOARD '{ALT+B}'
  ELSE
    RETURN
  ENDIF
 *B606594,1 WAB (End) 
 
  *B605057,1 (End)
ELSE
  IF lcNewVal = lcOldVal .AND. !llBrowse
    *-- "Old and new value Cannot be the same. Cannot proceed. !!!"
    *--                       <OK>
    =gfModalGen("INM00332B00000","DIALOG")
    lcNewVal = lcOld
    _CUROBJ = _CUROBJ
  ELSE
   
    lcExpr = cKeyType+cKey_Chg+Old_Key
    IF SEEK(lcKeyType+PADR(laFields[lnFields,2],10)+lcNewVal,lcValues)
      *-- "This value has been selected before as "old value". Cannot accept."
      =gfModalGen("INM00335B00000","DIALOG","old value")
      llRet = .F.
    ENDIF
    =SEEK(lcExpr,lcValues)
    IF llRet
      IF EMPTY(laFields[lnFields,3])
        *-- If there isn't special function , go to the browse function of all.
        *-- Browse field by using SYDFDCLD file.
        *B605057,1 (Begin) Check the new value.
        *lcNewVal =lfVldBrw(lcNewVal)
        
        *B606594,1 WAB (Start) -- in case of Sales representive we need to browse the new val
        *lcNewVal =lfGetVal(lcNewVal)
        IF llChgValue
          lcNewVal =lfVldBrw(lcNewVal)
        ELSE
          lcNewVal =lfGetVal(lcNewVal)
        ENDIF
        *B606594,1 WAB (End) 
        *B605057,1 (End)

      ELSE
        *-- Get the special function of the field and call it with the 
        *-- parameter "N"
        lcSpecFn = STRTRAN(laFields[lnFields,3] , '()','("N")'  )
        llRet    = &lcSpecFn
      ENDIF
      IF EMPTY(laFields[lnFields,7])
        *C101837,1 SSE 04/05/2000 Check if Empty of new value [Begin]
        *B605057,1 (Begin) Fix the condition.
        *llRet = IIF(EMPTY(lcNewVal),.F.,.T.)
        llRet = IIF(EMPTY(lcNewVal) OR (ALLTRIM(lcNewVal) = "?" AND llSclExist) OR !llRet,.F.,.T.)
        *B605057,1 (End)
        *C101837,1 SSE 04/05/2000 [End]
      
        IF !EMPTY(lcNewVal ) .AND. lcOldVal = lcNewVal 
          *-- "Old and new value Cannot be the same. Cannot proceed. !!!"  
          *--                  < OK >
          =gfModalGen("INM00332B00000","DIALOG")
          lcNewVal = lcOld
          llRet = .F.
        ENDIF
        SELECT(lcValues)
        lcExpr = cKeyType+cKey_Chg+Old_Key
        *B603725,1  AMM check if not empty of the new value
        *IF SEEK(lcKeyType+PADR(laFields[lnFields,2],10)+lcNewVal,lcValues)
        IF !EMPTY(lcNewVal) .AND. SEEK(lcKeyType+PADR(laFields[lnFields,2],10)+lcNewVal,lcValues)
        *B603725,1  AMM end
          *-- "This value has been selected before as "old value". Cannot accept."
          =gfModalGen("INM00335B00000","DIALOG","old value")
          lcNewVal = lcOld
          llRet = .F.
        ENDIF
        =SEEK(lcExpr,lcValues)
        ENDIF
      ENDIF
      *-- If the new valid entered is not valid.!!
      IF !llRet
        _CUROBJ = _CUROBJ
      ELSE
        *-- Update the temporary cursor
        SELECT (lcValues)
        *C102427,1 RAE [BEGIN]
        *IF !EMPTY(lcOldVal)      
        IF !EMPTY(lcOldVal) .AND. EMPTY(laFields[lnFields,7])
          *C102427,1 RAE [END]
          IF llNew
            APPEND BLANK
            REPLACE cStatus   WITH 'A',;
                    cFld_Head WITH  laFields[lnFields,1]
            llNew = .F.
          ELSE
            REPLACE cStatus WITH IIF(cStatus='S','M',cStatus)
          ENDIF
          REPLACE cKeyType   WITH lcKeyType,;
                  Old_Key    WITH lcOldVal,;
                  New_Key    WITH lcNewVal,;
                  cKey_Chg   WITH laFields[lnFields,2],;
                  ENTERED    WITH DATE()
          SAVE TO MEMO mSaveFilt ALL LIKE laFixFltr
          *-- Variable to indicate that data has modified
          llcUpDate = .T.
        ENDIF
      ENDIF
    ENDIF
  ENDIF

llBrowse = .F.
SELECT (lnAlias)
SHOW GET pbAdd    ENABLE
SHOW WINDOW (lcBrTtl) REFRESH


*!*************************************************************
*! Name      : lfvOldVal
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the old value.
*!*************************************************************
*! Called from : SMFLDVL.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : lfVldBrw(), 
*!                          &lcSpecFn Special function of the field if any.
*!               PROCEDURES : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOldVal()
*!*************************************************************
FUNCTION lfvOldVal

PRIVATE lnAlias,llRet
lnAlias = SELECT(0)

SELECT(lcValues)
llRet    = .T.
IF laFields[lnFields,6]
  lcOldVal = laOldVal[lnOldPop,2]
ENDIF

IF EMPTY(lcOldVal) .AND. !llBrowse
  KEYBOARD '{ALT+B}'
ELSE

  IF lcOldVal = lcNewVal .AND. !llBrowse
    *-- "Old and new value Cannot be the same. Cannot proceed. !!!"  
    *--                  < OK >
    =gfModalGen("INM00332B00000","DIALOG")
    lcOldVal = lcOld
    _CUROBJ = _CUROBJ
  ELSE
    *WAB
    lnRecNo = RECNO(lcValues)
    *WAB
    IF SEEK(lcKeyType+PADR(laFields[lnFields,2],10)+lcOldVal,lcValues)
      *-- "This value has been selected before as an "old value". Cannot accept."
      =gfModalGen("INM00335B00000","DIALOG","old value")
      llRet = .F.
    ELSE
      SELECT (lcValues)
      LOCATE FOR cKeyType+cKey_Chg+Old_Key+New_Key = ;
                    lcKeyType+PADR(laFields[lnFields,2],10)+Old_key+lcOldVal
      IF FOUND()
        *-- "This value has been selected before as a "new value". Cannot accept."
        =gfModalGen("INM00335B00000","DIALOG","new value")
        llRet = .F.
      ENDIF
    ENDIF
    *wab
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcValues))
      GO lnRecNO IN (lcValues)
    ENDIF
    *wab
    IF llRet
      *B125283,1 NNA 12/17/2004 (Begin) Check if user Select to Change Ship Via or not because in
      *B125283,1 NNA            case Customer File dosn't have an index on this Field and also there is
      *B125283,1 NNA            Browse button on the screen.
      *IF EMPTY(laFields[lnFields,3])
      IF EMPTY(laFields[lnFields,3]) AND !(ALLTRIM(laFields[lnFields,2]) == 'SHIPVIA')
      *B125283,1 NNA (End)
      
        *-- If there isn't special function, Call the browse of all.
        lcOldVal = lfVldBrw(lcOldVal)
      ELSE

        *B125283,1 NNA 12/17/2004 (Begin) if user Select to Change the Ship Via then there is no
        *B125283,1 NNA            index or function so I return only the selected Value
        IF (ALLTRIM(laFields[lnFields,2]) == 'SHIPVIA')
          lcOldVal = laOldVal[lnOldPop,2]
        ELSE
        *B125283,1 NNA (End)

        *-- Call the special function with parameter "O"
        lcSpecFn = STRTRAN(laFields[lnFields,3] , '()','("O")'  )
        llRet    = &lcSpecFn

        *B125283,1 NNA (Begin)
        ENDIF
        *B125283,1 NNA (End)

      ENDIF
      IF EMPTY(laFields[lnFields,7])
        IF !EMPTY(lcOldVal) .AND. lcNewVal = lcOldVal
          *-- "Old and new value Cannot be the same. Cannot proceed. !!!"  
          *--                  < OK >
          =gfModalGen("INM00332B00000","DIALOG")
          lcOldVal = lcOld
          llRet = .F.
        ENDIF
        IF SEEK(lcKeyType+PADR(laFields[lnFields,2],10)+lcOldVal,lcValues)
          *-- "This value has been selected before as an "old value". Cannot accept."
          =gfModalGen("INM00335B00000","DIALOG","old value")
          llRet = .F.
        ELSE
          SELECT (lcValues)
          LOCATE FOR cKeyType+cKey_Chg+Old_Key+New_Key = ;
                        lcKeyType+PADR(laFields[lnFields,2],10)+Old_key+lcOldVal
          IF FOUND()
            *-- "This value has been selected before as a "new value". Cannot accept."
            =gfModalGen("INM00335B00000","DIALOG","new value")
            llRet = .F.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    
    IF !llRet
      _CUROBJ = _CUROBJ
    ELSE
      IF EMPTY(laFields[lnFields,7])
        IF !llNew 
          SELECT (lcValues)
          REPLACE Old_Key   WITH lcOldVal,;
                  cStatus   WITH IIF(cStatus='S','M',cStatus),;
                  Entered   WITH IIF(cStatus='S',ENTERED,DATE())
         SAVE TO MEMO mSaveFilt ALL LIKE laFixFltr
        ENDIF
      ENDIF
      *-- Variable to indicate that data has modified
      llcUpDate = .T.

    ENDIF
  ENDIF
ENDIF

llBrowse = .F.

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfBrows
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To browse keys or fields
*!*************************************************************
*! Called from : SMFLDVL.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfCodDes(), lfwBrWhen()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfBrows()
*!*************************************************************
FUNCTION lfBrows
PRIVATE lnAlias,lcDum,lcDumm
STORE SPACE(0) TO lcDum,lcDumm
lnAlias = SELECT(0)
llNoItem = .T.
SELECT (lcValues)
lnOldRecNo = RECNO()

IF SEEK(lcKeyType+laFields[lnFields,2])
  llNoItem = .F.
ENDIF

BROWSE FOR cKey_Chg = laFields[lnFields,2] ;
       FIELDS cMarker=IIF(RECNO()=lnOldRecNo,'>>',' '):2:R:H=' ':W=.F.,;
         lcDum=IIF(laFields[lnFields,6],gfCodDes(ALLTRIM(Old_Key), ;
         ALLTRIM(laFields[lnFields,2])),Old_Key):H='Old Value' :R:30, ;
         lcDumm=IIF(laFields[lnFields,6] , gfCodDes(ALLTRIM(New_Key), ;
         ALLTRIM(laFields[lnFields,2])),New_Key) :H='New Value' :R:30, ;
         ENTERED :H='Date';
       WINDOW SMFLDVL1 ;
       IN WINDOW SMFLDVL ;
       WHEN lfwBrWhen();
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;
       NOCLEAR;
       SAVE;
       NOWAIT;
       TITLE lcBrTtl 

IF llNoItem 
  SHOW GET pbRemove DISABLE
  *wab
  lcNewVal = ''
  lcOldVal = ''
  *wab
ELSE
  SHOW GET pbRemove ENABLE
ENDIF

SHOW WINDOW (lcBrTtl) REFRESH
SELECT (lnAlias)

*!***********************************************************************
*! Function   : lfwBrWhen
*! Developer  : AHMED MOHAMMED IBRAHIM
*! Date       : 10/13/1998
*! Purpose    : When function of the browse to refresh the edit region when 
*!              the record pointer changes.
*!***********************************************************************
*! Parameters : None
*!***********************************************************************
*! Calls      : ....
*!              FUNCTIONS  : gfwCodePop()
*!              PROCEDURES : None
*!***********************************************************************
*! Returns    : None.
*!***********************************************************************
*! Example    : =lfwBrWhen()
*!***********************************************************************
FUNCTION lfwBrWhen

lnOldRecNo = RECNO()
*C102427,1 RAE [BEGIN]
*lcNewVal   = ALLTRIM(&lcValues..New_Key)
*lcOldVal   = ALLTRIM(&lcValues..Old_Key)
*IF  EMPTY(mSaveFilt) 
IF !EMPTY(laFields[lnFields,7])
  lcNewVal   = PADR(SUBSTR(&lcValues..New_Key , LEN(ALLTRIM(SUBSTR(&lcValues..New_Key,1,AT('-',&lcValues..New_Key)-1))) + 2),8)
  lcOldVal   = PADR(SUBSTR(&lcValues..Old_Key , LEN(ALLTRIM(SUBSTR(&lcValues..Old_Key,1,AT('-',&lcValues..Old_Key)-1))) + 2),8)
  lcKeyTemp  = ALLTRIM(SUBSTR(&lcValues..Old_Key,1,AT('-',&lcValues..Old_Key)-1))
 ELSE
  lcNewVal   = ALLTRIM(&lcValues..New_Key)
  lcOldVal   = ALLTRIM(&lcValues..Old_Key)
ENDIF


IF EOF(lcValues) .OR.  EMPTY(mSaveFilt) 
  STORE SPACE(0) TO laFixFltr
*C102427,1 RAE [END]
ELSE
  RESTORE FROM MEMO mSaveFilt ADDITIVE
ENDIF  
IF laFields[lnFields,6]
  lnOldPop = lcOldVal
  = gfwCodePop(@laCodInfo, ALLTRIM(laFields[lnFields,2]), "V,"+lcOldVal,laComp[lnComp,2])
  lnNewPop = lcNewVal
  = gfwCodePop(@laCodInf , ALLTRIM(laFields[lnFields,2]), "V,"+lcNewVal, laComp[lnComp,2])
ENDIF

*-- Variable to enable or disable the remove button.
llNoItem = EOF(lcValues)
IF llNoItem 
  SHOW GET pbRemove DISABLE
  IF laFields[lnFields,6]
    SHOW GET lnNewPop DISABLE
    SHOW GET lnOldPop DISABLE
  ELSE
    SHOW GET lcNewVal DISABLE
    SHOW GET lcOldVal DISABLE
    SHOW GET ibNewVal DISABLE
    SHOW GET ibOldVal DISABLE
  ENDIF
ELSE
  SHOW GET pbRemove ENABLE
  IF laFields[lnFields,6]
    SHOW GET lnNewPop ENABLE
    IF cStatus = 'A'
      SHOW GET lnOldPop ENABLE
    ELSE
      SHOW GET lnOldPop DISABLE
    ENDIF
  ELSE
    SHOW GET lcNewVal ENABLE
    SHOW GET ibNewVal ENABLE
    IF cStatus = 'A'
      SHOW GET lcOldVal ENABLE
      SHOW GET ibOldVal ENABLE
    ELSE
      SHOW GET lcOldVal DISABLE
      SHOW GET ibOldVal DISABLE
    ENDIF
  ENDIF
ENDIF
llNew = .F.
SHOW GET pbAdd ENABLE
SHOW WINDOW (lcBrTtl) REFRESH SAME


*!*************************************************************
*! Name      : lfReadAct
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/13/1998
*! Purpose   : READ Activate function of SMFLDVL.SCX
*!*************************************************************
*! Calls     : .....
*!             FUNCTIONS  : gfClearKey()
*!             PROCEDURES : None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
*-- Restore old key settings and clear the current settings
=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/13/1998
*! Purpose   : READ Deactivate function of SMFLDVL.SCX
*!*************************************************************
*! Calls     : lfTrapKey()
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfReadDeact()
*!*************************************************************
FUNCTION lfReadDeact
*-- Key traps for the browse
IF WONTOP() = lcBrTtl 
  ON KEY LABEL TAB        llDummy = lfTrapKey('SMFLDVL2', OBJNUM(lcOldVal))
  ON KEY LABEL BACKTAB    llDummy = lfTrapKey('SMFLDVL2', OBJNUM(pbClose))
  ON KEY LABEL ESC        DO lfEscape
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfTrapKey
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/13/98
*! Purpose   : Trap of keys
*!*************************************************************
*! Passed Parameters  :  lcWindName : Window name to activate
*!                       lnObjNum   : object number to focus.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  llDummy = lfTrapKey('MFG10002', OBJNUM(pbNew)
*!*************************************************************
FUNCTION lfTrapKey
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum

*!*************************************************************
*! Name      : lfEscape
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/13/1998
*! Purpose   : Trap of keys
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfEscape()
*!*************************************************************
FUNCTION lfEscape
ACTIVATE WINDOW JON100A4
_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13) CLEAR


*!*************************************************************
*! Name      : lfwOd
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : When function of the old and new value fields
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOd()
*!*************************************************************
FUNCTION lfwOd
*-- Get the old value of the object
lcOld = EVAL(VARREAD())

*!*************************************************************
*! Name      : lfvPrcd
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Valid function of the proceed button
*!*************************************************************
*! Called from : SMFLDCH.SPR
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfModalGen(), lfOpClFile(), lfBldTmp(),
*!                            lfbrwOpn(),
*!                          &lcSpecFn Special function of the field processed
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrcd()
*!*************************************************************
FUNCTION lfvPrcd

PRIVATE laOpnFl,lnCount,lnC,lcSkip, lnAlias
lnAlias = SELECT(0)
*-- CHECK THAT ALL USERS IS OUT THE SYSTEM
SELECT SYUSTATC
LOCATE FOR cObj_name ='OLDVARS' .AND. syuStatc.cStation <> gcStation ;
            .AND. syuStatc.cComp_ID = ALLTRIM(laComp[lnComp,2])

*** If at least one user have a record in the static files
*** keep locating for all users
DO WHILE FOUND()
  lcUser    = syuStatc.cUser_ID        
  lcStion   = syuStatc.cStation
  IF (lcUser = gcUser_Id .AND. lcStion = gcStation )
    CONTINUE
  ELSE
    lcOldRep = SET('REPROCESS')
    SET REPROCESS TO 1
    IF RLOCK('SYUSTATC')
      UNLOCK IN ALIAS('SYUSTATC')
      SET REPROCESS TO lcOldRep
      CONTINUE
    ELSE
      SET REPROCESS TO lcOldRep
      =gfModalGen ('INM00025B00000','Alert')  
      *glQuitting = .T.
      RETURN .T.
    ENDIF
  ENDIF
ENDDO
DIMENSION laOpnFl[1,4],laRelF[1,2], laAltFlds[1,2]
laOpnFl[1,1] = ''
lnCount = 1
STORE SPACE(0) TO laRelF,lcMainF,lcSkip
lnCnt = 1
SET ORDER TO TAG cFld_Name IN SYDFLFLD
lnProc = SET('REPROCESS')
SET REPROCESS TO 20
SELECT KEY_CHG
=SEEK(lcKeyType)

*B603725,1  AMM if old or new value is empty, don't continue
*SCAN WHILE cKeyType=lcKeyType  FOR KeyStatus = 'O'
SCAN WHILE cKeyType=lcKeyType  FOR KeyStatus = 'O' .AND. !EMPTY(OLD_KEY) .AND. !EMPTY(NEW_KEY)
*B603725,1  AMM end
  IF SEEK(cKeyType+cKey_Chg,'SYDFDVLD') .AND. !EMPTY(SYDFDVLD.mSpcFunc)
      lcSpecFn = STRTRAN( SYDFDVLD.mSpcFunc, '()','("R")'  )
      llRet    = &lcSpecFn
      IF llRet
        SELECT KEY_CHG
        REPLACE KeyStatus  WITH 'C',;
            Date_Procd WITH DATE()
      ENDIF
      LOOP
  ENDIF  
  IF SEEK(cKeyType+cKey_Chg,'SYDFDCHG')
    SELECT SYDFDCHG
    IF KEY_CHG.cKeyType = 'K'
      SCAN WHILE cKeyType+cFld_Name = Key_Chg.cKeyType + Key_Chg.cKey_Chg
        *-- If main field record , Get all files that has that field then replace old value by new value
        IF EMPTY(SYDFDCHG.cAltField)
          SELECT SYDFLFLD
          =SEEK(SYDFDCHG.cFld_Name)
          SCAN WHILE cFld_Name = SYDFDCHG.cFld_Name
            *-- Activate the file by openning or selecting it.
            *-- Don't replace the field value in the main file 
            *C102427,1 RAE [BEGIN] Replace the field value in the main file 
            *IF SYDFLFLD.cFile_Nam = SYDFDCHG.cFile_Nam      &&.AND. EMPTY(SYDFDCHG.cAlt_Fil)
            *  LOOP             
            *ENDIF
            *C102427,1 RAE [END]
            *-- Open necessary file.
            IF  !lfOpClFile('O',ALLTRIM(SYDFLFLD.cFile_Nam),'')
              LOOP
            ENDIF
            
            WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) +' file.' NOWAIT
            *-- Replace Old value by new value
            lcFldtmp = ALLTRIM(SYDFDCHG.cFld_Name)
            *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
            *REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                          FOR &lcFldtmp = ALLTRIM(Key_Chg.Old_Key) 
            REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                          FOR ALLTRIM(&lcFldtmp) == ALLTRIM(Key_Chg.Old_Key) 
            *B607346,1 ALB Bug when Change keys value that have shared char. [end]
          ENDSCAN
          *B040090,1 EIH  03/09/2006 Fix bug that not update notepad file when we change the account code [Begin].
          IF lcFldtmp = 'ACCOUNT'
            =lfOpClFile('O',ALLTRIM('NOTEPAD'),'')
            WAIT WINDOW 'Updating NOTEPAD file.' NOWAIT
            REPLACE ALL KEY WITH Key_Chg.New_Key ;
                        FOR ALLTRIM(KEY) == ALLTRIM(Key_Chg.Old_Key) AND TYPE == 'A'
          ENDIF
          *B040090,1 EIH  03/09/2006 [END]
          
        ELSE   && i.e. !EMPTY(SYDFDVLD.cAltField)
          *-- Open necessary files
          IF !lfOpClFile('O',ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag) )
            LOOP
          ENDIF

          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) + ' file.' NOWAIT

          *-- Replace Old value by new value
          lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
          
          *B607026,1 WAB (Start) -- error in the Flter Expersion "Invalid function"
          *REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
          *         FOR &lcFldtmp = ALLTRIM(Key_Chg.Old_Key) ;
          *         .AND. IIF(!EMPTY(SYDFDCHG.mFltExpr),ALLTRIM(SYDFDCHG.mFltExpr),.T.)
          lcFltExp = lfGetExp()
          *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
          *REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                   FOR &lcFldtmp = ALLTRIM(Key_Chg.Old_Key) ;
                   .AND. &lcFltExp
          REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                   FOR ALLTRIM(&lcFldtmp) == ALLTRIM(Key_Chg.Old_Key) ;
                   .AND. &lcFltExp
          *B607346,1 ALB Bug when Change keys value that have shared char. [end]
         *B607026,1 WAB (End)
        ENDIF
      ENDSCAN
    ELSE  && i.e. cKeyType = 'F'
      DIMENSION laRelF[1,2]
      lnCnt = 1
      STORE SPACE(0) TO laRelF
      SELECT SYDFDCHG
      STORE SPACE(0) TO laAltFlds
      lnNum = 0
      *-- Scan loop to Open necessary files and prepare relation between 
      *-- main file and its cheldren.
      *-- laAltFlds array hold each field with its file and filter 
      *-- expression if any to update all fields on one loop on the main 
      *-- file.
      SCAN WHILE cKeyType+cFld_Name = 'F'+Key_CHG.cKey_Chg
        IF EMPTY(cAltField)
          lcMainF = SYDFDCHG.cFile_Nam
        ENDIF
        lnNum = lnNum + 1
        DIMENSION laAltFlds[lnNum,3]
        laAltFlds[lnNum,1] = SYDFDCHG.cFile_Nam
        laAltFlds[lnNum,2] = IIF(EMPTY(SYDFDCHG.cAltField),SYDFDCHG.cFld_name,SYDFDCHG.cAltField)
        laAltFlds[lnNum,3] = IIF(EMPTY(SYDFDCHG.mFltExpr),".T.",ALLTRIM(SYDFDCHG.mFltExpr))
        IF ASCAN(laRelF,SYDFDCHG.cFile_Nam) = 0 .AND. lfOpClFile('O',ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag) )
          IF !EMPTY(SYDFDCHG.mRelation)
            *-- laRelF array hold each child file with its relation.
            laRelF[lnCnt,1] = SYDFDCHG.cFile_Nam
            laRelF[lnCnt,2] = ALLTRIM(SYDFDCHG.mRelation)
            lnCnt = lnCnt+1 
            DIMENSION laRelF[lnCnt,2]
            STORE SPACE(0) TO laRelF[lnCnt,1]
          ENDIF
        ELSE
          LOOP
        ENDIF
      ENDSCAN
      
      *-- Set necessary relations to the main file
      SELECT (lcMainF)
      FOR lnC = 1 TO ALEN(laRelF,1)
        IF !EMPTY(laRelF[lnC,1])
          SET RELATION TO &laRelF[lnC,2] INTO &laRelF[lnC,1] ADDITIVE
          *-- Collect files have relation with the main file to set the skip sequence
          lcSkip = lcSkip+IIF(EMPTY(lcSkip),'',',')+laRelF[lnC,1]
        ENDIF
      ENDFOR
      SET SKIP TO &lcSkip
      *lcFldtmp = EVAL(SYDFDCHG.cFld_Name)
      *-- Get the saved filter

      *B606594,1 WAB (Start) -fix the problem of invalid memo variable file 
      *RESTORE FROM MEMO KEY_CHG.mSaveFilt ADDITIVE
      IF !EMPTY(KEY_CHG.mSaveFilt) 
        RESTORE FROM MEMO KEY_CHG.mSaveFilt ADDITIVE
      ELSE
        STORE SPACE(0) TO laFixFltr
      ENDIF
      *B606594,1 WAB (End)
      
      lcFilt = ALLTRIM(gfGenFlt('laFixFltr', .T.))
      lcFilt=IIF(EMPTY(lcFilt),".T.",lcFilt)
      
      *-- Scan on the main file and replace values in its children
      WAIT WINDOW 'Updating ' + ALLTRIM(lcMainF) +' file.' NOWAIT

      SCAN FOR &lcFilt 
        FOR lnC = 1 TO ALEN(laAltFlds,1)
          SELECT &laAltFlds[lnC,1]
          lcFldtmp = laAltFlds[lnC,2]
          IF &lcFldTmp = ALLTRIM(KEY_CHG.Old_Key) .AND. &laAltFlds[lnC,3]
            REPLACE &lcFldtmp WITH Key_Chg.New_Key
          ENDIF
        ENDFOR
      ENDSCAN
    ENDIF
  ENDIF
  
  *--B803576,1 RAMY [start]
  IF UPPER(ALLTRIM(laFields[lnFields,1])) $ "CUSTOMER|VENDOR"
    lnAlias = SELECT(0)
    IF !USED('SYSCHDUL')
      =gfOpenFile(gcSysHome + 'SYSCHDUL' , '' , 'SH')
      SELECT SYSCHDUL
      SET ORDER TO SCHACCT ASCENDING
    ELSE
      SELECT SYSCHDUL
      SET ORDER TO
      SET ORDER TO SCHACCT ASCENDING
    ENDIF
*    lcContTyp = IIF(UPPER(ALLTRIM(laFields[lnFields,1])) = 'CUSTOMER' , 'C' , 'V')
*    IF SEEK(lcContTyp + PADR(Key_Chg.Old_Key,8))
*      SCAN REST WHILE cContType = lcContTyp FOR cCont_ID  = PADR(Key_Chg.Old_Key,8)
        WAIT WINDOW 'Updating ' + 'SYSCHDUL' +' file.' NOWAIT
*        REPLACE cCont_ID WITH PADR(Key_Chg.New_Key,5)
*        GO TOP
*        IF !SEEK(lcContTyp + PADR(Key_Chg.Old_Key,5))
*          EXIT
*        ENDIF
*      ENDSCAN
*    ENDIF
    REPLACE ALL cCont_Id with PADR(Key_Chg.New_Key,8) FOR cCont_Id = PADR(Key_Chg.Old_Key,8)
    IF !USED('CONTACT')
      = gfOpenFile(ALLTRIM(laComp[lnComp,3]) + 'CONTACT' , 'CONTACT' , 'SH')
      SELECT CONTACT
    ELSE
      SELECT CONTACT
    ENDIF
*    IF SEEK(lcContTyp + PADR(Key_Chg.Old_Key,8))
*      SCAN REST WHILE cContType + cCont_ID = lcContTyp + PADR(Key_Chg.Old_Key,8)
        WAIT WINDOW 'Updating ' + 'CONTACT' +' file.' NOWAIT
*        REPLACE cCont_ID WITH PADR(Key_Chg.New_Key,8)
*      ENDSCAN
*    ENDIF
*    SELECT (lnAlias)
    *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
    *B604438,1 HBG 28/05/2001 Move this line inside the condition to 
    *B604438,1                fix Bug of Variabel 'cCont_Id' not found [Begin]
    
    *REPLACE ALL cCont_Id with PADR(Key_Chg.New_Key,8) FOR cCont_Id = PADR(Key_Chg.Old_Key,8)
    REPLACE ALL cCont_Id with PADR(Key_Chg.New_Key,8) FOR ALLTRIM(cCont_Id) == ALLTRIM(PADR(Key_Chg.Old_Key,8))
    *B604438,1 [End] 
    *B607346,1 ALB Bug when Change keys value that have shared char. [end]
  ENDIF
  *B604438,1 HBG 28/05/2001 Move this line up inside the condition to 
  *B604438,1                fix Bug of Variabel 'cCont_Id' not found [Begin]
  *REPLACE ALL cCont_Id with PADR(Key_Chg.New_Key,8) FOR cCont_Id = PADR(Key_Chg.Old_Key,8)
  *B604438,1 [End]
  
  *--B803576,1 RAMY [END]
  
  *-- Close opened files (Files opened to change values in them)
  = lfOpClFile('C')
  SELECT KEY_CHG
  REPLACE KeyStatus  WITH 'C',;
          Date_Procd WITH DATE()

ENDSCAN
*-- Build the temporary files again
=lfBldTmp()
*-- Refresh the browse
=lfbrwOpn()

SET REPROCESS TO (lnProc)

*!*************************************************************
*! Name      : lfOpClFile
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To Open files then close them after processing
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfOpenFile()
*!               PROCEDURES : 
*!*************************************************************
*! Passed Parameters : lcStat,lcfil,lcTag
*!                     lcStat = "O" for Open, "C" for close
*!                     lcFile File to be opened
*!                     lcTag  Tag name of the file
*!*************************************************************
*! Return      : .T. if the file opened successfully, else .F.
*!*************************************************************
*! Example     : = lfOpClFile()
*!*************************************************************
FUNCTION lfOpClFile

PARAMETERS lcStat,lcfil,lcTag
IF lcStat='O'
  IF FILE(ALLTRIM(laComp[lnComp,3])+lcFil+'.DBF')
    IF !USED(lcFil) 
      laOpnFl[lnCount,1] = lcFil
      *B602722,1  AMM Remove the spaces from the file name string
      *laOpnFl[lnCount,2] = gfOpenFile(ALLTRIM(laComp[lnComp,3])+lcFil,lcTag,'SH')
      laOpnFl[lnCount,2] = gfOpenFile(ALLTRIM(laComp[lnComp,3])+ALLTRIM(lcFil),lcTag,'SH')
      *B602722,1  AMM end
      llLock = FLOCK()
      IF !llLock
        *-- "File " + &lcFil + " could not be locked. This file will be ignored."
        *--                        < OK >
        =gfModalGen("INM00333B00000","DIALOG",lcFil)
        RETURN .F.
      ENDIF
      laOpnFl[lnCount,3] = ''
      laOpnFl[lnCount,4] = ''
      lnCount = lnCount+1
      DIMENSION laOpnFl[lnCount,4]
      laOpnFl[lnCount,1] = SPACE(0)
    ELSE
      IF DBF(lcFil) = UPPER(ALLTRIM(laComp[lnComp,3])+lcFil+'.dbf')
        SELECT (lcFil)
      ELSE
        USE IN (lcFil)
        laOpnFl[lnCount,1] = lcFil
        *B602722,1  AMM Remove the spaces from the file name string
        *laOpnFl[lnCount,2] = gfOpenFile(ALLTRIM(laComp[lnComp,3])+lcFil,lcTag,'SH')
        laOpnFl[lnCount,2] = gfOpenFile(ALLTRIM(laComp[lnComp,3])+ALLTRIM(lcFil), lcTag,'SH')
        *B602722,1  AMM
        llLock = FLOCK()
        IF !llLock
          *-- "File " + &lcFil + " could not be locked. This file will be ignored."
          *--                        < OK >
          =gfModalGen("INM00333B00000","DIALOG",lcFil)
          RETURN .F.
        ENDIF

        laOpnFl[lnCount,3] = DBF(lcFil)
        laOpnFl[lnCount,4] = ORDER(lcFil)
        lnCount = lnCount+1
        DIMENSION laOpnFl[lnCount,4]
        laOpnFl[lnCount,1] = SPACE(0)
      ENDIF
    ENDIF
  ELSE
    RETURN .F.
  ENDIF
ELSE
  *-- Close files
  FOR lnc = 1 TO ALEN(laOpnFl,1)
    *-- If the file opened by me, close it
    IF !EMPTY(laOpnFl[lnC,1]) .AND. USED(laOpnFl[lnC,1]) .AND. laOpnFl[lnC,2]
      USE IN (laOpnFl[lnC,1])
      *-- If opened instead of another file, open the other file
      IF !EMPTY(laOpnFl[lnC,3])
        = gfOpenFile(laOpnFl[lnC,3],laOpnFl[lnC,4],'SH')
      ENDIF
    ENDIF
  ENDFOR
  DIMENSION laOpnFl[1,4]
  laOpnFl[1,1] = SPACE(0)
  lnCount = 1
ENDIF

*!*************************************************************
*! Name      : lfVldBrw
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To browse the old or new value field based on the data 
*!             in SYDFDVLD file
*!*************************************************************
*! Called from : lfvNewVal(), lfvOldVal()
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcVal
*!*************************************************************
*! Return      : lcVal
*!*************************************************************
*! Example     : = lfVldBrw()
*!*************************************************************
FUNCTION lfVldBrw
PARAMETERS lcVal

*-- Go to the field record in the SYDFDVLD table to get the browse information
IF SEEK(lcKeyType+laFields[lnFields,2],'SYDFDVLD') .AND. ;
    (!SEEK( ALLTRIM(SYDFDVLD.mKeyExpr)+ALLTRIM(lcVal),ALLTRIM(SYDFDvld.cMainFile)  ) .OR. llBrowse)
  SELECT (ALLTRIM(SYDFDvld.cMainFile))
  *-- Get the browse fields

  *B604119,1 BWA 28/02/2001 Fix the bug of "Compiled Code For This Line Too Long" ,
  *B604119,1                in case the the user select customer.[START]
  *lcBrFields=gfDbfField(ALLTRIM(SYDFDvld.cMainFile))
  IF "Customer" $ lafields[lnfields,1]
  
      lcBrFields = "ACCOUNT    :R :H= 'Account Code'       ," +;
                 "STNAME     :R :H= 'STNAME '            ," +;
                 "PHONE1     :R :H= 'Phone  '            ," +;
                 "BUYER      :R :H= 'BUYER  '            ," +;
                 "SALESREP   :R :H= 'Sales Representive' ," +;
                 "CADDRESS1  :R :H= 'CADDRESS1'          ," +;
                 "CADDRESS2  :R :H= 'CADDRESS2'          ," +;
                 "CADDRESS3  :R :H= 'CADDRESS3'          ," +;
                 "CADDRESS4  :R :H= 'CADDRESS4'          ," +;
                 "CADDRESS5  :R :H= 'CADDRESS5'          ," +;
                 "CADDRESS12 :R :H= 'CADDRESS12'         ," +;
                 "CADDRESS22 :R :H= 'CADDRESS22'         ," +;
                 "CADDRESS32 :R :H= 'CADDRESS32'         ," +;
                 "CADDRESS42 :R :H= 'CADDRESS42'         ," +;
                 "CADDRESS52 :R :H= 'CADDRESS52'         ," +;
                 "NETBAL     :R :H= 'NETBAL'             " 
  ELSE
    lcBrFields=gfDbfField(ALLTRIM(SYDFDvld.cMainFile))
  ENDIF
  *B604119,1 BWA 28/02/2001 [END]

  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Select Value"
  *-- Get the browse title
  IF SEEK(SYDFDvld.cMainFile,'SYDFILES')
    lcFile_ttl = SYDFILES.cFile_ttl
  ENDIF
  lcFilt = IIF(!EMPTY(SYDFDVLD.mFltExpr),'FOR ' +ALLTRIM(SYDFDVLD.mFltExpr),;
            IIF(!EMPTY(SYDFDVLD.mKeyExpr), ;
           ALLTRIM(SYDFDVLD.mKeyExpr),''))
  IF !EMPTY(SYDFDVLD.mFltExpr)
    *B602722,1  AMM Fix not to cause a bug
    *=gfBrows(lcFilt,SYDFDVLD.cOrg_Fld,"laTemp")
    =gfBrows(lcFilt,ALLTRIM(SYDFDVLD.cOrg_Fld),"laTemp")
    *B602722,1  AMM end
  ELSE
    *B602722,1  AMM Fix not to cause a bug
    *=gfBrows("lcFilt",SYDFDVLD.cOrg_Fld,"laTemp")
    =gfBrows("lcFilt",ALLTRIM(SYDFDVLD.cOrg_Fld),"laTemp")
    *B602722,1  AMM end
  ENDIF
  lcVal = laTemp[1]
ENDIF
IF EMPTY(lcVal)
  lcVal = lcOld
  KEYBOARD '{ALT+B}'
  llRet = .F.
ENDIF

RETURN lcVal

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Ahmed Mohammed Mohammed (AMM)
*! Date      : 10/13/98
*! Purpose   : Activate folder when Change.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfbrwOpn(), lfbrwHst()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfActFolder()
*!*************************************************************

FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
    SHOW GETS WINDOW (lcWinHist)  DISABLE ONLY
    =lfbrwOpn()
    SHOW GET lnFields ENABLE
    SHOW GET lnValues ENABLE
  CASE lnActFolder = 2
    SHOW GETS WINDOW (lcWinKey)  DISABLE ONLY
    =lfbrwHst()
ENDCASE


*!*************************************************************
*! Name      : lfbrwOpn
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To Browse open keys or fields.
*!*************************************************************
*! Called from : SMFLDCH.SPR, lfCrtFiles(), lfvFiles(), lfvFields(), 
*!               lfvValues(), lfvPrcd(), lfActFolder()
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfCodDes(), lfwbrwOpn()
*!               PROCEDURE  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfbrwOpn()
*!*************************************************************
FUNCTION lfbrwOpn

PRIVATE lnAlias,lcDum,lcDumm
STORE SPACE(0) TO lcDum,lcDumm
lnAlias = SELECT(0)

SELECT (lcNValues)
GO TOP
lnOldRNo = RECNO()
*E00789,1 Hesham (Start)
*BROWSE KEY lcKeyType FOR IIF(laFields[lnFields,2]=lcDummyCom,.T.,;
                              cKey_Chg = laFields[lnFields,2] );
       FIELDS cMarker=IIF(RECNO()=lnOldRNo,'>>',' '):2:R:H=' ':W=.F.,;
         cFld_Head :R:H = IIF(lcKeyType='K','Key','Field'),;
         lcDum=IIF(lKey,LOOKUP(CODES.cDiscrep, ALLTRIM(laComp[lnComp,2])+;
         ALLTRIM(Old_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         Old_Key) :H='Old Value' :R:30, ;
         lcDumm=IIF(lKey , LOOKUP(CODES.cDiscrep, ALLTRIM(laComp[lnComp,2])+;
         ALLTRIM(New_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         New_Key) :H='New Value' :R:30, ENTERED :H='Date';
       WINDOW SMFLDCH3 ;
       IN WINDOW (lcWinKey) ;
       WHEN lfwbrwOpn();
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;
       NOCLEAR;
       SAVE;
       NOWAIT;
       TITLE lcbrwOpen


BROWSE KEY lcKeyType FOR IIF(laFields[lnFields,2]=lcDummyCom,.T.,;
                              cKey_Chg = laFields[lnFields,2] );
       FIELDS cMarker=IIF(RECNO()=lnOldRNo,'>>',' '):2:R:H=' ':W=.F.,;
         cFld_Head :R:H = IIF(lcKeyType='K','Key','Field'),;
         lcDum=IIF(lKey,LOOKUP(CODES.cDiscrep, 'N'+;
         ALLTRIM(Old_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         Old_Key) :H='Old Value' :R:30, ;
         lcDumm=IIF(lKey , LOOKUP(CODES.cDiscrep, 'N'+;
         ALLTRIM(New_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         New_Key) :H='New Value' :R:30, ENTERED :H='Date';
       WINDOW SMFLDCH3 ;
       IN WINDOW (lcWinKey) ;
       WHEN lfwbrwOpn();
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;
       NOCLEAR;
       SAVE;
       NOWAIT;
       TITLE lcbrwOpen

*E00789,1 Hesham (End)
SHOW WINDOW (lcbrwOpen) REFRESH
SELECT (lnAlias)

*!***********************************************************************
*! Function   : lfwbrwOpn
*! Developer  : AHMED MOHAMMED IBRAHIM
*! Date       : 10/13/98
*! Purpose    : When function of the browse to refresh the edit region when 
*!              the record pointer changes.
*!***********************************************************************
*! Parameters : None
*!***********************************************************************
*! Calls      : None.
*!***********************************************************************
*! Returns    : None.
*!***********************************************************************
*! Example    : =lfwbrwOpn()
*!***********************************************************************
*! NOTES      :
*!***********************************************************************
FUNCTION lfwbrwOpn
lnOldRNo = RECNO()
SHOW WINDOW (lcbrwOpen) REFRESH SAME

*!*************************************************************
*! Name      : lfbrwHst
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To browse the history keys or fields (keys processed)
*!*************************************************************
*! Called from : lfActFolder()
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfCodDes(), lfwbrwHst()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfbrwHst()
*!*************************************************************
FUNCTION lfbrwHst

PRIVATE lnAlias,lcDum,lcDumm
STORE SPACE(0) TO lcDum,lcDumm
lnAlias = SELECT(0)
IF !USED('KEY_CHG')
  RETURN
ENDIF
SELECT (lcHistory)
GO TOP
lnORecNo = RECNO()

*E00789,1 Hesham (Start)
*BROWSE KEY lcKeyType FOR IIF(laFields[lnFields,2]=lcDummyCom,.T.,;
                              cKey_Chg = laFields[lnFields,2] );
       FIELDS cMarker=IIF(RECNO()=lnORecNo,'>>',' '):2:R:H=' ':W=.F.,;
         cFld_Head :R:H = IIF(lcKeyType='K','Key','Field'),;
         lcDum=IIF(lKey,LOOKUP(CODES.cDiscrep, ALLTRIM(laComp[lnComp,2])+;
         ALLTRIM(Old_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         Old_Key) :H='Old Value' :R:30, ;
         lcDumm=IIF(lKey , LOOKUP(CODES.cDiscrep, ALLTRIM(laComp[lnComp,2])+;
         ALLTRIM(New_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         New_Key) :H='New Value' :R:30, ;
         ENTERED :R:H='Entered', Date_Procd :R:H='Processed';
       WINDOW SMFLDCH4 ;
       IN WINDOW (lcWinHist) ;
       WHEN lfwbrwHst();
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;
       NOCLEAR;
       SAVE;
       NOWAIT;
       TITLE lcbrwHst

BROWSE KEY lcKeyType FOR IIF(laFields[lnFields,2]=lcDummyCom,.T.,;
                              cKey_Chg = laFields[lnFields,2] );
       FIELDS cMarker=IIF(RECNO()=lnORecNo,'>>',' '):2:R:H=' ':W=.F.,;
         cFld_Head :R:H = IIF(lcKeyType='K','Key','Field'),;
         lcDum=IIF(lKey,LOOKUP(CODES.cDiscrep, 'N'+;
         ALLTRIM(Old_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         Old_Key) :H='Old Value' :R:30, ;
         lcDumm=IIF(lKey , LOOKUP(CODES.cDiscrep, 'N'+;
         ALLTRIM(New_Key)+ 'N' +ALLTRIM(cKey_Chg), CODES.cCode_No, 'CODES'),;
         New_Key) :H='New Value' :R:30, ;
         ENTERED :R:H='Entered', Date_Procd :R:H='Processed';
       WINDOW SMFLDCH4 ;
       IN WINDOW (lcWinHist) ;
       WHEN lfwbrwHst();
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;
       NOCLEAR;
       SAVE;
       NOWAIT;
       TITLE lcbrwHst

*E00789,1 Hesham (End)
SHOW WINDOW (lcbrwHst) REFRESH
SELECT (lnAlias)

*!***********************************************************************
*! Function   : lfwbrwHst
*! Developer  : AHMED MOHAMMED IBRAHIM
*! Date       : 05/20/98
*! Purpose    : When function of the browse to refresh the edit region when 
*!              the record pointer changes.
*!***********************************************************************
*! Parameters : None
*!***********************************************************************
*! Called from : lfbrwHst()
*!*************************************************************
*! Calls      : None.
*!***********************************************************************
*! Returns    : None.
*!***********************************************************************
*! Example    : =lfwbrwHst()
*!***********************************************************************
*! NOTES      :
*!***********************************************************************
FUNCTION lfwbrwHst

lnORecNo = RECNO()

SHOW WINDOW (lcbrwHst) REFRESH SAME


*!*************************************************************
*! Name      : lfRdActOp
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/13/98
*! Purpose   : READ Activate function of SMFLDCH.SCX
*!*************************************************************
*! Calls     : ....
*!             FUNCTIONS  : gfClearKey()
*!             PROCEDURES : None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfRdActOp()
*!*************************************************************
FUNCTION lfRdActOp
*-- Restore old key settings and clear the current settings
=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcbrwOpen)


*!*************************************************************
*! Name      : lfRdDctOP
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/13/98
*! Purpose   : READ Deactivate function of SMFLDVL.SCX
*!*************************************************************
*! Calls     : lfTrapKey
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfRdDctOP()
*!*************************************************************
FUNCTION lfRdDctOP
*-- Key traps for the browse
IF WONTOP() = lcbrwOpen
  ON KEY LABEL TAB        llDummy = lfTrapKey('SMFLDCH5', OBJNUM(lnFields))
  ON KEY LABEL BACKTAB    llDummy = lfTrapKey('SMFLDCH5', OBJNUM(cbValues))
  ON KEY LABEL ESC        DO lfEsc
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfEsc
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/13/98
*! Purpose   : Trap of keys
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfEsc()
*!*************************************************************
FUNCTION lfEsc

ACTIVATE WINDOW (gwcContrl1)
_CUROBJ = OBJNUM(pbCls)
KEYBOARD CHR(13) CLEAR


*!*************************************************************
*! Name      : lfMajor
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Special function of the major key for Old , New Values 
*!             and replacing process
*!*************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfOpenFile(), gfStyBrw(), lfOpClFile()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : lcStatus
*!                     lcStatus = "O"  Validation of old value object
*!                     lcStatus = "N"  Validation of New value object
*!                     lcStatus = "R"  Replacing process
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajor()
*!*************************************************************
*! Notes       : Name of this function must be in mSpcFunc field 
*!               in the SYDFDVLD file.
*!*************************************************************
FUNCTION lfMajor
PARAMETERS lcStatus

PRIVATE lcSrchCod, lcCod, lnLen, lcOrd, llopSGVL, llopsty, llopscl
STORE .F. TO llopSGVL, llopsty, llopscl
llVld = .T.
STORE SPACE(0) to lcSrchCod, lcCod, lcOrd

IF FILE(ALLTRIM(laComp[lnComp,3])+'ICISTRU.DBF') 
  llICISTRU = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICISTRU','Segno','SH')
  IF !SEEK('U1')
    IF llICISTRU
      =gfCloseFile('ICISTRU')
    ENDIF
    RETURN .F.
  ENDIF
  IF llICISTRU
    =gfCloseFile('ICISTRU')
  ENDIF
ELSE
  RETURN .F.
ENDIF

IF USED('STYLE') .AND. DBF('STYLE') # UPPER(ALLTRIM(laComp[lnComp,3])+'STYLE.dbf')
  USE IN STYLE
ENDIF
llopsty = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'STYLE','CSTYLE','SH')

IF USED('SCALE') .AND. DBF('SCALE') # UPPER(ALLTRIM(laComp[lnComp,3])+'SCALE.dbf')
  USE IN SCALE
ENDIF
llopscl = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'SCALE','SCALE','SH')

IF USED('ICSEGVAL') .AND. DBF('ICSEGVAL') # UPPER(ALLTRIM(laComp[lnComp,3])+'ICSEGVAL.dbf')
  USE IN ICSEGVAL
ENDIF
llopSGVL =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICSEGVAL','SEGVAL','SH')

DO CASE 
  ***********************************************************************
  *-- Validation of the old value
  CASE lcStatus = 'O'
    *lcOldVal = lfVldBrw(lcOldVal)    
    *B602722,1  AMM Browse if not valid style
    *lcOldVal = gfStyBrw('M',lcOldVal,'',.F.)
    IF !SEEK(lcOldVal, 'STYLE')
      lcOldVal = gfStyBrw('M','','',.F.)
    ENDIF
    *B602722,1  AMM end
  ***********************************************************************
  *-- Validations of the new value
  CASE lcStatus = 'N'
    *-- If not existing major, validate its structure
    IF !SEEK(lcNewVal,'STYLE')
      *lcMajor = gfStyBrw('M',lcMajor,lcNonMjr,.T.)
      *-- 'Style '+ ALLTRIM(lcNewVal) +'does not exist in the style file. 
      *--                     Continue anyway?'
      *--              < YES >             < NO >
      
      *B603725,1  AMM comment out
      *IF gfModalGen("QRM00330B00006","DIALOG",ALLTRIM(lcNewVal) ) = 2
        *lcNewVal = lcOld
        *_CUROBJ = _CUROBJ
        *RETURN
      *ENDIF
      *B603725,1  AMM end
      
      lnLen = 1
      SELECT CODES
      lcOrd = ORDER()
      SET ORDER TO TAG cCode_No
      *-- Loop to get segment by segment
      FOR lnC=1 TO lnMajSeg 
        lcCod = SUBSTR(lcNewVal,lnLen,LEN(laMajSeg[lnC,3]) )
        lnLen = lnLen + LEN(laMajSeg[lnC,3])
        *!   SEGMENT TYPE 'FOTQZCDGS'
        *!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
        *!   C-> Color , D->Division ,G->Group ,S->Size
      
        DO CASE
          CASE laMajSeg[lnC,1] $ 'OTQ'
            llopSGVL =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICSEGVAL','SEGVAL','SH')
            IF !SEEK(STR(lnc,1)+lcCod,'ICSEGVAL') 
              llVld  = .F.
            ENDIF
          CASE laMajSeg[lnC,1] $ 'ZCDG'
            *-- check if these segments is a valid code
            lcSrchCod = IIF(laMajSeg[lnC,1]='Z','SEASON', ;
                         IIF(laMajSeg[lnC,1]='C','COLOR', ;
                          IIF(laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
            lcSrchCod = PADR(lcSrchCod,10)
            *E00789,1 Hesham (Start)
            *IF !SEEK(laComp[lnComp,2]+ lcSrchCod +lcCod,'CODES' )
            IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
            *E00789,1 Hesham (End)
              llVld  = .F.
            ENDIF
            *---MHM
          *-- If the segment is of type scale, check if it is a valid scale
          CASE laMajSeg[lnC,1] = 'S'
            IF !SEEK('S'+lcCod,'SCALE' )
              llVld  = .F.
            ENDIF
        ENDCASE
        IF !llVld
          *-- 'Invalid Major code segments. Cannot add to the file.'
          *--        < OK >
          =gfModalGen("INM00334B00000","DIALOG","Major")
          
          =gfStyBrw('M',lcNewVal,'',.F.)
          EXIT
        ENDIF
      ENDFOR
      SELECT CODES
      SET ORDER TO (lcOrd)
    *B603725,1  AMM if the new value exists in the file, don't accept it.
    ELSE
      *-- Message : Style XXXX already exists in the style file, cannot proceed.
      =gfModalgen('TRM00372B00000',"DIALOG",ALLTRIM(lcNewVal))

      lcNewVal = lcOld
      _CUROBJ = _CUROBJ
      RETURN .F.
    *B603725,1  AMM end
    ENDIF
    *B605057,1 (Begin) Don't accept "?" in new style value.
    IF ('?' $ lcNewVal)
      =gfModalGen("TRM00385B00000","DIALOG")
      llVld = .F.
    ENDIF
    *B605057,1 (End)
  ************************************************************************
  *-- Replacing process
  CASE lcStatus = 'R'
    *-- Validate the new value agian before updating
    lnLen = 1
    SELECT CODES
    lcOrd = ORDER()
    SET ORDER TO TAG cCode_No
    FOR lnC=1 TO lnMajSeg 
      lcCod = SUBSTR(KEY_CHG.New_Key,lnLen,LEN(laMajSeg[lnC,3]) )
      lnLen = lnLen + LEN(laMajSeg[lnC,3])
      *!   SEGMENT TYPE 'FOTQZCDGS'
      *!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
      *!   C-> Color , D->Division ,G->Group ,S->Size
      DO CASE
        CASE laMajSeg[lnC,1] $ 'OTQ'
          IF !SEEK(STR(lnc,1)+lcCod,'ICSEGVAL') 
            llVld  = .F.
          ENDIF
        CASE laMajSeg[lnC,1] $ 'ZCDG'
          lcSrchCod = IIF(laMajSeg[lnC,1]='Z','SEASON', ;
                       IIF(laMajSeg[lnC,1]='C','COLOR', ;
                        IIF(laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
          lcSrchCod = PADR(lcSrchCod,10)
          *E00789,1 Hesham (Start)
          *IF !SEEK(laComp[lnComp,2]+ lcSrchCod +lcCod,'CODES' )
          IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
          *E00789,1 Hesham (End)
            llVld  = .F.
          ENDIF
        CASE laMajSeg[lnC,1] = 'S'
          IF !SEEK('S'+lcCod,'SCALE' )
            llVld  = .F.
          ENDIF
      ENDCASE
      *-- If not a valid major, don't replace or do any thing.
      IF !llVld
        RETURN .F.
      ENDIF
    ENDFOR
  
    SELECT SYDFDCHG
    =SEEK('K'+'STYLE')
    SCAN WHILE cKeyType+cFld_Name = 'K'+'STYLE'
      *-- If main field record , Get all files that has that field then replace old value by new value
      IF EMPTY(SYDFDCHG.cAltField)
        SELECT SYDFLFLD
        =SEEK('STYLE')
        SCAN WHILE cFld_Name = 'STYLE'
          *-- Open necessary file.
          IF  !lfOpClFile('O',ALLTRIM(SYDFLFLD.cFile_Nam),'')
            LOOP
          ENDIF
          WAIT WINDOW 'Updating style file.' NOWAIT

          IF SYDFLFLD.cFile_Nam = 'STYLE'   
            SELECT STYLE
            SET ORDER TO
            *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
            *B603725,1  AMM add spaces to the major upon replacing
            *REPLACE ALL STYLE   WITH ALLTRIM(Key_Chg.New_Key) + ;
                                     SUBSTR(STYLE,lnMajSize+1), ;
                      CSTYMAJOR WITH ALLTRIM(Key_Chg.New_Key) ;
                    FOR CSTYMAJOR = ALLTRIM(Key_Chg.Old_Key)
            *REPLACE ALL STYLE   WITH PADR(ALLTRIM(Key_Chg.New_Key),lnMajSize) + ;
                                     SUBSTR(STYLE,lnMajSize+1), ;
                      CSTYMAJOR WITH ALLTRIM(Key_Chg.New_Key) ;
                    FOR CSTYMAJOR = ALLTRIM(Key_Chg.Old_Key)
            REPLACE ALL STYLE   WITH PADR(ALLTRIM(Key_Chg.New_Key),lnMajSize) + ;
                                     SUBSTR(STYLE,lnMajSize+1), ;
                      CSTYMAJOR WITH ALLTRIM(Key_Chg.New_Key) ;
                    FOR ALLTRIM(CSTYMAJOR) == ALLTRIM(Key_Chg.Old_Key)

            *B603725,1  AMM
            *B607346,1 ALB Bug when Change keys value that have shared char. [end]
            SET ORDER TO CSTYLE
            =SEEK(ALLTRIM(Key_Chg.New_Key))
            *-- Change necessary fields of the style record due to the new segment values.
            SCAN WHILE CSTYMAJOR = ALLTRIM(Key_Chg.New_Key)
              lnLen = 1 
              FOR lnC=1 TO lnMajSeg 
                lcCod = SUBSTR(KEY_CHG.New_Key,lnLen,LEN(laMajSeg[lnC,3]) )
                lnLen = lnLen + LEN(laMajSeg[lnC,3])
                *!   SEGMENT TYPE 'FOTQZCDGS'
                *!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
                *!   C-> Color , D->Division ,G->Group ,S->Size
                DO CASE
                  CASE laMajSeg[lnC,1] = 'Z'
                    REPLACE SEASON WITH lcCod
                  CASE laMajSeg[lnC,1] = 'D'
                    REPLACE CDIVISION WITH lcCod
                  CASE laMajSeg[lnC,1] = 'G'
                    REPLACE CSTYGROUP WITH lcCod
                  CASE laMajSeg[lnC,1] = 'S'
                    REPLACE SCALE WITH lcCod
                  CASE laMajSeg[lnC,1] = 'F'
                    IF !SEEK(STR(lnC,1)+lcCod,'ICSEGVAL')
                      *-- add a record for the free part in icsegval file
                      INSERT INTO ICSEGVAL (cISegNo,cISegVal) VALUES (STR(lnC,1),lcCod)
                    ENDIF
                ENDCASE
              ENDFOR
            ENDSCAN
          ELSE
            *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
            *-- Replace Old value by new value
            *REPLACE ALL STYLE WITH PADR(ALLTRIM(Key_Chg.New_Key),lnMajSize)+;
                                    SUBSTR(STYLE,lnMajSize+1) ;
                   FOR SUBSTR(STYLE,1,lnMajSize) = ALLTRIM(Key_Chg.Old_Key) 
            REPLACE ALL STYLE WITH PADR(ALLTRIM(Key_Chg.New_Key),lnMajSize)+;
                                    SUBSTR(STYLE,lnMajSize+1) ;
                   FOR ALLTRIM(SUBSTR(STYLE,1,lnMajSize)) == ALLTRIM(Key_Chg.Old_Key) 
            *B607346,1 ALB Bug when Change keys value that have shared char. [end]
          ENDIF
        ENDSCAN
      ELSE   && i.e. !EMPTY(SYDFDCHG.cAltField)
        *-- Open necessary files
        IF !lfOpClFile('O',ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag) )
          LOOP
        ENDIF
        WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_Nam) + ' file.' NOWAIT
        *-- Replace Old value by new value
        lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)

        *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
        *B607026,1 WAB (Start) -- error in the Flter Expersion "Invalid function"
        *REPLACE ALL &lcFldtmp WITH PADR(ALLTRIM(Key_Chg.New_Key),lnMajSize)+;
        *                          SUBSTR(&lcFldtmp,lnMajSize+1) ;
        *       FOR SUBSTR(&lcFldtmp,1,lnMajSize) = ALLTRIM(Key_Chg.Old_Key) ;
        *       .AND. IIF(!EMPTY(SYDFDCHG.mFltExpr),ALLTRIM(SYDFDCHG.mFltExpr),.T.)
        lcFltExp = lfGetExp()
        *REPLACE ALL &lcFldtmp WITH PADR(ALLTRIM(Key_Chg.New_Key),lnMajSize)+;
                                  SUBSTR(&lcFldtmp,lnMajSize+1) ;
               FOR SUBSTR(&lcFldtmp,1,lnMajSize) = ALLTRIM(Key_Chg.Old_Key) ;
               .AND. &lcFltExp
        REPLACE ALL &lcFldtmp WITH PADR(ALLTRIM(Key_Chg.New_Key),lnMajSize)+;
                                  SUBSTR(&lcFldtmp,lnMajSize+1) ;
               FOR ALLTRIM(SUBSTR(&lcFldtmp,1,lnMajSize)) == ALLTRIM(Key_Chg.Old_Key) ;
               .AND. &lcFltExp

       *B607026,1 WAB (End) 
       *B607346,1 ALB Bug when Change keys value that have shared char. [end]
      ENDIF
    ENDSCAN
    SELECT CODES
    SET ORDER TO (lcOrd)
    =lfOpClFile('C')
ENDCASE

IF llopSGVL
  USE IN ICSEGVAL
ENDIF
IF llopsty
  USE IN STYLE
ENDIF
IF llopscl
  USE IN SCALE
ENDIF

IF !llVld
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : Special function of the non major key for Old , New Values 
*!             and replacing process
*!*************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!*************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfOpenFile(), gfStyBrw(), lfOpClFile()
*!               PROCEDURES : None
*!*************************************************************
*! Passed Parameters : lcStatus
*!                     lcStatus = "O"  Validation of old value object
*!                     lcStatus = "N"  Validation of New value object
*!                     lcStatus = "R"  Replacing process
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj
PARAMETERS lcStatus

PRIVATE lcSrchCod, lcCod, lnLen, lcOrd, llopSGVL, llopsty, llopscl
STORE .F. TO llopSGVL, llopsty, llopscl
llVld = .T.
STORE SPACE(0) to lcSrchCod, lcCod, lcOrd
IF FILE(ALLTRIM(laComp[lnComp,3])+'ICISTRU.DBF') 
  llICISTRU = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICISTRU','Segno','SH')
  IF !SEEK('U1')
    IF llICISTRU
      =gfCloseFile('ICISTRU')
    ENDIF
    RETURN .F.
  ENDIF
  IF llICISTRU
    =gfCloseFile('ICISTRU')
  ENDIF
ELSE
  RETURN .F.
ENDIF

IF USED('STYLE') .AND. DBF('STYLE') # UPPER(ALLTRIM(laComp[lnComp,3])+'STYLE.dbf')
  USE IN STYLE
ENDIF
llopsty = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'STYLE','CSTYLE','SH')
IF USED('SCALE') .AND. DBF('SCALE') # UPPER(ALLTRIM(laComp[lnComp,3])+'SCALE.dbf')
  USE IN SCALE
ENDIF
llopscl = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'SCALE','SCALE','SH')
IF USED('ICSEGVAL') .AND. DBF('ICSEGVAL') # UPPER(ALLTRIM(laComp[lnComp,3])+'ICSEGVAL.dbf')
  USE IN ICSEGVAL
ENDIF
llopSGVL =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICSEGVAL','SEGVAL','SH')

DO CASE 
  ****************************************************
  *-- Validation of old or new values
  CASE lcStatus $ 'ON'
    lnLen = 1
    SELECT CODES
    lcOrd = ORDER()
    SET ORDER TO TAG cCode_No
    FOR lnC = lnMajSeg + 1 TO ALEN(laMajSeg,1)
      lcCod = SUBSTR(IIF(lcStatus = 'O',lcOldVal,lcNewVal) ,lnLen,LEN(laMajSeg[lnC,3]) )
      lnLen = lnLen + LEN(laMajSeg[lnC,3])
      *!   SEGMENT TYPE 'FOTQZCDGS'
      *!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
      *!   C-> Color , D->Division ,G->Group ,S->Size
     
      DO CASE
        CASE laMajSeg[lnC,1] $ 'OTQ'
          llopSGVL =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICSEGVAL','SEGVAL','SH')
          IF !SEEK(STR(lnc,1)+lcCod,'ICSEGVAL') 
            llVld  = .F.
          ENDIF
        CASE laMajSeg[lnC,1] $ 'ZCDG'
          lcSrchCod = IIF(laMajSeg[lnC,1]='Z','SEASON', ;
                         IIF(laMajSeg[lnC,1]='C','COLOR', ;
                         IIF(laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
          lcSrchCod = PADR(lcSrchCod,10)
          *E00789,1 Hesham (Start)
          *IF !SEEK(laComp[lnComp,2]+ lcSrchCod +lcCod,'CODES' )
          IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
          *E00789,1 Hesham (End)
            llVld  = .F.
          ENDIF
        CASE laMajSeg[lnC,1] = 'S'
          IF !SEEK('S'+lcCod,'SCALE' )
            llVld  = .F.
          ENDIF
      ENDCASE
      
      IF !llVld
        *B605057,1 (Begin) Don't accept "?" in new style value.
        IF ('?' $ lcNewVal)
          =gfModalGen("TRM00385B00000","DIALOG")
          _CUROBJ = _CUROBJ
          EXIT
        ENDIF
        *B605057,1 (End)
        *-- 'Invalid non Major code segments. Cannot add to the file.'
        =gfModalGen("INM00334B00000","DIALOG","non Major")
        *=gfStyBrw('M',lcNewVal,'')
        *B602722,1  AMM Don't leave the field if it is not valid.
        _CUROBJ = _CUROBJ
        *B602722,1  AMM end
        
        *C102592,1 Force to empty the new value if not valid. [Begin]
        lcNewVal = SPACE(LEN(lcNewVal))
        *C102592,1 Force to empty the new value if not valid. [End]

        EXIT
      ENDIF
    ENDFOR
    SELECT CODES
    SET ORDER TO (lcOrd)
  **********************************************************************
  *-- Replacing process
  CASE lcStatus = 'R'
    
    *C102592,1 Check if temp file is empty or not used. [Begin]
    IF !USED(lcTmpStyle)
      IF TYPE('lnCount') = "N"
        PRIVATE lnOldCount
        lnOldCount = lnCount
      ENDIF 
      DO lpSelecSty
      IF TYPE('lnCount') = "N"
        lnCount = lnOldCount
      ENDIF   
    ENDIF
    
    IF EMPTY(lcTmpStyle) OR (USED(lcTmpStyle) AND RECCOUNT(lcTmpStyle)=0)
      *-- Message < No style(s) selected.>
      *-- Buttons <           OK         >
      =gfModalGen("TRM32038B00000","DIALOG",lcStyMajor+"(s)")
      cbValues = .F.
      SHOW GET cbValues
      *KEYBOARD '{ALT+B}'

      IF llopSGVL
        USE IN ICSEGVAL
      ENDIF
      IF llopsty
        USE IN STYLE
      ENDIF
      IF llopscl
        USE IN SCALE
      ENDIF
      RETURN .F.
    ENDIF
    PRIVATE lcStyleTag , lcTmpExist
    lcTmpExist = gfTempName()
    CREATE CURSOR (lcTmpExist) (Style C(19))
    INDEX ON Style TAG (lcTmpExist) OF (gcWorkDir+lcTmpExist+'.CDX')
    IF !llColorExt

      lnMajSegmt  = gfItemMask('SM')  && No. of major segments.
      DIMENSION laMajSeg[1,1]
      = gfItemMask(@laMajSeg)
      
      *-- Loop Around Non Major elements.
      FOR lnI = lnMajSegmt + 1 TO ALEN(laMajSeg,1)
        *-- If you Find Color Type
        IF laMajSeg[lnI,1] = 'C'
          llColorExt = .T.
          lnClrPo    = laMajSeg[lnI,4]
          lnColorLen = laMajSeg[lnI,3]
          EXIT  
        ENDI
      ENDFOR
    ENDIF
    *C102592,1 Check if temp file is empty or not used. [End]
    
    *-- Validate the nonMajor before update
    lnLen = 1
    SELECT CODES
    lcOrd = ORDER()
    SET ORDER TO TAG cCode_No
    FOR lnC = lnMajSeg + 1 TO ALEN(laMajSeg,1)
      lcCod = SUBSTR( Key_CHG.New_Key ,lnLen,LEN(laMajSeg[lnC,3]) )
      lnLen = lnLen + LEN(laMajSeg[lnC,3])
      *!   SEGMENT TYPE 'FOTQZCDGS'
      *!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
      *!   C-> Color , D->Division ,G->Group ,S->Size
     
      DO CASE
        CASE laMajSeg[lnC,1] $ 'OTQ'
          llopSGVL =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'ICSEGVAL','SEGVAL','SH')
          IF !SEEK(STR(lnc,1)+lcCod,'ICSEGVAL') 
            llVld  = .F.
          ENDIF
        CASE laMajSeg[lnC,1] $ 'ZCDG'
          lcSrchCod = IIF(laMajSeg[lnC,1]='Z','SEASON', ;
                         IIF(laMajSeg[lnC,1]='C','COLOR', ;
                          IIF(laMajSeg[lnC,1]='D','CDIVISION','CSTYGROUP')))
            lcSrchCod = PADR(lcSrchCod,10)
            *E00789,1 Hesham (Start)
            *IF !SEEK(laComp[lnComp,2]+ lcSrchCod +lcCod,'CODES' )
            IF !SEEK('N'+ lcSrchCod +lcCod,'CODES' )
            *E00789,1 Hesham (End)
              llVld  = .F.
            ENDIF
            
        CASE laMajSeg[lnC,1] = 'S'
          =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'SCALE','SCALE','SH')
          IF !SEEK('S'+lcCod,'SCALE' )
            llVld  = .F.
          ENDIF
      ENDCASE
      IF !llVld
        RETURN .F.
      ENDIF
    ENDFOR
    
    SELECT SYDFDCHG
    =SEEK('K'+'STYLE')
    SCAN WHILE cKeyType+cFld_Name = 'K'+'STYLE'
      *-- If main field record , Get all files that has that field then replace old value by new value
      IF EMPTY(SYDFDCHG.cAltField)
        
        *C102592,1 Open the Style file with another alias. [Begin]
        USE (gcDataDir+'Style') SHARE AGAIN IN 0 ALIAS StyAgain ORDER TAG Style
        *C102592,1 Open the Style file with another alias. [End]
        
        SELECT SYDFLFLD
        =SEEK('STYLE')
        SCAN WHILE cFld_Name = 'STYLE'
          *-- Open necessary file.
          IF  !lfOpClFile('O',ALLTRIM(SYDFLFLD.cFile_Nam),'')
            LOOP
          ENDIF
          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) + ' file.' NOWAIT           
          IF SYDFLFLD.cFile_Nam = 'STYLE'
            
            *C102592,1 All those lines are moved downward. [Begin]
            *SELECT STYLE
            *SET ORDER TO
            *REPLACE ALL STYLE   WITH SUBSTR(STYLE,1,lnMajSize+1) + ;
            *                         ALLTRIM(Key_Chg.New_Key) ;
            *        FOR SUBSTR(STYLE, lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key)
            *lcStyleTag = ORDER('Style')
            *SET ORDER TO TAG Style IN Style
            *REPLACE ALL Style WITH SUBSTR(Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key) ;
            *        FOR IIF(SEEK(Style,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
            *        SUBSTR(Style, lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key)
            *SET ORDER TO TAG (lcStyleTag) IN Style        
            *SET ORDER TO CSTYLE
            *SCAN FOR SUBSTR(STYLE, lnMajSize+2 ) = ALLTRIM(Key_Chg.New_Key)
            *  lnLen = 1
            *  FOR lnC = lnMajSeg + 1 TO ALEN(laMajSeg,1)
            *    lcCod = SUBSTR(KEY_CHG.New_Key,lnLen,LEN(laMajSeg[lnC,3]) )
            *    lnLen = lnLen + LEN(laMajSeg[lnC,3])
            *    *!   SEGMENT TYPE 'FOTQZCDGS'
            *    *!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
            *    *!   C-> Color , D->Division ,G->Group ,S->Size
            *    DO CASE
            *      CASE laMajSeg[lnC,1] = 'Z'
            *        REPLACE SEASON WITH lcCod
            *      CASE laMajSeg[lnC,1] = 'D'
            *        REPLACE CDIVISION WITH lcCod
            *      CASE laMajSeg[lnC,1] = 'G'
            *        REPLACE CSTYGROUP WITH lcCod
            *      CASE laMajSeg[lnC,1] = 'S'
            *        REPLACE SCALE WITH lcCod
            *    ENDCASE
            *  ENDFOR
            *ENDSCAN
            *C102592,1 All those lines are moved downward. [End]
                        
          ELSE

            *-- Replace Old value by new value
            *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
            *C102592,1 Check for style if it exists in Temp style file. [Begin]
            *REPLACE ALL STYLE WITH SUBSTR(STYLE,1,lnMajSize+1) + ;
            *                       ALLTRIM(Key_Chg.New_Key);
            *       FOR SUBSTR(STYLE,lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key)
            lcStyleTag = ORDER('Style')
            SET ORDER TO TAG Style IN Style        
            *REPLACE ALL Style WITH SUBSTR(Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key);
                    FOR IIF(SEEK(Style,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
                    SUBSTR(Style,lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key)                   
            REPLACE ALL Style WITH SUBSTR(Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key);
                    FOR IIF(SEEK(Style,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
                    ALLTRIM(SUBSTR(Style,lnMajSize+2)) == ALLTRIM(Key_Chg.Old_Key)                   

            SET ORDER TO TAG (lcStyleTag) IN Style        
            *C102592,1 Check for style if it exists in Temp style file. [End]
            *B607346,1 ALB Bug when Change keys value that have shared char. [end]
          ENDIF
        ENDSCAN
                
      ELSE   && i.e. !EMPTY(SYDFDCHG.cAltField)
        *-- Open necessary files
        IF !lfOpClFile('O',ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag))
          LOOP
        ENDIF
        WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_Nam) + ' file.' NOWAIT
     
        *-- Replace Old value by new value
        lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)

        *C102592,1 Check for style if it exists in Temp style file. [Begin]
        *REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,lnMajSize+1)  + ;
        *                           ALLTRIM(Key_Chg.New_Key) ;
        *       FOR SUBSTR(&lcFldtmp,lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key) ;
        *       .AND. IIF(!EMPTY(SYDFDCHG.mFltExpr),ALLTRIM(SYDFDCHG.mFltExpr),.T.)
        lcStyleTag = ORDER('Style')
        SET ORDER TO TAG Style IN Style
        IF ALLTRIM(SydFdChg.cFile_Nam) = "BOM" AND lcFldTmp = "CITMMASK"
          *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
          *REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key) ;
                 FOR IIF(SEEK(SUBSTR(&lcFldtmp,1,lnMajSize+1),lcTmpStyle) AND SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1),'Style'),IIF(SUBSTR(&lcFldtmp,lnMajSize+2) = '******' AND EVALUATE(SydFdChg.cFile_Nam+'.IClr') = '******',lfAddFab(),.T.),.F.) AND ;
                 IIF(SEEK(&lcFldtmp,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
                 SUBSTR(&lcFldtmp,lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key)         
          REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key) ;
                 FOR IIF(SEEK(SUBSTR(&lcFldtmp,1,lnMajSize+1),lcTmpStyle) AND SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1),'Style'),IIF(SUBSTR(&lcFldtmp,lnMajSize+2) = '******' AND EVALUATE(SydFdChg.cFile_Nam+'.IClr') = '******',lfAddFab(),.T.),.F.) AND ;
                 IIF(SEEK(&lcFldtmp,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
                 ALLTRIM(SUBSTR(&lcFldtmp,lnMajSize+2)) == ALLTRIM(Key_Chg.Old_Key)         

          *B607346,1 ALB Bug when Change keys value that have shared char. [end]        
        ELSE
          *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
          *B607026,1 WAB (Start) -- error in the Flter Expersion "Invalid function"
          *REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key) ;
          *       FOR IIF(SEEK(&lcFldtmp,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
          *       SUBSTR(&lcFldtmp,lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key) AND ;
          *       IIF(!EMPTY(SYDFDCHG.mFltExpr),ALLTRIM(SYDFDCHG.mFltExpr),.T.)
          lcFltExp = lfGetExp()
          *REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key) ;
                 FOR IIF(SEEK(&lcFldtmp,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
                 SUBSTR(&lcFldtmp,lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key) AND &lcFltExp
          REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key) ;
                 FOR IIF(SEEK(&lcFldtmp,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'Style'),lfStyExist(&lcFldtmp),.T.),.F.) AND ;
                 ALLTRIM(SUBSTR(&lcFldtmp,lnMajSize+2)) == ALLTRIM(Key_Chg.Old_Key) AND &lcFltExp

          *B607026,1 WAB (End)           
          *B607346,1 ALB Bug when Change keys value that have shared char. [end]
        ENDIF         
        SET ORDER TO TAG (lcStyleTag) IN Style
        *C102592,1 Check for style if it exists in Temp style file. [End]
    
      ENDIF
    ENDSCAN

    *C102592,1 Finally change the Style file. [Begin]
    SELECT STYLE
    SET ORDER TO
            
    lcStyleTag = ORDER('Style')
    SET ORDER TO TAG Style IN Style
    *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
    *REPLACE ALL Style WITH SUBSTR(Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key) ;
            FOR IIF(SEEK(Style,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
            SUBSTR(Style, lnMajSize+2) = ALLTRIM(Key_Chg.Old_Key)
    REPLACE ALL Style WITH SUBSTR(Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key) ;
            FOR IIF(SEEK(Style,lcTmpStyle),IIF(SEEK(SUBSTR(&lcTmpStyle..Style,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),'StyAgain'),lfStyExist(StyAgain.Style),.T.),.F.) AND ;
            ALLTRIM(SUBSTR(Style, lnMajSize+2)) == ALLTRIM(Key_Chg.Old_Key)

    *B607346,1 ALB Bug when Change keys value that have shared char. [end]
    SET ORDER TO TAG (lcStyleTag) IN Style        
    
    SET ORDER TO CSTYLE
    SCAN FOR SUBSTR(STYLE, lnMajSize+2 ) = ALLTRIM(Key_Chg.New_Key)
      lnLen = 1
      FOR lnC = lnMajSeg + 1 TO ALEN(laMajSeg,1)
        lcCod = SUBSTR(KEY_CHG.New_Key,lnLen,LEN(laMajSeg[lnC,3]) )
        lnLen = lnLen + LEN(laMajSeg[lnC,3])
        *!   SEGMENT TYPE 'FOTQZCDGS'
        *!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
        *!   C-> Color , D->Division ,G->Group ,S->Size
        DO CASE
          CASE laMajSeg[lnC,1] = 'Z'
            REPLACE SEASON WITH lcCod
          CASE laMajSeg[lnC,1] = 'D'
            REPLACE CDIVISION WITH lcCod
          CASE laMajSeg[lnC,1] = 'G'
            REPLACE CSTYGROUP WITH lcCod
          CASE laMajSeg[lnC,1] = 'S'
            REPLACE SCALE WITH lcCod
        ENDCASE
      ENDFOR
    ENDSCAN

    IF USED('StyAgain')
      USE IN StyAgain
    ENDIF      
    *C102592,1 Finally change the Style file. [End]
        
    SELECT CODES
    SET ORDER TO (lcOrd)
    =lfOpClFile('C')

ENDCASE
IF llopSGVL
  USE IN ICSEGVAL
ENDIF
IF llopsty
  USE IN STYLE
ENDIF
IF llopscl
  USE IN SCALE
ENDIF


*!*************************************************************
*! Name      : lfBldTmp
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 10/13/1998
*! Purpose   : To build the temporary files
*!*************************************************************
*! Called from : lfCrtFiles(), lfvPrcd(), 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfBldTmp()
*!*************************************************************
FUNCTION lfBldTmp
PRIVATE lnAlias
lnAlias = SELECT(0)

IF USED(lcNValues)
  USE IN (lcNValues)
ENDIF

SELECT (lcValues)
GO TOP
IF !EOF()
  ZAP
ENDIF

SELECT (lcHistory)
GO TOP
IF !EOF()
  ZAP
ENDIF

SELECT KEY_CHG
SET RELATION TO Key_chg.ckeytype+ Key_chg.ckey_chg INTO Sydfdvld ADDITIVE
=SEEK(lcKeyType)
SCAN WHILE cKeyType = lcKeyType 
  IF ASCAN(laFields,ALLTRIM(cKey_Chg)) = 0
    LOOP
  ENDIF
  llKey=laFields[ASUBSCRIPT(laFields,ASCAN(laFields,ALLTRIM(cKey_Chg)),1),6]
  IF KeyStatus = 'O'
    INSERT INTO (lcValues) (cKeyType,cKey_Chg, Old_Key, New_Key,cFld_head, Entered,;
                                         cStatus, lKey, mSaveFilt) ;
       VALUES (KEY_CHG.cKeyType,KEY_CHG.cKey_Chg, KEY_CHG.Old_Key,      ;
           KEY_CHG.New_Key, SYDFDVLD.cFld_Head, KEY_CHG.ENTERED,'S',llKey,;
           KEY_CHG.mSaveFilt)
  ELSE
    INSERT INTO (lcHistory) (cKeyType,cKey_Chg, Old_Key, New_Key,  ;
                             cFld_head,Entered, Date_Procd, lKey)  ;
      VALUES (KEY_CHG.cKeyType,KEY_CHG.cKey_Chg, KEY_CHG.Old_Key,  ;
              KEY_CHG.New_Key,SYDFDVLD.cFld_Head, KEY_CHG.Entered, ;
               KEY_CHG.Date_Procd, llKey)
  ENDIF
ENDSCAN
FLUSH
USE (gcWorkDir+lcValues) AGAIN ALIAS (lcNValues) ORDER TAG (lcValues) IN 0 

SET RELATION TO
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfvScale
*! Developer : Sameh Saiid Ezzat 
*! Date      : 04/04/2000
*! Purpose   : Special function of the Scale key for Old , New Values 
*!             and replacing process
*!**************************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!**************************************************************************
*! Calls       : ....
*!               FUNCTIONS  : gfOpenFile(), gfStyBrw(), lfOpClFile()
*!**************************************************************************
*! Passed Parameters : lcStatus = "O"  Validation of old value object
*!                     lcStatus = "N"  Validation of New value object
*!                     lcStatus = "R"  Replacing process
*!**************************************************************************
*! Example     : = lfvScale()
*!**************************************************************************
*! Notes       : Name of this function must be in mSpcFunc field 
*!               in the SYDFDVLD file.
*!**************************************************************************
*
FUNCTION lfvScale
PARAMETERS lcStatus
PRIVATE llScaleHD , lnNumber
lnNumber = 0

IF USED('SCALEHD')
  USE IN SCALEHD
ENDIF
*--MHM open scalehd if company is ext. size scale
llScaleHD = IIF(llMScale,gfOpenFile(ALLTRIM(laComp[lnComp,3])+'SCALEHD','EXTSCALE','SH'),.F.)


DO CASE 
  *-- Validation of the Old Value
  CASE lcStatus = 'O'
    IF !SEEK('S'+lcOldVal,'SCALE')
      = lfScalBrow(@lcOldVal)
    ENDIF

  *-- Validations of the new value
  *B605057,1 (Begin) Accept the new value.
  *CASE lcStatus = 'N'
  CASE .F. AND lcStatus = 'N'
  *B605057,1 (End)
    IF !SEEK('S'+lcNewVal,'SCALE')
      llSclExist = .F.
      
      *-- 'Scale ' + ALLTRIM(lcNewVal) + 'does not exist in '      
      *--         the scale file Continue anyway?           '
      *--              < YES >       < NO >                 '
      
      IF gfModalGen("QRM00274B00006","DIALOG",'Scale ' + ;
         ALLTRIM(lcNewVal) + ' does not exist in the Scale ' + ;
         'file continue anyway ?') = 2
        lcNewVal = lcOld
        _CUROBJ = _CUROBJ
        RETURN .F.
      ENDIF
    ELSE
      llSclExist = .T.
    ENDIF

  *-- Replacing process
  CASE lcStatus = 'R'
  
    *-- For Loop Around the Major/Non Major Array [Begin]
    *-- To Get Scale in Style file (MHM)
    FOR lnC = lnMajSeg + 1 TO ALEN(laMajSeg,1)
      *-- If Type is Scale
      IF laMajSeg[lnC,1] = 'S'
        lnNumber = lnC
        EXIT
      ENDIF
    ENDFOR
    *-- End For Around the Major/Non Major Array [End]
    
    *-- First change Scale in Non Major part in all files that have Style field
    *-- If there is a Scale in Style Non Major [Begin] 
    IF lnNumber > 0
      SELECT SYDFDCHG    
      =SEEK('K'+'STYLE')
      *-- Scan around the changed style Fields in all system [Begin]
      SCAN WHILE cKeyType + cFld_Name = 'K' + 'STYLE'
        *-- If main field record , Get all files that has 
        *-- that field then replace old value by new value [Begin]
        IF EMPTY(SYDFDCHG.cAltField)
          SELECT SYDFLFLD
          =SEEK('STYLE')
          SCAN WHILE cFld_Name = 'STYLE'
            *-- Open necessary file.
            IF !lfOpClFile('O',ALLTRIM(SYDFLFLD.cFile_Nam),'')
              LOOP
            ENDIF
            WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) + ' file.' NOWAIT
            *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
            *REPLACE ALL STYLE WITH SUBSTR(STYLE,1,laMajSeg[lnNumber,4]-1) + ;
                    ALLTRIM(Key_Chg.New_Key) + SUBSTR(STYLE,laMajSeg[lnNumber,4]+lnLenght) ;
                    FOR SUBSTR(STYLE,laMajSeg[lnNumber,4],lnLenght) = ALLTRIM(Key_Chg.Old_Key)
            REPLACE ALL STYLE WITH SUBSTR(STYLE,1,laMajSeg[lnNumber,4]-1) + ;
                    ALLTRIM(Key_Chg.New_Key) + SUBSTR(STYLE,laMajSeg[lnNumber,4]+lnLenght) ;
                    FOR ALLTRIM(SUBSTR(STYLE,laMajSeg[lnNumber,4],lnLenght)) == ALLTRIM(Key_Chg.Old_Key)

            *B607346,1 ALB Bug when Change keys value that have shared char. [end]
          ENDSCAN
        ELSE   && Else not empty of Alternative Field
          *-- If has alternative field change (MHM)[Begin]
          *-- Open necessary files
          IF !lfOpClFile('O',ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag) )
            LOOP
          ENDIF
          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFDCHG.cFile_Nam) + ' file.' NOWAIT

          *-- Replace Old value by new value
          lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
          
          *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
          *B607026,1 WAB (Start) -- error in the Flter Expersion "Invalid function"
          *REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,laMajSeg[lnNumber,4]-1) + ;
          *        ALLTRIM(Key_Chg.New_Key) + SUBSTR(&lcFldtmp,laMajSeg[lnNumber,4]+lnLenght) ;
          *        FOR SUBSTR(&lcFldtmp,laMajSeg[lnNumber,4],lnLenght) = ALLTRIM(Key_Chg.Old_Key) ;
          *        AND IIF(EMPTY(SYDFDCHG.mFltExpr),.T.,ALLTRIM(SYDFDCHG.mFltExpr))
          lcFltExp = lfGetExp()                  
          *REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,laMajSeg[lnNumber,4]-1) + ;
                  ALLTRIM(Key_Chg.New_Key) + SUBSTR(&lcFldtmp,laMajSeg[lnNumber,4]+lnLenght) ;
                  FOR SUBSTR(&lcFldtmp,laMajSeg[lnNumber,4],lnLenght) = ALLTRIM(Key_Chg.Old_Key) ;
                  AND &lcFltExp
          REPLACE ALL &lcFldtmp WITH SUBSTR(&lcFldtmp,1,laMajSeg[lnNumber,4]-1) + ;
                  ALLTRIM(Key_Chg.New_Key) + SUBSTR(&lcFldtmp,laMajSeg[lnNumber,4]+lnLenght) ;
                  FOR ALLTRIM(SUBSTR(&lcFldtmp,laMajSeg[lnNumber,4],lnLenght)) == ALLTRIM(Key_Chg.Old_Key) ;
                  AND &lcFltExp

          *B607026,1 WAB (End) 
          *B607346,1 ALB Bug when Change keys value that have shared char. [end]
          *-- If has alternative field change (MHM)[End]        
        ENDIF
      ENDSCAN
      *-- EndScan Around the changed style Fields in all system [End]
    ENDIF
    *-- EndIf of Scale in Style Non Major [End]
     
    SELECT SYDFDCHG    
    =SEEK('K'+'SCALE')

    *-- Scan around the changed scale Fields in all system [Begin]
    SCAN WHILE cKeyType + cFld_Name = 'K' + 'SCALE'
      *-- If main field record , Get all files that has that field
      *-- then replace old value by new value
      IF EMPTY(SYDFDCHG.cAltField)
        SELECT SYDFLFLD
        =SEEK('SCALE')
        SCAN WHILE cFld_Name = 'SCALE'
          *-- Open necessary file.
          IF  !lfOpClFile('O',ALLTRIM(SYDFLFLD.cFile_Nam),'')
            LOOP
          ENDIF
            
          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) +' file.' NOWAIT
            
          *-- Replace Old value by new value
          lcFldtmp = ALLTRIM(SYDFDCHG.cFld_Name)
          IF SYDFLFLD.cFile_Nam = 'SCALE' AND llSclExist
            LOOP
          ENDIF
          *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
          *REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                  FOR &lcFldtmp = ALLTRIM(Key_Chg.Old_Key)
          REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                  FOR ALLTRIM(&lcFldtmp) = ALLTRIM(Key_Chg.Old_Key)

          *B607346,1 ALB Bug when Change keys value that have shared char. [end]
        ENDSCAN
      ELSE   && Else not empty of Alternative Field
        *-- If has alternative field change (MHM)[Begin]
        *-- Open necessary files
        IF !lfOpClFile('O',ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag) )
          LOOP
        ENDIF

        WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) + ' file.' NOWAIT

        *-- Replace Old value by new value
        lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
        *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
        *B607026,1 WAB (Start) -- error in the Flter Expersion "Invalid function"
        *REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
        *        FOR &lcFldtmp = ALLTRIM(Key_Chg.Old_Key) ;
        *        AND IIF(EMPTY(SYDFDCHG.mFltExpr),.T.,ALLTRIM(SYDFDCHG.mFltExpr))
        lcFltExp = lfGetExp()                  
        *REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                FOR &lcFldtmp = ALLTRIM(Key_Chg.Old_Key) ;
                AND &lcFltExp
        REPLACE ALL &lcFldtmp WITH Key_Chg.New_Key ;
                FOR ALLTrIM(&lcFldtmp) = ALLTRIM(Key_Chg.Old_Key) ;
                AND &lcFltExp

        *B607346,1 ALB Bug when Change keys value that have shared char. [end]
        *B607026,1 WAB (End) 
        
        *-- If has alternative field change (MHM)[End]        
      ENDIF
    ENDSCAN
    *-- Scan around the changed scale Fields in all system [End]

	IF llScaleHD
	  SELECT ScaleHD
      *- MHM REMOVE lnScalelen to Get Scale Header Lenght [begin]
	  *IF SEEK(ALLTRIM(SUBSTR(Key_Chg.Old_Key,1,lnScaleLen-1)))
	  IF SEEK(ALLTRIM(SUBSTR(Key_Chg.Old_Key,1,gfGetMemVar('M_EXTWIDTH',laComp[lnComp,2]))))	  
      *- MHM REMOVE lnScalelen to Get Scale Header Lenght [end]
        REPLACE cExtScale WITH ALLTRIM(SUBSTR(Key_Chg.Old_Key,1,gfGetMemVar('M_EXTWIDTH',laComp[lnComp,2])))
      ENDIF
	ENDIF
ENDCASE

IF llScaleHD
  USE IN SCALEHD
ENDIF
*-- End of lfvScale.

*!**************************************************************************
*! Name      : lfScalBrow
*! Developer : Sameh Saiid Ezzat 
*! Date      : 04/05/2000
*! Purpose   : Function to validate entered scale size value.
*!**************************************************************************
*! Calls     : ARIABROW()
*!**************************************************************************
*! Passed Parameters  :  Entered Scale
*!                       Multi scale 
*!**************************************************************************
*! Returns            :  .T. --> Valid scale 
*!                       .F. --> Invalid scale
*!**************************************************************************
*! Example            :  lfScalBrow(@m.Scale)
*!**************************************************************************
*C101837,1 SSE 04/05/2000
*
FUNCTION lfScalBrow
PARAMETER lcParaScale
PRIVATE lcBrFields

lnOldAlias = SELECT()
lcFile_Ttl = 'Scales'

SELECT SCALE
IF llBrowse OR EMPTY(lcParaScale) OR !SEEK('S'+lcParaScale)
  DECLARE laValues[1]  && array to get values from browse
  lcBrFields = "Scale:4:H='Scale' ,"+;
              "cScl_DESC :14:H='Description',"+;
              "CNT  :H='Cnt'  ,"+;
              "SZ1  :8:H='Size 1',"+;
              "SZ2  :8:H='Size 2',"+;
              "SZ3  :8:H='Size 3',"+;
              "SZ4  :8:H='Size 4',"+;
              "SZ5  :8:H='Size 5',"+;
              "SZ6  :8:H='Size 6',"+;
              "SZ7  :8:H='Size 7',"+;
              "SZ8  :8:H='Size 8'"
   
  =ARIABROW(['S'],lcFile_Ttl,gnBrHSRow1, gnBrHSCol1, gnBrHSRow2, gnBrHSCol2,'','','SCALE','laValues')
  lcParaScale = IIF(EMPTY(laValues[1]),SPACE(1),laValues[1])
ENDIF  

SELECT (lnOldAlias)
RETURN !EMPTY(lcParaScale)
*-- End of lfScalBrow.




*!**************************************************************************
*! Name      : lfGetVal
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/25/01
*! Purpose   : Check new values.
*!**************************************************************************
*B605057,1
FUNCTION lfGetVal
PARAMETERS lcVal

*IF ALLTRIM(lcVal) = "?"
IF ('?' $ lcVal)
  =gfModalGen("TRM00385B00000","DIALOG")
  llRet = .F.
  RETURN lcVal
ENDIF
*-- Go to the field record in the SYDFDVLD table to get the browse information
IF SEEK(lcKeyType+laFields[lnFields,2],'SYDFDVLD')
  DO CASE
    CASE "Customer" $ lafields[lnfields,1]
       *--Check if the account already exists.
       IF SEEK(ALLTRIM(SYDFDVLD.mKeyExpr)+lcVal,ALLTRIM(SYDFDvld.cMainFile)) 
          =gfModalGen("TRM00383B00000","DIALOG","Account "+ALLTRIM(lcVal))
          llRet = .F.
       ELSE
         IF LEN(ALLTRIM(lcVal)) < 5 .AND. !('?' $ lcVal)
           *-- Account code should not be less than 5 characters.
           = gfModalGen('TRM00384B00000','ALERT') = 2
           llRet = .F.
         ENDIF
       ENDIF
    
    *B125283,1 NNA  12/17/2004 (Begin) Add an Additional case for Ship Via to locate with it in the
    *B125283,1 NNA              customer file to see if it found or not.
    CASE   "SHIPVIA" $ laFields[lnFields,2]
      lnOldAls =SELECT(0)
      lcFile = SYDFDVLD.CFILE_NAM      
      SELECT &lcFile
      LOCATE FOR &lcFile..SHIPVIA = lcVal
      IF FOUND()
        =gfModalGen("TRM00383B00000","DIALOG",PROPER(ALLTRIM(laFields[lnFields,1]))+" "+ALLTRIM(lcVal))
        llRet = .F.
      ENDIF    
      SELECT(lnOldAls)
    *B125283,1 NNA (End)

    OTHERWISE
    
      *--Check if the account already exists.
      IF SEEK(ALLTRIM(SYDFDVLD.mKeyExpr)+lcVal,ALLTRIM(SYDFDvld.cMainFile)) 
         *B606594,1 WAB (Start)--Worng message
         *=gfModalGen("TRM00382B00000","DIALOG",PROPER(ALLTRIM(laFields[lnFields,1]))+" "+ALLTRIM(lcVal))
         =gfModalGen("TRM00383B00000","DIALOG",PROPER(ALLTRIM(laFields[lnFields,1]))+" "+ALLTRIM(lcVal))
         *B606594,1 WAB (End)
         llRet = .F.
      ENDIF
  ENDCASE  
ENDIF
RETURN lcVal


*!**************************************************************************
*! Name      : lfvKeyVal
*! Developer : Rania Abdel Razik ElSayed
*! Date      : 12/24/2001
*! Purpose   : Valid function of primary key object
*!**************************************************************************
*! Called from       : SMKEYNAM.SPR
*!**************************************************************************
*! Calls             : None
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return		     : None
*!**************************************************************************
*! Example           : = lfvKeyVal()
*!**************************************************************************
*!C102427,1 RAE 

FUNCTION lfvKeyVal

IF !EMPTY(laFields[lnFields,3]) AND  !EMPTY(laFields[lnFields,7])
  * Calling the sepcial function with a specific k according to the numbers of keys in SYDFDVLD.mFull_Exp
  lcSpecFn = STRTRAN(laFields[lnFields,3] , '()','("K')+ALLTRIM(STR(lnCount))+'")'
  llRet    = &lcSpecFn
ENDIF

*!**************************************************************************
*! Name      : lfvCustm
*! Developer : Rania Abdel Razik ElSayed
*! Date      : 12/24/2001
*! Purpose   : Special function of the Customer Store key for Primary Key,Old, 
*!             New Values and replacing process
*!**************************************************************************
*! Called from : lfvNewVal(), lfvOldVal(), lfvPrcd()
*!**************************************************************************
*! Calls       : ....
*!               FUNCTION  : lfOpClFile()
*!**************************************************************************
*! Passed Parameters : lcStatus = "K1"  Validation of Primary key object
*!                     lcStatus = "O"   Validation of old value object
*!                     lcStatus = "N"   Validation of New value object
*!                     lcStatus = "R"   Replacing process
*!**************************************************************************
*! Example     : = lfvCustm()
*!**************************************************************************
*! Notes       : Name of this function must be in mSpcFunc field 
*!               in the SYDFDVLD file.
*!               IF we have more than one key in the FullExp field we have 
*!               the validation for each field as the follwings:	
*!                      File # 1  valdition will be  CASE lcStatus = 'K1'
*!                      File # 2  valdition will be  CASE lcStatus = 'K2'
*!**************************************************************************
*!C102427,1 RAE 

FUNCTION lfvCustm 

PARAMETERS lcStatus 

Do Case 
  CASE lcStatus = 'K1'
  *--Validation of Primary key object
    lcBrFields = "ACCOUNT    :R :H= 'Account Code'       ," +;
                 "STNAME     :R :H= 'STNAME '            ," +;
                 "PHONE1     :R :H= 'Phone  '            ," +;
                 "BUYER      :R :H= 'BUYER  '            ," +;
                 "SALESREP   :R :H= 'Sales Representive' ," +;
                 "CADDRESS1  :R :H= 'CADDRESS1'          ," +;
                 "CADDRESS2  :R :H= 'CADDRESS2'          ," +;
                 "CADDRESS3  :R :H= 'CADDRESS3'          ," +;
                 "CADDRESS4  :R :H= 'CADDRESS4'          ," +;
                 "CADDRESS5  :R :H= 'CADDRESS5'          ," +;
                 "CADDRESS12 :R :H= 'CADDRESS12'         ," +;
                 "CADDRESS22 :R :H= 'CADDRESS22'         ," +;
                 "CADDRESS32 :R :H= 'CADDRESS32'         ," +;
                 "CADDRESS42 :R :H= 'CADDRESS42'         ," +;
                 "CADDRESS52 :R :H= 'CADDRESS52'         ," +;
                 "NETBAL     :R :H= 'NETBAL'             " 

  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Select Value"
  *-- Get the browse title
  IF SEEK(SYDFDvld.cMainFile,'SYDFILES')
    lcFile_ttl = SYDFILES.cFile_ttl
  ENDIF
  
  SELECT CUSTOMER
  lcFilt = "FOR TYPE = 'M' .AND. STATUS # 'X' "
  IF llBrowse OR '?' $ lcKeyVal .OR. !SEEK(lcKeyVal,'CUSTOMER')
    xAccount = lcKeyVal
    DO CUSBROWM WITH xAccount
    lcKeyVal = xAccount
    *=gfBrows(lcFilt,'ACCOUNT',"laTemp")
    *lcKeyVal = laTemp[1]
    lcKeyTemp=lcKeyVal 
    
    *-- lcKeyVal must contain a value 
    *=gfModalGen("TRM00386B00000","DIALOG",laFullExp[lnKeys,1])
    *_CUROBJ = _CUROBJ
  ENDIF 
  
  IF EMPTY(lcKeyTemp)
    lcKeyTemp= lcOld
    KEYBOARD '{ALT+B}'
    llRet = .F.
  ENDIF
  llBrowse = .F.
  
  CASE lcStatus = 'O'
  *--Validation of old value object
    
    lcBrFields = "ACCOUNT    :R :H= 'Account Code'       ," +;
                 "STORE		 :R :H= 'Store Code'         ," +;
                 "STNAME     :R :H= 'STNAME '            ," +;
                 "PHONE1     :R :H= 'Phone  '            ," +;
                 "BUYER      :R :H= 'BUYER  '            ," +;
                 "SALESREP   :R :H= 'Sales Representive' ," +;
                 "CADDRESS1  :R :H= 'CADDRESS1'          ," +;
                 "CADDRESS2  :R :H= 'CADDRESS2'          ," +;
                 "CADDRESS3  :R :H= 'CADDRESS3'          ," +;
                 "CADDRESS4  :R :H= 'CADDRESS4'          ," +;
                 "CADDRESS5  :R :H= 'CADDRESS5'          ," +;
                 "CADDRESS12 :R :H= 'CADDRESS12'         ," +;
                 "CADDRESS22 :R :H= 'CADDRESS22'         ," +;
                 "CADDRESS32 :R :H= 'CADDRESS32'         ," +;
                 "CADDRESS42 :R :H= 'CADDRESS42'         ," +;
                 "CADDRESS52 :R :H= 'CADDRESS52'         ," +;
                 "NETBAL     :R :H= 'NETBAL'             " 

  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Select Value"
  *-- Get the browse title
  IF SEEK(SYDFDvld.cMainFile,'SYDFILES')
    lcFile_ttl = SYDFILES.cFile_ttl
  ENDIF
  SELECT CUSTOMER
  
  lcFilt = "FOR TYPE = 'S' .AND. STATUS # 'X'.AND. ACCOUNT=lcKeyTemp "
  xAccount = lcKeyTemp
  xStore   = lcOldVal
  IF !CUSBROWS(lcKeyTemp,.T.)
    STORE SPACE(8) TO xStore
  ENDIF
  lcOldVal = xStore
  IF lcOldVal = lcOld 
    llRet = .T.
    RETURN
  ENDIF
  *=gfBrows(lcFilt,'STORE',"laTemp")
  *lcOldVal = laTemp[1]
  lnRecNo = RECNO(lcValues)
  IF SEEK(lcKeyType+PADR(laFields[lnFields,2],10)+lcKeyTemp+'-'+lcOldVal,lcValues) AND (ALLTRIM(lcOldVal)<>ALLTRIM(lcOld) OR EMPTY(lcOld))
    *-- "This value has been selected before as an "old value". Cannot accept."
    =gfModalGen("INM00335B00000","DIALOG","old value")
    lcOldVal = ''
    _CUROBJ = _CUROBJ
  ENDIF
  IF BETWEEN(lnRecNo,1,RECCOUNT(lcValues))
    GO lnRecNO IN (lcValues)
  ENDIF
  *=SEEK(lcKeyType+PADR(laFields[lnFields,2],10)+lcKeyTemp+'-'+lcOld,lcValues) 
  llRet = .T.
  IF EMPTY(lcOldVal)
    lcOldVal= lcOld
    KEYBOARD '{ALT+B}'
    llRet = .F.
  ELSE
 
    IF !llNEW 
      lnAlias = SELECT()
      SELECT (lcValues)
      REPLACE Old_Key    WITH lcKeyTemp+'-'+lcOldVal
      SELECT (lnAlias)
      llRet = .T.
    ENDIF
  ENDIF
  
  CASE lcStatus = 'N'
  
    *-- Validation of New value object
    SELECT (lcValues)
    IF lcNewVal = lcOld
      RETURN
    ENDIF
    IF ('?' $ lcNewVal)
      =gfModalGen("TRM00385B00000","DIALOG")
      llRet = .F.
      _CUROBJ = _CUROBJ
      RETURN
    ELSE
      lnRecNO = RECNO(lcValues)
      LOCATE FOR cKeyType+cKey_Chg+Old_Key+New_Key = ;
                 lcKeyType+PADR(laFields[lnFields,2],10) AND NEW_KEY = lcKeyTemp+'-'+lcNewVal
      IF FOUND()
        *-- "This value has been selected before as a "new value". Cannot accept."
        =gfModalGen("INM00335B00000","DIALOG","new value")
        lcNewVal = lcOld
        llRet = .F.
        _CUROBJ = _CUROBJ
        RETURN .F.  
      ENDIF
      IF BETWEEN(lnRecNO,1,RECCOUNT(lcValues))
         GO lnRecNO IN (lcValues)
      ENDIF
      IF !EMPTY(lcOldVal).AND.!EMPTY(lcNewVal)
        IF llNew
          APPEND BLANK
          REPLACE cStatus   WITH 'A',;
                  cFld_Head WITH  laFields[lnFields,1]
          llNew = .F.
        ELSE
          REPLACE cStatus WITH IIF(cStatus='S','M',cStatus)
        ENDIF
        REPLACE cKeyType   WITH lcKeyType,;
                Old_Key    WITH lcKeyTemp+'-'+lcOldVal,;
                New_Key    WITH lcKeyTemp+'-'+lcNewVal,;
                cKey_Chg   WITH laFields[lnFields,2],;
                ENTERED    WITH DATE()
        SAVE TO MEMO mSaveFilt ALL LIKE laFixFltr
        *-- Variable to indicate that data has modified
        llcUpDate = .T.
      ENDIF
    ENDIF  
    
  CASE lcStatus = 'R'  
  *-- Replacing process
    *-- lcPKey variable holds the Primary Key ( The same foe new and old value )
    lcPKey=ALLTRIM(SUBSTR(Key_Chg.Old_Key,1,AT('-',Key_Chg.Old_Key)-1))
    
    *-- lcOldKey variable holds the old value of secondary key
    lcOldKey=ALLTRIM(SUBSTR(Key_Chg.Old_Key,LEN(lcPKey)+2))
    
    *-- lcNewKey variable holds the new value of secondary key    
    lcNewKey=ALLTRIM(SUBSTR(Key_Chg.New_Key,LEN(lcPKey)+2))

    IF SEEK(cKeyType+cKey_Chg,'SYDFDCHG')
      SELECT SYDFDCHG
      IF KEY_CHG.cKeyType = 'K'            
        SCAN WHILE cKeyType+cFld_Name = Key_Chg.cKeyType + Key_Chg.cKey_Chg
        *-- If main field record , Get all files that has that field then replace old value by new value
        IF EMPTY(SYDFDCHG.cAltField)
          SELECT SYDFLFLD       
          =SEEK(SYDFDCHG.cFld_Name)
          SCAN WHILE cFld_Name = SYDFDCHG.cFld_Name
            *-- Open necessary file.
            IF ALLTRIM(SYDFLFLD.cFile_Nam) = 'ASN_SHIP'      
              LOOP
            ENDIF
            lcTagFile = IIF(ALLTRIM(SYDFLFLD.cFile_Nam) = 'INVHDR','INVHDR','')      
            IF  !lfOpClFile('O',ALLTRIM(SYDFLFLD.cFile_Nam),lcTagFile)
              LOOP
            ENDIF
            
            WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) +' file.' NOWAIT
            *-- Replace Old value by new value
            lcFldtmp=ALLTRIM(SYDFDCHG.cFld_Name)
            IF SYDFLFLD.cFile_Nam = 'PROFILE' OR SYDFLFLD.cFile_Nam = 'CONTACT'
              *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
              *REPLACE ALL &lcFldtmp WITH lcNewKey ;
                       FOR &lcFldtmp = ALLTRIM(lcOldKey) .AND. cCont_ID=lcPKey 
              REPLACE ALL &lcFldtmp WITH lcNewKey ;
                       FOR ALLTRIM(&lcFldtmp) == ALLTRIM(lcOldKey) .AND. cCont_ID=lcPKey 

              *B607346,1 ALB Bug when Change keys value that have shared char. [end]
            ELSE
              *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
              *REPLACE ALL &lcFldtmp WITH lcNewKey ;
                       FOR &lcFldtmp = ALLTRIM(lcOldKey) .AND. ACCOUNT=lcPKey 
              REPLACE ALL &lcFldtmp WITH lcNewKey ;
                       FOR ALLTRIM(&lcFldtmp) = ALLTRIM(lcOldKey) .AND. ACCOUNT=lcPKey 
              *B607346,1 ALB Bug when Change keys value that have shared char. [end]
            ENDIF           
          ENDSCAN
        
        ELSE   && i.e. !EMPTY(SYDFDVLD.cAltField)
          *-- Open necessary files
          IF !lfOpClFile('O',ALLTRIM(SYDFDCHG.cFile_Nam),ALLTRIM(SYDFDCHG.cBaseTag) )
            LOOP
          ENDIF
          WAIT WINDOW 'Updating ' + ALLTRIM(SYDFLFLD.cFile_Nam) + ' file.' NOWAIT

          *-- Replace Old value by new value
          lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
          IF SYDFDCHG.cFile_Nam = 'INVCHRG'
            lnAlias = ALIAS()
            IF !lfOpClFile('O','INVHDR','INVHDR')
              LOOP
            ENDIF
            SELECT (lnAlias)
            SET RELATION TO
            SET RELATION TO Invchrg.invoice INTO Invhdr ADDITIVE
            
            *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
            *B607026,1 WAB (Start) -- error in the Flter Expersion "Invalid function"
            *REPLACE ALL &lcFldtmp WITH lcNewKey ;
            *        FOR &lcFldtmp = ALLTRIM(lcOldKey) .AND. INVHDR.ACCOUNT=lcPKey ;
            *       .AND. IIF(!EMPTY(SYDFDCHG.mFltExpr),ALLTRIM(SYDFDCHG.mFltExpr),.T.)
            *ELSE
            *REPLACE ALL &lcFldtmp WITH lcNewKey ;
            *         FOR &lcFldtmp = ALLTRIM(lcOldKey).AND.ACCOUNT=lcPKey ;
            *         .AND. IIF(!EMPTY(SYDFDCHG.mFltExpr),ALLTRIM(SYDFDCHG.mFltExpr),.T.)
            lcFltExp = lfGetExp()                  
            *REPLACE ALL &lcFldtmp WITH lcNewKey ;
                    FOR &lcFldtmp = ALLTRIM(lcOldKey) .AND. INVHDR.ACCOUNT=lcPKey ;
                   .AND. &lcFltExp
            REPLACE ALL &lcFldtmp WITH lcNewKey ;
                    FOR ALLTRIM(&lcFldtmp) = ALLTRIM(lcOldKey) .AND. INVHDR.ACCOUNT=lcPKey ;
                   .AND. &lcFltExp

            *B607346,1 ALB Bug when Change keys value that have shared char. [end]
          ELSE            
            lcFltExp = lfGetExp()                  
            *B607346,1 ALB Bug when Change keys value that have shared char. [Begin]
            *REPLACE ALL &lcFldtmp WITH lcNewKey ;
                     FOR &lcFldtmp = ALLTRIM(lcOldKey).AND.ACCOUNT=lcPKey ;
                     .AND. &lcFltExp
            REPLACE ALL &lcFldtmp WITH lcNewKey ;
                     FOR ALLTRIM(&lcFldtmp) = ALLTRIM(lcOldKey).AND.ACCOUNT=lcPKey ;
                     .AND. &lcFltExp

          *B607026,1 WAB (End) 
          *B607346,1 ALB Bug when Change keys value that have shared char. [end]
          ENDIF
       ENDIF
        ENDSCAN
      ENDIF
    ENDIF  
ENDCASE

*!************************************************************************
*! Name      : lfGetSegmt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : To get the styles segments.
*!************************************************************************
*! Called from : Custom Option Grid.
*!************************************************************************
*! Example     : = lfGetSegmt()
*!************************************************************************
*C102592,1
FUNCTION lfGetSegmt

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSegmt  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegmt + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc 
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      llColorExt = .T.
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C' 
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))  
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*-- End of lfGetSegmt.

*!************************************************************************
*! Name      : lfsrSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Set and Rest functions for style filter.
*!************************************************************************
*! Example   : =lfsrSty()
*!************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!************************************************************************
*C102592,1
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- End of lfsrvSty.

*!************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : sum a specific field for the current style in style file
*!************************************************************************
*! Returns   : Calculated field value.
*!************************************************************************
*! Example   : =lfStySum()
*!************************************************************************
*C102592,1
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.

*!************************************************************************
*! Name      : lfvWareHo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Validate warehouse
*!************************************************************************
*! Called from : Option Grid
*!************************************************************************
*! Example     : = lfvWareHo()
*!************************************************************************
*C102592,1
FUNCTION lfvWareHo
PRIVATE lcWareHo , lcTag
lcWareHo = VARREAD()
lcTag = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF
SET ORDER TO WAREHOUS IN WAREHOUS
*-- End of lfvWareHo.

*!************************************************************************
*! Name      : lfvFabric
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Validate fabric
*!************************************************************************
*! Called from : Option Grid
*!************************************************************************
*! Example     : = lfvFabric()
*!************************************************************************
*C102592,1
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF

SET ORDER TO TAG (lcTag) IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF
*-- End of lfvFabric.

*!************************************************************************
*! Name      : lpStyle
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Brow the style inrange and modify the filter.
*!************************************************************************
*! Example     : DO lpStyle
*!************************************************************************
*C102592,1
PROCEDURE lpStyle
PRIVATE lcAlias , lcExpSty
lcAlias = ALIAS()
lcTmpStyle = gfTempName()
STORE '' TO lcExpSty

CREATE CURSOR (lcTmpStyle) (Style C(19))
ZAP
INDEX ON Style TAG (lcTmpStyle) OF (gcWorkDir+lcTmpStyle+'.CDX')
lcBrowFlds = [STYLE     :H = lcStyTtl      :25 ,]    + ;
             [DESC      :H = 'Description' :25 ,]    + ;
             [SEASON    :H = 'Season'      :15 ,]    + ;
             [CDIVISION :H = 'Division'    :15 ,]    + ;
             [PRICEA    :H = 'Price'       :10 ]

lcExpSty = 'FOR ' + IIF(EMPTY(lcStyleExp),".T.",lcStyleExp)
SELECT Style
SCAN FOR &lcStyleExp
  WAIT WINDOW 'Selecting ' + lcStyTtl + " : " + Style NOWAIT
  m.Style = Style
  INSERT INTO (lcTmpStyle) FROM MEMVAR
ENDSCAN
SELECT (lcTmpStyle)
LOCATE
SELECT Style
llLastExp = gfrange(lcBrowFlds,lcTmpStyle,"Style",(lcExpSty),"","","@! XXXXXXXXXXXXXXXXXXX")

IF !llLastExp
  SELECT (lcTmpStyle)
  ZAP
ENDIF
  
SELECT (lcAlias)
*-- End of lpStyle.

*!************************************************************************
*! Name      : lfAddFab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Add Fabric in Fabric file.
*!************************************************************************
*! Example   : lfAddFab()
*!************************************************************************
*C102592,1
FUNCTION lfAddFab

*-- If entering style cost sheet & the style structure has no 
*-- non major, No need for cross refrencing the colors.
IF !llNonMjExt .OR. !llColorExt
  RETURN .T.
ENDIF

PRIVATE lcAlias , laItemRec , llUseFabrc , lcFabrcTag , lcOnlyColr
lcAlias = ALIAS()
lcOnlyColr = SUBSTR(SUBSTR(&lcFldtmp,1,lnMajSize+1)  + ALLTRIM(Key_Chg.New_Key),lnClrPo,lnColorLen)

llUseFabrc = .F.
IF !USED('Fabric')
  llUseFabrc = .T.
  USE (gcDataDir+'Fabric') IN 0 SHARE
ENDIF
  
lcFabrcTag = ORDER('Fabric')
SET ORDER TO Fabric IN Fabric

*-- If "same as color" , cross refrencing the colors or the majors.
IF Bom.cCatgTyp = 'F' OR (Bom.cCatgTyp = 'T' AND SEEK(ALLTRIM(Bom.Item),'Fabric'))
  WAIT IIF(Bom.cCatgTyp='F','Cross referencing Fabric colors','Cross referencing Trim colors') WINDOW NOWAIT
  SELECT Fabric
  =SEEK(PADR(ALLTRIM(Bom.Item),7))
  SCATTER TO laItemRec
  
  *-- If the cost item was fabric or trim.
  IF !SEEK(SUBSTR(ALLTRIM(Bom.Item),1,7)+lcOnlyColr , 'Fabric')
    =gfModalGen("INM00393B00000" , "DIALOG" , gfItemMask("HN")+"|"+lcOnlyColr+"|"+IIF(Bom.cCatgTyp= 'F','Fabric','Trim')+"|"+SUBSTR(ALLTRIM(Bom.Item),1,7))
        
    *-- If the fabric + color was not found, add it in the temp. fabric file.
    SELECT Fabric
    APPEND BLANK
    GATHER FROM laItemRec
    REPLACE Color   WITH lcOnlyColr ;
            OnHand  WITH 0 ;
            OnOrder WITH 0 ;
            Usage   WITH 0 ;
            nStkVal WITH 0 ;
            nMatWip WITH 0
    *-- Call global function to add audit fields info.
    =gfAdd_Info('Fabric')
  ENDIF

ENDIF
WAIT CLEAR

SET ORDER TO TAG (lcFabrcTag) IN Fabric
IF llUseFabrc
  USE IN Fabric
ENDIF

SELECT (lcAlias)
*-- End of lfAddFab.

*!************************************************************************
*! Name      : lfStyExist
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Display a message of existing style.
*!************************************************************************
*! Example   : lfStyExist()
*!************************************************************************
*C102592,1
FUNCTION lfStyExist
PARAMETER lcCurStyle

IF !SEEK(SUBSTR(lcCurStyle,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key),lcTmpExist)
  *-- Message < Style : ~ exist in style file. Cannot proceed >
  *-- Buttons <                       OK                      >
  =gfModalGen("INM00372B00000","DIALOG",SUBSTR(lcCurStyle,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key))
  m.Style = SUBSTR(lcCurStyle,1,lnMajSize+1) + ALLTRIM(Key_Chg.New_Key)
  INSERT INTO (lcTmpExist) FROM MEMVAR  
ENDIF
RETURN .F.
*-- End of lfStyExist,

*!************************************************************************
*! Name      : lpSelecSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/17/2002
*! Purpose   : Display an OG to select styles to apply changes in its Non Major.
*!************************************************************************
*! Example   : DO lpSelecSty
*!************************************************************************
*C102592,1
PROCEDURE lpSelecSty
PRIVATE lcStyleExp , lcColorTt , lcNonMajT , lcFreeClr , lcNonMajPi , ;
        lnNonMajPo , lnFreeLen , llLastExp , lcStyTtl , lcRpDomImp
STORE '' TO lcColorTt , lcNonMajT , lcFreeClr , lcNonMajPi , ;
            lcStyTtl , lcRpDomImp
STORE 0 TO lnNonMajPo , lnFreeLen
=lfGetSegmt()
PUSH KEY CLEAR
IF !EMPTY(lcTmpStyle) AND USED(lcTmpStyle) AND RECCOUNT(lcTmpStyle) > 0
  SELECT (lcTmpStyle)
  ZAP
ENDIF
lcStyleExp = gfOpGrid('SMFLDVL', .T.)
IF TYPE('lcStyleExp') = "L"  
  cbValues = .F.
  SHOW GET cbValues
  *KEYBOARD '{ALT+B}'
  RETURN
ELSE
  DO lpStyle
ENDIF
POP KEY
*-- End of lpSelecSty

*!************************************************************************
*! Name      : lfGetExp
*! Developer : Walid A. Wahab (WAB)
*! Date      : 03/11/2002
*! Purpose   : create a filter expersion from SYDFDCHG.mFltExpr
*!************************************************************************
*! Example   : DO lpSelecSty
*!************************************************************************
*B607026,1 WAB 
*!************************************************************************
FUNCTION lfGetExp
Private lnLineNo
lnLineNo  = MEMLINES(SYDFDCHG.mFltExpr)
lcFltExp  = ''
FOR lnCount = 1 TO lnLineNo
  lcFltExp = lcFltExp + MLINE(sydfdchg.mfltexpr,lnCount)
ENDFOR
RETURN IIF(!EMPTY(lcFltExp),ALLTRIM(lcFltExp),".T.")

*--