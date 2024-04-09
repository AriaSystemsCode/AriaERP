*:**********************************************************************************************
*: Program file        : gfExpSe.PRG
*: Program description : Export & Receive System Settings
*: For screen          : SYEXPSE.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : MAIN SYSTEM
*: Developer Name      : Hossam El Etreby {HDM}
*: Tracking Job Number : E301318
*:**********************************************************************************************
*: Calls               : SYEXPSE.SPR
*:**********************************************************************************************
*: Passed Parameters   : lnTrancod
*:                     :      1 --------------------> Generate settings file
*:                     :      2 --------------------> Receive Settings File
*:                     : lcAccount
*:                     :    Customer account
*:                     :    Passed only if we are going to receive the settings file
*:                     :    to tell which customer we are going to restore his settings
*:**********************************************************************************************
*: Example             : DO gfExpSe
*:**********************************************************************************************
*: Documented          : 09/22/1999
*:**********************************************************************************************
*: Modifications       :
*:**********************************************************************************************
PARAMETERS lnTranCod , lcAccount
STORE '' TO lcFileDir,lcFileName
STORE 1 TO lnObject
STORE .F. TO llNoShow , llOverWrt , llFileFld , llField


IF PARAMETER() = 0
  lnTranCod = 1
ENDIF

lcSetsWnd = gfTempName()

IF lnTranCod = 1
  lcSeWinTTL = 'Export System Settings'
  *-- This Means we are going to Generate the system settings
  *--Define Array to hold Hardware Settings
  DIMENSION laHWSetts[12,2]
  laHWSetts[1,1] = 'Processor'
  laHWSetts[1,2] = 'SYS(17)'

  laHWSetts[2,1] = 'Video Adapter'
  laHWSetts[2,2] = 'sys(2006)'

  laHWSetts[3,1] = 'Memory Limit'
  laHWSetts[3,2] = 'SYS(24)'

  laHWSetts[4,1] = 'FoxPro Memory'
  laHWSetts[4,2] = 'SYS(1001)'

  laHWSetts[5,1] = 'FoxPro EMS Memory'
  laHWSetts[5,2] = 'SYS(23)'

  laHWSetts[6,1] = 'Memory in Use'
  laHWSetts[6,2] = 'SYS(12)'

  laHWSetts[7,1] = 'Operating System'
  laHWSetts[7,2] = 'OS()'

  laHWSetts[8,1] = 'Available Disk Space'
  laHWSetts[8,2] = 'DISKSPACE()'

  laHWSetts[9,1] = 'Temp File Directory'
  laHWSetts[9,2] = 'SYS(2023)'

  laHWSetts[10,1]='Station Physical ID'
  laHWSetts[10,2]='GETENV("P_STATION")'

  laHWSetts[11,1]='Printer Status'
  laHWSetts[11,2]='SYS(13)'

  laHWSetts[12,1]='Files in config.sys'
  laHWSetts[12,2]='SYS(2010)'

  *-- Declare an array to hold the modules we're going to
  *-- get its setups

  DIMENSION laModules[1]
  STORE '' TO laModules
ELSE
  lcSeWinTTL = 'Import System Settings'
ENDIF


lcFileDir = SET('DEFAULT')+CURDIR()
*DO (gcScrDir + 'SY\SYEXPSE.SPX')
DO SYEXPSE.SPR

*!*************************************************************
*! Name       : lfvOk
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Valid function for Proceed Button
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOk()
*!*************************************************************
FUNCTION lfvOk
IF lnTranCod = 1
  =lfProceed()
ELSE
  =lfReceive()
ENDIF

*!*************************************************************
*! Name       : lfvFileNam
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Valid function for File Name text box
*!*************************************************************
*! Calls      : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFileNam()
*!*************************************************************

FUNCTION lfvFileNam
IF EMPTY(lcFileName)
  llOverWrt = .F.
  RETURN
ENDIF

IF lnTranCod = 1
  llOverWrt = .T.
  IF lnTranCod = 1
  *-- Check file doesn't exist
  IF FILE(lcFileDir+lcFileName+'.TXT')
    *-- Display a message informing the user that this file already exists
    *-- and if he wants to overwrite it or not
    *-- M00356      File xxxxxxxx.txt already exists
    *-- M00356      Overwrite?
    *-- B00006      < Yes >        < No >
    lnResponse = gfModalGen("QRM00356B00006","DIALOG",lcFileDir+lcFileName+'.TXT')
    *-- if the user selected Yes we will rais a flag tells the program to
    *-- remove the old file and create another one
    *-- otherwise we will stop the operation and let the user select another file name
    IF lnResponse = 1
      llOverWrt = .T.
    ELSE
      llOverWrt = .F.
    ENDIF
  ENDIF
  IF llOverWrt
    SHOW GET pbProc ENABLED
  ELSE
    SHOW GET pbProc DISABLE
  ENDIF
ELSE
  IF !FILE(lcFileDir+lcFileName+'.TXT')
    WAIT WINDOW 'File ' + lcFileDir + lcFileName + ".TXT Doesn't Exist"
    lcFileName = ''
    _CUROBJ = OBJNUM(lcFileName)
  ENDIF
ENDIF
ELSE
  IF !FILE(ALLTRIM(lcFileName))
    WAIT WINDOW "File " + ALLTRIM(lcFileName) +" doesn't exist"
    lcFileName = ''
    SHOW GET lcFileName
    RETURN
  ELSE
    llOverWrt = .T.
    SHOW GET lcFileName
    SHOW GET pbProc ENABLED
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name       : lfvFileDir
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Valid function for File Directory text box
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFileDir()
*!*************************************************************

FUNCTION lfvFileDir

lcOldDir = lcFileDir
lcFileDir = GETDIR(lcFileDir,'Select Settings File Directory')
IF EMPTY(lcFileDir)
  lcFileDir = lcOldDir
ENDIF
=lfRefresh(lcSetsWnd)
*!*************************************************************
*! Name       : lfReadAct
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Local Read Function
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************

FUNCTION lfReadAct


*!*************************************************************
*! Name       : lfvClose
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Valid Function for Close Button
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClose()
*!*************************************************************

FUNCTION lfvClose
CLEAR READ

*!*************************************************************
*! Name       : lfDelFile
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Delete a previously generated file
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDelFile()
*!*************************************************************

FUNCTION lfDelFile
ERASE lcFileDir+lcFileName+'.TXT'

*!*************************************************************
*! Name       : lcCrtFile
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Generate a new text file
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  the new created file handle
*!*************************************************************
*! Example            :  =lcCrtFile()
*!*************************************************************

FUNCTION lcCrtFile

lnFileHNDL = FCREATE(lcFileDir+lcFileName+'.TXT')

RETURN lnFileHNDL


*!*************************************************************
*! Name       : lfGenHDR
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Generate a header for the new settings file
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfGenHDR()
*!*************************************************************
FUNCTION lfGenHDR

=FPUTS(lnFileHNDL,'                                 ARIA Advantage Series 2.7')
=FPUTS(lnFileHNDL,'                                      System Settings')
=FPUTS(lnFileHNDL,'Date :'+DTOC(DATE())+'                                                        By User: '+ ALLTRIM(gcUser_ID))
=FPUTS(lnFileHNDL,'Number of licensed Users = ' + ALLTRIM(STR(gnMaxUsers)))


*!*************************************************************
*! Name       : lfGenHWSet
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Writes the hardware settings for the new settings file
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfGenHWSet()
*!*************************************************************
FUNCTION lfGenHWSet
PRIVATE lnLoop

=FPUTS(lnFileHNDL,'Hardware Settings:')

lnLoop = ALEN(laHWSetts,1)
FOR lnI = 1 TO lnLoop
  =FPUTS(lnFileHNDL,laHWSetts[lnI,1] + '=' +;
    IIF(TYPE('EVAL(laHWSetts[lnI,2])')='C',EVAL(laHWSetts[lnI,2]),STR(EVAL(laHWSetts[lnI,2]))))
ENDFOR


*!*************************************************************
*! Name       : lfGenPrnt
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Writes the used printers settings to the new settings file
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfGenPrnt()
*!*************************************************************
FUNCTION lfGenPrnt

PRIVATE lnPrntCnt
STORE 1 TO lnPrntCnt
llSycPrnt = gfOpenFile(gcSysHome+'SycPrint')

=FPUTS(lnFileHNDL,'System Printers:')
SCAN
  =FPUTS(lnFileHNDL,STR(lnPrntCnt) + '-'+cPrint_id+'Driver:' + cPrint_Nam)
  lnPrntCnt = lnPrntCnt +1
ENDSCAN
IF llSycPrnt
  USE IN SYCPRINT
  llSycPrnt = .F.

ENDIF

FUNCTION lfGenCmpHD
=FPUTS(lnFileHNDL,'Company:'+CCOM_NAME)

*!*************************************************************
*! Name       : lfGentSet
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Writes the Settings for the passed module
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfGentSet()
*!*************************************************************

FUNCTION lfGentSet
PARAMETER lcModule
PRIVATE lcMessage

SELECT SETUPS
lcToSeek = IIF(EMPTY(lcModule),'ÿÿ',lcModule)
IF SEEK(lcToSeek)
  SCAN REST WHILE capp_id = lcToSeek
    WAIT 'Generating Settings for company ' + sycComp.cComp_id + IIF(EMPTY(lcModule),'',' Module: '+lcModule) WINDOW NOWAIT
    lcValue = mData_def
    =FPUTS(lnFileHNDL,cFld_head+'= ' + lcValue)
  ENDSCAN
ELSE
  IF EMPTY(lcModule)
    lcMessage = 'Company Setup Was not done'
  ELSE
    lcMessage = 'Setup for Module ' + lcModule + ' Was not done'
  ENDIF
  =FPUTS(lnFileHNDL , lcMessage)
ENDIF

*!*************************************************************
*! Name       : lfGetMod
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Searches the installed modules for the modules
*!            : we're going to generate its setups
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfGentSet()
*!*************************************************************

FUNCTION lfGetMod
PARAMETER lcModules

DIMENSION laModules[1]
STORE '' TO laModules
lnModNum = OCCURS('|',lcModules)

FOR lnI = 1 TO lnModNum + 1
  lcModEl = SUBSTR(lcModules , 1 , MAX(ATC('|',lcModules) - 1,2))
  IF lcModEl $ 'SP AR AL IC MA MF PO PS SO SR EB'
    IF EMPTY(laModules[1]) AND ALEN(laModules,1) = 1
      laModules[1]  = lcModEl
    ELSE
      DIMENSION laModules[ALEN(laModules,1)+1]
      laModules[ALEN(laModules,1)]  = lcModEl
    ENDIF
  ENDIF
  lcModules = SUBSTR(lcModules,ATC('|',lcModules)+1 ,LEN(lcModules))
ENDFOR

*!*************************************************************
*! Name       : lfPrepDic
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : Opens sydfield and sydfilfld and establishes a
*!            : relation between them
*!*************************************************************
*! Calls      : gfOpenFile
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfPrepDic()
*!*************************************************************
FUNCTION lfPrepDic
llFileFld = gfOpenFile(gcSysHome+'sydflfld','Cfile_nam','SH')
llField = gfOpenFile(gcSysHome+'sydfield','Cfld_name','SH')

SELECT sydFlFld
SET RELATION TO cfld_name INTO Sydfield ADDITIVE

*!*************************************************************
*! Name       : lfWrtApSet
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : writes the apsetup to the settings file
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfWrtApSet()
*!*************************************************************
FUNCTION lfWrtApSet
SELECT sydFlFld
IF SEEK('APSETUP')
  WAIT 'Generating Settings for company ' + sycComp.cComp_id + ' Module: AP' WINDOW NOWAIT
  SCAN REST WHILE CFILE_NAM = PADR('APSETUP',8) FOR !(CFLD_NAME $ 'CADD_USER |DADD_DATE |CADD_TIME LLOK_STAT |DLOK_DATE |CLOK_USER |CLOK_TIME |COWNER    ')
    lcField = EVAL("APSETUP."+ALLT(EVAL('SYDFLFLD.CFLD_NAME')))

    *-- Map the field value to string type
    *-- if the gained value is a logical --> Yes/No
    *--                          Numeric --> String(gained value)
    *--                          Date    --> DTOC(gained value)
    lcToPRNT = lcField
    *--logical --> Yes/No
    IF TYPE('lcField') = 'L'
      IF lcField
        lcToPRNT = 'Yes'
      ELSE
        lcToPRNT = 'No'
      ENDIF
    ELSE
      *-- Numeric --> String(gained value)
      IF TYPE('lcField') = 'N'
        lcToPRNT = STR(lcField)
      ENDIF
      *-- Date    --> DTOC(gained value)
      IF TYPE('lcField') = 'D'
        lcToPRNT = DTOC(lcField)
      ENDIF
    ENDIF
    = FPUTS(lnFileHNDL,SYDFIELD.CFLD_HEAD +' = ' + ALLTRIM(lcToPRNT))
  ENDSCAN
ENDIF


*!*************************************************************
*! Name       : lfWrtGLSet
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : writes the apsetup to the settings file
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfWrtGLSet()
*!*************************************************************
FUNCTION lfWrtGLSet
SELECT SYDFLFLD
IF SEEK('GLSETUP')
  SCAN REST WHILE CFILE_NAM = PADR('GLSETUP',8) FOR !(CFLD_NAME $ 'CADD_USER |DADD_DATE |CADD_TIME LLOK_STAT |DLOK_DATE |CLOK_USER |CLOK_TIME |COWNER    ')
    WAIT 'Generating Settings for company ' + sycComp.cComp_id + ' Module: GL' WINDOW NOWAIT
    lcField = EVAL("GLSETUP."+ALLT(EVAL('SYDFLFLD.CFLD_NAME')))

    *-- Map the field value to string type
    *-- if the gained value is a logical --> Yes/No
    *--                          Numeric --> String(gained value)
    *--                          Date    --> DTOC(gained value)
    lcToPRNT = lcField
    *--logical --> Yes/No
    IF TYPE('lcField') = 'L'
      IF lcField
        lcToPRNT = 'Yes'
      ELSE
        lcToPRNT = 'No'
      ENDIF
    ELSE
      *-- Numeric --> String(gained value)
      IF TYPE('lcField') = 'N'
        lcToPRNT = STR(lcField)
      ENDIF
      *-- Date    --> DTOC(gained value)
      IF TYPE('lcField') = 'D'
        lcToPRNT = DTOC(lcField)
      ENDIF
    ENDIF
    = FPUTS(lnFileHNDL,SYDFIELD.CFLD_HEAD + ' = ' + ALLTRIM(lcToPRNT))
  ENDSCAN
ENDIF

*!*************************************************************
*! Name       : lfCloseDic
*: Developer  : Hossam El Etreby {HDM}
*: Date       : 09/22/1999
*! Purpose    : to close the sydflfld and sydfield files case
*!            : they were opened by the program
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfCloseDic()
*!*************************************************************

FUNCTION lfCloseDic

IF llFileFld
  USE IN SYDFLFLD
  llFileFld = .F.
ENDIF

IF llField
  USE IN SYDFIELD
  llField = .F.
ENDIF


FUNCTION lfProceed
*-- Define these variables to make sure closing
*-- SycComp , Setups , SycPrint files if opened by this program
PRIVATE llSycComp , llSetups , llSycPrnt , llAPSetup , llGLSetup

*-- if the selected file exsists on the disk and
*-- the user selected not to overwrite it
*-- we have to double check the file existance
*-- if the user wants to overwrite the file
*-- erase it from the drive only when we are going to
*-- generate it
*-- Otherwise Re-Validate the file name
*-- And force the user to get a new file name

IF FILE(lcFileDir+lcFileName+'.TXT')
  IF llOverWrt
    =lfDelFile()
  ELSE
    =lfvFileNam()
    RETURN
  ENDIF
ENDIF
SHOW GET pbProc DISABLE

*-- Call a function to create the new file
*-- and return it's handle ID
lnFileHNDL = lcCrtFile()

*-- Call A function to write a header to the file
=lfGenHDR()
=FPUTS(lnFileHNDL,REPLICATE('-',90))

*-- Call a function to generate the hardware settings
llCallHWSet = lfGenHWSet()
=FPUTS(lnFileHNDL,REPLICATE('-',90))

*-- Collect Printers from SycPrint

=lfGenPrnt()
=FPUTS(lnFileHNDL,REPLICATE('-',90))

*-- Open the Companies File
llSycComp = gfOpenFile(gcSysHome+'SYCCOMP')
SELECT SYCCOMP

SCAN
  *-- Generate Company Information Header
  =lfGenCmpHD()
  *--
  llSetups = gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'SETUPS','NVARPOS','SH')

  *-- Generate Company Steup
  =lfGentSet('')

  *-- Get Company Installed Modules
  lcCmpMdl = ALLTRIM(SYCCOMP.mComp_mdl)

  *-- if AP OR GL is installed for the current company
  *-- call a function to open the sydFlFld & sydField Files
  *-- and establish a relation between them

  IF 'AP' $ lcCmpMdl OR 'GL' $ lcCmpMdl
    =lfPrepDic()
  ENDIF

  IF 'AP' $ lcCmpMdl
    llAPSetup = gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'APSETUP')
    *-- call a function to write the APSETUP
    *-- to the settings file
    =FPUTS(lnFileHNDL,REPLICATE('-',90))
    =FPUTS(lnFileHNDL,'Module: ' + 'AP')
    =lfWrtApSet()
    IF llAPSetup
      USE IN APSETUP
      llAPSetup = .F.
    ENDIF
  ENDIF

  IF 'GL' $ lcCmpMdl
    llGLSetup = gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'GLSETUP')
    *-- call a function to write the GLSETUP
    *-- to the settings file

    =FPUTS(lnFileHNDL,REPLICATE('-',90))
    =FPUTS(lnFileHNDL,'Module: ' + 'GL')
    =lfWrtGLSet()
    IF llGLSetup
      USE IN GLSETUP
      llGLSetup = .F.
    ENDIF
  ENDIF

  =lfGetMod(lcCmpMdl)
  *-- Start reading module Setup
  FOR lnLoop = 1 TO ALEN(laModules,1)
    =FPUTS(lnFileHNDL,REPLICATE('-',90))
    IF !EMPTY(laModules[lnLoop])
      =FPUTS(lnFileHNDL,'Module: ' + laModules[lnLoop])
    ENDIF
    IF laModules[lnLoop] = 'IC'
      lcStyHdr = gfItemMask('H',ALLTRIM(SYCCOMP.CCOM_DDIR))
      lcStyPic = gfItemMask('M',ALLTRIM(SYCCOMP.CCOM_DDIR))
      =FPUTS(lnFileHNDL,'Style Structure:')
      =FPUTS(lnFileHNDL,'Header : '+lcStyHdr)
      =FPUTS(lnFileHNDL,'Picture: '+lcStyPic)
    ENDIF
    IF !EMPTY(laModules[lnLoop])
      =lfGentSet(laModules[lnLoop])
    ENDIF
  ENDFOR
  =FPUTS(lnFileHNDL,REPLICATE('-',90))
  IF llSetups
    USE IN SETUPS
    llSetups = .F.
  ENDIF

  IF 'AP' $ lcCmpMdl OR 'GL' $ lcCmpMdl
    =lfCloseDic()
  ENDIF

ENDSCAN
=FCLOSE(lnFileHNDL)
WAIT CLEAR


FUNCTION lfReceive
PRIVATE lnCurPos , llNotePad
STORE 0  TO lnCurPos
STORE .F. TO llNotePad
lnFileHndl = FOPEN(lcFileName)
IF lnFileHndl <= 0
  WAIT WINDOW 'Cannot Open '+lcFileName+ ' File may be opened by another user'
  RETURN
ENDIF

*-- Get File Size
lnFileSize = FSEEK(lnFileHndl, 0, 2)

*-- Make Sure we are on the BOF
=FSEEK(lnFileHndl, 0)

IF lnFileSize < 0
  WAIT WINDOW 'File ' + lcFileDir+lcFileName+'.TXT is an EMPTY File, Nothing to Receive!'
  RETURN
ENDIF
SHOW GET pbProc DISABLE
*-- Open the NotePad File
llNotePad = gfOpenFile(gcDatadir+'NOTEPAD','Notepad','SH')
*-- Make sure there is a record for this customer settings
IF !SEEK('D'+ lcAccount)
  APPEND BLANK
  REPLACE TYPE WITH  'D' ,;
          KEY  WITH  lcAccount ,;
          CDESC WITH 'System settings for Customer '+lcAccount
ENDIF
IF !EMPTY(MNOTES)
  *-- Empty the customer prev settings
  REPLACE MNOTES WITH ''
ENDIF

*-- Start Copying the file
WAIT WINDOW 'Importing system settings ' NOWAIT

*-- Close the file
=FCLOSE(lnFileHndl)

*-- Append to memo field
APPEND MEMO MNOTES FROM &lcFileName



IF llNotePad
  USE IN NOTEPAD
ENDIF
WAIT WINDOW 'Finished Importing customer: '+ lcAccount + ' System Setttings'


FUNCTION lfGetFile

lcOldFile = lcFileName
lcFileName = GETFILE('TXT','Select Settings File','Open',0)

IF EMPTY(lcFileName)
  lcFileName = lcOldFile
ENDIF
SHOW GET lcFileName
=lfvFileNam()