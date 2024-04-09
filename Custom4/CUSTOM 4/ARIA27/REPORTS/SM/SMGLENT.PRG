*:***************************************************************************
*: Program file  : SMGLENT
*: Program desc. : Print G/L entries
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : SYSTEM MANAGER (SM)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SMGLENT
*:***************************************************************************
*Modifications :
*E301197,1 MAB 04/17/1999 1- Transfer report to .FRX form (Dos and Windows),
*                              2- Add report format(Detail/Summary)
*                              3- Add new filter on Gl Account. 
*                              4- Add Reference Column.
*                              4- Title is becames Optional Title.
*B802410,1 WMA  10/04/2000 Collect the attchment needed for this bug that was done by 
*B802410,1 Waild Abo Al-Magd & was not completed.
*B603504,1 ADEL 12/04/2000 Add a new transactions types for Closing material MFG and Closing Material PO.
*B603862,1 SSE 09/06/2000 Adding new transaction type for Closing C/T Cost Sheet
*B603862,1                and modify the 'JC' type to be for Closing P/O Cost Sheet
*B603892,1 KAM 10/19/2000 to add tran_tpe EX to latype array
*E302122,1 ALB 06/05/2003 Add Glbach No and Transaction No. to thr OG
*:***************************************************************************
*
*E301197,1 Comment out the following lines avoiding using too many variables[begin
*-- lcFileName : Variable that hold account chart file name
*-- lcFieldNam : Variable that hold account description field name
*-- lcGLVer    : Variable that hold G/L Link version
*XFNAME    = lcFileName
*XDFIELD   = lcFieldNam
*X_GL_VERS = lcGLVer
*XFILTER   = lcRpExp
*SORTBY    = lcRPSortBy
*E301197,1 Comment out the following lines avoiding using too many variables[End  

*E301197,1 The report becames .FRX report...[Begin]
lcSTime = TIME()
*E301197,1 The report becames .FRX report...[End  ]



*** CREATE A TEMPORARY FILE

*E301197,1 Create an empty temporary file then make index to it (not copy rest) [Begin]
GLTEMP = gfTEMPNAME()
SELECT GLDIST
COPY REST TO &gcWorkDir.&GLTEMP FOR &lcRpExp
SELECT 0
USE &gcWorkDir.&GLTEMP EXCLUSIVE

SELECT (GLTEMP)
*** CREATE INDEX DEPENDS ON THE SELECTED SORT METHOD
*E301197,1 lcFootVal : Variable to print its value in .FRX according to sort by.

DO CASE
  CASE lcRPSortBy = 'D'          && IFDEX ON TRANSACTION DATE
    INDEX ON DTOC(TRAN_DATE) TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'Tran_Date'
    lcFooter = 'Total for Transaction Date    :'
    *E301197,1
    lcFootVal = [DTOC(Tran_Date)]

  CASE lcRPSortBy = 'N'          && INDEX ON TRANSACTION NUMBER
    INDEX ON TRAN_NO TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'Tran_No'
    lcFooter = 'Total for Transaction Number  :'
    *E301197,1
    lcFootVal = [Tran_No]
    
  CASE lcRPSortBy = 'T'          && INDEX ON TRANSACTION TYPE
    INDEX ON TRAN_TYPE TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'Tran_Type'
    lcFooter = 'Total for Transaction Type    :'
    *E301197,1
    lcFootVal = [TRAN_DESC]

  CASE lcRPSortBy = 'A'          && INDEX ON Account+
    INDEX ON GlAccount+GlFYear+GlPeriod TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'GlAccount+GlFYear+GLPeriod'
    lcFooter = 'Total for Account/Period-Year :'
    *E301197,1
    lcFootVal = [GlAccount+' '+GLPeriod +'-'+GlFYear]
    
ENDCASE

*E301197,1 Evaluate end of each group to save one line in Dos FRX [Begin]
GO TOP
lcKeyVal = EVALUATE(lcKey)
SCAN
  IF !(lcKeyVal == EVALUATE(lcKey))
    SKIP -1
    REPLACE llOk_Stat WITH .T.  && To Control print footer in Dos Frx..
    SKIP
  ENDIF  
  lcKeyVal = EVALUATE(lcKey)
  REPLACE llOk_Stat WITH .F.
ENDSCAN

GO BOTTOM
REPLACE llOk_Stat WITH .T.
*E301197,1 Evaluate end of each group to save one line in Dos FRX [End  ]

*E301197 Make changes standard with system[Begin]
*Z = ALLTRIM(STR(RECCOUNT(),7))
*IF Z = '0'
IF RECCOUNT() = 0
  *?? CHR(7)
  *WAIT WINDOW 'NO RECORDS SELECTED TO DISPLAY ...' NOWAIT
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*WAIT WINDOW 'SORTING &Z RECORDS ...' NOWAIT
*E301197 Make changes standard with system[End]

*E301197,1 The report becames .FRX report...[Begin]
*WAIT CLEAR
*SET DEVICE TO PRINT
*DO lpPrint
*DO ENDREPORT
*SET DEVICE TO SCREEN

GO TOP
lnAccLen = LEN(lcAccPic)  && Gl Account length.
DO gfDispRe WITH EVAL('lcRpForm')
*E301197,1 The report becames .FRX report...[End  ]

USE IN (GLTEMP)
ERASE (gcWorkDir+GLTEMP+'.DBF')
ERASE (gcWorkDir+GLTEMP+'.CDX')
*-- end of report code.

*!*************************************************************
*! Name      : lpPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Print report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
*E301197 This function is no longer in use.

PROCEDURE lpPrint

PAGENO = 0
ROW    = 99

XTITLE  = lcRPTitle
R_TITLE = 'GENERAL LEDGER DISTRIBUTION REPORT'
R_WIDTH = 'W'

*E301197 Make changes standard with system[Begin]
*WAIT WINDOW 'REPORT PRINTING - <SPACE BAR> TO ABORT' NOWAIT
*E301197 Make changes standard with system[End  ]

lnTotAmnt  = 0
lcDocument = ''

*MAB NOW
*DO WHILE !EOF() .AND. INKEY() <> 32
DO WHILE !EOF()

  IF ROW >= 55
    PAGENO = PAGENO +1
    DO RPT_HDR WITH '',XTITLE,R_WIDTH
    IF lcGLVer = 'S'
      @ 05,01 SAY 'TRAN.# DATE     TYP DESCRIPTION          ACCOUNT   DESCRIPTION                                 AMOUNT BATCH#'
    ELSE
      @ 05,01 SAY 'TRAN.# DATE     TYP DESCRIPTION          ACCOUNT                  DESCRIPTION                                      AMOUNT BATCH#'
    ENDIF
    @ 06,01 SAY REPLICATE ('=',130)
    ROW = 7
  ENDIF

************ G/L DISTRIBUTION REPORT LAYOUT FOR << COMPANY 01 >>  ***************
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....*....1....*....2....*....3
* TRAN.# DATE     TYP DESCRIPTION          ACCOUNT   DESCRIPTION                                 AMOUNT BATCH#
* 123456 XX/XX/XX 12  12345678901234567890 123456789 12345678901234567890123456789012345 12345678901.99 XXXXXX
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+.
*
************ G/L DISTRIBUTION REPORT LAYOUT FOR << COMPANY 99 >>  ***************
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....*....1....*....2....*....3..
* TRAN.# DATE     TYP DESCRIPTION          ACCOUNT                  DESCRIPTION                                      AMOUNT BATCH#
* 123456 XX/XX/XX 12  12345678901234567890 123456789012345678901234 1234567890123456789012345678901234567890 12345678901.99 XXXXXX
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+.

  lcDocument = EVAL(lcKey)
  DO CASE
    CASE lcRPSortBy = 'D'
      lcFoot1 = DTOC(Tran_Date)
    CASE lcRPSortBy = 'N'
      lcFoot1 = Tran_No
    CASE lcRPSortBy = 'T'
      lcFoot1= TRAN_DESC
    CASE lcRPSortBy = 'A'
      lcFoot1 = GlAccount+' '+GLPeriod +'-'+GlFYear
  ENDCASE
  SCAN WHILE lcDocument = EVAL(lcKey) .AND. ROW < 55
    @ ROW,001 SAY TRAN_NO
    @ ROW,008 SAY TRAN_DATE
    @ ROW,017 SAY TRAN_TYPE
    @ ROW,021 SAY TRAN_DESC
    XGLACCOUNT = ALLTRIM(GLACCOUNT)

    @ ROW,042 SAY XGLACCOUNT
    *-- WAM 03/20/94
    *-- Name of chart of account file and account description field in that
    *-- file will depend on the GL linked application.
    *-- If linked to 'OTHERS' account description will not be printed.

    IF lcGLVer <> 'O'
      SELECT &lcFileName
      IF SEEK(XGLACCOUNT)
        DO CASE
          CASE lcGLVer = 'S' .OR. lcGLVer = 'A'
            @ ROW,067 SAY SUBSTR(&lcFieldNam,1,40)
          CASE lcGLVer = 'S'
            @ ROW,052 SAY &lcFieldNam
        ENDCASE
      ENDIF
    ENDIF

    *PADR(GLACCOUNT,24) + ' / ' + IIF(lcGLVer = 'O','',IIF(SEEK(ALLTRIM(GLACCOUNT),lcFileName),PADR(EVALUATE(lcFileName+'.'+lcFieldNam),47),''))

    SELECT &GLTEMP
    IF lcGLVer = 'S'
      @ ROW,088 SAY nEqvAmnt PICTURE '99999999999.99'
      @ ROW,103 SAY GLBATCH
    ELSE
      @ ROW,108 SAY nEqvAmnt PICTURE '99999999999.99'
      @ ROW,123 SAY GLBATCH
    ENDIF
    lnTotAmnt = lnTotAmnt + nEqvAmnt
    ROW = ROW+1
  ENDSCAN
  IF lcDocument <> EVAL(lcKey)
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW+1
    @ ROW,01 SAY lcFooter + lcFoot1
    @ ROW,IIF(lcGLVer='S',88,108) SAY lnTotAmnt PICTURE '99999999999.99'
    @ ROW+1,00 SAY REPLICATE('-',132)
    ROW = ROW+2
    lnTotAmnt = 0
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfvTranNo
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate tran. No.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvTranNo()
*!*************************************************************

FUNCTION lfvTranNo

lcTranVar = VARREAD()
lcTranVal = &lcTranVar

IF !EMPTY(lcTranVal) AND !SEEK(lcTranVal,'GLDIST')
  llNothing  = lfTranBrw()
  &lcTranVar = IIF(llNothing,GLDIST.Tran_No,'')
ENDIF  

*!*************************************************************
*! Name      : lfTranBrw
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Browse all transctions No.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfTranBrw()
*!*************************************************************

FUNCTION lfTranBrw

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "linktype,link_code"

lcBrFields = "Tran_No   :H='Tran. No.',"+;
             "Tran_Type :H='Tran. Type',"+;
             "Tran_Desc :H='Description',"+;
             "Tran_date :H='Tran. date'"
             
lcFile_Ttl  = 'Tarn. No.'

SELECT GLDIST
DECLARE laTemp[1]

llReturn  = gfBrows(.F., 'Tran_No', 'laTemp')

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfAllTypes
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Fill type array with transactions types
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAllTypes()
*!*************************************************************
*B603504,1 ADEL 12/04/2000 Add a new transactions types for Closing material MFG and Closing Material PO.
****************************************************************************************

FUNCTION lfAllTypes

*-- MAB Calling Transfered before array defenation which save time if return. [Begin]
= lfGLFiles()
*-- MAB Calling Transfered before array defenation which save time if return. [End  ]

*--E301197 Add Key Off Transaction.[Begin
*DIMENSION laType[17,1],laTypeRet[17,1]
*B603504,1 (Begin) Add a new transactions types for Closing material MFG and Closing Material PO.
*DIMENSION laType[18,1],laTypeRet[18,1]

*B603862,1 Add Type for Closing C/T Cost Sheet [Begin]
*DIMENSION laType[19,1],laTypeRet[19,1]
*B603892,1 KAM 10/19/2000 add tran_tpe to latype array [start]
IF !llMultCurr
  DIMENSION laType[20,1],laTypeRet[20,1]
ELSE
  DIMENSION laType[21,1],laTypeRet[21,1]
ENDIF  
*B603892,1 KAM [end]
*B603862,1 Add Type for Closing C/T Cost Sheet [End]
*B603504,1 (End)
*--E301197 Add Key Off Transaction.[End..


laTypeRet[1,1]  = 'IN'
laType[1,1]     = 'INVOICE             '
laTypeRet[2,1]  = 'VI'
laType[2,1]     = 'VOID INVOICE        '
laTypeRet[3,1]  = 'CR'
laType[3,1]     = 'CASH RECEIPT        '
laTypeRet[4,1]  = 'CA'
laType[4,1]     = 'CREDIT ADJUSTMENT   '
laTypeRet[5,1]  = 'DA'
laType[5,1]     = 'DEBIT ADJUSTMENT    '
laTypeRet[6,1]  = 'RM'
laType[6,1]     = 'RETURN MERCHANDISE  '
laTypeRet[7,1]  = 'VR'
laType[7,1]     = 'VOID RETURN         '
laTypeRet[8,1]  = 'IP'
laType[8,1]     = 'INVENTORY PHYSICAL  '
laTypeRet[9,1]  = 'IA'
laType[9,1]     = 'INVENTORY ADJUSTMENT'
laTypeRet[10,1] = 'PO'
laType[10,1]    = 'P/O RECEIVING       '
laTypeRet[11,1] = 'CT'
laType[11,1]    = 'C/T RECEIVING       '
laTypeRet[12,1] = 'ZE'
laType[12,1]    = 'ZERO OUT STOCK      '
laTypeRet[13,1] = 'MP'
laType[13,1]    = 'MATERIAL INV. PHYSI.'
laTypeRet[14,1] = 'MA'
laType[14,1]    = 'MATERIAL INV. ADJUS.'
laTypeRet[15,1] = 'MO'
laType[15,1]    = 'MATERIAL P/O REC.   '

*B603862,1 Modify description of JC [Begin]
*laTypeRet[16,1] = 'JC'
*laType[16,1]    = 'JOB COST CLOSING ADJ'
laTypeRet[16,1] = 'JC'
laType[16,1]    = 'P/O JOB COST CLOSING'
*B603862,1 Modify description of JC [End]

laTypeRet[17,1] = 'NL'
laType[17,1]    = 'NON MAT. LIABILITY. '

*--E301197,1 Add Key Off Transaction.[Begin
laTypeRet[18,1] = 'KO'
laType[18,1]    = 'KEY OFF           . '
*--E301197,1 Add Key Off Transaction.[End..
*B603504,1 (Begin) Add Closing Material PO transaction and Closing material MFG Order.
laTypeRet[19,1] = 'MC'
laType[19,1]    = 'MATERIAL JOB CLOSING'
*B603504,1 (End)

*B603862,1 Add new transaction type 'JP' for C/T closing cost sheet [Begin]
laTypeRet[20,1] = 'JP'
laType[20,1]    = 'C/T JOB COST CLOSING'
*B603862,1 Add new transaction type 'JP' for C/T closing cost sheet [Begin]
*B603892,1 add  tran_type KAM [start]
IF llMultCurr
  laTypeRet[21,1] = 'EX'
  laType[21,1]    = 'EX.RATE DIFFERENCES'
ENDIF  
*B603892,1 KAM [end]

*-- MAB Transfer the following function calling before array defenation
*--     which save time if return.
*= lfGLFiles()
*-- end of lfAllTypes.

*!*************************************************************
*! Name      : lfGLFiles
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : open the GL_Link file account chart file according to 
*!             the GL version(ARIA,SBT,Others) and to the active company
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGLFiles()
*!*************************************************************

FUNCTION lfGLFiles

STORE SPACE(0) TO lcTypeDesc,lcGLAcc,lcGLDesc,laComp,lcGLChrTag,;
                  lcGLDir,lcFile,lcGLCo,lcCodeFld,lcDescFld,lcSysDir,;
                  lcGLVer,lcGLCo,lcOldVal,lcPrntCo,lcPrSysDir,;
                  lcPrGLVer,lcPrGLCo,lcPrGLDir,lpopcPrFile,;
                  lcGLChart,lcAcMask
                  
STORE .F. TO llOtherVer,llChldComp

STORE 0 To lnAcLen

PRIVATE llContinue

llContinue = .T.

lcComp  = gcAct_Comp

IF EMPTY(lcComp)
  llContinue = .F.
  *-- You have to select company first
  *-- <OK>
  = gfModalGen("INM00192B00000","Dialog")  
  llContinue = .F.
ENDIF

IF llContinue
  llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
  IF !llGL_Link 
    *-- System has not been linked to gl_link yet
    *-- <OK>
    = gfModalGen("INM00292B00000","Dialog")  
    llContinue = .F.
  ENDIF
ENDIF

IF llContinue
  

  llChldComp = SEEK(lcComp,'SycComp') AND !EMPTY(SycComp.cCompPrnt)
  lcDataDir  = IIF(SEEK(lcComp,'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
  lcPrntCo   = IIF(llChldComp,ALLTRIM(SycComp.cCompPrnt),'')
  lcGLVer    = ALLTRIM(UPPER(gfGetMemVar('M_GL_VERS',lcComp)))
  lcGLCo     = ALLTRIM(UPPER(gfGetMemVar('M_GL_CO',lcComp)))


*B603892,1 KAM call gloal function to get the status of company[start] 
llMultCurr = gfGetMemVar("LLMULCURR",lcComp)
*B603892,1 KAM [end]


  DO CASE
    *-- GL Version is SBT
    CASE lcGLVer = 'S'
      lcSBTGLDir = ALLTRIM(UPPER(gfGetMemVar('M_SYS_DIR',lcComp)))
      USE (lcSBTGLDir+'SYSDATA') IN 0 AGAIN ALIAS (lcSysTmp)
      SELECT (lcSysTmp)
      LOCATE FOR SYSID = "GL" + lcGLCo
      IF !FOUND()
        *--lcInfoMsg = 'Company not found !!!'
        =gfModalGen('INM00269B00000','DIALOG')
        llContinue = .F.
      ELSE  &&FOUND
        *-- Get path for gl data and company name
        lcGLDir    = ALLTRIM(SUBSTR(DRIVE,61,30))         && DATA DIRECTORY PATH
        lcFile     = "GLACNT"+lcGLCo
        lcPrGLDir  = lcGLDir
      ENDIF
      USE IN (lcSysTmp)
      lcCodeFld   = 'GLACNT'       
      lcDescFld   = 'GLDESC'
      llOtherVer  = .F.

    *-- GL Version is ARIA
    CASE lcGLVer  = 'A'
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcFile      = "GLACCHAR"
      lcPrGLDir   = IIF(SEEK(lcPrntCo,'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcCodeFld   = 'CACCTCODE'       
      lcDescFld   = 'CACCNLDES'
      llOtherVer  = .F.

    *-- Other type of GL version
    OTHERWISE
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,lcComp),'SycComp'),ALLTRIM(SycComp.cCom_dDir),'')
      lcFile      = ''      
      lcCodeFld   = ''       
      lcDescFld   = ''
      llOtherVer  = .T.

  ENDCASE

  IF lcGLVer <> 'O'
    IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile+'.DBF')
      *-- Chart of account file for this company not found !!!
      *-- <OK>
      = gfModalGen("INM00293B00000","Dialog")
      llContinue = .F.
    ELSE
      IF USED(lcFile)
        USE IN (lcFile)
      ENDIF
      USE (IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile) IN 0 AGAIN SHARED
      DO CASE
        CASE lcGLVer = 'S'
          SET ORDER TO GLACNT IN (lcFile)
        CASE lcGLVer = 'A'
          SET ORDER TO ACCTCODE IN (lcFile)        
      ENDCASE
    ENDIF    &&IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+......
  ENDIF   &&IF lcGLVer <> 'O'
ENDIF  

IF llContinue
  lcFileName = lcFile
  lcFieldNam = lcDescFld
  SET ORDER TO GLDISTNO IN GLDIST
ELSE
  llOgTrmnat = .T.
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Mohamed Atia Badrn (MAB)
*! Date      : 04/17/99
*! Purpose   : Save current read old value
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWOldVal()
*!*************************************************************
*E301197,1
FUNCTION lfWOldVal
lcOldVal=EVAL(VARREAD())

*E302122,1 Add Glbach No and Transaction No. to thr OG [Begin]
IF !USED('GLBATCH')
  =gfOpenFile(gcDataDir+'GLBATCH','BATCHNO','SH')
ENDIF

SELECT GLBATCH
SET ORDER TO BATCHNO
lnBatPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLDIST.GLBATCH'),1)
IF !EMPTY(laOGFxFlt[lnBatPos,6])
  =SEEK(laOGFxFlt[lnBatPos,6])
ENDIF

IF !USED('GLPTRNHD')
  =gfOpenFile(gcDataDir+'GLPTRNHD','','SH')
ENDIF
IF !USED('GLTRNSHD')
  =gfOpenFile(gcDataDir+'GLTRNSHD','','SH')
ENDIF
*E302122,1 Add Glbach No and Transaction No. to thr OG [End]
*-- end of lfWOldVal.

*!*************************************************************
*! Name      : lfGetPic
*! Developer : Mohamed Atia Badrn (MAB)
*! Date      : 04/17/99
*! Purpose   : Get Gl Account Picture
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetPic()
*!*************************************************************
*E301197,1
FUNCTION lfGetPic
lnOldAlias = SELECT(0)    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

IF NOT USED("ACCOD")
  SELECT 0
  USE &gcDataDir.ACCOD 
  llUesdBefo = .T.
ENDIF
SELECT ACCOD

GO TOP
IF !EOF()
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF

IF llUesdBefo       && .F.- this file used by the system
  USE IN ACCOD
ENDIF

SELECT (lnOldAlias)
RETURN lcRpSegMas
*-- end of lfGetPic.

*!*************************************************************
*! Name      : lfvClrRead
*! Developer : Albert Raif (ALB)
*! Date      : 06/05/2003
*! Purpose   : validate Transaction Type
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvClrRead()
*!*************************************************************
*E302122,1 ALB 06/05/2003 Add Glbach No and Transaction No. to thr OG
FUNCTION lfvClrRead
CLEAR READ

*!*************************************************************
*! Name      : lfStBat
*! Developer : Albert Raif (ALB)
*! Date      : 06/05/2003
*! Purpose   : validate Transaction Type
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStBat()
*!*************************************************************
*E302122,1 ALB 06/05/2003 Add Glbach No and Transaction No. to thr OG
FUNCTION lfStBat
IF lnParm = "S"
*  GOTO TOP IN GLBATCH
ENDIF


*!*************************************************************
*! Name      : lfvTrnCode
*! Developer : Albert Raif (ALB)
*! Date      : 06/05/2003
*! Purpose   : validate Transaction Type
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvTrnCode()
*!*************************************************************
*E302122,1 ALB 06/05/2003 Add Glbach No and Transaction No. to thr OG

FUNCTION lfvTrnCode

lnBatPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLDIST.GLBATCH'),1)

IF lcOldVal<>EVAL(VARREAD()) AND !EMPTY(laOGFxFlt[lnBatPos,6])
  lcKey = GLBATCH.CBATCHNO+ EVAL(VARREAD())
  lcVar = VARREAD()
  =lfVlFld(IIF(GLBATCH.CBATSTAT='P','GLPTRNHD','GLTRNSHD'),;
 'CTRANNO', 'lcKey' ,'',.F.,.F.,.F.,'[BATCHTRN]',;
 [FOR CBATCHNO=GLBATCH.CBATCHNO] ,.T.)

&lcVar = lcKey
ENDIF



*:************************************************************************
*: Program file  : GFVLFLD.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*
FUNCTION lfVlFld
PARAMETERS lcDbfName,lcFrstField,lcFrstRet,lcOldVal,;
           lcSecField,lcSecRet,lcBrowsFld,lcOrdName,;
           lcKeyToUse,llCanFilter
PRIVATE lcCurDbf,lcFrstType,lcSecType,lcTmpBrFld,lnCurRec,lcWinTitle,;
        lcFilter,laScrMode
        
IF lcFrstField="CACCTCODE"
  IF EMPTY(STRTRAN(EVAL(lcFrstRet),'-'))
    IF ATC('-',EVAL(lcFrstRet)) > 0
      STORE "" TO (lcFrstRet)
    ENDIF  
    RETURN
  ENDIF
ENDIF  
        
IF EMPTY(EVAL(lcFrstRet))
  RETURN
ENDIF
DIMENSION laScrMode[5]        
STORE .F. TO laScrMode
lcCurDbf=SELECT()
lnCurRec=IIF(RECNO()>RECCOUNT(),0,RECNO())

llMyUse=.F.           
****** Validate the database to use name and the first field name           
lcFileName=IIF(TYPE('lcFileName')='U','',lcFileName)
IF EMPTY(lcDbfName) OR EMPTY(lcFrstField) ;
  OR TYPE('lcDbfName')<>'C' OR TYPE('lcFrstField')<>'C'
    &lcFrstRet = lcOldVal
    RETURN
ENDIF  
********************************************************

**** Validate the variable name to return value in it
IF TYPE('lcFrstRet')<>'C' OR TYPE(lcFrstRet)='U'
  &lcFrstRet = lcOldVal
  RETURN
ENDIF
********************************************************
lcFrstType = lfGetType(lcFrstRet)
lcSecType  =IIF(TYPE('lcSecRet')='C',lfGetType(lcSecRet),'U')
lcTmpBrFld=lcBrFields
IF !USED('SYDFILES')
  llMyUse=.T.
  
  SELECT 0
  USE &gcSysHome.SYDFILES ORDER Cfile_nam 
ELSE

  SELECT SYDFILES
  lcOldOrd=ORDER()
  lcFilter=FILTER()
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())

  SET ORDER TO TAG Cfile_nam
  SET FILTER TO
ENDIF  
lcWinTitle=lcDbfName
IF SEEK(ALLTRIM(lcDbfName))
  lcWinTitle=IIF(!EMPTY(cfile_ttl),cFile_ttl,lcWinTitle)
ENDIF  

IF llMyUse
  USE IN SYDFILES
ELSE
  SELECT SYDFILES  
  IF !EMPTY(lcOldOrd)
    SET ORDER TO &lcOldOrd
  ELSE
    SET ORDER TO
  ENDIF
  IF !EMPTY(lcFilter)
    SET FILTER TO &lcFilter
  ELSE
    SET FILTER TO
  ENDIF
  IF lnRecNo<>0
    GO lnRecNo
  ENDIF
ENDIF

llMyUse=.F.
IF !USED(lcDbfName)
  llMyUse=.T.
  lcPath=IIF(SUBSTR(UPPER(lcDbfName),1,2)='SY',gcSysHome,gcDataDir)  
  SELECT SELECT(1)
  USE &lcPath.&lcDbfName ORDER IIF(EMPTY(lcOrdName),1,&lcOrdName)
ELSE
  SELECT(lcDbfName)
  lcOldOrd=ORDER()
  lcFilter=FILTER()
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
  SET ORDER TO IIF(EMPTY(lcOrdName),1,&lcOrdName)
  SET FILTER TO
ENDIF
DECLARE laFieldBr[IIF(lcSecType='U',1,2)]
lcScatField=lcFrstField+IIF(EMPTY(lcSecField),'',','+lcSecField)
lcBrFields=gfDbfField(lcDbfName)
IF !EMPTY(ALLTRIM(&lcFrstRet))
  IF SEEK(&lcFrstRet)
    SCATTER FIELDS &lcScatField TO laFieldBr MEMO
  ELSE  
    IF LASTKEY()=13
       IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
         GO RECNO(0)
       ELSE
         GO TOP
       ENDIF
      =gfBrows(lckeyToUse,lcScatField,'laFieldBr',lcWinTitle,llCanFilter)
    ENDIF  
  ENDIF
ENDIF  
lcBrFields=lcTmpBrFld
SELECT(lcCurDbf)
IF lnCurRec<>0
  GO lnCurRec
ENDIF
IF !EMPTY(laFieldBr[1])
  IF lcFrstType='F'
    REPLACE &lcFrstRet WITH laFieldBr[1]
  ELSE
    IF lcFrstType='V'
     &lcFrstRet=laFieldBr[1]  
    ENDIF 
  ENDIF
  IF lcSecType='F'
    REPLACE &lcSecRet WITH laFieldBr[2]
  ELSE
    IF lcSecType='V'
      &lcSecRet=laFieldBr[1]  
    ENDIF 
  ENDIF
ELSE
  IF lcFrstType='F'
    REPLACE &lcFrstRet WITH lcOldVal
  ELSE
    IF lcFrstType='V'
     &lcFrstRet=lcOldVal
    ENDIF 
  ENDIF
ENDIF

IF llMyUse
  USE IN &lcDbfName
ELSE
  SELECT(lcDbfName)  
  IF !EMPTY(lcOldOrd)
    SET ORDER TO &lcOldOrd
  ELSE
    SET ORDER TO
  ENDIF
  IF !EMPTY(lcFilter)
    SET FILTER TO &lcFilter
  ELSE
    SET FILTER TO
  ENDIF
  IF lnRecNo<>0
    GO lnRecNo
  ENDIF
ENDIF
SELECT(lcCurDbf)
SHOW GET &lcFrstRet
IF lcSecType<>'U'
  SHOW GET &lcSecRet
ENDIF  

*!**************************************************************
*!
*!               FUNCTION lfGetType
*!
*!**************************************************************
*  FUNCTION to check a name if it is an array element or a field
*  or variable
FUNCTION lfGetType
PARAMETERS lcWhchType
PRIVAT lcSelect,lcType
lcSelect=SELECT()
SELECT SELECT(1)
lcType=''
IF TYPE(lcWhchType)<>'U'
 lcType='V'
ENDIF
SELECT(lcSelect)
RETURN IIF(EMPTY(lcType),IIF(TYPE(lcWhchType)='U','U','F'),'V')
