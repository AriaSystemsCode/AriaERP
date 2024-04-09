*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\SM\SMGLENT.PRG
*:  Desc.       : G/L Entries
*:  System      : Aria 4lfAddCommen
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/2/2012
*:  Reference   : *E303311,1 
*:************************************************************************

*-- lcFileName : Variable that hold account chart file name
*-- lcFieldNam : Variable that hold account description field name
*-- lcGLVer    : Variable that hold G/L Link version

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

lcSTime = TIME()
gcWorkDir = oAriaApplication.WorkDir

*** CREATE A TEMPORARY FILE

IF USED(GLTEMP)
  USE IN &GLTEMP
ENDIF 
ERASE &gcWorkDir.&GLTEMP..DBF
ERASE &gcWorkDir.&GLTEMP..CDX

SELECT GLDIST
COPY REST TO &gcWorkDir.&GLTEMP FOR &lcRpExp
SELECT 0
USE &gcWorkDir.&GLTEMP EXCLUSIVE

SELECT (GLTEMP)
*** CREATE INDEX DEPENDS ON THE SELECTED SORT METHOD

DO CASE
  CASE lcRPSortBy = 'D'          && IFDEX ON TRANSACTION DATE
    INDEX ON DTOC(TRAN_DATE) TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'Tran_Date'
    lcFooter = 'Total for Transaction Date    :'
    lcFootVal = [DTOC(Tran_Date)]

  CASE lcRPSortBy = 'N'          && INDEX ON TRANSACTION NUMBER
    INDEX ON TRAN_NO TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'Tran_No'
    lcFooter = 'Total for Transaction Number  :'
    lcFootVal = [Tran_No]
    
  CASE lcRPSortBy = 'T'          && INDEX ON TRANSACTION TYPE
    INDEX ON TRAN_TYPE TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'Tran_Type'
    lcFooter = 'Total for Transaction Type    :'
    lcFootVal = [TRAN_DESC]

  CASE lcRPSortBy = 'A'          && INDEX ON Account+
    INDEX ON GlAccount+GlFYear+GlPeriod TAG &GLTEMP OF (gcWorkDir+GLTEMP+'.CDX')
    lcKey = 'GlAccount+GlFYear+GLPeriod'
    lcFooter = 'Total for Account/Period-Year :'
    lcFootVal = [GlAccount+' '+GLPeriod +'-'+GlFYear]
    
ENDCASE

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

IF RECCOUNT() = 0
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

GO TOP
lnAccLen = LEN(lcAccPic)  && Gl Account length.

DO gfDispRe WITH EVAL('lcRpForm')

USE IN (GLTEMP)
ERASE (gcWorkDir+GLTEMP+'.DBF')
ERASE (gcWorkDir+GLTEMP+'.CDX')
*-- end of report code.

*!*************************************************************
*! Name      : lfvTranNo
*! Developer : TMI
*! Date      : 12/02/2012
*! Purpose   : validate tran. No.
*!*************************************************************
FUNCTION lfvTranNo
loFld = _screen.ActiveForm.ActiveControl


lcTranVal = loFld.Value

IF !EMPTY(lcTranVal) AND !SEEK(lcTranVal,'GLDIST')
  llNothing  = lfTranBrw()
  loFld.Value = IIF(llNothing,GLDIST.Tran_No,'')
ENDIF  

*!*************************************************************
*! Name      : lfTranBrw
*! Developer : TMI
*! Date      : 12/02/2012
*! Purpose   : Browse all transctions No.
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

llReturn  = gfBrows(.F., 'Tran_No', 'laTemp',lcFile_Ttl)

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfAllTypes
*! Developer : TMI
*! Date      : 12/02/2012
*! Purpose   : Fill type array with transactions types
*!*************************************************************
FUNCTION lfAllTypes

= lfGLFiles()

IF !llMultCurr
  DIMENSION laType[20,1],laTypeRet[20,1]
ELSE
  DIMENSION laType[21,1],laTypeRet[21,1]
ENDIF  

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

laTypeRet[16,1] = 'JC'
laType[16,1]    = 'P/O JOB COST CLOSING'

laTypeRet[17,1] = 'NL'
laType[17,1]    = 'NON MAT. LIABILITY. '

laTypeRet[18,1] = 'KO'
laType[18,1]    = 'KEY OFF           . '

laTypeRet[19,1] = 'MC'
laType[19,1]    = 'MATERIAL JOB CLOSING'

laTypeRet[20,1] = 'JP'
laType[20,1]    = 'C/T JOB COST CLOSING'

IF llMultCurr
  laTypeRet[21,1] = 'EX'
  laType[21,1]    = 'EX.RATE DIFFERENCES'
ENDIF  

*-- end of lfAllTypes.

*!*************************************************************
*! Name      : lfGLFiles
*! Developer : TMI
*! Date      : 12/02/2012
*! Purpose   : open the GL_Link file account chart file according to 
*!             the GL version(ARIA,SBT,Others) and to the active company
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

IF EMPTY(oAriaApplication.ActiveCompanyID)
  llContinue = .F.
  *-- You have to select company first
  *-- <OK>
  = gfModalGen("INM00192B00000","Dialog")  
  llContinue = .F.
ENDIF

IF llContinue
  llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',oAriaApplication.ActiveCompanyID)))   = 'Y'
  IF !llGL_Link 
    *-- System has not been linked to gl_link yet
    *-- <OK>
    = gfModalGen("INM00292B00000","Dialog")  
    llContinue = .F.
  ENDIF
ENDIF

IF llContinue
  
  llChldComp = SEEK(oAriaApplication.ActiveCompanyID,'SycComp') AND !EMPTY(SycComp.cCompPrnt)
  lcDataDir  = IIF(SEEK(oAriaApplication.ActiveCompanyID,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
  lcPrntCo   = IIF(llChldComp,ALLTRIM(SycComp.cCompPrnt),'')
  lcGLVer    = ALLTRIM(UPPER(gfGetMemVar('M_GL_VERS',oAriaApplication.ActiveCompanyID)))
  lcGLCo     = ALLTRIM(UPPER(gfGetMemVar('M_GL_CO',oAriaApplication.ActiveCompanyID)))

  llMultCurr = gfGetMemVar("LLMULCURR",oAriaApplication.ActiveCompanyID)

  DO CASE
    *-- GL Version is SBT
    CASE lcGLVer = 'S'
      lcSBTGLDir = ALLTRIM(UPPER(gfGetMemVar('M_SYS_DIR',oAriaApplication.ActiveCompanyID)))
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
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,oAriaApplication.ActiveCompanyID),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      lcFile      = "GLACCHAR"
      lcPrGLDir   = IIF(SEEK(lcPrntCo,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      lcCodeFld   = 'CACCTCODE'       
      lcDescFld   = 'CACCNLDES'
      llOtherVer  = .F.

    *-- Other type of GL version
    OTHERWISE
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,oAriaApplication.ActiveCompanyID),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')

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
*! Name      : lfGLBATCH_LOCATE
*! Developer : TMI
*! Date      : 12/02/2012
*! Purpose   : LOCATE record in GLBATCH file
*!*************************************************************
FUNCTION lfGLBATCH_LOCATE

SELECT GLBATCH
SET ORDER TO BATCHNO
lnBatPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLDIST.GLBATCH'),1)
IF !EMPTY(laOGFxFlt[lnBatPos,6])
  =SEEK(laOGFxFlt[lnBatPos,6])
ENDIF

*-- end of lfGLBATCH_LOCATE.

*!*************************************************************
*! Name      : lfGetPic
*! Developer : Mohamed Atia Badrn (MAB)
*! Date      : 12/02/2012
*! Purpose   : Get Gl Account Picture
*!*************************************************************
FUNCTION lfGetPic
lnOldAlias = SELECT(0)    && Save the current alias

SELECT ACCOD
GO TOP
IF !EOF()
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF

SELECT (lnOldAlias)
RETURN lcRpSegMas
*-- end of lfGetPic.

*!*************************************************************
*! Name      : lfvClrRead
*! Developer : Albert Raif (ALB)
*! Date      : 12/02/2012
*! Purpose   : validate Transaction Type
*!*************************************************************
FUNCTION lfvClrRead
CLEARREAD()


*!*************************************************************
*! Name      : lfvTrnCode
*! Developer : Albert Raif (ALB)
*! Date      : 12/02/2012
*! Purpose   : validate Transaction Type
*!*************************************************************
FUNCTION lfvTrnCode

lfGLBATCH_LOCATE()

lnBatPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLDIST.GLBATCH'),1)

loFld = _screen.ActiveForm.ActiveControl
lcOldVal = loFld.OldValue

loFld = _screen.ActiveForm.ActiveControl
IF loFld.Value<>loFld.OldValue AND !EMPTY(laOGFxFlt[lnBatPos,6])
  lcKey = GLBATCH.CBATCHNO+ loFld.Value

  =lfVlFld(IIF(GLBATCH.CBATSTAT='P','GLPTRNHD','GLTRNSHD'),;
 'CTRANNO', 'lcKey' ,'',.F.,.F.,.F.,'[BATCHTRN]',;
 [FOR CBATCHNO=GLBATCH.CBATCHNO] ,.T.)

  lcVar = loFld.parent.oItem.cAssociate  
  loOgScroll.&lcVar. = lcKey
  loFld.Value = lcKey
ENDIF

*:************************************************************************
*: Program file  : GFVLFLD.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
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

loFld = _screen.ActiveForm.ActiveControl
lcOldVal = loFld.OldValue

IF EMPTY(lcDbfName) OR EMPTY(lcFrstField) ;
  OR TYPE('lcDbfName')<>'C' OR TYPE('lcFrstField')<>'C'
    &lcFrstRet = lcOldVal
    RETURN
ENDIF  

**** Validate the variable name to return value in it
IF TYPE('lcFrstRet')<>'C' OR TYPE(lcFrstRet)='U'
  &lcFrstRet = lcOldVal
  RETURN
ENDIF

lcFrstType = lfGetType(lcFrstRet)
lcSecType  =IIF(TYPE('lcSecRet')='C',lfGetType(lcSecRet),'U')

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

lcBrFields = "CBATCHNO :H='GL Batch number',CTRANNO :H='GL Transaction number',NTRNINDIC :H='Transaction indicator',CTRNSLEDN :H='SJ transaction number',"+;
             "CTRNDESC :H='Transaction description',CTRNREFER :H='Transaction reference',DTRNPDATE :H='Posting date',CTRNPYR :H='Posting year',"+;
             "CTRNPPRD :H='Posting period',CTRNSTAT :H='Transaction status',CTRNTYPE :H='CTRNTYPE',CTRNREVER :H='Reverse transaction entry',"+;
             "DTRNREVDT :H='Reversing date',CTRNREVYR :H='Reversing year',CTRNREVPR :H='Reversing period',NTRNTOTDR :H='Total debit',"+;
             "NTRNTOTCR :H='Total credit',CSRCMODUL :H='Source module',CSTANDARD :H='Standard type',CSRCJRNL :H='Source journal entry',"+;
             "CCOMP_ID :H='Company ID',CAUTTYPE :H='Automatic entry type',CAUTCODE :H='Automatic entry code'"
IF !EMPTY(ALLTRIM(&lcFrstRet))
  IF SEEK(&lcFrstRet)
    SCATTER FIELDS &lcScatField TO laFieldBr MEMO
  ELSE  
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    =gfBrows(lckeyToUse,lcScatField,'laFieldBr',lcWinTitle,llCanFilter)
  ENDIF
ENDIF  

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


************************************************************
*! Name      : lfAcctBrow
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/02/2012
*! Purpose   : Account browse valid function
************************************************************
FUNCTION lfAcctBrow

=lfGLBATCH_LOCATE()

*GFVLFLD('GLACCHAR','CACCTCODE',VARREAD(),'',.F.,.F.,[CACCTCODE:H="Account code",CACCNLDES:H="Long report description"],'[ACCTCODE]',.F.,.t.))
lcBrFields    = [CACCTCODE:H="Account code",CACCNLDES:H="Long report description"]
lfGetBrow('GLACCOUNT','GLACCHAR','ACCTCODE','Chart of Accounts','CACCTCODE')

*- End of lfAcctBrow.

************************************************************
*! Name      : lfBatchBrow
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/02/2012
*! Purpose   : Batch browse valid function 
************************************************************
FUNCTION lfBatchBrow

=lfGLBATCH_LOCATE()

*GFVLFLD('GLBATCH','CBATCHNO',VARREAD(),'',.F.,.F.,.F.,'[BATCHNO]',[FOR !(CSRCMODUL $ 'GL')] ,.T.))
lcBrFields    = "CBATCHNO :H='GL Batch number',CBATSTAT :H='Batch status',CBATTYPE :H='Batch Type',CBATPYR :H='Posting year',DBATPBEG :H='Batch posting begin date',"+;
                "DBATPEND :H='Batch posting end date',CBATREFER :H='Batch reference',CBATDESC :H='Batch description',NBATCNTOT :H='Batch control total',"+;
                "NBATOTDR :H='Total debits',NBATOTCR :H='Total credits',CSRCMODUL :H='Source module',CCOMP_ID :H='Company ID'"
lfGetBrow('GLBATCH','GLBATCH','BATCHNO','Batch','CBATCHNO')

*- End of lfBatchBrow.

************************************************************
*! Name      : lfwRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/02/2012
*! Purpose   : When function for the report
************************************************************
FUNCTION lfwRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + 'GL\glrepfnc.fxp') ADDITIVE    && allow to see glrepfnc.fxp in program scope to use the function lfGetBrow
*- End of lfwRepWhen.