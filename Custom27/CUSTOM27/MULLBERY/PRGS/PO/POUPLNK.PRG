*:***************************************************************************
*: Program file  : POUPLNK.PRG
*: Program desc. : IMPORT THE PO RECORD(S) FROM MULBERRY PO HEADER & DETAIL INTO ARIA
*: Date          : 02/07/2002
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order - (PO)
*: Developer     : Ahmed Maher (AMH)
*: Tracking Job Number: C102511
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POUPLNK
*:***************************************************************************
*: Modifications :
*: B606153,1 AMH 06/30/2002 Updating the notepad file instead of WOR_NOTE field.
*:***************************************************************************

*-- Initializing the necessary variables.
STORE SPACE(0)  TO lcSorcDir,lcSession,lcDivision,lcTermCode,lShipVia
lcPoHdr   = gfTempName()
lcPOLine  = gfTempName()

IF !gfSetup()
  RETURN
ENDIF  

lcSession  = gfsequence('cSession')

*-Create PO Header file. 
SELECT POSHDR
=AFIELDS(laStrufile)
lnFileStru = ALEN(laStrufile,1)
DIMENSION laStrufile[lnFileStru+1,4]
laStrufile[lnFileStru+1,1] = 'nSteps'
laStrufile[lnFileStru+1,2] = 'N'
laStrufile[lnFileStru+1,3] = 2
laStrufile[lnFileStru+1,4] = 0
=gfCrtTmp(lcPOHdr,@laStrufile,'cStyType+PO',lcPOHdr)
SELECT (lcPOHdr)
APPEND BLANK 
REPLACE cStyType WITH 'P',lMultiWare WITH .F.

*-Create line file. 
SELECT POSLN
=AFIELDS(laStrufile)
lnFileStru = ALEN(laStrufile,1)

DIMENSION laStrufile[lnFileStru+4,4]

laStrufile[lnFileStru+1,1] = 'nSteps'
laStrufile[lnFileStru+1,2] = 'N'
laStrufile[lnFileStru+1,3] = 2
laStrufile[lnFileStru+1,4] = 0

laStrufile[lnFileStru+2,1] = 'cAccName'
laStrufile[lnFileStru+2,2] = 'C'
laStrufile[lnFileStru+2,3] = 30
laStrufile[lnFileStru+2,4] = 0
laStrufile[lnFileStru+3,1] = 'Start'
laStrufile[lnFileStru+3,2] = 'D'
laStrufile[lnFileStru+3,3] = 8
laStrufile[lnFileStru+3,4] = 0
laStrufile[lnFileStru+4,1] = 'Complete'
laStrufile[lnFileStru+4,2] = 'D'
laStrufile[lnFileStru+4,3] = 8
laStrufile[lnFileStru+4,4] = 0

=gfCrtTmp(lcPOline,@laStrufile,'Style+STR(LineNo,6)',lcPOline)

DO (gcScrDir+gcWinAppl+"\POUPLINK.SPX")

IF USED(lcPoHdr)
  USE IN (lcPOHdr)
  ERASE (gcWorkDir+lcPOHdr+'.DBF')
  ERASE (gcWorkDir+lcPOHdr+'.CDX')
  ERASE (gcWorkDir+lcPOHdr+'.FPT')
ENDIF

IF USED(lcPOLine)
  USE IN (lcPOLine)
  ERASE (gcWorkDir+lcPOLine+'.DBF')
  ERASE (gcWorkDir+lcPOLine+'.CDX')
ENDIF

*:*************************************************************
*! Name      : lfCrUnSess
*! Developer : AHMED MAHER
*! Date      : 02/13/2002
*! Purpose   : Create uncomplete session record.
*:*************************************************************
*! Call      : lfUpdVars()
*!*************************************************************
*! Passed Parameters  :lcPushBt  -> push botton
*!                     lcUnCmSta -> status
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCrUnSess()
*!*************************************************************
FUNCTION lfCrUnSess
PARA lcPushBt,lcUnCmSta

lnAlias = SELECT()
SELECT UNCMSESS
IF lcUnCmSta='O'
  IF !SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
    IF !SEEK('I'+lcUnCmPgId+PADR(gcUser_id,10)) 
	  lcSvdPo    = " "
      INSERT INTO UNCMSESS ;
      (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,cCurrObj) VALUES ;
      ('O',lcUnCmPgId,gcUser_id,lcSession,lcAProgm,'',gdSysDate,TIME(),lcPushBt)
    ELSE
	  lcSvdPo    = " "
      REPLACE Status    WITH 'O',;
              cSession  WITH lcSession,;
              dTranDate WITH gdSysDate,;
              cTranTime WITH TIME(),;
              ccurrobj  WITH lcPushBt
    ENDIF
  ELSE
    REPLACE cCurrObj WITH lcPushBt
  ENDIF
  =RLOCK('UNCMSESS')
ELSE  && lcUnCmSta $ 'IC'
  *--Cancel OR Complete unCmSess record.
  IF SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
    REPLACE Status WITH lcUnCmSta
    UNLOCK
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN
*--end of lfCrUnSess.

*!************************************************************************
*! Name : lfGtOrder.
*! Developer : AHMED MAHER
*! Date      : 02/13/2002
*! Purpose : Edit the P/o number manualy  if the system was setup to do so.
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfGtOrder
PRIVATE lcPONum
lcPONum   = SPACE(6)
lcSctTtle = lcPOType+' Number'
lcScrText = 'Please enter the '+lcPOType+' number'
lcHdEsc1= ON('KEY','ESC')
ON KEY LABEL ESC 
DO (gcScrDir+"POGNum.SPX")
ON KEY LABEL ESC &lcHdEsc1
RETURN (lcPONum)
*--end of lfGtOrder.

*:*************************************************************
*! Name      : lfvPoNum
*! Developer : AHMED MAHER
*! Date      : 02/13/2002
*! Purpose   : Validate P/O no in entered manualy.
*:*************************************************************
FUNCTION lfvPoNum

llRet=.T.
IF EMPTY(lcPONum)
  *-You cannot leave the "+lcPOType+" number empty.
  =gfModalGen('TRM34021B34000','DIALOG',lcPOType)
  llRet=.F.
ENDIF
IF llRet AND LEN(ALLTRIM(lcPONum)) < 6
  *-lcPOType+" number must be six digits.
  =gfModalGen('TRM34022B34000','DIALOG',lcPOType)
  llRet=.F.
ENDIF
IF llRet AND SEEK(lcAType+lcPONum,"POSHdr")
  *-This "+lcPOType+" number is already exist.
  =gfModalGen('TRM34023B34000','DIALOG',lcPOType)
  llRet=.F.
ENDIF
RETURN llRet
*--end of lfVPoNum.

*!************************************************************************
*! Name      : lfvGetPath.
*! Developer : AHMED MAHER
*! Date      : 02/17/2002
*! Purpose   : Valid function of path name botton.
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvGetPath

lcSorcDir  = GETDIR('','Select the path of Mulberry import files')
SHOW GET lcSorcDir
IF EMPTY(lcSorcDir)
  SHOW GET PbProceed DISABLE
ELSE
  SHOW GET PbProceed ENABLE
ENDIF
*--end of lfvGetPath.

*!************************************************************************
*! Name      : lfvProceed.
*! Developer : AHMED MAHER
*! Date      : 02/17/2002
*! Purpose   : Valid function of Proceed botton.
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvProceed

*-- Check the existance file in the selected directory.
IF !FILE(lcSorcDir+"POHEADER.DBF")
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'File POHEADER.DBF not found. Please try again.')
  RETURN
ENDIF

IF !FILE(lcSorcDir+"PODETAIL.DBF")
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'File PODETAIL.DBF not found. Please try again.')
  RETURN
ENDIF
    
*-- Open needed files.
=gfOpenFile(lcSorcDir+"POHEADER" , '', "SH")
=gfOpenFile(lcSorcDir+"PODETAIL" , '', "SH")

*-- Check if po already exists.
WAIT WINDOW 'Checking the POHEADER file ...' NOWAIT
SET ORDER TO TAG POHEADERID IN POSHDR
SELECT POHEADER
IF SEEK(POHEADID,'POSHDR')
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'PO already exists. File has been rejected.')
  =gfCloseFile('POHEADER')
  =gfCloseFile('PODETAIL')
  RETURN
ENDIF
SET ORDER TO TAG POSHDR IN POSHDR
    
*-- Check if invalid vendor.
IF !SEEK(VENDORCODE,'APVENDOR') .OR. !('S'$APVENDOR.CVENSUPTYP)
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'Invalid vendor. File has been rejected.')
  =gfCloseFile('POHEADER')
  =gfCloseFile('PODETAIL')
  RETURN
ENDIF
    
*-- Check if invalid location.
IF !SEEK(LOCATION,'WAREHOUS') .OR. !WAREHOUS.LSTYINV
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'Invalid location. File has been rejected.')
  =gfCloseFile('POHEADER')
  =gfCloseFile('PODETAIL')
  RETURN
ENDIF
    
*-- Check if invalid division.
IF !lfGetCode('CDIVISION ',DIVISION)
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'Invalid division. File has been rejected.')
  =gfCloseFile('POHEADER')
  =gfCloseFile('PODETAIL')
  RETURN
ENDIF
    
*-- Check if invalid payment terms.
IF !lfGetCode('CTERMCODE ',PAYMENTTER)
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'Invalid payment terms. File has been rejected.')
  =gfCloseFile('POHEADER')
  =gfCloseFile('PODETAIL')
  RETURN
ENDIF
    
*-- Check if invalid ship via.
IF !lfGetCode('SHIPVIA   ',SHIPVIA)
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'Invalid ship via. File has been rejected.')
  =gfCloseFile('POHEADER')
  =gfCloseFile('PODETAIL')
  RETURN
ENDIF
    
*-- Check if invalid style/color.
WAIT WINDOW 'Checking the PODETAIL file ...' NOWAIT
SELECT PODETAIL
llReject = .F.
STORE 0 TO lnLastLine
lcPurCode = IIF(SEEK('DCPURCODE  ','CODES'),CODES.CCODE_NO,'')
SCAN
  IF !SEEK(STYLE+'-'+COLOR,'STYLE')
    =gfModalGen("INM000000B00000","DIALOG",'','',;
    'Invalid style-color. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
    
  *-- Check if invalid style/color/Scale.
  FOR lnI = 8 TO 1 STEP -1
    lnLastSize = lnI
    lcI = STR(lnI,1)
    IF QTY&lcI. # 0
      EXIT
    ENDIF
  ENDFOR
  IF SEEK('S'+STYLE.SCALE,'SCALE') .AND. lnLastSize > SCALE.CNT
    =gfModalGen("INM000000B00000","DIALOG",'','','Size scale for style-color '+STYLE+'-'+ALLTRIM(COLOR)+;
                ', is different from the size scale in Aria 27. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
     
  *-- Check if invalid style/color/location.
  IF !SEEK(STYLE+'-'+COLOR+POHEADER.LOCATION,'STYDYE')
    =gfModalGen("INM000000B00000","DIALOG",'','',;
    'Invalid style-color/location. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
      
  *-- Check if invalid style/color/division.
  IF STYLE.CDIVISION # lcDivision
    =gfModalGen("INM000000B00000","DIALOG",'','','Division for style-color '+STYLE+'-'+ALLTRIM(COLOR)+;
                ', is different from the PO division '+ALLTRIM(POHEADER.DIVISION)+'. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
      
  *-- Check if invalid style/color/quality.
  IF STYLE.CSTYGRADE # lfGetGrade(POHEADER.ORDERQUALI)
    =gfModalGen("INM000000B00000","DIALOG",'','','The quality of style-color '+STYLE+'-'+ALLTRIM(COLOR)+;
                ', is different from the PO quality. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
      
  *-- Check if invalid style/color/purch. group.
  IF !EMPTY(STYLE.CPURCODE) .AND. STYLE.CPURCODE # lcPurCode
    =gfModalGen("INM000000B00000","DIALOG",'','','The purchase group of style-color '+STYLE+'-'+ALLTRIM(COLOR)+;
                ', is different from the PO Purchase group. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
      
  *-- Check if invalid style/color/make.
  IF STYLE.MAKE
    =gfModalGen("INM000000B00000","DIALOG",'','','Style-color '+STYLE+'-'+ALLTRIM(COLOR)+;
                ' is not a purchased style. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
      
  *-- Check if invalid style/color/status.
  IF STYLE.STATUS = 'X'
    =gfModalGen("INM000000B00000","DIALOG",'','','Style-color '+STYLE+'-'+ALLTRIM(COLOR)+;
                'is a canceled style. File has been rejected.')
    llReject = .T.
    EXIT
  ENDIF
      
  *-- Add line to lcPoLine file.
  SELECT (lcPoLine)
  APPEND BLANK
  lnLastLine = lnLastLine + 1
  REPLACE CSTYTYPE   WITH 'P',;
          TRANCD     WITH '1',;
          LINENO     WITH lnLastLine,;
          VENDOR     WITH POHEADER.VENDORCODE,;
          STYLE      WITH PODETAIL.STYLE+'-'+PODETAIL.COLOR,;
          SCALE      WITH STYLE.SCALE,;
          CWARECODE  WITH POHEADER.LOCATION
              
  REPLACE QTY1       WITH PODETAIL.QTY1,;
          QTY2       WITH PODETAIL.QTY2,;
          QTY3       WITH PODETAIL.QTY3,;
          QTY4       WITH PODETAIL.QTY4,;
          QTY5       WITH PODETAIL.QTY5,;
          QTY6       WITH PODETAIL.QTY6,;
          QTY7       WITH PODETAIL.QTY7,;
          QTY8       WITH PODETAIL.QTY8,;
          TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8,;
          PRICE      WITH PODETAIL.TIECOST,;
          CPOHEADID  WITH PODETAIL.POHEADID,;
          CSTYGRADE  WITH POHEADER.ORDERQUALI,;
          PREPAK     WITH STYLE.CBUYPREPK,;
          CVENSTY    WITH STYLE.CVENSTY
  REPLACE NCOST1     WITH STYLE.NICOST1,;
          NCOST2     WITH STYLE.NICOST2,;
          NCOST3     WITH STYLE.NICOST3,;
          NCOST4     WITH STYLE.NICOST4,;
          NCOST5     WITH STYLE.NICOST5,;
          DISC_PCNT  WITH STYLE.DISC_PCNT,;
          GROS_PRICE WITH IIF(STYLE.Gros_Price=0 AND STYLE.Disc_Pcnt=0,nCost1,STYLE.Gros_Price),;
          NECOST1    WITH NCOST1,;
          NECOST2    WITH NCOST2,;
          NECOST3    WITH NCOST3,;
          NECOST4    WITH NCOST4,;
          NECOST5    WITH NCOST5,;
          LFLAG      WITH .F.
ENDSCAN
WAIT CLEAR
IF llReject
  =gfCloseFile('POHEADER')
  =gfCloseFile('PODETAIL')
  RETURN
ENDIF
    
*--add Po record.
*--To know which laData is the field, Use the following expresion.
*--No = (5*(row-1)) + colum
***********=*----1-----*----2-----*----3-----*----4-----*----5-----****
lcScFields ='Po        ,Vendor    ,Status    ,cDivision ,Entered   ,'+;
            'Complete  ,Available ,Origin    ,cFob      ,cTermCode ,'+;
            'ShipVia   ,QuotaCat  ,cLcNo     ,LCExpire  ,Contact   ,'+;
            'Phone     ,Insurance ,ShpName   ,cWareCode ,Link_Code ,'+;
            'cOutAddr1 ,cOutAddr2 ,cOutAddr3 ,cOutAddr4 ,cOutAddr5 ,'+;
            'cPriceCur ,cDutyCur  ,nPriceRat ,nDutyRat  ,PoTotal   ,'+;
            'nICost1   ,nICost2   ,nICost3   ,nICost4   ,nICost5   ,'+;
            'nStyOrder ,Receive   ,Damage    ,Cancel    ,Open      ,'+;
            'nAct_Cost1,nAct_Cost2,nAct_Cost3,nAct_Cost4,nAct_Cost5,'+;
            'nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,'+;
            'nFCost1   ,nFCost2   ,nFCost3   ,nFCost4   ,nFCost5   ,'+;
            'nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,'+;
            'nFActCost1,nFActCost2,nFActCost3,nFActCost4,nFActCost5,'+;
            'cMultiLot ,cPurCode  ,Buyer     ,Account   ,Store     ,'+;
            'cPONo     ,Color     ,Cost'
***********************************************************************

*--Initialize laData array.
SELECT POSHDR 
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
laData[2]  = POHEADER.VENDORCODE
laData[3]  = 'H'
laData[4]  = lcDivision
laData[5]  = POHEADER.ENTERED
laData[6]  = POHEADER.COMPLETE
laData[7]  = POHEADER.AVAILABLE
laData[8]  = POHEADER.ORIGIN
laData[9]  = POHEADER.FOB
laData[10] = lcTermCode
laData[11] = lShipVia
laData[12] = POHEADER.QUOTACATEG
laData[15] = APVENDOR.CVENCONT
laData[16] = APVENDOR.CPHONENO
laData[18] = WAREHOUS.CDESC
laData[19] = POHEADER.LOCATION
laData[20] = 'DEFDEF'
laData[21] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laData[22] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laData[23] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laData[24] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laData[25] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
laData[26] = gcBaseCurr
laData[27] = gcBaseCurr
laData[28] = 1
laData[29] = 1
laData[67] = lcPurCode
laData[68] = POHEADER.BUYER
lcAType     = 'P'
llMultiWare = .F.
llWareHous  = (UPPER(ALLTRIM(gfGetMemVar('M_WareHouse'))) = 'Y')
lnShpLoc    = 1
lcAProgm    = 'POUPLNK'
lcUnCmPgId  = PADR('STYLE_PO'+lcAType,10)
lcCrtCSH    = 'M'
STORE .F. TO laScrMode
laScrMode[4] = .T.
llRecalTot   = .T.
llMulCurr    = .F.
llContinue   = .F.
lcPOType     = 'P/O'
llGenOrNum   = (UPPER(ALLTRIM(gfGetMemVar('M_GenStOrN'))) = 'Y')
llApIstall   = (OCCURS('AP',gcComp_mdl)<>0)
DO gfSavePo IN (gcapphome+'POUPDATE.PRG')
SELECT POSHDR

*B606153,1 AMH Updating the notepad file instead of WOR_NOTE field [Start]
*REPLACE WOR_NOTE   WITH POHEADER.NOTES,;
        CORDERQUAL WITH POHEADER.ORDERQUALI,;
        CPOHEADID  WITH POHEADER.POHEADID,;
        LMULTIWARE WITH .F.,;
        LHASNOTES  WITH .F.,;
        NCURRUNIT  WITH 1,;
        NDCURUNIT  WITH 1
REPLACE CORDERQUAL WITH POHEADER.ORDERQUALI,;
        CPOHEADID  WITH POHEADER.POHEADID,;
        LMULTIWARE WITH .F.,;
        LHASNOTES  WITH .T.,;
        NCURRUNIT  WITH 1,;
        NDCURUNIT  WITH 1
IF !USED('NOTEPAD')
  =gfOpenFile(gcDataDir+"NOTEPAD" , "NOTEPAD", "SH")
ENDIF
SELECT NOTEPAD
IF !SEEK('P'+POSHDR.PO)
  APPEND BLANK
ENDIF
REPLACE TYPE   WITH 'P',;
        KEY    WITH POSHDR.PO,;
        CDESC  WITH 'Notes For Style P/o Number : '+POSHDR.PO,;
        MNOTES WITH POHEADER.NOTES
*B606153,1 AMH [End]

lcSorcDir = ''
SHOW GET lcSorcDir
SHOW GET PbProceed DISABLE
STORE .F. TO laScrMode
laScrMode[1] = .T.
*--end of lfvProceed.

*!************************************************************************
*! Name      : lfGetCode.
*! Developer : AHMED MAHER
*! Date      : 02/24/2002
*! Purpose   : Function to get code no.
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcFldNam = field name to get code
*!                      lcDesc   = code description
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfGetCode
PARAMETERS lcFldNam, lcDesc

PRIVATE lnAlias, llRet, lcVarName
lnAlias = SELECT(0)
llRet = .F.
lcVarName = 'l'+ALLTRIM(lcFldNam)
IF SEEK('D'+lcFldNam,'CODES') .AND. UPPER(CODES.CDISCREP)=PADR(UPPER(lcDesc),30)
  llRet = .T.
  &lcVarName. = CODES.CCODE_NO
ELSE
  IF SEEK('N'+lcFldNam,'CODES')
    SELECT CODES
    LOCATE REST WHILE cDefCode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'N'+lcFldNam;
                FOR UPPER(CDISCREP) = PADR(UPPER(lcDesc),30)
    IF FOUND()
      llRet = .T.
      &lcVarName. = CCODE_NO
    ENDIF
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN llRet
*--end of lfGetCode.

*!************************************************************************
*! Name      : lfGetGrade
*! Developer : AHMED MAHER
*! Date      : 02/26/2002
*! Purpose   : Function to get Grade
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcQuali = Style Quality
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfGetGrade
PARAMETERS lcQuali

PRIVATE lcGrade
DO CASE
  CASE ALLTRIM(UPPER(lcQuali)) = 'FIRST'
    lcGrade = '1'
  CASE ALLTRIM(UPPER(lcQuali)) = 'SECOND'
    lcGrade = '2'
  CASE ALLTRIM(UPPER(lcQuali)) = 'DAMAGED'
    lcGrade = '3'
  OTHERWISE
    lcGrade = '4'
ENDCASE
RETURN lcGrade
*--end of lfGetCode.