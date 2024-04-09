*!********************************************************************
*: Program file  : ALAUPC.PRG (REFERE TO C102272)
*: Program desc. : Automatic packing list For Cathy danials
*: System        : ARIA 2.7
*: Module        : AL
*: Developer     : MOHAMED SHOKRY (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Modifications:
*:C102272,5 MHM 25/06/2001  Handel Case 3 and case 6 send by mail To AL module
*:B804274,1 MHM 02/07/2001  Fix some bugs 
*:B804334,1 MHM 31/07/2001  Fix some bugs to work with sadimara
*:B604765,1 MHM 02/08/2001  Fix th bug of carton Number in detail folder not 
*:B604765,1                 calculated right in case of Non prepack sadimara
*:B804346,1 SSH 09/Aug/2001 Fix the following:
*:B804346,1 SSH         1- Print -ve qty in the detail folder (Sadmara Case).
*:B804346,1 SSH         2- Numeric over flow (Sadmara Case).
*:B804346,1 SSH         3- Print -ve qty in the detail folder (Cathy Case).
*:B804367,1 ABD 08/15/2001 Fix bug "Alias not Order" when saving at customer Stunt .
*:B804367,4 SSH 08/16/2001 Fix the bug that the Use incorect module global variable.
*:B604842,1 SSH 20/08/2001 Inquire the user about the BOL on Packing Slip level
*:B604842,1 SSH            Not on style or carton level.
*:C200218,1 BWA 30/08/2001 Modify the custom invoice for sademara.
*:B606159,1 WAEL30/06/2002 Some bugs related to updating EDI files.
*:B120922,1 MHM 12/10/2003 Fix bug of variable 'lcMask' not found.
*:***************************************************************************
STORE SPACE(0) TO lcWReject,lcWReject1,lcWReject2,lcRejTtl,;
                  lcWPotPak,lcWPotPak1,lcWPotPak2,lcHdrTtl,;
                  lcWPakDtl,lcWPakDtl1,lcWPakDtl2,lcDtlTtl

*B120922,1 MHM 12/10/2003 (Begin) Fix bug of variable 'lcMask' not found.
STORE '' TO lcMask
*B120922,1 MHM 12/10/2003 (End)

PRIVATE llPikTktOp,llOrdLinOp,llStyleOp,llStyDyOp,llScaleOp,llOrdHdrOp,llCustmOp,;
        llPakHdrOp,llPakLinOp
STORE .F. To llPikTktOp,llOrdLinOp,llStyleOp,llStyDyOp,llScaleOp,llOrdHdrOp,llCustmOp,;
             llPakHdrOp,llPakLinOp,llBrowse ,llreturn

laDefproc[09] = .F.
*--- SAD
PRIVATE lnCrtWght , lnWgInCrt
lnCrtWght = 0
lnWgInCrt = 0
*--- SAD
STORE .F. TO llNoShow,llPackChng

PRIVATE lcDelPik,lcPikTkt,lcRejPkTmp,lcPakHdr,lcPakLin,lnActFolder 

STORE SPACE(6) TO lcDelPik,lcPikTkt,lcPackNo
STORE SPACE(0) TO lcRejPkTmp,lcPakHdr,lcPakLin,lcStyTtl,lcfolder,lcfoldprnt


STORE 0 To lnRejBrRec,lnHdrBrRec,lnDtlBrRec,lnOldCrt,lnOldQty
lnActFolder = 1

PRIVATE lcStyStat,lcQtyStat,lcCrtStat,lcPakStat,lcOrdStat,lcAccStat,lcStorStat
STORE "DISABLE" TO lcStyStat,lcQtyStat,lcCrtStat,lcPakStat,lcOrdStat,lcAccStat,lcStorStat

llDyelot =gfGetMemVar('M_DYELOT',gcAct_Comp) = 'Y'

PRIVATE lcExpr
lcExpr = ''

PRIVATE m.Style,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
        m.cnt,m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8,;
        lcStyPic,lcStyTtl,lnStyleWid, lcBol , llBOLL_Opn ,ShipVia ,lcBolStuts ,llBolnever        

*B804367,1 ABD This flage add to hold true or false in case BOL_Hdr File opened before 
*B804367,1 ABD that or still close.  [Begin]
PRIVATE llBOLH_Opn
*B804367,1 ABD [End]

lcBol = ""
lcOldValue = ""
ShipVia = ""

*B804367,1 ABD This flage add to hold true or false in case BOL_Hdr File opened before 
*B804367,1 ABD that or still close and Assign false to this variable.  [Begin]
*STORE .F. TO llBOLL_Opn
STORE .F. TO llBOLL_Opn , llBOLH_Opn
*B804367,1 ABD [End]

STORE .F. TO llBOLL_Opn
*--Define variables for EDI files and break weight
STORE 0 TO  lnRpWght
STORE '' TO lcCtnHdr,lcCtnDtl,lcScaFile,lcCartonSz ,lcPrePak
*mhm 804334,1
STORE 0 TO lnOldRem

*mhm 804334,1

*B606159,1 WAEL 30/06/2002 (Begin) Define array to hold BOL Info.
DECLARE laBOLInfo[1,2]
laBOLInfo=''
STORE .F. TO llBolAsi,llUseExs,llUsePre
*B606159,1 WAEL 30/06/2002 (End)

STORE SPACE(0) TO m.Style,m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8,;
                  lcStyPic,lcStyTtl

STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,;
           m.cnt,lnStyleWid ,lnNonMajSt ,lcFree_Clr
STORE 0  TO lnDumDtlR, lnDumCtnR, lnNonMajSt, lnMajLen , lnColorLen 
STORE SPACE(0) TO lcStore 

*B606159,1 WAEL 30/06/2002 (Begin)
*-- New variables For print setup
llScrPrnLb = .T.
*B606159,1 WAEL 30/06/2002 (End)

lcSndPort = "COM2"
*-- New variables
DIMENSION laMajSegs[1,1]
llCanPrnLb = gfUserPriv('AL','ALAUPC','PRNPACKING')

*--llCheck2 : variable to check for update with changed prepack 
STORE .F. TO llCheck2 ,llChngPrpk


*-- lnDrctTo    variable that showes if the paking is directed to store or consolidator
STORE 1 TO lnDrctTo
DIMENSION laSetup[1,2]
laSetUp[1,1] = 'XMANUFID'
=gfGetMemVar(@laSetUp,gcAct_Comp)
lcManufId  = ALLTRIM(laSetUp[1,2])
           
DECLARE laDRltFld[1,2]
laDRltFld[1,1] = 'DIVLNAME  '
laDRltFld[1,2] = 'lcDivLName'
lcDivLName = ''

*-- Add variable  llEdiSys to check if there is  AS module or not [Begin]
*-- llEdiSys   variable that showes if there is AS module or not

*:B804367,4 SSH 08/16/2001 Use gcComp_Mdl instead of gcCmpModules
*:B804367,4 SSH since that gcComp_Mdl indicate all installed module while
*:B804367,4 SSH gcCmpModules indicate setups one only.
*llEdiSys   =  ('AS' $ gcCmpModules)
llEdiSys   =   ('AS' $ gcComp_Mdl) .OR. ('AS' $ gcCmpModules)
*:B804367,4 SSH 08/16/2001 Fix the bug that the Use incorect module global variable.

*--Get the setting of BOL setup and Add flag to hold the valu of the setup (M_BOLSTUTS) [Begin]
lcBolStuts = gfGetMemVar('M_BOLSTUTS',gcAct_Comp)
llBolnever = (ALLTRIM(lcBolStuts) = "N")

*:B604765,1 MHM 04/08/2001 Variable must be before Gfsetup to retrive[Start]
STORE SPACE(0) TO lcStyPic,lcStyTtl 
STORE 0 TO lnStyleWid
*:B604765,1 MHM 04/08/2001 [end]

IF !gfSetup()
  RETURN
ENDIF  

*:B604765,1 MHM 04/08/2001 [Start]
*STORE SPACE(0) TO lcStyPic,lcStyTtl 
*STORE 0 TO lnStyleWid
*:B604765,1 MHM 04/08/2001 [end]


IF !WEXIST(gcBaseWind)
  = lfOpenFils()
  SELECT OrdLine
  SCATTER FIELDS Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty MEMVAR BLANK
  SELECT Scale
  SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR BLANK

  lcStyPic   = gfItemMask("PI")
  lcStyTtl   = gfItemMask("HI")
  lnStyleWid = LEN(lcStyPic)  

  SELECT Pack_Hdr
  lcWareCode = Pack_Hdr.cWareCode 

  lcRejTtl = "Rejected Picking Tickets"
  lcHdrTtl = "Packing Lists"
  lcDtlTtl = "Pack Detail"  
  lcWReject  = gfTempName()
  lcWReject1 = gfTempName()
  lcWReject2 = gfTempName()
  lcWPotPak  = gfTempName()
  lcWPotPak1 = gfTempName()
  lcWPotPak2 = gfTempName()
  lcWPakDtl  = gfTempName()
  lcWPakDtl1 = gfTempName()
  lcWPakDtl2 = gfTempName()
  lcfolder   = gfTempName()
  lcfoldprnt = gcBaseWind

  lcAsnLabel = gfTempName()
  lcTmpDetFl = gftempName()
  lcTmAsnShp = gfTempName()
  lcCtnHdr   = gfTempName()
  lcCtnDtl   = gfTempName()
  lcScaFile  = gfTempName()
  lcCartonSz = gfTempName()
  CREATE TABLE (gcWorkDir + lcScaFile) (SzCode C(1))
  INDEX ON SzCode TAG (lcScaFile)
  FOR lnI = 1 TO 8
    INSERT INTO (lcScaFile) (SzCode) VALUES (STR(lnI,1))
  ENDFOR
  USE (gcWorkDir+lcScaFile) IN 0 AGAIN ALIAS (lcCartonSz) ORDER (lcScaFile)
  *-- calling function that caluculat style parametars [Begin]
  =lfEvalSegs()
  *-- Temp files needed to update ASN_SHIP file [Begin]
  = lfCrCtnFiles()
  SELECT Asn_Ship
  IF !USED(lcTmAsnShp)
    COPY STRUCTURE TO (gcWorkDir+lcTmAsnShp)
    =gfOpenFile(gcWorkDir+lcTmAsnShp,'','EX')
    INDEX ON STR(CART_NO,6) TAG (lcTmAsnShp)
  ENDIF 

ENDIF

IF llCanPrnLb
  * HS 05/06/2001 Add some new variables and open the Style UPC file if the
  *             UP Module is installed [Begin]
  *-- Flag to know if the detailed label exist or not
  llDetLabel = SEEK("XX1" + "H" , "SYCASNLB") .AND. SEEK("XX1" + "L" , "SYCASNLB")
  
  IF llDetLabel
    *-- Flag to know if UPC module is installed or not
    *:B804367,4 SSH 08/16/2001 Use gcComp_Mdl instead of gcCmpModules
    *:B804367,4 SSH since that gcComp_Mdl indicate all installed module while
    *:B804367,4 SSH gcCmpModules indicate setups one only.
    *llUPCInst  = ('UP' $ gcCmpModules)
    llUPCInst  = ('UP' $ gcComp_Mdl) .OR. ('UP' $ gcCmpModules)
    *:B804367,4 SSH [END]
    *-- Flag to know if the user want to print detailed shipping label for all cartons or not
    lcDetLbAll = ""
    
    IF llUPCInst
      =gfOpenFile(gcDataDir + 'STYLEUPC' , gcDataDir + 'STYLEUPC' , 'SH')
    ENDIF
  ENDIF
  lcMask=gfItemMask("PM")
  *HS 05/06/2001 Add some new variables and open... [End]
ENDIF

*--check for Extended Size Scale
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
lcSizeSep  = ''
STORE 0 TO lnSizePos,lnSizeLen
IF llExtSizSc
  DECLARE laStySeg[1,1]
  STORE "" TO laStySeg
  =gfItemMask(@laStySeg)
  FOR lnCnt = 1 TO ALEN(laStySeg,1)
    IF laStySeg[lnCnt , 1] = "S"
      lcSizeSep  = ALLTRIM(laStySeg[lnCnt-1 , 6])
      lnSizePos  = laStySeg[lnCnt , 4] - IIF(!EMPTY(lcSizeSep) , 1 , 0)
      lnSizeLen  = LEN(laStySeg[lnCnt , 3]) + IIF(!EMPTY(lcSizeSep) , 1 , 0)
    ENDIF
  ENDFOR
ENDIF

DIMENSION lafoldwinds[3,2]
lnFolderCEnd = 102.40
lnFolderREnd = 1.99
lnNoFld      = 3
lcwfoldchng  = '=lfActFolder()'

lafoldwinds[1,1] = 'Rejected Picking Tickets'
lafoldwinds[1,2] = lcWReject
lafoldwinds[2,1] = 'Packs Header'
lafoldwinds[2,2] = lcWPotPak
lafoldwinds[3,1] = 'Packs Detail'
lafoldwinds[3,2] = lcWPakDtl

DIMENSION laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbOptGrid'
laPanelObj[1,2] = gcBmpHome+'SCOPE.BMP'
laPanelObj[1,3] = [VALID lfOptGrid() MESSAGE 'Option Grid' ENABLE ]

PUSH KEY
= lfActPad()
DO (gcScrDir + gcWinAppl + '\ALAUTPCT.SPR')
POP KEY
=lfRELPad()
RELEASE WINDOW (lcRejTtl)
RELEASE WINDOW (lcHdrTtl)
RELEASE WINDOW (lcDtlTtl)

IF glQuitting
   =lfClseFils()
ENDIF


*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Control Show Procedure
*!*************************************************************
*! Calls     : lfCrtTmp,lfOpGrid,lfActFolder
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpShow
*!*************************************************************
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
    SHOW GET pbOptGrid ENABLE
    = lfCrtTmp()
    STORE .F. To llBrowse,llPackChng
    STORE 0 TO lnOldCrt,lnOldQty
    STORE "DISABLE" TO lcStyStat,lcQtyStat
    SELECT(lcPakLin)
    SCATTER MEMVAR BLANK
    = lfOptGrid()
  CASE laScrMode[4]
    SHOW GET pbOptGrid DISABLE
ENDCASE
= lfActFolder()

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Control Folder
*!*************************************************************
*! Calls     : lfwRejBrow,lfPackCrt,lfwHdrBrow,lfwDtlBrow
*!           : lfPackFltr
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActFolder()
*!*************************************************************

FUNCTION lfActFolder

DO CASE
  CASE lnActFolder = 1
    IF WEXIST(lcRejTtl)
      = lfwRejBrow()
    ELSE
      = lfRejBrow()
    ENDIF
    SHOW GET ibCtnRej ENABLE
    SHOW GETS WINDOW (lcWPotPak2) DISABLE ONLY
    SHOW GETS WINDOW (lcWPakDtl2) DISABLE ONLY

  CASE lnActFolder = 2
    IF llPackChng
      llPackChng = .F.
      = lfPackCrt()
    ENDIF
    IF WEXIST(lcHdrTtl)
      = lfwHdrBrow()
    ELSE
      = lfHdrBrow()
    ENDIF
    SHOW GET ibCtnPot ENABLE
    SHOW GETS WINDOW (lcWReject2) DISABLE ONLY
    SHOW GETS WINDOW (lcWPakDtl2) DISABLE ONLY

  CASE lnActFolder = 3

    = lfPackFltr()
    IF WEXIST(lcDtlTtl)
      = lfwDtlBrow()
    ELSE
      = lfDtlBrow()
    ENDIF
    SHOW GET ibCtnDtl ENABLE
    SHOW GETS WINDOW (lcWReject2) DISABLE ONLY
    SHOW GETS WINDOW (lcWPotPak2) DISABLE ONLY
ENDCASE

*!*************************************************************
*! Name      : lfPackCrt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPackCrt()
*!*************************************************************
*!
FUNCTION lfPackCrt
PRIVATE lnCurAlias,lnlinRec,lnCurAlias,lnFrom,lnTo,lnCrtCnt
STORE 0 To lnFrom,lnTo,lnCrtCnt
lnCurAlias = SELECT(0)

SELECT (lcPakLin)

lnlinRec = RECNO()
IF SEEK(&lcPakHdr..Pack_No)
  lnFrom = From_Crt
  lnTO   = To_Crt
  lnCrtCnt = lnTo - lnFrom + 1
  SCAN REST WHILE Pack_No+Style+STR(nOrdLineNo,6) = &lcPakHdr..Pack_No
    IF From_Crt # 0 AND To_Crt # 0
      FOR lnI = From_Crt TO To_Crt
        IF !BETWEEN(lnI,lnFrom,lnTo)
          lnCrtCnt = lnCrtCnt +  1
          lnFrom = MIN(lnFrom,lnI)
          lnTo   = MAX(lnTo,lnI)
        ENDIF
      ENDFOR
    ENDIF
  ENDSCAN
ENDIF  
SELECT (lcPakHdr)
REPLACE Tot_Cart WITH lnCrtCnt
IF lnLinRec <= RECCOUNT(lcPakLin)
  GOTO lnLinRec IN (lcPakLin)
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfClseFils
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Close opend files
*!*************************************************************
*! Calls     : gfCloseFile,
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClseFils()
*!*************************************************************
*!
FUNCTION lfClseFils

SELECT PikTkt
SET RELATION OFF INTO OrdLine

IF llScaleOp
  = gfCloseFile('Scale')
ENDIF
IF llCustmOp
  = gfCloseFile('Customer')
ENDIF
IF llPikTktOp
  = gfCloseFile('PikTkt')
ENDIF
IF llOrdHdrOp
  = gfCloseFile('OrdHdr')
ENDIF
IF llOrdLinOp
  = gfCloseFile('OrdLine')
ENDIF
IF llStyleOp
  = gfCloseFile('Style')
ENDIF
IF llStyDyOp
  = gfCloseFile('StyDye')
ENDIF
IF llPakHdrOp
  = gfCloseFile('Pack_Hdr')
ENDIF
IF llPakLinOp
  = gfCloseFile('Pack_Lin')
ENDIF

IF USED(lcRejPkTmp)
  USE IN (lcRejPkTmp)
ENDIF
ERASE (gcWorkDir+lcRejPkTmp+'.DBF')
ERASE (gcWorkDir+lcRejPkTmp+'.CDX')

IF USED(lcPakHdr)
  USE IN (lcPakHdr)
ENDIF  
ERASE (gcWorkDir+lcPakHdr+'.DBF')
ERASE (gcWorkDir+lcPakHdr+'.CDX')

IF USED(lcPakLin)
  USE IN (lcPakLin)
ENDIF
ERASE (gcWorkDir+lcPakLin+'.DBF')
ERASE (gcWorkDir+lcPakLin+'.CDX')

IF USED(lcCtnHdr)
  USE IN (lcCtnHdr)
ENDIF
ERASE (gcWorkDir+lcCtnHdr+".DBF")
ERASE (gcWorkDir+lcCtnHdr+".CDX")
IF USED(lcCtnDtl)
  USE IN (lcCtnDtl)
ENDIF
ERASE (gcWorkDir+lcCtnDtl+".DBF")
ERASE (gcWorkDir+lcCtnDtl+".CDX")

IF USED(lcCartonSz)
  USE IN (lcCartonSz)
ENDIF
ERASE (gcWorkDir+lcCartonSz+".DBF")
ERASE (gcWorkDir+lcCartonSz+".CDX")

IF USED(lcScaFile)
  USE IN (lcScaFile)
ENDIF
ERASE (gcWorkDir+lcScaFile+".DBF")
ERASE (gcWorkDir+lcScaFile+".CDX")

*B804367,1 ABD Close the Bol_lin & Bol _Hdr , in case the program open those files. [Begin]
IF llBOLL_Opn
  = gfCloseFile('Bol_Lin')
ENDIF
  
IF llBOLH_Opn 
  = gfCloseFile('BOL_HDR')
ENDIF
*B804367,1 ABD [End]

*!*************************************************************
*! Name      : lfOptGrid
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Call option Grid
*!*************************************************************
*! Calls     : gfOpGrid,lfAssgnBol ,gfUpdate,lfwRejBrow,lfwHdrBrow
*!           : lfPackFltr ,lfwDtlBrow,lfClcult
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOptGrid()
*!*************************************************************
*!
FUNCTION lfOptGrid
PRIVATE lcExpr,lcPikTag,lcEscTrap
STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt
*--variable to Store the FRom Carton Number

*:B604765,1 MHM 02/08/2001 initialize variables [start]
STORE 0 TO lnRemendr , lnOldRem ,lnPicQty
*:B604765,1 MHM  [end]

STORE 0 TO lnStorFrm
*:B604765,1 MHM 04/08/2001 define tow variables[start]
STORE 0 TO lnOldCrtNo , lnOldweght
*:B604765,1 MHM 04/08/2001 [end]
*--- SAD
lnWgInCrt = 0
lnCrtWght = 0
lcPikTkt  = SPACE(06)
lcEscTrap = ON("KEY","ESC")

*lnRpWght  ------------------->       variable to Initialize Breake Weight
*STORE 0 TO  lnRpWght

lcExpr = gfOpGrid('ALAUTPCT' , .T.)
ON KEY LABEL ESCAPE &lcEscTrap

IF !(TYPE('lcExpr') $ 'UL')
  SELECT PikTkt

  lcPikTag = ORDER()
  SET ORDER TO 0
  *:B604842,1 SSH [Start] Fix Incorrect relatoin.
  *SET RELATION TO 'M' + Account + Store INTO Customer ADDITIVE
  SET RELATION TO  IIF(EMPTY(Store),'M'+Account,"S"+Account+Store) INTO Customer ADDITIVE
  *:B604842,1 SSH [END]
  SET RELATION TO 'O' + Order INTO OrdLine ADDITIVE
  SET SKIP TO OrdLine
  * Don't allow creating packing list on completed or Hold pick ticket 
  SCAN FOR &lcExpr AND PikTkt = OrdLine.PikTkt AND Status $ 'OP'
    WAIT "Preparing Data. Picking ticket# " + PikTkt.PikTkt WINDOW NOWAIT
    IF lcPikTkt # OrdLine.PikTkt
      STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt 
      lcPikTkt = OrdLine.PikTkt
      *:B604765,1 MHM 02/08/2001 [Start]
      STORE 0 TO lnStorFrm ,lnOldCrtNo ,lnPicQty ,lnOldweght
      *B804334,1 [start]
      STORE 0 TO lnOldRem
      *B804334,1 [end]
      *:B604765,1 MHM 02/08/2001 [end]
      lnWgInCrt = 0
      lnCrtWght = 0
      *:B604842,1 SSH [Start] Save BOL on Packing Slip level.
      SELECT (lcPakHdr)
      IF !SEEK(OrdLine.PikTkt)
        APPEND BLANK
        REPLACE Pack_No  WITH OrdLine.PikTkt  ,;
                Order    WITH OrdLine.Order   ,;
                Store    WITH OrdLine.Store   ,;        
                Account  WITH OrdLine.Account ,;
                AccName  WITH IIF(Customer.Type="M",Customer.BtName,Customer.StName)
        *B606159,1 WAEL 30/06/2002 (Begin) Call function to assign BOL.
        =lfAssgnBol(OrdLine.cOrdType + OrdLine.Order , OrdLine.Account , OrdLine.Store)
        *B606159,1 WAEL 30/06/2002 (End)
      ENDIF
      *B606159,1 WAEL 30/06/2002 (Begin) Comment out to call in the if statment.
      *=lfAssgnBol(OrdLine.cOrdType + OrdLine.Order , OrdLine.Account , OrdLine.Store)
      *B606159,1 WAEL 30/06/2002 (End)
      SELECT PikTkt
      *:B604842,1 SSH [END]
    ENDIF
    IF SEEK(OrdLine.Style,'Style') .AND. SEEK('P'+Style.Scale+Style.PrePak,'Scale')
      SELECT Scale
      IF Scale.PPTot <> 0 

        *:B804346,1 SSH 09/Aug/2001 Add the condition that the style 
        *:B804346,1 SSH weight<Carton break weight and this pick tickit is
        *:B804346,1 SSH not rejected.
        *IF Style.nstyweight <> 0
        IF Style.nstyweight <> 0  .AND. Style.nstyweight <= lnRpWght  .AND. !SEEK(lcPikTkt,lcRejPkTmp)
        *:B804346,1 SSH [END]

          lcPreWght = Scale.PPTot * Style.nstyweight
          IF  OrdLine.PikTkt <> lcDelPik AND lnRpWght <> 0 ;
              AND lnRpWght > Style.nstyweight AND lnRpWght >= lcPreWght
            
              =lfPreDist()
            lcPreWght = Scale.PPTot * Style.nstyweight
            *--- lnRpWght: Carton Wieght.
            *--- lcPreWght: PrePack Wieght.
            *--- lnCrtNo: TotNo of PrePack per carton.
            IF lcPreWght = 0
              llChngPrpk = .T.
            ENDIF

            IF lcPreWght = 0
              lcPreWght = Style.nstyweight 
              *--falg to case 3 and 6
              llflag200 = .T.
            ELSE
              lcPreWght = lcPreWght
            ENDIF

            *:B804346,1 SSH Check if the totpick weight < Break weight
            *:B804346,1 SSH calculate carton number depend on the totpick
            *:B804346,1 SSH else calculate depend on the break weight.
            
            IF Ordline.TotPik * Style.nstyweight < lnRpWght
              lnCrtNo = INT(Ordline.TotPik * Style.nstyweight / lcPreWght)
            ELSE
              lnCrtNo = INT(lnRpWght / lcPreWght)
            ENDIF
            *:B804346,1 SSH [END]
            
            *:B804274,1 MHM If the carton no = 0 then change to case 3 or case 6
            IF lnCrtNo = 0
              llChngPrpk = .T.
              *lnCrtNo =  INT(lnRpWght /Style.nstyweight)
            ENDIF
            *:B804274,1 MHM[END]
            *--- lnRealWght : (No. of prepack per carton* PrePack Weight)
            *:B804346,1 SSH 09/Aug/2001 Check if the carton=0 replace it with 1
            *lnRealWght = lnCrtNo * lcPreWght
            lnRealWght = IIF(lnCrtNo>0,lnCrtNo,1) * lcPreWght
            *:B804346,1 SSH .
            *--- lnNoCrt : Tot No of Carton = (Total style weight / Weight per carton)
            lnNoCrt = CEILING((OrdLine.TotPik * Style.nstyweight) / lnRealWght)
          
            lnFrmCrt = lnToCrt  + 1
            lnToCrt  = lnFrmCrt + lnNoCrt -  1

            *-- Function to calculate cartons
            *B606159,1 ASH 06/30/2002 (Begin) Add Option to pack with Prepack or without Prepack in the option grid.
            *llReturn=lfClcult(lnNoCrt,lnFrmCrt,lnToCrt)
            llReturn=llUsePre AND lfClcult(lnNoCrt,lnFrmCrt,lnToCrt)
            *B606159,1 ASH 06/30/2002 (End)
            IF llreturn
              SELECT (lcPakHdr)
              IF !SEEK(OrdLine.PikTkt)
                APPEND BLANK
                REPLACE Pack_No  WITH OrdLine.PikTkt  ,;
                        Order    WITH OrdLine.Order   ,;
                        Store    WITH OrdLine.Store   ,;        
                        Account  WITH OrdLine.Account ,;
                        AccName  WITH Customer.BtName        
              ENDIF
              REPLACE  Tot_Cart WITH Tot_Cart + lnNoCrt                    ,;
                       Tot_Pcs  WITH Tot_Pcs  + OrdLine.TotPik ,;
                       Tot_Wght WITH Tot_Wght + (OrdLine.TotPik * Style.nStyWeight);
                       Pick_Qty WITH  OrdLine.TotPik;
                       Order_Qty WITH OrdLine.TotQTY
                   
              *-Assign BOL for the packing list if there is EB or AS module 
              *:B604842,1 SSH [Start] Commented out to create BOL on packing 
              *:B604842,1 SSH slip level not style or carton level.
              *=lfAssgnBol(OrdLine.cOrdType + OrdLine.Order , Account , Store)          
              *:B604842,1 SSH [END]
            ELSE
              *C102272,5 MHM 25/06/2001  [Start]
              *lcReason = "There is no suitable prepack found. "
              *=lfUpRej()
              *:B804346,1 SSH Do not Print -ve qty in the detail folder.
              *-- We will use funcion "lfPakbySz"
              *-- Instead of use function "lfSpclCase"
              *=lfSpclCase(lnNoCrt ,lnFrmCrt , lnToCrt)
              IF Style.nstyweight > lnRpWght
                lcReason = "Style Weight is greater than break weight. "
                =lfUpRej()
              ELSE
                =lfPakbySz()
                =lfUpdHdr()
              ENDIF
              *:B804346,1 SSH (END).
              *--C102272,5 MHM 25/06/2001  [End].
            ENDIF  
          ELSE
            *lcReason = "Break weight less than prepack weight. "
            *=lfUpRej()
            *:B804346,1 SSH Do not Print -ve qty in the detail folder.
            *-- We will use funcion "lfPakbySz"
            *-- Instead of use function "lfSpclCase"
            *=lfSpclCase(lnNoCrt ,lnFrmCrt , lnToCrt)
            IF Style.nstyweight > lnRpWght
              lcReason = "Style Weight is greater than break weight. "
              =lfUpRej()
            ELSE
              =lfPakbySz()
              =lfUpdHdr()
            ENDIF
            *:B804346,1 SSH [END].
          ENDIF
        ELSE
          *--- Call function to add to reject browse.
          IF Style.nstyweight > lnRpWght
            lcReason = "Style Weight is greater than break weight. "
          ELSE
            lcReason = "There is no style weight."
          ENDIF
          =lfUpRej()
        ENDIF  
      ELSE
        *--- Call function to add to reject browse.
        lcReason = "There is no style prepack."
        *=lfUpRej()
        *:B804346,1 SSH Do not Print -ve qty in the detail folder.
        *-- We will use funcion "lfPakbySz"
        *-- Instead of use function "lfSpclCase"
        *=lfSpclCase(lnNoCrt ,lnFrmCrt , lnToCrt)
        *=lfPoakbySz(lnNoCrt ,lnFrmCrt , lnToCrt)
        IF Style.nstyweight > lnRpWght
          lcReason = "Style Weight is greater than break weight. "
          =lfUpRej()
        ELSE
          =lfPakbySz()
          =lfUpdHdr()
        ENDIF
        *:B804346,1 SSH (END)
      ENDIF
    ELSE
      IF !SEEK('P'+Style.Scale+Style.PrePak,'Scale')
        *--- Call function to add to reject browse.
        *=gfModalGen("TRM000000B00000","DIALOG",'','','There is no PrePack.')
        *--C102272,5 MHM 25/06/2001 [Start] 
        *lcReason = "There is no style prepack."
        *=lfUpRej()
        *:B804346,1 SSH Do not Print -ve qty in the detail folder.
        *-- We will use funcion "lfPakbySz"
        *-- Instead of use function "lfSpclCase"
        *=lfSpclCase(lnNoCrt ,lnFrmCrt , lnToCrt)
        lnCrtWght = IIF(lnCrtWght = 0 ,lnRpWght ,lnCrtWght)
        IF SEEK('S'+Style.Scale,'Scale')
          IF Style.nstyweight > lnRpWght
            lcReason = "Style Weight is greater than break weight. "
            =lfUpRej()
          ELSE
            =lfPakbySz()
            =lfUpdHdr()
          ENDIF
        ENDIF      
        *:B804346,1 SSH 
        *--C102272,5 MHM 25/06/2001  [End]
      ENDIF
    ENDIF
  ENDSCAN
  WAIT CLEAR
  SELECT PikTkt
  SET RELATION OFF INTO OrdLine
  SET RELATION OFF INTO Customer
  SET ORDER TO (lcPikTag) IN PikTkt

  = gfUpdate()
  STORE .F. TO laScrMode
  laScrMode[4] = .T.
  SHOW GETS
  
  SELECT (lcRejPkTmp)
  GO TOP
  IF lnActFolder = 1
    = lfwRejBrow()
  ENDIF
  SELECT (lcPakHdr)
  GO TOP
  IF lnActFolder = 2
    = lfwHdrBrow()
  ENDIF
  IF lnActFolder = 3
    = lfPackFltr()
    = lfwDtlBrow()
  ENDIF

ENDIF

laCtrStat[7]  = "ENABLED"

*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Trap Keys
*!*************************************************************
*! Calls     : lfRelPad
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTrap()
*!*************************************************************
*!
FUNCTION lfTrap

IF !INLIST(WONTOP(),gcBaseWind,lcRejTtl,lcHdrTtl,lcDtlTtl)
  =lfRelPad()
ENDIF

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF INLIST(WONTOP(),lcRejTtl,lcHdrTtl,lcDtlTtl)
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
ELSE
  glFromBrow = .F.
ENDIF

*!*************************************************************
*! Name      : lfBrTab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Trap Tab Key
*!*************************************************************
*! Calls     : lfRelPad
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrTab()
*!*************************************************************
*!
FUNCTION lfBrTab

ON KEY LABEL TAB
DO CASE
  CASE lnActFolder = 1 AND WONTOP() = lcRejTtl
    ACTIVATE WINDOW ('gwcContrl1')
    _CUROBJ = OBJNUM(pbTop)
  CASE lnActFolder = 2 AND WONTOP() = lcHdrTtl
    ACTIVATE WINDOW (lcWPotPak2)
    _CUROBJ = OBJNUM(m.Pack_No)
  CASE lnActFolder = 3 AND WONTOP() = lcDtlTtl
    ACTIVATE WINDOW (lcWPakDtl2)
    _CUROBJ = OBJNUM(m.From_Crt)
ENDCASE    

*!*************************************************************
*! Name      : lfBrBack
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Trap BACKTAB Key
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrBack()
*!*************************************************************
*!
FUNCTION lfBrBack

ON KEY LABEL BACKTAB
DO CASE
  CASE lnActFolder = 1 AND WONTOP() = lcRejTtl
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[1])
  CASE lnActFolder = 2 AND WONTOP() = lcHdrTtl
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[2])
  CASE lnActFolder = 3 AND WONTOP() = lcDtlTtl
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[3])
ENDCASE    

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Clear Trap Keys
*!*************************************************************
*! Calls     : lfActPad ,gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrTrap()
*!*************************************************************
*!
FUNCTION lfClrTrap

= lfActPad()

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfRejBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfActPad ,gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRejBrow()
*!*************************************************************
*!
FUNCTION lfRejBrow

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT(lcRejPkTmp)
lnRejBrRec = RECNO()

BROWSE FIELDS cMarker =IIF(RECNO()=lnRejBrRec,'>',' '):H= ' ':R:1:W=.F.,;
              PikTkt :H="PikTkt"  :R :7  ,;
              PikDate:H="Date"    :R :10 ,;
              Order  :H="Order"   :R :7  ,;
              Account:H="Account" :R :9  ,;
              Store  :H="Store"   :R :8  ,;
              AccName:H="Name"    :R :30 ,;
              cReason:H="Reason"  :R :50  ;
         SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
         TITLE(lcRejTtl) WHEN lfwRejBrow() ;
         VALID :F lfvRejBrow()             ;         
         WINDOW (lcWReject1) IN WINDOW (lcWReject)

=lfwRejBrow()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwRejBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRejBrow()
*!*************************************************************
*!
FUNCTION lfwRejBrow
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

SELECT(lcRejPkTmp)

lnRejBrRec = RECNO(lcRejPkTmp)

SHOW WINDOW (lcRejTtl) REFRESH SAME

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfwRejBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRejBrow()
*!*************************************************************
*!
FUNCTION lfvRejBrow

IF WONTOP() # (lcRejTtl)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfHdrBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwHdrBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfHdrBrow()
*!*************************************************************
*!
FUNCTION lfHdrBrow

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT(lcPakHdr)
lnHdrBrRec = RECNO()
 
*BROWSE FIELDS cMarker =IIF(RECNO()=lnHdrBrRec,'>',' '):H= ' ':R:1:W=.F.,;
              Pack_No :H="Pack#"   :R :8  ,;
              Order   :H="Order"   :R :8  ,;
              Account :H="Account" :R :9  ,;
              Store   :H="Store"   :R :8  ,;
              AccName :H="Name"    :R :30 ,;
              Tot_Cart:H="Cartons" :R :8  ,;
              Tot_Pcs :H="Pieces"  :R :8  ,;
              Tot_wght:H="Weight"  :R :13  ;
              SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
              TITLE(lcHdrTtl) WHEN lfwHdrBrow() ;
              VALID :F lfvHdrBrow()             ;         
              WINDOW (lcWPotPak1) IN WINDOW (lcWPotPak)

BROWSE FIELDS cMarker =IIF(RECNO()=lnHdrBrRec,'>',' '):H= ' ':R:1:W=.F.,;
              Pack_No :H="Pack#"   :R :8  ,;
              Order   :H="Order"   :R :8  ,;
              Account :H="Account" :R :9  ,;
              Store   :H="Store"   :R :8  ,;
              AccName :H="Name"    :R :30 ,;
              Tot_Cart:H="Cartons" :R :8  ,;
              Tot_Pcs   :H="Total Packed"   :R :8 ,;
              Pick_Qty  :H="Total Picked" :R :8 ,;
              Order_Qty :H="Total Ordered":R :8 ,;
              Tot_wght:H="Weight"  :R :13  ;
              SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
              TITLE(lcHdrTtl) WHEN lfwHdrBrow() ;
              VALID :F lfvHdrBrow()             ;         
              WINDOW (lcWPotPak1) IN WINDOW (lcWPotPak)


=lfwHdrBrow()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwHdrBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfPackFltr
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwHdrBrow()
*!*************************************************************
*!

FUNCTION lfwHdrBrow

PRIVATE lnCurAlias,lcPakStat,lcOrdStat,lcAccStat,lcStorStat,lcNewStat,lcRemStat
lnCurAlias = SELECT(0)
lnHdrBrRec = RECNO(lcPakHdr)

SHOW WINDOW (lcHdrTtl) REFRESH SAME
IF laScrMode[1]
  STORE 'DISABLE' TO lcPakStat,lcOrdStat,lcAccStat,lcStorStat,lcNewStat,lcRemStat
  SELECT (lcPakHdr)
  SCATTER MEMVAR BLANK
ELSE
  SELECT(lcPakHdr)
  SCATTER MEMVAR
  STORE 'ENABLE' TO lcNewStat
  IF EOF(lcPakHdr)
    STORE 'DISABLE' TO lcPakStat,lcOrdStat,lcAccStat,lcStorStat,lcRemStat
  ELSE
    STORE 'ENABLE' TO lcRemStat
    IF EMPTY(m.Pack_No)
      STORE 'ENABLE' TO lcPakStat,lcOrdStat,lcAccStat
      IF EMPTY(m.Account)
        lcStorStat = 'DISABLE'
      ELSE
        lcStorStat = 'ENABLE'
      ENDIF
    ELSE
      STORE 'DISABLE' TO lcPakStat,lcOrdStat,lcAccStat,lcStorStat
    ENDIF
  ENDIF
ENDIF

= lfPackFltr()

SHOW GET m.Pack_No &lcPakStat
SHOW GET pbPack    &lcPakStat
SHOW GET m.Order   &lcOrdStat
SHOW GET pbOrder   &lcOrdStat
SHOW GET m.Account &lcAccStat
SHOW GET pbAcc     &lcAccStat
SHOW GET m.Store   &lcStorStat
SHOW GET pbStore   &lcStorStat
SHOW GET pbNewPak  &lcNewStat
SHOW GET pbRemPak  &lcRemStat

*--Assign BOL for the packing list if there is EB or AS module and
*--control the opject lcBol in the header folder
lcBol = &lcPakHdr..Bill_Ladg
*--Enable the B.O.L field in all cases but if temp Pack header file is empty 
IF RECCOUNT(lcPakHdr) = 0
  SHOW GET lcBol DISABLE
ELSE
  SHOW GET lcBol ENABLE
ENDIF
*:B604842,1 SSH [Start] Refresh SAY fields.
=lfRefresh()
*:B604842,1 SSH [END]
SELECT (lnCurAlias)
*!*************************************************************
*! Name      : lfvHdrBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvHdrBrow()
*!*************************************************************
*!
FUNCTION lfvHdrBrow

IF WONTOP() # (lcHdrTtl)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfvPackNo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfModalGen ,lfGetBOL,lfwHdrBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPackNo()
*!*************************************************************
*!
FUNCTION lfvPackNo

IF MDOWN()
  RETURN
ENDIF
  
PRIVATE lnCurAlias,lcPikTag

lnCurAlias = SELECT(0)

lcPikTag = ORDER("PikTkt")
lcUseTag = IIF(EMPTY(m.Order),'PikTkt','OrdPik')
SET ORDER TO &lcUseTag. IN PikTkt

IF llBrowse OR (!EMPTY(m.Pack_No) AND ;
                !SEEK(IIF(EMPTY(m.Order),m.Pack_No,m.Order+m.Pack_No),'PikTkt'))
  llBrowse = .F.
  m.Pack_No = IIF(lfPackBrow(),PikTkt.PikTkt,SPACE(6))
ENDIF  
IF !EMPTY(m.Pack_No)
  IF PikTkt.Status = 'X'
    m.Pack_No = SPACE(6)
    *-- This Picking ticket is released. Cannot proceed.
    *-- OK
    = gfModalGen("INM44083B00000","Dialog")
  ELSE
    m.Account = PikTkt.Account
    m.Order   = PikTkt.Order
    m.Store   = Piktkt.Store
    
    *-- Create BOL# [begin]
    *-- IF there is EB or AS module 
    IF llEdiSys
      *-- Create BOL# 
      PRIVATE laFields      && array to hold defulted fields for BOL

      DIMENSION laFields[3,2]
      laFields[1,1] = "cgronhang"
      laFields[1,2] = "N"
      laFields[2,1] = "ctranmthd"
      laFields[2,2] = "M"
      laFields[3,1] = "packtype"
      laFields[3,2] = "CTN25"
   
      IF SEEK("O"+ m.Order , "ORDHDR")
        lcBol = lfGetBOL("",m.Account,m.Store,PikTkt.cWareCode,OrdHdr.ShipVia,'N',"laFields")
      ENDIF  
      REPLACE &lcPakHdr..Bill_ladg  WITH lcBol
    ENDIF  &&End of IF there is EB or AS module 
       
    SELECT (lcPakHdr)
    GATHER MEMVAR
    GO TOP
    =lfwHdrBrow()
  ENDIF
ENDIF

SET ORDER TO lcPikTag IN PikTkt

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfModalGen,gfBrows
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPackBrow()
*!*************************************************************
*!
FUNCTION lfPackBrow

PRIVATE lcBrFields,lcFile_Ttl,laBrow,lcFields,lnCurAlias,lcCurTag,llReturn,;
        laTemp,llNoRec,lcKey,lcFor,lcPikTag,lcOrdTag,lnCurAlias,lcBrowExp,lcAccount

lnCurAlias = SELECT(0)

STORE .F. TO llReturn,llNoRec
        
DIMENSION laBrow[7],laTemp[1]
STORE SPACE(0) TO laBrow,laTemp,lcKey,lcFor

lnCurAlias = SELECT(0)

lcBrFields  = [PikTkt.PikTkt:H='PikTkt'             ,]+;
              [PikTkt.Order :H='Order#'             ,]+;
              [PikTkt.Store :H='Store'              ,]+;
              [PikStat=lfPikStat() :H='Status'      ,]+;
              [Complete=OrdHdr.Complete:H='Complete',]+;
              [PikTkt.Date:H='Picking Date'         ,]+;
              [PikTkt.Account:H='Account'            ]
lcFile_Ttl = 'Pick Tickets'

lcPikTag = ORDER('PikTkt')
lcOrdTag = ORDER('OrdHdr')

IF !EMPTY(m.Order) OR EMPTY(m.Account) OR !EMPTY(m.Store)
  SELECT PikTkt
  IF EMPTY(m.Order) OR EMPTY(m.Store)
    IF EMPTY(m.Order)
      *--This means that the account and order both are empty and
      *--we will browse all piktkt
      lcKey = ''
      lcFor = ''
      IF !SEEK(lcKey,'PikTkt')
        llNoRec = .T.
        *-- There are no records to browse.
        *-- OK
        = gfModalGen("INM44032B00000","Dialog")
      ENDIF
    ELSE
      *--This means that the account and order both are empty and
      *--we will browse all piktkt
      lcKey = ''
      lcFor = ' FOR Store = m.Store'
      SELECT PikTkt
      LOCATE &lcFor
      IF !FOUND()
        llNoRec = .T.
        *-- There are no records to browse.
        *-- OK
        = gfModalGen("INM44032B00000","Dialog")
      ENDIF
    ENDIF
  ELSE
    *--we will browse piktkts for this order
    lcKey = m.Order
    lcFor = ''
    IF !SEEK(m.Order,'PikTkt')
      llNoRec = .T.
      *-- 'No piktkts found for order m.Order
      *-- <OK>
      llNoThing = gfModalGen("INM44028B00000","Dialog",m.Order)  
    ENDIF
  ENDIF
ELSE
  *--This means that the Order is empty and aacount is not
  *--and we will browse piktkt for this account
  SET ORDER TO OrdPik IN PikTkt
  SET ORDER TO OrdAcct IN OrdHdr
  SELECT OrdHdr
  SET RELATION TO Order INTO PikTkt
  SET SKIP TO PikTkt
  IF SEEK(m.Account,'OrdHdr')
    lcAccount = m.Account
    lcKey = ''
    lcFor = " FOR Account+cOrdType+Order = lcAccount + 'O' + PikTkt.Order"
    LOCATE REST &lcFor
    IF EOF()
      llNoRec = .T.
      *-- 'No piktkts found for account m.Account
      *-- <OK>
      llNoThing = gfModalGen("INM44043B00000","Dialog",m.Account)
    ENDIF
  ELSE
    llNoRec = .T.
    *-- 'No piktkts found for account m.Account
    *-- <OK>
    llNoThing = gfModalGen("INM44043B00000","Dialog",m.Account)
  ENDIF
ENDIF

IF !llNoRec
  lcBrowExp = lcKey + lcFor
  llReturn = gfBrows(lcBrowExp,'Order','laTemp')
ENDIF  
  
SELECT OrdHdr
SET ORDER TO lcOrdTag
SET RELATION TO

SET ORDER TO lcPikTag IN PikTkt

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfPikStat
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPikStat()
*!*************************************************************
*!
FUNCTION lfPikStat
PRIVATE lcRet

DO CASE
  CASE PikTkt.Status = 'X'
    lcRet = 'Released'
  CASE PikTkt.Status = 'H'
    lcRet = 'On Hold'
  CASE PikTkt.Status = 'O'
    lcRet = 'Open'
  CASE PikTkt.Status = 'P'
    lcRet = 'Pulled'
  OTHERWISE
    lcRet = PikTKt.Status
ENDCASE

RETURN  lcRet

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwHdrBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOrder()
*!*************************************************************
*!
FUNCTION lfvOrder

PRIVATE lnCurAlias,lcPikTkt,llFound,llValidOrd
lnCurAlias = SELECT(0)

IF llBrowse OR (!EMPTY(m.Order) AND !SEEK(m.Order,'OrdHdr') )
  llBrowse = .F.
  lcOrder = m.Order
  llValidOrd =lfOrdBrow(@lcOrder,m.Account)
  IF llValidOrd
    m.Order = OrdHdr.Order
  ELSE
    m.Order = SPACE(6)
  ENDIF
ENDIF  

IF !EMPTY(m.Order)
  SELECT PikTkt
  IF SEEK(m.Order,'PikTkt')
    lcPikTkt = PikTkt
    LOCATE REST WHILE Order+PikTkt = m.Order ;
                FOR   PikTkt # lcPikTkt
    llFound = FOUND('PikTkt')
    = SEEK(m.Order,'PikTkt')
    IF llFound
      m.Account = PikTkt.Account
      IF OrdHdr.Multi = 'N'
        m.Store = PikTkt.Store
      ENDIF
    ELSE
      m.Pack_No = PikTkt.PikTkt
      m.Account = PikTkt.Account
      m.Store   = PikTKt.Store
    ENDIF
  ELSE
    *-- 'No piktkts found for order m.Order
    *-- <OK>
    llNoThing = gfModalGen("INM44028B00000","Dialog",m.Order)
    m.Order = SPACE(6)
  ENDIF
ENDIF

SELECT (lcPakHdr)
GATHER MEMVAR
=lfwHdrBrow()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : browse all pack list or validated by the account
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : AriaBrow,gfModalGen
*!*************************************************************
*! Passed Parameters  : lcOrder,lcAccount
*!*************************************************************
*! Returns            : llReturn
*!*************************************************************
*! Example   : = lfOrdBrow()
*!*************************************************************
*!
FUNCTION lfOrdBrow

PARAMETERS lcOrder,lcAccount

*-- lcFields   variable that hold the name of browsed fields
*-- laBrow     array that hold the returned values from AriaBrow function
*-- lnCurAlias variable that hold the current alias
*-- lcCurTag   variable that hold the currend tag name
*-- llReturn   variable which is returned by this function 
*-- lcTag      variable that hold the name of the tag which is desired to switch 
*              file order to it

PRIVATE lcFields,laBrowArr,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,laTemp,;
        lcKey,lcFor,lcStore
DIMENSION laBrowArr[1],laTemp[1]
STORE SPACE(0) TO lcFields,laBrowArr,lcBrFields
llNoRec = .F.
lnCurAlias = SELECT(0)
SELECT ORDHDR
lcTag = ORDER('OrdHdr')
lcBrFields = [Order:H="Order#",status:1:H="S",lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
             [ACCOUNT:H="Acct",store=IIF(MULTI='Y','*Multi*',STORE):H="Store",Customer.stname]+;
             [:15:H="Name",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",]+;
             [start:H="Start",Complete:H="Complete",]+;
             [Note1:6:H="Notes"]

DO CASE
  CASE !EMPTY(lcAccount)
    SET ORDER TO TAG 'OrdAcct' IN OrdHdr
    lcKey = m.Account+'O'
    lcFor = ''
    IF !SEEK(lcKey,'OrdHdr')
      llNoRec = .T.
    ENDIF
  CASE EMPTY(lcAccount)
    SET ORDER TO TAG 'OrdHdr' IN OrdHdr
    lcKey = 'O'
    lcFor = ''
    llNoRec = !SEEK(lcKey,'OrdHdr')
ENDCASE
IF llNoRec
  llReturn = .F.
  *-- There are no records to browse.
  *-- OK
  = gfModalGen("INM44032B00000","Dialog")
ELSE
  llReturn = gfBrows("lcKey" + lcFor,'Order','laTemp')
ENDIF

SET ORDER TO lcTag IN OrdHdr

SELECT (lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwHdrBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAccount()
*!*************************************************************
*!
FUNCTION lfvAccount

IF MDOWN()
  RETURN
ENDIF
  
lnCurAlias = SELECT(0)

XACCOUNT = m.Account
IF llBrowse OR (!EMPTY(XACCOUNT) AND !SEEK('M'+XACCOUNT,'Customer'))
  llBrowse = .F.
  DO CUSBROWM WITH XACCOUNT
  m.Account = XACCOUNT
ENDIF  

IF EMPTY(m.Account)
  STORE SPACE(6) TO m.Order
  STORE SPACE(8) TO m.Store
ENDIF
SELECT (lcPakHdr)
GATHER MEMVAR
=lfwHdrBrow()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvStore
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwHdrBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStore()
*!*************************************************************
*!

FUNCTION lfvStore

PRIVATE xStore,lcOrLinTag,lnCurAlias

lnCurAlias = SELECT(0)
lcOrLinTag = ORDER('OrdLine')

IF !EMPTY(m.Account)  
  xStore = m.Store
  IF llBrowse .OR. (!EMPTY(m.Store) AND !SEEK('S'+m.Account+m.Store,'Customer'))
    llBrowse  = .F.
    m.Store = IIF(!CusBrowS(m.Account),SPACE(8),xStore)
  ENDIF
  SELECT OrdLine
  SET ORDER TO OrdLinSt
  IF !EMPTY(m.Store) AND !SEEK('O'+m.Order+m.Store,'OrdLine')
    *-- No ordered quantity for Store m.Store
    *-- <OK>
    llNoThing = gfModalGen("INM44031B00000","Dialog",m.Store)  
    m.Store = SPACE(8)
  ENDIF  
ENDIF

SELECT (lcPakHdr)
GATHER MEMVAR
=lfwHdrBrow()

SET ORDER TO lcOrLinTag IN OrdLine
  
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfDtlBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfvDtlBrow ,lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDtlBrow()
*!*************************************************************
*!
FUNCTION lfDtlBrow

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT(lcPakLin)

lnHdrBrRec = RECNO()

*BROWSE FIELDS cMarker =IIF(RECNO()=lnDtlBrRec,'>',' '):H= ' ':R:1:W=.F.,;
              From_Crt :H="From"   :R :4 ,;
              To_Crt   :H="To"     :R :4 ,;
              Style    :H=lcStyTtl :R :19,;
              Qty1     :H="Qty1"   :R :5 ,;
              Qty2     :H="Qty2"   :R :5 ,;
              Qty3     :H="Qty3"   :R :5 ,;
              Qty4     :H="Qty4"   :R :5 ,;
              Qty5     :H="Qty5"   :R :5 ,;
              Qty6     :H="Qty6"   :R :5 ,;
              Qty7     :H="Qty7"   :R :5 ,;
              Qty8     :H="Qty8"   :R :5 ,;
              TotQty   :H="Total"  :R :6  ;
         REST FOR Pack_No+Style+STR(nOrdLineNo,6) = &lcPakHdr..Pack_No ;
         SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
         TITLE (lcDtlTtl) WHEN lfwDtlBrow() ;
         VALID :F lfvDtlBrow()             ;         
         WINDOW (lcWPakDtl1) IN WINDOW (lcWPakDtl)
lcBrowFld = [cMarker =IIF(RECNO()=lnDtlBrRec,'>',' '):H= ' ':R:1:W=.F.,]+;
            [ From_Crt :H="From"   :R :4 ,]+;
            [To_Crt   :H="To"     :R :4  ,]+;
            [Style    :H=lcStyTtl :R :19 ,]

lcBrowFld = lcBrowFld + [Qty1     :H="Pack1"   :R :5  ,]+;
            [Qty2     :H="Pack2"   :R :5 , ]+;
            [Qty3     :H="Pack3"   :R :5 , ]+;
            [Qty4     :H="Pack4"   :R :5 ,]+;
            [Qty5     :H="Pack5"   :R :5 ,]
            
lcBrowFld = lcBrowFld +[Qty6     :H="Pack6"   :R :5 ,]+;
            [Qty7     :H="Pack7"   :R :5 ,]+;
            [Qty8     :H="Pack8"   :R :5 ,]+;
            [TotQty   :H="Total Pack "  :R :6 ,]+;
            [OrgOrd1  :H="Ord1"   :R :5 ,]
            
lcBrowFld = lcBrowFld +[OrgOrd2     :H="Ord2"   :R :5 ,]+;
            [OrgOrd3   :H="Ord3"   :R :5 ,]+;
            [OrgOrd4   :H="Ord4"   :R :5 ,]+;
            [OrgOrd5   :H="Ord5"   :R :5 ,]+;
            [OrgOrd6   :H="Ord6"   :R :5 ,]+;
            [OrgOrd7   :H="Ord7"   :R :5 ,]
            
lcBrowFld = lcBrowFld +[OrgOrd8   :H="Ord8"   :R :5 ,]+;
            [TotOrgOrd :H="TotalTotOrd"  :R :6 ,]+;
            [Pik1      :H="Pik1"   :R :5 ,]+;
            [Pik2      :H="Pik2"   :R :5 ,]+;
            [Pik3      :H="Pik3"   :R :5 ,]+;
            [Pik4      :H="Pik4"   :R :5 ,]
            
lcBrowFld = lcBrowFld +[Pik5      :H="Pik5"   :R :5 ,]+;
            [Pik6      :H="Pik6"   :R :5 ,]+;
            [Pik7      :H="Pik7"   :R :5 ,]+;
            [Pik8      :H="Pik8"   :R :5 ,]+;
            [TotPik    :H="Total Pik"  :R :6 ]
BROWSE FIELDS &lcBrowFld  ;
         REST FOR Pack_No+Style+STR(nOrdLineNo,6) = &lcPakHdr..Pack_No ;
         SAVE NOWAIT NOAPPEND NODELETE NOMENU NOCLEAR ;
         TITLE (lcDtlTtl) WHEN lfwDtlBrow() ;
         VALID :F lfvDtlBrow()             ;         
         WINDOW (lcWPakDtl1) IN WINDOW (lcWPakDtl)

=lfwDtlBrow()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwDtlBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfRefresh
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwDtlBrow()
*!*************************************************************
*!
FUNCTION lfwDtlBrow
PRIVATE lcStyStat,lcQtyStat,lcCrtStat,lcNewStat,lcRemStat
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

lnDtlBrRec = RECNO(lcPakLin)

SELECT Scale
SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR

*:B604765,1 MHM 04/08/2001 refresh Qty field[start]
*=lfRefresh()
*:B604765,1 MHM 04/08/2001[end]

SHOW WINDOW (lcDtlTtl) REFRESH SAME

IF laScrMode[1] OR EMPTY(&lcPakHdr..Pack_No)
  STORE 'DISABLE' TO lcStyStat,lcQtyStat,lcCrtStat,lcNewStat,lcRemStat
  SELECT(lcPakLin)
  SCATTER MEMVAR BLANK
ELSE
  lcNewStat = 'ENABLE'
  SELECT(lcPakLin)

  SCATTER MEMVAR
  IF From_Crt = 0 OR To_Crt = 0
    STORE 'DISABLE' TO lcStyStat,lcQtyStat
  ELSE
    IF EMPTY(m.Style)
      lcStyStat = 'ENABLE'
      lcQtyStat = 'DISABLE'
    ELSE
      lcStyStat = 'DISABLE'
      lcQtyStat = 'ENABLE'
    ENDIF
  ENDIF
  STORE 'ENABLE' TO lcCrtStat,lcRemStat

  IF EOF(lcPakLin)
    STORE 'DISABLE' TO lcCrtStat,lcRemStat
  ENDIF
ENDIF
SHOW GET m.Style    &lcStyStat
SHOW GET pbStyBrow  &lcStyStat
SHOW GET m.Qty1     &lcQtyStat
SHOW GET m.Qty2     &lcQtyStat
SHOW GET m.Qty3     &lcQtyStat
SHOW GET m.Qty4     &lcQtyStat
SHOW GET m.Qty5     &lcQtyStat
SHOW GET m.Qty6     &lcQtyStat
SHOW GET m.Qty7     &lcQtyStat
SHOW GET m.Qty8     &lcQtyStat
SHOW GET m.From_Crt &lcCrtStat
SHOW GET m.To_Crt   &lcCrtStat
SHOW GET pbNew      &lcNewStat
SHOW GET pbRem      &lcRemStat
*:B604765,1 MHM 04/08/2001 [start]
=lfRefresh()
*:B604765,1 MHM 04/08/2001 [end]
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvDtlBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDtlBrow()
*!*************************************************************
*!
FUNCTION lfvDtlBrow

IF WONTOP() # (lcDtlTtl)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfvNewPak
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwHdrBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewPak()
*!*************************************************************
*!
FUNCTION lfvNewPak
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
SELECT (lcPakHdr)
IF !SEEK(SPACE(6))
  APPEND BLANK
ENDIF

= lfwHdrBrow()
_CUROBJ = OBJNUM(m.Pack_No)
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvRemPak
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwHdrBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemPak()
*!*************************************************************
*!
FUNCTION lfvRemPak

*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1
  PRIVATE lnCurAlias
  lnCurAlias = SELECT(0)
  SELECT (lcPakLin)
  IF SEEK(&lcPakHdr..Pack_No)
    DELETE REST WHILE Pack_No = &lcPakHdr..Pack_No
  ENDIF

  SELECT (lcPakHdr)
  BLANK
  DELETE
  GO TOP
  = lfwHdrBrow()
  SELECT(lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfwOldCrt
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwOldCrt()
*!*************************************************************
*!
FUNCTION lfwOldCrt

lnOldCrt = EVAL('m.'+VARREAD())

*!*************************************************************
*! Name      : lfvCartons
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfHdPcsWgh ,lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCartons()
*!*************************************************************
*!
FUNCTION lfvCartons

PRIVATE lcVar,lnCurAlias,lnContinue,lnCrtCount,lcPakQtyFl,lcOrdQtyFl,lcOrgQtyFl
lnCrtCount = 0
lnContinue = 0

lnCurAlias = SELECT(0)
SELECT (lcPakLin)
lcVar = 'm.'+VARREAD()
IF &lcVar # lnOldCrt
  llPackChng = .T.
  DO CASE
    CASE lcVar = "m.FROM_CRT"
      IF m.From_Crt <= 0
        m.From_Crt = lnOldCrt
      ENDIF
      IF m.From_Crt > m.To_Crt
        m.To_Crt = m.From_Crt
      ENDIF

    CASE lcVar = "m.TO_CRT"
      IF m.To_Crt <= 0 OR m.To_Crt < m.From_Crt
        m.To_Crt = lnOldCrt
      ENDIF
  ENDCASE  

  lnCrtCount = m.To_Crt - m.From_Crt + 1
  
  IF !EMPTY(m.Style)
    IF (lnCrtCount * &lcPakLin..TotQty) > &lcPakLin..TotOrgOrd
      IF &lcPakLin..TotOrd <= (&lcPakLin..TotOrgOrd)

        *-- Packed quantity for Style x exceeds ordered quantity.;
        Do you want to modify the orderd quantity?
        *-- <Yes>,<No>
        lnContinue = gfModalGen("QRM44086B00006","Dialog",m.Style)
      ENDIF

      IF lnContinue # 2
        m.TotOrd = 0
        FOR lnI = 1 TO 8
          lcOrdQtyFl = 'm.Ord' + ALLTRIM(STR(lnI))
          lcPakQtyFl = 'Qty' + ALLTRIM(STR(lnI))
          &lcOrdQtyFl = lnCrtCount * &lcPakQtyFl
          m.TotOrd = m.TotOrd + &lcOrdQtyFl
        ENDFOR
      ENDIF
    ENDIF
    *-- IF &lcPakLin..TotOrgOrd > &lcPakLin..TotOrd AND ;
    *-- This if to retrive the OrdQty if the user make the packed
    *-- Qty less the new ord qty which he has exccede before
    IF &lcPakLin..TotOrgOrd < &lcPakLin..TotOrd AND ;
       (lnCrtCount * &lcPakLin..TotQty) < &lcPakLin..TotOrd

      FOR lnI = 1 TO 8
        lcOrdQtyFl = 'm.Ord' + ALLTRIM(STR(lnI))
        lcOrgQtyFl = 'OrgOrd' + ALLTRIM(STR(lnI))
        &lcOrdQtyFl = &lcOrgQtyFl
        m.TotOrd = TotOrgOrd
      ENDFOR
    ENDIF 
  ENDIF
  
  IF lnContinue # 2
    =lfHdPcsWgh() 
    IF m.TotQty = 0
      m.cZeroQty = 'Y'
    ELSE
      m.cZeroQty = 'N'
    ENDIF
    GATHER MEMVAR
  ENDIF

  = lfwDtlBrow()
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfwOldQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwOldQty()
*!*************************************************************
*!
FUNCTION lfwOldQty

lnOldQty = EVAL('m.'+VARREAD())

*!*************************************************************
*! Name      : lfvQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwDtlBrow ,gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvQty()
*!*************************************************************
*!
FUNCTION lfvQty
PRIVATE lnCurAlias,lnContinue,lnCrtCount,lcPakQtyFl,lcOrdQtyFl,lcOrgQtyFl
lnContinue = 0

lnCurAlias = SELECT(0)
SELECT (lcPakLin)

lcPakQtyFl = 'm.'+VARREAD()
IF EVAL(lcPakQtyFl) # lnOldQty AND !EMPTY(&lcPakLin..Style)
  llPackChng = .T.
  m.TotQty = m.TotQty - lnOldQty + EVAL(lcPakQtyFl)

  lnCrtCount = m.To_Crt - m.From_Crt + 1

  *-- This is to make this inquire happen just 1 time for each line

  IF (lnCrtCount * m.TotQty) > &lcPakLin..TotOrgOrd
  
    IF &lcPakLin..TotOrd <= (&lcPakLin..TotOrgOrd)
      *-- Packed quantity for Style x exceeds ordered quantity.;
      Do you want to modify the orderd quantity?
      *-- <Yes>,<No>
      lnContinue = gfModalGen("QRM44086B00006","Dialog",m.Style)
    ENDIF
    IF lnContinue # 2
      lcOrdFld  = STRTRAN(lcPakQtyFl,'QTY','Ord')
      &lcOrdFld = lnCrtCount * &lcPakQtyFl
      m.TotOrd  = lnCrtCount * m.TotQty
    ENDIF
  ENDIF
  *-- IF &lcPakLin..TotOrgOrd > &lcPakLin..TotOrd AND ;
  *-- This if to retrive the OrdQty if the user make the packed
  *-- Qty less the new ord qty which he has exccede before

  SELECT (lcPakLin)

  IF &lcPakLin..TotOrgOrd < &lcPakLin..TotOrd AND ;
     (lnCrtCount * &lcPakLin..TotQty) < &lcPakLin..TotOrd

     lcOrgQtyFl = SUBSTR(STRTRAN(lcPakQtyFl,'QTY','OrgOrd'),3)
     lcOrdFld   = STRTRAN(lcPakQtyFl,'QTY','Ord')
     &lcOrdFld = &lcOrgQtyFl
     m.TotOrd = TotOrgOrd
  ENDIF
  
  IF lnContinue # 2
    =lfHdPcsWgh()

    IF m.TotQty = 0
      m.cZeroQty = 'Y'
    ELSE
      m.cZeroQty = 'N'
    ENDIF

    SELECT (lcPakLin)

    GATHER MEMVAR
  ENDIF
  
  = lfwDtlBrow()
ENDIF  

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfHdPcsWgh
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfHdPcsWgh()
*!*************************************************************
*!
FUNCTION lfHdPcsWgh
PARAMETERS llRem

PRIVATE lnCurAlias,lnOldCrCnt,lnNewCrCnt,lnOldQty,lnNewQty
lnCurAlias = SELECT(0)
IF m.From_Crt # 0 AND m.To_Crt # 0

  lnOldCrCnt = &lcPakLin..To_Crt - &lcPakLin..From_Crt + 1

  lnNewCrCnt = m.To_Crt - m.From_Crt + 1

  lnOldQty   = &lcPakLin..TotQty

  IF llRem
    lnNewQty   = 0
  ELSE
    lnNewQty   = m.TotQty
  ENDIF
  SELECT (lcPakHdr)
  REPLACE Tot_Wght WITH MAX(0,Tot_Wght - ;
                              (lnOldCrCnt * lnOldQty * Style.nStyWeight) + ;
                              (lnNewCrCnt * lnNewQty * Style.nStyWeight)) ,;
          Tot_Pcs  WITH Tot_Pcs - (lnOldCrCnt * lnOldQty ) + ;
                                  (lnNewCrCnt * lnNewQty )
                       
  SELECT (lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvNew
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfwDtlBrow , 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNew()
*!*************************************************************
*!
FUNCTION lfvNew
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT (lcPakLin)


IF !SEEK(&lcPakHdr..Pack_No+SPACE(19))
  APPEND BLANK
  REPLACE Pack_No WITH &lcPakHdr..Pack_No
ENDIF  
= lfwDtlBrow()
_CUROBJ = OBJNUM(m.From_Crt)
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvRem
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfModalGen, lfHdPcsWgh ,lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRem()
*!*************************************************************
*!
FUNCTION lfvRem

*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1

  PRIVATE lnCurAlias
  lnCurAlias = SELECT(0)
  = lfHdPcsWgh(.T.)
  
  SELECT (lcPakLin)
  **-- Fix bug of variabel "lcPack_no" not found [Begin]
  lcPack_no = &lcPakLin..Pack_no

  BLANK
  DELETE
  GO TOP
  = lfwDtlBrow()
  SELECT(lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : lfStyBrow ,lfHdPcsWgh ,lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStyle()
*!*************************************************************
*!
FUNCTION lfvStyle
PRIVATE lcPakQtyFl
IF MDOWN()
  RETURN
ENDIF
  
PRIVATE lnCurAlias,lnLineNo,llGetInfo,lcMemVar ,lnContinue
llGetInfo = .F.
lnContinue = 0
lnCurAlias = SELECT(0)

SELECT PikTKt
SET RELATION TO 'O' + Order INTO OrdLine ADDITIVE
SET SKIP TO OrdLine

IF !llBrowse AND !EMPTY(m.Style) AND SEEK('O'+&lcPakHdr..Order+&lcPakHdr..Store+m.Style,'OrdLine')
  SELECT OrdLine
  LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                    'O'+&lcPakHdr..Order+&lcPakHdr..Store+m.Style ;
              FOR   PikTkt = &lcPakHdr..Pack_No
  IF FOUND()
    lnLineNo = OrdLine.LineNo
    LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                      'O'+&lcPakHdr..Order+&lcPakHdr..Store+m.Style ;
                FOR   PikTkt = &lcPakHdr..Pack_No AND LineNo # lnLineNo
    IF FOUND()
      llBrowse = .T.
    ELSE
      = SEEK('O'+&lcPakHdr..Order+&lcPakHdr..Store+m.Style,'OrdLine')
      LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                        'O'+&lcPakHdr..Order+&lcPakHdr..Store+m.Style ;
              FOR   PikTkt = &lcPakHdr..Pack_No
      llGetInfo = FOUND()
    ENDIF
  ENDIF
ELSE
  IF !EMPTY(m.Style)
    llBrowse = .T.
  ENDIF
ENDIF

IF llBrowse
  llBrowse = .F.
  llGetInfo = lfStyBrow()
ENDIF

IF llGetInfo
  llPackChng = .T.
  m.Style      = OrdLine.Style
  m.nOrdLineNo = OrdLine.LineNo
  m.Scale      = OrdLine.Scale
  m.PrePak     = OrdLine.PrePak

  FOR lnI = 1 To 8
    lcMemVar  = "m.Qty"+STR(lnI,1)
    &lcMemVar = EVAL('OrdLine.Pik'+STR(lnI,1))
    lcMemVar  = "m.OrgOrd"+STR(lnI,1)
    &lcMemVar = EVAL('OrdLine.Qty'+STR(lnI,1))
    lcMemVar  = "m.Ord"+STR(lnI,1)
    &lcMemVar = EVAL('OrdLine.Qty'+STR(lnI,1))
  ENDFOR
  m.TotQty    = OrdLine.TotPik
  m.TotOrgOrd = OrdLine.TotQty
  m.TotOrd    = OrdLine.TotQty

  = SEEK(&lcPakHdr..Pack_No+SPACE(19),lcPakLin)

  IF m.TotQty = 0
    m.cZeroQty = 'Y'
  ELSE
    m.cZeroQty = 'N'
  ENDIF
  
  =lfHdPcsWgh()
  
  SELECT(lcPakLin)
  
  GATHER MEMVAR
  = lfwDtlBrow()
ELSE
  m.Style = SPACE(19)
ENDIF


*-to update piked QTY
IF !EMPTY(m.Style)
  REPLACE Pik1       WITH OrdLine.Pik1    ,;
          Pik2       WITH OrdLine.Pik2    ,;
          Pik3       WITH OrdLine.Pik3    ,;
          Pik4       WITH OrdLine.Pik4    ,;
          Pik5       WITH OrdLine.Pik5    ,;
          Pik6       WITH OrdLine.Pik6    ,;
          Pik7       WITH OrdLine.Pik7    ,;
          Pik8       WITH OrdLine.Pik8    ,;
          TotPik     WITH OrdLine.TotPik  
  lnCrtCount = m.To_Crt - m.From_Crt + 1

  IF (lnCrtCount * m.TotQty) > &lcPakLin..TotOrgOrd

    IF &lcPakLin..TotOrd <= (&lcPakLin..TotOrgOrd)
      *-- Packed quantity for Style x exceeds ordered quantity.;
      Do you want to modify the orderd quantity?
      *-- <Yes>,<No>
      lnContinue = gfModalGen("QRM44086B00006","Dialog",m.Style)
    ENDIF
  
    IF lnContinue # 2
      m.TotOrd = 0
      FOR lnI = 1 TO 8
        lcOrdQtyFl = 'm.Ord' + ALLTRIM(STR(lnI))
        lcPakQtyFl = 'Qty' + ALLTRIM(STR(lnI))
        &lcOrdQtyFl = lnCrtCount * &lcPakQtyFl
        m.TotOrd = m.TotOrd + &lcOrdQtyFl
      ENDFOR

      *-- IF &lcPakLin..TotOrgOrd > &lcPakLin..TotOrd AND ;
      *-- This if to retrive the OrdQty if the user make the packed
      *-- Qty less the new ord qty which he has exccede before

      SELECT (lcPakLin)

      IF &lcPakLin..TotOrgOrd < &lcPakLin..TotOrd AND ;
        (lnCrtCount * &lcPakLin..TotQty) < &lcPakLin..TotOrd

        FOR lnI = 1 TO 8
          lcOrdQtyFl = 'm.Ord' + ALLTRIM(STR(lnI))
          lcOrgQtyFl = 'OrgOrd' + ALLTRIM(STR(lnI))
          &lcOrdQtyFl = &lcOrgQtyFl
          m.TotOrd = TotOrgOrd
        ENDFOR
      ENDIF
      IF lnContinue # 2
        =lfHdPcsWgh()

        IF m.TotQty = 0
          m.cZeroQty = 'Y'
        ELSE
          m.cZeroQty = 'N'
        ENDIF

        SELECT (lcPakLin)
        GATHER MEMVAR
      
      ENDIF
      = lfwDtlBrow()
    ELSE
      BLANK
      DELETE
      GO TOP
      = lfwDtlBrow()
      SELECT PikTkt
      SET RELATION OFF INTO OrdLine
      RETURN
    ENDIF
  ENDIF

  IF (  &lcPakLin..TotQty)*Style.nstyweight/lnCrtCount> lnRpWght

      *-- Packed weight for Style x exceeds break weight.;
      *Do you want to modify the orderd quantity?
      *-- <Ok>
      lcMessage = "Packed weight for Style "+m.style+" exceeds break weight."
      =gfModalGen("TRM000000B00000","DIALOG",'','','&lcMessage')  
  ENDIF  

  *--lnRpWght
  =SEEK('p'+Scale,'Scale')
  IF lnRpWght < Scale.PPTot * Style.nstyweight
    *-- Break weight exceeds prepack weight
    *-- <Ok>
    lcMessage = "Break weight exceeds prepack weight"
    =gfModalGen("TRM000000B00000","DIALOG",'','','&lcMessage')  
  ENDIF

  REPLACE &lcPakHdr..Tot_Cart WITH lnCrtCount ;
          &lcPakHdr..Tot_Wght WITH &lcPakHdr..Tot_Wght + (m.TotQty* Style.nStyWeight)
ENDIF

SELECT PikTkt
SET RELATION OFF INTO OrdLine

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfStyBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfStyBrow()
*!*************************************************************
*!
FUNCTION lfStyBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl,;
        lcPikTkt,laTemp
DIMENSION laBrow[1],laTemp[1]
STORE SPACE(0) TO lcFields,laBrow,laTemp
llReturn = .F.

lnCurAlias = SELECT(0)
lcBrFields = 'OrdLine.Style,OrdLine.PikTkt,OrdLine.Order,OrdLine.LineNo,'+;
           'OrdLine.Pik1,OrdLine.Pik2,OrdLine.Pik3,OrdLine.Pik4,'+;
           'OrdLine.Pik5,OrdLine.Pik6,OrdLine.Pik7,OrdLine.Pik8,'+;
           'OrdLine.TotPik'

lcFile_Ttl  = 'Style Picked Quantities'

PRIVATE lcPikTag
lcPikTkt = &lcPakHdr..Pack_No
SELECT PikTkt
lcPikTag = ORDER()
SET ORDER TO PikTKt
IF SEEK(lcPikTkt)
  lcPikTkt = &lcPakHdr..Pack_no
  llReturn  = gfBrows("lcPikTkt FOR PikTkt.PikTkt=OrdLine.PikTkt",'OrdLine.Style','laTemp')
ELSE
  llReturn = .F.
  *-- There are no records to browse.
  *-- OK
  = gfModalGen("INM44032B00000","Dialog")  
ENDIF
SET ORDER TO (lcPikTag)  
SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfActPad
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfActPad()
*!*************************************************************
*!
FUNCTION lfActPad

DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+J
DEFINE BAR 101 OF P01PU01 PROMPT "" KEY ALT+P
DEFINE BAR 102 OF P01PU01 PROMPT "" KEY ALT+D
ON SELECTION BAR 100 OF  P01PU01 ACTIVATE WINDOW (lcRejTtl)
ON SELECTION BAR 101 OF  P01PU01 ACTIVATE WINDOW (lcHdrTtl)
ON SELECTION BAR 102 OF  P01PU01 ACTIVATE WINDOW (lcDtlTtl)

*-- Define the "Options" popup.
DEFINE PAD   _OPTIONS OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , SPACE(1)
DEFINE POPUP _OPTPOP MARGIN SHADOW

*-- Define the "Options" bars in the "Options" menu pad.
DEFINE BAR 1 OF _OPTPOP PROMPT "Print Labels Setup" SKIP FOR (laScrMode[1] OR laScrMode[2] OR laScrMode[3])

*-- Define the action to be done when activating the pad and the popup.
ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTPOP
ON SELECTION BAR 1 OF _OPTPOP DO lpPortScr

*--end

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfModalGen ,gfGettime,lfUpdtBOL
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpSavscr
*!*************************************************************
*!
PROCEDURE lpSavscr

PRIVATE lcPLTmpTag,llReturn,lnCartNo,lnFromCrt,;
        lnBookQty,lnBookAmt,lnOldOQty,lnOldOAmt,lnLineNo,;
        lnOverWrite,llOverWrite,lcHdrPack,lcLinPack,lcAcc

lnOverWrite = 0        
STORE SPACE(6) TO lcHdrPack,lcLinPack

llReturn = .F.
llOverWrite = .F.

llPrnLbl = .T.
lcAcc = SPACE(6)

SELECT (lcPakLin)
lcPLTmpTag = ORDER(lcPakLin)
SET ORDER TO ZeroQty IN (lcPakLin)

IF SEEK('Y'+&lcPakHdr..Pack_No,lcPakLin)
  *-- One or more Pack line has 0 quantity.
  *-- Zero quantity lines will be ignored.
  *-- <Proceed>,<Cancel>
  lnChoice = gfModalGen("QRM44084B00012","Dialog")
  llReturn = lnChoice = 1
ELSE
  llReturn = .T.
ENDIF

IF llReturn
  SELECT(lcPakHdr)
  SCAN
    IF SEEK(Pack_No,'Pack_Hdr')
      *-- One or more Picking Ticket has been already packed.
      *-- Do you wish to overwrite this packs?
      *-- <Yes>,<No>,<Cancel>
      lnOverWrite = gfModalGen("QRM44085B00025","Dialog")
      llReturn = lnOverWrite # 3
      EXIT
    ENDIF
  ENDSCAN
ENDIF  

IF llReturn
  SCAN
    STORE 0 TO lnCartNo,lnFromCrt,lnBookQty,lnBookAmt,lnOldOQty,lnOldOAmt,lnLineNo
    IF SEEK('N'+&lcPakHdr..Pack_No,lcPakLin)

      *B606159,1 WAEL 30/06/2002 (Begin) Update BOL file.
      IF SEEK(&lcPakHdr..Pack_No,'Pack_Hdr') AND !EMPTY(Pack_Hdr.Bill_Ladg) AND SEEK(Pack_Hdr.Bill_Ladg,'BOL_Hdr')
        SELECT BOL_HDR
        REPLACE TOT_WGHT WITH TOT_WGHT - PACK_HDR.Tot_Wght ,;
                TOT_CART WITH TOT_CART - PACK_HDR.Tot_Cart ,;
                TOT_PCS  WITH TOT_PCS  - PACK_HDR.Tot_Pcs
        IF SEEK(Pack_Hdr.Bill_Ladg+Pack_hdr.Order+Pack_Hdr.Pack_No,'BOL_LIN')
          SELECT BOL_LIN
          DELETE 
        ENDIF
        IF SEEK(Pack_Hdr.Bill_Ladg+Pack_Hdr.Pack_No,'ASN_SHIP')
          SELECT ASN_SHIP
          DELETE REST WHILE bol_no+pack_no+STR(cart_no,6)+asn_ver=Pack_Hdr.Bill_Ladg+Pack_Hdr.Pack_No
        ENDIF
        SELECT(lcPakHdr)
      ENDIF
      *B606159,1 WAEL 30/06/2002 (End)

      llOverWrite = lnOverWrite = 0 OR (SEEK(&lcPakHdr..Pack_No,'Pack_Hdr') AND lnOverWrite = 1)
      lnOrdLno = 0
      SELECT (lcPakLin)
      SCAN REST WHILE cZeroQty + Pack_No = 'N' + &lcPakHdr..Pack_No
        
        lnCrtCount = &lcPakLin..To_Crt - &lcPakLin..From_Crt + 1
        llNoThing  = SEEK(&lcPakLin..Style,'Style')
        IF llNoThing
          llNoThing = SEEK('O'+&lcPakHdr..Order+&lcPakHdr..Store+&lcPakLin..Style+STR(&lcPakLin..nOrdLineNo,6),'OrdLine')
        ENDIF
        
        IF llNoThing
          *===============updating style,stydye files===================*
          IF llOverWrite
            SELECT Style
            REPLACE Alo1   WITH Alo1 - OrdLine.Pik1 + (lnCrtCount * &lcPakLin..Qty1),;
                    Alo2   WITH Alo2 - OrdLine.Pik2 + (lnCrtCount * &lcPakLin..Qty2),;
                    Alo3   WITH Alo3 - OrdLine.Pik3 + (lnCrtCount * &lcPakLin..Qty3),;
                    Alo4   WITH Alo4 - OrdLine.Pik4 + (lnCrtCount * &lcPakLin..Qty4),;
                    Alo5   WITH Alo5 - OrdLine.Pik5 + (lnCrtCount * &lcPakLin..Qty5),;
                    Alo6   WITH Alo6 - OrdLine.Pik6 + (lnCrtCount * &lcPakLin..Qty6),;
                    Alo7   WITH Alo7 - OrdLine.Pik7 + (lnCrtCount * &lcPakLin..Qty7),;
                    Alo8   WITH Alo8 - OrdLine.Pik8 + (lnCrtCount * &lcPakLin..Qty8),;
                    TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8

            REPLACE Ord1   WITH Ord1 - &lcPakLin..OrgOrd1 + &lcPakLin..Ord1,;
                    Ord2   WITH Ord2 - &lcPakLin..OrgOrd2 + &lcPakLin..Ord2,;
                    Ord3   WITH Ord3 - &lcPakLin..OrgOrd3 + &lcPakLin..Ord3,;
                    Ord4   WITH Ord4 - &lcPakLin..OrgOrd4 + &lcPakLin..Ord4,;
                    Ord5   WITH Ord5 - &lcPakLin..OrgOrd5 + &lcPakLin..Ord5,;
                    Ord6   WITH Ord6 - &lcPakLin..OrgOrd6 + &lcPakLin..Ord6,;
                    Ord7   WITH Ord7 - &lcPakLin..OrgOrd7 + &lcPakLin..Ord7,;
                    Ord8   WITH Ord8 - &lcPakLin..OrgOrd8 + &lcPakLin..Ord8,;
                    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

            IF SEEK (OrdLine.Style+OrdLine.cWareCode,'StyDye')
              SELECT StyDye
              REPLACE Alo1 WITH Alo1 - OrdLine.Pik1 + (lnCrtCount * &lcPakLin..Qty1),;
                      Alo2 WITH Alo2 - OrdLine.Pik2 + (lnCrtCount * &lcPakLin..Qty2),;
                      Alo3 WITH Alo3 - OrdLine.Pik3 + (lnCrtCount * &lcPakLin..Qty3),;
                      Alo4 WITH Alo4 - OrdLine.Pik4 + (lnCrtCount * &lcPakLin..Qty4),;
                      Alo5 WITH Alo5 - OrdLine.Pik5 + (lnCrtCount * &lcPakLin..Qty5),;
                      Alo6 WITH Alo6 - OrdLine.Pik6 + (lnCrtCount * &lcPakLin..Qty6),;
                      Alo7 WITH Alo7 - OrdLine.Pik7 + (lnCrtCount * &lcPakLin..Qty7),;
                      Alo8 WITH Alo8 - OrdLine.Pik8 + (lnCrtCount * &lcPakLin..Qty8),;
                      TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
            ENDIF
            IF llDyelot AND  Style.cDye_Flg = 'Y' AND ;
               SEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot,'StyDye')
              REPLACE Alo1 WITH Alo1 - OrdLine.Pik1 + (lnCrtCount * &lcPakLin..Qty1),;
                      Alo2 WITH Alo2 - OrdLine.Pik2 + (lnCrtCount * &lcPakLin..Qty2),;
                      Alo3 WITH Alo3 - OrdLine.Pik3 + (lnCrtCount * &lcPakLin..Qty3),;
                      Alo4 WITH Alo4 - OrdLine.Pik4 + (lnCrtCount * &lcPakLin..Qty4),;
                      Alo5 WITH Alo5 - OrdLine.Pik5 + (lnCrtCount * &lcPakLin..Qty5),;
                      Alo6 WITH Alo6 - OrdLine.Pik6 + (lnCrtCount * &lcPakLin..Qty6),;
                      Alo7 WITH Alo7 - OrdLine.Pik7 + (lnCrtCount * &lcPakLin..Qty7),;
                      Alo8 WITH Alo8 - OrdLine.Pik8 + (lnCrtCount * &lcPakLin..Qty8),;
                      TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
            ENDIF
            *===============end updating style,stydye files===================*
            SELECT OrdLine
            REPLACE Book1  WITH Book1+MAX(&lcPakLin..OrgOrd1-OrdLine.Qty1,0),;
                    Book2  WITH Book2+MAX(&lcPakLin..OrgOrd2-OrdLine.Qty2,0),;
                    Book3  WITH Book3+MAX(&lcPakLin..OrgOrd3-OrdLine.Qty3,0),;
                    Book4  WITH Book4+MAX(&lcPakLin..OrgOrd4-OrdLine.Qty4,0),;
                    Book5  WITH Book5+MAX(&lcPakLin..OrgOrd5-OrdLine.Qty5,0),;
                    Book6  WITH Book6+MAX(&lcPakLin..OrgOrd6-OrdLine.Qty6,0),;
                    Book7  WITH Book7+MAX(&lcPakLin..OrgOrd7-OrdLine.Qty7,0),;
                    Book8  WITH Book8+MAX(&lcPakLin..OrgOrd8-OrdLine.Qty8,0),;
                    ToTBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8
            lnBookQty = lnBookQty + ToTBook
            lnBookAmt = lnBookAmt + (TotBook * Price)
            lnOldOQty = lnOldOQty + TotQty
            lnOldOAmt = lnOldOAmt + (TotQty * Price)
                    
              REPLACE nPck1  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPck1  + (lnCrtCount * &lcPakLin..Qty1)),(lnCrtCount * &lcPakLin..Qty1))
              REPLACE nPck2  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPck2  + (lnCrtCount * &lcPakLin..Qty2)),(lnCrtCount * &lcPakLin..Qty2))
              REPLACE nPck3  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                     (nPck3  + (lnCrtCount * &lcPakLin..Qty3)),(lnCrtCount * &lcPakLin..Qty3))
              REPLACE nPck4  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPck4  + (lnCrtCount * &lcPakLin..Qty4)),(lnCrtCount * &lcPakLin..Qty4))
              REPLACE nPck5  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPck5  + (lnCrtCount * &lcPakLin..Qty5)),(lnCrtCount * &lcPakLin..Qty5))
              REPLACE nPck6  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPck6  + (lnCrtCount * &lcPakLin..Qty6)),(lnCrtCount * &lcPakLin..Qty6))
              REPLACE nPck7  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPck7  + (lnCrtCount * &lcPakLin..Qty7)),(lnCrtCount * &lcPakLin..Qty7))
              REPLACE nPck8  WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPck8  + (lnCrtCount * &lcPakLin..Qty8)),(lnCrtCount * &lcPakLin..Qty8)),;
                      nPWght WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                      (nPWght + (lnCrtCount * &lcPakLin..TotQty * Style.nStyWeight)),(lnCrtCount * &lcPakLin..TotQty * Style.nStyWeight))

            REPLACE Pik1   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (Pik1 + (lnCrtCount * &lcPakLin..Qty1)),(lnCrtCount * &lcPakLin..Qty1))             
            REPLACE Pik2   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (Pik2 + (lnCrtCount * &lcPakLin..Qty2)),(lnCrtCount * &lcPakLin..Qty2))             
            REPLACE Pik3   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (Pik3 + (lnCrtCount * &lcPakLin..Qty3)),(lnCrtCount * &lcPakLin..Qty3))             
            REPLACE Pik4   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (Pik4 + (lnCrtCount * &lcPakLin..Qty4)),(lnCrtCount * &lcPakLin..Qty4))             
            REPLACE Pik5   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (Pik5 + (lnCrtCount * &lcPakLin..Qty5)),(lnCrtCount * &lcPakLin..Qty5))             
            REPLACE Pik6   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo , ;
                           (Pik6 + (lnCrtCount * &lcPakLin..Qty6)),(lnCrtCount * &lcPakLin..Qty6))             
            REPLACE Pik7   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (Pik7 + (lnCrtCount * &lcPakLin..Qty7)),(lnCrtCount * &lcPakLin..Qty7))             
            REPLACE Pik8   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (Pik8 + (lnCrtCount * &lcPakLin..Qty8)),(lnCrtCount * &lcPakLin..Qty8)),;             
                    TotPik WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                           (TotPik +(lnCrtCount * &lcPakLin..TotQty)),(lnCrtCount * &lcPakLin..TotQty))             
            IF llreturn
              REPLACE Qty1   WITH &lcPakLin..Ord1,;
                    Qty2   WITH &lcPakLin..Ord2,;
                    Qty3   WITH &lcPakLin..Ord3,;
                    Qty4   WITH &lcPakLin..Ord4,;
                    Qty5   WITH &lcPakLin..Ord5,;
                    Qty6   WITH &lcPakLin..Ord6,;
                    Qty7   WITH &lcPakLin..Ord7,;
                    Qty8   WITH &lcPakLin..Ord8,;
                    TotQty WITH &lcPakLin..TotOrd
            ELSE       
              REPLACE Qty1   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty1 +  &lcPakLin..Ord1), &lcPakLin..Ord1)
              REPLACE Qty2   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty2 + &lcPakLin..Ord2), &lcPakLin..Ord2)
              REPLACE Qty3   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty3 + &lcPakLin..Ord3), &lcPakLin..Ord3)
              REPLACE Qty4   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty4 + &lcPakLin..Ord4),&lcPakLin..Ord4)
              REPLACE Qty5   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty5 + &lcPakLin..Ord5), &lcPakLin..Ord5)
              REPLACE Qty6   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty6 + &lcPakLin..Ord6),&lcPakLin..Ord6)
              REPLACE Qty7   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty7 +  &lcPakLin..Ord7), &lcPakLin..Ord7)
              REPLACE Qty8   WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (Qty8 +  &lcPakLin..Ord8),&lcPakLin..Ord8),;
                      TotQty WITH IIF(lnOrdLno = &lcPakLin..nOrdLineNo ,;
                             (TotQty +&lcPakLin..TotOrd), &lcPakLin..TotOrd)
            ENDIF                 
            lnOrdLno = &lcPakLin..nOrdLineNo
          ENDIF
        ENDIF
        IF lcLinPack # &lcPakLin..Pack_No
          lcLinPack = &lcPakLin..Pack_No
          SELECT Pack_Lin
          IF SEEK(&lcPakHdr..Pack_No,'Pack_Lin') AND llOverWrite
            DELETE REST WHILE Pack_No = &lcPakHdr..Pack_No
          ENDIF
        ENDIF
        IF llOverWrite

          SELECT (lcPakLin)
            FOR lnCountr = From_Crt TO To_Crt
              SCATTER MEMVAR
              lnLineNo   = lnLineNo + 1
              m.Line_No  = lnLineNo
              m.From_Crt = 0
              m.To_Crt   = 0
              m.No_Cart  = lnCountr
              m.Weight  = m.TotQty * Style.nStyWeight
              m.cAdd_user = gcUser_Id
              m.dAdd_date = gdSysDate
              m.cAdd_time = gfGettime()
              INSERT INTO Pack_Lin FROM MEMVAR
              =gfAdd_Info("Pack_Lin")
            ENDFOR  
            *If update UCC128 label file update carton header and detail temp files
            *          to be used in update ASN_SHIP file
            IF llPrnLbl
          
              IF SEEK('O'+&lcPakHdr..Order+&lcPakHdr..Store+&lcPakLin..Style+;
                                   STR(&lcPakLin..nOrdLineNo,6),'OrdLine')   
              *--loop to get all carton
                *B606159,1 WAEL 30/06/2002 (Begin) Update the carton detail with the right number of cartons.
                *FOR lnCount = 1 TO &lcPakLin..No_Cart
                FOR lnCount = &lcPakLin..From_Crt TO &lcPakLin..To_Crt
                *B606159,1 WAEL 30/06/2002 (End)
                  lnPackLin   = &lcPakLin..nOrdLineNo
                  lnStyOrdLin = OrdLine.LineNo
                  SELECT (lcCtnDtl)
                  APPEND BLANK
                  REPLACE pack_No    WITH &lcPakLin..pack_No,;
                          Style      WITH &lcPakLin..Style,;
                          Pack_Id    WITH OrdLine.Pack_Id ,;
                          SzCnt      WITH &lcPakLin..SzCnt,;
                          cStatus    WITH 'S',;
                          nOrdLineNo WITH lnStyOrdLin,;
                          PackLineNo WITH lnPackLin,;
                          Cart_No    WITH lnCount ,;
                          Qty1       WITH &lcPakLin..Qty1,;
                          Qty2       WITH &lcPakLin..Qty2,;
                          Qty3       WITH &lcPakLin..Qty3,;
                          Qty4       WITH &lcPakLin..Qty4,;
                          Qty5       WITH &lcPakLin..Qty5,;
                          Qty6       WITH &lcPakLin..Qty6,;
                          Qty7       WITH &lcPakLin..Qty7,;
                          Qty8       WITH &lcPakLin..Qty8
                      
                  REPLACE Size1      WITH IIF(Qty1>0,&lcPakLin..cSize1,Size1),;
                          Size2      WITH IIF(Qty2>0,&lcPakLin..cSize2,Size2),;
                          Size3      WITH IIF(Qty3>0,&lcPakLin..cSize3,Size3),;
                          Size4      WITH IIF(Qty4>0,&lcPakLin..cSize4,Size4),;
                          Size5      WITH IIF(Qty5>0,&lcPakLin..cSize5,Size5),;
                          Size6      WITH IIF(Qty6>0,&lcPakLin..cSize6,Size6),;
                          Size7      WITH IIF(Qty7>0,&lcPakLin..cSize7,Size7),;
                          Size8      WITH IIF(Qty8>0,&lcPakLin..cSize8,Size8)
                  
                  REPLACE Br1        WITH !EMPTY(Qty1),;
                          Br2        WITH !EMPTY(Qty2),;
                          Br3        WITH !EMPTY(Qty3),;
                          Br4        WITH !EMPTY(Qty4),;
                          Br5        WITH !EMPTY(Qty5),;
                          Br6        WITH !EMPTY(Qty6),;
                          Br7        WITH !EMPTY(Qty7),;
                          Br8        WITH !EMPTY(Qty8),;
                          Weight1    WITH Qty1*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          Weight2    WITH Qty2*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          Weight3    WITH Qty3*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          Weight4    WITH Qty4*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          Weight5    WITH Qty5*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          Weight6    WITH Qty6*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          Weight7    WITH Qty7*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          Weight8    WITH Qty8*(Pack_Lin.Weight/&lcPakLin..TotQty),;
                          OrgWgh     WITH &lcPakLin..Weight
                  *B606159,1 WAEL 30/06/2002 (Begin) Update Carton information.
                  IF SEEK(&lcPakHdr..Bill_Ladg+&lcPakHdr..Pack_No+STR(lnCount,4),lcCtnHdr)
                    SELECT (lcCtnHdr)
                    REPLACE TotPcs WITH TotPcs + ;
                                        &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                        &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                        &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                        &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                            TotWgh WITH TotWgh + ;
                                        &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                        &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                        &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                        &lcCtnDtl..Weight7+&lcCtnDtl..Weight8
                  ELSE 
                     IF (&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                        &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                        &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                        &lcCtnDtl..Qty7+&lcCtnDtl..Qty8) > 0
                        *lnMaxCtn = MAX(lnMaxCtn,&lcPakLin..No_Cart)
                        INSERT INTO (lcCtnHdr) ;
                        (BOL_NO,Pack_No,Cart_No,TotPcs,TotWgh,Empty);
                        VALUES (&lcPakHdr..Bill_Ladg,&lcPakHdr..Pack_No,lnCount,;
                                &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+&lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+&lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                                &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+&lcCtnDtl..Weight7+&lcCtnDtl..Weight8,'N')
                    ENDIF
                  ENDIF
                  *B606159,1 WAEL 30/06/2002 (End)

                ENDFOR  
                *B606159,1 WAEL 30/06/2002 (Begin) Comment out and move it inside the FOR expression.
                *IF SEEK(STR(&lcPakLin..No_Cart,4),lcCtnHdr)
                  *SELECT (lcCtnHdr)
                  *REPLACE TotPcs WITH TotPcs + ;
                                      &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                      &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                      &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                      &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                          TotWgh WITH TotWgh + ;
                                      &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                      &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                      &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                      &lcCtnDtl..Weight7+&lcCtnDtl..Weight8

                *ELSE 
                   *IF (&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                      &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                      &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                      &lcCtnDtl..Qty7+&lcCtnDtl..Qty8) > 0
                      *lnMaxCtn = MAX(lnMaxCtn,&lcPakLin..No_Cart)
    
                      *INSERT INTO (lcCtnHdr) (Cart_No,TotPcs,TotWgh,Empty);
                                      VALUES (&lcPakLin..No_Cart,;
                                              &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                              &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                              &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                              &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                                              &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                              &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                              &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                             &lcCtnDtl..Weight7+&lcCtnDtl..Weight8,'N')

                  *ENDIF
                *ENDIF
                *B606159,1 WAEL 30/06/2002 (End)
              ENDIF                           
              *-- Check if there is a mapping for the label of this account
              *B606159,1 WAEL 30/06/2002 (Begin) Commented out.
              *IF lcAcc <> &lcPakHdr..Account
                *lcAcc = &lcPakHdr..Account
                *llEdiAcc = llEdiSys .AND. SEEK('A' + &lcPakHdr..Account , 'EDIACPRT') .AND.;
                          SEEK(EDIACPRT.cPartCode , 'EDIPH')
                *lcUccVr = ''  
                *IF llEdiAcc
                   *B804367,1 ABD Save the data in the corrct Variable , this is different variable. [Begain]
                   *lcUccVer = IIF(lnDrctTo=1,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
                   *lcUccVr = IIF(lnDrctTo=1,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
                   *B804367,1 ABD [End]
                *ENDIF 
                *IF !SEEK(lcUccVr + 'H','SYCASNLB') 
                  *= gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'No Mapping found For this label for the customer '+&lcPakHdr..Account)
                  *llSavCrt = .F.
                *ELSE
                  *llSavCrt = .T.
                *ENDIF
              *ENDIF  
              *IF llSavCrt
                 *-- Saving Cartons in ASN_SHIP   
                *=lfSavCartn()  
              *ENDIF  
              *B606159,1 WAEL 30/06/2002 (End)
            ENDIF  

            SELECT (lcPakLin)
        ENDIF  
      ENDSCAN  
      IF lcHdrPack # &lcPakHdr..Pack_No
        lcHdrPack = &lcPakHdr..Pack_No
        IF SEEK(&lcPakHdr..Pack_No,'Pack_Hdr') AND llOverWrite
          SELECT Pack_Hdr
          DELETE
        ENDIF
      ENDIF
      
      IF llOverWrite
        SELECT (lcPakHdr)
        SCATTER MEMVAR
        m.PikTkt    = Pack_No
        m.cAdd_user = gcUser_Id
        m.dAdd_date = gdSysDate
        m.cAdd_time = gfGettime()
        INSERT INTO Pack_Hdr FROM MEMVAR
        IF SEEK('O'+&lcPakHdr..Order,'OrdHdr')
          SELECT OrdHdr
          REPLACE BOOK     WITH (Book    - lnOldOQty + lnBookQty) ;
                  BOOKAMT  WITH (BookAmt - lnOldOAmt + lnBookAmt) ;
                  OPEN     WITH (Open    - lnOldOQty + lnBookQty) ;
                  OPENAMT  WITH (OpenAmt - lnOldOAmt + lnBookAmt)
        ENDIF
      ENDIF

      *  update BOL_LIN & BOL_HDR if there is EB or AS modules [Begin]
      =lfUpdtBOL()
      * update BOL_LIN & BOL_HDR if there is EB or AS modules [End  ]
      
    ENDIF
  ENDSCAN

  *C200218,1 BWA 30/08/2001 Modify the custom invoice for sademara.[START]
  IF ASCAN(laEvntTrig , PADR('RECALC',10)) <> 0
    lcParam = "AUTO"
    =gfDoTriger('ALAUTPC',PADR('RECALC',10))
  ENDIF
  *C200218,1 BWA 30/08/2001.[END]
*--SAVE CARTON INFO. TO BE PRINTED 
*--print carton label without EDI 
*B606159,1 WAEL 30/06/2002 (Begin)
*=llCanPrnLb AND lfSavCartn()
=lfLblInfo()
*B606159,1 WAEL 30/06/2002 (End)

ENDIF

SET ORDER TO (lcPLTmpTag) IN (lcPakLin)

*B606159,1 WAEL 30/06/2002 (Begin)
DECLARE laBOLInfo[1,2]
laBOLInfo=''
*B606159,1 WAEL 30/06/2002 (End)

llCSave = llReturn

*!*************************************************************
*! Name      : lfRELPad
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRELPad()
*!*************************************************************
*!
FUNCTION lfRELPad

RELE BAR 100 OF P01PU01
RELE BAR 101 OF P01PU01
RELE BAR 102 OF P01PU01
*-- Release the "Options" menu pad.
RELE PAD   _OPTIONS OF _MSYSMENU

*!*************************************************************
*! Name      : lfOpenFils
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : gfTempName ,gfOpenFile
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFils()
*!*************************************************************
*!
FUNCTION lfOpenFils

lcRejPkTmp = gfTempName()
lcPakHdr   = gfTempName()
lcPakLin   = gfTempName()

llOrdHdrOp = gfOpenFile(gcDataDir+'OrdHdr',gcDataDir+'OrdHdr','SH')
llOrdLinOp = gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'OrdLinSt','SH')
llStyleOp  = gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH')
llStyDyOp  = gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
llScaleOp  = gfOpenFile(gcDataDir+'Scale',gcDataDir+'Scale','SH')
llCustmOp  = gfOpenFile(gcDataDir+'Customer',gcDataDir+'Customer','SH')
llPikTktOp = gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'OrdPik','SH')
llPakHdrOp = gfOpenFile(gcDataDir+'Pack_Hdr',gcDataDir+'Pack_Hdr','SH')
llPakLinOp = gfOpenFile(gcDataDir+'Pack_Lin',gcDataDir+'Pack_Lin','SH')
*--Open ASN_SHIP & WAREHOUS files and create temp ASN_SHIP file [Begin]
=gfOpenFile(gcDataDir+'Asn_Ship','Asn_Ship','SH')
IF llEdiSys
  =gfOpenFile(gcDataDir + 'EDIACPRT' , gcDataDir + 'ACCFACT' , 'SH')
  =gfOpenFile(gcDataDir + 'EDIPH' , gcDataDir + 'PARTNER' , 'SH')
  =gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
  =gfOpenFile(gcDataDir+'Spck_Lin','Spcklins','SH')
  =gfOpenFile(gcDataDir+'StyleUpc','StyleUpc','SH')
  =gfOpenFile(gcDataDir +'CUSTDEPT',gcDataDir+'CUSTDEPT' ,'SH')
  *B606159,1 WAEL 30/06/2002 (Begin) Open the needed files.
  =gfOpenFile(gcDataDir + 'EDIPD' , gcDataDir + 'PARTTRANS', 'SH')
  =gfOpenFile(gcDataDir +'BOL_HDR',gcDataDir+'BOL_HDR' ,'SH')
  =gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')
  =gfOpenFile(gcDataDir +'EdiSv',gcDataDir+'Key' ,'SH')
  *B606159,1 WAEL 30/06/2002 (End)
ENDIF  
IF llCanPrnLb
  =gfOpenFile(gcSysHome+'SYCASNLB','ASNlbl','SH')    
ENDIF

*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Create Temp File
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCrtTmp()
*!*************************************************************
*!
FUNCTION lfCrtTmp

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pack_No'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Order'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Account'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AccName'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Store'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cWareCode'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'ShipVia'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bill_ladg'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Tot_Cart'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Tot_Wght'
laFileStru[lnI,2] = 'N'

*laFileStru[lnI,3] = 8
*laFileStru[lnI,4] = 0
laFileStru[lnI,3] = 13
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Tot_Pcs'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pick_Qty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Order_Qty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0
*B606159,1 WAEL 30/06/2002 (Begin) Add new field to the temp file.
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lStandCtn'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0
*B606159,1 WAEL 30/06/2002 (End)
DIMENSION laIndx[1,2]
laIndx[1,1] = "Pack_No"
laIndx[1,2] = lcPakHdr
=gfCrtTmp(lcPakHdr,@laFileStru,@laIndx)

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pack_No'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'From_Crt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'To_Crt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'No_Cart'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Scale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PrePak'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotOrgOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cZeroQty'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0
*C102112,1 [End]

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pik8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotPik'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0


DIMENSION laIndx[2,2]
laIndx[1,1] = "Pack_No+Style+STR(nOrdLineNo,6)"
laIndx[1,2] = lcPakLin
laIndx[2,1] = "cZeroQty+Pack_No+Style+STR(nOrdLineNo,6)"
laIndx[2,2] = "ZeroQty"

=gfCrtTmp(lcPakLin,@laFileStru,@laIndx)

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PikTkt'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PikDate'
laFileStru[lnI,2] = 'D'
laFileStru[lnI,3] = 0
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Order'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Account'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AccName'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Store'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CReason'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 50
laFileStru[lnI,4] = 0


DIMENSION laIndx[1,2]
laIndx[1,1] = "PikTkt"
laIndx[1,2] = lcRejPkTmp
=gfCrtTmp(lcRejPkTmp,@laFileStru,@laIndx)

SELECT (lcPakLin)
SET RELATION TO 'S' + Scale INTO Scale ADDITITVE
SET RELATION TO Style INTO Style ADDITIVE


*!*************************************************************
*! Name      : lfPackFltr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPackFltr()
*!*************************************************************
*!
FUNCTION lfPackFltr
IF lcPackNo # &lcPakHdr..Pack_No
  =SEEK(&lcPakHdr..Pack_No,lcPakLin)
  lcPackNo = &lcPakHdr..Pack_No
ENDIF
*--

*!*************************************************************
*! Name      : lfAssgnBol
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Assign BOL to The packing list
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAssgnBol()
*!*************************************************************
*!
FUNCTION lfAssgnBol
PARAMETERS lcSeekOrd , lcAccount , lcStore
PRIVATE lcSeekOrd , lcAccount , lcStore

IF llEdiSys
            
  PRIVATE lnActvAlis , lcOrdTag , lcOrdKey
  lnActvAlis = SELECT(0)
  SELECT ORDHDR
  lcOrdTag = ORDER()
  lcOrdKey = EVALUATE(KEY())
  SET ORDER TO ORDHDR
  SELECT (lnActvAlis)
            
  *-- if this Piktkt is in ORDHDR get the ship via
  IF SEEK(lcSeekOrd,'ORDHDR')
    *--Get or Create BOL for this customer,Dist. Center,store,and shipvia
    PRIVATE laFields
    *-- array to hold defulted fields for BOL
    DIMENSION laFields[3,2]
    laFields[1,1] = "cgronhang"
    laFields[1,2] = "N"
    laFields[2,1] = "ctranmthd"
    laFields[2,2] = "M"
    laFields[3,1] = "packtype"
    laFields[3,2] = "CTN25"
    *:B604842,1 SSH [Start] Commented out to get the pack number form ordline
    *lcTitle = "for Packing lst " + &lcPakHdr..Pack_No
    lcTitle = "for Packing lst " + Ordline.Piktkt
    *:B604842,1 SSH [END]
    *B606159,1 WAEL 30/06/2002 (Begin) Update the BOL info.
    *lcBol = lfGetBOL(lcTitle,lcAccount,lcStore,OrdHdr.cWareCode,ORDHDR.Shipvia,'N',"laFields")
    *REPLACE Bill_ladg  WITH lcBol
    *REPLACE cWareCode  WITH OrdHdr.cWareCode
    *REPLACE ShipVia  WITH ORDHDR.Shipvia

    lcShipVia = OrdHdr.ShipVia
    lcDistCtr  = lcStore
    *-- Get the value of Distrepution center From Customer file
    IF SEEK('S'+lcAccount+lcStore,'Customer')
      lcDistCtr  = IIF(EMPTY(Customer.Dist_Ctr),Customer.Store,Customer.Dist_Ctr)
      lcShipVia = IIF(ALLTRIM(lcShipVia)='*',Customer.ShipVia,lcShipVia)
    ENDIF
    =SEEK('A' + lcAccount , 'EDIACPRT') .AND. SEEK(EDIACPRT.cPartCode , 'EDIPH')

    lcBol = lfGetBOL(lcAccount,lcDistCtr,OrdHdr.cWareCode,lcShipVia,IIF(EDIPH.cCrtnType='S','Y','N'),"S",OrdHdr.cDivision)
    REPLACE Bill_ladg WITH lcBol ,;
            cWareCode WITH OrdHdr.cWareCode ,;
            ShipVia   WITH lcShipVia ,;
            lStandCtn WITH EDIPH.cCrtnType='S'
    *B606159,1 WAEL 30/06/2002 (End)
  ENDIF  && end if this Piktkt is in ORDHDR get the ship via

  SELECT ORDHDR
  SET ORDER TO &lcOrdTag
  SEEK lcOrdKey
  SELECT (lnActvAlis)
          
ENDIF

*--end of lfAssgnBol
*!*************************************************************
*! Name      : lfUpdtBOL
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Update bill of leading
*!*************************************************************
*! Calls     : gfAdd_Info
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdtBOL()
*!*************************************************************
*!
FUNCTION lfUpdtBOL
PRIVATE lnActvAlis 

IF llEdiSys
  llBOLL_Opn = !llBOLL_Opn AND gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')

  *B804367,1 ABD Open this file in case this file is close and lfgetBol didn't open it. [Begin]
  llBOLH_Opn = !llBOLH_Opn .AND. !USED('BOL_HDR') .AND. gfOpenFile(gcDataDir+'BOL_HDR',gcDataDir+'BOL_HDR','SH')
  *B804367,1 ABD [End]

  IF !EMPTY(Pack_Hdr.Bill_ladg) AND !SEEK(Pack_Hdr.Bill_ladg+Pack_Hdr.order+Pack_Hdr.Pack_no,'BOL_LIN')
    INSERT INTO BOL_LIN (BOL_NO,ORDER    ,PACK_NO  ) ;
                VALUES (Pack_Hdr.Bill_ladg,Pack_Hdr.order,Pack_Hdr.Pack_no)
  ENDIF
  
  *--update totals of (Weight, cartons ,peices) in BOL_HDR file
  *B804367,1 ABD Check first if this field is not Empty before you seek in the BOL_Hdr File. [Begin]
  *IF SEEK(Pack_Hdr.Bill_ladg,"BOL_HDR")
  IF !EMPTY(Pack_Hdr.Bill_ladg) .AND. SEEK(Pack_Hdr.Bill_ladg,"BOL_HDR")
    *B804367,1 ABD [End]

    lnActvAlis = SELECT(0)
    SELECT BOL_HDR
    REPLACE TOT_WGHT WITH TOT_WGHT + PACK_HDR.Tot_Wght ,;
            TOT_CART WITH TOT_CART + PACK_HDR.Tot_Cart ,;
            TOT_PCS  WITH TOT_PCS  + PACK_HDR.Tot_Pcs
    
    =gfAdd_Info("BOL_HDR")
    SELECT (lnActvAlis)
  ENDIF  

ENDIF
*-- end of lfUpdtBOL.

*!*************************************************************
*! Name      : lfvBol
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : Validation function for BOL
*!*************************************************************
*! Called from : B.O.L [Option Grid]
*!*************************************************************
*! Calls       : lfvBol
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvBol
PRIVATE laBrowArr,lcDistCntr,lnAlias, lcBrFields,lcWareCode,lcStandCtn, lcShipVia,lcacc

*B606159,1 WAEL 30/06/2002 (Begin) Validate the BOL.
lcDistCntr = m.Store
IF SEEK('S'+m.Account+m.Store,'Customer') AND !EMPTY(Customer.Dist_Ctr)
  lcDistCntr = Customer.Dist_Ctr
ENDIF
SET ORDER TO TAG BOLACC IN BOL_HDR
IF !SEEK(m.Account+lcBol,'BOL_HDR') OR ;
  BOL_HDR.Store+BOL_HDR.W_Code+BOL_HDR.ShipVia+BOL_HDR.StanCarton <> ;
  lcDistCntr+&lcPakHdr..cWareCode+m.ShipVia+IIF(&lcPakHdr..lStandCtn,'Y','N') OR BOL_HDR.Status = "C",;

  lnAlias = SELECT()
  SELECT BOL_HDR

  lcBrFields=[bol_no:H="Bill of lading",Account,Store,ship_date:H="Shipping Date"]
  lcWareCode = &lcPakHdr..cWareCode
  lcStandCtn = IIF(&lcPakHdr..lStandCtn,'Y','N')
  lcShipVia = m.ShipVia
  lcacc = m.account
  DECLARE laBrowArr[1]
  laBrowArr=''
  lcBol = IIF(ARIABROW("lcacc"+' FOR Store+W_Code+ShipVia+StanCarton =lcDistCntr+lcWareCode+lcShipVia+lcStandCtn AND Status <> "C"',;
                  "BOLs",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','BOL_NO','laBrowArr'),;
                   BOl_HDr.BOL_NO,lcBol)
  SELECT (lnAlias)
ENDIF
SET ORDER TO TAG BOL_HDR IN BOL_HDR
*IF !MDOWN() AND !EMPTY(lcBol)
*  IF SEEK("O" + m.Order,'ORDHDR')
*    *:B604842,1 SSH Fix bug of syntax error
*    *--Get or Create BOL for this customer,Dist. Center,store,and shipvia
*    PRIVATE laFields
*    *-- array to hold defulted fields for BOL
*    DIMENSION laFields[3,2]
*    laFields[1,1] = "cgronhang"
*    laFields[1,2] = "N"
*    laFields[2,1] = "ctranmthd"
*    laFields[2,2] = "M"
*    laFields[3,1] = "packtype"
*    laFields[3,2] = "CTN25"
*    *:B604842,1 SSH Fix bug of syntax error
*    lcBol = lfGetBOL("",m.Account,m.Store,OrdHdr.cWareCode,ORDHDR.Shipvia,'N',"laFields",.F.,lcBol)
*    lcBol = IIF(EMPTY(lcBol),lcOldValue,lcBol)
*  ENDIF  && end if this Piktkt is in ORDHDR get the ship via
*ENDIF
*B606159,1 WAEL 30/06/2002 (End)
IF lcBol <> lcOldValue
  REPLACE &lcPakHdr..Bill_Ladg WITH lcBol
ENDIF  

*-- End Of lfvBol

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 18/04/2001
*! Purpose   : When function for BOL
*!*************************************************************
*! Called from : B.O.L [Option Grid]
*!*************************************************************
*! Calls       : lfWOldVal
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfWOldVal

lcOldValue = EVALUATE(SYS(18))
*-- end of lfWOldVal.

*!*************************************************************
*! Name      : lfClcult
*! Developer : Mohamed Shokry (MHM)
*! Date      : 23/04/2001
*! Purpose   : Calculate and distrebute Cartons 
*!*************************************************************
*! Called from : =lfOptGrid()
*!*************************************************************
*! Passed Parameters  : lnNoCrt   --->Number Of Cartons
*!                      lnFrmCrt  ---> From Carton
*!                      lnToCrt   ---> To Carton
*!*************************************************************
*! Calls       : lfUpdTmp
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfClcult
PARAMETER lnNoCrt , lnFrmCrt , lnToCrt

PRIVATE lnNCartn , lnRemend ,lnDeffPc ,lnOldNo ,lnFlag ,lnCurAlias
STORE 0 TO lnNCartn , lnRemend ,lnDeffPc ,lnOldNo ,lnFlag
STORE .T. To llChckCart
DIMENSION laScalPr[9]
DIMENSION laScalPr2[9]
STORE 0 TO laScalPr,laScalPr2
* lnNCartn   ----------> Variable to Get Total Qty in Carton
* lnOldNo    ----------> Variable to Gat old Number of Carton
* lnCompCrt   ----------> variable to get completed carton
* lnFrcCrt   ----------> variable to get fraction in last carton
* llChckCart ----------> variable to check if there is fraction in cartons.
lnCompCrt  = INT((OrdLine.TotPik * Style.nstyweight) / lnRealWght)
lnFrcCrt   = MOD((OrdLine.TotPik * Style.nstyweight) / lnRealWght,1)
*--if it is not equal one CARTON
lnFlag   = 1

IF lnFrcCrt <> 0 AND lnCompCrt  > 0
  llChckCart = .T.
ELSE
  llChckCart = .F.
ENDIF

IF lnCompCrt  = 0
  lnCompCrt   = 1
  llChckCart = .F.
  *--- Pack in the last carton.
  lnFlag     = 2
  lnLastCrt = OrdLine.TotPik
ENDIF

*--calculate Total carton and Distrebute it as Equation
*--Total Weight / Break Weight = Total Cartons

*--lcOldPr : Old PrePack
*lcOldPr = Scale.prepak

*--llChngPrpk : variable to check if prepack changed

IF llChngPrpk
  RETURN .F.
ENDIF

lnOldNo  = lnNoCrt
FOR lnCount = 1 TO 8
  lnCont = STR(lnCount,1)
  laScalPr[lnCount] = SCALE.Pp&lnCont
ENDFOR
laScalPr[9] = SCALE.PpTot
*--- There is freactoin
IF llChckCart
  *lnFrmCrt = IIF(lnFrmCrt = 1 ,lnFrmCrt,lnFrmCrt - 1)
  lnFrmCrt = IIF(lnFrmCrt = 1 ,lnFrmCrt,lnFrmCrt )
  lnToCrt  = lnToCrt -1
  lnNoCrt  = lnToCrt -1
ENDIF  
=lfUpdTmp(lnNoCrt , lnFrmCrt , lnToCrt,lnFlag)
IF llChckCart
  lnFrmCrt = lnToCrt +1
  lnToCrt  = lnToCrt +1
  lnNoCrt  = lnToCrt +1
  *--- Last Carton (piecies) = Total to Pack - (Total PrePack * Prepack PerCarton * No. of Completed Carton.)
  lnLastCrt = OrdLine.TotPik - laScalPr[9]*lnCrtNo*lnCompCrt
  lnFlag   = 2
  llCheck2 = .T.
  =lfUpdTmp(lnNoCrt , lnFrmCrt , lnToCrt,lnFlag)
ENDIF
*B804334,1 store from carton to use again[start]
lnStorFrm = lnToCrt +1
*B804334,1 [end]

RETURN .T.
*!*************************************************************
*! Name      : lfUpdTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 23/04/2001
*! Purpose   : Update Temp file
*!*************************************************************
*! Called from : =lfClcult()
*!*************************************************************
*! Passed Parameters  : lnNoCrt   ---> Number Of Cartons
*!                      lnFrmCrt  ---> From Carton
*!                      lnToCrt   ---> To Carton
*!                      lnFlag    ---> Flag to control Update
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfUpdTmp
PARAMETER lnNoCrt , lnFrmCrt , lnToCrt ,lnFlag

SELECT (lcPakLin)
APPEND BLANK
REPLACE Pack_No    WITH OrdLine.PikTkt  ,;
        No_Cart    WITH lnNoCrt         ,;
        From_Crt   WITH lnFrmCrt        ,;
        To_Crt     WITH lnToCrt         ,;
        Style      WITH OrdLine.Style   ,;
        nOrdLineNo WITH OrdLine.LineNo  ,;
        Scale      WITH OrdLine.Scale   ,;
        PrePak     WITH OrdLine.PrePak  ,;
        OrgOrd1    WITH OrdLine.Qty1    ,;
        OrgOrd2    WITH OrdLine.Qty2    ,;
        OrgOrd3    WITH OrdLine.Qty3    ,;
        OrgOrd4    WITH OrdLine.Qty4    ,;
        OrgOrd5    WITH OrdLine.Qty5    ,;
        OrgOrd6    WITH OrdLine.Qty6    ,;
        OrgOrd7    WITH OrdLine.Qty7    ,;
        OrgOrd8    WITH OrdLine.Qty8    ,;
        TotOrgOrd  WITH OrdLine.TotQty  ,;
        Ord1       WITH OrdLine.Qty1    ,;
        Ord2       WITH OrdLine.Qty2    ,;
        Ord3       WITH OrdLine.Qty3    ,;
        Ord4       WITH OrdLine.Qty4    ,;
        Ord5       WITH OrdLine.Qty5    ,;
        Ord6       WITH OrdLine.Qty6    ,;
        Ord7       WITH OrdLine.Qty7    ,;
        Ord8       WITH OrdLine.Qty8    ,;
        TotOrd     WITH OrdLine.TotQty  ,;
        Pik1       WITH OrdLine.Pik1    ,;
        Pik2       WITH OrdLine.Pik2    ,;
        Pik3       WITH OrdLine.Pik3    ,;
        Pik4       WITH OrdLine.Pik4    ,;
        Pik5       WITH OrdLine.Pik5    ,;
        Pik6       WITH OrdLine.Pik6    ,;
        Pik7       WITH OrdLine.Pik7    ,;
        Pik8       WITH OrdLine.Pik8    ,;
        TotPik     WITH OrdLine.TotPik  ,;
        cZeroQty   WITH 'N'
        =lfUpdQty(lnFlag)
*-- End lfUpdTmp
*!*************************************************************
*! Name      : lfUpdQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Update Qty file
*!*************************************************************
*! Called from : =lfUpdTmp()
*!*************************************************************
*! Passed Parameters  : lnFlag  ---> FLAG 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfUpdQty
PARAMETER  lnFlag
PRIVATE lnQty1 , lnQty2, lnQty3, lnQty4, lnQty5, lnQty6, lnQty7, lnQty8
PRIVATE lnNCrton
STORE 0 TO lnTotQty 

*--we will get prepak qty first and applay it for all code 
*--when we reach to it we calculate 

FOR lnCountr = 1 To 8 
  lnCotr= STR(lnCountr,1)
  IF lnFlag = 1
    lnQty&lnCotr = IIF(OrdLine.Pik&lnCotr <> 0 ,laScalPr[lnCountr]*lnCrtNo,0)
    lnTotQty = lnTotQty +lnQty&lnCotr
  ELSE
    IF llCheck2
      lnQty&lnCotr = IIF(OrdLine.Pik&lnCotr <> 0 ,OrdLine.Pik&lnCotr - laScalPr[lnCountr]*lnCrtNo*lnCompCrt,0)
    ELSE
      *:B804346,1 SSH Fix bug of Print -ve qty.
      *lnQty&lnCotr = IIF(OrdLine.Pik&lnCotr <> 0 ,laScalPr[lnCountr]*(lnLastCrt/laScalPr[9]),0)
       lnQty&lnCotr = IIF(OrdLine.Pik&lnCotr <> 0 .AND. laScalPr[9] <>0,laScalPr[lnCountr]*(lnLastCrt/laScalPr[9]),0)
      *:B804346,1 SSH(END)
    ENDIF
    lnTotQty = lnTotQty +lnQty&lnCotr
  ENDIF
ENDFOR
*:B804346,1 SSH Fix bug of Print -ve qty.
*REPLACE Qty1       WITH lnQty1 ,;
         Qty2       WITH lnQty2 ,;
         Qty3       WITH lnQty3 ,;
         Qty4       WITH lnQty4 ,;
         Qty5       WITH lnQty5 ,;
         Qty6       WITH lnQty6 ,;
         Qty7       WITH lnQty7,;
         Qty8       WITH lnQty8,;
         TotQty     WITH lnTotQty  
FOR lnTInd = 1 TO 8
  lcTInd = STR(lnTInd,1)
  REPLACE Qty&lcTInd WITH ABS(lnQty&lcTInd)
ENDFOR
REPLACE TotQty WITH ABS(lnTotQty)
*:B804346,1 SSH (END)
llCheck2 = .F.

*!*************************************************************
*! Name      : lfvLblInfo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Parameter : lnCarton : May be one of the following
*!           :         1- Carton No.
*!           :         2- Array or string of cartons
*!           :         3- Null for all cartons
*!*************************************************************
*! Example   : =lfvLblInfo(lnCarton)
*!*************************************************************
*!
FUNCTION lfvLblInfo
PARAMETERS lnCarton, llPrint
PRIVATE lnCarton , lcUccVer , laAcShipTo , laDcShipTo , laWareAddr ,;
        lcStyle,lcColor,lcSize, llPrint

DIMENSION laAcShipTo[5] , laDcShipTo[5] , laWareAddr[5]
STORE "" TO laAcShipTo , laDcShipTo , laWareAddr , lcStyle , lcColor , lcSize
=lfGetStySz()

m.bol_no   = lcBol
m.Cart_No  = lnCarton

=SEEK(lcWareCode,'Warehous')
m.VND_NAME  = Warehous.cDesc
m.VND_ADDR1 = Warehous.cAddress1
m.VND_ADDR2 = Warehous.cAddress2
m.VND_CITY  = Warehous.cAddress3
m.VND_STATE = Warehous.cAddress4
m.VND_ZIP   = Warehous.cAddress5

*-- Get Customer information [Begin]
lcStore = Pack_Hdr.Store

SELECT CUSTOMER
=SEEK(IIF(EMPTY(lcStore),'M'+PACK_HDR.account,'S'+PACK_HDR.account+PACK_HDR.store),'Customer')
lcDistCtr = Customer.Dist_Ctr 

STORE lcStore            TO m.cStStore,m.STORE
STORE Customer.StName    TO m.cStName ,m.SHP_NAME
STORE Customer.cAddress1 TO m.cStAddr1,m.SHP_ADDR1
STORE Customer.cAddress2 TO m.cStAddr2,m.SHP_ADDR2
STORE Customer.cAddress3 TO m.cStCity ,m.SHP_CITY
STORE Customer.cAddress4 TO m.cStState,m.SHP_STATE
STORE Customer.cAddress5 TO m.cStZip  ,m.SHP_ZIP

*-- Get Customer information [End  ]

*-- Get Distribution Center information [Begin]

=SEEK('M'+PACK_HDR.account,'Customer')
m.Int_Vend = Customer.cCusVend
IF SEEK('S'+PACK_HDR.account+lcDistCtr,'Customer')
  m.STORE     = lcDistCtr 
  m.SHP_NAME  = Customer.STName
  m.SHP_ADDR1 = Customer.cAddress1
  m.SHP_ADDR2 = Customer.cAddress2
  m.SHP_CITY  = Customer.cAddress3
  m.SHP_STATE = Customer.cAddress4
  m.SHP_ZIP   = Customer.cAddress5
ENDIF

*-- Get Distribution Center information [End  ]
m.ShipVia = Customer.ShipVia
lcwrname  = lcwarecode

*-- Get Carton information [Begin]
SELECT ORDHDR
SET ORDER TO ORDHDR
=SEEK('O'+PACK_HDR.Order)
lcOrStore   = OrdHdr.Store

lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
*-- remove the EDI condition from printing carton labels [Begin]
lcUccVer = ''
IF llEdiSys
  lcUccVer = IIF(lnDrctTo=1,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
ENDIF
*-- remove the EDI condition from printing carton labels [End  ]

lcUccVer    = IIF(EMPTY(lcUccVer),'XXX',lcUccVer)
m.CUSTPO    = OrdHdr.CustPo
m.DEPT      = OrdHdr.Dept

m.MANUF_ID  = PADL(ALLTRIM(lcManufId),7,'0')

m.UCC9      = RIGHT(PADL(ALLTRIM(PACK_HDR.PACK_NO),6,'0'),5)+PADL(lnCarton,4,'0')
m.UCC_CHECK = lfCheckNo('000'+m.MANUF_ID+m.Ucc9)
*-- These fields has to be filled 
m.Note1     = OrdHdr.Note1
m.Note2     = OrdHdr.Note2
m.Int_Vend  = IIF(EMPTY(ORDHDR.INT_VEND),m.Int_Vend,ORDHDR.INT_VEND)
m.Cancelled = OrdHdr.Complete


m.ASN_VER   = lcUccVer  
m.EVENT_COD = OrdHdr.Event_Cod


IF (lcFree_Clr!="C") OR EMPTY(lcColor) OR ("MIXED" $ lcColor)
  DIMENSION laCodDesc[1,3]
  laCodDesc = ""
  laCodDesc[1,1] = m.ShipVia
  laCodDesc[1,2] = "SHIPVIA"
  m.cClrDesc = lcColor
ELSE
  DIMENSION laCodDesc[2,3]
  laCodDesc = ""
  laCodDesc[1,1] = m.ShipVia
  laCodDesc[2,1] = SUBSTR(&lcCtnDtl..Style,lnNonMajSt,lnColorLen)
  laCodDesc[1,2] = "SHIPVIA"
  laCodDesc[2,2] = "COLOR"
  =gfCodDes(@laCodDesc)
  m.cClrDesc = laCodDesc[2,1]
ENDIF

=gfCodDes(@laCodDesc)
m.PACK_NO   = PACK_HDR.PACK_NO
m.Carrier   = laCodDesc[1,3]
m.Bol_No    = lcBol
m.Style     = lcStyle
*-- Get Carton information [End  ]

=SEEK(STR(m.Cart_No,4),lcctnhdr)

m.TotQty    = &lcCtnHdr..TotPcs
m.cSizeDesc = lcSize

*-- Get No. of Cartons.
lcDetOrder = ORDER(lcCtnHdr)
USE (gcWorkDir+lcCtnHdr) ORDER (lcDetOrder) IN 0 AGAIN ALIAS (lcTmpDetFl)
GO BOTTOM IN (lcTmpDetFl)
m.Cartons = &lcTmpDetFl..Cart_No
USE IN (lcTmpDetFl)

IF SEEK(STR(m.CART_NO,6),lcTmAsnShp)
  SELECT (lcTmAsnShp)
  GATHER MEMVAR MEMO
ELSE
  INSERT INTO (lcTmAsnShp) FROM MEMVAR
ENDIF
IF llPrint
  =lfPrintLbl(lcUccVer)
  IF llntFound
    RETURN
  ENDIF
ENDIF
SELECT (lcPakLin)

*-- End Of lfvLblInfo  
*!*************************************************************
*! Name      : lfPrintLbl
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : 
*!*************************************************************
*!C101735,1
*!
FUNCTION lfPrintLbl
PARAMETERS lcVersion
PRIVATE lnCurAlias, lnHandle, lcOutFile, lcString, lcData
lnCurAlias = SELECT(0)
WAIT 'Generating Shipping Label...Please stand by....' WINDOW NOWAIT
lcString  = SPACE(40)

SELECT SYCASNLB
IF !SEEK(lcVersion+'H') .AND. gfModalGen('INM44068B00000','DIALOG' )=1
  RETURN
  llntfound = .T.
ENDIF

*B606159,1 ASH 06/30/2002 (Begin) In Case of sending the label to None save it to a txt file.
*lcOutFile = gcWorkDir+lcAsnLabel+".TXT"
lcOutFile = IIF(lcSndPort='NONE',gcWorkDir+"AsnLabel.TXT",gcWorkDir+lcAsnLabel+".TXT")
*B606159,1 ASH 06/30/2002 (End)
lnHandle  = FCREATE(lcOutFile,0)
=FSEEK(lnHandle,0,2)

SCAN WHILE 	cVer+cEdiType = lcVersion+'H'
  STORE DATA TO lcData
  lcString = &lcData 
  =FPUTS(lnHandle,lcString)
ENDSCAN	

lcString = SPACE(3)
=FPUTS(lnHandle,lcString)

*--Load Size description,quantity and SKU
SELECT (lcTmAsnShp)
SCATTER MEMVAR
=gfRltFld(ORDHDR.cDivision,@laDRltFld,'CDIVISION ')
m.DivLName = lcDivLName
=SEEK(OrdHdr.Account+OrdHdr.Dept,'CUSTDEPT')
m.DeptDesc = CustDept.cDeptDesc
=SEEK(ALLTRIM(style),'Style')
m.StyDesc = Style.Desc

*-- Get carton information from carton details temp file
=SEEK(STR(&lcTmAsnShp..cart_no,4),lcCtnHdr)
m.Weight = &lcCtnHdr..TotWgh

SELECT (lcTmAsnShp)
m.Date     = gdSysDate
m.Order    = PACK_HDR.ORDER
m.Account  = PACK_HDR.account
m.Pattern = Style.Pattern

STORE '' TO m.SizeDesc1,m.SizeDesc2,m.SizeDesc3,m.SizeDesc4,m.SizeDesc5,m.SizeDesc6,m.SizeDesc7,m.SizeDesc8
STORE '' TO m.SizeSku1,m.Sizesku2,m.Sizesku3,m.Sizesku4,m.Sizesku5,m.Sizesku6,m.Sizesku7,m.Sizesku8
STORE 0  TO m.SizeQty1,m.SizeQty2,m.SizeQty3,m.SizeQty4,m.SizeQty5,m.SizeQty6,m.SizeQty7,m.SizeQty8


*-- Remove temp carton details relation
SELECT(lcPakLin)
lnStyRec = RECNO(lcPakLin)
lnSzeRec = RECNO(lcCartonSz)
SET RELATION TO 

*--Take into consideration styles with exteded size scale while printning labels
IF llExtSizSc
  SELECT (lcPakLin)
  IF SEEK(STR(&lcTmAsnShp..cart_no,4)+ALLTRIM(&lcTmAsnShp..Style))
    lnSizeCount = 0
    lcCurrSty = SUBSTR(Style,1,lnSizePos-IIF(EMPTY(lcSizeSep),1,2))
    SCAN REST WHILE STR(Cart_No,4)+Style+STR(nOrdLineNo,6) = STR(&lcTmAsnShp..cart_no,4)+lcCurrSty ;
              AND lnSizeCount < 8 
      =SEEK('S'+SUBSTR(Style,lnSizePos,lnSizeLen),'Scale')
      FOR lnCount = 1 TO Scale.Cnt
        lcCount = STR(lnCount,1)
        IF &lcPakLin..Qty&lcCount <> 0
          lnSizeCount = lnSizeCount + 1
          lcSizeCount = STR(lnSizeCount,1)
          m.SizeDesc&lcSizeCount = ALLTRIM(Scale.Sz&lcCount)
          m.SizeQty&lcSizeCount  = &lcPakLin..Qty&lcCount
          SELECT SPCK_LIN
          =SEEK('S'+PACK_HDR.account+&lcPakLin..style)
          LOCATE REST WHILE type+account+style+pack_id = 'S'+PACK_HDR.account+&lcPakLin..style FOR QTY&lcCount = 1
          IF FOUND()
            m.SizeSku&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
          ENDIF
        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
ELSE
  SELECT (lcTmAsnShp)
  =SEEK('S'+Style.Scale,'Scale')
  IF SEEK(STR(Cart_no,4)+ALLTRIM(Style),lcPakLin)
    FOR lnCount = 1 TO Scale.Cnt
      lcCount = STR(lnCount,1)
      IF &lcPakLin..Qty&lcCount <> 0
        m.SizeDesc&lcCount = ALLTRIM(Scale.Sz&lcCount)
        m.SizeQty&lcCount  = &lcPakLin..Qty&lcCount
        SELECT SPCK_LIN
        =SEEK('S'+PACK_HDR.account+&lcPakLin..style)
        LOCATE REST WHILE type+account+style+pack_id = 'S'+PACK_HDR.account+&lcPakLin..style FOR QTY&lcCount = 1
        IF FOUND()
          m.SizeSku&lcCount = ALLTRIM(Spck_Lin.Pack_Id)
        ENDIF
      ENDIF
    ENDFOR  
  ENDIF
ENDIF
SELECT (lcTmAsnShp)

TMP_ADDR = ALLTRIM(VND_CITY) + ",  " +ALLTRIM(VND_STATE) + "   " + ALLTRIM(VND_ZIP)
SELECT SYCASNLB
=SEEK(lcVersion+'L')
SCAN WHILE cVer+cEdiType= lcVersion+'L'
  STORE DATA TO lcData
  lcString = &lcData 
  =FPUTS(lnHandle,lcString)
ENDSCAN
lcString = SPACE(3)
=FPUTS(lnHandle,lcString)

* HS 05/06/2001 Print the detailed label, if needed [Begin]
IF llDetLabel
  PRIVATE lnChoice , llPrintLbl , lnMajorLen , lnClrLen , lnClrPos , llUseColor ,;
          lcUPCStyle , lcGenColor , lcGenSty
  
  IF EMPTY(lcDetLbAll)
    lnChoice = gfModalGen('TRM44105B40016' , 'DIALOG')
    DO CASE
      CASE lnChoice = 1
        llPrintLbl = .T.
      CASE lnChoice = 2
        llPrintLbl = .T.
        lcDetLbAll = "Y"
      CASE lnChoice = 3
        llPrintLbl = .F.
      CASE lnChoice = 4
        llPrintLbl = .F.
        lcDetLbAll = "N"
    ENDCASE
  ELSE
    llPrintLbl = (lcDetLbAll = "Y")
  ENDIF
  
  IF llPrintLbl
    STORE '' TO MDSTYLE1 , MDSTYLE2 , MDSTYLE3 , MDSTYLE4 , MDSTYLE5 ,;
                MDSTYLE6 , MDSTYLE7 , MDSTYLE8 , MDSTYLE9 , MDSTYLE10,;
                MDSTYLE11, MDSTYLE12, MDSTYLE13, MDSTYLE14, MDSTYLE15,;
                MDSTYLE16, MDSTYLE17, MDSTYLE18, MDSTYLE19, MDSTYLE20
    
    STORE '' TO MDSTYMAJ1 , MDSTYMAJ2 , MDSTYMAJ3 , MDSTYMAJ4 , MDSTYMAJ5 ,;
                MDSTYMAJ6 , MDSTYMAJ7 , MDSTYMAJ8 , MDSTYMAJ9 , MDSTYMAJ10,;
                MDSTYMAJ11, MDSTYMAJ12, MDSTYMAJ13, MDSTYMAJ14, MDSTYMAJ15,;
                MDSTYMAJ16, MDSTYMAJ17, MDSTYMAJ18, MDSTYMAJ19, MDSTYMAJ20
    
    STORE '' TO MDCOLOR1 , MDCOLOR2 , MDCOLOR3 , MDCOLOR4 , MDCOLOR5 ,;
                MDCOLOR6 , MDCOLOR7 , MDCOLOR8 , MDCOLOR9 , MDCOLOR10,;
                MDCOLOR11, MDCOLOR12, MDCOLOR13, MDCOLOR14, MDCOLOR15,;
                MDCOLOR16, MDCOLOR17, MDCOLOR18, MDCOLOR19, MDCOLOR20
    
    STORE '' TO MDSKU1 , MDSKU2 , MDSKU3 , MDSKU4 , MDSKU5 ,;
                MDSKU6 , MDSKU7 , MDSKU8 , MDSKU9 , MDSKU10,;
                MDSKU11, MDSKU12, MDSKU13, MDSKU14, MDSKU15,;
                MDSKU16, MDSKU17, MDSKU18, MDSKU19, MDSKU20
    
    STORE '' TO MDSTYUPC1 , MDSTYUPC2 , MDSTYUPC3 , MDSTYUPC4 , MDSTYUPC5 ,;
                MDSTYUPC6 , MDSTYUPC7 , MDSTYUPC8 , MDSTYUPC9 , MDSTYUPC10,;
                MDSTYUPC11, MDSTYUPC12, MDSTYUPC13, MDSTYUPC14, MDSTYUPC15,;
                MDSTYUPC16, MDSTYUPC17, MDSTYUPC18, MDSTYUPC19, MDSTYUPC20
    
    STORE '' TO MDSIZDES1 , MDSIZDES2 , MDSIZDES3 , MDSIZDES4 , MDSIZDES5 ,;
                MDSIZDES6 , MDSIZDES7 , MDSIZDES8 , MDSIZDES9 , MDSIZDES10,;
                MDSIZDES11, MDSIZDES12, MDSIZDES13, MDSIZDES14, MDSIZDES15,;
                MDSIZDES16, MDSIZDES17, MDSIZDES18, MDSIZDES19, MDSIZDES20
    
    STORE 0  TO MDQTY1 , MDQTY2 , MDQTY3 , MDQTY4 , MDQTY5 ,;
                MDQTY6 , MDQTY7 , MDQTY8 , MDQTY9 , MDQTY10,;
                MDQTY11, MDQTY12, MDQTY13, MDQTY14, MDQTY15,;
                MDQTY16, MDQTY17, MDQTY18, MDQTY19, MDQTY20
    
    *-- Get major length
    lnMajorLen = LEN(lcMask)
    
    *-- Get color start position and length
    STORE 0   TO lnClrLen , lnClrPos
    STORE .F. TO llUseColor
    DECLARE laItemSeg[1]
    =gfItemMask(@laItemSeg)
    FOR lnCount = 1 TO ALEN(laItemSeg , 1)
      IF laItemSeg[lnCount,1] = 'C'
        llUseColor = .T.
        lnClrLen   = LEN(laItemSeg[lnCount,3])
        lnClrPos   = laItemSeg[lnCount,4]
        EXIT
      ENDIF
    ENDFOR
    
    lcGenColor = PADR(gfGetMemVar("MCLRASSCOD") , 6)
    SELECT (lcCtnDtl)
    IF SEEK(STR(&lcTmAsnShp..Cart_No , 4))
      lnSizeCount = 0
      SCAN REST WHILE STR(Cart_No , 4) = STR(&lcTmAsnShp..Cart_No , 4) .AND. lnSizeCount < 20
        =SEEK(&lcCtnDtl..Style , 'STYLE')
        =SEEK('S' + Style.Scale , 'Scale')
        FOR lnCount = 1 TO Scale.Cnt
          lcCount = STR(lnCount , 1)
          IF &lcCtnDtl..Qty&lcCount <> 0
            lnSizeCount = lnSizeCount + 1
            *B804367,1 ABD In case the lnsizecount became more than 9 , equal 10 for exp , in this case
            *B804367,1 ABD If you get the Str(10,1) , it will Equal '*' and then the became lcSizeCount = '*'
            *B804367,1 ABD The Program will scan for 20 time so the program should take 2 char 
            *B804367,1 ABD In the STR() function. [Begin]
            *lcSizeCount = STR(lnSizeCount , 1)
            lcSizeCount = ALLTRIM(STR(lnSizeCount , 2))
            *B804367,1 ABD [End]

            
            MDStyle&lcSizeCount  = &lcCtnDtl..Style
            MDStyMaj&lcSizeCount = LEFT(&lcCtnDtl..Style , lnMajorLen)
            MDSizDes&lcSizeCount = ALLTRIM(Scale.Sz&lcCount)
            MDQty&lcSizeCount    = &lcCtnDtl..Qty&lcCount
            IF llUseColor
              MDColor&lcSizeCount = SUBSTR(&lcCtnDtl..Style , lnClrPos , lnClrLen)
            ENDIF
            IF llUPCInst
              lcUPCStyle = &lcCtnDtl..Style
              IF SEEK(lcUPCStyle + lcCount , "StyleUPC")
                MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
              ELSE
                lcGenSty = SUBSTR(lcUPCStyle , 1 , lnClrPos - 1) + lcGenColor +;
                           SUBSTR(lcUPCStyle , lnClrPos + lnCLrLen)
                
                IF SEEK(lcGenSty + lcCount , "StyleUPC")
                  MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 +;
                                         StyleUPC.cUPCNum3
                ENDIF
              ENDIF
            ENDIF
            
            SELECT SPCK_LIN
            =SEEK('S' + PACK_HDR.account + &lcCtnDtl..Style)
            LOCATE REST;
                  WHILE Type + Account + Style + Pack_ID = 'S' + PACK_HDR.account + &lcCtnDtl..Style;
                    FOR QTY&lcCount = 1
            
            IF FOUND()
              MDSKU&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
            ENDIF
          ENDIF
        ENDFOR
      ENDSCAN
    ENDIF
    
    SELECT SYCASNLB
    =SEEK("XX1" + 'H')
    SCAN WHILE cVer + cEdiType = "XX1" + 'H'
      STORE DATA TO lcData
      lcString = &lcData 
      =FPUTS(lnHandle,lcString)
    ENDSCAN	
    lcString = SPACE(3)
    =FPUTS(lnHandle,lcString)
    
    =SEEK("XX1" + 'L')
    SCAN WHILE cVer + cEdiType = "XX1" + 'L'
      STORE DATA TO lcData
      lcString = &lcData 
      =FPUTS(lnHandle,lcString)
    ENDSCAN
  ENDIF
ENDIF
*HS 05/06/2001 Print the detailed label, if needed [End]

=FCLOSE(lnHandle)
WAIT 'Sending Shipping Labels to Printer...Please stand by....' WINDOW NOWAIT
*! MODE COM1:24,N,8,1,P >>null
*! MODE LPT1:=COM1: >>null
*! PRINT /D:LPT1 /S:1 >>null
lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
!/N0 &lcCommand 

WAIT CLEAR

*-- Restore temp carton details relation
SELECT (lcPakLin)
SET RELATION TO '' INTO (lcCartonSz)
SET SKIP TO (lcCartonSz)
IF lnStyRec <= RECCOUNT(lcPakLin) 
  GOTO lnStyRec
  SKIP lnSzeRec-1
ENDIF
= RLOCK(lcPakLin) 
UNLOCk IN (lcPakLin)

SELECT(lnCurAlias)
RETURN
*-- End Of Function lfPrintLbl 

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfEvalSegs()
*!*************************************************************
*!
FUNCTION lfEvalSegs

lcNonMajPi = ""

  lnMajSeg = gfItemMask('SM')  && No. of major segments.
  lnMajLen = LEN(gfItemMask('PM'))
  
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  = gfItemMask(@laMajSegs)

  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]
      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
      lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                   laMajSegs[lnI,3],;
                   lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    ENDIF                     
    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

  ENDFOR    && end Loop Around Non Major elements.

  lnColorLen = LEN(lcNonMajPi)

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfCheckNo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Algorithm to Compute the modulus-10 Check digit for the 
*!             UCC 128 code
*!*************************************************************
*! Parameters: lcUccNo : UCC number without check digit
*!*************************************************************
*! Returns   : UCC Check digit
*!*************************************************************
*! Example   :  =lfCheckNo('1234567890123456789')
*!*************************************************************
*!
FUNCTION lfCheckNo
PARAMETER lcUccNo
PRIVATE lnChkDigit,lnSumOdd,lnSumEven,lnCount

STORE 0 TO lnSumOdd,lnSumEven,lnChkDigit
FOR lnCount = 1 TO 9
  lnSumOdd  = lnSumOdd + VAL(SUBSTR(lcUccNo,lnCount*2-1,1))
  lnSumEven = lnSumEven+ VAL(SUBSTR(lcUccNo,lnCount*2,1))
ENDFOR
lnSumOdd   = lnSumOdd + VAL(SUBSTR(lcUccNo,19,1))
lnChkDigit = MOD(lnSumOdd*3+lnSumEven,10)
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))
*-- end of lfCheckNo.

*!*************************************************************
*! Name      : lfGetStySz
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Clculatre the style size.
*!*************************************************************
*! Example     : = lfGetStySz()
*!*************************************************************
*!
FUNCTION lfGetStySz
PRIVATE lcCurrAlis , lnRecPoint , lnDetPoint

*-- Save current setting
lcCurrAlis = SELECT (0)

SELECT (lcPakLin)
lnDetPoint = RECNO(lcCartonSz)
SEEK STR(lncarton,4)
lcStyle  = SUBSTR(Style,1,lnMajLen)

IF lcFree_Clr="C"
  lcColor  = SUBSTR(Style,lnNonMajSt,lnColorLen)
ELSE
  lcColor  = SPACE(6)
ENDIF
  
lcUpdSty = Style


SELECT (lcCtnDtl)
lnRecPoint = RECNO()

lcSize = ''
FOR lnSzNo = 1 TO 8
  lcSzNo = STR(lnSzNo,1)
  IF Br&lcSzNO
    lcSize = Size&lcSzNo
    EXIT 
  ENDIF  
ENDFOR

SCAN REST WHILE STR(cart_no,4) = STR(lncarton,4)
  lcStyle = IIF(lcStyle<>SUBSTR(Style,1,lnMajLen),SPACE(lnMajLen),lcStyle)
  IF lcFree_Clr="C"
    lcColor = IIF(EMPTY(lcStyle),SPACE(lnColorLen),;
              IIF(lcColor<>SUBSTR(Style,lnNonMajSt,lnColorLen),'MIXED',lcColor))
  ENDIF            

  lcSize1 = ''
  IF !EMPTY(lcStyle)
    FOR lnSzNo1 = 1 TO 8
      lcSzNo = STR(lnSzNo1,1)    
      IF Br&lcSzNO
        lcSize1 = Size&lcSzNo
        lcSize  = IIF(lcSize <>lcSize1,'MIXED',lcSize)
        IF lcSize = 'MIXED'
          EXIT 
        ENDIF  
      ENDIF
    ENDFOR
  ELSE
    lcSize = SPACE(5)  
  ENDIF  

ENDSCAN

*-- Restore Setting.
IF BETWEEN(lnRecPoint,1,RECCOUNT())
  GO lnRecPoint
ENDIF

IF BETWEEN(lnDetPoint,1,RECCOUNT(lcCartonSz))
  GO lnDetPoint IN (lcCartonSz)
ENDIF

SELECT (lcCurrAlis)

*-- end of lfGetStySz.
*!*************************************************************
*! Name      : lfSavCartn
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Saving Cartons  in ASN_SHIP
*!*************************************************************
*! Example   : = lfSavCartn()
*!*************************************************************
*!C101735,1
*!
FUNCTION lfSavCartn
PRIVATE llchoice, lnCartons, lnCarRef , lnActAlias , lcDetOrder , llPrinSel
lnActAlias = SELECT(0)

STORE SPACE (0) TO laSource,laTarget
lnCartons =0
GO BOTTOM IN (lcCtnDtl)
lnCartons = &lcCtnDtl..Cart_No
IF lnCartons <> 0
  DIMENSION laSource[lnCartons] , laTarget[1]
ENDIF
DIMENSION laTarget[1]

lnCarRef = 0
lcDetOrder = ORDER(lcCtnDtl)
SET ORDER TO (lcCtnDtl) In (lcCtnDtl)
*-- Saving code.
FOR lnCarRef = 1 TO lnCartons
  =SEEK(STR(lnCarRef,4),lcCtnDtl) AND lfvLblInfo(lnCarRef)
  laSource[lnCarRef] =  "Carton # " + PADL(ALLTRIM(STR(&lcCtnDtl..CART_NO)),4) 
  IF SEEK(STR(lnCarRef,6),lcTmAsnShp)
    SELECT (lcTmAsnShp)
    SCATTER MEMVAR MEMO
    IF SEEK(&lcTmAsnShp..BOL_NO+&lcTmAsnShp..PACK_NO+STR(&lcTmAsnShp..CART_NO,6),"Asn_Ship")
      SELECT Asn_Ship
      GATHER MEMVAR MEMO
    ELSE
      INSERT INTO Asn_Ship FROM MEMVAR
    ENDIF
  ENDIF  
ENDFOR

llchoice = .T.
IF !llScrPrnLb

  DO WHILE llchoice
    lnCarRef = 0
    lnChoice = gfModalGen("QRM44097B44011","Dialog")
    DO CASE

     *-- Case of YES
     CASE lnChoice=1
       FOR lnCarRef = 1 TO lnCartons
         IF SEEK(STR(lnCarRef,6),lcTmAsnShp)
           SELECT (lcTmAsnShp)
           =lfPrintLbl(ASN_VER)
         ENDIF  
       ENDFOR
     
     *-- Case of Selective
     CASE lnChoice=2
       
       DIMENSION laTarBef[ALEN(laTarget,1)]
       =ACOPY(laTarget,laTarBef)
       = gfMover(@laSource,@laTarget,'Select Carton(s).',.T.,'')
       
       IF ALEN(laTarBef,1) = ALEN(laTarget,1)
         llPrinSel = .F.
         IF !EMPTY(laTarget[1])
           FOR lnJ = 1 TO ALEN(laTarget,1)
             IF ASCAN(laTarBef,laTarget[lnJ]) = 0)
               llPrinSel = .T.
               EXIT
             ENDIF  
           ENDFOR
         ENDIF
         
       ELSE
         llPrinSel = !EMPTY(laTarget[1])
       ENDIF
       
       IF llPrinSel
         FOR lnCarRef = 1 TO ALEN(laTarget,1)
           lnToPrnCrt = VAL(RIGHT(laTarget[lnCarRef],4))
           IF SEEK(STR(lnToPrnCrt,6),lcTmAsnShp)
             SELECT (lcTmAsnShp)
             =lfPrintLbl(ASN_VER)
           ENDIF  
         ENDFOR
       ENDIF
         
     *-- Case of proceed
     CASE lnChoice=3
       llchoice = .F.

    ENDCASE
  ENDDO

ENDIF  
SELECT (lnActAlias)
*-- end of lfSavCartn.

*!*************************************************************
*! Name      : lfCrCtnFiles
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!             (lcOrdLin,lcPckLin)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrCtnFiles()
*!*************************************************************

FUNCTION lfCrCtnFiles

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

DIMENSION laFileStru[7,4]

laFileStru[01,1] = 'Cart_No'
laFileStru[01,2] = 'N'
laFileStru[01,3] = 4
laFileStru[01,4] = 0

laFileStru[02,1] = 'Pal_No'
laFileStru[02,2] = 'N'
laFileStru[02,3] = 4
laFileStru[02,4] = 0

laFileStru[03,1] = 'TotPcs'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 7
laFileStru[03,4] = 0

laFileStru[04,1] = 'TotWgh'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 9
laFileStru[04,4] = 2

laFileStru[05,1] = 'Empty'
laFileStru[05,2] = 'C'
laFileStru[05,3] = 1
laFileStru[05,4] = 0

*B606159,1 WAEL 30/06/2002 (Begin) Add new fields.
laFileStru[06,1] = 'BOL_No'
laFileStru[06,2] = 'C'
laFileStru[06,3] = 6
laFileStru[06,4] = 0

laFileStru[07,1] = 'pack_No'
laFileStru[07,2] = 'C'
laFileStru[07,3] = 6
laFileStru[07,4] = 0
*B606159,1 WAEL 30/06/2002 (End)
DIMENSION laIndx[2,2]
*laIndx[1,1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,1] = "BOL_NO+pack_No+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,2] = lcCtnHdr

laIndx[2,1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[2,2] = "EMPTY"

=gfCrtTmp(lcCtnHdr,@laFileStru,@laIndx)           
SET ORDER TO lcCtnHdr In (lcCtnHdr)


lnI = 1
*B606159,1 WAEL 30/06/2002 (Begin) Add new fields.
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'pack_No'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pack_Id'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 16
laFileStru[lnI,4] = 0
*B606159,1 WAEL 30/06/2002 (End)
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Cart_No'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStep'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PackLineNo'
laFileStru[lnI,2] = 'N'

laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cStatus'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br1'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br2'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br3'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br4'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br5'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br6'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br7'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br8'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

DIMENSION laIndx[2,2]

*B606159,1 WAEL 30/06/2002 (Begin) Change the Index.
*laIndx[1,1] = "STR(Cart_No,4)+Style+STR(nOrdLineNo,6)"
laIndx[1,1] = "pack_No+STR(Cart_No,4)+Style+STR(nOrdLineNo,6)"
*B606159,1 WAEL 30/06/2002 (End)
laIndx[1,2] = lcCtnDtl

laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"

=gfCrtTmp(lcCtnDtl,@laFileStru,@laIndx)           
SET ORDER TO lcCtnDtl In (lcCtnDtl)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lpPortScr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Handle the printing  labels screen setup
*!*************************************************************
*! Example   : = lpPortScr
*!*************************************************************
*!
PROCEDURE lpPortScr
PRIVATE lcOldPort , llOldPrn , llCancel
lcOldPort = lcSndPort
llOldPrn  = llScrPrnLb
llCancel  = .F.
DO (gcScrDir + 'ALOUTPRT.SPX')
IF llCancel
  lcSndPort  = lcOldPort
  llScrPrnLb = llOldPrn
ENDIF
*-- end of lpPortScr.

*!*************************************************************
*! Name      : lfCheckPer
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : 
*!*************************************************************
*! Example   : = lfCheckPer
*!*************************************************************
FUNCTION lfCheckPer
PARAMETER lnChange
PRIVATE llCheck
llCheck = .F.
lnTotPrk = INT(Ordline.Totpik/Scale.PPTot)
*B804334,1 MHM 31/07/2001  in case of less than one carton deal as only one prepack [Start]
lnTotPrk = IIF(lnTotPrk = 0 ,1,lnTotPrk)
*B804334,1 MHM 31/07/2001  [End]
lnFrcQty = MOD(Ordline.Totpik/Scale.PPTot,1)
lnAdd1   = IIF(lnFrcQty<>0,1,0)
*:B804274,1 MHM 02/07/2001  Do not give value to the additional carton when
*:B804274,1 MHM             we back the first carton.

*:B804346,1 SSH Add condition if call from select another prepack.
*lnAdd1   = IIF(lnTotPrk=1,0,lnAdd1)
lnAdd1   = IIF(lnChange="2" .AND. lnTotPrk=0,0,lnAdd1)
*:B804346,1 SSH[END].

*:B804274,1 MHM 02/07/2001  [END]
FOR lnCount = 1 TO 8
  lnCont = STR(lnCount,1)
IF !EMPTY(SCALE.pp&lnCont)
  IF !EMPTY(Scale.Pp&lnCont) .AND. (lnTotPrk <= (Ordline.pik&lnCont/Scale.Pp&lnCont) AND ;
                                   (Ordline.pik&lnCont/Scale.Pp&lnCont) <= (lnTotPrk + lnAdd1) )
    
    IF lnFrcQty <> 0
    *:B804274,1 MHM Check if the total prepack = 0
      IF lnTotPrk <> 0 
        llCheck2 = .T.
      ELSE 
        llCheck2 = .F.
      ENDIF
    *:B804274,1 MHM[END]
    ENDIF
    *--mhm
    IF lnChange = '1'
      IF (lnTotPrk <> (Ordline.pik&lnCont/Scale.Pp&lnCont) AND ;
                                     (Ordline.pik&lnCont/Scale.Pp&lnCont) > (lnTotPrk + lnAdd1))
        llCheck = .T.
      ENDIF    
    ELSE
      IF lnTotPrk = (Ordline.pik&lnCont/Scale.Pp&lnCont) 
        *--- If true Do not change prepack
        *:B804346,1 SSH Alwayes True.
        *llCheck = (lnTotPrk = 0) .OR. llCheck
        llCheck = .T.
        *:B804346,1 SSH[END].
      ELSE
        *:B804346,1 SSH No need for this conditoin.
        *IF lnChange = '2' .AND. lnTotPrk < ((Ordline.pik&lnCont/Scale.Pp&lnCont) +1)
        IF lnTotPrk < ((Ordline.pik&lnCont/Scale.Pp&lnCont) +1)        
          llCheck = .T.
        *:B804346,1 SSH[END].
        ELSE
          llCheck = .F.
        ENDIF
      ENDIF    
    ENDIF
  ELSE
    IF lnChange = '1'
      llCheck = .T.
      EXIT  
    ELSE
      IF !EMPTY(Scale.Pp&lnCont) 
        llCheck = .F.
        EXIT  
      ENDIF
    ENDIF  
  ENDIF
ELSE
  IF !(Ordline.pik&lnCont = 0 .AND. SCALE.PP&lnCont = 0)
    IF lnChange = '1'
      llCheck =  Ordline.pik&lnCont<>0 .OR. SCALE.PP&lnCont = 0
    ENDIF
  ENDIF
ENDIF
ENDFOR
*:B804346,1 SSH If pack first carton and totpik weight < carton preak weight
*:B804346,1 SSH do not change prepack.
IF ((Ordline.TotPik * style.nStyWeight) < lnRpWght) .AND. lnStorFrm = 0
  llCheck = (lnChange = '1')
ENDIF

RETURN llCheck

*!*************************************************************
*! Name      : lfUpRej
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : rejected pack
*!*************************************************************
*! Example   : = lfUpRej
*!*************************************************************
*!
FUNCTION lfUpRej
lcDelPik = OrdLine.PikTkt
llCheck2 = .F.
IF SEEK(lcDelPik,lcPakHdr)
  SELECT (lcPakHdr)
  DELETE REST WHILE Pack_No = lcDelPik
ENDIF
IF SEEK(lcDelPik,lcPakLin)
  SELECT (lcPakLin)
  DELETE REST WHILE Pack_No = lcDelPik
ENDIF
IF !SEEK(lcDelPik,lcRejPkTmp)
  SELECT (lcRejPkTmp)
  APPEND BLANK
  REPLACE  PikTkt  WITH OrdLine.PikTkt   ,;
         PikDate WITH OrdLine.PikDate  ,;
         Order   WITH OrdLine.Order    ,;
         Account WITH OrdLine.Account  ,;
         AccName WITH Customer.BtName ,;
         cReason WITH lcReason ,;
         Store   WITH OrdLine.Store
ENDIF


*!*************************************************************
*! Name      : lfPreDist
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : Distrebute qty per size and prepak
*!*************************************************************
*! Example   : =lfPreDist()
*!*************************************************************
*!
FUNCTION lfPreDist
*--llChngPrpk : variable to check if prepack changed
llChngPrpk = .F.
lcOldPr = Scale.prepak
lcPrePak= Scale.prepak
llChngPrRk =lfCheckPer('1')
IF llChngPrRk
  lnCurAlias  =SELECT(0)
  SELECT SCALE
  =SEEK('P'+Style.Scale)
  SCAN FOR Type+Scale = 'P'+Style.Scale
      llChngPrpk=lfCheckPer('2')
      IF llChngPrpk
        llChngPrpk = .F.
        *-C102272,5 MHM 25/06/2001  [Start]
        *--comment message which appear
        *IF (Scale.prepak <> lcOldPr)
        *--PrePack x changed to y
        *--<Ok>
         *lcMessage = "PrePack "+lcOldPr +" changed to " +Scale.prepak
         *=gfModalGen("TRM000000B00000","DIALOG",'','','&lcMessage')  
        *ENDIF
        *-C102272,5 MHM 25/06/2001  [ENd]
        EXIT
      ENDIF
  ENDSCAN
  IF EOF()
    llChngPrpk = .T.
  ENDIF
  SELECT (lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvWieght
*! Developer : Mohamed Shokry (MHM)
*! Date      : 29/04/2001
*! Purpose   : validate weight
*!*************************************************************
*! Example   : = lfvWieght
*!*************************************************************
*!
FUNCTION lfvWieght
IF !(lnRpWght > 0)
  WAIT WINDOW "Positive value required."
  RETURN (.F.)
ENDIF

*--- BY SSH  This Function has been cancelled due to many bugs.
*--- And I have rewrite it under function "lfPakbySz".
*!*************************************************************
*! Name      : lfSpclCase
*! Developer : Mohamed Shokry (MHM)
*! Date      : 25/06/2001
*! Purpose   : Calculate and distrebute Cartons 
*!*************************************************************
*! Called from : =lfOptGrid()
*!*************************************************************
*! Passed Parameters  : lnNoCrt   --->Number Of Cartons
*!                      lnFrmCrt  ---> From Carton
*!                      lnToCrt   ---> To Carton
*!*************************************************************
*! Calls       : lfUpdTmp
*!*************************************************************
*! Return      : None
*!*************************************************************
*!C102272,5 
*--- BY SSH  This Function has been cancelled due to many bugs.
*--- And I have rewrite it uncer name "lfPakbySz"
FUNCTION lfSpclCase
PARAMETER lnNoCrt ,lnFrmCrt , lnToCrt
DIMENSION laQtyArr[9]
*B804334,1 [start]
*STORE 0 TO lnRemendr ,laQtyArr
STORE 0 TO laQtyArr
lnRemendr  = lnOldRem
*B804334,1 [end]
*--lnOrdQty    =====> Number of Carton Per Size
*--lnRemendr   =====> Remender of Size Piked Qty if it exceeds Carton
*--lnVar       =====> No of Pieces of Second Size QTY in case of mixed size at the same caron
*--lnPikQty    =====> No of Pieces Requared to pack
*--llflag200   =====> flag for case 3 and 6
*--llflag300   =====>
*--lnVar       =====> Variable to get both remender and piked
*--laQtyArr    =====> array to get Qty
*--llHavCrt    =====> Variable to check if have previos Carton for the same size
*--lnPicQty    =====> Variable to accumelate pices in one carton 


lnFrmCrt = IIF (lnStorFrm = 0 ,IIF(lnFrmCrt = 0 ,1,lnFrmCrt),lnStorFrm)
llCheck2 = .F.
lnCurAlias  =SELECT(0)

*:B604765,1 MHM 04/08/2001 retrive old Carton Number[start]
*lnCrtNo =  INT(lnRpWght /Style.nstyweight)
*lnNoCrt = CEILING((OrdLine.TotPik * Style.nstyweight) / lnRpWght )

lnCrtNo = lnOldCrtNo
IF lnOldweght <> 0
  lnCrtNo =  INT((lnRpWght-lnOldweght) /Style.nstyweight) +lnPicQty
  lnNoCrt = CEILING((OrdLine.TotPik * Style.nstyweight) / lnRpWght )
ELSE
  lnCrtNo =  INT(lnRpWght /Style.nstyweight)
  lnNoCrt = CEILING((OrdLine.TotPik * Style.nstyweight) / lnRpWght )
ENDIF  
*:B604765,1 MHM 04/08/2001 [end]

llHavCrt = .F.
=SEEK('S'+Style.Scale,'Scale')
FOR lnContr =1 TO  SCALE.Cnt 
  lnCont = STR(lnContr,1)
  llflag200 =.T.
  llflag300 =.T.
  lnPikQty&lnCont = Ordline.Pik&lnCont 
  
  IF lnRemendr >0
    lnVar = lnRemendr + Ordline.Pik&lnCont
*--- MHM1
*    IF lnVar > lnCrtNo
    IF lnVar > lnCrtNo .AND. ABS(lnCrtNo - lnRemendr) <> 0
*--- MHM1
      *laQtyArr[lnContr] = lnCrtNo - lnRemendr
      laQtyArr[lnContr] = ABS(lnCrtNo - lnRemendr)

      laQtyArr[9]       = laQtyArr[9] +laQtyArr[lnContr] 
      lnVar             = lnCrtNo - lnRemendr
      lnPikQty&lnCont   = Ordline.Pik&lnCont - lnVar
      *:B604765,1 MHM 02/08/2001 [start]
      *B804334,1 MHM [Start]
      IF !EMPTY(lnRemendr) AND !EMPTY(Ordline.Pik&lnCont)
        llHavCrt  = .F.
      ENDIF
      *B804334,1 MHM [End]
      IF llHavCrt 
        lnToCrt  = lnFrmCrt + 1
        llHavCrt = .F.
      ELSE
        lnToCrt  = lnFrmCrt
      ENDIF  
      lcType = 2
      =lfUpdCart(lnContr,lnFrmCrt,lnToCrt,lcType)
      *B804334,1 MHM [Start]
      *lnFrmCrt = lnFrmCrt  + 1

      *:B604765,1 MHM 02/08/2001 Check for add new carton [start]
      *lnFrmCrt = lnToCrt + 1
      IF lnRemendr < ordline.totpik
        lnFrmCrt = lnToCrt + 1
        lnPicQty = 0
        lnOldweght =0
      ENDIF
      IF lnContr = 1
*--- MHM1
        *IF (lnRemendr + ordline.pik1) => lnCrtNo 
        IF (lnRemendr + ordline.pik&lnCont) => lnCrtNo
          lnFrmCrt = lnToCrt + 1
        ENDIF 
      ENDIF  
      IF lnCrtNo >lnPicQty 
        lnFrmCrt = lnFrmCrt
      ELSE
        lnFrmCrt = lnToCrt + 1
        lnPicQty = 0
        lnOldweght = 0
      ENDIF

      *:B604765,1 MHM [End]

      *B804334,1 MHM [End]
      STORE 0 TO laQtyArr
      lnRemendr  = Ordline.Pik&lnCont - lnVar 
      IF lnRemendr >0
        laQtyArr[lnContr] =Ordline.Pik&lnCont - lnVar 
        laQtyArr[9]       =laQtyArr[lnContr] 
      ENDIF  
      
      IF (lnContr = SCALE.Cnt) 
        IF (lnRemendr <lnCrtNo )
          IF llHavCrt 
            lnToCrt  = lnFrmCrt + 1
            llHavCrt = .F.
          ELSE
            lnToCrt  = lnFrmCrt
          ENDIF  
          lcType = 2
          =lfUpdCart(lnContr,lnFrmCrt,lnToCrt,lcType)
          *B804334,1 MHM [Start]
          *lnFrmCrt = lnFrmCrt  + 1

          *:B604765,1 MHM 02/08/2001 check for new carton if carton is complete [start]
          *lnFrmCrt = lnToCrt + 1
          IF lnCrtNo >lnPicQty
            lnFrmCrt = lnFrmCrt
          ELSE
            lnFrmCrt = lnToCrt + 1
            lnPicQty = 0
            lnOldweght = 0
          ENDIF
          *:B604765,1 MHM [end]
          *B804334,1 MHM [End]
          STORE 0 TO lnRemendr
          llflag200 = .F.
        ELSE
          llflag200 = .T.
        ENDIF  
      ELSE
        llflag200 = .T.
      ENDIF
    ELSE
      lnRemendr         = lnRemendr + Ordline.Pik&lnCont
      laQtyArr[lnContr] = Ordline.Pik&lnCont
      laQtyArr[9]       =laQtyArr[9] +laQtyArr[lnContr] 
      
      llflag300 =.F.
      IF lnContr = SCALE.Cnt 
        IF (lnRemendr <lnCrtNo )
          IF lnCrtNo < lnRemendr 
            lnToCrt  = lnFrmCrt  + 1
          ELSE
            lnToCrt  = lnFrmCrt  
          ENDIF  
          lcType = 2
          =lfUpdCart(lnContr,lnFrmCrt,lnToCrt,lcType)
          *B804334,1 MHM [Start]
          *lnFrmCrt = lnFrmCrt  + 1

          *:B604765,1 MHM 02/08/2001 check for new carton if carton is complete [start]
          *lnFrmCrt = lnToCrt + 1
          IF lnCrtNo >lnPicQty 
            lnFrmCrt = lnFrmCrt
          ELSE
            lnFrmCrt = lnToCrt + 1
            lnPicQty = 0
            lnOldweght = 0
          ENDIF
          *:B604765,1 MHM [End]

          *B804334,1 MHM [End]

          *:B604765,1 MHM 02/08/2001 Store remender of the style [start]
          lnOldRem = lnRemendr
          STORE 0 TO lnRemendr
          *:B604765,1 MHM  [End]

          llflag200 = .F.
        ELSE
          llflag200 = .T.
        ENDIF  
      ELSE
        llflag200 = .T.
      ENDIF
    ENDIF
  ENDIF

  IF llflag200
    lnTOtPrCRt = INT(lnPikQty&lnCont/lnCrtNo)
    *--to get From Carton To Carton
    *:B604765,1 MHM 02/08/2001 [start]
    *lnFrmCrt = lnToCrt  + 1
    *lnToCrt  = lnFrmCrt + lnTOtPrCRt -  1
    *:B604765,1 MHM 02/08/2001 [end]
    *B804334,1 MHM 31/07/2001  let system accept  one in piked order [Start]
    *IF lnPikQty&lnCont >1
    IF lnPikQty&lnCont >0
    *B804334,1 MHM 31/07/2001  [Start]

      *IF lnPikQty&lnCont < lnCrtNo
      IF lnPikQty&lnCont < lnCrtNo
        IF llflag300 
          lnRemendr  = lnPikQty&lnCont 
          laQtyArr[lnContr]  = lnPikQty&lnCont
          laQtyArr[9]        = laQtyArr[lnContr] 
          
        ENDIF 
      ELSE  
        *B804334,1 MHM [Start]
        llHavCrt = .T.
        *lnToCrt  = lnFrmCrt + lnTOtPrCRt 
        lnToCrt  = lnFrmCrt + lnTOtPrCRt -1
        *B804334,1 MHM [End]
       
        lcType = 1
        laQtyArr[lnContr] = lnCrtNo
        laQtyArr[9]       = laQtyArr[lnContr] 
        
        =lfUpdCart(lnContr,lnFrmCrt,lnToCrt,lcType)
  
        *B804334,1 MHM [Start]
        *lnFrmCrt = lnFrmCrt  + 1

        *:B604765,1 MHM 02/08/2001 check for new carton if carton is complete [start]
        *lnFrmCrt = lnToCrt + 1
        IF lnCrtNo > lnPicQty 
          lnFrmCrt = lnFrmCrt
        ELSE
          lnFrmCrt = lnToCrt + 1
          lnPicQty = 0
          lnOldweght = 0
        ENDIF
        *:B604765,1 MHM [end]

        *B804334,1 MHM [Start]
        
        STORE 0 TO laQtyArr
        lnRemendr  = lnPikQty&lnCont - lnTOtPrCRt*lnCrtNo
        IF lnRemendr >0
          laQtyArr[lnContr] = lnPikQty&lnCont - lnTOtPrCRt*lnCrtNo
          laQtyArr[9]       = laQtyArr[lnContr] 
          
        ENDIF  
      ENDIF  
    ENDIF
  ENDIF  
ENDFOR
  
IF lnRemendr >0
  STORE 0 TO laQtyArr[9]
  laQtyArr[lnContr-1] = lnPikQty&lnCont - lnTOtPrCRt*lnCrtNo
  FOR lnContr = 1 TO 8 
    laQtyArr[9]  = laQtyArr[9] + laQtyArr[lnContr] 
  ENDFOR
  *lnFrmCrt = lnToCrt  + 1
  *804334,1 mhm
  IF lnRemendr < lncrtno
    llHavCrt = .F.
  ENDIF
  *804334,1 mhm

  IF llHavCrt 
    lnToCrt  = lnFrmCrt + 1
    llHavCrt = .F.
  ELSE
    lnToCrt  = lnFrmCrt
  ENDIF  
  *lnToCrt  = lnToCrt + 1
  lcType = 2
  =lfUpdCart(lnContr,lnFrmCrt,lnToCrt,lcType)

  *B804334,1 [start]
  *:B604765,1 MHM 02/08/2001 check for new carton if carton is complete [start]
  *lnOldRem = lnRemendr
  IF  lnRemendr = ordline.totpik  OR lnRemendr = lnCrtNo
    lnOldRem = 0
    *--initialize lnPicqty
    IF lnRemendr = lnCrtNo
      lnPicQty = 0
      lnOldweght = 0
    ENDIF
    lnFrmCrt = lnToCrt  + 1
  ELSE
    lnOldRem = lnRemendr
  ENDIF
  *:B604765,1 MHM  [end]

  *B804334,1[end]

ENDIF  
*B804334,1 MHM [Start]
*--Store From Number 
lnStorFrm = lnFrmCrt 
lnOldCrtNo = lnCrtNo
*B804334,1 MHM [End]

SELECT (lcPakHdr)                 
IF !SEEK(OrdLine.PikTkt)
  APPEND BLANK
  REPLACE Pack_No  WITH OrdLine.PikTkt  ,;
          Order    WITH OrdLine.Order   ,;
          Store    WITH OrdLine.Store   ,;        
          Account  WITH OrdLine.Account ,;
          AccName  WITH Customer.BtName        
ENDIF
*B804334,1 [start]
*REPLACE  Tot_Cart WITH Tot_Cart + lnNoCrt                    ,;
         Tot_Pcs  WITH Tot_Pcs  + OrdLine.TotPik ,;
         Tot_Wght WITH Tot_Wght + (OrdLine.TotPik * Style.nStyWeight);
         Pick_Qty WITH  OrdLine.TotPik;
         Order_Qty WITH OrdLine.TotQTY
REPLACE  Tot_Cart WITH  lnToCrt        ,;
         Tot_Pcs  WITH Tot_Pcs  + OrdLine.TotPik ,;
         Tot_Wght WITH Tot_Wght + (OrdLine.TotPik * Style.nStyWeight);
         Pick_Qty WITH  OrdLine.TotPik;
         Order_Qty WITH OrdLine.TotQTY

*B804334,1[end]

*-Assign BOL for the packing list if there is EB or AS module 
*:B604842,1 SSH [Start] Commented out and create on packing slip level.
*=lfAssgnBol(OrdLine.cOrdType + OrdLine.Order , Account , Store)          
*:B604842,1 SSH [END]
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfUpdCart
*! Developer : Mohamed Shokry (MHM)
*! Date      : 25/06/2001
*! Purpose   : Calculate and distrebute Cartons 
*!*************************************************************
*! Called from : =lfOptGrid()
*!*************************************************************
*! Passed Parameters  : lnNoCrt   --->Number Of Cartons
*!                      lnFrmCrt  ---> From Carton
*!                      lnToCrt   ---> To Carton
*!*************************************************************
*! Calls       : lfUpdTmp
*!*************************************************************
*! Return      : None
*!*************************************************************
*!C102272,5 
FUNCTION lfUpdCart
PARAMETER lnContr,lnFrmCrt,lnToCrt,lcType

*:B604765,1 MHM 02/08/2001 Increment lnPicQty to calculate carton QTY [start]
lnPicQty = lnPicQty + laQtyArr[9] 
lnOldweght = lnOldweght + laQtyArr[9]*Style.nStyweight
*:B604765,1 MHM [end]

SELECT (lcPakLin)
APPEND BLANK
REPLACE Pack_No    WITH OrdLine.PikTkt  ,;
        No_Cart    WITH lnNoCrt         ,;
        From_Crt   WITH lnFrmCrt        ,;
        To_Crt     WITH lnToCrt         ,;
        Style      WITH OrdLine.Style   ,;
        nOrdLineNo WITH OrdLine.LineNo  ,;
        Scale      WITH OrdLine.Scale   ,;
        PrePak     WITH OrdLine.PrePak  ,;
        OrgOrd1    WITH OrdLine.Qty1    ,;
        OrgOrd2    WITH OrdLine.Qty2    ,;
        OrgOrd3    WITH OrdLine.Qty3    ,;
        OrgOrd4    WITH OrdLine.Qty4    ,;
        OrgOrd5    WITH OrdLine.Qty5    ,;
        OrgOrd6    WITH OrdLine.Qty6    ,;
        OrgOrd7    WITH OrdLine.Qty7    ,;
        OrgOrd8    WITH OrdLine.Qty8    ,;
        TotOrgOrd  WITH OrdLine.TotQty  ,;
        Ord1       WITH OrdLine.Qty1    ,;
        Ord2       WITH OrdLine.Qty2    ,;
        Ord3       WITH OrdLine.Qty3    ,;
        Ord4       WITH OrdLine.Qty4    ,;
        Ord5       WITH OrdLine.Qty5    ,;
        Ord6       WITH OrdLine.Qty6    ,;
        Ord7       WITH OrdLine.Qty7    ,;
        Ord8       WITH OrdLine.Qty8    ,;
        TotOrd     WITH OrdLine.TotQty  ,;
        Pik1       WITH OrdLine.Pik1    ,;
        Pik2       WITH OrdLine.Pik2    ,;
        Pik3       WITH OrdLine.Pik3    ,;
        Pik4       WITH OrdLine.Pik4    ,;
        Pik5       WITH OrdLine.Pik5    ,;
        Pik6       WITH OrdLine.Pik6    ,;
        Pik7       WITH OrdLine.Pik7    ,;
        Pik8       WITH OrdLine.Pik8    ,;
        TotPik     WITH OrdLine.TotPik  ,;
        cZeroQty   WITH 'N'
        =lfSUpdQty(lcType)
*-- End lfUpdTmp
*!*************************************************************
*! Name      : lfSUpdQty  
*! Developer : Mohamed Shokry (MHM)
*! Date      : 25/06/2001
*! Purpose   : Update Qty file
*!*************************************************************
*! Called from : =lfUpdTmp()
*!*************************************************************
*! Passed Parameters  : lnFlag  ---> FLAG 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*!C102272,5 
FUNCTION lfSUpdQty
PARAMETER  lnFlag

*--we will get prepak qty first and applay it for all code 
*--when we reach to it we calculate 


REPLACE Qty1       WITH laQtyArr[1] ,;
        Qty2       WITH laQtyArr[2] ,;
        Qty3       WITH laQtyArr[3] ,;
        Qty4       WITH laQtyArr[4] ,;
        Qty5       WITH laQtyArr[5] ,;
        Qty6       WITH laQtyArr[6] ,;
        Qty7       WITH laQtyArr[7] ,;
        Qty8       WITH laQtyArr[8] ,;
        TotQty     WITH laQtyArr[9] 


*:B804346,1 SSH Fix bug of Printing -ve qty in the detail folder.
*!*************************************************************
*! Name      : lfPakbySz
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 25/06/2001
*! Purpose   : 
*!*************************************************************
*! Called from : =lfUpdTmp()
*!*************************************************************
*! Passed Parameters  : lnFlag  ---> FLAG 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfPakbySz

PRIVATE lnIndex , lcIndex , lnSzQty

*--- lnNoCrt,lnFrmCrt , lnToCrt
DIMENSION laQtyArr[9]
FOR lnInde1 = 1 TO 9
  laQtyArr[lnInde1] = 0
ENDFOR

IF lnFrmCrt < 2
  STORE 1 TO lnFrmCrt , lnToCrt
ENDIF
lnToCrt = IIF(lnToCrt =0,1,lnToCrt)
IF lnToCrt <> 0 .OR. lnFrmCrt >= 2
  lnFrmCrt = IIF (lnStorFrm = 0 ,IIF(lnFrmCrt = 0 ,1,lnFrmCrt),lnStorFrm)
  lnToCrt  = IIF (lnStorFrm = 0 ,IIF(lnToCrt = 0 ,1,lnToCrt),lnFrmCrt )
ENDIF

=SEEK('S'+Style.Scale,'Scale')
FOR lnIndex = 1 TO SCALE.CNT
  lcIndex = STR(lnIndex,1)
  lnTempWg = (OrdLine.Pik&lcIndex * Style.nStyWeight)
  DO WHILE lnTempWg > 0
    lnWgInCrt = lnWgInCrt + IIF(Style.nStyWeight>0,Style.nStyWeight,1)
    IF lnWgInCrt > lnRpWght
      IF laQtyArr[9] > 0
        =lfUpdCart("",lnFrmCrt,lnToCrt)
        IF EOF("Style")
          =SEEK(OrdLine.Style,'Style')
        ENDIF
      ENDIF    
      FOR lnInde1 = 1 TO 9
        laQtyArr[lnInde1] = 0
      ENDFOR
      lnToCrt  = lnToCrt + 1
      lnFrmCrt = lnToCrt
      lnWgInCrt = 0
      lnCrtWght = lnRpWght
    ELSE
      laQtyArr[lnIndex] = laQtyArr[lnIndex]+1
      laQtyArr[9] = laQtyArr[9] + 1
      lnCrtWght = MAX(lnCrtWght - Style.nStyWeight ,0)
      lnTempWg  = MAX(lnTempWg - Style.nStyWeight ,0)
    ENDIF    
  ENDDO
ENDFOR
IF laQtyArr[9] > 0
  =lfUpdCart("",lnFrmCrt,lnToCrt)
  IF EOF("Style")
    =SEEK(OrdLine.Style,'Style')
  ENDIF
ENDIF
lnStorFrm = lnFrmCrt

*:B804346,1 SSH Function to update header file for header browse.
*!*************************************************************
*! Name      : lfUpdHdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 25/06/2001
*! Purpose   : 
*!*************************************************************
*! Called from : =lfUpdTmp()
*!*************************************************************
*! Passed Parameters  : lnFlag  ---> FLAG 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfUpdHdr
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
SELECT (lcPakHdr)                 
IF !SEEK(OrdLine.PikTkt)
  APPEND BLANK
  REPLACE Pack_No  WITH OrdLine.PikTkt  ,;
          Order    WITH OrdLine.Order   ,;
          Store    WITH OrdLine.Store   ,;        
          Account  WITH OrdLine.Account ,;
          AccName  WITH Customer.BtName        
ENDIF
REPLACE  Tot_Cart WITH lnToCrt        ,;
         Tot_Pcs  WITH Tot_Pcs  + OrdLine.TotPik ,;
         Tot_Wght WITH Tot_Wght + (OrdLine.TotPik * Style.nStyWeight);
         Pick_Qty WITH  OrdLine.TotPik;
         Order_Qty WITH OrdLine.TotQTY
*-Assign BOL for the packing list if there is EB or AS module.
*:B604842,1 SSH [Start] Commendted out and save on packing slip level.
*=lfAssgnBol(OrdLine.cOrdType + OrdLine.Order , Account , Store)          
*:B604842,1 SSH [END]
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfLblInfo
*! Developer : Wael Ali Mohamed
*! Date      : 06/27/2002
*! Purpose   : Get labels information
*!*************************************************************
*! Called from : =lfLblInfo()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfLblInfo
llPrnLbl = llCanPrnLb AND llScrPrnLb
IF llPrnLbl
  lcOutFile = gcWorkDir+lcAsnLabel+".TXT"
  lnHandle  = FCREATE(lcOutFile,0)
  =FSEEK(lnHandle,0,2)
ENDIF
DECLARE laDRltFld[1,2]
laDRltFld[1,1] = 'DIVLNAME  '
laDRltFld[1,2] = 'lcDivLName'
lcDivLName = ''
lnMajorLen = LEN(lcMask)
llUPCInst  = ('UP' $ gcCmpModules)

IF llUPCInst
  =gfOpenFile(gcDataDir + 'STYLEUPC' , gcDataDir + 'Packupc' , 'SH')
ENDIF
*-- Get color segment information
llUseColor = .F.
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg , 1)
  IF laItemSeg[lnCount,1] = 'C'
    llUseColor = .T.
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrPos   = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
m.cAdd_User = gcUser_Id
m.dAdd_Date = gdSysDate
m.cAdd_Time = TIME()
m.MANUF_ID  = PADL(ALLTRIM(lcManufId),7,'0')

SET ORDER TO ORDHDR IN ORDHDR
SELECT (lcCtnHdr)
GO TOP
DO WHILE !EOF()
  
  m.BOl_No = BOL_NO
  =SEEK(m.BOl_No,'Bol_Hdr')

  m.ASN_VER = ''
  IF SEEK('A'+Bol_Hdr.Account,'EDIACPRT') .AND. SEEK(EDIACPRT.cPartCode,'EDIPH')
    m.ASN_VER = IIF(Bol_Hdr.CToStorCn='C',EdiPH.cASNLbl2,EdiPH.cASNLbl1)
  ENDIF  
  m.ASN_VER = IIF(EMPTY(m.ASN_VER),'XXX',m.ASN_VER)
  
  llAccLabel=llPrnLbl AND SEEK(m.ASN_VER + 'L','SycAsnLb') .AND. SEEK(m.ASN_VER+ 'H','SycAsnLb')  
  llDetLabel=llPrnLbl AND EdiPh.ldtlbl AND SEEK('XX1' + 'L','SycAsnLb') .AND. SEEK('XX1'+ 'H','SycAsnLb')

  *-- Print label header  
  IF llAccLabel AND !llDetLabel 
    SELECT SycAsnLb
    =SEEK(m.ASN_VER+'H','SycAsnLb')
    SCAN REST WHILE cVer+cEDIType = m.ASN_VER+'H'
      STORE DATA TO MDATA
      lcString = &Mdata 
      =FPUTS(lnHandle,lcString)
    ENDSCAN	
    lcString = '   '
    =FPUTS(lnHandle,lcString)
    SELECT (lcCtnHdr)
  ENDIF

  m.ShipVia = Bol_Hdr.ShipVia
  m.Carrier = gfCodDes(m.ShipVia,'SHIPVIA')
  mRmt_cod  = Bol_Hdr.Rmt_cod
  MCACODE   = Bol_Hdr.CarrierCod

  *-- Get Warehouse information
  =SEEK(Bol_Hdr.W_CODE,'Warehous')
  m.VND_NAME  = Warehous.cDesc
  m.VND_ADDR1 = Warehous.cAddress1
  m.VND_ADDR2 = Warehous.cAddress2
  m.VND_CITY  = Warehous.cAddress3
  m.VND_STATE = Warehous.cAddress4
  m.VND_ZIP   = Warehous.cAddress5
  m.TMP_ADDR  = ALLTRIM(m.VND_CITY) + ",  " +ALLTRIM(m.VND_STATE) + "   " + ALLTRIM(m.VND_ZIP)

  DO WHILE BOL_NO+pack_No+STR(Cart_No,4)+STR(Pal_No,4) = m.BOl_No

    m.Pack_No = Pack_No
    =SEEK(m.Pack_No,'Pack_Hdr')
    m.Cartons = Pack_Hdr.Tot_Cart
  
    *-- Get Customer information
    m.Store = Pack_Hdr.Store
    =SEEK(IIF(EMPTY(m.Store),'M'+PACK_HDR.Account,'S'+PACK_HDR.Account+m.Store),'Customer')
    STORE m.Store            TO m.cStStore
    STORE Customer.StName    TO m.cStName ,m.SHP_NAME
    STORE Customer.cAddress1 TO m.cStAddr1,m.SHP_ADDR1
    STORE Customer.cAddress2 TO m.cStAddr2,m.SHP_ADDR2
    STORE Customer.cAddress3 TO m.cStCity ,m.SHP_CITY
    STORE Customer.cAddress4 TO m.cStState,m.SHP_STATE
    STORE Customer.cAddress5 TO m.cStZip  ,m.SHP_ZIP

    *-- Get Distribution Center information
    lcDistCtr = Customer.Dist_Ctr 
    IF SEEK('S'+PACK_HDR.account+lcDistCtr,'Customer')
      m.STORE     = lcDistCtr 
      m.SHP_NAME  = Customer.STName
      m.SHP_ADDR1 = Customer.cAddress1
      m.SHP_ADDR2 = Customer.cAddress2
      m.SHP_CITY  = Customer.cAddress3
      m.SHP_STATE = Customer.cAddress4
      m.SHP_ZIP   = Customer.cAddress5
    ENDIF
    =SEEK('M'+PACK_HDR.account,'Customer')
    m.Int_Vend = Customer.cCusVend

    *-- Get Order information
    =SEEK('O'+PACK_HDR.Order,'ORDHDR')
    m.Note1     = OrdHdr.Note1
    m.Note2     = OrdHdr.Note2
    m.Int_Vend  = IIF(EMPTY(m.Int_Vend),ORDHDR.INT_VEND,m.Int_Vend)
    m.Cancelled = OrdHdr.Complete
    m.EVENT_COD = ORDHDR.EVENT_COD  
    m.CUSTPO    = OrdHdr.CustPo

    =gfRltFld(ORDHDR.cDivision,@laDRltFld,'CDIVISION ')
    m.DivLName = lcDivLName

    m.DEPT      = OrdHdr.Dept
    =SEEK(OrdHdr.Account+OrdHdr.Dept,'CUSTDEPT')
    m.DeptDesc = CustDept.cDeptDesc

    SCAN REST WHILE BOL_NO+pack_No+STR(Cart_No,4)+STR(Pal_No,4) = m.BOl_No+m.Pack_NO
      m.Cart_No   = Cart_No
      m.UCC9      = RIGHT(PADL(ALLTRIM(m.Pack_No),6,'0'),5)+PADL(m.Cart_No,4,'0')
      m.UCC_CHECK = lfCheckNo('000'+m.MANUF_ID+m.Ucc9)
      m.TotQty    = TotPcs

      *-- Get Carton details
      SELECT (lcCtnDtl)
      =SEEK(m.Pack_NO+STR(m.Cart_No,4))
      m.Pack_Id = Pack_Id
      m.Style = SUBSTR(Style,1,lnMajLen)
      lcStyle = Style
      lcColor = SUBSTR(Style,lnClrPos,lnClrLen)
      lcSizes = ''
      lcSize = ''
      FOR lnCount = 1 TO 8
        lcCount = ALLTRIM(STR(lnCount,1))
        lcSizes = lcSizes+IIF(Qty&lcCount=0,'',lcCount)
      ENDFOR
      IF LEN(lcSizes)=1 AND SEEK(lcStyle,'Style') AND SEEK('S'+Style.Scale,'Scale')
        m.cSizeDesc = Scale.Sz&lcSizes
        lcSize = lcSizes
      ELSE
        m.cSizeDesc = ''
      ENDIF
      lnLblInfo = 0
      DIMENSION laLblInfo[1, 8]
      STORE '' TO laLblInfo
      *-- Get carton information
      SCAN REST WHILE Pack_NO+STR(Cart_No,4) = m.Pack_NO+STR(m.Cart_No,4)
        m.Pack_Id = IIF(Pack_Id=m.Pack_Id,m.Pack_Id,'')
        m.Style   = IIF(SUBSTR(Style,1,lnMajLen) = m.Style,m.Style,SPACE(19))
        lcStyle   = IIF(Style = lcStyle,lcStyle,SPACE(19))
        lcColor   = IIF(SUBSTR(Style,lnClrPos,lnClrLen)=lcColor,lcColor,SPACE(6))
        IF !EMPTY(m.cSizeDesc)
          lcSizes = ''
          FOR lnCount = 1 TO 8
            lcCount = ALLTRIM(STR(lnCount,1))
            lcSizes = lcSizes+IIF(Qty&lcCount=0,'',lcCount)
          ENDFOR
          IF LEN(lcSizes)=1 AND SEEK(Style,'Style') AND SEEK('S'+Style.Scale,'Scale')
            lcSizeDesc = Scale.Sz&lcSizes
            m.cSizeDesc = IIF(m.cSizeDesc=lcSizeDesc,m.cSizeDesc,'')
          ELSE
            m.cSizeDesc = ''
          ENDIF
        ENDIF
        lnTOtQty = Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        *-- Store detail information
        IF !EMPTY(m.Pack_Id)
          lnLblInfo = lnLblInfo + 1
          DIMENSION laLblInfo[lnLblInfo , 8]
          IF SEEK('P' + Bol_Hdr.Account + m.Pack_Id + lcStyle , 'Spck_Lin') OR;
             SEEK('P' + '*****'  + m.Pack_Id + lcStyle , 'Spck_Lin')
            laLblInfo[lnLblInfo, 8] = Spck_Lin.TotQty
          ELSE
            laLblInfo[lnLblInfo, 8] = 0  
          ENDIF   
          laLblInfo[lnLblInfo, 1] = "$#" + Bol_Hdr.ACCOUNT
          laLblInfo[lnLblInfo, 2] = 0
          laLblInfo[lnLblInfo, 3] = ""
          laLblInfo[lnLblInfo, 4] = m.Pack_Id
          laLblInfo[lnLblInfo, 5] = ""
          laLblInfo[lnLblInfo, 6] = 0
          laLblInfo[lnLblInfo, 7] = lnTOtQty
          IF llUPCInst .AND.;
            ((SEEK(Bol_Hdr.Account + PADR(m.Pack_Id,19)+SPACE(1), 'StyleUPC') .AND. StyleUPC.lUPCPack) .OR. ;
              (SEEK('*****'  + PADR(m.Pack_Id,19)+SPACE(1), 'StyleUPC', 'packUPC') .AND. StyleUPC.lUPCPack))
             laLblInfo[lnLblInfo,5] = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
           ENDIF
        ELSE
          lcUpcStyle = ALLTRIM(SUBSTR(&lcCtnDtl..Style,1,lnMajorLen))
          lcUpcStyle = PADR(SUBSTR(lcUpcStyle,1,LEN(lcUpcStyle)-2),lnMajorLen)
          lcUpcStyle = lcUpcStyle + SUBSTR(&lcCtnDtl..Style,lnMajorLen+1)
          lcUpcStyle = IIF(SEEK(lcUpcStyle,'STYLE'),lcUpcStyle,&lcCtnDtl..Style)
          =SEEK(&lcCtnDtl..Style, 'Style') AND SEEK('S' + Style.Scale, 'Scale')

          FOR lnCount = 1 TO Scale.Cnt
            lcSize = STR(lnCount, 1)
            IF EVAL(lcCtnDtl+'.Qty'+lcSize) > 0
              lnLblInfo = lnLblInfo + 1
              DIMENSION laLblInfo[lnLblInfo , 8]
              lcSKU = ""
              IF SEEK('S' + Bol_Hdr.Account + &lcCtnDtl..Style, 'Spck_Lin')
                IF Spck_Lin.TotQty = 0
                  lcSKU = PADR(Spck_Lin.Pack_Id, 16)
                ELSE
                  SELECT SPCK_LIN
                  LOCATE REST ;
                  WHILE Type+Account+Style = 'S'+Bol_HDr.Account+&lcCtnDtl..Style FOR Qty&lcSize = 1
                  IF FOUND()
                    lcSKU = PADR(Pack_Id , 16)
                  ENDIF
                ENDIF
              ENDIF
              laLblInfo[lnLblInfo,1] = lcSKU
              laLblInfo[lnLblInfo,2] = EVAL(lcCtnDtl+'.Qty'+lcSize)
              laLblInfo[lnLblInfo,3] = Scale.Sz&lcSize
              laLblInfo[lnLblInfo,4] = &lcCtnDtl..Style
              IF llUPCInst AND SEEK(SPACE(5)+lcUpcStyle+ lcSize, "StyleUPC")
                laLblInfo[lnLblInfo,5] = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
              ELSE
                laLblInfo[lnLblInfo,5] = ''
              ENDIF
              laLblInfo[lnLblInfo,6] = lnCount
              laLblInfo[lnLblInfo,7] = lnTOtQty
              laLblInfo[lnLblInfo,8] = 0
            ENDIF
          ENDFOR
        ENDIF
      ENDSCAN
      IF EMPTY(lcColor)
        m.cclrDesc = 'MIXED'
      ELSE
        m.cclrDesc= gfCodDes(lcColor,'COLOR')
      ENDIF
      m.cUpc = ''
      IF EMPTY(m.cSizeDesc)
        m.cSizeDesc = 'MIXED'
      ELSE
        IF llUPCInst AND !EMPTY(lcStyle) 
          lcUpcStyle = ALLTRIM(SUBSTR(lcStyle,1,lnMajorLen))
          lcUpcStyle = PADR(SUBSTR(lcUpcStyle,1,LEN(lcUpcStyle)-2),lnMajorLen)
          lcUpcStyle = lcUpcStyle + SUBSTR(lcStyle,lnMajorLen+1)
          lcUpcStyle = IIF(SEEK(lcUpcStyle,'STYLE'),lcUpcStyle,lcStyle)

          IF SEEK(SPACE(5)+lcUpcStyle+lcSize,'StyleUpc')
            m.cUpc = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
          ENDIF
        ENDIF  
      ENDIF
      IF llUPCInst AND !EMPTY(m.Pack_Id) AND ;
         ( SEEK('P'+Bol_HDr.Account+m.Pack_Id,'SPCK_HDR') OR ;
           SEEK('P'+'*****'+m.Pack_Id,'SPCK_HDR') ) AND ;
         SEEK(SPCK_HDR.Account+PADR(m.Pack_Id,19)+' ','StyleUpc')
        m.cUpc = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
      ENDIF  
      *-- Item description
      m.StyDesc = ''
      IF !EMPTY(m.Pack_Id)
        IF SEEK('P'+Bol_HDr.Account+m.Pack_Id,'SPCK_HDR') OR ;
           SEEK('P'+'*****'+m.Pack_Id,'SPCK_HDR') OR ;
           SEEK('S'+Bol_HDr.Account+m.Pack_Id,'SPCK_HDR')
          m.StyDesc = SPCK_HDR.Desc
        ENDIF
      ELSE
        IF SEEK(ALLTRIM(lcStyle),'Style','Style')
          m.StyDesc = Style.Desc
        ENDIF
      ENDIF
      IF !EMPTY(m.Pack_Id) AND SEEK('S'+Bol_HDr.Account+m.Pack_Id,'SPCK_LIN') AND ;
          SPCK_LIN.TotQty = 0 AND SEEK(Spck_Lin.Style,'Style','Style') AND ;
          SEEK('P'+Style.Scale+Spck_Lin.PrePak,'Scale','Scale')
        lcPreCode= ALLTRIM(Spck_Lin.PrePak)
        lcPreQty = ALLTRIM(STR(Scale.ppTot))
        lcPQty1  = ALLTRIM(STR(Scale.pp1))
        lcPQty2  = ALLTRIM(STR(Scale.pp2))
        lcPQty3  = ALLTRIM(STR(Scale.pp3))
        lcPQty4  = ALLTRIM(STR(Scale.pp4))
        lcPQty5  = ALLTRIM(STR(Scale.pp5))
        lcPQty6  = ALLTRIM(STR(Scale.pp6))
        lcPQty7  = ALLTRIM(STR(Scale.pp7))
        lcPQty8  = ALLTRIM(STR(Scale.pp8))
        =SEEK("S"+Style.Scale,'Scale','Scale')
        FOR lnCount = 1 TO Scale.Cnt
          lcScale = lcScale + ALLTRIM(EVAL('SCALE.Sz'+STR(lnCount,1))) + ' ' + EVAL('lcPQty'+STR(lnCount,1))
          IF lnCount <> Scale.Cnt
            lcScale = lcScale + +', '
          ENDIF  
        ENDFOR
      ENDIF   
   
      INSERT INTO ASN_SHIP FROM MEMVAR
      IF lnLblInfo > 0
        SELECT ASN_SHIP
        SAVE TO MEMO MLBLINFO ALL LIKE laLblInfo
      ENDIF
  
      *-- Print label header if detailed labels
      IF llAccLabel AND llDetLabel
        SELECT SycAsnLb
        =SEEK(m.ASN_VER+'H','SycAsnLb')
        SCAN REST WHILE cVer + cEDIType = m.ASN_VER + 'H'
          STORE DATA TO MDATA
          lcString = &Mdata 
          =FPUTS(lnHandle , lcString)
        ENDSCAN
        lcString = '   '
        =FPUTS(lnHandle , lcString)
      ENDIF
      *-- Prine Label Lines
      IF llAccLabel 
        STORE '' TO MSKU1,MSKU2,MSKU3,MSKU4,MSKU5,MSKU6,MSKU7,MSKU8
        STORE 0  TO MQTY1,MQTY2,MQTY3,MQTY4,MQTY5,MQTY6,MQTY7,MQTY8
        STORE '' TO MSIZE1,MSIZE2,MSIZE3,MSIZE4,MSIZE5,MSIZE6,MSIZE7,MSIZE8
        IF llDetLabel .AND. !EMPTY(lcStyle) .AND. !EMPTY(laLblInfo)
          FOR lnCount1 = 1 TO MIN(8 , ALEN(laLblInfo , 1))
            STORE laLblInfo[lnCount1,1] TO ('MSKU'  + STR(lnCount1 , 1))
            STORE laLblInfo[lnCount1,2] TO ('MQTY'  + STR(lnCount1 , 1))
            STORE laLblInfo[lnCount1,3] TO ('MSIZE' + STR(lnCount1 , 1))
          ENDFOR
        ENDIF
        
        SELECT SycAsnLb
        =SEEK(m.Asn_Ver+'L','SYCAsnLb')
        SCAN REST WHILE cVer+cEDIType= m.ASN_VER+'L'
          STORE DATA TO MDATA
          lcString = &Mdata 
          =FPUTS(lnHandle,lcString)
        ENDSCAN	
        lcString = '   '
        =FPUTS(lnHandle,lcString)
      
        IF llDetLabel .AND. !EMPTY(laLblInfo)
          STORE '' TO MDStyle , MDStyMaj , MDColor , MDSKU , MDSizDes , MDQty , MDStyUPC, ;
                      MDPCS
          SELECT SYCAsnLb
          =lfPrDtLbl('XX1', lnHandle)
        ENDIF
      ENDIF
     
    ENDSCAN
  ENDDO
ENDDO
IF llPrnLbl
  =FCLOSE(lnHandle)
  IF lcSndPort <> 'NONE'
    WAIT 'Sending Shipping Labels to Printer...Please stand by....' WINDOW NOWAIT
    lcCommand = "TYPE " + lcOutFile + " > " + lcSndPort
    !/N0 &lcCommand 
  ENDIF
ENDIF


*!**************************************************************************
*! Name      : lfPrDtLbl
*! Developer : Wael Ali
*! Date      : 06/08/2001
*! Purpose   : Print detailed labels
*!**************************************************************************
*! Call From : The Mapping tables
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
FUNCTION lfPrDtLbl
PARAMETERS lcLabelVersion, lnFileHandle
PRIVATE MDATA,lcString

* Print the Header of the label for each in case of 'Separate Detailed Label' .
=SEEK(lcLabelVersion + 'H')
SCAN REST WHILE cVer + cEDIType = lcLabelVersion + 'H'
  STORE DATA TO MDATA
  lcString = &Mdata 
  =FPUTS(lnFileHandle , lcString)
ENDSCAN
lcString = '   '
=FPUTS(lnFileHandle , lcString)

*-- Define some new variables
PRIVATE lnMaxLines      , lnLabelsNumber , lnLinesNumber , lnStartItem ,;
        lnLabelsCounter , lnLinesCounter , lnItemRow

*-- Get the maximum number of lines that can be printed on one label
lnMaxLines = IIF(SEEK(lcLabelVersion + "C") , VAL(ALLTRIM(SYCAsnLb.DATA)) , 0)
IF lnMaxLines <> 0
  *-- Get the number of labels that will be printed
  lnLabelsNumber = CEILING(ALEN(laLblInfo , 1) / lnMaxLines)
  *-- Initialize the variable that holds the start element
  lnStartItem    = 0
  
  *-- Loop to print the labels
  FOR lnLabelsCounter = 1 TO lnLabelsNumber
  *-- Print the lines header
    = SEEK(lcLabelVersion + 'L')
    SCAN REST WHILE cVer + cEDIType = lcLabelVersion + 'L'
      STORE DATA TO MDATA
      lcString = &Mdata
      =FPUTS(lnFileHandle , lcString)
    ENDSCAN
    
    *-- Get the number of lines that will be printed on the current label
    lnLinesNumber = MIN(lnMaxLines , ALEN(laLblInfo , 1) - lnStartItem)
    *-- Loop to print the lines Details
    FOR lnLinesCounter = 1 TO lnLinesNumber
    *-- Get the row number of the current item
      lnItemRow = lnStartItem + lnLinesCounter
      *-- Load the variables that will be used in printing for the current
      *-- carton item
      MDSKU     = laLblInfo[lnItemRow,1]
      MDQty     = laLblInfo[lnItemRow,2]
      MDSizDes  = laLblInfo[lnItemRow,3]
      MDStyle   = laLblInfo[lnItemRow,4]
      IF ALEN(laLblInfo,2) => 8
        INPACKQTY = laLblInfo[lnItemRow,8]
      ELSE
        INPACKQTY = 0
      ENDIF 
      MDStyMaj  = LEFT(laLblInfo[lnItemRow,4] , lnMajorLen)
      IF llUseColor
        MDColor = SUBSTR(laLblInfo[lnItemRow,4] , lnClrPos , lnClrLen)
      ENDIF
      MDStyUPC  = laLblInfo[lnItemRow,5]
      IF ALEN(laLblInfo, 2) > 6
        MDPCS = laLblInfo[lnItemRow, 7]
      ENDIF
           
      *-- Print the current item line
      =SEEK(lcLabelVersion + 'P')
      SCAN REST WHILE cVer + cEDIType = lcLabelVersion + 'P'
        STORE DATA TO MDATA
        lcString = &Mdata
        =FPUTS(lnFileHandle , lcString)
      ENDSCAN
    ENDFOR
    lnStartItem = lnStartItem + lnLinesNumber
    
    *-- Print the lines footer
    lcString = ''
    =SEEK(lcLabelVersion + 'F')
    SCAN REST WHILE cVer + cEDIType = lcLabelVersion + 'F'
      IF !EMPTY(lcString)
        =FPUTS(lnFileHandle , lcString)
      ENDIF
      STORE DATA TO MDATA
      lcString = &Mdata
    ENDSCAN
    =FPUTS(lnFileHandle, lcString)      
   
  ENDFOR
ELSE
  STORE '' TO MDSTYLE1 , MDSTYLE2 , MDSTYLE3 , MDSTYLE4 , MDSTYLE5 ,;
              MDSTYLE6 , MDSTYLE7 , MDSTYLE8 , MDSTYLE9 , MDSTYLE10,;
              MDSTYLE11, MDSTYLE12, MDSTYLE13, MDSTYLE14, MDSTYLE15,;
              MDSTYLE16, MDSTYLE17, MDSTYLE18, MDSTYLE19, MDSTYLE20
              
  STORE '' TO MDSTYMAJ1 , MDSTYMAJ2 , MDSTYMAJ3 , MDSTYMAJ4 , MDSTYMAJ5 ,;
              MDSTYMAJ6 , MDSTYMAJ7 , MDSTYMAJ8 , MDSTYMAJ9 , MDSTYMAJ10,;
              MDSTYMAJ11, MDSTYMAJ12, MDSTYMAJ13, MDSTYMAJ14, MDSTYMAJ15,;
              MDSTYMAJ16, MDSTYMAJ17, MDSTYMAJ18, MDSTYMAJ19, MDSTYMAJ20
              
  STORE '' TO MDCOLOR1 , MDCOLOR2 , MDCOLOR3 , MDCOLOR4 , MDCOLOR5 ,;
              MDCOLOR6 , MDCOLOR7 , MDCOLOR8 , MDCOLOR9 , MDCOLOR10,;
              MDCOLOR11, MDCOLOR12, MDCOLOR13, MDCOLOR14, MDCOLOR15,;
              MDCOLOR16, MDCOLOR17, MDCOLOR18, MDCOLOR19, MDCOLOR20
              
  STORE '' TO MDSKU1 , MDSKU2 , MDSKU3 , MDSKU4 , MDSKU5 ,;
              MDSKU6 , MDSKU7 , MDSKU8 , MDSKU9 , MDSKU10,;
              MDSKU11, MDSKU12, MDSKU13, MDSKU14, MDSKU15,;
              MDSKU16, MDSKU17, MDSKU18, MDSKU19, MDSKU20
              
  STORE '' TO MDSIZDES1 , MDSIZDES2 , MDSIZDES3 , MDSIZDES4 , MDSIZDES5 ,;
              MDSIZDES6 , MDSIZDES7 , MDSIZDES8 , MDSIZDES9 , MDSIZDES10,;
              MDSIZDES11, MDSIZDES12, MDSIZDES13, MDSIZDES14, MDSIZDES15,;
              MDSIZDES16, MDSIZDES17, MDSIZDES18, MDSIZDES19, MDSIZDES20
              
  STORE 0  TO MDQTY1 , MDQTY2 , MDQTY3 , MDQTY4 , MDQTY5 ,;
              MDQTY6 , MDQTY7 , MDQTY8 , MDQTY9 , MDQTY10,;
              MDQTY11, MDQTY12, MDQTY13, MDQTY14, MDQTY15,;
              MDQTY16, MDQTY17, MDQTY18, MDQTY19, MDQTY20
               
  STORE '' TO MDSTYUPC1 , MDSTYUPC2 , MDSTYUPC3 , MDSTYUPC4 , MDSTYUPC5 ,;
              MDSTYUPC6 , MDSTYUPC7 , MDSTYUPC8 , MDSTYUPC9 , MDSTYUPC10,;
              MDSTYUPC11, MDSTYUPC12, MDSTYUPC13, MDSTYUPC14, MDSTYUPC15,;
              MDSTYUPC16, MDSTYUPC17, MDSTYUPC18, MDSTYUPC19, MDSTYUPC20
              
  STORE 0  TO MDPCS1 , MDPCS2 , MDPCS3 , MDPCS4 , MDPCS5 , ;
              MDPCS6 , MDPCS7 , MDPCS8 , MDPCS9 , MDPCS10, ;
              MDPCS11, MDPCS12, MDPCS13, MDPCS14, MDPCS15, ;
              MDPCS16, MDPCS17, MDPCS18, MDPCS19, MDPCS20
              
  STORE 0  TO INPACKQTY1 , INPACKQTY2 , INPACKQTY3 , INPACKQTY4 , INPACKQTY5 ,;
              INPACKQTY6 , INPACKQTY7 , INPACKQTY8 , INPACKQTY9 , INPACKQTY10,;
              INPACKQTY11, INPACKQTY12, INPACKQTY13, INPACKQTY14, INPACKQTY15,;
              INPACKQTY16, INPACKQTY17, INPACKQTY18, INPACKQTY19, INPACKQTY20                
  =SEEK(EdiAcPrt.cPartCode+'850','EDIPD')
  FOR lnCount = 1 TO MIN(20 , ALEN(laLblInfo , 1))
    lcCount           = ALLTRIM(STR(lnCount , 2))
    MDSKU&lcCount     = laLblInfo[lnCount,1]
    MDQTY&lcCount     = laLblInfo[lnCount,2]
    MDSIZDES&lcCount  = laLblInfo[lnCount,3]
    MDSTYLE&lcCount   = laLblInfo[lnCount,4]
    IF ALEN(laLblInfo,2) => 8
      INPACKQTY&lcCount  = laLblInfo[lnCount,8]
    ENDIF 
    MDSTYMAJ&lcCount  = LEFT(laLblInfo[lnCount,4] , lnMajorLen)
    IF llUseColor
      MDCOLOR&lcCount = SUBSTR(laLblInfo[lnCount,4] , lnClrPos , lnClrLen)
    ENDIF
    MDSTYUPC&lcCount  = laLblInfo[lnCount, 5]
    IF ALEN(laLblInfo, 2) => 7
      MDPCS&lcCount  = laLblInfo[lnCount, 7]
    ENDIF
    IF lnCount = 1 
      ShortSKU = LFREAD(7,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU1,'Detail','850','PO1')
      LongSku  = LFREAD(9,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU1,'Detail','850','PO1')
      ITEMDESC = LFREAD(2,ALLTRIM(Asn_Ship.CUSTPO),'Header','850','PO1')
    Else
      ShortSKU = IIF(ShortSku = LFREAD(7,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU&lcCount,'Detail','850','PO1'),ShortSku,"PREPACK")
      LongSku  = IIF(LongSku  = LFREAD(9,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU&lcCount,'Detail','850','PO1'),LongSku ,Left(LongSku,Len(LongSku)-3) +"***")
    EndIf  
  ENDFOR
  IF ALEN(laLblInfo , 1) > 20
    lcErrorText = lcErrorText +;
      "BOL# " + ALLTRIM(ASN_SHIP.Bol_No) +;
      " Carton# " + ALLTRIM(STR(ASN_SHIP.Cart_No)) +;
      " is packing more than twenty Style\Color\Size" + CHR(13)
  ENDIF
  =SEEK(EdiAcPrt.cPartCode+'856','EDIPD')
  lcString = ''
  =SEEK(lcLabelVersion + 'L')
  SCAN REST WHILE cVer + cEDIType = lcLabelVersion + 'L'
    IF !EMPTY(lcString)
      =FPUTS(lnFileHandle , lcString)
    ENDIF
    STORE DATA TO MDATA
    lcString = &Mdata
  ENDSCAN
  =FPUTS(lnFileHandle, lcString)  
ENDIF
lcString = '   '
=FPUTS(lnFileHandle, lcString)

*!**************************************************************************
*! Name      : lfRead
*! Developer : Hassan Ali
*! Date      : 06/08/2001
*! Purpose   : Read EDI file seg. or Specified Field value From "edisv" Table
*!             AND RETURN THE FIELD VALUE OR THE SEG VALUES ALL
*!**************************************************************************
*! Call From : The Mapping tables
*!**************************************************************************
*! Param(s)  :  nFld_no    :The field number(N) or (.F.) for Whole SEG.
*!              cKey_Value :The key value to detect the value
*!     		   	LEVEL      :THE lcPROCESSLEVEL
*!     	       cTran_type :THE TRANSACTION (WE NEED TO GET FROM ) TYPE
*!**************************************************************************
*! Returns   : The field value (OR THE FIRST FIELD VALUE IN CASE OF ALL SEG)
*!**************************************************************************
*! Example   : = lfRead(nFld_no,cKey_Value,PROCESSLEVEL,cTran_type)
*!**************************************************************************
*:->
FUNCTION lfREAD
Parameters lnFld_No, lcKey_Value, lcProcessLevel, lcTran_Type, lcSegId          
PRIVATE lnArea,  lcRet

lcSegId = IIf(Type('lcSegId')="C",lcSegId,mSegId)            
lcRet   = ""
lcField_Sep = Eval(AllTrim(EdiPh.cFieldSep))
If Seek( EdiAcPrt.cPartCode+EdiPd.cVersion+lcTran_Type+lcSegId+;
         Padr(lcProcessLevel, 50, " ") +Padr(lcKey_Value    , 60, " "),'EdiSv' )

  lcRet  = Allt(EdiSv.cValue)
  nOcur = Occur(lcField_Sep, lcRet)
  nLOC1 = At(lcField_Sep, lcRet, lnFld_No  )
  nLOC2 = At(lcField_Sep, lcRet, lnFld_No+1)
  Do Case
    Case nLoc1 <> 0 And nLoc2 <> 0
      lcRet = Substr(lcRet, nLoc1 + 1, nLoc2 - nLoc1 -1 )
    Case nLoc1 <> 0 And nLoc2 = 0
      lcRet = Substr(lcRet, nLoc1+1)
  EndCase  
ENDIF
RETURN lcRet

*!*************************************************************
*! Name      : lfGetBOL
*! Developer : HEND GHANEM (HBG)
*! Date      : 06/12/2000
*! Purpose   : Check IF there is an open BOL use it other wise generate new one
*!*************************************************************
*! Parameters: lcAcc      : The account for this packing list
*!           : lcStore    : The Store for this packing list
*!           : lcLoc      : The WareHouse for this packing list
*!           : lcShpVia   : The ShipVia for this packing list
*!           : lcStnCartn : The defult value of the field "StanCarton" for the BOL_HDR
*!           : lcToStorCn : Ship to store or consolidator
*!           : lcDivision : Division
*!*************************************************************
*! Returns   : BOL #
*!*************************************************************
*! Example   : lfGetBOL()
*!*************************************************************
FUNCTION lfGetBOL
PARAMETERS lcAcc, lcStore, lcLoc, lcShpVia , lcStnCartn, lcToStorCn,lcDivision

m.BOL_NO=''
IF llBolAsi
  lnBol= ASCAN(laBOLInfo,lcAcc+lcStore+lcLoc+lcShpVia+lcStnCartn+lcToStorCn)
  IF lnBol <> 0
    RETURN laBOLInfo[lnBol+1]
  ENDIF
  lnAlias = SELECT()
  IF llUseExs
    SELECT BOL_HDR
    LOCATE FOR Account+Store   +W_Code+ShipVia +StanCarton = ;
               lcAcc  +lcStore +lcLoc +lcShpVia+lcStnCartn ;
               AND Status <> "C"
    IF FOUND()
      m.BOL_NO= BOL_NO
    ENDIF
  ENDIF
  IF EMPTY(m.BOL_NO)
    *-- Generate new BOL 
    m.BOL_NO = gfSequence('BOL_NO')
    *-- Get the value of carreier code which is related field to Shipvia code
    DECLARE laVRltFld[1,2]
    laVRltFld[1,1] = 'CARRIERCOD'
    laVRltFld[1,2] = 'lcCarCod'
    lcCarCod = ''
    =gfRltFld(lcShpVia ,@laVRltFld,'SHIPVIA   ')
    
    *-- Insert record to the new BOL in BOL_HDR file.  
    m.ACCOUNT    = lcAcc
    m.STORE      = lcStore
    m.W_CODE     = lcLoc  
    m.ShipVia    = lcShpVia
    m.CarrierCod = lcCarCod
    m.StanCarton = lcStnCartn
    m.cToStorCn  = lcToStorCn
    m.BolDate    = gdSysDate
    m.cBolTime   = Time()
    m.cDivision  = lcDivision
    m.cTranMthd  = 'M'
    m.packtype   = "CTN25"
    INSERT INTO BOL_HDR FROM MEMVAR
  ENDIF
  DIMENSION laBOLInfo[ALEN(laBOLInfo,1)+1,2]
  laBOLInfo[ALEN(laBOLInfo,1),1] = lcAcc+lcStore+lcLoc+lcShpVia+lcStnCartn+lcToStorCn
  laBOLInfo[ALEN(laBOLInfo,1),2] = m.BOL_NO
  SELECT (lnAlias)

ENDIF
RETURN m.BOL_NO
