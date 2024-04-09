*:************************************************************************
*: Program file  : MFPROJ.PRG
*: Program desc. : Project management/Project information (N00341)
*: For screen    : MFPROJ
*:         System: ARIA APPAREL SYSTEM
*:      Developer: HEND GHANEM
*:************************************************************************
*: Calls         : MFPROJ.SPX
*:                 MFPROJA.SPX      
*:*************************************************************************
*: Passed Parameters  : None
*:*************************************************************************
*: Example            : DO MFPROJ
*:************************************************************************
*:E301869,1 HBG 07/04/2002 Generate project automaticlly for SO , C/T , P/O
*:E301869,2 AMH 11/19/2002 Create a production schedule project according to a setting.
*:B038431,1 TMI 08/21/2004 use laPrjTyp[puPrjTyp,1] instead of lcBrFlTtl variable
*:B127792,1 HBG 07/05/2005 In case of check last start date Re-Calculate the start date 
*:B127792,1                back if the complete date is falls after the project require date
*:E302251,1 AMH 8/31/2005  Add new project types and Calling the Notify Seting screen.
*:************************************************************************
PRIVATE lcBrTtl, lcSetExact, lnBrRecNo, lnCurAlias, lnNumOfLin, lcFuncName,;
        lcFileName,lcBrowTitl

STORE '' TO lcOldVal, lcFuncName , lcModal 
lcBrTtl    = 'Tasks Data'
lcBrowTitl = 'Project History'
lcPathFlds = [cPath_ID : H = 'Template ID', cPath_Dsc : H = 'Template Name']
lcOprFlds  = [cOprt_ID : H = 'Task ID', cOprt_Dsc : H = 'Task Name', cOprt_Res:H='Responsible', nest_dur : H = 'Duration']
lcCtgFlds  = [cOprt_Ctg: H = 'Category', cCtg_Dsc : H = 'Description']
lcCtgTtl   = 'Categories of Tasks'
lcCalFlds  = [cCal_ID : H = 'Calendar ID', cCal_Des : H = 'Description']
lcCalTtl   = 'Calendars'
llDisLstst = .F.
llFromOper = .F.
lnOprWdth = 0
STORE '' TO lcEditStat,lcCurStSta,lcPathStat,lcEstDStat,lcPrjTypSt
llAddToAdt = .F.
STORE .F. TO m.lSchedual,llSchedual,llEnbSave
STORE '' TO lcBrowCurs,lc_PMPrjDt,lc_PrjHist,lc_PrjAudt,lc_PMPrjRl,lc_Parser
STORE '' TO lcPrjID,lcmPrj_Typ,lcmPrj_ID,lcmcStyle,lcmcpathid,lcpath_dsc,lcmcPrjSDs,lcmcPrjStt,lcProj
STORE 0 TO lnCurStage,lnlatStrt
STORE .F. TO llUpdStrtd,llFromFld
lcdreqstrt = {}
lcdeststrt = {}
lcdclcstrt = {}
lcdactstrt = {}
lcdDtaDate = {}
lcdreqfnsh = {}
lcdestfnsh = {}
lcdclcfnsh = {}
lcdactfnsh = {}
ldOldEstS = {}
ldOldEstF = {}
puPrjTyp   = 1
lcPrj_ID   = SPACE(6)
lcStyle    = SPACE(12)

STORE '' TO lcMjrPct   
STORE 0  TO lnstylewid
STORE SPACE(10) TO lcoprt_res 
STORE '' TO lcBrowCurs,lc_PMPrjDt,lc_PMPrjRl,lc_Parser

*E302251,1 AMH Add new project types [Start]
*DECLARE laOpertion[1], laPredecsr[1], laUpdtMthd[1], laUsers[1],;
        laNotified[1], laPrjTypes[3,6], laStatus[5], laCategrie[1],;
        laStages[5,2], laCurStage[5,2],laPrjTyp[1,2] 
DECLARE laOpertion[1], laPredecsr[1], laUpdtMthd[1], laUsers[1],;
        laNotified[1], laPrjTypes[4,6], laStatus[5], laCategrie[1],;
        laStages[5,2], laCurStage[5,2],laPrjTyp[1,2] 
*E302251,1 AMH [End]

laUpdtMthd[1] = 'Manual'
laStatus[1]   = ' may not be selected.'
laStatus[2]   = ' is Complete.'
laStatus[3]   = ' is Cancelled.'                       
laStatus[4]   = ' is summarized.'
laStatus[5]   = ' is actualized.'

lcStages = 'PICXH'
laStages[1,1] = 'Planning'
laStages[1,2] = 'P'
laStages[2,1] = 'In Progress'
laStages[2,2] = 'I'
laStages[3,1] = 'Complete'
laStages[3,2] = 'C'
laStages[4,1] = 'Cancelled'
laStages[4,2] = 'X'
laStages[5,1] = 'History'
laStages[5,2] = 'H'
=ACOPY(laStages, laCurStage)
STORE laStages[1,1] TO lcCurStage
puCurStage = 1

*E301869,2 AMH Change the project type from Cutting Ticket to Manufacturing [Start]
*laPrjTypes[1,1] = 'Cutting Ticket'
laPrjTypes[1,1] = 'Manufacturing'
*E301869,2 AMH [End]

laPrjTypes[1,2] = 'C'
laPrjTypes[1,3] = 'CUTTKTH'
laPrjTypes[1,4] = 'CUTTKTL'
laPrjTypes[1,5] = [CutTkt  :H='C/T #'   ,]+;
                  [Style   :H='Style'   ,]+;
                  [Status  =IIF(Status='O', 'Open', 'Hold'):H='Status',]+;
                  [Entered :H='Issue'   ,]+;
                  [Complete:H='Complete',]+;
                  [Season  :H='Se'      ,]+;
                  [cDivision:H='Di'      ,]+;
                  [Pcs_Bud :H='Budget':P='999999',]+;
                  [Pcs_Rec :H='Recvd' :P='999999',]+;
                  [Pcs_Dam :H='Damged':P='999999',]+;
                  [Pcs_Opn :H='Open'  :P='999999' ]
laPrjTypes[1,6] = 'CUTTKT'

laPrjTypes[2,1] = 'Style Purchase Order'
laPrjTypes[2,2] = 'P'
laPrjTypes[2,3] = 'POSHDR'
laPrjTypes[2,4] = 'POSLN'
laPrjTypes[2,5] = [cStyType:H='P/O Type':8,]+;
                  [PO      :H='P/O #',]+;
                  [Status  =IIF(Status='O', 'Open', 'Hold'):H='Status',]+;
                  [Vendor   ,]+;
	              [Entered  ,]+;
	              [Complete ,]+;
  	              [nstyorder:H='Tot.Qty.':7,]+;
                  [POTotal  :H='Amount':10,]+;
                  [Receive  :R :H='Receive':7,]+;
                  [Open     :R :H='Open':7]
laPrjTypes[2,6] = 'PO'

laPrjTypes[3,1] = 'Sales Order'
laPrjTypes[3,2] = 'O'
laPrjTypes[3,3] = 'ORDHDR'
laPrjTypes[3,4] = 'ORDLINE'
laPrjTypes[3,5] = [cOrdType: H='Order Type':10,]+;
                  [Order   : H='Order#',]+;
                  [Status  = IIF(Status='O', 'Open', 'Hold'):H='Status',]+;
                  [Season  : H='SE',]+;
                  [cDivision: H='DI',]+;
                  [Account : H='Account',]+;
                  [Store=IIF(MULTI='Y','*Multi*',STORE):H='Store']
laPrjTypes[3,6] = 'ORDER'

*E302251,1 AMH Add new project types [Start]
laPrjTypes[4,1] = 'Product Development'
laPrjTypes[4,2] = 'S'
laPrjTypes[4,3] = 'STYLE'
laPrjTypes[4,4] = ''
laPrjTypes[4,5] = [cStyMajor : H='Style',]+;
                  [Desc  : H='Description',]+;
                  [Status  = IIF(Status='A', 'Active','On Hold'):H='Status',]+;
                  [Season  : H='SE',]+;
                  [cDivision: H='DI']
laPrjTypes[4,6] = 'CSTYMAJOR'
*E302251,1 AMH [End]

DO CASE
  CASE gcAct_Appl = 'MF'
    STORE 'Cutting Ticket' TO lcPrjType
    puPrjTyp = 1
    puPrjType = 1
    DIMENSION laPrjTyp[3,2]
    laPrjTyp[1,1] = 'Cutting Ticket'
    laPrjTyp[1,2] = ''
    laPrjTyp[2,1] = 'Adornment Order'
    laPrjTyp[2,2] = 'A'
    laPrjTyp[3,1] = 'Dye Order'
    laPrjTyp[3,2] = 'D'
    lcPrjPre  = ''
    lcPrjTypSt = 'ENABLE'
  CASE gcAct_Appl = 'PO'
    STORE 'Style Purchase Order' TO lcPrjType
    puPrjTyp  = 1
    puPrjType = 2
    DIMENSION laPrjTyp[3,2]
    laPrjTyp[1,1] = 'Purchase Order'
    laPrjTyp[1,2] = 'P'
    laPrjTyp[2,1] = 'Inter-Location P/O'
    laPrjTyp[2,2] = 'N'
    laPrjTyp[3,1] = 'Return P/O'
    laPrjTyp[3,2] = 'R'
    lcPrjPre  = 'P'
    lcPrjTypSt = 'ENABLE'
  CASE gcAct_Appl = 'SO'
    STORE 'Sales Order' TO lcPrjType
    puPrjTyp = 1
    puPrjType = 3
    DIMENSION laPrjTyp[2,2]
    laPrjTyp[1,1] = 'Sales Order'
    laPrjTyp[1,2] = 'O'
    laPrjTyp[2,1] = 'EDI temporary Order '
    laPrjTyp[2,2] = 'T'
    lcPrjPre  = 'O'
    lcPrjTypSt = 'ENABLE'    
  
  *E302251,1 AMH Add new project types [Start]
  CASE gcAct_Appl = 'IC'
    STORE 'Production' TO lcPrjType
    puPrjTyp = 1
    puPrjType = 4
    DIMENSION laPrjTyp[2,2]
    laPrjTyp[1,1] = 'Style'
    laPrjTyp[1,2] = 'S'
    laPrjTyp[2,1] = 'Other'
    laPrjTyp[2,2] = 'H'
    lcPrjPre  = 'S'
    lcPrjTypSt = 'ENABLE'    
  *E302251,1 AMH [End]
  
ENDCASE


STORE 'Project ID' TO lcPrjID
STORE 'Project Type' TO lcProj
STORE '' TO lcPrjFile, lcLinsFile, lcPrjFld , lcPrjFldSt, lcpath_dsc ,;
         lcPrj_Typ 

lcPrjBrFld  = "cPrj_Typ = RIGHT(laPrjTypes[AT(cPrj_Typ,'CPS'),1],14):H = 'Project Type':15,"+;
               "cPrj_ID  : H='Project ID',"+;
               "cStyle   : H='Style',"+;
               "cPrj_SDsc: H='Project Description'"     
=lfvPrjType()


lcTRemove    = 'Remo\<ve'
lcTVoid      = '\<Void'

lcTNotFound  = ' is not found in ' 

lcEnbCntrl   = SCHEME(1,9)
lcDisCntrl   = SCHEME(1,10)
lcSelCntrl   = SCHEME(1,6)

lcFileName   = SPACE(25) 
STORE 0 TO lnNumOfLin, lnBrRecNo 
ldCurDate    = gdSysDate
STORE .F. TO llBrowse, glUpdated, llMouseClic, llNewPath
lnCurObj = 1 

*--ToolBar external proc.
laDefProc[7]  = .F.              && Disable the control panel delete proc. (lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.   (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel save proc.   (lpClsScr)
PRIVATE lcOldBrFild
lcOldBrFild = ""
               
DECLARE laHolidays[1]
laHolidays = ''
lnRows  = 0

STORE .F. TO  llOper , llPathSel

*E302251,1 AMH Define variables for Nofity Setting Screen [Start]
lcBrUser   = "Users"
lcPMPRJNTF = gfTempName()
*E302251,1 AMH [End]

IF !gfSetup()
  RETURN
ENDIF

DO CASE
  CASE gcAct_Appl = 'MF'
   =gfOpenFile(gcDataDir + 'CUTTKTH' , 'CUTTKTH' , 'SH')
   =gfOpenFile(gcDataDir + 'CUTTKTL' , 'CUTTKTL' , 'SH')
   =gfOpenFile(gcDataDir + 'POSHDR', 'POSHDR' , 'SH')
   =gfOpenFile(gcDataDir + 'POSLN' , 'POSLN' , 'SH')
  CASE gcAct_Appl = 'PO'
   =gfOpenFile(gcDataDir + 'POSHDR', 'POSHDR' , 'SH')
   =gfOpenFile(gcDataDir + 'POSLN' , 'POSLN' , 'SH')
ENDCASE

m.lSchedual = llSchedual
m.cPrj_Typ  = lcmPrj_Typ
m.cPrj_ID   = lcmPrj_ID  
m.cStyle    = lcmcStyle 
m.cpath_id  = lcmcpathid
m.cPrj_SDsc = lcmcPrjSDs
m.cPrj_Stts = lcmcPrjStt
m.dreq_strt = lcdreqstrt
m.dest_strt = lcdeststrt
m.dclc_strt = lcdclcstrt
m.dact_strt = lcdactstrt
m.dDta_Date = lcdDtaDate
m.dreq_fnsh = lcdreqfnsh
m.dest_fnsh = lcdestfnsh
m.dclc_fnsh = lcdclcfnsh
m.dact_fnsh = lcdactfnsh
puCurStage  = lnCurStage

lcOldBrFild = lcBrfields

lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)

IF !WEXIST(gcBaseWind)
  
  lcScFields = 'cprj_typ,cprj_id,cstyle,cprj_sdsc,cpath_id,cprj_stts,dest_strt,dest_fnsh,;
                dact_strt,dact_fnsh,dclc_strt,dclc_fnsh,dreq_strt,dreq_fnsh,mprj_desc,;
                ddta_date,lschedual'
  
  lcBrowCurs   = gfTempName()
  lc_PMPrjDt   = gfTempName()
  lc_PrjHist   = gfTempName()
  lc_PrjAudt   = gfTempName()
  lc_PMPrjRl   = gfTempName()
  lc_Parser    = gfTempName()
  
  SELECT PMCALDT
  SCAN
    ldCal_HFrm = dCal_HFrm
    DO WHILE ldCal_HFrm <= dCal_HTo
      lnRows  = lnRows  + 1
      DIMENSION laHolidays[lnRows]
      laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
      ldCal_HFrm = ldCal_HFrm + 1
    ENDDO  
  ENDSCAN

  *-- Create a table with the same structure as PMPRJDT 
  *-- adding two more fields for saving.
  SELECT PMPRJDT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru, 1)
  DIMENSION laFileStru(lnFileStru + 3, 4)
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cStatus'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nRecNo'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 10
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cMComplt'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 3
  laFileStru[lnFileStru ,4] = 0
  
  CREATE CURSOR (lc_PMPrjDt) FROM ARRAY laFileStru
  INDEX ON cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID;
        TAG PMPRJDT OF (lc_PMPrjDt)
  INDEX ON cPrj_Typ + cPrj_ID + cStyle + cCtg_Seq + cOprt_Seq;
        TAG PMPRJDTS OF (lc_PMPrjDt)
  INDEX ON cPrj_Typ+cPrj_Id+cOprt_Ctg+cOprt_Id+cOprt_Res;
                TAG PMPRJUSR OF (lc_PMPrjDt)
  *B127792,1 HBG 07/05/2005 Add index to get the correct start date of the project [Begin]
  INDEX ON cPrj_Typ+cPrj_Id+cStyle+DTOS(dEst_strt);
                TAG (lc_PMPrjDt) OF (lc_PMPrjDt)
  *B127792,1 [End]                
  CREATE CURSOR (lc_PrjHist) (cOpr_ID C(20),cUser_ID C(10),cRemain C(3),cCompDate C(8),cActnDate C(8),cStatus C(15))
  INDEX ON cOpr_ID TAG PMPRJDT OF (lc_PrjHist)
  
  CREATE CURSOR (lc_PrjAudt) FROM ARRAY laFileStru
  INDEX ON cOprt_Ctg + cOprt_ID TAG PMPRJDT OF (lc_PrjAudt)

  *-- Create a dummy cursor to hold the order styles.
  CREATE CURSOR (lcBrowCurs);
        (cStyle C(12))

  CREATE CURSOR (lc_Parser);
     (cOprt_Ctg C(3), cOprt_ID C(5), dStrtDate D(8), nDurIndic N(1)) 
      
  SELECT PMPRJRL
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru, 1)
  DIMENSION laFileStru(lnFileStru + 2, 4)
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cStatus'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nRecNo'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 10
  laFileStru[lnFileStru ,4] = 0

  CREATE CURSOR (lc_PMPrjRl) FROM ARRAY laFileStru
  INDEX ON cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg  + cPrd_ID;
        TAG PMPRJRLP OF (lc_PMPrjRl)
  INDEX ON cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID;
        TAG PMPRJRL  OF (lc_PMPrjRl)

  SELECT cUser_ID ;
    FROM (gcSysHome + 'SYUUSER');
    INTO ARRAY laUsers;
   ORDER BY cUser_ID

  SELECT (lc_PMPrjDt)
  SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID INTO (lc_PMPrjRl)

  SELECT PMPTHDT
  SET RELATION TO cPath_ID + cOprt_Ctg + cOprt_ID INTO PMPTHRL
 
  SELECT PMPRJDT
  SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID INTO PMPRJRL

  SELECT PMPRJHD
  SET RELATION TO cPrj_Typ + cPrj_ID + cStyle INTO PMPRJDT


  GO TOP
  SCATTER MEMVAR MEMO BLANK

  *-- Display status of screen objects
  lcKeyStat = 'ENABLE'
  STORE 'DISABLE' TO lcEditStat, lcPathStat, lcEstDStat, lcCurStSta 

  lnOprWdth = '36'  
  
  *E302251,1 AMH Create cursor to hold data of Nofity Setting Screen [Start]
  CREATE CURSOR (lcPMPRJNTF) (CPrj_Typ   C(1) ,CPrj_ID    C(6),CStyle     C(12) ,COprt_Ctg  C(3)  ,COprt_ID   C(5)  ,;
                              CUser_ID   C(10),LBfrStrt   L   ,NBfrStrtDy N(3,0),LBfrCmplt  L     ,NBfrCmplDy N(3,0),;
                              LOnStrt    L    ,LOnCmplt   L   ,LOnRedrct  L     ,NStrtDelay N(3,0),NCmplDelay N(3,0),;
                              CEMAIL_ADD C(60),cStatus    C(1))
  SELECT (lcPMPRJNTF)
  INDEX ON CPrj_Typ+CPrj_ID+CStyle+COprt_Ctg+COprt_ID+CUser_ID TAG (lcPMPRJNTF) OF (lcPMPRJNTF)
  =gfOpenFile(gcDatadir+'PMPRJNTF','PMPRJNTF','SH')
  =gfOpenFile(gcDatadir+'PMPTHNTF','PMPTHNTF','SH')
  =gfOpenFile(gcDatadir+'PMPCTGNT','PMPCTGNT','SH')
  *E302251,1 AMH [End]
  
ENDIF

SELECT PMPRJHD
SET FILTER TO cPrj_Typ + cPrj_ID + cStyle = lcPrj_Typ

PUSH KEY
=gfClearKey()

REGIONAL  lcClnUpFun, llExitLoop
STORE .F. TO llExitLoop 
lcClnUpFun = "llExitLoop"


DO WHILE !llExitLoop
  PUSH KEY
  ON KEY LABEL ESC DO  lpEscScr
  DO (gcScrDir+"MFPROJ.SPR")
  POP KEY
  llExitLoop = .T.
  IF !llOper 
    EXIT
  ENDIF
  IF !EMPTY(lcFuncName)
    &lcFuncName
    lcBrfields  = lcOldBrFild
    lcFuncName = ' '
    lcKeyStat   = 'DISABLE'
    lcEditStat  = 'ENABLE'
    lcCurStSta  = IIF(laScrMode[4], 'DISABLE', 'ENABLE')
    lcPathStat  = IIF(lnNumOfLin > 0  , 'DISABLE', 'ENABLE' )
    lcEstDStat  = IIF(EMPTY(m.dclc_strt), 'ENABLE' , 'DISABLE')
    lcPrjTypSt = IIF(laScrMode[1], 'ENABLE', 'DISABLE')
  ENDIF  
ENDDO

POP KEY


IF glQuitting
  SELECT PMPRJHD
  SET RELATION TO
  SET FILTER TO
  IF USED(lc_PMPrjDt)
    USE IN (lc_PMPrjDt)
    ERASE (gcWorkDir+lc_PMPrjDt+'.DBF')
    ERASE (gcWorkDir+lc_PMPrjDt+'.CDX')
  ENDIF
  IF USED(lc_PMPrjRl)  
    USE IN (lc_PMPrjRl)  
    ERASE (gcWorkDir+lc_PMPrjRl+'.DBF')
    ERASE (gcWorkDir+lc_PMPrjRl+'.CDX')
  ENDIF
  IF USED(lcBrowCurs)
    USE IN (lcBrowCurs)
    ERASE (gcWorkDir+lcBrowCurs+'.DBF')    
  ENDIF
  IF USED(lc_Parser)
    USE IN (lc_Parser)
    ERASE (gcWorkDir+lc_Parser+'.DBF')    
  ENDIF
  
  *E302251,1 AMH Remove cursor of Nofity Setting Screen [Start]
  IF USED(lcPMPRJNTF)
    USE IN (lcPMPRJNTF)
  ENDIF
  *E302251,1 AMH [End]
  
ENDIF
  
*!*************************************************************
*! Name      : lfClearRead
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Clears READ upon branching if in Add or Edit mode.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRead()
*!*************************************************************
FUNCTION lfClearRead

STORE .T. TO  llOper 
IF laScrMode[2] .OR. m.cPrj_Stts $ 'CX'
  =lfvOprtins()
ELSE
  lcFuncName = '=lfvOprtins()'
  lnCurObj   = _CUROBJ
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lpshow
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Adjusts display of objects according to the mode.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpshow()
*!*************************************************************
FUNCTION lpshow

DO CASE
  CASE laScrMode[1]
    
    *E302251,1 AMH Collecting Data for Nofity Setting Screen [Start]
    SELECT (lcPMPRJNTF)
    ZAP
    *E302251,1 AMH [End]
    
    SELECT PMPRJHD
    SCATTER MEMVAR MEMO BLANK
    lcpath_dsc  = SPACE(40)
    STORE laStages[1,1] TO lcCurStage
    =lfInitStage('*')
    DO CASE
      CASE gcAct_Appl = 'MF'
        STORE 'Cutting Ticket' TO lcPrjType
        lcPrjPre  = ''
        lcPrjTypSt = 'ENABLE'
      CASE gcAct_Appl = 'PO'
        STORE 'Style Purchase Order' TO lcPrjType
        lcPrjPre  = 'P'
        lcPrjTypSt = 'ENABLE'
      CASE gcAct_Appl = 'SO'
        STORE 'Sales Order' TO lcPrjType
        lcPrjPre  = 'O'
        lcPrjTypSt = 'ENABLE'    
    ENDCASE

    STORE 'Project ID' TO lcPrjID
    STORE 'Project Type' TO lcProj
    STORE '' TO lcPrjFile, lcLinsFile, lcPrjFld , lcPrjFldSt, lcpath_dsc ,;
                lcPrj_Typ 

    lcPrjBrFld  = "cPrj_Typ = RIGHT(laPrjTypes[AT(cPrj_Typ,'CPS'),1],14):H = 'Project Type':15,"+;
                 "cPrj_ID  : H='Project ID',"+;
                 "cStyle   : H='Style',"+;
                 "cPrj_SDsc: H='Project Description'"     
    =lfvPrjType()

    puCurStage = 1
    puPrjTyp   = 1
    
    *E302251,1 AMH Add new project types [Start]
    *lcPrjID    = LEFT(laPrjTyp[puPrjTyp,1],20) + ' ID'
    lcPrjID    = IIF(lcPrj_Typ$"HS","Project",LEFT(laPrjTyp[puPrjTyp,1],20)) + ' ID'
    *E302251,1 AMH [End]
    
    lnlatStrt  = 0
    llUpdStrtd = .F.
    
    *E302251,1 AMH Add new project types [Start]
    *SHOW GET m.cPrj_ID   ENABLE
    *SHOW GET ibPrj_ID    ENABLE
    IF lcPrj_Typ = "S"
      SHOW GET m.cPrj_ID DISABLE
      SHOW GET ibPrj_ID  DISABLE
      SHOW GET m.cStyle  ENABLE
      SHOW GET ibStyle   ENABLE
    ELSE
      SHOW GET m.cPrj_ID ENABLE
      SHOW GET ibPrj_ID  ENABLE
      SHOW GET m.cStyle  DISABLE
      SHOW GET ibStyle   DISABLE
    ENDIF
    *E302251,1 AMH [End]
    
    SHOW GET puPrjTyp    ENABLE
    SHOW GET ibPath_ID   DISABLE
    SHOW GET m.cpath_id  DISABLE
    SHOW GET pbHistry    DISABLE
    SHOW GET pbNotes     DISABLE
    SHOW GET pbSchedule  DISABLE
    SHOW GET pbOprs      DISABLE
    SHOW GET puPrjType   DISABLE
    SHOW GET lnlatStrt   DISABLE
    SHOW GET lcPrjID
    =lfRefresh()
    _CUROBJ = OBJNUM(m.cPrj_ID)
           
  CASE laScrMode[2]
    
    IF llEnbSave
      llExitLoop = .T.
    ENDIF
    lcEditStat = 'ENABLE'
    DECLARE laOpertion[1], laPredecsr[1], laNotified[1]
    STORE '' TO laOpertion, laPredecsr, laNotified
    
    *E302251,1 AMH Collecting Data for Nofity Setting Screen [Start]
    SELECT (lcPMPRJNTF)
    ZAP
    *E302251,1 AMH [End]
    
    SELECT PMPRJHD
    SCATTER MEMVAR MEMO   

    SELECT cOprt_Ctg + '\' + cOprt_ID + ' ' +SUBSTR(cOprt_Dsc, 1, 18);
      FROM PMPRJDT  ;
     WHERE cPrj_Typ + cPrj_ID + cStyle = m.cPrj_Typ + m.cPrj_ID + m.cStyle;
      INTO ARRAY laOpertion
    
         
    STORE laStages[1,1] TO lcCurStage
    =lfInitStage('*')
    
    *E302251,1 AMH Collecting Data for Nofity Setting Screen [Start]
    SELECT PMPRJNTF
    IF SEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle)
      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID+CUSER_ID = m.cPrj_Typ + m.cPrj_ID + m.cStyle
        SCATTER MEMVAR
        m.cStatus = 'S'
        INSERT INTO (lcPMPRJNTF) FROM MEMVAR
      ENDSCAN
    ENDIF
    *E302251,1 AMH [End]
    
    lnlatStrt  = IIF(m.llastStrt,1,0) 
    llUpdStrtd = .F.

    puCurStage = 1
    SHOW GET ibPrjType   DISABLE
    SHOW GET puPrjType   DISABLE
    SHOW GET m.cPrj_ID   DISABLE
    SHOW GET ibPrj_ID    DISABLE
    SHOW GET puPrjTyp   DISABLE
    SHOW GET m.cStyle    DISABLE
    SHOW GET ibStyle     DISABLE
    SHOW GET m.cpath_id  DISABLE
    SHOW GET ibPath_ID   DISABLE
    SHOW GET m.cPrj_SDsc DISABLE
    SHOW GET m.dreq_strt DISABLE
    SHOW GET m.dreq_fnsh DISABLE
    SHOW GET m.dact_strt DISABLE
    SHOW GET m.dact_fnsh DISABLE
    SHOW GET m.dest_strt DISABLE
    SHOW GET m.lSchedual DISABLE
    SHOW GET lcOprt_Res  DISABLE
    SHOW GET puUser      DISABLE
    SHOW GET lnlatStrt   DISABLE
 
    IF m.cPrj_Stts = 'H'
      SHOW GET pbOprs     DISABLE
      SHOW GET pbModify   DISABLE
      SHOW GET pbSchedule DISABLE      
      SHOW GET pbEdt      DISABLE
      laCtrStat[7]  =  "DISABLE" 
    ELSE
      SHOW GET pbEdt   ENABLE
      laCtrStat[7]  =  "ENABLE" 
      SHOW GET pbOprs    ENABLE
      SHOW GET pbModify  ENABLE    
      IF m.cPrj_Stts $ 'PI' .AND. !EMPTY(laOpertion[1])
        SHOW GET pbSchedule ENABLE
      ELSE  
        SHOW GET pbSchedule DISABLE
      ENDIF
    ENDIF
    
    SHOW GET pbNotes     ENABLE
    SHOW GET pbHistry    ENABLE
    
    puCurStage = AT(m.cPrj_Stts, lcStages)
    lcCurStage = laCurStage[puCurStage, 1]
    SHOW GET ibCurStage  DISABLE
    SHOW GET puCurStage  DISABLE
    lcPath_Dsc = IIF(EMPTY(m.cPath_ID), SPACE(40),;
                     LOOKUP(PMPTHHD.cPath_Dsc, m.cPath_ID,;
                            PMPTHHD.cPath_ID, 'PMPTHHD'))
    =lfRefresh()      
    
  CASE laScrMode[3]

    llCUpDate  = .T.
    llExitLoop = .F.
    llOper     = .F.
    SHOW GET ibTabs ENABLE

          
    lNewPath = .F.
    lcEditStat = 'ENABLE'           
    IF !llEnbSave 
      DECLARE laOpertion[1], laPredecsr[1], laCategrie[1] 
      STORE '' TO laPredecsr, laOpertion, laCategrie
      
      =lfGetData()
      =lfInitStage(m.cPrj_Stts)
      puCurStage = AT(m.cPrj_Stts, lcStages)
      lcCurStage = laCurStage[puCurStage, 1]
    ENDIF  

    SHOW GET ibCurStage      ENABLE
    SHOW GET puCurStage      ENABLE

    IF m.cPrj_Stts $ 'PI'
      IF lnNumOfLin = 0
        SHOW GET ibPath_ID   ENABLE
        SHOW GET m.cpath_id  ENABLE
      ENDIF
      SHOW GET m.cPrj_SDsc ENABLE 
      SHOW GET m.dreq_strt ENABLE  
      SHOW GET m.dreq_Fnsh ENABLE   
      SHOW GET m.dact_strt ENABLE   
      SHOW GET m.dact_fnsh ENABLE   
      IF EMPTY(m.dclc_strt)
        SHOW GET m.dest_strt ENABLE  
        IF lnlatStrt = 0 
          SHOW GET lnlatStrt ENABLE
        ELSE
          SHOW GET lnlatStrt DISABLE      
        ENDIF  
      ENDIF  
    ENDIF  

    SHOW GET puPrjType   DISABLE
    SHOW GET pbSchedule  DISABLE
    SHOW GET pbNotes     ENABLE  
    SHOW GET pbHistry    ENABLE
    SHOW GET pbOprs      ENABLE  
    =lfRefresh()
    
    IF llEnbSave
      SHOW GET pbSav ENABLE  
      laCtrStat[11] = 'ENABLE'
      SHOW GET ibPrj_ID    DISABLE    
      SHOW GET m.cPrj_ID   DISABLE      
      SHOW GET puPrjTyp   DISABLE
    ENDIF
    
    IF llEnbSave OR llPathSel
      SHOW GET ibPath_ID   DISABLE
      SHOW GET m.cpath_id  DISABLE
    ENDIF    
    llDisLstst = .F.
    SET ORDER TO Coprusr IN SYSCHDUL
    SELECT PMPRJDT
    SET ORDER TO PMPRJDT
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
            SUBSTR(lcStyle,1,LEN(cStyle)))
       SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ))+;
                           SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle))
         *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
         *IF SEEK(PMPRJDT.cPrj_Typ + PMPRJDT.cPrj_ID + PMPRJDT.cOprt_Ctg+PMPRJDT.cOprt_ID,'SYSCHDUL');
         *        AND SYSCHDUL.COPERSTAT $ 'CP'
         IF SEEK(PMPRJDT.cPrj_Typ + PMPRJDT.cPrj_ID + PMPRJDT.cStyle + PMPRJDT.cOprt_Ctg+PMPRJDT.cOprt_ID,'SYSCHDUL');
                  AND SYSCHDUL.COPERSTAT $ 'CP'
         *B130984,1 HBG 12/04/2006 [End]

           llDisLstst = .T.
           EXIT
         ENDIF
       ENDSCAN
    ENDIF    
    IF llDisLstst 
      SHOW GET lnlatStrt DISABLE
    ENDIF
    =lfRefresh()
    
    *E301869,2 AMH Don't recalculate the estimated dates if is aleady calculated [Start]
    IF lnlatStrt = 1 AND !EMPTY(m.dest_fnsh)
      llUpdStrtd = .T.
    ENDIF
    *E301869,2 AMH [End]

  CASE laScrMode[4]

    glUpdated = .T.
    llNewPath = .F. 
    llCUpDate  = .T.    
    llExitLoop = .F.
    llOper     = .F.
        
    IF !llEnbSave
      DECLARE laOpertion[1], laPredecsr[1], laNotified[1], laCategrie[1]
      STORE '' TO laOpertion, laPredecsr, laNotified, laCategrie
      lnNumOfLin = 0    
    
      SELECT (lc_PMPrjRl)
      ZAP
    
      SELECT (lc_PMPrjDt)
      ZAP

      DO CASE 
        *-- 'S'ales Orders projects
        CASE lcPrj_Typ = 'O' OR lcPrj_Typ = 'T' 
          *-- Project Short description = Customer name + Style description
          m.cPrj_SDsc = ALLTRIM(ORDHDR.Account) + ', ' + ;
                        IIF(m.cStyle = REPLICATE('*', 12), 'all styles',;
                        LOOKUP(STYLE.Desc, m.cStyle, STYLE.STYLE, 'STYLE'))
          *-- Required start and finish dates.
          m.dReq_Strt = ORDHDR.Entered
          m.dReq_Fnsh = ORDHDR.Complete

        *-- Case Cutting Tickets projects
        CASE lcPrj_Typ = 'C'
          *-- Default style field with CUTTKTH.Style
          m.cStyle    = CUTTKTH.Style
          *-- Project Short description = Style description
          m.cPrj_SDsc = LOOKUP(STYLE.Desc, m.cStyle, STYLE.CSTYMAJOR, 'CSTYLE')
          *-- Required start and finish dates.
          m.dReq_Strt = CUTTKTH.Entered
          m.dReq_Fnsh = CUTTKTH.Complete

        *-- Case Purchase Orders projects
        CASE lcPrj_Typ = 'P' OR lcPrj_Typ = 'A' OR lcPrj_Typ = 'D' OR;
             lcPrj_Typ = 'N' OR lcPrj_Typ = 'R'
          *-- Project Short description = Vendor name + Style description
          m.cPrj_SDsc = ALLTRIM(POSHDR.Vendor) + ', ' + ;
                        IIF(m.cStyle = REPLICATE('*', 12), 'all styles',;
                        LOOKUP(STYLE.Desc, m.cStyle, STYLE.STYLE, 'STYLE'))
          *-- Required start and finish dates.        
          m.dReq_Strt = POSHDR.Entered
          m.dReq_Fnsh = POSHDR.Complete
        
      ENDCASE    
      m.dest_strt = ldCurDate
      ldOldEstS = m.dEst_Strt
      ldOldEstF = m.dEst_fnsh

      *-- Default the project status to 'P'lanning
      m.cPrj_Stts = 'P'
      STORE laStages[1,1] TO lcCurStage
      puCurStage  = 1 
    ENDIF
    SHOW GET ibCurStage  DISABLE
    SHOW GET puCurStage  DISABLE
    SHOW GET ibPrjType   DISABLE
    SHOW GET puPrjType   DISABLE
    SHOW GET ibPrj_ID    DISABLE    
    SHOW GET m.cPrj_ID   DISABLE
    SHOW GET puPrjTyp   DISABLE
    SHOW GET ibPath_ID   ENABLE
    SHOW GET m.cpath_id  ENABLE
    SHOW GET m.cPrj_SDsc ENABLE 
    SHOW GET m.dreq_strt ENABLE
    SHOW GET m.dreq_fnsh ENABLE        
    SHOW GET m.dact_strt ENABLE
    SHOW GET m.dact_fnsh ENABLE        
    SHOW GET m.dest_strt ENABLE
    SHOW GET lnlatStrt   ENABLE
    
    SHOW GET pbHistry    ENABLE
    SHOW GET pbNotes     ENABLE
    SHOW GET pbSchedule  DISABLE
    SHOW GET pbOprs      ENABLE    

    IF llEnbSave
      SHOW GET pbSav ENABLE  
      laCtrStat[11] = 'ENABLE'
      SHOW GET ibPrj_ID    DISABLE    
      SHOW GET m.cPrj_ID   DISABLE      
      SHOW GET puPrjTyp    DISABLE
    ENDIF
    
    IF llEnbSave OR llPathSel
      SHOW GET ibPath_ID   DISABLE
      SHOW GET m.cpath_id  DISABLE
    ENDIF
    
    IF EMPTY(m.cPrj_ID)
      SHOW GET lnlatStrt   DISABLE
    ENDIF
    
    =lfRefresh()
    
ENDCASE
lcPrj_ID  = m.cPrj_ID
lcStyle   = m.cStyle

llSchedual = m.lSchedual
lcmPrj_Typ = m.cPrj_Typ
lcmPrj_ID  = m.cPrj_ID
lcmcStyle  = m.cStyle
lcmcpathid = m.cpath_id
lcmcPrjSDs = m.cPrj_SDsc
lcmcPrjStt = m.cPrj_Stts
lcdreqstrt = m.dreq_strt
lcdeststrt = m.dest_strt
lcdclcstrt = m.dclc_strt
lcdactstrt = m.dact_strt
lcdDtaDate = m.dDta_Date
lcdreqfnsh = m.dreq_fnsh
lcdestfnsh = m.dest_fnsh
lcdclcfnsh = m.dclc_fnsh
lcdactfnsh = m.dact_fnsh
lnCurStage = puCurStage 

SELECT PMPRJHD
  
*!*************************************************************
*! Name      : lfwOldVals
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : When function of get fields.
*!             Stores a fields old value
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldVals()
*!*************************************************************
FUNCTION lfwOldVals
lcOldVal = EVALUATE('m.' + SYS(18))

*!*************************************************************
*! Name      : lfwOldFVal
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : When function of get fields.
*!             Stores a fields old value
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldFVal()
*!*************************************************************
FUNCTION lfwOldFVal
lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvProjTyp
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  lpshow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPrjType()
*!*************************************************************
FUNCTION lfvProjTyp

STORE laPrjTyp[puPrjTyp,2] TO lcPrj_Typ,m.cPrj_Typ,lcPrjPre

IF laPrjTyp[puPrjTyp,1] = 'Cutting Ticket'
  STORE 'C' TO lcPrj_Typ,m.cPrj_Typ
ENDIF

*E302251,1 AMH Add new project types [Start]
*lcPrjID    = LEFT(laPrjTyp[puPrjTyp,1],20) + ' ID'
lcPrjID    = IIF(lcPrj_Typ$"HS","Project",LEFT(laPrjTyp[puPrjTyp,1],20)) + ' ID'
*E302251,1 AMH [End]

IF gcAct_Appl = 'MF' 
 IF EMPTY(lcPrjPre)
   lcPrjFile  = 'CUTTKTH'
   lcLinsFile = 'CUTTKTL'
   lcPrjFldSt = [CutTkt  :H='C/T #'   ,]+;
                [Style   :H='Style'   ,]+;
                [Status  =IIF(Status='O', 'Open', 'Hold'):H='Status',]+;
                [Entered :H='Issue'   ,]+;
                [Complete:H='Complete',]+;
                [Season  :H='Se'      ,]+;
                [cDivision:H='Di'      ,]+;
                [Pcs_Bud :H='Budget':P='999999',]+;
                [Pcs_Rec :H='Recvd' :P='999999',]+;
                [Pcs_Dam :H='Damged':P='999999',]+;
                [Pcs_Opn :H='Open'  :P='999999' ]
   lcPrjFld   = 'CUTTKT'
 ELSE
   lcPrjFile  = 'POSHDR'
   lcLinsFile = 'POSLN'
   lcPrjFldSt = [cStyType:H='P/O Type':8,]+;
                [PO      :H='P/O #',]+;
                [Status  =IIF(Status='O', 'Open', 'Hold'):H='Status',]+;
                [Vendor   ,]+;
                [Entered  ,]+;
	            [Complete ,]+;
  	            [nstyorder:H='Tot.Qty.':7,]+;
                [POTotal  :H='Amount':10,]+;
                [Receive  :R :H='Receive':7,]+;
                [Open     :R :H='Open':7]
    lcPrjFld   = 'PO'
  ENDIF  
ENDIF
SHOW GET lcPrjID

*E302251,1 AMH Add new project types [Start]
IF lcPrj_Typ = "S"
  lcPrjFile  = 'STYLE'
  lcLinsFile = ''
  lcPrjFldSt = [cStyMajor : H='Style',]+;
               [Desc  : H='Description',]+;
               [Status  = IIF(Status='A', 'Active','On Hold'):H='Status',]+;
               [Season  : H='SE',]+;
               [cDivision: H='DI']
  lcPrjFld   = 'CSTYMAJOR'
  SHOW GET m.cPrj_ID DISABLE
  SHOW GET ibPrj_ID  DISABLE
  SHOW GET m.cStyle  ENABLE
  SHOW GET ibStyle   ENABLE
ELSE
  SHOW GET m.cPrj_ID ENABLE
  SHOW GET ibPrj_ID  ENABLE
  SHOW GET m.cStyle  DISABLE
  SHOW GET ibStyle   DISABLE
ENDIF

IF lcPrj_Typ = "H"
  lcPrjFile  = 'PMPRJHD'
  lcLinsFile = ''
  lcPrjFldSt = lcPrjBrFld
  lcPrjFld   = 'cPrj_ID'
ENDIF

_CUROBJ = OBJNUM(m.cPrj_ID)
*E302251,1 AMH [End]

=lfrefresh()

*!*************************************************************
*! Name      : lfvPrjType
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  lpshow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPrjType()
*!*************************************************************
FUNCTION lfvPrjType

*E302251,1 AMH Add new project types [Start]
*DIMENSION laPrjTypes[3,6]
DIMENSION laPrjTypes[4,6]
*E302251,1 AMH [End]

lcOldVal = lcPrjType

lcPrjType  = laPrjTyp[puPrjTyp,1]

lcSetExact = SET('EXACT')
SET EXACT ON
DO CASE
  CASE gcAct_Appl = 'MF'
    lnElem = 1
  CASE gcAct_Appl = 'PO'
    lnElem = 2
  CASE gcAct_Appl = 'SO'
    lnElem = 3
  
  *E302251,1 AMH Add new project types [Start]
  CASE gcAct_Appl = 'IC'
    lnElem = 4
  *E302251,1 AMH [End]
  
ENDCASE  
SET EXACT &lcSetExact


IF lnElem > 0
  STORE laPrjTypes[lnElem , 2] TO m.cPrj_Typ, lcPrj_Typ 
  lcPrjFile  = laPrjTypes[lnElem ,3] 
  lcLinsFile = laPrjTypes[lnElem ,4] 
  lcPrjFldSt = laPrjTypes[lnElem ,5] 
  lcPrjFld   = laPrjTypes[lnElem ,6] 
  
  *E302251,1 AMH Add new project types [Start]
  *lcPrjID    = LEFT(laPrjTyp[puPrjTyp,1],20) + ' ID'
  lcPrjID    = IIF(lcPrj_Typ$"HS","Project",LEFT(laPrjTyp[puPrjTyp,1],20)) + ' ID'
  *E302251,1 AMH [End]
  
  *E301869,2 AMH Change the cutting tickets type to transaction type [Start]
  *lcProj     = RIGHT(lcPrjType,14) + ' Type'
  lcProj     = IIF(LEFT(lcPrjType,1)#'C',RIGHT(lcPrjType,14) + ' Type','Transaction Type')
  *E301869,2 AMH [End]
  
  *-- ReInitialize project browse fields variables
  lcPrjBrFld  = "cPrj_ID  : H='Project ID',"+;
                "cStyle   : H='Style',"+;
                "cPrj_SDsc: H='Project Description'"     
  m.cPrj_ID = SPACE(6)
  m.cStyle  = SPACE(12)
ENDIF

*!*************************************************************
*! Name      : lfvPrj_ID
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for get field m.cPrj_ID
*!*************************************************************
*! Calls              :  - lfBrowIDs()
*!                       - lpshow()
*!                       - lfVldPrjct()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPrj_ID()
*!*************************************************************
FUNCTION lfvPrj_ID
PRIVATE llBrowPrjs, lcBrowMsg

m.cPrj_ID = PADR(ALLTRIM(m.cPrj_ID), 6)

*-- If browsing from the browsing button, or if calling the browse by a '?',
*-- Browse IDs from (lcPrjFile),

*E302251,1 AMH Add new project types [Start]
*IF llBrowse .OR. '?' $ m.cPrj_ID
IF llBrowse .OR. '?' $ m.cPrj_ID .OR. IIF(lcPrj_Typ="S",'?' $ m.cStyle,.F.)
*E302251,1 AMH [End]

  *-- Browse for available project id 's
  =lfBrowIDs()
*-- ELse, if an entry is written, validate the entry as folows:
ELSE
  
  *E302251,1 AMH Add new project types [Start]
  *IF !EMPTY(m.cPrj_ID) 
  IF !EMPTY(m.cPrj_ID) .OR. IIF(lcPrj_Typ="S",!EMPTY(m.cStyle),.F.)
  *E302251,1 AMH [End]
  
    llBrowPrjs = .T.
    
    *E302251,1 AMH Add new project types [Start]
    *lcBrowMsg  = lcPrjType + ' ' + ALLTRIM(m.cPrj_ID) + lcTNotFound + ;
                 lcPrjType +'s file.'  
    lcBrowMsg  = lcPrjType + ' ' + ALLTRIM(m.cPrj_ID) + IIF(lcPrj_Typ="S",ALLTRIM(m.cStyle),"") + lcTNotFound + ;
                 lcPrjType +'s file.'  
    *E302251,1 AMH [End]
    
    DO CASE
      *-- If the entry if found in the project file, move to the style
      *-- field and wait for another entry if the project type is 
      *-- Sales or Puechase orders, otherwise 'V'iew  the record
      
      *E302251,1 AMH Add new project types [Start]
      *CASE SEEK(lcPrj_Typ + m.cPrj_ID , 'PMPRJHD')
      CASE SEEK(lcPrj_Typ + m.cPrj_ID + IIF(lcPrj_Typ="S",m.cStyle,""), 'PMPRJHD')
      *E302251,1 AMH [End]
      
        *-- If the project type is cutting ticket, view
        
        *E302251,1 AMH Add new project types [Start]
        *IF lcPrj_Typ = 'C'
        IF lcPrj_Typ $ 'CSH'
        *E302251,1 AMH [End]
        
          STORE .F. TO laScrMode
          laScrMode[2] = .T.
          SHOW GETS
        ELSE  
          *-- Check style field
          SHOW GET ibStyle   ENABLE
          SHOW GET m.cStyle  ENABLE
          _CUROBJ = OBJNUM(m.cStyle)

        ENDIF  
        llBrowPrjs = .F.      

      *-- If the entry is not found in the projects file, but is found
      *-- in the project type file, proceed with project validation 
      *-- function lfVldPrjct
      CASE SEEK(lcPrjPre+m.cPrj_ID, lcPrjFile) 
        IF &lcPrjFile..Status $ 'OH'
          =lfVldPrjct(lcPrj_Typ +  m.cPrj_ID, lcPrjType + ' Projects')   
          llBrowPrjs = .F.
        ELSE
          lcBrowMsg  = lcPrjType + ' ' + ALLTRIM(m.cPrj_ID) + ;
                       laStatus[AT(&lcPrjFile..Status, 'CXSA')+1] 
        ENDIF  
      
      *E302251,1 AMH Add new project types [Start]
      CASE lcPrj_Typ = "H"
        =lfVldPrjct(lcPrj_Typ +  m.cPrj_ID, lcPrjType + ' Projects')
        llBrowPrjs = .F.
      *E302251,1 AMH [End]
      
    ENDCASE    

    *-- If the entry is not found, anywhere, prompt the user
    *-- to browse for available IDs from the project type file,
    *-- or Reenter
    IF llBrowPrjs
      *-- Browse project types..
       IF gfModalgen("TRM00000B00014","DIALOG",.F.,.F.,lcBrowMsg) = 1
         =lfBrowIDs()
       ELSE
         m.cPrj_ID = SPACE(6)  
         _CUROBJ   = _CUROBJ
       ENDIF 
    ENDIF
  ENDIF
ENDIF    

*E302251,1 AMH Add new project types [Start]
*IF !EMPTY(m.cPrj_ID) .AND. m.cPrj_ID <> lcOldVal
IF !EMPTY(m.cPrj_ID) .AND. m.cPrj_ID <> lcOldVal .AND. lcPrj_Typ # "H"
*E302251,1 AMH [End]

  SELECT DISTINCT SUBSTR(Style,1,LEN(m.cStyle)) AS STYLE;
    FROM (gcDataDir + lcLinsFile);
   WHERE &lcLinsFile..&lcPrjFld = m.cPrj_ID ;
    INTO CURSOR (lcBrowCurs)  
ENDIF    
llBrowse = .F.

lcmPrj_ID  = m.cPrj_ID

IF !EMPTY(m.cPrj_ID)
  lcPrjTypSt = 'DISABLE'
  SHOW GET puPrjTyp   DISABLE
  IF gcAct_Appl = 'PO' OR gcAct_Appl = 'SO'
    SHOW GET ibStyle     ENABLE
    SHOW GET m.cStyle    ENABLE  
  ELSE
    
    *E302251,1 AMH Add new project types [Start]
    *IF EMPTY(lcPrjPre)
    IF EMPTY(lcPrjPre) .OR. lcPrj_Typ = "H"
    *E302251,1 AMH [End]
    
      SHOW GET ibStyle     DISABLE
      SHOW GET m.cStyle    DISABLE  
    ELSE
      SHOW GET ibStyle     ENABLE
      SHOW GET m.cStyle    ENABLE       
    ENDIF  
  ENDIF
ELSE
  lcPrjTypSt = 'ENABLE'  
  SHOW GET puPrjTyp   ENABLE
  SHOW GET ibStyle     DISABLE
  SHOW GET m.cStyle    DISABLE  
ENDIF


*!*************************************************************
*! Name      : lfVldPrjct
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates a passed key expression of a project id
*!*************************************************************
*! Calls              :  - lfVldKey()
*!                       - lpshow()
*!*************************************************************
*! Passed Parameters  :  - lcKeyExp  : key expression = project type
*!                                                    + project ID.
*!                                      or space for all projects.                  
*!                       - lcPrjTitle : Browse file title.
*!*************************************************************
*! Returns            :  .T. if an ID is selected or added,
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  IF lfVldPrjct(m.cPrj_Typ +  m.cPrj_ID,;
*!                                     lcPrjType + ' Projects')   
*!*************************************************************
FUNCTION lfVldPrjct
PARAMETER lcKeyExp, lcPrjTitle
*-- Validates a passed project ID from PMPRJHD file.

*E302251,1 AMH Add new project types [Start]
*IF lfVldKey('PMPRJHD', 'PMPRJHD', lcPrjBrFld,;
            lcPrjTitle, 'cPrj_ID', ;
            lcPrjType + ' ' + ALLTRIM(m.cPrj_ID) + lcTNotFound + [Projects file.],;
            lcKeyExp, '')
IF lfVldKey('PMPRJHD', 'PMPRJHD', lcPrjBrFld,;
            lcPrjTitle, 'cPrj_ID', ;
            lcPrjType + ' ' + ALLTRIM(IIF(lcPrj_Typ="S",m.cStyle,m.cPrj_ID)) + lcTNotFound + [Projects file.],;
            lcKeyExp, '')
  *IF lcPrj_Typ <> 'C' 
  IF !(lcPrj_Typ $ 'CH')
*E302251,1 AMH [End]

    SHOW GET m.cStyle ENABLE
    SHOW GET ibStyle  ENABLE
    _CUROBJ = OBJNUM(m.cStyle)
  ELSE
    SHOW GETS
  ENDIF
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF

*!*************************************************************
*! Name      : lfBrowIDs
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates, and/or browses a file according to 
*!             a selected project type.
*!             Then proceeds to validate a selected ID as a 
*!             project ID.
*!*************************************************************
*! Called From        :  lfvPrj_ID()              
*!*************************************************************
*! Calls              :  - lfVldKey()
*!                       - lfVldPrjct()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :   None
*!*************************************************************
*! Example            :  =lfBrowIDs()
*!*************************************************************
FUNCTION lfBrowIDs
*-- Browses (lcPrjFile) (CUTTKTH, ORDHDR, or POSHDR) for an ID, 
*-- If one is selected, validates it as a project ID by calling lfVldPrjct()
llBrowse    = .T.
*--If a project is selected, continue.

DO CASE
  CASE gcAct_Appl = 'MF' 
    IF EMPTY(lcPrjPre)
      lcExpKey   = [FOR Status $ 'OH']
    ELSE
      lcExpKey = [FOR Status $ 'OH' AND cStyType = lcPrjPre]
    ENDIF  
  CASE gcAct_Appl = 'PO' 
    lcExpKey = [FOR Status $ 'OH' AND cStyType = lcPrjPre]
  CASE gcAct_Appl = 'SO'
    lcExpKey = [FOR Status $ 'OH' AND cOrdType = lcPrjPre]
  
  *E302251,1 AMH Add new project types [Start]
  CASE gcAct_Appl = 'IC'
    IF lcPrj_Typ = "S"
      lcExpKey = [FOR Status $ 'AH']
    ELSE
      lcExpKey = ""
    ENDIF
  *E302251,1 AMH [End]
  
ENDCASE

*E302251,1 AMH Add new project types [Start]
*IF lfVldKey(lcPrjFile, lcPrjFile, lcPrjFldSt, lcPrjType + 's',; 
          lcPrjFld, lcPrjType + ' not found.', m.cPrj_ID, lcExpKey)
IF lfVldKey(lcPrjFile, IIF(lcPrj_Typ="S","CSTYLE",lcPrjFile), lcPrjFldSt, lcPrjType + 's',; 
          lcPrjFld, lcPrjType + ' not found.', m.cPrj_ID, lcExpKey)
*E302251,1 AMH [End]

  llBrowse    = .F.
  STORE .F. TO laScrMode
  laScrMode[1] = .T.
  
  *E302251,1 AMH Add new project types [Start]
  *=lfVldPrjct(lcPrj_Typ +  m.cPrj_ID, lcPrjType + ' Projects')
  =lfVldPrjct(lcPrj_Typ + m.cPrj_ID + IIF(lcPrj_Typ="S",PADR(m.cStyle,12),""), lcPrjType + ' Projects')
  *E302251,1 AMH [End]
  
ENDIF

*!*************************************************************
*! Name      : lfVldKey
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : lcSrchFile   : File to be searched or browsed
*!                      lcSrchTag    : Tag to be used for seek or browse
*!                      lcBrFields   : Browse fields string
*!                      lcBrFlTtl    : Browse title
*!                      lcFldName    : return field name
*!                      lcNtFndMsg  : message string to be displayed if the key
*!                                     is not found.
*!                      lcCurCont : Search expression (optional)
*!                      lcBrowCond   : browse condition (optional)
*!                      lnOption     : Required option (optional)
*!*************************************************************
*! Returns            :  .T. if a key is selected or is to be added,
*!                       .F. otherwise 
*!*************************************************************
*! Example            :  lfVldKey('PMPTHHD', 'PATH_ID',;
*!                                lcPathFlds, lcPathTtl,;
*!                                'cPath_ID', m.cPath_ID, '')
*!*************************************************************
FUNCTION lfVldKey
PARAMETERS lcSrchFile , lcSrchTag, lcBrFields, lcBrFlTtl, lcFldName,;
           lcNtFndMsg, lcCurCont, lcBrowCond, lnOption
PRIVATE lcCurObj, lnCurObjLen, lnCurTag, lnCurAlias

lnCurAlias   = SELECT(0)
SELECT (lcSrchFile)
lnCurTag     = VAL(SYS(21))
SET ORDER TO TAG (lcSrchTag)

lcCurObj     = 'm.' + SYS(18)
lcCurCont = IIF(EMPTY(lcCurCont), EVALUATE(lcCurObj), lcCurCont)
lcBrowCond   = IIF(EMPTY(lcBrowCond), '', lcBrowCond)
lnCurObjLen  = FSIZE(lcFldName, lcSrchFile)

IF llBrowse .OR. '?' $ lcCurCont
  GO TOP
  DIMENSION laSelected[1]
  llNotFound = .F.
  IF lcSrchFile = "PMPRJHD"
    DO CASE   
      CASE gcAct_Appl = 'MF'
        IF EMPTY(lcPrjPre)
          LOCATE 
          IF EOF()
            llNotFound = .T.
          ENDIF 
        ELSE  
          IF !SEEK(lcPrjPre)
            llNotFound = .T.      
          ENDIF
        ENDIF
      CASE gcAct_Appl = 'PO' OR gcAct_Appl = 'SO'
        IF !SEEK(lcPrjPre)
          llNotFound = .T.      
        ENDIF
    ENDCASE  
  ENDIF
  IF llNotFound 
    = gfModalGen("INM42097B42001","Dialog") 
    &lcCurObj = "" 
    llBrowse   = .F.
    SET ORDER TO (lnCurTag)
    SELECT (lnCurAlias) 
    RETURN (laScrMode[2] OR laScrMode[4]) AND !EMPTY(EVAL(lcCurObj))
  ENDIF
  *B038431,1  TMI [Start] use laPrjTyp[puPrjTyp,1] instead of lcBrFlTtl variable
  *IF ARIABROW(lcBrowCond, lcBrFlTtl, ;
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laSelected')
  IF ARIABROW(lcBrowCond, laPrjTyp[puPrjTyp,1], ;
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laSelected')
    *B038431,1  TMI [End  ]             
    &lcCurObj = EVALUATE(lcSrchFile + '.' + lcFldName)
    IF !EMPTY(EVAL(lcCurObj))
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
    ENDIF  
  ELSE   
    &lcCurObj = SPACE(lnCurObjLen)
    _CUROBJ   = _CUROBJ
  ENDIF  
ELSE
  IF !EMPTY(lcCurCont) 
    IF !SEEK(lcCurCont)  
      lnSoftSeek = RECNO(0)
      IF EMPTY(lnOption)
        lnOption = gfModalgen("TRM00000B02004","DIALOG",.F.,.F.,lcNtFndMsg)  
      ENDIF
      DO CASE 
        CASE lnOption = 1
          IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
            GO lnSoftSeek
          ELSE
            GO TOP
          ENDIF
          DIMENSION laSelected[1]
          llNotFound = .F.
          IF lcSrchFile = "PMPRJHD"          
            DO CASE   
              CASE gcAct_Appl = 'MF'
                IF EMPTY(lcPrjPre)
                  LOCATE 
                  IF EOF()
                    llNotFound = .T.
                  ENDIF 
                ELSE  
                  IF !SEEK(lcPrjPre)
                    llNotFound = .T.      
                  ENDIF
                ENDIF
              CASE gcAct_Appl = 'PO' OR gcAct_Appl = 'SO'
                IF !SEEK(lcPrjPre)
                  llNotFound = .T.      
                ENDIF
            ENDCASE  
          ENDIF
          IF llNotFound 
            = gfModalGen("INM42097B42001","Dialog") 
            &lcCurObj = "" 
            llBrowse   = .F.
            SET ORDER TO (lnCurTag)
            SELECT (lnCurAlias) 
            RETURN (laScrMode[2] OR laScrMode[4]) AND !EMPTY(EVAL(lcCurObj))
          ENDIF
          IF ARIABROW(lcBrowCond, lcBrFlTtl, ;
                          gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laSelected')
            &lcCurObj = EVALUATE(lcSrchFile + '.' + lcFldName)
            IF !EMPTY(EVAL(lcCurObj))
              STORE .F. TO laScrMode
              laScrMode[2] = .T.
            ENDIF  
          ELSE   
            &lcCurObj = SPACE(lnCurObjLen)
            _CUROBJ   = _CUROBJ
          ENDIF  
        CASE lnOption = 2
          STORE .F. TO laScrMode
          laScrMode[4] = .T.
        OTHERWISE
          &lcCurObj = SPACE(lnCurObjLen)
          _CUROBJ   = _CUROBJ
      ENDCASE    
    ELSE
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
    ENDIF  
  ENDIF
ENDIF     

llBrowse   = .F.
SET ORDER TO (lnCurTag)
SELECT (lnCurAlias) 
RETURN (laScrMode[2] OR laScrMode[4]) AND !EMPTY(EVAL(lcCurObj))

*!*************************************************************
*! Name      : lfvStyle
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for get field Style
*!*************************************************************
*! Calls              :   ARIABROW()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :   None
*!*************************************************************
*! Example            :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lnCurAlias
*-- Search for an entered style, if not found browse

*-- Add all colors facility
IF MDOWN()
  RETURN
ENDIF  

*E302251,1 AMH Add new project types [Start]
IF lcPrj_Typ="S"
  =lfvPrj_ID()
  IF laScrMode[2] .OR. laScrMode[4]
    SHOW GETS
  ENDIF
  RETURN
ENDIF
*E302251,1 AMH [End]

m.cStyle   = IIF(!llBrowse .AND. EMPTY(m.cStyle), REPLICATE('*', 12),;
                 PADR(ALLTRIM(m.cStyle), 12))

lnCurAlias = SELECT(0)

llBrowse   = llBrowse .OR. '?' $ m.cStyle
IF !llBrowse
  IF laScrMode[1] OR laScrMode[2] 
    SELECT PMPRJHD
      
    IF SEEK(lcPrj_Typ + m.cPrj_ID + SUBSTR(m.cStyle,1,LEN(PMPRJHD.cStyle)))
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
      SHOW GETS
      *-- If not found in the projects file, validate the entry
      *-- from the styles of the order.
      *-- If found, present a message with Add, Browse, or 
      *-- Reeneter options, Otherwise, Present a not found message
      *-- <Browse>, <Reenter> option.
    ELSE
      *-- Reposition the file pointer.
      =SEEK(lcPrj_Typ + m.cPrj_ID)
      IF lfVldStyle()
        lcMsg = 'Project ' + ALLTRIM(m.cPrj_ID) + '\' + IIF(m.cStyle = REPLICATE('*', 12),;
                'all styles',ALLTRIM(m.cStyle)) + ' is not found in the projects file.'
        lnOption = gfModalgen("TRM00000B02004","DIALOG",.F.,.F.,lcMsg)                              
        DO CASE
          *-- If projects file is to be browsed for all styles
          *-- having the same project ID
          CASE lnOption = 1
            IF lfVldKey('PMPRJHD', 'PMPRJHD', lcPrjBrFld,;
                      'Styles of ' + lcPrjType + ' project ' + ;
                       ALLTRIM(m.cPrj_ID) , 'cStyle', '', ;
                       lcPrj_Typ + m.cPrj_ID + m.cStyle,;
                       ['] +lcPrj_Typ + m.cPrj_ID+['], 1)
              STORE .F. TO laScrMode
              laScrMode[2] = .T.
              SHOW GETS
            ELSE
              m.cStyle = SPACE(12)
              _CUROBJ = _CUROBJ
            ENDIF           
          *-- If the entry is to be Added,
          CASE lnOption = 2
            STORE .F. TO laScrMode
            laScrMode[4] = .T.
            SHOW GETS
          OTHERWISE
            m.cStyle = SPACE(12)
            _CUROBJ = _CUROBJ
        ENDCASE    
      ELSE      && ELSE IF lfVldStyle()
        *-- If the style does not belong to the order, browse, or
        *-- reenter message.
        lcMsg = 'Style ' + ALLTRIM(m.cStyle) + ' is not found in ' +;
                 lcPrjType + ' #' + ALLTRIM(m.cPrj_ID)+ '.' 
        IF gfModalgen("TRM00000B00014","DIALOG",.F.,.F.,lcMsg) = 1                 
            llBrowse = .T.
        ELSE
          m.cStyle = SPACE(12)
          _CUROBJ  = _CUROBJ
        ENDIF    && ENDIF gfModalgen(..)
      ENDIF    && ENDIF lfVldStyle()        
    ENDIF    && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)
  ELSE     
    *-- If the style is valid for the selected project, continue,
    *-- otherwise, browse.
    IF !lfVldStyle()
      llBrowse = .T.
    ELSE
    STORE .F. TO laScrMode
    laScrMode[4] = .T.
    SHOW GETS
    ENDIF   
  ENDIF    
ENDIF    && ENDIF !llBrowse  

IF llBrowse 
  SELECT (lcBrowCurs)
  lcBrFields = [Style, Desc=LOOKUP(STYLE.Desc, Style, STYLE.STYLE, 'STYLE'):H='Description']
  DIMENSION laSelected[1]
  lcFldName = 'STYLE'
  IF ARIABROW('', 'Styles',;
               gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laSelected')
    m.cStyle  = Style
    *-- If coming from 'S'elect mode, i.e. the project ID is already
    *-- found in the projects file. Look for the entered style with
    *-- the projects file. 
    IF laScrMode[1] OR laScrMode[2]
      SELECT PMPRJHD
      
      *-- If found, 'V'iew the entry, 
      IF SEEK(lcPrj_Typ + m.cPrj_ID + SUBSTR(m.cStyle,1,LEN(PMPRJHD.cStyle)))
        STORE .F. TO laScrMode
        laScrMode[2] = .T.
        SHOW GETS
         
        *-- If not found in the projects file, validate the entry
        *-- from the styles of the order.
        *-- If found, present a message with Add, Browse, or 
        *-- Reeneter options, Otherwise, Present a not found message
        *-- <Browse>, <Reenter> option.
      ELSE
        *-- Reposition the file pointer.
        =SEEK(lcPrj_Typ + m.cPrj_ID)
        lcMsg = 'Project ' + ALLTRIM(m.cPrj_ID) +'\' + ALLTRIM(m.cStyle) +' is not found ' + ;
                'in the projects file. '
        lnOption = gfModalgen("TRM00000B02004","DIALOG",.F.,.F.,lcMsg)                  
        DO CASE
          *-- If projects file is to be browsed for all styles
          *-- having the same project ID
          CASE lnOption = 1
            STORE .F. TO laScrMode
            laScrMode[1] = .T.
            IF lfVldKey('PMPRJHD', 'PMPRJHD', lcPrjBrFld,;
                        'Styles of ' + lcPrjType + ' project ' + ;
                        ALLTRIM(m.cPrj_ID) , 'cStyle', '', ;
                        lcPrj_Typ + m.cPrj_ID + m.cStyle,;
                        ['] +lcPrj_Typ + m.cPrj_ID+['], 1)
              STORE .F. TO laScrMode
              laScrMode[2] = .T.
              SHOW GETS
            ELSE
              m.cStyle = SPACE(12)
              _CUROBJ = _CUROBJ
            ENDIF          
          *-- If the entry is to be Added,
          CASE lnOption = 2
            STORE .F. TO laScrMode
            laScrMode[4] = .T.
            SHOW GETS             
          OTHERWISE
            m.cStyle = SPACE(12)
            _CUROBJ = _CUROBJ
        ENDCASE    
      ENDIF     && ENDIF SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle)  
    ELSE      
      STORE .F. To laScrMode
      laScrMode[4] = .T.
      SHOW GETS
    ENDIF    
  ELSE      && ELSE IF ARIABROW(..)
    m.cStyle = SPACE(12)
    _CUROBJ  = _CUROBJ
  ENDIF     && ENDIF ARIABROW(..)
  llBrowse = .F.
ENDIF
lcmcStyle  = m.cStyle

IF EMPTY(m.cStyle) AND (gcAct_Appl = 'PO' OR gcAct_Appl = 'SO' OR lcPrjPre = 'A' OR lcPrjPre = 'D')
  SHOW GET ibStyle     ENABLE
  SHOW GET m.cStyle    ENABLE  
ELSE
  SHOW GET ibStyle     DISABLE
  SHOW GET m.cStyle    DISABLE  
ENDIF

SELECT (lnCurAlias)
lcBrfields  = lcOldBrFild

*!*************************************************************
*! Name      : lfVldStyle
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates a style as belonging to an order or not.
*!*************************************************************
*! Calls              :   ARIABROW()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :  .T. if the style belongs to the project ID
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfVldStyle()
*!*************************************************************
FUNCTION lfVldStyle
PRIVATE lnCurAlias, llVldStyle
llVldStyle = .F.
IF m.cStyle = REPLICATE('*', 12)
  llVldStyle = .T.
ELSE  
  lnCurAlias = SELECT(0)
  DO CASE
    CASE lcPrj_Typ = 'O' OR lcPrj_Typ = 'T'
      SELECT ORDLINE
      IF SEEK(m.cPrj_ID)
        LOCATE REST WHILE Order = m.cPrj_ID FOR SUBSTR(Style,1,LEN(m.cStyle)) = m.cStyle
        llVldStyle = FOUND()
      ENDIF  
    CASE lcPrj_Typ = 'P' OR lcPrj_Typ = 'A' OR lcPrj_Typ = 'D' OR;
         lcPrj_Typ = 'N' OR lcPrj_Typ = 'R'
      llVldStyle = SEEK(m.cPrj_ID + m.cStyle, 'POSLN') 
  ENDCASE
  SELECT (lnCurAlias)
ENDIF  
RETURN llVldStyle      

*!*************************************************************
*! Name      : lfvPath_ID
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for get field m.cPath_ID
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPath_ID()
*!*************************************************************
FUNCTION lfvPath_ID
PRIVATE lcOldSMode

m.cPath_ID = PADR(ALLTRIM(m.cPath_ID),4)

IF llBrowse .OR. (!EMPTY(m.cPath_ID) AND !SEEK(m.cPath_ID,'PMPTHHD')) .OR. m.cPath_ID <> lcOldVal 
  FOR lnI = 1 To 4
   IF laScrMode[lnI]
     lnOdSMode = lnI
     EXIT
    ENDIF
  ENDFOR
    
  GO TOP IN PMPTHHD
  IF lfVldKey('PMPTHHD', 'PMPTHHD', lcPathFlds, 'Project Paths',;
              'cPath_ID', 'Path ' + ALLTRIM(m.cPath_ID) + ' ' + ;
               lcTNotFound + 'Templates File.', '', '', 1)
    lcpath_dsc = PMPTHHD.cPath_Dsc
    llPathSel = .T.
    llNewPath = RECCOUNT(lc_PMPrjDt) = 0 .OR. m.cPath_ID <> lcOldVal
    SHOW GET pbOprs ENABLE
  ELSE
    llNewPath  = .F.
    lcpath_dsc = SPACE(40)
  ENDIF
  SHOW GET lcpath_dsc
  =lfRefresh()  
  STORE .F. To laScrMode
  laScrMode[lnOdSMode] = .T.
  IF llBrowse
    llBrowse = .F.
  ENDIF
ELSE
  llBrowse   = .F.
  SHOW GET lcpath_dsc
  =lfRefresh()  
ENDIF  
   
lcmcpathid = m.cpath_id
            
*!*************************************************************
*! Name      : lfvPrjSDsc
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for get field m.cPrj_SDsc
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPrjSDsc()
*!*************************************************************
FUNCTION lfvPrjSDsc
IF m.cPrj_SDsc <> lcOldVal
  m.cPrj_SDsc = PADR(ALLTRIM(m.cPrj_SDsc), 40)
  glUpdated  = .T.
ENDIF

lcmcPrjSDs = m.cPrj_SDsc

*!*************************************************************
*! Name      : lfvDates
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for date get fields
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDates()
*!*************************************************************
FUNCTION lfvDates
PRIVATE ldCurObj

ldCurObj = 'm.' + SYS(18)
IF EVALUATE(ldCurObj) <> lcOldVal
  IF !EMPTY(m.dReq_Strt)        .AND. ;
     !EMPTY(m.dReq_Fnsh)        .AND. ;
     m.dReq_Strt > m.dReq_Fnsh  .AND. ;
     gfModalgen("TRM38219B00000","DIALOG") = 1
     &ldCurObj = lcOldVal
  ELSE
    glUpdated = .T.
  ENDIF  
ENDIF
lcdreqstrt = m.dreq_strt
lcdreqfnsh = m.dreq_fnsh

*!*************************************************************
*! Name      : lfvPActDat
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for date get fields
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPActDat()
*!*************************************************************
FUNCTION lfvPActDat
PRIVATE ldCurObj

ldCurObj = 'm.' + SYS(18)
IF EVALUATE(ldCurObj) <> lcOldVal
  IF !EMPTY(m.dact_strt)        .AND. ;
     !EMPTY(m.dact_fnsh)        .AND. ;
     m.dact_strt > m.dact_fnsh  .AND. ;
     gfModalgen("TRM38220B00000","DIALOG") = 1 
     &ldCurObj = lcOldVal
  ELSE
    glUpdated = .T.
  ENDIF  
ENDIF

lcdactstrt = m.dact_strt
lcdactfnsh = m.dact_fnsh


*!*************************************************************
*! Name      : lpDelScr
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Delete > (pbDelete).
*!             Saves current modifications to the file. 
*!*************************************************************
*! Calls     : lpshow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr
PRIVATE laDelRec, lnCurAlias

lnCurAlias = SELECT(0)

SELECT PMPRJHD
IF RLOCK()
  REPLACE llok_stat WITH .T.
  UNLOCK
  SELECT PMPRJDT

  *-- Scan through all the records of the same project ID in the details file.
  SCAN FOR cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id  = SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle))
    lcOprt_Ctg = cOprt_Ctg                    
    lcOprt_ID  = cOprt_ID
    
    *SCATTER MEMVAR MEMO 
    *IF !EMPTY(m.cOprt_res) OR !EMPTY(m.cGroup_Id)
    *  m.lVoid = .T.
    *  ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
    *  lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,m.nAct_dur)
    *  =lfUpdAdTrl()
    *  SELECT (lc_PrjAudt)
    *  IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
    *    =gfDoTriger('MFPROJ',PADR(lcEvent,10))
    *  ENDIF
    *ENDIF
    
    SELECT PMPRJRL
    SCAN FOR cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id  = ;
             SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
             SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
             SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
      SCATTER TO laDelRec MEMO BLANK
      GATHER FROM laDelRec MEMO
    ENDSCAN
    
    *E302251,1 AMH Delete Notify Setting Records [Start]
    SELECT PMPRJNTF
    IF SEEK(PMPRJDT.CPRJ_TYP+PMPRJDT.CPRJ_ID+PMPRJDT.CSTYLE+lcOprt_Ctg+lcOprt_ID)
      DELETE REST WHILE cPrj_Typ+cPrj_ID+cStyle+COprt_Ctg+COprt_ID+CUser_ID =;
                        PMPRJDT.CPRJ_TYP+PMPRJDT.CPRJ_ID+PMPRJDT.CSTYLE+lcOprt_Ctg+lcOprt_ID
    ENDIF
    *E302251,1 AMH [End]
    
    DELETE FOR cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id  = ;
             SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
             SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
             SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
             
    SELECT PMPRJDT  
    *-- for every line on the file, delete related predecessors.
    GATHER MEMVAR MEMO
  ENDSCAN
  DELETE FOR cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id  = SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle))
                     
  SELECT SYSCHDUL
  SET ORDER TO Coprusr
  *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
  *DELETE FOR cconttype+cseqnumber+ccont_id+coperstat+cuser_id = lcPrj_Typ + lcPrj_ID
  DELETE FOR cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id = lcPrj_Typ + lcPrj_ID + lcStyle
  *B130984,1 HBG 12/04/2006 [End]
  
  SELECT PMPRJHD
  SCATTER MEMVAR MEMO BLANK
  GATHER MEMVAR MEMO
  DELETE
  
  *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
  *STORE "" TO lcProg , lcKey , lcEvent
  STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
  *HBG [End]
  =lfUpdVar()
  IF !USED('AUDTRAIL')
    =gfOpenFile(gcDataDir + 'AUDTRAIL' , 'AUDTRAIL' , 'SH')
  ENDIF 
  SELECT AUDTRAIL
  DELETE FOR capobjnam+key+caudtralid = PADR(lcProg,10)+PADR(lcKey,20) 

  *-- Go back to 'S'elect mode
  STORE .F. TO laScrMode
  laScrMode[1] = .T.
  SHOW GETS
ENDIF  
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lpSavScr
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Accept > (pbAccept).
*!             Saves current modifications to the file. 
*!*************************************************************
*! Calls     : lpshow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
PRIVATE lcSetDel  

GO TOP IN (lc_PMPrjDt)
IF EOF(lc_PMPrjDt)
  =gfModalgen("INM38217B00000","DIALOG")          
  llCSave = .F.
  RETURN 
ENDIF

*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
=lfUpdVar()
*--Update Audit trail
*DO CASE
*  CASE gcAct_Appl = 'MF'
*    lcEvent = 'RESCHDCT'
*    DO CASE
*      CASE lcPrj_typ = 'A'      && Adorment Order
*        lcEvent = 'RESCHDPA'      
*      CASE lcPrj_typ = 'D'      && Dye Order 
*        lcEvent = 'RESCHDPD'      
*    ENDCASE
*  CASE gcAct_Appl = 'PO'
*    DO CASE
*      CASE lcPrj_typ = 'P'      && PO
*        lcEvent = 'RESCHDPO'
*      CASE lcPrj_typ = 'N'      && Inter-Location PO
*        lcEvent = 'RESCHDPN'      
*      CASE lcPrj_typ = 'R'      && Return PO
*        lcEvent = 'RESCHDPR'      
*    ENDCASE
*  CASE gcAct_Appl = 'SO'
*    DO CASE
*      CASE lcPrj_typ = 'O'      && SO
*        lcEvent = 'RESCHDSO'
*      CASE lcPrj_typ = 'T'      && EDI Order
*        lcEvent = 'RESCHDST'      
*    ENDCASE
*ENDCASE
*HBG [End]

*-- Add saving restriction,
*-- IF the project is marked as a history and it ahs not been 
*-- previously marked as one,
DO CASE
  *-- If the project is marked as history, confirm from the user.
  CASE m.cPrj_Stts = 'H' 
    IF gfModalgen("TRM38221B38006","DIALOG") = 1                 
      *-- Clear all related operaions
      SELECT (lc_PMPrjDt)
      REPLACE ALL cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
      SELECT (lc_PMPrjRl)
      REPLACE ALL cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
    ELSE
      _CUROBJ = IIF(_DOS .OR. _UNIX,;
                     OBJNUM(ibCurStage), OBJNUM(puCurStage))
      RETURN
    ENDIF             
  *-- If the project is cancelled, confirm from the user.
  CASE m.cPrj_Stts = 'X' .AND. PMPRJHD.cPrj_Stts <> 'X'.AND.;
    gfModalgen("TRM38222B38006","DIALOG") = 2                 
    _CUROBJ = IIF(_DOS .OR. _UNIX,;
                   OBJNUM(ibCurStage), OBJNUM(puCurStage))
    RETURN
ENDCASE  
     
m.cPrj_Typ   = lcPrj_Typ
m.cAdd_User  = gcUser_ID
m.dAdd_Date  = ldCurDate
m.cAdd_Time  = TIME()
  
*--Save header information
SELECT PMPRJHD 
IF SEEK(m.cPrj_Typ+m.cPrj_ID+m.cStyle)
  m.llok_stat = .T.
  GATHER MEMVAR MEMO
ELSE
  INSERT INTO PMPRJHD (llok_stat) VALUES (.T.)
  GATHER MEMVAR MEMO
ENDIF    

SELECT PMPRJDT
SET ORDER TO PMPRJDT

SELECT PMPRJRL
SET ORDER TO PMPRJRL

*-- If no lines are entered and a path ID is already selected,
*-- Save the path lines and predecessors (if any) into master files.
IF llNewPath
  SELECT PMPTHDT
  IF SEEK(m.cPath_ID)
    SCAN REST WHILE cpath_id+coprt_ctg+coprt_id = m.cPAth_ID
      SCATTER MEMVAR MEMO
      *-- Default remaining duration with the estimated duration for
      *-- every operation
      *-- Add cOprt_Ctg, cCtg_Seq, cOprt_Seq
      SELECT PMPRJDT
      SET ORDER TO PMPRJDT IN PMPRJDT
      IF !SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle))+;
               cOprt_Ctg+cOprt_ID)
        INSERT INTO PMPRJDT;
               (  cPrj_Typ ,   cPrj_ID  ,  cOprt_ID  ,   cOprt_Dsc,;
                  nest_dur ,   nrem_dur ,  cOprt_Ctg ,   cOprt_Res,;   
                  lShw2Cust,   mOprt_Com,  cUpdtMthd ,   mNotify  ,;  
                  cAdd_User,   dAdd_Date,  cAdd_Time ,   cStyle   ,;
                  cCtg_Seq ,   cOprt_Seq,  LORGINAL);
         VALUES( lcPrj_Typ ,   lcPrj_ID , m.cOprt_ID , m.cOprt_Dsc,;
                m.nest_dur , m.nest_dur , m.cOprt_Ctg, m.cOprt_Res,;
                m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd, m.mNotify  ,;    
                gcUser_ID  ,   ldCurDate, TIME()     , lcStyle    ,; 
                m.cCtg_Seq , m.cOprt_Seq,.T.)
      ELSE
        REPLACE cOprt_Dsc  WITH m.cOprt_Dsc,;
                nest_dur   WITH m.nest_dur ,;
                nrem_dur   WITH m.nest_dur ,;
                cOprt_Res  WITH m.cOprt_Res,;
                lShw2Cust  WITH m.lShw2Cust,;
                mOprt_Com  WITH m.mOprt_Com,;
                cUpdtMthd  WITH m.cUpdtMthd,;
                mNotify    WITH m.mNotify  ,;
                cAdd_User  WITH gcUser_ID  ,;
                dAdd_Date  WITH ldCurDate  ,;
                cAdd_Time  WITH TIME()     ,;
                cCtg_Seq   WITH cCtg_Seq   ,;
                cOprt_Seq  WITH m.cOprt_Seq  
      ENDIF          
      
     
      *-- Update SYSCHDUL 
      IF !EMPTY(m.cOprt_res) OR !EMPTY(m.cGroup_Id)
        SELECT SYSCHDUL 
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
        *IF !SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
        *         cOprt_Ctg+cOprt_ID+'O') 
        *  INSERT INTO SYSCHDUL;
        *        (cconttype , cseqnumber , ccont_id , csubject,;
        *         ctrantype,nestdur,ccompleted,cUser_Id,COPERSTAT,lPredComp);
        *  VALUES(lcPrj_Typ,lcPrj_ID,m.cOprt_Ctg+m.cOprt_ID,;
        *         m.coprt_dsc,'W',m.nest_dur,'N',;
        *         IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),'O',.T.)        
        IF !SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcstyle,1,LEN(cStyle)) +;
                 cOprt_Ctg+cOprt_ID+'O') 
          INSERT INTO SYSCHDUL;
                (cconttype , cseqnumber , cStyle , ccont_id , csubject,;
                 ctrantype,nestdur,ccompleted,cUser_Id,COPERSTAT,lPredComp);
          VALUES(lcPrj_Typ,lcPrj_ID,lcStyle,m.cOprt_Ctg+m.cOprt_ID,;
                 m.coprt_dsc,'W',m.nest_dur,'N',;
                 IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),'O',.T.)
        *B130984,1 HBG 12/04/2006 [End]        
          
        ELSE
          REPLACE cUser_Id   WITH IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
                  csubject   WITH m.coprt_dsc,;
                  ctrantype  WITH 'W',;
                  nestdur    WITH m.nest_dur ,;
                  ccompleted WITH 'N'  ,;
                  nactdur    WITH m.nest_dur,;
                  COPERSTAT  WITH 'O',;
                  lPredComp  WITH .T.
        ENDIF
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
        *IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        *B130984,1 HBG 12/04/2006 [End]
          IF m.nAct_Dur <> 0 OR !EMPTY(m.dAct_Fnsh)
            REPLACE SYSCHDUL.COPERSTAT  WITH 'C',;
                    SYSCHDUL.CCOMPLETED WITH 'Y'    
          ENDIF
          IF m.lVoid
            REPLACE SYSCHDUL.COPERSTAT WITH 'X'
          ENDIF        
        ENDIF  
      ENDIF
      
      *-- Append all predecessor lines to the temporary relations
      *-- file.
      SELECT PMPTHRL
      *-- Add cOprt_Ctg
      SCAN REST WHILE   cPath_ID + cOprt_Ctg   +  cOprt_ID = ;
                      m.cPath_ID + m.cOprt_Ctg + m.cOprt_ID
        SCATTER MEMVAR MEMO
        *-- Add cOprt_Ctg,cPrd_Ctg
        SELECT PMPRJRL
        IF !SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle))+;
                 m.cOprt_Ctg+m.cOprt_ID)
          INSERT INTO PMPRJRL;
                 (  cPrj_Typ ,   cPrj_ID  ,  cStyle   ,   cOprt_ID ,;
                    cPrd_ID  ,   nPrd_Lg  ,  cAdd_User,   dAdd_Date,;
                    cAdd_Time,  cOprt_Ctg , cPrd_Ctg);
           VALUES( lcPrj_Typ ,   lcPrj_ID , lcStyle   , m.cOprt_ID ,;
                  m.cPrd_ID  , m.nPrd_Lg  , gcUser_ID   ,  ldCurDate ,;
                    TIME()   , m.cOprt_Ctg, m.cPrd_Ctg)
        ELSE
          REPLACE cPrd_ID  WITH m.cPrd_ID ,;
                  nPrd_Lg  WITH m.nPrd_Lg ,;
                  cAdd_User WITH gcUser_ID,;
                  dAdd_Date WITH ldCurDate,;                  
                  cAdd_Time WITH TIME()   ,;                                    
                  cPrd_Ctg  WITH m.cPrd_Ctg                                                      
        ENDIF            
      ENDSCAN  
             
      SELECT PMPTHDT            
    ENDSCAN
    
    *-- Calculate operations estimated dates if operations are defined for
    *-- the first time new.
    =lfGetEstDt('PMPRJDT', 'PMPRJRL', .T.)
    
    IF !EMPTY(PMPRJDT.cOprt_res) OR !EMPTY(PMPRJDT.cGroup_Id)
      SELECT SYSCHDUL
      REPLACE dtrandate  WITH IIF(EMPTY(PMPRJDT.dclc_strt),PMPRJDT.dEst_strt,PMPRJDT.dclc_strt),;
              dcmpltdate WITH IIF(EMPTY(PMPRJDT.dclc_Fnsh),PMPRJDT.dEst_fnsh,PMPRJDT.dclc_Fnsh)
    ENDIF          
            
    SELECT PMPRJHD
    REPLACE dest_strt WITH m.dest_strt,;
            dest_fnsh WITH m.dest_fnsh
  ENDIF                   
ENDIF

*-- Save details
lcSetDel   = SET('DELETED')
lcSetExact = SET('EXACT')

SET DELETED OFF
SET EXACT OFF

SELECT (lc_PMPrjDt)

*-- Go through all the records of the temp cursor.
SCAN
  DO CASE
    *-- Case of a new line
    CASE cStatus = 'A'        
      SCATTER MEMVAR MEMO
      llAddToAdt = .T.
      SELECT PMPRJDT
      IF SEEK(' ')
        =RLOCK()
        RECALL         
        GATHER MEMVAR MEMO
        UNLOCK
      ELSE
        m.LORGINAL = .T.
        INSERT INTO PMPRJDT FROM MEMVAR
      ENDIF  
      
      *-- Update SYSCHDUL 
      SET DELETED &lcSetDel
      IF !EMPTY(m.cOprt_res) OR !EMPTY(m.cGroup_Id) 
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
        *IF !SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        *  INSERT INTO SYSCHDUL;
        *        (cconttype , cseqnumber , ccont_id , csubject,;
        *         ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
        *         lPredComp);
        *  VALUES(m.cPrj_Typ,m.cPrj_ID,m.cOprt_Ctg+m.cOprt_ID,;
        *         m.coprt_dsc,'W',m.nest_dur,'N',;
        *         IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
        *         IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
        *         IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
        IF !SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
          INSERT INTO SYSCHDUL;
                (cconttype , cseqnumber ,cStyle, ccont_id , csubject,;
                 ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
                 lPredComp);
          VALUES(m.cPrj_Typ,m.cPrj_ID,m.cStyle,m.cOprt_Ctg+m.cOprt_ID,;
                 m.coprt_dsc,'W',m.nest_dur,'N',;
                 IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
                 IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
                 IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
        *B130984,1 HBG 12/04/2006 [End]         
        ELSE
          REPLACE SYSCHDUL.cUser_Id   WITH IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
                  SYSCHDUL.csubject   WITH m.coprt_dsc,;
                  SYSCHDUL.ctrantype  WITH 'W',;
                  SYSCHDUL.nestdur    WITH m.nest_dur ,;
                  SYSCHDUL.ccompleted WITH 'N'  ,;
                  SYSCHDUL.dtrandate  WITH IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
                  SYSCHDUL.dcmpltdate WITH IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),;
                  SYSCHDUL.COPERSTAT  WITH 'O',;
                  SYSCHDUL.lPredComp  WITH .T.                   
        ENDIF
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
        *IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        *B130984,1 HBG 12/04/2006 [End]
          IF m.nAct_Dur <> 0 OR !EMPTY(m.dAct_Fnsh)
            SELECT SYSCHDUL
            REPLACE SYSCHDUL.COPERSTAT  WITH 'C',;
                    SYSCHDUL.CCOMPLETED WITH 'Y'    
          ENDIF
          IF m.lVoid
            SELECT SYSCHDUL
            REPLACE SYSCHDUL.COPERSTAT WITH 'X'
          ENDIF        
        ENDIF  
      ENDIF  
      

    *-- An old line that is modified in the current session.
    CASE cStatus = 'M'        
      *-- Add audit information
      SCATTER MEMVAR MEMO
      m.cAdd_User  = gcUser_ID
      m.dAdd_Date  = ldCurDate
      m.cAdd_Time  = TIME()

      SET DELETED &lcSetDel
      IF !EMPTY(m.cOprt_res) OR !EMPTY(m.cGroup_Id)     
        SELECT SYSCHDUL  
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                    
        *IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID)
        IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID)
        *B130984,1 HBG 12/04/2006 Add [End]
          lcStatus = SYSCHDUL.coperstat
          REPLACE SYSCHDUL.cuser_id   WITH IIF(EMPTY(m.cOprt_Res),m.cGroup_Id,m.cOprt_Res),;
                  SYSCHDUL.csubject   WITH m.coprt_dsc,;
                  SYSCHDUL.ctrantype  WITH 'W',;
                  SYSCHDUL.nestdur    WITH m.nest_dur ,;
                  SYSCHDUL.ccompleted WITH IIF(lcStatus = 'C','Y','N'),;
                  SYSCHDUL.dtrandate  WITH IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
                  SYSCHDUL.dcmpltdate WITH IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh)
         ELSE
          *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
          *INSERT INTO SYSCHDUL;
          *      (cconttype , cseqnumber , ccont_id , csubject,;
          *       ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
          *       lPredComp);
          *VALUES(m.cPrj_Typ,m.cPrj_ID,m.cOprt_Ctg+m.cOprt_ID,;
          *       m.coprt_dsc,'W',m.nest_dur,'N',;
          *       IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
          *       IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
          *       IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
          INSERT INTO SYSCHDUL;
                (cconttype , cseqnumber ,cStyle, ccont_id , csubject,;
                 ctrantype,nestdur,ccompleted,cUser_Id,dtrandate,dcmpltdate,COPERSTAT,;
                 lPredComp);
          VALUES(m.cPrj_Typ,m.cPrj_ID,m.cstyle,m.cOprt_Ctg+m.cOprt_ID,;
                 m.coprt_dsc,'W',m.nest_dur,'N',;
                 IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
                 IIF(EMPTY(m.dclc_strt),m.dEst_strt,m.dclc_strt),;
                 IIF(EMPTY(m.dclc_Fnsh),m.dEst_fnsh,m.dclc_Fnsh),'O',.T.)
          *B130984,1 HBG 12/04/2006 Add [End]
        ENDIF  
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
        *IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        *B130984,1 HBG 12/04/2006 [End]
          IF m.nAct_Dur <> 0 OR !EMPTY(m.dAct_Fnsh)
            SELECT SYSCHDUL
            REPLACE SYSCHDUL.COPERSTAT  WITH 'C',;
                    SYSCHDUL.CCOMPLETED WITH 'Y'
          ENDIF
          IF m.lVoid
            SELECT SYSCHDUL
            REPLACE SYSCHDUL.COPERSTAT WITH 'X'
          ENDIF        
        ENDIF  
      ENDIF

      SELECT PMPRJDT      
      IF BETWEEN(&lc_PMPrjDt..nRecNo, 1, RECCOUNT())
        GO &lc_PMPrjDt..nRecNo
      ENDIF
      GATHER MEMVAR MEMO
      
      SET DELETED OFF
      
    *-- An old line that is deleted in the current session.
    CASE cStatus = 'D'        
      SELECT PMPRJDT
      IF BETWEEN(&lc_PMPrjDt..nRecNo, 1, RECCOUNT())
        GO &lc_PMPrjDt..nRecNo
      ENDIF
  
      
      SCATTER MEMVAR MEMO
      SET DELETED &lcSetDel
      SELECT SYSCHDUL    
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
      *IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_Ctg+m.cOprt_ID)
      IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_Ctg+m.cOprt_ID)
      *B130984,1 HBG 12/04/2006 [End]
        BLANK
        DELETE    
      ENDIF  
      SET DELETED OFF
      
      
      SELECT PMPRJDT
      
      SCATTER MEMVAR MEMO BLANK
      =RLOCK() 
      GATHER MEMVAR MEMO
      DELETE
      UNLOCK
      
               
  ENDCASE  
  
  *-- Update related records for every line.
  SELECT (lc_PMPrjRl)
  *-- Add cOprt_Ctg
  
  SCAN FOR cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg  + cOprt_ID = ;
                       &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_Id+;
                      &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg + &lc_PMPrjDt..cOprt_ID
    SCATTER MEMVAR MEMO
    SET DELETED &lcSetDel
    DO CASE
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                       
      *CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cPrd_Ctg+m.cPrd_Id+'O','SYSCHDUL') 
      *  =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'O','SYSCHDUL') 
        =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *B130984,1 HBG 12/04/2006 [End]      
        REPLACE SYSCHDUL.lPredComp WITH .F.
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                    
      *CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cPrd_Ctg+m.cPrd_Id+'C','SYSCHDUL')
      *  =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'C','SYSCHDUL')
        =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *B130984,1 HBG 12/04/2006 [End]      
        REPLACE SYSCHDUL.lPredComp WITH .T.
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                    
      *CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cPrd_Ctg+m.cPrd_Id+'X','SYSCHDUL')
      *  =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      CASE SEEK(m.cPrj_Typ+m.cPrj_Id+m.cStyle+m.cPrd_Ctg+m.cPrd_Id+'X','SYSCHDUL')
        =SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.coprt_ctg+m.coprt_id+'O','SYSCHDUL')
      *B130984,1 HBG 12/04/2006 [End]      
        REPLACE SYSCHDUL.lPredComp WITH .T.
      OTHERWISE
        IF SEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle +m.cPrd_Ctg + m.cPrd_Id,'PMPRJDT') AND EMPTY(PMPRJDT.dAct_Fnsh)
          *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
          *=SEEK(m.cPrj_Typ + m.cPrj_ID + m.coprt_ctg + m.coprt_id + 'O','SYSCHDUL')
          =SEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.coprt_ctg + m.coprt_id + 'O','SYSCHDUL')
          *B130984,1 HBG 12/04/2006 [End]        
          REPLACE SYSCHDUL.lPredComp WITH .F.
        ENDIF
    ENDCASE      
    SET DELETED OFF
    
    DO CASE
      *-- Case of a new line
      CASE cStatus = 'A'        
        SCATTER MEMVAR MEMO

        SELECT PMPRJRL
        IF SEEK(' ')
          =RLOCK()
          RECALL         
          GATHER MEMVAR MEMO
          UNLOCK
        ELSE
          INSERT INTO PMPRJRL FROM MEMVAR
        ENDIF  

      *-- An old line that is modified in the current session.
      CASE cStatus = 'M'        
        *-- Add audit information
        SCATTER MEMVAR MEMO
        m.cAdd_User  = gcUser_ID
        m.dAdd_Date  = ldCurDate
        m.cAdd_Time  = TIME()
        SELECT PMPRJRL
        IF BETWEEN(&lc_PMPrjRl..nRecNo, 1, RECCOUNT())
          GO &lc_PMPrjRl..nRecNo
        ENDIF
        GATHER MEMVAR MEMO
          
      *-- An old line that is deleted in the current session.
      CASE cStatus = 'D'        
        SELECT PMPRJRL
        IF BETWEEN(&lc_PMPrjRl..nRecNo, 1, RECCOUNT())
          GO &lc_PMPrjRl..nRecNo
        ENDIF

        SCATTER MEMVAR MEMO
        SET DELETED &lcSetDel
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
        *IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cOprt_ctg+m.cOprt_id,'SYSCHDUL')
        IF SEEK(m.cPrj_Typ+ m.cPrj_ID+m.cStyle+m.cOprt_ctg+m.cOprt_id,'SYSCHDUL')
        *B130984,1 HBG 12/04/2006 [End]
          REPLACE SYSCHDUL.lPredComp WITH .T.
        ENDIF  
        SET DELETE OFF

        
        SELECT PMPRJRL
        
        SCATTER MEMVAR MEMO BLANK
        GATHER MEMVAR MEMO
        DELETE

    ENDCASE  
  ENDSCAN  
  
  *E302251,1 AMH Update Nofity Setting for each operation [Start]
  SELECT (lcPMPRJNTF)
  IF SEEK(EVALUATE(lc_PMPrjDt+".cPrj_Typ+"+lc_PMPrjDt+".cPrj_ID+"+lc_PMPrjDt+".cStyle+"+lc_PMPrjDt+".COprt_Ctg+"+lc_PMPrjDt+".cOprt_ID"))
    SCAN REST WHILE cPrj_Typ+cPrj_ID+cStyle+COprt_Ctg+COprt_ID+CUser_ID = EVALUATE(lc_PMPrjDt+".cPrj_Typ+"+lc_PMPrjDt+".cPrj_ID+"+lc_PMPrjDt+".cStyle+"+lc_PMPrjDt+".COprt_Ctg+"+lc_PMPrjDt+".cOprt_ID")
      DO CASE
        *-- Case of a new line
        CASE cStatus = 'A'        
          SCATTER MEMVAR
          
          SELECT PMPRJNTF
          IF SEEK(' ')
            =RLOCK()
            RECALL         
            GATHER MEMVAR
            UNLOCK
          ELSE
            INSERT INTO PMPRJNTF FROM MEMVAR
          ENDIF  
          
        *-- An old line that is modified in the current session.
        CASE cStatus = 'M'        
          * Add audit information
          SCATTER MEMVAR
          m.cAdd_User  = gcUser_ID
          m.dAdd_Date  = ldCurDate
          m.cAdd_Time  = TIME()
          SELECT PMPRJNTF
          IF SEEK(m.cPrj_Typ+m.cPrj_ID+m.cStyle+m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
            GATHER MEMVAR
          ENDIF  
        
        *-- An old line that is deleted in the current session.
        CASE cStatus = 'D'        
          SCATTER MEMVAR
          SELECT PMPRJNTF
          IF SEEK(m.cPrj_Typ+m.cPrj_ID+m.cStyle+m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
            DELETE
          ENDIF
      ENDCASE
    ENDSCAN
  ENDIF
  *E302251,1 AMH [End]
  
  SELECT (lc_PMPrjDt)
ENDSCAN
SET DELETED &lcSetDel
SET EXACT &lcSetExact


*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
*STORE "" TO lcProg , lcKey , lcEvent
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
*HBG [End]
=lfUpdVar()

IF !USED('AUDTRAIL')
  =gfOpenFile(gcDataDir + 'AUDTRAIL' , 'AUDTRAIL' , 'SH')
ENDIF 

IF !SEEK(PADR(lcProg,10)+PADR(lcKey,20),'AUDTRAIL') OR llAddToAdt   
  SELECT (lc_PMPrjDt)
  SCAN
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
    *IF SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'SYSCHDUL') 
    IF SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'SYSCHDUL') 
    *B130984,1 HBG 12/04/2006 [End]
      IF !(&lc_PMPrjDt..lOrginal)
        SELECT SYSCHDUL
        lcStauts = SYSCHDUL.COPERSTAT
        *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
        *LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
        *                  &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID ;
        *                 FOR COPERSTAT <> lcStauts
        LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
                          &lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID ;
                         FOR COPERSTAT <> lcStauts
        *B130984,1 HBG 12/04/2006 [End]        
        IF !FOUND()
          *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
          *=SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+lcStauts,'SYSCHDUL') 
          =SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID + &lc_PMPrjDt..cStyle + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+lcStauts,'SYSCHDUL') 
          *B130984,1 HBG 12/04/2006 [End]
        ENDIF                    
      ENDIF
   
      IF SYSCHDUL.lPredComp AND &lc_PMPrjDt..cStatus $ 'AM' AND &lc_PMPrjDt..lAddToAud
        IF SYSCHDUL.COPERSTAT = 'C'
          SET ORDER TO PMPRJDT IN PMPRJDT
          IF SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID+&lc_PMPrjDt..cStyle+;
                  &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'PMPRJDT')
            SELECT PMPRJDT
            REPLACE PMPRJDT.LAUDTUPD WITH .T.             
          ENDIF       
        ENDIF
        SELECT (lc_PMPrjDt)
        SCATTER MEMVAR MEMO
        ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
        lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,0)
        *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
        lcInform = ""
        *HBG [End]
        =lfUpdAdTrl()
        SELECT (lc_PrjAudt)
        *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
        *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
        *  =gfDoTriger('MFPROJ',PADR(lcEvent,10))
        *ENDIF
        DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
        *HBG [End]
  
      ENDIF 
    ELSE
      IF &lc_PMPrjDt..cStatus $ 'AM' AND &lc_PMPrjDt..lAddToAud AND;
         (!EMPTY(&lc_PMPrjDt..cOprt_res) OR !EMPTY(&lc_PMPrjDt..cGroup_Id))
        SELECT (lc_PMPrjDt)
        SCATTER MEMVAR MEMO
        ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
        lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,0)
        *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
        lcInform = ""
        *HBG [End]
        =lfUpdAdTrl()
        SELECT (lc_PrjAudt)
        *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
        *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
        *  =gfDoTriger('MFPROJ',PADR(lcEvent,10))
        *ENDIF
        DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
        *HBG [End]
      ENDIF   
    ENDIF  
    SET ORDER TO PMPRJDT IN PMPRJDT
    IF SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID+&lc_PMPrjDt..cStyle+;
            &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'PMPRJDT')
      SELECT PMPRJDT
      REPLACE PMPRJDT.lAddToAud WITH .F.             
    ENDIF                
  ENDSCAN
ENDIF

IF PMPRJHD.cPrj_Stts = 'C' 
  =lfComSchdl()
ELSE
  IF PMPRJHD.cPrj_Stts = 'X'
    STORE "" TO lcProg , lcKey
    =lfUpdVar()
    SELECT SYSCHDUL
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
    *SCAN REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id = lcPrj_Typ+lcPrj_ID      
    SCAN REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id = lcPrj_Typ+lcPrj_ID+lcStyle
    *B130984,1 HBG 12/04/2006 [End]
      REPLACE COPERSTAT  WITH 'X'
    ENDSCAN
    SELECT AUDTRAIL
    DELETE FOR capobjnam+key+caudtralid = PADR(lcProg,10)+PADR(lcKey,20) 
  ENDIF
ENDIF

llExitLoop = .T.
llEnbSave  = .F.
llPathSel  = .F.
*-- Back to 'S'elect mode.
SELECT PMPRJHD
REPLACE llok_stat WITH .F.
SHOW GETS

*!*************************************************************
*! Name      : lpClsScr
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Cancel >/< Close > (pbClose).
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

llExitLoop = .T.
llEnbSave  = .F.
llPathSel  = .F.
 
*!*************************************************************
*! Name      : lfvOprtins
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Operations > (pbOprs)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOprtins()
*!*************************************************************
FUNCTION lfvOprtins
*E301869,1 HBG 07/04/2002 Add parameters to call this function from SO , C/T , P/O screens[Begin]
PARAMETERS lcPrjType , lcProjID , lcPathID , lcPrjSty
*PRIVATE lcOprt_ID, lcOldOprt, llChngOprt 
PRIVATE lcOprt_ID, lcOldOprt, llChngOprt , llOutCall

IF TYPE('lcPrjType') = 'C' AND TYPE('lcProjID') = 'C' AND TYPE('lcPathID') = 'C'
  llOutCall   = .T.
  lcPrj_Typ   = lcPrjType
  lcPrj_ID    = lcProjID
  m.cPath_ID  = lcPathID
  lcStyle     = lcPrjSty
  m.cPrj_Stts = 'P'
  lnlatStrt   = 0
  llNewPath   = .T.
  DECLARE laOpertion[1],laUsers[1]
  SELECT cUser_ID ;
    FROM (gcSysHome + 'SYUUSER');
    INTO ARRAY laUsers;
   ORDER BY cUser_ID
  STORE .F. To laScrMode
  laScrMode[4] = .T.
ELSE
  llOutCall   = .F.
ENDIF  
*E301869,1 [End]


IF laScrMode[2] 
  STORE 'DISABLE' TO lcDispStat, lcNewStat, lcRemStat, lcEstDurSt,;
                     lcVoidStat, lcRem1Stat, lcRem2Stat
  SELECT PMPRJHD
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF  
  SELECT PMPRJDT
  lcNotfyMod = 'NOMODIFY'
  lcPMPrjDt = "PMPRJDT"
ELSE
  
  IF m.cPrj_Stts $ 'CX' 
    STORE 'DISABLE' TO lcDispStat, lcNewStat, lcRemStat, lcEstDurSt,;
                     lcVoidStat, lcRem1Stat, lcRem2Stat
    lcNotfyMod = 'NOMODIFY'
    SELECT (lc_PMPrjDt)
    GO TOP
  ELSE  
    *-- Add path lines if there are none and a path is selected.
    *-- Change the value of llNewPath at modify screen
    *-- and make it depend on no. of record in temp.
    *-- file. to privent recolect data again.
    IF laScrMode[3]
      llNewPath = RECCOUNT(lc_PMPrjDt) = 0
    ENDIF

    IF llNewPath
      SELECT PMPTHDT
      IF SEEK(m.cPath_ID)
        lnNumOfLin = 0
        SCAN REST WHILE cpath_id+coprt_ctg+coprt_id = m.cPath_ID
          SCATTER MEMVAR MEMO
          *-- Default remaining duration with the estimated duration for
          *-- every operation
          *-- Add cOprt_Ctg, cCtg_Seq, cOpt_Seq
          
          INSERT INTO (lc_PMPrjDt);
                 (  cPrj_Typ ,   cPrj_ID  ,  cOprt_ID  ,   cOprt_Dsc,;
                    nest_dur ,   nrem_dur ,  cOprt_Ctg ,   cOprt_Res,;   
                    lShw2Cust,   mOprt_Com,  cUpdtMthd ,   mNotify  ,;  
                    cAdd_User,   dAdd_Date,  cAdd_Time ,   cStatus  ,;
                    cStyle   ,   cOprt_Ctg,  cCtg_Seq  ,   cOprt_Seq,;
                    cCal_ID  ,   cGroup_id,lOrginal,cMComplt,lAddToAud);
           VALUES( lcPrj_Typ ,   lcPrj_ID , m.cOprt_ID , m.cOprt_Dsc,;
                  m.nest_dur , m.nest_dur , m.cOprt_Ctg, m.cOprt_Res,;
                  m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd, m.mNotify  ,;    
                  gcUser_ID  , ldCurDate  , TIME()     , 'A'        ,;
                  lcStyle    , m.cOprt_Ctg, m.cCtg_Seq , m.cOprt_Seq,;
                  m.cCal_ID  , m.cGroup_id,.T.,'YYY',.T.)
          
          LCOPRT_RES = IIF(EMPTY(m.cOprt_Res),m.cGroup_id,m.cOprt_Res)
                   
          *-- Fill operations array as well.        
          *-- Add category code and operation description
          laOpertion[ALEN(laOpertion)] = m.cOprt_Ctg + '\' + ;
                                             m.cOprt_ID  + ' ' + ;
                                             SUBSTR(m.cOprt_Dsc, 1, 18)
          DECLARE laOpertion[ALEN(laOpertion) + 1]
          lnNumOfLin = lnNumOfLin  + 1

          *-- Append all predecessor lines to the temporary relations
          *-- file.
          SELECT PMPTHRL
           *-- Add cOprt_Ctg
          IF SEEK(m.cPath_ID + m.cOprt_Ctg + m.cOprt_ID)
            SCAN REST WHILE   cPath_ID +   cOprt_Ctg +   cOprt_ID = ;
                              m.cPath_ID + m.cOprt_Ctg + m.cOprt_ID
              SCATTER MEMVAR MEMO
              *-- Add cOprt_Ctg, cPrd_Ctg
              INSERT INTO (lc_PMPrjRl);
                    (  cPrj_Typ ,   cPrj_ID  ,  cStyle   ,   cOprt_ID ,;
                       cPrd_ID  ,   nPrd_Lg  ,  cAdd_User,   dAdd_Date,;
                       cAdd_Time ,   cStatus ,  cOprt_Ctg, cPrd_Ctg);
               VALUES( lcPrj_Typ ,   lcPrj_ID, lcStyle   , m.cOprt_ID ,;
                      m.cPrd_ID  , m.nPrd_Lg , gcUser_ID   ,  ldCurDate ,;
                         TIME()     , 'A'    ,m.cOprt_Ctg,m.cPrd_Ctg)    
            ENDSCAN
          ENDIF  
          
          *E302251,1 AMH Collecting Data for Nofity Setting Screen [Start]
          SELECT PMPTHNTF
          IF SEEK(m.cPath_ID + m.cOprt_Ctg + m.cOprt_ID)
            SCAN REST WHILE CPATH_ID+COPRT_CTG+COPRT_ID+CUSER_ID = m.cPath_ID + m.cOprt_Ctg + m.cOprt_ID
              SCATTER MEMVAR
              m.cPrj_Typ = lcPrj_Typ
              m.cPrj_ID  = lcPrj_ID
              m.cStyle   = lcStyle
              m.cStatus  = 'A'
              INSERT INTO (lcPMPRJNTF) FROM MEMVAR
            ENDSCAN
          ENDIF
          *E302251,1 AMH [End]
          
          SELECT PMPTHDT
        ENDSCAN  
        IF ALEN(laOpertion) > 1
          DECLARE laOpertion[ALEN(laOpertion) - 1]
        ENDIF
        *-- Calculate estimated dates of new entries.
        =lfGetEstDt(@lc_PMPrjDt, @lc_PMPrjRl,.F.)
      ENDIF  && ENDIF SEEK(m.cPath_ID)
      *-- Collect available categories in an array
      SELECT DISTINCT cOprt_Ctg ;
        FROM (lc_PMPrjDt);
        INTO ARRAY laCategrie

    ENDIF  
    SELECT (lc_PMPrjDt)
    GO TOP
    STORE IIF(lnNumOfLin = 0 .OR. lVoid, 'DISABLE', 'ENABLE') ;
          TO lcDispStat, lcVoidStat, lcRemStat
    IF lnNumOfLin = 0 .OR. lVoid
      STORE 'DISABLE' TO lcDispStat, lcVoidStat, lcRemStat,;
                         lcRem1Stat, lcRem2Stat   
    ELSE
      STORE 'DISABLE' TO lcDispStat, lcVoidStat, lcRemStat
      IF m.cPrj_Stts = 'P' .OR. EMPTY(dest_strt)
        lcRem1Stat = 'ENABLE'
        lcRem2Stat = 'DISABLE'        
      ELSE
        lcRem1Stat = 'DISABLE'        
        lcRem2Stat = 'ENABLE'
      ENDIF
    ENDIF                     
    lcNewStat  = 'ENABLE'
    *-- Disable estimated duration field if the project has been scheduled
    *-- before.
    lcEstDurSt = IIF(EMPTY(m.dclc_strt), lcDispStat, 'DISABLE')

  ENDIF    
  lcNotfyMod = IIF(EMPTY(laUsers[1]), '', 'NOMODIFY')
  llChngOprt = .F.  
  lcPMPrjDt = lc_PMPrjDt 
ENDIF

lcCurrCtg  = SPACE(3)
STORE cOprt_Ctg TO lcOprt_Ctg, lcOldCtg
lcCtg_Dsc  = LOOKUP(PMCTGHD.cCtg_Dsc, cOprt_Ctg,;
                   PMCTGHD.cOprt_Ctg,'PMCTGHD')

STORE cOprt_ID TO lcOprt_ID, lcOldOprt 
lcNotify    = IIF(!EMPTY(laUsers[1]), IIF(!EMPTY(ALLTRIM(mNotify)),;
                  STRTRAN(ALLTRIM(mNotify), '|',', ') + '.', ''),;
                  ALLTRIM(mNotify))
m.cCal_Id   = cCal_ID
m.nEst_dur  = nEst_dur
m.cOprt_Dsc = cOprt_Dsc
                  
lcPredec    = IIF(lVoid, '', lfGetPred())
lcShw2Cust  = lShw2Cust
IF EOF()
  STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
ELSE
  STORE cUpdtMthd TO lcUpdtMthd, puUpdtMthd
ENDIF  
IF laScrMode[2]
  cCal_ID = PMPRJDT.cCal_ID
ELSE
  cCal_ID = &lc_PMPrjDt..cCal_ID
ENDIF

*E301869,1 HBG 07/04/2002 IF call this function from SO , C/T , P/O screens, don't call the operation screen[Begin]
IF !llOutCall
*E301869,1 [End]
  lcNoteStat = IIF(EOF(), 'DISABLE', 'ENABLE')  
  lcNotfySta = IIF(EOF() .OR. EMPTY(laUsers[1]), 'DISABLE', 'ENABLE')  
  lcSelAlis = IIF(laScrMode[2],'PMPRJDT',lc_PMPrjDt)
  llOpnTask = .F.
  *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
  *IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *               &lcSelAlis..cOprt_Ctg+&lcSelAlis..cOprt_ID,'SYSCHDUL') AND;
  *               !(SYSCHDUL.COPERSTAT $ 'OH')
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle)) +;
                 &lcSelAlis..cOprt_Ctg+&lcSelAlis..cOprt_ID,'SYSCHDUL') AND;
                 !(SYSCHDUL.COPERSTAT $ 'OH')
  *B130984,1 HBG 12/04/2006 [End]  
    llOpnTask = .T.
  ENDIF
  lcPredStat = IIF(llOpnTask,'DISABLE',IIF(ALEN(laOpertion) > 1 , 'ENABLE', 'DISABLE'))
  

  lcRemPrm   = IIF(m.cPrj_Stts = 'P' .OR. EMPTY(dest_strt),;
                  lcTRemove, lcTVoid)
  
  SET ORDER TO PMPRJDTS IN (lcPMPrjDt)
  LOCATE
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
  PUSH KEY
  ON KEY LABEL ESC T=0
  DO (gcScrDir+"MFPROJA.SPR")
  ON KEY LABEL ESC DO  lpEscScr
  POP KEY
*E301869,1 HBG 07/04/2002 End if call this function from SO , C/T , P/O screens[Begin]
ENDIF
*E301869,1 [End]


IF laScrMode[3] OR laScrMode[4]
  IF m.cPrj_Stts = 'P'
    IF lnNumOfLin = 0
      m.dest_fnsh = {} 
    ELSE
      *HBG
      IF !llupdstrtd
      *HBG      
        lcOldVal    = m.dEst_Strt
        ldOldStrt   = m.dEst_Strt
        ldOldFnsh   = m.dEst_fnsh 
        *-- ReCalculate estimated dates if any change occurs.
        =lfGetEstDt(@lc_PMPrjDt, @lc_PMPrjRl,.F.)
        ldOldEstS = m.dEst_Strt
        ldOldEstF = m.dEst_fnsh
        llFromOper = .T.
        =lfvLatStrt()
      *HBG
      ENDIF
      *HBG
    ENDIF
  ELSE
    IF !m.lSchedual AND llChngOprt
      m.lSchedual = llChngOprt
      llSchedual = m.lSchedual
    ENDIF  
  ENDIF  
  
  *E301869,2 AMH Fix bug of don't get templet taskes [Start]
  *=lfChkComp()
  =IIF(RECCOUNT(lc_PMPrjDt) = 0,.T.,lfChkComp())
  *E301869,2 AMH [End]
  
  STORE .F. TO llChngOprt, llNewPath
ENDIF
=gfClearKey()
SELECT PMPRJHD

*E301869,1 HBG 07/04/2002 IF call this function from SO , C/T , P/O screens[Begin]
IF !llOutCall
*E301869,1 [End]
  lcBrfields  = lcOldBrFild
*E301869,1 HBG 07/04/2002 End if call this function from SO , C/T , P/O screens[Begin]
ENDIF
*E301869,1 [End]

*!*************************************************************
*! Name      : lfActBrows
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Browse activation function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfActBrows()
*!*************************************************************
*!Modifications:
*!*************************************************************
FUNCTION lfActBrows

lcKeyFld  = IIF(laScrMode[2], [KEY ']+ m.cPrj_Typ + m.cPrj_ID + m.cStyle +['], '')
lnBrRecNo = RECNO()

BROWSE FIELDS cMarker   = IIF(RECNO() = lnBrRecNo, '', ' ');
                        : H = ' ' :R :W = .F.,;
              cOprt_Ctg : H = 'Ctg.'  :R:6,;
              cOprt_ID  : H = 'Op.ID':R:8,;                                                                          
              nrem_dur  : H = 'RDr' :R:5:P='999',;
              cOprt_Dsc : H = 'Task Name' :R :&lnOprWdth,; 
              dclc_strt : H = 'Clc.Strt':R,;
              dclc_fnsh : H = 'Clc.Fnsh':R;
         WINDOW MFPROJA0 ;
         IN WINDOW MFPROJA ;
         WHEN lfwBrwWhen(ALIAS(), (laScrMode[3] OR laScrMode[4]) ;
              .AND. m.cPrj_Stts $ 'PI' .AND. !lVoid );
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOEDIT;
         NOWAIT;
         FOR LORGINAL;
         SAVE;
         NOCLEAR;
         TITLE lcBrTtl &lcKeyFld

LOCATE
KEYBOARD "{ALT+B}" CLEAR

*!*************************************************************
*! Name      : lfwBrwWhen
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : WHEN function of the browse command
*!*************************************************************
*! Calls              : lfRefresh()
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  BROWSE ..  WHEN lfwBrwWhen()
*!*************************************************************
FUNCTION lfwBrwWhen
PARAMETERS lcPthDetFl, llEnableAll

*E301869,2 AMH Don't Enable for hold operations [Start]
llEnableAll = llEnableAll .AND. !lfIsHold()
*E301869,2 AMH [End]

SELECT (lcPthDetFl)
SET ORDER TO TAG PMPRJDTS

IF EMPTY(cOprt_Ctg)
  STORE ' ' TO lcCtg_Dsc, lcNotify, lcPredec 
ELSE
  lcOprt_Ctg = cOprt_Ctg
  lcCtg_Dsc  = LOOKUP(PMCTGHD.cCtg_Dsc, cOprt_Ctg,;
                      PMCTGHD.cOprt_Ctg,'PMCTGHD')

  lcOprt_ID  = cOprt_ID
  lcNotify   = IIF(!EMPTY(laUsers[1]), IIF(!EMPTY(ALLTRIM(mNotify)),;
                    STRTRAN(ALLTRIM(mNotify), '|',', ') + '.', ''),;
                    ALLTRIM(mNotify))
ENDIF

IF EOF()
  STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
  lcRemPrm   = lcTRemove
  lcPredec   = ''
  STORE SPACE(0) TO lcOprt_Ctg,lcOprt_id,m.coprt_dsc
  SHOW GET lcOprt_id
  SHOW GET lcOprt_Ctg
ELSE
  STORE cUpdtMthd TO lcUpdtMthd, puUpdtMthd
  lcRemPrm    = IIF(m.cPrj_Stts = 'P' .OR. EMPTY(dest_strt), lcTRemove, lcTVoid)
  lcPredec    = IIF(lVoid, '', lfGetPred())
  m.cCal_Id   = cCal_ID
  m.nEst_dur  = nEst_dur
  m.cOprt_Dsc = cOprt_Dsc
ENDIF  

lcShw2Cust = lShw2Cust
IF laScrMode[2]
  cCal_ID = PMPRJDT.cCal_ID
ELSE
  cCal_ID = &lc_PMPrjDt..cCal_ID
ENDIF

IF !EMPTY(lcOprt_Ctg)
  SHOW GET ibOprt_Ctg  DISABLE
  SHOW GET lcOprt_Ctg  DISABLE
  SHOW GET m.cOprt_Dsc DISABLE
  SHOW GET lcOprt_Res  DISABLE
  SHOW GET puUser      DISABLE
ENDIF
IF !EMPTY(lcOprt_id)
  SHOW GET ibOprt_ID  DISABLE
  SHOW GET lcOprt_id  DISABLE
ENDIF  
IF llEnableAll
  lcDispStat = ''
  SHOW GET ibCal_ID   ENABLE
  SHOW GET cCal_ID    ENABLE
  SHOW GET m.cOprt_Dsc  ENABLE
  SHOW GET ibUpdtMthd ENABLE
  SHOW GET puUpdtMthd ENABLE
  SHOW GET lcShw2Cust ENABLE
  
  IF EMPTY(m.dclc_strt)
    SHOW GET nest_dur ENABLE
  ELSE  
    SHOW GET nest_dur DISABLE
  ENDIF
  SHOW GET nrem_dur   ENABLE
  SHOW GET nAct_dur   ENABLE
  SHOW GET nprc_comp  ENABLE
  SHOW GET dact_strt  ENABLE
  SHOW GET dact_fnsh  ENABLE
  SHOW GET pbNew      ENABLE 
  SHOW GET pbNotes    ENABLE 
  SHOW GET pbSort     ENABLE 
  
  llOpnTask = .F.
  *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
  *IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *               cOprt_Ctg+cOprt_ID,'SYSCHDUL') AND !(SYSCHDUL.COPERSTAT $ 'OH')
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +  SUBSTR(lcStyle,1,LEN(cStyle)) +;
                 cOprt_Ctg+cOprt_ID,'SYSCHDUL') AND !(SYSCHDUL.COPERSTAT $ 'OH')
  *B130984,1 HBG 12/04/2006 [End]  
    llOpnTask = .T.
  ENDIF
  IF ALEN(laOpertion) > 1 .AND. !lVoid AND !llOpnTask 
    
    SHOW GET pbPredecsr ENABLE
    SHOW GET lcPredec   ENABLE 
  ELSE
    SHOW GET pbPredecsr DISABLE
    SHOW GET lcPredec   DISABLE
  ENDIF
  IF EMPTY(laUsers[1])
    SHOW GET pbNotify  DISABLE
    SHOW GET lcNotify   ENABLE
  ELSE
    SHOW GET pbNotify   ENABLE
    SHOW GET lcNotify   ENABLE 
  ENDIF
  IF m.cPrj_Stts = 'P' .OR. EMPTY(dest_strt)
    SHOW GET pbRemove,1  ENABLE  PROMPT lcRemPrm
    SHOW GET pbVoid      DISABLE
    SHOW GET pbWinRemove ENABLE
  ELSE
    SHOW GET pbWinRemove DISABLE
    IF lVoid
      SHOW GET pbVoid     DISABLE
      SHOW GET pbRemove,1 DISABLE PROMPT lcRemPrm
    ELSE
      SHOW GET pbVoid     ENABLE
      SHOW GET pbRemove,1 ENABLE  PROMPT lcRemPrm
    ENDIF  
  ENDIF  
  
ELSE
  SHOW GET ibCal_ID   DISABLE  
  SHOW GET cCal_ID    DISABLE  
  SHOW GET m.cOprt_Dsc  DISABLE  
  SHOW GET ibUpdtMthd DISABLE  
  SHOW GET puUpdtMthd DISABLE  
  SHOW GET lcShw2Cust DISABLE  
  SHOW GET nest_dur   DISABLE  
  SHOW GET pbRemove,1 DISABLE PROMPT lcRemPrm
  SHOW GET nest_dur   DISABLE
  SHOW GET nrem_dur   DISABLE
  SHOW GET nAct_dur   DISABLE
  SHOW GET nprc_comp DISABLE
  SHOW GET dact_strt  DISABLE
  SHOW GET dact_fnsh  DISABLE
  
  IF m.cPrj_Stts = 'P' .OR. EMPTY(dest_strt)
    SHOW GET pbVoid      DISABLE
    SHOW GET pbWinRemove DISABLE
  ELSE
    SHOW GET pbWinRemove DISABLE
    SHOW GET pbVoid      DISABLE    
  ENDIF
  IF (laScrMode[3] OR laScrMode[4]) .AND. m.cPrj_Stts $ 'PI'
    SHOW GET pbNew    ENABLE 
    IF EOF()
      SHOW GET pbSort DISABLE 
    ELSE
      SHOW GET pbSort ENABLE
    ENDIF
  ENDIF  
  IF EOF()
    SHOW GET pbNotes     DISABLE
    SHOW GET pbNotify    DISABLE
    SHOW GET lcNotify    DISABLE
    SHOW GET pbPredecsr  DISABLE
    SHOW GET lcPredec    DISABLE
  ELSE
    SHOW GET pbNotes     ENABLE 
    IF EMPTY(laUsers[1])
      SHOW GET pbNotify    DISABLE
      SHOW GET lcNotify    DISABLE
    ELSE
      SHOW GET pbNotify    ENABLE
      SHOW GET lcNotify    ENABLE   
    ENDIF  
    llOpnTask = .F.
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
    *IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *             cOprt_Ctg+cOprt_ID,'SYSCHDUL') AND !(SYSCHDUL.COPERSTAT $ 'OH')
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle)) +;
                 cOprt_Ctg+cOprt_ID,'SYSCHDUL') AND !(SYSCHDUL.COPERSTAT $ 'OH')
    *B130984,1 HBG 12/04/2006 [End]    
      llOpnTask = .T.
    ENDIF
    IF ALEN(laOpertion) > 1 .AND. !lVoid AND !llOpnTask 
      SHOW GET pbPredecsr ENABLE
      SHOW GET lcPredec   ENABLE 
    ELSE
      SHOW GET pbPredecsr DISABLE
      SHOW GET lcPredec   DISABLE
    ENDIF
  ENDIF   
ENDIF


PuUser     = IIF(EMPTY(coprt_res) AND EMPTY(cGroup_Id),'User',;
             IIF(EMPTY(coprt_res),'Group of users','User'))
lcOprt_res = IIF(EMPTY(coprt_res),cGroup_id,coprt_res)

IF laScrMode[2]
  SHOW GET lcOprt_res DISABLE
  SHOW GET PuUser     DISABLE
ELSE
  IF EMPTY(lcOprt_res) OR laScrMode[4] 
    SHOW GET lcoprt_res ENABLE
    SHOW GET PuUser     ENABLE
  ELSE
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
    *IF SEEK(SUBSTR(lcPrj_Typ,1,1)+SUBSTR(lcPrj_ID,1,6)+SUBSTR(lcOprt_Ctg+lcOprt_ID,1,8),'SYSCHDUL')
    IF SEEK(SUBSTR(lcPrj_Typ,1,1)+SUBSTR(lcPrj_ID,1,6)+SUBSTR(lcStyle,1,12)+SUBSTR(lcOprt_Ctg+lcOprt_ID,1,8),'SYSCHDUL')
    *B130984,1 HBG 12/04/2006 [End]
      IF EMPTY(SYSCHDUL.cOperStat) OR SYSCHDUL.cOperStat = 'O'
        SHOW GET lcOprt_res ENABLE
        SHOW GET PuUser     ENABLE
      ELSE
        SHOW GET lcOprt_res DISABLE
        SHOW GET PuUser     DISABLE   
      ENDIF  
    ELSE
      SHOW GET lcOprt_res ENABLE
      SHOW GET PuUser     ENABLE
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(dact_fnsh) 
  SHOW GET ibCal_ID    DISABLE  
  SHOW GET cCal_ID     DISABLE  
  SHOW GET m.cOprt_Dsc DISABLE  
  SHOW GET ibUpdtMthd  DISABLE  
  SHOW GET puUpdtMthd  DISABLE  
  SHOW GET lcShw2Cust  DISABLE  
  SHOW GET nest_dur    DISABLE  
  SHOW GET pbRemove,1  DISABLE PROMPT lcRemPrm
  SHOW GET nest_dur    DISABLE
  SHOW GET nrem_dur    DISABLE
  SHOW GET nAct_dur    DISABLE
  SHOW GET nprc_comp   DISABLE
  SHOW GET dEst_strt   DISABLE
  SHOW GET dEst_fnsh   DISABLE
  SHOW GET dClc_strt   DISABLE
  SHOW GET dClc_fnsh   DISABLE
  SHOW GET dact_strt   DISABLE
  SHOW GET dact_fnsh   DISABLE
  SHOW GET pbVoid      DISABLE
  SHOW GET pbWinRemove DISABLE
ELSE
  llOpen = .F.
  
  =lfChkPred()
  IF llOpen
    SHOW GET nAct_dur  DISABLE
    SHOW GET dAct_strt DISABLE
    SHOW GET dAct_fnsh DISABLE
  ENDIF
ENDIF
  
=lfRefresh()
lnBrRecNo = RECNO()
SHOW WINDOW (lcBrTtl) REFRESH 

*!*************************************************************
*! Name      : lfvNew
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for pudh button < New > (pbNew)
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNew()
*!*************************************************************
FUNCTION lfvNew

lnCurAlias = SELECT(0)
SELECT (lc_PMPrjDt)

STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
ACTIVATE WINDOW MFPROJA1
lcOldOprt  = cOprt_ID
lcOldCtg   = cOprt_Ctg
GO BOTTOM
IF !EOF()
  SKIP
ENDIF

lcOprt_id  = SPACE(5)

=lfwBrwWhen(lc_PMPrjDt)

lcOprt_Ctg = lcCurrCtg  
SHOW GET lcOprt_Ctg  ENABLE
SHOW GET ibOprt_Ctg  ENABLE
_CUROBJ    = OBJNUM(lcOprt_Ctg)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvRemove
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for pbRemove < Remove > (pbRemove)
*!             Removes a line from the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
PRIVATE llMayDelte, lnCurAlias

lnCurAlias = SELECT(0)

llVoiding = m.cPrj_Stts <> 'P' .AND. !EMPTY(dest_strt) 


llMayDelte = .F.

SET ORDER TO TAG PMPRJRLP IN (lc_PMPrjRl) 
IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
      SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjRl..cOprt_Ctg))+;
      SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjRl..cOprt_ID)), lc_PMPrjRl)
  IF !llVoiding
    llMayDelte = gfModalgen("TRM38234B38006","DIALOG",'Remove') = 1
  ELSE
    llMayDelte = gfModalgen("TRM38234B38006","DIALOG",'Void') = 1
  ENDIF                            
ELSE
  IF !llVoiding
    llMayDelte = gfModalgen("TRM38208B38006","DIALOG") = 1                       
  ELSE
    llMayDelte = gfModalgen("TRM38236B38006","DIALOG") = 1
  ENDIF
ENDIF                       
llPred = .F.                                            
IF llMayDelte
  SELECT (lc_PMPrjRl)
  lnI = 0
  SCAN FOR cPrj_Typ +  cPrj_ID +  cStyle +  cPrd_Ctg + cPrd_ID = ;
                  SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                  SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
                  SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
    llPred = .T.                   
    REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1) 
    lnI = lnI + 1
    DIME laOper[lnI]
    laOper[lnI] = cOprt_Ctg + cOprt_ID
  ENDSCAN                  
  DELETE FOR  cPrj_Typ +  cPrj_ID +  cStyle +  cPrd_Ctg + cPrd_ID = ;
                SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
                SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))

  SET ORDER TO TAG PMPRJRL
  REPLACE ALL cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1);
       FOR  cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
            SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
            SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
            SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))

  DELETE FOR  cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
            SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
            SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
            SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
  
  *E302251,1 AMH Delete Notify Setting Records [Start]
  SELECT (lcPMPRJNTF)
  IF SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id)
    SCAN REST WHILE cPrj_Typ+cPrj_ID+cStyle+COprt_Ctg+COprt_ID+CUser_ID = PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id
      REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
      DELETE
    ENDSCAN
  ENDIF
  *E302251,1 AMH [End]
  
  SELECT (lc_PMPrjDt)

  lcRemCatg = cOprt_Ctg
  lcPrvOrd = ORDER()
  SET ORDER TO PMPRJDT 
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
      SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
      SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID)),lc_PMPrjDt)
      
    IF llVoiding
      REPLACE lVoid     WITH .T.,;
              cStatus   WITH SUBSTR('MMS', AT(cStatus, 'SMA'), 1),;
              lAddToAud WITH .T.
      IF !laScrMode[4]
        IF llPred 
          lnI = 1        
          SCAN FOR cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
                  SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                  SUBSTR(lcStyle,1,LEN(cStyle))+laOper[lnI]            
            REPLACE cStatus    WITH SUBSTR('MMS', AT(cStatus, 'SMA'), 1),;
                    lAddToAud  WITH .T.,;
                    lSchAdToAd WITH .T.
            IF lnI < ALEN(laOper)
              lnI = lnI + 1      
            ENDIF  
          ENDSCAN        
        ENDIF  
      ENDIF  
      llAddToAdt = .T.
    ELSE
      lnNumOfLin = lnNumOfLin - 1
      REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
      DELETE
      IF !laScrMode[4]      
        IF llPred 
          lnI = 1        
          SCAN FOR cPrj_Typ +  cPrj_ID +  cStyle +  cOprt_Ctg + cOprt_ID = ;
                  SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                  SUBSTR(lcStyle,1,LEN(cStyle))+laOper[lnI]            
            REPLACE cStatus    WITH SUBSTR('MMS', AT(cStatus, 'SMA'), 1),;
                    lAddToAud  WITH .T.,;
                    lSchAdToAd WITH .T.
            IF lnI < ALEN(laOper)
              lnI = lnI + 1      
            ENDIF  
          ENDSCAN        
        ENDIF  
      ENDIF  
    ENDIF
    SET ORDER TO (lcPrvOrd)
    IF !EOF()
      SKIP
    ENDIF  
    IF EOF() .AND. lnNumOfLin > 0
      SKIP -1
    ENDIF  
  ENDIF
  
  llChngOprt = .T.
  llUpdStrtd = .F.
    
  lnElement = ASCAN(laOpertion, lcOprt_Ctg + '\' + lcOprt_ID)
  IF lnElement > 0
    IF ADEL(laOpertion, lnElement) > 0
      IF ALEN(laOpertion) > 1
        DIMENSION laOpertion[ALEN(laOpertion)-1]
      ELSE
        DIMENSION laOpertion[1]
        laOpertion = ''
      ENDIF  
    ENDIF
  ENDIF

  IF lnNumOfLin > 0
    lnRecNo = RECNO()
    SET ORDER TO TAG PMPRJDT

    IF !SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
             SUBSTR(lcStyle,1,LEN(cStyle)) + lcRemCatg) 
      lnElement = ASCAN(laCategrie,  lcRemCatg) 
      IF lnElement > 0 .AND. ADEL(laCategrie, lnElement) > 0 
        IF ALEN(laCategrie) > 1
          DIMENSION laCategrie[ALEN(laCategrie) -1]
        ELSE
          DIMENSION laCategrie[1]
          laCategrie = ''
        ENDIF
      ENDIF
    ENDIF  
    SET ORDER TO TAG PMPRJDTS    
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ENDIF
  ELSE
    lcOprt_Ctg = SPACE(3)
    lcOprt_ID  = SPACE(5)
    DIMENSION laCategrie[1]
    laCategrie = ''  
  ENDIF  
  IF ALEN(laOpertion) < 2
    SHOW GET pbPredecsr DISABLE
  ENDIF 
  glUpdated   = .T.
  =lfwBrwWhen(lc_PMPrjDt, !EOF() .AND. !lVoid))
ENDIF  
SET ORDER TO TAG PMPRJRL IN (lc_PMPrjRl) 
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : READ Activate function of MFPROJA.SCX
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
=gfClearKey()

*E302251,1 AMH Activate Nofity Setting window [Start]
*ON KEY LABEL ESC T=0
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrUser)
ON KEY LABEL ESC DO lfEsc
*E302251,1 AMH [End]

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : READ Deactivate function of MFPROJA.SCX
*!*************************************************************
*! Calls     : lfTrapKey
*!*************************************************************
*! Returns            :  .f.
*!*************************************************************
*! Example            :  =lfReadDeact()
*!*************************************************************
FUNCTION lfReadDeact

IF WONTOP() = lcBrTtl 
  ON KEY LABEL TAB        DO lpTab     WITH 'MFPROJA1', OBJNUM(lcOprt_id)
  ON KEY LABEL BACKTAB    DO lpTab     WITH 'MFPROJA1', OBJNUM(pbClose)
  ON KEY LABEL CTRL+ENTER llDummy = lfTrapKey('MFPROJA1', OBJNUM(pbClose))
  *ON KEY LABEL ESC        llDummy = lfTrapKey('MFPROJA1', OBJNUM(pbClose))
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
  IF (laScrMode[3] OR laScrMode[4]) .AND. m.cPrj_Stts $ 'PI'
    ON KEY LABEL ALT+W    llDummy = lfTrapKey('MFPROJA1', OBJNUM(pbNew))
    ON KEY LABEL ALT+V    llDummy = !EOF(lc_PMPrjDt) .AND. !lVoid .AND. lfvRemove()
    ON KEY LABEL ALT+S    llDummy = !EOF(lc_PMPrjDt) .AND. lfvShw2Cus()
    ON KEY LABEL ALT+T    llDummy = !EOF(lc_PMPrjDt) .AND. lfvSort()
  ENDIF  
  ON KEY LABEL ALT+F      llDummy = !EOF(lc_PMPrjDt) .AND. !EMPTY(laUsers[1]) .AND. ;
                                 lfTrapKey('MFPROJA1', OBJNUM(pbNotify))
  ON KEY LABEL ALT+P      llDummy = ALEN(laOpertion) > 1 .AND. !lVoid .AND.;
                                 lfTrapKey('MFPROJA1', OBJNUM(pbPredecsr))
  ON KEY LABEL ALT+N      llDummy = !EOF(lc_PMPrjDt) .AND. ;
                                 lfTrapKey('MFPROJA1', OBJNUM(pbNotes))
  ON KEY LABEL ALT+C      llDummy = lfTrapKey('MFPROJA1', OBJNUM(pbClose))
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfTrapKey
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Trap of keys
*!*************************************************************
*! Passed Parameters  :  lcWindName : Window name to activate
*!                       lnObjNum   : object number to focus.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  llDummy = lfTrapKey('MFG10002', OBJNUM(pbNew)
*!*************************************************************
FUNCTION lfTrapKey
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum
KEYBOARD CHR(13) CLEAR


*!*************************************************************
*! Name      : lpTab
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Trap of tab keys
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  DO lpTab WITH 'MFG10002', OBJNUM(pbNew)
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum

*!*************************************************************
*! Name      : lfvOprtCtg
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates category code field
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOprtCtg()
*!*************************************************************
FUNCTION lfvOprtCtg
PRIVATE lnCurAlias 

IF MDOWN() .AND. WONTOP() <> lcBrTtl
  RETURN
ENDIF  

lcOprt_Ctg   = PADR(ALLTRIM(lcOprt_Ctg), 3)
lnCurAlias   = SELECT(0)
llBrowse     = llBrowse .OR. '?' $ lcOprt_ID

FOR lnI = 1 To 4
  IF laScrMode[lnI]
    lnOdSMode = lnI
    EXIT
  ENDIF
ENDFOR

IF !llBrowse .AND. EMPTY(lcOprt_Ctg)  
  SET ORDER TO TAG PMPRJDT IN (lc_PMPrjDt)
  =lfwBrwWhen(lc_PMPrjDt,;
              SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOldCtg ,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
              SUBSTR(lcOldOprt,1,LEN(&lc_PMPrjDt..cOprt_ID)),;
                   lc_PMPrjDt) .AND. !lVoid)

ELSE  
  IF !llBrowse .AND. ASCAN(laCategrie, lcOprt_Ctg) > 0 
    SHOW GET lcOprt_Ctg DISABLE
    SHOW GET ibOprt_Ctg DISABLE
    SHOW GET lcOprt_ID  ENABLE
    SHOW GET ibOprt_ID  ENABLE
    _CUROBJ = OBJNUM(lcOprt_ID)     
  ELSE
    
    lcBasWnd = gcBaseWind  
    
    IF lfVldKey('PMCTGHD', 'PMCTGHD', @lcCtgFlds, @lcCtgTtl,;
                'cOprt_Ctg', 'Category code ' + ALLTRIM(lcOprt_Ctg) + ;
                ' is not found in the categories of tasks file.');
      .AND. (laScrMode[2] .OR. lfExtPrg('A',lcOprt_Ctg))

      STORE .F. To laScrMode
      laScrMode[lnOdSMode] = .T.
      lcBaseFile = 'PMPRJHD'    
      
      gcBaseWind = lcBasWnd 
      lcBaseWind  = gcBaseWind
      
      
      lcStamp =  PMPRJHD.cAdd_User+IIF(EMPTY(PMPRJHD.dAdd_Date),'',DTOC(PMPRJHD.dAdd_Date))+;
                     PMPRJHD.cAdd_Time
      
      
      lcCurrCtg = lcOprt_Ctg
      ACTIVATE WINDOW MFPROJA
      lcCtg_Dsc = PMCTGHD.cCtg_Dsc
      =lfRefresh()

      SELECT (lc_PMPrjDt)
      IF !EMPTY(lcOprt_Ctg)
        SHOW GET ibOprt_Ctg DISABLE
        SHOW GET lcOprt_Ctg DISABLE
      ENDIF
        
      IF ASCAN(laCategrie, lcOprt_Ctg) > 0 .OR. !lfAddOprs()  
        SHOW GET lcOprt_ID ENABLE
        SHOW GET ibOprt_ID ENABLE
        _CUROBJ = OBJNUM(lcOprt_ID)    
      ELSE
        SHOW GET lcOprt_ID   DISABLE
        SHOW GET ibOprt_ID   DISABLE
        SHOW GET m.cOprt_Dsc DISABLE
        SHOW GET lcOprt_Res  DISABLE
        SHOW GET puUser      DISABLE
        SELECT (lc_PMPrjDt) 
        lnCurTag = VAL(SYS(21))
        SET ORDER TO TAG PMPRJDT
        SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
             SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))
        SET ORDER TO (lnCurTag)
                
        *-- ReCalculate estimated dates if any change occurs.
        =lfGetEstDt(@lc_PMPrjDt, @lc_PMPrjRl,.F.)
        
        =lfwBrwWhen(lc_PMPrjDt, .T.)
        _CUROBJ = OBJNUM(pbNew)
      ENDIF
    ELSE
      lcOprt_Ctg = SPACE(3)
      _CUROBJ    = _CUROBJ
    ENDIF
    SET ORDER TO TAG PMCTGDT IN PMCTGDT   
  ENDIF  
ENDIF


STORE .F. To laScrMode
laScrMode[lnOdSMode] = .T.


SELECT (lnCurAlias) 

*!*************************************************************
*! Name      : lfvOprt_ID
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              : Ariabrow  
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOprt_ID()
*!*************************************************************
FUNCTION lfvOprt_ID
PRIVATE lnCurAlias


IF MDOWN() .AND. WONTOP() <> lcBrTtl
  RETURN
ENDIF  

lcOprt_ID    = PADR(ALLTRIM(lcOprt_ID), 5)
lnCurAlias   = SELECT(0)
llBrowse     = llBrowse .OR. '?' $ lcOprt_ID

IF !llBrowse .AND. EMPTY(lcOprt_ID)  
  SET ORDER TO TAG PMPRJDT IN (lc_PMPrjDt)
  
  =lfwBrwWhen(lc_PMPrjDt,;
              SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOldCtg ,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
              SUBSTR(lcOldOprt,1,LEN(&lc_PMPrjDt..cOprt_ID)),;              
                   lc_PMPrjDt) .AND. !lVoid)
  IF EOF(lc_PMPrjDt)
    lcOprt_Ctg = SPACE(3)
    SHOW GET lcOprt_Ctg
  ENDIF  
ELSE  
  llShouldAdd = .T.
  SET ORDER TO TAG PMPRJDT IN (lc_PMPrjDt)
  IF !llBrowse .AND. SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID)), lc_PMPrjDt)
    LOCATE REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID =;
                     SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID));
                FOR !lVoid 
                 
    IF FOUND()             
      SET ORDER TO TAG PMPRJDTS IN (lc_PMPrjDt)
      =lfwBrwWhen(lc_PMPrjDt, !lVoid)
      IF EOF(lc_PMPrjDt)
        lcOprt_Ctg = SPACE(3)
        SHOW GET lcOprt_Ctg
      ENDIF  
      llShouldAdd = .F.
    ENDIF  
  ENDIF
  IF llBrowse .OR. llShouldAdd
    FOR lnI = 1 To 4
    IF laScrMode[lnI]
      lnOdSMode = lnI
      EXIT
      ENDIF
    ENDFOR
    lnCurAlias = SELECT(0)
    
    lcBasWnd = gcBaseWind  
        
    IF lfVldKey('PMCTGDT', 'PMCTGDT', @lcOprFlds, ;
                ALLTRIM(lcCtg_Dsc) + ' ' + 'Tasks',;
                'cOprt_ID', ;
                'Category\Task code ' + ALLTRIM(lcOprt_Ctg) + ;
                '\' + ALLTRIM(lcOprt_ID) + ;
                ' is not found in the categories of tasks file.',;
                lcOprt_Ctg + lcOprt_ID, [lcOprt_Ctg]);
        .AND. (laScrMode[2] .OR. lfExtPrg('E', lcOprt_Ctg, lcOprt_ID))

      STORE .F. To laScrMode
      laScrMode[lnOdSMode] = .T.
      lcBaseFile = 'PMPRJHD'  
      
      gcBaseWind = lcBasWnd 
      lcBaseWind  = gcBaseWind
            
      lcStamp =  PMPRJHD.cAdd_User+IIF(EMPTY(PMPRJHD.dAdd_Date),'',DTOC(PMPRJHD.dAdd_Date))+;
                     PMPRJHD.cAdd_Time
      
      lcCurrCtg = lcOprt_Ctg  
       
      SET ORDER TO TAG PMCTGDT IN PMCTGDT
      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID)),;
              lc_PMPrjDt)
        LOCATE REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID =;
                       SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjDt..cPrj_ID)) +;
                       SUBSTR(lcStyle,1,LEN(&lc_PMPrjDt..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjDt..cOprt_Ctg))+;
                       SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjDt..cOprt_ID));
                       FOR !lVoid 

        llFound = FOUND()
      ELSE
        llFound = .F.
      ENDIF           
      IF !llFound
        SELECT PMCTGDT
        IF SEEK(lcOprt_Ctg + lcOprt_ID)
          SCATTER MEMVAR MEMO
          STORE .T. TO llChngOprt, glUpdated
          llUpdStrtd = .F.
          lcNewSeqNum = lfGetSeqNum()
          
          INSERT INTO (lc_PMPrjDt); 
            ( cPrj_Typ , cPrj_ID  ,  cStyle,  ;
              cCtg_Seq , cOprt_Ctg, cOprt_Seq,;
              cOprt_ID , cOprt_Dsc, cOprt_Res,;
              lShw2Cust, mOprt_Com, cUpdtMthd, ;
              nest_dur , nrem_dur , mNotify, cStatus,cCal_ID,lOrginal,cGroup_id,;
              cAdd_User, dAdd_Date, cAdd_Time,cMComplt,lAddToAud);
            VALUES;
             (lcPrj_Typ, lcPrj_ID, lcStyle, ;
             lcNewSeqNum, m.cOprt_Ctg, m.cOprt_Seq, ;
             m.cOprt_ID, m.cOprt_Dsc, m.cOprt_Res,;
             m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd,;   
             m.nest_dur, m.nest_dur, m.mNotify, 'A',m.cCal_ID,.T.,m.cGroup_id,;
             gcUser_ID  , ldCurDate, TIME(),'YYY',.T.)   
          
          SET ORDER TO PMPRJDTS IN (lc_PMPrjDt)
          SELECT MAX(cOprt_Seq) ;
             FROM (lc_PMPrjDt);
             INTO ARRAY laTmpArry
          lcNOprSeq = IIF(_TALLY > 0, PADL(INT(VAL(laTmpArry))+1,  2, '0'), '01')
          SET ORDER TO PMPRJDT IN (lc_PMPrjDt)
          IF SEEK(lcPrj_Typ+ lcPrj_ID+ lcStyle+m.cOprt_Ctg+m.cOprt_ID,lc_PMPrjDt)
            SELECT (lc_PMPrjDt)
            REPLACE &lc_PMPrjDt..cOprt_Seq WITH lcNOprSeq 
          ENDIF
            
          glUpdated = .T.
          lnNumOfLin = lnNumOfLin + 1
          IF !EMPTY(laOpertion[1]) 
            DECLARE laOpertion[ALEN(laOpertion) + 1] 
          ENDIF
          laOpertion[ALEN(laOpertion)] = ;  
                                         lcOprt_Ctg + '\' + lcOprt_ID + ;
                                         ' ' +SUBSTR(m.cOprt_Dsc, 1, 18)
          IF ALEN(laOpertion) > 1 
            SHOW GET pbPredecsr ENABLE
          ENDIF 
          
          IF ASCAN(laCategrie, lcOprt_Ctg) = 0 
            IF !EMPTY(laCategrie[1])
              DIMENSION laCategrie[ALEN(laCategrie) + 1]
            ENDIF  
            laCategrie[ALEN(laCategrie)] = lcOprt_Ctg
          ENDIF
        ENDIF  
       
        IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
          SELECT PMCTGRL 
          SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID;
             FOR ASCAN(laOpertion, cOprt_Ctg + '\' + cPrd_ID ) > 0
            SCATTER MEMVAR MEMO
            INSERT INTO (lc_PMPrjRl) FROM MEMVAR
            SELECT (lc_PMPrjRl)
            REPLACE cPrj_Typ  WITH lcPrj_Typ,;
                    cPrj_ID   WITH lcPrj_ID,;
                    cStyle    WITH lcStyle,;
                    cPrd_Ctg  WITH cOprt_Ctg,;
                    cAdd_User WITH gcUser_ID,;
                    dAdd_Date WITH ldCurDate,;
                    cAdd_Time WITH TIME(),;
                    cStatus WITH 'A' 
          ENDSCAN           
        ENDIF    && ENDIF SE EK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
        
        *E302251,1 AMH Add Nofity Setting Records [Start]
        IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMPCTGNT')
          SELECT PMPCTGNT
          SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID
            SCATTER MEMVAR
            m.cPrj_Typ = lcPrj_Typ
            m.cPrj_ID  = lcPrj_ID
            m.cStyle   = lcStyle
            m.cStatus  = 'A'
            INSERT INTO (lcPMPRJNTF) FROM MEMVAR
          ENDSCAN
        ENDIF
        *E302251,1 AMH [End]
        
      ENDIF  
      SELECT (lc_PMPrjDt)       
      SET ORDER TO TAG PMPRJDTS
      STORE .F. To laScrMode
      laScrMode[lnOdSMode] = .T.

      *-- ReCalculate estimated dates if any change occurs.
      =lfGetEstDt(@lc_PMPrjDt, @lc_PMPrjRl,.F.)

      =lfwBrwWhen(lc_PMPrjDt, !lVoid)      
    ELSE
      STORE .F. To laScrMode
      laScrMode[lnOdSMode] = .T.

      SET ORDER TO TAG PMPRJDTS IN (lc_PMPrjDt)       
      lcOprt_ID  = SPACE(5)
      _CUROBJ    = _CUROBJ
    ENDIF  
  ENDIF   && ENDIF !llBrowse .AND. SEEK(lcOprt_Ctg + lcOprt_ID, lc_PMPrjDt)
ENDIF
SELECT (lnCurAlias) 


*!*************************************************************
*! Name      : lfvShw2Cus
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates Show to customer check box.(lcShw2Cust)
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvShw2Cus()
*!*************************************************************
FUNCTION lfvShw2Cus
REPLACE &lc_PMPrjDt..lShw2Cust WITH !&lc_PMPrjDt..lShw2Cust,;
        &lc_PMPrjDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMPrjDt..cStatus, 'SMA'), 1),;
        &lc_PMPrjDt..cAdd_User WITH gcUser_ID,;
        &lc_PMPrjDt..dAdd_Date WITH ldCurDate,;
        &lc_PMPrjDt..cAdd_Time WITH TIME()
lcShw2Cust = &lc_PMPrjDt..lShw2Cust
*--- SSH
SHOW GET lcShw2Cust
glUpdated = .T.

*!*************************************************************
*! Name      : lfvDetails
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates detail fields
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvDetails()
*!*************************************************************
FUNCTION lfvDetails
PRIVATE lcCurObj

lnCurAlias = SELECT(0)
lcCurObj = SYS(18)

lcSetExact = SET('EXACT')
SET EXACT ON

IF lcCurObj = 'LCOPRT_RES'
  DO CASE
    CASE PuUser = 'User'
      SELECT SYUUSER
      lcFile  = 'SYUUSER'
      lcBrFields = [cUser_Id : H = 'User ID', cUsr_Name : H = 'User Name']  
      lcFldName  = 'cUser_Id'
      lcField    = 'COPRT_RES'
    CASE PuUser = 'Group of users'
      SELECT SYUGROUP
      lcFile  = 'SYUGROUP'
      lcBrFields = [cGroup_Id : H = 'Group ID', cGroup_Nam : H = 'Group Name'] 
      lcFldName  = 'cGroup_Id'
      lcField    = 'cGroup_Id'
  ENDCASE  
  IF !EMPTY(LCOPRT_RES) AND !SEEK(ALLTRIM(LCOPRT_RES),lcFile)
    DIMENSION laTemp[1]
    IF ARIABROW('','Responsible User',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laTemp')
      LCOPRT_RES = laTemp[1]
    ELSE
      LCOPRT_RES = SPACE(10)
    ENDIF  
  ENDIF  
ELSE
  lcField    = lcCurObj
ENDIF

SET EXACT &lcSetExact

**IF LCOPRT_RES <> lcOldVal
IF lcCurObj <> lcOldVal
  lcCurObj = IIF(lcCurObj = 'LCOPRT_RES',lcCurObj,"m."+lcCurObj)
  SELECT (lc_PMPrjDt)
  IF lcCurObj = 'LCOPRT_RES'
    REPLACE COPRT_RES  WITH ' ',;
            CGROUP_ID  WITH ' ',;
            lAddToAud  WITH .T.
    llAddToAdt = .T.
  ENDIF            
  REPLACE &lcField   WITH IIF(lcCurObj = 'LCOPRT_RES',ALLTRIM(LCOPRT_RES),EVALUATE(lcCurObj)),;
          cStatus    WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User  WITH gcUser_ID,;
          dAdd_Date  WITH ldCurDate,;
          cAdd_Time  WITH TIME()
  glUpdated = .T.
  SHOW WINDOW (lcBrTtl) REFRESH SAME  
ENDIF

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvEstDur
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates estimated duration fields
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvEstDur()
*!*************************************************************
FUNCTION lfvEstDur
PRIVATE lcCurFld
IF lcOldVal <> nest_dur
  lnCurAlias = SELECT(0)
  SELECT (lc_PMPrjDt)
  IF nest_dur < 0 .AND. gfModalgen("TRM38223B00000","DIALOG") = 1 
    REPLACE nest_dur  WITH lcOldVal
  ELSE
    REPLACE nrem_dur  WITH nest_dur,;
            cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
    STORE .T. TO llChngOprt, glUpdated
    llUpdStrtd = .F.
    SHOW GET nrem_dur
    SHOW WINDOW (lcBrTtl) REFRESH SAME  
  ENDIF  
  SELECT (lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvDurtion
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates duration fields
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvDurtion()
*!*************************************************************
FUNCTION lfvDurtion
PRIVATE lcCurFld

lnCurAlias = SELECT(0)
lcCurFld = SYS(18)

lcPredOpr = &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
IF lcOldVal <> EVALUATE(lcCurFld)
  llAddtoAdt = .T.
  SELECT (lc_PMPrjDt)

  IF lcCurFld = 'NACT_DUR'
    SELECT (lc_PMPrjDt)
    lnRecNo = RECNO()
    SCAN FOR lcPredec $ (cOprt_Ctg + '\' + cOprt_ID)
      IF EMPTY(dAct_Strt)
        =gfModalgen("INM38245B00000","DIALOG")
        IF BETWEEN(lnRecNo, 1, RECCOUNT())
          GO lnRecNo
        ENDIF
        REPLACE &lcCurFld WITH 0
        =lfRefresh()
        SHOW WINDOW (lcBrTtl) REFRESH SAME  
        SELECT (lnCurAlias)
        RETURN
      ENDIF
    ENDSCAN
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ENDIF
  ENDIF
         
  IF EVALUATE(lcCurFld) < 0 .AND. gfModalgen("TRM38223B00000","DIALOG") = 1 
    REPLACE &lcCurFld  WITH lcOldVal
  ELSE
    REPLACE &lcCurFld WITH EVALUATE(lcCurFld),;
            cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
            
    glUpdated = .T.
    IF 'DUR' $ SYS(18) .AND. EVALUATE(SYS(18)) <> lcOldVal
      llChngOprt = .T.
      llUpdStrtd = .F.
    ENDIF

    IF lcCurFld = 'NACT_DUR'
      IF EMPTY(dAct_Strt)
        SELECT (lc_PMPrjDt)
        REPLACE &lc_PMPrjDt..dAct_Strt WITH IIF(EMPTY(&lc_PMPrjDt..dClc_Strt),;
                                                &lc_PMPrjDt..dEst_Strt,&lc_PMPrjDt..dClc_Strt)
      ENDIF                

      SELECT (lc_PMPrjDt)                              
      lnWEnd = 0
      lnDur  = &lc_PMPrjDt..nAct_Dur
      =lfGetHlidy()                 
      
      *E301869,2 AMH Select temp table before replace to replace without using macro [Start]
      *REPLACE &lc_PMPrjDt..dAct_Fnsh WITH (&lc_PMPrjDt..dAct_Strt + (&lc_PMPrjDt..nAct_Dur - 1) + lnWEnd),;
              &lc_PMPrjDt..cMComplt  WITH 'YYY',;
              &lc_PMPrjDt..lAddToAud WITH .T.
      SELECT(lc_PMPrjDt)
      REPLACE dAct_Fnsh WITH (dAct_Strt + (nAct_Dur - 1) + lnWEnd),;
              nRem_Dur  WITH 0,;
              cMComplt  WITH 'YYY',;
              lAddToAud WITH .T.
      *E301869,2 AMH [End]
      
      =lfComTask()
    ELSE
      SELECT (lc_PMPrjDt)
      REPLACE &lc_PMPrjDt..cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
    ENDIF  
    SELECT (lc_PMPrjDt)
    SET ORDER TO PMPRJDT
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
          SUBSTR(lcStyle,1,LEN(cStyle)) + lcPredOpr)
  ENDIF  
  SELECT (lnCurAlias)
ENDIF

*E301869,2 AMH Refresh objects [Start]
=lfwBrwWhen(lc_PMPrjDt, m.cPrj_Stts $ 'PI' .AND. !lVoid)
*E301869,2 AMH [End]

*!*************************************************************
*! Name      : lfvPredecsr
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Predecessors >
*!             (pbPredecsr)
*!*************************************************************
*! Calls              : gfMover
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPredecesrs()
*!*************************************************************
FUNCTION lfvPredecsr
PRIVATE laPredOprs, lcOldPred, lnCurAlias 

lnCurAlias = SELECT(0)
lcSetProc = SET('PROCEDURE')
SET PROCEDURE TO
=ACOPY(laOpertion, laPredOprs)
lnElement = ASCAN(laPredOprs, lcOprt_Ctg + '\' + lcOprt_ID)

IF lnElement > 0
  IF ADEL(laPredOprs, lnElement) > 0
    IF ALEN(laPredOprs) > 1
      DIMENSION laPredOprs[ALEN(laPredOprs)-1]
    ELSE
      DIMENSION laPredOprs[1]
      laPredOprs = ''
    ENDIF  
  ENDIF
ENDIF   

IF laScrMode [2] 
  SELECT PMPRJRL
  SET ORDER TO PMPRJRL IN PMPRJRL
  SET ORDER TO PMPRJDT IN PMPRJDT 
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(PMPRJRL.cOprt_Ctg))+;
          SUBSTR(lcOprt_ID,1,LEN(PMPRJRL.cOprt_ID)))
    lnI = 0          
    SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
            SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID)) +;
            SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(PMPRJRL.cOprt_Ctg))+;
            SUBSTR(lcOprt_ID,1,LEN(PMPRJRL.cOprt_ID))
      lnI = lnI + 1          
      DIMENSION laPredecsr[lnI],laPred[lnI]
      laPredecsr[lnI] = PMPRJRL.cPrd_Ctg + '\' + PMPRJRL.cPrd_ID 
      laPred[lnI]     = PMPRJRL.cPrd_Ctg + PMPRJRL.cPrd_ID 
    ENDSCAN                   
    FOR lnI = 1 TO ALEN(laPred)
      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+laPred[lnI],'PMPRJDT')
        laPredecsr[lnI] = laPredecsr[lnI] + ' ' + SUBSTR(PMPRJDT.cOprt_Dsc, 1, 18)         
      ENDIF                          
    ENDFOR
  ELSE
    DIMENSION laPredecsr[1]
    laPredecsr = ''
  ENDIF    
  SET ORDER TO PMPRJDTS IN PMPRJDT 

  *SELECT cPrd_Ctg + '\' + cPrd_ID +  ' ' + SUBSTR(cOprt_Dsc, 1, 18);
    FROM PMPRJRL, PMPRJDT;
    WHERE PMPRJRL.cPrj_Typ  + PMPRJRL.cPrj_ID + PMPRJRL.cStyle +;
          PMPRJRL.cOprt_Ctg + PMPRJRL.cOprt_ID = ;
          SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(PMPRJRL.cOprt_Ctg))+;
          SUBSTR(lcOprt_ID,1,LEN(PMPRJRL.cOprt_ID));
    .AND. PMPRJRL.cPrj_Typ  + PMPRJRL.cPrj_ID + PMPRJRL.cStyle +;
          PMPRJRL.cPrd_Ctg  + PMPRJRL.cPrd_ID = ;    
          PMPRJDT.cPrj_Typ  + PMPRJDT.cPrj_ID + PMPRJDT.cStyle +;
          PMPRJDT.cOprt_Ctg + PMPRJDT.cOprt_ID ;
    INTO ARRAY laPredecsr     

  
  *IF _TALLY = 0
  *  DIMENSION laPredecsr[1]
  *  laPredecsr = ''
  *ENDIF
  
  =gfMover(@laPredOprs,@laPredecsr,;
           ALLTRIM(lcOprt_Ctg) + '\' + ;
           ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(PMPRJDT.coprt_dsc) +;
           ' Predecessors')
ELSE
  SELECT (lc_PMPrjRl)
  SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
  SET ORDER TO PMPRJDT IN (lc_PMPrjDt)
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjRl..cOprt_Ctg))+;
          SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjRl..cOprt_ID)))
    lnI = 0          
    SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
              SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(&lc_PMPrjRl..cOprt_Ctg))+;
              SUBSTR(lcOprt_ID,1,LEN(&lc_PMPrjRl..cOprt_ID))

      lnI = lnI + 1          
      DIMENSION laPredecsr[lnI],laPred[lnI]
      laPredecsr[lnI] = &lc_PMPrjRl..cPrd_Ctg + '\' + &lc_PMPrjRl..cPrd_ID 
      laPred[lnI]     = &lc_PMPrjRl..cPrd_Ctg + &lc_PMPrjRl..cPrd_ID 
    ENDSCAN                   
    FOR lnI = 1 TO ALEN(laPred)
      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(&lc_PMPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lc_PMPrjRl..cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+laPred[lnI],lc_PMPrjDt)
        laPredecsr[lnI] = laPredecsr[lnI] + ' ' + SUBSTR(&lc_PMPrjDt..cOprt_Dsc, 1, 18)         
      ENDIF                          
    ENDFOR
  ELSE
    DIMENSION laPredecsr[1]
    laPredecsr = ''
  ENDIF    
  SET ORDER TO PMPRJDTS IN (lc_PMPrjDt)

  DIMENSION laPathElems[1]
  laPathElems = ''
  SELECT (lc_PMPrjDt)
  SET RELATION OFF INTO (lc_PMPrjRl)

  SELECT (lc_PMPrjRl)
  SET ORDER TO TAG PMPRJRLP 
  lnCount = 0  
  
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
          SUBSTR(lcOprt_ID,1,LEN(cOprt_ID)))
    SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg + cPrd_ID = ;
                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
      lnCount = lnCount + 1  
      DIMENSION laPathElems[lnCount]
      laPathElems[lnCount] = cOprt_Ctg +  cOprt_ID
    ENDSCAN
 
    lnNxtCount = 0
    DO WHILE lnNxtCount < ALEN(laPathElems)
      lnNxtCount = lnNxtCount + 1 
      lcNxtOprt  = laPathElems[lnNxtCount]
 
      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(cStyle))+ lcNxtOprt)
        SCAN REST WHILE cPrj_Typ +  cPrj_ID +  cStyle +  cPrd_Ctg + cPrd_ID = ;
                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                       SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
          DIMENSION laPathElems[ALEN(laPathElems) + 1]
          laPathElems[ALEN(laPathElems)] = cOprt_Ctg + cOprt_ID
        ENDSCAN
      ENDIF     &&ENDIF SEEK(lcNxtOprt)
    ENDDO    &&ENDDO WHILE lnNxtCount < ALEN(laPathElems)
  ENDIF     &&ENDIF SEEK(lcOprt_ID)  
  lcOldPred = lcPredec 
  lcVldPrd = 'lfVldPrd(SUBSTR(IIF(lsSource = 0,laSource[1],laSource[lsSource]), 1, 3),;
                       SUBSTR(IIF(lsSource = 0,laSource[1],laSource[lsSource]), 5, 5),;
                      _CUROBJ = OBJNUM(pbAll))'  
  IF m.cPrj_Stts $ 'PI'
    =gfMover(@laPredOprs,@laPredecsr,;
             'Select Predecessors for '+ ALLTRIM(lcOprt_Ctg) + '\' + ;
             ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(&lc_PMPrjDt..coprt_dsc),;
            .T.,@lcVldPrd,.T.,.T.)
    SELECT (lc_PMPrjRl)
    SET ORDER TO TAG PMPRJRL
    SELECT (lc_PMPrjDt)
    SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID;
               INTO (lc_PMPrjRl)
    SELECT (lc_PMPrjRl)
    IF !EMPTY(laPredecsr[1]) 
      lnDelElems = ALEN(laPredecsr)
      FOR lnCount = 1 TO lnDelElems
        laPredecsr[lnCount] = SUBSTR(laPredecsr[lnCount], 1, 3) + ;
                              SUBSTR(laPredecsr[lnCount], 5, 5) 
      ENDFOR  

      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
              SUBSTR(lcOprt_ID,1,LEN(cOprt_ID)))
        SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))

          lnelement  = ASCAN(laPredecsr, cPrd_ID)
          IF lnelement > 0 
            =ADEL(laPredecsr, lnElement)
            lnDelElems = lnDelElems - 1
          ELSE
            REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
          ENDIF
        ENDSCAN
        DELETE FOR cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
        FOR lnCount = 1 TO lnDelElems
          INSERT INTO (lc_PMPrjRl);
                     (cPrj_Typ , cPrj_ID  , cOprt_ID , cPrd_ID,;
                      cOprt_Ctg,  cPrd_Ctg, ;
                      cStyle  , cAdd_User, dAdd_Date, cAdd_Time, cStatus);
               VALUES(lcPrj_Typ, lcPrj_ID , lcOprt_ID, SUBSTR(laPredecsr[lnCount], 4, 5),;
                      lcOprt_Ctg, SUBSTR(laPredecsr[lnCount], 1, 3),;
                      lcStyle , gcUser_ID  , ldCurDate, TIME()   , 'A')
        ENDFOR
      ELSE
        FOR lnCount = 1 TO ALEN(laPredecsr)
          INSERT INTO (lc_PMPrjRl);
                     (cPrj_Typ , cPrj_ID  , cOprt_ID , cPrd_ID,;
                      cOprt_Ctg,  cPrd_Ctg, ;
                      cStyle  , cAdd_User, dAdd_Date, cAdd_Time, cStatus);
               VALUES(lcPrj_Typ, lcPrj_ID , lcOprt_ID, SUBSTR(laPredecsr[lnCount], 4, 5),;
                      lcOprt_Ctg, SUBSTR(laPredecsr[lnCount], 1, 3),;
                      lcStyle , gcUser_ID  , ldCurDate, TIME()   , 'A')
        ENDFOR  
      ENDIF
    ELSE
      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
              SUBSTR(lcOprt_ID,1,LEN(cOprt_ID)))
        SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
          REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
          
        ENDSCAN
        DELETE FOR cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle))+SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg))+;
                     SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
      ENDIF  
    ENDIF  

    lcOldVal   = lcPredec
    lcPredec   = lfGetPred()
    IF lcOldVal <> lcPredec
      STORE .T. TO glUpdated, llChngOprt
      llUpdStrtd = .F.
      SHOW GET lcPredec
    ENDIF
  ELSE
    =gfMover(@laPredOprs,@laPredecsr,;
             ALLTRIM(lcOprt_Ctg) + '\' + ;
             ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(&lc_PMPrjDt..coprt_dsc) +;
             ' Predecessors')
  ENDIF
ENDIF  

SET PROCEDURE TO &lcSetProc
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetPred
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Gets predecessors of an operation in a string.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  A string of predecessors or  ''.
*!*************************************************************
*! Example            :  lfGetPred()
*!*************************************************************
FUNCTION lfGetPred
PRIVATE lcPrdStrng, lnCurAlias 

lnCurAlias = SELECT(0)
lcPrdStrng = ''
IF laScrMode[2] 
  SELECT PMPRJDT
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF
  SELECT PMPRJRL
ELSE
  SELECT (lc_PMPrjDt)
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF
  SELECT (lc_PMPrjRl)
ENDIF    
=SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
               SUBSTR(lcStyle,1,LEN(cStyle)) + SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg)) +;
               SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))) 
SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID = ;
               SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
               SUBSTR(lcStyle,1,LEN(cStyle)) + SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg)) +;
               SUBSTR(lcOprt_ID,1,LEN(cOprt_ID))
  lcPrdStrng = lcPrdStrng + ALLTRIM(cPrd_Ctg) + '\' + ALLTRIM(cPrd_ID) +', '
ENDSCAN

SELECT (lnCurAlias)
RETURN IIF(!EMPTY(lcPrdStrng),;
           SUBSTR(lcPrdStrng, 1, RAT(',', lcPrdStrng)-1) + '.', SPACE(40)) 
           
           
*!*************************************************************
*! Name      : lfvUpdMthd
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Update Method popup valid function.
*!*************************************************************
*! Calls              : gfActPop()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvUpdMthd()
*!*************************************************************
FUNCTION lfvUpdMthd

lcUpdtMthd = puUpdtMthd

REPLACE &lc_PMPrjDt..cUpdtMthd WITH lcUpdtMthd,;
        &lc_PMPrjDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMPrjDt..cStatus, 'SMA'), 1),;
        &lc_PMPrjDt..cAdd_User WITH gcUser_ID,;
        &lc_PMPrjDt..dAdd_Date WITH ldCurDate,;
        &lc_PMPrjDt..cAdd_Time WITH TIME()
glUpdated   = .T.

*!*************************************************************
*! Name      : lfvNotify
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Notify... >
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotify()
*!*************************************************************
FUNCTION lfvNotify
IF ! lcNotify == lcOldVal
  lnCurAlias = SELECT(0)
  SELECT (lc_PMPrjDt)
  REPLACE mNotify WITH IIF(!EMPTY(laUsers[1]), ;
                       STRTRAN(ALLTRIM(lcNotify), ',','|'),;
                       ALLTRIM(lcNotify)),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()
  SELECT (lnCurAlias)
  glUpdated   = .T.
ENDIF


*!*************************************************************
*! Name      : lfvNotifyBt
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Notify... >
*!*************************************************************
*! Calls              : gfMover
*!                      gfStr2Ar
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotifyBt()
*!*************************************************************
FUNCTION lfvNotifyBt

lcSetProc = SET('PROCEDURE')
SET PROCEDURE TO

lnCurAlias = SELECT(0)

*E302251,1 AMH Calling the Notify Seting screen [Start]
*SELECT (lc_PMPrjDt)
*
*=gfStr2Ar(ALLTRIM(mNotify), @laNotified, '|')
*
*DIME laUsers[1]
*SELECT cUser_ID ;
*  FROM (gcSysHome + 'SYUUSER');
*  INTO ARRAY laUsers;
*  ORDER BY cUser_ID
*
*IF laScrMode[2] .OR. m.cPrj_Stts $ 'CX' .OR. lVoid 
*  =gfMover(@laUsers,@laNotified,"Users to Notify")
*ELSE
*  =gfMover(@laUsers,@laNotified,"Select Users to Notify",.T.)
*  lcNotify   = ''
*  FOR lnCount = 1 TO ALEN(laNotified)
*    lcNotify = lcNotify + ALLTRIM(laNotified[lnCount]) + '|'
*  ENDFOR
*  IF !EMPTY(lcNotify)
*    lcNotify = SUBSTR(lcNotify, 1, RAT('|', lcNotify)-1) 
*  ENDIF
*  REPLACE mNotify WITH ALLTRIM(lcNotify),;
*          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
*          cAdd_User WITH gcUser_ID,;
*          dAdd_Date WITH ldCurDate,;
*          cAdd_Time WITH TIME()
*
*  lcNotify   = IIF(!EMPTY(lcNotify), STRTRAN(lcNotify, '|', ', ') + '.', '')
*  glUpdated  = .T.
*  SHOW GET lcNotify
*ENDIF

SELECT (lcPMPRJNTF)
SET KEY TO PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id
LOCATE
lnBrRecNo = RECNO()
SCATTER MEMVAR

PUSH KEY
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrUser)
ON KEY LABEL ESC DO lfEsc
DO (gcScrDir + 'SM\SMNTFSET.SPX')
POP KEY

IF !laScrMode[2]
  lcNotify   = ''
  SELECT (lcPMPRJNTF)
  SCAN FOR !EMPTY(cUser_ID)
    lcNotify = lcNotify + ALLTRIM(cUser_ID) + '|'
  ENDSCAN
  IF !EMPTY(lcNotify)
    lcNotify = SUBSTR(lcNotify, 1, RAT('|', lcNotify)-1) 
  ENDIF
  SELECT (lc_PMPRJDt)
  REPLACE mNotify   WITH ALLTRIM(lcNotify),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()

  lcNotify   = IIF(!EMPTY(lcNotify), STRTRAN(lcNotify, '|', ', ') + '.', '')
  glUpdated  = .T.
  SHOW GET lcNotify
ENDIF

SELECT (lcPMPRJNTF)
SET KEY TO
*E302251,1 AMH [End]

SET PROCEDURE TO &lcSetProc
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvNotes
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Notes... >
*!*************************************************************
*! Calls              : =gfZoom()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotes()
*!*************************************************************
FUNCTION lfvNotes
lnCurAlias = SELECT(0)

IF laScrMode[2]
  SELECT PMPRJDT
  =gfZoom('mOprt_Com')
ELSE
  SELECT (lc_PMPrjDt)
  =gfZoom('mOprt_Com')
  REPLACE cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()
  glUpdated = .T.
  SELECT (lnCurAlias)
ENDIF  


*!*************************************************************
*! Name      : lfvSchedul
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Schedule... >
*!*************************************************************
*! Calls              : MFSCHD.SPR
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvSchedul()
*!*************************************************************
FUNCTION lfvSchedul

*-- Default schedule date with todays date
dSch_Date = IIF(EMPTY(m.dClc_strt),m.dEst_strt,gdSysDate)

llSchedule = .F.
PUSH KEY
ON KEY LABEL ESC T=0
DO (gcScrDir+'MFSCHD.SPR')
ON KEY LABEL ESC DO  lpEscScr
POP KEY

IF llSchedule
  =lfSchedule()
  *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
  *STORE "" TO lcProg , lcKey , lcEvent
  STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
  =lfUpdVar()
  *[End]
  IF !USED('AUDTRAIL')
    =gfOpenFile(gcDataDir + 'AUDTRAIL' , 'AUDTRAIL' , 'SH')
  ENDIF 
  SELECT PMPRJDT
  lcTmpDtFl = 'PMPRJDT'
  SET ORDER TO PMPRJDT IN PMPRJDT
  =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ))+SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+SUBSTR(lcStyle,1,LEN(cStyle)))
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ))+;
                  SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle));
             FOR lSchAdToAd 
    =lfPrpAudt()
    REPLACE lSchAdToAd WITH .F.
  ENDSCAN
ENDIF

lcBrfields  = lcOldBrFild

*!*************************************************************
*! Name      : lfSchedule
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Schedule... >
*!*************************************************************
*! Calls              : MFSCHD.SPR
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfSchedule()
*!*************************************************************
FUNCTION lfSchedule
PARAMETERS lcProjType,lcProjID,lcPStyle,ldSchDate,lcSDir,lcTempName,llMSchdul

llFromWeb = .F.
IF TYPE('lcProjType') = 'C' AND TYPE('lcProjID') = 'C' AND TYPE('lcPStyle') = 'C' AND;
   TYPE('ldSchDate')  = 'D' AND TYPE('lcSDir')  = 'C'
  llFromWeb = .T.
  lcPrj_Typ = lcProjType
  lcPrj_ID  = lcProjID
  lcStyle   = lcPStyle
  dSch_Date = ldSchDate
ENDIF

IF llFromWeb 
  IF !USED('SYSCHDUL')
    USE lcSDir+'SYSCHDUL.DBF" SHARED IN 0
    SET ORDER TO COPRUSR
  ENDIF  

  =SEEK(lcProjType+lcProjID+lcPStyle,'PMPRJHD')
 
  DECLARE laHolidays[1]
  laHolidays = ''
  lnRows  = 0

  SELECT PMCALDT
  SCAN
    ldCal_HFrm = dCal_HFrm
    DO WHILE ldCal_HFrm <= dCal_HTo
      lnRows  = lnRows  + 1
      DIMENSION laHolidays[lnRows]
      laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
      ldCal_HFrm = ldCal_HFrm + 1
    ENDDO  
  ENDSCAN

ENDIF

*-- Check if the current record is being edited by another user.

IF PMPRJHD.llok_stat AND PMPRJHD.cLok_User <> gcUser_ID
  =gfModalgen("TRM00029B38030","DIALOG")
ELSE            
  SELECT PMPRJHD    
  *-- Proceed with scheduling if locking is successful.
  IF RLOCK()
    REPLACE llok_stat WITH .T.
    UNLOCK      
    *-- Update data date.
    STORE dSch_Date TO m.dDta_Date
    STORE .F. TO m.lSchedual, m.llok_stat
    m.cPrj_Stts = 'I'
    STORE 'In Progress' TO lcCurStage 
    
    IF !llMSchdul
      puCurStage  = 2
      SHOW GET puCurStage  
    ENDIF  
    *-- Add audit information ?
    =lfGetClcDt()  
    IF !llMSchdul
      SELECT PMPRJHD    
      GATHER MEMVAR MEMO
    ENDIF
    IF !llFromWeb     
      SHOW GETS
    ENDIF  
  ENDIF  
ENDIF  


*!*************************************************************
*! Name      : lfvOK
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button <  Ok > in 
*!             MFSCHD.SPR
*!*************************************************************
*! Called from        : MFSCHD.SPR
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOK()
*!*************************************************************
FUNCTION lfvOK
*-- If the schedule date is empty, do not accept the entry
*-- and return to the date field, otherwise, clear read and exit.
IF EMPTY(dSch_Date)
  =gfModalgen("TRM38224B00000","DIALOG")
  _CUROBJ = OBJNUM(dSch_Date)
ELSE
  llSchedule = .T.
  CLEAR READ
ENDIF   

*!*************************************************************
*! Name      : lfvEstDate
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for estimated start date.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvEstDate()
*!*************************************************************
FUNCTION lfvEstDate

IF m.dest_strt <> lcOldVal
  IF !EMPTY(m.dest_strt)
    IF lnNumOfLin > 0
      =lfGetEstDt(@lc_PMPrjDt, @lc_PMPrjRl,.F.)
      IF llFromFld
        =lfRefresh()
      ENDIF  
    ENDIF  
  ELSE
    m.dest_strt = lcOldVal
  ENDIF
ENDIF

lcdeststrt = m.dest_strt

GO TOP IN (lc_PmPrjRl)
lcCtgOprt = &lc_PmPrjRl..cPrd_ctg+&lc_PmPrjRl..cPrd_Id
lcPrvOrd = ORDER(lc_PmPrjDt)
*B127792,1 HBG 07/05/2005 Seek using the new index to get the correct start date of the project [Begin]
*SET ORDER TO PMPRJDT IN (lc_PmPrjDt)
*IF SEEK(lcPrj_typ+m.cPrj_Id+SUBSTR(m.cStyle,1,LEN(&lc_PmPrjDt..cStyle))+lcCtgOprt,lc_PmPrjDt)
SET ORDER TO (lc_PmPrjDt) IN (lc_PmPrjDt)
IF SEEK(lcPrj_typ+m.cPrj_Id+SUBSTR(m.cStyle,1,LEN(&lc_PmPrjDt..cStyle)),lc_PmPrjDt)
*B127792,1 [End]
  m.dEst_strt = &lc_PmPrjDt..dEst_strt
ENDIF  
SET ORDER TO &lcPrvOrd IN (lc_PmPrjDt)

  
*!*************************************************************
*! Name      : lfGetEstDt
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Calculates estimated dates for all operations
*!             as well as the estimated finish date for the 
*!             project based on the estimated start date.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : lcPrjDetFl : Details file name
*!                      lcPrjRelFl : Predecessors file name 
*!                      llFrmAcept : .T. if updating master files.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetEstDt()
*!*************************************************************
FUNCTION lfGetEstDt
PARAMETERS lcPrjDetFl, lcPrjRelFl, llFrmAcept

PRIVATE lnCurAlias, lnDetTag
*-- Save the project start date to a variable so as to be used
*-- ldest_fnsh
ldEst_Fnsh = m.dEst_Strt
lnCurAlias = SELECT(0)

*-- Change the tag of Project relations file to predecessors,
SET ORDER TO TAG PMPRJRLP IN (lcPrjRelFl)

*-- Store the current tag, and change tag to PMPRJDTS
SELECT (lcPrjDetFl)
lnDetTag = VAL(SYS(21))
SET ORDER TO TAG PMPRJDT

SELECT (lc_Parser)
*-- Add category code field
SET RELATION TO SUBSTR(lcPrj_Typ,1,LEN(&lcPrjDetFl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcPrjDetFl..cPrj_ID)) +;
                SUBSTR(lcStyle,1,LEN(&lcPrjDetFl..cStyle))+ cOprt_Ctg + cOprt_ID INTO (lcPrjDetFl)

SELECT (lcPrjDetFl)
SET RELATION OFF INTO (lcPrjRelFl)

IF !llFrmAcept
  REPLACE ALL dest_strt WITH {},;
              dest_fnsh WITH {},;
              cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)  
ENDIF               
SELECT (lcPrjDetFl)
SET ORDER TO PMPRJDT IN (lcPrjDetFl)
SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) + SUBSTR(lcStyle,1,LEN(cStyle))
SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = ;
          SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(cStyle)) ;
          FOR EMPTY(dest_strt)
  lcCurOprt = cOprt_Ctg + cOprt_ID

  
  *-- Update dates, and get the new finish date.
  SELECT (lcPrjDetFl)
  ldAdjEstFn = {}
  REPLACE dest_strt WITH lfAdjDate(m.dest_strt, @ldAdjEstFn,;
                                   MAX(0, nest_dur - 1), cCal_ID, '+'),;
          dest_fnsh WITH ldAdjEstFn         

          
  ldest_fnsh = dest_fnsh
  lnDurIndic = MIN(nest_dur,1)
  
  *-- if the new operation is a predecessor to other operations,
  *-- copy their IDs to the cursor.
  SELECT (lcPrjRelFl)  
  SET ORDER TO PMPRJDT IN (lcPrjDetFl)
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    SCAN REST WHILE cPrj_Typ +  cPrj_ID + cStyle  +  cPrd_Ctg + cPrd_ID = ;
                    SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                    SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
      lcNxtOprt = cOprt_Ctg + cOprt_ID
      INSERT INTO (lc_Parser);
         VALUES(&lcPrjRelFl..cOprt_Ctg, &lcPrjRelFl..cOprt_ID,;
                ldest_fnsh, lnDurIndic)

    ENDSCAN
  ENDIF        

  SELECT (lc_Parser)
  GO TOP
  DO WHILE !EOF() 
    lcNxtOprt  = cOprt_Ctg + cOprt_ID 
    lnRecNo    = RECNO()
    ldest_fnsh = dStrtDate
    lnDurIndic = nDurIndic
    SELECT (lcPrjDetFl)
    IF dest_strt <= ldest_fnsh 
      ldAdjEstFn = {}
      REPLACE dest_strt WITH lfAdjDate( IIF(nest_dur = 0, ldest_fnsh,;
                             ldest_fnsh + lnDurIndic),;
                             @ldAdjEstFn, MAX(0, nest_dur - 1), cCal_ID, '+'),;
              dest_fnsh WITH ldAdjEstFn


    ENDIF

    ldest_fnsh = dest_fnsh
    lnDurIndic = MAX(lnDurIndic, MIN(nest_dur,1))
    SELECT (lcPrjRelFl)  
    SET ORDER TO PMPRJDT IN (lcPrjDetFl)
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
            SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
     SCAN REST WHILE  cPrj_Typ +  cPrj_ID +  cStyle + cPrd_Ctg + cPrd_ID = ;
                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
       lcNxtOnPth = cOprt_Ctg + cOprt_ID
       INSERT INTO (lc_Parser);
        VALUES(&lcPrjRelFl..cOprt_Ctg, &lcPrjRelFl..cOprt_ID,;
              ldest_fnsh, lnDurIndic)

      ENDSCAN
    ENDIF 
    SELECT (lc_Parser)
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ENDIF

  SKIP
  ENDDO
  
  SELECT (lc_Parser)
  ZAP   
  
  SELECT (lcPrjDetFl)
  SET ORDER TO PMPRJDT IN (lcPrjDetFl)
  SEEK SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
       SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
ENDSCAN

*-- After all operations are scanned, get the project estimated finish date.
*-- The project estimated finish date is the greatest estimated finish
*-- date of all operations/
DECLARE laTmpVal[1]
SELECT MAX(dest_fnsh) ;
  FROM (lcPrjDetFl)   ;
  WHERE cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
        SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
        SUBSTR(lcStyle,1,LEN(cStyle));
  INTO ARRAY laTmpVal
IF !EMPTY(laTmpVal[1])  
  m.dest_fnsh = laTmpVal[1]
ENDIF

SET ORDER TO TAG PMPRJRL IN (lcPrjRelFl)
SELECT (lcPrjDetFl)
SET ORDER TO (lnDetTag)
SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID ;
           INTO (lcPrjRelFl)
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetClcDt
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Calculates calculated dates for all operations
*!             as well as the calculated finish date for the 
*!             project based on the calculated start date.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetClcDt()
*!*************************************************************
FUNCTION lfGetClcDt
PRIVATE lnCurAlias 

lnCurAlias = SELECT(0)

lcTmpPrjDt = IIF(llMSchdul,lc_PmPrjDt,'PMPRJDT')
lcTmpPrjRl = IIF(llMSchdul,lc_PmPrjRl,'PMPRJRL') 

*-- Change the tag of Project relations file to predecessors,
SET ORDER TO TAG PMPRJRLP IN (lcTmpPrjRl)
lnCurTag = VAL(SYS(21))
*-- Reset order tag

SELECT (lcTmpPrjDt)
SET ORDER TO TAG PMPRJDT

IF llFromWeb 
  lc_Parser = lcTempName
  CREATE CURSOR (lc_Parser);
         (cOprt_Ctg C(3), cOprt_ID C(5), dStrtDate D(8), nDurIndic N(1)) 
ENDIF

SELECT (lc_Parser)
*-- Add category code field
SET RELATION TO SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
                SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle))+ cOprt_Ctg + cOprt_ID INTO (lcTmpPrjDt)

           
*-- Scan all operations of the current project
SELECT (lcTmpPrjDt)

SET RELATION OFF INTO PMPRJRL
*-- Add category code field
SET FILTER TO  cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
              SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjRl..cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(&lcTmpPrjRl..cStyle));
              .AND. !lVoid


*--Clear all calculated dates fields
REPLACE ALL dclc_strt WITH {},;
            dclc_fnsh WITH {}

SCAN FOR EMPTY(dclc_strt)
  *-- Save the current operation into a variable.
  lcCurOprt = cOprt_Ctg + cOprt_ID

  
  *-- Update dates, and get the new finish date.
  SELECT (lcTmpPrjDt)

  *-- For the first operation, use the scheduling date as a 
  *-- candidate calculated start date 
  =lfUpdClcDt(dSch_Date)
  lldclc_fsh = dclc_fnsh
  lnDurIndic = MIN(nest_dur,1)
  
  *-- if the new operation is a predecessor to other operations,
  *-- copy their IDs to the cursor.
  SELECT (lcTmpPrjRl)
 
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    SCAN REST WHILE cPrj_Typ +  cPrj_ID + cStyle  +  cPrd_Ctg + cPrd_ID = ;
                   SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                   SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
      lcNxtOprt = cOprt_Ctg + cOprt_ID
      INSERT INTO (lc_Parser);
         VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)
    ENDSCAN  
  ENDIF        

  SELECT (lc_Parser)
  GO TOP
  IF EOF()
    SELECT (lcTmpPrjDt)
    SET ORDER TO PMPRJDT IN (lcTmpPrjDt)
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
        SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)           
      SELECT SYSCHDUL
      SET ORDER TO Coprusr
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
      *IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcCurOprt)
      IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt)
      *B130984,1 HBG 12/04/2006 [End]
        IF SYSCHDUL.COPERSTAT = 'C'
          lcStauts = SYSCHDUL.COPERSTAT
          *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
          *LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
          *            SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcCurOprt;
          *            FOR COPERSTAT <> lcStauts
          LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
                      SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt;
                      FOR COPERSTAT <> lcStauts
          *B130984,1 HBG 12/04/2006 [End]
        ENDIF      
        REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
                dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),;
                lpredcomp  WITH .T.
      ENDIF            
    ENDIF
  ENDIF
  SELECT (lc_Parser)
  DO WHILE !EOF() 
    *-- Update the estimated date of the current detail record
    *-- Add category code field
    lcNxtOprt  = cOprt_Ctg + cOprt_ID 
    lnRecNo    = RECNO()
    
    SELECT (lcTmpPrjDt)

    =lfUpdClcDt(IIF(nest_dur = 0, &lc_Parser..dStrtDate,;
                      &lc_Parser..dStrtDate + &lc_Parser..nDurIndic))
    lldclc_fsh = dclc_fnsh
    
    lnDurIndic = MAX(lnDurIndic, MIN(nest_dur,1))
    SELECT (lcTmpPrjRl)

    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
            SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
      SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg + cPrd_ID = ;
                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
        lcNxtOnPth = cOprt_Ctg + cOprt_ID
        INSERT INTO (lc_Parser);
           VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)
      ENDSCAN
    ENDIF 
    
    IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)           
      SELECT SYSCHDUL
      SET ORDER TO Coprusr
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
      *IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcNxtOprt)
      IF SEEK(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt)
      *B130984,1 HBG 12/04/2006 [End]
        IF SYSCHDUL.COPERSTAT = 'C'
          lcStauts = SYSCHDUL.COPERSTAT
          *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
          *LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
          *            SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + lcNxtOprt ;
          *            FOR COPERSTAT <> lcStauts
          LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
                      SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt ;
                      FOR COPERSTAT <> lcStauts
          *B130984,1 HBG 12/04/2006 [End]
        ENDIF      
        REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
                dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)
      ENDIF            
    ENDIF
           
    
    SELECT (lc_Parser)
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ENDIF

    SKIP
  ENDDO
  
  SELECT (lc_Parser)
  ZAP
  
  SELECT (lcTmpPrjDt)
  SET ORDER TO PMPRJDT IN (lcTmpPrjDt)
  =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
       SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
ENDSCAN

*-- After all operations are scanned, get the project estimated finish date.
*-- The project estimated finish date is the greatest estimated finish
*-- date of all operations/
DECLARE laTmpVal[1]
SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
  FROM (lcTmpPrjDt)   ;
  WHERE cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
              SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
              SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle)) ;
       .AND. !lVoid;       
  INTO ARRAY laTmpVal
IF !EMPTY(laTmpVal[1])
  m.dclc_strt = laTmpVal[1]  
  m.dclc_fnsh = laTmpVal[2]  
  IF llFromWeb 
    SELECT PMPRJHD
    REPLACE dclc_strt WITH m.dclc_strt,;
            dclc_fnsh WITH m.dclc_fnsh
               
    SELECT (lcTmpPrjDt)
  ENDIF
ENDIF

*--  Restore relations and tags
SET ORDER TO TAG PMPRJRL IN (lcTmpPrjRl)
SELECT (lcTmpPrjDt)
SET ORDER TO (lnCurTag)

SET FILTER TO  
SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID ;
           INTO (lcTmpPrjRl)
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfUpdClcDt
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Updates the current operation's calculated start 
*!             and finish date
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters : ldNxtStDte : next start date.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdClcDt({01/01/96})
*!*************************************************************
FUNCTION lfUpdClcDt
PARAMETERS ldNxtStDte

*--  Get the hidger date value as a start date
ldNxtStDte = MAX(ldNxtStDte,dclc_strt)

DO CASE
  *--  Case Actual values are entered   
  CASE !EMPTY(dact_strt) .OR. !EMPTY(dact_fnsh) .OR. nAct_dur > 0
    *--  If an actual start date exists, default the calculated start
    *--  date with its value,
    *--  If an actual finish date exists, default the calculated finish
    *--  date with its value,
    *--  else, 
    *--  If an actual duration exists, add it to the calculated start
    *--  date, else add the estimated duration to the calculated start
    *--  date.
    IF !EMPTY(dact_strt) 
      ldClcFshDt = {}
      REPLACE dclc_strt WITH lfAdjDate(dact_strt, @ldClcFshDt,;
                                       MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
                                       cCal_ID, '+'),;
              dclc_fnsh WITH IIF(!EMPTY(dact_fnsh), dact_fnsh, ldClcFshDt)

    *--  Else,
    *--  If an actual start date is not found, 
    ELSE  && ELSEIF !EMPTY(dact_strt)  
      *--  If an actual finish date exists, default the calculated finish
      *--  date with its value,
      *--  If an actual finish date exists, default the calculated finish
      *--  date with its value,
      *--  Calculate the calculated start date as follows :
      *--  If an actual duration exists, subtract it from the calculated 
      *--  finish date, else subtract the estimated duration from the 
      *--  calculated finish date.
      IF !EMPTY(dact_fnsh)
        ldClcStDt = {}        
        =lfAdjDate(dact_fnsh, @ldClcStDt,;
                   MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
                   cCal_ID, '-')
        REPLACE dclc_strt WITH ldClcStDt,;
                dclc_fnsh WITH dact_fnsh
               
      *--  Else, both actual start and finish dates are emoty,
      *--  i.e. the actual duration is not empty
      *--  Calculate the start and finish calculated dates
      *--  based on the finish date of the previous operation,
      *--  using actual duration
      ELSE  && ELSEIF !EMPTY(dact_fnsh)
        ldClcFshDt = {}
        REPLACE dclc_strt WITH lfAdjDate(ldNxtStDte, @ldClcFshDt          ,;
                               MAX(0, nAct_dur - 1), cCal_ID, '+'),;
                dclc_fnsh WITH ldClcFshDt                    
      ENDIF && ENDIF !EMPTY(dact_fnsh)                                    
    ENDIF && ENDIF !EMPTY(dact_strt)    
  *--  Otherwise, No Actual values are entered   
  OTHERWISE
    
    *--  If the remaining duration is different from the estimated
    *--  duration, this indicates that the current operation is in work.
    IF nrem_dur <> nest_dur
      *--  Get the calculated start date referenced to the 
      *--  scheduling date (dSch_Date)
        ldClcStDt = {}
        IF nest_dur = 0
          STORE ldNxtStDte + 1 TO ldClcStDt, ldClcFshDt
        ELSE
          STORE ldNxtStDte TO ldClcStDt, ldClcFshDt
        ENDIF  
        =lfAdjDate(@ldClcStDt,@ldClcFshDt,nrem_dur, cCal_ID, '+')                   

      REPLACE dclc_strt WITH ldClcStDt,;
              dclc_fnsh WITH ldClcFshDt   

    *--  Otherwise, calculate dates based on the finish date of
    *--  the previous operation and the remaining duration
    ELSE
      ldClcFshDt = {}
      REPLACE dclc_strt WITH lfAdjDate(ldNxtStDte, @ldClcFshDt,;
                                       MAX(0, nrem_dur - 1), cCal_ID, '+'),;
              dclc_fnsh WITH ldClcFshDt
    ENDIF  
ENDCASE    


   
*!*************************************************************
*! Name      : lfvAct_Dat
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for actual dates get fields in the
*!             operations screen MFPROJA1
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAct_Dat()
*!*************************************************************
FUNCTION lfvAct_Dat
PRIVATE ldCurObj

lnCurAlias = SELECT(0)
SELECT (lc_PMPrjDt)

lcCurFld = SYS(18)

=lfFixY2K(ALIAS())

lcPredOpr = &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
IF EVALUATE(lcCurFld) <> lcOldVal
  IF !EMPTY(dact_strt)     .AND. ;
     !EMPTY(dact_fnsh)     .AND. ;
     dact_strt > dact_fnsh .AND. ;
     gfModalgen("TRM38220B00000","DIALOG") = 1
     
     REPLACE &lcCurFld WITH lcOldVal
  ELSE
    SELECT (lc_PMPrjDt)
    lnRecNo = RECNO()
    SCAN FOR lcPredec $ (cOprt_Ctg + '\' + cOprt_ID)
      IF EMPTY(dAct_Strt)
        =gfModalgen("INM38245B00000","DIALOG")
        IF BETWEEN(lnRecNo, 1, RECCOUNT())
          GO lnRecNo
        ENDIF 
        REPLACE &lcCurFld WITH {}
        SELECT (lnCurAlias)
        RETURN
      ENDIF
    ENDSCAN
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ENDIF 
    IF lcCurFld = 'DACT_FNSH'
      IF EMPTY(dAct_Strt)
        SELECT (lc_PMPrjDt)
        REPLACE &lc_PMPrjDt..dAct_Strt WITH IIF(EMPTY(&lc_PMPrjDt..dClc_Strt),;
                                                &lc_PMPrjDt..dEst_Strt,&lc_PMPrjDt..dClc_Strt)
      ENDIF                

      SELECT (lc_PMPrjDt)       
      lnWEnd = 0
      lnDur  = (&lc_PMPrjDt..dAct_Fnsh - &lc_PMPrjDt..dAct_Strt) + 1
      =lfGetHlidy()
      
      *E301869,2 AMH Restore the selected alias and record after calling lfGetHlidy [Start]
      SELECT (lc_PMPrjDt)       
      IF BETWEEN(lnRecNo, 1, RECCOUNT())
        GO lnRecNo
      ENDIF
      *E301869,2 AMH [End]
      
      REPLACE &lc_PMPrjDt..nAct_Dur  WITH ((&lc_PMPrjDt..dAct_Fnsh - &lc_PMPrjDt..dAct_Strt) + 1 - lnWEnd),;
              &lc_PMPrjDt..nRem_Dur  WITH 0,;
              &lc_PMPrjDt..cMComplt  WITH 'YYY',;
              &lc_PMPrjDt..lAddToAud WITH .T.,;
              &lc_PMPrjDt..cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
      =lfComTask()                                              
    ELSE
      SELECT (lc_PMPrjDt)
      REPLACE &lc_PMPrjDt..cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)  
    ENDIF  
    SELECT (lc_PMPrjDt)
    SET ORDER TO PMPRJDT
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
          SUBSTR(lcStyle,1,LEN(cStyle)) + lcPredOpr)
    STORE .T. TO llChngOprt, glUpdated
    llUpdStrtd = .F.
  ENDIF  
ENDIF
*=lfActBrows()


*!*************************************************************
*! Name      : lfVldPrd
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Checks a predecessor whether it would create a 
*!             cyclic path or not.
*!*************************************************************
*! Called    : lfvMSource() in MAINPROC, from ARIAMOVR.SR
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            : .T. if the current operation may be 
*!                      selected as a predecessor, .f. otherwise.
*!*************************************************************
*! Example            :  IF lfVldPrd(..)....
*!*************************************************************
FUNCTION lfVldPrd
PARAMETERS lcArrCtg, lcArrOprt, llMoveAll

IF _CUROBJ = OBJNUM(lsTarget) .OR. (TYPE("lnMovmnts")="N" .AND. (lnMovmnts = 3 OR lnMovmnts = 4))
  RETURN
ELSE
  SET PROCEDURE TO &lcSetProc
  lnRetVal = IIF(ASCAN(laPathElems, lcArrCtg + lcArrOprt) = 0, .T.,;
             IIF(gfModalgen("TRM38214B00000","DIALOG",ALLTRIM(lcArrCtg)+'\'+;
                                                      ALLTRIM(lcArrOprt)) = 1, ;
                               .F., .T.))                             
  IF !EMPTY(laTarget)
    lcPrevOrd = ORDER(lc_PMPrjRl)                               
    IF llMoveAll 
      SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
      IF lnRetVal AND (SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
                       SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
                       (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
        =gfModalgen("TRM38241B00000","DIALOG",;
                     lcArrCtg+"|"+lcArrOprt+"|"+lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMPRJRLP IN (lc_PMPrjRl)
        IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND ASCAN(laTarget,&lc_PMPrjRl..COprt_Ctg+'\'+&lc_PMPrjRl..COprt_ID) <> 0)      
          =gfModalgen("TRM38241B00000","DIALOG",;
                     lcArrCtg+"|"+lcArrOprt+"|"+lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
          lnRetVal = .F.
        ELSE
          SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
          IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND ASCAN(laTarget,&lc_PMPrjRl..CPrd_Ctg+'\'+&lc_PMPrjRl..CPrd_ID) <> 0)
            =gfModalgen("TRM38242B00000","DIALOG",;
            lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc)+"|"+lcArrCtg+"|"+lcArrOprt)
            lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF 
    ELSE
      SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
      IF lnRetVal AND (SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcOprt_ctg+lcOprt_ID,lc_PMPrjRl) AND;
                       SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+&lc_PMPrjRl..Cprd_Ctg+&lc_PMPrjRl..Cprd_ID,lc_PMPrjRl)) AND ;
                       (lcArrCtg = &lc_PMPrjRl..Cprd_Ctg AND lcArrOprt = &lc_PMPrjRl..Cprd_ID)
        =gfModalgen("TRM38243B00000","DIALOG",;
        lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMPRJRLP IN (lc_PMPrjRl)
        IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
                        ASCAN(laTarget,&lc_PMPrjRl..COprt_Ctg+'\'+&lc_PMPrjRl..COprt_ID) <> 0)      
          =gfModalgen("TRM38243B00000","DIALOG",;
          lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
          lnRetVal = .F.
        ELSE
          SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
          IF lnRetVal AND SEEK(lcPrj_typ+lcPrj_ID+SUBSTR(lcStyle,1,LEN(&lc_PMPrjRl..cStyle))+lcArrCtg+lcArrOprt,lc_PMPrjRl) AND;
                      ASCAN(laTarget,&lc_PMPrjRl..CPrd_Ctg+'\'+&lc_PMPrjRl..CPrd_ID) <> 0)
            =gfModalgen("TRM38244B00000","DIALOG",;
             lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
            lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF            
    ENDIF
    SET ORDER TO &lcPrevOrd IN (lc_PMPrjRl)             
  ENDIF
  SET PROCEDURE TO 
  RETURN lnRetVal
ENDIF

*!*************************************************************
*! Name      : lfAddOprs
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Automatic addition of operations through a mover.
*!*************************************************************
*! Calls     : ARIAMOVR.PRG
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .T. if operations are added to the file,
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfAddOprs()
*!*************************************************************
FUNCTION lfAddOprs
PRIVATE laAvailOprs, laSelOprs, llAddOprs
DECLARE laAvailOprs[1], laSelOprs[1]

lcSetProc = SET('PROCEDURE')
SET PROCEDURE TO

llAddOprs = .F.
SET ORDER TO TAG PMCTGDT IN PMCTGDT

IF SEEK(lcOprt_Ctg, 'PMCTGDT')
  
  SELECT cOprt_ID + ' ' + cOprt_Dsc ;
   FROM PMCTGDT;
   WHERE cOprt_Ctg = lcOprt_Ctg;       
   ORDER BY cOprt_Seq; 
  INTO ARRAY laAvailOprs

  laSelOprs = '' 
  
  =gfMover(@laAvailOprs, @laSelOprs,"Select From Available Operations", .T.)

  IF !EMPTY(laSelOprs[1])
    STORE .T. TO llChngOprt, glUpdated
    llUpdStrtd = .F.
    SELECT (lc_PMPrjDt)
    lcNewCtgSeq = lfGetSeqNum()

    FOR lnCount = 1 TO ALEN(laSelOprs,1)
      IF SEEK(lcOprt_Ctg + SUBSTR(laSelOprs[lnCount], 1, 5), 'PMCTGDT') 
        SELECT PMCTGDT 
        SCATTER MEMVAR MEMO
        
        INSERT INTO (lc_PMPrjDt); 
            ( cPrj_Typ , cPrj_ID  ,  cStyle,  ;
              cCtg_Seq , cOprt_Ctg, cOprt_Seq,;
              cOprt_ID , cOprt_Dsc, cOprt_Res,;
              lShw2Cust, mOprt_Com, cUpdtMthd, ;
              nest_dur , nrem_dur , cCal_id,;
              mNotify, cStatus,LORGINAL,cGroup_ID,;
              cAdd_User, dAdd_Date, cAdd_Time,cMComplt,lAddToAud);
            VALUES;
             (lcPrj_Typ, lcPrj_ID, lcStyle, ;
             lcNewCtgSeq, m.cOprt_Ctg, m.cOprt_Seq, ;
             m.cOprt_ID, m.cOprt_Dsc, m.cOprt_Res,;
             m.lShw2Cust, m.mOprt_Com, m.cUpdtMthd,;   
             m.nest_dur, m.nest_dur, m.cCal_id,;
             m.mNotify,  'A',.T.,m.cGroup_ID,;
             gcUser_ID  , ldCurDate, TIME(),'YYY',.T.)   
            

        lnNumOfLin = lnNumOfLin + 1
        IF !EMPTY(laOpertion[1])
          DECLARE laOpertion[ALEN(laOpertion) + 1] 
        ENDIF  
        laOpertion[ALEN(laOpertion)] = lcOprt_Ctg + '\' + m.cOprt_ID + ;
                                     ' ' + SUBSTR(m.cOprt_Dsc, 1, 18)
        IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
          SELECT PMCTGRL 
          SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID;
             FOR ASCAN(laOpertion, cOprt_Ctg + '\' + cPrd_ID ) > 0
            SCATTER MEMVAR MEMO
            INSERT INTO (lc_PMPrjRl) FROM MEMVAR
            SELECT (lc_PMPrjRl)
            REPLACE cPrj_Typ  WITH lcPrj_Typ,;
                    cPrj_ID   WITH lcPrj_ID,;
                    cStyle    WITH lcStyle,;
                    cPrd_Ctg  WITH cOprt_Ctg,;
                    cAdd_User WITH gcUser_ID,;
                    dAdd_Date WITH ldCurDate,;
                    cAdd_Time WITH TIME(),;
                    cStatus WITH 'A' 
          ENDSCAN           
        ENDIF    && ENDIF SE EK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
        
        *E302251,1 AMH Add Nofity Setting Records [Start]
        IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMPCTGNT')
          SELECT PMPCTGNT
          SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID
            SCATTER MEMVAR
            m.cPrj_Typ = lcPrj_Typ
            m.cPrj_ID  = lcPrj_ID
            m.cStyle   = lcStyle
            m.cStatus  = 'A'
            INSERT INTO (lcPMPRJNTF) FROM MEMVAR
          ENDSCAN
        ENDIF
        *E302251,1 AMH [End]
        
      ENDIF    && ENDIF SEEK(lcOprt_Ctg + SUBSTR(laSelOprs[lnCount]), 1, 5), 'PMCTGDT')         
    ENDFOR   && ENDFOR lnCount = 1 TO ALEN(laSelOprs,1)
    IF ALEN(laOpertion) > 1
      SHOW GET pbPredecsr ENABLE
    ENDIF 
    llAddOprs = .T.

    IF !EMPTY(laCategrie[1])
      DIMENSION laCategrie[ALEN(laCategrie) + 1]
    ENDIF  
    laCategrie[ALEN(laCategrie)] = lcOprt_Ctg
  ENDIF    && ENDIF !EMTPY(laSelOprs[1])
  SELECT (lc_PMPrjDt)
ELSE
  =gfModalgen("TRM38225B00000","DIALOG",ALLTRIM(lcOprt_Ctg))
  llAddOprs = .T.
ENDIF
SET PROCEDURE TO &lcSetProc
RETURN llAddOprs

*!*************************************************************
*! Name      : lfvSort
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for push button < Sort... >
*!*************************************************************
*! Calls     : PRMSORT.PRG
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSort()
*!*************************************************************
FUNCTION lfvSort

PRIVATE puCatgs, puOprts, laCatgLine, laOperLine,lcSeqCursr ;
        lsCatgLine, lsOperLine, lcCategory;
        lcOldCatg, llSaveSort, llChngSort, lnCurAlias

DEFINE POPUP puCatgs MOVER SCROLL MARGIN MARK ''
DEFINE POPUP puOprts MOVER SCROLL MARGIN MARK ''

*-- Declare categories and operations arrays
DECLARE laCatgLine[1,2], laOperLine[1,3]

*-- Initialize categpries and operations list variables
STORE 1 TO lsCatgLine, lsOperLine

STORE .F. TO llSaveSort, llChngSort  
lcSeqCursr   = gfTempName()
*-- Collect the operations' sequence of all operations into 
*-- a cursor so as to restore if cancelled.

SELECT RECNO() AS nRecNo, cStatus, cOprt_Seq, cCtg_Seq;
  FROM (lc_PMPrjDt);
  INTO CURSOR (lcSeqCursr)
  
*-- Collect all categories in categories arrays
SELECT DISTINCT &lc_PMPrjDt..cOprt_Ctg, cCtg_Dsc;
  FROM (lc_PMPrjDt), PMCTGHD;
 WHERE &lc_PMPrjDt..cOprt_Ctg = PMCTGHD.cOprt_Ctg;
  INTO ARRAY laCatgLine;
 ORDER BY &lc_PMPrjDt..cCtg_Seq
lcOldCatg   = laCatgLine[1,1]
lcCategory  = laCatgLine[1,2]

*-- Collect the operations of the first category so as to be 
*-- displayed in the related operations array
SELECT RECNO(), cOprt_ID, cOprt_Dsc;
  FROM (lc_PMPrjDt);
 WHERE cOprt_Ctg =  lcOldCatg;
  INTO ARRAY laOperLine;
 ORDER BY cOprt_Seq

*-- Define category's popup bars 
FOR lnCount = 1 to ALEN(laCatgLine,1)
  DEFINE BAR lnCount OF puCatgs ;
         PROMPT laCatgLine[lnCount,1] + ' ' + ;
                laCatgLine[lnCount,2] 
ENDFOR

*-- Define operations's popup bars 
FOR lnCount = 1 to ALEN(laOperLine,1)
  DEFINE BAR lnCount OF puOprts ;
         PROMPT laOperLine[lnCount,2] + ' ' + ;
                laOperLine[lnCount,3]
ENDFOR

lnCurAlias = SELECT(0)


*=gfClearKey()
PUSH KEY
SELECT (lc_PMPrjDt)
ON KEY LABEL ESC T=0
DO (gcScrDir+"MFPSORT.SPR")
ON KEY LABEL ESC DO  lpEscScr
POP KEY

*-- If the current sorting is to be saved,
IF llSaveSort
  *-- Save the last set of operations' sequence into the file, 
  FOR lnCount = 1 TO CNTBAR('puOprts')
    lnSourcRow = GETBAR('puOprts', lnCount)     
    IF lnSourcRow > 0
      GO laOperLine[lnSourcRow, 1]
      REPLACE cOprt_Seq WITH PADL(lnCount, 2, '0'),;
              cStatus   WITH IIF(cStatus = 'S', 'M', cStatus)
    ENDIF  
  ENDFOR

  **-- Save categories new sequence into the file.
  IF llChngSort
    FOR lnCount = 1 TO CNTBAR('puCatgs')
      lnSourcRow = GETBAR('puCatgs', lnCount)     
      IF lnSourcRow > 0
         REPLACE cCtg_Seq  WITH PADL(lnCount, 2, '0');
                 cStatus   WITH IIF(cStatus = 'S', 'M', cStatus);
            FOR cOprt_Ctg = laCatgLine[lnSourcRow, 1]                 
      ENDIF  
    ENDFOR
  ENDIF
*-- Otherwise, restore the old operations' sort 
ELSE
  SELECT (lcSeqCursr)
  INDEX ON nRecNo TAG (lcSeqCursr) OF (lcSeqCursr)
  
  SELECT (lc_PMPrjDt)
  SET RELATION TO RECNO() INTO (lcSeqCursr)
  REPLACE ALL cStatus   WITH &lcSeqCursr..cStatus,;
              cOprt_Seq WITH &lcSeqCursr..cOprt_Seq,;
              cCtg_Seq  WITH &lcSeqCursr..cCtg_Seq
ENDIF

*-- Restore environment
RELEASE POPUPS puCatgs, puOprts EXTENDED 
IF USED(lcSeqCursr)
  USE IN (lcSeqCursr)
ENDIF
lcBrfields  = lcOldBrFild

SELECT (lc_PMPrjDt)
GO TOP 
=lfwBrwWhen(lc_PMPrjDt, m.cPrj_Stts $ 'PI' .AND. !lVoid)

*!*************************************************************
*! Name      : lfwLsCatgs
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : When function of lsCatgLine
*!             Adjusts display of a categoy's related operations
*!             in the operations list.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwLsCatgs()
*!*************************************************************
FUNCTION lfwLsCatgs
IF CNTBAR('puCatgs') > 0
  *-- If a new category is selected, or the order of the category
  *-- is changed,
  lnBarNo  = GETBAR('puCatgs', lsCatgLine)
  llChngSort  = llChngSort  .OR. lnBarNo <> lsCatgLine
  IF lcOldCatg <> laCatgLine[lnBarNo, 1]
    lcOldCatg  = laCatgLine[lnBarNo, 1]
    lcCategory = laCatgLine[lnBarNo, 2]
        
    *-- Save the old category's operations order into the file, 
    FOR lnCount = 1 TO CNTBAR('puOprts')
      lnSourcRow = GETBAR('puOprts', lnCount)     
      IF lnSourcRow > 0
        IF BETWEEN(laOperLine[lnSourcRow, 1], 1, RECCOUNT())
          GO laOperLine[lnSourcRow, 1]
        ENDIF

        REPLACE cOprt_Seq WITH PADL(lnCount, 2, '0'),;
                cStatus   WITH IIF(cStatus = 'S', 'M', cStatus) 
      ENDIF  
    ENDFOR
    
    *-- Recolloect the new category's operations into the operation
    *-- lines array
    SELECT RECNO(), cOprt_ID, cOprt_Dsc;
      FROM (lc_PMPrjDt);
     WHERE cOprt_Ctg =  lcOldCatg;
      INTO ARRAY laOperLine;
     ORDER BY cOprt_Seq

    *-- Release bars of the current operations file
    RELEASE BAR ALL OF puOprts
    
    *-- ReDefine operations's popup bars 
    FOR lnCount = 1 to ALEN(laOperLine,1)
      DEFINE BAR lnCount OF puOprts ;
             PROMPT laOperLine[lnCount,2] + ' ' + ;
                    laOperLine[lnCount,3]
    ENDFOR
    
    *-- Show operations list
    lsOperLine = 1
    SHOW GET lsOperLine 
    =lfRefresh()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfInitStage
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Initialize current stage array
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcCurCode : project status
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfInitStage(m.cPrj_Stts)
*!*************************************************************
FUNCTION lfInitStage
PARAMETERS lcCurCode
DO CASE
  *-- If in PLanning
  CASE lcCurCode = 'P'
    *AMH
    *DECLARE laCurStage[2,2]
    DECLARE laCurStage[3,2]
    laCurStage[1,1] = lcCurStage
    laCurStage[1,2] = lcCurCode
    *laCurStage[2,1] = laStages[4,1]
    *laCurStage[2,2] = laStages[4,2]
    *lcStages = 'PX'
    laCurStage[2,1] = laStages[3,1]
    laCurStage[2,2] = laStages[3,2]
    laCurStage[3,1] = laStages[4,1]
    laCurStage[3,2] = laStages[4,2]
    lcStages = 'PCX'
    *AMH
  CASE lcCurCode = 'I'
    DECLARE laCurStage[3,2]
    laCurStage[1,1] = lcCurStage
    laCurStage[1,2] = lcCurCode
    laCurStage[2,1] = laStages[3,1]
    laCurStage[2,2] = laStages[3,2]
    laCurStage[3,1] = laStages[4,1]
    laCurStage[3,2] = laStages[4,2]
    lcStages = 'ICX'
  CASE lcCurCode $ 'CX'
    DECLARE laCurStage[2,2]
    laCurStage[1,1] = lcCurStage
    laCurStage[1,2] = lcCurCode
    laCurStage[2,1] = laStages[5,1]
    laCurStage[2,2] = laStages[5,2]
    lcStages = lcCurCode + 'H'
  OTHERWISE
    lcStages        = 'PICXH'
    DECLARE laCurStage[5,2]
    =ACOPY(laStages, laCurStage)
ENDCASE


*!*************************************************************
*! Name      : lfvCurStag
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Initialize current stage array
*!*************************************************************
*! Calls     : gcActPop
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurStag()
*!*************************************************************
FUNCTION lfvCurStag

lcOldVal = m.cPrj_Stts
m.cPrj_Stts = laCurSTage[puCurStage, 2] 

IF lcOldVal <> m.cPrj_Stts    
  IF m.cPrj_Stts $ 'CXH'
    SHOW GET m.dReq_Strt   DISABLE
    SHOW GET m.dReq_Fnsh   DISABLE
    SHOW GET m.dest_strt   DISABLE
    SHOW GET m.dact_strt   DISABLE
    SHOW GET m.dact_fnsh   DISABLE    
    SHOW GET ibPath_ID     DISABLE
    SHOW GET m.cpath_id    DISABLE
    SHOW GET m.cPrj_SDsc   DISABLE
    IF m.cPrj_Stts = 'H'
      SHOW GET pbOprs    DISABLE
    ELSE
      SHOW GET pbOprs    ENABLE
    ENDIF  
  ELSE
    SHOW GET m.cPrj_SDsc ENABLE
    IF lnNumOfLin = 0
      SHOW GET ibPath_ID   ENABLE 
      SHOW GET m.cpath_id  ENABLE
    ELSE
      SHOW GET ibPath_ID   DISABLE
      SHOW GET m.cpath_id  DISABLE
    ENDIF
    SHOW GET m.dReq_Strt   ENABLE
    SHOW GET m.dReq_Fnsh   ENABLE
    IF m.cPrj_Stts = 'P'
      SHOW GET m.dest_strt ENABLE
    ELSE
      SHOW GET m.dest_strt DISABLE
    ENDIF  
    SHOW GET m.dact_strt   ENABLE
    SHOW GET m.dact_fnsh   ENABLE
  ENDIF  
ENDIF

IF m.cPrj_Stts = 'C'
  m.dAct_strt = m.dClc_strt
  m.dAct_Fnsh = m.dClc_Fnsh

  lcPrvAlis = SELECT(0)
  SELECT (lc_PMPrjDt)
  SCAN
    lcMCompS = 'Y'
    lcMCompF = 'Y'
    lcMCompD = 'Y'
    IF !EMPTY(&lc_PMPrjDt..dAct_strt)
      lcMCompS = 'N'
    ENDIF
    IF !EMPTY(&lc_PMPrjDt..dAct_Fnsh)
      lcMCompF = 'N'
    ENDIF
    IF &lc_PMPrjDt..nAct_Dur <> 0
      lcMCompD = 'N'
    ENDIF
    lcMComplt = lcMCompS + lcMCompF + lcMCompD
    
    REPLACE dAct_strt WITH IIF(lcMCompS = 'N',&lc_PMPrjDt..dAct_strt,&lc_PMPrjDt..dClc_strt),;
            dAct_Fnsh WITH IIF(lcMCompF = 'N',&lc_PMPrjDt..dAct_Fnsh,&lc_PMPrjDt..dClc_Fnsh),;
            nAct_Dur  WITH IIF(lcMCompD = 'N',&lc_PMPrjDt..nAct_Dur,&lc_PMPrjDt..nRem_Dur),;
            cStatus   WITH 'M',;
            cMComplt  WITH lcMComplt
             
  ENDSCAN
  
  SELECT (lcPrvAlis)

  SHOW GET m.dAct_strt
  SHOW GET m.dAct_fnsh
  =lfRefresh()
ELSE
  IF m.cPrj_Stts <> 'H'
    m.dAct_strt = {}
    m.dAct_Fnsh = {}

    lcPrvAlis = SELECT(0)
    SELECT (lc_PMPrjDt)
    SCAN
      lcMCompS = SUBSTR(&lc_PMPrjDt..cMComplt,1,1)
      lcMCompF = SUBSTR(&lc_PMPrjDt..cMComplt,2,1)
      lcMCompD = SUBSTR(&lc_PMPrjDt..cMComplt,3,1)
    
      REPLACE dAct_strt WITH IIF(lcMCompS = 'N',&lc_PMPrjDt..dAct_strt,{}),;
              dAct_Fnsh WITH IIF(lcMCompF = 'N',&lc_PMPrjDt..dAct_Fnsh,{}),;
              nAct_Dur  WITH IIF(lcMCompD = 'N',&lc_PMPrjDt..nAct_Dur,0),;
              cStatus   WITH 'M'
    ENDSCAN
  
    SELECT (lcPrvAlis)

    SHOW GET m.dAct_strt
    SHOW GET m.dAct_fnsh
    =lfRefresh()
  ENDIF  
ENDIF

glUpdated = glUpdated .OR. PMPRJHD.cPrj_Stts <> m.cPrj_Stts 
lnCurStage = puCurStage 

*!*************************************************************
*! Name      : lfGetSeqNum
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Calculates a category sequence number
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  A correct category sequence number
*!*************************************************************
*! Example            :  lfGetSeqNum()
*!*************************************************************
FUNCTION lfGetSeqNum
PRIVATE lnCurAlias, lnCurTag, lnCurRec, lcNewCtgSeq

lnCurAlias = SELECT(0)
SELECT (lc_PMPrjDt)
lnCurRec = RECNO()
lnCurTag = VAL(SYS(21))
SET ORDER TO TAG PMPRJDT
IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
        SUBSTR(lcStyle,1,LEN(cStyle))+ SUBSTR(lcOprt_Ctg,1,LEN(cOprt_Ctg)))
  lcNewCtgSeq = cCtg_Seq
ELSE  
  *SET ORDER TO TAG PMPRJDTS
  *IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
          SUBSTR(lcStyle,1,LEN(cStyle)) + PMCTGDT.cCtg_Seq)
    SELECT MAX(cCtg_Seq) ;
      FROM (lc_PMPrjDt);
      INTO ARRAY laTmpArry
    lcNewCtgSeq = IIF(_TALLY > 0, PADL(INT(VAL(laTmpArry))+1,  2, '0'), '01')
  *ELSE
  *  lcNewCtgSeq = PMCTGDT.cCtg_Seq
  *ENDIF
ENDIF
SET ORDER TO (lnCurTag)
IF BETWEEN(lnCurRec, 1, RECCOUNT())
  GO lnCurRec
ENDIF
SELECT (lnCurAlias)  
RETURN lcNewCtgSeq

*!*************************************************************
*! Name      : lfAdjPrm
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Adjusts remove/void button prompt in the Operations 
*!             screen. (Windows)     
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  A correct category sequence number
*!*************************************************************
*! Example            :  lfAdjPrm()
*!*************************************************************
FUNCTION lfAdjPrm


IF EOF(lc_PMPrjDt) .OR. m.cPrj_Stts = 'P' .OR. EMPTY(&lc_PMPrjDt..dest_strt)
  SHOW GET pbWinRemove 
ELSE
  SHOW GET pbVoid 
ENDIF  
_CUROBJ = ibTabs

*!*************************************************************
*! Name      : lfvCal_ID
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Validates get field cCal_ID
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvCal_ID()
*!*************************************************************
FUNCTION lfvCal_ID
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
SELECT (lc_PMPrjDt)
llBrowse  = llBrowse .OR. '?' $ cCal_ID
lcSetExact = SET('EXACT')
SET EXACT ON
 
REPLACE cCal_ID WITH PADR(ALLTRIM(cCal_ID),4)
SHOW GET cCal_ID
SELECT PMCALHD
IF !llBrowse 
  IF !EMPTY(&lc_PMPrjDt..cCal_ID) .AND. !SEEK(&lc_PMPrjDt..cCal_ID, 'PMCALHD') 
    IF BETWEEN(RECNO(0), 1, RECCOUNT())
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    llBrowse = .T.    
  ELSE
    SELECT (lc_PMPrjDt)
    REPLACE cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
    STORE .T. TO glUpdated, llChngOprt
    llUpdStrtd = .F.
  ENDIF
ELSE
  GO TOP  
ENDIF 

IF llBrowse  
  lcBrFields = lcCalFlds
  DIMENSION laSelected[1]  
  lcFldName = 'cCal_ID'
  IF ARIABROW('', @lcCalTtl,; 
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laSelected')
    SELECT (lc_PMPrjDt)
    REPLACE cCal_ID   WITH PMCALHD.cCal_ID,;
            cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
    STORE .T. TO glUpdated, llChngOprt
    llUpdStrtd = .F.
  ELSE
    SELECT (lc_PMPrjDt)    
    REPLACE cCal_ID WITH lcOldVal
  ENDIF           
ENDIF
llBrowse = .F.
SET EXACT &lcSetExact

SELECT (lnCurAlias)            


*!*************************************************************
*! Name      : lfAdjDate
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Adjusts date addition according to calendar 
*!             holidays and weekends 
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : ldOprt_Str : Start date
*!                      ldOprt_Fsh : pointer to finish date
*!                      lnOprt_Dur  : duration to add
*!                      lcCal_ID    : calendar ID of the operation
*!                      lcOperator  : '+', '-'
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  ldStrtDate = lfAdjDate(m.dest_strt,;
*!                                              @ldAdjEstFn,; 
*!                                              MAX(0, nest_dur - 1),;
*!                                              cCal_ID, '+'),;
*!*************************************************************
FUNCTION lfAdjDate
PARAMETERS ldOprt_Str, ldOprt_Fsh, lnOprt_Dur, lcCal_ID, lcOperator 
PRIVATE lnCount, ldOprt_Fsh 

IF SEEK(lcCal_ID, 'PMCALHD')
  DO WHILE STR(DOW(ldOprt_Str),1) $ PMCALHD.cCal_WEnd .OR. ;
               ASCAN(laHolidays,lcCal_ID + DTOC(ldOprt_Str)) > 0               
    ldOprt_Str = ldOprt_Str &lcOperator 1 
  ENDDO             
  
  lnCount = 1
  ldOprt_Fsh = ldOprt_Str
  DO WHILE lnCount <= lnOprt_Dur
     ldOprt_Fsh = ldOprt_Fsh &lcOperator 1
     lnCount    = lnCount ;
                 + IIF(STR(DOW(ldOprt_Fsh),1) $ PMCALHD.cCal_WEnd .OR. ;
                 ASCAN(laHolidays,lcCal_ID + DTOC(ldOprt_Fsh)) > 0 , 0 , 1)
  ENDDO
ELSE
  ldOprt_Fsh = ldOprt_Str &lcOperator lnOprt_Dur
ENDIF
RETURN ldOprt_Str


*!*************************************************************
*! Name      : lfvExpOK
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Calls a dialog MFPROJB.SPR so as to enter
*!             a file name to Export
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvExpOK() 
*!*************************************************************
FUNCTION lfvExpOK

llExport = .T.
CLEAR READ


*!*************************************************************
*! Name      : lfvGetFile
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Calls GETFILE
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvGetFile() 
*!*************************************************************
FUNCTION lfvGetFile
PRIVATE lcNewFile
lcNewFile = GETFILE("DBF","Select file for output","Select",1)
IF !EMPTY(lcNewFile)
  lcFileName = lcNewFile
  SHOW GET lcFileName
ENDIF


*!*************************************************************
*! Name      : lfvFilName
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : Valid function for get field lcFileName (MFPROJB)
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvFilName() 
*!*************************************************************
FUNCTION lfvFilName
IF lcFileName <> lcOldVal
  lcFileName = PADR(ALLTRIM(lcFileName), 25)
ENDIF


*!*************************************************************
*! Name      : lfExtPrg                      N49,1
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Calling External program
*!*************************************************************
*! Calls              :  lfExtPrg()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfExtPrg()
*!*************************************************************
FUNCTION lfExtPrg
PARAMETERS lcMode, lcOprtCtg, lcNewOprt

llCatgrAdd = .F.
lcCurBaseF = lcBaseFile
=gfStatic()
DO gcAppHome+"SM\"+"SMCATGR" WITH lcMode, lcOprtCtg, lcNewOprt
lcBaseFile = lcCurBaseF
RETURN llCatgrAdd

*!*************************************************************
*! Name      : lfvClose
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : valid function of Close operation screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClose()
*!*************************************************************
FUNCTION lfvClose


IF laScrMode[3] OR laScrMode[4]
  llEnbSave = .T.
  =lpShow()
ENDIF  

CLEAR READ

*!*************************************************************
*! Name      : lfvOkSort
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for pbOk (push button OK)
*!             Confirms current sort of categories and operations
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkSort()
*!*************************************************************
FUNCTION lfvOkSort
llSaveSort = .T.

*!*************************************************************
*! Name      : lfvUser
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Valid function of user popup
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfvUser

lcOprt_Res = SPACE(10)

SHOW GET lcOprt_Res

=lfRefresh()

*!*************************************************************
*! Name      : lfHistory
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Valid function of History button
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfHistory

lcOrgBase = lcBaseFile
lcOrgProg = lcProgName 
lcOrgKey  = lcSydKey   

DO CASE
  CASE gcAct_Appl = 'MF'
    lcProgName = 'MFCUTKT'
    lcBaseFile = 'CUTTKTH'
    lcSydKey   = 'IlcPrj_id|CUTTKTH'
    DO CASE  
      CASE lcPrj_typ = 'A'      && Adorment Order
        lcProgName = 'MFADPO'      
        lcBaseFile = 'POSHDR'
        lcSydKey   = 'B*lcPrj_typ+lcPrj_id|POSHDR'    
      CASE lcPrj_typ = 'D'      && Dye Order 
        lcProgName = 'MFDPO'      
        lcBaseFile = 'POSHDR'
        lcSydKey   = 'B*lcPrj_typ+lcPrj_id|POSHDR'
    ENDCASE      
  CASE gcAct_Appl = 'PO'
    DO CASE
      CASE lcPrj_typ = 'P'      && PO
        lcProgName = 'POSTY'
      CASE lcPrj_typ = 'N'      && Inter-Location PO
        lcProgName = 'POINTRC'      
      CASE lcPrj_typ = 'R'      && Return PO
        lcProgName = 'RETPO'      
    ENDCASE
    lcBaseFile = 'POSHDR'
    lcSydKey   = 'B*lcPrj_typ+lcPrj_id|POSHDR'
  CASE gcAct_Appl = 'SO'
    DO CASE
      CASE lcPrj_typ = 'O'      && SO
        lcProgName = 'SOORD'
      CASE lcPrj_typ = 'T'      && EDI Order
        lcProgName = 'SOEDORD'      
    ENDCASE
    lcBaseFile = 'ORDHDR'
    lcSydKey   = 'B*lcPrj_typ+lcPrj_id|ORDHDR'
  
  *E302251,1 AMH Add new project types [Start]
  CASE gcAct_Appl = 'IC'
    DO CASE
      CASE lcPrj_typ = 'S'      && Style
        lcProgName = 'ICSTYLE'
        lcBaseFile = 'STYLE'
        lcSydKey   = 'B*m.cStyle|STYLE'
      CASE lcPrj_typ = 'H'      && Other
        lcProgName = 'MFPROJ'
        lcBaseFile = 'PMPRJHD'
        lcSydKey   = 'B*lcPrj_typ+lcPrj_id|PMPRJHD'
    ENDCASE
  *E302251,1 AMH [End]
  
ENDCASE

=gfvAudTral()

lcBaseFile = lcOrgBase
lcProgName = lcOrgProg
lcSydKey   = lcOrgKey

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : create report template browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfDispBrow()
*!*************************************************************
FUNCTION lfDispBrow

lnBrRecNo  = RECNO(lc_PrjHist)
lcBrowTitl = 'Project History'
lcBrFields = "cOpr_ID   :R :H='Task ID':25,"+;
             "cUser_ID  :R :H='User ID':8,"+;
             "cRemain   :R :H='Remaining':10,"+;
             "cCompDate :R :H='Complete Date':14,"+;
             "cActnDate :R :H='Transaction Date':17,"+;
             "cStatus   :R :H='Status':15"

BROWSE FIELDS &lcBrFields;
       WINDOW MFPRHIS0;
       WHEN lfwBrow();
       IN WINDOW MFPRHIST;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       NOEDIT;
       VALID lfvBrows();
       SAVE;
       TITLE lcBrowTitl

*!*************************************************************
*! Name      : lfwBrow
*! Developer : HEND GHANEM
*! Date      : 12/20/2001
*! Purpose   : WHEN function of the browse command
*!*************************************************************
*! Calls              : lfRefresh()
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  BROWSE ..  WHEN lfwBrwWhen()
*!*************************************************************
FUNCTION lfwBrow

lnBrRecNo  = RECNO(lc_PrjHist)
SHOW WINDOW (lcBrowTitl) REFRESH 
KEYBOARD("{ALT+B}")

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Ahmed Ibrahim
*! Date      : 09/09/1999
*! Purpose   : Valid Browse function.
*! Reference : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBrows()
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcBrowTitl) 
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfPushKey
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : trap keys.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfPushKey()
*!*************************************************************
FUNCTION lfPushKey 


ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL ESC T=0
*ON KEY LABEL ESC        DO lfvClshist
ON KEY LABEL Ctrl+ENTER DO lfvClshist
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTitl)

*!*************************************************************
*! Name      : lfDeAct
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfDeAct()
*!*************************************************************
FUNCTION lfDeAct

*--  check if browse window is active.
ON KEY LABEL ALT+B
IF WONTOP(lcBrowTitl)
  lnBrRecNo  = RECNO(lc_PrjHist)
  =lfBrowTrap()
ELSE
  =lfBrwUnTrp() 
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lfAct
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfAct()
*!*************************************************************
FUNCTION lfAct

=gfClearKey()
ON KEY LABEL ESC T=0
=lfPushKey()
=lfBrwUnTrp() 
ON KEY LABEL ALT+B DO lpAct


*!*************************************************************
*! Name      : lpAct
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfAct()
*!*************************************************************
PROCEDURE lpAct

IF _WINDOWS OR _MAC
  =lFBROWTRAP() 
ENDIF  
SHOW WINDOW (lcBrowTitl)

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfBrowTrap()
*!*************************************************************
FUNCTION lfBrowTrap

ON KEY LABEL TAB         DO  lpTrap1
ON KEY LABEL BACKTAB     DO  lpTrap2 

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Release browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfBrwUnTrp()
*!*************************************************************
FUNCTION lfBrwUnTrp

ON KEY LABEL TAB    
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfvClshist
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Validation on push button close.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvClshist()
*!*************************************************************
FUNCTION lfvClshist

IF WONTOP() # (lcBrowTitl) 
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

ACTIVATE WINDOW MFPRHIS1
_CUROBJ = OBJNUM(pbOK)
KEYBOARD("{ENTER}")

*!*************************************************************
*! Name      : lpTrap1
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Tab trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lpTrap1()
*!*************************************************************
PROCEDURE lpTrap1

IF WONTOP(lcBrowTitl)
  ACTIVATE WINDOW MFPRHIS1
ENDIF


*!*************************************************************
*! Name      : lpTrap2
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : shift tab trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lpTrap2()
*!*************************************************************
PROCEDURE lpTrap2
 
IF WONTOP(lcBrowTitl)
  ACTIVATE WINDOW MFPRHIS1
  _CUROBJ = OBJNUM(pbOK)
ENDIF


*!*************************************************************
*! Name      : lfGetData
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Get data.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfGetData()
*!*************************************************************
FUNCTION lfGetData

SELECT (lc_PMPrjDt)
ZAP

SELECT PMPRJHD
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GO RECNO()
ENDIF
    
SELECT PMPRJDT
SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = m.cPrj_Typ + m.cPrj_ID + m.cStyle
  SCATTER MEMVAR MEMO
  m.nRecNo  = RECNO()
  m.cStatus = 'S'
  
  lcMCompS = IIF(EMPTY(m.dAct_Strt),'Y','N')
  lcMCompF = IIF(EMPTY(m.dAct_Fnsh),'Y','N')
  lcMCompD = IIF(m.nAct_Dur = 0 ,'Y','N')
  m.cMComplt = lcMCompS + lcMCompF + lcMCompD
  
  INSERT INTO (lc_PMPrjDt) FROM MEMVAR
  laOpertion[ALEN(laOpertion)] = m.cOprt_Ctg + '\' + m.cOprt_ID + ;
                                  ' ' +SUBSTR(m.cOprt_Dsc, 1, 18)
    
  DECLARE laOpertion[ALEN(laOpertion) + 1]
ENDSCAN
lnNumOfLin = RECCOUNT(lc_PMPrjDt)
IF ALEN(laOpertion) > 1
  DECLARE laOpertion[ALEN(laOpertion) - 1]
ENDIF
    
SELECT DISTINCT cOprt_Ctg ;
  FROM (lc_PMPrjDt);
  INTO ARRAY laCategrie

SELECT (lc_PMPrjRl)
ZAP
SELECT PMPRJRL
IF SEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle)
  SCAN REST WHILE  cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = ;
                   m.cPrj_Typ + m.cPrj_ID + m.cStyle
    SCATTER MEMVAR MEMO
    m.nRecNo  = RECNO()
    m.cStatus = 'S'
    INSERT INTO (lc_PMPrjRl) FROM MEMVAR
  ENDSCAN
ENDIF
    
SELECT PMPRJHD
SCATTER MEMVAR MEMO


*-----
FUNCTION gfCPBrows

DO CASE
  CASE gcAct_Appl = 'MF'
    lcKey = 'CAD'
  CASE gcAct_Appl = 'PO'
    lcKey = 'PRN'  
  CASE gcAct_Appl = 'SO'    
    lcKey = 'OT'
  
  *E302251,1 AMH Add new project types [Start]
  CASE gcAct_Appl = 'IC'
    lcKey = 'SH'
  *E302251,1 AMH [End]
  
ENDCASE
SELECT (lcBaseFile)
=gfBrows('FOR &lcBaseFile..cPrj_typ $ lcKey')
DO lpShow

*!*************************************************************
*! Name      : lpEscScr
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   :
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
PROCEDURE lpEscScr
=gfCPClose()


*!*************************************************************
*! Name      : lfUpdAdTrl
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   :
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfUpdAdTrl
PARAMETERS lcProjType

SELECT (lc_PrjAudt)
ZAP

IF TYPE('lcProjType') = 'C'
  lcPrj_Typ = lcProjType
ENDIF

INSERT INTO (lc_PrjAudt) (cPrj_Typ,cOprt_Ctg,cOprt_ID,cOprt_Dsc,dest_Strt,dest_Fnsh,nrem_dur,;
                          cOprt_res,lVoid);
                  VALUES (lcPrj_Typ,m.cOprt_ctg,m.cOprt_ID,m.coprt_dsc,;
                          gdSysDate,ldest_Fnsh,lnrem_dur,;
                          IIF(EMPTY(m.cOprt_res),m.cGroup_ID,m.cOprt_res),m.lVoid)

*B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
*IF SEEK(m.cPrj_Typ + m.cPrj_ID + m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL') 
IF SEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL') 
*B130984,1 HBG 12/04/2006 [End]
  IF !(m.lOrginal)
    SELECT SYSCHDUL
    lcStauts = SYSCHDUL.COPERSTAT
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
    *LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
    *                m.cPrj_Typ + m.cPrj_ID + m.cOprt_Ctg+m.cOprt_ID ;
    *                FOR COPERSTAT <> lcStauts
    LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
                    m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.cOprt_Ctg+m.cOprt_ID ;
                    FOR COPERSTAT <> lcStauts
    *B130984,1 HBG 12/04/2006 [End]    
    IF !FOUND()
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
      *=SEEK(m.cPrj_Typ + m.cPrj_ID + m.cOprt_Ctg+m.cOprt_ID+lcStauts,'SYSCHDUL') 
      =SEEK(m.cPrj_Typ + m.cPrj_ID + m.cStyle + m.cOprt_Ctg+m.cOprt_ID+lcStauts,'SYSCHDUL') 
      *B130984,1 HBG 12/04/2006 [End]
    ENDIF                    
  ENDIF
  IF !(m.lVoid)
    REPLACE &lc_PrjAudt..cStatus WITH SYSCHDUL.COPERSTAT
  ELSE
    REPLACE &lc_PrjAudt..cStatus WITH 'X'
  ENDIF
ELSE
  IF !(m.lVoid)
    REPLACE &lc_PrjAudt..cStatus WITH IIF(m.nact_dur <> 0  OR !EMPTY(m.dAct_Fnsh),'C','O')
  ELSE
    REPLACE &lc_PrjAudt..cStatus WITH 'X'
  ENDIF  
ENDIF  

*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
SELECT (lc_PrjAudt)
LOCATE

*E302251,1 AMH Add new project types [Start]
*lcInform = 'Opr.: ' +EVAL(lc_PrjAudt+[.cOprt_Ctg]) +','+EVAL(lc_PrjAudt+[.cOprt_Id]) +;
   '-'+ALLTRIM(EVAL(lc_PrjAudt+[.cOprt_Dsc]))+;
   ', User : '+ALLTRIM(EVAL(lc_PrjAudt+[.cOprt_res]))+', Rem. : '+;
   ALLTRIM(STR(EVAL(lc_PrjAudt+[.nRem_Dur]),3))+', TDate: '+;
   DTOC(EVAL(lc_PrjAudt+[.dEst_strt]))+', CDate: '+;
   DTOC(EVAL(lc_PrjAudt+[.dEst_Fnsh]))+', Stat : '+;
   IIF(EVAL(lc_PrjAudt+[.cStatus]) = [O],[Open],IIF(EVAL(lc_PrjAudt+[.cStatus]) = [C],[Complete],IIF(EVAL(lc_PrjAudt+[.cStatus]) = [P],[In Work],IIF(EVAL(lc_PrjAudt+[.cStatus])= [H],[Hold],IIF(EVAL(lc_PrjAudt+[.cStatus])= [X],[Void],[])))))
IF !USED("PMCTGHD")
  =gfOpenFile(gcDatadir+'PMCTGHD','PMCTGHD','SH')
ENDIF
SELECT (lc_PrjAudt)

=SEEK(cOprt_Ctg,"PMCTGHD")
lcInform = 'Operation Category: '           + PMCTGHD.cCtg_Dsc                     + CHR(13)+;
           'Task: '                         + cOprt_Dsc                            + CHR(13)+;
           'User: '                         + cOprt_res                            + CHR(13)+;
           'Remaining Time on Completion: ' + ALLTRIM(STR(nRem_Dur,3)) + ' day(s)' + CHR(13)+;
           'Transaction Date: '             + DTOC(dEst_strt)                      + CHR(13)+;
           'Completion Date: '              + DTOC(dEst_Fnsh)                      + CHR(13)+;
           'Status: '+IIF(cStatus=[O],[Open],IIF(cStatus=[C],[Complete],IIF(cStatus=[P],[In Work],;
                      IIF(cStatus=[H],[Hold],IIF(cStatus=[X],[Void],[])))))
*E302251,1 AMH [End]

*HBG [End]

*!*************************************************************
*! Name      : lfFixY2K
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   :
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfFixY2K
PARAMETER lcFile2Chk

PRIVATE lnOldAls , laFFild , lnInd , lcDay , lcMonth , lcYear , lcOldSet , lcOldDat
lcOldSet = SET("CENTURY")
SET CENTURY ON
lcOldDat = SET("DATE")
SELECT (lcFile2Chk)
= AFIELDS(laFFild)
SET DATE "DMY"
FOR lnInd = 1 TO ALEN(laFFild,1)
  IF laFFild[lnInd,2] = "D" .AND. YEAR(EVAL(laFFild[lnInd,1]))<1950
    lcDay    = STR(DAY(EVAL(laFFild[lnInd,1])),2)
    lcMonth  = STR(MONTH(EVAL(laFFild[lnInd,1])),2)
    lcYear   = STR(YEAR(EVAL(laFFild[lnInd,1]))+100,4)
    ldNewDat = CTOD(lcDay+"\"+lcMonth+"\"+lcYear)
    REPLACE &laFFild[lnInd,1] WITH ldNewDat
  ENDIF
ENDFOR
SET CENTURY &lcOldSet
SET DATE &lcOldDat
lnOldAls = SELECT(0)
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfvLatStrt
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : valid function of latest start date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClose()
*!*************************************************************
FUNCTION lfvLatStrt

m.llastStrt = IIF(lnlatStrt = 1,.T.,.F.)

IF m.cPrj_Stts <> 'P' AND lnlatStrt = 0
  RETURN
ENDIF

ldOldEstmS  = m.dEst_Strt
ldOldEstmF  = m.dEst_fnsh

IF lnlatStrt = 1 AND !EMPTY(m.dest_fnsh) AND !llUpdStrtd 
  llUpdStrtd = .T.
  IF m.dreq_fnsh > m.dEst_fnsh 
    llFromFld   = .F.
    lcOldVal    = m.dEst_Strt
    ldOldStrt   = m.dEst_Strt
    ldOldFnsh   = m.dEst_fnsh 
    lnDefr = m.dreq_fnsh - m.dEst_fnsh 
    m.dEst_Strt = m.dEst_Strt + lnDefr
    =lfvEstDate()
    IF m.dEst_fnsh > m.dreq_fnsh
      DO WHILE (m.dEst_fnsh > m.dreq_fnsh)
        m.dEst_Strt = ldOldStrt   
        m.dEst_fnsh = ldOldFnsh   
        lnDefr = lnDefr - 1
        m.dEst_Strt = m.dEst_Strt + lnDefr
        =lfvEstDate()
        IF lnDefr = 0
          EXIT
        ENDIF
      ENDDO  
    ENDIF
  ELSE
    IF laScrMode[3] AND SEEK(lcPrj_Typ + m.cPrj_ID + m.cStyle , 'PMPRJHD') AND PMPRJHD.lLaststrt
      RETURN 
    ENDIF 

    IF m.dreq_fnsh < m.dEst_fnsh 
      *B127792,1 HBG 07/05/2005 Re-Calculate the start date back if the complete date is falls after the project require date [Begin]
      *=gfModalgen("INM38246B00000","DIALOG")
      IF gfModalgen("INM38246B00006","DIALOG") = 1
        llFromFld   = .F.
        lcOldVal    = m.dEst_Strt
        ldOldStrt   = m.dEst_Strt
        ldOldFnsh   = m.dEst_fnsh 
        lnDefr = m.dEst_fnsh - m.dreq_fnsh 
        m.dEst_Strt = m.dEst_Strt - lnDefr
        =lfvEstDate()
        IF m.dreq_fnsh < m.dEst_fnsh 
          DO WHILE (m.dreq_fnsh < m.dEst_fnsh )
            lnDefr = lnDefr + 1
            m.dEst_Strt = ldOldStrt - lnDefr
            =lfvEstDate()
          ENDDO  
        ENDIF
      ENDIF  
      *B127792,1 [End]
    ENDIF  
  ENDIF
ELSE
  IF lnlatStrt = 0
    m.dEst_Strt = ldOldEstmS  
    m.dEst_fnsh = ldOldEstmF 
  ENDIF  
ENDIF 
IF !llFromOper 
  SHOW GET m.dEst_Strt
  SHOW GET m.dEst_fnsh
  =lfRefresh()
ELSE
  llFromOper = .F.  
ENDIF  

*!*************************************************************
*! Name      : lfComTask
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : valid function of latest start date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfComTask()
*!*************************************************************
FUNCTION lfComTask

*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
=lfUpdVar()
*--Update Audit trail
*DO CASE
*  CASE gcAct_Appl = 'MF'
*    lcEvent = 'RESCHDCT'
*    DO CASE
*      CASE lcPrj_typ = 'A'      && Adorment Order
*        lcEvent = 'RESCHDPA'      
*      CASE lcPrj_typ = 'D'      && Dye Order 
*        lcEvent = 'RESCHDPD'      
*    ENDCASE
*  CASE gcAct_Appl = 'PO'
*    DO CASE
*      CASE lcPrj_typ = 'P'      && PO
*        lcEvent = 'RESCHDPO'
*      CASE lcPrj_typ = 'N'      && Inter-Location PO
*        lcEvent = 'RESCHDPN'      
*      CASE lcPrj_typ = 'R'      && Return PO
*        lcEvent = 'RESCHDPR'      
*    ENDCASE
*  CASE gcAct_Appl = 'SO'
*    DO CASE
*      CASE lcPrj_typ = 'O'      && SO
*        lcEvent = 'RESCHDSO'
*      CASE lcPrj_typ = 'T'      && EDI Order
*        lcEvent = 'RESCHDST'      
*    ENDCASE
*ENDCASE
*HBG [End]

SELECT SYSCHDUL
SET ORDER TO Coprusr
*B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
*IF SEEK(lcPrj_Typ+lcPrj_ID+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID)
IF SEEK(lcPrj_Typ+lcPrj_ID+lcStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID)
*B130984,1 HBG 12/04/2006 [End]
  REPLACE ccompleted WITH 'Y',;
          COPERSTAT  WITH 'C'
  llAddToAdt = .T.
ENDIF

lcPredOpr = &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
DIMENSION lANCont_ID[1]
lANCont_ID[1] =""
SET ORDER TO PMPRJRLP IN (lc_PMPrjRl)
IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
        SUBSTR(lcStyle,1,LEN(cStyle)) + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,lc_PMPrjRl)
  lnI = 0
  SELECT (lc_PMPrjRl)
  SCAN REST WHILE cprj_typ+cprj_id+cstyle+cprd_ctg+cprd_id = ;
                  SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
                  SUBSTR(lcStyle,1,LEN(cStyle)) + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
    DIMENSION lANCont_ID[lnI+1]
    lnI = ALEN(lANCont_ID,1)
    lANCont_ID[lnI] = &lc_PMPrjRl..cOprt_Ctg+&lc_PMPrjRl..cOprt_ID
  ENDSCAN
ENDIF

IF !EMPTY(lANCont_ID[1])
  FOR lnI = 1 TO ALEN(lANCont_ID,1)

    DIMENSION laNPrd_ID[1]
    STORE "" TO laNPrd_ID
    
    SET ORDER TO PMPRJRL IN PMPRJRL
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID))+;
            SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+lANCont_ID[lnI],'PMPRJRL')
      lnK = 0
      SELECT PMPRJRL
      SCAN REST WHILE cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id =;
              SUBSTR(lcPrj_Typ,1,LEN(PMPRJRL.cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(PMPRJRL.cPrj_ID))+;
              SUBSTR(lcStyle,1,LEN(PMPRJRL.cStyle))+lANCont_ID[lnI]
        DIMENSION laNPrd_ID[lnK +1]
        lnK = ALEN(laNPrd_ID,1)
        laNPrd_ID[lnK] =PMPRJRL.cPrd_Ctg+PMPRJRL.cPrd_ID              
      ENDSCAN        
    ENDIF
    
    llLoop = .F.
    FOR lnK = 1 TO ALEN(laNPrd_ID,1)
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
      *IF SEEK(lcPrj_Typ+lcPrj_ID+laNPrd_ID[lnK],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C' 
      IF SEEK(lcPrj_Typ+lcPrj_ID+lcStyle+laNPrd_ID[lnK],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C' 
      *B130984,1 HBG 12/04/2006 [End]
        llLoop = .T.
        EXIT
      ENDIF  
    ENDFOR
    IF llLoop 
      LOOP
    ENDIF

    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
    *IF SEEK(lcPrj_Typ+lcPrj_ID+lANCont_ID[lnI],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C'
    IF SEEK(lcPrj_Typ+lcPrj_ID+lcStyle+lANCont_ID[lnI],'SYSCHDUL') AND SYSCHDUL.cOperStat <> 'C'
    *B130984,1 HBG 12/04/2006 [End]
      SELECT  SYSCHDUL
      REPLACE lPredComp WITH .T.
      IF SYSCHDUL.cOperStat = 'H'
        REPLACE cOperStat  WITH 'O'
      ENDIF
    ENDIF
    llAddToAdt = .T.
    SELECT (lc_PMPrjDt)
    lcPrvOrd = ORDER()
    SET ORDER TO PMPRJUSR
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ))+SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+lANCont_ID[lnI])
    REPLACE &lc_PMPrjDt..cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            &lc_PMPrjDt..lAddToAud WITH .T.
    SET ORDER TO &lcPrvOrd
  ENDFOR
ENDIF

dSch_date = gdSysDate
=lfSchedule(.F.,.F.,.F.,.F.,.F.,.F.,.T.)


SET ORDER TO TAG PMPRJRLP IN (lc_PMPrjRl) 
SET ORDER TO TAG PMPRJDT  IN (lc_PMPrjDt) 
DO WHILE .T.
  IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
     SUBSTR(lcStyle,1,LEN(cStyle)) + lcPredOpr,lc_PMPrjRl)
    lcOprToUp = &lc_PMPrjRl..cOprt_Ctg+&lc_PMPrjRl..cOprt_ID
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
          SUBSTR(lcStyle,1,LEN(cStyle)) + lcOprToUp,lc_PMPrjDt)
    SELECT (lc_PMPrjDt) 
    REPLACE &lc_PMPrjDt..cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)  
  ELSE
    EXIT
  ENDIF  
  lcPredOpr = lcOprToUp 
ENDDO

*!*************************************************************
*! Name      : lfComSchdl
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : valid function of latest start date.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfComSchdl()
*!*************************************************************
FUNCTION lfComSchdl

*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
STORE "" TO lcProg , lcKey , lcApObjNam ,lcEvent
=lfUpdVar()
*--Update Audit trail
*DO CASE
*  CASE gcAct_Appl = 'MF'
*    lcEvent = 'RESCHDCT'
*    DO CASE
*      CASE lcPrj_typ = 'A'      && Adorment Order
*        lcEvent = 'RESCHDPA'      
*      CASE lcPrj_typ = 'D'      && Dye Order 
*        lcEvent = 'RESCHDPD'      
*    ENDCASE
*  CASE gcAct_Appl = 'PO'
*    DO CASE
*      CASE lcPrj_typ = 'P'      && PO
*        lcEvent = 'RESCHDPO'
*      CASE lcPrj_typ = 'N'      && Inter-Location PO
*        lcEvent = 'RESCHDPN'      
*      CASE lcPrj_typ = 'R'      && Return PO
*        lcEvent = 'RESCHDPR'      
*    ENDCASE
*  CASE gcAct_Appl = 'SO'
*    DO CASE
*      CASE lcPrj_typ = 'O'      && SO
*        lcEvent = 'RESCHDSO'
*      CASE lcPrj_typ = 'T'      && EDI Order
*        lcEvent = 'RESCHDST'      
*    ENDCASE
*ENDCASE
*HBG [End]

SELECT SYSCHDUL
SET ORDER TO Coprusr
IF SEEK(lcPrj_Typ+lcPrj_ID)
  SELECT (lc_PMPrjDt)
  lcPrvOrd = ORDER()
  SELECT SYSCHDUL
  *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
  *SCAN REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id = lcPrj_Typ+lcPrj_ID 
  SCAN REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id = lcPrj_Typ+lcPrj_ID+lcStyle
  *B130984,1 HBG 12/04/2006 [End]
    
    REPLACE ccompleted WITH 'Y',;
            COPERSTAT  WITH 'C'
          
    SELECT (lc_PMPrjDt)
    SET ORDER TO PMPRJUSR
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ))+SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+SYSCHDUL.ccont_id)
      SET ORDER TO PMPRJDT IN PMPRJDT        
      IF SYSCHDUL.COPERSTAT = 'C' AND SEEK(&lc_PMPrjDt..cPrj_Typ + &lc_PMPrjDt..cPrj_ID+;
                    &lc_PMPrjDt..cStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'PMPRJDT')
        IF PMPRJDT.LAUDTUPD
          LOOP
        ENDIF           
      ENDIF

      SCATTER MEMVAR MEMO
      ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
      lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,0)
      *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
      lcInform = ""
      *HBG [End]
      =lfUpdAdTrl()
      SELECT (lc_PrjAudt)
      *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
      *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
      *  =gfDoTriger('MFPROJ',PADR(lcEvent,10))
      *ENDIF
      DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
      *HBG [End]
     ENDIF 
  ENDSCAN
  SELECT (lc_PMPrjDt)
  SET ORDER TO &lcPrvOrd
ENDIF

*!*************************************************************
*! Name      : lfUpdVar
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdVar()
*!*************************************************************
FUNCTION lfUpdVar


*HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
lcApObjNam = 'MFPROJ'
*HBG [End]

*-- Update Audit Trail
DO CASE
  CASE gcAct_Appl = 'MF'
    lcProg = 'MFCUTKT'
    lcKey  = lcPrj_ID
    lcEvent = 'RESCHDCT'
    DO CASE
      CASE lcPrj_typ = 'A'      && Adorment Order
        lcProg= 'MFADPO'    
        lcKey = lcPrj_Typ + lcPrj_ID  
        lcEvent = 'RESCHDPA'
      CASE lcPrj_typ = 'D'      && Dye Order 
        lcProg = 'MFDPO'      
        lcKey = lcPrj_Typ + lcPrj_ID
        lcEvent = 'RESCHDPD'
    ENDCASE
  CASE gcAct_Appl = 'PO'
    DO CASE
      CASE lcPrj_typ = 'P'      && PO
        lcProg = 'POSTY'
        lcEvent = 'RESCHDPO'
      CASE lcPrj_typ = 'N'      && Inter-Location PO
        lcProg = 'POINTRC' 
        lcEvent = 'RESCHDPN'    
      CASE lcPrj_typ = 'R'      && Return PO
        lcProg = 'RETPO'    
        lcEvent = 'RESCHDPR'  
    ENDCASE
    lcKey = lcPrj_Typ + lcPrj_ID
  CASE gcAct_Appl = 'SO'
    DO CASE
      CASE lcPrj_typ = 'O'      && SO
        lcProg = 'SOORD'
        lcEvent = 'RESCHDSO'
      CASE lcPrj_typ = 'T'      && EDI Order
        lcProg = 'SOEDORD'  
        lcEvent = 'RESCHDST'    
    ENDCASE
    lcKey = lcPrj_Typ + lcPrj_ID
  
  *E302251,1 AMH Add new project types [Start]
  CASE gcAct_Appl = 'IC'
    DO CASE
      CASE lcPrj_typ = 'S'      && Style
        lcProg  = 'ICSTYLE'
        lcEvent = 'RESCHDSS'
        lcKey   = m.cStyle
      CASE lcPrj_typ = 'H'      && Other
        lcProg  = 'MFPROJ'
        lcEvent = 'RESCHDSH'
        lcKey   = lcPrj_Typ + lcPrj_ID
    ENDCASE
  *E302251,1 AMH [End]
  
ENDCASE


*!*************************************************************
*! Name      : lfPrpAudt
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPrpAudt()
*!*************************************************************
FUNCTION lfPrpAudt

*B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
*IF SEEK(&lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID,'SYSCHDUL') 
IF SEEK(&lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID + &lcTmpDtFl..cStle + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID,'SYSCHDUL') 
*B130984,1 HBG 12/04/2006 [End]
  IF !(&lcTmpDtFl..lOrginal)
    SELECT SYSCHDUL
    lcStauts = SYSCHDUL.COPERSTAT
    *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
    *LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
    *                 &lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID ;
    *                 FOR COPERSTAT <> lcStauts
    LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
                     &lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID + &lcTmpDtFl..cStyle + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID ;
                     FOR COPERSTAT <> lcStauts
    *B130984,1 HBG 12/04/2006 [End]    
    IF !FOUND()
      *B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
      *=SEEK(&lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID+lcStauts,'SYSCHDUL') 
      =SEEK(&lcTmpDtFl..cPrj_Typ + &lcTmpDtFl..cPrj_ID = &lcTmpDtFl..cStyle + &lcTmpDtFl..cOprt_Ctg+&lcTmpDtFl..cOprt_ID+lcStauts,'SYSCHDUL') 
      *B130984,1 HBG 12/04/2006 [End]
    ENDIF                    
  ENDIF
   
  IF SYSCHDUL.lPredComp 
    SELECT (lcTmpDtFl)
    SCATTER MEMVAR MEMO
    ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
    lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,0)
    *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    lcInform = ""
    *HBG [End]
    =lfUpdAdTrl()
    SELECT (lc_PrjAudt)
    *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
    *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
    *  =gfDoTriger('MFPROJ',PADR(lcEvent,10))
    *ENDIF
    DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
    *HBG [End]
  ENDIF 
ELSE
  SELECT (lcTmpDtFl)
  SCATTER MEMVAR MEMO
  ldest_Fnsh = IIF(EMPTY(m.dAct_Fnsh),IIF(EMPTY(m.dClc_Fnsh),m.dest_Fnsh,m.dClc_Fnsh),m.dAct_Fnsh)
  lnrem_dur  = IIF(EMPTY(m.nAct_dur),m.nrem_dur,0)
  *HBG 8/1/2003 Preaper variables to call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
  lcInform = ""
  *HBG [End]
  =lfUpdAdTrl()
  SELECT (lc_PrjAudt)
  *HBG 8/1/2003 Call GFAUDTRL.prg instead of using Syctrigg to update audit trail [Begin]
  *IF ASCAN(laEvntTrig , PADR(lcEvent,10)) <> 0
  *  =gfDoTriger('MFPROJ',PADR(lcEvent,10))
  *ENDIF
  DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
  *HBG [End]
ENDIF   

*!*************************************************************
*! Name      : lfGetHlidy
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetHlidy()
*!*************************************************************
FUNCTION lfGetHlidy

DECLARE laHolidays[1]
laHolidays = ''
lnRows  = 0

IF SEEK(&lc_PMPrjDt..cCal_id,'PMCALDT')
  SELECT PMCALDT
  SCAN
    ldCal_HFrm = dCal_HFrm
    DO WHILE ldCal_HFrm <= dCal_HTo
      lnRows  = lnRows  + 1
      DIMENSION laHolidays[lnRows]
      laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
      ldCal_HFrm = ldCal_HFrm + 1
    ENDDO  
  ENDSCAN
ENDIF
  
IF SEEK(&lc_PMPrjDt..cCal_id,'PMCALHD')
  ldOprtDt = &lc_PMPrjDt..dAct_Strt
  FOR lnI = 1 TO lnDur
     lnWEnd = lnWEnd + IIF(STR(DOW(ldOprtDt),1) $ PMCALHD.cCal_WEnd .OR. ;
                   ASCAN(laHolidays,PMPRJDT.cCal_id+ DTOC(ldOprtDt)) > 0 , 1 , 0)
     ldOprtDt = ldOprtDt + 1                   
  ENDFOR
ENDIF


*!*************************************************************
*! Name      : lfChkPred
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkPred()
*!*************************************************************
FUNCTION lfChkPred

lcCurAls = ALIAS()
lcCurID  = &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
DIMENSION laPred_ID[1]
laPred_ID[1] = ""
SET ORDER TO PMPRJRL IN (lc_PMPrjRl)
lnI = 0
SELECT (lc_PMPrjRl)
SCAN FOR cprj_typ+cprj_id+cstyle+coprt_ctg+coprt_id = ;
                SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
                SUBSTR(lcStyle,1,LEN(cStyle)) + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID
  DIMENSION laPred_ID[lnI+1]
  lnI = ALEN(laPred_ID,1)
  laPred_ID[lnI] = &lc_PMPrjRl..cPrd_Ctg+&lc_PMPrjRl..cPrd_ID
ENDSCAN


lcCurOrd = ORDER(lc_PMPrjDt)
SET ORDER TO PMPRJDT IN (lc_PMPrjDt)
IF !EMPTY(laPred_ID[1])
  FOR lnI = 1 TO ALEN(laPred_ID,1)
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
            SUBSTR(lcStyle,1,LEN(cStyle)) + laPred_ID[1],lc_PMPrjDt) AND EMPTY(&lc_PMPrjDt..dAct_strt)
      llOpen  = .T.
      EXIT
    ENDIF      
  ENDFOR
ENDIF  
=SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID))+;
            SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurID ,lc_PMPrjDt)
SET ORDER TO (lcCurOrd) IN (lc_PMPrjDt)
SELECT (lcCurAls)

SHOW GET EVAL(lcCurFld)
=lfRefresh()

*!*************************************************************
*! Name      : lfChkPred
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfChkPred()
*!*************************************************************
FUNCTION lfChkComp

llComplete = .T.
SELECT (lc_PMPrjDt)
LOCATE
IF EOF()
  llComplete = .F.
ELSE
  SCAN 
    IF EMPTY(dAct_fnsh)
      llComplete = .F.
      EXIT
    ENDIF
  ENDSCAN
ENDIF

IF llComplete
  m.dAct_strt = m.dClc_strt
  m.dAct_Fnsh = m.dClc_Fnsh
  m.cPrj_Stts = 'C'
  puCurStage = AT(m.cPrj_Stts, lcStages)
  lcCurStage = laCurStage[puCurStage, 1]
ENDIF

*!*************************************************************
*! Name      : lfIsHold
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/15/2002
*! Purpose   : Check if the current opertion is hold
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIsHold()
*!*************************************************************
*E301869,2 AMH
FUNCTION lfIsHold

PRIVATE lnAlias,lcOldOrder,llRet
llRet = .F.
lnAlias = SELECT(0)
SELECT SYSCHDUL
lcOldOrder = SET('ORDER')
SET ORDER TO TAG COPRUSR
*B130984,1 HBG 12/04/2006 Add Style code as a new field to SYSCHDUL file and its index [Begin]                  
*IF SEEK(lcPrj_Typ+lcPrj_ID+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID)
*  LOCATE REST WHILE cconttype+cseqnumber+ccont_id+coperstat+cuser_id =;
*                    lcPrj_Typ+lcPrj_ID+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+'O'
IF SEEK(lcPrj_Typ+lcPrj_ID+lcStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID)
  LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
                    lcPrj_Typ+lcPrj_ID+lcStyle+&lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+'O'
*B130984,1 HBG 12/04/2006 [End]
  IF !FOUND()
    llRet = .T.
  ENDIF
ENDIF
SET ORDER TO &lcOldOrder.
SELECT (lnAlias)
RETURN llRet
*--end of lfIsHold.

*!*************************************************************
*! Name      : lfUserBrow
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Browse activation function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfUserBrow()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfUserBrow

lnCurAlias = select(0)
SELECT (lcPMPRJNTF)
lnBrRecNo = RECNO()
BROWSE FIELDS cMarker = IIF(RECNO() = lnBrRecNo, '', ' ');
                         : H = ' '   :R:1 :W = .F.,;
              cUser_ID   : H = 'User':R:10,;
              nBfrStrtDy : H = 'Before Start With':R,;
              lOnStrt    : H = 'On Start':R,;
              nStrtDelay : H = 'On Start Delay Every' :R,;
              lOnRedrct  : H = 'On Redirect':R:10,;
              nBfrCmplDy : H = 'Before Completion With':R,;
              lOnCmplt   : H = 'On Completion':R,;
              nCmplDelay : H = 'On Completion Delay Every' :R;
         WINDOW SMNTFST1 IN WINDOW SMNTFSET;
         WHEN lfwBrow();
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOEDIT;
         NOWAIT;
         SAVE;
         NOCLEAR;
         TITLE (lcBrUser)
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : READ Deactivate function of PRM500.SCX
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Returns            :  .f.
*!*************************************************************
*! Example            :  =lfReadDeact()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfReadDeact

*-- Key traps for the browse
IF WONTOP() = lcBrUser
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
  ON KEY LABEL TAB        DO lpTab WITH "SMNTFST2",OBJNUM(m.cUser_ID)
  ON KEY LABEL BACKTAB    DO lpTab WITH "SMNTFST2",OBJNUM(pbClose)
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfwBrow
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : When function of users browse for notify screen
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  lfwBrow()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfwBrow
PARAMETERS llDontRef

SELECT (lcPMPRJNTF)
lnBrRecNo = RECNO()
SCATTER MEMVAR
=lfNtfRef()

IF !llDontRef
  SHOW WINDOW (lcBrUser) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfNtfRef
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Refresh function for notify screen
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  lfNtfRef()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfNtfRef

IF laScrMode[2] .OR. EOF(lcPMPRJNTF)
  SHOW GET m.cUser_ID   DISABLE
  SHOW GET ibUser_ID    DISABLE
  SHOW GET m.cEmail_Add DISABLE
  SHOW GET m.lBfrStrt   DISABLE
  SHOW GET m.nBfrStrtDy DISABLE
  SHOW GET m.lBfrCmplt  DISABLE
  SHOW GET m.nBfrCmplDy DISABLE
  SHOW GET m.nStrtDelay DISABLE
  SHOW GET m.nCmplDelay DISABLE
  SHOW GET m.lOnStrt    DISABLE
  SHOW GET m.lOnCmplt   DISABLE
  SHOW GET m.lOnRedrct  DISABLE
  SHOW GET pbRemove     DISABLE
ELSE
  SHOW GET m.cUser_ID   ENABLE
  SHOW GET ibUser_ID    ENABLE
  SHOW GET m.cEmail_Add ENABLE
  SHOW GET m.lBfrStrt   ENABLE
  IF m.lBfrStrt
    SHOW GET m.nBfrStrtDy ENABLE
  ELSE
    SHOW GET m.nBfrStrtDy DISABLE
  ENDIF
  SHOW GET m.lBfrCmplt  ENABLE
  IF m.lBfrCmplt
    SHOW GET m.nBfrCmplDy ENABLE
  ELSE
    SHOW GET m.nBfrCmplDy DISABLE
  ENDIF
  SHOW GET m.nStrtDelay ENABLE
  SHOW GET m.nCmplDelay ENABLE
  SHOW GET m.lOnStrt    ENABLE
  SHOW GET m.lOnCmplt   ENABLE
  SHOW GET m.lOnRedrct  ENABLE
  SHOW GET pbRemove     ENABLE
ENDIF

IF laScrMode[2]
  SHOW GET pbNew        DISABLE
ELSE
  SHOW GET pbNew        ENABLE
ENDIF

SHOW WINDOW SMNTFST2 REFRESH

*!*************************************************************
*! Name      : lfvUser_ID
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates User_ID field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvUser_ID()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvUser_ID

IF (PADR(m.cUser_ID,10) = PADR(lcOldVal,10)) .AND. !llBrowse
  RETURN .T.
ENDIF

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

lcOldBrFld = lcBrFields
SELECT SYUUSER
lcBrFields = [cUser_Id : H = 'User ID', cUsr_Name : H = 'User Name']  
IF (!EMPTY(m.cUser_ID) AND !SEEK(PADR(m.cUser_ID,10),'SYUUSER')) .OR. llBrowse
  llBrowse = .F.
  DIMENSION laTemp[2]
  IF ARIABROW('','Users',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,'cUser_Id,cEmail_Add','laTemp')
    m.cUser_ID   = laTemp[1]
    m.cEmail_Add = laTemp[2]
  ELSE
    m.cUser_ID   = lcOldVal
  ENDIF
ELSE
  IF !EMPTY(m.cUser_ID)
    lnTmpRecNo = RECNO(lcPMPRJNTF)
    IF !SEEK(PADR(lcPrj_Typ,1)+PADR(lcPrj_ID,6)+PADR(lcStyle,12)+lcOprt_Ctg+lcoprt_id+m.cUser_ID,lcPMPRJNTF)
      m.cEmail_Add = SYUUSER.CEMAIL_ADD
    ELSE
      WAIT WINDOW "User Exist"
      m.cUser_ID = lcOldVal
    ENDIF
    GO lnTmpRecNo IN (lcPMPRJNTF)
  ENDIF
ENDIF  
lcBrFields = lcOldBrFld

SELECT (lcPMPRJNTF)
REPLACE cUser_ID   WITH m.cUser_ID,;
        cEmail_Add WITH m.cEmail_Add,;
        cStatus    WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
SHOW WINDOW (lcBrUser) REFRESH SAME
glUpdated = .T.
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvNewUser
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates New button of notify screen
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNewUser()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvNewUser

SELECT (lcPMPRJNTF)
APPEND BLANK
REPLACE cStatus   WITH "A",;
        cPrj_Typ  WITH PADR(lcPrj_Typ,1),;
        cPrj_ID   WITH PADR(lcPrj_ID,6),;
        cStyle    WITH PADR(lcStyle,12),;
        COprt_Ctg WITH lcOprt_Ctg,;
        COprt_ID  WITH lcOprt_id
=lfwBrow(.T.)
_CUROBJ = OBJNUM(m.cUser_ID)

*!*************************************************************
*! Name      : lfEsc
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates ESC Key
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfEsc()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfEsc

IF WEXIST("SMNTFST2")
  ACTIVATE WINDOW SMNTFST2
ELSE
  ACTIVATE WINDOW MFPROJA1
ENDIF

_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvEmail
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates cEmail_Add field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvEmail()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvEmail

=lfTmpUpdat("cEmail_Add")

*!*************************************************************
*! Name      : lfTmpUpdat
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Update field on lcPMPCTGNT
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : lcField
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfTmpUpdat()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfTmpUpdat
PARAMETERS lcField,lcValue

SELECT (lcPMPRJNTF)
REPLACE (lcField) WITH EVALUATE("m."+lcField),;
        cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
SHOW WINDOW (lcBrUser) REFRESH SAME
glUpdated = .T.

*!*************************************************************
*! Name      : lfvBfrStrt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lBfrStrt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrStrt()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrStrt

=lfTmpUpdat("lBfrStrt")
IF m.lBfrStrt
  m.nBfrStrtDy = 1
  =lfTmpUpdat("nBfrStrtDy")
  SHOW GET m.nBfrStrtDy ENABLE
ELSE
  m.nBfrStrtDy = 0
  =lfTmpUpdat("nBfrStrtDy")
  SHOW GET m.nBfrStrtDy DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvBfrStDy
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nBfrStrtDy field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrStDy()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrStDy

=lfTmpUpdat("nBfrStrtDy")

*!*************************************************************
*! Name      : lfvBfrCmpl
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lBfrCmplt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrCmpl()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrCmpl

=lfTmpUpdat("lBfrCmplt")
IF m.lBfrCmplt
  m.nBfrCmplDy = 1
  =lfTmpUpdat("nBfrCmplDy")
  SHOW GET m.nBfrCmplDy ENABLE
ELSE
  m.nBfrCmplDy = 0
  =lfTmpUpdat("nBfrCmplDy")
  SHOW GET m.nBfrCmplDy DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvBfrCmDy
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nBfrCmplDy field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrCmDy()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrCmDy

=lfTmpUpdat("nBfrCmplDy")

*!*************************************************************
*! Name      : lfvStDelay
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nStDelay field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvStDelay()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvStDelay

=lfTmpUpdat("nStrtDelay")

*!*************************************************************
*! Name      : lfvCmDelay
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nCmplDelay field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvCmDelay()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvCmDelay

=lfTmpUpdat("nCmplDelay")

*!*************************************************************
*! Name      : lfvOnStrt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lOnStrt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOnStrt()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvOnStrt

=lfTmpUpdat("lOnStrt")

*!*************************************************************
*! Name      : lfvOnCmplt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lOnCmplt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOnCmplt()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvOnCmplt

=lfTmpUpdat("lOnCmplt")

*!*************************************************************
*! Name      : lfvOnRdrct
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lOnRedrct field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOnRdrct()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvOnRdrct

=lfTmpUpdat("lOnRedrct")

*!*************************************************************
*! Name      : lfvRemUser
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates Remove button of notify screen
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvRemUser()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvRemUser

lnCurAlias = SELECT(0)

IF gfModalgen("TRM38208B38006","DIALOG") = 1
  SELECT (lcPMPRJNTF)
  REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
  DELETE
  LOCATE
  glUpdated   = .T.
  =lfwBrow()
ENDIF
SELECT (lnCurAlias)