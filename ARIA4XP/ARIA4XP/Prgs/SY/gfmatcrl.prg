*:**************************************************************************
*: Program file  : gfMatCrl.prg
*: Program desc. : Material Stock Control Function,
*:                 Material Average Cost Update and Inventory Journal Update.
*:        System : ARIA APPAREL SYSTEM 4.0
*:        Module : Material (MA).
*:     Developer : AHM - Ahmed Amer
*:          Date : 11/23/98
*:**************************************************************************
*: Calls Functions    : lfJrRoData(), lfIsuJlTr(), lfUpdGLDist()
*:**************************************************************************
*: Passed Parameters  :
*:   lcTrType   =>  Transaction Type.
*:             '1' for Receive Material PO,Receive Return Material PO  (+/-) 
*:             '2' for Material Adjustement(Rec,Iss)    (+/-)
*:             '3' for Physical material inventory      (+/-)
*:             '4' for PO,CT Cost sheet                 (+/-)
*:             '5' for Material Invoice & Void Material Invoice
*:   lcFabric    =>  Fabric.
*:   lcColor     =>  Color.
*:   lcWareCode  =>  Warehouse Code.
*:   lcFDyelot   =>  Dyelot ,pass empty if not a dyelot fabric or system
*:                   dyelot No, (if you want to add this dyelot you have to 
*:                   add it before calling this function).
*:   ldTrDate   =>  Transaction Date.
*:   lcTrCode   =>  Transaction Code as ex. Rec PO No.
*:   lnAdjStk   =>  Issued or received Stock.
*:   lnNewCost  =>  New receiving cost ,Pass it Zero if it is issue transaction
*:                  and not PO/CT, in PO/CT it should be passed either rec. or iss.
*:   lcRefer    =>  Transaction Reference.  
*:   lcAdjCdRsn =>  Adjustment code reason.
*:   lnStarStep =>  Starting count for uncomplete session steps
*:                  You have to continue checking steps after the function
*:                  by the return value of this function.
*:                  put zero if it is no need to uncomplete session.
*:   lcTmpLFile =>  Temp line file name that has field for counting steps.
*:   lcStepFld  =>  Name of the field that you need to count steps for,
*:                  as example 'nSteps'.
*: laGLInvAry[X,13]=> Array that holds all GL entry information needed for 
*:                    Inventory control or Anventory adjustment.
*:                    Put the array elements as follows :
*:              [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:              [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:              [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:              [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*:                  If no G/L is used pass this array as len 1 and .F.
*:                  Pass Category type '006' as a first array element.
*: lcFJlSess => Variable that hold the session No ,if it is empty or not
*:              passed Session No will be generated
*:**************************************************************************
*: Note    : Files ->Fabric,FabDye,Temprory Line file and 
*:           Temprorary G/L Distribution files must be
*:           opened before calling this function.
*:**************************************************************************
*: Update  : Fabric=> (Fabric  rec.) OnHand,nStkVal,nFAve_Cost,nAveCstBuy
*:           FabDye=> (Fab/War rec.) OnHand,nStkVal,nFAve_Cost,nAveCstBuy
*:           FabDye=> (Fab/Dye rec.) OnHand,nStkVal,nFAve_Cost,nAveCstBuy
*:           MatInvJl=> Create a new record.
*:           TmpGLDist=> Create a new records for GL inventory entres.
*:**************************************************************************
*: Returns :
*: RETURN 0 Zero  If No updates was done. 
*: RETURN 1 One   If all files was succesfuly updated and 
*:                No uncomplete session checks was used in this function. 
*:**************************************************************************
*:****************************************************************************
*FUNCTION gfmatcrl
PARAMETERS lcTrType,lcFabric,lcColor,lcWareCode,lcFDyelot,;
           ldTrDate,ldPostDate,lcTrCode,lnAdjStk,lnNewCost,lcRefer,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lcFJlSess,;
           lcCShetTyp,lcPONo,lcCTRSess,lcCTISess,lcRetPO,lcToWare,lcLastRSess,laOtherPar

IF TYPE('laOtherPar') # "L"
  FOR lnI = 1 TO ALEN(laOtherPar,1) 
    &laOtherPar[lnI,1] = laOtherPar[lnI,2]
  ENDFOR
ENDIF  

IF TYPE('lcOrgFab') # 'C'
  lcOrgFab = SPACE(0)
ENDIF
IF TYPE('lcOrgClr') # 'C'
  lcOrgClr = SPACE(0)
ENDIF
IF TYPE('lcOrgDye') # 'C'
  lcOrgDye = SPACE(0)
ENDIF
IF TYPE('lnLineNo') $ 'LU'
  lnLineNo = 0
ENDIF  
IF TYPE('llUseACst') = 'U'
  llUseACst = .F.
ENDIF
IF TYPE('llLastRec') = 'U'
  llLastRec = .T.
ENDIF
IF TYPE('lcPrvWare') # 'C'
  lcPrvWare = SPACE(0)
ENDIF
IF TYPE('lnMarkQty') $ 'LU'
  lnMarkQty = 0
ENDIF
IF TYPE('lnMarkVal') $ 'LU'
  lnMarkVal = 0
ENDIF
IF TYPE('llAdjYes') # 'L'
  llAdjYes = .F.
ENDIF

PRIVATE lnStkVal,lnCost
STORE 0 TO lnStkVal,lnCost

PRIVATE lcLastRSess
IF TYPE('lcLastRSess') $ 'UL'
  lcLastRSess = SPACE(6)
ENDIF  
IF TYPE('lcToWare') $ "UL"
  lcToWare = ''
ENDIF  

*--Initialize function variables.
PRIVATE lcOldWAr,lnSAveCost,lnFOldStk,lnFOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,llUInvtry,lnFStkVal,lnWStkVal,lnCurRec,;
        lcFJlSess,llTrkRolls,lcCShetTyp,lcPONo,llWareHous,lcCTRSess,lcCTISess

lcVoucNo = IIF(TYPE('lcVoucNo') = 'U' ,"",lcVoucNo)
llWareHous = gfGetMemVar('M_WareHouse')='Y'
IF TYPE('llExtCall')='U'
  PRIVATE lcTmpJour,lcTmpRoll,lcFullRoll
  STORE SPACE(0) TO lcTmpJour,lcTmpRoll,lcFullRoll
ENDIF
llTrkRolls = ALLTRIM(gfGetMemVar('M_TrkRolls')) = 'Y'
IF TYPE('lcFJlSess') = 'L'
  lcFJlSess = ''
ENDIF
llGenRolId  = ('MA' $ oAriaApplication.CompanyInstalledModules AND ALLTRIM(gfGetMemVar('M_GENROLID')) = 'N')

*--Fabric and Warehouse Average Cost,Old Stock and Old Cost variables.
STORE 0 TO lnFOldStk,lnFOldCst
STORE 0 TO lnWOldStk,lnWOldCst
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.  
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcFDyelot  = IIF(EMPTY(lcFDyelot),SPACE(10),lcFDyelot)
llDyeLvl   = !EMPTY(lcFDyelot)

*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,!EMPTY(laGLInvAry[1,1]))

*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_MatCstMth')
*--Check the existing of the Fabric and
*--Point the record in Fabric and Fabric dyelot files.
llFabWarDy = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot,'FabDye')
llFabWar   = SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FabDye')
IF !SEEK(lcFabric+lcColor,'Fabric') .OR. !llFabWar .OR. !llFabWarDy
  *--The material/Color ???? record are missing,Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  lcMsgExp = lcFabric + "/" + lcColor
  =gfModalGen('TRM36133B00036','DIALOG',lcMsgExp)
  RETURN (0)
ENDIF
*--Check if MatInvJL file is Open.
llOpnJurnl=gfOpenFile(oAriaApplication.DataDir+"MatInvJl","MatInvJl","SH")

*--Get the Old Stock and Cost before updateing the new tansaction.
lnFOldStk = Fabric.OnHand
lnFOldCst = ABS(IIF(Fabric.OnHAnd=0,Fabric.nfAve_Cost,Fabric.nStkVal / Fabric.OnHand))
lnFStkVal = Fabric.nStkVal

IF lcTrType = '9'
  lnWOldStk = lnMarkQty
  lnWOldCst = ABS(IIF(lnMarkQty=0,FabDye.nfAve_Cost,lnMarkVal / lnMarkQty))
ELSE
  lnWOldStk = FabDye.OnHand
  lnWOldCst = ABS(IIF(FabDye.OnHand=0,FabDye.nfAve_Cost,FabDye.nStkVal / FabDye.OnHand))
ENDIF

IF TYPE('lcProg') $ "UL"
  lcProg = SPACE(10)
ENDIF
IF lcTrType = '1'
  DO CASE
    CASE lcProg = 'MARECI'
      PRIVATE lcPoFTag,lcKey,lnAlias,lnPoLin,lnRecLin,lnOldOnOrd
      lnAlias = SELECT(0)
      SELECT PoFLn
      lcKey = cMatType+POMat+Fabric+Color
      lcPoFTag = ORDER('PoFLn')
      SET ORDER TO Pofln
      lnPoLin = 0
      lnRecLin = 0
      lnOldOnOrd = 0
      IF SEEK(lcKey)
        SCAN REST WHILE cMatType+POMat+Fabric+Color+TRANCD = lcKey
          IF TranCd = '1'
            lnPoLin = PoFLn.nFabTotQty * Fabric.Conv
          ELSE
            IF TranCd $ "234"
              lnRecLin = lnRecLin + (PoFLn.nFabTotQty * Fabric.Conv)
            ENDIF
          ENDIF
        ENDSCAN 
        lnOldOnOrd = lnPoLin - lnRecLin
        =SEEK(lcKey)
      ENDIF
      IF lnOldOnOrd = 0
        lnOldOnOrd = lnAdjStk
      ENDIF
      SET ORDER TO lcPoFTag
      SELECT(lnAlias)
    CASE lcProg = 'MAMNREC'
      *-- Collect Data from mmfgordh and mmfgordd
      PRIVATE lcMFGTag,lcMFKey,lnAlias,lnMFGLin,lnRecLin,lnOldOnOrd
      *-- Save envionment setings
      lnAlias = SELECT(0)
      *--Issue and receive the material manufacturing Rework order from MAMNREC program & I will
      *--Not update the On order Qty.
      *-- IF we will issue and recive rework order.
      IF TYPE('llRwIssRec') = 'L' .AND. !llRwIssRec
      
        SELECT MMFGORDD
        *-- Compose the key that will be used in the search
        lcMFKey = lcTrCode + ;
                IIF(EMPTY(lcOrgFab) , lcFabric  , lcOrgFab) +;
                IIF(EMPTY(lcOrgClr) , lcColor   , lcOrgClr) +;
                IIF(EMPTY(lcOrgDye) , lcFDyelot , lcOrgDye)
              
        lcMFGTag   = ORDER('MMFGORDD')
        lnMFGLin   = 0
        lnRecLin   = 0
        lnOldOnOrd = 0
        SET ORDER TO Mmfgordd
        IF SEEK(lcMFKey)
          SCAN REST WHILE cmfgordno+cfabric+COLOR+dyelot = lcMFKey
            IF TranCd = '1'
              lnMFGLin = NMFGTOTQTY * Fabric.Conv
            ELSE
              IF TranCd $ "234"
                lnRecLin = lnRecLin + (MMFGORDD.NMFGTOTQTY * Fabric.Conv)
              ENDIF
            ENDIF
          ENDSCAN
          lnOldOnOrd = lnMFGLin - lnRecLin
          =SEEK(lcKey)
        ENDIF
      ELSE
        SELECT (lcBfileLin)
        *-- Compose the key that will be used in the search
        lcMFKey = lcTrCode + ;
                IIF(EMPTY(lcOrgFab) , lcFabric  , lcOrgFab) +;
                IIF(EMPTY(lcOrgClr) , lcColor   , lcOrgClr) +;
                IIF(EMPTY(lcOrgDye) , lcFDyelot , lcOrgDye)
              
        lcMFGTag   = ORDER(lcBfileLin)
        lnMFGLin   = 0
        lnRecLin   = 0
        lnOldOnOrd = 0
        SET ORDER TO MARWLIN
        IF SEEK(lcMFKey)
          SCAN REST WHILE cmfgrwnum+cfabric+color+dyelot+trancd = lcMFKey
            IF TranCd = '2'
              lnMFGLin = MARWLIN.NMFGTOTQTY * Fabric.Conv
            ELSE
              IF TranCd $ "346"
                lnRecLin = lnRecLin + (MARWLIN.NMFGTOTQTY * Fabric.Conv)
              ENDIF
            ENDIF
          ENDSCAN
          lnOldOnOrd = lnMFGLin - lnRecLin
          =SEEK(lcKey)
        ENDIF
      ENDIF
     
    OTHERWISE
      lnOldOnOrd = 0
  ENDCASE
ENDIF

lnWStkVal = IIF(lcTrType='9',lnMarkVal,FabDye.nStkVal)
PRIVATE lnDyeCost
IF lcTrType = '9'
  lnDyeCost = IIF(lnMarkQty=0,FabDye.nFAve_Cost,lnMarkVal/lnMarkQty)
ELSE
  lnDyeCost = IIF(FabDye.OnHand = 0,FabDye.nFAve_Cost,FabDye.nStkVal/FabDye.OnHand)
ENDIF  

PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = FabDye.OnHand
lnPrvVal = FabDye.nStkVal
IF !EMPTY(lcFDyelot)
  =SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot,'FabDye')
  lnPrvQty = FabDye.OnHAnd
  lnPrvVal = FabDye.OnHAnd * lnDyeCost
  lnWStkVal = IIF(lcTrType='9',lnMarkVal,lnPrvVal)
ENDIF

*-- Temp Old Stock variable used in Physical issue the old stock first and
*-- then receive the pysical quantity.
*-- In all other cases this variable is Zero.
lnTmpOStk = 0
IF lcTrType = '3'
  SELECT FabDye
  IF !EMPTY(lcFDyelot)
    = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot,'FabDye')
  ENDIF
  lnTmpOStk = OnHand
ENDIF

IF lcTrType = '9'
  lnTmpOStk = lnMarkQty
ENDIF  

*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
lcIRType = IIF(lnAdjStk<0 , 'I' , 'R' )

*--Calculate Transaction Issuing Cost in differend cost methods.
IF lcIRType = 'I'
  DO CASE
    CASE lcCostMeth = 'A'   && Average.
      IF !llUseACst
        lnNewCost = IIF(llWareHous,lnWOldCst,lnFOldCst)
      ENDIF

    CASE lcCostMeth = 'S'   && Standard.
      IF !llUseACst
        lnNewCost = Fabric.CostUse
      ENDIF

    CASE lcCostMeth $ 'FIL' AND lcTrType <> '4'
      IF !(lcTrType $ '15' AND lnAdjStk < 0 AND TYPE('llExtCall')='L' AND llExtCall)
           
        IF !lfJrRoData()
          SELECT (lcOldWAr)
          RETURN (0)
        ELSE
          IF !EMPTY(lcToWare)
            PRIVATE lcOrgWare
            lcOrgWare = lcWareCode
            lcWareCode = lcToWare
            lnAdjStk = ABS(lnAdjStk)
            lcIRType = 'R'
            IF !lfJrRoData()
              SELECT (lcOldWAr)
              RETURN (0)
            ENDIF
            lcWareCode = lcOrgWare
            lnAdjStk = - lnAdjStk
            lcIRType = 'I'
          ENDIF
        ENDIF
      ENDIF

    CASE lcCostMeth $ 'FIL' AND lcTrType = '4'
      *--Receiving cost will be passed as parameter in this function.
  ENDCASE
ELSE
  
  IF lcTrType <> '4'
    IF (TYPE('llExtCall')='U' OR !llExtCall) AND !lfJrRoData()
      RETURN (0)
    ENDIF
  ELSE
    *--Receiving cost will be passed as parameter in this function.
  ENDIF
ENDIF



*--  Update Material journal file. -------------------------------------
*--Read session no.
IF lcTrType <> '4'
  IF lcIRType = 'R' AND lcCostMeth $ 'LFI'
    lnCurRec = RECNO('MatInvJl')
    IF SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+lcFJlSess+SPACE(6),"MatInvJL")
      *--We should Select the MatInvJL before locate on it , this will
      *--Make a bug in case we make phisical Inv.  [Begin]
      Private lnOldAlias
      lnOldAlias = SELECT(0)
      SELECT MatInvJL
      LOCATE REST WHILE cFabric +cColor +cWareCode +cDyelot  +cRSession+cISession = ;
                        lcFabric+lcColor+lcWareCode+lcFDyelot+lcFJlSess+SPACE(6)    ;
                  FOR   LineNo = lnLineNo
      IF FOUND()            
        lcFJlSess = gfSEQUENCE('GLSession')
      ENDIF
      SELECT (lnOldAlias)
    ENDIF

    IF RECCOUNT('MatInvJl')>=lnCurRec
      GO lnCurRec IN MatInvJl
    ENDIF
  ENDIF
  IF EMPTY(lcFJlSess) 
    lcFJlSess = gfSequence('GLSESSION')
  ENDIF
ENDIF

IF lcTrType = '4' AND EMPTY(lcCTRSess)
  lcCTRSess = gfSequence('GLSESSION')
ENDIF  

*--Read the adjustment code reason to get the GL Account.
IF !EMPTY(lcAdjCdRsn)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjCdRsn , @laTrmRltFd , "CADJREASON")
ENDIF

*--Update journal for Issue Transaction ,FIFO or LIFO method.
IF lcIRType = 'I'

  = lfIssue()

ELSE  && Rec. with in any costing or issu in "S,A"

  = lfReceive()

ENDIF

*-- 5) Update Rolls File
IF !(lcTrType $ '45') .OR. (lcTrType = '5' .AND. lcIRType = 'I')

  IF (llTrkRolls .AND. Fabric.ltrkrolls)
  
      IF lcTrType = '3'
        = lfUpdtRoll(.T.)
      ENDIF

      = lfUpdtRoll()
      
  ENDIF
ENDIF

IF TYPE('llExtCall') = 'U' OR !llExtCall OR llLastRec
  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (oAriaApplication.WorkDir+lcTmpJour+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcTmpJour+'.CDX')

  IF USED(lcTmpRoll)
    USE IN (lcTmpRoll)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (oAriaApplication.WorkDir+lcTmpRoll+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcTmpRoll+'.CDX')

  IF USED(lcFullRoll)
    USE IN (lcFullRoll)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (oAriaApplication.WorkDir+lcFullRoll+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcFullRoll+'.CDX')

ENDIF

SELECT (lcOldWAr)
RETURN 1



*!*************************************************************
*! Name      : lfUpdtRoll
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Update rolls file
*! Note      : 
*!*************************************************************
*! Parameter : If it calls for Physical issue.
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  =lfUpdtRoll()
*!*************************************************************

FUNCTION lfUpdtRoll
PARAMETERS llPhyIssu
PRIVATE lcRoTmpUse,llPhyIssu,lnTrQty

lcRoTmpUse = IIF((lnAdjStk<0 OR llPhyIssu) .AND. lcTrType # '5',lcFullRoll,lcTmpRoll)

*-- add new records in the Rolls file from lcTmpRolls
PRIVATE lnAlias,lcRolTag
lnAlias = SELECT(0)
lcRolTag = ORDER('Rolls')
SET ORDER TO TAG ROLLITEM IN Rolls
SELECT (lcRoTmpUse)
GO TOP
SCAN FOR cfabric+ccolor+cwarecode+cdyelot = lcFabric+lcColor+lcWareCode+lcFDyelot
  IF !llPhyIssu AND SEEK(&lcRoTmpUse..cFabric+&lcRoTmpUse..cColor+;
                         &lcRoTmpUse..cWareCode+&lcRoTmpUse..cDyelot+;
                         &lcRoTmpUse..cRollId+'1','ROLLS')
    SELECT ROLLS
    IF lcTrType='3'
      REPLACE nQtyBal WITH ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='1',Fabric.Conv,1)
    ELSE
      *-- lcRoTmpUse is either "lcTmpRoll" or "lcFullRoll"
      *-- IF it is receiving lcRoTmpUse is "lcTmpRoll"
      *-- IF it is issuing   lcRoTmpUse is "lcFullRoll"
      *-- Variable to hold the applied qty
      PRIVATE lnRToApply
      IF (&lcRoTmpUse..RolTranCd $ '13') OR;
                     (&lcRoTmpUse..RolTranCd = '2' AND lcTrType = '2' AND lnAdjStk > 0)
        IF (&lcRoTmpUse..RolTranCd $ '13')
          lnRToApply = ABS(&lcRoTmpUse..nApply)
        ELSE
          lnRToApply = ABS(&lcRoTmpUse..nApply) - nQTYBal
        ENDIF
      ELSE
        lnRToApply = &lcRoTmpUse..nApply * (-1)
      ENDIF

      *- To issue The Qty From the Atock with sell Conv.[Begin]
      IF lcTrType='5'
        lnRToApply = lnRToApply * m.nsellConv
      ENDIF
     
      REPLACE nQtyBal WITH nQtyBal + lnRToApply
    ENDIF
  ENDIF

  lnTrQty = ABS(IIF(llPhyIssu,&lcRoTmpUse..nBalance,&lcRoTmpUse..nApply))
  IF lnTrQty > 0

    *WAB - replace the roll Id in the temp file with the sequence no cause in case of 
    *WAB - (START)recieving po we create a temp. sequence no cause we used a temp file in this case
    *ABD - Fix bug that when return from exist roll don't generate sequence no. [Begin]
    IF lcTrType = '1' .AND. llGenRolId .AND. lcKeyType  = 'P'
      REPLACE &lcRoTmpUse..cRollId WITH gfSequence('CROLLID')
    ENDIF
    
    SELECT ROLLS
    *-- Check if we update the rolls file with the same recored before that
    *-- I shouldn't update the rolls file again , this case happen while receive the same 
    *-- Issue return 2 Po for the same color fabric.
    
    PRIVATE lcOldOrder
    lcOldOrder = ORDER()
    *-- crsession+cisession+crollitem+color+cwarecode+dyelot+crollid
    SET ORDER TO ROLAPL
    IF lcTrType # '5' .OR. !SEEK(&lcTmpRoll..cRSession +lcFJlSess +&lcTmpRoll..cFabric + ;
             &lcTmpRoll..cColor +&lcTmpRoll..cWarecode +&lcTmpRoll..cDyelot +&lcTmpRoll..cRollid)
      APPEND BLANK
    ENDIF
    REPLACE cRollItem WITH &lcRoTmpUse..cFabric    ,;
            Color     WITH &lcRoTmpUse..cColor     ,;
            cWareCode WITH &lcRoTmpUse..cWareCode  ,;
            Dyelot    WITH &lcRoTmpUse..cDyelot    ,;
            cRollId   WITH &lcRoTmpUse..cRollId    ,;
            nQty      WITH nQty + (lnTrQty * IIF(lcTrType$ '15',IIF(lcTrType = '1',Fabric.Conv,m.nSellConv),1))   ,;
            nQtyBal   WITH nQtyBal + (lnTrQty * IIF(lcTrType$ '15',IIF(lcTrType = '1',Fabric.Conv,m.nSellConv),1)),;
            TranCd    WITH IIF(llPhyIssu,'2',&lcRoTmpUse..RolTranCd)          ,;
            cSession  WITH lcFJlSess                                          ,;
            cRSession WITH IIF(TranCd $ '13',lcFJlSess,&lcRoTmpUse..cRSession),;
            cISession WITH IIF(TranCd = '2' ,lcFJlSess,cISession)
    =gfAdd_Info('ROLLS')
    SET ORDER TO &lcOldOrder
  ENDIF
ENDSCAN

IF !EMPTY(lcToWare)
  SELECT (lcTmpRoll)
  SCAN FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcToWare+lcFDyelot+'??????',;
                cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession)
    SELECT Rolls
    IF SEEK(&lcTmpRoll..cFabric+&lcTmpRoll..cColor+&lcTmpRoll..cWareCode+;
            &lcTmpRoll..cDyelot+&lcTmpRoll..cRollId+'1')
      REPLACE nQtyBal WITH nQtyBal + (ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='1',Fabric.Conv,1))
    ENDIF
    IF &lcTmpRoll..nApply > 0
      APPEND BLANK
      llUptRcRls = .F.
      IF ASCAN(oAriaApplication.laEvntTrig,PADR("UPMATRLS",10)) <> 0 AND lcCostMeth = 'L'
        llUptRcRls = .T.
        =gfDoTriger('MAINVCT',PADR('UPMATRLS',10))
      ENDIF   
      IF !llUptRcRls
        REPLACE cRollItem WITH &lcTmpRoll..cFabric    ,;
                Color     WITH &lcTmpRoll..cColor     ,;
                cWareCode WITH &lcTmpRoll..cWareCode  ,;
                Dyelot    WITH &lcTmpRoll..cDyelot    ,;
                cRollId   WITH &lcTmpRoll..cRollId    ,;
                nQty      WITH &lcTmpRoll..nApply * IIF(lcTrType='1',Fabric.Conv,1),;
                nQtyBal   WITH &lcTmpRoll..nApply * IIF(lcTrType='1',Fabric.Conv,1),;
                TranCd    WITH &lcTmpRoll..RolTranCd  ,;
                cSession  WITH lcFJlSess              ,;
                cRSession WITH lcFJlSess
        =gfAdd_Info('ROLLS')
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

SET ORDER TO (lcRolTag) IN Rolls
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfIssue
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Update MatInvJl file for issue record
*! Note      : 
*!*************************************************************
*! Parameter : 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  =lfIssue()
*!*************************************************************

FUNCTION lfIssue

PRIVATE lcReIsFld
IF lcIRType = 'I' AND lcTrType $ '15'
  lnStkVal = -1* (lnAdjStk * lnNewCost)
  lnCost   = lnNewCost
ENDIF
lcReIsFld = IIF(lnWOldStk<0 AND lcTrType $ '93',"nReceived","nIssued")
IF (lcCostMeth $ 'FIL' AND lcTrType <> '4') AND lnWOldStk <> 0
  SELECT (lcTmpJour)
  SCAN FOR cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession = ;
           lcFabric+lcColor+lcWareCode+lcFDyelot
      SELECT MatInvJl
      APPEND BLANK

      REPLACE cFabric    WITH lcFabric   ,;
              cColor     WITH lcColor    ,;
              cWareCode  WITH lcWareCode ,;
              cDyelot    WITH lcFDyelot  ,;
              Reference  WITH lcRefer    ,;
              cAdjReason WITH lcAdjCdRsn ,;
              cGlMatAdj  WITH lcAdjAcct  ,;
              dTranDate  WITH ldTrDate   ,;
              dPostDate  WITH ldPostDate ,;
              cTranType  WITH lcTrType   ,;
              cTran      WITH IIF(cTranType $ "239" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
              nUnitCost  WITH IIF(lcTrType='5',&lcTmpJour..nUnitCost/m.nSellconv,;
                              IIF(lcTrType='1',&lcTmpJour..nUnitCost/Fabric.Conv,&lcTmpJour..nUnitCost)),;
              nUntCstBuy WITH &lcTmpJour..nUntCstBuy     ,;
              cRSession  WITH &lcTmpJour..cRSession      ,;
              cISession  WITH IIF(lcTrType='4',lcCTISess,lcFJlSess)

      REPLACE &lcReIsFld WITH &lcReIsFld + IIF(lcTrType='3',ABS(&lcTmpJour..nBalance)         ,;
              IIF(lcTrType='5',ABS(&lcTmpJour..nApply) * m.nSellconv,;
              IIF(lcTrType='1',ABS(&lcTmpJour..nApply)*Fabric.Conv,ABS(&lcTmpJour..nApply)))),;
              nStkVal    WITH IIF(lcTrType='5',- ABS(&lcTmpJour..nApply) * nUnitCost * m.nSellconv ,;
                                  - &lcReIsFld * nUnitCost)                ,;
              dPostDate  WITH ldPostDate                                   ,; 
              cTrn_Seq   WITH IIF(EMPTY(cISession),cRSession,cISession)    ,;
              cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
              cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo)            ,;
              nMPrvSQty  WITH lnPrvQty                                     ,;
              nPrvSVal   WITH lnPrvVal                                     ,;
              llockflg   WITH lcTrType='9'                                 ,;
              LineNo     WITH lnLineNo
      *AMM Update the operation code and lot #
      IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
        REPLACE cOprCode  WITH lcOprCode ,;
                cLotNo    WITH lcLotNo
      ENDIF

      * Update voucher no variable for CON10.
      IF ASCAN(oAriaApplication.laEvntTrig,PADR("GETVOUT",10)) <> 0
         REPLACE cvoucherno WITH lcVoucNo
      ENDIF   
      *

      *-- Call global function to add audit fields info.
      =gfAdd_Info('MATINVJL')

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfFabWarDy()
  ENDSCAN
  
  IF !EMPTY(lcToWare)
    SELECT (lcTmpJour)
    SCAN
        SELECT MatInvJl
        APPEND BLANK
        llUptRcMat = .F.
        IF ASCAN(oAriaApplication.laEvntTrig,PADR("UPMATINV",10)) <> 0 .AND. lcCostMeth = 'L'
          llUptRcMat = .T.
          =gfDoTriger('MAINVCT',PADR('UPMATINV',10))
        ENDIF   
        IF !llUptRcMat
           REPLACE cFabric    WITH lcFabric   ,;
                   cColor     WITH lcColor    ,;
                   cWareCode  WITH lcToWare   ,;
                   cDyelot    WITH lcFDyelot  ,;
                   Reference  WITH lcRefer    ,;
                   cAdjReason WITH lcAdjCdRsn ,;
                   cGlMatAdj  WITH lcAdjAcct  ,;
                   dTranDate  WITH ldTrDate   ,;
                   dPostDate  WITH ldPostDate ,;
                   cTranType  WITH lcTrType   ,;
                   cTran      WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
                   nUnitCost  WITH &lcTmpJour..nUnitCost      ,;
                   nUntCstBuy WITH &lcTmpJour..nUntCstBuy     ,;
                   cRSession  WITH gfSEQUENCE('GLSession')    ,;
                   nReceived  WITH ABS(&lcTmpJour..nApply)    ,;
                   nStkVal    WITH nReceived * nUnitCost      ,;
                   dPostDate  WITH ldPostDate                 ,; 
                   cTrn_Seq   WITH cRSession                  ,;
                   nMPrvSQty WITH lnPrvQty                    ,;
                   nPrvSVal  WITH lnPrvVal 
         ENDIF
        IF TYPE('lcLotNo') # 'U'  .AND. TYPE('lcOprCode') # 'U'
          REPLACE cOprCode  WITH lcOprCode ,;
                  cLotNo    WITH lcLotNo
        ENDIF

        *Update voucher no variable for CON10.
        IF ASCAN(oAriaApplication.laEvntTrig,PADR("GETVOUT",10)) <> 0
          REPLACE cvoucherno WITH lcVoucNo
        ENDIF   
        *-- Call global function to add audit fields info.
        =gfAdd_Info('MATINVJL')


      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfFabWarDy(.T.)
    ENDSCAN

  ENDIF
  
ELSE
    SELECT MatInvJl
    APPEND BLANK

    REPLACE cFabric    WITH lcFabric ,;
            cColor     WITH lcColor  ,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcFDyelot ,;
            dTranDate  WITH lDTrDate  ,;
            dPostDate  WITH ldPostDate,;
            cTranType  WITH lcTrType  ,;
            cTran      WITH IIF(cTranType $ "239" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            nUnitCost  WITH IIF(lcTrType='1',lnCost,IIF(lcTrType $ '39',lnWOldCst,lnNewCost)),;
            nUntCstBuy WITH IIF(lcTrType='1',lnCost,IIF(lcTrType $ '39',lnWOldCst,lnNewCost)) * Fabric.Conv,;
            &lcReIsFld WITH IIF(lcTrType $ '39',ABS(lnTmpOStk),ABS(lnAdjStk)),;
            nStkVal    WITH - IIF(lcTrType='1',lnStkVal,IIF(lcTrType$'39',lnWStkVal,&lcReIsFld * nUnitCost)) ,;
            Reference  WITH lcRefer   ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct

    REPLACE cISession  WITH IIF(lcTrType = '4',lcCTISess,IIF(lcTrType $ '39',IIF(lnWOldStk>0,lcFJlSess,''),lcFJlSess)),;
            cRSession  WITH IIF(lcTrType = '4',lcCTRSess,IIF(lcTrType $ '39',IIF(lnWOldStk<0,lcFJlSess,''),'')),;
            cTrn_Seq   WITH IIF(lcTrType $ '39',lcFJlSess,cISession) ,;
            cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
            cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
            nMPrvSQty  WITH lnPrvQty ,;
            nPrvSVal   WITH lnPrvVal,;
            llockflg   WITH lcTrType='9'

      IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
        REPLACE cOprCode  WITH lcOprCode ,;
                cLotNo    WITH lcLotNo
      ENDIF

      *C200255,1 (Begin) Update voucher no variable for CON10.
      IF ASCAN(oAriaApplication.laEvntTrig,PADR("GETVOUT",10)) <> 0
         REPLACE cvoucherno WITH lcVoucNo
      ENDIF   
      *C200255,1 (End)

    *-- Call global function to add audit fields info.
    =gfAdd_Info('MatInvJl')


  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()

  *ABD - Call function to Update the Material inv. Journal with  issue 
  *ABD - Recored in case we lock the item. [Begin]
  IF lcIRType = 'I' .AND. lcTrType  = '9'
    = lfRcvLock ()
  ENDIF

  IF !EMPTY(lcToWare)
      SELECT MatInvJl
      APPEND BLANK
      REPLACE cFabric    WITH lcFabric  ,;
              cColor     WITH lcColor   ,;
              cWareCode  WITH lcToWare  ,;
              cDyelot    WITH lcFDyelot ,;
              dTranDate  WITH ldTrDate  ,;
              dPostDate  WITH ldPostDate,;
              cTranType  WITH lcTrType  ,;
              cTran      WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
              nUnitCost  WITH lnNewCost             ,;
              nUntCstBuy WITH lnNewCost*Fabric.Conv ,;
              nReceived  WITH ABS(lnAdjStk)         ,;
              nStkVal    WITH nReceived * nUnitCost ,;
              Reference  WITH lcRefer    ,;
              cAdjReason WITH lcAdjCdRsn ,;
              cGlMatAdj  WITH lcAdjAcct  ,;
              cRSession  WITH lcFJlSess  ,;
              cTrn_Seq   WITH cISession  ,;
              nMPrvSQty  WITH lnPrvQty   ,;
              nPrvSVal   WITH lnPrvVal 

      *AMM Update the operation code and lot #
      IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
        REPLACE cOprCode  WITH lcOprCode ,;
                cLotNo    WITH lcLotNo
      ENDIF

      *C200255,1 (Begin) Update voucher no variable for CON10.
      IF ASCAN(oAriaApplication.laEvntTrig,PADR("GETVOUT",10)) <> 0
         REPLACE cvoucherno WITH lcVoucNo
      ENDIF   
      *C200255,1 (End)

      *-- Call global function to add audit fields info.
      =gfAdd_Info('MatInvJl')


    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfFabWarDy(.T.)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfReceive
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Update MatInvJl file for receive record
*! Note      : 
*!*************************************************************
*! Parameter : 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  =lfReceive()
*!*************************************************************
FUNCTION lfReceive

*--Create an issue record for Physical inventory or 
*--Markdown inventory transaction in Style inventory Journal.
IF lcTrType $ '39'
  IF lfDoPhys('I')
    =lfIssue()
    STORE 0 TO lnPrvQty,lnPrvVal
  ENDIF
ELSE
  IF lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    =lfAdjRec()
  ENDIF
ENDIF

IF !(lcTrType $ '39') OR (lcTrType $ '39' AND lfDoPhys('R'))
  *--Create a main record in journal file.
  SELECT MatInvJl
  APPEND BLANK

  REPLACE cFabric    WITH lcFabric  ,;
          cColor     WITH lcColor   ,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcFDyelot ,;
          dTranDate  WITH ldTrDate  ,;
          dPostDate  WITH ldPostDate,;
          cTranType  WITH lcTrType  ,;
          cTran      WITH IIF(cTranType $ "239" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
          nUnitCost  WITH lnNewCost ,;
          nUntCstBuy WITH lnNewCost*Fabric.Conv,;
          nReceived  WITH IIF(lcTrType = '5',lnAdjStk  * m.nSellConv,lnAdjStk),;
          nStkVal    WITH IIF(lcTrType = '5',lnAdjStk  * nUnitCost * laConv,nReceived * nUnitCost),;
          Reference  WITH lcRefer   ,;
          cAdjReason WITH lcAdjCdRsn ,;
          cGlMatAdj  WITH lcAdjAcct  ,;
          cRSession  WITH IIF(lcTrType $ '45',lcCTRSess,lcFJlSess) ,;
          cISession  WITH IIF(lcTrType $ '45',lcCTISess,'') ,;          
          cTrn_Seq   WITH cRSession ,;
          cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
          cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
          nMPrvSQty  WITH lnPrvQty ,;
          nPrvSVal   WITH lnPrvVal ,;
          LineNo     WITH lnLineNo,;
          llockflg   WITH lcTrType='9'
  *AMM Update the operation code and lot #
  IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
    REPLACE cOprCode  WITH lcOprCode ,;
            cLotNo    WITH lcLotNo
  ENDIF

  *C200255,1 (Begin) Update voucher no variable for CON10.
  IF ASCAN(oAriaApplication.laEvntTrig,PADR("GETVOUT",10)) <> 0
    REPLACE cvoucherno WITH lcVoucNo
  ENDIF   
  *C200255,1 (End)
    
  *-- Call global function to add audit fields info.
  =gfAdd_Info('MatInvJl')

  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()
  
  IF lcTrType = '9' AND ((!EMPTY(lcFDyelot) AND llAdjYes) OR EMPTY(lcFDyelot))
    =lfLkAdjRec()
  ENDIF
ENDIF


*:*************************************************************
*: Name       : lfRcvLock
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 08/01/2002
*: Purpose    : Add Recive Line in case lock the Item.
*:*************************************************************
*: Calls      : None.
*:*************************************************************
*: Parameters : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example    : = lfRcvLock()
*:*************************************************************
FUNCTION lfRcvLock
PRIVATE lnAlias
RETURN PADR(PADL(ALLTRIM(STR(lnSequence+1)),6,"0"),20)



*!*************************************************************
*! Name      : lfJrRoData
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : This function will get the journal records or rolls
*!             records.
*!  Note     : This function is called in case it is issuing 
*!             and costing method is "FIL"
*!                           OR 
*!             it is issuing or receiving rolls
*!*************************************************************
*! Parameter : If it calls for Physical inventorty procedure.
*!*************************************************************
*! Returns   : A temp file that contain all needed information
*!             this file named as 'lcTmpJour'
*!             If this function returns .F. that means that there
*!             is somthing wrong with creating data. 
*!*************************************************************
*! Example   :  =lfJrRoData()
*!*************************************************************
FUNCTION lfJrRoData
PARAMETERS llExtCall

PRIVATE llContnu,laTotRcvd,lnMaJorRec

*ABD - Define new variable to check if we came from the material invoice
*ABD -  sales order or not. [Begin]
*-- this Variable to know the calling program EX lcCalProg = 'ARMINV' ,
*-- If I call this function from the invoice Material Sales Order.
IF TYPE('lcCalProg') # 'C'
  lcCalProg = SPACE(0)
ENDIF

*WAB - get the llgenRolId from ma setup . if user choose create roll id 
*(START) manual or generated sequence no 

llGenRolId   = ('MA' $ oAriaApplication.CompanyInstalledModules AND ALLTRIM(gfGetMemVar('M_GENROLID')) = 'N')

IF lnAdjStk < 0 OR lcTrType = '3'
  PRIVATE lcItemJour,lcUseFile,llCollcData
  IF !llExtCall
    lcTmpJour = gfTempName() 
    lcUseFile = lcTmpJour
    llCollcData = .T.
  ELSE
    IF USED(lcTmpJour)
      llCollcData = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot,lcTmpJour)
      lcItemJour = gfTempName()
      lcUseFile = lcItemJour
    ELSE
      llCollcData = .T.
      lcItemJour = gfTempName()
      lcTmpJour = gfTempName() 
      lcUseFile = lcTmpJour
    ENDIF
  ENDIF
  IF llCollcData 
    IF lcTrType = '5'
      SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
             cTran,cTranType,dTranDate,dPostDate,nUnitCost*m.nSellconv As 'nUnitCost',nUntCstBuy,;
             SUM(nReceived-nIssued) / m.nSellconv AS 'nBalance'           ,;
             SUM(nReceived) / m.nSellconv AS nReceived                    ,;
             SUM(nIssued)   / m.nSellconv AS nIssued                      ,;
             00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
             "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nMPrvSQty,nPrvSVal  ;
      FROM   MATINVJL                                                      ;
      WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
             lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
      GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
               MatInvJl.cDyelot,MatInvJl.cRSession                         ;
      HAVING nBalance <> 0                                                 ;
      ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
               MatInvJl.cDyelot,MatInvJl.cRSession                         ;
      INTO DBF (oAriaApplication.workdir+lcUseFile)
    ELSE
      IF lcTrType = '1'
        SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
               cTran,cTranType,dTranDate,dPostDate,nUnitCost*lnFabConv AS 'nUnitCost',nUntCstBuy,;
               SUM(nReceived-nIssued) / lnFabConv AS 'nBalance'             ,;
               SUM(nReceived) / lnFabConv AS nReceived                      ,;
               SUM(nIssued)   / lnFabConv AS nIssued                        ,;
               00000000.000 As 'nApply','' AS RolTranCd                     ,;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nMPrvSQty,nPrvSVal  ;
        FROM   MATINVJL                                                      ;
        WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
               lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
        GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
                 MatInvJl.cDyelot,MatInvJl.cRSession                         ;
        HAVING nBalance <> 0                                                 ;
        ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
                 MatInvJl.cDyelot,MatInvJl.cRSession                         ;
        INTO DBF (oAriaApplication.workdir+lcUseFile)
      ELSE
        SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
               cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy     ,;
               SUM(nReceived-nIssued) AS 'nBalance'                         ,;
               SUM(nReceived) AS nReceived                                  ,;
               SUM(nIssued)   AS nIssued                                    ,;
               00000000.000 As 'nApply','' AS RolTranCd                      ,;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nMPrvSQty,nPrvSVal  ;
        FROM   MATINVJL                                                      ;
        WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
               lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
        GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
                 MatInvJl.cDyelot,MatInvJl.cRSession                         ;
        HAVING nBalance <> 0                                                 ;
        ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
                 MatInvJl.cDyelot,MatInvJl.cRSession                         ;
        INTO DBF (oAriaApplication.workdir+lcUseFile)
      ENDIF
    ENDIF

    IF llExtCall AND (lcUseFile = lcItemJour)
      SELECT (lcTmpJour)
      APPEND FROM (oAriaApplication.workdir+lcItemJour+'.DBF')
    ENDIF

  ENDIF
  
  SELECT (lcTmpJour)
  *--Indexing the file on Ascending or Descending expresion 
  *--depends on LIFO or FIFO method.
  IF !(TAG() == lcTmpJour)
    IF lcCostMeth $ 'FL'  &&FIFO OR LOT
      INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
    ELSE
      INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
    ENDIF
  ENDIF
  GO TOP
  *-- This check is only if you issuing
  IF EOF() AND lcTrType <> '3' AND lnAdjStk < 0
    *--No open receiving exist for material/color XXXX/yyyyy ,
    *--This transaction line will be ignored.
    lcMsgExp = lcFabric + "/" + lcColor
    =gfModalGen('TRM36131B00036','DIALOG',lcMsgExp)
    USE
    RETURN .F.
  ENDIF

  IF lcTrType # '3' .AND. lcCostMeth $ "FI"  &&FIFO OR LIFO
    *--Start checking the only needed open receinving transaction for this
    *--issue transaction and put zero for all not needed receivings.

    *--Variable to Hold the accomulation of the receiving untill it cover
    *--the issue quantity needed.
    lnRcvdQty  = 0      
    SCAN WHILE lnRcvdQty <> ABS(lnAdjStk) OR EOF()
      IF lnRcvdQty < ABS(lnAdjStk)
        IF nBalance <= ABS(lnAdjStk+lnRcvdQty)
          lnUpdtQty = IIF(ABS(lnAdjStk+lnRcvdQty) > nBalance,nBalance,ABS(lnAdjStk+lnRcvdQty))
          REPLACE nApply  WITH lnUpdtQty ,;
                  lNeeded WITH .T.
        ELSE
          REPLACE nApply  WITH ABS(lnAdjStk+lnRcvdQty) ,;
                  lNeeded WITH .T.
          *B606285,1 ABD - [End]        
        ENDIF
        lnRcvdQty = lnRcvdQty + nApply
      ENDIF
    ENDSCAN
    *--Check if all Issue quantity are covered by the receivings.
    IF lnRcvdQty < ABS(lnAdjStk)
      *--The receiving quantity are not covered the issued quantity
      *--for material/color XXXX , This transaction line will be ignored.
      lcMsgExp = lcFabric + "/" + lcColor
      =gfModalGen('TRM36132B00036','DIALOG',lcMsgExp)
      USE
      RETURN .F. 
    ENDIF
  ELSE
    *-- it is lot
    *call lot rol SCREEN only if trans. type <> 3
    IF (lcTrType # '3' OR (llTrkRolls .AND. Fabric.ltrkrolls)) AND !lfLotRolScr()
      RETURN .F.
    ENDIF
  ENDIF
ELSE    && receiving
  IF llTrkRolls .AND. Fabric.ltrkrolls
    IF !lfLotRolScr()
      RETURN .F.
    ENDIF
  ENDIF
ENDIF


IF lnAdjStk < 0
  *--Delete all not needed receiving transactions for Journal file
  SELECT(lcTmpJour)
  SCAN WHILE cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =;
             lcFabric+lcColor+lcWareCode+lcFDyelot
    IF lNeeded
      *--Change it to Issue transactions,to use it in updating master Journal file.
      REPLACE dTranDate WITH ldTrDate  ,;
              dPostDate WITH ldPostDate,;
              cTranType WITH lcTrType  ,;
              cTran     WITH lcTrCode  ,;
              nApply    WITH nApply
    ELSE
      DELETE
    ENDIF
  ENDSCAN
ENDIF

*--Delete all not needed receiving transactions for Roll file
IF llTrkRolls .AND. Fabric.ltrkrolls
  SELECT(lcTmpRoll)
  PRIVATE lcTag
  lcTag = ORDER()
  SET ORDER TO lcTmpRoll2
  *-- Retuern the Pointer to first record match the correct Expr.
  = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6))
  SCAN WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)
    IF lNeeded
      *--Change it to Issue transactions,to use it in updating master Journal file.
      REPLACE dTranDate WITH ldTrDate  ,;
              dPostDate WITH ldPostDate,;
              cTranType WITH lcTrType  ,;
              cTran     WITH lcTrCode  ,;
              nApply    WITH nApply
    ELSE
      DELETE
    ENDIF
  ENDSCAN
  SET ORDER TO (lcTag) IN (lcTmpRoll)
  
  IF lnAdjStk < 0
    SELECT(lcFullRoll)
    PRIVATE lcTag
    lcTag = ORDER()
    SET ORDER TO lcFullRoll
    *-- Retuern the Pointer to first record match the correct Expr.    
    = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6))
    SCAN WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
               lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)

      IF lNeeded
        *--Change it to Issue transactions,to use it in updating master Journal file.
        REPLACE dTranDate WITH ldTrDate  ,;
                dPostDate WITH ldPostDate,;
                cTranType WITH lcTrType  ,;
                cTran     WITH lcTrCode  ,;
                nApply    WITH nApply
      ELSE
        DELETE
      ENDIF
    ENDSCAN
    *-- Return Order To the Old Order.
    SET ORDER TO &lcTag
   
  ENDIF  
ENDIF

SELECT MatInvJl
IF llExtCall
  RETURN (lnAdjStk) 
ENDIF

RETURN .T.

*!***********************************************************************
*! Name      : lfAdjRec
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Add Rec. record and Iss. record in MatInvJl.
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfAdjRec()
*!***********************************************************************

FUNCTION lfAdjRec

PRIVATE lcOdTrnTyp
lcOdTrnTyp = lcTrType
*-- Rec. with the old cost
  SELECT MatInvJl
  APPEND BLANK
  REPLACE cFabric    WITH lcFabric  ,;
          cColor     WITH lcColor   ,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcFDyelot ,;
          dTranDate  WITH ldTrDate  ,;
          dPostDate  WITH ldPostDate,;
          cTranType  WITH '2'  ,;
          cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
          nUnitCost  WITH lnWOldCst ,;
          nUntCstBuy WITH lnWOldCst*Fabric.Conv,;
          nReceived  WITH MIN(ABS(lnAdjStk),ABS(lnWOldStk));
          nStkVal    WITH IIF(lnAdjStk+lnWOldStk>=0,-lnWStkVal,nReceived * nUnitCost),;
          Reference  WITH lcRefer   ,;
          cAdjReason WITH lcAdjCdRsn ,;
          cGlMatAdj  WITH lcAdjAcct  ,;
          cRSession  WITH IIF(lcTrType = '4',lcCTRSess,lcFJlSess) ,;
          cTrn_Seq   WITH cRSession ,;
          nMPrvSQty  WITH lnPrvQty ,;
          nPrvSVal   WITH lnPrvVal

  *-- Call global function to add audit fields info.
  =gfAdd_Info('MatInvJl')

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)

lcTrType = '2'

=lfFabWarDy()

lcTrType = lcOdTrnTyp
*-- Iss. with the new cost
  SELECT MATINVJL
  APPEND BLANK
         
  REPLACE cFabric    WITH lcFabric  ,;
          cColor     WITH lcColor   ,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcFDyelot ,;
          dTranDate  WITH ldTrDate  ,;
          dPostDate  WITH ldPostDate,;
          cTranType  WITH '2'  ,;
          cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
          nUnitCost  WITH lnNewCost ,;
          nUntCstBuy WITH lnNewCost*Fabric.Conv,;
          nIssued    WITH MIN(ABS(lnAdjStk),ABS(lnWOldStk));
          nStkVal    WITH - nIssued * nUnitCost,;
          Reference  WITH lcRefer   ,;
          cAdjReason WITH lcAdjCdRsn ,;
          cGlMatAdj  WITH lcAdjAcct  ,;
          cISession  WITH IIF(lcTrType = '4',lcCTISess,lcFJlSess) ,;
          cTrn_Seq   WITH cISession,;
          nMPrvSQty  WITH lnPrvQty ,;
          nPrvSVal   WITH lnPrvVal
  
  *-- Call global function to add audit fields info.
  =gfAdd_Info('MATINVJL')


*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)

*AMH Change the transaction type to Material Adjustement before updating Fabric & Fabdye files [Start]
lcTrType = '2'

=lfFabWarDy()

*AMH Restore the current transaction type [Start]
lcTrType = lcOdTrnTyp


*!*************************************************************
*! Name      : lfUpdGLDist()
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Update Temp G/L Distribution file.
*!*************************************************************
*:      [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:      [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:      [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:      [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*!*************************************************************
*! Call      : GLDIST
*!*************************************************************
*! Example   : =lfUpdGLDist()
*!*************************************************************

FUNCTION lfUpdGLDist
PARAMETERS llNegStkAd,llLockAdj

PRIVATE lnCurAlias

*--Donot update if no GL used.
IF ! llGLUsed
  RETURN
ENDIF

*-- This means it is Main Record
IF !llNegStkAd

  *--Update Gl for Main inventory record for Isue or Receive.
  *- Receiving Trans.(+1,+2,+3):    None
  *-  => +/-  lnAmount = Total Recv. Qty * New Recv. Cost     
  *- Issue Trans.(-1,-2,-3)     :  None
  *-  => +/-  lnAmount = Total Issue Qty * Issue Cost     
  FOR lnAln=1 TO ALEN(laGLInvAry,1)

    laGLInvAry[lnAln,5] = IIF(MatInvJl.cTranType $ "23",MatInvJl.Ctrn_Seq,laGLInvAry[lnAln,5])

      IF MatInvJl.nReceived > 0
        lnQty = MatInvJl.nReceived
      ELSE
        IF MatInvJl.nIssued > 0
          lnQty = - MatInvJl.nIssued
        ELSE
          *-- this means received qty and issued qty both are Zero
          lnQty   = 0
        ENDIF
      ENDIF
        
      lnGLEnAmount = MatInvJl.nStkVal * (laGLInvAry[lnAln,3])
      DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2]  ,;
                     lnGLEnAmount,; 
                     laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
                     IIF(llLockAdj,ldPostDate,laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7]  ,;
                     laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
                     laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
                     laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
      lnCurAlias = SELECT(0)
      SELECT (laGLInvAry[lnAln,9])
      REPLACE glSession WITH lcFJlSess
      SELECT MatInvJl
      
      DO CASE 
        CASE &laGLInvAry[lnAln,9]..Catg_Key = "015"
          REPLACE cMIcAcct WITH &laGLInvAry[lnAln,9]..GLAccount
        CASE &laGLInvAry[lnAln,9]..Catg_Key = "013" OR EMPTY(cGLMatAdj)
          REPLACE cGLMatAdj WITH &laGLInvAry[lnAln,9]..GLAccount
      ENDCASE

      
      SELECT (lnCurAlias)
    
  ENDFOR


ELSE
*-- This means it is Adj. Record 

*--Update Gl for Receiving inventory record and -ve old Stock.
*- Receiving Trans.(+1,+2,+3)

  lcMatLink = IIF(EMPTY(FabDye.GL_Link),Fabric.Link_Code,FabDye.GL_Link)
  DO GLDIST WITH lcMatLink,'015',MatInvJl.nStkVal,'MA',;
                 MatInvJl.cTran,MatInvJl.dTranDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  lnCurAlias = SELECT(0)
  SELECT MatInvJl
  REPLACE cMIcAcct WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  *--Update Uncomplete session Step.
  DO GLDIST WITH lcMatLink,'016',-MatInvJl.nStkVal,'MA',;
                 MatInvJl.cTran,MatInvJl.dTranDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  IF EMPTY(MatInvJl.cGLMatAdj)
    lnCurAlias = SELECT(0)
    SELECT MatInvJl
    REPLACE cGLMatAdj WITH &laGLInvAry[1,9]..GLAccount
    SELECT (lnCurAlias)
  ENDIF

ENDIF
RETURN




*!*********************************************************************************************************
*!*************************************************************
*! Name      : lfLotRolScr()
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Call lot or roll screen
*!*************************************************************
*! Example   : =lfLotRolScr()
*!*************************************************************

FUNCTION lfLotRolScr
PRIVATE lcChck,lcUnChck,;
        lcFab,lcClr,lcWare,lcDye,lnTotApply,lnUsrApply,;
        lcRNewSta,lcRRemSta,lcRModSta,llRetVal,lnOldVal,lnNewRec
PRIVATE lcFileToUse,llBrowse
PRIVATE lcSerchExp


STORE SPACE(0) To lcSerchExp
llBrowse = .F.

IF llTrkRolls .AND. Fabric.ltrkrolls
  *-- "!llExtCall" If it's called from the function (gfMatCrl) OR 
  *-- "!USED(lcTmpRoll" it's called from receiving material manufacturing order OR
  *-- "lcTrType = '1' AND lnAdjStk < 0" it's called from issue Material PO return
  IF (llExtCall AND (!USED(lcTmpRoll) OR (lcTrType $ '15' AND lnAdjStk < 0)) )  OR !llExtCall
    IF EMPTY(lcToWare) OR lcToWare # lcWareCode
      = lfTmpRoll()
    ENDIF
  ENDIF
ENDIF

lcFileToUse = IIF(llTrkRolls .AND. Fabric.ltrkrolls,lcTmpRoll,lcTmpJour)
lnNewRec = RECNO(lcFileToUse)
llRetVal = .T.

lcFab  = lcFabric
lcClr  = lcColor
lcWare = lcWareCode
lcDye  = lcFDyelot

STORE 0 TO lnOldVal

STORE IIF(lcTrType='3',lnTmpOStk,0) TO lnUsrApply
STORE lnAdjStk TO lnTotApply

IF TYPE('llExtCall') = 'L' AND llExtCall
  DIMENSION laUsrApply[1]
  laUsrApply = 0
  *AAN Check if the case is roll then add (lineno to where condition[Start]
  IF llTrkRolls .AND. Fabric.ltrkrolls
    SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
            lcFab+lcClr+lcWare+lcDye+STR(lnLineNo,6) ;
      INTO ARRAY laUsrApply
  ELSE
    SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cFabric+cColor+cWareCode+cDyelot = ;
            lcFab+lcClr+lcWare+lcDye ;
      INTO ARRAY laUsrApply
  ENDIF
  STORE laUsrApply * IIF(lnAdjStk>0,1,-1) TO lnUsrApply
ENDIF

lcLotRo  = gfTempName()
lcLotRo1 = gfTempName()
lcLotRo2 = gfTempName()
lcLotRo3 = gfTempName()

DO CASE
  CASE lnAdjStk < 0
    IF llTrkRolls .AND. Fabric.ltrkrolls
      llShowRol = .T.
      STORE 0 TO lnCurRolBa,lnAppRolQt
    ELSE
      STORE 0 TO lnCurLotBa,lnAppLotQt,lnNewLotBa
      llShowRol = .F.
    ENDIF
  CASE lnAdjStk >= 0 AND llTrkRolls .AND. Fabric.ltrkrolls
    llShowRol = .T.
    STORE 0 TO lnCurRolBa,lnAppRolQt
ENDCASE

DO FORM (oAriaApplication.ScreenHome +'MALotRo.scx')

SELECT(lcFileToUse)
SET FILTER TO
PRIVATE lnTotRoll
IF (lnAdjStk < 0 ) AND llTrkRolls .AND. Fabric.ltrkrolls
  SELECT (lcTmpRoll)
  PRIVATE lcTag
  lcTag = ORDER()
  SET ORDER TO lcTmpRoll2
  SCAN FOR cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcFabric+lcColor+lcWareCode+lcFDyelot
    lnTotRoll = nApply
    SELECT (lcFullRoll)
    IF SEEK(&lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
            &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot)
      SCAN REST WHILE lnTotRoll <> 0 AND ;
                      cRollID+cFabric+cColor+cWareCode+cDyelot = ;
                      &lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
                      &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot
        REPLACE nApply    WITH IIF(nBalance<lnTotRoll,nBalance,lnTotRoll),;
                RolTranCd WITH IIF(lnAdjStk<0,'2',&lcTmpRoll..RolTranCd) ,;
                lNeeded   WITH !EMPTY(nApply)
        lnTotRoll = lnTotRoll - &lcFullRoll..nApply
      ENDSCAN
    ENDIF
  ENDSCAN
  SET ORDER TO (lcTag) IN (lcTmpRoll)
  PRIVATE lnOldAlais , lcOldRlOdr , lnOldRecNR , lnOldRecNJ , lcScanExpr
  *-- Save Old Alias
  lnOldAlais = SELECT (0)
  *-- Save Old Pointer For the FullRoll File.
  SELECT (lcFullRoll)
  lcOldRlOdr = ORDER()
  SET ORDER TO lcFullRoll
  lnOldRecNR = RECNO()
  *-- Save Old Pointer For the TmpJour File.
  SELECT (lcTmpJour)
  lnOldRecNJ = RECNO()
  LOCATE
  SCAN
    lcScanExpr = cFabric+cColor+cWareCode+cDyelot
    *-- I will update the nApply Qty With 0 to Void duplicated 
    *-- value if we issue the same fabric color in the same session.
    REPLACE nApply WITH 0

    *-- Scan On the Full Roll File and get the total Qty fro this session.
    IF SEEK(lcScanExpr,lcFullRoll)
      SELECT (lcFullRoll)
      SCAN REST WHILE cfabric+ccolor+cwarecode+cdyelot+STR(lineno,6) = ;
          lcScanExpr  FOR cRSession = &lcTmpJour..cRSession  ;
          .AND. nApply > 0
        SELECT (lcTmpJour)
         REPLACE nApply  WITH nApply + &lcFullRoll..nApply,;
                 lNeeded WITH !EMPTY(nApply)
        SELECT (lcFullRoll)
      ENDSCAN
    ENDIF
    SELECT (lcTmpJour)  
  ENDSCAN
  SELECT (lcTmpJour)
  IF BETWEEN(lnOldRecNJ,1,RECCOUNT()) 
    GOTO lnOldRecNJ
  ENDIF
  
  SELECT (lcFullRoll)
  SET ORDER TO &lcOldRlOdr
  IF BETWEEN(lnOldRecNR,1,RECCOUNT()) 
    GOTO lnOldRecNR
  ENDIF

  SELECT (lnOldAlais)
ENDIF

RETURN llRetVal


*!*************************************************************
*! Name      : lfRollBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse existing rolls
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfRollBrow()
*!*************************************************************

FUNCTION lfRollBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl,llFound

DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "cRollID"
lcBrFields  = [cRollID  :H='Roll ID',]+;
              [nQtyBal  :H='Balance']
lcFile_Ttl  = 'MExising Rolls'
SELECT Rolls
lcCurTag = Order('Rolls')

SET ORDER TO RollItem
llFound = SEEK(lcFab+lcClr+lcWare+lcDye)
IF llFound
  LOCATE REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollID+TranCd+cRSession = ;
                    lcFab+lcClr+lcWare+lcDye FOR TranCd='1'
  llFound = FOUND()
ENDIF
IF llFound
  llReturn = AriaBrow("lcFab+lcClr+lcWare+lcDye FOR TranCd='1'",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'Rolls',.F.)
ELSE
  = gfModalGen('TRM00052B00036','DIALOG')
ENDIF  

SET ORDER TO lcCurTag IN Rolls
SELECT(lnCurAlias)

RETURN llReturn




*!*************************************************************
*! Name      : lfTmpRoll
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Create rolls temp. files
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTmpRoll()
*!*************************************************************
FUNCTION lfTmpRoll

IF !USED(lcTmpRoll)
  lcTmpRoll  = gfTempName()
  DIMENSION laTags[3,3]
  laTags[1,1]='cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession'
  laTags[1,2]=lcTmpRoll
  IF llExtCall .AND. lcCalProg = 'ARMINV'
    laTags[2,1]='cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)+cMorder'
    laTags[2,2]='lcTmpRoll2'
  ELSE
    laTags[2,1]='cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)'
    laTags[2,2]='lcTmpRoll2'
  ENDIF
  laTags[3,1]='cRsession+cFabric+cColor+cWareCode+cDyelot'
  laTags[3,2]='lcTmpRoll3'

  IF llExtCall .AND. lcCalProg = 'ARMINV'
      DIMENSION laFileStru[25,4]
  ELSE
    DIMENSION laFileStru[23,4]
  ENDIF

  laFileStru[1 ,1] = 'cRollId'
  laFileStru[1 ,2] = 'C'
  laFileStru[1 ,3] = 20
  laFileStru[1 ,4] = 0

  laFileStru[2 ,1] = 'cTrn_Seq'
  laFileStru[2 ,2] = 'C'
  laFileStru[2 ,3] = 6
  laFileStru[2 ,4] = 0

  laFileStru[3 ,1] = 'cFabric'
  laFileStru[3 ,2] = 'C'
  laFileStru[3 ,3] = 7
  laFileStru[3 ,4] = 0

  laFileStru[4 ,1] = 'cColor'
  laFileStru[4 ,2] = 'C'
  laFileStru[4 ,3] = 6
  laFileStru[4 ,4] = 0

  laFileStru[5 ,1] = 'cWareCode'
  laFileStru[5 ,2] = 'C'
  laFileStru[5 ,3] = 6
  laFileStru[5 ,4] = 0

  laFileStru[6 ,1] = 'cDyelot'
  laFileStru[6 ,2] = 'C'
  laFileStru[6 ,3] = 10
  laFileStru[6 ,4] = 0

  laFileStru[7 ,1] = 'cRSession'
  laFileStru[7 ,2] = 'C'
  laFileStru[7 ,3] = 6
  laFileStru[7 ,4] = 0

  laFileStru[8 ,1] = 'cISession'
  laFileStru[8 ,2] = 'C'
  laFileStru[8 ,3] = 6
  laFileStru[8 ,4] = 0

  laFileStru[9 ,1] = 'cTran'
  laFileStru[9 ,2] = 'C'
  laFileStru[9 ,3] = 6
  laFileStru[9 ,4] = 0

  laFileStru[10,1] = 'cTranType'
  laFileStru[10,2] = 'C'
  laFileStru[10,3] = 1
  laFileStru[10,4] = 0

  laFileStru[11,1] = 'dTranDate'
  laFileStru[11,2] = 'D'
  laFileStru[11,3] = 8
  laFileStru[11,4] = 0

  laFileStru[12,1] = 'dPostDate'
  laFileStru[12,2] = 'D'
  laFileStru[12,3] = 8
  laFileStru[12,4] = 0

  laFileStru[13,1] = 'nUnitCost'
  laFileStru[13,2] = 'N'
  laFileStru[13,3] = 9
  laFileStru[13,4] = 3

  laFileStru[14,1] = 'nUntCstBuy'
  laFileStru[14,2] = 'N'
  laFileStru[14,3] = 9
  laFileStru[14,4] = 3

  laFileStru[15,1] = 'nBalance'
  laFileStru[15,2] = 'N'
  laFileStru[15,3] = 12
  laFileStru[15,4] = 3

  laFileStru[16,1] = 'nReceived'
  laFileStru[16,2] = 'N'
  laFileStru[16,3] = 12
  laFileStru[16,4] = 3

  laFileStru[17,1] = 'nIssued'
  laFileStru[17,2] = 'N'
  laFileStru[17,3] = 12
  laFileStru[17,4] = 3

  laFileStru[18,1] = 'nApply'
  laFileStru[18,2] = 'N'
  laFileStru[18,3] = 12
  laFileStru[18,4] = 3

  laFileStru[19,1] = 'cMarker'
  laFileStru[19,2] = 'C'
  laFileStru[19,3] = 1
  laFileStru[19,4] = 0

  laFileStru[20,1] = 'lStatus'
  laFileStru[20,2] = 'C'
  laFileStru[20,3] = 1
  laFileStru[20,4] = 0

  laFileStru[21,1] = 'lNeeded'
  laFileStru[21,2] = 'L'
  laFileStru[21,3] = 0
  laFileStru[21,4] = 0

  laFileStru[22,1] = 'RolTranCd'
  laFileStru[22,2] = 'C'
  laFileStru[22,3] = 1
  laFileStru[22,4] = 0

  laFileStru[23,1] = 'LineNo'
  laFileStru[23,2] = 'N'
  laFileStru[23,3] = 6
  laFileStru[23,4] = 0

  IF llExtCall .AND. lcCalProg = 'ARMINV'
    laFileStru[24,1] = 'nIssue'
    laFileStru[24,2] = 'N'
    laFileStru[24,3] = 6
    laFileStru[24,4] = 3

    laFileStru[25,1] = 'cMorder'
    laFileStru[25,2] = 'C'
    laFileStru[25,3] = 6
    laFileStru[25,4] = 0
  
  ENDIF

  =gfCrtTmp(lcTmpRoll ,@laFileStru,@laTags)

ENDIF

IF lcTrType$'1235'
  IF !USED(lcFullRoll)

    lcFullRoll = gfTempName()

    DIMENSION laTagArr[2,2]
    laTagArr[1,1] = 'cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession'
    laTagArr[1,2] = lcFullRoll
    laTagArr[2,1] = 'cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)'
    laTagArr[2,2] = 'lcFullRol2'
    =gfCrtTmp(lcFullRoll ,@laFileStru,@laTagArr)
  ENDIF
  
  SELECT Rolls
  lcRolTag = ORDER()
  SET ORDER TO RollItem
  *--crollitem+color+cwarecode+dyelot+crollid+trancd+crsession
  SELECT MatInvJl
  lcJorTag = ORDER()
  SET ORDER TO MatInvJl
  *--cfabric+ccolor+cwarecode+cdyelot+crsession+cisession+STR(RECNO(),7)
  
  SELECT Rolls
  lcRollRela = SET('RELATION')
  SET RELATION TO cRollItem+Color+cWareCode+Dyelot+cRSession+cISession INTO MatInvJl

  IF (TYPE('llExtCall') = 'U' OR !llExtCall) OR (llExtCall AND lcTrType $ '15' AND lnAdjStk < 0)
    PRIVATE lcTag,llColldata
    lcTag = ORDER(lcTmpRoll)
    SET ORDER TO lcTmpRoll2 IN (lcTmpRoll)
  
    *ABD - collecte the data again if not the same order. [Begin]
    *llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)
    IF llExtCall .AND. lcCalProg = 'ARMINV'
     llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)+lcTrCode,lcTmpRoll)
    ELSE
      llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)
    ENDIF

    SET ORDER TO (lcTag) IN (lcTmpRoll)
    IF llColldata
   
      SELECT Rolls
      IF SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot)
        SCAN WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
                   lcFabric +lcColor+lcWareCode+lcFDyelot FOR nQtyBal # 0
          IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot,lcTmpRoll)
            IF Rolls.TranCd = '1'
              SELECT (lcTmpRoll)
              APPEND BLANK
              REPLACE cTrn_Seq   WITH ''                           ,;
                      cRollId    WITH Rolls.cRollID                ,;
                      cFabric    WITH Rolls.cRollItem              ,;
                      cColor     WITH Rolls.Color                  ,;
                      cWareCode  WITH Rolls.cWareCode              ,;
                      cDyelot    WITH Rolls.Dyelot                 ,;
                      cRSession  WITH Rolls.cRSession              ,;
                      cISession  WITH Rolls.cISession              ,;
                      cTran      WITH ''                           ,;
                      cTranType  WITH ''                           ,;
                      dTranDate  WITH MatInvJl.dTranDate           ,;
                      dPostDate  WITH MatInvJl.dPostDate           ,;
                      nUnitCost  WITH IIF(lcTrType = '5',(MatInvJl.nUnitCost*m.nSellconv),;
                                      MatInvJl.nUnitCost),;
                      nUntCstBuy WITH MatInvJl.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH IIF(lcTrType$'15',Rolls.nQtyBal/IIF(lcTrType='1',;
                                          Fabric.Conv,m.nSellconv),Rolls.nQtyBal) ,;
                      nApply     WITH IIF(lcTrType='3' OR (lcTrType='2' AND lnAdjStk > 0),nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType$'15' AND lnAdjStk<0,'2','3'),;
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='3'                 ,;
                      LineNo     WITH lnLineNo
              IF llExtCall .AND. lcCalProg = 'ARMINV'
                REPLACE nIssue WITH 0        ,;
                        cMorder WITH lcTrCode
              ENDIF 
            ENDIF
          ENDIF
          SELECT (lcFullRoll)
          IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession,lcFullRoll)
            APPEND BLANK
            REPLACE cTrn_Seq   WITH ''                  ,;
                    cRollId    WITH Rolls.cRollID       ,;
                    cFabric    WITH Rolls.cRollItem     ,;
                    cColor     WITH Rolls.Color         ,;
                    cWareCode  WITH Rolls.cWareCode     ,;
                    cDyelot    WITH Rolls.Dyelot        ,;
                    cRSession  WITH Rolls.cRSession     ,;
                    cISession  WITH Rolls.cISession     ,;
                    cTran      WITH ''                  ,;
                    cTranType  WITH ''                  ,;
                    dTranDate  WITH MatInvJl.dTranDate  ,;
                    dPostDate  WITH MatInvJl.dPostDate  ,;
                    nUnitCost  WITH MatInvJl.nUnitCost  ,;
                    nUntCstBuy WITH MatInvJl.nUntCstBuy ,;
                    nReceived  WITH 0                   ,;
                    nIssued    WITH 0                   ,;
                    RolTranCd  WITH IIF(lcTrType$'15' AND lnAdjStk<0,'2','3'),;
                    lStatus    WITH "S"                 ,;
                    lNeeded    WITH lcTrType='3'        ,;
                    LineNo     WITH lnLineNo
          ENDIF
          REPLACE nBalance WITH nBalance+(IIF(Rolls.TranCd='2',-Rolls.nQty,Rolls.nQty)/IIF(lcTrType$'15',;
                                IIF(lcTrType ='1',Fabric.Conv,m.nSellconv),1)),;
                  nApply   WITH IIF(lcTrType='3',nBalance,0)
          IF llExtCall .AND. lcCalProg = 'ARMINV'
            REPLACE cMorder WITH lcTrCode
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
  ENDIF

  SELECT Rolls
  SET ORDER TO lcRolTag
  SELECT MatInvJl
  SET ORDER TO lcJorTag
  SELECT Rolls
  SET RELATION TO &lcRollRela
ENDIF

***************************************************************************

FUNCTION lfFabWarDy
PARAMETERS llToWare
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

*--1 ) Update Stock and Avarege cost in Style file. ------------
SELECT Fabric
=RLOCK()
IF EMPTY(lcOrgFab)
  *--variable to check if come from po OR not 
  DO CASE
    CASE (lcProg = 'MARECI')
      lnReplace = IIF(lnAdjStk > 0,IIF((lnpolin-lnreclin)<>0,;
                  MIN(lnOldOnOrd,lnAdjStk),0),;
                  IIF(OnOrder < 0 ,-1*MIN(ABS(lnOldOnOrd),ABS(lnAdjStk)),lnAdjStk))
       
    CASE (lcProg = 'MAMNREC')
      lnReplace = 0
      lnReplace = IIF(lnAdjStk > 0,IIF(lnOldOnOrd > 0 ,;
                  MIN(lnOldOnOrd,lnAdjStk),OnOrder),;
                  IIF(OnOrder < 0 ,-1*MIN(ABS(lnOldOnOrd),ABS(lnAdjStk)),lnAdjStk))
    OTHERWISE
      lnReplace = 0
  ENDCASE
                 
  *ABD Issue and receive the material manufacturing 
  *ABD Rework order from MAMNREC program & I will
  *ABD Not update the On order Qty. [Begin]
  IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
    REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,; 
            nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
            nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Conv                               ,;
            OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0)
  ELSE
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
            nFAve_Cost WITH IIF(lcIRType='I',IIF(llToWare,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand),nFAve_Cost),;
            IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Conv,;
            OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0),;
            OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
  ENDIF

ELSE
    
  *ABD Issue and receive the material manufacturing 
  *ABD Rework order from MAMNREC program & I will
  *ABD Not update the On order Qty. [Begin]
  IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
    REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
            nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Conv
  ELSE
    =RLOCK()
    REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
            nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Conv,;
            OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
    UNLOCK
  ENDIF

  *-- Seek the original fabric/color then updte it
  IF SEEK(lcOrgFab+lcOrgClr)
    =RLOCK()
    REPLACE  ONORDER WITH MAX((ONORDER - lnAdjStk),0)
    UNLOCK
  ENDIF
    
ENDIF
UNLOCK 


*--2 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
SELECT FabDye
IF !EMPTY(lcPrvWare)
  =SEEK(lcFabric+lcColor+IIF(llToWare,lcToWare,lcPrvWare)+SPACE(10),'FabDye')
    
  IF EMPTY(lcOrgFab)
    REPLACE OnOrder WITH OnOrder - IIF(lcTrType='1',MIN(lnOldOnOrd , lnAdjStk),0)
  ELSE
    IF SEEK(lcOrgFab + lcOrgClr + IIF(llToWare,lcToWare,lcPrvWare) + SPACE(10))
      =RLOCK()
      REPLACE  ONORDER WITH MAX((ONORDER - lnAdjStk),0)
      UNLOCK
    ENDIF
  ENDIF
 
  =SEEK(lcFabric+lcColor+IIF(llToWare,lcToWare,lcWareCode)+SPACE(10),'FabDye')
  IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
    REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,; 
            nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
            nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Fabric.Conv
  ELSE
    REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
            nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Fabric.Conv ,;
            OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
  ENDIF
ELSE
 
  =SEEK(lcFabric+lcColor+IIF(llToWare,lcToWare,lcWareCode)+SPACE(10),'FabDye')
  =RLOCK()
  IF EMPTY(lcOrgFab)
    DO CASE
         CASE (lcProg = 'MARECI')
           lnReplace = IIF(lnAdjStk > 0,IIF((lnpolin-lnreclin)<>0,;
                       MIN(lnOldOnOrd,lnAdjStk),0),;
                       IIF(OnOrder < 0 ,-1*MIN(ABS(lnOldOnOrd),ABS(lnAdjStk)),lnAdjStk))
       
         CASE (lcProg = 'MAMNREC')
           lnReplace = IIF(lnAdjStk > 0,IIF(lnOldOnOrd > 0 ,;
                       MIN(lnOldOnOrd,lnAdjStk),OnOrder),;
                       IIF(OnOrder < 0 ,-1*MIN(ABS(lnOldOnOrd),ABS(lnAdjStk)),lnAdjStk))
        OTHERWISE
          lnReplace = 0
    ENDCASE
    IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,; 
              nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
              nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Fabric.Conv,;
              OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0)
    ELSE
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
              nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
              nFAve_Cost WITH IIF(lcIRType='I',IIF(llToWare,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand),nFAve_Cost),;
              IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Fabric.Conv,;
              OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0),;
              OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
              Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
             nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
    ENDIF  
    ELSE 
      *ABD Issue and receive the material manufacturing 
      *ABD Rework order from MAMNREC program & I will
      *ABD Not update the On order Qty. [Begin]
      IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
        REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,; 
                nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
                nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
                nAveCstBuy WITH nFAve_Cost * Fabric.Conv

        *-- Seek the original fabric/color/dyelot then updte it
        IF SEEK(lcOrgFab + lcOrgClr + IIF(llToWare,lcToWare,lcWareCode) + SPACE(10))
          =RLOCK()
          REPLACE  ONORDER WITH MAX((ONORDER - lnAdjStk),0)
          UNLOCK
        ENDIF
        
      ELSE
        REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
                nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
                nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
                nAveCstBuy WITH nFAve_Cost * Fabric.Conv ,;
                OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
                Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
                nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)

        *-- Seek the original fabric/color/dyelot then updte it
        IF SEEK(lcOrgFab + lcOrgClr + IIF(llToWare,lcToWare,lcWareCode) + SPACE(10))
          =RLOCK()
          REPLACE  ONORDER WITH MAX((ONORDER - lnAdjStk),0)
          UNLOCK
        ENDIF
      ENDIF
    ENDIF
  ENDIF  
  UNLOCK


  lnPrvQty  = OnHand
  lnPrvVal  = nStkVal
  lnDyeCost = IIF(FabDye.OnHand = 0,FabDye.nFAve_Cost,FabDye.nStkVal/FabDye.OnHAnd)


*--3 )  Update Stock in Fabric Dyelot file Dyelot record. --------
IF !EMPTY(lcFDyelot) AND SEEK(lcFAbric+lcColor+IIF(llToWare,lcToWare,lcWareCode)+lcFDyelot,'FabDye')
    SELECT FabDye
    =RLOCK()
    REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)

    UNLOCK 
  
    lnPrvQty = OnHand
    lnPrvVal = OnHand * lnDyeCost

ENDIF


SELECT(lnCurAlias)
************************************************************************

FUNCTION lfCalcStkVal

PRIVATE lnOrgJRec,lnRJRec,llDiffSess,lcOrgTran,lcJourTag,lnCurRec,lcOrgTrTyp
llDiffSess = .F.

lnStkVal = lnAdjStk * lnNewCost
lnCost   = lnNewCost

IF lcIRType = 'I' AND lcTrType $ '15'
  lcOrgTran  = PoFHdr.POMat
  SELECT MatInvJl
  lnOrgJRec = RECNO()

  IF SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+lcLastRSess)
    LOCATE REST WHILE cFabric +cColor +cWareCode +cDyelot  +cRSession+cISession = ;
                      lcFabric+lcColor+lcWareCode+lcFDyelot+lcLastRSess ;
                FOR   cTran = lcOrgTran   AND cTrType = '1'

    IF FOUND()
      lnRJRec = RECNO()
      LOCATE REST WHILE cFabric +cColor +cWareCode +cDyelot  +cRSession+cISession = ;
                        lcFabric+lcColor+lcWareCode+lcFDyelot;
             FOR !INLIST(cTarn,lcTrCode,lcOrgTran) ;
             AND cTrn_Seq <> lcLastRSess

      llDiffSess = FOUND()

      IF !llDiffSess
        IF lnRJRec <= RECCOUNT()
          GO lnRJRec
        ENDIF
        lnStkVal  = nStkVal
        lnNewCost = nUnitCost
      ENDIF
    ENDIF
  ENDIF
  IF lnOrgJRec <= RECCOUNT()
    GO lnOrgJRec
  ENDIF
ENDIF

***************************************

FUNCTION lfDoPhys
PARAMETERS lcRI
PRIVATE lcRI
lnRet = .F.

DO CASE
  CASE lcRI = 'I'
    lnRet = lnWStkVal # 0           OR ;
            lnWOldStk # 0           OR ;
            lnWOldCst # lnNewCost   OR;
            lnWStkVal # lnAdjStk
  CASE lcRI = 'R'
    lnRet = !(lnWStkVal # 0       OR ;
            lnWOldStk # 0         OR ;
            lnWOldCst # lnNewCost OR ;
            lnWStkVal # lnAdjStk      ) OR ;
            lnAdjStk  # 0               OR ;
            lnWOldCst # lnNewCost        

ENDCASE

RETURN lnRet

*******************************************************************

FUNCTION lfLkAdjRec

PRIVATE lcJourTag,lnCurAlias,lcCurFlt;
        lnTotQty,lnTotVal,lnTranVal,lnDiffere

lnCurAlias = SELECT(0)
SELECT MatInvJl
lcJourTag = ORDER('MatInvJl')
SET ORDER TO 0
lcCurFlt = FILTER()
SET FILTER TO cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
              lcFabric+lcColor+lcWareCode
SET ORDER TO Mtinvseq

IF EMPTY(lcFDyelot)
  lnTotQty  = lnAdjStk
  lnTotVal  = lnAdjStk * lnNewCost
  lnTranVal = 0
  lnDiffere = 0
ELSE
  lnTotQty = 0
  lnTotVal = 0
  SCAN FOR CtranType = '9' AND nIssued = 0
    lnTotQty  = lnTotQty+nReceived
    lnTotVal  = lnTotVal+(nStkVal)
  ENDSCAN
  lnNewCost = lnTotVal/lnTotQty
  lnTranVal = 0
  lnDiffere = 0
ENDIF  
GOTO TOP
IF !EOF('MatInvJl')
  SCAN FOR !lLockFlg
    lcIRVar   = IIF(EMPTY(nIssued),'R','I')
    lnTranVal = IIF(lcIRVar='I',-nIssued*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nReceived  - nIssued 
    lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
  ENDSCAN
  lnDiffere = - 1 * ROUND(lnDiffere,2)
  IF lnDiffere # 0
    SELECT MatInvJl
    APPEND BLANK
    REPLACE cFabric    WITH lcFabric   ,;
            cColor     WITH lcColor    ,;
            cWareCode  WITH lcWareCode ,;
            Reference  WITH lcRefer    ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct  ,;
            dTranDate  WITH ldPostDate ,;
            dPostDate  WITH ldPostDate ,;
            cTranType  WITH lcTrType   ,;
            cTran      WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            cRSession  WITH IIF(lnDiffere<0,lcFJlSess,'')          ,;
            cISession  WITH IIF(lnDiffere>0,lcFJlSess,'')          ,;
            nStkVal    WITH lnDiffere                              ,;
            Reference  WITH lcRefer                                ,;
            cAdjReason WITH lcAdjCdRsn                             ,;
            cGlMatAdj  WITH lcAdjAcct                              ,;
            cTrn_Seq   WITH IIF(lnDiffere<0,cRSession,cISession)   ,;
            nMPrvSQty  WITH lnPrvQty                               ,;
            nPrvSVal   WITH lnPrvVal                               ,;
            LineNo     WITH lnLineNo


    *-- Call global function to add audit fields info.
    =gfAdd_Info('MatInvJl')

    =lfUpdGLDist(.F.,.T.)
    =lfFabWarDy()

  ENDIF
ENDIF

SET FILTER To &lcCurFlt.
SET ORDER TO (lcJourTag) IN MatInvJl
SELECT (lnCurAlias)

