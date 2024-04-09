*:**************************************************************************
*: Program file  : gfMatCrl.prg (E301121)
*: Program desc. : Material Stock Control Function,
*:                 Material Average Cost Update and Inventory Journal Update.
*:        System : ARIA APPAREL SYSTEM 2.7
*:        Module : Material (MA).
*:     Developer : AHM - Ahmed Amer
*:          Date : 11/23/98
*:**************************************************************************
*: Calls Functions    : lfJrRoData(), lfIsuJlTr()
*:                      lfUpdGLDist(), lfUpdStep().
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
*: RETURN Value   If all files was succesfuly updated and 
*:                uncomplete session checks was used,
*:                this value is the starting step after returning
*:                from the function. 
*:**************************************************************************
*: Example   : =gfMatCrl()
*:**************************************************************************
*: Modifications:-
*:B802450,1 HDM 08/03/1999 handle over receive in (onorder) qty for 
*:                         fabric & fabdye files as it writes negative values
*:B603049   AHM 07/04/1999 Fix the bug of not creating issuing record in physical
*:B603049                      when no changes has been made and not zero StkVal
*:E301235,1 WAM (Walid) 07/22/1999 Make all needed modifications to call lfJrRoData() 
*:E301235,1 from mamnrec.prg .
*:B603116,1 AHM 08/17/1999 make the last parameter array to hold the other
*:B603116,1                parameters exceed 25
*:B603124,1 AHM 08/23/1999 Exchange nPrvSQty in MatInvJl file with field nMPrvSQty
*:B603143,1 HDM 09/05/1999 Fixing Bugs of
*:                                 - Alias '' Not foung in Inventory transfer
*:                                 - SQL Column 'LineNo' Not Found
*:B603233,1 HDM 10/21/1999 lnOldOnOrd should hold the original qty of the Received
*:                         PO Line not the onorder Qty in the fabdye File
*:B603232,1 AHM 10/20/1999 Fix the bug of variable cRollID not found in case
*:B603232,1                 of issuing non Roll Fabric
*:B603245,1 HDM 10/25/1999 Fixing Bugs of Alias '' Not foung in physical Inventory
*:B603263,1 AHM 11/03/1999 add new parameter in other parameters array to 
*:B603263,1                dfferentiate between receiving po calling and 
*:B603263,1                receiving material manufacturing order
*:B603262,1 AHM 11/03/1999 Handling the roll screens while issuing return po
*:E401271   AHM 11/07/1999 Implement Mark Down
*:B603280,2 AHM 11/11/1999 Fix the bug of updating Rolls Qty with Buy Qty
*:B603280,2                instead of Use Qty
*:B603280,3 AHM 11/11/1999 prevent the user of using Negative sigen in roll balance
*:B603280,4 AHM 11/11/1999 Fix the bug of not accepting decimals in roll screen
*:B603280,5 AHM 11/11/1999 Fix the bug of open empty browse for roll if there
*:B603280,5                is no rolls
*:E301345,1 AHM 11/14/1999 Enhance the way we handle "Rolls" in Material control function
*:B603311,1 HDM 12/01/1999 make Material control take in consideration
*:                         receiving to another fabric
*:                         - Added 3 Variables to laOtherPar array
*:                           1- lcOrgFab ==> Holds the original Fabric
*:                           2- lcOrgClr ==> Holds the original Color
*:                           3- lcOrgDye ==> Holds the original Dyelot
*B802475,1 AMM 12/22/1999 Fix the bug of not updating MATINVJL with operation and Lot #
*B802955,1 HDM 02/03/2000 Make the adjustment able to receive to an existing roll
*B803012,1 HDM 02/10/2000 Redunduncy found in rolls file when receiving PO 
*                         for more than 1 line for fabrics using rolls
*E500329,1 WAB 08/06/2000 Generate Sequential numbers when receiving by rolls
*E500330,1 WAB 08/06/2000 Generate sequential numbers when doing physical or inv adj
*B603812,1 ABD 09/12/2000 When you in issue return and press rolls button , the available rolls 
*B603812,1 ABD            Screen will appear, That screen should browse the available 
*B603812,1 ABD            Rolls for that item but in Fact you will notice that 
*B603812,1 ABD            It doesn't show any available rolls.  
*B603813,1 ABD 09/13/2000 change wrong message.
*B603814,1 ABD 09/13/2000 Fix Bug 'Alias Not Found'
*B603249,1 ABD 09/24/2000 Fix bug that the program does not found the material/Color
*B603249,1 ABD            On fabdye file.
*B803712,1 ABD 10/03/2000 Generate Sequential numbers when receiving by rolls
*B803712,1 ABD            When press New and didn't open the rol id filed.
*B603950,1 ABD 10/15/2000 Fix Bug that didn't update the rolls file.
*B603981,1 ABD 10/19/2000 Fix bug that accept exist rolls.
*B604051,1 AAN 12/07/2000 Fix bug that when press button rolls (variable crollid not found)
*B604023,1 AAN 12/07/2000 when make physical inventory and save it (Alias not found)
*B604580,1 HBG 07/08/2001 Fix bug of Wrong Ordered qty on the locations level      
*B604678,1 MHM 08/20/2001 Fix the bug of on order QTY. is wrong in case of issue Return
*B605137,1 ADEL 28/11/01 Fix the bug of not accepting decimals when making minus adjustment.
*C200255,1 ADEL 23/12/01 Update MATINVJL with custom Voucher no for CON10.
*C200254,1 ABD 02/04/2002 Issue and receive the material manufacturing 
*C200254,1 ABD            Rework order from MAMNREC program.
*C200313,1 ABD 04/15/2002 Update 2 field in MATINVJL [cTran & cRsession ].
*N000388,1 KHM 06/11/2002 Modifications in gfMatCrl to support Material Sales Order Invoice screen.
*B606094,1 ABD 06/11/2002 Fix problem while update the rolls file.
*B606163,1 ABD 06/25/2002 Solve many problems in the material invoice sales order program.
*E301897,1 AMH 07/01/2002 Fix the problem of wrong updating the Mat WIP & Usage in Fabric&Fabdye 
*E301897,1                when the stock is below zero and changing the cost
*B605925,4 ABD 08/01/2002 Fix the bug of didn't saving a new recored for the locking.
*B606231,1 KHM 08/05/2002 Fix the bug of displaying the quantities in use instead of buy
*B606231,1                in case of material issue return.
*B606285,1 ABD 10/08/2002 Fix the problem of not being able to make physical inv. in case
*B606285,1                of "FI" costing method, and the problem of wrong figures in the
*B606285,1                MATINVJL when making adjstments.
*B606673,1 KHM 01/29/2003 Fix the bug of wrong receiving session when inventory transfer.
*B607338,1 AMH 06/30/2003 Fix the bug of inventory adjustment dont update any files.
*B121414,1 NNA 03/09/2004 Fix Bug of incorrectly updates for the Rolls File if we have more than one (PO)
*B121414,1 NNA            and they have the same Fabric/Color wareHouse and we received these
*B121414,1 NNA            POs in the same receiving session.
*B123252,1 NNA 06/17/2004 Fix bug of Erorr message when posting batch in inventory locking and the
*B123252,1 NNA            Costing method is FIFO,LIFO OR LOT and Keep track of roll [NO]
*B038753,1 KHM 12/18/2004 Add a new parameter lnTranCost in laOtherPar to hold the transaction cost
*B038753,1                in case of cost method is standard.
*B126598,1 ASH 02/27/2005 Fix bug of updating wrong cost in case of locking.
*B126107,1 NNA 03/01/2005 Fix bug that happen in cause of B#121414
*C126726,1 TMI 03/07/2005 Add a trigger to allow to update the apply field automatically for Restex
*C126356,1 NNA 03/15/2005 Custom mask for Roll-ID in the Material receiving PO
*B128927,1 TMI 08/14/2005 Use the absolute value of the nBalance field
*B607971,1 TMI 02/13/2007 send the correct parameter to the global function gfModalGen to show the correct button description (T20061117.0016 )
*B608030,1 ASH 04/04/2007 Set the correct index for Fabric file before checking the existance of a fabric.
*:****************************************************************************
*B603033 (Start)
*PARAMETERS lcTrType,lcFabric,lcColor,lcWareCode,lcFDyelot,;
           ldTrDate,ldPostDate,lcTrCode,;
           lnAdjStk,lnNewCost,lcRefer,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,;
           lcFJlSess,lcCShetTyp,lcPONo,lcCTRSess,lcCTISess,;
           lcRetPO,lcToWare,lcLastRSess

*E301235,1 (Start)
*PARAMETERS lcTrType,lcFabric,lcColor,lcWareCode,lcFDyelot,;
           ldTrDate,ldPostDate,lcTrCode,;
           lnAdjStk,lnNewCost,lcRefer,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,;
           lcFJlSess,lcCShetTyp,lcPONo,lcCTRSess,lcCTISess,;
           lcRetPO,lcToWare,lcLastRSess,llUseActCost


*B603116 (Start)
*PARAMETERS lcTrType,lcFabric,lcColor,lcWareCode,lcFDyelot,;
           ldTrDate,ldPostDate,lcTrCode,;
           lnAdjStk,lnNewCost,lcRefer,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,;
           lcFJlSess,lcCShetTyp,lcPONo,lcCTRSess,lcCTISess,;
           lcRetPO,lcToWare,lcLastRSess,llUseActCost,lnLineNo

PARAMETERS lcTrType,lcFabric,lcColor,lcWareCode,lcFDyelot,;
           ldTrDate,ldPostDate,lcTrCode,;
           lnAdjStk,lnNewCost,lcRefer,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,;
           lcFJlSess,lcCShetTyp,lcPONo,lcCTRSess,lcCTISess,;
           lcRetPO,lcToWare,lcLastRSess,laOtherPar

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

*E301345,1 (Start)
IF TYPE('llLastRec') = 'U'
  llLastRec = .T.
ENDIF
*E301345,1 (End)

*B603116 (End)

*E301235,1 (End)

*B603033 (End)

*B038753,1 KHM 12/18/2004 This parameter to hold the original transaction cost in case of
*B038753,1                material costing method is standard [Begin].
IF TYPE('lnTranCost') $ 'LU'
  lnTranCost = 0
ENDIF  
*B038753,1 KHM 12/18/2004 [End]


*B604580,1 HBG 07/08/2001 Get the value of Orginal WareHouse of the PO which sent as a parameter[Begin]
IF TYPE('lcPrvWare') # 'C'
  lcPrvWare = SPACE(0)
ENDIF
*B604580,1 [End]

*E401271 (Start)
IF TYPE('lnMarkQty') $ 'LU'
  lnMarkQty = 0
ENDIF
IF TYPE('lnMarkVal') $ 'LU'
  lnMarkVal = 0
ENDIF

*--- E401271 SSH (Start)
IF TYPE('llAdjYes') # 'L'
  llAdjYes = .F.
ENDIF
*--- E401271 SSH (END)
*E401271 (End)


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
*E301235,1 Don't overwrite lcTmpRoll in case of calling from MAMNREC.PRG[Start]
*PRIVATE lcOldWAr,llChekUncmp,lnSAveCost,;
        lnFOldStk,lnFOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,lcTmpJour,lcTmpRoll,lcFullRoll,lnRetStep,llUInvtry,lnFStkVal,lnWStkVal,lnCurRec,;
        lcFJlSess,llTrkRolls,lcCShetTyp,lcPONo,llWareHous,lcCTRSess,lcCTISess

*B603262,1 (Start)
*PRIVATE lcOldWAr,llChekUncmp,lnSAveCost,;
        lnFOldStk,lnFOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,lcTmpJour,lcFullRoll,lnRetStep,llUInvtry,lnFStkVal,lnWStkVal,lnCurRec,;
        lcFJlSess,llTrkRolls,lcCShetTyp,lcPONo,llWareHous,lcCTRSess,lcCTISess
PRIVATE lcOldWAr,llChekUncmp,lnSAveCost,;
        lnFOldStk,lnFOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,lnRetStep,llUInvtry,lnFStkVal,lnWStkVal,lnCurRec,;
        lcFJlSess,llTrkRolls,lcCShetTyp,lcPONo,llWareHous,lcCTRSess,lcCTISess
*B603262,1 (End)
*C200255,1 (Begin) Initialize voucher no variable for CON10
lcVoucNo = IIF(TYPE('lcVoucNo') = 'U' ,"",lcVoucNo)
*C200255,1 (End)


*E301235,1 Don't overwrite lcTmpRoll in case of calling from MAMNREC.PRG[End..]        
llWareHous = gfGetMemVar('M_WareHouse')='Y'
*E301235,1 Don't overwrite lcTmpRoll in case of calling from MAMNREC.PRG[Start]
*STORE SPACE(0) TO lcTmpJour,lcTmpRoll,lcFullRoll
IF TYPE('llExtCall')='U'
  *B603262,1 (Start)
  *PRIVATE lcTmpRoll
  PRIVATE lcTmpJour,lcTmpRoll,lcFullRoll
  *B603262,1 (End)
  STORE SPACE(0) TO lcTmpJour,lcTmpRoll,lcFullRoll
ELSE
  *B603262,1 (Start)
  *STORE SPACE(0) TO lcTmpJour,lcFullRoll
  *B603262,1 (End)
ENDIF
*E301235,1 Don't overwrite lcTmpRoll in case of calling from MAMNREC.PRG[End..]        
llTrkRolls = ALLTRIM(gfGetMemVar('M_TrkRolls')) = 'Y'

IF TYPE('lcFJlSess') = 'L'
  lcFJlSess = ''
ENDIF

*E500329,1/*E500330,1 WAB - get the llgenRolId dreom ma setup . if user choose create roll id 
*E500329,1/*E500330,1 WAB (START) manual or generated sequence no 
llGenRolId   = ('MA' $ gcCmpModules .AND. ALLTRIM(gfGetMemVar('M_GENROLID')) = 'N')
*E500329,1/*E500330,1 WAB (END)

*--Fabric and Warehouse Average Cost,Old Stock and Old Cost variables.
STORE 0 TO lnFOldStk,lnFOldCst
STORE 0 TO lnWOldStk,lnWOldCst
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.  
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcFDyelot  = IIF(EMPTY(lcFDyelot),SPACE(10),lcFDyelot)
llDyeLvl   = !EMPTY(lcFDyelot)

*--Check Uncomplete session flag if steps are passed as value not as zero.
llChekUncmp = ( lnStarStep <> 0 )    

*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,!EMPTY(laGLInvAry[1,1]))

*--Return step to continue for after exit the function.
lnRetStep   = 0

*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_MatCstMth')
*--Check the existing of the Fabric and
*--Point the record in Fabric and Fabric dyelot files.

*B603249,1 ABD Fix Bug that when item have a dyelot search with it. [Begin]
*IF ! SEEK(lcFabric+lcColor,'Fabric') OR ;
*   ! SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FabDye')
*!C200254,1 ABD - Fix bug that while we seek in the fabye with dyelot
*!C200254,1 ABD - The will get a roung value for the stock and stock amount. [Begin]
*IF !SEEK(lcFabric+lcColor,'Fabric') OR ;
*  ! SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot,'FabDye')
llFabWarDy = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot,'FabDye')
llFabWar   = SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FabDye')
*B608030,1 ASH 04/04/2007 (Begin) Set the correct index for Fabric file before checking the existance of a fabric.
SET ORDER TO Fabric IN Fabric
*B608030,1 ASH 04/04/2007 (End)
IF !SEEK(lcFabric+lcColor,'Fabric') .OR. !llFabWar .OR. !llFabWarDy
*!C200254,1 ABD - [End]
  *--The material/Color ???? record are missing,
  *--Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  lcMsgExp = lcFabric + "/" + lcColor
  *B607971,1 TMI [Start] send the correct button number
  *=gfModalGen('TRM36133B00036','DIALOG',lcMsgExp)
  =gfModalGen('TRM36133B36000','DIALOG',lcMsgExp)
  *B607971,1 TMI [End  ] 
  RETURN (0)
ENDIF
*B603249,1 ABD [End]
*--Check if MatInvJL file is Open.
llOpnJurnl=gfOpenFile(gcDataDir+"MatInvJl","MatInvJl","SH")


*--Get the Old Stock and Cost before updateing the new tansaction.
lnFOldStk = Fabric.OnHand
lnFOldCst = ABS(IIF(Fabric.OnHAnd=0,Fabric.nfAve_Cost,Fabric.nStkVal / Fabric.OnHand))
lnFStkVal = Fabric.nStkVal

*E401271 (Start)
IF lcTrType = '9'
  lnWOldStk = lnMarkQty
  lnWOldCst = ABS(IIF(lnMarkQty=0,FabDye.nfAve_Cost,lnMarkVal / lnMarkQty))
ELSE
*E401271 (End)
  lnWOldStk = FabDye.OnHand
  lnWOldCst = ABS(IIF(FabDye.OnHand=0,FabDye.nfAve_Cost,FabDye.nStkVal / FabDye.OnHand))
ENDIF
*E401271 (End)


*B802450,1 [Start] Declare a new variable to hold on order old qty
*B603233,1 [Start] lnOldOnOrd should hold the original qty of this PO Line
*lnOldOnOrd = FabDye.OnOrder
*B603263,1 (Start)
IF TYPE('lcProg') $ "UL"
  lcProg = SPACE(10)
ENDIF
*B603263,1 (End)
*B603233,1[START]
*IF lcTrType = '1' AND lcProg = 'MARECI'
IF lcTrType = '1'
  DO CASE
    CASE lcProg = 'MARECI'
      *B603233,1[END]
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
          
            *B603311,1[Start] Check the received Dyelots
            *B604678,1 MHM 08/20/2001 change the way we update OnOrder field in[Start]
            IF TranCd $ "234"
            *IF TranCd $ "234" AND Dyelot = PADR(lcFDyelot,10)
            *B604678,1 MHM [End]
            *B603311,1[End]
          
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
    *B603233,1 [START]
    CASE lcProg = 'MAMNREC'
      *-- Collect Data from mmfgordh and mmfgordd
      
      PRIVATE lcMFGTag,lcMFKey,lnAlias,lnMFGLin,lnRecLin,lnOldOnOrd
      *-- Save envionment setings
      lnAlias = SELECT(0)
      *!C200254,1 ABD Issue and receive the material manufacturing 
      *!C200254,1 ABD Rework order from MAMNREC program & I will
      *!C200254,1 ABD Not update the On order Qty. [Begin]
      *-- IF we will issue and recive rework order.
      IF TYPE('llRwIssRec') = 'L' .AND. !llRwIssRec
        *!C200254,1 ABD [End]
       
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
        
        *!C200254,1 ABD - Do the Same But In the rwork header file & rework line file. [Begin]
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
     *!C200254,1 ABD - [End]        
      
    OTHERWISE
      lnOldOnOrd = 0
ENDCASE
*ELSE
*  lnOldOnOrd = 0
*B603233,1[END]
ENDIF
*B603233,1 [End]
*B802450,1 [End]

*E401271 (Start)
*lnWStkVal = FabDye.nStkVal
lnWStkVal = IIF(lcTrType='9',lnMarkVal,FabDye.nStkVal)
*E401271 (End)

PRIVATE lnDyeCost
*E401271 (Start)
*lnDyeCost = IIF(FabDye.OnHand = 0,FabDye.nFAve_Cost,FabDye.nStkVal/FabDye.OnHand)
IF lcTrType = '9'
  lnDyeCost = IIF(lnMarkQty=0,FabDye.nFAve_Cost,lnMarkVal/lnMarkQty)
ELSE
  lnDyeCost = IIF(FabDye.OnHand = 0,FabDye.nFAve_Cost,FabDye.nStkVal/FabDye.OnHand)
ENDIF  
*E401271 (End)

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

*B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
*IF lcTrType = '3'
IF lcTrType $ '39'
*B123252,1 NNA (End)

  SELECT FabDye
  IF !EMPTY(lcFDyelot)
    = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot,'FabDye')
  ENDIF
  lnTmpOStk = OnHand
ENDIF

*E401271 (Start)
IF lcTrType = '9'
  lnTmpOStk = lnMarkQty
ENDIF  
*E401271 (End)

*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
lcIRType = IIF(lnAdjStk<0 , 'I' , 'R' )

*--Calculate Transaction Issuing Cost in differend cost methods.
IF lcIRType = 'I'
  DO CASE
    CASE lcCostMeth = 'A'   && Average.
      *B603033 (Start)
      *lnNewCost = IIF(llWareHous,lnWOldCst,lnFOldCst)
      
      *B603116 (Start)
      *IF !llUseActCost
      IF !llUseACst
      *B603116 (End)
        lnNewCost = IIF(llWareHous,lnWOldCst,lnFOldCst)
      ENDIF
      *B603033 (End)
                                   

    CASE lcCostMeth = 'S'   && Standard.
      *B603033 (Start)
      *lnNewCost = Fabric.CostUse
      *B603116 (Start)
      *IF !llUseActCost
      IF !llUseACst
      *B603116 (End)
        lnNewCost = Fabric.CostUse
      ENDIF
      *B603033 (End)

    CASE lcCostMeth $ 'FIL' AND lcTrType <> '4'
      *B603262,1 (Start)
      *IF ! lfJrRoData()
      
      *B603262,1 (Start)
      *comment this part of code and rewrite it
      *IF !(lcTrType = '1' AND lnAdjStk < 0 AND TYPE('llExtCall')='L' AND llExtCall) AND ! lfJrRoData()
      **B603262,1 (End)
      *  SELECT (lcOldWAr)
      *  RETURN (0)
      *ENDIF
      
      *N000388,1 KHM (Begin) Include the type of materila invoice lcTrType $ '15'
      *IF !(lcTrType = '1' AND lnAdjStk < 0 AND TYPE('llExtCall')='L' AND llExtCall)
      IF !(lcTrType $ '15' AND lnAdjStk < 0 AND TYPE('llExtCall')='L' AND llExtCall)
      *N000388,1 KHM (End) 
            
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
      *B603262,1 (End)

    CASE lcCostMeth $ 'FIL' AND lcTrType = '4'
      *--Receiving cost will be passed as parameter in this function.
  ENDCASE
ELSE
  
  *B603245,1 (Start) We don't need to check tracking rolls
  *IF lcTrType <> '4' AND llTrkRolls .AND. Fabric.ltrkrolls
  IF lcTrType <> '4'
  *B603245,1 (End)

    *E301235,1 (Start) AHM 07/20/1999
    *IF !lfJrRoData()
    
    IF (TYPE('llExtCall')='U' OR !llExtCall) AND !lfJrRoData()
    *E301235,1 (End) AHM 07/20/1999
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
    
    *E301235,1 (Start)
    *IF SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+lcFJlSess+SPACE(6),"MatInvJL"))
    *  lcFJlSess = gfSEQUENCE('GLSession')
    *ENDIF
    IF SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+lcFJlSess+SPACE(6),"MatInvJL"))

      *B606094,1 ABD - We should Select the MatInvJL before locate on it , this will
      *B606094,1 ABD - Make a bug in case we make phisical Inv.  [Begin]
      Private lnOldAlias
      lnOldAlias = SELECT(0)
      SELECT MatInvJL
      *B606094,1 ABD - [End]

	  *B121414,1 NNA 03/09/2004 (Begin) Add the PO# NO to the filter to get the correct Record if we receive
	  *B121414,1 NNA            multi POs. in the same session with the same (cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,lineNo)
      *LOCATE REST WHILE cFabric +cColor +cWareCode +cDyelot  +cRSession+cISession = ;
                        lcFabric+lcColor+lcWareCode+lcFDyelot+lcFJlSess+SPACE(6)    ;
                  FOR   LineNo = lnLineNo
      LOCATE REST WHILE cFabric +cColor +cWareCode +cDyelot  +cRSession+cISession = ;
                        lcFabric+lcColor+lcWareCode+lcFDyelot+lcFJlSess+SPACE(6)    ;
                  FOR   LineNo = lnLineNo .AND. cTran = lctrcode
	  *B121414,1 NNA (End)

      IF FOUND()            
        lcFJlSess = gfSEQUENCE('GLSession')
      ENDIF
      
      *B606094,1 ABD - Select the Old Alias befor complet. [Begin]
      SELECT (lnOldAlias)
      *B606094,1 ABD - [End]

    ENDIF
    *E301235,1 (End)

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

*--Initialize next step to continue.
lnTmpStp = lnStarStep
*--Update journal for Issue Transaction ,FIFO or LIFO method.
IF lcIRType = 'I'

  = lfIssue()

ELSE  && Rec. with in any costing or issu in "S,A"

  = lfReceive()

ENDIF

*-- 5) Update Rolls File
*B606163,1 ABD - Don't do the following lines in case I'm type 5 & recive. [Begin]
*IF lcTrType <> '4'
IF !(lcTrType $ '45') .OR. (lcTrType = '5' .AND. lcIRType = 'I')
*B606163,1 ABD - [End]

  IF (llTrkRolls .AND. Fabric.ltrkrolls)
    *--Initialize next step to continue.
    lnTmpStp = lnStarStep + 4
  
    IF lfCheckUnCmp(lnTmpStp)

      *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
      *IF lcTrType = '3'
      IF lcTrType $ '39'
      *B123252,1 NNA (End)
        = lfUpdtRoll(.T.)
      ENDIF

      = lfUpdtRoll()
      
    ENDIF
  ENDIF
ENDIF

*--Initialize next step to continue when return.
lnRetStep = lnTmpStp

*E301345,1 (Start)
IF TYPE('llExtCall') = 'U' OR !llExtCall OR llLastRec
*E301345,1 (End)
  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpJour+'.DBF')
  ERASE (gcWorkDir+lcTmpJour+'.CDX')

  IF USED(lcTmpRoll)
    USE IN (lcTmpRoll)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpRoll+'.DBF')
  ERASE (gcWorkDir+lcTmpRoll+'.CDX')

  IF USED(lcFullRoll)
    USE IN (lcFullRoll)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcFullRoll+'.DBF')
  ERASE (gcWorkDir+lcFullRoll+'.CDX')

*E301345,1 (Start)
ENDIF
*E301345,1 (End)

SELECT (lcOldWAr)
RETURN IIF(llChekUncmp , lnRetStep , 1 )

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

*B606163,1 ABD - Select the lctmproll file in case we are type '5' , that ,
*B606163,1 ABD - Because we work with this file only in case we save the material invoice.  [Begin]
*lcRoTmpUse = IIF(lnAdjStk<0 OR llPhyIssu,lcFullRoll,lcTmpRoll)
lcRoTmpUse = IIF((lnAdjStk<0 OR llPhyIssu) .AND. lcTrType # '5',lcFullRoll,lcTmpRoll)
*B606163,1 ABD - [End]

*-- add new records in the Rolls file from lcTmpRolls
*B121414,1 NNA 03/09/2004 (Begin) add new variable to hold the seek expression if we case of receiving
*PRIVATE lnAlias,lcRolTag
PRIVATE lnAlias,lcRolTag,lcScan
*B121414,1 NNA (End)

lnAlias = SELECT(0)
lcRolTag = ORDER('Rolls')
SET ORDER TO TAG ROLLITEM IN Rolls
SELECT (lcRoTmpUse)

*B803012,1 [Start] Scan for the passed key only as the temp. file in case of receiving PO
*                  Contains the full transaction rolls
*SCAN
GO TOP

*B121414,1 NNA 03/09/2004 (Begin) Add PO# to the seek expression because if there are more one po in the same session 
*B121414,1 NNA 			          the function make a duplication in records
*SCAN FOR cfabric+ccolor+cwarecode+cdyelot = lcFabric+lcColor+lcWareCode+lcFDyelot
*B803012,1 [End]
*IF !llPhyIssu AND SEEK(&lcRoTmpUse..cFabric+&lcRoTmpUse..cColor+;
                        &lcRoTmpUse..cWareCode+&lcRoTmpUse..cDyelot+;
                        &lcRoTmpUse..cRollId+'1','ROLLS')

IF lcTrType='1'			&& if we are in the case of Receive Material PO or Receive Return Material PO
  *B126107,1 NNA 03/01/2005 (BEGIN) in case we Receive a material Manufacturing order , file lcTmpPo not used So
  *B126107,1 NNA             I'm using now Parameter lcTrcode
  *lcScan = 'cfabric+ccolor+cwarecode+cdyelot = lcFabric+lcColor+lcWareCode+lcFDyelot .AND. Ctran=&lcTmpPo..PoMat'
  lcScan = 'cfabric+ccolor+cwarecode+cdyelot = lcFabric+lcColor+lcWareCode+lcFDyelot .AND. Ctran=lcTrCode'
  *B126107,1 NNA (END)
ELSE
  lcScan = 'cfabric+ccolor+cwarecode+cdyelot = lcFabric+lcColor+lcWareCode+lcFDyelot'
ENDIF
SCAN FOR &lcSCAN
*--NNA Seek for the (lcRoTmpUse..cRollId) not for (lcRoTmpUse..cRollId+'1') because it is already exist in the tmp. file
  IF !llPhyIssu AND SEEK(&lcRoTmpUse..cFabric+&lcRoTmpUse..cColor+;
                         &lcRoTmpUse..cWareCode+&lcRoTmpUse..cDyelot+;
                         &lcRoTmpUse..cRollId,'ROLLS')
*B121414,1 NNA (End)

    SELECT ROLLS
    IF lcTrType='3'
      *-- lcRoTmpUse is "lcTmpRoll"
      *B603280,2 (Start)
      *REPLACE nQtyBal WITH ABS(&lcRoTmpUse..nApply)
      REPLACE nQtyBal WITH ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='1',Fabric.Conv,1)
      *B603280,2 (Start)
    ELSE
      *-- lcRoTmpUse is either "lcTmpRoll" or "lcFullRoll"
      *-- IF it is receiving lcRoTmpUse is "lcTmpRoll"
      *-- IF it is issuing   lcRoTmpUse is "lcFullRoll"
      *B603280,2 (Start)
      *REPLACE nQtyBal WITH nQtyBal + IIF(&lcRoTmpUse..RolTranCd $ '13',;
                                         ABS(&lcRoTmpUse..nApply),;
                                         -ABS(&lcRoTmpUse..nApply))
      *B802955,1 [Start] re-write this part to handle applying the qty to an
      *                  existing roll while adjustments
      *REPLACE nQtyBal WITH nQtyBal + (IIF(&lcRoTmpUse..RolTranCd $ '13',;
                                         ABS(&lcRoTmpUse..nApply),;
                                         -ABS(&lcRoTmpUse..nApply)) * IIF(lcTrType='1',Fabric.Conv,1))
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

      *B606094,1 ABD - To issue The Qty From the Atock with sell Conv.[Begin]
      IF lcTrType='5'
        lnRToApply = lnRToApply * m.nsellConv
      ENDIF
      *B606094,1 ABD - [End]
      
      REPLACE nQtyBal WITH nQtyBal + lnRToApply
      *B802955,1 [End]
      *B603280,2 (End)
    ENDIF
  ENDIF

  lnTrQty = ABS(IIF(llPhyIssu,&lcRoTmpUse..nBalance,&lcRoTmpUse..nApply))
  IF lnTrQty > 0

    *E500329,1 WAB - replace the roll Id in the temp file with the sequence no cause in case of 
    *E500329,1 WAB - (START)recieving po we create a temp. sequence no cause we used a temp file in this case
    *B603814,1 ABD - Fix bug that when return from exist roll don't generate sequence no. [Begin]
    *IF lcTrType = '1' .AND. llGenRolId
    IF lcTrType = '1' .AND. llGenRolId .AND. lcKeyType  = 'P'
    *B603814,1 ABD [End]
      REPLACE &lcRoTmpUse..cRollId WITH gfSequence('CROLLID')
    ENDIF
    *E500329,1 WAB - (END)
    
    SELECT ROLLS
    *B606094,1 ABD - [Begin]
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
    *B606094,1 ABD - [End]

    *B603280,2 (Start)
    *REPLACE cRollItem WITH &lcRoTmpUse..cFabric    ,;
            Color     WITH &lcRoTmpUse..cColor     ,;
            cWareCode WITH &lcRoTmpUse..cWareCode  ,;
            Dyelot    WITH &lcRoTmpUse..cDyelot    ,;
            cRollId   WITH &lcRoTmpUse..cRollId    ,;
            nQty      WITH lnTrQty,;
            nQtyBal   WITH lnTrQty,;
            TranCd    WITH IIF(llPhyIssu,'2',&lcRoTmpUse..RolTranCd)  ,;
            cSession  WITH lcFJlSess              ,;
            cRSession WITH IIF(TranCd $ '13',lcFJlSess,&lcRoTmpUse..cRSession),;
            cISession WITH IIF(TranCd = '2' ,lcFJlSess,cISession)

     *B606094,1 ABD - Multi. the Qty & Balnace Qty * in the Sell Conv.[Begin]
     *REPLACE cRollItem WITH &lcRoTmpUse..cFabric    ,;
              Color     WITH &lcRoTmpUse..cColor     ,;
              cWareCode WITH &lcRoTmpUse..cWareCode  ,;
              Dyelot    WITH &lcRoTmpUse..cDyelot    ,;
              cRollId   WITH &lcRoTmpUse..cRollId    ,;
              nQty      WITH lnTrQty * IIF(lcTrType='1',Fabric.Conv,1)  ,;
              nQtyBal   WITH lnTrQty * IIF(lcTrType='1',Fabric.Conv,1)  ,;
              TranCd    WITH IIF(llPhyIssu,'2',&lcRoTmpUse..RolTranCd)  ,;
              cSession  WITH lcFJlSess              ,;
              cRSession WITH IIF(TranCd $ '13',lcFJlSess,&lcRoTmpUse..cRSession),;
              cISession WITH IIF(TranCd = '2' ,lcFJlSess,cISession)
     REPLACE cRollItem WITH &lcRoTmpUse..cFabric     ,;
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
   SET ORDER TO &lcOldOrder
   *B606094,1 ABD - [End]

  *B603280,2 (End)
  ENDIF
ENDSCAN

*B603262,1 (Start)
IF !EMPTY(lcToWare)
  SELECT (lcTmpRoll)
  SCAN FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcToWare+lcFDyelot+'??????',;
                cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession)
    SELECT Rolls
    IF SEEK(&lcTmpRoll..cFabric+&lcTmpRoll..cColor+&lcTmpRoll..cWareCode+;
            &lcTmpRoll..cDyelot+&lcTmpRoll..cRollId+'1')
      *B603280,2 (Start)
      *REPLACE nQtyBal WITH nQtyBal + ABS(&lcRoTmpUse..nApply)
      REPLACE nQtyBal WITH nQtyBal + (ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='1',Fabric.Conv,1))
      *B603280,2 (End)
    ENDIF
    IF &lcTmpRoll..nApply > 0
      APPEND BLANK
      *B603280,2 (Start)
      *REPLACE cRollItem WITH &lcTmpRoll..cFabric    ,;
              Color     WITH &lcTmpRoll..cColor     ,;
              cWareCode WITH &lcTmpRoll..cWareCode  ,;
              Dyelot    WITH &lcTmpRoll..cDyelot    ,;
              cRollId   WITH &lcTmpRoll..cRollId    ,;
              nQty      WITH &lcTmpRoll..nApply     ,;
              nQtyBal   WITH &lcTmpRoll..nApply     ,;
              TranCd    WITH &lcTmpRoll..RolTranCd  ,;
              cSession  WITH lcFJlSess              ,;
              cRSession WITH lcFJlSess


      *C200313,1 ABD - Update 2 field in MATINVJL [cTran & cRsession ] i will define 
      *C200313,1 ABD - New variable to know if I will update or not. [Begin]
      llUptRcRls = .F.
      IF ASCAN(laEvntTrig,PADR("UPMATRLS",10)) <> 0 .AND. lcCostMeth = 'L'
        llUptRcRls = .T.
          =gfDoTriger('MAINVCT',PADR('UPMATRLS',10))
      ENDIF   
      IF !llUptRcRls
        *C200313,1 ABD - [End]
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
        *C200313,1 ABD - End For IF statment. [Begin]
       ENDIF
        *C200313,1 ABD - [End]                
    *B603280,2 (End)
    ENDIF
  ENDSCAN
ENDIF
*B603262,1 (End)

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
*B604678,4 MHM 09/27/2001 call this function to get unit cost value in case of Issue[Start]

*N000388,1 KHM (Begin) Include the material invoice lcTrType $ '15'
*IF lcIRType = 'I' AND lcTrType = '1'
IF lcIRType = 'I' AND lcTrType $ '15'
*N000388,1 KHM (End)

  lnStkVal = -1* (lnAdjStk * lnNewCost)
  lnCost   = lnNewCost
ENDIF
*B604678,4 [END]

*E401271 (Start)
*lcReIsFld = IIF(lnWOldStk<0 AND lcTrType='3',"nReceived","nIssued")
lcReIsFld = IIF(lnWOldStk<0 AND lcTrType $ '93',"nReceived","nIssued")
*E401271 (End)
*-- HDM B603143,1 [Start] Check the lnWOldStk <> 0 
*IF (lcCostMeth $ 'FIL' AND lcTrType <> '4')
IF (lcCostMeth $ 'FIL' AND lcTrType <> '4') AND lnWOldStk <> 0
  *-- HDM B603143,1 [End]

  SELECT (lcTmpJour)
  *B603950,1 ABD Fix Bug that didn't update the MatinvJl file with correct value. [Begin]
  *SCAN
  SCAN FOR cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession = ;
           lcFabric+lcColor+lcWareCode+lcFDyelot
    *B603950,1 ABD [End]
    IF lfCheckUnCmp(lnTmpStp)
      SELECT MatInvJl
      APPEND BLANK
      *B603124 (Start)
      *REPLACE cFabric    WITH lcFabric   ,;
              cColor     WITH lcColor    ,;
              cWareCode  WITH lcWareCode ,;
              cDyelot    WITH lcFDyelot  ,;
              Reference  WITH lcRefer    ,;
              cAdjReason WITH lcAdjCdRsn ,;
              cGlMatAdj  WITH lcAdjAcct  ,;
              dTranDate  WITH ldTrDate   ,;
              dPostDate  WITH ldPostDate ,;
              cTranType  WITH lcTrType   ,;
              cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
              nUnitCost  WITH &lcTmpJour..nUnitCost      ,;
              nUntCstBuy WITH &lcTmpJour..nUntCstBuy     ,;
              cRSession  WITH &lcTmpJour..cRSession      ,;
              cISession  WITH IIF(lcTrType='4',lcCTISess,lcFJlSess),;
              &lcReIsFld WITH IIF(lcTrType='3',ABS(&lcTmpJour..nBalance),ABS(&lcTmpJour..nApply)) ,;
              nStkVal    WITH - &lcReIsFld * nUnitCost   ,;
              dPostDate  WITH ldPostDate                 ,; 
              cTrn_Seq   WITH IIF(EMPTY(cISession),cRSession,cISession),;
              cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
              cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
              nPrvSQty   WITH lnPrvQty ,;
              nPrvSVal   WITH lnPrvVal 
      *E401271 (Start)
      *REPLACE cFabric    WITH lcFabric   ,;
              cColor     WITH lcColor    ,;
              cWareCode  WITH lcWareCode ,;
              cDyelot    WITH lcFDyelot  ,;
              Reference  WITH lcRefer    ,;
              cAdjReason WITH lcAdjCdRsn ,;
              cGlMatAdj  WITH lcAdjAcct  ,;
              dTranDate  WITH ldTrDate   ,;
              dPostDate  WITH ldPostDate ,;
              cTranType  WITH lcTrType   ,;
              cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
              nUnitCost  WITH &lcTmpJour..nUnitCost      ,;
              nUntCstBuy WITH &lcTmpJour..nUntCstBuy     ,;
              cRSession  WITH &lcTmpJour..cRSession      ,;
              cISession  WITH IIF(lcTrType='4',lcCTISess,lcFJlSess),;
              &lcReIsFld WITH IIF(lcTrType='3',ABS(&lcTmpJour..nBalance),ABS(&lcTmpJour..nApply)) ,;
              nStkVal    WITH - &lcReIsFld * nUnitCost   ,;
              dPostDate  WITH ldPostDate                 ,; 
              cTrn_Seq   WITH IIF(EMPTY(cISession),cRSession,cISession),;
              cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
              cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
              nMPrvSQty  WITH lnPrvQty ,;
              nPrvSVal   WITH lnPrvVal 

      *B606094,1 ABD - We should take type '5' , this type fro Invoice. [Begin]
      *REPLACE cFabric    WITH lcFabric   ,;
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
              nUnitCost  WITH &lcTmpJour..nUnitCost      ,;
              nUntCstBuy WITH &lcTmpJour..nUntCstBuy     ,;
              cRSession  WITH &lcTmpJour..cRSession      ,;
              cISession  WITH IIF(lcTrType='4',lcCTISess,lcFJlSess),;
              &lcReIsFld WITH IIF(lcTrType='3',ABS(&lcTmpJour..nBalance),ABS(&lcTmpJour..nApply)) ,;
              nStkVal    WITH - &lcReIsFld * nUnitCost   ,;
              dPostDate  WITH ldPostDate                 ,; 
              cTrn_Seq   WITH IIF(EMPTY(cISession),cRSession,cISession),;
              cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
              cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
              nMPrvSQty  WITH lnPrvQty ,;
              nPrvSVal   WITH lnPrvVal ,;
              llockflg   WITH lcTrType='9'
      
      *B606231,1 KHM 08/08/2002 (Begin) In case of return PO divide the unit cost by the conversion factor.              
      *REPLACE cFabric    WITH lcFabric   ,;
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
              nUnitCost  WITH IIF(lcTrType='5',&lcTmpJour..nUnitCost/m.nSellconv,&lcTmpJour..nUnitCost),;
              nUntCstBuy WITH &lcTmpJour..nUntCstBuy     ,;
              cRSession  WITH &lcTmpJour..cRSession      ,;
              cISession  WITH IIF(lcTrType='4',lcCTISess,lcFJlSess)

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
      *B606231,1 KHM 08/08/2002 (End)
      
      *B606231,1 KHM 08/08/2002 (Begin) In case of return PO multiply the applied qty with 
      *B606231,1                the conversion factor.              
      *REPLACE &lcReIsFld WITH &lcReIsFld + IIF(lcTrType='3',ABS(&lcTmpJour..nBalance)         ,;
              IIF(lcTrType='5',ABS(&lcTmpJour..nApply) * m.nSellconv,ABS(&lcTmpJour..nApply))),;
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
      *B606231,1 KHM 08/08/2002 (End)
      
      *B606094,1 ABD - [End]
      *B038753,2 ASH 12/18/2004 (Begin) Make sure to save stk value in received record with +ve amount.
      IF UPPER(lcReIsFld) = 'NRECEIVED' AND lcTrType='9'
        REPLACE nStkVal WITH ABS(nStkVal)
      ENDIF
      *B038753,2 ASH 12/18/2004 (End)
      *E401271 (End)
      *B603124 (End)
      *B802475,1 AMM Update the operation code and lot #
      IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
        REPLACE cOprCode  WITH lcOprCode ,;
                cLotNo    WITH lcLotNo
      ENDIF
      *B802475,1 AMM end

      *C200255,1 (Begin) Update voucher no variable for CON10.
      IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
         REPLACE cvoucherno WITH lcVoucNo
      ENDIF   
      *C200255,1 (End)

      *-- Call global function to add audit fields info.
      =gfAdd_Info('MATINVJL')

      *--Update and Flush Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  

    ENDIF  
    lnTmpStp = lnTmpStp + 1

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfFabWarDy()
  ENDSCAN
  
  IF !EMPTY(lcToWare)
    SELECT (lcTmpJour)
    SCAN
      IF lfCheckUnCmp(lnTmpStp)
        SELECT MatInvJl
        APPEND BLANK
        *B603124 (Start)
        *REPLACE cFabric    WITH lcFabric   ,;
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
                nPrvSQty   WITH lnPrvQty                   ,;
                nPrvSVal   WITH lnPrvVal
        *C200313,1 ABD - Update 2 field in MATINVJL [cTran & cRsession ] i will define 
        *C200313,1 ABD - New variable to know if I will update or not. [Begin]
        llUptRcMat = .F.
        IF ASCAN(laEvntTrig,PADR("UPMATINV",10)) <> 0 .AND. lcCostMeth = 'L'
          llUptRcMat = .T.
            =gfDoTriger('MAINVCT',PADR('UPMATINV',10))
        ENDIF   
        IF !llUptRcMat
           *C200313,1 ABD - [End]

           *B606673,1 KHM 01/29/2003 (Begin) Replacing the cRSession with lcFJlSess instead
           *B606673,1                of new sequence.
           *REPLACE cFabric    WITH lcFabric   ,;
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
                   cRSession  WITH lcFJlSess                  ,;
                   nReceived  WITH ABS(&lcTmpJour..nApply)    ,;
                   nStkVal    WITH nReceived * nUnitCost      ,;
                   dPostDate  WITH ldPostDate                 ,; 
                   cTrn_Seq   WITH cRSession                  ,;
                   nMPrvSQty WITH lnPrvQty                    ,;
                   nPrvSVal  WITH lnPrvVal 
           *B606673,1 KHM 01/29/2003 (End)        
           *C200313,1 ABD - End For IF statment. [Begin]
         ENDIF
         *C200313,1 ABD - [End]
        
        *B603124 (End)
        *B802475,1 AMM Update the operation code and lot #
        IF TYPE('lcLotNo') # 'U'  .AND. TYPE('lcOprCode') # 'U'
          REPLACE cOprCode  WITH lcOprCode ,;
                  cLotNo    WITH lcLotNo
        ENDIF
        *B802475,1 AMM end

        *C200255,1 (Begin) Update voucher no variable for CON10.
        IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
           REPLACE cvoucherno WITH lcVoucNo
        ENDIF   
        *C200255,1 (End)
        *-- Call global function to add audit fields info.
        =gfAdd_Info('MATINVJL')

        *--Update and Flush Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

      ENDIF  
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfFabWarDy(.T.)
    ENDSCAN

  ENDIF
  
ELSE
  IF lfCheckUnCmp(lnTmpStp)
    SELECT MatInvJl
    APPEND BLANK
    *B603124 (Start)
    *REPLACE cFabric    WITH lcFabric  ,;
            cColor     WITH lcColor   ,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcFDyelot ,;
            dTranDate  WITH ldTrDate  ,;
            dPostDate  WITH ldPostDate,;
            cTranType  WITH lcTrType  ,;
            cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode)     ,;
            nUnitCost  WITH IIF(lcTrType='1',lnCost,IIF(lcTrType = '3',lnWOldCst,lnNewCost)) ,;
            nUntCstBuy WITH IIF(lcTrType='1',lnCost,lnNewCost) * Fabric.Conv,;
            &lcReIsFld WITH IIF(lcTrType='3',ABS(lnTmpOStk),ABS(lnAdjStk)),;
            nStkVal    WITH - IIF(lcTrType='1',lnStkVal,IIF(lcTrType='3',lnWStkVal,&lcReIsFld * nUnitCost)) ,;
            Reference  WITH lcRefer   ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct  ,;
            cISession  WITH IIF(lcTrType = '4',lcCTISess,lcFJlSess) ,;
            cRSession  WITH IIF(lcTrType = '4',lcCTRSess,'') ,;
            cTrn_Seq   WITH cISession ,;
            cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
            cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
            nPrvSQty   WITH lnPrvQty ,;
            nPrvSVal   WITH lnPrvVal
    *E401271 (Start)
    *REPLACE cFabric    WITH lcFabric  ,;
            cColor     WITH lcColor   ,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcFDyelot ,;
            dTranDate  WITH ldTrDate  ,;
            dPostDate  WITH ldPostDate,;
            cTranType  WITH lcTrType  ,;
            cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode)     ,;
            nUnitCost  WITH IIF(lcTrType='1',lnCost,IIF(lcTrType = '3',lnWOldCst,lnNewCost)) ,;
            nUntCstBuy WITH IIF(lcTrType='1',lnCost,lnNewCost) * Fabric.Conv,;
            &lcReIsFld WITH IIF(lcTrType='3',ABS(lnTmpOStk),ABS(lnAdjStk)),;
            nStkVal    WITH - IIF(lcTrType='1',lnStkVal,IIF(lcTrType='3',lnWStkVal,&lcReIsFld * nUnitCost)) ,;
            Reference  WITH lcRefer   ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct  ,;
            cISession  WITH IIF(lcTrType = '4',lcCTISess,lcFJlSess) ,;
            cRSession  WITH IIF(lcTrType = '4',lcCTRSess,'') ,;
            cTrn_Seq   WITH cISession ,;
            cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
            cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
            nMPrvSQty  WITH lnPrvQty ,;
            nPrvSVal   WITH lnPrvVal

    *B605925,4 ABD - Fix the bug of didn't saving a new recored for the locking. [Begin]
    *REPLACE cFabric    WITH lcFabric ,;
    *        cColor     WITH lcColor  ,;
    *        cWareCode  WITH lcWareCode,;
    *        cDyelot    WITH lcFDyelot ,;
    *        dTranDate  WITH lDTrDate  ,;
    *        dPostDate  WITH ldPostDate,;
    *        cTranType  WITH lcTrType  ,;
    *        cTran      WITH IIF(cTranType $ "239" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
    *        nUnitCost  WITH IIF(lcTrType='1',lnCost,IIF(lcTrType $ '39',lnWOldCst,lnNewCost)),;
    *        nUntCstBuy WITH IIF(lcTrType='1',lnCost,IIF(lcTrType $ '39',lnWOldCst,lnNewCost)) * Fabric.Conv,;
    *        &lcReIsFld WITH IIF(lcTrType $ '39',ABS(lnTmpOStk),ABS(lnAdjStk)),;
    *        nStkVal    WITH - IIF(lcTrType='1',lnStkVal,IIF(lcTrType$'39',lnWStkVal,&lcReIsFld * nUnitCost)) ,;
    *        Reference  WITH lcRefer   ,;
    *        cAdjReason WITH lcAdjCdRsn ,;
    *        cGlMatAdj  WITH lcAdjAcct  ,;
    *        cISession  WITH IIF(lcTrType = '4',lcCTISess,lcFJlSess) ,;
    *        cRSession  WITH IIF(lcTrType = '4',lcCTRSess,'') ,;
    *        cTrn_Seq   WITH cISession ,;
    *        cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
    *        cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
    *        nMPrvSQty  WITH lnPrvQty ,;
    *        nPrvSVal   WITH lnPrvVal            
    *        REPLACE llockflg   WITH lcTrType='9'

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

    *B038753,2 ASH 12/18/2004 (Begin) Make sure to save stk value in received record with +ve amount.
    IF UPPER(lcReIsFld) = 'NRECEIVED' AND lcTrType = '9'
      REPLACE nStkVal WITH ABS(nStkVal)
    ENDIF
    *B038753,2 ASH 12/18/2004 (End)
    *B605925,4 ABD - [End]            
    *E401271 (End)
    *B603124 (End)   
    
    *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
    *B038753,1                transaction cost in case of standard costing method [Begin].
    REPLACE nTranCost WITH lnTranCost
    *B038753,1 KHM 12/18/2004 [End]
     
      *B802475,1 AMM Update the operation code and lot #
      IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
        REPLACE cOprCode  WITH lcOprCode ,;
                cLotNo    WITH lcLotNo
      ENDIF
      *B802475,1 AMM end

      *C200255,1 (Begin) Update voucher no variable for CON10.
      IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
         REPLACE cvoucherno WITH lcVoucNo
      ENDIF   
      *C200255,1 (End)

    *-- Call global function to add audit fields info.
    =gfAdd_Info('MatInvJl')

    *--Update and Flush Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  

  ENDIF
  lnTmpStp = lnTmpStp + 1
  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()

  *B605925,4 ABD - Call function to Update the Material inv. Journal with  issue 
  *B605925,4 ABD - Recored in case we lock the item. [Begin]
  IF lcIRType = 'I' .AND. lcTrType  = '9'
    = lfRcvLock ()
  ENDIF
  *B605925,4 ABD - [End]


  IF !EMPTY(lcToWare)
    IF lfCheckUnCmp(lnTmpStp)
      SELECT MatInvJl
      APPEND BLANK
      *B603124 (Start)
      *REPLACE cFabric    WITH lcFabric  ,;
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
              nPrvSQty   WITH lnPrvQty   ,;
              nPrvSVal   WITH lnPrvVal 
            
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
      *B603124 (End)
      *B802475,1 AMM Update the operation code and lot #
      IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
        REPLACE cOprCode  WITH lcOprCode ,;
                cLotNo    WITH lcLotNo
      ENDIF
      *B802475,1 AMM end

      *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
      *B038753,1                transaction cost in case of standard costing method [Begin].
      REPLACE nTranCost WITH lnTranCost
      *B038753,1 KHM 12/18/2004 [End]


      *C200255,1 (Begin) Update voucher no variable for CON10.
      IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
         REPLACE cvoucherno WITH lcVoucNo
      ENDIF   
      *C200255,1 (End)

      *-- Call global function to add audit fields info.
      =gfAdd_Info('MatInvJl')

      *--Update and Flush Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  

    ENDIF
    lnTmpStp = lnTmpStp + 1
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
*E401271 (Start)
*IF lcTrType = '3'
IF lcTrType $ '39'
*E401271 (End)
  IF lfDoPhys('I')
    =lfIssue()
    STORE 0 TO lnPrvQty,lnPrvVal
  ENDIF
ELSE
  IF lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    = lfAdjRec()
  ENDIF
ENDIF

*E401271 (Start)
*IF lcTrType # '3' OR (lcTrType = '3' AND lfDoPhys('R'))
IF !(lcTrType $ '39') OR (lcTrType $ '39' AND lfDoPhys('R'))
*E401271 (End)
  *--Create a main record in journal file.
  IF lfCheckUnCmp(lnTmpStp)
    SELECT MatInvJl
    APPEND BLANK
    *B603124 (Start)
    *REPLACE cFabric    WITH lcFabric  ,;
            cColor     WITH lcColor   ,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcFDyelot ,;
            dTranDate  WITH ldTrDate  ,;
            dPostDate  WITH ldPostDate,;
            cTranType  WITH lcTrType  ,;
            cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            nUnitCost  WITH lnNewCost ,;
            nUntCstBuy WITH lnNewCost*Fabric.Conv,;
            nReceived  WITH lnAdjStk,;
            nStkVal    WITH nReceived * nUnitCost,;
            Reference  WITH lcRefer   ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct  ,;
            cRSession  WITH IIF(lcTrType = '4',lcCTRSess,lcFJlSess) ,;
            cISession  WITH IIF(lcTrType = '4',lcCTISess,'') ,;          
            cTrn_Seq   WITH cRSession ,;
            cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
            cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
            nPrvSQty   WITH lnPrvQty ,;
            nPrvSVal   WITH lnPrvVal ,;
            LineNo     WITH lnLineNo
    *E401271 (Start)
    *REPLACE cFabric    WITH lcFabric  ,;
            cColor     WITH lcColor   ,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcFDyelot ,;
            dTranDate  WITH ldTrDate  ,;
            dPostDate  WITH ldPostDate,;
            cTranType  WITH lcTrType  ,;
            cTran      WITH IIF(cTranType $ "23" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            nUnitCost  WITH lnNewCost ,;
            nUntCstBuy WITH lnNewCost*Fabric.Conv,;
            nReceived  WITH lnAdjStk,;
            nStkVal    WITH nReceived * nUnitCost,;
            Reference  WITH lcRefer   ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct  ,;
            cRSession  WITH IIF(lcTrType = '4',lcCTRSess,lcFJlSess) ,;
            cISession  WITH IIF(lcTrType = '4',lcCTISess,'') ,;          
            cTrn_Seq   WITH cRSession ,;
            cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
            cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
            nMPrvSQty  WITH lnPrvQty ,;
            nPrvSVal   WITH lnPrvVal ,;
            LineNo     WITH lnLineNo
    
    *B606163,1 ABD - Update the line like type '4' when Void the Invoice. [Begin]
    *REPLACE cFabric    WITH lcFabric  ,;
            cColor     WITH lcColor   ,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcFDyelot ,;
            dTranDate  WITH ldTrDate  ,;
            dPostDate  WITH ldPostDate,;
            cTranType  WITH lcTrType  ,;
            cTran      WITH IIF(cTranType $ "239" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            nUnitCost  WITH lnNewCost ,;
            nUntCstBuy WITH lnNewCost*Fabric.Conv,;
            nReceived  WITH lnAdjStk,;
            nStkVal    WITH nReceived * nUnitCost,;
            Reference  WITH lcRefer   ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct  ,;
            cRSession  WITH IIF(lcTrType = '4',lcCTRSess,lcFJlSess) ,;
            cISession  WITH IIF(lcTrType = '4',lcCTISess,'') ,;          
            cTrn_Seq   WITH cRSession ,;
            cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
            cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
            nMPrvSQty  WITH lnPrvQty ,;
            nPrvSVal   WITH lnPrvVal ,;
            LineNo     WITH lnLineNo,;
            llockflg   WITH lcTrType='9'

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
    *B606163,1 ABD - [End]
    *E401271 (End)
    *B603124 (End)  
    
    *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
    *B038753,1                transaction cost in case of standard costing method [Begin].
    REPLACE nTranCost WITH lnTranCost
    *B038753,1 KHM 12/18/2004 [End]
    *B038753,2 ASH 12/18/2004 (Begin) Make sure to save stk value in received record with +ve amount.
    IF nReceived = 0 AND lcTrType='9'
      REPLACE nStkVal WITH ABS(lnMStkVl)
    ENDIF
    *B038753,2 ASH 12/18/2004 (End)
    *B802475,1 AMM Update the operation code and lot #
    IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
      REPLACE cOprCode  WITH lcOprCode ,;
              cLotNo    WITH lcLotNo
    ENDIF
    *B802475,1 AMM end

    *C200255,1 (Begin) Update voucher no variable for CON10.
    IF ASCAN(laEvntTrig,PADR("GETVOUT",10)) <> 0
       REPLACE cvoucherno WITH lcVoucNo
    ENDIF   
    *C200255,1 (End)
    
    *-- Call global function to add audit fields info.
    =gfAdd_Info('MatInvJl')

    *--Update and Flush Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  
  ENDIF
  lnTmpStp = lnTmpStp + 1

  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()
  
  *E401271 (Start)
  IF lcTrType = '9' AND ((!EMPTY(lcFDyelot) AND llAdjYes) OR EMPTY(lcFDyelot))
    =lfLkAdjRec()
  ENDIF
  *E401271 (End)
  
  
ENDIF

*!***********************************************************************
*! Name      : lfCheckUnCmp
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Function To Check uncomplete session Steps.
*!***********************************************************************
*! Return    : .T. for Check , .F. for Already checked no need to check.
*!***********************************************************************
*! Example   : lfCheckUnCmp()
*!***********************************************************************

FUNCTION lfCheckUnCmp
PARA lnStepNo

IF !llChekUncmp
   RETURN .T.
ELSE
  RETURN ( &lcTmpLFile..&lcStepFld < lnStepNo )
ENDIF

*!***********************************************************************
*! Name      : lfUpdStep
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Update uncomplete session step.
*!***********************************************************************
*! Example   : lfUpdStep()
*!***********************************************************************

FUNCTION lfUpdStep
PARA lnCurntStep
PRIVATE lnAlas
IF llChekUncmp
  lnAlas = SELECT()
  SELECT (lcTmpLFile)
  =RLOCK()
  REPLACE &lcStepFld WITH lnCurntStep
  UNLOCK
  SELECT(lnAlas)
ENDIF
RETURN

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
*E301235,1 (Start)
PARAMETERS llExtCall
*E301235,1 (End)
PRIVATE lcAlias,llContnu,laTotRcvd,lnMaJorRec
lcAlias = ALIAS()

*606163,1 ABD - Define new variable to check if we came from the material invoice
*606163,1 ABD -  sales order or not. [Begin]
*-- this Variable to know the calling program EX lcCalProg = 'ARMINV' ,
*-- If I call this function from the invoice Material Sales Order.
IF TYPE('lcCalProg') # 'C'
  lcCalProg = SPACE(0)
ENDIF
*606163,1 ABD -  [End]

*E500329,1/*E500330,1 WAB - get the llgenRolId from ma setup . if user choose create roll id 
*E500329,1/*E500330,1 WAB (START) manual or generated sequence no 

llGenRolId   = ('MA' $ gcCmpModules .AND. ALLTRIM(gfGetMemVar('M_GENROLID')) = 'N')

*E500329,1/*E500330,1 WAB (END) 

*B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
*IF lnAdjStk < 0 OR lcTrType = '3'
IF lnAdjStk < 0 OR lcTrType $ '39'
*B123252,1 NNA (End)

  *E301345,1 (Start)
  *--Comment this part of code and rewrite it to handle multible records in
  *--issuing return PO.
  *lcTmpJour = gfTempName()
  **--This means it is "LFI" and not rol
  **--Create the Temp journal file with open receiving transactions.
  **-- HDM B603143,1 [Start] Add LineNo,nMPrvSQty,nPrvSVal To the SQL Statement
  **SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
  *       cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy     ,;
  *       SUM(nReceived-nIssued) AS 'nBalance'                         ,;
  *       SUM(nReceived) AS nReceived                                  ,;
  *       SUM(nIssued)   AS nIssued                                    ,;
  *       00000000 As 'nApply','' AS RolTranCd                         ,;
  *       "S" AS lStatus ,.F. AS 'lNeeded'                              ;
  *FROM   MATINVJL                                                      ;
  *WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
  *       lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
  *GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
  *         MatInvJl.cDyelot,MatInvJl.cRSession                         ;
  *HAVING nBalance <> 0                                                 ;
  *ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
  *         MatInvJl.cDyelot,MatInvJl.cRSession                         ;
  *INTO DBF (gcWorkDir+lcTmpJour)
  *SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
  *       cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy     ,;
  *       SUM(nReceived-nIssued) AS 'nBalance'                         ,;
  *       SUM(nReceived) AS nReceived                                  ,;
  *       SUM(nIssued)   AS nIssued                                    ,;
  *       00000000 As 'nApply','' AS RolTranCd                         ,;
  *       "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nMPrvSQty,nPrvSVal  ;
  *FROM   MATINVJL                                                      ;
  *WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
  *       lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
  *GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
  *         MatInvJl.cDyelot,MatInvJl.cRSession                         ;
  *HAVING nBalance <> 0                                                 ;
  *ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
  *         MatInvJl.cDyelot,MatInvJl.cRSession                         ;
  *INTO DBF (gcWorkDir+lcTmpJour)
  **-- HDM B603143,1 [End]
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
    *B605137,1 (Begin) Fix the bug of not accepting decimals when making minus adjustment.
    *SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
           cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy     ,;
           SUM(nReceived-nIssued) AS 'nBalance'                         ,;
           SUM(nReceived) AS nReceived                                  ,;
           SUM(nIssued)   AS nIssued                                    ,;
           00000000 As 'nApply','' AS RolTranCd                         ,;
           "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nMPrvSQty,nPrvSVal  ;
    FROM   MATINVJL                                                      ;
    WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
           lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
    GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
             MatInvJl.cDyelot,MatInvJl.cRSession                         ;
    HAVING nBalance <> 0                                                 ;
    ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
             MatInvJl.cDyelot,MatInvJl.cRSession                         ;
    INTO DBF (gcWorkDir+lcUseFile)

    *B606094,1 ABD - Divide the Qty With the Sell Conv.  [Begin]
    *SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession,;
    *       cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy     ,;
    *       SUM(nReceived-nIssued) AS 'nBalance'                         ,;
    *       SUM(nReceived) AS nReceived                                  ,;
    *       SUM(nIssued)   AS nIssued                                    ,;
    *       00000000.000 As 'nApply','' AS RolTranCd                      ,;
    *       "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nMPrvSQty,nPrvSVal  ;
    *FROM   MATINVJL                                                      ;
    *WHERE  cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =        ;
    *       lcFabric+lcColor+lcWareCode+lcFDyelot+MatInvJl.cRSession      ;
    *GROUP BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
    *         MatInvJl.cDyelot,MatInvJl.cRSession                         ;
    *HAVING nBalance <> 0                                                 ;
    *ORDER BY MatInvJl.cFabric,MatInvJl.cColor,MatInvJl.cWareCode        ,;
    *         MatInvJl.cDyelot,MatInvJl.cRSession                         ;
    *INTO DBF (gcWorkDir+lcUseFile)
    
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
      INTO DBF (gcWorkDir+lcUseFile)
    ELSE
      *B606231,1 KHM 08/05/2002 (Begin) Multiply the quantities by fabric's conversion
      *B606231,1                factor in case of Issue Material Return.
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
        INTO DBF (gcWorkDir+lcUseFile)
      ELSE
      *B606231,1 KHM 08/05/2002 (End)
      
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
        INTO DBF (gcWorkDir+lcUseFile)
      *B606231,1 KHM 08/05/2002 (Begin)
      ENDIF
      *B606231,1 KHM 08/05/2002 (End)
    ENDIF
    *B606094,1 ABD - [End]
    *B605137,1 (End)

    IF llExtCall AND (lcUseFile = lcItemJour)
      SELECT (lcTmpJour)
      APPEND FROM (gcWorkDir+lcItemJour+'.DBF')
    ENDIF

  *B604051,1 AAN NO Need to asign the naply filed to be 0  [Start].
  *ELSE
  *  SELECT(lcTmpJour)
  *  REPLACE nApply    WITH 0   ,;
  *          lStatus   WITH "S" ,;
  *          lNeeded   WITH .F. ,;
  *          RolTranCd WITH ''
  *B604051,1 AAN NO Need to asign the naply filed to be 0  [END].
  ENDIF
  *E301345,1 (End)

  
  SELECT (lcTmpJour)

  *--Indexing the file on Ascending or Descending expresion 
  *--depends on LIFO or FIFO method.

  *B604051,1 AAN 12/07/2000 If tag not found make an index[Start].
  *IF lcCostMeth $ 'FL'  &&FIFO OR LOT
  *  INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
  *ELSE
  *  INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
  *ENDIF
  IF !(TAG() == lcTmpJour)
    IF lcCostMeth $ 'FL'  &&FIFO OR LOT
      INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
    ELSE
      INDEX ON cFabric+cColor+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
    ENDIF
  ENDIF
  *B604051,1 AAN [End].
  GO TOP
  *-- This check is only if you issuing
  IF EOF() AND lcTrType <> '3' AND lnAdjStk < 0
    *--No open receiving exist for material/color XXXX/yyyyy ,
    *--This transaction line will be ignored.
    lcMsgExp = lcFabric + "/" + lcColor
    *B607971,1 TMI [Start] send the correct button number
    *=gfModalGen('TRM36131B00036','DIALOG',lcMsgExp)
    =gfModalGen('TRM36131B36000','DIALOG',lcMsgExp)
    *B607971,1 TMI [End  ] 
    USE
    RETURN .F.
  ENDIF

  *B606285,1 ABD - ignore the case of physical inventory.. [Begin]
  *IF lcCostMeth $ "FI"  &&FIFO OR LIFO
  IF lcTrType # '3' .AND. lcCostMeth $ "FI"  &&FIFO OR LIFO
  *B606285,1 ABD - [End]
    *--Start checking the only needed open receinving transaction for this
    *--issue transaction and put zero for all not needed receivings.

    *--Variable to Hold the accomulation of the receiving untill it cover
    *--the issue quantity needed.
    lnRcvdQty  = 0      
    SCAN WHILE lnRcvdQty <> ABS(lnAdjStk) OR EOF()
      IF lnRcvdQty < ABS(lnAdjStk)
        
        *B606285,1 ABD - Changing the IF command. [Begin]
        *IF nBalance < ABS(lnAdjStk+lnRcvdQty)
        *  REPLACE lNeeded  WITH .T.
        *ELSE
        *  REPLACE nApply  WITH ABS(lnAdjStk+lnRcvdQty) ,;
        *          lNeeded WITH .T.
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
      *B607971,1 TMI [Start] send the correct button number
      *=gfModalGen('TRM36132B00036','DIALOG',lcMsgExp)
      =gfModalGen('TRM36132B36000','DIALOG',lcMsgExp)
      *B607971,1 TMI [End  ] 
      USE
      RETURN .F. 
    ENDIF
  ELSE
    *-- it is lot
    *B603245,1 (Start) call lot rol SCREEN only if trans. type <> 3
    *IF !lfLotRolScr()
    IF (lcTrType # '3' OR (llTrkRolls .AND. Fabric.ltrkrolls)) AND !lfLotRolScr()
    *B603245,1 (End)
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


*B603814,1 ABD Fix Bug 'Alias Not Found'  that this file will not found 
*B603814,1 ABD if you are coming from adjustment screen.  [Begin]
IF lnAdjStk < 0
  *IF lnAdjStk < 0 .AND. lcType # 'A'
  *B603814,1 ABD [End]
  *--Delete all not needed receiving transactions for Journal file
  SELECT(lcTmpJour)
  *E301345,1 (Start)
  *SCAN
  SCAN WHILE cFabric+cColor+cWareCode+cDyelot+cRSession+cISession =;
             lcFabric+lcColor+lcWareCode+lcFDyelot
  *E301345,1 (End)
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

  *E301345,1 (Start)
  *SCAN
  PRIVATE lcTag
  lcTag = ORDER()
  SET ORDER TO lcTmpRoll2

  *B606163,1 ABD - [Begin]
  *-- Retuern the Pointer to first record match the correct Expr.
  = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6))
  *B606163,1 ABD - [End]
  
  SCAN WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)
  *E301345,1 (End)

    IF lNeeded
      *--Change it to Issue transactions,to use it in updating master Journal file.
	  *B121414,1 NNA 03/09/2004 (Begin) Replace in the correct record if we receive more one (PO) in the same
	  *B121414,1 NNA            Session and with the same Fab./Color/WH./lineNo 
	  IF CTRAN = lcTrCode
	  *B121414,1 NNA (End)	  
        REPLACE dTranDate WITH ldTrDate  ,;
                dPostDate WITH ldPostDate,;
                cTranType WITH lcTrType  ,;
                cTran     WITH lcTrCode  ,;
                nApply    WITH nApply
	  *B121414,1 NNA (Begin)
      ELSE
      	IF CTRAN = SPACE(6)
          REPLACE dTranDate WITH ldTrDate  ,;
                  dPostDate WITH ldPostDate,;
                  cTranType WITH lcTrType  ,;
                  cTran     WITH lcTrCode  ,;
                  nApply    WITH nApply
        ENDIF
	  ENDIF
	  *B121414,1 NNA (End)
    ELSE
      DELETE
    ENDIF
  ENDSCAN
  SET ORDER TO (lcTag) IN (lcTmpRoll)
  
  *B603814,1 ABD Fix Bug 'Alias Not Found'  that this file will not found 
  *B603814,1 ABD if you are coming from adjustment screen.  [Begin]
  IF lnAdjStk < 0
    *IF lnAdjStk < 0  .AND. lcType # 'A'
    *B603814,1 ABD [End]
  
    SELECT(lcFullRoll)
    *E301345,1 (Start)
    *SCAN
    PRIVATE lcTag
    lcTag = ORDER()

    *B606163,1 ABD - Set Order To correct Order. [Begin]
    *SET ORDER TO lcFullRoll2
    SET ORDER TO lcFullRoll
    *-- Retuern the Pointer to first record match the correct Expr.    
    = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6))
    *B606163,1 ABD - [End]

    SCAN WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
               lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)
    *E301345,1 (End)

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

    *B606163,1 ABD - [Begin]
    *-- Return Order To the Old Order.
    SET ORDER TO &lcTag
    *B606163,1 ABD - [End]
    
  ENDIF  
ENDIF

SELECT (lcAlias)

*E301235,1 [Begin]
IF llExtCall
  RETURN (lnAdjStk) 
ENDIF
*E301235,1 [End..]

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

*E301897,1 AMH Save the current transation type [Start]
PRIVATE lcOdTrnTyp
lcOdTrnTyp = lcTrType
*E301897,1 AMH [End]

*-- Rec. with the old cost
IF lfCheckUnCmp(lnTmpStp)
  SELECT MatInvJl
  APPEND BLANK
  *B603124 (Start)
  *REPLACE cFabric    WITH lcFabric  ,;
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
          nPrvSQty   WITH lnPrvQty ,;
          nPrvSVal   WITH lnPrvVal
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
  *B603124 (End)
  
  *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
  *B038753,1                transaction cost in case of standard costing method [Begin].
  REPLACE nTranCost WITH lnTranCost
  *B038753,1 KHM 12/18/2004 [End]

  *-- Call global function to add audit fields info.
  =gfAdd_Info('MatInvJl')

  *--Update and Flush Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)

*E301897,1 AMH Change the transaction type to Material Adjustement before updating 
*E301897,1     Fabric & Fabdye files [Start]
lcTrType = '2'
*E301897,1 AMH [End]

=lfFabWarDy()

*E301897,1 AMH Restore the current transaction type [Start]
lcTrType = lcOdTrnTyp
*E301897,1 AMH [End]

*-- Iss. with the new cost
IF lfCheckUnCmp(lnTmpStp)
  SELECT MATINVJL
  APPEND BLANK
  *B603124 (Start)
  *REPLACE cFabric    WITH lcFabric  ,;
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
          nPrvSQty   WITH lnPrvQty ,;
          nPrvSVal   WITH lnPrvVal
          
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
  *B603124 (End)
  
  *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
  *B038753,1                transaction cost in case of standard costing method [Begin].
  REPLACE nTranCost WITH lnTranCost
  *B038753,1 KHM 12/18/2004 [End]
  
  *-- Call global function to add audit fields info.
  =gfAdd_Info('MATINVJL')

  *--Update and Flush Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)

*E301897,1 AMH Change the transaction type to Material Adjustement before updating 
*E301897,1     Fabric & Fabdye files [Start]
lcTrType = '2'
*E301897,1 AMH [End]

=lfFabWarDy()

*E301897,1 AMH Restore the current transaction type [Start]
lcTrType = lcOdTrnTyp
*E301897,1 AMH [End]

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

*-- llNegStkAd Showes if it is main record or it is adj. record
*-- because the stock is less than Zero
*koko (Start)
*PARAMETERS llNegStkAd
PARAMETERS llNegStkAd,llLockAdj
*koko (End)

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

    IF lfCheckUnCmp(lnTmpStp)
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
        
      *E401271 (Start)
      *lnGLEnAmount = (lnQty * MatInvJl.nUnitCost) * (laGLInvAry[lnAln,3])
      lnGLEnAmount = MatInvJl.nStkVal * (laGLInvAry[lnAln,3])
      *E401271 (End)

      *koko (Start)
      *DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2]  ,;
                     lnGLEnAmount,; 
                     laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
                     laGLInvAry[lnAln,6],laGLInvAry[lnAln,7]  ,;
                     laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
                     laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
                     laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
      DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2]  ,;
                     lnGLEnAmount,; 
                     laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
                     IIF(llLockAdj,ldPostDate,laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7]  ,;
                     laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
                     laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
                     laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
      *koko (End)
      lnCurAlias = SELECT(0)
      SELECT (laGLInvAry[lnAln,9])
      REPLACE glSession WITH lcFJlSess
      SELECT MatInvJl
      
      *IF &laGLInvAry[lnAln,9]..Catg_Key = "015"
      *  REPLACE cMIcAcct WITH &laGLInvAry[lnAln,9]..GLAccount
      *ENDIF
      *IF &laGLInvAry[lnAln,9]..Catg_Key = "013" AND EMPTY(cGLMatAdj)
      *  REPLACE cGLMatAdj WITH &laGLInvAry[lnAln,9]..GLAccount
      *ENDIF
      DO CASE 
        CASE &laGLInvAry[lnAln,9]..Catg_Key = "015"
          REPLACE cMIcAcct WITH &laGLInvAry[lnAln,9]..GLAccount
        CASE &laGLInvAry[lnAln,9]..Catg_Key = "013" OR EMPTY(cGLMatAdj)
          REPLACE cGLMatAdj WITH &laGLInvAry[lnAln,9]..GLAccount
      ENDCASE

      
      SELECT (lnCurAlias)
    
      *--Update and Flush Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  
    ENDIF
    lnTmpStp = lnTmpStp + 1
  ENDFOR


ELSE
*-- This means it is Adj. Record 

*--Update Gl for Receiving inventory record and -ve old Stock.
*- Receiving Trans.(+1,+2,+3)

  *FOR lnAln=1 TO ALEN(laGLInvAry,1)
  *  laGLInvAry[lnAln,5] = IIF(MatInvJl.cTranType $ "23",MatInvJl.cTrn_Seq,laGLInvAry[lnAln,5])
  *  IF lfCheckUnCmp(lnTmpStp)
  *    *--Calculate the GL entry amount.
  *    IF MatInvJl.nReceived <> 0
  *      lnQty = MatInvJl.nReceived
  *    ELSE
  *      IF MatInvJl.nIssued <> 0
  *        lnQty = - MatInvJl.nIssued
  *      ELSE
  *        *-- this means received qty and issued qty both are Zero
  *        lnQty   = 0
  *      ENDIF
  *    ENDIF
  *    lnGLEnAmount = lnQty * MatInvJl.nUnitCost * laGLInvAry[lnAln,3]
  *    *--If the GL entres of the negative stock condition is not depends on
  *    *--the main Gl entry line, Undependent array elements.
  *    lcGLEnLkCod = laGLInvAry[1,1]
  *    lcGLEnCtTp  = IIF(lnAln=1,'015','016')
  *    DO GLDIST WITH lcGLEnLkCod ,;
  *                   lcGLEnCtTp  ,;
  *                   lnGLEnAmount,; 
  *                   'MA',laGLInvAry[lnAln,5]  ,;
  *                   laGLInvAry[lnAln,6],laGLInvAry[lnAln,7]  ,;
  *                   laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
  *                   laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
  *                   laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
  *    lnCurAlias = SELECT(0)
  *    SELECT (laGLInvAry[lnAln,9])
  *    REPLACE glSession WITH lcFJlSess
  *    SELECT MatInvJl
  *    IF &laGLInvAry[lnAln,9]..Catg_Key = "015"
  *      REPLACE cMICAcct WITH &laGLInvAry[lnAln,9]..GLAccount
  *    ENDIF
  *    IF &laGLInvAry[lnAln,9]..Catg_Key = "013" AND EMPTY(cGLMatAdj)
  *      REPLACE cGLMatAdj WITH &laGLInvAry[lnAln,9]..GLAccount
  *    ENDIF
  *    SELECT (lnCurAlias)
  *   *--Update and Flush Uncomplete session Step.
  *    =lfUpdStep(lnTmpStp)  
  *  ENDIF
  *  lnTmpStp = lnTmpStp + 1
  *ENDFOR

  lcMatLink = IIF(EMPTY(FabDye.GL_Link),Fabric.Link_Code,FabDye.GL_Link)
  DO GLDIST WITH lcMatLink,'015',MatInvJl.nStkVal,'MA',;
                 MatInvJl.cTran,MatInvJl.dTranDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  lnCurAlias = SELECT(0)
  SELECT MatInvJl
  REPLACE cMIcAcct WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1

  DO GLDIST WITH lcMatLink,'016',-MatInvJl.nStkVal,'MA',;
                 MatInvJl.cTran,MatInvJl.dTranDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  IF EMPTY(MatInvJl.cGLMatAdj)
    lnCurAlias = SELECT(0)
    SELECT MatInvJl
    REPLACE cGLMatAdj WITH &laGLInvAry[1,9]..GLAccount
    SELECT (lnCurAlias)
  ENDIF
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1
  
ENDIF
RETURN

*!*************************************************************
*! Name      : lfLotRolScr()
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Call lot or roll screen
*!*************************************************************
*! Example   : =lfLotRolScr()
*!*************************************************************

*!*************************************************************
*! Name      : lfLotRolScr()
*! Developer : AAMER
*! Date      : 01/22/98
*! Purpose   : Call lot or roll screen
*!*************************************************************
*! Example   : =lfLotRolScr()
*!*************************************************************

FUNCTION lfLotRolScr
PRIVATE lcWinCh5,lcWinCh6,lcWinCh7,lcWinCh8,lcChck,lcUnChck,lcRLTit,;
        lcFab,lcClr,lcWare,lcDye,lnTotApply,lnUsrApply,;
        lcRNewSta,lcRRemSta,lcRModSta,llRetVal,lnOldVal,lnNewRec

PRIVATE lcFileToUse,llBrowse

*E301345,1 (Start)
PRIVATE lcSerchExp
STORE SPACE(0) To lcSerchExp
*E301345,1 (End)

llBrowse = .F.


IF llTrkRolls .AND. Fabric.ltrkrolls
  *E301235,1
  *-- "!llExtCall" If it's called from the function (gfMatCrl) OR 
  *-- "!USED(lcTmpRoll" it's called from receiving material manufacturing order OR
  *-- "lcTrType = '1' AND lnAdjStk < 0" it's called from issue Material PO return
  
  *B606094,1 ABD - check on type '5'.  [Begin]
  *IF (llExtCall AND (!USED(lcTmpRoll) OR (lcTrType = '1' AND lnAdjStk < 0)) ) ;
  *   OR !llExtCall
  IF (llExtCall AND (!USED(lcTmpRoll) OR (lcTrType $ '15' AND lnAdjStk < 0)) ) ;
     OR !llExtCall
  *B606094,1 ABD - [End]

    *B603262,1 (Start)
    IF EMPTY(lcToWare) OR lcToWare # lcWareCode
    *B603262,1 (End)
      = lfTmpRoll()
    *B603262,1 (Start)
    ENDIF
    *B603262,1 (End)
  ENDIF
ENDIF

lcFileToUse = IIF(llTrkRolls .AND. Fabric.ltrkrolls,lcTmpRoll,lcTmpJour)
lnNewRec = RECNO(lcFileToUse)

STORE SPACE(0) TO lcWinCh5,lcWinCh6,lcWinCh7,lcWinCh8
llRetVal = .T.

lcFab  = lcFabric
lcClr  = lcColor
lcWare = lcWareCode
lcDye  = lcFDyelot

STORE 0 TO lnOldVal

STORE IIF(lcTrType='3',lnTmpOStk,0) TO lnUsrApply
STORE lnAdjStk TO lnTotApply

*B603232,1 (Start)
*IF TYPE('llExtCall') = 'L'
IF TYPE('llExtCall') = 'L' AND llExtCall
*B603232,1 (End)
  DIMENSION laUsrApply[1]
  laUsrApply = 0
  *B604023 AAN Check if the case is roll then add (lineno to where condition[Start]
  IF llTrkRolls .AND. Fabric.ltrkrolls
    *B121414,1 NNA 03/09/2004 (Begin) Add the P/O# to the filter to make a correct Receiving 
	*B121414,1                        Operation if we are receiving more than one order in the same session    
    *SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
            lcFab+lcClr+lcWare+lcDye+STR(lnLineNo,6) ;
      INTO ARRAY laUsrApply
    SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
            lcFab+lcClr+lcWare+lcDye+STR(lnLineNo,6) AND cTran = lcTrCode;
      INTO ARRAY laUsrApply
    *B121414,1 NNA (End)
  ELSE
    SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cFabric+cColor+cWareCode+cDyelot = ;
            lcFab+lcClr+lcWare+lcDye ;
      INTO ARRAY laUsrApply
  ENDIF
  *B604023[End]
  STORE laUsrApply * IIF(lnAdjStk>0,1,-1) TO lnUsrApply
ENDIF

lcRNewSta  = IIF(lnAdjStk > 0,'ENABLE','DISABLE')
*E301235,1  No need to check the same condition agian [Begin]
*lcRRemSta  = IIF(lnAdjStk > 0,'ENABLE','DISABLE')
*lcRModSta  = IIF(lnAdjStk > 0,'ENABLE','DISABLE')
STORE lcRNewSta TO lcRRemSta , lcRModSta
*E301235,1  No need to check the same condition agian [End]

lcRLTit = IIF(llTrkRolls .AND. Fabric.ltrkrolls,'_Rolls','Lots')

*E301345,1 (Start)
*lcWinCh5 = gfTempName()
*lcWinCh6 = gfTempName()
*lcWinCh7 = gfTempName()      
*lcWinCh8 = gfTempName()        
lcLotRo  = gfTempName()
lcLotRo1 = gfTempName()
lcLotRo2 = gfTempName()
lcLotRo3 = gfTempName()
*E301345,1 (End)

PRIVATE lcRollTitl,lcRlBrFild

DO CASE
  CASE lnAdjStk < 0
    IF llTrkRolls .AND. Fabric.ltrkrolls
      *E301345,1 (Start)
      llShowRol = .T.
      STORE 0 TO lnCurRolBa,lnAppRolQt
      *E301345,1 (End)
      lcRollTitl  = 'Available Rolls'
      *B603262 (Start)
      *lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1:W=.F.,"+;
                    "cRSession               :H='R.Sess.'   :R:W=.F.,"+;
                    "dTranDate               :H='Date'      :R:W=.F.,"+;
                    "cRollID                 :H='Roll ID'   :R:W=.F.,"+;
                    "nBalance                :H='Roll Bal.' :R:W=.F.,"+;
                    "nApply                  :H='Appl Qty.'   :W=lfwOldVals():V=lfvApply(),"+;
                    "nNewBal=nBalance-nApply :H='New Bal.'  :R:W=.F.,"+;
                    "nunitcost               :H='Unit Cost' :R:W=.F."
      *E301345,1 (Start)
      *lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1:W=.F.,"+;
                    "cRSession               :H='R.Sess.'   :R:W=.F.,"+;
                    "dTranDate               :H='Date'      :R:W=.F.,"+;
                    "cRollID                 :H='Roll ID'   :R:W=.F.,"+;
                    "nBalance                :H='Roll Bal.' :R:W=.F.,"+;
                    "nApply                  :H='Appl Qty.' :R:W=lfwOldVals():V=lfvApply(),"+;
                    "nNewBal=nBalance-ABS(nApply) :H='New Bal.'  :R:W=.F.,"+;
                    "nunitcost               :H='Unit Cost' :R:W=.F."
      
      *B606163,1 ABD - Add new filed Call 'Issue Qty.' to know if we issue 
      *B606163,1 ABD - any Qty from this line before that. [Begin]
      IF llExtCall .AND. lcCalProg = 'ARMINV'
        lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1,"+;
                      "cRSession               :H='R.Sess.'   :R,"+;
                      "dTranDate               :H='Date'      :R,"+;
                      "cRollID                 :H='Roll ID'   :R,"+;
                      "nBalance                :H='Roll Bal.' :R,"+;
                      "nApply                  :H='Appl Qty.' :R,"+;
                      "nissue                  :H='Issue Qty.' :R,"+;
                      "nNewBal=nBalance-ABS(nApply) :H='New Bal.':R,"+;
                      "nunitcost   :H='Unit Cost' :R:W=.F."
      ELSE
        *B606163,1 ABD - [End]
        lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1,"+;
                      "cRSession               :H='R.Sess.'   :R,"+;
                      "dTranDate               :H='Date'      :R,"+;
                      "cRollID                 :H='Roll ID'   :R,"+;
                      "nBalance                :H='Roll Bal.' :R,"+;
                      "nApply                  :H='Appl Qty.' :R,"+;
                      "nNewBal=nBalance-ABS(nApply) :H='New Bal.':R,"+;
                      "nunitcost               :H='Unit Cost' :R:W=.F."
      *B606163,1 ABD - End if For if statment. [Begin]
      ENDIF
      *B606163,1 ABD - [End]

      *E301345,1 (End)
      *B603262 (End)
    ELSE
      *E301345,1 (Start)
      STORE 0 TO lnCurLotBa,lnAppLotQt,lnNewLotBa
      llShowRol = .F.
      *E301345,1 (End)
      lcRollTitl  = 'Available Lots' 
      *E301345,1 (Start)
      *lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1:W=.F.,"+;
                    "Ctran     :H='Tran.'    :R:W=.F.,cRsession:H='R.Sess.'   :R:W=.F.,"+;
                    "dTrandate :H='Date'     :R:W=.F.,nunitcost:H='Unit Cost' :R:W=.F.,"+;
                    "nReceived :H='Received' :R:W=.F.,nIssued  :H='Issued'    :R:W=.F.,"+;
                    "nApply    :H='Apply'      :W=lfwOldVals():V=lfvApply(),"+;
                    "nBalance  :H='Balance'  :R:W=.F."                  
      *B606094,1 ABD - Show the unit cost with weight 10 and New picture. [Begin]
      *lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1,"+;
                    "Ctran     :H='Tran.'    :R,cRsession:H='R.Sess.'  :R,"+;
                    "dTrandate :H='Date'     :R,nunitcost:H='Unit Cost':R,"+;
                    "nReceived :H='Received' :R:10,nIssued  :H='Issued':R:10,"+;
                    "nCurBal=nReceived-nIssued:H='Curr. Bal.'          :R:10,"+;
                    "nApply    :H='Apply Qty.'                         :R:10,"+;
                    "nNewBal=nReceived-nIssued-nApply:H='New Bal.'     :R:10"
      *B606163,1 ABD -  [Begin]
      *--Add issue Qty And move the date to the en of the browse to show the available qty.
      IF llExtCall .AND. lcCalProg = 'ARMINV'
        lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1,"+;
                      "Ctran     :H='Tran.'    :R,cRsession:H='R.Sess.'  :R,"+;
                      "nunitcost:H='Unit Cost':R:11:P= '9999999.999',"+;
                      "nReceived :H='Received' :R:10,nIssued  :H='Issued':R:10,"+;
                      "nCurBal=nReceived-nIssued:H='Curr. Bal.'          :R:10,"+;
                      "nApply    :H='Apply Qty.'                         :R:10,"+;
                      "niSsue    :H='Issue Qty.'                         :R:10,"+;
                      "nNewBal=nReceived-nIssued-nIssue:H='New Bal.'     :R:10,"+;
                      "dTrandate :H='Date'     :R"
      ELSE
        *B606163,1 ABD -  [End]
        lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1,"+;
                      "Ctran     :H='Tran.'    :R,cRsession:H='R.Sess.'  :R,"+;
                      "dTrandate :H='Date'     :R,nunitcost:H='Unit Cost':R:11:P= '9999999.999',"+;
                      "nReceived :H='Received' :R:10,nIssued  :H='Issued':R:10,"+;
                      "nCurBal=nReceived-nIssued:H='Curr. Bal.'          :R:10,"+;
                      "nApply    :H='Apply Qty.'                         :R:10,"+;
                      "nNewBal=nReceived-nIssued-nApply:H='New Bal.'     :R:10"
       *B606163,1 ABD -  End IF for If statment. [Begin]
      ENDIF
      *B606163,1 ABD -  [End]      
      *B606094,1 ABD - [End]
    *E301345,1 (End)
    ENDIF

  *E301345,1 (Start)
  *CASE lnAdjStk > 0 AND llTrkRolls .AND. Fabric.ltrkrolls
  CASE lnAdjStk >= 0 AND llTrkRolls .AND. Fabric.ltrkrolls
    llShowRol = .T.
    STORE 0 TO lnCurRolBa,lnAppRolQt
  *E301345,1 (End)
    lcRollTitl  = IIF(lcTrType='3','M_Rolls new balances','New Rolls')
    
    *E301345,1 (Start)
    *lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1:W=.F.,"+;
                  "cRollID                :H='Roll ID'  :R:W = .F.,"+;
                  "nNewBal=IIF(lcTrType='3',IIF(nApply=0,nBalance,nApply),nApply+nBalance) :H='Roll balance':R:20:W = .F."
    
    *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
    *IF lcTrType = '3'
    IF lcTrType $ '39'
    *B123252,1 NNA (End)
    
      lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1,"+;
                    "cRollID                :H='Roll ID'  :R,"+;
                    "nBalance :H='Current balance'        :R,"+;
                    "nApply   :H='New balance'            :R"
    ELSE
       lcRlBrFild  = "cMarker=IIF(RECNO()=lnNewRec,'>',' '):H=' ':R:1,"+;
                    "cRollID        :H='Roll ID'     :R,"+;
                    "nApply         :H='Roll balance':R"
     ENDIF
    *E301345,1 (END)
  
ENDCASE
*-- Call the rolls or lots browse screen
PUSH KEY
lcSysMen = SET('SYSMENU')
SET SYSMENU ON
*E301345,1 (Start)
*DEFINE PAD _BROWSE OF _MSYSMENU PROMPT "" KEY CTRL+B
*ON SELECTION PAD _BROWSE OF _MSYSMENU ACTIVATE WINDOW (lcRollTitl)
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcRollTitl)
*E301345,1 (End)

*E301345,1 (Start)
*DO (gcScrDir+gcWinAppl+'\MAINVCT5.SPX')
RELEASE PAD _BROWSE OF _MSYSMENU
*B606094,1 ABD - We should call this screen from the MA. [Begin]
*DO (gcScrDir+gcWinAppl+'\MALOTRO.SPX')

*B123252,1 NNA 06/17/2004 (Begin) Show The Material Lot Roll Screen only if Keep track of Roll [Yes]
*B123252,1 NNA                    and the Fabric too
IF (llTrkRolls .AND. Fabric.ltrkrolls) .OR. lcCostMeth = 'L' 
*B123252,1 NNA (End)

*C126356,1 NNA 03/15/2005 (Begin) A Trigger For RES03 to Create a Cursor and Change the Roll_ID Format
STORE '' TO lcRollFrmt
IF ASCAN(laEvntTrig,PADR("CRTCURSR",10)) <> 0 AND gfGetMemVar('M_GENROLID')='Y'
  =gfDoTriger('MAPOREC',PADR('CRTCURSR',10))
  lcRollFrmt = "@! XXXXXXXXXX-XXXXXXXXX"
ELSE
  lcRollFrmt = "@! XXXXXXXXXXXXXXXXxxxx"
ENDIF   
*C126356,1 NNA (End)

  DO (gcScrDir+'MALOTRO.SPX')

*B123252,1 NNA (Begin)
ENDIF
*B123252,1 NNA (End)

*B606094,1 ABD - [End]

*E301345,1 (End)

SET SYSMENU &lcSysMen
POP KEY
PRIVATE lnTotRoll
IF (lnAdjStk < 0 ) AND llTrkRolls .AND. Fabric.ltrkrolls
  SELECT (lcTmpRoll)

  *E301345,1 (Start)
  PRIVATE lcTag
  lcTag = ORDER()
  SET ORDER TO lcTmpRoll2
  *SCAN

  *B603950,1 ABD Fix Bug that didn't update the MatinvJl file with correct value. [Begin]
  *SCAN WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
  *           lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)
  SCAN FOR cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcFabric+lcColor+lcWareCode+lcFDyelot
    *B603950,1 ABD  [End]
    *E301345,1 (End)
  
    lnTotRoll = nApply
    SELECT (lcFullRoll)
    IF SEEK(&lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
            &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot)
      *B603262,1 (Start)
      *SCAN REST WHILE lnTotRoll> 0 AND ;
                      cRollID+cFabric+cColor+cWareCode+cDyelot = ;
                      &lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
                      &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot
        *REPLACE nApply    WITH IIF(nBalance<lnTotRoll,nBalance,lnTotRoll),;
                RolTranCd WITH &lcTmpRoll..RolTranCd ,;
                lNeeded   WITH !EMPTY(nApply)

      SCAN REST WHILE lnTotRoll <> 0 AND ;
                      cRollID+cFabric+cColor+cWareCode+cDyelot = ;
                      &lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
                      &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot

        *B128927,1  TMI [Start] use the absolute value of the nBalance field
        *REPLACE nApply    WITH IIF(nBalance<lnTotRoll,nBalance,lnTotRoll),;
        *        RolTranCd WITH IIF(lnAdjStk<0,'2',&lcTmpRoll..RolTranCd) ,;
        *        lNeeded   WITH !EMPTY(nApply)
        REPLACE nApply    WITH IIF(ABS(nBalance)<lnTotRoll,ABS(nBalance),lnTotRoll),;
                RolTranCd WITH IIF(lnAdjStk<0,'2',&lcTmpRoll..RolTranCd) ,;
                lNeeded   WITH !EMPTY(nApply)
        *B128927,1  TMI [End  ] 

      *B603262,1 (End)
        *B603980,1 ABD Fix Bug "Alias Not Found" When use Adjustment Screen. [Begin]
        *B606094,1 ABD - Remark the next few lines. [Begin]
        *IF SEEK(&lcFullRoll..cFabric+&lcFullRoll..cColor+&lcFullRoll..cWareCode+;
        *        &lcFullRoll..cDyelot+&lcFullRoll..cRSession,lcTmpJour)
        *  *IF lcType # 'A' .AND. SEEK(&lcFullRoll..cFabric+&lcFullRoll..cColor+&lcFullRoll..cWareCode+;
        *  *        &lcFullRoll..cDyelot+&lcFullRoll..cRSession,lcTmpJour)
        *  *B603980,1 ABD [End]
        *  SELECT(lcTmpJour)
          
        *  REPLACE nApply  WITH nApply + &lcFullRoll..nApply,;
        *          lNeeded WITH !EMPTY(nApply)
        *ENDIF
        *B606094,1 ABD - [End]        
        lnTotRoll = lnTotRoll - &lcFullRoll..nApply
      ENDSCAN
    ENDIF
  ENDSCAN
  *E301345,1 (Start)
  SET ORDER TO (lcTag) IN (lcTmpRoll)
  *E301345,1 (End)
  
  *B606094,1 ABD - We should collect the data for rolls from rolls file one time. [Begin]
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
  *B606094,1 ABD - [End]

ENDIF

RETURN llRetVal

*!***********************************************************************
*! Name      : lfActBrwRo
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Browse lot or roll records
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfActBrwRo()
*!***********************************************************************

FUNCTION lfActBrwRo

SELECT(lcFileToUse)
GO TOP
lnNewRec = RECNO(lcFileToUse)

*E301235,1 (Start)
*BROWSE FIELDS &lcRlBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwRo();
       VALID :F lfvBrwRo();
       TITLE lcRollTitl   ;
       WINDOW lcWinCh6 IN WINDOW lcWinCh5


PRIVATE lcSerchExp
*B603232,1 (Start)
*IF TYPE('llExtCall') = 'U'
IF TYPE('llExtCall') = 'U' OR !llExtCall
*B603232,1 (End)
  *B603262,1 (Start)
  IF !EMPTY(lcToWare) AND lcToWare = lcWareCode
    lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
                 "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) " + ;
                 "AND LineNo = lnlineNo"
  ELSE
  *B603262,1 (End)
    lcSerchExp = ""
  *B603262,1 (Start)
  ENDIF
  *B603262,1 (End)
ELSE
  *B603262 (Start)

  *N000388,1 KHM (Begin) Include the material invoice lcTrType $ '15'
  *IF lnAdjStk < 0  AND lcTrType = '1'
  IF lnAdjStk < 0  AND lcTrType $ '15'
  *N000388,1 KHM (End) 
  
    lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
               "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) "
  ELSE
  *B603262 (End)
    lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
                 "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) " + ;
                 "AND LineNo = lnlineNo"

  *B603262 (Start)
  ENDIF
  *B603262 (End)
ENDIF

BROWSE FIELDS &lcRlBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwRo();
       VALID :F lfvBrwRo();
       TITLE lcRollTitl   ;
       &lcSerchExp        ;
       WINDOW lcWinCh6 IN WINDOW lcWinCh5
*E301235,1 (End)
=lfwBrwRo()

*!***********************************************************************
*! Name      : lfwBrwRo
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Execute when function for lot/rol browse
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfwBrwRo()
*!***********************************************************************

FUNCTION lfwBrwRo

*E301345,1 (Start)
*Comment this function and rewrite it
*lnNewRec = RECNO(lcFileToUse)
*SHOW WINDOW (lcRollTitl) REFRESH SAME
*IF lcTrType $ '23' AND lcIRType = 'R' OR TYPE('llExtCall') = 'L'
*  IF !EMPTY(&lcFileToUse..cFabric) 
*    *B603262,1 (Start)
*    IF llTrkRolls .AND. Fabric.ltrkrolls
*    *B603262,1 (End)
*      lcRModSta = "ENABLE"
*      IF lcTrType = '3'
*        IF &lcTmpRoll..RolTranCd='1'
*          lcRRemSta = "ENABLE"
*        ELSE
*          lcRRemSta = "DISABLE"
*        ENDIF
*      ELSE
*        lcRRemSta = IIF(lnAdjStk>0,"ENABLE","DISABLE")
*      ENDIF
*    ELSE
*      lcRModSta = "DISABLE"
*      lcRRemSta = "DISABLE"
*    ENDIF
*  *B603262,1 (Start)
*  ELSE
*    lcRModSta = "DISABLE"
*    lcRRemSta = "DISABLE"
*  ENDIF
*  *B603262,1 (End)
*ELSE
*  lcRModSta  = "DISABLE"
*  lcRRemSta  = "DISABLE"
*ENDIF
*SHOW GET pbRModify &lcRModSta
*SHOW GET pbRRemove &lcRRemSta

lnNewRec = RECNO(lcFileToUse)
SHOW WINDOW (lcRollTitl) REFRESH SAME
DO CASE
  CASE llShowRol
    lcRollID   = &lcFileToUse..cRollID
    lnAppRolQt = &lcFileToUse..nApply
    lnCurRolBa = &lcFileToUse..nBalance
    lcRolStat = IIF(EMPTY(&lcFileToUse..cRollID) AND !EOF(lcFileToUse),'ENABLE','DISABLE')
    lcAppStat = IIF(EMPTY(lcRollID),"DISABLE","ENABLE")
    SHOW GET lnAppRolQt &lcAppStat
    SHOW GET lcRollID   &lcRolStat
    IF lnAdjStk >= 0
      lcRemStat = IIF(EOF(lcFileToUse),"DISABLE","ENABLE")
      SHOW GET pbRRemove  &lcRemStat
      SHOW GET pbRNew     ENABLE
    ENDIF
    *C126726,1  TMI [Start] Add a trigger to allow to update the apply field automatically for Restex
    IF ASCAN(laEvntTrig,"AUTUPAPL") <> 0
      =gfDoTriger('ARMINV','AUTUPAPL')
    ENDIF   
    *C126726,1  TMI [End  ] 
    
  CASE !llShowRol
    lnAppRolQt = &lcFileToUse..nApply
    lnCurLotBa = &lcFileToUse..nReceived - &lcFileToUse..nIssued
    lnAppLotQt = &lcFileToUse..nApply

    *B606163,1 ABD - Show the correct balance if we issue any qty from this line before. [Begin]
    IF llExtCall .AND. lcCalProg = 'ARMINV'
     lnNewLotBa = &lcFileToUse..nReceived - &lcFileToUse..nIssued - &lcFileToUse..nIssue
    ELSE
      *B606163,1 ABD - [End]    
      lnNewLotBa = &lcFileToUse..nReceived - &lcFileToUse..nIssued - &lcFileToUse..nApply
      *B606163,1 ABD - End if for If statment.[Begin]
    ENDIF
    *B606163,1 ABD - [End]
      
    SHOW GET lnAppLotQt
ENDCASE
= lfLOtRoRef()
*E301345,1 (End)

*!***********************************************************************
*! Name      : lfvBrwRo
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Execute valid function for lot/rol browse
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfvBrwRo()
*!***********************************************************************

FUNCTION lfvBrwRo

IF WONTOP() # (lcRollTitl)
  =gfStopBrow()
ENDIF

*!***********************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : TO overwrite the functionality of some keys to not affect the browse
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfTrap()
*!***********************************************************************

FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcRollTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
  ON KEY LABEL ESCAPE  DO lfEscape
ENDIF
*!***********************************************************************
*! Name      : lfClrTrap
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Clear Keys trapping
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfClrTrap()
*!***********************************************************************

FUNCTION lfClrTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESCAPE  DO lfEscape

*!***********************************************************************
*! Name      : lfBrTab
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Trap TAB key
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfBrTab()
*!***********************************************************************

FUNCTION lfBrTab

ON KEY LABE TAB
*E301345,1 (Start)
*ACTIVATE WINDOW lcWinCh8
*_CUROBJ = OBJNUM(pbRNew)
DO CASE 
  CASE !llShowRol
    ACTIVATE WINDOW (lcLotRo3)
    _CUROBJ = OBJNUM(lnAppLotQt)
  CASE llShowRol
    ACTIVATE WINDOW (lcLotRo3)
    IF lnAdjStk < 0
      _CUROBJ = OBJNUM(lnAppRolQt)
    ENDIF

ENDCASE
*E301345,1 (End)

*!***********************************************************************
*! Name      : lfBrBack
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Trap SHIFT+TAB key
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfBrBack()
*!***********************************************************************

FUNCTION lfBrBack

ON KEY LABE BACKTAB
*E301345,1 (Start)
*ACTIVATE WINDOW lcWinCh8
*_CUROBJ = OBJNUM(pbRClose)
DO CASE 
  CASE !llShowRol
    ACTIVATE WINDOW (lcLotRo3)
    _CUROBJ = OBJNUM(pbLClose)
  CASE llShowRol
    ACTIVATE WINDOW (lcLotRo3)
    IF lnAdjStk < 0
      _CUROBJ = OBJNUM(pbLClose)
    ENDIF
ENDCASE
*E301345,1 (End)

*!***********************************************************************
*! Name      : lfEscape
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Trap ESCAPE key
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfEscape()
*!***********************************************************************

FUNCTION lfEscape

ON KEY LABE ESCAPE
= lfvpbClose()

*!*************************************************************
*! Name      : lfwOldVals
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To save the old value befor modifying it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfwOldVals()
*!*************************************************************

FUNCTION lfwOldVals

lnOldVal  = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvApply
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate apply push button
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvApply()
*!*************************************************************

FUNCTION lfvApply

*E301345,1 (Start)
*comment this function and rewrite it
*PRIVATE llApply
*llApply = .T.
**-- Check the range of the applied quantity
*IF ABS(lnUsrApply) = ABS(lnTotApply) AND nApply <> 0
*  WAIT 'The entry quantity is applied completely.' WINDOW
*  llApply = .F.
*ELSE
*  IF !BETWEEN(nApply, 0, nBalance)
*    WAIT "Range 0 .. " + ALLTRIM(STR(nBalance)) WINDOW
*    llApply = .F.
*  ELSE
*    IF (ABS(lnUsrApply) - lnOldVal + nApply) > ABS(lnTotApply)
*      WAIT 'The total entry quantity exceeds on applied quantity.' + ;
*           'You can apply only ' + ALLTRIM(STR(ABS(lnTotApply) - ABS(lnUsrApply))) WINDOW
*      llApply = .F.
*    ENDIF
*  ENDIF
*ENDIF  
*IF llApply
*  lnUsrApply = lnUsrApply - nApply + lnOldVal
*ELSE
*  REPLACE nApply WITH lnOldVal
*ENDIF  
*SELECT(lcFileToUse)
**-- Updating RolTranCd is needed only if it is issue from rolls
*REPLACE lNeeded   WITH !EMPTY(nApply),;
*        RolTranCd WITH IIF(lNeeded,'2',RolTranCd)
*=lfRefresh()

*B606163,1 ABD -  return in case you use the mouse. [Begin]
IF MDOWN() .AND. lnOldVal  = IIF(llShowRol,lnAppRolQt,lnAppLotQt)
  RETURN
ENDIF
*B606163,1 ABD -  [End]

PRIVATE llApply,lnCurAlias,lcVarName
lnCurAlias = SELECT(0)

llApply = .T.
DO CASE
  *B606163,1 ABD -  Get the Correct Value for Balance Field if we have decimal. [Begin]
  *CASE lnAdjStk < 0 AND !BETWEEN(IIF(llShowRol,lnAppRolQt,lnAppLotQt),0,nBalance)
  *WAIT "Range 0 .. " + ALLTRIM(STR(nBalance)) WINDOW
  CASE lnAdjStk < 0 AND !BETWEEN(IIF(llShowRol,lnAppRolQt,lnAppLotQt),0,;
                     IIF(llExtCall .AND. lcCalProg = 'ARMINV',nBalance-nissue+lnOldVal,nBalance+lnOldVal))
    IF llExtCall .AND. lcCalProg = 'ARMINV'
      WAIT "Range 0 .. " + ALLTRIM(STR(nBalance-nissue+lnOldVal,10,3)) WINDOW
    ELSE
      WAIT "Range 0 .. " + ALLTRIM(STR(nBalance+lnOldVal,10,3)) WINDOW
    ENDIF
    *B603980,1 ABD [End]

    llApply = .F.
  CASE lnAdjStk > 0 AND IIF(llShowRol,lnAppRolQt,lnAppLotQt) < 0
    WAIT "Applied quantity is not allowed to be negative." WINDOW
    llApply = .F.
    *B603980,1 ABD Fix Bug alias not found that we will not decrease the qty that recived before.
    *B603980,1 ABD in case we made Adj+ .   [Begin]
  CASE lnAdjStk > 0 .AND. TYPE('lcType') = "C" .AND. lcTYPE = "A"
    lnAdjQty = lnAppRolQt - lnOldVal
    IF lnAdjQty < -1
      WAIT "Applied quantity is not allowed to decrease." WINDOW
      llApply = .F.
    ENDIF
    *B603980,1 ABD [End]
ENDCASE  
  
IF llApply
  IF lnAdjStk >= 0
    lnUsrApply = lnUsrApply - lnOldVal + lnAppRolQt
  ELSE
    lnUsrApply = lnUsrApply - IIF(llShowRol,lnAppRolQt,lnAppLotQt) + lnOldVal
  ENDIF
  SELECT(lcFileToUse)
  *-- Updating RolTranCd is needed only if it is issue from rolls
  *B606163,1 ABD - [Begin]
  IF llExtCall .AND. lcCalProg = 'ARMINV'

    *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
    *REPLACE nApply    WITH IIF(llShowRol,lnAppRolQt,lnAppLotQt)                     ,;
            nissue    WITH nissue + IIF(llShowRol,lnAppRolQt,lnAppLotQt) - lnOldVal ,;
            lNeeded   WITH !(lcTrType = '3' AND RolTranCd = '1' AND nApply = 0) OR   ;
                            (lcTrType = '3' AND RolTranCd $ '23') OR nApply # 0     ,;
            RolTranCd WITH IIF(RolTranCd='3','2',RolTranCd)
  
    REPLACE nApply    WITH IIF(llShowRol,lnAppRolQt,lnAppLotQt)                     ,;
            nissue    WITH nissue + IIF(llShowRol,lnAppRolQt,lnAppLotQt) - lnOldVal ,;
            lNeeded   WITH !(lcTrType $ '39' AND RolTranCd = '1' AND nApply = 0) OR   ;
                            (lcTrType $ '39' AND RolTranCd $ '23') OR nApply # 0     ,;
            RolTranCd WITH IIF(RolTranCd='3','2',RolTranCd)
    *B123252,1 NNA (End)

  ELSE
    *B606163,1 ABD - [End]

  *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
  *REPLACE nApply    WITH IIF(llShowRol,lnAppRolQt,lnAppLotQt) ,;
          lNeeded   WITH !(lcTrType = '3' AND RolTranCd = '1' AND nApply = 0) OR ;
                          (lcTrType = '3' AND RolTranCd $ '23') OR nApply # 0,;
          RolTranCd WITH IIF(RolTranCd='3','2',RolTranCd)
  REPLACE nApply    WITH IIF(llShowRol,lnAppRolQt,lnAppLotQt) ,;
          lNeeded   WITH !(lcTrType $ '39' AND RolTranCd = '1' AND nApply = 0) OR ;
                          (lcTrType $ '39' AND RolTranCd $ '23') OR nApply # 0,;
          RolTranCd WITH IIF(RolTranCd='3','2',RolTranCd)
  *B123252,1 NNA (End)

    *B606163,1 ABD - End if for if statment. [Begin]
  ENDIF
  *B606163,1 ABD - [End]

ELSE
  lcVarName = VARREAD()
  &lcVarName = lnOldVal
  _CUROBJ = _CUROBJ
ENDIF

= lfwBrwRo()

SELECT(lnCurAlias)
*E301345,1 (End)

*!*************************************************************
*! Name      : lfvNewMod
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate new and modify push buttons
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvNewMod()
*!*************************************************************

FUNCTION lfvNewMod

PRIVATE lcRollid,lnRolBal,lcRIdSta,lnOldVal,llApply,lnCurAlias,llBrowse,lcAddOrMod,;
        llFndInTmp,lcRSession

llApply    = .F.
llBrowse   = .F.
llFndInTmp = .F.

lcRollid   = SPACE(0)
lcTrnCode  = SPACE(0)
lcRSession = SPACE(0)
lnRolBal   = 0
lnOldVal   = 0

PRIVATE lnOldRoBal,lcTrnCode
lcAddOrMod = VARREAD()
DO CASE
  CASE lcAddOrMod = "PBRNEW"
    lnOldRoBal = 0
    lcTrnCode  = ''
    lcRIdSta   = "ENABLE"
  CASE lcAddOrMod = "PBRMODIFY"
    lcRollID   = &lcTmpRoll..cRollID
    
    *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
    *IF lcTrType = '3'
    IF lcTrType $ '39'    
    *B123252,1 NNA (End)
    
      IF EMPTY(&lcTmpRoll..nApply)
        lnRolBal   = &lcTmpRoll..nBalance
      ELSE
        lnRolBal   = &lcTmpRoll..nApply
      ENDIF
    ELSE
      lnRolBal   = &lcTmpRoll..nApply + &lcTmpRoll..nBalance
    ENDIF
    
    lnOldRoBal = &lcTmpRoll..nBalance
    lcTrnCode  = &lcTmpRoll..RolTranCd
    lcRIdSta   = "DISABLE"
ENDCASE

PUSH KEY
DO (gcScrDir+gcWinAppl+'\MAINVCT9.SPX')
POP KEY

IF llApply
  
  lnCurAlias = SELECT(0)
  SELECT (lcTmpRoll)
  IF lcAddOrMod = "PBRNEW" AND !llFndInTmp
    APPEND BLANK
    REPLACE cFabric   WITH lcFab          ,;
            cColor    WITH lcClr          ,;
            cWareCode WITH lcWare         ,;
            cDyelot   WITH lcDye          ,;
            cRollId   WITH lcRollID       ,;
            RolTranCd WITH lcTrnCode      ,;
            cRSession WITH lcRSession     ,;
            nBalance  WITH lnOldRoBal     ,;
            LineNo    WITH lnLineNo
  ENDIF
  
  IF lnRolBal = lnOldRoBal

    *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
    *IF lcTrType = '3'
    IF lcTrType $ '39'
    *B123252,1 NNA (End)

      IF &lcTmpRoll..RolTranCd = '1'
        DELETE
      ENDIF
    ELSE
      DELETE
    ENDIF
  ENDIF
  lnUsrApply = lnUsrApply - nApply + IIF(lcTrType='3',lnRolBal,lnRolBal - lnOldRoBal)
  REPLACE nApply  WITH IIF(lcTrType='3',lnRolBal,lnRolBal - lnOldRoBal),;
          lNeeded WITH IIF(lcTrType='3',!EMPTY(nApply) OR !EMPTY(nBalance),!EMPTY(nApply))

  SELECT(lnCurAlias)
ENDIF

= lfwBrwRo()
= lfRefresh(lcWinCh7)

*!*************************************************************
*! Name      : lfvRem
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate remove push button
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvRem()
*!*************************************************************

FUNCTION lfvRem
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT (lcFileToUse)
lnUsrApply = lnUsrApply - ABS(&lcFileToUse..nApply)
BLANK
DELETE
= lfwBrwRo()

SELECT (lnCurAlias)

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
*B603280,5 (Start)
*llReturn = AriaBrow("lcFab+lcClr+lcWare+lcDye FOR TranCd='1'",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'Rolls',.F.)
llFound = SEEK(lcFab+lcClr+lcWare+lcDye)
IF llFound
  LOCATE REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollID+TranCd+cRSession = ;
                    lcFab+lcClr+lcWare+lcDye FOR TranCd='1'
  llFound = FOUND()
ENDIF
IF llFound
  llReturn = AriaBrow("lcFab+lcClr+lcWare+lcDye FOR TranCd='1'",lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'Rolls',.F.)
ELSE
  *B607971,1 TMI [Start] send the correct button number
  *= gfModalGen('TRM00052B00036','DIALOG')
  = gfModalGen('TRM00052B36000','DIALOG')
  *B607971,1 TMI [End  ] 
  
ENDIF  
*B603280,5 (End)

SET ORDER TO lcCurTag IN Rolls
SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfvRoll_ID
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate roll id field
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvRoll_ID()
*!*************************************************************

FUNCTION lfvRoll_ID

PRIVATE lcRolTag,lnCurAlias

lnCurAlias = SELECT(0)

IF !( (EMPTY(lcRollID) OR LASTKEY() <> 13) AND !llBrowse)

  *E301235,1 (Start)
  IF TYPE('llExtCall') = 'L' AND llExtCall
    lcRolTag = ORDER('Rolls')
    SET ORDER TO RollItem IN Rolls
    IF SEEK(lcFab+lcClr+lcWare+lcDye+lcRollId+'1','Rolls')
      WAIT "This roll id already exist." WINDOW
      lcRollID = ''
      _CUROBJ = _CUROBJ
    ELSE
      SELECT(lcTmpRoll)
      IF SEEK(lcRollID+lcFab+lcClr+lcWare+lcDye)
      
        LOCATE REST WHILE cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession = ;
                          lcRollID+lcFab+lcClr+lcWare+lcDye ;
                    FOR   LineNo = lnLineNo
        IF FOUND()
          lnRolBal   = &lcTmpRoll..nApply + &lcTmpRoll..nBalance
          lnOldRoBal = &lcTmpRoll..nBalance
          lcTrnCode  = &lcTmpRoll..RolTranCd
          lcRSession = &lcTmpRoll..cRSession
        ELSE
          WAIT "This Roll-ID already exist." WINDOW
          lcRollID = ''
          _CUROBJ = _CUROBJ
        ENDIF
        llFndInTmp = .T.
      ELSE
        WAIT "New Roll has been added." WINDOW TIMEOUT 2
        lcTrnCode  = '1'
        lcRSession = lcFJlSess
      ENDIF
    ENDIF
    SET ORDER TO lcRolTag IN Rolls
  ELSE
  *E301235,1 (End)
    IF llBrowse OR '?' $ lcRollID
      llBrowse = .F.
      IF lfRollBrow()
        lcRollID = Rolls.cRollID
      ELSE
        lcRollID = ''
        _CUROBJ = _CUROBJ
      ENDIF
    ENDIF  
  
    IF !EMPTY(lcRollid)
  
      *-- check if the roll ID is used before or not
    
      SELECT(lcTmpRoll)
      IF !SEEK(lcRollID+lcFab+lcClr+lcWare+lcDye)
        lcRolTag = ORDER('Rolls')
        SET ORDER TO RollItem IN Rolls
        IF SEEK(lcFab+lcClr+lcWare+lcDye+lcRollId+'1','Rolls')
          lcTrnCode  = '3'
          lnRolBal   = Rolls.nQtyBal
          lnOldRoBal = Rolls.nQtyBal
          lcRSession = Rolls.cRsession
        ELSE
          WAIT "New Roll has been added." WINDOW TIMEOUT 2
          lcTrnCode  = '1'
          lcRSession = lcFJlSess
        ENDIF
        SET ORDER TO lcRolTag IN Rolls
      ELSE
        llFndInTmp = .T.

        *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
        *IF lcTrType = '3'
        IF lcTrType $ '39'    
        *B123252,1 NNA (End)
        
          IF EMPTY(&lcTmpRoll..nApply)
            lnRolBal   = &lcTmpRoll..nBalance
          ELSE
            lnRolBal   = &lcTmpRoll..nApply
          ENDIF
        ELSE
          lnRolBal   = &lcTmpRoll..nApply + &lcTmpRoll..nBalance
        ENDIF
        lnOldRoBal = &lcTmpRoll..nBalance
        lcTrnCode  = &lcTmpRoll..RolTranCd
        lcRSession = &lcTmpRoll..cRSession
        llFndInTmp = .T.
      ENDIF
      SHOW GET lcRollID DISABLE
      SHOW GET pbRollBr DISABLE
    ENDIF  
  
  
  *E301235,1 (Start)
  ENDIF
  *E301235,1 (End)

ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvAplPosAdj
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : roll qty
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvAplPosAdj()
*!*************************************************************

FUNCTION lfvAplPosAdj

*B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
*IF lcTrType = '3'
IF lcTrType $ '39'    
*B123252,1 NNA (End)

  IF lnRolBal < 0
    lnRolBal = lnOldVal
    WAIT "Roll stock could not be less than zero." WINDOW
    _CUROBJ = _CUROBJ
  ELSE
    llApply = .T.
  ENDIF
ELSE  
  *B603262 (Start)
  *Comment this part of code and rewrite it
  *IF lnRolBal <> lnOldVal
  *  IF lnUsrApply = lnTotApply AND lnRolBal > lnOldVal
  *    lnRolBal = lnOldVal
  *    WAIT 'The entry quantity is applied completely.' WINDOW
  *    _CUROBJ = _CUROBJ
  *  ELSE
  *    IF lcTrnCode = '3' AND lnRolBal < lnOldRoBal
  *      IF lnAdjStk > 0
  *        lnRolBal = lnOldVal
  *        WAIT "You cannot reduce an existing quantity since you are doing a receiving adjustment" WINDOW
  *        _CUROBJ = _CUROBJ
  *      ENDIF
  *    ELSE
  *      IF lnUsrApply - (lnOldVal-lnOldRoBal) + (lnRolBal-lnOldRoBal) > lnTotApply
  *        lnRolBal = lnOldVal
  *          WAIT 'The applied quantity exceeds than total entry quantity. ' + ;
  *               'The applied quantity can be increased by only ' + ALLTRIM(STR(lnTotApply - lnUsrApply)) WINDOW
  *        _CUROBJ = _CUROBJ
  *      ELSE
  *        llApply = .T.
  *      ENDIF
  *    ENDIF
  *  ENDIF
  *ENDIF
  IF lnRolBal <> lnOldVal

    *B603280,3 (Start)
    IF lnRolBal < 0
      lnRolBal = lnOldVal
      WAIT "Negative values not allowed." WINDOW
    ELSE
    *B603280,3 (End)

      IF lnAdjStk < 0
        IF lnRolBal > lnOldVal
          WAIT "You cannot increase Roll quantity since you are doing a Issuing Return PO." WINDOW
          lnRolBal = lnOldVal
          _CUROBJ = _CUROBJ
        ELSE      
          IF ABS(lnUsrApply) = ABS(lnTotApply) AND lnRolBal < lnOldVal
            WAIT 'The entry quantity is applied completely.' WINDOW
            lnRolBal = lnOldVal
            _CUROBJ = _CUROBJ
          ELSE
            IF ABS(lnUsrApply) - ABS(lnOldVal-lnOldRoBal) + ABS(lnRolBal-lnOldRoBal) > ABS(lnTotApply)
              WAIT 'The applied quantity exceeds than total entry quantity. ' + ;
                   'The applied quantity can be increased by only ' + ALLTRIM(STR(ABS(lnTotApply) - ABS(lnUsrApply))) WINDOW
              lnRolBal = lnOldVal
              _CUROBJ = _CUROBJ
            ELSE
              llApply = .T.
            ENDIF
          ENDIF
        ENDIF
      ELSE
        IF lnUsrApply = lnTotApply AND lnRolBal > lnOldVal
          lnRolBal = lnOldVal
          WAIT 'The entry quantity is applied completely.' WINDOW
          _CUROBJ = _CUROBJ
        ELSE
          IF lcTrnCode = '3' AND lnRolBal < lnOldRoBal
            IF lnAdjStk > 0
              lnRolBal = lnOldVal
              WAIT "You cannot reduce an existing quantity since you are doing a receiving adjustment" WINDOW
              _CUROBJ = _CUROBJ
            ENDIF
          ELSE
            IF lnUsrApply - (lnOldVal-lnOldRoBal) + (lnRolBal-lnOldRoBal) > lnTotApply
              lnRolBal = lnOldVal
              WAIT 'The applied quantity exceeds than total entry quantity. ' + ;
                   'The applied quantity can be increased by only ' + ALLTRIM(STR(lnTotApply - lnUsrApply)) WINDOW
              _CUROBJ = _CUROBJ
            ELSE
              llApply = .T.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    *B603280,3 (Start)
    ENDIF
    *B603280,3 (End)
  ENDIF
  *B603262 (End)
ENDIF  

*!*************************************************************
*! Name      : lfvbpOk
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate OK push button
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvbpOk()
*!*************************************************************

FUNCTION lfvbpOk

CLEAR READ

*!*************************************************************
*! Name      : lfvbpCancel
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : validate cancel push button
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvbpCancel()
*!*************************************************************

FUNCTION lfvbpCancel

llApply = .F.
CLEAR READ

*!*************************************************************
*! Name      : lfActRolScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : trap some keys while running roll screen
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfActRolScr()
*!*************************************************************

FUNCTION lfActRolScr

ON KEY LABEL ESCAPE
ON KEY LABEL ESCAPE DO lfvbpCancel

*!*************************************************************
*! Name      : lfvpbClose
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate close push button
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvpbClose()
*!*************************************************************

FUNCTION lfvpbClose

*-- Check if the current entry is valid before closing the window.
*-- Check if the applied amount is equal to the amount specified 
*-- by the user, if not, confirm the new value from the user, 
*-- or return to the same screen for modifications.

DO CASE
  *E301345,1 (Start)
  *CASE lnUsrApply  = 0
  CASE lnUsrApply  = 0 AND lcTrType # '3'
  *E301345,1 (End)
    IF gfModalGen('QRM36066B36001','ALERT') = 1
      *B803712,1 ABD Delete the line that equal to Zero. [Begin]
       DELETE ALL FOR NAPPLY <= 0
      *B803712,1 ABD [End]
      llRetVal = .F.
      *B603950,1 ABD Fix Bug that didn't update Some record in the rolls file. [Begin]

      *B604023,1 AAN Add if statment to do this select statment when (lltrkrolls and fabric.ltrkrolls) = .T. [Begin].      
      *SELECT (lcTmpRoll)
      *GOTO TOP
      IF llTrkRolls .AND. Fabric.ltrkrolls             
        SELECT (lcTmpRoll)
        GOTO TOP
      ELSE
        SELECT (lcTmpJour)
        GOTO TOP
      ENDIF
      *B604023,1 AAN [End].      

      *B603950,1 ABD [End]
      CLEAR READ 
    ENDIF
    
  *B603813,1 ABD If User change here mined and didn't enter any roll
  *B603813,1     dont tell him any message [Begin]
  *CASE lnUsrApply <> lnTotApply
  CASE lnUsrApply <> lnTotApply .AND. lnUsrApply # 0
    *--  Chenge the wrong message.
    *IF gfModalGen('QRM36067B36001','ALERT') = 1    
    *B603813,1 ABD Message Text : The quantity you have just applied does not equal 
    *B603813,1 ABD Message Text : The total quantity you want to xxxxx , Would you 
    *B603813,1 ABD Message Text : like to modify the applied quantity ?.    
    *B603813,1 ABD Message No.  : 36173
    *B603813,1 ABD Buttom  Text : YES ---- No 
    *B603813,1 ABD Buttom  No.  : 36001
    DO CASE
      *N000388,1 KHM (Begin) Include the material invoice lcTrType  '15'
      *CASE  lcTrType = '1'  AND lcType # 'A'
      CASE  lcTrType $ '15'  AND lcType # 'A'
      *N000388,1 KHM (End)
      
        IF lnUsrApply  > 0
          lcMessage = 'receive'
        ELSE
          *N000388,1 KHM (Begin) handle the case of material invoice lcMessage = 'issue'
          *lcMessage = 'return'
          IF lcTrType = '1'
            lcMessage = 'return'
          ELSE
            lcMessage = 'Issue'
          ENDIF  
          *N000388,1 KHM (End)
        ENDIF  
      OTHERWISE
        lcMessage = 'adjust'
    ENDCASE
    IF gfModalGen('QRM36173B36001','ALERT',lcMessage) = 1
    *B603813,1 ABD [End]
      lnAdjStk = lnUsrApply
      *B603950,1 ABD Fix Bug that didn't update Some record in the rolls file. [Begin]

      *B604023,1 AAN Add if statment to do this select statment when (lltrkrolls and fabric.ltrkrolls) = .T. [Begin].      
      *SELECT (lcTmpRoll)
      *GOTO TOP
      IF llTrkRolls .AND. Fabric.ltrkrolls             
        SELECT (lcTmpRoll)
        GOTO TOP
      ELSE
        SELECT (lcTmpJour)
        GOTO TOP
      ENDIF
      *B604023,1 AAN Add if statment to do this select statment when (lltrkrolls and fabric.ltrkrolls) = .T. [End].
      *B603950,1 ABD [End]
      CLEAR READ
    ELSE
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  OTHERWISE
    *B603950,1 ABD Fix Bug that didn't update Some record in the rolls file. [Begin]

      *B604023,1 AAN Add if statment to do this select statment when (lltrkrolls and fabric.ltrkrolls) = .T. [Begin].      
      *SELECT (lcTmpRoll)
      *GOTO TOP
      IF llTrkRolls .AND. Fabric.ltrkrolls             
        SELECT (lcTmpRoll)
        GOTO TOP
      ELSE
        SELECT (lcTmpJour)
        GOTO TOP
      ENDIF
      *B604023,1 AAN Add if statment to do this select statment when (lltrkrolls and fabric.ltrkrolls) = .T. [End].      

    *B603950,1 ABD [End]
    CLEAR READ  
ENDCASE
*C126356,1 NNA 03/15/2005 (Begin) A Trigger For RES03 to remove the RollID's invoice Part if we're removed the
*C126356,1 NNA            roll after we entered
IF ASCAN(laEvntTrig,PADR("REMOVINV",10)) <> 0 
  =gfDoTriger('MAPOREC',PADR('REMOVINV',10))
ENDIF   
*C126356,1 NNA (End)

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

*E301345,1 (Start)
IF !USED(lcTmpRoll)
*E301345,1 (End)

  lcTmpRoll  = gfTempName()
  *E301235,1 add new index [begain]
  
  *E301345,1 (Start)
  *IF llExtCall
  *E301345,1 (End)
  *B603812,1 ABD add new index. [Begin]
    *DIMENSION laTags[2,2]
    DIMENSION laTags[3,3]
    *B603812,1 ABD [End]
    laTags[1,1]='cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession'
    laTags[1,2]=lcTmpRoll
    
    *B606163,1 ABD - Add cMorder to the index key. [Begin]
    IF llExtCall .AND. lcCalProg = 'ARMINV'
      laTags[2,1]='cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)+cMorder'
      laTags[2,2]='lcTmpRoll2'
    ELSE
      laTags[2,1]='cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)'
      laTags[2,2]='lcTmpRoll2'
    ENDIF
    *B606163,1 ABD - [End]

    *B603812,1 ABD add new index. [Begin]
    laTags[3,1]='cRsession+cFabric+cColor+cWareCode+cDyelot'
    laTags[3,2]='lcTmpRoll3'
    *B603812,1 ABD [End]
    
  *E301345,1 (Start)
  *ELSE
  *  DIMENSION laTags[1,2]
  *  laTags[1,1]='cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession'
  *  laTags[1,2]='lcTmpRoll'
  *ENDIF
  *E301345,1 (End)
  
  *E301235,1 add new index [end..]

  *B606163,1 ABD - Add new fields in case we are in the material invoice sales order.[Begin]
  IF llExtCall .AND. lcCalProg = 'ARMINV'
      DIMENSION laFileStru[25,4]
  ELSE
  *B606163,1 ABD - [End]
    DIMENSION laFileStru[23,4]
    *B606163,1 ABD - End if for if statment. [Begin] 
  ENDIF
  *B606163,1 ABD - [End]

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

  *B603280,4 (Start)
  *laFileStru[15,1] = 'nBalance'
  *laFileStru[15,2] = 'N'
  *laFileStru[15,3] = 8
  *laFileStru[15,4] = 0

  *laFileStru[16,1] = 'nReceived'
  *laFileStru[16,2] = 'N'
  *laFileStru[16,3] = 8
  *laFileStru[16,4] = 0

  *laFileStru[17,1] = 'nIssued'
  *laFileStru[17,2] = 'N'
  *laFileStru[17,3] = 8
  *laFileStru[17,4] = 0

  *laFileStru[18,1] = 'nApply'
  *laFileStru[18,2] = 'N'
  *laFileStru[18,3] = 8
  *laFileStru[18,4] = 0

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
  *B603280,4 (End)

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

  *B606163,1 ABD - Add new fields in case we are in the material invoice sales order.[Begin]
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
  *B606163,1 ABD - [End]

  *E301235,1 change the calling style
  *=gfCrtTmp(lcTmpRoll ,@laFileStru,'cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession',lcTmpRoll)
  =gfCrtTmp(lcTmpRoll ,@laFileStru,@laTags)
  *E301235,1 change the calling style [end..]

*E301345,1 (Start)
ENDIF
*E301345,1 (End)

*B802955,1[Start] if +ve adjustment also collect the rolls data
*IF lcTrType='3' OR lnAdjStk < 0
*B603812,1 ABD Fix bug that the screen should browse the available Rolls for that item. [Begin]
*IF lcTrType$'23'

*N000388,1 KHM (Begin) Handle the material invoice.
*IF lcTrType$'123'
IF lcTrType$'1235'
*N000388,1 KHM (End)

*B603812,1 ABD [End]
*B802955,1[End]
  
  *E301345,1 (Start)
  IF !USED(lcFullRoll)
  *E301345,1 (End)  

    lcFullRoll = gfTempName()

    *E301345,1 (Start)
    *=gfCrtTmp(lcFullRoll ,@laFileStru,'cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession',lcFullRoll)
    DIMENSION laTagArr[2,2]
    laTagArr[1,1] = 'cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession'
    laTagArr[1,2] = lcFullRoll
    laTagArr[2,1] = 'cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)'
    laTagArr[2,2] = 'lcFullRoll2'
    =gfCrtTmp(lcFullRoll ,@laFileStru,@laTagArr)
    *E301345,1 (End)
  
  *E301345,1 (Start)
  ENDIF
  *E301345,1 (End)
  
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

  *E301235,1 (Start)
  *B603232,1 (Start)
  *IF TYPE('llExtCall') = 'U'

  *B606094,1 ABD - Check on type '5'. [Begin]
  *IF (TYPE('llExtCall') = 'U' OR !llExtCall) OR (llExtCall AND lcTrType = '1' AND lnAdjStk < 0)
  IF (TYPE('llExtCall') = 'U' OR !llExtCall) OR (llExtCall AND lcTrType $ '15' AND lnAdjStk < 0)
  *B606094,1 ABD - [End]

  *B603232,1 (End)
  *E301235,1 (End)
  
    *E301345,1 (Start)
    PRIVATE lcTag,llColldata
    lcTag = ORDER(lcTmpRoll)
    SET ORDER TO lcTmpRoll2 IN (lcTmpRoll)
  
    *B606163,1 ABD - collecte the data again if not the same order. [Begin]
    *llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)
    IF llExtCall .AND. lcCalProg = 'ARMINV'
     llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)+lcTrCode,lcTmpRoll)
    ELSE
      llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)
    ENDIF
    *B606163,1 ABD - [End]

    SET ORDER TO (lcTag) IN (lcTmpRoll)
    IF llColldata
    *E301345,1 (End)
    
      SELECT Rolls
      IF SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot)
        *B606094,1 ABD - Don't show rolls with zero.  [Begin]
        *SCAN WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
                   lcFabric +lcColor+lcWareCode+lcFDyelot 
        *B607338,1 AMH To privante show rolls with zero that not be done correctlly if we
        *B607338,1 AMH don't show all records with nQtyBal = 0 but the correct is
        *B607338,1 AMH don't show all recors with the same crollid if the record of trancd
        *B607338,1 AMH = '1' has nqtybal = 0 [Start]
        *SCAN WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
                   lcFabric +lcColor+lcWareCode+lcFDyelot FOR nQtyBal # 0
        lcSkipRoll = SPACE(20)
        SCAN WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
                   lcFabric +lcColor+lcWareCode+lcFDyelot
          IF TRANCD = '1' .AND. NQTYBAL = 0
            lcSkipRoll = CROLLID
          ENDIF
          IF CROLLID = lcSkipRoll
            LOOP
          ENDIF
        *B607338,1 AMH [End]

        *B606094,1 ABD - [End]
          IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot,lcTmpRoll)
            IF Rolls.TranCd = '1'
              SELECT (lcTmpRoll)
              APPEND BLANK
              *B603262,1 (Start)
              *REPLACE cTrn_Seq   WITH ''                           ,;
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
                      nUnitCost  WITH MatInvJl.nUnitCost           ,;
                      nUntCstBuy WITH MatInvJl.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH Rolls.nQtyBal                ,;
                      nApply     WITH IIF(lcTrType='3',nBalance,0) ,;
                      RolTranCd  WITH '3'                          ,;
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='3'
              *B603280,2 (Start)
              *REPLACE cTrn_Seq   WITH ''                           ,;
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
                      nUnitCost  WITH MatInvJl.nUnitCost           ,;
                      nUntCstBuy WITH MatInvJl.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH Rolls.nQtyBal                ,;
                      nApply     WITH IIF(lcTrType='3',nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType='1' AND lnAdjStk<0,'2','3'),;
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='3'

              *B802955,1 [Start] fill the nApply with the original roll qty
              *                  in case of +ve Adjustment
              *REPLACE cTrn_Seq   WITH ''                           ,;
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
                      nUnitCost  WITH MatInvJl.nUnitCost           ,;
                      nUntCstBuy WITH MatInvJl.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH IIF(lcTrType='1',Rolls.nQtyBal/Fabric.Conv,Rolls.nQtyBal) ,;
                      nApply     WITH IIF(lcTrType='3',nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType='1' AND lnAdjStk<0,'2','3'),;
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='3'                 ,;
                      LineNo     WITH lnLineNo

              *B606094,1 ABD - check on type '5' . [Begin]
              *REPLACE cTrn_Seq   WITH ''                           ,;
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
                      nUnitCost  WITH MatInvJl.nUnitCost           ,;
                      nUntCstBuy WITH MatInvJl.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH IIF(lcTrType='1',Rolls.nQtyBal/Fabric.Conv,Rolls.nQtyBal) ,;
                      nApply     WITH IIF(lcTrType='3' OR (lcTrType='2' AND lnAdjStk > 0),nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType='1' AND lnAdjStk<0,'2','3'),;
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='3'                 ,;
                      LineNo     WITH lnLineNo
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
                      *B606094,1 ABD - [End]
               *B802955,1[End]

            *B606163,1 ABD - assign zero to issue Qty and Update the order field with 
            *B606163,1 ABD - the order No. [Begin]
            IF llExtCall .AND. lcCalProg = 'ARMINV'
              REPLACE nIssue WITH 0        ,;
                      cMorder WITH lcTrCode

            ENDIF
            *B606163,1 ABD - [End]

            *B603280,2 (End)
            *B603262,1 (End)
            ENDIF
          ENDIF
          SELECT (lcFullRoll)
          IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession,lcFullRoll)
            APPEND BLANK
            *B603262,1 (Start)
            *REPLACE cTrn_Seq   WITH ''                  ,;
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
                    RolTranCd  WITH '3'                 ,;
                    lStatus    WITH "S"                 ,;
                    lNeeded    WITH lcTrType='3'
            *B606094,1 ABD - Divide the Qty With the Sell Conv.  [Begin]
            *REPLACE cTrn_Seq   WITH ''                  ,;
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
                    RolTranCd  WITH IIF(lcTrType='1' AND lnAdjStk<0,'2','3'),;
                    lStatus    WITH "S"                 ,;
                    lNeeded    WITH lcTrType='3'        ,;
                    LineNo     WITH lnLineNo
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
                    *B606094,1 ABD - [End]
          *B603262,1 (End)
          ENDIF
          *B603280,2 (Start)
          *REPLACE nBalance WITH nBalance+IIF(Rolls.TranCd='2',-Rolls.nQty,Rolls.nQty),;
                  nApply   WITH IIF(lcTrType='3',nBalance,0)

          *B606094,1 ABD - Divide the Qty With the Sell Conv.  [Begin]
          *REPLACE nBalance WITH nBalance+(IIF(Rolls.TranCd='2',-Rolls.nQty,Rolls.nQty)/IIF(lcTrType='1',Fabric.Conv,1)),;
                  nApply   WITH IIF(lcTrType='3',nBalance,0)
          REPLACE nBalance WITH nBalance+(IIF(Rolls.TranCd='2',-Rolls.nQty,Rolls.nQty)/IIF(lcTrType$'15',;
                                IIF(lcTrType ='1',Fabric.Conv,m.nSellconv),1)),;
                  nApply   WITH IIF(lcTrType='3',nBalance,0)
          *B606094,1 ABD - [End]

          *B606163,1 - ABD Update the ORder No. [Begin]
          IF llExtCall .AND. lcCalProg = 'ARMINV'
            REPLACE cMorder WITH lcTrCode
          ENDIF
         *B606163,1 - ABD [End]  

         *B603280,2 (End)
        ENDSCAN
      *E301345,1 (Start)
      ENDIF
      *E301345,1 (End)
    ENDIF
  *E301235,1 (Start)
  ENDIF
  *E301235,1 (End)

  *B603950,1 ABD   Remark cause in cuttkt must apply the issued qty to rolls 1st [Begin]
  *B603812,1 ABD Subtract the Qty That have CT on this rolls. [Begin]
  *SELECT (lcTmpRoll)
  *lcTag = ORDER(lcTmpRoll)
  *SET ORDER TO lcTmpRoll3 IN (lcTmpRoll)
  *SELECT (lcFullRoll)
  *GOTO TOP
  *SCAN FOR cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession = ;
  *         '***** N/A *****     '+lcFabric +lcColor+lcWareCode+lcFDyelot
  *  *- Need this index .['cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession']
  *  IF SEEK(cRsession+cFabric+cColor+cWareCode+cDyelot,lcTmpRoll)
  *    SELECT (lcTmpRoll)
  *    REPLACE nBalance WITH nBalance+&lcFullRoll..nBalance
  *    SELECT (lcFullRoll)
  *  ENDIF                   
  *ENDSCAN
  *SELECT (lcTmpRoll)
  *SET ORDER TO (lcTag) IN (lcTmpRoll)  
  *- subtract all recored that trancd = 2
  *B603812,1 ABD [END]
  *B603950,1 ABD [End]

  SELECT Rolls
  SET ORDER TO lcRolTag
  SELECT MatInvJl
  SET ORDER TO lcJorTag
  SELECT Rolls
  SET RELATION TO &lcRollRela. INTO MatInvJl

ENDIF

***************************************************************************

FUNCTION lfFabWarDy
PARAMETERS llToWare
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

*--1 ) Update Stock and Avarege cost in Style file. ------------
IF lfCheckUnCmp(lnTmpStp)
  SELECT Fabric
  =RLOCK()
  
  *B603044 (Start)
  *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
          nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
          nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
          OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk>0,lnAdjStk,0),0),;
          OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
          Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
          nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
  *-- HDM B802450,1 [Start] If over receive subtract old onorder value
  *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
          nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
          nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
          nAveCstBuy WITH nFAve_Cost * Conv,;
          OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk>0,lnAdjStk,0),0),;
          OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
          Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
          nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)

  *B603311,1[Start] If There is an original fabric
  *                  - Update onhand for the receiving fabric
  *                  - Skip updating the on order for the receiving fabric
  *                  - update original fabric onorder 
  *                  - Skip updating original fabric onhand
  
  *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
          nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
          nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
          nAveCstBuy WITH nFAve_Cost * Conv,;
          OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk > 0,MIN(lnOldOnOrd,lnAdjStk),0),0),;
          OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
          Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
          nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
  IF EMPTY(lcOrgFab)
    *B604678,1 MHM 08/20/2001 add issued in case of return PO to OnOrder [Start]
    *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
            nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Conv,;
            OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk > 0,MIN(lnOldOnOrd,lnAdjStk),0),0),;
            OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
            
    *--variable to check if come from po OR not 
    *llComFrmPo = IIF((lcProg = 'MARECI'),.T.,.F.)
    *lnReplace = IIF(lnAdjStk > 0,IIF(IIF(llComFrmPo,(lnpolin-lnreclin)<>0,;
    *              lnOldOnOrd>0),MIN(lnOldOnOrd,lnAdjStk),IIF(lcProg = 'MAMNREC'AND lnOldOnOrd = 0,lnAdjStk,0)),lnAdjStk)
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
                  
    *!C200254,1 ABD Issue and receive the material manufacturing 
    *!C200254,1 ABD Rework order from MAMNREC program & I will
    *!C200254,1 ABD Not update the On order Qty. [Begin]
    IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,; 
              nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
              nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Conv                               ,;
              OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0)
    ELSE
    *!C200254,1 ABD [End]
      *B606285,1 ABD - Update the Average Cost for the fabric in case transfer with the new Cost. [Begin]      
      *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
      *        nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
      *        nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
      *        nAveCstBuy WITH nFAve_Cost * Conv,;
      *        OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0),;
      *        OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
      *        Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
      *        nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
        REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
              nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
              nFAve_Cost WITH IIF(lcIRType='I',IIF(llToWare,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand),nFAve_Cost),;
              IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Conv,;
              OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0),;
              OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
              Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
              nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
    *B606285,1 ABD - [End]
      *!C200254,1 ABD End if for if statment. [Begin]
    ENDIF
    *!C200254,1 ABD [End]
    *B604678,1 MHM  [End]

  ELSE
    
    *!C200254,1 ABD Issue and receive the material manufacturing 
    *!C200254,1 ABD Rework order from MAMNREC program & I will
    *!C200254,1 ABD Not update the On order Qty. [Begin]
    IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
              nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
              nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Conv
    ELSE
      *!C200254,1 ABD - [End]
      =RLOCK()
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
              nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
              nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Conv,;
              OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
              Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
              nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
      UNLOCK
    *!C200254,1 ABD - End if for if statment. [End]
    ENDIF
    *!C200254,1 ABD - [End]

    *-- Seek the original fabric/color then updte it
    IF SEEK(lcOrgFab+lcOrgClr)
      =RLOCK()
      REPLACE  ONORDER WITH MAX((ONORDER - lnAdjStk),0)
      UNLOCK
    ENDIF
    
  ENDIF
  *B603311,1[End]
  *-- HDM B802450,1 [End]
  
  *B603044 (End)
  
  UNLOCK 
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep)  
  *--Call TraceKey global function.
ENDIF 

lnTmpStp = lnTmpStp + 1

*--2 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
IF lfCheckUnCmp(lnStarStep+1)
  SELECT FabDye
  *B604580,1 HBG 07/08/2001 If the warehouse of the PO changed , update only the OnOrder Qty of 
  *B604580,1                the orginal WareHouse and only the OnHand Qty of the new WareHouse [Begin]
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
    *!C200254,1 ABD Issue and receive the material manufacturing 
    *!C200254,1 ABD Rework order from MAMNREC program & I will
    *!C200254,1 ABD Not update the On order Qty. [Begin]
    IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,; 
              nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
              nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Fabric.Conv
    ELSE
      *!C200254,1 ABD [End]
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
              nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
              nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Fabric.Conv ,;
              OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
              Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
              nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
    *!C200254,1 ABD End if For If statment. [Begin]            
    ENDIF
    *!C200254,1 ABD [End]
  ELSE
  *B604580,1 [End]
  
    =SEEK(lcFabric+lcColor+IIF(llToWare,lcToWare,lcWareCode)+SPACE(10),'FabDye')
    =RLOCK()
  
    *B603044 (Start)
    *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
            nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk>0,lnAdjStk,0),0),;
            OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
          
    *-- HDM B802450,1 [Start] If over receive subtract old onorder value
    *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
            nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
            nAveCstBuy WITH nFAve_Cost * Fabric.Conv ,;
            OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk>0,lnAdjStk,0),0),;
            OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)

    *B603311,1[Start] If There is an original fabric
    *                  - Update onhand for the receiving fabric
    *                  - Skip updating the on order for the receiving fabric
    *                  - update original fabric onorder
    *                  - Skip updating original fabric onhand
  
    *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
             nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
             nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
             nAveCstBuy WITH nFAve_Cost * Fabric.Conv ,;
             OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk>0,MIN(lnOldOnOrd , lnAdjStk),0),0),;
             OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
             Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
             nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
    IF EMPTY(lcOrgFab)
      *B604678,1 MHM 08/20/2001 add issued in case of return PO  to OnOrder [Start]
      *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
              nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
              nFAve_Cost WITH IIF(lcIRType='I' ,nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Fabric.Conv ,;
              OnOrder    WITH OnOrder - IIF(lcTrType='1',IIF(lnAdjStk>0,MIN(lnOldOnOrd , lnAdjStk),0),0),;
              OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
              Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
              nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)

      *--variable to check if come from po OR not 
      *llComFrmPo = IIF((lcProg = 'MARECI'),.T.,.F.)
      *lnReplace = IIF(lnAdjStk > 0,IIF(IIF(llComFrmPo,(lnpolin-lnreclin)<>0,;
      *              lnOldOnOrd>0),MIN(lnOldOnOrd,lnAdjStk),IIF(lcProg = 'MAMNREC'AND lnOldOnOrd = 0,lnAdjStk,0)),lnAdjStk)
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
                  
    *B604678,1 MHM [End]
    *!C200254,1 ABD Issue and receive the material manufacturing 
    *!C200254,1 ABD Rework order from MAMNREC program & I will
    *!C200254,1 ABD Not update the On order Qty. [Begin]
    IF TYPE('llRwIssRec') = 'L' .AND. llRwIssRec
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,; 
              nStkVal    WITH nStkVal + MatInvJl.nStkVal                      ,;
              nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Fabric.Conv,;
              OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0)
    ELSE
      *!C200254,1 ABD [End]
      *B606285,1 ABD - Update the Average Cost for the fabric in case transfer with the new Cost. [Begin]      
      *REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
      *        nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
      *        nFAve_Cost WITH IIF(lcIRType='I',nFAve_Cost,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
      *        nAveCstBuy WITH nFAve_Cost * Fabric.Conv,;
      *        OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0),;
      *        OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
      *        Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
      *       nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
      REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
              nStkVal    WITH nStkVal + MatInvJl.nStkVal,;
              nFAve_Cost WITH IIF(lcIRType='I',IIF(llToWare,IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand),nFAve_Cost),;
              IIF(OnHand=0,MatInvJl.nUnitCost,nStkVal/OnHand)),;
              nAveCstBuy WITH nFAve_Cost * Fabric.Conv,;
              OnOrder    WITH OnOrder - IIF(lcTrType='1',lnReplace,0),;
              OnRet      WITH OnRet   + IIF(lcTrType='1',IIF(lnAdjStk<0,lnAdjStk,0),0),;          
              Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
             nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)
    *B606285,1 ABD - [End]      
    *!C200254,1 ABD End IF For If statment. [Begin]
    ENDIF  
    *!C200254,1 ABD [End]

    ELSE 
      *!C200254,1 ABD Issue and receive the material manufacturing 
      *!C200254,1 ABD Rework order from MAMNREC program & I will
      *!C200254,1 ABD Not update the On order Qty. [Begin]
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
      *!C200254,1 ABD [End]      
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
        *!C200254,1 ABD End For IF statment. [Begin]    
      ENDIF
      *!C200254,1 ABD [End]
      ENDIF
  *B604580,1 HBG 07/08/2001 If the warehouse of the PO changed [Begin]
  ENDIF  
  *B604580,1 [End]
  UNLOCK



  *-- HDM B802450,1 [End]

  *B603044 (End)
  
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep+1)  
  *--Call TraceKey global function.

  lnPrvQty  = OnHand
  lnPrvVal  = nStkVal
  lnDyeCost = IIF(FabDye.OnHand = 0,FabDye.nFAve_Cost,FabDye.nStkVal/FabDye.OnHAnd)

ENDIF 

lnTmpStp = lnTmpStp + 1

*--3 )  Update Stock in Fabric Dyelot file Dyelot record. --------
IF !EMPTY(lcFDyelot) AND SEEK(lcFAbric+lcColor+IIF(llToWare,lcToWare,lcWareCode)+lcFDyelot,'FabDye')
  IF lfCheckUnCmp(lnStarStep+2)
    SELECT FabDye
    =RLOCK()
    REPLACE OnHand     WITH OnHAnd  + MatInvJl.nReceived - MatInvJl.nIssued ,;
            Usage      WITH Usage   - IIF(lcTrType='4',lnAdjStk,0),;
            nMatWIP    WITH nMatWIP - IIF(lcTrType='4',lnAdjStk,0)

    UNLOCK 
    *--Update Uncomplete session Step.
    =lfUpdStep(lnStarStep+2)  
    *--Call TraceKey global function.
  
    lnPrvQty = OnHand
    lnPrvVal = OnHand * lnDyeCost

  ENDIF 
ENDIF

lnTmpStp = lnTmpStp + 1

SELECT(lnCurAlias)
************************************************************************

FUNCTION lfCalcStkVal

PRIVATE lnOrgJRec,lnRJRec,llDiffSess,lcOrgTran,lcJourTag,lnCurRec,lcOrgTrTyp
llDiffSess = .F.

lnStkVal = lnAdjStk * lnNewCost
lnCost   = lnNewCost

*N000388,1 KHM (Begin) Handle the material invoice lcTrType $ '15'
*IF lcIRType = 'I' AND lcTrType = '1'
IF lcIRType = 'I' AND lcTrType $ '15'
*N000388,1 KHM (End)

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

*DO CASE
*  CASE lcRI = 'I'
*    IF lnWOldStk = 0
*      lnRet = lnWStkVal # 0
*    ELSE
*      lnRet = lnWOldStk # lnAdjStk OR lnWOldCst # lnNewCost
*    ENDIF
*  CASE lcRI = 'R'
*    lnRet =  lnWOldCst # lnNewCost OR ;
*             (lnAdjStk # 0 AND lnWOldStk # lnAdjStk) 
*ENDCASE
*B126598,1 ASH 02/27/2005 (Begin) Fix bug of updating wrong cost in case of locking.
lnWOldCst = ROUND(lnWOldCst ,3)
*B126598,1 ASH 02/27/2005 (End)
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
  *=SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FabDye')
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
  IF lnDiffere # 0 AND lfCheckUnCmp(lnTmpStp)
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

    *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
    *B038753,1                transaction cost in case of standard costing method [Begin].
    REPLACE nTranCost WITH lnTranCost
    *B038753,1 KHM 12/18/2004 [End]

    *-- Call global function to add audit fields info.
    =gfAdd_Info('MatInvJl')

    *--Update and Flush Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  
  
    lnTmpStp = lnTmpStp + 1
    =lfUpdGLDist(.F.,.T.)
    =lfFabWarDy()

  ENDIF
ENDIF

SET FILTER To &lcCurFlt.
SET ORDER TO (lcJourTag) IN MatInvJl
SELECT (lnCurAlias)

************************************************

*E301345,1 use this function instead of lfActBrwRo
FUNCTION lfLOtRoBrw
SELECT(lcFileToUse)
GO TOP
lnNewRec = RECNO(lcFileToUse)
IF TYPE('llExtCall') = 'U' OR !llExtCall
  IF !EMPTY(lcToWare) AND lcToWare = lcWareCode
    lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
                 "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) " + ;
                 "AND LineNo = lnlineNo"
  ELSE
    lcSerchExp = ""
  ENDIF
ELSE

  *N000388,1 KHM (Begin) Handle the material invoice lcTrType $ '15'
  *IF lnAdjStk < 0  AND lcTrType = '1'
  IF lnAdjStk < 0  AND lcTrType $ '15'
  *N000388,1 KHM (End)
  
    *B604051,1 AAN Add only in case of the MA setup of keep trak rolls = Y and the fabri 
    *B604051,1 AAN  is used Rolls there is a search exp.[Begin].
    *lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
               "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) "
    IF llTrkRolls .AND. Fabric.ltrkrolls             
      lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
                   "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) "
    ELSE
      lcSerchExp = "FOR LIKE(lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
                   "cFabric+cColor+cWareCode+cDyelot+cRsession) "
      *lcSerchExp = ""
    ENDIF
    *B604051,1 AAN .[End].
  ELSE
    *B121414,1 NNA 03/09/2004 (Begin) Add the P/O# to the filter that to make a correct Receiving 
	*B121414,1                        Operation if we are receiving more one order in the same session    
    *lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
                 "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) " + ;
                 "AND LineNo = lnlineNo"

    lcSerchExp = "FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcWareCode+lcFDyelot+'??????'," + ;
                 "cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession) " + ;
                 "AND (LineNo = lnlineNo AND Ctran = lctrcode OR EMPTY(Ctran))"		
    *-- (Ctran = lctrcode) in case we want to change the Qty. Of Existing Rolls For a PO# NO.
    *-- EMPTY(Ctran) in case we add  new Rolls
    *B121414,1 NNA (End)
  ENDIF
ENDIF

BROWSE FIELDS &lcRlBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOEDIT             ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwRo();
       VALID :F lfvBrwRo();
       TITLE lcRollTitl   ;
       &lcSerchExp        ;
       WINDOW (lcLotRo2) IN WINDOW (lcLotRo)
= lfwBrwRo()

*************************************************************************

*E301345,1 use this function instead of lfvNewMod

FUNCTION lfvNewRol

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
IF !SEEK(SPACE(20),lcTmpRoll)
  SELECT(lcTmpRoll)
  APPEND BLANK
ENDIF
=lfwBrwRo()

*B803712,1 ABD- Generate Sequential numbers when receiving by rolls
*B803712,1 ABD- When press New and didn't open the rol id filed. [Begin]
IF  lnAdjStk > 0 .AND. llGenRolId
  lcRollID  = IIF(llExtCall,lfSequence(),gfSequence('CROLLID'))
  SHOW GET lcRollID   DISABLE
  SHOW GET lnAppRolQt ENABLE
  REPLACE cFabric   WITH lcFab          ,;
          cColor    WITH lcClr          ,;
          cWareCode WITH lcWare         ,;
          cDyelot   WITH lcDye          ,;
          cRollId   WITH lcRollID       ,;
          RolTranCd WITH '1'            ,;
          cRSession WITH lcFJlSess      ,;
          nBalance  WITH 0              ,;
          LineNo    WITH lnLineNo
ENDIF
*B803712,1 ABD [End]

IF EMPTY(lcRollID)
  _CUROBJ = OBJNUM(lcRollID)
ELSE
  _CUROBJ = OBJNUM(lnAppRolQt)
ENDIF
*C126356,1 NNA 03/15/2005 (Begin) A Trigger For RES03 to Repeat the RollID's invoice Part if we're in the same MA_PO
IF ASCAN(laEvntTrig,PADR("REPETINV",10)) <> 0 
  =gfDoTriger('MAPOREC',PADR('REPETINV',10))
ENDIF   
*C126356,1 NNA (End)
  
*************************************************************************

*E301345,1 use this function instead of lfvRem

FUNCTION lfvRemRol

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT (lcTmpRoll)

*B603980,1 ABD Fix Bug alias not found that we will not decrease the qty that recived before.
*B603980,1 ABD in case we made Adj+ but we will remove the new line only.   [Begin]
IF TYPE('lcType') = "C" .AND. lcTYPE = "A" .AND. cRSession <> lcFJlSess
  WAIT "Applied quantity is not allowed to decrease." WINDOW
  lcVarName = VARREAD()
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*B603980,1 ABD [End]

lnUsrApply = lnUsrApply - nApply

IF &lcTmpRoll..RolTranCd = '1'
  BLANK
  DELETE
  LOCATE &lcSerchExp
ELSE
 
  *B123252,1 NNA 06/17/2004 (Begin) if Physical material inventory (3) or inventory Locking(9)
  *REPLACE nApply    WITH 0 ,;
          lNeeded   WITH !(lcTrType = '3' AND RolTranCd = '1' AND nApply = 0) OR ;
                          (lcTrType = '3' AND RolTranCd $ '23') OR nApply # 0,;
          RolTranCd WITH IIF(RolTranCd='3','2',RolTranCd)
  REPLACE nApply    WITH 0 ,;
          lNeeded   WITH !(lcTrType $ '39' AND RolTranCd = '1' AND nApply = 0) OR ;
                          (lcTrType $ '39' AND RolTranCd $ '23') OR nApply # 0,;
          RolTranCd WITH IIF(RolTranCd='3','2',RolTranCd)
  *B123252,1 NNA (End)
ENDIF

*C126356,1 NNA 03/15/2005 (Begin) A Trigger For RES03 to Get the previous RollID's invoice Part 
*C126356,1 NNA            if we removed the current line
IF ASCAN(laEvntTrig,PADR("PREVINV",10)) <> 0 
  =gfDoTriger('MAPOREC',PADR('PREVINV',10))
ENDIF   
*C126356,1 NNA (End)

=lfwBrwRo()

SELECT (lnCurAlias)
    
*!*************************************************************

*E301345,1 use this function instead of lfvRoll_ID

FUNCTION lfvRollID

PRIVATE lcRolTag,lnCurAlias,llFound
lnCurAlias = SELECT(0)
lcRolTag = ORDER('Rolls')
*B603981,1 ABD Fix bug that accept exist rolls & Set order to rolls Only. [Begin]
*SET ORDER TO RollItem IN Rolls
SET ORDER TO ROLLS IN Rolls
*C126356,1 NNA 03/15/2005 (Begin) Trigger For RES03 to Save the Invoice NO. that the User Typed 
*C126356,1 NNA            at the Roll_ID to Be defaulted with the Next Lines in the Same MA_PO 
IF ASCAN(laEvntTrig,PADR("SAVINVNO",10)) <> 0 
  =gfDoTriger('MAPOREC',PADR('SAVINVNO',10))
ENDIF   
*C126356,1 NNA (END)
lcRollid = IIF(EMPTY(lcRollid),'',PADR(ALLTRIM(lcRollid),20))
*B603981,1 ABD  [End]

IF !EMPTY(lcRollid)
  *E500329,1/*E500330,1 WAB - if generate roll id setup is true add zeros to the left of the no
  *E500329,1/*E500330,1 WAB (START) instead of the user write all zeros at the left og the roll id no
  *IF SEEK(lcFab+lcClr+lcWare+lcDye+lcRollId+'1','Rolls') AND lcTrType # '3'  
  IF llGenRolID
    *--incase of recieve replace with 6 seros @left cause we save in temp file so we generate a 
    *--temp sequentioal no
    lcRollid = PADL(ALLTRIM(lcRollId),IIF(llExtCall,6,20),"0")
    IF llExtCall
      *--add space to the right so we can get the correct result @ seek experetion in case of temp 
      *--sequential no caue it has a 6 C length
      lcRollid = PADR(lcRollId,20)
    ENDIF
  ENDIF
  *E500329,1/*E500330,1 WAB (END) 
  
  
  *B603981,1 ABD Fix bug that accept exist rolls. [Begin]
  *IF SEEK(lcFab+lcClr+lcWare+lcDye+lcRollId+'1','Rolls') AND !(lcTrType $ '13')
  *IF SEEK(lcFab+lcClr+lcWare+lcDye+lcRollId+'1','Rolls')
  *--Don't accept exist rolls  
  IF SEEK(lcRollId,'Rolls')  
    *B603981,1 ABD  [End]
    *B121414,1 NNA 03/09/2004 (Begin) Prevent to add the roll ID more than once if it exists in the Rolls file.
    *WAIT "This roll id already exist." WINDOW
    *lcRollID = ''
    *_CUROBJ = _CUROBJ
		  =gfModalGen("TRM36064B00000","DIALOG",ALLTRIM(lcRollID))
          lcRollId = ''
          _CUROBJ = OBJNUM(lcRollID)
          RETURN
    *B121414,1 NNA (End)          
  ELSE
    SELECT(lcTmpRoll)
    llFound = SEEK(lcRollID+lcFab+lcClr+lcWare+lcDye)
    IF llFound
      LOCATE REST WHILE cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession = ;
                        lcRollID+lcFab+lcClr+lcWare+lcDye ;
                  FOR   LineNo = lnLineNo
      llFound = FOUND()
    ENDIF
    IF !llFound
      *E500329,1/*E500330,1 WAB - create a temp sequence no for roll id in case of recieving po 
      *E500329,1/*E500330,1 WAB - & create a real sequence no  in case of hisical or adjust inventory
      *E500329,1/*E500330,1 WAB - (START)

      *-- message ( "the Roll Id is not Found in data file . Generate , Reenter ")
      lnMessAns = IIF(llGenRolId,gfModalGen("TRM00001B36016","DIALOG","Rolls ID. "),1)
      IF lnMessAns = 2
        lcRollId = ''
        _CUROBJ = OBJNUM(lcRollID)
        RETURN
      ELSE
        IF llGenRolId
          lcRollID  = IIF(llExtCall,lfSequence(),gfSequence('CROLLID'))
        ENDIF
      *E500329,1/*E500330,1 WAB - (END)

	    *B121414,1 NNA 03/09/2004 (Begin) Prevent to add the roll ID more than once.
		lnOldRec=Recno() - 1		&& get the new record number
		IF SEEK(lcRollID)
	      GOTO lnOldRec				&& get the pointer on the new record
		  =gfModalGen("TRM36064B00000","DIALOG",ALLTRIM(lcRollID))
          lcRollId = ''
          _CUROBJ = OBJNUM(lcRollID)
          RETURN
        ELSE   
		*B121414,1 NNA (End)
          IF SEEK(SPACE(20)) 
            REPLACE cFabric   WITH lcFab          ,;
                  cColor    WITH lcClr          ,;
                  cWareCode WITH lcWare         ,;
                  cDyelot   WITH lcDye          ,;
                  cRollId   WITH lcRollID       ,;
                  RolTranCd WITH '1'            ,;
                  cRSession WITH lcFJlSess      ,;
                  nBalance  WITH 0              ,;
                  LineNo    WITH lnLineNo
          ENDIF
		ENDIF

      *E500329,1/*E500330,1 WAB - (START) end if for if lnMessage = 2

      ENDIF

      *E500329,1/*E500330,1 WAB - (END)
    ENDIF
  ENDIF
ELSE
  *B121414,1 NNA 03/09/2004 (Begin) Show a message if the user input nothing in the Roll ID Text 
  WAIT WINDOW "You have to input a Roll ID " 
  lcRollID = ''
  _CUROBJ = OBJNUM(lcRollID)
  RETURN
  *B121414,1 NNA (End)          
ENDIF
= lfwBrwRo()

SET ORDER TO (lcRolTag) IN Rolls

SELECT(lnCurAlias)


*!**************************************************************************
*! Name      : lfSequence
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/06/2000
*! Purpose   : create a temp. sequence no for roll id
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfSequence()
*!**************************************************************************
*-*E500329,1
*!**************************************************************************
FUNCTION lfSequence
PRIVATE lnSequence
lnSequence = 0
SCAN
  lnSequence = MAX(lnSequence,VAL(cRollId))
ENDSCAN
=SEEK(SPACE(20)) 
RETURN PADR(PADL(ALLTRIM(STR(lnSequence+1)),6,"0"),20)
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
*B605925,4 ABD - [Begin]
FUNCTION lfRcvLock
PRIVATE lnAlias

lnAlias = SELECt (0)

IF lcTrType = '9' .AND. lfDoPhys('I'))
  *--Create a main record in journal file.
  IF lfCheckUnCmp(lnTmpStp)
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
            nIssued    WITH IIF(lcTrType = '5',ABS(lnAdjStk) * m.nSellConv,ABS(lnAdjStk)),;
            nStkVal    WITH IIF(lcTrType = '5',ABS(lnAdjStk)  * nUnitCost * laConv,-1*(nIssued* nUnitCost)),;
            Reference  WITH lcRefer    ,;
            cAdjReason WITH lcAdjCdRsn ,;
            cGlMatAdj  WITH lcAdjAcct  ,;
            cRSession  WITH IIF(lcTrType $ '45',lcCTISess,'') ,;
            cISession  WITH IIF(lcTrType $ '45',lcCTRSess,lcFJlSess) ,;
            cTrn_Seq   WITH cISession,;
            cIMTyp     WITH IIF(TYPE('lcCShetTyp')='C',lcCShetTyp,cIMTyp),;
            cTktNo     WITH IIF(!EMPTY(cIMTyp),lcPONo,cTktNo),;
            nMPrvSQty  WITH lnPrvQty ,;
            nPrvSVal   WITH lnPrvVal ,;
            LineNo     WITH lnLineNo ,;
            llockflg   WITH lcTrType='9'

      IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
        REPLACE cOprCode  WITH lcOprCode ,;
                cLotNo    WITH lcLotNo
      ENDIF
      *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
      *B038753,1                transaction cost in case of standard costing method [Begin].
      REPLACE nTranCost WITH lnTranCost
      *B038753,1 KHM 12/18/2004 [End]
    
    *-- Call global function to add audit fields info.
    =gfAdd_Info('MatInvJl')

    *--Update and Flush Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  
  ENDIF
  lnTmpStp = lnTmpStp + 1

  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()
  
  IF lcTrType = '9' AND ((!EMPTY(lcFDyelot) AND llAdjYes) OR EMPTY(lcFDyelot))
    =lfLkAdjRec()
  ENDIF
  
ENDIF


SELECT(lnAlias)

*- End OF lfRcvLock.
*B605925,4 ABD - [End]


