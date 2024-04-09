*:**************************************************************************
*: Program file  : gfItemCrl.prg
*: Program desc. : Style/Material Stock Control Function,
*:                 Style/Material Average Cost Update and Inventory Journal Update.
*:        System : ARIA APPAREL SYSTEM 4.0
*:        Module : Material (MA).
*:     Developer : AHMED MAHER (AMH)
*:          Date : 08/02/2004
*:**************************************************************************
*: Passed Parameters  :
*:         lcTrType => Transaction Type.
*:                     '1' for Adjustment
*:                     '2' for Physical inventory
*:                     '3' for Invoice
*:                     '4' for Void invoice
*:                     '5' for Receive Cutting ticket,PO,Dye order,Inter PO
*:                     '6' for Issue Inter PO,Adornment order,return PO
*:                     '7' for Void credit memo
*:                     '8' for Inventory markdown
*:                     '9' for Issue style/fabric component
*:        lcInvType => Inventory type ('0001' for style and '0002' for fabric).
*:          lcStyle => Style/fabric.
*:       lcWareCode => Warehouse Code.
*:         lcDyelot => Dyelot ,pass empty if not a dyelot fabric or system
*:                     dyelot No, (if you want to add this dyelot you have to 
*:                     add it before calling this function).
*:         ldTrDate => Transaction Date.
*:       ldPostDate => Posting Date.
*:         lcTrCode => Transaction Code as ex. Rec PO No.
*:      laAdjStk[9] => Issued or received Stock as an array per sizes and total.
*:        lnNewCost => New receiving cost ,Pass it Zero if it is issue transaction
*:                     it will be calculated in this function except issue return P/o.
*:          lcRefer => Transaction Reference.  
*:       lcAdjCdRsn => Adjustment code reason.
*:         lcAdjRef => Adjustment reference.
*: laGLInvAry[X,13] => Array that holds all GL entry information needed for 
*:                     Inventory control or Anventory adjustment.
*:                     Put the array elements as follows :
*:                     [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:                     [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:                     [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:                     [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*:                     If no G/L is used pass this array as len 1 and .F.
*:                     Pass Category type '006' as a first array element.
*:        lcFJlSess => Variable that hold the session No ,if it is empty or not
*:                     passed Session No will be generated
*:        lcBusDocu => Transaction Business Document Type.
*:        lcStyType => Transaction Style PO Type.
*:        lcCTRSess => Receive Session number.
*:        lcCTISess => Issue Session number.
*:         lcToWare => Translate to Warehouse Code.
*:      lcLastRSess => 
*:   laLockInfo[11] => Array that holds all GL entry information.
*:                     Put the array elements as follows :
*:                     [1] Qty1 ,[2] Qty2 ,[3] Qty3 ,[4] Qty4 ,[5] Qty5 ,[6] Qty6
*:                     [7] Qty7 ,[8] Qty8 ,[9] TotQty ,[10] StkVal ,[11]Locking Date.
*:       lcTranCode => SQL transaction number for update SQL files with it.
*:                     if not passed then the program create new transaction then update
*:                     all SQL files with it and commite this transaction.
*:         lnLineNo => Transaction Line Number.
*:  laOtherPar[x,2] => Array that holds all other parameters needed.
*:                     Put the array elements as follows :
*:                     [x,1] Parameter name
*:                     [x,2] Parameter value
*:**************************************************************************
*: Note    : Files ->Temprory Line file and Temprorary G/L Distribution files must be
*:           opened before calling this function.
*:**************************************************************************
*: Update  : ITEM      => (Item  Record) Ave_Cost field.
*:           ITEMLOC   => (Itm/War rec.) Stk1->8 and TotStk and Ave_Cost fields.
*:           ITEMLOC   => (Itm/Dye rec.) Stk1->8 and TotStk fields.
*:           ITEMJRNL  => Create a new record.
*:           TmpGLDist => Create a new records for GL inventory entres.
*:**************************************************************************
*: Returns :
*: RETURN 0 Zero  If No updates was done.
*: RETURN 1 One   If all files was succesfuly updated.
*:**************************************************************************
*N038370,1 AMH
*:**************************************************************************
*: Modifcations:
*: B125939,1 AMH 01/09/2005 Fix bug of update avarige cost.
*: B126063,1 AMH 01/13/2005 Fix bug of Auto cost adj. use the same adj. session ID.
*: B607585,1 AMH 05/25/2005 Add nLineNo field to ITEMJRNL file.
*: E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File.
*: B131608,1 WSH 05/03/2006 Fix bug of not updating Item Dyelot Record in ItemLoc.
*: B607883,1 MMT 12/18/2006 Fix bug of History usage is update in ISSUE/RET T20061211.0008
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   T20070117.0001
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP T20070206.0007 
*: N000548,1 MMT 08/20/2007 Convert locking Screen to Aria4XP T20060908.0003 
*: N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[T20060908.0003]
*: B608417,1 WAM 02/03/2008 Fix adding issue records in ITEMJRNL table when costing method is FIFO
*: B608422,1 WAM 02/06/2008 Fix error while update material inventory transfer [T20080117.0001]
*: B608635,1 WAM 07/28/2008 Fix wrong update of TO warehouse inventory [T20080722.0002]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[T20090303.0017]
*! B608847,1 MMT 04/14/2009 Fix bug of Not updating rolls nqty field while +VE adj [T20090304.0016]
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[T20091204.0001]
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[T20110823.0017][Start]
*! B609812,1 MMT 02/01/2012 Material Average cost is -ve in Item and Itemloc tables[T20120124.0019]
*:**************************************************************************

PARAMETERS lcTrType,lcInvType,lcStyle,lcWareCode,lcDyelot,ldTrDate,ldPostDate,lcTrCode,laAdjStk,lnNewCost,;
           lcRefer,lcAdjCdRsn,lcAdjRef,laGLInvAry,lcFJlSess,lcBusDocu,lcStyType,lcCTRSess,lcCTISess,lcToWare,;
           lcLastRSess,laLockInfo,lcTranCode,lnLineNo,laOtherPar


IF TYPE('laOtherPar') # "L"
  FOR lnI = 1 TO ALEN(laOtherPar,1) 
    &laOtherPar[lnI,1] = laOtherPar[lnI,2]
  ENDFOR
ENDIF  

IF TYPE('lnLineNo') # 'N'
  lnLineNo = 0
ENDIF  
IF TYPE('llUseACst') = 'U'
  llUseACst = .F.
ENDIF
IF TYPE('llLastRec') = 'U'
  llLastRec = .T.
ENDIF
IF TYPE('llAdjYes') # 'L'
  llAdjYes = .F.
ENDIF


*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
IF lcInvType = '0002'
  lcitmpic  = gfItemMask("HI",'', lcInvType)
  lenclrlen  = LEN(gfitemmask("PN", "", lcInvType))
  lnmajlength = LEN(gfItemMask('PM','', lcInvType))
  lcSepar   = SUBSTR(lcitmpic,lnmajlength +1,1)
  lcFabric  =  PADR(SUBSTR(lcStyle,1,lnmajlength),7) 
  lcColor   =  RIGHT(lcStyle,lenclrlen)
  lcFDyelot = lcDyelot
  lnAdjStk  = laAdjStk[9]
ENDIF   
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]


*If the system tyoe is POS and the transaction created for an warhouse 
*not belong to the current site return without do any thing.
lcSysType = gfGetMemVar('M_SYSTYPE')
IF lcSysType = 'P' AND lcInvType = '0001'
  PRIVATE llOpnWarhs 
  llOpnWarhs = .F.
  IF !USED('WAREHOUS')
    llOpnWarhs = gfOpenFile(oAriaApplication.DataDir+"WAREHOUS","WAREHOUS","SH")
  ENDIF
  IF SEEK(lcWareCode,'WAREHOUS') AND WAREHOUS.cSiteId <> oAriaApplication.CurrentSite
    IF llOpnWarhs
      USE IN WAREHOUS
    ENDIF
    RETURN (1)
  ENDIF
ENDIF

PRIVATE lnStkVal,lnCost
STORE 0 TO lnStkVal,lnCost

PRIVATE lcLastRSess
IF TYPE('lcLastRSess') # 'C'
  lcLastRSess = SPACE(6)
ENDIF  
IF TYPE('lcToWare') # "C"
  lcToWare = ''
ENDIF  

PRIVATE lcRelCode
IF TYPE('lcRelCode') # 'C'
  lcRelCode = SPACE(6)
ENDIF  

PRIVATE llDontUpdate
IF TYPE('llDontUpdate') # 'L'
  llDontUpdate = .F.
ENDIF  


*--Initialize function variables.
PRIVATE lcOldWAr,lnFOldStk,lnFOldCst,lnWOldStk,lnWOldCst,lnSAveCost,lnWAveCost,laOldstk,lcCostMeth,;
        lcAdjAcct,llUInvtry,lnFStkVal,lnWStkVal,lnCurRec,;
        lcFJlSess,llTrkRolls,lcBusDocu,lcStyType,llWareHous,lcCTRSess,lcCTISess

lcVoucNo = IIF(TYPE('lcVoucNo') = 'U' ,"",lcVoucNo)
llWareHous = gfGetMemVar('M_WareHouse')='Y'
IF TYPE('llExtCall')='U'

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
IF (TYPE('lcTmpJour')= 'U' AND TYPE('lcTmpRoll')= 'U' AND TYPE('lcFullRoll')= 'U') OR lcTrType = '8'
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

  PRIVATE lcTmpJour,lcTmpRoll,lcFullRoll
  STORE SPACE(0) TO lcTmpJour,lcTmpRoll,lcFullRoll
  
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
ENDIF
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

ENDIF
llTrkRolls = IIF(lcInvType='0001',.F.,(ALLTRIM(gfGetMemVar('M_TrkRolls')) = 'Y'))
IF TYPE('lcFJlSess') = 'L'
  lcFJlSess = ''
ENDIF
llGenRolId  = ('MA' $ oAriaApplication.CompanyInstalledModules AND ALLTRIM(gfGetMemVar('M_GENROLID')) = 'N')

*--Style/Fabric and Warehouse Average Cost,Old Stock and Old Cost variables.
STORE 0 TO lnFOldStk,lnFOldCst
STORE 0 TO lnWOldStk,lnWOldCst
DIMENSION laOldstk[9]
laOldstk   = 0
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.  
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcDyelot   = IIF(EMPTY(lcDyelot),SPACE(10),lcDyelot)
llDyeLvl   = !EMPTY(lcDyelot)

*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,!EMPTY(laGLInvAry[1,1]))

*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
LOCAL lcCstMthVar
lcCstMthVar = IIF(lcInvType='0001','M_Cost_Meth','M_MatCstMth')
lcCostMeth  = gfGetMemVar(lcCstMthVar)
lcCostMeth  = IIF(lcInvType='0001' and lcCostMeth='L','I',lcCostMeth)

DIMENSION laIndex[1,3]
laIndex = ''
laIndex[1,1] = 'CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)'
laIndex[1,2] = 'StyInvJl'
*B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
*IF !gfOpnSqlFl('itemjrnl',"itemjrnl","cinvtype='"+lcInvType+"' and style='"+lcStyle+"' and cwarecode='"+;
                                     lcWareCode+"'",@laIndex,"styinvjl")
PRIVATE lcItemKeyV, lcWareCodV
lcItemKeyV = lcStyle
lcWareCodV = lcWareCode
IF !gfOpnSqlFl('itemjrnl',"itemjrnl","cinvtype='"+lcInvType+"' and style = ?m.lcItemKeyV " +;
						 " AND cwarecode = ?m.lcWareCodV ",@laIndex,"styinvjl")
*B607658,1 KHM 07/07/2005 [End]
  RETURN (0)
ENDIF

*--Check the existing of the Style/Fabric and
*--Point the record in Item and ItemLoc files.
STORE .T. TO llItmWarDy,llItmWar,llItm

*-- Open the ITEM and ITEMLOC files.
DIMENSION laIndex[1,3]
laIndex = ''
laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
laIndex[1,2] = 'StyDye'

*B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
*IF !gfOpnSqlFl('itemloc',"itemloc","cinvtype='"+lcInvType+"' and style='"+lcStyle+"'",@laIndex,"stydye")
IF !gfOpnSqlFl('itemloc',"itemloc","cinvtype='"+lcInvType+"' and style = ?m.lcItemKeyV",@laIndex,"stydye")
*B607658,1 KHM 07/07/2005 [End]
  STORE .F. TO llItmWarDy,llItmWar
ENDIF


laIndex[1,1] = 'cInvType+Style'
laIndex[1,2] = 'Style'

IF !gfOpnSqlFl('item',"item","cinvtype='"+lcInvType+"' and style= ?m.lcItemKeyV ",@laIndex,"Style")
  llItm = .F.
ENDIF

llItmWarDy = llItmWarDy AND SEEK(lcInvType+lcStyle+lcWareCode+lcDyelot ,'itemloc')
llItmWar   = llItmWar   AND SEEK(lcInvType+lcStyle+lcWareCode+SPACE(10),'itemloc')
llItm      = llItm      AND SEEK(lcInvType+lcStyle,'item')
IF !(llItm AND llItmWar AND llItmWarDy)
  *--The material/Color ???? record are missing,Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  lcMsgExp = ALLTRIM(gfItemMask("HI","",lcInvType)) +" "+ ALLTRIM(lcStyle)
  =gfModalGen('TRM42260B00036','DIALOG',lcMsgExp)
  RETURN (0)
ENDIF

IF EMPTY(lcRelCode)
  IF lcTrType $ '56'
    lcRelCode = item.cConvBuy
  ENDIF
  
  IF lcTrType $ '34'
    lcRelCode = item.cConvSell
  ENDIF
ENDIF

PRIVATE lnConv
lnConv = 1

IF !EMPTY(lcRelCode)
  =gfGetUOMData(lcRelCode, '', '', @lnConv)
ENDIF

*--Case of Invoice or Void credit memo transactions, Put the Qty as
*--negative values since it is an Issue transaction.
*--If it is an Inventory adjustment or Receve return P/o the Qty must
*--Passes to this function as negative values since we have a Receive
*--case for this Inventory adj. and Receive P/o.
IF lcTrType $ '37'
  FOR lnI=1 TO 9
    laAdjStk[lnI] = -(ABS(laAdjStk[lnI]))
  ENDFOR
ENDIF

*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '13679' , 'I' , 'R' )

*--Check the Item Inventory Yes or No.
llUInvtry = Item.lInvSty

*--Get the Old Stock and Cost before updateing the new tansaction.
SELECT itemloc
SUM FOR Dyelot = SPACE(10) TotStk TO lnFOldStk
lnFOldCst = ABS(IIF(lnFOldStk=0,Item.Ave_Cost,Item.nStkVal / lnFOldStk))
lnFStkVal = IIF(lcTrType $ '28',0,item.nStkVal)
lnOldSVal = IIF(lcTrType $ '28',nStkVal,0)
=SEEK(lcInvType+lcStyle+lcWareCode+SPACE(10))

IF lcTrType = '8'
  lnWOldStk = laLockInfo[9]
  lnWOldCst = ABS(IIF(lnWOldStk=0,Ave_Cost,laLockInfo[10] / lnWOldStk))
  lnWStkVal = laLockInfo[10]
ELSE
  lnWOldStk = TotStk
  lnWOldCst = ABS(IIF(TotStk=0,Ave_Cost,nStkVal / TotStk))
  lnWStkVal = IIF(lcTrType='2',0,nStkVal)
ENDIF

PRIVATE lnDyeCost
IF lcTrType = '8'
  lnDyeCost = IIF(laLockInfo[9]=0,Ave_Cost,laLockInfo[10]/laLockInfo[9])
ELSE
  lnDyeCost = IIF(TotStk = 0,Ave_Cost,nStkVal/TotStk)
ENDIF  

PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = TotStk
lnPrvVal = nStkVal
IF !EMPTY(lcDyelot)
  =SEEK(lcInvType+lcStyle+lcWareCode+lcDyelot)
  lnPrvQty  = TotStk
  lnPrvVal  = TotStk * lnDyeCost
  *lnWStkVal = IIF(lcTrType='8',laLockInfo[10],lnPrvVal)
ENDIF

*--Temp Old Stock Array used in Physical or Markdown 
*--issue the old stock first and then receive the pysical quantity.
*-- In all other cases this array is Zero.
IF lcTrType = '8'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
  
  *B126063,1 AMH Fix bug of old quantity is zero in case of adj. [Start]
  *IF lcTrType $ '2'
  *  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
  *  FOR lnI = 1 TO 9
  *    laOldStk[lnI] = -laOldStk[lnI]
  *  ENDFOR
  *ENDIF
  IF lcTrType $ '12'
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
    IF lcTrType = '2'
      FOR lnI = 1 TO 9
        laOldStk[lnI] = -laOldStk[lnI]
      ENDFOR
    ENDIF
  ENDIF
  *B126063,1 AMH [End]
  
ENDIF

*--Calculate Transaction Issuing Cost in differend cost methods.
IF lcIRType = 'I'
  DO CASE
    CASE lcCostMeth = 'A'   && Average.
      IF !llUseACst
        lnNewCost = IIF(llWareHous,lnWOldCst,lnFOldCst)
      ENDIF

    CASE lcCostMeth = 'S'   && Standard.
      IF !llUseACst
        lnNewCost = item.TotCost
      ENDIF

   CASE lcCostMeth $ 'FIL' AND lcTrType <> '9'
        IF !(lcTrType $ '3456' AND laAdjStk[9] < 0 AND TYPE('llExtCall')='L' AND llExtCall) 
 *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*!*          IF !lfJrRoData()
*!*            SELECT (lcOldWAr)
*!*            RETURN (0)
*!*          ELSE
*!*            IF !EMPTY(lcToWare)
*!*              PRIVATE lcOrgWare
*!*              lcOrgWare = lcWareCode
*!*              lcWareCode = lcToWare
*!*              FOR lnI = 1 TO 9
*!*                laAdjStk[lnI] = ABS(laAdjStk[lnI])
*!*              ENDFOR
*!*              *MMT
*!*              lnAdjStk = laAdjStk[9]
*!*              *MMT
*!*              lcIRType = 'R'
*!*              IF !lfJrRoData()
*!*                SELECT (lcOldWAr)
*!*                RETURN (0)
*!*              ENDIF
*!*              lcWareCode = lcOrgWare
*!*              FOR lnI = 1 TO 9
*!*                laAdjStk[lnI] = - laAdjStk[lnI]
*!*              ENDFOR
*!*              *MMT
*!*              lnAdjStk = laAdjStk[9]
*!*              *MMT
*!*              lcIRType = 'I'
*!*            ENDIF
*!*          ENDIF
        IF !((lcTrType $ '1256') AND llTrkRolls AND Item.ltrkrolls) AND !((lcTrType $ '1' AND lcCostMeth = 'L' AND laAdjStk[9] < 0)) AND !(lcTrType $ '6' AND lcCostMeth = 'L')
          llRetvalue  = .T.   
          DO (oAriaApplication.ApplicationHome+'maJrRoData.PRG') WITH lcTrType,;
             lcInvType,lcStyle,;
             lcDyelot,lcWareCode,;
             lcToWare,laAdjStk,lcTmpJour,lcFullRoll,;
             lcTmpRoll,ldTrDate,ldPostDate 
          IF !llRetvalue    
            RETURN (0)
          ENDIF 
        ENDIF   
      ENDIF
 *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[END]
 
    CASE lcCostMeth $ 'FIL' AND lcTrType = '9'
      *--Receiving cost will be passed as parameter in this function.
  ENDCASE
ELSE
  
  IF lcTrType <> '9'
    IF lcTrType = '4' AND  lcCostMeth $ 'FI'  && FIFO or LIFO.
      lcInvJour = gfTempName()
      = lfInvJour()
    ELSE
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *!*         IF (TYPE('llExtCall')='U' OR !llExtCall) AND !lfJrRoData()
    *!*            RETURN (0)
    *!*         ENDIF
     IF !((lcTrType $ '1256') AND llTrkRolls AND Item.ltrkrolls) AND !((lcTrType $ '1' AND lcCostMeth = 'L' AND laAdjStk[9] < 0)) AND !(lcTrType $ '6' AND lcCostMeth = 'L')
         llRetvalue  = .T.   
         DO (oAriaApplication.ApplicationHome+'maJrRoData.PRG') WITH lcTrType,;
          lcInvType,lcStyle,;
          lcDyelot,lcWareCode,;
          lcToWare,laAdjStk,lcTmpJour,lcFullRoll,;
          lcTmpRoll,ldTrDate,ldPostDate
        IF !llRetvalue    
          RETURN (0)
        ENDIF 
        *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[END]
      ENDIF 
    ENDIF
  ELSE
    *--Receiving cost will be passed as parameter in this function.
  ENDIF
ENDIF

*--  Update Material journal file. -------------------------------------
*--Read session no.
IF lcTrType <> '9'
  IF lcIRType = 'R' AND lcCostMeth $ 'LFI'
    SELECT itemjrnl
    LOCATE FOR cDyelot+cRSession+cISession = lcDyelot+lcFJlSess+SPACE(6) AND LineNo = lnLineNo
    IF FOUND()
      lcFJlSess = gfSEQUENCE('GLSession')
    ENDIF
  ENDIF
  IF EMPTY(lcFJlSess) 
    lcFJlSess = gfSequence('GLSESSION')
  ENDIF
ENDIF

IF lcTrType = '9' AND EMPTY(lcCTRSess)
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
IF !(lcTrType $ '349') .OR. (lcTrType $ '34' .AND. lcIRType = 'I')
  IF (llTrkRolls .AND. Item.ltrkrolls)

  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
  *    IF lcTrType = '2'
    IF lcTrType $ '28'
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    
      = lfUpdtRoll(.T.)
    ENDIF
    = lfUpdtRoll()
  ENDIF
ENDIF

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*!*  IF TYPE('llExtCall') = 'U' OR !llExtCall OR llLastRec
*!*    IF USED(lcTmpJour)
*!*      USE IN (lcTmpJour)
*!*    ENDIF
*!*    *--Erase the temp. journal file.
*!*    ERASE (oAriaApplication.WorkDir+lcTmpJour+'.DBF')
*!*    ERASE (oAriaApplication.WorkDir+lcTmpJour+'.CDX')

*!*    IF USED(lcTmpRoll)
*!*      USE IN (lcTmpRoll)
*!*    ENDIF
*!*    *--Erase the temp. journal file.
*!*    ERASE (oAriaApplication.WorkDir+lcTmpRoll+'.DBF')
*!*    ERASE (oAriaApplication.WorkDir+lcTmpRoll+'.CDX')

*!*    IF USED(lcFullRoll)
*!*      USE IN (lcFullRoll)
*!*    ENDIF
*!*    *--Erase the temp. journal file.
*!*    ERASE (oAriaApplication.WorkDir+lcFullRoll+'.DBF')
*!*    ERASE (oAriaApplication.WorkDir+lcFullRoll+'.CDX')
*!*  ENDIF
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

IF !llDontUpdate
  LOCAL llNewTran
  llNewTran = EMPTY(lcTranCode)
  
  IF llNewTran
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
    IF TYPE('lcTranCode') = 'N'
      =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
      RETURN (0)
    ENDIF
  ENDIF
  
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('item',lcTranCode,SET("Datasession"),;
                        'CINVTYPE,STYLE','ITEM','STYLE')
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN (0)
  ENDIF
  
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemloc',lcTranCode,SET("Datasession"),;
                        'CINVTYPE,STYLE,CWARECODE,DYELOT','ITEMLOC','STYDYE')
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN (0)
  ENDIF
  
  *B607585,1 AMH Add nLineNo field to ITEMJRNL file.
  *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemjrnl',lcTranCode,SET("Datasession"),;
                        'CINVTYPE,STYLE,CWARECODE,DTRDATE,CSESSION,CIRTYPE,CTRCODE,LINENO','ITEMJRNL','STYDATE')
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemjrnl',lcTranCode,SET("Datasession"),;
                        'CINVTYPE,STYLE,CWARECODE,DTRDATE,CSESSION,CIRTYPE,CTRCODE,LINENO,CTRTYPE,NLINENO','ITEMJRNL','STYDATE')
  *B607585,1 AMH [End]
  
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN (0)
  ENDIF
  
  IF llNewTran
    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
    IF lnConnectionHandlar # 1
      =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
      RETURN (0)
    ENDIF
  ENDIF
  
  IF USED('item')
    USE IN item
  ENDIF
  
  IF USED('itemloc')
    USE IN itemloc
  ENDIF
  
  IF USED('itemjrnl')
    USE IN itemjrnl
  ENDIF
ENDIF

SELECT (lcOldWAr)
RETURN 1

*!*************************************************************
*! Name      : lfUpdtRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/12/2004
*! Purpose   : Update rolls file
*!*************************************************************
*! Parameter : If it calls for Physical issue.
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  =lfUpdtRoll()
*!*************************************************************
FUNCTION lfUpdtRoll
LPARAMETERS llPhyIssu



PRIVATE lcRoTmpUse,llPhyIssu,lnTrQty

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*lcRoTmpUse = IIF((lnAdjStk<0 OR llPhyIssu) .AND. lcTrType # '5',lcFullRoll,lcTmpRoll)
lcRoTmpUse = IIF((lnAdjStk<0 OR llPhyIssu) .AND. lcTrType $ '34',lcFullRoll,lcTmpRoll)
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
  
*-- add new records in the Rolls file from lcTmpRolls
PRIVATE lnAlias,lcRolTag
lnAlias = SELECT(0)
lcRolTag = ORDER('Rolls')

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*SET ORDER TO TAG ROLLITEM IN Rolls
SELECT ROLLS 
=gfSetOrder("ROLLITEM")
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [ENd]

SELECT (lcRoTmpUse)
GO TOP

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*SCAN FOR cfabric+ccolor+cwarecode+cdyelot = lcFabric+lcColor+lcWareCode+lcFDyelot
* IF !llPhyIssu AND SEEK(&lcRoTmpUse..cFabric+&lcRoTmpUse..cColor+;
                         &lcRoTmpUse..cWareCode+&lcRoTmpUse..cDyelot+;
                         &lcRoTmpUse..cRollId+'1','ROLLS')
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]
*SCAN FOR Style+cwarecode+cdyelot = lcStyle+lcWareCode+lcFDyelot
SCAN FOR Style+cwarecode+cdyelot = lcStyle+lcWareCode+lcFDyelot AND cRollId<>'***** N/A *****'
*! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]
  IF !llPhyIssu AND gfSEEK(&lcRoTmpUse..Style+;
                         &lcRoTmpUse..cWareCode+&lcRoTmpUse..cDyelot+;
                         &lcRoTmpUse..cRollId+'1','ROLLS')
                         
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    SELECT ROLLS
    
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *IF lcTrType='3'
    *REPLACE nQtyBal WITH ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='1',lnConv,1)
    
    *N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[Start]
    *IF lcTrType='2'
    IF lcTrType $'28'
    *N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[End]
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
	  *   REPLACE nQtyBal WITH ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='5',lnConv,1)
    *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[Start]
    *  gfREPLACE("nQtyBal WITH ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='5',lnConv,1)")
      SCAN REST WHILE STYLE+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION  = &lcRoTmpUse..Style+;
                         &lcRoTmpUse..cWareCode+&lcRoTmpUse..cDyelot+;
                         &lcRoTmpUse..cRollId+'1'
                                                                                                
        gfREPLACE("nQtyBal WITH 0")
      ENDSCAN     
      SELECT Rolls                       
      APPEND BLANK 
      REPLACE cRollItem WITH &lcRoTmpUse..cFabric    ,;
              Color     WITH &lcRoTmpUse..cColor     ,;
              cWareCode WITH &lcRoTmpUse..cWareCode  ,;
              Dyelot    WITH &lcRoTmpUse..cDyelot    ,;
              cRollId   WITH &lcRoTmpUse..cRollId    ,;
              nQty      WITH ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='5',lnConv,1),;
              nQtyBal   WITH ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='5',lnConv,1),;
              TranCd    WITH '1',;
              cSession  WITH lcFJlSess                                          ,;
              cRSession WITH  lcFJlSess,;
              cISession WITH '',;
              Style     WITH &lcRoTmpUse..Style

      =gfAdd_Info('ROLLS')
      =gfReplace('')
    *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[End]  
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]  
    
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    
    ELSE
      *-- lcRoTmpUse is either "lcTmpRoll" or "lcFullRoll"
      *-- IF it is receiving lcRoTmpUse is "lcTmpRoll"
      *-- IF it is issuing   lcRoTmpUse is "lcFullRoll"
      *-- Variable to hold the applied qty
      
      *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[Start]  
      SELECT Rolls 
      LOCATE REST WHILE STYLE+CWARECODE+DYELOT+CROLLID+TRANCD+CRSESSION  = &lcRoTmpUse..Style+;
                         &lcRoTmpUse..cWareCode+&lcRoTmpUse..cDyelot+;
                         &lcRoTmpUse..cRollId+'1' FOR nQtybal <> 0
      *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[End]  
      
      PRIVATE lnRToApply
      
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      *IF (&lcRoTmpUse..RolTranCd $ '13') OR;
                     (&lcRoTmpUse..RolTranCd = '2' AND lcTrType = '2' AND lnAdjStk > 0)
      IF (&lcRoTmpUse..RolTranCd $ '13') OR;
                    (&lcRoTmpUse..RolTranCd = '2' AND lcTrType = '1' AND lnAdjStk > 0)
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
      
        IF (&lcRoTmpUse..RolTranCd $ '13')
          lnRToApply = ABS(&lcRoTmpUse..nApply)
        ELSE
          lnRToApply = ABS(&lcRoTmpUse..nApply) - nQTYBal
        ENDIF
      ELSE
        lnRToApply = &lcRoTmpUse..nApply * (-1)
      ENDIF

      *- To issue The Qty From the Atock with sell Conv.[Begin]
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      *IF lcTrType='5'
      IF lcTrType$'34'
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
  
        lnRToApply = lnRToApply * lnConv
      ENDIF
 
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *REPLACE nQtyBal WITH nQtyBal + lnRToApply      
      gfREPLACE("nQtyBal WITH nQtyBal + lnRToApply")
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
      
      *! B608847,1 MMT 04/14/2009 Fix bug of Not updating rolls nqty field while +VE adj [Start]
      IF lnRToApply > 0
        gfREPLACE("nQty WITH nQty + lnRToApply")
      ENDIF 
      *! B608847,1 MMT 04/14/2009 Fix bug of Not updating rolls nqty field while +VE adj [End]
      
    ENDIF
  ENDIF

  lnTrQty = ABS(IIF(llPhyIssu,&lcRoTmpUse..nBalance,&lcRoTmpUse..nApply))
  
  
  *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[Start]  
  *IF lnTrQty > 0
  IF lcTrType $ '28' AND &lcRoTmpUse..RolTranCd <> '1' AND !llPhyIssu
    lnTrQty = 0
  ENDIF 
  IF lnTrQty > 0 AND !(lcTrType = '1' AND laAdjStk[9] > 0 AND &lcRoTmpUse..RolTranCd <> '1') 
  *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[End]

    *WAB - replace the roll Id in the temp file with the sequence no cause in case of 
    *WAB - (START)recieving po we create a temp. sequence no cause we used a temp file in this case
    *ABD - Fix bug that when return from exist roll don't generate sequence no. [Begin]
    
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *IF lcTrType = '1' .AND. llGenRolId .AND. lcKeyType  = 'P'
    IF lcTrType = '5' .AND. llGenRolId .AND. lcRecvType  = 'P' AND EMPTY(&lcRoTmpUse..cRollId)
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
  
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*      REPLACE &lcRoTmpUse..cRollId WITH gfSequence('CROLLID')      

      *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustemnt Material[Start]
      *gfREPLACE("&lcRoTmpUse..cRollId WITH gfSequence('CROLLID')")
      REPLACE &lcRoTmpUse..cRollId WITH gfSequence('CROLLID')      
      *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustemnt Material[End]
      
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
      
    ENDIF
    
    SELECT ROLLS
    *-- Check if we update the rolls file with the same recored before that
    *-- I shouldn't update the rolls file again , this case happen while receive the same 
    *-- Issue return 2 Po for the same color fabric.
    
    PRIVATE lcOldOrder
    lcOldOrder = ORDER()
    *-- crsession+cisession+crollitem+color+cwarecode+dyelot+crollid
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]    
    *SET ORDER TO ROLAPL
    =gfSetOrder("ROLAPL")
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]    
    
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *IF lcTrType # '5' .OR. !SEEK(&lcTmpRoll..cRSession +lcFJlSess +&lcTmpRoll..cFabric + ;
             &lcTmpRoll..cColor +&lcTmpRoll..cWarecode +&lcTmpRoll..cDyelot +&lcTmpRoll..cRollid)
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
    *IF lcTrType $ '34' .OR. !SEEK(&lcTmpRoll..cRSession +lcFJlSess +&lcTmpRoll..cFabric + ;
             &lcTmpRoll..cColor +&lcTmpRoll..cWarecode +&lcTmpRoll..cDyelot +&lcTmpRoll..cRollid)
    IF lcTrType $ '34' .OR. !gfSEEK(&lcTmpRoll..cRSession +lcFJlSess +&lcTmpRoll..Style + ;
             &lcTmpRoll..cWarecode +&lcTmpRoll..cDyelot +&lcTmpRoll..cRollid)
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
             
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    
      APPEND BLANK
    ENDIF

    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *REPLACE cRollItem WITH &lcRoTmpUse..cFabric    ,;
            Color     WITH &lcRoTmpUse..cColor     ,;
            cWareCode WITH &lcRoTmpUse..cWareCode  ,;
            Dyelot    WITH &lcRoTmpUse..cDyelot    ,;
            cRollId   WITH &lcRoTmpUse..cRollId    ,;
            nQty      WITH nQty + (lnTrQty * IIF(lcTrType$ '15',IIF(lcTrType = '1',lnConv,lnConv),1))   ,;
            nQtyBal   WITH nQtyBal + (lnTrQty * IIF(lcTrType$ '15',IIF(lcTrType = '1',lnConv,lnConv),1)),;
            TranCd    WITH IIF(llPhyIssu,'2',&lcRoTmpUse..RolTranCd)          ,;
            cSession  WITH lcFJlSess                                          ,;
            cRSession WITH IIF(TranCd $ '13',lcFJlSess,&lcRoTmpUse..cRSession),;
            cISession WITH IIF(TranCd = '2' ,lcFJlSess,cISession)
            
    REPLACE cRollItem WITH &lcRoTmpUse..cFabric    ,;
            Color     WITH &lcRoTmpUse..cColor     ,;
            cWareCode WITH &lcRoTmpUse..cWareCode  ,;
            Dyelot    WITH &lcRoTmpUse..cDyelot    ,;
            cRollId   WITH &lcRoTmpUse..cRollId    ,;
            nQty      WITH nQty + (lnTrQty * IIF(lcTrType$ '345',IIF(lcTrType = '5',lnConv,lnConv),1))   ,;
            nQtyBal   WITH nQtyBal + (lnTrQty * IIF(lcTrType$ '345',IIF(lcTrType = '5',lnConv,lnConv),1)),;
            TranCd    WITH IIF(llPhyIssu,'2',&lcRoTmpUse..RolTranCd)          ,;
            cSession  WITH lcFJlSess                                          ,;
            cRSession WITH IIF(TranCd $ '13',lcFJlSess,&lcRoTmpUse..cRSession),;
            cISession WITH IIF(TranCd = '2' ,lcFJlSess,cISession)
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    REPLACE Style WITH &lcRoTmpUse..Style
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]        
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    
    =gfAdd_Info('ROLLS')
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *SET ORDER TO &lcOldOrder
    =gfReplace("")
    =gfSetORder(lcOldOrder)
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  ENDIF
ENDSCAN

IF !EMPTY(lcToWare)
  SELECT (lcTmpRoll)
  
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *  SCAN FOR LIKE(REPLICATE('?',20)+lcFabric+lcColor+lcToWare+lcFDyelot+'??????',;
                cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession)
  *! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[Start]                
*!*	  SCAN FOR LIKE(REPLICATE('?',20)+lcStyle+lcToWare+lcFDyelot+'??????',;
*!*	                cRollID+Style+cWareCode+cDyelot+cRsession)
  SCAN FOR LIKE(REPLICATE('?',20)+lcStyle+lcToWare+lcFDyelot+'??????',;
                cRollID+Style+cWareCode+cDyelot+cRsession) AND cRollId <> '***** N/A *****'
  *! B609274,1 MMT 05/26/2010 Insert temporary issue record in rolls table[End]                
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    SELECT Rolls
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *    IF SEEK(&lcTmpRoll..cFabric+&lcTmpRoll..cColor+&lcTmpRoll..cWareCode+;
            &lcTmpRoll..cDyelot+&lcTmpRoll..cRollId+'1')
    
    *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustemnt Material[Start]        
    *IF gfSEEK(&lcTmpRoll..Style+&lcTmpRoll..cWareCode+;
            &lcTmpRoll..cDyelot+&lcTmpRoll..cRollId+'1')
    IF gfSEEK(&lcTmpRoll..Style+&lcTmpRoll..cWareCode+;
            &lcTmpRoll..cDyelot+&lcTmpRoll..cRollId+'1') AND nQTYBAL <> 0           
    *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustemnt Material[End]        
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      *REPLACE nQtyBal WITH nQtyBal + (ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='1',lnConv,1))      
      
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *REPLACE nQtyBal WITH nQtyBal + (ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='5',lnConv,1))
      gfREPLACE("nQtyBal WITH nQtyBal + (ABS(&lcRoTmpUse..nApply) * IIF(lcTrType='5',lnConv,1))")
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
      
    ENDIF
    IF &lcTmpRoll..nApply > 0
      APPEND BLANK
      llUptRcRls = .F.
      IF ASCAN(oAriaApplication.laEvntTrig,PADR("UPMATRLS",10)) <> 0 AND lcCostMeth = 'L'
        llUptRcRls = .T.
        =gfDoTriger('MAINVCT',PADR('UPMATRLS',10))
      ENDIF   
      IF !llUptRcRls
      
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      *  REPLACE cRollItem WITH &lcTmpRoll..cFabric    ,;
                Color     WITH &lcTmpRoll..cColor     ,;
                cWareCode WITH &lcTmpRoll..cWareCode  ,;
                Dyelot    WITH &lcTmpRoll..cDyelot    ,;
                cRollId   WITH &lcTmpRoll..cRollId    ,;
                nQty      WITH &lcTmpRoll..nApply * IIF(lcTrType='1',lnConv,1),;
                nQtyBal   WITH &lcTmpRoll..nApply * IIF(lcTrType='1',lnConv,1),;
                TranCd    WITH &lcTmpRoll..RolTranCd  ,;
                cSession  WITH lcFJlSess              ,;
                cRSession WITH lcFJlSess
        REPLACE cRollItem WITH &lcTmpRoll..cFabric    ,;
                Color     WITH &lcTmpRoll..cColor     ,;
                cWareCode WITH &lcTmpRoll..cWareCode  ,;
                Dyelot    WITH &lcTmpRoll..cDyelot    ,;
                cRollId   WITH &lcTmpRoll..cRollId    ,;
                nQty      WITH &lcTmpRoll..nApply * IIF(lcTrType='5',lnConv,1),;
                nQtyBal   WITH &lcTmpRoll..nApply * IIF(lcTrType='5',lnConv,1),;
                TranCd    WITH &lcTmpRoll..RolTranCd  ,;
                cSession  WITH lcFJlSess              ,;
                cRSession WITH lcFJlSess
        
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        REPLACE Style WITH &lcTmpRoll..Style
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
        *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
        
        =gfAdd_Info('ROLLS')
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        =gfReplace("")
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*SET ORDER TO (lcRolTag) IN Rolls
SELECT Rolls
=gfSetOrder(lcRolTag)
=gfTableUpdate()
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfIssue
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/12/2004
*! Purpose   : Update ItemJrnl file for issue record
*! Note      : 
*!*************************************************************
*! Parameter : 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  =lfIssue()
*!*************************************************************
FUNCTION lfIssue


*B126063,1 AMH Fix bug of issue inv. adj. with cost zero [Start]
*IF lcTrType $ '3456'
IF lcTrType $ '13456'
*B126063,1 AMH [End]

  lnStkVal = -1* (laAdjStk[9] * lnNewCost)
  lnCost   = lnNewCost
ENDIF
IF (lcCostMeth $ 'FIL' AND lcTrType <> '9') AND lnWOldStk <> 0
  SELECT (lcTmpJour)
  SCAN FOR cInvType+Style+cWareCode+cDyelot+cRSession+cISession = ;
           lcInvType+lcStyle+lcWareCode+lcDyelot
    SELECT ItemJrnl
    APPEND BLANK
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*    REPLACE cInvType   WITH lcInvType,;
            cRSession  WITH EVALUATE(lcTmpJour+'.cRSession'),;
            cISession  WITH IIF(lcTrType='9',lcCTISess,lcFJlSess),;
            cSession   WITH IIF(EMPTY(cISession),cRSession,cISession),;
            Style      WITH lcStyle,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcDyelot,;
            dTrDate    WITH ldTrDate,;
            cTrType    WITH lcTrType,;
            cTrCode    WITH IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            LineNo     WITH lnLineNo,;
            nCost      WITH EVALUATE(lcTmpJour+'.nCost')/lnConv,;
            cIRType    WITH lcIRType
            
    REPLACE cInvType   WITH lcInvType,;
            cRSession  WITH EVALUATE(lcTmpJour+'.cRSession'),;
            cISession  WITH IIF(lcTrType='9',lcCTISess,lcFJlSess),;
            cSession   WITH IIF(EMPTY(cISession),cRSession,cISession),;
            Style      WITH lcStyle,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcDyelot,;
            dTrDate    WITH ldTrDate,;
            cTrType    WITH lcTrType,;
            cTrCode    WITH IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            LineNo     WITH lnLineNo,;
            nCost      WITH EVALUATE(lcTmpJour+'.nCost')/lnConv,;
            cIRType    WITH 'I'
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*    REPLACE nStk1      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal1'),EVALUATE(lcTmpJour+'.nApply1')*lnConv),;
            nStk2      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal2'),EVALUATE(lcTmpJour+'.nApply2')*lnConv),;
            nStk3      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal3'),EVALUATE(lcTmpJour+'.nApply3')*lnConv),;
            nStk4      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal4'),EVALUATE(lcTmpJour+'.nApply4')*lnConv),;
            nStk5      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal5'),EVALUATE(lcTmpJour+'.nApply5')*lnConv),;
            nStk6      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal6'),EVALUATE(lcTmpJour+'.nApply6')*lnConv),;
            nStk7      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal7'),EVALUATE(lcTmpJour+'.nApply7')*lnConv),;
            nStk8      WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBal8'),EVALUATE(lcTmpJour+'.nApply8')*lnConv),;
            nTotStk    WITH IIF(lcTrType='2',EVALUATE(lcTmpJour+'.nBalance'),EVALUATE(lcTmpJour+'.nApply')*lnConv)

    *B608417,1 WAM 02/03/2008 Don't multiple quantity by conversion factor
    *REPLACE nStk1      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal1'),EVALUATE(lcTmpJour+'.nApply')*lnConv),;
            nStk2      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal2'),EVALUATE(lcTmpJour+'.nApply2')*lnConv),;
            nStk3      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal3'),EVALUATE(lcTmpJour+'.nApply3')*lnConv),;
            nStk4      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal4'),EVALUATE(lcTmpJour+'.nApply4')*lnConv),;
            nStk5      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal5'),EVALUATE(lcTmpJour+'.nApply5')*lnConv),;
            nStk6      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal6'),EVALUATE(lcTmpJour+'.nApply6')*lnConv),;
            nStk7      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal7'),EVALUATE(lcTmpJour+'.nApply7')*lnConv),;
            nStk8      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal8'),EVALUATE(lcTmpJour+'.nApply8')*lnConv),;
            nTotStk    WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBalance'),EVALUATE(lcTmpJour+'.nApply')*lnConv)
    REPLACE nStk1      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal1'),EVALUATE(lcTmpJour+'.nApply1')),;
            nStk2      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal2'),EVALUATE(lcTmpJour+'.nApply2')),;
            nStk3      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal3'),EVALUATE(lcTmpJour+'.nApply3')),;
            nStk4      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal4'),EVALUATE(lcTmpJour+'.nApply4')),;
            nStk5      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal5'),EVALUATE(lcTmpJour+'.nApply5')),;
            nStk6      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal6'),EVALUATE(lcTmpJour+'.nApply6')),;
            nStk7      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal7'),EVALUATE(lcTmpJour+'.nApply7')),;
            nStk8      WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBal8'),EVALUATE(lcTmpJour+'.nApply8')),;
            nTotStk    WITH -1*IIF(lcTrType$'28',EVALUATE(lcTmpJour+'.nBalance'),EVALUATE(lcTmpJour+'.nApply'))
    *B608417,1 WAM 02/03/2008 (End)

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]


    REPLACE nStkVal    WITH nTotStk * nCost,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal,;
            llockflg   WITH lcTrType='8',;
            Reference  WITH lcRefer,;
            cAdjRef    WITH lcAdjRef,;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            dPostDate  WITH ldPostDate,;
            nUntCstBuy WITH EVALUATE(lcTmpJour+'.nUntCstBuy'),;
            cBusDocu   WITH lcBusDocu,;
            cStyType   WITH lcStyType
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
    =gfAdd_Info('itemjrnl')
    
    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfFabWarDy()

    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    lnLineNo = lnLineNo + 1
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    
  ENDSCAN
  
  IF !EMPTY(lcToWare)
    SELECT (lcTmpJour)
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *SCAN
    SCAN FOR cInvType+Style+cWareCode+cDyelot+cRSession+cISession = ;
           lcInvType+lcStyle+lcWareCode+lcDyelot
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
      SELECT itemjrnl
      APPEND BLANK
      llUptRcMat = .F.
      IF ASCAN(oAriaApplication.laEvntTrig,PADR("UPMATINV",10)) <> 0 .AND. lcCostMeth = 'L'
        llUptRcMat = .T.
        =gfDoTriger('MAINVCT',PADR('UPMATINV',10))
      ENDIF
      IF !llUptRcMat
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*        REPLACE cInvType   WITH lcInvType,;
                cRSession  WITH gfSEQUENCE('GLSession'),;
                cSession   WITH IIF(EMPTY(cISession),cRSession,cISession),;
                Style      WITH lcStyle,;
                cWareCode  WITH lcToWare,;
                cDyelot    WITH lcDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
                LineNo     WITH lnLineNo,;
                nCost      WITH EVALUATE(lcTmpJour+'.nCost')/lnConv,;
                cIRType    WITH 'R'

*        REPLACE nStk1      WITH ABS(EVALUATE(lcTmpJour+'.nApply1'))*lnConv,;
                nStk2      WITH ABS(EVALUATE(lcTmpJour+'.nApply2'))*lnConv,;
                nStk3      WITH ABS(EVALUATE(lcTmpJour+'.nApply3'))*lnConv,;
                nStk4      WITH ABS(EVALUATE(lcTmpJour+'.nApply4'))*lnConv,;
                nStk5      WITH ABS(EVALUATE(lcTmpJour+'.nApply5'))*lnConv,;
                nStk6      WITH ABS(EVALUATE(lcTmpJour+'.nApply6'))*lnConv,;
                nStk7      WITH ABS(EVALUATE(lcTmpJour+'.nApply7'))*lnConv,;
                nStk8      WITH ABS(EVALUATE(lcTmpJour+'.nApply8'))*lnConv,;
                nTotStk    WITH ABS(EVALUATE(lcTmpJour+'.nApply'))*lnConv

        REPLACE cInvType   WITH lcInvType,;
                cRSession  WITH lcFJlSess,;
                cSession   WITH IIF(EMPTY(cISession),cRSession,cISession),;
                Style      WITH lcStyle,;
                cWareCode  WITH lcToWare,;
                cDyelot    WITH lcDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
                LineNo     WITH lnLineNo,;
                nCost      WITH EVALUATE(lcTmpJour+'.nCost')/lnConv,;
                cIRType    WITH 'R'
                
        *B608635,1 WAM 07/28/2008 Don't multiply by conversion factor
        *REPLACE nStk1      WITH ABS(EVALUATE(lcTmpJour+'.nApply'))*lnConv,;
                nStk2      WITH ABS(EVALUATE(lcTmpJour+'.nApply2'))*lnConv,;
                nStk3      WITH ABS(EVALUATE(lcTmpJour+'.nApply3'))*lnConv,;
                nStk4      WITH ABS(EVALUATE(lcTmpJour+'.nApply4'))*lnConv,;
                nStk5      WITH ABS(EVALUATE(lcTmpJour+'.nApply5'))*lnConv,;
                nStk6      WITH ABS(EVALUATE(lcTmpJour+'.nApply6'))*lnConv,;
                nStk7      WITH ABS(EVALUATE(lcTmpJour+'.nApply7'))*lnConv,;
                nStk8      WITH ABS(EVALUATE(lcTmpJour+'.nApply8'))*lnConv,;
                nTotStk    WITH ABS(EVALUATE(lcTmpJour+'.nApply'))*lnConv
        REPLACE nStk1      WITH ABS(EVALUATE(lcTmpJour+'.nApply')),;
                nStk2      WITH ABS(EVALUATE(lcTmpJour+'.nApply2')),;
                nStk3      WITH ABS(EVALUATE(lcTmpJour+'.nApply3')),;
                nStk4      WITH ABS(EVALUATE(lcTmpJour+'.nApply4')),;
                nStk5      WITH ABS(EVALUATE(lcTmpJour+'.nApply5')),;
                nStk6      WITH ABS(EVALUATE(lcTmpJour+'.nApply6')),;
                nStk7      WITH ABS(EVALUATE(lcTmpJour+'.nApply7')),;
                nStk8      WITH ABS(EVALUATE(lcTmpJour+'.nApply8')),;
                nTotStk    WITH ABS(EVALUATE(lcTmpJour+'.nApply'))

        *B608635,1 WAM 07/28/2008 (End)

    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    
        REPLACE nStkVal    WITH nTotStk * nCost,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal,;
                llockflg   WITH lcTrType='8',;
                Reference  WITH lcRefer,;
                cAdjRef    WITH lcAdjRef,;
                cAdjReason WITH lcAdjCdRsn,;
                cAdjAcct   WITH lcAdjAcct,;
                dPostDate  WITH ldPostDate,;
                nUntCstBuy WITH EVALUATE(lcTmpJour+'.nUntCstBuy'),;
                cBusDocu   WITH lcBusDocu,;
                cStyType   WITH lcStyType
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
      =gfAdd_Info('itemjrnl')
      
      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfFabWarDy(.T.)
      *B608422,1 WAM 02/06/2008 Fix error while update material inventory transfer [T20080117.0001]
      lnLineNo = lnLineNo + 1
      *B608422,1 WAM 02/06/2008 (End)
    ENDSCAN
  ENDIF
ELSE
  SELECT itemjrnl
  APPEND BLANK
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*  REPLACE cInvType   WITH lcInvType,;
          cRSession  WITH IIF(lcTrType='9',lcCTRSess,IIF(lcTrType$'28',IIF(lnWOldStk<0,lcFJlSess,''),'')),;
          cISession  WITH IIF(lcTrType='9',lcCTISess,IIF(lcTrType$'28',IIF(lnWOldStk>0,lcFJlSess,''),lcFJlSess)),;
          cSession   WITH IIF(lcTrType$'28',lcFJlSess,cISession),;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH lcTrType,;
          cTrCode    WITH IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
          LineNo     WITH lnLineNo,;
          nCost      WITH IIF(lcTrType='1',lnCost,IIF(lcTrType $ '28',lnWOldCst,lnNewCost)),;
          cIRType    WITH lcIRType
  REPLACE cInvType   WITH lcInvType,;
          cRSession  WITH IIF(lcTrType='9',lcCTRSess,IIF(lcTrType$'28',IIF(lnWOldStk<0,lcFJlSess,''),'')),;
          cISession  WITH IIF(lcTrType='9',lcCTISess,IIF(lcTrType$'28',IIF(lnWOldStk>0,lcFJlSess,''),lcFJlSess)),;
          cSession   WITH IIF(lcTrType$'28',lcFJlSess,cISession),;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH lcTrType,;
          cTrCode    WITH IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
          LineNo     WITH lnLineNo,;
          nCost      WITH IIF(lcTrType='1',lnCost,IIF(lcTrType $ '28',lnWOldCst,lnNewCost)),;
          cIRType    WITH "I"
          
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
 
  REPLACE nStk1      WITH IIF(lcTrType$'28',laOldstk[1],laAdjStk[1]),;
          nStk2      WITH IIF(lcTrType$'28',laOldstk[2],laAdjStk[2]),;
          nStk3      WITH IIF(lcTrType$'28',laOldstk[3],laAdjStk[3]),;
          nStk4      WITH IIF(lcTrType$'28',laOldstk[4],laAdjStk[4]),;
          nStk5      WITH IIF(lcTrType$'28',laOldstk[5],laAdjStk[5]),;
          nStk6      WITH IIF(lcTrType$'28',laOldstk[6],laAdjStk[6]),;
          nStk7      WITH IIF(lcTrType$'28',laOldstk[7],laAdjStk[7]),;
          nStk8      WITH IIF(lcTrType$'28',laOldstk[8],laAdjStk[8]),;
          nTotStk    WITH IIF(lcTrType$'28',laOldstk[9],laAdjStk[9])
  REPLACE nStkVal    WITH nTotStk * nCost,;
          nPrvSQty   WITH lnPrvQty,;
          nPrvSVal   WITH lnPrvVal,;
          llockflg   WITH lcTrType='8',;
          Reference  WITH lcRefer,;
          cAdjRef    WITH lcAdjRef,;
          cAdjReason WITH lcAdjCdRsn,;
          cAdjAcct   WITH lcAdjAcct,;
          dPostDate  WITH ldPostDate,;
          nUntCstBuy WITH IIF(lcTrType='1',lnCost,IIF(lcTrType $ '28',lnWOldCst,lnNewCost)) * lnConv,;
          cBusDocu   WITH lcBusDocu,;
          cStyType   WITH lcStyType
  
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
  =gfAdd_Info('itemjrnl')
  
  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()
  
  *Call function to Update the Material inv. Journal with  issue 
  *Recored in case we lock the item.
  
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
  *IF lcTrType  = '8'
  IF lcIRType = 'I' AND lcTrType  = '8'
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    = lfRcvLock ()
  ENDIF
  
  IF !EMPTY(lcToWare)
    SELECT itemjrnl
    APPEND BLANK
    REPLACE cInvType   WITH lcInvType,;
            cRSession  WITH lcFJlSess,;
            cSession   WITH lcFJlSess,;
            Style      WITH lcStyle,;
            cWareCode  WITH lcToWare,;
            cDyelot    WITH lcDyelot,;
            dTrDate    WITH ldTrDate,;
            cTrType    WITH lcTrType,;
            cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            LineNo     WITH lnLineNo,;
            nCost      WITH lnNewCost,;
            cIRType    WITH "R"
    REPLACE nStk1      WITH ABS(laAdjStk[1]),;
            nStk2      WITH ABS(laAdjStk[2]),;
            nStk3      WITH ABS(laAdjStk[3]),;
            nStk4      WITH ABS(laAdjStk[4]),;
            nStk5      WITH ABS(laAdjStk[5]),;
            nStk6      WITH ABS(laAdjStk[6]),;
            nStk7      WITH ABS(laAdjStk[7]),;
            nStk8      WITH ABS(laAdjStk[8]),;
            nTotStk    WITH ABS(laAdjStk[9])
    REPLACE nStkVal    WITH nTotStk * nCost,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal,;
            llockflg   WITH lcTrType='8',;
            Reference  WITH lcRefer,;
            cAdjRef    WITH lcAdjRef,;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            dPostDate  WITH ldPostDate,;
            nUntCstBuy WITH lnNewCost * lnConv,;
            cBusDocu   WITH lcBusDocu,;
            cStyType   WITH lcStyType
    
    *Update the operation code and lot #
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
    =gfAdd_Info('itemjrnl')
    
    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfFabWarDy(.T.)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfReceive
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/12/2004
*! Purpose   : Update itemjrnl file for receive record
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
IF lcTrType $ '28'
  IF lfDoPhys('I')
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
  *  lcIRType = 'I'
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

    =lfIssue()
    
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
   * lcIRType = 'R'
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
  
    STORE 0 TO lnPrvQty,lnPrvVal
  ENDIF
ELSE
  IF lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    =lfAdjRec()
  ENDIF
ENDIF

IF !(lcTrType $ '28') OR (lcTrType $ '28' AND lfDoPhys('R'))
  *--Create a main record in journal file.
  SELECT itemjrnl
  
  *! B608792,1 MMT 02/04/2009 Fix bug of Error while update while returning fabric [Start]
  lnNewLineNo = 0
  IF SEEK(lcInvType+lcStyle+lcWareCode+IIF(lcTrType $ '349',lcCTRSess,lcFJlSess)+DTOs(ldTrDate)+;
  		  IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode)+STR(lnLineNo,6))
    SCAN REST WHILE CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
    	  lcInvType+lcStyle+lcWareCode+IIF(lcTrType $ '349',lcCTRSess,lcFJlSess)+DTOs(ldTrDate)+;
  		  IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode)+STR(lnLineNo,6) FOR CTRTYPE = lcTrType AND cIRType =lcIRType
      lnNewLineNo = lnNewLineNo + 1 		  
    ENDSCAN 		  
    lnNewLineNo = lnNewLineNo + 1 		      
  ENDIF   		  
  *! B608792,1 MMT 02/04/2009 Fix bug of Error while update while returning fabric [End]
  
  APPEND BLANK
  
  
  REPLACE cInvType   WITH lcInvType,;
          cRSession  WITH IIF(lcTrType $ '349',lcCTRSess,lcFJlSess),;
          cISession  WITH IIF(lcTrType $ '349',lcCTISess,''),;
          cSession   WITH cRSession,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH lcTrType,;
          cTrCode    WITH IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
          LineNo     WITH lnLineNo,;
          nCost      WITH lnNewCost,;
          cIRType    WITH lcIRType
  REPLACE nStk1      WITH laAdjStk[1] * IIF(lcTrType $ '34',lnConv,1),;
          nStk2      WITH laAdjStk[2] * IIF(lcTrType $ '34',lnConv,1),;
          nStk3      WITH laAdjStk[3] * IIF(lcTrType $ '34',lnConv,1),;
          nStk4      WITH laAdjStk[4] * IIF(lcTrType $ '34',lnConv,1),;
          nStk5      WITH laAdjStk[5] * IIF(lcTrType $ '34',lnConv,1),;
          nStk6      WITH laAdjStk[6] * IIF(lcTrType $ '34',lnConv,1),;
          nStk7      WITH laAdjStk[7] * IIF(lcTrType $ '34',lnConv,1),;
          nStk8      WITH laAdjStk[8] * IIF(lcTrType $ '34',lnConv,1),;
          nTotStk    WITH laAdjStk[9] * IIF(lcTrType $ '34',lnConv,1)
  REPLACE nStkVal    WITH nTotStk * nCost,;
          nPrvSQty   WITH lnPrvQty,;
          nPrvSVal   WITH lnPrvVal,;
          llockflg   WITH lcTrType='8',;
          Reference  WITH lcRefer,;
          cAdjRef    WITH lcAdjRef,;
          cAdjReason WITH lcAdjCdRsn,;
          cAdjAcct   WITH lcAdjAcct,;
          dPostDate  WITH ldPostDate,;
          nUntCstBuy WITH lnNewCost*lnConv,;
          cBusDocu   WITH lcBusDocu,;
          cStyType   WITH lcStyType
  
  *Update the operation code and lot #
  IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
    REPLACE cOprCode  WITH lcOprCode ,;
            cLotNo    WITH lcLotNo
  ENDIF

  *! B608792,1 MMT 02/04/2009 Fix bug of Error while update while returning fabric [Start]
  REPLACE NLINENO WITH lnNewLineNo 
  *! B608792,1 MMT 02/04/2009 Fix bug of Error while update while returning fabric [End]

  *C200255,1 (Begin) Update voucher no variable for CON10.
  IF ASCAN(oAriaApplication.laEvntTrig,PADR("GETVOUT",10)) <> 0
    REPLACE cvoucherno WITH lcVoucNo
  ENDIF   
  *C200255,1 (End)
    
  *-- Call global function to add audit fields info.
  =gfAdd_Info('itemjrnl')
  
  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()
  
  IF lcTrType = '8' AND ((!EMPTY(lcDyelot) AND llAdjYes) OR EMPTY(lcDyelot))
    =lfLkAdjRec()
  ENDIF
ENDIF

*:*************************************************************
*: Name       : lfRcvLock
*: Developer  : AHMED MAHER (AMH)
*: Date       : 08/12/2004
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

LOCAL lnAlias
lnAlias = SELECt (0)

IF lfDoPhys('I')
  *--Create a main record in journal file.
  SELECT itemjrnl
  APPEND BLANK
  REPLACE cInvType   WITH lcInvType,;
          cISession  WITH lcFJlSess,;
          cSession   WITH cISession,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH lcTrType,;
          cTrCode    WITH IIF(lcTrType $ "128" AND EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
          LineNo     WITH lnLineNo,;
          nCost      WITH lnNewCost,;
          cIRType    WITH "I"
  REPLACE nStk1      WITH laAdjStk[1],;
          nStk2      WITH laAdjStk[2],;
          nStk3      WITH laAdjStk[3],;
          nStk4      WITH laAdjStk[4],;
          nStk5      WITH laAdjStk[5],;
          nStk6      WITH laAdjStk[6],;
          nStk7      WITH laAdjStk[7],;
          nStk8      WITH laAdjStk[8],;
          nTotStk    WITH laAdjStk[9]
  REPLACE nStkVal    WITH nTotStk * nCost,;
          nPrvSQty   WITH lnPrvQty,;
          nPrvSVal   WITH lnPrvVal,;
          llockflg   WITH .T.,;
          Reference  WITH lcRefer,;
          cAdjRef    WITH lcAdjRef,;
          cAdjReason WITH lcAdjCdRsn,;
          cAdjAcct   WITH lcAdjAcct,;
          dPostDate  WITH ldPostDate,;
          nUntCstBuy WITH lnNewCost * lnConv,;
          cBusDocu   WITH lcBusDocu,;
          cStyType   WITH lcStyType
  
  IF TYPE('lcLotNo') # 'U' .AND. TYPE('lcOprCode') # 'U'
    REPLACE cOprCode  WITH lcOprCode ,;
            cLotNo    WITH lcLotNo
  ENDIF
  
  *-- Call global function to add audit fields info.
  =gfAdd_Info('itemjrnl')
  
  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfFabWarDy()
  
  IF (!EMPTY(lcDyelot) AND llAdjYes) OR EMPTY(lcDyelot)
    =lfLkAdjRec()
  ENDIF
ENDIF
SELECT(lnAlias)
*- End OF lfRcvLock.

*!*************************************************************
*! Name      : lfJrRoData
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2004
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


*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
IF !USED('ROLLS')
  =gfOpenTable(oAriaApplication.DataDir+'ROLLS',oAriaApplication.DataDir+'ROLLS','SH')
ENDIF 
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

*-- this Variable to know the calling program EX lcCalProg = 'ARMINV' ,
*-- If I call this function from the invoice Material Sales Order.
IF TYPE('lcCalProg') # 'C'
  lcCalProg = SPACE(0)
ENDIF

llGenRolId   = ('MA' $ oAriaApplication.CompanyInstalledModules AND ALLTRIM(gfGetMemVar('M_GENROLID')) = 'N')

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*IF laAdjStk[9] < 0 OR lcTrType = '2'
IF laAdjStk[9] < 0 OR lcTrType $ '28'
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

  PRIVATE lcItemJour,lcUseFile,llCollcData
  IF !llExtCall
    lcTmpJour = gfTempName() 
    lcUseFile = lcTmpJour
    llCollcData = .T.
  ELSE
    IF USED(lcTmpJour)
      llCollcData = !SEEK(lcInvType+lcStyle+lcWareCode+lcDyelot,lcTmpJour)
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
    LOCAL lnEngineBehavior
    lnEngineBehavior = SET("EngineBehavior")
    SET ENGINEBEHAVIOR 70
    IF lcTrType $ '34'
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      *SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
             cTrCode,cTrType,dTrDate,dPostDate,nCost*lnConv As 'nUnitCost',;
             SUM(nStk1) / lnConv AS 'nBal1',SUM(nStk2) / lnConv AS 'nBal2',;
             SUM(nStk3) / lnConv AS 'nBal3',SUM(nStk4) / lnConv AS 'nBal4',;
             SUM(nStk5) / lnConv AS 'nBal5',SUM(nStk6) / lnConv AS 'nBal6',;
             SUM(nStk7) / lnConv AS 'nBal7',SUM(nStk8) / lnConv AS 'nBal8',;
             SUM(nTotStk) / lnConv AS 'nBalance',;
             00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
             00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
             00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
             00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
             00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
             "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
      FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
      ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
      
      SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
             cTrCode,cTrType,dTrDate,dPostDate,nCost*lnConv As 'nCost',nUntCstBuy,;
             SUM(nStk1) / lnConv AS 'nBal1',SUM(nStk2) / lnConv AS 'nBal2',;
             SUM(nStk3) / lnConv AS 'nBal3',SUM(nStk4) / lnConv AS 'nBal4',;
             SUM(nStk5) / lnConv AS 'nBal5',SUM(nStk6) / lnConv AS 'nBal6',;
             SUM(nStk7) / lnConv AS 'nBal7',SUM(nStk8) / lnConv AS 'nBal8',;
             SUM(nTotStk) / lnConv AS 'nBalance',;
             SUM(IIF(CIRTYPE='R',nTotStk,0))/lnConv AS 'nReceived',;
             SUM(IIF(CIRTYPE='I',nTotStk,0))/lnConv AS 'nIssued',;
             00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
             00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
             00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
             00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
             00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
             "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
      FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
      ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    ELSE
      IF lcTrType $ '56'
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]  
       * SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
               cTrCode,cTrType,dTrDate,dPostDate,nCost*lnConv As 'nUnitCost',;
               SUM(nStk1) / lnConv AS 'nBal1',SUM(nStk2) / lnConv AS 'nBal2',;
               SUM(nStk3) / lnConv AS 'nBal3',SUM(nStk4) / lnConv AS 'nBal4',;
               SUM(nStk5) / lnConv AS 'nBal5',SUM(nStk6) / lnConv AS 'nBal6',;
               SUM(nStk7) / lnConv AS 'nBal7',SUM(nStk8) / lnConv AS 'nBal8',;
               SUM(nTotStk) / lnConv AS 'nBalance',;
               00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
               00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
               00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
               00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
               00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
        FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
        ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
        SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
               cTrCode,cTrType,dTrDate,dPostDate,nCost*lnConv As 'nCost',nUntCstBuy,;
               SUM(nStk1) / lnConv AS 'nBal1',SUM(nStk2) / lnConv AS 'nBal2',;
               SUM(nStk3) / lnConv AS 'nBal3',SUM(nStk4) / lnConv AS 'nBal4',;
               SUM(nStk5) / lnConv AS 'nBal5',SUM(nStk6) / lnConv AS 'nBal6',;
               SUM(nStk7) / lnConv AS 'nBal7',SUM(nStk8) / lnConv AS 'nBal8',;
               SUM(nTotStk) / lnConv AS 'nBalance',;
               SUM(IIF(CIRTYPE='R',nTotStk,0))/lnConv AS 'nReceived',;
               SUM(IIF(CIRTYPE='I',nTotStk,0))/lnConv AS 'nIssued',;
               00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
               00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
               00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
               00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
               00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
        FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
        ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
        *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
      ELSE
      
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      *  SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
               cTrCode,cTrType,dTrDate,dPostDate,nCost As 'nUnitCost',;
               SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',;
               SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',;
               SUM(nStk5) AS 'nBal5',SUM(nStk6) AS 'nBal6',;
               SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8',;
               SUM(nTotStk) AS 'nBalance',;
               00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
               00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
               00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
               00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
               00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
        FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
        ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
        SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
               cTrCode,cTrType,dTrDate,dPostDate,nCost As 'nCost',nUntCstBuy,;
               SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',;
               SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',;
               SUM(nStk5) AS 'nBal5',SUM(nStk6) AS 'nBal6',;
               SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8',;
               SUM(nTotStk) AS 'nBalance',;
               SUM(IIF(CIRTYPE='R',nTotStk,0))/lnConv AS 'nReceived',;
               SUM(IIF(CIRTYPE='I',nTotStk,0))/lnConv AS 'nIssued',;
               00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
               00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
               00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
               00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
               00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
        FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
        ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
      ENDIF
    ENDIF
    SET ENGINEBEHAVIOR lnEngineBehavior
    
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
      INDEX ON cInvType+Style+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
    ELSE
      INDEX ON cInvType+Style+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
    ENDIF
  ENDIF
  GO TOP
  *-- This check is only if you issuing
  IF EOF() AND lcTrType <> '2' AND laAdjStk[9] < 0
    *--No open receiving exist for material/color XXXX/yyyyy ,
    *--This transaction line will be ignored.
    lcMsgExp = ALLTRIM(gfItemMask("HI","",lcInvType)) +" "+ ALLTRIM(lcStyle)
    =gfModalGen('TRM42261B00036','DIALOG',lcMsgExp)
    USE
    RETURN .F.
  ENDIF
  
  IF lcTrType # '2' .AND. lcCostMeth $ "FI"  &&FIFO OR LIFO
    *--Start checking the only needed open receinving transaction for this
    *--issue transaction and put zero for all not needed receivings.
    
    *--Variable to Hold the accomulation of the receiving untill it cover
    *--the issue quantity needed.
    DECLARE laRcvdQty[9]
    STORE 0 TO laRcvdQty
    SCAN WHILE laRcvdQty[9] <> ABS(laAdjStk[9]) OR EOF()
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        IF laRcvdQty[lnI] < ABS(laAdjStk[lnI])
          lnUpdate = MIN(EVALUATE('nBal'+lcI) , ABS(laAdjStk[lnI]+laRcvdQty[lnI]))
          REPLACE ('nApply'+lcI) WITH EVALUATE('nApply'+lcI) + lnUpdate,;
                  nApply         WITH nApply + lnUpdate ,;
                  lNeeded        WITH .T.
          laRcvdQty[lnI] = laRcvdQty[lnI] + lnUpdate
          laRcvdQty[9]   = laRcvdQty + lnUpdate
        ENDIF
      ENDFOR
    ENDSCAN
    *--Check if all Issue quantity are covered by the receivings.
    IF laRcvdQty[9] < ABS(laAdjStk[9])
      *--The receiving quantity are not covered the issued quantity
      *--for material/color XXXX , This transaction line will be ignored.
      lcMsgExp = ALLTRIM(gfItemMask("HI","",lcInvType)) +" "+ ALLTRIM(lcStyle)
      =gfModalGen('TRM42262B00036','DIALOG',lcMsgExp)
      USE
      RETURN .F. 
    ENDIF
  ELSE
    *-- it is lot
    *call lot rol SCREEN only if trans. type <> 2
    IF (lcTrType # '2' OR (llTrkRolls .AND. item.ltrkrolls)) AND !lfLotRolScr()
      RETURN .F.
    ENDIF
  ENDIF
ELSE    && receiving
  IF llTrkRolls .AND. item.ltrkrolls
    IF !lfLotRolScr()
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

IF laAdjStk[9] < 0
  *--Delete all not needed receiving transactions for Journal file
  SELECT(lcTmpJour)
  SCAN WHILE cInvType+Style+cWareCode+cDyelot+cRSession+cISession =;
             lcInvType+lcStyle+lcWareCode+lcDyelot
    IF lNeeded
      *--Change it to Issue transactions,to use it in updating master Journal file.
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      *REPLACE dTranDate WITH ldTrDate  ,;
              dPostDate WITH ldPostDate,;
              cTranType WITH lcTrType  ,;
              cTran     WITH lcTrCode  ,;
              nApply1   WITH nApply1,;
              nApply2   WITH nApply2,;
              nApply3   WITH nApply3,;
              nApply4   WITH nApply4,;
              nApply5   WITH nApply5,;
              nApply6   WITH nApply6,;
              nApply7   WITH nApply7,;
              nApply8   WITH nApply8,;
              nApply    WITH nApply
      REPLACE dTrDate   WITH ldTrDate  ,;
              dPostDate WITH ldPostDate,;
              cTrType   WITH lcTrType  ,;
              cTrCode   WITH lcTrCode  ,;
              nApply1   WITH nApply1,;
              nApply2   WITH nApply2,;
              nApply3   WITH nApply3,;
              nApply4   WITH nApply4,;
              nApply5   WITH nApply5,;
              nApply6   WITH nApply6,;
              nApply7   WITH nApply7,;
              nApply8   WITH nApply8,;
              nApply    WITH nApply
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    ELSE
      DELETE
    ENDIF
  ENDSCAN
ENDIF


*--Delete all not needed receiving transactions for Roll file
IF llTrkRolls .AND. item.ltrkrolls
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
     
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
      * REPLACE dTranDate WITH ldTrDate  ,;
             dPostDate WITH ldPostDate,;
              cTranType WITH lcTrType  ,;
              cTran     WITH lcTrCode  ,;
              nApply    WITH nApply

      REPLACE dTranDate WITH ldTrDate  ,;
              dPostDate WITH ldPostDate,;
              cTranType WITH lcTrType  ,;
              cTran     WITH lcTrCode  ,;
              nApply    WITH nApply
     *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
     
    ELSE
      DELETE
    ENDIF
  ENDSCAN
  SET ORDER TO (lcTag) IN (lcTmpRoll)
  
  IF lnAdjStk < 0
    SELECT(lcFullRoll)
    PRIVATE lcTag
    lcTag = ORDER()
    
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *SET ORDER TO lcFullRoll
    SET ORDER TO 'lcFullRol2'
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    
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

SELECT itemjrnl
IF llExtCall
  RETURN (laAdjStk[9])
ENDIF

RETURN .T.

*!***********************************************************************
*! Name      : lfAdjRec
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/15/2004
*! Purpose   : Add Rec. record and Iss. record in itemjrnl table.
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfAdjRec()
*!***********************************************************************
FUNCTION lfAdjRec

*B126063,1 AMH Calculate the correct Ave. cost and Qty. for this adj. [Start]
LOCAL lnStk1,lnStk2,lnStk3,lnStk4,lnStk5,lnStk6,lnStk7,lnStk8,lnTotStk,lnStkVal,lnAveCost,lnRecNo
STORE 0 TO lnStk1,lnStk2,lnStk3,lnStk4,lnStk5,lnStk6,lnStk7,lnStk8,lnTotStk,lnStkVal,lnAveCost
SELECT itemjrnl
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[Start]
*lnRecNo = RECNO()
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[END]
SET ORDER TO StyInvJl DESCENDING
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[Start]
lcSeekKeyValue = EVALUATE(KEY())
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[END]
LOCATE
SCAN REST
  IF nTotStk < 0
    lnStk1 = lnStk1 - nStk1
    lnStk2 = lnStk2 - nStk2
    lnStk3 = lnStk3 - nStk3
    lnStk4 = lnStk4 - nStk4
    lnStk5 = lnStk5 - nStk5
    lnStk6 = lnStk6 - nStk6
    lnStk7 = lnStk7 - nStk7
    lnStk8 = lnStk8 - nStk8
  ELSE
    lnTotStk = lnTotStk + nTotStk
    lnStkVal = lnStkVal + nStkVal
  ENDIF
  
  IF nPrvsQty > 0
    lnTotStk = lnTotStk + nPrvsQty
    lnStkVal = lnStkVal + nPrvsVal
    EXIT
  ENDIF
ENDSCAN
lnAveCost = ((lnStkVal+(laAdjStk[9]*lnNewCost))/(lnTotStk+laAdjStk[9]))
SET ORDER TO StyInvJl ASCENDING
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[Start]
*GO lnRecNo
=SEEK(lcSeekKeyValue)
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[END]

*B126063,1 AMH [End]

*-- Rec. with the old cost
SELECT itemjrnl
APPEND BLANK

*B126063,1 AMH Update itemjrnl table with the correct qty. [Start]
*REPLACE cSession   WITH IIF(lcTrType = '9',lcCTRSess,lcFJlSess),;
        cInvType   WITH lcInvType,;
        Style      WITH lcStyle,;
        cWareCode  WITH lcWareCode,;
        cDyelot    WITH lcDyelot,;
        dTrDate    WITH ldTrDate,;
        dPostDate  WITH ldPostDate,;
        cTrType    WITH '1',;
        cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
        nCost      WITH lnWOldCst,;
        nUntCstBuy WITH lnWOldCst*lnConv,;
        cIRType    WITH "R",;
        nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[1],laAdjStk[1]),;
        nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[2],laAdjStk[2]),;
        nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[3],laAdjStk[3]),;
        nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[4],laAdjStk[4]),;
        nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[5],laAdjStk[5]),;
        nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[6],laAdjStk[6]),;
        nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[7],laAdjStk[7]),;
        nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[8],laAdjStk[8]),;
        nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
        nStkVal    WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-lnWStkVal,nTotStk * lnWOldCst),;
        Reference  WITH "Auto cost adj. " + cTrCode ,;
        cAdjReason WITH lcAdjCdRsn ,;
        cAdjAcct   WITH lcAdjAcct  ,;
        cRSession  WITH cSession   ,;
        nPrvSQty   WITH lnPrvQty   ,;
        nPrvSVal   WITH lnPrvVal   ,;
        cAdjRef    WITH lcAdjRef
REPLACE cSession   WITH IIF(lcTrType = '9',lcCTRSess,lcFJlSess),;
        cInvType   WITH lcInvType,;
        Style      WITH lcStyle,;
        cWareCode  WITH lcWareCode,;
        cDyelot    WITH lcDyelot,;
        dTrDate    WITH ldTrDate,;
        dPostDate  WITH ldPostDate,;
        cTrType    WITH '1',;
        cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
        nCost      WITH lnWOldCst,;
        nUntCstBuy WITH lnWOldCst*lnConv,;
        cIRType    WITH "R",;
        nStk1      WITH lnStk1,;
        nStk2      WITH lnStk2,;
        nStk3      WITH lnStk3,;
        nStk4      WITH lnStk4,;
        nStk5      WITH lnStk5,;
        nStk6      WITH lnStk6,;
        nStk7      WITH lnStk7,;
        nStk8      WITH lnStk8,;
        nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
        nStkVal    WITH nTotStk * lnWOldCst,;
        Reference  WITH "Auto cost adj. " + cTrCode ,;
        cAdjReason WITH lcAdjCdRsn ,;
        cAdjAcct   WITH lcAdjAcct  ,;
        cRSession  WITH cSession   ,;
        nPrvSQty   WITH lnPrvQty   ,;
        nPrvSVal   WITH lnPrvVal   ,;
        cAdjRef    WITH lcAdjRef   ,;
        LineNo     WITH lnLineNo
lnLineNo = lnLineNo + 1
*B126063,1 AMH [End]

*-- Call global function to add audit fields info.
=gfAdd_Info('itemjrnl')

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfFabWarDy()

*-- Iss. with the new cost
SELECT itemjrnl
APPEND BLANK

*B126063,1 AMH Update itemjrnl table with the correct qty. [Start]
*REPLACE cSession   WITH IIF(lcTrType = '9',lcCTRSess,lcFJlSess),;
        cInvType   WITH lcInvType,;
        Style      WITH lcStyle,;
        cWareCode  WITH lcWareCode,;
        cDyelot    WITH lcDyelot,;
        dTrDate    WITH ldTrDate,;
        dPostDate  WITH ldPostDate,;
        cTrType    WITH '1',;
        cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
        nCost      WITH lnNewCost,;
        nUntCstBuy WITH lnNewCost*lnConv,;
        cIRType    WITH "I",;
        nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[1],-laAdjStk[1]),;
        nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[2],-laAdjStk[2]),;
        nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[3],-laAdjStk[3]),;
        nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[4],-laAdjStk[4]),;
        nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[5],-laAdjStk[5]),;
        nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[6],-laAdjStk[6]),;
        nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[7],-laAdjStk[7]),;
        nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[8],-laAdjStk[8]),;
        nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
        nStkVal    WITH nTotStk * lnNewCost,;
        Reference  WITH "Auto cost adj. " + cTrCode ,;
        cAdjReason WITH lcAdjCdRsn ,;
        cAdjAcct   WITH lcAdjAcct  ,;
        cISession  WITH cSession   ,;
        nPrvSQty   WITH lnPrvQty   ,;
        nPrvSVal   WITH lnPrvVal   ,;
        cAdjRef    WITH lcAdjRef
REPLACE cSession   WITH IIF(lcTrType = '9',lcCTRSess,lcFJlSess),;
        cInvType   WITH lcInvType,;
        Style      WITH lcStyle,;
        cWareCode  WITH lcWareCode,;
        cDyelot    WITH lcDyelot,;
        dTrDate    WITH ldTrDate,;
        dPostDate  WITH ldPostDate,;
        cTrType    WITH '1',;
        cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
        nCost      WITH lnAveCost,;
        nUntCstBuy WITH lnAveCost*lnConv,;
        cIRType    WITH "I",;
        nStk1      WITH -lnStk1,;
        nStk2      WITH -lnStk2,;
        nStk3      WITH -lnStk3,;
        nStk4      WITH -lnStk4,;
        nStk5      WITH -lnStk5,;
        nStk6      WITH -lnStk6,;
        nStk7      WITH -lnStk7,;
        nStk8      WITH -lnStk8,;
        nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
        nStkVal    WITH nTotStk * lnAveCost,;
        Reference  WITH "Auto cost adj. " + cTrCode ,;
        cAdjReason WITH lcAdjCdRsn ,;
        cAdjAcct   WITH lcAdjAcct  ,;
        cISession  WITH cSession   ,;
        nPrvSQty   WITH lnPrvQty   ,;
        nPrvSVal   WITH lnPrvVal   ,;
        cAdjRef    WITH lcAdjRef   ,;
        LineNo     WITH lnLineNo
lnLineNo = lnLineNo + 1
*B126063,1 AMH [End]

*-- Call global function to add audit fields info.
=gfAdd_Info('itemjrnl')

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfFabWarDy()

*!*************************************************************
*! Name      : lfUpdGLDist()
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/15/2004
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

LOCAL lnCurAlias

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
    laGLInvAry[lnAln,5] = ItemJrnl.cTrCode
    lnGLEnAmount = ItemJrnl.nStkVal * laGLInvAry[lnAln,3]
    DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2],;
                   lnGLEnAmount,; 
                   laGLInvAry[lnAln,4],laGLInvAry[lnAln,5],;
                   IIF(llLockAdj,ldPostDate,laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7],;
                   laGLInvAry[lnAln,8],laGLInvAry[lnAln,9],;
                   laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
                   laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
    lnCurAlias = SELECT(0)
    SELECT (laGLInvAry[lnAln,9])
    REPLACE glSession WITH lcFJlSess
    SELECT itemjrnl
    
    DO CASE
      CASE EVALUATE(laGLInvAry[lnAln,9]+'.Catg_Key') $ "015|006"
        REPLACE cIcAcnt WITH EVALUATE(laGLInvAry[lnAln,9]+'.GLAccount')
      CASE EVALUATE(laGLInvAry[lnAln,9]+'.Catg_Key') = "013" OR EMPTY(cAdjAcct)
        REPLACE cAdjAcct WITH EVALUATE(laGLInvAry[lnAln,9]+'.GLAccount')
    ENDCASE
    
    SELECT (lnCurAlias)
  ENDFOR
ELSE
*-- This means it is Adj. Record 
*--Update Gl for Receiving inventory record and -ve old Stock.
*- Receiving Trans.(+1,+2,+3)
  lcLinkCode = IIF(EMPTY(itemloc.GL_Link),item.Link_Code,itemloc.GL_Link)
  DO GLDIST WITH lcLinkCode,IIF(lcInvType='0001','006','015'),itemjrnl.nStkVal,IIF(lcInvType='0001','I','M')+'A',;
                 itemjrnl.cTrCode,itemjrnl.dTrDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  lnCurAlias = SELECT(0)
  SELECT itemjrnl
  REPLACE cIcAcnt WITH EVALUATE(laGLInvAry[1,9]+'.GLAccount')
  SELECT (lnCurAlias)
  
  *--Update Uncomplete session Step.
  DO GLDIST WITH lcLinkCode,IIF(lcInvType='0001','007','016'),-itemjrnl.nStkVal,IIF(lcInvType='0001','I','M')+'A',;
                 itemjrnl.cTrCode,itemjrnl.dTrDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  IF EMPTY(itemjrnl.cAdjAcct)
    lnCurAlias = SELECT(0)
    SELECT itemjrnl
    REPLACE cAdjAcct WITH EVALUATE(laGLInvAry[1,9]+'.GLAccount')
    SELECT (lnCurAlias)
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lfLotRolScr()
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/15/2004
*! Purpose   : Call lot or roll screen
*!*************************************************************
*! Example   : =lfLotRolScr()
*!*************************************************************

FUNCTION lfLotRolScr

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*PRIVATE lcChck,lcUnChck,lnTotApply,lnUsrApply,lcRNewSta,lcRRemSta,lcRModSta,llRetVal,;
        lnOldVal,lnNewRec,lcFileToUse,llBrowse,lcSerchExp
PRIVATE lcChck,lcUnChck,lnTotApply,lnUsrApply,lcRNewSta,lcRRemSta,lcRModSta,llRetVal,;
        lnOldVal,lnNewRec,lcFileToUse,llBrowse,lcSerchExp,lcFab,lcClr,lcWare,lcDye

lcFab     = lcFabric
lcClr     = lcColor
lcWare    = lcWareCode
lcDye     = lcFDyelot
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

STORE SPACE(0) To lcSerchExp
llBrowse = .F.

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*IF llTrkRolls .AND. Fabric.ltrkrolls
IF llTrkRolls .AND. ITEM.ltrkrolls
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]


  *-- "!llExtCall" If it's called from the function (gfMatCrl) OR 
  *-- "!USED(lcTmpRoll" it's called from receiving material manufacturing order OR
  *-- "lcTrType = '1' AND lnAdjStk < 0" it's called from issue Material PO return
  *MMT
  *IF (llExtCall AND (!USED(lcTmpRoll) OR (lcTrType $ '15' AND lnAdjStk < 0)) )  OR !llExtCall
  IF (llExtCall AND (!USED(lcTmpRoll) OR (lcTrType $ '345' AND lnAdjStk < 0)) )  OR !llExtCall
  *MMT
    IF EMPTY(lcToWare) OR lcToWare # lcWareCode
      = lfTmpRoll()
    ENDIF
  ENDIF
ENDIF

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*lcFileToUse = IIF(llTrkRolls .AND. Fabric.ltrkrolls,lcTmpRoll,lcTmpJour)
lcFileToUse = IIF(llTrkRolls .AND. ITEM.ltrkrolls,lcTmpRoll,lcTmpJour)
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

lnNewRec = RECNO(lcFileToUse)
llRetVal = .T.
STORE 0 TO lnOldVal

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*STORE IIF(lcTrType='2',lnTmpOStk,0) TO lnUsrApply
STORE IIF(lcTrType='2',laOldStk[9],0) TO lnUsrApply
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

STORE laAdjStk[9] TO lnTotApply

IF TYPE('llExtCall') = 'L' AND llExtCall
  DIMENSION laUsrApply[1]
  laUsrApply = 0
  *Check if the case is roll then add (lineno to where condition[Start]
  
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*  IF llTrkRolls .AND. Fabric.ltrkrolls
  IF llTrkRolls .AND. ITEM.ltrkrolls
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

    SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
            lcFab+lcClr+lcWare+lcDye+STR(lnLineNo,6) ;
      INTO ARRAY laUsrApply
  ELSE
    SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cInvType+Style+cWareCode+cDyelot = ;
            lcInvType+lcStyle+lcWareCode+lcDyelot ;
      INTO ARRAY laUsrApply
  ENDIF
  STORE laUsrApply * IIF(lnAdjStk>0,1,-1) TO lnUsrApply
ENDIF

lcLotRo  = gfTempName()
lcLotRo1 = gfTempName()
lcLotRo2 = gfTempName()
lcLotRo3 = gfTempName()

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
llShowRol = .F.
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

DO CASE
  CASE laAdjStk[9] < 0

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*    IF llTrkRolls .AND. Fabric.ltrkrolls
    IF llTrkRolls .AND. ITEM.ltrkrolls
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

      llShowRol = .T.
      STORE 0 TO lnCurRolBa,lnAppRolQt
    ELSE
      STORE 0 TO lnCurLotBa,lnAppLotQt,lnNewLotBa
      llShowRol = .F.
    ENDIF
    
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*  CASE laAdjStk[9] >= 0 AND llTrkRolls .AND. Fabric.ltrkrolls    
  CASE laAdjStk[9] >= 0 AND llTrkRolls .AND. ITEM.ltrkrolls
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
  
    llShowRol = .T.
    STORE 0 TO lnCurRolBa,lnAppRolQt
ENDCASE

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
IF (llTrkRolls .AND. ITEM.ltrkrolls) OR lcCostMeth = 'L' 
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
  DO FORM (oAriaApplication.ScreenHome +'MALotRo.scx')
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
ENDIF 
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
laAdjStk[1] = lnAdjStk 
laAdjStk[9] = lnAdjStk 
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

SELECT(lcFileToUse)
SET FILTER TO
LOCAL lnTotRoll

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*IF (laAdjStk[9] < 0 ) AND llTrkRolls .AND. Fabric.ltrkrolls
IF (laAdjStk[9] < 0 ) AND llTrkRolls .AND. ITEM.ltrkrolls
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]

  SELECT (lcTmpRoll)
  PRIVATE lcTag
  lcTag = ORDER()
  SET ORDER TO lcTmpRoll2
  SCAN FOR cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcFabric+lcColor+lcWareCode+lcFDyelot
    lnTotRoll = nApply
    SELECT (lcFullRoll)
    IF SEEK(&lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
            &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot,lcFullRoll,lcFullRoll)
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
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
    *lcScanExpr = cFabric+cColor+cWareCode+cDyelot
    lcScanExpr =padr(substr(Style,1,lnmajlength),7)+RIGHT(Style,lenclrlen)+cWareCode+cDyelot
    *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
    *-- I will update the nApply Qty With 0 to Void duplicated 
    *-- value if we issue the same fabric color in the same session.
    REPLACE nApply WITH 0

    *-- Scan On the Full Roll File and get the total Qty fro this session.
    IF SEEK(lcScanExpr,lcFullRoll,'lcFullRol2')
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
*! Name      : lfTmpRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/15/2004
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

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*  laFileStru[13,1] = 'nUnitCost'
  laFileStru[13,1] = 'nCost'
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
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

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]                                        
*IF lcTrType$'1235'
IF lcTrType$'12345'
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]                                        

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

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]                                        
*!*    SELECT MatInvJl
*!*    lcJorTag = ORDER()
*!*    SET ORDER TO MatInvJl
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]                                        
  
  *--cfabric+ccolor+cwarecode+cdyelot+crsession+cisession+STR(RECNO(),7)
  
  SELECT Rolls

*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]                                        
*!*    lcRollRela = SET('RELATION')
*!*    SET RELATION TO cRollItem+Color+cWareCode+Dyelot+cRSession+cISession INTO MatInvJl
* IF (TYPE('llExtCall') = 'U' OR !llExtCall) OR (llExtCall AND lcTrType $ '15' AND lnAdjStk < 0)
  IF (TYPE('llExtCall') = 'U' OR !llExtCall) OR (llExtCall AND lcTrType $ '345' AND lnAdjStk < 0)
  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]                                        
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
      *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]                                        
      *  SCAN WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
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
          
          SELECT ITEMJRNL
          LOCATE FOR Style+CWARECODE+CDYELOT+CRSESSION+CISESSION = PADR(Rolls.cRollItem,lnmajlength)+lcSepar+PADR(Rolls.Color,lenclrlen)+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession+Rolls.cISession
          *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]                                        
          
          IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot,lcTmpRoll)
          
          
            IF Rolls.TranCd = '1'
              SELECT (lcTmpRoll)
              APPEND BLANK
              *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]                                        
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
                      nUnitCost  WITH IIF(lcTrType = '5',(MatInvJl.nUnitCost*lnConv),;
                                      MatInvJl.nUnitCost),;
                      nUntCstBuy WITH MatInvJl.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH IIF(lcTrType$'15',Rolls.nQtyBal/IIF(lcTrType='1',;
                                          lnConv,lnConv),Rolls.nQtyBal) ,;
                      nApply     WITH IIF(lcTrType='3' OR (lcTrType='2' AND lnAdjStk > 0),nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType$'15' AND lnAdjStk<0,'2','3'),;
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
                      dTranDate  WITH ITEMJRNL.DTRDATE           ,;
                      dPostDate  WITH ITEMJRNL.dPostDate           ,;
                      nCost      WITH IIF(lcTrType = '34',(ITEMJRNL.NCOST*lnConv),;
                                      ITEMJRNL.NCOST),;
                      nUntCstBuy WITH ITEMJRNL.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH IIF(lcTrType$'345',Rolls.nQtyBal/IIF(lcTrType='5',;
                                          lnConv,lnConv),Rolls.nQtyBal) ,;
                      nApply     WITH IIF(lcTrType='2' OR (lcTrType='1' AND lnAdjStk > 0),nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType$'345' AND lnAdjStk<0,'2','3'),;
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='2'                 ,;
                      LineNo     WITH lnLineNo
                      
              *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]                                        
              IF llExtCall .AND. lcCalProg = 'ARMINV'
                REPLACE nIssue WITH 0        ,;
                        cMorder WITH lcTrCode
              ENDIF 
            ENDIF
          ENDIF
          SELECT (lcFullRoll)
          IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession,lcFullRoll,lcFullRoll)
            APPEND BLANK

            *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]                                        
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
                    RolTranCd  WITH IIF(lcTrType$'15' AND lnAdjStk<0,'2','3'),;
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
                    dTranDate  WITH ITEMJRNL.DTRDATE  ,;
                    dPostDate  WITH ITEMJRNL.dPostDate  ,;
                    nCost      WITH ITEMJRNL.NCOST  ,;
                    nUntCstBuy WITH ITEMJRNL.nUntCstBuy ,;
                    nReceived  WITH 0                   ,;
                    nIssued    WITH 0                   ,;
                    RolTranCd  WITH IIF(lcTrType$'345' AND lnAdjStk<0,'2','3'),;
                    lStatus    WITH "S"                 ,;
                    lNeeded    WITH lcTrType='2'        ,;
                    LineNo     WITH lnLineNo
                  *: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
          ENDIF
          REPLACE nBalance WITH nBalance+(IIF(Rolls.TranCd='2',-Rolls.nQty,Rolls.nQty)/IIF(lcTrType$'345',;
                                IIF(lcTrType ='5',lnConv,lnConv),1)),;
                  nApply   WITH IIF(lcTrType='2',nBalance,0)
          IF llExtCall .AND. lcCalProg = 'ARMINV'
            REPLACE cMorder WITH lcTrCode
          ENDIF

        ENDSCAN
      ENDIF
    ENDIF
  ENDIF

  SELECT Rolls
  SET ORDER TO lcRolTag
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[Start]
*!*    SELECT MatInvJl
*!*    SET ORDER TO lcJorTag
*!*    SELECT Rolls
*!*    SET RELATION TO &lcRollRela
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP[End]
ENDIF

*!***********************************************************************
*! Name      : lfFabWarDy
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/15/2004
*! Purpose   : Update the item and itemloc tables.
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfFabWarDy()
*!***********************************************************************
FUNCTION lfFabWarDy
LPARAMETERS llToWare

LOCAL lnCurAlias,lnItemStkVal,lnItemTotStk
lnCurAlias = SELECT(0)

*B125939,1 AMH Update the itemloc first then item table [Start]
*--1 ) Update Stock and Avarege cost in Item table. ------------
*SELECT itemloc
*SUM FOR Dyelot = SPACE(10) nStkVal,TotStk TO lnItemStkVal,lnItemTotStk

*SELECT item
*REPLACE nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
        Ave_Cost   WITH IIF(lcIRType='I' AND !llToWare,Ave_Cost,IIF(lnItemTotStk=0,itemjrnl.nCost,lnItemStkVal/lnItemTotStk)),;
        nAveCstBuy WITH Ave_Cost * lnConv
*B125939,1 AMH [End]

*--1 ) Update Stock and Avarege cost in Item Dyelot table Warehouse record.
SELECT itemloc
=SEEK(lcInvType+lcStyle+IIF(llToWare,lcToWare,lcWareCode)+SPACE(10))

*B126063,1 AMH Update the average cost correctly [Start]
*REPLACE Stk1       WITH Stk1 + IIF(llUInvtry,itemjrnl.nStk1,0),;
        Stk2       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk3       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk4       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk5       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk6       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk7       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk8       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        TotStk     WITH Stk1 + Stk2 + Stk3 + Stk4 + Stk5 + Stk6 + Stk7 + Stk8,;
        nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
        Ave_Cost   WITH IIF(lcIRType='I',Ave_Cost,IIF(TotStk=0,itemjrnl.nCost,nStkVal/TotStk)),;
        nAveCstBuy WITH Ave_Cost * lnConv
        
*! B609812,1 MMT 02/01/2012 Material Average cost is -ve in Item and Itemloc tables[T20120124.0019][START]        
*!*	REPLACE Stk1       WITH Stk1 + IIF(llUInvtry,itemjrnl.nStk1,0),;
*!*	        Stk2       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	        Stk3       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	        Stk4       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	        Stk5       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	        Stk6       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	        Stk7       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	        Stk8       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	        TotStk     WITH Stk1 + Stk2 + Stk3 + Stk4 + Stk5 + Stk6 + Stk7 + Stk8,;
*!*	        nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
*!*	        Ave_Cost   WITH IIF(TotStk=0,IIF(lcIRType='I',Ave_Cost,itemjrnl.nCost),nStkVal/TotStk),;
*!*	        nAveCstBuy WITH Ave_Cost * lnConv
REPLACE Stk1       WITH Stk1 + IIF(llUInvtry,itemjrnl.nStk1,0),;
        Stk2       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk3       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk4       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk5       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk6       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk7       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        Stk8       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
        TotStk     WITH Stk1 + Stk2 + Stk3 + Stk4 + Stk5 + Stk6 + Stk7 + Stk8,;
        nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
        Ave_Cost   WITH IIF(TotStk=0,IIF(lcIRType='I',Ave_Cost,itemjrnl.nCost),ABS(nStkVal/TotStk)),;
        nAveCstBuy WITH Ave_Cost * lnConv
*! B609812,1 MMT 02/01/2012 Material Average cost is -ve in Item and Itemloc tables[T20120124.0019][END]                
*B126063,1 AMH [End]

*B607883,1 MMT 12/18/2006 Fix bug of History usage is update in ISSUE/RET[Start]
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[Start]
REPLACE NHUSAGE1   WITH NHUSAGE1 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk1,0),;
        NHUSAGE2   WITH NHUSAGE2 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk2,0),;
        NHUSAGE3   WITH NHUSAGE3 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk3,0),;
        NHUSAGE4   WITH NHUSAGE4 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk4,0),;
        NHUSAGE5   WITH NHUSAGE5 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk5,0),;
        NHUSAGE6   WITH NHUSAGE6 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk6,0),;
        NHUSAGE7   WITH NHUSAGE7 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk7,0),;
        NHUSAGE8   WITH NHUSAGE8 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk8,0),;
        NTOTHUSAGE WITH NHUSAGE1 + NHUSAGE2 + NHUSAGE3 + NHUSAGE4 + NHUSAGE5 + NHUSAGE6 + NHUSAGE7 + NHUSAGE8
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage[End]        
*B607883,1 MMT 12/18/2006 Fix bug of History usage is update in ISSUE/RET [End]

REPLACE NCUSAGE1   WITH NCUSAGE1 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk1,0),;
        NCUSAGE2   WITH NCUSAGE2 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk2,0),;
        NCUSAGE3   WITH NCUSAGE3 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk3,0),;
        NCUSAGE4   WITH NCUSAGE4 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk4,0),;
        NCUSAGE5   WITH NCUSAGE5 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk5,0),;
        NCUSAGE6   WITH NCUSAGE6 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk6,0),;
        NCUSAGE7   WITH NCUSAGE7 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk7,0),;
        NCUSAGE8   WITH NCUSAGE8 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk8,0),;
        NTOTCUSA   WITH NCUSAGE1 + NCUSAGE2 + NCUSAGE3 + NCUSAGE4 + NCUSAGE5 + NCUSAGE6 + NCUSAGE7 + NCUSAGE8
lnPrvQty  = TotStk
lnPrvVal  = nStkVal
lnDyeCost = IIF(itemloc.TotStk = 0,itemloc.Ave_Cost,itemloc.nStkVal/itemloc.TotStk)

*B125939,1 AMH Update the itemloc first then item table [Start]
*--2 ) Update Stock and Avarege cost in Item table. ------------
SELECT itemloc
SUM FOR Dyelot = SPACE(10) nStkVal,TotStk TO lnItemStkVal,lnItemTotStk

SELECT item

*B126063,1 AMH Update the average cost correctly [Start]
*REPLACE nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
        Ave_Cost   WITH IIF(lcIRType='I' AND !llToWare,Ave_Cost,IIF(lnItemTotStk=0,itemjrnl.nCost,lnItemStkVal/lnItemTotStk)),;
        nAveCstBuy WITH Ave_Cost * lnConv
*! B609812,1 MMT 02/01/2012 Material Average cost is -ve in Item and Itemloc tables[T20120124.0019][START]                
*!*	REPLACE nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
*!*	        Ave_Cost   WITH IIF(lnItemTotStk=0,IIF(lcIRType='I' AND !llToWare,Ave_Cost,itemjrnl.nCost),lnItemStkVal/lnItemTotStk),;
*!*	        nAveCstBuy WITH Ave_Cost * lnConv
REPLACE nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
        Ave_Cost   WITH IIF(lnItemTotStk=0,IIF(lcIRType='I' AND !llToWare,Ave_Cost,itemjrnl.nCost),ABS(lnItemStkVal/lnItemTotStk)),;
        nAveCstBuy WITH Ave_Cost * lnConv
*! B609812,1 MMT 02/01/2012 Material Average cost is -ve in Item and Itemloc tables[T20120124.0019][ENd]                
*B126063,1 AMH [End]

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
LOCAL lnTotStock
lnTotStock = ItemJrnl.nStk1 + ItemJrnl.nStk2 + ItemJrnl.nStk3 + ItemJrnl.nStk4 +;
             ItemJrnl.nStk5 + ItemJrnl.nStk6 + ItemJrnl.nStk7 + ItemJrnl.nStk8

*B607883,1 MMT 12/18/2006 Fix bug of History usage is update in ISSUE/RET[Start]
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [Start]
REPLACE TotStk     WITH TotStk     + IIF(llUInvtry, lnTotStock, 0),;
        NTOTCUSA   WITH NTOTCUSA   - IIF(llUInvtry AND lcTrType = '9', lnTotStock, 0),;
        NTOTHUSAGE WITH NTOTHUSAGE - IIF(llUInvtry AND lcTrType = '9', lnTotStock, 0)

*REPLACE TotStk     WITH TotStk     + IIF(llUInvtry, lnTotStock, 0),;
        NTOTCUSA   WITH NTOTCUSA   - IIF(llUInvtry AND lcTrType = '9', lnTotStock, 0)
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [End]        
*B607883,1 MMT 12/18/2006 Fix bug of History usage is update in ISSUE/RET[End]

*E039550,1 WSH 08/07/2005 [End]

*B125939,1 AMH [End]

*--3 )  Update Stock in Item Dyelot table Dyelot record. --------

*B131608,1 WSH 05/03/2006 Seek in ItemLoc..! [Start]
*IF !EMPTY(lcDyelot) AND SEEK(lcInvType+lcStyle+IIF(llToWare,lcToWare,lcWareCode)+lcDyelot)
IF !EMPTY(lcDyelot) AND SEEK(lcInvType+lcStyle+IIF(llToWare,lcToWare,lcWareCode)+lcDyelot, "ItemLoc")
*B131608,1 WSH 05/03/2006 [End]

  SELECT Itemloc
  
  *B126063,1 AMH Update the average cost correctly [Start]
  *REPLACE Stk1       WITH Stk1 + IIF(llUInvtry,itemjrnl.nStk1,0),;
          Stk2       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk3       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk4       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk5       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk6       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk7       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk8       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          TotStk     WITH Stk1 + Stk2 + Stk3 + Stk4 + Stk5 + Stk6 + Stk7 + Stk8,;
          nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
          Ave_Cost   WITH IIF(lcIRType='I',Ave_Cost,IIF(TotStk=0,itemjrnl.nCost,nStkVal/TotStk)),;
          nAveCstBuy WITH Ave_Cost * lnConv
*! B609812,1 MMT 02/01/2012 Material Average cost is -ve in Item and Itemloc tables[T20120124.0019][START]                          
*!*	  REPLACE Stk1       WITH Stk1 + IIF(llUInvtry,itemjrnl.nStk1,0),;
*!*	          Stk2       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	          Stk3       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	          Stk4       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	          Stk5       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	          Stk6       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	          Stk7       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	          Stk8       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
*!*	          TotStk     WITH Stk1 + Stk2 + Stk3 + Stk4 + Stk5 + Stk6 + Stk7 + Stk8,;
*!*	          nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
*!*	          Ave_Cost   WITH IIF(TotStk=0,IIF(lcIRType='I',Ave_Cost,itemjrnl.nCost),nStkVal/TotStk),;
*!*	          nAveCstBuy WITH Ave_Cost * lnConv
  REPLACE Stk1       WITH Stk1 + IIF(llUInvtry,itemjrnl.nStk1,0),;
          Stk2       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk3       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk4       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk5       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk6       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk7       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          Stk8       WITH Stk2 + IIF(llUInvtry,itemjrnl.nStk2,0),;
          TotStk     WITH Stk1 + Stk2 + Stk3 + Stk4 + Stk5 + Stk6 + Stk7 + Stk8,;
          nStkVal    WITH nStkVal + itemjrnl.nStkVal,;
          Ave_Cost   WITH IIF(TotStk=0,IIF(lcIRType='I',Ave_Cost,itemjrnl.nCost),ABS(nStkVal/TotStk)),;
          nAveCstBuy WITH Ave_Cost * lnConv
*! B609812,1 MMT 02/01/2012 Material Average cost is -ve in Item and Itemloc tables[T20120124.0019][End]                          
  *B126063,1 AMH [End]

*B607883,1 MMT 12/18/2006 Fix bug of History usage is update in ISSUE/RET[Start]
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [Start]
  REPLACE NHUSAGE1   WITH NHUSAGE1 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk1,0),;
          NHUSAGE2   WITH NHUSAGE2 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk2,0),;
          NHUSAGE3   WITH NHUSAGE3 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk3,0),;
          NHUSAGE4   WITH NHUSAGE4 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk4,0),;
          NHUSAGE5   WITH NHUSAGE5 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk5,0),;
          NHUSAGE6   WITH NHUSAGE6 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk6,0),;
          NHUSAGE7   WITH NHUSAGE7 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk7,0),;
          NHUSAGE8   WITH NHUSAGE8 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk8,0),;
          NTOTHUSAGE WITH NHUSAGE1 + NHUSAGE2 + NHUSAGE3 + NHUSAGE4 + NHUSAGE5 + NHUSAGE6 + NHUSAGE7 + NHUSAGE8
**: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [End]          
*B607883,1 MMT 12/18/2006 Fix bug of History usage is update in ISSUE/RET[End]

  REPLACE NCUSAGE1   WITH NCUSAGE1 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk1,0),;
          NCUSAGE2   WITH NCUSAGE2 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk2,0),;
          NCUSAGE3   WITH NCUSAGE3 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk3,0),;
          NCUSAGE4   WITH NCUSAGE4 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk4,0),;
          NCUSAGE5   WITH NCUSAGE5 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk5,0),;
          NCUSAGE6   WITH NCUSAGE6 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk6,0),;
          NCUSAGE7   WITH NCUSAGE7 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk7,0),;
          NCUSAGE8   WITH NCUSAGE8 - IIF(llUInvtry AND lcTrType='9',itemjrnl.nStk8,0),;
          NTOTCUSA   WITH NCUSAGE1 + NCUSAGE2 + NCUSAGE3 + NCUSAGE4 + NCUSAGE5 + NCUSAGE6 + NCUSAGE7 + NCUSAGE8
  
  lnPrvQty = TotStk
  lnPrvVal = totStk * lnDyeCost
ENDIF

SELECT(lnCurAlias)

*!***********************************************************************
*! Name      : lfDoPhys
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/15/2004
*! Purpose   : Check if we need to do Physical inventory with (issue/receive) record.
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfDoPhys()
*!***********************************************************************
FUNCTION lfDoPhys
LPARAMETERS lcRI

LOCAL lnRet
lnRet = .F.

DO CASE
  CASE lcRI = 'I'
    lnRet = lnOldSVal   # 0 OR ;
            laOldStk[1] # 0 OR laOldStk[2] # 0 OR laOldStk[3] # 0 OR laOldStk[4] # 0 OR ;
            laOldStk[5] # 0 OR laOldStk[6] # 0 OR laOldStk[7] # 0 OR laOldStk[8] # 0 OR ;
            lnWOldCst   # lnNewCost       OR      laOldStk[9] # laAdjStk[9]
  CASE lcRI = 'R'
    lnRet = !(lnOldSVal   # 0 OR ;
              laOldStk[1] # 0 OR laOldStk[2] # 0 OR laOldStk[3] # 0 OR laOldStk[4] # 0 OR ;
              laOldStk[5] # 0 OR laOldStk[6] # 0 OR laOldStk[7] # 0 OR laOldStk[8] # 0 OR ;
              lnWOldCst   # lnNewCost       OR      laOldStk[9] # laAdjStk[9]        ) OR ;
              laAdjStk[9] # 0               OR      lnWOldCst   # lnNewCost
ENDCASE

RETURN lnRet

*!***********************************************************************
*! Name      : lfLkAdjRec
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/15/2004
*! Purpose   : 
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfLkAdjRec()
*!***********************************************************************
FUNCTION lfLkAdjRec

LOCAL lnCurAlias,lnTotQty,lnTotVal,lnTranVal,lnDiffere
lnCurAlias = SELECT(0)

SELECT itemjrnl
lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0
LOCATE
IF !EOF()
  SCAN FOR cDyelot = lcDyelot AND !lLockFlg
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
    lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
  ENDSCAN
  
  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0
    SELECT itemjrnl
    APPEND BLANK
    *: N000548,1 MMT 08/20/2007 Convert locking Screen to Aria4XP[Start]
    *REPLACE cSession   WITH lcFJlSess,;
            cInvType   WITH lcInvType,;
            Style      WITH lcStyle,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcDyelot,;
            dTrDate    WITH laLockInfo[11],;
            dPostDate  WITH ldPostDate,;
            cTrType    WITH lcTrType,;
            cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSessn,lcTrCode),;
            cIRType    WITH IIF(lnDiffere<0,'I','R'),;
            nStkVal    WITH lnDiffere   ,;
            Reference  WITH 'Mark Down Adjustement Value',;
            cAdjReason WITH lcAdjCdRsn ,;
            cAdjAcct   WITH lcAdjAcct  ,;
            cISession  WITH IIF(lnDiffere<0,cSession,''),;
            cRSession  WITH IIF(lnDiffere>0,cSession,''),;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal,;
            cAdjRef    WITH lcAdjRef,;
            LineNo     WITH lnLineNo
    REPLACE cSession   WITH lcFJlSess,;
            cInvType   WITH lcInvType,;
            Style      WITH lcStyle,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcDyelot,;
            dTrDate    WITH laLockInfo[11],;
            dPostDate  WITH ldPostDate,;
            cTrType    WITH lcTrType,;
            cTrCode    WITH IIF(EMPTY(lcTrCode),lcFJlSess,lcTrCode),;
            cIRType    WITH IIF(lnDiffere<0,'I','R'),;
            nStkVal    WITH lnDiffere   ,;
            Reference  WITH 'Mark Down Adjustement Value',;
            cAdjReason WITH lcAdjCdRsn ,;
            cAdjAcct   WITH lcAdjAcct  ,;
            cISession  WITH IIF(lnDiffere<0,cSession,''),;
            cRSession  WITH IIF(lnDiffere>0,cSession,''),;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal,;
            cAdjRef    WITH lcAdjRef,;
            LineNo     WITH lnLineNo + 1
	*: N000548,1 MMT 08/20/2007 Convert locking Screen to Aria4XP[END]
    =lfUpdGLDist(.F.,.T.)
    
    *: N000548,1 MMT 08/20/2007 Convert locking Screen to Aria4XP[Start]
    *=lfStyWarDy()
    =lfFabWarDy()
    *: N000548,1 MMT 08/20/2007 Convert locking Screen to Aria4XP[END]
    
  ENDIF
ENDIF

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfInvJour
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/12/2004
*! Purpose   : This function will get the journal records of Invoice.
*!*************************************************************
*! Parameter : None.
*!*************************************************************
*! Returns   : None. 
*!*************************************************************
*! Example   :  =lfInvJour()
*!*************************************************************
FUNCTION lfInvJour

SELECT SPACE(6) AS cSession,Style,cWareCode,cDyelot,dTrDate,'4' AS cTrType,cTrCode,;
       nCost,'R' AS cIRType,SPACE(6) AS cRSession,SPACE(6) AS cISession,LineNo    ,;
       -nStk1 AS nStk1,-nStk2 AS nStk2,-nStk3 AS nStk3,-nStk4 AS nStk4            ,;
       -nStk5 AS nStk5,-nStk6 AS nStk6,-nStk7 AS nStk7,-nStk8 AS nStk8            ,;
       -nTotStk AS nTotStk,-nStkVal AS nStkVal                ;
FROM  itemjrnl                                                ;
WHERE ctrcode +coprcode+clotno  +ctrtype+style  +cwarecode  = ;
      lcTrCode+SPACE(6)+SPACE(2)+'3'    +lcStyle+lcWareCode   ;
AND   LineNo = lnLineNo                   ;
AND   Dyelot = lcDyelot                   ;
ORDER BY Style,cWareCode,cDyelot,LineNo   ;
INTO  DBF (oAriaApplication.WorkDir+lcInvJour)