*:**************************************************************************
*: Program file  : gfStyCrl.prg
*: Program desc. : Style Stock Control Function,
*:                 Style Average Cost Update and Inventory Journal Update.
*:        System : ARIA APPAREL SYSTEM 2.7
*:        Module : Inventory Control (IC).
*:     Developer : AHM - Ahmed Amer
*:          Date : 05/19/99
*:**************************************************************************
*: Calls Functions    : lfGetAveCst(), lfIsueCost(), lfIsuJlTr()
*:                      lfUpdGLDist(), lfUpdStep().
*:**************************************************************************
*: Passed Parameters  :
*:   lcTrType   =>  Transaction Type.
*:             '1' for Inventory Adjustments  (+/-) 
*:             '2' for Physical Inventorys    (+/-)
*:             '3' for Invoices               (-)
*:             '4' for Void Invoices          (+)
*:             '5' for Recive Cutting Tickets (+)
*:             '6' for Recive Purchase Orders (+/-) 
*:             '7' for Recive Returns         (+) 
*:             '8' for Void Credit Memos      (-)
*:             '9' for Inventory Markdown     (+/-)
*:   lcStyle    =>  Style.
*:   lcWareCode =>  Warehouse Code.
*:   lcSDyelot  =>  Dyelot ,pass empty if not a dyelot style or system
*:                  dyelot No, (if you want to add this dyelot you have to 
*:                  add it before calling this function).
*:   ldTrDate   =>  Transaction Date.
*:   lcTrCode   =>  Transaction Code as ex. invoice no.
*:   laAdjStk[9]=>  Issued or received Stock as an array per sizes and total.
*:   lnNewCost  =>  New receiving cost ,Pass it Zero if it is issue transaction
*:                  it will be calculated in this function except issue return P/o.
*:   lcRefer    =>  Transaction Reference.  
*:   lcRISessn  =>  Session number for Recieving or Issue transactions,
*:                  It will be genereted in this function if not passed or empty.
*:                  or pass it empty for useing defferent session number.
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
*:                   Put the array elements as follows :
*:              [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:              [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:              [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:              [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*:                  If no G/L was used pass this array as len 1 and .F.
*:                  Pass Category type '006' as a first array element.
*:**************************************************************************
*: Note    : Files ->Style,StyDye,Temprory Line file and 
*:           Temprorary G/L Distribution files must be
*:           opened before calling this function.
*:**************************************************************************
*: Update  : Style => (Style Record) Stk1->8 and TotStk and Ave_Cost fields.
*:           StyDye=> (Sty/War rec.) Stk1->8 and TotStk and Ave_Cost fields.
*:           StyDye=> (Sty/Dye rec.) Stk1->8 and TotStk fields.
*:           StyInvJl=> Create a new record.
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
*: Example   : =gfStyCrl()
*:**************************************************************************
*E300839,1 TAK 01/22/98 Added FIFO and LIFO cost methods in Journal update.
*                       Replace name of lcSession by lcRISessn and join replace.
*B602066,1 AMM 08/05/98 Fix the bug of wronge calculation of Average cost 
*                       in case of physical inventory.
*B602566,1 AHM 02/28/99 Fix the bug of creating GL entery wit value
*                       defferes than the updated value in style file (nStkVal)
*B602743,1 AHM 04/05/99 calculate avg. cost instead of using ave_cost field
*E301200,1 AHM 04/13/99 update the added field (lineNo) in StyInvJl file
*E301192   AHM 04/19/1999 Update reference field in styinvjl with descriptive text
*E301198   AHM 04/20/1999 Update Added fields (nPrvSQty,nPrvSval) in styinvjl
*B602875   AHM 05/09/1999 fix the bug of wrong avg_cost update in style,stydye in physical
*B602884   lcLastRSess
*B602892   AHM 05/13/1999 1-Create Adj. records in case of receiving on neagtive
*B602892                    stock before main receiving record.
*B602892                  2-IF received qty less than ABS(current negative stock)
*B602892                  3-create adj. records with the received qty.
*B602892                  4-Disgard creating adj. records in Physical inventory.
*B602892                    update style, stydye with the value of the 2 adj. records
*B602898   AHM 05/13/1999 instead of calculate stkval for void invoice get this 
*B602898                  value from invoice issued line in styinvjl file to
*B602898                  elemenate dicemals               
*E301223   AHM 05/13/1999 issue ret. po with the same cost of receiving po
*E301223                  if there are no transactions have been done after
*E301223                  this receiving, if so the issuing will be with
*E301223                  inventory cost
*E301224   AHM 05/13/1999 void credit memo with the same cost of receiving CM
*E301224                  if there are no transactions have been done after
*E301224                  this receiving, if so the issuing will be with
*E301224                  inventory cost
*B603049   AHM 07/04/1999 Fix the bug of not creating issuing record in physical
*B603049                      when no changes has been made and not zero StkVal
*B802503,1 Reham On 08/10/1999
*B802503,1 Receive the inventort adjustment reference to be saved in the style
*B802503,1 inventory adjustment file in field "cAdjRef"
*B603149,1 TAK 09/07/1999 Added STR(lineNo,6) to the STYINVJL file index expresion
*B603149,1                and Make a coresponding changes due to that.
*B802827,1 AHM 11/24/1999 Fix the bug of not making any update in case of 
*B802827,1                doing physical while the total qty is zero, but 
*B802827,1                the sizes have quantities. 
*B603913,1 AMH 09/25/2000 Fix Addjastment Inventory with -ev Qty in FIFO method
*E301596,1 WAB 05/23/2001 Do not update inventory for in POS for other locations 
*B604383,1 WAB 05/24/2001 fix the bug of variable cAdjRef not exist
*B604999,1 KHM 10/09/2001 Fix the bug of Wrong update of CIRTYPE in the StyInvJl file. 
*B605013,1 ASH 10/10/2001 Don't appear message "The receiving quantity are not covered the issued quantity" 
*B605013,1                incase of receiving some sizes and issuing another sizes.
*B804516,1 ASH 11/15/2001 Fix the bug of wrong posting date in the auto cost adj. record in the gldist file.
*B605211,1 ASH 12/06/2001 Recompute the average cost & the stock value to avoid the decimals problem.
*B605347,1 ASH 07/30/2002 Fix the bug of recomputing the avg cost in case of issue style component. (when press on Actual cost button)
*B605925,4 ASH 08/01/2002 Fix the bug of saving wrong cirtype in case of locking styles with -ve stocks.
*B606577,1 ABD 12/17/2002 Fix bug that the style dye file update with wrong cost value in case we void a credit memo.
*B606875,1 ASH 01/16/2003 Fix bug 'variable lctmpjour not found'
*B607080,1 MAN 03/20/2003 Calculate cost of issued lots in case of LIFO and FIFO to be use by
*B607080,1 MAN            the caller programs such as arinv
*B607032,1 WAB 04/09/2003 Fix the bug of wrong Stokc value in the Stydye.nstkval in case of The stock still negative after receiving
*B607146,1 ABD 06/12/2003 Wrong computing nstkval in case average cost.
*B607458,1 KHM 07/31/2003 Fixed under bug # 607080.
*B607457,1 KHM 07/30/2003 Fix the problem of not updating the GLDIST correctly in case
*B607457,1                of void a consolidated invoice 
*B607395,1 ABD 07/24/2003 Fix Bug that if cost method is FIFO/LIFO and ship 2 record from the same style in 
*B607395,1 ABD            In one sales order and only one cover the stk the program ship the 2 lines.
*B119408,1 ABD 02/09/2003 Fix bug that the program add new line while locking the style,
*B119408,1 ABD            with Zero Qty in cost method Lifo and fifo.
*B038753,1 KHM 12/18/2004 Add a new parameter lnTranCost to hold the transaction cost
*B038753,1                in case of cost method is standard.
*B039094,1 NNA 04/13/2005 Fix bug that in the inventory Adj. the Avg. Cost updates with the Last Cost
*B039094,1 NNA            not the Avg. (if the Stock cost=9 and you Adjust Stock with(+) Qty with cost 11
*B039094,1 NNA            you'll find that it updated with 11 not 10)
*B128128,1 HBG 11/20/2005 Calculate the WIP based on the current cost case of cost method is standard. 
*B607981,1 TMI 02/25/2007 Get the nStkVal from nCost*nTotStk not from variables to face the rounding problems ( Ticket # T20070105.0006 )
*B608010,3 TMI 04/24/2007 remove the bug B128128, pls check my comment in MFCSSH.PRG under *B608010,3*
*B608092,1 tmi 05/23/2007 update nstkval with ncost * ntotstk in making physical inv. (T20070305.0001)
*              05/30/2007 also stopping updating the ave_cost while invoicing    ( T20061110.0001 )
*B608299,1 NNA 10/01/2007 (T20070713.0001) fix Bug that inventory control programs deal with non inventory styles
*B608299,1 NNA            SO in Gfstycrl I prevent updating styinvjl file if style is non inventory style
*B608351,1 NNA 11/14/2007 Fix bug that Voiding consolidated Invoices returning stock at Zero Cost
*B608725,1 TMI 10/16/2008 Sum the nStkVal for all lines of a style in "lfCalcStkVal" only if this is a consolidated invoice
*:**************************************************************************
FUNCTION gfStyCrl
*B802503,1 Reham On 08/10/1999   *** Begin ***
*B802503,1 Add new parameter for adjustment reference.
*PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
           laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,lcLastRSess

*koko (Start)
*PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
           laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,;
           lcLastRSess,lcAdjRef

*B038753,1 KHM 12/18/2004 Add a new parameter to hold the transaction cost [Begin]
*PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
           laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,;
           lcLastRSess,lcAdjRef,laLockInfo
PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
           laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
           lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,;
           lcLastRSess,lcAdjRef,laLockInfo, lnTranCost
PRIVATE lnTranCost
IF TYPE('lnTranCost') $ 'LU'
  lnTranCost = 0
ENDIF  
*B038753,1 KHM 12/18/2004 [End]

*koko (End)           
*B802503,1 Reham On 08/10/1999   *** End   ***
*E301596,1 WAB (START) - if the system tyoe is POS and the transaction created for an warhouse 
*E301596,1 WAB (START) - not belong to the current site return without do any thing.
lcSysType = gfGetMemVar('M_SYSTYPE')
IF lcSysType = 'P'
  PRIVATE llOpnWarhs 
  llOpnWarhs = .F.
  IF !USED('WAREHOUS')
    llOpnWarhs = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  ENDIF
  IF SEEK(lcWareCode,'WAREHOUS') AND WAREHOUS.cSiteId <> gcCurSite
    IF llOpnWarhs
      USE IN STYINVJL
    ENDIF
    RETURN (1)
  ENDIF
ENDIF
*E301596,1 WAB (END) 


PRIVATE lnStkVal
STORE 0 TO lnStkVal

PRIVATE lnLineNo
IF TYPE('lnLineNo') = 'L'
  lnLineNo = 0
ENDIF  

PRIVATE lcLastRSess
IF TYPE('lcLastRSess') = 'L'
  lcLastRSess = SPACE(6)
ENDIF  

*B802503,1 Reham On 08/10/1999   *** Begin ***
*B802503,1 Add new parameter to save the adjustment reference in case of inventory adjustment.
PRIVATE lcAdjRef
IF TYPE('lcAdjRef') = 'L'
  lcAdjRef = SPACE(6)
ENDIF
*B802503,1 Reham On 08/10/1999   *** End   ***

*--Initialize function variables.
PRIVATE lcOldWAr,laOldstk,llChekUncmp,lnSAveCost,lnWAveCost,;
        lnSOldStk,lnSOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,lcTmpJour,lcInvJour,lnRetStep,llUInvtry,;
        lnSStkVal,lnWStkVal

*--Style and Warehouse Average Cost,Old Stock and Old Cost variables.
STORE 0 TO lnSAveCost,lnSOldStk,lnSOldCst
STORE 0 TO lnWAveCost,lnWOldStk,lnWOldCst
DIME laOldstk[9]
laOldstk   = 0
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.  
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcSDyelot  = IIF(EMPTY(lcSDyelot),SPACE(10),lcSDyelot)

*--Check Uncomplete session flag if steps are passed as value not as zero.
llChekUncmp = ( lnStarStep <> 0 )    
*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,IIF(EMPTY(laGLInvAry[1,1]),.F.,.T.))
*--Return step to continue for after exit the function.
lnRetStep   = 0
*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_Cost_Meth')


*--Check the existing of the style and
*--Point the record in style and style dyelot files.
IF ! SEEK(lcStyle,'STYLE') OR !SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  *--The style ???? record are missing,
  *--Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  =gfModalGen('TRM42114B42000','DIALOG',lcStyle)
  RETURN (0)
ENDIF


*--Check if StyInvJL file is Open.
llOpnJurnl=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")


*--Case of Invoice or Void credit memo transactions, Put the Qty as
*--negative values since it is an Issue transaction.
*--If it is an Inventory adjustment or Receve return P/o the Qty must
*--Passes to this function as negative values since we have a Receive
*--case for this Inventory adj. and Receive P/o.
IF lcTrType $ '38'
  FOR lnI=1 TO 9
    laAdjStk[lnI] = -(ABS(laAdjStk[lnI]))
  ENDFOR
ENDIF


*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
*B604999,1 KHM 10/09/2001 (Begin) Fix the bug of Wrong update of CIRTYPE in the StyInvJl file. 
*lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '1368' , 'I' , 'R' )
*B605925,4 ASH 08/01/2002 (Begin) Fix the bug of saving wrong cirtype in case of locking styles with -ve stocks.
*lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '1368I' , 'I' , 'R' )
lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '123689I' , 'I' , 'R' )
*B605925,4 ASH 08/01/2002 (End)
*B604999,1 KHM 10/09/2001 (End)



*--Check the style Inventory Yes or No.
llUInvtry = STYLE.lInvSty

*B608299,1 NNA 10/01/2007 (Begin) Return if we deal with a non inventory style
IF !llUInvtry 
  RETURN (0)
ENDIF 
*B608299,1 NNA (End)

*--Get the Old Stock and Cost before updateing the new tansaction.

lnSOldStk = STYLE.TotStk
lnSOldCst = ABS(IIF(Style.TotStk=0,STYLE.Ave_Cost,STYLE.nStkVal / Style.TotStk))
lnWOldStk = STYDYE.TotStk

*koko (Start)
IF lcTrType = '9'
  lnWOldCst = IIF(laLockInfo[9]=0,0,laLockInfo[10]/laLockInfo[9])
ELSE
*koko (End)
  lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,STYDYE.nStkVal / StyDye.TotStk))
ENDIF  

*--Stock Value variable for style and StyDye.
lnSStkVal = IIF(lcTrType $ '29',0,STYLE.nStkVal )
lnOldSVal = IIF(lcTrType $ '29',STYDYE.nStkVal,0)

*koko (Start)
IF lcTrType = '9'
  lnWStkVal = laLockInfo[10]
ELSE
*koko (End)
  lnWStkVal = IIF(lcTrType $ '29',0,STYDYE.nStkVal)
*koko (Start)  
ENDIF
*koko (End)

PRIVATE lnDyeCost
lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)


PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = StyDye.TotStk
lnPrvVal = StyDye.nStkVal
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  lnPrvQty = StyDye.TotStk
  lnPrvVal = StyDye.TotStk * lnDyeCost
ENDIF

*B605347,1 ASH 07/30/2002 (Begin) Don't recompute the average cost in case of issue/return the style component.
*IF lcIRType = 'I'
*B606875,1 ASH 01/16/2003 (Begin) Fix bug 'variable lctmpjour not found'
*IF lcIRType = 'I' and lcTrType <> 'I' 
IF lcIRType = 'I' AND (lcTrType <> 'I' OR lcCostMeth $ 'FL')
*B606875,1 ASH 01/16/2003 (End)
*B605347,1 ASH 07/30/2002 (End)

  DO CASE
    CASE lcCostMeth = 'A'   && Average.
      lnNewCost = IIF((gfGetMemVar('M_WareHouse')='Y'),lnWOldCst ,lnSOldCst )

    CASE lcCostMeth = 'S'   && Standard.
      lnNewCost = STYLE.TotCost

    CASE lcCostMeth $ 'FL'  && FIFO or LIFO.
      *--In this case may be has more than cost so we hold this costs
      *--in lcTmpJour file that the following function will return.
      lcTmpJour = gfTempName()
      IF ! lfIsueCost(.F.)
        SELECT (lcOldWAr)
        RETURN (0)
      ENDIF
  ENDCASE
ELSE
  IF lcTrType = '4' AND  lcCostMeth $ 'FL'  && FIFO or LIFO.
    lcInvJour = gfTempName()
    = lfInvJour()
  ENDIF

  *--Receiving cost will be passed as parameter in this function.
ENDIF

*koko (Start)
IF lcTrType = '9'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
*koko (Start)
  SELECT STYDYE
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
ENDIF
*koko (End)

*--Temp Old Stock Array used in Physical or Markdown 
*--issue the old stock first and then receive the pysical quantity.
*-- In all other cases this array is Zero.

*koko (Start)
*IF lcTrType $ '29'
IF lcTrType $ '2'
*koko (End)
  SELECT STYDYE
  IF !EMPTY(lcSDyelot)
    = SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  ENDIF
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laOldstk
ENDIF

*--Calculate Avarage Cost for Style and StyDye records. 

*-- 1)  Update Style journal file. -------------------------------------
*--Read session no.
*--If receiving transaction and costing methos Lifo or Fifo make sure
*--that the session not duplecated fir same key.
lcRISessn = IIF( TYPE('lcRISessn')='C', lcRISessn ,'' )
IF lcIRType = 'R' AND lcCostMeth $ 'FL' AND !EMPTY(lcRISessn)
  lnJrlRec  = IIF(EOF('STYINVJL'),0,RECNO('STYINVJL'))
  lcRISessn = IIF(SEEK(lcStyle+lcWareCode+lcRISessn,'STYINVJL'),'',lcRISessn)
  IF lnJrlRec<>0
    GOTO lnJrlRec IN STYINVJL
  ENDIF
ENDIF
*--Read session no.
IF EMPTY(lcRISessn)
  lcRISessn = gfSequence('GLSESSION')
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
IF lcIRType = 'I' AND lcCostMeth $ 'FL'

  *B607080,1 MAN  Start
   lnIssTCst = 0
   lnIssTStk = 0
  *B607080,1 MAN  End
  SELECT (lcTmpJour)  
  SCAN
    REPLACE cSession  WITH lcRISessn,;
            cISession WITH cSession,;
            cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,cTrCode)
    *--AAMER (End)
    SCATTER MEMVAR
    IF lfCheckUnCmp(lnTmpStp)      
      SELECT STYINVJL
      APPEND BLANK
      GATHER MEMVAR  

      *B607080,1 MAN  Start
      lnIssTCst = lnIssTCst + m.nTotStk * m.nCost
      lnIssTStk = lnIssTStk + m.nTotStk
      *B607080,1 MAN  End

      REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
              cAdjReason WITH lcAdjCdRsn,;
              cAdjAcct   WITH lcAdjAcct,;
              nStkVal    WITH nTotStk * nCost,;
              LineNo     WITH lnLineNo,;
              nPrvSQty   WITH lnPrvQty,;
              nPrvSval   WITH lnPrvVal

      *B802503,1 Reham On 08/10/1999   *** Begin ***
      *B802503,1 Save the adjustment reference in case of inventory adjustment.
      REPLACE cAdjRef    WITH lcAdjRef
      *B802503,1 Reham On 08/10/1999   *** End   ***
      
      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')

      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  

      *--Call TraceKey global function.
      *B603149,1 Start, added STR(lineno,6) to index expr.
      *=gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode,'A')
      =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
      *B603149,1 End.
    ENDIF  
    lnTmpStp = lnTmpStp + 1

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()

    = lfStyWarDy()

  ENDSCAN

  *B607080,1 MAN  Start
  IF lnIssTStk <> 0
    lnIssAvg = ABS(lnIssTCst / lnIssTStk )
  ELSE
    lnIssAvg = 0
  ENDIF  
  *B607080,1 MAN  End 

  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpJour+'.DBF')
  ERASE (gcWorkDir+lcTmpJour+'.CDX')

ELSE  && Not LIFO or FIFO or Receiving.

  *--Create an issue record for Physical inventory or 
  *--Markdown inventory transaction in Style inventory Journal.

  IF lcTrType $ '29'
    IF lfDoPhys('I')
      =lfIsuJlTr()
      STORE 0 TO lnPrvQty,lnPrvVal
    ENDIF
  ENDIF

  IF !(lcTrType $ '29') AND lcIRType = 'R' AND lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    *-- This is to create 2 records in journal file
    *-- one for rec. the qty with it's old cost
    *-- the other for issue the qty with it's new cost
    = lfAdjRec()
  ENDIF

  IF lcTrType = '4' AND lcCostMeth $ 'FL'
    SELECT (lcInvJour)  
    SCAN
      lcRISessn = gfSequence('GLSESSION')
      REPLACE cSession  WITH lcRISessn,;
              cRSession WITH lcRISessn,;
              cTrCode   WITH lcTrCode
      SCATTER MEMVAR
       
      IF lfCheckUnCmp(lnTmpStp)      
        SELECT STYINVJL
        APPEND BLANK
        GATHER MEMVAR  
        REPLACE Reference  WITH lcRefer,;
                cAdjReason WITH lcAdjCdRsn,;
                cAdjAcct   WITH lcAdjAcct,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSval   WITH lnPrvVal

        *B802503,1 Reham On 08/10/1999   *** Begin ***
        *B802503,1 Save the adjustment reference in case of inventory adjustment.
        REPLACE cAdjRef    WITH lcAdjRef
        *B802503,1 Reham On 08/10/1999   *** End   ***
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')

        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        *B603149,1 Start, added STR(lineno,6) to index expr.
        *=gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode,'A')
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
        *B603149,1 End.
      ENDIF  
      lnTmpStp = lnTmpStp + 1
  
      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()

      = lfStyWarDy()

    ENDSCAN
    IF USED(lcInvJour)
      USE IN (lcInvJour)
    ENDIF
    *--Erase the temp. journal file.
    ERASE (gcWorkDir+lcInvJour+'.DBF')
    ERASE (gcWorkDir+lcInvJour+'.CDX')
  ELSE
  
    *--Create a main record in journal file.
    IF !(lcTrType $ '29') OR (lcTrType $ '29' AND lfDoPhys('R'))          
      IF lfCheckUnCmp(lnTmpStp)      
        SELECT STYINVJL
        *B602898 (Start)
        *APPEND BLANK
        *REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH laAdjStk[9] * lnNewCost,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal
    
        = lfCalcStkVal()
        APPEND BLANK
        *koko (Start)
        *REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH lnStkVal   ,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal

        *B607981,1 TMI [Start] Get the nStkVal from nCost*nTotStk not from variables to face rounding problems
        *REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH lnStkVal   ,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal
        REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH nTotStk * nCost ,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal
        *B607981,1 TMI [End  ] 

        *koko (End)
        *B602898 (End)
        
        *B802503,1 Reham On 08/10/1999   *** Begin ***
        *B802503,1 Save the adjustment reference in case of inventory adjustment.
        REPLACE cAdjRef    WITH lcAdjRef
        *B802503,1 Reham On 08/10/1999   *** End   ***
        
        *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
        *B038753,1                transaction cost in case of standard costing method [Begin].
        REPLACE nTranCost WITH lnTranCost
        *B038753,1 KHM 12/18/2004 [End]
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')

        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        *B603149,1 Start, added STR(lineno,6) to index expr.
        *=gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode,'A')
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
        *B603149,1 End.
      ENDIF
      lnTmpStp = lnTmpStp + 1


      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
    *koko (Start)
    IF lcTrType = '9'
      = lfLkAdjRec()
    ENDIF
    *koko (End)
  ENDIF
ENDIF

*--Initialize next step to continue when return.
lnRetStep = lnTmpStp

*--Close style journal if this function open it.
IF llOpnJurnl AND USED("StyInvJl")
  USE IN STYINVJL
ENDIF

SELECT (lcOldWAr)
RETURN IIF(llChekUncmp , lnRetStep , 1 )
*--End of gfStyCrl.prg

*!***********************************************************************
*! Name      : lfCheckUnCmp
*! Developer : Timour A. K.
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
*! Developer : Timour A. K.
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
*! Name      : lfIsueCost
*! Developer : Timour A. K.
*! Date      : 01/22/98
*! Purpose   : This function will get the journal records for the
*!             receiving transactions that will be applied on
*!             the current Issue transaction and take the cost of
*!             the receiving to be a new cost of issueing,
*!              depends on cost method FIFO or LIFO.    
*!*************************************************************
*! Parameter : If it calls for Physical inventorty procedure.
*!*************************************************************
*! Returns   : A temp file that contain all needed information
*!             this file named as 'lcTmpJour'
*!             If this function returns .F. that means that there
*!             is somthing wrong with creating data. 
*!*************************************************************
*! Example   :  =lfIsueCost()
*!*************************************************************
FUNCTION lfIsueCost
PARA llForPhys

PRIVATE lcAlias,llContnu,laTotRcvd
lcAlias = ALIAS()
SELECT STYINVJL
*--Create the Temp journal file with open receiving transactions.
*B603913,1 AMH 09/25/2000 Fix Addjastment Inventory with -ev Qty in FIFO method [Start]
*SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,;
       cIRType,cRSession,cISession,;
       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
       SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
       SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ;
FROM  STYINVJL ;
WHERE Style + cWareCode + cDyelot + cRSession + cISession = ;
      lcStyle + lcWareCode + lcSDyelot ;
GROUP BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
HAVING SUM(nTotStk) <> 0 OR SUM(nStkVal) <> 0;
ORDER BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
INTO DBF (gcWorkDir+lcTmpJour)
*B604383,1 WAB (start) add field cAdjRef to query
*SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,;
       cIRType,cRSession,cISession,;
       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
       SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
       SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ;
FROM  STYINVJL ;
WHERE Style + cWareCode + cDyelot + cRSession + cISession = ;
      lcStyle + lcWareCode + lcSDyelot ;
GROUP BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
HAVING SUM(nTotStk) <> 0 OR SUM(nStkVal) <> 0;
ORDER BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
INTO DBF (gcWorkDir+lcTmpJour)
*B605013,1 ASH 11/22/2001 (Begin) Remove the HAVING line from the select command and delete the zero stock records after creating the tmp file.
SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,;
       cIRType,cRSession,cISession,;
       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
       SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
       SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) as cAdjRef ;
FROM  STYINVJL ;
WHERE Style + cWareCode + cDyelot + cRSession + cISession = ;
      lcStyle + lcWareCode + lcSDyelot ;
GROUP BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
ORDER BY STYINVJL.Style,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
INTO DBF (gcWorkDir+lcTmpJour)
*B605013,1 ASH 11/22/2001 (End)
*B604383,1 WAB (END) 
*B603913,1 AMH [End]
SELECT (lcTmpJour)
*B605013,1 ASH 11/22/2001 (Begin) Delete the zero stock records from the tmp file.
DELETE ALL FOR nStk1=0 AND nStk2=0 AND nStk3=0 AND nStk4=0 AND nStk5=0 AND nStk6=0 AND nStk7=0 AND nStk8=0
*B605013,1 ASH 11/22/2001 
*DELETE ALL FOR nTotStk = 0
GO TOP
IF EOF()
  IF !llForPhys
    *--No open receiving exist for style XXXX ,
    *--This transaction line will be ignored.
    =gfModalGen('TRM42116B42000','DIALOG',lcStyle)
    USE
    RETURN .F.
  ELSE  && Get issue for Physical transactions.
    APPEND BLANK
    REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            nCost     WITH lnWOldCst,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH lcTrCode,;
            cIRType   WITH "I",;
            nStkVal   WITH lnOldSVal
    *B802503,1 Reham On 08/10/1999   *** Begin ***
    *B802503,1 Save the adjustment reference in case of inventory adjustment.
    REPLACE cAdjRef    WITH lcAdjRef
    *B802503,1 Reham On 08/10/1999   *** End   ***

    *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
    *B038753,1                transaction cost in case of standard costing method [Begin].
    REPLACE nTranCost WITH lnTranCost
    *B038753,1 KHM 12/18/2004 [End]

    RETURN
  ENDIF 
ENDIF

*--For Not Phyical.
IF !llForPhys
  *--Indexing the file on Ascending or Descending expresion 
  *--depends on LIFO or FIFO method.
  IF lcCostMeth = 'F'
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
  ELSE
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
  ENDIF
  GO TOP


  *--Start checking the only needed open receinving transaction for this
  *--issue transaction and put zero for all not needed receivings.

  *--Array to Hold the accomulation of the receiving untill it cover
  *--the issue quantity needed.
  DIME laTotRcvd[9]
  laTotRcvd = 0      
  SCAN
    llContnu  = .F.
    FOR I=1 TO 8 
      Z=STR(I,1)
      IF ABS(laAdjStk[I]) > laTotRcvd[I]
        llContnu = .T.
        laTotRcvd[I] = laTotRcvd[I] + nStk&Z
        laTotRcvd[9] = nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        IF ABS(laAdjStk[I]) <= laTotRcvd[I]
          REPLACE nStk&Z  WITH nStk&Z - (laTotRcvd[I] - ABS(laAdjStk[I]))
          REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
          REPLACE lNeeded WITH .T.

          *B607395,1 ABD - Fix Bug that if cost method is FIFO/LIFO and ship 2 record from the same style in 
          *B607395,1 ABD - In one sales order and only one cover the stk the program ship the 2 lines. [Begin]
        ELSE
          REPLACE lNeeded WITH .T.
          *B607395,1 ABD - [End]

        ENDIF   
      ELSE
        REPLACE nStk&Z  WITH 0
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE lNeeded WITH .T.
      ENDIF   
    ENDFOR 
    IF !llContnu 
      EXIT
    ENDIF
  ENDSCAN

  *--Check if all Issue quantity are covered by the receivings.
  IF llContnu 
    FOR I=1 TO 8 
      Z=STR(I,1)
      *B605013,1 ASH 10/10/2001 (Begin) Don't appear this message in case of receiving some sizes and issuing another sizes.
      *IF ABS(laAdjStk[I]) > laTotRcvd[I]
      IF laAdjStk[I] < 0 and ABS(laAdjStk[I]) > laTotRcvd[I]
      *B605013,1 ASH 10/10/2001 (End)
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        =gfModalGen('TRM42115B42000','DIALOG',lcStyle)
        USE
        RETURN .F. 
      ENDIF
    ENDFOR
  ENDIF

  *--Delete all not needed receiving transactions.
  DELETE ALL FOR nTotStk = 0 OR !lNeeded
ENDIF

*--Change it to Issue transactions,to use it in updating master Journal file.
REPLACE ALL cIRType WITH "I"    ,;
            dTrDate WITH ldTrDate,;
            cTrType WITH lcTrType,;
            cTrCode WITH lcTrCode,;
            nStk1   WITH -nStk1 ,;
            nStk2   WITH -nStk2 ,;  
            nStk3   WITH -nStk3 ,;  
            nStk4   WITH -nStk4 ,;  
            nStk5   WITH -nStk5 ,;  
            nStk6   WITH -nStk6 ,;  
            nStk7   WITH -nStk7 ,;  
            nStk8   WITH -nStk8 ,;  
            nTotStk WITH -nTotStk,;
            nStkVal WITH -nStkVal

SELECT (lcAlias)
RETURN .T.


*!*************************************************************
*! Name      : lfIsuJlTr
*! Developer : Timour A. K.
*! Date      : 01/22/98
*! Purpose   : This function will update the journal file with 
*!             Issue transaction record(s) for Physical or
*!             markdown transactions only.  
*!             If the method is Standard or Average we create
*!             only one Issue record for old stock before 
*!             physical transaction was done Else if the method
*!             is LIFO or FIFO we create issue records depends
*!             on all open receivings exist in journal.
*!*************************************************************
*! Call      :  lfIsueCost(.T.)
*!*************************************************************
*! Example   :  =lfIsuJlTr()
*!*************************************************************
FUNCTION lfIsuJlTr

IF lcCostMeth $ 'FL'  && ISSUE FIFO or LIFO.
  *--Get the open receivings.
  lcTmpJour = gfTempName()
  =lfIsueCost(.T.)

  SELECT (lcTmpJour)
  SCAN
    IF nTotStk <> 0 OR nStkVal <> 0
      REPLACE cSession  WITH lcRISessn,;
              cISession WITH cSession,;
              cTrCode   WITH IIF(cTrType $ "12",cSession,cTrCode)
      SCATTER MEMVAR       
      IF lfCheckUnCmp(lnTmpStp)
        SELECT STYINVJL
        APPEND BLANK
        GATHER MEMVAR
        REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
                cAdjReason WITH lcAdjCdRsn,;
                cAdjAcct   WITH lcAdjAcct ,;
                nStkVal    WITH IIF(nTotStk = 0,nStkVal,nTotStk * nCost),;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSval   WITH lnPrvVal
        
        *B802503,1 Reham On 08/10/1999   *** Begin ***
        *B802503,1 Save the adjustment reference in case of inventory adjustment.
        REPLACE cAdjRef    WITH lcAdjRef
        *B802503,1 Reham On 08/10/1999   *** End   ***

        *B119408,1 ABD - lock the issue recored in case lock the style. [Begin]
        REPLACE lLockFlg  WITH (lcTrType='9')
        *B119408,1 ABD - [End]

        *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
        *B038753,1                transaction cost in case of standard costing method [Begin].
        REPLACE nTranCost WITH lnTranCost
        *B038753,1 KHM 12/18/2004 [End]
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')

        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        *B603149,1 Start, added STR(lineno,6) to index expr.
        *=gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode,'A')
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
        *B603149,1 End.
      ENDIF  
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
  ENDSCAN
  USE
  *--Erase the temp. journal file.
  ERASE (gcWorkDir+lcTmpJour+'.DBF')

ELSE
      
  IF lfCheckUnCmp(lnTmpStp)       
    SELECT STYINVJL
    APPEND BLANK
    *REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost     WITH lnWOldCst,;
            cIRType   WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession WITH cSession,;
            nStk1     WITH -(laOldstk[1]) ,;
            nStk2     WITH -(laOldstk[2]) ,;
            nStk3     WITH -(laOldstk[3]) ,;
            nStk4     WITH -(laOldstk[4]) ,;
            nStk5     WITH -(laOldstk[5]) ,;
            nStk6     WITH -(laOldstk[6]) ,;
            nStk7     WITH -(laOldstk[7]) ,;
            nStk8     WITH -(laOldstk[8]) ,;
            nTotStk   WITH -(laOldstk[9]) ,;
            nStkVal   WITH IIF(laOldstk[9]=0,-lnOldSVal,-(laOldstk[9]) * lnWOldCst) ,;
            lLockFlg  WITH (lcTrType='9') ,;
            Reference WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal

    *koko (Start)
    *REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost     WITH lnWOldCst,;
            cIRType   WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
            cRSession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
            nStk1     WITH -(laOldstk[1]) ,;
            nStk2     WITH -(laOldstk[2]) ,;
            nStk3     WITH -(laOldstk[3]) ,;
            nStk4     WITH -(laOldstk[4]) ,;
            nStk5     WITH -(laOldstk[5]) ,;
            nStk6     WITH -(laOldstk[6]) ,;
            nStk7     WITH -(laOldstk[7]) ,;
            nStk8     WITH -(laOldstk[8]) ,;
            nTotStk   WITH -(laOldstk[9]) ,;
            nStkVal   WITH -(lnOldSVal) ,;
            lLockFlg  WITH (lcTrType='9') ,;
            Reference WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal
    *B608092,1 TMI [Start] replace the command
    *                                     nStkVal   WITH -(lnOldSVal)   ,;
    * with the equivelant                 nStkVal   WITH nCost*nTotStk  ,;
    * So that we alyas sure that the multiplication result is correct
    *REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost     WITH lnWOldCst,;
            cIRType   WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
            cRSession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
            nStk1     WITH -(laOldstk[1]) ,;
            nStk2     WITH -(laOldstk[2]) ,;
            nStk3     WITH -(laOldstk[3]) ,;
            nStk4     WITH -(laOldstk[4]) ,;
            nStk5     WITH -(laOldstk[5]) ,;
            nStk6     WITH -(laOldstk[6]) ,;
            nStk7     WITH -(laOldstk[7]) ,;
            nStk8     WITH -(laOldstk[8]) ,;
            nTotStk   WITH -(laOldstk[9]) ,;
            nStkVal   WITH -(lnOldSVal) ,;
            lLockFlg  WITH (lcTrType='9') ,;
            Reference WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal
    REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost     WITH lnWOldCst,;
            cIRType   WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
            cRSession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
            nStk1     WITH -(laOldstk[1]) ,;
            nStk2     WITH -(laOldstk[2]) ,;
            nStk3     WITH -(laOldstk[3]) ,;
            nStk4     WITH -(laOldstk[4]) ,;
            nStk5     WITH -(laOldstk[5]) ,;
            nStk6     WITH -(laOldstk[6]) ,;
            nStk7     WITH -(laOldstk[7]) ,;
            nStk8     WITH -(laOldstk[8]) ,;
            nTotStk   WITH -(laOldstk[9]) ,;
            nStkVal   WITH nCost*nTotStk  ,;
            lLockFlg  WITH (lcTrType='9') ,;
            Reference WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn,;
            cAdjAcct   WITH lcAdjAcct,;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal
     *B608092,1 TMI [End  ] 
    *koko (End)
    *B802503,1 Reham On 08/10/1999   *** Begin ***
    *B802503,1 Save the adjustment reference in case of inventory adjustment.
    REPLACE cAdjRef    WITH lcAdjRef
    *B802503,1 Reham On 08/10/1999   *** End   ***

    *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
    *B038753,1                transaction cost in case of standard costing method [Begin].
    REPLACE nTranCost WITH lnTranCost
    *B038753,1 KHM 12/18/2004 [End]
    
    *-- Call global function to add audit fields info.
    =gfAdd_Info('STYINVJL')

    *--Update Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  

    *--Call TraceKey global function.
    *B603149,1 Start, added STR(lineno,6) to index expr.
    *=gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode,'A')
    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    *B603149,1 End.
    lnTmpStp = lnTmpStp + 1
  
    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfStyWarDy()
  ENDIF
ENDIF  

RETURN

*!***********************************************************************
*! Name      : lfAdjRec
*! Developer : Ahmed Amer
*! Date      : 11/22/98
*! Purpose   : Add Rec. record and Iss. record in StyInvJl.
*!***********************************************************************
*! Return    : ......
*!***********************************************************************
*! Example   : lfAdjRec()
*!***********************************************************************

FUNCTION lfAdjRec

*-- Rec. with the old cost
      
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcSDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH '1',;
          cTrCode    WITH lcRISessn,;
          nCost      WITH lnWOldCst,;
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
          nPrvSVal   WITH lnPrvVal 

  *B802503,1 Reham On 08/10/1999   *** Begin ***
  *B802503,1 Save the adjustment reference in case of inventory adjustment.
  REPLACE cAdjRef    WITH lcAdjRef
  *B802503,1 Reham On 08/10/1999   *** End   ***
  
  *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
  *B038753,1                transaction cost in case of standard costing method [Begin].
  REPLACE nTranCost WITH lnTranCost
  *B038753,1 KHM 12/18/2004 [End]

  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  *B603149,1 Start, added STR(lineno,6) to index expr.
  *=gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode,'A')
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
  *B603149,1 End.
ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*-- Iss. with the new cost

IF lfCheckUnCmp(lnTmpStp)   
  SELECT STYINVJL
  APPEND BLANK
  *B607981,1 TMI [Start] Get the nStkVal from nCost*nTotStk not from variables to face the rounding problems
  *REPLACE cSession   WITH lcRISessn,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcSDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH '1',;
          cTrCode    WITH lcRISessn,;
          nCost      WITH lnNewCost,;
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
          nPrvSVal   WITH lnPrvVal
  REPLACE cSession   WITH lcRISessn,;
          Style      WITH lcStyle,;
          cWareCode  WITH lcWareCode,;
          cDyelot    WITH lcSDyelot,;
          dTrDate    WITH ldTrDate,;
          cTrType    WITH '1',;
          cTrCode    WITH lcRISessn,;
          nCost      WITH lnNewCost,;
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
          nStkVal    WITH nTotStk * nCost,;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn ,;
          cAdjAcct   WITH lcAdjAcct  ,;
          cISession  WITH cSession   ,;
          nPrvSQty   WITH lnPrvQty   ,;
          nPrvSVal   WITH lnPrvVal
  *B607981,1 TMI [End  ]  
  *B802503,1 Reham On 08/10/1999   *** Begin ***
  *B802503,1 Save the adjustment reference in case of inventory adjustment.
  REPLACE cAdjRef    WITH lcAdjRef
  *B802503,1 Reham On 08/10/1999   *** End   ***
  
  *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
  *B038753,1                transaction cost in case of standard costing method [Begin].
  REPLACE nTranCost WITH lnTranCost
  *B038753,1 KHM 12/18/2004 [End]

  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  *B603149,1 Start, added STR(lineno,6) to index expr.
  *=gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode,'A')
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
  *B603149,1 End.
ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*!*************************************************************
*! Name      : lfUpdGLDist()
*! Developer : Timour A. K.
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

*-- llNegStkAd Showes if it is main record (Start)
*-- or it is adj. record because the stock is less than Zero
*-- AAMER 11/22/98
*koko (Start)
*PARAMETERS llNegStkAd
PARAMETERS llNegStkAd,llLockAdj
*koko (End)
*-- llNegStkAd Showes if it is main record (End)

*-- AAMER (Start) 11/05/98
PRIVATE lnCurAlias
*-- AAMER (End)

*--Donot update if no GL used.
IF ! llGLUsed
  RETURN
ENDIF

*-- This means it is Main Record (Start)
*-- AAMER 11/22/98
IF !llNegStkAd
*-- This means it is Main Record (End)

  *--Update Gl for Main inventory record for Isue or Receive.
  *- Receiving Trans.(+1,2,4,5,+6,7):    None
  *-  => +/-  lnAmount = Total Recv. Qty * New Recv. Cost     
  *- Issue Trans.(-1,-3,-6,-8,-2)     :  None
  *-  => +/-  lnAmount = Total Issue Qty * Issue Cost     
  FOR lnAln=1 TO ALEN(laGLInvAry,1)

    *-- AAMER (Start)
    *laGLInvAry[lnAln,5] = IIF(StyInvJl.cTrType $ "12",STYINVJL.cSession,laGLInvAry[lnAln,5])
    laGLInvAry[lnAln,5] = STYINVJL.cTrCode
    *-- AAMER (End)       

    IF lfCheckUnCmp(lnTmpStp)
      
      *--B602566,1 (Start)
      *lnGLEnAmount = (STYINVJL.nTotStk * STYINVJL.nCost) * (laGLInvAry[lnAln,3])
      lnGLEnAmount = STYINVJL.nStkVal * laGLInvAry[lnAln,3]
      *--B602566,1 (End)
      *B608010,3 TMI [Start] Commenting this code, do not use lnTranCost in updating the GL here
      **B128128,1 HBG 11/20/2005 Calculate the WIP based on the current cost case of cost method is standard. [Begin]
      *IF laGLInvAry[lnAln,2]= '013'
      *  lnGLEnAmount = (STYINVJL.nTotStk * lnTranCost) * laGLInvAry[lnAln,3]
      *ENDIF  
      **B128128,1 [End]
      *B608010,3 TMI [End  ]

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
                     IIF(llLockAdj,laLockInfo[11],laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7]  ,;
                     laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
                     laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
                     laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
      *koko (End)
      *-- AAMER (Start)
      DO CASE
        CASE &laGLInvAry[lnAln,9]..catg_Key = '006'
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cICAcnt WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)

        *--update cadjact field in all cases 

        *--Updae cAdjAcct if it is empty in all catg_keys (Start) AAMER 04/13/99
        *--not if it is empty and catg_key = '007' 
        *CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR (EMPTY(StyInvJl.cAdjAcct) AND &laGLInvAry[lnAln,9]..catg_Key = '007')
        CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR EMPTY(StyInvJl.cAdjAcct)
        *--Updae cAdjAcct if it is empty in all catg_keys (End)
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cAdjAcct WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)
      ENDCASE
      *-- AAMER (End)
    
      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  
    ENDIF
    lnTmpStp = lnTmpStp + 1
  ENDFOR

*-- This means it is Adj. Record (Start)
*-- AAMER 11/22/98
ELSE
*-- This means it is Adj. Record (End)

*--Update Gl for Receiving inventory record and -ve old Stock.
*- Receiving Trans.(+1,2,4,5,+6,7):    OldStk<0 AND OldCost<>NewCost
*-   => +/-  lnAmount = MIN(Total Recv. Qty,ABS(OldStk)) * (Old Cost - New Recv. Cost )    

*-- This check is needn't now because the esle condition achive this check (Start)
*-- AAMER 11/22/98
*IF STYINVJL.cIRType = 'R' AND lnWOldStk < 0 AND lnWOldCst <> STYINVJL.nCost
*-- This check is needn't now because the esle condition achive this check (End)

  *--comment this part of code to use the styinvjl information
  *--to create adj. records instead of array info.
  
  *FOR lnAln=1 TO ALEN(laGLInvAry,1)
  *  *-- AAMER (Start)
  *  laGLInvAry[lnAln,5] = IIF(StyInvJl.cTrType $ "12",STYINVJL.cSession,laGLInvAry[lnAln,5])
  *  *-- AAMER (End)       

  *  IF lfCheckUnCmp(lnTmpStp)
  *    *--Calculate the GL entry amount.

  *    *-- Modify the amount of the GL entry (Start)
  *    *-- AAMER 11/22/98
  *    *-- Now gldist is created for every record in StyInvJl
  *    *-- AAMER 11/22/98
  *    *lnGLEnAmount = MIN(STYINVJL.nTotStk,ABS(lnWOldStk)) * ;
  *                   (lnWOldCst - STYINVJL.nCost) * (laGLInvAry[lnAln,3])

  *    *--B602566,1 (Start)
  *    *lnGLEnAmount = STYINVJL.nTotStk * STYINVJL.nCost * laGLInvAry[lnAln,3]
  *    *--AAMER Fix the sigen of the adj. records (Start) 
  *    *lnGLEnAmount = STYINVJL.nStkVal * laGLInvAry[lnAln,3]
  *    lnGLEnAmount = STYINVJL.nStkVal * IIF(STYINVJL.cIRType='R',1,-1) * laGLInvAry[lnAln,3]
  *    *--AAMER Fix the sigen of the adj. records (End) 
  *    *--B602566,1 (End)
  *    *-- Modify the amount of the GL entry (End)
      
  *    *--If the GL entres of the negative stock condition is not depends on
  *    *--the main Gl entry line, Undependent array elements.
  *    lcGLEnLkCod = laGLInvAry[1,1]
  *    lcGLEnCtTp  = IIF(lnAln=1,'006','007')

  *    DO GLDIST WITH lcGLEnLkCod ,;
  *                   lcGLEnCtTp  ,;
  *                   lnGLEnAmount,; 
  *                   'IA',laGLInvAry[lnAln,5]  ,;
  *                   laGLInvAry[lnAln,6],laGLInvAry[lnAln,7]  ,;
  *                   laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
  *                   laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
  *                   laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]


  *    *-- AAMER (Start)
  *    DO CASE
  *      CASE &laGLInvAry[lnAln,9]..catg_Key = '006'
  *        lnCurAlias = SELECT(0)
  *        SELECT StyInvJl
  *        REPLACE cICAcnt WITH &laGLInvAry[lnAln,9]..GLAccount
  *        SELECT (lnCurAlias)
  *      CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR (EMPTY(StyInvJl.cAdjAcct) AND &laGLInvAry[lnAln,9]..catg_Key = '007')
  *        lnCurAlias = SELECT(0)
  *        SELECT StyInvJl
  *        REPLACE cAdjAcct WITH &laGLInvAry[lnAln,9]..GLAccount
  *        SELECT (lnCurAlias)
  *    ENDCASE
  *    *-- AAMER (End)


  *    *--Update Uncomplete session Step.
  *    =lfUpdStep(lnTmpStp)  
  *  ENDIF
  *  lnTmpStp = lnTmpStp + 1
  *ENDFOR

  lcStyLink = IIF(EMPTY(StyDye.GL_Link),Style.Link_Code,StyDye.GL_Link)
  *B804516,1 ASH 11/15/2001 (Begin) Update the GLDIST with the posting date instead of the tran. date.
  *DO GLDIST WITH lcStyLink,'006',StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,StyInvJl.DtrDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
   DO GLDIST WITH lcStyLink,'006',StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''                 
  *B804516,1 ASH 11/15/2001 (End)               
  lnCurAlias = SELECT(0)
  SELECT StyInvJl
  REPLACE cICAcnt WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1
  *B804516,1 ASH 11/15/2001 (Begin) Update the GLDIST with the posting date instead of the tran. date.
  *DO GLDIST WITH lcStyLink,'007',-StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,StyInvJl.DtrDate,laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],StyInvJl.cAdjAcct,'','',''
  DO GLDIST WITH lcStyLink,'007',-StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],StyInvJl.cAdjAcct,'','',''
  *B804516,1 ASH 11/15/2001 (End)
                 
  IF EMPTY(StyInvJl.cAdjAcct)
    lnCurAlias = SELECT(0)
    SELECT StyInvJl
    REPLACE cAdjAcct WITH &laGLInvAry[1,9]..GLAccount
    SELECT (lnCurAlias)
  ENDIF
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1

ENDIF
RETURN

****************************************************************************
*B605211,1 ASH 12/06/2001 (Begin) ReWrite this function and rename it to be old.
*FUNCTION OldlfStyWarDy

*PRIVATE lnCurAlias
*lnCurAlias = SELECT(0)

**--1 ) Update Stock and Avarege cost in Style file. ------------
*IF lfCheckUnCmp(lnTmpStp)
*  SELECT STYLE
*  =RLOCK()
*  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
*          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
*          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
*          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
*          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
*          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
*          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
*          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
*          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
*          nStkVal  WITH IIF(TotStk=0,0,nStkVal+ StyInvJl.nStkVal),;
*          Ave_Cost WITH IIF(lcIRType='I' AND EMPTY(lcLastRSess),Ave_Cost,IIF(TotStk=0,StyInvJl.nCost,nStkVal/TotStk))
*  UNLOCK 
*  *--Update Uncomplete session Step.
*  =lfUpdStep(lnStarStep)  
*  *--Call TraceKey global function.
*  =gfTraceKey('STYLE',STYLE.Style,'M')
*ENDIF 

*lnTmpStp = lnTmpStp + 1

**--2 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
*IF lfCheckUnCmp(lnStarStep+1)
*  SELECT STYDYE
*  =SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
*  =RLOCK()

*  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
*          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
*          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
*          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
*          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
*          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
*          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
*          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
*          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
*          nStkVal  WITH IIF(TotStk=0,0,nStkVal+StyInvJl.nStkVal),;
*          Ave_Cost WITH IIF(lcIRType='I' AND EMPTY(lcLastRSess),Ave_Cost,IIF(TotStk=0,StyInvJl.nCost,nStkVal/TotStk))
*          UNLOCK 
*  *--Update Uncomplete session Step.
*  =lfUpdStep(lnStarStep+1)  
*  *--Call TraceKey global function.
*  =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')

*  lnPrvQty  = TotStk
*  lnPrvVal  = nStkVal
*  lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)

*ENDIF 

*lnTmpStp = lnTmpStp + 1

**--3 )  Update Stock in Style Dyelot file Dyelot record. --------
*IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
*  IF lfCheckUnCmp(lnStarStep+2)
*    SELECT STYDYE
*    =RLOCK()
*    REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
*            Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
*            Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
*            Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
*            Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
*            Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
*            Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
*            Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
*            TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
*    UNLOCK 
*    *--Update Uncomplete session Step.
*    =lfUpdStep(lnStarStep+2)  
*    *--Call TraceKey global function.
*    =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')

*    lnPrvQty = TotStk
*    lnPrvVal = TotStk * lnDyeCost

*  ENDIF 
*ENDIF

*lnTmpStp = lnTmpStp + 1

*SELECT(lnCurAlias)
************************************************************************


FUNCTION lfStyWarDy

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

*B607146,1 ABD - Start define new variable to us lnNewcost field Instead of styinvjl.ncost field. [Begin]
PRIVATE llNew_Cost
llNew_Cost = .F. 

*B039094,1 NNA 04/13/2005 (BEGIN) ADD the Average cost method to the if Statment
*IF lcIRType = 'R' .AND. !(lcCostMeth $ 'FL') .AND. !(lcTrType = '4' .AND. lcCostMeth $ 'FL' ) ;
  .AND. ( !(lcTrType $ '29') .OR. (lcTrType $ '29' AND lfDoPhys('R')))
IF lcIRType = 'R' .AND. !(lcCostMeth $ 'FLA') .AND. !(lcTrType = '4' .AND. lcCostMeth $ 'FL' ) ;
  .AND. ( !(lcTrType $ '29') .OR. (lcTrType $ '29' AND lfDoPhys('R')))
*B039094,1 NNA (END)

  llNew_Cost = .T.
ENDIF
*B607146,1 ABD - [End]


*B605211,1 ASH 12/06/2001 (Begin) Variable to hold the stock value difference.
lnValDiff=0
*** We changed the sequence of updating, we will update the stydye record first then the style record.
*B605211,1 ASH 12/06/2001 (End)
*--1 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
IF lfCheckUnCmp(lnTmpStp)
  *MAN1
  *MAN2 Start
  SELECT STYDYE
  =SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  =RLOCK()
  *B605211,1 ASH 12/6/2001 (Begin) Recompute the average cost & the stock value to avoid the decimals problem.
  *REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH IIF(TotStk=0,0,nStkVal+StyInvJl.nStkVal),;
          Ave_Cost WITH IIF(lcIRType='I' AND EMPTY(lcLastRSess),Ave_Cost,IIF(TotStk=0,StyInvJl.nCost,nStkVal/TotStk))
  
  lnPrvStk  = TotStk   && Old Stock
  lnStkVal  = nStkVal  && Old Stock Value
  lnAveCost = Ave_Cost && Old Average Cost
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
 
  *B607146,1 ABD - Remark the next few lines and use the Cost variable that define 
  *B607146,1 ABD - When Receive and cost method not FL and Transaction type # '29'. [Begin]
  *IF StyInvJl.nTotStk > 0                && Receive transaction
  *  IF lnPrvStk < 0                      && The stock was negative
  *    
  *    *B607032,1 WAB (Start) Wrong Condition because the lnPrvStk is a minus qty ( less then zero )
  *    **IF StyInvJl.nTotStk <= lnPrvStk    && The stock still negative after receiving
  *    
  *    IF StyInvJl.nTotStk + lnPrvStk < 0   && The stock still negative after receiving
  *    *B607032,1 WAB (End) 
  *    
  *      lnStkVal = lnStkVal +  StyInvJl.nTotStk * lnAveCost 
  *    ELSE                               && Use the transaction cost if the stock will be > 0
  *      lnStkVal = (StyInvJl.nTotStk+lnPrvStk) * StyInvJl.nCost
  *    ENDIF
  *  ELSE                                  
  *    lnStkVal = lnStkVal + (StyInvJl.nTotStk * StyInvJl.nCost)
  *  ENDIF
  *ELSE                                   && Issue transaction
  *  IF lnPrvStk = 0                      && If it is the 1st transaction for this style or the stock became 0.
  *    lnAveCost = StyInvJl.nCost
  *  ENDIF
  *  lnStkVal = TotStk * IIF(lnprvstk=0,lnAveCost,lnstkval/lnprvstk)
  *ENDIF
  *IF TotStk = 0 AND StyInvJl.nTotStk > 0
  *   lnAveCost = StyInvJl.nCost
  *ENDIF
  *IF TotStk > 0
  *  lnAveCost = lnStkVal/TotStk
  *ENDIF  
  
  IF StyInvJl.nTotStk > 0                && Receive transaction
    IF lnPrvStk < 0                      && The stock was negative
      
      IF StyInvJl.nTotStk + lnPrvStk < 0   && The stock still negative after receiving
        IF llNew_Cost
          lnStkVal = lnStkVal + StyInvJl.nTotStk * lnNewCost
        ELSE
          lnStkVal = lnStkVal +  StyInvJl.nTotStk * lnAveCost 
        ENDIF
      ELSE                               && Use the transaction cost if the stock will be > 0
        lnStkVal = (StyInvJl.nTotStk+lnPrvStk) * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
      ENDIF
    ELSE
      lnStkVal = lnStkVal + (StyInvJl.nTotStk * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost))
    ENDIF
  ELSE                                   && Issue transaction
    IF lnPrvStk = 0                      && If it is the 1st transaction for this style or the stock became 0.
      lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
    ENDIF
    lnStkVal = TotStk * IIF(llNew_Cost,lnNewCost,IIF(lnprvstk=0,lnAveCost,lnstkval/lnprvstk))
  ENDIF
  IF TotStk = 0 AND StyInvJl.nTotStk > 0
     lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
  ENDIF
  IF TotStk > 0
    lnAveCost = IIF(llNew_Cost,lnNewCost,lnStkVal/TotStk)
  ENDIF  
  *B607146,1 ABD - [End]
  
  IF lcIRType='I' AND !EMPTY(lcLastRSess)
    *B606577,1 ABD - Remark the next line and get the correct cost value. [Begin]
    *lnAveCost = IIF(TotStk=0,StyInvJl.nCost,nStkVal/TotStk)
    lnAveCost = IIF(TotStk=0,StyInvJl.nCost,lnStkVal/TotStk)
    *B606577,1 ABD - [End]

  ENDIF
  lnValDiff = lnStkVal - nStkVal
  *B039094,1 NNA 04/13/2005 (BEGIN) be Sure that it update in the Stydye file
  *REPLACE nStkVal  WITH IIF(TotStk=0,0,lnStkVal),;
          Ave_Cost WITH lnAveCost 
  *B608092,1 TMI [Start] stop updating the ave_cost when invoicing   - T20061110.0001
  *REPLACE StyDye.nStkVal  WITH IIF(TotStk=0,0,lnStkVal),;
          StyDye.Ave_Cost WITH lnAveCost 
  REPLACE StyDye.nStkVal  WITH IIF(TotStk=0,0,lnStkVal)
  IF lcTrType <> '3'
    REPLACE StyDye.Ave_Cost WITH lnAveCost 
  ENDIF
  *B608092,1 TMI [End  ] 
  *B039094,1 NNA (END)
  *B605211,1 ASH 12/6/2001 (End)

  UNLOCK 
*MAN2 End  
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep)  
*MAN3   
*MAN4 Start
  *--Call TraceKey global function.
  =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
  lnPrvQty  = TotStk
  lnPrvVal  = nStkVal
  lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)

*MAN4 End
ENDIF 

lnTmpStp = lnTmpStp + 1

*--2 ) Update Stock and Avarege cost in Style file. ------------
IF lfCheckUnCmp(lnStarStep+1)
  *MAN1 Start
  SELECT STYLE
  =RLOCK()
  *B605211,1 ASH 12/6/2001 (Begin) Recompute the average cost & the stock value to avoid the decimals problem.
  *REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH IIF(TotStk=0,0,nStkVal+ StyInvJl.nStkVal),;
          Ave_Cost WITH IIF(lcIRType='I' AND EMPTY(lcLastRSess),Ave_Cost,IIF(TotStk=0,StyInvJl.nCost,nStkVal/TotStk))
  *B608092,1 TMI [Start]   stop updating the ave_cost when invoicing   - T20061110.0001
  *REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH nStkVal + lnValDiff ,;
          Ave_Cost WITH IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH nStkVal + lnValDiff
   IF lcTrType <> '3'
     REPLACE Ave_Cost WITH IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
   ENDIF
  *B608092,1 TMI [End  ] 
  *B605211,1 ASH 12/6/2001 (End)
  UNLOCK 
  *MAN1 End

  *MAN2
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep+1)  
  *MAN3 Start  
  *--Call TraceKey global function.
  =gfTraceKey('STYLE',STYLE.Style,'M')
  *MAN3 End
  *MAN4 

ENDIF 

lnTmpStp = lnTmpStp + 1

*--3 )  Update Stock in Style Dyelot file Dyelot record. --------
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  IF lfCheckUnCmp(lnStarStep+2)
    SELECT STYDYE
    =RLOCK()
    REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
            Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
            Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
            Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
            Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
            Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
            Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
            Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
            TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
    UNLOCK 
    *--Update Uncomplete session Step.
    =lfUpdStep(lnStarStep+2)  
    *--Call TraceKey global function.
    =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')

    lnPrvQty = TotStk
    lnPrvVal = TotStk * lnDyeCost

  ENDIF 
ENDIF

lnTmpStp = lnTmpStp + 1

SELECT(lnCurAlias)

************************************************************************

FUNCTION lfCalcStkVal

*B602898
*E301223
*E301224

PRIVATE lnOrgJRec,lnRJRec,llDiffSess,lcOrgTran,lcJourTag,lnCurRec,lcOrgTrTyp
llDiffSess = .F.

lnStkVal = laAdjStk[9] * lnNewCost
IF lcTrType = '4'
  lcJourTag = ORDER('StyInvJl')
  lnCurRec  = RECNO('StyInvJl')
  SET ORDER TO Mfgopr
  IF SEEK(InvLine.Invoice+SPACE(6)+SPACE(2)+'3'+lcStyle+lcWareCode)
    *B608351,1 NNA 11/14/2007 (Begin) if this invoice is consolidated then remove lineno from searching expr.
    IF SEEK(InvLine.Invoice,'INVHDR') AND INVHDR.CONSOL='Y'
      LOCATE REST FOR cDyelot = InvLine.Dyelot 
    ELSE
    *B608351,1 NNA (End)
 
      LOCATE REST FOR cDyelot = InvLine.Dyelot AND LineNo = InvLine.LineNo

     *B608351,1 NNA (Begin)
     ENDIF
     *B608351,1 NNA (End)

    IF FOUND()
      
      *B607457,1 KHM 07/30/2003 (Begin) To handle the consolidated invoice also.
      lnStkVal = ABS(StyInvJl.nStkVal)
      *B608725,1 TMI [Start] get this sum only if this a consolidated invoice
      IF INVHDR.CONSOL = 'Y'
      *B608725,1 TMI [End  ] get this sum only if this a consolidated invoice
    
        SUM REST nStkVal WHILE cTrCode+cOprCode+cLotNo+cTrType+Style+cWareCode = ;
                             InvLine.Invoice+SPACE(6)+SPACE(2)+'3'+lcStyle+lcWareCode;      
                       FOR cDyelot = InvLine.Dyelot;
                       TO lnStkVal
        lnStkVal = ABS(lnStkVal)
  
        *B608725,1 TMI [Start] close the above if statement
      ENDIF
      *B608725,1 TMI [End  ]
      *B607457,1 KHM 07/30/2003 (End)
      
      *B607146,1 ABD -  Get The Correct & Actual Cost for the the Stok with Decimal number. [Begin]
      lnNewCost = EVAL(STR((lnStkVal/laAdjStk[9]),13,3))
      *B607146,1 ABD - [End]
    ENDIF
  ENDIF
  SET ORDER TO (lcJourTag)
  IF lnCurRec <= RECCOUNT()
    GO lnCurRec
  ENDIF
ELSE
  IF lcIRType = 'I' AND lcTrType $ '68'
    DO CASE
      CASE lcTrType = '6'
        lcOrgTran  = PosHdr.cPONo
        lcOrgTrTyp = '6'
      CASE lcTrType = '8'
        lcOrgTran  = lcTrCode
        lcOrgTrTyp = '7'
    ENDCASE
    
    SELECT StyInvJl
    lnOrgJRec = RECNO()

    IF SEEK(lcStyle+lcWareCode+lcLastRSess)
      LOCATE REST WHILE Style + cWareCode + cSession = ;
                        lcStyle + lcWareCode + lcLastRSess ;
                  FOR cTrCode = lcOrgTran   AND cTrType = lcOrgTrTyp ;
                  AND LineNo  = lnLineNo AND cDyelot  = lcSDyelot
      IF FOUND()
        lnRJRec = RECNO()
        LOCATE REST WHILE Style + cWareCode = lcStyle + lcWareCode ;
               FOR !INLIST(CtrCode,lcTrCode,lcOrgTran) ;
               AND IIF(CtrCode $ lcTrCode+','+lcOrgTran,LineNo = lnLineNo,.T.) ;
               AND cDyelot = lcSDyelot

        llDiffSess = FOUND()

        IF !llDiffSess
          IF lnRJRec <= RECCOUNT()
            GO lnRJRec
          ENDIF
          lnStkVal  = -ABS(nStkVal)

          *B607146,1 ABD -  Get The Correct & Actual Cost for the the Stok with Decimal number. [Begin]
          *lnNewCost = nCost
          lnNewCost = EVAL(STR((lnStkVal/laAdjStk[9]),13,3))
          *B607146,1 ABD - [End]
          
        ENDIF
      ENDIF
    ENDIF
    IF lnOrgJRec <= RECCOUNT()
      GO lnOrgJRec
    ENDIF
  ENDIF
ENDIF
*************************************

FUNCTION lfInvJour

SELECT SPACE(6) AS cSession,Style,cWareCode,cDyelot,dTrDate,'4' AS cTrType,cTrCode,;
       nCost,'R' AS cIRType,SPACE(6) AS cRSession,SPACE(6) AS cISession,LineNo    ,;
       -nStk1 AS nStk1,-nStk2 AS nStk2,-nStk3 AS nStk3,-nStk4 AS nStk4            ,;
       -nStk5 AS nStk5,-nStk6 AS nStk6,-nStk7 AS nStk7,-nStk8 AS nStk8            ,;
       -nTotStk AS nTotStk,-nStkVal AS nStkVal                ;
FROM  STYINVJL                                                ;
WHERE ctrcode +coprcode+clotno  +ctrtype+style  +cwarecode  = ;
      lcTrCode+SPACE(6)+SPACE(2)+'3'    +lcStyle+lcWareCode   ;
AND   LineNo = lnLineNo                   ;
AND   Dyelot = lcSDyelot                  ;
ORDER BY Style,cWareCode,cDyelot,LineNo   ;
INTO  DBF (gcWorkDir+lcInvJour)

***************************************

FUNCTION lfDoPhys
PARAMETERS lcRI
PRIVATE lcRI
lnRet = .F.

*B603049 (Start)
*DO CASE
*  CASE lcRI = 'I'
*    IF   laOldstk[9] = 0
*      lnRet = lnOldSVal # 0
*    ELSE
*      lnRet =  lnWOldCst   # lnNewCost    OR ;
*               laOldstk[1] # laAdjStk[1]  OR ;
*               laOldstk[2] # laAdjStk[2]  OR ;
*               laOldstk[3] # laAdjStk[3]  OR ;
*               laOldstk[4] # laAdjStk[4]  OR ;
*               laOldstk[5] # laAdjStk[5]  OR ;
*               laOldstk[6] # laAdjStk[6]  OR ;
*               laOldstk[7] # laAdjStk[7]  OR ;
*               laOldstk[8] # laAdjStk[8]  OR ;
*               laOldstk[9] # laAdjStk[9] 
*    ENDIF
*  CASE lcRI = 'R'
*    lnRet =  lnWOldCst # lnNewCost    OR ;
*             ( (laAdjStk[1] # 0 AND laOldstk[1] # laAdjStk[1]) OR ;
*               (laAdjStk[2] # 0 AND laOldstk[2] # laAdjStk[2]) OR ;
*               (laAdjStk[3] # 0 AND laOldstk[3] # laAdjStk[3]) OR ;
*               (laAdjStk[4] # 0 AND laOldstk[4] # laAdjStk[4]) OR ;
*               (laAdjStk[5] # 0 AND laOldstk[5] # laAdjStk[5]) OR ;
*               (laAdjStk[6] # 0 AND laOldstk[6] # laAdjStk[6]) OR ;
*               (laAdjStk[7] # 0 AND laOldstk[7] # laAdjStk[7]) OR ;
*               (laAdjStk[8] # 0 AND laOldstk[8] # laAdjStk[8]) OR ;
*               (laAdjStk[9] # 0 AND laOldstk[9] # laAdjStk[9]) )
*ENDCASE

DO CASE
  CASE lcRI = 'I'
    *-- Issue if the old stock value or the old stock qty doen't equal zero 
    *-- or the cost value has changed or the balance has changed.
    *B802827 (Start)
    *lnRet = lnOldSVal   # 0           OR ;
            laOldstk[9] # 0           OR ;
            lnWOldCst   # lnNewCost   OR ;
            laOldstk[9] # laAdjStk[9]
    lnRet = lnOldSVal   # 0 OR ;
            laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
            laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR ;
            lnWOldCst   # lnNewCost   OR ;
            laOldstk[9] # laAdjStk[9]
    *B802827 (End)

  CASE lcRI = 'R'
    *-- Receive only if the Issue record hasn't been issued or
    *-- the new balance is not zero or cost value has changed
    *B802827 (Start)
    *lnRet = !(lnOldSVal   # 0           OR ;
              laOldstk[9] # 0           OR ;
              lnWOldCst   # lnNewCost   OR ;
              laOldstk[9] # laAdjStk[9]      ) OR ;
              laAdjStk[9] # 0                  OR ;
              lnWOldCst   # lnNewCost        
    lnRet = !(lnOldSVal   # 0           OR ;
              laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
              laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR ;
              lnWOldCst   # lnNewCost   OR ;
              laOldstk[9] # laAdjStk[9]      ) OR ;
              laAdjStk[9] # 0                  OR ;
              lnWOldCst   # lnNewCost                      
    *B802827 (End)
ENDCASE
*B603049 (End)

RETURN lnRet

*******************************************************************

FUNCTION lfLkAdjRec

PRIVATE lcJourTag,lnCurAlias,;
        lnTotQty,lnTotVal,lnTranVal,lnDiffere

lnCurAlias = SELECT(0)

SELECT StyInvJl
lcJourTag = ORDER('StyInvJl')
SET ORDER TO StyInvJl

lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0
IF SEEK(lcStyle+lcWareCode,'StyInvJl')
  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode = ;
                  lcStyle+lcWareCode ;
            FOR   cDyelot = lcSDyelot AND !lLockFlg
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
    lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
  ENDSCAN
  
  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0 AND lfCheckUnCmp(lnTmpStp)
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn,;
            Style      WITH lcStyle,;
            cWareCode  WITH lcWareCode,;
            cDyelot    WITH lcSDyelot,;
            dTrDate    WITH laLockInfo[11],;
            cTrType    WITH lcTrType,;
            cTrCode    WITH IIF(EMPTY(lcTrCode),lcRISessn,lcTrCode),;
            cIRType    WITH IIF(lnDiffere<0,'I','R'),;
            nStkVal    WITH lnDiffere   ,;
            Reference  WITH 'Mark Down Adjustement Value',;
            cAdjReason WITH lcAdjCdRsn ,;
            cAdjAcct   WITH lcAdjAcct  ,;
            cISession  WITH IIF(lnDiffere<0,cSession,''),;
            cRSession  WITH IIF(lnDiffere>0,cSession,''),;
            nPrvSQty   WITH lnPrvQty,;
            nPrvSVal   WITH lnPrvVal,;
            cAdjRef    WITH lcAdjRef
    
    *B038753,1 KHM 12/18/2004 Replace the transaction cost. This is to replace the 
    *B038753,1                transaction cost in case of standard costing method [Begin].
    REPLACE nTranCost WITH lnTranCost
    *B038753,1 KHM 12/18/2004 [End]

    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    lnTmpStp = lnTmpStp + 1
    =lfUpdGLDist(.F.,.T.)
    =lfStyWarDy()
  ENDIF
ENDIF

SET ORDER TO (lcJourTag) IN StyInvJl
SELECT (lnCurAlias)




