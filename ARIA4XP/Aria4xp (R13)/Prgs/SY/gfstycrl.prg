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
*:                      lfUpdGLDist()
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
*: Modifications:
**B608073,1 MMT 05/07/2007 fix bug of wrong Cost Value
*B608284,1 TMI 09/24/2007 Fix a bug that  using the DYELOT field instead of CDYELOT ( ticket T20070920.0002 )
*B608791,1 MMT 01/26/2009 fix bug of numeric overflow[T20090113.0003]
*B609404,1 MMT 09/08/2010 Make Styinvjl.nStkVal always equal to STYINVJL.NCOST * STYINVJL.NTOTSTK[T20090818.0006]
*B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(T20100129.0008)
*B609836,1 MMT 02/20/2012 Date sensitive report displays incorrect unit cost[T20120210.0002]
*B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[T20120316.0004]
*B610218,1 MMT 01/27/2013 Invoice screen checks the stock on non-inventory style in case of FIFO/LIFO costing methods[T20130118.0003]
*B610223,1 MMT 01/31/2013 Incorrect recrod added to Styinvjl in case of  FIFO/LIFO method in  of lockingp[T20130130.0004]
*B610346,1 HIA 05/30/13 T20130510.0010 - The client wants to know how Inventory value & Avg. cost works
*B610436,1 TMI 07/11/2013 Invetory Adjustment report does not match styinvjl [T20130705.0001] 
*B610438,1 TMI 07/14/2013 fix bug of  WRONG COST ON STOCK ADJUSTMENT JOURNAL [T20130708.0014] 
*B610458,1 SAB 08/04/2013 Change the width of ncost field in Fox tables to be 3 decimal same as SQL tables [T20130329.0002]
*B610662,1 TMI 02/02/2014 18:49 [Start] update the lnStkVal in case of FIFO/LIFO based on the actal stock value [T20140121.0052]
*B610999,1 MMT 05/05/2015 Wrong style issue cost while invoicing[T20150126.0035]
*B611316,1 MHM 05/18/2017 style duplicated by itself when client do physical inventory [T20170309.0033]
*E303955,1 AHH 1/04/2018  use the GLDIST table remotely not native, because of conversion to SQL 
*E304018,1 SAH 06/14/2018 CONVERT STYINVJL TO SQL 
*E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL 
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [T20180508.0022]
*B611781,1 ES 06/02/2019 -  When user run the Data integrity check for Inventory control module, he got many record has incorrect stock values [T20181017.0003]
*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement]
*:**************************************************************************
FUNCTION gfStyCrl
*N037578,1 KHM 10/05/2004 Add a new parameter to be use in case of issuing inter-location PO.
*N037578,1                Because it uses POSHDR.cPONO and the POSHDR file is converted
*N037578,1                to SQL [Begin]
*PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,;
lcLastRSess,lcAdjRef,laLockInfo

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
  laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
  lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,;
  lcLastRSess,lcAdjRef,laLockInfo,lcCPONO
  
PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,;
  laAdjStk,lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,;
  lnStarStep,lcTmpLFile,lcStepFld,laGLInvAry,lnLineNo,;
  lcLastRSess,lcAdjRef,laLockInfo,lcCPONO,llDontSaveStyinvjl
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

  
*N037578,1 KHM 10/05/2004 [End]

*WAB- if the system tyoe is POS and the transaction created for an warhouse
*WAB- not belong to the current site return without do any thing.
lcSysType = gfGetMemVar('M_SYSTYPE')
IF lcSysType = 'P'
  PRIVATE llOpnWarhs
  llOpnWarhs = .F.
  IF !USED('WAREHOUS')
    llOpnWarhs = gfOpenFile(oAriaApplication.DataDir+"WAREHOUS","WAREHOUS","SH")
  ENDIF
  IF SEEK(lcWareCode,'WAREHOUS') AND WAREHOUS.cSiteId <> oAriaApplication.CurrentSite
    IF llOpnWarhs
      *E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN] 
      *USE IN STYINVJL
      USE IN WAREHOUS
      *E304018,1 SAH CONVERT STYINVJL TO SQL [END] 
    ENDIF
    RETURN (1)
  ENDIF
ENDIF
*--


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

*-- Add new parameter to save the adjustment reference in case of inventory adjustment.
PRIVATE lcAdjRef
IF TYPE('lcAdjRef') = 'L'
  lcAdjRef = SPACE(6)
ENDIF
*--

*--Initialize function variables.
PRIVATE lcOldWAr,laOldstk,lnSAveCost,lnWAveCost,;
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

*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,IIF(EMPTY(laGLInvAry[1,1]),.F.,.T.))
*--Return step to continue for after exit the function.
lnRetStep   = 0
*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_Cost_Meth')

*N037578,1 KHM 10/05/2004 Open the style,stydye and gldist files if it not open before
*N037578,1                because some programs open them remotely [Begin]
PRIVATE llOpnStyFil,llOpnSDyFil,llOpnGlDFil
STORE .F. TO llOpnStyFil,llOpnSDyFil,llOpnGlDFil
IF !USED('STYLE')
  llOpnStyFil = .T.
  = gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF
IF !USED('STYDYE')
  llOpnSDyFil = .T.
  = gfOpenFile(oAriaApplication.DataDir+'STYDYE','STYDYE','SH')
ENDIF
IF !USED('GLDIST')
  llOpnGlDFil = .T.
  *E303955,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*!*	  = gfOpenFile(oAriaApplication.DataDir+'GLDIST','','SH')
  = gfOpenTable(oAriaApplication.DataDir+'GLDIST','','SH')
  *E303955,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [END]
ENDIF
*N037578,1 KHM 10/05/2004 [End]


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


*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN] 
*llOpnJurnl=gfOpenFile(oAriaApplication.DataDir+"StyInvJl","StyInvJl","SH")
IF !USED("StyInvJl")
  llOpnJurnl = gfOpenTable(oAriaApplication.DataDir+"StyInvJl","StyInvJl","SH")
ELSE
  llOpnJurnl = .F.
ENDIF
*E304018,1 SAH CONVERT STYINVJL TO SQL [END] 
    
    
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
lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '1368I' , 'I' , 'R' )


*--Check the style Inventory Yes or No.
llUInvtry = STYLE.lInvSty
*--Get the Old Stock and Cost before updateing the new tansaction.

lnSOldStk = STYLE.TotStk
*B610458,1 SAB 08/04/2013 Change the width of ncost field in Fox tables to be 3 decimal same as SQL tables [T20130329.0002][Start]
**B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
**lnSOldCst = ABS(IIF(Style.TotStk=0,STYLE.Ave_Cost,STYLE.nStkVal / Style.TotStk))
*lnSOldCst = ABS(IIF(STYLE.TotStk=0,STYLE.Ave_Cost,ROUND(STYLE.nStkVal / STYLE.TotStk,2)))
**B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]
lnSOldCst = ABS(IIF(STYLE.TotStk=0,STYLE.Ave_Cost,ROUND(STYLE.nStkVal / STYLE.TotStk,3)))
*B610458,1 SAB 08/04/2013 Change the width of ncost field in Fox tables to be 3 decimal same as SQL tables [T20130329.0002][End]
lnWOldStk = STYDYE.TotStk

IF lcTrType = '9'
  *B610458,1 SAB 08/04/2013 Change the width of ncost field in Fox tables to be 3 decimal same as SQL tables [T20130329.0002][Start]
  **B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
  **lnWOldCst = IIF(laLockInfo[9]=0,0,laLockInfo[10]/laLockInfo[9])
  *lnWOldCst = IIF(laLockInfo[9]=0,0,ROUND(laLockInfo[10]/laLockInfo[9],2))
  **B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]
  lnWOldCst = IIF(laLockInfo[9]=0,0,ROUND(laLockInfo[10]/laLockInfo[9],3))
  *B610458,1 SAB 08/04/2013 Change the width of ncost field in Fox tables to be 3 decimal same as SQL tables [T20130329.0002][End]
ELSE
  *B610458,1 SAB 08/04/2013 Change the width of ncost field in Fox tables to be 3 decimal same as SQL tables [T20130329.0002][Start]
  **B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
  **lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,STYDYE.nStkVal / StyDye.TotStk))
  *lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,ROUND(STYDYE.nStkVal / StyDye.TotStk,2)))
  **B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]
  lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,ROUND(STYDYE.nStkVal / StyDye.TotStk,3)))
  *B610458,1 SAB 08/04/2013 Change the width of ncost field in Fox tables to be 3 decimal same as SQL tables [T20130329.0002][End]
ENDIF

*--Stock Value variable for style and StyDye.
lnSStkVal = IIF(lcTrType $ '29',0,STYLE.nStkVal )
lnOldSVal = IIF(lcTrType $ '29',STYDYE.nStkVal,0)

IF lcTrType = '9'
  lnWStkVal = laLockInfo[10]
ELSE
  lnWStkVal = IIF(lcTrType $ '29',0,STYDYE.nStkVal)
ENDIF

PRIVATE lnDyeCost
*B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
*lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,ROUND(StyDye.nStkVal/StyDye.TotStk,2))
*B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]

PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = StyDye.TotStk
lnPrvVal = StyDye.nStkVal
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  lnPrvQty = StyDye.TotStk
  lnPrvVal = StyDye.TotStk * lnDyeCost
ENDIF

IF lcIRType = 'I'
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

IF lcTrType = '9'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
  SELECT STYDYE
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
ENDIF

*--Temp Old Stock Array used in Physical or Markdown
*--issue the old stock first and then receive the pysical quantity.
*-- In all other cases this array is Zero.
IF lcTrType $ '2'
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
  *E304018,1 07/24/2018 MMT CONVERT STYINVJL TO SQL [BEGIN] 
  *lcRISessn = IIF(SEEK(lcStyle+lcWareCode+lcRISessn,'STYINVJL'),'',lcRISessn)
  lcRISessn = IIF(gfSEEK(lcStyle+lcWareCode+lcRISessn,'STYINVJL'),'',lcRISessn)
  *E304018,1 07/24/2018 MMT CONVERT STYINVJL TO SQL [End] 
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

*--Update journal for Issue Transaction ,FIFO or LIFO method.
IF lcIRType = 'I' AND lcCostMeth $ 'FL'
  SELECT (lcTmpJour)
  SCAN
    REPLACE cSession  WITH lcRISessn,;
      cISession WITH cSession,;
      cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,cTrCode)
    SCATTER MEMVAR
    SELECT STYINVJL
    APPEND BLANK
    GATHER MEMVAR
    REPLACE REFERENCE  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
      cAdjReason WITH lcAdjCdRsn,;
      cAdjAcct   WITH lcAdjAcct,;
      nStkVal    WITH nTotStk * nCost,;
      LINENO     WITH lnLineNo,;
      nPrvSQty   WITH lnPrvQty,;
      nPrvSval   WITH lnPrvVal,;
      cAdjRef    WITH lcAdjRef

    *-- Call global function to add audit fields info.
    =gfAdd_Info('STYINVJL')


	*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
	Replace OID with gfTempName()
	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
	lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
	SELECT STYINVJL
	IF lnQueryRslt > 0
 	  REPLACE CENTRYID WITH TMPNEWID.NEWOID
  ELSE
    REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
  ENDIF 	
  *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
	=gfReplace('') 
	*E304018,1 SAH CONVERT STYINVJL TO SQL [END]


    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()

    = lfStyWarDy()

  ENDSCAN
  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  ERASE (oAriaApplication.WorkDir+lcTmpJour+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcTmpJour+'.CDX')

ELSE  && Not LIFO or FIFO or Receiving.

  *--Create an issue record for Physical inventory or
  *--Markdown inventory transaction in Style inventory Journal.
 
  IF lcTrType $ '29'
    IF lfDoPhys('I')
      =lfIsuJlTr()
      STORE 0 TO lnPrvQty,lnPrvVal
    ENDIF
  ENDIF
  *B611316,1 MHM 05/18/2017 style duplicated by itself when client do physical inventory [start]
  *IF !(lcTrType $ '29') AND lcIRType = 'R' AND lnWOldStk < 0 AND lnWOldCst <> lnNewCost
   IF !(lcTrType $ '29') AND lcIRType = 'R' AND (laOldstk[1]< 0 AND laAdjStk[1]> 0) OR (laOldstk[2]< 0 AND laAdjStk[2]> 0) OR (laOldstk[3]< 0 AND laAdjStk[3]> 0) OR (laOldstk[4]< 0 AND laAdjStk[4]> 0) OR (laOldstk[5]< 0 AND laAdjStk[5]> 0) OR (laOldstk[6]< 0 AND laAdjStk[6]>0)OR (laOldstk[7]< 0 AND laAdjStk[7]>0) OR (laOldstk[8]<0 AND laAdjStk[8]>0); 
    AND lnWOldCst <> lnNewCost
  *B611316,1 05/18/2017 MHM style duplicated by itself when client do physical inventory [End]
  
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

      SELECT STYINVJL
      APPEND BLANK
      GATHER MEMVAR
      REPLACE REFERENCE  WITH lcRefer,;
        cAdjReason WITH lcAdjCdRsn,;
        cAdjAcct   WITH lcAdjAcct,;
        nPrvSQty   WITH lnPrvQty,;
        nPrvSval   WITH lnPrvVal,;
        cAdjRef    WITH lcAdjRef

      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')


      *E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
      Replace OID with gfTempName()
      *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  	  lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
  	  SELECT STYINVJL
  	  IF lnQueryRslt > 0
  	    REPLACE CENTRYID WITH TMPNEWID.NEWOID
      ELSE
  	    REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
      ENDIF 	
	    *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
      =gfReplace('') 
      *E304018,1 SAH CONVERT STYINVJL TO SQL [END]


      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()

      = lfStyWarDy()

    ENDSCAN
    IF USED(lcInvJour)
      USE IN (lcInvJour)
    ENDIF
    *--Erase the temp. journal file.
    ERASE (oAriaApplication.WorkDir+lcInvJour+'.DBF')
    ERASE (oAriaApplication.WorkDir+lcInvJour+'.CDX')
  ELSE

    *--Create a main record in journal file.
    IF !(lcTrType $ '29') OR (lcTrType $ '29' AND lfDoPhys('R'))

      SELECT STYINVJL

      = lfCalcStkVal()
      APPEND BLANK
      *B609404,1 MMT 09/08/2010 Make Styinvjl.nStkVal always equal to STYINVJL.NCOST * STYINVJL.NTOTSTK[Start]
      *!*	      REPLACE cSession   WITH lcRISessn,;
      *!*	              Style      WITH lcStyle,;
      *!*	              cWareCode  WITH lcWareCode,;
      *!*	              cDyelot    WITH lcSDyelot,;
      *!*	              dTrDate    WITH ldTrDate,;
      *!*	              cTrType    WITH lcTrType,;
      *!*	              cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
      *!*	              nCost      WITH lnNewCost,;
      *!*	              cIRType    WITH lcIRType,;
      *!*	              nStk1      WITH laAdjStk[1],;
      *!*	              nStk2      WITH laAdjStk[2],;
      *!*	              nStk3      WITH laAdjStk[3],;
      *!*	              nStk4      WITH laAdjStk[4],;
      *!*	              nStk5      WITH laAdjStk[5],;
      *!*	              nStk6      WITH laAdjStk[6],;
      *!*	              nStk7      WITH laAdjStk[7],;
      *!*	              nStk8      WITH laAdjStk[8],;
      *!*	              nTotStk    WITH laAdjStk[9],;
      *!*	              nStkVal    WITH lnStkVal   ,;
      *!*	              Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
      *!*	              lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
      *!*	              cAdjReason WITH lcAdjCdRsn ,;
      *!*	              cAdjAcct   WITH lcAdjAcct  ,;
      *!*	              cISession  WITH IIF(cIRType='I',cSession,''),;
      *!*	              cRSession  WITH IIF(cIRType='R',cSession,''),;
      *!*	              LineNo     WITH lnLineNo,;
      *!*	              nPrvSQty   WITH lnPrvQty,;
      *!*	              nPrvSVal   WITH lnPrvVal,;
      *!*	              cAdjRef    WITH lcAdjRef
      REPLACE cSession   WITH lcRISessn,;
        STYLE      WITH lcStyle,;
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
        nStkVal    WITH nCost * nTotStk  ,;
        REFERENCE  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
        lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
        cAdjReason WITH lcAdjCdRsn ,;
        cAdjAcct   WITH lcAdjAcct  ,;
        cISession  WITH IIF(cIRType='I',cSession,''),;
        cRSession  WITH IIF(cIRType='R',cSession,''),;
        LINENO     WITH lnLineNo,;
        nPrvSQty   WITH lnPrvQty,;
        nPrvSVal   WITH lnPrvVal,;
        cAdjRef    WITH lcAdjRef
      *B609404,1 MMT 09/08/2010 Make Styinvjl.nStkVal always equal to STYINVJL.NCOST * STYINVJL.NTOTSTK[End]
      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')

      *E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
      Replace OID with gfTempName()
      *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
    	lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
    	SELECT STYINVJL
    	IF lnQueryRslt > 0
      	REPLACE CENTRYID WITH TMPNEWID.NEWOID
      ELSE
      	REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
      ENDIF 	
    	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
      =gfReplace('') 
      *E304018,1 SAH CONVERT STYINVJL TO SQL [END]

   
      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
    *B610223,1 MMT 01/31/2013 Incorrect recrod added to Styinvjl in case of  FIFO/LIFO method in  of lockingp[Start]
    *IF lcTrType = '9'
    IF lcTrType = '9' AND lcCostMeth = 'A'
      *B610223,1 MMT 01/31/2013 Incorrect recrod added to Styinvjl in case of  FIFO/LIFO method in  of lockingp[END]
      = lfLkAdjRec()
    ENDIF

  ENDIF
ENDIF

*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
IF !llDontSaveStyinvjl
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
  SELECT StyInvJl
  =gfTableUpdate()
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
ENDIF 
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E304018,1 SAH CONVERT STYINVJL TO SQL [END]

*--Close style journal if this function open it.
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*IF llOpnJurnl AND USED("StyInvJl") 
IF llOpnJurnl AND USED("StyInvJl") AND !llDontSaveStyinvjl
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

  * USE IN STYINVJL
  *E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
  =gfCloseTable('STYINVJL')
  *E304018,1 SAH CONVERT STYINVJL TO SQL [END]
  
ENDIF

*N037578,1 KHM 10/05/2004 Close the style,stydye and gldist if they opened in this function [Begin]
IF llOpnStyFil
  USE IN STYLE
ENDIF
IF llOpnSDyFil
  USE IN STYDYE
ENDIF
IF llOpnGlDFil
  *E303955,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
  *USE IN GLDIST
  =gfCloseTable('GLDIST')
  *E303955,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
ENDIF
*N037578,1 KHM 10/05/2004 [End]

SELECT (lcOldWAr)
RETURN 1
*--End of gfStyCrl.prg



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
*N037552,1 HBG 08/18/2005 Enables SQL data engine compatibility with Visual FoxPro 7.0 to fix bug of GROUP BY is not valid [Begin]
lnPrvEng = SET("EngineBehavior")
SET ENGINEBEHAVIOR 70
*N037552,1 [End]
SELECT STYINVJL

*E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]
=gfSeek(lcStyle + lcWareCode,'STYINVJL','STYINVJL')
*E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]

*-ASH Remove the HAVING line from the select command and delete the zero stock records after creating the tmp file.
*B610438,1 TMI 07/14/2013 [Start] Modify the gfStyCrl.PRG to get the MAX(nCost) field as ncost in the select statement that gets the sum(nstk1)..... in lfIsueCost function
*SELECT cSession,STYLE,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,;
  cIRType,cRSession,cISession,;
  SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
  SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
  SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
  SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) AS cAdjRef ;
  FROM  STYINVJL ;
  WHERE STYLE + cWareCode + cDyelot + cRSession + cISession = ;
  lcStyle + lcWareCode + lcSDyelot ;
  GROUP BY STYINVJL.STYLE,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
  ORDER BY STYINVJL.STYLE,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
  INTO DBF (oAriaApplication.WorkDir+lcTmpJour)
SELECT cSession,STYLE,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,MAX(nCost) AS nCost,;
  cIRType,cRSession,cISession,;
  SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
  SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
  SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
  SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) AS cAdjRef ;
  FROM  STYINVJL ;
  WHERE STYLE + cWareCode + cDyelot + cRSession + cISession = ;
  lcStyle + lcWareCode + lcSDyelot ;
  GROUP BY STYINVJL.STYLE,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
  ORDER BY STYINVJL.STYLE,STYINVJL.cWareCode,STYINVJL.cDyelot,STYINVJL.cRSession ;
  INTO DBF (oAriaApplication.WorkDir+lcTmpJour)
*B610438,1 TMI 07/14/2013 [End  ] 
*N037552,1 HBG 08/18/2005 Enables SQL data engine compatibility with Visual FoxPro 7.0 to fix bug of GROUP BY is not valid [Begin]
SET ENGINEBEHAVIOR lnPrvEng
*N037552,1 [End]
*--
SELECT (lcTmpJour)
*-- Delete the zero stock records from the tmp file.
DELETE ALL FOR nStk1=0 AND nStk2=0 AND nStk3=0 AND nStk4=0 AND nStk5=0 AND nStk6=0 AND nStk7=0 AND nStk8=0
GO TOP
IF EOF()
  IF !llForPhys
    *--No open receiving exist for style XXXX ,
    *--This transaction line will be ignored.
    *B610218,1 MMT 01/27/2013 Invoice screen checks the stock on non-inventory style in case of FIFO/LIFO costing methods[T20130118.0003][Start]
    *!*	    =gfModalGen('TRM42116B42000','DIALOG',lcStyle)
    *!*	    USE
    IF STYLE.linvsty
      =gfModalGen('TRM42116B42000','DIALOG',lcStyle)
    ENDIF
    USE IN(lcTmpJour)
    *B610218,1 MMT 01/27/2013 Invoice screen checks the stock on non-inventory style in case of FIFO/LIFO costing methods[T20130118.0003][END]
    RETURN .F.
  ELSE  && Get issue for Physical transactions.
    APPEND BLANK
    REPLACE cSession  WITH lcRISessn,;
      STYLE     WITH lcStyle,;
      cWareCode WITH lcWareCode,;
      cDyelot   WITH lcSDyelot,;
      nCost     WITH lnWOldCst,;
      dTrDate   WITH ldTrDate,;
      cTrType   WITH lcTrType,;
      cTrCode   WITH lcTrCode,;
      cIRType   WITH "I",;
      nStkVal   WITH lnOldSVal,;
      cAdjRef    WITH lcAdjRef

    RETURN
  ENDIF
ENDIF

*--For Not Phyical.
IF !llForPhys
  *--Indexing the file on Ascending or Descending expresion
  *--depends on LIFO or FIFO method.
  IF lcCostMeth = 'F'
    INDEX ON STYLE+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
  ELSE
    INDEX ON STYLE+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
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
          *B610436,1 TMI 07/11/2013 [Start] comment this and move this replacement to out of the if statement
          *REPLACE lNeeded WITH .T.
          *B610436,1 TMI 07/11/2013 [End  ] 
        ENDIF
        *B610436,1 TMI 07/11/2013 [Start] always update lNeeded with .T.
        REPLACE lNeeded WITH .T.
        *B610436,1 TMI 07/11/2013 [End  ] 
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
      *-ASH Don't appear this message in case of receiving some sizes and issuing another sizes.
      *IF ABS(laAdjStk[I]) > laTotRcvd[I]
      IF laAdjStk[I] < 0 AND ABS(laAdjStk[I]) > laTotRcvd[I]
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
      SELECT STYINVJL
      APPEND BLANK
      GATHER MEMVAR
      REPLACE REFERENCE  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
        cAdjReason WITH lcAdjCdRsn,;
        cAdjAcct   WITH lcAdjAcct ,;
        nStkVal    WITH IIF(nTotStk = 0,nStkVal,nTotStk * nCost),;
        nPrvSQty   WITH lnPrvQty,;
        nPrvSval   WITH lnPrvVal,;
        cAdjRef    WITH lcAdjRef

      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')


      *E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
      Replace OID with gfTempName()
     	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
    	lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
    	SELECT STYINVJL
    	IF lnQueryRslt > 0
      	REPLACE CENTRYID WITH TMPNEWID.NEWOID
      ELSE
      	REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
      ENDIF 	
	  	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
      =gfReplace('') 
      *E304018,1 SAH CONVERT STYINVJL TO SQL [END]

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
  ENDSCAN
  USE
  *--Erase the temp. journal file.
  ERASE (oAriaApplication.WorkDir+lcTmpJour+'.DBF')

ELSE

  SELECT STYINVJL
  APPEND BLANK
  *B609404,1 MMT 09/08/2010 Make Styinvjl.nStkVal always equal to STYINVJL.NCOST * STYINVJL.NTOTSTK[Start]
  *!*	  REPLACE cSession  WITH lcRISessn,;
  *!*	          Style     WITH lcStyle,;
  *!*	          cWareCode WITH lcWareCode,;
  *!*	          cDyelot   WITH lcSDyelot,;
  *!*	          dTrDate   WITH ldTrDate,;
  *!*	          cTrType   WITH lcTrType,;
  *!*	          cTrCode   WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
  *!*	          nCost     WITH lnWOldCst,;
  *!*	          cIRType   WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
  *!*	          cISession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
  *!*	          cRSession WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
  *!*	          nStk1     WITH -(laOldstk[1]) ,;
  *!*	          nStk2     WITH -(laOldstk[2]) ,;
  *!*	          nStk3     WITH -(laOldstk[3]) ,;
  *!*	          nStk4     WITH -(laOldstk[4]) ,;
  *!*	          nStk5     WITH -(laOldstk[5]) ,;
  *!*	          nStk6     WITH -(laOldstk[6]) ,;
  *!*	          nStk7     WITH -(laOldstk[7]) ,;
  *!*	          nStk8     WITH -(laOldstk[8]) ,;
  *!*	          nTotStk   WITH -(laOldstk[9]) ,;
  *!*	          nStkVal   WITH -(lnOldSVal) ,;
  *!*	          lLockFlg  WITH (lcTrType='9') ,;
  *!*	          Reference WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
  *!*	          cAdjReason WITH lcAdjCdRsn,;
  *!*	          cAdjAcct   WITH lcAdjAcct,;
  *!*	          nPrvSQty   WITH lnPrvQty,;
  *!*	          nPrvSVal   WITH lnPrvVal,;
  *!*	          cAdjRef    WITH lcAdjRef
  REPLACE cSession  WITH lcRISessn,;
    STYLE     WITH lcStyle,;
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
    nStkVal   WITH nCost* nTotStk,;
    lLockFlg  WITH (lcTrType='9') ,;
    REFERENCE WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
    cAdjReason WITH lcAdjCdRsn,;
    cAdjAcct   WITH lcAdjAcct,;
    nPrvSQty   WITH lnPrvQty,;
    nPrvSVal   WITH lnPrvVal,;
    cAdjRef    WITH lcAdjRef
  *B609404,1 MMT 09/08/2010 Make Styinvjl.nStkVal always equal to STYINVJL.NCOST * STYINVJL.NTOTSTK[End]
  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')


  *E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
  Replace OID with gfTempName()
 	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
	lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
	SELECT STYINVJL
	IF lnQueryRslt > 0
  	REPLACE CENTRYID WITH TMPNEWID.NEWOID
  ELSE
  	REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
  ENDIF 	
	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
  =gfReplace('') 
  *E304018,1 SAH CONVERT STYINVJL TO SQL [END]

  *--Update Temp G/L Distribution file.
  =lfUpdGLDist()
  =lfStyWarDy()
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
SELECT STYINVJL
APPEND BLANK
*B609404,1 MMT 09/08/2010 Make Styinvjl.nStkVal always equal to STYINVJL.NCOST * STYINVJL.NTOTSTK[Start]
*!*	REPLACE cSession   WITH lcRISessn,;
*!*	        Style      WITH lcStyle,;
*!*	        cWareCode  WITH lcWareCode,;
*!*	        cDyelot    WITH lcSDyelot,;
*!*	        dTrDate    WITH ldTrDate,;
*!*	        cTrType    WITH '1',;
*!*	        cTrCode    WITH lcRISessn,;
*!*	        nCost      WITH lnWOldCst,;
*!*	        cIRType    WITH "R",;
*!*	        nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[1],laAdjStk[1]),;
*!*	        nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[2],laAdjStk[2]),;
*!*	        nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[3],laAdjStk[3]),;
*!*	        nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[4],laAdjStk[4]),;
*!*	        nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[5],laAdjStk[5]),;
*!*	        nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[6],laAdjStk[6]),;
*!*	        nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[7],laAdjStk[7]),;
*!*	        nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[8],laAdjStk[8]),;
*!*	        nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
*!*	        nStkVal    WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-lnWStkVal,nTotStk * lnWOldCst),;
*!*	        Reference  WITH "Auto cost adj. " + cTrCode ,;
*!*	        cAdjReason WITH lcAdjCdRsn ,;
*!*	        cAdjAcct   WITH lcAdjAcct  ,;
*!*	        cRSession  WITH cSession   ,;
*!*	        nPrvSQty   WITH lnPrvQty   ,;
*!*	        nPrvSVal   WITH lnPrvVal   ,;
*!*	        cAdjRef    WITH lcAdjRef
*B609836,1 MMT 02/20/2012 Date sensitive report displays incorrect unit cost[T20120210.0002][Start]
*!*	REPLACE cSession   WITH lcRISessn,;
*!*	        Style      WITH lcStyle,;
*!*	        cWareCode  WITH lcWareCode,;
*!*	        cDyelot    WITH lcSDyelot,;
*!*	        dTrDate    WITH ldTrDate,;
*!*	        cTrType    WITH '1',;
*!*	        cTrCode    WITH lcRISessn,;
*!*	        nCost      WITH lnWOldCst,;
*!*	        cIRType    WITH "R",;
*!*	        nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[1],laAdjStk[1]),;
*!*	        nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[2],laAdjStk[2]),;
*!*	        nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[3],laAdjStk[3]),;
*!*	        nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[4],laAdjStk[4]),;
*!*	        nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[5],laAdjStk[5]),;
*!*	        nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[6],laAdjStk[6]),;
*!*	        nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[7],laAdjStk[7]),;
*!*	        nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[8],laAdjStk[8]),;
*!*	        nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
*!*	        nStkVal    WITH nCost*nTotStk ,;
*!*	        Reference  WITH "Auto cost adj. " + cTrCode ,;
*!*	        cAdjReason WITH lcAdjCdRsn ,;
*!*	        cAdjAcct   WITH lcAdjAcct  ,;
*!*	        cRSession  WITH cSession   ,;
*!*	        nPrvSQty   WITH lnPrvQty   ,;
*!*	        nPrvSVal   WITH lnPrvVal   ,;
*!*	        cAdjRef    WITH lcAdjRef
REPLACE cSession   WITH lcRISessn,;
  STYLE      WITH lcStyle,;
  cWareCode  WITH lcWareCode,;
  cDyelot    WITH lcSDyelot,;
  dTrDate    WITH ldTrDate,;
  cTrType    WITH '1',;
  cTrCode    WITH lcRISessn,;
  nCost      WITH lnWOldCst,;
  cIRType    WITH "R",;
  nStk1      WITH IIF(laOldstk[1]+laAdjStk[1]>=0,-laOldstk[1],laAdjStk[1]),;
  nStk2      WITH IIF(laOldstk[2]+laAdjStk[2]>=0,-laOldstk[2],laAdjStk[2]),;
  nStk3      WITH IIF(laOldstk[3]+laAdjStk[3]>=0,-laOldstk[3],laAdjStk[3]),;
  nStk4      WITH IIF(laOldstk[4]+laAdjStk[4]>=0,-laOldstk[4],laAdjStk[4]),;
  nStk5      WITH IIF(laOldstk[5]+laAdjStk[5]>=0,-laOldstk[5],laAdjStk[5]),;
  nStk6      WITH IIF(laOldstk[6]+laAdjStk[6]>=0,-laOldstk[6],laAdjStk[6]),;
  nStk7      WITH IIF(laOldstk[7]+laAdjStk[7]>=0,-laOldstk[7],laAdjStk[7]),;
  nStk8      WITH IIF(laOldstk[8]+laAdjStk[8]>=0,-laOldstk[8],laAdjStk[8]),;
  nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
  nStkVal    WITH nCost*nTotStk ,;
  REFERENCE  WITH "Auto cost adj. " + cTrCode ,;
  cAdjReason WITH lcAdjCdRsn ,;
  cAdjAcct   WITH lcAdjAcct  ,;
  cRSession  WITH cSession   ,;
  nPrvSQty   WITH lnPrvQty   ,;
  nPrvSVal   WITH lnPrvVal   ,;
  cAdjRef    WITH lcAdjRef
*B609836,1 MMT 02/20/2012 Date sensitive report displays incorrect unit cost[T20120210.0002][End]
*B609404,1 MMT 09/08/2010 Make Styinvjl.nStkVal always equal to STYINVJL.NCOST * STYINVJL.NTOTSTK[End]

*-- Call global function to add audit fields info.
=gfAdd_Info('STYINVJL')


*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
Replace OID with gfTempName()
*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
SELECT STYINVJL
IF lnQueryRslt > 0
 	REPLACE CENTRYID WITH TMPNEWID.NEWOID
ELSE
 	REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
ENDIF 	
*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
=gfReplace('')
*E304018,1 SAH CONVERT STYINVJL TO SQL [END]


*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*-- Iss. with the new cost

SELECT STYINVJL
APPEND BLANK
*B609836,1 MMT 02/20/2012 Date sensitive report displays incorrect unit cost[T20120210.0002][Start]
*!*	REPLACE cSession   WITH lcRISessn,;
*!*	          Style      WITH lcStyle,;
*!*	          cWareCode  WITH lcWareCode,;
*!*	          cDyelot    WITH lcSDyelot,;
*!*	          dTrDate    WITH ldTrDate,;
*!*	          cTrType    WITH '1',;
*!*	          cTrCode    WITH lcRISessn,;
*!*	          nCost      WITH lnNewCost,;
*!*	          cIRType    WITH "I",;
*!*	          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[1],-laAdjStk[1]),;
*!*	          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[2],-laAdjStk[2]),;
*!*	          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[3],-laAdjStk[3]),;
*!*	          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[4],-laAdjStk[4]),;
*!*	          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[5],-laAdjStk[5]),;
*!*	          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[6],-laAdjStk[6]),;
*!*	          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[7],-laAdjStk[7]),;
*!*	          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[8],-laAdjStk[8]),;
*!*	          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
*!*	          nStkVal    WITH nTotStk * lnNewCost,;
*!*	          Reference  WITH "Auto cost adj. " + cTrCode ,;
*!*	          cAdjReason WITH lcAdjCdRsn ,;
*!*	          cAdjAcct   WITH lcAdjAcct  ,;
*!*	          cISession  WITH cSession   ,;
*!*	          nPrvSQty   WITH lnPrvQty   ,;
*!*	          nPrvSVal   WITH lnPrvVal   ,;
*!*	          cAdjRef    WITH lcAdjRef
REPLACE cSession   WITH lcRISessn,;
  STYLE      WITH lcStyle,;
  cWareCode  WITH lcWareCode,;
  cDyelot    WITH lcSDyelot,;
  dTrDate    WITH ldTrDate,;
  cTrType    WITH '1',;
  cTrCode    WITH lcRISessn,;
  nCost      WITH lnNewCost,;
  cIRType    WITH "I",;
  nStk1      WITH IIF(laOldstk[1]+laAdjStk[1]>=0,laOldstk[1],-laAdjStk[1]),;
  nStk2      WITH IIF(laOldstk[2]+laAdjStk[2]>=0,laOldstk[2],-laAdjStk[2]),;
  nStk3      WITH IIF(laOldstk[3]+laAdjStk[3]>=0,laOldstk[3],-laAdjStk[3]),;
  nStk4      WITH IIF(laOldstk[4]+laAdjStk[4]>=0,laOldstk[4],-laAdjStk[4]),;
  nStk5      WITH IIF(laOldstk[5]+laAdjStk[5]>=0,laOldstk[5],-laAdjStk[5]),;
  nStk6      WITH IIF(laOldstk[6]+laAdjStk[6]>=0,laOldstk[6],-laAdjStk[6]),;
  nStk7      WITH IIF(laOldstk[7]+laAdjStk[7]>=0,laOldstk[7],-laAdjStk[7]),;
  nStk8      WITH IIF(laOldstk[8]+laAdjStk[8]>=0,laOldstk[8],-laAdjStk[8]),;
  nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
  nStkVal    WITH nTotStk * lnNewCost,;
  REFERENCE  WITH "Auto cost adj. " + cTrCode ,;
  cAdjReason WITH lcAdjCdRsn ,;
  cAdjAcct   WITH lcAdjAcct  ,;
  cISession  WITH cSession   ,;
  nPrvSQty   WITH lnPrvQty   ,;
  nPrvSVal   WITH lnPrvVal   ,;
  cAdjRef    WITH lcAdjRef
*B609836,1 MMT 02/20/2012 Date sensitive report displays incorrect unit cost[T20120210.0002][End]
*-- Call global function to add audit fields info.
=gfAdd_Info('STYINVJL')

*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
Replace OID with gfTempName()
*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
SELECT STYINVJL
IF lnQueryRslt > 0
	REPLACE CENTRYID WITH TMPNEWID.NEWOID
ELSE
	REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
ENDIF 	
*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
=gfReplace('')
*E304018,1 SAH CONVERT STYINVJL TO SQL [END]


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
PARAMETERS llNegStkAd,llLockAdj
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

    laGLInvAry[lnAln,5] = STYINVJL.cTrCode
    lnGLEnAmount = STYINVJL.nStkVal * laGLInvAry[lnAln,3]
   	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  	lnReccountGLDist = RECCOUNT(laGLInvAry[lnAln,9])
	  *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
    DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2]  ,;
      lnGLEnAmount,;
      laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
      IIF(llLockAdj,laLockInfo[11],laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7]  ,;
      laGLInvAry[lnAln,8],laGLInvAry[lnAln,9]  ,;
      laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],;
      laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]

    DO CASE
    CASE &laGLInvAry[lnAln,9]..catg_Key = '006'
      lnCurAlias = SELECT(0)
      SELECT StyInvJl
      *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]
      *REPLACE cICAcnt WITH &laGLInvAry[lnAln,9]..GLAccount
      =gfReplace("cICAcnt WITH '"+&laGLInvAry[lnAln,9]..GLAccount+"'")
      *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
      SELECT (lnCurAlias)

      *--update cadjact field in all cases
      *--Updae cAdjAcct if it is empty in all catg_keys (Start) AAMER 04/13/99
      *--not if it is empty and catg_key = '007'
      *CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR (EMPTY(StyInvJl.cAdjAcct) AND &laGLInvAry[lnAln,9]..catg_Key = '007')
    CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR EMPTY(StyInvJl.cAdjAcct)
      *--Updae cAdjAcct if it is empty in all catg_keys (End)
      lnCurAlias = SELECT(0)
      SELECT StyInvJl
      *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]
      *REPLACE cAdjAcct WITH &laGLInvAry[lnAln,9]..GLAccount
      =gfReplace("cAdjAcct WITH '"+&laGLInvAry[lnAln,9]..GLAccount+"'")
      *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
      SELECT (lnCurAlias)
    ENDCASE
    *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
    lnOldAlias = SELECT(0)
    SELECT (laGLInvAry[lnAln,9])
    SCAN FOR  (RECNO()>lnReccountGLDist) OR (EMPTY(CENTRYID) AND RECNO()<0)
      REPLACE CENTRYID WITH STYINVJL.CENTRYID 
    ENDSCAN 
    SELECT (lnOldAlias)
   	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
  ENDFOR

  *-- This means it is Adj. Record (Start)
ELSE
  *-- This means it is Adj. Record (End)
  *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  lnReccountGLDist = RECCOUNT(laGLInvAry[1,9])
	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
  lcStyLink = IIF(EMPTY(StyDye.GL_Link),STYLE.Link_Code,StyDye.GL_Link)
  DO GLDIST WITH lcStyLink,'006',StyInvJl.nStkVal,'IA',;
    StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
    laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  lnCurAlias = SELECT(0)
  SELECT StyInvJl
  REPLACE cICAcnt WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  DO GLDIST WITH lcStyLink,'007',-StyInvJl.nStkVal,'IA',;
    StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
    laGLInvAry[1,8],laGLInvAry[1,9],StyInvJl.cAdjAcct,'','',''

  IF EMPTY(StyInvJl.cAdjAcct)
    lnCurAlias = SELECT(0)
    SELECT StyInvJl
    *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]
    *REPLACE cAdjAcct WITH &laGLInvAry[1,9]..GLAccount
    =gfReplace("cAdjAcct WITH '"+&laGLInvAry[1,9]..GLAccount+"'")
    *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
    SELECT (lnCurAlias)
  ENDIF
 	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  lnOldAlias = SELECT(0)
  SELECT (laGLInvAry[1,9])
  SCAN FOR (RECNO()>lnReccountGLDist) OR (EMPTY(CENTRYID) AND RECNO()<0)
    REPLACE CENTRYID WITH STYINVJL.CENTRYID 
  ENDSCAN 
  SELECT (lnOldAlias)
	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
ENDIF



RETURN

****************************************************************************
****************************************************************************

FUNCTION lfStyWarDy

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

*ASH  Variable to hold the stock value difference.
lnValDiff=0
*** We changed the sequence of updating, we will update the stydye record first then the style record.

*--1 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
SELECT STYDYE
=SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
=RLOCK()

lnPrvStk  = TotStk   && Old Stock
*B610999,1 MMT 05/05/2015 Wrong style issue cost while invoicing[T20150126.0035][Start]
*lnStkVal  = nStkVal  && Old Stock Value
lnStkVal  = IIF(lnPrvStk = 0,0,nStkVal)  && Old Stock Value
*B610999,1 MMT 05/05/2015 Wrong style issue cost while invoicing[T20150126.0035][End]
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
UNLOCK
IF StyInvJl.nTotStk > 0                && Receive transaction
  IF lnPrvStk < 0                      && The stock was negative
    *!B610346,1 HIA 05/30/13 T20130510.0010 - The client wants to know how Inventory value & Avg. cost works [Begin]
    *IF StyInvJl.nTotStk <= lnPrvStk    && The stock still negative after receiving
    IF StyInvJl.nTotStk <= ABS(lnPrvStk)    && The stock still negative after receiving
      *!B610346,1 HIA 05/30/13 T20130510.0010 - The client wants to know how Inventory value & Avg. cost works [End]
      lnStkVal = lnStkVal +  StyInvJl.nTotStk * lnAveCost
    ELSE                               && Use the transaction cost if the stock will be > 0
      lnStkVal = (StyInvJl.nTotStk+lnPrvStk) * StyInvJl.nCost
    ENDIF
  ELSE
    lnStkVal = lnStkVal + (StyInvJl.nTotStk * StyInvJl.nCost)
  ENDIF
ELSE                                   && Issue transaction
  IF lnPrvStk = 0                      && If it is the 1st transaction for this style or the stock became 0.
    lnAveCost = StyInvJl.nCost
  ENDIF
  *B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
  *lnStkVal = TotStk * IIF(lnprvstk=0,lnAveCost,lnstkval/lnprvstk)
  *B610662,1 TMI 02/02/2014 18:49 [Start] update the lnStkVal in case of FIFO/LIFO based on the actal stock value 
  IF lcCostMeth $ 'FL'
    lnStkVal = lnStkVal + (StyInvJl.nTotStk * StyInvJl.nCost)
  ELSE
    *B610662,1 TMI 02/02/2014 18:50 [End  ] 

*B611781,1 ES 06/02/2019 -  When user run the Data integrity check for Inventory control module, he got many record has incorrect stock values [Start]
   * lnStkVal = TotStk * IIF(lnprvstk=0,lnAveCost,ROUND(lnstkval/lnprvstk,2))
     lnStkVal = TotStk * IIF(lnprvstk=0,lnAveCost,(lnstkval/lnprvstk))
  *B611781,1 ES 06/02/2019 -  When user run the Data integrity check for Inventory control module, he got many record has incorrect stock values [End]
    *B610662,1 TMI 02/02/2014 18:53 [Start] 
  ENDIF 
  *B610662,1 TMI 02/02/2014 18:53 [End  ] 

  *B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]
ENDIF

IF TotStk = 0 AND StyInvJl.nTotStk > 0
  lnAveCost = StyInvJl.nCost
ENDIF
IF TotStk > 0
  *B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
  *lnAveCost = lnStkVal/TotStk
  lnAveCost = ROUND(lnStkVal/TotStk,2)
  *B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]
ENDIF

IF lcIRType='I' AND !EMPTY(lcLastRSess)
  *B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
  *lnAveCost = IIF(TotStk=0,StyInvJl.nCost,nStkVal/TotStk)
  lnAveCost = IIF(TotStk=0,StyInvJl.nCost,ROUND(nStkVal/TotStk,2))
  *B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]
ENDIF
lnValDiff = lnStkVal - nStkVal
=RLOCK()
REPLACE nStkVal  WITH IIF(TotStk=0,0,lnStkVal),;
  Ave_Cost WITH lnAveCost
UNLOCK

lnPrvQty  = TotStk
lnPrvVal  = nStkVal
*B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[Start]
*lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,ROUND(StyDye.nStkVal/StyDye.TotStk,2))
*B609957,1 MMT 06/11/2012 Fix rounding problem in gfstycrl.prg while updating nstkval  field in style file[END]
*--2 ) Update Stock and Avarege cost in Style file. ------------
SELECT STYLE
=RLOCK()

*B608073,1 MMT 05/06/2007 fix bug of wrong Cost Value[Start]
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
  nStkVal  WITH nStkVal + lnValDiff ,;
  Ave_Cost WITH lnAveCost &&IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
*B608073,1 MMT 05/06/2007 fix bug of wrong Cost Value[End]

UNLOCK

*--3 )  Update Stock in Style Dyelot file Dyelot record. --------
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
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

  lnPrvQty = TotStk
  lnPrvVal = TotStk * lnDyeCost
ENDIF

SELECT(lnCurAlias)

************************************************************************

FUNCTION lfCalcStkVal

PRIVATE lnOrgJRec,lnRJRec,llDiffSess,lcOrgTran,lcJourTag,lnCurRec,lcOrgTrTyp
llDiffSess = .F.
*E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [Start]
IF !USED('STYINVJL_T')
  = gfOpenTable('STYINVJL','STYINVJL','SH','StyINVJL_T')
ENDIF
SELECT 'STYINVJL_T'
*E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [End]
lnStkVal = laAdjStk[9] * lnNewCost

IF lcTrType = '4'
  *E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [Start]
  lcJourTag = ORDER('StyINVJL_T')
  lnCurRec  = RECNO('StyINVJL_T')
  *E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [End]
  *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]  
*!*	  SET ORDER TO Mfgopr
*!*	  IF SEEK(InvLine.Invoice+SPACE(6)+SPACE(2)+'3'+lcStyle+lcWareCode)
  =gfSetOrder('Mfgopr')
  IF gfSEEK(InvLine.Invoice+SPACE(6)+SPACE(2)+'3'+lcStyle+lcWareCode)
  *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]  
    LOCATE REST FOR cDyelot = InvLine.Dyelot AND LINENO = InvLine.LINENO
    IF FOUND()
      *E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [Start]
	  *lnStkVal = ABS(StyINVJL_T.nStkVal)      
      lnStkVal = ABS(StyINVJL.nStkVal)
      *E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [End]
    ENDIF
  ENDIF
  *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN] 
  *SET ORDER TO (lcJourTag)
  =gfSetOrder(lcJourTag)
  *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End] 
  IF lnCurRec <= RECCOUNT()
    GO lnCurRec
  ENDIF
ELSE
  IF lcIRType = 'I' AND lcTrType $ '68'
    DO CASE
    CASE lcTrType = '6'
      *N037578,1 KHM 10/05/2004 Used the passed parameter [Begin]
      *lcOrgTran  = PosHdr.cPONo
      lcOrgTran  = IIF(TYPE("lcCPONO") = "C",lcCPONO,"")
      *N037578,1 KHM 10/05/2004 [End]
      lcOrgTrTyp = '6'
    CASE lcTrType = '8'
      lcOrgTran  = lcTrCode
      lcOrgTrTyp = '7'
    ENDCASE
    *E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [Start]
    SELECT StyINVJL_T
    *E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [End]
    lnOrgJRec = RECNO()
    *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]
    *IF SEEK(lcStyle+lcWareCode+lcLastRSess)
    IF gfSeek(lcStyle+lcWareCode+lcLastRSess)
    *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
      LOCATE REST WHILE STYLE + cWareCode + cSession = ;
        lcStyle + lcWareCode + lcLastRSess ;
        FOR cTrCode = lcOrgTran   AND cTrType = lcOrgTrTyp ;
        AND LINENO  = lnLineNo AND cDyelot  = lcSDyelot
      IF FOUND()
        lnRJRec = RECNO()
        LOCATE REST WHILE STYLE + cWareCode = lcStyle + lcWareCode ;
          FOR !INLIST(CtrCode,lcTrCode,lcOrgTran) ;
          AND IIF(CtrCode $ lcTrCode+','+lcOrgTran,LINENO = lnLineNo,.T.) ;
          AND cDyelot = lcSDyelot

        llDiffSess = FOUND()

        IF !llDiffSess
          IF lnRJRec <= RECCOUNT()
            GO lnRJRec
          ENDIF
          lnStkVal  = -ABS(nStkVal)
          lnNewCost = nCost
        ENDIF
      ENDIF
    ENDIF
    IF lnOrgJRec <= RECCOUNT()
      GO lnOrgJRec
    ENDIF
  ENDIF
ENDIF
*E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [Start]
=gfCloseTable('StyINVJL_T')
SELECT STYINVJL
*E304018,2 MMT 01/13/2019 CONVERT STYINVJL TO SQL [End]
*************************************

FUNCTION lfInvJour

*E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]  
=gfSeek(lcTrCode+SPACE(6)+SPACE(2)+'3'    +lcStyle+lcWareCode ,'STYINVJL','MFGOPR')
*E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]  

*B608284,1 TMI [Start] change the field DYELOT to CDYELOT
*SELECT SPACE(6) AS cSession,Style,cWareCode,cDyelot,dTrDate,'4' AS cTrType,cTrCode,;
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
INTO  DBF (oAriaApplication.WorkDir+lcInvJour)
SELECT SPACE(6) AS cSession,STYLE,cWareCode,cDyelot,dTrDate,'4' AS cTrType,cTrCode,;
  nCost,'R' AS cIRType,SPACE(6) AS cRSession,SPACE(6) AS cISession,LINENO    ,;
  -nStk1 AS nStk1,-nStk2 AS nStk2,-nStk3 AS nStk3,-nStk4 AS nStk4            ,;
  -nStk5 AS nStk5,-nStk6 AS nStk6,-nStk7 AS nStk7,-nStk8 AS nStk8            ,;
  -nTotStk AS nTotStk,-nStkVal AS nStkVal                ;
  FROM  STYINVJL                                                ;
  WHERE ctrcode +coprcode+clotno  +ctrtype+STYLE  +cwarecode  = ;
  lcTrCode+SPACE(6)+SPACE(2)+'3'    +lcStyle+lcWareCode   ;
  AND   LINENO = lnLineNo                   ;
  AND   cDyelot = lcSDyelot                  ;
  ORDER BY STYLE,cWareCode,cDyelot,LINENO   ;
  INTO  DBF (oAriaApplication.WorkDir+lcInvJour)
*B608284,1 TMI [End  ]

***************************************

FUNCTION lfDoPhys
PARAMETERS lcRI
PRIVATE lcRI
lnRet = .F.

DO CASE
CASE lcRI = 'I'
  *-- Issue if the old stock value or the old stock qty doen't equal zero
  *-- or the cost value has changed or the balance has changed.
  lnRet = lnOldSVal   # 0 OR ;
    laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
    laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR ;
    lnWOldCst   # lnNewCost   OR ;
    laOldstk[9] # laAdjStk[9]

CASE lcRI = 'R'
  *-- Receive only if the Issue record hasn't been issued or
  *-- the new balance is not zero or cost value has changed
  lnRet = !(lnOldSVal   # 0           OR ;
    laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
    laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR ;
    lnWOldCst   # lnNewCost   OR ;
    laOldstk[9] # laAdjStk[9]      ) OR ;
    laAdjStk[9] # 0                  OR ;
    lnWOldCst   # lnNewCost
ENDCASE

RETURN lnRet

*******************************************************************

FUNCTION lfLkAdjRec

*B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(START)
IF TYPE('loFormSet') ='O' AND ASCAN(loFormSet.laEvntTrig,PADR('STYADJREC',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
  =loFormSet.mDoTrigger(PADR('STYADJREC',10))
  RETURN
ENDIF
*B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(END)

PRIVATE lcJourTag,lnCurAlias,;
  lnTotQty,lnTotVal,lnTranVal,lnDiffere

lnCurAlias = SELECT(0)

SELECT StyInvJl
lcJourTag = ORDER('StyInvJl')

*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
*SET ORDER TO StyInvJl
=gfSetOrder('STYINVJL')
*E304018,1 SAH CONVERT STYINVJL TO SQL [END]

lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0

*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
*IF SEEK(lcStyle+lcWareCode,'StyInvJl')
IF gfSeek(lcStyle+lcWareCode,'StyInvJl')
*E304018,1 SAH CONVERT STYINVJL TO SQL [END]

  SCAN REST WHILE STYLE+cWareCode+cSession+DTOS(dTrDate)+cTrCode = ;
      lcStyle+lcWareCode ;
      FOR   cDyelot = lcSDyelot AND !lLockFlg
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
    *B608791,1 MMT 01/26/2009 fix bug of numeric overflow[Start]
    *lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
    *B608791,1 MMT 01/26/2009 fix bug of numeric overflow[End]
  ENDSCAN

  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn,;
      STYLE      WITH lcStyle,;
      cWareCode  WITH lcWareCode,;
      cDyelot    WITH lcSDyelot,;
      dTrDate    WITH laLockInfo[11],;
      cTrType    WITH lcTrType,;
      cTrCode    WITH IIF(EMPTY(lcTrCode),lcRISessn,lcTrCode),;
      cIRType    WITH IIF(lnDiffere<0,'I','R'),;
      nStkVal    WITH lnDiffere   ,;
      REFERENCE  WITH 'Mark Down Adjustement Value',;
      cAdjReason WITH lcAdjCdRsn ,;
      cAdjAcct   WITH lcAdjAcct  ,;
      cISession  WITH IIF(lnDiffere<0,cSession,''),;
      cRSession  WITH IIF(lnDiffere>0,cSession,''),;
      nPrvSQty   WITH lnPrvQty,;
      nPrvSVal   WITH lnPrvVal,;
      cAdjRef    WITH lcAdjRef
      
    *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [BEGIN]  
    Replace OID with gfTempName()
  	*E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  	lnQueryRslt = oAriaApplication.RemoteCompanyData.SqlRun("select NEWID() AS NEWOID",'TMPNEWID','TMPNEWID',oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
  	SELECT STYINVJL
  	IF lnQueryRslt > 0
    	REPLACE CENTRYID WITH TMPNEWID.NEWOID
    ELSE
    	REPLACE CENTRYID WITH ALLTRIM(SYS(2007,SYS(0)))+SUBSTR(SYS(2015),4)
    ENDIF 	
	  *E611837,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
    =gfReplace('')
    *E304018,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]

    =lfUpdGLDist(.F.,.T.)
    =lfStyWarDy()
  ENDIF
ENDIF

*E304018,1 SAH CONVERT STYINVJL TO SQL [BEGIN]
*SET ORDER TO (lcJourTag) IN StyInvJl
Select STYINVJL
=gfSetOrder(lcJourTag)
*E304018,1 SAH CONVERT STYINVJL TO SQL [END]



SELECT (lnCurAlias)

***
