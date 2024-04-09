*:************************************************************
*: Program file  : SrTrn.prg
*: Program desc. : Sales Representative Payments, 
*:                 Sales Representative Debit Adjustment,
*:                 Sales Representative Credit Adjustment.
*: For screen    : Srpay
*:        System : Aria Advantage Series.
*:        Module : Sales Representative (SR).
*:     Developer : Ahmed Amer (AHM)
*:************************************************************
*: Calls : 
*:         Procedures : 
*:         Functions  : 
*:************************************************************
*: Passed Parameters  : lcProgMode...
*:                      'P' => Rep. Payment menu item
*:                      'C' => Rep. Credit Adjustement menu item
*:                      'D' => Rep. Debit  Adjustement menu item
*:************************************************************
*: Example : 
*:************************************************************
*: Modifications : 
*: B802720,1 11/10/1999 AHM Fix the bug of showing paid transaction with
*: B802720,1                "No" Status
*: B802935,1 01/11/2000 AHM Fix the bug of not updating status field in repcomm file 
*: B802935,1                with "P" although the commission was paid
*: B802984,1 01/02/2000 SSH Update PAYDATE WITH batch Date.
*: B803602,1 10/23/2000 MHM Update fields cCurrCode,nForAmnt,nExRate,nCurrUnit in repcomm
*: B603713,8  MHM 11/09/2000 increase the length of the fields (amount)
*: C200176,1  03/21/2001 MHM Apply partially paymnet commission
*: B605943,1 ADEL 07/04/02 Fixed the bug of not updating the status in RecComm file with 
*: B605943,1               "P" for payed invoices in case of Consulidated invoice. 
*: B804347,1 ADEL 07/16/02 Assure that you collect all the transactions for the salesrep
*: B804347,1               when Mamual payment by Cuttoff date and All. 
*: B606734,1 ABDOU 12/18/2002 We searched for this bug and we found it fixed before. 
*: B606372,1 REHAM 01/15/2003 Give the ability to enter more than one transaction for the
*: B606372,1 same salesrep. with type manual payment more than once in the same session
*: B122143,1 NNA 04/05/2004 Fix bug of problem with sales rep. commission report when we input the sales rep.
*: B122143,1 NNA            payments and delete it more than time without go out of the screen, in this case the
*: B122143,1 NNA            invoice payment status still open even we payed
*: B130751,1 EIH 03/16/2006 Fix bug of wrong commission in case of consolidated invoice.
*:******************************************************************************************
PARAMETERS lcProgMode

*-- To force the control pannel to call the local save 
*-- 'procedure lpSavScr'
laDefproc[9] = .F.

*-- To force the control pannel to call the local close 
*-- 'procedure 'lpClsScr'
laDefProc[10] = .F.
*: C200176,1  03/21/2001 MHM [start]
*--Define Needed Variables
STORE 0 TO lcAmount , lcTotAmt 
STORE .T. TO llRrturn
STORE '' TO lcTrNo
*: C200176,1  03/21/2001 MHM [End]
*-- lcsrpay1  : This is to hold the name of the header screen
*-- lcsrpay2  : This is to hold the name of the Browse screen
*-- lcsrpay3  : This is to hold the name of the repcode payment information screen
*-- lcsrpay4  : This is to hold the name of the cut-off date screen
*-- lcsrpay5  : This is to hold the name of the transaction screen
*-- lcSrpay6  : This is to hold the name of the new & remove button screen
*-- lcTmpRep  : This is to hold the name of the browse tempfile 
*-- lcTmpComm : This is to hold the name of the tempfile for manual payment Reps.
*-- lcRepCode : This is to hold the value of the Repcode
*-- lcRepName : This is to hold the value of the Repname
*-- lcDesc    : This is to hold the value of the Description of the transaction
*-- lcFiles   : This is to hold the name of temp. files to be updated in 
*--             Uncmsess file
*: C200176,1  03/21/2001 MHM [start]
*STORE SPACE(0) TO lcsrpay1 ,lcsrpay2,lcsrpay3, lcsrpay4,lcsrpay5,lcSrpay6,;
*     		      lcTmpRep ,lcTmpComm,lcRepCode,lcRepName,lcDesc,lcTmpRep,lcFiles 

STORE SPACE(0) TO lcsrpay1 ,lcsrpay2,lcsrpay3, lcsrpay4,lcsrpay5,lcSrpay6,;
     		      lcTmpRep ,lcTmpComm,lcRepCode,lcRepName,lcDesc,lcTmpRep,lcFiles ,lcTmpPay
*: C200176,1  03/21/2001 MHM [End]

*-- lnTrCont  : This is to hold the count of the batch
*-- lnTapTot  : This is to hold the amount of the batch
*-- lnActual  : This is to hold the actual amount yielded from sum browse amount fields
*-- lnDiff    : This is to hold the difference between tape amount and actual amount 
*-- lnAmt     : This is to hold the amount for each transaction
*-- lnTrnRecNo: This is to hold the record number of the tempfile for the browse to be used with marker
*-- lnRepCount: This is to hold the count of the rep in tmp file lctmprep
*-- lnManual4 : This is to hold the value of the manual payment popup in the cut-off date screen
*-- lnManual  : This is to hold the value of the manual payment popup in the transaction screen
*-- lnChoice  : This is to hold the choice of the message that ask about ;
                paid invoices only or all invoices in case manual payment

STORE 0 TO lnTrCont,lnTapTot,lnActual,lnDiff, lnAmt,lnTrnRecNo,lnRepCount,;
           lnManual4,lnManual,lnChoice 

*-- llBrowse  : This is .T. if push button of the repcode is clicked
*-- llShowPop : This is .T. if the rep. payment is manual and it is used ;
                 to make the screens lcsrpay4,lcsrpay5 visible or invisible
*-- llWExist  : This is .T. if the screen is exist                 
*-- llChkUnCm : Can we go and check the existance of any incomplete session
*--             from the "lpShow" procedure?
*-- llSetupChk: This is to show if uncmp. was checked from setup
STORE .F. TO llBrowse,llShowPop,llChecked,llWExist,llChkUnCm,llSetupChk


ldCutoff  = {}                   && this is to hold the date of the cutof date
ldBatch   = gdSysDate            && this is to hold the date of the batch date
lcBrTtl   = 'Payments'           && This is to hold the title of the browse screen
lnSessNo  = gnProgCopy           && This is to hold the no of the opened session form global variable
lcSession = SPACE(1)             && This is to hold the session id for the uncompleted session
lcUserID  = PADR(gcUser_id, 10)  && This is to hold the user id 

DIMENSION laFileStru[1]          && This array for creating the temp files
STORE SPACE(0) TO laFileStru

lcPopStat = "DISABLE"           && This is to hold the enable status for the manual payment popup
lcDatStat = "DISABLE"           && This is to hold the enable status for the cut-off date
lcTrnStat = "DISABLE"           && This is to hold the enable status for the transaction push button
lcAmtStat = "DISABLE"           && This is to hold the enable status for the amount control

lnUnCmSeRc = 0                  && This is to hold the uncomplete session record no in the "uncmsess" file
lnStep     = 0                  && This is to hold the save step number 
lcScrMode  = ""                 && This is to hold the screen mode to be used in the uncomplete session 
llCupdate  = .F.                && This is .T. if the updating for any field is happened

*-- Variables that hold the red and green colors used to display the 
*-- "Difference" object on the screen.
lcRedClr   = "RGB(,,,255,0,0)"
lcGreenClr = "RGB(,,,0,128,0)"

*-- Variables that need to be saved and restored when detecting an
*-- uncomplete program session.

DIMENSION laVars[12]
laVars[01] = "lcScrMode"
laVars[02] = "ldBatch"
laVars[03] = "lnTrCont"
laVars[04] = "lnTapTot"
laVars[05] = "lnActual"
laVars[06] = "lnDiff"
laVars[07] = "lcRepCode"
laVars[08] = "lnManual"
laVars[09] = "lnTrnRecNo"
laVars[10] = "lnStep"
laVars[11] = "lnUnCmSeRc"
laVars[12] = "llCupdate"

IF !gfSetup()
  RETURN
ENDIF  

*-- lcProgId is used to be stored in the "uncmsess" file
DO CASE
  CASE lcProgMode = 'P'
    lcProgID = PADR("SR_PAY",10)
  CASE lcProgMode = 'D'
    lcProgID = PADR("SR_DR", 10)
  CASE lcProgMode = 'C'
    lcProgID = PADR("SR_CR", 10)
ENDCASE

laCtrStat[10]  = "DISABLE"       && Disable BROWSE button in control panel

*-- To control the "Control Panel" show. to execute the lpshow once you enter the prog.
llNoShow     = .F.

IF !WEXIST(gcBaseWind)
  lcSRPay1 = gfTempName()
  lcSRPay2 = gfTempName()
  lcSRPay3 = gfTempName()
  lcSRPay6 = gfTempName()

  *-- The following two variables will hold the name of the two windows 
  *-- that will be used to display the payment information if the sales 
  *-- rep. uses manual payment method, the displayed window will be 
  *-- determined according to the sales rep manual payment type, if the
  *-- payment is using "Cut-Off Date"; "lcSRPay4" will be displayed and 
  *-- the other window will be HIDDEN, and vice versa, in case of not 
  *-- using manual payment none of the two windows will be displayed 
  *-- (the two of them will be HIDDEN).
  *-- When switching these two windows, the program runs the deactivate
  *-- snippet of the base window which calls the global function
  *-- gfStopRead. In this case, the "gfStopRead" acts as if the "Close"
  *-- button from the upper left corner of the screen was clicked, and 
  *-- it displays the message to save the information before closing
  *-- the screen. To avoid this case, we had to force the "gfStopRead"
  *-- to deal with these two windows as child screens called from the
  *-- base window (Separate screens inside the main screen set).
  *-- To do so we had to have the character "C" as a prefix to the 
  *-- window name.
  lcSRPay4  = "C" + gfTempName()
  lcSRPay5  = "C" + gfTempName()

  lcTmpRep  = gfTempName()
  lcTmpComm = gfTempName()


  *-- Get a session number, to be updated in the uncomplete session
  *-- number file foe each session.
  lcSession  = gfsequence('CSESSION')

  llWExist = .T.

ENDIF

PUSH KEY                                     && To save the the current keys functions.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl) && To activate the browse screen when pressing ALT+B
DO (gcScrDir + gcWinAppl + '\SRTRN.SPR')     && Run the main screen.
RELEASE WINDOW (lcBrTtl)
POP KEY                                      && Restore the origenal keys functions.

SELECT REPCOMM
SET RELATION TO

*-- If we realy quitting the screen 
IF glQuitting        
  IF USED(lcTmpRep)
    USE IN (lcTmpRep)
  ENDIF
  ERASE (gcWorkDir+lcTmpRep+".DBF")
  ERASE (gcWorkDir+lcTmpRep+".CDX")
  IF USED(lcTmpComm)
    USE IN (lcTmpComm)
  ENDIF
  ERASE (gcWorkDir+lcTmpComm +".DBF")
  ERASE (gcWorkDir+lcTmpComm +".CDX")
ENDIF

*!*************************************************************
*! Name      : lfPayBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Browse the payment of the Rep
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfwBrow(),lfvBrow()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPayBrow()
*!*************************************************************

FUNCTION lfPayBrow
PRIVATE lnAlias

lnAlias   = SELECT(0)
*: B603713,8  MHM 11/09/2000 [start]
*lcBrowStr = "cMarker=IIF(RECNO(lcTmpRep)=lnTrnRecNo,'>',' '):1:H=' ':W=.F.,"+; 
            "REPCODE :5  :R :H='Rep',"+;
            "RepName :30 :R :H='Salesrep Name',"+;
            "DESC    :50 :R :H='Description',"+;
            "AMOUNT  :10 :R :P='9999999.99' :H='Amount'"
lcBrowStr = "cMarker=IIF(RECNO(lcTmpRep)=lnTrnRecNo,'>',' '):1:H=' ':W=.F.,"+; 
            "REPCODE :5  :R :H='Rep',"+;
            "RepName :30 :R :H='Salesrep Name',"+;
            "DESC    :50 :R :H='Description',"+;
            "AMOUNT  :13 :R :P='9999999999.99' :H='Amount'"
*: B603713,8  MHM 11/09/2000 [end]

SELECT (lcTmpRep)
BROWSE FIELDS &lcBrowStr   ;
         SAVE              ;
         NOWAIT            ;
         NOAPPEND	 	   ;
         NODELETE          ;
         NOEDIT            ;
         NOMENU            ;
         NOCLEAR           ;
         TITLE (lcBrTtl)   ;
         WHEN lfwBrow()    ;
         VALID :F lfvBrow();         
         WINDOW (lcSrpay2) ;
         IN WINDOW (gcBaseWind)         

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() 
*!             function
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrow()
*!*************************************************************

FUNCTION lfVBrow

IF !INLIST(WONTOP(),lcBrTtl)
  glFromBrow = .T.
  llNoThing  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : 
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfRefresh(),lfUpdUnCmS()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow()
*!*************************************************************

FUNCTION lfwBrow
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcPopStat = "DISABLE"
lcDatStat = "DISABLE"
lcTrnStat = "DISABLE"
lcAmtStat = "DISABLE"

SELECT (lcTmpRep)
IF EOF() OR BOF() OR DELETED()
   SHOW GET pbRem DISABLE
   llShowPop = .F.
   lcRepCode = SPACE(0)
   lcRepName = SPACE(0)
   lcDesc    = SPACE(0)
   lcRepStat = "DISABLE"
   lcNrmStat = "DISABLE"
   lnAmt     = 0
   ldCutoff  = {} 
ELSE
   SHOW GET pbRem ENABLE
   IF EMPTY(RepCode)
     lcRepStat  = "ENABLE"
     lcNrmStat  = "DISABLE"
     llShowPop  = .F.
     lcPayUsing = SPACE(0)
     lcRepCode  = SPACE(0)
     lcRepName  = SPACE(0)
     lcDesc     = SPACE(0)
     lnAmt      = 0
     ldCutoff   = {}     
   ELSE
     lcRepCode  = RepCode
     lcRepName  = IIF(SEEK(RepCode, "SalesRep"), SalesRep.Name, SPACE(0))
     lcDesc     = Desc
     lnAmt      = Amount
     ldCutoff   = Cutdate
     lcRepStat  = "DISABLE"
     lcNrmStat  = "ENABLE"
     lcAmtStat  = "ENABLE"
     lcPayUsing = ManType
     llShowPop  = !EMPTY(lcPayUsing)
     
     IF llShowPop
       IF lcPayUsing = "N"
         lcPopStat = "ENABLE"
         lcDatStat = "DISABLE"
         lcTrnStat = "DISABLE"
       ELSE
         lcAmtStat = IIF(SEEK(RepCode,lcTmpComm),"DISABLE","ENABLE")
         lcPopStat = "DISABLE"
         lcDatStat = IIF(EMPTY(ldCutoff),"ENABLE","DISABLE")
         IF lcPayUsing = "D"
           lcTrnStat = IIF(EMPTY(ldCutoff),"DISABLE","ENABLE")
         ELSE
           lcTrnStat = "ENABLE"
         ENDIF
       ENDIF
     ENDIF
   ENDIF
ENDIF

= lfShowMan(ManType = "D", ManType $ "TN")
SHOW GET pbRepBrow &lcRepStat
SHOW GET lcRepCode &lcRepStat
SHOW GET lcRepName &lcNrmStat
SHOW GET lcDesc    &lcNrmStat
SHOW GET lnAmt     &lcAmtStat

lnTrnRecNo = RECNO()
llNoThing = lfRefresh()
SHOW WINDOW (lcBrTtl) REFRESH SAME
llNoThing = lfUpdUnCmS("Open", "ibNothing" )
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfShowMan
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : To make the objects releated to Manual payment 
*!             visible or invisible
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfShowMan()
*!*************************************************************

FUNCTION lfShowMan
PARAMETERS llShow4, llShow5
PRIVATE lcWONTOP

lcWONTOP = WONTOP()

IF llShow4
  STORE 1 TO lnManual,lnManual4 
  SHOW WINDOW (lcSRPay4) SAME
  SHOW GET lnManual4  &lcPopStat
  SHOW GET ldCutoff   &lcDatStat
  SHOW GET pbTrn4     &lcTrnStat
ELSE
  SHOW GETS WINDOW (lcSrpay4) DISABLE ONLY
  HIDE WINDOW (lcSrpay4) SAME
ENDIF

IF llShow5
  lnManual = IIF(ManType = "N", 3, 2)
  SHOW WINDOW (lcSRPay5) SAME
  SHOW GET lnManual   &lcPopStat
  SHOW GET pbTrn      &lcTrnStat
ELSE
  SHOW GETS WINDOW (lcSrpay5) DISABLE ONLY
  HIDE WINDOW (lcSrpay5) SAME
ENDIF

IF !EMPTY(lcWONTOP) AND lcWONTOP == lcBrTtl
  ACTIVATE WINDOW (lcWONTOP)
ENDIF

*!*************************************************************
*! Name      : lfvKeys
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Check if any of the screen keys is empty
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvKeys()
*!*************************************************************

FUNCTION lfvKeys

IF !EMPTY(ldBatch) AND !EMPTY(lnTrCont) AND !EMPTY(lnTapTot)
  laScrMode    = .F.
  laScrMode[4] = .T.                     && Select, View, Edit, Add
  _CUROBJ = OBJNUM(pbNew)
  lnDiff   = lnTapTot - lnActual
  SHOW GETS 
ENDIF

llNoThing = lfUpdUnCmS("Open", VARREAD() )

*!*************************************************************
*! Name      : lfvNew
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : To add new record to lcTmpRep file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNew()
*!*************************************************************

FUNCTION lfvNew
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpRep)
IF !SEEK(SPACE(3))
  APPEND BLANK
  lnRepCount = lnRepCount + 1
ENDIF
= lfwBrow()
_CUROBJ = OBJNUM(lcRepCode)

SELECT (lnAlias)
llNoThing = lfUpdUnCmS("Open", VARREAD() )

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : To remove record from lcTmpRep file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRemove()
*!*************************************************************

FUNCTION lfvRemove
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpRep)
lcORep    = lcRepCode
llNoThing = lfRefresh()
*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("QRM00002B36001","Dialog","Delete") = 1
  lnActual  = lnActual - Amount  
  lnDiff    = lnTapTot - lnActual
  DELETE
  GOTO TOP
  llNoThing = lfwBrow()
  SELECT (lcTmpComm)
  IF SEEK (lcORep)
    DELETE REST WHILE REPCODE = lcRepCode
  ENDIF  
  lnRepCount = lnRepCount - 1
  SELECT (lnAlias)
ENDIF
llNoThing = lfUpdUnCmS("Open", VARREAD() )

*!*************************************************************
*! Name      : lfvRep
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRep()
*!*************************************************************

FUNCTION lfvRep
PRIVATE lnAlias
IF !( (EMPTY(lcRepCode) OR LASTKEY() <> 13) AND !llBrowse)
  lnAlias = SELECT(0)
  SELECT (lcTmpRep)
  IF llBrowse OR !SEEK(lcRepCode, "SALESREP")
    XREPCODE = lcRepCode
    DO REPCHK WITH XREPCODE, .T.
    lcRepCode = XREPCODE
    llBrowse = .F.
  ENDIF
  IF !EMPTY(lcRepCode)
    *B606372,1 Reham On 01/15/2003   *** Begin ***
    *B606372,1 Allow to enter same salesrep. more than once if he has type manual
    *IF SEEK(lcRepCode,lcTmpRep) AND lcProgMode = 'P'
    *  *-- "Salesrep already entered! cannot add a second entry.."
    *  *-- <Ok>
    *   llNoThing = gfModalGen("INM30008B36000","Dialog",lcRepCode)  
    *  lcRepCode = SPACE(3)
    *  _CUROBJ   = _CUROBJ
    *ELSE
    *B606372,1 Reham On 01/15/2003   *** End   ***
      = SEEK(SPACE(3), lcTmpRep)
      REPLACE RepCode WITH lcRepCode,;
              RepName WITH SalesRep.Name,;
              ManType WITH IIF(lcProgMode = "P",IIF(SALESREP.Pay_Type = "M", "N", SPACE(0)),SPACE(0))
      llNoThing = lfUpdUnCmS("Open", VARREAD() )
    *REHAM B
    *ENDIF
    *REHAM E
    = lfwBrow()
  ENDIF
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfvDesc
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDesc()
*!*************************************************************

FUNCTION lfvDesc
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpRep)
REPLACE Desc WITH lcDesc
SHOW WINDOW (lcBrTtl) REFRESH SAME
SELECT (lnAlias)

llNoThing = lfUpdUnCmS("Open", VARREAD() )

IF lcProgMode = "P" AND SALESREP.Pay_Type = "M"
  _CUROBJ = OBJNUM(lnManual)    
ENDIF

*!*************************************************************
*! Name      : lfvAmt
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAmt()
*!*************************************************************

FUNCTION lfvAmt
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpRep)
lnActual  = lnActual - &lcTmpRep..Amount + lnAmt
lnDiff    = lnTapTot - lnActual
REPLACE Amount WITH lnAmt
SHOW WINDOW (lcBrTtl) REFRESH SAME
llNoThing = lfRefresh()
SELECT (lnAlias)

llNoThing = lfUpdUnCmS("Open", VARREAD() )

*!*************************************************************
*! Name      : lfvCutDate
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCutDate()
*!*************************************************************

FUNCTION lfvCutDate
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT (lcTmpRep)
REPLACE Cutdate WITH ldCutoff
SELECT (lnAlias)

lnChoice = 0

IF EMPTY(ldCutoff)
  SHOW GET pbTrn4   DISABLE
ELSE  
  SHOW GET pbTrn4   ENABLE
  SHOW GET ldCutoff DISABLE  
  
  *-- All invoices or only the paid ones ?
  *-- < All > < Paid >
  lnChoice = gfModalGen("QRM30007B30000","Dialog")
  
  *B802720,1 (Start)
  *Remove lnChoice Parameter because there is no use to this parameter
  *= lfCollectData(lcRepCode,lnManual,ldCutoff,lnChoice)
  = lfCollectData(lcRepCode,lnManual,ldCutoff)
  *B802720,1 (End)
ENDIF

llNoThing = lfUpdUnCmS("Open", VARREAD() )

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************

PROCEDURE lpShow
PRIVATE lnAlias

lnAlias = SELECT(0)

DO CASE  
  CASE laScrMode[1]
    lcScrMode  = "S"
    
    IF llChkUnCm AND lfChkUnCmS()
      RETURN
    ENDIF

    lcFiles = "lcTmpRep,"  + lcTmpRep  + "," + ORDER(lcTmpRep)  + ";" + ;
              "lcTmpComm," + lcTmpComm + "," + ORDER(lcTmpComm) + ";"

    llShowPop = .F.
    STORE 0 TO lnTrCont,lnTapTot,lnActual,lnDiff
    lnRepCount = 0
    STORE SPACE(0) TO lcRepCode,lcRepName,lcDesc
    STORE 0 TO lnAmt,lnTrnRecNo  
    STORE .F. TO llBrowse
    ldCutoff  = {}
    ldBatch   = gdSysDate
    SELECT (lcTmpRep)
*   ZAP
    llNoThing = lfPayBrow() AND lfRefresh()
    SELECT (lcTmpComm)
*   ZAP
    lnUnCmSeRc = 0

   CASE laScrMode[4]                    && Add Mode
     *-- Create an "Open" record in the uncomplete session file.
     lcScrMode  = "A"
     llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.) 
     llChkUnCm  = .T.
ENDCASE

= lfwBrow()
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvManType
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvManType()
*!*************************************************************

FUNCTION lfvManType
PRIVATE lnAlias

lnChoice = 0 
lcVarName = SYS(18)
STORE &lcVarName TO lnManual4, lnManual

IF lnManual < 3
  lnAlias = SELECT(0)
  SELECT (lcTmpRep)
  REPLACE ManType WITH IIF(lnManual = 1, "D", "T")
  lcPopStat = "DISABLE"
  lcDatStat = "ENABLE"
  lcTrnStat = IIF(lnManual = 1, "DISABLE", "ENABLE") 
  = lfShowMan (lnManual=1, lnManual#1)
 
  IF lnManual = 2
    *-- All invoices or only the paid ones ?
    *-- < All > < Paid >
    lnChoice = gfModalGen("QRM30007B30000","Dialog")
    *B802720,1 (Start)
    *Remove lnChoice Parameter because there is no use to this parameter
    *= lfCollectData(lcRepCode,lnManual,ldCutoff,lnChoice)
    = lfCollectData(lcRepCode,lnManual,ldCutoff)
    *B802720,1 (End)
  ELSE
    ACTIVATE WINDOW (lcSRPay4)
    _CUROBJ = OBJNUM(ldCutoff)
  ENDIF
  SELECT (lnAlias)
ENDIF  

llNoThing = lfUpdUnCmS("Open", VARREAD() )

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpSavscr()
*!*************************************************************

PROCEDURE lpSavscr
PRIVATE llReturn,lnTotalRec,lnCurrRec
PRIVATE lnAmtCh    && To hold the choice of saving or modifying if amount is ZERO
lnAmtCh = 0        

llReturn = .T.
*-- This means there is only record in lctmprep file and it is empty
IF lnRepCount <= 1 AND EMPTY(&lcTmpRep..RepCode) 
  llReturn = .F.
  *-- No Transactions made, can not proceed
  *-- Ok
  = gfModalGen("INM30015B36000","Dialog")
  _CUROBJ = OBJNUM(lcRepCode)
ELSE
  IF lnRepCount # lnTrCont
    *-- "Actual count not equal Tape count."
    *-- <PROCEED> , <MODIFY> 
    llReturn = gfModalGen("QRM30009B30001","Dialog") = 1
    _CUROBJ = OBJNUM(pbNew)
  ENDIF

  *: C200176,1  03/21/2001 MHM triger to check Validation[start]
  *IF llReturn AND lnDiff # 0
  *  *-- "Actual amount not equal Tape amount."
  *  *-- <PROCEED> , <MODIFY> 
  *  llReturn = gfModalGen("QRM30010B30001","Dialog") = 1
  * _CUROBJ = OBJNUM(pbNew)
  *ENDIF 
  
  
  IF ASCAN(laEvntTrig , PADR('CHCKVALD',10)) <> 0
    =gfDoTriger('SRPAY',PADR('CHCKVALD',10)) 
    IF !llRrturn
      llReturn = .F.
    ENDIF
  ELSE
    IF llReturn AND lnDiff # 0
      *-- "Actual amount not equal Tape amount."
      *-- <PROCEED> , <MODIFY> 
      llReturn = gfModalGen("QRM30010B30001","Dialog") = 1
     _CUROBJ = OBJNUM(pbNew)
    ENDIF 
  ENDIF  
  *: C200176,1  03/21/2001 MHM triger to check Validation[End]
  
  SELECT(lcTmpRep)
  GOTO TOP
  IF llReturn
    SCAN FOR !EMPTY(RepCode)
      lnAmtCh   = 0
      IF AMOUNT = 0 
        *-- Transaction for sales Rep. XXXXXX has zero amount, Transaction
        *-- with zero amount will be ignored.
        lnAmtCh = gfModalGen("QRM30018B30001","Dialog",RepCode) 
        IF lnAmtCh = 2
          llReturn = .F.
          = lfwBrow()
          _CUROBJ = OBJNUM(lnAmt)
          EXIT
        ENDIF
      ENDIF
      IF lnAmtCh = 0
        IF ManType = "N" 
          *-- Manual payment method should be selected for SalesRep 
          *-- <OK>
          llNoThing = gfModalGen("INM30011B36000","Dialog",RepCode)                    
          llReturn = .F.
          =lfwBrow()
          _CUROBJ = OBJNUM(lnManual)
          EXIT
        ELSE
          IF ManType = "D" AND EMPTY(CutDate)
            *-- Cut-off Date should be entered for SalesRep
            *-- <OK>
            llNoThing = gfModalGen("INM30012B36000","Dialog",RepCode)
            llReturn = .F.
            =lfwBrow()
            _CUROBJ = OBJNUM(ldCutoff)
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

*B605943,1 (Begin) Open Invoice Header.
=gfOpenFile(gcDataDir+'INVHDR', gcDataDir+'INVHDR','SH')
SELECT (lcTmpRep)
*B605943,1 (End)

IF llReturn
  lnTotalRec = RECCOUNT(lcTmpRep)
  lcFxdMsg   = "Files are now being updated ..."
  lcVarMsg   = ''
  lnCurrRec  = 0
  lnStep     = 0
 
  llNoThing = lfUpdUnCmS("Open", VARREAD() )
  SCAN FOR !EMPTY(RepCode) AND AMOUNT # 0
    lnStep = 0
    lnCurrRec = lnCurrRec + 1
    = gfThermo(lnTotalRec,lnCurrRec,lcFxdMsg,lcVarMsg)
    lnAmount = IIF(lcProgMode='C',Amount,-1*( ABS(Amount) ) )
    IF EMPTY(nStep)
      IF SEEK(RepCode,'SALESREP')
        REPLACE SALESREP.Current WITH SALESREP.Current + lnAmount, ;
                SALESREP.Balance WITH SALESREP.Balance + lnAmount
        lcPayType = SALESREP.Pay_Type
      ENDIF
      *-------------------------------------------
      *----- This is for uncomplete session ------
      lnStep =  lnStep + 1
      REPLACE &lcTmpRep..nStep WITH lnStep
      *-------------------------------------------
    ENDIF
    IF nStep = 1
      *lcTrNo    = gfSequence('SALESREP')
      lcTrNo    = gfsequence('SALESREP', gcAct_Comp, "", "", "TRAN")
      
      *lcBatchNo = PADL(gfSequence('BATCH')   ,6,'0')
      lcBatchNo = PADL(gfsequence('REPBATCH',gcAct_Comp,"","","BATCH"),6,'0')
      REPLACE Tran       WITH lcTrNo     ,;
              Date       WITH ldBatch    ,;
              Batch      WITH lcBatchNo  ,;
              dAdd_Date  WITH Date()     ,;
              cAdd_Time  WITH gfGettime(),;
              cAdd_User  WITH gcUser_Id  
      *-------------------------------------------
      *----- This is for uncomplete session ------
      lnStep =  lnStep + 1
      REPLACE &lcTmpRep..nStep WITH lnStep
      *-------------------------------------------
    ENDIF
    IF nStep = 2
      DO CASE
        CASE lcProgMode="P"
          *: B802984,1 01/02/2000 SSH (Start) Update PAYDATE WITH batch Date.
          *REPLACE Amount   WITH -1*(ABS(Amount)),;
                  Trantype WITH '2',;
                  Status   WITH IIF( lcPayType = 'M', 'P'      , 'O'     ),;
                  Paydate  WITH IIF( lcPayType = 'M', gdSysDate, PAYDATE ),;
                  Custpo   WITH '*PAYMENT*'
          *: B803602,1 10/23/2000 MHM Update fields [Start]
          *REPLACE Amount   WITH -1*(ABS(Amount)),;
                  Trantype WITH '2',;
                  Status   WITH IIF( lcPayType = 'M', 'P'      , 'O'     ),;
                  Paydate  WITH IIF( lcPayType = 'M', ldBatch , PAYDATE ),;
                  Custpo   WITH '*PAYMENT*'
          *: B802984,1 01/02/2000 SSH (End)
          REPLACE Amount   WITH -1*(ABS(Amount)),;
                  Trantype WITH '2',;
                  Status   WITH IIF( lcPayType = 'M', 'P'      , 'O'     ),;
                  Paydate  WITH IIF( lcPayType = 'M', ldBatch , PAYDATE ),;
                  Custpo   WITH '*PAYMENT*',;
                  CcurrCode WITH gcBasecurr ,;
                  nCurrUnit WITH 1,;
                  nExRate   WITH 1,;
                  nForAmnt  WITH -1*(ABS(Amount))
                  
          *: B803602,1 10/23/2000 MHM Update fields [End]

        CASE lcProgMode="D"
          *: B803602,1 10/23/2000 MHM Update fields [Start]
          *REPLACE Amount   WITH -1*(ABS(AMOUNT)),;
                   Trantype WITH '3',;
                   Status   WITH 'O'
          REPLACE Amount   WITH -1*(ABS(AMOUNT)),;
                  Trantype WITH '3',;
                  Status   WITH 'O',;
                  CcurrCode WITH gcBasecurr ,;
                  nCurrUnit WITH 1,;
                  nExRate   WITH 1,;
                  nForAmnt  WITH -1*(ABS(Amount))
          *: B803602,1 10/23/2000 MHM Update fields [End]
        CASE lcProgMode="C"
          *: B803602,1 10/23/2000 MHM Update fields [Start]
          *REPLACE Amount   WITH AMOUNT,;
                  Trantype WITH '4',;
                  Status   WITH 'O'
          REPLACE Amount   WITH AMOUNT,;
                  Trantype WITH '4',;
                  Status   WITH 'O',;
                  CcurrCode WITH gcBasecurr ,;
                  nCurrUnit WITH 1,;
                  nExRate   WITH 1,;
                  nForAmnt  WITH AMOUNT
          *: B803602,1 10/23/2000 MHM Update fields [End]
      ENDCASE
      *-------------------------------------------
      *----- This is for uncomplete session ------
      lnStep =  lnStep + 1
      REPLACE &lcTmpRep..nStep WITH lnStep
      *-------------------------------------------
    ENDIF
    IF lnTotalRec > lnCurrRec
      FOR lnCurrRec=lnCurrRec TO lnTotalRec
        =gfTherm(lnTotalRec,lnCurrRec,lcFxdMsg,lcVarMsg)
      ENDFOR
    ENDIF  
    IF !EMPTY(ManType)
      *B606372,1 Reham ON 01/15/2003  *** Begin ***
      *B606372,1 Update the sales rep. related transactions
      SET ORDER TO 'RecNo' IN (lcTmpComm)
      *IF SEEK (&lcTmpRep..RepCode, lcTmpComm)
      IF SEEK (&lcTmpRep..RepCode + ALLTRIM(STR(RECNO(lcTmpRep))) , lcTmpComm)
      *B606372,1 Reham ON 01/15/2003  *** End   ***
        SELECT (lcTmpComm)
        *B606372,1 Reham ON 01/15/2003  *** Begin ***
        *B606372,1 Scan the sales rep. related transactions
        *SCAN REST FOR REPCODE = &lcTmpComm..RepCode AND Flag = "Y" AND EMPTY(nStep)
        SCAN REST FOR REPCODE+CRECNO = &lcTmpRep..RepCode + ALLTRIM(STR(RECNO(lcTmpRep))) AND Flag = "Y" AND EMPTY(nStep)
        *B606372,1 Reham ON 01/15/2003  *** End   ***
          SELECT REPCOMM
          IF SEEK(&lcTmpComm..RepCode+DTOS(&lcTmpComm..DATE)+&lcTmpComm..TRAN+&lcTmpComm..TRANTYPE)

            *B605943,1 (Begin) In case of Consulidated Invoice, the above command will ever get the first
            *B605943,1         record for the consulidated one, So match the order and store too.
            IF SEEK(&lcTmpComm..TRAN,'INVHDR') AND INVHDR.CONSOL = 'Y' ;
                                               AND (REPCOMM.ORDER <> &lcTmpComm..Order;
                                               OR REPCOMM.STORE <> &lcTmpComm..STORE)
               LOCATE REST WHILE repcode+DTOS(date)+tran+trantype = &lcTmpComm..RepCode+DTOS(&lcTmpComm..DATE)+&lcTmpComm..TRAN+&lcTmpComm..TRANTYPE;
                           FOR   REPCOMM.ORDER  = &lcTmpComm..Order;
                           AND   REPCOMM.STORE  = &lcTmpComm..STORE
            ENDIF
            *B605943,1 (End)


            *: B802984,1 01/02/2000 SSH (Start) Update PAYDATE WITH batch Date.
            *REPLACE STATUS  WITH 'P',;
                    PAYDATE WITH gdSysDate
            *: C200176,1  03/21/2001 MHM triger to Change Status [start]
            *REPLACE STATUS  WITH 'P',;
            *        PAYDATE WITH IIF(TranType$'12' AND lcPayType = 'M',ldBatch,gdSysDate)

            IF !(ASCAN(laEvntTrig , PADR('CHNGSTS',10)) <> 0) 
              REPLACE STATUS  WITH 'P',;
                      PAYDATE WITH IIF(TranType$'12' AND lcPayType = 'M',ldBatch,gdSysDate)
            ELSE
              =gfDoTriger('SRPAY',PADR('CHNGSTS',10)) 
            ENDIF          
            *: C200176,1  03/21/2001 MHM triger to Change Status [End]
            *: B802984,1 01/02/2000 SSH (End)
            *-------------------------------------------
            *----- This is for uncomplete session ------
            REPLACE &lcTmpComm..nStep WITH lnStep
            *-------------------------------------------
            SELECT (lcTmpComm)
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    SELECT(lcTmpRep)
    IF nStep = 3
      SCATTER TO laData
      INSERT INTO REPCOMM FROM ARRAY laData
      lnStep = lnStep + 1
      REPLACE &lcTmpRep..nStep WITH lnStep
    ENDIF
  ENDSCAN
  llNoThing = lfUpdUnCmS("Comp", SPACE(0))
  IF lnAmtCh # 1
    *-- Generated batch number is : XXXXXXXX
    *-- < ok >
    lnNoThing = gfModalGen("INM30014B36000", "DIALOG", lcBatchNo)
  ENDIF
ENDIF
*: C200176,1  03/21/2001 MHM triger to Update Payments [start]
IF (ASCAN(laEvntTrig , PADR('UPDATPAY',10)) <> 0) AND llReturn
  =gfDoTriger('SRPAY',PADR('UPDATPAY',10))
ENDIF
*: C200176,1  03/21/2001 MHM triger to Update Payments [End]

llCSave = llReturn

*!*************************************************************
*! Name      : lfCollectData
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Collect the data that appropirate the manual payment criteria
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCollectData()
*!*************************************************************

FUNCTION lfCollectData

*B802720,1 (Start)
*Remove lnAnswer Parameter because there is no use to this parameter
*PARAMETERS lcRep,lnManType,ldCutDate,lnAnswer
PARAMETERS lcRep,lnManType,ldCutDate
*B802720,1 (End)
PRIVATE lnCurAlias,lcFlag
lcSel = "Select"

STORE SPACE(0) TO lcBrRpCom
lcBrTrttl  = "Transactions"
lnBrTrNo   = 0
lnCurAlias = SELECT(0)

lnTotalRec = RECCOUNT('REPCOMM')
lcFxdMsg   = 'Collecting data ...'
lcVarMsg   = ''
lnCurrRec  = 0

*B606372,1 Reham ON 01/15/2003  *** Begin ***
*B606372,1 Set order to the tran # in the temp comm. file
SET ORDER TO 'RepTran' IN (lcTmpcomm)
*B606372,1 Reham ON 01/15/2003  *** End   ***

STORE 0 TO lnCurrRec,lnTotPay

llFromTrn = "PBTRN" $ SYS(18)
IF !llFromTrn

  SELECT REPCOMM
  *-- check in the debit file if the invoice is paid or not
  *B804347,1 (Begin) Sometimes LOCATE doesn't properly work and so work with SEEK.
  *B804347,1         Bug suggests to replace WHILE with AND in the SCAN statement but this will 
  *B804347,1         force scannig all the file records which will take much more time.
  *SEEK lcRepCode
  *LOCATE REST FOR STATUS='O' WHILE REPCODE = lcRepCode
  *IF FOUND()
  IF SEEK(lcRepCode,'REPCOMM')
  *B804347,1 (End)
    lcScanExp = IIF(EMPTY(ldCutoff)," STATUS = 'O' "," STATUS = 'O' .AND. DATE <= ldCutOff ")
    SCAN FOR  &lcScanExp. WHILE REPCODE = lcRepCode
      lnCurrRec = lnCurrRec+1
      =gfThermo(lnTotalRec,lnCurrRec,lcFxdMsg,lcVarMsg)
      SCATTER TO laCommData

      IF lnChoice = 1                           && ALL
        lcSelect  = '»'
        lcFlag    = 'Y'
        IF !EOF('DEBIT')                        && Not paid 
          lcPaid   = 'No'
        ELSE
          lcPaid   = 'Yes'
        ENDIF
        lnTotPay   = lnTotPay + AMOUNT  
      ELSE
        IF !EOF('DEBIT')                        && Not paid
          lcSelect = ''
          lcFlag   = "N"
          lcPaid   = 'No'
        ELSE
          lcSelect = '»'
          *B802935,1 (Start)
          *lcFlag   = "N"
          lcFlag   = "Y"
          *B802935,1 (End)
          lcPaid   = 'Yes'
          lnTotPay = lnTotPay + AMOUNT  
        ENDIF
      ENDIF

      *B606372,1 Reham ON 01/15/2003  *** Begin ***
      *B606372,1 Check if the transaction was entered befor do no eneter it in the temp comm file
      IF !SEEK(repcode+DTOS(date)+tran+trantype , lcTmpcomm )
      *B606372,1 Reham ON 01/15/2003  *** End   ***
        INSERT INTO (lcTmpComm) FROM ARRAY laCommData
        SELECT(lcTmpComm)
        REPLACE FLAG    WITH lcFlag,;
                CSELECT WITH lcSelect ,;
                CPAID   WITH lcPaid
        *B606372,1 Reham ON 01/15/2003  *** Begin ***
        *B606372,1 Replace the current selected transaction with the current sales rep. record #
        REPLACE CRECNO  WITH ALLTRIM(STR(RECNO(lcTmpRep)))
      ELSE
        *B606372,1 If found the transaction & it's not assigned to any sales rep., assign it to the current sales rep.
        SELECT (lcTmpComm)
        IF EMPTY(cRecno) AND EMPTY(cSelect)
          REPLACE FLAG    WITH lcFlag ;
                  CSELECT WITH lcSelect ;
                  CPAID   WITH lcPaid ;
                  CRECNO  WITH ALLTRIM(STR(RECNO(lcTmpRep)))
        
        
        *B130751,1 EIH 03/16/2006 In this case another line to the same invoice so the invoice is consolidated and 
        *B130751,1 EIH 03/16/2006 We must insert new line to the other store of this invoice.[Begin]
        ELSE
          INSERT INTO (lcTmpComm) FROM ARRAY laCommData
          SELECT(lcTmpComm)
          REPLACE FLAG    WITH lcFlag   ,;
                  CSELECT WITH lcSelect ,;
                  CPAID   WITH lcPaid   ,;
                  CRECNO  WITH ALLTRIM(STR(RECNO(lcTmpRep)))   
        *B130751,1 EIH 03/16/2006 [End]
        
        ENDIF
      ENDIF
      *B606372,1 Reham ON 01/15/2003  *** End   ***
      SELECT REPCOMM
    ENDSCAN
    IF lnTotalRec > lnCurrRec
      FOR lnCurrRec=lnCurrRec TO lnTotalRec
        =gfThermo(lnTotalRec,lnCurrRec,lcFxdMsg,lcVarMsg)
      ENDFOR
    ENDIF  
    *B606372,1 Reham ON 01/15/2003  *** Begin ***
    *lnAmt = lnTotPay
    *SHOW GET lnAmt
    *B606372,1 Reham ON 01/15/2003  *** End ***
  ENDIF
ENDIF

*B606372,1 Reham ON 01/15/2003  *** Begin ***
*B606372,1 Collect the paid amount for 
SELECT (lctmpcomm)
SUM Amount ALL FOR Cselect = '»' AND REPCODE = lcRepCode AND cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))) TO lnTotPay
lnAmt = lnTotPay
= lfvAmt()
SHOW GET lnAmt
*B606372,1 Reham ON 01/15/2003  *** End   ***

IF !SEEK(lcRepCode,lcTmpComm)
   *-- Sales Rep. XXXXX has No Transactions.
   *-- <Ok>
   llNoThing = gfModalGen("INM30013B36000","Dialog",lcRepCode)
   lcAmtStat = "ENABLE"
ELSE
  *B606372,1 Reham ON 01/15/2003  *** Begin ***
  *B606372,1 Search for the un-selected transactions.
  SELECT (lcTmpComm)
  LOCATE FOR (EMPTY(cRecNo) OR cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))))
  IF FOUND()
  *B606372,1 Reham ON 01/15/2003  *** End   ***
    IF llFromTrn OR lnManType = 2
      lcMode = IIF(lnManual = 1, "View", "Edit")
      SELECT (lcTmpComm)
      *: C200176,1  03/21/2001 MHM triger to Call Screen [start]
      *PUSH KEY                                     && To save the the current on key label
      *ON KEY LABEL ALT+T ACTIVATE WINDOW (lcBrTrttl) && To activate the browse screen when pressing ALT+B
      *DO (gcScrDir + gcWinAppl + '\SRREPTR.SPR')
      *POP KEY                                      && To Restore the previous assignments for on key label
      IF (ASCAN(laEvntTrig , PADR('ACTISCR',10)) <> 0) 
        =gfDoTriger('SRPAY',PADR('ACTISCR',10))
      ELSE
        PUSH KEY                                     && To save the the current on key label
        ON KEY LABEL ALT+T ACTIVATE WINDOW (lcBrTrttl) && To activate the browse screen when pressing ALT+B
        
        DO (gcScrDir + gcWinAppl + '\SRREPTR.SPR')
        POP KEY                                      && To Restore the previous assignments for on key label
      ENDIF
      *: C200176,1  03/21/2001 MHM triger to Call Screen [End]
    *B606372,1 Reham ON 01/15/2003  *** Begin ***
    *B606372,1 Collect the amount from the selected transactions.
    ELSE
      SELECT (lctmpcomm)
      SUM Amount ALL FOR Cselect = '»' AND REPCODE = lcRepCode AND cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))) TO lnTotPay
      lnAmt = lnTotPay
      *B606372,1 Reham ON 01/15/2003  *** End   ***
    ENDIF
    = lfvAmt()
    lcAmtStat = "DISABLE"
  *B606372,1 Reham ON 01/15/2003  *** Begin ***
  *B606372,1 If the sales rep. has no trans.
  ELSE
   *** Sales Rep. XXXXX has No Transactions. ***
   *** <  Ok  > ***
   =gfModalGen("INM30013B00000","Dialog",lcRepCode)
   lcAmtStat = "ENABLE"
  ENDIF
  *B606372,1 Reham ON 01/15/2003  *** End   ***
ENDIF  
_CUROBJ = OBJNUM(lnAmt)
SHOW GET lnAmt &lcAmtStat
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvTrn
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvTrn()
*!*************************************************************

FUNCTION lfvTrn

*B802720,1 (Start)
*Remove lnChoice Parameter because there is no use to this parameter
*= lfCollectData(lcRepCode,lnManual,ldCutoff,lnChoice)
= lfCollectData(lcRepCode,lnManual,ldCutoff)
*B802720,1 (End)

llNoThing = lfUpdUnCmS("Open", VARREAD() )

*!*************************************************************
*! Name      : lfTrnBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse the invoice records
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrnBrow()
*!*************************************************************

FUNCTION lfTrnBrow

lnCurAlias = SELECT(0)


lcBrRpCom = "cMarker=IIF(RECNO()=lnBrTrNo,'>',' '):1:H=' ':W=.F.,"+; 
            "CSELECT :1  :W=.F. :R :H=' ',"+;
            "TRAN    :8  :W=.F. :R :H='Trans.',"+;
            "ORDER   :8  :W=.F. :R :H='Order ',"+;
            "DESC    :34 :W=.F. :R :H='Description',"+;
            "ACCOUNT :8  :W=.F. :R :H='Acct#',"+;
            "CPAID   :8  :W=.F. :R :H='Paid',"+;
            "AMOUNT  :10 :W=.F. :R :H='Amount' :P='9999999.99'"

SELECT (lcTmpComm)
GOTO TOP

*B606372,1 Reham ON 01/15/2003  *** Begin ***
*B606372,1 Browse only the current sales rep. selected transactions or the un-assigned transactions
*BROWSE KEY lcRepCode ;
   FIELDS &lcBrRpCom ;
   WINDOW 'TRN2'     ;
   WHEN lfwTrnBrow() ;
   IN WINDOW 'Trn'   ;
   LOCK 0            ;
   NOAPPEND          ;
   NOEDIT            ;
   NOCLEAR           ;
   NODELETE          ;
   NOMENU            ;   
   SAVE              ;
   TITLE lcBrTrttl   ;
   NOWAIT
BROWSE KEY lcRepCode ;
   FOR (cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))) OR EMPTY(cRecNo)) ;
   FIELDS &lcBrRpCom ;
   WINDOW 'TRN2'     ;
   WHEN lfwTrnBrow() ;
   IN WINDOW 'Trn'   ;
   LOCK 0            ;
   NOAPPEND          ;
   NOEDIT            ;
   NOCLEAR           ;
   NODELETE          ;
   NOMENU            ;   
   SAVE              ;
   TITLE lcBrTrttl   ;
   NOWAIT
*B606372,1 Reham ON 01/15/2003  *** End   ***
   
ON KEY LABEL TAB        DO lfTrBrTab
ON KEY LABEL BACKTAB    DO lfBrBack

SELECT (lnCurAlias)               

*!*************************************************************
*! Name      : lfwTrnBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwTrnBrow()
*!*************************************************************
              
FUNCTION lfwTrnBrow

lnCurAlias = SELECT(0)
SELECT (lcTmpComm)
lnBrTrNo = RECNO()

SELECT (lnCurAlias)
SHOW WINDOW (lcBrTrttl) REFRESH SAME
lcSel = IIF(CSELECT='»','Un\<select','\<Select')
SHOW GET pbSel,1 PROMPT lcSel

*!*************************************************************
*! Name      : lfvSel
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSel()
*!*************************************************************
              
FUNCTION lfvSel

lnCurAlias = SELECT(0)
SELECT (lcTmpComm)
REPLACE CSELECT WITH IIF(CSELECT='»','','»'), ;
        FLAG    WITH IIF(FLAG   ='Y','N','Y')

*B606372,1 Reham ON 01/15/2003  *** Begin ***
*B606372,1 Assign/un-assign the current sales rep. to the current selected/unselected transaction
REPLACE cRecNo  WITH IIF(cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))) , "" , IIF(EMPTY(cRecNo) , ALLTRIM(STR(RECNO(lcTmpRep))) , cRecNo))
*B606372,1 Reham ON 01/15/2003  *** End   ***
lnAmt = IIF(CSELECT='»', lnAmt + Amount, lnAmt - Amount)
SHOW GET lnAmt
= lfwTrnBrow()
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvSelAllNo
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcBtn
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSelAllNo()
*!*************************************************************

FUNCTION lfvSelAllNo
PARAMETERS lcBtn
lcSelMark = ''
lcFlag    = 'N'

lnCurAlias = SELECT(0)

SELECT (lcTmpComm)
lcSelMark = IIF(lcBtn = "ALL",'»','')
lcFlag    = IIF(lcBtn = "ALL",'Y','N')
IF lcBtn = "INVERT"
  *B606372,1 Reham ON 01/15/2003  *** Begin ***
  *REPLACE ALL cSelect WITH IIF(cSelect='»','','»') ,;
              FLAG    WITH IIF(FLAG   ='Y','N','Y') ;
              WHILE REPCODE = lcRepCode
  *B606372,1 Assign/un-assign the current sales rep. to the current selected/unselected transaction
  REPLACE ALL cSelect WITH IIF(cSelect='»','','»') ,;
              FLAG    WITH IIF(FLAG   ='Y','N','Y') ;
              cRecNo  WITH IIF(cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))) , "" , IIF(EMPTY(cRecNo) , ALLTRIM(STR(RECNO(lcTmpRep))) , cRecNo)) ;
              WHILE REPCODE = lcRepCode AND (EMPTY(cRecNo) OR cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))))
  *B606372,1 Reham ON 01/15/2003  *** End   ***
ELSE
  *B606372,1 Reham ON 01/15/2003  *** Begin ***
  *REPLACE ALL cSelect WITH lcSelMark ,;
              FLAG    WITH lcFlag     ;
              WHILE REPCODE = lcRepCode
  *B606372,1 Assign/un-assign the current sales rep. to the current selected/unselected transaction
  REPLACE ALL cSelect WITH lcSelMark ,;
              FLAG    WITH lcFlag     ;
              cRecNo  WITH IIF(lcBtn = "ALL" , ALLTRIM(STR(RECNO(lcTmpRep))) , "") ;
              WHILE REPCODE = lcRepCode AND (EMPTY(cRecNo) OR cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))))
  *B606372,1 Reham ON 01/15/2003  *** End   ***
ENDIF

*B606372,1 Reham ON 01/15/2003  *** Begin ***
*B606372,1 Collect the amount of the selected transaction for the current sales rep.
*SUM Amount ALL FOR Cselect = '»' WHILE REPCODE = lcRepCode TO lnAmt
SUM Amount ALL FOR Cselect = '»'  AND REPCODE = lcRepCode AND cRecNo = ALLTRIM(STR(RECNO(lcTmpRep))) TO lnAmt
*B606372,1 Reham ON 01/15/2003  *** End   ***

GOTO TOP
= lfwTrnBrow()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrnTrap()
*!*************************************************************

FUNCTION lfTrnTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTrttl
  ON KEY LABEL TAB     DO lfTrBrTab
  ON KEY LABEL BACKTAB DO lfTrBrBack
  ON KEY LABEL ESC     DO lfTrBrEsc
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfClrTrnTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClrTrnTrap()
*!*************************************************************

FUNCTION lfClrTrnTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfTrBrTab
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfTrBrTab
ON KEY LABEL TAB
ACTIVATE WINDOW Trn3
IF lnManual = 1 
  _CUROBJ = OBJNUM(pbOk)
ELSE
  _CUROBJ = OBJNUM(pbSel)
ENDIF  

*!*************************************************************
*! Name      : lfTrBrBack
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrBack()
*!*************************************************************

FUNCTION lfTrBrBack
ON KEY LABEL BACKTAB
ACTIVATE WINDOW Trn3
IF lnManual = 1 
  _CUROBJ = OBJNUM(pbOk)
ELSE
  _CUROBJ = OBJNUM(pbTrnClose)
ENDIF  

*!*************************************************************
*! Name      : lfTrBrEsc
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrEsc()
*!*************************************************************

FUNCTION lfTrBrEsc
ON KEY LABEL ESC
ACTIVATE WINDOW Trn3
IF lnManual = 1 
  _CUROBJ = OBJNUM(pbOk)
ELSE
  _CUROBJ = OBJNUM(pbTrnClose)
ENDIF  
KEYBOARD "{ENTER}" CLEAR
*!*************************************************************

       *-------------------------------------------------------*
       *-- THIS IS THE TRAPING OF THE MAIN BROWSE -------------*
       *-------------------------------------------------------*

*!*************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrap()
*!*************************************************************

FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key


*-- Set the global flag "glFromBrow" to true only if one of the
*-- screen's browses is active.
glFromBrow = INLIST(WONTOP(),lcBrTtl)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL TAB        DO lfBrTab
  ON KEY LABEL BACKTAB    DO lfBrBack
ENDIF  

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************

FUNCTION lfClrTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 


*-- If going out of one of the browses, call the global function
*-- to stop the browse and reset the global flag.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the 
*-- trapped keys.
IF !INLIST(WONTOP(),lcBrTtl)
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF

*!*************************************************************
*! Name      : lfBrTab
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfBrTab

ON KEY LABEL TAB

ACTIVATE WINDOW (lcSrpay3) 
IF EMPTY(lcRepCode)
  _CUROBJ = OBJNUM(lcRepCode)
ELSE  
  _CUROBJ = OBJNUM(lcDesc)
ENDIF

*!*************************************************************
*! Name      : lfBrBack
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrBack()
*!*************************************************************

FUNCTION lfBrBack
ON KEY LABEL BACKTAB

ACTIVATE WINDOW gwcContrl1
_CUROBJ = OBJNUM(pbCls)

*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Check if there is uncomplete session
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkUnCmS()
*!*************************************************************

FUNCTION lfChkUnCmS

PARAMETERS llFromSetUp

PRIVATE lnAlias, llGoOut, lnReprocess

llGoOut     = .F.

IF !llFromSetUp OR (llFromSetup AND !llSetupChk)

  llSetupChk = .T.
  llChkUnCm  = .F.

  IF gfUnCompSession(lcProgID,lnsessno,"Sales Rep. Payment")
    = lfPayBrow()
    llGoOut   = .T.
    STORE .F. TO laScrMode
    laScrMode[ATC(lcScrMode,"SVEA")] = .T.
    lcSession = UnCmSess.cSession
    lcCurObj  = ALLTRIM(UnCmSess.cCurrObj)
    llNoThing = SEEK (lcRepCode,lcTmpRep)
    _CUROBJ   = OBJNUM(&lcCurObj)
    SHOW GETS
    = lfwBrow()
    IF VARREAD() = 'PBSAV'
      DO lpSavscr
    ENDIF
  ELSE
    =lfCrtUnCmp()
    =lfPayBrow()  
    _CUROBJ = OBJNUM(ldBatch)
  ENDIF
ELSE
  =lfPayBrow()  
ENDIF

RETURN llGoOut

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Adding record in uncomplete session file 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfAdUnCmSR()
*!*************************************************************

FUNCTION lfAdUnCmSR
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT UnCmSess
IF !SEEK('I')
  APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE Status     WITH 'O'      ,;
        cUTranType WITH lcProgID ,;
        cUserId    WITH lcUserID ,;
        cSession   WITH lcSession,;
        cProgram   WITH lcProgID ,;
        cCurrScr   WITH lcProgID ,;
        cCurrObj   WITH VARREAD(),;
        dTranDate  WITH gdSysDate,;
        cTranTime  WITH TIME()                            

llNoThing = RLOCK()

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdUnCmS
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Update the current object and the status of the session,
*!           : variables and temp. files
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdUnCmS()
*!*************************************************************

FUNCTION lfUpdUnCmS
PARAMETERS lcStatus, lcCurObj
PRIVATE lnAlias

IF lnUnCmSeRc <> 0
  lnAlias  = SELECT(0)
  lcStatus = UPPER(LEFT(lcStatus,1))
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH lcCurObj ,;
          Status   WITH lcStatus
  IF Status $ "IC"
    UNLOCK
  ENDIF
  SELECT(lnAlias)          
ENDIF

llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars,lcSession))

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lpClsScr()
*!*************************************************************

FUNCTION lpClsScr

llNoThing = lfUpdUnCmS("Initia", SPACE(0))

*!*************************************************************

FUNCTION lfCrtUnCmp
  SELECT REPCOMM
  = AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
      
  DIMENSION laFileStru[lnFileStru + 4, 4]
     
  *-- This field is to carry the type of the payment if it is manual payment
  *-- "D" cut-off date , "T" transactions , Empty run balance
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'ManType'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
      
  *-- This field to carry the cut-off date
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'CutDate'
  laFileStru[lnFileStru ,2] = 'D'
  laFileStru[lnFileStru ,3] = 8
  laFileStru[lnFileStru ,4] = 0

  *-- This field to carry the number of save step procedure befor unregular termenation
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nStep'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 3
  laFileStru[lnFileStru ,4] = 0

  *-- This field to carry the name of the Rep
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'RepName'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 24
  laFileStru[lnFileStru ,4] = 0

  =gfCrtTmp(lcTmpRep,@laFileStru,[REPCODE],[REPCODE])           

  *B606372,1 Reham ON 01/15/2003  *** Begin ***
  *B606372,1 Increase the array dime. with 1 extra field for record no. of the temp rep comm file (lcTmpRep)
  *DIMENSION laFileStru[lnFileStru + 2, 4]
  DIMENSION laFileStru[lnFileStru + 3, 4]
  *B606372,1 Reham ON 01/15/2003  *** End   ***
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'CSELECT'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
      
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'CPAID'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 3
  laFileStru[lnFileStru ,4] = 0
  
  *B606372,1 Reham ON 01/15/2003  *** Begin ***
  *B606372,1 Add field for record no. to hold the temp rep. record # (lcTmpRep)
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'CRECNO'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 8
  laFileStru[lnFileStru ,4] = 0
  *B606372,1 Reham ON 01/15/2003  *** End   ***
  
  *: C200176,1  03/21/2001 MHM triger to Create Temp File [start]
  IF (ASCAN(laEvntTrig , PADR('CREATTMP',10)) <> 0) 
    =gfDoTriger('SRPAY',PADR('CREATTMP',10))
  ENDIF
  *: C200176,1  03/21/2001 MHM triger to Create Temp File [End]

  *B606372,1 Reham ON 01/15/2003  *** Begin ***
  *=gfCrtTmp(lcTmpComm,@laFileStru,[REPCODE],[REPCODE])
  *-- Ceate array for the rep comm. line indexes.
  DECLARE laIndexArr[2,2]
  laIndexArr[1,1] = [repcode+DTOS(date)+tran+trantype]
  laIndexArr[1,2] = 'RepTran'

  *B122143,1 NNA 04/05/2004 (Begin) change the index to get the recno. value that saved in the Crecno Field
  *laIndexArr[2,1] = [repcode+ALLTRIM(STR(RECNO()))]
  laIndexArr[2,1] = [repcode+ALLTRIM(CRECNO)]
  *B122143,1 NNA (End)
  laIndexArr[2,2] = 'RecNo'
  
  =gfCrtTmp(lcTmpComm,@laFileStru,@laIndexArr)
  SET ORDER TO Tag 'RepTran' IN (lcTmpcomm)
  *B606372,1 Reham ON 01/15/2003  *** End   ***

  SELECT(lcTmpRep)
  *B802720,1 (Start)
  *IF !EMPTY(SET('RELATION'))
  IF EMPTY(SET('RELATION'))
  *B802720,1 (End)
    SET RELATION TO REPCODE INTO SALESREP
  ENDIF

  SELECT REPCOMM
  *B802720,1 (Start)
  *IF !EMPTY(SET('RELATION'))
  IF EMPTY(SET('RELATION'))
  *B802720,1 (End)
    SET RELATION TO account+tran INTO DEBIT ADDITIVE
  ENDIF
  