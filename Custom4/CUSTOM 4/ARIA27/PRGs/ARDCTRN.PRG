*!**************************************************************************
*! Program file        : ARDCTRN.PRG
*! Program description : Menu Option
*! For screen          : ARDCTRN.SPR
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : Account Receivable - (AR)
*! Developer Name      : Walid Abou El-Magd (WAM) Due to E301219,1
*! Last modi           : 
*!**************************************************************************
*! Calls               : ARDCTRN.SPR
*!**************************************************************************
*! Passed Parameters   : None.
*!**************************************************************************
*! Example             : DO ARDCTRN.SPR 
*!**************************************************************************
*! Modification        : 
*! E301534,1 ASH 01/15/2001 Update the Credit/Debit Adjustments reason. 
*! B606739,1 KHM 05/26/2003 Fix the bug of not getting the debit on account
*! B606739,1                or credit on account transactions.
*! B607426,1 KHM 07/16/2003 Fuix the bug of not refreshing the reason description
*! B607426,1                when the user changes the popup of the description.
*!**************************************************************************
*! Note that the transaction types used to update the , 
*!      "DEBIT" and "CREDIT" files are..
*!      Type  Meaning
*!      0     Returns (Credit Memo).
*!      1     Invoice, Direct Invoice.
*!   *  2     Debit Adjustment.
*!   *  3     Charge Back (Debit On Account).
*!      4     Payment.
*!   *  5     Credit Adjustment.
*!   *  6     Credit On Account.
*!      7     Allowance (Credit Adjustment in this program).
*!      8     Charge Back (Only in the ARHIST).
*!      9     Credit On Account (Only in the ARHIST).
*!**************************************************************************

EXTERNAL ARRAY laScrMode,laData,laDefproc
DIMENSION laReson  [1]
DIMENSION laCodeInf [2,10]

*lcBrwFlds  : Variabel that hold the fields to browsed in global brows window 
*lcBrowTitl : Variabel that hold names of brows window
*lcRemStat  : Variabel that hold the state of Radio

*-- Used screen set windows names.
*lcChWin0, lcChWin1, lcChWin2

*lcOldVal      = SPACE(30)
*lcTran        = SPACE(02)
*-- Main temproray file name.
*lcTmpFile : Variable that hold the temproray version of Credit file
STORE SPACE(1) TO lcBrwFlds, lcBrowTitl, lcRemStat, lcChWin0, lcChWin1,;
				  lcChWin2 , lcTmpFile , lcOldVal , lcTran , m.reference , m.desc

*glFromBrow     I'm not comming from any browse.

*laDefProc[9]   Force local < SAVE >

STORE .F. TO glFromBrow, llBrowse, llHasDebit, llHasCredit,; 
			 laDefProc[9]

*lnOldVal       To prevent calling Brows function without changing the selected option 
*lnMarker       Browse marker
*lnRadio        Radio butto  variable

STORE 1 TO lnOldVal, lnMarker, lnRadio , lnDebitPos , lnCreditPos
 
*lcRadState     Variabel that control the ENA/DIS of Radio button 
*lcResState     Variabel that control the ENA/DIS of laReson & Reference field
*laCtrStat[8]   To disabel local <Delete>

STORE 'DISABLE' TO lcRadState, lcResState, laCtrStat[8]

IF !gfSetup()    
  RETURN
ENDIF  

*-- Open the needed files
=gfOpenFile(gcDataDir+'CODES','','SH')
*-- 

lcForCond     = "Account=laData[1]"
lcScFields    = "Account,BtName,Reference"  &&-- laData[1] & laData[2] & laData[3]

*-------------------- Make the suitable filters [BEGIN] -----------------
SELECT CUSTOMER                                         
SET ORDER TO
SET FILTER TO type+account+store = 'M'                    
LOCATE                     &&-- To activate the filter
SET ORDER TO TAG CUSTOMER

SELECT DEBIT                                              
SET ORDER TO TAG DEBIT 

SELECT CREDIT
SET ORDER TO TAG CREDIT

*-------------------- Make the suitable filters [ END ] -----------------

*-- The first time we run the screen, 

IF !WEXIST(gcBaseWind)
  
  *-- Initialize some variables
  
  *-- tEMP WINDOW NAMES
  lcChWin0        = gfTempName()
  lcChWin1        = gfTempName()
  lcChWin2        = gfTempName()
  lcTmpFile       = gfTempName()
  *E301534,1 ASH 01/15/2001 (Begin) Change the title.
  *lcBrowTitl      = 'Debit on Account'       &&-- Preselected Transaction type
  lcBrowTitl      = 'Debit Adj. / Debit on Acc.'       &&-- Preselected Transaction type
  *E301534,1 ASH 01/15/2001 (End)

  *-- Array hold the header's codes info.
  laReson         = SPACE(0)           &&-- compo array
  laCodeInf       = SPACE(0)

  laCodeInf[1,01] = "TRANCODE"
  laCodeInf[1,02] = "laReson"
  laCodeInf[1,03] = "lnReson" 
  laCodeInf[1,04] = ""
  laCodeInf[1,05] = .F.
  laCodeInf[1,06] = .F.
  laCodeInf[1,07] = ""
  laCodeInf[1,08] = ""
  laCodeInf[1,09] = ""
  laCodeInf[1,10] = ""
  
  laCodeInf[2,01] = "CCREDITCOD"
  laCodeInf[2,02] = "laReson"
  laCodeInf[2,03] = "lnReson" 
  laCodeInf[2,04] = ""
  laCodeInf[2,05] = .F.
  laCodeInf[2,06] = .F.
  laCodeInf[2,07] = ""
  laCodeInf[2,08] = ""
  laCodeInf[2,09] = ""
  laCodeInf[2,10] = ""

  *-- Creat the tmp file .
  DO lpCrtTmp
  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  =gfwCodePop(@laCodeInf,   "TRANCODE"   , "N")
  m.Reference=SPACE(30)
  m.Desc=SPACE(20)

ENDIF


PUSH KEY

*--Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcBrowTitl KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)
**-------------------------- Call the screen

DO (gcScrDir+"ARDCTRN.SPR")
POP KEY

RELEASE WINDOW (lcBrowTitl)
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

*-- Removed resetting filter. ReSet filter anyway
SELECT CUSTOMER
SET FILTER TO

IF glQuitting
  *SELECT CUSTOMER
  *SET FILTER TO
  *-- Deactivate the filters
  *-- Erase tmp file if it was created
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
    ERASE (gcWorkDir+lcTmpFile+'.DBF')
    ERASE (gcWorkDir+lcTmpFile+'.CDX')
  ENDIF  
ENDIF  

*-- FUNCTIONS & PROCEDURES SECTION
*!*************************************************************
*! Name               : lpShow
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : lfRefresh() , lfBrowse() AND lfwBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpShow
*!************************************************************
*!
PROCEDURE lpShow
PARAMETERS llFrmRadio

SHOW GET pbDlt  DISABLE
*SHOW GET ibTrap ENABLE
lcForCond = "Account=laData[1]"

DO CASE
  *-- Select mode ..
  CASE laScrMode[1]
    lnRadio=1
    lcRadState = 'DISABLE'
    lcResState = 'DISABLE' &&-- in lcChWin2        
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    =gfwCodePop(@laCodeInf,   "TRANCODE"   , "N")
    m.Reference=SPACE(30)
    m.Desc=SPACE(20)
    
  *-- View mode..
  CASE laScrMode[2]
    *-- lcRadState value will be aasigned in account validation
    lcResState = 'DISABLE' &&-- in lcChWin2    

    IF lnRadio=1
      SELECT DEBIT
      GO lnDebitPos
      laCodeInf[1,04] = ""
      =gfwCodePop(@laCodeInf,    "TRANCODE"    , "L")   
    ELSE
      SELECT CREDIT
      GO lnCreditPos
      laCodeInf[2,04] = ""
      =gfwCodePop(@laCodeInf,   "CCREDITCOD"   , "L")         
    ENDIF
    *-- record pointer is adjusted in lfvAccount

      SCATTER FIELDS &lcScFields MEMO TO laData    
      SCATTER FIELDS Reference , Desc MEMVAR &&-- Update the memory variables of Reference get field   
    
  *-- lcRadState will not be controlled here because it already controled

  *-- Edit mode .. 
  CASE laScrMode[3]

    IF !llFrmRadio
      *-- Code to Collect the data for selected customer 
      =lfGetInfo(.F.)
    ENDIF    &&-- endif of llFrmRadio
    
    IF lnRadio=1
      laCodeInf[1,04] = ""
      =gfwCodePop(@laCodeInf,    "TRANCODE"    , "L")   
    ELSE
      laCodeInf[2,04] = "" 
      =gfwCodePop(@laCodeInf,   "CCREDITCOD"   , "L")   
    ENDIF
    lcResState = 'ENABLE' &&-- in lcChWin2
    SCATTER FIELDS Reference , Desc MEMVAR &&-- Update the memory variables 
ENDCASE


SHOW GET lnRadio &lcRadState
=lfRefresh(lcChWin0)  &&-- Refresh the say field .
=lfBrowse() AND lfwBrow()

*!*************************************************************
*! Name               : lfvAccount
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : Valid function for the account field
*!*************************************************************
*! Calls              : CUSBROWM()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAccount()
*!*************************************************************
*!
FUNCTION lfvAccount
PRIVATE lcAccount 

IF MDOWN()                           &&-- if the user click any thing 
  laData[1] = SPACE(5)               &&-- ( exit for example )
  SHOW GET laData[1]                 &&-- blank the account and exit
  RETURN                             &&-- the lfvaccount function
ENDIF

IF !llBrowse AND EMPTY(laData[1])          
  RETURN                           
ENDIF

IF llBrowse .OR. ('?' $ laData[1]) .OR. !SEEK('M'+laData[1],'CUSTOMER')
  lcAccount = laData[1]
  DO CUSBROWM WITH lcAccount      &&-- this global function has the following
  laData[1] = lcAccount           &&-- properity it blanks its parameter 
  SHOW GET laData[1]  
  _CUROBJ = _CUROBJ               &&-- when cancel or escab from brows
  llBrowse = .F.                  
ENDIF


IF EMPTY(laData[1])      
  RETURN                
ENDIF

*-- the next block of code will affect the apperance of radio button as follw 
*-- 1- if llHasDebit  AND llHasCredit  radio  enable
*-- 2- if llHasDebit  AND !llHasCredit radio  disable and chargback selected
*-- 3- if !llHasDebit AND llHasCredit  radio  disable and Credit    selected
*-- 4- if !llHasDebit AND !llHasCredit radio  Measage and select again .

STORE .F. TO llHasDebit,llHasCredit
=lfGetInfo(.T.) 

DO CASE
  CASE !llHasDebit AND !llHasCredit
    =gfModalGen('TRM40158B00000','DIALOG',laData[1])
    laData[1] = SPACE(5)
    SHOW GET laData[1]  
    _CUROBJ = _CUROBJ   
    llBrowse = .F.                  
    RETURN              
  CASE llHasDebit  AND !llHasCredit
    lnRadio       = 1           
    lcRadState    = 'DISABLE'   
 
  CASE !llHasDebit AND llHasCredit
    lnRadio       = 2           
    lcRadState    = 'DISABLE'   
  CASE llHasDebit  AND llHasCredit
    lnRadio       = 1           
    lcRadState    = 'ENABLE'   
ENDCASE  

SELECT CUSTOMER
STORE .F. TO laScrMode
STORE .T. TO laScrMode[2]
SHOW GETS
*-- end of lfvAccount.

*!*************************************************************
*! Name               : lfwRadio
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRadio()
*!*************************************************************
*!
FUNCTION lfwRadio
lnOldVal = lnRadio

*!*************************************************************
*! Name               : lfvRadio
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRadio()
*!*************************************************************
*!
*-- this function can't be called befor lpshow
FUNCTION lfvRadio
IF lnRadio != lnOldVal
  DO lpShow WITH .T.
ENDIF

KEYBOARD '{TAB}' PLAIN

*!*************************************************************
*! Name               : lfBrowse
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowse()
*!*************************************************************
*!
FUNCTION lfBrowse

*-- Save current alias  
PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)

IF !EMPTY(lcBrowTitl) AND WEXIST(lcBrowTitl)
  RELEASE WINDOW (lcBrowTitl)
ENDIF


DO CASE
  CASE laScrMode[1] OR (laScrMode[2] AND lnRadio=1) &&-- Chargeback view mode 
    SELECT DEBIT
    lcBrwFlds  = [cMarker = IIF(lnMarker = RECNO('DEBIT'),'>',' ') :H=' ',]+;
                 [TRAN :8 :H= 'Tran.n#',TRANDATE :10 :H= 'Date',]+;
                 [CodDes=gfCodDes(DEBIT.TRANCODE,'TRANCODE') :20 :H='Reason',]+;
                 [DESC :19 :H= 'Description',Reference]
    *E301534,1 ASH 01/15/2001 (Begin) Add the debit adj. type (2) to be edited.         
    *lcBrowTitl = 'Debit on Account'                             
    *lcForCond  = "trantype+tran+cinstalno='3'"
    lcBrowTitl = 'Debit Adj. / Debit on Acc.'                             
    lcForCond  = "trantype+tran+cinstalno='2' OR trantype+tran+cinstalno='3'"
    *E301534,1 ASH 01/15/2001 (End)
  
  CASE laScrMode[2] AND lnRadio=2  &&-- CREDIT view mode
    SELECT CREDIT
    lcBrwFlds  = [cMarker = IIF(lnMarker = RECNO('CREDIT'),'>',' ') :H=' ',]+;
                 [TRAN :8 :H= 'Tran.n#',TRANDATE :10 :H= 'Date',]+;
                 [CodDes=gfCodDes(CREDIT.CCREDITCOD,'CCREDITCOD') :20 :H='Reason',]+;
                 [DESC :19 :H= 'Description',Reference]
    *E301534,1 ASH 01/15/2001 (Begin) Add the debit adj. type (5) to be edited.         
    *lcBrowTitl = 'Credit on Account'                             
    *lcForCond  = "trantype+tran='6'"
    lcBrowTitl = 'Credit Adj. / Credit on Acc.'                             
    lcForCond  = "trantype+tran='5' OR trantype+tran='6'"
    *E301534,1 ASH 01/15/2001 (End)
  
  CASE  laScrMode[3] &&--AND lnRadio=1  &&-- Chargeback Edit mode from (lcTmpFile) 
    SELECT (lcTmpFile)
    *E301534,1 ASH 01/15/2001 (Begin) Add Debit/Credit Adjusments to the title.
    *lcBrowTitl  = IIF(lnRadio=1,'Debit on Account','Credit on Account')
    lcBrowTitl  = IIF(lnRadio=1,'Debit Adj. / Debit on Acc.','Credit Adj. / Credit on Acc.')
    *E301534,1 ASH 01/15/2001 (End)
    *lcTran      = IIF(lnRadio=1 , '3', '6')
    IF lnRadio=1
      *E301534,1 ASH 01/15/2001 (Begin) Add the debit adj. type (2) to be edited.         
      *lcForCond  = "trantype+tran+cinstalno='3'"
      lcForCond  = "trantype+tran+cinstalno='2' OR trantype+tran+cinstalno='3'"
      *E301534,1 ASH 01/15/2001 (End)
    ELSE
      *E301534,1 ASH 01/15/2001 (Begin) Add the Credit adj. type (5) to be edited.         
      *lcForCond  = "trantype+tran='6'"
      lcForCond  = "trantype+tran='5' OR trantype+tran='6'"
      *E301534,1 ASH 01/15/2001 (End)
    ENDIF


    IF lnRadio=1
      lcBrwFlds  = [cMarker = IIF(lnMarker = RECNO(lcTmpFile),'>',' ') :H=' ',]+;
                   [TRAN :8 :H= 'Tran.n#',TRANDATE :10 :H= 'Date',]+;
                   [CodDes=gfCodDes(&lcTmpFile..TRANCODE,'TRANCODE') :20 :H='Reason',]+;
                   [DESC :19 :H= 'Description',Reference,Status]
                            
    ELSE
      lcBrwFlds  = [cMarker = IIF(lnMarker = RECNO(lcTmpFile),'>',' ') :H=' ',]+;
                   [TRAN :8 :H= 'Tran.n#',TRANDATE :10 :H= 'Date',]+;
                   [CodDes=gfCodDes(&lcTmpFile..CCREDITCOD,'CCREDITCOD') :20 :H='Reason',]+;
                   [DESC :19 :H= 'Description',Reference,Status]
    ENDIF
    
ENDCASE


BROWSE KEY laData[1]          ;
       FIELDS &lcBrwFlds      ; 
       FOR &lcForCond         ; 
      	   SAVE               ;
       	   NOWAIT             ;
           NOCLEAR            ;
           NOEDIT             ;
           NOMENU             ;  
           WHEN lfwBrow()     ;
           VALID :F lfvBrow() ;         
           FREEZE cMarker     ;
    WINDOW (lcChWin1)         ;
    IN WINDOW (gcBaseWind)    ; 
    TITLE  (lcBrowTitl)

*-- Restore previous alias
SELECT (lnCurAlias)
*-- end of lfBrowse.
*!*************************************************************
*! Name               : lfVBrow
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfVBrow()
*!*************************************************************
*!
FUNCTION lfVBrow
IF WONTOP() # (lcBrowTitl)
  IF glFromBrow
    =gfStopBrow()
  ENDIF
ENDIF

*-- end of lfVBrow.
*!*************************************************************
*! Name               : lfwBrow
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwBrow()
*!*************************************************************
*!
FUNCTION lfwBrow
DO CASE

  CASE laScrMOde[2] AND lnRadio=1

    lnMarker = RECNO("DEBIT") 
    m.Reference=DEBIT.Reference             
    m.Desc=DEBIT.Desc             
    lnReson=(ASCAN(laReson,DEBIT.trancode) / 2 )

  CASE laScrMOde[2] AND lnRadio=2  

    lnMarker = RECNO("CREDIT")
    lnReson=(ASCAN(laReson,CREDIT.CCREDITCOD) / 2 )
    m.Reference=CREDIT.Reference
    m.Desc=CREDIT.Desc             

  CASE  laScrMode[3]

    lnMarker = RECNO(lctmpfile)
    IF lnRadio=1
      lnReson=(ASCAN(laReson,&lctmpfile..trancode) / 2 )
    ELSE
      lnReson=(ASCAN(laReson,&lctmpfile..CCREDITCOD) / 2 )
    ENDIF
    m.Reference=&lctmpfile..Reference
    m.Desc=&lctmpfile..Desc             

ENDCASE

*Renee add lcResState to the objects
SHOW GET lnReson     &lcResState
SHOW GET m.Reference &lcResState
SHOW GET m.Desc      &lcResState
SHOW WINDOW (lcBrowTitl) REFRESH SAME

*!*************************************************************
*! Name               : lfvReference
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvReference()
*!*************************************************************
*!
FUNCTION lfvReference
PRIVAT lcVar

lcVar      = 'm.' + SYS(18)
lcNewVal   = EVALUATE(lcVar)
IF lcNewVal <> lcOldVal
  SCATTER FIELDS Status , Reference MEMVAR  &&-- From work alias temp
  m.Status      = IIF(m.Status <> "M" , "M" , m.Status)
  &lcVar     = lcNewVal
  GATHER MEMVAR FIELDS Status , Reference
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
  RELEASE m.Status
  lcOldVal = SPACE(30) 
ENDIF
*!*************************************************************
*! Name               : lfwReson
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwReson()
*!*************************************************************
*!
FUNCTION lfwReson




*!*************************************************************
*! Name               : lfvReson
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvReson()
*!*************************************************************
*!
FUNCTION lfvReson
*- Note that this object is enable only in edit mode so the work alias is tmp

SCATTER FIELDS Trancode , Ccreditcod , Status , Reference MEMVAR 
IF lnRadio    =1
  m.Trancode  =laReson[lnReson,2]
  m.Ccreditcod=SPACE(6)
ELSE  
  m.Ccreditcod=laReson[lnReson,2]
  m.Trancode  =SPACE(6)
ENDIF    
m.Status      = IIF(m.Status <> "M" , "M" , m.Status)

*B607426,1 KHM 07/16/2003 (Begin) Refersh the reason description and save it in the file
m.Desc = laReson[lnReson,1]
SHOW GET m.Desc
*GATHER MEMVAR FIELDS Trancode , Ccreditcod , Status
GATHER MEMVAR FIELDS Trancode , Ccreditcod , Status, Desc
*B607426,1 KHM 07/16/2003 (End)

RELEASE m.Trancode , m.Ccreditcod , m.Status
CodDes=laReson[lnReson,1] &&-- Refresh the calculated field .
SHOW WINDOW (lcBrowTitl) REFRESH SAME

*!*************************************************************
*! Name               : lpSavScr
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpSavScr
*!*************************************************************
*!
PROCEDURE lpSavScr
PRIVATE lnSelAls
IF llCupdate
  lnSelAls = SELECT(0)
  SELECT (lcTmpFile)
  SCAN FOR Status = "M" 
    *E301534,1 ASH 01/15/2001 (Begin) Add the debit adj. type (2) to be edited.         
    *IF Trantype='3'    
    IF Trantype $ '23'    
    *E301534,1 ASH 01/15/2001 (End)
      SELECT DEBIT
      IF gfObj_Lock(.T.)
        REPLACE Debit.Trancode     WITH &lcTmpFile..Trancode ,;
                Debit.Reference    WITH &lcTmpFile..Reference,;
                Debit.Desc         WITH &lcTmpFile..Desc     ,;
                &lcTmpFile..Status WITH SPACE(1)
         =gfObj_Lock(.F.)
       ENDIF
   ELSE
     SELECT CREDIT
     IF gfObj_Lock(.T.)
       REPLACE Credit.Ccreditcod  WITH &lcTmpFile..Ccreditcod ,;
                 Credit.Reference   WITH &lcTmpFile..Reference  ,;
                 Credit.Desc        WITH &lcTmpFile..Desc       ,;
                 &lcTmpFile..Status WITH SPACE(1)            
        =gfObj_Lock(.F.)
      ENDIF
    ENDIF 
    SELECT (lcTmpFile)
  ENDSCAN
  SELECT (lnSelAls)
ENDIF  

*!*************************************************************
*! Name               : lpTrap
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpTrap
*!*************************************************************
*!
PROCEDURE lpTrap
PARAMETERS WIN_NAME,UNTRAP_KEY
ON KEY LABEL &UNTRAP_KEY        &&-- AVOIDING INFINIT LOOP BY UNTRAP
ACTIVATE WINDOW &WIN_NAME
*!*************************************************************
*! Name               : lfDeact
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDeact()
*!*************************************************************
*!
FUNCTION lfDeact
*-- Trap keys in this function
IF WONTOP()=(lcBrowTitl)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTrap WITH (lcChWin2),'TAB'
  ON KEY LABEL BACKTAB    DO lpTrap WITH (lcChWin0),'BACKTAB'
  ON KEY LABEL ESCAPE     LLDUMMY1=.T.
ENDIF


*!*************************************************************
*! Name               : lfAct
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAct()
*!*************************************************************
*!
FUNCTION lfAct
*-- Untrap keys in this function
IF WONTOP() # (lcBrowTitl)  
  IF glFromBrow
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    *ON KEY LABEL ESCAPE
    ON KEY LABEL ESC DO gfEscap
    =gfStopBrow()
  ENDIF
ENDIF  


*!*************************************************************
*! Name               : lpCrtTmp
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : DO lpCrtTmp
*!************************************************************
*!
PROCEDURE lpCrtTmp
PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)

DIMENSION laFileStru[10,4]

laFileStru[1,1]   = 'Account'              &&-- COMMON
laFileStru[1,2]   = 'C'
laFileStru[1,3]   = 5
laFileStru[1,4]   = 0
      
laFileStru[2,1]   = 'Trancode'             &&-- DEBIT
laFileStru[2,2]   = 'C'
laFileStru[2,3]   = 6
laFileStru[2,4]   = 0

laFileStru[3,1]   = 'Desc'                &&-- COMMON
laFileStru[3,2]   = 'C'
laFileStru[3,3]   = 20
laFileStru[3,4]   = 0

laFileStru[4,1]   = 'Reference'            &&COMM
laFileStru[4,2]   = 'C'
laFileStru[4,3]   = 30
laFileStru[4,4]   = 0

laFileStru[5,1]   = 'Ccreditcod'       && CREDIT
laFileStru[5,2]   = 'C'
laFileStru[5,3]   = 6
laFileStru[5,4]   = 0

laFileStru[6,1]   = 'Tran'        &&COMMON
laFileStru[6,2]   = 'C'
laFileStru[6,3]   = 6
laFileStru[6,4]   = 0

laFileStru[7,1]   = 'Trandate'      &&COM
laFileStru[7,2]   = 'D'
laFileStru[7,3]   = 8
laFileStru[7,4]   = 0

laFileStru[8,1]   = 'Status'        && NEW
laFileStru[8,2]   = 'C'
laFileStru[8,3]   = 1
laFileStru[8,4]   = 0

laFileStru[9,1]   = 'Trantype'       && COMMON
laFileStru[9,2]   = 'C'
laFileStru[9,3]   = 1
laFileStru[9,4]   = 0

laFileStru[10,1]   = 'Cinstalno'   &&-- Debit
laFileStru[10,2]   = 'C'
laFileStru[10,3]   = 3
laFileStru[10,4]   = 0

CREATE TABLE &gcWorkDir.&lcTmpFile FROM ARRAY laFileStru
*INDEX ON account+Trantype+tran+DTOS(trandate) TAG (lcTmpFile)
INDEX ON account+tran+DTOS(trandate) TAG (lcTmpFile)
SET ORDER TO TAG (lcTmpFile)
SELECT(lnCurAlias)
*-------------------------------------------------------------------------

FUNCTION lfwReferenc
lcOldVal= EVALUATE('m.' + SYS(18))

FUNCTION lfwDesc
lcOldVal= EVALUATE('m.' + SYS(18))
*!*************************************************************
*! Name               : lfvDesc
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDesc()
*!*************************************************************
*!
FUNCTION lfvDesc
PRIVAT lcVar

lcVar      = 'm.' + SYS(18)
lcNewVal   = EVALUATE(lcVar)
IF lcNewVal <> lcOldVal
  SCATTER FIELDS Status , Desc MEMVAR  &&-- From work alias temp
  m.Status      = IIF(m.Status <> "M" , "M" , m.Status)
  &lcVar     = lcNewVal
  GATHER MEMVAR FIELDS Status , Desc
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
  RELEASE m.Status
  lcOldVal = SPACE(30) 
ENDIF
*!*************************************************************
*! Name               : lfGetInfo
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetInfo()
*!*************************************************************
*!
FUNCTION lfGetInfo
PARAMETER llFromAcc 
PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)
SELECT (lcTmpFile)
ZAP
SET RELATION TO

IF llFromAcc OR llHasDebit
  SELECT DEBIT
  SET ORDER TO TAG Drtran
  *E301534,1 ASH 01/15/2001 (Begin) Add the debit adj. type (2) to be edited.           
  *IF SEEK('3','DEBIT')
    *SCAN REST WHILE trantype+tran+cinstalno= '3' 
  
  *B606739,1 KHM 05/26/2003 (Begin) Add the seek of type '3' SEEK('3','DEBIT')
  *IF SEEK('2','DEBIT')  
  IF SEEK('2','DEBIT') OR SEEK('3','DEBIT')  
  *B606739,1 KHM 05/26/2003 (End)
  
    SCAN REST WHILE trantype+tran+cinstalno = '2' OR trantype+tran+cinstalno = '3' 
  *E301534,1 ASH 01/15/2001 (End)
      IF DEBIT.ACCOUNT=laData[1]
        IF llFromAcc
          llHasDebit = .T.
          lnDebitPos = RECNO('DEBIT')
          EXIT
        ELSE
          SCATTER FIELDS ACCOUNT,Cinstalno,Trancode,Trantype,DESC,REFERENCE,TRAN,TRANDATE MEMVAR        
          INSERT INTO (lcTmpFile) FROM MEMVAR        
        ENDIF  
      ENDIF
    ENDSCAN
    SET ORDER TO TAG Debit
    IF !llFromAcc
      SELECT (lcTmpFile)
      SET RELATION TO account+tran+cinstalno+DTOS(trandate) INTO DEBIT
      RELEASE m.ACCOUNT,m.Trancode,m.Cinstalno,m.Trantype,m.TRAN,m.TRANDATE
    ENDIF
  ENDIF  
ENDIF

IF llFromAcc OR llHasCredit
  SELECT CREDIT
  SET ORDER TO TAG Crtran
  *E301534,1 ASH 01/15/2001 (Begin) Add the Credit adj. type (5) to be edited.         
  *IF SEEK('6','CREDIT')
    *SCAN REST WHILE trantype+tran= '6' 
  
  *B606739,1 KHM 05/26/2003 (Begin) Add the seek of type '6' SEEK('6','DEBIT')
  *IF SEEK('5','CREDIT')
  IF SEEK('5','CREDIT') OR SEEK('6','CREDIT')
  *B606739,1 KHM 05/26/2003 (End)
  
    SCAN REST WHILE trantype+tran = '5' OR trantype+tran = '6'     
  *E301534,1 ASH 01/15/2001 (End)
      IF CREDIT.ACCOUNT=laData[1]
        IF llFromAcc  &&-- Assigen value for llHasCredit only if you came from lfvAccount
          llHasCredit = .T.
          lnCreditPos = RECNO('CREDIT')
          EXIT
        ELSE
          SCATTER FIELDS ACCOUNT,Ccreditcod,Trantype,DESC,REFERENCE,TRAN,TRANDATE MEMVAR
          INSERT INTO (lcTmpFile) FROM MEMVAR
        ENDIF
      ENDIF
    ENDSCAN
    SET ORDER TO TAG Credit    
    IF !llFromAcc
      SELECT (lcTmpFile)
      SET RELATION TO account+tran+DTOS(trandate) INTO CREDIT ADDITIVE
      RELEASE m.ACCOUNT,m.Ccreditcod,m.Cinstalno,m.Trantype,m.TRAN,m.TRANDATE
    ENDIF
  ENDIF  
ENDIF

SELECT (lcTmpFile)
GO TOP

SELECT(lnCurAlias)