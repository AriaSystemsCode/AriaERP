*:*************************************************************************
*: Program file  : ARPOSCH
*: Program desc. : Post dated checks.
*: For screen    : ARPOSCH
*:        System : Aria Advantage Series.
*:        Module : Accounts Receivables (AR).
*:     Developer : Ahmed Amer (AHM)
*:*************************************************************************
*: Calls : 
*:         Procedures : lpShow,lpSavscr,lpDelScr
*:         Functions  : gfSetup,gfOpenFile,lfvData_1,lfBrow,
*:                    : lfOldValue,lfTrap,lfClrTrap,lfBrTab,
*:                    : lfBrBack,lfwBrow,lfvBrow,lfvNew,
*:                    : lfvRem,lfCreateTmp,lfCollData,lfvChqNo
*:                    : lfvPayDate,lfvAmount,lfvComment 
*:*************************************************************************
*: Passed Parameters  : lcSetupCom,lcSULnkVer
*:*************************************************************************
*: Example : DO ARPOSCH
*:*************************************************************************
*:C101426,1 Sameh (SSE) 04/19/99 Adding 2 Fields with 50 Chrs each on 
*:                      "Post Dated Checks" screen as a free format fields
*:                      These 2 fields sbould be shown on the browse and use
*:                      the word "Comments" as the field title
*:                      Post Dated Checks should be sorted by field "PAY DATE"    
*:                      on Post Dated Checks Browse in Screen
*:B802170,1 Sameh (SSE) fixing 'Variable Status not found'
*:
*:B602815,1 Sameh (SSE) making the screen saves the deleted lines 
*:B602838,1 NAD 09/24/2000 Not to allow transactions for hold account.          
*:B603713,5 NAD 09/27/2000 Increase the Amount Field to accept large currencies 
*:B603713,5                like Italian Lira in the Screen.  
*:B804270,1 BWA 08/13/2001 Fix the bug making transactions to the hold accounts.[START]
*:B605440,1 NAD            FIX Bugs in the realease 
*:*************************************************************************

DIMENSION laData[1],laScrMode[4],laKeyField[1,4],laFileStru[1,4]
llNoShow = .F.
laDefproc[09] = .F.                 && This is to Enable local save
laDefproc[07] = .F.                 && This is to Enable local delete

*-- lcBrTtl variable that hold the browse title
lcBrTtl = 'Post dated checks'
 
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] = .T.
laKeyField[1,3] = "POSTDCHQ"
laKeyField[1,4] = 1

STORE SPACE(0) TO lcPosChq1,lcPosChq2,lcPosChq3,lcFileToUse,lcTempFile

*C101426,1 define variables lcChqNo , lcComment1 , lcComment2 [Begin.]
*STORE SPACE(0) TO lcChqNo
STORE SPACE(0) TO lcChqNo,lcComment1,lcComment2
*C101426,1 define variables lcChqNo , lcComment1 , lcComment2 [End.]

STORE 0 TO lnAmount
STORE 'DISABLE' TO lcNewStat,lcRemStat,lcChqStat,lcDatAmtS

STORE gdSysDate TO ldPayDate

lnBrRecNo = 0
llBrowse  = .F.

IF !gfSetup()
  RETURN
ENDIF  

IF !WEXIST(gcBaseWind)
  lcPosChq1  = gfTempName()
  lcPosChq2  = gfTempName()
  lcPosChq3  = gfTempName()
  
  lcTempFile = gfTempName()
  
  lcScFields = "Account"
  SELECT CUSTOMER
  SCATTER FIELDS &lcScFields TO laData BLANK
  
  SELECT POSTDCHQ
  = AFIELDS(laFileStru)
  
  
  DIMENSION laFileStru[ALEN(laFileStru,1) + 1, 4]
  laFileStru[ALEN(laFileStru,1) ,1] = 'cStatus'
  laFileStru[ALEN(laFileStru,1) ,2] = 'C'
  laFileStru[ALEN(laFileStru,1) ,3] = 1
  laFileStru[ALEN(laFileStru,1) ,4] = 0

  DIMENSION laFileStru[ALEN(laFileStru,1) + 1, 4]
  laFileStru[ALEN(laFileStru,1) ,1] = 'nRecNo'
  laFileStru[ALEN(laFileStru,1) ,2] = 'N'
  laFileStru[ALEN(laFileStru,1) ,3] = 10
  laFileStru[ALEN(laFileStru,1) ,4] = 0
  
  
  *B605440,1 NAD    
  DIMENSION laFileStru[ALEN(laFileStru,1) + 1, 4]
  laFileStru[ALEN(laFileStru,1) ,1] = 'lNewRec'
  laFileStru[ALEN(laFileStru,1) ,2] = 'L'
  laFileStru[ALEN(laFileStru,1) ,3] = 0
  laFileStru[ALEN(laFileStru,1) ,4] = 0  
  *B605440,1 NAD    
  
ENDIF    &&IF !WEXIST(gcBaseWind)

PUSH KEY                                      && To save the the current on key label
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)  && To activate the browse screen when pressing ALT+B
DO (gcScrDir + gcWinAppl + '\ARPOSCH.SPR')    
RELEASE WINDOW (lcBrTtl)                      && Release the Browse Window
POP KEY                                       && To Restore the previous assignments for on key label

*-- If we realy quitting the screen 
IF glQuitting
  IF USED(lcTempFile)
    USE IN (lcTempFile)
  ENDIF
  ERASE (gcWorkDir+lcTempFile +".DBF")
  ERASE (gcWorkDir+lcTempFile +".CDX")
ENDIF     &&IF glQuitting

*:*************************************************************************
*! Name      : lfvData_1
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (Account).
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvData_1()
*:*************************************************************************

FUNCTION lfvData_1
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

XACCOUNT = laData[1]

IF !( (EMPTY(laData[1]) OR LASTKEY() <> 13) AND !llBrowse)
  IF !SEEK('M'+laData[1],'CUSTOMER')
    llBrowse = .F.
    DO CUSBROWM WITH XACCOUNT
    laData[1] = XACCOUNT
  ENDIF

  IF !EMPTY(laData[1])
    *B602838,1 NAD 09/24/2000  (Start) Added the if condition.
    IF SEEK('M'+laData[1],'CUSTOMER')

      *B804270,1 BWA 08/13/2001 Fix the bug making transactions to the hold accounts.[START]
      *IF Customer.Status <> 'A'
      *  *Message : 40022
      *  *This is a non-active account . Cannot proceed.
      *  *Button : 00000
      *  *Ok
      *  =gfModalGen('TRM40172B00000','ALERT')
      *  _CUROBJ = _CUROBJ
      *  laData[1]=''
      *  RETURN  
      *ELSE
      *B804270,1 BWA 08/13/2001.[END]

      *B602838,1 NAD (End)
        STORE .F. TO laScrMode
        laScrMode[2] = .T.
        SHOW GETS
      *B602838,1 NAD 09/24/2000 (Start) Added the ENDIF.

      *B804270,1 BWA 08/13/2001 Fix the bug making transactions to the hold accounts.[START]
      *ENDIF
      *B804270,1 BWA 08/13/2001.[END]

    ENDIF    
    *B602838,1 NAD (End)
  ENDIF    
ENDIF

SELECT (lnCurAlias)

*:*************************************************************************
*! Name      : lfBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : to bowse ALL Postdchq for an Account selected
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : NONE
*:*************************************************************************
*! Returns            : NONE
*:*************************************************************************
*! Example   : =lfBrow()
*:*************************************************************************

FUNCTION lfBrow

IF WEXIST(lcBrTtl)
  RELEASE WINDOW (lcBrTtl)
ENDIF

lcFileToUse = IIF(laScrMode[2],'POSTDCHQ',lcTempFile)
SELECT (lcFileToUse)
GO TOP

*C101426,1 Adding Comment1 , Comment2 in the Browse [Begin.]

*BROWSE FIELDS cMarker =IIF(RECNO()=lnBrRecNo,'>',' ') :H = ' ':R:1:W=.F.,;
              ChequeNo :R :H= 'check No.', PayDate :R :H= 'Pay Date',Amount ;
         SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
         TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfvBrow() ;         
         WINDOW (lcPosChq2) IN WINDOW (gcBaseWind)         

BROWSE FIELDS cMarker = IIF(RECNO()=lnBrRecNo,'>',' ') :H = ' ':R:1:W=.F.,;
              PayDate :R :H = 'Pay Date' , ChequeNo  :R :H = 'check No.' ,Amount,;
 			  Ccomment1 :R :H = 'Comment 1' , Ccomment2 :R :H = 'Comment 2';	       
         	  SAVE NOWAIT NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR ;
              TITLE(lcBrTtl) WHEN lfwBrow() VALID :F lfvBrow() ;         
              WINDOW (lcPosChq2) IN WINDOW (gcBaseWind)         

*C101426,1 Adding Comment1 , Comment2 in the Browse [End.]

= lfwBrow()

*:*************************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWHBrow,lfwBrow
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : DO lpShow
*:*************************************************************************
PROCEDURE lpShow
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

DO CASE
  *-- Select mode
  CASE laScrMode[1]

    lnBrRecNo = 0
    *C101426,1 considering lcComment1 , lcComment2 in Select Mode [Begin.]
    *STORE SPACE(0) TO lcChqNo
    STORE SPACE(0) TO lcChqNo,lcComment1,lcComment2
    *C101426,1 considering lcComment1 , lcComment2 in Select Mode [End.]
    
    STORE 0 TO lnAmount
    STORE gdSysDate TO ldPayDate
    llBrowse = .F.
    STORE 'DISABLE' TO lcNewStat,lcRemStat,lcChqStat,lcDatAmtS
    SELECT POSTDCHQ
    SET FILTER TO 
    = lfCreateTmp()
    
  *-- View mode
  CASE laScrMode[2]
    SCATTER FIELDS &lcScFields TO laData
    SELECT POSTDCHQ
    
    *C101426,1 Remove STR(RECNO(),7) so as to be like the POSTDCHQ index [Begin.] 
    *SET FILTER TO Account+DTOS(Paydate)+STR(RECNO(),7) = ALLTRIM(laData[1])
	SET FILTER TO Account+DTOS(Paydate) = ALLTRIM(laData[1])
    *C101426,1 Remove STR(RECNO(),7) so as to be like the POSTDCHQ index [End.]
	
  *-- Edit mode
  CASE laScrMode[3]
    SELECT CUSTOMER
    = gfObj_Lock(.F.)
    = lfCollData()
    ACTIVATE WINDOW (lcPosChq3)
    _CUROBJ = OBJNUM(pbNew)
      
  *-- Add mode
  CASE laScrMode[4]

ENDCASE

= lfBrow()

SELECT (lnCurAlias)

*:*************************************************************************
*! Name      : lpSavscr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local save.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : NONE
*:*************************************************************************
*! Returns            : NONE
*:*************************************************************************
*! Example   : DO lpSavscr
*:*************************************************************************

PROCEDURE lpSavscr
PRIVATE llContinue,lnCurAlias,lcCurOrder

lnCurAlias = SELECT(0)
lcCurOrder = ORDER()

SELECT(lcTempFile)
SET ORDER TO UPDATE

*B602815 Saves DELETED settings

lcDelStat = SET('DELETED')
SET DELETED OFF

IF SEEK('Y')

  llContinue = .T.
  =gfTmp2Mast('POSTDCHQ',lcTempFile,'','')

ELSE

  llContinue = .F.  
  *-- No Transactions made, cannot proceed
  *-- Ok
  = gfModalGen("INM400705B00000","Dialog")

ENDIF  

SET DELETED &lcDelStat
*B602815 Restore DELETED settings       
       
SELECT (lnCurAlias)
SET ORDER TO lcCurOrder

llCSave = llContinue

*:*************************************************************************
*! Name      : lpDelScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local delete.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : NONE
*:*************************************************************************
*! Returns            : NONE
*:*************************************************************************
*! Example   : DO lpDelScr
*:*************************************************************************

PROCEDURE lpDelScr
PRIVATE lnCurAlias,lcTag,lnCurRec,llOpen,lnI,llContinue

lcDelStat = SET('DELETED')
SET DELETED OFF

lnCurAlias = SELECT(0)

SELECT (lcTempFile)

IF SEEK(laData[1],'POSTDCHQ')
  SELECT POSTDCHQ
  SCAN REST WHILE Account+DTOC(PayDate) = laData[1]
    BLANK
    DELETE
  ENDSCAN
ENDIF

SET DELETED &lcDelStat
  
SELECT CUSTOMER
= gfObj_Lock(.F.)

SELECT (lnCurAlias)

STORE .F. TO laScrMode
laScrMode[1] = .T.
SHOW GETS

*:*************************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO Assign functions to some keys to not affect the browse
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*:*************************************************************************
*! Passed Parameters  : NONE 
*:*************************************************************************
*! Returns            : NONE
*:*************************************************************************
*! Example   : =lfTrap()
*:*************************************************************************

FUNCTION lfTrap
*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTtl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
ENDIF

*:*************************************************************************
*! Name      : lfClrTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Clearing the previous trapping
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : NONE
*:*************************************************************************
*! Returns            : NONE
*:*************************************************************************
*! Example   : =lfClrTrap()
*:*************************************************************************

FUNCTION lfClrTrap
*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*:*************************************************************************
*! Name      : lfBrTab
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Tab Key
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : NONE
*:*************************************************************************
*! Returns            : NONE
*:*************************************************************************
*! Example   : =lfBrTab()
*:*************************************************************************

FUNCTION lfBrTab

ON KEY LABEL TAB
ACTIVATE WINDOW (lcPosChq3)
_CUROBJ = OBJNUM(lcChqNo)

*:*************************************************************************
*! Name      : lfBrBack
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the BackTab Key
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : NONE
*:*************************************************************************
*! Returns            : NONE
*:*************************************************************************
*! Example   : =lfBrBack()
*:*************************************************************************

FUNCTION lfBrBack

ON KEY LABEL BACKTAB

DO CASE
  CASE laScrMode[1]
    ACTIVATE WINDOW (lcPosChq1)
    _CUROBJ = OBJNUM(laData[1])
  CASE laScrMode[2]
    ACTIVATE WINDOW ('gwcContrl1')
    _CUROBJ = OBJNUM(pbCls)
  CASE laScrMode[3]
    ACTIVATE WINDOW ('gwcContrl1')
    _CUROBJ = OBJNUM(pbCls)
ENDCASE    

*:*************************************************************************
*! Name      : lfwBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : adjust the label of pbsel button
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfwBrow()
*:*************************************************************************

FUNCTION lfwBrow

lcFileToUse = IIF(laScrMode[2],'POSTDCHQ',lcTempFile)
SELECT (lcFileToUse)

lnBrRecNo = RECNO()

DO CASE
  CASE laScrMode[1]
    lcNewStat = "DISABLE"
    lcRemStat = "DISABLE"    
    lcChqStat = "DISABLE"
    lcDatAmtS = "DISABLE"

  CASE laScrMode[2]
    lcNewStat = "DISABLE"
    lcRemStat = "DISABLE"    
    lcChqStat = "DISABLE"
    lcDatAmtS = "DISABLE"

  CASE laScrMode[3]
    lcNewStat = "ENABLE"
    IF EOF(lcTempFile) OR BOF(lcTempFile) OR DELETED(lcTempFile)
      lcRemStat = "DISABLE"
    ELSE
      lcRemStat = "ENABLE"
    ENDIF
    IF EMPTY(&lcTempFile..ChequeNo) AND !EOF(lcTempFile)
      lcChqStat = "ENABLE"
    ELSE
      lcChqStat = "DISABLE"
    ENDIF
    IF EMPTY(&lcFileToUse..ChequeNo) OR EOF(lcTempFile)
      lcDatAmtS = "DISABLE"
    ELSE
      lcDatAmtS = "ENABLE"
    ENDIF
ENDCASE

lcChqNo    = &lcFileToUse..ChequeNo
ldPayDate  = &lcFileToUse..PayDate
lnAmount   = &lcFileToUse..Amount

*C101426,1 showing the value of lcComment1,lcComment2 in Screen [Begin.] 
lcComment1 = &lcFileToUse..Ccomment1
lcComment2 = &lcFileToUse..Ccomment2
*C101426,1 showing the value of lcComment1,lcComment2 in Screen [End.]

SHOW GET pbRem       &lcRemStat
SHOW GET lcChqNo     &lcChqStat

SHOW GET ldPayDate   &lcDatAmtS
SHOW GET lnAmount    &lcDatAmtS

*C101426,1 show get for lcComment1 & lcComment2 [Begin.]
SHOW GET lcComment1  &lcDatAmtS
SHOW GET lcComment2  &lcDatAmtS
*C101426,1 show get for lcComment1 & lcComment2 [End.]

SHOW WINDOW (lcBrTtl) REFRESH SAME

*:*************************************************************************
*! Name      : lfvBrow
*! Developer : Ahmed Amer (AHM) 
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvBrow()
*:*************************************************************************

FUNCTION lfvBrow
IF WONTOP() # (lcBrTtl)
  =gfStopBrow()
ENDIF

*:*************************************************************************
*! Name      : lfvNew
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate of PbNew in the Screen.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvNew()
*:*************************************************************************

FUNCTION lfvNew
PRIVATE lnCurAlias 

lnCurAlias = SELECT(0)

SELECT (lcTempFile)

*C101426,1 Changing the SEEK on (Account) to be on (PayDate)
*IF !SEEK(laData[1]+SPACE(10))
IF !SEEK(DTOS(ldPayDate)+SPACE(10))

  APPEND BLANK
  REPLACE Account WITH laData[1]

  *C101426,1 adding gdSysDate to Paydate [Begin.]
  REPLACE PayDate WITH gdSysDate
  *:B605440,1 NAD    
  REPLACE lNewRec WITH .T.
  *:B605440,1 NAD    
  _CUROBJ = OBJNUM(lcChqNo)
  *C101426,1 adding gdSysDate to Paydate [End.]

ENDIF  

= lfwBrow()

SELECT (lnCurAlias)

*:*************************************************************************
*! Name      : lfvRem
*! Developer : Ahmed Amer (AHM) 
*! Date      : 08/28/97
*! Purpose   : Validate of PbNew in the Screen.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvRem()
*:*************************************************************************

FUNCTION lfvRem
PRIVATE lnCurAlias 

lnCurAlias = SELECT(0)

*-- "Are You Sure To remove this line"
*-- <YES>, <NO>
IF gfModalGen("QRM40139B42002","Dialog") = 1
  SELECT(lcTempFile)
  *B605440,1 NAD     
  *REPLACE cStatus WITH 'D'
  REPLACE cStatus WITH IIF(lNewRec,'','D')
  *B605440,1 NAD    
  DELETE
  GOTO TOP
  llNoThing = lfwBrow()
ENDIF

SELECT (lnCurAlias)

*!**************************************************************************
*! Name      : lfCreateTmp
*! Developer : Ahmed Amer (AHM) 
*! Date      : 08/28/97
*! Purpose   : creating the Temp file of POSTDCHQ with 2 index
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : = lfCreateTmp()
*!**************************************************************************

FUNCTION lfCreateTmp

DIMENSION laIndex[2,2]
*
*laIndex[1,1] = 'ACCOUNT+CHEQUENO'
laIndex[1,1] = 'DTOS(PAYDATE)+CHEQUENO'

laIndex[1,2] = lcTempFile
laIndex[2,1] = "IIF(cStatus='S','N','Y')"
laIndex[2,2] = 'Update'

=gfCrtTmp(lcTempFile,@laFileStru,@laIndex)           
SET ORDER TO (lcTempFile) IN (lcTempFile)

*!**************************************************************************
*! Name      : lfCollData
*! Developer : Ahmed Amer (AHM) 
*! Date      : 08/28/97
*! Purpose   : copy POST DATED CHECKS of selected account into temp. file
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : = lfCollData()
*!**************************************************************************

FUNCTION lfCollData
PRIVATE lnCurAlias 

lnCurAlias = SELECT(0)

= lfCreateTmp()

SELECT POSTDCHQ
IF SEEK(laData[1])

  *C101426,1
  *SCAN REST WHILE Account+DTOS(Paydate)+STR(RECNO(),7) = ALLTRIM(laData[1])
  SCAN REST WHILE Account+DTOS(Paydate) = ALLTRIM(laData[1])
  *C101426,1 

    SCATTER MEMVAR MEMO
    SELECT (lcTempFile)
    APPEND BLANK
    GATHER MEMVAR MEMO

    *B802170,1 
    *REPLACE Status WITH 'S',;
            *nRecNo WITH RECNO('POSTDCHQ')
    REPLACE cStatus WITH 'S',;
            nRecNo WITH RECNO('POSTDCHQ')                    
    *B802170,1        	
  ENDSCAN
ENDIF

GO TOP IN (lcTempFile)

SELECT(lnCurAlias)

*:*************************************************************************
*! Name      : lfvChqNo
*! Developer : Ahmed Amer (AHM) 
*! Date      : 08/28/97
*! Purpose   : Validate CHECK NO in the Screen.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvChqNo()
*:*************************************************************************

FUNCTION lfvChqNo

PRIVATE lnAlias,lnRecNo

lnAlias = SELECT(0)

IF !EMPTY(lcChqNo) 

  *C101426,1 replace Validation to be on DTOS(ldPayDate)+ALLTRIM(lcChqNo)
  *			 inspite of laData[1]+ALLTRIM(lcChqNo)	
  
  PRIVATE llFindChq , lnCurrRec
  lnCurrRec = RECNO(lcTempFile)
  llFindChq = SEEK(DTOS(ldPayDate)+ALLTRIM(lcChqNo),lcTempFile)

  IF BETWEEN(lnCurrRec,1,RECCOUNT(lcTempFile))
    GO lnCurrRec IN (lcTempFile)
  ELSE
    GO TOP IN (lcTempFile)
  ENDIF

  IF llFindChq
    
    *-- Check number already exists.
    *-- OK
    = gfModalGen("INM40138B00000","Dialog")
    _CUROBJ = OBJNUM(lcChqNo)
  
  ELSE
    
    SELECT (lcTempFile)    
    REPLACE ChequeNo WITH lcChqNo , cStatus  WITH 'A'

  ENDIF
ENDIF

= lfwBrow()
SELECT (lnAlias)

*:*************************************************************************
*! Name      : lfvPayDate
*! Developer : Ahmed Amer (AHM) 
*! Date      : 08/28/97
*! Purpose   : Validate PAY DATE in the Screen.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvPayDate()
*:*************************************************************************

FUNCTION lfvPayDate
PRIVATE lnAlias

lnAlias = SELECT(0)

IF !EMPTY(ldPayDate) 
  SELECT (lcTempFile)
  REPLACE PayDate WITH ldPayDate ,;
          cStatus WITH IIF(cStatus='A','A','M')
 
ENDIF

= lfwBrow()
SELECT (lnAlias)

*:*************************************************************************
*! Name      : lfvAmount
*! Developer : Ahmed Amer (AHM) 
*! Date      : 08/28/97
*! Purpose   : Validate POST DATED CHECKS AMOUNT in the Screen.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvAmount()
*:*************************************************************************

FUNCTION lfvAmount
PRIVATE lnAlias

lnAlias = SELECT(0)

*IF !EMPTY(lnAmount)  
IF lnAmount > 0 
  SELECT (lcTempFile)
  REPLACE Amount  WITH lnAmount ,;
          cStatus WITH IIF(cStatus='A','A','M')
ELSE  
  *-- the Payment Amount should be greater than zero
  *-- Ok
  = gfModalGen("INM040965B00000","Dialog")
  _CUROBJ = OBJNUM(lnAmount)

ENDIF

= lfwBrow()
SELECT (lnAlias)

*:*************************************************************************
*! Name      : lfvComment
*! Developer : Sameh (SSE)
*! Date      : 04/19/1999
*! Purpose   : Validate Comment1 & Comment2 in the Screen.
*:*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*:*************************************************************************
*! Passed Parameters  : None
*:*************************************************************************
*! Returns            : None
*:*************************************************************************
*! Example   : = lfvComment()
*:*************************************************************************
*
FUNCTION lfvComment
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
SELECT (lcTempFile)
IF ALLTRIM(RIGHT(SYS(18),1)) = '1'
  REPLACE Ccomment1  WITH lcComment1 , cStatus WITH IIF(cStatus='A','A','M')
ELSE
  REPLACE Ccomment2  WITH lcComment2 , cStatus WITH IIF(cStatus='A','A','M')
ENDIF          

= lfwBrow()
SELECT (lnCurAlias)
*-- end of lfvComment.
