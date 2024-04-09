*:***************************************************************************
*: Program file  : SOAND20
*: Program desc. : Order Approval.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO )
*: Developer     : Ashraf Sherif Mohammad (ASH)
*: Customer      : Andrew
*:***************************************************************************
*: Calls : lfvAccount,lfOldValue,lfBrows,lfwBrow,lfReadAct,lfvDeact,
*:         lpTrapKey,lfvApprove,lfvApproval,lfvApprAmt,lfvHold,lfvDecline,
*:         lfvPrepaid,lfvResub,lfvSelect,lfvDecCode,lfvNoteCode,lfvActBrow
*:***************************************************************************
*C101387,1 ASH 03/03/99
*Notes : This program was copied from AND200 in ver 2.6.
*:***************************************************************************

DIMENSION laData[1],laCodInfo[1,10],laDeclCode[1]
laCodInfo = ' '
laDeclCode = ' '
lnDeclCode = 0
lcOldValue = ' '
lcAccount = SPACE(5)
lcNotes   =  ' '
lcDecline = ' '
lcReason  = ' '
lnApprAmt = 0
lcBrTit   = 'ORDERS BROWSE'
lcCode    = SPACE(10)
STORE SPACE(30) TO lcBtname,lcBtaddr1,lcBtaddr2,lcCity
llBrowse = .F.
laCodInfo[1,01] = "DECL_CODE"                 && Field Name
laCodInfo[1,02] = "laDeclCode"                && Array Name
laCodInfo[1,03] = "lnDeclCode"                && Popup Name
laCodInfo[1,04] = ""                        && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.                       && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.                       && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = "OrdHdr"                && Alternative File (For default val.)
laCodInfo[1,08] = "ORDACCT"                && Use this index for the Alternative file.
laCodInfo[1,09] = "lcDecline"               && Seek this expretion.
laCodInfo[1,10] = "DECL_CODE"                 && Alternative Field Name
IF !gfSetup()
  RETURN
ENDIF

= gfwCodePop(@laCodInfo, "DECL_CODE", "N")
*--Open files
SELECT OrdHdr
SET ORDER TO ORDACCT
lcFilter = "STATUS $ 'OH'.AND.OPENAMT>0"

LOCATE ALL FOR &lcFilter
IF EOF()
  =gfModalGen('SOM32060B00000', 'DIALOG')
  RETURN
ENDIF
SET FILTER TO &lcFilter
SET KEY TO lcAccount
DO (gcScrDir + gcWinAppl + '\SOAND201.SPR')

FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Account code field.
*!*************************************************************
*! Calls     : lfwBrow(),lfRefresh()
*!*************************************************************

FUNCTION lfvAccount

IF !llBrowse .AND. lcOldValue = lcAccount 
  RETURN
ENDIF


IF llBrowse OR (!EMPTY(lcAccount) AND !SEEK('M'+lcAccount,'Customer'))
  DO CUSBROWM WITH lcAccount
  llBrowse = .F.
 _CUROBJ = OBJNUM(lcAccount)
ENDIF
IF EMPTY(lcAccount) 
  RETURN
ENDIF

llBrowse = .F.
lcBtname  = Customer.BTNAME
lcBtaddr1 = Customer.cAddress1
lcBtaddr2 = Customer.cAddress2
lcCity    = TRIM(Customer.cAddress3)+ ' ' + Customer.cAddress4 + ' '+TRIM(Customer.cAddress5)
=lfRefresh()
SELECT ORDHDR
SET KEY TO lcAccount
=SEEK(lcAccount)
SHOW GET ibAccount DISABLE
SHOW GET lcAccount DISABLE
SHOW GETS WINDOW 'SOAND204' ENABLE
=lfwBrow()

*!*************************************************************
*! Name      : lfBrows
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To browse the Orders for the selected account.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfBrows
SELECT ORDHDR
BROWSE FIELDS ;
       Order     :H = 'Order'        :R,;
       Status    :H = 'S'            :R,;
       Season    :H = 'SE'           :R,;
       Store=IIF(MULTI='Y','*Multi*',STORE)   :H = 'STORE'        :R,;
       cFacCode  :H = 'FACT'         :R,;
       Complete  :H = 'COMPLETE'     :R,;
       Openamt   :H = 'OPEN'         :R,;
       Approval  :H = 'APPROV-NO'    :R,; 
       Appramt   :H = 'APPROVED'     :R;
       WINDOW    SOAND202  ;
       IN WINDOW SOAND201 ;
       WHEN lfwBrow()    ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       TITLE lcBrTit

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To be proceeded while moving through the browse.
*!*************************************************************
*! Calls     : gfCodDes(),gfwCodePop(),lfRefresh
*!*************************************************************

FUNCTION lfwBrow
SELECT OrdHdr
SHOW WINDOW (lcBrTit) REFRESH SAME
lcCode   = Approval
lcNotes  = Note1
lcDecline= Decl_Code
lcReason = gfCodDes(lcDecline,'DECL_CODE ')
lnApprAmt= ApprAmt
IF Status = 'O' AND ApprAmt <> 0
  SHOW GET lcCode    ENABLE
  SHOW GET lnApprAmt ENABLE
ELSE
  SHOW GET lcCode    DISABLE
  SHOW GET lnApprAmt DISABLE  
ENDIF
IF APPRAMT <> 0 .OR. APPROVAL = 'DECLINE'
  SHOW GET lcNotes ENABLE
ELSE
  SHOW GET lcNotes DISABLE
ENDIF
IF APPROVAL = 'DECLINE'
  = gfwCodePop(@laCodInfo, "DECL_CODE", "V,"+lcDecline)
  SHOW GET lnDeclCode ENABLE
  SELECT OrdHdr
  SET KEY TO lcAccount  
ELSE
  lnDeclCode = 0
  SHOW GET lnDeclCode DISABLE
ENDIF
=lfRefresh('SOAND203')

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : The READ Activate function of screen SOAND200
*!*************************************************************
*! Calls     : 
*!*************************************************************

FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC 
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 

*!*************************************************************
*! Name      : lfvDeact
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : The READ Deactivate function of screen AND200.
*!*************************************************************
*! Calls     : lpTrapKey
*!*************************************************************

FUNCTION lfvDeact

IF WONTOP() = lcBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC DO lpTrapKey WITH 'SOAND204', 'PbClose', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH 'SOAND203', 'lcCode'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'SOAND204', 'PbClose'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To handle the Trapping of keys.
*!*************************************************************
*! Calls     : 
*!*************************************************************

PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvApprove
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Approve button.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfvApprove

SELECT ORDHDR
=RLOCK()
REPLACE STATUS     WITH 'O' ,;
        NOTE1      WITH DTOC(DATE()),;
        Approval   WITH  '' ,;
        APPRAMT    WITH ROUND(OPENAMT,0)+1 ,;
        DECL_CODE  WITH ''   ,;
        DECL_DATE  WITH {}  

=gfAdd_Info()
UNLOCK
=lfwBrow()  
_CUROBJ = OBJNUM(lcCode)

*!*************************************************************
*! Name      : lfvApproval
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Approval number.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfvApproval

SELECT ORDHDR
=RLOCK()
REPLACE Approval WITH lcCode
UNLOCK
=lfwBrow()  

*!*************************************************************
*! Name      : lfvApprAmt
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Approval amount.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfvApprAmt

SELECT ORDHDR
=RLOCK()
REPLACE ApprAmt WITH lnApprAmt
UNLOCK
=lfwBrow()  

*!*************************************************************
*! Name      : lfvHold
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Hold button.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfvHold

SELECT ORDHDR
=RLOCK()
REPLACE APPROVAL   WITH ' ',;
        APPRAMT    WITH 0,;
        STATUS     WITH 'H'

=gfAdd_Info()
UNLOCK        
=lfwBrow()  

*!*************************************************************
*! Name      : lfvDecline
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Decline button.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfvDecline
lcNotes   =	DTOC(DATE())
SELECT ORDHDR
= RLOCK()

REPLACE STATUS     WITH 'H',;
        NOTE1      WITH lcNotes,;
        APPRAMT    WITH 0 ,;
        APPROVAL   WITH 'DECLINE' 

=gfAdd_Info()
UNLOCK
= gfwCodePop(@laCodInfo, "DECL_CODE", "N")
SHOW GET lnDeclCode enable
_CUROBJ = OBJNUM(lnDeclCode)

*!*************************************************************
*! Name      : lfvPrepaid
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Prepaid button.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfvPrepaid

SELECT ORDHDR
=RLOCK()
REPLACE APPROVAL   WITH 'PREPAID',;
        APPRAMT    WITH 0,;
        STATUS     WITH 'H',;
        DECL_CODE  WITH ''  ,;
        DECL_DATE  WITH {}  

=gfAdd_Info()
UNLOCK
=lfwBrow()
RETURN

*!*************************************************************
*! Name      : lfvResub
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Resubmitted button.
*!*************************************************************
*! Calls     : lfwBrow()
*!*************************************************************

FUNCTION lfvResub

SELECT ORDHDR
=RLOCK()
REPLACE APPROVAL   WITH 'RESUB.',;
        APPRAMT    WITH 0,;
        STATUS     WITH 'H',;
        DECL_CODE  WITH ''   ,;
        DECL_DATE  WITH {}  

=gfAdd_Info()
UNLOCK
=lfwBrow()  
RETURN

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Select button.
*!*************************************************************
*! Calls     : lfRefresh(),lfwBrow()
*!*************************************************************

FUNCTION lfvSelect

lcAccount = SPACE(5)
lnDeclCode = 0
STORE ' ' TO lcBtname,lcBtaddr1,lcBtaddr2,lcCity
SELECT ORDHDR
SET KEY TO lcAccount
=SEEK(lcAccount)
STORE ' ' TO lcCode,lcDecline,lcReason,lcNotes
=lfRefresh()
=lfwBrow()
SHOW GET ibAccount ENABLE
SHOW GET lcAccount ENABLE
SHOW GETS WINDOW 'SOAND204' DISABLE
SHOW GET pbClose ENABLE
_CUROBJ = OBJNUM(lcAccount)

*!*************************************************************
*! Name      : lfvDecCode
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Decline code field.
*!*************************************************************
*! Calls     : 
*!*************************************************************

FUNCTION lfvDecCode
if empty(laDeclCode[lnDeclCode,2])
  _CUROBJ = OBJNUM(lnDeclCode)
else
  SELECT ORDHDR
  =RLOCK()
  REPLACE DECL_CODE WITH laDeclCode[lnDeclCode,2]
  UNLOCK
  SHOW GET lnDeclCode
  =lfwBrow()  
endif

*!*************************************************************
*! Name      : lfvNoteCode
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To validate Notes field.
*!*************************************************************
*! Calls     : 
*!*************************************************************

FUNCTION lfvNoteCode

SELECT ORDHDR
=RLOCK()
REPLACE NOTE1 WITH lcNotes
UNLOCK

*!*************************************************************
*! Name      : lfvActBrow
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 03/03/99
*! Purpose   : To Browse the accounts.
*!*************************************************************
*! Calls     : 
*!*************************************************************

FUNCTION lfvActBrow
PARAMETERS lcObjName
llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)

