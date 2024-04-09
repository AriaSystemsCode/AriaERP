*:************************************************************************
*:
*: Procedure file: APOPERA.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Malak Hanna Aziz
*:      Copyright (c) 
*:  Last modified: 09/12/95
*:
*:  Procs & Fncts: 
*:  
*:  This program "Cutting ticket operations" is calling from  
*:  AP menu-->   
*:            Transactions-->
*:                     Invoice receipts-->
*:                                  Cutting ticket operations        
*:      Documented 09/12/1995
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300683,1                AND Add prgs directory path to the calling of programs
*:************************************************************************
* 
*E300683,1 Call programs from PRGS directory
*DO APPyInv WITH  .F.,.F.,4
DO (gcAppHome + gcWinAppl + '\APPyInv') WITH  .F.,.F.,4
*E300683,1 end

*!*************************************************************
*! Name      : lfwOprBrow
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : When function for browse window.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwOprBrow()
*!*************************************************************
FUNCTION lfwOprBrow

lnOprMarkr = RECNO('MFGOPRDT')
SHOW WINDOW (lcOprTtl) REFRESH SAME


*!*************************************************************
*! Name      : lfBrwOpr
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : Function to activate the browse window.
*!*************************************************************
*! Calls     : lfwOprBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrwOpr()
*!*************************************************************
FUNCTION lfBrwOpr
PRIVATE lcClrSchm

*E100219,1 Activate browse command.
SELECT MFGOPRDT
lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")

lnOprMarkr = RECNO()
lcBrwStr = ;
  "CMARKER=IIF(RECNO()=lnOprMarkr,'',' '):1:H=' ':W=.F.,"+;
  "cTktNo            :R :H='C/T No'     :6  ," +;
  "cOprCode          :R :H='Opr'        :3  ," +;
  "MFGOPRHD.cOprDesc :R :H='Description':"+IIF(_DOS,'21','16')+","+;
  "cLotNo            :R :H='Lot'        :3  ," +;
  "nInvAmount        :R :H='Applied'    :10 ," +;
  "dTranDate         :R :H='Recv. Date' :10 ," +;
  "lnInvAmnt=IIF(EOF(lcTmpPORec),0.00,&lcTmpPORec..nInvAmount):R :H='Inv Amount' :10," +;
  "MFGOPRHD.cOutCode :R :H='Output Code':12 ," +;
  "cPrdClr           :R :H='Item'       :6" 

BROWSE                 ;
  FIELDS &lcBrwStr     ;
  WINDOW APOPER1       ;
  IN WINDOW AWDAPOPERA ;
  LOCK 0               ;
  NOAPPEND             ;
  NOCLEAR              ;
  NODELETE             ;
  SAVE                 ;
  WHEN  lfwOprBrow()   ;
  TITLE lcOprTtl       ;
  FOR TranCd $ '25' AND (Receive + Damage + Cancel) > 0; 
  NOEDIT &lcClrSchm


*!*************************************************************
*! Name      : lfReadAct
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : READ Activate function.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

*E100219,1 Restore old key settings and clear the current settings
ON KEY
ON KEY LABEL ALT+O ACTIVATE WINDOW (lcOprTtl)


*!*************************************************************
*! Name      : lfROperDec
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : READ Deactivate function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfROperDec()
*!*************************************************************
FUNCTION lfROperDec

IF WONTOP() = lcOprTtl
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC     DO lpOprTab WITH 'APOPER2', 'pbOprCls'
  ON KEY LABEL TAB     DO lpOprTab WITH 'APOPER2', 'pbOprInv'
  ON KEY LABEL BACKTAB DO lpOprTab WITH 'APOPER2', 'pbOprCls'
ENDIF  
RETURN .F.


*!*************************************************************
*! Name      : lpOprTab
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: lcWindName --> Window Name.
*!             lcObjName  --> Object Name.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : DO lpOprTab WITH 'MFG300_4', 'pbNewBat'
*!*************************************************************
PROCEDURE lpOprTab
PARAMETERS lcWindName, lcObjName

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)


*!*************************************************************
*! Name      : lfvOprInv
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : Valid function for push button < Invoice >.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOprInv()
*!*************************************************************
FUNCTION lfvOprInv

IF EOF(lcTmpPORec)
  *E100219,1 Get the default invoice amount value.
  =SEEK('O' + MFGOPRDT.cTktNo  + MFGOPRDT.cOprCode + '1'    +;
  PADR((MFGOPRHD.cOutCode),12) + MFGOPRDT.cPrdClr,'BOMLINE')
  lnTmpAmnt = MFGOPRDT.nInvAmount - ;
              (BOMLINE.UnitCost * BOMLINE.UnitQty * MFGOPRDT.Receive)
  lnOprInv  = IIF(lnTmpAmnt > 0, lnTmpAmnt, 0.00)
ELSE
  *E100219,1 Get the old invoice amount from temp file.
  lnOprInv = &lcTmpPORec..nInvAmount
ENDIF

lcTitOper_A = 'Apply invoice amount'
*E300683,1 Call *.SPR from screens directory
* DO APOPER_A.SPR 
DO (gcScrDir + gcWinAppl + '\APOPER_A.SPR')
*E300683,1 end          

SELECT (lcTmpPORec) 
*E100219,1 Update the total applied amount.
SUM(&lcTmpPORec..nInvAmount) TO lnTAplAmnt
SHOW GET lnTAplAmnt
SELECT MFGOPRDT
SHOW WINDOW (lcOprTtl) REFRESH SAME


*!*************************************************************
*! Name      : lfvOprAOk
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : Valid function for push button <   Ok   >.
*!             Called from screen APOPER_A.SPR. 
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOprAOk()
*!*************************************************************
FUNCTION lfvOprAOk

*E100219,1 Check if invoice amount is less than 0.
IF lnOprInv <= 0
  ** MESSAGE : " ð should be greater than ð."
  **           "            ® Ok ¯          "
  =gfModalGen("TRM04072B00000","DIALOG",'Invoice amount|0')
  _CUROBJ = OBJNUM(lnOprInv)
  RETURN
ENDIF

*E100219,1 Update temp file.
SELECT (lcTmpPORec) 
IF EOF()
  APPEND BLANK
  REPLACE                              ;
    cTktNo     WITH MFGOPRDT.cTktNo   ,;
    cOprCode   WITH MFGOPRDT.cOprCode ,;
    cLotNo     WITH MFGOPRDT.cLotNo   ,;
    cPrdClr    WITH MFGOPRDT.cPrdClr  ,;    
    TranCd     WITH MFGOPRDT.TranCd   ,;        
    nInvAmount WITH lnOprInv
ELSE
  REPLACE nInvAmount WITH lnOprInv
ENDIF

CLEAR READ


*!*************************************************************
*! Name      : lfvOprOk
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : Valid function for push button <   Ok   >.
*!             Called from screen APOPERA.SPR. 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOprOk()
*!*************************************************************
FUNCTION lfvOprOk

*E100219,1 Update the invoice amount "laData[12]"
SELECT(lcTmpPORec)
SUM(&lcTmpPORec..nInvAmount) TO laData[12]

SELECT cTktNo, SUM(nInvAmount) AS 'nTotAmnt' ;
  FROM &gcWorkDir.&lcTmpPORec;
  GROUP BY cTktNo;
  INTO CURSOR &lcTmpCost


*!*************************************************************
*! Name      : lfvOprCls
*! Developer : MALAK - Malak Hanna
*! Date      : 09/12/1995
*! Purpose   : Valid function for push button < Cancel >.
*!             Called from screen APOPERA.SPR. 
*!*************************************************************
*! Calls     : lfvOprCls
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOprCls()
*!*************************************************************
FUNCTION lfvOprCls

SELECT(lcTmpPORec)
ZAP
laData[12] = 0.00