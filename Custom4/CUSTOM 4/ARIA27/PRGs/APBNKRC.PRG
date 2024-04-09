*:************************************************************************
*:
*: Procedure file: APBNKRC.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:  Last modified: 05/15/95
*:
*:  Procs & Fncts: lfDispBrow
*:               : lfwBrow
*:               : lfDispFrst
*:               : lfvScope
*:               : lfvClose
*:               : lfShow
*:               : lfvFrTodat
*:               : lfShowpb
*:               : lfvOpen
*:               : lfvClear
*:               : lfvVoid
*:               : lfvCbStat
*:               : lfvRecon
*:               : lfvCancel
*:               : lfDefBank
*:               : lfwOldVals
*:               : lfvDat
*:               : lfvBnkChk
*:               : lfDeAct
*:               : lfBrowTrap
*:               : lpDoTrap
*:               : lfBrowUnTrap
*:               : lfPushKey
*:               : lfActBrows
*:               : lfDefinePad
*:               : lpTrap
*:               : lfvOrder   
*:               : lfCreatTmp
*:               : lfInsrtRow
*:               : lfGenTmp
*:
*:      Documented 00/00/1994
*:************************************************************************
*B500708,4 05/15/95 Hesham El-Sheltawi Changed the date variables 
*B500708,4                             [ldFrPrdRng , ldToPrdRng ,ldOFrPdRng,ldOToPdRng] to character variables 
*B500708,4                             [lcFrPrdRng , lcToPrdRng ,lcOFrPdRng,lcOToPdRng] and add function lfwFrTodat 
*B600491,1 06/21/95 MAN   Fixed the bug of the screen close
*B600492,1 Reham On 06/21/95
*B600492,1 change the browse variable under windows.
*B800588,2 M.H On 05/22/96 Show the book balance in all cases with the total of the cleared & open.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300921,1 TAK 07/12/98 Added changes due to Point of Sale module (cash register).
*E300921,1              Added a bank type (Bank/Safe) and GL Cash account
*E300921,1              which used only in (S)afe Case. 
*E300921,1              Changed the path of the screen due to sharing.
*E301077,71 AMM 02/18/99 Reduce opened files 
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*E301266,1  AKA 06/17/99 Adding more options in bank reconc. screen 
*E301266,1               1)Sorted by Transaction type or Transaction Date.
*E301266,1               2)Display AR tran. that has same AR batch no. into one record. 
*B803925,1  AME 05/06/01 Repaire the bug of jumping to the last record when change status to 
*B803925,1  AME          to clear while the scope doesn't permit clear. 
*B607010,1  ABD 03/31/2003 Fix Error While selecting trans dates in Bank Reconciliation, that 
*B607010,1  ABD           Variable paydate not.
*B131475,1 HBG 18/04/2006 Change status void to be Hide 
*B608912,1 TMI 07/01/2009 Fix the bug that the scope filter does not work correctly [T20090617.0116] 
*:************************************************************************

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laField_H,laCtrStat
PRIVATE laBankObjs
DECLARE laRecNo[1], laBankObjs[2,3]
** laRecNo       Array hold records add to the filter.
** laBankObjs    Array to hold bank objects'names for validations      
** lcFuncName    Variable to hold Refresh window. 
** lcBrowName    Variable to hold Brows Window name. 
** lcReadNam1    Variable to hold Read Window name.
** lcReadNam2    Variable to hold Read Window name.
** lcExprsion    Variable to hold filter expresion.
** lcOrExpr      Variable to hold or expresion.
** lcBankCode    Variable to hold bank code.
** lcOldVal      Variable to hold old field contents
** lcOldBank     Variable to hold old bank code.
** lcOldCheck    Variable to hold old checking account code.
** lcObjDisp     Variable to hold objects status.
** lcORecst      Variable to hold old request. 
** lcTFrom       Variable to hold the word from.
** lcTThrou      Variable to hold the word through.
** lcEscTrp      Variable to hold escape trap.
** lcBrowFile    Vairable to hold the Browse File.
** lcBrTtl       Variable to hold Browse Window Title.
** lcCheckcode   Variable to hold check code. 
**
** lnBrRecN1     browse record number 
** lnPayment     Variable to hold 
** lnCleared     Variable to hold cleared amount.
** lnOpen        Variable to hold open amount.
** lnBook        Variable to hold book amount.
** lnArlen       Variable to hold array dimension.
** lnOldRange    Variable to hold old radio button range.
**
** lcFrPrdRng    Variable to hold from period range.
** lcToPrdRng    Variable to hold to period range.
** ldFrDatRng    Variable to hold from date range.
** ldToDatRng    Variable to hold date range. 
** lcOFrPdRng    Variable to hold old from period range.
** lcOToPdRng    Variable to hold old to period range.
** ldOFrDtRng    Variable to hold old from date range.
** ldOToDtRng    Variable to hold old to date range. 
**
** cbOpen        Variable to hold check box open.
** cMarker       Vairable to mark current record.
** cbClear       Variable to hold check box clear. 
** cbVoid        Variable to hold check box void.
**
** llOldOpen     Variable to hold old check box open.
** llOldClear    Variable to hold old check box clear.
** llNoContrl    no control panel (AP.PRG)
** llOldVoid     Variable to hold old check box void.
** llrbldexp     Variable to hold if rebuild expresion or not.
** llBrowse      Variable to hold flag in case of activate browse by mouse.
** llScope       Variable to hold if creating a scope or not.
** llFrstShw     Variable to hold show at the frist time.
**
** rbRange       Variable to hold radio button range.
** rbBrwsOrd     Variable to hold radio button order by.   
** lnOldOrder    Variable to hold old radio button order by.
** rbBrwsData    Variable to hold radio button summary/detail.   
** lnOldBrDat    Variable to hold old radio button summary/detail.   
** lcTmpApPay    Varible to hold a Temp. name for a APPAYMNT table

**
** laCtrStat     To disable the browse pad in the menu
** lcOldPriod    to Store last value of the period objects
STORE "Bank/Checking Acct. entries" TO lcBrTtl 
STORE ""           TO lcEscTrp
STORE "APINVHDR"   TO lcBrowFile
STORE SPACE(12)    TO lcCheckcode 
STORE 'DISABLE'    TO laCtrStat   
STORE 0            TO lnBrRecN1  , lnPayment
STORE 0.00         TO lnCleared  , lnOpen    , lnBook
STORE 1            TO lnArlen    , rbRange   , lnOldRange  

*E301266,1 (Start)
STORE 1            TO rbBrwsOrd  , lnOldOrder  
STORE 1            TO rbBrwsData , lnOldBrDat    
STORE ''           TO lcTmpGLTDt   
lcTmpApPay         = gfTempName()        
lcTmpIndx          = gfTempName()        
*E301266,1 (End)

STORE .T.          TO cbOpen     , llOldOpen , cbClear    , llOldClear ,;
                      llNoContrl
STORE .F.          TO cbVoid     , llOldVoid , llrbldexp  , llBrowse   ,;
                      llScope    , llFrstShw
STORE {}           TO  ldFrDatRng , ldToDatRng ,ldOFrDtRng , ldOToDtRng
STORE ' '          TO lcFuncName , cMarker   , lcBrowName , lcReadNam1 ,;
                      lcReadNam2 , lcExprsion, lcOrExpr   , lcBankCode ,;
                      lcOldBank  , lcOldCheck, lcOldVal, lcObjDisp  , lcORecst   ,;
                      lcTFrom    , lcTThrou , lcFrPrdRng , lcToPrdRng  ,lcOldPriod,;
                      lcOFrPdRng,lcOToPdRng
*E300921,1 Initialize the bank type as bank.
lcBnkType = 'B'
*E300921,1 End.


IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)
  llFrstShw   = .T.
   *** Variable holding Filter expresion ***
  
  *E301266,1 (Start)
  *lcExprsion = "APPAYMNT.CPAYSTAT <> 'V'.AND."+;
  *             "APPAYMNT.CPAYMETH <> 'H'"

  lcExprsion = "CPAYSTAT <> 'V'.AND."+;
               "CPAYMETH <> 'H'"
  
  *E301266,1 (End)
  lcBrowName = gfTempName()   
  lcReadNam1 = gfTempName()
  lcReadNam2 = gfTempName()

  =lfDefBank()
  *** Prepare an array to hold bank objects to be used for
  *** global bank and checking accounts validations as follows :
  *** One row for every object, such that 
  *** row no. 1 holds bank object names,
  *** row no. 2 holds checking account object names
  *** row no. 3 holds the corresponding G/L account object names,
  *** Columns are ordered as follows :
  *** Column no. 1 : invisible button name for corresponding object
  *** Column no. 2 : object name (e.g. bank object name)
  *** Column no. 3 : object description name(if required)
  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'lcBankCode'     && Bank code 
  laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
  laBankObjs[2,2] = 'lcCheckCode'    && Checking account 

ENDIF

*E300921,1 Open Ap setup file only if AP was installed.
*E301077,71 AMM Open it when needed.
*IF (OCCURS('AP',gcCmpModules)<>0)
  *=gfOpenFile(gcDataDir+'APSETUP','','SH')
*ENDIF
*E301077,71 AMM end

*E300921,1 End.

*** store scape trap.
lcEscTrp=ON("KEY","ESC")

*** Trap some keys before browse. ***
PUSH KEY

=lfPushKey()
SELECT APPAYMNT
*E301266,1  (Strat)
INDE ON cbnkcode+cchkacct+batch TAG chkbtch OF (gcWorkDir + lcTmpIndx+ '.CDX') 
*E301266,1  (End)


*E301266,1  (Strat)
*! Copy all records of appaymnt table to temp table
*! To copy all record as detailed change the setting of rbBrwsData variable.

*SET RELATION TO CPAYCLNO INTO APVENDOR
rbBrwsData = 2
DO lfGenTmp
* Reset value of rbBrwsData. 
rbBrwsData = 1    
SELECT PayTemp
SET RELATION TO CPAYCLNO INTO APVENDOR
*E301266,1  (End)


lcSavOrd = ORDER() 
SET ORDER TO 0
*B800588,2 M.H On 05/22/96 Begin.
*SET FILTER TO APPAYMNT.CBNKCODE=lcBankCode .AND. EVALUATE(lcExprsion)

*E301266,1  (Strat)
*SET FILTER TO APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT=lcBankCode+lcCheckCode .AND. EVALUATE(lcExprsion)
*E301266,1  (End)

*B800588,2 M.H On 05/22/96 End.
SET ORDER TO &lcSavOrd

*E300683,1 Call *.SPR from screens directory
* DO APBNKRC.SPR 
*E300921,1 Change path due to sharing of this screen.
*DO (gcScrDir + gcWinAppl + '\APBNKRC.SPR')
DO (gcScrDir + 'APBNKRC.SPR')
*E300921,1 End.
*E300683,1 end          

SELECT APPAYMNT
SET RELATION TO
SET FILTER TO


*** restores on key labels that were placed on the stack with push key. ***
POP KEY

RELEASE PAD _BROWSE OF _MSYSMENU
***Release the browse window.
RELEASE WINDOW (lcBrTtl)

IF glQuitting
  HIDE WINDOW (gcBaseWind)
  *khm1
   = lfCreatTmp(.F.)
   *khm1
ENDIF

glFromBrow  = .F.

*!**************************************************************************
*!
*!      Function: lfDispBrow
*!
*!**************************************************************************
* Display browse.
FUNCTION lfDispBrow
PRIVATE lcCurAlias

lcCurAlias = ALIAS()


*E301266,1  (Strat)
*SELECT APPAYMNT
*lnBrRecN1  = RECNO('APPAYMNT')
SELECT PayTemp
lnBrRecN1  = RECNO('PayTemp')
*E301266,1  (End)


*B600492,1 change the browse variable under windows.
IF _WINDOWS
*E300921,1 Change the fields lenth in browse due to change font with standard.
*  lcBrowStr =  "cMarker=IIF(RECNO()=lnBrRecN1,'',' '):1:H=' ':W=.F.,"+; 
               "lcDummy1=IIF(CPAYMETH='P','Prn Chk',IIF(CPAYMETH='M','Man Chk',IIF(CPAYMETH='N',IIF(CPAYTYPE='P','Non Chk','Non A/R'),"+;
               "IIF(CPAYMETH='B',IIF(CPAYMETH='B' AND NPAYAMNT < 0,'Deposit','Charge'),'A/R')))) :R :H='Tr. Type',"+;
               "CPAYDOCNO :R :H='Payment#':8,"+;
               "DPAYDATE  :R :H='Date':10,"+;
               "CPAYCOMP=IIF(EMPTY(cpaycomp) .AND. CPAYTYPE = 'P',APVENDOR.CVENCOMP,cpaycomp):R :H='Comment/Payee name':14,"+;
               "lcDummy2=IIF(CPAYRECST='O','Open',IIF(CPAYRECST='C','Clear','Void')) :R :H='Status':6,"+;
               "Amount = -NPAYAMNT :15  "

  *B131475,1 HBG 18/04/2006 Change status vid to be Hide [Begin]
  *lcBrowStr =  "cMarker=IIF(RECNO()=lnBrRecN1,'>',' '):1:H=' ':W=.F.,"+; 
               "lcDummy1=IIF(CPAYMETH='P','Prn Chk',IIF(CPAYMETH='M','Man Chk',IIF(CPAYMETH='N',IIF(CPAYTYPE='P','Non Chk','Non A/R'),"+;
               "IIF(CPAYMETH='B',IIF(CPAYMETH='B' AND NPAYAMNT < 0,'Deposit','Charge'),'A/R')))) :R :H='Tr. Type',"+;
               "CPAYDOCNO :R :H='Payment#':14,"+;
               "DPAYDATE  :R :H='Date':10,"+;
               "CPAYCOMP=IIF(EMPTY(cpaycomp) .AND. CPAYTYPE = 'P',APVENDOR.CVENCOMP,cpaycomp):R :H='Comment/Payee name':34,"+;
               "lcDummy2=IIF(CPAYRECST='O','Open',IIF(CPAYRECST='C','Clear','Void')) :R :H='Status':9,"+;
               "Amount = -NPAYAMNT :15 " 
  lcBrowStr =  "cMarker=IIF(RECNO()=lnBrRecN1,'>',' '):1:H=' ':W=.F.,"+; 
               "lcDummy1=IIF(CPAYMETH='P','Prn Chk',IIF(CPAYMETH='M','Man Chk',IIF(CPAYMETH='N',IIF(CPAYTYPE='P','Non Chk','Non A/R'),"+;
               "IIF(CPAYMETH='B',IIF(CPAYMETH='B' AND NPAYAMNT < 0,'Deposit','Charge'),'A/R')))) :R :H='Tr. Type',"+;
               "CPAYDOCNO :R :H='Payment#':14,"+;
               "DPAYDATE  :R :H='Date':10,"+;
               "CPAYCOMP=IIF(EMPTY(cpaycomp) .AND. CPAYTYPE = 'P',APVENDOR.CVENCOMP,cpaycomp):R :H='Comment/Payee name':34,"+;
               "lcDummy2=IIF(CPAYRECST='O','Open',IIF(CPAYRECST='C','Clear','Hide')) :R :H='Status':9,"+;
               "Amount = -NPAYAMNT :15 " 
  *B131475,1 HBG 18/04/2006 [End]
*E300921,1 end.

ELSE
  *B131475,1 HBG 18/04/2006 Change status vid to be Hide [Begin]
  *lcBrowStr =  "cMarker=IIF(RECNO()=lnBrRecN1,'',' '):1:H=' ':W=.F.,"+; 
               "lcDummy1=IIF(CPAYMETH='P','Prn Chk',IIF(CPAYMETH='M','Man Chk',IIF(CPAYMETH='N',IIF(CPAYTYPE='P','Non Chk','Non A/R'),"+;
               "IIF(CPAYMETH='B',IIF(CPAYMETH='B' AND NPAYAMNT < 0,'Deposit','Charge'),'A/R')))) :R :H='Tr. Type',"+;
               "CPAYDOCNO :R :H='Payment#':8,"+;
               "DPAYDATE  :R :H='Date':10,"+;
               "CPAYCOMP=IIF(EMPTY(cpaycomp) .AND. CPAYTYPE = 'P',APVENDOR.CVENCOMP,cpaycomp):R :H='Comment/Payee name':18,"+;
               "lcDummy2=IIF(CPAYRECST='O','Open',IIF(CPAYRECST='C','Clear','Void')) :R :H='Status':6,"+;
               "Amount = -NPAYAMNT :15  "
  lcBrowStr =  "cMarker=IIF(RECNO()=lnBrRecN1,'',' '):1:H=' ':W=.F.,"+; 
               "lcDummy1=IIF(CPAYMETH='P','Prn Chk',IIF(CPAYMETH='M','Man Chk',IIF(CPAYMETH='N',IIF(CPAYTYPE='P','Non Chk','Non A/R'),"+;
               "IIF(CPAYMETH='B',IIF(CPAYMETH='B' AND NPAYAMNT < 0,'Deposit','Charge'),'A/R')))) :R :H='Tr. Type',"+;
               "CPAYDOCNO :R :H='Payment#':8,"+;
               "DPAYDATE  :R :H='Date':10,"+;
               "CPAYCOMP=IIF(EMPTY(cpaycomp) .AND. CPAYTYPE = 'P',APVENDOR.CVENCOMP,cpaycomp):R :H='Comment/Payee name':18,"+;
               "lcDummy2=IIF(CPAYRECST='O','Open',IIF(CPAYRECST='C','Clear','Hide')) :R :H='Status':6,"+;
               "Amount = -NPAYAMNT :15  "
  *B131475,1 HBG 18/04/2006 [End]
ENDIF
IF _DOS
  BROWSE FIELDS &lcBrowStr;
            WINDOW (lcBrowName) ;
            WHEN lfwBrow() .AND. lfBrowTrap();
            VALID :F lfvBrowse();
            IN WINDOW (gcBaseWind) ;
            LOCK 0;
            NOAPPEND;
            NOCLEAR;
            NODELETE;
            NOWAIT;
            NOEDIT;
            SAVE;
            COLOR SCHEME 13;
            TITLE lcBrTtl
ELSE
   BROWSE FIELDS &lcBrowStr;
            WINDOW (lcBrowName) ;
            WHEN lfwBrow() .AND. lfBrowTrap();
            VALID :F lfvBrowse();
            IN WINDOW (gcBaseWind) ;            
            LOCK 0;
            NOAPPEND;
            NOCLEAR;
            NODELETE;
            NOWAIT;
            NOEDIT;
            SAVE;
            NOMENU;
            TITLE lcBrTtl
ENDIF

SELECT (lcCurAlias)

*!**************************************************************************
*!
*!      Function: lfwBrow
*!
*!**************************************************************************
*
FUNCTION lfwBrow

glFromBrow = .T.

*E301266,1 (Start)
*lnBrRecN1  = RECNO('APPAYMNT')
lnBrRecN1  = RECNO('PayTemp')
*E301266,1 (End)

SHOW WINDOW (lcBrTtl) REFRESH

=lfShowpb()

*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse

IF WONTOP(lcBrTtl)   && Close from corner in dos. (browse exist,brows window not exist)
  IF !WEXIST(lcBrowName) 
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO APBNKRC.SPR
  ENDIF
ELSE                 && Close from corner in windows. (browse not exit, base window not vis.)
  =lfBrowUnTrap()
  IF _DOS
    =lfDefinePad()
  ENDIF
  IF !WVISIBLE(gcBaseWind)
    RELEASE PAD _BROWSE OF _MSYSMENU
    glQuitting = .T.
  ENDIF  
  =gfStopBrow()     
ENDIF    




*!**************************************************************************
*!
*!      Function: lfDispFrst
*!
*!**************************************************************************
*
FUNCTION lfDispFrst

IF llFrstShw 
  =lfvScope()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvScope
*!
*!**************************************************************************
*
FUNCTION lfvScope

**Release browse pad before running modal screen.
RELEASE PAD _BROWSE OF _MSYSMENU
**Restore keys.
POP KEY

** Restore the old check box status.
cbOpen      = llOldOpen
cbClear     = llOldClear 
cbVoid      = llOldVoid
rbRange     = lnOldRange

*E301266,1 (Start)
rbBrwsOrd     = lnOldOrder
rbBrwsData    = lnOldBrDat    
*E301266,1 (End)

*IF EMPTY(lcBankCode)
*  lcBankCode  = lcOldBank
*ENDIF
*IF EMPTY(lcCheckCode)
*  lcCheckCode = lcOldCheck
*ENDIF
lcObjDisp  = IIF(EMPTY(lcBankCode), "DISABLE", "ENABLE")
IF rbRange = 1
  lcPrdStat  = "ENABLE"
  lcDateStat = "DISABLE"
ELSE
  lcPrdStat  = "DISABLE"
  lcDateStat = "ENABLE"
ENDIF  



lcHidObjClr = SUBSTR(SCHEME(5,1),ATC('/',SCHEME(5,1))+1)
lcHidObjClr = lcHidObjClr + '/' + STRTRAN(lcHidObjClr, '*', '+')

RELEASE PAD _BROWSE OF _MSYSMENU


*E300683,1 Call *.SPR from screens directory
* DO APBRSCP.SPR 
*E300921,1 Change path due to sharing of this screen.
*DO (gcScrDir + gcWinAppl + '\APBRSCP.SPR')
DO (gcScrDir + 'APBRSCP.SPR')
*E300921,1 End.
*E300683,1 end          

IF _DOS
  =lfDefinePad()
ENDIF

** Saving the current check box status.
llOldOpen  = cbOpen     
llOldClear = cbClear    
llOldVoid  = cbVoid     
lnOldRange = rbRange     

*E301266,1 (Start)
lnOldOrder = rbBrwsOrd 
lnOldBrDat = rbBrwsData    
*E301266,1 (End)


**trap keys.
=lfPushKey()

**Redefine browse in menu.
IF _DOS
  =lfDefinePad()
ENDIF  

** IF cancel form the frist time entering the program quit the progrm.
IF llFrstShw
  glQuitting=.T.
  CLEAR READ
ENDIF

llFrstShw = .F.

IF _DOS
  =lfDefinePad()
ENDIF

***Save Scope to be an old value***
ldOToDtRng = ldToDatRng
ldOFrDtRng = ldFrDatRng
lcOToPdRng = lcToPrdRng
lcOFrPdRng = lcFrPrdRng
llOldOpen  = cbOpen
llOldClear = cbClear     
llOldVoid  = cbVoid
lnOldRange = rbRange

*E301266,1 (Start)
lnOldOrder = rbBrwsOrd 
lnOldBrDat = rbBrwsData    
*E301266,1 (End)




IF llScope  = .T. && if the user push <Recocile> to do the filter he selected.
  lnCleared = 0.00 
  lnOpen    = 0.00
  lnBook    = 0.00
  llScope   = .F.
  
   
  *MAN Select the APPAYMENT
  *B608912,1 TMI 07/01/2009 10:19:30 AM [Start] use PAYTEMP instead of APPAYMNT
  *SELECT APPAYMNT 
  SELECT PAYTEMP
  *B608912,1 TMI 07/01/2009 10:19:38 AM [End  ] 
  lcSavOrd = ORDER()  && save the old order.
  
  *B800588,2 M.H On 05/22/96 Begin.
  SET FILTER TO
  SET ORDER TO TAG CHKTMNO
  SEEK lcBankCode+lcCheckCode

  
  *SUM REST ;
  *IIF(CPAYRECST = 'C',-NPAYAMNT,0),IIF(CPAYRECST = 'O',-NPAYAMNT,0) ;
  *WHILE CBNKCODE+CCHKACCT+CPAYTYPE+CPAYMETH+CPAYDOCNO=lcBankCode+lcCheckCode ;
  *FOR APPAYMNT.CPAYSTAT <> 'V' .AND. APPAYMNT.CPAYMETH <> 'H' ;
  *TO lnCleared,lnOpen
  
  *B608912,1 TMI 07/01/2009 10:19:54 AM [Start]  use PAYTEMP instead of APPAYMNT
  *SCAN REST WHILE CBNKCODE+CCHKACCT = lcBankCode+lcCheckCode  ;
              FOR APPAYMNT.CPAYSTAT <> 'V' .AND. APPAYMNT.CPAYMETH <> 'H'
  SCAN REST WHILE CBNKCODE+CCHKACCT = lcBankCode+lcCheckCode  ;
              FOR PAYTEMP.CPAYSTAT <> 'V' .AND. PAYTEMP.CPAYMETH <> 'H'
              *B608912,1 TMI 07/01/2009 10:20:03 AM [End  ] 
    *IF APPAYMNT.cCurrCode <> gcBaseCurr
	*  * Initialize lnEqvAmnt variable
	*  lcExSin2  = ''
	*  lcExSin1  = gfGetExSin(@lcExSin2, APPAYMNT.cCurrCode ) 
	*  lnValue   = ROUND( APPAYMNT.nPayAmnt &lcExSin1 APPAYMNT.nExRate &lcExSin2 APPAYMNT.nCurrUnit ,2)
	*ELSE
	  *B608912,1 TMI 07/01/2009 10:20:18 AM [Start]  use PAYTEMP instead of APPAYMNT
      *lnValue   =  APPAYMNT.nPayAmnt
      lnValue   =  PAYTEMP.nPayAmnt
      *B608912,1 TMI 07/01/2009 10:20:21 AM [End  ] 
	*ENDIF  
	lnCleared = lnCleared + IIF(CPAYRECST = 'C', -lnValue , 0  ) 
	lnOpen    = lnOpen    + IIF(CPAYRECST = 'O', -lnValue , 0  ) 
  ENDSCAN 
  
  
  
  
  
  
  *B800588,2 M.H On 05/22/96 End.
  SET ORDER TO 0      && set order to zero to activate rushmore.
  ** Set the filter the user selected. ***
  *B800588,2 M.H On 05/22/96 Begin.
  *SET FILTER TO APPAYMNT.CBNKCODE=lcBankCode .AND. EVALUATE(lcExprsion)
  
  *E301266,1  (Start)
  *SET FILTER TO APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT=lcBankCode+lcCheckCode .AND. EVALUATE(lcExprsion)
  *E301266,1  (End)    
  
  *B800588,2 M.H On 05/22/96 End.
  SET ORDER TO &lcSavOrd         && restore the old order.

  ** Calculate cleared and open.
*B800588,2 M.H On 05/22/96 Begin.
*  SCAN
*    DO CASE
*      CASE APPAYMNT.CPAYRECST = 'C'
*        lnCleared = lnCleared - APPAYMNT.NPAYAMNT
*      CASE APPAYMNT.CPAYRECST = 'O'
*        lnOpen    = lnOpen    - APPAYMNT.NPAYAMNT
*    ENDCASE
*  ENDSCAN
*B800588,2 M.H On 05/22/96 End.

  *E301266 (Strat)
  SELECT PayTemp
  *E301266 (End)  

  lnBook = lnOpen+lnCleared
  GO TOP
  

  *E301266,1  (Strat)
  *lnBrRecN1  = RECNO('APPAYMNT') 
  lnBrRecN1  = RECNO('PayTemp')
  *E301266,1  (End)
  
  SHOW WINDOW (lcBrTtl) REFRESH  && refresh the browse window.
  SHOW GETS
  *** If the browse has no elements colse all branches no need to use.***
  IF EOF() 
    SHOW GET pbOpen    DISABLE
    SHOW GET pbClear   DISABLE
    SHOW GET pbVoid    DISABLE
    
    ** MESSAGE : "There are no bank entries matching the selected criteria."
    **           "                       ® Ok ¯ 
    =gfModalGen("TRM04118B00000","DIALOG")
  ELSE && if the browse has elements.
    =lfShowpb() 
  ENDIF
ENDIF  
SHOW GET lnCleared 
SHOW GET lnOpen
SHOW GET lnBook
*MAN

*!**************************************************************************
*!
*!      Function: lfvDat
*!
*!**************************************************************************
*
FUNCTION lfvDat

IF rbRange = 1
  SHOW GET lcFrPrdRng ENABLE
  SHOW GET lcToPrdRng ENABLE
  ldFrDatRng = {}
  ldToDatRng = {}
  SHOW GET ldFrDatRng DISABLE
  SHOW GET ldToDatRng DISABLE
ELSE          
  SHOW GET ldFrDatRng ENABLE
  SHOW GET ldToDatRng ENABLE
  lcFrPrdRng = ""
  lcToPrdRng = ""
  SHOW GET lcFrPrdRng DISABLE
  SHOW GET lcToPrdRng DISABLE
ENDIF
=lfRefresh()
*!**************************************************************************
*!
*!      Function: lfvClose
*!
*!**************************************************************************
*
FUNCTION lfvClose

*E301266,1   (Strat)
* Close temp. file 
=lfCreatTmp(.F.)
*E301266,1   (End)

IF !WONTOP(lcBrTtl)
*B600491,1 MAN 06/21/95 Remarked the following lines
*IF _DOS
    *** Close program. ***
  * glQuitting = .T.
  * CLEAR READ
 *ELSE
    *B600491,1 Reham I found that this variable is empty all the time, so it was
    *B600491,1 
    *&lcEscTrp
    glQuitting = .T.
    CLEAR READ
 *ENDIF
*B600491,1 MAN 06/21/95 END 
ELSE && IF EXIT FROM BROWSE WINDOW
  HIDE WINDOW(gcBaseWind)
  *** Close program. ***
  glFromBrow = .F.
  glQuitting = .T.
  CLEAR READ
  KEYBOARD CHR(13)
  RETURN TO APBNKRC.SPR
ENDIF  

*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
* 
FUNCTION lfShow
PARAMETERS lcObjDisp

SHOW GET lcBankCode  &lcObjDisp
SHOW GET ibBank      &lcObjDisp
SHOW GET lcCheckCode &lcObjDisp
SHOW GET ibChecks    &lcObjDisp

*!**************************************************************************
*!
*!      Function: lfwFrTodat
*!
*!**************************************************************************
* when from through date in scope window.
*
FUNCTION lfwFrTodat
lcOldPriod = EVAL(VARREAD())

*!**************************************************************************
*!
*!      Function: lfvFrTodat
*!
*!**************************************************************************
* Validate from through date in scope window.
*
FUNCTION lfvFrTodat

*B607010,1 ABD - First, Save the old alias to return it before u exit from the 
*B607010,1 ABD - The function that becasue this function open new file and change
*B607010,1 ABD - The used file to another file. [Begin]
PRIVATE lnOldAlias
lnOldAlias = SELECT (0)

*B607010,1 ABD - [End]

IF rbRange = 1 && in case of due date.
  *B500708,4 added code to valid the year+period if it is not valid
  *B500708,4 then browse file sycfsprd for the parent company
  lcObjName=VARREAD()
  IF EMPTY(EVAL(lcObjName))
    RETURN
  ENDIF
  lcYearPrd=EVAL(lcObjName)
  *E301077,71 AMM Open file.
  =gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  =gfOpenFile(lcDataDir+'FSPRD','Comfyrprdi','SH')
  *E301077,71 AMM end

  *E300692,1 Change file name from SYCFSPRD TO FSPRD
  *IF LASTKEY()=13 AND  !SEEK(gcPrnt_Cmp+RIGHT(lcYearPrd,4)+LEFT(lcYearPrd,2),"SYCFSPRD")
  *E300789,4  AMM Adjust to fit the new index.
  *IF LASTKEY()=13 AND  !SEEK(gcPrnt_Cmp+RIGHT(lcYearPrd,4)+LEFT(lcYearPrd,2),"FSPRD")
  IF LASTKEY()=13 AND  !SEEK(RIGHT(lcYearPrd,4)+LEFT(lcYearPrd,2),"FSPRD")
  *E300789,4  AMM end
    *E300692,1 end 
    PRIVATE lcBrFields,lcCurArea
    DIMENSION laYearPrd[2]
    STORE '' TO laYearPrd
    lcCurArea = SELECT()
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
    *SELECT SYCFSPRD
    SELECT FSPRD
    *E300692,1 end 
    lcBrFields=[cfisfyear:H="Fiscal year",cfspprdid:H="Fiscal period",]+;
               [cfsppdesc:H="Description",dfsppbgdt:H="Begining date",]+;
               [dfsppendt:H="Ending date"]
    *MAN           
    *=gfBrows([gcPrnt_Cmp],'cfisfyear,cfspprdid','laYearPrd')
    =gfBrows("",'cfisfyear,cfspprdid','laYearPrd')    
     &lcObjName=laYearPrd[2]+laYearPrd[1]
     SHOW GET (lcObjname)
     SELECT (lcCurArea)
  ELSE
    IF LASTKEY()<>13
      &lcObjName=lcOldPriod
    ENDIF
  ENDIF
  *B500708,4 END OF B500708,4 MODIFICATIONS
  ***if there is from and through date. ***  
  IF !EMPTY(lcFrPrdRng) .AND. !EMPTY(lcToPrdRng)
    ***if from date is greater than through date.***
**    IF lcFrPrdRng > lcToPrdRng   
      IF (RIGHT(lcFrPrdRng,4)+LEFT(lcFrPrdRng,2)) > (RIGHT(lcToPrdRng,4)+LEFT(lcToPrdRng,2))
      ** MESSAGE : "From Data can not be less than Through Date"
      **           "                       ® Ok ¯ 
      =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)
      _CUROBJ=_CUROBJ
    ENDIF               
  ENDIF
ELSE && in case of discount date.
  ***if there is from and through date.***
  IF !EMPTY(ldFrDatRng) .AND. !EMPTY(ldToDatRng)
    ***if from date is greater than through date.***
    IF ldFrDatRng > ldToDatRng   
      ** MESSAGE : "From Data can not be less than Through Date"
      **           "                       ® Ok ¯ 
      =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)      
      _CUROBJ=OBJNUM(rbRange)
    ENDIF               
  ENDIF
ENDIF

*B607010,1 ABD - Return to old alias. [Begin]
SELECT (lnOldAlias)
*B607010,1 ABD - [End]

*!**************************************************************************
*!
*!      Function: lfShowpb
*!
*!**************************************************************************
*
FUNCTION lfShowpb

*E300921,1 TAK 07/12/98  Added if bank type is safe disable push buttons.
IF CPAYRECST = 'O' OR lcBnkType = 'S'
  SHOW GET pbOpen DISABLE
ELSE
  SHOW GET pbOpen ENABLE
ENDIF
IF CPAYRECST = "C" OR lcBnkType = 'S' 
  SHOW GET pbClear DISABLE 
ELSE
  SHOW GET pbClear ENABLE
ENDIF
IF CPAYRECST = "V" OR lcBnkType = 'S'
  SHOW GET pbVoid DISABLE
ELSE  
  SHOW GET pbVoid ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvOpen
*!
*!**************************************************************************
*
FUNCTION lfvOpen
PRIVATE  lcOpenOrd 


*E301266,1   (Start)
*IF APPAYMNT.CPAYRECST = "O"
IF PayTemp.CPAYRECST = "O"
  RETURN
ENDIF
*E301266,1   (End)


*E301266,1   (Start)
*lcORecst = APPAYMNT.CPAYRECST
*REPLACE APPAYMNT.CPAYRECST WITH 'O',;
*        APPAYMNT.DPAYRECDT WITH gdSysDate

lcORecst = PayTemp.CPAYRECST
REPLACE PayTemp.CPAYRECST WITH 'O',;
        PayTemp.DPAYRECDT WITH gdSysDate

*E301266,1   (End)


*E301266,1   (Start)
*Update same fields in master table
cBatchNo  = PayTemp.Batch

IF rbBrwsData =  2    && Detail data
  SELECT APPAYMNT   
  IF BETWEEN(PayTemp.nRecNo , 1, RECCOUNT("APPAYMNT"))
    GOTO PayTemp.nRecNo
    REPLACE APPAYMNT.CPAYRECST WITH 'O',;
            APPAYMNT.DPAYRECDT WITH gdSysDate
  ENDIF          
ELSE
  * Data summarized.
  IF EMPTY(cBatchNo)
    SELECT APPAYMNT   
    IF PayTemp.nRecNo <= RECCOUNT("APPAYMNT")
      GOTO PayTemp.nRecNo
      REPLACE APPAYMNT.CPAYRECST WITH 'O',;
              APPAYMNT.DPAYRECDT WITH gdSysDate
    ENDIF
  ELSE
    SELECT APPAYMNT   
    REPLACE ALL APPAYMNT.CPAYRECST WITH 'O',;
                APPAYMNT.DPAYRECDT WITH gdSysDate ;
                FOR APPAYMNT.Batch = cBatchNo AND ;
                    APPAYMNT.CPAYRECST = lcORecst
  ENDIF
ENDIF
SELECT PayTemp   
*E301266,1   (End)

       


*IF APPAYMNT.CPAYTYPE='P'
  DO CASE
    CASE lcORecst = 'V'

      *E301266,1   (Start)
      *lnOpen      = lnOpen    - APPAYMNT.NPAYAMNT
      lnOpen      = lnOpen    -  PayTemp.NPAYAMNT
      *E301266,1   (End)

    CASE lcORecst = 'C'
      *E301266,1   (Start)
      *lnOpen     = lnOpen    - APPAYMNT.NPAYAMNT
      *lnCleared  = lnCleared + APPAYMNT.NPAYAMNT
      lnOpen      = lnOpen    - PayTemp.NPAYAMNT
      lnCleared   = lnCleared + PayTemp.NPAYAMNT
      *E301266,1   (End)      
  ENDCASE
*ENDIF
lnBook = lnOpen+lnCleared

IF !cbOpen  &&If the open records voided from the scope add this record agian to the scop.
  lnArlen=lnArlen+1
  = AINS(laRecNo,1)
  laRecNo[1] = RECNO()
  DIMENSION laRecNo[lnArlen]
  *B803925,1 AME [Start]
  *=lfvRecon(.T.)
  *B803925,1 AME [end]
ENDIF


SKIP IIF(EOF(),0,1) 
SKIP IIF(EOF(),-1,0) 

*E301266,1  (Start)
*lnBrRecN1  = RECNO('APPAYMNT')
lnBrRecN1  = RECNO('PayTemp')
*E301266,1  (End)
  

=lfShowpb()

SHOW GET lnCleared 
SHOW GET lnOpen
SHOW GET lnBook
SHOW WINDOW (lcBrTtl) REFRESH

*!**************************************************************************
*!
*!      Function: lfvClear
*!
*!**************************************************************************
*
FUNCTION lfvClear
PRIVATE lcClearOrd  

*E301266,1   (Start)
*IF APPAYMNT.CPAYRECST = 'C'
IF PayTemp.CPAYRECST = 'C'
  RETURN
ENDIF
*E301266,1   (End)


*E301266,1   (Start)
*lcORecst = APPAYMNT.CPAYRECST
*REPLACE APPAYMNT.CPAYRECST WITH 'C',;
*        APPAYMNT.DPAYRECDT WITH gdSysDate
lcORecst = PayTemp.CPAYRECST
REPLACE PayTemp.CPAYRECST WITH 'C',;
        PayTemp.DPAYRECDT WITH gdSysDate
      
*E301266,1   (End)


*E301266,1   (Start)
*Update same fields in master table
cBatchNo  = PayTemp.Batch
IF rbBrwsData =  2    && Detail data
  SELECT APPAYMNT   
  IF BETWEEN(PayTemp.nRecNo , 1 , RECCOUNT("APPAYMNT") ) 
    GOTO PayTemp.nRecNo
    REPLACE APPAYMNT.CPAYRECST WITH 'C',;
            APPAYMNT.DPAYRECDT WITH gdSysDate
  ENDIF          
ELSE
  * Data summarized.
  IF EMPTY(cBatchNo)
    SELECT APPAYMNT   
    IF PayTemp.nRecNo <= RECCOUNT("APPAYMNT")
      GOTO PayTemp.nRecNo
      REPLACE APPAYMNT.CPAYRECST WITH 'C',;
              APPAYMNT.DPAYRECDT WITH gdSysDate
    ENDIF
  ELSE
    SELECT APPAYMNT   
    REPLACE ALL APPAYMNT.CPAYRECST WITH 'C',;
                APPAYMNT.DPAYRECDT WITH gdSysDate ;
                FOR APPAYMNT.Batch = cBatchNo AND ;
                    APPAYMNT.CPAYRECST = lcORecst
  ENDIF
ENDIF

SELECT PayTemp   
*E301266,1   (End)


* MAN Removed by man to count the AR Record  
*IF APPAYMNT.CPAYTYPE='P'
  DO CASE
    CASE lcORecst = 'V'
      *E301266,1   (Start)
      *lnCleared = lnCleared - APPAYMNT.NPAYAMNT
      lnCleared = lnCleared - PayTemp.NPAYAMNT      
      *E301266,1   (End)      
      
    CASE lcORecst = 'O'
      *E301266,1   (Start)
      *lnOpen    = lnOpen    + APPAYMNT.NPAYAMNT
      *lnCleared = lnCleared - APPAYMNT.NPAYAMNT
      lnOpen    = lnOpen   + PayTemp.NPAYAMNT
      lnCleared = lnCleared - PayTemp.NPAYAMNT
      *E301266,1   (End)

  ENDCASE
*ENDIF
lnBook = lnOpen+lnCleared

IF !cbClear         &&If the cleared records voided from the scope add this record agian.
  lnArlen=lnArlen+1
  = AINS(laRecNo,1)
  laRecNo[1] = RECNO()
  DIMENSION laRecNo[lnArlen]
  *B803925,1 AME [Start]
  *=lfvRecon(.T.)
  *B803925,1 AME [end]
ENDIF  



SKIP IIF(EOF(),0,1) 
SKIP IIF(EOF(),-1,0) 

*E301266,1  (Start)
*lnBrRecN1  = RECNO('APPAYMNT')
lnBrRecN1  = RECNO('PayTemp')
*E301266,1  (End)


=lfShowpb()

SHOW GET lnCleared 
SHOW GET lnOpen
SHOW GET lnBook
SHOW WINDOW(lcBrTtl) REFRESH

*!**************************************************************************
*!
*!      Function: lfvVoid
*!
*!**************************************************************************
*
FUNCTION lfvVoid
PRIVATE lcVoidOrd  

*E301266,1   (Start)
*IF APPAYMNT.CPAYRECST = 'V'
IF PayTemp.CPAYRECST = 'V'
  RETURN
ENDIF
*E301266,1   (End)

*E301266,1   (Start)
*lcORecst = APPAYMNT.CPAYRECST
*REPLACE APPAYMNT.CPAYRECST WITH 'V',;
*        APPAYMNT.DPAYRECDT WITH gdSysDate

lcORecst = PayTemp.CPAYRECST
REPLACE PayTemp.CPAYRECST WITH 'V',;
        PayTemp.DPAYRECDT WITH gdSysDate
*E301266,1   (End)        

*E301266,1   (Start)
*Update same fields in master table
cBatchNo  = PayTemp.Batch
IF rbBrwsData =  2    && Detail data
  SELECT APPAYMNT   
  IF BETWEEN(PayTemp.nRecNo ,1 ,  RECCOUNT("APPAYMNT") ) 
    GOTO PayTemp.nRecNo
    REPLACE APPAYMNT.CPAYRECST WITH 'V',;
            APPAYMNT.DPAYRECDT WITH gdSysDate
  ENDIF         
ELSE
  * Data summarized.
  IF EMPTY(cBatchNo)
    SELECT APPAYMNT   
    IF PayTemp.nRecNo <= RECCOUNT("APPAYMNT")
      GOTO PayTemp.nRecNo
      REPLACE APPAYMNT.CPAYRECST WITH 'V',;
              APPAYMNT.DPAYRECDT WITH gdSysDate
    ENDIF
  ELSE
    SELECT APPAYMNT   
    REPLACE ALL APPAYMNT.CPAYRECST WITH 'V',;
                APPAYMNT.DPAYRECDT WITH gdSysDate ;
                FOR APPAYMNT.Batch = cBatchNo AND ;
                    APPAYMNT.CPAYRECST = lcORecst
  ENDIF
ENDIF

SELECT PayTemp   
*E301266,1   (End)


        
* MAN Removed by man to count the AR Record  
*IF APPAYMNT.CPAYTYPE='P'
  DO CASE
    CASE lcORecst = 'C'
      *E301266,1   (Start)
      *lnCleared = lnCleared + APPAYMNT.NPAYAMNT
      lnCleared = lnCleared + PayTemp.NPAYAMNT
      *E301266,1   (End)
    CASE lcORecst = 'O'
      *E301266,1   (Start)
      *lnOpen    = lnOpen    + APPAYMNT.NPAYAMNT
      lnOpen     = lnOpen    +  PayTemp.NPAYAMNT
      *E301266,1   (End)
  ENDCASE
*ENDIF
lnBook = lnOpen+lnCleared

IF !cbVoid  &&If the void records voided from the scope this add record agian.
  lnArlen=lnArlen+1
  = AINS(laRecNo,1)
  laRecNo[1] = RECNO()
  DIMENSION laRecNo[lnArlen]
  *B803925,1 AME [Start]
  *=lfvRecon(.T.)
  *B803925,1 AME [end]
ENDIF  


SKIP IIF(EOF(),0,1) 
SKIP IIF(EOF(),-1,0) 

*E301266,1  (Start)
*lnBrRecN1  = RECNO('APPAYMNT')
lnBrRecN1  = RECNO('PayTemp')
*E301266,1  (End)


=lfShowpb()

IF CPAYRECST = "V"
  SHOW GET pbVoid DISABLE
ELSE  
  SHOW GET pbVoid ENABLE
ENDIF

SHOW GET lnCleared 
SHOW GET lnOpen
SHOW GET lnBook
SHOW WINDOW (lcBrTtl) REFRESH

*!**************************************************************************
*!
*!      Function: lfvCbStat
*!
*!**************************************************************************
*
FUNCTION lfvCbStat

IF !cbOpen .AND. !cbClear .AND. !cbVoid
  SHOW GET pbRconcl DISABLE
ELSE
  SHOW GET pbRconcl ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvRecon
*!
*!**************************************************************************
* Fill the filter expresion when the <Reconcile> pressed in scope window.
*
FUNCTION lfvRecon
PARAMETERS llrbldexp

*** Check if the bank code is empty, if it is,
*** do not continue and return to the bank code field
IF !llrbldexp .AND. EMPTY(lcBankCode)
  *** Message : "   You have to enter the ð.  "
  ***                 ® OK  ¯
  =gfModalGen("TRM04066B00000","DIALOG",lcTBankCode)
  _CUROBJ = OBJNUM(lcBankCode)
  RETURN
ENDIF

llFrstShw   = .F.
***clear the expresion***
lcExprsion = ""

IF !llrbldexp
  
  DIMENSION laRecNo[1]
  =ADEL(laRecNo,1)
ENDIF

*** flag to differentiate between <Reconcile> and <cancel>***
llScope = .T.

*** Variable holding Filter expresion ***
*E301266,1   (Start)
*lcExprsion = "APPAYMNT.CPAYSTAT <> 'V'.AND."+;
             "APPAYMNT.CPAYMETH <> 'H'"

lcExprsion = "CPAYSTAT <> 'V'.AND."+;
             "CPAYMETH <> 'H'"             
*E301266,1   (End)

*E301266,1  (Start)
*- No need for that exp. now
*IF !EMPTY(lcBankCode)
  *lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
             "CBNKCODE=lcBankCode"  
*ENDIF             
*IF !EMPTY(lcCheckCode)
  *lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
             "CCHKACCT=lcCheckCode"  
*ENDIF             
*E301266,1  (Start)

***    Due date.   ***
IF rbRange = 1
  DO CASE
    *** !empty "from"  and  empty "to"
    CASE !EMPTY(lcFrPrdRng) .AND. EMPTY(lcToPrdRng)
*B500708,4 remarked to use the character variables     
*      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "CTOD(CFSPPRDID+'/01/'+CFISFYEAR) >= ldFrPrdRng"

       *B608912,1 TMI 07/01/2009 10:25:37 AM [Start] make the comparison between the correct string comparison order
       *lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                    "CFSPPRDID+CFISFYEAR >=lcFrPrdRng"            
       lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                    "CFISFYEAR+CFSPPRDID >=RIGHT(lcFrPrdRng,4)+LEFT(lcFrPrdRng,2)"            
       *B608912,1 TMI 07/01/2009 10:25:50 AM [End  ]                     
                    
                    
    *** empty "from"  and  !empty  "to"
    CASE  EMPTY(lcFrPrdRng) .AND. !EMPTY(lcToPrdRng)
*B500708,4 remarked to use the character variables         
*      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "CTOD(CFSPPRDID+'/01/'+CFISFYEAR) <= ldToPrdRng"
       *B608912,1 TMI 07/01/2009 10:26:59 AM [Start] make the comparison between the correct string comparison order
       *lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                    "CFSPPRDID+CFISFYEAR <=lcToPrdRng"
       lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                    "CFISFYEAR+CFSPPRDID <=RIGHT(lcToPrdRng,4)+LEFT(lcToPrdRng,2)"
       *B608912,1 TMI 07/01/2009 10:27:11 AM [End  ] 

    *** !empty "from"  and  !empty "to"
    CASE !EMPTY(lcFrPrdRng) .AND. !EMPTY(lcToPrdRng)
*      IF lcFrPrdRng > lcToPrdRng   && if from date greater than through date.
      IF (RIGHT(lcFrPrdRng,4)+LEFT(lcFrPrdRng,2)) > (RIGHT(lcToPrdRng,4)+LEFT(lcToPrdRng,2))      
        ** MESSAGE : "From Data can not be less than Through Date"
        **           "                       ® Ok ¯ 
        =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)        
*        _CUROBJ=OBJNUM(rbRange)
        _CUROBJ=OBJNUM(lcFrPrdRng)
        RETURN
      ELSE
*B500708,4 remarked to use the character variables           
*        lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(CTOD(CFSPPRDID+'/01/'+CFISFYEAR),ldFrPrdRng,ldToPrdRng)"
                     
        *B608912,1 TMI 07/01/2009 10:28:38 AM [Start] make the comparison between the correct string comparison order
        *lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(CFSPPRDID+CFISFYEAR,lcFrPrdRng,lcToPrdRng)"
        lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(CFISFYEAR+CFSPPRDID,RIGHT(lcFrPrdRng,4)+LEFT(lcFrPrdRng,2),RIGHT(lcToPrdRng,4)+LEFT(lcToPrdRng,2))"
        *B608912,1 TMI 07/01/2009 10:28:47 AM [End  ]              
                     
      ENDIF               
  ENDCASE
ELSE
  ***  Discount date from.  ***
  DO CASE
    *** !empty "from"  and  empty "to"
    CASE !EMPTY(ldFrDatRng) .AND. EMPTY(ldToDatRng)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DPAYDATE >= ldFrDatRng"
                   
    *** empty "from"  and  !empty  "to"
    CASE  EMPTY(ldFrDatRng) .AND. !EMPTY(ldToDatRng)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DPAYDATE <= ldToDatRng"
    *** !empty "from"  and  !empty "to"
    CASE !EMPTY(ldFrDatRng) .AND. !EMPTY(ldToDatRng)
      IF ldFrDatRng > ldToDatRng   
        ** MESSAGE : "From Data can not be less than Through Date"
        **           "                       ® Ok ¯ 
        =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)        
        _CUROBJ=OBJNUM(rbRange)
        RETURN
      ELSE
        lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(DPAYDATE,ldFrDatRng,ldToDatRng)"  
      ENDIF               
  ENDCASE
ENDIF

lcOrExpr = IIF(cbOpen ,".OR. CPAYRECST='O'",' ')+;
           IIF(cbClear,".OR. CPAYRECST='C'",' ')+;
           IIF(cbVoid ,".OR. CPAYRECST='V'",' ')

IF !EMPTY(lcOrExpr)
  lcOrExpr="("+ALLTRIM(STRTRAN(SUBSTR(lcOrExpr,1,10),'.OR.','  '))+;
           ALLTRIM(SUBSTR(lcOrExpr,11,50))+".OR. Ascan(laRecNo,Recno()) > 0"+")"
  lcExprsion = lcExprsion +".AND."+lcOrExpr          

ENDIF

lcOldBank  = lcBankCode 
lcOldCheck = lcCheckCode 


*E301266,1  (Start)
DO lfGenTmp
SELECT PayTemp
=lfvOrder()
SET RELATION TO CPAYCLNO INTO APVENDOR
* SET FILTER TO PayTemp.CBNKCODE+PayTemp.CCHKACCT=lcBankCode+lcCheckCode .AND. EVALUATE(lcExprsion)
*E301266,1  (End)

IF !llrbldexp
  CLEAR READ
ENDIF  

llrbldexp = .F.

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
* Restore old values if push cancel from scope.
*
FUNCTION lfvCancel

*E301266,1   (Strat)
* If user has selected 'Cancel' from first time, close the temp file
IF llFrstShw      
  =lfCreatTmp(.F.)
ENDIF  
*E301266,1   (End)


***flag to deferntiate between <Reconcile> and <cancel>***
llScope  = .F.
*  *** RESTORE OLD VAR. IN CURRENT VAR.
ldToDatRng  = ldOToDtRng
ldFrDatRng  = ldOFrDtRng
lcToPrdRng  = lcOToPdRng
lcFrPrdRng  = lcOFrPdRng
cbOpen      = llOldOpen  
cbClear     = llOldClear
cbVoid      = llOldVoid
rbRange     = lnOldRange
lcBankCode  = lcOldBank 
lcCheckCode = lcOldCheck  
  
  
*!**************************************************************************
*!
*!      Function: lfDefBank
*!
*!**************************************************************************
* Get default division, bank, and checks.
*
FUNCTION lfDefBank

*E300921,1 Restore the bank default if Ap installed only.
*IF EMPTY(lcBankCode)
IF (OCCURS('AP',gcCmpModules)<>0) AND EMPTY(lcBankCode)
*E300921,1 End.
  llOpSet = gfOpenFile(gcDataDir+'APSETUP','','SH')
  STORE APSETUP.CBNKCODE TO lcBankCode
  STORE APSETUP.CCHKACCT TO lcCheckCode
  IF llOpSet
    =gfCloseFile('APSETUP')
  ENDIF
ENDIF  
SELECT APPAYMNT

*!**************************************************************************
*!
*!      Function: lfwOldVals
*!
*!**************************************************************************
* save the old value of feilds upon entry 
*
FUNCTION lfwOldVals
lcOldVal   = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
* Validation of bank and checking account fields.
*
FUNCTION lfvBnkChk
PRIVATE llVldBnk
*E301077,71 AMM Open file 
=gfOpenFile(gcDataDir+'APBANKS','BANKCODE','SH')
*E301077,71 AMM end

llVldBnk = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcObjDisp)
*E300921,1 Read the Bank Type and disable the checking account if type safe.
lcBnkType = APBANKS.cBnkType
IF lcBnkType = 'S'
  SHOW GET ibChecks    DISABLE
  SHOW GET lcCheckCode DISABLE
  *--The transactions alwayes open in case of safe.
  STORE .T. TO cbOpen
  STORE .F. TO cbClear,cbVoid
  SHOW GET cbOpen  DISABLE
  SHOW GET cbClear DISABLE
  SHOW GET cbVoid  DISABLE
ELSE
  SHOW GET cbOpen  ENABLE
  SHOW GET cbClear ENABLE
  SHOW GET cbVoid  ENABLE
ENDIF
*E300921,1 end.

=lfRefresh()

*E301266,1 (Start)
*SELECT APPAYMNT
SELECT PayTemp
*E301266,1 (End)

RETURN IIF(llVldBnk, .T., 1)

*!**************************************************************************
*!
*!      FUNCTION : lfDeAct
*!
*!**************************************************************************
*
FUNCTION lfDeAct

IF WONTOP(lcBrTtl)
  RELEASE PAD _BROWSE OF _MSYSMENU
  =lfBrowTrap()
ELSE
  =lfBrowUnTrap() 
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfBrowTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowTrap

ON KEY LABEL Alt+V      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+L      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+P      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+S      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+C      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL ESC        DO  lpDoTrap WITH LASTKEY()
ON KEY LABEL Ctrl+ENTER DO  lfvClose

*!**************************************************************************
*!
*!      PROCEDURE: lpDoTrap
*!
*!**************************************************************************
*
PROCEDURE lpDoTrap
PARAMETER lnLstKey

KEYBOARD "{SHIFT+HOME}"       && dummy key press to release menu access

DO CASE
   CASE lnLstKey = 27 
     IF _DOS
       HIDE MENU _MSYSMENU
       SHOW MENU _MSYSMENU
     ENDIF  
     ACTIVATE WINDOW (lcReadNam2)
     _CUROBJ = OBJNUM(pbClose)
     KEYBOARD "{ENTER}"
   CASE lnLstKey = 25
     =lfvOpen() 
   CASE lnLstKey = 38
     =lfvClear()
   CASE lnLstKey = 47
     =lfvVoid()
   CASE lnLstKey = 31
     =lfvScope()
   CASE lnLstKey = 46
     ACTIVATE WINDOW (lcReadNam2)
     _CUROBJ = OBJNUM(pbClose)
     KEYBOARD '{ENTER}'
ENDCASE

*!**************************************************************************
*!
*!      FUNCTION : lfBrowUnTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowUnTrap

ON KEY LABEL Alt+V
ON KEY LABEL Alt+L
ON KEY LABEL Alt+P
ON KEY LABEL Alt+S
ON KEY LABEL Alt+C
ON KEY LABEL Ctrl+ENTER
*B600491,1 MAN 06/21/95 Remarked the following lines
*IF _DOS
*  ON KEY LABEL ESC 
*ELSE
  ON KEY LABEL ESC &lcEscTrp
 * ON KEY LABEL ESC 
*ENDIF
*B600491,1 MAN 06/21/95 END

*!**************************************************************************
*!
*!      FUNCTION : lfPushKey
*!
*!**************************************************************************
* Trap keys.
*
FUNCTION lfPushKey

*** places all current on key labels on a stack in memory.
*** deactivate control keys for browse window 
***  TAB, Ctrl+W, Ctrl+Q, Ctrl+Enter
ON KEY LABEL TAB         DO  lpTrap
ON KEY LABEL BACKTAB     DO  lpTrap 
*B600491,1 MAN 06/21/95 Remarked the following lines
*IF _DOS
*  ON KEY LABEL ESC
*ELSE
  ON KEY LABEL ESC &lcEscTrp
*  ON KEY LABEL ESC
*ENDIF
*B600491,1 MAN 06/21/95 END
*!**************************************************************************
*!
*!      Function: lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows

RELEASE PAD _BROWSE OF _MSYSMENU
ACTIVATE WINDOW (lcBrTtl)

*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
*
FUNCTION lfDefinePad

IF _DOS
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' KEY ALT+B
  ON SELECTION PAD _BROWSE OF _msysmenu DO lfActBrows 
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpTrap
*!
*!**************************************************************************
*
PROCEDURE lpTrap

ON KEY LABEL TAB       
ON KEY LABEL BACKTAB   
 
*** check if the window on top is  browse window
DO CASE
  ***Tab. or Right cursor***  
  CASE LASTKEY() = 9 .OR. LASTKEY() = 4
    IF _DOS
      HIDE MENU _MSYSMENU
      SHOW MENU _MSYSMENU
    ENDIF      
    IF WONTOP(lcBrTtl)
      ACTIVATE WINDOW (lcReadNam2)
      _CUROBJ=OBJNUM(pbOpen)
    ELSE  
      IF WONTOP(lcReadNam2)
        IF _CUROBJ = OBJNUM(pbClose)
          ACTIVATE WINDOW (lcBrTtl)
        ELSE
          KEYBOARD "{TAB}" PLAIN
        ENDIF
      ENDIF
    ENDIF  

  ***Shift Tab key or Left cursor key***    
  CASE  LASTKEY() = 15 .OR. LASTKEY() = 19
    IF _DOS
      HIDE MENU _MSYSMENU
      SHOW MENU _MSYSMENU
    ENDIF      
    IF WONTOP(lcReadNam2)
      IF _CUROBJ = OBJNUM(pbOpen)
        ACTIVATE WINDOW (lcBrTtl)
      ELSE
        KEYBOARD "{BACKTAB}" PLAIN
      ENDIF
    ELSE  
      IF WONTOP(lcBrTtl)
        ACTIVATE WINDOW (lcReadNam2)
        _CUROBJ=OBJNUM(pbClose)
      ENDIF
    ENDIF  
ENDCASE

ON KEY LABEL TAB         DO  lpTrap
ON KEY LABEL BACKTAB     DO  lpTrap 








*!**************************************************************************
*!
*!      Function: lfvOrder
*!
*!**************************************************************************
*
FUNCTION lfvOrder
IF rbBrwsOrd = 1 
  SET ORDER TO TAG CHKTMNO
ELSE
  SET ORDER TO TAG PayDate 
ENDIF 
RETURN 


*!*************************************************************
*! Name      : lfGenTmp
*! Developer : Amin Khodary 
*! Date      : 06/20/1999
*! Purpose   : Copy structure of temp. table form  APPATMNT 
*!*************************************************************
*! Called from : APBNKRC.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      :  None
*!*************************************************************
PROCEDURE lfGenTmp
PRIVATE lcExpr


* This variable will holds the value of equvlant amount of each record.
PRIVATE lnEqvAmnt 

lnAmount  = 0 
cBatchNo  = '' 
=lfCreatTmp(.T.)
SELECT PayTemp

SELECT APPAYMNT
lcGenOrd  = ORDER() 
SET ORDER TO TAG Chkbtch OF (gcWorkDir +lcTmpIndx  + '.CDX')

SEEK lcBankCode+lcCheckCode
SCAN REST WHILE APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT=lcBankCode+lcCheckCode  ;
     .AND.  !EOF('APPAYMNT')  FOR &lcExprsion
  
  SELECT APPAYMNT 
  
  * If no batch or user has selected detail radio button.
  IF EMPTY(APPAYMNT.Batch)  OR  rbBrwsData <> 1    

    * No need to do the following conversion each time, it will be better 
    * if doing it whenever the transaction currency not equal to base currency.
    *IF APPAYMNT.cCurrCode <> gcBaseCurr
      * Initialize lnEqvAmnt variable
    *  lcExSin2  = ''
    *  lcExSin1  = gfGetExSin(@lcExSin2, APPAYMNT.cCurrCode ) 
    *  lnEqvAmnt = ROUND(APPAYMNT.nPayAmnt &lcExSin1 APPAYMNT.nExRate &lcExSin2 APPAYMNT.nCurrUnit ,2)
    *ELSE
      lnEqvAmnt = APPAYMNT.nPayAmnt  
    *ENDIF  

    * insert row in temp. file 
    DO lfInsrtRow WITH lnEqvAmnt , APPAYMNT.CPAYCOMP , APPAYMNT.CPAYDOCNO , APPAYMNT.CPAYMETH , APPAYMNT.cPayRecSt
    
  ELSE
    * summary data   
    IF rbBrwsData = 1    
      * Get batch no. 
      cBatchNo  = APPAYMNT.Batch
      * Get record no. 
      nlRecNo = RECNO('APPAYMNT')

      * Initialize variable holds equevlant amount 
      lnEqvAmnt = 0 

      * Initialize varibale holds no. of entries 
      lnEntryNo = 0 
      lcEntryNo = ''
      lnClrAmt  = 0
      lnOpnAmt  = 0
      lnVodAmt  = 0
  	  lnClrEntrs  = 0
      lnOpnEntrs  = 0
      lnVodEntrs  = 0
      SCAN REST WHILE cBatchNo = APPAYMNT.BATCH ;
           .AND. APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT=lcBankCode+lcCheckCode  ;
           .AND.  !EOF('APPAYMNT')  FOR &lcExprsion
        
        * Get record no.
        nlRecNo = RECNO('APPAYMNT')
	    lnValue   = APPAYMNT.nPayAmnt 
        lnEqvAmnt = lnEqvAmnt + APPAYMNT.nPayAmnt
     	lnClrAmt  = lnClrAmt  + IIF(CPAYRECST = 'C', lnValue , 0  ) 
    	lnOpnAmt  = lnOpnAmt  + IIF(CPAYRECST = 'O', lnValue , 0  ) 
        lnVodAmt  = lnVodAmt  + IIF(CPAYRECST = 'V', lnValue , 0  ) 
      
      
        * Increment no. of entry by one
      	lnClrEntrs  = lnClrEntrs + IIF(CPAYRECST = 'C', 1 , 0  ) 
    	lnOpnEntrs  = lnOpnEntrs + IIF(CPAYRECST = 'O', 1 , 0  ) 
        lnVodEntrs  = lnVodEntrs + IIF(CPAYRECST = 'V', 1 , 0  ) 
        
        
      ENDSCAN
      GOTO nlRecNo
      
      IF lnClrEntrs > 0 
        lcEntryNo = 'No. of entries per batch is ' + ALLTRIM(STR(lnClrEntrs))
        DO lfInsrtRow WITH lnClrAmt , lcEntryNo , cBatchNo , 'A' , 'C'
      ENDIF

      IF lnOpnEntrs  > 0 
        lcEntryNo = 'No. of entries per batch is ' + ALLTRIM(STR(lnOpnEntrs))
        DO lfInsrtRow WITH lnOpnAmt , lcEntryNo , cBatchNo , 'A' , 'O'
      ENDIF
      
      IF lnVodEntrs > 0 
        lcEntryNo = 'No. of entries per batch is ' + ALLTRIM(STR(lnVodEntrs))
        DO lfInsrtRow WITH lnVodAmt , lcEntryNo , cBatchNo , 'A' , 'V'
      ENDIF
      
      
    ENDIF
  ENDIF
ENDSCAN
SELECT APPAYMNT 
SET ORDER TO &lcGenOrd 



*!*************************************************************
*! Name      : lfInsrtRow
*! Developer : Amin Khodary 
*! Date      : 06/20/1999
*! Purpose   : Copy the current data of APPATMNT row to temp. table 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) lnAmtValue
*!*************************************************************
*! Return      :  None
*!*************************************************************
PROCEDURE lfInsrtRow
PARAMETER lnAmtValue , lcPayComp , lcPayDocNo , lcPayMeth , lcPayRecSt

INSERT INTO PayTemp ( CPAYTYPE , CPAYMETH , CBNKCODE ,;
                      CCHKACCT , CPAYDOCNO, CPAYSTAT ,;
		              DPAYDATE , CFISFYEAR, CFSPPRDID,;
	  				  DPAYVDATE, LPAYADVAN, CPAYCLNO ,;
					  CPAYCOMP , NPAYAMNT , NPAYDISC ,;
					  NPAYADJ  , CPAYRECST, DPAYRECDT,;
					  CPAYRECRF, CBNKADJ  , BATCH  , NRECNO  );
         VALUES (   APPAYMNT.CPAYTYPE  ,;
                    lcPayMeth          ,;
                    APPAYMNT.CBNKCODE  ,;
                    APPAYMNT.CCHKACCT  ,;
                    lcPayDocNo         ,;
     			    APPAYMNT.CPAYSTAT  ,;
                    APPAYMNT.DPAYDATE  ,;
                    APPAYMNT.CFISFYEAR ,;  
                    APPAYMNT.CFSPPRDID ,;
					APPAYMNT.DPAYVDATE ,; 
					APPAYMNT.LPAYADVAN ,;
					APPAYMNT.CPAYCLNO  ,;
					lcPayComp          ,;
                    lnAmtValue         ,; 
					APPAYMNT.NPAYDISC  ,;
					APPAYMNT.NPAYADJ   ,;
					lcPayRecSt,;
					APPAYMNT.DPAYRECDT ,;
					APPAYMNT.CPAYRECRF ,;
					APPAYMNT.CBNKADJ   ,;
				    APPAYMNT.BATCH     ,;
				    RECNO('APPAYMNT')   )  
SELECT APPAYMNT 
RETURN 

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Amin Khodary 
*! Date      : 06/20/1999
*! Purpose   : 1) Create temp. table 
*!             2) Open or close table based on parameter
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) True to create and open the file 
*!                     2)  False to close and erase the file 
*!*************************************************************
*! Return      :  None
*!*************************************************************
FUNCTION  lfCreatTmp
PARAMETERS llOpen 
PRIVATE  llBalnBfor           && Flag to know if the temp. table has been created 
IF FILE(gcWorkDir +lcTmpApPay+ '.DBF') 
  IF llOpen   
    SELECT PayTemp
    ZAP
    =lfvOrder()
    =lfDispBrow()
  ELSE
    *IF file that we are using it (PayTemp) is file exist, close it. 
    IF USED('PayTemp')
      SELECT PayTemp
      USE 
    ENDIF 
    ERASE (gcWorkDir +lcTmpApPay + '.DBF')
    ERASE (gcWorkDir +lcTmpApPay + '.CDX')
    
    * To erase appaymnt temp. index you have to select it first 
    SELECT APPAYMNT
    CLOSE INDEX
    ERASE (gcWorkDir +lcTmpIndx  + '.CDX')
  ENDIF
ELSE
  IF llOpen   
    DIMENSION ArryPayTmp(1,4)
    SELECT APPAYMNT
    =AFIELDS(ArryPayTmp)
    nAryLen = ALEN(ArryPayTmp ,1 ) + 1
    DIMENSION ArryPayTmp( nAryLen  , 4 )
    ArryPayTmp(nAryLen  , 1 ) = 'NRECNO'
    ArryPayTmp(nAryLen  , 2 ) = 'N'
    ArryPayTmp(nAryLen  , 3 ) = 6
    ArryPayTmp(nAryLen  , 4 ) = 0
    
    CREATE DBF (gcWorkDir + lcTmpApPay)  FROM ARRAY ArryPayTmp
    USE 
    SELE 0 
    USE (gcWorkDir + lcTmpApPay) ALIAS PayTemp EXCL 
    INDE ON cbnkcode+cchkacct+cpaytype+cpaymeth+cpaydocno TAG CHKTMNO OF (gcWorkDir + lcTmpApPay+ '.CDX') 
    INDE ON cbnkcode+cchkacct+cpaytype+cpaymeth+DTOS(dPayDate) TAG PayDate OF (gcWorkDir + lcTmpApPay+ '.CDX') 
    SELECT PayTemp
    =lfvOrder()
  ENDIF
ENDIF
RETURN



