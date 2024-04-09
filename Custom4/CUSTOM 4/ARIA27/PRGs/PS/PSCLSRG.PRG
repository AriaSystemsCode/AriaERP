*:****************************************************************
*: Program file  : PSCLSRG.PRG
*: Program desc. : Close Register Program.
*: System        : Aria Apparel System (A27).
*: Module        : Point Of Sale.  (PS)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102057,1
*: Date          : 12/04/2000
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvSafe  , lfOpenFils , lfClosFils
*:               :            : lfcollect, lfOldValue , lfActBrow
*:               :            : lfwBrow  , lfvBrows   , lfReadAct
*:               :            : lfTrapKy , lfUnTrapKy , lfvNewbaln
*:               :            : lfvCancel, lfCloseReg , lfvUsr_Pass
*:               :            : lfvChecks,
*:               -----------------------------------------------
*:               : PROCEDURE  : lpTab    , lpShiftTab , lfHoldKey
*:               :            : lpDetEsc , 
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*:C#102057,1          : 
*:****************************************************************
*:Modifications :
*:B#604352,1 SSH FIx some bugs in the screnn and add balance field.
*N000232,1 HS  04/12/2001 Check if NC module is installed and the
*N000232,1                company setup "System Type" is set to
*N000232,1                "Point Of Sale", and if so add a record
*N000232,1                in the EDITrans to be used to send the
*N000232,1                Close Cash Register.
*B606117,1 SSH Prevent closing register if closing found in date greater than system date
*B038431,1 NNA 08/20/2004 Fix bug that if module [NC] not installed and we go to run 'Close Register'
*B038431,1 NNA            from [Transaction - cash Register] we will get a massege error that file
*B038431,1 NNA            'EdiTrans.dbf' Not found
*:****************************************************************
*:
*-- Declaration variables.
*-- llOpnApbnk : - .T. if this program open Apbanks  file.
*-- llOpnApMnt : - .T. if this program open AppayMent file.
*-- llOpnPostr : - .T. if this program open POSTRAN  file.
*-- llOpnApChk : - .T. if this program open APCECKS  file.
*-- llOpInst   : - .T. if this program open SYCINST  file.
*-- llOpnCodes : - .T. if this program open the codes file.
*-- llSyuuser  : - .T.
*-- lcSafe     : - Variable hold the selected safe.
*-- lcApayMent : - Variable hold temp index on AppayMent File.
*-- lcPostRan  : - Variable hold temp index on POSTRAN   File.
*-- lcOldValue : - Variable hold the old value .
*-- lcAmntendr : - Variable hold temp file for diff. tender amount.
*-- lcWinCh1   : - Variable hold the browse window.
*-- lcWinCh2   : - Variable hold the browse window.
*-- lcWinCh3   : - Variable hold the browse window.
*-- lcSeqNum   : - Variable to hold sequence number.
*-- ldAdjDat   : - Variable to hold Adjustment Date.
*-- lcYear     : - Variable to hold first year.
*-- lcFisPrd   : - Variable to hold First period.
*-- lcFisYer   : - Variable to hold first year.
*-- lcCurrency : - Variable to hold currency code.
*-- lnCurrUnit : - Variable to hold currency unit.
*-- lnExRate   : - Variable to hold exchange rate.
*-- llUsersExt : - If we need user login is requiered.
*-- lcSfUserStr: - Variable hold the user id.
*-- lcCheckCode: - Variable to hold check SaFe. 
*-- lnSfUser   : - user number in the popup array.
*-- llAlowAcss : - Allow access to selected safe.
*-- lcSesNum   ; - Variable to hold session number.
STORE ''  To lcSafe, lcApayMent , lcPostRan , lcAmntendr ,lcWinCh1        ,;
          lcHldTab ,lcHldBtb,lcHldEsc,lcHldAtB ,lcSeqNum,lcYear , lcSesNum,;
          lcPeriod,lcFisYer ,lcFisPrd,lcCurrency ,lcSfUserStr ,lcCheckCode
          
STORE .F. To llOpnApbnk , llOpnApMnt ,llOpnPostr , llBrowse ,llFirsttim ,llOpnApChk ,;
             llSyuuser , llOpInst ,llUsersExt ,llAlowAcss , llOpnCodes
STORE 0 To lnOpenBaln , lnTotSales , lnTotRetrn , lnTotAdjst , lnNetClose ,;
           lnNwOpnBln ,lnExRate, lnCurrUnit, lntotsals, lnTotS, lnTotR

*N000232,1 HS 04/12/2001 Add these lines to add some new flags [Begin]
*-- Flag to hold if the NC Module is installed for the active company or not
llNCInst = "NC" $ gcComp_Mdl

*-- Flag to know if the company setup "System Type" is set to "Point Of Sale" or not
llSite   = (gfGetMemVar("M_SYSTYPE ") = "P")

*-- Flag to hold if the EDITrans file were opened by this program or not
llOpEDITrn = .F.

*-- Flag to hold if the EDIAcPrt file were opened by this program or not
llOpAcPrt  = .F.

*-- Flag to hold if the EDIPD file were opened by this program or not
llOpEdiPd  = .F.
*N000232,1 HS 04/12/2001 Add these lines to add some new flags [End]

*-- This Variable Fill in PS Program
STORE lcEmptyAcc  TO lcGlChkAct, lcGlDstAct
lcOldValue = SPACE(10)
STORE 1 To lnMarker , lnSfUser
ldsydate   = gdSysDate
ldyestday  = gdSysDate - 1
lcApayMent = gfTempName()
lcPostRan  = gfTempName()
lcAmntendr = gfTempName()
*:B#604352,1 SSH Variable to hold the net sales.
lnCatNetSl = 0
*:B#604352,1 SSH [END]

lcBrow_Ttl = 'Detail Tender Amount'
*-- Screens on main screen.
lcWinCh1 = gfTempName()
lcWinCh2 = gfTempName()
lcWinCh3 = gfTempName()
*-- End Declaration variables.

IF !gfSetup()
  RETURN
ENDIF


*-- call function to open need files.
= lfOpenFils ()

IF EMPTY (lcSesNum)
  lcSesNum = gfSequence('CAPSESSNO')
ENDIF


*-- Initialize the "Codes Information" array.
DIMENSION laCodInfo [1,10] , laBAdj[1,2]
STORE '' To laCodInfo , laBAdj
STORE 1 To lnBAdj
laCodInfo = SPACE(0)
*-- Fill the "Codes Information" array.
laCodInfo[1,01] = "CREGADJRES"
laCodInfo[1,02] = "laBAdj"
laCodInfo[1,03] = "lnBAdj"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,07] = ""    
laCodInfo[1,08] = ""
laCodInfo[1,09] = ""
laCodInfo[1,10] = ""
= gfwCodePop( @laCodInfo, "CREGADJRES" ,'D' )

*-- Create temp file hold different tender type.
CREATE TABLE (gcWorkDir+lcAmntendr) ;
               (cMarker C(1)    , TranCode C(6)  , Descript C(30) ,;
                nsalstendr N(12,2) , nRetrtendr N(12,2), nNetClstdr N(12,2))
INDEX ON TranCode TAG (lcAmntendr)

*-- title screen.
lcWindTitle = 'Close Register'
*-- Do screen code.
lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose

=lfHoldKey('S')
=lfUnTrapKy()
DO (gcScrDir + gcWinAppl + '\PSCLSRG.SPR')
=lfHoldKey('R')

glQuitting = .T.  && This is modal screen, this rise quitting flag.

*-- call function to Close opened files.
= lfClosFils ()

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfvSafe
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/04/2000
*: Purpose   : Valid the safe.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvSafe()
*:*************************************************************
*
FUNCTION lfvSafe
PRIVATE lnAlias
lnAlias = SELECT(0)


IF MDOWN()
  RETURN
ENDIF
lcSaFe = IIF(EMPTY(lcSafe),lcSafe,LEFT(lcSafe,8))

SELECT APBANKS
GOTO TOP
IF llBrowse .OR. (!EMPTY(lcSafe) .AND. !SEEK(LEFT(lcSafe,8)))
  DECLARE laData[1]
  laData = ''
  lcOld_ttl   = lcFile_ttl
  lcPBrowTitl = "Registers"            && Browse title.
  lcTmpFld    = lcBrFields             && Save browse fields.
  lcBrFields = [cbnkcode:H="Register Code",cbnklndes:H="Long Description",cbnkshdes:H="Short Description",]+;
               [caddress1:H="Ship To Address1",caddress2:H="Ship To Address2",caddress3:H="Ship To Address3",]+;
               [caddress4:H="Ship To Address4",caddress5:H="Ship To Address5",caddress6:H="Ship To Address6",]+;
               [cphoneno:H="Phone",cfaxno:H="Fax"]
             
  llSele = ARIABROW('FOR cbnktype= "S"',lcPBrowTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
                       "","","cbnkcode","laData")
  
  *-- Restore browse fields.
  lcBrFields = lcTmpFld
  lcFile_ttl = lcOld_ttl
  llBrowse = .F.
  IF llSele
   lcSafe = laData[1]
   SHOW GET lcSafe
  ELSE
    lcSafe = ''
    _CUROBJ = OBJNUM(lcSafe)
    SHOW GET pbCICls    DISABLE    
  ENDIF
ENDIF

IF EMPTY (lcSafe)
  STORE 0 To lnOpenBaln , lnTotSales , lnTotRetrn , lnTotAdjst , lnNetClose ,;
             lnNwOpnBln , lntotsals,lnTotS, lnTotR
  IF !EOF(lcAmntendr)
    SELECT (lcAmntendr)
    ZAP
  ENDIF
    SHOW GET lnNwOpnBln DISABLE
    SHOW GET lnBAdj     DISABLE
    _CUROBJ = OBJNUM(lcSafe)
    
    llFirsttim = .F.
ELSE
  IF lcOldValue # lcSafe
    **- Check if this safe closed before in the same day.
    IF APCHECKS.dClosdate = gdsysdate
      =gfModalGen("TRM000000B00000","DIALOG",'','','This Register was closed before at the same date.')
    ENDIF
    *B606117,1 SSH Prevent closing register if closing found in date greater than system date
    IF APCHECKS.dClosdate > gdsysdate
      =gfModalGen("TRM000000B00000","DIALOG",'','','This Register was closed before for date greater than system date, Cannot proceed.')
      RETURN
    ENDIF
    *B606117,1 SSH Prevent closing register if closing found in date greater than system date
    *- Check if user is operator not adminstretor
    llTrnNocls = .F.  
    STORE 0  To lnStart , lndays
    IF SEEK (gcUser_id,'SYUUSER') .AND. SYUUSER.cUsr_levl = 'O'
      = SEEK(lcSafe,'APCHECKS')
      
      lndays = ldyestday - APCHECKS.dClosdate
      IF lndays >  1
        SELECT APPAYMNT
        FOR lnStart = 1  TO lndays
          IF SEEK (lcSafe+DTOC(APCHECKS.dClosdate+lnStart))
            llTrnNocls = .T.
            EXIT
          ENDIF
        ENDFOR
      ENDIF  
    ENDIF
    
    IF llTrnNocls
      *- Text Message   :- Access denied. Please check with your system  
      *- Text Message   :- administrator.                                
      *- Text Number    :- 00017
      *- button message :- OK
      *- button Number  :- 00000
      =gfModalGen("TRM00017B00000","DIALOG")
      STORE '' TO lcSafe      
      _CUROBJ = OBJNUM(lcSaFe)
      RETURN    
    ENDIF
  
    *-- If need login and check if User login have access on this safe.
    IF llUsersExt
      lcSfUserStr = APBANKS.mSafeUsers
      IF EMPTY(lcSfUserStr)
        *- Text Message   :- No users found for this safe.
        *- Text Number    :- 04166
        *- button message :- OK
        *- button Number  :- 00000
        =gfModalGen("TRM04166B00000","DIALOG")
        _CUROBJ = OBJNUM(lcSaFe)
        RETURN
      ENDIF  
    ENDIF

    lcCheckCode = gcUser_id
    IF !(gcUser_id $ lcSfUserStr) .OR. !llUsersExt
      =lfvChecks()
    ELSE
      llAlowAcss = .T.
    ENDIF  
    IF !llAlowAcss
      STORE 0 To lnOpenBaln , lnTotSales , lnTotRetrn , lnTotAdjst , lnNetClose ,;
                 lnNwOpnBln ,lntotsals, lnTotS, lnTotR
      SHOW GET lnNwOpnBln DISABLE
      SHOW GET lnBAdj     DISABLE
      lcSafe = ''
     _CUROBJ = OBJNUM(lcSafe)
    ELSE
      IF !EOF(lcAmntendr)
        SELECT (lcAmntendr)
        ZAP
      ENDIF
      STORE 0 To lnOpenBaln , lnTotSales , lnTotRetrn , lnTotAdjst , lnNetClose ,;
                 lnNwOpnBln  ,lntotsals, lnTotS, lnTotR
      SHOW GET lnNwOpnBln DISABLE
      SHOW GET pbCICls    DISABLE
      SHOW GET lnBAdj     DISABLE    
      SHOW GET lnNwOpnBln DISABLE
      SHOW GET lnBAdj     DISABLE
      SHOW GET lnNwOpnBln ENABLE
      SHOW GET lnBAdj     ENABLE
      SHOW GET pbCICls    ENABLE      
      *-- Collect data for selected safe.
      =lfcollect()
      llFirsttim = .T.
    ENDIF
  ENDIF
ENDIF

SELECT (lnAlias)
*-- Refresh the browse screen.
=lfActBrow()    
=lfRefresh()

*-- End OF lfvSafe.
*:*************************************************************
*: Name      : lfOpenFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/04/2000
*: Purpose   : Open need file that close when run option grid.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfOpenFils()
*:*************************************************************
*
FUNCTION lfOpenFils

*-- Open the APBANKS file.
IF !USED('APBANKS')
  llOpnApbnk = gfOpenFile(gcDataDir+'APBANKS','BANKCODE','SH')
ENDIF

*-- Open the APBANKS file.
IF !USED('APPAYMNT')
  llOpnApMnt = gfOpenFile(gcDataDir+'APPAYMNT','','SH')
  INDEX ON cBnkCode+DTOC(DpayDate) TAG (lcApayMent) OF (gcWorkDir+lcApayMent)
ENDIF

*-- Open the POSTRAN file.
IF !USED('POSTRAN')
  llOpnPostr = gfOpenFile(gcDataDir+'POSTRAN','','SH')
  INDEX ON cBnkCode+DTOC(trandate) TAG (lcPostRan) OF (gcWorkDir+lcPostRan)
ENDIF

*-- Open the POSTRAN file.
IF !USED('APCHECKS')
  llOpnApChk = gfOpenFile(gcDataDir+'APCHECKS','Bankcheck','SH')
ENDIF

*-- Open the CODES file.
IF !USED('CODES')
  llOpnCodes = gfOpenFile(gcDataDir+'CODES','CODES','SH')
ENDIF


IF !USED('SYCINST')
  llOpInst = gfOpenFile(gcSysHome+'SYCINST','','SH')
ENDIF


*--If there is a users defined in the system and login required.
llUsersExt = !EOF('SYUUSER') AND SYCINST.lInsLogRq   
*-- Close file, no need for it.
IF llOpInst
  =gfCloseFile('SYCINST')
ENDIF


*-- Make sure that SYUUSER is opened
IF !USED('SYUUSER')
  llSyuuser  =gfOpenFile(gcSysHome+'SYUUSER','CUSER_ID','SH')
ENDIF

*N000232,1 HS 04/12/2001 Open EDITrans file and EDIAcPrt [Begin]

*B038431,1 NNA 08/20/2004 (Begin) Open the 'EDITrans' File if the [NC] module installed
*IF !USED('EDITrans')
IF llNCInst .AND. !USED('EDITrans')
*B038431,1 NNA (End)

  llOpEDITrn = gfOpenFile(gcDataDir + 'EDITrans' , '' , 'SH')
ENDIF

IF !USED('EDIAcPrt')
  llOpAcPrt = gfOpenFile(gcDataDir + 'EDIAcPrt' , '' , 'SH')
ENDIF

*B038431,1 NNA 08/20/2004 (Begin) Open the 'EDITrans' File if the [NC] module installed
*IF !USED('EDIPD')
IF llNCInst .AND. !USED('EDIPD')
*B038431,1 NNA (End)

  llOpEdiPd =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
ENDIF
*N000232,1 HS 04/12/2001 Open EDITrans file and EDIAcPrt [End]

*-- End Of lfOpenFils.
*:*************************************************************
*: Name      : lfClosFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/04/2000
*: Purpose   : Open need file that close when run option grid.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfClosFils()
*:*************************************************************
*
FUNCTION lfClosFils

*-- Close Codes file if this program open it.
IF llOpnApbnk
  =gfCloseFile('APBANKS')
ENDIF

*-- Close APPAYMNT file if this program open it.
IF llOpnApMnt
  =gfCloseFile('APPAYMNT')
ENDIF

*-- Close POSTRAN file if this program open it.
IF llOpnPostr
  =gfCloseFile('POSTRAN')
ENDIF 

*-- Close CODES file if this program open it.
IF llOpnCodes
  =gfCloseFile('CODES')
ENDIF

*-- Close APCHECKS file if this program open it.
IF llOpnApChk
  =gfCloseFile('APCHECKS')
ENDIF

*-- Close SYUUSER file if this program open it.
IF llSyuuser
  =gfCloseFile('SYUUSER')
ENDIF

*N000232,1 HS 04/12/2001 Close EDITrans file and EDIAcPrt [Begin]
*-- Close EDITrans file if this program open it.
IF llOpEDITrn
  =gfCloseFile('EDITrans')
ENDIF

*-- Close EDIAcPrt file if this program open it.
IF llOpAcPrt
  =gfCloseFile('EDIAcPrt')
ENDIF

IF llOpEdiPd
  =gfCloseFile('EDIPD')
ENDIF

*N000232,1 HS 04/12/2001 Close EDITrans file and EDIAcPrt [End]


*- Erase Temp Files

IF USED(lcAmntendr)
  USE IN (lcAmntendr)
  ERASE (gcWorkDir+lcAmntendr+'.DBF')
  ERASE (gcWorkDir+lcAmntendr+'.CDX')
ENDIF

ERASE (gcWorkDir+lcPostRan+'.CDX')
ERASE (gcWorkDir+lcApayMent+'.CDX')


*-- End Of lfClosFils.
*:*************************************************************
*: Name      : lfcollect
*: Developer : ABDOU ELGENDI  - (ABD)
*: Date      : 12/04/2000
*: Purpose   : Calculate the amounts for selected safe.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfcollect()
*:*************************************************************
*
FUNCTION lfcollect
PRIVATE lnAlias,lcSeekExp
lnAlias = SELECT (0)
*:B#604352,1 SSH Variable to hold the net sales.
lnCatNetSl = 0
*:B#604352,1 SSH [END]
SELECT APCHECKS
=SEEK(lcSaFe)
IF EMPTY(nacbopbal)
  lnOpenBaln = 0
  ldSeekExp  = ""
ELSE
  IF DclosDate # ldsydate .AND. DclosDate  < ldsydate
    REPLACE nacbclbal WITH nacbopbal  
    lnOpenBaln = nacbclbal
    ldSeekExp  = DTOC(DclosDate +1)
  ELSE
    lnOpenBaln = nacbclbal
    ldSeekExp  = DTOC(ldsydate)
  ENDIF
ENDIF
lcSeekExp  = "lcSafe+ldSeekExp"
*-- Select appayment file and get the Today's opening Balance.

SELECT APPAYMNT
GOTO TOP
IF SEEK(&lcSeekExp)
  SCAN REST WHILE cBnkCode + DTOC(DpayDate) = lcSafe .AND. DpayDate < ldsydate
    lnOpenBaln = lnOpenBaln + nPayAmnt
  ENDSCAN
ENDIF

*-- get the total adjustments during the day from  appayment file.
IF SEEK(lcSafe+DTOC(ldsydate))
  SCAN REST WHILE cBnkCode + DTOC(DpayDate) = lcSafe + DTOC(ldsydate) FOR cPayMeth $ 'ABN'
    IF cPayMeth = 'B' .OR. (cPayMeth $ 'NA' .AND. !EMPTY(Batch))
      lnTotAdjst = lnTotAdjst + nPayAmnt
    ELSE
      IF nPayAmnt < 0
        lnTotS = lnTotS + nPayAmnt
      ELSE
        lnTotR = lnTotR + nPayAmnt
      ENDIF
      lntotsals = lntotsals   + (nPayAmnt*-1)
    ENDIF  
  ENDSCAN

ENDIF

SELECT POSTRAN
IF SEEK(lcSafe+DTOC(ldsydate))
  SCAN REST WHILE cBnkCode + DTOC(trandate) = lcSafe + DTOC(ldsydate)
    *-- Select POSTRAN File and get the total sales amount 
    *-- during the day from  POSTRAN  file.[Hold type '1'].
    IF POSTRAN.TranType = '1'
      lnTotSales = lnTotSales + nAmount
    ENDIF
    *-- Select POSTRAN File and get the total returns amount 
    *-- during the day from  POSTRAN  file.[Hold type '0'].
    IF POSTRAN.TranType = '0' .AND. cType = "R"
      lnTotRetrn = lnTotRetrn + nAmount*-1
    ENDIF
   
    IF POSTRAN.TranType = '4' .AND. !EMPTY(POSTRAN.TranCode)
      IF !SEEK (POSTRAN.TranCode,lcAmntendr)
        SELECT (lcAmntendr)
        APPEND BLANK
        REPLACE TranCode   WITH POSTRAN.TranCode ,;
                Descript   WITH POSTRAN.Desc1    ,;
                nsalstendr WITH 0  ,;
                nRetrtendr WITH 0  ,;
                nNetClstdr WITH 0
      ENDIF
      SELECT (lcAmntendr)
      IF POSTRAN.nAmount < 0 
        REPLACE nsalstendr WITH nsalstendr + ABS(POSTRAN.nAmount),; 
                nNetClstdr WITH nNetClstdr + ABS(POSTRAN.nAmount)
        *:B#604352,1 SSH Variable to hold the net sales.
        lnCatNetSl = lnCatNetSl + ABS(POSTRAN.nAmount)
        *:B#604352,1 SSH {END}
        SELECT POSTRAN
      ELSE
        IF .T.
          REPLACE nRetrtendr WITH nRetrtendr + ABS(POSTRAN.nAmount),;
                  nNetClstdr WITH nNetClstdr - ABS(POSTRAN.nAmount)
        *:B#604352,1 SSH Variable to hold the net sales.
        lnCatNetSl = lnCatNetSl - ABS(POSTRAN.nAmount)
        *:B#604352,1 SSH [END]
        ELSE
        ENDIF
      ENDIF  
    ENDIF
  ENDSCAN
ENDIF

*-- Get net closing balance that diff. bettween the tatal sales ans total return.
lnNetClose = lnTotSales - lnTotRetrn

=lfActBrow()
SELECT (lnAlias)
*-- End Of lfcollect.
*:*************************************************************
*: Name      : lfOldValue
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : To store the old value value
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfOldValue()
*:*************************************************************
*
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))

*-- End Of lfOldValue.
*:*************************************************************
*: Name      : lfActBrow
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Original Browse function.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfActBrow()
*:*************************************************************
*
FUNCTION lfActBrow

lcSelect = SELECT()

SELECT (lcAmntendr)
GO TOP
lnMarker = RECNO()
*:B#604352,1 SSH add picture.
*lcBfields = [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
            [Descript  :30:H='Description'  :R:30,]+;
            [nsalstendr:15:H='Paid Amount' :R:12,]+;
            [nRetrtendr:15:H='Refund Amount':R:12,]+;
            [nNetClstdr:15:H='Net Balance'  :R:12]

lcBfields = [cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
            [Descript  :30:H='Description'  :R:30,]+;
            [nsalstendr:15:H='Paid Amount' :R:12 :P='999999999.99',]+;
            [nRetrtendr:15:H='Refund Amount':R:12 :P='999999999.99',]+;
            [nNetClstdr:15:H='Net Balance'  :R:12 :P='999999999.99']


*:B#604352,1 SSH add picture.[END]
BROWSE FIELDS &lcBfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcBrow_Ttl) ;
       WHEN lfwBrow() ;
       VALID :F lfvBrows();
       WINDOW (lcWinCh1);
       IN WINDOW (gcBaseWind)

SELECT (lcSelect)
SHOW GETS WINDOW (lcWinCh1)  ENABLE
RETURN

*-- End OF lfActBrow.
*:*************************************************************
*: Name      : lfwBrow
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : When Browse function.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfwBrow()
*:*************************************************************
*
FUNCTION lfwBrow
glFromBrow = .T.
lnMarker = RECNO()
SHOW WINDOW (lcBrow_Ttl) REFRESH
RETURN

*-- End OF lfwBrow.
*:*************************************************************
*: Name      : lfvBrows
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Valid Browse function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfvBrows()
*:*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcBrow_Ttl)
  = gfStopBrow()
ENDIF

*-- End OF lfvBrows.
*:*************************************************************
*: Name      : lfReadAct
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Read Activate function clear traping.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfReadAct()
*:*************************************************************
*
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*-- End OF lfReadAct
*:*************************************************************
*: Name      : lfTrapKy
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Trap key.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfTrapKy()
*:*************************************************************
*
FUNCTION lfTrapKy

IF WONTOP() = lcBrow_Ttl
  ON KEY LABEL TAB     DO lptab
  ON KEY LABEL backtab DO lpshifttab
ENDIF  
RETURN

*-- End OF lfTrapKy
*:*************************************************************
*: Name      : lfUnTrapKy
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Releases Key traps necessary for browse.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfUnTrapKy()
*:*************************************************************
*
FUNCTION lfUnTrapKy

ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL ESC DO lpDetEsc
RETURN

*-- End OF lfUnTrapKy
*:*************************************************************
*: Name      : lpTab
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Tab key trapping.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpTab
*:*************************************************************
PROCEDURE lpTab

IF WONTOP(lcBrow_Ttl)
  _CUROBJ=OBJNUM(pbCICls)
  ACTIVATE WINDOW (lcWinCh3)
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF
RETURN
*-- End OF lpTab.
*:*************************************************************
*: Name      : lpShiftTab
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Shift Tab key trapping.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpShiftTab
*:*************************************************************
PROCEDURE lpShiftTab 

IF WONTOP(lcBrow_Ttl)
   _CUROBJ=_CUROBJ-1
ENDIF
RETURN

*-- End Of lpShiftTab.
*:*************************************************************
*: Name      : lfHoldKey
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Save and restore traping.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lcKyOper -> Operation Save or Restore
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfHoldKey()
*:*************************************************************
FUNCTION lfHoldKey
PARA lcKyOper

DO CASE
  CASE lcKyOper='S'
    lcHldTab = ON('KEY','TAB')
    lcHldBtb = ON('KEY','BACKTAB')    
    lcHldEsc = ON('KEY','ESC')
    lcHldAtB = ON('KEY','ALT+B')
  CASE lcKyOper='R'
    ON KEY LABEL TAB     &lcHldTab
    ON KEY LABEL BACKTAB &lcHldBtb
    ON KEY LABEL ESC     &lcHldEsc
    ON KEY LABEL ALT+B   &lcHldAtB
ENDCASE

*-- End OF lfHoldKey.
*:*************************************************************
*: Name      : lpDetEsc
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpDetEsc
*:*************************************************************
*
PROCEDURE lpDetEsc

IF !llFirsttim
  IF WONTOP() # 'APSFUSR'
    CLEAR READ
  ENDIF  
ELSE  
  _CUROBJ = OBJNUM(pbCIcan)
  KEYBOARD '{ENTER}'
ENDIF
RETURN

*-- End Of lpDetEsc
*:*************************************************************
*: Name      : lfvNewbaln
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Enable close button if we have value.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  =lfvNewbaln()
*:*************************************************************
*
FUNCTION lfvNewbaln

IF lnNwOpnBln < 0
  *-- Text Message  : " The closing balance should be greater than zero"
  *-- Text Number   : 04072.
  *-- Button Message: OK
  *-- Button Number : 00000
  =gfModalGen("TRM04072B00000","DIALOG",'The closing balance'+'|'+'Zero.')
  lnNwOpnBln = lcOldValue
  RETURN
ENDIF

*-- End Of lfvNewbaln.
*:*************************************************************
*: Name      : lfvCancel
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Update the register with the new close amount
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  =lfvCancel()
*:*************************************************************
*
FUNCTION lfvCancel

IF EMPTY(lcSafe)
  CLEAR READ
ELSE
  *- Text Message  :- Are you sure you want to cancel this session 
  *- Text Message  :-   and lose all your ð?
  *- Text Number   :- 42063
  *- Text button   :- \<Yes --- \<No 
  *- Button Number :- 42002
  
  IF gfModalGen('TRM42063B42002','DIALOG','changes') = 1
    IF !EOF(lcAmntendr)
      SELECT (lcAmntendr)
      ZAP
    ENDIF
    STORE 0 To lnOpenBaln , lnTotSales , lnTotRetrn , lnTotAdjst , lnNetClose ,;
               lnNwOpnBln  ,lntotsals, lnTotS, lnTotR
    SHOW GET lnNwOpnBln DISABLE
    SHOW GET pbCICls    DISABLE
    SHOW GET lnBAdj     DISABLE    
    SHOW GET lnNwOpnBln DISABLE
    SHOW GET lnBAdj     DISABLE
    *:B#604352,1 SSH Net Sales.
    lnCatNetSl = 0
    SHOW GET lnCatNetSl DISABLE
    *:B#604352,1 SSH [END]
    STORE '' TO lcSafe
    llFirsttim = .F.
    =lfRefresh()
    *-- Refresh the browse screen.
    =lfActBrow()    
  ENDIF
ENDIF
_CUROBJ = OBJNUM(lcSafe)

*-- End OF lfvCancel.
*:*************************************************************
*: Name      : lfCloseReg
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Update the register with the new close amount
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  =lfCloseReg()
*:*************************************************************
*
FUNCTION lfCloseReg

*-- give user message to approve the close ammount.
*-- Text Message : - Are you sure you want to proceed updating.
*-- Text No      : - 42195.
*-- Text Button  : - Yes  - No .
*-- Button No    : -42002
IF gfModalGen('QRM42195B42002','ALERT','closing register.') = 2
  RETURN
ENDIF

IF lnNwOpnBln < 0
  *-- Text Message  : " The closing balance should be greater than zero"
  *-- Text Number   : 04072.
  *-- Button Message: OK
  *-- Button Number : 00000
  =gfModalGen("TRM04072B00000","DIALOG",'The closing balance'+'|'+'Zero')
  lnNwOpnBln = lcOldValue
  _CUROBJ    = OBJNUM(lnNwOpnBln)
  RETURN
ENDIF

*-- Get the year and period from apsetup.
IF lfVDtMsg(gcPrnt_Cmp ,@lcFisPrd,@lcFisYer,ldsydate)
  lcPeriod = lcFisPrd
  lcYear   = lcFisYer
ELSE
  _CUROBJ = OBJNUM(lcSaFe)
  RETURN
ENDIF  


*-- made to (gfSequence)
lcSeqNum = gfSequence('CPAYDOCNO')

*- Get the new opening balance for the follwing day.

*N000232,1 HS 04/12/2001 Fix this equation; do not reverse the closing balance sign [Begin]
*lnNewadj = (lnNwOpnBln*-1) -  (lnOpenBaln + lnTotAdjst + lnTotR + lnTotS)
lnNewadj = (lnOpenBaln - lnTotAdjst - ABS(lnTotR) + ABS(lnTotS)) - lnNwOpnBln  
*N000232,1 HS 04/12/2001 Fix this equation; do not reverse the closing balance sign [End]
*lnNewadj = lnNwOpnBln -  (lnOpenBaln + lnTotAdjst - ABS(lnTotR) + ABS(lnTotS))
LLF = IIF(lnNwOpnBln >  (lnOpenBaln + lnTotAdjst - ABS(lnTotR) + ABS(lnTotS)),.T.,.F.)
*--  Add record to the payment file With the new balance. 
llAddPymnt = lnOpenBaln <> 0 .OR. lnNwOpnBln <> 0 .OR. lnTotAdjst <> 0 .OR. ;
             lntotsals <> 0 .OR. lnTotS - lnTotR <> 0

IF llAddPymnt .AND. lnNewadj <> 0
  SELECT APPAYMNT
  APPEND BLANK
  *N000232,1 HS 04/12/2001 Add the Cash Register Adjustment Reason to the APPAYMNT file [Begin]
  *REPLACE CPAYTYPE  WITH "P"           ,;
  *        CPAYMETH  WITH "B"           ,;
  *        CBNKCODE  WITH lcSaFe        ,;
  *        CCHKACCT  WITH 'CASH REGISTR',;        
  *        CPAYDOCNO WITH lcSeqNum      ,;
  *        DPAYDATE  WITH ldsydate      ,;
  *        CFISFYEAR WITH lcYear        ,;
  *        CFSPPRDID WITH lcPeriod      ,;
  *        LPAYADVAN WITH .F.           ,;
  *        NPAYAMNT  WITH lnNewadj      ,;
  *        CPAYRECST WITH 'O'           ,;
  *        cCurrCode WITH gcBaseCurr    ,;
  *        nExRate   WITH 1             ,;
  *        nCurrUnit WITH 1             ,;
  *        cPayComp  WITH 'Closing transaction on '+DTOC(DATE())+' - ' + TIME()
  REPLACE CPAYTYPE  WITH "P"           ,;
          CPAYMETH  WITH "B"           ,;
          CBNKCODE  WITH lcSaFe        ,;
          CCHKACCT  WITH 'CASH REGISTR',;        
          CPAYDOCNO WITH lcSeqNum      ,;
          DPAYDATE  WITH ldsydate      ,;
          CFISFYEAR WITH lcYear        ,;
          CFSPPRDID WITH lcPeriod      ,;
          LPAYADVAN WITH .F.           ,;
          NPAYAMNT  WITH lnNewadj      ,;
          CPAYRECST WITH 'O'           ,;
          cCurrCode WITH gcBaseCurr    ,;
          nExRate   WITH 1             ,;
          nCurrUnit WITH 1             ,;
          cBnkAdj   WITH laBAdj[lnBAdj,2] ,;
          cPayComp  WITH 'Closing transaction on '+DTOC(DATE())+' - ' + TIME()
  *N000232,1 HS 04/12/2001 Add the Cash Register Adjustment Reason to the APPAYMNT file [End]

  =gfAdd_Info()  && Add the audit information to the record.

  *N000232,1 HS 04/12/2001 Check if NC module is installed and the company setup
  *N000232,1               "System Type" is set to "Point Of Sale" then add a
  *N000232,1               record in the EDITrans to be used to send the
  *N000232,1               Close Cash Register [Begin]
  IF llNCInst .AND. llSite
    PRIVATE lcCodeOrdr
  
    SELECT CODES
    lcCodeOrdr = ORDER()
    SET ORDER TO IDRLTFNAME
    IF SEEK("N" + "Y" + PADR("CSITEID" , 10))
      LOCATE REST;
             WHILE cDefCode + cRltField + cFld_Name = "N" + "Y" + PADR("CSITEID" , 10);
             FOR cRltd_Nam = "CCMSITETYP" .AND. cRltd_Vlu = "B"
      IF FOUND()
        SELECT EDIAcPrt
        LOCATE FOR cSiteID = CODES.cCode_No
        IF FOUND() AND SEEK(EdiAcPrt.cPartCode+'812','EdiPd')
          INSERT INTO EDITrans;
          (cEDITrnTyp,Key,cPartner,Type,cStatus,lInterComp) VALUES  ;
          ("812","7"+APPaymnt.cPayDocNo+APPaymnt.cBnkCode,EDIAcPrt.cPartner,EDIAcPrt.Type,"N",EDIAcPrt.lInterComp)
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
    ENDIF
    SELECT CODES
    IF EMPTY(lcCodeOrdr)
      SET ORDER TO
    ELSE
      SET ORDER TO &lcCodeOrdr
    ENDIF
  ENDIF
  *N000232,1 HS 04/12/2001 Check if NC module is installed and this is a Cash Register [End]
  
ENDIF  

*-- update Apchecks with last date and last amount
SELECT APCHECKS
=SEEK(lcSaFe)
REPLACE  DclosDate  WITH ldsydate  ,;
         cclos_time WITH TIME()    ,;
         nacbopbal WITH lnNwOpnBln


*-- Check the AP intalled and open the files needed if 
*-- AP was installed else disable link to Gl.
llApLink = (OCCURS('AP',gcCmpModules)<>0)
IF llApLink .AND. llAddPymnt .AND. lnNewadj <> 0
   STORE IIF(EMPTY(APCHECKS.CCHKGLACC),lcEmptyAcc, APCHECKS.CCHKGLACC) ;
         TO lcGlChkAct
  DIMENSION laTrmRltFd [1,2]
  laTrmRltFd[1,1] = 'CGLACCT   '
  laTrmRltFd[1,2] = 'lcGlDstAct'
  =gfRltFld(laBAdj[lnBAdj,2],@laTrmRltFd,'CREGADJRES')
  lcGlDstAct= IIF(EMPTY(lcGlDstAct),lcEmptyAcc,lcGlDstAct)


  =gfOpenFile(gcDataDir+'APDIST','PAYMNTS','SH')
  *-- Add record to distribution file.
  SELECT APDIST
  APPEND BLANK
  REPLACE CINVNO    WITH lcSeqNum    ,;
          DAPDTRDAT WITH ldsydate    ,;
          LAPDPOST  WITH .F.         ,;
          CAPDREF   WITH lcSeqNum    ,;
          CAPDGLACT WITH lcGlchkact  ,;
          NAPDAMNT  WITH lnNewadj    ,;
          CAPDACTID WITH "C"         ,;
          CFISFYEAR WITH lcYear      ,;
          CFSPPRDID WITH lcPeriod    ,;
          CAPSESSNO WITH lcSesNum    ,;
          CAPDTRTYP WITH "B"         ,;
          CBNKCODE  WITH lcSaFe      ,;
          CCHKACCT  WITH lcCheckcode ,;
          cCurrCode WITH gcBaseCurr  ,;
          nExRate   WITH 1           ,;
          nEqvAmnt  WITH NAPDAMNT    ,;
          CCHKACCT  WITH 'CASH REGISTR'
          
          =gfAdd_Info()  && Add the audit information to the record.
          *-- Replace user with the user name
          REPLACE CADD_USER WITH lcCheckcode          

                  
  *-- Add record to distribution file.
  SELECT APDIST
  APPEND BLANK
  REPLACE CINVNO    WITH lcSeqNum    ,;
          DAPDTRDAT WITH ldsydate    ,;
          LAPDPOST  WITH .F.         ,;
          CAPDREF   WITH lcSeqNum    ,;
          CAPDGLACT WITH lcGldstact  ,;
          NAPDAMNT  WITH 0 - lnNewadj,;
          CAPDACTID WITH "D"         ,;
          CFISFYEAR WITH lcYear      ,;
          CFSPPRDID WITH lcPeriod    ,;
          CAPSESSNO WITH lcSesNum    ,;
          CAPDTRTYP WITH "B"         ,;
          CBNKCODE  WITH lcSaFe      ,;
          CCHKACCT  WITH lcCheckcode ,;
          cCurrCode WITH gcBaseCurr  ,;
          nExRate   WITH 1           ,;
          nEqvAmnt  WITH NAPDAMNT    ,;
          CCHKACCT  WITH 'CASH REGISTR'
          =gfAdd_Info()  && Add the audit information to the record.
          REPLACE CADD_USER WITH lcCheckcode
ENDIF
 
*-- End OF lfCloseReg.
*:*************************************************************
*: Name      : lfvUsr_Pass
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Valid function for safe user password.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvUsr_Pass()
*:*************************************************************
*
FUNCTION lfvUsr_Pass

lcCurUsrPas = ALLTRIM(laSafeUsrs[lnSfUser,2])

IF SYS(2007,ALLTRIM(lcUsrPass))=lcCurUsrPas
  llAlowAcss = .T.
  CLEAR READ
ELSE
  llAlowAcss = .F.
  lcUsrPass  = SPACE(8)
  *--Invalid user password.
  =gfModalGen('TRM42138B42000','DIALOG','user password')
  IF lnUsrCnt = 3
    CLEAR READ
  ELSE
    lnUsrCnt = lnUsrCnt + 1
    _CUROBJ=OBJNUM(lcUsrPass)
    RETURN
  ENDIF
ENDIF
*-- End OF lfvUsr_Pass.
*:*************************************************************
*: Name      : lfvChecks
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Valid function for safe user password.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvChecks()
*:*************************************************************
*
FUNCTION lfvChecks

IF llUsersExt
  *--'BROWSE FOR USERS'
  lnStrLn=1
  FOR lnCnt = 1 TO INT(LEN(lcSfUserStr)/11)
    lcSfUsr = SUBSTR(lcSfUserStr,lnStrLn,10)
    IF !EMPTY(lcSfUsr) 
      =SEEK(lcSfUsr,'SYUUSER')
      DIMENSION laSafeUsrs[lnCnt,3]
      laSafeUsrs[lnCnt,1] = SYUUSER.cUsr_Name
      laSafeUsrs[lnCnt,2] = SYUUSER.cUsr_Pass
      laSafeUsrs[lnCnt,3] = lcSfUsr
      lnStrLn =lnStrLn + 11 
    ELSE
      EXIT        
    ENDIF  
  ENDFOR 
  llAlowAcss = .F.
  lcUsrPass  = SPACE(8)
  lnUsrCnt = 1
  *--Call select user and edit user password screen.

  DO (gcScrDir + 'APSFUSR.SPR')

  IF !llAlowAcss
    _CUROBJ = OBJNUM(lcSaFe)
    RETURN
  ENDIF
  lcCheckCode = laSafeUsrs[lnSfUser,3]  
ELSE
  llAlowAcss = .T.
ENDIF

*-- End OF lfvChecks.
*:*************************************************************