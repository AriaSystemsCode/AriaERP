*!**************************************************************************
*! Name      : DIRMAIN.PRG
*! Developer : Nader NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : New Option on Invoice Sales Order (Generate Consolidated Invoices)
*!           : for Customer/Direct Corporate Clothing (DIR03)
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C123950,1
*!**************************************************************************
*! Modifications
*!**************************************************************************

*-- laScrMode [1] --> SELECT MODE
*-- laScrMode [2] --> VIEW   MODE
*-- laScrMode [3] --> EDIT   MODE
*-- laScrMode [4] --> ADD    MODE


PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfDispScr
*! Developer : Nader NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Function to Display 'Generate Consolidated Invoices' and
*!           : 'Generate Invoices from ship Date' Screens.
*!**************************************************************************
*! Example   : lfDispScr()
*!**************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfDispScr
PARAMETER llConsInv
PRIVATE lnOldAlS , lcAccount,lcNeeded
STORE ' ' TO lcAccount
STORE .F. TO llBrowse
lcNeeded = gfTempName()
lnOldAlS = SELECT(0)
= lfCRTTEMP()
PUSH KEY                                     && To save the the current on key label
ON KEY
IF llConsInv
  DO (gcScrDir + gcWinAppl + '\ARCOINV.SPR')   && calling the screen ARCOINV
ELSE
  DO (gcScrDir + gcWinAppl + '\ARUNCOIN.SPR')   && calling the screen ARCOINV
ENDIF
POP KEY                                      && To Restore the previous assignments for on key label
SELECT(lnOldAlS)
RETURN
*-- END OF FUNCTION lfDispScr

*!*******************************************************************************
*! Name      : lfWDispScr
*! Developer : NADER NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Valid Function to validate screen when it displaying
*!*******************************************************************************
*! Calls     : from the screen
*!*******************************************************************************
*! Example   :  =lfWDispScr()
*!*******************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfWDispScr
PARAMETER llConsScr
IF llConsInv  && if user is working with 'Generate Consolidated Invoices' Screen
  SHOW GET ldFDate    DISABLE
  SHOW GET ldToDate   DISABLE
  SHOW GET IbCustPo   DISABLE
  SHOW GET lcCustPo   DISABLE
  SHOW GET pbOk       DISABLE
ELSE          && if user is working with 'Generate Invoices from Ship Date' Screen
  SHOW GET pbUnOk     DISABLE
ENDIF
*-- End of lfWDispScr.

*!**************************************************************************
*! Name      : lfCRTTEMP
*! Developer : Nader NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Create temp file .
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfCRTTEMP
lcTrgCursr = lcInvLine + 'A'
CREATE CURSOR (lcTrgCursr) (ACCOUNT C(FSIZE('Account', 'CUSTOMER')),;
              FDATE D(10) , TODATE D(10) ,CUSTPO C(15),DISPSCR L(1),llNCustPo L(1))
INSERT INTO (lcTrgCursr) (ACCOUNT,FDATE,TODATE,CUSTPO,DISPSCR,llNCustPo) VALUES ;
           (SPACE(FSIZE('Account', 'CUSTOMER')),{},{},'',.F.,.F.)

CREATE TABLE (gcWorkDir+lcNeeded)(ORDER C(6),PIKTKT C(6))
INDEX ON (ORDER+PIKTKT) TAG (lcNeeded) OF (lcNeeded)

RETURN ''
*-- END OF FUNCTION lfCRTTEMP
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Nader NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Validat the screen key (Account).
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Example   : = lfvAccount()
*!*************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfvAccount

PRIVATE lnCurAlias
lcTrgCursr = lcInvLine + 'A'
lnCurAlias = SELECT(0)
lcAccount  = PADR(ALLTRIM(lcAccount), FSIZE('Account', 'CUSTOMER'))
XACCOUNT   = SPACE(FSIZE('Account', 'CUSTOMER'))
IF MDOWN()
  RETURN
ENDIF
IF llBrowse OR (!EMPTY(lcAccount) AND !SEEK('M'+lcAccount,'Customer')) OR '?' $ lcAccount ;
  llBrowse = .F.
  DO CUSBROWM WITH XACCOUNT
  lcAccount = IIF(!EMPTY(XACCOUNT),XACCOUNT,lcOldValue)
  IF EMPTY(lcAccount) 
    _CUROBJ = OBJNUM(lcAccount)
  ENDIF  
ENDIF  
SHOW GETS
IF !EMPTY(lcAccount) AND SEEK('M'+lcAccount,'Customer')
  IF Customer.Status <> 'A'
    *--Message : 40113
    *--Account XXXXX is Non-Active. Invoicing is not allowed.
    *--Button : 00000
    *--Ok
    =gfModalGen('TRM40113B00000','ALERT','Account '+Customer.Account)
    _CUROBJ = OBJNUM(lcAccount)
    SHOW GET ldFDate    DISABLE
    SHOW GET ldToDate   DISABLE
    SHOW GET IbCustPo   DISABLE
    SHOW GET lcCustPo   DISABLE
    SHOW GET pbOk       DISABLE
    RETURN
  ENDIF
  IF CUSTOMER.CONSOL='N'
    =gfModalGen("QRM00000B00000",.F.,.F.,.F.,'Consolidated invoicing not valid for this customer' )
    _CUROBJ = OBJNUM(lcAccount)
    SHOW GET ldFDate    DISABLE
    SHOW GET ldToDate   DISABLE
    SHOW GET IbCustPo   DISABLE
    SHOW GET lcCustPo   DISABLE
    SHOW GET pbOk       DISABLE
    RETURN
  ELSE
    SHOW GET ldFDate    ENABLE
    SHOW GET ldToDate   ENABLE
    SHOW GET IbCustPo   ENABLE
    SHOW GET lcCustPo   ENABLE
  ENDIF
ENDIF
=lfRefresh('ARCOINV')
SELECT(lcTrgCursr)
REPLACE &lcTrgCursr..ACCOUNT WITH lcAccount
SELECT (lnCurAlias)

*-- END OF FUNCTION lfvAccount

*!*************************************************************
*! Name      : lfvDate
*! Developer : Nader NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Validat the screen key Date Range.
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Example   : = lfvAccount()
*!*************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfvDate
PARAMETER llConsDate
PRIVATE lnOldAlS 
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)
IF MDOWN()
  RETURN
ENDIF
lcTrgCursr = lcInvLine + 'A'
IF _CUROBJ = OBJNUM(ldToDate) AND ldToDate < ldFDate 
  =gfModalGen("QRM00000B00000",.F.,.F.,.F.,'"To" Date must be greater than the "From" Date' )
  _CUROBJ = OBJNUM(ldToDate)
  IF !llConsDate
    SHOW GET pbUnOk DISABLE
  ENDIF
  RETURN
ENDIF
SELECT(lcTrgCursr)
REPLACE &lcTrgCursr..FDATE WITH ldFDate , &lcTrgCursr..TODATE WITH ldToDate
IF !llConsDate AND ldToDate <> {}
  SHOW GET pbUnOk ENABLE
ENDIF
IF llConsDate AND ldToDate <> {}
  SHOW GET pbOk ENABLE
ENDIF
SELECT(lnOldAlS)
*--End of function lfvDate.
*!**************************************************************************
*! Name      : lfConsOk
*! Developer : Nader NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Valid function of the push button pbOk in Generate consolidated.
*!           : Invoices Screen.
*!**************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfConsOk
PRIVATE lcScan , lcOldOrHdr , lcOldOrLin , lnRecNo , lcCurOrder
STORE '' TO lcScan , lcOldOrHdr , lcOldOrLin , lcCurOrder
lcTrgCursr = lcInvLine + 'A'

IF !USED('PACK_HDR')
  = gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
ENDIF
IF !USED('PACK_LIN')
  = gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
ENDIF
IF !USED('STYLE')
  = gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
ENDIF
IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH') 
ENDIF
IF !USED('ORDLINE')
  = gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH') 
ENDIF
IF !USED('PIKTKT')
  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH') 
ENDIF
SELECT ORDHDR
SET RELATION TO
SET RELATION TO Ordhdr.order INTO Pack_hdr ADDITIVE
SET RELATION TO Ordhdr.cordtype+ Ordhdr.order INTO Ordline ADDITIVE
SELECT PACK_HDR
SET RELATION TO
SET RELATION TO Pack_hdr.order + Pack_hdr.piktkt INTO Piktkt ADDITIVE
SET RELATION TO Pack_hdr.pack_no INTO Pack_lin ADDITIVE

SELECT ORDLINE
lcOldOrLin = SET('ORDER')
SET ORDER TO TAG Ordlinst

SELECT ORDHDR
lcOldOrHdr = SET('ORDER')
SET ORDER TO TAG ORDACCT

IF !EMPTY(&lcTrgCursr..CUSTPO) AND !(&lcTrgCursr..llNCustPo)
  lcScan = "FOR ORDHDR.CUSTPO = &lcTrgCursr..CUSTPO AND !(ORDHDR.STATUS $ 'XC')"
ELSE
  lcScan = "FOR !(ORDHDR.STATUS $ 'XC')"
ENDIF
IF SEEK(&lcTrgCursr..Account)
  SCAN REST WHILE account+cordtype+order = &lcTrgCursr..Account &lcScan
    WAIT WINDOW "Checking Order : " + ORDHDR.ORDER NOWAIT
    *--If OrdHdr.Order = Pack_hdr.Order and there is a Packing List for the piktkt and this 
    *-piktkt not Completed or Canceled and there are lines for the packing list in the 
    *-Pack_line file and the dShipDate for the packing list is between the date Range that the 
    *-user input, then I'll do the lfGetreport function in Ariinv.prg. to be as the standard
    IF SEEK(OrdHdr.Order,'Pack_Hdr') 
      SELECT PACK_HDR

      *--Scan For Not Empty DshipDate Specially if user didn't input the (From) Date and entered only the (To) Date
      SCAN REST WHILE Order+Store+Pack_No= OrdHdr.Order FOR ;
           BETWEEN(Pack_Hdr.dShipDate,&lcTrgCursr..FDate,&lcTrgCursr..ToDate) ;
           AND !EMPTY(Pack_Hdr.dShipDate)
      
          IF !EMPTY(Pack_Hdr.PIKTKT) AND SEEK(PACK_HDR.Pack_No,'Pack_Lin') 
            IF SEEK(Pack_Hdr.Order + Pack_Hdr.PIKTKT,'PIKTKT') AND !(PIKTKT.STATUS $ 'XC')
              SELECT PIKTKT
              SCAN REST WHILE ORDER+PIKTKT= Pack_Hdr.Order + Pack_Hdr.PIKTKT FOR !(PIKTKT.STATUS $ 'XC')
                *--If the CustPo that entered is new , I replace it in the Empty CustPo field in
                 *-the OrdHdr and Piktkt File
                  IF !EMPTY(&lcTrgCursr..CUSTPO) AND &lcTrgCursr..llNCustPo
                    IF EMPTY(ORDHDR.CUSTPO)
                      REPLACE ORDHDR.CUSTPO WITH &lcTrgCursr..CUSTPO
                    ENDIF
                    IF EMPTY(PIKTKT.CUSTPO)
                    REPLACE PIKTKT.CUSTPO WITH &lcTrgCursr..CUSTPO
                    ENDIF
                  ENDIF
                  lnRecNo = RECNO('PACK_HDR')
                  *--If there is more one piktkt for the same order so with the first one the order's
                  *--record will be locked , so I make it unlock to can handele it with the following piktkts
                  IF ORDHDR.ORDER == lcCurOrder
                    = lfUnLock()
                  ELSE
                    lcCurOrder = ORDHDR.ORDER
                  ENDIF
                  =lfGetOrder(OrdHdr.Order,OrdHdr.Store,.T.,PACK_HDR.PIKTKT)
                  IF BETWEEN(lnRecNo,1,RECCOUNT('PACK_HDR'))
                    GOTO lnRecNo IN PACK_HDR
                  ENDIF
              ENDSCAN
            ENDIF
          ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
IF EOF(lcInvHdr)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No order falls in the selected range')
  RETURN(.F.)
ENDIF
SELECT(lcTrgCursr)
REPLACE DISPSCR WITH .T.  && to do the Trigger that disable screen lcWinch1
*-- return to the previous Tag in OrdHdr and OrdLine to make user be able to cancel this Process
 *- and input an order in the order field at the standard screen
SET ORDER TO &lcOldOrHdr IN ORDHDR
SET ORDER TO &lcOldOrLin IN ORDLINE

*-- END OF FUNCTION lfConsOk
*!**************************************************************************
*! Name      : lfUnConsOk
*! Developer : Nader NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Valid function of the push button pbOk in Generate Invoices 
*!           : from Ship Date. Screen.
*!**************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfUnConsOk
PRIVATE lnRecNo , lcCurOrder
STORE 0 TO lnRecNo
STORE '' TO lcCurOrder
lcTrgCursr = lcInvLine + 'A'

IF !USED('PACK_HDR')
  = gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
ENDIF
IF !USED('PACK_LIN')
  = gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
ENDIF
IF !USED('STYLE')
  = gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
ENDIF
IF !USED('ORDHDR')
  = gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH') 
ENDIF
IF !USED('ORDLINE')
  = gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH') 
ENDIF
IF !USED('PIKTKT')
  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH') 
ENDIF
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH') 
ENDIF

SELECT PACK_HDR
SET RELATION TO
SET RELATION TO Pack_hdr.order + Pack_hdr.piktkt INTO Piktkt ADDITIVE
SET RELATION TO Pack_hdr.pack_no INTO Pack_lin ADDITIVE
SELECT ORDHDR
SET RELATION TO
SET RELATION TO Ordhdr.order INTO Pack_hdr ADDITIVE
SET RELATION TO Ordhdr.cordtype+ Ordhdr.order INTO Ordline ADDITIVE
SET RELATION TO 'M'+ Ordhdr.ACCOUNT INTO CUSTOMER ADDITIVE
SCAN FOR !(ORDHDR.STATUS $ 'XC') AND CUSTOMER.CONSOL='N'
  *--If OrdHdr.Order = Pack_hdr.Order and there is a Packing List for the piktkt and this 
  *-piktkt not Completed or Canceled and there are lines for the packing list in the 
  *-Pack_line file and the dShipDate for the packing list is between the date Range that the 
  *-user input, then I'll do the lfGetreport function in Ariinv.prg. to be as the standard
  WAIT WINDOW "Checking Order : " + ORDHDR.ORDER NOWAIT 
  IF SEEK(OrdHdr.Order,'Pack_Hdr') 
    SELECT Pack_Hdr
    *--Scan For Not Empty DshipDate Specially if user didn't input the (From) Date and entered only the (To) Date
    SCAN REST WHILE Order+Store+Pack_No= OrdHdr.Order FOR ;
         BETWEEN(Pack_Hdr.dShipDate,&lcTrgCursr..FDate,&lcTrgCursr..ToDate) ;
         AND !EMPTY(Pack_Hdr.dShipDate)

      IF !EMPTY(Pack_Hdr.PIKTKT) AND SEEK(PACK_HDR.Pack_No,'Pack_Lin') 
        IF SEEK(Pack_Hdr.Order + Pack_Hdr.PIKTKT,'PIKTKT') 
          SELECT PIKTKT
          SCAN REST WHILE ORDER+PIKTKT= Pack_Hdr.Order + Pack_Hdr.PIKTKT FOR !(PIKTKT.STATUS $ 'XC')
            lnRecNo = RECNO('PACK_HDR')
            *--If there is more one piktkt for the same order so with the first one the order's
            *--record will be locked , so I make it unlock to can handele it with the following piktkts
            IF ORDHDR.ORDER == lcCurOrder
              = lfUnLock()
            ELSE
              lcCurOrder = ORDHDR.ORDER
            ENDIF
            =lfGetOrder(OrdHdr.Order,OrdHdr.Store,.T.,PACK_HDR.PIKTKT)
            IF BETWEEN(lnRecNo,1,RECCOUNT('PACK_HDR'))
              GOTO lnRecNo IN PACK_HDR
            ENDIF
            SELECT(lcNeeded)
            IF !SEEK(PACK_HDR.ORDER+PACK_HDR.PIKTKT)
              APPEND BLANK
              REPLACE ORDER  WITH PACK_HDR.ORDER,PIKTKT WITH PACK_HDR.PIKTKT
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

IF EOF(lcInvHdr)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No order falls in the selected range')
  RETURN(.F.)
ENDIF
SELECT(lcTrgCursr)
REPLACE DISPSCR WITH .T.  && to do the Trigger that disable screen lcWinch1


*-- END OF FUNCTION lfUnConsOk
*!**************************************************************************
*! Name      : lfAddMPad
*! Developer : NADER NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Add a new option titled 'Generate Consolidated Invoices' on the Options menu
*!             with the Invoice Sales Order
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfAddMPad

*--check if the option pad is already defined on the sysmenu
PRIVATE llFound
STORE .F. TO llFound
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ELSE
  *-- Count Options BARS to add the new BAR at the end of the Popup
  lnBarNo = CNTBAR('_INQURYPOP') + 1

  *-- Define 2 New options (Generate Consolidated Invoices) and 
  *-- (Generate Invoices from Ship Date)
  DEFINE BAR lnBarNo OF _INQURYPOP PROMPT '\<Generate Consolidated Invoices' ;
         SKIP FOR !EMPTY(laData[1])
  ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfDispScr IN DIRMAIN WITH .T.

  DEFINE BAR lnBarNo+1 OF _INQURYPOP PROMPT 'Generate In\<voices from Ship Date' ;
         SKIP FOR !EMPTY(laData[1])
  ON SELECTION BAR lnBarNo+1 OF _INQURYPOP DO lfDispScr IN DIRMAIN WITH .F.

ENDIF
RETURN

*-- END OF FUNCTION lfAddMPad.

*!*************************************************************
*! Name      : lfOldValue
*! Developer : NADER NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfOldvalue
lcOldValue = EVALUATE(SYS(18))
*-- End Of lfoldvalue

*!*******************************************************************************
*! Name      : lfvCustPo
*! Developer : NADER NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Validate Customer po Number
*!*******************************************************************************
*! Calls     : gfModalGen
*!*******************************************************************************
*! Parameters: None
*!*******************************************************************************
*! Returns   :  None.
*!*******************************************************************************
*! Example   :  =lfvCustPo()
*!*******************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfvCustPo
PRIVATE lnOldAlS , lcOldOrder , lcCustPoNo
STORE 0 TO lnOldAlS
STORE '' TO lcOldOrder , lcCustPoNo
lnOldAlS = SELECT(0)

lcTrgCursr = lcInvLine + 'A'

lcTmpCuPo = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpCuPo) (CUSTPO C(15))
INDEX ON (CUSTPO) TAG (lcTmpCuPo)

IF !USED('PACK_HDR')
  =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
ENDIF
SELECT ORDHDR
lcOldOrder = SET('ORDER')
SET ORDER TO TAG ORDCUST IN ORDHDR
IF SEEK(lcAccount)
  SCAN REST WHILE account+UPPER(custpo)+cordtype+order = lcAccount FOR !EMPTY(CUSTPO)
    IF SEEK(ORDHDR.Order ,'PACK_HDR') AND ;
       BETWEEN(Pack_Hdr.dShipDate,&lcTrgCursr..FDate,&lcTrgCursr..ToDate)
      lcCustPoNo = ORDHDR.CustPo
      SELECT (lcTmpCuPo)
      IF !SEEK(lcCustPoNo)
        APPEND BLANK
        REPLACE CUSTPO WITH lcCustPoNo
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
lcBrFields = [CUSTPO :H="Customer PO" :25]
SELECT (lcTmpCuPo)
LOCATE
IF llBrowse OR '?' $ lcCustPO 
  llBrowse = .F.
  = ARIABROW('','Edit Average Sales/Week',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'','')
  lcCustPO = ALLTRIM(EVAL(LCTMPCUPO + '.CUSTPO'))
ENDIF  
IF !EMPTY(lcCustPO) AND !SEEK(ALLTRIM(lcCustPO),lcTmpCuPo)
 IF gfModalGen("QRM00000B40000",.F.,.F.,.F.,'This PO does not exist . Add to all records ?' ) = 2
    _CUROBJ = OBJNUM(lcCustPO)
   REPLACE &lcTrgCursr..CUSTPO WITH ''
 ELSE
   SELECT (lcTrgCursr)
   REPLACE &lcTrgCursr..CUSTPO WITH ALLTRIM(lcCustPO) ,;
           &lcTrgCursr..llNCustPo WITH .T.
 ENDIF
ELSE
  SELECT (lcTrgCursr)
  REPLACE &lcTrgCursr..CUSTPO WITH ALLTRIM(lcCustPO)
ENDIF  
SELECT ORDHDR
SET ORDER TO &lcOldOrder
SELECT(lnOldAlS)

*-- End of lfvCustPo.
*!*******************************************************************************
*! Name      : lfUpdPo
*! Developer : NADER NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : update the Entered Po number to all order and pick tickets records
*!           : for only if the ordhdr.Custpo is blank. for the customer selected
*!*******************************************************************************
*! Calls     : 
*!*******************************************************************************
*! Example   :  =lfUpdPo()
*!*******************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfUpdPo

lcTrgCursr = lcInvLine + 'A'
SELECT ORDHDR
IF SEEK(&lcTrgCursr..ACCOUNT)
  SCAN REST WHILE account+UPPER(custpo)+cordtype+order = &lcTrgCursr..Account ;
       FOR EMPTY(CUSTPO)
    IF SEEK(ORDHDR.Order ,'PACK_HDR')
      REPLACE ORDHDR.CUSTPO WITH &lcTrgCursr..CUSTPO
    ENDIF
  ENDSCAN
ENDIF
RETURN
*-- End of lfUpdPo.
*!*******************************************************************************
*! Name      : lfRefWind   
*! Developer : NADER NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Refresh window lcWinCh1 (Oredr Field) to make it Disable
*!*******************************************************************************
*! Calls     : from Activate window (lcWinCh1)
*!*******************************************************************************
*! Example   :  =lfRefWind()
*!*******************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfRefWind   
PRIVATE lnOldAlS 
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)
lcTrgCursr = lcInvLine + 'A'
IF USED(lcTrgCursr)
  SELECT(lcTrgCursr)
  IF &lcTrgCursr..DISPSCR  && if the custom screen displayed. then we can disable lcWinCh1
    REPLACE &lcTrgCursr..DISPSCR WITH .F.
    STORE .F. TO laScrMode
    STORE .T. TO laScrMode[4]
    GO TOP IN (lcInvHdr)
    SHOW GETS
  ENDIF
ENDIF
SELECT(lnOldAlS)
*--End of Function lfRefWind.

*!*******************************************************************************
*! Name      : lfUnLock
*! Developer : NADER NABIL (NNA)
*! Date      : 11/23/2004
*! Purpose   : Unlock the Ordhdr Record to use it again if there are more one piktkt
*!           : for the same Order.
*!*******************************************************************************
*! Calls     : from Dirmain.prg
*!*******************************************************************************
*! Example   :  =lfUnLock()
*!*******************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
FUNCTION lfUnLock
PRIVATE lnOldAlS 
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)
SELECT OrdHdr
=RLOCK()
REPLACE lLok_Stat WITH .F. ,;
        cLok_User WITH ''  ,;
        dLok_Date WITH {}  ,;
        cLok_Time WITH ''
UNLOCK        
SELECT(lnOldAlS)

*--End of Function lfUnLock.

*!*************************************************************
*! Name      : lfGetOrder
*! Developer : Nader NABIL (NNA)
*! Date      : 09/02/2005
*! Purpose   : This Function copied from Ariinv Program and I made
*!             some Changes in it to be suitable to Get my order 
*!             information
*!*************************************************************
*! Calls     : gfRltFld,gfChkRate,gfModalGen
*!*************************************************************
*! Parameters: lcOrderNo  : Order Number
*!             lcStoreNo  : Store
*!             llPicked   : Picked lines only
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfGetOrder()
*!*************************************************************
FUNCTION lfGetOrder
PARAMETERS lcOrderNo,lcStoreNo,llPicked ,lcPikTkt
PRIVATE ldDueDate,lcStore,lcCustLink,lcCustSales,lcSaleRep1,lcSaleRep2,;
        lcCurrCode,lnExRate,lnCurrUnit,llPacked,lcLinesCond,lnTaxQty,;
        lnTaxRate,llBackOrd

PRIVATE lnAllCartn
lnAllCartn = 0
PRIVATE lnSyUserRec , lcLok_User
IF OrdHdr.lLok_Stat
  *-- Display the message "Record is in use by user AAAA"
  lnSyUserRec   = IIF(RECNO('SyUUser')>RECCOUNT('SyUUser'),0,RECNO('SyUUser'))
  lcLok_User = ALLTRIM(PROPER(LOOKUP(SyUUser.cUsr_name,OrdHdr.cLok_User,SyUUser.cUser_id,'cUser_id')))
  lcLok_User = IIF(EMPTY(lcLok_User),gcUser_ID,lcLok_User)
  IF lnSyUserRec > 0
    GO lnSyUserRec IN SyUUser
  ENDIF  
          
  *-- Record is in use by user ????    
  IF gfModalGen("INM40182B00000","ALERT",'Order #' + OrdHdr.Order + '|' + lcLok_User) = 1
    RETURN(.F.)
  ENDIF
ENDIF
IF OrdHdr.Status='C'
  *--Message : 40109
  *--Order# 999999 has been shipped complete.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40109B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Status = 'X'
  *--Message : 40110
  *--Order# 999999 has been cancelled! Cannot ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40110B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Status = 'B'
  *--Message : 40155
  *--Order# 999999 is Bid. Cannot ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40155B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF !SEEK('M'+OrdHdr.Account,'Customer')
  *--Message : 40112
  *--Account XXXXX not found. Cannot Ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40112B00000','ALERT','Account '+OrdHdr.Account)
  RETURN(.F.)
ENDIF
IF Customer.Status <> 'A'
  *--Message : 40113
  *--Account XXXXX is Non-Active. Invoicing is not allowed.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40113B00000','ALERT','Account '+OrdHdr.Account)
  RETURN(.F.)
ENDIF
IF !EMPTY(lcStoreNo) .AND. !SEEK('S'+OrdHdr.Account+lcStoreNo,'Customer')
  *--Message : 40112
  *--Store XXXXX not found. Cannot Ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40112B00000','ALERT','Store '+ALLTRIM(lcStoreNo))
  RETURN(.F.)
ENDIF
IF Customer.Status <> 'A'
  *--Message : 40113
  *--Store XXXXX is Non-Active. Invoicing is not allowed.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40113B00000','ALERT','Store '+ALLTRIM(lcStoreNo))
  RETURN(.F.)
ENDIF
IF !SEEK('O'+lcOrderNo,'ordline')
  *--Message : 40114
  *--The lines for order# 999999 were not found.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40114B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Bulk='Y' AND laSetups[21,2]='N'
  *--Message : 40151
  *--Order# 999999 is a Bulk order. Cannot Ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40151B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Status = 'H' 
  IF laSetups[22,2]='Y'
    *--Message : 40111
    *--Order# 999999 is On Hold.
    *--Button : 40003
    *--Proceed Cancel
    IF gfModalGen('QRM40111B40003','ALERT',lcOrderNo+'| ')=2
      RETURN(.F.)
    ENDIF
  ELSE
    *--Message : 40111
    *--Order# 999999 is On Hold. Cannot Ship.
    *--Button : 00000
    *--Ok
    =gfModalGen('TRM40111B00000','ALERT',lcOrderNo+'| Cannot Ship.')
    RETURN(.F.)
  ENDIF
ENDIF
IF !OrdHdr.lLok_Stat
  PRIVATE lcCurAlias
  lcCurAlias = ALIAS()
  SELECT OrdHdr
  =RLOCK()
  REPLACE lLok_Stat WITH .T.         ,;
          cLok_User WITH gcUser_ID   ,;
          dLok_Date WITH DATE()      ,;
          cLok_Time WITH gfGetTime()
  UNLOCK
  IF !EMPTY(lcCurAlias)
    SELECT (lcCurAlias)
  ENDIF  
ENDIF
IF ldDefInvDate < ORDHDR.START
  *--Message : 40116
  *--Order# 999999 start date is not until 99/99/9999.
  *--Button : 00000
  *--Ok
  =gfModalGen('INM40116B00000','ALERT',lcOrderNo+'|'+DTOC(ORDHDR.START))
ENDIF
IF 'AL' $ gcCmpModules
  IF !USED('ORDERPCK')
    =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
  ENDIF
  IF !USED('PACK_LIN')
    =gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
  ENDIF
ENDIF
*-- if the function was not called from the scope screen
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
ENDIF
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
ENDIF  
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
SELECT (lcInvLine)
SET RELATION TO STYLE INTO STYLE
SET RELATION TO Style+cWareCode+Dyelot INTO STYDYE ADDITIVE
IF llPicked
  *-- Pick ticket filter   
  lcLinesCond = "Picked .AND. TotPik > 0 AND PIKTKT = lcPiktkt"
ENDIF
*-- get the Term code related fields               
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')
lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
IF llIsEngland
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  CTOD('01'+SUBSTR(DTOC(GOMONTH(ldDefInvDate,1)),3))-1+lnTDaysDue)
ELSE                
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(ldDefInvDate),1,3)+'10'+;
                  SUBSTR(DTOC(ldDefInvDate),6,5)),IIF(DAY(ldDefInvDate) > lnEOMDay,2,1))+lnTDaysDue)
ENDIF
lcCurrCode= IIF(EMPTY(OrdHdr.cCurrCode),gcBaseCurr,OrdHdr.cCurrCode)
STORE 1 TO lnExRate, lnCurrUnit
IF lcCurrCode <> gcBaseCurr
  lnExRate=gfChkRate('lnCurrUnit',lcCurrCode,ldDefInvDate,.T.,.F.,.F.,llEditExRt)
  IF lnExRate = 0
    IF llEditExRt
      *--Message : 00262
      *--A valid xxx to xxx exchange rate could not be found on xxx.
      *--Button : 00000
      *--Ok
      =gfModalGen('INM00262B00000','ALERT',ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldDefInvDate))
    ELSE
      lcCurrCode = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    ENDIF
  ENDIF
ENDIF
=SEEK('M'+OrdHdr.Account,'Customer')
IF EMPTY(OrdHdr.Store)  
  lcPhone=Customer.Phone1
ENDIF  
llBackOrd = INLIST(IIF(EMPTY(Customer.cBackOrd),laSetups[23,2],Customer.cBackOrd),'A','I')
SELECT OrdLine
SET ORDER TO TAG Ordlinst
=SEEK('O'+lcOrderNo+ALLTRIM(lcStoreNo))
DO WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrderNo+ALLTRIM(lcStoreNo)
  lcStore = Store
  =IIF(EMPTY(Store),SEEK('M'+Account,'Customer'),SEEK('S'+Account+Store,'Customer'))
  lcSaleRep1 = Ordhdr.Rep1
  lnComm1    = OrdHdr.Comm1
  lcSaleRep2 = Ordhdr.Rep2
  lnComm2    = OrdHdr.Comm2
  IF OrdHdr.Multi='Y' AND EMPTY(OrdHdr.Rep1) AND EMPTY(OrdHDr.Rep2)
    lcSaleRep1 = Customer.SalesRep
    lnComm1    = Customer.Comm
    lcSaleRep2 = Customer.Rep2
    lnComm2    = Customer.Comm2
  ENDIF
  *-- Scan only for the Piktkt that Passed From Function lfCons of lfUnCons
  SCAN REST FOR   EVAL(lcLinesCond) ;
            WHILE cOrdType+Order+Store+Style+STR(LineNo,6)='O'+lcOrderNo+lcStore
    llPacked = .F.
    IF 'AL' $ gcCmpModules AND ;
      SEEK(OrdLine.Order+OrdLine.Store+OrdLine.PikTkt,'Pack_Hdr') AND ;
      SEEK(PACK_HDR.Pack_No,'Pack_Lin')
      IF !SEEK(OrdLine.PikTkt+OrdLine.Order,lcPackLine)
        =lfPackLin(OrdLine.Order,OrdLine.PikTkt)
      ENDIF
      llPacked = SEEK(OrdLine.PikTkt+OrdLine.Order+STR(OrdLine.LineNo,6),lcPackLine)
      SELECT OrdLine
    ENDIF
    SCATTER MEMVAR MEMO
    m.Gros_Price = IIF(m.Gros_Price=0,m.Price,m.Gros_Price)
    =SEEK(m.Style,'Style')
    STORE 0 TO lnTaxQty,lnTaxRate
    IF llIsEngland .AND. laSetups[9,2]='Y' .AND. ;
       !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
      =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE') 
     ENDIF
    m.lTaxable = Style.lTaxable
    IF llPacked
      m.Pik1 = &lcPackLine..Qty1
      m.Pik2 = &lcPackLine..Qty2
      m.Pik3 = &lcPackLine..Qty3
      m.Pik4 = &lcPackLine..Qty4
      m.Pik5 = &lcPackLine..Qty5
      m.Pik6 = &lcPackLine..Qty6
      m.Pik7 = &lcPackLine..Qty7
      m.Pik8 = &lcPackLine..Qty8
      m.TotPik = &lcPackLine..TotQty

      IF !EMPTY(m.Prepak)  
        =SEEK('P'+m.Scale+m.Prepak,'Scale')
        IF Scale.pp1/Scale.PPTot*m.TotPik <> m.Pik1 OR ;
           Scale.pp2/Scale.PPTot*m.TotPik <> m.Pik2 OR ;
           Scale.pp3/Scale.PPTot*m.TotPik <> m.Pik3 OR ;
           Scale.pp4/Scale.PPTot*m.TotPik <> m.Pik4 OR ;
           Scale.pp5/Scale.PPTot*m.TotPik <> m.Pik5 OR ;
           Scale.pp6/Scale.PPTot*m.TotPik <> m.Pik6 OR ;
           Scale.pp7/Scale.PPTot*m.TotPik <> m.Pik7 OR ;
           Scale.pp8/Scale.PPTot*m.TotPik <> m.Pik8
          m.Prepak = ''
          m.PPQty  = 0
        ELSE
          m.PPQty = IIF(Scale.PPTot=0,0,m.TotPik /Scale.PPTot)
        ENDIF
        =SEEK('S'+m.Scale,'Scale')
      ENDIF
    ENDIF
    INSERT INTO (lcInvLine);
    (Order,Account,LineNo,Store,PikTkt,Style,Dyelot,Note_Mem,Comm1,Comm2,Book1,Book2,;
     Book3,Book4,Book5,Book6,Book7,Book8,TotBook,Flag,Pik1,Pik2,Pik3,Pik4,Pik5,;
     Pik6,Pik7,Pik8,TotPik,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
     Price,Pack_Id,Gros_Price,Disc_Pcnt,lPacked,lBackOrd,nTaxRate,Group,Prepak,;
     Desc1,Season,PPQty,Scale,cWareCode,Consol,cDivision,cCurrCode,lTaxable,cDyeFlag,cwarecode,;
     Gl_Sales);
     VALUES (m.Order,m.Account,m.LineNo,m.Store,m.PikTkt,m.Style,m.Dyelot,;
     m.Note_Mem,m.Comm1,m.Comm2,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
     m.Qty7,m.Qty8,m.TotQty,IIF(llPicked,'B',' '),m.Pik1,m.Pik2,m.Pik3,m.Pik4,;
     m.Pik5,m.Pik6,m.Pik7,m.Pik8,m.TotPik,IIF(llPicked,m.Pik1,m.Qty1),;
     IIF(llPicked,m.Pik2,m.Qty2),IIF(llPicked,m.Pik3,m.Qty3),IIF(llPicked,m.Pik4,m.Qty4),;
     IIF(llPicked,m.Pik5,m.Qty5),IIF(llPicked,m.Pik6,m.Qty6),IIF(llPicked,m.Pik7,m.Qty7),;
     IIF(llPicked,m.Pik8,m.Qty8),IIF(llPicked,m.TotPik,m.TotQty),m.Price,m.Pack_Id,;
     m.Gros_Price,m.Disc_Pcnt,llPacked,llBackOrd,lnTaxRate,m.Group,m.Prepak,;
     m.Desc1,m.Season,m.PPQty,m.Scale,m.cWareCode,'N',OrdHdr.cDivision,OrdHdr.cCurrCode,;
     m.lTaxable,Style.cDye_Flg,m.cwarecode,OrdLine.Gl_Sales)


    SELECT (lcInvLine)
    IF INLIST(laSetups[7,2],'F','L') 
      *-- Funtion to Update The Stk Qty.
      = lfUpStkQty ()
    ENDIF
    =SEEK(m.Style,'Style')
    IF llIsEngland .AND. laSetups[9,2]='Y' .AND. ;
      !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
      FOR lnCount = Style.nTaxBreak TO 8
        lnTaxQty = lnTaxQty + EVAL(lcInvLine+'.Qty'+STR(lnCount,1))
      ENDFOR
    ENDIF
    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    SELECT (lcInvHdr)
    llInvPck = !EMPTY(m.PikTkt) AND SEEK(m.Order+m.Store+m.PikTkt,'Pack_Hdr')
   
    IF !SEEK(m.Account+m.Order+m.Store+m.PikTkt)
      APPEND BLANK
      REPLACE cSelect    WITH '»' ,;
              Order      WITH m.Order ,;
              Store      WITH m.Store ,;
              PikTkt     WITH m.PikTkt ,;
              CustPo     WITH IIF(EMPTY(m.CustPo),OrdHdr.CustPo,m.CustPo),;
              Dist_Ctr   WITH Customer.Dist_Ctr ,;
              Account    WITH OrdHdr.Account ,;
              cTermCode  WITH OrdHdr.cTermCode  ,;
              SpcInst    WITH OrdHdr.SpcInst ,;
              lUpsIns    WITH (OrdHdr.CINSUR='Y') ,;
              Rep1       WITH lcSaleRep1 ,;
              Comm1      WITH lnComm1    ,;
              Rep2       WITH lcSaleRep2 ,;
              Comm2      WITH lnComm2    ,;
              Note1      WITH OrdHdr.Note1 ,;
              Note2      WITH OrdHdr.Note2 ,;
              lCompUps   WITH .T. ,;
              Consol     WITH 'N'
                    
      REPLACE LastLine   WITH OrdHdr.LastLine   
      IF llInvPck AND !EMPTY(Pack_Hdr.Ship_Date)
        IF llIsEngland
          ldDueDate = IIF(lcTEOM <> 'Y',Pack_Hdr.Ship_Date+lnTDaysDue,;
                          CTOD('01'+SUBSTR(DTOC(GOMONTH(Pack_Hdr.Ship_Date,1)),3))-1+lnTDaysDue)
        ELSE                
          ldDueDate = IIF(lcTEOM <> 'Y',Pack_Hdr.Ship_Date+lnTDaysDue,;
                          GOMONTH(CTOD(SUBSTR(DTOC(Pack_Hdr.Ship_Date),1,3)+'10'+;
                          SUBSTR(DTOC(Pack_Hdr.Ship_Date),6,5)),IIF(DAY(Pack_Hdr.Ship_Date) > lnEOMDay,2,1))+lnTDaysDue)
        ENDIF      
      ENDIF
      REPLACE DiscPcnt   WITH OrdHdr.Disc                                       ,;
              InvDate    WITH ldDefInvDate                                      ,;
              ShipDate   WITH IIF(llInvPck AND !EMPTY(Pack_Hdr.Ship_Date)        ;
                              ,Pack_Hdr.Ship_Date,ldDefInvDate)   ,;
              dPostDate  WITH ldDefPstDate                                      ,;
              DueDate    WITH ldDueDate                                         ,;
              Dept       WITH OrdHdr.Dept                                       ,; 
              cFacCode   WITH OrdHdr.cFacCode                                   ,;
              Approval   WITH OrdHdr.Approval                                   ,;
              Appramt    WITH OrdHdr.ApprAmt                                    ,;
              Season     WITH OrdHdr.Season                                     ,;
              cDivision  WITH OrdHdr.cDivision                                  ,;
              UpsZone    WITH Customer.UpsZone                                  ,;
              Phone      WITH IIF(EMPTY(Ordhdr.Store),lcPhone,Customer.Phone1)  ,;
              cWareCode  WITH IIF(llPicked .AND. SEEK(m.Order+m.PikTkt,'PikTkt'),;
                                  PikTkt.cWareCode,OrdHdr.cWareCode)            ,;
              Trde_Disc  WITH lnTerDiscR                                        ,;
              Tax_Rate   WITH IIF(laSetups[9,2] <> 'Y',0,                        ;
                              IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) ,;
              nPstRate   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                               Customer.nTaxRate,0) ,;
              cTaxRule   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                              Customer.cTaxRule,'') ,;
              Cod_Flag   WITH IIF(lcTCod='Y','Y','N'),;
              Status     WITH OrdHdr.Status ,;
              cCurrCode  WITH lcCurrCode ,;
              nExRate    WITH lnExRate   ,;
              nCurrUnit  WITH lnCurrUnit ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()     ,;
              cAdd_User  WITH gcUser_id
      REPLACE nHstRate   WITH IIF(laSetups[9,2] <> 'Y' OR !llIsCanada,0,laSetups[26,2])
      IF llInvPck
        REPLACE Weight    WITH Pack_Hdr.Tot_Wght ,;
                nCartons  WITH Pack_Hdr.Tot_Cart ,;
                Cartons   WITH Pack_Hdr.Tot_Cart
        REPLACE BOL_No WITH     Pack_Hdr.Bill_ladg    
      
      ENDIF
    ENDIF
    lnCartons  = nCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
    REPLACE Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
            Ship      WITH Ship     + m.TotQty ,;
            ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
            Discount  WITH -ShipAmt * DiscPcnt/100,;
            Weight    WITH Weight   + IIF(llInvPck,0,m.TotQty*Style.nStyWeight) ,;
            nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
            Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
            Picked    WITH Picked  + m.TotPik ,;
            ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                           IIF(llInvPck,IIF(ALLTRIM(Pack_Hdr.ShipVia)='*',Customer.ShipVia,Pack_Hdr.ShipVia),IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia))),;
            nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
            Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
            TotalChg  WITH ShipAmt+Tax_Amt+Discount,;
            nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
    IF llIsEngland
      REPLACE NTrueShip 	WITH NTrueShip + (m.TotQty*m.Price) ,;
              NTrueDscnt	WITH -NTrueShip * DiscPcnt/100 		,;
              NTrueChrg     WITH NTrueShip + nMerchTax + NTrueDscnt
    ENDIF
    IF llRevue
      REPLACE Complete WITH OrdHdr.Complete ,;
              ShipDesc WITH gfCodDes(ShipVia,'SHIPVIA')
    ENDIF          
  ENDSCAN
ENDDO

RETURN

*-- End of Function lfGetOrder.
*!*************************************************************
*! Name      : lfPackLinv
*! Developer : Nader NABIL (NNA)
*! Date      : 09/02/2005
*! Purpose   : Compute order packed quantity
*! Note      : this Function Copied from the Ariinv Program
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcOrder  : Order#
*!                       lcPackNO : Pack#
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPackLin(lcOrder,lcPAckNO)
*!*************************************************************
FUNCTION lfPackLin
PARAMETERS lcOrder,lcPackNO
WAIT 'Computing packed quantity...' WINDOW NOWAIT
SELECT PACK_LIN
=SEEK(lcPackNO)
SCAN REST WHILE pack_no+STR(line_no,3)+style+cpackcolor = lcPackNO
  IF !SEEK(lcPackNO+lcOrder+STR(nordlineno,6),lcPackLine)
    INSERT INTO (lcPackLine) (Order,Pack_No,nOrdLineNo) VALUES ;
    (lcOrder,lcPackNO,Pack_Lin.nordlineno)
  ENDIF
  SELECT (lcPackLine)
  REPLACE Qty1 WITH Qty1 + Pack_Lin.Qty1 ,;
          Qty2 WITH Qty2 + Pack_Lin.Qty2 ,;
          Qty3 WITH Qty3 + Pack_Lin.Qty3 ,;
          Qty4 WITH Qty4 + Pack_Lin.Qty4 ,;
          Qty5 WITH Qty5 + Pack_Lin.Qty5 ,;
          Qty6 WITH Qty6 + Pack_Lin.Qty6 ,;
          Qty7 WITH Qty7 + Pack_Lin.Qty7 ,;
          Qty8 WITH Qty8 + Pack_Lin.Qty8 ,;
          TotQty WITH TotQty + Pack_Lin.TotQty
ENDSCAN
WAIT CLEAR
*-- End Of lfPackLin
*!*************************************************************
*! Name      : lfUpStkQty
*! Developer : Nader NABIL (NNA)
*! Date      : 09/02/2005
*! Purpose   : Function to get the correct stk Qty for current style 
*!             if the style exist more than one time.
*! Note      : this Function Copied from the Ariinv Program
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
FUNCTION lfUpStkQty
PRIVATE lnPrvAlias
lnPrvAlias = SELECT (0)
*-- Check if enter at temp file before.
IF !SEEK(m.Style+m.cWareCode+m.Dyelot,lcTempStk) .AND. SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
    SELECT (lcTempStk)
    APPEND BLANK
    REPLACE Qty1      WITH StyDye.Stk1 ,;
            Qty2      WITH StyDye.Stk2 ,;
            Qty3      WITH StyDye.Stk3 ,;
            Qty4      WITH StyDye.Stk4 ,;
            Qty5      WITH StyDye.Stk5 ,;
            Qty6      WITH StyDye.Stk6 ,;
            Qty7      WITH StyDye.Stk7 ,;
            Qty8      WITH StyDye.Stk8 ,;
            Style     WITH M.Style     ,;
            cWareCode WITH M.cWareCode ,;
            Dyelot    WITH M.Dyelot    ,;
            TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
ENDIF
SELECT (lcInvLine)
IF SEEK(m.Style+m.cWareCode+m.Dyelot,lcTempStk)
  REPLACE Qty1 WITH MAX(MIN(Qty1,&lcTempStk..Qty1),0) ,;
          Qty2 WITH MAX(MIN(Qty2,&lcTempStk..Qty2),0) ,;
          Qty3 WITH MAX(MIN(Qty3,&lcTempStk..Qty3),0) ,;
          Qty4 WITH MAX(MIN(Qty4,&lcTempStk..Qty4),0) ,;
          Qty5 WITH MAX(MIN(Qty5,&lcTempStk..Qty5),0) ,;
          Qty6 WITH MAX(MIN(Qty6,&lcTempStk..Qty6),0) ,;
          Qty7 WITH MAX(MIN(Qty7,&lcTempStk..Qty7),0) ,;
          Qty8 WITH MAX(MIN(Qty8,&lcTempStk..Qty8),0) ,;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  SELECT (lcTempStk)
  REPLACE Qty1      WITH MAX(Qty1 - &lcInvLine..Qty1,0) ,;
          Qty2      WITH MAX(Qty2 - &lcInvLine..Qty2,0) ,;
          Qty3      WITH MAX(Qty3 - &lcInvLine..Qty3,0) ,;
          Qty4      WITH MAX(Qty4 - &lcInvLine..Qty4,0) ,;
          Qty5      WITH MAX(Qty5 - &lcInvLine..Qty5,0) ,;
          Qty6      WITH MAX(Qty6 - &lcInvLine..Qty6,0) ,;
          Qty7      WITH MAX(Qty7 - &lcInvLine..Qty7,0) ,;
          Qty8      WITH MAX(Qty8 - &lcInvLine..Qty8,0)
ENDIF
SELECT(lnPrvAlias)
*-- End Of lfUpStkQty
*:**************************************************************************
*:* Name        : lfUpdDates
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on PERIOD field
*:***************************************************************************
*C123847,1
FUNCTION lfUpdDates
PARAMETERS ldStart,lnPeriod,ldEnd
PRIVATE lnMnth,lnYr

ldEnd = {}
IF lnPeriod > 0
  ldEnd = ldStart-1
  lnYr = YEAR(ldStart)
  lnMnth = MONTH(ldStart)
  FOR lnI = 0 TO lnPeriod-1
    ldEnd = ldEnd + lfMnthDays(lnMnth+lnI,lnYr)
    IF lnMnth+lnI = 12
      lnMnth =  - lnI
      lnYr = lnYr + 1
    ENDIF
  ENDFOR
ENDIF

SHOW GET ldStart
SHOW GET lnPeriod
SHOW GET ldEnd

=lfRefresh()  
*-- end of lfUpdDates.

*:**************************************************************************
*:* Name        : lfMnthDays
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/12/2004
*:* Purpose     : return month days count
*:***************************************************************************
*C123847,1
FUNCTION lfMnthDays
PARAMETERS lnMnth,lnYr
PRIVATE lnDays

DO CASE
CASE INLIST(lnMnth,1,3,5,7,8,10,12)
  lnDays = 31
CASE INLIST(lnMnth,4,6,9,11)
  lnDays = 30
CASE lnMnth = 2
  lnDays = 29-IIF(MOD(lnYr,4)>0,1,0)
ENDCASE

RETURN lnDays
*-- end of lfMnthDays.

*:**************************************************************************
*:* Name        : lfENTLORD   
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2004
*:* Purpose     : Define 'Entitlement Order' bar in the option pad in sales order screen
*:***************************************************************************
*:* Called from : SOORD.PRG
*:***************************************************************************
*C123847,1    &&<<<
FUNCTION lfENTLORD
PRIVATE lnBarNo
lnBarNo = CNTBAR('_INQURYPOP') + 1
DEFINE BAR lnBarNo   OF _INQURYPOP PROMPT '\<View Order Charges' ;
       SKIP FOR laScrMode[1] .OR. laScrMode[2] .OR. laScrMode[4] .OR. (laScrMode[3] .AND. ORDHDR.STATUS='C')
       
DEFINE BAR lnBarNo+1 OF _INQURYPOP PROMPT 'En\<titlement Order' ;
       SKIP FOR laScrMode[1] .OR. laScrMode[2] .OR. lnActFolder<>2
       
DEFINE BAR lnBarNo+2 OF _INQURYPOP PROMPT '\<Add New Employee' ;
       SKIP FOR laScrMode[1] .OR. laScrMode[2] .OR. lnActFolder<>2
       
ON SELECTION BAR lnBarNo   OF _INQURYPOP DO lfChrgOrd WITH .T. IN DIRMAIN.PRG
ON SELECTION BAR lnBarNo+1 OF _INQURYPOP DO lfEntitlm WITH laData[2],laData[3] IN DIRMAIN.PRG
ON SELECTION BAR lnBarNo+2 OF _INQURYPOP DO lfAddEmpl IN DIRMAIN.PRG
*-- end of lfENTLORD.

*:**************************************************************************
*:* Name        : lfStandard
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/18/2004
*:* Purpose     : Check if this order is standard or entered via SOENORD custom screen
*                 if EMPLOYEE field is empty , it is standard
*:***************************************************************************
*C123847,1
FUNCTION lfStandard
PRIVATE lnSlct, lnRecno, llStandard
llStandard = .F.
lnSlct = SELECT()

SELECT &lcOrdLine
lnRecno = RECNO()

GO TOP
IF !EOF()
  llStandard = EMPTY(EMPLOYEE)

  IF BETWEEN(lnRecno,1,RECCOUNT())
    GOTO (lnRecno)
  ENDIF
  
ENDIF
  
SELECT (lnSlct)

RETURN llStandard
*-- end of lfStandard.
*:**************************************************************************
*:* Name        : lfEntitlm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/14/2004
*:* Purpose     : Entitlement order screen
*:***************************************************************************
*C123847,1
FUNCTION lfEntitlm
PARAMETERS lcAccount,lcStore
PRIVATE laTemp,lcFieldsNam,lcBrFields,lcTmpEmpl,lcEmployee,llUpd,lcSto,;
        lcItemPct, lcSepart, lnClrLen, lnClrPos, lcClrSpr, lnExtSzWd,lnEmplsNo

*- lcEmployee  : Holds The selected employee code
*- llUpd       : if .T. indicate that some data entered, if .F.  then no changes occured
*- lcSto       : If the user does not entered a store and selected from the ariabrow an employee
*                from a certain store , save it in this variable to seek in EMPLOYEE file

*- if this order is entered as standard order, do not allow ent. screen to work
IF lfStandard()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Entitlemnt Screen not available.')
  RETURN
ENDIF

=gfOpenFile(gcDataDir+'UNIFORM' ,'UNIFORM' ,'SH')
=gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE','SH')
=gfOpenFile(gcDataDir+'STYHIST' ,'STYHIST' ,'SH')
=gfOpenFile(gcSysHome+'SYUUSER' ,'CUSER_ID','SH')
=gfOpenFile(gcDataDir+'BOM'     ,'BOM'     ,'SH')
=gfOpenFile(gcDataDir+'CSTPRICE','CSTPRICE','SH')


lcTmpEmpl = gfTempName()
SELECT ACCOUNT,STORE,EMPLOYEE,EMPNAME,UCODE ;
  FROM EMPLOYEE ;
  WHERE ACCOUNT = lcAccount .AND. STORE = lcStore ;
  INTO TABLE (gcWorkDir+lcTmpEmpl)
INDEX ON EMPLOYEE TAG &lcTmpEmpl
lnEmplsNo = RECCOUNT(lcTmpEmpl)

STORE ' ' TO lcSto, lcEmployee

*- if there are previously added employees for this account store  show them
IF RECCOUNT(lcTmpEmpl) > 0
  GO TOP
  DIMENSION laTemp[4]
  latemp = ''
  lcFieldsNam = 'STORE,EMPLOYEE'
  lcBrFields  = "EMPLOYEE:H='Employee Code' ,"+;
                "EMPNAME :H='Employee Name' ,"+;
                "UCODE   :H='Uniform Code'  ,"+;
                "STORE   :H='  Store  '     "
  PUSH KEY
  ON KEY
  PRIVATE lcFile_ttl,lcWinTitl
  =Ariabrow('','Employees',gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
               lcFieldsNam,'laTemp')
  POP KEY
  lcSto      = laTemp[1]
  lcEmployee = laTemp[2]
ENDIF

*- Remove the temp file
IF USED(lcTmpEmpl)
  USE IN &lcTmpEmpl
ENDIF
ERASE (gcWorkDir+lcTmpEmpl+'.DBF')
ERASE (gcWorkDir+lcTmpEmpl+'.CDX')

*- Open the sogtemp screen only if there is no employee to select 
IF lnEmplsNo = 0
  llNewEmp = .T.  && Show the "Select" Button
  lcEmployee = SPACE(12)
  DO (gcAppHome+'SO\SOGTEMPL') WITH lcAccount,lcStore
ENDIF

IF EMPTY(lcEmployee)
  RETURN
ENDIF

*- If the order entered date is greater than the Period end date on the employee, then the Period 
*- end date on the employee file should be updated to the next period end based on the Entitlement 
*- calculation period  

SELECT EMPLOYEE
=SEEK(lcAccount+lcSto+lcEmployee,'EMPLOYEE')
=SEEK('M'+lcAccount,'CUSTOMER')
IF CUSTOMER.LCHKENTLM
  *- If the order entered date is greater than the Period end date on the employee, then the Period end date on the employee file should be updated to the next period end based on the Entitlement calculation period
  IF laData[8] > EMPLOYEE.DEND 
    SELECT EMPLOYEE
    SCATTER FIELDS DSTART, NPERIOD, DEND MEMVAR
    m.DSTART = m.DEND+1
    =lfUpdDates(@m.DSTART,@m.NPERIOD,@m.DEND)
*    GATHER FIELDS DSTART, NPERIOD, DEND FROM MEMVAR
    GATHER FIELDS DSTART, NPERIOD, DEND MEMVAR
    
    IF SEEK(lcAccount+lcSto+lcEmployee,'STYHIST')
      SELECT STYHIST
      REPLACE NUSED WITH 0 REST ;
         WHILE ACCOUNT+STORE+EMPLOYEE+CSTYMAJOR+COLOUR = lcAccount+lcSto+lcEmployee
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(EMPLOYEE.UCODE)

  *- If the employee has a Uniform Code then the following screen will appear
  =lfEnOrd()

ELSE

  *- If there is no Uniform Code then you are taken straight into the Quick Order Entry Screen
  *- and when the order is saved the Employee Transaction History File is still updated
*  DO lfQkOrd IN DAVMAIN.FXP
  DO lfQkOrd IN (gcDef_Path+'DAVMAIN.FXP')
  
  *- Since quick order entry screen is opened via selecting an employee that has no uniform,then
  *- it is expected to add lines to lcOrdline file with new styles , so update all emply new fields with the 
  *- current employee id
  SELECT &lcOrdLine
  REPLACE EMPLOYEE WITH lcEmployee FOR EMPTY(EMPLOYEE)

ENDIF

*-- end of lfEntitlm.

*:**************************************************************************
*:* Name        : lfEnOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/16/2004
*:* Purpose     : Open entitlement screen
*:***************************************************************************
*C123847,1
FUNCTION lfEnOrd
PRIVATE lnGp,lnGps,laWobjects,lcTmpOTS
STORE 0 TO lnMrk,lnClrPos,lnClrLen, lnLastMode,lcGps,lnGp,lnGps

*- Group 'x' of 'y' , a group is 10 sizes
lcGps = '1/1'
lnGps = 1

*- Defa. Comp. Dt to Sold Dt [ option for David Luke ]
llCMP2SOLD = gfGetMemVar('M_CMP2SOLD')
lnExtSzWd  = gfGetMemVar('M_EXTWIDTH')
llUpd      = .F.  
lcItemPct  = gfItemMask('PI')
lcSepart   = SUBSTR(lcItemPct,lnMajorLen+1,1)

*- Get color start and lenght
STORE 0  TO lnClrLen, lnClrPos
STORE '' TO lcClrSpr
=lfGetClrD()

*- Get character related to style component setup
STORE ' ' TO lcIType, lcMType
=lfStyCmpTp()

*- Initialize state variables for the screen
PRIVATE     lcKeyStat, lcObjStat, lcWinCh0, lcWinCh1, lcWinCh2, lcOldValue, lcTempCur, ;
            lcBrTtl, lcSvRel
STORE '' TO lcKeyStat, lcObjStat, lcWinCh0, lcWinCh1, lcWinCh2, lcOldValue, lcTempCur
lcBrTtl    = 'List Styles of Uniform ' + ALLTRIM(EMPLOYEE.UCODE)

SELECT &lcOrdLine
lcSvRel = SET('RELATION')
SET RELATION TO

lcWinCh0   = gfTempName()
lcWinCh1   = gfTempName()
lcWinCh2   = gfTempName()
lcTempCur = '_'+SUBSTR(lcfolder,2)

*- Browse header variables
STORE ''  TO lc1,lc2,lc3,lc4,lc5,lc6,lc7,lc8,lc9,lc10
*- Create temp file for uniform assigned to the selected employee
=lfCrTmpFl()
=lfFillTemp()

IF RECCOUNT(lcTempCur) = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not open the Entitlement order screen.')
  RETURN
ENDIF  

*- Clear styhist relations
SELECT STYHIST
SET RELATION TO

*- Set relations
SELECT &lcTempCur
SET RELATION TO 
SET RELATION TO SUBSTR(CSTYMAJOR,1,lnMajorLen)              INTO STYLE 
SET RELATION TO EMPLOYEE.UCODE+CSTYMAJOR+COLOUR             INTO UNIFORM ADDITIVE
SET RELATION TO lcAccount+lcSto+m.Employee+CSTYMAJOR+COLOUR INTO STYHIST ADDITIVE
GO TOP

*-Initialize screen variables
SELECT EMPLOYEE
SCATTER MEMVAR

SELECT &lcTempCur
lcDesc    = STYLE.DESC
lcCSTYNOT = STYLE.CSTYNOT
lnEnt     = STYHIST.ENTITLEMNT
lnUsed    = STYHIST.NUSED     
lnBalance = lnEnt - lnUsed

*-Create a temp file when the screen SOENORD is activated
PRIVATE lcTmpChk
lcTmpChk = gcWorkDir+'#'+SUBSTR(lcFolder,2)+'.TMI'
COPY TO (lcTmpChk) SDF FOR .F.
GO TOP

PUSH KEY                                     && To save the the current on key label
ON KEY
ON KEY LABEL ALT+C ACTIVATE WINDOW (lcBrTtl)
DO (gcScrDir + gcWinAppl + '\SOENORD.SPR')   && calling the screen

POP KEY                                      && To Restore the previous assignments for on key label

*- Remove temp create files 
IF USED(lcTempCur)
  USE IN &lcTempCur
ENDIF
ERASE (gcWorkDir+lcTempCur+'.DBF')
ERASE (gcWorkDir+lcTempCur+'.CDX')

lcTmpOts = 'N'+SUBSTR(lcfolder,2)
IF USED(lcTmpOTS)
  USE IN &lcTmpOTS
ENDIF
ERASE (gcWorkDir+lcTmpOTS+'.DBF')
ERASE (gcWorkDir+lcTmpOTS+'.CDX')


SELECT &lcOrdLine
SET RELATION TO &lcSvRel
*-- end of lfEnOrd.

*:**************************************************************************
*:* Name        : lfvEmployee
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Select a new employee to Add entitlement order data
*:***************************************************************************
*C123847,1
FUNCTION lfvEmployee
PRIVATE lcModalMes,lcEmp,llContinue
llContinue = .F.

IF !llBrowse .AND. EMPTY(m.Employee)
  SELECT EMPLOYEE
  RETURN
ENDIF

IF CHRTRAN(m.Employee,'?/\','') <> m.Employee
  llBrowse = .T.
ENDIF

IF llBrowse
  =lfBrowEmp()
  IF !EMPTY(m.Employee)
    =SEEK(laData[2]+laData[3]+m.Employee,'EMPLOYEE')
    llContinue = .T.
  ENDIF
ELSE
  IF !EMPTY(m.Employee)
    IF SEEK(laData[2]+laData[3]+m.Employee,'EMPLOYEE')
      llContinue = .T.
    ELSE
      =lfBrowEmp()
      IF !EMPTY(m.Employee)
        =SEEK(laData[2]+laData[3]+m.Employee,'EMPLOYEE')
        llContinue = .T.
      ENDIF
    ENDIF  
  ENDIF
ENDIF  

IF EMPTY(EMPLOYEE.UCODE)
  llContinue = .F.
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No uniform code assigned to the employee :'+m.employee)
  m.employee = ' '
  SHOW GET m.employee
ENDIF

IF llContinue
  
  m.SITE_NO = EMPLOYEE.SITE_NO
  m.EMPNAME = EMPLOYEE.EMPNAME
  m.DSTART  = EMPLOYEE.DSTART     
  =lfFillTemp()
  IF RECCOUNT(lcTempCur) > 0
    SHOW GET m.Employee DISABLE
    SHOW GET pbEmployee DISABLE
  
    SHOW GET pbClr ENABLE
    SHOW GET pbOTS ENABLE

  ELSE
    _CUROBJ = OBJNUM(m.Employee)
  ENDIF

  *- Refreshments
  =lfRefNav()
  =lfActBr()
  =lfRefresh()

ENDIF


llBrowse = .F.


*:**************************************************************************
*:* Name        : lfBrowEmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Get employee from a browse
*:***************************************************************************
*C123847,1
FUNCTION lfBrowEmp
PRIVATE laTemp,lcFieldsNam,lcBrFields,lcTmpEmpl
lcTmpEmpl = gfTempName()
SELECT ACCOUNT,STORE,EMPLOYEE,EMPNAME ;
  FROM EMPLOYEE ;
  WHERE ACCOUNT = laData[2] .AND. STORE = laData[3] ;
  INTO TABLE (gcWorkDir+lcTmpEmpl)
INDEX ON ACCOUNT+STORE+EMPLOYEE TAG &lcTmpEmpl
GO TOP
DIMENSION laTemp[4]
latemp = ''
lcFieldsNam = 'ACCOUNT,STORE,EMPLOYEE,EMPNAME'
lcBrFields  = 'EMPLOYEE:H="Employee Code",'+;
              'EMPNAME :H="Employee Name"'
PUSH KEY
ON KEY
PRIVATE lcFile_ttl,lcWinTitl
=Ariabrow('','Employees',gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
             lcFieldsNam,'laTemp')
USE IN &lcTmpEmpl
ERASE(gcWorkDir+lcTmpEmpl+'.DBF')
ERASE(gcWorkDir+lcTmpEmpl+'.CDX')
POP KEY
*m.Employee = laTemp[4]
m.Employee = laTemp[3]

*-- end of lfBrowEmp.

*:**************************************************************************
*:* Name        : lfAddEmpl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/03/2005
*:* Purpose     : Add a new employee while entring a sales order
*:***************************************************************************
*C123847,1
FUNCTION lfAddEmpl
llNewEmp = .F.  && Hide the "Select" Button
lcEmployee = SPACE(12)
DO (gcAppHome+'SO\SOGTEMPL') WITH laData[2],laData[3]
 
*-- end of lfAddEmpl.

*:**************************************************************************
*:* Name        : lfCrTmpFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Create temp file for uniform browse
*:***************************************************************************
*C123847,1
FUNCTION lfCrTmpFl
PRIVATE lnSlct,laStru,lnI,lnLen
lnSlct = SELECT()

SELECT UNIFORM
=AFIELD(laStru)

*- Store in this field the component styles of the current style from BOM file
lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'CSTYCOMP'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 19
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'FIT'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 5
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'GP'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 2
laStru[lnLen,4] = 0
 
*FOR lnI = 1 TO 10
* Use fields from 1 to 10 in the browse in the SOENORD screen, use from 1 to 16 in the OTS browse screen
FOR lnI = 1 TO 16
  lnLen = ALEN(laStru,1) + 1
  DIMENSION laStru[lnLen,4]
  laStru[lnLen,1] = 'QTY'+ALLTRIM(STR(lnI))
  laStru[lnLen,2] = 'N'
  laStru[lnLen,3] = 2
  laStru[lnLen,4] = 0
ENDFOR

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'TOTAL'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 5
laStru[lnLen,4] = 0

*- store scale of the current size 
FOR lnI = 1 TO 16
  lnLen = ALEN(laStru,1) + 1
  DIMENSION laStru[lnLen,4]
  laStru[lnLen,1] = 'SCALE'+ALLTRIM(STR(lnI))
  laStru[lnLen,2] = 'C'
  laStru[lnLen,3] = 4
  laStru[lnLen,4] = 0
ENDFOR

FOR lnI = 1 TO 16
  lnLen = ALEN(laStru,1) + 1
  DIMENSION laStru[lnLen,4]
  laStru[lnLen,1] = 'SZ'+ALLTRIM(STR(lnI))
  laStru[lnLen,2] = 'C'
  laStru[lnLen,3] = 5
  laStru[lnLen,4] = 0
ENDFOR

*- store OTS value for this size
FOR lnI = 1 TO 16
  lnLen = ALEN(laStru,1) + 1
  DIMENSION laStru[lnLen,4]
  laStru[lnLen,1] = 'OTS'+ALLTRIM(STR(lnI))
  laStru[lnLen,2] = 'N'
  laStru[lnLen,3] = 6
  laStru[lnLen,4] = 0
ENDFOR

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'FLAG'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 1
laStru[lnLen,4] = 0

CREATE TABLE (gcWorkDir+lcTempCur) FROM ARRAY laStru
INDEX ON CSTYMAJOR+COLOUR+FIT+STR(GP,2) TAG &lcTempCur

SELECT (lnSlct)
*-- end of lfCrTmpFl.

*:**************************************************************************
*:* Name        : lfFillTemp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/18/2004
*:* Purpose     : Fill temp file with uniform styles
*:***************************************************************************
*C123847,1
FUNCTION lfFillTemp
PARAMETERS llEdit

PRIVATE lnI,lcSvOrd,lcSvRel,lcSty,lcTp,lcClr,lcItemSty,lnResp
lcSvOrd = ORDER(lcOrdLine)
SELECT &lcOrdLine
INDEX ON EMPLOYEE+STYLE TO (gcWorkDir+lcOrdLine)

lcSvRel = SET('RELATION')
SELECT &lcTempCur
SET RELATION TO

SELECT UNIFORM
=SEEK(EMPLOYEE.UCODE,'UNIFORM')
SCAN REST WHILE UCODE+CSTYMAJOR+COLOUR = EMPLOYEE.UCODE
  SCATTER MEMVAR
  
  =SEEK(SUBSTR(CSTYMAJOR,1,lnMajorLen),'STYLE')
  =SEEK('S'+LEFT(STYLE.SCALE,lnExtSzWd),'SCALE')
  SELECT SCALE
  DO WHILE TYPE+SCALE+PREPAK = 'S'+LEFT(STYLE.SCALE,lnExtSzWd) .AND. !EOF('SCALE')
    lcFit = SCALE.CDIM1
    lnSzCnt = 0
    lnGp = 1
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+LEFT(STYLE.SCALE,lnExtSzWd) .AND. ;
                    SCALE.CDIM1 = lcFit
      FOR lnI = 1 TO SCALE.CNT
        lnSzCnt = lnSzCnt + 1
        IF lnSzCnt > 10
          lnGp = lnGp + 1
          lnSzCnt = 1
        ENDIF

        SELECT &lcTempCur
        IF !SEEK(UNIFORM.CSTYMAJOR+UNIFORM.COLOUR+lcFit+STR(lnGp,2),lcTempCur)
          APPEND BLANK
          REPLACE CSTYMAJOR WITH UNIFORM.CSTYMAJOR ;
                  COLOUR    WITH UNIFORM.COLOUR ;
                  FIT       WITH lcFit ;
                  GP        WITH lnGp
        ENDIF
        lcI = STR(lnI,1)
        lcTmpSz = ALLTRIM(STR(lnSzCnt))
        
        REPLACE SCALE&lcTmpSz  WITH SCALE.SCALE + lcI ;
                SZ&lcTmpSz     WITH SCALE.SZ&lcI 

        lcSty = SUBSTR(CSTYMAJOR,1,lnMajorLen)+lcSepart+;
                SUBSTR(COLOUR,1,lnClrLen)+lcClrSpr+SCALE.SCALE
        *- tmi 03/02/2005
        lcSty = PADR(lcSty,19)                
        *- tmi 03/02/2005
        
        *- Get data from lcOrdline if it entered befor
        IF SEEK(lcEmployee+lcSty,lcOrdLine)
          REPLACE QTY&lcTmpSz WITH &lcOrdLine..Qty&lcI
        ENDIF
         
        *-If style is not assigned to the selected WH ...
        IF laSetups[5,2]='Y' .AND. !SEEK(lcSty+laData[31]+SPACE(10),'StyDye')
          *- Button : 40002  :: \<Add;\?\<Reenter
          lcMsg = 'Style ' + lcSty + ' is not assigned to location ' + laWareHouses[lnWareHouse,1] + CHR(13) +;
                  ' This style is a member of uniform ' + EMPLOYEE.UCODE +;
                  ' which is assigned to the employee ' + EMPLOYEE.EMPLOYEE
          lnResp = gfModalGen('QRM00000B40002',.F.,.F.,.F.,lcMsg)
          IF lnResp = 1
            DO gpAdStyWar WITH lcSty,SPACE(10),laData[31]
          ELSE
            SELECT &lcTempCur
            ZAP
            RETURN .F.
          ENDIF
        ENDIF
        
      ENDFOR
    ENDSCAN
    
    SELECT SCALE
    
  ENDDO
  
ENDSCAN

SELECT &lcTempCur
SET FILTER TO GP=1
SET RELATION TO &lcSvRel
GO TOP

SELECT &lcOrdLine
SET ORDER TO &lcSvOrd
*-- end of lfFillTemp.

*:**************************************************************************
*:* Name        : lfActBr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Browse detail function
*:***************************************************************************
*C123847,1
FUNCTION lfActBr
PARAMETERS llClear
PRIVATE lcBrFlds ,lcBrTitle,lcKey,lnSlct,lcMd
lnSlct = SELECT(0)

lc1  = &lcTempCur..Sz1
lc2  = &lcTempCur..Sz2
lc3  = &lcTempCur..Sz3
lc4  = &lcTempCur..Sz4
lc5  = &lcTempCur..Sz5
lc6  = &lcTempCur..Sz6
lc7  = &lcTempCur..Sz7
lc8  = &lcTempCur..Sz8
lc9  = &lcTempCur..Sz9
lc10 = &lcTempCur..Sz10

SELECT &lcTempCur
lnMrk = RECNO(lcTempCur)
IF llClear

  lcBrFlds = "lcMrk=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "CSTYMAJOR:H='Style' :W=.F. :R:15"

ELSE

  lcBrFlds = "lcMrk=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "CSTYMAJOR:H='Style' :W=.F. :R:15,"+;
             "COLOUR   :H='Colour':W=.F. :R:9 ,"+;
             "FIT      :H='Fit'   :W=.F. :R:9 ,"+;
             "QTY1    :H='' :W=!EMPTY(SCALE1) :V=lfvTotal(1) :5,"+;
             "QTY2    :H='' :W=!EMPTY(SCALE2) :V=lfvTotal(2) :5,"+;
             "QTY3    :H='' :W=!EMPTY(SCALE3) :V=lfvTotal(3) :5,"+;
             "QTY4    :H='' :W=!EMPTY(SCALE4) :V=lfvTotal(4) :5,"+;
             "QTY5    :H='' :W=!EMPTY(SCALE5) :V=lfvTotal(5) :5,"+;
             "QTY6    :H='' :W=!EMPTY(SCALE6) :V=lfvTotal(6) :5,"+;
             "QTY7    :H='' :W=!EMPTY(SCALE7) :V=lfvTotal(7) :5,"+;
             "QTY8    :H='' :W=!EMPTY(SCALE8) :V=lfvTotal(8) :5,"+;
             "QTY9    :H='' :W=!EMPTY(SCALE9) :V=lfvTotal(9) :5,"+;
             "QTY10   :H='':W=!EMPTY(SCALE10):V=lfvTotal(10):5,"+;
             "Total   :H='Total' :W=.F.:R :5  "

ENDIF

BROWSE FIELDS &lcBrFlds;
       LOCK 0  ;
       NOAPPEND;
       NOCLEAR ;
       NODELETE;
       NOMENU  ;
       NOWAIT  ;
       SAVE    ;
       WHEN lfwBrows();
       VALID :F lfvBrow();         
       TITLE (lcBrTtl) ;
       WINDOW "SOENORD2" ;
       IN WINDOW "SOENORD"

=lfRefresh()

SELECT (lnSlct)

*:**************************************************************************
*:* Name        : lfwBrows
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/07/2004
*:* Purpose     : Refresh record pointer.
*:***************************************************************************
*C123847,1
FUNCTION lfwBrows
lnMrk  = RECNO(lcTempCur)

*C123847,3  TMI [Start] get size descriptions trimmed
*-lc1  = &lcTempCur..Sz1
*-lc2  = &lcTempCur..Sz2
*-lc3  = &lcTempCur..Sz3
*-lc4  = &lcTempCur..Sz4
*-lc5  = &lcTempCur..Sz5
*-lc6  = &lcTempCur..Sz6
*-lc7  = &lcTempCur..Sz7
*-lc8  = &lcTempCur..Sz8
*-lc9  = &lcTempCur..Sz9
*-lc10 = &lcTempCur..Sz10

lc1  = ALLTRIM(&lcTempCur..Sz1)
lc2  = ALLTRIM(&lcTempCur..Sz2)
lc3  = ALLTRIM(&lcTempCur..Sz3)
lc4  = ALLTRIM(&lcTempCur..Sz4)
lc5  = ALLTRIM(&lcTempCur..Sz5)
lc6  = ALLTRIM(&lcTempCur..Sz6)
lc7  = ALLTRIM(&lcTempCur..Sz7)
lc8  = ALLTRIM(&lcTempCur..Sz8)
lc9  = ALLTRIM(&lcTempCur..Sz9)
lc10 = ALLTRIM(&lcTempCur..Sz10)
*C123847,3  TMI [End  ] 
SHOW WINDOW (lcBrTtl)  REFRESH

lcDesc    = STYLE.DESC
lnEnt     = UNIFORM.ENTITLEMNT
lnUsed    = STYHIST.NUSED     
lnBalance = lnEnt - lnUsed
lcCSTYNOT = STYLE.CSTYNOT
=lfRefresh('SOENORD1')
=lfRefresh('SOENORD3')

*:**************************************************************************
*:* Name        : lfvBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Valid fn. for browse
*:***************************************************************************
*C123847,1
FUNCTION lfvBrow
*-
*-- end of lfvBrow.

*:**************************************************************************
*:* Name        : lfvTotal
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/19/2004
*:* Purpose     : Valid function for browse fields
*:***************************************************************************
*C123847,1
FUNCTION lfvTotal
PARAMETER lnFld
PRIVATE lcI
lcI = ALLTRIM(STR(lnFld))
IF EMPTY(SCALE&lcI)
  BLANK FIELDS QTY&lcI
  RETURN
ENDIF

llUpd = .T.
SHOW GET pbClr ENABLE
SHOW GET pbSv  ENABLE

REPLACE TOTAL WITH QTY1 + QTY2 + QTY3 + QTY4 + QTY5 + ;
                   QTY6 + QTY7 + QTY8 + QTY9 + QTY10
*-- end of lfvTotal.

*:**************************************************************************
*:* Name        : lfAudtFlds
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Update audit fields
*:***************************************************************************
*C123847,1
FUNCTION lfAudtFlds
IF laScrMode[4]
  REPLACE CADD_USER WITH gcUser_ID ;
          CADD_TIME WITH TIME()    ;
          DADD_DATE WITH gdSysDate
ELSE
  REPLACE CEDIT_USER WITH gcUser_ID ;
          CEDIT_TIME WITH TIME()    ;
          DEDIT_DATE WITH gdSysDate
ENDIF
*-- end of lfAudtFlds.

*-------------------------*  Valid functions for the screen "soenord" *-------------------------*
*:**************************************************************************
*:* Name        : lfvSav
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Save function
*:***************************************************************************
*C123847,1
FUNCTION lfvSav
PRIVATE lcSty,lcOrd,lnQkCnt,lnJ,lcJ,lcQ,lnIncrmnt,lnK,llDfChkQty
Private lnPrvsAls , lnSizeSeq , lnScaleLen , lnQkCnt , lnGrprice , lnCommPrc,;
        lcOldColor , llSkip

SELECT &lcOrdLine
lcSvOrd = ORDER(lcOrdLine)
INDEX ON EMPLOYEE+STYLE TO (gcWorkDir+lcOrdLine)

SELECT &lcTempCur
SET FILTER TO GP < 70
GO TOP
SCAN
  WAIT WINDOW 'Collect data for Style # : ' + CSTYMAJOR + '/'+ COLOUR +'...'    NOWAIT
  FOR lnI = 1 TO 10
    lcI = ALLTRIM(STR(lnI))
    IF !EMPTY(&lcTempCur..SCALE&lcI)
    
      lcSty = SUBSTR(&lcTempCur..CSTYMAJOR,1,lnMajorLen)+lcSepart+;
              SUBSTR(&lcTempCur..COLOUR,1,lnClrLen)+lcClrSpr+SUBSTR(&lcTempCur..SCALE&lcI,1,3)
      =SEEK(lcSty,'STYLE')
      =SEEK('S'+STYLE.SCALE,'SCALE')
      
      lcSz = SUBSTR(&lcTempCur..SCALE&lcI,4,1)
      
      IF !SEEK(m.Employee+lcSty , lcOrdLine ) .AND. &lcTempCur..QTY&lcI > 0
        *- Update line count
        lnLines    = lnLines + 1
        
        *- Get price based on price codes
        STORE 0 TO lnGrssPric, lnStyComm
        =lfPrice()

        m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
        m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')

        SELECT &lcOrdLine
        APPEND BLANK        
        REPLACE CORDTYPE   WITH lcOrdType ;
                ORDER      WITH laData[1] ;
                ACCOUNT    WITH laData[2] ;
                STORE      WITH laData[3] ;
                EMPLOYEE   WITH M.EMPLOYEE ;
                CUSTPO     WITH IIF(!laData[7],&lcOrdHdr..CustPo,'') ;
                cWareCode  WITH laWareHouse[lnWareHouse,2] ;
                SCALE      WITH LEFT(&lcTempCur..SCALE&lcI,3) ;
                STYLE      WITH lcSty ;
                Desc1      WITH STYLE.Desc1  ;
                PrePak     WITH Style.PrePak ;
                Season     WITH Style.Season ;
                Gl_Sales   WITH m.Gl_Sales ;
                Flag       WITH 'N'        ;
                COMPLETE   WITH laData[10] ;
                Gros_Price WITH lnGrssPric ;
                Price      WITH lnGrssPric ;
                COMM1      WITH lnStyComm  ;
                Start      WITH laData[9]  ;
                COMPLETE   WITH IIF( llCMP2SOLD .AND. !EMPTY(STYLE.SOLDOUT) , STYLE.SOLDOUT , laData[10] ) ;
                CUSTPO     WITH laData[4]  ;
                LineNo     WITH lnLines      
        =lfAudtFlds()
      ENDIF

      *--Update laData array 
      laData[35] = laData[35] + (&lcTempCur..QTY&lcI - QTY&lcSz)
      laData[36] = laData[36] + (&lcTempCur..QTY&lcI - QTY&lcSz)*Price
      laData[41] = laData[41] + (&lcTempCur..QTY&lcI - QTY&lcSz)
      laData[42] = laData[42] + (&lcTempCur..QTY&lcI - QTY&lcSz)*Price
      
      M.QTY&lcSz = &lcTempCur..QTY&lcI
  
      SELECT &lcOrdLine
      REPLACE QTY&lcSz  WITH &lcTempCur..QTY&lcI ;
              BOOK&lcSz WITH &lcTempCur..QTY&lcI ;
              TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
              TOTBOOK   WITH TOTQTY   
    ENDIF
  ENDFOR
ENDSCAN
WAIT CLEAR

*- Remove lines with zero qty from "lcOrdline" temp file
SELECT &lcOrdLine
SCAN FOR TOTQTY = 0
  REPLACE FLAG WITH 'M'
ENDSCAN
GO TOP  
DELETE FOR TOTQTY = 0

SHOW GET pbSv  DISABLE
SHOW GET pbClr DISABLE
llUpd = .F.

SELECT &lcOrdLine
SET ORDER TO &lcSvOrd

*-Clears the current employee data from the temp file
=lfvClr(.T.)

*-- end of lfSav.

*:**************************************************************************
*:* Name        : lfPrice
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/22/2004
*:* Purpose     : Get price , using Davmain procedure lfPRICCODE
*:***************************************************************************
*C123847,1
FUNCTION lfPrice
PRIVATE lcProc,lcPriCod
lcProc = SET('PROCEDURE')
SET PROCEDURE TO DAVMAIN.PRG ADDITIVE

*C123847,1  TMI [Start] 
lM_PRCCOD = gfGetMemvar('M_PRCCOD')
lM_PRCCOD = IIF(TYPE('lM_PRCCOD')<>'L',.T.,lM_PRCCOD)
*C123847,1  TMI [End  ] 
lcPriCod = lfPRICCODE()
IF !EMPTY(lcPriCod) .AND. SEEK(lcPriCod+laData[33]+lcSty,'CSTPRICE')        
  *- use the special size price , if it is 0 use the special scale price ( FOR DIR03 ) 
  lnGrssPric = IIF(CSTPRICE.PRICE&lcSz<>0,CSTPRICE.PRICE&lcSz, CSTPRICE.PRICEDV)
  lnStyComm  = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,laData[28],0),CSTPRICE.COMMDV)
  
ELSE      
  IF llMulCurr .AND. laData[33]<>gcBaseCurr 
    IF SEEK(lcSty+laData[33],'STYPRICE')
      lnGrssPric= STYPRICE.PRICEA
      lnStyComm = IIF(Style.Commission,laData[28],0)
    ENDIF
  ELSE     
    lnGrssPric = STYLE.PRICEA
    lnStyComm  = IIF(Style.Commission,laData[28],0)
  ENDIF
ENDIF

SET PROCEDURE TO &lcProc
*-- end of lfPrice.

*:**************************************************************************
*:* Name        : lfvClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Clears Screen and allows to reinput
*:***************************************************************************
*C123847,1
FUNCTION lfvClr
PARAMETERS llFrmSav
PRIVATE lnSlct,lnI,lnResp
lnSlct = SELECT()

lnResp = 1
IF !llFrmSav .AND. llUpd
  lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure to clear entered data?')
ENDIF  
IF lnResp = 1
  SELECT &lcTempCur
  ZAP

  STORE '' TO m.Employee,m.SITE_NO, m.EMPNAME,lcCSTYNOT
  STORE {} TO m.DSTART
  
  SHOW GET pbSv  DISABLE
  SHOW GET pbClr DISABLE
  SHOW GET pbOTS DISABLE
  
  SHOW GET pbEmployee ENABLE
  SHOW GET m.Employee ENABLE
  _CUROBJ = OBJNUM(m.Employee) 
  llUpd = .F.
  lcGps = '1/1'

  *- Update the browse
  =lfActBr(.T.)
ENDIF

SELECT (lnSlct)
*-- end of lfClr.

*:**************************************************************************
*:* Name        : lfOTS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : OTS browse function
*:***************************************************************************
*C123847,1
FUNCTION lfvOTS

PRIVATE lcBrFields, lnSlct,lcFile_ttl,lcWinTitl,lcBrTtl,;
        lnI,lcTp,lcClr,lcItemSty,;
        lcTmpOts,lcStyMaj,lcStyClr,lcSty

lnSlct = SELECT()

lcStyMaj = &lcTempCur..CSTYMAJOR
lcStyClr = &lcTempCur..COLOUR

lcTmpOts = 'N'+SUBSTR(lcfolder,2)

IF !FILE(gcWorkDir+lcTmpOts+'.DBF')
  SELECT &lcTempCur
  COPY STRUCTURE TO (gcWorkDir+lcTmpOts)
ENDIF
IF !USED(lcTmpOts)
  SELECT 0
  USE (gcWorkDir+lcTmpOts) 
ENDIF
SELECT &lcTmpOts
LOCATE

IF RECCOUNT(lcTmpOts) = 0 .OR. &lcTmpOts..CSTYMAJOR <> lcStyMaj

  INDEX ON FLAG+CSTYMAJOR+COLOUR+FIT TAG &lcTmpOts

  =SEEK(SUBSTR(lcStyMaj,1,lnMajorLen),'STYLE')
  =SEEK('S'+LEFT(STYLE.SCALE,lnExtSzWd),'SCALE')
  SELECT SCALE
  DO WHILE TYPE+SCALE+PREPAK = 'S'+LEFT(STYLE.SCALE,lnExtSzWd) .AND. !EOF('SCALE')
    lcFit = SCALE.CDIM1
    lnSzCnt = 0
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+LEFT(STYLE.SCALE,lnExtSzWd) .AND. ;
                    SCALE.CDIM1 = lcFit
      FOR lnI = 1 TO SCALE.CNT
        lnSzCnt = lnSzCnt + 1

        SELECT &lcTmpOTS
        IF !SEEK(' '+lcStyMaj+lcStyClr+lcFit,lcTmpOTS)
          APPEND BLANK
          REPLACE CSTYMAJOR WITH lcStyMaj ;
                  COLOUR    WITH lcStyClr ;
                  FIT       WITH lcFit 
        ENDIF
        lcI = STR(lnI,1)
        lcTmpSz = ALLTRIM(STR(lnSzCnt))
        
        REPLACE SCALE&lcTmpSz  WITH SCALE.SCALE + lcI ;
                SZ&lcTmpSz     WITH SCALE.SZ&lcI 

        lcSty = SUBSTR(lcStyMaj,1,lnMajorLen)+lcSepart+;
                SUBSTR(lcStyClr,1,lnClrLen)+lcClrSpr+SCALE.SCALE

        *- Get OTS for this size
        =SEEK(lcSty+laWareHouses[lnWareHouse,2],'STYDYE')
        REPLACE OTS&lcTmpSz WITH STYDYE.STK&lcI + STYDYE.WIP&lcI - STYDYE.ORD&lcI
         
        *- Get component styles from BOM file and get OTS for them
        *- key : CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR        
 
        lcTp = IIF(STYLE.MAKE,lcMType,lcIType)
        
        IF SEEK(lcStyMaj+lcTp,'BOM')
          SELECT BOM
          SCAN REST WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = lcStyMaj+lcTp
            lcClr = ' '
            IF '*' $ SUBSTR(BOM.ITEM,lnClrPos,lnClrLen)
              && case of component color same as style color
              lcClr = lcStyClr
            ELSE  
              && case of different or constant color
              lcClr = PADR(SUBSTR(BOM.ITEM,lnClrPos,lnClrLen),6)
            ENDIF
  
            IF !SEEK('X'+lcStyMaj+lcClr+lcFit,lcTmpOTS)
              SELECT &lcTmpOTS
              APPEND BLANK
              REPLACE CSTYMAJOR WITH UNIFORM.CSTYMAJOR ;
                      CSTYCOMP  WITH SUBSTR(BOM.ITEM,1,lnMajorLen) ;
                      COLOUR    WITH UNIFORM.COLOUR ;
                      FIT       WITH lcFit  ;
                      FLAG      WITH 'X'
            ENDIF

            lcItemSty = SUBSTR(BOM.ITEM,1,lnMajorLen)+lcSepart+PADR(lcClr,lnClrLen)+lcClrSpr+SCALE.SCALE
            IF SEEK(lcItemSty+laWareHouses[lnWareHouse,2],'STYDYE')
              SELECT &lcTmpOTS
              REPLACE OTS&lcTmpSz WITH STYDYE.STK&lcI + STYDYE.WIP&lcI - STYDYE.ORD&lcI
            ENDIF

          ENDSCAN
        ENDIF
        
      ENDFOR
    ENDSCAN
    
    SELECT SCALE
    
  ENDDO
  
ENDIF

*- ============= Browsing ============= -*
SELECT &lcTmpOTS
GO TOP
PRIVATE laSz,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SZ9,SZ10,SZ11,SZ12,SZ13,SZ14,SZ15,SZ16
lcFields = 'SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SZ9,SZ10,SZ11,SZ12,SZ13,SZ14,SZ15,SZ16'
SCATTER FIELDS &lcFields TO laSz

lcBrFields  = "CSTYMAJOR:H='Style          ' ,"+;
              "CSTYCOMP :H='Component Style' ,"+;
              "COLOUR   :H='Colour'   ,"+;
              "FIT      :H='  Fit  '  ,"
FOR lnI = 1 TO 16
  lcI = ALLTRIM(STR(lnI))
  IF !EMPTY(laSz[lnI])
    lcBrFields = lcBrFields + "OTS&lcI :H='&laSz[lnI]' ,"
  ENDIF
ENDFOR        
lcBrFields = LEFT(lcBrFields,LEN(lcBrFields)-1)
              
lcBrTtl     = 'O T S' 
SELECT &lcTmpOTS
LOCATE
  
PUSH KEY
ON KEY
=ARIABROW([''],lcBrTtl,gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2)
POP KEY

SELECT (lnSlct)  
*-- end of lfOTS.

*:**************************************************************************
*:* Name        : lfClo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Close function
*:***************************************************************************
*C123847,1
FUNCTION lfvClo
PRIVATE lnI,lnResp
lnResp = 1
IF llUpd
  lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure to close the screen and lose entered data?')
ENDIF  
IF lnResp = 1
  CLEAR READ
ENDIF
 
*-- end of lfClo.

*:**************************************************************************
*:* Name        : lfBrBack
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*! Purpose      : TO Assign functions to some keys to not affect the browse
*:***************************************************************************

*C123847,1
FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

*-- Set the global flag "glFromBrow" to true only if one of the
*-- screen's browses is active.
glFromBrow = INLIST(WONTOP(),lcBrTtl)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF WONTOP() = lcBrTtl
  ON KEY LABEL TAB        DO lfBrTab
  ON KEY LABEL BACKTAB    DO lfBrBack
ENDIF  
ON KEY LABEL ESC llDummy = .T.
*ON KEY LABEL ESC  DO lfvClo

RETURN .F.

*:**************************************************************************
*:* Name        : lfClrTrap
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*! Purpose      : Clearing the previous trapping
*:***************************************************************************

*C123847,1
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
IF WONTOP () <> lcBrTtl
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF  
*ON KEY LABEL ESC DO lfvClo IN DIRMAIN.PRG
ON KEY LABEL ESC llDummy = .T.

SHOW GET pbClo ENABLE

*:**************************************************************************
*:* Name        : lfBrBack
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*! Purpose      : Trap the Tab Key
*:***************************************************************************
*C123847,1
FUNCTION lfBrTab

IF WONTOP() = lcBrTtl
  ACTIVATE WINDOW "SOENORD3"
  _CUROBJ = OBJNUM(pbSv)
ENDIF  

*:**************************************************************************
*:* Name        : lfBrBack
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Trap the BackTab Key
*:***************************************************************************
*C123847,1
FUNCTION lfBrBack
IF WONTOP () = lcBrTtl
  ACTIVATE WINDOW "SOENORD1"
  _CUROBJ = OBJNUM(pbPrior)
ENDIF  

*!*************************************************************
*! Name      : lfGetClrD
*! Developer : TMI
*! Date      : 04/03/2001
*! Purpose   : To get color position also color length
*!*************************************************************
*! Called from : ICSLCAT1.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetClrD
*!*************************************************************
*C123847,1
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount &&Tmi 07/15/2002
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
  ENDCASE
ENDFOR

SELECT(lcOldSelect)
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfvPr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : go top previous group
*:***************************************************************************
*C123847,1
FUNCTION lfvPr
PRIVATE lcKey
SELECT &lcTempCur
lcKey = CSTYMAJOR+COLOUR+FIT
lnGp = &lcTempCur..GP
lnGp = lnGp - 1
SET FILTER TO GP=lnGp
GO TOP
SEEK lcKey

lcGps = ALLTRIM(STR(lnGp))+'/'+ALLTRIM(STR(lnGps))
SHOW GET lcGps
SHOW GET pbNext ENABLE
IF lnGp = 1 
  SHOW GET pbPrior DISABLE
ENDIF

*- Refresh browse
=lfActBr()

*-- end of lfvPr.

*:**************************************************************************
*:* Name        : lfvNx
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : go to next group
*:***************************************************************************
*C123847,1
FUNCTION lfvNx
PRIVATE lcKey

SELECT &lcTempCur
lcKey = CSTYMAJOR+COLOUR+FIT
lnGp = &lcTempCur..GP
lnGp = lnGp + 1
SET FILTER TO GP=lnGp
GO TOP
SEEK lcKey

lcGps = ALLTRIM(STR(lnGp))+'/'+ALLTRIM(STR(lnGps))

SHOW GET lcGps 
SHOW GET pbPrior  ENABLE
IF lnGp = lnGps
  SHOW GET pbNext DISABLE
ENDIF

*- Refresh browse
=lfActBr()
*-- end of lfvNx.

*:**************************************************************************
*:* Name        : lfRefNav
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/20/2004
*:* Purpose     : Refresh navigate buttons in 'SOENORD1' screen
*:***************************************************************************
*:* Called from : when function of the screen 'soenord'
*:***************************************************************************
*C123847,1
FUNCTION lfRefNav

*- Refresh "pbNext" button
PRIVATE laGp
DIMENSION laGp[1]
laGp[1] = 0
SELECT MAX(GP) FROM &lcTempCur WHERE GP<70 INTO ARRAY laGp
IF laGp[1]>1
  lnGps = laGp[1]
  lcGps = '1/'+ALLTRIM(STR(lnGps))
  SHOW GET lcGps
  SHOW GET pbNext ENABLE
  =lfRefresh() 
ENDIF
*-- end of lfRefNav.
*:**************************************************************************
*:* Name        : lfREFBRW
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/20/2004
*:* Purpose     : Refresh browse in soord screen after closing the "soenord" screen
*:***************************************************************************
*:* Called from : lfReadAct in SOORD.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfREFBRW
PRIVATE lcTmpChk,lnSlct,lnCnt,lnRecno
lnSlct = SELECT()
lcTmpChk = gcWorkDir+'#'+SUBSTR(lcFolder,2)+'.TMI'
IF USED(lcOrdLine) .AND. FILE(lcTmpChk)
  ERASE (lcTmpChk)
  =lfvBrowZoom()
ENDIF
 
*C123847,1  TMI [Start] 
*IF USED(lcOrdLine) .AND. lnActFolder = 2
IF (laScrMode[3] .OR. laScrMode[4]) .AND. lnActFolder = 2 .AND. USED(lcOrdLine)
  *C123847,1  TMI [End  ]
  SELECT &lcOrdLine
  lnRecno = RECNO()
  lnCnt = 0
  COUNT TO lnCnt
  IF BETWEEN(lnRecno,1,RECCOUNT())
    GOTO (lnRecno)
  ENDIF
*  GO BOTTOM
  IF !EMPTY(EMPLOYEE)
    IF lnCnt > 0
      SHOW GETS WINDOW (lcWinCh32)  DISABLE ONLY  
      SHOW GETS WINDOW (lcWinCh321) DISABLE ONLY  
      SHOW GETS WINDOW (lcWinCh322) DISABLE ONLY
      SHOW GET pbRemove ENABLE
    ELSE
      SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
      SHOW GET pbNew     ENABLE
      SHOW GET pbRemove DISABLE
    ENDIF
  
  ELSE
  
    SHOW GETS WINDOW (lcWinCh322) ENABLE ONLY
    SHOW GET pbNew ENABLE
    IF lnCnt > 0
      SHOW GET pbRemove  ENABLE
    ELSE
      SHOW GET pbRemove  DISABLE
    ENDIF
  
  ENDIF    
  ACTIVATE WINDOW gwcContrl1
ENDIF

*C123847,5  TMI [Start] Check if this is DIR03 and the screen is called from ALORDAL then remove the definition of the ESC key
IF !EMPTY(lcOrderNo)
  ON KEY LABEL ESC llDummyVar = .T.
ENDIF
*C123847,5  TMI [End  ] 

SELECT (lnSlct)
*-- end of lfREFBRW.

*:**************************************************************************
*:* Name        : lfStyCmpTp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/22/2004
*:* Purpose     : Get character related to style component setup
*:***************************************************************************
*C123847,1
FUNCTION lfStyCmpTp
PRIVATE lnI,lnK,lcK,laMainSetp,lcIM

DIMENSION laMainSetp[5,2]
FOR lnI = 1 TO 2
  
  laMainSetp = .F.
  lcIM = IIF(lnI = 1 , 'I' , 'M' )
  
  FOR lnK = 1 TO 5
    lcK = STR(lnK,1)
    laMainSetp[lnK,1] = 'M_C'+lcIM+'TYPE'+lcK
  ENDFOR  
  =gfGetMemVar(@laMainSetp , gcAct_Comp)
  
  FOR lnK = 1 TO 5
    IF laMainSetp[lnK,2] = 'S'
      lc&lcIM.Type = STR(lnK,1)
      EXIT
    ENDIF
  ENDFOR
  
ENDFOR  
*-- end of lfStyCmpTp.

*:**************************************************************************
*:* Name        : lfCMPCSTPO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2005
*:* Purpose     : set the field customer.LCMPCSTPO to 'yes' as default when adding a new customer
*:***************************************************************************
*:* Called from : lpShow IN ARCUST.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfCMPCSTPO
PRIVATE lnPos
lnPos = ASCAN(laUsrFields,'LCMPCSTPO')
IF lnPos>0
  lnPos = ASUBSCRIPT( laUsrFields , lnPos , 1 )
  laUsrFields[lnPos,6] = .T.
ENDIF

*-- end of lfCMPCSTPO.

*:**************************************************************************
*:* Name        : lfADDVLDFN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/24/2005
*:* Purpose     : Update the array laRelFld with the custom valid function ( trigger )
*:***************************************************************************
*:* Called from : lfObj_Typ IN SMCODES.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfADDVLDFN
PRIVATE lnSlct,lnRecno,lcSvOrd
lnSlct = SELECT()

IF laRelFld[1,1] = 'NORDCHG   '
  
  IF !USED('SYDFIELD')
    =gfOpenFile(gcSysHome+'SYDFIELD','CFLD_NAME','SH')
  ENDIF
  
  SELECT SYDFIELD
  lcSvOrd = ORDER('SYDFIELD')
  lnRecno = RECNO('SYDFIELD')
  
  SET ORDER TO CFLD_NAME
  =SEEK(laRelFld[lnAryNo,1])
  
  IF !EMPTY(CPICT_STR)
    laRelFld[lnAryNo,7] = ALLTRIM(SYDFIELD.CPICT_STR)
  ENDIF
  
  IF !EMPTY(laRelFld[lnAryNo,8])
    laRelFld[lnAryNo,10] = ' VALID ' + laRelFld[lnAryNo,8]
  ENDIF
  
  SELECT SYDFIELD
  LOCATE
  SET ORDER TO &lcSvOrd 
  IF BETWEEN(lnRecno,1,RECCOUNT('SYDFIELD'))
    GOTO lnRecno
  ENDIF

ENDIF

SELECT (lnSlct)  
*-- end of lfADDVLDFN.

*:**************************************************************************
*:* Name        : lfCHGACC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2005
*:* Purpose     : Valid function for charged account field
*:***************************************************************************
*:* Called from : SMCODES screen , validate the field CHGACC, that is related to CORDCHG
*:***************************************************************************
*C123847,1
FUNCTION lfCHGACC
PRIVATE lnPos,lcChgAcc, xAccount,lnSlct
lnSlct = SELECT()
lnPos = ASCAN(laRelFld,'CHGACC')
IF lnPos > 0
  lnPos = ASUBSCRIPT(laRelFld,lnPos,1)
  lcChgAcc = "laRelFld[lnPos,6]"

  IF !USED('CUSTOMER')
    =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  ENDIF

  IF !EMPTY(&lcChgAcc) .AND. !SEEK('M'+&lcChgAcc,'CUSTOMER')
    xAccount = &lcChgAcc
    SELECT CUSTOMER
    PRIVATE lcBrFields, lcBrowTitle  
    DO CUSBROWM WITH xAccount
    &lcChgAcc = xAccount
  ENDIF
  
ENDIF  
SELECT (lnSlct)
*-- end of lfCHGACC.

*:**************************************************************************
*:* Name        : lfMRCHDSCD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2005
*:* Purpose     : update codes file with a code in "Non-merchandise charges" for each new code in "Order charges"
*:***************************************************************************
*:* Called from : lfvOk in SMCODES.PRG 
*:***************************************************************************
*C123847,4  TMI 
FUNCTION lfMRCHDSCD
IF laRelFld[1,1] = 'NORDCHG   '
        
  *KEY : CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME
  IF !SEEK('N'+lcCode_no+'N'+'CCHRGCODE ',lc_TmpFl)
    INSERT INTO (gcWorkDir+lc_Tmpfl) ;
           (cdefcode,cfld_name,ccode_no, ;
            cdiscrep,crltfield, ;
            cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
            cadd_user,dadd_date,cadd_time,cStatus) ;
    VALUES ('N','CCHRGCODE ',lcCode_no, ;
            lcCode_no ,"N","",;
            "","", ;
            gcUser_ID,DATE(),gfGetTime(),"A")
  ENDIF
  IF !SEEK('N'+lcCode_no+'Y'+'CCHRGCODE ',lc_TmpFl)
    DIMENSION laNoTaxRt[1]
    laNoTaxRt[1] = ''
    SELECT CCODE_NO  FROM CODES ;
      WHERE CFLD_NAME = 'CTAXCODE  ' .AND. ;
          CRLTD_NAM = 'NTAXRATE  ' .AND. ;
          '0.0' $ CRLTD_VLU ;
          INTO ARRAY laNoTaxRt
    INSERT INTO (gcWorkDir+lc_Tmpfl) ;
           (cdefcode,cfld_name,ccode_no, ;
            cdiscrep,crltfield, ;
            cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
            cadd_user,dadd_date,cadd_time,cStatus) ;
    VALUES ('N','CCHRGCODE ',lcCode_no, ;
            "","Y","CTAXCODE  ",;
            "C",laNoTaxRt[1], ;
            gcUser_ID,DATE(),gfGetTime(),"A")
  ENDIF
  
ENDIF
*-- end of lfMRCHDSCD.

*:**************************************************************************
*:* Name        : lfADDENTDT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/23/2005
*:* Purpose     : 
*- if "NADDENTDT" > 0 then add this number to ORDHDR.ENTERED date to calculate the ORDHDR.START
*- if "NADDSTRDT" > 0 then add this number to ORDHDR.START   date to calculate the ORDHDR.COMPLETE
*:***************************************************************************
*:* Called from : lfvAccount IN SOORD.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfADDENTDT
IF laScrMode[4]
  IF CUSTOMER.NADDENTDT > 0
    laData[9] = laData[8] + CUSTOMER.NADDENTDT
    SHOW GET laData[9]
  ENDIF
  IF CUSTOMER.NADDSTRDT > 0
    laData[10] = laData[9] + CUSTOMER.NADDSTRDT
    SHOW GET laData[10]
  ENDIF
ENDIF
*-- end of lfADDENTDT.
*:**************************************************************************
*:* Name        : lfSODIRSV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/23/2004
*:* Purpose     : Save entitlement data when the order is saved
*:***************************************************************************
*:* Called from : SOORD.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfSODIRSV
PRIVATE laStySum,lnI,lnUsed,lnResp,lcAuthSty,llAuthEnt,lcTitle

IF !USED('EMPLOYEE')
  =gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE','SH')
ENDIF
IF !USED('UNIFORM')
  =gfOpenFile(gcDataDir+'UNIFORM','UNIFORM','SH')
ENDIF
IF !USED('STYHIST')
  =gfOpenFile(gcDataDir+'STYHIST','STYHIST','SH')
ENDIF
IF !USED('TRNHIST')
  =gfOpenFile(gcDataDir+'TRNHIST','TRNHIST','SH')
ENDIF

*- check the new user definable field to the Customer File called “Compulsory Customer PO ? "
*- If it is set to Yes then the user cannot  save a Sales Order unless the Customer PO field on the Order Header has been entered.
=SEEK('M'+laData[2],'CUSTOMER')
IF CUSTOMER.LCMPCSTPO 
  IF EMPTY(laData[4])
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not save unless the Customer PO field on the Order Header has been entered.')
    _CUROBJ = OBJNUM(laData[4])
    llCSave = .F.
    RETURN .F.
  ENDIF
ENDIF

*- Check first "Allow auth. of Over Ent. " setup option
*- call password screen for ent. orders only
SELECT &lcOrdLine
GO TOP
IF !EMPTY(&lcOrdLine..EMPLOYEE)

  STORE 0 TO lnClrPos,lnClrLen
  =lfGetClrD()
  
  *- Check Used qty is OK
  =SEEK('M'+laData[2],'CUSTOMER')
  IF CUSTOMER.LCHKENTLM
  
    PRIVATE lcStySum,lcStySum2
    lcStySum = gfTempName()
    lcStySum2= gfTempName()
    SELECT EMPLOYEE,;
           PADR(SUBSTR(STYLE,1,lnMajorLen),19) AS STY , ;
           SUBSTR(STYLE,lnClrPos,lnClrLen) AS CLR , ;
           SUM(TOTQTY) AS TOTQTY , ;
           SUM(TOTQTY*PRICE) AS TOTAMT, ;
           0000.00 AS ORD,000000 AS OLDORD,00000.00 AS OLDAMT,;
           '            ' AS UCODE ;
       FROM &lcOrdLine ;
       GROUP BY EMPLOYEE,STY,CLR ;
       INTO TABLE &gcWorkDir.&lcStySum
    
    SELECT * FROM &lcStySum INTO TABLE (gcWorkDir+lcStySum2)
    INDEX ON EMPLOYEE+STY+CLR TAG &lcStySum
    
    *C123847,4  TMI [Start] 
    IF laScrMode[3]
      PRIVATE lcSvOrd
      SELECT ORDLINE
      lcSvOrd = ORDER('ORDLINE')
      SET ORDER TO ORDLINE
      =SEEK('O'+laData[1],'ORDLINE')
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+laData[1] FOR !EMPTY(EMPLOYEE)
        IF SEEK(EMPLOYEE+PADR(SUBSTR(STYLE,1,lnMajorLen),19)+SUBSTR(STYLE,lnClrPos,lnClrLen) , lcStySum2 )
          SELECT &lcStySum2
          REPLACE OLDORD WITH OLDORD + ORDLINE.TOTQTY ;
                  OLDAMT WITH OLDAMT + ORDLINE.TOTQTY*ORDLINE.PRICE
        ENDIF
      ENDSCAN
      SELECT ORDLINE
      SET ORDER TO TAG &lcSvOrd
    ENDIF
    *C123847,4  TMI [End  ] 

    *- Calculate used+ordered qty's to compare them with entitlement qty
    
    =lfGtUsed()
    
    SELECT &lcStySum
    GO TOP
    lcAuthSty = ' '
    SCAN FOR !EMPTY(UCODE)
*      =SEEK(EMPLOYEE+STY+CLR,'UNIFORM')  
      =SEEK(UCODE+STY+CLR,'UNIFORM')  
      IF ORD > UNIFORM.ENTITLEMNT
        lcAuthSty = lcAuthSty + PADR(&lcStySum..STY,lnMajorLen)+'-'+PADR(&lcStySum..CLR,lnClrLen) + ', '
      ENDIF
    ENDSCAN
    USE IN &lcStySum
    USE IN &lcStySum2
    ERASE (gcWorkDir+lcStySum +'.DBF')
    ERASE (gcWorkDir+lcStySum +'.CDX')
    ERASE (gcWorkDir+lcStySum2+'.DBF')
    ERASE (gcWorkDir+lcStySum2+'.CDX')

    IF !EMPTY(lcAuthSty)
      lnResp = gfModalGen('INM00000B00030',.F.,.F.,.F.,;
                          'The Customer has gone over the entitlement on style(s) &lcAuthSty '+;
                          'please enter user code and password to authorise order.')

      lcTitle = 'Enter user code and password to authorise order.' 
      IF lnResp<>1 .OR. !lfApprvd()
        llCSave = .F.
        RETURN .F.
      ENDIF
    ENDIF

  ENDIF
ENDIF

*-- end of lfSODIRSV.

*:**************************************************************************
*:* Name        : lfChrgOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/28/2004
*:* Purpose     : Charge the order 
*                 also update the styhist file with the used employee units
*:***************************************************************************
*:* Parameters  : llFrmMenu = .T.  : Called from menu pad
*:*                           .F.  : Called from global Save function.
*:***************************************************************************
*C123847,1
FUNCTION lfChrgOrd
PARAMETERS llFrmMenu
PRIVATE lnSlct,lcOrdChg,lcSvOrd, lcCORDCHG,lnClrPos,lnClrLen,;
        lcSty,lcClr,lnUpdFctr,lnLnk1,lnLnk2,lnEntlmnt
lnSlct = SELECT()


IF !llFrmMenu

  *- Color information
  STORE 0 TO lnClrPos,lnClrLen
  =lfGetClrD() 
  
  *- Open files
  IF !USED('STYHIST')
    =gfOpenFile(gcDataDir+'STYHIST','STYHIST','SH')
  ENDIF
  IF !USED('EMPLOYEE')
    =gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE','SH')
  ENDIF
  IF !USED('UNIFORM')
    =gfOpenFile(gcDataDir+'UNIFORM','UNIFORM','SH')
  ENDIF
  
  *- Reset orders
  SET ORDER TO EMPLOYEE IN EMPLOYEE
  SET ORDER TO UNIFORM  IN UNIFORM
  
  *- Update the style history file , only if this is entitlement order ( employee field is not empty ) 
  SELECT &lcOrdLine
  LOCATE
  IF !EMPTY(EMPLOYEE)
    SCAN
      =SEEK(laData[2]+laData[3]+&lcOrdLine..EMPLOYEE,'EMPLOYEE')
      lcSty = PADR(SUBSTR(&lcOrdLine..STYLE,1,lnMajorLen),19)
      lcClr = PADR(SUBSTR(&lcOrdLine..STYLE,lnClrPos,lnClrLen),6)
      lnUpdFctr = 1
      lnEntlmnt = 0
      IF !EMPTY(EMPLOYEE.UCODE)
        =SEEK(EMPLOYEE.UCODE+lcSty+lcClr,'UNIFORM')
        lnEntlmnt = UNIFORM.ENTITLEMNT
        
        DO CASE
        CASE UNIFORM.TYPE = 'V'
          lnUpdFctr = &lcOrdLine..PRICE
        CASE UNIFORM.TYPE = 'P'
          lnUpdFctr = UNIFORM.PNTSVAL
        ENDCASE
      ENDIF
      
      SELECT STYHIST
      IF !SEEK(laData[2]+laData[3]+&lcOrdLine..EMPLOYEE+lcSty+lcClr , 'STYHIST' )
        APPEND BLANK    
        REPLACE ACCOUNT    WITH laData[2] ;
                STORE      WITH laData[3] ;
                EMPLOYEE   WITH &lcOrdline..EMPLOYEE ;
                CSTYMAJOR  WITH lcSty ;
                COLOUR     WITH lcClr ;
                ENTITLEMNT WITH lnEntlmnt
      ENDIF
  
      *- Note:
      *- The Entitlement Used should not be updated until a Packing List has been 
      *- confirmed - (until issue 123851 is completed - you can keep the update in the Sales Order 
      *- as it is now until this is done)
      lnOrgTotQty = 0
      IF laScrMode[3]  && take in considration edit case
        =SEEK(&lcOrdLine..CORDTYPE+&lcOrdLine..ORDER+STR(&lcOrdLine..LINENO,6),'ORDLINE')
        lnOrgTotQty = ORDLINE.TOTQTY
      ENDIF
      REPLACE NUSED    WITH NUSED + (&lcOrdLine..TOTQTY - lnOrgTotQty)*lnUpdFctr 
      IF laScrMode[4]
        REPLACE DLASTORD WITH laData[8] ;
                ORDER    WITH laData[1]
      ENDIF
      =lfAudtFlds()
      
      *:***************************************************************************
      IF !EMPTY(EMPLOYEE.UCODE) 
        =SEEK(EMPLOYEE.UCODE,'UNIFORM')
        SELECT UNIFORM
        SCAN REST WHILE UCODE+CSTYMAJOR+COLOUR = EMPLOYEE.UCODE 
          IF !SEEK(laData[2]+laData[3]+&lcOrdLine..EMPLOYEE+CSTYMAJOR+COLOUR , 'STYHIST' )
            SELECT STYHIST
            APPEND BLANK    
            REPLACE ACCOUNT    WITH laData[2]            ;
                    STORE      WITH laData[3]            ;
                    EMPLOYEE   WITH &lcOrdline..EMPLOYEE ;
                    CSTYMAJOR  WITH UNIFORM.CSTYMAJOR    ;
                    COLOUR     WITH UNIFORM.COLOUR       ;
                    ENTITLEMNT WITH UNIFORM.ENTITLEMNT
            =lfAudtFlds()
          ENDIF
        ENDSCAN
      ENDIF    
      *:***************************************************************************
  
    ENDSCAN
  
  ENDIF

ENDIF

*- =============================== -*- =============================== -*
*- The following lines for charging the order
*- =============================== -*- =============================== -*

lcCORDCHG = ''
=SEEK('M'+laData[2],'CUSTOMER')   &&<<< &&<<< check the key
*- check Charge Carriage user field
IF CUSTOMER.LCHRGCHG .AND. 0 < CUSTOMER.NMINORDCHG .AND. laData[36]<=CUSTOMER.NMINORDCHG
   *- Pick one of the order charges
  
  IF !USED('ORDCHG')
    =gfOpenFile(gcDataDir+'ORDCHG','ORDCHG','SH')
  ENDIF
  
  lcOrdChg = 'm'+SUBSTR(lcfolder,2)
  IF !FILE(gcWorkDir+lcOrdChg+'.DBF')
    CREATE TABLE (gcWorkDir+lcOrdChg) (ORDER C(6),CORDCHG C(6),CDISCREP C(30),NORDCHG N(8,2),CHGACC C(5),STNAME C(30),CUPDATE C(1))
    INDEX ON ORDER+CORDCHG TAG &lcOrdChg
  ENDIF
  IF !USED(lcOrdChg)
    USE (gcWorkDir+lcOrdChg) ORDER &lcOrdChg EXCL IN 0
  ENDIF

  SELECT &lcOrdChg
  LOCATE
  IF ORDER <> laData[1]
    ZAP
  ENDIF
  m.Order = laData[1]
  
  SELECT CODES
  lcSvOrd = ORDER('CODES')

  IF RECCOUNT(lcOrdChg)=0
    SET ORDER TO CCODE_NO     && key :: CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
  
    IF SEEK('N'+'CORDCHG   ')
      DO WHILE CFLD_NAME = 'CORDCHG   '
        m.CORDCHG = CCODE_NO  
        SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'NCORDCHG   '+m.CORDCHG
          IF CRLTFIELD = 'N'
            M.CDISCREP = CODES.CDISCREP 
          ELSE
            lcRlFld = 'm.'+ALLTRIM(CODES.CRLTD_NAM)
            &lcRlFld = IIF(CODES.CRLTD_TYP = 'N' , VAL(CODES.CRLTD_VLU) , CODES.CRLTD_VLU )
          ENDIF
        ENDSCAN
        IF EMPTY(m.CHGACC) .OR. m.CHGACC = laData[2] .AND. !SEEK(m.ORDER+m.CORDCHG,lcOrdChg)
          IF SEEK(m.ORDER+m.CORDCHG,'ORDCHG')
            M.NORDCHG = ORDCHG.NORDCHG
          ENDIF
          INSERT INTO &lcOrdChg FROM MEMVAR
        ENDIF
      ENDDO
    ENDIF
  ENDIF
    
  IF laScrMode[3]
    lcSvOrdHdr = ORDER('ORDHDR')
    SET ORDER TO ORDHDR IN ORDHDR
    SET ORDER TO CODES IN CODES     && CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME
    *- Add to the temp file the charge codes from ordchg file that may not added via codes file
    SELECT ORDCHG
    =SEEK(laData[1],'ORDCHG')
    SCAN REST WHILE ORDER+CORDCHG = laData[1]
      IF !SEEK(ORDCHG.ORDER+ORDCHG.CORDCHG,lcOrdChg)
        =SEEK('O'+ORDCHG.ORDER,'ORDHDR')
        =SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
        =SEEK('NCORDCHG   N','CODES')
  
        SELECT &lcOrdChg
        APPEND BLANK
        REPLACE ORDER    WITH ORDCHG.ORDER    ;
                CORDCHG  WITH ORDCHG.CORDCHG  ;
                CDISCREP WITH CODES.CDISCREP  ;
                NORDCHG  WITH ORDCHG.NORDCHG  ;
                CHGACC   WITH ORDHDR.ACCOUNT  ;
                STNAME   WITH CUSTOMER.STNAME
      ENDIF
    ENDSCAN
    SET ORDER TO &lcSvOrdHdr IN ORDHDR
  ENDIF
    
  *- if there are Order Charges in the Codes file that are specific to that customer – then only 
  *- those should be shown – if not order charges are specific to the customer then all those that 
  *- are not assigned to a customer should be displayed
  SELECT &lcOrdChg
  GO TOP
  LOCATE FOR !EMPTY(&lcOrdChg..CHGACC)
  IF FOUND()
    DELETE FOR EMPTY(&lcOrdChg..CHGACC)
  ENDIF
  
  *- Show the charges assigned to the current account - or not assigned to a specific account
  IF laScrMode[4]
    IF RECCOUNT(lcOrdChg)>0
      lcCORDCHG = lfLocBrow()
      IF !EMPTY(lcCORDCHG)
        INSERT INTO ORDCHG (ORDER,CORDCHG,NORDCHG) VALUES (laData[1],lcCORDCHG,&lcOrdChg..NORDCHG)
      ENDIF
    ENDIF
    
  ELSE
    IF llFrmMenu    
      IF RECCOUNT(lcOrdChg)>0
        SELECT &lcOrdChg
        SET RELATION TO laData[1]+CORDCHG INTO ORDCHG
  
        m.CORDCHG = '      '
        m.NORDCHG = 0
        lcTitle = 'Order Charges'
        PUSH KEY
        ON KEY LABEL ESC lDummy = .T.
        
        =lfvBrOrChg()   && start by browsing available charges
        IF !EMPTY(m.CORDCHG )        
          DO (gcScrDir + gcWinAppl + '\SOORDCHG.SPR') 
        ENDIF
        
        ON KEY
        POP KEY
      ELSE
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No charge codes available for the account:'+laData[2])
      ENDIF
    ELSE
      SELECT &lcOrdChg
      LOCATE
      SCAN FOR CUPDATE <> ' '
        DO CASE
        CASE &lcOrdChg..CUPDATE = 'U'
          IF !SEEK(&lcOrdChg..ORDER+&lcOrdChg..CORDCHG,'ORDCHG')
            SELECT ORDCHG
            APPEND BLANK
            REPLACE ORDER     WITH laData[1] ;
                    CORDCHG   WITH &lcOrdChg..CORDCHG
          ENDIF
          SELECT ORDCHG
          REPLACE NORDCHG   WITH &lcOrdChg..NORDCHG   

        CASE &lcOrdChg..CUPDATE = 'D'
          =SEEK(&lcOrdChg..ORDER+&lcOrdChg..CORDCHG,'ORDCHG')
          SELECT ORDCHG
          DELETE

        ENDCASE
      ENDSCAN
    ENDIF
  ENDIF
  
  USE IN &lcOrdChg
  IF !llFrmMenu
    ERASE (gcWorkDir+lcOrdChg+'.DBF')
    ERASE (gcWorkDir+lcOrdChg+'.CDX')
  ENDIF
  SELECT CODES
  SET ORDER TO &lcSvOrd
ELSE
  IF llFrmMenu
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No charges applied to this order.')
  ENDIF
ENDIF
  
SELECT (lnSlct)
*-- end of lfChrgOrd.

*:**************************************************************************
*:* Name        : lpTrap
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/26/2005
*:* Purpose     : Trap fuction for Deactivate
*:***************************************************************************
*:* Called from : soordchg screen
*:***************************************************************************
FUNCTION lpTrap
IF WONTOP() <> "SOORDCHG"
  _CUROBJ = OBJNUM(m.CORDCHG)
ENDIF  
*-- end of lpTrap.

*:**************************************************************************
*:* Name        : lfvBrOrChg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/29/2004
*:* Purpose     : Browse order charges
*:***************************************************************************
*:* Called from : lfChrgOrd IN DIRMAIN.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfvBrOrChg
PRIVATE laTemp, lcFieldsNam

SELECT &lcOrdChg
GO TOP

DIMENSION laTemp[2]
laTemp = ''
lcFieldsNam = 'CORDCHG,NORDCHG'
lcBrFields = "CORDCHG  :R:H='Order Charge Code',"+;
             "CDISCREP :R:H='Description'      ,"+;
             "NORDCHG  :H='Order Charge',"+;
             "Added    =IIF(EOF('ORDCHG'),'Not Added','Added'),"+;
             "Updated  =IIF(CUPDATE='U','Updated',IIF(CUPDATE='D','Deleted',''))"
PUSH KEY
ON KEY
PRIVATE lcFile_ttl,lcWinTitl
=Ariabrow('','Order Charges',gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
             lcFieldsNam,'laTemp')
POP KEY
m.CORDCHG = laTemp[1]
m.NORDCHG = laTemp[2]

SHOW GET m.CORDCHG
SHOW GET m.NORDCHG
=lfvOrdChCd()

*-- end of lfvBrOrChg.

*:**************************************************************************
*:* Name        : lfvUpOrChg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/29/2004
*:* Purpose     : Update order charges
*:***************************************************************************
*C123847,1
FUNCTION lfvUpOrChg
IF !EMPTY(m.CORDCHG) .AND. SEEK(laData[1]+m.CORDCHG,lcOrdChg)
  SELECT &lcOrdChg
  REPLACE NORDCHG WITH M.NORDCHG ;
          CUPDATE WITH 'U'

  M.CORDCHG = ''
  M.NORDCHG = 0
  SHOW GET M.CORDCHG
  SHOW GET M.NORDCHG

  SHOW GET pbBrow ENABLE
  SHOW GET pbUpd  DISABLE
  SHOW GET pbDel  DISABLE
ENDIF
*-- end of lfvUpOrChg.

*:**************************************************************************
*:* Name        : lfvDlOrChg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/29/2004
*:* Purpose     : Delete a selected order charge
*:***************************************************************************
*C123847,1
FUNCTION lfvDlOrChg
IF !EMPTY(m.CORDCHG) .AND. SEEK(laData[1]+m.CORDCHG,lcOrdChg)
  SELECT &lcOrdChg
  REPLACE CUPDATE WITH 'D'

  M.CORDCHG = ''
  M.NORDCHG = 0
  SHOW GET M.CORDCHG
  SHOW GET M.NORDCHG

  SHOW GET pbBrow    ENABLE
  SHOW GET pbUpd     DISABLE
  SHOW GET pbDel     DISABLE
ENDIF  

*-- end of lfvDlOrChg.

*:**************************************************************************
*:* Name        : lfvOrdChCd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/29/2004
*:* Purpose     : Valid fn. for order charge code
*:***************************************************************************
*C123847,1
FUNCTION lfvOrdChCd
PRIVATE lcDelStat
IF !EMPTY(m.CORDCHG) .AND. !SEEK(laData[1]+m.CORDCHG,lcOrdChg)
  =lfvBrOrChg() 
ENDIF
IF !EMPTY(m.CORDCHG)
*  SHOW GET m.CORDCHG DISABLE
*  m.NORDCHG = IIF(EOF('ORDCHG'),&lcOrdChg..NORDCHG,ORDCHG.NORDCHG) 
  m.NORDCHG = &lcOrdChg..NORDCHG
  lcDelStat = IIF(EOF('ORDCHG'),'DISABLE','ENABLE') 
  lcUpdStat = IIF(EOF('ORDCHG'),'ENABLE','DISABLE') 
  SHOW GET m.NORDCHG 
  SHOW GET pbBrow DISABLE
  SHOW GET pbUpd  &lcUpdStat
  SHOW GET pbDel  &lcDelStat
ENDIF
*-- end of lfvOrdChCd.

*:**************************************************************************
*:* Name        : lfvOrdChAm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/29/2004
*:* Purpose     : Valid function for order charge amount
*:***************************************************************************
*:* Called from : SOORDCHG screen
*:***************************************************************************
*C123847,1
FUNCTION lfvOrdChAm
PRIVATE lcDelStat
IF m.NORDCHG < 0
  m.NORDCHG = lcOldValue
ENDIF
IF m.NORDCHG <> lcOldValue
  lcDelStat = IIF(EOF('ORDCHG'),'DISABLE','ENABLE') 
  SHOW GET pbBrow DISABLE
  SHOW GET pbUpd  ENABLE
  SHOW GET pbDel  &lcDelStat
ENDIF

*-- end of lfvOrdChAm.
 
*:**************************************************************************
*:* Name        : lfINVEMP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/16/2005
*:* Purpose     : Update the &lcInvline..EMPLOYEE field to use finally to update
*                 the trnhist file with the invoice
*:***************************************************************************
*:* Called from : lfGetOrder IN ARIINV.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfINVEMP
PRIVATE lnSlct
lnSlct = SELECT()

SELECT &lcInvLine
REPLACE EMPLOYEE WITH M.EMPLOYEE

SELECT (lnSlct)
*-- end of lfUPDEMP.
*:**************************************************************************
*:* Name        : lfINVCHARG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/19/2005
*:* Purpose     : Update invoice charges for DIR03
*:***************************************************************************
*:* Called from : ARINV.PRG
*:***************************************************************************
*C123847,1  
FUNCTION lfINVCHARG

lnSlct = SELECT()
IF !USED('ORDCHG')
  =gfOpenFile(gcDataDir+'ORDCHG','ORDCHG','SH')
ENDIF

*- Order charges are added only to the first invoice ( in case if the order is shipped via more than one invoice )
IF SEEK(M.ORDER,'ORDCHG') .AND. !ORDCHG.INVOICED
  SELECT ORDCHG
  SCAN REST WHILE ORDER+CORDCHG = M.ORDER
    REPLACE INVOICED WITH .T.
  ENDSCAN
ENDIF
*-- end of lfINVCHARG.

*:**************************************************************************
*:* Name        : lfUPALLAMT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/21/2005
*:* Purpose     : Update the total charges variable "lnAllAmnt" with the new added Charges for DIR03
*:***************************************************************************
*:* Called from : lfBatchSum IN ARIINV.PRG
*:***************************************************************************
FUNCTION lfUPALLAMT
lnSlct = SELECT()
IF !USED('ORDCHG')
  =gfOpenFile(gcDataDir+'ORDCHG','ORDCHG','SH')
ENDIF

IF !EMPTY(laData[1]) .AND. SEEK(laData[1],'ORDCHG') .AND. !ORDCHG.INVOICED
  =lfUPDIRCHG()
  
  */*  =SEEK(laData[1],'ORDCHG') 
  */*  SELECT ORDCHG
  */*  SCAN REST WHILE ORDER+CORDCHG = laData[1]
  */*    lnAllAmnt = lnAllAmnt + ordchg.nordchg
  */*  ENDSCAN
ENDIF

*-- end of lfUPALLAMT.

*:**************************************************************************
*:* Name        : lfRMVTMP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2005
*:* Purpose     : Remove the temp file (gcWorkDir+lcInvHdr+'.CHG')
*:***************************************************************************
*:* Called from : gpCharges IN ARINV.PRG
*:***************************************************************************
FUNCTION lfRMVTMP
PRIVATE lnSlct
lnSlct = SELECT()

ERASE (gcWorkDir+lcInvHdr+'.CHG')
SELECT &lcInvHdr
LOCATE

SELECT (lnSlct)
*-- end of lfRMVTMP.

*:**************************************************************************
*:* Name        : lfDIRCHG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/21/2005
*:* Purpose     : browse the new added charges for DIR03
*:***************************************************************************
*:* Called from : lfActPad in ARIINV.PRG
*:***************************************************************************
FUNCTION lfDIRCHG
lnBarNo = CNTBAR('_INQURYPOP') + 1
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT 'Added Charges for DCC' SKIP FOR laScrMode[1] .OR. EMPTY(laData[1])
ON SELECTION BAR lnBarNo   OF _INQURYPOP DO lfSWDIRCHG IN DIRMAIN.PRG 

*-- end of lfDIRCHG.

*:**************************************************************************
*:* Name        : lfUPDIRCHG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2005
*:* Purpose     : Update charges from ORDCHG file for DIR03 when selecting Charges folder
*:***************************************************************************
*:* Called from : lfActFolder in ARIINV.PRG
*:***************************************************************************
*C123847,4  TMI
FUNCTION lfUPDIRCHG
PRIVATE lnSlct,lcTaxCode,lcGlAccnt,laRltFld,lnTaxRate
lnSlct = SELECT()

DIMENSION laRltFld[2,2],;
          laRltFld2[1,2]
laRltFld[1,1] = 'CTAXCODE'
laRltFld[1,2] = 'lcTaxCode'
laRltFld[2,1] = 'CFRGTACNT'
laRltFld[2,2] = 'lcGlAccnt'

laRltFld2[1,1] = 'NTAXRATE'
laRltFld2[1,2] = 'lnTaxRate'

IF !USED('ORDCHG')
  =gfOpenFile(gcDataDir+'ORDCHG','ORDCHG','SH')
ENDIF

*- Order charges are added only to the first invoice ( in case if the order is shipped via more than one invoice )

IF SEEK(laData[1],'ORDCHG') .AND. !ORDCHG.INVOICED
  SELECT ORDCHG
  SCAN REST WHILE ORDER+CORDCHG = laData[1]
    *key :: ORDER+CSTORE+PIKTKT+CCHRGCODE
    IF !SEEK(laData[1]+laData[5]+laData[3]+ORDCHG.CORDCHG,lcEngChrg)
      *C123847,5  TMI [Start] get related fields values of the order charge "ORDCHG.CORDCHG"
      lcGlAccnt = ' '
      lcTaxCode = ' '
      =gfRltFld(ORDCHG.CORDCHG,@laRltFld,'CCHRGCODE')

      lnTaxRate = 0
      =gfRltFld(lcTaxCode,@laRltFld2,'CTAXCODE') 
      *C123847,5  TMI [End  ] 
      
      SELECT &lcEngChrg
      APPEND BLANK
      REPLACE ORDER     WITH laData[1]       ;
              PIKTKT    WITH laData[3]       ;
              CCHRGCODE WITH ORDCHG.CORDCHG  ;
              NCHRGAMNT WITH ORDCHG.NORDCHG  ;
              CFRGTACNT WITH lcGlAccnt       ;
              NTAXRATE  WITH lnTaxRate       
      m.NCHARGES = m.NCHARGES + ORDCHG.NORDCHG
      m.TAX_AMT  = m.TAX_AMT  + NCHRGAMNT * NTAXRATE
      SAVE TO (gcWorkDir+lcInvHdr+'.CHG') ALL LINE m.NCHARGES
    ENDIF  
  ENDSCAN
  IF FILE(gcWorkDir+lcInvHdr+'.CHG')
    =lfvCharges()
  ENDIF
 
ENDIF

SELECT (lnSlct)
*-- end of lfUPDIRCHG.

*:**************************************************************************
*:* Name        : lfSWDIRCHG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/21/2005
*:* Purpose     : Browse the added charges for DIR03
*:***************************************************************************
FUNCTION lfSWDIRCHG
PRIVATE lnSlct,lcDirChg,laStru,lnLen,lnSer,lnTotAmt,llShow
lnSlct = SELECT()

lcDirChg = gfTempName()

IF !USED('ORDCHG')
  =gfOpenFile(gcDataDir+'ORDCHG','ORDCHG','SH')
ENDIF
SELECT ORDCHG
=AFIELDS(laStru)
lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'DESC'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 20
laStru[lnLen,4] = 0

lnLen = ALEN(laStru,1) + 1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'SER'
laStru[lnLen,2] = 'N'
laStru[lnLen,3] = 2
laStru[lnLen,4] = 0

CREATE CURSOR &lcDirChg FROM ARRAY laStru
INDEX ON PADL(SER,2) TAG &lcDirChg

llShow = .F.
m.Ser = 0
lnTotAmt = 0
SELECT ORDCHG
IF SEEK(laData[1],'ORDCHG') .AND. !ORDCHG.INVOICED
  m.DESC = ''
  llShow = .T.
  SCAN REST WHILE ORDER+CORDCHG = laData[1]
    SCATTER MEMVAR
    m.Ser = m.Ser + 1
    lnTotAmt = lnTotAmt + ORDCHG.NORDCHG
    INSERT INTO &lcDirChg FROM MEMVAR
  ENDSCAN
ENDIF

m.order = '------'
m.cordchg = '------'
m.nordchg = 0
m.Ser = m.Ser + 1
INSERT INTO &lcDirChg FROM MEMVAR

m.Desc = 'Total Added Charges'
m.order = ''
m.cordchg = ''
m.nordchg = lnTotAmt
m.Ser = m.Ser + 1
INSERT INTO &lcDirChg FROM MEMVAR

m.Desc = 'Total Amount'
m.order = ''
m.cordchg = ''
m.nordchg = lnTotAmt+m.TOTALCHG
m.Ser = m.Ser + 1
INSERT INTO &lcDirChg FROM MEMVAR

lcBrFields  = "Desc                           :H=''                    , " +;
              "CORDCHG                        :H='Order Charge'        , " +;
              "nChg=IIF(NORDCHG=0,'',NORDCHG) :H='Value' :P='9999999.99' "

SELECT &lcDirCHg
LOCATE
IF llShow
  =ARIABROW([''],'Added Charges for DCC.',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2) 
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Added Charges for this invoice.')
ENDIF  

USE IN &lcDirChg

SELECT (lnSlct)
*-- end of lfSWDIRCHG.

*:**************************************************************************
*:* Name        : lfORDCHARG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2005
*:* Purpose     : Update the field ORDCHG.INVOICED  field for DIR03 
*                 Also add a line to TRNHIST.DBF file with the current invoice
*:***************************************************************************
*:* Called from : gpSaveInv IN ARINV.PRG
*:***************************************************************************
*C123847,1
*C123847,4  TMI [Start] rename the function with a more descriptive name
*FUNCTION lfORDCHARG
FUNCTION lfTRNHIST
*C123847,4  TMI [End  ] 
PRIVATE lnSlct

lnSlct = SELECT()

*- Add a line to TRNHIST.DBF file with the current invoice
*- key :: ACCOUNT+STORE+EMPLOYEE+STYLE
IF !USED('TRNHIST')
*  =gfOpenFile(gcDataDir+'TRNHIST','TRNHIST','SH')
  =gfOpenFile(gcDataDir+'TRNHIST','TRANTYPE','SH')
ENDIF
SET ORDER TO TRANTYPE IN TRNHIST

SELECT &lcInvHdr
LOCATE

SELECT &lcInvLine
SCAN
  =SEEK('O'+ORDER+STR(LINENO,6), 'ORDLINE' )
  IF !EMPTY(ORDLINE.EMPLOYEE)
    *IF !SEEK(ACCOUNT+STORE+ORDLINE.EMPLOYEE+STYLE,'TRNHIST')
    IF !SEEK('I'+&lcInvHdr..INVOICE+ORDLINE.EMPLOYEE+STYLE,'TRNHIST')
      SELECT TRNHIST
      APPEND BLANK
    ENDIF
    SELECT TRNHIST
    REPLACE TRANTYPE WITH 'I'                 ;
            TRANNO   WITH &lcInvHdr..INVOICE  ;
            DTRANDT  WITH &lcInvHdr..INVDATE  ;
            ACCOUNT  WITH &lcInvLine..ACCOUNT ;
            STORE    WITH &lcInvLine..STORE   ;
            EMPLOYEE WITH ORDLINE.EMPLOYEE    ;
            STYLE    WITH &lcInvLine..STYLE   ;
            QTY1     WITH &lcInvLine..QTY1    ;
            QTY2     WITH &lcInvLine..QTY2    ;
            QTY3     WITH &lcInvLine..QTY3    ;
            QTY4     WITH &lcInvLine..QTY4    ;
            QTY5     WITH &lcInvLine..QTY5    ;
            QTY6     WITH &lcInvLine..QTY6    ;
            QTY7     WITH &lcInvLine..QTY7    ;
            QTY8     WITH &lcInvLine..QTY8    ;
            TOTQTY   WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
    =lfAudtFlds()
  ENDIF
ENDSCAN

SELECT (lnSlct)

*-- end of lfORDCHARG.

*:**************************************************************************
*:* Name        : lfLocBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/13/2004
*:* Purpose     : A local browse one can edit some fields in it
*:***************************************************************************
*C123847,1
FUNCTION lfLocBrow

PRIVATE lcBrTtl,lcBrFields,lcBrWinName,lnWinWidth, lnWinHight, lcRet 

lcBrTtl    = 'Charges'
lcBrFields = "CORDCHG  :R:H='Order Charge Code',"+;
             "CDISCREP :R:H='Description'      ,"+;
             "NORDCHG    :H='Order Charge'     "

lcBrWinName = gfTempName()
  
lnWinWidth = 100
lnWinHight = 20
  
DEFINE WINDOW (lcBrWinName);
       AT 0, 0 SIZE lnWinHight, lnWinWidth;
       FONT "MS Sans Serif", 9 ;
       FLOAT ;
       NOCLOSE ;
       SHADOW ;
       NOMINIMIZE ;
       SYSTEM ;
       COLOR SCHEME 10  	   
MOVE WINDOW (lcBrWinName) CENTER
PUSH KEY
ON KEY
ON KEY LABEL ENTER DEACTIVATE WINDOW (lcBrTtl)
*C123847,5  TMI [Start] trap the case ESC key is pressed
ON KEY LABEL ESC   DEACTIVATE WINDOW (lcBrTtl)
*C123847,5  TMI [End  ]   

GO TOP
*C123847,5  TMI [Start] use the contents of the varialbe lcBrTTl not itself
*BROWSE FIELDS &lcBrFields ;
       WINDOW (lcBrWinName);
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NODELETE;
       TITLE lcBrTtl
BROWSE FIELDS &lcBrFields ;
       WINDOW (lcBrWinName);
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NODELETE;
       TITLE (lcBrTtl)
*C123847,5  TMI [End  ]          
RELEASE WINDOW (lcBrWinName)

ON KEY
POP KEY

lcRet = IIF( LASTKEY() = 27 , '' , &lcOrdChg..CORDCHG ) 
RETURN lcRet 
*-- end of lfLocBrow.

*:**************************************************************************
*:* Name        : lfGtUsed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/23/2004
*:* Purpose     : Get used qty for linked styles
*:***************************************************************************
*C123847,1
FUNCTION lfGtUsed
PRIVATE lnUsed,lcUcode,laUsedSum,laLnk,lcType,lnTotOrdUsd

*- Get used qty's from STYHIST file if exists
SELECT &lcStySum
SCAN
  
  =SEEK(laData[2]+laData[3]+&lcStySum..EMPLOYEE,'EMPLOYEE')
  IF !EMPTY(EMPLOYEE.UCODE)  
    REPLACE UCODE WITH EMPLOYEE.UCODE

    =SEEK(EMPLOYEE.UCODE+&lcStySum..STY+&lcStySum..CLR,'UNIFORM')    

    lnUsed = 0
    *- The case of linked styles, sum the used qty for all linked together
    RELEASE laLnk

    IF UNIFORM.NLINK1 + UNIFORM.NLINK2 > 0
  
      SELECT UNIFORM
      SCATTER FIELDS NLINK1,NLINK2 TO laLnk
      
      =SEEK(EMPLOYEE.UCODE,'UNIFORM')
      SCAN REST WHILE UCODE+CSTYMAJOR+COLOUR = EMPLOYEE.UCODE ;
                  FOR BETWEEN(NLINE,laLnk[1],laLnk[2])
        =SEEK(laData[2]+laData[3]+&lcStySum..EMPLOYEE+UNIFORM.CSTYMAJOR+SUBSTR(UNIFORM.COLOUR,1,lnClrLen) ,'STYHIST')
        lnUsed = lnUsed + lfUsed()
      ENDSCAN

    ELSE
  
      =SEEK(laData[2]+laData[3]+&lcStySum..EMPLOYEE+&lcStySum..STY+&lcStySum..CLR,'STYHIST')
      lnUsed = lfUsed()
  
    ENDIF
    
    SELECT &lcStySum
    REPLACE ORD WITH lnUsed
    
  ENDIF  
  
ENDSCAN

*-- end of lfGtUsed.
*:**************************************************************************
*:* Name        : lfUsed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/27/2004
*:* Purpose     : Used qty depending on type
*:***************************************************************************
*C123847,1
FUNCTION lfUsed
SELECT &lcStySum2
LOCATE
=SEEK(EMPLOYEE.EMPLOYEE+UNIFORM.CSTYMAJOR+SUBSTR(UNIFORM.COLOUR,1,lnClrLen),lcStySum2)

DO CASE
CASE UNIFORM.TYPE = 'U'
*  RETURN &lcStySum2..TOTQTY + STYHIST.NUSED
  RETURN (TOTQTY-OLDORD) + STYHIST.NUSED

CASE UNIFORM.TYPE = 'P'
*  RETURN &lcStySum2..TOTQTY*UNIFORM.PNTSVAL + STYHIST.NUSED
  RETURN (TOTQTY-OLDORD)*UNIFORM.PNTSVAL + STYHIST.NUSED

CASE UNIFORM.TYPE = 'V'
*  RETURN &lcStySum2..TOTAMT + STYHIST.NUSED
  RETURN (TOTAMT-OLDAMT) + STYHIST.NUSED

ENDCASE
*-- end of lfUsed.

*:**************************************************************************
*:* Name        : lfApprvd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/23/2004
*:* Purpose     : Get used qty for linked styles
*:***************************************************************************
*C123847,1
FUNCTION lfApprvd
PRIVATE llApprvd,llPwChk,llAuth
llApprvd = .F.
llAuth = .F.
DO WHILE !llAuth
  m.User_ID  = SPACE(10) 
  m.Password = SPACE(8)
  DO (gcScrDir + gcWinAppl + '\SOPASWRD.SPR')
  IF llApprvd
    =SEEK(m.User_ID,'SYUUSER')
    IF SYUUSER.AUTHENTL = 'Y'
      EXIT
    ELSE
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This user is not allowed to authorize over entitlement.')
      LOOP
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO  
RETURN llApprvd
*-- end of lfApprvd.

*:**************************************************************************
*:* Name        : lfwPW
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/30/2004
*:* Purpose     : When function to hide entring the password characters
*:***************************************************************************
*C123847,1
FUNCTION lfwPW
SET CONFIRM Off

    lnYpos    = 3.843
    lnXpos    = 15.395
    lnDypos   = 3.843
    lnDxPos   = 15.395
    @ lnDypos,lnDxPos+0.300 SAY REPLICATE(CHR(219),8) COLOR N/N;
         FONT "FOXFONT",9 STYLE "N"

lcPass    =' '
lcPassWrd =''

DO WHILE LEN(lcPassWrd) < 8 
  @ lnYpos,lnXpos GET lcPass  SIZE 1.000,1 PICTURE "@!" COLOR ,N/N;
      FONT "FOXFONT",9 STYLE "N"
      READ COLOR ,N/N
  
  IF INLIST(LASTKEY(),13,9,15)  OR ASC(lcPass) < 48 OR  ASC(lcPass) > 90   
    EXIT
  ENDIF  
  lcPassWrd = lcPassWrd+lcPass
  lcPass = ' '

  lnXpos = lnXpos + 1.34
  @ lnDypos ,lnDxPos SAY REPLICAT('!',LEN(lcPassWrd)) ;
         FONT "FOXFONT",9 STYLE "N" COLOR &gcReadClr
ENDDO    

IF INLIST(LASTKEY(),13,9,15) 
  IF !EMPTY(lcPassWrd) 
*    m.Password = SYS(2007,ALLTRIM(lcPassWrd))
    m.Password = lcPassWrd
    lcPassWrd = REPLICAT("*",8)
 ELSE
    IF !EMPTY(laData[3])
      lcPassWrd = REPLICAT("*",8)
    ENDIF
  ENDIF  
ELSE
  IF EMPTY(m.Password)
    lcPassWrd = SPACE(8)
  ELSE
    lcPassWrd = REPLICAT("*",8)
  ENDIF
ENDIF

SET CONFIRM ON
lnLastKey=LASTKEY()
KEYBOARD CHR(13)

*-- end of lfwPW.

*:**************************************************************************
*:* Name        : lfvPwcan
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/23/2004
*:* Purpose     : Cancel
*:***************************************************************************
*:* Called from : SOPASWRD.SCX
*:***************************************************************************
*C123847,1
FUNCTION lfvPwChk

SHOW GET pbOk DISABLE
llApprvd = .F.

IF SEEK(m.User_ID,'SYUUSER') .AND. SYUUSER.CUSR_PASS = SYS(2007,ALLTRIM(m.Password))
  SHOW GET pbOk ENABLE
  llApprvd = .T.
ENDIF
llPwChk = .F.
*-- end of lfvPwChk.

*:**************************************************************************
*:* Name        : lfvPwcan
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/23/2004
*:* Purpose     : Cancel
*:***************************************************************************
*:* Called from : SOPASWRD.SCX
*:***************************************************************************
*C123847,1
FUNCTION lfvPwcan
llApprvd = .F.
*-- end of lfvPwcan.


*:**************************************************************************
*:* Name        : lfSELEMPL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/28/2004
*:* Purpose     : Define bar "Select Employee"
*:***************************************************************************
*:* Called from : lfActPad in RMRTATH.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfSELEMPL
PRIVATE lnBarNo
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT '\<Select Employee' ;
       SKIP FOR laScrMode[1] .OR. laScrMode[2] .OR. lnActFolder<>2
ON SELECTION BAR lnBarNo OF _OPTIONPOP ;
             DO lfEmplLst IN DIRMAIN.PRG

*-- end of lfSELEMPL.

*:**************************************************************************
*:* Name        : lfEmplLst
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/28/2004
*:* Purpose     : List of all employees who paurchased the selected style
*                 so that when the  RA is turned into a Credit Memo a new entry is written to 
*                 the Employee History File
*:***************************************************************************
*:* Called from : lfSELEMPL
*:***************************************************************************
*C123847,1
FUNCTION lfEmplLst
PRIVATE lnSlct,lcSty,lnClrLen,lnClrPos,lcTmpEmp,lcEmp;
        lcBrTtl,lcFieldsNam,lcBrFields,laTemp,lcEmployee
lnSlct = SELECT()        

*- get a list of all Employees who purchased that style on the selected Sales order
IF EMPTY(laData[11])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No sales order/invoice is selected.')
  RETURN
ENDIF
IF EMPTY(&lcTmpRetLn..STYLE)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No sales style is selected.')
  RETURN
ENDIF

STORE 0  TO lnClrLen, lnClrPos
=lfGetClrD()

IF !USED('TRNHIST')
  =gfOpenFile(gcDataDir+'TRNHIST','TRANTYPE','SH')
ENDIF
IF !USED('EMPLOYEE')
  =gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE','SH')
ENDIF
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINST','SH')
ENDIF

SET ORDER TO TRANTYPE IN TRNHIST
SET ORDER TO ORDLINST IN ORDLINE

lcTmpEmp = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpEmp) (EMPLOYEE C(12),EMPNAME C(30))
INDEX ON EMPLOYEE TAG &lcTmpEmp 

IF SEEK('O'+laData[11]+laData[3]+&lcTmpRetLn..STYLE,'ORDLINE')
  SELECT ORDLINE
  SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+laData[11]+laData[3]+&lcTmpRetLn..STYLE ;
              FOR !EMPTY(ORDLINE.EMPLOYEE)
    IF !SEEK(ORDLINE.EMPLOYEE,lcTmpEmp)
      =SEEK(laData[2]+laData[3]+ORDLINE.EMPLOYEE,'EMPLOYEE')
      INSERT INTO &lcTmpEmp VALUES (EMPLOYEE.EMPLOYEE,EMPLOYEE.EMPNAME)
    ENDIF
    
  ENDSCAN
ENDIF
    
IF RECCOUNT(lcTmpEmp) > 0
    
    DIMENSION laTemp[1]
    laTemp = ' '
    lcFieldsNam = 'EMPLOYEE'
    lcBrFields  = "EMPLOYEE  :H='Employee Code' ,"+;
                  "EMPNAME   :H='Employee Name'  "
    PUSH KEY
    ON KEY
    lcBrTtl = 'Employees purchased style '+ &lcTmpRetLn..STYLE + ' in the order:'+laData[11]
    SELECT &lcTmpEmp
    =Ariabrow('',lcBrTtl,gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
                 lcFieldsNam,'laTemp')
    POP KEY
    
    SELECT &lcTmpRetLn
    REPLACE EMPLOYEE WITH laTemp[1]
    
ELSE
    
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Employee had purchased this style.')
  
ENDIF                                                                                                                               
  
*- Remove the created temp file
USE IN &lcTmpEmp
ERASE (gcWorkDir+lcTmpEmp+'.DBF')
ERASE (gcWorkDir+lcTmpEmp+'.CDX')

SELECT (lnSlct)
*-- end of lfEmplLst.

*:**************************************************************************
*:* Name        : lfUPEMPFLD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/28/2004
*:* Purpose     : Update employee field from raline.employee files
*:***************************************************************************
*:* Called from : lfgetraln() IN rmcrmem.prg
*:***************************************************************************
*C123847,1
FUNCTION lfUPEMPFLD
REPLACE EMPLOYEE WITH RALINE.EMPLOYEE
*-- end of lfUPEMPFLD.

*:**************************************************************************
*:* Name        : lfEMPHST
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/28/2004
*:* Purpose     : reduce the STYHIST.used qty for an employee for DIR03 
*                 also update the TrnHist file
*:***************************************************************************
*:* Called from : lfCMSav IN RMSAVE.PRG (called from :lpsavscr IN RMCRMEM)
*:***************************************************************************
*C123847,1
FUNCTION lfEMPHST
PRIVATE lnSlct,lnClrLen, lnClrPos,lnI,lcI,lnMajorLen
lnSlct = SELECT()

IF !USED('STYHIST')
  =gfOpenFile(gcDataDir+'STYHIST','STYHIST','SH')
ENDIF
IF !USED('TRNHIST')
  =gfOpenFile(gcDataDir+'TRNHIST','TRANTYPE','SH')
ENDIF
IF !USED('UNIFORM')
  =gfOpenFile(gcDataDir+'UNIFORM','UNIFORM','SH')
ENDIF
IF !USED('EMPLOYEE')
  =gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE','SH')
ENDIF

STORE 0  TO lnClrPos,lnClrLen
=lfGetClrD()

SELECT &lcCrMemLin
IF QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8>0 .AND. !EMPTY(&lcCrMemLin..EMPLOYEE)

  *- Update the TRNHIST file
  SELECT TRNHIST
  IF !SEEK('C'+&lcCrMemHdr..CRMEMO+&lcCrMemLin..EMPLOYEE+&lcCrMemLin..STYLE,'TRNHIST')
    APPEND BLANK
    REPLACE TRANTYPE WITH 'C' ;
            TRANNO   WITH &lcCrMemHdr..CRMEMO ;
            ACCOUNT  WITH laData[2] ;
            STORE    WITH laData[4] ;
            EMPLOYEE WITH &lcCrMemLin..EMPLOYEE ;
            STYLE    WITH &lcCrMemLin..STYLE ;
            DTRANDT  WITH laData[5]
  ENDIF
  REPLACE QTY1 WITH &lcCrMemLin..QTY1 ;
          QTY2 WITH &lcCrMemLin..QTY2 ;
          QTY3 WITH &lcCrMemLin..QTY3 ;
          QTY4 WITH &lcCrMemLin..QTY4 ;
          QTY5 WITH &lcCrMemLin..QTY5 ;
          QTY6 WITH &lcCrMemLin..QTY6 ;
          QTY7 WITH &lcCrMemLin..QTY7 ;
          QTY8 WITH &lcCrMemLin..QTY8 ;
          TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
  
  =lfAudtFlds()
  
  *-Update the STYHIST file
  lnMajorLen = LEN(gfItemMask("PM"))
  lcSty = PADR(SUBSTR(&lcCrMemLin..STYLE,1,lnMajorLen),19)
  lcClr = PADR(SUBSTR(&lcCrMemLin..STYLE,lnClrPos,lnClrLen ),6)

  IF SEEK(laData[2]+laData[4]+&lcCrMemLin..EMPLOYEE+lcSty+lcClr,'STYHIST')
    
    SELECT STYHIST
    
    *- Reduce the used qty with the returned styles
    =SEEK(laData[2]+laData[4]+&lcCrMemLin..EMPLOYEE,'EMPLOYEE')
    =SEEK(EMPLOYEE.UCODE+lcSty+lcClr,'UNIFORM')
    lnFactor = 1
    DO CASE
    CASE UNIFORM.TYPE = 'V'
      lnFactor = PRICE
    CASE UNIFORM.TYPE = 'P'
      lnFactor = UNIFORM.PNTSVAL
    ENDCASE
      
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      REPLACE NUSED WITH NUSED - &lcCrMemLin..QTY&lcI*lnFactor
    ENDFOR 
    
    =lfAudtFlds()
  
  ENDIF

ENDIF
  
SELECT (lnSlct)
*-- end of lfEMPHST.

*:**************************************************************************
*:* Name        : lfBRWSVAR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/03/2005
*:* Purpose     : Add the Employee code between Style and Group for Dir03 in the ordline browse
*:***************************************************************************
*:* Called from : SOORD.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfBRWSVAR
PRIVATE lnStyPos,lcBr1,lcBr2,lnComma
IF 'STYLE' $ UPPER(lcOrderBr) .AND. !'EMPLOYEE' $ UPPER(lcOrderBr)
  lnStyPos = AT('STYLE',lcOrderBr)
  lcBr1 = SUBSTR(lcOrderBr,1,lnStyPos-1)
  lcBr2 = SUBSTR(lcOrderBr,lnStyPos)  
  lnComma = AT(',' , lcBr2)
  lcBr2 = STUFF(lcBr2,lnComma+1,0,[EMPLOYEE :H='Employee' :R:12,])
  lcOrderBr = lcBr1+lcBr2
ENDIF
*-- end of lfBRWSVAR.

*:**************************************************************************
*:* Name        : lfUSROPTN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/04/2005
*:* Purpose     : Add "Option" pad to Users system screen via a trigger for 
*                 DIR03 to update user field "SYUUSER.AUTHENTL"
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfUSROPTN
PRIVATE laUsr_lvl

DIMENSION laUsr_lvl[1]
laUsr_lvl[1] = 'O'

SELECT CUSR_LEVL ;
 FROM (gcSysHome+'SYUUSER') ;
 WHERE CUSER_ID  = gcUser_ID ;
 INTO ARRAY laUsr_lvl

IF laUsr_lvl[1] = 'A'
  IF !lfFoundPad('Options')
    DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P,' '
    ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
    DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  ENDIF

  lnBarNo = CNTBAR('_OPTIONPOP') + 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Allow Autherization Over Entitlement' SKIP FOR laScrMode[1]
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfAlwAuth IN DIRMAIN.PRG

  *- 
  IF !'AUTHENTL' $ lcScFields
    lcScFields = lcScFields + ',AUTHENTL'
  ENDIF
  lnPos = OCCURS([,] , lcScFields)+1
  DIMENSION laData[lnPos]
ENDIF
*-- end of lfUSROPTN.

*:**************************************************************************
*:* Name        : lfFoundPad
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*!* Purpose     : check if any pad menu is exit in _sysmenu
*:***************************************************************************
*C123847,1
FUNCTION lfFoundPad
PARAMETER lcPadName

PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
    llfound = .T.
	EXIT
  ENDIF
ENDFOR
RETURN(llFOund)
*-- end of lfFoundPad.

*:**************************************************************************
*:* Name        : lfAlwAuth
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/04/2005
*:* Purpose     : Change the field SYUUSER.AUTHENTL (Y / N)
*:***************************************************************************
*C123847,1
FUNCTION lfAlwAuth
PRIVATE lnPos,lcTitle
lnPos = OCCURS([,] , lcScFields)+1

*- Update the field list "lcScFields"
IF 'AUTHENTL' $ lcScFields
  IF TYPE('laData[lnPos]')='L'
    laData[lnPos] = IIF(laScrMode[2],SYUUSER.AUTHENTL,'N')
  ENDIF
  lnPos = OCCURS([,] , lcScFields)+1
  llAuth = (laData[lnPos]='Y')
  DO (gcScrDir + gcWinAppl + '\SMAUENT.SPR')    
  laData[lnPos]= IIF(llAuth,'Y','N')
ENDIF

*-- end of lfAlwAuth.

*:**************************************************************************
*:* Name        : lfUPDOPBAR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/05/2005
*:* Purpose     : Set mark to the option bar for "Allow Auth. of over entitlement" for DIR03
*:***************************************************************************
*:* Called from : lpshow IN smusers.prg
*:***************************************************************************
*C123847,1
FUNCTION lfUPDOPBAR
PRIVATE lnPos
IF 'AUTHENTL' $ lcScFields
  lnPos = OCCURS([,] , lcScFields)+1
  IF laScrMode[2]
    laData[lnPos] = SYUUSER.AUTHENTL
  ENDIF  
ENDIF
*-- end of lfUPDOPBAR.

*:**************************************************************************
*:* Name        : lfRELAUPAD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*:* Purpose     : Release Added option pad
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfRELAUPAD

RELEASE PAD _Option OF _MSYSMENU

*-- end of lfRELPAD.

*:**************************************************************************
*:* Name        : lfADQTCFLD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2005
*:* Purpose     : Add qty/Ctn field between PPqty and Gross Price for DIR03
*:***************************************************************************
*:* Called from : lfvBrow1 in POSTYLE.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfADQTCFLD
IF !'QTY_CTN' $ UPPER(lcBrD1fld)
  lnPos = AT('GROS_PRICE',UPPER(lcBrD1fld))
  lcBrD1fld = STUFF(lcBrD1fld,lnPos,0,"STYLE.QTY_CTN :H='Qty/Ctn' :P='999',")
ENDIF
*-- end of lfADQTCFLD.

*:**************************************************************************
*:* Name        : lfSODIRALO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/12/2005
*:* Purpose     : Call alordal.prg with the order# to allocate
*:***************************************************************************
*:* Called from : lfSavScr IN soupdate.prg
*:***************************************************************************
*C123847,1
FUNCTION lfSODIRALO
*-- Call the Po cost sheet program.
* llNoShow    = .F.
* llCallShow  = .T.
* llShow      = .F.

*/*  Ignore this function and make all allocation in mbimain.prg 
return


PRIVATE Dir_llAloc,Dir_lnAvl

IF laScrMode[4]
  
  RESTORE FROM (gcWorkDir+lcFolder+'.MEM') ADDITIVE
  ERASE (gcWorkDir+lcFolder+'.MEM')
  IF Dir_llAloc
    IF Dir_lnAvl > 0
      PUSH KEY
      ON KEY
      
      lcParameter = "'" + laData[1] + "'"
      DO gpDoProg WITH "AWRALORDAL", .F., "AL", lcParameter
      POP KEY
      llShow = .F.    
      
      laScrMode    = .F.
      laScrMode[1] = .T.
      =gfStatic()                      && Save environment before terminate
    ELSE
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Currently there is no stock to allocate this order.')
    ENDIF
  ENDIF
  
ENDIF
*-- end of lfSODIRALO.

*:**************************************************************************
*:* Name        : lfALDIROP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/13/2005
*:* Purpose     : Update laData array with the passed order as a parameter , and call the lfvdata_1 
*                 as if the order# is entered manually
*:***************************************************************************
*:* Called from : lfChkUnCmS IN ALORDAL.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfALDIROP

IF TYPE('lcOrderNo') = 'C'
  laData[1] = lcOrderNo
  Select ordhdr
  =Seek('O'+laData[1])
  SCATTER FIELDS &lcScFields MEMO TO laData
  =lfvData_1()
  laScrMode    = .F.
  laScrMode[3] = .T.
  
  puWareHous = ASCAN(laWareHous,ORDHDR.CWARECODE)
  puWareHous = ASUBSCRIPT(laWareHous,puWareHous,1)
  
  llGenPkTk = .T.
ENDIF

*-- end of lfALDIROP.

*:**************************************************************************
*:* Name        : lfPKBTN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/13/2005
*:* Purpose     : open the screen with Pick button activated for DIR03
*:***************************************************************************
*:* Called from : lfReadAct IN ALORDAL.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfPKBTN
PRIVATE lnSlct
lnSlct = SELECT()
IF TYPE('lcOrderNo') = 'C'
  =lfShowPkBt()
  
  *- Also allow to edit the record preventing the system message to appear
  SELECT ORDHDR
  lcStamp       =  cAdd_User+IIF(EMPTY(dAdd_Date),'',DTOC(dAdd_Date))+;
                     cAdd_Time
ENDIF  

SELECT (lnSlct)

*-- end of lfPKBTN.

*:**************************************************************************
*:* Name        : lfOnHandBr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/16/2005
*:* Purpose     : change the browse such that to Calculate the onhand qty 
*               : taking in account the picked qty for DIR03
*:***************************************************************************
*:* Called from : lfBrowse IN MFCSSH.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfOnHandBr

  lcBrowFild = ;
         "cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,"+;
         "cItem =IIF(cCatgTyp='M',gfCodDes(MfgCode,'MfgCode'),IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item)) :H='Item' :R,"+;
         IIF(llUseDyelot,"Dyelot :H='Dyelot' :R,","")+;
         "Uom       :H='UOM'     :R ,"+;
         "UntQty    :H='Units'   :R,"+;
         "UntCost   :H='Cost'    :R,"+;
         "nStk=IIF(cCatgTyp $ 'FT' AND !lascrmode[1],IIF(EMPTY(cWarecode),Fabric.OnHand,FABDYE.ONHAND),IIF(cCatgTyp='S',IIF(EMPTY(cWarecode),STYLE.TOTSTK-STYLE.TOTALO ,STYDYE.TOTSTK-STYDYE.TOTALO),'')):H='OnHand':R,"+;
         "Req_Qty   :H='Required':R,"+;
         "Issue_Qty :H='Issued'  :R,"+;
         "Used_Qty  :H='Used'    :R"

*-- end of lfOnHandBr.

*:**************************************************************************
*:* Name        : lfSTKPKISU
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/16/2005
*:* Purpose     : check (stock - picked) >= issued , and warn if not for DIR03 
*:***************************************************************************
*:* Called from : lfIssRetSty IN MFCSSH.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfSTKPKISU
PRIVATE lcFile,lcKey,lnI,lcMsg,lnResp,lcSize

IF TYPE('llIssue')='U' .OR. (TYPE('llIssue')='L' .AND. llIssue)

  lcFile = IIF(EMPTY(lcWareCode),'STYLE','STYDYE')
  lcKey  = lcStyle + IIF(EMPTY(lcWareCode),'',lcWareCode+lcDyelot)
  =SEEK(lcKey,lcFile)
  IF lcFile <> 'STYLE'
    =SEEK(lcStyle,'STYLE')
  ENDIF
  =SEEK('S'+STYLE.SCALE,'SCALE')
    
  lcSize = ' '
  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    IF ABS(laIssued[lnI]) > &lcFile..STK&lcI - &lcFile..ALO&lcI
      lcSize = lcSize + SCALE.SZ&lcI + ','
    ENDIF
  ENDFOR
  lcSize = LEFT(lcSize,LEN(lcSize)-1)
  IF !EMPTY(lcSize)
    lcMsg = 'Inventory of style '+&lcFile..STYLE+ ', Sizes:'+ lcSize + ' is not sufficient, Continue ?'
    lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,lcMsg) 
    IF lnResp <> 1
      RETURN .F.
    ENDIF       
  ENDIF
    
ENDIF
*-- end of lfSTKPKISU.

*:**************************************************************************
*:* Name        : lfOPNORDBR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2005
*:* Purpose     : Define bar 'Open order' for Dir03
*:***************************************************************************
*:* Called from : ALORDAL.PRG
*:***************************************************************************
*C123847,1
FUNCTION lfOPNORDBR
PRIVATE lnBarNo
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT '\<Sales Order Screen' SKIP FOR EMPTY(laData[1]) .OR. laScrMode[1] .OR. laScrMode[3] .OR. TYPE('lcOrderNo')='C'
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfOPNORD IN DIRMAIN.PRG
*-- end of lfOPNORDBR.

*:**************************************************************************
*:* Name        : lfOPNORD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2005
*:* Purpose     : Open sales order screen with the current order opened in alordal screen
*:***************************************************************************
*C123847,1
FUNCTION lfOPNORD
PRIVATE lcToPass
PUSH KEY
ON KEY LABEL ESC llDummyVar=.T.
lcToPass =  "'O','" + laData[1] + "'"
DO gpDoProg WITH 'AWRSOORD',.F.,'SO',lcToPass
POP KEY
*-- end of lfOPNORD.

*:**************************************************************************
*:* Name        : lfGENEWPKT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/20/2005
*:* Purpose     : Always generate a new Piktkt for DIR03, so no need for the grid to be shown
*:***************************************************************************
*:* Called from : lfPickGrid IN ALAUTAL.PRG
*:***************************************************************************
*C123847,5
FUNCTION lfGENEWPKT
PRIVATE lnSlct,lnRecno,lcDirOrdLn,;
        lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8
lnSlct = SELECT()

IF !llRpAlocat
  RETURN
ENDIF  

*- Always generate a new piktkt
llRpGenPik = .T.
lnRpGenNew = 2
STORE 0 TO lnRpPikCor,lnRpPikSep

*- Since I may add more lines to the temp file , create a typical copy of it to loop only the original lines
lcDirOrdLn = gfTempName()
SELECT * FROM &lcTmpOrdLn INTO TABLE (gcWorkDir+lcDirOrdLn)

SELECT &lcDirOrdLn
SET RELATION TO RECNO() INTO &lcTmpOrdLn
LOCATE

SCAN FOR !EMPTY(PIKTKT)

  SELECT &lcDirOrdLn
  =SEEK(STYLE+CWARECODE+SPACE(10),'STYDYE')
  
  lnQty1 = IIF( QTY1-PIK1>0 .AND. STYDYE.STK1-STYDYE.ALO1>0 , MIN( QTY1-PIK1 , STYDYE.STK1-STYDYE.ALO1 ) , 0 )
  lnQty2 = IIF( QTY2-PIK2>0 .AND. STYDYE.STK2-STYDYE.ALO2>0 , MIN( QTY2-PIK2 , STYDYE.STK2-STYDYE.ALO2 ) , 0 )
  lnQty3 = IIF( QTY3-PIK3>0 .AND. STYDYE.STK3-STYDYE.ALO3>0 , MIN( QTY3-PIK3 , STYDYE.STK3-STYDYE.ALO3 ) , 0 )
  lnQty4 = IIF( QTY4-PIK4>0 .AND. STYDYE.STK4-STYDYE.ALO4>0 , MIN( QTY4-PIK4 , STYDYE.STK4-STYDYE.ALO4 ) , 0 )
  lnQty5 = IIF( QTY5-PIK5>0 .AND. STYDYE.STK5-STYDYE.ALO5>0 , MIN( QTY5-PIK5 , STYDYE.STK5-STYDYE.ALO5 ) , 0 )
  lnQty6 = IIF( QTY6-PIK6>0 .AND. STYDYE.STK6-STYDYE.ALO6>0 , MIN( QTY6-PIK6 , STYDYE.STK6-STYDYE.ALO6 ) , 0 )
  lnQty7 = IIF( QTY7-PIK7>0 .AND. STYDYE.STK7-STYDYE.ALO7>0 , MIN( QTY7-PIK7 , STYDYE.STK7-STYDYE.ALO7 ) , 0 )
  lnQty8 = IIF( QTY8-PIK8>0 .AND. STYDYE.STK8-STYDYE.ALO8>0 , MIN( QTY8-PIK8 , STYDYE.STK8-STYDYE.ALO8 ) , 0 )
  
  IF lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8 > 0

    REPLACE QTY1 WITH QTY1-lnQty1 ;
            QTY2 WITH QTY2-lnQty2 ;
            QTY3 WITH QTY3-lnQty3 ;
            QTY4 WITH QTY4-lnQty4 ;
            QTY5 WITH QTY5-lnQty5 ;
            QTY6 WITH QTY6-lnQty6 ;
            QTY7 WITH QTY7-lnQty7 ;
            QTY8 WITH QTY8-lnQty8 ;
            TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
            BOOK1 WITH BOOK1-lnQty1 ;
            BOOK2 WITH BOOK2-lnQty2 ;
            BOOK3 WITH BOOK3-lnQty3 ;
            BOOK4 WITH BOOK4-lnQty4 ;
            BOOK5 WITH BOOK5-lnQty5 ;
            BOOK6 WITH BOOK6-lnQty6 ;
            BOOK7 WITH BOOK7-lnQty7 ;
            BOOK8 WITH BOOK8-lnQty8 ;
            TOTQTY WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8

    SCATTER MEMVAR MEMO
    SELECT &lcTmpOrdLn
    GOTO RECNO(lcDirOrdLn)
    GATHER MEMVAR

    STORE lnQty1 TO M.QTY1,M.BOOK1
    STORE lnQty2 TO M.QTY2,M.BOOK2
    STORE lnQty3 TO M.QTY3,M.BOOK3
    STORE lnQty4 TO M.QTY4,M.BOOK4
    STORE lnQty5 TO M.QTY5,M.BOOK5
    STORE lnQty6 TO M.QTY6,M.BOOK6
    STORE lnQty7 TO M.QTY7,M.BOOK7
    STORE lnQty8 TO M.QTY8,M.BOOK8
    
    STORE 0 TO M.PIK1,M.PIK2,M.PIK3,M.PIK4,M.PIK5,M.PIK6,M.PIK7,M.PIK8,M.TOTPIK
    
    =SEEK('O'+ORDER,'ORDHDR')
    m.LASTLINE = ORDHDR.LASTLINE + 1

    m.PICKED  = .F.
    m.PIKTKT  = ' '
    m.PIKDATE = {}
    m.LINENO  = m.LASTLINE
    
    *- Add line to the temp file
    INSERT INTO &lcTmpOrdLn FROM MEMVAR    

    *- Also add a line to the ORDLINE file 
    INSERT INTO ORDLINE FROM MEMVAR
    
    *- Update the line numbers in ORDHDR file
    =SEEK('O'+ORDER,'ORDHDR')
    SELECT ORDHDR
    REPLACE LASTLINE WITH M.LASTLINE
    
    *- Finally update the current line in ORDLINE file
    && ORDLINE key :: CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
    SELECT &lcDirOrdLn   
    =SEEK(CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6),'ORDLINE') 
    SELECT ORDLINE
    REPLACE QTY1 WITH QTY1-lnQty1 ;
            QTY2 WITH QTY2-lnQty2 ;
            QTY3 WITH QTY3-lnQty3 ;
            QTY4 WITH QTY4-lnQty4 ;
            QTY5 WITH QTY5-lnQty5 ;
            QTY6 WITH QTY6-lnQty6 ;
            QTY7 WITH QTY7-lnQty7 ;
            QTY8 WITH QTY8-lnQty8 ;
            TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 ;
            BOOK1 WITH BOOK1-lnQty1 ;
            BOOK2 WITH BOOK2-lnQty2 ;
            BOOK3 WITH BOOK3-lnQty3 ;
            BOOK4 WITH BOOK4-lnQty4 ;
            BOOK5 WITH BOOK5-lnQty5 ;
            BOOK6 WITH BOOK6-lnQty6 ;
            BOOK7 WITH BOOK7-lnQty7 ;
            BOOK8 WITH BOOK8-lnQty8 ;
            TOTQTY WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8
        
  ENDIF
  
ENDSCAN

USE IN &lcDirOrdLn
ERASE (gcWorkDir+lcDirOrdLn+'.DBF')

SELECT &lcTmpOrdLn
LOCATE

SELECT (lnSlct)
*-- end of lfGENEWPKT.