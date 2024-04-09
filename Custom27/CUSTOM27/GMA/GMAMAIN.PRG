*!**************************************************************************
*! Name      : GMAMAIN.PRG
*! Developer : Nader Anis
*! Date      : 04/17/2000
*! Purpose   : GMA Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C101796,1
*!**************************************************************************
*! Modifications
*! B803350,1 NAD 06/22/2000 1-Disable the department Field in the invoice header as 2.6.   
*! B803350,1                2-Edit the store field in the INVLINE,UPSBOX, REPCOMM
*! B803350,1                  DEBIT and ARHIST.         
*! C102276,1 MHM 10/04/2001 Add a new option to the adjust cost for receiving screen
*! C102276,1 MHM            to get the total cost for all POs in case of shipment 
*! B604475,1 KHM 05/15/2001 Fix the bug of accumolating the unit cost instead
*! B604475,1                amount
*! C101945,1 TMI 07/03/2001 Modification on the pack generation screen by maintining Version 
*! C101945,1 TMI            numbers for each pack id
*! C101946,1 SSE 07/31/2001 Add modifications in Sales Order Screen for Packs.
*! C200212,1 AMH 09/12/2001 Add two Custom Codes to PosHdr for Gma.
*! C102452,1 HBG 11/21/2001 Make the Mode of The OG of the Custom fields always View
*! C102514,1 SSE 02/28/2002 Custom modifications in Gross profit report.
*! C200326,1 MHM 04/03/2002 Customize the report to another one.
*! C102574,1 HBG 04/15/2002 MOdification in direct invoice screen for selecting Packs
*! C102566,1 MHM 04/15/2002 Customize SKU screen
*! C102575,1 HBG 04/15/2002 MOdification in invoice from sales order screen for selecting Packs
*! C200352,1 HBG 04/15/2002 Update 'SHDAEXPR' File of UPS system 
*! C102570,1 MHM 04/20/2002 modification in Sales Order Screen
*! C200404,1 HBG 08/28/2002 Add new Bar to Allow Modify # of ordered pack
*! C200410,1 HBG 23/09/2002 Calculate ship amount using the new field npackNo [Begin]
*! B606160,1 HBG 14/10/2002 prevent adding more than one SKU# for the same 
*! B606160,1                Account/Style/Color/size OR the same Account/Pack Id/Color/Size.
*! C200424,1 MHM 10/28/2002 Add new bar to modify CustPo And Selling price
*! E302037,1 HBG 21/10/2002 Allow changing the pack price in the order.
*! C200433,1 HBG 17/11/2002 Enable price field in case of range pack & price per 
*! C200433,1                piece and selling price = 0 
*! C200479,1 HBG 30/12/2002 Add lRange & lPCkPrPiec fields to ORDLINE & INVLINE files 
*! C200479,1                and use them instead of SPCH_HDR fields
*! C200484,1 ALB 01/13/2003 Get carton\weight from pack_dhr as default and update the SO Inv.
*! C200491,1 ALB 01/23/2003 Copy lines from SO to PO
*! C200492,1 ALB 01/27/2003 Allocate PO to the poshdr.ccutcode in SO
*! B606913,1 HBG 01/27/2003 Fix bugs in Modify # of packs screen
*! C200499,1 HBG 01/27/2003 UnShip Styles and packs in Invoice SO Screen For all stores
*! C200507,1 TMI 03/06/2003 Trigger for GMA to not include Canceled store in invoicing program  
*! C200508,1 TMI 03/11/2003 Add Asking for the Location  ( in Case multiware = .F. ) in the check of adding a line to POTOSEND file
*! C200518,1 HBG 03/12/2003 Get the Ship To address from Connect file 
*! C200519,1 TMI 03/13/2003 Add bar for GMA titled "Customer Packing Info"
*! B607250,1 SSE 05/11/2003 Add trigger for GMA to enable warehouse code in Edit mode if it's allocated to PO or CT.
*! C200558,1 SSE 05/15/2003 Some modifications in UPS Package Tracking screen.
*! B607397,1 SSE 07/06/2003 Fix bug of SHDAEXPR doesn't get updated of a big multi store is being invoiced.
*! B120209,1 MHM 12/22/2003 Fix bug of Modify # of packs option doesn't update the price per piece field incase user changes pack version.
*! B120210,1 MHM 12/28/2003 Fix bug of Rebalance program doesn't update the 
*! B120210,1                amount correctly incase the pack's are scattered for the same store
*! B037436,1 MHM 01/20/2004 Fix bug of goto infinit loop if you open Direct Invoice 
*! B121372,1 MHM 02/05/2004 Fix bug of Calculate wrong piece per pack in Modify # of Packs in Sales order Screen
*! B121820,1 MHM 02/25/2004 Enhance Speed of Sales Order Invoice when saving Invoice
*! B122625,1 NNA 04/29/2004 Fix bug of Zero Shipped Amount When Invoice SO (For GMA) Specially If we have
*! B122625,1 NNA            M.Stores SO and Invoiced All Stores Except One and When We go to invoice It
*! B122625,1 NNA            The Shipped Amount Comes Zero
*! B123600,1 MHM 08/31/2004 Fix bug of Unship a Pack/color when invoice Sales Order unship all packs
*! C125213,1 NNA 12/26/2004 Add a new field for the "Assist Cost" on the SO and Invoice 
*! B124629,1 MHM 12/15/2004 Fix bug of not correct retrive of size 
*! B124944,1 MHM 12/20/2004 Fix bug of not of delete and save SKU
*! B125927,1 NNA 01/07/2005 Fix bug The user cannot clear ship qty of an item from one store 
*! B125927,1 NNA            in case of multi store order
*! B125563,1 NNA 02/07/2005 Fix bug that if you chose a multi store order then options - sort by
*! B125563,1 NNA            Store - Show Summary - Summarize by Pack . you'll get an error
*! B125563,1 NNA            'Variable 'npkslprice' not found'
*! B126137,1 NNA 02/21/2005  Fix bug that an error massage appear if you edit the invoice and
*! B126137,1 NNA             tried to change the Qty and save (The previous Part Fixed in the 
*! B126137,1 NNA             ARDINV.prg) but here Aria4 made 2 Triggers Called (Insert and Update)
*! B126137,1 NNA             that make an error because they don't found in aria27
*! B126325,1 NNA 03/20/2005 Fix bug that in the Sku Screen when you choose the user defined field
*! B126325,1 NNA            icon then moving by the Scroll Bar then you'll get an Error message
*! B127452,1 NNA 06/20/2005 Fix bug : Wrong Amount gets invoiced that happen if you made a multi store order with unconsolidated
*! B127452,1 NNA            customer then made an allocation for this order by picked all lines but change in the any store
*! B127452,1 NNA            picked qty. then invoice this order , you'll find that the invoice for that store has a wrong amount
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*!**************************************************************************
*! Name      : lfEdtFlds
*! Developer : Nader Anis
*! Date      : 04/17/2000
*! Purpose   : Add GMA editable fields to the main fields
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfEdtFlds
*B803350,1 (Start) Remove the department field from the fields that will be edited. 
*lcEdtFld= lcEdtFld+',lnShipVia,STORE,ibStore,CARTONS,WEIGHT,SHIPVIA'
lcEdtFld='SHIPDATE,DUEDATE,NOTE1,NOTE2,lnSpcinst,SPCINST,lnShipVia,STORE,ibStore,CARTONS,WEIGHT,SHIPVIA'
*B803350,1 (End)
*!**************************************************************************
*! Name      : lfStoreAdd
*! Developer : Nader Anis
*! Date      : 04/17/2000
*! Purpose   : Get the ship Address in edit mode
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************

FUNCTION lfStoreAdd

IF EMPTY(laData[5]) 
  =SEEK('M'+laData[2],'CUSTOMER')
ELSE
  =SEEK('S'+laData[2]+laData[5],'CUSTOMER')
ENDIF
STORE '' TO lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5
lcShipName = IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
=gfGetAdr('CUSTOMER','','','',1,'')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
=lfRefresh(lcWinCh2)
SHOW GETS WINDOW (lcWinCh2) ONLY
llCUpdate = .T.

*C200352,1 HBG 28/05/2002 Get the Ship information of the new store and update SHDAEXPR with it[Begin]
lcDVarFle = lcInvHdr+'A'
GO TOP IN (lcDVarFle)
REPLACE &lcDVarFle..llUpStrInf WITH .T.
*C200352,1 [End]

*!**************************************************************************
*! Name      : lfUPDEDIFL
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : Add a new Bar to the option's pad 'Update Edi Fields'
*!**************************************************************************
*! Example   : lfUPDEDIFL()
*!**************************************************************************
*! C101887,1 KHM 05/26/2000
*!**************************************************************************
FUNCTION lfUPDEDIFL

DEFINE BAR 17 OF _INQURYPOP PROMPT 'Update EDI Fields' SKIP FOR EMPTY(laData[2])

*!**************************************************************************
*! Name      : lfUPDEDISC
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : To call the SOEDIFLD screen that enable you to change EDI Fields.
*!**************************************************************************
*! Example   : lfUPDEDISC()
*!**************************************************************************
*! C101887,1 KHM 05/26/2000
*!**************************************************************************
FUNCTION lfUPDEDISC

lcEscTr = ON("KEY","ESC")
ON KEY LABEL ESCAPE DO lpEdiClose
DO (gcScrDir+gcWinAppl+"\SOEDIFLD.SPX")
ON KEY LABEL ESCAPE &lcEscTr

*!**************************************************************************
*! Name      : lpEdiClose
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : Valid procedure of Close Button for screen(SOEDIFLD)
*!**************************************************************************
*! Example   : lpEdiClose()
*!**************************************************************************
*! C101887,1 KHM 05/26/2000
*!**************************************************************************
PROCEDURE lpEdiClose
CLEAR READ


*!**************************************************************************
*! Name      : lfActivate
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : Valid Function of when the SOEDIFLD screen is activated.
*!**************************************************************************
*! Example   : lfActivate()
*!**************************************************************************
*! C101887,1 KHM 05/26/2000
*!**************************************************************************
FUNCTION lfActivate

IF laScrMode[2]
  *-- Calling the function with view mode.
  =lfGetEdiFl('V')
  SHOW GETS DISABLE
  SHOW GET pbEdiClose,1 PROMPT '\<Close'
ELSE
  IF laScrMode[3]
    *-- Calling the function with edit mode.
    =lfGetEdiFl('E')
  ENDIF
  SHOW GETS ENABLE
  SHOW GET pbEdiClose,1 PROMPT '\<Cancel'
  _CUROBJ = OBJNUM(m.Int_Vend)
ENDIF

*!**************************************************************************
*! Name      : lfvOkEdiFl
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : Valid Function for OK button of SOEDIFLD screen.
*!**************************************************************************
*! Example   : lfvOkEdiFl()
*!**************************************************************************
*! C101887,1 KHM 05/26/2000
*!**************************************************************************
FUNCTION lfvOkEdiFl

*-- Calling the function with add mode.
=lfGetEdiFl('S')
CLEAR READ

*!**************************************************************************
*! Name      : lfGetEdiFl
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : To enable or disable object on the screen according to the
*!             screen mode.
*!**************************************************************************
*! Parameters : 'V' : View mode, 'E' : Edit mode, 'S' : Add mode.
*!**************************************************************************
*! Example   : lfGetEdiFl()
*!**************************************************************************
*! C101887,1 KHM 05/26/2000
*!**************************************************************************
FUNCTION lfGetEdiFl
PARAMETER lcStatus

IF lcStatus  $ 'VE'
  m.Int_Vend  = laData[54]
  m.Event_Cod = laData[55]
  m.BillNo    = laData[56]
  m.Merc_Type = laData[57]
  m.Blank_Ord = laData[58]
  m.Distrb_No = laData[59]
  m.cClass    = laData[56]
ELSE
  laData[54] = m.Int_Vend
  laData[55] = m.Event_Cod
  laData[56] = m.BillNo
  laData[57] = m.Merc_Type
  laData[58] = m.Blank_Ord
  laData[59] = m.Distrb_No
  laData[60] = m.cClass
ENDIF

*!**************************************************************************
*! Name      : lfOpnDepFl
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : To open the DeptHd & DeptDt files in order to assing style's
*!             group to departments and assign styles to deprtments.
*!**************************************************************************
*! Example   : lfOpnDepFl()
*!**************************************************************************
*! C200120,1 KHM 06/11/2000
*!**************************************************************************
FUNCTION lfOpnDepFl
PRIVATE lnAlias

lnAlias = SELECT(0)

=gfOpenFile(gcDataDir+'ICDEPTHD','CGROUP','SH')
=gfOpenFile(gcDataDir+'ICDEPTDT','DEPTDTS','SH')

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfUPDDEPFL
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : To assing new styles or delete the existing one according
*!             to the entered style's group.
*!**************************************************************************
*! Example   : lfUPDDEPFL()
*!**************************************************************************
*! C200120,1 KHM 06/11/2000
*!**************************************************************************
FUNCTION lfUPDDEPFL
PRIVATE lnAlias,llReturn,lnRecNo

lcOldGroup = IIF(EMPTY(Style.cStyGroup),'',Style.cStyGroup)
lcNewGroup = laGrp[lnGrp,2]

llReturn = .F.
lnAlias  = SELECT()

*---Case of adding new color to existing style with group assigning to dept.
IF llAllColors AND SEEK(lcNewGroup,'ICDeptHd')
  SELECT (lcColorFil)
  lnRecNo = RECNO()
  SCAN ALL FOR cstatus = 'A'
    llReturn = .T.
    SELECT ICDeptDt
    APPEND BLANK
    REPLACE Dept     WITH ICDeptHd.Dept;
            cStyGroup WITH lcNewGroup  ;
            Style     WITH &lcColorFil..Style
  ENDSCAN
  IF BETWEEN(lnRecNo,1,RECCOUNT(lcColorFil))
    GOTO lnRecNo IN (lcColorFil)
  ENDIF
  IF EMPTY(lcOldGroup) AND llReturn
    RETURN
  ENDIF
ENDIF

DO CASE
*--- Case new and old not assigned to dept (Return).
 CASE (lcOldGroup = lcNewGroup) OR (!SEEK(lcOldGroup,'ICDeptHd') AND ;
                                    !SEEK(lcNewGroup,'ICDeptHd'))
   SELECT(lnAlias)
   RETURN

*--- Case old assigened to dept and new not assigened(Delete from dept file).
 CASE SEEK(lcOldGroup,'ICDeptHd') AND !SEEK(lcNewGroup,'ICDeptHd')
   SELECT ICDeptDt
   IF llAllColors
     IF SEEK(lcMajor+lcNMKey)
       DELETE REST WHILE Style+Dept = lcMajor+lcNMKey
     ENDIF
   ELSE    
     IF SEEK(lcMajor+lcNMKey)
       DELETE
     ENDIF
   ENDIF

*--- Case old not assigened to dept and new assigened(Append to dept file).
 CASE !SEEK(lcOldGroup,'ICDeptHd') AND SEEK(lcNewGroup,'ICDeptHd')
  IF llAllColors
    SELECT (lcColorFil)
    lnRecNo = RECNO()
    SCAN
      SELECT ICDeptDt
      APPEND BLANK
      REPLACE Dept      WITH ICDeptHd.Dept ;
              cStyGroup WITH lcNewGroup    ;
              Style     WITH &lcColorFil..Style
    ENDSCAN
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcColorFil))
      GOTO lnRecNo IN (lcColorFil)
    ENDIF

  ELSE
    SELECT ICDeptDt
    APPEND BLANK
    REPLACE Dept      WITH ICDeptHd.Dept;
            cStyGroup WITH lcNewGroup   ;
            Style     WITH lcMajor+lcNMKey
  ENDIF

*--- Case new and old assigened to dept (Replace with new group and Dept).
 OTHERWISE
   SELECT ICDeptDt

   IF llAllColors
     IF SEEK(lcMajor+lcNMKey) AND SEEK(lcNewGroup,'ICDeptHd')     
       SCAN REST WHILE STYLE+Dept = lcMajor+lcNMKey
          REPLACE Dept      WITH ICDeptHd.Dept ;
                  cStyGroup WITH lcNewGroup
       ENDSCAN
     ENDIF
   ELSE
     IF SEEK(lcMajor+lcNMKey) AND SEEK(lcNewGroup,'ICDeptHd')
        REPLACE Dept      WITH ICDeptHd.Dept ;
                cStyGroup WITH lcNewGroup
     ENDIF
   ENDIF
ENDCASE

*--- Cheick if there exist group assigened to dept with no style color
*--- then delete this dept from header file.
IF SEEK(lcOldGroup,'ICDeptHd')
  SET ORDER TO TAG DeptDt IN ICDeptDt
  IF !SEEK(ICDeptHd.Dept,'ICDeptDt')
    SELECT ICDeptHd
    DELETE
  ENDIF
  SET ORDER TO TAG DEPTDTS IN ICDeptDt
ENDIF
SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfDELDPSty
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/26/2000
*! Purpose   : To delete the style form IcDeptDt when delete the style 
*!             from the file.
*!**************************************************************************
*! Example   : lfDELDPSty()
*!**************************************************************************
*! C200120,1 KHM 06/11/2000
*!**************************************************************************
FUNCTION lfDELDPSty
PRIVATE lnAlias,lcOldGroup

lnAlias = SELECT()
lcOldGroup = laGrp[lnGrp,2]
IF SEEK(lcMajor+lcSepart+lccolor,'IcDeptDt')
  SELECT IcDeptDt
  BLANK
  DELETE
  IF SEEK(lcOldGroup,'ICDeptHd')
  SET ORDER TO TAG DeptDt IN ICDeptDt
  IF !SEEK(ICDeptHd.Dept,'ICDeptDt')
    SELECT ICDeptHd
    BLANK
    DELETE
  ENDIF
  SET ORDER TO TAG DEPTDTS IN ICDeptDt
ENDIF
   
ENDIF
SELECT(lnAlias )


*!**************************************************************************
*! Name      : lfEdtStore
*! Developer : Nader Anis
*! Date      : 06/27/2000
*! Purpose   : Edit the store in the DEBIT and ARHIST files
*! Ref       : B803350,1
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfEdtStore
IF InvHdr.Consol <> "Y"
  REPLACE STORE WITH laData[5]
ENDIF
*!**************************************************************************
*! Name      : lfSavStore
*! Developer : Nader Anis
*! Date      : 06/27/2000
*! Purpose   : Edit the store in the REPCOMM and UPSBOX files.
*! Ref       : B803350,1
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfSavStore
IF InvHdr.Consol <> "Y"
  SELECT InvLine
  IF SEEK (InvHdr.Invoice)
    REPLACE REST STORE WITH laData[5] WHILE Invoice=InvHdr.Invoice
  ENDIF

  IF !USED('REPCOMM')
    =gfOpenFile(gcDataDir+'REPCOMM','REPCOMM','SH')
  ENDIF
  SELECT REPCOMM 
  IF SEEK(InvHdr.REP1+DTOS(InvHdr.INVDATE)+InvHdr.Invoice+'1')
    REPLACE STORE WITH laData[5] 
  ENDIF
  IF SEEK(InvHdr.REP2+DTOS(InvHdr.INVDATE)+InvHdr.Invoice+'1')
    REPLACE STORE WITH laData[5] 
  ENDIF        

  IF !USED('UPSBOX')
    =gfOpenFile(gcDataDir+'UPSBOX','UPSBOX','SH')
  ENDIF  
  SELECT UPSBOX
  IF SEEK(InvHdr.Invoice)
    SET ORDER TO 
    REPLACE REST STORE WITH laData[5] WHILE Invoice=InvHdr.Invoice
    SET ORDER TO  UPSBOX   
  ENDIF
ENDIF
*!**************************************************************************
*! Name      : lfStoreDis
*! Developer : Nader Anis
*! Date      : 06/27/2000
*! Purpose   : Not to edit the store field in case of conslidated invoice.
*! Ref       : B803350,1
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfStoreDis
IF InvHdr.Consol="Y" 
  SHOW GET laData[5] DISABLE
  SHOW GET ibStore   DISABLE
ENDIF  

*!*************************************************************
*! Name      : lfPoToSnd()
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/26/2000
*! Purpose   : Add Record to PoToSend .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Due to C102087
*!*************************************************************
*
FUNCTION lfPoToSnd

*C200508,1 TMI [Start] Ask for the vendor and also for the Location  ( in Case multiware = .F. )
*IF SEEK(laData[2],'APVENDOR')
*  IF APVENDOR.LUSECARGO
IF SEEK(laData[2],'APVENDOR') AND IIF(llMultiWare,.T.,SEEK(laData[19],'WAREHOUS'))
  IF APVENDOR.LUSECARGO AND IIF(llMultiWare,.T.,WAREHOUS.LUSECARGO)
  *C200508,1 TMI [End  ] 
    =gfOpenFile(gcDataDir+'POTOSEND','POTOSEND','SH')
    IF !SEEK(lcAType+laData[1],'POTOSEND')
      APPEND BLANK
      REPLACE CSTYTYPE WITH lcAType   ,;
              PO       WITH laData[1]
    ENDIF
    IF laScrMode[4]
      REPLACE TYPE WITH "A"
    ELSE
      IF laData[3]='X'
        REPLACE TYPE WITH "D"
      ELSE
        REPLACE TYPE WITH "C"
      ENDIF
    ENDIF
    REPLACE CSTATUS  WITH "N"
    =gfCloseFile('POTOSEND')
  ENDIF
ENDIF
*-- end of lfPoToSnd.

*!*************************************************************************
*! Name      : lfAdOptGma
*! Developer : AMH (Ahmed Maher)
*! Date      : 02/08/2001
*! Purpose   : add option to show summary and show received lines 
*!           : in Style shipment screen.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to C102160
*:**************************************************************************
*
FUNCTION lfAdOptGma
PRIVATE lnAlias

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW

lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Show Original lines'   SKIP FOR (lnactfolder<>2) .OR. !laScrMode[2]
DEFINE BAR lnBarNo+1 OF _OPTIONPOP PROMPT 'Show Summary'        SKIP FOR (lnactfolder<>2) .OR. !laScrMode[2]
DEFINE BAR lnBarNo+2 OF _OPTIONPOP PROMPT 'Show Received lines' SKIP FOR (lnactfolder<>2) .OR. !laScrMode[2]

SET MARK OF BAR 1 OF _OPTIONPOP TO .T.
ON SELECTION POPUP _OPTIONPOP DO gfDoTriger WITH "POSHP",PADR("ACTBAR",10)
lnAlias = SELECT(0)
SELECT (lcShpLine)
INDEX ON Style+PO+STR(LineNo,6) TAG lcShpStyPo OF (gcWorkDir+lcShpLine)
SELECT (lnAlias)
RETURN
*-- end of lfAdOptGma.


*!*************************************************************************
*! Name      : lfActBar
*! Developer : AMH (Ahmed Maher)
*! Date      : 02/08/2001
*! Purpose   : show Original Lines mode in Style shipment screen.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to C102160
*:**************************************************************************
*
FUNCTION lfActBar
*C102160,4 AMH Fix incorrect data if select shipment in show summary 
*C102160,4     or show recived lines [Start]
*PRIVATE lnAlias, lcOrder, lcTmpOrder, lcForExp, llSeek, lcSeekExp
PRIVATE lnAlias, lcOrder, lcTmpOrder, lcForExp, llSeek, lcSeekExp, lnRecNo
*C102160,4 AMH [End]

DO CASE
  CASE BAR() = 1 .AND. lnactfolder <> 1
    SET MARK OF BAR 1 OF _OPTIONPOP TO .T.
    SET MARK OF BAR 2 OF _OPTIONPOP TO .F.
    SET MARK OF BAR 3 OF _OPTIONPOP TO .F.
    lcShowMode = 'ORG'
    *C102160,4 AMH Fix incorrect data if select shipment in show summary 
    *C102160,4     or show recived lines [Start]
    *lcTmpOrder = lcShpLine
    *lcForExp = [TranCd = '3']
    *llSeek = .F.
    *lcSeekExp = ''
    *C102160,4 AMH [End]
  CASE BAR() = 2 .AND. lnactfolder <> 1
    SET MARK OF BAR 1 OF _OPTIONPOP TO .F.
    SET MARK OF BAR 2 OF _OPTIONPOP TO .T.
    SET MARK OF BAR 3 OF _OPTIONPOP TO .F.
    lcShowMode = 'SUM'
    *C102160,4 AMH Fix incorrect data if select shipment in show summary 
    *C102160,4     or show recived lines [Start]
    *lcTmpOrder = 'lcShpStyPo'
    *lcForExp = [TranCd $ '23']
    *llSeek = .T.
    *lcSeekExp = [m.Style]
    *C102160,4 AMH [End]
  CASE BAR() = 3 .AND. lnactfolder <> 1
    SET MARK OF BAR 1 OF _OPTIONPOP TO .F.
    SET MARK OF BAR 2 OF _OPTIONPOP TO .F.
    SET MARK OF BAR 3 OF _OPTIONPOP TO .T.
    lcShowMode = 'REC'
    *C102160,4 AMH Fix incorrect data if select shipment in show summary 
    *C102160,4     or show recived lines [Start]
    *lcTmpOrder = lcShpLine
    *lcForExp = [TranCd = '2']
    *llSeek = .T.
    *lcSeekExp = [m.Po+m.Style+STR(m.LineNo,6)]
    *C102160,4 AMH [End]
ENDCASE
*C102160,4 AMH Fix incorrect data if select shipment in show summary 
*C102160,4     or show recived lines [Start]
DO CASE
  CASE lcShowMode = 'ORG'
    lcTmpOrder = lcShpLine
    lcForExp = [TranCd = '3']
    llSeek = .F.
    lcSeekExp = ''
  CASE lcShowMode = 'SUM'
    lcTmpOrder = 'lcShpStyPo'
    lcForExp = [TranCd $ '23']
    llSeek = .T.
    lcSeekExp = [m.Style]
  CASE lcShowMode = 'REC'
    lcTmpOrder = lcShpLine
    lcForExp = [TranCd = '2']
    llSeek = .T.
    lcSeekExp = [m.Po+m.Style+STR(m.LineNo,6)]
ENDCASE
*C102160,4 AMH [End]
lnAlias = SELECT(0)
SELECT (lcShpLine)

SET ORDER TO TAG (lcTmpOrder)
ZAP
SELECT POsLn
*C102160,4 AMH Fix incorrect data if select shipment in show summary 
*C102160,4     or show recived lines [Start]
lnRecNo = RECNO()
*C102160,4 AMH [End]
lcOrder = SET('ORDER')
SET ORDER TO TAG POsLnSh
IF SEEK(laData[1])
  SCAN REST WHILE ShipNo+cStyType+Po+Style+STR(LineNo,6)+TranCd = laData[1] FOR &lcForExp
    SCATTER MEMVAR
    SELECT (lcShpLine)
    IF llSeek .AND. SEEK(EVALUATE(lcSeekExp))
      *C102160,4 AMH Fix incorrect data if select shipment in show summary 
      *C102160,4     or show recived lines [Start]
      *IF BAR() = 3
      IF lcShowMode = 'REC'
      *C102160,4 AMH [End]
        REPLACE QTY1 WITH QTY1 + m.Qty1,;
                QTY2 WITH QTY2 + m.Qty2,;
                QTY3 WITH QTY3 + m.Qty3,;
                QTY4 WITH QTY4 + m.Qty4,;
                QTY5 WITH QTY5 + m.Qty5,;
                QTY6 WITH QTY6 + m.Qty6,;
                QTY7 WITH QTY7 + m.Qty7,;
                QTY8 WITH QTY8 + m.Qty8,;
                TOTQTY WITH TOTQTY + m.TotQty
      ELSE
        IF m.Trancd = '3'
          REPLACE TOTQTY WITH TOTQTY + m.TotQty
        ELSE
          REPLACE TOTORD WITH TOTORD + m.TotQty
        ENDIF
      ENDIF
    ELSE
      APPEND BLANK
      GATHER MEMVAR
      *C102160,4 AMH Fix incorrect data if select shipment in show summary 
      *C102160,4     or show recived lines [Start]
      *IF BAR() = 2
      IF lcShowMode = 'SUM'
      *C102160,4 AMH [End]
        IF m.Trancd = '2'
          REPLACE TOTQTY WITH 0, TOTORD WITH m.TotQty
        ELSE
          REPLACE TOTORD WITH 0
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
SELECT POsLn
SET ORDER TO &lcOrder.
*C102160,4 AMH Fix incorrect data if select shipment in show summary 
*C102160,4     or show recived lines [Start]
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
*C102160,4 AMH [End]
SELECT (lnAlias)
=lfActBrow1()
*C102160,4 AMH Refresh Screen fields [Start]
=lfwBrow1()
*C102160,4 AMH [End]
*-- end of lfActBar.
*!**************************************************************************
*! Name      : AddOptn                     (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001
*! Purpose   : Add a new Option to the  menu Bar
*!**************************************************************************
*! Example   : lfAddOptn()
*!*************************************************************
*!
FUNCTION lfADDOPTN

DEFINE BAR 3 OF _OPTIONPOP PROMPT 'Shipment Total Costs'  SKIP FOR !EMPTY(laScrMode[1])
ON SELECTION BAR 3 OF _OPTIONPOP DO lpDispScr IN GMAMAIN
*-End 


*!**************************************************************************
*! Name      : lpDispScr            (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001        
*! Purpose   : Display Screen to show the total costs for the shipment.
*!*************************************************************
*! Example   : DO lpDispScr
*!*************************************************************
*!
PROCEDURE lpDispScr

lcTmpFil  = gfTempName()
=lfCrtTmp()
lnBrTrNo = 0
lcBrTrttl = "Total Costs"
=lfColctDat()
SELECT (lcTmpFil)
lcOkBmp    = gcBmpHome + "CLS.bmp"
lcEscInt = ON('KEY','ESC')
lcTabInt = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcAltB   = ON('KEY','ALT+B')    

DO (gcScrDir+"PO\POSHCST.SPX")

ON KEY LABEL ESC      &lcEscInt
ON KEY LABEL TAB      &lcTabInt
ON KEY LABEL BACKTAB  &lcHldBtb
ON KEY LABEL ALT+B    &lcAltB

*!*************************************************************
*! Name      : lfLCCrTrp        (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001
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
*! Example   : =lfLCCrTrp()
*!*************************************************************

FUNCTION lfLCCrTrp

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfTrBrTab                      (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001        
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
ACTIVATE WINDOW PoShCst2
_CUROBJ = OBJNUM(pbCan)

*!*************************************************************
*! Name      : lfTrBrBack  (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001        
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
*ON KEY LABEL BACKTAB
*IF WONTOP() = (lcBrTrttl)
  _CUROBJ = OBJNUM(pbCan)
*ENDIF

*!*************************************************************
*! Name      : lfTrap             (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001        
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

FUNCTION lfLCTrnTrp

*-- This function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTrttl
  ON KEY LABEL TAB     DO lfTrBrTab
  ON KEY LABEL BACKTAB DO lfTrBrBack
  ON KEY LABEL ESC     DO lfTrBrEsc
ENDIF
RETURN .F.


*!*************************************************************
*! Name      : lfTrBrEsc               (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001        
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
ACTIVATE WINDOW PoShCst2
_CUROBJ = OBJNUM(pbCan)
KEYBOARD "{ENTER}" CLEAR

*!*************************************************************
*! Name      : lfLcTrnBrw                 (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001
*! Purpose   : Browse the Shipment records
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

FUNCTION lfLcTrnBrw

lnCurAlias = SELECT(0)
lcBrCost = "cMarker=IIF(RECNO()=lnBrTrNo,'>',' '):1:H=' ':W=.F.,"+; 
            "cItem      :20  :R :H='Item',"+;
            "cCost      :13 :R :H='Total Cost' :P=IIF(!EMPTY(cCost),'999999999.999','')"

SELECT (lcTmpFil)
LOCATE

BROWSE FIELDS &lcBrCost;
   WINDOW 'PoShCst1'     ;
   IN WINDOW 'PoShCst' ;
   WHEN lfwTrnBrow() ;
   LOCK 0            ;
   NOAPPEND          ;
   NOEDIT            ;
   NOCLEAR           ;
   NODELETE          ;
   NOMENU            ;   
   SAVE              ;
   TITLE lcBrTrttl   ;
   NOWAIT

SELECT (lnCurAlias)               
*!*************************************************************
*! Name      : lfCrtTmp                 (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Create Temp File 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCrtTmp()
*!*************************************************************

FUNCTION lfCrtTmp
CREATE Table (gcWorkDir+lcTmpFil) (cItem C(19),cCost C(13))

*!*************************************************************
*! Name      : lfwTrnBrow             (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/04/2001
*! Purpose   : When Function for browse 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwTrnBrow()
*!*************************************************************
*!
FUNCTION lfwTrnBrow
lnCurAlias = SELECT(0)
SELECT (lcTmpFil)
lnBrTrNo = RECNO()
*--initialize fields
SELECT (lnCurAlias)
SHOW WINDOW (lcBrTrttl) REFRESH

*--End
*!*************************************************************
*! Name      : lfColctDat                 (C102276,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/04/2001
*! Purpose   : Collect needed Data 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfColctDat()
*!*************************************************************
*!
FUNCTION lfColctDat
PRIVATE lnTotAmt,lcOldTag,lnAlias,lnRecNum
STORE 0 TO lnTotAmt1 ,lnTotAmt2 ,lnTotAmt3 ,lnTotAmt4 ,lnTotAmt5 ,lnTotAmt

lnAlias = SELECT()
SELECT (lcTmpLine)
lnRecNum = RECNO()
lcOldTag = SET('ORDER')
SET ORDER TO TAG (lcTmpLine1)

FOR lnCounter = 1 TO 5
  lcCounter =STR(lnCounter,1)
  SELECT (lcTmpLine)
  *lcOldTag = SET('ORDER')
  *SET ORDER TO TAG (lcTmpLine1)
  IF SEEK(lcCounter)
  
    *B604475,1 KHM 05/15/2001 (Begin) Acumolating the amount instead of
    *B604475,1                unit cost.
    *SUM REST unitcost TO lnTotAmt&lcCounter ;
        WHILE cBomTyp+MfgCode+Item+IClr+Style = lcCounter

    SCAN REST WHILE cBomTyp+MfgCode+Item+IClr+Style = lcCounter
      IF SEEK(ShipNo+'P'+cTktNo+Style+STR(lineno,6)+'3','PosLn')
        lnTotAmt&lcCounter = lnTotAmt&lcCounter + (unitcost * PosLn.TotQty)
      ENDIF
    ENDSCAN

    *B604475,1 KHM 05/15/2001 (End)
    
    IF lnTotAmt&lcCounter <> 0
      SELECT (lcTmpFil)
      APPEND BLANK
      REPLACE cCost WITH STR(lnTotAmt&lcCounter,13,3),;
              cItem WITH lc&lcPType.Cost&lcCounter
      lnTotAmt = lnTotAmt + lnTotAmt&lcCounter
    ENDIF
  ENDIF  
ENDFOR
SELECT (lcTmpFil)
APPEND BLANK
APPEND BLANK
REPLACE cCost WITH STR(lnTotAmt,13,3) ,;
        cItem WITH "Total Shipment"

SELECT (lcTmpLine)
SET ORDER TO &lcOldTag
IF BETWEEN(lnRecNum,1,RECCOUNT())
  GOTO lnRecNum
ENDIF  
SELECT(lnAlias)
*--end

*!**************************************************************************
*! Name      : lfADDOPTIN
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/06/2001
*! Purpose   : Add a new Option to the menu Bar "Edit pack information"
*!**************************************************************************
*! Example   : lfADDOPTIN()
*!**************************************************************************
*!C102225,1 MHM 
FUNCTION lfADDOPTIN

IF !llImpCost 
  *-mhm2000
  *DEFINE BAR 11 OF _OPTIONPOP PROMPT 'Edit pack information'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR  lfCkEOF(lcPoLine)
  DEFINE BAR 11 OF _OPTIONPOP PROMPT 'Edit style information'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR  lfCkEOF(lcPoLine)
  *-mhm2000

  ON SELECTION BAR 11 OF _OPTIONPOP DO lpEdtScr IN GMAMAIN
ELSE
  IF llDispPric 
    *-mhm2000
    *DEFINE BAR 14 OF _OPTIONPOP PROMPT 'Edit pack information'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR lfCkEOF(lcPoLine)
    DEFINE BAR 14 OF _OPTIONPOP PROMPT 'Edit style information'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR lfCkEOF(lcPoLine)
    *--mhm2000

    ON SELECTION BAR 14 OF _OPTIONPOP DO lpEdtScr IN GMAMAIN
  ELSE
    *--mhm2000
    *DEFINE BAR 13 OF _OPTIONPOP PROMPT 'Edit pack information'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR lfCkEOF(lcPoLine)
    DEFINE BAR 13 OF _OPTIONPOP PROMPT 'Edit style information'  SKIP FOR (lnactfolder<>2) OR laScrMode[1] OR lfCkEOF(lcPoLine)
    *-mhm2000
    ON SELECTION BAR 13 OF _OPTIONPOP DO lpEdtScr IN GMAMAIN
  ENDIF  
ENDIF  
*-End 

*!**************************************************************************
*! Name      : lpEdtScr            
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/06/2001        
*! Purpose   : Display Screen to Edit the pack information.
*!*************************************************************
*! Example   : DO lpEdtScr
*!*************************************************************
*!C102225,1 MHM
PROCEDURE lpEdtScr
PRIVATE lcEscInt, lcTabInt, lcHldBtb, lcAltB

m.lnmpcube = (m.Nmpdim*m.NmPDepth*m.NmPcube)/1728

lcEscInt = ON('KEY','ESC')
lcTabInt = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcAltB   = ON('KEY','ALT+B')    


ON KEY LABEL ESC DO lfVCan
DO (gcScrDir+"PO\POEDTPAK.SPX")

ON KEY LABEL ESC      &lcEscInt
ON KEY LABEL TAB      &lcTabInt
ON KEY LABEL BACKTAB  &lcHldBtb
ON KEY LABEL ALT+B    &lcAltB


*!**************************************************************************
*! Name      : lfGETVAR
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/06/2001
*! Purpose   : Get fields from style file
*!**************************************************************************
*! Example   : lfGETVAR()
*!**************************************************************************
*!C102225,1 MHM
FUNCTION lfGETVAR

REPLACE &lcPOLine..Ninpack WITH STYLE.Ninpack;
        &lcPOLine..Nmpack  WITH STYLE.Nmpack;
        &lcPOLine..Nmpdim  WITH STYLE.Nmpdim;
        &lcPOLine..NmPDepth WITH STYLE.NmPDepth;
        &lcPOLine..NmPcube WITH STYLE.NmPcube;
        &lcPOLine..NmpWeight WITH STYLE.NmpWeight

*--mhm2000
REPLACE &lcPOLine..cHTSNo WITH STYLE.cHTSNo   
*-mhm2000

m.Ninpack          = STYLE.Ninpack
m.Nmpack           = STYLE.Nmpack
m.Nmpdim           = STYLE.Nmpdim
m.NmPDepth         = STYLE.NmPDepth
m.NmPcube          = STYLE.NmPcube
m.NmpWeight        = STYLE.NmpWeight
m.lnMPcube         = (m.Nmpdim*m.NmPDepth*m.NmPcube)/1728
*--mhm2000
m.cHTSNo           =STYLE.cHTSNo 
*-mhm2000

*!**************************************************************************
*! Name      : lfVOk
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/05/2001
*! Purpose   : validate Ok Button
*!**************************************************************************
*! Example   : =lfVOk()
*!**************************************************************************
*! C102225,1 MHM 
FUNCTION lfVOk

REPLACE &lcPOLine..Ninpack WITH m.Ninpack ;
        &lcPOLine..Nmpack  WITH m.Nmpack;
        &lcPOLine..Nmpdim  WITH m.Nmpdim;
        &lcPOLine..NmPDepth WITH m.NmPDepth;
        &lcPOLine..NmPcube WITH m.NmPcube;
        &lcPOLine..NmpWeight WITH m.NmpWeight

*--mhm2000
REPLACE &lcPOLine..cHTSNo WITH m.cHTSNo   
*-mhm2000

m.lnmpcube = (m.Nmpdim*m.NmPDepth*m.NmPcube)/1728

CLEAR READ
*!**************************************************************************
*! Name      : lfVMpk
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/05/2001
*! Purpose   : validate master pack cube
*!**************************************************************************
*! Example   : =lfVMpk()
*!**************************************************************************
*!*! C102225,1 MHM 
FUNCTION lfVMpk

m.lnmpcube = (m.Nmpdim*m.NmPDepth*m.NmPcube)/1728
SHOW GET m.lnMpCube
*--end lfVMpk
*!**************************************************************************
*! Name      : lfVCan
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/05/2001
*! Purpose   : validate Cancel Button
*!**************************************************************************
*! Example   : =lfVCan()
*!**************************************************************************
*!*! C102225,1 MHM 
FUNCTION lfVCan

IF laScrMode[2]
  CLEAR READ
  RETURN
ENDIF
m.Ninpack  = &lcPOLine..Ninpack 
m.Nmpack   = &lcPOLine..Nmpack  
m.Nmpdim   = &lcPOLine..Nmpdim  
m.NmPDepth = &lcPOLine..NmPDepth
m.NmPcube  = &lcPOLine..NmPcube 
m.NmpWeight= &lcPOLine..NmpWeight 
m.lnmpcube = (m.Nmpdim*m.NmPDepth*m.NmPcube)/1728
*--mhm2000
m.cHTSNo   = &lcPOLine..cHTSNo
*-mhm2000

CLEAR READ
*!**************************************************************************
*! Name      : lfVWhn
*! Developer : Mohamed Shokry (MHM)
*! Date      : 06/05/2001
*! Purpose   : validate When enter screen
*!**************************************************************************
*! Example   : =lfVWhn()
*!**************************************************************************
*! C102225,1 MHM 
FUNCTION lfVWhn

IF laScrMode[2]
  m.lnmpcube = (m.Nmpdim*m.NmPDepth*m.NmPcube)/1728
  SHOW GET m.Ninpack    DISABLE
  SHOW GET m.Nmpack     DISABLE
  SHOW GET m.Nmpdim     DISABLE
  SHOW GET m.NmPDepth   DISABLE
  SHOW GET m.NmPcube    DISABLE
  SHOW GET m.NmpWeight  DISABLE
  SHOW GET m.lnmpcube   DISABLE
  SHOW GET PBOK         DISABLE
  *--mhm2000
  SHOW GET m.cHTSNo     DISABLE
  *-mhm2000
ENDIF


*:**************************************************************************
*:* Name        : lfNewVersn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/03/2001
*:* Purpose     : Add a new version number for the current pack id for certain account
*:* Entry       : C101945,1
*:***************************************************************************
*:* Called from : Pack Generation screen
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfNewVersn()
*:***************************************************************************
FUNCTION lfNewVersn
PRIVATE lcVer
*--Get current pack values
SELECT SPCK_HDR
SCATTER MEMV MEMO
lcVer = M.CPKVERSION

*--Disable Lock State
REPLACE LLOK_STAT WITH .F.

*--Get a new version number
SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKVERSION = 'P'+laData[1]+laData[2]
  M.CPKVERSION  = SPCK_HDR.CPKVERSION 
ENDSCAN
M.CPKVERSION = STR(VAL(M.CPKVERSION)+1,4)

*APPEND BLANK
*GATHER MEMVAR MEMO

*--Update version in laData
laData[9] = M.CPKVERSION

*--Update version in laUsrFields
lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKVERSION'),1)
laUsrFields[lnFieldPos,6] = M.CPKVERSION


SELECT * FROM SPck_Lin;
  WHERE Type + Account   + Pack_ID + CPKVERSION + Style;
  =     'P'  + laData[1] + laData[2] + lcVer ;
  INTO DBF (gcWorkDir + lcLinFil)  
SELECT(lcLinFil)
INDEX ON Type+Account+Pack_ID+CPKVERSION+Style TAG StyPack ADDITIVE

*--Update Version and lock state in detail file
GO TOP
REPLACE ALL CPKVERSION WITH M.CPKVERSION ,;
            LLOK_STAT  WITH .F. 

=SEEK('P'+laData[1]+laData[2]+laData[9],'SPCK_HDR')

*--Turn on Add mode
laScrMode    = .F.  
laScrMode[4] = .T.  

*--Ask to undo updates
llCUpDate = .T.

_CUROBJ = OBJNUM(laData[3])

*-- end of lfNewVersn.

*:**************************************************************************
*:* Name        : lfVerCount
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/08/2001
*:* Purpose     : browse a screen if there are more than one version of the entered pack id
*:* Entry       : C101945,1
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVerCount()
*:***************************************************************************
FUNCTION lfVerCount
PRIVATE lcOldAlias,lcAlias
lcAlias = ''
lcOldAlias = SELECT()
laScrMode    = .F.   
lnVersions = 0    && lnVersions--> Holds number of versions of this pack id for this account

SELECT SPCK_HDR
=gfOpenFile(gcDataDir+'SPCK_HDR','SPCK_HDRVr','SH',@lcAlias,.T.)
SELECT (lcAlias)
=SEEK('P'+laData[1]+laData[2],(lcAlias))
SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKVERSION = 'P'+laData[1]+laData[2]
  lnVersions = lnVersions + 1  
ENDSCAN
USE IN (lcAlias)
  
IF lnVersions > 1
  PRIVATE lnBrowVer
  lnBrowVer = gfModalGen("INM00000B42015","Dialog",.F.,.F.,;
                  'The entered Pack ID has more than one version for the same account, Please select one.')
  *'There are more than one version of the selected pack id for the same account'
  IF lnBrowVer=1   && BROWSE
    PRIVATE laBrowVer,lcBrTitle
    DIME laBrowVer[10]
    lcBrTitle  = 'Available Packs'            
    lcBrFields = "Account           :R :H='Account'      :16," +;
                 "Pack_ID           :R :H='Pack_ID'      :16," +;
                 "Desc              :R :H='Desc'         :30," +;
                 "CPKVERSION        :R :H='Version'      :7,"  +;
                 "cDivision         :R :H='Devision'     :8,"  +;
                 "Season            :R :H='Season'       :8,"  +;
                 "NPKSLPRICE        :R :H='Selling Price':13," +;
                 "STYLEUPC.CUPCNUM1 :R :H='UPC'          :7,"  +;  
                 "STYLEUPC.CUPCNUM2 :R :H=''             :6,"  +;  
                 "STYLEUPC.CUPCNUM3 :R :H=''             :2"            
    SELECT SPCK_HDR
    lcKey = "'P'+laData[1]+laData[2]"
    =gfBrows(lcKey,"Account, Pack_ID   , Desc     ,cDivision  ,Season,;
              LRANGE , NPKSLPRICE,LASSORTED, CPKVERSION, CPKCOLOR" ;  
              ,"laBrowVer",lcBrTitle)
    IF !EMPTY(laBrowVer)
      =SEEK('P'+laBrowVer[1]+laBrowVer[2]+laBrowVer[9],'SPCK_HDR')
      laScrmOde[2] = .T.               && Turn to view mode
    ELSE
      laScrmOde[1] = .T.               && Turn to SELECT mode
    ENDIF
  ELSE              && REENTER
    laScrmOde[1] = .T.                 && Turn to select mode
  ENDIF                              
ELSE  && ONLY ONE VERSION
  laScrmOde[2] = .T.                   && Turn to view mode
ENDIF
SELECT (lcOldAlias)
*-- end of lfVerCount.

*:**************************************************************************
*:* Name        : lfvDispClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/11/2001
*:* Purpose     : Valid function for updating the screen from OG
*:* Entry       : C101945,1
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvDispClr()
*:***************************************************************************
FUNCTION lfvDispFld
PARAMETER llReturn,lcFldName,lnPos
llReturn=.T.
IF TYPE('LAUSRFIELD') <> "U" AND AT('.',lcFldName)=0
  lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
  IF lnFieldPos > 0 
    lcVar = SYS(18)
    lcVal = IIF('INVB' $ lcVar , EVALUATE(lcVar)=1 , EVALUATE(lcVar) )
    laUsrFields[lnFieldPos,6] = lcVal
    laOGFxFlt[lnFieldPos,6]   = lcVal

    *B126325,1 NNA 03/20/2005 (Begin) If length of laData is < lnPos then increase this Length 
    *B126325,1 NNA             to be Applicable with the lnPos
    IF ALEN(laData,1) <lnPos
      DECLARE laData[lnPos]
    ENDIF
    *B126325,1 NNA (End)

    laData[lnPos]             = lcVal
    SHOW GET laData[lnPos] LEVEL RDLEVEL()-1
  ENDIF
ENDIF
RETURN llReturn
*-- end of lfvPack.

*:**************************************************************************
*:* Name        : lfUpdUsrFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/11/2001
*:* Purpose     : update user defined field values
*:* Entry       : C101945,1
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdUsrFl()
*:***************************************************************************
FUNCTION lfUpdUsrFl
*PARAMETERS lcFldName,lcFldValue
lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue
*-- end of lfUpdUsrFl.

*:**************************************************************************
*:* Name        : lfASSRT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/12/2001
*:* Purpose     : Check for quantities not to be more than 1 for all sizes
*:* Entry       : C101945,1
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfASSRT()
*:***************************************************************************
FUNCTION lfASSRT
PRIVATE lnOldSlct,lnCnt,lcQTY
lnOldSlct = SELECT()
*--if lcOldVal is .t. nothing to do Because we allow to put any quantities
IF lcOldVal=.F.
  SELECT (lcLinFil)
  SCAN
    FOR lnCnt = 1 TO 8
      lcQTY = 'Qty'+STR(lnCnt,1)
      IF &lcQTY. > 1        
        llAssrtRet = .F.   && Return .F. by the valid function
        *--If there is any quantity More than 1 then Display a warning 
        =gfModalGen("INM00000B42001","Dialog",.F.,.F.,;
                 "There are sizes with quantities more than 1, this Pack ID cannot be assorted.")
        EXIT
      ENDIF
    ENDFOR
    IF llAssrtRet=.F.
      EXIT
    ENDIF
  ENDSCAN

  IF llAssrtRet=.F.
    laData[8] = .F.
    SHOW GET laData[8]
  ENDIF
      
ENDIF

*--Set old value
IF llAssrtRet
  lcOldVal = !laData[8]  
ENDIF  

SELECT(lnOldSlct)
*-- end of lfASSRT.

*!**************************************************************************
*! Name      : lfGMAPacks
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/31/2001
*! Purpose   : Create Temp. file for generic & account packs GMA
*! Reference : C101946,1
*!**************************************************************************
*! Example   : =lfGMAPacks()
*!**************************************************************************
*
FUNCTION lfGMAPacks
PRIVATE lnPacks , lcVersion

*! C102570,1 MHM 04/20/2002 add color size version to temp file[Start]
*CREATE DBF (gcWorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),cPkVersion C(4),nPkSlPrice N(8,2),;
         Season C(6),cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),;
         TotQty N(6),PackQty N(6))
*INDEX ON Account + Pack_Id + cPkVersion TAG (lcPackHdr)
*SELECT Spck_Lin
*SET RELATION TO Type+Account+Pack_Id+cPkVersion INTO SPck_Hdr ADDITIVE
*lcVersion = IIF(laScrMode[2],OrdLine.Pack_ID + OrdLine.cPkVersion,'')
*C200479,1 HBG 30/12/2002 Add lRange & lPCkPrPiec fields to the temp file [Begin]
*CREATE DBF (gcWorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),cPkVersion C(4),nPkSlPrice N(8,2),;
         Season C(6),cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),;
         TotQty N(6),PackQty N(6),cPkColor C(6),cPckSize C(3),lpckprpiec L(1))
CREATE DBF (gcWorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),cPkVersion C(4),nPkSlPrice N(8,2),;
         Season C(6),cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),;
         TotQty N(6),PackQty N(6),cPkColor C(6),cPckSize C(3),lpckprpiec L(1),lRange L(1))
*C200479,1 [End]

INDEX ON Account + Pack_Id +cpkcolor+cpcksize+ cPkVersion TAG (lcPackHdr)

SELECT Spck_Lin

SET RELATION TO Type+Account+Pack_Id+cPkColor+cpcksize+ cPkVersion INTO SPck_Hdr ADDITIVE
lcVersion = IIF(laScrMode[2],OrdLine.Pack_ID +OrdLine.cPkColor +OrdLine.cpcksize+OrdLine.cPkVersion,'')
*! C102570,1 MHM 04/20/2002 [End]

lnPacks = 0
WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT

*! C102570,1 MHM 04/20/2002 add color size version [Start]
*Account + Pack_Id +cpkcolor+cpcksize+ cPkVersion 
*IF SEEK('P'+laData[2]+lcVersion,'SPck_Lin')
   *LOCATE REST WHILE Type+Account ='P'+laData[2] ;
   *            FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
IF SEEK('P'+laData[2]+lcVersion,'SPck_Lin')
  LOCATE REST WHILE Type+Account+Pack_Id+cpkcolor+cpcksize+ cPkVersion ='P'+laData[2] ;
              FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]

*! C102570,1 MHM 04/20/2002 [End]

  lnPacks = IIF(FOUND(),1,0)
  SELECT SPck_Lin

  *! C102570,1 MHM 04/20/2002 add color size version [Start]
  *lcPack_Id = SPck_Lin.Pack_Id + SPck_Lin.cPkVersion
  lcPack_Id = SPck_Lin.Pack_Id + SPck_Lin.cpkcolor + SPck_Lin.cpcksize + SPck_Lin.cPkVersion 
  m.cPkColor = SPck_Hdr.cPkColor
  m.cPckSize = SPck_Hdr.cPckSize
  *! C102570,1 MHM 04/20/2002 [End]

  m.cPkVersion = SPck_Hdr.cPkVersion
  m.Desc = SPck_Hdr.Desc
  m.Season = SPck_Hdr.Season
  m.cDivision = SPck_Hdr.cDivision
  m.nPkSlPrice = SPck_Hdr.nPkSlPrice
  m.lpckprpiec = SPck_Hdr.lpckprpiec 
  *C200479,1 HBG 30/12/2002 update lRange field [Begin]
  m.lRange = SPck_Hdr.lRange 
  *C200479,1 [End]

  STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty  

  *! C102570,1 MHM 04/20/2002 add color size version [Start]
  *SCAN REST WHILE Type+Account+Pack_id+cPkVersion = 'P'+laData[2]+lcVersion ;
  *          FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  SCAN REST WHILE Type+Account+Pack_Id+cpkcolor+cpcksize+ cPkVersion+Style = 'P'+laData[2]+lcVersion;
            FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  *! C102570,1 MHM [End]
    *--improve speed of the Pack Screen
    *WAIT WINDOW lcStyHdr+Style+"\"+Pack_id NOWAIT

    *! C102570,1 MHM 04/20/2002 add color size version [Start]
    *IF lcPack_Id # SPck_Lin.Pack_Id + SPck_Lin.cPkVersion
    *  INSERT INTO (lcPackHdr) ;
    *  (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,TotQty);
    *   VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
    *        m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
    IF lcPack_Id # SPck_Lin.Pack_Id + SPck_Lin.cpkcolor + SPck_Lin.cpcksize+ SPck_Lin.cPkVersion  
    
      
      *C200479,1 HBG 30/12/2002 Update lRange & lPCkPrPiec fields in the temp file [Begin]
      *INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
       TotQty,cPkColor,cPckSize,lpckprpiec );
       VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
            m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
            m.cPkColor,m.cPckSize,m.lpckprpiec)
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
       TotQty,cPkColor,cPckSize,lpckprpiec,lRange);
       VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
            m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
            m.cPkColor,m.cPckSize,m.lpckprpiec,m.lRange)
      *C200479,1 [End]
            
      m.cPkColor = SPck_Hdr.cPkColor
      m.cPckSize = SPck_Hdr.cPckSize
    *! C102570,1 MHM 04/20/2002 [End]

      m.cPkVersion = SPck_Hdr.cPkVersion
      m.nPkSlPrice = SPck_Hdr.nPkSlPrice 
      m.lpckprpiec = SPck_Hdr.lpckprpiec 
      *C200479,1 HBG 30/12/2002 Update lRange field [Begin]
      m.lRange = SPck_Hdr.lRange 
      *C200479,1 [End]
      lcPack_Id = SPck_Lin.Pack_Id + SPck_Lin.cpkcolor + SPck_Lin.cpcksize + SPck_Lin.cPkVersion
      m.Desc = SPck_Hdr.Desc
      m.Season = SPck_Hdr.Season
      m.cDivision = SPck_Hdr.cDivision
      lnPacks = lnPacks + 1
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    ENDIF
    lnPackQty = lnPackQty+Spck_Lin.TotQty
    lnPackWip = lnPackWip+Style.TotWip
    lnPackOrd = lnPackOrd+Style.TotOrd
    lnPackStk = lnPackStk+Style.TotStk
  ENDSCAN

  IF lnPacks > 0

    *! C102570,1 MHM 04/20/2002 add color size version [Start]
    *INSERT INTO (lcPackHdr) ;
    *(Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,TotQty);
    * VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
    *      m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
    *C200479,1 HBG 30/12/2002 Add lRange & lPCkPrPiec fields [Begin]
    *INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
     TotQty,cPkColor,cPckSize,lpckprpiec );
     VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
          m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
          m.cPkColor,m.cPckSize,m.lpckprpiec)
    INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
     TotQty,cPkColor,cPckSize,lpckprpiec,lRange);
     VALUES ('P',laData[2],lcPack_Id,m.Desc,m.Season,;
          m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
          m.cPkColor,m.cPckSize,m.lpckprpiec,m.lRange)
    *C200479,1 [End]

    *! C102570,1 MHM 04/20/2002 [End]

    m.cPkVersion = SPck_Hdr.cPkVersion
    m.nPkSlPrice = SPck_Hdr.nPkSlPrice 
    m.lpckprpiec = SPck_Hdr.lpckprpiec 
    *C200479,1 HBG 30/12/2002 Update lRange [Begin]
    m.lRange     = SPck_Hdr.lRange     
    *C200479,1 [End]
  ENDIF
ENDIF

WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
IF SEEK('P*****'+lcVersion,'SPck_Lin')

  SELECT SPck_Lin

  *! C102570,1 MHM 04/20/2002 add color size version [Start]
  *LOCATE REST WHILE Type+Account+cPkVersion='P*****';
  *            FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  LOCATE REST WHILE Type+Account+Pack_Id+cpkcolor+cpcksize+ cPkVersion+Style ='P*****';
              FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  *! C102570,1 MHM 04/20/2002 [End]

  lnPacks = IIF(FOUND(),1,0)              

  *! C102570,1 MHM 04/20/2002 add color size version [Start]
  *lcPack_Id = SPck_Lin.Pack_Id + SPck_Lin.cPkVersion
  lcPack_Id  = SPck_Lin.Pack_Id+SPck_Lin.cpkcolor+SPck_Lin.cpcksize+ SPck_Lin.cPkVersion 
  m.cPkColor = SPck_Hdr.cPkColor
  m.cPckSize = SPck_Hdr.cPckSize
  *! C102570,1 MHM 04/20/2002 [End]

  m.Desc = SPck_Hdr.Desc
  m.Season = SPck_Hdr.Season
  m.cDivision = SPck_Hdr.cDivision
  m.cPkVersion = SPck_Hdr.cPkVersion
  m.nPkSlPrice = SPck_Hdr.nPkSlPrice
  m.lpckprpiec = SPCK_HDR.lpckprpiec
  *C200479,1 HBG 30/12/2002 Update lRange [Begin]
  m.lRange     = SPck_Hdr.lRange     
  *C200479,1 [End]
  STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty

  *! C102570,1 MHM 04/20/2002 add color size version [Start]
  *SCAN REST WHILE Type+Account+Pack_Id+cPkVersion='P*****'+lcVersion;
  *   FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  SCAN REST WHILE Type+Account+Pack_Id+cpkcolor+cpcksize+ cPkVersion+Style='P*****'+lcVersion;
     FOR IIF(ALLTRIM(laData[14])='*',.T.,SPCK_HDR.Season=laData[14]) .AND. SPCK_HDR.cDivision=laData[15]
  *! C102570,1 MHM 04/20/2002 [End]

     *--improve speed of the Pack Screen
     *WAIT WINDOW lcStyHdr+Style+"\"+Pack_id NOWAIT
  
    *! C102570,1 MHM 04/20/2002 add color size version [Start]
    *IF lcPack_Id # SPck_Lin.Pack_Id + SPck_Lin.cPkVersion
    *  INSERT INTO (lcPackHdr) ;
    *  (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,TotQty);
    *   VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
    *        m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
    IF lcPack_Id # SPck_Lin.Pack_Id+SPck_Lin.cpkcolor+SPck_Lin.cpcksize+ SPck_Lin.cPkVersion 
      *C200479,1 HBG 30/12/2002 Add lRange & lPCkPrPiec fields [Begin]  
      *INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
       TotQty,cpkcolor,cpcksize,lpckprpiec );
       VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
            m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
            m.cpkcolor,m.cpcksize,m.lpckprpiec)            
      INSERT INTO (lcPackHdr) ;
      (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
       TotQty,cpkcolor,cpcksize,lpckprpiec,lRange);
       VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
            m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
            m.cpkcolor,m.cpcksize,m.lpckprpiec,m.lRange)            
      *C200479,1 [End]
      m.cPkColor = SPck_Hdr.cPkColor
      m.cPckSize = SPck_Hdr.cPckSize

    *! C102570,1 MHM 04/20/2002 [End]

      m.cPkVersion = SPck_Hdr.cPkVersion
      m.nPkSlPrice = SPck_Hdr.nPkSlPrice
      m.lpckprpiec = SPCK_HDR.lpckprpiec
      *C200479,1 HBG 30/12/2002 Update lRange [Begin]
      m.lRange     = SPck_Hdr.lRange     
      *C200479,1 [End]
      lcPack_Id = SPck_Lin.Pack_Id + SPck_Lin.cpkcolor + SPck_Lin.cpcksize + SPck_Lin.cPkVersion
      m.Desc = SPck_Hdr.Desc
      m.Season = SPck_Hdr.Season
      m.cDivision = SPck_Hdr.cDivision
      lnPacks = lnPacks + 1
      STORE 0 TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty
    ENDIF
    lnPackQty = lnPackQty+Spck_Lin.TotQty
    lnPackWip = lnPackWip+Style.TotWip
    lnPackOrd = lnPackOrd+Style.TotOrd
    lnPackStk = lnPackStk+Style.TotStk
  ENDSCAN

  IF lnPacks > 0

    *! C102570,1 MHM 04/20/2002 add color size version [Start]
    *INSERT INTO (lcPackHdr) ;
    *(Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,TotQty);
    * VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
    *      m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty)
    *C200479,1 HBG 30/12/2002 Add lRange & lPCkPrPiec fields [Begin]
    *INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
     TotQty,cpkcolor,cpcksize,lpckprpiec );
     VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
          m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
          m.cpkcolor,m.cpcksize,m.lpckprpiec )
    INSERT INTO (lcPackHdr) ;
    (Type,Account,Pack_Id,Desc,Season,cDivision,cPkVersion,nPkSlPrice,TotWip,TotStk,TotOrd,;
     TotQty,cpkcolor,cpcksize,lpckprpiec,lRange);
     VALUES ('P','*****',lcPack_Id,m.Desc,m.Season,;
          m.cDivision,m.cPkVersion,m.nPkSlPrice,lnPackWip,lnPackStk,lnPackOrd,lnPackQty,;
          m.cpkcolor,m.cpcksize,m.lpckprpiec,m.lRange)
    *C200479,1 [End]
    
    m.cPkColor = SPck_Hdr.cPkColor
    m.cPckSize = SPck_Hdr.cPckSize
    *! C102570,1 MHM 04/20/2002  [End]

    m.cPkVersion = SPck_Hdr.cPkVersion
    m.nPkSlPrice = SPck_Hdr.nPkSlPrice
    m.lpckprpiec = SPck_Hdr.lpckprpiec 
    *C200479,1 HBG 30/12/2002 Update lRange [Begin]
    m.lRange     = SPck_Hdr.lRange     
    *C200479,1 [End]
  ENDIF
ENDIF
*-- End of lfGMAPacks.

*!**************************************************************************
*! Name      : lfGMAPrice
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/31/2001
*! Purpose   : Get the price for Style in a pack. 
*! Reference : C101946,1
*!**************************************************************************
*! Example   : =lfGMAPrice()
*!**************************************************************************
*
FUNCTION lfGMAPrice

*! C102570,1 MHM 04/20/2002 change the way of calculating price[Start]
*STORE 0 TO m.Gros_Price,m.Price,m.Disc_Pcnt
*m.lContract = lcOrdType<>'C' .AND. lfvContPri(Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
*IF !m.lContract
*  m.Gros_Price= lfGetprice(Style,lcPriceLvl,TotQty*&lcPackHdr..PackQty)
*  *-- get the cDiscCode From stydye in every case
*  lcDiscCode  = IIF(SEEK(Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
*  m.Disc_Pcnt = 0
*  IF !EMPTY(ALLTRIM(lcDiscCode))
    *-- Get the discount related filed to now which type whole Sale Or Retail sale Or Both.
*    DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
*    STORE '' To lcDisType , ldstartDte ,ldEndDate
    *-- Array to get the Discount affect for DecCode.
*    laDisType[1,1]  = 'CCOSTAFECT'
*    laDisType[1,2]  = 'lcDisType'
    *-- Array to get the start date For DescCode.
*    lastartDte[1,1] = 'START'
*    lastartDte[1,2] = 'ldstartDte'
*    *-- Array to get the end date For DescCode.
*    laEndDate[1,1]  = 'DENDATE'
*    laEndDate[1,2]  = 'ldEndDate'
*    = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
*    = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
*    = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
  
*    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
*      lnDisc_Pcnt = 0
*      =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
*      m.Disc_Pcnt = lnDisc_Pcnt
*    ENDIF
*  ENDIF 
*  m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
*ENDIF
*lnCurPrice = m.Price
*lnCurGrPrc = m.Gros_Price

IF Spck_Lin.nPck_price > 0
    lnCurPrice = Spck_Lin.nPck_price
    lnCurGrPrc = Spck_Lin.nPck_price
    m.Price    = Spck_Lin.nPck_price
ELSE
  =SEEK(Style,'STYLE')
    lnCurPrice  = STYLE.PriceA
    lnCurGrPrc  = STYLE.PriceA
    m.Price     = STYLE.PriceA
ENDIF

*! C102570,1 MHM 04/20/2002 [End]

lnTotPrice = lnTotPrice + lnCurPrice
INSERT INTO (lcTmpPrice) VALUE (Spck_Lin.Style , lnCurGrPrc , lnCurPrice , 0)
STORE 0 TO lnCurPrice , lnCurGrPrc

*-- End of lfGMAPrice.

*!**************************************************************************
*! Name      : lfGMARemov
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/31/2001
*! Purpose   : Remove all the Pack ID lines if one line is removed. 
*! Reference : C101946,1
*!**************************************************************************
*! Example   :  =lfGMARemov()
*!**************************************************************************
*
FUNCTION lfGMARemov

*-- Message < Are you sure you want to remove this order line? >
*-- Buttons <                    Yes     No                    >
IF gfModalGen('QRM32035B32000','ALERT',IIF(lcOrdType='C','contract','order'))= 1
  *-- Message < Order line has allocated quantity Allocation will be released.>
  *-- Buttons <                       Proceed    Cancel                       >
  IF TotCut > 0 AND gfModalGen('QRM32062B32005','ALERT') = 2
    RETURN
  ENDIF

  *-- Message < This line is included in a pack, do you want to remove the entire pack ? >
  *-- Buttons <                                Yes     No                                >
  IF gfModalGen('TRM00000B32000','DIALOG','','', 'This line is included in a pack,  do you want to remove the entire pack ?') = 2
    RETURN
  ENDIF
  
  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  PRIVATE lcCurPckID , lcCurVersn , lcCurStore
  lcCurStore = Store
  lcCurPckID = Pack_id + cPkColor + cPckSize
  lcCurVersn = cPkVersion
  SCAN FOR Store + Pack_id + cPkColor + cPckSize + cPkVersion = lcCurStore + lcCurPckID + lcCurVersn
    SCATTER MEMVAR MEMO 

    IF llStores
      lnTotPcs = lnTotPcs - m.TotQty
      lnTotAmt = lnTotAmt - m.TotQty*m.Price
    ELSE
      SELECT (lcOrdLine)
      =RLOCK()    
      REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
      UNLOCK

      IF TotCut > 0 
        =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
        =SEEK(IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6),'CutPick')
        SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
        SELECT CutPick
        SCAN REST WHILE TranCd+Order+cOrdLine = ;
                        IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6)
          IF !SEEK(CutPick.TranCd+CutPick.cTktNo+CutPick.cTktLineNo+;
                   CutPick.Order+CutPick.cOrdLine,lcAlocated)
            INSERT INTO (lcAlocated) ;
            (cTktno,TranCd,cTktLineNo,Order,cOrdLine,Style) VALUES ;
            (CutPick.cTktNo,CutPick.Trancd,CutPick.cTktLineNo,CutPick.Order,;
             CutPick.cOrdLine,CutPick.Style)
          ENDIF
          SELECT (lcAlocated)
          =RLOCK()
          REPLACE Qty1   WITH 0 ,;
                  Qty2   WITH 0 ,;
                  Qty3   WITH 0 ,;
                  Qty4   WITH 0 ,;
                  Qty5   WITH 0 ,;
                  Qty6   WITH 0 ,;
                  Qty7   WITH 0 ,;
                  Qty8   WITH 0 ,;
                  TotQty WITH 0
          UNLOCK
          SELECT (lcOrdHdr)
          =RLOCK()
          REPLACE TotCut WITH TotCut - CutPick.TotQty
          UNLOCK
        ENDSCAN
        =gfCloseFile('CUTPICK')
        SELECT (lcOrdLine)
      ENDIF
      IF laScrMode[4] .AND. !EMPTY(laData[43]) .AND. ;
         SEEK(STR(BulkLineNo,6),lcBulkOrd) 
        SELECT (lcBulkOrd)
        =RLOCK()
        REPLACE USED1 WITH USED1 - m.Qty1 ,;
                USED2 WITH USED2 - m.Qty2 ,;
                USED3 WITH USED3 - m.Qty3 ,;
                USED4 WITH USED4 - m.Qty4 ,;
                USED5 WITH USED5 - m.Qty5 ,;
                USED6 WITH USED6 - m.Qty6 ,;
                USED7 WITH USED7 - m.Qty7 ,;
                USED8 WITH USED8 - m.Qty8 ,;
                TOTUSED WITH TOTUSED - m.TotQty
        UNLOCK
      ENDIF
      laData[35] = laData[35] - IIF(FLAG='N' OR llUpdBook,m.TotQty,0)
      laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,m.TotQty*m.Price,0)
      laData[39] = laData[39] + IIF(FLAG='N' OR llUpdBook,0,m.TotQTy)
      laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,m.TotQTy)*m.Price
      laData[41] = laData[41] - m.TotQty
      laData[42] = laData[42] - m.TotQty*m.Price
      SELECT (lcOrdHdr)
      =RLOCK()
      REPLACE BOOK      WITH laData[35] ,;
              BOOKAMT   WITH laData[36] ,;
              CANCEL    WITH laData[39] ,;
              CANCELAMT WITH laData[40] ,;
              OPEN      WITH laData[41] ,;
              OPENAMT   WITH laData[42]
      UNLOCK
      IF laScrMode[3] AND !llUpdBook
        =gfwCodePop(@laCodes,'CCANCRESON','D')
        INSERT INTO (lcOrdCanLn) ;
         (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
          Style,Account,Store,Dyelot,Price) VALUES ;
         (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
          m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
          m.Style,m.Account,m.Store,m.Dyelot,m.Price)      
        
        SELECT (lcOrdCanLn)
        STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
        STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
        =gfwCodePop(@laCodes,'CCANCRESON','T')
        STORE gdSysDate TO m.Cancelled
        DO (gcScrDir+"SOORDCLN.SPX")
        STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
        =gfAdd_Info(lcOrdCanLn)
      ENDIF 
    ENDIF
  
    IF llBomVarnt
      SELECT (lcT_BomVar)
      =SEEK("SO"+laData[1]+STR(&lcOrdLine..LineNo,6))
      SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
        REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
      ENDSCAN
      DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
    ENDIF

    SELECT IIF(llStores,lcTempLine,lcOrdLine)
    DELETE
  ENDSCAN
  
  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  GO TOP
  
  lcDispDet = IIF(EOF(),'DISABLE','')
  SCATTER MEMVAR
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI3' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI4' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI5' &lcDispDet ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
    SHOW GET pbClose ENABLE
    _CUROBJ = OBJNUM(ibInv200E)
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32)  &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh321) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh322) &lcDispDet ONLY
    =lfRefresh(lcWinCh321)
    _CUROBJ = OBJNUM(ibBrowOrd)
  ENDIF
  IF EOF()
    SHOW GET llMultiSt ENABLE
    SHOW GET pbNew     ENABLE 
    SHOW GET pbPacks   ENABLE
    IF llMultiSt
      SHOW GET pbTemplate ENABLE
    ENDIF
   _CUROBJ = OBJNUM(pbNew)
  ENDIF
  STORE .T. TO llCUpdate
ENDIF
*-- End of lfGMARemov


*!**************************************************************************
*! Name      : GMADUMMY
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/20/2001
*! Purpose   : Dummy function with no use. 
*! Reference : C101946,1
*!**************************************************************************
*! Example   : lfGMADUMMY()
*!**************************************************************************
*
FUNCTION lfGMADUMMY
RETURN .T.
*-- End of lfGMADUMMY

*!*************************************************************
*! Name      : lfvFob
*! Developer : Ahmed Maher (AMH)
*! Date      : 09/12/2001
*! Purpose   : Valid function of Fob Code.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C200212
*:*************************************************************
*
FUNCTION lfvFob
PARAMETERS lcRetVal

PRIVATE lcVar, lnFobPos, lcFob, lcOgTyp
lcRetVal = .T.
lcVar = SYS(18)
IF TYPE('LAUSRFIELD') = "U"
  RETURN lcRetVal
ENDIF
lnFobPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CFOBCODE'),1)
lcOgTyp = STRTRAN(LAOGOBJTYP[lnFobPos,2],'[','(')
lcOgTyp = STRTRAN(lcOgTyp,']',')')
IF !(lcVar == lcOgTyp)
  RETURN lcRetVal
ENDIF
lcFob = LAOGFXFLT[lnFobPos,6]
laData[9] = IIF(EMPTY(lcFob),'',gfCodDes(lcFob,'CFOBCODE'))
SHOW GET laData[9] LEVEL RDLEVEL()-1
RETURN lcRetVal
*-- end of lfvFob.

*!*************************************************************
*! Name      : lfvOrigin
*! Developer : Ahmed Maher (AMH)
*! Date      : 09/12/2001
*! Purpose   : Valid function of Origin Code.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C200212
*:*************************************************************
*
FUNCTION lfvOrigin
PARAMETERS lcRetVal

PRIVATE lcVar, lnOrgnPos, lcOrigin, lcOgTyp
lcRetVal = .T.
lcVar = SYS(18)
IF TYPE('LAUSRFIELD') = "U"
  RETURN lcRetVal
ENDIF
lnOrgnPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CORIGINCOD'),1)
lcOgTyp = STRTRAN(LAOGOBJTYP[lnOrgnPos,2],'[','(')
lcOgTyp = STRTRAN(lcOgTyp,']',')')
IF !(lcVar == lcOgTyp)
  RETURN lcRetVal
ENDIF
lcOrigin = LAOGFXFLT[lnOrgnPos,6]
laData[8] = IIF(EMPTY(lcOrigin),'',gfCodDes(lcOrigin,'CORIGINCOD'))
SHOW GET laData[8] LEVEL RDLEVEL()-1
RETURN lcRetVal
*-- end of lfvOrigin.

*!*************************************************************
*! Name      : lfwGmaCode
*! Developer : Ahmed Maher (AMH)
*! Date      : 09/12/2001
*! Purpose   : When function of Fob & Origin Codes.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Due to C200212
*:*************************************************************
*
FUNCTION lfwGmaCode
PARAMETERS lcRetVal

IF ASCAN(laEvntTrig , PADR('WGMACODE',10)) <> 0
  lcRetVal = .T.
ELSE
  lcRetVal = .F.
ENDIF
RETURN lcRetVal
*-- end of lfwGmaCode.

*!*************************************************************
*! Name      : lfwGmaCode
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 09/23/2001
*! Purpose   : Adding the following function in order to disable the
*!             FOB and Origin fields.
*!*************************************************************
*C200233,1 KHM 09/23/2001
*:*************************************************************
FUNCTION lfDisaFOOR

IF lcAtype = "P" AND (laScrMode[3] OR laScrMode[4])
  SHOW GET laData[9] DISABLE
  SHOW GET laData[8] DISABLE
ENDIF

*!*************************************************************
*! Name      : lfClcWipGma
*! Developer : WAB - Walid A. Wahab
*! Date      : 10/22/2001
*! Purpose   : caluclate the WIP depen on the budjet qty only 
*!*************************************************************
*B605049,1  WAB 
*:*************************************************************
FUNCTION lfClcWipGma

RETURN

*!**************************************************************************
*! Name      : lfChgMode   
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/11/2001
*! Purpose   : Make the Mode of The OG of the Custom fields always View
*!**************************************************************************
*! Reference : C102452,1
*!**************************************************************************
*! Example   : =lfChgMode()
*!**************************************************************************
FUNCTION lfChgMode   

laScrMode = .F.
*-- IF Sales Description field or Product descreption field Go to View mode.
IF ALLTRIM(&lcOGArrName[lnOGArrPos,1]) = 'CSTYCATG'   OR;
   ALLTRIM(&lcOGArrName[lnOGArrPos,1]) = 'CSTYCLASS'  OR;
   ALLTRIM(&lcOGArrName[lnOGArrPos,1]) = 'CSTYSBCLAS' OR;
   ALLTRIM(&lcOGArrName[lnOGArrPos,1]) = 'CSTYSBCLS2' OR;
   ALLTRIM(&lcOGArrName[lnOGArrPos,1]) = 'CSTYSBCLS3'
  laOGObjCnt[lnOGArrPos] = .F.
  = lfOGShowGet(&lcOGArrName[lnOGArrPos,1])
ELSE  && else OG Edit mode.
  laOGObjCnt[lnOGArrPos] = .T.
  = lfOGShowGet(&lcOGArrName[lnOGArrPos,1])
ENDIF

*-- end of lfChgMode  .

*!**************************************************************************
*! Name      : lfPrntRun
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/17/2002
*! Purpose   : To do not display the printer screen
*!**************************************************************************
*! Example   : = lfPrntRun()
*!**************************************************************************
*! Due to C102514,1
*!**************************************************************************
FUNCTION lfPrntRun

*--200326,1 MHM change the name as new report created [Start]
*IF TYPE("lcFormName") = "C" AND (lcFormName = "ARGRSPDP")
IF TYPE("lcFormName") = "C" AND (lcFormName = "ARGRSPGM")
*--200326,1 MHM [End]
  IF lcRpReport = "D"
    gcDevice = 'PRINTER'
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF  
ELSE
  RETURN .F.
ENDIF

*!**************************************************************************
*! Name      : lfGetShpAm
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetShpAm()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfGetShpAm

lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
lcTmpFle = &lcSVarFle..lcTmpFle
SET ORDER TO (lcTmpFle) IN (lcTmpFle)

REPLACE &lcSVarFle..lcPack_id WITH ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+;
                                   ORDLINE.cPkVersion 
IF !EMPTY(ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+ORDLINE.cPkVersion)
  = SEEK('P'+ORDLINE.cPckAcc+ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+;
         ORDLINE.cPkVersion,'SPCK_HDR') 
  IF SEEK(ORDLINE.cPckAcc+ORDLINE.Pack_id+ORDLINE.cPkColor+ORDLINE.cPckSize+;
          ORDLINE.cPkVersion,lcTmpFle)
    *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec from ORDLINE instead of SPCK_HDR [Begin]      
    *IF SPCK_HDR.nPkSlPrice = 0 OR SPCK_HDR.lpckprpiec OR SPCK_HDR.lrange
    IF ORDLINE.nPkSlPrice = 0 OR ORDLINE.lpckprpiec OR ORDLINE.lrange
    *C200479,1 [End]
      REPLACE &lcSVarFle..lnShipAmnt WITH &lcSVarFle..lnShipAmnt + (ORDLINE.TotQty * ORDLINE.Price) 
    ENDIF
    RETURN
  ELSE
    INSERT INTO (lcTmpFle) (cPack_id) VALUE (ORDLINE.cPckAcc+ORDLINE.Pack_id+ORDLINE.cPkColor+;
                                             ORDLINE.cPckSize+ORDLINE.cPkVersion)
  ENDIF 
  *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec from ORDLINE instead of SPCK_HDR [Begin]       
  *IF SPCK_HDR.nPkSlPrice = 0 OR SPCK_HDR.lpckprpiec OR SPCK_HDR.lrange
  IF ORDLINE.nPkSlPrice = 0 OR ORDLINE.lpckprpiec OR ORDLINE.lrange
  *C200479,1 [End]
    REPLACE &lcSVarFle..lnShipAmnt WITH &lcSVarFle..lnShipAmnt + (ORDLINE.TotQty * ORDLINE.Price) 
  ELSE
    REPLACE &lcSVarFle..lnShipAmnt WITH &lcSVarFle..lnShipAmnt + (ORDLINE.nPackNo * ORDLINE.nPkSlPrice)
  ENDIF
ELSE  
  *B127452,1 NNA 06/20/2005 (Begin) if the ordline's line is picked then get the totpik qty not the totqty.
  *REPLACE &lcSVarFle..lnShipAmnt WITH &lcSVarFle..lnShipAmnt + (ORDLINE.TotQty * ORDLINE.Price) 
  REPLACE &lcSVarFle..lnShipAmnt WITH &lcSVarFle..lnShipAmnt + IIF(OrdLine.Picked,(ORDLINE.TotPik * ORDLINE.Price),;
         (ORDLINE.TotQty * ORDLINE.Price))
  *B127452,1 NNA (End)
ENDIF  

*!**************************************************************************
*! Name      : lfUpdHdr
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdHdr()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUpdHdr

lcSVarFle = lcConsInvH + 'A'
lcGMAInvH = &lcSVarFle..lcGMAInvH 
SELECT (lcGMAInvH)
LOCATE
SCATTER MEMVAR MEMO
SELECT (lcInvHdr)
LOCATE
GATHER MEMVAR MEMO
*C200484,1 Get carton\weight from pack_dhr as default and update the SO Inv.[Begin]
*          Update the invoice herder temp file from GMA temp file
SELECT (lcInvHdr)
DELETE ALL
SELECT (lcGMAInvH)
LOCATE
SCAN 
  SCATTER MEMVAR MEMO
  INSERT INTO (lcInvHdr) FROM MEMVAR
ENDSCAN
*C200484,1 Get carton\weight from pack_dhr as default and update the SO Inv.[end]

*!**************************************************************************
*! Name      : lfUPDTMPH
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUPDTMPH()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUPDTMPH


lcSVarFle = lcConsInvH + 'A'
lcGMAInvH = &lcSVarFle..lcGMAInvH 

SELECT (lcGMAInvH)
*C101945,1 TMI [Start] Ask only for order+store
*llInvPck = !EMPTY(m.PikTkt) AND SEEK(m.Order+m.Store+m.PikTkt,'Pack_Hdr')
llInvPck = SEEK(m.Order+m.Store,'Pack_Hdr')    
*C101945,1 TMI [End  ] 
    
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

  REPLACE DiscPcnt   WITH OrdHdr.Disc      ,;
          InvDate    WITH ldDefInvDate     ,;
          ShipDate   WITH ldDefInvDate     ,;
          dPostDate  WITH ldDefPstDate     ,;
          DueDate    WITH ldDueDate        ,;
          Dept       WITH OrdHdr.Dept      ,; 
          cFacCode   WITH OrdHdr.cFacCode  ,;
          Approval   WITH OrdHdr.Approval  ,;
          Appramt    WITH OrdHdr.ApprAmt   ,;
          Season     WITH OrdHdr.Season    ,;
          cDivision  WITH OrdHdr.cDivision ,;
          UpsZone    WITH Customer.UpsZone ,;
          Phone      WITH IIF(EMPTY(Ordhdr.Store),lcPhone,Customer.Phone1)  ,;
          cWareCode  WITH IIF(llPicked .AND. SEEK(m.Order+m.PikTkt,'PikTkt'),;
                              PikTkt.cWareCode,OrdHdr.cWareCode) ,;
          Trde_Disc  WITH lnTerDiscR ,;
          Tax_Rate   WITH IIF(laSetups[9,2] <> 'Y',0,;
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
            Cartons   WITH Pack_Hdr.Tot_Cart ,;
            BOL_No    WITH Pack_Hdr.Bill_ladg    
  ENDIF
  *C200484,1 Get carton\weight from pack_dhr as default and update the SO Inv.[Begin]
  IF SEEK(m.Order+m.Store,'Pack_Hdr')
    REPLACE Weight    WITH Pack_Hdr.Tot_Wght ,;
            nCartons  WITH Pack_Hdr.Tot_Cart ,;
            Cartons   WITH Pack_Hdr.Tot_Cart
  ENDIF
  *C200484,1 Get carton\weight from pack_dhr as default and update the SO Inv.[end]
 

ENDIF

lnCartons  = nCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))

    
REPLACE Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
        Ship      WITH Ship     + m.TotQty ,;
        Weight    WITH Weight   + IIF(llInvPck,0,m.TotQty*Style.nStyWeight) ,;
        nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
        Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
        Picked    WITH Picked  + m.TotPik ,;
        ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                       IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia)),;
        nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
        Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
        nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
    
IF llRevue             && Commented out for customer Revue only
  REPLACE Complete WITH OrdHdr.Complete ,;
          ShipDesc WITH gfCodDes(ShipVia,'SHIPVIA')
ENDIF          

*!**************************************************************************
*! Name      : lfUpdShip
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdShip()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUpdShip

lcSVarFle = lcConsInvH + 'A'
lcGMAInvH = &lcSVarFle..lcGMAInvH 

*B122625,1 NNA 04/29/2004 (Begin) if the Store has Shiped return because (lnShipAmnt) Became [0]
IF !&lcSVarFle..llClearVar
  RETURN
ENDIF  
*B122625,1 NNA (End)


SELECT (lcGMAInvH)
REPLACE ShipAmt   WITH &lcSVarFle..lnShipAmnt,;
        Discount  WITH -ShipAmt * DiscPcnt/100,;
        Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
        TotalChg  WITH IIF(llIsEngland,ShipAmt+Tax_Amt+Discount,0)
SELECT (lcCurAlias)               

*B122625,1 NNA 04/29/2004 (Begin) Replace (llClearVar) field with .F. to know that (lnShipAmnt) Became Zero
REPLACE &lcSVarFle..llClearVar WITH .F.
*B122625,1 NNA (End)

*!**************************************************************************
*! Name      : lfUpdShpAm
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfUpdShpAm()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUpdShpAm

lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
IF &lcPackHdr..lpckprpiec OR &lcPackHdr..lRange OR &lcPackHdr..nPkSlPrice = 0
  REPLACE &lcSVarFle..lnShipAmt WITH &lcSVarFle..lnShipAmt + &lcSVarFle..lnLinShAt
ELSE
  REPLACE &lcSVarFle..lnShipAmt WITH &lcSVarFle..lnShipAmt + (&lcPackHdr..nPkSlPrice*&lcPackHdr..PackQty)
ENDIF
*!**************************************************************************
*! Name      : lfUpdSAmnt
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfUpdSAmnt()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUpdSAmnt

m.ShipAmt = m.ShipAmt - TotQty*Price
lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
IF &lcPackHdr..lpckprpiec OR &lcPackHdr..lRange OR &lcPackHdr..nPkSlPrice = 0
  REPLACE &lcSVarFle..lnLinShAt WITH &lcSVarFle..lnLinShAt + TotQty*Price
ENDIF  

*!**************************************************************************
*! Name      : lfGtShpAmt
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfGtShpAmt()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfGtShpAmt


lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
m.ShipAmt = m.ShipAmt + &lcSVarFle..lnShipAmt 

*!**************************************************************************
*! Name      : lfGtShpAmt
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfGtShpAmt()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfGetAmnt

lcDVarFle = lcInvHdr+'A'
GO TOP IN (lcDVarFle)
laData[31] = &lcDVarFle..lnOldShp  + &lcDVarFle..lnShpAmt 

*!**************************************************************************
*! Name      : lfUpdShp
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfUpdShp()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfUpdShp

*C200410,1 HBG 23/09/2002 Calculate ship amount using the new field npackNo [Begin]
lcDVarFle = lcInvHdr+'A'
GO TOP IN (lcDVarFle)

lcTmpFle = &lcDVarFle..lcTmpFle
SET ORDER TO (lcTmpFle) IN (lcTmpFle)

IF !EMPTY(&lcPackHdr..Pack_id+&lcPackHdr..cpkColor+&lcPackHdr..cPckSize+&lcPackHdr..cPkVersion)
  IF SEEK(&lcPackHdr..Account+&lcPackHdr..Pack_id+&lcPackHdr..cpkColor+&lcPackHdr..cPckSize+&lcPackHdr..cPkVersion,lcTmpFle)
    IF &lcPackHdr..lpckprpiec OR &lcPackHdr..lRange OR &lcPackHdr..nPkSlPrice = 0
      REPLACE &lcDVarFle..lnShpAmt WITH &lcDVarFle..lnShpAmt + (TotQty*Price)
    ENDIF
    RETURN
  ELSE
    INSERT INTO (lcTmpFle) (cPack_id) VALUE (&lcPackHdr..Account+&lcPackHdr..Pack_id+&lcPackHdr..cpkColor+&lcPackHdr..cPckSize+&lcPackHdr..cPkVersion)
  ENDIF  
  IF &lcPackHdr..lpckprpiec OR &lcPackHdr..lRange OR &lcPackHdr..nPkSlPrice = 0
    REPLACE &lcDVarFle..lnShpAmt WITH &lcDVarFle..lnShpAmt + (TotQty*Price)
  ELSE
    REPLACE &lcDVarFle..lnShpAmt WITH &lcDVarFle..lnShpAmt + (&lcPackHdr..nPkSlPrice*&lcPackHdr..PackQty )
  ENDIF  
ELSE
  REPLACE &lcDVarFle..lnShpAmt WITH &lcDVarFle..lnShpAmt + (TotQty*Price)
ENDIF  
*C200410,1 [End]

*!**************************************************************************
*! Name      : lfSeek
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfSeek()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfSeek

=SEEK(&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id+&lcPackHdr..cPkColor+;
      &lcPackHdr..cPckSize+&lcPackHdr..cPkVersion)

*!**************************************************************************
*! Name      : lfShowGetD
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfShowGetD()
*!**************************************************************************
*! Due to C200518
*!**************************************************************************
FUNCTION lfShowGetD

lcPAlias = ALIAS()
=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LINVR','SH')
SELECT (lcPAlias)

llGetPack = SEEK('P'+laData[2]+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR') AND;
            SEEK('P'+laData[2]+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_LIN')
IF !llGetPack 
  llGetPack = SEEK('P*****'+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR') AND;
              SEEK('P*****'+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_LIN')
ENDIF  

llRange = m.lRange
llPrice = (m.lRange) OR (SPCK_LIN.nPck_price = 0) OR (m.nPkSlPrice = 0) OR;
          (m.lPckPrPiec)

IF !laScrMode[2]
  IF !EMPTY(m.Pack_id) 
    IF llPrice
      SHOW GET m.Gros_Price  ENABLE
    ELSE
      SHOW GET m.Gros_Price  DISABLE
    ENDIF
  
    SHOW GET ibStyle  DISABLE
    SHOW GET m.Style  DISABLE
    IF !(llRange) 
      SHOW GET m.Qty1   DISABLE
      SHOW GET m.Qty2   DISABLE
      SHOW GET m.Qty3   DISABLE    
      SHOW GET m.Qty4   DISABLE    
      SHOW GET m.Qty5   DISABLE
      SHOW GET m.Qty6   DISABLE    
      SHOW GET m.Qty7   DISABLE
      SHOW GET m.Qty8   DISABLE    
      SHOW GET m.TotQty DISABLE    
    ELSE
      SHOW GET m.Qty1   ENABLE    
      SHOW GET m.Qty2   ENABLE    
      SHOW GET m.Qty3   ENABLE    
      SHOW GET m.Qty4   ENABLE    
      SHOW GET m.Qty5   ENABLE    
      SHOW GET m.Qty6   ENABLE    
      SHOW GET m.Qty7   ENABLE    
      SHOW GET m.Qty8   ENABLE    
      SHOW GET m.TotQty ENABLE    
    ENDIF  
    SHOW GET m.Pack_Id DISABLE
  ELSE
    SHOW GET m.Qty1   ENABLE    
    SHOW GET m.Qty2   ENABLE    
    SHOW GET m.Qty3   ENABLE    
    SHOW GET m.Qty4   ENABLE    
    SHOW GET m.Qty5   ENABLE    
    SHOW GET m.Qty6   ENABLE    
    SHOW GET m.Qty7   ENABLE    
    SHOW GET m.Qty8   ENABLE    
    SHOW GET m.TotQty ENABLE    
    SHOW GET ibStyle  ENABLE    
    SHOW GET m.Style  ENABLE    
  ENDIF  
ENDIF  

SHOW GET m.Pack_Id DISABLE
SHOW GET ibPackId  DISABLE

*!**************************************************************************
*! Name      : lfShowGet
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfShowGet()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfShowGet
*C200433,1 HBG 17/11/2002 Enable price field in case of range pack & price per 
*C200433,1                piece and selling price = 0 [Begin]
lcPAlias = ALIAS()
=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LINVR','SH')
SELECT (lcPAlias)
*B126137,1 NNA 02/21/2005 (Begin) if we're in the Ardinv, Disable Qtys and Prices fileds
lcEnable = IIF(lcProgName = 'ARIINV','ENABLE','DISABLE')
*B126137,1 NNA (End)
llGetPack = SEEK('P'+laData[2]+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR') AND;
            SEEK('P'+laData[2]+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_LIN')
IF !llGetPack 
  llGetPack = SEEK('P*****'+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR') AND;
              SEEK('P*****'+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_LIN')
ENDIF  
*C200433,1 [End]
*C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec from INVLINE instead of SPCK_HDR [Begin]      
*IF ALIAS() = 'INVLINE'
*C200479,1 [End]
  llRange = .T.
  *C200479,1 HBG 30/12/2002 Comment these lines to use lRange & lPCkPrPiec from INVLINE instead of SPCK_HDR [Begin]      
  *C200433,1 HBG 17/11/2002 Move this block of code outside the condition [Begin]
  *llGetPack = SEEK('P'+laData[2]+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')  
  *IF !llGetPack 
  *  llGetPack = SEEK('P*****'+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')  
  *ENDIF 
  *C200433,1 [End]
  *C200479,1 [End]
  llPrice = .F.
  *C200479,1 HBG 30/12/2002 use lRange  from INVLINE instead of SPCK_HDR [Begin]      
  *IF llGetPack
    *llRange = SPCK_HDR.lRange
    llRange = m.lRange
  *C200479,1 [end]
    *C200433,1 HBG 17/11/2002 Enable price field in case of range pack & price per 
    *C200433,1                piece and selling price = 0 [Begin]
    *C200479,1 HBG 30/12/2002 use lRange  from INVLINE instead of SPCK_HDR [Begin]      
    *llPrice = (SPCK_HDR.lRange) OR (SPCK_LIN.nPck_price = 0) OR (SPCK_HDR.nPkSlPrice = 0) OR;
              (SPck_Hdr.lPckPrPiec)
    llPrice = (m.lRange) OR (SPCK_LIN.nPck_price = 0) OR (m.nPkSlPrice = 0) OR;
              (m.lPckPrPiec)
    *C200479,1 [End]                         
    *C200433,1 [End]
*C200479,1 HBG 30/12/2002 Comment these lines to use lRange & lPCkPrPiec from INVLINE instead of SPCK_HDR [Begin]      
*  ENDIF  
*ELSE
*  llRange = m.lRange 
  *C200433,1 HBG 17/11/2002 Enable price field in case of range pack & price per 
  *C200433,1                piece and selling price = 0 [Begin]
*  llPrice = .F.
*  IF llGetPack 
*    llPrice = (m.lRange) OR (SPCK_LIN.nPck_price = 0) OR (SPCK_HDR.nPkSlPrice = 0) OR;
*              (SPck_Hdr.lPckPrPiec)
*  ENDIF  
  *C200433,1 [End]
*ENDIF  
*C200479,1 [End]
IF !laScrMode[2]
  IF !EMPTY(m.Pack_id) 
    *C200433,1 HBG 17/11/2002 Enable price field in case of range pack & price per 
    *C200433,1                piece and selling price = 0 [Begin]
    IF llPrice
      *B126137,1 NNA 02/21/2005 (Begin) if we're in the Ardinv, Disable Qtys and Prices fileds
      *SHOW GET m.Gros_Price  ENABLE
      SHOW GET m.Gros_Price  &lcEnable
      *B126137,1 NNA (End)

    ELSE
      SHOW GET m.Gros_Price  DISABLE
    ENDIF
    *C200433,1 [End]
  
    SHOW GET ibStyle  DISABLE
    SHOW GET m.Style  DISABLE
    IF !(llRange) 
      SHOW GET m.Qty1   DISABLE
      SHOW GET m.Qty2   DISABLE
      SHOW GET m.Qty3   DISABLE    
      SHOW GET m.Qty4   DISABLE    
      SHOW GET m.Qty5   DISABLE
      SHOW GET m.Qty6   DISABLE    
      SHOW GET m.Qty7   DISABLE
      SHOW GET m.Qty8   DISABLE    
      SHOW GET m.TotQty DISABLE    
    ELSE
      *B126137,1 NNA 02/21/2005 (Begin) if we're in the Ardinv, Disable Qtys and Prices fileds
      *SHOW GET m.Qty1   ENABLE    
      *SHOW GET m.Qty2   ENABLE    
      *SHOW GET m.Qty3   ENABLE    
      *SHOW GET m.Qty4   ENABLE    
      *SHOW GET m.Qty5   ENABLE    
      *SHOW GET m.Qty6   ENABLE    
      *SHOW GET m.Qty7   ENABLE    
      *SHOW GET m.Qty8   ENABLE    
      *SHOW GET m.TotQty ENABLE    
      SHOW GET m.Qty1   &lcEnable
      SHOW GET m.Qty2   &lcEnable
      SHOW GET m.Qty3   &lcEnable
      SHOW GET m.Qty4   &lcEnable
      SHOW GET m.Qty5   &lcEnable
      SHOW GET m.Qty6   &lcEnable
      SHOW GET m.Qty7   &lcEnable
      SHOW GET m.Qty8   &lcEnable
      SHOW GET m.TotQty &lcEnable
      *B126137,1 NNA (End)
    ENDIF  
    SHOW GET m.Pack_Id DISABLE
  ELSE
    *B126137,1 NNA 02/21/2005 (Begin) if we're in the Ardinv, Disable Qtys and Prices fileds
    *SHOW GET m.Qty1   ENABLE    
    *SHOW GET m.Qty2   ENABLE    
    *SHOW GET m.Qty3   ENABLE    
    *SHOW GET m.Qty4   ENABLE    
    *SHOW GET m.Qty5   ENABLE    
    *SHOW GET m.Qty6   ENABLE    
    *SHOW GET m.Qty7   ENABLE    
    *SHOW GET m.Qty8   ENABLE    
    *SHOW GET m.TotQty ENABLE    
    *SHOW GET ibStyle  ENABLE    
    *SHOW GET m.Style  ENABLE    
    SHOW GET m.Qty1   &lcEnable
    SHOW GET m.Qty2   &lcEnable
    SHOW GET m.Qty3   &lcEnable
    SHOW GET m.Qty4   &lcEnable
    SHOW GET m.Qty5   &lcEnable
    SHOW GET m.Qty6   &lcEnable
    SHOW GET m.Qty7   &lcEnable
    SHOW GET m.Qty8   &lcEnable
    SHOW GET m.TotQty &lcEnable
    SHOW GET ibStyle  &lcEnable
    SHOW GET m.Style  &lcEnable
    *B126137,1 NNA (End)
  ENDIF  
ENDIF  

*!**************************************************************************
*! Name      : lfUpdtPrcD
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdtPrcD()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfUpdtPrcD

=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH') 
=lfUpdPric()
SET ORDER TO TAG InvLine IN (lcInvLine)
SELECT INVHDR

*!**************************************************************************
*! Name      : lfUpdtPrcS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdtPrcS()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUpdtPrcS

=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH') 
SELECT (lcInvHdr)
SCAN FOR Consol='N'
  =lfUpdPric()
ENDSCAN  
LOCATE
SET ORDER TO TAG (lcInvLine) IN (lcInvLine)

*!**************************************************************************
*! Name      : lfUpdtOrdD
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdtOrdD()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfUpdtOrdD

IF SEEK(laData[1],'INVLINE')
  SELECT ORDLINE
  SET ORDER TO ORDLINE
  SELECT INVLINE
  SET RELATION TO 'O'+InvLine.Order+STR(LineNo,6) INTO ORDLINE ADDI
  =lfUpdOrdl()
  SET RELATION TO
ENDIF
*C200352,1 HBG 04/15/2002 IF Trigger to GMA update 'SHDAEXPR' File of UPS system [Begin] 
=lfUPDUPSD()
*C200352,1 [End] 

*!**************************************************************************
*! Name      : lfUpdtOrdS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdtOrdS()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUpdtOrdS

lcInv = ""
FOR lnInv = 1 TO ALEN(laInv)
  lcInv = lcInv + laInv[lnInv]+'|'
ENDFOR
SELECT ORDLINE
SET ORDER TO ORDLINE
SELECT INVLINE
SET RELATION TO 'O'+InvLine.Order+STR(LineNo,6) INTO ORDLINE ADDI
SELECT INVHDR

*! B121820,1 MHM 02/25/2004 Enhance Speed of Sales Order Invoice when saving Invoice[Start]
*=SEEK(laInv[1])
*SCAN REST FOR Invoice $ lcInv 
*  laData[1] = INVHDR.Invoice
*  IF SEEK(laData[1],'INVLINE')
*    =lfUpdOrdl()
*  ENDIF  
*ENDSCAN  
FOR lnInv = 1 TO ALEN(laInv)
  IF SEEK(laInv[lnInv])
    SCAN REST WHILE Invoice =laInv[lnInv]
      laData[1] = INVHDR.Invoice
      IF SEEK(laData[1],'INVLINE')
        =lfUpdOrdl()
      ENDIF  
    ENDSCAN  
  ENDIF  
ENDFOR
*! B121820,1 MHM [End]

SELECT INVHDR
=SEEK(laInv[1])
SELECT INVLINE
SET RELATION TO

*C200352,1 HBG 04/15/2002 IF Trigger to GMA update 'SHDAEXPR' File of UPS system [Begin] 
=lfUPDUPSS()
*C200352,1 [End] 

*!**************************************************************************
*! Name      : lfAddIndx
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfAddIndx()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfAddIndx

lnIndex = ALEN(laIndex,1)
DIMENSION laIndex[lnIndex+1,2]
laIndex[lnIndex+1,1] = 'Invoice'
laIndex[lnIndex+1,2] = 'Invoice'

laIndex[1,1] = 'Account+Order+Store+PikTkt+CDIVISION+Dist_Ctr'
laIndex[1,2] = lcInvHdr
laIndex[2,1] = 'cSelect+Account+Order+Store+PikTkt+CDIVISION+Dist_Ctr'
laIndex[2,2] = 'Select'


*!**************************************************************************
*! Name      : lfAddIIndx
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfAddIIndx()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfAddIIndx

lnIndex = ALEN(laIndex,1)
DIMENSION laIndex[lnIndex+1,2]
laIndex[lnIndex+1,1] = 'Account+Order+Store+PikTkt+Pack_id+cPkColor+cPckSize+cPkVersion'
laIndex[lnIndex+1,2] = 'Packs'


*!**************************************************************************
*! Name      : lfAddOIndx
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfAddOIndx()
*!**************************************************************************
*! Due to C102570,1
*!**************************************************************************
FUNCTION lfAddOIndx

lnIndex = ALEN(laIndex,1)
*C200404,1 HBG 08/28/2002 Add new index to Allow Modify # of ordered pack [Begin]
*DIMENSION laIndex[lnIndex+1,2]
DIMENSION laIndex[lnIndex+2,2]
*C200404,1 [End]

laIndex[lnIndex+1,1] = 'cOrdType+Order+Store+PikTkt+Pack_id+cPkColor+cPckSize+cPkVersion'
laIndex[lnIndex+1,2] = 'Packs'
*C200404,1 HBG 08/28/2002 Add new index to Allow Modify # of ordered pack [Begin]
laIndex[lnIndex+2,1] = 'cPckAcc+Store+Pack_id+cPkColor+cPckSize+cPkVersion+Style'
laIndex[lnIndex+2,2] = 'AccPacks'
*C200404,1 [End]

*!**************************************************************************
*! Name      : lfOpenFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfOpenFile()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfOpenFile

=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LINVR','SH')

*!**************************************************************************
*! Name      : lfSetRel
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfSetRel()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfSetRel

SET RELATION OFF INTO SPck_Hdr
SET RELATION TO TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION INTO SPck_Hdr

*!**************************************************************************
*! Name      : lfDisblFld
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfDisblFld()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfDisblFld

SHOW GET m.Pack_Id DISABLE
SHOW GET ibPackId  DISABLE

*!**************************************************************************
*! Name      : lfGetPrc
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetPrc()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfGetPrc

*C200433,1 HBG 17/11/2002 Always get price from SPCK_LIN[Begin]
*m.Gros_Price = IIF(Spck_Lin.npck_price<> 0 AND !(&lcPackHdr..lpckprpiec),Spck_Lin.npck_price,;
                   lfGetprice(Spck_Lin.Style,lcPriceLvl,&lcPackHdr..PackQty*Spck_Lin.TotQty))
m.Gros_Price = Spck_Lin.npck_price
*C200433,1 [End]
                   

*!**************************************************************************
*! Name      : lfSetRelTm
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfSetRelTm()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfSetRelTm

SELECT (lcPackHdr)
SET RELATION OFF INTO Spck_lin
SET RELATION TO TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION INTO SPck_Lin ADDI

*!**************************************************************************
*! Name      : lfSetRelMs
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfSetRelMs()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfSetRelMS

SELECT SPck_Hdr
SET RELATION OFF INTO Spck_lin
SET RELATION TO TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION INTO SPck_Lin

*!**************************************************************************
*! Name      : lfUpdFileA
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdFileA()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfUpdFileA

lcAlias = ALIAS()

SUM REST WHILE Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion =;
    'P'+laData[2]+Spck_Hdr.Pack_Id+Spck_Hdr.cPkColor+Spck_Hdr.cPckSize+Spck_Hdr.cPkVersion;
     StyDye.TotStk,StyDye.TotOrd,StyDye.TotWip,Spck_Lin.TotQty ;
     TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty

lcPkSzDsc = "*****"
IF !EMPTY(Spck_Hdr.cPckSize)
  lcSiz = RIGHT(Spck_Hdr.cPckSize,1)
  IF SEEK('S'+LEFT(Spck_Hdr.cPckSize,1),'SCALE')
    lcPkSzDsc =  SCALE.Sz&lcSiz
  ENDIF
ENDIF  
SELECT (lcPackHdr)      
REPLACE &lcPackHdr..Type       WITH 'P',;
        &lcPackHdr..Account    WITH SPck_Hdr.Account,;
        &lcPackHdr..Pack_Id    WITH SPck_Hdr.Pack_Id,;
        &lcPackHdr..Desc       WITH SPck_Hdr.Desc,;
        &lcPackHdr..Season     WITH SPck_Hdr.Season,;
        &lcPackHdr..cDivision  WITH SPck_Hdr.cDivision,;
        &lcPackHdr..TotWip     WITH lnPackWip,;
        &lcPackHdr..TotStk     WITH lnPackStk,;
        &lcPackHdr..TotOrd     WITH lnPackOrd,;
        &lcPackHdr..TotQty     WITH lnPackQty,;
        &lcPackHdr..cPkColor   WITH Spck_Hdr.cPkColor,;
        &lcPackHdr..cPckSize   WITH Spck_Hdr.cPckSize,;
        &lcPackHdr..cPkVersion WITH Spck_Hdr.cPkVersion,;
        &lcPackHdr..nPkSlPrice WITH Spck_Hdr.nPkSlPrice,;
        &lcPackHdr..cpkSzDsc   WITH lcPkSzDsc,;
        &lcPackHdr..lRange     WITH Spck_Hdr.lRange,;
        &lcPackHdr..lpckprpiec WITH Spck_Hdr.lpckprpiec

SELECT (lcAlias)
           
*!**************************************************************************
*! Name      : lfUpdFileG
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdFileG()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfUpdFileG

lcAlias = ALIAS()
SUM REST WHILE Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion =;
       'P*****'+Spck_Hdr.Pack_Id+Spck_Hdr.cPkColor+Spck_Hdr.cPckSize+Spck_Hdr.cPkVersion;
       StyDye.TotStk,StyDye.TotOrd,StyDye.TotWip,Spck_Lin.TotQty ;
       TO lnPackStk, lnPackOrd, lnPackWip, lnPackQty

lcPkSzDsc = "*****"
IF !EMPTY(Spck_Hdr.cPckSize)
  lcSiz = RIGHT(Spck_Hdr.cPckSize,1)
  IF SEEK('S'+LEFT(Spck_Hdr.cPckSize,1),'SCALE')
    lcPkSzDsc =  SCALE.Sz&lcSiz
  ENDIF
ENDIF  
SELECT (lcPackHdr)
REPLACE &lcPackHdr..Type       WITH 'P',;
        &lcPackHdr..Account    WITH SPck_Hdr.Account,;
        &lcPackHdr..Pack_Id    WITH SPck_Hdr.Pack_Id,;
        &lcPackHdr..Desc       WITH SPck_Hdr.Desc,;
        &lcPackHdr..Season     WITH SPck_Hdr.Season;
        &lcPackHdr..cDivision  WITH SPck_Hdr.cDivision,;
        &lcPackHdr..TotWip     WITH lnPackWip,;
        &lcPackHdr..TotStk     WITH lnPackStk,;
        &lcPackHdr..TotOrd     WITH lnPackOrd,;
        &lcPackHdr..TotQty     WITH lnPackQty,;
        &lcPackHdr..cPkColor   WITH Spck_Hdr.cPkColor,;
        &lcPackHdr..cPckSize   WITH Spck_Hdr.cPckSize,;
        &lcPackHdr..cPkVersion WITH Spck_Hdr.cPkVersion,;
        &lcPackHdr..nPkSlPrice WITH Spck_Hdr.nPkSlPrice,;
        &lcPackHdr..cpkSzDsc   WITH lcPkSzDsc,;
        &lcPackHdr..lRange     WITH Spck_Hdr.lRange,;
        &lcPackHdr..lpckprpiec WITH Spck_Hdr.lpckprpiec

SELECT (lcAlias)
*!**************************************************************************
*! Name      : lfCrtFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfCrtFile()
*!*************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfCrtFile

CREATE DBF (gcWorkDir+lcPackHdr) ;
        (Type C(1),Account C(5),Pack_Id C(16),Desc C(20),Season C(6),;
        cDivision C(6),TotWip N(6),TotStk N(6),TotOrd N(6),TotQty N(6),PackQty N(6),;
        CPKCOLOR C(6),CPCKSIZE C(3),CPKVERSION C(4),nPkSlPrice N(8,2),cPkSzDsc C(5),lRange L(1),lpckprpiec L(1))
INDEX ON ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION TAG (lcPackHdr)         

*!**************************************************************************
*! Name      : lfDifBarD
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfDifBarD()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
*! B037436,1 MHM 01/20/2004 Fix bug of goto infinit loop if you open Direct Invoice[Start] 
*FUNCTION lfDifBarD
FUNCTION lfVEWBarD
*!B037436,1 MHM [END] 

lcDVarFle = lcInvHdr+'A'
GO TOP IN (lcDVarFle)
REPLACE &lcDVarFle..llByStyle WITH .T.,;
        &lcDVarFle..llSumInv  WITH .T.
 
DEFINE BAR 13 OF _INQURYPOP PROMPT '\<Show Invoice Lines by Pack' SKIP FOR (lnactfolder<>2)  OR (laScrMode[4] AND EOF(lcInvline))
DEFINE BAR 14 OF _INQURYPOP PROMPT '\<Summarize By Pack' SKIP FOR (lnactfolder<>2) OR !(laScrMode[2])

=lfBrowDet() 
*!**************************************************************************
*! Name      : lfDifBarS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfDifBarS()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfDifBarS

DEFINE BAR 15 OF _INQURYPOP PROMPT '\<Show Invoice Lines by Pack' SKIP FOR (lnactfolder<>3) OR (laScrMode[4] AND EOF(lcInvline))

=lfBrowDet()


*!**************************************************************************
*! Name      : lfBrowFld
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfBrowFld()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfBrowFld

SELECT IIF(laScrMode[2] OR laScrMode[3],'INVLINE',lcInvLine)
=SEEK(IIF(laScrMode[2] OR laScrMode[3] ,laData[1],laData[2]+laData[3]+laData[5]+laData[30]))
lnMarker = RECNO()

lcDVarFle = lcInvHdr+'A'
IF !(&lcDVarFle..llByStyle)
  IF !(&lcDVarFle..llSumInv)
    lcFields = "cMarker=IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,lcPack=ALLTRIM(Pack_ID)+;
                        '-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('ARDINV','GETGMSZ')+'-'+;
                         ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R,STORE :R,"+;
                         "nPkSlPrice :H='Pack Price' :R,PackQty :H='Pack Qty':P='999999':R"
  ELSE
    lcFields = "cMarker=IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,lcPack=ALLTRIM(Pack_ID)+;
                        '-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('ARDINV','GETGMSZ')+'-'+;
                         ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R,"                         
   
    lcFields = lcFields + "Style :H=lcStyHdr :R,"+IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
    lcFields = lcFields +;
               "Group :H='Group' :R,PrePak :H='Prepack' :R,Gros_Price :H='Gross Price' :R,"+;
               "Disc_Pcnt :H='Disc. Pcnt.' :R,Price :H='Net Price' :R,TotQty :H='Quantity' :R,"+;
               "lnAmount=PRICE*TOTQTY :12:H='Ship Amount':R"
  ENDIF                         
ELSE
  IF !(&lcDVarFle..llSumInv)
    lcFields = "cMarker=IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,lcPack=ALLTRIM(Pack_ID)+;
                        '-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('ARDINV','GETGMSZ')+'-'+;
                         ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R,STORE :R,"+;
                         "nPkSlPrice :H='Pack Price' :R,PackQty :H='Pack Qty':P='999999':R"
  ELSE
    lcFields = "cMarker=IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,Style :H=lcStyHdr :R,"
    lcFields = lcFields + IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
    lcFields = lcFields +;
               "Group :H='Group' :R,PrePak :H='Prepack' :R,Gros_Price :H='Gross Price' :R,"+;
               "Disc_Pcnt :H='Disc. Pcnt.' :R,Price :H='Net Price' :R,TotQty :H='Quantity' :R,"+;
               "lnAmount=PRICE*TOTQTY :12:H='Ship Amount':R"
  ENDIF  
ENDIF

lcDVarFle = lcInvHdr+'A'
IF !(&lcDVarFle..llSumInv)
  lcSumPck = &lcDVarFle..lcSumPck
  SELECT (lcSumPck)                         
ELSE
  SELECT IIF(laScrMode[2] OR laScrMode[3],'INVLINE',lcInvLine)
ENDIF

IF WEXIST(lcBrTtlZ)
  HIDE WINDOW (lcBrTtlZ)
  RELEASE WINDOW (lcBrTtlZ)
ENDIF
IF WEXIST(lcInvBrow)
  HIDE WINDOW (lcInvBrow)
  RELEASE WINDOW (lcInvBrow)
ENDIF
IF llZoom
  HIDE WINDOW (lcWinCh32),(lcWinCh34),(lcWinCh35) SAME
  SHOW GETS WINDOW (lcWinCh32) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh34) DISABLE ONLY
  SHOW GETS WINDOW (lcWinCh35) DISABLE ONLY
  BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh33)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],'')  ;
	     WHEN lfwBrow()    ;
         TITLE lcBrTtlZ 
ELSE
  SHOW WINDOW (lcWinCh32),(lcWinCh34),(lcWinCh35) TOP
  BROWSE FIELDS &lcFields ;
         WINDOW (lcWinCh31)  ;
         IN WINDOW (lcWinCh3);
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
	     NOCLEAR           ;
  	     KEY IIF(laScrMode[2] OR laScrMode[3],laData[1],'')  ;
  	     WHEN lfwBrow()    ;
         TITLE lcInvBrow   
ENDIF

*!**************************************************************************
*! Name      : lfBrwFSInv
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfBrwFSInv()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfBrwFSInv

lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
IF !(&lcSVarFle..llByStyle) 
  IF !(&lcSVarFle..llSumInv)
    lcFields = "cMarker=IIF(RECNO()=lnInvMark ,'>',' '):H=' ':R:1:W=.F.,lcPack=ALLTRIM(Pack_ID)+;
                        '-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('ARIINV','GETGMSZ')+'-'+;
                         ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R,STORE :R,"+;
                         "nPkSlPrice :H='Pack Price' :R,PackQty :H='Pack Qty':P='999999':R"
  ELSE
    lcFields = "cMarker=IIF(RECNO()=lnInvMark ,'>',' '):H=' ':R:1:W=.F.,lcPack=ALLTRIM(Pack_ID)+'-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('ARIINV','GETGMSZ')+'-'+;
                                   ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R,Style :H=lcStyHdr :R,"
    lcFields = lcFields + IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
    lcFields = lcFields +IIF('AL' $ gcCmpModules,"","Group :H='Group' :R,")+;
               "TotBook :H='Ordered' :R,"+IIF('AL' $ gcCmpModules,"TotPik :H='Picked' :R,",'')+;
               "TotQty :H='Ship' :R,"+;
               "Gros_Price :H='Gross Price' :P='999999999.99' :R,"+;
               "Disc_Pcnt :H='Disc. Pcnt.' :P='999.99' :R,Price :H='Net Price' :P='999999999.99' :R,"+;
               "lnAmount=PRICE*TotQty :H='Ship Amount':R:P='99999999999.99'"
    lcFields = lcFields + IIF('AL' $ gcCmpModules,",Packed=IIF(lPacked,'Y','N') :H='PS':R",'')
    SET ORDER TO PACKS IN (lcInvLine)
  ENDIF
ELSE
 IF !(&lcSVarFle..llSumInv)
    lcFields = "cMarker=IIF(RECNO()=lnInvMark ,'>',' '):H=' ':R:1:W=.F.,lcPack=ALLTRIM(Pack_ID)+;
                        '-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('ARIINV','GETGMSZ')+'-'+;
                         ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R,STORE :R,"+;
                         "nPkSlPrice :H='Pack Price' :R,PackQty :H='Pack Qty':P='999999':R"
  ELSE
    lcFields = "cMarker=IIF(RECNO()=lnInvMark ,'>',' '):H=' ':R:1:W=.F.,Style :H=lcStyHdr :R,"
    lcFields = lcFields + IIF(laSetups[8,2]='Y',"Dyelot :R,",'')
    lcFields = lcFields +IIF('AL' $ gcCmpModules,"","Group :H='Group' :R,")+;
               "TotBook :H='Ordered' :R,"+IIF('AL' $ gcCmpModules,"TotPik :H='Picked' :R,",'')+;
               "TotQty :H='Ship' :R,"+;
               "Gros_Price :H='Gross Price' :P='999999999.99' :R,"+;
               "Disc_Pcnt :H='Disc. Pcnt.' :P='999.99' :R,Price :H='Net Price' :P='999999999.99' :R,"+;
               "lnAmount=PRICE*TotQty :H='Ship Amount':R:P='99999999999.99'"
    lcFields = lcFields + IIF('AL' $ gcCmpModules,",Packed=IIF(lPacked,'Y','N') :H='PS':R",'')
    SET ORDER TO (lcInvLine) IN (lcInvLine) 
  ENDIF
ENDIF


*!**************************************************************************
*! Name      : lfBrowPkFl
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfBrowPkFl()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfBrowPkFl

lcFields = [cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,]+;
           [Account   :H='Account' :R,]+;
           [Pack_Id   :H='Pack Id' :R,]+;
           [cPkColor  :H='Color':R,]+;
           [cpkSzDsc  :H='Size':R,]+;
           [cPkVersion:H='Version':R,]+;
           [Desc      :H='Description':R,]+;
           [PackQty   :H='Pack',]+;
           [TotWip    :H='Wip':R,]+;
           [TotStk    :H='Stock':R,]+;
           [TotOrd    :H='Ordered' :R,]+;
           [TotQty    :H='Qty.' :R,]+;
           [lcSesDesc=gfCodDes(Season,'SEASON') :H="Season" :R :20,]+;
           [lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H="Division" :20 :R]

*!**************************************************************************
*! Name      : lfScanPckA
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfScanPckA()
*!**************************************************************************
*! Due to C102574,1 
*!**************************************************************************
FUNCTION lfScanPckA

lcScnExpr = "Type+Account+Pack_id+cPkColor+cPckSize+cPkVersion ='P'+laData[2]"

*!**************************************************************************
*! Name      : lfScanPckA
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfScanPckA()
*!**************************************************************************
*! Due to C102575,1 
*!**************************************************************************
FUNCTION lfScanPkAS

lcScnExpr = "Type+Account+Style+cPkcolor+cPcksize+Pack_id+cPkversion ='P'+laData[4]"

*!**************************************************************************
*! Name      : lfScanPckG
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfScanPckG()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfScanPckG

lcScnExpr = "Type+Account+Style+cPkcolor+cPcksize+Pack_id+cPkversion ='P*****'"

*!**************************************************************************
*! Name      : lfScanExp
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfScanExp()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfScanExp

lcScnExpr = 'Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion =;
            &lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id+&lcPackHdr..cPkColor+;
            &lcPackHdr..cPckSize+&lcPackHdr..cPkVersion'

*!**************************************************************************
*! Name      : lfAddFld
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfAddFld()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfAddFld

*C200479,1 HBG 30/12/2002 Comment these lines to use lRange & lPCkPrPiec from ORLINE instead of SPCK_HDR [Begin]      
*lnFileStru = ALEN(laFileStru,1)
*DIMENSION laFileStru[lnFileStru+1,4]
*laFileStru[lnFileStru+1,1] = 'lRange'
*laFileStru[lnFileStru+1,2] = 'L'
*laFileStru[lnFileStru+1,3] = 0
*laFileStru[lnFileStru+1,4] = 0
*C200479,1 [End]

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'lFrmpack'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 0
laFileStru[lnFileStru+1,4] = 0

*C200499,1 HBG 01/27/2003 Add flag to UnShip Styles and packs in Invoice SO 
*C200499,1                Screen For all stores [Begin]
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'lShip'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 0
laFileStru[lnFileStru+1,4] = 0
*C200499,1 [End]


*!**************************************************************************
*! Name      : lfUpdSInv
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfUpdSInv()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfUpdSInv

*C200410,1 HBG 23/09/2002 Update the new field npackNo [Begin]
*REPLACE Pack_id    WITH m.Pack_id,;
        cPkColor   WITH m.cPkColor,;
        cPckSize   WITH m.cPckSize,;
        nPkSlPrice WITH m.nPkSlPrice,;
        cPkVersion WITH m.cPkVersion
*C200479,1 HBG 30/12/2002 Update lRange & lPCkPrPiec in INVLINE[Begin]              
*REPLACE Pack_id    WITH m.Pack_id,;
        cPkColor   WITH m.cPkColor,;
        cPckSize   WITH m.cPckSize,;
        nPkSlPrice WITH m.nPkSlPrice,;
        cPkVersion WITH m.cPkVersion,;        
        nPackNo    WITH m.nPackNo    
REPLACE Pack_id    WITH m.Pack_id,;
        cPkColor   WITH m.cPkColor,;
        cPckSize   WITH m.cPckSize,;
        nPkSlPrice WITH m.nPkSlPrice,;
        cPkVersion WITH m.cPkVersion,;        
        nPackNo    WITH m.nPackNo,;
        lRange     WITH m.lRange,;
        lPckPrPiec WITH m.lPckPrPiec,;
        lShip      WITH .T.        
*C200479,1 [ENd]
*C200410,1 [End]

*!**************************************************************************
*! Name      : lfUpdInv
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfUpdInv()
*!**************************************************************************
*! Due to C102574,1 , C102575,1 
*!**************************************************************************
FUNCTION lfUpdInv

*C200410,1 HBG 23/09/2002 Update the new field npackNo [Begin]
*REPLACE cPkColor   WITH &lcPackHdr..cPkColor,;
        cPckSize   WITH &lcPackHdr..cPckSize,;
        cPkVersion WITH &lcPackHdr..cPkVersion,;
        lRange     WITH &lcPackHdr..lRange,;
        lFrmpack   WITH .T.
*C200479,1 HBG 30/12/2002 Update lRange & lPCkPrPiec in INVLINE[Begin]                      
*REPLACE cPkColor   WITH &lcPackHdr..cPkColor,;
        cPckSize   WITH &lcPackHdr..cPckSize,;
        cPkVersion WITH &lcPackHdr..cPkVersion,;
        lRange     WITH &lcPackHdr..lRange,;
        lFrmpack   WITH .T.,;
        nPackNo    WITH &lcPackHdr..PackQty 
REPLACE cPkColor   WITH &lcPackHdr..cPkColor,;
        cPckSize   WITH &lcPackHdr..cPckSize,;
        cPkVersion WITH &lcPackHdr..cPkVersion,;
        lRange     WITH &lcPackHdr..lRange,;
        lPckPrPiec WITH &lcPackHdr..lPckPrPiec ,;
        nPkSlPrice WITH &lcPackHdr..nPkSlPrice,;
        lFrmpack   WITH .T.,;
        nPackNo    WITH &lcPackHdr..PackQty 
*C200479,1 [End]
*C200410,1 [End]

*!**************************************************************************
*! Name      : lfGetRang
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : =lfGetRang()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfGetRang

*C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in ORDLINE instead of SPCK_HDR [Begin]              
*llGetPack  = SEEK('P'+m.Account+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')  
*IF !llGetPack 
*  llGetPack = SEEK('P*****'+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')  
*ENDIF 
*IF llGetPack 
*  REPLACE lRange WITH SPCK_HDR.lRange
*ENDIF  
REPLACE lRange WITH m.lRange
*C200479,1 [End]

*!**************************************************************************
*! Name      : lfVldBarD
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfVldBarD()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfVldBarD

lcDVarFle = lcInvHdr+'A'
GO TOP IN (lcDVarFle)
lcPckAcc = laData[2]
IF &lcDVarFle..llByStyle
  DEFINE BAR 13 OF _INQURYPOP PROMPT '\<Show Invoice Lines by Style' SKIP FOR (lnactfolder<>2)  OR (laScrMode[4] AND EOF(lcInvline))
  REPLACE &lcDVarFle..llByStyle WITH .F.
ELSE
  DEFINE BAR 13 OF _INQURYPOP PROMPT '\<Show Invoice Lines by Pack' SKIP FOR (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcInvline))
  REPLACE &lcDVarFle..llByStyle WITH .T.
ENDIF

=lfBrowDet()

*!**************************************************************************
*! Name      : lfVldBarS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfVldBarS()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfVldBarS

lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
lcPckAcc = laData[4]
IF &lcSVarFle..llByStyle
  DEFINE BAR 15 OF _INQURYPOP PROMPT '\<Show Invoice Lines by Style' SKIP FOR (lnactfolder<>3)  OR (laScrMode[4] AND EOF(lcInvline))
  REPLACE &lcSVarFle..llByStyle WITH .F.
ELSE
  DEFINE BAR 15 OF _INQURYPOP PROMPT '\<Show Invoice Lines by Pack' SKIP FOR (lnactfolder<>3)  OR (laScrMode[4] AND EOF(lcInvline))
  REPLACE &lcSVarFle..llByStyle WITH .T.
ENDIF

=lfBrowDet()

*!**************************************************************************
*! Name      : lfVldSumD
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfVldSumD()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfVldSumD

lcDVarFle = lcInvHdr+'A'               
lcPckAcc = laData[2]
IF &lcDVarFle..llSumInv  
  DEFINE BAR 14 OF _INQURYPOP PROMPT '\<Show pack details' SKIP FOR (lnactfolder<>2) OR !(laScrMode[2])
  REPLACE &lcDVarFle..llSumInv WITH .F.
  lcSumPck  = &lcDVarFle..lcSumPck
  DO CASE
    CASE laScrMode[2] OR laScrMode[3]      
      =lfInvColSm()
    CASE laScrMode[4] 
      =lfInvClSmS()
  ENDCASE
ELSE
  DEFINE BAR 14 OF _INQURYPOP PROMPT '\<Summarize By Pack' SKIP FOR (lnactfolder<>2) OR !(laScrMode[2])
  REPLACE &lcDVarFle..llSumInv WITH .T.
ENDIF

=lfBrowDet()
*!**************************************************************************
*! Name      : lfUpdPric
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Update Pack price in INVLINE file
*!**************************************************************************
*! Example   : = lfUpdPric()
*!**************************************************************************
*! Due to 
*!**************************************************************************
FUNCTION lfUpdPric
                            
SELECT (lcInvLine)
SCAN FOR Account+Order+Store+PikTkt+STR(LineNo,6) = ;
         &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..Piktkt
  llGetPack = SEEK('P'+Account+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')  
  IF !llGetPack 
    llGetPack = SEEK('P*****'+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')  
  ENDIF 
  IF llGetPack 
    REPLACE &lcInvLine..nPkSlPrice WITH SPCK_HDR.nPkSlPrice
  ENDIF  
ENDSCAN
               
*!**************************************************************************
*! Name      : lfUpdOrdL
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Updatefields of Pack in ORDLINE file
*!**************************************************************************
*! Example   : = lfUpdOrdL()
*!**************************************************************************
*! Due to 
*!**************************************************************************
FUNCTION lfUpdOrdL
                            
SELECT INVLINE
SCAN REST WHILE invoice+STR(lineno,6) = laData[1]
  *C200410,1 HBG 23/09/2002 Update the new field npackNo [Begin]
  *REPLACE ORDLINE.cPkColor   WITH INVLINE.cPkColor  ,;
          ORDLINE.cPckSize   WITH INVLINE.cPckSize  ,;
          ORDLINE.cPkVersion WITH INVLINE.cPkVersion,;
          ORDLINE.nPkSlPrice WITH INVLINE.nPkSlPrice 
  *C200479,1 HBG 30/12/2002 Update lRange & lPCkPrPiec in ORDLINE [Begin]                      
  *REPLACE ORDLINE.cPkColor   WITH INVLINE.cPkColor  ,;
          ORDLINE.cPckSize   WITH INVLINE.cPckSize  ,;
          ORDLINE.cPkVersion WITH INVLINE.cPkVersion,;
          ORDLINE.nPkSlPrice WITH INVLINE.nPkSlPrice,;
          ORDLINE.nPackNo    WITH INVLINE.nPackNo    
  REPLACE ORDLINE.cPkColor   WITH INVLINE.cPkColor  ,;
          ORDLINE.cPckSize   WITH INVLINE.cPckSize  ,;
          ORDLINE.cPkVersion WITH INVLINE.cPkVersion,;
          ORDLINE.nPkSlPrice WITH INVLINE.nPkSlPrice,;
          ORDLINE.nPackNo    WITH INVLINE.nPackNo,;
          ORDLINE.lRange     WITH INVLINE.lRange,;              
          ORDLINE.lPckPrPiec WITH INVLINE.lPckPrPiec 
  *C200479,1 [End]
  *C200410,1 [End]                 
ENDSCAN

*!**************************************************************************
*! Name      : lfRmvPack
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Remove the pack 
*!**************************************************************************
*! Example   : = lfRmvPack()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfRmvPack


IF !EMPTY(m.Pack_ID) AND !EMPTY(m.cPkColor) AND !EMPTY(m.cPckSize) AMD !EMPTY(m.cPkVersion)
  IF !EMPTY(m.cPckSize) AND SEEK('S'+LEFT(m.cPckSize,1),'SCALE')
    lcSiz = RIGHT(m.cPckSize,1)
    lcSize = SCALE.Sz&lcSiz
  ELSE
    lcSize = '*****'    
  ENDIF  
  IF gfModalGen('QRM00000B40000','ALERT',.F.,.F.,;
                'This Style in belong to a pack: '+ALLTRIM(m.Pack_ID)+'-'+ALLTRIM(m.cPkColor)+;
                '-'+ALLTRIM(lcSize)+'-'+ALLTRIM(m.cPkVersion)+;
                '. Do you want to remove the all pack') = 1
    lcPack_ID   = m.Pack_ID
    lcPkColor   = m.cPkColor
    lcPckSize   = m.cPckSize
    lcPkVersion = m.cPkVersion
    llFromPack = .T.
    IF SEEK('P'+laData[4]+m.Pack_ID+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR') OR;
       SEEK('P*****'+m.Pack_ID+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')
     *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in INVLINE instead of SPCK_HDR [Begin]                
     *IF SPCK_HDR.lpckprpiec OR SPCK_HDR.lRange OR SPCK_HDR.nPkSlPrice = 0  
     IF m.lpckprpiec OR m.lRange OR m.nPkSlPrice = 0  
     *C200479,1 [End]
       llFromPack = .F.
     ENDIF
    ENDIF   
    

    SELECT (lcInvLine)
    SCAN FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion ;
         AND lFrmPack
      SCATTER MEMVAR MEMO
      lnCartons  = &lcInvHdr..nCartons - IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
      m.Cartons  = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
      m.Weight   = m.Weight  - m.TotQty*Style.nStyWeight
      
      IF llFromPack 
        m.ShipAmt  = m.ShipAmt - (m.nPackNo*SPCK_HDR.nPkSlPrice)
      ELSE
        m.ShipAmt  = m.ShipAmt - m.TotQty*m.Price
      ENDIF
      
      m.Ship     = m.Ship    - m.TotQty
      m.Discount = -1*m.DiscPcnt*m.ShipAmt/100
      lnQuantity = 0
      IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
        FOR lnCount = Style.nTaxBreak TO 8
          lnQuantity = lnQuantity - EVAL('m.Qty'+STR(lnCount,1))
        ENDFOR
      ENDIF
      m.nTaxDue = &lcInvHdr..nTaxDue - IIF(lTaxable,m.TotQty*m.Price,0)
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
      =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
      SELECT (lcInvHdr)  
      =RLOCK()  
      REPLACE CARTONS   WITH m.Cartons ,;
              nCartons  WITH lnCartons ,;
              WEIGHT    WITH m.Weight  ,;
              SHIPAMT   WITH m.ShipAmt ,;
              SHIP      WITH m.Ship    ,;
              Discount  WITH m.Discount,;
              lCompUps  WITH !llIsEngland ,;
              nMerchTax WITH nMerchTax +  lnQuantity * Price * nTaxRate/100 ,;
              Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
              Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                               Tax_Amt+Discount,Cod_Amt),0) ,; 
              TotalChg  WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
              nTaxDue   WITH m.nTaxDue
      UNLOCK
    ENDSCAN
    SELECT (lcInvLine)
    DELETE FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion ;
           AND lFrmPack         
    GO TOP
    *-- change  the status  for the screen fields
    SCATTER MEMVAR
    SHOW WINDOW (lcInvBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh42) ONLY
    SHOW GETS WINDOW (lcWinCh44) ONLY
    SHOW GETS WINDOW (lcWinCh45) ONLY
    STORE .T. TO llCUpdate
  ENDIF
ELSE
  IF gfModalGen('QRM40011B40000','ALERT','invoice line')= 1

    *B602905,1 Default No. of cartons must be 1 at least [Begin]
    lnCartons  = &lcInvHdr..nCartons - IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    m.Cartons  = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
    *B602905,1 Default No. of cartons must be 1 at least [End  ]
    m.Weight   = m.Weight - m.TotQty*Style.nStyWeight
    m.ShipAmt  = m.ShipAmt - m.TotQty*m.Price
    m.Ship     = m.Ship - m.TotQty
    m.Discount = -1*m.DiscPcnt*m.ShipAmt/100
    lnQuantity = 0
    IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
      FOR lnCount = Style.nTaxBreak TO 8
        lnQuantity = lnQuantity - EVAL('m.Qty'+STR(lnCount,1))
      ENDFOR
    ENDIF
    m.nTaxDue = &lcInvHdr..nTaxDue - IIF(lTaxable,m.TotQty*m.Price,0)
    STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
    =IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)
    SELECT (lcInvHdr)  
    =RLOCK()
    REPLACE CARTONS   WITH m.Cartons ,;
            nCartons  WITH lnCartons ,;
            WEIGHT    WITH m.Weight  ,;
            SHIPAMT   WITH m.ShipAmt ,;
            SHIP      WITH m.Ship    ,;
            Discount  WITH m.Discount,;
            lCompUps  WITH !llIsEngland ,;
            nMerchTax WITH nMerchTax +  lnQuantity * Price * nTaxRate/100 ,;
            Tax_Amt   WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                               Tax_Amt+Discount,Cod_Amt),0) ,; 
            TotalChg  WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
            nTaxDue   WITH m.nTaxDue
    UNLOCK

    m.Tax_Amt = Tax_Amt
    m.TotalChg = TotalChg

    SELECT (lcInvLine)
    DELETE
    GO TOP
    SCATTER MEMVAR
    SHOW WINDOW (lcInvBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh42) ONLY
    SHOW GETS WINDOW (lcWinCh44) ONLY
    SHOW GETS WINDOW (lcWinCh45) ONLY
    STORE .T. TO llCUpdate

  ENDIF

ENDIF



*!**************************************************************************
*! Name      : lfRemPack
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Remove the pack 
*!**************************************************************************
*! Example   : = lfRemPack()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfRemPack

IF !EMPTY(m.Pack_ID) AND !EMPTY(m.cPkColor) AMD !EMPTY(m.cPkVersion)
  IF !EMPTY(m.cPckSize) AND SEEK('S'+LEFT(m.cPckSize,1),'SCALE')
    lcSiz = RIGHT(m.cPckSize,1)
    lcSize = SCALE.Sz&lcSiz
  ELSE
    lcSize = '*****'    
  ENDIF  
  IF gfModalGen('QRM00000B40000','ALERT',.F.,.F.,;
                'This Style in belong to a pack: '+ALLTRIM(m.Pack_ID)+'-'+ALLTRIM(m.cPkColor)+;
                '-'+ALLTRIM(lcSize)+'-'+ALLTRIM(m.cPkVersion)+;
                '. Do you want to remove the all pack') = 1
    lcPack_ID   = m.Pack_ID
    lcPkColor   = m.cPkColor
    lcPckSize   = m.cPckSize
    lcPkVersion = m.cPkVersion
    llFirst    = .T.    
    llFromPack = .T.
    IF SEEK('P'+laData[2]+m.Pack_ID+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR') OR;
       SEEK('P*****'+m.Pack_ID+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')
     *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in ORDLINE instead of SPCK_HDR [Begin]              
     *IF SPCK_HDR.lpckprpiec OR SPCK_HDR.lRange OR SPCK_HDR.nPkSlPrice = 0  
     IF m.lpckprpiec OR m.lRange OR m.nPkSlPrice = 0  
     *C200479,1 [End]
       llFromPack = .F.
     ENDIF
    ENDIF   
    
    SELECT (lcInvLine)
    SCAN FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion 

      *--decrese the quantity per carton with the style quantity
      SCATTER MEMVAR MEMO
      lnCartons  = lnCartons  - IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
      laData[25] = CEILING(lnCartons)
      laData[25] = IIF(laData[25]=0,1,laData[25])
      *-- decrease the total wieght with style wieght
      laData[26] = laData[26] - m.TotQty*Style.nStyWeight
      *-- decrease the shipped amount 
      
      IF llFromPack 
        IF llFirst    
          *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in INVLINE instead of SPCK_HDR [Begin]              
          *laData[31] = laData[31] - (m.nPackNo*SPCK_HDR.nPkSlPrice)
          laData[31] = laData[31] - (m.nPackNo*m.nPkSlPrice)
          *C200479,1 [End]
          llFirst    = .F.
        ENDIF  
      ELSE
        laData[31] = laData[31] - (m.TotQty*m.Price)
      ENDIF  
      
      *-- merchandise discount
      laData[33] = -ROUND(laData[31]*laData[32]/100,2)
      *-- approval amount =Shipped amount
      laData[48] = laData[31]
      *-- decrease shipped quantyties
      laData[42] = laData[42] - m.TotQty
      *-- if this style was taxable then decrease the tax amount
      lnTaxDue   = IIF(lTaxable,lnTaxDue-m.TotQty*m.Price,lnTaxDue)
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE CARTONS  WITH laData[25] ,;
              WEIGHT   WITH laData[26] ,;
              SHIPAMT  WITH laData[31] ,;
              Discount WITH laData[33] ,;
              SHIP     WITH laData[42] ,;
              APPRAMT  WITH laData[48] ,;
              nTaxDue  WITH lnTaxDue   ,;
              lCompUps WITH .T.    
      UNLOCK
      SELECT (lcInvLine)
    
      IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
        lnQuantity = 0
        FOR lnCount = Style.nTaxBreak TO 8
        *-- Sum the quantity for only the sizes that is Greater than the tax break weight
          lnQuantity = lnQuantity+EVAL('m.Qty'+STR(lnCount,1))
        ENDFOR
        *-- Decrease Merchandise tax
        lnMerchTax = lnMerchTax - lnQuantity * m.Price * nTaxRate/100
      ENDIF
    ENDSCAN
    SELECT (lcInvLine)
    DELETE FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion 
    GO TOP
    *-- change  the status  for the screen fields
    lcDispDet = IIF(EOF(),'DISABLE','')
    SCATTER MEMVAR
    SHOW WINDOW (lcInvBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh34) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh35) &lcDispDet ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh34)
    IF EOF()
      SHOW GET pbNew ENABLE 
      SHOW GET pbPacks ENABLE
    ENDIF
    STORE .T. TO llReMerTax,llCUpdate
  ENDIF
ELSE
  IF gfModalGen('QRM40011B40000','ALERT','invoice line')= 1
    *--decrese the quantity per carton with the style quantity
    lnCartons  = lnCartons  - IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    laData[25] = CEILING(lnCartons)

    laData[25] = IIF(laData[25]=0,1,laData[25])
    *-- decrease the total wieght with style wieght
    laData[26] = laData[26] - m.TotQty*Style.nStyWeight
    *-- decrease the shipped amount 
    laData[31] = laData[31] - m.TotQty*m.Price
  
    *-- merchandise discount
    laData[33] = -ROUND(laData[31]*laData[32]/100,2)
    *-- approval amount =Shipped amount
    laData[48] = laData[31]
    *-- decrease shipped quantyties
    laData[42] = laData[42] - m.TotQty
    *-- if this style was taxable then decrease the tax amount
    lnTaxDue   = IIF(lTaxable,lnTaxDue-m.TotQty*m.Price,lnTaxDue)
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE CARTONS  WITH laData[25] ,;
            WEIGHT   WITH laData[26] ,;
            SHIPAMT  WITH laData[31] ,;
            Discount WITH laData[33] ,;
            SHIP     WITH laData[42] ,;
            APPRAMT  WITH laData[48] ,;
            nTaxDue  WITH lnTaxDue   ,;
            lCompUps WITH .T.    
    UNLOCK
    SELECT (lcInvLine)
    
    IF laSetups[9,2]='Y' .AND. llIsEngland .AND. Style.nTaxBreak <> 0
      lnQuantity = 0
      FOR lnCount = Style.nTaxBreak TO 8
      *-- Sum the quantity for only the sizes that is Greater than the tax break weight
        lnQuantity = lnQuantity+EVAL('m.Qty'+STR(lnCount,1))
      ENDFOR
      *-- Decrease Merchandise tax
      lnMerchTax = lnMerchTax - lnQuantity * m.Price * nTaxRate/100
    ENDIF
    SELECT (lcInvLine)
    DELETE
    GO TOP
    *-- change  the status  for the screen fields
    lcDispDet = IIF(EOF(),'DISABLE','')
    SCATTER MEMVAR
    SHOW WINDOW (lcInvBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh34) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh35) &lcDispDet ONLY
    =lfRefresh(lcWinCh32)
    =lfRefresh(lcWinCh34)
    IF EOF()
      SHOW GET pbNew ENABLE 
      SHOW GET pbPacks ENABLE
    ENDIF
    STORE .T. TO llReMerTax,llCUpdate
  ENDIF
ENDIF  

*!**************************************************************************
*! Name      : lfBrowFlds
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfBrowFlds()
*!**************************************************************************
*! Due to C200330,1
*!**************************************************************************
FUNCTION lfBrowFlds

lcOrderBr = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
            [lcPack=ALLTRIM(Pack_ID)+'-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('ARDINV','GETGMSZ')+'-'+;
                    ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R,]+;
	        [STYLE= Style :H=lcStyHdr:R,]+;
            [GROUP:H='G':P='!':1:R,]+; 
			 IIF(llUseDyes,;
            [DYELOT= Dyelot :H='Dyelot':10:R,],[])+;
		    [nAvail=lfGetTotStk():H='Avail.':P='999999':R,]+;
		    [TOTQTY= TotQty]+;
		    [:H='Open':P='999999' :R,]+;
  		    [OpnAmt= TotQty*Price]+;
            [:H='Opn Amnt.':P='9999999999.99' :R,]+;
		    [TOTPIK=TotPik]+;
		    [:H='Picked':P='999999' :R,]+;
            [PikAmt=TotPik*Price]+;
            [:H='Pik Amnt.':P='9999999999.99':R]

*!**************************************************************************
*! Name      : lfRlesOptn
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfRlesOptn()
*!**************************************************************************
*! Due to C200330,1
*!**************************************************************************
FUNCTION lfRlesOptn

RELEASE PAD _Option OF _MSYSMENU

*!**************************************************************************
*! Name      : lfAddBar
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfAddBar()
*!**************************************************************************
*! Due to C200330,1
*!**************************************************************************
FUNCTION lfAddBar

IF llFirst
  llFirst = .F.
  *-- Options menu pad definition
  DEFINE PAD _OPTION OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P,SPACE(1)
  ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP
  *-- Options popup definition
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  DEFINE BAR 1 OF _OPTIONPOP PROMPT '\<Sort By Pack' SKIP FOR (lnactfolder<>2 OR !laScrMode[3])
  ON SELECTION POPUP _OPTIONPOP DO lfPikBrow IN gcDef_Path+'GMAMAIN.prg'
ENDIF

*!**************************************************************************
*! Name      : lfSoAddBar
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfSoAddBar()
*!**************************************************************************
*! Due to C102570,1
*!**************************************************************************
FUNCTION lfSoAddBar

*B125563,1 NNA 02/07/2005 (BEGIN) This line moved here from the middle of the function
lcFileNamA = lcOrdLine+"A"
*B125563,1 NNA (END)

DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Show Order Lines by Pack' SKIP FOR (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline))
*--to summrize by pack

*B125563,1 NNA 02/07/2005 (BEGIN) Add new condition to skip the next bar if (Show Summary) has selected
*DEFINE BAR 19 OF _INQURYPOP PROMPT '\<Summarize by Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]
DEFINE BAR 19 OF _INQURYPOP PROMPT '\<Summarize by Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2] ;
       OR &lcFileNamA..lnBarNO=11
DEFINE BAR 11 OF _INQURYPOP PROMPT 'Show Su\<mmary'     SKIP FOR (lnActFolder<>2) .OR. !llMultiSt ;
      .OR. (lnSortBy=1) OR INLIST(lnOrdTrans,4,5,6,7) OR &lcFileNamA..lnBarNO = 19
*B125563,1 NNA (END)
*C200404,1 HBG 08/28/2002 Add new Bar to Allow Modify # of ordered pack [Begin]
*B125563,1 NNA 02/07/2005 (BEGIN) Move this line to the top of the function
*lcFileNamA = lcOrdLine+"A"
*B125563,1 NNA (END)

llRngPack = &lcFileNamA..llRngPack
llFromBulk = &lcFileNamA..llFromBulk
DEFINE BAR 20 OF _INQURYPOP PROMPT '\<Modify # of ordered Packs' SKIP FOR (lnactfolder<>2) OR ;
                       laScrMode[2] OR (laScrMode[3] AND ORDHDR.Status = 'C') OR (&lcFileNamA..llRngPack);
                                    OR (&lcFileNamA..llFromBulk)
*C200404,1 [End]

*C200424,1 MHM 10/28/2002 Add new Bar to Allow Modify CustPo# [Begin]
DEFINE BAR 21 OF _INQURYPOP PROMPT 'Modify CustPO# & Retail Price' SKIP FOR (lnactfolder<>2) OR ;
                       laScrMode[2] OR (laScrMode[3] AND ORDHDR.Status = 'C') 
*C200424,1 MHM 10/28/2002 Add new Bar to Allow Modify CustPo# [Begin]

*!**************************************************************************
*! Name      : lfEdAddBar
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfEdAddBar()
*!**************************************************************************
*! Due to C102570,1
*!**************************************************************************
FUNCTION lfEdAddBar

DEFINE BAR 17 OF _INQURYPOP PROMPT '\<Show Order Lines by Pack' SKIP FOR (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline))
*--to summrize by pack
DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Summarize by Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]

*C200404,1 HBG 08/28/2002 Add new Bar to Allow Modify # of ordered pack [Begin]
lcFileNamA = lcOrdLine+"A"
llRngPack = &lcFileNamA..llRngPack
llFromBulk = &lcFileNamA..llFromBulk
DEFINE BAR 19 OF _INQURYPOP PROMPT '\<Modify # of ordered Packs' SKIP FOR (lnactfolder<>2) OR ;
                       laScrMode[2] OR (laScrMode[3] AND ORDHDR.Status = 'C') OR (&lcFileNamA..llRngPack);
                                    OR (&lcFileNamA..llFromBulk)
*C200404,1 [End]
*!**************************************************************************
*! Name      : lfSetFlag
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfSetFlag()
*!**************************************************************************
*! Due to C200330,1
*!**************************************************************************
FUNCTION lfSetFlag

llFirst = .T.
*!**************************************************************************
*! Name      : lfShwGets
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfShwGets()
*!**************************************************************************
*! Due to C200330,1
*!**************************************************************************
FUNCTION lfShwGets

IF laScrMode[3]
  llRange = .T.
  *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in PIKLINE instead of SPCK_HDR [Begin]              
  *llGetPack = SEEK('P'+laData[4]+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')  
  *IF !llGetPack 
  *  llGetPack = SEEK('P*****'+m.Pack_id+m.cPkColor+m.cPckSize+m.cPkVersion,'SPCK_HDR')  
  *ENDIF 
  *IF llGetPack 
  *  llRange = SPCK_HDR.lRange
  *ENDIF  
  *C200479,1 [End]
  IF !EMPTY(m.Pack_id) 
    *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in PIKLINE instead of SPCK_HDR [Begin]              
    *IF !(llRange)
    IF !(m.lRange)
    *C200479,1 [End]
      SHOW GET m.Pik1   DISABLE
      SHOW GET m.Pik2   DISABLE
      SHOW GET m.Pik3   DISABLE    
      SHOW GET m.Pik4   DISABLE    
      SHOW GET m.Pik5   DISABLE
      SHOW GET m.Pik6   DISABLE    
      SHOW GET m.Pik7   DISABLE
      SHOW GET m.Pik8   DISABLE    
      SHOW GET m.TotPik DISABLE    
    ELSE
      SHOW GET m.Pik1   ENABLE    
      SHOW GET m.Pik2   ENABLE    
      SHOW GET m.Pik3   ENABLE    
      SHOW GET m.Pik4   ENABLE    
      SHOW GET m.Pik5   ENABLE    
      SHOW GET m.Pik6   ENABLE    
      SHOW GET m.Pik7   ENABLE    
      SHOW GET m.Pik8   ENABLE    
      SHOW GET m.TotPik ENABLE    
    ENDIF  
  ELSE
    SHOW GET m.Pik1   ENABLE    
    SHOW GET m.Pik2   ENABLE    
    SHOW GET m.Pik3   ENABLE    
    SHOW GET m.Pik4   ENABLE    
    SHOW GET m.Pik5   ENABLE    
    SHOW GET m.Pik6   ENABLE    
    SHOW GET m.Pik7   ENABLE    
    SHOW GET m.Pik8   ENABLE    
    SHOW GET m.TotPik ENABLE    
    *SHOW GET pbRemove ENABLE    
  ENDIF  
ENDIF
*!**************************************************************************
*! Name      : lfAddIndix
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfAddIndix()
*!**************************************************************************
*! Due to C200330,1
*!**************************************************************************
FUNCTION lfAddIndix

INDEX ON cOrdType + Order + Pack_Id + cPkColor + cPckSize +;
         cPkVersion + STR(LineNo,6) TAG OrdPack
INDEX ON cOrdType + Order + STR(LineNo,6) TAG OrdLine
=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH') 
SELECT (lc_TmpOrdL) 

*!**************************************************************************
*! Name      : lfRemvPck
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Remove the pack 
*!**************************************************************************
*! Example   : = lfRemvPck()
*!**************************************************************************
*! Due to C200330,1
*!**************************************************************************
FUNCTION lfRemvPck

IF !EMPTY(m.Pack_ID)
  IF !EMPTY(m.cPckSize) AND SEEK('S'+LEFT(m.cPckSize,1),'SCALE')
    lcSiz = RIGHT(m.cPckSize,1)
    lcSize = SCALE.Sz&lcSiz
  ELSE
    lcSize = '*****'    
  ENDIF  
  IF gfModalGen('QRM00000B40000','ALERT',.F.,.F.,;
                'This Style in belong to a pack: '+ALLTRIM(m.Pack_ID)+'-'+ALLTRIM(m.cPkColor)+;
                '-'+ALLTRIM(lcSize)+'-'+ALLTRIM(m.cPkVersion)+;
                '. Do you want to remove the all pack') = 1
    lcPack_ID   = m.Pack_ID
    lcPkColor   = m.cPkColor
    lcPckSize   = m.cPckSize
    lcPkVersion = m.cPkVersion
    lnCurAlias = SELECT(0)
    SELECT (lc_TmpOrdL)
    SCAN FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion 
      REPLACE cStatus WITH 'D'
    ENDSCAN
    DELETE FOR Pack_ID+cPkColor+cPckSize+cPkVersion = lcPack_ID+lcPkColor+lcPckSize+lcPkVersion
  
    IF !EOF()
       SKIP-1
    ENDIF
  
    =lfwPikBrow()            && avoid browse if no need
  
    SELECT (lc_TmpOrdL)
    IF EOF()
      SHOW GET pbRemove  DISABLE
      SHOW GET m.Pik1    DISABLE
      SHOW GET m.Pik2    DISABLE  
      SHOW GET m.Pik3    DISABLE  
      SHOW GET m.Pik4    DISABLE  
      SHOW GET m.Pik5    DISABLE  
      SHOW GET m.Pik6    DISABLE  
      SHOW GET m.Pik7    DISABLE  
      SHOW GET m.Pik8    DISABLE  
      SHOW GET m.TotPik  DISABLE  
    ENDIF
    SELECT (lnCurAlias)
  ENDIF  
ENDIF

*!**************************************************************************
*! Name      : lfPIKBROW
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfPIKBROW()
*!**************************************************************************
*! Due to C200330
*!**************************************************************************
FUNCTION lfPIKBROW

IF llByLine
  DEFINE BAR 1 OF _OPTIONPOP PROMPT '\<Sort By Line #' SKIP FOR (lnactfolder<>2) OR !laScrMode[3]
  SET ORDER TO OrdPack IN (lc_TmpOrdL)
  llByLine= .F.
ELSE
  DEFINE BAR 1 OF _OPTIONPOP PROMPT '\<Sort By Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[3]
  SET ORDER TO OrdLine IN (lc_TmpOrdL)
  llByLine= .T.
ENDIF

SELECT (lc_TmpOrdL)    
LOCATE    

=lfBrowse()
                          
*!**************************************************************************
*! Name      : lfINITVAR
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : To Initiate variables for the new modification
*!**************************************************************************
*! Example   : = lfINITVAR()
*!**************************************************************************
*! Due to C102566,1
*!**************************************************************************
FUNCTION lfINITVAR   

* lcLinFil  ======> variable to store data on it
STORE 1 TO lnGmaNo
STORE '' TO lcAlias,lcGmaSz,lcGmaColor,lcGmaSize


lcStyPic   = "XXXXXXXXXXXXXXXXXX"

lcFileNamA = lcLinFil+"A"
CREATE CURSOR (lcFileNamA) (lnGmaNo N(2,0),lnType N(1,0),lcGmaSz C(5),lcAlias C(10),;
                                          lcGmaColor C(6),lcGmaSize C(3),lcGmSizeV C(3))

=gfOpenFile(gcDataDir+'STYLEUPC','GMAPKUPC','SH')
=gfOpenFile(gcDataDir+'SPCK_HDR','SPCK_HDRST','SH',@lcAlias,.T.)
SELECT(lcAlias)
m.lcAlias = ALIAS()
INSERT INTO (lcFileNamA) FROM MEMVAR 

SET FILTER TO (cpkversion = "1   ")

SET RELATION TO 'S'+LEFT(cPckSize,1) INTO Scale
*--Open STYLEUPC
SET RELATION TO Account+Pack_Id+'   '+cPkColor+cPckSize INTO STYLEUPC  ADDI

*!**************************************************************************
*! Name      : lfVALDTYPE
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : To Validate Type
*!**************************************************************************
*! Example   : = lfVALDTYPE()
*!**************************************************************************
*! Due to C102566,1
*!**************************************************************************
FUNCTION lfVALDTYPE  

*IF laType[lnType]= "Pack"
IF lnType= "Pack"
  lcStyTtl = "PackId"
  lcStyPic   = "XXXXXXXXXXXXXXXX"
  lnStyleWid = LEN(lcStyPic)
ELSE
  lcStyTtl   = gfItemMask("HI")
  lcStyPic   = gfItemMask("PI")
  lnStyleWid = LEN(lcStyPic)
ENDIF  
SHOW GET lcStyTtl
SHOW GET lnType ENABLE
=lfRefresh()
_CUROBJ = OBJNUM(laData[2])

*!**************************************************************************
*! Name      : lfVALDPACK
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : To Validate Style Or Pack
*!**************************************************************************
*! Example   : = lfVALDPACK()
*!**************************************************************************
*! Due to C102566,1
*!**************************************************************************
FUNCTION lfVALDPACK
PRIVATE llEmptyAcc
STORE .F. TO llEmptyAcc


*--102566
lcAlias    = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
lcGmSizeV  = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmSizeV'))
lcGmaColor = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmaColor'))

*IF laType[lnType] = "Style"
IF lnType = "Style"
*--102566
  laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))
  
  IF llBrowse OR !SEEK(laData[2],"Style")
    llBrowse = .F.
    laData[2]  = gfStyBrw('I',"","",.F.)
    laData[2]  = PADR(laData[2],19)
  ENDIF
  IF !EMPTY(laData[2])
    
    laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))

    SELECT Scale
    SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR
    
    lcScale = Scale.Scale
    = lfStyDesc()
    *B606160,1 HBG 14/10/2002 If there is SKU for this Account/Style get it and prevent 
    *B606160,1                adding more than one SKU# for same Account/Style [Begin]
    IF SEEK('S'+laData[1]+PADR(laData[2],19),'SPCK_HDR')
      laData[3] = SPCK_HDR.Pack_Id
      laScrMode    = .F.
      laScrmOde[2] = .T.
      SHOW GETS
    ENDIF
    *B606160,1 [End]    
  ELSE
    _CUROBJ = _CUROBJ
  ENDIF

ELSE
  = lfPackIDBrow()
  IF !EMPTY(laData[2]) AND !llEmptyAcc
    IF !EMPTY(lcGmSizeV)
      *--there is only one size in case of Pack
      SELECT(lcAlias)
      
      lcNo = RIGHT(cPckSize,1)
      SELECT Scale
      SCATTER FIELDS cnt,Sz&lcNo MEMVAR
      m.Sz1=m.Sz&lcNo
      m.Cnt = 1
      *--102566
      *lcGmaSz = m.Sz1
      *lnGmaNo = m.Cnt 
      PRIVATE lcFileNama
      lcFileNama = lcLinFil+"A"
      REPLACE &lcFileNamA..lcGmaSz WITH m.Sz1
      REPLACE &lcFileNamA..lnGmaNo WITH m.Cnt 
      *-102566,1
      
      lcScale = Scale.Scale
      = lfPckDesc()
      SELECT(lcAlias)
    ENDIF
    IF laScrMode[1]
      = lfRefresh()
    ENDIF  
     
    *B606160,1 HBG 14/10/2002 If there is SKU for this  Account/Pack Id/Color/Size. 
    *B606160,1                get it and prevent adding more than one SKU# for same 
    *B606160,1                Account/Pack Id/Color/Size.[Begin]
    lcAlias    = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
    lcGmSizeV  = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmSizeV'))
    lcGmaColor = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmaColor'))
    IF SEEK('S'+laData[1]+PADR(laData[2],19)+PADR(lcGmaColor,6)+PADR(lcGmSizeV,3),lcAlias)
      laData[3] = &lcAlias..Pack_Id
      SELECT SPCK_HDR
      lcOrder = SET('ORDER')
      SET ORDER TO TAG Spck_hdrst 
      llSeekGma = SEEK('S'+laData[1]+PADR(laData[2],19)+PADR(lcGmaColor,6)+PADR(lcGmSizeV,3)+PADR(laData[3],16),'SPCK_HDR')
      SET ORDER TO &lcOrder
      laScrMode    = .F.
      laScrmOde[2] = .T.
      SHOW GETS
    ENDIF
    *B606160,1 [End]
    
  ELSE
    llBrowse = .F.
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfPckDesc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/2002
*! Purpose   : To get the desc. ,division andseason for the Pack
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C102566,1
*!*************************************************************
*! Exampl    : =lfPckDesc()
*!*************************************************************

FUNCTION lfPckDesc
*--102566
PRIVATE lcAlias
lcAlias = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
*--102566

IF !EMPTY(ladata[2])
  IF SEEK('P'+laData[1]+laData[2],lcAlias)
    lcStyDesc = &lcAlias..Desc
    laData[5] = &lcAlias..cDivision
    laData[6] = &lcAlias..Season
    lcDivDesc = gfCodDes(&lcAlias..cDivision, "CDIVISION")
    lcSeaDesc = gfCodDes(&lcAlias..Season  , "SEASON"   )
  ENDIF
ENDIF

*-- There is no need to make refresh if it isn't select mode
*-- because in other modes lfChkMulSca is called which by it's part calls 
*-- lfGetSzCode which calls lfRefresh
IF laScrMode[1]
  = lfRefresh()
ENDIF  


*!*************************************************************
*! Name      : lfPackIDBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/15/02
*! Purpose   : browse all pack id to select from it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llFromCopy
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Due to C102566,1
*!*************************************************************
*! Example   : = lfPackIDBrow()
*!*************************************************************
*--102566,1
FUNCTION lfPackIDBrow

PRIVATE lcBrFields ,lcBrTitle,lcKey,lnCurAlias,laRetData

*--102566
PRIVATE lcAlias
lcAlias = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
lcFileNama = lcLinFil+"A"
*--102566

lnCurAlias = SELECT(0)

lcBrTitle  = 'Available Packs'

lcBrFields = "Account           :R :H='Account'      :16," +;
             "Pack_ID           :R :H='Pack_ID'      :16," +;
             "CPKCOLOR          :R :H='Color'        :6,"  +;
             "lcGmSize=gfDoTriger('ARDINV','GETGMSZ'):R :H='Size' :5," +;             
             "Desc              :R :H='Desc'         :30," +;
             "cDivision         :R :H='Devision'     :8,"  +;
             "Season            :R :H='Season'       :8,"  +;
             "NPKSLPRICE        :R :H='Selling Price':13," +;
             "STYLEUPC.CUPCNUM1 :R :H='UPC'          :7,"  +;  
             "STYLEUPC.CUPCNUM2 :R :H=''             :6,"  +;  
             "STYLEUPC.CUPCNUM3 :R :H=''             :2"


IF !SEEK('P'+laData[1],lcAlias)

  *--There are no records to browse.
  *-- OK
  llEmptyAcc = .T.
  = gfModalGen("INM42097B42001","Dialog")  
  _CUROBJ = OBJNUM(laData[2])
ELSE
  lcKey = "'P'" +' + laData[1]' 
  SELECT(lcAlias)
  lcOrder = SET('ORDER')
  SET ORDER TO TAG Spck_hdrVR
  
  *102566
  PRIVATE laBrow
  DIMENSION laBrow[10]
  laBrow = ''
  =gfBrows(lcKey,"Account, Pack_ID   , Desc     ,cDivision  ,Season,;
                  LRANGE , NPKSLPRICE, CPKVERSION, CPKCOLOR , CPCKSIZE" ;  
          ,"laBrow",lcBrTitle)

  laData[2]  = ALLTRIM(laBrow[2])
  lcGmaSize  = lfGetGmSz(laBrow[10])
  *--102566
  *lcGmSizeV  = laBrow[10]
  *lcGmaColor = laBrow[9]
  PRIVATE lcFileNama
  lcFileNama = lcLinFil+"A"
  REPLACE  &lcFileNama..lcGmaSize  WITH lcGmaSize
  REPLACE  &lcFileNama..lcGmaColor WITH laBrow[9]
  REPLACE  &lcFileNama..lcGmSizeV  WITH   laBrow[10]
  
  *--102566  
  laData[5]  = laBrow[4]
  laData[6]  = laBrow[5]
  SET ORDER TO &lcOrder.  
ENDIF

SELECT(lnCurAlias)


*!*************************************************************
*! Name      : lfCONTSHW
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/16/02
*! Purpose   : Control showing of sizes
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Due to C102566,1
*!*************************************************************
*! Example   : = lfCONTSHW()
*!*************************************************************
*--102566,1
FUNCTION lfCONTSHW

*--102566
*IF laType[lnType] = "Pack"
IF lnType = "Pack"
*--102566
  *--102566
  *m.Sz1=lcGmaSz
  *m.Cnt = lnGmaNo
  PRIVATE lcFileNama
  lcFileNama = lcLinFil+"A"
  m.Cnt = &lcFileNamA..lnGmaNo
  m.Sz1 = &lcFileNamA..lcGmaSz
  *--102566
  *--always disable prepack
  SHOW GET llPPak DISABLE
ENDIF
IF laScrMode[1] OR laScrMode[3] OR laScrMode[4]
  *102566
  SHOW GET lntype ENABLE
  *SHOW GET &lcFileNamA..lnType ENABLE
ELSE
  *--102566
  SHOW GET lntype DISABLE
  *SHOW GET &lcFileNamA..lnType DISABLE
  *102566
ENDIF  
*--End lfCONTSHW
*!*************************************************************
*! Name      : lfSAVSRGMA
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/16/2002
*! Purpose   : To make local save for GMA.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!************************************************************
*! Due to C102566,1
*!*************************************************************
*! Example   : DO lfSAVSRGMA
*!*************************************************************

PROCEDURE lfSAVSRGMA
PRIVATE lnCurAlias,lcCurNdx,llReturn,lnChoice

*--102566
PRIVATE lcAlias , lcGmaColor
lcAlias = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
lcGmaColor = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmaColor'))
lcGmSizeV = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmSizeV'))

*--102566

lnCurAlias = SELECT(0)
llReturn = .T.
*--102566
*IF latype[lntype]="Pack"
IF lntype="Pack"
*--102566

  SELECT Spck_Lin
  SET ORDER TO Spck_linSt
ENDIF  

SELECT (lcLinFil)
*--102566
*IF laType[lnType]="Style"
IF lnType="Style"
*--102566
  INDEX ON Type+Account+Style+Pack_ID TAG StyPack ADDITIVE
ELSE
  INDEX ON TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION+PACK_ID TAG StyPack ADDITIVE
ENDIF  

GO TOP
IF EOF() OR BOF() OR DELETED()
  *-- You have to enter at least one SKU or create prepak
  *-- <Ok>
  = gfModalGen("INM42099B42001","Dialog")
  llReturn = .F.
ELSE
  SCAN
    IF !EMPTY(&lcLinFil..SKU) AND EMPTY(&lcLinFil..Prepak) 
      *-- You have to select prepak
      *-- <OK>
      = gfModalGen("QRM42100B42001","Dialog")
      llReturn = .F.
      EXIT
    ENDIF
  ENDSCAN
  IF llReturn
    SELECT Spck_Lin

    *--mhm102566
    *IF laType[lntype] = "Pack"
    IF lntype = "Pack"
    *--mhm102566
      llSeekGma = SEEK('S'+laData[1]+laData[2]+lcGmaColor+lcGmSizeV,(lcAlias))
    ELSE
      llSeekGma = SEEK('S'+laData[1]+laData[2],'Spck_Lin')
    ENDIF  

    IF llSeekGma
      SELECT Spck_Lin
      SCAN REST FOR Type+Account+Style+cPkColor+cPckSize+Pack_id+cPkVersion = "S"+laData[1]+laData[2]

        *! B124944,1 MHM 12/20/2004 check for the same Color size for GMA[Start]
        IF CPKCOLOR <> LEFT(lcGmaColor,6) OR CPCKSIZE <> LEFT(lcGmSizeV,3) OR PACK_ID <> ALLTRIM(laData[3])
          LOOP
        ENDIF
        *! B124944,1 MHM [End]
        
        IF ALLTRIM(Pack_ID) = ALLTRIM(laData[3])
          BLANK
          DELETE
        ENDIF
      ENDSCAN
    ENDIF
    
    SELECT(lcLinFil)
    SCAN
      
      SCATTER MEMVAR
      SELECT Spck_Lin
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE TotQty     WITH IIF(EMPTY(&lcLinFil..PrePak),1,0) ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH gfGettime(),;
              cAdd_User  WITH gcUser_Id,;
              CPKCOLOR   WITH lcGmaColor,;
              CPCKSIZE   WITH lcGmSizeV,;
              cPkVersion WITH "1   ",;
              cSkuType   WITH IIF(lnType="Style",'S','P')

    *--102566
    *IF laType[lntype] = "Pack"
    IF lntype = "Pack"
    *--102566
      lcHdrKey = "S"+ladata[1]+PADR(ladata[2],19)+lcGmaColor+lcGmSizeV+m.Pack_Id
    ELSE
      lcHdrKey = "S"+ladata[1]+PADR(ladata[2],19)+m.Pack_Id
    ENDIF  

      SELECT Spck_Hdr
      IF SEEK(lcHdrKey,'Spck_Hdr')
        REPLACE Spck_Hdr.Desc      WITH laData[4],;
                Spck_Hdr.cDivision WITH laData[5],;
                Spck_Hdr.Season    WITH laData[6]

        *--to save Color in user defined field        
        lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKCOLOR'),1)
        laUsrField[lnFieldPos,6] = lcGmaColor

        *--to save Size in user defined field        
        lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPCKSIZE'),1)
        laUsrField[lnFieldPos,6] = lcGmSizeV

        *--to save Size in user defined field        
        lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKVERSION'),1)
        laUsrField[lnFieldPos,6] = "1   "

        *--to save pack calculated price in user defined field        
        lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CSKUTYPE'),1)
        *--102566
        *laUsrField[lnFieldPos,6] = IIF(laType(lnType)="Style",'S','P')
        laUsrField[lnFieldPos,6] = IIF(lnType="Style",'S','P')
        *--102566
                
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
IF llReturn

  *--102566
  *IF laType[lntype] = "Pack"
  IF lntype = "Pack"
    lcHdrKey = "S"+ladata[1]+PADR(ladata[2],19)+lcGmaColor+lcGmSizeV+ALLTRIM(laData[3])
  ELSE
    lcHdrKey = "S"+ladata[1]+PADR(ladata[2],19)+lcGmaColor+lcGmSizeV+ALLTRIM(laData[3])
  ENDIF  

  SELECT Spck_Hdr
  IF SEEK(lcHdrKey,'Spck_Hdr')
    REPLACE Spck_Hdr.Desc      WITH laData[4],;
            Spck_Hdr.cDivision WITH laData[5],;
            Spck_Hdr.Season    WITH laData[6]    
  ELSE
    INSERT INTO Spck_Hdr (Type,Account,Style,Pack_ID,Sku,Desc,cDivision,Season,;
                          dAdd_Date,cAdd_Time,cAdd_User) VALUES ;
    ('S' ,laData[1], PADR(laData[2],19),ladata[3]+IIF(llPPak and Customer.Skutmpl='JCP',"*ONLY",""),;
                          laData[3],ladata[4],laData[5],laData[6],gdSysDate,gfGettime(),gcUser_Id)
  ENDIF
  *--to save Color in user defined field        
  lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKCOLOR'),1)
  laUsrField[lnFieldPos,6] = lcGmaColor

  *--to save Size in user defined field        
  lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPCKSIZE'),1)
  laUsrField[lnFieldPos,6] = lcGmSizeV

  *--to save Size in user defined field        
  lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CPKVERSION'),1)
  laUsrField[lnFieldPos,6] = "1   "

  *--to save pack calculated price in user defined field        
  lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CSKUTYPE'),1)
  *--102566
  *laUsrField[lnFieldPos,6] = IIF(laType(lnType)="Style",'S','P')
  laUsrField[lnFieldPos,6] = IIF(lnType="Style",'S','P')
  *--102566
  
ENDIF  
llCSave = llReturn

*!*************************************************************
*! Name      : lfVALDSKU
*! Developer : Mohamed Shokry (MHM)
*! Date      : 04/16/2002
*! Purpose   : To Validate SKU to avoid making same sku for style and pack
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Due to C102566,1
*!*************************************************************
*! Example   : DO lfVALDSKU
*!*************************************************************

FUNCTION lfVALDSKU
PRIVATE lcStyPck,lcOldStyPk
STORE .F. TO llSkuFound

*--102566
PRIVATE lcAlias , lcGmaColor
lcAlias = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
lcGmaColor = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmaColor'))
lcGmSizeV = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmSizeV'))
*--102566

*-- to check if user choose the same SKU to nother Style or Pack
*B606160,1 HBG 08/27/2002 Fix Bug of unable to assign the main SKU for a Pack and a style in the same time [End]
*--102566
*IF laType[lntype] = "Pack"
*IF lntype = "Pack"
*--102566
*  SELECT (lcAlias)
*  IF SEEK("S"+ladata[1])
*    SCAN REST WHILE Type+Account+Style+cPkColor+cPckSize+Pack_id+cPkVersion = "S"+ladata[1] FOR cSkuType = "S"
*      IF ladata[3] = Pack_id
*        STORE .T. TO llSkuFound
*        lcOldStyPk = Style
*      ENDIF  
*    ENDSCAN
*  ENDIF
*ELSE
*  SELECT SPCK_HDR
*  IF SEEK("S"+ladata[1])
*    SCAN REST WHILE Type+Account+Style+Pack_id = "S"+ladata[1] FOR cSkuType = "P"
*      IF ladata[3] = Pack_id
*        STORE .T. TO llSkuFound
*        lcOldStyPk = Style
*      ENDIF  
*    ENDSCAN
*  ENDIF
*ENDIF  

*--mhm102566
*IF laType[lnType] = "Pack"
*IF lnType = "Pack"
*--mhm102566
*  lcStyPck ="Pack"
*ELSE
*  lcStyPck ="Style"
*ENDIF

*IF llSkuFound
*  *--102566
*  *IF laType[lnType] = "Style"
*  IF lnType = "Style"
*  *--102566
*    lcStyPck ="Pack"
*  ELSE
*    lcStyPck ="Style"
*  ENDIF

*  =gfModalGen("INM00000B42001","Dialog",.F.,.F.,;
*           'SKU already assigned to ' +lcStyPck+' '+lcOldStyPk+' can not proceed')
*ENDIF

*IF llSkuFound
*  RETURN
*ENDIF  
*B606160,1 [End]

IF lnType = "Pack"
  lcStyPck ="Pack"
ELSE
  lcStyPck ="Style"
ENDIF

  IF laData[3] = "?"
    llBrowse = .T.
  ENDIF
  IF !EMPTY(laData[1]) AND !EMPTY(laData[2]) AND !EMPTY(laData[3])  OR llBrowse
    
    *--102566
    *IF laType[lntype] = "Pack"
    IF lntype = "Pack"
    *--102566
      llSeekGma = SEEK('S'+laData[1]+laData[2]+lcGmaColor+lcGmSizeV+laData[3],(lcAlias))
    ELSE
      llSeekGma = SEEK('S'+laData[1]+laData[2]+ladata[3],'SPCK_HDR')
    ENDIF  

    IF !llSeekGma OR llBrowse
      IF !llBrowse AND !EMPTY(laData[3])
        *-- THIS SKU IS NOT FOUND DO YOU WANT TO BROWSE OR ADD
        *-- < Add > < Browse > < Reenter >
        lnABR = gfModalGen("QRM42090B42003","Dialog",'SKU'+'|'+laData[1]+' and '+lcStyPck+' '+laData[2])

        DO CASE
          CASE lnABR = 1            &&Add
            IF lfPackScr() = 1    && OK
              laScrMode    = .F.
              laScrMode[4] = .T.
              _CUROBJ = OBJNUM(laData[4])
              SHOW GETS
            ELSE                  && Cancel
              _CUROBJ = OBJNUM(laData[3])
            ENDIF
          CASE lnABR = 2            &&Browse
            = lfSKUBrowG()
          CASE lnABR = 3            &&Reenter
            STORE SPACE(0) TO laData[3]
            _CUROBJ = OBJNUM(laData[3])
        ENDCASE
      ELSE
        llBrowse = .F.
        = lfSKUBrowG()
      ENDIF
    ELSE
      *--102566
      *IF laType[lntype] = "Pack"
      IF lntype = "Pack"
      *--102566
        SELECT SPCK_HDR
        lcOrder = SET('ORDER')
        SET ORDER TO TAG Spck_hdrst 
        llSeekGma = SEEK('S'+laData[1]+PADR(ladata[2],19)+lcGmaColor+lcGmSizeV+laData[3],'SPCK_HDR')
        SET ORDER TO &lcOrder
      ENDIF  
      laScrMode    = .F.
      laScrmOde[2] = .T.
      SHOW GETS
    ENDIF
  ELSE
    IF !EMPTY(laData[3])
      =lfSKUBrowG()
    ENDIF
  ENDIF
*-102566
IF llSkuFound
  STORE SPACE(0) TO laData[3]
  _CUROBJ = OBJNUM(laData[3])
ENDIF  
*-102566

*!*************************************************************
*! Name      : lfSKUBrowG
*! Developer : Mohamed Shokry
*! Date      : 04/17/2202
*! Purpose   : Browse all sku.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Due to C102566,1
*!*************************************************************
*! Example   : = lfSKUBrowG()
*!*************************************************************

FUNCTION lfSKUBrowG

PRIVATE lcBrFields ,lcBrTitle,lcKey,lnCurAlias
PRIVATE laBrow    &&This is to hold the data of the sku form sku browse
DIMENSION laBrow[6]
STORE SPACE(0) TO laBrow
*--102566
PRIVATE lcAlias
lcAlias = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
lcGmSizeV = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmSizeV'))

*--102566

lnCurAlias = SELECT(0)

lcBrTitle  = 'Sku #'
lcBrFields = "Account   :R :H='Account' :16," +;
             "Style     :R :H=lcStyPck  :19," +;
             "Pack_ID   :R :H='SKU#'    :16," +;
             "Desc      :R :H='Desc'    :30," +;
             "cDivision :R :H='Devision'  :8,"  +;
             "Season    :R :H='Season':8"

llFound = .F.
*--102566
*-IF laType[lntype] = "Pack"
IF lntype = "Pack"
*--102566
  SELECT (lcAlias)
ELSE
  SELECT SPCK_HDR
ENDIF  
=SEEK('S')

*--102566
*IF laType[lntype] = "Pack"
IF lntype = "Pack"
*--102566
  COUNT REST WHILE Type+Account+Style+cPkColor+cPckSize+Pack_id+cPkVersion ='S' FOR Pack_Id=laData[3] TO lnNoSku
ELSE
  COUNT REST WHILE Type+Account+Style+Pack_id ='S' FOR Pack_Id=laData[3] TO lnNoSku
ENDIF  

PRIVATE llShowGet
llShowGet = .F.

DO CASE
  CASE !EMPTY(laData[3]) AND lnNoSku > 0
    IF lnNoSku = 1
      =SEEK('S')
      *--102566
      *IF laType[lntype] = "Pack"
      IF lntype = "Pack"
      *--102566
       LOCATE REST WHILE Type+Account+Style+cPkColor+cPckSize+Pack_id+cPkVersion ='S' FOR Pack_Id=laData[3] 
      ELSE
       LOCATE REST WHILE Type+Account+Style+Pack_id ='S' FOR Pack_Id=laData[3] 
      ENDIF  
      
      =SEEK (ALLTRIM(Style),'STYLE')
      llShowGet = .T.

    ELSE  
      llFound = .T.
      lcKey = "FOR Type = 'S' AND Pack_ID=laData[3]"
    ENDIF

  CASE EMPTY(laData[1]) AND EMPTY(laData[2])
    *--102566
    *IF SEEK('S',IIF(laType[lntype] = "Pack",(lcAlias),'SPCK_HDR'))
    IF SEEK('S',IIF(lntype = "Pack",(lcAlias),'SPCK_HDR'))
    *--102566
      llFound = .T.
      lcKey = "FOR Type = 'S' "        
    ELSE 
      llFound = .F.
    ENDIF

  CASE EMPTY(laData[1]) AND !EMPTY(laData[2])
    *--102566
    *IF laType[lntype] = "Pack"
    IF lntype = "Pack"
    *--102566
      SELECT (lcAlias)
    ELSE
      SELECT SPCK_HDR
    ENDIF  
    LOCATE FOR Type = 'S' AND Style = laData[2]
    IF FOUND()
      llFound = .T.
      lcKey = "FOR Type+Style = 'S' + laData[2]"        
    ELSE 
      llFound = .F.
    ENDIF

  CASE !EMPTY(laData[1]) AND EMPTY(laData[2])

    *--102566
    *IF SEEK('S'+laData[1],IIF(laType[lntype] = "Pack",(lcAlias),'SPCK_HDR'))
    IF SEEK('S'+laData[1],IIF(lntype = "Pack",(lcAlias),'SPCK_HDR'))
    *--102566
      llFound = .T.
      lcKey = "FOR Type+Account = 'S' + laData[1]"
    ELSE 
      llFound = .F.
    ENDIF
    
  CASE !EMPTY(laData[1]) AND !EMPTY(laData[2]) 

    *--102566
    *IF laType[lntype] = "Pack"
    IF lntype = "Pack"
    *--102566
      llSeekGma = SEEK('S'+laData[1]+PADR(laData[2],19)+lcGmaColor+lcGmSizeV,(lcAlias))
      SELECT (lcAlias)
    ELSE
      llSeekGma = SEEK('S'+laData[1]+PADR(laData[2],19),'SPCK_HDR')
      SELECT SPCK_HDR
    ENDIF  

    IF llSeekGma
      llFound=.T.
       *--102566
       *IF laType[lnType] = "Style"
       IF lnType = "Style"
       *--102566
         lcKey = "'S'+laData[1]+laData[2]"
       ELSE
         lcKey = "'S'+laData[1]+PADR(laData[2],19)+lcGmaColor+lcGmSizeV"
       ENDIF  
    ELSE
      llFound=.F.  
    ENDIF    
         
ENDCASE

IF llShowGet
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS
ELSE

  IF llFound
    *--102566
    *IF laType[lntype] = "Pack"
    IF lntype = "Pack"
    *--102566
      SELECT (lcAlias)
    ELSE
      SELECT SPCK_HDR
    ENDIF  

    = gfBrows(lcKey,"Account,Style,Pack_ID,Desc,cDivision,Season",;
              "laBrow",lcBrTitle)
    IF !EMPTY(laBrow)

      = ACOPY(laBrow,laData)
      *--102566
      *IF laType[lnType] = "Style"
      IF lnType = "Style"
      *--102566
        laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))
       ENDIF
      *--102566
      *IF laType[lnType] = "Style"
      IF lnType = "Style"
      *--102566
        =SEEK (ALLTRIM(laData[2]),'STYLE')
      ELSE
        SELECT SPCK_HDR
        lcOldOrd = SET('ORDER')
        SET ORDER TO SPCK_HDRST
      
        =SEEK('S'+laData[1]+PADR(laData[2],19)+lcGmaColor+lcGmSizeV+ALLTRIM(laData[3]),'SPCK_HDR')
        SET ORDER TO &lcOldOrd

        lcGmaSize  = lfGetGmSz(CPCKSIZE)
        lcGmSizeV  = CPCKSIZE 
        *--102566
        *lcGmaColor = CPKCOLOR
        PRIVATE lcFileNamA
        lcFileNamA = lcLinFil+"A"
        REPLACE &lcFileNamA..lcGmaColor WITH CPKCOLOR
        *--102566
      ENDIF  
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ELSE
      _CUROBJ = OBJNUM(laData[3])
    ENDIF
    
  ELSE
    *--There are no records to browse.
    *-- OK
    = gfModalGen("INM42097B42001","Dialog")  
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF

SELECT(lnCurAlias)
*!*************************************************************
*! Name      : lfPRCSHOW1
*! Developer : Mohamed Shokry
*! Date      : 04/17/2202
*! Purpose   : control showing of price .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPRCSHOW1()
*!*************************************************************
*! C102570,1 MHM 
FUNCTION lfPRCSHOW1

IF laScrMode[4] OR laScrMode[3] 
 SET ORDER TO SPCK_LINVR IN SPCK_LIN 
 *--mhm2000 if this style not belong to any Pack then let it works as it is
 IF SEEK('P'+laData[2]+&lcOrdLine..pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion+&lcOrdLine..Style,'SPck_LIN') OR;
    SEEK('P*****'+&lcOrdLine..pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion+&lcOrdLine..Style,'SPck_LIN')  
   SHOW GET m.pack_id DISABLE
   SHOW GET ibPackid  DISABLE
   SHOW GET m.Style   DISABLE
   SHOW GET ibStyle   DISABLE
   llSeek = SEEK('P'+laData[2]+&lcOrdLine..pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion,'SPck_Hdr')
   IF !llSeek
     llSeek = SEEK('P*****'+&lcOrdLine..pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion,'SPck_Hdr')
   ENDIF
   *C200433,1 HBG 17/11/2002 Enable price field in case of range pack & price per 
   *C200433,1                piece and selling price = 0 [Begin]
   *IF !SPck_Hdr.lPckPrPiec  
   *C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
   *IF !SPck_Hdr.lPckPrPiec  AND !SPck_Hdr.lRange AND SPck_LIN.nPck_price <> 0 AND;
      SPck_Hdr.nPkSlPrice <> 0
   IF !m.lPckPrPiec  AND !m.lRange AND SPck_LIN.nPck_price <> 0 AND;
       m.nPkSlPrice <> 0
   *C200479,1 [End]
   *C200433,1 [End]
     SHOW GET m.Gros_Price DISABLE
     SHOW GET m.Disc_Pcnt  DISABLE
     SHOW GET m.Price      DISABLE
   ELSE
     SHOW GET m.Gros_Price ENABLE
     SHOW GET m.Disc_Pcnt  ENABLE
     SHOW GET m.Price      ENABLE
   ENDIF
   *--control show in case of QTY changed 
   llSeek = SEEK('P'+laData[2]+&lcOrdLine..pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion,'SPck_Hdr')  
   IF !llSeek 
     llSeek = SEEK('P*****'+&lcOrdLine..pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion,'SPck_Hdr')  
   ENDIF
   *C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
   *IF !SPck_Hdr.lRange  
   IF !m.lRange  
   *C200479,1 [End]
     FOR lnCount = 1 TO 8
       lcCount = STR(lnCount,1)
       SHOW GET m.Qty&lcCount  DISABLE
     ENDFOR
     SHOW GET m.TotQty         DISABLE
   ELSE
     FOR lnCount = 1 TO 8
       lcCount = STR(lnCount,1)
       SHOW GET m.Qty&lcCount  ENABLE
     ENDFOR
     SHOW GET m.TotQty     ENABLE
   ENDIF

 ELSE
    SHOW GET m.pack_id ENABLE
    SHOW GET ibPackid  ENABLE
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      SHOW GET m.Qty&lcCount  ENABLE
    ENDFOR
    SHOW GET m.TotQty     ENABLE
    SHOW GET m.Gros_Price ENABLE
    SHOW GET m.Disc_Pcnt  ENABLE
    SHOW GET m.Price      ENABLE
 ENDIF
 *--mhm2000 if this style not belong to any Pack then let it works as it is
ENDIF
SHOW GET pbPacks ENABLE
*!*************************************************************
*! Name      : lfPRCSHOW2
*! Developer : Mohamed Shokry
*! Date      : 04/17/2202
*! Purpose   : control showing of price .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPRCSHOW2()
*!*************************************************************
*! C102570,1 MHM 
FUNCTION lfPRCSHOW2

IF laScrMode[4] OR laScrMode[3] 
 SET ORDER TO SPCK_LINVR IN SPCK_LIN 
 *--mhm2000 if this style not belong to any Pack then let it works as it is
 IF SEEK('P'+laData[2]+&lcTempLine..pack_id+&lcTempLine..cPkColor+&lcTempLine..cPckSize+&lcTempLine..cPkVersion+&lcTempLine..Style,'SPck_LIN') OR;
    SEEK('P*****'+&lcTempLine..pack_id+&lcTempLine..cPkColor+&lcTempLine..cPckSize+&lcTempLine..cPkVersion+&lcTempLine..Style,'SPck_LIN')  
  SHOW GET m.pack_id DISABLE
  SHOW GET ibPackid  DISABLE
  SHOW GET m.Style   DISABLE
  SHOW GET ibStyle   DISABLE
  llSeek = SEEK('P'+laData[2]+&lcTempLine..pack_id+&lcTempLine..cPkColor+&lcTempLine..cPckSize+&lcTempLine..cPkVersion,'SPck_Hdr')
  IF !llSeek
    llSeek = SEEK('P*****'+&lcTempLine..pack_id+&lcTempLine..cPkColor+&lcTempLine..cPckSize+&lcTempLine..cPkVersion,'SPck_Hdr')
  ENDIF
  *C200433,1 HBG 17/11/2002 Enable price field in case of range pack & price per 
  *C200433,1                piece and selling price = 0 [Begin]
  *IF !SPck_Hdr.lPckPrPiec  
  *C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
  *IF !SPck_Hdr.lPckPrPiec AND !SPck_Hdr.lRange AND SPck_LIN.nPck_price <> 0 AND;
      SPck_Hdr.nPkSlPrice <> 0
  IF !m.lPckPrPiec AND !m.lRange AND SPck_LIN.nPck_price <> 0 AND;
      m.nPkSlPrice <> 0
  *C200479,1 [End]
  *C200433,1 [End]
    SHOW GET m.Gros_Price DISABLE
    SHOW GET m.Disc_Pcnt  DISABLE
    SHOW GET m.Price      DISABLE
  ELSE
    SHOW GET m.Gros_Price ENABLE
    SHOW GET m.Disc_Pcnt  ENABLE
    SHOW GET m.Price      ENABLE
  ENDIF
  *--control show in case of QTY changed 
  llSeek = SEEK('P'+laData[2]+&lcTempLine..pack_id+&lcTempLine..cPkColor+&lcTempLine..cPckSize+&lcTempLine..cPkVersion,'SPck_Hdr')  
  IF !llSeek 
    llSeek = SEEK('P*****'+&lcTempLine..pack_id+&lcTempLine..cPkColor+&lcTempLine..cPckSize+&lcTempLine..cPkVersion,'SPck_Hdr')  
  ENDIF
  *C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
  *IF !SPck_Hdr.lRange  
  IF !m.lRange  
  *C200479,1 [End]
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      SHOW GET m.Qty&lcCount  DISABLE
    ENDFOR
    SHOW GET m.TotQty         DISABLE
  ELSE
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      SHOW GET m.Qty&lcCount  ENABLE
    ENDFOR
    SHOW GET m.TotQty     ENABLE
  ENDIF

 ELSE
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      SHOW GET m.Qty&lcCount  ENABLE
    ENDFOR
    SHOW GET m.TotQty     ENABLE
    SHOW GET m.Gros_Price ENABLE
    SHOW GET m.Disc_Pcnt  ENABLE
    SHOW GET m.Price      ENABLE
 ENDIF
 *--mhm2000 if this style not belong to any Pack then let it works as it is
ENDIF

*!**************************************************************************
*! Name      : lfSOVldBar
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfVldBar()
*!**************************************************************************
*! Due to C102570,1
*!**************************************************************************
FUNCTION lfSOVldBar

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle
  *DEFINE BAR 17 OF _INQURYPOP PROMPT '\<Show Order Lines by Style' SKIP FOR (lnactfolder<>2)
  DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Show Order Lines by Pack' SKIP FOR (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline))

  *--C102570,1 [Start]
  *llByStyle = .F.
  REPLACE &lcFileNamA..llByStyle WITH .F.
  *--C102570,1 [End]
  
ELSE
  *DEFINE BAR 17 OF _INQURYPOP PROMPT '\<Show Order Lines by Pack' SKIP FOR (lnactfolder<>2)
  DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Show Order Lines by Style' SKIP FOR (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline))
  *--C102570,1 [Start]
  *llByStyle = .T.
  REPLACE &lcFileNamA..llByStyle WITH .T.
  *--C102570,1 [End]
ENDIF

=lfBrowDet(.T.,'O')
*!**************************************************************************
*! Name      : lfEDVldBar
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfEDVldBar()
*!**************************************************************************
*! Due to C102570,1
*!**************************************************************************
FUNCTION lfEDVldBar

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle =  &lcFileNamA..llByStyle 
*--C102570,1 [End]

IF llByStyle
  DEFINE BAR 17 OF _INQURYPOP PROMPT '\<Show Order Lines by Pack' SKIP FOR (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline))
  *--C102570,1 [Start]
  *llByStyle = .F.
  REPLACE &lcFileNamA..llByStyle WITH .F.
  *--C102570,1 [End]
  
ELSE
  DEFINE BAR 17 OF _INQURYPOP PROMPT '\<Show Order Lines by Style' SKIP FOR (lnactfolder<>2) OR (laScrMode[4] AND EOF(lcOrdline))
  *--C102570,1 [Start]
  *llByStyle = .T.
  REPLACE &lcFileNamA..llByStyle WITH .T.
  *--C102570,1 [End]
ENDIF

=lfBrowDet(.T.,'O')

*!**************************************************************************
*! Name      : lfInvRebal
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfInvRebal()
*!**************************************************************************
*! Due to C102574,1 , C102575,1
*!**************************************************************************
FUNCTION lfInvRebal

SET ORDER TO 'Itemhdr' IN 'lcTempHdr'
IF SEEK('LCRPITEM14', "lcTempHdr") AND lcTempHdr.cUpdVryIgn = 'U'

  =gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SPCK_HDRVR','SH')
  =gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_lINVR','SH')
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  =gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
  =gfOpenFile(gcDataDir+'CONSINVH',gcDataDir+'CONSINVH','SH')
  =gfOpenFile(gcDataDir+'CONSINVL',gcDataDir+'CONSINVL','SH')

  IF ATC('LCINVN',llExpr1) > 0
    lcInvTo  = SUBSTR(llExpr1,ATC('LCINVN',llExpr1)+17,6)
    lcInvFrm = SUBSTR(llExpr1,ATC('LCINVN',llExpr1)+8,6)
    lcGmExpr = 'BETWEEN(Invoice,lcInvFrm,lcInvTo)'
  ELSE
    lcGmExpr = '.T.'
  ENDIF  
  SELECT INVHDR
  SCAN FOR &lcGmExpr   
    WAIT WINDOW "Replacing Invoice : " + INVHDR.Invoice NOWAIT
    SELECT INVLINE
    lnShipAmnt = 0

    *B120210,1 MHM 12/28/2003 Define array to check same packs or not[Start]
    *lcPack_Id = ""
    DIMENSION  laPackId[1]
    *B120210,1 MHM 12/28/2003[End]

    IF SEEK(INVHDR.Invoice)
      SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.Invoice
        IF !EMPTY(Pack_id + cPkColor + cPckSize + cPkVersion) 
          llGetPack  = SEEK('P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR') OR;
                       SEEK('P*****'+Pack_id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')   
          llGetPack  = SEEK('P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN') OR;
                       SEEK('P*****'+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')                
          *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in INVLINE instead of SPCK_HDR [Begin]              
          *IF SPCK_HDR.lpckprpiec OR SPCK_HDR.lRange OR SPCK_HDR.nPkSlPrice = 0
          IF INVLINE.lpckprpiec OR INVLINE.lRange OR INVLINE.nPkSlPrice = 0
          *C200479,1 [End]
            lnShipAmnt = lnShipAmnt + (INVLINE.TotQty * INVLINE.Price) 
          ELSE

            *B120210,1 MHM 12/28/2003 change the way we check with[Start]
            *IF lcPack_id <> Pack_id + cPkColor + cPckSize + cPkVersion     
            IF ASCAN(laPackId,Pack_id + cPkColor + cPckSize + cPkVersion) =0
            *B120210,1 MHM 12/28/2003 change the way we check with[End]
            
              lnPackNo  = INVLINE.TotQty/SPCK_LIN.TotQty
              lnShipAmnt = lnShipAmnt + (lnPackNo * INVLINE.nPkSlPrice) 

              *B120210,1 MHM 12/28/2003 [Start]
              *lcPack_id  = Pack_id + cPkColor + cPckSize + cPkVersion     
			  lnPckInd = ALEN(laPackId,1)
	  		  DIMENSION laPackId[lnPckInd+1]
	  		  laPackId[lnPckInd+1]= Pack_id + cPkColor + cPckSize + cPkVersion
              *B120210,1 MHM 12/28/2003 [End]

            ENDIF  
          ENDIF  
        ELSE
          lnShipAmnt = lnShipAmnt + (TotQty * Price) 
        ENDIF  
      ENDSCAN
    ENDIF  
    SELECT INVHDR
    REPLACE INVHDR.ShipAmt  WITH lnShipAmnt,;
            INVHDR.TotalChg WITH lnShipAmnt + INVHDR.FREIGHT + INVHDR.INSUR + INVHDR.COD +;
                                 INVHDR.DISCOUNT +INVHDR.TAX_AMT + INVHDR.nPstAmt

  ENDSCAN

  SELECT CONSINVH
  lcInvoice = Invoice
  lcInvShip = 0
  lclastInv = Invoice
  SCAN FOR &lcGmExpr   
    lclastInv = Invoice
    IF lcInvoice <> Invoice
      IF SEEK(Invoice,'INVHDR')
        SELECT INVHDR
        REPLACE INVHDR.ShipAmt  WITH lcInvShip ,;
                INVHDR.TotalChg WITH lcInvShip + INVHDR.FREIGHT + INVHDR.INSUR + INVHDR.COD +;
                                     INVHDR.DISCOUNT +INVHDR.TAX_AMT + INVHDR.nPstAmt
      ENDIF
      lcInvShip = 0        
    ENDIF
    WAIT WINDOW "Replacing Consalidated Invoice : " + CONSINVH.Invoice NOWAIT
    SELECT CONSINVL
    lnShipAmnt = 0
    lcPack_Id = ""
    IF SEEK(CONSINVH.invoice+CONSINVH.store+CONSINVH.order)
      SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.Invoice
        IF !EMPTY(Pack_id + cPkColor + cPckSize + cPkVersion) 
          llGetPack  = SEEK('P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR') OR;
                       SEEK('P*****'+Pack_id+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')   
          llGetPack  = SEEK('P'+Account+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN') OR;
                       SEEK('P*****'+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')                
          *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in CONSINVL instead of SPCK_HDR [Begin]                                     
          *IF SPCK_HDR.lpckprpiec OR SPCK_HDR.lRange OR SPCK_HDR.nPkSlPrice = 0
          IF CONSINVL.lpckprpiec OR CONSINVL.lRange OR CONSINVL.nPkSlPrice = 0
          *C200479,1 [End]
            lnShipAmnt = lnShipAmnt + (CONSINVL.TotQty * CONSINVL.Price) 
          ELSE
            IF lcPack_id <> Pack_id + cPkColor + cPckSize + cPkVersion     
              lnPackNo  = CONSINVL.TotQty/SPCK_LIN.TotQty
              lnShipAmnt = lnShipAmnt + (lnPackNo * INVLINE.nPkSlPrice) 
              lcPack_id  = Pack_id + cPkColor + cPckSize + cPkVersion     
            ENDIF  
          ENDIF  
        ELSE
          lnShipAmnt = lnShipAmnt + (TotQty * Price) 
        ENDIF  
      ENDSCAN
    ENDIF  
    SELECT CONSINVH
    REPLACE CONSINVH.ShipAmt  WITH lnShipAmnt
    lcInvShip = lcInvShip + lnShipAmnt
  ENDSCAN
  IF SEEK(lclastInv,'INVHDR')
    SELECT INVHDR
    REPLACE INVHDR.ShipAmt  WITH lcInvShip ,;
            INVHDR.TotalChg WITH lcInvShip + INVHDR.FREIGHT + INVHDR.INSUR + INVHDR.COD +;
                                 INVHDR.DISCOUNT + INVHDR.TAX_AMT + INVHDR.nPstAmt
  ENDIF
ENDIF

*!**************************************************************************
*! Name      : lfUpdUPSD
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdUPSD()
*!**************************************************************************
*! Due to C200352,1
*!**************************************************************************
FUNCTION lfUpdUPSD

=gfOpenFile(gcDataDir+'SHDAEXPR',gcDataDir+'SHDAEXPR','SH')
=gfOpenFile(gcDataDir+'CONNECT',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CUSTDEPT',gcDataDir+'CUSTDEPT','SH')

SELECT INVHDR
IF SEEK(laData[1])
  lcShipVia = INVHDR.ShipVia
  =lfUpdUPS()
ENDIF

*!**************************************************************************
*! Name      : lfUpdUPSS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdUPSS()
*!**************************************************************************
*! Due to C200352,1
*!**************************************************************************
FUNCTION lfUpdUPSS

=gfOpenFile(gcDataDir+'SHDAEXPR',gcDataDir+'SHDAEXPR','SH')
=gfOpenFile(gcDataDir+'CONNECT',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CUSTDEPT',gcDataDir+'CUSTDEPT','SH')
=gfOpenFile(gcDataDir+'CONSINVH',gcDataDir+'CONSINVH','SH')

lcInv = ""
FOR lnInv = 1 TO ALEN(laInv)
  lcInv = lcInv + laInv[lnInv]+'|'
ENDFOR
SELECT INVHDR
=SEEK(laInv[1])

*B607397,1 SSE 07/06/2003 Change the scan to be without any condition. [Begin] 
*This loop will not take time because it will scan only the new invoices made.
*SCAN REST WHILE Invoice $ lcInv 

*! B121820,1 MHM 02/25/2004 Enhance Speed of Sales Order Invoice when saving Invoice[Start]
*SCAN REST WHILE .T.
*  IF !(Invoice $ lcInv)
*    LOOP
*  ENDIF
**B607397,1 SSE 07/06/2003 Change the scan to be without any condition. [End]
*
*  lcShipVia = INVHDR.ShipVia
*  =lfUpdUPS()
*ENDSCAN
FOR lnInv = 1 TO ALEN(laInv)
  IF SEEK(laInv[lnInv])
    SCAN REST WHILE Invoice =laInv[lnInv]
      lcShipVia = INVHDR.ShipVia
      =lfUpdUPS()
    ENDSCAN  
  ENDIF  
ENDFOR
*! B121820,1 MHM [End]

SELECT INVHDR
=SEEK(laInv[1])

*!**************************************************************************
*! Name      : lfUpdUPS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdUPS()
*!**************************************************************************
*! Due to C200352,1
*!**************************************************************************
FUNCTION lfUpdUPS

llUpdUPS = .F.
STORE "" TO lcUPSType , lcUPSBill , lcShpName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5, lcShpCunt ,;
            lcShpTel  , lcShpFax  , lcCstEmail, lcDeptName 

*-- Get Ship Via Information
IF SEEK('N'+lcShipVia+'Y'+'SHIPVIA','CODES')
  SELECT CODES
  lnI = 0
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+lcShipVia+'Y'+'SHIPVIA'
    lnI = lnI + 1
    DIMENSION laShpRlt[lnI,2]
    laShpRlt[lnI,1] = CODES.crltd_nam
    laShpRlt[lnI,2] = ALLTRIM(CODES.cRltd_vlu)
  ENDSCAN
  lnFound = ASCAN(laShpRlt,'CUPS')
  IF lnFound > 0
    lnFound  = ASUBSCRIPT(laShpRlt,lnFound,1)
    llUpdUPS = SUBSTR(laShpRlt[lnFound,2],1,5) = 'USUPS'
    IF llUpdUPS
     lcUPSType = laShpRlt[lnFound,2]
    ENDIF
  ENDIF
  IF llUpdUPS
    lnFound = ASCAN(laShpRlt,'CUPSBILL')
    IF lnFound > 0
      lnFound   = ASUBSCRIPT(laShpRlt,lnFound,1)
      lcUPSBill = laShpRlt[lnFound,2]
    ENDIF
  ENDIF
ENDIF

*-- Get Order Information
IF llUpdUPS AND SEEK('O'+INVHDR.Order,'ORDHDR')
  
ENDIF

*-- Get Customer Information  
IF llUpdUPS
  IF SEEK('O'+INVHDR.Order,'ORDHDR') AND ORDHDR.Alt_ShpTo 
    lcCustPO   = ORDHDR.CustPO
    lcShpName  = ORDHDR.STName
    lcShipAdd1 = ORDHDR.caddress1
    lcShipAdd2 = ORDHDR.caddress2
    lcShipAdd3 = ORDHDR.caddress3
    lcShipAdd4 = ORDHDR.caddress4
    lcShipAdd5 = ORDHDR.caddress5
    =SEEK('M'+INVHDR.Account,'CUSTOMER')
    lcShpCunt  = CUSTOMER.cCont_Code
    lcShpTel   = CUSTOMER.Phone1       
    lcShpFax   = CUSTOMER.Fax  
    lcCstEmail = CUSTOMER.cEmail      
  ELSE
    lcCustPO   = INVHDR.CustPO
    IF SEEK(IIF(EMPTY(INVHDR.Store),'M'+INVHDR.Account,'S'+INVHDR.Account+INVHDR.Store),'CUSTOMER')
      lcDistCntr = CUSTOMER.Dist_Ctr
      *C200518,1 HBG 12/03/2003 Get the Ship To address from Connect file [Begin]
      *IF !EMPTY(lcDistCntr)
      *=SEEK('S' + INVHDR.Account + lcDistCntr,'CUSTOMER')  
      *  lcShpName  = CUSTOMER.StName
      *  lcShpAdd1  = CUSTOMER.caddress12
      *  lcShpAdd2  = CUSTOMER.caddress22
      *  lcShpCunt  = CUSTOMER.cCont_Cod2
      *  lcShpCity  = CUSTOMER.caddress32
      *  lcShpStat  = CUSTOMER.caddress42
      *  lcShpZip   = CUSTOMER.caddress52
      *  lcShpTel   = CUSTOMER.Phone1       
      *  lcShpFax   = CUSTOMER.Fax  
      *  lcCstEmail = CUSTOMER.cEmail
      *ELSE
      *  lcShpName  = CUSTOMER.StName
      *  lcShpAdd1  = CUSTOMER.caddress1
      *  lcShpAdd2  = CUSTOMER.caddress2
      *  lcShpCunt  = CUSTOMER.cCont_Code
      *  lcShpCity  = CUSTOMER.caddress3
      *  lcShpStat  = CUSTOMER.caddress4
      *  lcShpZip   = CUSTOMER.caddress5
      *  lcShpTel   = CUSTOMER.Phone1  
      *  lcShpFax   = CUSTOMER.Fax  
      *  lcCstEmail = CUSTOMER.cEmail
      *ENDIF
      =SEEK('S' + INVHDR.Account + IIF(EMPTY(lcDistCntr),INVHDR.Store,lcDistCntr),'CUSTOMER')            
      IF SEEK(INVHDR.Account+IIF(EMPTY(lcDistCntr),INVHDR.Store,lcDistCntr),'CONNECT')
        lcShpName   = CONNECT.StName
        lcShipAdd1  = CONNECT.caddress1
        lcShipAdd2  = CONNECT.caddress2
        lcShipAdd3  = CONNECT.caddress3
        lcShipAdd4  = CONNECT.caddress4
        lcShipAdd5  = CONNECT.caddress5
        lcShpCunt  = CUSTOMER.cCont_Code
        lcShpTel   = CUSTOMER.Phone1       
        lcShpFax   = CUSTOMER.Fax  
        lcCstEmail = CUSTOMER.cEmail
      ELSE
        DECLARE laAddress[6,3]
        lcShpName  =  IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba)
        =gfGetAdr('CUSTOMER','','','',1,'')
        FOR lnCount = 1 TO ALEN(laAddress,1)
          lcCount = STR(lnCount,1)
          lcShipAdd&lcCount = laAddress[lnCount,2]
        ENDFOR
        lcShpCunt  = CUSTOMER.cCont_Code
        lcShpTel   = CUSTOMER.Phone1  
        lcShpFax   = CUSTOMER.Fax  
        lcCstEmail = CUSTOMER.cEmail
      ENDIF 
      *C200518,1 [End]
    ENDIF
  ENDIF  
  IF SEEK(INVHDR.Account+INVHDR.Dept,'CUSTDEPT')
    lcDeptName = CUSTDEPT.cdeptdesc
  ENDIF
ENDIF
  
*-- Update OnLine WorldShip Shipment Data Export file 'SHDAEXPR'
IF llUpdUPS 
  IF !SEEK(INVHDR.Invoice,'SHDAEXPR')
  
    *C200558,1 Modify some fields. [Begin]
    *INSERT INTO SHDAEXPR (cSiisVoid,cSiservtyp,cSibilopt,cStcustid,cStcmpnam,;
    *                      cStstrad,cStromflr,cStcontry,cstZipcod,cStcty,cStStat,cStteleph,;
    *                      cStFax,npkgweght,cpkpkgrf1,cpkpkgrf2,cpkpkgrf3,;
    *                      cpkpkgrf4,cpkpkgrf5,cpksnt1op,cpksnt1ty,cpksntfe,cstdepart);
    *             VALUES  ('N',lcUPSType,lcUPSBill,ALLTRIM(INVHDR.Account+INVHDR.STORE),lcShpName,;
    *                      lcShipAdd1,lcShipAdd2,lcShpCunt,lcShipAdd5,lcShipAdd3,lcShipAdd4,lcShpTel,;
    *                      lcShpFax,ALLTRIM(STR(CEILING(INVHDR.Weight/INVHDR.Cartons))),;
    *                      lcCustPO,INVHDR.Invoice,lcDeptName,'N/A','N/A','N','Email',;
    *                      lcCstEmail,lcDeptName)
    INSERT INTO SHDAEXPR (cSiisVoid,cSiservtyp,cSibilopt,cStcustid,cStcmpnam,;
                          cStstrad,cStromflr,cStcontry,cstZipcod,cStcty,cStStat,cStteleph,;
                          cStFax,npkgweght,cpkpkgrf1,cpkpkgrf2,cpkpkgrf3,;
                          cpkpkgrf4,cpkpkgrf5,cpksnt1op,cpksnt1ty,cpksntfe,cstdepart);
                 VALUES  ('N',lcUPSType,lcUPSBill,ALLTRIM(INVHDR.Account+INVHDR.STORE),lcShpName,;
                          lcShipAdd1,lcShipAdd2,lcShpCunt,lcShipAdd5,lcShipAdd3,lcShipAdd4,lcShpTel,;
                          lcShpFax,ALLTRIM(STR(CEILING(INVHDR.Weight/INVHDR.Cartons))),;
                          lcCustPO,INVHDR.Invoice,InvHdr.Dept,ALLTRIM(InvHdr.Invoice + ' / ' +InvHdr.Dept),'N/A','N','Email',;
                          lcCstEmail,'')
    *C200558,1 Modify some fields. [End]

  ELSE
    SELECT SHDAEXPR
    
    *C200558,1 Modify some fields. [Begin]
    *REPLACE cSiservtyp WITH lcUPSType,;
    *        cSibilopt  WITH lcUPSBill,;
    *        cStcustid  WITH ALLTRIM(INVHDR.Account+INVHDR.STORE),;
    *        cStcmpnam  WITH lcShpName,;
    *        cStstrad   WITH lcShipAdd1,;
    *        cStromflr  WITH lcShipAdd2,;
    *        cStcontry  WITH lcShpCunt,;
    *        cStcty     WITH lcShipAdd3,;
    *        cStStat    WITH lcShipAdd4,;
    *        cstZipcod  WITH lcShipAdd5,;
    *        cStteleph  WITH lcShpTel,;
    *        cStFax     WITH lcShpFax,;
    *        npkgweght  WITH ALLTRIM(STR(CEILING(INVHDR.Weight/INVHDR.Cartons))),;
    *        cpkpkgrf1  WITH lcCustPO,;
    *        cpkpkgrf3  WITH lcDeptName,;
    *        cpksntfe   WITH lcCstEmail,;
    *        cstdepart  WITH lcDeptName
    REPLACE cSiservtyp WITH lcUPSType,;
            cSibilopt  WITH lcUPSBill,;
            cStcustid  WITH ALLTRIM(INVHDR.Account+INVHDR.STORE),;
            cStcmpnam  WITH lcShpName,;
            cStstrad   WITH lcShipAdd1,;
            cStromflr  WITH lcShipAdd2,;
            cStcontry  WITH lcShpCunt,;
            cStcty     WITH lcShipAdd3,;
            cStStat    WITH lcShipAdd4,;
            cstZipcod  WITH lcShipAdd5,;
            cStteleph  WITH lcShpTel,;
            cStFax     WITH lcShpFax,;
            npkgweght  WITH ALLTRIM(STR(CEILING(INVHDR.Weight/INVHDR.Cartons))),;
            cpkpkgrf1  WITH lcCustPO,;
            cpkpkgrf3  WITH InvHdr.Dept,;
            cpkpkgrf4  WITH ALLTRIM(InvHdr.Invoice + ' / ' +InvHdr.Dept) ,;
            cpksntfe   WITH lcCstEmail,;
            cstdepart  WITH ''
    *C200558,1 Modify some fields. [End]

  ENDIF                          
ENDIF               

*!**************************************************************************
*! Name      : lfVldShip
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/05/2002
*! Purpose   : Get the Ship Via information of the new Ship via and update SHDAEXPR with it
*!**************************************************************************
*! Example   : = lfVldShip()
*!**************************************************************************
*! Due to C200352,1
*!**************************************************************************
FUNCTION lfVldShip

lcCurAlis = SELECT(0)

=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'SHDAEXPR',gcDataDir+'SHDAEXPR','SH')

IF SEEK(laData[1],'SHDAEXPR')

  llUpdUPS   = .F.
  STORE "" TO lcUPSType , lcUPSBill 
 
  lcPrevOrd = ORDER('CODES')
  SET ORDER TO CODES IN CODES
  IF SEEK('N'+laData[10]+'Y'+'SHIPVIA','CODES')
    SELECT CODES
    lnI = 0
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+laData[10]+'Y'+'SHIPVIA'
      lnI = lnI + 1
      DIMENSION laShpRlt[lnI,2]
      laShpRlt[lnI,1] = CODES.crltd_nam
      laShpRlt[lnI,2] = ALLTRIM(CODES.cRltd_vlu)
    ENDSCAN
    lnFound = ASCAN(laShpRlt,'CUPS')
    IF lnFound > 0
      lnFound  = ASUBSCRIPT(laShpRlt,lnFound,1)
      llUpdUPS = SUBSTR(laShpRlt[lnFound,2],1,5) = 'USUPS'
      IF llUpdUPS
       lcUPSType = laShpRlt[lnFound,2]
      ENDIF
    ENDIF
    IF llUpdUPS
      lnFound = ASCAN(laShpRlt,'CUPSBILL')
      IF lnFound > 0
        lnFound   = ASUBSCRIPT(laShpRlt,lnFound,1)
        lcUPSBill = laShpRlt[lnFound,2]
      ENDIF
    ENDIF
  ENDIF
  SET ORDER TO (lcPrevOrd) IN CODES
 
  SELECT SHDAEXPR 
  IF llUpdUPS  
    REPLACE cSiservtyp WITH lcUPSType,;
            cSibilopt  WITH lcUPSBill 
  ELSE
    DELETE
  ENDIF
ELSE
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  =gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
  =gfOpenFile(gcDataDir+'CUSTDEPT',gcDataDir+'CUSTDEPT','SH')
  lcShipVia = laData[10]
  =lfUpdUPS()
ENDIF
SELECT (lcCurAlis)

*!**************************************************************************
*! Name      : lfUpdCrtAv
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/05/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdCrtAv()
*!**************************************************************************
*! Due to C200352,1
*!**************************************************************************
FUNCTION lfUpdCrtAv

lcCurAlis = SELECT(0)
=lfVldShip()
lcDVarFle = lcInvHdr+'A'
GO TOP IN (lcDVarFle)

=gfOpenFile(gcDataDir+'SHDAEXPR',gcDataDir+'SHDAEXPR','SH')
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')

IF SEEK(laData[1],'SHDAEXPR') AND SEEK(laData[1],'INVHDR') AND;
   (laData[26] <> INVHDR.Weight OR laData[25] <> INVHDR.Cartons)
  SELECT SHDAEXPR 
  REPLACE npkgweght WITH ALLTRIM(STR(CEILING(laData[26]/laData[25])))
ENDIF

IF &lcDVarFle..llUpStrInf
  STORE "" TO lcShpName , lcShpAdd1 , lcShpAdd2 , lcShpCunt , lcShpCity ,;
              lcShpStat , lcShpZip  , lcShpTel  , lcShpFax  , lcCstEmail

  IF SEEK(IIF(EMPTY(laData[5]),'M'+laData[2],'S'+laData[2]+laData[5]),'CUSTOMER')
    lcDistCntr = CUSTOMER.Dist_Ctr
    IF !EMPTY(lcDistCntr)
      =SEEK('S' + laData[2]+ lcDistCntr,'CUSTOMER')
      lcShpName  = CUSTOMER.StName
      lcShpAdd1  = CUSTOMER.caddress12
      lcShpAdd2  = CUSTOMER.caddress22
      lcShpCunt  = CUSTOMER.cCont_Cod2
      lcShpCity  = CUSTOMER.caddress32
      lcShpStat  = CUSTOMER.caddress42
      lcShpZip   = CUSTOMER.caddress52
      lcShpTel   = CUSTOMER.Phone1       
      lcShpFax   = CUSTOMER.Fax  
      lcCstEmail = CUSTOMER.cEmail
    ELSE
      lcShpName  = CUSTOMER.StName
      lcShpAdd1  = CUSTOMER.caddress1
      lcShpAdd2  = CUSTOMER.caddress2
      lcShpCunt  = CUSTOMER.cCont_Code
      lcShpCity  = CUSTOMER.caddress3
      lcShpStat  = CUSTOMER.caddress4
      lcShpZip   = CUSTOMER.caddress5
      lcShpTel   = CUSTOMER.Phone1  
      lcShpFax   = CUSTOMER.Fax  
      lcCstEmail = CUSTOMER.cEmail
    ENDIF
  ENDIF
  
  IF SEEK(laData[1],'SHDAEXPR')
    SELECT SHDAEXPR 
    REPLACE cStcustid WITH ALLTRIM(INVHDR.Account+laData[5]),;
            cStcmpnam WITH lcShpName,;
            cStstrad  WITH lcShpAdd1,;
            cStromflr WITH lcShpAdd2,;
            cStcontry WITH lcShpCunt,;
            cstZipcod WITH lcShpZip,;
            cStcty    WITH lcShpCity,;
            cStStat   WITH lcShpStat,;
            cStteleph WITH lcShpTel,;
            cStFax    WITH lcShpFax,;
            cpksntfe  WITH lcCstEmail
  ENDIF
ENDIF
SELECT (lcCurAlis)

*!**************************************************************************
*! Name      : lfVoidUps
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/05/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfVoidUps()
*!**************************************************************************
*! Due to C200352,1
*!**************************************************************************
FUNCTION lfVoidUps

lcCurAlis = SELECT(0)
=gfOpenFile(gcDataDir+'SHDAEXPR',gcDataDir+'SHDAEXPR','SH')
IF SEEK(laData[1],'SHDAEXPR')
  SELECT SHDAEXPR 
  REPLACE cSiisVoid WITH 'Y'
  DELETE
ENDIF
SELECT (lcCurAlis)
*!**************************************************************************
*! Name      : lfSOSumBar
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfSOSumBar()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfSOSumBar
*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]
IF llSumLin
  *B125563,1 NNA 02/07/2005 (BEGIN) Add new condition to skip the next bar if (Show Summary) has selected
  *DEFINE BAR 19 OF _INQURYPOP PROMPT '\<Summarize By Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]
  DEFINE BAR 19 OF _INQURYPOP PROMPT '\<Summarize by Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2] ;
         OR &lcFileNamA..lnBarNO=11
  *B125563,1 NNA (END)

  llSumLin = .F.
  *B125563,1 NNA 02/07/2005 (BEGIN) if (Summarize by Pack) has selected then replace lnBarNO with
  *B125563,1 NNA            (11) to skip of Bar (Show Summary)
  REPLACE &lcFileNamA..lnBarNO WITH 11
  *B125563,1 NNA (END)

ELSE
  DEFINE BAR 19 OF _INQURYPOP PROMPT '\<Show pack details' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]
  llSumLin = .T.
  *B125563,1 NNA 02/07/2005 (BEGIN) IF Bar(Summarize by Pack) changed to (Show pack details) and
  *B125563,1 NNA            user checked this bar(Show pack details) then Enable both (Show Summary)
  *B125563,1 NNA            and (Summarize by Pack)
  REPLACE &lcFileNamA..lnBarNO WITH 0
  *B125563,1 NNA (End)

ENDIF
*--mhm 102570
REPLACE &lcFileNamA..llSumLin WITH llSumLin 

=lfBrowDet(.T.,'O')
*!**************************************************************************
*! Name      : lfEDSumBar
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfEDSumBar()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfEDSumBar

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llSumLin
  DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Summarize By Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]
  llSumLin = .F.
ELSE
  DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Show pack details' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]
  llSumLin = .T.
ENDIF

REPLACE &lcFileNamA..llSumLin WITH llSumLin

=lfBrowDet(.T.,'O')

*!**************************************************************************
*! Name      : lfSOSumBar
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfSOSumBar()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfSOColSum
PRIVATE lnCurAlias,lnCurRec
*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
lcSumPck = &lcFileNamA..lcSumPck
*--C102570,1 [End]

*-create temp file for Pack and collect data into this temp file
lnCurAlias = SELECT(0)
CREATE DBF (gcWorkDir+lcSumPck) ;
        (cOrdType C(1),Order C(6), Account C(5),Pack_Id C(19),cPkColor C(6),cPckSize C(3),cPkVersion C(4),;
        Store C(8),PackQty N(6),nPkSlPrice N(8,2),LineNo N(3,0))
         
INDEX ON cOrdType+Order+Account+Pack_Id+cpkcolor+cpcksize+cPkVersion+STORE TAG (lcSumPck)
SELECT OrdLine
lnCurRec = RECNO()
lcLocPack = ""
lcLocSty  = ""
lcLocStore=""
m.PackQty = 0
m.nPkSlPrice = 0


lnPackQty = 0

SCAN REST WHILE cOrdType+Order+STR(lineno,6)= lcOrdType+laData[1]
  SCATT MEMVAR MEMO
  *--mhm2000
  =SEEK('P'+Account+Pack_Id+cpkcolor+cpcksize+cPkVersion+Style,'SPCK_LIN') OR ;
   SEEK('P'+"*****"+Pack_Id+cpkcolor+cpcksize+cPkVersion+Style,'SPCK_LIN')
  
  =SEEK('P'+Account+Pack_Id+cpkcolor+cpcksize+cPkVersion,'SPCK_HDR') OR ;
   SEEK('P'+"*****"+Pack_Id+cpkcolor+cpcksize+cPkVersion,'SPCK_HDR') 

 
  IF !EMPTY(m.Pack_Id)
    IF !SEEK(cOrdType+Order+Account+PADR(Pack_Id,19)+cpkcolor+cpcksize+cPkVersion+STORE,lcSumPck)
      m.PackQty    = nPackNo
      m.nPkSlPrice = npkslprice
      INSERT INTO (lcSumPck) FROM MEMVAR
    ENDIF
  ELSE
    m.PACK_ID = m.STYLE
    m.PackQty = TOTQTY
    m.nPkSlPrice = Price
    INSERT INTO (lcSumPck) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT (lcSumPck)
LOCATE
SELECT OrdLine
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF

SELECT (lnCurAlias)               
*!**************************************************************************
*! Name      : lfBrwsVar
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfBrwsVar()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar

DO CASE
  *-- Sort by line# - Total quantity
  CASE lnSortBy = 1 .AND. !llQtyPSize
    IF !llColorSeg
      =lfBrwsVar1()
    ELSE
      =lfBrwsVar2()
    ENDIF
  CASE lnSortBy = 1 .AND. llQtyPSize
    =lfBrwsVar3()  
  CASE lnSortBy = 2 .AND. !llQtyPSize .AND. llDetails
    =lfBrwsVar4()   
  CASE lnSortBy = 2 .AND. !llQtyPSize .AND. !llDetails
    =lfBrwsVar5()            
  CASE lnSortBy = 2 .AND. llQtyPSize .AND. llDetails
    =lfBrwsVar6()                
  CASE lnSortBy = 2 .AND. llQtyPSize .AND. !llDetails
    =lfBrwsVar7()                    
  CASE lnSortBy = 3 .AND. !llQtyPSize .AND. llDetails    
    =lfBrwsVar8()                      
  CASE lnSortBy = 3 .AND. !llQtyPSize .AND. !llDetails    
    =lfBrwsVar9()                        
  CASE lnSortBy = 3 .AND. llQtyPSize .AND. llDetails
    =lfBrwVar10()                          
  CASE lnSortBy = 3 .AND. llQtyPSize .AND. !llDetails
    =lfBrwVar11()                              
ENDCASE    

*!**************************************************************************
*! Name      : lfBrwsVar1
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfBrwsVar1()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar1

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R:P='999999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',] +;
                IIF(ORDHDR.cOrdType='T' .AND. ORDHDR.MON_FLG='G' ,;
                "Status=lfEDIStat() :H='Status' :R :18",;
                [PIKTKT :H='Piktkt' :R ,] +;
                [TOTPIK :H='Picked' :R ,] +;
                [PIKDATE :H='Date' :R])
    IF laScrMode[3] or laScrMode[4]                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF      
  ENDIF            
ELSE 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R:P='999999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',] +;
                IIF(ORDHDR.cOrdType='T' .AND. ORDHDR.MON_FLG='G' ,;
                "Status=lfEDIStat() :H='Status' :R :18",;
                [PIKTKT :H='Piktkt' :R ,] +;
                [TOTPIK :H='Picked' :R ,] +;
                [PIKDATE :H='Date' :R])
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                
    ENDIF  
  ENDIF
ENDIF            
*!**************************************************************************
*! Name      :  lfBrwsVar2
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar2()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar2

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R:P='999999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',] +;
                IIF(ORDHDR.cOrdType='T' .AND. ORDHDR.MON_FLG='G' ,;
                "Status=lfEDIStat() :H='Status' :R :18",;
                [PIKTKT :H='Piktkt' :R ,] +;
                [TOTPIK :H='Picked' :R ,] +;
                [PIKDATE :H='Date' :R,]+[lcColorDesc = gfCodDes(SUBSTR(STYLE,lnMajorLen+2) , 'COLOR') :R :H=lcNonMajTl+' Description'])
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STYLE :R :H=lcStyHdr,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(llMultiSt,[STORE :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo  :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [GROUP :H='Group' :R,Gros_Price :H='Gross Price' :R,] +;
                [Disc_Pcnt :H='Discount' :R,PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R:P='999999999.99' :R,] +;
                [TOTQTY :H= 'Qty.' :P='999999':R ,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',] +;
                IIF(ORDHDR.cOrdType='T' .AND. ORDHDR.MON_FLG='G' ,;
                "Status=lfEDIStat() :H='Status' :R :18",;
                [PIKTKT :H='Piktkt' :R ,] +;
                [TOTPIK :H='Picked' :R ,] +;
                [PIKDATE :H='Date' :R,]+[lcColorDesc = gfCodDes(SUBSTR(STYLE,lnMajorLen+2) , 'COLOR') :R :H=lcNonMajTl+' Description'])
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwsVar3
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar3()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar3

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +IIF(llMultiSt,[STORE :8 :R,],'') +;
                IIF(laData[7],IIF(lcScrMode='V',[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [TOTQTY :P='999999' :R, lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',TOTPIK :H='Tot.Pik.' :R ,]+;
                [Qty1 :P='99999':R,Qty2 :P='99999':R,Qty3 :P='99999':R,Qty4 :P='99999':R,]+;
                [Qty5 :P='99999':R,Qty6 :P='99999':R,Qty7 :P='99999':R,Qty8 :P='99999':R,]+;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']+;
                IIF(laSetups[2,2]='Y',",n=laData[27] :H='Rep 1':R ,COMM1 :H='Comm 1' :R,n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R",'')+;
                IIF(laData[7],",CUSTPO :H='Cust. PO#':R",'')
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +IIF(llMultiSt,[STORE :8 :R,],'') +;
                IIF(laData[7],IIF(lcScrMode='V',[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [TOTQTY :P='999999' :R, lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',TOTPIK :H='Tot.Pik.' :R ,]+;
                [Qty1 :P='99999':R,Qty2 :P='99999':R,Qty3 :P='99999':R,Qty4 :P='99999':R,]+;
                [Qty5 :P='99999':R,Qty6 :P='99999':R,Qty7 :P='99999':R,Qty8 :P='99999':R,]+;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']+;
                IIF(laSetups[2,2]='Y',",n=laData[27] :H='Rep 1':R ,COMM1 :H='Comm 1' :R,n1=laData[29] :H='Rep 2':R ,COMM2 :H='Comm 2' :R",'')+;
                IIF(laData[7],",CUSTPO :H='Cust. PO#':R",'')
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwsVar4
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar4()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar4

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R,]+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'),TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',]+;
                [TOTPIK :H='Tot.Pik.' :R]
                
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STYLE :H=lcStyHdr :R,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R,]+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'),TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',]+;
                [TOTPIK :H='Tot.Pik.' :R]

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwsVar5
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar5()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar5

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STYLE  :H=lcStyHdr :R ,] +;
                [Style.Desc :H= 'Description', nTOTQTY :H='Quantity' :R:P='9999999999',]+;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):14:R:P='99999999999.99',nTOTPIK :H='Tot.Pik.']
                
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STYLE  :H=lcStyHdr :R ,] +;
                [Style.Desc :H= 'Description', nTOTQTY :H='Quantity' :R:P='9999999999',]+;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):14:R:P='99999999999.99',nTOTPIK :H='Tot.Pik.']

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwsVar6
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar6()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar6

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]


IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R ,] +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R,TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',] +;
                [QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',] +;
                [QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',] +;
                [QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',] +;
                [QTY8 :H='Size8' :R:P='99999',] +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']
                
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                [STORE :H='Store' :R ,] +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):R,TOTQTY :H='Quantity' :R,] +;
                [lnAmount=PRICE*TOTQTY :H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='9999999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',] +;
                [QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',] +;
                [QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',] +;
                [QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',] +;
                [QTY8 :H='Size8' :R:P='99999',] +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK :H='Tot.Pik.']

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwsVar7
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar7()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar7

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STYLE :H=lcStyHdr :R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999',] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
                'Amount'):14:R:P='99999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :R]
                
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STYLE :H=lcStyHdr :R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999',] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
                'Amount'):14:R:P='99999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :R]

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwsVar8
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar8()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar8

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :H='Store' :R,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R ,TOTQTY :H='Quantity' :10:R,] +;
                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',TOTPIK :H='Tot.Pik']                

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STORE :H='Store' :R,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'') +;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(),]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R ,TOTQTY :H='Quantity' :10:R,] +;
                [lnAmount=PRICE*TOTQTY :10:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',TOTPIK :H='Tot.Pik']

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwsVar9
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwsVar9()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwsVar9
*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :H='Store' :R ,] +;
                [Customer.StName :H='Name' :30 ,] +;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999'  ,] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
                'Amount'):14:R:P='99999999999.99',nTotPIK :H = 'Picked']
                
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STORE :H='Store' :R ,] +;
                [Customer.StName :H='Name' :30 ,] +;
                [nTOTQTY :H='Quantity' :10:R:P='9999999999'  ,] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Wholesale Price',;
                'Amount'):14:R:P='99999999999.99',nTotPIK :H = 'Picked']

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwVar10
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwVar10()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwVar10

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'')+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(), ]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R,TOTQTY :H='TotQty' :P='999999':R,]+;
                [lnAmount=PRICE*TOTQTY :14:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',QTY8 :H='Size8' :R:P='99999',]  +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK:H='Tot.Pik.']
                
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STORE :H='Store' :R ,STYLE :H=lcStyHdr :R ,]+IIF(laSetups[14,2]='Y',[Dyelot :R ,],'')+;
                IIF(laData[7],IIF(laScrMode[2],[CustPo :15 :R,],[CustPo :15 :W=lfwCustPo() :V=lfvLCustPo(), ]),'') +;
                [PRICE :H=IIF(ORDHDR.cOrdType='T','Transmitted Price','Price'):9:R,TOTQTY :H='TotQty' :P='999999':R,]+;
                [lnAmount=PRICE*TOTQTY :14:H=IIF(ORDHDR.cOrdType='T','Wholesale Price','Amount'):R:P='99999999999.99',]+;
                [QTY1 :H='Size1' :R:P='99999',QTY2 :H='Size2' :R:P='99999',] +;
                [QTY3 :H='Size3' :R:P='99999',QTY4 :H='Size4' :R:P='99999',] +;
                [QTY5 :H='Size5' :R:P='99999',QTY6 :H='Size6' :R:P='99999',] +;
                [QTY7 :H='Size7' :R:P='99999',QTY8 :H='Size8' :R:P='99999',]  +;
                [PIK1 :P='99999':R,PIK2 :P='99999':R,PIK3 :P='99999':R,PIK4 :P='99999':R,]+;
                [PIK5 :P='99999':R,PIK6 :P='99999':R,PIK7 :P='99999':R,PIK8 :P='99999':R,TOTPIK:H='Tot.Pik.']

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*!**************************************************************************
*! Name      :  lfBrwVar11
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfBrwVar11()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfBrwVar11

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llByStyle = &lcFileNamA..llByStyle 
llSumLin  = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF llByStyle 
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :H='Store' :8:R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Transmitted Price',;
                'Price') :14:R:P='99999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :P='99999':R]
                
    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO PACKS IN (lcOrdLine)                
    ENDIF  
  ENDIF             
ELSE
  IF llSumLin        
    lcOrderBr = [lcPackExp=PACK_ID+' -'+CPKCOLOR+' -'+gfDoTriger('ARDINV','GETGMSZ')+' -'+CPKVERSION:H='Pack - Color - Size - Version' :R ,] +;
                [STORE :R,] +;
                [nPkSlPrice :H='Pack Price' :R,] +;
                [PackQty :H='Pack Qty' :P='999999' :R]
  ELSE
    lcOrderBr = [STORE :H='Store' :8:R ,] +;
                [nQTY1 :H='Qty1' :6:R:P='999999',]+;
                [nQTY2 :H='Qty2' :6:R:P='999999',]+;
                [nQTY3 :H='Qty3' :6:R:P='999999',]+;
                [nQTY4 :H='Qty4' :6:R:P='999999',]+;
                [nQTY5 :H='Qty5' :6:R:P='999999',]+;
                [nQTY6 :H='Qty6' :6:R:P='999999',]+;
                [nQTY7 :H='Qty7' :6:R:P='999999',]+;
                [nQTY8 :H='Qty8' :6:R:P='999999',]+;
                [nTOTQTY :H='Tot. Qty.' :10:R:P='9999999999' ,] +;
                [nAMOUNT :H=IIF(ORDHDR.cOrdType='T','Transmitted Price',;
                'Price') :14:R:P='99999999999.99',] +;
                [nPIK1 :H='Pik1' :P='99999':R,]+;
                [nPIK2 :H='Pik2' :P='99999':R,]+;
                [nPIK3 :H='Pik3' :P='99999':R,]+;
                [nPIK4 :H='Pik4' :P='99999':R,]+;
                [nPIK5 :H='Pik5' :P='99999':R,]+;
                [nPIK6 :H='Pik6' :P='99999':R,]+;
                [nPIK7 :H='Pik7' :P='99999':R,]+;
                [nPIK8 :H='Pik8' :P='99999':R,]+;
                [nTotPik :H='Tot.Pik' :P='99999':R]

    IF laScrMode[3] or laScrMode[4]                                
      SET ORDER TO ORDLINE IN (lcOrdLine)                                
    ENDIF  
  ENDIF            
ENDIF

*--
*!**************************************************************************
*! Name      : lfSOEDTSUM
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfSOEDTSUM()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfSOEDTSUM

=lfEDTSUM()

IF laScrMode[4] 
  DEFINE BAR 19 OF _INQURYPOP PROMPT '\<Summarize By Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]
  *--C102570,1 [Start]
  lcFileNamA = lcOrdLine+"A"
  REPLACE &lcFileNamA..llSumLin WITH .F.
  *--C102570,1 [End]
EndIF

*!**************************************************************************
*! Name      : lfEDEDTSUM
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfEDEDTSUM()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfEDEDTSUM

=lfEDTSUM()

IF laScrMode[4] 
  DEFINE BAR 18 OF _INQURYPOP PROMPT '\<Summarize By Pack' SKIP FOR (lnactfolder<>2) OR !laScrMode[2]
  *--C102570,1 [Start]
  lcFileNamA = lcOrdLine+"A"
  REPLACE &lcFileNamA..llSumLin WITH llSumLin
  *--C102570,1 [End]
EndIF

*!**************************************************************************
*! Name      : lfEDTSUM
*! Developer : Mohamed SHokry (MHM)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : =  lfEDTSUM()
*!**************************************************************************
*! Due to C102570,1 for new modification send from GMA
*!**************************************************************************
FUNCTION lfEDTSUM

*--C102570,1 [Start]
lcFileNamA = lcOrdLine+"A"
llSumLin = &lcFileNamA..llSumLin 
*--C102570,1 [End]

IF laScrMode[3] AND llSumLin
  *    *-- Message < This Order has a Pre_Billed invoice, Cannot edit. >
  *    *-- Buttons <                         OK                        >
  =gfModalGen('INM00000B00000','DIALOG','','', 'Choose detail option, Cannot edit.')
  STORE .F. TO laScrMode
  laScrMode[2]=.T.
  lcGmaAl =Alias()
  lnGmaRec=RECNO('ORDHDR')
  SELECT ORDHDR
  =SEEK(lcOrdType+laData[1])
  =gfObj_Lock(.F.)
  IF BETWEEN (lnGmaRec,1,RECCOUNT('ORDHDR'))
    GOTO lnGmaRec IN ('ORDHDR')
  ENDIF
  SELECT (lcGmaAl)        
  SHOW GETS
  RETURN
ENDIF
*!**************************************************************************
*! Name      : lfInvColSm
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfInvColSm()
*!**************************************************************************
*! Due to C102574 for new modification send from GMA
*!**************************************************************************
FUNCTION lfInvColSm
PRIVATE lnCurAlias,lnCurRec

=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LINVR','SH')

*-create temp file for Pack and collect data into this temp file
lnCurAlias = SELECT(0)
CREATE DBF (gcWorkDir+lcSumPck) ;
        (Invoice C(6),Account C(5),Pack_Id C(19),cPkColor C(6),cPckSize C(3),cPkVersion C(4),;
        Store C(8),PackQty N(6),nPkSlPrice N(8,2),lRange L(1),Style C(19),Qty1 N(6),;
        Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),Qty8 N(6),TotQty N(7),;
        Scale C(3),Gros_price N(12,2),Price N(12,2),Type C(1))
        
INDEX ON Invoice+account+pack_id+cpkcolor+cpcksize+cpkversion TAG (lcSumPck)

SELECT INVLINE

lnCurRec = RECNO()
lcPack_ID = ""
m.PackQty = 0
m.nPkSlPrice = 0
lcKeyInv     = ""
lcKeyAcc     = ""
lcKeypack    = ""
lcKeyRest    = ""

=SEEK(laData[1])
SCAN REST WHILE invoice+STR(lineno,6) = laData[1]
  SCATTER MEMVAR MEMO
  llGetPack = SEEK('P'+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion,'SPCK_HDR')
  IF !llGetPack 
    = SEEK('P*****'+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion,'SPCK_HDR')
  ENDIF
  llGetPack =SEEK('P'+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion+m.Style,'SPCK_LIN')
  IF !llGetPack 
    =SEEK('P*****'+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion+m.Style,'SPCK_LIN')
  ENDIF
  IF !EMPTY(m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion)
    m.Style    = ""
    m.PackQty   = m.nPackNo

    STORE m.nPkSlPrice TO m.Gros_price , m.Price
    m.lRange   = .F.
    m.Type     = 'P'
    IF SEEK(m.Invoice+m.Account+PADR(m.Pack_Id,19)+m.cpkcolor+m.cpcksize+m.cPkVersion,lcSumPck)
      SELECT (lcSumPck)
      REPLACE &lcSumPck..Qty1    WITH &lcSumPck..Qty1    + m.Qty1,;
              &lcSumPck..Qty2    WITH &lcSumPck..Qty2    + m.Qty2,;
              &lcSumPck..Qty3    WITH &lcSumPck..Qty3    + m.Qty3,;
              &lcSumPck..Qty4    WITH &lcSumPck..Qty4    + m.Qty4,;
              &lcSumPck..Qty5    WITH &lcSumPck..Qty5    + m.Qty5,;
              &lcSumPck..Qty6    WITH &lcSumPck..Qty6    + m.Qty6,;
              &lcSumPck..Qty7    WITH &lcSumPck..Qty7    + m.Qty7,;
              &lcSumPck..Qty8    WITH &lcSumPck..Qty8    + m.Qty8,;
              &lcSumPck..TotQty  WITH &lcSumPck..TotQty  + m.TotQty
    ELSE
      INSERT INTO (lcSumPck) FROM MEMVAR
    ENDIF
  ELSE
    m.PACK_ID    = m.STYLE
    m.PackQty    = m.TOTQTY
    m.nPkSlPrice = m.Price
    m.Type       = 'S'
    STORE m.Price TO m.nPkSlPrice , m.Gros_price , m.Price
    m.lRange     = .T.
    INSERT INTO (lcSumPck) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT (lcSumPck)
LOCATE

SELECT INVLINE
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF
SELECT (lnCurAlias)               

*!**************************************************************************
*! Name      : lfInvClSmS
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/25/2002
*! Purpose   : Validate the Bar in Option menu
*!**************************************************************************
*! Example   : = lfInvClSmS()
*!**************************************************************************
*! Due to C102575 for new modification send from GMA
*!**************************************************************************
FUNCTION lfInvClSmS
PRIVATE lnCurAlias,lnCurRec

=gfOpenFile(gcDataDir+'SPck_Hdr',gcDataDir+'SPCK_HDRVR','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LINVR','SH')

*-create temp file for Pack and collect data into this temp file
lnCurAlias = SELECT(0)
CREATE DBF (gcWorkDir+lcSumPck) ;
        (Account C(5),Pack_Id C(19),cPkColor C(6),cPckSize C(3),cPkVersion C(4),;
        Store C(8),PackQty N(6),nPkSlPrice N(8,2),lRange L(1),Consol C(1),cDivision C(6),;
        Style C(19),Qty1 N(6),Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
        Qty8 N(6),TotQty N(7),Scale C(3),Gros_price N(12,2),Price N(12,2),;
        Book1 N(6),Book2 N(6),Book3 N(6),Book4 N(6),Book5 N(6),Book6 N(6),Book7 N(6),;
        Book8 N(6),TotBook N(7),Type C(1))

INDEX ON account+pack_id+cpkcolor+cpcksize+cpkversion TAG (lcSumPck)

SELECT (lcInvLine)
lnCurRec = RECNO()
lcPack_ID = ""
m.PackQty = 0
m.nPkSlPrice = 0
lcKeyAcc     = ""
lcKeypack    = ""
lcKeyRest    = ""

SCAN 
  SCATTER MEMVAR MEMO
  
  llGetPack = SEEK('P'+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion,'SPCK_HDR')
  IF !llGetPack 
    = SEEK('P*****'+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion,'SPCK_HDR')
  ENDIF
  llGetPack =SEEK('P'+m.Account+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion+m.Style,'SPCK_LIN')
  IF !llGetPack 
    =SEEK('P*****'+m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion+m.Style,'SPCK_LIN')
  ENDIF
  IF !EMPTY(m.Pack_Id+m.cpkcolor+m.cpcksize+m.cPkVersion)
    m.Style    = ""
    
    m.PackQty  = m.nPackNo


    m.Type     = 'P'
    STORE m.npkslprice TO m.Gros_price , m.Price
    m.lRange    = .F.
    IF SEEK(m.Account+PADR(m.Pack_Id,19)+m.cpkcolor+m.cpcksize+m.cPkVersion,lcSumPck)
      SELECT (lcSumPck)
      REPLACE &lcSumPck..Qty1    WITH &lcSumPck..Qty1    + m.Qty1,;
              &lcSumPck..Qty2    WITH &lcSumPck..Qty2    + m.Qty2,;
              &lcSumPck..Qty3    WITH &lcSumPck..Qty3    + m.Qty3,;
              &lcSumPck..Qty4    WITH &lcSumPck..Qty4    + m.Qty4,;
              &lcSumPck..Qty5    WITH &lcSumPck..Qty5    + m.Qty5,;
              &lcSumPck..Qty6    WITH &lcSumPck..Qty6    + m.Qty6,;
              &lcSumPck..Qty7    WITH &lcSumPck..Qty7    + m.Qty7,;
              &lcSumPck..Qty8    WITH &lcSumPck..Qty8    + m.Qty8,;
              &lcSumPck..TotQty  WITH &lcSumPck..TotQty  + m.TotQty

      REPLACE &lcSumPck..Book1   WITH &lcSumPck..Book1   + m.Book1,;
              &lcSumPck..Book2   WITH &lcSumPck..Book2   + m.Book2,;
              &lcSumPck..Book3   WITH &lcSumPck..Book3   + m.Book3,;
              &lcSumPck..Book4   WITH &lcSumPck..Book4   + m.Book4,;
              &lcSumPck..Book5   WITH &lcSumPck..Book5   + m.Book5,;
              &lcSumPck..Book6   WITH &lcSumPck..Book6   + m.Book6,;
              &lcSumPck..Book7   WITH &lcSumPck..Book7   + m.Book7,;
              &lcSumPck..Book8   WITH &lcSumPck..Book8   + m.Book8,;
              &lcSumPck..TotBook WITH &lcSumPck..TotBook + m.TotBook

    ELSE
      INSERT INTO (lcSumPck) FROM MEMVAR
    ENDIF
  ELSE
    m.PACK_ID    = m.STYLE
    m.PackQty    = m.TOTQTY
    m.Type       = 'S'
    STORE m.Price TO m.nPkSlPrice , m.Gros_price , m.Price
    m.lRange     = .T.
    INSERT INTO (lcSumPck) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT (lcSumPck)
LOCATE

SELECT (lcInvLine)
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF
SELECT (lnCurAlias)               

*!**************************************************************************
*! Name      : lfBrwFlds
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfBrwFlds()
*!**************************************************************************
*! Due to C102570,1
*!**************************************************************************
FUNCTION lfBrwFlds

lcFields = lcFields + ",lcPack=ALLTRIM(Pack_ID)+;
                     '-'+ALLTRIM(cpkColor)+'-'+gfDoTriger('SOORD','GETGMSZ')+'-'+;
                      ALLTRIM(cpkversion) :30:H='Pack-Color-Size-Version':R"
                      
*!**************************************************************************
*! Name      : lfUpdMnOrd
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdMnOrd()
*!**************************************************************************
*! Due to C102570,1
*!**************************************************************************
FUNCTION lfUpdMnOrd

lcCurAlis = ALIAS()
SELECT (lcOrdLine)
*C200410,1 HBG 23/09/2002 Update the new field npackNo [Begin]
*REPLACE &lcOrdLine..Pack_Id    WITH m.Pack_Id,;
        &lcOrdLine..cPkColor   WITH m.cPkColor,;
        &lcOrdLine..cPckSize   WITH m.cPckSize,;
        &lcOrdLine..cPkVersion WITH m.cPkVersion,;
        &lcOrdLine..npkslprice WITH m.npkslprice
*C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]        
*REPLACE &lcOrdLine..Pack_Id    WITH m.Pack_Id,;
        &lcOrdLine..cPkColor   WITH m.cPkColor,;
        &lcOrdLine..cPckSize   WITH m.cPckSize,;
        &lcOrdLine..cPkVersion WITH m.cPkVersion,;
        &lcOrdLine..npkslprice WITH m.npkslprice,;
        &lcOrdLine..nPackNo    WITH m.nPackNo    
REPLACE &lcOrdLine..Pack_Id    WITH m.Pack_Id,;
        &lcOrdLine..cPkColor   WITH m.cPkColor,;
        &lcOrdLine..cPckSize   WITH m.cPckSize,;
        &lcOrdLine..cPkVersion WITH m.cPkVersion,;
        &lcOrdLine..npkslprice WITH m.npkslprice,;
        &lcOrdLine..nPackNo    WITH m.nPackNo,;
        &lcOrdLine..lRange     WITH m.lRange,;
        &lcOrdLine..lPckPrPiec WITH m.lPckPrPiec 
*C200479,1 [End]
*C200410,1 [End]
                
SELECT (lcCurAlis)                     

*!**************************************************************************
*! Name      : lfDisaPck
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfDisaPck()
*!**************************************************************************
*! Due to C102575,1
*!**************************************************************************
FUNCTION lfDisaPck

SHOW GET m.Pack_Id DISABLE

*!**************************************************************************
*! Name      : lfUpOrdSh
*! Developer : Hend Ghanem (HBG)
*! Date      : 04/15/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpOrdSh()
*!**************************************************************************
*! Due to C102574,1
*!**************************************************************************
FUNCTION lfUpOrdSh

REPLACE SHIPAMT    WITH SHIPAMT + &lcInvHFile..SHIPAMT ,;
        OPENAMT    WITH OPENAMT - (&lcInvHFile..SHIPAMT + lnDifValue)
IF SEEK(&lcInvHFile..Invoice,'INVLINE')
  SELECT INVLINE
  lnAmnt = 0
  SCAN REST WHILE Invoice + STR(lineno,6) = &lcInvHFile..Invoice 
    lnAmnt = lnAmnt  + (TOTQTY * Price)
  ENDSCAN
  SELECT ORDHDR
  REPLACE ORDHDR.SHIPAMT WITH ORDHDR.SHIPAMT - lnAmnt,;
          ORDHDR.OPENAMT WITH ORDHDR.OPENAMT + lnAmnt  
ENDIF

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : HEnd Ghanem (HBG)
*! Date      : 12/10/2001
*! Purpose   : Function To Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
*!102574,102575

FUNCTION lfGetGmSz
PARAMETER lcPackSize

IF !EMPTY(lcPackSize) AND SEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcSiz = RIGHT(lcPackSize,1)
  lcSize = SCALE.Sz&lcSiz
ELSE
  lcSize = '*****'    
ENDIF  

RETURN lcSize 

*!**************************************************************************
*! Name      : lfALBrwOrd
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/01/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfALBrwOrd()
*!**************************************************************************
*! Due to C200333,1
*!**************************************************************************
FUNCTION lfALBrwOrd

lcOrderBr    = [cMarker= IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+; 
               [cPicked=IIF(EVAL(lc_TmpOrdL + '.Picked') .OR. EOF(lc_TmpOrdL) .AND. ORDLINE.Picked,]+;
               ['>>','  '):R:W=.F.:H='  ':2,]+;
               [STORE:H='Store':8,]+;
               [ pack_id =pack_id+'-'+cPkColor+'-'+gfDoTriger('ARDINV','GETGMSZ')+'-'+cPkVersion:H = 'Pack_Id-Color-Size-Version' :35:R,]+;
               [STYLE=IIF(EOF(lc_TmpOrdL), Style, &lc_TmpOrdL..Style):H=lcStyHdr:R,]+;
               [GROUP:H='G':P='!':1:R,]+; 
                IIF(llMultWare,;
               [cWareCode=IIF(EOF(lc_TmpOrdL), cWareCode, &lc_TmpOrdL..cWareCode):H='Pick from':10:R,],[])+;
                IIF(llUseDyes,;
               [DYELOT=IIF(EOF(lc_TmpOrdL), Dyelot, &lc_TmpOrdL..Dyelot):H='Dyelot':10:R,],[])

*!**************************************************************************
*! Name      : lfChkRgPck
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/01/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfChkRgPck()
*!**************************************************************************
*! Due to C200404,1
*!**************************************************************************
FUNCTION lfChkRgPck

lcFileNamA = lcOrdLine+"A"
REPLACE &lcFileNamA..llRngPack WITH .F.
REPLACE &lcFileNamA..llFromBulk WITH .F.

lcPrvAls = ALIAS()
lcPrvOrd = ORDER('SPCK_HDR')
SET ORDER TO SPCK_HDRVr IN SPCK_HDR
SELECT (lcOrdLine)
lnRecNo = RECNO()
SCAN
  *C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
  *IF !EMPTY(Pack_ID)
  IF !EMPTY(Pack_ID) AND (lRange OR !EMPTY(cFromOrder))
    IF lRange 
      REPLACE &lcFileNamA..llRngPack WITH .T.
    ENDIF  
    IF !EMPTY(cFromOrder)
      REPLACE &lcFileNamA..llFromBulk WITH .T.
    ENDIF
    EXIT
    *IF SEEK('P'+&lcOrdLine..cPckAcc+&lcOrdLine..Pack_Id+;
    *        &lcOrdLine..cpkcolor+&lcOrdLine..cpcksize+&lcOrdLine..cPkVersion,'SPCK_HDR') AND;
    *   !SPCK_HDR.lRange 
    *  REPLACE &lcFileNamA..llRngPack WITH .T.
    *  EXIT
    *ENDIF
  *C200479,1 [End]
  ENDIF
ENDSCAN
SELECT (lcOrdLine)
IF BETWEEN(lnRecNo, 1, RECCOUNT())
  GO lnRecNo
ENDIF
   
SET ORDER TO &lcPrvOrd IN SPCK_HDR
SELECT (lcPrvAls)              
        
*!**************************************************************************
*! Name      : lfGetData
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/01/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetData()
*!**************************************************************************
*! Due to C200404,1
*!**************************************************************************
FUNCTION lfGetData

lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack
*E302037,1 HBG 21/10/2002 Add field to allow changing the pack price [Begin]
*CREATE DBF (gcWorkDir+lcModPack) ;
        (Type C(1),Account C(5),Store C(8),Pack_Id C(16),cPkVersion C(4),nPkSlPrice N(8,2),;
         PackQty N(6),cPkColor C(6),cPckSize C(3),UPC C(16),SKU C(16),PckNum N(6),;
         NewPkNum N(6),Price N(12,2))
*C200425,1 HBG 07/11/2002 Add fields to allow modifing the pack [Begin]
*CREATE DBF (gcWorkDir+lcModPack) ;
        (Type C(1),Account C(5),Store C(8),Pack_Id C(16),cPkVersion C(4),nPkSlPrice N(8,2),;
         PackQty N(6),cPkColor C(6),cPckSize C(3),UPC C(16),SKU C(16),PckNum N(6),;
         NewPkNum N(6),NewPrice N(8,2))
*C200479,1 HBG 30/12/2002 Add lPCkPrPiec field [Begin] 
*CREATE DBF (gcWorkDir+lcModPack) ;
        (Type C(1),Account C(5),MStore M(10),Pack_Id C(16),cPkVersion C(4),nPkSlPrice N(8,2),;
         PackQty N(6),cPkColor C(6),cPckSize C(3),UPC C(16),SKU C(16),PckNum N(6),;
         NewPkNum N(6),NewPrice N(8,2),NewPack C(16),NewColor C(6),NewSize C(3),NewVer C(4),MNewStr M(10),NewAcc C(5))
CREATE DBF (gcWorkDir+lcModPack) ;
        (Type C(1),Account C(5),MStore M(10),Pack_Id C(16),cPkVersion C(4),nPkSlPrice N(8,2),;
         PackQty N(6),cPkColor C(6),cPckSize C(3),UPC C(16),SKU C(16),PckNum N(6),;
         NewPkNum N(6),NewPrice N(8,2),NewPack C(16),NewColor C(6),NewSize C(3),NewVer C(4),;
         MNewStr M(10),NewAcc C(5),lPrPerPiec L(1))
*C200479,1 [End]
*C200425,1 [End]
*E302037,1 [End]
*C200425,1 HBG 07/11/2002 remove store filed from the index [Begin]
*INDEX ON Account+Store+Pack_Id+cPkColor+cPckSize+cPkVersion TAG (lcModPack)
INDEX ON Account+Pack_Id+cPkColor+cPckSize+cPkVersion TAG (lcModPack)
*C200425,1 [End]

llFound = .F.
lcPrvAls = ALIAS()
lcPrvOrd = ORDER('SPCK_HDR')
SET ORDER TO SPCK_HDRVr IN SPCK_HDR

SELECT (lcOrdLine)
SCAN
  IF !EMPTY(Pack_ID)
    *C200479,1 HBG 30/12/2002 Use lRange & lPCkPrPiec in ORDLINE instead of SPCK_HDR [Begin]                                     
    *IF SEEK('P'+&lcOrdLine..cPckAcc+&lcOrdLine..Pack_Id+;
            &lcOrdLine..cpkcolor+&lcOrdLine..cpcksize+&lcOrdLine..cPkVersion,'SPCK_HDR') AND;
       !SPCK_HDR.lRange AND EMPTY(&lcOrdLine..cFromOrder)
    IF SEEK('P'+&lcOrdLine..cPckAcc+&lcOrdLine..Pack_Id+;
            &lcOrdLine..cpkcolor+&lcOrdLine..cpcksize+&lcOrdLine..cPkVersion,'SPCK_HDR') AND;
       !(&lcOrdLine..lRange) AND EMPTY(&lcOrdLine..cFromOrder)
    *C200479,1 [End]
      llFound = .T.
      IF !SEEK(&lcOrdLine..cPckAcc+&lcOrdLine..Pack_Id+&lcOrdLine..cpkcolor+;
               &lcOrdLine..cpcksize+&lcOrdLine..cPkVersion,lcModPack) 
        *B606913,1 HBG 01/27/2003 Update # of Packs using nPackNo field [Begin]
        *INSERT INTO (lcModPack) (Type,Account,MStore,Pack_Id,cPkVersion,nPkSlPrice,;
                                 PackQty,cPkColor,cPckSize);
                         VALUES ('P',&lcOrdLine..cPckAcc,&lcOrdLine..Store,;
                                 &lcOrdLine..Pack_Id,&lcOrdLine..cPkVersion,&lcOrdLine..nPkSlPrice,;
                                 &lcOrdLine..TotQty,&lcOrdLine..cpkcolor,&lcOrdLine..cpcksize)               
        *C200479,1 HBG 30/12/2002 Add lPCkPrPiec field and update it from ORDLINE [Begin] 
        *INSERT INTO (lcModPack) (Type,Account,MStore,Pack_Id,cPkVersion,nPkSlPrice,;
                                 cPkColor,cPckSize,PckNum);
                         VALUES ('P',&lcOrdLine..cPckAcc,&lcOrdLine..Store,;
                                 &lcOrdLine..Pack_Id,&lcOrdLine..cPkVersion,&lcOrdLine..nPkSlPrice,;
                                 &lcOrdLine..cpkcolor,&lcOrdLine..cpcksize,&lcOrdLine..nPackNo)               
        INSERT INTO (lcModPack) (Type,Account,MStore,Pack_Id,cPkVersion,nPkSlPrice,;
                                 cPkColor,cPckSize,PckNum,lPrPerPiec);
                         VALUES ('P',&lcOrdLine..cPckAcc,&lcOrdLine..Store,;
                                 &lcOrdLine..Pack_Id,&lcOrdLine..cPkVersion,&lcOrdLine..nPkSlPrice,;
                                 &lcOrdLine..cpkcolor,&lcOrdLine..cpcksize,&lcOrdLine..nPackNo,&lcOrdLine..lPckPrPiec)               
        *C200479,1 [End]                                                                
        *B606913,1 [End]
      ELSE
        SELECT (lcModPack)
        *B606913,1 HBG 01/27/2003 Update # of Packs using nPackNo field [Begin]
        *REPLACE &lcModPack..PackQty WITH &lcModPack..PackQty + &lcOrdLine..TotQty
        *B606913,1 [End]
        *C200425,1 HBG 07/11/2002 Collect the stores for this pack in memo filed [Begin]
        IF ATC( &lcOrdLine..Store,MStore) = 0
          REPLACE &lcModPack..MStore  WITH MStore +'|'+ &lcOrdLine..Store
          *B606913,1 HBG 01/27/2003 Update # of Packs using nPackNo field [Begin]
          REPLACE &lcModPack..PckNum WITH &lcModPack..PckNum + &lcOrdLine..nPackNo
          *B606913,1 [End]
        ENDIF  
        *C200425,1 [End]
      ENDIF     
    ENDIF
  ENDIF
ENDSCAN
IF llFound 
  =lfGetPkInf()
  *C200425,1 HBG 07/11/2002 Fill the popup to allow modifing the pack [Begin]
  SELECT (lcModPack)
  LOCATE
  SET ORDER TO SPCK_HDRVR IN SPCK_HDR
  DIME laAcc[1],laPack[1],laColor[1],laSize[1,2],laVer[1]
  IF SEEK('P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
           &lcModPack..cPkVersion,'SPCK_HDR')
    laAcc[1] = &lcModPack..Account
    IF &lcModPack..Account = '*****'
      IF SEEK('P'+laData[2]+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
              &lcModPack..cPkVersion,'SPCK_HDR')
        DIME laAcc[2]
        laAcc[2] = laData[2] 
      ENDIF
    ELSE
      IF SEEK('P*****'+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
              &lcModPack..cPkVersion,'SPCK_HDR')
        DIME laAcc[2]              
        laAcc[2] = '*****'
      ENDIF
    ENDIF
  ENDIF
  
  SET ORDER TO Spck_hdrst IN SPCK_HDR
  IF SEEK(&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'STYLEUPC') OR;
     SEEK('S'+&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'SPCK_HDR')
    IF SEEK('S'+&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'SPCK_HDR')
      DIME laAcc[1]
      laAcc[1] = &lcModPack..Account    
      lnAcc = 1
    ENDIF          
    SET ORDER TO SPCK_HDRVR IN SPCK_HDR
    IF SEEK('P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+;
          &lcModPack..cPckSize,'SPCK_HDR')
      laPack[1]   = SPCK_HDR.Pack_ID               
      laColor[1]  = SPCK_HDR.cPkColor
      laSize[1,1] = lfGetGmSz(SPCK_HDR.cPckSize)
      laSize[1,2] = SPCK_HDR.cPckSize    
      STORE 1 TO lnPack,lnColor,lnSize
      lnI = 1
      DIME laVer[1]
      laVer[lnI]     = SPCK_HDR.cPkVersion
      SELECT SPCK_HDR
      SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                      'P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+;
                      &lcModPack..cPckSize
        IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
          lnI = ALEN(laVer,1)+1
          DIME laVer[lnI]
          laVer[lnI]     = SPCK_HDR.cPkVersion
        ENDIF
      ENDSCAN
      lnFound = ASCAN(laVer,IIF(EMPTY(&lcModPack..NewVer),&lcModPack..cPkVersion,&lcModPack..NewVer))
      IF lnFound > 0
        lnFound = ASUBSCRIPT(laVer,lnFound,1)
        STORE lnFound TO lnVer
      ENDIF
    ENDIF
  ELSE
    SET ORDER TO SPCK_HDRVR IN SPCK_HDR
    IF SEEK('P'+&lcModPack..Account,'SPCK_HDR')
      lnI = 1
      DIME laPack[1],laColor[1],laSize[1,2],laVer[1]
      laPack[lnI]    = SPCK_HDR.Pack_ID
      laColor[lnI]   = SPCK_HDR.cPkColor
      laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
      laSize[lnI,2]  = SPCK_HDR.cPckSize    
      laVer[lnI]     = SPCK_HDR.cPkVersion
      SELECT SPCK_HDR
      SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion = 'P'+&lcModPack..Account
        IF ASCAN(laPack,SPCK_HDR.Pack_ID) = 0
          lnI = ALEN(laPack,1)+1
          DIME laPack[lnI]
          laPack[lnI]    = SPCK_HDR.Pack_ID
        ENDIF  
        IF ASCAN(laColor,SPCK_HDR.cPkColor) = 0
          lnI = ALEN(laColor,1)+1
          DIME laColor[lnI]
          laColor[lnI]   = SPCK_HDR.cPkColor
        ENDIF
        IF ASCAN(laSize,SPCK_HDR.cPckSize) = 0
          lnI = ALEN(laSize,1)+1
          DIME laSize[lnI,2]
          laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
          laSize[lnI,2]  = SPCK_HDR.cPckSize
        ENDIF  
        IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
          lnI = ALEN(laVer,1)+1
          DIME laVer[lnI]
          laVer[lnI]     = SPCK_HDR.cPkVersion
        ENDIF
      ENDSCAN
      lnFound = ASCAN(laPack,&lcModPack..Pack_ID)
      IF lnFound > 0
        lnFound = ASUBSCRIPT(laPack,lnFound,1)
        STORE lnFound TO lnPack
      ENDIF
      lnFound = ASCAN(laColor,&lcModPack..cPkColor)
      IF lnFound > 0
        lnFound = ASUBSCRIPT(laColor,lnFound,1)
        STORE lnFound TO lnColor
      ENDIF
      lnFound = ASCAN(laSize,&lcModPack..cPckSize)
      IF lnFound > 0
        lnFound = ASUBSCRIPT(laSize,lnFound,1)
        STORE lnFound TO lnSize
      ENDIF
      lnFound = ASCAN(laVer,&lcModPack..cPkVersion)
      IF lnFound > 0
        lnFound = ASUBSCRIPT(laVer,lnFound,1)
        STORE lnFound TO lnVer
      ENDIF
    ENDIF
    SELECT (lcModPack)
  ENDIF  
  *C200425,1 [End]
ENDIF

SET ORDER TO &lcPrvOrd IN SPCK_HDR
SELECT (lcPrvAls)              

*!**************************************************************************
*! Name      : lfGetPkInf
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/01/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfGetPkInf()
*!**************************************************************************
*! Due to C200404,1
*!**************************************************************************
FUNCTION lfGetPkInf

=gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'Gmapkupc','SH') 
lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack
SELECT (lcModPack)
SCAN
  *B606913,1 HBG 01/27/2003 Update # of Packs using nPackNo field [Begin]
  *SET ORDER TO SPCK_HDRVr IN SPCK_HDR
  *IF SEEK('P'+&lcModPack..Account+PADR(&lcModPack..Pack_Id,16)+&lcModPack..cpkcolor+;
  *        &lcModPack..cpcksize+&lcModPack..cPkVersion,'SPCK_HDR')
  *  REPLACE &lcModPack..PckNum WITH (&lcModPack..PackQty / SPCK_HDR.nPckQty)
  *ENDIF
  *B606913,1 [End]
  
  SET ORDER TO Spck_hdrst IN SPCK_HDR
  IF SEEK('S'+&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'SPCK_HDR')
    REPLACE &lcModPack..SKU WITH SPCK_HDR.Pack_ID
  ENDIF
  
  SET ORDER TO Gmapkupc IN STYLEUPC
  IF SEEK(&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'STYLEUPC')
    REPLACE &lcModPack..UPC WITH (STYLEUPC.cupcnum1 + STYLEUPC.cupcnum2 + STYLEUPC.cupcnum3)
  ENDIF
ENDSCAN
     
*!**************************************************************************
*! Name      : lfMODFYVLD     
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/01/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfMODFYVLD()
*!**************************************************************************
*! Due to C200404,1
*!**************************************************************************
FUNCTION lfMODFYVLD     

lnBrTrNo = 0
llUpdpack = .F.
*C200425,1 HBG 07/11/2002 Allow changing the pack [Begin]
DECLARE laAcc[1],laPack[1],laColor[1],laSize[1,2],laVer[1]
STORE 1 TO lnAcc,lnPack,lnColor,lnSize,lnVer
STORE 0 TO lnNewQty,lnNewPrc
*C200425,1 [End]
=lfGetData()
lcFileNamA = lcOrdLine+"A"
lclstBrw = lcBrTtl1 
lcBrTtl1   = "Orderd Packs"
lcModPack  = &lcFileNamA..lcModPack

SELECT (lcModPack)
lcEscInt = ON('KEY','ESC')
lcTabInt = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcAltB   = ON('KEY','ALT+B') 
*C200425,1 HBG 07/11/2002 Allow changing the pack [Begin]
lcObjStat = IIF(lcProgName = 'SOEDORD','DISABLE','ENABLE')

DIMENSION laStores[1] , laNewStr[1]   
STORE "" TO laStores , laNewStr
=gfSubStr(&lcModPack..MStore,@laStores,"|")

IF !llMultiSt AND !EMPTY(laStores[1])
  =ACOPY(laStores,laNewStr)
  SELECT (lcModPack)
  SCAN
    IF !EMPTY(laNewStr[1])
      REPLACE &lcModPack..MNewStr WITH &lcModPack..MNewStr + laNewStr[1]
    ENDIF    
  ENDSCAN
ENDIF

*C200425,1 [End]
DO (gcScrDir+"SO\SOUPPCK.SPX")

lcBrTtl1 = lclstBrw
ON KEY LABEL ESC      &lcEscInt
ON KEY LABEL TAB      &lcTabInt
ON KEY LABEL BACKTAB  &lcHldBtb
ON KEY LABEL ALT+B    &lcAltB

*!**************************************************************************
*! Name      : lfCanUpd
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/01/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfCanUpd()
*!**************************************************************************
*! Due to C200404,1
*!**************************************************************************
FUNCTION lfCanUpd

CLEAR READ
SELECT (lcOrdLine)
LOCATE

*!**************************************************************************
*! Name      : lfUpdPck
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/01/2002
*! Purpose   : 
*!**************************************************************************
*! Example   : = lfUpdPck()
*!**************************************************************************
*! Due to C200404,1
*!**************************************************************************
FUNCTION lfUpdPck


lcTmPrvOrd = ORDER(lcOrdLine)
SET ORDER TO AccPacks IN (lcOrdLine)
SET ORDER TO Spck_linst IN SPCK_LIN
llUpdate = .F.
lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack
SELECT (lcModPack)
*E302037,1 HBG 21/10/2002 Allow changing the pack price [Begin]
*SCAN FOR NewPkNumm <> 0  
*C200425,1 HBG 07/11/2002 Allow changing the pack [Begin]
*SCAN FOR NewPkNum <> 0  OR NewPrice <> 0
LOCATE
SCAN FOR NewPkNum <> 0  OR NewPrice <> 0 OR;
        (!EMPTY(NewAcc) OR !EMPTY(NewPack) OR !EMPTY(NewColor) OR !EMPTY(NewSize) OR !EMPTY(NewVer))
*C200425,1 [End]
  IF NewPkNum <> 0
*E302037,1 [End]
    =lfUpdQty()
*E302037,1 HBG 21/10/2002 Allow changing the pack price [Begin]
  ENDIF
  IF NewPrice <> 0
    =lfUpdPrc()
  ENDIF
*E302037,1 [End]
  *C200425,1 HBG 07/11/2002 Allow changing the pack [Begin]
  IF !EMPTY(NewAcc) OR !EMPTY(NewPack) OR !EMPTY(NewColor) OR !EMPTY(NewSize) OR !EMPTY(NewVer)
    llUpdate = .T.
    =lfChgPck()
  ENDIF
  *C200425,1 [End]
  SELECT (lcModPack)           
ENDSCAN

IF llUpdate
  CLEAR READ
  SELECT (lcOrdLine)
  STORE 0 TO laData[35],laData[36],laData[41],laData[42]
  SCAN 
    laData[35] = laData[35] + &lcOrdLine..TotQty
    laData[36] = laData[36] + &lcOrdLine..TotQty * &lcOrdLine..Price
    laData[41] = laData[41] + &lcOrdLine..TotQty
    laData[42] = laData[42] + &lcOrdLine..TotQty * &lcOrdLine..Price
  ENDSCAN
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOK    WITH laData[35] ,;
          BOOKAMT WITH laData[36] ,;
          OPEN    WITH laData[41] ,;
          OPENAMT WITH laData[42]
  
  SELECT (lcOrdLine)
  LOCATE
  SET ORDER TO (lcTmPrvOrd) IN (lcOrdLine)
  =lfBrowDet(.T.,IIF(lnOrdTrans=1 .OR. llZoom,'O','B'))
ELSE
  *E302037,1 HBG 21/10/2002 If pack price dosen't change and pack Qty , cann't update[Begin]
  *=gfModalGen("INM00000B00000","Dialog",.F.,.F.,"Non of Pack's numbers has changed. Cann't update.")
  =gfModalGen("INM00000B00000","Dialog",.F.,.F.,"Non of Pack's numbers or price has changed. Cann't update.")
  *E302037,1 [End]
  SET ORDER TO (lcTmPrvOrd) IN (lcOrdLine)
ENDIF

*!*************************************************************
*! Name      : lfLCPkTrp         (C200404,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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
*! Example   : =lfLCPkTrp()
*!*************************************************************

FUNCTION lfLCPkTrp

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfTrBkTab                      (C200404,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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

FUNCTION lfTrPkTab

ON KEY LABEL TAB
ACTIVATE WINDOW SOUPPK1
_CUROBJ = OBJNUM(lnNewQty)

*!*************************************************************
*! Name      : lfTrPkBack  (C200404,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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
*! Example   : =lfTrPkBack()
*!*************************************************************

FUNCTION lfTrPkBack
*ON KEY LABEL BACKTAB
*IF WONTOP() = (lcBrowTitl)
  _CUROBJ = OBJNUM(pbCan)
*ENDIF

*!*************************************************************
*! Name      : lfLCPckTrp     (C200404,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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
*! Example   : =lfLCPckTrp()
*!*************************************************************

FUNCTION lfLCPckTrp

*-- This function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key
IF WONTOP()  = (lcBrTtl1)
  ON KEY LABEL TAB     DO lfTrPkTab
  ON KEY LABEL BACKTAB DO lfTrPkBack
  ON KEY LABEL ESC     DO lfTrPkEsc
ENDIF
RETURN .F.


*!*************************************************************
*! Name      : lfTrPkEsc               (C200404,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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
*! Example   : =lfTrPkEsc()
*!*************************************************************

FUNCTION lfTrPkEsc

ACTIVATE WINDOW SOUPPK1
_CUROBJ = OBJNUM(pbCls)
KEYBOARD "{ENTER}" CLEAR


*!*************************************************************
*! Name      : lfLcPckBrw                (C200404,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : Browse the Shipment records
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfLcPckBrw()
*!*************************************************************

FUNCTION lfLcPckBrw

lnCurAlias = SELECT(0)
lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack

SELECT (lcModPack)
LOCATE

*E302037,1 HBG 21/10/2002 Add new price column to allow changing the pack price [Begin]
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnBrTrNo,'>',' '):H=' ':R:1:W=.F.,;
       Account    :R :H= 'Account'  :8 ,;
       Pack_id    :R :H= 'Pack ID'  :16,;
       cPkColor   :R :H= 'Color'    :8 ,;
       lcSize =gfDoTriger('ARDINV','GETGMSZ'):R :H= 'Size':7,;
       cPkVersion :R :H= 'Version'  :8 ,;
       PckNum     :R :H= 'Qty'      :6 ,;
       Store      :R :H= 'Store'    :12,;
       NewPkNum   :H= 'New Pack Qty':15,;
       nPkSlPrice :R :H= 'Price'    :6 ,;
       UPC        :R :H= 'UPC'      :20,;
       SKU        :R :H= 'SKU'      :20 ;
       WINDOW 'SOUPPK0'   ;
       IN WINDOW 'SOUPPCK';
       WHEN lfwPkBrow()   ;
       LOCK 0             ;
       NOAPPEND           ;
       NOCLEAR            ;
       NODELETE           ;
       NOMENU             ;   
       SAVE               ;
       TITLE (lcBrTtl1);
       NOWAIT
*C200425,1 HBG 07/11/2002 remove the store field from the browse [Begin]       
*BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnBrTrNo,'>',' '):H=' ':R:1:W=.F.,;
       Account    :R :H= 'Account'  :8 ,;
       Pack_id    :R :H= 'Pack ID'  :16,;
       cPkColor   :R :H= 'Color'    :8 ,;
       lcSize =gfDoTriger('ARDINV','GETGMSZ'):R :H= 'Size':7,;
       cPkVersion :R :H= 'Version'  :8 ,;
       PckNum     :R :H= 'Qty'      :6 ,;
       Store      :R :H= 'Store'    :12,;
       NewPkNum   :H= 'New Qty'     :10,;
       nPkSlPrice :R :H= 'Price'    :6 ,;
       NewPrice   :H= 'New Price'   :10,;
       UPC        :R :H= 'UPC'      :20,;
       SKU        :R :H= 'SKU'      :20 ;
       WINDOW 'SOUPPK0'   ;
       IN WINDOW 'SOUPPCK';
       WHEN lfwPkBrow()   ;
       LOCK 0             ;
       NOAPPEND           ;
       NOCLEAR            ;
       NODELETE           ;
       NOMENU             ;   
       SAVE               ;
       TITLE (lcBrTtl1)   ;
       NOWAIT
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnBrTrNo,'>',' '):H=' ':R:1:W=.F.,;
       Account    :R :H= 'Account'  :8 ,;
       Pack_id    :R :H= 'Pack ID'  :16,;
       cPkColor   :R :H= 'Color'    :8 ,;
       lcSize =gfDoTriger('ARDINV','GETGMSZ'):R :H= 'Size':7,;
       cPkVersion :R :H= 'Version'  :8 ,;
       PckNum     :R :H= 'Qty'      :6 ,;
       nPkSlPrice :R :H= 'Price'    :6 ,;
       UPC        :R :H= 'UPC'      :20,;
       SKU        :R :H= 'SKU'      :20 ;
       WINDOW 'SOUPPK0'   ;
       IN WINDOW 'SOUPPCK';
       WHEN lfwPkBrow()   ;
       LOCK 0             ;
       NOAPPEND           ;
       NOCLEAR            ;
       NODELETE           ;
       NOMENU             ;   
       SAVE               ;
       TITLE (lcBrTtl1)   ;
       NOWAIT
*C200425,1 [End]                 
*E302037,1 [End]
SELECT (lnCurAlias)               

*!*************************************************************
*! Name      : lfwPkBrow             (C200404,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : When Function for browse 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwPkBrow()
*!*************************************************************
*!
FUNCTION lfwPkBrow
lnCurAlias = SELECT(0)
lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack

SELECT (lcModPack)
lnBrTrNo = RECNO()
*C200425,1 HBG 07/11/2002 Fill the popup to allow modifing the pack [Begin]
lnNewQty = &lcModPack..NewPkNum
lnNewPrc = &lcModPack..NewPrice
SET ORDER TO SPCK_HDRVR IN SPCK_HDR
*C200425,1 [End]

DIME laAcc[1],laPack[1],laColor[1],laSize[1,2],laVer[1]
IF SEEK('P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
         &lcModPack..cPkVersion,'SPCK_HDR')
  DIME laAcc[1]         
  laAcc[1] = &lcModPack..Account
  IF &lcModPack..Account = '*****'
    IF SEEK('P'+laData[2]+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
            &lcModPack..cPkVersion,'SPCK_HDR')
      DIME laAcc[2]            
      laAcc[2] = laData[2] 
    ENDIF
  ELSE
    IF SEEK('P*****'+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
            &lcModPack..cPkVersion,'SPCK_HDR')
      DIME laAcc[2]
      laAcc[2] = '*****'
    ENDIF
  ENDIF
  lnFound = ASCAN(laAcc,IIF(EMPTY(&lcModPack..NewAcc),&lcModPack..Account,&lcModPack..NewAcc))
  IF lnFound > 0
    lnFound = ASUBSCRIPT(laAcc,lnFound,1)
    STORE lnFound TO lnAcc
  ENDIF
ENDIF
SET ORDER TO Spck_hdrst IN SPCK_HDR
IF SEEK(&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
        &lcModPack..cpcksize,'STYLEUPC') OR;
   SEEK('S'+&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
        &lcModPack..cpcksize,'SPCK_HDR')

  IF SEEK('S'+&lcModPack..Account+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
        &lcModPack..cpcksize,'SPCK_HDR')
    DIME laAcc[1]
    laAcc[1] = &lcModPack..Account
    lnAcc = 1    
  ENDIF          
        
  SET ORDER TO SPCK_HDRVR IN SPCK_HDR
  IF SEEK('P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+;
        &lcModPack..cPckSize,'SPCK_HDR')
    laPack[1]   = SPCK_HDR.Pack_ID               
    laColor[1]  = SPCK_HDR.cPkColor
    laSize[1,1] = lfGetGmSz(SPCK_HDR.cPckSize)
    laSize[1,2] = SPCK_HDR.cPckSize    
    STORE 1 TO lnPack,lnColor,lnSize
    lnI = 1
    DIME laVer[1]
    laVer[lnI]     = SPCK_HDR.cPkVersion
    SELECT SPCK_HDR
    SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                    'P'+&lcModPack..Account++&lcModPack..Pack_id+&lcModPack..cPkColor+;
                    &lcModPack..cPckSize
      IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
        lnI = ALEN(laVer,1)+1
        DIME laVer[lnI]
        laVer[lnI]     = SPCK_HDR.cPkVersion
      ENDIF
    ENDSCAN
    lnFound = ASCAN(laVer,IIF(EMPTY(&lcModPack..NewVer),&lcModPack..cPkVersion,&lcModPack..NewVer))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laVer,lnFound,1)
      STORE lnFound TO lnVer
    ENDIF
  ENDIF
ELSE
  SET ORDER TO SPCK_HDRVR IN SPCK_HDR
  IF SEEK('P'+&lcModPack..Account,'SPCK_HDR')
    lnI = 1
    DIME laPack[1]
    laPack[lnI]    = SPCK_HDR.Pack_ID
    SELECT SPCK_HDR
    SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion = 'P'+&lcModPack..Account
      IF ASCAN(laPack,SPCK_HDR.Pack_ID) = 0
        lnI = ALEN(laPack,1)+1
        DIME laPack[lnI]
        laPack[lnI]    = SPCK_HDR.Pack_ID
      ENDIF  
    ENDSCAN
    lnFound = ASCAN(laPack,IIF(EMPTY(&lcModPack..NewPack),&lcModPack..Pack_ID,&lcModPack..NewPack))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laPack,lnFound,1)
      STORE lnFound TO lnPack
    ENDIF
    IF SEEK('P'+&lcModPack..Account+PADR(laPack[lnPack],16),'SPCK_HDR')
      lnI = 1
      DIME laColor[1],laSize[1,2],laVer[1]
      laColor[lnI]   = SPCK_HDR.cPkColor
      SELECT SPCK_HDR
      SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                      'P'+&lcModPack..Account+PADR(laPack[lnPack],16)
        IF ASCAN(laColor,SPCK_HDR.cPkColor) = 0
          lnI = ALEN(laColor,1)+1
          DIME laColor[lnI]
          laColor[lnI]   = SPCK_HDR.cPkColor
        ENDIF
      ENDSCAN
    ENDIF  
    lnFound = ASCAN(laColor,IIF(EMPTY(&lcModPack..NewColor),&lcModPack..cPkColor,&lcModPack..NewColor))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laColor,lnFound,1)
      STORE lnFound TO lnColor
    ENDIF
    IF SEEK('P'+&lcModPack..Account+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6),'SPCK_HDR')
      lnI = 1
      DIME laSize[1,2],laVer[1]
      laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
      laSize[lnI,2]  = SPCK_HDR.cPckSize    
      SELECT SPCK_HDR
      SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                      'P'+&lcModPack..Account+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6)
        IF ASCAN(laSize,SPCK_HDR.cPckSize) = 0
          lnI = ALEN(laSize,1)+1
          DIME laSize[lnI,2]
          laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
          laSize[lnI,2]  = SPCK_HDR.cPckSize
        ENDIF  
      ENDSCAN
    ENDIF  
    lnFound = ASCAN(laSize,IIF(EMPTY(&lcModPack..NewSize),&lcModPack..cPckSize,&lcModPack..NewSize))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laSize,lnFound,1)
      STORE lnFound TO lnSize
    ENDIF
    IF SEEK('P'+&lcModPack..Account+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6)+;
            PADR(laSize[lnSize,2],3),'SPCK_HDR')
      lnI = 1
      DIME laVer[1]
      laVer[lnI]     = SPCK_HDR.cPkVersion
      SELECT SPCK_HDR
      SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                      'P'+&lcModPack..Account+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6)+;
                       PADR(laSize[lnSize,2],3)
        IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
          lnI = ALEN(laVer,1)+1
          DIME laVer[lnI]
          laVer[lnI]     = SPCK_HDR.cPkVersion
        ENDIF
      ENDSCAN
    ENDIF  
    lnFound = ASCAN(laVer,IIF(EMPTY(&lcModPack..NewVer),&lcModPack..cPkVersion,&lcModPack..NewVer))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laVer,lnFound,1)
      STORE lnFound TO lnVer
    ENDIF
  ENDIF
ENDIF

SHOW GET lnNewQty 
SHOW GET lnNewPrc
SHOW GET lnAcc
SHOW GET lnPack
SHOW GET lnColor
SHOW GET lnSize
SHOW GET lnVer
SELECT (lcModPack)
*C200425,1 [End]
*--initialize fields
SELECT (lnCurAlias)
SHOW WINDOW (lcBrTtl1) REFRESH

*!*************************************************************
*! Name      : lfUpdPkAc             (C102570,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUpdPkAc()
*!*************************************************************
*!
FUNCTION lfUpdPkAc             

PRIVAT lcAlias
lcAlias = SELECT(0)
SELECT &lcTmpPrice
IF !EOF()
  SKIP
ENDIF
SELECT (lcAlias)
   
m.Disc_Pcnt  = 0
m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100
m.cPkVersion = &lcPackHdr..cPkVersion   

m.cPkColor   = &lcPackHdr..cPkColor
m.cPckSize   = &lcPackHdr..cPckSize
m.nPkSlPrice = &lcPackHdr..nPkSlPrice
*C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
m.lRange     = &lcPackHdr..lRange
m.lPckPrPiec = &lcPackHdr..lPckPrPiec 
*REPLACE cOrdType   WITH lcOrdType ,;
        Order      WITH laData[1] ,; 
        Account    WITH laData[2] ,;
        Store      WITH IIF(llStores,lcStore,laData[3]) ,;
        LineNo     WITH lnLineNo  ,;
        Style      WITH Spck_Lin.Style ,;
        Gros_Price WITH m.Gros_Price ,;
        Price      WITH m.Price ,;
        Disc_Pcnt  WITH m.Disc_Pcnt ,;
        cFromOrder WITH m.cFromOrder ,; 
        BulkLineNo WITH m.BulkLineNo ,;
        cPkVersion WITH m.cPkVersion ,;
        cPkColor   WITH m.cPkColor,; 
        cPckSize   WITH m.cPckSize ,;
        nPkSlPrice WITH m.nPkSlPrice
REPLACE cOrdType   WITH lcOrdType ,;
        Order      WITH laData[1] ,; 
        Account    WITH laData[2] ,;
        Store      WITH IIF(llStores,lcStore,laData[3]) ,;
        LineNo     WITH lnLineNo  ,;
        Style      WITH Spck_Lin.Style ,;
        Gros_Price WITH m.Gros_Price ,;
        Price      WITH m.Price ,;
        Disc_Pcnt  WITH m.Disc_Pcnt ,;
        cFromOrder WITH m.cFromOrder ,; 
        BulkLineNo WITH m.BulkLineNo ,;
        cPkVersion WITH m.cPkVersion ,;
        cPkColor   WITH m.cPkColor,; 
        cPckSize   WITH m.cPckSize ,;
        nPkSlPrice WITH m.nPkSlPrice,;
        lRange     WITH m.lRange,;
        lPckPrPiec WITH m.lPckPrPiec 
*C200479,1 [End

=SEEK(STYLE,'STYLE')              
REPLACE Desc1    WITH Style.Desc1 ,;
        Scale    WITH Style.Scale ,;
        Season   WITH Style.Season 

*C200404,1 HBG 23/09/2002 Update the new field cPckAcc [Begin]   
m.cPckAcc    = &lcPackHdr..Account
*C200410,1 HBG 23/09/2002 Update the new field npackNo [Begin]
*REPLACE cPckAcc    WITH m.cPckAcc
m.nPackNo    = &lcPackHdr..PackQty 
REPLACE cPckAcc WITH m.cPckAcc,;
        nPackNo WITH m.nPackNo 
*C200410,1 [End]
*C200404,1 [End[

*!*************************************************************
*! Name      : lfIntSVar             (C102575,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfIntSVar()
*!*************************************************************
*!
FUNCTION lfIntSVar 
        
lcSVarFle = lcConsInvH + 'A'

*C200507,1 TMI [Start] Define a variable for GMA 
*CREATE CURSOR (lcSVarFle) ;
*        (llSumInv L(1),llByStyle L(1),lcPckAcc C(5),lnShipAmnt N(14,2),;
*         lnPackQty N(6),lcPack_id C(29),lnPkPrice N(8),lnLinShAt N(6,2),;
*         lnShipAmt N(14,2),lcTmpFle C(8),lcGMAInvH C(8))
*INSERT INTO (lcSVarFle) (llSumInv,llByStyle,lcPckAcc,lnShipAmnt,lnPackQty,;
*                         lcPack_id,lnPkPrice,lnLinShAt,lnShipAmt,lcTmpFle,lcGMAInvH);
*                VALUES  (.T.,.T.,"",0,0,"",0,0,0,gfTempName(),gfTempName())

*B122625,1 NNA 04/29/2004 (Begin) Define a new variable for GMA 
*CREATE CURSOR (lcSVarFle) ;
        (llSumInv L(1),llByStyle L(1),lcPckAcc C(5),lnShipAmnt N(14,2),;
         lnPackQty N(6),lcPack_id C(29),lnPkPrice N(8),lnLinShAt N(6,2),;
         lnShipAmt N(14,2),lcTmpFle C(8),lcGMAInvH C(8),lcIgrdStrs M(10))

*INSERT INTO (lcSVarFle) (llSumInv,llByStyle,lcPckAcc,lnShipAmnt,lnPackQty,;
                         lcPack_id,lnPkPrice,lnLinShAt,lnShipAmt,lcTmpFle ,lcGMAInvH,lcIgrdStrs);
                VALUES  (.T.,.T.,"",0,0,"",0,0,0,gfTempName(),gfTempName(),"")

CREATE CURSOR (lcSVarFle) ;
        (llSumInv L(1),llByStyle L(1),lcPckAcc C(5),lnShipAmnt N(14,2),;
         lnPackQty N(6),lcPack_id C(29),lnPkPrice N(8),lnLinShAt N(6,2),;
         lnShipAmt N(14,2),lcTmpFle C(8),lcGMAInvH C(8),lcIgrdStrs M(10), llClearVar L(1))

INSERT INTO (lcSVarFle) (llSumInv,llByStyle,lcPckAcc,lnShipAmnt,lnPackQty,;
                         lcPack_id,lnPkPrice,lnLinShAt,lnShipAmt,lcTmpFle ,;
                         lcGMAInvH,lcIgrdStrs,llClearVar);
                VALUES  (.T.,.T.,"",0,0,"",0,0,0,gfTempName(),gfTempName(),"",.F.)
*B122625,1 NNA (End)                

*C200507,1 [End]

lcTmpFle = &lcSVarFle..lcTmpFle
CREATE CURSOR (lcTmpFle) (cPack_id C(34))
INDEX ON cPack_id TAG (lcTmpFle)


*!*************************************************************
*! Name      : lfSETSVAR1          (C102575,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSETSVAR1()
*!*************************************************************
*!
FUNCTION lfSETSVAR1                

lcAlias = ALIAS()
lcSVarFle = lcConsInvH + 'A'

SELECT (lcSVarFle)
LOCATE
REPLACE lnShipAmnt WITH 0,;
        lnPackQty  WITH 0,;
        lcPack_id  WITH &lcAlias..Pack_id + &lcAlias..cPkColor + &lcAlias..cPckSize + &lcAlias..cPkVersion 
SELECT (lcAlias)        

lcTmpFle = &lcSVarFle..lcTmpFle
CREATE CURSOR (lcTmpFle) (cPack_id C(34))
INDEX ON cPack_id TAG (lcTmpFle)

*!*************************************************************
*! Name      : lfSETSVAR2        (C102575,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSETSVAR2()
*!*************************************************************
*!
FUNCTION lfSETSVAR2                

lcAlias = ALIAS()
lcSVarFle = lcConsInvH + 'A'
SELECT (lcSVarFle)
LOCATE
REPLACE lnPkPrice WITH &lcAlias..nPkSlPrice,;
        lnPackQty WITH &lcAlias..PackQty,;
        lnLinShAt WITH 0,;
        lnShipAmt WITH 0

SELECT (lcAlias)        


lcTmpFle = &lcSVarFle..lcTmpFle
CREATE CURSOR (lcTmpFle) (cPack_id C(34))
INDEX ON cPack_id TAG (lcTmpFle)

*!*************************************************************
*! Name      : lfIntDVar             (C102574,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfIntDVar()
*!*************************************************************
*!
FUNCTION lfIntDVar 
        
lcDVarFle = lcInvHdr+'A'               
CREATE CURSOR (lcDVarFle) ;
           (llUpStrInf L(1),llByStyle L(1),llSumInv L(1),lcSumPck C(8),lcPckAcc C(5),;
            lnPkPrice N(6,2),lnPckQty N(6),lnShpAmt N(6,2),lnLinShAt N(6,2),lcTmpFle C(8),lnOldShp N(6,2))

INSERT INTO (lcDVarFle) (llUpStrInf,llByStyle,llSumInv,lcSumPck,lcPckAcc,;
                         lnPkPrice,lnPckQty,lnShpAmt,lnLinShAt,lcTmpFle,lcSumPck,lnOldShp);
                VALUES  (.T.,.T.,.T.,"","",0,0,0,0,gfTempName(),gfTempName(),0)

lcTmpFle = &lcDVarFle..lcTmpFle
CREATE CURSOR (lcTmpFle) (cPack_id C(34))
INDEX ON cPack_id TAG (lcTmpFle)


*!*************************************************************
*! Name      : lfSETDVAR1          (C102574,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSETDVAR1()
*!*************************************************************
*!
FUNCTION lfSETDVAR1

lcAlias = ALIAS()
lcDVarFle = lcInvHdr+'A'
SELECT (lcDVarFle)
LOCATE
REPLACE lnPkPrice WITH &lcAlias..nPkSlPrice,;
        lnPckQty  WITH &lcAlias..PackQty,;
        lnLinShAt WITH 0,;
        lnShpAmt  WITH 0,;
        lnOldShp  WITH laData[31]
lcTmpFle = &lcDVarFle..lcTmpFle
CREATE CURSOR (lcTmpFle) (cPack_id C(34))
INDEX ON cPack_id TAG (lcTmpFle)
SELECT (lcAlias)        
                
                
*!*************************************************************
*! Name      : lfCRTTMPH        (C102575,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCRTTMPH()
*!*************************************************************
*!
FUNCTION lfCRTTMPH       

lcSVarFle = lcConsInvH + 'A'
lcGMAInvH = &lcSVarFle..lcGMAInvH 
=gfCrtTmp(lcGMAInvH ,@laFileStru,[Account+Order+Store+PikTkt+CDIVISION],lcGMAInvH)

*!*************************************************************
*! Name      : lfINTSORD        (C102575,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : intiate variables
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfINTSORD()
*!*************************************************************
*!
FUNCTION lfINTSORD

STORE '' TO lcAlias

lcFileNamA = lcOrdLine+"A"
*C200404,1 HBG 09/01/2002 add variables to use in modifing # f order packs [Begin]
*CREATE CURSOR (lcFileNamA) (llByStyle L(1),llSumLin L(1),lcSumPck C(8))

*B125563,1 NNA 02/07/2005 (Begin) Add new flag to use it with options menu and sorting by store
*CREATE CURSOR (lcFileNamA) (llByStyle L(1),llSumLin L(1),lcSumPck C(8),;
                            llRngPack L(1),lcModPack C(8),lcAlias C(10),llFromBulk L(1))
*INSERT INTO (lcFileNamA) (llByStyle,llSumLin,lcSumPck,llRngPack,lcModPack,lcAlias,llFromBulk);
                VALUES  (.F.,.F.,gfTempName(),.F.,gfTempName(),gfTempName(),.F.)

CREATE CURSOR (lcFileNamA) (llByStyle L(1),llSumLin L(1),lcSumPck C(8),;
                            llRngPack L(1),lcModPack C(8),lcAlias C(10),llFromBulk L(1),;
                            lnBarNO N(2))
INSERT INTO (lcFileNamA) (llByStyle,llSumLin,lcSumPck,llRngPack,lcModPack,lcAlias,llFromBulk,lnBarNO);
                VALUES  (.F.,.F.,gfTempName(),.F.,gfTempName(),gfTempName(),.F.,0)
*B125563,1 NNA (End)
*C200404,1 [End]

=gfOpenFile(gcDataDir+'STYLEUPC','GMAPKUPC','SH')
=gfOpenFile(gcDataDir+'SPCK_HDR','SPCK_HDRST','SH',@lcAlias,.T.)
=gfOpenFile(gcDataDir+'SPCK_LIN','SPCK_LINVR','SH')
  
*!*************************************************************
*! Name      : lfCHKBRWS        (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCHKBRWS()
*!*************************************************************
*!
FUNCTION lfCHKBRWS

lcFileNamA = lcOrdLine+"A"
llSumLin  = &lcFileNamA..llSumLin 
lcSumPck  = &lcFileNamA..lcSumPck  

IF laScrMode[2] AND llSumLin AND llDetails
  lcOrderFile = lcSumPck
ENDIF

*!*************************************************************
*! Name      : lfSEKGPRC1      (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSEKGPRC1()
*!*************************************************************
*!
FUNCTION lfSEKGPRC1

*C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
*IF !EMPTY(m.Pack_ID) AND ;
  SEEK('P'+laData[2]+m.Pack_ID+m.cPkColor+m.cPckSize+m.cPkVersion,'SPck_Hdr') AND SPck_Hdr.nPkSlPrice > 0   
IF !EMPTY(m.Pack_ID) AND m.nPkSlPrice > 0   
*C200479,1 [End]
  RETURN .T.
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfSEKGPRC2      (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSEKGPRC2()
*!*************************************************************
*!
FUNCTION lfSEKGPRC2

  *C101946,1 If customer is GMA ask to update price pack. [Begin]
  *-- Message < This line is included in a pack, do you want to update its pack price ?'>
  *-- Buttons <                                    OK                                    >
  *C102570,1 MHM 04/20/2002 add color size version to EXP. [Start]
  *IF llGMA AND SEEK('P'+laData[2]+m.Pack_ID+m.cPkVersion,'SPck_Hdr') AND Spck_Hdr.nPkSlPrice > 0
  *C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
  *IF SEEK('P'+laData[2]+m.Pack_ID+m.cPkColor+m.cPckSize+m.cPkVersion,'SPck_Hdr') AND Spck_Hdr.nPkSlPrice > 0
  IF m.nPkSlPrice > 0
  *C200479,1 [End]
  *C102570,1 MHM 04/20/2002 [End]
    IF gfModalGen('TRM00000B32000','DIALOG','','', 'This line is included in a pack,  do you want to update its pack price ?') = 2
      m.Price = &lcOrdLine..Price
      m.Disc_Pcnt = IIF(m.Gros_Price=0,0,100-m.Price*100/m.Gros_Price)
      _CUROBJ = _CUROBJ
      RETURN .T. 
    ENDIF    
  ENDIF  
  *C101946,1 If customer is GMA ask to update price pack. [End]

RETURN .F.

*!*************************************************************
*! Name      : lfSEKGPRC3      (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSEKGPRC3()
*!*************************************************************
*!
FUNCTION lfSEKGPRC3
=SEEK(&lcPackHdr..Type+&lcPackHdr..Account+&lcPackHdr..Pack_Id+&lcPackHdr..cPkColor+&lcPackHdr..cPckSize+&lcPackHdr..cPkVersion)

*!*************************************************************
*! Name      : lfUPDGPRC1      (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUPDGPRC1()
*!*************************************************************
*!
FUNCTION lfUPDGPRC1

*--102570,1 mhm i double comment it all to move it to trigger
*C101946,1 If customer is GMA update Pack Price. [Begin]
IF llGMA AND !EMPTY(m.Pack_ID) AND m.Price <> &lcOrdLine..Price       
  *C102570,1 MHM 04/20/2002 add color size version to EXP. [Start]
  *IF SEEK('P'+laData[2]+m.Pack_ID+m.cPkVersion,'SPck_Hdr') AND Spck_Hdr.nPkSlPrice > 0
  IF SEEK('P'+laData[2]+m.Pack_ID+m.cPkColor+m.cPckSize+m.cPkVersion,'SPck_Hdr') AND Spck_Hdr.nPkSlPrice > 0
  *C102570,1 MHM 04/20/2002 [End]
    PRIVATE lcAlias
    lcAlias = ALIAS()
    SELECT SPck_Hdr
    =RLOCK()
    REPLACE nPkSlPrice WITH nPkSlPrice + (m.Price - &lcOrdLine..Price)
    UNLOCK
    SELECT (lcAlias)
  ENDIF    
ENDIF

*!*************************************************************
*! Name      : lfGETREL1      (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfGETREL1()
*!*************************************************************
*!
FUNCTION lfGETREL1

SET RELATION OFF INTO SPck_Hdr
SET RELATION TO Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion INTO SPck_Hdr
lcBrFields = [Account :H="Account",Pack_Id :H="Pack Id",cPkColor :H="Color",lcGmSize=gfDoTriger('ARDINV','GETGMSZ'):H="Size",cPkVersion :H="Version",SPck_Hdr.Desc :H='Description',TotQty :H='Quantity']

*!*************************************************************
*! Name      : lfGETREL2      (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfGETREL2()
*!*************************************************************
*!
FUNCTION lfGETREL2

SET RELATION OFF INTO SPck_Lin
SET RELATION TO Type+Account+Pack_Id+cPkColor+cPckSize+cPkVersion INTO SPck_Lin , ;
                Account+Pack_Id INTO StyleUPC ADDITIVE

*!*************************************************************
*! Name      : lfGETCOND1     (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfGETCOND1()
*!*************************************************************
*!
FUNCTION lfGETCOND1
lcLeftCond = 'cPkColor+cPckSize+cPkVersion'
lcRghtCond = '&lcPackHdr..cPkColor+&lcPackHdr..cPckSize+&lcPackHdr..cPkVersion'

*!*************************************************************
*! Name      : lfGRSPRCE   (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfGRSPRCE()
*!*************************************************************
*!
FUNCTION lfGRSPRCE

*C200433,1 HBG 17/11/2002 Always get the price from SPCK_LIN[Begin]
*m.Gros_Price = IIF(Spck_Lin.npck_price<> 0 AND !(&lcPackHdr..lpckprpiec),Spck_Lin.npck_price,;
               lfGetprice(Spck_Lin.Style,lcPriceLvl,&lcPackHdr..PackQty*Spck_Lin.TotQty))
m.Gros_Price = Spck_Lin.npck_price
*C200433,1 [End]
               
*!*************************************************************
*! Name      : lfCHGBRFLD   (C102570,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 13/03/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCHGBRFLD()
*!*************************************************************
*!
FUNCTION lfCHGBRFLD

lcFields = [cMarker  =IIF(RECNO()=lnTopMark,'>',' '):H=' ':R:1:W=.F.,]+;
           [Account  :H='Account' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
           [Pack_Id  :H='Pack Id' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
           [Desc     :H='Description':R :20 :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
           [cPkVersion :H='Version' :R :W=lfVPacksWin() :V=lfVPacksWin() :F :7,]+;
           [nPkSlPrice :H='Selling Price' :R :W=lfVPacksWin() :V=lfVPacksWin() :F :13,]+;
           [StyleUPC.cUPCNum1 :H='UPC' :R :W=lfVPacksWin() :V=lfVPacksWin() :F    :7,]+;
           [StyleUPC.cUPCNum2 :H=''    :R :W=lfVPacksWin() :V=lfVPacksWin() :F    :6,]+;
           [StyleUPC.cUPCNum3 :H=''    :R :W=lfVPacksWin() :V=lfVPacksWin() :F    :2,]+;
           [PackQty  :H='Pack' :W =lfTrapKeys(.F.) :V= PackQty >= 0 AND lfTrapKeys(.T.),]
             
lcFields = lcFields + [TotWip   :H='Wip':R :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
                      [TotStk   :H='Stock':R :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
                      [TotOrd   :H='Ordered' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
                      [TotQty   :H='Qty.' :R :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
                      [lcSesDesc=gfCodDes(Season,'SEASON') :H= 'Season' :20 :R :W=lfVPacksWin() :V=lfVPacksWin() :F,]+;
                      [lcDivDesc=gfCodDes(cDivision,'CDIVISION') :H= 'Division' :20 :R :W=lfVPacksWin() :V=lfVPacksWin() :F]

*!*************************************************************
*! Name      : lfUpdQty    (C200404,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 23/09/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUpdQty()
*!*************************************************************
*!
FUNCTION lfUpdQty                      

lcAlias  = ALIAS()
llUpdate = .T.
*C200425,1 HBG 07/11/2002 Allow modifing the pack & remove store field from seek expr.[Begin]
DIMENSION laNewStr[1]    
STORE "" TO laNewStr
=gfSubStr(&lcModPack..MNewStr,@laNewStr,"|")
IF EMPTY(laNewStr[1]) AND llMultiSt 
  DIMENSION laStores[1]    
  STORE "" TO laStores
  =gfSubStr(&lcModPack..MStore,@laStores,"|")
  =ACOPY(laStores,laNewStr)
ENDIF


FOR lnI = 1 TO ALEN(laNewStr,1)
  *IF SEEK(&lcModPack..Account+&lcModPack..Store+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
          &lcModPack..cPkVersion,lcOrdLine)
  IF SEEK(&lcModPack..Account+PADR(laNewStr[lnI],8)+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
          &lcModPack..cPkVersion,lcOrdLine)
  *C200425,1 [End]
    SELECT (lcOrdLine)
    *C200425,1 HBG 07/11/2002 Remove store field from scan expr.[Begin]
    *SCAN REST WHILE cPckAcc+Store+Pack_id+cPkColor+cPckSize+cPkVersion = &lcModPack..Account+;
            &lcModPack..Store+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
            &lcModPack..cPkVersion
    SCAN REST WHILE cPckAcc+Store+Pack_id+cPkColor+cPckSize+cPkVersion = &lcModPack..Account+;
            PADR(laNewStr[lnI],8)+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
            &lcModPack..cPkVersion
    *C200425,1 [End]
      =SEEK('P'+&lcOrdLine..cPckAcc+&lcOrdLine..style+&lcOrdLine..cpkcolor+;
            &lcOrdLine..cpcksize+&lcOrdLine..pack_id+&lcOrdLine..cpkversion,'SPCK_LIN')        
      REPLACE &lcOrdLine..Qty1 WITH SPCK_LIN.Qty1 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Qty2 WITH SPCK_LIN.Qty2 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Qty3 WITH SPCK_LIN.Qty3 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Qty4 WITH SPCK_LIN.Qty4 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Qty5 WITH SPCK_LIN.Qty5 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Qty6 WITH SPCK_LIN.Qty6 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Qty7 WITH SPCK_LIN.Qty7 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Qty8 WITH SPCK_LIN.Qty8 * &lcModPack..NewPkNum
          
      REPLACE &lcOrdLine..TotQty WITH (&lcOrdLine..Qty1+&lcOrdLine..Qty2+&lcOrdLine..Qty3+;
                                       &lcOrdLine..Qty4+&lcOrdLine..Qty5+&lcOrdLine..Qty6+;
                                       &lcOrdLine..Qty7+&lcOrdLine..Qty8)
                                           
      REPLACE &lcOrdLine..Book1 WITH SPCK_LIN.Qty1 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Book2 WITH SPCK_LIN.Qty2 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Book3 WITH SPCK_LIN.Qty3 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Book4 WITH SPCK_LIN.Qty4 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Book5 WITH SPCK_LIN.Qty5 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Book6 WITH SPCK_LIN.Qty6 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Book7 WITH SPCK_LIN.Qty7 * &lcModPack..NewPkNum,;     
              &lcOrdLine..Book8 WITH SPCK_LIN.Qty8 * &lcModPack..NewPkNum
        
      REPLACE &lcOrdLine..TotBook WITH (&lcOrdLine..Book1+&lcOrdLine..Book2+&lcOrdLine..Book3+;
                                        &lcOrdLine..Book4+&lcOrdLine..Book5+&lcOrdLine..Book6+;
                                        &lcOrdLine..Book7+&lcOrdLine..Book8)
      
      REPLACE &lcOrdLine..nPackNo WITH &lcModPack..NewPkNum
      REPLACE &lcOrdLine..Flag WITH 'M'

    ENDSCAN              
  ENDIF
*C200425,1 HBG 07/11/2002 Allow modifing the pack[Begin]
ENDFOR
*C200425,1 [End]
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfUpdPrc    (E302037,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUpdPrc()
*!*************************************************************
*!
FUNCTION lfUpdPrc

lcAlias  = ALIAS()
llUpdate = .T.
lnCalcPr  = 0
lnTotPrice = 0
lnCountRec = 0
SELECT SPCK_LIN
SET ORDER TO SPCK_LINVR

IF SEEK('P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
        &lcModPack..cPkVersion)
  SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion+style =;
                  'P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+;
                  &lcModPack..cPckSize+&lcModPack..cPkVersion
    lnCountRec = lnCountRec +1
    *B121372,1 MHM 02/05/2004 Let it get only total Qty [Start]
    *lnTotPrice = lnTotPrice + TotQty * npck_price
    lnTotPrice = lnTotPrice + TotQty 
    *B121372,1 MHM 02/05/2004 [End]
  ENDSCAN
ENDIF  
lnCalcPr  = 0
  
lnCalPrSty  = 0
IF SEEK('P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
        &lcModPack..cPkVersion)
  SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion+style =;
                  'P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+;
                  &lcModPack..cPckSize+&lcModPack..cPkVersion
    *B606913,1 HBG 01/27/2003 If price per piece case , assign the same price to all styles  [Begin]
    *C200479,1 HBG 30/12/2002 Use lPckPrPiec which come from ORDLINE [Begin] 
    *=SEEK('P'+&lcModPack..Account+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
    *    &lcModPack..cPkVersion,'SPCK_HDR')
    *IF SPCK_HDR.lPckPrPiec
    IF &lcModPack..lPrPerPiec
    *C200479,1 [End[
      IF TotQty <> 0
        DIMENSION laNewStr[1]    
        STORE "" TO laNewStr
        =gfSubStr(&lcModPack..MNewStr,@laNewStr,"|")
        IF EMPTY(laNewStr[1]) AND llMultiSt 
          DIMENSION laStores[1]    
          STORE "" TO laStores
          =gfSubStr(&lcModPack..MStore,@laStores,"|")
          =ACOPY(laStores,laNewStr)
        ENDIF
        FOR lnI = 1 TO ALEN(laNewStr,1)
          IF SEEK(&lcModPack..Account+PADR(laNewStr[lnI],8)+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
                  &lcModPack..cPkVersion+SPCK_LIN.Style,lcOrdLine)  
           SELECT (lcOrdLine)
           REPLACE &lcOrdLine..Price      WITH &lcModPack..NewPrice,;
                   &lcOrdLine..Gros_Price WITH &lcModPack..NewPrice,;
                   &lcOrdLine..nPkSlPrice WITH &lcModPack..NewPrice
           REPLACE &lcOrdLine..Flag WITH 'M'
       
          ENDIF   
        ENDFOR
      ENDIF  
    ELSE
    *B606913,1 [End]

      *B121372,1 MHM 02/05/2004 No need for the following Variables [Start]
      *lnCalPrSty = (TotQty * IIF(npck_price <> 0, npck_price, 1)) / ;
      *             IIF(lnTotPrice = 0,lnCountRec,lnTotPrice)
      *lnCalPrSty = ROUND(lnCalPrSty, 2)
      *lnStyPer   = lnCalPrSty * &lcModPack..NewPrice
      *lnCalcPr   = lnCalcPr + lnStyPer
      *B121372,1 MHM 02/05/2004  [End]
      
      IF TotQty <> 0
        *C200425,1 HBG 07/11/2002 Update pack Price for all selected stores[Begin]
        DIMENSION laNewStr[1]    
        STORE "" TO laNewStr
        =gfSubStr(&lcModPack..MNewStr,@laNewStr,"|")
        IF EMPTY(laNewStr[1]) AND llMultiSt 
          DIMENSION laStores[1]    
          STORE "" TO laStores
          =gfSubStr(&lcModPack..MStore,@laStores,"|")
          =ACOPY(laStores,laNewStr)
        ENDIF
      
        FOR lnI = 1 TO ALEN(laNewStr,1)
          *IF SEEK(&lcModPack..Account+&lcModPack..Store+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
                  &lcModPack..cPkVersion+SPCK_LIN.Style,lcOrdLine)  
          IF SEEK(&lcModPack..Account+PADR(laNewStr[lnI],8)+&lcModPack..Pack_id+&lcModPack..cPkColor+&lcModPack..cPckSize+;
                  &lcModPack..cPkVersion+SPCK_LIN.Style,lcOrdLine)  
           *C200425,1 [END]
           SELECT (lcOrdLine)

           *B121372,1 MHM 02/05/2004 replace with correct values [Start]
           *REPLACE &lcOrdLine..Price      WITH ROUND(lnStyPer / SPCK_LIN.TotQty, 2),;
           *        &lcOrdLine..Gros_Price WITH ROUND(lnStyPer / SPCK_LIN.TotQty, 2),;
           *        &lcOrdLine..nPkSlPrice WITH &lcModPack..NewPrice
           REPLACE &lcOrdLine..Price      WITH ROUND(&lcModPack..NewPrice/lnTotPrice,2),;
                   &lcOrdLine..Gros_Price WITH ROUND(&lcModPack..NewPrice/lnTotPrice,2),;
                   &lcOrdLine..nPkSlPrice WITH &lcModPack..NewPrice
           *B121372,1 MHM 02/05/2004  [End]

           REPLACE &lcOrdLine..Flag WITH 'M'     
          ENDIF   
          *C200425,1 HBG 07/11/2002 Update pack Price for all selected stores[Begin]
        ENDFOR
        *C200425,1 [End]
      ENDIF  
    *B606913,1 HBG 01/27/2003 End if price per piece case , assign the same price to all styles  [Begin]
    ENDIF
    *B606913,1 [End]
  ENDSCAN
ENDIF  
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfUPDPKFLD    (E302037,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUPDPKFLD()
*!*************************************************************
*!
FUNCTION lfUPDPKFLD

IF !EMPTY(m.Pack_Id)
  lcAlias = ALIAS()
  SELECT (lcOrdLine)
  SET ORDER TO SPCK_HDRVR IN SPCK_HDR
  llUpdate = .T.
  =SEEK('S'+&lcOrdLine..Scale,'SCALE')
  FOR lnI =  1 TO SCALE.cnt 
    lcI = ALLTRIM(STR(lnI))
    IF MOD(&lcOrdLine..Qty&lcI / SPCK_LIN.Qty&lcI,1) <> 0
      llUpdate = .F.
      EXIT
    ENDIF
  ENDFOR
  IF llUpdate 
    REPLACE &lcOrdLine..Price      WITH SPCK_LIN.npck_price,;
            &lcOrdLine..Gros_Price WITH SPCK_LIN.npck_price,;
            &lcOrdLine..cPkColor   WITH SPCK_LIN.cPkColor  ,;
            &lcOrdLine..cPckSize   WITH SPCK_LIN.cPckSize  ,;
            &lcOrdLine..cPkVersion WITH SPCK_LIN.cPkVersion,;
            &lcOrdLine..cPckAcc    WITH SPCK_LIN.Account    
    =SEEK('P'+SPCK_LIN.Account+SPCK_LIN.Pack_Id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize+SPCK_LIN.cPkVersion,'SPCK_HDR')
    *C200479,1 HBG 30/12/2002 use lRange & lPCkPrPiec fields from ORDLINE instead of SPCH_HDR [Begin]
    *REPLACE &lcOrdLine..nPkSlPrice WITH SPCK_HDR.nPkSlPrice,;
            &lcOrdLine..nPackNo    WITH &lcOrdLine..TotQty/SPCK_LIN.TotQty
    REPLACE &lcOrdLine..lRange     WITH SPCK_HDR.lRange,;
            &lcOrdLine..lPckPrPiec WITH SPCK_HDR.lPckPrPiec ,;
            &lcOrdLine..nPkSlPrice WITH SPCK_HDR.nPkSlPrice,;
            &lcOrdLine..nPackNo    WITH &lcOrdLine..TotQty/SPCK_LIN.TotQty
    *C200479,1 [End]
  ELSE
    =gfModalGen("INM00000B00000","Dialog",.F.,.F.,"Qty of this line not divisable by pack Qty.Cann't select this pack.")  
    m.Pack_id = &lcOrdLine..Pack_Id
  ENDIF  
  =lfChkRgPck()
  SELECT (lcAlias)
ENDIF
*!**************************************************************************
*! Name      : lfEDTSCRSO
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/28/2001        
*! Purpose   : Display Screen to Edit the pack information.
*!*************************************************************
*! Example   : =lfEDTSCRSO
*!*************************************************************
*!C200424,1 MHM
PROCEDURE lfEDTSCRSO
PRIVATE lcEscInt, lcTabInt, lcHldBtb, lcAltB

lcEscInt = ON('KEY','ESC')
lcTabInt = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcAltB   = ON('KEY','ALT+B')    

ON KEY LABEL ESC DO lfVCanSo
DO (gcScrDir+"SO\SOEDTCST.SPX")

ON KEY LABEL ESC      &lcEscInt
ON KEY LABEL TAB      &lcTabInt
ON KEY LABEL BACKTAB  &lcHldBtb
ON KEY LABEL ALT+B    &lcAltB

*!**************************************************************************
*! Name      : lfVOkSo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/28/2001        
*! Purpose   : validate Ok Button
*!**************************************************************************
*! Example   : =lfVOkSo()
*!**************************************************************************
*!C200424,1 MHM 
FUNCTION lfVOkSo

REPLACE &lcOrdLine..custpo     WITH m.custpo;
        &lcOrdLine..nsugretpri WITH m.nsugretpri,;
        &lcOrdLine..Flag WITH "M"

CLEAR READ
*!**************************************************************************
*! Name      : lfVCanSo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/28/2001        
*! Purpose   : validate Cancel Button
*!**************************************************************************
*! Example   : =lfVCanSo()
*!**************************************************************************
*!C200424,1 MHM 
FUNCTION lfVCanSo

IF laScrMode[2]
  CLEAR READ
  RETURN
ENDIF
m.custpo       = &lcOrdLine..custpo
m.nsugretpri   = &lcOrdLine..nsugretpri

CLEAR READ
*!**************************************************************************
*! Name      : lfVWhnSo 
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/28/2001        
*! Purpose   : validate When enter screen
*!**************************************************************************
*! Example   : =lfVWhnSo()
*!**************************************************************************
*!C200424,1 MHM 
FUNCTION lfVWhnSo

IF laScrMode[2]
  SHOW GET m.custpo     DISABLE
  SHOW GET m.nsugretpri DISABLE
  SHOW GET pbOk         DISABLE
ENDIF

*!**************************************************************************
*! Name      : lfSOCUSTPO
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/28/2001        
*! Purpose   : change Updating CustPo 
*!**************************************************************************
*! Example   : =lfVWhnSo()
*!**************************************************************************
*!C200424,1 MHM 
FUNCTION lfSOCUSTPO

IF EMPTY(laData[4])
  SELECT (lcOrdLine)
  SCAN 
    =SEEK(lcOrdType+laData[1]+STR(LineNo,6),"ORDLINE")
    REPLACE ORDLINE.CustPo WITH &lcOrdLine..CustPo
  ENDSCAN  
ENDIF

******

*!*************************************************************
*! Name      : lfNewQty   (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfNewQty()
*!*************************************************************
*!
FUNCTION lfNewQty

lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack

SELECT (lcModPack)
REPLACE &lcModPack..NewPkNum WITH lnNewQty

*!*************************************************************
*! Name      : lfNewPrc   (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfNewPrc()
*!*************************************************************
*!
FUNCTION lfNewPrc

lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack

SELECT (lcModPack)
REPLACE &lcModPack..NewPrice WITH lnNewPrc

*!*************************************************************
*! Name      : lfNewAcc       (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfNewAcc()
*!*************************************************************
*!
FUNCTION lfNewAcc

lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack

SELECT (lcModPack)
REPLACE &lcModPack..NewAcc WITH PADR(laAcc[lnAcc],5)
SET ORDER TO SPCK_HDRVR IN SPCK_HDR
IF SEEK('P'+PADR(laAcc[lnAcc],5),'SPCK_HDR')
  lnI = 1
  DIME laPack[1],laColor[1],laSize[1,2],laVer[1]

  SET ORDER TO Spck_hdrst IN SPCK_HDR
  IF SEEK(PADR(&lcModPack..Account,5)+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'STYLEUPC') OR;
     SEEK('S'+PADR(&lcModPack..Account,5)+PADR(&lcModPack..Pack_Id,19)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'SPCK_HDR')
    laPack[lnI]    = &lcModPack..Pack_id
    laColor[lnI]   = &lcModPack..cPkColor
    laSize[lnI,1]  = lfGetGmSz(&lcModPack..cPckSize)
    laSize[lnI,2]  = &lcModPack..cPckSize  
    laVer[lnI]     = &lcModPack..cPkVersion
    STORE 1 TO lnPack,lnColor,lnSize
    SELECT SPCK_HDR
    SET ORDER TO SPCK_HDRVR IN SPCK_HDR
    =SEEK('P'+PADR(laAcc[lnAcc],5)+PADR(&lcModPack..Pack_Id,16)+&lcModPack..cpkcolor+;
          &lcModPack..cpcksize,'SPCK_HDR')
    SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                    'P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6)+;
                    PADR(laSize[lnSize,2],3)
      IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
        lnI = ALEN(laVer,1)+1
        DIME laVer[lnI]
        laVer[lnI]     = SPCK_HDR.cPkVersion
      ENDIF
    ENDSCAN
    STORE 1 TO lnPack,lnColor,lnSize,lnVer
  ELSE
    SET ORDER TO SPCK_HDRVR IN SPCK_HDR
    IF SEEK('P'+PADR(laAcc[lnAcc],5),'SPCK_HDR')
      lnI = 1
      DIME laPack[1],laColor[1],laSize[1,2],laVer[1]
      laPack[lnI]    = SPCK_HDR.Pack_id
      laColor[lnI]   = SPCK_HDR.cPkColor
      laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
      laSize[lnI,2]  = SPCK_HDR.cPckSize    
      laVer[lnI]     = SPCK_HDR.cPkVersion
      SELECT SPCK_HDR
      SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                      'P'+PADR(laAcc[lnAcc],5)
        IF ASCAN(laPack,SPCK_HDR.Pack_id) = 0
          lnI = ALEN(laPack,1)+1
          DIME laPack[lnI]
          laPack[lnI]   = SPCK_HDR.Pack_id
        ENDIF
      ENDSCAN
      lnFound = ASCAN(laPack,IIF(EMPTY(&lcModPack..NewPack),&lcModPack..Pack_ID,&lcModPack..NewPack))
      IF lnFound > 0
        lnFound = ASUBSCRIPT(laPack,lnFound,1)
        STORE lnFound TO lnPack
      ENDIF
      SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                      'P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16)
                      
        IF ASCAN(laColor,SPCK_HDR.cPkColor) = 0
          lnI = ALEN(laColor,1)+1
          DIME laColor[lnI]
          laColor[lnI]   = SPCK_HDR.cPkColor
        ENDIF
        IF ASCAN(laSize,SPCK_HDR.cPckSize) = 0
          lnI = ALEN(laSize,1)+1
          DIME laSize[lnI,2]
          laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
          laSize[lnI,2]  = SPCK_HDR.cPckSize
        ENDIF  
        IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
          lnI = ALEN(laVer,1)+1
          DIME laVer[lnI]
          laVer[lnI]     = SPCK_HDR.cPkVersion
        ENDIF
      ENDSCAN                
    ENDIF  
    lnFound = ASCAN(laColor,IIF(EMPTY(&lcModPack..NewColor),&lcModPack..cPkColor,&lcModPack..NewColor))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laColor,lnFound,1)
      STORE lnFound TO lnColor
    ENDIF
    lnFound = ASCAN(laSize,IIF(EMPTY(&lcModPack..NewSize),&lcModPack..cPckSize,&lcModPack..NewSize))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laSize,lnFound,1)
      STORE lnFound TO lnSize
    ENDIF
    lnFound = ASCAN(laVer,IIF(EMPTY(&lcModPack..NewVer),&lcModPack..cPkVersion,&lcModPack..NewVer))
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laVer,lnFound,1)
      STORE lnFound TO lnVer
    ENDIF
  ENDIF  
ENDIF  
SHOW GET lnPack
SHOW GET lnColor
SHOW GET lnSize
SHOW GET lnVer
SELECT (lcModPack)
REPLACE &lcModPack..NewPack  WITH PADR(laPack[lnPack],16),;
        &lcModPack..NewColor WITH PADR(laColor[lnColor],6),;
        &lcModPack..NewSize  WITH PADR(laSize[lnSize,2],3),;
        &lcModPack..NewVer   WITH PADR(laVer[lnVer],4)

*!*************************************************************
*! Name      : lfNewPck(C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfNewPck()
*!*************************************************************
*!
FUNCTION lfNewPck

lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack

SELECT (lcModPack)
REPLACE &lcModPack..NewAcc  WITH PADR(laAcc[lnAcc],5),;
        &lcModPack..NewPack WITH PADR(laPack[lnPack],16)

IF SEEK('P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16),'SPCK_HDR')
  lnI = 1
  DIME laColor[1],laSize[1,2],laVer[1]
  laColor[lnI]   = SPCK_HDR.cPkColor
  laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
  laSize[lnI,2]  = SPCK_HDR.cPckSize    
  laVer[lnI]     = SPCK_HDR.cPkVersion
  SELECT SPCK_HDR
  SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                  'P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16)
    IF ASCAN(laColor,SPCK_HDR.cPkColor) = 0
      lnI = ALEN(laColor,1)+1
      DIME laColor[lnI]
      laColor[lnI]   = SPCK_HDR.cPkColor
    ENDIF
    IF ASCAN(laSize,SPCK_HDR.cPckSize) = 0
      lnI = ALEN(laSize,1)+1
      DIME laSize[lnI,2]
      laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
      laSize[lnI,2]  = SPCK_HDR.cPckSize
    ENDIF  
    IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
      lnI = ALEN(laVer,1)+1
      DIME laVer[lnI]
      laVer[lnI]     = SPCK_HDR.cPkVersion
    ENDIF
  ENDSCAN
ENDIF  
STORE 1 TO lnColor,lnSize,lnVer
SHOW GET lnColor
SHOW GET lnSize
SHOW GET lnVer
SELECT (lcModPack)
REPLACE &lcModPack..NewColor WITH PADR(laColor[lnColor],6),;
        &lcModPack..NewSize  WITH PADR(laSize[lnSize,2],3),;
        &lcModPack..NewVer   WITH PADR(laVer[lnVer],4)
        
*!*************************************************************
*! Name      : lfNewColor    (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfNewColor()
*!*************************************************************
*!
FUNCTION lfNewColor

lcFileNamA = lcOrdLine+"A"
lcModPack  = &lcFileNamA..lcModPack

SELECT (lcModPack)
REPLACE &lcModPack..NewAcc   WITH PADR(laAcc[lnAcc],5),;
        &lcModPack..NewPack  WITH PADR(laPack[lnPack],16),;
        &lcModPack..NewColor WITH PADR(laColor[lnColor],6)

IF SEEK('P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6),'SPCK_HDR')
  lnI = 1
  DIME laSize[1,2],laVer[1]
  laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
  laSize[lnI,2]  = SPCK_HDR.cPckSize    
  laVer[lnI]     = SPCK_HDR.cPkVersion
  SELECT SPCK_HDR
  SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                  'P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6)
    IF ASCAN(laSize,SPCK_HDR.cPckSize) = 0
      lnI = ALEN(laSize,1)+1
      DIME laSize[lnI,2]
      laSize[lnI,1]  = lfGetGmSz(SPCK_HDR.cPckSize)
      laSize[lnI,2]  = SPCK_HDR.cPckSize
    ENDIF  
    IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
      lnI = ALEN(laVer,1)+1
      DIME laVer[lnI]
      laVer[lnI]     = SPCK_HDR.cPkVersion
    ENDIF
  ENDSCAN
ENDIF  
STORE 1 TO lnSize,lnVer
SHOW GET lnSize
SHOW GET lnVer
SELECT (lcModPack)
REPLACE &lcModPack..NewSize  WITH PADR(laSize[lnSize,2],3),;
        &lcModPack..NewVer   WITH PADR(laVer[lnVer],4)

*!*************************************************************
*! Name      : lfNewSize    (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfNewSize()
*!*************************************************************
*!
FUNCTION lfNewSiz

lcFileNamA = lcOrdLine+"A"
lcModPack  = &lcFileNamA..lcModPack


SELECT (lcModPack)
REPLACE &lcModPack..NewAcc   WITH PADR(laAcc[lnAcc],5),;
        &lcModPack..NewPack  WITH PADR(laPack[lnPack],16),;
        &lcModPack..NewColor WITH PADR(laColor[lnColor],6),;
        &lcModPack..NewSize  WITH PADR(laSize[lnSize,2],3)

IF SEEK('P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6)+;
        PADR(laSize[lnSize,2],3),'SPCK_HDR')
  lnI = 1
  DIME laVer[1]
  laVer[lnI]     = SPCK_HDR.cPkVersion
  SELECT SPCK_HDR
  SCAN REST WHILE type+account+pack_id+cpkcolor+cpcksize+cpkversion =;
                  'P'+PADR(laAcc[lnAcc],5)+PADR(laPack[lnPack],16)+PADR(laColor[lnColor],6)+;
                   PADR(laSize[lnSize,2],3)
    IF ASCAN(laVer,SPCK_HDR.cPkVersion) = 0
      lnI = ALEN(laVer,1)+1
      DIME laVer[lnI]
      laVer[lnI]     = SPCK_HDR.cPkVersion
    ENDIF
  ENDSCAN
ENDIF  
STORE 1 TO lnVer
SHOW GET lnVer
SELECT (lcModPack)
REPLACE &lcModPack..NewVer WITH PADR(laVer[lnVer],4)

*!*************************************************************
*! Name      : lfNewVer    (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 21/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfNewVer()
*!*************************************************************
*!
FUNCTION lfNewVer

lcFileNamA = lcOrdLine+"A"
lcModPack  = &lcFileNamA..lcModPack

SELECT (lcModPack)
REPLACE &lcModPack..NewAcc   WITH PADR(laAcc[lnAcc],5),;
        &lcModPack..NewPack  WITH PADR(laPack[lnPack],16),;
        &lcModPack..NewColor WITH PADR(laColor[lnColor],6),;
        &lcModPack..NewSize  WITH PADR(laSize[lnSize,2],3),;
        &lcModPack..NewVer   WITH PADR(laVer[lnVer],4)

*!*************************************************************
*! Name      : lfChgPck   (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfChgPck()
*!*************************************************************
*!
FUNCTION lfChgPck
PARAMETERS llStores

DIMENSION laStores[1]    
STORE "" TO laStores
=gfSubStr(&lcModPack..MStore,@laStores,"|")
DIMENSION laNewStr[1]    
STORE "" TO laNewStr
=gfSubStr(&lcModPack..MNewStr,@laNewStr,"|")
IF EMPTY(laNewStr[1]) AND llMultiSt 
  =ACOPY(laStores,laNewStr)
ENDIF

DIMENSION laThsPckNo[ALEN(laNewStr,1)]    
STORE 0 TO laThsPckNo
FOR lnI = 1 TO ALEN(laNewStr,1)
  =lfREMOVPck()
ENDFOR

SET ORDER TO SPCK_LINVR IN SPCK_LIN
SET ORDER TO SPCK_HDRVR IN SPCK_HDR
FOR lnI = 1 TO ALEN(laNewStr,1)
  =SEEK('P'+&lcModPack..NewAcc+&lcModPack..NewPack+&lcModPack..NewColor+;
          &lcModPack..NewSize+&lcModPack..NewVer,'SPCK_HDR')      
  IF SEEK('P'+&lcModPack..NewAcc+&lcModPack..NewPack+&lcModPack..NewColor+;
          &lcModPack..NewSize+&lcModPack..NewVer,'SPCK_LIN')        
    
    *B120209,1 MHM 12/22/2003 Get 2 fields from Pack Header [Start]
    m.lrange = SPCK_HDR.lrange
    m.lpckprpiec = SPCK_HDR.lpckprpiec
    *B120209,1 MHM 12/22/2003 Get 2 fields from Pack Header [End]
          
    SELECT SPCK_LIN
    SCAN REST WHILE Type + Account + Pack_id + cPkColor + cPckSize + cPkVersion =;
                    'P'+&lcModPack..NewAcc+&lcModPack..NewPack+&lcModPack..NewColor+;
                     &lcModPack..NewSize+&lcModPack..NewVer     

      =SEEK(Style,'STYLE')

      IF laSetups[5,2]='Y' .AND. !SEEK(Style+laData[31]+SPACE(10),'StyDye')
        DO gpAdStyWar WITH Style,SPACE(10),laData[31]
      ENDIF
      STORE 0 TO m.Gros_Price,m.Price,m.Disc_Pcnt
      m.lContract = lcOrdType<>'C' .AND. lfvContPri(Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
      IF !m.lContract
         
        m.Gros_Price = Spck_Lin.npck_price
        lcDiscCode  = IIF(SEEK(Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
        m.Disc_Pcnt = 0
        IF !EMPTY(ALLTRIM(lcDiscCode))
          *-- Get the disecound related filed to now which 
          *-- type whole Sale Or Retail sale Or Both.
          DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
          STORE '' To lcDisType , ldstartDte ,ldEndDate
          *-- Array to get the Discount affect for DecCode.
          laDisType[1,1]  = 'CCOSTAFECT'
          laDisType[1,2]  = 'lcDisType'
          *-- Array to get the start date For DescCode.
          lastartDte[1,1] = 'START'
          lastartDte[1,2] = 'ldstartDte'
          *-- Array to get the end date For DescCode.
          laEndDate[1,1]  = 'DENDATE'
          laEndDate[1,2]  = 'ldEndDate'
          = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
          = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
          = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
          IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
            lnDisc_Pcnt = 0
            =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
            m.Disc_Pcnt = lnDisc_Pcnt
          ENDIF
        ENDIF 
        m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
      ENDIF         
      m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
      IF laSetups[4,2]='Y' AND !SEEK('02'+m.Gl_Sales,'Gl_Link')
        m.Gl_Sales = 'DEFDEF'
      ENDIF
      lnLines  = lnLines + 1
      lnLineNo = lnLines
      SELECT (lcOrdLine)
      APPEND BLANK
      =RLOCK()
      m.Disc_Pcnt  = 0
      m.Price      = m.Gros_Price*(100-m.Disc_Pcnt)/100
      m.cPkVersion = &lcModPack..NewVer

      m.cPkColor   = &lcModPack..NewColor
      m.cPckSize   = &lcModPack..NewSize
      m.nPkSlPrice = IIF(&lcModPack..NewPrice = 0,SPCK_HDR.nPkSlPrice,&lcModPack..NewPrice)
      

      REPLACE cOrdType   WITH lcOrdType ,;
              Order      WITH laData[1] ,; 
              Account    WITH laData[2] ,;
              Store      WITH PADR(laNewStr[lnI],8),;
              LineNo     WITH lnLineNo  ,;
              Style      WITH Spck_Lin.Style ,;
              Gros_Price WITH m.Gros_Price ,;
              Price      WITH m.Price ,;
              Disc_Pcnt  WITH m.Disc_Pcnt ,;
              cFromOrder WITH m.cFromOrder ,; 
              BulkLineNo WITH m.BulkLineNo ,;
              cPkVersion WITH m.cPkVersion ,;
              cPkColor   WITH m.cPkColor,; 
              cPckSize   WITH m.cPckSize ,;
              nPkSlPrice WITH m.nPkSlPrice

      REPLACE Flag WITH 'M'
      
      =SEEK(STYLE,'STYLE')              
      REPLACE Desc1    WITH Style.Desc1 ,;
              Scale    WITH Style.Scale ,;
              Season   WITH Style.Season 

      m.cPckAcc    = &lcModPack..NewAcc
      m.nPackNo    = IIF(&lcModPack..NewPkNum = 0,laThsPckNo[lnI],&lcModPack..NewPkNum)
      REPLACE cPckAcc WITH m.cPckAcc,;
              nPackNo WITH m.nPackNo 
      REPLACE Qty1       WITH Spck_Lin.Qty1*m.nPackNo,;
              Qty2       WITH Spck_Lin.Qty2*m.nPackNo,;
              Qty3       WITH Spck_Lin.Qty3*m.nPackNo,;
              Qty4       WITH Spck_Lin.Qty4*m.nPackNo,;
              Qty5       WITH Spck_Lin.Qty5*m.nPackNo,;
              Qty6       WITH Spck_Lin.Qty6*m.nPackNo,;
              Qty7       WITH Spck_Lin.Qty7*m.nPackNo,;
              Qty8       WITH Spck_Lin.Qty8*m.nPackNo,;
              TotQty     WITH Spck_Lin.TotQty*m.nPackNo,;
              Comm1      WITH laData[28] ,;
              Comm2      WITH laData[30] ,;
              cWareCode  WITH laData[31] ,;
              Gl_Sales   WITH m.Gl_Sales ,;
              Pack_Id    WITH &lcModPack..NewPack,;
              lContract  WITH m.lContract
      REPLACE Flag    WITH 'N' ,;
              Book1   WITH Spck_Lin.Qty1*m.nPackNo,;
              Book2   WITH Spck_Lin.Qty2*m.nPackNo,;
              Book3   WITH Spck_Lin.Qty3*m.nPackNo,;
              Book4   WITH Spck_Lin.Qty4*m.nPackNo,;
              Book5   WITH Spck_Lin.Qty5*m.nPackNo,;
              Book6   WITH Spck_Lin.Qty6*m.nPackNo,;
              Book7   WITH Spck_Lin.Qty7*m.nPackNo,;
              Book8   WITH Spck_Lin.Qty8*m.nPackNo,;
              TotBook WITH Spck_Lin.TotQty*m.nPackNo

      *B120209,1 MHM 12/22/2003 Replace in temp Ordline file [Start]
      REPLACE lrange     WITH m.lrange ,;
              lpckprpiec WITH m.lpckprpiec
      *B120209,1 MHM 12/22/2003 Replace in temp Ordline file [End]

      laData[35] = laData[35] + TotQty
      laData[36] = laData[36] + TotQty*Price
      laData[41] = laData[41] + TotQty
      laData[42] = laData[42] + TotQty*Price
      UNLOCK
    ENDSCAN
  ENDIF    
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOK    WITH laData[35] ,;
          BOOKAMT WITH laData[36] ,;
          OPEN    WITH laData[41] ,;
          OPENAMT WITH laData[42]
  UNLOCK
  =lfCHKRGPCK()
ENDFOR
*!*************************************************************
*! Name      : lfREMOVPck (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfREMOVPck()
*!*************************************************************
*!
FUNCTION lfREMOVPck

SELECT IIF(llStores,lcTempLine,lcOrdLine)
PRIVATE lcCurPckID , lcCurVersn , lcCurStore
lcCurAccou = &lcModPack..Account
lcCurStore = PADR(laStores[lnI],8)
lcCurPckID = &lcModPack..Pack_id + &lcModPack..cPkColor + &lcModPack..cPckSize
lcCurVersn = &lcModPack..cPkVersion

SCAN FOR Account + Store + Pack_id + cPkColor + cPckSize + cPkVersion = lcCurAccou + lcCurStore + lcCurPckID + lcCurVersn
  SCATTER MEMVAR MEMO 
  laThsPckNo[lnI] = m.nPackNo
  IF llStores
    lnTotPcs = lnTotPcs - m.TotQty
    lnTotAmt = lnTotAmt - m.TotQty*m.Price
  ELSE
    SELECT (lcOrdLine)
    =RLOCK()    
    REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
    UNLOCK

    IF TotCut > 0 
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
      =SEEK(IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6),'CutPick')
      SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
      SELECT CutPick
      SCAN REST WHILE TranCd+Order+cOrdLine = ;
                      IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6)
        IF !SEEK(CutPick.TranCd+CutPick.cTktNo+CutPick.cTktLineNo+;
                 CutPick.Order+CutPick.cOrdLine,lcAlocated)
          INSERT INTO (lcAlocated) ;
          (cTktno,TranCd,cTktLineNo,Order,cOrdLine,Style) VALUES ;
          (CutPick.cTktNo,CutPick.Trancd,CutPick.cTktLineNo,CutPick.Order,;
           CutPick.cOrdLine,CutPick.Style)
        ENDIF
        SELECT (lcAlocated)
        =RLOCK()
        REPLACE Qty1   WITH 0 ,;
                Qty2   WITH 0 ,;
                Qty3   WITH 0 ,;
                Qty4   WITH 0 ,;
                Qty5   WITH 0 ,;
                Qty6   WITH 0 ,;
                Qty7   WITH 0 ,;
                Qty8   WITH 0 ,;
                TotQty WITH 0
        UNLOCK
        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE TotCut WITH TotCut - CutPick.TotQty
        UNLOCK
      ENDSCAN
      =gfCloseFile('CUTPICK')
      SELECT (lcOrdLine)
    ENDIF
    IF laScrMode[4] .AND. !EMPTY(laData[43]) .AND. ;
       SEEK(STR(BulkLineNo,6),lcBulkOrd) 
      SELECT (lcBulkOrd)
      =RLOCK()
      REPLACE USED1 WITH USED1 - m.Qty1 ,;
              USED2 WITH USED2 - m.Qty2 ,;
              USED3 WITH USED3 - m.Qty3 ,;
              USED4 WITH USED4 - m.Qty4 ,;
              USED5 WITH USED5 - m.Qty5 ,;
              USED6 WITH USED6 - m.Qty6 ,;
              USED7 WITH USED7 - m.Qty7 ,;
              USED8 WITH USED8 - m.Qty8 ,;
              TOTUSED WITH TOTUSED - m.TotQty
      UNLOCK
    ENDIF
    laData[35] = laData[35] - m.TotQty
    laData[36] = laData[36] - m.TotQty*m.Price
    laData[39] = laData[39] + m.TotQTy
    laData[40] = laData[40] + m.TotQTy*m.Price
    laData[41] = laData[41] - m.TotQty
    laData[42] = laData[42] - m.TotQty*m.Price
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
  ENDIF
  IF laScrMode[3] AND !llUpdBook
    =gfwCodePop(@laCodes,'CCANCRESON','D')
    INSERT INTO (lcOrdCanLn) ;
     (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
      Style,Account,Store,Dyelot,Price) VALUES ;
     (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
      m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
      m.Style,m.Account,m.Store,m.Dyelot,m.Price)      
        
    SELECT (lcOrdCanLn)
    STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
    STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
    =gfwCodePop(@laCodes,'CCANCRESON','T')
    STORE gdSysDate TO m.Cancelled
    DO (gcScrDir+"SOORDCLN.SPX")
    STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
    =gfAdd_Info(lcOrdCanLn)
  ENDIF 
  
  IF llBomVarnt
    SELECT (lcT_BomVar)
    =SEEK("SO"+laData[1]+STR(&lcOrdLine..LineNo,6))
    SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
      REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
    ENDSCAN
    DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcOrdLine..LineNo,6)
  ENDIF

  SELECT IIF(llStores,lcTempLine,lcOrdLine)
  DELETE
ENDSCAN
  
SELECT IIF(llStores,lcTempLine,lcOrdLine)
GO TOP
  
STORE .T. TO llCUpdate
*-- End of lfREMOVPck 

*!*************************************************************
*! Name      : lfSelStor   (C200425,1)
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/10/2002
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSelStor()
*!*************************************************************
*!
FUNCTION lfSelStor

lcFileNamA = lcOrdLine+"A"
lcModPack = &lcFileNamA..lcModPack

DIMENSION laStores[1] , laNewStr[1]   
STORE "" TO laStores , laNewStr
=gfSubStr(&lcModPack..MStore,@laStores,"|")

IF !EMPTY(&lcModPack..MNewStr)
  =gfSubStr(&lcModPack..MNewStr,@laNewStr,"|")
ENDIF

=gfMover(@laStores,@laNewStr,"Select Stores to be update")

IF llMultiSt
  SELECT (lcModPack)
  IF !EMPTY(laNewStr[1])
    FOR lnI  = 1 TO ALEN(laNewStr,1)
      IF ATC(laNewStr[lnI],MNewStr) = 0
        REPLACE &lcModPack..MNewStr WITH &lcModPack..MNewStr + IIF(lnI = 1,"",'|') + laNewStr[lnI]
      ENDIF  
    ENDFOR
  ENDIF    
ENDIF  

*!*************************************************************
*! Name      : lfvUdCrtWgt (C200484,1)
*! Developer : ALBERT RAIF (ALB)
*! Date      : 01/13/2003
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvUdCrtWgt()
*!*************************************************************
*!

FUNCTION lfvUdCrtWgt
m.Cartons  = Cartons
m.Weight   = Weight

IF llFirstUpd
  IF RECCOUNT() > 1 AND gfModalGen('QRM00000B40000','DIALOG',.F.,.F.,;
                'Do You Want To Copy Weight/Cartons to All stores') = 1
    lnRecNo = RECNO()
    REPLACE ALL Cartons WITH m.Cartons,;
                Weight  WITH m.Weight
    GOTO lnRecNo
  ENDIF
  llFirstUpd = .F.
ENDIF
=lfvUpsChrg()
RETURN .T.

*!*************************************************************
*! Name      : lfUdtBrow (C200484,1)
*! Developer : ALBERT RAIF (ALB)
*! Date      : 01/13/2003
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUdtBrow()
*!*************************************************************
*!
FUNCTION lfUdtBrow

PUBLIC llFirstUpd 
STORE .T. TO llFirstUpd
lcFields = "cMarker=IIF(RECNO()=lnOrdMark ,'>',' '):H=' ':R:1:W=.F.,;
cSelect :H=' ' :R,cAcc=IIF(Flag$'AO','*****',Account) :H='Acnt.' :R,"

lcFields = lcFields + "cOrd=IIF(Flag='O','******',Order) :H='Order#' :R,Custpo :H='Cust. P.O.' :R,cstore=IIF(Consol='Y',cConStore,Store) :H='Store' :R,Invoice :H='Inv.#' :R,"

lcFields = lcFields + "Ordered :H='Order' :R, Ship :H= 'Ship' :R, ShipAmt :H='Ship Amnt.' :13 :R,;
           Cartons :H='Cartons' :V=gfDoTriger('ARIINV',PADR('VUDCRTWGT',10)),;
           Weight  :H='Weight'  :V=gfDoTriger('ARIINV',PADR('VUDCRTWGT',10))"
lcFields = lcFields + IIF('AL' $ gcCmpModules,",PikTkt :H='PikTkt' :R,Picked :H='Picked' :R",'')


*!*************************************************************
*! Name      : lfGenPOfSO
*! Developer : Albert Raif (ALB)
*! Date      : 01/17/2003
*! Purpose   : Generate PO from SO
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Reference : C200491,1
*!*************************************************************
*! Example   :  =lfGenPOfSO()
*!*************************************************************
FUNCTION lfGenPOfSO
STORE SPACE(0) TO lcSoOrder

IF RECCOUNT(lcPoLine) <= 0
  IF !USED('ORDHDR')
    =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
  ENDIF
  IF !USED('ORDLINE')
    =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
  ENDIF

  IF gfModalGen('QRM00000B40000','DIALOG',.F.,.F.,;
                'Do You Want To Copy the PO lines from a Sales Order?') = 1
    IF lfOrdBrow()
      =lfUpdPoFSo(lcSoOrder)
    ENDIF
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfUpdPoFSo
*! Developer : Albert Raif (ALB)
*! Date      : 01/17/2003
*! Purpose   : Update the PO Temp files from SO order files
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Reference : C200491,1
*!*************************************************************
*! Example   :  =lfUpdPoFSo()
*!*************************************************************
FUNCTION lfUpdPoFSo
PARAMETER lcSoOrdNo

STORE '/' TO lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,gcBaseCurr)
lcDExSign = gfGetExSin(@lcDUntSin,gcBaseCurr)
llEditExRt = gfGetMemVar('LLEDITEXRA')
STORE 1 TO lnUnit1,lnUnit2
STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnOrder,lnAllocat,;
           lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5

lnPRate = gfchkrate('lnUnit1',gcBaseCurr,gdSysDate,llEditExRt,gcAct_Comp,.F.)
lnDRate = gfchkrate('lnUnit2',gcBaseCurr,gdSysDate,llEditExRt,gcAct_Comp,.F.)

llDyelot   = ALLTRIM(gfGetMemVar('M_DYELOT'))   = 'Y'
llMulCurr  = gfGetMemVar('llMulCurr')
lcMSLbl1   = gfGetMemVar('M_CMSLbl1')
lcMSLbl2   = gfGetMemVar('M_CMSLbl2')
lcMSLbl3   = gfGetMemVar('M_CMSLbl3')
lcMSLbl4   = gfGetMemVar('M_CMSLbl4')
lcMSLbl5   = gfGetMemVar('M_CMSLbl5')
lcISLbl1   = gfGetMemVar('M_CISLbl1')
lcISLbl2   = gfGetMemVar('M_CISLbl2')
lcISLbl3   = gfGetMemVar('M_CISLbl3')
lcISLbl4   = gfGetMemVar('M_CISLbl4')
lcISLbl5   = gfGetMemVar('M_CISLbl5')
lcIType1   = gfGetMemVar('M_cIType1')
lcIType2   = gfGetMemVar('M_cIType2')
lcIType3   = gfGetMemVar('M_cIType3')
lcIType4   = gfGetMemVar('M_cIType4')
lcIType5   = gfGetMemVar('M_cIType5')
llFDyelot  = gfGetMemVar('M_MATDYE')='Y'
SELECT ORDHDR
SET ORDER TO ORDHDR
IF SEEK('O'+lcSoOrdNo)
  SELECT (lcPOHdr)
  REPLACE cStyType   WITH 'P'               ,;
          Vendor     WITH laData[2]         ,;
          STATUS     WITH 'H'               ,;
          CDIVISION  WITH ORDHDR.cDivision  ,;
          cPurCode   WITH laPurCode[lnPurCode,2],;
          ENTERED    WITH gdSysDate         ,;
          COMPLETE   WITH ENTERED+90        ,;
          SHIPVIA    WITH ORDHDR.SHIPVIA    ,;
          CTERMCODE  WITH ORDHDR.CTERMCODE  ,;
          cWareCode  WITH ORDHDR.cWareCode  ,;
          cPriceCur  WITH ORDHDR.cCurrCode  ,;
          nPriceRat  WITH ORDHDR.nExrate    ,;
          nCurrUnit  WITH ORDHDR.nCurrUnit  ,;
          cDutyCur   WITH gcBaseCurr        ,;
          nDutyRat   WITH ORDHDR.nExrate    ,;
          nDCurUnit  WITH ORDHDR.nCurrUnit  ,;
          lMultiWare WITH (lnShpLoc=2)
  =gfwCodePop ( @laCodInfo, "SHIPVIA"   ,'L' )
  =gfwCodePop ( @laCodInfo, "CDIVISION" ,'L' )
  =gfwCodePop ( @laCodInfo, "CTERMCODE" ,'L' )
  =gfwCodePop ( @laCodInfo, "CPURCODE"  ,'L' )
  lnShpVia = ASUBSCRIPT(laShpVia,ASCAN(laShpVia,SHIPVIA),1)
  lnDiv    = ASUBSCRIPT(laDiv,ASCAN(laDiv,CDIVISION),1)
  lnTerms  = ASUBSCRIPT(laTerms,ASCAN(laTerms,CTERMCODE),1)
  SHOW GET lnShpVia
  SHOW GET lnDiv
  SHOW GET lnTerms
ENDIF
SELECT ORDLINE
SET ORDER TO ORDLINE
IF SEEK('O'+lcSoOrdNo)
  lnLineNo = 0
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+lcSoOrdNo
    lcStyle  = ORDLINE.Style
    lnLineNo = lnLineNo + 1
    SCATTER MEMVAR MEMO
    STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8
    SELECT (lcPoLine)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE LINENO    WITH lnLineNo                            ,;
            Vendor    WITH &lcPOHdr..Vendor                    ,;
            CStyType  WITH 'P'                                 ,;
            TranCd    WITH '1'                                 ,;
            Account   WITH ''                                  ,;
            TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
    =SEEK(lcStyle,'Style')
    FOR lnCount = 1 TO 5
      lcCount = STR(lnCount,1)
      DO CASE
        CASE lcIType&lcCount = 'P'
          lnCost&lcCount = IIF(&lcPoHdr..cPriceCur=Style.cPriceCur,TOTQTY*STYLE.nICost&lcCount,0)
          lnECost&lcCount= lnCost&lcCount &lcPExSign lnPRate &lcPUntSin lnUnit1
        CASE INLIST(lcIType&lcCount,'M','D')
          lnCost&lcCount = IIF(&lcPoHdr..cDutyCur=Style.cDutyCur,TOTQTY*STYLE.nICost&lcCount,0)
          lnECost&lcCount= lnCost&lcCount &lcDExSign lnDRate &lcDUntSin lnUnit2
        OTHERWISE
          lnCost&lcCount = TOTQTY*STYLE.nICost&lcCount
          lnECost&lcCount= lnCost&lcCount
      ENDCASE
      lnTCost&lcCount  = lnTCost&lcCount  + lnECost&lcCount
      lnTFCost&lcCount = lnTFCost&lcCount + lnCost&lcCount
    ENDFOR
    REPLACE nCost1  WITH nCost1 + lnCost1  ,;
            nCost2  WITH nCost2 + lnCost2  ,;
            nCost3  WITH nCost3 + lnCost3  ,;
            nCost4  WITH nCost4 + lnCost4  ,;
            nCost5  WITH nCost5 + lnCost5  ,;
            nECost1 WITH nECost1+ lnECost1 ,;
            nECost2 WITH nECost2+ lnECost2 ,;
            nECost3 WITH nECost3+ lnECost3 ,;
            nECost4 WITH nECost4+ lnECost4 ,;
            nECost5 WITH nECost5+ lnECost5
    IF llDispPric
      lnTotCost   = (nEcost1+nEcost2+nEcost3+nEcost4+nEcost5)/TOTQTY
      lnRotSub    = IIF(llStyMark,lnTotCost,lnSelPrice)
      lnGrosMrgn  = IIF(lnRotSub=0,0,((lnSelPrice - lnTotCost)/lnRotSub)*100)
      REPLACE nSelPrice WITH lnSelPrice ,;
        nGrosMrgn WITH lnGrosMrgn
      STORE 0 TO lnSelPrice,lnAllQty
    ENDIF
    lnOrder   = lnOrder   + TOTORD
    lnAllocat = lnAllocat + TOTQTY
  ENDSCAN  
  SELECT (lcPOHdr)
  REPLACE nStyOrder WITH lnAllocat ,;
          TotOrd    WITH lnOrder   ,;
          NFCOST1   WITH lnTFCost1 ,;
          NFCOST2   WITH lnTFCost2 ,;
          NFCOST3   WITH lnTFCost3 ,;
          NFCOST4   WITH lnTFCost4 ,;
          NFCOST5   WITH lnTFCost5 ,;
          NICOST1   WITH lnTCost1  ,;
          NICOST2   WITH lnTCost2  ,;
          NICOST3   WITH lnTCost3  ,;
          NICOST4   WITH lnTCost4  ,;
          NICOST5   WITH lnTCost5

ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Albert Raif (ALB)
*! Date      : 01/17/2003
*! Purpose   : Brows Salse Orders records to select 
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Reference : C200491,1
*!*************************************************************
*! Example   :  =lfOrdBrow()
*!*************************************************************
FUNCTION lfOrdBrow

SELECT ORDHDR
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
lcBrFields = lfDefnFlds()               && Define lcBrFields

SET ORDER TO TAG ORDHDR IN OrdHdr
PRIVATE lcOldFilt
lcOldFilt = FILTER()
lcOrdType = 'O'
SET FILTER TO
lcSoOrder = IIF(ARIABROW('lcOrdType',"Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
              OrdHdr.Order,SPACE(6))
SET FILTER TO &lcOldFilt
SET RELATION OFF INTO CUSTOMER
RETURN !EMPTY(lcSoOrder)

*!**************************************************************************
*! Name      : lfDefnFlds
*! Developer : Albert Raif (ALB)
*! Date      : 01/17/2003
*! Purpose   : Define the lcBrFields variable.
*!**************************************************************************
*! Reference : C200491,1
*!**************************************************************************
*! Example   : =lfDefnFlds()
*!**************************************************************************
*
FUNCTION  lfDefnFlds
PRIVATE lcSOBrowFld
*lcBrFields
lcSOBrowFld = [Order:H="Order#",Status:1:H="Status",Account:H="Acct",]+;
             [Store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [Dept:H="Department",MultiPo=IIF(MultiPo,'Yes','No'):H="Multi PO",]+;
             [CustPo=IIF(MultiPO,'*Multi_PO*',CustPO):H="Cust. P.O#",]+;
             [Bulk:H="Bulk",Multi:H="Multi",cFacCode:H="Factor Code",]+;
             [Rep1:H="Sales Rep1",Comm1:H="Commission 1",Rep2:H="Sales Rep2",]+;
             [Comm2:H="Commission 2",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",ShipAmt:H="Ship.Amt.",]+;
             [Book:H="Book.Qty.",BookAmt:H="Book.Amt.",]+;
             [Cancel:H="Cancel.Qty.",CancelAmt:H="Cancel.Amt."]
lcSOBrowFld = lcSOBrowFld + [,Entered:H="Entered",Start:H="Start",Complete:H="Complete",]+;
             [Cancelled:H="Cancelled",Disc:H="Disc.",]+;
             [cWareCode:H="Warehouse",cAdd_User:H="User",dAdd_Date:H="Date",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [lcShipVia=gfCodDes(ShipVia,'SHIPVIA'):H="ShipVia",]+;
             [Note1:6:H="Notes",Note2:6:H="Notes 2"]
IF llMulCurr
  lcSOBrowFld = lcSOBrowFld + [,cCurrCode:H="Currency",nExRate:H="Ex. Rate",nCurrUnit:H="Unit"]
ENDIF             
RETURN lcSOBrowFld
*-- End of lpDefnFlds

*!*************************************************************
*! Name      : lfvPoCust
*! Developer : Albert Raif (ALB)
*! Date      : 01/27/2003
*! Purpose   : Validate the customer code
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Reference : C200492,1
*!*************************************************************
*! Example   :  =lfvPoCust()
*!*************************************************************
FUNCTION lfvPoCust
PARAMETERS lcRetVal
PRIVATE lcCustCode,lcOldAlias, llCustUsed
llCustUsed = .F.
lcRetVal = .T.
lcOldAlias = ALIAS()
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  llCustUsed = .T.
ENDIF

IF TYPE('laUsrFields') = 'U'
  RETURN lcRetVal
ENDIF

lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CCUSTCODE'),1)
IF TYPE('laOGFxFlt') = 'U'
  lcCustCode = laUsrFields[lnFieldPos,6]
ELSE
  lnOGFildPs = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CCUSTCODE'),1)
  lcCustCode = laOGFxFlt[lnOGFildPs,6]
ENDIF


IF (!EMPTY(lcCustCode) .AND. !SEEK('M'+lcCustCode,'CUSTOMER'))
  SELECT CUSTOMER
  DO CUSBROWM WITH lcCustCode
  laUsrFields[lnFieldPos,6] = lcCustCode
  laOGFxFlt[lnOGFildPs,6]   = lcCustCode
  SHOW GET CCUSTCODE
ENDIF

IF llCustUsed 
  SELECT CUSTOMER
  USE
ENDIF
RETURN lcRetVal

*!*************************************************************
*! Name      : lfCustFlt
*! Developer : Albert Raif (ALB)
*! Date      : 01/27/2003
*! Purpose   : Validate the customer code
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Reference : C200492,1
*!*************************************************************
*! Example   :  =lfCustFlt()
*!*************************************************************
FUNCTION lfCustFlt

lcAlias = ALIAS()

IF SEEK('P'+laData[2] , 'POSHDR') AND IIF(EMPTY(POSHDR.CCUSTCODE),.F.,OrdHdr.ACCOUNT <> POSHDR.CCUSTCODE)
  select (lcOrderLin)
  DELETE
ENDIF

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfShpStrLn
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2003
*! Purpose   : Ship/UnShip Invoice Line
*!*************************************************************
*! Calls     : lfRefresh(),lfUpCnsLine()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfShpStrLn()
*!*************************************************************
*!C200499,1
FUNCTION lfShpStrLn
PRIVATE lnTaxQty,lcInvKey

SELECT (lcInvLine)
lcInvKey=Account+Order+Store+PikTkt+STR(LineNo,6)
SET KEY TO

IF SEEK('O'+&lcInvLine..Order,'ORDHDR') AND ORDHDR.Multi = 'Y' 
  SELECT (lcInvHdr)
  lcKey = &lcInvHdr..Account+&lcInvHdr..Order+&lcInvHdr..Store+&lcInvHdr..PikTkt
  lnStores = 0
  SCAN FOR Order = &lcInvLine..Order AND Consol = 'N'
    lnStores = lnStores + 1
  ENDSCAN
  =SEEK(lcKey,lcInvHdr)
  IF lnStores > 1  
    IF gfModalGen('QRM00000B32000','ALERT',.F.,.F.,"Do you want to "+IIF(!(&lcInvLine..lShip),"Ship","UnShip")+" Style # "+;
      &lcInvLine..Style+" for all the stores ?") = 1
      llShip = &lcInvLine..lShip
      SELECT (lcInvLine)
      lcStyle = &lcInvLine..Style
      lcPack_id = &lcInvLine..Pack_id
      *B123600,1 MHM 08/31/2004 Define Color size version  [Start]
      lcColor_id = &lcInvLine..cpkcolor
      lcSize_id  = &lcInvLine..cpcksize
      lcVrsn_id  = &lcInvLine..cpkversion
      *B123600,1 MHM 08/31/2004  [End]
      
      lcPrvOrd = ORDER()
      SET ORDER TO Styles
      llAll  = .F.
      IF !EMPTY(&lcInvLine..Pack_ID) 
        llAll = gfModalGen('QRM00000B32000','ALERT',.F.,.F.,;
                     "This style belongs to pack "+&lcInvLine..Pack_id+"."+;
                     IIF(!llShip ,"Ship","UnShip")+" the whole pack ?") = 1
      ENDIF             
      SELECT (lcInvLine)

      lcExpr   = 'Account+Order+PikTkt+Style  = &lcInvHdr..Account+ &lcInvHdr..Order+;
                                              &lcInvHdr..PikTkt + lcStyle AND;
                                              IIF(llShip,lShip,!lShip)'
      lcPkExpr = 'Account+Order+PikTkt+Pack_id= &lcInvHdr..Account+&lcInvHdr..Order+;
                                                &lcInvHdr..PikTkt +lcPack_id;
                                                AND IIF(llShip,lShip,!lShip)'   

      lcStore = " "

      *B125927,1 NNA 01/07/2005 (Begin) Scan for !empty(Store) to Get on the First Store
      *SCAN FOR IIF(llAll,&lcPkExpr,&lcExpr) 
      SCAN FOR IIF(llAll,&lcPkExpr,&lcExpr) AND !EMPTY(STORE)
      *B125927,1 NNA (End)

        IF !EMPTY(&lcInvLine..Pack_ID) AND !llAll
          EXIT
        ENDIF 
        
        *B123600,1 MHM 08/31/2004 Check if full pack Index found or not  [Start]
        IF llAll AND !((lcColor_id = &lcInvLine..cpkcolor) AND (lcSize_id  = &lcInvLine..cpcksize) AND (lcVrsn_id  = &lcInvLine..cpkversion))
          LOOP
        ENDIF
        *B123600,1 MHM  [End]
        
        IF Store <> lcStore 
          lcStore  = Store
          SELECT (lcInvHdr)
          =SEEK(&lcInvLine..Account+&lcInvLine..Order+&lcInvLine..Store+&lcInvLine..PikTkt)
          SCATTER MEMVAR MEMO
        ENDIF  
        =lfClearLn()
      ENDSCAN
      SELECT (lcInvLine)
      SET ORDER TO &lcPrvOrd 
      SELECT (lcInvHdr)
      =SEEK(lcKey,lcInvHdr)
      IF !EOF()
       SKIP
       SKIP -1 
      ENDIF
    ELSE
      llShip = &lcInvLine..lShip
      IF !EMPTY(&lcInvLine..Pack_ID) AND gfModalGen('QRM00000B32000','ALERT',.F.,.F.,;
         "This style belongs to pack "+&lcInvLine..Pack_id+"."+;
         IIF(!llShip ,"Ship","UnShip")+" the whole pack ?") = 1
        SELECT (lcInvLine)
        lcPack_id = &lcInvLine..Pack_id
        *B123600,1 MHM 08/31/2004 Define Color size version  [Start]
        lcColor_id = &lcInvLine..cpkcolor
        lcSize_id  = &lcInvLine..cpcksize
        lcVrsn_id  = &lcInvLine..cpkversion
        *B123600,1 MHM 08/31/2004  [End]
        SCAN FOR Account+Order+Store+PikTkt+Pack_id= &lcInvHdr..Account+&lcInvHdr..Order+;
                                                     &lcInvHdr..Store+&lcInvHdr..PikTkt+;
                                                     lcPack_id AND IIF(llShip,lShip,!lShip)
          *B123600,1 MHM 08/31/2004 Check if full pack Index found or not  [Start]
          IF !((lcColor_id = &lcInvLine..cpkcolor) AND (lcSize_id  = &lcInvLine..cpcksize) AND (lcVrsn_id  = &lcInvLine..cpkversion))
            LOOP
          ENDIF
          *B123600,1 MHM 08/31/2004  [End]
          
          =lfClearLn()
        ENDSCAN  

      *B125927,1 NNA 01/07/2005 (Begin) if there is no pack for this order.
      ELSE
        SELECT (lcInvLine)
        =lfClearLn()
      *B125927,1 NNA (End)
  
      ENDIF
    ENDIF  
  ELSE
    =lfClearLn()
  ENDIF  
ELSE
  =lfClearLn()
ENDIF

SELECT (lcInvLine)
IF &lcInvHdr..Consol = 'Y' AND !EMPTY(&lcInvHdr..cConStore)
  SET KEY TO laData[4]+laData[1]+cConStore+laData[3]
ELSE
  SET KEY TO laData[4]+laData[1]+laData[5]+laData[3]
ENDIF
=SEEK(lcInvKey)
SHOW GETS WINDOW (lcWinCh42) ONLY
SHOW GETS WINDOW (lcWinCh44) ONLY
SHOW GETS WINDOW (lcWinCh45) ONLY
=lfRefresh(lcWinCh44)
IF m.TotQty = 0
  SHOW GET pbShipLine,1 ENABLE PROMPT '\<Ship'
ELSE
  SHOW GET pbShipLine,1 ENABLE PROMPT '\<Clear'
ENDIF

*!*************************************************************
*! Name      : lfClearLn
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2003
*! Purpose   : Ship/UnShip Invoice Line
*!*************************************************************
*! Calls     : lfRefresh(),lfUpCnsLine()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfClearLn()
*!*************************************************************
*!C200499,1
FUNCTION lfClearLn

*B125927,1 NNA 01/07/2005 (Begin) Two Variables To hold Recno for lcInvLine and lcInvhdr
STORE 0 TO lnRecLine,lnRecHdr
*B125927,1 NNA (End)

IF SEEK('O'+Order+STR(LineNO,6),lcOrdCanLn)
  SELECT (lcOrdCanLn)
  BLANK
  DELETE
  SELECT (lcInvLine)
  m.lBackOrd = .T.
  llCnRemain = .F.
  SHOW GET llCnRemain DISABLE
  SHOW GET pbCnRemain DISABLE
ENDIF

SELECT (lcInvLine)

*B125927,1 NNA 01/07/2005 (Begin) Scatter Current Record to Update its Qty.
SCATTER MEMVAR MEMO
*B125927,1 NNA (End)

m.lShip = !(&lcInvLine..lShip)

STORE IIF(TotQty=0,IIF(Flag='B',Pik1,Book1),0) TO m.Qty1
STORE IIF(TotQty=0,IIF(Flag='B',Pik2,Book2),0) TO m.Qty2
STORE IIF(TotQty=0,IIF(Flag='B',Pik3,Book3),0) TO m.Qty3
STORE IIF(TotQty=0,IIF(Flag='B',Pik4,Book4),0) TO m.Qty4
STORE IIF(TotQty=0,IIF(Flag='B',Pik5,Book5),0) TO m.Qty5
STORE IIF(TotQty=0,IIF(Flag='B',Pik6,Book6),0) TO m.Qty6
STORE IIF(TotQty=0,IIF(Flag='B',Pik7,Book7),0) TO m.Qty7
STORE IIF(TotQty=0,IIF(Flag='B',Pik8,Book8),0) TO m.Qty8
IF INLIST(laSetups[7,2],'F','L') AND TotQty=0
  STORE MAX(MIN(m.Qty1,StyDye.Stk1),0) TO m.Qty1
  STORE MAX(MIN(m.Qty2,StyDye.Stk2),0) TO m.Qty2
  STORE MAX(MIN(m.Qty3,StyDye.Stk3),0) TO m.Qty3
  STORE MAX(MIN(m.Qty4,StyDye.Stk4),0) TO m.Qty4
  STORE MAX(MIN(m.Qty5,StyDye.Stk5),0) TO m.Qty5
  STORE MAX(MIN(m.Qty6,StyDye.Stk6),0) TO m.Qty6
  STORE MAX(MIN(m.Qty7,StyDye.Stk7),0) TO m.Qty7
  STORE MAX(MIN(m.Qty8,StyDye.Stk8),0) TO m.Qty8
ENDIF
STORE Price TO m.Price
STORE m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 TO m.TotQty
STORE lBackOrd TO m.lBackOrd

lnCartons  = &lcInvHdr..nCartons + IIF(Style.Qty_Ctn>0,(m.TotQty-TotQty)/Style.Qty_Ctn,0)
m.Weight   = m.Weight  + (m.TotQty-TotQty)*Style.nStyWeight

IF m.TotQty =0
  m.Cartons = CEILING(lnCartons)
ELSE
  m.Cartons = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
ENDIF    

m.ShipAmt  = m.ShipAmt + (m.TotQty-TotQty)*Price
m.Ship     = m.Ship    + (m.TotQty-TotQty)
m.Discount = -m.ShipAmt * m.DiscPcnt/100

*B125927,1 NNA 01/07/2005 (Begin) Hold the current index and Record No. from (lcInvLine) and (lcInvHdr)
lcCurOrder = ORDER(lcInvLine)
lnRecLine  = Recno(lcInvLine)
lnRecHdr   = Recno(lcInvHdr)
*B125927,1 NNA (End)

=IIF(llUpCnsLine,lfUpCnsLine() AND lfUpCnsCtn(&lcInvHdr..Cartons,m.Cartons),.T.)

*B125927,1 NNA 01/07/2005 (Begin) Return to the previous index and Record No. in (lcInvLine)
*B125927,1 NNA            because they changed in function lfUpCnsLine()
SET ORDER TO &lcCurOrder IN (lcInvLine)
IF BETWEEN(lnRecLine,1,RECCOUNT(lcInvLine))
  GOTO lnRecLine IN (lcInvLine)
ENDIF
IF BETWEEN(lnRecHdr,1,RECCOUNT(lcInvHdr))
  GOTO lnRecHdr IN (lcInvHdr)
ENDIF
*B125927,1 NNA (End)

lnTaxQty = 0
IF laSetups[9,2]='Y' AND llIsEngland
  FOR lnSize = 1 TO Scale.Cnt
    lnTaxQty = lnTaxQty + IIF(BETWEEN(lnSize,Style.nTaxBreak,8),;
    IIF(TotQty=0,EVAL('m.Qty'+STR(lnSize,1)),EVAL('-Qty'+STR(lnSize,1))),0)
  ENDFOR
ENDIF
m.nTaxdue = &lcInvHdr..nTaxDue + IIF(lTaxable,(m.TotQty-TotQty)*Price,0)
=RLOCK()
GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,lBackOrd
UNLOCK
STORE .T. TO llCUpdate
SELECT (lcInvHdr)
=RLOCK()
REPLACE CARTONS    WITH m.Cartons    ,;
        nCartons   WITH lnCartons    ,;
        WEIGHT     WITH m.Weight     ,;
        SHIPAMT    WITH m.ShipAmt    ,;
        SHIP       WITH m.Ship       ,;
        Discount   WITH m.Discount   ,;
        lCompUps   WITH !llIsEngland ,;
        nMerchTax  WITH nMerchTax +   ;
        IIF(laSetups[9,2]='Y' AND llIsEngland,lnTaxQty*m.Price*m.nTaxRate/100,0) ,;
        Tax_Amt    WITH IIF(llIsEngland,nChrgTax+nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100,TAX_AMT) ,;
        Cod_Amt    WITH IIF(Cod_Flag='Y',IIF(llIsEngland,ShipAmt+nCharges+;
                           Tax_Amt+Discount,Cod_Amt),0) ,; 
        TotalChg   WITH IIF(llIsEngland,ShipAmt+nCharges+Tax_Amt+Discount,TotalChg) ,;
        nTaxdue    WITH m.nTaxdue
UNLOCK
SCATTER MEMVAR FIELDS Tax_Amt,Cod_Amt,TotalChg

SELECT (lcInvLine)
REPLACE &lcInvLine..lShip WITH m.lShip 


*!*************************************************************
*! Name      : lfvSRLog
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/27/2003
*! Purpose   : Validate the User_Id code
*!*************************************************************
*! Calls     : ARIABROW
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Reference : C200494,1
*!*************************************************************
*! Example   :  =lfvSRLog()
*!*************************************************************
FUNCTION lfvSRLog
PARAMETERS lcRetVal
PRIVATE lcCustCode,lcOldAlias, llCustUsed
llCustUsed = .F.
lcRetVal = .T.
DECLARE laBrowArr[1]
STORE '' TO laBrowArr
lcOldAlias = ALIAS()
IF !USED('SYUUSER')
  =gfOpenFile(gcDataDir+'SYUUSER','Cuser_id','SH')
  llCustUsed = .T.
ENDIF

IF TYPE('laUsrFields') = 'U'
  RETURN lcRetVal
ENDIF

lnFieldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CLOGIN    '),1)
IF TYPE('laOGFxFlt') = 'U'
  lcCustCode = laUsrFields[lnFieldPos,6]
ELSE
  lnOGFildPs = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CLOGIN    '),1)
  lcCustCode = laOGFxFlt[lnOGFildPs,6]
ENDIF


IF (!EMPTY(lcCustCode) .AND. !SEEK(lcCustCode,'SYUUSER'))
  SELECT SYUUSER

  PRIVATE laBrowVer,lcBrTitle
  DIME laBrowVer[1]
  lcBrTitle  = 'Users'            
  lcBrFields = "CUser_Id          :R :H='User Id'      :10," +;
               "cusr_name         :R :H='Name'         :35," +;
               "cusr_grup         :R :H='User Group'   :4" 
   =gfBrows('',"CUser_Id","laBrowVer",lcBrTitle)
  
  lcCustCode= laBrowVer[1]
  IF !EMPTY(lcCustCode)
    laUsrFields[lnFieldPos,6] = lcCustCode
    laOGFxFlt[lnOGFildPs,6]   = lcCustCode
    SHOW GET Clogin
  ELSE
    laUsrFields[lnFieldPos,6] = ''
    laOGFxFlt[lnOGFildPs,6]   = ''
  ENDIF  
ENDIF

IF llCustUsed 
  SELECT SYUUSER
  USE
ENDIF
RETURN lcRetVal

*!*************************************************************
*! Name      : lfSETFLG
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2003
*! Purpose   : Ship/UnShip Invoice Line
*!*************************************************************
*! Calls     : lfRefresh(),lfUpCnsLine()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfSETFLG()
*!*************************************************************
*!C200499,1
FUNCTION lfSETFLG

REPLACE &lcInvLine..lShip WITH llShip

*:**************************************************************************
*:* Name        : lfIGRDSTRS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/05/2003
*:* Purpose     : Ignore non-active stores
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfIGRDSTRS()
*:***************************************************************************
*C200507,1 TMI 
FUNCTION lfIGRDSTRS
PRIVATE llRet    
llRet = .F.

*C200507,1 TMI [Start] Define a variable for GMA 
lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
lcIgrdStrs = &lcSVarFle..lcIgrdStrs
*C200507,1 [End]

IF CUSTOMER.STATUS # 'A'
  IF !ALLTRIM(CUSTOMER.STORE) $ lcIgrdStrs
    lcIgrdStrs = lcIgrdStrs + ALLTRIM(CUSTOMER.STORE) + ','
  ENDIF
  llRet = .T.
  SKIP IN ORDLINE    
ENDIF

*C200507,1 TMI [Start] Define a variable for GMA 
REPLACE &lcSVarFle..lcIgrdStrs WITH lcIgrdStrs
*C200507,1 [End]

RETURN llRet
*-- end of lfIGRDSTRS.

*:**************************************************************************
*:* Name        : lfSHOWIGST
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/05/2003
*:* Purpose     : Show ignored stores
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSHOWIGST()
*:***************************************************************************
*C200507,1 TMI 
FUNCTION lfSHOWIGST
PRIVATE lcSt

*C200507,1 TMI [Start] Define a variable for GMA 
lcSVarFle = lcConsInvH + 'A'
GO TOP IN (lcSVarFle)
lcIgrdStrs = &lcSVarFle..lcIgrdStrs
*C200507,1 [End]

IF !EMPTY(lcIgrdStrs)
  lcIgrdStrs = SUBSTR(lcIgrdStrs,1,LEN(lcIgrdStrs)-1)
  lcSt = lcIgrdStrs+IIF(OCCURS(',',lcIgrdStrs)=0,' is ','s are ')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The Store'+IIF(OCCURS(',',lcIgrdStrs)=0,' ','s ')+;
                                            lcIgrdStrs+IIF(OCCURS(',',lcIgrdStrs)=0,' is ',' are ')+;
                                           'non-active, all related order lines are ignored.')
ENDIF
*-- end of lfSHOWIGST.

*:**************************************************************************
*:* Name        : lfDisabMsg
*:* Developer   : Hend Ghanem (HBG)
*:* Date        : 03/05/2003
*:* Purpose     : Disable message "Invalid Shipper Or Ship To Zone"
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSHOWIGST()
*:***************************************************************************
*C200518,1
FUNCTION lfDisabMsg

RETURN .T.

*:**************************************************************************
*:* Name        : lfAddCstPk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Define pad line for "Customer Packing info" for style screen
*:***************************************************************************
*:* Called from : icstyle.prg
*:***************************************************************************
*C200519,1
FUNCTION lfAddCstPk
PRIVATE lnBarNo
lnBarNo = CNTBAR('_lPopOpt') 
DEFINE BAR lnBarNo+1 OF _lPopOpt PROMPT '\-' SKIP FOR .T.
DEFINE BAR lnBarNo+2 OF _lPopOpt PROMPT 'Customer Pac\<king Info' SKIP FOR laScrMode[1]
ON SELECTION BAR lnBarNo+2 OF _lPopOpt DO gfDoTriger WITH "ICSTYLE",PADR("CSTPKINF",10)

IF !EMPTY(lcfolder)
  *--Define temp file to hold temp names for GMA 
  PRIVATE lcSvVrFle
  lcSvVrFle = lcfolder + 'A'
  CREATE CURSOR (lcSvVrFle) (cTmpName C(8),cTempCur C(8))
  INSERT INTO (lcSvVrFle) (cTmpName,cTempCur) ;
                   VALUES (gfTempName(),gfTempName())
ENDIF  

*-- end of lfPoQOrdPD.

*:**************************************************************************
*:* Name        : lfAddCstPk                                      *C200519,1 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/08/2003
*:* Purpose     : Open the screen "Customer Packing info" 
*:***************************************************************************
*:* Called from : icstyle.prg
*:***************************************************************************
FUNCTION lfCstPkInf
PRIVATE lnAlias,lcTempCur,lcStyPkTtl,lnStyPk,lcSlctd,lcDesc,lcSlctCust,lcCusts;
        lnWidth,lnDepth,lnLnght,lnQtyCrt,lnWghtUnt,laFlds,lcOldValue,lcTmpName,laBrow,llBrowse,;
        lcUpdStat
        
PRIVATE lcSvVrFle,lcCstPkInf
lcSvVrFle = lcfolder + 'A'

lcCstPkInf = &lcSvVrFle..cTmpName
lcTempCur  = &lcSvVrFle..cTempCur
        
DIMENSION laBrow[8]
laBrow = ''
lcOldValue = ''
llBrowse = .F.
lcTmpName = gfTempName()

lnAlias = SELECT()

=lfOpnFls()      &&Open needed files
=lfCrTmpFls()    &&Create needed temp files

STORE 0 TO lnWidth,lnDepth,lnLnght,lnQtyCrt,lnWghtUnt
lcStyPkTtl = 'Style  -Color'
lcSlctd    = lcMajor+lcSepart+lcNonMjr
lcDesc     = STYLE.DESC
lcSlctCust = '' 
lcSavCust  = ''
lnQtyCrt   = STYLE.QTY_CTN   
lnWghtUnt  = STYLE.NSTYWEIGHT
lnTotWgt =  lnQtyCrt * lnWghtUnt

lcUpdStat = IIF(laScrMode[2],'DISABLE','ENABLE')

SELECT CUSTOMER
GO TOP

*-- Opens "Customer Packing Info" screen
PUSH KEY
DO (gcScrDir+'IC\ICPKINF.SPX')
ON KEY LABEL LEFTMOUSE
ON KEY LABEL MOUSE

USE IN (lcTmpName)

POP KEY
SELECT (lnAlias)
*-- end of lfCstPkInf.

*:**************************************************************************
*:* Name        : lfOpnFls                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Open needed files
*:***************************************************************************
FUNCTION lfOpnFls
IF !USED('SPCK_HDR')
  =gfOpenFile(gcDataDir+'SPCK_HDR','SPCK_HDRVR','SH')  
ENDIF
IF !USED('SPCK_LIN')
  =gfOpenFile(gcDataDir+'SPCK_LIN','SPCK_LINVR','SH')  
ENDIF
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF  
IF !USED('CSTPKINF')
  =gfOpenFile(gcDataDir+'CSTPKINF','CSTPKINF','SH')
ENDIF  
IF USED(lcTmpName)
  USE IN (lcTmpName)
ENDIF
USE (gcDataDir+'STYLE') ORDER 'STYLE' AGAIN ALIAS (lcTmpName) IN 0
*-- end of lfOpnFls.

*:**************************************************************************
*:* Name        : lfCrTmpFls                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/08/2003
*:* Purpose     : Create needed temp files
*:**************************************************************************
FUNCTION lfCrTmpFls
PRIVATE laFlds,lnI

IF !USED(lcCstPkInf)
  DIMENSION laFlds[1,4]
  laFlds = .F.
  SELECT CSTPKINF
  =AFIELDS(laFlds)
  lnI = ALEN(laFlds,1)+1
  DIMENSION laFlds[lnI,4]
  laFlds[lnI,1] = 'STATUS'
  laFlds[lnI,2] = 'C'
  laFlds[lnI,3] = 1
  laFlds[lnI,4] = 0
  
  CREATE DBF (gcWorkDir+lcCstPkInf) FROM ARRAY laFlds
  INDEX ON TYPE+ACCOUNT+STYLE TAG (lcCstPkInf)
  INDEX ON TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION TAG (lcCstPkInf+'P')
  
  CREATE DBF (gcWorkDir+lcTempCur) (ACCOUNT C(5))
  INDEX ON ACCOUNT TAG (lcTempCur)
ENDIF

*-- end of lfCrTmpFls.
*:**************************************************************************
*:* Name        : lfvCustmer                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/13/2003
*:* Purpose     : Select one or more customers
*:***************************************************************************
*:* Called from : GMAMAIN.lfCstPkInf
*:***************************************************************************
*C200519,1 
FUNCTION lfvCustmer
PRIVATE lnRecno,lcBrowFlds,lcOldCst
lcOldCst = LEFT(lcSlctCust,5)
*-- Open Browse to select customer(s)
lcBrowFlds = [ACCOUNT   :H = 'Account'  :10,]  + ;
      	     [BTNAME    :H = 'Name'     :25 ] 
SELECT CUSTOMER
IF gfRange(lcBrowFlds,lcTempCur,"ACCOUNT",["M" FOR STATUS="A"],"","","!!!!!")
  lcSlctCust = lfGetCsts()
  lcSavCust = lcSlctCust
  IF lnStyPk = 2
    lcSlctCust = PADR(lcSlctCust,5)
    lcSlctd = IIF(lcSlctCust == lcOldCst,lcSlctd,'')
  ENDIF  
  *C200519,5  TMI [Start] remove this line to Fix the bug 'String is too long to fit' , also remove the 
  *C200519,5  TMI         variabel "lcSlctCust" from the screen
  *SHOW GET lcSlctCust
  *C200519,5  TMI [End  ] 
  SHOW GET lcSlctd
  
  *--Update Account/style-pack qtys
  lnW = IIF('*'$SUBSTR(lcSlctd,lnStyleWid+2) , lnStyleWid , 19 )          
  IF !EMPTY(lcSlctd) .AND. SEEK(SUBSTR(lcSlctd,1,lnW),lcTmpName)
    llBrowse = .F.
    =lfvStyPkBr(.T.)
  ENDIF

ENDIF  
*-- end of lfvCustmer.

*:**************************************************************************
*:* Name        : lfGetCsts                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Get selected customers
*:***************************************************************************
FUNCTION lfGetCsts
PRIVATE lnSlct,lnRecno,lcCusts
lnSlct = SELECT()
lcCusts = ''

SELECT &lcTempCur
lnRecno = RECNO(lcTempCur)

GO TOP
lcCusts = ''
SCAN
  lcCusts = lcCusts + &lcTempCur..ACCOUNT + ','
ENDSCAN  
lcCusts = SUBSTR(lcCusts,1,LEN(lcCusts)-1)
  
IF BETWEEN(lnRecno,1,RECCOUNT(lcTempCur))
  GOTO (lnRecno)
ENDIF

SELECT (lnSlct)
RETURN lcCusts
*-- end of lfGetCsts.
*:**************************************************************************
*:* Name        : lfvStyPk                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2003
*:* Purpose     : Change browse contents of Style-Color field depends on this fiels choice
*:***************************************************************************
*C200519,1 
FUNCTION lfvStyPk

IF lnStyPk = 1

  lcStyPkTtl = 'Style  -Color'
  lcSlctd = lcMajor+lcSepart+lcNonMjr
  lcDesc  = STYLE.DESC
  lnQtyCrt = STYLE.QTY_CTN   
  lnWghtUnt = STYLE.NSTYWEIGHT
  lnWidth = STYLE.NMPCUBE   
  lnDepth = STYLE.NMPDEPTH  
  lnLnght = STYLE.NMPDIM  
  lcSlctCust = lcSavCust  
  *C200519,5  TMI [Start] Define the variable lnW
  lnW = IIF('*'$SUBSTR(lcSlctd,lnStyleWid+2) , lnStyleWid , 19 )
  *C200519,5  TMI [End  ] 
  IF !EMPTY(lcSlctd) .AND. SEEK(SUBSTR(lcSlctd,1,lnW),lcTmpName)
    llBrowse = .F.
    =lfvStyPkBr(.T.)
  ENDIF 
  
ELSE

  lcStyPkTtl = 'Pack ID'
  lcSlctd = ' '
  lcDesc  = ' '
  lnQtyCrt = 0
  lnWghtUnt = 0
  lnWidth = 0
  lnDepth = 0
  lnLnght = 0 
  lcSlctCust = PADR(lcSavCust , 5 )

ENDIF  

SHOW GETS

*-- end of lfStyPk.

*:**************************************************************************
*:* Name        : lfPackWght                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2003
*:* Purpose     : Get pack weight from Pack_lin file
*:***************************************************************************
FUNCTION lfPackWght
PRIVATE lnSlct,lcAccnt,lnWght
lnSlct = SELECT()
lnWght = 0
SELECT SPCK_LIN
lcAccnt = PADR(lcSlctCust,5)
IF SEEK('P'+lcAccnt+lcSlctd),'SPCK_LIN')
  SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = 'P'+lcAccnt+lcSlctd
    =SEEK(SPCK_LIN.STYLE,lcTmpName)
    lnWght = lnWght + SPCK_LIN.TOTQTY * &lcTmpName..NSTYWEIGHT
  ENDSCAN
ENDIF
SELECT (lnSlct)
RETURN lnWght
*-- end of lfPackWght.

*:**************************************************************************
*:* Name        : lfvStyPkBr                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/18/2003
*:* Purpose     : Browse styles / or Packs for the selected Account
*:***************************************************************************
* The browse button to browse styles and Packs , 
* for packs it browses those of first customer selectd
* For styles browse style majors
* if style is selected then browse all colors ,  
* if escape is pressed then add '*' in place of NonMajor part

FUNCTION lfvStyPkBr
PARAMETERS llCstVld
PRIVATE lcOldSlctd,lnW,llFound,lcAcct,lcSavOrd,lcSavOrdTm

IF EMPTY(lcSlctd)
  lcSlctd = ' '
  lcDesc  = ' '
  STORE 0 TO lnQtyCrt, lnWghtUnt, lnWidth, lnDepth, lnLnght 
ENDIF

lcOldSlctd = lcSlctd
llBrowse = IIF('?'$lcSlctd,.T.,llBrowse)
llFound = .F.

lcSavOrd   = ORDER('CSTPKINF')
lcSavOrdTm = ORDER(lcCstPkInf)

IF lnStyPk = 1  
  SET ORDER TO (lcCstPkInf) IN (lcCstPkInf)
  IF llBrowse
    llFound = lfStyBrow()
  ELSE
    IF !EMPTY(lcSlctd) .AND. lcOldValue # lcSlctd
      lcSlctd = IIF('*'$SUBSTR(lcSlctd,lnStyleWid+2,lnColorWid),;
                SUBSTR(lcSlctd,1,lnStyleWid)+lcSepart+REPL('*',lnColorWid) , lcSlctd )
      lnW = IIF('*'$SUBSTR(lcSlctd,lnStyleWid+2) , lnStyleWid , 19 )          
      IF !SEEK(SUBSTR(lcSlctd,1,lnW),lcTmpName)
        *- Browse to select a style-color  
        llFound = lfStyBrow()
      ELSE
        llFound = .T.
      ENDIF
    ENDIF
  ENDIF
  IF llFound OR llCstVld
    *-- Retrive saved data for 1st account
    lcAcct = PADR(lcSlctCust,5)
    SET ORDER TO TAG (lcCstPkInf) IN (lcCstPkInf)
    SET ORDER TO TAG CSTPKINF IN CSTPKINF
    lnW = IIF('*'$SUBSTR(lcSlctd,lnStyleWid+2) , lnStyleWid , 19 )          
    IF SEEK('S'+lcAcct+SUBSTR(lcSlctd,1,lnW),lcCstPkInf)
      SELECT &lcCstPkInf
      lnQtyCrt   = &lcCstPkInf..NINPACK      
      lnWghtUnt  = &lcCstPkInf..NMPWEIGHT 
      lnTotWgt   = lnQtyCrt * lnWghtUnt
      lnWidth    = &lcCstPkInf..NMPCUBE      
      lnDepth    = &lcCstPkInf..NMPDEPTH  
      lnLnght    = &lcCstPkInf..NMPDIM      
    ELSE
      IF SEEK('S'+lcAcct+SUBSTR(lcSlctd,1,lnW),'CSTPKINF')
        SELECT CSTPKINF
        lnQtyCrt   = CSTPKINF.NINPACK      
        lnWghtUnt  = CSTPKINF.NMPWEIGHT 
        lnTotWgt   = lnQtyCrt * lnWghtUnt
        lnWidth    = CSTPKINF.NMPCUBE   
        lnDepth    = CSTPKINF.NMPDEPTH  
        lnLnght    = CSTPKINF.NMPDIM      
      ELSE
        lnQtyCrt   = &lcTmpName..QTY_CTN   
        lnWghtUnt  = &lcTmpName..NSTYWEIGHT
        lnTotWgt   = lnQtyCrt * lnWghtUnt
        lnWidth    = &lcTmpName..NMPCUBE   
        lnDepth    = &lcTmpName..NMPDEPTH  
        lnLnght    = &lcTmpName..NMPDIM      
      ENDIF
    ENDIF    
    lcDesc = &lcTmpName..DESC
  ENDIF
ELSE
  *--Truncate extra spaces to meet the index
  lcSlctd = PADR(lcSlctd,29)
  lcOldValue = PADR(lcOldValue,29)  
  SET ORDER TO (lcCstPkInf+'P') IN (lcCstPkInf)
  SET ORDER TO CSTPKINFP IN CSTPKINF
  
  IF llBrowse
    llFound = lfPackBr()
  ELSE  
    IF !EMPTY(lcSlctd) .AND. lcOldValue # lcSlctd
      SELECT SPCK_HDR
      SET ORDER TO SPCK_HDRVR
      LOCATE FOR TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = ;
                 'P'+PADR(lcSlctCust,5)+lcSlctd       
      IF !FOUND()
        llFound = lfPackBr()
      ELSE
        laBrow[2] = SPCK_HDR.PACK_ID   
        laBrow[6] = SPCK_HDR.CPKCOLOR
        laBrow[7] = SPCK_HDR.CPCKSIZE  
        laBrow[8] = SPCK_HDR.CPKVERSION
        llFound = .T.
      ENDIF
    ENDIF
  ENDIF
  IF llFound
    STORE 0 TO lnQtyCrt,lnWghtUnt,lnTotWgt,lnWidth,lnDepth,lnLnght
    lcDesc     = SPCK_HDR.DESC
    lnWghtUnt  = lfPackWght()    
    IF SEEK('P'+PADR(lcSlctCust,5)+lcSlctd,lcCstPkInf)
      lnQtyCrt  = &lcCstPkInf..NINPACK   
      lnTotWgt  = lnQtyCrt * lnWghtUnt
      lnWidth   = &lcCstPkInf..NMPCUBE   
      lnDepth   = &lcCstPkInf..NMPDEPTH  
      lnLnght   = &lcCstPkInf..NMPDIM 
    ELSE
      IF SEEK('P'+PADR(lcSlctCust,5)+lcSlctd,'CSTPKINF')
        lnQtyCrt  = CSTPKINF.NINPACK   
        lnTotWgt  = lnQtyCrt * lnWghtUnt
        lnWidth   = CSTPKINF.NMPCUBE   
        lnDepth   = CSTPKINF.NMPDEPTH  
        lnLnght   = CSTPKINF.NMPDIM 
      ENDIF
    ENDIF    
  ENDIF  

ENDIF

SET ORDER TO &lcSavOrd IN CSTPKINF
SET ORDER TO &lcSavOrdTm IN (lcCstPkInf)

IF !laScrMode[2]
  lcWghtStat = IIF(lnStyPk=1,'ENABLE','DISABLE')
  SHOW GET lnWghtUnt &lcWghtStat
ENDIF  

IF !laScrMode[2] .AND. lcOldValue # lcSlctd
  SHOW GET pbCancel,1 PROMPT '\<Cancel'
  SHOW GET pbUpdate ENABLE
ENDIF
llBrowse = .F.
SHOW GETS
  
*-- end of lfvStyPkBr.

*!*************************************************************
*! Name      : lfPackBr                            C#200519
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : browse pack id for the selected customers only
*!*************************************************************
FUNCTION lfPackBr

PRIVATE lcBrFields ,lcBrTitle,lcKey,lnCurAlias,laRetData,llReturn

llReturn = .F.
STORE SPACE(0) TO laBrow
lnCurAlias = SELECT(0)

lcBrTitle  = 'Available Packs'
lcBrFields = "Account    :R :H='Account' :16," +;
             "Pack_ID    :R :H='Pack_ID' :16," +;
             "Desc       :R :H='Desc'    :30," +;
             "cDivision  :R :H='Devision':8,"  +;
             "Season     :R :H='Season'  :8,"  +;
             "CPKCOLOR   :R :H='Pack Color',"  +;
             "CPCKSIZE    :R :H='Pack Size',"  +;
             "CPKVERSION :R :H='Pack Version'"

DIMENSION laPacks[1]
laPacks = ''
SELECT ACCOUNT FROM SPCK_HDR WHERE ACCOUNT = PADR(lcSlctCust,5) AND TYPE = 'P' INTO ARRAY laPacks
IF EMPTY(laPacks[1])
  *--There are no records to browse.
  *-- OK
  = gfModalGen("INM42097B42001","Dialog")  
  _CUROBJ = OBJNUM(laData[2])
ELSE
  lcKeyFor = "'P' FOR ACCOUNT = [" + PADR(lcSlctCust,5) + "]"
  SELECT Spck_Hdr
  =gfBrows(lcKeyFor,"Account,Pack_ID,Desc,cDivision,Season,CPKCOLOR,CPCKSIZE,CPKVERSION",;
           "laBrow",lcBrTitle)
  IF !EMPTY(laBrow)
    lcSlctd = laBrow[2]+laBrow[6]+laBrow[7]+laBrow[8]
    lcDesc  = laBrow[3]
    llReturn = .T.
  ELSE
    lcSlctd = ''
    lcDesc  = ''
  ENDIF
  SHOW GETS
ENDIF

SELECT(lnCurAlias)
RETURN llReturn

*:**************************************************************************
*:* Name        : lfPkInfUpd                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Customer Pack information Update in the Temp file "lcCstPkInf"
*:***************************************************************************
FUNCTION lfPkInfUpd
PRIVATE lcTag,lcKey,lnAlias,laCusts,lnCnt
lnAlias = SELECT()

IF EMPTY(lcSlctCust)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No account is selected, cannot proceed.')
  _CUROBJ = OBJNUM(pbCustmer)
  RETURN
ENDIF
IF EMPTY(lcSlctd)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No '+IIF(lnStyPk=1,'style','pack ID')+' is selected, cannot proceed.')
  _CUROBJ = OBJNUM(lcSlctd)
  RETURN
ENDIF
IF lnQtyCrt=0 OR lnWghtUnt=0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Qty/Ctn or Wgh/Unt is zero, cannot update.')
  _CUROBJ = OBJNUM(lnQtyCrt)  
  RETURN
ENDIF

DIMENSION laCusts[1]
laCusts = .F.
=gfSubStr(lcSlctCust,@laCusts)    
SELECT (lcTmpName)

IF lnStyPk = 1
  SET ORDER TO (lcCstPkInf) IN (lcCstPkInf)
  IF REPL('*',lncolorwid) $ lcNonMjr
    =SEEK(SUBSTR(lcSlctd,1,lnstylewid),lcTmpName)    
    SCAN REST WHILE STYLE = SUBSTR(lcSlctd,1,lnstylewid)      
      FOR lnCnt = 1 TO ALEN(laCusts,1)
        *-ACCOUNT+TYPE+STYLE
        IF !SEEK(laCusts[lnCnt]+'S'+&lcTmpName..STYLE,lcCstPkInf)
          INSERT INTO (lcCstPkInf) (ACCOUNT,TYPE,STYLE) VALUES (laCusts[lnCnt],'S',&lcTmpName..STYLE)
        ENDIF
        REPLACE &lcCstPkInf..NINPACK   WITH lnQtyCrt,;
                &lcCstPkInf..NMPWEIGHT WITH lnWghtUnt,;
                &lcCstPkInf..NMPCUBE   WITH lnWidth,;
                &lcCstPkInf..NMPDEPTH  WITH lnDepth,;
                &lcCstPkInf..NMPDIM    WITH lnLnght,;
                &lcCstPkInf..STATUS    WITH 'U'                  
        STORE .T. TO glUpdated,llCUpdate
      ENDFOR
    ENDSCAN
  ELSE
    =SEEK(lcSlctd,lcTmpName)    
    FOR lnCnt = 1 TO ALEN(laCusts,1)
      *-ACCOUNT+TYPE+STYLE
      IF !SEEK(laCusts[lnCnt]+'S'+&lcTmpName..STYLE,lcCstPkInf)
        INSERT INTO (lcCstPkInf) (ACCOUNT,TYPE,STYLE) VALUES (laCusts[lnCnt],'S',&lcTmpName..STYLE)
      ENDIF
      REPLACE &lcCstPkInf..NINPACK   WITH lnQtyCrt,;
              &lcCstPkInf..NMPWEIGHT WITH lnWghtUnt,;
              &lcCstPkInf..NMPCUBE   WITH lnWidth,;
              &lcCstPkInf..NMPDEPTH  WITH lnDepth,;
              &lcCstPkInf..NMPDIM    WITH lnLnght,;
              &lcCstPkInf..STATUS    WITH 'U'
      STORE .T. TO glUpdated,llCUpdate
    ENDFOR
  ENDIF  
  
ELSE

  SET ORDER TO (lcCstPkInf+'P') IN (lcCstPkInf)
  IF !SEEK('P'+laCusts[1]+PADR(lcSlctd,29),lcCstPkInf)
    INSERT INTO (lcCstPkInf) (ACCOUNT,TYPE,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION) ;
                      VALUES (laCusts[1],'P',laBrow[2],laBrow[6],laBrow[7],laBrow[8])
  ENDIF
  SELECT (lcCstPkInf)
  REPLACE &lcCstPkInf..NINPACK   WITH lnQtyCrt,;
          &lcCstPkInf..NMPWEIGHT WITH lnWghtUnt,;
          &lcCstPkInf..NMPCUBE   WITH lnWidth,;
          &lcCstPkInf..NMPDEPTH  WITH lnDepth,;
          &lcCstPkInf..NMPDIM    WITH lnLnght,;
          &lcCstPkInf..STATUS    WITH 'U'
  STORE .T. TO glUpdated,llCUpdate
  
ENDIF

lcSlctd = ' '
lcDesc  = ' '
lnQtyCrt = 0
lnWghtUnt = 0
lnWidth = 0
lnDepth = 0
lnLnght = 0 
   
SHOW GETS

SHOW GET pbCancel,1 PROMPT '\<Close' 
SHOW GET pbUpdate DISABLE

*C200519,5  TMI [Start] Empties selected accounts
SELECT &lcTempCur
ZAP
*C200519,5  TMI [End  ] 

SELECT (lnAlias )

*-- end of lfPkInfUpd.

*:**************************************************************************
*:* Name        : lfPkInfCan                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Customer Pack information Cancel
*:***************************************************************************
FUNCTION lfPkInfCan
*C200519,5  TMI [Start] Empties selected accounts
SELECT &lcTempCur
ZAP
*C200519,5  TMI [End  ] 
CLEAR READ
*-- end of lfPkInfCan.


*:**************************************************************************
*:* Name        : lfwOldVal                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Saves old value
*:***************************************************************************
FUNCTION lfwOldVal
lcOldValue = EVAL(SYS(18))
*-- end of lfvOldVal.

*:**************************************************************************
*:* Name        : lfvSlctCst                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Selected customers valid functions
* Note          : This field is for display only, so any changes is ignoted and the old value is returned
*:***************************************************************************
FUNCTION lfvSlctCst
lcSlctCust = lcOldValue
*-- end of lfvSlctCst.

*:**************************************************************************
*:* Name        : lfQtyCrt                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Valid function for Master carton qty
*:***************************************************************************
FUNCTION lfQtyCrt
SHOW GETS
*-- end of lfQtyCrt.

*:**************************************************************************
*:* Name        : lfWghtUnt                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/19/2003
*:* Purpose     : Valid function for the Weight / unit field
*:***************************************************************************
FUNCTION lfWghtUnt
SHOW GETS
*-- end of lfWghtUnt.

*:**************************************************************************
*:* Name        : lfEXPCTOLD                                      *C200519,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/08/2003
*:* Purpose     : Save data to CSTPKINF file
*:***************************************************************************
*:* Called from : ICSTYLE.lpSavScr
*:***************************************************************************
FUNCTION lfEXPCTOLD
PRIVATE lcSvVrFle,lcCstPkInf,lcSeekKey
lcSvVrFle = lcfolder + 'A'
lcCstPkInf = &lcSvVrFle..cTmpName

IF USED(lcCstPkInf)
  SELECT &lcCstPkInf
  SCAN
    IF &lcCstPkInf..TYPE = 'S'
      SET ORDER TO 'CSTPKINF' IN CSTPKINF
      lcSeekKey = TYPE+ACCOUNT+STYLE
    ELSE
      SET ORDER TO 'CSTPKINFP' IN CSTPKINF
      lcSeekKey = TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
    ENDIF

    IF STATUS = 'U'
      SCATTER MEMVAR
      IF !SEEK(lcSeekKey,'CSTPKINF')
        INSERT INTO CSTPKINF FROM MEMVAR
      ELSE
        SELECT CSTPKINF
        GATHER MEMVAR
      ENDIF
    ELSE
*-*       IF STATUS = 'D' .AND. SEEK(lcSeekKey,'CSTPKINF')
*-*         DELETE
*-*       ENDIF      
    ENDIF
  ENDSCAN  
  USE IN (lcCstPkInf)
  ERASE (gcWorkDir+lcCstPkInf+'.DBF')
  ERASE (gcWorkDir+lcCstPkInf+'.CDX')
ENDIF
*-- end of lfEXPCTOLD.

*!*************************************************************
*! Name      : lfStyBrow                             C#200519
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/05/2002
*! Purpose   : Browse order styles/sizes.
*!*************************************************************
FUNCTION lfStyBrow

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl,lcBrFlds
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

SELECT (lcTmpName)

lnCurAlias = SELECT(0)

*llGetDtl
*lcFields    = "Style"
lcBrFlds    = [Scale     :H='Scale'       ,]+;
              [Season    :H='Season'      ,]+;
              [CDIVISION :H='Division'    ,]+;
              [PATTERN   :H='Pattern'     ,]+;
              [cStyGroup :H='Group'        ]

lcBrFields  = [cStyMajor :H=lcMjrTtl      ,]+;
              [DESC      :H='Description' ,]+;
              lcBrFlds
lcFile_Ttl  = 'Style File'
*lcForExp = 'FOR .T.'
lcForExp = ''

DECLARE laTemp[1],laTempClr[1]
laTemp[1] = .F.
laTempClr[1] = .F.
SELECT (lcTmpName)
SET ORDER TO CSTYLE IN (lcTmpName)
llReturn  = gfBrows(lcForExp,'Style','laTemp')                       
IF !EMPTY(laTemp[1])
  lcFile_Ttl  = 'Style Colors'
  lcBrFields  = [Style     :H=lcItemTl      ,]+;
                [DESC1     :H='Description' ,]+;
                lcBrFlds
  
  lcForExp = 'FOR STYLE=['+SUBSTR(laTemp[1],1,lnstylewid)+']'
  SET ORDER TO STYLE IN (lcTmpName)
  =gfBrows(lcForExp,'Style','laTempClr')
  IF !EMPTY(laTempClr[1])
    lcSlctd = laTempClr[1]
  ELSE
    lcSlctd = STUFF(laTemp[1],lnstylewid+2,lncolorwid,REPL('*',lncolorwid))
  ENDIF
ELSE
  lcSlctd = lcOldValue
ENDIF
SET ORDER TO STYLE IN (lcTmpName)
SELECT(lnCurAlias)

RETURN llReturn

*!**************************************************************************
*! Name      : lfChangWhs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/13/2003
*! Purpose   : Function To control status of WareHuose
*!**************************************************************************
*! Called Form : lfActFolder in Sales Order screen.
*!**************************************************************************
*B607250,1
FUNCTION lfChangWhs
PRIVATE lnAlias , lnRecNo

lnAlias = SELECT()

*--if Add mode
IF laScrMode[4] 
  SHOW GET lnWareHouse ENABLE
ENDIF

*--if in Edit mode
*Make warehouse editable in case of
*  1-No pick tickt created for the Sales Order
*  2-No Cut tickt Or PO created for the Sales Order      (This is cancelled now in this bug)
*  3-No Partialy Shipment for the Sales Order
IF laScrMode[3] 
  SELECT (lcOrdLine)
  lnRecNo = RECNO()
  
  *LOCATE FOR TotPik > 0 .OR. TotCut > 0  .OR.  &lcOrdHdr..ship >0
  LOCATE FOR TotPik > 0 OR  &lcOrdHdr..ship >0
  IF BETWEEN(lnRecNo ,1,RECCOUNT()) 
   GOTO lnRecNo
  ENDIF
  SELECT (lnAlias)
  *RETURN IIF( FOUND('&lcOrdLine') , 'DISABLE' , 'ENABLE' ) 
  IF FOUND('&lcOrdLine')
    SHOW GET lnWareHouse DISABLE
  ELSE
    SHOW GET lnWareHouse ENABLE
  ENDIF  
ELSE
  SELECT (lnAlias)
  SHOW GET lnWareHouse DISABLE
ENDIF  
*--End of lfChangWhs.

*!*************************************************************
*! Name      : lfClearVar
*! Developer : Nader Nabil (NNA)
*! Date      : 04/29/2004
*! Purpose   : Replace (llClearVar) Field with .T. to know that the Store
*!           : Not Shiped Yet and can update Ship amount
*!*************************************************************
*!B122625,1
FUNCTION lfClearVar

lcSVarFle = lcConsInvH + 'A'
REPLACE &lcSVarFle..llClearVar WITH .T.

*--End of lfClearVar.

*!*************************************************************
*! Name      : lfGMAGTSCD
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/15/2004
*! Purpose   : To Handel Sizes in SKU Screen
*!*************************************************************
*--MHM B124629
FUNCTION lfGMAGTSCD
PRIVATE lcOldOrd
SELECT (lcFileToUse)
lcOldOrd = ORDER()
IF laScrMode[2] OR laScrMode[3]
  IF laScrMode[2] 
    SET ORDER TO SPCK_LINST
  ENDIF
  lcAlias    = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
  lcGmSizeV  = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmSizeV'))
  lcGmaColor = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmaColor'))
ENDIF

SCAN FOR TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = ;
         'S'+laData[1]+ALLTRIM(ladata[2])
  IF (laScrMode[2] OR laScrMode[3]) AND (CPKCOLOR <> LEFT(lcGmaColor,6) OR CPCKSIZE <> LEFT(lcGmSizeV,3) OR PACK_ID <> ALLTRIM(laData[3]))
    LOOP
  ENDIF
  
  lnJ = 1
  llPPak = !EMPTY(&lcFileToUse..Sku)
  IF !llPPak
    *-- Looping 8 times because there are 8 fields represent 8 quantities 
    *-- in file 'Spck_lin' form Qty1->Qty8
    FOR lnJ = 1 TO 8
      IF EVAL(lcFileToUse+'.Qty'+ALLTRIM(STR(lnJ))) = 1
        laSzCode[lnJ] = SUBSTR(Pack_ID,LEN(ALLTRIM(laData[3]))+1)
        EXIT
      ENDIF
    ENDFOR
  ELSE
    lcPrePack  = &lcFileToUse..Prepak
    lnScCuRec  = RECNO('Scale')
    = SEEK('P'+Style.Scale+lcPrePack,'Scale')
    SELECT Scale
    SCATTER FIELDS Pp1,Pp2,Pp3,Pp4,Pp5,Pp6,Pp7,Pp8 TO laSzCode
    FOR lnI = 1 TO 8
      laSzCode[lnI] = IIF(lnI<=m.Cnt,LTRIM(STR(laSzCode[lnI])),'')
    ENDFOR

    SELECT (lcFileToUse)
    lcPrPkDesc = Scale.Cscl_Desc
    IF RECCOUNT('Scale') >= lnScCuRec
      GOTO lnScCuRec IN ('Scale')
    ENDIF
    EXIT
  ENDIF
ENDSCAN

*-- This is because we needn't to call lfRefresh in view mode becuse 
*-- the say fields have been refreshed by control panel when we have
*-- been transfered to view mode
*-- but we need to refresh say fields when we are in view mode when 
*-- UP OR DOWN button is clicked

= lfStyDesc()

IF !laScrMode[2] OR llUpDn
  = lfRefresh()
ENDIF

SELECT (lcFileToUse)
SET ORDER TO &lcOldOrd

SELECT(lnCurAlias)

*!**************************************************************************
*! Name      : lfBrwAssCst
*! Developer : NADER NABIL (NNA)
*! Date      : 12/26/2004
*! Purpose   : Add the nAssCost field the the browse fields at the So and invoice
*!**************************************************************************
*! Example   : = lfBrwAssCst()
*!**************************************************************************
*! Due to C125213,1
*!**************************************************************************
FUNCTION lfBrwAssCst

*--I Used !&lcFileNamA..llSumLin AND llDetails to not show this field with The 
*--Options Selections
IF lcProgName = 'SOORD'
  lcFileNamA = lcOrdLine+"A"
  IF !&lcFileNamA..llSumLin AND llDetails
    IF !(laScrMode[2] OR laScrMode[1])
      lcOrderBr = lcOrderBr + [,NASSCOST :H='Assist/PC' :V=gfDoTriger('SOORD',PADR('UPDFLAG',10)) :F]
    ELSE
      lcOrderBr = lcOrderBr + [,NASSCOST :H='Assist/PC' :R]  
    ENDIF
  ENDIF
ENDIF
IF lcProgName = 'ARIINV'
  lcFields = lcFields + ",NASSCOST :H='Assist/PC' :R"
ENDIF

*--End of function lfBrwAssCst.
*!**************************************************************************
*! Name      : lfTmpAssCst
*! Developer : NADER NABIL (NNA)
*! Date      : 12/26/2004
*! Purpose   : Add the nAssCost field to the browse fields at the Template
*!           : So Screen
*!**************************************************************************
*! Example   : = lfTmpAssCst()
*!**************************************************************************
*! Due to C125213,1
*!**************************************************************************
FUNCTION lfTmpAssCst
IF !(laScrMode[2] OR laScrMode[1])
  lcFields = lcFields + [,NASSCOST :H='Assist/PC']    
ELSE
  lcFields = lcFields + [,NASSCOST :H='Assist/PC' :R]        
ENDIF
*--End of function lfTmpAssCst.
*!**************************************************************************
*! Name      : lfGetAssCst
*! Developer : NADER NABIL (NNA)
*! Date      : 12/26/2004
*! Purpose   : Save the Ordline.Assist Cost in the lcinvline file
*!**************************************************************************
*! Example   : = lfGetAssCst()
*!**************************************************************************
*! Due to C125213,1
*!**************************************************************************
FUNCTION lfGetAssCst
PRIVATE lnOldAlias
lnOldAlias = SELECT(0)
IF lcProgName = 'SOORD'
  SELECT (lcOrdLine)
  REPLACE nAssCost WITH EVAL(lcTempline+'.nAssCost')
ELSE
  IF ALIAS()=(lcConsinvd)
    REPLACE nAssCost WITH m.nAssCost
  ELSE
    SELECT (lcInvLine)
    REPLACE nAssCost WITH ORDLINE.nAssCost
  ENDIF
ENDIF
SELECT(lnOldAlias)
*--End of function lfGetAssCst.
*!*************************************************************
*! Name      : lfGMADLSKU
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/28/2004
*! Purpose   : Delete SKU 
*!*************************************************************
*--MHM B124944
FUNCTION lfGMADLSKU

PRIVATE lnCurAlias,lcTag,lnCurRec,llOpen,lnI,llContinue

lnCurAlias = SELECT(0)
SELECT Spck_Hdr
lcSpkHdOrd = ORDER()
SET ORDER TO SPCK_HDRST

lcAlias    = ALLTRIM(EVAL(lcLinFil+"A"+'.lcAlias'))
lcGmSizeV  = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmSizeV'))
lcGmaColor = ALLTRIM(EVAL(lcLinFil+"A"+'.lcGmaColor'))

IF SEEK('S'+laData[1]+ALLTRIM(ladata[2]),'Spck_Hdr') 
  SELECT Spck_Hdr
  SCAN FOR TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = ;
           'S'+laData[1]+ALLTRIM(ladata[2])
    IF CPKCOLOR <> LEFT(lcGmaColor,6) OR CPCKSIZE <> LEFT(lcGmSizeV,3) OR PACK_ID <> ALLTRIM(laData[3])
      LOOP
    ENDIF
    BLANK
    DELETE
  ENDSCAN
ENDIF

SELECT Spck_Lin
lcSpkLnOrd = ORDER()
SET ORDER TO Spck_LinST
IF SEEK('S'+laData[1]+ALLTRIM(ladata[2]),'Spck_Lin') 
  SELECT Spck_Lin
  SCAN FOR TYPE+ACCOUNT+STYLE+CPKCOLOR+CPCKSIZE+PACK_ID+CPKVERSION = ;
           'S'+laData[1]+ALLTRIM(ladata[2])
    IF CPKCOLOR <> LEFT(lcGmaColor,6) OR CPCKSIZE <> LEFT(lcGmSizeV,3) OR PACK_ID <> ALLTRIM(laData[3])
      LOOP
    ENDIF
    BLANK
    DELETE
  ENDSCAN
ENDIF

STORE .F. TO laScrMode
laScrMode[1]  = .T.

SELECT Spck_Hdr
SET ORDER TO &lcSpkHdOrd 

SELECT Spck_Lin
SET ORDER TO &lcSpkLnOrd 

SELECT (lnCurAlias)

*!**************************************************************************
*! Name      : lfAddBarNO
*! Developer : NADER NABIL (NNA)
*! Date      : 02/07/2005
*! Purpose   : Update A Field to skip or not skip of bar (Summarize By Pack)
*!           : and (Show Summary)
*!**************************************************************************
*! Example   : = lfAddBarNO()
*!**************************************************************************
*! Due to C125563,1
*!**************************************************************************
FUNCTION lfAddBarNO
PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)
lcFileNamA = lcOrdLine+"A"
SELECT(lcFileNamA)
DO CASE
  *-- if (Show Summary) Marked then Skip of (Summarize By Pack)
  CASE MRKBAR('_INQURYPOP',11)
    REPLACE &lcFileNamA..lnBarNO WITH 11
  *-- if (Show Summary) UnMarked and Was Skipping of (Summarize By Pack) then Show Both
  CASE !MRKBAR('_INQURYPOP',11) AND &lcFileNamA..lnBarNO=11
    REPLACE &lcFileNamA..lnBarNO WITH 0
  *-- if (Show Summary) UnMarked Was Skipping of (Show Summary) then don't Show it if user 
   *- Marked any option Else
  CASE !MRKBAR('_INQURYPOP',11) AND &lcFileNamA..lnBarNO=19
    REPLACE &lcFileNamA..lnBarNO WITH 19
  *--If not of the Above then Replace the Field with the Bar No.
  OTHERWISE 
    REPLACE &lcFileNamA..lnBarNO WITH BAR()
ENDCASE
SELECT(lnOldAlias)
*--End Of FUNCTION lfAddBarNO.

*!**************************************************************************
*! Name      : lfUpdDinv
*! Developer : NADER NABIL (NNA)
*! Date      : 02/21/2005
*! Purpose   : Function to do nothing only to deal with trigger (Update or Insert)
*!           : that made in Aria4
*!**************************************************************************
*! Example   : = lfUpdDinv()
*!**************************************************************************
*! Due to 126137,1
*!**************************************************************************
FUNCTION lfUpdDinv
RETURN
*--End of function lfUpdDinv

*!**************************************************************************
*! Name      : lfUpdFlag
*! Developer : NADER NABIL (NNA)
*! Date      : 02/21/2005
*! Purpose   : Update field(flag) in the lcOrdLine file with 'M' to save the 
*1           : any changes in the Assist Cost field
*!**************************************************************************
*! Example   : = lfUpdFlag()
*!**************************************************************************
*! Due to 125213,1
*!**************************************************************************
FUNCTION lfUpdFlag
lnOldAlias = SELECT(0)
IF laScrMode[3]
  SELECT(lcOrdLine)
  REPLACE FLAG WITH IIF(EMPTY(FLAG),'M',FLAG)
ENDIF
SELECT(lnOldAlias)
*--End of funtion lfUpdFlag.
