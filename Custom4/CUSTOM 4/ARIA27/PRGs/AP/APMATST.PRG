*:************************************************************************
*:
*: Procedure file: APMATSTY.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfShowMtST
*:               : lfBrwCost
*:               : lfwBrwCost
*:               : lfvBrwCost
*:               : lfBrwCont
*:               : lfwBrwCont
*:               : lfvBrwCont
*:               : lfwItmAmnt
*:               : lfvItmAmnt
*:               : lfShowMt
*:               : lfvNewMtSt
*:               : lfvCanNew
*:               : lfwTktNo
*:               : lfvTktNo
*:               : lfwSession
*:               : lfvSession
*:               : lfwCostTyp
*:               : lfvCostTyp
*:               : lfwMFG
*:               : lfvMFG
*:               : lfwAmount
*:               : lfvAmount
*:               : lfvProceed
*:               : lfvRemMtSt
*:               : lfvInvMtSt
*:               : lfvCanMtSt
*:               : lfSMtStTrp
*:               : lfRMtStTrp
*:               : lfKeyNew
*:               : lpTabMtSt
*:               : lpShftMtSt
*:               :
*:
*:      Documented 00/00/1994
*:************************************************************************
*B600460,1 MAN 06/18/95   Fix Red Error While Browsing the Ticket number  
*B600460,1                Fix Deleting the contribution records
*E100219,8 MAN 07/24/95   Prevent Applying MFG Codes that has been setup
*E100219,8                to be applied directly through the Cost sheet 
*B601580,1 HS  03/26/97   Fix the popup Cost type under windows.
*B601643,1 HS  03/26/97   1) Make the Con. % column in the browse
*B601643,1                   a Read only column.
*B601643,1                2) Fix the UNITCOST field calculation.
*B601643,1                3) Fix the record pointer of the browse lcCostTtl.
*B601643,1                4) Fix the calculation of the Contributed total.
*B601643,1                5) Warehouse field disappears sometimes
*B601643,1                6) Checked Numeric overflow.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*********************************************************************************

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laField_H,laCtrStat,laCostType

** lcObjTtl      : Variable to hold
** lcCostType    : Variable to hold
** lcMFG         : Variable to hold
** lcOldMFG      : Variable to hold
** lcTktNo       : Variable to hold
** lcRSession    : Variable to hold
** lcOldTktNo    : Variable to hold
** lcOldRSession : Variable to hold
**
** lnAmount      : Variable to hold
** lnOldAmount   : Variable to hold
** lnOldItmAmt   : Variable to hold
** lnTotAmnt     : Variable to hold
** lnTotCont     : Variable to hold
** lnCostCode    : Variable to hold
** lnOldCostType : Variable to hold
** lnBrCstNo     : Variable to hold
** lnBrCntNo     : Variable to hold
** lnNoOfRec     : Variable to hold
** lcOldValue    : Variable to hold

STORE ' '       TO lcObjTtl  , lcCostType, lcOldValue
STORE SPACE(2)  TO lcMFG     , lcOldMFG
STORE SPACE(6)  TO lcTktNo   , lcRSession   , lcOldTktNo , lcOldRSession
STORE 0.00      TO lnAmount  , lnOldAmount  , lnOldItmAmt, lnTotAmnt     , lnTotCont
STORE 0         TO lnCostCode, lnOldCostType, lnBrCstNo  , lnBrCntNo     , lnNoOfRec
STORE 1         TO rbBaseCost 
STORE .F.       TO llCanProc

lcTShipping  = 'Shippment number '
lcTPo        = 'PO number '
lcTCutting   = 'Cutting ticket number '

=lfSMtStTrp()

*E300683,1 Call *.SPR from screens directory
* DO APMATST.SPR 
DO (gcScrDir + gcWinAppl + '\APMATST.SPR')
*E300683,1 end          

=lfRMtStTrp()

*!**************************************************************************
*!
*!      Function: lfShowMtST
*!
*!**************************************************************************
*
FUNCTION lfShowMtST

IF lnNoOfRec = 0
  SHOW GET pbRemMtSt DISABLE
ELSE
  SHOW GET pbRemMtSt ENABLE
ENDIF  

*!**************************************************************************
*!
*!      Function: lfBrwCost
*!
*!**************************************************************************
*
FUNCTION lfBrwCost
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT (lcTmpPORec)

lnBrCstNo = RECNO()

lcBrwCost = "CMARKER=IIF(RECNO()=lnBrCstNo,'',' '):1:H=' ':W=.F.,"+;
            "CTKTNO :R  :H=IIF(lnReceipts = 3,'CT',IIF(llShipment,'Ship.#','PO No.')),"+;
            "CRSESSION :R :H='Receipt',"+;
            "CCSTTYPE=IIF(EMPTY(CBOMTYPE),SPACE(10),laCostType[ASUBSCRIPT(laCostType,ASCAN('laCostType',CBOMTYPE),1),1]) :10 :R :H='Cost type',"+;
            "CMFGCODE :R :H='MFG',"+;
            "NAMOUNT :R :H='Amount',"+;
            "NCONTRIB :11 :R :H='Contributed'"

BROWSE FIELDS &lcBrwCost ;
              WINDOW APMTST1 ;
              WHEN lfwBrwCost();
              VALID :F (WONTOP() = lcCostTtl .AND. lfvBrwCost()) .OR. gfStopBrow() ;
              IN WINDOW AWDAPMATST;
              LOCK 0;
              NOAPPEND;
              NOCLEAR;
              NODELETE;
              NOWAIT;
              SAVE;
              TITLE lcCostTtl;
              NOEDIT &lcClrSchm

*!**************************************************************************
*!
*!      Function : lfwBrwCost
*!
*!**************************************************************************
*
FUNCTION lfwBrwCost

glFromBrow= .T.

llConFirm = .F. && Variable to tell if the Zero is confirmed.

lnBrCstNo = RECNO()
lnBrCntNo = RECNO(lcTmpBomLn)

SHOW WINDOW (lcCostTtl) REFRESH

=lfSMtStTrp()

*!**************************************************************************
*!
*!      Function : lfvBrwCost
*!
*!**************************************************************************
*
FUNCTION lfvBrwCost

lnBrCstNo = RECNO()
lnBrCntNo = RECNO(lcTmpBomLn)

SHOW WINDOW (lcCostTtl) REFRESH

=lfRMtStTrp()

*!**************************************************************************
*!
*!      Function: lfBrwCont
*!
*!**************************************************************************
*
FUNCTION lfBrwCont
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT (lcTmpBomLn)

lnBrCntNo = RECNO()

*B601643,1 Change this line to make the Con. % column in the browse
*B601643,1 a Read only column and to add the forced validation option
*B601643,1 to the validation of the Contributed column and remove the
*B601643,1 calling of the function gfUpdate() from this validation [Begin]
*lcBrwCont = "CMARKER=IIF(RECNO()=lnBrCntNo,'',' '):1:H=' ':W=.F.,"+;
*            "CTKTNO :R  :H=IIF(lnReceipts = 2,'PO No.','CT No.'),"+;
*            "STYLE :R :H='Style',"+;
*            "SCLR  :R :H='Color',"+;
*            "CWHOUSE=IIF(lnReceipts = 2,POSLN.CWARECODE,CUTTKTL.CWARECODE) :R :H='W. House',"+;
*            "STYQTY :R :H='Rec. Qty',"+;
*            "ITEMAMT :H='Contributed' :W=lfwItmAmnt() :V=lfvItmAmnt() .AND. gfUpdate(),"+;
*            "nConPer=ITEMAMT/&lcTmpPORec..NAMOUNT*100 :7 :H='Con. %' :W=.F."

lcBrwCont = "CMARKER=IIF(RECNO()=lnBrCntNo,'',' '):1:H=' ':W=.F.,"+;
            "CTKTNO :R  :H=IIF(lnReceipts = 2,'PO No.','CT No.'),"+;
            "STYLE :R :H='Style',"+;
            "SCLR  :R :H='Color',"+;
            "CWHOUSE=IIF(lnReceipts = 2,POSLN.CWARECODE,CUTTKTL.CWARECODE) :R :H='W. House',"+;
            "STYQTY :R :H='Rec. Qty',"+;
            "ITEMAMT :H='Contributed' :W=lfwItmAmnt() :V=lfvItmAmnt() :F,"+;
            "nConPer=ITEMAMT/&lcTmpPORec..NAMOUNT*100 :7 :R :H='Con. %' :W=.F."

*B601643,1 Change this line [End]

BROWSE FIELDS &lcBrwCont ;
              WINDOW APMTST2 ;
              WHEN lfwBrwCont();
              VALID :F (WONTOP() = lcContTtl .AND. lfvBrwCont()) .OR. gfStopBrow() ;
              IN WINDOW AWDAPMATST;
              LOCK 0;
              NOAPPEND;
              NOCLEAR;
              NODELETE;
              NOWAIT;
              SAVE;
              TITLE lcContTtl;
              NOEDIT &lcClrSchm

SELECT (lcTmpPORec)

*!**************************************************************************
*!
*!      Function : lfwBrwCont
*!
*!**************************************************************************
*
FUNCTION lfwBrwCont

glFromBrow= .T.

llConFirm = .F. && Variable to tell if the Zero is confirmed.

lcStyCol  = IIF(lnReceipts = 2,IIF(llShipment,'PO','SHIPNO')+"+STYLE+SCLR","STYLE+COLOR")

lnBrCntNo = RECNO()

SHOW WINDOW (lcContTtl) REFRESH

=lfSMtStTrp()

*!**************************************************************************
*!
*!      Function : lfvBrwCont
*!
*!**************************************************************************
*
FUNCTION lfvBrwCont

lnBrCntNo = RECNO()
SHOW WINDOW (lcContTtl) REFRESH

lcStyCol  = ''

=lfRMtStTrp()

*!**************************************************************************
*!
*!      Function : lfwItmAmnt
*!
*!**************************************************************************
*
FUNCTION lfwItmAmnt

lnOldItmAmt = &lcTmpBomLn..ITEMAMT
lnTotCont   = lnTotCont - &lcTmpPORec..NCONTRIB

*!**************************************************************************
*!
*!      Function : lfvItmAmnt
*!
*!**************************************************************************
*
FUNCTION lfvItmAmnt

IF &lcTmpBomLn..ITEMAMT >= 0 .AND. &lcTmpBomLn..ITEMAMT <> lnOldItmAmt
  IF &lcTmpPORec..NCONTRIB + &lcTmpBomLn..ITEMAMT - lnOldItmAmt > &lcTmpPORec..NAMOUNT
    ** MESSAGE : " The total contributed amount cannot "
    **           " be greater than the total cost.     "
    **           "                   ® Ok ¯            "
    =gfModalGen('QRM04123B00000','Dialog')
    REPLACE &lcTmpBomLn..ITEMAMT WITH lnOldItmAmt
  ELSE
    REPLACE &lcTmpPORec..NCONTRIB WITH &lcTmpPORec..NCONTRIB + &lcTmpBomLn..ITEMAMT - lnOldItmAmt

    *B601643,1 Add this lines to update the UNITCOST field 
    *B601643,1 and to clear the varible lcStyCol to refresh the relation 
    *B601643,1 to prevent the warehouse from disappearing [Begin]
    REPLACE &lcTmpBomLn..UNITCOST WITH IIF(&lcTmpBomLn..STYQTY <> 0 , &lcTmpBomLn..ITEMAMT / &lcTmpBomLn..STYQTY , 0)
    lcStyCol = ''
    *B601643,1 Add this lines to update the UNITCOST field [End]

    SHOW WINDOW (lcCostTtl) REFRESH SAME
    SHOW WINDOW (lcContTtl) REFRESH SAME
  ENDIF
ELSE
  REPLACE &lcTmpBomLn..ITEMAMT WITH lnOldItmAmt
ENDIF

lnTotCont = lnTotCont + &lcTmpPORec..NCONTRIB

=lfRefMater()

*!**************************************************************************
*!
*!      Function: lfShowMt
*!
*!**************************************************************************
*
FUNCTION lfShowMt

SHOW GET lcTktNo    ENABLE
SHOW GET ibTktNo    ENABLE

=lfShowFunc()
SHOW GET ibCostType ENABLE
SHOW GET puCostType ENABLE
SHOW GET lnAmount   ENABLE
SHOW GET rbBaseCost ENABLE

SHOW GET lcMFG      DISABLE
SHOW GET ibMFG      DISABLE

IF lnNoOfRec = 0
  SHOW GET pbRemMtSt DISABLE
ELSE
  SHOW GET pbRemMtSt ENABLE
ENDIF  

=lfMatRef()

*!**************************************************************************
*!
*!      Function: lfvNewMtSt
*!
*!**************************************************************************
*
FUNCTION lfvNewMtSt

lcTktNo    = SPACE(6)
lcRSession = SPACE(6)
lcMFG      = SPACE(2)
lcCostType = 'Select cost type'

lnAmount   = 0.00
lnCostCode = 0
rbBaseCost = 1

SELECT (lcTmp_Rec)
SET ORDER TO
GO BOTTOM
lcRSession = IIF(EMPTY(lcRSession),&lcTmp_Rec..CRSESSION,lcRSession)
IF lnReceipts = 2
  IF llShipment
    lcObjTtl = 'Shipment No.'
    lcTktNo  = IIF(EMPTY(lcTktNo),&lcTmp_Rec..SHIPNO,lcTktNo)
  ELSE
    lcObjTtl = 'PO Number'
    lcTktNo  = IIF(EMPTY(lcTktNo),&lcTmp_Rec..PO,lcTktNo)
  ENDIF
ELSE
  lcObjTtl   = 'CT No.'
  lcTktNo    = IIF(EMPTY(lcTktNo),&lcTmp_Rec..CUTTKT,lcTktNo)
ENDIF
SET ORDER TO (lcTmp_Rec)
lcCostType = 'Select cost type'
puCostType = 'Select cost type'

ON KEY LABEL ESC
ON KEY LABEL Ctrl+ENTER

=lfRMtStTrp()

*E300683,1 Call *.SPR from screens directory
* DO APMTST5.SPR 
DO (gcScrDir + gcWinAppl + '\APMTST5.SPR')
*E300683,1 end          

=lfRefMater()

=lfSMtStTrp()

ON KEY LABEL Ctrl+ENTER lnDummy = 1

IF lnNoOfRec = 0
  SHOW GET pbRemMtSt DISABLE
ELSE
  SHOW GET pbRemMtSt ENABLE
ENDIF  

_CUROBJ = OBJNUM(pbNewMtSt)
SHOW GET pbNewMtSt

SHOW WINDOW (lcCostTtl) REFRESH SAME
SHOW WINDOW (lcContTtl) REFRESH SAME


*!**************************************************************************
*!
*!      Function: lfvCanNew
*!
*!**************************************************************************
*
FUNCTION lfvCanNew

ON KEY LABEL ESC DO lfvCanMtSt
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfwTktNo
*!
*!**************************************************************************
*
FUNCTION lfwTktNo

lcOldTktNo    = lcTktNo
lcOldRSession = lcRSession

*!**************************************************************************
*!
*!      Function: lfvTktNo
*!
*!**************************************************************************
*
FUNCTION lfvTktNo

IF llBrowse OR !EMPTY(lcTktNo) OR ATC("?",lcTktNo) > 0
  IF llBrowse .OR. ATC("?",lcTktNo) > 0
    llBrowse = .F.
    IF lnReceipts = 2
      IF llShipment
        SELECT SHPMTHDR
        DIMENSION laTemp[2]
        laTemp = ''
        *B600460,1 MAN 06/18/95
        *lcSavBrFld=lcBrFields
        *lcSavTitle=lcFile_Ttl

        lnClosRec = RECNO(0)
          
        IF BETWEEN(lnClosRec,1,RECCOUNT('SHPMTHDR'))
          GO lnClosRec
        ELSE
          GO TOP
        ENDIF

        lcFile_Ttl="Shippment"
        lcBrFields="SHIPNO :H= 'Ship. #',STATUS = IIF(STATUS = 'C','Complete','Open'),ENTERED :H='Entered',TOTQTY :H= 'Tot. Qty.'"

        =gfBrows([FOR STATUS $ 'CO' .AND. (RECV_STK+RECV_DAM+RECV_CAN)>0 ],'SHIPNO,STATUS','laTemp')

       *B600460,1 MAN 06/18/95
       *lcBrFields=lcSavBrFld
       *lcFile_Ttl=lcSavTitle
      ELSE
        SELECT POSHDR
        DIMENSION laTemp[2]
        laTemp = ''
       *B600460,1 MAN 06/18/95 
       *lcSavBrFld=lcBrFields
       *lcSavTitle=lcFile_Ttl
        lnClosRec = RECNO(0)
        IF BETWEEN(lnClosRec,1,RECCOUNT('POSHDR'))
          GO lnClosRec
        ELSE
          GO TOP
        ENDIF

        lcFile_Ttl="Purchase order"
        lcBrFields="PO,VENDOR :H='Vendor',STATUS=IIF(STATUS = 'C','Complete','Open') :H='Status',ENTERED :H='Entered'"

        =gfBrows([FOR STATUS $ 'CO' .AND. (RECEIVE+DAMAGE+CANCEL)>0],'PO,VENDOR','laTemp')
       *B600460,1 MAN 06/18/95 
       *lcBrFields=lcSavBrFld
       *lcFile_Ttl=lcSavTitle
      ENDIF
    ELSE
      SELECT CUTTKTH
      DIMENSION laTemp[2]
      laTemp = ''

*     lcSavBrFld=lcBrFields
*     lcSavTitle=lcFile_Ttl
      lnClosRec = RECNO(0)
          
      IF BETWEEN(lnClosRec,1,RECCOUNT('CUTTKTH'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF
      lcFile_Ttl="Cutting ticket"
      lcBrFields="CUTTKT :H='CT',STYLE :H='Style',Desc :H='Desc.',STATUS=IIF(STATUS='A','Actualized',IIF(STATUS='C','Complete','Open')) :H='Status'"

      =gfBrows([FOR STATUS $ 'ACO' .AND. (PCS_REC+PCS_DAM+PCS_CAN) > 0],'CUTTKT,STYLE','laTemp')
    
*     lcBrFields=lcSavBrFld
*     lcFile_Ttl=lcSavTitle
    ENDIF
    IF !EMPTY(laTemp[1])
      lcTktNo = laTemp[1]
      =lfShowFunc()
    ELSE
      lcTktNo = lcOldTktNo
      lcOldRSession = lcRSession
      SHOW GET lcTktNo
      =lfShowFunc()
      RETURN
    ENDIF
  ENDIF
  =lfShowFunc()
  
  IF lnReceipts = 2
    IF llShipment
      SELECT SHPMTHDR
      IF EMPTY(lcTktNo)
        =lfShowFunc()
      ELSE
        lcSavExact = SET('EXACT')
        SET EXACT ON
        IF SEEK(lcTktNo)
          SET EXACT &lcSavExact
          IF lcTktNo <> lcOldTktNo
            lcRSession = SPACE(6)
          ENDIF  
          =lfShowFunc()
        ELSE
          SET EXACT &lcSavExact
          ** MESSAGE : " Shippment number ð not found. "
          **           "     < Browse > < Reenter >   "
          IF gfModalGen("TRM04099B00014","DIALOG",lcTShipping+ALLTRIM(lcTktNo)) = 1
            SELECT SHPMTHDR
            DIMENSION laTemp[2]
            laTemp = ''
           *B600460,1 MAN 06/18/95 
           *lcBrFields=lcSavBrFld
           *lcFile_Ttl=lcSavTitle
            lnClosRec = RECNO(0)

            IF BETWEEN(lnClosRec,1,RECCOUNT('SHPMTHDR'))
              GO lnClosRec
            ELSE
              GO TOP
            ENDIF

            lcFile_Ttl="Shippment"

            lcBrFields="SHIPNO :H= 'Ship. #',STATUS = IIF(STATUS = 'C','Complete','Open'),ENTERED :H='Entered',TOTQTY :H= 'Tot. Qty.'"

            =gfBrows([FOR STATUS $ 'CO'],'SHIPNO,STATUS','laTemp')
           *B600460,1 MAN 06/18/95
           *lcBrFields=lcSavBrFld
           *lcFile_Ttl=lcSavTitle
      
            IF !EMPTY(laTemp[1])
              lcTktNo = laTemp[1]
              =lfShowFunc()
            ELSE
              lcTktNo = lcOldTktNo
              llCanProc = .T.        
              SHOW GET lcTktNo
              =lfShowFunc()
              RETURN
            ENDIF
          ELSE
            lcTktNo = lcOldTktNo
            SHOW GET lcTktNo
            llCanProc = .T.        
            =lfShowFunc()
            _CUROBJ = _CUROBJ
            RETURN
          ENDIF
        ENDIF
      ENDIF
    ELSE
      SELECT POSHDR
      IF EMPTY(lcTktNo)
        =lfShowFunc()
        llCanProc = .T.        
      ELSE
        lcSavExact = SET('EXACT')
        SET EXACT ON
        IF SEEK(lcTktNo)
          SET EXACT &lcSavExact
          IF STATUS $ 'XHS'
            llCanProc = .T.        
            ** MESSAGE : " The status of this purchase "
            **           " order is ð                  "
            **           "            ® Ok ¯           "
            lcPoStatus = IIF(STATUS = 'X','canceled',IIF(STATUS = 'H','hold','closed'))
            =gfModalGen("TRM04080B00000","DIALOG",lcPoStatus+"|"+'PO')
            lcTktNo = lcOldTktNo
            SHOW GET lcTktNo
            =lfShowFunc()
            RETURN
          ELSE
            SET EXACT &lcSavExact
            IF lcTktNo <> lcOldTktNo
              lcRSession = SPACE(6)
            ENDIF  
          ENDIF
          =lfShowFunc()
        ELSE
          SET EXACT &lcSavExact
          ** MESSAGE : " PO number ð not found.       "
          **           "     < Browse > < Reenter >   "

          IF gfModalGen("TRM04099B00014","DIALOG",lcTPO+"|"+ALLTRIM(lcTktNo)) = 1
            SELECT POSHDR
            DIMENSION laTemp[2]
            laTemp = ''
           *B600460,1 MAN 06/18/95
           *lcSavBrFld=lcBrFields
           *lcSavTitle=lcFile_Ttl
            lnClosRec = RECNO(0)
          
            IF BETWEEN(lnClosRec,1,RECCOUNT('POSHDR'))
              GO lnClosRec
            ELSE
              GO TOP
            ENDIF

            lcFile_Ttl= "Purchase order"
            lcBrFields= "PO,VENDOR :H='Vendor',STATUS=IIF(STATUS = 'C','Complete','Open') :H='Status',ENTERED :H='Entered'"

            =gfBrows([FOR STATUS $ 'CO'],'PO,VENDOR','laTemp')
           *B600460,1 MAN 06/18/95 
           *lcBrFields=lcSavBrFld
           *lcFile_Ttl=lcSavTitle
            
            IF !EMPTY(laTemp[1])
              lcTktNo = laTemp[1]
              =lfShowFunc()
            ELSE
              lcTktNo = lcOldTktNo
              llCanProc = .T.        
              SHOW GET lcTktNo
              =lfShowFunc()
              RETURN
            ENDIF
          ELSE
            lcTktNo = lcOldTktNo
            SHOW GET lcTktNo
            llCanProc = .T.        
            =lfShowFunc()
            _CUROBJ = _CUROBJ
            RETURN
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSE
    SELECT CUTTKTH
    IF EMPTY(lcTktNo)
      =lfShowFunc()
      llCanProc = .T.        
    ELSE
      lcSavExact = SET('EXACT')
      SET EXACT ON
      IF SEEK(lcTktNo)
        SET EXACT &lcSavExact
        IF STATUS $ 'XHS'
          llCanProc = .T.        
          ** MESSAGE : " The status of this purchase "
          **           " order is ð                  "
          **           "            ® Ok ¯           "
          lcPoStatus = IIF(STATUS = 'X','canceled',IIF(STATUS = 'H','hold','closed'))
          =gfModalGen("TRM04080B00000","DIALOG",lcPoStatus+"|"+'CT')
          lcTktNo = lcOldTktNo
          SHOW GET lcTktNo
          =lfShowFunc()
          RETURN
        ELSE
          IF lcTktNo <> lcOldTktNo
            lcRSession = SPACE(6)
          ENDIF  
          =lfShowFunc()
        ENDIF
      ELSE
        SET EXACT &lcSavExact
        ** MESSAGE : " Cutting ticket number ð not found. "
        **           "        < Browse > < Reenter >      "

        IF gfModalGen("TRM04099B00014","DIALOG",lcTCutting+"|"+ALLTRIM(lcTktNo)) = 1
          SELECT CUTTKTH
          DIMENSION laTemp[2]
          laTemp = ''
         *B600460,1 MAN 06/18/95
         *lcSavBrFld=lcBrFields
         *lcSavTitle=lcFile_Ttl
          lnClosRec = RECNO(0)
          
          IF BETWEEN(lnClosRec,1,RECCOUNT('CUTTKTH'))
            GO lnClosRec
          ELSE
            GO TOP
          ENDIF

          lcFile_Ttl= "Cutting ticket"
          lcBrFields= "CUTTKT :H='CT',STYLE :H='Style',Desc :H='Desc.',CSTATUS=IIF(STATUS='A','Actualized',IIF(STATUS='C','Complete','Open')) :H='Status'"
         
          =gfBrows([FOR STATUS $ 'ACO'],'CUTTKT,STYLE','laTemp')
         *B600460,1 MAN 06/18/95 
         *lcBrFields=lcSavBrFld
         *lcFile_Ttl=lcSavTitle
            
          IF !EMPTY(laTemp[1])
            lcTktNo = laTemp[1]
            =lfShowFunc()
          ELSE
            lcTktNo = lcOldTktNo
            llCanProc = .T.        
            SHOW GET lcTktNo
            =lfShowFunc()
            RETURN
          ENDIF
        ELSE
          lcTktNo = lcOldTktNo
          SHOW GET lcTktNo
          llCanProc = .T.        
          =lfShowFunc()
          _CUROBJ = _CUROBJ
          RETURN
        ENDIF
      ENDIF
    ENDIF  
  ENDIF
ENDIF

SHOW GET lcTktNo
=lfShowFunc()

*!**************************************************************************
*!
*!      Function: lfwSession
*!
*!**************************************************************************
*
FUNCTION lfwSession

lcOldRSession = lcRSession

*!**************************************************************************
*!
*!      Function: lfvSession
*!
*!**************************************************************************
*
FUNCTION lfvSession

IF llBrowse .OR. !EMPTY(lcRSession);
            .OR. ATC("?",lcRSession) > 0
  IF llBrowse .OR. ATC("?",lcRSession) > 0
    llBrowse = .F.
*   lcSavBrFld=lcBrFields
*   lcSavTitle=lcFile_Ttl

    SELECT (lcTmp_Rec)
    DIMENSION laTemp[2]
    laTemp = ''

    lnClosRec = RECNO(0)
          
    IF BETWEEN(lnClosRec,1,RECCOUNT(lcTmp_Rec))
      GO lnClosRec
    ELSE
      GO TOP
    ENDIF

    IF lnReceipts = 2
      IF llShipment
        lcFile_Ttl= 'Shippment receiving'
        lcBrFields= "SHIPNO :H= 'Shipment #',CRSESSION :H='Rec. session',VENDOR :H='Vendor',STYLE :H= 'Style'"

        =gfBrows([FOR SHIPNO=lcTktNo],'SHIPNO,CRSESSION','laTemp')
      ELSE
        lcFile_Ttl= 'Purchase order receiving'
        lcBrFields= "PO :H='PO',CRSESSION :H='Rec. session',VENDOR :H='Vendor',STYLE :H='Style'"

        =gfBrows([FOR PO=lcTktNo],'PO,CRSESSION','laTemp')
      ENDIF
    ELSE
      lcFile_Ttl= 'Cutting ticket receiving'
      lcBrFields= "CUTTKT :H='CT',CRSESSION :H='Rec. session',STYLE :H='Style'"

      =gfBrows([FOR CUTTKT=lcTktNo],'CUTTKT,CRSESSION','laTemp')
    ENDIF
   *B600460,1 MAN 06/18/95  
   *lcBrFields=lcSavBrFld
   *lcFile_Ttl=lcSavTitle

    IF !EMPTY(laTemp[1])
      lcRsession = laTemp[2]
    ELSE
      llCanProc = .T.        
      lcRsession = lcOldRSession
      RETURN
    ENDIF
  ENDIF
  
  lcSavExact = SET('EXACT')
  SET EXACT ON

  SELECT (lcTmp_Rec)

  IF !SEEK(lcTktNo+lcRsession)
    SET EXACT &lcSavExact
    ** MESSAGE : " Receiving session ð not exist for ð# ð. "
    **           "         < Browse > < Reenter >          "

    lcRecType = IIF(lnReceipts = 2 .AND. llShipment,'shipment ',IIF(lnReceipts = 2 .AND. !llShipment,'PO ','CT '))

    IF gfModalGen("TRM04094B00014","DIALOG",ALLTRIM(lcRSession)+"|"+lcRecType+"|"+ALLTRIM(lcTktNo)) = 1

*     lcSavBrFld=lcBrFields
*     lcSavTitle=lcFile_Ttl

      SELECT (lcTmp_Rec)
      DIMENSION laTemp[2]
      
      laTemp = ''

      lnClosRec = RECNO(0)

      IF BETWEEN(lnClosRec,1,RECCOUNT(lcTmp_Rec))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF

      IF lnReceipts = 2
        IF llShipment
          lcFile_Ttl= 'Shippment receiving'
          lcBrFields= "SHIPNO :H='Shipment #',CRSESSION :H='Rec. session',VENDOR :H='Vendor',STYLE :H='Style'"
          =gfBrows([FOR SHIPNO=lcTktNo],'SHIPNO,CRSESSION','laTemp')
        ELSE
          lcFile_Ttl= 'Purchase order receiving'
          lcBrFields= "PO :H='PO',CRSESSION :H='Rec. session',VENDOR :H='Vendor',STYLE :H='Style'"
          =gfBrows([FOR PO=lcTktNo],'PO,CRSESSION','laTemp')
        ENDIF
      ELSE
        lcFile_Ttl= 'Cutting ticket receiving'
        lcBrFields= "CUTTKT :H='CT',CRSESSION :H='Rec. session',STYLE :H='Style'"
        =gfBrows([FOR CUTTKT=lcTktNo],'CUTTKT,CRSESSION','laTemp')
      ENDIF

*     lcBrFields=lcSavBrFld
*     lcFile_Ttl=lcSavTitle

      IF !EMPTY(laTemp[1])
        lcRsession = laTemp[2]
      ELSE
        lcRsession = lcOldRSession
        llCanProc = .T.        
        SHOW GET lcRSession
        RETURN
      ENDIF
    ELSE
      llCanProc = .T.        
      SET EXACT &lcSavExact
      lcRsession = lcOldRSession
      SHOW GET lcRSession
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF
ENDIF  

SET EXACT &lcSavExact

*!**************************************************************************
*!
*!      Function: lfwCostTyp
*!
*!**************************************************************************
*
FUNCTION lfwCostTyp

lnOldCostType = lnCostCode
puCostType    = 0

*!**************************************************************************
*!
*!      Function: lfvCostTyp
*!
*!**************************************************************************
*
FUNCTION lfvCostTyp

DO CASE
  CASE _DOS
    lnCostCode = VAL(gfActPop(4,14,10,33,'laCostType',4,1,@lcCostType))
  CASE _WINDOWS

    *B601580,1 Change this line [Begin]
    *lnCostCode = VAL(laCostType[puCostType,4])
    lnCostCode = IIF(puCostType = 0 , lnCostCode , IIF(puCostType <= ALEN(laCostType , 1) , VAL(laCostType[puCostType , 4]) , 0))
    *B601580,1 Change this line [End]

ENDCASE

IF lnCostCode > 0

  IF laCostType[lnCostCode,3] = 'M'
    lcMFG = SPACE(2)
    SHOW GET lcMFG ENABLE
    SHOW GET ibMFG ENABLE
  ELSE
    IF laCostType[lnCostCode,3] = 'P'
      lcMFG = '**'
    ELSE
      lcMFG = '##'
    ENDIF
    SHOW GET lcMFG DISABLE
    SHOW GET ibMFG DISABLE
  ENDIF
ENDIF  

=lfMatRef()

*!**************************************************************************
*!
*!      Function: lfwMFG
*!
*!**************************************************************************
*
FUNCTION lfwMFG

lcOldMFG = lcMFG

*!**************************************************************************
*!
*!      Function: lfvMFG
*!
*!**************************************************************************
*
FUNCTION lfvMFG

IF llBrowse .OR. !EMPTY(lcMFG);
            .OR. ATC("?",lcMFG) > 0
  IF llBrowse .OR. ATC("?",lcMFG) > 0
    llBrowse = .F.
    SELECT CODE
    
    DIMENSION laTemp[2]
    laTemp = ''

*   lcSavBrFld=lcBrFields
*   lcSavTitle=lcFile_Ttl

    lcFile_Ttl= 'Codes'
    lnClosRec = RECNO(0)

    lcBrFields="CODE=SUBSTR(CODE,2) :H= 'Code',;
                CDATA :H= 'Description'"

          
    IF BETWEEN(lnClosRec,1,RECCOUNT('CODE'))
      GO lnClosRec
    ELSE
      GO TOP
    ENDIF
    
    *E100219,8 MAN Added For Condition to prevent browsing Direct MFG codes
    =gfBrows(['M' FOR EMPTY(GLACCOUNT)],'CODE,CDATA','laTemp')
    
*   lcBrFields=lcSavBrFld
*   lcFile_Ttl=lcSavTitle

    IF !EMPTY(laTemp[1])
      lcMFG = SUBSTR(laTemp[1],2,2)
    ELSE
      lcMFG = lcOldMFG
      llCanProc = .T.        
      SHOW GET lcMFG
      RETURN
    ENDIF
  ENDIF
  
  lcSavExact = SET('EXACT')
  SET EXACT ON

  SELECT CODE

  IF !SEEK('M'+lcMFG) 

    SET EXACT &lcSavExact

    ** MESSAGE : " ð not found.                 "
    **           "     < Browse > < Reenter >   "

    lcMFGTxt = 'MFG code '+ALLTRIM(lcMFG)
        
    IF gfModalGen("TRM04099B00014","DIALOG",lcMFGTxt) = 1

      SELECT CODE

      DIMENSION laTemp[2]
      laTemp = ''

*     lcSavBrFld=lcBrFields
*     lcSavTitle=lcFile_Ttl
      lcFile_Ttl= 'Codes'
      lnClosRec = RECNO(0)

      lcBrFields="CODE  :H= 'Code',;
                  CDATA :H= 'Description'"

      IF BETWEEN(lnClosRec,1,RECCOUNT('CODE'))
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF

     *E100219,8 MAN Added For Condition to prevent browsing Direct MFG codes
     =gfBrows(['M' FOR EMPTY(GLACCOUNT)],'CODE,CDATA','laTemp')
    
*     lcBrFields=lcSavBrFld
*     lcFile_Ttl=lcSavTitle

      IF !EMPTY(laTemp[1])
        lcMFG = SUBSTR(laTemp[1],2,2)
      ELSE
        llCanProc = .T.        
        lcMFG = lcOldMFG
      ENDIF
    ELSE
      SET EXACT &lcSavExact
      llCanProc = .T.        
      lcMFG = lcOldMFG
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF

  *E100219,8 MAN Added the ELSE to handle the data entry of MFG Codes that
  *E100219,8 MAN has GL Accounts ( These type of MFG codes applied only from
  *E100219,8 MAN MFG400.PRG)
  ELSE 
    IF !EMPTY(GLACCOUNT)
      SET EXACT &lcSavExact
      
      ** MESSAGE : " MFG Code ð has been setup to be    "
      **           " applied directly through the Cost  "
      **           " Sheet program.                     "
      **           "               ®  Ok  ¯             "
      =gfModalGen("INM04152B00000","DIALOG",IIF(EMPTY(CODE.cData),lcMFG,PROPER(ALLTRIM(CODE.cData)))) 
      llCanProc = .T.        
      lcMFG     = lcOldMFG
      _CUROBJ   = _CUROBJ
      RETURN
    ENDIF  
  ENDIF
  SET EXACT &lcSavExact
ENDIF



*!**************************************************************************
*!
*!      Function: lfwAmount
*!
*!**************************************************************************
*
FUNCTION lfwAmount

lnOldAmount  = lnAmount

*!**************************************************************************
*!
*!      Function: lfvAmount
*!
*!**************************************************************************
*
FUNCTION lfvAmount

IF lnAmount <= 0
  ** MESSAGE : " The cost amount must be greater "
  **           " than zero.                      "
  **           "               ® Ok ¯            "
  =gfModalGen("TRM04121B00000","DIALOG")
  llCanProc = .T.
  lnAmount =  lnOldAmount
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*!**************************************************************************
*!
*!      Function: lfvProceed
*!
*!**************************************************************************
*
FUNCTION lfvProceed

llCanProc = .F.

=lfwTktNo()
=lfvTktNo()

IF llCanProc
  ** MESSAGE : " You have to enter the ð."
  **           "         ® Ok ¯          "

  lcMsgTxt = IIF(lnReceipts = 3,'CT',IIF(llShipment,'ship.#','PO No.'))
  
  =gfModalGen("TRM04066B00000","DIALOG",lcMsgTxt)
  _CUROBJ = OBJNUM(lcTktNo)
  SHOW GET lcTktNo
  RETURN
ELSE
  =lfwSession()
  =lfvSession()
  IF llCanProc .OR. EMPTY(lcRsession)
    ** MESSAGE : " You have to enter the ð."
    **           "         ® Ok ¯          "
    
    lcMsgTxt = 'receiving session'
    =gfModalGen("TRM04066B00000","DIALOG",lcMsgTxt)
    _CUROBJ = OBJNUM(lcRSession)
    SHOW GET lcRSession
    RETURN
  ELSE
    IF lnCostCode = 0
      ** MESSAGE : " You have to enter the ð."
      **           "         ® Ok ¯          "

      lcMsgTxt = 'cost type'
      =gfModalGen("TRM04066B00000","DIALOG",lcMsgTxt)
      IF _DOS
        _CURBOJ = OBJNUM(ibCostType)
        SHOW GET ibCostType
      ELSE

        *B601580,1 Add this line [Begin]
        puCostType = 'Select cost type'
        *B601580,1 Add this line [End]

        _CUROBJ = OBJNUM(puCostType)
        SHOW GET puCostType

        *B601580,1 Add this lines [Begin]
        puCostType = '       '
        SHOW GET puCostType
        lcCostType = 'Select cost type'
        puCostType = 'Select cost type'
        SHOW GET puCostType
        *B601580,1 Add this lines [End]

      ENDIF
      RETURN
    ENDIF
    IF laCostType[lnCostCode,3] = 'M'
      =lfwMFG()
      =lfvMFG()
    ENDIF  
    IF llCanProc
      ** MESSAGE : " You have to enter the ð."
      **           "         ® Ok ¯          "
 
      lcMsgTxt = 'MFG'
      =gfModalGen("TRM04066B00000","DIALOG",lcMsgTxt)
      _CUROBJ = OBJNUM(lcMFG)
      SHOW GET lcMFG ENABLE
      RETURN
    ELSE
      =lfwAmount()
      =lfvAmount()
      IF llCanProc
        ** MESSAGE : " You have to enter the ð."
        **           "         ® Ok ¯          "
*        lcMsgTxt = 'amount'
*        =gfModalGen("TRM04066B00000","DIALOG",lcMsgTxt)
        _CUROBJ = OBJNUM(lnAmount)
        SHOW GET lnAmount    
        RETURN
      ENDIF
    ENDIF  
  ENDIF
ENDIF  

llRecFound = .F.

SELECT (lcTmpBomLn)
**MAN Added Defining lcIMTyp and using it in the seek expr.
lcIMTyp = IIF(lnReceipts = 3,'M','I')
IF SEEK(lcIMTyp+laCostType[lnCostCode,2]+lcMFG+lcTktNo+lcRSession)
  llRecFound = .T.
ENDIF

IF llRecFound
  ** MESSAGE : " This cost item has been applied  "
  **           " to ð.
  **           "                ® Ok ¯            "
  
  ** ð = Shipment #,PO #,or CT # + lcTktNo.
  lcMsgTxt = IIF(lnReceipts = 2 .AND. llShipment,'Shipment # ',IIF(lnReceipts = 2 .AND. !llShipment,'PO # ','CT # '))+ALLTRIM(lcTktNo)
  =gfModalGen("TRM04127B00000","DIALOG",lcMsgTxt)
  RETURN
ENDIF  

SELECT BOMLINE

IF lnReceipts = 2 .AND. llShipment
 **MAN Added CIMTYP to the seek expr.
 IF rbBaseCost = 1 .AND. !SEEK('I2'+laCostType[lnCostCode,2]+lcMFG+lcTktNo+lcRSession)
    ** MESSAGE : " The entered cost type does not exist  "
    **           " in the budget for this receiving. "
    **           " You cannot make your contribution "
    **           " based on the budget cost.         "
    **           "                ® Ok ¯             "
   =gfModalGen("TRM04122B00000","DIALOG")
   IF _DOS
     _CURBOJ = OBJNUM(ibCostType)
   ELSE
     _CUROBJ = OBJNUM(puCostType)
   ENDIF
   RETURN
 ENDIF

ELSE
  **MAN Added Defining lcIMTyp and using it in the seek expr.
  lcIMTyp = IIF(lnReceipts = 3,'M','I')
  IF rbBaseCost = 1 .AND. !SEEK(lcIMTyp+'2'+laCostType[lnCostCode,2]+lcMFG+lcTktNo+lcRSession)
    ** MESSAGE : " The entered cost type not exists  "
    **           " in the budget for this receiving. "
    **           " You cannot make your contribution "
    **           " based on the budget cost.         "
    **           "                ® Ok ¯             "
    =gfModalGen("TRM04122B00000","DIALOG")
    IF _DOS
      _CURBOJ = OBJNUM(ibCostType)
    ELSE
      _CUROBJ = OBJNUM(puCostType)
    ENDIF
    RETURN
  ENDIF
ENDIF

SELECT (lcTmpPORec)

lnTotBud   = 0
lnTotRecQty= 0
lcStyCol   = ''
lnNoOfRec  = lnNoOfRec + 1

APPEND BLANK
REPLACE CTKTNO    WITH lcTktNo,;
        CBOMTYPE  WITH laCostType[lnCostCode,2] ,;
        CRSESSION WITH lcRSession ,;
        CMFGCODE  WITH lcMFG      ,;
        NAMOUNT   WITH lnAmount  

lnTotAmnt = lnTotAmnt + lnAmount

lnBrCstNo = RECNO()
    
SELECT (lcFileInUse)

*B601643,1 Change this line to fix the condition [Begin] 
*IF RECCOUNT() < RECNO()
IF RECCOUNT() >= RECNO()
*B601643,1 Change this line to fix the condition [End]

  GO RECNO()
ENDIF  
lnTItmAmt = 0
DO CASE
  CASE rbBaseCost = 1

    SUM REST BOMLINE.ITEMAMT WHILE &lcCondition FOR &lcForCond TO lnTotBud

    SELECT (lcTmpPORec)

    GO RECNO()
    
    SELECT (lcFileInUse)

    *B601643,1 Change this line to discard records with TOTQTY = 0 [Begin]
    *SCAN REST WHILE &lcCondition FOR &lcForCond
    SCAN REST WHILE &lcCondition FOR &lcForCond .AND. TOTQTY <> 0
    *B601643,1 Change this line to discard records with TOTQTY = 0 [End]

      IF !EOF('BOMLINE')
        SELECT BOMLINE
        SCATTER MEMVAR MEMO
        m.UNITQTY  = 1

        *B601643,1 Change this line to check that lnTotBud and TOTQTY <> 0 [Begin]
        *m.UNITCOST =  BOMLINE.ITEMAMT / lnTotBud *;
        *             lnAmount / &lcFileInUse..TOTQTY
        m.UNITCOST = IIF(lnTotBud <> 0 , BOMLINE.ITEMAMT / lnTotBud *;
                     lnAmount / &lcFileInUse..TOTQTY , 0)
        *B601643,1 Change this line [End]

        m.STYQTY   = &lcFileInUse..TOTQTY
        m.ITEMQTY  = &lcFileInUse..TOTQTY

        *B601643,1 Change this line to check that lnTotBud <> 0 [Begin]
        *m.ITEMAMT  = BOMLINE.ITEMAMT / lnTotBud * lnAmount
        m.ITEMAMT  = IIF(lnTotBud <> 0 , BOMLINE.ITEMAMT / lnTotBud * lnAmount , 0)
        *B601643,1 Change this line [End]
        
        SELECT (lcTmpBomLn)
        APPEND BLANK
        GATHER MEMVAR MEMO
        lnTItmAmt  = lnTItmAmt + ITEMAMT
        lnBrCntNo  = RECNO()
        REPLACE &lcTmpPORec..NCONTRIB WITH &lcTmpPORec..NCONTRIB + ITEMAMT
      ENDIF  
      SELECT (lcFileInUse)
    ENDSCAN

    SELECT (lcTmpBomLn)
    lnOldItmAmt = ITEMAMT
    REPLACE ITEMAMT WITH ITEMAMT + (lnAmount - lnTItmAmt)

    *B601643,1 Add this line to update the value of the UNITCOST field 
    *B601643,1 in the last record of (lcTmpBomLn) [Begin]
    REPLACE UNITCOST WITH ITEMAMT / STYQTY
    *B601643,1 Add this line [End] 

   *REPLACE &lcTmpPORec..NCONTRIB WITH &lcTmpPORec..NCONTRIB + &lcTmpBomLn..ITEMAMT - lnOldItmAmt
    REPLACE &lcTmpPORec..NCONTRIB WITH lnAmount
    
  CASE rbBaseCost = 2

    SUM REST TOTQTY WHILE &lcCondition FOR &lcForCond TO lnTotRecQty
    
    SELECT (lcTmpPORec)
    GO RECNO()
    
    SELECT (lcFileInUse)
    IF lcFileInUse = "POSLN"
      lcTkt_No  = IIF(llShipment,"POSLN.PO","lcTktNo" )     
      lcShip_No = "POSLN.SHIPNO"  
      lcLine_No = "ALLTRIM(STR(POSLN.LINENO))" 
    ELSE
      lcTkt_No  = "lcTktNo"
      lcShip_No = "''"
      lcLine_No = "''"
    ENDIF
    
    *B601643,1 Change this line to discard records with TOTQTY = 0 [Begin]
    *SCAN REST WHILE &lcCondition FOR &lcForCond
    SCAN REST WHILE &lcCondition FOR &lcForCond .AND. TOTQTY <> 0
    *B601643,1 Change this line to discard records with TOTQTY = 0 [Begin]
      
      SELECT (lcTmpBomLn)
      APPEND BLANK
      **MAN Added REPLACE CIMTYP    WITH IIF(lnReceipts = 3,'M','I')
      REPLACE CTKTNO    WITH &lcTkt_No,;
              SHIPNO    WITH &lcShip_No,;
              LINENO    WITH &lcLine_No,;
              STYLE     WITH &lcFileInUse..STYLE,; 
              SCLR      WITH &lcFileInUse..COLOR,;  
              MFGCODE   WITH lcMFG,; 
              UNITQTY   WITH 1,;  
              UNITCOST  WITH (&lcFileInUse..TOTQTY / lnTotRecQty *;
                              lnAmount) / &lcFileInUse..TOTQTY,;  
              STYQTY    WITH &lcFileInUse..TOTQTY,;  
              ITEMQTY   WITH STYQTY,;
              ITEMAMT   WITH &lcFileInUse..TOTQTY / lnTotRecQty * lnAmount,; 
              CBOMTYP   WITH laCostType[lnCostCode,2],;  
              CTYPE     WITH '3',;  
              CCATGTYP  WITH laCostType[lnCostCode,3],; 
              CRSESSION WITH lcRSession  ,;
              CIMTYP    WITH IIF(lnReceipts = 3,'M','I')
      lnTItmAmt = lnTItmAmt + ITEMAMT
      lnBrCntNo = RECNO()
      REPLACE &lcTmpPORec..NCONTRIB WITH &lcTmpPORec..NCONTRIB + ITEMAMT
      SELECT (lcFileInUse)
    ENDSCAN
    SELECT (lcTmpBomLn)
    lnOldItmAmt = ITEMAMT
    REPLACE ITEMAMT WITH ITEMAMT + (lnAmount - lnTItmAmt)

    *B601643,1 Add this line to update the value of the UNITCOST field 
    *B601643,1 in the last record of (lcTmpBomLn) [Begin]
    REPLACE UNITCOST WITH ITEMAMT / STYQTY
    *B601643,1 Add this line [End] 

  * REPLACE &lcTmpPORec..NCONTRIB WITH &lcTmpPORec..NCONTRIB + &lcTmpBomLn..ITEMAMT - lnOldItmAmt
    REPLACE &lcTmpPORec..NCONTRIB WITH lnAmount
ENDCASE
SELECT (lcTmpPORec)
GO RECNO()

lnTotCont = lnTotCont + &lcTmpPORec..NCONTRIB

lnBrCntNo = RECNO(lcTmpBomLn)

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvRemMtSt
*!
*!**************************************************************************
*
FUNCTION lfvRemMtSt
SELECT (lcTmpPORec)
GO RECNO()

SELECT (lcTmpBomLn)

IF lnReceipts = 2
  IF llShipment
   *B600460,1 MAN 06/18/95  Fix the delete While Condition
   *DELETE REST WHILE CBOMTYP+MFGCODE+SHIPNO+CRSESSION+CTKTNO=;
                      &lcTmpPORec..CBOMTYPE+&lcTmpPORec..CMFGCODE+;
                      &lcTmpPORec..CTKTNO+&lcTmpPORec..CRSESSION+SPACE(6)

    DELETE REST WHILE CBOMTYP+MFGCODE+SHIPNO+CRSESSION+CTKTNO=;
                      &lcTmpPORec..CBOMTYPE+&lcTmpPORec..CMFGCODE+;
                      &lcTmpPORec..CTKTNO+&lcTmpPORec..CRSESSION
  ELSE  
    DELETE REST WHILE CBOMTYP+MFGCODE+SHIPNO+CRSESSION+;
                      CTKTNO=&lcTmpPORec..CBOMTYPE+&lcTmpPORec..CMFGCODE+;
                      SPACE(6)+&lcTmpPORec..CRSESSION+&lcTmpPORec..CTKTNO
  ENDIF
ELSE
  DELETE REST WHILE CBOMTYP+MFGCODE+SHIPNO+CRSESSION =;
                    &lcTmpPORec..CBOMTYPE+&lcTmpPORec..CMFGCODE+;
                    SPACE(6)+&lcTmpPORec..CRSESSION
ENDIF

SELECT (lcTmpPORec)

lnTotAmnt = lnTotAmnt - nAmount
lnTotCont = lnTotCont - NCONTRIB

DELETE

lnNoOfRec = lnNoOfRec - 1

IF lnNoOfRec = 0
  SHOW GET pbRemMtSt DISABLE
ELSE
  SKIP 1
  IF EOF()
    SKIP - 1
  ENDIF  
  SHOW GET pbRemMtSt ENABLE
ENDIF  


SHOW WINDOW (lcCostTtl) REFRESH SAME
SHOW WINDOW (lcContTtl) REFRESH SAME

=lfRefMater()

lnBrCstNo = RECNO()
lnBrCntNo = RECNO(lcTmpBomLn)

*!**************************************************************************
*!
*!      Function: lfvInvMtSt
*!
*!**************************************************************************
*
FUNCTION lfvInvMtSt

SELECT(lcTmpPORec)
GO TOP

IF EOF()
  ** MESSAGE : " You did not enter any cost "
  **           " to be invoice.             "
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04073B00000","DIALOG")
  RETURN
ENDIF

LOCATE FOR NCONTRIB <> NAMOUNT
IF FOUND()
  ** MESSAGE : " There are no selected PO "
  **           " receipts.                "
  **           "           ® Ok ¯         "
  =gfModalGen("TRM04128B00000","DIALOG")

  *B601643,1 Add this lines to update the record pointer varibles befor
  *B601643,1 refreshing the browses [Begin]
  lnBrCstNo = RECNO()
  lnBrCntNo = RECNO(lcTmpBomLn)
  *B601643,1 Add this lines [End]

  SHOW WINDOW (lcCostTtl) REFRESH SAME
  SHOW WINDOW (lcContTtl) REFRESH SAME

  *B601643,1 Ther is no need for this lines any more [Begin]
  *lnBrCstNo = RECNO()
  *lnBrCntNo = RECNO(lcTmpBomLn)
  *B601643,1 Ther is no need for this lines any more [End]

  RETURN
ELSE
  
  SELECT CTKTNO,SUM(ITEMAMT) AS 'NTOTAMNT';
    FROM &gcWorkDir.&lcTmpBomLn;
   GROUP BY CTKTNO;
    INTO CURSOR &lcTmpCost

  SELECT (lcTmpCost)

  SUM NTOTAMNT TO lnTotRecAss

  laData[12] = lnTotRecAss

  CLEAR READ
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvCanMtSt
*!
*!**************************************************************************
*
FUNCTION lfvCanMtSt

SELECT (lcTmpBomLn)
ZAP

SELECT (lcTmpPORec)
ZAP

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfSMtStTrp
*!
*!**************************************************************************
*
FUNCTION lfSMtStTrp

ON KEY LABEL TAB     DO lpTabMtSt
ON KEY LABEL BACKTAB DO lpShftMTSt
ON KEY LABEL ALT+N   DO lfKeyNew
ON KEY LABEL ALT+R   DO lfvRemMtSt
ON KEY LABEL ESC     DO lfvCanMtSt

*!**************************************************************************
*!
*!      Function: lfRMtStTrp
*!
*!**************************************************************************
*
FUNCTION lfRMtStTrp

ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL ALT+N
ON KEY LABEL ALT+R
ON KEY LABEL ESC

*!**************************************************************************
*!
*!      FUNCTION : lfKeyNew
*!
*!**************************************************************************
* 
FUNCTION lfKeyNew

KEYBOARD "{SHIFT+HOME}"       && dummy key press to release menu access

=lfvNewMtSt()

*!**************************************************************************
*!
*!      Procedure : lpTabMtSt
*!
*!**************************************************************************
*
PROCEDURE lpTabMtSt

DO CASE
  CASE WONTOP('APMTST3')
    IF lnNoOfRec > 0
      IF _CUROBJ = OBJNUM(pbRemMtSt)
        ACTIVATE WINDOW (lcCostTtl)
      ELSE
        _CUROBJ = _CUROBJ + 1
      ENDIF
    ELSE
      ACTIVATE WINDOW (lcCostTtl)
    ENDIF  

  CASE WONTOP(lcCostTtl)
    ACTIVATE WINDOW (lcContTtl)

  CASE WONTOP(lcContTtl)
    lcStyCol = ''
    ACTIVATE WINDOW 'APMTST4'
    _CUROBJ = OBJNUM(pbInvMtSt)
    SHOW GET pbInvMtSt
    
  CASE WONTOP('APMTST4')
    IF _CUROBJ = OBJNUM(pbCanMtSt)
      ACTIVATE WINDOW ('APMTST3')
      _CUROBJ = OBJNUM(pbNewMtSt)
      SHOW GET pbNewMtSt
    ELSE
      _CUROBJ = _CUROBJ + 1
    ENDIF  

ENDCASE

*!**************************************************************************
*!
*!      Procedure : lpShftMtSt
*!
*!**************************************************************************
*
PROCEDURE lpShftMtSt

DO CASE
  CASE WONTOP('APMTST3')
    IF _CUROBJ = OBJNUM(pbNewMtSt)
      ACTIVATE WINDOW ('APMTST4')
      _CUROBJ = OBJNUM(pbCanMtSt)
      SHOW GET pbCanMtSt
    ELSE
      _CUROBJ = _CUROBJ - 1
      SHOW GET pbNewMtSt
    ENDIF  

  CASE WONTOP(lcCostTtl)
    ACTIVATE WINDOW ('APMTST3')
    IF lnNoOfRec > 0
      _CUROBJ = OBJNUM(pbRemMtSt)
      SHOW GET pbRemMtSt
    ELSE
      _CUROBJ = OBJNUM(pbNewMtSt)
      SHOW GET pbNewMtSt
    ENDIF  

  CASE WONTOP(lcContTtl)
    lcStyCol = ''
    ACTIVATE WINDOW (lcCostTtl)

  CASE WONTOP('APMTST4')
  
    IF _CUROBJ = OBJNUM(pbInvMtSt)
      ACTIVATE WINDOW (lcContTtl)
    ELSE
      _CUROBJ = _CUROBJ - 1
    ENDIF  

ENDCASE


*!**************************************************************************
*!
*!      Function : lfShowFunc
*!
*!**************************************************************************
*
FUNCTION lfShowFunc

IF EMPTY(lcTktNo)
  lcDispTxt1 = 'DISABLE'
  STORE SPACE(6) TO lcRSession
ELSE
  lcDispTxt1 = 'ENABLE'
ENDIF

SHOW GET lcRSession &lcDispTxt1
SHOW GET ibSession  &lcDispTxt1
