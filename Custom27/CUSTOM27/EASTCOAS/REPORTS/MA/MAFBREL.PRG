*:****************************************************************
*: Program file  : MAFBREL.PRG
*: Program desc. : Fabric Release Form
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : Khalid Mohi El-Din Mohamed
*: Date          : 03/02/2003
*:****************************************************************
*:C#102766,1
*:****************************************************************

CREATE CURSOR (lcTmpFile) (cOrder C(6), cShipVia C(30), cAccount C(5),;
                           cStore C(8), cCustPO C(15), cdyeOrd C(6),;
                           cFabric C(7), cColor C(6), cFabDesc C(20),;
                           cClrDesc C(30), cLot C(30), nAvlQty N(12,3),;
                           nShpQty N(12,3), mNotes M, cType C(1))
                           
INDEX ON cOrder + cType + cFabric + cColor TAG (lcTmpFile) OF (lcTmpFile)


STORE '' TO lcOrders
STORE .F. TO llUseOrder
STORE ".T." TO lcDateFltr
STORE {} TO ldStartSt, ldStartEd, ldComplSt, ldComplEd
DECLARE laCompAdd[7,1], laShipTo[5,1]
STORE '' TO laCompAdd, laShipTo, lcShpTName

*-- To get the selected ware houses if any. 
lnPosition = ASCAN(laOGFxFlt,'MASOHDR.CMORDER')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcOrders   = laOGFxFlt[lnPosition,6]
  llUseOrder = IIF(!EMPTY(lcOrders) .AND. USED(lcOrders) .AND. RECCOUNT(lcOrders)>0,.T.,.F.)
ENDIF

*-- if Status = "B"
lcStatFltr = IIF(lcRpStatus = "B", "Status $ 'OH'", "Status = lcRpStatus" )

*-- To get the Start Date
lnStartPos = ASCAN(laOGFxFlt,'MASOHDR.START')
IF lnStartPos > 0
  lnStartPos = ASUBSCRIPT(laOGFxFlt,lnStartPos,1)
  ldStartSt  = CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],1,;
                  ATC('|',laOGFxFlt[lnStartPos,6])-1))

  ldStartEd  = CTOD(SUBSTR(laOGFxFlt[lnStartPos,6],;
                   ATC('|',laOGFxFlt[lnStartPos,6])+1))

  IF !EMPTY(ldStartEd)
    lcDateFltr = lcDateFltr + " AND BETWEEN(MaSoHdr.Start,ldStartSt,ldStartEd)"
  ENDIF  
ENDIF

*-- To get the Complete Date
lnCompPos = ASCAN(laOGFxFlt,'MASOHDR.COMPLETE')
IF lnCompPos > 0
  lnCompPos = ASUBSCRIPT(laOGFxFlt,lnCompPos,1) 
  ldComplSt = CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],1,;
                  ATC('|',laOGFxFlt[lnCompPos,6])-1))

  ldComplEd  = CTOD(SUBSTR(laOGFxFlt[lnCompPos,6],;
                   ATC('|',laOGFxFlt[lnCompPos,6])+1))

  IF !EMPTY(ldComplSt)
    lcDateFltr = lcDateFltr + " AND BETWEEN(MaSoHdr.Complete,ldComplSt,ldComplEd)"
  ENDIF  
ENDIF

*-- If the user selects by fabric
IF llUseOrder
  =lfSelBySO()
ELSE
  =lfSelByAll()
ENDIF

SELECT (lcTmpFile)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcCompFax  = cCom_fax              && Variable to hold the Company Fax
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
*-- Load Company address.
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6] = 'Phone # '+TRANSFORM(lcCompPhon , lcPhonPict)
laCompAdd[7] = 'Fax # '+TRANSFORM(lcCompFax, lcPhonPict)

DO lfShiftArr WITH laCompAdd

SELECT (lcTmpFile)
DO gfDispRe WITH EVAL('lcRpName')

SET DEVICE TO SCREEN

*!**************************************************************************
*! Name      : lfSelBySO
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 03/02/2003
*! Purpose   : If the user selects by SO
*!**************************************************************************
FUNCTION  lfSelBySO

SELECT(lcOrders)
LOCATE
SCAN
  lcOrdNo = cMOrder
  WAIT WINDOW "Material Sales Order # : " + lcOrdNo NOWAIT      

  IF SEEK('O'+lcOrdNo,'MaSoHdr')
    SELECT MaSoHdr
    *-- Check the validity of status if
    *-- Check the validity of start date And Check the validity of complete date
    *-- Check if there is assigned dye order.
    *-- Check if the assigned dye order do exist in the Mfg order header file.
    *-- Check if there is any receiving quantities in the Mfg order  
    IF !EVALUATE(lcStatFltr) OR !EVALUATE(lcDateFltr) OR EMPTY(MaSoHdr.cDye) OR;
       !SEEK(MaSoHdr.cDye, 'MmfgOrdH') OR MmfgOrdH.Received = 0
      LOOP
    ENDIF
       
    *-- Get the manufacturing order details.
    =lfGetLines()
      
  ENDIF  
ENDSCAN

*!**************************************************************************
*! Name      : lfSelByAll
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : If the user selects start or complete date.
*!**************************************************************************
FUNCTION lfSelByAll

SELECT MaSoHdr
SEEK 'O'
SCAN REST WHILE cOrdType+cMOrder = 'O'
  lcOrdNo = cMOrder
  WAIT WINDOW "Material Sales Order # : " + lcOrdNo NOWAIT      
  *-- Check the validity of status if
  *-- Check the validity of start date And Check the validity of complete date
  *-- Check if there is assigned dye order.
  *-- Check if the assigned dye order do exist in the Mfg order header file.
  *-- Check if there is any receiving quantities in the Mfg order  
  IF !EVALUATE(lcStatFltr) OR !EVALUATE(lcDateFltr) OR EMPTY(MaSoHdr.cDye) OR;
     !SEEK(MaSoHdr.cDye, 'MmfgOrdH') OR MmfgOrdH.Received = 0
    LOOP
  ENDIF
      
  *-- Get the manufacturing order details.
  =lfGetLines()
              
ENDSCAN

*!**************************************************************************
*! Name      : lfSetOrdRp
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 03/02/2003
*! Purpose   : control the SO browse 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSetOrdRp()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
FUNCTION lfSetOrdRp
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
IF lcParm = 'S'  && Set code
    GO TOP IN MASOHDR
ENDIF
*-- End of lfSetOrdRp.

*!**************************************************************************
*! Name      : lfGetShpQt
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 03/02/2003
*! Purpose   : Get the shiped quantity
*!**************************************************************************
*! Example   : =lfGetShpQt()
*!**************************************************************************
FUNCTION lfGetShpQt
PARAMETERS lcFabric, lcColor, lcWareCode, lcDyelot, lcRSess
PRIVATE lnQty, lnAlias
lnQty = 0
lnAlias = SELECT(0)
SELECT MatInvJl
*cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
IF SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRSess)
  SUM REST nIssued TO lnQty;
      WHILE cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = ;
            lcFabric+lcColor+lcWareCode+lcDyelot+lcRSess;
      FOR cTranType = "5"       
ENDIF
SELECT(lnAlias)
RETURN lnQty


*!**************************************************************************
*! Name      : lfGetLines
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 03/02/2003
*! Purpose   : Get the manufacturing order details
*!**************************************************************************
*! Example   : =lfGetLines()
*!**************************************************************************
FUNCTION lfGetLines
PRIVATE lnAlias, lcClr, llAddRec
lnAlias = SELECT(0)
llAddRec = .F.

IF SEEK(MaSoHdr.cDye, 'MmfgOrdD')
  *-- Get the shipvia description
  lcShipVia = gfCodDes(MaSoHdr.ShipVia ,PADR('SHIPVIA',10))

  SELECT MmfgOrdD        
  lcClr   = Color

  SCAN REST WHILE cMfgOrdNo+cFabric+Color+Dyelot+Trancd = MaSoHdr.cDye;
            FOR TranCd = "2"
    

    *- Get the shipped quantity
    lnShipQty = lfGetShpQt(cFabric,Color,cWareCode,Dyelot,cRSession)
        
    IF nMfgTotQty - lnShipQty > 0
      llAddRec = .T.

      =SEEK(cFabric+Color ,'Fabric')
      lcClrDes  = gfCodDes(Color ,PADR('COLOR',10))
          
      INSERT INTO (lcTmpFile) ;
             (cOrder, cShipVia, cAccount, cStore, cCustPO, cdyeOrd, cFabric, cColor,;
              cFabDesc, cClrDesc, cLot, nAvlQty,  cType);
      VALUES (lcOrdNo, lcShipVia, MaSoHdr.Account, MaSoHdr.Store, MaSoHdr.CustPO,MaSoHdr.cDye,;
              MmfgOrdD.cFabric, MmfgOrdD.Color, Fabric.Desc, lcClrDes, MmfgOrdD.Reference,;
              MmfgOrdD.nMfgTotQty - lnShipQty, 'A')
                  
    ENDIF
  ENDSCAN

  *-- Print note pad
  IF llAddRec
    =SEEK('V'+lcOrdNo, 'NotePad')     
    INSERT INTO (lcTmpFile) ;
           (cOrder, cShipVia, cAccount, cStore, cCustPO, cdyeOrd, mNotes, cType);
    VALUES (lcOrdNo, lcShipVia, MaSoHdr.Account, MaSoHdr.Store, MaSoHdr.CustPO,MaSoHdr.cDye,;              
            IIF(llRpPrnNPd, NotePad.mNotes,''), 'B')                        
  ENDIF
ENDIF
SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 03/02/2003
*! Purpose   : Get the customer shipto address
*!**************************************************************************
*! Example   : =lfGetAddr()
*!**************************************************************************
FUNCTION lfGetAddr
lcAlias = ALIAS()

=SEEK(IIF(EMPTY(cStore),'M'+cAccount,'S'+cAccount+cStore),'Customer')
lcShpTName = IIF(EMPTY(Customer.DBA) , Customer.STName , Customer.DBA)

laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

DO lfShiftArr WITH laShipTo
SELECT &lcAlias
RETURN ''

*!**************************************************************************
*! Name      : lfShiftArr
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 03/02/2003
*! Purpose   : To shift the array of addresses.
*!**************************************************************************
*! Example   : =lfShiftArr()
*!**************************************************************************
FUNCTION lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
* Get length of the array
lnALen = ALEN(laArray,1)
* check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR
