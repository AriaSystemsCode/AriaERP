*:************************************************************************
*:
*: Procedure file: APPOREC.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfBrwRec
*:               : lfwBrwRec
*:               : lfvBrwRec
*:               : lfStRecTrp
*:               : lfRsRecTrp
*:               : lpTabRec
*:               : lpShftRec
*:               : lfvSelect
*:               : lfvAll
*:               : lfvNone
*:               : lfvInvert
*:               : lpMClkSel
*:               : lfwPrice
*:               : lfvPrice
*:               : lfwFreight
*:               : lfvFreight
*:               : lfwTax
*:               : lfvTax
*:               : lfwQuota
*:               : lfvQuota
*:               : lfvInvRec
*:               : lfvCanRec
*:               :
*:
*:      Documented 11/11/1994
*:
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*:************************************************************************
* This program is called from the payable invoice screen from the
* button of the P.O. receipts.
*
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laField_H,laCtrStat

ON KEY LABEL TAB     DO lpTabRec
ON KEY LABEL BACKTAB DO lpShftRec

*E300683,1 Call *.SPR from screens directory
* DO APRECPT.SPR 
DO (gcScrDir + gcWinAppl + '\APRECPT.SPR')
*E300683,1 end          

=lfRsRecTrp()

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!**************************************************************************
*!
*!      Function: lfBrwRec
*!
*!**************************************************************************
*
FUNCTION lfBrwRec
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT POFLN

IF llWareHous
  lcBrwStr = "CMARKER=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
             "CMARK=IIF(EOF(lcTmpPORec),' ','û') :R :H='Inc',"+;
             "POFLN.PO :R  :H='PO',"+;
             "POFLN.FABRIC :R :H='Item',"+;
             "POFLN.COLOR :R :H='Color',"+;
             "POFLN.CWARECODE :R :H='W. House',"+;
             "POFLN.DYELOT :R :H='Dyelot',"+;
             "POFLN.TOTQTY :R :H='Quantity',"+;
             "NPRICE =IIF(EOF(lcTmpPORec),POFLN.NLAN_COST1,&lcTmpPORec..NLAN_COST1) :R :H='Price',"+;
             "NFREGHT=IIF(EOF(lcTmpPORec),POFLN.NLAN_COST3,&lcTmpPORec..NLAN_COST3) :R :H='Freight',"+;
             "NTAX   =IIF(EOF(lcTmpPORec),POFLN.NLAN_COST2,&lcTmpPORec..NLAN_COST2) :R :H='Tax',"+;
             "NQUOTA =IIF(EOF(lcTmpPORec),POFLN.NLAN_COST4,&lcTmpPORec..NLAN_COST4) :R :H='QUOTA'"
ELSE
  lcBrwStr = "CMARKER=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
             "CMARK=IIF(EOF(lcTmpPORec),' ','û') :R :H='Inc',"+;
             "POFLN.PO :R  :H='PO',"+;
             "POFLN.FABRIC :R :H='Item',"+;
             "POFLN.COLOR :R :H='Color',"+;
             "POFLN.DYELOT :R :H='Dyelot',"+;
             "POFLN.TOTQTY :R :H='Quantity',"+;
             "NPRICE =IIF(EOF(lcTmpPORec),POFLN.NLAN_COST1,&lcTmpPORec..NLAN_COST1) :R :H='Price',"+;
             "NFREGHT=IIF(EOF(lcTmpPORec),POFLN.NLAN_COST3,&lcTmpPORec..NLAN_COST3) :R :H='Freight',"+;
             "NTAX   =IIF(EOF(lcTmpPORec),POFLN.NLAN_COST2,&lcTmpPORec..NLAN_COST2) :R :H='Tax',"+;
             "NQUOTA =IIF(EOF(lcTmpPORec),POFLN.NLAN_COST4,&lcTmpPORec..NLAN_COST4) :R :H='QUOTA'"
ENDIF             

BROWSE FIELDS &lcBrwStr ;
          FOR POFLN.VENDOR = lcVendor AND POFLN.TRANCD = '2' AND EMPTY(POFLN.CAPINVNO) ;
              WINDOW APRECP1 ;
              WHEN lfwBrwRec();
              VALID :F (WONTOP() = lcRecTtl .AND. lfvBrwRec()) .OR. lfRsRecTrp() ;
              IN WINDOW AWDAPRECPT;
              LOCK 0;
              NOAPPEND;
              NOCLEAR;
              NODELETE;
              SAVE;
              TITLE lcRecTtl;
              NOEDIT &lcClrSchm

IF EOF()
  =gfModalGen('QRM04111B00000','Dialog',ALLTRIM(laData[1]))
  RETURN .F.
ENDIF  

*!**************************************************************************
*!
*!      Function : lfwBrwRec
*!
*!**************************************************************************
*
FUNCTION lfwBrwRec

llConFirm = .F. && Variable to tell if the Zero is confirmed.

lnQuota   = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST4,&lcTmpPORec..NLAN_COST4) && New Modification 02/01/1995
lnTax     = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST2,&lcTmpPORec..NLAN_COST2) && New Modification 02/01/1995
lnFreight = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST3,&lcTmpPORec..NLAN_COST3) && New Modification 02/01/1995
lnPrice   = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST1,&lcTmpPORec..NLAN_COST1) && New Modification 02/01/1995

lcRecObjSt= IIF(EOF(lcTmpPORec),'DISABLE','ENABLE')

SHOW GET lnPrice   &lcRecObjSt
SHOW GET lnFreight &lcRecObjSt
SHOW GET lnTax     &lcRecObjSt
SHOW GET lnQuota   &lcRecObjSt

IF !EOF(lcTmpPORec)
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'  
ENDIF

lnBrRecNo = RECNO('POFLN')

SHOW WINDOW (lcRecTtl) REFRESH

=lfStRecTrp()
=lfRefresh()

*!**************************************************************************
*!
*!      Function : lfvBrwRec
*!
*!**************************************************************************
*
FUNCTION lfvBrwRec

lnBrRecNo = RECNO('POFLN')
SHOW WINDOW (lcRecTtl) REFRESH

=lfRsRecTrp()

*!**************************************************************************
*!
*!      Function: lfStRecTrp
*!
*!**************************************************************************
*
FUNCTION lfStRecTrp

ON KEY LABEL ALT+L   DO lfvSelect
ON KEY LABEL ALT+A   DO lfvAll
ON KEY LABEL ALT+N   DO lfvNone
ON KEY LABEL ALT+I   DO lfvInvert
ON KEY LABEL LEFTMOUSE DO lpMClkSel

*!**************************************************************************
*!
*!      Function: lfRsRecTrp
*!
*!**************************************************************************
*
FUNCTION lfRsRecTrp

ON KEY LABEL ALT+L
ON KEY LABEL ALT+A
ON KEY LABEL ALT+N
ON KEY LABEL ALT+I
ON KEY LABEL LEFTMOUSE

*!**************************************************************************
*!
*!      Procedure : lpTabRec
*!
*!**************************************************************************
*
PROCEDURE lpTabRec

IF WONTOP(lcRecTtl)
  ACTIVATE WINDOW "APRECP2"
  _CUROBJ = OBJNUM(pbSelect)
ELSE
  IF _CUROBJ = OBJNUM(pbCanRec)
    ACTIVATE WINDOW (lcRecTtl)
  ELSE
    _CUROBJ = _CUROBJ + 1
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Procedure : lpShftRec
*!
*!**************************************************************************
*
PROCEDURE lpShftRec

IF WONTOP(lcRecTtl)
  ACTIVATE WINDOW "APRECP2"
  _CUROBJ = OBJNUM(pbCanRec)
ELSE
  IF _CUROBJ = OBJNUM(pbSelect)
    ACTIVATE WINDOW (lcRecTtl)
  ELSE
    _CUROBJ = _CUROBJ - 1
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
*
FUNCTION lfvSelect

SELECT POFLN

IF EOF(lcTmpPORec)
  SELECT POFLN
  lnTotRecAss = lnTotRecAss + (NLAN_COST4+NLAN_COST3+NLAN_COST2+NLAN_COST1) * TOTQTY  && New Modification 02/01/1995
  SCATTER MEMVAR MEMO
  lnRecord = RECNO()
  IF SEEK(POFLN.FABRIC+POFLN.COLOR,'FABRIC')
    IF gfRLock("FABRIC",.T.)
      SELECT(lcTmpPORec)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE NRECNO WITH lnRecord
    ENDIF  
  ENDIF  
ELSE
  SCATTER MEMVAR MEMO BLANK
  SELECT(lcTmpPORec)
  IF !EOF()
    lcFabric = FABRIC
    lcColor  = COLOR
    SKIP 1
    IF FABRIC+COLOR <> lcFabric+lcColor
      SKIP - 1
      =gfRLock("FABRIC",.F.)
    ELSE
      SKIP - 1
    ENDIF
  ENDIF  
  lnTotRecAss = lnTotRecAss - (NLAN_COST4+NLAN_COST3+NLAN_COST2+NLAN_COST1) * TOTQTY  && New Modification 02/01/1995
  GATHER MEMVAR MEMO
  DELETE  
  SELECT POFLN
ENDIF

SELECT POFLN

IF !EOF()
  GO RECNO()
ENDIF  

lnBrRecNo = RECNO('POFLN')

IF !EOF(lcTmpPORec)
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
  lcRecObjSt = 'ENABLE'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'  
  lcRecObjSt = 'DISABLE'
ENDIF

lnQuota   = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST4,&lcTmpPORec..NLAN_COST4) && New Modification 02/01/1995
lnTax     = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST2,&lcTmpPORec..NLAN_COST2) && New Modification 02/01/1995
lnFreight = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST3,&lcTmpPORec..NLAN_COST3) && New Modification 02/01/1995
lnPrice   = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST1,&lcTmpPORec..NLAN_COST1) && New Modification 02/01/1995

SHOW GET lnPrice   &lcRecObjSt
SHOW GET lnFreight &lcRecObjSt
SHOW GET lnTax     &lcRecObjSt
SHOW GET lnQuota   &lcRecObjSt
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
SHOW WINDOW (lcRecTtl) REFRESH

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvAll
*!
*!**************************************************************************
*
FUNCTION lfvAll

SELECT POFLN
lnSavRecNo = RECNO()

SCAN FOR POFLN.VENDOR = lcVendor AND POFLN.TRANCD = '2' AND EMPTY(POFLN.CAPINVNO)

  IF EOF(lcTmpPORec)
    SELECT POFLN
    lnRecord = RECNO()
    lnTotRecAss = lnTotRecAss + (NLAN_COST4+NLAN_COST3+NLAN_COST2+NLAN_COST1) * TOTQTY  && New Modification 02/01/1995
    SCATTER MEMVAR MEMO
    IF SEEK(POFLN.FABRIC+POFLN.COLOR,'FABRIC')
      IF gfRLock("FABRIC",.T.)
        SELECT(lcTmpPORec)
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE NRECNO WITH lnRecord
      ENDIF  
    ENDIF  
  ENDIF  
  SELECT POFLN
ENDSCAN  

SELECT POFLN
IF lnSavRecNo > 0
  GO lnSavRecNo
ENDIF
  
lnBrRecNo = RECNO('POFLN')

SHOW WINDOW (lcRecTtl) REFRESH

lnQuota   = &lcTmpPORec..NLAN_COST4 && New Modification 02/01/1995
lnTax     = &lcTmpPORec..NLAN_COST2 && New Modification 02/01/1995
lnFreight = &lcTmpPORec..NLAN_COST3 && New Modification 02/01/1995
lnPrice   = &lcTmpPORec..NLAN_COST1 && New Modification 02/01/1995

SHOW GET lnPrice   ENABLE
SHOW GET lnFreight ENABLE
SHOW GET lnTax     ENABLE
SHOW GET lnQuota   ENABLE
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvNone
*!
*!**************************************************************************
*
FUNCTION lfvNone

SELECT(lcTmpPORec)
ZAP

UNLOCK IN FABRIC

SELECT POFLN

GO RECNO()

lnBrRecNo = RECNO('POFLN')

SHOW WINDOW (lcRecTtl) REFRESH


lnQuota   = POFLN.NLAN_COST4  && New Modification 02/01/1995
lnTax     = POFLN.NLAN_COST2  && New Modification 02/01/1995
lnFreight = POFLN.NLAN_COST3  && New Modification 02/01/1995

lnPrice   = POFLN.NLAN_COST1  && New Modification 02/01/1995

lnTotRecAss= 0   && Variable to hold the total receipts assigned.

SHOW GET lnPrice   DISABLE
SHOW GET lnFreight DISABLE
SHOW GET lnTax     DISABLE
SHOW GET lnQuota   DISABLE
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
SHOW GET pbSelect,1 PROMPT 'Se\<lect'  

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
*
FUNCTION lfvInvert

SELECT POFLN
lnSavRecNo = RECNO()

SCAN FOR POFLN.VENDOR = lcVendor AND POFLN.TRANCD = '2' AND EMPTY(POFLN.CAPINVNO)
  IF EOF(lcTmpPORec)
    SELECT POFLN
    lnRecord = RECNO()
    lnTotRecAss = lnTotRecAss + (NLAN_COST4+NLAN_COST3+NLAN_COST2+NLAN_COST1) * TOTQTY  && New Modification 02/01/1995
    SCATTER MEMVAR MEMO
    IF SEEK(POFLN.FABRIC+POFLN.COLOR,'FABRIC')
      IF gfRLock("FABRIC",.T.)
        SELECT(lcTmpPORec)
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE NRECNO WITH lnRecord
      ENDIF  
    ENDIF  
  ELSE
    SCATTER MEMVAR MEMO BLANK
    SELECT(lcTmpPORec)
    IF !EOF()
      lcFabric = FABRIC
      lcColor  = COLOR
      SKIP 1
      IF FABRIC+COLOR <> lcFabric+lcColor
        SKIP - 1
        =gfRLock("FABRIC",.F.)
      ELSE
        SKIP - 1
      ENDIF
    ENDIF  
    lnTotRecAss = lnTotRecAss - (NLAN_COST4+NLAN_COST3+NLAN_COST2+NLAN_COST1) * TOTQTY  && New Modification 02/01/1995
    GATHER MEMVAR MEMO
    DELETE
    SELECT POFLN
  ENDIF
  SELECT POFLN
ENDSCAN  

SELECT POFLN

IF lnSavRecNo > 0
  GO lnSavRecNo
ENDIF

IF !EOF(lcTmpPORec)
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
  lcRecObjSt = 'ENABLE'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'  
  lcRecObjSt = 'DISABLE'
ENDIF

lnQuota   = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST4,&lcTmpPORec..NLAN_COST4) && New Modification 02/01/1995
lnTax     = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST2,&lcTmpPORec..NLAN_COST2) && New Modification 02/01/1995
lnFreight = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST3,&lcTmpPORec..NLAN_COST3) && New Modification 02/01/1995
lnPrice   = IIF(EOF(lcTmpPORec),POFLN.NLAN_COST1,&lcTmpPORec..NLAN_COST1) && New Modification 02/01/1995

SHOW GET lnPrice   &lcRecObjSt
SHOW GET lnFreight &lcRecObjSt
SHOW GET lnTax     &lcRecObjSt
SHOW GET lnQuota   &lcRecObjSt
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
lnBrRecNo = RECNO('POFLN')

SHOW WINDOW (lcRecTtl) REFRESH

=lfRefresh()

*!**************************************************************************
*!
*!      Procedure: lpMClkSel
*!
*!**************************************************************************
*If mouse double click select record.
*
PROCEDURE lpMClkSel

IF (SECONDS() < lnTime) .AND. WONTOP()=lcRecTtl
  =lfvSelect()
  SHOW GET pbSelect
ENDIF
lnTime = _DBLCLICK+SECONDS()

*!**************************************************************************
*!
*!      Function: lfwPrice
*!
*!**************************************************************************
*
FUNCTION lfwPrice

lnOldValue = lnPrice

*!**************************************************************************
*!
*!      Function: lfvPrice
*!
*!**************************************************************************
*
FUNCTION lfvPrice

IF lnOldValue = lnPrice .OR. lnPrice < 0
  lnPrice = lnOldValue
  SHOW GET lnPrice
  RETURN
ELSE
  IF lnPrice = 0 .AND. !llConFirm
    ** MESSAGE : " Confirming the zero price. "
    **           "           ® Ok ¯           "
    =gfModalGen("TRM04104B00000","DIALOG")
    llConFirm = .T.
  ENDIF
  lnTotRecAss = lnTotRecAss + (lnPrice - lnOldValue) * &lcTmpPORec..TOTQTY
  REPLACE &lcTmpPORec..NLAN_COST1 WITH lnPrice  && New Modification 02/01/1995
  SHOW GET lnPrice
ENDIF
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
SHOW WINDOW (lcRecTtl) REFRESH SAME

=lfRefresh()
  
*!**************************************************************************
*!
*!      Function: lfwFreight
*!
*!**************************************************************************
*
FUNCTION lfwFreight

lnOldValue = lnFreight

*!**************************************************************************
*!
*!      Function: lfvFreight
*!
*!**************************************************************************
*
FUNCTION lfvFreight

IF lnFreight < 0
  lnFreight = lnOldValue
  SHOW GET lnFreight
  RETURN
ELSE
  lnTotRecAss = lnTotRecAss + (lnFreight - lnOldValue) * &lcTmpPORec..TOTQTY
  REPLACE &lcTmpPORec..NLAN_COST3 WITH lnFreight  && New Modification 02/01/1995
  SHOW GET lnFreight
ENDIF
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
SHOW WINDOW (lcRecTtl) REFRESH SAME

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfwTax
*!
*!**************************************************************************
*
FUNCTION lfwTax

lnOldValue = lnTax

*!**************************************************************************
*!
*!      Function: lfvTax
*!
*!**************************************************************************
*
FUNCTION lfvTax

IF lnTax < 0
  lnTax = lnOldValue
  SHOW GET lnTax
  RETURN
ELSE
  lnTotRecAss = lnTotRecAss + (lnTax - lnOldValue) * &lcTmpPORec..TOTQTY
  REPLACE &lcTmpPORec..NLAN_COST2 WITH lnTax  && New Modification 02/01/1995
  SHOW GET lnTax
ENDIF
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
SHOW WINDOW (lcRecTtl) REFRESH SAME

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfwQuota
*!
*!**************************************************************************
*
FUNCTION lfwQuota

lnOldValue = lnQuota

*!**************************************************************************
*!
*!      Function: lfvQuota
*!
*!**************************************************************************
*
FUNCTION lfvQuota

IF lnQuota < 0
  lnQuota = lnOldValue
  SHOW GET lnQuota
  RETURN
ELSE
  lnTotRecAss = lnTotRecAss + (lnQuota - lnOldValue) * &lcTmpPORec..TOTQTY
  REPLACE &lcTmpPORec..NLAN_COST4 WITH lnQuota  && New Modification 02/01/1995
  SHOW GET lnQuota
ENDIF
SHOW GET lnTotRecAss LEVEL RDLEVE() &&MAN
SHOW WINDOW (lcRecTtl) REFRESH SAME

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvInvRec
*!
*!**************************************************************************
*
FUNCTION lfvInvRec

SELECT(lcTmpPORec)
GO TOP

IF !EOF()
  laData[12] = VAL(STR(lnTotRecAss,15,2))
  SHOW GET laData[12] LEVEL RDLEVEL() - 1
  SHOW GET laData[38] DISABLE LEVEL RDLEVEL() - 1
  SHOW GET ibTemplate DISABLE LEVEL RDLEVEL() - 1
  CLEAR READ
ELSE
  ** MESSAGE : " There are no selected PO "
  **           " receipts.                "
  **           "           ® Ok ¯         "
  =gfModalGen("TRM04108B00000","DIALOG")
  SHOW GET laData[38] ENABLE LEVEL RDLEVEL() - 1
  SHOW GET ibTemplate ENABLE LEVEL RDLEVEL() - 1
  SHOW WINDOW (lcRecTtl) REFRESH SAME
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCanRec
*!
*!**************************************************************************
*
FUNCTION lfvCanRec

SELECT(lcTmpPORec)
ZAP

UNLOCK IN FABRIC

lnTotRecAss = 0

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfDeactive
*!
*!**************************************************************************
*
FUNCTION lfDeactive

IF WONTOP() = lcRecTtl
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
  =lfStRecTrp()
  RETURN .F.
ENDIF
