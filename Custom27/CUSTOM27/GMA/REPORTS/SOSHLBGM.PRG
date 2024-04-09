*:************************************************************************
*: Program file  : SOSHLBGM.Prg
*: Program desc. : SHIPPING LABELS REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO,AR,AL
*: Developer     : Wael Mohammed Anwar (WMA)
*: Date          : 05/11/2000
*: Reference     : C#101808
*:************************************************************************
*: Calls : NONE
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************


*!**************************************************************************
*! Name      : lfBringStr
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 05/11/2000
*! Purpose   : Function to collect (Department, Cust Po, Store and ShipVia) 
*!           : as one string
*!           : to Print In Shipping Labels.
*!**************************************************************************
*! Called from : Label
*!**************************************************************************
*! Calls       : lfBringStr()
*!**************************************************************************
*
FUNCTION lfBringStr
PARAMETER lcDummy
PRIVATE lcShipStr,lcPoStr
lcStr = ""
IF lcXTYPE = 'I'

  = SEEK(Invoice,"INVHDR")
  lcShipStr = gfCodDes(InvHdr.ShipVia,'SHIPVIA',.T.)
  lcStr = INVHDR.DEPT + '     ' + INVHDR.CUSTPO + SPACE(5) + STORE + " " 

ELSE

  *= SEEK(Order,"ORDHDR")
  IF "*" $ OrdHdr.ShipVia
    lcShipStr = Customer.ShipVia
  ELSE
    lcShipStr = OrdHdr.ShipVia
  ENDIF
  lcShipStr = gfCodDes(lcShipStr,'SHIPVIA',.T.)

  IF OrdHdr.MultiPo
    lcPoStr = "*Multi_PO*     "
  ELSE
    lcPoStr = ORDHDR.CUSTPO  
  ENDIF
  lcStr = ORDHDR.DEPT + '     ' + lcPoStr + SPACE(6)

ENDIF
lcStr = lcStr + lcShipStr
RETURN ""
*-- end of lfBringStr