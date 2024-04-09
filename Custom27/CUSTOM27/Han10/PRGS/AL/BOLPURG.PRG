*:************************************************************************
*: Program file  : BOLPURG
*: Program desc. : Purge BOL 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Allocation
*:      Developer: Hend Ghanem (HBG)
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************

ldDate = {}

DO (gcScrDir + gcWinAppl + '\BOLPURG.SPR')



*!***************************************************************************
*!
*!         FUNCTION :lfvOK
*!
*!***************************************************************************
*
FUNCTION lfvOK

IF EMPTY(ldDate)
  = gfModalGen("INM00000B00000","Dialog",.F.,.F.,'You must enter shipping date.') 
  RETURN
ENDIF
IF ldDate > gdSysDate
  = gfModalGen("INM00000B00000","Dialog",.F.,.F.,'Shipping date cannot be greater than System date.') 
  RETURN
ENDIF

= gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')
= gfOpenFile(gcDataDir+'Bol_Lin',gcDataDir+'Bol_Lin','SH')
= gfOpenFile(gcDataDir+'PikTkt',gcDataDir+'PikTkt','SH')
= gfOpenFile(gcDataDir+'PikLine',gcDataDir+'PikLine','SH')
= gfOpenFile(gcDataDir+'Pack_Hdr',gcDataDir+'Pack_Hdr','SH')
= gfOpenFile(gcDataDir+'Pack_Lin',gcDataDir+'Pack_Lin','SH')
= gfOpenFile(gcDataDir+'ASN_SHIP',gcDataDir+'ASN_SHIP','SH')
= gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

llFound = .F.

SELECT BOL_HDR
SCAN FOR !EMPTY(Ship_date) AND Ship_date <= ldDate
  llFound = .T.
  *-- Deleting from PACK Header
  SELECT PACK_HDR
  SCAN FOR Bill_Ladg = BOL_HDR.BOL_NO AND Ship_date <= ldDate
    *-- Deleting from PACK Detail
    SELECT PACK_LIN
    IF SEEK(PACK_HDR.Pack_no)
      DELETE FOR pack_no+STR(line_no,6)+style+cpackcolor = PACK_HDR.Pack_no AND dadd_date <= ldDate
    ENDIF  
    *-- Deleting from Pick ticket header
    SELECT PIKTKT
    IF SEEK(PACK_HDR.PIKTKT)
      DELETE FOR PIKTKT = PACK_HDR.PIKTKT AND Date <= ldDate
    ENDIF
    *-- Deleting from Pick ticket Detail    
    SELECT PIKLINE
    IF SEEK(PACK_HDR.PIKTKT)
      DELETE FOR piktkt+order+STR(lineno,6) = PACK_HDR.PIKTKT AND Start <= ldDate
    ENDIF
  ENDSCAN
  DELETE FOR Bill_Ladg = BOL_HDR.BOL_NO AND Ship_date <= ldDate
  *-- Deleting from BOL Detail
  SELECT BOL_LIN
  IF SEEK(BOL_HDR.BOL_NO)
    DELETE FOR bol_no+order+pack_no = BOL_HDR.BOL_NO
  ENDIF
  *-- Deleting from ASN_SHIP file
  SELECT ASN_SHIP
  IF SEEK(BOL_HDR.BOL_NO)
    DELETE FOR bol_no+pack_no+STR(cart_no,6)+asn_ver = BOL_HDR.BOL_NO
  ENDIF
  *-- Deleting from EDI Transaction file
  SELECT EDITRANS
  IF SEEK('856'+PADR(BOL_HDR.BOL_NO,20))
    DELETE FOR ceditrntyp+key+type+cpartner = '856'+PADR(BOL_HDR.BOL_NO,20)
  ENDIF
ENDSCAN

*-- Deleting from BOL Header
DELETE FOR !EMPTY(Ship_date) AND Ship_date <= ldDate

IF llFound 
  = gfModalGen("INM00000B00000","Dialog",.F.,.F.,'Purging BOL Finished successfully.') 
ELSE
  = gfModalGen("INM00000B00000","Dialog",.F.,.F.,'No BOLs have been shipped on/before this date.') 
ENDIF