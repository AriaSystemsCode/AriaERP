*:***************************************************************************
*: Program file  : ARPINVPH.PRG
*: Program desc. : Customized Invoice for Phonex.
*: 				   The program is a converted from 2.6 (REFER TO C100988)
*: Date          : 07/31/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat(SSE)
*: Tracking Job Number: C102679
*:***************************************************************************
*: Example : DO ARPINVPH
*:***************************************************************************
*

SET ORDER TO TAG cCode_No IN Codes

*-- Search for company Phone number in laCompAdd
PRIVATE lnPhonePos
lnPhonePos = ASCAN(laCompAdd,TRANSFORM(lcCompPhon , lcPhonPict))
IF lnPhonePos > 0
  laCompAdd[lnPhonePos] = "Phone# : " + laCompAdd[lnPhonePos]
  DIMENSION laCompAdd[lnPhonePos+1]
  laCompAdd[lnPhonePos+1] = "Fax # : " + lcCompFax

  IF EMPTY(laCompAdd[3])
    laCompAdd[3] = laCompAdd[lnPhonePos]
    laCompAdd[4] = "Fax # : " + lcCompFax
  ELSE
    laCompAdd[4] = laCompAdd[lnPhonePos]
    laCompAdd[5] = "Fax # : " + lcCompFax
  ENDIF
ELSE
  IF EMPTY(laCompAdd[3])
    laCompAdd[3] = "Fax # : " + lcCompFax
  ELSE
    laCompAdd[4] = "Fax # : " + lcCompFax
  ENDIF  
ENDIF

*-- Get Style Major & Non Major
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'   && Free (Style)
    lnStyleLen = LEN(laItemSeg[lnCount,3])
    lnStylePos = laItemSeg[lnCount,4]
  ENDIF
  IF laItemSeg[lnCount,1]='C'   && Color
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*-- End of Report.

*!***************************************************************************
*! Name      : lfSkuSize
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 07/31/2002
*! Purpose   : To fill laSku array. 
*:***************************************************************************
*
FUNCTION lfSkuSize
PARAMETERS llDummy
llDummy = .T.

*IIF(llPrtSku,'Size:'+ALLTRIM(STR(lnSpkLin))+ ' '+ALLTRIM(Spck_lin.Pack_ID),'')

PRIVATE lnPrevAl
STORE ' ' TO lcStrToPrn
STORE '' TO laSku

IF !SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF
lnPrevAl = SELECT (0)
SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style AND lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku[lnX] = 'Size:' + ALLTRIM(STR(lnX)) + ' ' + ALLTRIM(Spck_lin.Pack_ID)
        lcStrToPrn = lcStrToPrn + laSku[lnx]
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
ENDIF
SELECT (lnPrevAl)
IF EMPTY(lcStrToPrn)
  STORE '' TO laSku
  llPrtSku = .F.
  RETURN .F.
ELSE
  llPrtSku = .T.
  RETURN .T.
ENDIF
*-- End of lfSkuSize.
