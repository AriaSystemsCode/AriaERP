SET STEP ON 

*** 
*** ReFox XI+  #WR654772  Mariam  Mariam [VFP90]
***
lnalias = SELECT(0)
IF !EMPTY(ordhdr.cinvnam)
 IF ordhdr.cinvnam = 'SAME'
  lcsoltname = lcshptname
  lasoldto[1] = lashipto(1)
  lasoldto[2] = lashipto(2)
  lasoldto[3] = lashipto(3)
  lasoldto[4] = lashipto(4)
  lasoldto[5] = lashipto(5)
 ELSE
  lcsoltname = ordhdr.cinvnam
  lasoldto[1] = ordhdr.cinvadd1
  lasoldto[2] = ordhdr.cinvadd2
  lasoldto[3] = ordhdr.cinvadd3
  lasoldto[4] = ordhdr.cinvadd4
  lasoldto[5] = ordhdr.cinvadd5
 ENDIF
ENDIF

DO lfshiftarr WITH lasoldto
IF !USED('pack_hdr')
 = gfopentable('pack_hdr', 'pack_hdr')
 SELECT invhdr
 SET RELATION ADDITIVE TO piktkt INTO pack_hdr
ENDIF
DIMENSION labank[1]
STORE SPACE(0) TO labank
IF gfseek('T' + invhdr.ccurrcode, 'NOTEPAD')
 lnrows = ALINES(labank, notepad.mnotes)
 DO lfshiftarr WITH labank
ENDIF
IF ALEN(labank, 1) < 4
 DIMENSION labank[4]
ENDIF
STORE SPACE(0) TO lcordnotes
IF llrpordnotes
 lnrows = 0
 IF gfseek('B' + invhdr.order, 'NOTEPAD')
  lnrows = ALINES(laarray, notepad.mnotes)
 ENDIF
 IF lnrows > 0
  DO lfshiftarr WITH laarray
  lnrows = ALEN(laarray, 1)
  IF lnrows > 0
   FOR lni = 1 TO lnrows
    lcordnotes = lcordnotes + IIF(lni > 1, CHR(13) + CHR(10), '') + laarray(lni)
   ENDFOR
  ENDIF
 ENDIF
ENDIF
SELECT (lnalias)
RETURN ''
ENDFUNC
**
PROCEDURE lfShiftArr
PARAMETER laarrayparm
PRIVATE lnalen, lncount, lnc
lnalen = ALEN(laarrayparm, 1)
FOR lncount = 1 TO lnalen
 IF EMPTY(laarrayparm(lncount))
  FOR lnc = lncount TO lnalen - 1
   laarrayparm[lnc] = laarrayparm(lnc + 1)
  ENDFOR
  laarrayparm[lnalen] = ''
 ENDIF
ENDFOR
ENDPROC
**
*** 
*C202424,1 MMT 06/15/2021 Add new format for IRELAND[Start]
FUNCTION lfNetWght
LPARAMETERS lcinvoice
lcPiktktNo = Invhdr.Piktkt
lnAlias = ALIAS()
IF !EMPTY(lcPiktktNo)
  IF !USED('PACK_HDR_A')
    =gfOpenTable('PACK_HDR','PACK_HDR','SH','PACK_HDR_A')
  ENDIF
  IF !USED('INVHDR_A')
    =gfOpenTable('INVHDR','INVHDRPK','SH','INVHDR_A')
  ENDIF
  IF !USED('INVLINE_A')
    =gfOpenTable('INVLINE','INVLINE','SH','INVLINE_A')
  ENDIF
  IF !USED('STYLE_A')
    =gfOpenTable('STYLE','STYLE','SH','STYLE_A')
  ENDIF
  lnRetWght = 0
  *Style.ninpackwgt
  =gfSeek(lcPiktktNo,'PACK_HDR_A','PACK_HDR')
  lcConsgmnt = PACK_HDR_A.consgment 
  lnCartons = PACK_HDR_A.noofcarton
  IF EMPTY(ALLTRIM(lcConsgmnt))
    SELECT(lnAlias) 
    RETURN 0
  ENDIF
  *XX
  *SELECT SUM(INVHDR_A.weight) as weight FROM INVHDR_A WHERE PIKTKT in;
  (SELECT DISTINCT PACK_NO FROM PACK_HDR_A WHERE !DELETED() AND consgment = lcConsgmnt) INTO CURSOR 'InvWght'
  SELECT DISTINCT PACK_NO,ACCOUNT FROM PACK_HDR_A WHERE !DELETED() AND consgment = lcConsgmnt INTO CURSOR 'ConsPack'
  SELECT 'ConsPack'
  LOCATE
  SCAN
    SELECT INVHDR_A
    =gfSeek(ConsPack.Pack_no)
    LOCATE REST WHILE piktkt+INVOICE = ConsPack.PACK_NO FOR Status <> 'V' && PIKTKT = ConsPack.PACK_NO AND
    IF FOUND()
      SELECT INVLINE_A
      =gfSeek(INVHDR_A.INVOICE)
      SCAN REST WHILE  INVOICE+STR(LINENO,6) = INVHDR_A.INVOICE
        =gfSeek(INVLINE_A.Style,'STYLE_A','STYLE')
        lnRetWght = lnRetWght + (STYLE_A.ninpackwgt * INVLINE_A.TotQty)
      ENDSCAN 
    ENDIF
  ENDSCAN
  USE IN ConsPack
  SELECT(lnAlias)
  RETURN lnRetWght
  *XX
  SELECT InvWght
  LOCATE  
  IF !ISNULL(InvWght.weight)
    lnRetWght = InvWght.weight
    USE IN InvWght
    SELECT(lnAlias)
    RETURN lnRetWght
  ELSE
    SELECT(lnAlias)  
    RETURN 0
  ENDIF  
ELSE
  SELECT(lnAlias)
  RETURN 0
ENDIF

FUNCTION lfGrsWght
LPARAMETERS lcInvoice
lnCartons = 0
lnWghtNet = lfNetWght (lcInvoice)
lnCrtWeight = gfGetMemVar('M_STBOXWGH')
IF TYPE('lnCrtWeight') <> 'N' OR lnCrtWeight = 0
  RETURN lnWghtNet 
ELSE 
  lnWghtNet = lnWghtNet + lnCartons * lnCrtWeight 
  RETURN lnWghtNet 
ENDIF
*C202424,1 MMT 06/15/2021 Add new format for IRELAND[End]