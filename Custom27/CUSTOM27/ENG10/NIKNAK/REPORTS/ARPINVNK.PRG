*:***************************************************************************
*: Program file  : ARPINVNK.PRG
*: Program desc. : CUSTOMIZED INVOICE FORM FOR NIK NAK
*: Date          : 09/07/2004
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C123649
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVNK
*:***************************************************************************
*: Modifications :
***************************************************************************

IF !USED('POCRTNMF')
  = gfOpenFile(gcDataDir + 'POCRTNMF' , gcDataDir + 'POPIKTKT', 'SH')
ENDIF

IF !USED('CONSINVH')
  = gfOpenFile(gcDataDir + 'CONSINVH' , gcDataDir + 'CONSINVH', 'SH')
ENDIF

               *-- Section break the relatoins --*
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO INVLINE
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT INVLINE
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
             *-- End Section break the relatoins --*

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
STORE SPACE(0) TO  lnStyLnNK , lnStyPosNK
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnNK  = LEN(laItemSeg[lnCount,3])
    lnStyPosNK = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

=lfCreatTmp()  && Create work cursor.
=lfCollData()  && Collect the data.

SELECT (lclinesNK)
SET ORDER TO TAG (lclinesNK)

            *-- Section create the relatoins --*
SELECT INVHDR
IF llPrntInst .OR. llRpInvNot
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.INVOICE , '*') INTO &lclinesNk ADDITIVE
ELSE
  SET RELATION TO INVHDR.INVOICE INTO &lclinesNk ADDITIVE
ENDIF

SELECT (lclinesNk)
LOCATE

SET RELATION TO EVAL(lclinesNk+'.STYLE') INTO STYLE ADDITIVE
SET RELATION TO "S" + EVAL(lclinesNk+'.Account') + EVAL(lclinesNk+'.STYLE') INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
          'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO ORDHDR ADDITIVE
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , (lclinesNk)
ELSE
  SET SKIP TO (lclinesNk)
ENDIF

SELECT INVHDR
                   *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/07/2004
*! Purpose   : Create Temp. file that hold the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp
PRIVATE lcAlias

lcAlias = ALIAS()

SELECT INVLINE
DIMENSION laInvLnFld[1,4]
=AFIELDS(laInvLnFld)

=lfAddField("laInvLnFld", "StylOnly" ,"C",lnStyLnNK,0)

CREATE CURSOR (lclinesNk) FROM ARRAY laInvLnFld
SELECT (lclinesNk)
*-- You Must Make A Zap To decoument The File After Cursor on The Hard Disk
ZAP
INDEX ON INVOICE + STR(LINENO,6) TAG (lclinesNk) OF (gcWorkDir + lclinesNk + '.CDX')
INDEX ON INVOICE + StylOnly TAG 'StylOnly' OF (gcWorkDir + lclinesNk + '.CDX') ADDITIVE

SELECT(lcAlias)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/07/2004
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ARPINVNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcAlias , lcRecNo

lcAlias = ALIAS()

SELECT INVHDR
lcRecNo = EVAL(KEY())

SCAN FOR &lcRpExp
  SELECT INVLINE
  =SEEK(INVHDR.INVOICE)
  SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
    IF SEEK(INVLINE.INVOICE + SUBSTR(INVLINE.STYLE , lnStyPosNK , lnStyLnNK) ,  lclinesNk )
      REPLACE &lclinesNk..Qty1   WITH &lclinesNk..Qty1   + INVLINE.Qty1     ,;
              &lclinesNk..Qty2   WITH &lclinesNk..Qty2   + INVLINE.Qty2     ,;
              &lclinesNk..Qty3   WITH &lclinesNk..Qty3   + INVLINE.Qty3     ,;
              &lclinesNk..Qty4   WITH &lclinesNk..Qty4   + INVLINE.Qty4     ,;
              &lclinesNk..Qty5   WITH &lclinesNk..Qty5   + INVLINE.Qty5     ,;
              &lclinesNk..Qty6   WITH &lclinesNk..Qty6   + INVLINE.Qty6     ,;
              &lclinesNk..Qty7   WITH &lclinesNk..Qty7   + INVLINE.Qty7     ,;
              &lclinesNk..Qty8   WITH &lclinesNk..Qty8   + INVLINE.Qty8     ,;
              &lclinesNk..TotQty WITH &lclinesNk..TotQty + INVLINE.TotQty
    ELSE
      SCATTER MEMVAR MEMO
      SELECT (lclinesNk)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE &lclinesNk..StylOnly WITH SUBSTR(IIF(EMPTY(INVLINE.ALTSTYLE) , INVLINE.STYLE , INVLINE.ALTSTYLE ), lnStyPosNK , lnStyLnNK)
    ENDIF
  ENDSCAN
ENDSCAN

SELECT INVHDR
=SEEK(lcRecNo)

SELECT(lcAlias)

*--End of lfCollData
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/07/2004
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from : ARPINVNK.PRG
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfCartons
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/07/2004
*! Purpose   : Function to Get the number of cartons.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfCartons
PARAMETER lcCartons
PRIVATE lcAlias , lccartno

lcAlias = ALIAS()
lnCartons = 0

SELECT INVHDR
IF INVHDR.CONSOL = "Y"
  SELECT CONSINVH
  IF SEEK(INVHDR.INVOICE , 'CONSINVH')
    SCAN REST WHILE INVOICE + STORE + ORDER = INVHDR.INVOICE
	  IF SEEK(CONSINVH.PIKTKT , 'POCRTNMF')
        SELECT POCRTNMF
          lccartno = ""
          SCAN REST WHILE PIKTKT = CONSINVH.PIKTKT
            IF lccartno <> POCRTNMF.CCARTONNO
              lnCartons = lnCartons + 1
              lccartno = POCRTNMF.CCARTONNO
            ENDIF
          ENDSCAN
	  ENDIF
    ENDSCAN
  ENDIF
ELSE
  IF SEEK(INVHDR.PIKTKT , 'POCRTNMF')
    SELECT POCRTNMF
    lccartno = ""
    SCAN REST WHILE PIKTKT = INVHDR.PIKTKT
      IF lccartno <> POCRTNMF.CCARTONNO
        lnCartons = lnCartons + 1
        lccartno = POCRTNMF.CCARTONNO
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SELECT(lcAlias)
RETURN ''

*--End of lfCartons.