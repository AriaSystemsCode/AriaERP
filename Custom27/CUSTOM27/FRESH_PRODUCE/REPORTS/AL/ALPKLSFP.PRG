****************************************************************************
*: Program file  : ALPKLSFP.PRG --- ALPKLSFP.FRX
*: Program desc. : Print Packing List	( new form for Fresh Produce )
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : WAB - WALID A. WAHAB 
*: Date          : 10/05/1999
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:				      : New Form code.
*:**************************************************************************
*:Modifications :
*:CPR# :C200094
*C200137,1 TAK 10/10/2000
*B#604366,1 SSH Fix the bug of not print all the selected piacking List.
*:**************************************************************************

**C200137,1
*IF !USED(lcLogBmp)  
*  CREATE CURSOR (lcLogBmp) (gPic G)
*  APPEND BLANK
*  APPEND GENERAL gPIC FROM (gcBmpHome+'fp_pslip.bmp')
*ENDIF
*Ren end

*-- open some files didn't opened in main prg
IF !USED(lcBomVar)
  =gfOpenFile(gcDataDir+'BOMVAR','BOMVAR','SH', @lcBomVar, .T.)
ENDIF
IF !USED(lcNamDrp)
=gfOpenFile(gcDataDir+'ICNAMDRP','ICNAMDRPID','SH', @lcNamDrp, .T.)
ENDIF
IF !USED(lcDesign)
=gfOpenFile(gcDataDir+'ICDESIGN','ICDESIGN','SH' , @lcDesign, .T.)
ENDIF
DECLARE laStruTemp[1,4] , lallshpext[1]

*--call function to create cursor 
=lfCreatTmp()


SELECT ORDLINE
SET ORDER TO ORDLINE
*--add relation between ordhdr and ordline
*SELECT ORDHDR
*SET RELATION TO 'O'+ORDHDR.Order INTO ORDLINE ADDITIVE


SELECT (lcPackTmp)
GO TOP
=lfHeadVar()
SCAN
  LnRecNo = 0
  DO CASE 
    CASE nRprtTyp = 1
      SELECT INVLINE
      *B#604366,1 SSH Fix the bug of not print all the selected invoice.
      =SEEK(INVHDR.INVOICE)
      *B#604366,1 SSH [END]
      SCAN WHILE INVLINE.INVOICE = INVHDR.INVOICE
        LnRecNo = LnRecNo + 1 
        SCATTER  MEMVAR MEMO
        *--get name drop id from bomvar
        IF SEEK('SO'+ORDER+STR(LineNo,6),lcBomVar)
          LnRecNo = LnRecNo + 1 
          m.cNDrpId   = &lcBomVar..cNDrpId
          m.cDsgnCode = &lcBomVar..cDsgnCode
        ELSE
          STORE '' TO m.cNDrpId,m.cDsgnCode
        ENDIF
        INSERT INTO (lcDetlTmp) FROM MEMVAR
      ENDSCAN
      LnRecNo = lftotpage(LnRecNo)
    CASE nRprtTyp = 2           && pick ticket not found in pack header file. 
      
      SELECT PACK_LIN
      *B#604366,1 SSH Fix the bug of not print all the selected pick ticket.
      =SEEK(&lcPackTmp..Pack_No)
      *B#604366,1 SSH [END]
      SCAN WHILE PACK_LIN.Pack_No = &lcPackTmp..Pack_No
        LnRecNo = LnRecNo + 1 
        m.Invoice    = Pack_No
        m.Style      = Style  
        m.TotQty     = TotQty
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          m.Qty&lcCount = Qty&lcCount
        ENDFOR
        *--get name drop id from bomvar
        IF SEEK('SO'+&lcPackTmp..ORDER+STR(nordlineno,6), lcBomVar )
          LnRecNo = LnRecNo + 1 
          m.cNDrpId   = &lcBomVar..cNDrpId
          m.cDsgnCode = &lcBomVar..cDsgnCode
        ELSE
          STORE '' TO m.cNDrpId,m.cDsgnCode
        ENDIF
        *--get gros_price from orderline
        IF SEEK('O'+&lcPackTmp..ORDER+STR(nordlineno,6),'ORDLINE')
          m.Gros_price = ORDLINE.Gros_price
        ELSE
          m.Gros_price = 0
        ENDIF
        INSERT INTO (lcDetlTmp) FROM MEMVAR
      ENDSCAN
      LnRecNo = lftotpage(LnRecNo)
    CASE nRprtTyp = 3
      SELECT ORDLINE
      *B#604366,1 SSH Fix the bug of not print all the selected pick ticket.
      =SEEK('O'+&lcPackTmp..ORDER)
      *SCAN WHILE ORDLINE.ORDER = &lcPackTmp..ORDER FOR &lcPackTmp..Pack_No = OrdLine.PikTkt
      SCAN WHILE ORDLINE.cOrdType+ORDLINE.Order = 'O'+&lcPackTmp..ORDER FOR &lcPackTmp..Pack_No = OrdLine.PikTkt
      *B#604366,1 SSH [END]
        LnRecNo = LnRecNo + 1 
        m.Invoice    = &lcPackTmp..Pack_No
        m.Style      = Style  
        m.TotQty     = TotPik
        m.Gros_price = Gros_price
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          m.Qty&lcCount = PIK&lcCount
        ENDFOR
        *--get name drop id from bomvar
        IF SEEK('SO'+ORDER+STR(lineno,6), lcBomVar )
          LnRecNo = LnRecNo + 1 
          m.cNDrpId   = &lcBomVar..cNDrpId
          m.cDsgnCode = &lcBomVar..cDsgnCode
        ELSE
          STORE '' TO m.cNDrpId,m.cDsgnCode
        ENDIF
        INSERT INTO (lcDetlTmp) FROM MEMVAR
      ENDSCAN
      LnRecNo = lftotpage(LnRecNo)
  ENDCASE
  SELECT (lcPackTmp)    
  REPLACE nLastLno WITH LnRecNo
ENDSCAN  

SELECT ORDLINE
SET ORDER TO ORDLINST

SELECT (lcDetlTmp)
SET RELATION TO &lcDetlTmp..cNDrpId   INTO (lcNamDrp)  ADDITIVE
SET RELATION TO &lcDetlTmp..STYLE 	INTO STYLE         ADDITIVE
SET RELATION TO &lcDetlTmp..INVOICE   INTO (lcPackTmp) ADDITIVE
SET RELATION TO &lcDetlTmp..cDsgnCode INTO (lcDesign)  ADDITIVE
INDEX ON &lcPackTmp..pack_no+ordline.Store+cNDrpID TAG lcpackno OF (gcWorkDir+lcDetlTmp+'.CDX')

RETURN

*!**************************************************************************
*! Name      : lfCreatTmp
*! Developer : WAB - WALID A. WAHAB
*! Date      : 10/10/1999
*! Purpose   : Create work cursors.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfCreatTmp()
*!**************************************************************************
FUNCTION lfCreatTmp
PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)

SELECT INVLINE
DIMENSION laStruTemp[1,4]
lnFldLen = AFIELDS(laStruTemp)
*--add 2 fields for ( name drop id and design code )
DIMENSION laStruTemp[lnFldLen + 2 , 4]
laStruTemp[lnFldLen+1 , 1] = 'cNDrpId'
laStruTemp[lnFldLen+1 , 2] = 'C'
laStruTemp[lnFldLen+1 , 3] = 6
laStruTemp[lnFldLen+1 , 4] = 0
laStruTemp[lnFldLen+2 , 1] = 'cDsgnCode'
laStruTemp[lnFldLen+2 , 2] = 'C'
laStruTemp[lnFldLen+2 , 3] = 6
laStruTemp[lnFldLen+2 , 4] = 0

CREATE CURSOR (lcDetlTmp) FROM ARRAY laStruTemp
ZAP
INDEX ON INVOICE TAG (lcDetlTmp) OF (gcWorkDir+lcDetlTmp+'.CDX')
SELECT (lnCurAlias)

*!**************************************************************************
*! Name      : lftotpage
*! Developer : WAB - WALID A. WAHAB
*! Date      : 10/10/1999
*! Purpose   : Calculate total page for each pack list
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lftotpage()
*!**************************************************************************
FUNCTION lftotpage
PARAMETERS lnRecNo
IF lnRecNo > 9				&& 1st page filled with  9 lines 
  lnRecNo = lnRecNo - 9 
  lnRecNo = CEILING(lnRecNo/16)		&& other page filled with  16 lines 
ELSE
  lnRecNo = 0
ENDIF
RETURN(lnRecNo+1)
