*:***************************************************************************
*: Program file  : ARPINVCO.PRG
*: Program desc. : Customized Invoice Form for Collage UK Ltd (ENGLAND)
*: Date          : 07/22/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Tracking Job Number : C200363
*:***************************************************************************
*: Example : DO ARPINVCO
*:***************************************************************************
*B606370,1 SSE 08/11/2002 Fix some changes need to be done.

*-- Case the user select sort by Style.
SELECT InvHdr
LOCATE FOR &lcRpExp

IF !FOUND()
  *-- Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  *SET DEVICE TO SCREEN
  RETURN
ENDIF

PRIVATE lnStylePos , lnStyleLen , lnClrPos , lnClrLen , laItemSeg
STORE 0 TO lnStylePos , lnStyleLen , lnClrPos , lnClrLen
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

*-- Open BomVar & IcDesign & Debit files with Temp names.
IF !USED(lcTmpOrdLn)
  =gfOpenFile(gcDataDir+"OrdLine","OrdLinst",'SH', @lcTmpOrdLn)
ENDIF

*-- Search for company Phone number in laCompAdd
PRIVATE lnPhonePos , lcTaxRefr
lnPhonePos = ASCAN(laCompAdd,TRANSFORM(lcCompPhon , lcPhonPict))
IF lnPhonePos > 0
  laCompAdd[lnPhonePos] = "Tel No: " + laCompAdd[lnPhonePos]
ENDIF

PRIVATE lnCompLoop
FOR lnCompLoop = 1 TO 6
  IF EMPTY(laCompAdd[lnCompLoop])
    laCompAdd[lnCompLoop] = "Fax # : " + lcCompFax
    EXIT
  ENDIF
ENDFOR
=lfAdrShift('laCompAdd')
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
*SET DEVICE TO PRINT

SELECT InvLine
=AFIELDS(laFileStru)
DIMENSION laFileStru[ALEN(laFileStru,1)+2,4]
laFileStru[ALEN(laFileStru,1),1] = 'cDim1'
laFileStru[ALEN(laFileStru,1),2] = 'C'
laFileStru[ALEN(laFileStru,1),3] = 5
laFileStru[ALEN(laFileStru,1),4] = 0
laFileStru[ALEN(laFileStru,1)-1,1] = 'cReference'
laFileStru[ALEN(laFileStru,1)-1,2] = 'C'
laFileStru[ALEN(laFileStru,1)-1,3] = 100
laFileStru[ALEN(laFileStru,1)-1,4] = 0

CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru

*-- The InvLine is right due to use this name when replace the InvLine file place the temp. file(gcWorkDir + InvLine + '.CDX')
INDEX ON Invoice + STR(LineNo,6) TAG InvLine OF (gcWorkDir + InvLine + '.CDX')

*B606370,1 Change the index. [Begin]
*INDEX ON cDim1 + SUBSTR(Style,1,16) + Store + Invoice TAG (lcStylIndx) OF (gcWorkDir + InvLine + '.CDX')
INDEX ON SUBSTR(Style,1,16) + Store + Invoice + cDim1 TAG (lcStylIndx) OF (gcWorkDir + InvLine + '.CDX')
*B606370,1 Change the index. [End]

SELECT InvHdr
PRIVATE lcDateCent , lcInvLnTag
lcDateCent = SET('CENTURY')
SET CENTURY ON

SCAN FOR &lcASExp
  SELECT InvLine
  SCAN REST WHILE Invoice+STR(lineno,6) = InvHdr.Invoice
    WAIT WINDOW 'Selecting Records For The Report ...' + InvHdr.Invoice NOWAIT
    SCATTER MEMVAR MEMO
        
    SELECT (lcStylIndx)

    *B606370,1 Change in way of collecting data. [Begin]
    *IF SEEK(Scale.cDim1 + SUBSTR(InvLine.Style,1,16) + InvLine.Store + InvLine.Invoice)
    *  REPLACE TotQty WITH TotQty + m.TotQty  
    *ELSE
    *  APPEND BLANK
    *  m.cDim1 = Scale.cDim1
    *  IF SEEK('O'+InvLine.Order+InvLine.Store+InvLine.Style+STR(InvLine.LineNo,6),lcTmpOrdLn)
    *    m.cReference = MLINE(&lcTmpOrdLn..Note_Mem,1)
    *  ENDIF
    *  GATHER MEMVAR MEMO
    *ENDIF
    IF SEEK(SUBSTR(InvLine.Style,1,16) + InvLine.Store + InvLine.Invoice)
      m.cReference = cReference
      m.cDim1 = Scale.cDim1
      IF SEEK(SUBSTR(InvLine.Style,1,16) + InvLine.Store + InvLine.Invoice + Scale.cDim1)
        REPLACE TotQty WITH TotQty + m.TotQty
      ELSE
        APPEND BLANK        
        GATHER MEMVAR MEMO  
      ENDIF      
    ELSE
      APPEND BLANK
      m.cDim1 = Scale.cDim1
      IF SEEK('O'+InvLine.Order+InvLine.Store+InvLine.Style,lcTmpOrdLn)
        m.cReference = MLINE(&lcTmpOrdLn..Note_Mem,1)
      ENDIF
      GATHER MEMVAR MEMO
    ENDIF
    *B606370,1 Change in way of collecting data. [End]
    
  ENDSCAN
ENDSCAN

SET CENTURY &lcDateCent

*-- Section break the relations --*
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO Customer
SET RELATION OFF INTO OrdHdr
SET RELATION OFF INTO ConsInvL
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO Style
SET RELATION OFF INTO Spck_Lin

SELECT Style
SET RELATION OFF INTO Scale
             *-- End Section break the relations --*

*--Clos the InvLine and open the temp. file with the InvLine name.
SELECT InvLine
CLOSE INDEX
USE IN InvLine
USE IN (lcStylIndx)
USE (gcWorkDir+lcStylIndx) IN 0 ALIAS InvLine
SELECT InvLine
INDEX ON Invoice + Style + STR(LINENO,6) TAG InvLine OF (gcWorkDir + lcStylIndx + '.CDX')

             *-- Section Create the new relations --*
SELECT InvHdr
IF llPrntInst .OR. llRpInvNot 
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO IIF(CFILE_NUM = '1', InvHdr.Invoice, '*') INTO InvLine ADDITIVE
ELSE
  SET RELATION TO InvHdr.Invoice INTO InvLine ADDITIVE
ENDIF

SELECT InvLine
SET ORDER TO TAG InvLine
LOCATE
SET RELATION TO IIF(!EMPTY(InvLine.ALTStyle) , InvLine.ALTStyle , InvLine.Style) INTO Style ADDITIVE
SET RELATION TO "S" + InvLine.Account + InvLine.Style INTO SPCK_LIN ADDITIVE

SELECT Style
SET RELATION TO 'S' + Scale INTO Scale ADDITIVE

SELECT InvHdr
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                  'S' + Account + Store) INTO Customer ADDITIVE
SET RELATION TO 'O' + InvHdr.Order INTO OrdHdr ADDITIVE
SET RELATION TO 'C' + InvHdr.Invoice INTO NotePad ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , InvLine
ELSE
  SET SKIP TO InvLine
ENDIF

SELECT InvHdr
DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
*SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

SELECT InvLine
CLOSE INDEX
USE IN InvLine
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')
*-- End of Report.
