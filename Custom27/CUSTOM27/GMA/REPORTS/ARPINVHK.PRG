*:************************************************************************
*: Procedure file: ARPINVHK.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author:Albert Raif (ALB)
*:      Copyright (c) 
*:  Last modified: 01/28/2003
*:
*:  Procs & Fncts: lfGetForm
*:
*:  Documented      C200482 
*:******************************************************************************************
*:Modifications :
*:B607132,1 SSE 04/06/2003 Some modifications in BHK Invoice form.
*:B607334,1 SSE 06/09/2003 Fix bug to remove the word 'Phone No : ' from Invoice header.
*:C125213,1 NNA 12/26/2004 Add new 3 columns to the FRX called (Assist/PC - Total CMT+Assist 
*:C125213,1 NNA            - Total Declared Value)
*:******************************************************************************************

*B607132,1 SSE Commented out. [Begin]
*lcAlias = ALIAS()
*B607132,1 SSE Commented out. [End]

IF !USED('WAREHOUS')
  =gfOpenFile(gcdatadir + "WAREHOUS" , "WAREHOUS" , 'SH')
ENDIF

IF !USED('SPCK_HDR')
  =gfOpenFile(gcdatadir + "SPCK_HDR" , "SPCK_HDR" , 'SH')
ENDIF

IF !USED('SYCCURR')
  =gfOpenFile(gcsyshome + "SYCCURR" , "CCURRCODE" , 'SH')
ENDIF

IF !USED('CUTPICK')
  =gfOpenFile(gcdatadir + "CUTPICK" , "CUTORD" , 'SH')
ENDIF

IF !USED('ORDLINE')
  =gfOpenFile(gcdatadir + "ORDLINE" , "ORDLINE" , 'SH')
ENDIF
*B607132,1 SSE Commented out (not used in this report). [Begin]
*PRIVATE lcTEOM , lnEomDay , lnTDaysDue , ldDueDate
*lcTEOM = ''
*lnTDaysDue = 0
*lnEomDay = 20
*DECLARE laTRltFld[3,2]
*laTRltFld[1,1] = 'EOM'
*laTRltFld[1,2] = 'lcTEOM'
*laTRltFld[2,1] = 'NTERDUED'
*laTRltFld[2,2] = 'lnTDaysDue'
*laTRltFld[3,1] = 'EOMDAY'
*laTRltFld[3,2] = 'lnEomDay'
*B607132,1 SSE Commented out (not used in this report). [End]

*-- Search for company Phone number in laCompAdd
PRIVATE lnPhonePos , lcTaxRefr
lnPhonePos = ASCAN(laCompAdd,TRANSFORM(lcCompPhon , lcPhonPict))
IF lnPhonePos > 0
  
  *B607334,1 Remove the word 'Phone No : ' from Invoice header. [Begin]
  *laCompAdd[lnPhonePos] = 'Phone No : ' + laCompAdd[lnPhonePos]
  laCompAdd[lnPhonePos] = laCompAdd[lnPhonePos]
  *B607334,1 Remove the word 'Phone No : ' from Invoice header. [End]
  
ENDIF

*B607132,1 SSE Commented out (not used in this report). [Begin]
*STORE 0 TO lnClrLen , lnClrPos
*DECLARE laItemSeg[1]
*=gfItemMask(@laItemSeg)
*FOR lnCount = 1 TO ALEN(laItemSeg,1)
*  IF laItemSeg[lnCount,1]='C'
*    lnClrLen = LEN(laItemSeg[lnCount,3])
*    lnClrPos = laItemSeg[lnCount,4]
*    EXIT
*  ENDIF
*ENDFOR
*lcTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
*lnTaxRat = 0
*DECLARE laTaxRat[1,2]
*laTaxRat[1,1] = 'NTAXRATE'
*laTaxRat[1,2] = 'lnTaxRat'
*lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
*B607132,1 SSE Commented out (not used in this report). [End]

*-- Case the user select sort by style.
SELECT InvHdr
LOCATE FOR &lcRpExp

IF !FOUND()
  *-- Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SELECT INVLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]

laFileStru[lnFileStru +1,1] = 'CTKTNO'
laFileStru[lnFileStru +1,2] = 'C'
laFileStru[lnFileStru +1,3] = 6
laFileStru[lnFileStru +1,4] = 0

laFileStru[lnFileStru +2,1] = 'CustPO'
laFileStru[lnFileStru +2,2] = 'C'

*B607132,1 Change the field length. [Begin]
*laFileStru[lnFileStru +2,3] = 6
laFileStru[lnFileStru +2,3] = 15
*B607132,1 Change the field length. [End]

laFileStru[lnFileStru +2,4] = 0

CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru

*-- The invline is right due to use this name when replace the invline file place the temp. file(gcWorkDir + INVLINE + '.CDX')
INDEX ON INVOICE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + INVLINE + '.CDX')

*B607132,1 Copy Invoice header structure. [Begin] 
SELECT INVHDR
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcInvHdr) FROM ARRAY laFileStru
INDEX ON INVOICE TAG INVHDR OF (gcWorkDir + lcInvHdr+ '.CDX')
*B607132,1 Copy Invoice header structure. [End]

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO CONSINVL ADDITIVE

PRIVATE lnConsLine , lcDateCent , lcInvLnTag
lcDateCent = SET('CENTURY')
SET CENTURY ON

lnConsLine = 0
SCAN FOR &lcASExp

  *-lcInvcee    >> Variable hold the invoice No.
  lcInvcee = INVHDR.INVOICE
  lnConsLine = 0
  IF INVHDR.CONSOL = 'Y'
     
    =SEEK(INVHDR.Invoice,'CONSINVH') AND SEEK('O'+CONSINVH.ORDER,'ORDHDR')

    *B607132,1 Scatter Invoice records. [Begin]
    *m.CLOC_NO   = ORDHDR.CLOC_NO
    *m.DLOCISSUE = ORDHDR.DLOCISSUE 
    *m.CLOCBANK  = ORDHDR.CLOCBANK 
    SCATTER MEMVAR MEMO
    m.CLOC_NO   = ORDHDR.CLOC_NO
    m.DLOCISSUE = ORDHDR.DLOCISSUE 
    m.CLOCBANK  = ORDHDR.CLOCBANK     
    SELECT(lcInvHdr)
    IF !SEEK(lcInvcee)
      APPEND BLANK
      GATHER MEMVAR MEMO    
    ENDIF  
    *B607132,1 Scatter Invoice records. [End]    
    
    *--Case consolidated invoice.
    SELECT CONSINVL
    SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT

      SCATTER MEMVAR MEMO
      lnConsLine = lnConsLine + 1
  
      *B607132,1 Reinitialize m.CTKTNO & m.CustPO. [Begin]
      STORE '' TO m.CTKTNO , m.CustPO
      *B607132,1 Reinitialize m.CTKTNO & m.CustPO. [End]

      IF SEEK("2"+CONSINVL.order+STR(m.LineNo,6),'Cutpick')
        m.CTKTNO   = Cutpick.CTKTNO
      ENDIF
      IF SEEK('O'+ CONSINVL.order+STR(m.lineno,6),'Ordline')
        m.CustPO   = Ordline.CustPO
      ENDIF
      m.LineNo     = lnConsLine
      
      lnRecNoSt = 0
      lnRecNoSt = RECNO('STYLE')
      =SEEK(CONSINVL.STYLE,'STYLE')
      IF BETWEEN(lnRecNoSt,1,RECCOUNT('STYLE'))
        GOTO lnRecNoSt IN STYLE
      ENDIF  
      SELECT InvLine
      lcInvLnTag = TAG()
      SET ORDER TO TAG InvLines
      m.Desc1 = IIF(SEEK(m.Style+m.Invoice),Desc1,'')
      SET ORDER TO TAG (lcInvLnTag)
      
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO

    ENDSCAN
  ELSE

    *B607132,1 Scatter Invoice records. [Begin]
    =SEEK('O'+INVHDR.ORDER,'ORDHDR')
    SCATTER MEMVAR MEMO
    m.CLOC_NO   = ORDHDR.CLOC_NO
    m.DLOCISSUE = ORDHDR.DLOCISSUE 
    m.CLOCBANK  = ORDHDR.CLOCBANK     
    SELECT(lcInvHdr)
    IF !SEEK(lcInvcee)
      APPEND BLANK
      GATHER MEMVAR MEMO    
    ENDIF  
    *B607132,1 Scatter Invoice records. [End]

    SELECT INVLINE
    SCAN REST WHILE Invoice+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      SCATTER MEMVAR MEMO
      
      *B607132,1 Change Consolidated Invoice to Invoice line file. [Begin]
      STORE '' TO m.CTKTNO , m.CustPO
      *IF SEEK("2"+CONSINVL.order+STR(m.LineNo,6),'Cutpick')
      IF SEEK("2"+InvLine.order+STR(m.LineNo,6),'Cutpick')
      *B607132,1 Change Consolidated Invoice to Invoice line file. [End]

        m.CTKTNO   = Cutpick.CTKTNO
      ENDIF

      *B607132,1 Change Consolidated Invoice to Invoice line file. [Begin]
      *IF SEEK('O'+ CONSINVL.order+STR(m.lineno,6),'Ordline')
      IF SEEK('O'+ InvLine.order+STR(m.lineno,6),'Ordline')
      *B607132,1 Change Consolidated Invoice to Invoice line file. [End]

        m.CustPO = Ordline.CustPO
      ENDIF
      
      *B607132,1 Commented out. [Begin]
      *m.LineNo     = lnConsLine
      *B607132,1 Commented out. [End]

      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO

    ENDSCAN
  ENDIF
  
ENDSCAN

SET CENTURY &lcDateCent

             *-- Section break the relatoins --*
SELECT InvHdr

*B607132,1 Return the commented line. [Begin]
*SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO (lcTmpDbt)
*B607132,1 Return the commented line. [End]

SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO ORDHDR
SET RELATION OFF INTO CONSINVL

*SELECT (lcTmpDbt)
*SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
             *-- End Section break the relatoins --*

*--Clos the invline and open the temp. file with the invline name.
SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
USE IN (lcStylIndx)
USE (gcWorkDir+lcStylIndx) IN 0 ALIAS INVLINE EXCLUSIVE
SELECT INVLINE

*B607132,1 Remove the style from index. [Begin]
*INDEX ON INVOICE + STYLE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + lcStylIndx + '.CDX')
SET INDEX TO (gcWorkDir+InvLine+'.cdx')
SET ORDER TO TAG Invline OF (gcWorkDir+InvLine+'.cdx')
USE IN INVHDR
USE IN (lcInvHdr)
USE (gcWorkDir+lcInvHdr) IN 0 ALIAS INVHDR EXCLUSIVE
SELECT INVHDR
INDEX ON INVOICE TAG INVHDR OF (gcWorkDir + lcInvHdr + '.CDX')
*B607132,1 Remove the style from index. [End]

             *-- Section Create the new relatoins --*
SELECT INVHDR

IF llPrntInst .OR. llRpInvNot

  *B607132,1 Return the commented line. [Begin] 
*  SET RELATION TO '' INTO (lcTmpDbt)
 * SELECT (lcTmpDbt)
  *SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  IF cFile_Num <> '1'
    REPLACE cFile_Num WITH '1'     && add this line of code to always print Invoice Notepad only.
  ENDIF  
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE
  *B607132,1 Return the commented line. [End]
  
ELSE
  SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
ENDIF

SELECT INVLINE
LOCATE
SET RELATION TO IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE , INVLINE.Style) INTO STYLE ADDITIVE
SET RELATION TO "S" + INVLINE.Account + INVLINE.Style INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                  'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO OrdHdr ADDITIVE
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE
SET RELATION TO INVHDR.CWARECODE INTO WAREHOUS ADDITIVE

IF llPrntInst .OR. llRpInvNot
  
  *B607132,1 return the commented line. [Begin]
*  SET SKIP TO (lcTmpDbt) , INVLINE
  SET SKIP TO (lcTmpDbt) , INVLINE
  *B607132,1 return the commented line. [End]

ELSE
  SET SKIP TO INVLINE
ENDIF

SELECT INVHDR
DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

*B607132,1 Close the temp invoice header file which was opened as InvHdr alias. [Begin]
SELECT InvHdr
CLOSE INDEX
USE IN InvHdr
=gfOpenFile(gcDataDir+"InvHdr","InvHdr",'SH')
*B607132,1 Close the temp invoice header file which was opened as InvHdr alias. [End]

SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')

                     *-- End of the Program --*
