*:***************************************************************************
*: Program file  : ARPINVEN.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR ENGLAND.
*: Date          : 05/14/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200189
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVEN
*:***************************************************************************

*-- Case the user select sort by style.
IF lcRpSrot = "S"
  SELECT InvHdr
  LOCATE FOR &lcRpExp

  IF !FOUND()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    llNoRec = .T.
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
  SET DEVICE TO PRINT

  SELECT INVLINE
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru
  INDEX ON INVOICE + STYLE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + INVLINE + '.CDX') 

  SELECT InvHdr
  SCAN FOR &lcASExp
    *-lcInvcee    >> Variable hold the invoice No.
    lcInvcee = INVHDR.INVOICE
    SELECT INVLINE
    SCAN REST WHILE Invoice+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      SCATTER MEMVAR MEMO
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
  ENDSCAN

               *-- Section break the relatoins --*
  SELECT InvHdr
  SET RELATION OFF INTO (lcTmpDbt)
  SET RELATION OFF INTO CUSTOMER
  SET RELATION OFF INTO Ordhdr
  SELECT (lcTmpDbt)
  SET RELATION TO

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
  USE (gcWorkDir+lcStylIndx) IN 0 ALIAS INVLINE
  SELECT INVLINE
  INDEX ON INVOICE + STYLE + STR(LINENO,6) TAG INVLINE OF (gcWorkDir + lcStylIndx + '.CDX')

               *-- Section Create the new relatoins --*
  SELECT INVHDR
  IF llPrntInst .OR. llRpInvNot 
    SET RELATION TO '' INTO (lcTmpDbt)
    SELECT (lcTmpDbt)
    SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE
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

  IF llPrntInst .OR. llRpInvNot
    SET SKIP TO (lcTmpDbt) , INVLINE
  ELSE
    SET SKIP TO INVLINE
  ENDIF

  SELECT INVHDR
  DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
  SET DEVICE TO SCREEN
  llarpinv = .F.
  WAIT CLEAR

  SELECT INVLINE
  CLOSE INDEX
  USE IN INVLINE
  =gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')
ENDIF

SELECT INVHDR

                       *-- End of the Program --*