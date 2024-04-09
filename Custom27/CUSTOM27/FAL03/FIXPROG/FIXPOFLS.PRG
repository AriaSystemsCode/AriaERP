*:****************************************************************
*: Program file  : FIXPOFLS.PRG
*: Program desc. : Fix program to the poshdr,ctktbom,bomline and styinvjl
*: System        : Aria Apparel System (A27).
*: Module        : Style Purchase Order - (PO)
*: Developer     : AHMED MAHER (AMH)
*: Date          : 03/13/2003
*:****************************************************************
*:B#607028,1
*:****************************************************************

lcDataDir = GETDIR(SET('DEFAULT'),'Select Data Directory')

IF EMPTY(lcDataDir)
  RETURN
ENDIF

=lfUpdPosLn()
=lfUpdFiles('POSHDR','OPEN','MAX(NSTYORDER-RECEIVE-CANCEL-DAMAGE,0)','PO')
=lfUpdFiles('CTKTBOM','CUTTKT','PADL(ALLTRIM(CUTTKT),6,"0")','CUTTKT')
=lfUpdFiles('BOMLINE','CTKTNO','PADL(ALLTRIM(CTKTNO),6,"0")','CTKTNO',.T.)
=lfUpdFiles('STYINVJL','CTRCODE','PADL(ALLTRIM(CTRCODE),6,"0")','CTRCODE',.T.)

WAIT WINDOW 'Fixing completed successfully'

*!*************************************************************
*! Name      : lfUpdFiles
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/13/2003
*!*************************************************************
*! Purpose   : Update field in file.
*!*************************************************************
FUNCTION lfUpdFiles
PARAMETERS lcFileName,lcFldName,lcValue,lcPo,llUpdLineNo

IF FILE(lcDataDir+lcFileName+'.DBF')
  USE (lcDataDir+lcFileName) IN 0 SHARED
  IF llUpdLineNo
    IF FILE(lcDataDir+'POSLN.DBF')
      USE (lcDataDir+'POSLN') IN 0 SHARED ORDER TAG POSLN
    ELSE
      llUpdLineNo = .F.
    ENDIF
  ENDIF
  SELECT (lcFileName)
  SCAN
    WAIT WINDOW 'Fixing PO # : ' + EVALUATE(lcPo) + ' in the ' + lcFileName + ' file.' NOWAIT
    REPLACE (lcFldName) WITH EVALUATE(lcValue)
    IF llUpdLineNo .AND. IIF(lcFileName='BOMLINE',CIMTYP='I',CTRTYPE='6') .AND. LINENO = 0;
       .AND. SEEK('P'+EVALUATE(lcFldName)+STYLE,'POSLN')
      REPLACE LINENO WITH POSLN.LINENO
    ENDIF
  ENDSCAN
  IF llUpdLineNo
    USE IN POSLN
  ENDIF
  USE IN (lcFileName)
ELSE
  WAIT WINDOW lcFileName + ' file does not exist.' + CHR(13) + CHR(10) + 'Press any key to continue'
ENDIF

*!*************************************************************
*! Name      : lfUpdPosLn
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/13/2003
*!*************************************************************
*! Purpose   : Update PosLn file.
*!*************************************************************
FUNCTION lfUpdPosLn

IF FILE(lcDataDir+'POSLN.DBF')
  USE (lcDataDir+'POSLN') IN 0 SHARED ORDER TAG POSLN
  SELECT POSLN
  SCAN FOR TRANCD = '1'
    WAIT WINDOW 'Fixing PO # : ' + PO + ' in the POSLN file.' NOWAIT
    lnRecNo = RECNO()
    lcKey = cstytype+po+style
    STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty
    lnLineNo = LINENO
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = lcKey;
                FOR TRANCD = '1' .AND. RECNO() # lnRecNo
      lnLineNo = LINENO
      lnQty1   = lnQty1   + QTY1
      lnQty2   = lnQty2   + QTY2
      lnQty3   = lnQty3   + QTY3
      lnQty4   = lnQty4   + QTY4
      lnQty5   = lnQty5   + QTY5
      lnQty6   = lnQty6   + QTY6
      lnQty7   = lnQty7   + QTY7
      lnQty8   = lnQty8   + QTY8
      lnTotQty = lnTotQty + TOTQTY
      DELETE
    ENDSCAN
    GOTO lnRecNo
    REPLACE LINENO WITH lnLineNo,;
            QTY1   WITH QTY1   + lnQty1,;
            QTY2   WITH QTY2   + lnQty2,;
            QTY3   WITH QTY3   + lnQty3,;
            QTY4   WITH QTY4   + lnQty4,;
            QTY5   WITH QTY5   + lnQty5,;
            QTY6   WITH QTY6   + lnQty6,;
            QTY7   WITH QTY7   + lnQty7,;
            QTY8   WITH QTY8   + lnQty8,;
            TOTQTY WITH TOTQTY + lnTotQty
  ENDSCAN
  SCAN FOR TRANCD # '1' .AND. LINENO = 0
    WAIT WINDOW 'Fixing PO # : ' + PO + ' in the POSLN file.' NOWAIT
    lnRecNo = RECNO()
    lcKey = cstytype+po+style
    lnLineNo = 0
    IF SEEK(lcKey)
      LOCATE REST WHILE cstytype+po+style+STR(lineno,6)+trancd = lcKey;
                    FOR TRANCD = '1'
      IF FOUND()
        lnLineNo = LINENO
      ENDIF
    ENDIF
    GOTO lnRecNo
    IF lnLineNo # 0
      REPLACE LINENO WITH lnLineNo
    ENDIF
  ENDSCAN
  USE IN POSLN
ELSE
  WAIT WINDOW 'POSLN file does not exist.' + CHR(13) + CHR(10) + 'Press any key to continue'
ENDIF