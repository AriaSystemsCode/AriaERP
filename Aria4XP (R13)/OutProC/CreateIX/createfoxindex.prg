*!************************************************************************
*! Modifications : 
*! E037241,2 MAH 04/17/2005 Browse User Defined Sort.
*! B609254,1 MMT 05/18/2010 Style SCreen-Cut&Sold-Shipping browse UD Sort not working[T20100312.0048]
*! E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*!************************************************************************
LPARAMETERS lcParameter
*! B609254,1 MMT 05/18/2010 Style SCreen-Cut&Sold-Shipping browse UD Sort not working[Start]
*LOCAL lcSourceTable, lcTargetTable, lcModificationTable, lcFilter, lcIndexExpression, lcIndexExpressionNC, lcIndexCommand, lcIndexCommandNC
LOCAL lcSourceTable, lcTargetTable, lcModificationTable, lcFilter, lcIndexCommand, lcIndexCommandNC
*! B609254,1 MMT 05/18/2010 Style SCreen-Cut&Sold-Shipping browse UD Sort not working[End]
lcSourceTable       = SUBSTR(lcParameter, 1, AT("|", lcParameter) - 1)
lcTargetTable       = SUBSTR(lcParameter, AT("|", lcParameter   ) + 1, AT("|", lcParameter, 2) - AT("|", lcParameter) - 1)
lcModificationTable = SUBSTR(lcParameter, AT("|", lcParameter, 2) + 1, AT("|", lcParameter, 3) - AT("|", lcParameter, 2) - 1)
lcFilter            = SUBSTR(lcParameter, AT("|", lcParameter, 3) + 1, AT("|", lcParameter, 4) - AT("|", lcParameter, 3) - 1)
lcIndexExpression   = SUBSTR(lcParameter, AT("|", lcParameter, 4) + 1, AT("|", lcParameter, 5) - AT("|", lcParameter, 4) - 1)
lcIndexExpressionNC = SUBSTR(lcParameter, AT("|", lcParameter, 5) + 1, AT("|", lcParameter, 6) - AT("|", lcParameter, 5) - 1)
lcIndexCommand      = SUBSTR(lcParameter, AT("|", lcParameter, 6) + 1, AT("|", lcParameter, 7) - AT("|", lcParameter, 6) - 1)
lcIndexCommandNC    = SUBSTR(lcParameter, AT("|", lcParameter, 7) + 1, AT("|", lcParameter, 8) - AT("|", lcParameter, 7) - 1)
lcOutPutFile        = SUBSTR(lcParameter, AT("|", lcParameter, 8) + 1)

IF EMPTY(lcFilter)
  SELECT *, STR(RECNO()) AS _Rec_No, &lcIndexExpression. AS _INDEX, &lcIndexExpressionNC. AS _INDEXNC FROM (lcSourceTable) INTO TABLE (lcTargetTable)
ELSE
  LOCAL llTemp
  ON ERROR llTemp = .T.

  SELECT *, STR(RECNO()) AS _Rec_No, &lcIndexExpression. AS _INDEX, &lcIndexExpressionNC. AS _INDEXNC FROM (lcSourceTable) WHERE &lcFilter. INTO TABLE (lcTargetTable)
  
  ON ERROR gfQuit()
  
  IF llTemp = .T.
    SELECT *, STR(RECNO()) AS _Rec_No, &lcIndexExpression. AS _INDEX, &lcIndexExpressionNC. AS _INDEXNC FROM (lcSourceTable) INTO TABLE (lcTargetTable)
  ENDIF
ENDIF
USE

LOCAL lcTargetAlias
lcTargetAlias = GFTempName()
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
*USE (lcTargetTable) ALIAS (lcTargetAlias) SHARED
USE (lcTargetTable) ALIAS (lcTargetAlias) Exclusive
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
SELECT(lcTargetAlias)
INDEX ON _Rec_No TAG IXRecNO
USE IN (lcTargetAlias)

IF !EMPTY(lcModificationTable)
  = gfMergTables(lcTargetTable, lcModificationTable)
ENDIF
LOCAL lcTargetAlias
lcTargetAlias = GFTempName()

USE (lcTargetTable) ALIAS (lcTargetAlias) EXCLUSIVE IN 0
SELECT(lcTargetAlias)

&lcIndexCommand.
&lcIndexCommandNC.

STRTOFILE("T", lcOutPutFile)

FUNCTION gfMergTables
LPARAMETERS lcTargetTable, lcModificationTable

LOCAL lcTargetAlias, lcModificationAlias
lcTargetAlias       = GFTempName()
lcModificationAlias = GFTempName()

USE (lcTargetTable) ALIAS (lcTargetAlias) SHARE IN 0
USE (lcModificationTable) ALIAS (lcModificationAlias) SHARE IN 0
SELECT(lcModificationAlias)

SCAN 
  DO CASE
    *-- Update ----------------------------------------------------------------- 
    CASE _RecType = 'U'
      SCATTER MEMVAR MEMO
      SELECT(lcTargetAlias)
      LOCATE FOR _Rec_No = &lcModificationAlias.._Rec_No
      IF EOF()
        APPEND BLANK 
      ENDIF
      GATHER MEMVAR MEMO
      SELECT(lcModificationAlias)

    *-- Delete ----------------------------------------------------------------- 
    CASE _RecType = 'D'
      SELECT(lcTargetAlias)
      LOCATE FOR _Rec_No = &lcModificationAlias.._Rec_No
      DELETE
      SELECT(lcModificationAlias)
      
    *-- Insert ----------------------------------------------------------------- 
    CASE _RecType = 'I'
      SCATTER MEMVAR MEMO
      *! B609254,1 MMT 05/18/2010 Style SCreen-Cut&Sold-Shipping browse UD Sort not working[Start]      
      M._INDEX = &lcIndexExpression.
      M._INDEXNC = &lcIndexExpressionNC.      
      *! B609254,1 MMT 05/18/2010 Style SCreen-Cut&Sold-Shipping browse UD Sort not working[End]
      SELECT(lcTargetAlias)
      APPEND BLANK 
      GATHER MEMVAR MEMO
      SELECT(lcModificationAlias)
  ENDCASE
ENDSCAN

USE IN (lcTargetAlias)
USE IN (lcModificationAlias)
