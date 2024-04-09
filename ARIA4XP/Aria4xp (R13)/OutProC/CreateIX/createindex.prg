*:************************************************************************
*: Modifications : 
*! E037241,2 MAH 04/17/2005 Browse User Defined Sort.
*! E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*:************************************************************************

LPARAMETERS lcParameter

PUBLIC lcOutPutFile

SET PROCEDURE TO aria ADDITIVE

SET COLLATE TO "MACHINE" 

SET TABLEPROMPT OFF
SET CPDIALOG OFF
SET NOTIFY OFF
SET NOTIFY CURSOR OFF
SET SAFETY OFF 
SET EXCLUSIVE OFF 
*B609762,1 MMT 12/01/2011 Deleted Records appears when user create UDF Index[T20111114.0031][Start]
SET DELETED ON
*B609762,1 MMT 12/01/2011 Deleted Records appears when user create UDF Index[T20111114.0031][End]
ON ERROR gfQuit()

lcParameter = FILETOSTR(lcParameter)

LOCAL lcType, lcEngineParameter
lcType            = SUBSTR(lcParameter, 1, AT("|", lcParameter) - 1)
lcEngineParameter = SUBSTR(lcParameter, AT("|", lcParameter) + 1)
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
SET PROCEDURE TO 'AriaGlb.fxp' ADDITIVE
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]

DO CASE
  CASE UPPER(ALLTRIM(lcType)) == 'NATIVE'
    DO CreateFoxIndex WITH lcEngineParameter
    
  CASE UPPER(ALLTRIM(lcType)) == 'SQL'
    DO CreateSQLIndex WITH lcEngineParameter
ENDCASE

FUNCTION gfQuit
STRTOFILE("F", lcOutPutFile)
QUIT