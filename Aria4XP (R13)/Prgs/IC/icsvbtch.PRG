*:*********************************************************************************
*: Program file  : ICSVBTCH.PRG
*: Program desc. : Save Scan Item barcode Batch
*:        System : Aria4 XP.
*:        Module : IC
*!        Date   : 07/15/2018
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : E303040[P20112011.0011]
*:*********************************************************************************
*: Modifications:
*:*********************************************************************************
LPARAMETERS lcTmpHeader,lcTempDetail,lcMode

#INCLUDE r:\aria4xp\screens\ic\scanupc.h


IF !lfApplyScanBatchBR(lcTmpHeader,lcTempDetail)
  RETURN .F.
ENDIF

IF !USED('ScanBatchH')
  =gfOpenTable('SCAN_BATCH_HEADER_T','BTCH_HDR','SH','ScanBatchH')
ENDIF
IF !USED('ScanBatchD')
  =gfOpenTable('SCAN_BATCH_DETAILS_T','BTCH_DET','SH','ScanBatchD')
ENDIF  
IF lcMode == "A"
  *- get the maximum assinged key value and use it for the "scan_batch_header_key", there won't be duplication as this happens in the save time
  SELECT ScanBatchH
  lcMaxKeyValAlias = gfTempName()  
  =gfSqlRun("select MAX(scan_batch_header_key) AS nMaxKeyVal From [scan_batch_header_t]","ScanBatchH",.F.,lcMaxKeyValAlias)
  SELECT (lcTmpHeader)
  lnMaxKeyVal = iif(isnull(&lcMaxKeyValAlias..nMaxKeyVal),0,&lcMaxKeyValAlias..nMaxKeyVal)
  REPLACE scan_batch_header_key WITH lnMaxKeyVal + 1 
  USE IN &lcMaxKeyValAlias
  *- Update batch seq. #  
  lcBatch = gfSequence('SCANUPC')
  REPLACE BATCH WITH lcBatch 
  SCATTER MEMO MEMVAR 
  SELECT ScanBatchH
  APPEND BLANK 
  GATHER MEMO MEMVAR
  =gfReplace('')
ELSE
  SELECT (lcTmpHeader)
  =gfSeek(&lcTmpHeader..Batch,'ScanBatchH')
  SCATTER MEMO MEMVAR
  SELECT 'ScanBatchH'
  GATHER MEMO MEMVAR
  =gfReplace('')  
ENDIF
SELECT 'ScanBatchH'
llUpdated = gfTableUpdate()
IF llUpdated 
  IF lcMode == "A"
    IF gfSeek(PADR(lcBatch,6), 'ScanBatchH')
      SELECT (lcTempDetail) 
      LOCATE
      SCAN
        SCATTER MEMO MEMVAR
        m.SCAN_BATCH_HEADER_KEY  = ScanBatchH.SCAN_BATCH_HEADER_KEY 
        SELECT ScanBatchD
        APPEND BLANK
        GATHER MEMO MEMVAR
        =gfReplace('')
      ENDSCAN
      SELECT ScanBatchD
      llUpdated = gfTableUpdate()
    ENDIF
  ELSE
    lcOnDelBef = SET("Deleted")
    SET DELETED OFF
    SELECT (lcTempDetail) 
    LOCATE
    SCAN
      SCATTER MEMO MEMVAR
      IF DELETED(lcTempDetail) AND gfSeek(STR(ScanBatchH.SCAN_BATCH_HEADER_KEY,12)+m.STYLE+m.SCALE+m.SIZE+m.ITEM_NUMBER,'ScanBatchD')  
        SELECT 'ScanBatchD'
        =gfDelete()      
      ELSE
        IF gfSeek(STR(ScanBatchH.SCAN_BATCH_HEADER_KEY,12)+m.STYLE+m.SCALE+m.SIZE+m.ITEM_NUMBER,'ScanBatchD')  
  		SELECT 'ScanBatchD'
		  GATHER MEMO MEMVAR
  		=gfReplace('')
        ELSE  
  		SELECT 'ScanBatchD'
		  APPEND BLANK 
  		GATHER MEMO MEMVAR
		  =gfReplace('')      
        ENDIF
      ENDIF  
	ENDSCAN 
	SELECT ScanBatchD
	llUpdated = gfTableUpdate()
	SET DELETED &lcOnDelBef.
  ENDIF
ENDIF
*:**************************************************************************
*: Name        : lfApplyScanBatchBR
*! Developer   : Mariam Mazhar
*! Date        : 07/15/2018
*: Purpose     : Apply Scan Batch Business Rules
*:***************************************************************************
FUNCTION lfApplyScanBatchBR
LPARAMETERS lcTmpHeader,lcTempDetail
IF EMPTY(&lcTmpHeader..User)
  *- No user selected
  =gfModalGen('INM00000B00000', .F., .F., .F., LANG_SCANUPC_NoSelectedUser)
  RETURN .F.
ENDIF

IF EMPTY(&lcTmpHeader..STATUS)
  *- No user selected
  =gfModalGen('INM00000B00000', .F., .F., .F., LANG_SCANUPC_NoSelectedStatus)
  RETURN .F.
ENDIF

SELECT (lcTempDetail)
LOCATE FOR !DELETED()
IF EOF()
  *- No lines added
  =gfModalGen('INM00000B00000', .F., .F., .F., LANG_SCANUPC_NoLinesExists)
  RETURN .F.
ENDIF
RETURN .T.