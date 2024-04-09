*:***************************************************************************
*: Program file  : MFPRCTVI.PRG
*: Program desc. : Print Cutting Tickets for Vital in order to sort the bill of 
*:                 material report by fabric.loc
*: For Report    : MFPRCTVI.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Date          : 09/09/2001
*: Developer     : Khalid Mohi El-Din (KHM)
*: Reference     : C102681
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*:Modifications:
*:***************************************************************************
*:
IF llrPrtCs .AND. !EOF(lcCostF)

  PRIVATE lnCurAlias  
  lnCurAlias = SELECT(0)
  
  lcBillOfMt = gfTempName()

  SELECT (lcCostF)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)

  DIMENSION laFileStru[lnFileStru+1,4]

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cLoc'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  CREATE TABLE (gcWorkDir+lcBillOfMt) FROM ARRAY laFileStru
  
  
  SELECT (lcCostF)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT (lcBillOfMt)
    INSERT INTO (lcBillOfMt) FROM MEMVAR
    IF cCatgTyp $ 'FT' AND  SEEK(LEFT(Item,7)+IClr,'Fabric')
      SELECT (lcBillOfMt)
      REPLACE cLoc WITH Fabric.Loc    
    ENDIF
  ENDSCAN
  USE IN (lcCostF)
  IF FILE(gcWorkDir+lcCostF+'.DBF')
    ERASE (gcWorkDir+lcCostF+'.DBF')
  ENDIF  

  IF FILE(gcWorkDir+lcCostF+'.CDX')
    ERASE (gcWorkDir+lcCostF+'.CDX')
  ENDIF  
  
  USE IN (lcBillOfMt)
  RENAME (gcWorkDir+lcBillOfMt) TO (gcWorkDir+lcCostF)
  =gfOpenFile(gcWorkDir+lcCostF,' ','EX')

  IF llRpSortBy
    INDEX ON CUTTKT+Typ+cCatgTyp+cLoc+Item+IClr TAG (lcCostF) OF (lcCostF)
  ELSE
    INDEX ON CUTTKT+Typ+cCatgTyp+Item+IClr TAG (lcCostF) OF (lcCostF)
  ENDIF

  SELECT (lnCurAlias)
  
ENDIF