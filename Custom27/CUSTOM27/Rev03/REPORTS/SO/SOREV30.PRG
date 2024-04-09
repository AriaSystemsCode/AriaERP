*:***************************************************************************
*: Program file  : SOREV30
*: Program desc. : Open Orders Summaries for REV10
*: For Report    : SOVEN30.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry (MHM)
*: Date          : 11/01/2001
*: Reference     : C102486
*:**************************************************************************
*: Procedures    : lpCreatFil , lpCollData , 
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************

SELECT Codes
SET ORDER TO TAG cCode_No
IF !SEEK("NSEASON")
  *-- Message <This company has no Season codes, Cannot proceed.>
  *-- Buttons <                             OK                             >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"This company has no  Season codes, Cannot proceed.")
  RETURN  
ENDIF
*--if filter changed
IF llOGFltCh
  DO lpCreatFil
  DO lpCollData            && Collect the data for report.
ENDIF  

SELECT (lcWorkFile)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE

  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*: Developer : Mohamed Shokry (MHM)
*: Date      : 11/01/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lcOrder
lcOrder = ''     

SELECT OrdLine
SET RELATION TO cOrdType + Order INTO  OrdHdr ADDITIVE
  
SCAN FOR &lcRpExp AND (OrdHdr.Status $ "OH") AND cOrdType <> 'T'
  *--for Open Qty (from ordhdr file)
  WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT
  
  IF SEEK(Season,(lcWorkFile)) 
    SELECT (lcWorkFile)
    REPLACE nOpenQty   WITH nOpenQty  + OrdLine.TotQty,;
            nOpenAmnt  WITH nOpenAmnt + OrdLine.TotQty * OrdLine.Price,;
            nAlcatQty  WITH nAlcatQty + OrdLine.TotPik  ,;
            nAlcatAMNT WITH nAlcatAmnt+ OrdLine.TotPik * OrdLine.Price
  ELSE
    m.nOpenQty   = OrdLine.TotQty
    m.nOpenAmnt  = OrdLine.TotQty * OrdLine.Price
    m.nAlcatQty  = OrdLine.TotPik  
    m.nAlcatAmnt = OrdLine.TotPik * OrdLine.Price
    m.cSesnCod   = OrdLine.SEASON
    m.cSeason    = gfCodDes(OrdLine.SEASON,'SEASON')  

    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF
ENDSCAN      
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpCreatFil
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil

*-- Create a temporary table to hold data which will be displayed in report.

CREATE TABLE (gcWorkDir+lcWorkFile) (cSeason C(30),cSesnCod C(6),nAlcatAmnt N(13,2), nAlcatQty N(5),;
nOpenAmnt N(11,2), nOpenQty N(7))
INDEX ON cSesnCod TAG (lcWorkFile)

*-- End of lpCreatFil.
