*:***************************************************************************
*: Program file  : ALPKTKGP.PRG
*: Program desc. : Picking ticket form with barcode and Profile text based on form A        
*: Date          : 07/26/2022
*: System        : ARIA4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar(MMT)
*: Tracking NO   : E612605
*: TICKET NO     : T20220407.0001
*:***************************************************************************
*: Calls : 
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*Modifications:
*:***************************************************************************
SET STEP ON 
SELECT (lcTmpOrdL)
lcOrderCurrent = ORDER()
lcRelationCurrent = SET("Relation")
lcCurrIndexFld = KEY()
IF TYPE("&lcTmpOrdL..PKTKTFNT")<> 'U'
  lcNewTempOrd = lcTmpOrdL
ELSE 
  lcNewTempOrd = loogscroll.gfTempName()
  COPY TO (oAriaApplication.WorkDir+lcNewTempOrd+'.DBF') WITH cdx
  ALTER table (oAriaApplication.WorkDir+lcNewTempOrd+'.DBF') ADD PKTKTFNT G
  SELECT (lcNewTempOrd)
  INDEX ON &lcCurrIndexFld. TAG (lcNewTempOrd) additive  
ENDIF  
SELECT(lcNewTempOrd)
SCAN 
  SCATTER MEMO MEMVAR
  =OLEBarCode(m.PIKTKT)
ENDSCAN 
SET ORDER TO (lcOrderCurrent)
SET RELATION TO &lcRelationCurrent.
lcTmpOrdL =   lcNewTempOrd
SELECT (lcTmpOrdL)
LOCATE
*!**************************************************************
*! Name      : OLEBarCode
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/26/2022
*! Purpose   : Updating the general field for the report
*!**************************************************************
*! Called from : PRG
*!**************************************************************
*! Calls       : ....
*!**************************************************************
*! Passed Parameters : None
*!**************************************************************
*! Return      : None
*!**************************************************************
*! Example     : =OLEBarCode ()
*!**************************************************************
PROCEDURE OLEBarCode
  PARAMETERS  tcCUPC

  lcF_name = UPPER('PKTKTFNT')
  WITH loOgScroll && your form name
    IF TYPE('loOgScroll.'+ lcF_name) <> 'O'
      .ADDOBJECT(lcF_name,"OLEBoundControl")
    ENDIF
    .&lcF_name..CONTROLSOURCE = lcF_name
    .&lcF_name..WIDTH         = 300
    .&lcF_name..HEIGHT        = 200
    APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
    .&lcF_name..REFRESH
    .&lcF_name..OBJECT.DataToEncode = ALLTRIM(tcCUPC)
    .&lcF_name..OBJECT.Barheight    = 1
    .&lcF_name..OBJECT.SymbologyID  = 16&& 6
    .&lcF_name..OBJECT.showtext     = 0
    .&lcF_name..OBJECT.NarrowBarWidth =  0.04
    .&lcF_name..OBJECT.ORIENTATION = 0
    .&lcF_name..OBJECT.TopMarginCm = 0
    .&lcF_name..OBJECT.LeftMarginCm = 0
    .&lcF_name..OBJECT.Wide2NarrowRatio = 3
    .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
    .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
    .&lcF_name..OBJECT.AddCheckDigit = .F.
  ENDWITH
