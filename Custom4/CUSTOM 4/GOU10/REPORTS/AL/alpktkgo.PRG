*:***************************************************************************
*: Program file  : ALPKTKGO.PRG
*: Program desc. : CUSTOMIZED PICKING TICKET Form FOR GOU10.
*: Date          : 07/14/2014
*: System        : ARIA4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar(MMT)
*: Tracking NO   : C201632
*: TICKET NO     : T20140623.0002
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
  *APPEND GENERAL PKTKTFNT CLASS ("IDAuto.BarCode")
   =OLEBarCode(m.PIKTKT)
   *GATHER MEMO MEMVAR
ENDSCAN 
SET ORDER TO (lcOrderCurrent)
SET RELATION TO &lcRelationCurrent.
lcTmpOrdL =   lcNewTempOrd
SELECT (lcTmpOrdL)
LOCATE
*!**************************************************************
*! Name      : OLEBarCode
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/14/2014
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
    .&lcF_name..OBJECT.SymbologyID  = 4&& 6
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
