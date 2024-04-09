*:************************************************************************
*: Program file  : MAMATRQ.PRG
*: Program desc. : Material Requirements Report.
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MATERIALS (MA).
*:      Developer: TAK - Timour Abdalla Khalil 
*:           Date: 04/04/98               
*:   Call Screens: MAEDTPJ.SPX  Edit projection.
*:               : MAEDTFB.SPX  Edit -ve remaining materials.
*:   Call Reports: MAMATRQ.FRX  Summary format. 
*:               : MAMATRQH.FRX Detail Short format.
*:               : MAMATRQL.FRX Detail Long format.
*:               : MAOTS.FRX (E301128,1 Walid) Material Open to sell .
*:************************************************************************
*: Modifications :
*B602405,1 TAK 01/04/99   Take remaining qty insted of requirement.
*B602404,1 TAK 01/05/99   Take The onhand and onorder from FABDYE instead of
*B602404,1                fabric if fabric location has been selected in grid. 
*B602408,1 TAK 01/10/99   Added to update the duty currency also for the 
*B602408,1                common vendor in the -ve remaining materials.
*B602429,1 TAK 01/12/99   Fixed calculation of remaining to be by size.
*E301128,1 WALID 02/17/99 Make needed modifications to use this report
*E301128,1                as (material open to sell) report.
*B802011,1 AHM 03/22/99   calculate the fabric req. for impordted styles only
*B802011,1                if there are styles in filter
*B802158,1 TAK 05/09/99   calculate the requirement based on yield per size
*B802158,1                insted on avarege yield, and modify detail forms.
*B802377,1 TAK 07/24/99   Open codes file to elemenate alias codes not found.
*C200096,1 HDM 01/11/2000 Make a 2 custom new layouts
*                          1- By Fabric Trim
*                          2- By Color
*B603393,1 HDM 01/19/2000 Fixing bug of calculating incorrect data for a 
*                         newly added fabric to a style cost sheet
*B802981,1 HDM 01/30/2000 Correct the spelling error OF Geneteted TO Generated
*B802995,1 HDM 02/17/2000 Incorrect pointer to fabric file while generating
*                         Material POs
*B603605,1 SSH 01/05/2000 Print the fabric openqty by yards for Mexx
*B603633,1 ADEL 11/05/2000 Fix the big of not centering the second line in the following reports:
*B603633,1                (MAMATRQ.FRX - MAMATRQH.FRX- MAMATRQL.FRX).
*B603527,1 NAD 06/28/2000 Fix bug separate Po Print for each material even if they have the same
*B603527,1                Vendor,quality,currency,completion date 
*B603525,1 NAD 07/06/2000 Fix bug complete date in the The PO edit screen is in the format DD/MM/YY instead of
*B603525,1                DD/MM/YYYY	 
*B603378,1 NAD 07/06/2000 Fix bug wrong issued qty in detail long format by disabling the calculate option
*B603378,1 NAD            from the field in the MAMATRQL.FRX  
*B602909,1 NAD 07/09/2000 Fix bug The available qty has no decimals by removing the print when expr from
*B602909,1                in the MAMATRQL.FRX 
*B803561,1 NAD 08/03/2000 Fix the bug of not priting any styles when filter include zero quantity is set to 'No'.
*E500349,1 WAB 08/20/2000 ability to enter manual material PO #  
*B603818,1 ADEL 08/29/2000 When generation M. POs and Edit the qty with 0 and press OK, get off the edit screen.
*B603753,1 NAD  09/20/2000 Numeric over flow after running the report
*B603889,1 NAD  09/19/2000 Wrong vendor for the material in the 'Negative Remaining Materials' browse.
*B803005,1 NAD  09/20/2000 Change the browse window for the Generated Po's with a screen. 
*B604564,1 HBG 07/02/2001 Fix bug of "Requir.US" column is calculating wrong
*B604689,1 HBG 08/16/2001 Fix Bug of - Not allowing the Price of the material to be 0 when generate PO
*B604689,1                           - Repeting Ship to Name .
*B604785,1 HBG 08/16/2001 Fix bug of calculating issue Qty wrong
*B605049,1 WAB 10/22/2001 Fix the bug of Wrong calucalting the WIP and change the calcalting of the WIP 
*B605049,1 WAB                to be the budjet qty instead of the open qty for GMA only until they testing 
*B605049,1 WAB                this solution , then we will make is it standard for all customer ( AS PER OMAR )
*B605074,1 ADEL 11/25/01  Fix the bug of changing to Text mode and printing the text OTS report
*B605074,1                when pressing Rest but. (only delete the Form record in OTS SYREPUVR)
*B804511,1 ADEL 11/29/01  Fix the bug of not filling the Material types popup in OG (OTS).
*E301888,1 ABD 04/14/2002 add new option to the material requirment report to 
*E301888,1 ABD            get the styles outside the filter or not  Variable Is llRpIncSty.
*B606175,1 KHM 06/26/2002 Fix the problem of not applying the projected figures to all the cost
*B606175,1                items for the modified style.
*E301943,1 AMH 07/09/2002 Change the style option from between to be in list.
*C102741,1 AMH 11/04/2002 Add option to print report by warehouse.
*B606664,1 ABD 12/12/2002 Add the style status to the filter Exprition. 
*C200451,1 ABD 01/27/2003 Add new triger for SHI to get the plan Qty From the Forcast table.
*B606818,1 AMH 02/24/2003 Fix the bug of not included the PO with status A.
*B607082,1 AMH 03/23/2003 Fix the bug of wronge calculations in case of requirments by location.
*C102819,1 AMH 04/08/2003 Save the MRP# of the report.
*B607267,1 AMH 06/05/2003 Fix bug of No cost sheet information found.
*B607385,1 AMH 06/29/2003 Change the fabric option to be in list.
*E037351,1 HBG 23/12/2003 Add new field to SYREPUVR to differentiate between records belong to
*E037351,1                Aria27 and records belong to Aria4.
*B122673,1 NNA 05/15/2004 change the Vendmatl.Price field name to be (nFabCost) by add
*B122673,1 NNA            new record in sydflfld.dbf with (Vendmatl & nFabCost) and Replace 
*B122673,1 NNA            all (Price) with (nFabCost) in all related prg.
*B038431,1 NNA 09/09/2004 Fix bug that if you didn't define item code structure you'll get error
*B038431,1 NNA            [Variable 'laSeg' Not Found ] when you run this report , Fixing in the
*B038431,1 NNA            Syrepuvr file for Variable (lcStyHdr) in [Mdata_Def] Field
*B129606,1 EIH 09/29/2005 Fix bug that can't generate PO for negative remaining material.
*B608607,1 TMI 07/08/2008 Fix the bug the if the user has not make a previow then no Mat.PO's are not created for the not previoued pages ( ticket T20080331.0012 )
*:*********************************************************************************************************

*E301077,72 MAB intializing all variables [Begin]

IF TYPE('lcTmpRmain') $ 'UL'
  lcTmpRmain = ''
ENDIF

IF TYPE('lcMatReq') $ 'UL'
  lcMatReq = ''
ENDIF

IF TYPE('lcTmpItems') $ 'UL'
  lcTmpItems = ''
ENDIF

IF TYPE('lcPOTmpHD') $ 'UL'
  lcPOTmpHD = ''
ENDIF

IF TYPE('lcPOTmpLN') $ 'UL'
  lcPOTmpLN = ''
ENDIF
*E301077,72 MAB intializing all variables [Begin]


*B129606,1 EIH 09/29/2005 Fix bug that can't generate PO for negative remaining material [Begin].
*We comment this line cause of this varaible filled from option grid .
*NADER
*STORE '' TO lcRpMrpBas 
*NADER

*B129606,1 EIH 09/29/2005 [End].

*C102819,1 AMH Add variable to save the MRP report [Start]
PRIVATE llSaveMrp,lcMrp,laSaveVar
STORE .F. TO llSaveMrp
STORE SPACE(0) TO lcMrp
DECLARE laSaveVar[20,2]
laSaveVar[01,1] = 'lcDescrp'
laSaveVar[02,1] = 'lnOnHand'
laSaveVar[03,1] = 'lnOnOrdr'
laSaveVar[04,1] = 'lnAvl1'
laSaveVar[05,1] = 'lnAvl2'
laSaveVar[06,1] = 'lnAvl3'
laSaveVar[07,1] = 'lnAvl4'
laSaveVar[08,1] = 'lnAvl5'
laSaveVar[09,1] = 'lnAvl6'
laSaveVar[10,1] = 'lnAvl7'
laSaveVar[11,1] = 'lnAvl8'
laSaveVar[12,1] = 'lcFDesc'
laSaveVar[13,1] = 'lcClrDsc'
laSaveVar[14,1] = 'lcFabVen'
laSaveVar[15,1] = 'lnConv'
laSaveVar[16,1] = 'lnLeadTm'
laSaveVar[17,1] = 'lnFabcost'
laSaveVar[18,1] = 'lcUOMBuy'
laSaveVar[19,1] = 'lnfUOM'
laSaveVar[20,1] = 'llOTC'
*C102819,1 AMH [End]

STORE 0 TO lnOTotRem , lnHTotRem , lnPTotRem , lnOIssued , lnHIssued , lnPIssued
STORE '' TO lcFDesc , lcClrDsc
*B603605,1 SSH 01/05/2000 Print the fabric openqty by yards for Mexx
lnfUOM = 1
*B603605,1 SSH 01/05/2000 [End]
*E301128,1 Initiate unrequired variabels [BEGIN]
PRIVATE llFromOts
llFromOts = (lcOgRepid = 'MAOTS')  && Called from Requirement or O.T.S.

*-- if Called from O.T.S.
IF llFromOts
  lcSTime    = TIME()

  *-- Define variables used in .FRX [Begin
  lnRemainO  = 0 
  lnRemainH  = 0
  lnRemainP  = 0
  lnOReq     = 0
  lnHReq     = 0
  lnPReq     = 0
  *-- Define variables used in .FRX [End

  lcCurrMark =  SET('CURRENCY',1)
  lcRpComPn  = 'A'
  lcRpRmChk  = 'O'
  lcRpBasePj = 'P'
  
  *C102741,1 AMH Change the name of variable llRpOTC to be lcRpReqBas [Start]
  *llRpOTC    = .F.
  lcRpReqBas = 'B'
  *C102741,1 AMH [End]
  
  llRpEditPj = .F.
  llRpInvItm = .F.
  llRpPastCm = .F.
  llRpzeroRq = .T.
  lnRpPrinCr = 0
ENDIF  && end if Called from O.T.S.
*E301128,1 Initiate unrequired variabels [End]

*--The report has 3 filter expretions.
*--lcRpExp0  Expresion on Style file.
*--lcRpExp1  Expresion on OrdLine file.
*--lcRpExp2  Expresion on Bom file.

*E301943,1 AMH Don't copy the style filter to laStyFlt since we will add it to lcRpExp0 
*E301943,1     without using the gfGenFlt function [Start]
*=ACOPY(laOGFxFlt,laStyFlt, 1,56)
*DIME laStyFlt[8,7]
=ACOPY(laOGFxFlt,laStyFlt, 8,56)
DIME laStyFlt[7,7]
*E301943,1 AMH [End]

=ACOPY(laOGFxFlt,laOrdFlt,57,21)
laOrdFlt[1,6]=DTOC(laOrdFlt[1,6])
DIME laOrdFlt[3,7]

*E301128,1 Redimension the array to include O.T.S. added filter [BEGIN]
*=ACOPY(laOGFxFlt,laBomFlt,78,21)
*DIME laBomFlt[3,7]

*B607385,1 AMH Don't copy the fabric filter to laBomFlt since we will add it to lcRpExp2 
*B607385,1     without using the gfGenFlt function [Start]
*=ACOPY(laOGFxFlt,laBomFlt,78,IIF(llFromOts,35,21))
*DIMENSION laBomFlt[IIF(llFromOts,5,3),7]
=ACOPY(laOGFxFlt,laBomFlt,85,IIF(llFromOts,28,14))
DIMENSION laBomFlt[IIF(llFromOts,4,2),7]
*B607385,1 AMH [End]

*E301128,1 Redimension the array to include O.T.S. added filter [End]

lcRpExp0 = gfGenFlt('laStyFlt',.T.)
lcRpExp0 = IIF(EMPTY(lcRpExp0),".T.",lcRpExp0) 
lcRpExp1 = gfGenFlt('laOrdFlt',.T.)
lcRpExp1 = IIF(EMPTY(lcRpExp1),".T.",lcRpExp1) 
lcRpExp2 = gfGenFlt('laBomFlt',.T.)
lcRpExp2 = IIF(EMPTY(lcRpExp2),".T.",lcRpExp2) 

*B607385,1 AMH Add the fabric filter to lcRpExp2 [Start]
lcFabFile = ''
llFabRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SUBSTR(Bom.item,1,7)'),1)
IF lnPosition > 0
  lcFabFile = laOGFxFlt[lnPosition,6]
  llFabRang = IIF(!EMPTY(lcFabFile) .AND. USED(lcFabFile) .AND. RECCOUNT(lcFabFile)>0,.T.,.F.)
ENDIF
lcRpExp2 = lcRpExp2 + IIF(llFabRang,' .AND. SEEK(SUBSTR(BOM.ITEM,1,7),lcFabFile)','')
*B607385,1 AMH [End]

*--If fabric color was selected in grid filter it on variable istead of field
*--Bom.iclr wich may be hald astricks.
IF AT('INLIST',lcRpExp2) <> 0
  lcRpExp2 = STRTRAN(lcRpExp2,'Bom.iclr','lcIClr')
ENDIF
*--If Exclude orders past completion is Yes update the orders filter.
IF llRpPastCm
  lcRpExp1 = lcRpExp1 +" .AND. ORDHDR.COMPLETE>=gdSysDate"
ENDIF


*--Program variabels.
*--Read the Style major part length and item title.
lcMjrPct  = gfItemMask('PM')
lcStyTtl  = gfItemMask('HI')
lCnMJPct  = gfItemMask('PN')
lnMajorLn = LEN(lcMjrPct)
lnNMjrLn  = LEN(lCnMJPct)
*--Use extended size scale.  
llMScale  = gfGetMemVar('M_USEEXSSC')
*--Show OTC equal to grid variable.

*C102741,1 AMH Change the name of variable llRpOTC to be lcRpReqBas [Start]
*llOtc     = llRpOtc
llOtc     = (lcRpReqBas='O')
*C102741,1 AMH [End]

lcBaseProj= lcRpBasePj

*--Need to update plane in style file.
llUpdPlan = .F.
*--Flag for style by location or no (Lcation was entered in grid).
llByLoctn = ('STYDYE.' $ lcRpExp0)
*--Read style location.
lcLocation= IIF(llByLoctn,SUBSTR(lcRpExp,AT('STYDYE.',lcRpExp)+20,6),"")


*--flag if style range was entered in grid.
*E301943,1 AMH Get the selected styles if any [Start]
*llStyRang = ('BETWEEN(STYLE.STYLE' $ lcRpExp0)
lcStylFile = ''
llStyRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.STYLE'),1)
IF lnPosition > 0
  lcStylFile = laOGFxFlt[lnPosition,6]
  llStyRang = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
*--Low style major code in entered range.
*lcLMStyle = IIF(llStyRang,SUBSTR(lcRpExp,22,lnMajorLn),"")
IF llStyRang
  SELECT (lcStylFile)
  LOCATE
  lcLMStyle = SUBSTR(CSTYMAJOR,1,lnMajorLn)
  lcRpExp0 = IIF(lcRpExp0='.T.','',lcRpExp0+' .AND. ') +;
             'SEEK(SUBSTR(STYLE.STYLE,1,lnMajorLn),lcStylFile)'
ELSE
  lcLMStyle = ''
ENDIF
*E301943,1 AMH [End]

*--Flag for fabric by location or no (Lcation was entered in grid).
llByFabLoc= ('FABDYE.' $ lcRpExp2)
*--Read fabric location.
lcFLoctn  = IIF(llByFabLoc,SUBSTR(lcRpExp2,AT('FABDYE.',lcRpExp2)+20,6),"")

*--Check if there is no filter on style.

llNoStyFlter = ( lcRpExp0 = ".T." ) OR ;
  ( llByLoctn AND EMPTY(SUBSTR(lcRpExp0 ,1,AT('STYDYE.',lcRpExp0 )-1)) )

*--Open report needed files.

*C102741,1 AMH Open files needed for case of print by warehouse [Start]
*DIME laArOpnFl[7]
DIME laArOpnFl[9]
*C102741,1 AMH [End]

laArOpnFl = .F.
laArOpnFl[1] = gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
laArOpnFl[2] = gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
laArOpnFl[3] = gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
IF (OCCURS('PO',gcCmpModules)<>0)
  laArOpnFl[4] = gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
  laArOpnFl[5] = gfOpenFile(gcDataDir+'POSLN','POSLNS','SH')
  SET RELATION TO cStyType+PO INTO POSHDR
ENDIF
IF (OCCURS('MF',gcCmpModules)<>0) 
  laArOpnFl[6] = gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
  laArOpnFl[7] = gfOpenFile(gcDataDir+'CUTTKTL','CUTTKTLS','SH')
  SET ORDER TO TAG CUTTKTLS DISENDING
  SET RELATION TO CUTTKT INTO CUTTKTH
ENDIF

*C102741,1 AMH Open files needed for case of print by warehouse [Start]
IF llRpByWare
  laArOpnFl[8] = gfOpenFile(gcDataDir+'CODES','Idrltfname','SH')
  laArOpnFl[9] = gfOpenFile(gcDataDir+'MFGOPRHD','MFGOPRHD','SH')
ENDIF
*C102741,1 AMH [End]

*--Create MR temp file with structure.
=lfCreateFl()
= gfOpenFile(gcWorkDir+lcMatReq,'','EX')
*--Open needed indeces.
INDEX ON cCatgTyp+Item+IClr+Style TAG Matreq ADDITIVE
INDEX ON Style UNIQUE TAG Mrstyle ADDITIVE
INDEX ON Style+cCatgTyp+Item+IClr TAG Mrstyitm ADDITIVE

*C102741,1 AMH Add New Index to sort by fabric location [Start]
INDEX ON CCATGTYP+CINHOUSE+CFABLOC+ITEM+ICLR+STYLE TAG MRFABLOC ADDITIVE

*C102819,1 AMH Add New Index for Detail line layout [Start]
INDEX ON CCATGTYP+ITEM+ICLR+STYLE+DTOS(COMPLETE)+ORDER TAG MATREQDT ADDITIVE
INDEX ON CCATGTYP+CINHOUSE+CFABLOC+ITEM+ICLR+STYLE+DTOS(COMPLETE)+ORDER TAG MRFABLOCDT ADDITIVE
*C102819,1 AMH [End]

*C102741,1 AMH [End]

IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  INDEX ON cCatgTyp+IClr+Item+Style TAG MatreqC ADDITIVE
ENDIF

*--Create and Open items temp file.
IF ! llNoStyFlter
  IF USED(lcTmpItems)
    USE IN (lcTmpItems)
  ENDIF
  COPY STRU TO (gcWorkDir+lcTmpItems)
  = gfOpenFile(gcWorkDir+lcTmpItems,'','EX')
  INDEX ON Typ+Item+IClr TAG (lcTmpItems)
ENDIF

*C102819,1 AMH Print the existing MRP report [Start]
IF !llFromOts .AND. lcRpMrpBas = 'E'
  =lfPrntMrp()
  RETURN
ENDIF
*C102819,1 AMH [End]

** (1) FIRST PART STEPS. ********************************************
*--Calculating the total yield (qty/unit) in Cost sheet.
*- Scan for styles with selected criteria.
*- Check if style major has a cost sheet.
*- Append or Update record in (lcMatReq) file. 
*- Update the total yield (qty/utit) in (lcMatReq) file.
*********************************************************************
SELECT (lcMatReq)
SET ORDER TO TAG MATReq

*C102741,1 AMH Sort by fabric location in case print by warehouse Yes [Start]
IF llRpByWare
  SET ORDER TO TAG MRFABLOC
ENDIF
*C102741,1 AMH [End]

SELECT BOM
SET ORDER TO TAG BOM
SELECT STYLE
SET ORDER TO TAG Style

*--Prepare the scan file and expresion.
IF llByLoctn 
  SELECT STYDYE
  SET RELATION TO Style INTO Style
  SET ORDER TO TAG Stydyew
  IF llStyRang
    SEEK lcLocation+lcLMStyle
    
    *B607267,1 AMH if location/style not found in stydye seek for the location only [Start]
    IF EOF()
      SEEK lcLocation
    ENDIF
    *B607267,1 AMH [End]
    
    lcWhileExp = "cWareCode = lcLocation"
  ELSE 
    SEEK lcLocation
    lcWhileExp = "cWareCode = lcLocation"
  ENDIF
ELSE
  SELECT STYLE
  IF llStyRang
    SEEK lcLMStyle
    lcWhileExp = "SUBSTR(Style,1,lnMajorLn) >= lcLMStyle"
  ELSE
    SET ORDER TO
    GO TOP
    lcWhileExp = ".T."
  ENDIF
ENDIF

*B606664,1 ABD - Add the style status to the filter Exprition. [Begin]
lcStyleExp  = IIF(llRpActSty,' .T. ', "Style.Status # 'X'")
*SCAN WHILE &lcWhileExp FOR &lcRpExp0
SCAN WHILE &lcWhileExp FOR &lcRpExp0 .AND. &lcStyleExp
*B606664,1 ABD - [End]

  *--Dont take the dyelot records.
  IF llByLoctn AND !EMPTY(STYDYE.Dyelot)
    LOOP  
  ENDIF

  *--Check if the style has a cost sheet.
  IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
    LOOP
  ENDIF
    
  *--Current style code and style scale.
  lcCStyle = STYLE.Style
  lnCnt    = IIF( SEEK("S"+STYLE.Scale,'SCALE'), SCALE.Cnt , 8 )
  WAIT WINDOW 'Collecting the cost sheet information for Style : '+lcCStyle NOWAIT

  SELECT BOM

  *-- walid in this code a relation between Fabric and Bom must be created [begin]

  *E301128,1 if Called from O.T.S. [Begin]
  IF llFromOts
    lcFabOrder = ORDER('FABRIC')
    lcFabRec   = FABRIC.FABRIC + FABRIC.COLOR   

    SET ORDER TO FABRIC IN FABRIC
    *B804511,1 (Begin) Fix the relation to get the records right in Fbric file.
    *SET RELATION TO BOM.Item INTO FABRIC
    SET RELATION TO ALLTRIM(BOM.Item) INTO FABRIC
    *B804511,1 (End)
  ENDIF 
  *E301128,1 if Called from O.T.S. [End  ]

  SCAN WHILE cItmMajor = SUBSTR(lcCStyle,1,lnMajorLn) ;
         FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
             cCatgTyp $ IIF(lcRpCompn='A','FTS',lcRpCompn)

    *--Don't include non inventory trims if not setup to use it.
    IF (cCatgTyp='T' AND !Trim_Invt AND !llRpInvItm)   
      LOOP
    ENDIF  

    *--Read an item code and an item color[lcItem,lcIClr].
    STORE '' TO lcItem,lcIClr
    IF !lfReadItem()
      LOOP
    ENDIF 

    *--If commponent is fabric check fabric and colors in grid filter.
    
    *B607267,1 AMH Check also for tims [Start]
    *IF llByFabLoc AND cCatgTyp='F' 
    IF llByFabLoc AND (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt))
    *B607267,1 AMH [End]
    
      IF !SEEK(PADR(lcItem,7)+lcIClr+lcFLoctn,'FABDYE')
        LOOP
      ENDIF
    ENDIF
    *--Check if maririal/color entered in grid is valid.
    
    *B607082,1 AMH Don't include non inventory trims if not in the selection critria [Start]
    *IF (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt)) AND !( &lcRpExp2 ) 
    IF (cCatgTyp = 'F' OR cCatgTyp='T') AND !( &lcRpExp2 ) 
    *B607082,1 AMH [End]
    
      LOOP
    ENDIF

    *--Append or Update record in (lcMatReq) file. 
    =lfUpdReq()

    *--Append item/color used in style cost sheet in temp items file. 

    *--B802011 (Start)
    *IF ! llNoStyFlter
    IF ! llNoStyFlter AND !(Style.Make)
    *--B802011 (End)
      =lfUpdItem()
    ENDIF

  ENDSCAN

  *E301128,1 if Called from O.T.S. [Begin]
  IF llFromOts
    lnCurrAlis = SELECT(0)
    =SEEK(lcFabRec,'FABRIC')
    SELECT BOM
    SET RELATION OFF INTO FABRIC
    
    SELECT FABRIC
    SET ORDER TO &lcFabOrder
    SELECT (lnCurrAlis)
  ENDIF 
  *E301128,1 if Called from O.T.S. [End  ]
  
ENDSCAN

WAIT CLEAR

*--Check requirements existance.
GO TOP IN (lcMatReq)

IF EOF(lcMatReq)
  *--No cost sheet information found for any of the selected
  *--styles or fabrics 'in location'.
  *--                           [Ok]
  =gfModalGen('TRM36107B36000','DIALOG',IIF(llByLoctn OR llByFabLoc,'in location',''))
  RETURN
ENDIF



** (2) SECOND PART STEPS. *******************************************
*--Calculating the total yield (qty/utit) in Cost sheet.
*- Scan for selected items in preivius step.
*- Scan for styles out of the selected criteria for this items.
*- Append or Update record in (lcMatReq) file. 
*- Update the total yield (qty/utit) in (lcMatReq) file.
*********************************************************************

*--Start select styles that use the same item components and 
*--out of the grid filter.
*--Only if there is a filter used ,becouse if not this styles allready
*--selected if preivus part.
IF llByLoctn 
  SELECT STYDYE
  SET RELATION TO 
  SET ORDER TO TAG Stydye
ENDIF
SELECT STYLE
SET ORDER TO TAG STYLE
IF ! llNoStyFlter
  SELECT BOM
  SET ORDER TO TAG BOMItem 
  SELECT (lcTmpItems)
  SCAN
    IF cCatgTyp='S'
      lnAstPos = AT('*',Item)
      lcKeyCnd = IIF(lnAstPos=0,Item,ALLTRIM(SUBSTR(Item,1,lnAstPos-1)) )
    ELSE
      lcKeyCnd = Typ + Item
    ENDIF
    lcWhlCnd = "Typ + Item = lcKeyCnd"
    WAIT WINDOW 'Checking for styles useing material : '+PADR(Item,19) NOWAIT

    SELECT BOM
    SEEK lcKeyCnd
    SCAN WHILE &lcWhlCnd ;
       FOR LIKE(STRTRAN(Item,'*','?'),&lcTmpItems..Item) AND ;
           (IClr = '******' OR IClr = &lcTmpItems..IClr)

      *--Current item code and an item color.
      lcItem = &lcTmpItems..Item
      lcIClr = &lcTmpItems..IClr

      *--Read the style code and an style color for Exist Item.
      lnAstPos = AT('*',cItmMask)
      lcStyMask = IIF(lnAstPos = 0 , cItmMask ,;
                     ALLTRIM(SUBSTR(cItmMask,1,lnAstPos-1)) )

      lcStyClr = ""
      IF CCATGTYP $ 'FT'
        IF lnAstPos <> 0 AND lnClrSrt<>0 AND SUBSTR(cItmMask,lnClrSrt,lnClrEnd) ='*'
          lcStyClr = lcIClr
        ENDIF
      ELSE
        IF lnAstPos <> 0
          lcStyClr = SUBSTR(lcItem,lnAstPos,LEN(lcItem)-IIF(llMScale,3,0))
        ENDIF
      ENDIF
      
      *--Check if style is out of filter.
      SELECT STYLE
      SEEK lcStyMask
      
      IF llByLoctn 
        SELECT STYDYE 
        SEEK lcStyMask
        *B605049,1 WAB (Start) select the style file before the scan 
        SELECT STYLE
        *B605049,1 WAB (End)
      ENDIF

      *E301888,1 ABD - Add new option to the material requirment report to 
      *E301888,1 ABD - Get the styles outside the filter or not  Variable Is llRpIncSty. [Begin]
      IF llRpIncSty
        *E301888,1 ABD - [End]
        *B606664,1 ABD - Add the style status to the filter Exprition. [Begin]
        lcStyleExp  = IIF(llRpActSty,' .T. ', "Style.Status # 'X'")
        *SCAN WHILE Style = lcStyMask FOR !( &lcRpExp0 ) AND ;
        *           LIKE(STRTRAN(BOM.cItmMask,'*','?'),Style) AND ;
        *           SUBSTR(Style,lnClrSrt,lnClrEnd) = lcStyClr
        SCAN WHILE Style = lcStyMask FOR !( &lcRpExp0 ) .AND.  &lcStyleExp .AND. ;
                   LIKE(STRTRAN(BOM.cItmMask,'*','?'),Style) AND ;
                   SUBSTR(Style,lnClrSrt,lnClrEnd) = lcStyClr
          *B606664,1 ABD - [End]
          *--Current style code and style scale.
          lcCStyle = STYLE.Style
          lnCnt    = IIF( SEEK("S"+STYLE.Scale,'SCALE') , SCALE.Cnt , 8 )
   
          *--Append or Update record in (lcMatReq) file. 
          *B605049,1 WAB (Start) in case select by location we must set the pointer in the stydye to the correct location
          *IF llByLoctn   && cWareCode = lcLocation
          IF llByLoctn   AND SEEK(lcCStyle+lcLocation,'STYDYE')
          *B605049,1 WAB (ENd)
            =lfUpdReq()
          ENDIF

          IF !llByLoctn
            =lfUpdReq()
          ENDIF
  
        ENDSCAN
        *E301888,1 ABD - End For If Statment. [Begin]
      ENDIF
      *E301888,1 ABD - [End]
      
    ENDSCAN 
  ENDSCAN
  WAIT CLEAR
  USE IN (lcTmpItems)
  ERASE (gcWorkDir+lcTmpItems+'.DBF')
  ERASE (gcWorkDir+lcTmpItems+'.CDX')
ENDIF


** (3) THERD PART STEPS. ********************************************
*--Calculating the Open-to-Cut for the selected styles.
*- Scan for styles selected in privius steps.
*- Calculate ordered pieces.
*- Calculate work in prosses pieces.
*- Calculate Requirements Pieces per size.
*- Compute the Projection      
*- Update style with Requirements pieces.
*********************************************************************

SELECT (lcMatReq)

*C102819,1 AMH Calculating the requirements in case of Detail line layout [Start]
IF lcRpRName = 'MAMATRQD'
  =lfPrntDtLn()
  RETURN
ENDIF
*C102819,1 AMH [End]

*--Initilize report variables and arrays.
*--[1] Ordered pieces for Open and Open+Hold orders. 
DECLARE laOrders[2,9]               && Row =1 OPEN ,2=HOLD+OPEN
*--[2] WIP open pieces for Actualize,Open,Hold C/t.
DECLARE laWIP[3,9]                  && ROW =1 'A', =2 'O', =3 'H'
*--[3] Pieces requirements for Open and Open+Hold orders or Plane. 
DECLARE laRequr[3,9]                 && Row =1 OPEN ,2=HOLD+OPEN, 3=PLANE

*--Thermometer counter.
lnRecCount = RECCOUNT()
lnCurNum   = 0
*--Set tag on styles unique.
SET ORDER TO TAG MRStyle

SCAN
  *--Current style code.
  lcCStyle = Style
  lnCurNum = lnCurNum + 1
  =gfThermo(lnRecCount,lnCurNum ,"Calculating requirements",'Style : '+Style)

  *--Style scale count.
  lnCnt    = Cnt
  = SEEK(lcCStyle , 'STYLE')
  lcChkFile = 'STYLE'
  IF llByLoctn
    = SEEK(lcCStyle+lcLocation,'STYDYE')
    lcChkFile = 'STYDYE'
  ENDIF
  lcForWare = IIF( llByLoctn ,"cWarecode = lcLocation" , ".T." )
  
  *--Initilize variables and arrayes for starting calculation.
  laOrders  = 0        && Ordered pieces for Open and Open+Hold orders.   
  laWIP     = 0        && WIP pieces.
  laRequr   = 0        && Requirements pieces.


  *--Calculate ordered pieces.
  *C102741,1 AMH Not need to calculate ordered pieces in WIP case [Start]
  *IF &lcChkFile..TotOrd <> 0
  IF lcRpReqBas <>'W' .AND. &lcChkFile..TotOrd <> 0
  *C102741,1 AMH [End]
  
    =lfChkOrder()
  ENDIF 

  *B603393,1 [Start] comment out this part and insert it into scan section
  *--Calculate work in prosses pieces.
  *IF &lcChkFile..TotWip <> 0
  *    IF STYLE.Make
  *      *--For cutting ticket. 
  *      =lfChk_CT()
  *    ELSE
  *      *--For Purchase order.
  *      =lfChk_PO()
  *    ENDIF
  *ENDIF 
  *B603393,1 [End]

  *B603393,1 [Start] comment out this part and insert it into scan section
  *  *--Calculate Requirements Pieces per size.
  *  FOR I=1 TO lnCnt
  *    Z=STR(I,1)
  *    *--Read the style Stock pieces.
  *    lnStock = IIF(&lcChkFile..Stk&Z  > 0, &lcChkFile..Stk&Z , 0)
  *    *--[1] Requirements Pieces for OPEN orders.
  *    lnReqPieces = laOrders[1,I] - ( lnStock + laWIP[1,I] + IIF(llRpOTC,laWIP[2,I],0) )
  *    laRequr[1,I] = IIF(llRpOTC,lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
  *    laRequr[1,I] = MAX( laRequr[1,I] , 0 )
  *    laRequr[1,9] = laRequr[1,9] + laRequr[1,I]
  *    *--[2] Requirements Pieces for OPEN+HOLD orders.
  *    lnReqPieces = laOrders[2,I] - ( lnStock + laWIP[1,I] + IIF(llRpOTC,laWIP[2,I],0) )
  *    laRequr[2,I] = IIF(llRpOTC,lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
  *    laRequr[2,I] = MAX( laRequr[2,I] , 0 )
  *    laRequr[2,9] = laRequr[2,9] + laRequr[2,I]
  *    *--[3] Requirements Pieces for Projection (dependent).
  *    *--Compute the Projection (Depends on requirements calculated before).    
  *    *--or depens on plane if select projection for plane.
  *    DO CASE
  *      *--[1] Projection base on Open.
  *      CASE lcRpBasePj = 'O'
  *        laRequr[3,I] = laRequr[1,I] + ROUND(laRequr[1,I] * (lnRpPrIncr/100),0)
  *      *--[2] Projection base on Open+Hold.
  *      CASE lcRpBasePj = 'H' 
  *        laRequr[3,I] = laRequr[2,I] + ROUND(laRequr[2,I] * (lnRpPrIncr/100),0)
  *      *--[3] Requirements Pieces for Plane.    
  *      CASE lcRpBasePj = 'P' 
  *        laRequr[3,I] = IIF(STYLE.Plan&Z > 0, STYLE.Plan&Z, 0)
  *    ENDCASE
  *    laRequr[3,9] = laRequr[3,9] + laRequr[3,I]
  *  ENDFOR
  *B603393,1 [End]

  *--Append style with Requirements.
  *- Don't take the styles with zero requirements if the report
  *- setup to not print zero requirements.
  
  *B803561,1 NAD 08/03/2000 (Start) Comment the If Condition because there is no need for it after
  *B803561,1                inserting the part that calculate requirements inside the scan
  *B803561,1                by the entry no. B603393.
  *IF llRpZeroRq OR laRequr[2,9] <> 0
  *B803561,1 (End)
  
  *C102741,1 AMH Consider case of fabric in C/T cost sheet not in the BOM file for WIP [Start]
  IF lcRpReqBas = 'W'
    =lfWipReq()
  ELSE
  *C102741,1 AMH [End]
  
    SELECT (lcMatReq)
    SET ORDER TO TAG MRStyItm
    SEEK lcCStyle
    *--Start update the style with the qty. 
    SCAN WHILE Style = lcCStyle
      *B603393,1 [Start] Reset laWip And laRequr arrayes
      FOR lnXLoop = 1 TO 3
        FOR lnYLoop = 1 TO 9
          laWIP[lnXLoop,lnYLoop] = 0
        ENDFOR
      ENDFOR

      FOR lnXLoop = 1 TO 3
        FOR lnYLoop = 1 TO 9
          laRequr[lnXLoop,lnYLoop] = 0
        ENDFOR
      ENDFOR
      *B603393,1 [End]
      IF STYLE.Make
        *--For cutting ticket. 
        =lfChk_CT()
      ELSE
        *--For Purchase order.
        =lfChk_PO()
      ENDIF
    *--Calculate Requirements Pieces per size.
    
    
    FOR I=1 TO lnCnt

      Z=STR(I,1)

      *--Read the style Stock pieces.
      lnStock = IIF(&lcChkFile..Stk&Z  > 0, &lcChkFile..Stk&Z , 0)

      *--[1] Requirements Pieces for OPEN orders.
      
      *C102741,1 AMH Change the name of variable llRpOTC to be lcRpReqBas [Start]
      *lnReqPieces = laOrders[1,I] - ( lnStock + laWIP[1,I] + IIF(llRpOTC,laWIP[2,I],0) )
      *laRequr[1,I] = IIF(llRpOTC,lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
      lnReqPieces = laOrders[1,I]-(lnStock+laWIP[1,I]+IIF(lcRpReqBas='O',laWIP[2,I],0))
      laRequr[1,I] = IIF(lcRpReqBas='O',lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
      *C102741,1 AMH [End]
      
      laRequr[1,I] = MAX( laRequr[1,I] , 0 )
      laRequr[1,9] = laRequr[1,9] + laRequr[1,I]
  
      *--[2] Requirements Pieces for OPEN+HOLD orders.
      
      *C102741,1 AMH Change the name of variable llRpOTC to be lcRpReqBas [Start]
      *lnReqPieces = laOrders[2,I] - ( lnStock + laWIP[1,I] + IIF(llRpOTC,laWIP[2,I],0) )
      *laRequr[2,I] = IIF(llRpOTC,lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
      lnReqPieces = laOrders[2,I]-(lnStock+laWIP[1,I]+IIF(lcRpReqBas='O',laWIP[2,I],0))
      laRequr[2,I] = IIF(lcRpReqBas='O',lnReqPieces,MAX(lnReqPieces,(laWIP[2,I]+laWIP[3,I])))
      *C102741,1 AMH [End]
      
      laRequr[2,I] = MAX( laRequr[2,I] , 0 )
      laRequr[2,9] = laRequr[2,9] + laRequr[2,I]

      *--[3] Requirements Pieces for Projection (dependent).
      *--Compute the Projection (Depends on requirements calculated before).    
      *--or depens on plane if select projection for plane.
      DO CASE
        *--[1] Projection base on Open.
        CASE lcRpBasePj = 'O'
          laRequr[3,I] = laRequr[1,I] + ROUND(laRequr[1,I] * (lnRpPrIncr/100),0)
        *--[2] Projection base on Open+Hold.
        CASE lcRpBasePj = 'H' 
          laRequr[3,I] = laRequr[2,I] + ROUND(laRequr[2,I] * (lnRpPrIncr/100),0)
        *--[3] Requirements Pieces for Plane.    
        CASE lcRpBasePj = 'P' 
          *B606175,1 KHM 06/26/2002 (Begin) Taking into consideration the percentage.
          *laRequr[3,I] = IIF(STYLE.Plan&Z > 0, STYLE.Plan&Z, 0)
          laRequr[3,I] = IIF(STYLE.Plan&Z > 0, STYLE.Plan&Z + ROUND(STYLE.Plan&Z*((lnRpPrIncr/100)),0), 0)
          *B606175,1 KHM 06/26/2002 (End)
          
          *C200451,1 ABD - Add new triger for SHI to get the plan Qty From the Forcast table. [Begin]
          = gfDoTriger('MAMATRQ',PADR('GETPRDPL',10))
          *C200451,1 ABD - [End]

      ENDCASE
      laRequr[3,9] = laRequr[3,9] + laRequr[3,I]    

  ENDFOR
  REPLACE OReq1    WITH laRequr[1,1],;
          OReq2    WITH laRequr[1,2],;
          OReq3    WITH laRequr[1,3],;
          OReq4    WITH laRequr[1,4],;
          OReq5    WITH laRequr[1,5],;
          OReq6    WITH laRequr[1,6],;
          OReq7    WITH laRequr[1,7],;
          OReq8    WITH laRequr[1,8],;
          OReqTot  WITH laRequr[1,9],;
          OHReq1   WITH laRequr[2,1],;
          OHReq2   WITH laRequr[2,2],;
          OHReq3   WITH laRequr[2,3],;
          OHReq4   WITH laRequr[2,4],;
          OHReq5   WITH laRequr[2,5],;
          OHReq6   WITH laRequr[2,6],;
          OHReq7   WITH laRequr[2,7],;
          OHReq8   WITH laRequr[2,8],;
          OHReqTot WITH laRequr[2,9],;
          nProj1   WITH laRequr[3,1],;
          nProj2   WITH laRequr[3,2],;
          nProj3   WITH laRequr[3,3],;
          nProj4   WITH laRequr[3,4],;
          nProj5   WITH laRequr[3,5],;
          nProj6   WITH laRequr[3,6],;
          nProj7   WITH laRequr[3,7],;
          nProj8   WITH laRequr[3,8],;
          Project  WITH laRequr[3,9]
      *--Update the open WIP pieces.
      *B602429,1 Added to update open wip pieces per size not only the total.

      REPLACE nOpnWip1 WITH laWIP[2,1],;
              nOpnWip2 WITH laWIP[2,2],;
              nOpnWip3 WITH laWIP[2,3],;
              nOpnWip4 WITH laWIP[2,4],;
              nOpnWip5 WITH laWIP[2,5],;
              nOpnWip6 WITH laWIP[2,6],;
              nOpnWip7 WITH laWIP[2,7],;
              nOpnWip8 WITH laWIP[2,8],;
              nOpnWip  WITH laWIP[2,9]
      *B602429,1 End.
    ENDSCAN
    SET ORDER TO TAG MRStyle
    SEEK lcCStyle
  *B803561,1 NAD 08/03/2000 (Start) Comment the If Condition because there is no need for it after
  *B803561,1                inserting the part that calculate requirements inside the scan
  *B803561,1                by the entry no. B603393.
  *ENDIF
  *B803561,1 (End)
  
  *C102741,1 AMH End if WIP case [Start]
  ENDIF
  *C102741,1 AMH [End]
  
ENDSCAN
=gfThermo(100,100)
WAIT CLEAR

SELECT (lcMatReq)
SET ORDER TO TAG MATREQ

*--Check if there is requirement.
IF !llRpZeroRq
  SET FILTER TO OHReqTot <> 0
ENDIF
GO TOP


*--Check requirements existance.
IF EOF()
  *--None of the selected styles have any requirements.
  *--                     [Ok]
  =gfModalGen('TRM36108B36000','DIALOG')
  RETURN
ENDIF

*--If Edit Projection is required.
IF llRpEditPj
  =lfEditProj()
ENDIF

*E301128,1 if called from Requirement only [Begin]
*-- There is no need for the followin block of code in case of O.T.S
IF !llFromOts  &&-- if called from Requirement only do the next block of code .
  *--Open -ve remaining items file.
  lcTmpRmain = gfTempName()
  SELECT (lcMatReq)
  *B603527,1 (Start) Change cDutyCur and cPriceCur width to 3 instead of 5
  *CREATE DBF (gcWorkDir+lcTmpRmain) ;
   ( Fabric C(7), Color C(6),Vendor C(8),nFabTotQty N(11,3),LeadTime N(2),;
     Complete D(8),UOMBuy C(3),cFabGrade C(1),nfabcost N(9,3),cPriceCur C(5),;
     nPriceRat N(9,4),nCurrUnit N(6),cDutyCur C(5),nDutyRat N(9,4),nDCurUnit N(6),;
     nItm_Frt N(9,3),nItem_Tax N(9,3), nItemQuota N(9,3), cWareCode C(6) )
  *B603753,1 (Start) Field LeadTime is N(3) as in the Fabric master file. 
  *CREATE DBF (gcWorkDir+lcTmpRmain) ;
   ( Fabric C(7), Color C(6),Vendor C(8),nFabTotQty N(11,3),LeadTime N(2),;
     Complete D(8),UOMBuy C(3),cFabGrade C(1),nfabcost N(9,3),cPriceCur C(3),;
     nPriceRat N(9,4),nCurrUnit N(6),cDutyCur C(3),nDutyRat N(9,4),nDCurUnit N(6),;
     nItm_Frt N(9,3),nItem_Tax N(9,3), nItemQuota N(9,3), cWareCode C(6) )
     
  *C102819,1 AMH Add new fieldS for Delivry date, vendor item and vendor color [Start]
  *CREATE DBF (gcWorkDir+lcTmpRmain) ;
   ( Fabric C(7), Color C(6),Vendor C(8),nFabTotQty N(11,3),LeadTime N(3),;
     Complete D(8),UOMBuy C(3),cFabGrade C(1),nfabcost N(9,3),cPriceCur C(3),;
     nPriceRat N(9,4),nCurrUnit N(6),cDutyCur C(3),nDutyRat N(9,4),nDCurUnit N(6),;
     nItm_Frt N(9,3),nItem_Tax N(9,3), nItemQuota N(9,3), cWareCode C(6) )
  CREATE DBF (gcWorkDir+lcTmpRmain) ;
   ( Fabric C(7), Color C(6),Vendor C(8),nFabTotQty N(11,3),LeadTime N(3),;
     Complete D(8),UOMBuy C(3),cFabGrade C(1),nfabcost N(9,3),cPriceCur C(3),;
     nPriceRat N(9,4),nCurrUnit N(6),cDutyCur C(3),nDutyRat N(9,4),nDCurUnit N(6),;
     nItm_Frt N(9,3),nItem_Tax N(9,3), nItemQuota N(9,3), cWareCode C(6),;
     dDelivDate D,cVenFab C(10),cVenColr C(10))
  *C102819,1 AMH [End]
  
  *B603753,1 (End)
  *B603527,1 (End)
  INDEX ON Fabric + Color TAG (lcTmpRmain)
  SET ORDER TO 1 
ENDIF &&--endif if called from Requirement only do the next block of code .
*E301128,1 if called from Requirement only [End  ]

*--Collecting -ve remaining fabrics variables.
STORE '' TO lcUOMBuy
lnFabcost = 0

*C102819,1 AMH Add variable to get the use vendor referance setup [Start]
llVenRef   = (gfGetMemVar('M_VENREF  ')='Y')
IF llVenRef
  = gfOpenFile(gcDataDir+'VENDMATH','VENDMATH','SH')
  = gfOpenFile(gcDataDir+'VENDMATL' , 'MATCOL','SH')
ENDIF
*C102819,1 AMH [End]

DO WHILE .T.
  SELECT (lcMatReq)
  
  *C102741,1 AMH Sort by fabric location in case print by warehouse Yes [Start]
  IF lcRpReqBas = 'W'
    IF llRpByWare
      SET ORDER TO TAG MRFABLOC
    ELSE
      SET ORDER TO TAG MATREQ
    ENDIF
  ENDIF
  *C102741,1 AMH [End]

  *--Print reports.
  wait window 'Printing....' NOWAIT
  =lfPrintRep()

  *E301128,1 if Called from O.T.S. [Begin]
  IF llFromOts
    EXIT
  ENDIF
  *E301128,1 if Called from O.T.S. [End  ]

  *--Do you wish to reprint the report with edited projections.
  *--                        [ Yes / No ] 
  lnChoice=gfModalGen('TRM36109B36001','DIALOG')
  IF lnChoice = 1
    *--Edit Projection.
    =lfEditProj()


    *--Proceed with printing report ?
    *--         [ Yes / No ] 
    lnChoice=gfModalGen('TRM36110B36001','DIALOG')
    
  ENDIF

  IF lnChoice = 2
    
    *C102819,1 AMH Save the MRP# of the report [Start]
    IF llMrp .AND. !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
      =lfSaveMrp()
    ENDIF
    *C102819,1 AMH [End]
    
    *--Ceck if there is negative remainig in some materials.
    GO TOP IN (lcTmpRmain)
    IF !EOF(lcTmpRmain)
      *--Negative remaining quantity on some materials. Create P/Os ?
      *--                        [ Yes / No ] 
      lnChoice=gfModalGen('QRM36111B36001','DIALOG')
      IF lnChoice = 1
        
        *C102819,1 AMH Save the MRP# of the report [Start]
        IF llMrp .AND. !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
          =lfSaveMrp()
        ENDIF
        *C102819,1 AMH [End]
        
        wait window 'Generate P/Os...' NOWAIT
        IF lfGenMPo()
          RETURN
        ENDIF
      ENDIF
    ENDIF
    USE IN (lcTmpRmain)
    ERASE (gcWorkDir+lcTmpRmain+'.DBF')
    ERASE (gcWorkDir+lcTmpRmain+'.CDX')
    EXIT
  ENDIF
ENDDO

*--Update plane in style file.
*-- Do you wish to update the projection to the style master file.
*--                        [ Yes / No ] 
IF llUpdPlan AND gfModalGen('QRM36112B36001','DIALOG') = 1
  =lfUpdStyle()
ENDIF

*--Close all Open Files.
IF USED(lcMatReq)
  USE IN (lcMatReq)
ENDIF
ERASE (gcWorkDir+lcMatReq+'.DBF')
ERASE (gcWorkDir+lcMatReq+'.CDX')

IF laArOpnFl[1] AND USED('SCALE')
  USE IN SCALE
ENDIF

IF laArOpnFl[2] AND USED('BOMLINE')
  USE IN BOMLINE
ENDIF

IF laArOpnFl[3] AND USED('CTKTBOM')
  USE IN CTKTBOM
ENDIF

IF laArOpnFl[4] AND USED('POSHDR')
  USE IN POSHDR
ENDIF
IF laArOpnFl[5] AND USED('POSLN')
  USE IN POSLN
ENDIF
IF laArOpnFl[6] AND USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF
IF laArOpnFl[7] AND USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF

*C102741,1 AMH Close files needed for case of print by warehouse [Start]
IF laArOpnFl[8] AND USED('CODES')
  USE IN CODES
ENDIF
IF laArOpnFl[9] AND USED('MFGOPRHD')
  USE IN MFGOPRHD
ENDIF
*C102741,1 AMH [End]

*C200451,1 ABD - Add new triger for SHI to get the plan Qty From the Forcast table. [Begin]
= gfDoTriger('MAMATRQ',PADR('CLOSEFLS',10))
*C200451,1 ABD - [End]


RETURN
*--End...



*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*--Set needed orders in grid. 
SELECT STYDYE
SET ORDER TO TAG StyDye
SELECT STYLE
SET ORDER TO TAG Style
SELECT FABDYE
SET ORDER TO TAG FabDye
SELECT FABRIC
SET ORDER TO TAG Fabric
SELECT WAREHOUS
SET ORDER TO TAG WAREHOUS
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
SELECT ORDLINE
SET ORDER TO TAG ORDLINES DISENDING

*--Temp files names.
lcMatReq   = gfTempName()
lcTmpItems = gfTempName()

lcOldValue = ' '
*--Get the style major picture.
lcStyPict = '@! '+gfItemMask('PM')
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()
RETURN



*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfGetColor
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfvLocation
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate location entered in grid.
*!*************************************************************
*! Calls     : gfBrowWare()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvLocation()
*!*************************************************************
FUNCTION lfvLocation

lcLocation  = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcLocation = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcLocation) AND !SEEK(lcLocation,'WAREHOUS')
  lcLocation = gfBrowWare(.T.)
  &lcFldLocNam = IIF(!EMPTY(lcLocation),lcLocation,lcOldValue)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvStyMaj
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate style major in range entered in grid.
*!*************************************************************
*! Calls     : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStyMaj()
*!*************************************************************
FUNCTION lfvStyMaj
lcMStyle = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcMStyle = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcMStyle) AND !SEEK(lcMStyle,'STYLE')
  lcMStyle = gfStyBrw('M',"","",.F.)
  &lcFldLocNam = IIF(!EMPTY(lcMStyle),lcMStyle,lcOldValue)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvFabric
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate primary fabric and fabrics in range
*!             entered in grid.
*!*************************************************************
*! Calls     : FaBrow()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
lcFabric = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcFabric = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcFabric) AND !SEEK(lcFabric,'FABRIC')
  = FaBrow(@lcFabric,'*')
  &lcFldLocNam = IIF(!EMPTY(lcFabric),lcFabric,lcOldValue)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfUpdReq
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Update material requirement file.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfUpdReq()
*!*************************************************************
FUNCTION lfUpdReq

SELECT (lcMatReq)


*C102741,1 AMH Consider case of print by warehouse Yes [Start]
*IF !SEEK(BOM.cCatgTyp+lcItem+lcIClr+lcCStyle)
IF llRpByWare .AND. BOM.CCATGTYP $ 'FT'
  llFound = .F.
  SET ORDER TO VENCODE IN APVENDOR
  IF !EMPTY(BOM.COPRCODE) .AND. SEEK('NYMFGCODE','CODES')
    SELECT CODES
    LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYMFGCODE';
                  FOR CCODE_NO = BOM.COPRCODE .AND. CRLTD_NAM = 'CCONTCODE'
    IF FOUND() .AND. !EMPTY(CRLTD_VLU) .AND. SEEK(PADR(CRLTD_VLU,8),'APVENDOR') .AND. !EMPTY(APVENDOR.CWARECODE)
      llFound = .T.
    ENDIF
  ENDIF
  SELECT (lcMatReq)
ENDIF
IF !SEEK(BOM.cCatgTyp+IIF(llRpByWare .AND. BOM.CCATGTYP $ 'FT',;
         IIF(llByFabLoc,'N'+lcFLoctn,IIF(llFound,'N'+APVENDOR.CWARECODE,'Y'+SPACE(6))),'')+;
         lcItem+lcIClr+lcCStyle)
*C102741,1 AMH [End]

  APPEND BLANK
  REPLACE Style    WITH lcCStyle     ,;
          Typ      WITH BOM.Typ      ,;
          cCatgTyp WITH BOM.cCatgTyp ,;
          Item     WITH lcItem       ,;
          IClr     WITH lcIClr       ,;
          Desc     WITH BOM.Desc     ,;
          Uom      WITH BOM.Uom      ,;
          Cnt      WITH IIF(lnCnt=0,8,lnCnt),;
          lStyMake WITH STYLE.Make   ,;
          cWareCode WITH IIF(llByLoctn,lcLocation,"")
  
  *C102741,1 AMH Consider case of print by warehouse Yes [Start]
  IF llRpByWare .AND. CCATGTYP $ 'FT'
    IF llByFabLoc
        REPLACE CFABLOC  WITH lcFLoctn,;
                CINHOUSE WITH 'N'
    ELSE
      IF llFound
        REPLACE CFABLOC  WITH APVENDOR.CWARECODE,;
                CINHOUSE WITH 'N'
      ELSE
        REPLACE CINHOUSE WITH 'Y'
      ENDIF
    ENDIF
  ENDIF
  REPLACE LBOM WITH .T.
  *C102741,1 AMH [End]
  
ENDIF
*--Update the bom unit qty.
IF !EMPTY(BOM.mSizes)
  lcAvlSizes = ALLTRIM(SUBSTR(MLINE(BOM.mSizes,1),AT('~',MLINE(BOM.mSizes,1))+1))
ELSE
  *--All Sizes.
  lcAvlSizes = "1,2,3,4,5,6,7,8"
ENDIF
FOR I=1 TO lnCnt
  lcSz = STR(I,1)
  IF lcSz $ lcAvlSizes
    REPLACE Qty&lcSz WITH Qty&lcSz + BOM.nBOMTotQty
    
    *C102741,1 AMH Consider case of print by warehouse Yes [Start]
    REPLACE YIELD&lcSz WITH YIELD&lcSz + BOM.nBOMTotQty
    *C102741,1 AMH [End]
    
  ENDIF
ENDFOR
RETURN


*******************
FUNCTION lfReadItem

*--Get style component code.
IF cCatGTyp='S'
 *--If style size not used in cost sheet.
 IF !(STYLE.Scale $ BOM.MSIZES)
   RETURN .F.
 ENDIF
       
 *--Get an equevelent item non major part.
 lcCompNmj=''
 *--Non major length without scale segment.
 lnNmjPart = lnNMjrLn - IIF(llMScale,3,0)
 FOR lnI = 1 TO lnNmjPart
   lcCutChr  = SUBSTR(BOM.Item ,lnMajorLn+1+lnI ,1)
   lcCompNmj = lcCompNmj + ;
      IIF(lcCutChr='*',SUBSTR(STYLE.Style, lnMajorLn+1+lnI ,1),lcCutChr)
 ENDFOR
 lcItem = SUBSTR(Item,1,lnMajorLn+1)+lcCompNmj

 *--Get an equevelent item scale part.       
 IF llMScale
   lcEqSCSz = STYLE.Scale
   FOR lnMI = 1 TO MEMLINES(BOM.MSZCROSREF)
     lcMemSLine = MLINE(BOM.MSZCROSREF,lnMI)
     IF STYLE.Scale $ lcMemSLine
       lcEqSCSz = SUBSTR(lcMemSLine,AT('~',lcMemSLine)+1,3)
       EXIT
     ENDIF 
   ENDFOR
   lcItem = lcItem + lcEqSCSz
 ENDIF

 *--Check existance of style component.
 lnRcSv=IIF(!EOF('STYLE'),RECNO('STYLE'),0)
 SELECT STYLE
 lcStyOrder=ORDER('STYLE') 
 SET ORDER TO TAG Style
 llSComFund = SEEK(lcItem,'STYLE')
 IF lnRcSv<>0
   GOTO lnRcSv IN STYLE
 ENDIF
 SET ORDER TO &lcStyOrder
 SELECT BOM
 IF ! llSComFund
   RETURN .F.
 ENDIF   
 lcIClr = ''

ELSE  && CCATGTYP $ 'FT'
  lcItem = BOM.Item
  lcIClr = IIF(IClr='*',SUBSTR(lcCStyle,lnClrSrt,lnClrEnd),BOM.IClr)

ENDIF
RETURN .T.


*!*************************************************************
*! Name      : lfUpdItem
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Update items temp file.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfUpdItem()
*!*************************************************************
FUNCTION lfUpdItem

IF !SEEK(BOM.Typ+lcItem+lcIClr,lcTmpItems)
  SELECT (lcTmpItems)
  APPEND BLANK
  REPLACE Typ      WITH BOM.Typ      ,;
          cCatgTyp WITH BOM.cCatgTyp ,;
          Item     WITH lcItem       ,;
          IClr     WITH lcIClr
ENDIF                  
RETURN


*!*************************************************************
*! Name      : lfChkOrder
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get the ordered pieces.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : laOrders[]
*!*************************************************************
*! Example   : =lfChkOrder()
*!*************************************************************
FUNCTION lfChkOrder
PRIVATE lnAlias 

lnAlias = SELECT()
SELECT ORDLINE
SEEK lcCStyle
SCAN WHILE Style=lcCStyle FOR &lcForWare
  =SEEK('O'+ORDLINE.Order,'ORDHDR')
  *--Check orders filter.
  IF ! &lcRpExp1
    LOOP
  ENDIF
  IF OrdHdr.Status $ 'OH'
    FOR I=1 TO lnCnt      
      Z=STR(I,1)
      IF OrdHdr.Status = 'O'
        laOrders[1,I]=laOrders[1,I]+ORDLINE.Qty&Z  
      ENDIF
      IF OrdHdr.Status $ 'OH'
        laOrders[2,I]=laOrders[2,I]+ORDLINE.Qty&Z  
      ENDIF
    ENDFOR
    laOrders[1,9]=laOrders[1,1]+laOrders[1,2]+laOrders[1,3]+laOrders[1,4]+laOrders[1,5]+laOrders[1,6]+laOrders[1,7]+laOrders[1,8]
    laOrders[2,9]=laOrders[2,1]+laOrders[2,2]+laOrders[2,3]+laOrders[2,4]+laOrders[2,5]+laOrders[2,6]+laOrders[2,7]+laOrders[2,8]
    IF &lcChkFile..TotOrd=laOrders[2,9]
      EXIT
    ENDIF
  ENDIF
ENDSCAN
SELECT ORDLINE
SET RELATION TO
SELECT (lnAlias)
RETURN


*!*************************************************************
*! Name      : lfChk_CT
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get WIP pieces from cuttkts.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : laWIP[]
*!*************************************************************
*! Example   : =lfChk_CT()
*!*************************************************************
FUNCTION lfChk_CT
PRIVATE lnAlias

lnAlias = SELECT()
SELECT CUTTKTL
SEEK lcCStyle
SCAN WHILE Style=lcCStyle FOR CUTTKTH.Status $ 'AOH' AND &lcForWare
  IF CUTTKTH.STATUS<>'H' AND !SEEK('M'+'1'+CUTTKTH.Cuttkt+STR(CUTTKTL.lineno,6)+&lcMatReq..Typ+&lcMatReq..style+SPACE(6)+&lcMatReq..item+&lcMatReq..iclr,'BOMLINE')
    LOOP
  ENDIF
  FOR I=1 TO lnCnt      
    Z=STR(I,1)
    DO CASE
      CASE CUTTKTH.Status = 'A'
        laWIP[1,I]=IIF(TranCd = '1', (laWIP[1,I] + MAX(Qty&Z,0)) ,;
                          (MAX(laWIP[1,I]-ABS(Qty&Z),0) ) )
        *B605049,1 WAB (Start) move the calucation of the total after the scan 
        *laWIP[1,9]=laWIP[1,9]+laWIP[1,I]
        *B605049,1 WAB (End)
      
      CASE CUTTKTH.Status = 'O'      
        *B605049,1 WAB (Start) move the calucation of the total after the scan and change the calculating
        *B605049,1             of the WIP to be the budjet qty for GMA only         
        *laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] + MAX(Qty&Z,0)) ,;
                          (MAX(laWIP[2,I]-ABS(Qty&Z),0) ) )
        *laWIP[2,9]=laWIP[2,9]+laWIP[2,I]

        IF gfDoTriger('MAMATRQ',PADR('CLCWIPGMA',10))
          laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] + MAX(Qty&Z,0)) , laWIP[2,I])
        ELSE
          laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] + MAX(Qty&Z,0)) ,;
                          (MAX(laWIP[2,I]-ABS(Qty&Z),0) ) )
        ENDIF
        *B605049,1 WAB (END) 
        
      CASE CUTTKTH.Status = 'H'
        laWIP[3,I]=IIF(TranCd = '1', (laWIP[3,I] + MAX(Qty&Z,0)) ,;
                          (MAX(laWIP[3,I]-ABS(Qty&Z),0) ) )
        *B605049,1 WAB (Start) move the calucation of the total after the scan        
        *laWIP[3,9]=laWIP[3,9]+laWIP[3,I]
        *B605049,1 WAB (End) 
    ENDCASE
  ENDFOR
  IF &lcChkFile..TotWip=laWIP[1,9]+laWIP[2,9]+laWIP[3,9]
    EXIT
  ENDIF
ENDSCAN
*B605049,1 WAB (Start) move the calucation of the total after the scan 
FOR I=1 TO lnCnt      
  laWIP[1,9]=laWIP[1,9]+laWIP[1,I]
  laWIP[2,9]=laWIP[2,9]+laWIP[2,I]
  laWIP[3,9]=laWIP[3,9]+laWIP[3,I]
ENDFOR
*B605049,1 WAB (END) 

SELECT (lnAlias)
RETURN


*!*************************************************************
*! Name      : lfChk_PO
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get WIP pieces from P/Os.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : laWIP[]
*!*************************************************************
*! Example   : =lfChk_PO()
*!*************************************************************
FUNCTION lfChk_PO
PRIVATE lnAlias 

lnAlias = SELECT()
SELECT POSLN
SEEK lcCStyle

*B606818,1 AMH Add the status A to the for condition [Start]
*SCAN WHILE Style=lcCStyle FOR cStyType $ 'PRN' AND POSHDR.Status $ 'OH' ;
           AND !(TranCd $ '36') AND &lcForWare
SCAN WHILE Style=lcCStyle FOR cStyType $ 'PRN' AND POSHDR.Status $ 'OHA' ;
           AND !(TranCd $ '36') AND &lcForWare
*B606818,1 AMH [End]

  *B603527,1 (Start) Seek for "I" instead of "M" because in this case the style is imported not manufactured
  *IF POSHDR.STATUS<>'H' AND !SEEK('M'+'1'+POSHDR.PO+STR(POSLN.lineno,6)+&lcMatReq..Typ+&lcMatReq..style+SPACE(6)+&lcMatReq..item+&lcMatReq..iclr,'BOMLINE')
  IF POSHDR.STATUS<>'H' AND !SEEK('I'+'1'+POSHDR.PO+STR(POSLN.lineno,6)+&lcMatReq..Typ+&lcMatReq..style+SPACE(6)+&lcMatReq..item+&lcMatReq..iclr,'BOMLINE')
  *B603527,1  (End) 
    LOOP
  ENDIF

  lcSign  = IIF(cStyType='R','-','+')
  lcOSign = IIF(cStyType='R','+','-')
  FOR I=1 TO lnCnt
    Z=STR(I,1)
    DO CASE
    
      *B606818,1 AMH Add case of status A [Start]
      CASE POSHDR.STATUS = 'A'
        laWIP[1,I]=IIF(TranCd = '1', (laWIP[1,I] &lcSign MAX(Qty&Z,0)) ,;
                           (MAX(laWIP[1,I] &lcOSign ABS(Qty&Z),0) ) )
      *B606818,1 AMH [End]
      
      CASE POSHDR.Status = 'O'      
        *B605049,1 WAB (Start) move the calucation of the total after the scan and change the calculating
        *B605049,1             of the WIP to be the budjet qty for GMA only         
        *laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] &lcSign MAX(Qty&Z,0)) ,;
                          (MAX(laWIP[2,I] &lcOSign ABS(Qty&Z),0) ) )
        *laWIP[2,9]=laWIP[2,9]+laWIP[2,I]

        IF gfDoTriger('MAMATRQ',PADR('CLCWIPGMA',10))
          laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] &lcSign MAX(Qty&Z,0)) , laWIP[2,I])
        ELSE
          laWIP[2,I]=IIF(TranCd = '1', (laWIP[2,I] &lcSign MAX(Qty&Z,0)) ,;
                           (MAX(laWIP[2,I] &lcOSign ABS(Qty&Z),0) ) )
        ENDIF
        *B605049,1 WAB (END) 
      CASE POSHDR.Status = 'H'
        laWIP[3,I]=IIF(TranCd = '1', (laWIP[3,I] &lcSign MAX(Qty&Z,0)) ,;
                          (MAX(laWIP[3,I] &lcOSign ABS(Qty&Z),0) ) )
        *B605049,1 WAB (Start) move the calucation of the total after the scan 
        *laWIP[3,9]=laWIP[3,9]+laWIP[3,I]
        *B605049,1 WAB (End) 
    ENDCASE
  ENDFOR
ENDSCAN
*B605049,1 WAB (Start) move the calucation of the total after the scan 
FOR I=1 TO lnCnt      
  
  *B606818,1 AMH Add case of status A [Start]
  laWIP[1,9]=laWIP[1,9]+laWIP[1,I]
  *B606818,1 AMH [End]
  
  laWIP[2,9]=laWIP[2,9]+laWIP[2,I]
  laWIP[3,9]=laWIP[3,9]+laWIP[3,I]
ENDFOR
*B605049,1 WAB (End) 

SELECT (lnAlias)
RETURN


*!*************************************************************
*! Name      : lfEditProj
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Edit Projection.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Call               : ARIABROW() , lfvEdtPrj()
*!*************************************************************
*! Example   : =lfEditProj()
*!*************************************************************
FUNCTION lfEditProj

lcBrFields = "Style    :R :H=lcStyHdr    :30,"+;
	         "lcDesc = STYLE.Desc1  :R :H='Description' :41,"+;
	         "OReqTot  :R :H='Open'      :12,"+;
	         "OHReqTot :R :H='Open+Hold' :12,"+;
	         "Project  :R :H='Projected' :12 "
SELECT STYLE
SET ORDER TO TAG STYLE
SELECT (lcMatReq)
SET ORDER TO TAG MRStyle
SET RELA TO STYLE INTO STYLE
GO TOP
=ARIABROW('','Edit projection',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'lfvEdtPrj',;
             'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok' )
SELECT STYLE
SET RELATION TO
RETURN


*!*************************************************************
*! Name      : lfvEdtPrj
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Modify Projection, Valid of edit projection.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvEdtPrj()
*!*************************************************************
FUNCTION lfvEdtPrj
*B606175,1 KHM 06/26/2002 (Begin) Initialize the following variables.
PRIVATE lnCurrRec, lcCurrSty
*B606175,1 KHM 06/26/2002 (End)

llOk = .F.
SCATTER FIELDS nProj1,nProj2,nProj3,nProj4,nProj5,nProj6,nProj7,nProj8,Project TO laPrjn
DO (gcRepHome+"MA\MAEDTPJ.SPX")
IF llOk

  *B606175,1 KHM 06/26/2002 (Begin) Aply the modified projected figures to all the
  *B606175,1                cost items for the edited style.
  lnCurrRec = RECNO()
  lcCurrSty = STYLE
  SET ORDER TO TAG MrStyItm
  SEEK lcCurrSty
  SCAN REST WHILE Style+cCatgTyp+Item+IClr = lcCurrSty
  *B606175,1 KHM 06/26/2002 (End)
  
    GATHER FROM laPrjn FIELDS nProj1,nProj2,nProj3,nProj4,nProj5,nProj6,nProj7,nProj8,Project

  *B606175,1 KHM 06/26/2002 (Begin) Restore the old tag and re-positioning the record pointer.
  ENDSCAN  
  SET ORDER TO TAG MRStyle
  IF BETWEEN(lnCurrRec,1,RECCOUNT())
    GOTO lnCurrRec
  ENDIF
  *B606175,1 KHM 06/26/2002 (End)
  
  llUpdPlan = .T.
ENDIF
RETURN


*!*************************************************************
*! Name      : lfPrintRep
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Start printing.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfPrintRep()
*!*************************************************************
*!Modifications :
*E301128,1 if Called from O.T.S.
*!*************************************************************
*
FUNCTION lfPrintRep
*--Clear -ve remain file from previous items Printed.
*SELECT (lcTmpRmain)
*ZAP

*E301128,1 if called from requirement report. [Begin]

*C102819,1 AMH consider case of Detail line layout [Start]
*IF !llFromOts
IF !llFromOts .AND. (lcRpRName # 'MAMATRQD')
*C102819,1 AMH [End]

  *--Clear -ve remain file from previous items Printed.
  SELECT (lcTmpRmain)
  ZAP
ENDIF  
*E301128,1 if called from requirement report. [End  ]

SELECT (lcMatReq)
SET ORDER TO TAG MATREQ

*C102819,1 AMH consider case of Detail line layout [Start]
IF lcRpRName = 'MAMATRQD'
  SET ORDER TO TAG MATREQ
ENDIF
*C102819,1 AMH [End]

*C102741,1 AMH Sort by fabric location in case print by warehouse Yes [Start]
IF llRpByWare
  SET ORDER TO TAG MRFABLOC

  *C102819,1 AMH consider case of Detail line layout [Start]
  IF lcRpRName = 'MAMATRQD'
    SET ORDER TO TAG MRFABLOCDT
  ENDIF
  *C102819,1 AMH [End]
  
  SET RELATION TO CFABLOC INTO WAREHOUS ADDITIVE
ENDIF
*C102741,1 AMH [End]

*--Reset report calculated fields.
*B602429,1 Add new fields nYTOWIP1,nNetReq1 to reseting.
*REPLACE ALL nYTOWIP  WITH 0,;
             nNetReq  WITH 0,;
             nUsedReq WITH 0

*C102819,1 AMH consider case of Detail line layout [Start]
*REPLACE ALL nYTOWIP  WITH 0,;
            nYTOWIP1 WITH 0,;
            nYTOWIP2 WITH 0,;
            nYTOWIP3 WITH 0,;
            nYTOWIP4 WITH 0,;
            nYTOWIP5 WITH 0,;
            nYTOWIP6 WITH 0,;
            nYTOWIP7 WITH 0,;
            nYTOWIP8 WITH 0,;
            nNetReq  WITH 0,;
            nNetReq1 WITH 0,;
            nNetReq2 WITH 0,;
            nNetReq3 WITH 0,;
            nNetReq4 WITH 0,;
            nNetReq5 WITH 0,;
            nNetReq6 WITH 0,;
            nNetReq7 WITH 0,;
            nNetReq8 WITH 0,;
            nUsedReq WITH 0
IF lcRpRName <> 'MAMATRQD'
  REPLACE ALL nYTOWIP  WITH 0,;
              nYTOWIP1 WITH 0,;
              nYTOWIP2 WITH 0,;
              nYTOWIP3 WITH 0,;
              nYTOWIP4 WITH 0,;
              nYTOWIP5 WITH 0,;
              nYTOWIP6 WITH 0,;
              nYTOWIP7 WITH 0,;
              nYTOWIP8 WITH 0,;
              nNetReq  WITH 0,;
              nNetReq1 WITH 0,;
              nNetReq2 WITH 0,;
              nNetReq3 WITH 0,;
              nNetReq4 WITH 0,;
              nNetReq5 WITH 0,;
              nNetReq6 WITH 0,;
              nNetReq7 WITH 0,;
              nNetReq8 WITH 0,;
              nUsedReq WITH 0
ENDIF
*C102819,1 AMH [End]

*C102741,1 AMH Empty the POCT field [Start]
REPLACE ALL POCT WITH ''
*C102741,1 AMH [End]

*B602429,1 End.

*--Initialize printing report variables.
STORE 0   TO lnOnHand,lnOnOrdr,lnConv,lnLeadTm,lnYield,lnYield1,lnYield2,;
             lnAvl1,lnAvl2,lnAvl3,lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8
STORE " " TO lcDescrp,lcFabVen,lcOldItem,lcSDescrp,lcTkTRUn,;
             lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8

*B608607,1 tmi [Start] update the ..REMAIN fields in the lcMatReq temp file
SELECT (lcMatReq)
LOCATE
SCAN

  lnOReq  = lfvReq('O')
  lnHReq  = lfvReq('H')
  lnPReq  = lfvReq('P')    
  lnYTWip = lfvReq('S')

  lnNetReq = EVAL(lcMatReq+'.nNetReq')

  =lfGetInfo()
      
  REPLACE nOREMAIN WITH lfRemain('O') ;
          nHREMAIN WITH lfRemain('H') ;
          nPREMAIN WITH lfRemain('P')
  
ENDSCAN
LOCATE
*B608607,1 tmi [End  ] 

*--[1] Fabrics requirement report.
SET KEY TO 'F'

IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  SET ORDER TO MatreqC ADDITIVE
ENDIF

IF SEEK('F')
  DO gfDispRe WITH EVAL('lcRpRname')
ENDIF

*--[2] Trims requirement report.
SELECT (lcMatReq)
SET KEY TO 'T'
IF SEEK('T')
  *--Initialize printing variables
  =lfInitVar()
  DO gfDispRe WITH EVAL('lcRpRname')
ENDIF

*C102741,1 AMH Clear relation with warehous file in case print by warehouse Yes [Start]
IF llRpByWare
  SET RELATION OFF INTO WAREHOUS
ENDIF
*C102741,1 AMH [End]

*--[3] Style components requirement report.
SELECT (lcMatReq)
SET KEY TO 'S'
IF SEEK('S')
  *--Initialize printing variables
  =lfInitVar()
  DO gfDispRe WITH EVAL('lcRpRname')
ENDIF
SET KEY TO

RETURN

*!*************************************************************
*! Name      : lfInitVar
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to initialize report calculated varbls.
*!*************************************************************
FUNCTION lfInitVar
STORE 0   TO lnOnHand,lnOnOrdr,lnConv,lnLeadTm,lnYield,lnYield1,lnYield2,;
             lnAvl1,lnAvl2,lnAvl3,lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8
STORE " " TO lcDescrp,lcFabVen,lcOldItem,lcSDescrp,lcTkTRUn,;
             lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8
RETURN

*!*************************************************************
*! Name      : lfCreateFl
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Create the report temp file.
*!*************************************************************
FUNCTION lfCreateFl

*B602429,1 Add A new fields NOPNWIP1,nYTOWIP1,nNetReq1.
*DIMENSION laFlSruc[49,4]
*B605049,1 WAB (Start) - add a memo filed to hold the CT/Po that already calculate their issued qty
*DIMENSION laFlSruc[73,4]

*C102741,1 AMH Add New fields to save the fabric location [Start]
*DIMENSION laFlSruc[74,4]

*C102819,1 AMH Add new field to save memory variables [Start]
*DIMENSION laFlSruc[86,4]
DIMENSION laFlSruc[95,4]
*C102819,1 AMH [End]

*C102741,1 AMH [End]

*B605049,1 WAB (END) 
*B602429,1 End.

*--First array element [Name].
laFlSruc[1,1] = "STYLE"
laFlSruc[2,1] = "ITEM"
laFlSruc[3,1] = "ICLR"
laFlSruc[4,1] = "DESC"
laFlSruc[5,1] = "CWARECODE"
laFlSruc[6,1] = "TYP"
laFlSruc[7,1] = "CCATGTYP"
laFlSruc[8,1] = "CNT"
laFlSruc[9,1] = "UOM"
laFlSruc[10,1]= "QTY1"
laFlSruc[11,1]= "QTY2"
laFlSruc[12,1]= "QTY3"
laFlSruc[13,1]= "QTY4"
laFlSruc[14,1]= "QTY5"
laFlSruc[15,1]= "QTY6"
laFlSruc[16,1]= "QTY7"
laFlSruc[17,1]= "QTY8"
laFlSruc[18,1]= "OREQ1"
laFlSruc[19,1]= "OREQ2"
laFlSruc[20,1]= "OREQ3"
laFlSruc[21,1]= "OREQ4"
laFlSruc[22,1]= "OREQ5"
laFlSruc[23,1]= "OREQ6"
laFlSruc[24,1]= "OREQ7"
laFlSruc[25,1]= "OREQ8"
laFlSruc[26,1]= "OREQTOT"
laFlSruc[27,1]= "OHREQ1"
laFlSruc[28,1]= "OHREQ2"
laFlSruc[29,1]= "OHREQ3"
laFlSruc[30,1]= "OHREQ4"
laFlSruc[31,1]= "OHREQ5"
laFlSruc[32,1]= "OHREQ6"
laFlSruc[33,1]= "OHREQ7"
laFlSruc[34,1]= "OHREQ8"
laFlSruc[35,1]= "OHREQTOT"
laFlSruc[36,1]= "NPROJ1"
laFlSruc[37,1]= "NPROJ2"
laFlSruc[38,1]= "NPROJ3"
laFlSruc[39,1]= "NPROJ4"
laFlSruc[40,1]= "NPROJ5"
laFlSruc[41,1]= "NPROJ6"
laFlSruc[42,1]= "NPROJ7"
laFlSruc[43,1]= "NPROJ8"
laFlSruc[44,1]= "PROJECT"
laFlSruc[45,1]= "NOPNWIP"
laFlSruc[46,1]= "NYTOWIP"
laFlSruc[47,1]= "NUSEDREQ"
laFlSruc[48,1]= "NNETREQ"
laFlSruc[49,1]= "LSTYMAKE"
*B602429,1 Add a new fields NOPNWIP1,nYTOWIP1,nNetReq1.
laFlSruc[50,1]= "NNETREQ1"
laFlSruc[51,1]= "NNETREQ2"
laFlSruc[52,1]= "NNETREQ3"
laFlSruc[53,1]= "NNETREQ4"
laFlSruc[54,1]= "NNETREQ5"
laFlSruc[55,1]= "NNETREQ6"
laFlSruc[56,1]= "NNETREQ7"
laFlSruc[57,1]= "NNETREQ8"
laFlSruc[58,1]= "NOPNWIP1"
laFlSruc[59,1]= "NOPNWIP2"
laFlSruc[60,1]= "NOPNWIP3"
laFlSruc[61,1]= "NOPNWIP4"
laFlSruc[62,1]= "NOPNWIP5"
laFlSruc[63,1]= "NOPNWIP6"
laFlSruc[64,1]= "NOPNWIP7"
laFlSruc[65,1]= "NOPNWIP8"
laFlSruc[66,1]= "NYTOWIP1"
laFlSruc[67,1]= "NYTOWIP2"
laFlSruc[68,1]= "NYTOWIP3"
laFlSruc[69,1]= "NYTOWIP4"
laFlSruc[70,1]= "NYTOWIP5"
laFlSruc[71,1]= "NYTOWIP6"
laFlSruc[72,1]= "NYTOWIP7"
laFlSruc[73,1]= "NYTOWIP8"
*B602429,1 End.


*--Second array element [Type].
STORE "C" TO laFlSruc[1,2] ,laFlSruc[2,2] ,laFlSruc[3,2] ,laFlSruc[4,2],;
             laFlSruc[5,2] ,laFlSruc[6,2] ,laFlSruc[7,2] ,laFlSruc[9,2]
STORE "N" TO laFlSruc[8,2] ,laFlSruc[10,2],laFlSruc[11,2],laFlSruc[12,2],;
             laFlSruc[13,2],laFlSruc[14,2],laFlSruc[15,2],laFlSruc[16,2],;
             laFlSruc[17,2],laFlSruc[18,2],laFlSruc[19,2],laFlSruc[20,2],;
             laFlSruc[21,2],laFlSruc[22,2],laFlSruc[23,2],laFlSruc[24,2],;
             laFlSruc[25,2],laFlSruc[26,2],laFlSruc[27,2],laFlSruc[28,2],;
             laFlSruc[29,2],laFlSruc[30,2],laFlSruc[31,2],laFlSruc[32,2],;
             laFlSruc[33,2],laFlSruc[34,2],laFlSruc[35,2],laFlSruc[36,2],;
             laFlSruc[37,2],laFlSruc[38,2],laFlSruc[39,2],laFlSruc[40,2],;             
             laFlSruc[41,2],laFlSruc[42,2],laFlSruc[43,2],laFlSruc[44,2],;
             laFlSruc[45,2],laFlSruc[46,2],laFlSruc[47,2],laFlSruc[48,2]
STORE "L" TO laFlSruc[49,2]
*B602429,1 Start.
STORE "N" TO laFlSruc[50,2],laFlSruc[51,2],laFlSruc[52,2],laFlSruc[53,2],;
             laFlSruc[54,2],laFlSruc[55,2],laFlSruc[56,2],laFlSruc[57,2],;
             laFlSruc[58,2],laFlSruc[59,2],laFlSruc[60,2],laFlSruc[61,2],;
             laFlSruc[62,2],laFlSruc[63,2],laFlSruc[64,2],laFlSruc[65,2],;
             laFlSruc[66,2],laFlSruc[67,2],laFlSruc[68,2],laFlSruc[69,2],;
             laFlSruc[70,2],laFlSruc[71,2],laFlSruc[72,2],laFlSruc[73,2]
*B602429,1 End.

*--Thered array element [Length].
STORE  1  TO laFlSruc[6,3] ,laFlSruc[7,3] ,laFlSruc[8,3],laFlSruc[49,3]
STORE  3  TO laFlSruc[9,3]
STORE  6  TO laFlSruc[3,3] ,laFlSruc[5,3] ,laFlSruc[18,3],laFlSruc[19,3],;
             laFlSruc[20,3],laFlSruc[21,3],laFlSruc[22,3],laFlSruc[23,3],;
             laFlSruc[24,3],laFlSruc[25,3],laFlSruc[27,3],laFlSruc[28,3],;
             laFlSruc[29,3],laFlSruc[30,3],laFlSruc[31,3],laFlSruc[32,3],;
             laFlSruc[33,3],laFlSruc[34,3],laFlSruc[36,3],laFlSruc[37,3],;
             laFlSruc[38,3],laFlSruc[39,3],laFlSruc[40,3],laFlSruc[41,3],;
             laFlSruc[42,3],laFlSruc[43,3]
STORE  7  TO laFlSruc[10,3],laFlSruc[11,3],laFlSruc[12,3],laFlSruc[13,3],;
             laFlSruc[14,3],laFlSruc[15,3],laFlSruc[16,3],laFlSruc[17,3],;
             laFlSruc[26,3],laFlSruc[35,3],laFlSruc[44,3],laFlSruc[45,3]
STORE 19  TO laFlSruc[1,3] ,laFlSruc[2,3]
STORE 12  TO laFlSruc[46,3],laFlSruc[47,3],laFlSruc[48,3]
STORE 20  TO laFlSruc[4,3]
*B602429,1 Start.
STORE 12  TO laFlSruc[50,3],laFlSruc[51,3],laFlSruc[52,3],laFlSruc[53,3],;
             laFlSruc[54,3],laFlSruc[55,3],laFlSruc[56,3],laFlSruc[57,3]
STORE  7  TO laFlSruc[58,3],laFlSruc[59,3],laFlSruc[60,3],laFlSruc[61,3],;
             laFlSruc[62,3],laFlSruc[63,3],laFlSruc[64,3],laFlSruc[65,3],;
             laFlSruc[66,3],laFlSruc[67,3],laFlSruc[68,3],laFlSruc[69,3],;
             laFlSruc[70,3],laFlSruc[71,3],laFlSruc[72,3],laFlSruc[73,3]
*B602429,1 End.

*--Forth array element [Decemal].
STORE  0  TO laFlSruc[1,4] ,laFlSruc[2,4] ,laFlSruc[3,4] ,laFlSruc[4,4], ;
             laFlSruc[5,4] ,laFlSruc[6,4] ,laFlSruc[7,4] ,laFlSruc[8,4], ;
             laFlSruc[9,4] ,laFlSruc[18,4],laFlSruc[19,4],laFlSruc[20,4],;
             laFlSruc[21,4],laFlSruc[22,4],laFlSruc[23,4],laFlSruc[24,4],;
             laFlSruc[25,4],laFlSruc[26,4],laFlSruc[27,4],laFlSruc[28,4],;
             laFlSruc[29,4],laFlSruc[30,4],laFlSruc[31,4],laFlSruc[32,4],;
             laFlSruc[33,4],laFlSruc[34,4],laFlSruc[35,4],laFlSruc[36,4],;
             laFlSruc[37,4],laFlSruc[38,4],laFlSruc[39,4],laFlSruc[40,4],;
             laFlSruc[41,4],laFlSruc[42,4],laFlSruc[43,4],laFlSruc[44,4],;
             laFlSruc[45,4],laFlSruc[49,4]
STORE  3  TO laFlSruc[10,4],laFlSruc[11,4],laFlSruc[12,4],laFlSruc[13,4],;
             laFlSruc[14,4],laFlSruc[15,4],laFlSruc[16,4],laFlSruc[17,4],;
             laFlSruc[46,4],laFlSruc[47,4],laFlSruc[48,4]
*B602429,1 Start.
STORE  0  TO laFlSruc[58,4],laFlSruc[59,4],laFlSruc[60,4],laFlSruc[61,4],;
             laFlSruc[62,4],laFlSruc[63,4],laFlSruc[64,4],laFlSruc[65,4]
STORE  3  TO laFlSruc[50,4],laFlSruc[51,4],laFlSruc[52,4],laFlSruc[53,4],;
             laFlSruc[54,4],laFlSruc[55,4],laFlSruc[56,4],laFlSruc[57,4],;
             laFlSruc[66,4],laFlSruc[67,4],laFlSruc[68,4],laFlSruc[69,4],;
             laFlSruc[70,4],laFlSruc[71,4],laFlSruc[72,4],laFlSruc[73,4]
*B602429,1 End.             

*B605049,1 WAB (Start) - add a memo filed to hold the CT/Po that already calculate their issued qty
laFlSruc[74,1] = "PoCt"
laFlSruc[74,2] = "M"
laFlSruc[74,3] = "10"
laFlSruc[74,4] = "0"
*B605049,1 WAB (END)

*C102741,1 AMH Add New fieldS to save the fabric location [Start]
laFlSruc[75,1] = "CFABLOC"
laFlSruc[75,2] = "C"
laFlSruc[75,3] = "6"
laFlSruc[75,4] = "0"
laFlSruc[76,1] = "CINHOUSE"
laFlSruc[76,2] = "C"
laFlSruc[76,3] = "1"
laFlSruc[76,4] = "0"
laFlSruc[77,1] = "LGETTHIS"
laFlSruc[77,2] = "L"
laFlSruc[77,3] = "1"
laFlSruc[77,4] = "0"
laFlSruc[78,1] = "LBOM"
laFlSruc[78,2] = "L"
laFlSruc[78,3] = "1"
laFlSruc[78,4] = "0"
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  laFlSruc[78+lnI,1] = 'YIELD'+lcI
  laFlSruc[78+lnI,2] = 'N'
  laFlSruc[78+lnI,3] = 7
  laFlSruc[78+lnI,4] = 3
ENDFOR
*C102741,1 AMH [End]

*C102819,1 AMH Add new field to save memory variables [Start]
laFlSruc[87,1] = "MFltVal"
laFlSruc[87,2] = "M"
laFlSruc[87,3] = "10"
laFlSruc[87,4] = "0"
laFlSruc[88,1] = "ORDER"
laFlSruc[88,2] = "C"
laFlSruc[88,3] = "6"
laFlSruc[88,4] = "0"
laFlSruc[89,1] = "COMPLETE"
laFlSruc[89,2] = "D"
laFlSruc[89,3] = "8"
laFlSruc[89,4] = "0"
laFlSruc[90,1] = "ONHAND"
laFlSruc[90,2] = "N"
laFlSruc[90,3] = "12"
laFlSruc[90,4] = "3"
laFlSruc[91,1] = "ONORDER"
laFlSruc[91,2] = "N"
laFlSruc[91,3] = "12"
laFlSruc[91,4] = "3"
laFlSruc[92,1] = "ETA"
laFlSruc[92,2] = "D"
laFlSruc[92,3] = "8"
laFlSruc[92,4] = "0"
laFlSruc[93,1] = "LMULTIPO"
laFlSruc[93,2] = "L"
laFlSruc[93,3] = "1"
laFlSruc[93,4] = "0"
laFlSruc[94,1] = "LNOTPRINT"
laFlSruc[94,2] = "L"
laFlSruc[94,3] = "1"
laFlSruc[94,4] = "0"
laFlSruc[95,1] = "ITEMSTK"
laFlSruc[95,2] = "N"
laFlSruc[95,3] = "12"
laFlSruc[95,4] = "3"
*C102819,1 AMH [End]

*B608607,1 TMI [Start] Add the fields NOREMAIN,NHREMAIN and NPREMAIN to hold the remain quantities
lnLn = ALEN(laFlSruc,1)

lnLn = lnLn + 1
DIMENSION laFlSruc[lnLn,4]
laFlSruc[lnLn,1] = "NOREMAIN"
laFlSruc[lnLn,2] = "N"
laFlSruc[lnLn,3] = "12"
laFlSruc[lnLn,4] = "3"

lnLn = lnLn + 1
DIMENSION laFlSruc[lnLn,4]
laFlSruc[lnLn,1] = "NHREMAIN"
laFlSruc[lnLn,2] = "N"
laFlSruc[lnLn,3] = "12"
laFlSruc[lnLn,4] = "3"

lnLn = lnLn + 1
DIMENSION laFlSruc[lnLn,4]
laFlSruc[lnLn,1] = "NPREMAIN"
laFlSruc[lnLn,2] = "N"
laFlSruc[lnLn,3] = "12"
laFlSruc[lnLn,4] = "3"
*B608607,1 TMI [End  ] 

CREATE DBF (gcWorkDir+lcMatReq) FROM ARRAY laFlSruc
RETURN


*!*************************************************************
*! Name      : lfUpdStyle
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Update style plane with projection.
*!*************************************************************
FUNCTION lfUpdStyle

WAIT WINDOW 'Updating projections to the style master file ...' NOWAIT
SELECT (lcMatReq)
SET ORDER TO TAG MRStyItm
SCAN
  SELECT STYLE
  =SEEK(&lcMatReq..Style) 
  =RLOCK()
  REPLACE PLAN1   WITH &lcMatReq..nProj1,;
          PLAN2   WITH &lcMatReq..nProj2,;
          PLAN3   WITH &lcMatReq..nProj3,;
          PLAN4   WITH &lcMatReq..nProj4,;
          PLAN5   WITH &lcMatReq..nProj5,;
          PLAN6   WITH &lcMatReq..nProj6,;
          PLAN7   WITH &lcMatReq..nProj7,;
          PLAN8   WITH &lcMatReq..nProj8,;
          TOTPLAN WITH &lcMatReq..Project
  UNLOCK
ENDSCAN
SELECT (lcMatReq)
SET ORDER TO TAG MATREQ
WAIT CLEAR
RETURN





****************************

*** FRX FUNCTIONS

****************************

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get Fabric,Trim or Style comp information like
*!             description,onhand and onorder.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo

*C102819,1 AMH Restore the memory variables in case of MRP existing [Start]
IF !llFromOts .AND. lcRpMrpBas = 'E'
  RESTORE FROM MEMO MFLTVAL ADDITIVE
  FOR lnI = 1 TO 20
    &laSaveVar[lnI,1]. = laSaveVar[lnI,2]
  ENDFOR
  RETURN ''
ENDIF
*C102819,1 AMH [End]

lcSekFile = IIF(cCatgTyp = 'S' , 'STYLE' , 'FABRIC' )
lcSekExpr = IIF(cCatgTyp = 'S' , Item , PADR(Item,7)+IClr )
IF SEEK(lcSekExpr,lcSekFile)
  IF cCatgTyp = 'S'
    lcDescrp = STYLE.Desc1
    lnOnHand = STYLE.TotStk
    lnOnOrdr = STYLE.TotWip
    FOR lnI=1 TO 8
      lcZ=STR(lnI,1)
      lnAvl&lcZ = ( STYLE.Stk&lcZ + STYLE.WIP&lcZ)
    ENDFOR

  ELSE
  

    *C200096,1 [Start] Define a variable to hold Fabric description
    lcFDesc  = FABRIC.Desc
    lcClrDsc = gfCodDes(FABRIC.Color,'COLOR     ')
    *C200096,1 [End]
  
    lnOnHand = FABRIC.OnHand
    lnOnOrdr = FABRIC.OnOrder
    lcFabVen = FABRIC.Vendor
    lnConv   = FABRIC.Conv
    lnLeadTm = FABRIC.LeadTime
    lnFabcost= FABRIC.nFabcost
    lcUOMBuy = FABRIC.UOMBUY
    *B603605,1 SSH 01/05/2000 Print the fabric openqty by yards for Mexx
    lnfUOM   = FABRIC.Conv
    *B603605,1 SSH 01/05/2000 [End]
    lcDescrp = gfCodDes(FABRIC.Color,'COLOR     ')+' - '+FABRIC.Desc
    *B602404,1 If fabric location was selected get the onhand and onorder from there.
    
    *C102819,1 Include the trim location [Start]
    *IF llByFabLoc AND cCatgTyp='F' 
    IF llByFabLoc
    *C102819,1 [End]
    
      =SEEK(lcSekExpr+lcFLoctn,'FABDYE')
      lnOnHand = FABDYE.OnHand
      lnOnOrdr = FABDYE.OnOrder
    ENDIF
    *B602404,1 End.
    
    *C102741,1 AMH Consider case of print by warehouse Yes [Start]
    IF llRpByWare .AND. BOM.CCATGTYP $ 'FT' .AND. !EMPTY(CFABLOC)
      =SEEK(lcSekExpr+CFABLOC,'FABDYE')
      lnOnHand = FABDYE.OnHand
      lnOnOrdr = FABDYE.OnOrder
    ENDIF
    *C102741,1 AMH [End]
    
  ENDIF
ELSE

  STORE 0   TO lnOnHand, lnOnOrdr ,lnConv ,lnLeadTm  ,lnFabcost
  STORE " " TO lcDescrp, lcFabVen,lcUOMBuy
ENDIF

*C102819,1 AMH Save memory variables to restore them when print existing MRP report [Start]
FOR lnI = 1 TO 20
  laSaveVar[lnI,2] = EVALUATE(laSaveVar[lnI,1])
ENDFOR
SAVE TO MEMO MFLTVAL ALL LIKE laSaveVar
*C102819,1 AMH [End]

*E301128,1 Compute Required yields for Open, Open+Hold, and Projected [Begin 
*!       Instead of calling lfvReq() from .frx 3 times with defferent 
*!       Parameters per Fabric/color to calculate 
*!       lnOReq , lnHReq , lnPReq which are report variabels each
*!       of them call this function .
*!       In Material open to sell report we calculat them one time to 
*!       in lfGetInfo() 
*B604564,1 HBG 07/02/2001 Comment these lines to fix bug of "Requir.US" column is 
*B604564,1                calculating wrong [Begin] */wab because function lfreq calculate these fields
*IF llFromOts
*  lnOReq = oreqtot*(Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/cnt

*  lnHReq = ohreqtot*(Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/cnt

*  lnPReq = Project*(Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/cnt
*ENDIF  
*B604564,1 [HBG]
*E301128,1 Compute Required yields for Open, Open+Hold, and Projected [End
RETURN ''

*!*************************************************************
*! Name      : lfLGetInfo
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get Info on line level (Style/item).
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfLGetInfo()
*!*************************************************************
FUNCTION lfLGetInfo

IF SEEK(Style,'STYLE')
 lcSDescrp = STYLE.Desc
 =SEEK('S'+STYLE.Scale,'SCALE')
 FOR lnI=1 TO 8
   lcI = STR(lnI,1)
   lcSize&lcI = SCALE.Sz&lcI
 ENDFOR 
ELSE
 lcDescrp = " "
ENDIF
*--Average Yield.
lnYield  = (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/cnt

RETURN ''


*!*************************************************************
*! Name      : lfvReq
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Calculate the requirement use (pieces*yield)
*!*************************************************************
*! Passed Parameters  : Complute
*!                      Requirement yield
*!                             'O' for open order
*!                             'H' for open+hold
*!                             'P' for projection
*!                     'S' Open Wip yield
*!*************************************************************
*! Returns            : lnItmRec -> requirement for item/color.
*!*************************************************************
*! Example   : =lfvReq('O')
*!*************************************************************
FUNCTION lfvReq
PARA lcForTyp

lnItmRec=0
DO CASE
  CASE lcForTyp='O'
*B802158,1 Start, get the requirement by size yield insted of ave yield.
*    lnItmRec = oreqtot*(Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/cnt
     lnItmRec = (oreq1*Qty1)+(oreq2*Qty2)+(oreq3*Qty3)+(oreq4*Qty4)+;
                (oreq5*Qty5)+(oreq6*Qty6)+(oreq7*Qty7)+(oreq8*Qty8)
    
    *C102741,1 AMH Consider the WIP case [Start]
    IF lcRpReqBas = 'W'
      lnItmRec = ((oHreq1*(YIELD1-QTY1))+(OREQ1*QTY1))+((oHreq2*(YIELD2-QTY2))+(OREQ2*QTY2))+;
                 ((oHreq3*(YIELD3-QTY3))+(OREQ3*QTY3))+((oHreq4*(YIELD4-QTY4))+(OREQ4*QTY4))+;
                 ((oHreq5*(YIELD5-QTY5))+(OREQ5*QTY5))+((oHreq6*(YIELD6-QTY6))+(OREQ6*QTY6))+;
                 ((oHreq7*(YIELD7-QTY7))+(OREQ7*QTY7))+((oHreq8*(YIELD8-QTY8))+(OREQ8*QTY8))
    ENDIF
    *C102741,1 AMH [End]
    
  CASE lcForTyp='H'
*    lnItmRec = ohreqtot*(Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/cnt
     lnItmRec = (oHreq1*Qty1)+(oHreq2*Qty2)+(oHreq3*Qty3)+(oHreq4*Qty4)+;
                (oHreq5*Qty5)+(oHreq6*Qty6)+(oHreq7*Qty7)+(oHreq8*Qty8)
    
    *C102741,1 AMH Consider the WIP case [Start]
    IF lcRpReqBas = 'W'
      lnItmRec = (oHreq1*YIELD1)+(oHreq2*YIELD2)+(oHreq3*YIELD3)+(oHreq4*YIELD4)+;
                 (oHreq5*YIELD5)+(oHreq6*YIELD6)+(oHreq7*YIELD7)+(oHreq8*YIELD8)
    ENDIF
    *C102741,1 AMH [End]
    
  CASE lcForTyp='P'
*    lnItmRec = Project*(Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)/cnt
     lnItmRec = (NPROJ1*Qty1)+(NPROJ2*Qty2)+(NPROJ3*Qty3)+(NPROJ4*Qty4)+;
                (NPROJ5*Qty5)+(NPROJ6*Qty6)+(NPROJ7*Qty7)+(NPROJ8*Qty8)
    
    *C102741,1 AMH Consider the WIP case [Start]
    IF lcRpReqBas = 'W' .AND. !llUpdPlan
      IF lcRpBasePj = 'O'
        lnItmRec = 0
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          lnohreq = EVALUATE('OHREQ'+lcI) + ROUND(EVALUATE('OHREQ'+lcI) * (lnRpPrIncr/100),0)
          lnoxreq = EVALUATE('OREQ'+lcI)  + ROUND(EVALUATE('OREQ'+lcI)  * (lnRpPrIncr/100),0)
          lnItmRec = lnItmRec + (lnohreq*(EVALUATE('YIELD'+lcI+'-QTY'+lcI))) +;
                                (lnoxreq*EVALUATE('QTY'+lcI))
        ENDFOR
      ENDIF
      IF lcRpBasePj = 'H'
        lnItmRec = (NPROJ1*YIELD1)+(NPROJ2*YIELD2)+(NPROJ3*YIELD3)+(NPROJ4*YIELD4)+;
                   (NPROJ5*YIELD5)+(NPROJ6*YIELD6)+(NPROJ7*YIELD7)+(NPROJ8*YIELD8)
      ENDIF
    ENDIF
    *C102741,1 AMH [End]
    
*B802158,1 End.
  CASE lcForTyp='S'
    lnItmRec = nYToWIP
ENDCASE
RETURN (lnItmRec)


*!*************************************************************
*! Name      : lfvReqln                             *B602429,1 
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Calculate the requirement use per size.
*!*************************************************************
*! Passed Parameters  : lcForTyp > Requirement for type
*!                             'O' for open order
*!                             'H' for open+hold
*!                             'P' for projection
*!                             'S' Open Wip yield
*!                     lnRSize > Size no to get req. for it.
*!*************************************************************
*! Returns            : lnItmRec -> requirement for item/color.
*!*************************************************************
*! Example   : =lfvReqLn('O',3)
*!*************************************************************
FUNCTION lfvReqLn
PARA lcForTyp,lnRSize

lcRSize=STR(lnRSize,1)
lnItmRec=0
DO CASE
  CASE lcForTyp='O'
    lnItmRec = (OReq&lcRSize * Qty&lcRSize) 
  CASE lcForTyp='H'
    lnItmRec = (OHReq&lcRSize * Qty&lcRSize) 
  CASE lcForTyp='P'
    lnItmRec = (nProj&lcRSize * Qty&lcRSize) 
  *--Open Wip yards on P/o or C/t. passed as 'S'
  CASE lcForTyp='S'
    lnItmRec = nYToWIP&lcRSize
ENDCASE
RETURN (lnItmRec)


*!*************************************************************
*! Name      : lfRqIsUsd
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get issed Used and Net required qty.
*!*************************************************************
*! Passed Parameters  : lcPrStyle -> Current style
*!                      lcPrItem  -> Current Item
*!                      lcPrIClr  -> Current Item color
*!                      llPrMake  -> Style Make (Dom. or Imp.)
*!                      lcPrTyp   -> Item type
*!                      lnPrOpnWip-> Open Style WIP.
*!                      llClcIssue-> Need to calculate issue.
*!*************************************************************
*! Returns            : space(0) to not print anything in repo.
*!*************************************************************
*! Example   :
*! lfRqIsUsd(EVAL(lcMatReq+'.Style'),EVAL(lcMatReq+'.Item'),;
*!            EVAL(lcMatReq+'.IClr'),EVAL(lcMatReq+'.lStyMake'),;
*!            EVAL(lcMatReq+'.Typ'),EVAL(lcMatReq+'.nOpnWip'),;
*!           (EVAL(lcMatReq+'.Item')+EVAL(lcMatReq+'.Iclr')<>lcOldItem))
*!*************************************************************
FUNCTION lfRqIsUsd
PARA lcPrStyle,lcPrItem,lcPrIClr,llPrMake,lcPrTyp,lnPrOpnWip,llClcIssue

*C102819,1 AMH Do nothing in case of MRP existing [Start]
IF !llFromOts .AND. lcRpMrpBas = 'E'
  RETURN ''
ENDIF
*C102819,1 AMH [End]

IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  SELECT (lcMatReq)
  lnXRecNo = RECNO()
  SET ORDER TO TAG Matreq
ENDIF

*--Hold old item/color code.
lcOldItem = lcPrItem + lcPrIClr
*--If no open wip qty exist and no need to calculate issue then exit.
IF lnPrOpnWip = 0  AND ! llClcIssue

  *C200096,1[start] correct the index
  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    SELECT (lcMatReq)
    GO lnXRecNo
    SET ORDER TO TAG MatreqC
    SET KEY TO cCatgTyp
    GO lnXRecNo

  ENDIF
  *C200096,1[end]

  *B606664,4 AMH Calculate the net req. by style [Start]
  =lfUpdNetRq()
  *B606664,4 AMH [End]
        
  RETURN ''
ENDIF

*--Work files.
lcWrkLnFl=IIF(llPrMake,'CUTTKTL','POSLN')
lcWrkHdFl=IIF(llPrMake,'CUTTKTH','POSHDR')
IF !SEEK(lcPrStyle,lcWrkLnFl)
  *C200096,1[start] restore the old index case by fabric
  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    SELECT (lcMatReq)
    GO lnXRecNo
    SET ORDER TO TAG MatreqC
    SET KEY TO cCatgTyp
    GO lnXRecNo

  ENDIF
  *C200096,1[end]
  RETURN ''
ENDIF
lcSvTkt = "  "
lnAlias = SELECT()
SELECT (lcWrkLnFl)

SCAN WHILE Style = lcPrStyle FOR &lcWrkHdFl..Status $ 'O' AND TranCd = '1'
  *--Tiket type.
  lcTktType = IIF(llPrMake,'M','I') 
  *--C/t no or P/o no.
  lcTktk    = IIF(llPrMake,Cuttkt,Po) 
  *--Line no.
  lcSLineNo = STR(&lcWrkLnFl..LineNo,6)
  *B605049,1 WAB (Start) Caluclate the WIP for each PO or CT because the program multiply the 
  *B605049,1 WAB (Start) BOM.untiQty * the wip witch is rong because the Untiqty changed from Po/ct to onther
  *IF lnPrOpnWip <> 0 
    DECLARE laWipPln[9]
    laWipPln = 0
   =lfGtWipPln()
  IF laWipPln[9] <> 0
    *B605049,1 WAB (End)
    SELECT BOMLINE 
    *--Seek on full expresion in BomLine.
    *-cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
    lcBomLnKey = lcTktType+'1'+lcTktk+lcSLineNo+lcPrTyp+lcPrStyle+SPACE(6)+lcPrItem+lcPrIClr+SPACE(6)
    IF SEEK(lcBomLnKey)
      
      *C102741,1 AMH Consider case of print by warehouse [Start]
      IF llRpByWare .AND. CCATGTYP $ 'FT'
        llFound = !EMPTY(COPRCODE) .AND. SEEK(CIMTYP+CTKTNO+COPRCODE,'MFGOPRHD') .AND.;
                  !EMPTY(MFGOPRHD.CCONTCODE) .AND. SEEK(MFGOPRHD.CCONTCODE,'APVENDOR') .AND.;
                  !EMPTY(APVENDOR.CWARECODE)
        IF llFound
          IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'N'+APVENDOR.CWARECODE
            LOOP
          ENDIF
        ELSE
          IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'Y'+SPACE(6)
            LOOP
          ENDIF
        ENDIF
      ENDIF
      *C102741,1 AMH [End]
      
      lnBomUqt = BOMLINE.UnitQty
      SELECT (lcMatReq)
      *B602429,1 Added to update nYToWip per size.
      *B605049,1 WAB (Start) muttply the unitqty by open wip for each PO/CT
      *REPLACE nYTOWIP  WITH nYTOWIP  + (lnPrOpnWip * lnBomUqt),;
              nYTOWIP1 WITH nYTOWIP1 + ( nOpnWIP1 * lnBomUqt ),;
              nYTOWIP2 WITH nYTOWIP2 + ( nOpnWIP2 * lnBomUqt ),;
              nYTOWIP3 WITH nYTOWIP3 + ( nOpnWIP3 * lnBomUqt ),;
              nYTOWIP4 WITH nYTOWIP4 + ( nOpnWIP4 * lnBomUqt ),;
              nYTOWIP5 WITH nYTOWIP5 + ( nOpnWIP5 * lnBomUqt ),;
              nYTOWIP6 WITH nYTOWIP6 + ( nOpnWIP6 * lnBomUqt ),;
              nYTOWIP7 WITH nYTOWIP7 + ( nOpnWIP7 * lnBomUqt ),;
              nYTOWIP8 WITH nYTOWIP8 + ( nOpnWIP8 * lnBomUqt )
     
      REPLACE nYTOWIP  WITH nYTOWIP  + (laWipPln[9] * lnBomUqt),;
              nYTOWIP1 WITH nYTOWIP1 + (laWipPln[1] * lnBomUqt ),;
              nYTOWIP2 WITH nYTOWIP2 + (laWipPln[2] * lnBomUqt ),;
              nYTOWIP3 WITH nYTOWIP3 + (laWipPln[3] * lnBomUqt ),;
              nYTOWIP4 WITH nYTOWIP4 + (laWipPln[4] * lnBomUqt ),;
              nYTOWIP5 WITH nYTOWIP5 + (laWipPln[5] * lnBomUqt ),;
              nYTOWIP6 WITH nYTOWIP6 + (laWipPln[6] * lnBomUqt ),;
              nYTOWIP7 WITH nYTOWIP7 + (laWipPln[7] * lnBomUqt ),;
              nYTOWIP8 WITH nYTOWIP8 + (laWipPln[8] * lnBomUqt )           
      *B605049,1 WAB (END)
      *B602429,1 End.
    ENDIF
  ENDIF
  
  *B604785,1 HBG 08/16/2001 Fix bug of duplicating the issue and remaining Qty [Begin]
  *B605049,1 WAB (Start) in case of po the next command will prevent lineno mre than 1
  *IF &lcWrkLnFl..lineNo <> 1
  *  LOOP
  *ENDIF  
  *B605049,1 WAB (End)
  *B604785,1 [End]

 *B605049,1 WAB (Start) - coment the next line becasue i add a memo filed . this memo field 
 *B605049,1 WAB 		- will hold any CT/PO have been calculation and save the issued qty 
 *B605049,1 WAB 		- so the program will acumulate the Po/Ct just one time 
 *IF (llClcIssue AND lcSvTkt <> lcTktk)  OR !(lcTktType $ lcTkTRUn) 
 *B605049,1 WAB (End)
    SELECT CTKTBOM
    
    IF SEEK(lcTktType+lcTktk+lcPrTyp+lcPrItem+lcPrIClr)
      *B602429,1 Start.
      *--If style component need to calculate the net required by size ,
      *--used in remaining.
      
      IF EVAL(lcMatReq+'.cCatgTyp') = 'S'
        STORE 0 TO lnIssue,lnNRequ,lnNrqu1,lnNrqu2,lnNrqu3,lnNrqu4,lnNrqu5,lnNrqu6,lnNrqu7,lnNrqu8
        SUM REST Used_Qty,MAX(Req_Qty-Used_Qty,0),MAX(Req_Qty1-Used_Qty1,0),MAX(Req_Qty2-Used_Qty2,0),;
           MAX(Req_Qty3-Used_Qty3,0),MAX(Req_Qty4-Used_Qty4,0),MAX(Req_Qty5-Used_Qty5,0),;
           MAX(Req_Qty6-Used_Qty6,0),MAX(Req_Qty7-Used_Qty7,0),MAX(Req_Qty8-Used_Qty8,0);
           TO lnIssue,lnNRequ,lnNrqu1,lnNrqu2,lnNrqu3,lnNrqu4,lnNrqu5,lnNrqu6,lnNrqu7,lnNrqu8 ;
           WHILE cIMTyp+Cuttkt+typ+Item+IClr = lcTktType+lcTktk+lcPrTyp+lcPrItem+lcPrIClr
        
        SELECT (lcMatReq)
        lnSavRec = RECNO()
        lcSekKey = cCatgTyp+Item+IClr 
        SEEK lcSekKey
        *B605049,1 WAB (Start) - before accumulate the usedqty and the netreq qty we must check frst
        *B605049,1 WAB (Start) - if this PO/Ct is already acumulate to the file 
        *REPLACE REST nUsedReq WITH nUsedReq + lnIssue,;
                     nNetReq  WITH nNetReq  + lnNRequ,;
                     nNetReq1 WITH nNetReq1 + lnNrqu1,;
                     nNetReq2 WITH nNetReq2 + lnNrqu2,;
                     nNetReq3 WITH nNetReq3 + lnNrqu3,;
                     nNetReq4 WITH nNetReq4 + lnNrqu4,;
                     nNetReq5 WITH nNetReq5 + lnNrqu5,;
                     nNetReq6 WITH nNetReq6 + lnNrqu6,;
                     nNetReq7 WITH nNetReq7 + lnNrqu7,;                                                                                                         
                     nNetReq8 WITH nNetReq8 + lnNrqu8 ;
                     WHILE cCatgTyp+Item+IClr=lcSekKey
        SCAN REST WHILE cCatgTyp+Item+IClr=lcSekKey
          IF !(lcTktType+lcTktk  $ PoCt )
            REPLACE  nUsedReq WITH nUsedReq + lnIssue,;
                     nNetReq  WITH nNetReq  + lnNRequ,;
                     nNetReq1 WITH nNetReq1 + lnNrqu1,;
                     nNetReq2 WITH nNetReq2 + lnNrqu2,;
                     nNetReq3 WITH nNetReq3 + lnNrqu3,;
                     nNetReq4 WITH nNetReq4 + lnNrqu4,;
                     nNetReq5 WITH nNetReq5 + lnNrqu5,;
                     nNetReq6 WITH nNetReq6 + lnNrqu6,;
                     nNetReq7 WITH nNetReq7 + lnNrqu7,;                                                                                                         
                     nNetReq8 WITH nNetReq8 + lnNrqu8,;
                     PoCt 	  WITH poCt + lcTktType+lcTktk+','
          ENDIF
        ENDSCAN
        *B605049,1 WAB (END)
       GOTO lnSavRec
      *B602429,1 End.
       
      ELSE
        
        STORE 0 TO lnIssue,lnNRequ
        SUM REST Used_Qty,MAX(Req_Qty-Used_Qty,0) TO lnIssue,lnNRequ ;
           WHILE cIMTyp+Cuttkt+typ+Item+IClr = lcTktType+lcTktk+lcPrTyp+lcPrItem+lcPrIClr
        
        *B606664,4 AMH Not include style outside filter in the net req. [Start]
        =lfGetNetRq()
        *B606664,4 AMH [End]
        
        SELECT (lcMatReq)
        lnSavRec = RECNO()
        
        *C102741,1 AMH Consider case of print by warehouse [Start]
        *lcSekKey = cCatgTyp+Item+IClr
        lcSekKey = IIF(llRpByWare,cCatgTyp+cInHouse+cFabLoc+Item+IClr,cCatgTyp+Item+IClr)
        lcWhrCnd = IIF(llRpByWare,'cCatgTyp+cInHouse+cFabLoc+Item+IClr','cCatgTyp+Item+IClr')+;
                   '=lcSekKey'
        *C102741,1 AMH [End]
        
        SEEK lcSekKey
        *B605049,1 WAB (Start) - before accumulate the usedqty and the netreq qty we must check frst
        *B605049,1 WAB (Start) - if this PO/Ct is already acumulate to the file 
        *REPLACE REST nUsedReq WITH nUsedReq + lnIssue,;
                     nNetReq  WITH nNetReq  + lnNRequ ;
                     WHILE cCatgTyp+Item+IClr=lcSekKey
        
        *C102741,1 AMH Consider case of print by warehouse [Start]
        *SCAN REST WHILE cCatgTyp+Item+IClr=lcSekKey
        
        *B606664,4 AMH Scan for the current style only [Start]
        *SCAN REST WHILE &lcWhrCnd.
        SCAN REST WHILE &lcWhrCnd. FOR STYLE = lcPrStyle
        *B606664,4 AMH [End]
        
        *C102741,1 AMH [End]
        
          IF !(lcTktType+lcTktk  $ PoCt )
            REPLACE nUsedReq WITH nUsedReq + lnIssue,;
                    nNetReq  WITH nNetReq  + lnNRequ,;
                    PoCt 	 WITH poCt + lcTktType+lcTktk+','
          ENDIF
        ENDSCAN
        *B605049,1 WAB (End)
        GOTO lnSavRec
      ENDIF
      
      *B604785,1 HBG 08/16/2001 Comment these lines to fix bug of calculating issue Qty wrong[Begin]
      *IF !(lcTktType $ lcTkTRUn) 
      *   lcTkTRUn = lcTkTRUn + lcTktType
      *ENDIF
      *B604785,1 [END]
    ENDIF
    lcSvTkt = lcTktk
  *B605049,1 WAB (Start) 
  *ENDIF
  *B605049,1 WAB (End)

ENDSCAN
*C200096,1[start] restore the index and the key
IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  SELECT (lcMatReq)
  GO lnXRecNo
  SET ORDER TO TAG MatreqC
  SET KEY TO cCatgTyp
  GO lnXRecNo
ENDIF
*C200096,1[end]
SELECT (lnAlias)
RETURN ''



*!*************************************************************
*! Name      : lfRemain
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Compute remaining and check -ve remaining.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Call               : lfSavNRItm()
*!*************************************************************
*! Returns            : lnRemain -> Remaining.
*!*************************************************************
*! Example   : =lfRemain()
*!*************************************************************
FUNCTION lfRemain
PARA lcForTyp

*--Initilize remaining as zero.
STORE 0 TO lnRemReq , lnRemain

IF lcRpBasePj='P' AND lcForTyp='P'
  lnRemain = (lnOnHand+lnOnOrdr) - lnPReq

ELSE

  IF EVAL(lcMatReq+'.cCatgTyp') = 'S'
    *B602429,1 Start Calculate the remainning per size and accumulate it.
    FOR lnI = 1 TO 8
      lcZ=STR(lnI,1)
      lnRequr  = EVAL('ln'+lcForTyp+'Req'+lcZ)
      
      *C102741,1 AMH Change the name of variable llRpOTC to be lcRpReqBas [Start]
      *lnRemReq = IIF( llRpOTC, lnRequr , ABS( lnRequr - lnYTWip&lcZ ))
      lnRemReq = IIF( lcRpReqBas='O', lnRequr , ABS( lnRequr - lnYTWip&lcZ ))
      *C102741,1 AMH [End]

      *--Read Remaining and acumulate for all sizes.
      lnRemain = lnRemain + ;
        (lnAvl&lcZ - ( lnRemReq + EVAL(lcMatReq+'.nNetReq'+lcZ) ))
    ENDFOR
    *B602429,1 End.
  ELSE
  
    *C102741,1 AMH Change the name of variable llRpOTC to be lcRpReqBas [Start]
    *lnRemReq = IIF( llRpOTC , EVAL('ln'+lcForTyp+'Req') , ;
                    ABS( EVAL('ln'+lcForTyp+'Req') - lnYTWip) )
    lnRemReq = IIF( lcRpReqBas='O' , EVAL('ln'+lcForTyp+'Req') , ;
                    ABS( EVAL('ln'+lcForTyp+'Req') - lnYTWip) )
    *C102741,1 AMH [End]
    
    *--Read Remaining.
    
    *B606664,4 AMH Calculate the net req. by style [Start]
    *lnRemain = (lnOnHand+lnOnOrdr) - ( lnRemReq + EVAL(lcMatReq+'.nNetReq') )
    lnRemain = (lnOnHand+lnOnOrdr) - ( lnRemReq + lnNetReq )
    *B606664,4 AMH [End]
    
  ENDIF
ENDIF

*--Check negative remaining for fabrics and trims. 
IF EVAL(lcMatReq+'.cCatgTyp') $ 'FT' AND lcForTyp = lcRpRmChk AND lnRemain < 0
  *B602405,1 Take remaining qty insted of requirement.
  *lnRequired = EVAL('ln'+lcForTyp+'Req')
  lnRequired = ABS(lnRemain)
  *B602405,1 End.

  *E301128,1 if called from requirement report. [Begin]
  
  *C102819,1 AMH Do nothing in case of MRP existing [Start]
  *IF !llFromOts
  IF !llFromOts .AND. lcRpMrpBas = 'N'
  *C102819,1 AMH [End]
  
    =lfSavNRItm()
  ENDIF  
  *E301128,1 if called from requirement report. [End  ]

ENDIF

*E301128,1 if called from O.T.S. report. [Begin]
IF llFromOts
  *-- IN MATERIAL O.T.S REPORT WE NEED TO CALL THIS 
  *-- FUNCTION AGAIN TO CALCULATE VALUE OPEN TO SELL. 
  *-- INSTEAD OF THAT THE RETURN VALUE WILL BE SAVED
  *-- IN VARIABELS lnRemainO if the parameter is O
  *--              lnRemainH if the parameter is H 
  *--              lnRemainP if the parameter is P
  DO CASE
    CASE lcForTyp = 'O'
      lnRemainO = lnRemain
    CASE lcForTyp = 'H'
      lnRemainH = lnRemain
    CASE lcForTyp = 'P'
      lnRemainP = lnRemain
  ENDCASE
ENDIF
*E301128,1 if called from O.T.S. report. [Begin]

RETURN (lnRemain)


*!*************************************************************
*! Name      : lfSavNRItm
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Save item if negative remaining.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfSavNRItm()
*!*************************************************************
FUNCTION lfSavNRItm

*--Update -ve remaining only for Imported materials.
IF !SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..IClr,'FABRIC') OR FABRIC.Make
  RETURN
ENDIF
SCATTER MEMVAR
SELECT (lcTmpRmain)
lnConv = IIF(lnConv=0,1,lnConv)
IF !SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..IClr)
  APPEND BLANK
  
  *C102819,1 AMH Consider case of use vendor referance [Start]
  *REPLACE Fabric     WITH PADR(&lcMatReq..Item,7),;
          Color      WITH &lcMatReq..IClr,;
          Vendor     WITH lcFabVen ,;
          LeadTime   WITH lnLeadTm , ;
          Complete   WITH gdSysDate + LeadTime,;
          UOMBuy     WITH lcUOMBuy,;
          nFabTotQty WITH ROUND((lnRequired/lnConv),3), ;
          nFabcost   WITH lnFabcost
  IF llVenRef .AND. SEEK(PADR(lcFabVen,8)+PADR(EVALUATE(lcMatReq+'.ITEM'),7)+;
                         EVALUATE(lcMatReq+'.ICLR'),'VENDMATL')
    *B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
    *REPLACE Fabric     WITH PADR(&lcMatReq..Item,7),;
            Color      WITH &lcMatReq..IClr,;
            Vendor     WITH lcFabVen ,;
            LeadTime   WITH VENDMATL.LEADTIME , ;
            Complete   WITH gdSysDate + LeadTime,;
            UOMBuy     WITH lcUOMBuy,;
            nFabTotQty WITH ROUND((lnRequired/lnConv),3), ;
            nFabcost   WITH VENDMATL.PRICE,;
            DDELIVDATE WITH COMPLETE,;
            CVENFAB    WITH VENDMATL.CVENFAB,;
            CVENCOLR   WITH VENDMATL.CVENCOLR
    REPLACE Fabric     WITH PADR(&lcMatReq..Item,7),;
            Color      WITH &lcMatReq..IClr,;
            Vendor     WITH lcFabVen ,;
            LeadTime   WITH VENDMATL.LEADTIME , ;
            Complete   WITH gdSysDate + LeadTime,;
            UOMBuy     WITH lcUOMBuy,;
            nFabTotQty WITH ROUND((lnRequired/lnConv),3), ;
            nFabcost   WITH VENDMATL.NFABCOST,;
            DDELIVDATE WITH COMPLETE,;
            CVENFAB    WITH VENDMATL.CVENFAB,;
            CVENCOLR   WITH VENDMATL.CVENCOLR
    *B122673,1 NNA (End)
  
  ELSE
    REPLACE Fabric     WITH PADR(&lcMatReq..Item,7),;
            Color      WITH &lcMatReq..IClr,;
            Vendor     WITH lcFabVen ,;
            LeadTime   WITH lnLeadTm , ;
            Complete   WITH gdSysDate + LeadTime,;
            UOMBuy     WITH lcUOMBuy,;
            nFabTotQty WITH ROUND((lnRequired/lnConv),3), ;
            nFabcost   WITH lnFabcost,;
            DDELIVDATE WITH COMPLETE
  ENDIF
  *C102819,1 AMH [End]
  
ELSE
  REPLACE nFabTotQty WITH nFabTotQty + ROUND((lnRequired/lnConv),3)
ENDIF
SELECT (lcMatReq)
RETURN



*!*************************************************************
*! Name      : lfGenMPo
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Generate Material P/O.
*!  GENERATE PURCHASE ORDER(S) FOR MATERIAL(S) FOR WHICH THERE IS
*!                  NEGATIVE REMAINING QUANTITY IF ANY.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfGenMPo()
*!*************************************************************
FUNCTION lfGenMPo

= gfOpenFile(gcDataDir+'APVENDOR','VenCode','SH')
GO TOP
IF EOF()
 *--The vendor file is empty, you cannot create PO's .
  =gfModalGen('TRM36128B36000','DIALOG')
  RETURN .F.
ENDIF

= gfOpenFile(gcDataDir+'POFHDR','POFHDR','SH')
= gfOpenFile(gcDataDir+'POFLN' ,'POFLNF','SH')
*--Read the multi currency and multi warehouse setup.
*--Rights for edit exch. rates.
llEditExRt = gfGetMemVar('llEditExRa')
llMulCurr  = gfGetMemVar('llMulCurr')
llWareHous = (gfGetMemVar('M_WareHouse')='Y')

*E500349,1 - WAB (START) - save the company setup ( Enter Ma Po No Manual ) into llGenMApon 
*---llGenMaPoN = .T.  -----> generated Po no
*---llGenMaPoN = .F.  -----> manuall Po no 
llGENMAPON = gfGetMemVar('M_GENMAPON')='N'
*E500349,1 - WAB (END) 

IF !llWareHous
  GO TOP IN WAREHOUS
ENDIF

SELECT (lcTmpRmain)   
SCAN
  lcFabKey = Fabric+Color
  lcFbCVen = Vendor
  WAIT WINDOW 'Collecting data for Material : ' + lcFabKey NOWAIT

  =SEEK(lcFabKey,'FABRIC')
  =RLOCK()
  
  *C102819,1 AMH Consider case of use vendor referance [Start]
  *REPLACE cFabGrade  WITH FABRIC.cFabGrade,;
          cWareCode  WITH IIF(!llWareHous,WAREHOUS.cWareCode,''),;
          cPriceCur  WITH IIF(EMPTY(FABRIC.cPriceCur),gcBaseCurr,FABRIC.cPriceCur),;
          cDutyCur   WITH IIF(EMPTY(FABRIC.cDutyCur) ,gcBaseCurr,FABRIC.cDutyCur),;
          nFabcost   WITH FABRIC.nFabcost,;
          nItm_Frt   WITH FABRIC.nItm_Frt,;
          nItem_Tax  WITH FABRIC.nItem_Tax,;
          nItemQuota WITH FABRIC.nItemQuota
  REPLACE cFabGrade  WITH FABRIC.cFabGrade,;
          cWareCode  WITH IIF(!llWareHous,WAREHOUS.cWareCode,''),;
          cPriceCur  WITH IIF(EMPTY(FABRIC.cPriceCur),gcBaseCurr,FABRIC.cPriceCur),;
          cDutyCur   WITH IIF(EMPTY(FABRIC.cDutyCur) ,gcBaseCurr,FABRIC.cDutyCur),;
          nFabcost   WITH IIF(llVenRef .AND. SEEK(lcFbCVen+lcFabKey,'VENDMATL'),;
                              NFABCOST,FABRIC.nFabcost),;
          nItm_Frt   WITH FABRIC.nItm_Frt,;
          nItem_Tax  WITH FABRIC.nItem_Tax,;
          nItemQuota WITH FABRIC.nItemQuota
  *C102819,1 AMH [End]
  
  UNLOCK


  *--Search For Last Price This Material Was Purchased At From This Vendor,
  *--If it Was Never Purchased Before, Take the Price From Fabric file.
  SELECT POFLN  
  lcMPO=SPACE(6) 
  =SEEK(lcFabKey+'P')
  
  *C102819,1 AMH get the correct vendor [Start]
  *SCAN WHILE Fabric+Color+cMatType = lcFabKey+'P' ;
         FOR Trancd = '1' AND Vendor = ALLT(lcFbCVen)
  SCAN WHILE Fabric+Color+cMatType = lcFabKey+'P' ;
         FOR Trancd = '1' AND Vendor = PADR(lcFbCVen,8)
  *C102819,1 AMH [End]
  
    lcMPO  = POMat
    lnCost1 = nCost1
    lnCost2 = nCost2
    lnCost3 = nCost3
    lnCost4 = nCost4        
  ENDSCAN
  
  SELECT (lcTmpRmain)   
  IF !EMPTY(lcMPO)
    =SEEK('P'+lcMPO,'POFHDR')
    =RLOCK()
    *B603889,1 (Start)  get the vendor from the POFHdr File instead of the POFln. 
    *REPLACE Vendor     WITH POFLN.Vendor,;
            cPriceCur  WITH POFHDR.cPriceCur,;
            cDutyCur   WITH POFHDR.cDutyCur,;
            nFabcost   WITH lnCost1,;
            nItm_Frt   WITH lnCost2,;
            nItem_Tax  WITH lnCost3,;
            nItemQuota WITH lnCost4    
    
    *C102819,1 AMH Consider case of use vendor referance [Start]
    *REPLACE Vendor     WITH POFHDR.Vendor,;
            cPriceCur  WITH POFHDR.cPriceCur,;
            cDutyCur   WITH POFHDR.cDutyCur,;
            nFabcost   WITH lnCost1,;
            nItm_Frt   WITH lnCost2,;
            nItem_Tax  WITH lnCost3,;
            nItemQuota WITH lnCost4
    REPLACE Vendor     WITH POFHDR.Vendor,;
            cPriceCur  WITH POFHDR.cPriceCur,;
            cDutyCur   WITH POFHDR.cDutyCur,;
            nFabcost   WITH IIF(llVenRef .AND. SEEK(lcFbCVen+lcFabKey,'VENDMATL'),;
                                NFABCOST,lnCost1),;
            nItm_Frt   WITH lnCost2,;
            nItem_Tax  WITH lnCost3,;
            nItemQuota WITH lnCost4
    *C102819,1 AMH [End]
    
    *B603889,1 (End) 
    UNLOCK

  ELSE
    IF llMulCurr AND !EMPTY(lcFbCVen) AND ;
       SEEK(lcFbCVen,'APVENDOR') AND !EMPTY(APVENDOR.cCurrCode)
      =RLOCK()
      REPLACE cPriceCur WITH IIF(!EMPTY(APVENDOR.cCurrCode),APVENDOR.cCurrCode,cPriceCur)
      UNLOCK
    ENDIF
  ENDIF
  
  *--Update the Currency Rate and units.
  STORE 1 TO lnCurrUnt1,lnCurrUnt2
  STORE 1 TO lnPRate,lnDRate
  IF llMulCurr
    lnPRate = gfChkRate('lnCurrUnt1',cPriceCur,gdSysDate,llEditExRt,gcAct_comp,.F.)
    lnDRate = gfChkRate('lnCurrUnt2',cDutyCur ,gdSysDate,llEditExRt,gcAct_comp,.F.)
  ENDIF
  =RLOCK()
  REPLACE nPriceRat  WITH IIF(lnPRate=0,1,lnPRate),;
          nCurrUnit  WITH lnCurrUnt1,;
          nDutyRat   WITH IIF(lnDRate=0,1,lnDRate),;
          nDCurUnit  WITH lnCurrUnt2
  UNLOCK     

ENDSCAN

*--Initialize the currency method and symbol.
STORE '' TO lcPMethod,lcPUnMeth
STORE SET('CURRENCY',1) TO lcPBseSmbl
*B603525,1  (Start) 1)Increase the width of the complete date in the browse. 
*B603525,1          2)Save the century settings.  
*B603525,1          3)set century on .
*lcBrFields = "Fabric     :R :H='Fabric'      :10,"+;
             "Color      :R :H='Color'       :8 ,"+;
	         "Vendor     :R :H='Vendor'      :10,"+;
             "LeadTime   :R :H='LdTime'      :6 ,"+;
             "Complete   :R :H='Complete'    :9 ,"+;
             "nFabcost   :R :H='Price'       :8 ,"+;
             "lnBsPrc=lfGetEquv(cPriceCur,nFabcost,nPriceRat,nCurrUnit) :R :H=lcPBseSmbl+' Price'  :P='999999.99' :12,"+;
             "nFabTotQty :R :H='Qty. Needed' :12,"+;
             "UOMBUY     :R :H='UOMBuy'       :7 ,"+; 
             "cPriceCur  :R :H='PriceCur.'    :8,"+;
             "nPriceRat  :R :H='PriceRate'    :9,"+;
             "cDutyCur   :R :H='Duty Cur.'    :8,"+;
             "nDutyRat   :R :H='Duty Rate'    :9"


*C102819,1 AMH Consider case of use vendor referance [Start]
*lcBrFields = "Fabric     :R :H='Fabric'      :10,"+;
             "Color      :R :H='Color'       :8 ,"+;
	         "Vendor     :R :H='Vendor'      :10,"+;
             "LeadTime   :R :H='LdTime'      :6 ,"+;
             "Complete   :R :H='Complete'    :10 ,"+;
             "nFabcost   :R :H='Price'       :8 ,"+;
             "lnBsPrc=lfGetEquv(cPriceCur,nFabcost,nPriceRat,nCurrUnit) :R :H=lcPBseSmbl+' Price'  :P='999999.99' :12,"+;
             "nFabTotQty :R :H='Qty. Needed' :12,"+;
             "UOMBUY     :R :H='UOMBuy'       :7 ,"+; 
             "cPriceCur  :R :H='PriceCur.'    :8,"+;
             "nPriceRat  :R :H='PriceRate'    :9,"+;
             "cDutyCur   :R :H='Duty Cur.'    :8,"+;
             "nDutyRat   :R :H='Duty Rate'    :9"
lcBrFields = "Fabric     :R :H='Fabric'      :10,"+;
             "Color      :R :H='Color'       :8 ,"+;
	         "Vendor     :R :H='Vendor'      :10,"+;
             "LeadTime   :R :H='LdTime'      :6 ,"+;
             IIF(llVenRef,"dDelivDate :R :H='Delivry'    :10 ,","Complete   :R :H='Complete'    :10 ,")+;
             "nFabcost   :R :H='Price'       :8 ,"+;
             "lnBsPrc=lfGetEquv(cPriceCur,nFabcost,nPriceRat,nCurrUnit) :R :H=lcPBseSmbl+' Price'  :P='999999.99' :12,"+;
             "nFabTotQty :R :H='Qty. Needed' :12,"+;
             "UOMBUY     :R :H='UOMBuy'       :7 ,"+; 
             "cPriceCur  :R :H='PriceCur.'    :8,"+;
             "nPriceRat  :R :H='PriceRate'    :9,"+;
             "cDutyCur   :R :H='Duty Cur.'    :8,"+;
             "nDutyRat   :R :H='Duty Rate'    :9"
IF llVenRef
  lcBrFields = lcBrFields + ",cVenFab :R :H='Supp. Item' :10,cVenColr :R :H='Supp. Color' :10"
ENDIF
*C102819,1 AMH [End]

SELECT (lcTmpRmain)   
GO TOP
DO WHILE .T.
  *B603525,1 (Start)
   lcCentry= SET('CENTURY')
  SET CENTURY ON
  *B603525,1 (End)


  =ARIABROW('','Negative Remaining Materials',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'lfvEdtMat',;
               'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok' )

  *B603525,1 (Start) Restore the saved century setting .
  SET CENTURY  &lcCentry
  *B603525,1 (End)

  *--Check for any valid record (nothing selected).
  *B604689,1 HBG 08/16/2001 Allow Price field to be empty.[Begin]
  *LOCATE FOR !EMPTY(Vendor) AND nFabcost <> 0 AND nFabTotQty<>0
  LOCATE FOR !EMPTY(Vendor) AND nFabTotQty <> 0
  *B604689,1 [END]
  IF !FOUND()
    *--No vendor,price or quantity needed has been defined for these materials.
    *--Do you wish to Assign?   <Assign><Cancel>
    IF gfModalGen('INM36129B36004','DIALOG') = 1
      LOOP
    ELSE
      RETURN .F.
    ENDIF
  ENDIF

  LOCATE FOR EMPTY(Vendor)
  IF FOUND() AND gfModalGen('QRM36122B36001','DIALOG') = 2
    *---Some materials has no vendor,therefor no purchase orders will be generated for this Materials! YES/NO
    LOOP
  ENDIF

  *--Generate Same P/O for this Key :
  *--KEY VENDOR+CFABGRADE+COMPLETE+CPRICECUR+CDUTYCUR+NPRICERAT+NDUTYRAT 

  *--Note that the materials with same vendor,quality,currency and completion date will be joinded in same P/O.
  *-- "<Generate> <Modify> <Cancel>"
  lnSeletn = gfModalGen('QRM36123B36009','DIALOG')
  DO CASE
    CASE lnSeletn = 1
      EXIT 
    CASE lnSeletn = 3
      RETURN .F.
  ENDCASE
ENDDO

*C102819,1 AMH Let the complete date be the max of delivry dates 
*C102819,1 AMH for same vendor in case of use vendor referance [Start]
IF llVenRef
  SELECT VENDOR, MAX(DDELIVDATE) AS DCOMPDATE;
    FROM (lcTmpRmain);
    GROUP BY VENDOR;
    INTO CURSOR TMPVENDCOM
  SELECT TMPVENDCOM
  SCAN
    SELECT (lcTmpRmain)
    REPLACE ALL COMPLETE WITH TMPVENDCOM.DCOMPDATE FOR VENDOR = TMPVENDCOM.VENDOR
  ENDSCAN
  USE IN TMPVENDCOM
  SELECT (lcTmpRmain)
ENDIF
*C102819,1 AMH [End]

*--Open temp P/O Heder.
lcPOTmpHD = gfTempName()
SELECT POFHDR
*E500349,1 - WAB (START) - add field cTmpPoMat ( temp MA Po No ) in case of generate ma Po No manually
*COPY STRU TO (gcWorkDir+lcPOTmpHD)
= AFIELDS(laTempStru)
IF !llGENMAPON 
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
  *-- cTmpPoMat :  field used to hold the manual po no 
  laTempStru[ALEN(laTempStru,1),1] = 'cTmpPoMat'
  laTempStru[ALEN(laTempStru,1),2] = 'C'
  laTempStru[ALEN(laTempStru,1),3] = 6
  laTempStru[ALEN(laTempStru,1),4] = 0
ENDIF
CREATE DBF (gcWorkDir+lcPOTmpHD) FROM ARRAY laTempStru
*E500349,1 - WAB (END)

= gfOpenFile(gcWorkDir+lcPOTmpHD,'','EX')
INDEX ON Vendor+cFabGrade+DTOC(Complete)+cPriceCur+cDutyCur+STR(nPriceRat,9,4)+STR(nDutyRat,9,4) TAG (lcPOTmpHD)

*--Open temp P/O Lines.
lcPOTmpLN = gfTempName()
SELECT POFLN
COPY STRU TO (gcWorkDir+lcPOTmpLN)
= gfOpenFile(gcWorkDir+lcPOTmpLN,'','EX')
INDEX ON POMAT+STR(LINENO,6) TAG (lcPOTmpLN)

*B802377,1 Start. - Open codes file.
= gfOpenFile(gcDataDir+'CODES','CODES','SH')
*B802377,1 End.
*--Read Default.
DECLARE laCodes[3,10],laTerms[1,2],laDivision[1,2],laShipVia[1,2]
STORE '' TO laCodes,laTerms,laDivision,laShipVia
STORE 1  TO lnTerms,lnDivision,lnShipVia
STORE '' TO laCodes[1,4],laCodes[2,4],laCodes[3,4]
STORE .F. TO laCodes[1,5],laCodes[1,6],laCodes[2,5],laCodes[2,6],laCodes[3,5],laCodes[3,6]
laCodes[1,1] = 'CTERMCODE'
laCodes[1,2] = 'laTerms'
laCodes[1,3] = 'lnTerms'
laCodes[1,10] = 'cTermCode'
= gfwCodePop( @laCodes, "CTERMCODE" ,'D' )  
laCodes[2,1] = 'SHIPVIA'
laCodes[2,2] = 'laShipVia'
laCodes[2,3] = 'lnShipVia'
laCodes[2,10] = 'SHIPVIA'
= gfwCodePop( @laCodes, "SHIPVIA" ,'D' )  
laCodes[3,1] = 'CDIVISION'
laCodes[3,2] = 'laDivision'
laCodes[3,3] = 'lnDivision'
laCodes[3,10] = 'cDivision'
= gfwCodePop( @laCodes, "CDIVISION" ,'D' )  

*--Read codes from defaults.
lcDEFDiv  = laDivision[lnDivision,2]
lcDEFTerms= laTerms[lnTerms,2]
lcDEFShpVa= laShipVia[lnShipVia,2]


SELECT (lcTmpRmain)   
*B604689,1 HBG 08/16/2001 Allow Price field to be empty.[Begin]
*SCAN FOR !EMPTY(Vendor) AND nFabTotQty<>0 AND nFabcost<>0
SCAN FOR !EMPTY(Vendor) AND nFabTotQty<>0 
*B604689,1 [End]
  
  lcPOKey = Vendor+cFabGrade+DTOC(Complete)+cPriceCur+cDutyCur+STR(nPriceRat,9,4)+STR(nDutyRat,9,4)
  =SEEK(cWareCode,'WAREHOUS')
  =SEEK(Vendor,'APVENDOR')

  SELECT (lcPOTmpHD)
  IF SEEK(lcPOKey)
    lnQtyToAcc =  &lcTmpRmain..nFabTotQty
    REPLACE LASTLINE    WITH LASTLINE+1,;
            NFABORDER   WITH NFABORDER+ lnQtyToAcc,;
            NPO_OPEN    WITH NPO_OPEN + lnQtyToAcc,;
            NCOST1      WITH NCOST1   + (lnQtyToAcc * &lcTmpRmain..nFabcost  ),;
            NCOST2      WITH NCOST2   + (lnQtyToAcc * &lcTmpRmain..nItm_Frt  ),;
            NCOST3      WITH NCOST3   + (lnQtyToAcc * &lcTmpRmain..nItem_Tax ),;
            NCOST4      WITH NCOST4   + (lnQtyToAcc * &lcTmpRmain..nItemQuota),;
            NECOST1     WITH NECOST1  + (NFABORDER * lfGetEquv(&lcTmpRmain..cPriceCur,&lcTmpRmain..nFabcost,&lcTmpRmain..nPriceRat,&lcTmpRmain..nCurrUnit)),;
            NECOST2     WITH NECOST2  + (NFABORDER * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItm_Frt,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
            NECOST3     WITH NECOST3  + (NFABORDER * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItem_Tax,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
            NECOST4     WITH NECOST4  + (NFABORDER * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItemQuota,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit)),;
            POTOTAL     WITH NECOST1+NECOST2+NECOST3+NECOST4
  ELSE
    *E500349,1 - WAB (START)- in case manual ma po no call local function lfsequence()
    *lcMatPo = gfSEQUENCE('POMAT')
    lcMatPo = IIF(llGENMAPON,gfSequence('POMAT'),lfSequence())
    *E500349,1 - WAB (END) 
    APPEND BLANK  
    REPLACE POMAT       WITH lcMatPo,;
            CMATTYPE    WITH 'P',;
            STATUS      WITH 'O',;  
            VENDOR      WITH &lcTmpRmain..Vendor,;
            ENTERED     WITH gdSysDate,;
            COMPLETE    WITH &lcTmpRmain..Complete ,;
            CPRICECUR   WITH &lcTmpRmain..cPriceCur,;
            CDUTYCUR    WITH &lcTmpRmain..cDutyCur ,;
            NPRICERAT   WITH &lcTmpRmain..nPriceRat,;
            NDUTYRAT    WITH &lcTmpRmain..nDutyRat ,;
            CFABGRADE   WITH &lcTmpRmain..cFabGrade,;
            LASTLINE    WITH 1,;
            CONTACT     WITH APVENDOR.CVENCONT,;
            PHONE       WITH APVENDOR.CPHONENO,;
            CDIVISION   WITH IIF(EMPTY(APVENDOR.CDIVISION),lcDEFDiv,APVENDOR.CDIVISION),;
            CTERMCODE   WITH IIF(EMPTY(APVENDOR.CTERMCODE),lcDEFTerms,APVENDOR.CTERMCODE),;
            SHIPVIA     WITH lcDEFShpVa
    REPLACE NFABORDER   WITH &lcTmpRmain..nFabTotQty,;
            NPO_OPEN    WITH NFABORDER ,;
            NCOST1      WITH NFABORDER * &lcTmpRmain..nFabcost  ,;
            NCOST2      WITH NFABORDER * &lcTmpRmain..nItm_Frt  ,;
            NCOST3      WITH NFABORDER * &lcTmpRmain..nItem_Tax ,;
            NCOST4      WITH NFABORDER * &lcTmpRmain..nItemQuota,;
            NECOST1     WITH NFABORDER * lfGetEquv(&lcTmpRmain..cPriceCur,&lcTmpRmain..nFabcost,&lcTmpRmain..nPriceRat,&lcTmpRmain..nCurrUnit),;
            NECOST2     WITH NFABORDER * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItm_Frt,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
            NECOST3     WITH NFABORDER * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItem_Tax,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
            NECOST4     WITH NFABORDER * lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItemQuota,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
            POTOTAL     WITH NECOST1+NECOST2+NECOST3+NECOST4
    *B604689,1 HBG 08/16/2001 Address1 field in POFHDR should be Address1 of the Warehouse not the description [Begin] 
    *REPLACE CWARECODE   WITH &lcTmpRmain..cWareCode,;
            COUTADDR1   WITH WAREHOUS.CDESC,;
            COUTADDR2   WITH WAREHOUS.CADDRESS1,;
            COUTADDR3   WITH WAREHOUS.CADDRESS2,;
            COUTADDR4   WITH WAREHOUS.CADDRESS3,;
            COUTADDR5   WITH WAREHOUS.CADDRESS4,;
            LINK_CODE   WITH 'DEFDEF',;
            CADD_USER   WITH gcUser_id,;
            DADD_DATE   WITH gdSysDate,;
            CADD_TIME   WITH TIME()
    REPLACE CWARECODE   WITH &lcTmpRmain..cWareCode,;
            COUTADDR1   WITH WAREHOUS.CADDRESS1,;
            COUTADDR2   WITH WAREHOUS.CADDRESS2,;
            COUTADDR3   WITH WAREHOUS.CADDRESS3,;
            COUTADDR4   WITH WAREHOUS.CADDRESS4,;
            COUTADDR5   WITH WAREHOUS.CADDRESS5,;
            LINK_CODE   WITH 'DEFDEF',;
            CADD_USER   WITH gcUser_id,;
            DADD_DATE   WITH gdSysDate,;
            CADD_TIME   WITH TIME()
    *B604689,1 [End]
    
    *C102819,1 AMH Save the MRP# of the report in the temp MPO header file [Start]
    IF llSaveMrp
      REPLACE CMRP WITH lcMrp
    ENDIF
    *C102819,1 AMH [End]
    
  ENDIF

  *--Update Material P/O lines.
  *B802995,1 [START] We have to make sure that we have the correct data in fabric File
  =SEEK(&lcTmpRmain..Fabric+&lcTmpRmain..Color,'FABRIC')
  *B802995,1[END]
  
  SELECT (lcPOTmpLN)
  APPEND BLANK
  REPLACE CMATTYPE    WITH 'P',;
          POMAT       WITH &lcPOTmpHD..POMAT,;
          TRANCD      WITH '1',;
          LINENO      WITH &lcPOTmpHD..LASTLINE,;
          VENDOR      WITH &lcPOTmpHD..VENDOR,;
          FABRIC      WITH &lcTmpRmain..Fabric,;
          COLOR       WITH &lcTmpRmain..Color,;
          PATTERN     WITH FABRIC.Pattern,;
          REFERENCE   WITH 'Refer To Material Requirements',;
          WIDTH       WITH FABRIC.Width,;
          NFABTOTQTY  WITH &lcTmpRmain..nFabTotQty,;
          CWARECODE   WITH &lcPOTmpHD..cWareCode
  REPLACE NCOST1      WITH &lcTmpRmain..nFabcost ,;
          NCOST2      WITH &lcTmpRmain..nItm_Frt ,;
          NCOST3      WITH &lcTmpRmain..nItem_Tax,;
          NCOST4      WITH &lcTmpRmain..nItemQuota,;
          NECOST1     WITH lfGetEquv(&lcTmpRmain..cPriceCur,&lcTmpRmain..nFabcost,&lcTmpRmain..nPriceRat,&lcTmpRmain..nCurrUnit),;
          NECOST2     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItm_Frt,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
          NECOST3     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItem_Tax,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
          NECOST4     WITH lfGetEquv(&lcTmpRmain..cDutyCur,&lcTmpRmain..nItemQuota,&lcTmpRmain..nDutyRat,&lcTmpRmain..nDCurUnit),;
          CFABGRADE   WITH &lcTmpRmain..cFabGrade,;
          CADD_USER   WITH gcUser_id,;
          DADD_DATE   WITH gdSysDate,;
          CADD_TIME   WITH TIME()
    
  *C102819,1 AMH Save the delivry date , vendor item and vendor color [Start]
  REPLACE DDELIVDATE  WITH EVALUATE(lcTmpRmain+'.dDelivDate'),;
          CVENFAB     WITH EVALUATE(lcTmpRmain+'.cVenFab'),;
          CVENCOLR    WITH EVALUATE(lcTmpRmain+'.cVenColr')
  *C102819,1 AMH [End]
  
ENDSCAN

*--Edit warehous and some additional P/O information.
*B802981,1 [Start] Correct the spelling error
*lcBrTtl   = 'Geneteted P/Os'
lcBrTtl   = 'Generated P/Os'
*B802981,1 [End]
SELECT (lcPOTmpHD)
GO TOP
*B803005,1 (Start) Comment the code for the generated po browse and add the new screen code.
*DEFINE WINDOW lcTmpWind ;
       AT 0, 0 SIZE 19,89;
       FONT "MS Sans Serif", 9 ;
       FLOAT ;
       NOCLOSE ;
       SHADOW ;
       NOMINIMIZE ;
       SYSTEM ;
       COLOR SCHEME 10  	   
*MOVE WINDOW lcTmpWind CENTER
*DO WHILE .T. 
  *E500349,1 - WAB (START)- in case of generate manual ma po no browse the temp po no .
  *BROWSE FIELDS pomat :R :H='P/O',Vendor :R :15,lcVenName=IIF(SEEK(Vendor,'APVENDOR'),APVENDOR.CVENCOMP,'') :H='Vendor name' : 40,;
     cWareCode :P='@! XXXXXX' :H='Shipto Location' :V=lfvEdtLoc() ;
         WINDOW lcTmpWind ;
         LOCK 0;
         NOMENU;         
         NOAPPEND;
         NODELETE;
         TITLE lcBrTtl
         
*  lcFields = IIF(llGENMAPON,"pomat :R ","cTmpPoMat :P='@! XXXXXX' :V=lfvTmpPoMa()")+":H='P/O' ,Vendor :R :15,"+;
            "lcVenName=IIF(SEEK(Vendor,'APVENDOR'),APVENDOR.CVENCOMP,'') :H='Vendor name' : 40,"+;
            "cWareCode :P='@! XXXXXX' :H='Shipto Location' :V=lfvEdtLoc()"
         
*  BROWSE FIELDS &lcFields;
         WINDOW lcTmpWind;                
         LOCK 0;
         NOMENU;         
         NOAPPEND;
         NODELETE;
         TITLE lcBrTtl
         
  *E500349,1 - WAB (END)
*  IF LASTKEY() = 27
    *E500349,1 - WAB (START)-check the Ma po no is not empty and correct
*    IF !llGENMAPON .AND. !lfvTmpPoMa(.T.)
      *-Material Po. Number have not been defined in some generates Pos.Please enter the Material Po.Number.
*      = gfModalGen('INM36172B36000','DIALOG')
*      LOOP
*    ENDIF
    *E500349,1 - WAB (END)
    *--Check the warehous is not empty.
*    IF llWareHous
*      LOCATE FOR EMPTY(cWareCode)
*      IF FOUND()
        *-Some generated P/Os has not defined shipto location, Please enter the shipto location.
*        = gfModalGen('INM36124B36000','DIALOG')
*      ELSE
*        EXIT
*      ENDIF
*    ELSE
*      EXIT
*  ENDIF
*  ENDIF
*ENDDO
*RELEASE WINDOW lcTmpWind

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrwTitl)
ON KEY LABEL ESCAPE     DO lpEsc
lcBsWind=gfTempName()
lcBrwCont=gfTempName()
lcPushB=gfTempName()
lcBmpOK= gcBmpHome + "OK.bmp"
lcMainTtl='Material Requirements'
lcBrwTitl='Generated P/Os'
DO(gcRepHome+gcAct_Appl+"\MaPOMR.spx")
ON KEY LABEL ALT+B
ON KEY LABEL ESCAPE
*B803005,1  (End)

*E500349,1 - WAB (START) in case of manual ma po no replace the temp poline with MA PO NO save it i nte temp headre
IF !llGENMAPON
  GO TOP
  SCAN 
    SELECT (lcPOTmpLN)
    REPLACE ALL POMAT WITH &lcPOTmpHD..cTmpPoMat FOR POMAT = &lcPOTmpHD..POMAT
    SELECT (lcPOTmpHD)
    REPLACE POMAT WITH cTmpPoMat 
  ENDSCAN
ENDIF
*E500349,1 - WAB (END)

*--Start update the master files.
SELECT (lcPOTmpLN)
SCAN 

  *-1) Update FABRIC and FABDYE file.-------------------------------
  =SEEK(Fabric+Color,'FABRIC')
  lnOnOrder = &lcPOTmpLN..nFabTOTQTY * FABRIC.CONV 
  SELECT FABRIC
  =RLOCK()
  REPLACE ONORDER WITH ONORDER + lnOnOrder
  UNLOCK
  SELECT (lcPOTmpHD)
  LOCATE FOR POMAT = &lcPOTmpLN..POMAT
  lcHdrLoc = cWareCode
  SELECT FABDYE
  IF !SEEK(&lcPOTmpLN..Fabric+&lcPOTmpLN..Color+lcHdrLoc,'FABDYE')
    APPEND BLANK
    REPLACE FABRIC     WITH &lcPOTmpLN..FABRIC,;
            COLOR      WITH &lcPOTmpLN..COLOR,;
            CWARECODE  WITH lcHdrLoc,;
            nFAve_Cost WITH FABRIC.nFAve_Cost,;
            nAveCstBuy WITH FABRIC.nAveCstBuy
  ENDIF
  =RLOCK()
  REPLACE ONORDER WITH ONORDER + lnOnOrder
  UNLOCK

  *-2) Update APVENDOR file.---------------------------------------
  SELECT APVENDOR  
  =SEEK(&lcPOTmpLN..Vendor)
  =RLOCK()
  REPLACE nVenOpnPO WITH nVenOpnPO + &lcPOTmpLN..nEcost1
  UNLOCK        

ENDSCAN

*-4) Update POFHDR file.--------------------------------------------
SELECT POFLN
APPEND FROM (gcWorkDir+lcPOTmpLN)

*-3) Update POFLN file.---------------------------------------------

SELECT POFHDR
APPEND FROM (gcWorkDir+lcPOTmpHD)

*--Would you like to preview/print the purchase orders just created?
*--<Preview>  <Print>  <None>.
lnSelDev = gfModalGen('QRM36125B36010','DIALOG')

IF lnSelDev <> 3
  IF lnSelDev = 1
    gcDevice = 'SCREEN'
  ELSE
    IF ! pSetup(.T.,.F.)
      RETURN
    ENDIF  
  ENDIF

  *--Set the print po report tag.
  SET ORDER TO TAG POFLN IN POFLN
  
  SELECT (lcPOTmpHD)
  INDEX ON POMat TAG (lcPOTmpHD)

  *--Run Print Material P/O form program...
  lcRpRName  = 'MAMATPA'
  lcSavTmpFrm = lcOGTmpForm
  lcOGTmpForm = ' '

  *E301077,72 Open Company File.
  IF !USED('SYCCOMP')
    USE (gcSysHome+'SYCCOMP') ORDER TAG Ccomp_id IN 0 SHARED
  ENDIF

  DO (gcRepHome+gcAct_Appl+'\MAMATP') WITH lcPOTmpHD
  lcOGTmpForm = lcSavTmpFrm
  lcRpRName   = 'MAMATRQ' 

ENDIF

*--Remove the P/O temp files.
USE IN (lcPOTmpLN)
USE IN (lcPOTmpHD)

ERASE (gcWorkDir+lcPOTmpHD+'.DBF')
ERASE (gcWorkDir+lcPOTmpHD+'.CDX')
ERASE (gcWorkDir+lcPOTmpLN+'.DBF')
ERASE (gcWorkDir+lcPOTmpLN+'.CDX')

RETURN .T.



      
*!*************************************************************
*! Name      : lfvEdtMat
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Edit -ve Remaining line information.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvEdtMat()
*!*************************************************************
FUNCTION lfvEdtMat

*--Initialize screen variables.
llOk = .F.
SCATTER MEMVAR
STORE 1 TO lnCurrUnt1,lnCurrUnt2

lnSavRec = RECNO()
DO (gcRepHome+"MA\MAEDTFB.SPX")
IF llOk
  IF Vendor <> m.Vendor OR cPriceCur<>m.cPriceCur OR nPriceRat<>m.nPriceRat
    SELECT (lcTmpRmain)   
    LOCATE FOR Vendor = m.Vendor AND (cPriceCur<>m.cPriceCur OR nPriceRat<>m.nPriceRat)
    IF FOUND()
      *--This vendor has other PO lines. Replace all with new currency/rate ?","Yes;No
      IF gfModalGen('QRM36126B36001','DIALOG') = 1
        *B602408,1 Added to update the duty currency also for the common vendor.
        REPLACE ALL cPriceCur WITH m.cPriceCur,;
                    cDutyCur  WITH m.cDutyCur,;
                    nPriceRat WITH m.nPriceRat,;
                    nDutyRat  WITH m.nDutyRat,;
                    nCurrUnit WITH lnCurrUnt1,;
                    nDCurUnit WITH lnCurrUnt2  FOR Vendor = m.Vendor
      ENDIF
    ENDIF
  ENDIF
  SELECT (lcTmpRmain)   
  GOTO lnSavRec
  
  *C102819,1 AMH the delivry date must be same as complete date 
  *C102819,1 AMH ic case of not use vendor referance [Start]
  IF !llVenRef
    m.dDelivDate = m.Complete
  ENDIF
  *C102819,1 AMH [End]
  
  GATHER MEMVAR
  REPLACE nCurrUnit  WITH lnCurrUnt1,;
          nDCurUnit  WITH lnCurrUnt2
ENDIF
RETURN


*!*************************************************************
*! Name      : lfGetEquv
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get equivelent cost in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfGetEquv()
*!*************************************************************
FUNCTION lfGetEquv
PARA lcPcrcy,lnFrnCost,lnCurRate,lnCurUnt

lcPMethod = gfGetExSin(@lcPUnMeth,lcPcrcy)
lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
RETURN lnEquCost   

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate vendor in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvVendor()
*!*************************************************************
*C102819,1 AMH Change the function name since it was used for vendor option of OTS report [Start]
*FUNCTION lfvVendor
FUNCTION lfvVendorX
*C102819,1 AMH [End]

PRIVATE lcBrFields

IF !SEEK(m.Vendor , 'APVENDOR')
  lcBrFields = "CVENDCODE :H='Vendor',"+;
               "CVENCOMP  :H='Name',"+;
               "cPhoneNo  :H='Phone',"+;
               "cVenOurAc :H='Our Account' "
  SELECT APVENDOR
  IF gfBrows('','cVendCode', 'laTempArr','Vendors')
    m.Vendor=APVENDOR.cVendCode
  ELSE
    m.Vendor=SPACE(8)
  ENDIF
  SHOW GET m.Vendor
  SELECT (lcTmpRmain)   
ENDIF
=lfrefresh()
RETURN

*!*************************************************************
*! Name      : lfvMatVend
*! Developer : Ahmed Maher - AMH
*! Date      : 09/04/2002
*! Purpose   : Validate vendor in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvMatVend()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfvMatVend
PRIVATE lcVendor

IF !EMPTY(m.Vendor) .AND. (!SEEK(m.Vendor , 'APVENDOR') .OR. !('M' $ APVENDOR.CVENSUPTYP))
  lcVendor = m.Vendor
  =gfApVnBrow (@lcVendor,.F.,'M')
  m.Vendor = lcVendor
  SHOW GET m.Vendor
ENDIF
IF llVenRef .AND. SEEK(m.Vendor+m.Fabric+m.Color,'VENDMATL')
  m.LeadTime   = VENDMATL.LEADTIME

  *B122673,1 NNA 05/15/2004 (Begin) Replace the Price Field with the new name (NFABCOST)
  *m.nFabcost   = VENDMATL.PRICE
  m.nFabcost   = VENDMATL.NFABCOST
  *B122673,1 NNA (End)
  
  m.DDELIVDATE = gdSysDate + m.LeadTime
  m.CVENFAB    = VENDMATL.CVENFAB
  m.CVENCOLR   = VENDMATL.CVENCOLR
ELSE
  SELECT (lcTmpRmain)
  SCATTER FIELDS LEADTIME,NFABCOST,DDELIVDATE,CVENFAB,CVENCOLR MEMVAR
ENDIF
SHOW GET m.nFabCost
SHOW GET m.dDelivDate
=lfrefresh()
RETURN

*!*************************************************************
*! Name      : lfvEdtLoc
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate the location.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvEdtLoc()
*!*************************************************************
FUNCTION lfvEdtLoc

IF !SEEK(&lcPOTmpHD..cWareCode ,'WareHous' )
  SELECT (lcPOTmpHD)
  lcWarCd=gfBrowWare(.F.)
  *B604689,1 HBG 08/16/2001 Address1 field in POFHDR should be Address1 of the Warehouse not the description [Begin] 
  *REPLACE cWareCode WITH lcWarCd,;
          COUTADDR1 WITH WAREHOUS.CDESC,;
          COUTADDR2 WITH WAREHOUS.CADDRESS1,;
          COUTADDR3 WITH WAREHOUS.CADDRESS2,;
          COUTADDR4 WITH WAREHOUS.CADDRESS3,;
          COUTADDR5 WITH WAREHOUS.CADDRESS4
  REPLACE cWareCode WITH lcWarCd,;
          COUTADDR1 WITH WAREHOUS.CADDRESS1,;
          COUTADDR2 WITH WAREHOUS.CADDRESS2,;
          COUTADDR3 WITH WAREHOUS.CADDRESS3,;
          COUTADDR4 WITH WAREHOUS.CADDRESS4,;
          COUTADDR5 WITH WAREHOUS.CADDRESS5
  *B604689,1 [END]        
ENDIF  
RETURN


*!*************************************************************
*! Name      : lfvCurrency
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate currency in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvCurrency()
*!*************************************************************
FUNCTION lfvCurrency
PARA lcCurTyp

IF (lcCurTyp='P' AND cPriceCur = m.cPriceCur) OR ;
   (lcCurTyp='D' AND cDutyCur = m.cDutyCur)
  RETURN
ENDIF

lcCurrency = IIF(lcCurTyp='P',m.cPriceCur,m.cDutyCur)
=gfOpenFile(gcSysHome+'syccurr','Ccurrcode','SH')
=gfcurrbrow(@lcCurrency)
SELECT (lcTmpRmain)   
IF lcCurTyp='P'
  m.cPriceCur= IIF(EMPTY(lcCurrency),gcBaseCurr,lcCurrency)
  IF m.cPriceCur <> cPriceCur
    m.nPriceRat=gfChkRate('lnCurrUnt1',m.cPriceCur,gdSysDate,llEditExRt,gcAct_comp,.F.)
    m.nPriceRat=IIF(m.nPriceRat=0 ,1,m.nPriceRat)
    lnCurrUnt1 =IIF(lnCurrUnt1 =0 ,1,lnCurrUnt1)
  ENDIF
  SHOW GET m.cPriceCur
  SHOW GET m.nPriceRat
ELSE
  m.cDutyCur= IIF(EMPTY(lcCurrency),gcBaseCurr,lcCurrency)
  IF m.cDutyCur <> cDutyCur
    m.nDutyRat = gfChkRate('lnCurrUnt2',m.cDutyCur ,gdSysDate,llEditExRt,gcAct_comp,.F.)
    m.nDutyRat = IIF(m.nDutyRat = 0 ,1,m.nDutyRat)
    lnCurrUnt2 = IIF(lnCurrUnt2 = 0 ,1,lnCurrUnt2)
  ENDIF
  SHOW GET m.cDutyCur
  SHOW GET m.nDutyRat
ENDIF
=lfrefresh()
RETURN

*!*************************************************************
*! Name      : lfvOKFab
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Valid Ok in Edit -ve Remaining screen.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvOKFab()
*!*************************************************************
FUNCTION lfvOKFab

*B604689,1 HBG 08/16/2001 Comment these lines to Allow Price field to be empty.[Begin]
*IF m.nFabcost=0
*  *--Price field cannot be empty.
*  = gfModalGen('TRM36002B36000','DIALOG','Price')
*  RETURN
*ENDIF
*B604689,1 [END]

IF m.nFabTotQty=0
  *--Total quantity is 0 !! This line will not be included in the P/O.
  = gfModalGen('INM36127B36000','DIALOG')
  *B603818,1 (Begin) Get off the screen.
  *--Update Changes.
  GATHER MEMVAR
  CLEAR READ
  *B603818,1 (End)
  RETURN
ENDIF
llOk = .T.
CLEAR READ
RETURN

*!*************************************************************
*! Name        : lfvVendor
*! Developer   : Walid (wam)
*! Date        : 02/03/1999
*! Purpose     : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
*E301128,1 Add vendor validation.
FUNCTION lfvVendor
PRIVATE lnSelAls , lcVar
lnSelAls = SELECT(0)

SELECT APVENDOR
SET ORDER TO TAG VenCode 
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))   && Varible to hold the current field value

*IF !EMPTY(lcRPVendor) .AND. ;
   ('?' $ lcRPVendor .OR. !SEEK(lcRPVendor , 'APVENDOR'))

*C102819,1 AMH Validate vendors support material only [Start]
*IF !EMPTY(lcObj) .AND. ;
*   ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
*  =gfApVnBrow(@lcObj)
*ENDIF
IF !EMPTY(lcObj) .AND. ;
   ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR') .OR. !('M' $ APVENDOR.CVENSUPTYP))
  =gfApVnBrow(@lcObj,.F.,'M')
ENDIF
*C102819,1 AMH [End]

&lcVar = lcObj      && Update the field
SELECT (lnSelAls)
*--------------------- END OF lfvVendor

*!*************************************************************
*! Name        : lfDefForm
*! Developer   : Walid (wam)
*! Date        : 02/03/1999
*! Purpose     : Return form name (either O.T.S. or Requirement) Form
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Form Name
*!*************************************************************
*! Example     : = lfDefForm()
*!*************************************************************
*E301128,1 Return form name
FUNCTION lfDefForm
PRIVATE lcFormName
IF lcOgRepid = 'MAOTS'
  lcFormName = 'MAOTS'
ELSE
  lcFormName = 'MAMATRQ'
ENDIF
RETURN lcFormName

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 03/03/1999
*! Purpose   : Function that we call when Close the option grid.
*!           : to clear our variables.
*!*************************************************************
*! Called from : [Option Grid] < Close >
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*E301077,72 Close oppened files.
FUNCTION lfClearRep

PRIVATE laOpenFile,laTempFile,lnFilCnt
DIMENSION laOpenFile[12,1],laTempFile[5,1]
laOpenFile[1,1] = 'SYCCOMP'
laOpenFile[2,1] = 'CODES'
laOpenFile[3,1] = 'SCALE'
laOpenFile[4,1] = 'BOMLINE'
laOpenFile[5,1] = 'CTKTBOM'
laOpenFile[6,1] = 'POSHDR'
laOpenFile[7,1] = 'POSLN'
laOpenFile[8,1] = 'CUTTKTH'
laOpenFile[9,1] = 'CUTTKTL'
laOpenFile[10,1] = 'APVENDOR'
laOpenFile[11,1] = 'POFHDR'
laOpenFile[12,1] = 'POFLN'

*-- Close all open files. [Begin
FOR lnFilCnt = 1 TO ALEN(laOpenFile,1)
  IF USED(laOpenFile[lnFilCnt,1]) AND (ASCAN(laSelFile,laOpenFile[lnFilCnt,1]) = 0)
    USE IN (laOpenFile[lnFilCnt,1])
  ENDIF
ENDFOR
*-- Close all open files. [End

laTempFile[1,1] = 'lcTmpRmain'
laTempFile[2,1] = 'lcMatReq'
laTempFile[3,1] = 'lcTmpItems'
laTempFile[4,1] = 'lcPOTmpHD'
laTempFile[5,1] = 'lcPOTmpLN'

*-- Close all Temp. files. [Begin
FOR lnFilCnt = 1 TO ALEN(laTempFile,1)
  IF TYPE(laTempFile[lnFilCnt,1]) = 'C' AND USED(laTempFile[lnFilCnt,1])
    USE IN (laTempFile[lnFilCnt,1])
    IF FILE(gcWorkDir+laTempFile[lnFilCnt,1]+'.DBF')
      ERASE (gcWorkDir+laTempFile[lnFilCnt,1]+'.DBF')
    ENDIF
    IF FILE(gcWorkDir+laTempFile[lnFilCnt,1]+'.CDX')
      ERASE (gcWorkDir+laTempFile[lnFilCnt,1]+'.CDX')
    ENDIF
    IF FILE(gcWorkDir+laTempFile[lnFilCnt,1]+'.FPT')
      ERASE (gcWorkDir+laTempFile[lnFilCnt,1]+'.FPT')
    ENDIF
  ENDIF
ENDFOR
*-- Close all Temp. files. [End
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfGetRemn
*! Developer : Hossam El Etreby [HDM]
*! Date      : 01/11/2000
*! Purpose   : Function to get the remaining qty[By Fabric/Trim]
*!*************************************************************
*! Called from : MAMATRQF.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetRemn()
*!*************************************************************
*C200096,1
FUNCTION lfGetRemn

PRIVATE lnTmp

lnTmp     = lfRemain('O')
lnOTotRem = lnOTotRem + lnTmp

lnTmp     = lfRemain('H')
lnHTotRem = lnHTotRem + lnTmp

lnTmp     = lfRemain('P')
lnPTotRem = lnPTotRem + lnTmp

lnOIssued = lnOIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))
lnHIssued = lnHIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))
lnPIssued = lnPIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))

RETURN ''


*!*************************************************************
*! Name      : lfGetRemnC
*! Developer : Hossam El Etreby [HDM]
*! Date      : 01/11/2000
*! Purpose   : Function to get the remaining qty[By Color]
*!*************************************************************
*! Called from : MAMATRQC.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetRemnC()
*!*************************************************************
*C200096,1

FUNCTION lfGetRemnC

PRIVATE lnTmp

lnTmp     = lfRemain('O')
lnOTotRem = lnOTotRem + lnTmp

lnTmp     = lfRemain('H')
lnHTotRem = lnHTotRem + lnTmp

lnTmp     = lfRemain('P')
lnPTotRem = lnPTotRem + lnTmp

lnOIssued = lnOIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))
lnHIssued = lnHIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))
lnPIssued = lnPIssued + IIF(llOTC,IIF(lcBaseProj='P',0,-(EVAL(lcMatReq+'.nNetReq'))),-(EVAL(lcMatReq+'.nUsedReq')))


RETURN ''

*!*************************************************************
*! Name      : lfReset
*! Developer : Hossam El Etreby[HDM]
*! Date      : 01/11/2000
*! Purpose   : Function to RESET ISSUED AND REMAINING VARIABLES
*!*************************************************************
*! Called from : MAMATRQC.FRX & MAMATRQF.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfReset()
*!*************************************************************
*C200096,1

FUNCTION lfReset
STORE 0 TO lnOIssued , lnHIssued , lnPIssued , lnOTotRem , lnHTotRem , lnPTotRem 
return ''


*!**************************************************************************
*! Name      : lfSequence
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/06/2000
*! Purpose   : create a temp. sequence no for PO
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfSequence()
*!**************************************************************************
**E500349,1 
*!**************************************************************************
FUNCTION lfSequence
PRIVATE lnSequence
lnSequence = 0
SCAN
  lnSequence = MAX(lnSequence,VAL(POMAT))
ENDSCAN
=SEEK(SPACE(20)) 
RETURN PADL(ALLTRIM(STR(lnSequence+1)),6,"0")

*!**************************************************************************
*! Name      : lfvTmpPoMa
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/20/2000
*! Purpose   : validate MA po no enterd by user
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Parameters: llCheckAll = .T. if it called to chaeck all records in the file
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfSequence()
*!**************************************************************************
**E500349,1 
FUNCTION lfvTmpPoMa
PARAMETER llCheckAll
PRIVATE lnMessage
 
IF !llCheckAll
  lcTmpPoMat = EVALUATE(SYS(18))
  lcField    = SYS(18)
ELSE
  lcTmpPoMat = ''
  GO TOP
ENDIF
llRet=.T.
SCAN REST
  IF llCheckAll
    lcTmpPoMat = cTmpPoMat
  ENDIF
  lnRecNo = RECNO()
  COUNT TO lnRecord FOR cTmpPoMat = lcTmpPoMat
  Go Record lnRecNo
  IF lnRecord > 1  OR SEEK("P"+lcTmpPoMat,"POFHDR")
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34023B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF EMPTY(lcTmpPoMat)
    *-You cannot leave the Materila Po. number empty.
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34021B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF llRet AND LEN(ALLTRIM(lcTmpPoMat)) < 6
    *- "MA PO  number must be six digits.
    lnMessage = IIF(llCheckAll,0,gfModalGen('TRM34022B34000','DIALOG',"Material Po. "))
    llRet=.F.
  ENDIF
  IF !llCheckAll
    EXIT
  ENDIF
ENDSCAN
IF !llRet .AND. !llCheckAll
  lcTmpPoMat = ''
  REPLACE &lcField WITH lcTmpPoMat
  KEYBOARD CHR(15)
  llRet = .T.
ENDIF
RETURN llRet

*!*************************************************************
*! Name      : lfBrow
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : Browse function for the Generated Po's Screen.
*! Ref       : B803005,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkPO()
*!*************************************************************
FUNCTION lfBrow

lcFields = IIF(llGENMAPON,"pomat :R ","cTmpPoMat :P='@! XXXXXX' :V=lfvTmpPoMa()")+":H='P/O' ,Vendor :R :15,"+;
            "lcVenName=IIF(SEEK(Vendor,'APVENDOR'),APVENDOR.CVENCOMP,'') :H='Vendor name' : 40,"+;
            "cWareCode :P='@! XXXXXX' :H='Shipto Location' :V=lfvEdtLoc()"
         
*C102819,1 AMH Add customer field for Dream Apparel [Start]
=gfDoTriger('MAMATRQ',PADR('MATACCT',10))
*C102819,1 AMH [End]

BROWSE FIELDS &lcFields                     ;
         WINDOW (lcbrwcont)                 ;  
         IN WINDOW (lcBsWind)               ;
         LOCK 0                             ;
         NOMENU                             ;         
         NOAPPEND                           ;
         NOWAIT                             ;
         SAVE                               ;
         NOCLEAR                            ;
         NODELETE                           ;
         TITLE  (lcBrwTitl)  
   

 


*!*************************************************************
*! Name      : lfvOkPO
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : Valid function for the ok button in the generated Po's screen.
*! Ref       : B803005,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkPO()
*!*************************************************************
FUNCTION lfvOkPO

IF !llGENMAPON .AND. !lfvTmpPoMa(.T.)
  *-Material Po. Number have not been defined in some generates Pos.Please enter the Material Po.Number.
  = gfModalGen('INM36172B36000','DIALOG')
  RETURN
ENDIF

*--Check the warehous is not empty.
IF llWareHous
  LOCATE FOR EMPTY(cWareCode)
  IF FOUND()
    *-Some generated P/Os has not defined shipto location, Please enter the shipto location.
    = gfModalGen('INM36124B36000','DIALOG')
    RETURN   
  ELSE
    CLEAR READ
  ENDIF
ENDIF  



*!*************************************************************
*! Name      : lfTrap
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : Trap the Tab ,BackTab 
*! Ref       : B803005,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTrap()
*!*************************************************************
FUNCTION lfTrap

IF WONTOP() = lcBrwTitl 
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpBackTab	  
ENDIF

RETURN .F.
*!*************************************************************
*! Name      : lfClrTrap
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : Clear the  Trap
*! Ref       : B803005,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrTrap()
*!*************************************************************
FUNCTION lfClrTrap

ON KEY LABEL TAB
ON KEY LABEL BACKTAB


*!*************************************************************
*! Name      : lpTab
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : the Tab Key
*! Ref       : B803005,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpTab()
*!*************************************************************

PROCEDURE lpTab
ON KEY LABEL TAB
IF WONTOP() = lcBrwTitl  
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbOk)  
ENDIF
*!*************************************************************
*! Name      : lpBackTab
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : The BackTab Key
*! Ref       : B803005,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpBackTab()
*!*************************************************************

PROCEDURE lpBackTab
ON KEY LABEL BACKTAB
IF WONTOP()=lcBrwTitl
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbOk) 
ENDIF

*!*************************************************************
*! Name      : lpEsc
*! Developer : NAD
*! Date      : 09/20/2000
*! Purpose   : The Escape Key
*! Ref       : B803005,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .F.
*!*************************************************************
*! Example            :  =lpEsc()
*!*************************************************************

PROCEDURE lpEsc

RETURN .F.

*B605049,1 WAB (Start)
*!*************************************************************
*! Name      : lfGtWipPln
*! Developer : WAB - Walid A. Wahab
*! Date      : 11/10/2001
*! Purpose   : caclculate the WIP for specific PO/CT
*! Ref       : B803005,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .F.
*!*************************************************************
*! Example            :  =lfGtWipPln()
*!*************************************************************
FUNCTION lfGtWipPln
  lnCurRec = RECNO()
  lcTranNo = IIF(llPrMake,Cuttkt,Po) 
  IF llPrMake
    SCAN WHILE Style=lcPrStyle FOR CutTkt = lcTranNo AND &lcForWare
      FOR I = 1 to 8
        Z=STR(I,1)
        IF gfDoTriger('MAMATRQ',PADR('CLCWIPGMA',10))
           laWipPln[I]=IIF(TranCd = '1', (laWipPln[I] + MAX(Qty&Z,0)) , laWipPln[I])
        ELSE
          laWipPln[I]=IIF(TranCd = '1', (laWipPln[I] + MAX(Qty&Z,0)) ,;
                            (MAX(laWipPln[I]-ABS(Qty&Z),0) ) )
        ENDIF
      ENDFOR
    ENDSCAN
  ELSE
    SCAN WHILE Style=lcPrStyle FOR Po = lcTranNo AND !(TranCd $ '36') AND &lcForWare
    
      lcSign  = IIF(cStyType='R','-','+')
      lcOSign = IIF(cStyType='R','+','-')
      FOR I=1 TO 8
        Z=STR(I,1)
        IF gfDoTriger('MAMATRQ',PADR('CLCWIPGMA',10))
          laWipPln[I]=IIF(TranCd = '1', (laWipPln[I] &lcSign  MAX(Qty&Z,0)) , laWipPln[I])
        ELSE
          laWipPln[I]=IIF(TranCd = '1', (laWipPln[I] &lcSign MAX(Qty&Z,0)) ,;
                           (MAX(laWipPln[I] &lcOSign ABS(Qty&Z),0) ) )
        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
  FOR I=1 TO 8      
   laWipPln[9]=laWipPln[9]+laWipPln[I]
  ENDFOR
  GO lnCurRec
RETURN
*B605049,1 WAB (End)

*!*************************************************************
*! Name      : lfStySum
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/08/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
*E301943,1 AMH
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/08/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*E301943,1 AMH
FUNCTION lfSRSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfvReqBase
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/04/2002
*! Purpose   : Validate Requirement Based on option
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvReqBase()
*!*************************************************************
*C102741,1 AMH
FUNCTION lfvReqBase

IF lcRpReqBas <> 'W'
  llRpByWare = .F.
ENDIF
CLEAR READ
*-- end of lfvReqBase.

*!*************************************************************
*! Name      : lfWipReq
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/05/2002
*! Purpose   : Calculate the required in case of WIP
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWipReq()
*!*************************************************************
*C102741,1 AMH
FUNCTION lfWipReq

PRIVATE lnAlias,lcHdrFile,lcLineFile,lcTranFld,lcNewTran

*B607082,1 AMH Add variable to save the line number [Start]
PRIVATE lnLineNo,lcStatus
*B607082,1 AMH [End]

lnAlias = SELECT(0)
IF STYLE.Make
  lcHdrFile  = 'CUTTKTH'
  lcLineFile = 'CUTTKTL'
  lcTranFld  = 'CUTTKTL.CUTTKT'
ELSE
  lcHdrFile  = 'POSHDR'
  lcLineFile = 'POSLN'
  lcTranFld  = 'POSLN.PO'
ENDIF
lcNewTran = SPACE(6)

*B607082,1 AMH Hold the inisial value of the line number [Start]
lnLineNo = 0
lcStatus = SPACE(1)
*B607082,1 AMH [End]

SELECT (lcLineFile)
SEEK lcCStyle
SCAN WHILE Style=lcCStyle FOR EVALUATE(lcHdrFile+'.Status') $ 'AOH' .AND.;
                              IIF(STYLE.MAKE,.T.,CSTYTYPE $ 'PRN' .AND.;
                                  !(TRANCD $ '36')) .AND. &lcForWare
  lcSign  = IIF(!STYLE.MAKE .AND. cStyType='R','-','+')
  lcOSign = IIF(!STYLE.MAKE .AND. cStyType='R','+','-')
  IF EVALUATE(lcTranFld) # lcNewTran
    
    *B607082,1 AMH Update the lcMatReq file after get the requirements [Start]
    *IF !lfUpdWipRq()
    *  LOOP
    *ENDIF
    *B607082,1 AMH [End]
    
    IF !EMPTY(lcNewTran)
      
      *B607082,1 AMH Pass the transaction number and line number to get the correct req. [Start]
      *=lfGetWipRq()
      =lfGetWipRq(lcNewTran,lnLineNo,lcStatus)
      *B607082,1 AMH [End]
      
    ENDIF
    lcNewTran = EVALUATE(lcTranFld)
    
    *B607082,1 AMH Save the line number [Start]
    lnLineNo = EVALUATE(lcLineFile+'.LINENO')
    lcStatus = EVALUATE(lcHdrFile+'.STATUS')
    SELECT (lcLineFile)
    IF !lfUpdWipRq()
      LOOP
    ENDIF
    *B607082,1 AMH [End]
    
  ENDIF
  
  FOR I=1 TO lnCnt      
    Z=STR(I,1)
    DO CASE
      CASE EVALUATE(lcHdrFile+'.Status') = 'A'
        laWIP[1,I]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[1,I] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) ,;
                       (MAX(laWIP[1,I] &lcOSign. ABS(EVALUATE(lcLineFile+'.Qty'+Z)),0) ) )
        laWIP[1,9]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[1,9] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) ,;
                       (MAX(laWIP[1,9] &lcOSign. ABS(EVALUATE(lcLineFile+'.Qty'+Z)),0) ) )
      CASE EVALUATE(lcHdrFile+'.Status') = 'O'      
        IF gfDoTriger('MAMATRQ',PADR('CLCWIPGMA',10))
          laWIP[2,I]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[2,I] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) , laWIP[2,I])
          laWIP[2,9]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[2,9] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) , laWIP[2,9])
        ELSE
          laWIP[2,I]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[2,I] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) ,;
                         (MAX(laWIP[2,I] &lcOSign. ABS(EVALUATE(lcLineFile+'.Qty'+Z)),0) ) )
          laWIP[2,9]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[2,9] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) ,;
                         (MAX(laWIP[2,9] &lcOSign. ABS(EVALUATE(lcLineFile+'.Qty'+Z)),0) ) )
        ENDIF
      CASE EVALUATE(lcHdrFile+'.Status') = 'H'
        laWIP[3,I]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[3,I] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) ,;
                       (MAX(laWIP[3,I] &lcOSign. ABS(EVALUATE(lcLineFile+'.Qty'+Z)),0) ) )
        laWIP[3,9]=IIF(EVALUATE(lcLineFile+'.TranCd') = '1', (laWIP[3,9] &lcSign. MAX(EVALUATE(lcLineFile+'.Qty'+Z),0)) ,;
                       (MAX(laWIP[3,9] &lcOSign. ABS(EVALUATE(lcLineFile+'.Qty'+Z)),0) ) )
    ENDCASE
  ENDFOR
ENDSCAN
IF !EMPTY(lcNewTran)
  
  *B607082,1 AMH Pass the transaction number and line number to get the correct req. [Start]
  *=lfGetWipRq()
  =lfGetWipRq(lcNewTran,lnLineNo,lcStatus)
  *B607082,1 AMH [End]
  
ENDIF

SELECT (lnAlias)
RETURN
*-- end of lfWipReq.

*!*************************************************************
*! Name      : lfUpdWipRq
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/18/2002
*! Purpose   : Calculate the required in case of WIP
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdWipRq()
*!*************************************************************
*C102741,1 AMH
FUNCTION lfUpdWipRq

IF EVALUATE(lcHdrFile+'.STATUS') <> 'H'
  IF SEEK(IIF(STYLE.MAKE,'M1'+CUTTKT,'I1'+PO)+STR(LINENO,6),'BOMLINE')
    SELECT (lcMatReq)
    IF llRpByWare
      SET ORDER TO TAG MRFABLOC
    ELSE
      SET ORDER TO TAG MATREQ
    ENDIF
    SELECT BOMLINE
    SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                    IIF(STYLE.MAKE,'M','I')+'1'+EVALUATE(lcLineFile+IIF(STYLE.MAKE,'.CUTTKT','.PO'))+;
                    STR(EVALUATE(lcLineFile+'.LINENO'),6);
                FOR STYLE = lcCStyle .AND. cCatgTyp $ IIF(lcRpCompn='A','FTS',lcRpCompn)
      =SEEK(CIMTYP+CTKTNO+CBOMTYP+ITEM+ICLR,'CTKTBOM')
      IF llRpByWare .AND. CCATGTYP $ 'FT'
        llFound = !EMPTY(COPRCODE) .AND. SEEK(CIMTYP+CTKTNO+COPRCODE,'MFGOPRHD') .AND.;
                  !EMPTY(MFGOPRHD.CCONTCODE) .AND. SEEK(MFGOPRHD.CCONTCODE,'APVENDOR') .AND.;
                  !EMPTY(APVENDOR.CWARECODE)
        IF llByFabLoc
          IF llFound
            IF APVENDOR.CWARECODE <> lcFLoctn
              LOOP
            ENDIF
          ELSE
            LOOP
          ENDIF
        ENDIF
      ENDIF
      
      *B607082,1 AMH Add only fabrics/trims in the selection critria [Start]
      *IF !SEEK(CCATGTYP+IIF(llRpByWare .AND. CCATGTYP $ 'FT',;
               IIF(llFound,'N'+APVENDOR.CWARECODE,'Y'+SPACE(6)),'')+ITEM+ICLR+STYLE,lcMatReq)
      IF !SEEK(CCATGTYP+IIF(llRpByWare .AND. CCATGTYP $ 'FT',;
               IIF(llFound,'N'+APVENDOR.CWARECODE,'Y'+SPACE(6)),'')+ITEM+ICLR+STYLE,lcMatReq);
         .AND. IIF(CCATGTYP $ 'FT',EVALUATE(lcRpExp2),.T.)
      *B607082,1 AMH [End]
      
        SELECT (lcMatReq)
        APPEND BLANK
        
        *B607082,1 AMH Update the correct style style (Make .T./.F.) [Start]
        *REPLACE Style     WITH BOMLINE.STYLE       ,;
                Typ       WITH BOMLINE.CBOMTYP     ,;
                cCatgTyp  WITH BOMLINE.cCatgTyp    ,;
                Item      WITH BOMLINE.ITEM        ,;
                IClr      WITH BOMLINE.ICLR        ,;
                Desc      WITH CTKTBOM.Desc        ,;
                Uom       WITH CTKTBOM.Uom         ,;
                Cnt       WITH IIF(lnCnt=0,8,lnCnt),;
                lStyMake  WITH .T.                 ,;
                LGETTHIS  WITH .T.                 ,;
                cWareCode WITH IIF(llByLoctn,lcLocation,"")
        REPLACE Style     WITH BOMLINE.STYLE       ,;
                Typ       WITH BOMLINE.CBOMTYP     ,;
                cCatgTyp  WITH BOMLINE.cCatgTyp    ,;
                Item      WITH BOMLINE.ITEM        ,;
                IClr      WITH BOMLINE.ICLR        ,;
                Desc      WITH CTKTBOM.Desc        ,;
                Uom       WITH CTKTBOM.Uom         ,;
                Cnt       WITH IIF(lnCnt=0,8,lnCnt),;
                lStyMake  WITH STYLE.MAKE          ,;
                LGETTHIS  WITH .T.                 ,;
                cWareCode WITH IIF(llByLoctn,lcLocation,"")
        *B607082,1 AMH [End]
        
        IF llRpByWare .AND. CCATGTYP $ 'FT'
          IF llFound
            REPLACE CFABLOC  WITH APVENDOR.CWARECODE,;
                    CINHOUSE WITH 'N'
          ELSE
            REPLACE CINHOUSE WITH 'Y'
          ENDIF
        ENDIF
        FOR I=1 TO lnCnt
          lcSz = STR(I,1)
          IF lcSz $ BOMLINE.CSIZES
            REPLACE Qty&lcSz WITH BOMLINE.UNITQTY
          ENDIF
        ENDFOR
      ELSE
        SELECT (lcMatReq)
        REPLACE LGETTHIS WITH .T.
      ENDIF
    ENDSCAN
  ELSE
    RETURN .F.
  ENDIF
ENDIF
FOR lnXLoop = 1 TO 3
  FOR lnYLoop = 1 TO 9
    laWIP[lnXLoop,lnYLoop] = 0
  ENDFOR
ENDFOR
RETURN .T.
*-- end of lfUpdWipRq.

*!*************************************************************
*! Name      : lfGetWipRq
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/18/2002
*! Purpose   : Calculate the required in case of WIP
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetWipRq()
*!*************************************************************
*C102741,1 AMH
FUNCTION lfGetWipRq

*B607082,1 AMH Add parameters to hold the transaction number and line number [Start]
PARAMETERS lcNewTran,lnLineNo,lcStatus
*B607082,1 AMH [End]

SELECT (lcMatReq)
SET ORDER TO TAG MRSTYITM
SEEK lcCStyle
SCAN REST WHILE STYLE = lcCStyle
  
  *B607082,1 AMH Get the correct status [Start]
  *IF !LGETTHIS .AND. IIF(EVALUATE(lcHdrFile+'.STATUS')='H',!LBOM,.T.)
  IF !LGETTHIS .AND. IIF(lcStatus='H',!LBOM,.T.)
  *B607082,1 AMH [End]
  
    LOOP
  ENDIF
  
  *B607082,1 AMH Seek by passed parameters since it hold the correct values [Start]
  *=SEEK(IIF(STYLE.MAKE,'M1'+EVALUATE(lcLineFile+'.CUTTKT'),'I1'+EVALUATE(lcLineFile+'.PO'))+;
        STR(EVALUATE(lcLineFile+'.LINENO'),6)+TYP+lcCStyle+SPACE(6)+ITEM+ICLR,'BOMLINE')
  =SEEK(IIF(STYLE.MAKE,'M1','I1')+lcNewTran+;
        STR(lnLineNo,6)+TYP+lcCStyle+SPACE(6)+ITEM+ICLR,'BOMLINE')
  *B607082,1 AMH [End]
  
  FOR I=1 TO lnCnt
    lcSz = STR(I,1)
    IF EVALUATE('OHREQ'+lcSz) <> 0
      IF lcSz $ BOMLINE.CSIZES
        REPLACE YIELD&lcSz WITH (EVALUATE('YIELD'+lcSz+'*OHREQ'+lcSz)+;
                              (BOMLINE.UNITQTY*laRequr[2,I]))/;
                              (EVALUATE('OHREQ'+lcSz)+laRequr[2,I])
      ELSE
        
        *B607082,1 AMH Get the correct status [Start]
        *IF EVALUATE(lcHdrFile+'.Status') <> 'H'
        IF lcStatus <> 'H'
        *B607082,1 AMH [End]
        
          REPLACE YIELD&lcSz WITH EVALUATE('YIELD'+lcSz+'*OHREQ'+lcSz)/;
                                (EVALUATE('OHREQ'+lcSz)+laRequr[2,I])
        ELSE
          REPLACE YIELD&lcSz WITH (EVALUATE('YIELD'+lcSz+'*OHREQ'+lcSz)+;
                                (EVALUATE('QTY'+lcSz)*laRequr[2,I]))/;
                                (EVALUATE('OHREQ'+lcSz)+laRequr[2,I])
        ENDIF
      ENDIF
    ELSE
      IF lcSz $ BOMLINE.CSIZES
        REPLACE YIELD&lcSz WITH BOMLINE.UNITQTY
      ENDIF
    ENDIF
  ENDFOR
  
  *--Calculate Requirements Pieces per size.
  FOR lnXLoop = 1 TO 3
    FOR lnYLoop = 1 TO 9
      laRequr[lnXLoop,lnYLoop] = 0
    ENDFOR
  ENDFOR
  FOR I=1 TO lnCnt
    Z=STR(I,1)
    *--[1] Requirements Pieces for OPEN orders.
    laRequr[1,I] = MAX( laWIP[1,I]+laWIP[2,I] , 0 )
    laRequr[1,9] = laRequr[1,9] + laRequr[1,I]
    
    *--[2] Requirements Pieces for OPEN+HOLD orders.
    laRequr[2,I] = MAX( laWIP[1,I]+laWIP[2,I]+laWIP[3,I] , 0 )
    laRequr[2,9] = laRequr[2,9] + laRequr[2,I]
    
    *--[3] Requirements Pieces for Projection (dependent).
    *--Compute the Projection (Depends on requirements calculated before).
    *--or depens on plane if select projection for plane.
    DO CASE
      *--[1] Projection base on Open.
      CASE lcRpBasePj = 'O'
        laRequr[3,I] = laRequr[1,I] + ROUND(laRequr[1,I] * (lnRpPrIncr/100),0)
      *--[2] Projection base on Open+Hold.
      CASE lcRpBasePj = 'H'
        laRequr[3,I] = laRequr[2,I] + ROUND(laRequr[2,I] * (lnRpPrIncr/100),0)
      *--[3] Requirements Pieces for Plane.
      CASE lcRpBasePj = 'P'
        laRequr[3,I] = IIF(STYLE.Plan&Z > 0, STYLE.Plan&Z + ROUND(STYLE.Plan&Z*((lnRpPrIncr/100)),0), 0)

        *C200451,1 ABD - Add new triger for SHI to get the plan Qty From the Forcast table. [Begin]
        = gfDoTriger('MAMATRQ',PADR('GETPRDPL',10))
        *C200451,1 ABD - [End]
        
    ENDCASE
    laRequr[3,9] = laRequr[3,9] + laRequr[3,I]
  ENDFOR
  
  REPLACE OReq1    WITH OReq1 + laRequr[1,1],;
          OReq2    WITH OReq2 + laRequr[1,2],;
          OReq3    WITH OReq3 + laRequr[1,3],;
          OReq4    WITH OReq4 + laRequr[1,4],;
          OReq5    WITH OReq5 + laRequr[1,5],;
          OReq6    WITH OReq6 + laRequr[1,6],;
          OReq7    WITH OReq7 + laRequr[1,7],;
          OReq8    WITH OReq8 + laRequr[1,8],;
          OReqTot  WITH OReqTot + laRequr[1,9],;
          OHReq1   WITH OHReq1 + laRequr[2,1],;
          OHReq2   WITH OHReq2 + laRequr[2,2],;
          OHReq3   WITH OHReq3 + laRequr[2,3],;
          OHReq4   WITH OHReq4 + laRequr[2,4],;
          OHReq5   WITH OHReq5 + laRequr[2,5],;
          OHReq6   WITH OHReq6 + laRequr[2,6],;
          OHReq7   WITH OHReq7 + laRequr[2,7],;
          OHReq8   WITH OHReq8 + laRequr[2,8],;
          OHReqTot WITH OHReqTot + laRequr[2,9],;
          nProj1   WITH nProj1 + laRequr[3,1],;
          nProj2   WITH nProj2 + laRequr[3,2],;
          nProj3   WITH nProj3 + laRequr[3,3],;
          nProj4   WITH nProj4 + laRequr[3,4],;
          nProj5   WITH nProj5 + laRequr[3,5],;
          nProj6   WITH nProj6 + laRequr[3,6],;
          nProj7   WITH nProj7 + laRequr[3,7],;
          nProj8   WITH nProj8 + laRequr[3,8],;
          Project  WITH Project + laRequr[3,9]
  
  *--Update the open WIP pieces.
  REPLACE nOpnWip1 WITH nOpnWip1 + laWIP[2,1],;
          nOpnWip2 WITH nOpnWip2 + laWIP[2,2],;
          nOpnWip3 WITH nOpnWip3 + laWIP[2,3],;
          nOpnWip4 WITH nOpnWip4 + laWIP[2,4],;
          nOpnWip5 WITH nOpnWip5 + laWIP[2,5],;
          nOpnWip6 WITH nOpnWip6 + laWIP[2,6],;
          nOpnWip7 WITH nOpnWip7 + laWIP[2,7],;
          nOpnWip8 WITH nOpnWip8 + laWIP[2,8],;
          nOpnWip  WITH nOpnWip + laWIP[2,9],;
          LGETTHIS WITH .F.
ENDSCAN
SET ORDER TO TAG MRStyle
SEEK lcCStyle
*-- end of lfGetWipRq.

*!*************************************************************
*! Name      : lfGetNetRq
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/23/2002
*! Purpose   : Calculate the net required
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetNetRq()
*!*************************************************************
*B606664,4 AMH
FUNCTION lfGetNetRq

PRIVATE lnAlias
lnAlias = SELECT(0)
STORE 0 TO lnNRequ
IF SEEK(lcTktType+'1'+lcTktk+lcSLineNo+lcPrTyp+lcPrStyle+SPACE(6)+lcPrItem+lcPrIClr,'BOMLINE')
  SELECT BOMLINE
  SUM REST ITEMQTY TO lnNRequ;
           WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
                 lcTktType+'1'+lcTktk+lcSLineNo+lcPrTyp+lcPrStyle+SPACE(6)+lcPrItem+lcPrIClr
ENDIF
lnNRequ = MAX(lnNRequ-lnIssue,0)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUpdNetRq
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/23/2002
*! Purpose   : Update the net required by style 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdNetRq()
*!*************************************************************
*B606664,4 AMH
FUNCTION lfUpdNetRq

lcWrkLnFl=IIF(llPrMake,'CUTTKTL','POSLN')
lcWrkHdFl=IIF(llPrMake,'CUTTKTH','POSHDR')
IF !SEEK(lcPrStyle,lcWrkLnFl)
  IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
    SELECT (lcMatReq)
    GO lnXRecNo
    SET ORDER TO TAG MatreqC
    SET KEY TO cCatgTyp
    GO lnXRecNo
  ENDIF
  RETURN
ENDIF
lcSvTkt = "  "
lnAlias = SELECT(0)
SELECT (lcWrkLnFl)

SCAN WHILE Style = lcPrStyle FOR &lcWrkHdFl..Status $ 'O' AND TranCd = '1'
  *--Tiket type.
  lcTktType = IIF(llPrMake,'M','I') 
  *--C/t no or P/o no.
  lcTktk    = IIF(llPrMake,Cuttkt,Po) 
  *--Line no.
  lcSLineNo = STR(&lcWrkLnFl..LineNo,6)
  SELECT BOMLINE 
  *--Seek on full expresion in BomLine.
  *-cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
  lcBomLnKey = lcTktType+'1'+lcTktk+lcSLineNo+lcPrTyp+lcPrStyle+SPACE(6)+lcPrItem+lcPrIClr+SPACE(6)
  IF SEEK(lcBomLnKey)
    IF llRpByWare .AND. CCATGTYP $ 'FT'
      llFound = !EMPTY(COPRCODE) .AND. SEEK(CIMTYP+CTKTNO+COPRCODE,'MFGOPRHD') .AND.;
                !EMPTY(MFGOPRHD.CCONTCODE) .AND. SEEK(MFGOPRHD.CCONTCODE,'APVENDOR') .AND.;
                !EMPTY(APVENDOR.CWARECODE)
      IF llFound
        IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'N'+APVENDOR.CWARECODE
          LOOP
        ENDIF
      ELSE
        IF EVALUATE(lcMatReq+'.CINHOUSE+'+lcMatReq+'.CFABLOC') <> 'Y'+SPACE(6)
          LOOP
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  SELECT CTKTBOM
  IF SEEK(lcTktType+lcTktk+lcPrTyp+lcPrItem+lcPrIClr)
    IF !(EVAL(lcMatReq+'.cCatgTyp') = 'S')
      
      *B607082,1 AMH Fix bug of vabriable lnIssue/lnNRequ not found [Start]
      STORE 0 TO lnIssue,lnNRequ
      *B607082,1 AMH [End]
      
      =lfGetNetRq()
      SELECT (lcMatReq)
      lnSavRec = RECNO()
      lcSekKey = IIF(llRpByWare,cCatgTyp+cInHouse+cFabLoc+Item+IClr,cCatgTyp+Item+IClr)
      lcWhrCnd = IIF(llRpByWare,'cCatgTyp+cInHouse+cFabLoc+Item+IClr','cCatgTyp+Item+IClr')+;
                 '=lcSekKey'
      SEEK lcSekKey
      SCAN REST WHILE &lcWhrCnd.
        IF !(lcTktType+lcTktk  $ PoCt )
          REPLACE nNetReq  WITH nNetReq  + lnNRequ,;
                  PoCt 	 WITH poCt + lcTktType+lcTktk+','
        ENDIF
      ENDSCAN
      GOTO lnSavRec
    ENDIF
  ENDIF
  lcSvTkt = lcTktk
ENDSCAN

IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
  SELECT (lcMatReq)
  GO lnXRecNo
  SET ORDER TO TAG MatreqC
  SET KEY TO cCatgTyp
  GO lnXRecNo
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfSaveMrp
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Save the MRP report.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSaveMrp()
*!*************************************************************
*C102819,1 AMH

FUNCTION lfSaveMrp

PRIVATE lnAlias,lnReccount,lnCurNum,lnFltCnt,lnI,lcOrder
PRIVATE lnSep,lcvEntries,lnvEntries,lavEntries,lcVarType,lcFieldNam
lnAlias = SELECT(0)

*-- Update the header file.
SELECT MRPHDR
APPEND BLANK
lcMrp = gfSequence('CMRP')
REPLACE CMRP WITH lcMrp
SAVE TO MEMO MFLTVAL ALL LIKE l?Rp*
=gfAdd_Info('MRPHDR')

*-- Update the lines file.
SELECT (lcMatReq)
LOCATE

*--Thermometer counter.
lnRecCount = RECCOUNT()
lnCurNum   = 0
SCAN
  lnCurNum = lnCurNum + 1
  =gfThermo(lnRecCount,lnCurNum ,"Save the MRP data",'MRP # : '+lcMrp)
  SCATTER MEMO MEMVAR
  SELECT MRPLN
  APPEND BLANK
  REPLACE CMRP      WITH lcMrp      ,;
          STYLE     WITH M.STYLE    ,;
          ITEM      WITH M.ITEM     ,;
          ICLR      WITH M.ICLR     ,;
          DESC      WITH M.DESC     ,;
          CWARECODE WITH M.CWARECODE,;
          TYP       WITH M.TYP      ,;
          CCATGTYP  WITH M.CCATGTYP ,;
          CNT       WITH M.CNT      ,;
          UOM       WITH M.UOM
  REPLACE NQTY1     WITH M.QTY1     ,;
          NQTY2     WITH M.QTY2     ,;
          NQTY3     WITH M.QTY3     ,;
          NQTY4     WITH M.QTY4     ,;
          NQTY5     WITH M.QTY5     ,;
          NQTY6     WITH M.QTY6     ,;
          NQTY7     WITH M.QTY7     ,;
          NQTY8     WITH M.QTY8
  REPLACE NOREQ1    WITH M.OREQ1    ,;
          NOREQ2    WITH M.OREQ2    ,;
          NOREQ3    WITH M.OREQ3    ,;
          NOREQ4    WITH M.OREQ4    ,;
          NOREQ5    WITH M.OREQ5    ,;
          NOREQ6    WITH M.OREQ6    ,;
          NOREQ7    WITH M.OREQ7    ,;
          NOREQ8    WITH M.OREQ8    ,;
          NOREQTOT  WITH M.OREQTOT
  REPLACE NOHREQ1   WITH M.OHREQ1   ,;
          NOHREQ2   WITH M.OHREQ2   ,;
          NOHREQ3   WITH M.OHREQ3   ,;
          NOHREQ4   WITH M.OHREQ4   ,;
          NOHREQ5   WITH M.OHREQ5   ,;
          NOHREQ6   WITH M.OHREQ6   ,;
          NOHREQ7   WITH M.OHREQ7   ,;
          NOHREQ8   WITH M.OHREQ8   ,;
          NOHREQTOT WITH M.OHREQTOT
  REPLACE NPROJ1    WITH M.NPROJ1   ,;
          NPROJ2    WITH M.NPROJ2   ,;
          NPROJ3    WITH M.NPROJ3   ,;
          NPROJ4    WITH M.NPROJ4   ,;
          NPROJ5    WITH M.NPROJ5   ,;
          NPROJ6    WITH M.NPROJ6   ,;
          NPROJ7    WITH M.NPROJ7   ,;
          NPROJ8    WITH M.NPROJ8   ,;
          NPROJECT  WITH M.PROJECT
  REPLACE NOPNWIP   WITH M.NOPNWIP  ,;
          NYTOWIP   WITH M.NYTOWIP  ,;
          NUSEDREQ  WITH M.NUSEDREQ ,;
          NNETREQ   WITH M.NNETREQ  ,;
          LSTYMAKE  WITH M.LSTYMAKE ,;
          NNETREQ1  WITH M.NNETREQ1 ,;
          NNETREQ2  WITH M.NNETREQ2 ,;
          NNETREQ3  WITH M.NNETREQ3 ,;
          NNETREQ4  WITH M.NNETREQ4 ,;
          NNETREQ5  WITH M.NNETREQ5 ,;
          NNETREQ6  WITH M.NNETREQ6 ,;
          NNETREQ7  WITH M.NNETREQ7 ,;
          NNETREQ8  WITH M.NNETREQ8
  REPLACE NOPNWIP1  WITH M.NOPNWIP1 ,;
          NOPNWIP2  WITH M.NOPNWIP2 ,;
          NOPNWIP3  WITH M.NOPNWIP3 ,;
          NOPNWIP4  WITH M.NOPNWIP4 ,;
          NOPNWIP5  WITH M.NOPNWIP5 ,;
          NOPNWIP6  WITH M.NOPNWIP6 ,;
          NOPNWIP7  WITH M.NOPNWIP7 ,;
          NOPNWIP8  WITH M.NOPNWIP8
  REPLACE NYTOWIP1  WITH M.NYTOWIP1 ,;
          NYTOWIP2  WITH M.NYTOWIP2 ,;
          NYTOWIP3  WITH M.NYTOWIP3 ,;
          NYTOWIP4  WITH M.NYTOWIP4 ,;
          NYTOWIP5  WITH M.NYTOWIP5 ,;
          NYTOWIP6  WITH M.NYTOWIP6 ,;
          NYTOWIP7  WITH M.NYTOWIP7 ,;
          NYTOWIP8  WITH M.NYTOWIP8 ,;
          MPOCT     WITH M.PoCt     ,;
          MFLTVAL   WITH M.MFLTVAL
  REPLACE CFABLOC   WITH M.CFABLOC  ,;
          CINHOUSE  WITH M.CINHOUSE ,;
          LGETTHIS  WITH M.LGETTHIS ,;
          LBOM      WITH M.LBOM     ,;
          NYIELD1   WITH M.YIELD1   ,;
          NYIELD2   WITH M.YIELD2   ,;
          NYIELD3   WITH M.YIELD3   ,;
          NYIELD4   WITH M.YIELD4   ,;
          NYIELD5   WITH M.YIELD5   ,;
          NYIELD6   WITH M.YIELD6   ,;
          NYIELD7   WITH M.YIELD7   ,;
          NYIELD8   WITH M.YIELD8
  REPLACE ORDER     WITH M.ORDER    ,;
          COMPLETE  WITH M.COMPLETE ,;
          ONHAND    WITH M.ONHAND   ,;
          ONORDER   WITH M.ONORDER  ,;
          ETA       WITH M.ETA      ,;
          LMULTIPO  WITH M.LMULTIPO ,;
          LNOTPRINT WITH M.LNOTPRINT,;
          ITEMSTK   WITH M.ITEMSTK
  =gfAdd_Info('MRPLN')
ENDSCAN
=gfThermo(100,100)

*-- Update the OG selections.
SELECT SYREPUVR
lcOrder = SET('ORDER')
SET ORDER TO TAG CREP_ID

*--Thermometer counter.
lnFltCnt = ALEN(laOgObjType,1)

FOR lnI = 1 TO lnFltCnt
  =gfThermo(lnFltCnt,lnI ,"Save the OG selections",'MRP # : '+lcMrp)
  SELECT MRPFLT
  APPEND BLANK
  REPLACE CMRP WITH lcMrp,;
          MFLD_NAME WITH IIF(UPPER(laOgObjType[lnI,1])='LAOGFXFLT',;
                             EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','1]')),laOgObjType[lnI,1])
                             
  *-- Get the filter header and value.
  *E037351,1 HBG 23/12/2003 Add Flag to know if the record found in SYREPUVR [Begin]
  llNotFound = .F.
  llFound = .F.
  *E037351,1 [End]
  IF SEEK('MAMATRQ '+PADR(MFLD_NAME,100),'SYREPUVR') .OR.;
     SEEK('MAMATRQ '+ALLTRIM(MFLD_NAME),'SYREPUVR')
    *E037351,1 HBG 23/12/2003 Locate for records belong to Aria27 only [Begin]
    IF !EMPTY(SYREPUVR.CVER) AND SYREPUVR.CVER <> 'A27'
       lcAlias = ALIAS()
       lcMFLD_NAME = MFLD_NAME
       SELECT SYREPUVR
       LOCATE WHILE crep_id+PADR(SUBSTR(mfld_name,1,100),100," ") = 'MAMATRQ '+PADR(lcMFLD_NAME,100) FOR (EMPTY(CVER) OR CVER = 'A27')
       IF !FOUND()
         LOCATE WHILE crep_id+PADR(SUBSTR(mfld_name,1,100),100," ") = 'MAMATRQ '+ALLTRIM(MFLD_NAME) FOR (EMPTY(CVER) OR CVER = 'A27')
       ENDIF
       IF FOUND()
         llFound = .T.
       ENDIF
       SELECT (lcAlias)
    ELSE
      llFound = .T. 
    ENDIF
    IF llFound
    *E037351,1 [End] 
      REPLACE CFLD_HEAD WITH IIF(SYREPUVR.CFLD_HEAD='~',;
                                 EVALUATE(ALLTRIM(SUBSTR(SYREPUVR.CFLD_HEAD,2))),;
                                 SYREPUVR.CFLD_HEAD)
      DO CASE
        CASE UPPER(laOgObjType[lnI,1]) = 'LAOGFXFLT'  && case of fixed filter
          lcVarType = UPPER(ALLTRIM(EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','5]'))))
          DO CASE
            CASE lcVarType = 'BETWEEN'
              lnSep = AT('|',EVALUATE(laOgObjType[lnI,1]))
              IF lnSep > 0 .AND. LEN(EVALUATE(laOgObjType[lnI,1])) > 1
                REPLACE MFLTVAL WITH 'From '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),1,lnSep-1)+;
                                      ' To '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),lnSep+1)
              ENDIF
            CASE lcVarType = 'IN LIST'
              IF EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','7]')) = 'R'
                IF USED(EVALUATE(laOgObjType[lnI,1]))
                  SELECT (EVALUATE(laOgObjType[lnI,1]))
                  lcFieldNam = EVALUATE(laOgObjType[lnI,1]) + '.' +;
                               FIELD(1,EVALUATE(laOgObjType[lnI,1]))
                  SCAN
                    SELECT MRPFLT
                    REPLACE MFLTVAL WITH MFLTVAL+IIF(EMPTY(MFLTVAL),'',',')+EVALUATE(lcFieldNam)
                  ENDSCAN
                  SELECT MRPFLT
                ENDIF
              ELSE
                REPLACE MFLTVAL WITH STRTRAN(EVALUATE(laOgObjType[lnI,1]),'|',',')
              ENDIF
            OTHERWISE
              lcVarType = TYPE('EVALUATE(laOgObjType[lnI,1])')
              DO CASE
                CASE lcVarType = 'C'
                  REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,1])
                CASE lcVarType = 'N'
                  REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,1]))
                CASE lcVarType = 'D'
                  REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,1]))
              ENDCASE
          ENDCASE
        CASE UPPER(laOgObjType[lnI,2]) = 'INVB'       && case of popup
          IF EMPTY(SYREPUVR.MVENTRIES)  && case of yes/no
            IF EVALUATE(laOgObjType[lnI,2]) = 1
              REPLACE MFLTVAL WITH 'Yes'
            ELSE
              REPLACE MFLTVAL WITH 'No'
            ENDIF
          ELSE  && case of valid entries
            IF EMPTY(SYREPUVR.CVLDENTTYP)
              lnSep = AT('~',SYREPUVR.MVENTRIES)
              IF lnSep > 0
                lcvEntries = SUBSTR(SYREPUVR.MVENTRIES,1,lnSep-1)
                lnvEntries = OCCURS('|',lcvEntries)
                DIMENSION lavEntries[lnvEntries+1]
                lavEntries=''
                =gfSubStr(lcvEntries,@lavEntries,"|")
                REPLACE MFLTVAL WITH lavEntries[EVALUATE(laOgObjType[lnI,2])]
              ENDIF
            ELSE
              lnSep = AT('~',SYREPUVR.MVENTRIES)
              IF lnSep > 0
                lcvEntries = SUBSTR(SYREPUVR.MVENTRIES,1,lnSep-1)
                REPLACE MFLTVAL WITH EVALUATE(lcvEntries+'['+STR(EVALUATE(laOgObjType[lnI,2]))+']')
              ENDIF
            ENDIF
          ENDIF
        OTHERWISE
          lcVarType = TYPE('EVALUATE(laOgObjType[lnI,2])')
          DO CASE
            CASE lcVarType = 'C'
              REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,2])
            CASE lcVarType = 'N'
              REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,2]))
            CASE lcVarType = 'D'
              REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,2]))
          ENDCASE
      ENDCASE
    *E037351,1 HBG 23/12/2003 End Locate for records belong to Aria27 only [Begin]
    ELSE
      llNotFound = .T.
    ENDIF
    *E037351,1 [End]  
  ELSE
    *E037351,1 HBG 23/12/2003 Move this code to be done depending on the new Flag [Begin]  
    *IF UPPER(laOgObjType[lnI,1]) = 'LAOGFXFLT'
    *  lnSep = AT('.',MFLD_NAME)
    *  IF lnSep > 0 .AND. LEN(MFLD_NAME) > lnSep .AND.;
    *     SEEK(PADR(SUBSTR(MFLD_NAME,lnSep+1),10),'SYDFIELD')
    *    REPLACE CFLD_HEAD WITH SYDFIELD.CFLD_HEAD
    *    lcVarType = UPPER(ALLTRIM(EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','5]'))))
    *    DO CASE
    *      CASE lcVarType = 'LIKE'
    *        lcVarType = TYPE('EVALUATE(laOgObjType[lnI,1])')
    *        DO CASE
    *          CASE lcVarType = 'C'
    *            REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,1])
    *          CASE lcVarType = 'N'
    *            REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,1]))
    *          CASE lcVarType = 'D'
    *            REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,1]))
    *        ENDCASE
    *      CASE lcVarType = 'BETWEEN'
    *        lnSep = AT('|',EVALUATE(laOgObjType[lnI,1]))
    *        IF lnSep > 0 .AND. LEN(EVALUATE(laOgObjType[lnI,1])) > 1
    *          REPLACE MFLTVAL WITH 'From '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),1,lnSep-1)+;
    *                                ' To '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),lnSep+1)
    *        ENDIF
    *      CASE lcVarType = 'IN LIST'
    *        REPLACE MFLTVAL WITH STRTRAN(EVALUATE(laOgObjType[lnI,1]),'|',',')
    *    ENDCASE
    *  ENDIF
    *ENDIF
    llNotFound = .T.
    *E037351,1 [End]
  ENDIF
  
  *E037351,1 HBG 23/12/2003 If the record found in SYREPUVR [Begin]  
  IF llNotFound 
    IF UPPER(laOgObjType[lnI,1]) = 'LAOGFXFLT'
      lnSep = AT('.',MFLD_NAME)
      IF lnSep > 0 .AND. LEN(MFLD_NAME) > lnSep .AND.;
         SEEK(PADR(SUBSTR(MFLD_NAME,lnSep+1),10),'SYDFIELD')
        REPLACE CFLD_HEAD WITH SYDFIELD.CFLD_HEAD
        lcVarType = UPPER(ALLTRIM(EVALUATE(STRTRAN(laOgObjType[lnI,1],'6]','5]'))))
        DO CASE
          CASE lcVarType = 'LIKE'
            lcVarType = TYPE('EVALUATE(laOgObjType[lnI,1])')
            DO CASE
              CASE lcVarType = 'C'
                REPLACE MFLTVAL WITH EVALUATE(laOgObjType[lnI,1])
              CASE lcVarType = 'N'
                REPLACE MFLTVAL WITH STR(EVALUATE(laOgObjType[lnI,1]))
              CASE lcVarType = 'D'
                REPLACE MFLTVAL WITH DTOC(EVALUATE(laOgObjType[lnI,1]))
            ENDCASE
          CASE lcVarType = 'BETWEEN'
            lnSep = AT('|',EVALUATE(laOgObjType[lnI,1]))
            IF lnSep > 0 .AND. LEN(EVALUATE(laOgObjType[lnI,1])) > 1
              REPLACE MFLTVAL WITH 'From '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),1,lnSep-1)+;
                                    ' To '+SUBSTR(EVALUATE(laOgObjType[lnI,1]),lnSep+1)
            ENDIF
          CASE lcVarType = 'IN LIST'
            REPLACE MFLTVAL WITH STRTRAN(EVALUATE(laOgObjType[lnI,1]),'|',',')
        ENDCASE
      ENDIF
    ENDIF
  ENDIF
  *E037351,1 [End]
  
  =gfAdd_Info('MRPFLT')
ENDFOR

SELECT SYREPUVR
SET ORDER TO &lcOrder.
STORE .T. TO llSaveMrp
=gfModalGen('TRM36194B00000','DIALOG',lcMrp)
SELECT (lnAlias)
*-- end of lfSaveMrp.

*!*************************************************************
*! Name      : lfvMrpBas
*! Developer : Ahmed Maher - AMH
*! Date      : 04/09/2003
*! Purpose   : Validate the MRP base on option
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvMrpBas()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfvMrpBas

IF lcRpRName = 'MAMATRQD'
  lcRpReqBas = 'O'
ENDIF
=lfGetBase(lcRpRName='MAMATRQD')
CLEAR READ

*!*************************************************************
*! Name      : lfPrntMrp
*! Developer : Ahmed Maher - AMH
*! Date      : 04/09/2003
*! Purpose   : Print the existing Mrp report
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfPrntMrp()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfPrntMrp

*--Collecting -ve remaining fabrics variables.
STORE '' TO lcUOMBuy
lnFabcost = 0

llVenRef   = (gfGetMemVar('M_VENREF  ')='Y')
IF llVenRef
  = gfOpenFile(gcDataDir+'VENDMATH','VENDMATH','SH')
  = gfOpenFile(gcDataDir+'VENDMATL' , 'MATCOL','SH')
ENDIF

*-- Create cursor to print the first page of MRP report.
lcMrpCurs = gfTempName()
CREATE CURSOR (lcMrpCurs) (cType C(1),cPoMat C(6),cVendCode C(8),cVenComp C(30),;
                           cFabric C(7),cColor C(6),cVenFab C(10),cVenColr C(10),;
                           nOnOrder N(11,3),nReceived N(11,3),nPending N(11,3),;
                           cFld_Head C(25),mFltVal M)
INDEX ON cType+cPoMat+cFabric+cColor TAG (lcMrpCurs) OF (lcMrpCurs)
SELECT (lcMrpCurs)
SET ORDER TO TAG (lcMrpCurs)

STORE .F. TO llOpenVend,llOpenPfln,llPrintRep
IF !USED('APVENDOR')
  = gfOpenFile(gcDataDir+'APVENDOR','VenCode','SH')
  llOpenVend = .T.
ENDIF
IF !USED('POFLN')
  = gfOpenFile(gcDataDir+'POFLN' ,'POFLN','SH')
  llOpenPfln = .T.
ENDIF

*-- Get the selected MRPs if any
lcMrpFile = ''
llMrpRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MRPHDR.CMRP'),1)
IF lnPosition > 0
  lcMrpFile = laOGFxFlt[lnPosition,6]
  llMrpRang = IIF(!EMPTY(lcMrpFile) .AND. USED(lcMrpFile) .AND. RECCOUNT(lcMrpFile)>0,.T.,.F.)
ENDIF

*-- Get the selected Vendor if any
lcVendor = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
IF lnPosition > 0
  lcVendor = laOGFxFlt[lnPosition,6]
ENDIF

*-- Get the selected MPos if any
lcMPoFile = ''
llMPoRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POFHDR.POMAT'),1)
IF lnPosition > 0
  lcMPoFile = laOGFxFlt[lnPosition,6]
  llMPoRang = IIF(!EMPTY(lcMPoFile) .AND. USED(lcMPoFile) .AND. RECCOUNT(lcMPoFile)>0,.T.,.F.)
ENDIF

IF llMPoRang .OR. !EMPTY(lcVendor)
  SET ORDER TO TAG MRP IN POFHDR
ENDIF

IF !llMrpRang
  lcMrpFile = 'MRPHDR'
ENDIF
SELECT (lcMrpFile)
SCAN
  lcMrp = CMRP
  SELECT (lcMatReq)
  ZAP
  IF llMPoRang
    IF SEEK(lcMrp,'POFHDR')
      SELECT POFHDR
      LOCATE REST WHILE CMRP+CMATTYPE+POMAT = lcMrp;
                    FOR SEEK(POMAT,lcMPoFile) .AND. IIF(EMPTY(lcVendor),.T.,VENDOR == lcVendor)
      IF FOUND()
        =lfGetMrp(lcMrp)
      ENDIF
    ENDIF
  ELSE
    IF EMPTY(lcVendor)
      =lfGetMrp(lcMrp)
    ELSE
      IF SEEK(lcMrp,'POFHDR')
        SELECT POFHDR
        LOCATE REST WHILE CMRP+CMATTYPE+POMAT = lcMrp FOR VENDOR == lcVendor
        IF FOUND()
          =lfGetMrp(lcMrp)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  
  *--Print reports.
  SELECT (lcMatReq)
  SET ORDER TO TAG MATREQ
  
  *--Check if there is requirement.
  IF !llRpZeroRq
    IF lcRpRName = 'MAMATRQD'
      SET FILTER TO !LNOTPRINT
    ELSE
      SET FILTER TO OHReqTot <> 0
    ENDIF
  ENDIF
  GO TOP
  
  *--Check requirements existance.
  IF !EOF()
    wait window 'Printing....' NOWAIT
    
    *--[1] MRP first page.
    SELECT (lcMrpCurs)
    ZAP
    SELECT MRPFLT
    SEEK lcMrp
    SCAN REST WHILE CMRP = lcMrp
      SELECT (lcMrpCurs)
      APPEND BLANK
      REPLACE CTYPE     WITH '1',;
              CFLD_HEAD WITH MRPFLT.CFLD_HEAD,;
              MFLTVAL   WITH MRPFLT.MFLTVAL
    ENDSCAN
    SELECT APVENDOR
    lcOrderVnd = SET('ORDER')
    SET ORDER TO TAG VENCODE
    SELECT POFLN
    lcOrderLn = SET('ORDER')
    SET ORDER TO TAG POFLN
    SELECT POFHDR
    lcOrder = SET('ORDER')
    SET ORDER TO TAG MRP
    IF SEEK(lcMrp)
      SCAN REST WHILE CMRP+CMATTYPE+POMAT = lcMrp
        =SEEK(VENDOR,'APVENDOR')
        IF SEEK(CMATTYPE+POMAT,'POFLN')
          SELECT POFLN
          SCAN REST WHILE CMATTYPE+POMAT = POFHDR.CMATTYPE+POFHDR.POMAT
            SELECT (lcMrpCurs)
            IF !SEEK('2'+POFLN.POMAT+POFLN.FABRIC+POFLN.COLOR)
              APPEND BLANK
              REPLACE CTYPE     WITH '2',;
                      CPOMAT    WITH POFHDR.POMAT,;
                      CVENDCODE WITH POFHDR.VENDOR,;
                      CVENCOMP  WITH APVENDOR.CVENCOMP,;
                      CFABRIC   WITH POFLN.FABRIC,;
                      CCOLOR    WITH POFLN.COLOR
              IF llVenRef .AND. SEEK(CVENDCODE+CFABRIC+CCOLOR,'VENDMATL')
                REPLACE CVENFAB  WITH VENDMATL.CVENFAB,;
                        CVENCOLR WITH VENDMATL.CVENCOLR
              ENDIF
            ENDIF
            IF POFLN.TRANCD = '1'
              REPLACE NONORDER  WITH NONORDER + POFLN.NFABTOTQTY
            ELSE
              REPLACE NRECEIVED WITH NRECEIVED+ POFLN.NFABTOTQTY
            ENDIF
            REPLACE NPENDING WITH NONORDER - NRECEIVED
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
    SELECT POFHDR
    SET ORDER TO &lcOrder.
    SELECT POFLN
    SET ORDER TO &lcOrderLn.
    SELECT APVENDOR
    SET ORDER TO &lcOrderVnd.
    lcCurForm = lcRpRname
    lcRpRname = 'MAMRP'
  
    =gfCrtFrm(lcRpRname,lcOGFormArr,llOGRefForm)
    =lfRepPltFr(lcRpRname)

    SELECT (lcMrpCurs)
    DO gfDispRe WITH EVALUATE('lcRpRname')
    lcRpRname = lcCurForm
    
    *--[2] Fabrics requirement report.
    SELECT (lcMatReq)
    SET ORDER TO TAG MATREQ

    IF llRpByWare
      SET ORDER TO TAG MRFABLOC
      SET RELATION TO CFABLOC INTO WAREHOUS ADDITIVE
    ENDIF
    
    *--Initialize printing report variables.
    STORE 0   TO lnOnHand,lnOnOrdr,lnConv,lnLeadTm,lnYield,lnYield1,lnYield2,;
                 lnAvl1,lnAvl2,lnAvl3,lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8
    STORE " " TO lcDescrp,lcFabVen,lcOldItem,lcSDescrp,lcTkTRUn,;
                 lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8
    
    RESTORE FROM MEMO MFLTVAL ADDITIVE
    FOR lnI = 1 TO 20
      &laSaveVar[lnI,1]. = laSaveVar[lnI,2]
    ENDFOR
    
    SET KEY TO 'F'
    
    IF TYPE('lcCustRp') ='C' AND lcRpRName = 'MAMATRQC'
      SET ORDER TO MatreqC ADDITIVE
    ENDIF
    
    IF SEEK('F')
      =gfCrtFrm(lcRpRname,lcOGFormArr,llOGRefForm)
      =lfRepPltFr(lcRpRname)
      SELECT (lcMatReq)
      DO gfDispRe WITH EVAL('lcRpRname')
    ENDIF
    
    *--[3] Trims requirement report.
    SELECT (lcMatReq)
    SET KEY TO 'T'
    IF SEEK('T')
      *--Initialize printing variables
      =gfCrtFrm(lcRpRname,lcOGFormArr,llOGRefForm)
      =lfRepPltFr(lcRpRname)
      SELECT (lcMatReq)
      =lfInitVar()
      DO gfDispRe WITH EVAL('lcRpRname')
    ENDIF
    
    *--[4] Style components requirement report.
    SELECT (lcMatReq)
    SET KEY TO 'S'
    IF SEEK('S')
      *--Initialize printing variables
      =gfCrtFrm(lcRpRname,lcOGFormArr,llOGRefForm)
      =lfRepPltFr(lcRpRname)
      SELECT (lcMatReq)
      =lfInitVar()
      DO gfDispRe WITH EVAL('lcRpRname')
    ENDIF
    SET KEY TO
    STORE .T. TO llPrintRep
  ENDIF

ENDSCAN

IF !llPrintRep
  *--None of the selected styles have any requirements.
  *--                     [Ok]
  =gfModalGen('TRM00052B00000','DIALOG' )
ENDIF

IF llMPoRang .OR. !EMPTY(lcVendor)
  SET ORDER TO TAG POFHDR IN POFHDR
ENDIF

*--Close all Open Files.
IF USED(lcMatReq)
  USE IN (lcMatReq)
ENDIF
ERASE (gcWorkDir+lcMatReq+'.DBF')
ERASE (gcWorkDir+lcMatReq+'.CDX')

IF USED(lcMrpCurs)
  USE IN (lcMrpCurs)
ENDIF

IF llOpenPfln
  USE IN POFLN
ENDIF

IF llOpenVend
  USE IN APVENDOR
ENDIF

IF laArOpnFl[1] AND USED('SCALE')
  USE IN SCALE
ENDIF

IF laArOpnFl[2] AND USED('BOMLINE')
  USE IN BOMLINE
ENDIF

IF laArOpnFl[3] AND USED('CTKTBOM')
  USE IN CTKTBOM
ENDIF

IF laArOpnFl[4] AND USED('POSHDR')
  USE IN POSHDR
ENDIF
IF laArOpnFl[5] AND USED('POSLN')
  USE IN POSLN
ENDIF
IF laArOpnFl[6] AND USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF
IF laArOpnFl[7] AND USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetMrp
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Get data of the MRP report.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetMrp()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfGetMrp
PARAMETERS lcMrp

PRIVATE lnReccount,lnCurNum
IF SEEK(lcMrp,'MRPHDR')
  *-- Restore the memory variables.
  SELECT MRPHDR
  RESTORE FROM MEMO MFLTVAL ADDITIVE
  lcRpMrpBas = 'E'
  
  *-- Get the lines of MRP report.
  IF SEEK(lcMrp,'MRPLN')
    SELECT MRPLN
    SET KEY TO lcMrp
    
    *--Thermometer counter.
    COUNT ALL TO lnRecCount
    lnCurNum   = 0
    
    LOCATE
    SCAN
      lnCurNum = lnCurNum + 1
      =gfThermo(lnRecCount,lnCurNum ,"Collecting the MRP data",'MRP # : '+lcMrp)
      SCATTER MEMO MEMVAR
      SELECT (lcMatReq)
      APPEND BLANK
      REPLACE STYLE     WITH M.STYLE    ,;
              ITEM      WITH M.ITEM     ,;
              ICLR      WITH M.ICLR     ,;
              DESC      WITH M.DESC     ,;
              CWARECODE WITH M.CWARECODE,;
              TYP       WITH M.TYP      ,;
              CCATGTYP  WITH M.CCATGTYP ,;
              CNT       WITH M.CNT      ,;
              UOM       WITH M.UOM
      REPLACE QTY1      WITH M.NQTY1    ,;
              QTY2      WITH M.NQTY2    ,;
              QTY3      WITH M.NQTY3    ,;
              QTY4      WITH M.NQTY4    ,;
              QTY5      WITH M.NQTY5    ,;
              QTY6      WITH M.NQTY6    ,;
              QTY7      WITH M.NQTY7    ,;
              QTY8      WITH M.NQTY8
      REPLACE OREQ1     WITH M.NOREQ1   ,;
              OREQ2     WITH M.NOREQ2   ,;
              OREQ3     WITH M.NOREQ3   ,;
              OREQ4     WITH M.NOREQ4   ,;
              OREQ5     WITH M.NOREQ5   ,;
              OREQ6     WITH M.NOREQ6   ,;
              OREQ7     WITH M.NOREQ7   ,;
              OREQ8     WITH M.NOREQ8   ,;
              OREQTOT   WITH M.NOREQTOT
      REPLACE OHREQ1    WITH M.NOHREQ1  ,;
              OHREQ2    WITH M.NOHREQ2  ,;
              OHREQ3    WITH M.NOHREQ3  ,;
              OHREQ4    WITH M.NOHREQ4  ,;
              OHREQ5    WITH M.NOHREQ5  ,;
              OHREQ6    WITH M.NOHREQ6  ,;
              OHREQ7    WITH M.NOHREQ7  ,;
              OHREQ8    WITH M.NOHREQ8  ,;
              OHREQTOT  WITH M.NOHREQTOT
      REPLACE NPROJ1    WITH M.NPROJ1   ,;
              NPROJ2    WITH M.NPROJ2   ,;
              NPROJ3    WITH M.NPROJ3   ,;
              NPROJ4    WITH M.NPROJ4   ,;
              NPROJ5    WITH M.NPROJ5   ,;
              NPROJ6    WITH M.NPROJ6   ,;
              NPROJ7    WITH M.NPROJ7   ,;
              NPROJ8    WITH M.NPROJ8   ,;
              PROJECT   WITH M.NPROJECT
      REPLACE NOPNWIP   WITH M.NOPNWIP  ,;
              NYTOWIP   WITH M.NYTOWIP  ,;
              NUSEDREQ  WITH M.NUSEDREQ ,;
              NNETREQ   WITH M.NNETREQ  ,;
              LSTYMAKE  WITH M.LSTYMAKE ,;
              NNETREQ1  WITH M.NNETREQ1 ,;
              NNETREQ2  WITH M.NNETREQ2 ,;
              NNETREQ3  WITH M.NNETREQ3 ,;
              NNETREQ4  WITH M.NNETREQ4 ,;
              NNETREQ5  WITH M.NNETREQ5 ,;
              NNETREQ6  WITH M.NNETREQ6 ,;
              NNETREQ7  WITH M.NNETREQ7 ,;
              NNETREQ8  WITH M.NNETREQ8
      REPLACE NOPNWIP1  WITH M.NOPNWIP1 ,;
              NOPNWIP2  WITH M.NOPNWIP2 ,;
              NOPNWIP3  WITH M.NOPNWIP3 ,;
              NOPNWIP4  WITH M.NOPNWIP4 ,;
              NOPNWIP5  WITH M.NOPNWIP5 ,;
              NOPNWIP6  WITH M.NOPNWIP6 ,;
              NOPNWIP7  WITH M.NOPNWIP7 ,;
              NOPNWIP8  WITH M.NOPNWIP8
      REPLACE NYTOWIP1  WITH M.NYTOWIP1 ,;
              NYTOWIP2  WITH M.NYTOWIP2 ,;
              NYTOWIP3  WITH M.NYTOWIP3 ,;
              NYTOWIP4  WITH M.NYTOWIP4 ,;
              NYTOWIP5  WITH M.NYTOWIP5 ,;
              NYTOWIP6  WITH M.NYTOWIP6 ,;
              NYTOWIP7  WITH M.NYTOWIP7 ,;
              NYTOWIP8  WITH M.NYTOWIP8 ,;
              POCT      WITH M.MPoCt    ,;
              MFLTVAL   WITH M.MFLTVAL
      REPLACE CFABLOC   WITH M.CFABLOC  ,;
              CINHOUSE  WITH M.CINHOUSE ,;
              LGETTHIS  WITH M.LGETTHIS ,;
              LBOM      WITH M.LBOM     ,;
              YIELD1    WITH M.NYIELD1  ,;
              YIELD2    WITH M.NYIELD2  ,;
              YIELD3    WITH M.NYIELD3  ,;
              YIELD4    WITH M.NYIELD4  ,;
              YIELD5    WITH M.NYIELD5  ,;
              YIELD6    WITH M.NYIELD6  ,;
              YIELD7    WITH M.NYIELD7  ,;
              YIELD8    WITH M.NYIELD8
      REPLACE ORDER     WITH M.ORDER    ,;
              COMPLETE  WITH M.COMPLETE ,;
              ONHAND    WITH M.ONHAND   ,;
              ONORDER   WITH M.ONORDER  ,;
              ETA       WITH M.ETA      ,;
              LMULTIPO  WITH M.LMULTIPO ,;
              LNOTPRINT WITH M.LNOTPRINT,;
              ITEMSTK   WITH M.ITEMSTK
    ENDSCAN
    =gfThermo(100,100)
    SET KEY TO
  ENDIF
ENDIF
*-- end of lfGetMrp.

*!*************************************************************
*! Name      : lfsrvMrp
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the MrpHdr file when browse the Mrps.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvMrp()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*C102819,1 AMH
FUNCTION lfSrvMrp
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT MRPHDR
    LOCATE
  CASE lcParm = 'R'  && Reset code
  CASE lcParm = 'V'  && Valid code
ENDCASE
*-- end of lfsrvMrp.

*!*************************************************************
*! Name      : lfsrvPof
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the PofHdr file when browse the MPOs.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvPof()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*C102819,1 AMH
FUNCTION lfSrvPof
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT POFHDR
    LOCATE
  CASE lcParm = 'R'  && Reset code
  CASE lcParm = 'V'  && Valid code
ENDCASE
*-- end of lfsrvPof.

*!*************************************************************
*! Name      : lfsrvOrd
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the OrdHdr file when browse the SOs.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvOrd()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*C102819,1 AMH
FUNCTION lfSrvOrd
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT ORDHDR
    LOCATE
  CASE lcParm = 'R'  && Reset code
  CASE lcParm = 'V'  && Valid code
ENDCASE
*-- end of lfsrvOrd.

*!*************************************************************
*! Name      : lfsrvCut
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the CutTktH file when browse the C/Ts.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvCut()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*C102819,1 AMH
FUNCTION lfSrvCut
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT CUTTKTH
    LOCATE
  CASE lcParm = 'R'  && Reset code
  CASE lcParm = 'V'  && Valid code
ENDCASE
*-- end of lfsrvCut.

*!*************************************************************
*! Name      : lfsrvPos
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/09/2003
*! Purpose   : Locate in the PosHdr file when browse the SPOs.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvPos()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*C102819,1 AMH
FUNCTION lfSrvPos
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT POSHDR
    LOCATE
  CASE lcParm = 'R'  && Reset code
  CASE lcParm = 'V'  && Valid code
ENDCASE
*-- end of lfsrvPos.

*!*************************************************************
*! Name      : lfPrntDtLn
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/14/2003
*! Purpose   : Print the Detail Line layout
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrntDtLn()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfPrntDtLn

SET ORDER TO TAG MATREQDT
lcForWare = IIF( llByLoctn ,"cWarecode = lcLocation" , ".T." )
IF lcRpReqBas = 'W'
  IF llRpByWare
    SET ORDER TO TAG MRFABLOCDT
  ENDIF
  
  lcCutFile = ''
  llCutRang = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
  IF lnPosition > 0
    lcCutFile = laOGFxFlt[lnPosition,6]
    llCutRang = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
  ENDIF
  
  lcPosFile = ''
  llPosRang = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.PO'),1)
  IF lnPosition > 0
    lcPosFile = laOGFxFlt[lnPosition,6]
    llPosRang = IIF(!EMPTY(lcPosFile) .AND. USED(lcPosFile) .AND. RECCOUNT(lcPosFile)>0,.T.,.F.)
  ENDIF
  
  ldCutFrom = {}
  ldCutTo   = {}
  llCutDate = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.ENTERED'),1)
  IF lnPosition > 0
    ldCutFrom = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,10))
    ldCutTo   = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],12))
    llCutDate = !EMPTY(ldCutFrom) .OR. !EMPTY(ldCutTo)
  ENDIF
  
  ldPosFrom = {}
  ldPosTo   = {}
  llPosDate = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.ENTERED'),1)
  IF lnPosition > 0
    ldPosFrom = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,10))
    ldPosTo   = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],12))
    llPosDate = !EMPTY(ldPosFrom) .OR. !EMPTY(ldPosTo)
  ENDIF
ELSE
  lcOrdFile = ''
  llOrdRang = .F.
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ORDER'),1)
  IF lnPosition > 0
    lcOrdFile = laOGFxFlt[lnPosition,6]
    llOrdRang = IIF(!EMPTY(lcOrdFile) .AND. USED(lcOrdFile) .AND. RECCOUNT(lcOrdFile)>0,.T.,.F.)
  ENDIF
ENDIF
lcStyleKey = SPACE(32)

*--Thermometer counter.
lnRecCount = RECCOUNT()
lnCurNum   = 0
SCAN
  IF !EMPTY(lcStyleKey) .AND. ITEM+ICLR+STYLE == lcStyleKey
    LOOP
  ELSE
    lcStyleKey = ITEM+ICLR+STYLE
    REPLACE LNOTPRINT WITH .T.
  ENDIF
  lnCurNum = lnCurNum + 1
  =gfThermo(lnRecCount,lnCurNum ,"Calculating requirements",'Style : '+Style)
  lcStyle = STYLE
  IF lcRpReqBas = 'W'
    =lfGetWipDt()
  ELSE
    =lfGetOrdDt()
  ENDIF
ENDSCAN
=gfThermo(100,100)
WAIT CLEAR

*--Get the OnHand and the OnOrder
STORE .F. TO llOpenPfHd,llOpenPfln,llOpenMfgH,llOpenMfgD
IF !USED('POFHDR')
  = gfOpenFile(gcDataDir+'POFHDR' ,'POFHDR','SH')
  llOpenPfHd = .T.
ENDIF
IF !USED('POFLN')
  = gfOpenFile(gcDataDir+'POFLN' ,'POFLNF','SH')
  llOpenPfln = .T.
ENDIF
IF !USED('MMFGORDH')
  = gfOpenFile(gcDataDir+'MMFGORDH' ,'MMFGORDD','SH')
  llOpenMfgH = .T.
ENDIF
IF !USED('MMFGORDD')
  = gfOpenFile(gcDataDir+'MMFGORDD' ,'MMFGORDD','SH')
  llOpenMfgD = .T.
ENDIF

*-- There is no need for the followin block of code in case of O.T.S
IF !llFromOts  &&-- if called from Requirement only do the next block of code .
  *--Open -ve remaining items file.
  lcTmpRmain = gfTempName()
  SELECT (lcMatReq)
  CREATE DBF (gcWorkDir+lcTmpRmain) ;
   ( Fabric C(7), Color C(6),Vendor C(8),nFabTotQty N(11,3),LeadTime N(3),;
     Complete D(8),UOMBuy C(3),cFabGrade C(1),nfabcost N(9,3),cPriceCur C(3),;
     nPriceRat N(9,4),nCurrUnit N(6),cDutyCur C(3),nDutyRat N(9,4),nDCurUnit N(6),;
     nItm_Frt N(9,3),nItem_Tax N(9,3), nItemQuota N(9,3), cWareCode C(6),;
     dDelivDate D,cVenFab C(10),cVenColr C(10))
  INDEX ON Fabric + Color TAG (lcTmpRmain)
  SET ORDER TO 1 
ENDIF &&--endif if called from Requirement only do the next block of code .

*--Collecting -ve remaining fabrics variables.
STORE '' TO lcUOMBuy
lnFabcost = 0

llVenRef   = (gfGetMemVar('M_VENREF  ')='Y')
IF llVenRef
  = gfOpenFile(gcDataDir+'VENDMATH','VENDMATH','SH')
  = gfOpenFile(gcDataDir+'VENDMATL' , 'MATCOL','SH')
ENDIF

SELECT (lcMatReq)
lcStyleKey = SPACE(32)

*--Thermometer counter.
lnRecCount = RECCOUNT()
lnCurNum   = 0
lnItemStk  = 0
lcItemKey  = SPACE(25)
SCAN FOR !LNOTPRINT
  =SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
  IF ITEM+ICLR+STYLE <> lcStyleKey
    lcStyleKey = ITEM+ICLR+STYLE
    ldStart = {}
    lnOnHand = FABRIC.ONHAND
    IF llByFabLoc
      =SEEK(PADR(ITEM,7)+ICLR+lcFLoctn,'FABDYE')
      lnOnHand = FABDYE.OnHand
    ENDIF
    IF llRpByWare .AND. BOM.CCATGTYP $ 'FT' .AND. !EMPTY(CFABLOC)
      =SEEK(PADR(ITEM,7)+ICLR+CFABLOC,'FABDYE')
      lnOnHand = FABDYE.OnHand
    ENDIF
    llFrstSty = .F.
  ELSE
    ldStart = ldEnd + 1
  ENDIF
  IF ITEM+ICLR <> lcItemKey
    lcItemKey = ITEM+ICLR
    lnItemStk = lnOnHand
    llFrstSty = .T.
  ELSE
    lnItemStk = 0
  ENDIF
  lnCurNum = lnCurNum + 1
  =gfThermo(lnRecCount,lnCurNum ,"Calculating OnHand and OnOrder",'Style : '+Style)
  ldEnd    = COMPLETE
  lcPoMat  = SPACE(6)
  llUpdate = .F.
  IF FABRIC.MAKE
    IF SEEK(PADR(ITEM,7),'MMFGORDH')
      SELECT MMFGORDH
      SCAN REST WHILE CFABRIC+MMFGORDNO = PADR(EVALUATE(lcMatReq+'.ITEM'),7);
                  FOR STATUS = 'O' .AND. CWARECODE = lcFLoctn .AND.;
                      IIF(llRpByWare .AND. !EMPTY(EVALUATE(lcMatReq+'.CFABLOC')),;
                          CWARECODE=EVALUATE(lcMatReq+'.CFABLOC'),.T.) .AND.;
                      BETWEEN(MMFGORDH.COMPLETE,ldStart,ldEnd)
        IF SEEK(MMFGORDNO+CFABRIC+EVALUATE(lcMatReq+'.ICLR'),'MMFGORDD')
          IF EMPTY(lcPoMat)
            lcPoMat = MMFGORDNO
          ELSE
            IF MMFGORDNO <> lcPoMat
              lcPoMat = '******'
            ENDIF
          ENDIF
          SELECT MMFGORDD
          SCAN REST WHILE CMFGORDNO+CFABRIC+COLOR+DYELOT+TRANCD =;
                          MMFGORDH.MMFGORDNO+MMFGORDH.CFABRIC+EVALUATE(lcMatReq+'.ICLR')
            SELECT (lcMatReq)
            llUpdate = .T.
            REPLACE ONHAND   WITH lnOnHand,;
                    ONORDER  WITH MAX(ONORDER+(MMFGORDD.NMFGTOTQTY*FABRIC.CONV*IIF(MMFGORDD.TRANCD='1',1,-1)),0),;
                    ITEMSTK  WITH lnItemStk+IIF(llFrstSty,ONORDER,0),;
                    LMULTIPO WITH (lcPoMat = '******'),;
                    ETA      WITH MMFGORDH.COMPLETE
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
  ELSE
    IF SEEK(PADR(ITEM,7)+ICLR,'POFLN')
      SELECT POFLN
      lcPoMatKey = SPACE(20)
      ldDelvDate = {}
      SCAN REST WHILE FABRIC+COLOR+CMATTYPE+POMAT+TRANCD =;
                      EVALUATE('PADR('+lcMatReq+'.ITEM,7)+'+lcMatReq+'.ICLR')
        IF FABRIC+COLOR+CMATTYPE+POMAT # lcPoMatKey
          lcPoMatKey = FABRIC+COLOR+CMATTYPE+POMAT
          ldDelvDate = DDELIVDATE
        ENDIF
        IF SEEK(CMATTYPE+POMAT,'POFHDR') .AND. POFHDR.STATUS = 'O' .AND. POFHDR.CWARECODE = lcFLoctn;
           .AND. IIF(llRpByWare .AND. !EMPTY(EVALUATE(lcMatReq+'.CFABLOC')),;
                     POFHDR.CWARECODE=EVALUATE(lcMatReq+'.CFABLOC'),.T.)
          IF BETWEEN(IIF(EMPTY(ldDelvDate),POFHDR.COMPLETE,ldDelvDate),ldStart,ldEnd)
            IF EMPTY(lcPoMat)
              lcPoMat = POMAT
            ELSE
              IF POMAT <> lcPoMat
                lcPoMat = '******'
              ENDIF
            ENDIF
            SELECT (lcMatReq)
            llUpdate = .T.
            REPLACE ONHAND   WITH lnOnHand,;
                    ONORDER  WITH MAX(ONORDER+(POFLN.NFABTOTQTY*FABRIC.CONV*IIF(POFLN.TRANCD='1',1,-1)),0),;
                    ITEMSTK  WITH lnItemStk+IIF(llFrstSty,ONORDER,0),;
                    LMULTIPO WITH (lcPoMat = '******'),;
                    ETA      WITH IIF(EMPTY(ldDelvDate),POFHDR.COMPLETE,ldDelvDate)
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  SELECT (lcMatReq)
  IF !llUpdate
    REPLACE ONHAND  WITH lnOnHand,;
            ITEMSTK WITH lnItemStk
  ENDIF
  lnOnHand = ONHAND + ONORDER - NYTOWIP
  IF ORDER = 'ZZZZZ*'
    IF ONORDER = 0
      REPLACE LNOTPRINT WITH .T.
    ENDIF
    IF lnOnHand < 0 .AND. !llFromOts .AND. lcRpMrpBas = 'N'
      lcFabVen   = FABRIC.Vendor
      lnConv     = FABRIC.Conv
      lnLeadTm   = FABRIC.LeadTime
      lnFabcost  = FABRIC.nFabcost
      lcUOMBuy   = FABRIC.UOMBUY
      lnRequired = ABS(lnOnHand)
      =lfSavNRItm()
    ENDIF
  ENDIF
ENDSCAN
=gfThermo(100,100)
WAIT CLEAR

IF llOpenPfln
  USE IN POFLN
ENDIF
IF llOpenPfHd
  USE IN POFHDR
ENDIF
IF llOpenMfgH
  USE IN MMFGORDH
ENDIF
IF llOpenMfgD
  USE IN MMFGORDD
ENDIF

SELECT (lcMatReq)
*--Check if there is requirement.
*IF !llRpZeroRq
  SET FILTER TO !LNOTPRINT
*ENDIF
LOCATE

*--Check requirements existance.
IF EOF()
  *--None of the selected styles have any requirements.
  *--                     [Ok]
  =gfModalGen('TRM36108B36000','DIALOG')
  RETURN
ENDIF

SELECT (lcMatReq)
IF lcRpReqBas = 'W'
  IF llRpByWare
    SET ORDER TO TAG MRFABLOCDT
  ELSE
    SET ORDER TO TAG MATREQDT
  ENDIF
ENDIF

*--Print reports.
wait window 'Printing....' NOWAIT
=lfPrintRep()

IF !llFromOts
  IF !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
    =lfSaveMrp()
  ENDIF
  
  *--Ceck if there is negative remainig in some materials.
  GO TOP IN (lcTmpRmain)
  IF !EOF(lcTmpRmain)
    *--Negative remaining quantity on some materials. Create P/Os ?
    *--                        [ Yes / No ] 
    lnChoice=gfModalGen('QRM36111B36001','DIALOG')
    IF lnChoice = 1
      IF !llSaveMrp .AND. gfModalGen('TRM36193B36001','DIALOG') = 1
        =lfSaveMrp()
      ENDIF
      wait window 'Generate P/Os...' NOWAIT
      IF lfGenMPo()
        RETURN
      ENDIF
    ENDIF
  ENDIF
  USE IN (lcTmpRmain)
  ERASE (gcWorkDir+lcTmpRmain+'.DBF')
  ERASE (gcWorkDir+lcTmpRmain+'.CDX')
ENDIF

*--Close all Open Files.
IF USED(lcMatReq)
  USE IN (lcMatReq)
ENDIF
ERASE (gcWorkDir+lcMatReq+'.DBF')
ERASE (gcWorkDir+lcMatReq+'.CDX')

IF laArOpnFl[1] AND USED('SCALE')
  USE IN SCALE
ENDIF
IF laArOpnFl[2] AND USED('BOMLINE')
  USE IN BOMLINE
ENDIF
IF laArOpnFl[3] AND USED('CTKTBOM')
  USE IN CTKTBOM
ENDIF
IF laArOpnFl[4] AND USED('POSHDR')
  USE IN POSHDR
ENDIF
IF laArOpnFl[5] AND USED('POSLN')
  USE IN POSLN
ENDIF
IF laArOpnFl[6] AND USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF
IF laArOpnFl[7] AND USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF
IF laArOpnFl[8] AND USED('CODES')
  USE IN CODES
ENDIF
IF laArOpnFl[9] AND USED('MFGOPRHD')
  USE IN MFGOPRHD
ENDIF
*-- end lfPrntDtLn.

*!*************************************************************
*! Name      : lfGetWipDt
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/14/2003
*! Purpose   : Get the wip req. in case of Detail Line layout
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetWipDt()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfGetWipDt

PRIVATE lnAlias,llFirst
lnAlias  = SELECT(0)
llFirst  = .T.
llAddRec = .F.
IF lStyMake
  SELECT CUTTKTL
  IF SEEK(lcStyle)
    lcCutNo = SPACE(6)
    SCAN REST WHILE STYLE+CUTTKT+TRANCD = lcStyle FOR &lcForWare.
      =SEEK(CUTTKT,'CUTTKTH')
      *--Check C/Ts filter.
      IF llCutDate .AND. !BETWEEN(CUTTKTH.ENTERED,ldCutFrom,ldCutTo)
        LOOP
      ENDIF
      IF CUTTKTH.Status $ 'HOA'
        SELECT (lcMatReq)
        IF llFirst
          llFirst = .F.
          lcCutNo = CUTTKTH.CUTTKT
        ELSE
          IF CUTTKTH.CUTTKT <> lcCutNo
            lcCutNo = CUTTKTH.CUTTKT
            SCATTER MEMO MEMVAR
            STORE 0 TO M.OREQ1,M.OREQ2,M.OREQ3,M.OREQ4,M.OREQ5,M.OREQ6,M.OREQ7,M.OREQ8,M.OREQTOT
            INSERT INTO (lcMatReq) FROM MEMVAR
          ENDIF
        ENDIF
        lnSign = IIF(CUTTKTL.TRANCD='1',1,-1)
        IF !llAddRec
          llAddRec = IIF(llCutRang,SEEK(CUTTKTH.CUTTKT,lcCutFile),!llPosRang)
        ENDIF
        REPLACE ORDER     WITH CUTTKTH.CUTTKT,;
                COMPLETE  WITH CUTTKTH.ENTERED,;
                LNOTPRINT WITH IIF(llCutRang,!SEEK(CUTTKTH.CUTTKT,lcCutFile),llPosRang),;
                OReq1     WITH MAX(OREQ1+(CUTTKTL.QTY1*lnSign),0),;
                OReq2     WITH MAX(OREQ2+(CUTTKTL.QTY2*lnSign),0),;
                OReq3     WITH MAX(OREQ3+(CUTTKTL.QTY3*lnSign),0),;
                OReq4     WITH MAX(OREQ4+(CUTTKTL.QTY4*lnSign),0),;
                OReq5     WITH MAX(OREQ5+(CUTTKTL.QTY5*lnSign),0),;
                OReq6     WITH MAX(OREQ6+(CUTTKTL.QTY6*lnSign),0),;
                OReq7     WITH MAX(OREQ7+(CUTTKTL.QTY7*lnSign),0),;
                OReq8     WITH MAX(OREQ8+(CUTTKTL.QTY8*lnSign),0),;
                OReqTot   WITH OREQ1+OREQ2+OREQ3+OREQ4+OREQ5+OREQ6+OREQ7+OREQ8,;
                NYTOWIP   WITH (OREQ1*QTY1)+(OREQ2*QTY2)+(OREQ3*QTY3)+(OREQ4*QTY4)+;
                               (OREQ5*QTY5)+(OREQ6*QTY6)+(OREQ7*QTY7)+(OREQ8*QTY8)
      ENDIF
    ENDSCAN
  ENDIF
  
  IF llAddRec
    =lfAddLstRc()
  ELSE
    SELECT (lcMatReq)
    REPLACE LNOTPRINT WITH .T.
  ENDIF
  
  SELECT CUTTKTL
  SET RELATION TO
ELSE
  SELECT POSLN
  IF SEEK(lcStyle)
    lcPosNo = SPACE(6)
    SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = lcStyle;
                FOR &lcForWare. .AND. !(TRANCD $ '36')
      =SEEK(CSTYTYPE+PO,'POSHDR')
      *--Check POs filter.
      IF llPosDate .AND. !BETWEEN(POSHDR.ENTERED,ldPosFrom,ldPosTo)
        LOOP
      ENDIF
      IF POSHDR.Status $ 'HOA'
        SELECT (lcMatReq)
        IF llFirst
          llFirst = .F.
          lcPosNo = POSHDR.PO
        ELSE
          IF POSHDR.PO <> lcPosNo
            lcPosNo = POSHDR.PO
            SCATTER MEMO MEMVAR
            STORE 0 TO M.OREQ1,M.OREQ2,M.OREQ3,M.OREQ4,M.OREQ5,M.OREQ6,M.OREQ7,M.OREQ8,M.OREQTOT
            INSERT INTO (lcMatReq) FROM MEMVAR
          ENDIF
        ENDIF
        lnSign = IIF(POSLN.TRANCD='1',1,-1)
        IF !llAddRec
          llAddRec = IIF(llPosRang,SEEK(POSHDR.PO,lcPosFile),!llCutRang)
        ENDIF
        REPLACE ORDER     WITH POSHDR.PO,;
                COMPLETE  WITH POSHDR.ENTERED,;
                LNOTPRINT WITH IIF(llPosRang,!SEEK(POSHDR.PO,lcPosFile),llCutRang),;
                OReq1     WITH MAX(OREQ1+(POSLN.QTY1*lnSign),0),;
                OReq2     WITH MAX(OREQ2+(POSLN.QTY2*lnSign),0),;
                OReq3     WITH MAX(OREQ3+(POSLN.QTY3*lnSign),0),;
                OReq4     WITH MAX(OREQ4+(POSLN.QTY4*lnSign),0),;
                OReq5     WITH MAX(OREQ5+(POSLN.QTY5*lnSign),0),;
                OReq6     WITH MAX(OREQ6+(POSLN.QTY6*lnSign),0),;
                OReq7     WITH MAX(OREQ7+(POSLN.QTY7*lnSign),0),;
                OReq8     WITH MAX(OREQ8+(POSLN.QTY8*lnSign),0),;
                OReqTot   WITH OREQ1+OREQ2+OREQ3+OREQ4+OREQ5+OREQ6+OREQ7+OREQ8,;
                NYTOWIP   WITH (OREQ1*QTY1)+(OREQ2*QTY2)+(OREQ3*QTY3)+(OREQ4*QTY4)+;
                               (OREQ5*QTY5)+(OREQ6*QTY6)+(OREQ7*QTY7)+(OREQ8*QTY8)
      ENDIF
    ENDSCAN
  ENDIF
  
  IF llAddRec
    =lfAddLstRc()
  ELSE
    SELECT (lcMatReq)
    REPLACE LNOTPRINT WITH .T.
  ENDIF
  
  SELECT POSLN
  SET RELATION TO
ENDIF
SELECT (lnAlias)
*-- end lfGetWipDt.

*!*************************************************************
*! Name      : lfGetOrdDt
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/14/2003
*! Purpose   : Get the wip req. in case of Detail Line layout
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetOrdDt()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfGetOrdDt

PRIVATE lnAlias,llFirst
lnAlias  = SELECT(0)
llFirst  = .T.
llAddRec = .F.
SELECT ORDLINE
IF SEEK(lcStyle)
  SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle;
              FOR &lcForWare.
    =SEEK('O'+Order,'ORDHDR')
    *--Check orders filter.
    IF !EVALUATE(lcRpExp1)
      LOOP
    ENDIF
    IF OrdHdr.Status $ IIF(llRpHldOrd,'OH','O')
      SELECT (lcMatReq)
      IF llFirst
        llFirst = .F.
      ELSE
        SCATTER MEMO MEMVAR
        INSERT INTO (lcMatReq) FROM MEMVAR
      ENDIF
      IF !llAddRec
        llAddRec = IIF(llOrdRang,SEEK(ORDHDR.ORDER,lcOrdFile),.T.)
      ENDIF
      REPLACE ORDER     WITH ORDHDR.ORDER,;
              COMPLETE  WITH ORDHDR.COMPLETE,;
              LNOTPRINT WITH IIF(llOrdRang,!SEEK(ORDHDR.ORDER,lcOrdFile),.F.),;
              OReq1     WITH ORDLINE.QTY1,;
              OReq2     WITH ORDLINE.QTY2,;
              OReq3     WITH ORDLINE.QTY3,;
              OReq4     WITH ORDLINE.QTY4,;
              OReq5     WITH ORDLINE.QTY5,;
              OReq6     WITH ORDLINE.QTY6,;
              OReq7     WITH ORDLINE.QTY7,;
              OReq8     WITH ORDLINE.QTY8,;
              OReqTot   WITH ORDLINE.TOTQTY,;
              NYTOWIP   WITH (OREQ1*QTY1)+(OREQ2*QTY2)+(OREQ3*QTY3)+(OREQ4*QTY4)+;
                             (OREQ5*QTY5)+(OREQ6*QTY6)+(OREQ7*QTY7)+(OREQ8*QTY8)
    ENDIF
  ENDSCAN
ENDIF

IF llAddRec
  =lfAddLstRc()
ELSE
  SELECT (lcMatReq)
  REPLACE LNOTPRINT WITH .T.
ENDIF

SELECT ORDLINE
SET RELATION TO
SELECT (lnAlias)
*-- end lfGetOrdDt.

*!*************************************************************
*! Name      : lfAddLstRc
*! Developer : AHMED MAHER (AMH)
*! Date      : 04/14/2003
*! Purpose   : Add the last record of style
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : lfAddLstRc()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfAddLstRc

SELECT (lcMatReq)
IF llRpByWare
  SCATTER FIELDS CCATGTYP,CINHOUSE,CFABLOC,ITEM,ICLR,STYLE MEMVAR
  SET ORDER TO TAG MRFABLOCDT DESCENDING
  SEEK m.cCatgTyp+m.cInHouse+m.cFabLoc+m.Item+m.IClr+m.Style
ELSE
  SCATTER FIELDS CCATGTYP,ITEM,ICLR,STYLE MEMVAR
  SET ORDER TO TAG MATREQDT DESCENDING
  SEEK m.cCatgTyp+m.Item+m.IClr+m.Style
ENDIF
SCATTER MEMO MEMVAR
INSERT INTO (lcMatReq) FROM MEMVAR
REPLACE ORDER     WITH 'ZZZZZ*',;
        COMPLETE  WITH COMPLETE+(365.25*400),;
        LNOTPRINT WITH .F.,;
        NYTOWIP   WITH 0,;
        OREQTOT   WITH 0
IF llRpByWare
  SET ORDER TO TAG MRFABLOCDT ASCENDING
ELSE
  SET ORDER TO TAG MATREQDT ASCENDING
ENDIF
*-- end of lfAddLstRc.

*!*************************************************************
*! Name      : lfGetFrmat
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2003
*! Purpose   : Get the report format
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : lfGetFrmat()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfGetFrmat

DECLARE laRpFrmDsc[IIF(llMrp,4,3),1],laRpFrmId[IIF(llMrp,4,3),1]

laRpFrmDsc[1,1] = 'Summary'
laRpFrmDsc[2,1] = 'Detail short'
laRpFrmDsc[3,1] = 'Detail long'
IF llMrp
  laRpFrmDsc[4,1] = 'Detail lines'
ENDIF

laRpFrmId[1,1] = 'MAMATRQ'
laRpFrmId[2,1] = 'MAMATRQH'
laRpFrmId[3,1] = 'MAMATRQL'
IF llMrp
  laRpFrmId[4,1] = 'MAMATRQD'
ENDIF

*!*************************************************************
*! Name      : lfGetBase
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/10/2003
*! Purpose   : Get the Requirement Based on popup.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : lfGetBase()
*!*************************************************************
*C102819,1 AMH
FUNCTION lfGetBase
PARAMETERS llDet

DECLARE laRpBasDsc[IIF(llDet,2,3),1],laRpBasId[IIF(llDet,2,3),1]

laRpBasDsc[1,1] = 'OTC'
laRpBasDsc[2,1] = 'WIP'
IF !llDet
  laRpBasDsc[3,1] = 'Both'
ENDIF

laRpBasId[1,1] = 'O'
laRpBasId[2,1] = 'W'
IF !llDet
  laRpBasId[3,1] = 'B'
ENDIF

*!*************************************************************
*! Name      : lfSRVFab
*: Developer : AHMED MAHER (AMH)
*! Date      : 06/29/2003
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
*!B607385,1
FUNCTION lfSRVFab
PARAMETERS lcParm

PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*: Developer : AHMED MAHER (AMH)
*! Date      : 06/29/2003
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
*!B607385,1
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp

PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfSegInfo
*! Developer : Nader Nabil (NNA)
*! Date      : 09/09/2004
*! Purpose   : Check if The item code Structure found or not.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns : False or True
*!*************************************************************
*! Example   : =lfSegInfo()
*!*************************************************************
*!038431 (Bld41)
FUNCTION lfSegInfo
lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  llOgtrmnat = .T.
  CLEAR READ  
ENDIF
SELECT(lnalias)
RETURN
*-- End of function lfSegInfo.