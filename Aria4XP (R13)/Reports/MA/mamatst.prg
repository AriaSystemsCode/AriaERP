*:***************************************************************************
*: Program file  : MAMATST
*: Program desc. : Style Material master
*: For Report    : MAMATST.RPT ,MAMATSTA.RPT
*: System        : Aria Advantage Series.
*: Module        : Materials (MA)
*: Developer     : Heba Mohamed Amin	(HMA)
*:***************************************************************************
*: Calls :
*:    Procedures : lpPrint
*:    Functions  : lfBldSqlCur(),lfCrtIndex(),lfGetColor(),lfSlctFox(),lfSRVSty(),
*:                 lfStySum(),lfSumFab1(),lfSumFab2(),lfwRepWhen()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAMATST
*:***************************************************************************
*: Modifications:*--B037636 ,1 HMA  10/28/2003 Try to improve the performance.
*:B037636 ,2 HMA  10/28/2003 Fix onhand Qty Filter.
*:B125737 ,1 HMA  01/09/2005 Return the whole vaue of numeric field value instead of return only its integer value
*:B127322 ,1 ASM  04/09/2005 Fix Not Prinitng Summary
*:E126798 ,1 HMA  04/18/2005 use Filter Change Variable in order to improve tha performance,Don't recollect Data if nothing change in Option Grid
*:HMA 07/12/2005 ,remove expression of fixed filter 'ITEM.CSTYMAJOR' from lcRpSqlExp
*:************************************************************************
#INCLUDE R:\Aria4xp\reports\MA\mamatst.H
LOCAL lcStyFab,lcSelVend,lcSelFab,lnResult2,loRDA1

IF loOGScroll.llOGFltCh   &&If Filter Changed
  lcStySty = ""
  lcStyFab =""
  lcSelFab = ""
  lcSelVend = ""
  llFabric = .F.
  llVendor = .F.

  *-- Variable Hold only the Selected Styles From OG.
  lcStySty = loOgScroll.laOgFxFlt[1,6]
  *-- Variable Hold the Primary Fabrics Selected for Styles.
  lcStyFab = loOgScroll.laOgFxFlt[2,6]
  lcStySty1 =""
  lcStyFab1 =""
  *--Retrieve Cursor From Style File(Fox File)which hold only Styles which had Fabrics.
  lcStyFlds="Style.STYLE , Style.FABRIC "
  lcStyTable="STYLE "

  IF !EMPTY(lcStySty)
    SELECT(lcStySty)
    lcStySty1 = loOgScroll.gfTempName()
    COPY TO oAriaApplication.WorkDir+lcStySty1+".dbf"
    lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStySty1+".dbf" + " ON  STYLE.CSTYMAJOR = " +lcStySty1+".cStyMajor"
  ENDIF
  IF !EMPTY(lcStyFab)
    SELECT(lcStyFab)
    lcStyFab1 = loOgScroll.gfTempName()
    COPY TO oAriaApplication.WorkDir+lcStyFab1+".dbf"
    lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyFab1+".dbf" + " ON  STYLE.FABRIC = " +lcStyFab1+".cStyMajor"
  ENDIF

  *--Make Temp File For Selected Style Divisions
  lcDivCursor =""
  lnStyDivision = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CDIVISION")
  lnStyDivision = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStyDivision,1)
  lcDivisions= loOgScroll.laOgFxFlt[lnStyDivision,6]
  IF !EMPTY(lcDivisions)
    IF lnStyDivision > 0
      lcDivCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Divisions
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CDIVISION'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.F.)
      IF !EMPTY(lcDivisions)
        lnStart=1
        lnEnd=AT('|',lcDivisions)
        DO WHILE lnEnd <> 0
          SELECT(lcDivCursor)
          APPEND BLANK
          REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
          lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcDivisions)
        ENDDO
        IF lnEnd = 0
          SELECT(lcDivCursor)
          APPEND BLANK
          REPLACE CDIVISION WITH lcDivisions
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcDivCursor)
    USE IN &lcDivCursor
    lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcDivCursor+".dbf" + " ON  STYLE.CDIVISION = " +lcDivCursor+".CDIVISION"
  ENDIF


  *--Make Temp File For Selected Style Seasons
  lcSeaCursor =""
  lnStySeason = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
  lnStySeason = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStySeason,1)
  lcSeasons= loOgScroll.laOgFxFlt[lnStySeason,6]
  IF !EMPTY(lcSeasons)
    IF lnStySeason > 0
      lcSeaCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='SEASON'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcSeaCursor,@laTempacstru,"SEASON",lcSeaCursor,.F.)
      IF !EMPTY(lcSeasons)
        lnStart=1
        lnEnd=AT('|',lcSeasons)
        DO WHILE lnEnd <> 0
          SELECT(lcSeaCursor)
          APPEND BLANK
          REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
          lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcSeasons)
        ENDDO
        IF lnEnd = 0
          SELECT(lcSeaCursor)
          APPEND BLANK
          REPLACE SEASON WITH lcSeasons
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcSeaCursor)
    USE IN &lcSeaCursor
    lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+ lcSeaCursor+".dbf" + " ON  STYLE.SEASON = " +lcSeaCursor+".SEASON"
  ENDIF


  *--Make Temp File For Selected Style Colors
  lcColCursor =""
  lnColorSeg = lnClrSrt-lnClrEnd+1
  lnStyColor = ASCAN(loOgScroll.laOgFXFlt,"SUBSTR(STYLE.STYLE,lnClrSrt,lnClrEnd)")
  lnStyColor = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStyColor,1)
  lcColors= loOgScroll.laOgFxFlt[lnStyColor,6]
  IF !EMPTY(lcColors)
    IF lnStyColor > 0
      lcColCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Colors
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='COLOR'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= lnColorSeg
      laTempacstru[1,4]= 0
      gfCrtTmp(lcColCursor,@laTempacstru,"Color",lcColCursor,.F.)
      IF !EMPTY(lcColors)
        lnStart=1
        lnEnd=AT('|',lcColors)
        DO WHILE lnEnd <> 0
          SELECT(lcColCursor)
          APPEND BLANK
          REPLACE COLOR WITH SUBSTR(lcColors,lnStart,lnEnd-1)
          lcColors = STUFF(lcColors ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcColors)
        ENDDO
        IF lnEnd = 0
          SELECT(lcColCursor)
          APPEND BLANK
          REPLACE COLOR WITH lcColors
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcColCursor)
    USE IN &lcColCursor
    lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+ lcColCursor+".dbf" + " ON  SUBSTR(STYLE.STYLE,"+ALLTRIM(STR(lnClrSrt))+","+ALLTRIM(STR(lnClrEnd))+") = " +lcColCursor+".COLOR"
  ENDIF

  *--Make Temp File For Selected Style Groups
  lcGrpCursor =""
  lnStyGroup = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYGROUP")
  lnStyGroup = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStyGroup,1)
  lcGroups= loOgScroll.laOgFxFlt[lnStyGroup,6]
  IF !EMPTY(lcGroups)
    IF lnStyGroup > 0
      lcGrpCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CSTYGROUP'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcGrpCursor,@laTempacstru,"CSTYGROUP",lcGrpCursor,.F.)
      IF !EMPTY(lcGroups)
        lnStart=1
        lnEnd=AT('|',lcGroups)
        DO WHILE lnEnd <> 0
          SELECT(lcGrpCursor)
          APPEND BLANK
          REPLACE CSTYGROUP WITH SUBSTR(lcGroups,lnStart,lnEnd-1)
          lcGroups = STUFF(lcGroups ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcGroups)
        ENDDO
        IF lnEnd = 0
          SELECT(lcGrpCursor)
          APPEND BLANK
          REPLACE CSTYGROUP WITH lcGroups
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcGrpCursor)
    USE IN &lcGrpCursor
    lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+ lcGrpCursor+".dbf" + " ON  STYLE.CSTYGROUP  = " +lcGrpCursor+".CSTYGROUP "
  ENDIF

  *--B037636 ,1 HMA  10/28/2003 Try to improve the performance.  [BEGIN]
  lcStyCond=""
  *lcStyCond=" !EMPTY(STYLE.FABRIC)"    && Where Condition


  *-- Variable Hold only the Selected Style Pattern From OG.
  lcStyPat = loOgScroll.laOgFxFlt[6,6]

  IF !EMPTY(lcStyPat)
   * lcStyCond= lcStyCond+ "  AND  STYLE.PATTERN = " + "'"+lcStyPat+"'" &&Where Condition
    lcStyCond= "  STYLE.PATTERN = " + "'"+lcStyPat+"'" &&Where Condition
  ENDIF
  *--B037636 ,1 HMA  10/28/2003 Try to improve the performance.  [END]
  *--Retrieve Cursor Hold the Styles after adding all filters which made on Style file.

  =lfSlctFox(lcStyFlds,lcStyTable,lcStyle,lcStyCond)
  *=lfSlctFox(lcStyFlds,lcStyTable,lcStyCond,lcStyle,'STYLE','LCSTYLE')
  *--Close all Created dbfs which joined to Style File to create temp. File for style after Optimization(to enhance Performance)

  IF FILE(oAriaApplication.WorkDir +lcStySty1+".DBF")
    IF USED(lcStySty1)
      USE IN &lcStySty1
    ENDIF
    ERASE oAriaApplication.WorkDir+lcStySty1+".DBF"
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcStyFab1+".DBF")
    IF USED(lcStyFab1)
      USE IN &lcStyFab1
    ENDIF
    ERASE oAriaApplication.WorkDir+lcStyFab1+".DBF"
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcDivCursor+".DBF")
    ERASE oAriaApplication.WorkDir+lcDivCursor+".DBF"
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcSeaCursor+".DBF")
    ERASE oAriaApplication.WorkDir+lcSeaCursor+".DBF"
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcColCursor+".DBF")
    ERASE oAriaApplication.WorkDir+lcColCursor+".DBF"
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcGrpCursor+".DBF")
    ERASE oAriaApplication.WorkDir+lcGrpCursor+".DBF"
  ENDIF
  *-- lcMtCstMth variable that hold the system costing method.
  lcMtCstMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET')))
  *-- lcAvg_Cost : identify costing method.
  llAvg_Cost = IIF(lcMtCstMth = 'A', .T., .F.)
  *-- llCostPrv print cost privilage (per user).
  llCostPrv = gfUserPriv('IC','ICSTYLE','COSTING')

  *-- Variable Hold the Selected Vendor Temp. File.
  *-- Append the selected Vendors to the where condition Of Sql Statement.
  lcSelVend=lfBldSqlCur('APVENDOR.CVENDCODE','llVendor','lcSelVend','Vendor C(8)','CVENDCODE') &&IT'S A CURSOR HOLD THE SELECTED VENDORS
  *-- Append the selected Fabrics to the where condition Of Sql Statement.
  lcSelFab=lfBldSqlCur('ITEM.CSTYMAJOR','llFabric','lcSelFab','Fabric C(19)','CSTYMAJOR') &&IT'S A CURSOR HOLD THE SELECTED FABRBRICS
  *--Create Sql Cursor Filtered by  all the Selection Criteria.

  lcFabric=loogscroll.gftempname()

  lcSelFld2= "SELECT ITEM.STYLE,ITEM.CSTYMAJOR AS FABRIC,ITEM.[DESC],ITEM.CITEMFLD3 AS CONTENT,;
            ITEM.LOCATION AS LOC,ITEM.INTRODUCED,ITEM.ITEM_TYPE,ITEM.VENDOR,ITEM.PATTERN,ITEM.CITEMFLD1 AS WIDTH ,"

  lcSelFld2= lcSelFld2+"ITEM.CITEMFLD2 AS CFABWEIGHT,ITEM.LEADTIME ,ITEM.NICOST1 AS NFABCOST ,ITEM.NICOST2 AS NITEM_TAX ,;
                      ITEM.NICOST3 AS NITEMQUATA,ITEM.NICOST4 AS NITM_FRT,ITEM.NTOTREORD AS REORDER, ITEM.AVE_COST ,"

  lcSelFld2= lcSelFld2 + "ITEM.TOTCOST AS COSTBUY ,ITEM.NAVECSTBUY ,ITEM.CDYE_FLG , UOM.CUOM_B AS UOMUSE,;
                        UOM.CUOM_V AS UOMBUY ,UOM.NCONF AS CONV FROM ITEM INNER JOIN UOM ON ITEM.CCONVBUY = UOM.CUOMCODE "

  *--Add the filter of selected Items.
  IF llFabric  && Fabrics selected in the in list browse
    lcSelFld2 = lcSelFld2 + " INNER JOIN " + lcSelFab + " TmpFabric ON TmpFabric.Fabric = ITEM.CSTYMAJOR "
  ENDIF

  *--Add filter of selected vendors.
  IF llVendor
    lcSelFld2 = lcSelFld2 + " INNER JOIN " + lcSelVend + " TmpVendor ON TmpVendor.Vendor = ITEM.VENDOR "
  ENDIF

  *--Add the Filter of Inventory Type (Fabric OR Style).
  lcSelFld2= lcSelFld2 + "  WHERE ITEM.CINVTYPE= '" +lcInvType+"'"

  *--Add Filter of lcRpSqlExp
*HMA ,07/12/2005 ,remove expression of fixed filter 'ITEM.CSTYMAJOR' from lcRpSqlExp [Begin]
  lcRpSqlExp=loOgScroll.lcRpSqlExp
  IF OCCURS('ITEM.CSTYMAJOR',lcRpSqlExp)> 0
   lnStartPos= ATC('ITEM.CSTYMAJOR',lcRpSqlExp)
   lnEndPos = ATC(")",lcRpSqlExp)
   IF ATC('AND',lcRpSqlExp) >0
     lcRetExp=SUBSTR(lcRpSqlExp,lnStartPos,lnEndPos - lnStartPos+5)
   ELSE
     lcRetExp=SUBSTR(lcRpSqlExp,lnStartPos,lnEndPos - lnStartPos+1)
   ENDIF
   lcRpSqlExp=STRTRAN(lcRpSqlExp,lcRetExp,"")
  ENDIF
*HMA ,07/12/2005 ,remove expression of fixed filter 'ITEM.CSTYMAJOR' from lcRpSqlExp [End]
  IF !EMPTY(lcRpSqlExp)
    lcSelFld2= lcSelFld2 + " AND " +lcRpSqlExp
  ENDIF
  lnResult2 = loOGScroll.oRDA.SqlRun(lcSelFld2,lcFabric,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))

  *--Add Filter of Onhand Qty
  lcOnHand_Q = IIF(llRPOnhand,'Y','N')
  lcWherCond=""
  IF lcOnHand_Q= 'Y'
    IF EMPTY(lcWherCond)
      lcWherCond= "&lcFabDye..ONHAND <> 0 "
    ELSE
      lcWherCond= lcWherCond + " AND " + "&lcFabDye..ONHAND <> 0 "
    ENDIF
  ENDIF

  *--Create another File(FABDYE1) to hold FABDYE Fields with filters needed to be made on it
  lcFabDye1=loOgScroll.gftempname()
  IF !EMPTY(lcWherCond )
    SELECT *  FROM &lcFabDye WHERE &lcWherCond INTO CURSOR &lcFabDye1
  ELSE
    SELECT *  FROM &lcFabDye INTO CURSOR &lcFabDye1
  ENDIF

  SELECT(lcFabDye1)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye1

  IF lnResult2 >=1
    SELECT (lcFabric)
    lnBuffering = CURSORGETPROP("Buffering",lcFabric)
    =CURSORSETPROP("Buffering",3,lcFabric)
    INDEX ON STYLE TAG lcFabric
    SET RELATION TO STYLE INTO &lcFabDye1 ADDITIVE
  ENDIF

  *--Create TempFile with the Same Structure of the Fabric Cursor(lcFabric)
  lcTempFile   = loOgScroll.gfTempName()
  *--Create Work File
  lcWorkFile   = loOgScroll.gfTempName()
  SELECT (lcFabric)
  =AFIELDS(laFileStru)
  DIMENSION laIndex[2,2]
  laIndex[1,1] ="STYLE"
  laIndex[1,2] ='STYLE'
  laIndex[2,1] ="FABRIC"
  laIndex[2,2] = 'FABRIC'

  =gfCrtTmp(lcTempFile,@laFileStru,@laIndex,lcTempFile,.T.)

  *-----------------------------------------Start Collecting Data------------------------------------------
  SELECT (lcTempFile)
  SET ORDER TO FABRIC
  SELECT (lcStyle)
  SET ORDER TO TAG LCSTYLE
  SCAN
    lcStyFabric = FABRIC
  *--B037636 ,1 HMA  10/28/2003 Try to improve the performance.  [BEGIN]
    IF !EMPTY(lcStyFabric)
  *--B037636 ,1 HMA  10/28/2003 Try to improve the performance.  [END]
      SELECT (lcTempFile)
      IF !SEEK(lcStyFabric) && If selected Fabric doesn't exists in  lcTempFile
        SELECT (lcFabric)
        IF SEEK(lcStyFabric) && If Selected Fabric exists in  Item File (Cursor lcFabric)
          COPY TO (oAriaApplication.WorkDir+lcWorkFile) WHILE  ALLTRIM(lcStyFabric) = ALLTRIM(&lcFabric..FABRIC) && Now lcWorkFile hold the Fabrics After Filters Added For it .
          SELECT (lcTempFile)
          IF !EMPTY(oAriaApplication.WorkDir+lcWorkFile)
            APPEND FROM (oAriaApplication.WorkDir+lcWorkFile)  && Now lcTempFile hold the Items According to the Selected Styles Filter
          ENDIF
        ENDIF
      ENDIF
  *--B037636 ,1 HMA  10/28/2003 Try to improve the performance.  [BEGIN]
    ENDIF
  *--B037636 ,1 HMA  10/28/2003 Try to improve the performance. [END]
    SELECT (lcStyle)
  ENDSCAN




  SELECT (lcTempFile)
  SET ORDER TO STYLE
  GO TOP
  IF EOF()
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  *--Create Temp File to collect Data in order to  link it With Crystal.
  lcMaterial = loOgScroll.gfTempName()

  SELECT (lcTempFile)
  =AFIELDS(laFileStru)
  lnArrLen = ALEN(laFileStru,1)

	DIMENSION laFileStru[lnArrLen+7,18]
	laFileStru[lnArrLen+1,1] = 'ClrDesc'
	laFileStru[lnArrLen+1,2] = 'C'
	laFileStru[lnArrLen+1,3] = 12
	laFileStru[lnArrLen+1,4] = 0

	laFileStru[lnArrLen+2,1] = 'ItemDesc'
	laFileStru[lnArrLen+2,2] = 'C'
	laFileStru[lnArrLen+2,3] = 20
	laFileStru[lnArrLen+2,4] = 0

	laFileStru[lnArrLen+3,1] = 'Usage'
	laFileStru[lnArrLen+3,2] = 'N'
	laFileStru[lnArrLen+3,3] = 12
	laFileStru[lnArrLen+3,4] = 3

	laFileStru[lnArrLen+4,1] = 'OnHand'
	laFileStru[lnArrLen+4,2] = 'N'
	laFileStru[lnArrLen+4,3] = 12
	laFileStru[lnArrLen+4,4] = 3

	laFileStru[lnArrLen+5,1] = 'OnOrder'
	laFileStru[lnArrLen+5,2] = 'N'
	laFileStru[lnArrLen+5,3] = 12
	laFileStru[lnArrLen+5,4] = 3

	laFileStru[lnArrLen+6,1] = 'nStkVal'
	laFileStru[lnArrLen+6,2] = 'N'
	laFileStru[lnArrLen+6,3] = 12
	laFileStru[lnArrLen+6,4] = 2

	laFileStru[lnArrLen+7,1] = 'Color'
	laFileStru[lnArrLen+7,2] = 'C'
	laFileStru[lnArrLen+7,3] = lnColorLen
	laFileStru[lnArrLen+7,4] = 0

  FOR lncnt=7 TO 16
    FOR lnInc=1 TO 7
      STORE SPACE(0) TO laFileStru[lnArrLen+lnInc,lnCnt]
    ENDFOR
  ENDFOR
  FOR lnInc=1 TO 7
    STORE 0 TO laFileStru[lnArrLen+lnInc,17],laFileStru[lnArrLen+lnInc,18]
  ENDFOR

  IF lcRPSortBy='I'
    lcSrtByIdx = "STYLE"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortName = LANG_MAMATST_ItemSort
lcSortName = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAMATST_ItemSort,oAriaApplication.GetHeaderText("LANG_MAMATST_ItemSort",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    lcSrtByIdx = "VENDOR+STYLE"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortName = LANG_MAMATST_ItemVendor
lcSortName = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAMATST_ItemVendor,oAriaApplication.GetHeaderText("LANG_MAMATST_ItemVendor",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF

  =gfCrtTmp(lcMaterial,@laFileStru,lcSrtByIdx,lcMaterial,.F.)

  *--Create Temp File with the Same Structur of lcFabDye1 File to link it with Crystal.
  lcDyelots = loOgScroll.gfTempName()
  SELECT (lcFabDye1)
  =AFIELDS(laFileStru)
  =gfCrtTmp(lcDyelots,@laFileStru,"STYLE+CWARECODE+DYELOT",lcDyelots,.F.)
  SELECT(lcDyelots)

  SELECT (lcTempFile)
  SET RELATION TO STYLE INTO &lcFabDye1 ADDITIVE
  *--B037636 ,2 HMA  10/28/2003 Fix onhand Qty Filter. [BEGIN]
  SELECT (lcFabDye1)
  LOCATE
  IF EOF()
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_MAMATST_NoRecord NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAMATST_NoRecord,oAriaApplication.GetHeaderText("LANG_MAMATST_NoRecord",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN
  ENDIF
  *--B037636 ,2 HMA  10/28/2003 Fix onhand Qty Filter. [END]
  *--One Loop To Collect all of Data.
  SELECT (lcTempFile)
  SCAN
  *--B037636 ,2 HMA  10/28/2003 Fix onhand Qty Filter. [BEGIN]
    IF &lcTempFile..STYLE <> &lcFabDye1..STYLE
      LOOP
    ENDIF
  *--B037636 ,2 HMA  10/28/2003 Fix onhand Qty Filter. [END]
    SELECT (lcTempFile)
    lcClrDesc = SUBSTR(gfCodDes(SUBSTR(&lcTempFile..STYLE,lnMajorlen+2,lnColorLen),'COLOR'),1,12)
    lcTyp_Desc = gfCodDes(ITEM_TYPE,'ITEM_TYPE')
    SCATTER MEMVAR memo
    SELECT(lcMaterial)
    APPEND blank
    GATHER MEMVAR memo
    REPLACE ONHAND     WITH lfSumFab2(STYLE,'ONHAND'),;
            ONORDER    WITH lfSumFab2(STYLE,'ONORDER'),;
            ClrDesc    WITH lcClrDesc,;
            ItemDesc   WITH lcTYP_DESC,;
            USAGE      WITH lfSumFab2(STYLE,'USAGE'),;
            NSTKVAL    WITH lfSumFab2(STYLE,'NSTKVAL') ,;
            COLOR      WITH SUBSTR(&lcTempFile..STYLE,lnMajorlen+1,lnColorLen)
    *--Check Dyelots Information
    *--in all Cases we will Fill the lcDyelots file with the Style (to make link in Crystal)
    SELECT (lcFabDye1)
    IF SEEK(&lcTempFile..Style)
      SCAN REST WHILE &lcFabDye1..STYLE+&lcFabDye1..CWARECODE+&lcFabDye1..DYELOT = &lcTempFile..Style
        IF !EMPTY(&lcFabDye1..Dyelot)
          SCATTER MEMVAR MEMO
          SELECT (lcDyelots)
          APPEND BLANK
          GATHER MEMVAR MEMO
        ELSE
          IF !SEEK(&lcFabdye1..Style,lcDyelots)
            INSERT INTO &lcDyelots (Style) VALUES (&lcFabDye1..Style)
          ENDIF
        ENDIF
      ENDSCAN
    ELSE
      IF !SEEK(&lcTempFile..Style,lcDyelots)
        INSERT INTO &lcDyelots (Style) VALUES (&lcTempFile..Style)
      ENDIF
    ENDIF
  ENDSCAN


  *--Running the Crystal Report.
  lcLayOut=""
  DO CASE
  *:B127322 ,1 ASM Fix Not Prinitng Summary [Start]
  *!*	  CASE lcRpForm = "MAMATST"
  *!*	   lcLayOut = LANG_MAMATST_Detail
  *!*	  CASE lcRpForm = "MAMATSTA"
  *!*	   lcLayOut = LANG_MAMATST_Summary
    CASE lcRpForm = "MAMATSTA"
     *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcLayOut = LANG_MAMATST_Summary
lcLayOut = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAMATST_Summary,oAriaApplication.GetHeaderText("LANG_MAMATST_Summary",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE lcRpForm = "MAMATST"
     *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcLayOut = LANG_MAMATST_Detail
lcLayOut = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAMATST_Detail,oAriaApplication.GetHeaderText("LANG_MAMATST_Detail",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *:B127322 ,1 ASM Fix Not Prinitng Summary [End]
  ENDCASE


  DIMENSION loOGScroll.laCRTables[2]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcMaterial  + ".DBF"
  loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcDyelots  + ".DBF"


  DIMENSION loOGScroll.laCRParams[8,2]

  loOGScroll.laCRParams[1,1] = 'SortBy'
  loOGScroll.laCRParams[1,2] = lcSortName
  loOGScroll.laCRParams[2,1] = 'ReportName'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOGScroll.laCRParams[2,2] = LANG_MAMATST_ReportTitle
loOGScroll.laCRParams[2,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAMATST_ReportTitle,oAriaApplication.GetHeaderText("LANG_MAMATST_ReportTitle",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loOGScroll.laCRParams[3,1] = 'LayOut'
  loOGScroll.laCRParams[3,2] = lcLayOut
  loOGScroll.laCRParams[4,1] = 'llCostPrv'
  loOGScroll.laCRParams[4,2] = llCostPrv
  loOGScroll.laCRParams[5,1] = 'llAvg_Cost'
  loOGScroll.laCRParams[5,2] = llAvg_Cost
  loOGScroll.laCRParams[6,1] = 'llDyelot'
  loOGScroll.laCRParams[6,2] = llDyelot
  loOGScroll.laCRParams[7,1] = 'llRpPrnDye'
  loOGScroll.laCRParams[7,2] = llRpPrnDye
  loOGScroll.laCRParams[8,1] = 'lcRpSortBy'
  loOGScroll.laCRParams[8,2] = lcRpSortBy


  USE IN &lcMaterial
  USE IN &lcDyelots

  DO CASE
    *:B127322 ,1 ASM Fix Not Prinitng Summary [Start]
  *!*	  CASE lcRpForm = "MAMATST"
  *!*	   loOgScroll.lcOgLastForm= "MAMATST"
  *!*	  CASE lcRpForm = "MAMATSTA"
  *!*	   loOgScroll.lcOgLastForm= "MAMATSTA"
    CASE lcRpForm = "MAMATSTA"
     loOgScroll.lcOgLastForm= "MAMATSTA"
    CASE lcRpForm = "MAMATST"
     loOgScroll.lcOgLastForm= "MAMATST"
    *:B127322 ,1 ASM Fix Not Prinitng Summary [End]
  ENDCASE
  *--To Display report in PDF in landscape layout
  loOgScroll.cCROrientation = 'L'

  *--Display the report.
  DO gfDispRe

ELSE
*EE126798 ,1 HMA 04/18/2005 use Filter Change Variable in order to improve tha performance,Don't recollect Data if nothing change in Option Grid  [Begin]
  IF !USED(lcTempFile)
    USE oAriaApplication.WorkDir +  lcTempFile  + ".DBF" IN 0
  ENDIF
  SELECT (lcTempFile)
  SET ORDER TO STYLE
  GO TOP
  IF EOF()
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
    IF USED(lcMaterial)
      USE IN &lcMaterial
    ENDIF
  ELSE
    *--Display the report.
    DO gfDispRe
  ENDIF
*E126798 ,1 HMA 04/18/2005 use Filter Change Variable in order to improve tha performance,Don't recollect Data if nothing change in Option Grid [End]
ENDIF

*-- end of report code

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
LOCAL lnResult1

*--Set needed orders in grid.
SELECT STYLE
SET ORDER TO TAG Style

*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))

*--Make Temp. File From Item Location File
IF oAriaApplication.ActiveModuleID = 'MA' .AND. !llFrstTime
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC "

  lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"

  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))

  llFrstTime = .T.

ENDIF


IF lnResult1 >=1
  lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
  =CURSORSETPROP("Buffering",3,lcFabDye)
  SELECT (lcFabDye)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
ENDIF




*--Check dyelot option in MA Module Setup
llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
LNPRNDYEP = ASUBSCRIPT(LOOGSCROLL.LAOGOBJTYPE,ASCAN(LOOGSCROLL.LAOGOBJTYPE,'LLRPPRNDYE'),1)
LOOGSCROLL.LAOGOBJCNT[LNPRNDYEP] = LLDYELOT
= LFOGSHOWGET('LLRPPRNDYE')

*--Get the style major picture.
lcStyPict = '@! '+gfItemMask('PM')
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()
RETURN

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
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
*! Name      : lfSRVSty
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style   IN  0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    LOCATE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
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
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)

lnTotcomp = 0
SELECT Style_X
SET ORDER TO Style
IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF

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
*! Name      : lfSumFab1
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
*! Purpose   : sum a specific field for the current fabric
*!                  in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,Item browse calculated fields.
*!*************************************************************
*! Passed Parameters  : (Item.CSTYMAJOR,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab1()
*!*************************************************************

FUNCTION lfSumFab1
*!*	PARAMETERS lcFab,lcComp
*!*	LOCAL lnAlias

*!*	lnAlias = SELECT()

*!*	SELECT ITEM
*!*	lnRecoNo = IIF(BETWEEN(RECNO('ITEM'),1,RECCOUNT('ITEM')),RECNO('ITEM'),0)

*!*	SELECT(lcFabDye)
*!*	lnTotComp = 0
*!*	IF lnRecoNo > 0
*!*	  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
*!*	    SUM &lcCOMP TO lnTotComp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT ITEM
*!*	susp
*!*	GOTO lnRecoNo

*!*	SELECT(lnAlias)

*!*	RETURN INT(lnTotcomp)

PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias
lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcFabDye)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcFabDye)
  LOCATE
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF

SELECT(lnAlias)

RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfBldSqlCur
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/12/2004
*! Purpose   : Build Sql Cursors Needed For Getting Data
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : = lfBldSqlCur()
*!*************************************************************

FUNCTION lfBldSqlCur
PARAMETERS lcFilter,lcFound,lcCursor,lcFldName,lcSntFld
LOCAL   lnPosition,lnFltPos,lnRow,lcExpression
STORE 0 TO lnPosition,lnFltPos,lnRow
STORE '' TO lcExpression
lnFltPos = ASCAN(loOGScroll.laOGFxFlt,lcFilter)
IF lnFltPos > 0
  lnRow = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnFltPos,1)
  lcTmpCur = loOGScroll.laOGFxFlt[lnRow,6]
  IF !EMPTY(lcTmpCur)  &&user selected some styles.
    SELECT &lcTmpCur
    &lcFound = (RECCOUNT() > 0)
    IF &lcFound
      &lcCursor = loOgScroll.gfSQLTempName('',lcFldName,lcTmpCur,lcSntFld) && SQL Temp File
      IF EMPTY(&lcCursor)
        *-- SQL connection Error. Can't open The Report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF
RETURN &lcCursor
*!*************************************************************
*! Name      : lfSumFab2
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/14/2004
*! Purpose   : sum a specific field for the current fabric
*!                  in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab2()
*!*************************************************************


FUNCTION lfSumFab2
PARAMETERS lcFab,lcComp

lnTotComp = 0
lnSelected=SELECT()
SELECT(lcFabDye1)
IF RECCOUNT() != 0
  lnStyRec = IIF(BETWEEN(RECNO(lcFabDye1),1,RECCOUNT(lcFabDye1)),RECNO(lcFabDye1),1)  && Return the Current Record
  IF SEEK(ALLTRIM(lcFab))
*HMA
*    SUM &lcCOMP TO lnTotComp WHILE  ALLTRIM(STYLE) = ALLTRIM(lcFab)  .AND. EMPTY(&lcFabDye1..DYELOT)
    SUM &lcCOMP TO lnTotComp WHILE  STYLE = PADR(lcFab,19)  .AND. EMPTY(&lcFabDye1..DYELOT)
*HMA
  ENDIF
  GO lnStyRec
ENDIF
SELECT(lnSelected)

*B125737 ,1 HMA  01/09/2005 Return the whole vaue of numeric field value instead of return only its integer value [BEGIN]
*RETURN INT(lnTotcomp)
RETURN lnTotcomp
*B125737 ,1 HMA  01/09/2005 Return the whole vaue of numeric field value instead of return only its integer value [END]


*!*************************************************************
*! Name      : lfSlctFox
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/14/2004
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   : =lfSlctFox()
*!*************************************************************

FUNCTION lfSlctFox

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

IF TYPE("loRDA1") <> 'O'
  loRDA1 = CREATEOBJECT("RemoteDataAccess")
ENDIF
lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM  " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")
lnConnectionHandlar = loRDA1.sqlrun(lcSqlStatment,lcCursor,,oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF

RETURN
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 17/08/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE

  CASE UPPER(lcTable) = lcStyle
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'STYLE'
    laIndex[1,2] = 'LCSTYLE'


ENDCASE













