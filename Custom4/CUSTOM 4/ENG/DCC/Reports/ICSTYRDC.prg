*:***************************************************************************
*: Program file  : ICSTYRDC
*: Program desc. : Custom Style Summary
*: For Report    : ICSTYDCS.FRX,ICSTYDCO.FRX,ICSTYDCL.FRX,ICSTYDCW.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar(MMT)  C201692(T20150515.0010) 
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSTYREP
*:***************************************************************************
*: Modifications:
*: C201692,1 MMT 07/07/2015 Add Option to export -ve OTS to CSV file[T20150317.0018] 
*: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001]
*:**********************************************************************************************************************
#INCLUDE R:\Aria4xp\reports\ICSTYREP.H

ON ERROR
IF ALEN(laRPRepTar,1) = 0 OR EMPTY(laRPRepTar)
  *-- "You have to select transaction to be printed"
  *-- <OK>
  = gfModalGen("INM42146B00000",LANG_ICSTYREP_Dialog)
  RETURN .F.
ENDIF

*:Media Tersting: AYM the layout looks landscape ..Begin
loogScroll.cCROrientation = 'P'
*:Media Tersting: AYM the layout looks landscape .. End


*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*B608163,1 TMI [Start]
llGetNext = .F.
lcGetNext = ' '
*B608163,1 TMI [End  ]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]

IF loOGScroll.llOGFltCh
  lcFabric1=""
  lcStyle1=""
  *------------ Retrieve all Fox files used in program remotely [Begin]
  *--If no Item Type Selected and Fabric Selected (A)
  *--If Item Type Selected and Fabric Selected(B)
  *--If Item Type Selected and no Fabric Selected(B)
  *--If Sort by Material Type & no fabrics selected & no Item_type selected ,Retrieve all Fabric File&set relation with Style file(C)
  *-- 1.Style file  [BEGIN]
  lcStySty = ""
  lcStyFab =""
  lcStyLoc=""
  *-- Variable Hold only the Selected Styles From OG.
  lcStySty = loOgScroll.laOgFxFlt[1,6]
  *-- Variable Hold the Primary Fabrics Selected for Styles.
  lcStyFab = loOgScroll.laOgFxFlt[2,6]
  *-- Variable Hold the Selected Departments Selected for Styles.
  lcStyDept = loOgScroll.laOgFxFlt[11,6]
  lcStySty1 =""
  lcStyFab1 =""
  lcStyDept1 =""
  *--Retrieve Cursor From Style File(Fox File)which hold only Styles which had Fabrics.
  *! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
  *lcStyFlds="style.* "
  lcStyFlds="style.*, gfCodDes(SUBSTR(style.Style,lnClrPo,lnColorLen),'COLOR     ') as Color "
  *! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
  lcStyTable="STYLE "

  lcTypeVal=""   &&Variable Hold Item_Type(Material Type)
  IF !EMPTY(loOgScroll.laOgFxflt[12,6])
    lcTypeVal="'"+STRTRAN(loOgScroll.laOgFxflt[12,6],"|","','")+"'"
  ENDIF

  *: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
  *B608163,1TMI [Start] if the user has not selected any style then fill temp style with all styles to be used in the sql select
  IF ALEN(laRpRepTar,1)=1 && This change will be made only in case of one transactin is selected
    IF EMPTY(lcStySty)
      lcStySty = loOgScroll.gfTempName()
      CREATE TABLE (oAriaApplication.WorkDir+lcStySty) (KEYEXP C(19),CSTYMAJOR C(19))
      INDEX ON KEYEXP TAG &lcStySty
    ENDIF
    IF USED(lcStySty)
      SELECT (lcStySty)
      lnCnt = 0
      COUNT TO lnCnt
      IF lnCnt = 0
        SELECT STYLE
        lcSvOrder = ORDER('STYLE')
        =gfSetOrder('CSTYLE')
        =gfSeek('')
        SCAN
          INSERT INTO (lcStySty) (KEYEXP , CSTYMAJOR ) VALUES (STYLE.CSTYMAJOR,STYLE.CSTYMAJOR)
        ENDSCAN
        =gfSetOrder(lcSvOrder)
      ENDIF
    ENDIF
  ENDIF
  *B608163,1TMI [End  ]
  *: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]

  IF !EMPTY(lcStySty) AND USED(lcStySty)
    SELECT(lcStySty)
    *: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
    *B608163,1TMI [Start] to fix a bug that when select a style and then to not unselect it from the browse a
    *                           no record found messages appears
    *IF RECCOUNT() > 0
    lnCnt = 0
    COUNT TO lnCnt
    IF lnCnt > 0
      *B608163,1TMI [End  ]
      *: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[ENd]
      lcStySty1 = loOgScroll.gfTempName()
      COPY TO oAriaApplication.WorkDir+lcStySty1+".dbf"
      lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStySty1+".dbf" + " ON  STYLE.CSTYMAJOR = " +lcStySty1+".cStyMajor"
    ENDIF
  ENDIF
  *--(A)
  IF !EMPTY(lcStyFab) AND EMPTY(lcTypeVal)
    SELECT(lcStyFab)
    IF RECCOUNT() > 0
      lcStyFab1 = loOgScroll.gfTempName()
      COPY TO oAriaApplication.WorkDir+lcStyFab1+".dbf"
      lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyFab1+".dbf" + " ON  STYLE.FABRIC = " +lcStyFab1+".cStyMajor"
    ENDIF
  ENDIF
  *--(A)
  IF !EMPTY(lcStyDept) AND USED(lcStyDept)
    SELECT(lcStyDept)
    IF RECCOUNT() > 0
      lcStyDept1 = loOgScroll.gfTempName()
      COPY TO oAriaApplication.WorkDir+lcStyDept1+".dbf"
      lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyDept1+".dbf" + " ON  STYLE.DEPT = " +lcStyDept1+".Dept"
    ENDIF
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
  lnColorSeg = lnClrPo-lnColorLen+1
  lnStyColor = ASCAN(loOgScroll.laOgFXFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
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
    lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+ lcColCursor+".dbf" + " ON  SUBSTR(STYLE.STYLE,"+ALLTRIM(STR(lnClrPo))+","+ALLTRIM(STR(lnColorLen))+") = " +lcColCursor+".COLOR"
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

  *--(B)
  *-- Retrieve cursor hold the required fields From item file [Begin]
  IF  !EMPTY(lcTypeVal) && if the user select some fabrics or some Item_types
    IF !EMPTY(lcStyFab) AND  USED(lcStyFab) AND  RECCOUNT(lcStyFab)> 0
      llFabric=.F.
      lcSelFab=""
      lcFabric=loOgScroll.gftempname()
      lcSelFld1= "SELECT DISTINCT ITEM.CSTYMAJOR AS FABRIC ,ITEM.ITEM_TYPE FROM ITEM "
      *--Add Filter of Selected Primary Fabrics
      lcSelFab=lfBldSqlCur('ITEM.CSTYMAJOR','llFabric','lcSelFab','Fabric C(19)','CSTYMAJOR') &&IT'S A CURSOR HOLD THE SELECTED FABRBRICS
      IF llFabric  && Fabrics selected in the in list browse
        lcSelFld1 = lcSelFld1 + " INNER JOIN " + lcSelFab + " TmpFabric ON TmpFabric.Fabric = ITEM.CSTYMAJOR "
      ENDIF
    ELSE
      lcSelFld1= "SELECT DISTINCT ITEM.CSTYMAJOR AS FABRIC ,ITEM.ITEM_TYPE FROM ITEM "
    ENDIF
    *--Add the Filter of Inventory Type (Fabric OR Style).
    lcSelFld1= lcSelFld1 + "  WHERE ITEM.CINVTYPE= '" +lcInvType+"'"
    *--Add the Item Type Filter.
    IF !EMPTY(lcTypeVal)
      lcSelFld1= lcSelFld1 + " AND  ITEM.ITEM_TYPE  IN (" + lcTypeVal +")"
    ENDIF
    lnResult1 = loOGScroll.oRDA.SqlRun(lcSelFld1,lcFabric,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))

    IF lnResult1 >=1
      SELECT (lcFabric)
      lnBuffering = CURSORGETPROP("Buffering",lcFabric)
      =CURSORSETPROP("Buffering",3,lcFabric)
      INDEX ON FABRIC TAG lcFabric
    ENDIF
    *-- Retrieve cursor hold the required fields From item file [End]

    *--Filter the style file according to the selected fabrics&item_types [Begin]
    IF !EMPTY(lcFabric) AND USED(lcFabric) AND RECCOUNT(lcFabric) > 0
      SELECT(lcFabric)
      lcFabric1 = loOgScroll.gfTempName()
      COPY TO oAriaApplication.WorkDir+lcFabric1+".dbf"
      lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcFabric1 +".Dbf" + " ON  STYLE.Fabric = " + lcFabric1 +".Fabric"
    ENDIF
    *--Filter the style file according to the selected fabrics&item_types [End]

  ENDIF  &&End of Filter the Fabric File & style file according to this fabric File
  *--(B)
  lcStyCond=""
  *-- Variable Hold only the Selected Style Pattern From OG.
  lcStyPat = loOgScroll.laOgFxFlt[7,6]

  IF !EMPTY(lcStyPat)
    lcStyCond= "  STYLE.PATTERN = " + "'"+lcStyPat+"'" &&Where Condition
  ENDIF

  *--Create variable have the selected status
  lcStatusVal =""
  IF !EMPTY(loOgScroll.laOgFxflt[8,6])
    lcStatusVal ="'"+STRTRAN(loOgScroll.laOgFxflt[8,6],"|","','")+"'"
    IF !EMPTY(lcStyCond)
      lcStyCond= lcStyCond + " AND INLIST(STYLE.STATUS ," + lcStatusVal  + ") " &&Where Condition
    ELSE
      lcStyCond= " INLIST(STYLE.STATUS ,"+ lcStatusVal  + ") " &&Where Condition
    ENDIF
  ENDIF

  *--Add filter of domestic&imported styles
  IF lcRPDomImp <> 'B'
    IF lcRPDomImp = 'D'   &&Domestic
      IF !EMPTY(lcStyCond)
        lcStyCond= lcStyCond + " AND STYLE.Make = .T. "  &&Where Condition
      ELSE
        lcStyCond= "STYLE.Make =.T. " &&Where Condition
      ENDIF
    ELSE     &&Imported
      IF !EMPTY(lcStyCond)
        lcStyCond= lcStyCond + " AND STYLE.Make = .F. "  &&Where Condition
      ELSE
        lcStyCond= "STYLE.Make =.F. " &&Where Condition
      ENDIF
    ENDIF
  ENDIF

  *--Retrieve Cursor Hold the Styles after adding all filters which made on Style file.

  *B127396,1 AMH Fix bug of missing styles in case of select no styles [Start]
  *=lfSlctFox(lcStyFlds,lcStyTable,lcStyle,lcStyCond)
  *B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [Start]
  *IF EMPTY(lcStyCond)
  IF EMPTY(lcStyCond) AND !("INNER JOIN" $ lcStyTable)
    *B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [End]
    lcStyle = 'Style'
  ELSE
    *B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [Start]
    IF lcStyle = 'Style'
      lcStyle = Loogscroll.gfTempName()
    ENDIF
    *B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [End]
    =lfSlctFox(lcStyFlds,lcStyTable,lcStyle,lcStyCond)
  ENDIF
  *B127396,1 AMH [End]

  *--Close all Created dbfs which joined to Style File to create temp. File for style after Optimization(to enhance Performance)
  IF FILE(oAriaApplication.WorkDir +lcStySty1+".DBF")
    IF USED(lcStySty1)
      USE IN &lcStySty1
    ENDIF

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcStySty1+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcStyFab1+".DBF")
    IF USED(lcStyFab1)
      USE IN &lcStyFab1
    ENDIF
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcStyFab1+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcStyDept1+".DBF")
    IF USED(lcStyDept1)
      USE IN &lcStyDept1
    ENDIF
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcStyDept1+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcDivCursor+".DBF")
    **: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcDivCursor+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcSeaCursor+".DBF")
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcSeaCursor+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcColCursor+".DBF")
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcColCursor+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcGrpCursor+".DBF")
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcGrpCursor+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ENDIF
  IF FILE(oAriaApplication.WorkDir +lcFabric1+".DBF")
    IF USED(lcFabric1)
      USE IN &lcFabric1
    ENDIF
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *ERASE oAriaApplication.WorkDir+lcFabric1+".DBF"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ENDIF
  *--check if there is no records with the previous criteria in style file,Display message No Records to Display
  SELECT (lcStyle)
  LOCATE
  IF EOF()
    =gfModalGen('TRM00052B40011',LANG_ICSTYREP_ALERT)
    RETURN
  ENDIF
  *-- 1.Style file  [END]

  *-- 2.StyDye file  [BEGIN]

  *-- Variable Hold only the Selected locations From OG.
  lcStyLoc = loOgScroll.laOgFxFlt[3,6]
  lcStyLoc1 =""
  *--Retrieve Cursor From Style File(Fox File)which hold only Styles which had the selected locations.
  *! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
  *lcStyFlds="styDye.* "
  lcStyFlds="styDye.*,gfCodDes(SUBSTR(styDye.Style,lnClrPo,lnColorLen),'COLOR     ') as Color "
  *! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
  lcStyTable="STYDYE "

  IF !EMPTY(lcStyLoc)
    SELECT(lcStyLoc)
    IF RECCOUNT() > 0
      lcStyLoc1 = loOgScroll.gfTempName()
      COPY TO oAriaApplication.WorkDir+lcStyLoc1+".dbf"
      lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyLoc1+".dbf" + " ON  STYDYE.CWARECODE = " +lcStyLoc1+".CWARECODE"
    ENDIF
  ENDIF

  *HMA ,to improve the performance(Begin)
  IF !EMPTY(lcStyle) AND USED(lcStyle)
    SELECT(lcStyle)
    IF RECCOUNT() > 0
      lcStyle1 = loOgScroll.gfTempName()
      COPY TO oAriaApplication.WorkDir+lcStyle1+".dbf"
      lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyle1+".dbf" + " ON  STYDYE.STYLE = " +lcStyle1+".STYLE"
    ENDIF
  ENDIF
  *HMA ,to improve the performance(End)
  lcStyCond=""
  =lfSlctFox(lcStyFlds,lcStyTable,lcStyDye,lcStyCond)

  IF FILE(oAriaApplication.WorkDir +lcStyLoc1+".DBF")
    IF USED(lcStyLoc1)
      USE IN &lcStyLoc1
    ENDIF
    ERASE oAriaApplication.WorkDir+lcStyLoc1+".DBF"
  ENDIF
  *HMA ,to improve the performance(Begin)
  IF FILE(oAriaApplication.WorkDir +lcStyle1+".DBF")
    IF USED(lcStyle1)
      USE IN &lcStyle1
    ENDIF
    ERASE oAriaApplication.WorkDir+lcStyle1+".DBF"
  ENDIF
  *HMA , to improve the performance(End)
  *--check if there is no records with the previous criteria in styDye file,Display message No Records to Display
  SELECT (lcStyDye)
  LOCATE
  IF EOF()
    =gfModalGen('TRM00052B40011',LANG_ICSTYREP_ALERT)
    RETURN
  ENDIF
  *-- 2.StyDye file  [END]

  *-- 5.WHSLOC file  [BEGIN]
  lcStyFlds="WhsLoc.Style,WhsLoc.Color,WhsLoc.cWareCode,WhsLoc.cLocation,WhsLoc.cOwner "
  lcStyTable="WHSLOC"
  lcStyCond=""
  =lfSlctFox(lcStyFlds,lcStyTable,lcWhsLoc,lcStyCond)
  *-- 5.WHSLOC file  [END]

  *-- 6.SCALE file  [BEGIN]

  * B131649,1 AYM 03/29/2006 Fix Bug of Scale.cscl_desc [Begin]
  *!*	  lcStyFlds="Scale.Type,Scale.Scale,Scale.PrePak,Scale.SZ1,Scale.SZ2,Scale.SZ3,Scale.SZ4,Scale.SZ5,Scale.SZ6,Scale.SZ7,Scale.SZ8,Scale.Cnt "
  lcStyFlds="Scale.Type,Scale.Scale,Scale.PrePak,Scale.SZ1,Scale.SZ2,Scale.SZ3,Scale.SZ4,Scale.SZ5,Scale.SZ6,Scale.SZ7,Scale.SZ8,Scale.Cnt,Scale.Cscl_desc "
  * B131649,1 AYM 03/29/2006 Fix Bug of Scale.cscl_desc [End]
  lcStyTable="SCALE"
  lcStyCond=""
  =lfSlctFox(lcStyFlds,lcStyTable,lcscal,lcStyCond)
  *-- 6.SCALE file  [END]

  *-- 7.WAREHOUS file  [BEGIN]
  lcStyFlds="WAREHOUS.cWareCode,WAREHOUS.cDesc "
  lcStyTable="WAREHOUS"
  lcStyCond=""
  =lfSlctFox(lcStyFlds,lcStyTable,lcWareHous,lcStyCond)
  *-- 7.WAREHOUS file  [END]

  *-- 8.IcSegVal file  [BEGIN]

  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
  IF !USED('IcSegVal')
    =gfOpenTable('IcSegVal','SEGVAL','SH')
  ENDIF
  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  lcStyFlds="IcSegVal.ciSegNo,IcSegVal.ciSegVal,IcSegVal.ciSgValSd  "
  lcStyTable="ICSEGVAL"
  lcStyCond=""
  =lfSlctFox(lcStyFlds,lcStyTable,lcSegVal,lcStyCond)
  *-- 8.IcSegVal file  [END]
  *--(C)
  IF EMPTY(lcStyFab) AND EMPTY(lcTypeVal)
    IF lcRpSortBy =='MT'
      lcFabric=loogscroll.gftempname()
      lcSelFld1= "SELECT ITEM.STYLE,ITEM.ITEM_TYPE FROM ITEM "
      *--Add the Filter of Inventory Type (Fabric OR Style).
      lcSelFld1= lcSelFld1 + "  WHERE ITEM.CINVTYPE= '" +lcInvType+"'"
      *--Add the Item Type Filter.
      lnResult1 = loOGScroll.oRDA.SqlRun(lcSelFld1,lcFabric,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
      IF lnResult1 >=1
        SELECT (lcFabric)
        lnBuffering = CURSORGETPROP("Buffering",lcFabric)
        =CURSORSETPROP("Buffering",3,lcFabric)
        INDEX ON STYLE TAG lcFabric
      ENDIF
    ENDIF
  ENDIF
  *--(C)
  *------------ Retrieve all Fox files used in program remotely [End]

ENDIF  &&end of filter changes
lcRpExp=""


*******************************START COLLLECTING DATA***************************************************************
lcTime     =  gfGetTime()
lnFrstRec  = 0
lnFrstRec1 = 0
llLinkGlJl = ALLTRIM(gfGetMemVar('M_LINK_GL')) = 'Y'
*B608189,1 HIA [Begin]
lcRepMode = UPPER(lcRepMode)
*B608189,1 HIA [END]
*-- llTextMode :  Hold .T. or .T. upon print in text format (Dos Mode) or graphic format.
llTextMode = (UPPER(ALLTRIM(lcRepMode))== LANG_ICSTYREP_TextMode)  && Print Text Format
DIMENSION laScals[8]
STORE "" TO laScals
DIMENSION laScalWs[8]
STORE "" TO laScalWs
*-- if sort by Style or By Location and user select one transaction to print.

IF lcRpSortBy $ 'SW' AND ALEN(laRPRepTar,1) = 1
  =lfSWOneTrn()  && Call Style,Location One transaction Function.
  RETURN
ENDIF

*-- lcRepNmTtl hold the header of the non major segment for the frx
*-- lcSortTtl  hold the sort type
*-- lcGroupExp hold the expression of the report first group
*-- lcSortExp  hold the expression of the report second group
*-- lcMajExp   hold the expression of the major seg. expression
*-- lcNMajExp  hold the expression of the nonmajor seg. expression
*-- lcDescExp  hold the expression of the description expression
*-- lcSortFld  hold the field name which the sort will be upon
*-- lcSourFlds Hold selected transactions fields in Master files to be printed
*-- lcTargFlds Hold selected transactions fields in Temp files to be printed
*-- lcAllTrns  String that hold all transactions
*-- lcUALOFlds hold Unallocated fields name in temp file
*-- lcIOTSFlds hold IOTS fields name in temp file
*-- lcOTSFlds  hold OTS fields name in temp file
*-- lcBokFlds  hold Book fields name in temp file

lcUALOFlds = "UAlo1,UAlo2,UAlo3,UAlo4,UAlo5,UAlo6,UAlo7,UAlo8,TotUAlo"
lcIOTSFlds = "IOTS1,IOTS2,IOTS3,IOTS4,IOTS5,IOTS6,IOTS7,IOTS8,TotIOTS"
lcOTSFlds  = "OTS1,OTS2,OTS3,OTS4,OTS5,OTS6,OTS7,OTS8,TotOTS"
lcBokFlds  = "Bok1,Bok2,Bok3,Bok4,Bok5,Bok6,Bok7,Bok8,TotBok"

lcRepNmTtl =  gfItemMask("HN")

STORE SPACE(0) TO lcSortTtl,lcGroupExp,lcSortExp,lcMajExp,lcNMajExp,lcDescExp,lcSortFld,;
  lcSourFlds,lcTargFlds,lcAllTrns

lcStkFlds = "nStkVWIP,nStkVSOH,nStkVPLA,nStkVOTS,nStkVIOTS,nStkVBOK,;
             nStkVSHP,nStkVRet,nStkVRetA,nStkVAlo,nStkVUAlo,nStkVInt,;
             nStkVWOrd,nStkVOrd"


lcSalFlds = "nSalVWIP,nSalVSOH,nSalVPLA,nSalVOTS,nSalVIOTS,nSalVBOK,;
             nSalVSHP,nSalVRet,nSalVRetA,nSalVAlo,nSalVUAlo,nSalVInt,;
             nSalVWOrd,nSalVOrd"
DIMENSION laAllVal[1],laUAloVal[9],laIOTSVal[11],laOTSVal[11],laBokVal[9],;
  laStkVal[14],laSalVal[14]
STORE 0 TO laAllVal,laUAloVal,laIOTSVal,laOTSVal,laBokVal,laStkVal,laSalVal



STORE 0 TO lnMajSV,lnSorSV,lnRepSV
*-- This is to create and declare the variables that hold the totals
*-- in the Group footer

STORE .T. TO llOnlyOTS
lcScale1 = SPACE(3)

IF llRPPrnLoc AND !llMultiWH
  llRPWhDeta = .T.
ENDIF




*B608189,3 TMI [START] be sure that the variable lcDummy is alwayes defined with non empty value
lcDummy = loOgScroll.gfTempName()
*B608189,3 TMI [END  ]
= lfCrTmp()
= lfDummFill()
WAIT WINDOW LANG_ICSTYREP_CollData NOWAIT
IF lcRpSortBy = 'W'
  = lfDatCollW()
ELSE
  = lfDatCollS()
ENDIF
WAIT CLEAR

lnOldRec = 0
lcOldFld = SPACE(1)



GO TOP
IF EOF()
  =gfModalGen("INM00052B00000",LANG_ICSTYREP_Dialog)
  RETURN
ELSE

   *: C201692,1 MMT 07/07/2015 Add Option to export -ve OTS to CSV file[T20150317.0018][Start]
   IF ",OTS," $ UPPER(lcAllTrns) AND lcRPOTSSig ='N' AND !llRPShwZer AND llRPCrtPO
     lcSelF = ALIAS()
     IF !USED('Style_V')
       =gfOpenTable('Style','Style','SH','Style_V')
     ENDIF
     CREATE CURSOR 'TmpStyCode' (Style C(19))
     SELECT 'TmpStyCode'
     INDEX ON Style TAG 'TmpStyCode'
     *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
     *=STRTOFILE(CHR(13),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",0)
     IF !DIRECTORY(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))
       MD (ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))
     ENDIF
     =STRTOFILE(CHR(13),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",0)
     *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
     
     SELECT (lcSelF)
     SCAN FOR NotScale ='Y' AND (OTS1 < 0 OR OTS2 < 0 OR OTS3 < 0 OR OTS4 < 0 OR OTS5 < 0 OR OTS6 < 0 OR OTS7 < 0 OR OTS8 < 0)
       IF SEEK(&lcSelF..StyCode,'TmpStyCode','TmpStyCode')
         LOOP 
       ELSE
         INSERT INTO 'TmpStyCode' VALUES (&lcSelF..StyCode)  
       ENDIF
       =gfSeek(StyCode,'Style_V','Style')
       lcString = ""+StyCode+","+ IIF(EMPTY(ALLTRIM(Style_V.vendor)),'TBA',Style_V.vendor)+","+ALLTRIM(STR(OTS1))+","+ALLTRIM(STR(OTS2))+","+;
                  ALLTRIM(STR(OTS3))+","+ALLTRIM(STR(OTS4))+","+ALLTRIM(STR(OTS5))+","+ALLTRIM(STR(OTS6))+","+;
                  ALLTRIM(STR(OTS7))+","+ALLTRIM(STR(OTS8))+","+ALLTRIM(STR(TOTOTS))
       *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
       *STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",1)
       STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",1)
       *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
     ENDSCAN
     =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'You have created a PO to create file - if you want to process this file - please use the new Create PO screen. If you want to recreate the file with different selections then please reload the option grid and start again.')     
   ENDIF
   *: C201692,1 MMT 07/07/2015 Add Option to export -ve OTS to CSV file[T20150317.0018][End]


  IF lcRpSortBy == 'SE' OR lcRpSortBy == 'D'
    FOR LnLop = 1 TO 8
      lcSiz = 'SZ' + ALLTRIM(STR(LnLop))
      laScals[LnLop] = &lcscal..&lcSiz
    ENDFOR
  ENDIF

  IF lcRpSortBy == 'SE' OR lcRpSortBy == 'D'
    GO BOTTOM
    STORE SPACE(6) TO lcSeason , lcDivision
    lcSeason   = Season
    lcDivision = Division
    LOCATE FOR CENDREP = "A"
    REPLACE Season   WITH lcSeason ,;
      Division WITH lcDivision
    LOCATE
  ENDIF
  DO gfDispRe WITH EVAL('lcRPFormNa')
ENDIF

*-- End of main program code.

*!*************************************************************
*!*************************************************************
*!***************  Functions Section **************************
*!*************************************************************
*!*************************************************************




*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/10/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
LOCAL lnResult1

*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))

*--Make Temp. File From Item Location File
IF !llFirstTim
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC "
  lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"
  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  llFirstTim = .T.
ENDIF

IF lnResult1 >=1
  lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
  =CURSORSETPROP("Buffering",3,lcFabDye)
  SELECT (lcFabDye)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
ENDIF

*-- Check the cost access
IF llCostAccs
  llShowCost = lcRPShow $ 'CB'
ELSE
  llShowCost = .F.
ENDIF

IF llShowCost
  lcCostMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET')))
ENDIF
llShowSale = lcRPShow $ 'SB'

*-- to prepare the array that hold transactions
= lfTransArr()

*-- to adjust the status of "OTS based on"
= lfOTSbStat()

*-- to adjust the status of "Print dyelots detail"
= lfDyeDtStat()

*-- to adjust the status of "Print location detail"
= lfWhDtStat()

*-- to adjust the status of "Location in list"
= lfWhsOptSt()

*--to check the status of "Print sizes" option.
= lfPrtSizSt()

*--to check the status of "Print size Scale" option.
= lfPrtSzScl()


*E302811,1 TMI 01/04/2011 [Start]
= lfLongDscSup()
*E302811,1 TMI 01/04/2011 [End  ]

*-- end of lfwRepWhen.


*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/10/2005
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
      lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
      lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

*--Retrive the value of some Setups
DIMENSION laSetUp[5,2]
laSetUp[1,1] = 'M_WAREHOUS' &&
laSetUp[2,1] = 'M_WARELOC'  &&
laSetUp[3,1] = 'M_DYELOT'   &&
laSetUp[4,1] = 'M_COST_MET' &&
laSetUp[5,1] = 'M_STYCNFG' &&

=gfGetMemVar(@laSetUp)

llMultiWH = ALLTRIM(laSetUp[1,2]) = 'Y'
llTrakLoc = ALLTRIM(laSetUp[2,2]) = 'Y'
llDyelot  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcCstMeth = ALLTRIM(laSetUp[4,2])
llConfig  = ALLTRIM(UPPER(laSetUp[5,1])) = 'Y'

llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')

RETURN ''

*!*************************************************************
*! Name      : lfvPrint
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Validate print Cost and price option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrint()
*!*************************************************************

FUNCTION lfvPrint

IF llCostAccs
  llShowCost = lcRPShow $ 'CB'
ELSE
  llShowCost = .F.
ENDIF

IF llShowCost
  lcCostMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET')))
ENDIF

llShowSale = lcRPShow $ 'SB'

IF (lcLastSel $ 'SCB' AND lcRPShow = 'N') OR (lcLastSel = 'N' AND lcRPShow $ 'SCB')
  DIMENSION laRPRepTar[1]
  laRPRepTar = SPACE(0)
ENDIF

lcLastSel = lcRPShow

= lfOTSbStat()
= lfDyeDtStat()


*!*************************************************************
*! Name      : lfvPrnReps
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Validate transactions to be printed option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!**************************************************************
*! Modification:
*!**************************************************************
*! Example     : = lfvPrnReps()
*!*************************************************************

FUNCTION lfvPrnReps

= lfTransArr()

*-- This is to clear target array in case the user change what he want to
*-- print
= lfOGMover(@laRPRepSou,@laRPRepTar,LANG_ICSTYREP_PrintedTranscations ,.T.,"lfvPrtSize",.F.,.T.)
= lfPrtSizSt()
= lfPrtSzScl()
=lfChngForm()  && Change report form.
=lfvSort()     && this is to check plan report
= lfOTSbStat()
= lfDyeDtStat()
= lfFillDumy()
*-- end of lfvPrnReps.

*!*************************************************************
*! Name      : RefreshTrans
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Refresh transactions area.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!**************************************************************
*! Modification:
*!**************************************************************
*! Example     : = RefreshTrans()
*!*************************************************************
FUNCTION RefreshTrans
LOCAL lcTransactionsStr, lnTarget
lcTransactionsStr = ""
IF !EMPTY(laRPRepTar)
  FOR lnTarget = 1 TO ALEN(laRPRepTar,1)
    lcTransactionsStr = lcTransactionsStr + ", " + laRPRepTar[lnTarget]
  ENDFOR
  lcTransactionsStr = SUBSTR(lcTransactionsStr,3)
ENDIF
RETURN lcTransactionsStr
ENDFUNC
*-- end of RefreshTrans.

*!*************************************************************
*! Name      : lfTransArr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : To fill the array that hold the availabe transactions
*!             to be printed
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfTransArr()
*!*************************************************************

FUNCTION lfTransArr

IF TYPE('lcTrns')='U'
  lcTrns   = loOgScroll.gfTempName()
  lcStyTmp = loOgScroll.gfTempName()
  lcTotTmp = loOgScroll.gfTempName()
  lcDummy  = loOgScroll.gfTempName()
ENDIF

IF TYPE('laRPRepSou[1,1]') = 'U' OR EMPTY(laRPRepSou[1,1])
  DIMENSION laRPRepSou[1]
  STORE SPACE(0) TO laRPRepSou
ENDIF

IF TYPE('laRPRepTar[1,1]') = 'U' OR EMPTY(laRPRepTar[1,1])
  DIMENSION laRPRepTar[1]
  STORE SPACE(0) TO laRPRepTar
ENDIF
*E124814,1 HMA 07/04/2005,remove plan Trans. from the selected trans. while Print Location Bins =Yes [Begin]
* If Configuration=Yes & Print Bins=Yes   ,Remove PLAN & Remove DYELOT
* If Configuration=Yes & Print Bins=No    ,Remove PLAN & Remove DYELOT
* If Configuration=NO & Print Bins=Yes    ,Remove PLAN & Don't Remove DYELOT
* If Configuration=No & Print Bins=No     ,Don't Remove PLAN &Don't Remove DYELOT
IF !llRpConfig AND !llRPPrnLoc
  DIMENSION laAllTrns[15,2]
  laAllTrns[01,1] = LANG_ICSTYREP_WIP
  laAllTrns[02,1] = LANG_ICSTYREP_Stock
  laAllTrns[03,1] = LANG_ICSTYREP_Dyelot
  laAllTrns[04,1] = LANG_ICSTYREP_Plan
  laAllTrns[05,1] = LANG_ICSTYREP_Unallocated
  laAllTrns[06,1] = LANG_ICSTYREP_OTS
  laAllTrns[07,1] = LANG_ICSTYREP_Imm_OTS
  laAllTrns[08,1] = LANG_ICSTYREP_Orders
  laAllTrns[09,1] = LANG_ICSTYREP_Work_orders
  laAllTrns[10,1] = LANG_ICSTYREP_Intransit
  laAllTrns[11,1] = LANG_ICSTYREP_Book
  laAllTrns[12,1] = LANG_ICSTYREP_Shipped
  laAllTrns[13,1] = LANG_ICSTYREP_Allocated
  laAllTrns[14,1] = LANG_ICSTYREP_Return
  laAllTrns[15,1] = LANG_ICSTYREP_Return_auth

  laAllTrns[01,2] = "WIP"
  laAllTrns[02,2] = "SOH"
  laAllTrns[03,2] = "SOH"
  laAllTrns[04,2] = "PLA"
  laAllTrns[05,2] = "UALO"
  laAllTrns[06,2] = "OTS"
  laAllTrns[07,2] = "IOTS"
  laAllTrns[08,2] = "ORD"
  laAllTrns[09,2] = "WORD"
  laAllTrns[10,2] = "INT"
  laAllTrns[11,2] = "BOK"
  laAllTrns[12,2] = "SHP"
  laAllTrns[13,2] = "ALO"
  laAllTrns[14,2] = "RET"
  laAllTrns[15,2] = "RETA"

ELSE
  IF (llRpConfig AND llRPPrnLoc) OR (llRpConfig AND !llRPPrnLoc)
    DIMENSION laAllTrns[13,2]
    laAllTrns[01,1] = LANG_ICSTYREP_WIP
    laAllTrns[02,1] = LANG_ICSTYREP_Stock
    laAllTrns[03,1] = LANG_ICSTYREP_Unallocated
    laAllTrns[04,1] = LANG_ICSTYREP_OTS
    laAllTrns[05,1] = LANG_ICSTYREP_Imm_OTS
    laAllTrns[06,1] = LANG_ICSTYREP_Orders
    laAllTrns[07,1] = LANG_ICSTYREP_Work_orders
    laAllTrns[08,1] = LANG_ICSTYREP_Intransit
    laAllTrns[09,1] = LANG_ICSTYREP_Book
    laAllTrns[10,1] = LANG_ICSTYREP_Shipped
    laAllTrns[11,1] = LANG_ICSTYREP_Allocated
    laAllTrns[12,1] = LANG_ICSTYREP_Return
    laAllTrns[13,1] = LANG_ICSTYREP_Return_auth

    laAllTrns[01,2] = "WIP"
    laAllTrns[02,2] = "SOH"
    laAllTrns[03,2] = "UALO"
    laAllTrns[04,2] = "OTS"
    laAllTrns[05,2] = "IOTS"
    laAllTrns[06,2] = "ORD"
    laAllTrns[07,2] = "WORD"
    laAllTrns[08,2] = "INT"
    laAllTrns[09,2] = "BOK"
    laAllTrns[10,2] = "SHP"
    laAllTrns[11,2] = "ALO"
    laAllTrns[12,2] = "RET"
    laAllTrns[13,2] = "RETA"
  ELSE
    IF !llRpConfig AND llRPPrnLoc
      DIMENSION laAllTrns[14,2]
      laAllTrns[01,1] = LANG_ICSTYREP_WIP
      laAllTrns[02,1] = LANG_ICSTYREP_Stock
      laAllTrns[03,1] = LANG_ICSTYREP_Dyelot
      laAllTrns[04,1] = LANG_ICSTYREP_Unallocated
      laAllTrns[05,1] = LANG_ICSTYREP_OTS
      laAllTrns[06,1] = LANG_ICSTYREP_Imm_OTS
      laAllTrns[07,1] = LANG_ICSTYREP_Orders
      laAllTrns[08,1] = LANG_ICSTYREP_Work_orders
      laAllTrns[09,1] = LANG_ICSTYREP_Intransit
      laAllTrns[10,1] = LANG_ICSTYREP_Book
      laAllTrns[11,1] = LANG_ICSTYREP_Shipped
      laAllTrns[12,1] = LANG_ICSTYREP_Allocated
      laAllTrns[13,1] = LANG_ICSTYREP_Return
      laAllTrns[14,1] = LANG_ICSTYREP_Return_auth

      laAllTrns[01,2] = "WIP"
      laAllTrns[02,2] = "SOH"
      laAllTrns[03,2] = "SOH"
      laAllTrns[04,2] = "UALO"
      laAllTrns[05,2] = "OTS"
      laAllTrns[06,2] = "IOTS"
      laAllTrns[07,2] = "ORD"
      laAllTrns[08,2] = "WORD"
      laAllTrns[09,2] = "INT"
      laAllTrns[10,2] = "BOK"
      laAllTrns[11,2] = "SHP"
      laAllTrns[12,2] = "ALO"
      laAllTrns[13,2] = "RET"
      laAllTrns[14,2] = "RETA"
    ENDIF
  ENDIF
ENDIF
*--Fill the transaction array according to the Print Costing Option.[BEGIN]
*--S For sales value
*--C For cost value
*--B For both
*--N For none

IF UPPER(lcRPShow) $ 'SCB'
  *!*    IF !llRpConfig
  *!*      DIMENSION laRPRepSou[6]
  *!*      laRPRepSou[01] = LANG_ICSTYREP_WIP
  *!*      laRPRepSou[02] = LANG_ICSTYREP_Stock
  *!*      laRPRepSou[03] = LANG_ICSTYREP_Plan
  *!*      laRPRepSou[04] = LANG_ICSTYREP_Unallocated
  *!*      laRPRepSou[05] = LANG_ICSTYREP_OTS
  *!*      laRPRepSou[06] = LANG_ICSTYREP_Imm_OTS
  *!*    ELSE
  *!*      DIMENSION laRPRepSou[5]
  *!*      laRPRepSou[01] = LANG_ICSTYREP_WIP
  *!*      laRPRepSou[02] = LANG_ICSTYREP_Stock
  *!*      laRPRepSou[03] = LANG_ICSTYREP_Unallocated
  *!*      laRPRepSou[04] = LANG_ICSTYREP_OTS
  *!*      laRPRepSou[05] = LANG_ICSTYREP_Imm_OTS
  *!*    ENDIF
  IF llRpConfig OR llRPPrnLoc
    DIMENSION laRPRepSou[5]
    laRPRepSou[01] = LANG_ICSTYREP_WIP
    laRPRepSou[02] = LANG_ICSTYREP_Stock
    laRPRepSou[03] = LANG_ICSTYREP_Unallocated
    laRPRepSou[04] = LANG_ICSTYREP_OTS
    laRPRepSou[05] = LANG_ICSTYREP_Imm_OTS
  ELSE
    DIMENSION laRPRepSou[6]
    laRPRepSou[01] = LANG_ICSTYREP_WIP
    laRPRepSou[02] = LANG_ICSTYREP_Stock
    laRPRepSou[03] = LANG_ICSTYREP_Plan
    laRPRepSou[04] = LANG_ICSTYREP_Unallocated
    laRPRepSou[05] = LANG_ICSTYREP_OTS
    laRPRepSou[06] = LANG_ICSTYREP_Imm_OTS
  ENDIF
ELSE
  DIMENSION laRPRepSou[14]
  laRPRepSou[01] = LANG_ICSTYREP_WIP
  laRPRepSou[02] = LANG_ICSTYREP_Stock
  laRPRepSou[03] = LANG_ICSTYREP_Plan
  laRPRepSou[04] = LANG_ICSTYREP_Unallocated
  laRPRepSou[05] = LANG_ICSTYREP_OTS
  laRPRepSou[06] = LANG_ICSTYREP_Imm_OTS
  laRPRepSou[07] = LANG_ICSTYREP_Orders
  laRPRepSou[08] = LANG_ICSTYREP_Work_orders
  laRPRepSou[09] = LANG_ICSTYREP_Intransit
  laRPRepSou[10] = LANG_ICSTYREP_Book
  laRPRepSou[11] = LANG_ICSTYREP_Shipped
  laRPRepSou[12] = LANG_ICSTYREP_Allocated
  laRPRepSou[13] = LANG_ICSTYREP_Return
  laRPRepSou[14] = LANG_ICSTYREP_Return_auth
ENDIF
*--Fill the transaction array according to the Print Costing Option.[END]

*--Fill the transaction array according to the Print Configuration option.[BEGIN]
*IF llRpConfig AND ! UPPER(lcRPShow) $ 'SCB'
IF (llRpConfig OR llRPPrnLoc) AND ! UPPER(lcRPShow) $ 'SCB'
  DIMENSION laRPRepSou[13]
  laRPRepSou[01] = LANG_ICSTYREP_WIP
  laRPRepSou[02] = LANG_ICSTYREP_Stock
  laRPRepSou[03] = LANG_ICSTYREP_Unallocated
  laRPRepSou[04] = LANG_ICSTYREP_OTS
  laRPRepSou[05] = LANG_ICSTYREP_Imm_OTS
  laRPRepSou[06] = LANG_ICSTYREP_Orders
  laRPRepSou[07] = LANG_ICSTYREP_Work_orders
  laRPRepSou[08] = LANG_ICSTYREP_Intransit
  laRPRepSou[09] = LANG_ICSTYREP_Book
  laRPRepSou[10] = LANG_ICSTYREP_Shipped
  laRPRepSou[11] = LANG_ICSTYREP_Allocated
  laRPRepSou[12] = LANG_ICSTYREP_Return
  laRPRepSou[13] = LANG_ICSTYREP_Return_auth
ENDIF
*E124814,1 HMA 07/04/2005,remove plan Trans. from the selected trans. while Print Location Bins =Yes [End]
*--Fill the transaction array according to the Print Configuration option.[BEGIN]

*!*************************************************************
*! Name      : lfvConfig
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/10/2005
*! Purpose   : validation of configuration option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvConfig()
*!*************************************************************
FUNCTION lfvConfig

IF llRpConfig
  = lfTransArr() &&fill again the transaction menu after removing PLAN transaction.
  llPlanFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_Plan) > 0
  lnArrLen = ALEN(laRPRepTar)
  IF llPlanFoun  && delete PLAN from the selected transaction menu(target array).
    = ADEL(laRPRepTar,ASCAN(laRPRepTar,LANG_ICSTYREP_Plan))
    IF ALEN(laRPRepTar,1) > 1
      DIMENSION laRPRepTar[lnArrLen - 1]
    ELSE
      STORE SPACE(0) TO laRPRepTar
    ENDIF
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvSort
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Validate sort option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSort()
*!*************************************************************

FUNCTION lfvSort

llPlanFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_Plan) > 0

IF (lcRpSortBy = "W" OR llRPWhDeta) AND llPlanFoun
  *-- "Plan" cannot be printed while sorting by Location or printing locations detail
  *-- <OK>
  = gfModalGen("INM42147B00000","Dialog")
  lnArrLen = ALEN(laRPRepTar)
  = ADEL(laRPRepTar,ASCAN(laRPRepTar,LANG_ICSTYREP_Plan))
  IF ALEN(laRPRepTar,1) > 1
    DIMENSION laRPRepTar[lnArrLen - 1]
  ELSE
    STORE SPACE(0) TO laRPRepTar
  ENDIF
ENDIF

=lfChngForm()  && Change report form.

= lfOTSbStat()
= lfWhDtStat()
= lfWhsOptSt()
*! B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
= lfFillDumy()
*! B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
*!*************************************************************
*! Name      : lfClrRead
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Refresh the option grid
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   : =lfClrRead()
*!*************************************************************
FUNCTION lfClrRead
ClearRead()

*!*************************************************************
*! Name      : lfvPrnWhDet
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Validate print location detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Nonen
*!*************************************************************
*! Example     : = lfvPrnWhDet()
*!*************************************************************
FUNCTION lfvPrnWhDet
= lfvSort()
= lfWhsOptSt()

*!*************************************************************
*! Name      : lfvPrnBins
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 04/07/2005
*! Purpose   : Validate print Bins Option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Nonen
*!*************************************************************
*! Example     : = lfvPrnBins()
*!*************************************************************
*E124814,1 HMA 07/04/2005,remove plan Trans. from the selected
*               trans. while Print Location Bins =Yes
*!*************************************************************
FUNCTION lfvPrnBins

llPlanFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_Plan) > 0

IF llRPPrnLoc
  *-- "Plan" cannot be printed while Print Location Bins=Yes
  lnArrLen = ALEN(laRPRepTar)
  = ADEL(laRPRepTar,ASCAN(laRPRepTar,LANG_ICSTYREP_Plan))
  IF ALEN(laRPRepTar,1) > 1
    DIMENSION laRPRepTar[lnArrLen - 1]
  ELSE
    STORE SPACE(0) TO laRPRepTar
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvOts
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : to validate "OTS based on" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOts()
*!*************************************************************
FUNCTION lfvOts

DO CASE
CASE lcRPOTSSig = 'P'
  lnRPOTSMin = 1
CASE lcRPOTSSig = 'N'
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
  *lnRPOTSMin = -1
  lnRPOTSMax = -1
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
ENDCASE

*!*************************************************************
*! Name      : lfPrtSzScl
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : adjust the status of "Print Sizes Scal"
*!*************************************************************
*! Called from : lfwRepWhen & lfvPrnReps
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrtSzScl()
*!*************************************************************
*!
FUNCTION lfPrtSzScl
loOGScroll.EnableObject('LLRPSCALE',LLRPPRTSIZ)
*-- end of lfPrtSzScl

*!*************************************************************
*! Name      : lfsrSty
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm

DO CASE
CASE lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major
  *-- unique index.
  USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE   IN  0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  LOCATE
CASE lcParm = 'R'  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfOTSbStat
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : to adjust the status of "OTS based on" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOTSbStat()
*!*************************************************************

FUNCTION lfOTSbStat
llOTSFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_OTS) > 0 OR ASCAN(laRPRepTar,LANG_ICSTYREP_Imm_OTS) > 0

IF llOTSFoun  &&if found OTS one of the selected trans.
  IF !(lcRpSortBy = "W" ) AND  !llRPWhDeta
    llStatOB = .T.
    llStatOS = .T.
    llStatOM = .T.
  ELSE
    *-- if OTS is selected but the sort by location or print location detail
    *-- then make the OTS based on WIP "W" and disable this option "OTS based on"
    *-- because this will be from StyDye file which has not plan fields
    lcRPOTSB = 'W'
    llStatOB = .F.
    llStatOS = .T.
    llStatOM = .T.
  ENDIF
  IF llRpConfig &&if print configuration=yes,disable this object with default value='WIP'
    lcRPOTSB = 'W'
    llStatOB = .F.
  ENDIF
ELSE
  llStatOB = .F.
  llStatOS = .F.
  llStatOM = .F.
ENDIF

loOGScroll.EnableObject('LCRPOTSB', LLSTATOB)
loOGScroll.EnableObject("LCRPOTSSIG",LLSTATOS)
loOGScroll.EnableObject("LNRPOTSMIN",LLSTATOM)
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
loOGScroll.EnableObject("LNRPOTSMAX",LLSTATOM)
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[END]
*-- end of lfOTSbStat.

*!*************************************************************
*! Name      : lfDyeDtStat
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : to adjust the status of "Print Dyelots for stock" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDyeDtStat()
*!*************************************************************

FUNCTION lfDyeDtStat

*-- just to adjust the status of "Print Dyelots for stock" in lfwRepWhen function

*-- if the system support dyelots
IF llDyelot
  llStat = ASCAN(laRPRepTar,LANG_ICSTYREP_Stock) > 0
ELSE
  llStat = .F.
ENDIF

IF !llStat
  llRPPrnDye = .F.
ENDIF

loOGScroll.EnableObject("LLRPPRNDYE", LLSTAT)
*-- end of lfDyeDtStat.

*!*************************************************************
*! Name      : lfWhDtStat
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : to adjust the status of "Print location detail" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfWhDtStat()
*!*************************************************************

FUNCTION lfWhDtStat

*-- just to adjust the status of "Print location detail" in lfwRepWhen function
*-- Show if the system multible or single location

*-- if it is not multi location OR it is multi location but the sort by
*-- location, we neednot location detail
llStat = llMultiWH AND lcRpSortBy <> 'W'

loOGScroll.EnableObject("LLRPWHDETA", LLSTAT)

loOGScroll.EnableObject("LLRPPRNLOC", LLTRAKLOC AND ((LLMULTIWH AND LLRPWHDETA) OR !LLMULTIWH OR lcRpSortBy = 'W'))


IF !llRPWhDeta AND llMultiWH
  LLRPPRNLOC=.F. &&set default value=NO if "print location detail" option is disabled
ENDIF

*-- end of lfWhDtStat.

*!*************************************************************
*! Name      : lfWhsOptSt
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : To adjust enabling status of "Location in list"
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Nonen
*!*************************************************************
*! Example     : = lfWhsOptSt()
*!*************************************************************

FUNCTION lfWhsOptSt
loOGScroll.EnableObject("STYDYE.CWARECODE",(lcRpSortBy = "W" ) OR llRPWhDeta,"laOGFxFlt")
*-- end of lfWhsOptSt.

*!*************************************************************
*! Name      : lfPrtSizSt
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : adjust the status of "Print Sizes"
*!*************************************************************
*! Called from : lfwRepWhen & lfvPrnReps
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrtSizSt()
*!*************************************************************

FUNCTION lfPrtSizSt
loOGScroll.EnableObject('LLRPPRTSIZ', ALEN(LARPREPTAR,1) = 1)
*-- end of lfPrtSizSt

*!*************************************************************
*! Name      : lfChngForm
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Change printed form
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfChngForm()
*!*************************************************************
FUNCTION lfChngForm
*-- if sort by location.
IF lcRpSortBy = "W"
  *-- if have one transaction only.
  IF ALEN(laRPRepTar,1) = 1 AND !EMPTY(laRPRepTar)
    lcRPFormNa = "ICSTYDCL"
  ELSE
    lcRPFormNa = "ICSTYDCW"
  ENDIF
  llRPWhDeta = .F.

ELSE  && Sort by anything rather than location.

  *-- if have one transaction only.
  IF (lcRpSortBy == "S" ) AND ALEN(laRPRepTar,1) = 1 AND !EMPTY(laRPRepTar)
    lcRPFormNa = "ICSTYDCS"
  ELSE
    lcRPFormNa = "ICSTYDCO"
  ENDIF

ENDIF

lcNo1 = lcRepMode
lcNo2 = lcOGPlatForm
=lfRepPltFr(lcRPFormNa)

IF lcRepMode # lcNo1 OR lcOGPlatForm # lcNo2
  lcRepMode    = lcNo1
  lcOGPlatForm = lcNo2
ENDIF
*-- end of lfChngForm.

*!*************************************************************
*! Name      : lfVarPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 04/06/2004
*! Purpose   : OG when function
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(loOGScroll.laOGObjType,UPPER(lcItmInFlt))
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfVarPos.

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfPvRun
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : change color code in filter array
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfPvRun()
*!*************************************************************
FUNCTION lfPvRun
*--lcsty1 var to get exp
*--lcsty2 var to get colors and concatenate it to lcsty1
PRIVATE lcSty1,lcSty2
STORE 0 TO lcSty1,lcSty2

*-- get color length
DECLARE laItemSeg[1]
STORE 0 TO lncolorLen
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lncolorLen = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

*-- get color from array and change it
*-- get color position
lnClrSgPo = ASCAN(loOGScroll.laOGFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)',1,ALEN(loOGScroll.laOGFxFlt,1),1,10)
IF lnClrSgPo = 0
  lcsty1 = ""
  RETURN .T.
ENDIF

*-- Get first color
lcsty1 = SUBSTR(loOGScroll.laOGFxFlt[lnClrSgPo,6],1,lnColorLen)

*-- loop for No. of Occurance of Separator "|" in Color exp. and add 1 to last color
FOR lnCounter = 1 TO OCCUR("|",loOGScroll.laOGFxFlt[lnClrSgPo,6])+1

  *--get from second color to rest color
  IF lnCounter > 1
    *-- get  position of "|"
    lnFirstPos  = ATC('|',loOGScroll.laOGFxFlt[lnClrSgPo,6],lnCounter-1)
    *-- we add one to positon to substr after "|"
    lcSty2      = SUBSTR(loOGScroll.laOGFxFlt[lnClrSgPo,6],lnFirstPos+1,lnColorLen)
  ENDIF

  IF !EMPTY(lcSty2)
    *--Concatenate expression
    lcSty1 = lcsty1 + '|' + lcSty2
  ELSE

    *--for chose first color only
    lcSty1 = lcsty1
  ENDIF
ENDFOR

loOGScroll.laOGFxFlt[lnClrSgPo,6] = lcSty1

*-- end of lfPvRun

*!*************************************************************
*! Name      : lfExtScale
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : Check if the system uses extended size scale
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfExtScale()
*!*************************************************************

FUNCTION lfExtScale

*-- This is to check if the system use extended size or not
RETURN gfGetMemVar('M_USEEXSSC')

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : To get major segment Picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*!*************************************************************
*! Name      : lfSrvDep
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : Set and Rest functions for Department filter.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSrvDep()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSrvDep
PARAMETERS lcParm

DO CASE
CASE lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Department.
  *-- unique index.
  SELECT ICDeptHd
  lcDeptTag = ORDER()
  INDEX ON Dept TAG DeptUnique OF (oAriaApplication.WorkDir+lcStyTag+".CDX") UNIQUE
  SET ORDER TO DeptUnique
CASE lcParm = 'R'  && Reset code
  SET INDEX TO
  SET ORDER TO (lcDeptTag)
ENDCASE
*-- end of lfSrvDep.

*!*************************************************************
*! Name      : lfSumFab1
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/21/2004
*! Purpose   : sum a specific field for the current fabric
*!                  in fabric file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab1()
*!*************************************************************
FUNCTION lfSumFab1
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
*! Name      : lfStySum
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : sum a specific field for the current style in style file
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
SET ORDER TO STYLE
IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF

SELECT STYLE
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
*! Name      : lfvPrtSize
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : Validate Sizes to be printed
*!*************************************************************
*! Called from : gfMover()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrtSize()
*!*************************************************************
FUNCTION lfvPrtSize
LPARAMETERS lnButtn

*-- if Remove or the option of printing the size is "Yes"
IF INLIST(lnButtn,3,4) OR llRpPrtSiz
  RETURN .T.
ENDIF
PRIVATE lnMesButtn,llReturn

llReturn = .T.


*-- If more than one transaction are selected have a massege to tell that the
*-- option of the sizes can't be "No" then return it "Yes" and disable it
IF (lnButtn = 2) OR ((ALEN(laTarget,1) = 1) AND !EMPTY(laTarget[1]))

  lnMesButtn = gfModalGen('QRM42215B00012',.F.,"",.F.,"")

  llReturn = lnMesButtn = 1
  llRpPrtSiz = llReturn

ENDIF && end If more than one transaction are selected

RETURN (llReturn)
*-- end of lfvPrtSize

*!*************************************************************
*! Name      : lfSWOneTrn
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/31/2005
*! Purpose   : Proccess function when User select one transaction
*!             and sort by Style or Location.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSWOneTrn()
*!*************************************************************
FUNCTION lfSWOneTrn

PRIVATE lcStyorder,lcStyDyeorder
STORE '' TO lcStyorder,lcStyDyeorder
*-- lcMastFile : Variable hold master file name (Style OR StyDye)
*-- lcChldFile : Variable hold Child  file name (Style OR StyDye)
*-- lcExtndFlt : Variable hold Filter when alias is stydye.
*-- lcStyGroup : Style Major Group.
*-- lcLocGroup : Location Group.
*-- lcDyeGroup : Dyelot   Group.
*-- lnMajorLen : Style Major Length.
*-- lcCost_Val : Cost (And/Or) Value Header
*-- laTranAray : Array Hold Reference to printed transaction.
*-- laTrnNonAr : Array Hold Reference to printed transaction From style master.
*-- lnEndOfSty : End of Style group.
*-- lnEndOfLoc : End of Location group.
*-- lnEndOfDye : End of Dyelot group.
*-- lcRevFile  : Reverse File Name
*-- lcRepTarVl : Printed transaction Code.
*-- lcPrnTitle : Title of printed transaction.

*-- lcCst_VlPn : Print price and cost value in detail band.
*-- lcCst_VlOp : Print price and cost value in Style Group.
*-- lcCst_VlCl : Print price and cost value Per non major.
*-- lcCst_VlGd : Print price and cost value in Summary Band.

*-- lcPrntSty  : Detect Style Change.
*-- lcPrntNon  : Detect location change.

*-- lnCstAveVl : Save average cost in stydye location record.

*-- lcNonMjDes : Non Major description.
*-- llPrintClr : Can you print non major data from style file while loop stydye.
*-- llPrnClrLn : Print Non Major line.
*-- lcLocBins  : Variable Have Bins for printed location.

*-- Report Numeric and calculated values [Begin]
*** Detail Band ***
*-- lnMaxCnt   : Maxium scale Count.
*-- lnSize1 TO ---> lnSize8  : Variables Hold Size quantities.
*-- lnSize9    : Variable Hold total for all sizes.
*-- lnSize10    : Variable Hold total for Positive quantities sizes in case of transaction is OTS or IOTS.
*-- lnSize11    : Variable Hold total for negative quantities sizes in case of transaction is OTS or IOTS.
*-- lnNonPrice : PriceA * lnSize9
*-- lnNonCost  : Ave_Cost * lnSize9

*** Master is StyDye and Sort by style then print non major from style file ***
*-- lnClrSz1 TO ---> lnClrSz8  : Variables Hold Size quantities.
*-- lnClrSz9    : Variable Hold total for all sizes.
*-- lnClrSz10    : Variable Hold total for Positive quantities sizes in case of transaction is OTS or IOTS.
*-- lnClrSz11    : Variable Hold total for negative quantities sizes in case of transaction is OTS or IOTS.
*-- lnClrPrice : PriceA * lnSize9
*-- lnClrCost  : Ave_Cost * lnSize9

*** Location Group ***
*-- lnLocSz1 TO ---> lnLocSz8  : Variables Hold Size quantities.
*-- lnLocSz9    : Variable Hold total for all sizes.
*-- lnLocSz10    : Variable Hold total for Positive quantities sizes in case of transaction is OTS or IOTS.
*-- lnLocSz11    : Variable Hold total for negative quantities sizes in case of transaction is OTS or IOTS.
*-- lnNonPrcLc  : Total Price.
*-- lnNonCstLc  : Total Cost.

*** Style Group ***
*-- lnStySz1 TO ---> lnStySz8  : Variables Hold Size quantities.
*-- lnStySz9    : Variable Hold total for all sizes.
*-- lnStySz10    : Variable Hold total for Positive quantities sizes in case of transaction is OTS or IOTS.
*-- lnStySz11    : Variable Hold total for negative quantities sizes in case of transaction is OTS or IOTS.
*-- lnNonPrcOp  : Total Price.
*-- lnNonCstOp  : Total Cost.

*** Summary Band ***
*-- lnGrdSz1 TO ---> lnGrdSz8  : Variables Hold Size quantities.
*-- lnGrdSz9    : Variable Hold total for all sizes.
*-- lnGrdSz10    : Variable Hold total for Positive quantities sizes in case of transaction is OTS or IOTS.
*-- lnGrdSz11    : Variable Hold total for negative quantities sizes in case of transaction is OTS or IOTS.
*-- lnNonPrcGd  : Total Price.
*-- lnNonCstGd  : Total Cost.
*-- Report Numeric and calculated values [End  ]

lcScalCode = ' '

DIMENSION laTranAray[8],laTrnNonAr[8]
STORE ''   TO lcMastFile , lcChldFile , lcCost_Val , laTranAray ,;
  lcRepTarVl , lcPrnTitle , lcRevFile , lcCst_VlPn  ,;
  lcCst_VlOp , lcCst_VlGd , lcNonMjDes, lcExtndFlt  ,;
  laTrnNonAr , lcCst_VlCl , lcLocBins , lcPrintSty,lcCst_VlLc
STORE '' TO lcCst_VlC1,lcCst_VlC2,lcCst_VlP1,lcCst_VlP2,lcCst_VlO1,lcCst_VlO2,lcCst_VlG1,lcCst_VlG2


STORE [""] TO lcStyGroup , lcLocGroup , lcDyeGroup

STORE 0 TO lnEndOfSty , lnEndOfLoc , lnEndOfDye ,;
  lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
  lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11,lnMaxCnt,;
  lnNonPrice,lnNonCost

STORE 0 TO lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
  lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,lnStySz10,lnStySz11,;
  lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
  lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
  lnNonPrcOp,lnNonCstOp,lnNonPrcGd,lnNonCstGd

STORE 0 TO lnLocSz1,lnLocSz2,lnLocSz3,lnLocSz4,;
  lnLocSz5,lnLocSz6,lnLocSz7,lnLocSz8,lnLocSz9,lnLocSz10,lnLocSz11,;
  lnNonPrcLc,lnNonCstLc

STORE 0 TO lnClrSz1,lnClrSz2,lnClrSz3,lnClrSz4,;
  lnClrSz5,lnClrSz6,lnClrSz7,lnClrSz8,lnClrSz9,lnClrSz10,lnClrSz11,;
  lnClrPrice,lnClrCost,lnCstAveVl


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
STORE .F. TO llChkAllSz
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

llPrnClrLn = .F.
= lfTranAray()  && Fill Transaction Arrays.
IF EMPTY(laTranAray[1])
  RETURN
ENDIF

lnMajorLen = LEN(gfItemMask("PM"))  && Major Length

lcPrntLoc = SPACE(6)
lcPrntSty = SPACE(lnMajorLen)
lcPrntNon = SPACE(19 - lnMajorLen)
lnSelect=SELECT()
SELECT(lcStyle)
lcStyorder=ORDER()
SET ORDER TO
RevStyle=loOgScroll.gfTempName()
COPY TO oAriaApplication.WorkDir+RevStyle+".dbf"
IF FILE(oAriaApplication.WorkDir +RevStyle+".DBF")
  IF !USED(RevStyle)
    *! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][Start]
    *USE (oAriaApplication.WorkDir +RevStyle+".DBF") IN 0
    USE (oAriaApplication.WorkDir +RevStyle+".DBF") IN 0 EXCLUSIVE
    *! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][End]
  ENDIF
  SELECT(RevStyle)
  INDEX ON STYLE TAG RevStyle
ENDIF
IF !EMPTY(lcStyOrder)
  SET ORDER TO (lcStyOrder) IN &lcStyle
ENDIF
SELECT(lcStyDye)
lcStyDyeorder=ORDER()
SET ORDER TO
RevStyDy=loOgScroll.gfTempName()
COPY TO oAriaApplication.WorkDir+RevStyDy+".dbf"
IF FILE(oAriaApplication.WorkDir +RevStyDy+".DBF")
  IF !USED(RevStyDy)
    *! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][Start]
    *USE (oAriaApplication.WorkDir +RevStyDy+".DBF") IN 0
    USE (oAriaApplication.WorkDir +RevStyDy+".DBF") IN 0 EXCLUSIVE
    *! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][End]
  ENDIF
  SELECT(RevStyDy)
  INDEX ON  STYLE+CWARECODE+DYELOT TAG RevStyDy
  INDEX ON  CWARECODE+STYLE+DYELOT TAG RevStyDyW
ENDIF
SET ORDER TO TAG lcStyle  IN &lcStyle
SET ORDER TO TAG RevStyle IN &RevStyle
IF !EMPTY(lcStyDyeorder)
  SET ORDER TO (lcStyDyeorder) IN &lcStyDye
ENDIF
SELECT(lnSelect)

lcStyGroup = [PADR(&lcStyDye..Style,lnMajorLen)]

*-- if Sort by Style.
IF lcRpSortBy == 'S'
  SET ORDER TO TAG lcStyDye IN &lcStyDye
  SET ORDER TO TAG RevStyDy IN &RevStyDy
  IF llConfig
    llSetup= llRpConfig
  ELSE
    llSetup= llRpPrnDye
  ENDIF
  llPrintClr = llRpWhDeta OR llSetup && Get sizes from StyDye file.
  lcMastFile = '&lcStyDye'
  lcChldFile = '&lcStyle'
  lcRevFile  = '&REVSTYDY'
  llSChkDyeB = .F.
  IF llPrintClr
    llSChkDyeB = .T.
    lcMastFile = '&lcStyDye'
    lcChldFile = '&lcStyle'
    lcRevFile  = '&REVSTYDY'
    *-- if print locations with out print Configuration.
    IF llRpWhDeta AND !llSetup
      lcExtndFlt = [EMPTY(Dyelot)]
    ENDIF
  ELSE       && else Print style records only without any other details.
    lcMastFile = '&lcStyle'
    lcChldFile = '&lcStyDye'
    lcRevFile  = '&REVSTYLE'
  ENDIF      && end if print location detail or print Configuration.
ELSE         && else sort by location.
  llSChkDyeB = .T.
  llPrintClr = .F.
  SET ORDER TO TAG lcStyDyeW IN &lcStyDye
  SET ORDER TO TAG RevStyDyW IN &RevStyDy
  lcMastFile = '&lcStyDye'
  lcChldFile = '&lcStyle'
  lcRevFile  = '&REVSTYDY'
  lcLocGroup = [&lcStyDye..CWARECODE]
ENDIF  && end if sort by style or location


*-- What is line cost and Style Cost [Begin]
*-- Update this lines to calculate the stock value on the cost method in IC settings [start]

*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lcLineCost  = [Ave_Cost]
*  lcStyCost   = [&lcStyle..Ave_Cost]
*ELSE
*B608407,1 WAM 01/14/2008 (End)

IF lcCstMeth = "S"
  lcLineCost  = [&lcStyle..TotCost]
  lcStyCost   = [&lcStyle..TotCost]
ELSE
  lcLineCost  = [Ave_Cost]
  lcStyCost   = [&lcStyle..Ave_Cost]
ENDIF
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)

*-- Update this lines to calculate the stock value on the cost method in IC settings [end]
*-- Prepair report cost and price variables [Begin]
DO CASE
  *-- Case show Unit Price and Unit Cost only.
CASE lcRpShow = 'S'
  *-- Per Header.
  *B608189,1 TMI [Start] set the "Unit_price" to the correct alignment
  *lcCost_Val = IIF(lcRepMode = LANG_ICSTYREP_TextMode,' '+LANG_ICSTYREP_Unit_Price + ' '+LANG_ICSTYREP_Sales_Value;
  ,SPACE(3)+LANG_ICSTYREP_Unit_Price + SPACE(3) + LANG_ICSTYREP_Sales_Value)
  lcCost_Val = IIF(lcRepMode = LANG_ICSTYREP_TextMode,''+LANG_ICSTYREP_Unit_Price + '  '+LANG_ICSTYREP_Sales_Value;
    ,SPACE(3)+LANG_ICSTYREP_Unit_Price + SPACE(3) + LANG_ICSTYREP_Sales_Value)
  *B608189,1 TMI [End  ]
  *-- Per Line.
  IF lcRepMode = LANG_ICSTYREP_TextMode

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
    ' ' + TRANSFORM(lnNonPrice,'999999999.99'))"

    lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
                 ' ' + TRANSFORM(lnNonPrice,'999999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ELSE

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *!*	      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
    *!*	      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99'))"
    lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
    lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnNonPrice,'999999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ENDIF
  *-- Per Non Majors from style file.
  IF lcRepMode = LANG_ICSTYREP_TextMode

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
    ' ' + TRANSFORM(lnClrPrice,'999999999.99'))"
    lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
                 ' ' + TRANSFORM(lnClrPrice,'999999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ELSE

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *!*	      lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
    *!*	      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99'))"
    lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
    lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrPrice,'999999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ENDIF
  *-- Per Location Group.
  lcCst_VlLc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcLc,'999999999.99')"
  *-- Per Style (Major) Group.
  IF lcRepMode = LANG_ICSTYREP_TextMode
    lcCst_VlOp = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcOp,'999999999.99')"
  ELSE
    lcCst_VlO1 = "' '"
    lcCst_VlO2 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
  ENDIF

  *-- Per Grand Band.
  IF lcRepMode = LANG_ICSTYREP_TextMode
    lcCst_VlGd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcGd,'999999999.99')"
  ELSE
    lcCst_VlG1 = "' '"
    lcCst_VlG2 = "TRANSFORM(lnNonPrcGd,'999999999.99')"
  ENDIF

  *-- Case show cost values only.
CASE lcRpShow = 'C'
  *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
  *IF llLinkGlJl
  *  lcCost_Val = IIF(lcRepMode = LANG_ICSTYREP_TextMode,'  '+LANG_ICSTYREP_Avrg_Cost + '  '+LANG_ICSTYREP_Cost_Value;
  ,SPACE(2)+' '+LANG_ICSTYREP_Avrg_Cost + SPACE(4) + LANG_ICSTYREP_Cost_Value)
  *ELSE
  *B608407,1 WAM 01/14/2008 (End)
  IF lcCstMeth = "S"
    lcCost_Val = IIF(lcRepMode = LANG_ICSTYREP_TextMode,'  '+LANG_ICSTYREP_Unit_Cost + '  '+LANG_ICSTYREP_Cost_Value;
      ,SPACE(2)+' '+LANG_ICSTYREP_Unit_Cost + SPACE(4) + LANG_ICSTYREP_Cost_Value)
  ELSE
    lcCost_Val = IIF(lcRepMode = LANG_ICSTYREP_TextMode,'  '+LANG_ICSTYREP_Avrg_Cost + '  '+LANG_ICSTYREP_Cost_Value;
      ,SPACE(2)+' '+LANG_ICSTYREP_Avrg_Cost + SPACE(4) + LANG_ICSTYREP_Cost_Value)
  ENDIF
  *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
  *ENDIF
  *B608407,1 WAM 01/14/2008 (End)
  IF lcRepMode = LANG_ICSTYREP_TextMode

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' +;
    TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') +;
    ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
    lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' +;
                 TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') +;
                 ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ELSE

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *!*	      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',;
    *!*	                   TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') )"
    *!*	      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"
    lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',;
                   TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') )"
    lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnNonCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ENDIF

  IF lcRepMode = LANG_ICSTYREP_TextMode


    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(EVALUATE(lcStyCost),'9999999.99') +;
    ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
    lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(EVALUATE(lcStyCost),'9999999.99') +;
                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ELSE

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *!*	      lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(EVALUATE(lcStyCost),'9999999.99') )"
    *!*	      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"
    lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(EVALUATE(lcStyCost),'9999999.99') )"
    lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[end]

  ENDIF

  lcCst_VlLc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstLc,'99999999.99')"

  IF lcRepMode = LANG_ICSTYREP_TextMode
    lcCst_VlOp = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
  ELSE
    lcCst_VlO1 = "' '"
    lcCst_VlO2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
  ENDIF

  IF lcRepMode = LANG_ICSTYREP_TextMode
    lcCst_VlGd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
  ELSE
    lcCst_VlG1 = "' '"
    lcCst_VlG2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
  ENDIF

  *-- Case show both total Price and cost values.
CASE lcRpShow = 'B'
  lcCost_Val =  IIF(lcRepMode = LANG_ICSTYREP_TextMode,LANG_ICSTYREP_Sales_Value + '  '+LANG_ICSTYREP_Cost_Value;
    ,SPACE(1)+LANG_ICSTYREP_Sales_Value +SPACE(3)+ LANG_ICSTYREP_Cost_Value)

  IF lcRepMode = LANG_ICSTYREP_TextMode

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *!*	      lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnNonPrice,'999999999.99') +;
    *!*	                   ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
    lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(lnNonPrice,'999999999.99') +;
                   ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ELSE

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *!*	      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99') )"
    *!*	      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"

    lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer AND  !llChkAllSz,'',TRANSFORM(lnNonPrice,'999999999.99') )"
    lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer AND  !llChkAllSz,'',TRANSFORM(lnNonCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ENDIF

  IF lcRepMode = LANG_ICSTYREP_TextMode

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnClrPrice,'999999999.99') +;
    ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
    lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(lnClrPrice,'999999999.99') +;
                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  ELSE

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *!*	      lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99') )"
    *!*	      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"

    lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrPrice,'999999999.99') )"
    lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrCost,'99999999.99'))"
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  ENDIF

  *: B608756,1 MMT 12/04/2008 Fix bug of Not displaying Sales value when sort by location[Start]
  *lcCst_VlLc = "' ' + TRANSFORM(lnNonPrcLc,'999999999.99') +;
  ' ' + TRANSFORM(lnNonCstLc,'99999999.99')"
  lcCst_VlLc = "Space(2)+ TRANSFORM(lnNonPrcLc,'999999999.99') +;
                  SPACE(5)+ TRANSFORM(lnNonCstLc,'99999999.99')"
  *: B608756,1 MMT 12/04/2008 Fix bug of Not displaying Sales value when sort by location[End]

  IF lcRepMode = LANG_ICSTYREP_TextMode
    lcCst_VlOp = "' ' + TRANSFORM(lnNonPrcOp,'999999999.99') +;
                    ' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
  ELSE
    lcCst_VlO1 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
    lcCst_VlO2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
  ENDIF

  IF lcRepMode = LANG_ICSTYREP_TextMode
    lcCst_VlGd = "' ' + TRANSFORM(lnNonPrcGd,'999999999.99') +;
                    ' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
  ELSE
    lcCst_VlG1 = "TRANSFORM(lnNonPrcGd,'999999999.99')"
    lcCst_VlG2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
  ENDIF
ENDCASE
*-- Prepair report cost and price variables [End  ]

*-- lcRevFlt   : Variable hold Filter for REVSTYDY.
*-- laTrnRevAr : Array Hold Reference to printed transaction From REVSTYDY.
DIMENSION laTrnRevAr [8]
FOR lnI = 1 TO 8
  *B608406,1 WAM 01/14/2008 When Stock is selected, replace lcStydye with the new temp file
  *laTrnRevAr [lnI] = STRTRAN(latrnNonAr[lnI],'&lcStyle',lcRevFile)
  laTrnRevAr [lnI] = STRTRAN(latrnNonAr[lnI],IIF(lcRepTarVl == "SOH",lcStydye,lcStyle),lcRevFile)
  *B608406,1 WAM 01/14/2008 (End)
ENDFOR

*: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
*!*	lcRevFlt = "EVAL(laTrnRevAr[1])+EVAL(laTrnRevAr[2])+"+;
*!*	           "EVAL(laTrnRevAr[3])+EVAL(laTrnRevAr[4])+EVAL(laTrnRevAr[5])+"+;
*!*	           "EVAL(laTrnRevAr[6])+EVAL(laTrnRevAr[7])+EVAL(laTrnRevAr[8]) <> 0"

lcRevFlt = "EVAL(laTrnRevAr[1])<> 0 OR EVAL(laTrnRevAr[2]) <> 0 OR "+;
  "EVAL(laTrnRevAr[3])<> 0 OR EVAL(laTrnRevAr[4]) <> 0 OR EVAL(laTrnRevAr[5]) <> 0 OR "+;
  "EVAL(laTrnRevAr[6])<> 0 OR EVAL(laTrnRevAr[7]) <> 0 OR EVAL(laTrnRevAr[8]) <> 0"
*: B608166,1 SSH 07/15/2007
IF !EMPTY(lcExtndFlt)
  lcRpExp = lcRpExp + lcExtndFlt
  lcRevFlt = lcRevFlt + [ AND ] + lcExtndFlt
  SELECT (REVSTYDY)
  SET FILTER TO &lcExtndFlt
ENDIF
IF !llRPShwZer
  SELECT (lcRevFile)
  SET FILTER TO &lcRevFlt
ENDIF
IF !llRPShwZer &&if don't show zeros quantities
  IF llSChkDyeB AND llRpPrnDye
    IF !EMPTY(lcRpExp)

      *:*B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
      *!*	      lcRpExp = lcRpExp + " AND (EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
      *!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
      *!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"

      lcRpExp = lcRpExp + " AND (EVAL(laTrnNonar[1]) <> 0 OR EVAL(laTrnNonar[2])<> 0 OR " +;
        "EVAL(laTrnNonar[3]) <> 0 OR EVAL(laTrnNonar[4]) <>0 OR EVAL(laTrnNonar[5]) <> 0 OR " +;
        "EVAL(laTrnNonar[6]) <> 0 OR EVAL(laTrnNonar[7]) <>0 OR EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"
      *: B608166,1 SSH 07/15/2007 [END]
    ELSE
      *: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
      *!*	      lcRpExp = lcRpExp + " (EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
      *!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
      *!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"

      lcRpExp = lcRpExp + " (EVAL(laTrnNonar[1]) <> 0 OR EVAL(laTrnNonar[2])<>0 OR"+;
        "EVAL(laTrnNonar[3]) <> 0 OR EVAL(laTrnNonar[4])<>0 OR EVAL(laTrnNonar[5])<>0 OR "+;
        "EVAL(laTrnNonar[6]) <>0 OR EVAL(laTrnNonar[7])<>0 OR EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"
      *: B608166,1 SSH 07/15/2007 [END]
    ENDIF
  ELSE
    *B608507,1 WAM 04/08/2008 Fix filter to check inventory in style file not in STYDYE
    FOR lnI = 1 TO 8
      latrnNonAr[lnI] = STRTRAN(latrnNonAr[lnI],IIF(lcRepTarVl == "SOH",lcStydye,lcStyle),lcMastFile)
    ENDFOR
    *B608507,1 WAM 04/08/2008 (End)

    IF !EMPTY(lcRpExp)
      *: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
      *!*	      lcRpExp = lcRpExp + " AND EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
      *!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
      *!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0"

      lcRpExp = lcRpExp + " AND EVAL(laTrnNonar[1]) <> 0 OR EVAL(laTrnNonar[2]) <>0 OR "+;
        "EVAL(laTrnNonar[3]) <>0 OR EVAL(laTrnNonar[4])<>0 OR EVAL(laTrnNonar[5]) <>0 OR "+;
        "EVAL(laTrnNonar[6])<>0 OR EVAL(laTrnNonar[7])<>0 OR EVAL(laTrnNonar[8]) <> 0"
      *: B608166,1 SSH 07/15/2007 [END]
    ELSE
      *: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
      *!*	      lcRpExp = lcRpExp + " EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
      *!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
      *!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0"
      lcRpExp = lcRpExp + " EVAL(laTrnNonar[1])<>0 OR EVAL(laTrnNonar[2]) <>0 OR "+;
        "EVAL(laTrnNonar[3])<>0 OR EVAL(laTrnNonar[4])<>0 OR EVAL(laTrnNonar[5])<>0 OR "+;
        "EVAL(laTrnNonar[6])<>0 OR EVAL(laTrnNonar[7])<>0 OR EVAL(laTrnNonar[8]) <> 0"
      *: B608166,1 SSH 07/15/2007 [END]
    ENDIF
  ENDIF
ENDIF

*-- Set Relation between Master and Child Files.
SELECT (lcStyle)

IF ATC("INTO &lcscal",SET("Relation")) > 0
  SET RELATION OFF INTO &lcscal
ENDIF
SET RELATION TO "S" + SCALE INTO &lcscal ADDITIVE   && To print Scales.
SELECT (lcMastFile)
*: C201692,1 MMT 07/07/2015 Add Option to export -ve OTS to CSV file[T20150317.0018][Start]
IF ALEN(LARPREPTAR,1) = 1 AND LARPREPTAR[1]="OTS"  AND LCRPOTSSIG ='N' AND !LLRPSHWZER AND LLRPCRTPO
  LCSELF = ALIAS()
  IF !USED('STYLE_V')
    =GFOPENTABLE('STYLE','STYLE','SH','STYLE_V')
  ENDIF
  IF !USED('SCALE_V')
    =GFOPENTABLE('SCALE','SCALE','SH','SCALE_V')
  ENDIF
  CREATE CURSOR 'TMPSTYCODE' (STYLE C(19))
  SELECT 'TMPSTYCODE'
  INDEX ON STYLE TAG 'TMPSTYCODE'
  *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
  *=STRTOFILE(CHR(13),ADDBS(OARIAAPPLICATION.RESOURCEHOME)+ALLTRIM(OARIAAPPLICATION.USER_ID)+"PO.CSV",0)
  IF !DIRECTORY(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))
    MD (ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))
  ENDIF
  =STRTOFILE(CHR(13),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OARIAAPPLICATION.USER_ID)+"PO.CSV",0)
  *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
  SELECT (lcMastFile)
  SCAN 
    IF SEEK(&lcMastFile..STYlE,'TMPSTYCODE','TMPSTYCODE')
      LOOP 
    ELSE
      INSERT INTO 'TMPSTYCODE' VALUES (&LCSELF..STYlE)  
    ENDIF
    =GFSEEK(STYLE,'STYLE_V','STYLE')
    =gfSeek('S'+STYLE_V.Scale,'Scale_V','Scale')
    STORE 0 TO m.Ots1, m.Ots2,m.Ots3,m.Ots4,m.Ots5,m.Ots6,m.Ots7,m.Ots8,M.TOTOTS
    FOR lnCnt = 1 TO Scale_V.Cnt
      lcCntr = STR(lnCnt,1)
      M.ots&lcCntr.  = STYLE_V.STK&lcCntr.-STYLE_V.ORD&lcCntr.+IIF(lcRpOTSB="W",STYLE_V.WIP&lcCntr.,IIF(llRPSTKPL,-1*STYLE_V.PLAN&lcCntr.,STYLE_V.PLAN&lcCntr.))
      M.ots&lcCntr.   = IIF(M.ots&lcCntr.  < 0,M.ots&lcCntr.  ,0)
    ENDFOR 
    M.TOTOTS =M.ots1+M.ots2+M.ots3+M.ots4+M.ots5+M.ots6+M.ots7+M.ots8
    IF M.TOTOTS < 0
      LCSTRING = ""+STYLE+","+ IIF(EMPTY(ALLTRIM(Style_V.vendor)),'TBA',Style_V.vendor)+","+ALLTRIM(STR(M.ots1))+","+ALLTRIM(STR(M.ots2))+","+;
          ALLTRIM(STR(M.ots3))+","+ALLTRIM(STR(M.ots4))+","+ALLTRIM(STR(M.ots5))+;
          ","+ALLTRIM(STR(M.ots6))+","+;
          ALLTRIM(STR(M.ots7))+","+ALLTRIM(STR(M.ots8))+","+ALLTRIM(STR(M.TOTOTS))
      *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]    
      *=STRTOFILE(LCSTRING +CHR(13)+CHR(10),ADDBS(OARIAAPPLICATION.RESOURCEHOME)+ALLTRIM(OARIAAPPLICATION.USER_ID)+"PO.CSV",1)
      =STRTOFILE(LCSTRING +CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OARIAAPPLICATION.USER_ID)+"PO.CSV",1)
      *: B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
    ENDIF  
  ENDSCAN
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'You have created a PO to create file - if you want to process this file - please use the new Create PO screen. If you want to recreate the file with different selections then please reload the option grid and start again.')     
ENDIF
*: C201692,1 MMT 07/07/2015 Add Option to export -ve OTS to CSV file[T20150317.0018][End]


IF ATC("INTO &lcChldFile",SET("Relation")) > 0
  SET RELATION OFF INTO &lcChldFile
ENDIF
SET RELATION TO STYLE INTO (lcChldFile) ADDITIVE
lcNnMajTl = gfItemMask('HN')  && Non Major title.
IF !EMPTY(lcRpExp)
  DO gfDispRe WITH EVAL('lcRPFormNa') , 'FOR ' + lcRpExp
ELSE
  DO gfDispRe WITH EVAL('lcRPFormNa')
ENDIF

IF FILE(oAriaApplication.WorkDir +RevStyDy+".DBF")
  IF USED(RevStyDy)
    USE IN &RevStyDy
  ENDIF
  ERASE oAriaApplication.WorkDir+RevStyDy+".DBF"
  ERASE oAriaApplication.WorkDir+"RevStyDy.CDX"
  ERASE oAriaApplication.WorkDir+"RevStyDyW.CDX"
ENDIF

IF FILE(oAriaApplication.WorkDir +RevStyle+".DBF")
  IF USED(RevStyle)
    USE IN &RevStyle
  ENDIF
  ERASE oAriaApplication.WorkDir+RevStyle+".DBF"
  ERASE oAriaApplication.WorkDir+"RevStyle.CDX"
ENDIF

*-- end of lfSWOneTrn.
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

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
* Important Note :loRDA1.sqlrun caused problem in the cusrsor it return there are wrong fields value
* so When All table will be converted to SQL when can use it with SQL connection string

*lnConnectionHandlar = loRDA1.sqlrun(lcSqlStatment,lcCursor,,oAriaApplication.cAriaNativeDataFilesConStr,3,;
'BROWSE',SET("DATASESSION"))
*IF lnConnectionHandlar = 1
lcSqlStatment   = lcSqlStatment   + " INTO CURSOR &lcCursor READWRITE"
&lcSqlStatment.
IF USED(lcCursor)
  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

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
  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
  *=loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  RETURN .F.
ENDIF

RETURN
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 10/14/2004
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
CASE UPPER(lcTable) = lcStyDye
  DIMENSION laIndex[2,2]
  laIndex[1,1] = 'STYLE+CWARECODE+DYELOT'
  laIndex[1,2] = 'LCSTYDYE'
  laIndex[2,1] = 'CWARECODE+STYLE+DYELOT'
  laIndex[2,2] = 'LCSTYDYEW'
  *!*    CASE UPPER(lcTable) = lcDeptHd
  *!*      DIMENSION laIndex[1,2]
  *!*      laIndex[1,1] = 'DEPT+CSTYGROUP'
  *!*      laIndex[1,2] = 'LCDEPTHD'
  *!*    CASE UPPER(lcTable) = lcDeptDt
  *!*      DIMENSION laIndex[2,2]
  *!*      laIndex[1,1] = 'DEPT+CSTYGROUP+STYLE'
  *!*      laIndex[1,2] = 'LCDEPTDT'
  *!*      laIndex[2,1] = 'STYLE+DEPT'
  *!*      laIndex[2,2] = 'LCDEPTDTS'
CASE UPPER(lcTable) = lcWhsLoc
  DIMENSION laIndex[2,2]
  laIndex[1,1] = 'STYLE+COLOR+CWARECODE+CLOCATION'
  laIndex[1,2] = 'LCWHSLOCST'
  laIndex[2,1] = 'CWARECODE+CLOCATION+STYLE+COLOR'
  laIndex[2,2] = 'LCWHSLOC'
CASE UPPER(lcTable) = lcscal
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'TYPE+SCALE+PREPAK'
  laIndex[1,2] = 'lcscal'
CASE UPPER(lcTable) = lcWareHous
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'CWARECODE'
  laIndex[1,2] = 'LCWAREHOUS'
CASE UPPER(lcTable) = lcSegVal
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'CISEGNO+CISEGVAL'
  laIndex[1,2] = 'LCSEGVAL'
ENDCASE


*!*************************************************************
*! Name      : lfTranAray
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/01/2005
*! Purpose   : Fill Transaction array with proper values to be evaluated in .FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfTranAray()
*!*************************************************************
FUNCTION lfTranAray


PRIVATE lnFindTran
lnFindTran = ASCAN(laAllTrns,laRpRepTar[1])
IF lnFindTran > 0
  lnFindTran = ASUBSCRIPT(laAllTrns,lnFindTran,1)
  lcRepTarVl = ALLTRIM(laAllTrns[lnFindTran,2])
ELSE
  WAIT WINDOW LANG_ICSTYREP_Error NOWAIT
  RETURN
ENDIF

lcPrnTitle = ''  && Print title for transaction, printed in report header.
*-- Fill Transaction Arrays [Begin]

DO CASE
  *------------------------------------- Direct Cases ----------------------
  *-----------------------------------------------------------------------------

  *-- Wip Case...
CASE lcRepTarVl == "WIP"
  =lfDirctVal("WIP")
  lcPrnTitle = LANG_ICSTYREP_WIP

  *-- Stock on hand Case...
CASE lcRepTarVl == "SOH"
  =lfDirctVal("STK")
  lcPrnTitle = LANG_ICSTYREP_Stock

  *-- Plan Case...
CASE lcRepTarVl == "PLA"
  =lfDirctVal("PLAN")
  lcPrnTitle = LANG_ICSTYREP_Plan

  *-- Order Case...
CASE lcRepTarVl == "ORD"
  =lfDirctVal("ORD")
  lcPrnTitle = LANG_ICSTYREP_Ordered

  *-- Work order Case...
CASE lcRepTarVl == "WORD"
  =lfDirctVal("NWO")
  lcPrnTitle = LANG_ICSTYREP_Work_Ordered
  *-- Intransit Case...
CASE lcRepTarVl == "INT"
  =lfDirctVal("INTRANS")
  lcPrnTitle = LANG_ICSTYREP_Intransit

  *-- Shipped Case...
CASE lcRepTarVl == "SHP"
  =lfDirctVal("SHP")
  lcPrnTitle = LANG_ICSTYREP_Shipped

  *-- Credit memo return Case...
CASE lcRepTarVl == "RET"
  =lfDirctVal("RET")
  lcPrnTitle = LANG_ICSTYREP_Return

  *-- Return Authorization Case...
CASE lcRepTarVl == "RETA"
  =lfDirctVal("RA")
  lcPrnTitle = LANG_ICSTYREP_Return_Auth1

  *-- Allocation Case...
CASE lcRepTarVl == "ALO"
  =lfDirctVal("ALO")
  lcPrnTitle = LANG_ICSTYREP_Allocated

  *------------------------------------- Calculated Cases ----------------------
  *-----------------------------------------------------------------------------

  *-- UnAllocate Case...
CASE lcRepTarVl == "UALO"
  =lfInDirect("STK","-ALO")
  lcPrnTitle = LANG_ICSTYREP_Unallocated

  *-- Book Case...
CASE lcRepTarVl == "BOK"
  =lfInDirect("SHP","+ORD")
  lcPrnTitle = LANG_ICSTYREP_Booked

  *------------------------------------- Special Cases ----------------------
  *-----------------------------------------------------------------------------
  *-- OTS Case...
CASE lcRepTarVl == "OTS"
 * =lfInDirect("STK","-ORD","+" + IIF(lcRpOTSB="W","WIP","PLAN"),.T.)  
  =lfInDirect("STK","-ORD",IIF(lcRpOTSB="W","+" + "WIP",IIF(llRPSTKPL,"-PLAN","+" + "PLAN")),.T.)
  lcPrnTitle = LANG_ICSTYREP_Open_to_sell

  *-- IOTS Case...
CASE lcRepTarVl == "IOTS"
  =lfInDirect("STK","-ORD",'',.T.)
  lcPrnTitle = LANG_ICSTYREP_Imm_Open_to_sell

ENDCASE
*-- Fill Transaction Arrays [End  ]
*-- end of lfTranAray.

*!*************************************************************
*! Name      : lfDirctVal
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/01/2005
*! Purpose   : Evaluate Transaction to be printed.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDirctVal()
*!*************************************************************
FUNCTION lfDirctVal
PARAMETER lcDirctVal

PRIVATE lnI
lnI = 0
STORE '' TO laTranAray , laTrnNonAr  && Initially null values.
FOR lnI = 1 TO 8
  laTranAray[lnI] = lcDirctVal + STR(lnI,1)
  IF lcDirctVal="STK"
    laTrnNonAr[lnI] = "&lcStyDye.." + lcDirctVal + STR(lnI,1)
  ELSE
    laTrnNonAr[lnI] = "&lcStyle.." + lcDirctVal + STR(lnI,1)
  ENDIF
ENDFOR

*-- end of lfDirctVal.

*!*************************************************************
*! Name      : lfInDirect
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/01/2005
*! Purpose   : Evaluate Indirect Transactions to be printed.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInDirect()
*!*************************************************************
FUNCTION lfInDirect
PARAMETER lcValue1,lcValue2,lcValue3,llSpecCond
PRIVATE lcClrVal2,lcClrVal3,lnI


lcClrVal2 = STRTRAN(lcValue2,"-","-&lcStyle..")
lcClrVal2 = STRTRAN(lcClrVal2,"+","+&lcStyle..")

IF TYPE('lcValue3') $ 'UL' OR EMPTY(lcValue3)
  lcValue3 = ''
ELSE
  lcClrVal3 = STRTRAN(lcValue3,"-","-&lcStyle..")
  lcClrVal3 = STRTRAN(lcClrVal3,"+","+&lcStyle..")
ENDIF

lnI = 0
STORE '' TO laTranAray , laTrnNonAr  && Intially null values.
FOR lnI = 1 TO 8
  laTranAray[lnI] = lcValue1 + STR(lnI,1) + lcValue2 + STR(lnI,1) +;
    IIF(EMPTY(lcValue3) , "" , lcValue3 + STR(lnI,1))


  laTrnNonAr[lnI] = "&lcStyle.." + lcValue1 + STR(lnI,1) + lcClrVal2 + STR(lnI,1) +;
    IIF(EMPTY(lcValue3) , "" , lcClrVal3 + STR(lnI,1))
  IF llSpecCond
    laTranAray[lnI] = [IIF(((lcRpOTSSig = 'P') AND ] + laTranAray[lnI] +;
      [ < 0) OR ((lcRpOTSSig = 'N') AND ] + laTranAray[lnI] +;
      [ > 0),0,] + laTranAray[lnI] + [)]

    laTrnNonAr[lnI] = [IIF(((lcRpOTSSig = 'P') AND ] + laTrnNonAr[lnI] +;
      [ < 0) OR ((lcRpOTSSig = 'N') AND ] + laTrnNonAr[lnI] +;
      [ > 0),0,] + laTrnNonAr[lnI] + [)]
  ENDIF

ENDFOR
*-- end of lfInDirect.


*!*************************************************************
*! Name      : lfCrTmp
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/02/2005
*! Purpose   : Create temp. files.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrTmp()
*!*************************************************************

FUNCTION lfCrTmp
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
*DIMENSION laFileStru[181,18]
DIMENSION laFileStru[182,18]
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
laFileStru[1,1] = 'StyCode'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 19
laFileStru[1,4] = 0

laFileStru[2,1] = 'StyDesc'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 60
laFileStru[2,4] = 0

laFileStru[3,1] = 'cStyMajor'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 19
laFileStru[3,4] = 0

laFileStru[4,1] = 'Status'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 1
laFileStru[4,4] = 0

laFileStru[5,1] = 'Scale'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 3
laFileStru[5,4] = 0

laFileStru[6,1] = 'Price'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 12
laFileStru[6,4] = 2

laFileStru[7,1] = 'GL_Link'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 6
laFileStru[7,4] = 0

laFileStru[8,1] = 'TotCost'
laFileStru[8,2] = 'N'
laFileStru[8,3] = 13
laFileStru[8,4] = 2

laFileStru[9,1] = 'Ave_Cost'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 15
laFileStru[9,4] = 7

laFileStru[10,1] = 'Dyelot'
laFileStru[10,2] = 'C'
laFileStru[10,3] = 10
laFileStru[10,4] = 0

laFileStru[11,1] = 'WareCode'
laFileStru[11,2] = 'C'
laFileStru[11,3] = 6
laFileStru[11,4] = 0

laFileStru[12,1] = 'WareDesc'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

laFileStru[13,1] = 'WIP1'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 8
laFileStru[13,4] = 0

laFileStru[14,1] = 'WIP2'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 8
laFileStru[14,4] = 0

laFileStru[15,1] = 'WIP3'
laFileStru[15,2] = 'N'
laFileStru[15,3] = 8
laFileStru[15,4] = 0

laFileStru[16,1] = 'WIP4'
laFileStru[16,2] = 'N'
laFileStru[16,3] = 8
laFileStru[16,4] = 0

laFileStru[17,1] = 'WIP5'
laFileStru[17,2] = 'N'
laFileStru[17,3] = 8
laFileStru[17,4] = 0

laFileStru[18,1] = 'WIP6'
laFileStru[18,2] = 'N'
laFileStru[18,3] = 8
laFileStru[18,4] = 0

laFileStru[19,1] = 'WIP7'
laFileStru[19,2] = 'N'
laFileStru[19,3] = 8
laFileStru[19,4] = 0

laFileStru[20,1] = 'WIP8'
laFileStru[20,2] = 'N'
laFileStru[20,3] = 8
laFileStru[20,4] = 0

laFileStru[21,1] = 'TotWIP'
laFileStru[21,2] = 'N'
laFileStru[21,3] = 8
laFileStru[21,4] = 0

laFileStru[22,1] = 'SOH1'
laFileStru[22,2] = 'N'
laFileStru[22,3] = 8
laFileStru[22,4] = 0

laFileStru[23,1] = 'SOH2'
laFileStru[23,2] = 'N'
laFileStru[23,3] = 8
laFileStru[23,4] = 0

laFileStru[24,1] = 'SOH3'
laFileStru[24,2] = 'N'
laFileStru[24,3] = 8
laFileStru[24,4] = 0

laFileStru[25,1] = 'SOH4'
laFileStru[25,2] = 'N'
laFileStru[25,3] = 8
laFileStru[25,4] = 0

laFileStru[26,1] = 'SOH5'
laFileStru[26,2] = 'N'
laFileStru[26,3] = 8
laFileStru[26,4] = 0

laFileStru[27,1] = 'SOH6'
laFileStru[27,2] = 'N'
laFileStru[27,3] = 8
laFileStru[27,4] = 0

laFileStru[28,1] = 'SOH7'
laFileStru[28,2] = 'N'
laFileStru[28,3] = 8
laFileStru[28,4] = 0

laFileStru[29,1] = 'SOH8'
laFileStru[29,2] = 'N'
laFileStru[29,3] = 8
laFileStru[29,4] = 0

laFileStru[30,1] = 'TotSOH'
laFileStru[30,2] = 'N'
laFileStru[30,3] = 8
laFileStru[30,4] = 0

laFileStru[31,1] = 'Pla1'
laFileStru[31,2] = 'N'
laFileStru[31,3] = 10
laFileStru[31,4] = 0

laFileStru[32,1] = 'Pla2'
laFileStru[32,2] = 'N'
laFileStru[32,3] = 10
laFileStru[32,4] = 0

laFileStru[33,1] = 'Pla3'
laFileStru[33,2] = 'N'
laFileStru[33,3] = 10
laFileStru[33,4] = 0

laFileStru[34,1] = 'Pla4'
laFileStru[34,2] = 'N'
laFileStru[34,3] = 10
laFileStru[34,4] = 0

laFileStru[35,1] = 'Pla5'
laFileStru[35,2] = 'N'
laFileStru[35,3] = 10
laFileStru[35,4] = 0

laFileStru[36,1] = 'Pla6'
laFileStru[36,2] = 'N'
laFileStru[36,3] = 10
laFileStru[36,4] = 0

laFileStru[37,1] = 'Pla7'
laFileStru[37,2] = 'N'
laFileStru[37,3] = 10
laFileStru[37,4] = 0

laFileStru[38,1] = 'Pla8'
laFileStru[38,2] = 'N'
laFileStru[38,3] = 10
laFileStru[38,4] = 0

laFileStru[39,1] = 'TotPla'
laFileStru[39,2] = 'N'
laFileStru[39,3] = 10
laFileStru[39,4] = 0

laFileStru[40,1] = 'UAlo1'
laFileStru[40,2] = 'N'
laFileStru[40,3] = 10
laFileStru[40,4] = 0

laFileStru[41,1] = 'UAlo2'
laFileStru[41,2] = 'N'
laFileStru[41,3] = 10
laFileStru[41,4] = 0

laFileStru[42,1] = 'UAlo3'
laFileStru[42,2] = 'N'
laFileStru[42,3] = 10
laFileStru[42,4] = 0

laFileStru[43,1] = 'UAlo4'
laFileStru[43,2] = 'N'
laFileStru[43,3] = 10
laFileStru[43,4] = 0

laFileStru[44,1] = 'UAlo5'
laFileStru[44,2] = 'N'
laFileStru[44,3] = 10
laFileStru[44,4] = 0

laFileStru[45,1] = 'UAlo6'
laFileStru[45,2] = 'N'
laFileStru[45,3] = 10
laFileStru[45,4] = 0

laFileStru[46,1] = 'UAlo7'
laFileStru[46,2] = 'N'
laFileStru[46,3] = 10
laFileStru[46,4] = 0

laFileStru[47,1] = 'UAlo8'
laFileStru[47,2] = 'N'
laFileStru[47,3] = 10
laFileStru[47,4] = 0

laFileStru[48,1] = 'TotUAlo'
laFileStru[48,2] = 'N'
laFileStru[48,3] = 10
laFileStru[48,4] = 0

laFileStru[49,1] = 'OTS1'
laFileStru[49,2] = 'N'
laFileStru[49,3] = 8
laFileStru[49,4] = 0

laFileStru[50,1] = 'OTS2'
laFileStru[50,2] = 'N'
laFileStru[50,3] = 8
laFileStru[50,4] = 0

laFileStru[51,1] = 'OTS3'
laFileStru[51,2] = 'N'
laFileStru[51,3] = 8
laFileStru[51,4] = 0

laFileStru[52,1] = 'OTS4'
laFileStru[52,2] = 'N'
laFileStru[52,3] = 8
laFileStru[52,4] = 0

laFileStru[53,1] = 'OTS5'
laFileStru[53,2] = 'N'
laFileStru[53,3] = 8
laFileStru[53,4] = 0

laFileStru[54,1] = 'OTS6'
laFileStru[54,2] = 'N'
laFileStru[54,3] = 8
laFileStru[54,4] = 0

laFileStru[55,1] = 'OTS7'
laFileStru[55,2] = 'N'
laFileStru[55,3] = 8
laFileStru[55,4] = 0

laFileStru[56,1] = 'OTS8'
laFileStru[56,2] = 'N'
laFileStru[56,3] = 8
laFileStru[56,4] = 0

laFileStru[57,1] = 'TotOTS'
laFileStru[57,2] = 'N'
laFileStru[57,3] = 8
laFileStru[57,4] = 0

laFileStru[58,1] = 'IOTS1'
laFileStru[58,2] = 'N'
laFileStru[58,3] = 8
laFileStru[58,4] = 0

laFileStru[59,1] = 'IOTS2'
laFileStru[59,2] = 'N'
laFileStru[59,3] = 8
laFileStru[59,4] = 0

laFileStru[60,1] = 'IOTS3'
laFileStru[60,2] = 'N'
laFileStru[60,3] = 8
laFileStru[60,4] = 0

laFileStru[61,1] = 'IOTS4'
laFileStru[61,2] = 'N'
laFileStru[61,3] = 8
laFileStru[61,4] = 0

laFileStru[62,1] = 'IOTS5'
laFileStru[62,2] = 'N'
laFileStru[62,3] = 8
laFileStru[62,4] = 0

laFileStru[63,1] = 'IOTS6'
laFileStru[63,2] = 'N'
laFileStru[63,3] = 8
laFileStru[63,4] = 0

laFileStru[64,1] = 'IOTS7'
laFileStru[64,2] = 'N'
laFileStru[64,3] = 8
laFileStru[64,4] = 0

laFileStru[65,1] = 'IOTS8'
laFileStru[65,2] = 'N'
laFileStru[65,3] = 8
laFileStru[65,4] = 0

laFileStru[66,1] = 'TotIOTS'
laFileStru[66,2] = 'N'
laFileStru[66,3] = 8
laFileStru[66,4] = 0

laFileStru[67,1] = 'Ord1'
laFileStru[67,2] = 'N'
laFileStru[67,3] = 8
laFileStru[67,4] = 0

laFileStru[68,1] = 'Ord2'
laFileStru[68,2] = 'N'
laFileStru[68,3] = 8
laFileStru[68,4] = 0

laFileStru[69,1] = 'Ord3'
laFileStru[69,2] = 'N'
laFileStru[69,3] = 8
laFileStru[69,4] = 0

laFileStru[70,1] = 'Ord4'
laFileStru[70,2] = 'N'
laFileStru[70,3] = 8
laFileStru[70,4] = 0

laFileStru[71,1] = 'Ord5'
laFileStru[71,2] = 'N'
laFileStru[71,3] = 8
laFileStru[71,4] = 0

laFileStru[72,1] = 'Ord6'
laFileStru[72,2] = 'N'
laFileStru[72,3] = 8
laFileStru[72,4] = 0

laFileStru[73,1] = 'Ord7'
laFileStru[73,2] = 'N'
laFileStru[73,3] = 8
laFileStru[73,4] = 0

laFileStru[74,1] = 'Ord8'
laFileStru[74,2] = 'N'
laFileStru[74,3] = 8
laFileStru[74,4] = 0

laFileStru[75,1] = 'TotOrd'
laFileStru[75,2] = 'N'
laFileStru[75,3] = 8
laFileStru[75,4] = 0

laFileStru[76,1] = 'WOrd1'
laFileStru[76,2] = 'N'
laFileStru[76,3] = 8
laFileStru[76,4] = 0

laFileStru[77,1] = 'WOrd2'
laFileStru[77,2] = 'N'
laFileStru[77,3] = 8
laFileStru[77,4] = 0

laFileStru[78,1] = 'WOrd3'
laFileStru[78,2] = 'N'
laFileStru[78,3] = 8
laFileStru[78,4] = 0

laFileStru[79,1] = 'WOrd4'
laFileStru[79,2] = 'N'
laFileStru[79,3] = 8
laFileStru[79,4] = 0

laFileStru[80,1] = 'WOrd5'
laFileStru[80,2] = 'N'
laFileStru[80,3] = 8
laFileStru[80,4] = 0

laFileStru[81,1] = 'WOrd6'
laFileStru[81,2] = 'N'
laFileStru[81,3] = 8
laFileStru[81,4] = 0

laFileStru[82,1] = 'WOrd7'
laFileStru[82,2] = 'N'
laFileStru[82,3] = 8
laFileStru[82,4] = 0

laFileStru[83,1] = 'WOrd8'
laFileStru[83,2] = 'N'
laFileStru[83,3] = 8
laFileStru[83,4] = 0

laFileStru[84,1] = 'TotWOrd'
laFileStru[84,2] = 'N'
laFileStru[84,3] = 8
laFileStru[84,4] = 0

laFileStru[85,1] = 'Int1'
laFileStru[85,2] = 'N'
laFileStru[85,3] = 8
laFileStru[85,4] = 0

laFileStru[86,1] = 'Int2'
laFileStru[86,2] = 'N'
laFileStru[86,3] = 8
laFileStru[86,4] = 0

laFileStru[87,1] = 'Int3'
laFileStru[87,2] = 'N'
laFileStru[87,3] = 8
laFileStru[87,4] = 0

laFileStru[88,1] = 'Int4'
laFileStru[88,2] = 'N'
laFileStru[88,3] = 8
laFileStru[88,4] = 0

laFileStru[89,1] = 'Int5'
laFileStru[89,2] = 'N'
laFileStru[89,3] = 8
laFileStru[89,4] = 0

laFileStru[90,1] = 'Int6'
laFileStru[90,2] = 'N'
laFileStru[90,3] = 8
laFileStru[90,4] = 0

laFileStru[91,1] = 'Int7'
laFileStru[91,2] = 'N'
laFileStru[91,3] = 8
laFileStru[91,4] = 0

laFileStru[92,1] = 'Int8'
laFileStru[92,2] = 'N'
laFileStru[92,3] = 8
laFileStru[92,4] = 0

laFileStru[93,1] = 'TotInt'
laFileStru[93,2] = 'N'
laFileStru[93,3] = 8
laFileStru[93,4] = 0

laFileStru[94,1] = 'Bok1'
laFileStru[94,2] = 'N'
laFileStru[94,3] = 8
laFileStru[94,4] = 0

laFileStru[95,1] = 'Bok2'
laFileStru[95,2] = 'N'
laFileStru[95,3] = 8
laFileStru[95,4] = 0

laFileStru[96,1] = 'Bok3'
laFileStru[96,2] = 'N'
laFileStru[96,3] = 8
laFileStru[96,4] = 0

laFileStru[97,1] = 'Bok4'
laFileStru[97,2] = 'N'
laFileStru[97,3] = 8
laFileStru[97,4] = 0

laFileStru[98,1] = 'Bok5'
laFileStru[98,2] = 'N'
laFileStru[98,3] = 8
laFileStru[98,4] = 0

laFileStru[99,1] = 'Bok6'
laFileStru[99,2] = 'N'
laFileStru[99,3] = 8
laFileStru[99,4] = 0

laFileStru[100,1] = 'Bok7'
laFileStru[100,2] = 'N'
laFileStru[100,3] = 8
laFileStru[100,4] = 0

laFileStru[101,1] = 'Bok8'
laFileStru[101,2] = 'N'
laFileStru[101,3] = 8
laFileStru[101,4] = 0

laFileStru[102,1] = 'TotBok'
laFileStru[102,2] = 'N'
laFileStru[102,3] = 8
laFileStru[102,4] = 0

laFileStru[103,1] = 'Shp1'
laFileStru[103,2] = 'N'
laFileStru[103,3] = 8
laFileStru[103,4] = 0

laFileStru[104,1] = 'Shp2'
laFileStru[104,2] = 'N'
laFileStru[104,3] = 8
laFileStru[104,4] = 0

laFileStru[105,1] = 'Shp3'
laFileStru[105,2] = 'N'
laFileStru[105,3] = 8
laFileStru[105,4] = 0

laFileStru[106,1] = 'Shp4'
laFileStru[106,2] = 'N'
laFileStru[106,3] = 8
laFileStru[106,4] = 0

laFileStru[107,1] = 'Shp5'
laFileStru[107,2] = 'N'
laFileStru[107,3] = 8
laFileStru[107,4] = 0

laFileStru[108,1] = 'Shp6'
laFileStru[108,2] = 'N'
laFileStru[108,3] = 8
laFileStru[108,4] = 0

laFileStru[109,1] = 'Shp7'
laFileStru[109,2] = 'N'
laFileStru[109,3] = 8
laFileStru[109,4] = 0

laFileStru[110,1] = 'Shp8'
laFileStru[110,2] = 'N'
laFileStru[110,3] = 8
laFileStru[110,4] = 0

laFileStru[111,1] = 'TotShp'
laFileStru[111,2] = 'N'
laFileStru[111,3] = 8
laFileStru[111,4] = 0

laFileStru[112,1] = 'Alo1'
laFileStru[112,2] = 'N'
laFileStru[112,3] = 8
laFileStru[112,4] = 0

laFileStru[113,1] = 'Alo2'
laFileStru[113,2] = 'N'
laFileStru[113,3] = 8
laFileStru[113,4] = 0

laFileStru[114,1] = 'Alo3'
laFileStru[114,2] = 'N'
laFileStru[114,3] = 8
laFileStru[114,4] = 0

laFileStru[115,1] = 'Alo4'
laFileStru[115,2] = 'N'
laFileStru[115,3] = 8
laFileStru[115,4] = 0

laFileStru[116,1] = 'Alo5'
laFileStru[116,2] = 'N'
laFileStru[116,3] = 8
laFileStru[116,4] = 0

laFileStru[117,1] = 'Alo6'
laFileStru[117,2] = 'N'
laFileStru[117,3] = 8
laFileStru[117,4] = 0

laFileStru[118,1] = 'Alo7'
laFileStru[118,2] = 'N'
laFileStru[118,3] = 8
laFileStru[118,4] = 0

laFileStru[119,1] = 'Alo8'
laFileStru[119,2] = 'N'
laFileStru[119,3] = 8
laFileStru[119,4] = 0

laFileStru[120,1] = 'TotAlo'
laFileStru[120,2] = 'N'
laFileStru[120,3] = 8
laFileStru[120,4] = 0

laFileStru[121,1] = 'Ret1'
laFileStru[121,2] = 'N'
laFileStru[121,3] = 8
laFileStru[121,4] = 0

laFileStru[122,1] = 'Ret2'
laFileStru[122,2] = 'N'
laFileStru[122,3] = 8
laFileStru[122,4] = 0

laFileStru[123,1] = 'Ret3'
laFileStru[123,2] = 'N'
laFileStru[123,3] = 8
laFileStru[123,4] = 0

laFileStru[124,1] = 'Ret4'
laFileStru[124,2] = 'N'
laFileStru[124,3] = 8
laFileStru[124,4] = 0

laFileStru[125,1] = 'Ret5'
laFileStru[125,2] = 'N'
laFileStru[125,3] = 8
laFileStru[125,4] = 0

laFileStru[126,1] = 'Ret6'
laFileStru[126,2] = 'N'
laFileStru[126,3] = 8
laFileStru[126,4] = 0

laFileStru[127,1] = 'Ret7'
laFileStru[127,2] = 'N'
laFileStru[127,3] = 8
laFileStru[127,4] = 0

laFileStru[128,1] = 'Ret8'
laFileStru[128,2] = 'N'
laFileStru[128,3] = 8
laFileStru[128,4] = 0

laFileStru[129,1] = 'TotRet'
laFileStru[129,2] = 'N'
laFileStru[129,3] = 8
laFileStru[129,4] = 0

laFileStru[130,1] = 'RetA1'
laFileStru[130,2] = 'N'
laFileStru[130,3] = 8
laFileStru[130,4] = 0

laFileStru[131,1] = 'RetA2'
laFileStru[131,2] = 'N'
laFileStru[131,3] = 8
laFileStru[131,4] = 0

laFileStru[132,1] = 'RetA3'
laFileStru[132,2] = 'N'
laFileStru[132,3] = 8
laFileStru[132,4] = 0

laFileStru[133,1] = 'RetA4'
laFileStru[133,2] = 'N'
laFileStru[133,3] = 8
laFileStru[133,4] = 0

laFileStru[134,1] = 'RetA5'
laFileStru[134,2] = 'N'
laFileStru[134,3] = 8
laFileStru[134,4] = 0

laFileStru[135,1] = 'RetA6'
laFileStru[135,2] = 'N'
laFileStru[135,3] = 8
laFileStru[135,4] = 0

laFileStru[136,1] = 'RetA7'
laFileStru[136,2] = 'N'
laFileStru[136,3] = 8
laFileStru[136,4] = 0

laFileStru[137,1] = 'RetA8'
laFileStru[137,2] = 'N'
laFileStru[137,3] = 8
laFileStru[137,4] = 0

laFileStru[138,1] = 'TotRetA'
laFileStru[138,2] = 'N'
laFileStru[138,3] = 8
laFileStru[138,4] = 0

laFileStru[139,1] = 'HasDye'
laFileStru[139,2] = 'L'
laFileStru[139,3] = 0
laFileStru[139,4] = 0

laFileStru[140,1] = 'lPrnOTS'
laFileStru[140,2] = 'L'
laFileStru[140,3] = 0
laFileStru[140,4] = 0

laFileStru[141,1] = 'lPrnIOTS'
laFileStru[141,2] = 'L'
laFileStru[141,3] = 0
laFileStru[141,4] = 0

laFileStru[142,1] = 'Season'
laFileStru[142,2] = 'C'
laFileStru[142,3] = 6
laFileStru[142,4] = 0

laFileStru[143,1] = 'Division'
laFileStru[143,2] = 'C'
laFileStru[143,3] = 6
laFileStru[143,4] = 0

laFileStru[144,1] = 'FGroup'
laFileStru[144,2] = 'C'
laFileStru[144,3] = 7
laFileStru[144,4] = 0

laFileStru[145,1] = 'SGroup'
laFileStru[145,2] = 'C'
laFileStru[145,3] = 6
laFileStru[145,4] = 0

laFileStru[146,1] = 'Loc'
laFileStru[146,2] = 'M'
laFileStru[146,3] = 0
laFileStru[146,4] = 0

laFileStru[147,1] = 'HasLoc'
laFileStru[147,2] = 'L'
laFileStru[147,3] = 1
laFileStru[147,4] = 0

laFileStru[148,1] = 'nStkVWIP'
laFileStru[148,2] = 'N'
laFileStru[148,3] = 18
laFileStru[148,4] = 2

laFileStru[149,1] = 'nStkVSOH'
laFileStru[149,2] = 'N'
laFileStru[149,3] = 18
laFileStru[149,4] = 2

laFileStru[150,1] = 'nStkVPLA'
laFileStru[150,2] = 'N'
laFileStru[150,3] = 18
laFileStru[150,4] = 2

laFileStru[151,1] = 'nStkVOTS'
laFileStru[151,2] = 'N'
laFileStru[151,3] = 18
laFileStru[151,4] = 2

laFileStru[152,1] = 'nStkVIOTS'
laFileStru[152,2] = 'N'
laFileStru[152,3] = 18
laFileStru[152,4] = 2

laFileStru[153,1] = 'nStkVBOK'
laFileStru[153,2] = 'N'
laFileStru[153,3] = 18
laFileStru[153,4] = 2

laFileStru[154,1] = 'nStkVSHP'
laFileStru[154,2] = 'N'
laFileStru[154,3] = 18
laFileStru[154,4] = 2

laFileStru[155,1] = 'nStkVRet'
laFileStru[155,2] = 'N'
laFileStru[155,3] = 18
laFileStru[155,4] = 2

laFileStru[156,1] = 'nStkVRetA'
laFileStru[156,2] = 'N'
laFileStru[156,3] = 18
laFileStru[156,4] = 2

laFileStru[157,1] = 'nStkVAlo'
laFileStru[157,2] = 'N'
laFileStru[157,3] = 18
laFileStru[157,4] = 2

laFileStru[158,1] = 'nStkVUAlo'
laFileStru[158,2] = 'N'
laFileStru[158,3] = 18
laFileStru[158,4] = 2

laFileStru[159,1] = 'nStkVInt'
laFileStru[159,2] = 'N'
laFileStru[159,3] = 18
laFileStru[159,4] = 2

laFileStru[160,1] = 'nStkVWOrd'
laFileStru[160,2] = 'N'
laFileStru[160,3] = 18
laFileStru[160,4] = 2

laFileStru[161,1] = 'nStkVOrd'
laFileStru[161,2] = 'N'
laFileStru[161,3] = 18
laFileStru[161,4] = 2

laFileStru[162,1] = 'nSalVWIP'
laFileStru[162,2] = 'N'
laFileStru[162,3] = 18
laFileStru[162,4] = 2

laFileStru[163,1] = 'nSalVSOH'
laFileStru[163,2] = 'N'
laFileStru[163,3] = 18
laFileStru[163,4] = 2

laFileStru[164,1] = 'nSalVPLA'
laFileStru[164,2] = 'N'
laFileStru[164,3] = 18
laFileStru[164,4] = 2

laFileStru[165,1] = 'nSalVOTS'
laFileStru[165,2] = 'N'
laFileStru[165,3] = 18
laFileStru[165,4] = 2

laFileStru[166,1] = 'nSalVIOTS'
laFileStru[166,2] = 'N'
laFileStru[166,3] = 18
laFileStru[166,4] = 2

laFileStru[167,1] = 'nSalVBOK'
laFileStru[167,2] = 'N'
laFileStru[167,3] = 18
laFileStru[167,4] = 2

laFileStru[168,1] = 'nSalVSHP'
laFileStru[168,2] = 'N'
laFileStru[168,3] = 18
laFileStru[168,4] = 2

laFileStru[169,1] = 'nSalVRet'
laFileStru[169,2] = 'N'
laFileStru[169,3] = 18
laFileStru[169,4] = 2

laFileStru[170,1] = 'nSalVRetA'
laFileStru[170,2] = 'N'
laFileStru[170,3] = 18
laFileStru[170,4] = 2

laFileStru[171,1] = 'nSalVAlo'
laFileStru[171,2] = 'N'
laFileStru[171,3] = 18
laFileStru[171,4] = 2

laFileStru[172,1] = 'nSalVUAlo'
laFileStru[172,2] = 'N'
laFileStru[172,3] = 18
laFileStru[172,4] = 2

laFileStru[173,1] = 'nSalVInt'
laFileStru[173,2] = 'N'
laFileStru[173,3] = 18
laFileStru[173,4] = 2

laFileStru[174,1] = 'nSalVWOrd'
laFileStru[174,2] = 'N'
laFileStru[174,3] = 18
laFileStru[174,4] = 2

laFileStru[175,1] = 'nSalVOrd'
laFileStru[175,2] = 'N'
laFileStru[175,3] = 18
laFileStru[175,4] = 2

laFileStru[176,1] = 'cEndMaj'
laFileStru[176,2] = 'C'
laFileStru[176,3] = 1
laFileStru[176,4] = 0

laFileStru[177,1] = 'cEndSort'
laFileStru[177,2] = 'C'
laFileStru[177,3] = 1
laFileStru[177,4] = 0

laFileStru[178,1] = 'cEndRep'
laFileStru[178,2] = 'C'
laFileStru[178,3] = 1
laFileStru[178,4] = 0

laFileStru[179,1] = 'NotScale'
laFileStru[179,2] = 'C'
laFileStru[179,3] = 1
laFileStru[179,4] = 0

laFileStru[180,1] = 'MatType'
laFileStru[180,2] = 'C'
laFileStru[180,3] = 20
laFileStru[180,4] = 0

laFileStru[181,1] = 'Dept'
laFileStru[181,2] = 'C'
laFileStru[181,3] = 5
laFileStru[181,4] = 0

*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
laFileStru[182,1] = 'Color'
laFileStru[182,2] = 'C'
laFileStru[182,3] = 30
laFileStru[182,4] = 0
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]

DIMENSION laIndx[2,2]
laIndx[1,1] = "WareCode+StyCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
laIndx[1,2] = 'WareSort'
laIndx[2,1] = "StyCode+WareCode+Dyelot+NotScale"
laIndx[2,2] = 'StySort'

FOR lnCnt=7 TO 16
  *B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
  *FOR lnInc=1 TO 181
  FOR lnInc=1 TO 182
    *B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
    STORE SPACE(0) TO laFileStru[lnInc,lnCnt]
  ENDFOR
ENDFOR
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
*FOR lnInc=1 TO 181
FOR lnInc=1 TO 182
  *B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
  STORE 0 TO laFileStru[lnInc,17] , laFileStru[lnInc,18]
ENDFOR

=gfCrtTmp(lcTrns,@laFileStru,@laIndx,lcTrns,.F.)


SELECT(lcTrns)
=AFIELDS(laFileStru)
DIMENSION laIndx[1,2]
DO CASE
CASE lcRpSortBy == 'S'   && Sort by Style
  laIndx[1,1] = "StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
  laIndx[1,2] = 'StySort'
CASE lcRpSortBy == 'SE'  && Sort by season
  laIndx[1,1] = "Season+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
  laIndx[1,2] = 'SeaSort'
CASE lcRpSortBy == 'D'   && Sort by division
  laIndx[1,1] = "Division+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
  laIndx[1,2] = 'DivSort'
CASE lcRpSortBy == 'FG'  && Sort by Primary Fabric
  laIndx[1,1] = "FGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
  laIndx[1,2] = 'FGrpSort'
CASE lcRpSortBy == 'SG'  && Sort by Style Group
  laIndx[1,1] = "SGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
  laIndx[1,2] = 'SGrpSort'
CASE lcRpSortBy == 'MT'  && Sort by Material Type
  laIndx[1,1] = "MatType+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
  laIndx[1,2] = 'MatTypSort'
CASE lcRpSortBy == 'DE'  && Sort by Department
  laIndx[1,1] = "DEPT+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
  laIndx[1,2] = 'DeptSort'
  *! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [Start]
CASE lcRpSortBy == 'W'  && Sort by Department
  laIndx[1,1] = "CEndRep+WareCode+CendMaj+CEndSort+StyCode+Dyelot+NotScale"
  laIndx[1,2] = 'WareSort2'
  *! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [End]
ENDCASE

=gfCrtTmp(lcStyTmp,@laFileStru,@laIndx,lcStyTmp,.F.)


DO CASE
CASE lcRpSortBy == 'W'   && Sort by Location
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+WareCode+cStyMajor+NotScale"
  laIndx[1,2] = 'WareSort'
CASE lcRpSortBy == 'S'   && Sort by Style
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+cStyMajor+NotScale"
  laIndx[1,2] = 'StySort'
CASE lcRpSortBy == 'SE'  && Sort by season
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Season+cStyMajor+NotScale"
  laIndx[1,2] = 'SeaSort'
CASE lcRpSortBy == 'D'   && Sort by division
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Division+cStyMajor+NotScale"
  laIndx[1,2] = 'DivSort'
CASE lcRpSortBy == 'FG'  && Sort by Primary Fabric
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+FGroup+cStyMajor+NotScale"
  laIndx[1,2] = 'FGrpSort'
CASE lcRpSortBy == 'SG'  && Sort by Style Group
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+SGroup+cStyMajor+NotScale"
  laIndx[1,2] = 'SGrpSort'
CASE lcRpSortBy == 'MT'   && Sort by Material Type
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+MatType+cStyMajor+NotScale"
  laIndx[1,2] = 'MatTypSort'
CASE lcRpSortBy == 'DE'   && Sort by Department
  laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Dept+cStyMajor+NotScale"
  laIndx[1,2] = 'DeptSort'
ENDCASE

=gfCrtTmp(lcTotTmp,@laFileStru,@laIndx,lcTotTmp,.F.)

lnI=ALEN(laFileStru,1)
DIMENSION laFileStru[lnI+3,18]

laFileStru[lnI+1,1] = 'Item'
laFileStru[lnI+1,2] = 'C'
laFileStru[lnI+1,3] = 4
laFileStru[lnI+1,4] = 0

laFileStru[lnI+2,1] = 'Label'
laFileStru[lnI+2,2] = 'C'
laFileStru[lnI+2,3] = 14
laFileStru[lnI+2,4] = 0

laFileStru[lnI+3,1] = 'cRecNo'
laFileStru[lnI+3,2] = 'C'
laFileStru[lnI+3,3] = 2
laFileStru[lnI+3,4] = 0

FOR lnCnt=7 TO 16
  FOR lnInc=1 TO 3
    STORE SPACE(0) TO laFileStru[lnI+lnInc,lnCnt]
  ENDFOR
ENDFOR
FOR lnInc=1 TO 3
  STORE 0 TO laFileStru[lnI+lnInc,17] , laFileStru[lnI+lnInc,18]
ENDFOR

=gfCrtTmp(lcDummy,@laFileStru,"cRecNo",lcDummy,.F.)


*!*************************************************************
*! Name      : lfDummFill
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : To fill target array
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDummFill()
*!*************************************************************

FUNCTION lfDummFill

*-- The need of "laTarArray" array is to hold the target transactions beside
*-- adding element for dyelot if "Print dyelot detail" is selected, and that
*-- is instead of adding this element to laRPRepTar array inorder to not change
*-- it because "Dyelot" should not appear in laRPRepTar array(in the mover).

= ACOPY(laRPRepTar,laTarArray)

IF lcRpSortBy = 'W'
  IF llRPPrnDye
    lnStkPo = ASCAN(laTarArray,LANG_ICSTYREP_Stock)
    IF lnStkPo > 0
      DIMENSION laTarArray[ALEN(laTarArray)+1]
      =AINS(laTarArray,lnStkPo+1)
      laTarArray[lnStkPo+1] = LANG_ICSTYREP_Dyelot
    ENDIF
  ELSE
    lnDyePo = ASCAN(laTarArray,LANG_ICSTYREP_Dyelot)
    IF lnDyePo > 0
      = ADEL(laTarArray,lnDyePo)
      DIMENSION laTarArray[ALEN(laTarArray,1)-1]
    ENDIF
  ENDIF
ENDIF

FOR lnI = 1 TO ALEN(laTarArray)
  *-- lnElemNo  = ASUBSCRIPT(laAllTrns,ASCAN(laAllTrns,laTarArray[lnI]),1)
  lnElemNo = ASCAN(laAllTrns,laTarArray[lnI],1,ALEN(laAllTrns,1),1,9)

  IF !(UPPER(laAllTrns[lnElemNo,2]) $ "IOTS")
    llOnlyOTS = .F.
  ENDIF

  lcAllTrns = lcAllTrns + "," +laAllTrns[lnElemNo,2]+ ","
  IF !(UPPER(laAllTrns[lnElemNo,2]) $ "OTS,IOTS,BOK") AND UPPER(laAllTrns[lnElemNo,2]) <> "UALO"
    lcTargFlds = IIF(EMPTY(lcTargFlds),lcTargFlds,lcTargFlds+",") + ;
      laAllTrns[lnElemNo,2]+'1,'+laAllTrns[lnElemNo,2]+'2,'+;
      laAllTrns[lnElemNo,2]+'3,'+laAllTrns[lnElemNo,2]+'4,'+;
      laAllTrns[lnElemNo,2]+'5,'+laAllTrns[lnElemNo,2]+'6,'+;
      laAllTrns[lnElemNo,2]+'7,'+laAllTrns[lnElemNo,2]+'8,'+;
      'Tot'+laAllTrns[lnElemNo,2]  &&Example  OTS1,OTS2,.....,TotOTS

    = lfSourFlds(UPPER(laAllTrns[lnElemNo,2]))
  ENDIF

  INSERT INTO (lcDummy) (ITEM                 ,LABEL          ,cRecNo        ) ;
    VALUES (laAllTrns[lnElemNo,2],laTarArray[lnI],PADL(lnI,2,'0'))
ENDFOR
INSERT INTO (lcDummy) (ITEM,LABEL,cRecNo             ) ;
  VALUES (''  ,''   ,PADL(RECNO(),2,'0'))

*!*************************************************************
*! Name      : lfSourFlds
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : Function used to get the required fields
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :(lcTrnFld)Transaction Field
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSourFlds()
*!*************************************************************

FUNCTION lfSourFlds
PARAMETERS lcTrnFld

PRIVATE lcTrnFld
DO CASE
CASE lcTrnFld = "WIP"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP"
CASE lcTrnFld = "SOH"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TotSTK"
CASE lcTrnFld = "PLA"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "PLAN1,PLAN2,PLAN3,PLAN4,PLAN5,PLAN6,PLAN7,PLAN8,TotPLAN"
CASE lcTrnFld = "ORD"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotORD"
CASE lcTrnFld = "WORD"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "NWO1,NWO2,NWO3,NWO4,NWO5,NWO6,NWO7,NWO8,NTotWO"
CASE lcTrnFld = "INT"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "INTRANS1,INTRANS2,INTRANS3,INTRANS4,INTRANS5,INTRANS6,INTRANS7,INTRANS8,TotINTRN"
CASE lcTrnFld = "SHP"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "SHP1,SHP2,SHP3,SHP4,SHP5,SHP6,SHP7,SHP8,TotSHP"
CASE lcTrnFld = "ALO"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "ALO1,ALO2,ALO3,ALO4,ALO5,ALO6,ALO7,ALO8,TotALO"
CASE lcTrnFld == "RET"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "RET1,RET2,RET3,RET4,RET5,RET6,RET7,RET8,TotRET"
CASE lcTrnFld = "RETA"
  lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
    "RA1,RA2,RA3,RA4,RA5,RA6,RA7,RA8,TotRA"
ENDCASE





*!*************************************************************
*! Name      : lfDatCollW
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : Collecting data if it is sorted by location
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDatCollW()
*!*************************************************************

FUNCTION lfDatCollW

PRIVATE lcStyl1,lcDyelot,llHasDye,lcScale,lcWareCode

llHasDye = .F.
llAdded  = .F.    && 4 ONLY.

SELECT (lcStyle)
SET ORDER TO TAG lcStyle

IF ATC("INTO &lcscal",SET("Relation")) > 0
  SET RELATION OFF INTO &lcscal
ENDIF
SET RELATION TO 'S' + SCALE INTO &lcscal ADDITIVE





SELECT (lcStyDye)
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*B608163,1 TMI [Start] do not use the asscending order
*SET ORDER TO TAG lcStyDyeW DESCENDING
SET ORDER TO TAG lcStyDyeW
*B608163,1 TMI [End  ]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]

IF ATC("INTO &lcStyle",SET("Relation")) > 0
  SET RELATION OFF INTO &lcStyle
ENDIF
SET RELATION TO STYLE INTO &lcStyle ADDITIVE


lcStyl1  = SPACE(19)
lcscale = SPACE(3)
lcWareCode= SPACE(6)

SCAN


  llAdded = .T.
  *--Collect data From StyDye in case of (dyelots for Stock)=Yes ,or (print configuration)=Yes
  IF llConfig
    llSetUp= llRpConfig
  ELSE
    llSetUp= llRpPrnDye
  ENDIF
  IF llSetUp
    IF lcStyl1 <> &lcStyDye..STYLE
      llHasDye = !EMPTY(Dyelot)
    ENDIF
    lcStyl1  = &lcStyDye..STYLE
  ENDIF
  IF !EMPTY(Dyelot)
    LOOP
  ENDIF

  DO lpInsInTmp WITH '&lcStyDye'

  *: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
  *!*	  IF !llRPShwZer AND ;
  *!*	     (&lcTrns..TotWip  + &lcTrns..TotSOH + &lcTrns..TotPLA  + &lcTrns..TotOTS + ;
  *!*	      &lcTrns..TotIOTS + &lcTrns..TotBOK + &lcTrns..TotSHP  + &lcTrns..TotRet + ;
  *!*	      &lcTrns..TotRetA + &lcTrns..TotAlo + &lcTrns..TotUAlo + &lcTrns..TotInt + ;
  *!*	      &lcTrns..TotWOrd + &lcTrns..TotOrd) = 0

  IF !llRPShwZer AND lfZeroStok()
    *: B608166,1 SSH 07/15/2007 end
    SELECT (lcTrns)
    DELETE
    llAdded = .F.
  ELSE
    IF llOnlyOTS AND !&lcTrns..lPrnOTS AND !&lcTrns..lPrnIOTS
      SELECT (lcTrns)
      DELETE
      llAdded = .F.
    ELSE
      SELECT (lcTrns)
      *B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
      *SCATTER FIELDS cStyMajor,StyCode,WareCode,Dyelot,Scale,Price,TotCost,Ave_Cost MEMVAR
      SCATTER FIELDS cStyMajor,StyCode,WareCode,Dyelot,SCALE,Price,TotCost,Ave_Cost,COLOR MEMVAR
      *B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
      SELECT (lcTotTmp)
      IF !SEEK('A  '+&lcTrns..WareCode+&lcTrns..cStyMajor)
        APPEND BLANK
        GATHER MEMVAR
        REPLACE cEndMaj WITH "A",NotScale WITH 'Y'
      ENDIF
      = lfUpdTotals("Maj",lcTrns)

      IF !SEEK('AA '+&lcTrns..WareCode)
        APPEND BLANK
        GATHER MEMVAR
        REPLACE cEndMaj WITH "A",cEndSort WITH "A",NotScale WITH "Y"
      ENDIF
      = lfUpdTotals("Sor",lcTrns)

      IF !SEEK('AAA')
        APPEND BLANK
        GATHER MEMVAR
        REPLACE cEndMaj WITH "A",cEndSort WITH "A",cEndRep WITH "A",NotScale WITH "Y"
        *! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [Start]
      ELSE
        REPLACE  WareCode  WITH  m.WareCode
        REPLACE  cStyMajor WITH m.cStyMajor

        *! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [End]
      ENDIF
      = lfUpdTotals("Rep",lcTrns)
      *------------------------------------------------------*
    ENDIF
    IF llRPPrnLoc
      IF SEEK(&lcStyDye..STYLE+SPACE(6)+&lcStyDye..cWareCode,lcWhsLoc)
        SELECT (lcWhsLoc)
        lcLoc = ''
        SCAN REST WHILE STYLE+COLOR+cWareCode = ;
            &lcStyDye..STYLE+SPACE(6)+&lcStyDye..cWareCode ;
            FOR !EMPTY(cLocation)
          llHasLoc = .T.

          lcLoc =  lcLoc + IIF(EMPTY(lcLoc),LANG_ICSTYREP_Bins+'                 ',SPACE(2)) + &lcWhsLoc..cLocation
        ENDSCAN
        IF llHasLoc
          SELECT (lcTrns)
          REPLACE HasLoc WITH llHasLoc,;
            Loc    WITH lcLoc
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF llAdded
    IF lcscale <> &lcscal..SCALE OR lcWareCode <> &lctrns..WareCode
      DO lpInsInTmp WITH '&lcStyDye',.T.
      lcscale = &lcscal..SCALE
      lcWareCode = &lctrns..WareCode
    ENDIF
  ENDIF

ENDSCAN
SELECT (lcTrns)
IF !EMPTY(oAriaApplication.WorkDir+lcTotTmp)
  APPEND FROM (oAriaApplication.WorkDir+lcTotTmp)
ENDIF

SELECT (lcStyle)
SET ORDER TO
SET RELATION TO

SELECT (lcStyDye)
SET ORDER TO
SET RELATION TO

DO lpWareRela
&& End of lfDatCollw()
*!*************************************************************
*! Name      : lpInsInTmp
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : Insert record in temp. file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpInsInTmp
*!*************************************************************

PROCEDURE lpInsInTmp
PARAMETERS lcFromFile,llScale


PRIVATE lcFileToUse,lcCostFld,lcFromFile

PRIVATE lnCurAlias


lnCurAlias = SELECT(0)

lcFileToUse = IIF(lcFromFile='&lcStyDye',(lcTrns),(lcStyTmp))

SELECT(lcFileToUse)
APPEND BLANK
REPLACE StyCode   WITH &lcStyle..STYLE,;
  StyDesc   WITH &lcStyle..Desc1,;
  cStyMajor WITH &lcStyle..cStyMajor,;
  STATUS    WITH &lcStyle..STATUS,;
  SCALE     WITH &lcStyle..SCALE,;
  Price     WITH &lcStyle..PriceA,;
  TOTCOST   WITH &lcStyle..TOTCOST,;
  Season    WITH &lcStyle..Season,;
  Division  WITH &lcStyle..cDivision,;
  FGroup    WITH &lcStyle..Fabric,;
  SGroup    WITH &lcStyle..cStyGroup,;
  Dept      WITH &lcStyle..Dept

*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
REPLACE COLOR WITH gfCodDes(SUBSTR(&lcStyle..STYLE,lnClrPo,lnColorLen),'COLOR     ')
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]

IF USED(lcFabric) AND RECCOUNT(lcFabric) > 0
  IF SEEK(&lcStyle..Fabric,lcFabric)
    REPLACE  MatType   WITH  &lcFabric..ITEM_TYPE
  ENDIF
ENDIF
IF lcFromFile = '&lcStyDye'
  REPLACE WareCode  WITH &lcStyDye..cWareCode,;
    Dyelot    WITH &lcStyDye..Dyelot,;
    WareDesc  WITH &lcStyDye..DESC,;
    HasDye    WITH llHasDye
ENDIF

REPLACE AVE_COST  WITH &lcFromFile..AVE_COST,;
  GL_LINK   WITH IIF(lcFromFile='&lcStyle',&lcStyle..Link_Code,&lcStyDye..GL_LINK)

IF llScale
  REPLACE NotScale WITH 'N'
ELSE
  REPLACE NotScale WITH 'Y'



  IF !EMPTY(lcSourFlds)
    *--in case of print dyelots for stock and no details for another transactions
    IF llRpPrnDye AND !llRPWhDeta AND lcRpSortBy <> 'W'&& Retrieve all quantities from style file
      IF lcFromFile='&lcStyle'
        SELECT (lcFromFile)
        SCATTER FIELDS &lcSourFlds. TO laAllVal
        SELECT (lcFileToUse)
        GATHER FIELDS &lcTargFlds. FROM laAllVal
      ELSE && Scan in StyDye File
        SELECT (lcFromFile)  &&Retrieve only stock values for dyelots from StyDye file
        lcStkMastFlds='STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK'
        SCATTER FIELDS &lcStkMastFlds. TO laAllVal
        SELECT (lcFileToUse)
        lcStkTempFlds='SOH1,SOH2,SOH3,SOH4,SOH5,SOH6,SOH7,SOH8,TOTSOH'
        GATHER FIELDS &lcStkTempFlds. FROM laAllVal
      ENDIF
    ELSE
      IF !llConfig &&the rest of dyelot cases
        *--if !llRpPrnDye AND  llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
        *--if  llRpPrnDye AND  llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
        *--if !llRpPrnDye AND !llRpWhsDeta,lcFromFile='&lcStyle'

        *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
        IF !(lcFromFile = '&lcStyle' AND llRpWhDeta)
          *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

          SELECT (lcFromFile)
          SCATTER FIELDS &lcSourFlds. TO laAllVal
          SELECT (lcFileToUse)
          GATHER FIELDS &lcTargFlds. FROM laAllVal

          *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
        ENDIF
        *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


      ELSE
        *--if !llRpConfig AND !llRPWhDeta,lcFromFile='&lcStyle'
        *--if !llRpConfig AND  llRpWhsDeta,lcFromFile='&lcStyDye' ,plan doesn't exist(NO Problem)
        *--if llRpConfig AND  llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
        *--if llRpConfig AND !llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
        *--Sorting by location also use this part of code.

        *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
        IF !(lcFromFile = '&lcStyle' AND llRpWhDeta)
          *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

          SELECT (lcFromFile)
          SCATTER FIELDS &lcSourFlds. TO laAllVal
          SELECT (lcFileToUse)
          GATHER FIELDS &lcTargFlds. FROM laAllVal

          *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
        ENDIF
        *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

      ENDIF
    ENDIF
  ENDIF


  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
  IF !(lcFromFile = '&lcStyle' AND llRpWhDeta)
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

    IF ",UALO," $ UPPER(lcAllTrns)
      = lfUAloCalc()
      SELECT (lcFileToUse)
      GATHER FIELDS &lcUALOFlds. FROM laUAloVal
    ENDIF
    IF ",IOTS," $ UPPER(lcAllTrns)
      STORE 0 TO laIOTSVal
      =lfOTSCalc('IOTS')
      SELECT (lcFileToUse)
      GATHER FIELDS &lcIOTSFlds. FROM laIOTSVal
    ENDIF
    IF ",OTS," $ UPPER(lcAllTrns)
      STORE 0 TO laOTSVal
      =lfOTSCalc('OTS')
      SELECT (lcFileToUse)
      GATHER FIELDS &lcOTSFlds. FROM laOTSVal
    ENDIF
    IF ",BOK," $ UPPER(lcAllTrns)
      =lfBokCalc()
      SELECT (lcFileToUse)
      GATHER FIELDS &lcBokFlds. FROM laBokVal
    ENDIF

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
  ENDIF
  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

  SELECT (lcFileToUse)

  IF llShowCost
    IF lcRpSortBy = 'W'
      lcCostFld = 'Ave_Cost'
    ELSE
      IF lcFromFile = '&lcStyDye'
        lcCostFld = 'Ave_Cost'
      ELSE
        *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
        *IF llLinkGlJl
        *  lcCostFld = 'Ave_Cost'
        *ELSE
        *B608407,1 WAM 01/14/2008 (End)

        IF lcCstMeth = "S"
          lcCostFld = 'TotCost'
        ELSE
          lcCostFld = 'Ave_Cost'
        ENDIF
        *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
        *ENDIF
        *B608407,1 WAM 01/14/2008 (End)
      ENDIF
    ENDIF

    laStkVal[01] = TotWip * &lcCostFld.

    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *IF llLinkGlJl
    *  laStkVal[02] = &lcFromFile..nStkVal
    *ELSE
    *B608407,1 WAM 01/14/2008 (End)

    laStkVal[02] = IIF(lcCstMeth = "S" , &lcStyle..TotCost * &lcStyle..ToTSTK , &lcFromFile..nStkVal)
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *ENDIF
    *B608407,1 WAM 01/14/2008 (End)

    laStkVal[03] = TotPLA  * &lcCostFld.
    laStkVal[04] = TotOTS  * &lcCostFld.
    laStkVal[05] = TotIOTS * &lcCostFld.
    laStkVal[06] = TotBOK  * &lcCostFld.
    laStkVal[07] = TotSHP  * &lcCostFld.
    laStkVal[08] = TotRet  * &lcCostFld.
    laStkVal[09] = TotRetA * &lcCostFld.
    laStkVal[10] = TotAlo  * &lcCostFld.
    laStkVal[11] = TotUAlo * &lcCostFld.
    laStkVal[12] = TotInt  * &lcCostFld.
    laStkVal[13] = TotWOrd * &lcCostFld.
    laStkVal[14] = TotOrd  * &lcCostFld.
    GATHER FIELDS &lcStkFlds. FROM laStkVal
  ENDIF
  IF llShowSale
    laSalVal[01] = TotWip  * Price
    laSalVal[02] = TotSOH  * Price
    laSalVal[03] = TotPLA  * Price
    laSalVal[04] = TotOTS  * Price
    laSalVal[05] = TotIOTS * Price
    laSalVal[06] = TotBOK  * Price
    laSalVal[07] = TotSHP  * Price
    laSalVal[08] = TotRet  * Price
    laSalVal[09] = TotRetA * Price
    laSalVal[10] = TotAlo  * Price
    laSalVal[11] = TotUAlo * Price
    laSalVal[12] = TotInt  * Price
    laSalVal[13] = TotWOrd * Price
    laSalVal[14] = TotOrd  * Price
    GATHER FIELDS &lcSalFlds. FROM laSalVal
  ENDIF
ENDIF
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfOTSCalc
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : to caculate OTS
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOTSCalc()
*!*************************************************************

FUNCTION lfOTSCalc
PARAMETER lcItm



PRIVATE lnI,lcArray,lcArrayEle,lcSz,lcArTotEle1,lcArTotEle2,lcArTotEle3

lcArray = "la" + lcItm + "Val"
lcArTotEle  = lcArray + "[9]"

STORE 0 TO &lcArray

FOR lnI = 1 TO 8
  lcSz = ALLTRIM(STR(lnI))

  lcArrayEle  = lcArray + "[" + lcSz + "]"

  IF !llRPSTKPL 
    &lcArrayEle = &lcFromFile..Stk&lcsz+;
      IIF(lcItm='IOTS',0,IIF(lcRPOTSB='W',;
      &lcFromFile..WIP&lcSz,;
      &lcStyle..Plan&lcSz))-;
      &lcFromFile..Ord&lcSz
  ELSE 
    &lcArrayEle = &lcFromFile..Stk&lcsz+;
      IIF(lcItm='IOTS',0,IIF(lcRPOTSB='W',;
      &lcFromFile..WIP&lcSz,;
      -1*&lcStyle..Plan&lcSz))-;
      &lcFromFile..Ord&lcSz
  ENDIF 



  DO CASE
  CASE lcRPOTSSig = 'P' AND &lcArrayEle <= 0
    &lcArrayEle = 0
  CASE lcRPOTSSig = 'N' AND &lcArrayEle >= 0
    &lcArrayEle = 0
  ENDCASE
  &lcArTotEle = &lcArTotEle + &lcArrayEle
ENDFOR
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*!*	IF (lcRPOTSSig ='P' AND &lcArTotEle >= lnRPOTSMin) OR ;
*!*	   (lcRPOTSSig ='N' AND &lcArTotEle <= lnRPOTSMin)
IF lcRPOTSSig ='A' OR ((INLIST(lcRPOTSSig ,'L','B','P') AND &lcArTotEle >= lnRPOTSMin) OR ;
    (INLIST(lcRPOTSSig ,'L','B','N') AND &lcArTotEle <= lnRPOTSMax))
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
  DO CASE
  CASE lcItm = 'OTS'
    REPLACE lPrnOTS  WITH .T.
  CASE lcItm = 'IOTS'
    REPLACE lPrnIOTS WITH .T.
  ENDCASE
ELSE
  &lcArTotEle = 0
ENDIF

*!*************************************************************
*! Name      : lfUAloCalc
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : to caculate unAllocated Value
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUAloCalc()
*!*************************************************************
FUNCTION lfUAloCalc

PRIVATE lcSz,lcArrayEle

STORE 0 TO laUAloVal

FOR lnI = 1 TO 8
  lcSz = ALLTRIM(STR(lnI))
  lcArrayEle  = "laUaloVal" + "[" + lcSz + "]"
  &lcArrayEle = &lcFromFile..STK&lcSz - &lcFromFile..ALO&lcSz
  laUAloVal[9] = laUAloVal[9] + &lcArrayEle
ENDFOR

*!*************************************************************
*! Name      : lfBokCalc
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : to caculate Booked Quantity
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBokCalc()
*!*************************************************************

FUNCTION lfBokCalc
PRIVATE lcSz,lcArrayEle

STORE 0 TO laBokVal

FOR lnI = 1 TO 8
  lcSz = ALLTRIM(STR(lnI))
  lcArrayEle  = "laBokVal" + "[" + lcSz + "]"
  &lcArrayEle = &lcFromFile..Shp&lcSz+&lcFromFile..Ord&lcSz
  laBokVal[9] = laBokVal[9] + &lcArrayEle
ENDFOR

*!*************************************************************
*! Name      : lfUpdTotals
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : to caculate & update totals
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUpdTotals()
*!*************************************************************

FUNCTION lfUpdTotals
PARAMETERS lcRecType,lcUseFile
PRIVATE lcRecType,lcUseFile


DO CASE
CASE lcRecType = "Maj"
  lcUsedArr = "laT"
  REPLACE cEndMaj WITH "A"
CASE lcRecType = "Sor"
  lcUsedArr = "laSorT"
  REPLACE cEndMaj  WITH "A" ,;
    cEndSort WITH "A"
CASE lcRecType = "Rep"
  lcUsedArr = "laRepT"
  REPLACE cEndMaj  WITH "A" ,;
    cEndSort WITH "A" ,;
    cEndRep  WITH "A"
ENDCASE
REPLACE WIP1      WITH WIP1+&lcUseFile..WIP1 ,;
  WIP2      WITH WIP2+&lcUseFile..WIP2 ,;
  WIP3      WITH WIP3+&lcUseFile..WIP3 ,;
  WIP4      WITH WIP4+&lcUseFile..WIP4 ,;
  WIP5      WITH WIP5+&lcUseFile..WIP5 ,;
  WIP6      WITH WIP6+&lcUseFile..WIP6 ,;
  WIP7      WITH WIP7+&lcUseFile..WIP7 ,;
  WIP8      WITH WIP8+&lcUseFile..WIP8 ,;
  TOTWIP    WITH TotWip+&lcUseFile..TotWIP ,;
  SOH1      WITH Soh1+&lcUseFile..SOH1 ,;
  SOH2      WITH Soh2+&lcUseFile..SOH2 ,;
  SOH3      WITH Soh3+&lcUseFile..SOH3 ,;
  SOH4      WITH Soh4+&lcUseFile..SOH4 ,;
  SOH5      WITH Soh5+&lcUseFile..SOH5 ,;
  SOH6      WITH Soh6+&lcUseFile..SOH6 ,;
  SOH7      WITH Soh7+&lcUseFile..SOH7 ,;
  SOH8      WITH Soh8+&lcUseFile..SOH8 ,;
  TOTSOH    WITH TotSoh+&lcUseFile..TotSOH
REPLACE PLA1      WITH Pla1+&lcUseFile..Pla1 ,;
  PLA2      WITH Pla2+&lcUseFile..Pla2 ,;
  PLA3      WITH Pla3+&lcUseFile..Pla3 ,;
  PLA4      WITH Pla4+&lcUseFile..Pla4 ,;
  PLA5      WITH Pla5+&lcUseFile..Pla5 ,;
  PLA6      WITH Pla6+&lcUseFile..Pla6 ,;
  PLA7      WITH Pla7+&lcUseFile..Pla7 ,;
  PLA8      WITH Pla8+&lcUseFile..Pla8 ,;
  TOTPLA    WITH TotPla+&lcUseFile..TotPla ,;
  UALO1     WITH UAlo1+&lcUseFile..UAlo1 ,;
  UALO2     WITH UAlo2+&lcUseFile..UAlo2 ,;
  UALO3     WITH UAlo3+&lcUseFile..UAlo3 ,;
  UALO4     WITH UAlo4+&lcUseFile..UAlo4 ,;
  UALO5     WITH UAlo5+&lcUseFile..UAlo5 ,;
  UALO6     WITH UAlo6+&lcUseFile..UAlo6 ,;
  UALO7     WITH UAlo7+&lcUseFile..UAlo7 ,;
  UALO8     WITH UAlo8+&lcUseFile..UAlo8 ,;
  TOTUALO   WITH TotUAlo+&lcUseFile..TotUAlo

REPLACE OTS1      WITH OTS1+&lcUseFile..OTS1 ,;
  OTS2      WITH OTS2+&lcUseFile..OTS2 ,;
  OTS3      WITH OTS3+&lcUseFile..OTS3 ,;
  OTS4      WITH OTS4+&lcUseFile..OTS4 ,;
  OTS5      WITH OTS5+&lcUseFile..OTS5 ,;
  OTS6      WITH OTS6+&lcUseFile..OTS6 ,;
  OTS7      WITH OTS7+&lcUseFile..OTS7 ,;
  OTS8      WITH OTS8+&lcUseFile..OTS8 ,;
  TOTOTS    WITH TotOTS+&lcUseFile..TotOTS ,;
  IOTS1     WITH IOTS1+&lcUseFile..IOTS1 ,;
  IOTS2     WITH IOTS2+&lcUseFile..IOTS2 ,;
  IOTS3     WITH IOTS3+&lcUseFile..IOTS3 ,;
  IOTS4     WITH IOTS4+&lcUseFile..IOTS4 ,;
  IOTS5     WITH IOTS5+&lcUseFile..IOTS5 ,;
  IOTS6     WITH IOTS6+&lcUseFile..IOTS6 ,;
  IOTS7     WITH IOTS7+&lcUseFile..IOTS7 ,;
  IOTS8     WITH IOTS8+&lcUseFile..IOTS8 ,;
  TOTIOTS   WITH TotIOTS+&lcUseFile..TotIOTS

REPLACE ORD1      WITH Ord1+&lcUseFile..Ord1 ,;
  ORD2      WITH Ord2+&lcUseFile..Ord2 ,;
  ORD3      WITH Ord3+&lcUseFile..Ord3 ,;
  ORD4      WITH Ord4+&lcUseFile..Ord4 ,;
  ORD5      WITH Ord5+&lcUseFile..Ord5 ,;
  ORD6      WITH Ord6+&lcUseFile..Ord6 ,;
  ORD7      WITH Ord7+&lcUseFile..Ord7 ,;
  ORD8      WITH Ord8+&lcUseFile..Ord8 ,;
  TOTORD    WITH TotOrd+&lcUseFile..TotOrd ,;
  WORD1     WITH WOrd1+&lcUseFile..WOrd1 ,;
  WORD2     WITH WOrd2+&lcUseFile..WOrd2 ,;
  WORD3     WITH WOrd3+&lcUseFile..WOrd3 ,;
  WORD4     WITH WOrd4+&lcUseFile..WOrd4 ,;
  WORD5     WITH WOrd5+&lcUseFile..WOrd5 ,;
  WORD6     WITH WOrd6+&lcUseFile..WOrd6 ,;
  WORD7     WITH WOrd7+&lcUseFile..WOrd7 ,;
  WORD8     WITH WOrd8+&lcUseFile..WOrd8 ,;
  TOTWORD   WITH TotWOrd+&lcUseFile..TotWOrd

REPLACE INT1      WITH Int1+&lcUseFile..Int1 ,;
  INT2      WITH Int2+&lcUseFile..Int2 ,;
  INT3      WITH Int3+&lcUseFile..Int3 ,;
  INT4      WITH Int4+&lcUseFile..Int4 ,;
  INT5      WITH Int5+&lcUseFile..Int5 ,;
  INT6      WITH Int6+&lcUseFile..Int6 ,;
  INT7      WITH Int7+&lcUseFile..Int7 ,;
  INT8      WITH Int8+&lcUseFile..Int8 ,;
  TOTINT    WITH TotInt+&lcUseFile..TotInt ,;
  BOK1      WITH Bok1+&lcUseFile..Bok1 ,;
  BOK2      WITH Bok2+&lcUseFile..Bok2 ,;
  BOK3      WITH Bok3+&lcUseFile..Bok3 ,;
  BOK4      WITH Bok4+&lcUseFile..Bok4 ,;
  BOK5      WITH Bok5+&lcUseFile..Bok5 ,;
  BOK6      WITH Bok6+&lcUseFile..Bok6 ,;
  BOK7      WITH Bok7+&lcUseFile..Bok7 ,;
  BOK8      WITH Bok8+&lcUseFile..Bok8 ,;
  TOTBOK    WITH TotBok+&lcUseFile..TotBok

REPLACE SHP1      WITH Shp1+&lcUseFile..Shp1 ,;
  SHP2      WITH Shp2+&lcUseFile..Shp2 ,;
  SHP3      WITH Shp3+&lcUseFile..Shp3 ,;
  SHP4      WITH Shp4+&lcUseFile..Shp4 ,;
  SHP5      WITH Shp5+&lcUseFile..Shp5 ,;
  SHP6      WITH Shp6+&lcUseFile..Shp6 ,;
  SHP7      WITH Shp7+&lcUseFile..Shp7 ,;
  SHP8      WITH Shp8+&lcUseFile..Shp8 ,;
  TOTSHP    WITH TotShp+&lcUseFile..TotShp ,;
  ALO1      WITH Alo1+&lcUseFile..Alo1 ,;
  ALO2      WITH Alo2+&lcUseFile..Alo2 ,;
  ALO3      WITH Alo3+&lcUseFile..Alo3 ,;
  ALO4      WITH Alo4+&lcUseFile..Alo4 ,;
  ALO5      WITH Alo5+&lcUseFile..Alo5 ,;
  ALO6      WITH Alo6+&lcUseFile..Alo6 ,;
  ALO7      WITH Alo7+&lcUseFile..Alo7 ,;
  ALO8      WITH Alo8+&lcUseFile..Alo8 ,;
  TOTALO    WITH TotAlo+&lcUseFile..TotAlo

REPLACE RET1      WITH Ret1+&lcUseFile..Ret1 ,;
  RET2      WITH Ret2+&lcUseFile..Ret2 ,;
  RET3      WITH Ret3+&lcUseFile..Ret3 ,;
  RET4      WITH Ret4+&lcUseFile..Ret4 ,;
  RET5      WITH Ret5+&lcUseFile..Ret5 ,;
  RET6      WITH Ret6+&lcUseFile..Ret6 ,;
  RET7      WITH Ret7+&lcUseFile..Ret7 ,;
  RET8      WITH Ret8+&lcUseFile..Ret8 ,;
  TOTRET    WITH TotRet+&lcUseFile..TotRet ,;
  RETA1     WITH RetA1+&lcUseFile..RetA1 ,;
  RETA2     WITH RetA2+&lcUseFile..RetA2 ,;
  RETA3     WITH RetA3+&lcUseFile..RetA3 ,;
  RETA4     WITH RetA4+&lcUseFile..RetA4 ,;
  RETA5     WITH RetA5+&lcUseFile..RetA5 ,;
  RETA6     WITH RetA6+&lcUseFile..RetA6 ,;
  RETA7     WITH RetA7+&lcUseFile..RetA7 ,;
  RETA8     WITH RetA8+&lcUseFile..RetA8 ,;
  TOTRETA   WITH TotRetA+&lcUseFile..TotRetA

REPLACE nStkVWIP    WITH nStkVWIP +&lcUseFile..nStkVWIP ,;
  nStkVSOH    WITH nStkVSoh +&lcUseFile..nStkVSOH ,;
  nStkVPLA    WITH nStkVPla +&lcUseFile..nStkVPla ,;
  nStkVUALO   WITH nStkVUAlo+&lcUseFile..nStkVUAlo,;
  nStkVOTS    WITH nStkVOTS +&lcUseFile..nStkVOTS ,;
  nStkVIOTS   WITH nStkVIOTS+&lcUseFile..nStkVIOTS,;
  nStkVORD    WITH nStkVOrd +&lcUseFile..nStkVOrd ,;
  nStkVWORD   WITH nStkVWOrd+&lcUseFile..nStkVWOrd,;
  nStkVINT    WITH nStkVInt +&lcUseFile..nStkVInt ,;
  nStkVBOK    WITH nStkVBok +&lcUseFile..nStkVBok ,;
  nStkVSHP    WITH nStkVShp +&lcUseFile..nStkVShp ,;
  nStkVALO    WITH nStkVAlo +&lcUseFile..nStkVAlo ,;
  nStkVRET    WITH nStkVRet +&lcUseFile..nStkVRet ,;
  nStkVRETA   WITH nStkVRetA+&lcUseFile..nStkVRetA

REPLACE nSalVWIP    WITH nSalVWIP +&lcUseFile..nSalVWIP ,;
  nSalVSOH    WITH nSalVSoh +&lcUseFile..nSalVSOH ,;
  nSalVPLA    WITH nSalVPla +&lcUseFile..nSalVPla ,;
  nSalVUALO   WITH nSalVUAlo+&lcUseFile..nSalVUAlo,;
  nSalVOTS    WITH nSalVOTS +&lcUseFile..nSalVOTS ,;
  nSalVIOTS   WITH nSalVIOTS+&lcUseFile..nSalVIOTS,;
  nSalVORD    WITH nSalVOrd +&lcUseFile..nSalVOrd ,;
  nSalVWORD   WITH nSalVWOrd+&lcUseFile..nSalVWOrd,;
  nSalVINT    WITH nSalVInt +&lcUseFile..nSalVInt ,;
  nSalVBOK    WITH nSalVBok +&lcUseFile..nSalVBok ,;
  nSalVSHP    WITH nSalVShp +&lcUseFile..nSalVShp ,;
  nSalVALO    WITH nSalVAlo +&lcUseFile..nSalVAlo ,;
  nSalVRET    WITH nSalVRet +&lcUseFile..nSalVRet ,;
  nSalVRETA   WITH nSalVRetA+&lcUseFile..nSalVRetA


*!*************************************************************
*! Name      : lpWareRela
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/03/2005
*! Purpose   : Create relation between the dummy file and data
*!             temp. file when it is sorted by location
*!*************************************************************
*! Called from : lfDatCollW()
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpWareRela
*!*************************************************************

PROCEDURE lpWareRela

SELECT(lcDummy)
SET ORDER TO (lcDummy)
SELECT(lcTrns)
SET ORDER TO WARESORT
SET RELATION TO IIF(NotScale='Y','',SPACE(10)) INTO (lcDummy) ADDITIVE
SELECT (lcStyDye)
SET ORDER TO lcStyDyeW ASCENDING
SELECT (lcDummy)
IF !llRpConfig
  SET RELATION TO IIF(&lcDummy..LABEL=LANG_ICSTYREP_Dyelot AND  &lcTrns..NotScale='Y',;
    &lcTrns..WareCode+&lcTrns..StyCode ,;
    SPACE(25)) INTO &lcStyDye ADDITIVE
ELSE
  SET RELATION TO IIF(&lcTrns..NotScale='Y',;
    &lcTrns..WareCode+&lcTrns..StyCode ,;
    SPACE(25)) INTO &lcStyDye ADDITIVE
ENDIF
SELECT(lcTrns)

*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [Start]
*B610311,1 TMI 05/05/2013 [Start] set the order in the correct alias
*SET ORDER TO WareSort2
SET ORDER TO WareSort2 IN (lcStyTmp)
*B610311,1 TMI 05/05/2013 [End  ] 
*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [End]

SET SKIP TO (lcDummy),&lcStyDye
SET RELATION TO 'S' + SCALE INTO &lcscal ADDITIVE
SET RELATION TO  WareCode INTO &lcWareHous ADDITIVE
SET ORDER TO TAG lcStyle IN &lcStyle
SET RELATION TO  StyCode INTO &lcStyle ADDITIVE


*!*************************************************************
*! Name      : lfDatCollS
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/06/2005
*! Purpose   : Collecting data if it is sorted by any other sort
*!             type rather than sort by location
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDatCollS()
*!*************************************************************

FUNCTION lfDatCollS


PRIVATE llHasDye,lcWare,lnI,lcPrvMaj,lcScale

DO CASE
CASE lcRpSortBy == 'S'
  lcSortKey = "''"
CASE lcRpSortBy == 'SE'
  lcSortKey = lcStyTmp+'.Season'
CASE lcRpSortBy == 'D'
  lcSortKey = lcStyTmp+'.Division'
CASE lcRpSortBy == 'FG'
  lcSortKey = lcStyTmp+'.FGroup'
CASE lcRpSortBy == 'SG'
  lcSortKey = lcStyTmp+'.SGroup'
CASE lcRpSortBy == 'MT'
  lcSortKey = lcStyTmp+'.MatType'
CASE lcRpSortBy == 'DE'
  lcSortKey = lcStyTmp+'.Dept'
ENDCASE


llHasDye = .F.

lcStyleWare = SPACE(25)

SELECT (lcStyDye)
SET ORDER TO TAG lcStyDye DESCENDING

SELECT (lcStyle)
SET ORDER TO TAG lcStyle

IF ATC("INTO &lcscal",SET("Relation")) > 0
  SET RELATION OFF INTO &lcscal
ENDIF
SET RELATION TO 'S' + SCALE INTO &lcscal ADDITIVE

IF ATC("INTO &lcStyDye",SET("Relation")) > 0
  SET RELATION OFF INTO &lcStyDye
ENDIF
SET RELATION TO STYLE INTO &lcStyDye ADDITIVE


lcPrvMaj = SPACE(19)

lnLocNo = 0
SELECT (lcStyle)
lcscale = SPACE(3)

SCAN
  llAdded = .T.



  DO lpInsInTmp WITH '&lcStyle'

  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
  *IF !llRPShwZer AND ;
  (&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS +;
  &lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet +;
  &lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt +;
  &lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
  IF !llRPWhDeta AND  !llRPShwZer AND ;
      (&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS +;
      &lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet +;
      &lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt +;
      &lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

    SELECT (lcStyTmp)
    DELETE
    llAdded = .F.
  ELSE

    *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
    *IF llOnlyOTS AND !&lcStyTmp..lPrnOTS AND !&lcStyTmp..lPrnIOTS
    IF !llRPWhDeta AND llOnlyOTS AND !&lcStyTmp..lPrnOTS AND !&lcStyTmp..lPrnIOTS
      *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

      SELECT (lcStyTmp)
      DELETE
      llAdded = .F.
    ELSE

      *------------------------------------------------------*
      SELECT (lcStyTmp)
      SCATTER FIELDS cStyMajor,StyCode,WareCode,Dyelot,Season,Division,FGroup,SGroup,MatType,Dept,SCALE,Price,TotCost,Ave_Cost MEMVAR
      SELECT (lcTotTmp)
      IF !SEEK('A  '+&lcSortKey+&lcStyTmp..cStyMajor)
        APPEND BLANK
      ENDIF
      GATHER MEMVAR
      REPLACE cEndMaj WITH "A",NotScale WITH 'Y'

      = lfUpdTotals("Maj",lcStyTmp)

      IF !(lcRpSortBy == 'S')
        IF !SEEK('AA '+&lcSortKey)
          APPEND BLANK
        ENDIF
        GATHER MEMVAR
        REPLACE cEndMaj WITH "A",cEndSort WITH "A",NotScale WITH 'Y'

        = lfUpdTotals("Sor",lcStyTmp)
      ENDIF

      IF !SEEK('AAA')
        APPEND BLANK
      ENDIF
      GATHER MEMVAR
      REPLACE cEndMaj WITH "A",cEndSort WITH "A",cEndRep WITH "A",NotScale WITH 'Y'

      = lfUpdTotals("Rep",lcStyTmp)

      *------------------------------------------------------*
      *--Collect data From StyDye for Location Details ,or dyelots for Stock,or print configuration=Yes
      IF llConfig
        llSetUp= llRpConfig
      ELSE
        llSetUp= llRpPrnDye
      ENDIF
      IF llRPWhDeta OR llSetUp
        lnselAlias=SELECT()
        SELECT (lcStyDye)
        SCAN WHILE STYLE = &lcStyle..STYLE
          llNew       = lcStyleWare # &lcStyDye..STYLE+&lcStyDye..cWareCode
          lcStyleWare = &lcStyDye..STYLE+&lcStyDye..cWareCode
          lnLocNo     = IIF(llNew, 0, lnLocNo)
          IF llSetUp AND llNew
            llHasDye = !EMPTY(Dyelot)
          ENDIF
          IF !EMPTY(Dyelot)
            LOOP
          ENDIF


          DO lpInsInTmp WITH '&lcStyDye'




          *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
          IF llRPWhDeta AND llOnlyOTS AND !&lcTrns..lPrnOTS AND !&lcTrns..lPrnIOTS
            SELECT (lcTrns)
            LOOP
          ENDIF

          IF llRPWhDeta AND  !llRPShwZer AND ;
              (&lcTrns..TotWip  + &lcTrns..TotSOH + &lcTrns..TotPLA  + &lcTrns..TotOTS +;
              &lcTrns..TotIOTS + &lcTrns..TotBOK + &lcTrns..TotSHP  + &lcTrns..TotRet +;
              &lcTrns..TotRetA + &lcTrns..TotAlo + &lcTrns..TotUAlo + &lcTrns..TotInt +;
              &lcTrns..TotWOrd + &lcTrns..TotOrd) = 0
            SELECT (lcTrns)
            DELETE
            LOOP
          ENDIF

          SELECT(lcStyTmp)
          LOCATE
          LOCATE FOR StyCode = &lcStyle..STYLE
          IF FOUND()

            = lfUpdTotals("",lcTrns)
            IF ",IOTS," $ UPPER(lcAllTrns)
              =lfOTSCalcW('IOTS')
            ENDIF
            IF ",OTS," $ UPPER(lcAllTrns)
              =lfOTSCalcW('OTS')
            ENDIF
          ENDIF


          SELECT (lcTotTmp)
          IF SEEK('A  '+&lcSortKey+&lcStyTmp..cStyMajor)
            = lfUpdTotals("Maj",lcTrns)
          ENDIF

          IF !(lcRpSortBy == 'S')
            IF SEEK('AA '+&lcSortKey)
              = lfUpdTotals("Sor",lcTrns)
            ENDIF
          ENDIF


          IF SEEK('AAA')
            = lfUpdTotals("Rep",lcTrns)
          ENDIF
          *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


          IF llOnlyOTS AND !&lcTrns..lPrnOTS AND !&lcTrns..lPrnIOTS
            SELECT (lcStyTmp)
          ELSE
            IF llRPWhDeta AND llRPPrnLoc
              IF SEEK(&lcStyDye..STYLE+SPACE(6)+&lcStyDye..cWareCode,lcWhsLoc)
                SELECT (lcWhsLoc)
                lcLoc = ''
                SCAN REST WHILE STYLE+COLOR+cWareCode = ;
                    &lcStyDye..STYLE+SPACE(6)+&lcStyDye..cWareCode ;
                    FOR !EMPTY(cLocation)
                  llHasLoc = .T.

                  lcLoc =  lcLoc + IIF(EMPTY(lcLoc),LANG_ICSTYREP_Bins+'                 ',SPACE(2)) + &lcWhsLoc..cLocation
                ENDSCAN
                IF llHasLoc
                  SELECT (lcTrns)
                  REPLACE HasLoc WITH llHasLoc,;
                    Loc    WITH lcLoc
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
        *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
        IF llRPWhDeta AND  !llRPShwZer AND ;
            (&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS +;
            &lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet +;
            &lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt +;
            &lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
          SELECT (lcStyTmp)
          DELETE
          llAdded = .F.
          SELECT(lcTotTmp)
          DELETE FOR StyCode = &lcStyle..STYLE AND  IIF(cEndSort = "A",(&lcTotTmp..TotWip  + &lcTotTmp..TotSOH + &lcTotTmp..TotPLA  + &lcStyTmp..TotOTS +;
            &lcTotTmp..TotIOTS + &lcTotTmp..TotBOK + &lcTotTmp..TotSHP  + &lcTotTmp..TotRet +;
            &lcTotTmp..TotRetA + &lcTotTmp..TotAlo + &lcTotTmp..TotUAlo + &lcTotTmp..TotInt +;
            &lcTotTmp..TotWOrd + &lcTotTmp..TotOrd) = 0   ,.T.)
        ENDIF
        *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

        SELECT(lnSelAlias)
      ENDIF
    ENDIF
  ENDIF




  IF llAdded

    IF lcscale <> &lcscal..SCALE
      DO lpInsInTmp WITH '&lcStyle',.T.
      lcscale = &lcscal..SCALE
    ENDIF
  ENDIF

ENDSCAN


SELECT (lcStyTmp)
IF !EMPTY(oAriaApplication.WorkDir+lcTotTmp)
  APPEND FROM (oAriaApplication.WorkDir+lcTotTmp)
ENDIF



SELECT (lcStyle)
SET ORDER TO
SET RELATION TO

SELECT (lcStyDye)
SET ORDER TO
SET RELATION TO

DO lpStyleRela



*!*************************************************************
*! Name      : lpStyleRela
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/06/2005
*! Purpose   : Create relation between the dummy file and data
*!             temp. file when it is sorted by any other way rather than
*!             location
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpStyleRela
*!*************************************************************

PROCEDURE lpStyleRela

SET ORDER TO StySort IN (lcTrns)
SELECT(lcStyTmp)
DO CASE
CASE lcRpSortBy == 'S'
  lcSortTtl  = LANG_ICSTYREP_Style
  lcGroupExp = "''"
  lcSortExp  = "''"
  lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(LANG_ICSTYREP_Description),LEN(ALLTRIM(lcRepNMTtl)))-LEN(lcMajTtl),0))]
  lcNMajExp  = [ALLTRIM(lcRepNMTtl) + SPACE(1) + PADL(": ",2+MAX(MAX(LEN(LANG_ICSTYREP_Description),LEN(lcMajTtl))-LEN(ALLTRIM(lcRepNMTtl)),0))]
  lcDescExp  = [LANG_ICSTYREP_Description + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(lcMajTtl),LEN(ALLTRIM(lcRepNMTtl)))-LEN(LANG_ICSTYREP_Description),0))]
CASE lcRpSortBy == 'SE'
  lcSortFld  = '.Season'
  lcSortTtl  = LANG_ICSTYREP_Season
  lcGroupExp = lcStyTmp+'.Season'
  lcSortExp  = [LANG_ICSTYREP_Season+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Season),0))]
  lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Season)-LEN(lcMajTtl),0))]
  lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
  lcDescExp  = [LANG_ICSTYREP_Description + SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
CASE lcRpSortBy == 'D'
  lcSortFld  = '.Division'
  lcSortTtl  = LANG_ICSTYREP_Division
  lcGroupExp = lcStyTmp+'.Division'
  lcSortExp  = [LANG_ICSTYREP_Division+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Division),0))]
  lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Division)-LEN(lcMajTtl),0))]
  lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
  lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
CASE lcRpSortBy == 'FG'
  lcSortFld  = '.FGroup'
  lcSortTtl  = LANG_ICSTYREP_Primary_Fabric
  lcGroupExp = lcStyTmp+'.FGroup'
  lcSortExp  = [LANG_ICSTYREP_Primary_Fabric+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Primary_Fabric),0))]
  lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Primary_Fabric)-LEN(lcMajTtl),0))]
  lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
  lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
CASE lcRpSortBy == 'SG'
  lcSortFld  = '.SGroup'
  lcSortTtl  = LANG_ICSTYREP_Style_Group
  lcGroupExp = lcStyTmp+'.SGroup'
  lcSortExp  = [LANG_ICSTYREP_Style_Group+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Style_Group),0))]
  lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Style_Group)-LEN(lcMajTtl),0))]
  lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
  lcDescExp  = [LANG_ICSTYREP_Description + SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
CASE lcRpSortBy == 'MT'
  lcSortFld  = '.MatType'
  lcSortTtl  = LANG_ICSTYREP_Material_type
  lcGroupExp = lcStyTmp+'.MatType'
  lcSortExp  = [LANG_ICSTYREP_Material_type + SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Material_type),0))]
  lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Material_type)-LEN(lcMajTtl),0))]
  lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
  lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
CASE lcRpSortBy == 'DE'
  lcSortFld  = '.Dept'
  lcSortTtl  = LANG_ICSTYREP_Department
  lcGroupExp = lcStyTmp+'.Dept'
  lcSortExp  = [LANG_ICSTYREP_Department+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Department),0))]
  lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Department)-LEN(lcMajTtl),0))]
  lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
  lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
ENDCASE

SET ORDER TO (lcDummy) IN (lcDummy)
SET ORDER TO TAG lcStyDye ASCENDING IN &lcStyDye

SELECT(lcStyTmp)

SET RELATION TO IIF(NotScale='Y','',SPACE(10)) INTO (lcDummy) ADDITIVE
SELECT (lcDummy)
SET RELATION TO IIF(&lcStyTmp..NotScale='Y',&lcStyTmp..StyCode,SPACE(10)) INTO (lcTrns) ADDITIVE
SELECT (lcTrns)
SET RELATION TO IIF(&lcStyTmp..NotScale='Y',StyCode+WareCode,SPACE(10)) INTO &lcStyDye ADDITIVE
SELECT (lcStyTmp)
SET SKIP TO (lcDummy),(lcTrns),&lcStyDye ADDITIVE
SET RELATION TO 'S' + SCALE INTO &lcscal ADDITIVE
SET ORDER TO TAG lcStyle IN &lcStyle
SET RELATION TO StyCode INTO &lcStyle ADDITIVE

*!*************************************************************
*! Name      : lfCostSalPr
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/06/2005
*! Purpose   : To construct the string that hold the cost & price
*!             label according to print one or both of them
*!*************************************************************
*! Called from : FRX Files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCostSalPr()
*!*************************************************************

FUNCTION lfCostSalPr


lcReturn = ''

IF llShowCost
  *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
  *IF llLinkGlJl
  *  lcReturn = SPACE(15) + LANG_ICSTYREP_Average_cost +' : '
  *ELSE
  *B608407,1 WAM 01/14/2008 (End)
  IF lcCstMeth = "S"
    lcReturn = SPACE(15) + LANG_ICSTYREP_Unit_cost + ' :    '
  ELSE
    lcReturn = SPACE(15) + LANG_ICSTYREP_Average_cost +' : '
  ENDIF
  *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
  *ENDIF
  *B608407,1 WAM 01/14/2008 (End)


  IF lcRpSortBy == 'W'

    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *IF llLinkGlJl
    *  lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..Ave_Cost,10,2))
    *ELSE
    *B608407,1 WAM 01/14/2008 (End)

    IF lcCstMeth = "S"
      lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..TotCost,10,2))
    ELSE
      lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..Ave_Cost,10,2))
    ENDIF
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *ENDIF
    *B608407,1 WAM 01/14/2008 (End)

  ELSE
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *IF llLinkGlJl
    *  lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".Ave_Cost"),10,2))
    *ELSE
    *B608407,1 WAM 01/14/2008 (End)

    IF lcCstMeth = "S"
      lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".TotCost"),10,2))
    ELSE
      lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".Ave_Cost"),10,2))
    ENDIF
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *ENDIF
    *B608407,1 WAM 01/14/2008 (End)

  ENDIF

  IF llShowSale
    IF lcRpSortBy == 'W'
      lcReturn = lcReturn + SPACE(15)+LANG_ICSTYREP_Unit_Price+'  : ' + ALLTRIM(STR(EVAL(lcTrns+".Price"),12,2))
    ELSE
      lcReturn = lcReturn + SPACE(15)+LANG_ICSTYREP_Unit_Price+'  : ' + ALLTRIM(STR(EVAL(lcStyTmp+".Price"),12,2))
    ENDIF
  ENDIF
ELSE
  IF llShowSale
    IF lcRpSortBy == 'W'
      lcReturn = SPACE(15) + LANG_ICSTYREP_Unit_Price+' : ' + ALLTRIM(STR(EVAL(lcTrns+".Price"),12,2))
    ELSE
      lcReturn = SPACE(15) + LANG_ICSTYREP_Unit_Price+' : ' + ALLTRIM(STR(EVAL(lcStyTmp+".Price"),12,2))
    ENDIF
  ENDIF
ENDIF

RETURN lcReturn

*!*************************************************************
*! Name      : lfDyltItm
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to print dyelot line values
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDyltItm()
*!*************************************************************

FUNCTION lfDyltItm
PARAMETERS lcFld

PRIVATE lcFld,lnRetVal

lnRetVal = 0

DO CASE
CASE lcFld = 'Tot'
  lnRetVal = &lcStyDye..TotStk
CASE lcFld = 'nStkVal'
  lnRetVal = &lcStyDye..nStkVal
CASE lcFld = 'Price'
  lnRetVal = (&lcStyDye..TotStk )* (&lcStyle..PriceA)
OTHERWISE
  lnRetVal = EVAL(lcStyDye+'.Stk'+lcFld)
ENDCASE

RETURN lnRetVal

*!*************************************************************
*! Name      : lfGetCosts
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : If print dyelots for stock get cost from location record.
*!*************************************************************
*! Called from : lfStyHeadr()
*!*************************************************************
*! Passed Parameters : Costing Field
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetCosts()
*!*************************************************************
FUNCTION lfGetCosts
PARAMETERS lnThisCost
PRIVATE lnCurrAlis , lcSeekExpr
lnCurrAlis = SELECT(0)
SELECT REVSTYDY
lcSeekExpr = IIF(lcRpSortBy== "S",&lcStyDye..STYLE+&lcStyDye..CWARECODE,;
  &lcStyDye..CWARECODE+&lcStyDye..STYLE)
IF SEEK(lcSeekExpr)
  lnThisCost = Ave_Cost
ELSE
  lnThisCost = 0
ENDIF
SELECT (lnCurrAlis)
*-- end of lfGetCosts.

*!*************************************************************
*! Name      : lfInitVals
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Initially Zero all line values.
*!*************************************************************
*! Called from : lfStyHeadr()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInitVals()
*!*************************************************************
FUNCTION lfInitVals

*: B608711,1 MMT 10/08/2008 Fix bug of Wrong Positive OTS when use print BOTH OTS [Start]
*STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,;
lnSize5,lnSize6,lnSize7,lnSize8,lnSize9,lcSize10,lnSize11,;
lnNonPrice,lnNonCost
STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,;
  lnSize5,lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11,;
  lnNonPrice,lnNonCost
*: B608711,1 MMT 10/08/2008 Fix bug of Wrong Positive OTS when use print BOTH OTS [End]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
STORE .F. TO llChkAllSz
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

IF lcMastFile = "&lcStyDye" AND llPrintClr
  STORE 0 TO lnClrSz1,lnClrSz2,lnClrSz3,lnClrSz4,;
    lnClrSz5,lnClrSz6,lnClrSz7,lnClrSz8,lnClrSz9,lnClrSz10,lnClrSz11,;
    lnClrPrice,lnClrCost
ENDIF
*-- end of lfInitVals.

*!*************************************************************
*! Name      : lfPrintHdr
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Evaluate all report variables and print header.
*!*************************************************************
*! Called from : FRX Files
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Report Header
*!*************************************************************
*! Example     : =lfPrintHdr()
*!*************************************************************
FUNCTION lfPrintHdr
PRIVATE lcPrintHdr

lcOldAlias = SELECT(0)



IF lcRpSortBy == "S"
  lcPrintHdr = lfStyHeadr()  && Sort by style header.
ELSE
  lcPrintHdr = lfLocHeadr()  && Sort by location header.
ENDIF
SELECT(lcOldAlias)

RETURN lcPrintHdr
*-- end of lfPrintHdr.

*!*************************************************************
*! Name      : lfStyHeadr
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Evaluate By Style all report variables and print header.
*!*************************************************************
*! Called from : lfPrintHdr()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Report Header
*!*************************************************************
*! Example     : =lfStyHeadr()
*!*************************************************************
FUNCTION lfStyHeadr
PRIVATE lcPrintHdr , lcSize , lnAllSizes

STORE '' TO lcNonMjDes , lcPrintHdr , lcLocBins
lnAllSizes = 0
=lfInitVals()  && Assign initial values to all line variables.
*-- Evaluate all sizes values. [Begin]
FOR lnAllSizes = 1 TO 8
  lcSize  = "lnSize"+STR(lnAllSizes,1)
  &lcSize = EVALUATE(laTranAray[lnAllSizes])
  lnSize9 = lnSize9 + &lcSize

  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
  IF !llRpShwZer AND &lcSize <> 0
    llChkAllSz = .T.
  ENDIF
  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


  IF lcRpOTSSig $ 'BL'
    IF &lcSize > 0
      lnSize10 = lnSize10+ &lcSize && variable hold sum of +ve OTS Qtys
    ELSE
      lnSize11 = lnSize11+ &lcSize && variable hold sum of -ve OTS Qtys
    ENDIF
  ENDIF

ENDFOR


*-- Evaluate all sizes values. [End  ]

*-- Zero values if OTS or IOTS and condition is true. [Begin]
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMin)))
IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMax)) OR (lcRpOTSSig $ 'LB' AND (lnSize9 > lnRpOTSMax AND lnSize9 < lnRpOTSMin)))
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]

  STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
    lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11

ENDIF
*-- Zero values if OTS or IOTS and condition is true. [End  ]



*-- if user want to print zeros or total value not equal zero.
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*IF llRpShwZer OR lnSize9 <> 0
IF llRpShwZer OR lnSize9 <> 0 OR (!llRpShwZer AND llChkAllSz)
  *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
  lnNonPrice = lnSize9 * (&lcStyle..PriceA)
  IF lcMastFile = "&lcStyle" OR EMPTY(DYELOT)

    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *IF llLinkGlJl
    *  lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="S", &lcStyle..TotCost,Ave_Cost)*lnSize9)
    *ELSE
    *B608407,1 WAM 01/14/2008 (End)

    lnNonCost  = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth="S" , (&lcStyle..TotCost) * lnSize9 ,nStkVal) , IIF(lcCstMeth="S", &lcStyle..TotCost,Ave_Cost)*lnSize9)
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *ENDIF
    *B608407,1 WAM 01/14/2008 (End)


    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *IF llLinkGlJl
    *  lnCstAveVl = IIF("SOH" $ lcRepTarVl , nStkVal/lnSize9 , IIF(lcCstMeth="S" , ;
    &lcStyle..TotCost,Ave_Cost))
    *ELSE
    lnCstAveVl = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth="S" , &lcStyle..TotCost ,nStkVal/lnSize9 ) , IIF(lcCstMeth="S" , ;
      &lcStyle..TotCost,Ave_Cost))
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *ENDIF
    *B608407,1 WAM 01/14/2008 (End)

  ELSE
    IF !llRpWhDeta
      IF lcCstMeth="A"
        = lfGetCosts(@lnCstAveVl)
      ELSE
        lnCstAveVl = &lcStyle..TotCost
      ENDIF
    ENDIF
    lnNonCost = lnSize9 * lnCstAveVl
  ENDIF

  =lfNonMjDes()  && Evaluate Non Major Description.

  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
  *  IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
  IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
    lnFrstRec1 = RECNO()
    lcPrntSty = SPACE(lnMajorLen)
    lcScalCode = SPACE(3)
  ENDIF
  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
  *  ENDIF
  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

  *-- if it is new style group.
  IF !(PADR(&lcStyle..STYLE,lnMajorLen)==lcPrntSty)
    * B607997,1 T20070119.0022 AYM 03/06/2007 Problem not printing first col_line if the prev. style ends with same color [Begin]
    lcPrntNon = SPACE(19 - lnMajorLen)
    * B607997,1 T20070119.0022 AYM 03/06/2007 Problem not printing first col_line if the prev. style ends with same color [End]
    lcPrntSty = PADR(&lcStyle..STYLE,lnMajorLen)
    *-- if not print sizes "lcPrintHdr" will not include scales
    IF !llRpPrtSiz
      lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,15) +;
        lfEndOfGrp(@lnEndOfSty,PADR(&lcStyle..STYLE,lnMajorLen)),35) + SPACE(3)
    ELSE
      IF llTextMode
        *E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [Begin]
        *!*          lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,12) +;
        *!*                    lfEndOfGrp(@lnEndOfSty,PADR(&lcStyle..STYLE,lnMajorLen)),28) + SPACE(0) +;
        *!*                       IIF(lcScalCode = &lcStyle..Scale,'',;
        *!*                       PADL(ALLTRIM(&lcscal..Sz1),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz2),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz3),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz4),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz5),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz6),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz7),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz8),5))
        lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,12) +;
          lfEndOfGrp(@lnEndOfSty,PADR(&lcStyle..STYLE,lnMajorLen)),28) + SPACE(0) +;
          IIF(lcScalCode = &lcStyle..SCALE,'',IIF(llRpScale,;
          PADL(ALLTRIM(&lcscal..Sz1),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz2),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz3),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz4),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz5),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz6),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz7),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz8),5),''))
        *E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [End]
      ELSE
        *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
        *!*	        lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
        *!*	                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
        lcPrintHdr = PADR(ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + &lcStyle..DESC +;
          lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
        *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]

        FOR I = 1 TO 8
          Z = STR(I,1)
          laScals[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
        ENDFOR
      ENDIF

    ENDIF  && end of if not print sizes "lcPrintHdr" will not include scales

    lnMaxCnt   = &lcscal..CNT
    =lfSumGroup("lnStySz","lnSize",.T.) && Calculate Style initial totals.
    lnNonPrcOp = lnNonPrice             && Calculate Total Price.
    lnNonCstOp = lnNonCost              && Calculate Total Cost.

    lcScalCode = &lcStyle..SCALE

  ELSE  && it is the same style
    IF llRpScale
      IF &lcStyle..SCALE <> lcScalCode
        *-- if not print sizes "lcPrintHdr" will not include scales
        IF !llRpPrtSiz
          lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,15) +;
            lfEndOfGrp(@lnEndOfSty,PADR(&lcStyle..STYLE,lnMajorLen)),35) + SPACE(3)
        ELSE
          IF llTextMode

            lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,12) +;
              lfEndOfGrp(@lnEndOfSty,PADR(&lcStyle..STYLE,lnMajorLen)),28) + SPACE(0) +;
              PADL(ALLTRIM(&lcscal..Sz1),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz2),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz3),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz4),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz5),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz6),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz7),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz8),5)
            lcPrintHdr = STRTRAN(lcPrintHdr,SUBSTR(lcPrintHdr,1,35),SPACE(35))
          ELSE
            *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
            *!*	            lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
            *!*	                         lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
            lcPrintHdr = PADR(ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + &lcStyle..DESC +;
              lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
            *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]

            FOR I = 1 TO 8
              Z = STR(I,1)
              laScals[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
            ENDFOR
          ENDIF

        ENDIF  && end of if not print sizes "lcPrintHdr" will not include scales
        lcScalCode = &lcStyle..SCALE
      ELSE
        lcPrintHdr = ''
      ENDIF
    ENDIF

    lnMaxCnt = MAX(lnMaxCnt,&lcscal..CNT)

    *-- if line have quantities then calculate style group totals.
    IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
        lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
      *      IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
      IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
        lnFrstRec1 = RECNO()
        STORE 0 TO lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
          lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,lnStySz10,lnStySz11,;
          lnNonPrcOp,lnNonCstOp
      ENDIF
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
      *      ENDIF
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

      =lfSumGroup("lnStySz","lnSize")       && Calculate Style totals.
      IF lcMastFile = '&lcStyle' OR EMPTY(Dyelot)
        lnNonPrcOp = lnNonPrcOp + lnNonPrice  && Calculate Total Price.
        lnNonCstOp = lnNonCstOp + lnNonCost   && Calculate Total Cost.
      ENDIF
    ENDIF

  ENDIF   && end if it is new style.


  *-- if line have quantities then calculate grand totals.
  IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
      lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0
    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
    *    IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
    IF lnFrstRec = 0 OR lnFrstRec = RECNO()
      lnFrstRec = RECNO()
      STORE 0 TO lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
        lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
        lnNonPrcGd,lnNonCstGd
    ENDIF
    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
    *   ENDIF
    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

    =lfSumGroup("lnGrdSz","lnSize")      && Calculate Grand totals.
    IF lcMastFile = '&lcStyle' OR EMPTY(Dyelot)
      lnNonPrcGd = lnNonPrcGd + lnNonPrice && Calculate Total Price.
      lnNonCstGd = lnNonCstGd + lnNonCost  && Calculate Total Cost.
    ENDIF
  ENDIF

  *-- Evaluate all sizes values from style file Case Master file is StyDye
  *-- and print location detail, or print style dyelots . [Begin]
  IF lcMastFile = "&lcStyDye" AND llPrintClr AND   !(RIGHT(STYLE,19 - lnMajorLen)==lcPrntNon)
    llPrnClrLn = .T.
    lcPrntNon  = RIGHT(STYLE,19 - lnMajorLen)
    lnAllSizes = 0
    lnClrSz9   = 0
    lnClrSz10   = 0
    lnClrSz11   = 0
    FOR lnAllSizes = 1 TO 8
      lcSize   = "lnClrSz"+STR(lnAllSizes,1)
      &lcSize  = EVALUATE(laTrnNonAr[lnAllSizes])
      lnClrSz9 = lnClrSz9 + &lcSize
      IF lcRpOTSSig $ 'BL'
        IF &lcSize > 0
          lnClrSz10 = lnClrSz10 + &lcSize && variable hold sum of +ve OTS Qtys
        ELSE
          lnClrSz11 = lnClrSz11 + &lcSize && variable hold sum of -ve OTS Qtys
        ENDIF
      ENDIF
    ENDFOR
    lnClrPrice = lnClrSz9 * (&lcStyle..PriceA)

    *! B609307,1 HES 06/19/2010 Fic bug of Style Summary report with Loc detail incorrect totals by colour [Start]
    IF ALEN(laRPRepTar,1) = 1 AND llRPWhDeta
      lcOldAlis = SELECT(0)
      lnlocPos = ASCAN(loogScroll.laOgFXFLT,"STYDYE.CWARECODE")
      lcFileLoc = ''
      llLocSelected  =.F.
      IF lnlocPos > 0
        lnlocPos  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnlocPos ,1)
        IF !EMPTY(loOGScroll.laOgFxFlt[lnlocPos  ,6]) AND USED(loOGScroll.laOgFxFlt[lnlocPos  ,6])
          lcFileLoc = loOGScroll.laOgFxFlt[lnlocPos  ,6]
          SELECT(lcFileLoc)
          LOCATE
          IF !EOF()
            llLocSelected  = .T.
          ENDIF
        ENDIF
      ENDIF
      SELECT (lcStyDye)
      lcStyDyeKey=EVALUATE(KEY())
      FOR lnT =1 TO 8
        lcT = STR(lnT,1)
        lnClrSz&lcT.  = 0
      ENDFOR
      lnClrSz9 = 0

      SELECT (lcStyDye)
      lcSty = &lcStyle..STYLE
      =SEEK(lcSty)
      SCAN REST WHILE STYLE+CWARECODE+DYELOT = lcSty FOR EMPTY(DYELOT) AND IIF(llLocSelected,SEEK(CWARECODE,lcFileLoc),.T.)
        FOR lnT =1 TO 8
          lcT = STR(lnT,1)
          lnClrSz&lcT. = lnClrSz&lcT. +  EVALUATE(laTranAray[lnT])
          lnClrSz9 = lnClrSz9 +  EVALUATE(laTranAray[lnT])
        ENDFOR
      ENDSCAN
      SELECT (lcStyDye)
      =SEEK(lcStyDyeKey)
      SELECT(lcOldAlis)
    ENDIF
    *! B609307,1 HES 06/19/2010 Fic bug of Style Summary report with Loc detail incorrect totals by colour [End  ]

    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *IF llLinkGlJl
    *  lnClrCost = IIF("SOH" $ lcRepTarVl , &lcStyle..nStkVal , IIF(lcCstMeth="S",;
    &lcStyle..TotCost,&lcStyle..Ave_Cost)*lnClrSz9)
    *ELSE
    *B608407,1 WAM 01/14/2008 (End)

    lnClrCost = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth="S", &lcStyle..TotCost * lnClrSz9 , &lcStyle..nStkVal ) , IIF(lcCstMeth="S",;
      &lcStyle..TotCost,&lcStyle..Ave_Cost)*lnClrSz9)
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *ENDIF
    *B608407,1 WAM 01/14/2008 (End)

  ELSE
    llPrnClrLn = .F.
  ENDIF
  *-- and print location detail, or print style dyelots . [End  ]

ENDIF     && end if user want to print zeros or total value not equal zero.

RETURN lcPrintHdr
*-- end of lfStyHeadr.

*!*************************************************************
*! Name      : lfSumGroup
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Get summation of any group.
*!*************************************************************
*! Called from : lfStyHeadr()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSumGroup()
*!*************************************************************
FUNCTION lfSumGroup
PARAMETERS lcSummedVr,lcLineVar,llPrimary
*-- if you loop style file or you loop location record in stydye file.
IF lcMastFile = '&lcStyle' OR EMPTY(Dyelot)
  PRIVATE lnAllSizes,lcVar,lcVal
  lnAllSizes = 0
  FOR lnAllSizes = 1 TO 9
    lcVar  = lcSummedVr + STR(lnAllSizes,1)
    lcVal  = lcLineVar + STR(lnAllSizes,1)
    *-- if it is new group.
    IF llPrimary
      &lcVar = &lcVal
    ELSE  && another line in the same group.
      &lcVar = &lcVar + &lcVal
    ENDIF
  ENDFOR
  IF lcRpOTSSig $ 'BL'
    FOR lnAllSizes = 10 TO 11
      lcVar  = lcSummedVr + STR(lnAllSizes,2)
      lcVal  = lcLineVar + STR(lnAllSizes,2)
      *-- if it is new group.
      IF llPrimary
        &lcVar = &lcVal
      ELSE  && another line in the same group.
        &lcVar = &lcVar + &lcVal
      ENDIF
    ENDFOR
  ENDIF
ENDIF  && end if you loop style file or you loop location record in stydye file.
*-- end of lfSumGroup.


*!*************************************************************
*! Name      : lfNonMjDes
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Evaluate Non Major Code and Description
*!*************************************************************
*! Called from : lfStyHeadr()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNonMjDes()
*!*************************************************************
FUNCTION lfNonMjDes
LOCAL lnI , lcTemp

lcTemp = ''
lnI = 0


*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lcTemp = ''
  DO CASE
    *-- Free, Other, Make, or Quality Segment.
  CASE laMajSeg[lnI,1] $ "FOTQ"
    IF SEEK(STR(lnI,1)+SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcSegVal)
      lcTemp = ALLTRIM(&lcSegVal..cISgValSd)
    ENDIF
    *-- Season, Color, Division, or Style group Segment.
  CASE laMajSeg[lnI,1] $ "ZCDG"
    DO CASE
    CASE laMajSeg[lnI,1] = "Z"
      lcCodeExpr = "SEASON"
    CASE laMajSeg[lnI,1] = "C"
      lcCodeExpr = "COLOR"
    CASE laMajSeg[lnI,1] = "D"
      lcCodeExpr = "CDIVISION"
    OTHERWISE
      lcCodeExpr = "CSTYGROUP"
    ENDCASE

    lcTemp = ALLTRIM(gfCodDes(SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcCodeExpr,.T.))

    *-- Size Seqment case.
  OTHERWISE

    IF SEEK("S"+SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcscal)
      lcTemp = ALLTRIM(&lcscal..cScl_desc)
    ENDIF

  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
ENDFOR    && end Loop Around Non Major elements.
*-- end of lfNonMjDes.

*!*************************************************************
*! Name      : lfEndOfGrp
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Calculate End of any Group
*!*************************************************************
*! Called from : lfStyHeadr()
*!*************************************************************
*! Passed Parameters : 1- Variable Name passed by reference.
*!                   : 2- Seek value.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfEndOfGrp()
*!*************************************************************
FUNCTION lfEndOfGrp
PARAMETERS lcVariable,lcEqualExp
PRIVATE lnCurAlis
*--T20070628.0010 AKM [Start]
*Screen.Visible = .T.
*SET STEP ON
*--T20070628.0010 AKM [End]

lnCurAlis = SELECT(0)
lcTagName = IIF(lcMastFile = '&lcStyle','RevStyle',IIF(lcRpSortBy == "S",'RevStyDy','RevStyDyW'))
SELECT (lcRevFile)
lcFilter = SET("Filter")
SET FILTER TO

SET ORDER TO TAG lcTagName DESCENDING

=SEEK(lcEqualExp)

*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*B608163,1TMI [Start] if one transaction and sort by Location - other sort
IF ALEN(laRpRepTar,1)=1 AND lcRpFormNa $ 'ICSTYDCS|ICSTYDCL'
  *- Get the correct expression to remove unneeded lines based on
  LOCAL lcRmvExpr
  lcRmvExpr = laTranAray[1]+'+'+;
    laTranAray[2]+'+'+;
    laTranAray[3]+'+'+;
    laTranAray[4]+'+'+;
    laTranAray[5]+'+'+;
    laTranAray[6]+'+'+;
    laTranAray[7]+'+'+;
    laTranAray[8]
  *B608470,1 WAM 03/04/2008 Fix condition based on which the style sum total is printed.
  *B608470,1 WAM 03/04/2008 Following lines are commented out
  *!*	  IF 'OTS' $ laRpRepTar[1]
  *!*	    lcRmvExpr = 'STK1+STK2+STK3+STK4+STK5+STK6+STK7+STK8 - (ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8)'+;
  *!*	                IIF(laRpRepTar[1] = 'Imm. OTS','',;
  *!*	                '+'+IIF(lcRpOTSB='W','WIP1+WIP2+WIP3+WIP4+WIP5+WIP6+WIP7+WIP8','PLAN1+PLAN2+PLAN3+PLAN4+PLAN5+PLAN6+PLAN7+PLAN8'))
  *!*	  ENDIF
  *B608470,1 WAM 03/04/2008 (End)

  *: B608711,2 MMT 10/28/2008 Fix bug of some Styles subtotal is not printed when use print BOTH OTS [Start]
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
  *IF  'OTS' $ laRpRepTar[1] AND (lcRPOTSSig $ 'BAL')
  IF  'OTS' $ laRpRepTar[1] AND (lcRPOTSSig = 'A')
    *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[END]
    lcRmvExpr = laTranAray[1]+' <> 0 OR '+;
      laTranAray[2]+' <> 0 OR '+;
      laTranAray[3]+' <> 0 OR '+;
      laTranAray[4]+' <> 0 OR '+;
      laTranAray[5]+' <> 0 OR '+;
      laTranAray[6]+' <> 0 OR '+;
      laTranAray[7]+' <> 0 OR '+;
      laTranAray[8]
  ENDIF
  *: B608711,2 MMT 10/28/2008 Fix bug of some Styles subtotal is not printed when use print BOTH OTS [End]

  lcKey = KEY()
  IF !llRPShwZer
    LOCATE REST WHILE &lcKey = lcEqualExp ;
      FOR &lcRmvExpr <> 0
  ENDIF
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
  *IF 'OTS' $ laRpRepTar[1] AND (lcRPOTSSig='P' OR lcRPOTSSig='N')
  IF 'OTS' $ laRpRepTar[1] AND (lcRPOTSSig $ 'LBPN')
    IF (lcRPOTSSig='P' OR lcRPOTSSig='N')
      lcSgn = IIF(lcRPOTSSig='P', '>= lnRPOTSMin' , '<= lnRPOTSMax')
      *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[END]
      =SEEK(lcEqualExp)
      *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
      *!*	    LOCATE REST WHILE &lcKey = lcEqualExp ;
      *!*	                  FOR &lcRmvExpr &lcSgn lnRPOTSMin .AND. IIF(!llRPShwZer,&lcRmvExpr <> 0,.T.)
      LOCATE REST WHILE &lcKey = lcEqualExp ;
        FOR &lcRmvExpr &lcSgn  .AND. IIF(!llRPShwZer,&lcRmvExpr <> 0,.T.)
    ELSE
      =SEEK(lcEqualExp)
      LOCATE REST WHILE &lcKey = lcEqualExp ;
        FOR &lcRmvExpr >= lnRPOTSMin OR &lcRmvExpr <= lnRPOTSMax .AND. IIF(!llRPShwZer,&lcRmvExpr <> 0,.T.)
    ENDIF
    *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
  ENDIF
ENDIF
*B608163,1TMI [End  ]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]
lcVariable = IIF(BETWEEN(RECNO(),1,RECCOUNT()),RECNO(),1)
SET ORDER TO TAG lcTagName ASCENDING
SET FILTER TO &lcFilter
SELECT (lnCurAlis)

RETURN ''
*-- end of lfEndOfGrp.
*!*************************************************************
*! Name      : lfLastItem
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to get the last printed item to avoid replicate
*!             the item label if the item has not changed
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfLastItem()
*!*************************************************************

FUNCTION lfLastItem

IF NotScale = 'Y'
  lcLastItem = &lcDummy..LABEL
  lcLastSty  = StyCode
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfLastSty
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to get the last printed style to avoid replicate
*!             the item label if the item has not changed
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfLastSty()
*!*************************************************************

FUNCTION lfLastSty

IF NotScale = 'Y'
  lcLastSty = &lcTrns..StyCode
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfLocHeadr
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Evaluate By Location all report variables and print header.
*!*************************************************************
*! Called from : lfPrintHdr()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Report Header
*!*************************************************************
*! Example     : =lfLocHeadr()
*!*************************************************************
FUNCTION lfLocHeadr
PRIVATE lcPrintHdr , lcSize , lnAllSizes
STORE '' TO lcNonMjDes , lcPrintHdr , lcLocBins , lcPrintSty
lnAllSizes = 0
=lfInitVals()  && Assign ititial values to all line variables.

*-- Evaluate all sizes values. [Begin]
FOR lnAllSizes = 1 TO 8
  lcSize  = "lnSize"+STR(lnAllSizes,1)
  &lcSize = EVALUATE(laTranAray[lnAllSizes])
  lnSize9 = lnSize9 + &lcSize
  IF lcRpOTSSig $ 'BL'
    IF &lcSize > 0
      lnSize10 = lnSize10+ &lcSize && variable hold sum of +ve OTS Qtys
    ELSE
      lnSize11 = lnSize11+ &lcSize && variable hold sum of -ve OTS Qtys
    ENDIF
  ENDIF
ENDFOR
*-- Evaluate all sizes values. [End  ]

*-- Zero values if OTS or IOTS and condition is true. [Begin]
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMin)))
IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMax)) OR (lcRpOTSSig $ 'LB' AND lnSize9 > lnRpOTSMax AND lnSize9 < lnRpOTSMin))
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[END]
  STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
    lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11
ENDIF
*-- Zero values if OTS or IOTS and condition is true. [End  ]

*-- if user want to print zeros or total value not equal zero.
*: B608166,1 SSH 07/15/2007 Change the condition to be on size level not total level
*IF llRpShwZer OR lnSize9
IF llRpShwZer OR lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR lnSize5 <> 0 OR lnSize6<> 0 OR lnSize7 <> 0 OR lnSize8 <> 0 OR lnSize9 <> 0
  *: B608166,1 SSH 07/15/2007 Change the condition to be on size level not total level
  lnNonPrice = lnSize9 * (&lcStyle..PriceA)
  *-- if Location record.
  IF EMPTY(DYELOT)

    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *IF llLinkGlJl
    *  lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="A", Ave_Cost,&lcStyle..TotCost)*lnSize9)
    *  lnCstAveVl = IIF("SOH" $ lcRepTarVl,nStkVal/lnSize9,IIF(lcCstMeth="A",Ave_Cost,&lcStyle..TotCost))
    *ELSE
    *B608407,1 WAM 01/14/2008 (End)

    lnNonCost  = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth = "S" , &lcStyle..TotCost *lnSize9 , nStkVal )       , IIF(lcCstMeth="A", Ave_Cost,&lcStyle..TotCost)*lnSize9)
    lnCstAveVl = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth = "S" , &lcStyle..TotCost          , nStkVal/lnSize9), IIF(lcCstMeth="A",Ave_Cost,&lcStyle..TotCost))
    *B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
    *ENDIF
    *B608407,1 WAM 01/14/2008 (End)

    =lfNonMjDes()  && Evaluate Non Major Description.

  ELSE  && Dyelot Record value.
    IF llRpPrnDye
      IF lcCstMeth="A"
        = lfGetCosts(@lnCstAveVl)
      ELSE
        lnCstAveVl = &lcStyle..TotCost
      ENDIF
    ENDIF
    lnNonCost = lnSize9 * lnCstAveVl
  ENDIF

  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
  *  IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
  IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
    lnFrstRec1 = RECNO()
    lcPrntLoc  = SPACE(6)
    lcPrntSty  = SPACE(lnMajorLen)
    lcScalCode = SPACE(3)
  ENDIF
  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
  *  ENDIF
  *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

  *-- if it is new location.
  IF !(cWareCode==lcPrntLoc)
    lcPrntLoc = cWareCode
    lcPrintHdr = LANG_ICSTYREP_Location+" : " + cWareCode +;
      lfEndOfGrp(@lnEndOfLoc,cWareCode)

    =lfSumGroup("lnLocSz","lnSize",.T.) && Calculate Location initial totals.
    lnNonPrcLc = lnNonPrice             && Calculate Total Price.
    lnNonCstLc = lnNonCost              && Calculate Total Cost.

  ELSE  && it is the same location

    IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
        lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
      *      IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
      IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
        lnFrstRec1 = RECNO()
        STORE 0 TO lnLocSz1,lnLocSz2,lnLocSz3,lnLocSz4,;
          lnLocSz5,lnLocSz6,lnLocSz7,lnLocSz8,lnLocSz9,lnLocSz10,lnLocSz11,;
          lnNonPrcLc,lnNonCstLc
      ENDIF
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
      *      ENDIF
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

      =lfSumGroup("lnLocSz","lnSize")         && Calculate Style totals.
      IF EMPTY(Dyelot)
        lnNonPrcLc = lnNonPrcLc + lnNonPrice  && Calculate Total Price.
        lnNonCstLc = lnNonCstLc + lnNonCost   && Calculate Total Cost.
      ENDIF
    ENDIF

  ENDIF   && end if it is new location.

  *-- if it is new style.
  IF !(cWareCode + PADR(&lcstyle..STYLE,lnMajorLen)==lcPrntSty)
    lcPrntSty  = cWareCode + PADR(&lcStyle..STYLE,lnMajorLen)
    *-- if not print sizes "lcPrintHdr" will not include scales
    IF !llRpPrtSiz
      lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,15) +;
        lfEndOfGrp(@lnEndOfSty,lcPrntSty),35) + SPACE(3)
    ELSE
      IF llTextMode
        *E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [Begin]
        *!*          lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,12) +;
        *!*                       lfEndOfGrp(@lnEndOfSty,lcPrntSty),29) + SPACE(1) +;
        *!*                       IIF(lcScalCode = &lcStyle..Scale,'',;
        *!*                       PADL(ALLTRIM(&lcscal..Sz1),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz2),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz3),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz4),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz5),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz6),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz7),5) + SPACE(2) +;
        *!*                       PADL(ALLTRIM(&lcscal..Sz8),5))
        lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,12) +;
          lfEndOfGrp(@lnEndOfSty,lcPrntSty),29) + SPACE(1) +;
          IIF(lcScalCode = &lcStyle..SCALE,'',IIF(!llRpScale,'',;
          PADL(ALLTRIM(&lcscal..Sz1),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz2),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz3),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz4),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz5),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz6),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz7),5) + SPACE(2) +;
          PADL(ALLTRIM(&lcscal..Sz8),5)))

        *E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [End]
      ELSE
        *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
        *!*	        lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
        *!*	                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
        lcPrintSty = PADR(ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,15) +;
          lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
        *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]
        FOR I = 1 TO 8
          Z = STR(I,1)
          laScals[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
        ENDFOR
      ENDIF

    ENDIF     && end if not print sizes "lcPrintHdr" will not include scales
    lnMaxCnt   = &lcscal..CNT
    =lfSumGroup("lnStySz","lnSize",.T.) && Calculate Style initial totals.
    lnNonPrcOp = lnNonPrice             && Calculate Total Price.
    lnNonCstOp = lnNonCost              && Calculate Total Cost.

    lcScalCode = &lcStyle..SCALE

  ELSE  && it is the same style

    IF llRpScale
      IF &lcStyle..SCALE <> lcScalCode
        IF !llRpPrtSiz
          lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,15) +;
            lfEndOfGrp(@lnEndOfSty,PADR(&lcStyle..STYLE,lnMajorLen)),35) + SPACE(3)
        ELSE
          IF llTextMode
            lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..DESC,12) +;
              lfEndOfGrp(@lnEndOfSty,lcPrntSty),29) + SPACE(1) +;
              PADL(ALLTRIM(&lcscal..Sz1),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz2),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz3),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz4),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz5),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz6),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz7),5) + SPACE(2) +;
              PADL(ALLTRIM(&lcscal..Sz8),5)
            lcPrintHdr = STRTRAN(lcPrintHdr,SUBSTR(lcPrintHdr,1,35),SPACE(35))
          ELSE
            *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
            *!*	            lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
            *!*	                         lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
            lcPrintSty = PADR(ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + &lcStyle..DESC +;
              lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
            *! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]
            FOR I = 1 TO 8
              Z = STR(I,1)
              laScals[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
            ENDFOR
          ENDIF
        ENDIF  && end of if not print sizes "lcPrintHdr" will not include scales
        lcScalCode = &lcStyle..SCALE
      ELSE
        lcPrintHdr = ''
      ENDIF
    ENDIF

    lnMaxCnt = MAX(lnMaxCnt,&lcscal..CNT)
    IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
        lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
      *      IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
      IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
        lnFrstRec1 = RECNO()
        STORE 0 TO lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
          lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,lnStySz10,lnStySz11,;
          lnNonPrcOp,lnNonCstOp
      ENDIF
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
      *      ENDIF
      *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

      =lfSumGroup("lnStySz","lnSize")       && Calculate Style totals.
      IF EMPTY(Dyelot)
        lnNonPrcOp = lnNonPrcOp + lnNonPrice  && Calculate Total Price.
        lnNonCstOp = lnNonCstOp + lnNonCost   && Calculate Total Cost.
      ENDIF
    ENDIF

  ENDIF   && end if it is new style.

  IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
      lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
    *    IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
    IF lnFrstRec = 0 OR lnFrstRec = RECNO()
      lnFrstRec = RECNO()
      STORE 0 TO lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
        lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
        lnNonPrcGd,lnNonCstGd
    ENDIF
    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
    *    ENDIF
    *B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

    =lfSumGroup("lnGrdSz","lnSize")      && Calculate Grand totals.
    IF EMPTY(Dyelot)
      lnNonPrcGd = lnNonPrcGd + lnNonPrice && Calculate Total Price.
      lnNonCstGd = lnNonCstGd + lnNonCost  && Calculate Total Cost.
    ENDIF
  ENDIF
ENDIF     && end if user want to print zeros or total value not equal zero.
*-- not equal zero. [End  ]
RETURN lcPrintHdr
*-- end of lfLocHeadr.

*!*************************************************************
*! Name      : lfLstToItm
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to get the last printed style which it's total has printed
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfLstToItm()
*!*************************************************************

FUNCTION lfLstToItm

IF NotScale = 'Y'
  lcLstToItm = &lcDummy..ITEM
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfPrnLoc
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to print locations
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrnLoc()
*!*************************************************************

FUNCTION lfPrnLoc

IF NotScale = 'Y'
  IF (llRPWhDeta OR lcRpSortBy == 'W') AND llRPPrnLoc AND ;
      EVAL(lcDummy+'.Label') <> LANG_ICSTYREP_Dyelot AND EMPTY(&lcStyDye..Dyelot)
    IF ALLTRIM(&lcDummy..ITEM) $ 'IOTS'
      DO CASE
      CASE lcRPOTSSig ='P'
        lcRet = IIF(!llRPPrnLoc,'',;
          IIF(ALLTRIM(&lcDummy..ITEM) $ 'IOTS',;
          IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..ITEM))>=lnRPOTSMin,&lcTrns..Loc,'');
          ,&lcTrns..Loc))
      CASE lcRPOTSSig ='N'
        *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
        *!*	          lcRet = IIF(!llRPWhDeta,'',;
        *!*	                      IIF(ALLTRIM(&lcDummy..Item) $ 'IOTS',;
        *!*	                          IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..Item))<=lnRPOTSMin,&lcTrns..Loc,'');
        *!*	                              ,&lcTrns..Loc))
        lcRet = IIF(!llRPWhDeta,'',;
          IIF(ALLTRIM(&lcDummy..ITEM) $ 'IOTS',;
          IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..ITEM))<=lnRPOTSMax,&lcTrns..Loc,'');
          ,&lcTrns..Loc))

      CASE lcRPOTSSig $ 'LB'
        lcRet = IIF(!llRPWhDeta,'',;
          IIF(ALLTRIM(&lcDummy..ITEM) $ 'IOTS',;
          IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..ITEM))<=lnRPOTSMax OR IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..ITEM))>=lnRPOTSMin,;
          &lcTrns..Loc,'');
          ,&lcTrns..Loc))
        *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
      ENDCASE
    ELSE
      lcRet = &lcTrns..Loc
    ENDIF

  ELSE
    lcRet = ''
  ENDIF

ELSE
  lcRet = ''
ENDIF

RETURN ALLTRIM(lcRet)

*!*************************************************************
*! Name      : lfPrnStyItm
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to print the nonmajor line
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrnStyItm()
*!*************************************************************

FUNCTION lfPrnStyItm

PARAMETERS lcFld



PRIVATE lcFld,lnRet,lcUseFile


lcUseFile = IIF(lcRpSortBy == "W",lcTrns,lcStyTmp)
lnRet = ''
IF NotScale = 'Y'
  IF &lcscal..CNT >= VAL(lcFld) OR !EMPTY(&lcUseFile..cEndMaj+&lcUseFile..cEndSort+&lcUseFile..cEndRep)
    IF !EMPTY(&lcDummy..ITEM)
      IF (lcRpSortBy == "W" AND &lcDummy..LABEL <> LANG_ICSTYREP_Dyelot AND EMPTY(&lcUseFile..Dyelot)) OR ;
          lcRpSortBy <> "W"
        IF lcFld = 'ItmLabel'
          IF !(ALLTRIM(&lcDummy..ITEM) $ 'IOTS')
            lnRet = &lcDummy..LABEL
          ELSE
            IF (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND lPrnIOTS) OR ;
                (ALLTRIM(&lcDummy..ITEM) = 'OTS' AND lPrnOTS)   OR ;
                !EMPTY(&lcUseFile..cEndMaj)
              lnRet = &lcDummy..LABEL
            ELSE
              lnRet = ''
            ENDIF
          ENDIF
        ELSE
          IF lcFld = 'WhsLabel'
            IF llRPWhDeta AND EMPTY(cEndMaj)
              IF !(ALLTRIM(&lcDummy..ITEM) $ 'IOTS')
                lnRet = LANG_ICSTYREP_Location+' '
              ELSE
                IF (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND lPrnIOTS) OR ;
                    (ALLTRIM(&lcDummy..ITEM) = 'OTS' AND lPrnOTS)   OR ;
                    !EMPTY(&lcUseFile..cEndMaj)
                  lnRet = LANG_ICSTYREP_Location+' '
                ELSE
                  lnRet = ''
                ENDIF
              ENDIF
            ELSE
              lnRet = ''
            ENDIF
          ELSE
            IF !INLIST(lcFld,'Price','Cost')
              lnRet = EVAL(lcUseFile+"."+IIF(lcFld='Tot','Tot'+ALLTRIM(&lcDummy..ITEM),;
                ALLTRIM(&lcDummy..ITEM)+lcFld))

              IF ALLTRIM(&lcDummy..ITEM) $ 'IOTS' AND EMPTY(cEndMaj)
                IF !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND lPrnIOTS) OR ;
                    (ALLTRIM(&lcDummy..ITEM) = 'OTS' AND lPrnOTS) )

                  lnRet = ''
                ELSE
                  IF BETWEEN(lcFld,"1","8")
                    DO CASE
                    CASE lcRPOTSSig = 'P' AND lnRet <= 0
                      lnRet = ''
                    CASE lcRPOTSSig = 'N' AND lnRet >= 0
                      lnRet = ''
                    ENDCASE
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              IF lcFld = 'Cost'
                IF !EMPTY(cEndMaj) OR ;
                    !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND !lPrnIOTS) OR ;
                    (ALLTRIM(&lcDummy..ITEM) = 'OTS'  AND !lPrnOTS ) )
                  lnRet = EVAL('nStkV'+ALLTRIM(&lcDummy..ITEM))
                ELSE
                  lnRet = ''
                ENDIF
              ENDIF
              IF lcFld = 'Price'
                IF !EMPTY(cEndMaj) OR ;
                    !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND !lPrnIOTS) OR ;
                    (ALLTRIM(&lcDummy..ITEM) = 'OTS'  AND !lPrnOTS ) )

                  lnRet = EVAL('nSalV'+ALLTRIM(&lcDummy..ITEM))
                ELSE
                  lnRet = ''
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF
RETURN lnRet
*!*************************************************************
*! Name      : lfPrntBin
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Print location bins.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrntBin()
*!*************************************************************
FUNCTION lfPrntBin
IF (lcRpSortBy == "L" OR llRpPrnLoc) AND EMPTY(Dyelot) AND (llRpShwZer OR lnSize9 <> 0)
  PRIVATE lnCurrAls , lcCurrOrd
  lnCurrAls = SELECT(0)
  SELECT (lcWhsLoc)
  lcCurrOrd = ORDER()
  SET ORDER TO TAG LCWHSLOCST
  IF SEEK(&lcStyDye..STYLE+SPACE(6)+&lcStyDye..cWareCode)
    PRIVATE lnBinNum
    lnBinNum = 0
    SCAN REST WHILE STYLE       +COLOR   +cWareCode       +cLocation = ;
        &lcStyDye..STYLE+SPACE(6)+&lcStyDye..cWareCode
      lcLocBins = IIF(EMPTY(lcLocBins),LANG_ICSTYREP_Bin+' : ',lcLocBins+', ') + ALLTRIM(cLocation)
      lnBinNum = lnBinNum + 1
    ENDSCAN
    IF lnBinNum > 1
      lcLocBins = STRTRAN(lcLocBins,LANG_ICSTYREP_Bin+" :",LANG_ICSTYREP_Bin+" :")
    ENDIF
  ENDIF
  SET ORDER TO &lcCurrOrd
  SELECT (lnCurrAls)
ENDIF
RETURN ''
*-- end of lfPrntBin.


*!*************************************************************
*! Name      : lfPrnWhsItm
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to print the location line
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrnWhsItm()
*!*************************************************************

FUNCTION lfPrnWhsItm
PARAMETERS lcFld

PRIVATE lcFld,lnRet


lnRet = ''

IF NotScale = 'Y'
  IF llRPWhDeta AND EMPTY(&lcStyDye..Dyelot) AND EMPTY(&lcStyTmp..cEndMaj) AND !EMPTY(&lcDummy..ITEM)
    IF lcFld = 'WhsCode'
      IF !(ALLTRIM(&lcDummy..ITEM) $ 'IOTS')
        lnRet = &lcTrns..WareCode
      ELSE
        IF (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND &lcTrns..lPrnIOTS) OR ;
            (ALLTRIM(&lcDummy..ITEM) = 'OTS'  AND &lcTrns..lPrnOTS)
          lnRet = &lcTrns..WareCode
        ELSE
          lnRet = ''
        ENDIF
      ENDIF
    ELSE
      IF !INLIST(lcFld,'Price','Cost')
        lnRet = EVAL(lcTrns+'.'+IIF(lcFld='Tot','Tot'+ALLTRIM(&lcDummy..ITEM),;
          ALLTRIM(&lcDummy..ITEM)+lcFld))

        IF ALLTRIM(&lcDummy..ITEM) $ 'IOTS'
          IF !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND &lcTrns..lPrnIOTS) OR ;
              (ALLTRIM(&lcDummy..ITEM) = 'OTS'  AND &lcTrns..lPrnOTS) )
            lnRet = ''
          ELSE
            IF BETWEEN(lcFld,"1","8")
              DO CASE
              CASE lcRPOTSSig = 'P' AND lnRet <= 0
                lnRet = ''
              CASE lcRPOTSSig = 'N' AND lnRet >= 0
                lnRet = ''
              ENDCASE
            ENDIF
          ENDIF
        ENDIF
      ELSE
        IF lcFld = 'Cost'
          IF !EMPTY(&lcStyTmp..cEndMaj) OR ;
              !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND !&lcTrns..lPrnIOTS) OR ;
              (ALLTRIM(&lcDummy..ITEM) = 'OTS'  AND !&lcTrns..lPrnOTS ) )
            lnRet = EVAL(lcTrns+'.nStkV'+ALLTRIM(&lcDummy..ITEM))
          ELSE
            lnRet = ''
          ENDIF
        ENDIF
        IF lcFld = 'Price'
          IF !EMPTY(&lcStyTmp..cEndMaj) OR ;
              !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND !&lcTrns..lPrnIOTS) OR ;
              (ALLTRIM(&lcDummy..ITEM) = 'OTS'  AND !&lcTrns..lPrnOTS ) )
            lnRet = EVAL(lcTrns+'.nSalV'+ALLTRIM(&lcDummy..ITEM))
          ELSE
            lnRet = ''
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSE
    lnRet = ''
  ENDIF
ENDIF

RETURN lnRet


*!**************************************************************************
*! Name        : lfRstGrdVr
*! Developer   : Heba Mohamed Amin	(HMA)
*! Date        : 02/09/2005
*! Purpose     : Reset grand totals variables
*!***************************************************************************
*! Called from :
*!***************************************************************************
*! Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     :  = lfRstGrdVr()
*!***************************************************************************
FUNCTION lfRstGrdVr

STORE 0 TO lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
  lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
  lnNonPrcGd,lnNonCstGd

RETURN ''
*--End of lfRstGrdVr.

*!*************************************************************
*! Name      : lfTotTtl
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to print total Title
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfTotTtl()
*!*************************************************************
FUNCTION lfTotTtl



PRIVATE lcRet

IF lcRpSortBy == 'W'
  IF NotScale = 'N'
    lcRet = ''
  ELSE
    IF EMPTY(cEndMaj)
      IF lcLastSty=EVAL(lcTrns+'.StyCode')
        lcRet = ''
      ELSE
        lcRet = lfPrnLoc()
      ENDIF
    ELSE
      IF EVAL(lcDummy+'.cRecNo')<>'01' OR EMPTY(EVAL(lcDummy+'.Item'))
        lcRet = ''
      ELSE
        IF EMPTY(cEndSort)
          IF EMPTY(EVAL(lcStyDye+'.Dyelot'))

            *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
            *lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+' : '
            lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+IIF(!EMPTY(EVAL(lcTrns+".cStyMajor")),' for : ',':')+EVAL(lcTrns+".cStyMajor")
            *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

          ELSE
            lcRet = ''
          ENDIF
        ELSE
          IF EMPTY(cEndRep)
            IF EMPTY(EVAL(lcStyDye+'.Dyelot'))

              *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
              *lcRet = LANG_ICSTYREP_Warehouse_Total+' : '
              lcRet = LANG_ICSTYREP_Warehouse_Total+' for : '+ EVAL(lcTrns+'.WareCode')
              *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

            ELSE
              lcRet =''
            ENDIF
          ELSE
            IF EMPTY(EVAL(lcStyDye+'.Dyelot'))
              lcRet = LANG_ICSTYREP_Grand_Total+' : '
            ELSE
              lcRet =''
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

ELSE
  IF NotScale = 'N'
    lcRet = ''
  ELSE
    IF EMPTY(cEndMaj) OR EMPTY(EVAL(lcDummy+'.Item')) OR EVAL(lcDummy+'.Label')=lcLastItem
      lcRet = ''
    ELSE
      IF EVAL(lcDummy+'.cRecNo')<>'01'
        lcRet = ''
      ELSE
        IF EMPTY(cEndSort)
          IF EMPTY(EVAL(lcStyDye+'.Dyelot'))

            *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
            *lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+' : '
            lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+IIF(!EMPTY(EVAL(lcStyTmp+".cStyMajor")),' for : ',':')+EVAL(lcStyTmp+".cStyMajor")
            *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

          ELSE
            lcRet = ''
          ENDIF
        ELSE
          IF EMPTY(cEndRep)
            IF EMPTY(EVAL(lcStyDye+'.Dyelot'))

              *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
              *lcRet = lcSortTtl + ' '+LANG_ICSTYREP_Total+' : '
              lcRet = lcSortTtl + ' '+LANG_ICSTYREP_Total+IIF(!EMPTY(EVAL(lcStyTmp+lcSortFld)),' for : ',':')+EVAL(lcStyTmp+lcSortFld)
              *: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

            ELSE
              lcRet =''
            ENDIF
          ELSE
            IF EMPTY(EVAL(lcStyDye+'.Dyelot'))
              lcRet = LANG_ICSTYREP_Grand_Total+' : '
            ELSE
              lcRet =''
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

RETURN lcRet

*!*************************************************************
*! Name      : lfScale
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : to print Scale Header
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfScale()
*!*************************************************************

FUNCTION lfScale
PRIVATE  lcRet

IF NotScale = 'N' AND IIF(lcRpSortBy == 'W' ,  &lcTrns..SCALE #  lcScale1 , &lcStyTmp..SCALE #  lcScale1 )
  *E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [Begin]
  IF llRpScale
    IF llTextMode
      lcRet = SPACE(15) + ;
        PADL(ALLTRIM(&lcscal..Sz1),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz2),8) + SPACE(1) +;
        PADL(ALLTRIM(&lcscal..Sz3),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz4),8) + SPACE(1) +;
        PADL(ALLTRIM(&lcscal..Sz5),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz6),8) + SPACE(1) +;
        PADL(ALLTRIM(&lcscal..Sz7),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz8),8) + SPACE(1) +;
        PADL(LANG_ICSTYREP_Total,8)

      IF llShowCost
        lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Cost_Val
      ENDIF
      IF llShowSale
        lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Sales_Val
      ENDIF
    ELSE
      *-- we will let it old the scale.

      lcRet=''

      *B608096,1 MMT 05/24/2007 fix bug of not displaying the Scale [Start]
      lcRet = SPACE(15) + ;
        PADL(ALLTRIM(&lcscal..Sz1),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz2),8) + SPACE(1) +;
        PADL(ALLTRIM(&lcscal..Sz3),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz4),8) + SPACE(1) +;
        PADL(ALLTRIM(&lcscal..Sz5),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz6),8) + SPACE(1) +;
        PADL(ALLTRIM(&lcscal..Sz7),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz8),8) + SPACE(1) +;
        PADL("Total",8)


      *B608096,1 MMT 05/24/2007 fix bug of not displaying the Scale [End]
      FOR I = 1 TO 8
        Z = STR(I,1)
        laScalWs[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
      ENDFOR
      FOR I = 1 TO 8
        Z = STR(I,1)
        laScals[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
      ENDFOR

    ENDIF

    lcScale1 = &lcStyle..SCALE

  ELSE &&if don't print Size Scale
    IF llTextMode
      lcRet = SPACE(95)
      IF llShowCost
        lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Cost_Val
      ENDIF
      IF llShowSale
        lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Sales_Val
      ENDIF
    ELSE
      *-- we will let it old the scale.
      lcRet=''
    ENDIF
    lcScale1 = &lcStyle..SCALE
  ENDIF
ELSE
  lcRet = ''
ENDIF
*!*    IF llTextMode
*!*      lcRet = SPACE(15) + ;
*!*              PADL(ALLTRIM(&lcscal..Sz1),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz2),8) + SPACE(1) +;
*!*              PADL(ALLTRIM(&lcscal..Sz3),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz4),8) + SPACE(1) +;
*!*              PADL(ALLTRIM(&lcscal..Sz5),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz6),8) + SPACE(1) +;
*!*              PADL(ALLTRIM(&lcscal..Sz7),8) + SPACE(1) + PADL(ALLTRIM(&lcscal..Sz8),8) + SPACE(1) +;
*!*              PADL(LANG_ICSTYREP_Total,8)

*!*      IF llShowCost
*!*        lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Cost_Val
*!*      ENDIF
*!*      IF llShowSale
*!*        lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Sales_Val
*!*      ENDIF
*!*    ELSE
*!*      *-- we will let it old the scale.
*!*      lcRet=''
*!*      FOR I = 1 To 8
*!*        Z = STR(I,1)
*!*        laScalWs[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
*!*      ENDFOR
*!*      FOR I = 1 To 8
*!*        Z = STR(I,1)
*!*        laScals[I] = PADL(ALLTRIM(&lcscal..Sz&z),5)
*!*      ENDFOR

*!*    ENDIF

*!*    lcScale1 = &lcStyle..SCALE

*!*  ELSE
*!*    lcRet = ''
*!*  ENDIF

*E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [End]
RETURN lcRet


*!*************************************************************
*! Name      : lfvSalVal
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Validation If user does not have cost previlages,
*!           : title is to print price (Y/N).
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSalVal()
*!*************************************************************
FUNCTION lfvSalVal
IF llRpSalVal
  lcRpShow = "S"
ELSE
  lcRpShow = "N"
ENDIF
=lfvPrint()  && Call normal validate.
*-- end of lfvSalVal.

*!*************************************************************
*! Name      : lfFillDumy
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 03/17/2005
*! Purpose   : Fill OTS Array to show in comboBox
*!*************************************************************
*! Parameters: ...
*!*************************************************************
*! Returns   : ...
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfFillDumy

*Positive|Negative|Both|Net|All~P|N|B|A|L
IF lcRpSortBy $ 'SW' AND ALEN(laRPRepTar,1) = 1 AND (ASCAN(laRPRepTar,LANG_ICSTYREP_OTS) > 0  .OR.  ASCAN(laRPRepTar,LANG_ICSTYREP_Imm_OTS) > 0)
  DIMENSION laOTSDesc[5,1],laOTSTarget[5,1]
  laOTSDesc[1] = LANG_ICSTYREP_Positive
  laOTSDesc[2] = LANG_ICSTYREP_Negative
  laOTSDesc[3] = LANG_ICSTYREP_Both
  laOTSDesc[4] = LANG_ICSTYREP_Net
  laOTSDesc[5] = LANG_ICSTYREP_All

  laOTSTarget[1] = 'P'
  laOTSTarget[2] = 'N'
  laOTSTarget[3] = 'B'
  laOTSTarget[4] = 'A'
  laOTSTarget[5] = 'L'
ELSE
  DIMENSION laOTSDesc[3,1],laOTSTarget[3,1]
  laOTSDesc[1] = LANG_ICSTYREP_Positive
  laOTSDesc[2] = LANG_ICSTYREP_Negative
  laOTSDesc[3] = LANG_ICSTYREP_Net

  laOTSTarget[1] = 'P'
  laOTSTarget[2] = 'N'
  laOTSTarget[3] = 'A'
  IF lcRpOtsSig $'BL'
    lcRpOtsSig='A'
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfPrtConfigTtl
*! Developer : Heba Mohamed Amin  (HMA)
*! Date      : 03/27/2005
*! Purpose   : to print (Configuration) Label
*!*************************************************************
*! Called from : FRX file(ICSTYBYW)
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrtConfigTtl()
*!*************************************************************
FUNCTION lfPrtConfigTtl

lcPrtConfigTtl =""
IF EMPTY(cEndMaj) AND llRpConfig AND !EMPTY(EVAL(lcDummy+".Label")) AND !EMPTY(EVAL(lcStyDye+".Dyelot")) AND EVAL(lcTrns+".HasDye") AND !llPrtConfigTtl
  llPrtConfigTtl=.T.
  RETURN LANG_ICSTYREP_Configuration
ELSE
  llPrtConfigTtl=.F.
  RETURN ''
ENDIF



*!*************************************************************
*! Name      : lfPrnDyeItm
*! Developer : Heba Mohamed Amin  (HMA)
*! Date      : 02/09/2005
*! Purpose   : to print the Configuration lines
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrnStyItm()
*!*************************************************************

FUNCTION lfPrnDyeItm
PARAMETERS lcFld

PRIVATE lcFld,lnRet,lcUseFile,lcStyDyeVal,lcStyDyeTot,lcStyDyeVal1,lcStyDyeTot1,lcStyDyeVal2,lcStyDyeTot2
STORE '' TO lcStyDyeVal,lcStyDyeTot,lcStyDyeVal1,lcStyDyeTot1,lcStyDyeVal2,lcStyDyeTot2
DO CASE
  *------------------------------------- Direct Cases ----------------------
  *-- Wip Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'WIP'
  lcStyDyeVal="WIP"
  lcStyDyeTot="TOTWIP"

  *-- Stock on hand Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'SOH'
  lcStyDyeVal="STK"
  lcStyDyeTot="TOTSTK"

  *-- Order Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'ORD'
  lcStyDyeVal="ORD"
  lcStyDyeTot="TOTORD"

  *-- Work order Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'WORD'
  lcStyDyeVal="NWO"
  lcStyDyeTot="NTOTWO"

  *-- Intransit Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'INT'
  lcStyDyeVal="INTRANS"
  lcStyDyeTot="TOTINTRN"

  *-- Shipped Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'SHP'
  lcStyDyeVal="SHP"
  lcStyDyeTot="TOTSHP"

  *-- Credit memo return Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'RET'
  lcStyDyeVal="RET"
  lcStyDyeTot="TOTRET"

  *-- Return Authorization Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'RETA'
  lcStyDyeVal="RA"
  lcStyDyeTot="TOTRA"

  *-- Allocation Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'ALO'
  lcStyDyeVal="ALO"
  lcStyDyeTot="TOTALO"

  *------------------------------------- InDirect Cases ----------------------

  *-- UnAllocation Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'UALO'
  lcStyDyeVal1="STK"
  lcStyDyeTot1="TOTSTK"
  lcStyDyeVal2="ALO"
  lcStyDyeTot2="TOTALO"

  *-- Imm. OTS  Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'IOTS'
  lcStyDyeVal1="STK"
  lcStyDyeTot1="TOTSTK"
  lcStyDyeVal2="ORD"
  lcStyDyeTot2="TOTORD"

  *-- Booked  Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'BOK'
  lcStyDyeVal1="SHP"
  lcStyDyeTot1="TOTSHP"
  lcStyDyeVal2="ORD"
  lcStyDyeTot2="TOTORD"

  *-- OTS  Case...
CASE ALLTRIM(&lcDummy..ITEM) == 'OTS'
  lcStyDyeVal="STK"
  lcStyDyeTot="TOTSTK"
  lcStyDyeVal1="ORD"
  lcStyDyeTot1="TOTORD"
  lcStyDyeVal2="WIP"
  lcStyDyeTot2="TOTWIP"

ENDCASE

lcUseFile = IIF(lcRpSortBy == "W",lcTrns,lcStyTmp)
lnRet = ''
IF NotScale = 'Y'
  IF &lcscal..CNT >= VAL(lcFld) OR !EMPTY(&lcUseFile..cEndMaj+&lcUseFile..cEndSort+&lcUseFile..cEndRep)
    IF !EMPTY(&lcDummy..ITEM)
      IF !EMPTY(&lcStyDye..Dyelot)  && all cases for sorting
        IF !INLIST(lcFld,'Price','Cost')
          IF !INLIST(ALLTRIM(&lcDummy..ITEM),'OTS','IOTS','BOK','UALO')
            lnRet = EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot,lcStyDyeVal+lcFld))
          ELSE
            DO CASE
            CASE ALLTRIM(&lcDummy..ITEM) == 'OTS'
              lnRet = EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot,lcStyDyeVal+lcFld))- EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot1,lcStyDyeVal1+lcFld)) + EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot2,lcStyDyeVal2+lcFld))
            CASE ALLTRIM(&lcDummy..ITEM) == 'BOK'
              lnRet = EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot1,lcStyDyeVal1+lcFld))+ EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot2,lcStyDyeVal2+lcFld))
            CASE INLIST(ALLTRIM(&lcDummy..ITEM),'UALO','IOTS')
              lnRet = EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot1,lcStyDyeVal1+lcFld))- EVAL(lcStyDye+"."+IIF(lcFld='Tot',lcStyDyeTot2,lcStyDyeVal2+lcFld))
            ENDCASE
            IF ALLTRIM(&lcDummy..ITEM) $ 'IOTS' AND EMPTY(cEndMaj)
              IF !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND lPrnIOTS) OR ;
                  (ALLTRIM(&lcDummy..ITEM) = 'OTS' AND lPrnOTS) )
                lnRet = ''
              ELSE
                IF BETWEEN(lcFld,"1","8")
                  DO CASE
                  CASE lcRPOTSSig = 'P' AND lnRet <= 0
                    lnRet = ''
                  CASE lcRPOTSSig = 'N' AND lnRet >= 0
                    lnRet = ''
                  ENDCASE
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSE
          IF lcFld = 'Price'
            IF EMPTY(cEndMaj) AND !EMPTY(EVAL(lcStyDye+'.Dyelot'))
              IF !INLIST(ALLTRIM(&lcDummy..ITEM),'OTS','IOTS','UALO')
                lnRet = EVAL(lcStyDye+'.'+lcStyDyeTot)*EVAL(lcStyle+'.PriceA')
              ELSE
                DO CASE
                CASE ALLTRIM(&lcDummy..ITEM) == 'OTS'
                  lnRet = (EVAL(lcStyDye+"."+lcStyDyeTot)- EVAL(lcStyDye+"."+lcStyDyeTot1) + EVAL(lcStyDye+"."+lcStyDyeTot2))* EVAL(lcStyle+'.PriceA')
                CASE INLIST(ALLTRIM(&lcDummy..ITEM),'UALO','IOTS')
                  lnRet = (EVAL(lcStyDye+"."+lcStyDyeTot1)- EVAL(lcStyDye+"."+lcStyDyeTot2))* EVAL(lcStyle+'.PriceA')
                ENDCASE
                IF ALLTRIM(&lcDummy..ITEM) $ 'IOTS' AND EMPTY(cEndMaj)
                  IF !( (ALLTRIM(&lcDummy..ITEM) = 'IOTS' AND lPrnIOTS) OR ;
                      (ALLTRIM(&lcDummy..ITEM) = 'OTS' AND lPrnOTS) )
                    lnRet = ''
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              lnRet = ''
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

RETURN lnRet


*!*************************************************************
*! Name      : lfBldSqlCur
*! Developer : Heba Mohamed Amin  (HMA)
*! Date      : 04/04/2005
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
*! Name      : lfBldSqlCur  B608166
*! Developer : Ahmed Salah shalaby - SSH
*! Date      : 07/15/2007
*! Purpose   : check for each size not the sum of all sizes
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : = lfZeroStok()
*!*************************************************************
FUNCTION lfZeroStok
PRIVATE lnOldAls,ll2Return,lnInd,lcInd,lnValue

lnOldAls = SELECT(0)
ll2Return=.T.
lnValue=0
SELECT(lcTrns)
FOR lnInd=1 TO 8
  lcInd = ALLTRIM(STR(lnInd))
  lnValue=lnValue+EVAL(lcTrns+".Wip"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".SOH"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".PLA"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".OTS"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".IOTS"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".BOK"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".SHP"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".Ret"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".RetA"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".Alo"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".UAlo"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".Int"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".WOrd"+lcInd)
  lnValue=lnValue+EVAL(lcTrns+".Ord"+lcInd)
  ll2Return = ll2Return AND (lnValue=0)
ENDFOR
SELECT(lnOldAls)
RETURN(ll2Return)
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*:**************************************************************************
*:* Name        : lfGetnext
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/22/2007
*:* Purpose     : Get the next line "NotScale" status
*:***************************************************************************
*:* Called from : icstybyo.frx
*:***************************************************************************
*B608163,1
FUNCTION lfGetnext
SKIP
llGetNext = NotScale == 'Y'
lcGetNext = NotScale
SKIP -1
RETURN llGetNext
*-- end of lfGetnext.
*:**************************************************************************
*:* Name        : lfTotSz
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/06/2007
*:* Purpose     : used to reduce the length of line criteria in the frx
*:***************************************************************************
*:* Called from : ICSTYBYS.FRX
*:***************************************************************************
*B608189,1
FUNCTION lfTotSz
LOCAL llRet
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*llRet = (!llRpShwZer AND lnSize9=0) OR (llPrintClr AND !llRpWhDeta AND EMPTY(Dyelot))
llRet = (!llRpShwZer AND lnSize9=0 AND !llChkAllSz) OR (llPrintClr AND !llRpWhDeta AND EMPTY(Dyelot))
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
RETURN llRet
*-- end of lfTotSz.

*:**************************************************************************
*:* Name        : lfWhCr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/06/2007
*:* Purpose     : used to reduce the length of line criteria in the frx
*:***************************************************************************
*:* Called from : ICSTYBYL.FRX
*:***************************************************************************
*B608189,1
FUNCTION lfWhCr
LOCAL llRet
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*llRet = (!llRpShwZer AND lnSize9=0) OR (!llRpPrnDye AND !EMPTY(Dyelot))
llRet = (!llRpShwZer AND lnSize9=0 AND !llChkAllSz) OR (!llRpPrnDye AND !EMPTY(Dyelot))
RETURN llRet
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*************************************************************
*! Name      : lfOTSCalcW
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/19/2008
*! Purpose   : to caculate OTS in case of print location detail is YES
*!*************************************************************
*! Called from : FRX files
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOTSCalcW()
*!*************************************************************

FUNCTION lfOTSCalcW
PARAMETER lcItm
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*!*	IF (lcRPOTSSig ='P' AND IIF(lcItm='OTS' , TotOTS,TotIOTS)  >= lnRPOTSMin) OR ;
*!*	   (lcRPOTSSig ='N' AND IIF(lcItm='OTS' , TotOTS,TotIOTS) <= lnRPOTSMin)
IF lcRPOTSSig ='A' OR ((lcRPOTSSig $ 'LBP' AND IIF(lcItm='OTS' , TotOTS,TotIOTS)  >= lnRPOTSMin) OR ;
    (lcRPOTSSig $'LBN' AND IIF(lcItm='OTS' , TotOTS,TotIOTS) <= lnRPOTSMax))
  *B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[ENd]
  DO CASE
  CASE lcItm = 'OTS'
    REPLACE lPrnOTS  WITH .T.
  CASE lcItm = 'IOTS'
    REPLACE lPrnIOTS WITH .T.
  ENDCASE
ELSE
  IF lcItm='OTS'
    REPLACE TotOTS WITH 0
  ELSE
    REPLACE TotIOTS WITH 0
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfChkTotal
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/19/2008
*! Purpose   : Check if Total is Zero
*!*************************************************************
FUNCTION lfChkTotal

PRIVATE lnOldAls,ll2Return,lnInd,lcInd,lnValue
ll2Return=.T.
lnValue=0
FOR lnInd=1 TO 8
  lcInd   =  ALLTRIM(STR(lnInd))
  lnValue = EVAL(ALLTRIM(&lcDummy..ITEM)+lcInd)
  ll2Return = ll2Return AND (lnValue=0)
ENDFOR
RETURN(ll2Return)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


*!*************************************************************
*! Name      : lfChkTotal
*! Developer : Tarek Ibrahim
*! Date      : 01/4/2011
*! Purpose   : Check if Total is Zero
*!*************************************************************
*E302831,1 TMI 01/04/2011 [Start] enable/disable the new option 'Print long description'
FUNCTION lfLongDscSup
LOCAL llEnable

llEnable = lcRpSortBy $ "WS" AND ALEN(laRPRepTar,1) = 1 AND !EMPTY(laRPRepTar)
loOGScroll.EnableObject('llRpLongDsc', llEnable )
*- End of lfLongDscSup.
*E302831,1 TMI 01/04/2011 [End  ]
