*!************************************************************************************************
*: Program file  : MAROLINV.PRG
*: Program desc. : MATERIAL INVENTORY REPORTS BY ROLL.
*:         System: ARIA APPAREL SERIES 4XP
*:         Module: MA
*:      Developer: Mariam Mazhar(MMT)
*:           Date: 02/22/2007
*!************************************************************************************************
*! Refer to (N000590)T20061227.0010
*!************************************************************************************************
*!Modifications :
*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*!************************************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include R:\ARIA4XP\REPORTS\MA\marolinv.H
*N000682,1 MMT 02/11/2013 Globalization changes[End]
*----Variables used in OG
*-- llRpOnHand  && Only Onhand Qty?
*-- llRpRolDet  && Print roll detail?
*-- llRpDyeDet  && Print dyelot detail?
*-- lcRpTitle   && Title.
*-- lcRpSort    && Sort By.
*--Have we got data matching the user's selection criteria.
*--Collect data only when the filter changes.
loogScroll.cCROrientation = 'P'
IF loOGScroll.llOgFltCh
  IF USED(lcFabTemp) AND RECCOUNT(lcFabTemp) > 0
    SELECT (lcFabTemp)
    ZAP
  ENDIF
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  *WAIT WINDOW 'Collecting data...' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_COLLECTDATA NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECTDATA,oAriaApplication.GetHeaderText("LANG_COLLECTDATA",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  =lfCollect()
ENDIF
SELECT (lcFabTemp)
GO TOP
IF EOF()
  *---Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*-- Start printing
DO gfDispRe WITH EVAL('lcRepName')
SET DEVICE TO SCREEN
RETURN
*--End of program MAROLINV

*!*************************************************************
*! Name      : lfFabSum
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/22/2007
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
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
*! Name      : lfWhenFunc
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/22/2007
*! Purpose   : Valid WHNE fulction.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
FUNCTION lfWhenFunc


=gfOpenTable('ITEM','STYLE','SH')
=gfOpenTable('ITEMLOC','STYDYE','SH')

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*=gfOpenTable('ROLLS','ROLLS','SH')
=gfOpenTable('ROLLS','ROLLITEM','SH')
*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

=gfOpenTable('ITEMJRNL','styinvjl','SH')
=gfOpenTable('POSHDR','POSHDR','SH')
=gfOpenTable('UOM','UOMCODE','SH')

=gfOpenFile(oAriaApplication.SysPath+'syccomp',oAriaApplication.SysPath+'Ccomp_id','SH')
=SEEK(oAriaApplication.ActiveCompanyID,'SycComp')


IF !llFirst
  lcSqlStatment   = "SELECT ItemLoc.STYLE,ItemLoc.TOTWIP  AS ONORDER,ItemLoc.TOTSTK AS ONHAND ,"+;
           "ItemLoc.TOTORD,WIP1,DYELOT,ITEM.CSTYMAJOR   FROM ItemLoc(INDEX = STYDYE),ITEM(INDEX = CSTYLE) WHERE ITEMLOC.dyelot ='' AND ITEMLOC.STYLE = ITEM.STYLE AND ITEMLOC.CINVTYPE = ITEM.CINVTYPE "
  lcSqlStatment   = lcSqlStatment   +" AND ITEM.CINVTYPE = 0002"
  lcTable ='ItemLoc'
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursorLoc,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                        'BROWSE',SET("DATASESSION"))

  IF lnConnectionHandlar >= 1
    SELECT(lcCursorLoc)
    lnBuffering = CURSORGETPROP("Buffering",lcCursorLoc)
     =CURSORSETPROP("Buffering",3,lcCursorLoc)
    SELECT (lcCursorLoc)
    INDEX ON CSTYMAJOR TAG &lcCursorLoc
    SET ORDER TO TAG &lcCursorLoc
    SET RELATION TO
    LOCATE
  ENDIF
  llFirst = .T.
ENDIF


*-- Create temp file
DIMENSION laStructArr [13,4]

laStructArr[1,1] = 'Fabric'
laStructArr[1,2] = 'C'

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*laStructArr[1,3] = 7
laStructArr[1,3] = 19
*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

laStructArr[1,4] = ''

laStructArr[2,1] = 'Color'
laStructArr[2,2] = 'C'
laStructArr[2,3] = 6
laStructArr[2,4] = ''

laStructArr[3,1] = 'Vendor'
laStructArr[3,2] = 'C'
laStructArr[3,3] = 8
laStructArr[3,4] = ''

laStructArr[4,1] = 'Desc'
laStructArr[4,2] = 'C'
laStructArr[4,3] = 20
laStructArr[4,4] = ''

laStructArr[5,1] = 'Width'
laStructArr[5,2] = 'C'
laStructArr[5,3] = 6
laStructArr[5,4] = ''

laStructArr[6,1] = 'Loc'
laStructArr[6,2] = 'C'
laStructArr[6,3] = 6
laStructArr[6,4] = ''

laStructArr[7,1] = 'OnHand'
laStructArr[7,2] = 'N'
laStructArr[7,3] = 12
laStructArr[7,4] = 3

laStructArr[8,1] = 'Uom'
laStructArr[8,2] = 'C'
laStructArr[8,3] = 3
laStructArr[8,4] = ''

laStructArr[9,1] = 'Cost'
laStructArr[9,2] = 'N'
laStructArr[9,3] = 10
laStructArr[9,4] = 3

laStructArr[10,1] = 'Value'
laStructArr[10,2] = 'N'
laStructArr[10,3] = 12
laStructArr[10,4] = 3

laStructArr[11,1] = 'RollID'
laStructArr[11,2] = 'C'
laStructArr[11,3] = 20
laStructArr[11,4] = ''

laStructArr[12,1] = 'Dyelot'
laStructArr[12,2] = 'C'
laStructArr[12,3] = 10
laStructArr[12,4] = ''

laStructArr[13,1] = 'ItemType'
laStructArr[13,2] = 'C'
laStructArr[13,3] = 6
laStructArr[13,4] = ''

= gfCrtTmp(lcFabTemp,@laStructArr)

SELECT (lcFabTemp)

INDEX ON Vendor    TAG Vendor   ADDITIVE
INDEX ON Loc       TAG Location ADDITIVE
INDEX ON ItemType  TAG ItemType ADDITIVE
INDEX ON Fabric    TAG Fabric   ADDITIVE

SELECT UOM
DIMENSION laStrucFile[1,18]
AFIELDS(laStrucFile)
= gfCrtTmp(lcUOMTemp,@laStrucFile,'cuomcode',lcUOMTemp)


*!*************************************************************
*! Name      : lfvSort
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/22/2007
*! Purpose   : Valid sort by option.
*!*************************************************************
*! Called from : Sort By
*!*************************************************************
FUNCTION lfvSort
lcIndName = IIF(lcRpSort = 'V','Vendor',IIF(lcRpSort = 'C','Fabric',IIF(lcRpSort = 'L','Location','ItemType')))
SELECT (lcFabTemp)
SET ORDER TO TAG lcIndName

*!*************************************************************
*! Name      : lfCollect
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/22/2007
*! Purpose   : Collect data.
*!*************************************************************
*! Called from : Sort By
*!*************************************************************
FUNCTION lfCollect

*Item
lcItemFile  = ''
llUseItem   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'FABRIC.FABRIC'),1)
IF lnPosition > 0
  lcItemFile = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  llUseItem   = IIF(!EMPTY(lcItemFile) .AND. USED(lcItemFile) .AND. RECCOUNT(lcItemFile)>0,.T.,.F.)
ENDIF
IF llUseItem
  SELECT(lcItemFile)
  LOCATE
  IF EOF()
    llUseItem = .F.
  ENDIF
ENDIF


*Vendor
lcVendFile  = ''
llUseVend   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'APVENDOR.CVENDCODE'),1)
IF lnPosition > 0
  lcVendFile = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  llUseVend  = IIF(!EMPTY(lcVendFile) .AND. USED(lcVendFile) .AND. RECCOUNT(lcVendFile)>0,.T.,.F.)
ENDIF
IF llUseVend
  SELECT(lcVendFile)
  LOCATE
  IF EOF()
    llUseVend  = .F.
  ENDIF
ENDIF


*Color
llUseClr = .F.
lnClrPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'FABRIC.COLOR'),1)
IF lnClrPos > 0
  lcClrStr = LOOGSCROLL.laOGFXFlt[lnClrPos,6]
  lcClrFile = loOGScroll.gfTempName()
  llUseClr = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'CSTYCLR',lcClrFile)
ENDIF

lnMajLen=LEN(gfItemMask('PM','', "0002"))
lnClrLen=LEN(gfitemmask("PN", "", "0002"))
IF llUseItem
  SELECT Item
  =gfSetOrder('Style')
  SELECT(lcItemFile)
  SCAN
    IF gfSEEK('0002'+SUBSTR(&lcItemFile..CstyMajor,1,lnMajLen),'ITEM','Style')
     SELECT Item
     SCAN REST WHILE cinvtype+ style ='0002'+SUBSTR(&lcItemFile..CstyMajor,1,lnMajLen) ;
         FOR IIF(llUseClr,SEEK(RIGHT(ITEM.Style,lnClrLen),lcClrFile),.T.)

       IF SEEK('0002'+ITEM.STYLE,'ITEMLOC') OR (gfSeek('0002'+SUBSTR(&lcItemFile..CstyMajor,1,lnMajLen),'ITEMLOC','stydye') AND SEEK('0002'+ITEM.STYLE,'ITEMLOC'))
         SELECT ITEMLOC
         SCAN REST WHILE cinvtype+ style+ cwarecode+ dyelot ='0002'+ITEM.STYLE FOR ;
           IIF(llRpDyeDet AND ITEM.cDye_Flg = 'Y',!EMPTY(Dyelot),EMPTY(Dyelot))
           WAIT WINDOW 'Collecting data..please wait.. Fabric\color '+ITEMLOC.Style NOWAIT

			*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	           IF llRpRolDet AND gfSEEK(PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7)+RIGHT(ITEMLoc.Style,lnClrLen)+;
*!*	                         cWareCode+Dyelot,'ROLLS')
*!*	
*!*	             lcKey = PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7)+RIGHT(ITEMLoc.Style,lnClrLen)+cWareCode+Dyelot
           IF llRpRolDet AND gfSEEK(ITEMLOC.Style+cWareCode+Dyelot,'ROLLS')

             lcKey = ITEMLOC.Style+cWareCode+Dyelot
			*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
			
             SELECT ROLLS

             *!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	             SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession = lcKey;
*!*	                FOR (IIF(llRpOnHand,nQtyBal <>0,.T.)  AND TRANCD ='1' )
             SCAN REST WHILE Style+cwarecode+dyelot+crollid+trancd+crsession = lcKey;
                FOR (IIF(llRpOnHand,nQtyBal <>0,.T.)  AND TRANCD ='1' )
             *!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

               IF llUseVend

                IF gfSQLRUN("SELECT ItemJrnl.*,Poshdr.Vendor as POVend FROM POSHDR "+;
                          " INNER JOIN Itemjrnl ON Poshdr.Cbusdocu = 'P' AND Poshdr.Cstytype = 'M'"+;
                          " AND Poshdr.po = ITEMJRNL.CTRCODE WHERE ITEMJRNL.cinvtype = '0002' AND "+ ;
                          " ITEMJRNL.style = '"+ITEM.Style+"' AND ITEMJRNL.cwarecode = '"+ITEMLOC.CWARECODE+"' AND "+;
                          " ITEMJRNL.csession = '"+ ROLLS.crsession +"'",'ITEMJRNL') AND SEEK(ITEMJRNL.POVend ,lcVendFile)

                  IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
                    SELECT UOM
                    SCATTER MEMO MEMVAR
                    SELECT(lcUOMTemp)
                    APPEND BLANK
                    GATHER MEMO MEMVAR
                    lcUom = M.Cuom_V
                    lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
                  ELSE
                    IF SEEK(ITEM.cconvbuy,lcUOMTemp)
                      lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
                      lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
                    ENDIF
                  ENDIF

         			*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	                  INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      ,;
*!*	                                       Width      ,Loc             , OnHand ,Uom          , Cost         ,;
*!*	                                       Value                , RollID            , DyeLot , ItemType );
*!*	                               VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                       ITEMJRNL.POVend,ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,;
*!*	                                       lcUom ,lnCostUSE ,Rolls.nQtyBal*lnCostUSE,Rolls.cRollID,Rolls.DyeLot,;
*!*	                                       ITEM.Item_Type)
                  INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      ,;
                                       Width      ,Loc             , OnHand ,Uom          , Cost         ,;
                                       Value                , RollID            , DyeLot , ItemType );
                               VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                       ITEMJRNL.POVend,ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,;
                                       lcUom ,lnCostUSE ,Rolls.nQtyBal*lnCostUSE,Rolls.cRollID,Rolls.DyeLot,;
                                       ITEM.Item_Type)
					*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

            ENDIF
          ELSE
            IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
              SELECT UOM
              SCATTER MEMO MEMVAR
              SELECT(lcUOMTemp)
              APPEND BLANK
              GATHER MEMO MEMVAR
              lcUom = M.Cuom_V
              lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
            ELSE
              IF SEEK(ITEM.cconvbuy,lcUOMTemp)
                lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
                lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
              ENDIF
            ENDIF

            =gfSQLRUN("SELECT ItemJrnl.*,Poshdr.Vendor as POVend FROM POSHDR "+;
                          " INNER JOIN Itemjrnl ON Poshdr.Cbusdocu = 'P' AND Poshdr.Cstytype = 'M'"+;
                          " AND Poshdr.po = ITEMJRNL.CTRCODE WHERE ITEMJRNL.cinvtype = '0002' AND "+ ;
                          " ITEMJRNL.style = '"+ITEM.Style+"' AND ITEMJRNL.cwarecode = '"+ITEMLOC.CWARECODE+"' AND "+;
                          " ITEMJRNL.csession = '"+ ROLLS.crsession +"'",'ITEMJRNL')

             SELECT ITEMJRNL
             LOCATE
             IF !EOF()
               lcVendor = ITEMJRNL.POVend
             ELSE
               lcVendor = ITEM.VENDOR
             ENDIF

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	             INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , ;
*!*	                                      Width      ,Loc             , OnHand ,Uom          , Cost         ,;
*!*	                                      Value                , RollID            , DyeLot , ItemType );
*!*	                              VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                      lcVendor,;
*!*	                                      ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,lcUom,lnCostUSE ,;
*!*	                                      Rolls.nQtyBal*lnCostUSE ,Rolls.cRollID,Rolls.DyeLot,ITEM.Item_Type)
             INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , ;
                                      Width      ,Loc             , OnHand ,Uom          , Cost         ,;
                                      Value                , RollID            , DyeLot , ItemType );
                              VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                      lcVendor,;
                                      ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,lcUom,lnCostUSE ,;
                                      Rolls.nQtyBal*lnCostUSE ,Rolls.cRollID,Rolls.DyeLot,ITEM.Item_Type)

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          ENDIF
         ENDSCAN
      ELSE
        IF !llRpOnHand OR ITEMLOC.TOTSTK <>0
          IF llUseVend
            IF SEEK(ITEM.Vendor,lcVendFile)
              IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
                SELECT UOM
                SCATTER MEMO MEMVAR
                SELECT(lcUOMTemp)
                APPEND BLANK
                GATHER MEMO MEMVAR
                lcUom = M.Cuom_V
                lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
              ELSE
                IF SEEK(ITEM.cconvbuy,lcUOMTemp)
                  lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
                  lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
                ENDIF
              ENDIF
              *!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	              INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      ,;
*!*	                                       Width      ,Loc             , OnHand       ,Uom          ,;
*!*	                                       Cost         , Value                      , RollID , DyeLot      ,;
*!*	                                        ItemType );
*!*	                               VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                        ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,itemloc.totstk,;
*!*	                                        lcUom,lnCostUSE ,itemloc.totstk*lnCostUSE ,SPACE(20),itemloc.DyeLot,;
*!*	                                        item.Item_Type)
              INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      ,;
                                       Width      ,Loc             , OnHand       ,Uom          ,;
                                       Cost         , Value                      , RollID , DyeLot      ,;
                                        ItemType );
                               VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                        ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,itemloc.totstk,;
                                        lcUom,lnCostUSE ,itemloc.totstk*lnCostUSE ,SPACE(20),itemloc.DyeLot,;
                                        item.Item_Type)

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
            ENDIF
          ELSE
            IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
              SELECT UOM
              SCATTER MEMO MEMVAR
              SELECT(lcUOMTemp)
              APPEND BLANK
              GATHER MEMO MEMVAR
              lcUom = M.Cuom_V
              lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
            ELSE
              IF SEEK(ITEM.cconvbuy,lcUOMTemp)
                lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
                lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
              ENDIF
            ENDIF

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	            INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      ,;
*!*	                                      Width      ,Loc             , OnHand       ,Uom          ,;
*!*	                                     Cost         , Value                      , RollID , DyeLot      ,;
*!*	                                      ItemType );
*!*	                             VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                     ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,;
*!*	                                     itemloc.totstk,lcUom,lnCostUSE ,itemloc.totstk*lnCostUSE ,;
*!*	                                     SPACE(20),itemloc.DyeLot,item.Item_Type)
            INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      ,;
                                      Width      ,Loc             , OnHand       ,Uom          ,;
                                     Cost         , Value                      , RollID , DyeLot      ,;
                                      ItemType );
                             VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                     ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,;
                                     itemloc.totstk,lcUom,lnCostUSE ,itemloc.totstk*lnCostUSE ,;
                                     SPACE(20),itemloc.DyeLot,item.Item_Type)

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

               ENDIF
             ENDIF
         ENDIF
         ENDSCAN
       ENDIF
     ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  IF gfSEEK('0002','ITEM','Style') AND  gfSeek('0002','ITEMLOC','stydye')

     SELECT Item
     SCAN REST WHILE cinvtype+ style ='0002' ;
         FOR IIF(llUseClr,SEEK(RIGHT(ITEM.Style,lnClrLen),lcClrFile),.T.)
       IF SEEK('0002'+ITEM.STYLE,'ITEMLOC')
         SELECT ITEMLOC
         SCAN REST WHILE cinvtype+ style+ cwarecode+ dyelot ='0002'+ITEM.STYLE FOR ;
           IIF(llRpDyeDet AND ITEM.cDye_Flg = 'Y',!EMPTY(Dyelot),EMPTY(Dyelot))

           WAIT WINDOW 'Collecting data..please wait.. Fabric\color '+ITEMLOC.Style NOWAIT

           *!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	           IF llRpRolDet AND gfSEEK(PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7)+RIGHT(ITEMLoc.Style,lnClrLen)+cWareCode+Dyelot,'ROLLS')
*!*	             lcKey = PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7)+RIGHT(ITEMLoc.Style,lnClrLen)+cWareCode+Dyelot
           IF llRpRolDet AND gfSEEK(ITEMLOC.Style+cWareCode+Dyelot,'ROLLS')
             lcKey = ITEMLOC.Style+cWareCode+Dyelot
		*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

             SELECT ROLLS

             *!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*             SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession = lcKey FOR (IIF(llRpOnHand,nQtyBal <>0,.T.)  AND TRANCD ='1' )
             SCAN REST WHILE Style+cwarecode+dyelot+crollid+trancd+crsession = lcKey FOR (IIF(llRpOnHand,nQtyBal <>0,.T.)  AND TRANCD ='1' )
             *!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

               IF llUseVend
                 IF gfSQLRUN("SELECT ItemJrnl.*,Poshdr.Vendor as POVend FROM POSHDR "+;
                          " INNER JOIN Itemjrnl ON Poshdr.Cbusdocu = 'P' AND Poshdr.Cstytype = 'M'"+;
                          " AND Poshdr.po = ITEMJRNL.CTRCODE WHERE ITEMJRNL.cinvtype = '0002' AND "+ ;
                          " ITEMJRNL.style = '"+ITEM.Style+"' AND ITEMJRNL.cwarecode = '"+ITEMLOC.CWARECODE+"' AND "+;
                          " ITEMJRNL.csession = '"+ ROLLS.crsession +"'",'ITEMJRNL') AND SEEK(ITEMJRNL.POVend ,lcVendFile)


                  IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
                    SELECT UOM
                    SCATTER MEMO MEMVAR
                    SELECT(lcUOMTemp)
                    APPEND BLANK
                    GATHER MEMO MEMVAR
                    lcUom = M.Cuom_V
                    lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
                  ELSE
                    IF SEEK(ITEM.cconvbuy,lcUOMTemp)
                      lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
                      lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
                    ENDIF
                  ENDIF

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	                 INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , ;
*!*	                                          Width      ,Loc             , OnHand ,Uom          , Cost         ,;
*!*	                                          Value                , RollID            , DyeLot , ItemType );
*!*	                                  VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                          ITEMJRNL.POVend,ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,;
*!*	                                          lcUom ,lnCostUSE,Rolls.nQtyBal*lnCostUSE,Rolls.cRollID,Rolls.DyeLot,;
*!*	                                          ITEM.Item_Type)
                 INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , ;
                                          Width      ,Loc             , OnHand ,Uom          , Cost         ,;
                                          Value                , RollID            , DyeLot , ItemType );
                                  VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                          ITEMJRNL.POVend,ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,;
                                          lcUom ,lnCostUSE,Rolls.nQtyBal*lnCostUSE,Rolls.cRollID,Rolls.DyeLot,;
                                          ITEM.Item_Type)
*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]


          ENDIF
        ELSE
          IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
            SELECT UOM
            SCATTER MEMO MEMVAR
            SELECT(lcUOMTemp)
            APPEND BLANK
            GATHER MEMO MEMVAR
            lcUom = M.Cuom_V
            lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
          ELSE
            IF SEEK(ITEM.cconvbuy,lcUOMTemp)
              lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
              lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
            ENDIF
          ENDIF

          =gfSQLRUN("SELECT ItemJrnl.*,Poshdr.Vendor as POVend FROM POSHDR "+;
                   " INNER JOIN Itemjrnl ON Poshdr.Cbusdocu = 'P' AND Poshdr.Cstytype = 'M'"+;
                   " AND Poshdr.po = ITEMJRNL.CTRCODE WHERE ITEMJRNL.cinvtype = '0002' AND "+ ;
                   " ITEMJRNL.style = '"+ITEM.Style+"' AND ITEMJRNL.cwarecode = '"+ITEMLOC.CWARECODE+"' AND "+;
                   " ITEMJRNL.csession = '"+ ROLLS.crsession +"'",'ITEMJRNL')

             SELECT ITEMJRNL
             LOCATE
             IF !EOF()
               lcVendor = ITEMJRNL.POVend
             ELSE
               lcVendor = ITEM.VENDOR
             ENDIF

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	             INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,;
*!*	                                      Loc             , OnHand ,Uom          , Cost         , Value                , ;
*!*	                                      RollID            , DyeLot , ItemType );
*!*	                              VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                      lcVendor,ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,;
*!*	                                      lcUom ,lnCostUSE,Rolls.nQtyBal*lnCostUSE,Rolls.cRollID,Rolls.DyeLot,ITEM.Item_Type)
             INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,;
                                      Loc             , OnHand ,Uom          , Cost         , Value                , ;
                                      RollID            , DyeLot , ItemType );
                              VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                      lcVendor,ITEM.Desc,ITEM.CITEMFLD1,ITEMLOC.cWarecode,Rolls.nQtyBal ,;
                                      lcUom ,lnCostUSE,Rolls.nQtyBal*lnCostUSE,Rolls.cRollID,Rolls.DyeLot,ITEM.Item_Type)

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
            ENDIF



           ENDSCAN
           ELSE
             IF !llRpOnHand OR ITEMLOC.TOTSTK <>0
             IF llUseVend
               IF SEEK(ITEM.Vendor,lcVendFile)
                  IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
                    SELECT UOM
                    SCATTER MEMO MEMVAR
                    SELECT(lcUOMTemp)
                    APPEND BLANK
                    GATHER MEMO MEMVAR
                    lcUom = M.Cuom_V
                    lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
                  ELSE
                    IF SEEK(ITEM.cconvbuy,lcUOMTemp)
                      lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
                      lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
                    ENDIF
                  ENDIF

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	                INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,;
*!*	                                          Loc             , OnHand       ,Uom          , Cost         ,;
*!*	                                          Value                      , RollID , DyeLot      , ItemType );
*!*	                                  VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                          ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,itemloc.totstk,;
*!*	                                          lcUom ,lnCostUSE,itemloc.totstk*lnCostUSE,SPACE(20),itemloc.DyeLot,;
*!*	                                          item.Item_Type)
                INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,;
                                          Loc             , OnHand       ,Uom          , Cost         ,;
                                          Value                      , RollID , DyeLot      , ItemType );
                                  VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                          ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,itemloc.totstk,;
                                          lcUom ,lnCostUSE,itemloc.totstk*lnCostUSE,SPACE(20),itemloc.DyeLot,;
                                          item.Item_Type)
*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
               ENDIF
             ELSE
                  IF !SEEK(ITEM.cconvbuy,lcUOMTemp) AND gfSeek(ITEM.cconvbuy,'UOM')
                    SELECT UOM
                    SCATTER MEMO MEMVAR
                    SELECT(lcUOMTemp)
                    APPEND BLANK
                    GATHER MEMO MEMVAR
                    lcUom = M.Cuom_V
                    lnCostUSE = ROUND(ITEM.TOTCOST/M.nConf,3)
                  ELSE
                    IF SEEK(ITEM.cconvbuy,lcUOMTemp)
                      lcUom = EVALUATE(lcUOMTemp +'.Cuom_V')
                      lnCostUSE = ROUND(ITEM.TOTCOST/EVALUATE(lcUOMTemp +'.nConf'),3)
                    ENDIF
                  ENDIF


*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	               INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,;
*!*	                                        Loc             , OnHand       ,Uom          , Cost         ,;
*!*	                                        Value                      , RollID , DyeLot      , ItemType );
*!*	                                VALUES (PADR(SUBSTR(ITEMLOC.Style,1,lnMajLen),7),RIGHT(ITEMLoc.Style,lnClrLen),;
*!*	                                        ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,itemloc.totstk,;
*!*	                                        lcUom ,lnCostUSE,itemloc.totstk*lnCostUSE,SPACE(20),itemloc.DyeLot,;
*!*	                                        item.Item_Type)
               INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,;
                                        Loc             , OnHand       ,Uom          , Cost         ,;
                                        Value                      , RollID , DyeLot      , ItemType );
                                VALUES (SUBSTR(ITEMLOC.Style,1,lnMajLen),RIGHT(ITEMLoc.Style,lnClrLen),;
                                        ITEM.Vendor,ITEM.Desc,ITEM.CITEMFLD1,itemloc.cWarecode,itemloc.totstk,;
                                        lcUom ,lnCostUSE,itemloc.totstk*lnCostUSE,SPACE(20),itemloc.DyeLot,;
                                        item.Item_Type)

*!B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

             ENDIF
           ENDIF
         ENDIF
       ENDSCAN
     ENDIF
   ENDSCAN
 ENDIF
ENDIF




*!*************************************************************
*! Name      : lfsumfab1
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/22/2007
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
FUNCTION lfsumfab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias
lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcCursorLoc)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcCursorLoc)
  LOCATE
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE cstymajor=lcFab AND DYELOT =''
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF

SELECT(lnAlias)

RETURN INT(lnTotcomp)
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/22/2007
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE
  CASE   ALLTRIM(lcFieldName) = 'CSTYCLR'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0

ENDCASE
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.



