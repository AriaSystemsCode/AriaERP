*:***********************************************************************************
*:           File: ICUPDSV.PRG                                                     :*
*:         System: ARIA APPAREL SYSTEM 4XP                                         :*
*:         Module: Inventory Control, Material                                     :*
*:        Program: Adjust Stock Value.                                             :*
*:         Author: Mariam Mazhar (MMT)                                             :*
*:           Date: 04/23/2008   B608534                                            :*
*:***********************************************************************************
*: Passed Parameters  :                                                            :*
*:       lcItemType : Item Type '0001 for Style and 0002 for Material'             :*
*:       lcItemCode : Item Code "Style Field 'Style'" or "Fabric+Color 'Material'" :*
*:       lcColorFil : Item Colors Temp Data Cursor                                 :*
*:       llNoScreen : Update Stock without displaying Screen.                      :*
*:       laCostFld  : Cost Array
*: 		 loParForm	: Form Called Program
*:***********************************************************************************
*: Modification       :
*: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[T20081003.0002]
*! B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[T20081128.0001]
*! B608975,1 MMT 08/24/2009 Fix bug of error while updating while adding new color to fabric{T20090731.0009}
*! B609439,1 TMI 10/20/2010 Error in MF Style Cost Sheet (Warehous is not found)        [T20100920.0022 ]
*! B609440,1 TMI 10/20/2010 Error (Variable'LNOLDCOST'is not found) when try to save on a style [T20101018.0011]
*! E303956,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL 
*! E304012,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL 
*! E303956,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL 
*! B611781,1 ES 06/03/2019 -  When user run the Data integrity check for Inventory control module, he got many record has incorrect stock values [T20181017.0003]
*! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement]
*:***********************************************************************************
#INCLUDE r:\Aria4XP\prgs\icupdsv.h

*! B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[Start]
*PARAMETERS lcItemType, lcItemCode, lcColorFil, llNoScreen , laCostFld,loParForm
PARAMETERS lcItemType, lcItemCode, lcColorFil, llNoScreen , laCostFld,loParForm,llFromCostSheet
*! B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]

PRIVATE lcItemCode, lcItemType, lnNewCost, lnOldCost
STORE ''  TO lcGlFYear,lcGlPeriod
STORE ' ' TO lcStyHdr,lcStyPict,lcStyCst
STORE ''  TO lcAdjLink
STORE .F. to llDyeLvl, llDyelot, llMultiWH, llOpStyDye, llOpFabDye, llOpStyJl,  llOpMatJl
STORE ''  TO lcItemDye

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRefer      = LANG_UPDATE_STK   && Reference to update tables with
lcRefer      = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_STK,loFormSet.GetHeaderText("LANG_UPDATE_STK",loFormSet.HeaderAlias))   && Reference to update tables with
*N000682,1 11/20/2012 MMT Globlization changes[End]


llContinSave = .T.                       && Don't continue saving of Style or Fabric if Update failed
lcTmpAdj     = gfTempName()              && Temp Cursor for Adjustment file

llGlLink  = gfGetMemVar('M_Link_GL')  ='Y'
llMultiWH = gfGetMemVar('M_WareHouse')='Y'

IF llGlLink
  =gfOpenTable(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO (oAriaApplication.WorkDir+lcTmpGlDt)
  USE (oAriaApplication.WorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
ENDIF

DO CASE
  CASE lcItemType = '0001'
    lcItem    = 'STYLE'
    lcItemDye = 'STYDYE'
    =gfOpenTable(oAriaApplication.DataDir+"INVTADJ", "INVTADJ", "SH", @lcTmpAdj, .T.)

    IF !Used('StyInvJL')
    *E304012,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
     * =gfOpenFile(oAriaApplication.DataDir+"StyInvJl", "StyInvJl", "SH")
       =gfOpenTable(oAriaApplication.DataDir+"StyInvJl", "StyInvJl", "SH")
      *E304012,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]
      llOpStyJl = .T.
    ENDIF

    IF !Used('STYDYE')
      =gfOpenTable(oAriaApplication.DataDir+"StyDye", "StyDye", "SH")
      llOpStyDye = .T.
    ENDIF

    llDyelot   = gfGetMemVar('M_Dyelot') ='Y'

    lcStyHdr   = gfItemMask('HM')
    lnMajLen   = LEN(gfItemMask('PM'))

    *--Displayed Screen Message
    lcStyCst  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_TOTCST,loFormSet.GetHeaderText("LANG_UPDATE_TOTCST",loFormSet.HeaderAlias)) + ALLTRIM(lcStyHdr) + ': "' + ;
                 ALLTRIM(SUBSTR(lcItemCode, 1, lnMajLen)) + ;
                IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_CHNG,loFormSet.GetHeaderText("LANG_UPDATE_CHNG",loFormSet.HeaderAlias))

    *-- Get Gl Current Period and Year
    IF llNoScreen
      =CHECKPRD(oAriaApplication.SystemDate,'lcGLFYear','lcGLPeriod', 'IP', .T.)
    ENDIF

  CASE lcItemType = '0002'

    *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[Start]
*!*      lcItem    = IIF(TYPE('loParForm') = 'O',loParForm.lcItem ,'Fabric_U')
*!*      lcItemDye = IIF(TYPE('loParForm') = 'O',loParForm.lcItemloc ,'ITEMLOC_U')
    lcItem    = IIF(TYPE('loParForm') = 'O' and TYPE("loParForm.lcItem") <> 'U',loParForm.lcItem ,'Fabric_U')
    lcItemDye = IIF(TYPE('loParForm') = 'O' and TYPE("loParForm.lcItemloc") <> 'U',loParForm.lcItemloc ,'ITEMLOC_U')
    *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]

    =gfOpenTable(oAriaApplication.DataDir+"ITEMADJ", "itemadj", "SH", @lcTmpAdj, .T.)
    =gfOpenTable(oAriaApplication.DataDir+'UOM' ,'UOMCODE','SH')

    IF !Used('itemjrnl_U')
      =gfOpenTable(oAriaApplication.DataDir+"itemjrnl", "StyInvJl", "SH",'itemjrnl_U')
      llOpMatJl = .T.
    ENDIF

    llDyelot   = gfGetMemVar('M_MATDYE') ='Y'


	lcMatHdr   = gfItemMask('HM',,'0002')
    lnMatMajLen   = LEN(gfItemMask('PM',,'0002'))
    *--Displayed Screen Message

    IF SUBSTR(ALLTRIM(lcItemCode),LEN(lcItemCode)-1,1) = '-'
      lcStyCst  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_TOTCST,loFormSet.GetHeaderText("LANG_UPDATE_TOTCST",loFormSet.HeaderAlias)) + lcMatHdr + ':"' + ;
      SUBSTR(ALLTRIM(lcItemCode),1,LEN(lcItemCode)-1)+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_CHNG,loFormSet.GetHeaderText("LANG_UPDATE_CHNG",loFormSet.HeaderAlias))
    ELSE
      lcStyCst  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_TOTCST,loFormSet.GetHeaderText("LANG_UPDATE_TOTCST",loFormSet.HeaderAlias)) + lcMatHdr + ':"' + ;
      lcItemCode+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_CHNG,loFormSet.GetHeaderText("LANG_UPDATE_CHNG",loFormSet.HeaderAlias))
    ENDIF


    lcStyCst  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_TOTCST,loFormSet.GetHeaderText("LANG_UPDATE_TOTCST",loFormSet.HeaderAlias)) + lcMatHdr + ':"' + ;
                lcItemCode+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_CHNG,loFormSet.GetHeaderText("LANG_UPDATE_CHNG",loFormSet.HeaderAlias))
    *lcStyCst  = 'The cost use for Fabric: "' + ;
                 ALLTRIM(SUBSTR(lcItemCode, 1, lnMatMajLen)) + IIF(!EMPTY(SUBSTR(lcItemCode, 8, 6)), ' - ', '') + ALLTRIM(SUBSTR(lcItemCode, 8, 6)) + ;
                '" has been changed.'

    *-- Get Gl Current Period and Year
    IF llNoScreen
      =CHECKPRD(oAriaApplication.SystemDate,'lcGLFYear','lcGLPeriod', 'MP', .T.)
    ENDIF
ENDCASE

PRIVATE llFound
llFound = .F.
lcGlSess = gfsequence('GLSESSION')

DIMENSION laStyStk[10]

SELECT (lcColorFil)
LOCATE
IF SEEK(lcItemCode)
  SCAN REST WHILE Style = lcItemCode
    lcFCond    = IIF(llDyeLvl, "!EMPTY(Dyelot)", IIF(llMultiWH, "EMPTY(Dyelot)", ".T."))
    lcWCond    = "Style = "+ "&lcColorFil..Style"

    lcWorkFile = IIF(llMultiWH OR llDyeLvl, lcItemDye, lcItem)
    SELECT(lcWorkFile)
	*: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[Start]
    lnRecNum = RECNO()
	*: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[End]

    =SEEK(&lcColorFil..Style)

    *-- Scan in ItemDye or Item Files with the Current Item Code
    *---- If we scan in Item file, there is only one record...
    *---- If we scan in ItemDye, the number of records depends on Warecodes and Dyelots for current Item Code...
    SCAN REST WHILE &lcWCond FOR &lcFCond
      =lfBalanced(IIF(lcItemType = '0001', 'S', 'M'))
      IF IIF(lcItemType = '0001', laStyStk[9]<>0 OR laStyStk[10] <> 0,laStyStk[1]<>0 OR laStyStk[2]<>0)
        llFound = .T.
        EXIT
      ENDIF
    ENDSCAN

	*: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[Start]
    SELECT(lcWorkFile)
    IF BETWEEN(lnRecNum ,1,RECCOUNT())
      GO RECORD lnRecNum
    ENDIF
	*: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[End]


    IF llFound
      EXIT
    ENDIF
  ENDSCAN
ENDIF

*--If no transactions found, return
IF !llFound
  llContinSave = .T.
  RETURN
ENDIF

lcAdjReason = ''


*!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[Start]
IF llFromCostSheet
  =gfOpenTable('BOM','multibom','SH','CSTSHTBOM')
  IF lcItemType = '0002'
    STORE 0 TO lnCost1,lnCost2,lnCost3,lnCost4,lnCost5,lnCost6,lnCost7
    loGetItemMask = CREATEOBJECT('GetItemMask')
    =gfSeek('0002'+PADR(loParForm.ariaForm1.cntCostSheet.kbItem.keytextbox.Value,19)+"T"+PADR(loParForm.ariaForm1.cntCostSheet.kbCstSht_ID.keytextbox.Value,6),'CSTSHTBOM')
  ELSE
    LOCAL ARRAY laClrCost[7], laClrECost[7], laClrCurrency[7,3]
    =gfSeek('0001'+PADR(loParForm.ariaForm1.cntCostSheet.kbItem.keytextbox.Value,19)+loParForm.lcStyleTyp+PADR(loParForm.ariaForm1.cntCostSheet.kbCstSht_ID.keytextbox.Value,6),'CSTSHTBOM')
  ENDIF
ENDIF
*!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]


SELECT (lcColorFil)
LOCATE
IF SEEK(lcItemCode)

  *: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[Start]
  lnRecNumber = RECNO(lcItem)
  *: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[End]

  SCAN REST WHILE Style = lcItemCode
    *! B608975,1 MMT 08/24/2009 Fix bug of error while updating while adding new color to fabric{Start}
    *=gfSEEK(&lcColorFil..Style, lcItem)
    IF gfSEEK(&lcColorFil..Style, lcItem)
    *! B608975,1 MMT 08/24/2009 Fix bug of error while updating while adding new color to fabric{End}
	    DO CASE
	      CASE lcItemType = '0001'
	        *B608768,1 MMT 12/31/2008 Fix bug of not updating Manf. Styles[Start]
	*!*	        IF &lcColorFil..Make
	*!*	          lnNewCost = &lcColorFil..nMCost1 + &lcColorFil..nMCost2 + &lcColorFil..nMCost3 + &lcColorFil..nMCost4 + &lcColorFil..nMCost5+ &lcColorFil..nMCost6+ &lcColorFil..nMCost7
	*!*	          lnOldCost = Style.nMCost1 + Style.nMCost2 + Style.nMCost3 + Style.nMCost4 + Style.nMCost5+ Style.nMCost6 + Style.nMCost7
	*!*	        ELSE
	        *B608768,1 MMT 12/31/2008 Fix bug of not updating Manf. Styles[Start]
	        STORE 0 TO lnNewCost , lnOldCost
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[Start]
	        IF !llFromCostSheet
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]

	          FOR lnI = 1 TO ALEN(laCostFld,1)
	            lnOldCost = lnOldCost + laCostFld[lnI,1]
	            lnNewCost = lnNewCost + laCostFld[lnI,2]
	          ENDFOR
	
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[Start]
	        ELSE
	          STORE '' TO laClrCurrency
	          STORE 0 TO laClrCost, laClrECost
	
	          loParForm.ariaForm1.mfCSTSC.mgetstyclrcst(&lcColorFil..style,&lcColorFil..scale,loParForm.llExtSizSc ,'CSTSHTBOM',loParForm.lcTmpFbric,@laClrCost,@laClrECost,@laClrCurrency)
	          FOR lnA=1 TO 7
	            lnOldCost = lnOldCost +  laClrECost[lnA]
	          ENDFOR
	           STORE '' TO laClrCurrency
	          STORE 0 TO laClrCost, laClrECost
	          loParForm.ariaForm1.mfCSTSC.mgetstyclrcst(&lcColorFil..style,&lcColorFil..scale,loParForm.llExtSizSc ,loParForm.lcTmpBom,loParForm.lcTmpFbric,@laClrCost,@laClrECost,@laClrCurrency)
	          FOR lnA=1 TO 7
	            lnNewCost = lnNewCost +  laClrECost[lnA]
	          ENDFOR
	        ENDIF
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]
	
	        *B608768,1 MMT 12/31/2008 Fix bug of not updating GLDIST File[Start]
	        *!*ENDIF
	    		*B608768,1 MMT 12/31/2008 Fix bug of not updating GLDIST File[End]
	      CASE lcItemType = '0002'
	
	        STORE 0 TO lnNewCost , lnOldCost
	
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[Start]
	        IF !llFromCostSheet
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]

	          FOR lnI = 1 TO ALEN(laCostFld,1)
	            lnOldCost = lnOldCost + IIF(TYPE('laCostFld[lnI,1]')='N',laCostFld[lnI,1],0)
	          ENDFOR
	
	          lnNewCost = IIF(gfSeek(&lcColorFil..cConvBuy,'UOM'),&lcColorFil..TOTCOST/ UOM.nconf,0)
	          lnOldCost = IIF(gfSeek(EVALUATE(IIF(TYPE('loParForm') = 'O',loParForm.lcItem,'FABRIC_U') +'.cConvBuy'),'UOM'),lnOldCost / UOM.nconf,0)
	
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[STart]
	        ELSE
	          lcFbrClr  = SUBSTR(&lcColorFil..Style , loParForm.lnColorStr , loParForm.lnColorLen)   && Save the current color.
	          *-- Blank all the cost variables.
	          STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5 , lnCost6 , lnCost7
	          lfCalcCstElem('CSTSHTBOM')
	          lnOldCost = lnCost1 + lnCost2 + lnCost3 + lnCost4 + lnCost5 + lnCost6 + lnCost7
	          STORE 0 TO lnCost1 , lnCost2 , lnCost3 , lnCost4 , lnCost5 , lnCost6 , lnCost7
	          lfCalcCstElem(loParForm.lcTmpBom)
	          lnNewCost = lnCost1 + lnCost2 + lnCost3 + lnCost4 + lnCost5 + lnCost6 + lnCost7
	        ENDIF
	        *!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]
	
	
	    ENDCASE
	
	    *B609440,1 TMI 10/20/2010 [Start] compare between lnOldCost and lnNewCost in the correct place
        IF lnOldCost <> lnNewCost
          IF !llNoScreen
            llContinSave = .F.
            DO FORM (oAriaApplication.ScreenHome+ "ICUPDSV.ScX") WITH lcStyCst,lnOldCost,lnNewCost
            IF !llContinSave
              EXIT
            ENDIF
            llNoScreen = .T.
          ELSE
            =lfvOK()
          ENDIF
        ENDIF
	    *B609440,1 TMI 10/20/2010 [End  ]
	
    *! B608975,1 MMT 08/24/2009 Fix bug of error while updating while adding new color to fabric{Start}
    ENDIF
    *! B608975,1 MMT 08/24/2009 Fix bug of error while updating while adding new color to fabric{End}

    *B609440,1 TMI 10/20/2010 [Start] Don�t compare old and new costs for newly added colors
    *                                 to achive this I'll move the IF lnOldCost <> lnNewCost statement to be at the end of the
    *                                 above if statement
    *IF lnOldCost <> lnNewCost
    *  IF !llNoScreen
    *    llContinSave = .F.
    *    DO FORM (oAriaApplication.ScreenHome+ "ICUPDSV.ScX") WITH lcStyCst,lnOldCost,lnNewCost
    *    IF !llContinSave
    *      EXIT
    *    ENDIF
    *    llNoScreen = .T.
    *  ELSE
    *    =lfvOK()
    *  ENDIF
    *ENDIF
    *B609440,1 TMI 10/20/2010 [End  ]
    SELECT (lcColorFil)

    IF !llContinSave
      EXIT
    ENDIF
  ENDSCAN

  *: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[Start]
  IF BETWEEN(lnRecNumber ,1, RECCOUNT(lcItem))
    GO RECORD lnRecNumber IN (lcItem)
  ENDIF
  *: B608751,1 MMT 12/03/2008 Fix bug of can not enter new style color if this prg called[END]

ENDIF
*! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
*IF llGlLink AND !EOF(lcTmpGlDt)
IF llGlLink
*! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
  SELECT (lcTmpGlDt)
  LOCATE
  *! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  IF !EOF(lcTmpGlDt)
  *! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_UPDATE_GL  NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UPDATE_GL,loFormSet.GetHeaderText("LANG_UPDATE_GL",loFormSet.HeaderAlias))  NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


  *-- Get a unique session number.
  *! E303956,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
  *REPLACE ALL GLSESSION WITH lcGlSess
  lcGLDISTCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
  SELECT (lcTmpGlDt)
  SCAN 
    REPLACE GLSESSION WITH lcGlSess
    =gfAdd_Info(lcTmpGlDt)    
    SCATTER MEMVAR MEMO
    SELECT GLDIST
    APPEND BLANK
    GATHER MEMVAR MEMO
    IF !EMPTY(lcGLDISTCursorUpdate)
      SELECT (lcGLDISTCursorUpdate)
      APPEND BLANK
      GATHER MEMVAR MEMO 
    ENDIF    
  ENDSCAN
  *! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  =gfCreateGLEntries(lcTmpGlDt,'')
  *! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
  *! E303956,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
*E303956,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*USE
*! E303956,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	SELECT (lcTmpGlDt)
*!*	SCAN 
*!*	  SCATTER MEMVAR MEMO
*!*	  m.Oid = gfTempName()
*!*	  SELECT GLDIST
*!*	  *!*	  APPEND FROM (oAriaApplication.WorkDir+lcTmpGlDt)
*!*	  APPEND BLANK
*!*	  GATHER MEMVAR MEMO
*!*	  =gfReplace('')
*!*	ENDSCAN 
*! E303956,2 MMT 12/13/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
USE IN (lcTmpGlDt)     
*E303956,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [END]
  *B608768,1 MMT 12/31/2008 Fix bug of not updating GLDIST File[Start]
  SELECT GLDIST
  =gfTableUpdate()
  *B608768,1 MMT 12/31/2008 Fix bug of not updating GLDIST File[End]

  ERASE (oAriaApplication.WorkDir+lcTmpGlDt+'.DBF')
  *! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
  ENDIF
  *! E611841,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
ENDIF

IF lcItemType = '0001' AND llOpStyDye
*B611781,1 ES 06/03/2019 -  When user run the Data integrity check for Inventory control module, he got many record has incorrect stock values [Start]
 SELECT STYDYE
  =gfTableUpdate()
*B611781,1 ES 06/03/2019 -  When user run the Data integrity check for Inventory control module, he got many record has incorrect stock values [End]

  =gfCloseTable('STYDYE')
ENDIF

WAIT CLEAR

*!**************************************************************************
*! Name      : lfBalanced
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/23/2008
*! Purpose   : Check Stock.
*!**************************************************************************
*! Passed Parameters :
*!**************************************************************************
*! Notes :
*!**************************************************************************
FUNCTION lfBalanced
PARAMETERS lcType
PRIVATE lnAlias

lnAlias = ALIAS()

IF lcType = 'S'
  IF !USED('StyInvJl')
     *E304012,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
   * =gfOpenFile(oAriaApplication.DataDir+"StyInvJl","StyInvJl","SH")
     =gfOpenTable(oAriaApplication.DataDir+"StyInvJl","StyInvJl","SH")
       *E304012,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]
  ENDIF
  STORE 0 TO laStyStk
  SELECT (lcWorkFile)
  llDyeLvl = ( llDyelot AND &lcItem..cDye_Flg = 'Y' )
  lcKey    = IIF(!llMultiWH,STyle,Style+cWareCode+IIF(llDyeLvl,Dyelot,''))
  lcDyeLot = IIF(llDyeLvl,DyeLot,'')
  *E304012,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [BEGIN]
  *IF SEEK(lcKey,'StyInvJl')
  IF GFSEEK(lcKey,'StyInvJl')
  *E304012,1 SAH 06/25/2018 CONVERT STYINVJL TO SQL [END]
    SELECT StyInvJl
    SUM REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =lcKey;
             FOR IIF(llDyelot AND &lcItem..cDye_Flg = 'Y',cDyeLot = lcDyeLot,.T.);
                    nStk1,nStk2,nStk3,nStk4,nStk5,nStk6,nStk7,nStk8,nTotStk,nStkVal;
         TO laStyStk[1],laStyStk[2],laStyStk[3],laStyStk[4],laStyStk[5],laStyStk[6],laStyStk[7],;
            laStyStk[8],laStyStk[9],laStyStk[10]
    SELECT (lcWorkFile)
    IF Stk1 <> laStyStk[1] OR;
       Stk2 <> laStyStk[2] OR;
       Stk3 <> laStyStk[3] OR;
       Stk4 <> laStyStk[4] OR;
       Stk5 <> laStyStk[5] OR;
       Stk6 <> laStyStk[6] OR;
       Stk7 <> laStyStk[7] OR;
       Stk8 <> laStyStk[8] OR;
       TotStk  <> laStyStk[9] OR;
       nStkVal <> laStyStk[10]
       RETURN .F.
    ENDIF
  ENDIF
ELSE
  IF !USED('itemjrnl_U')
    =gfOpenTable(oAriaApplication.DataDir+"ITEMJRNL","StyInvJl","SH",'itemjrnl_U')
  ENDIF
  STORE 0 TO laStyStk
  SELECT (lcWorkFile)
  llDyeLvl = ( llDyelot AND &lcItem..cDye_Flg = 'Y' )
  lcKey    = "0002" + IIF(!llMultiWH,Style,Style+cWareCode+IIF(llDyeLvl,Dyelot,''))
  lcDyeLot = IIF(llDyeLvl,DyeLot,'')
  IF gfSEEK(lcKey,'itemjrnl_U')
    SELECT itemjrnl_U
    SUM REST WHILE cinvtype+ style+ cwarecode+ csession+ DTOS(dtrdate)+ ctrcode+ STR(lineno,6)= lcKey;
             FOR IIF(llDyelot AND &lcItem..cDye_Flg = 'Y',cDyeLot = lcDyeLot,.T.);
                    (Ntotstk),nStkVal;
         TO laStyStk[1],laStyStk[2]

    SELECT (lcWorkFile)
    IF TOTsTK <> laStyStk[1] OR;
       nStkVal <> laStyStk[2]
       RETURN .F.
    ENDIF
  ENDIF

ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lpModOK()
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/23/2008
*! Purpose   : Valid Fuction for the Ok Button
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfvOK
PARAMETERS loChldFormSet

*B609439,1 TMI 10/20/2010 [Start] open the warehous file
=gfOpenTable(oAriaapplication.Datadir+"WAREHOUS", "WAREHOUS", "SH")
*B609439,1 TMI 10/20/2010 [End  ]

IF !llNoScreen
  =CHECKPRD(oAriaApplication.SystemDate,'lcGLFYear','lcGLPeriod', IIF(lcItemType = '0001', 'IP', 'MP'), .T.)
ENDIF

PRIVATE lcWorkFile, llUpdRet, lcFCond, lnCurrRec
llDyeLvl   = (llDyelot AND &lcItem..cDye_Flg = 'Y')
lcWorkFile = IIF(llMultiWH OR llDyeLvl, lcItemDye, lcItem)
llUpdRet   = .T.
lcFCond    = IIF(llDyeLvl, "!EMPTY(Dyelot)", IIF(llMultiWH, "EMPTY(Dyelot)", ".T."))
lcWCond    =  "Style = " + "&lcColorFil..Style"
IF TYPE('loChldFormSet') = 'O'
  lcAdjReason = loChldFormSet.AriaForm1.cboCodeReason.Value
ENDIF

SELECT (lcWorkFile)
=SEEK(&lcColorFil..Style)



*-- Scan in ItemDye or Item Files with the Current Item Code
*---- If we scan in Item file, there is only one record...
*---- If we scan in ItemDye, the number of records depends on Warecodes and Dyelots for current Item Code...
SCAN REST WHILE &lcWCond FOR &lcFCond
  SELECT (lcWorkFile)
  lnCurrRec = RECNO()

  SELECT IIF(!llMultiWH .AND. !llDyeLvl, lcItem, lcItemDye)
  DO CASE
    CASE lcItemType = '0001'
      IF !lfBalanced('S')
        GATHER FROM laStyStk FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,nStkVal
      ENDIF

      *--If stock value equal zero, don't update
      IF TotStk = 0 AND nStkVal = 0
        LOOP
      ENDIF

      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
      lnTotStk = TotStk

      SELECT (lcTmpAdj)
      gfAppend()
      APPEND BLANK
      GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
                                  OldQty5,OldQty6,OldQty7,OldQty8
      GATHER FROM laToSave FIELDS Adj1,Adj2,Adj3,Adj4 ,;
                                  Adj5,Adj6,Adj7,Adj8
      REPLACE Style     WITH &lcWorkFile..Style ,;
              TotOld    WITH lnTotStk  ,;
              TotAdj    WITH lnTotStk  ,;
              dPostDate WITH oAriaApplication.SystemDate ,;
              cReason   WITH lcRefer   ,;
              Date      WITH oAriaApplication.SystemDate ,;
              Type      WITH 'P'       ,;
              Unt_Cost  WITH lnNewCost ,;
              Old_Cost  WITH lnOldCost
      gfReplace()

    CASE lcItemType = '0002'
      IF !lfBalanced('M')
        GATHER FROM laStyStk FIELDS TOTsTK,nStkVal
      ENDIF

      *--If stock value equal zero, don't update
      IF TOTsTK = 0 AND nStkVal = 0
        LOOP
      ENDIF

      lnTotStk = TOTsTK

      SELECT (lcTmpAdj)
      gfAppend()

      gfREPLACE("CINVTYPE   with '0002',;
                 Style      WITH &lcWorkFile..Style ,;
	             TOTOLD     WITH lnTotStk  ,;
    	         ADJ1       WITH lnTotStk  ,;
    	         TOTADJ     WITH lnTotStk  ,;
            	 cReason    WITH lcRefer   ")
            	
	  gfREPLACE("Date       WITH oAriaApplication.SystemDate ,;
    	         Type       WITH 'P'       ,;
        	     UNT_COST   WITH lnNewCost ,;
            	 nUntCstBuy WITH &lcColorFil..TOTCOST")
  ENDCASE

  SELECT (lcTmpAdj)
  gfREPLACE("dAdd_Date WITH oAriaApplication.SystemDate  ,;
          cAdd_Time WITH TIME()     ,;
          cAdd_User WITH oAriaApplication.User_ID  ,;
          GlFYear   WITH lcGlFyear  ,;
          GlPeriod  WITH lcGlPeriod")

  IF llDyelvl
    gfREPLACE("Dyelot WITH &lcWorkFile..Dyelot")
  ENDIF

  IF llMultiWH
    gfREPLACE("cFromWare WITH &lcWorkFile..cWareCode")
  ENDIF

  PRIVATE lcWMsg

  DO CASE
    CASE lcItemType = '0001'
      lcWMsg = ALLTRIM(&lcColorFil..Style) + ;
               IIF(llDyelvl, '/' + &lcWorkFile..Dyelot, '')
    CASE lcItemType = '0002'
      lcWMsg = ALLTRIM(&lcColorFil..Style) + ;
               IIF(llDyelvl, '/' + &lcWorkFile..Dyelot, '')
  ENDCASE

  WAIT WINDOW 'Start updating => ' + lcWMsg NOWAIT

  *--Start Updating.
  llUpdRet = lfInvUpdt()
  IF !llUpdRet
    Exit
  ENDIF

  SELECT (lcTmpAdj)
  DO CASE
    CASE lcItemType = '0001'
      gfREPLACE("cSession WITH lcGlSess")
    CASE lcItemType = '0002'
      gfREPLACE("cSession WITH lcGlSess")
  ENDCASE

  SELECT (lcWorkFile)
  IF lnCurrRec <> 0
    GOTO lnCurrRec
  ENDIF
ENDSCAN

llContinSave = llUpdRet
IF llContinSave
  DO CASE
    CASE lcItemType = '0001'
      SELECT (lcTmpAdj)
      gfTableUpdate()
      IF llGlLink
        SELECT GLDist
        gfTableUpdate()
      ENDIF
    CASE lcItemType = '0002'
      SELECT (lcTmpAdj)
      gfTableUpdate()
      IF llGlLink
        SELECT GLDist
        gfTableUpdate()
      ENDIF
  ENDCASE
ENDIF

*!*************************************************************
*! Name      : lfInvUpdt
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/23/2008
*! Purpose   : Update Inventory Journal and GLDist Files
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpUnLock
*!*************************************************************
FUNCTION lfInvUpdt

PRIVATE lcWareFrom, lcDyeCode
STORE '' TO lcWareFrom, lcDyeCode, lcLinkCode

SELECT (lcWorkFile)

lcWareFrom = IIF(llMultiWH, &lcWorkFile..cWareCode, WAREHOUS.CWARECODE )
lcDyeCode  = IIF(llDyeLvl, &lcWorkFile..Dyelot, '')
lcLinkCode = IIF(llMultiWH OR llDyeLvl, IIF(!EMPTY(GL_Link), GL_Link, &lcItem..Link_Code), Link_Code)
lcLinkCode = IIF(llGlLink, IIF(!EMPTY(lcLinkCode), lcLinkCode, 'DEFDEF'), "")

*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  DO CASE
    CASE lcItemType = '0001'
      laGLDistAr[1,2] = '006'
      laGLDistAr[2,2] = '007'
      STORE 'IP' TO laGLDistAr[1,4],laGLDistAr[2,4]
    CASE lcItemType = '0002'
      laGLDistAr[1,2] = '015'
      laGLDistAr[2,2] = '016'
      STORE 'MP' TO laGLDistAr[1,4],laGLDistAr[2,4]
  ENDCASE

  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt  TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjLink
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*--Adjustment quantity array.
DECLARE laAdjust[9]
STORE 0 TO laAdjust
DO CASE
  CASE lcItemType = '0001'
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laAdjust
  CASE lcItemType = '0002'
    laAdjust[1] = totStk
    laAdjust[9] = totStk
ENDCASE

DO CASE
  CASE lcItemType = '0001'
    *--Call the global function for update Style Inventory Control.
    lnRet = gfStyCrl('2',&lcWorkFile..Style,lcWareFrom,lcDyeCode,oAriaApplication.SystemDate,'',@laAdjust,lnNewCost,;
                     lcRefer,lcGlSess,lcAdjReason,0,'','',@laGLDistAr,0,"","")
  CASE lcItemType = '0002'
    *--Call the global function for update Material Inventory Control.
    lnRet = gfItemCrl('2','0002',&lcWorkFile..Style,;
                     lcWareFrom,lcDyeCode,oAriaApplication.SystemDate,oAriaApplication.SystemDate,SPACE(6),@laAdjust,lnNewCost,;
                     lcRefer,lcAdjReason,'',@laGLDistAr,lcGlSess,'','','','','','')
ENDCASE

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.
ENDIF

RETURN .T.

*!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[Start]
*!*************************************************************
*! Name      : lfCalcCstElem
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/14/2009
*! Purpose   : calc Cost sheet elements values
*!*************************************************************
FUNCTION lfCalcCstElem
PARAMETERS lcFileBom
SELECT (lcFileBom)
LOCAl lcItem
STORE '' TO lcItem
SCAN FOR LIKE(STRTRAN(EVALUATE(lcFileBom+'.cItmMask'),'*','?') , &lcColorFil..Style )
  lcCurVar = 'lnCost'+EVALUATE(lcFileBom+'.Typ')
  DO CASE
    *-- If the cost item is fabric or trim & trim inventory.
    CASE (cCatGTyp='F') .OR. (cCatGTyp='T' .AND. Trim_Invt)
      LOCAL lnMatMajLen,lcFbrClrComp,lnFbrClrStr,lnFbrClrLen,lcItem
        *-- Get the major length of material.
      lnMatMajLen = LEN(loGetItemMask.Do("PM",'',cInvTypC))
      DIMENSION laStySeg[1,1]
      =loGetItemMask.Do(@laStySeg,'',cInvTypC)
      FOR lnCnt = 1 TO ALEN(laStySeg,1)
        IF laStySeg[lnCnt , 1] = "C"
          lnFbrClrStr = laStySeg[lnCnt , 4]
          lnFbrClrLen = LEN(laStySeg[lnCnt , 3])
        ENDIF
      ENDFOR
      *-- Get the current fabric color from the temp. bom file.
      lcFbrClrComp = IIF("*" $ SUBSTR(Item,lnFbrClrStr,lnFbrClrLen) , lcFbrClr , SUBSTR(Item,lnFbrClrStr,lnFbrClrLen))
      lcItem = STUFF(Item,lnFbrClrStr,lnFbrClrLen,lcFbrClrComp)

      *-- Seek for the current fabric + color in the fabric file.
      m.cInvType  = cInvTypC
      m.cStyMajor = PADR(Item,lnMatMajLen)
      IF loParForm.mopensql("item","FABRIC_A","cstyle","CINVTYPE+CSTYMAJOR")
        SELECT FABRIC_A
        LOCATE FOR LIKE(STRTRAN(lcItem , "*" , "?") , FABRIC_A.Style)
        IF FOUND()
          *-- Calculate the cost for the current cost item.
          LOCAL lnConf
          lnConf = 1
          =gfGetUOMData(EVALUATE(lcFileBom+'.CUOMCODE'), '', '', @lnConf)
          &lcCurVar = &lcCurVar + ROUND((EVALUATE(lcFileBom+'.nBomTotQty') * FABRIC_A.TotCost / lnConf), 2)
        ELSE
          IF SEEK(m.cInvType+PADR(m.cStyMajor,19) , loParForm.lcTmpFbric)
            SELECT (loParForm.lctmpfbric)
            LOCATE FOR LIKE(STRTRAN(lcItem , "*" , "?") , Style)
            IF FOUND()
              *-- Calculate the cost for the current cost item.
              LOCAL lnConf
              lnConf = 1
              =gfGetUOMData(EVALUATE(lcFileBom+'.CUOMCODE'), '', '', @lnConf)
              &lcCurVar = &lcCurVar + ROUND((EVALUATE(lcFileBom+'.nBomTotQty') * TotCost / lnConf), 2)
            ENDIF
          ENDIF
        ENDIF
        USE IN FABRIC_A
        SELECT (lcFileBom)
      ENDIF
      *-- If the cost item is trim & not trim inventory or
      *-- Price, Duty or Mfg operation.
    CASE (cCatGTyp = 'T' .AND. !Trim_Invt) .OR. cCatGTyp $ 'MDP'
      *-- Calculate the cost for the current cost item.
      &lcCurVar = &lcCurVar + ROUND(TotCost,2)
      *-- If the cost item is style component.

    CASE cCatGTyp = 'S'
      LOCAL lnStyMajLen,lcItmFbr,lnStyClrStr,lnStyClrLen
      *-- Get the major length of material.
      lnStyMajLen = loGetItemMask.Do("PM",'',cInvTypC)
      DIMENSION laStySeg[1,1]
      =loGetItemMask.Do(@laStySeg,'',cInvTypC)
      FOR lnCnt = 1 TO ALEN(laStySeg,1)
        IF laStySeg[lnCnt , 1] = "C"
          lnStyClrStr = laStySeg[lnCnt , 4]
          lnStyClrLen = LEN(laStySeg[lnCnt , 3])
        ENDIF
      ENDFOR

      IF '*' $ SUBSTR(Item , lnStyClrStr , lnStyClrLen)
        lcItmFbr  = PADR(STUFF(Item , lnStyClrStr , lnStyClrLen , lcFbrClr),19)
      ELSE
        lcItmFbr  = EVALUATE(lcFileBom+'.Item')
      ENDIF
      *-- Save the current record pointer in the style file.
      lnTmpRec = RECNO("STYLE")
      *-- Seek for the current style + color in the style file.
      SELECT STYLE
      =SEEK(PADR(lcItmFbr,loParForm.lnMajorLen) , "STYLE")
      LOCATE FOR LIKE(STRTRAN(lcItmFbr , "*" , "?") , STYLE.Style)
      *-- Calculate the cost for the current cost item.
      IF FOUND()
        &lcCurVar = &lcCurVar + ROUND(EVALUATE(lcFileBom+'.nBomTotQty') * Style.TotCost,2)
      ENDIF
      *-- Restore the record pointer in the style file.
      IF lnTmpRec > 0 .AND. lnTmpRec <= RECCOUNT("STYLE")
        GOTO lnTmpRec IN STYLE
      ENDIF
      SELECT (lcFileBom)
 ENDCASE
 SELECT (lcFileBom)
ENDSCAN
*!*  B608768,2 MMT 01/14/2009 Fix bug of updating all item colors with same cost in Styinvl[End]
