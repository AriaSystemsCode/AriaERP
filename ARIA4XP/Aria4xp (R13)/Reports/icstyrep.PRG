*:***************************************************************************
*: Program file  : ICSTYREP
*: Program desc. : Style Summary
*: For Report    : ICSTYBYS.FRX,ICSTYBYO.FRX,ICSTYBYL.FRX,ICSTYBYW.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Heba Mohamed Amin	(HMA)
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
*: B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria
*: B131649,1 AYM 03/29/2006 Fix Bug of Scale.cscl_desc
*:Media Testing: AYM the layout looks landscape
*: B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when printing from the preview screen (T20061130.0056)
*: B607997,1 AYM 03/06/2007 Problem not printing first col_line if the prev. style ends with same color (T20070119.0022)
*: B608096,1 MMT 05/24/2007 fix bug of not displaying the Scale[T20070523.0016]
*: B608166,1 SSH 07/15/2007 The bugs in the report are as follows:
*: B608166,1 SSH            a) If you are selecting just one stock location the if the Print Zero Invetory flag is set to NO - then you must checkt the stock location record for zero stock and not the main stock record
*: B608166,1 SSH            b) you should not be checkign the totstk figure anyway -but each size - so that if there is a positve/negative figure in any size then the style shoudl be included in the report - even if the total stock is zero (for example i
*: B608163,1 TMI 07/12/2007 Fix a problem of not showing subtotal for case of one transaction selected (T20070601.0019)
*: B608189,1 TMI,HIA 08/06/2007 fix aligning problems in the reports ( check ticket T20070625.0011 )
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[T20071024.0014]
*: B608406,1 WAM 01/14/2008 Fix collecting data when select to print Stock and sort by style [T20070913.0001]
*: B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard [T20071204.0007]
*: B608470,1 WAM 03/04/2008 Fix condition based on which the style sum total is printed [T20071212.0031]
*: B608507,1 WAM 04/08/2008 some colors are not shown when select to sort by style [T20080307.0007]
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[T20080506.0003]
*: B608711,1 MMT 10/08/2008 Fix bug of Wrong Positive OTS when use print BOTH OTS [T20080910.0019]
*: B608711,2 MMT 10/28/2008 Fix bug of some Styles subtotal is not printed when use print BOTH OTS [T20080910.0019]
*: B608756,1 MMT 12/04/2008 Fix bug of Not displaying Sales value when sort by location[T20080927.0001]
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[T20080819.0022]
*! B609307,1 HES 06/19/2010 Fic bug of Style Summary report with Loc detail incorrect totals by colour [T20100121.0005]
*! E302831,1 TMI 01/04/2011 Enable/disable the new option 'Print long description'[T20101207.0001]
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004]
*! B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[T20120321.0039]
*! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[T20121015.0001]
*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026]
*! B611348,1 AHH 02/07/2017 How to export reports in excel
*! B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel[T20170721.0006]
*! B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][Start]
*! B611410,1 AHH 10/09/2017 Error while printing style summary report sorted by location [T20170831.0004]
*! B611409,1 AHH 11/09/2017 Style summary report export incorrect data [location details]= Yes [T20170905.0013]
*! B611499 ,1 HMS 12/19/2017- Aria 4XP - Need to do a style summary report based - [T20171215.0006]
*! B611545,1 SAH 03/04/2018 Modify the report program to replace line in which Style Summary report repeats records when exported to EXCEL when more than one transaction is selected[T20180323.0003]
*  B611608,1 Es 06/27/2018  Modify this issue -> "The IOTS Qty displayed in Exported data to Excel is incorrect" [T20180507.0002]
*! B611640 ,1 HMS 08/06/2018 Aria 5 - ESTEE LAUDER STYLE & SUMMARY REPORT [T20180718.0015]
*! B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [T20180524.0012]
*! B611747, HMS 03/20/2019  - Issue with Style Summary OTS report [T20190318.0002]
*! B611899,1 Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003]
*! B612042,1 MMT 02/12/2020 The Style Summary takes time to collect data when user selects single transaction in option grid and clients data us huge[T20200123.0004]
*! B612019,1 Es 03/01/2020 If the user sorted the Style Summary report by location and previewed it. then tried to export it to Excel, he got the error "Variable is not found" [T20191231.0002]
*! B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [T20200225.0002]
*! E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001]
*! B612323,1 Es 03/04/2021 Style Summary Report does not export sizes description when exported to Excel [T20210122.0001][Start]
*! B612349,1 Es 03/22/2021  Sometimes the Style Summary report does not display the Style total line [T20210226.0002]
*! B612540,1 MMT 03/30/2022 Style report deos not print styles that has some sizes with negative or positive OTS Qty if the Sum of the sizes OTS is not negative or positive[T20220323.0001]
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001]
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[T-ERP-20230531.0002]
*! B612709,1 MMT 03/31/2024 Fix error with the 2nd preview if the 1st preview gives no data[T-ERP-20230927.0002]
*:**********************************************************************************************************************
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID
IF TYPE('lcXMLFileName') = 'C'
	PRIVATE loAgent
*loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
	loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
	PRIVATE loProgress
	loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
	loProgress.Percent = 0
	loProgress.DESCRIPTION = "Opening Data Files..."
	loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
	LOCAL loEnvironment
*loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
	loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
	loEnvironment.ClientID = ClientID
	loEnvironment.ConnectionsRefresh()
	LOCAL lcCurrentProcedure

	lcCurrentProcedure = loEnvironment.Aria40SharedPath
	SET DEFAULT TO &lcCurrentProcedure.
	SET STEP ON
	DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH     loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID, lcCurrentProcedure, loEnvironment
	oAriaEnvironment.PROCESSID = "ICSTYREP"
	SET DEFAULT TO &lcCurrentProcedure.

	oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
* MT

*!*		lcTargetV = laRPRepTar
*!*		DIMENSION laRPRepTar[1,1]
*!*		laRPRepTar[1,1]= lcTargetV

*MT
	oAriaApplication.user_id = oAriaEnvironment.User_ID
	oariaApplication.user_id = GCUSER_ID
	oAriaEnvironment.REPORT.gcAct_Appl = 'IC'
	PUBLIC gcAct_Appl
	gcAct_Appl = 'IC'
	oAriaEnvironment.activeModuleID = 'IC'
	IF LEFT(gcDevice, 7) = "PRINTER"
		oAriaEnvironment.gcDevice = "PRINTER"
	ELSE
		oAriaEnvironment.gcDevice = "FILE"
	ENDIF
	oAriaEnvironment.REPORT.cCROrientation = 'P'


	loogScroll   = oAriaEnvironment.REPORT
ELSE
ENDIF

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
#INCLUDE R:\Aria4xp\reports\ICSTYREP.H
*xxxxx
IF TYPE('lcXMLFileName') = 'C'

	ON ERROR m=1
	IF !USED('STYLE')
		USE  (oAriaApplication.DataDir+'STYLE')IN 0 SHARED AGAIN ALIAS STYLE Order Tag STYLE
	ENDIF

	IF !USED('STYDYE')
		Use (oAriaApplication.DataDir+'STYDYE') IN 0 SHARED AGAIN ALIAS STYDYE Order Tag STYDYE
	ENDIF

	IF !USED('SCALE')
		Use (oAriaApplication.DataDir+'SCALE') IN 0 SHARED AGAIN ALIAS SCALE Order Tag SCALE
	ENDIF

	IF !USED('WAREHOUS')
		Use (oAriaApplication.DataDir+'WAREHOUS') IN 0 SHARED AGAIN ALIAS WAREHOUS Order Tag WAREHOUS
	ENDIF

	IF !USED('WHSLOC')
		Use (oAriaApplication.DataDir+'WHSLOC') IN 0 SHARED AGAIN ALIAS WHSLOC Order Tag WHSLOCST
	ENDIF

	IF !USED('ICSEGVAL')
		Use (oAriaApplication.DataDir+'ICSEGVAL')IN 0 SHARED AGAIN ALIAS ICSEGVAL Order Tag SEGVAL
	ENDIF

	IF !USED('ICDEPTHD')
		Use (oAriaApplication.DataDir+'ICDEPTHD')IN 0 SHARED AGAIN ALIAS ICDEPTHD Order Tag DEPTHD
	ENDIF

	IF !USED('ICDEPTDT')
		Use (oAriaApplication.DataDir+'ICDEPTDT')IN 0 SHARED AGAIN ALIAS ICDEPTDT Order Tag DEPTDTS
	ENDIF


*On Error
	SET STEP ON
*!*	DIMENSION laRPRepTar[1]
*!*	laRPRepTar[1]='WIP'
ENDIF
*xxxxx






If Alen(laRPRepTar,1) = 0 Or Empty(laRPRepTar)

*-- "You have to select transaction to be printed"
*-- <OK>
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen("INM42146B00000",LANG_ICSTYREP_Dialog)
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
		= gfModalGen("INM42146B00000",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dialog,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dialog",AHEADERFILE)))
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

	Return .F.
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

*B612709,1 MMT 03/31/2024 Fix error with the 2nd preview if the 1st preview gives no data[T-ERP-20230927.0002][Start]
*If loogScroll.llOGFltCh
If .T.
*B612709,1 MMT 03/31/2024 Fix error with the 2nd preview if the 1st preview gives no data[T-ERP-20230927.0002][End]
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
	lcStySty = loogScroll.laOgFxFlt[1,6]
*-- Variable Hold the Primary Fabrics Selected for Styles.
	lcStyFab = loogScroll.laOgFxFlt[2,6]
*-- Variable Hold the Selected Departments Selected for Styles.
	lcStyDept = loogScroll.laOgFxFlt[11,6]
	lcStySty1 =""
	lcStyFab1 =""
	lcStyDept1 =""
*--Retrieve Cursor From Style File(Fox File)which hold only Styles which had Fabrics.
*! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]

*lcStyFlds="style.* "
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*  lcStyFlds="style.*, gfCodDes(SUBSTR(style.Style,lnClrPo,lnColorLen),'COLOR     ') as Color "
	lcStyFlds="style.* "

*B612019,1 Es 03/01/2020 If the user sorted the Style Summary report by location and previewed it. then tried to export it to Excel, he got the error "Variable is not found" [Start]
*LCWHSLOCST = loogscroll.gfTempName()
*LCSTYDYEW= loogscroll.gfTempName()
*B612019,1 Es 03/01/2020 If the user sorted the Style Summary report by location and previewed it. then tried to export it to Excel, he got the error "Variable is not found" [End]


*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*lcStyFlds="style.*, SUBSTR(style.Style,lnClrPo,lnColorLen) as Color "
*! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
	lcStyTable="STYLE "

	lcTypeVal=""   &&Variable Hold Item_Type(Material Type)
	If !Empty(loogScroll.laOgFxFlt[12,6])
		lcTypeVal="'"+Strtran(loogScroll.laOgFxFlt[12,6],"|","','")+"'"
	Endif

*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*B608163,1TMI [Start] if the user has not selected any style then fill temp style with all styles to be used in the sql select
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*!*	  If Alen(laRPRepTar,1)=1 && This change will be made only in case of one transactin is selected
*!*	    If Empty(lcStySty)
*!*	      lcStySty = loogScroll.gfTempName()
*!*	      Create Table (oAriaApplication.WorkDir+lcStySty) (KEYEXP C(19),CSTYMAJOR C(19))
*!*	      Index On KEYEXP Tag &lcStySty
*!*	    Endif
*!*	    If Used(lcStySty)
*!*	      Select (lcStySty)
*!*	      lnCnt = 0
*!*	      Count To lnCnt
*!*	      If lnCnt = 0
*!*	        Select Style
*!*	        lcSvOrder = Order('STYLE')
*!*	        =gfSetOrder('CSTYLE')
*!*	        =gfSeek('')
*!*	        Scan
*!*	          Insert Into (lcStySty) (KEYEXP , CSTYMAJOR ) Values (Style.CSTYMAJOR,Style.CSTYMAJOR)
*!*	        ENDSCAN
*!*	        =gfSetOrder(lcSvOrder)
*!*	      Endif
*!*	    Endif
*!*	  ENDIF
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*B608163,1TMI [End  ]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]

	If !Empty(lcStySty) And Used(lcStySty)
		Select(lcStySty)
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*B608163,1TMI [Start] to fix a bug that when select a style and then to not unselect it from the browse a
*                           no record found messages appears
*IF RECCOUNT() > 0
		lnCnt = 0
		Count To lnCnt
		If lnCnt > 0
*B608163,1TMI [End  ]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[ENd]
			lcStySty1 = loogScroll.gfTempName()
			Copy To oAriaApplication.WorkDir+lcStySty1+".dbf"
			lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStySty1+".dbf" + " ON  STYLE.CSTYMAJOR = " +lcStySty1+".cStyMajor"
		Endif
	Endif
*--(A)
	If !Empty(lcStyFab) And Empty(lcTypeVal)
		Select(lcStyFab)
		If Reccount() > 0
			lcStyFab1 = loogScroll.gfTempName()
			Copy To oAriaApplication.WorkDir+lcStyFab1+".dbf"
			lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyFab1+".dbf" + " ON  STYLE.FABRIC = " +lcStyFab1+".cStyMajor"
		Endif
	Endif
*--(A)
	If !Empty(lcStyDept) And Used(lcStyDept)
		Select(lcStyDept)
		If Reccount() > 0
			lcStyDept1 = loogScroll.gfTempName()
			Copy To oAriaApplication.WorkDir+lcStyDept1+".dbf"
			lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyDept1+".dbf" + " ON  STYLE.DEPT = " +lcStyDept1+".Dept"
		Endif
	Endif

*--Make Temp File For Selected Style Divisions
	lcDivCursor =""
	lnStyDivision = Ascan(loogScroll.laOgFxFlt,"STYLE.CDIVISION")
	lnStyDivision = Asubscript(loogScroll.laOgFxFlt,lnStyDivision,1)
	lcDivisions= loogScroll.laOgFxFlt[lnStyDivision,6]
	If !Empty(lcDivisions)
		If lnStyDivision > 0
			lcDivCursor = loogScroll.gfTempName() &&Cursor Hold Selected Divisions
			Dimension laTempacstru[1,4]
			laTempacstru[1,1]='CDIVISION'
			laTempacstru[1,2]='C'
			laTempacstru[1,3]= 6
			laTempacstru[1,4]= 0
			gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.F.)
			If !Empty(lcDivisions)
				lnStart=1
				lnEnd=At('|',lcDivisions)
				Do While lnEnd <> 0
					Select(lcDivCursor)
					Append Blank
					Replace CDIVISION With Substr(lcDivisions,lnStart,lnEnd-1)
					lcDivisions = Stuff(lcDivisions ,lnStart,lnEnd,"")
					lnEnd=At('|',lcDivisions)
				Enddo
				If lnEnd = 0
					Select(lcDivCursor)
					Append Blank
					Replace CDIVISION With lcDivisions
				Endif
			Endif
		Endif
	Endif

	If !Empty(lcDivCursor)
		Use In &lcDivCursor
		lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcDivCursor+".dbf" + " ON  STYLE.CDIVISION = " +lcDivCursor+".CDIVISION"
	Endif

*--Make Temp File For Selected Style Seasons
	lcSeaCursor =""
	lnStySeason = Ascan(loogScroll.laOgFxFlt,"STYLE.SEASON")
	lnStySeason = Asubscript(loogScroll.laOgFxFlt,lnStySeason,1)
	lcSeasons= loogScroll.laOgFxFlt[lnStySeason,6]
	If !Empty(lcSeasons)
		If lnStySeason > 0
			lcSeaCursor = loogScroll.gfTempName() &&Cursor Hold Selected Seasons
			Dimension laTempacstru[1,4]
			laTempacstru[1,1]='SEASON'
			laTempacstru[1,2]='C'
			laTempacstru[1,3]= 6
			laTempacstru[1,4]= 0
			gfCrtTmp(lcSeaCursor,@laTempacstru,"SEASON",lcSeaCursor,.F.)
			If !Empty(lcSeasons)
				lnStart=1
				lnEnd=At('|',lcSeasons)
				Do While lnEnd <> 0
					Select(lcSeaCursor)
					Append Blank
					Replace SEASON With Substr(lcSeasons,lnStart,lnEnd-1)
					lcSeasons = Stuff(lcSeasons ,lnStart,lnEnd,"")
					lnEnd=At('|',lcSeasons)
				Enddo
				If lnEnd = 0
					Select(lcSeaCursor)
					Append Blank
					Replace SEASON With lcSeasons
				Endif
			Endif
		Endif
	Endif

	If !Empty(lcSeaCursor)
		Use In &lcSeaCursor
		lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+ lcSeaCursor+".dbf" + " ON  STYLE.SEASON = " +lcSeaCursor+".SEASON"
	Endif


*--Make Temp File For Selected Style Colors
	lcColCursor =""
	lnColorSeg = lnClrPo-lnColorLen+1
	lnStyColor = Ascan(loogScroll.laOgFxFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
	lnStyColor = Asubscript(loogScroll.laOgFxFlt,lnStyColor,1)
	lcColors= loogScroll.laOgFxFlt[lnStyColor,6]
	If !Empty(lcColors)
		If lnStyColor > 0
			lcColCursor = loogScroll.gfTempName() &&Cursor Hold Selected Colors
			Dimension laTempacstru[1,4]
			laTempacstru[1,1]='COLOR'
			laTempacstru[1,2]='C'
*! B611499 ,1 HMS 12/19/2017- Aria 4XP - Need to do a style summary report based - T20171215.0006 [Begin]
*laTempacstru[1,3]= lnColorSeg
			laTempacstru[1,3]= lnColorLen
*! B611499 ,1 HMS 12/19/2017- Aria 4XP - Need to do a style summary report based - T20171215.0006 [END]
			laTempacstru[1,4]= 0
			gfCrtTmp(lcColCursor,@laTempacstru,"Color",lcColCursor,.F.)
			If !Empty(lcColors)
				lnStart=1
				lnEnd=At('|',lcColors)
				Do While lnEnd <> 0
					Select(lcColCursor)
					Append Blank
					Replace Color With Substr(lcColors,lnStart,lnEnd-1)
					lcColors = Stuff(lcColors ,lnStart,lnEnd,"")
					lnEnd=At('|',lcColors)
				Enddo
				If lnEnd = 0
					Select(lcColCursor)
					Append Blank
					Replace Color With lcColors
				Endif
			Endif
		Endif
	Endif

	If !Empty(lcColCursor)
		Use In &lcColCursor
		lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+ lcColCursor+".dbf" + " ON  SUBSTR(STYLE.STYLE,"+Alltrim(Str(lnClrPo))+","+Alltrim(Str(lnColorLen))+") = " +lcColCursor+".COLOR"
	Endif

*--Make Temp File For Selected Style Groups
	lcGrpCursor =""
	lnStyGroup = Ascan(loogScroll.laOgFxFlt,"STYLE.CSTYGROUP")
	lnStyGroup = Asubscript(loogScroll.laOgFxFlt,lnStyGroup,1)
	lcGroups= loogScroll.laOgFxFlt[lnStyGroup,6]
	If !Empty(lcGroups)
		If lnStyGroup > 0
			lcGrpCursor = loogScroll.gfTempName() &&Cursor Hold Selected Seasons
			Dimension laTempacstru[1,4]
			laTempacstru[1,1]='CSTYGROUP'
			laTempacstru[1,2]='C'
			laTempacstru[1,3]= 6
			laTempacstru[1,4]= 0
			gfCrtTmp(lcGrpCursor,@laTempacstru,"CSTYGROUP",lcGrpCursor,.F.)
			If !Empty(lcGroups)
				lnStart=1
				lnEnd=At('|',lcGroups)
				Do While lnEnd <> 0
					Select(lcGrpCursor)
					Append Blank
					Replace CSTYGROUP With Substr(lcGroups,lnStart,lnEnd-1)
					lcGroups = Stuff(lcGroups ,lnStart,lnEnd,"")
					lnEnd=At('|',lcGroups)
				Enddo
				If lnEnd = 0
					Select(lcGrpCursor)
					Append Blank
					Replace CSTYGROUP With lcGroups
				Endif
			Endif
		Endif
	Endif

	If !Empty(lcGrpCursor)
		Use In &lcGrpCursor
		lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+ lcGrpCursor+".dbf" + " ON  STYLE.CSTYGROUP  = " +lcGrpCursor+".CSTYGROUP "
	Endif

*--(B)
*-- Retrieve cursor hold the required fields From item file [Begin]
	If  !Empty(lcTypeVal) && if the user select some fabrics or some Item_types
		If !Empty(lcStyFab) And  Used(lcStyFab) And  Reccount(lcStyFab)> 0
			llFabric=.F.
			lcSelFab=""
			lcFabric=loogScroll.gfTempName()
			lcSelFld1= "SELECT DISTINCT ITEM.CSTYMAJOR AS FABRIC ,ITEM.ITEM_TYPE FROM ITEM "
*--Add Filter of Selected Primary Fabrics
			lcSelFab=lfBldSqlCur('ITEM.CSTYMAJOR','llFabric','lcSelFab','Fabric C(19)','CSTYMAJOR') &&IT'S A CURSOR HOLD THE SELECTED FABRBRICS
			If llFabric  && Fabrics selected in the in list browse
				lcSelFld1 = lcSelFld1 + " INNER JOIN " + lcSelFab + " TmpFabric ON TmpFabric.Fabric = ITEM.CSTYMAJOR "
			Endif
		Else
			lcSelFld1= "SELECT DISTINCT ITEM.CSTYMAJOR AS FABRIC ,ITEM.ITEM_TYPE FROM ITEM "
		Endif
*--Add the Filter of Inventory Type (Fabric OR Style).
		lcSelFld1= lcSelFld1 + "  WHERE ITEM.CINVTYPE= '" +lcInvType+"'"
*--Add the Item Type Filter.
		If !Empty(lcTypeVal)
			lcSelFld1= lcSelFld1 + " AND  ITEM.ITEM_TYPE  IN (" + lcTypeVal +")"
		Endif
		lnResult1 = loogScroll.oRDA.SqlRun(lcSelFld1,lcFabric,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",Set("Datasession" ))

		If lnResult1 >=1
			Select (lcFabric)
			lnBuffering = CursorGetProp("Buffering",lcFabric)
			=CursorSetProp("Buffering",3,lcFabric)
			Index On FABRIC Tag lcFabric
		Endif
*-- Retrieve cursor hold the required fields From item file [End]

*--Filter the style file according to the selected fabrics&item_types [Begin]
		If !Empty(lcFabric) And Used(lcFabric) And Reccount(lcFabric) > 0
			Select(lcFabric)
			lcFabric1 = loogScroll.gfTempName()
			Copy To oAriaApplication.WorkDir+lcFabric1+".dbf"
			lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcFabric1 +".Dbf" + " ON  STYLE.Fabric = " + lcFabric1 +".Fabric"
		Endif
*--Filter the style file according to the selected fabrics&item_types [End]

	Endif  &&End of Filter the Fabric File & style file according to this fabric File
*--(B)

	lcStyCond=""

*-- Variable Hold only the Selected Style Pattern From OG.
	lcStyPat = loogScroll.laOgFxFlt[7,6]

	If !Empty(lcStyPat)
		lcStyCond= "  STYLE.PATTERN = " + "'"+lcStyPat+"'" &&Where Condition
	Endif

*--Create variable have the selected status
	lcStatusVal =""
	If !Empty(loogScroll.laOgFxFlt[8,6])
		lcStatusVal ="'"+Strtran(loogScroll.laOgFxFlt[8,6],"|","','")+"'"
		If !Empty(lcStyCond)
			lcStyCond= lcStyCond + " AND INLIST(STYLE.STATUS ," + lcStatusVal  + ") " &&Where Condition
		Else
			lcStyCond= " INLIST(STYLE.STATUS ,"+ lcStatusVal  + ") " &&Where Condition
		Endif
	Endif

*--Add filter of domestic&imported styles
	If lcRPDomImp <> 'B'
		If lcRPDomImp = 'D'   &&Domestic
			If !Empty(lcStyCond)
				lcStyCond= lcStyCond + " AND STYLE.Make = .T. "  &&Where Condition
			Else
				lcStyCond= "STYLE.Make =.T. " &&Where Condition
			Endif
		Else     &&Imported
			If !Empty(lcStyCond)
				lcStyCond= lcStyCond + " AND STYLE.Make = .F. "  &&Where Condition
			Else
				lcStyCond= "STYLE.Make =.F. " &&Where Condition
			Endif
		Endif
	Endif
*--Retrieve Cursor Hold the Styles after adding all filters which made on Style file.

*B127396,1 AMH Fix bug of missing styles in case of select no styles [Start]
*=lfSlctFox(lcStyFlds,lcStyTable,lcStyle,lcStyCond)
*B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [Start]
*IF EMPTY(lcStyCond)
	SET STEP ON
	If Empty(lcStyCond) And !("INNER JOIN" $ lcStyTable)

*B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [End]
		lcStyle = 'Style'
	ELSE
*B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [Start]
		If lcStyle = 'Style'
			lcStyle = loogScroll.gfTempName()
		Endif
*B131034,1 MMT 01/30/2006 Fix bug of Not applying the selection Criteria [End]


		=lfSlctFox(lcStyFlds,lcStyTable,lcStyle,lcStyCond)


	Endif
*B127396,1 AMH [End]

*--Close all Created dbfs which joined to Style File to create temp. File for style after Optimization(to enhance Performance)
	If File(oAriaApplication.WorkDir +lcStySty1+".DBF")
		If Used(lcStySty1)
			Use In &lcStySty1
		Endif

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcStySty1+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Endif
	If File(oAriaApplication.WorkDir +lcStyFab1+".DBF")
		If Used(lcStyFab1)
			Use In &lcStyFab1
		ENDIF

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcStyFab1+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Endif
	If File(oAriaApplication.WorkDir +lcStyDept1+".DBF")
		If Used(lcStyDept1)
			Use In &lcStyDept1
		ENDIF

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcStyDept1+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Endif
	If File(oAriaApplication.WorkDir +lcDivCursor+".DBF")
**: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcDivCursor+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Endif
	If File(oAriaApplication.WorkDir +lcSeaCursor+".DBF")
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcSeaCursor+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Endif
	If File(oAriaApplication.WorkDir +lcColCursor+".DBF")
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcColCursor+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	ENDIF


	If File(oAriaApplication.WorkDir +lcGrpCursor+".DBF")
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcGrpCursor+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Endif
	If File(oAriaApplication.WorkDir +lcFabric1+".DBF")
		If Used(lcFabric1)
			Use In &lcFabric1
		ENDIF

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*ERASE oAriaApplication.WorkDir+lcFabric1+".DBF"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	ENDIF

*--check if there is no records with the previous criteria in style file,Display message No Records to Display

	Select (lcStyle)
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start]
	IF llRpShMin
		lcOldFltExp = FILTER()
		IF !EMPTY(lcOldFltExp)
			lcNewFltExp = lcOldFltExp +" AND TotStk+TOTWIP < nMinInv"
			SET FILTER TO &lcNewFltExp.
		ELSE
			SET FILTER TO TotStk+TOTWIP < nMinInv
		ENDIF
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
	LOCATE


	If Eof()
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00052B40011',LANG_ICSTYREP_ALERT)
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

		IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
			=gfModalGen('TRM00052B40011',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_ALERT,oAriaApplication.GetHeaderText("LANG_ICSTYREP_ALERT",AHEADERFILE)))
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
		ENDIF

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

		Return
	Endif
*-- 1.Style file  [END]

*-- 2.StyDye file  [BEGIN]

*-- Variable Hold only the Selected locations From OG.
	lcStyLoc = loogScroll.laOgFxFlt[3,6]
	lcStyLoc1 =""
*--Retrieve Cursor From Style File(Fox File)which hold only Styles which had the selected locations.
*! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
*lcStyFlds="styDye.* "
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*  lcStyFlds="styDye.*,SUBSTR(styDye.Style,lnClrPo,lnColorLen) as Color "
	lcStyFlds="styDye.* "
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*! B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
	lcStyTable="STYDYE "


	If !Empty(lcStyLoc)

		Select(lcStyLoc)
		If Reccount() > 0
			lcStyLoc1 = loogScroll.gfTempName()
			Copy To oAriaApplication.WorkDir+lcStyLoc1+".dbf"
			lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyLoc1+".dbf" + " ON  STYDYE.CWARECODE = " +lcStyLoc1+".CWARECODE"
		ENDIF


	Endif

*HMA ,to improve the performance(Begin)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*If !Empty(lcStyle) And Used(lcStyle)
	If !Empty(lcStyle) And Used(lcStyle) And lcStyle != 'Style'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
		Select(lcStyle)
		If Reccount() > 0
			lcStyle1 = loogScroll.gfTempName()
			Copy To oAriaApplication.WorkDir+lcStyle1+".dbf"
			lcStyTable = lcStyTable + "  INNER JOIN  " + oAriaApplication.WorkDir+lcStyle1+".dbf" + " ON  STYDYE.STYLE = " +lcStyle1+".STYLE"
		Endif
	ENDIF

*HMA ,to improve the performance(End)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*lcStyCond=""
*=lfSlctFox(lcStyFlds,lcStyTable,lcStyDye,lcStyCond)
	If !('INNER JOIN' $ lcStyTable)
		lcStyDye ='STYDYE'
		lcStyDyeW = 'STYDYEW'
	Else
*! B612042,1 MMT 02/12/2020 The Style Summary takes time to collect data when user selects single transaction in option grid and clients data us huge[Start]
		If lcStyDye ='STYDYE'
			lcStyDye = loogScroll.gfTempName()
		Endif
*! B612042,1 MMT 02/12/2020 The Style Summary takes time to collect data when user selects single transaction in option grid and clients data us huge[End]
		=lfSlctFox(lcStyFlds,lcStyTable,lcStyDye,'')
	ENDIF

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	If File(oAriaApplication.WorkDir +lcStyLoc1+".DBF")
		If Used(lcStyLoc1)
			Use In &lcStyLoc1
		Endif
		Erase oAriaApplication.WorkDir+lcStyLoc1+".DBF"
	Endif
*HMA ,to improve the performance(Begin)
	If File(oAriaApplication.WorkDir +lcStyle1+".DBF")
		If Used(lcStyle1)
			Use In &lcStyle1
		Endif
		Erase oAriaApplication.WorkDir+lcStyle1+".DBF"
	ENDIF

*HMA , to improve the performance(End)
*--check if there is no records with the previous criteria in styDye file,Display message No Records to Display

	Select (lcStyDye)
	LOCATE
	If Eof()
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00052B40011',LANG_ICSTYREP_ALERT)
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start


		IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
			=gfModalGen('TRM00052B40011',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_ALERT,oAriaApplication.GetHeaderText("LANG_ICSTYREP_ALERT",AHEADERFILE)))
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
		ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

		Return
	Endif
*-- 2.StyDye file  [END]

*-- 5.WHSLOC file  [BEGIN]
	lcStyFlds="WhsLoc.Style,WhsLoc.Color,WhsLoc.cWareCode,WhsLoc.cLocation,WhsLoc.cOwner "
	lcStyTable="WHSLOC"
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*lcStyCond=""
* =lfSlctFox(lcStyFlds,lcStyTable,lcWhsLoc,lcStyCond)
	If !('INNER JOIN' $ lcStyTable)
		lcWhsLoc='WHSLOC'
		LCWHSLOCST = 'WHSLOCST'
	Else
		=lfSlctFox(lcStyFlds,lcStyTable,lcWhsLoc,"")
*-- 5.WHSLOC file  [END]
	Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*-- 6.SCALE file  [BEGIN]

* B131649,1 AYM 03/29/2006 Fix Bug of Scale.cscl_desc [Begin]
*!*	  lcStyFlds="Scale.Type,Scale.Scale,Scale.PrePak,Scale.SZ1,Scale.SZ2,Scale.SZ3,Scale.SZ4,Scale.SZ5,Scale.SZ6,Scale.SZ7,Scale.SZ8,Scale.Cnt "
	lcStyFlds="Scale.Type,Scale.Scale,Scale.PrePak,Scale.SZ1,Scale.SZ2,Scale.SZ3,Scale.SZ4,Scale.SZ5,Scale.SZ6,Scale.SZ7,Scale.SZ8,Scale.Cnt,Scale.Cscl_desc "
* B131649,1 AYM 03/29/2006 Fix Bug of Scale.cscl_desc [End]
	lcStyTable="SCALE"
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*lcStyCond=""
*=lfSlctFox(lcStyFlds,lcStyTable,lcscal,lcStyCond)
	If !('INNER JOIN' $ lcStyTable)
		lcscal='SCALE'
	Else
		=lfSlctFox(lcStyFlds,lcStyTable,lcscal,"")
	Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

*-- 6.SCALE file  [END]

*-- 7.WAREHOUS file  [BEGIN]
	lcStyFlds="WAREHOUS.cWareCode,WAREHOUS.cDesc "
	lcStyTable="WAREHOUS"
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*  lcStyCond=""
*=lfSlctFox(lcStyFlds,lcStyTable,lcWareHous,lcStyCond)
	If !('INNER JOIN' $ lcStyTable)
		lcWareHous = "WAREHOUS"
	Else
*-- 7.WAREHOUS file  [END]
		=lfSlctFox(lcStyFlds,lcStyTable,lcWareHous,"")
	Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*-- 8.IcSegVal file  [BEGIN]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
	If !Used('IcSegVal')
		=gfOpenTable('IcSegVal','SEGVAL','SH')
	Endif
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	lcStyFlds="IcSegVal.ciSegNo,IcSegVal.ciSegVal,IcSegVal.ciSgValSd  "
	lcStyTable="ICSEGVAL"
	lcStyCond=""
	=lfSlctFox(lcStyFlds,lcStyTable,lcSegVal,lcStyCond)
*-- 8.IcSegVal file  [END]
*--(C)

	If Empty(lcStyFab) And Empty(lcTypeVal)
		If lcRpSortBy =='MT'

			lcFabric=loogScroll.gfTempName()
			lcSelFld1= "SELECT ITEM.STYLE,ITEM.ITEM_TYPE FROM ITEM "
*--Add the Filter of Inventory Type (Fabric OR Style).
			lcSelFld1= lcSelFld1 + "  WHERE ITEM.CINVTYPE= '" +lcInvType+"'"
*--Add the Item Type Filter.
			lnResult1 = loogScroll.oRDA.SqlRun(lcSelFld1,lcFabric,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",Set("Datasession" ))
			If lnResult1 >=1
				Select (lcFabric)
				lnBuffering = CursorGetProp("Buffering",lcFabric)
				=CursorSetProp("Buffering",3,lcFabric)
				Index On Style Tag lcFabric
			Endif
		Endif
	Endif
*--(C)
*------------ Retrieve all Fox files used in program remotely [End]

Endif  &&end of filter changes
lcRpExp=""


*******************************START COLLLECTING DATA***************************************************************

lcTime     =  gfGetTime()
lnFrstRec  = 0
lnFrstRec1 = 0
llLinkGlJl = Alltrim(gfGetMemVar('M_LINK_GL')) = 'Y'
*B608189,1 HIA [Begin]
*xxxx
IF TYPE('lcXMLFileName') = 'C'
	lcRepMode ="Graphics"
ENDIF
*xxxx

lcRepMode = Upper(lcRepMode)
*B608189,1 HIA [END]
*-- llTextMode :  Hold .T. or .T. upon print in text format (Dos Mode) or graphic format.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llTextMode = (UPPER(ALLTRIM(lcRepMode))== LANG_ICSTYREP_TextMode)  && Print Text Format
llTextMode = (Upper(Alltrim(lcRepMode))== Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE)))  && Print Text Format
*N000682,1 11/20/2012 MMT Globlization changes[End]
Dimension laScals[8]
Store "" To laScals
Dimension laScalWs[8]
Store "" To laScalWs
*-- if sort by Style or By Location and user select one transaction to print.

*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [Begin][T20170721.0006]
*!* B611348,1 AHH 02/07/2017 How to export reports in excel  [Begin]
*!*	IF lcRpSortBy $ 'SW' AND ALEN(laRPRepTar,1) = 1
*!*	IF lcRpSortBy $ 'SW' AND ALEN(laRPRepTar,1) = 1 AND !((oAriaApplication.gcDevice = "FILE" .AND. loogScroll.cTextRepType = "EXCEL") AND "OTS" $ laRPRepTar[1])
*!* B611348,1 AHH 02/07/2017 How to export reports in excel  [End]
If lcRpSortBy $ 'SW' And Alen(laRPRepTar,1) = 1 And !((oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL"))
*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [End][T20170721.0006]
	=lfSWOneTrn()  && Call Style,Location One transaction Function.
	Return
ENDIF
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
llWipSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))) > 0
llStockSel =  Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))) > 0 Or ;
	ASCAN(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE))) > 0
llPlanSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))) > 0
llOTSSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))) > 0
llImmOTSSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))) > 0
llWorkOrderSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Work_orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Work_orders",AHEADERFILE))) > 0
llBookSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Book,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Book",AHEADERFILE))) > 0
llShipSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Shipped,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Shipped",AHEADERFILE))) > 0
llReturnSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return",AHEADERFILE))) > 0
llRetAuthSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return_auth,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return_auth",AHEADERFILE))) > 0
llUnAllocSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))) > 0
llAllocSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Allocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Allocated",AHEADERFILE))) > 0
llOrderSel = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Orders",AHEADERFILE))) > 0
llInTransSel=  Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Intransit,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Intransit",AHEADERFILE))) > 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
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
Store Space(0) To lcSortTtl,lcGroupExp,lcSortExp,lcMajExp,lcNMajExp,lcDescExp,lcSortFld,;
	lcSourFlds,lcTargFlds,lcAllTrns

lcStkFlds = "nStkVWIP,nStkVSOH,nStkVPLA,nStkVOTS,nStkVIOTS,nStkVBOK,;
             nStkVSHP,nStkVRet,nStkVRetA,nStkVAlo,nStkVUAlo,nStkVInt,;
             nStkVWOrd,nStkVOrd"


lcSalFlds = "nSalVWIP,nSalVSOH,nSalVPLA,nSalVOTS,nSalVIOTS,nSalVBOK,;
             nSalVSHP,nSalVRet,nSalVRetA,nSalVAlo,nSalVUAlo,nSalVInt,;
             nSalVWOrd,nSalVOrd"
Dimension laAllVal[1],laUAloVal[9],laIOTSVal[11],laOTSVal[11],laBokVal[9],;
	laStkVal[14],laSalVal[14]
Store 0 To laAllVal,laUAloVal,laIOTSVal,laOTSVal,laBokVal,laStkVal,laSalVal

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
lcStkFlds = ""
lcSalFlds = ""
Dimension laStkVal[1],laSalVal[1]
lnUsedCnt = 0
If llWipSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVWIP"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVWIP"
	lnUsedCnt = lnUsedCnt + 1
Endif
If llStockSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVSOH"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVSOH"
	lnUsedCnt = lnUsedCnt + 1
Endif
If llPlanSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVPLA"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVPLA"
	lnUsedCnt = lnUsedCnt + 1
Endif
If llOTSSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVOTS"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVOTS"
	lnUsedCnt = lnUsedCnt + 1
Endif
If llImmOTSSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVIOTS"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVIOTS"
	lnUsedCnt = lnUsedCnt + 1
Endif
If llBookSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVBOK"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVBOK"
	lnUsedCnt = lnUsedCnt + 1
Endif

If llShipSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVSHP"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVSHP"
	lnUsedCnt = lnUsedCnt + 1
Endif

If llReturnSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVRet"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVRet"
	lnUsedCnt = lnUsedCnt + 1
Endif

If llRetAuthSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVRetA"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVRetA"
	lnUsedCnt = lnUsedCnt + 1
Endif

If llAllocSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVAlo"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVAlo"
	lnUsedCnt = lnUsedCnt + 1
Endif


If llUnAllocSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVUAlo"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVUAlo"
	lnUsedCnt = lnUsedCnt + 1
Endif

If llInTransSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVInt"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVInt"
	lnUsedCnt = lnUsedCnt + 1
Endif

If llWorkOrderSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVWOrd"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVWOrd"
	lnUsedCnt = lnUsedCnt + 1
Endif

If llOrderSel
	Dimension laStkVal[lnUsedCnt +1],laSalVal[lnUsedCnt +1]
	lcStkFlds = lcStkFlds + Iif(Empty(lcStkFlds),'',',')+"nStkVOrd"
	lcSalFlds = lcSalFlds + Iif(Empty(lcSalFlds),'',',')+"nSalVOrd"
	lnUsedCnt = lnUsedCnt + 1
Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Store 0 To lnMajSV,lnSorSV,lnRepSV
*-- This is to create and declare the variables that hold the totals
*-- in the Group footer

Store .T. To llOnlyOTS
lcScale1 = Space(3)

If llRPPrnLoc And !llMultiWH
	llRPWhDeta = .T.
Endif


*B608189,3 TMI [START] be sure that the variable lcDummy is alwayes defined with non empty value
lcDummy = loogScroll.gfTempName()
*B608189,3 TMI [END  ]
= lfCrTmp()
= lfDummFill()
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_ICSTYREP_CollData NOWAIT

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

IF TYPE('lcXMLFileName') <> 'C'

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

	Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_CollData,oAriaApplication.GetHeaderText("LANG_ICSTYREP_CollData",AHEADERFILE)) Nowait

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

If lcRpSortBy = 'W'
	= lfDatCollW()
Else
	= lfDatCollS()
Endif

Wait Clear

lnOldRec = 0
lcOldFld = Space(1)



Go Top
If Eof()
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("INM00052B00000",LANG_ICSTYREP_Dialog)
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

		=gfModalGen("INM00052B00000",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dialog,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dialog",AHEADERFILE)))
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*N000682,1 11/20/2012 MMT Globlization changes[End]

	Return
Else

	If lcRpSortBy == 'SE' Or lcRpSortBy == 'D'
		For LnLop = 1 To 8
			lcSiz = 'SZ' + Alltrim(Str(LnLop))
			laScals[LnLop] = &lcscal..&lcSiz
		Endfor
	Endif

	If lcRpSortBy == 'SE' Or lcRpSortBy == 'D'
		Go Bottom
		Store Space(6) To lcSeason , lcDivision
		lcSeason   = SEASON
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* lcDivision = Division
		lcDivision = CDIVISION
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

		Locate For CENDREP = "A"

*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* Replace SEASON   With lcSeason ,;
Division With lcDivision

		Replace SEASON   With lcSeason ,;
			CDIVISION With lcDivision
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

		Locate
	Endif

*!* B611348,1 AHH 02/07/2017 How to export reports in excel  [Begin]
*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [Begin][T20170721.0006]
*!*	  IF (oAriaApplication.gcDevice = "FILE" .AND. loogScroll.cTextRepType = "EXCEL") AND llOnlyOTS
*!* B611545,1 SAH 03/04/2018 Modify the report program to replace line in which Style Summary report repeats records when exported to EXCEL when more than one transaction is selected [Begin] [T20180323.0003]
* If (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL") And Alen(laRPRepTar,1) = 1
	If (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL")
*!* B611545,1 SAH 03/04/2018 Modify the report program to replace line in which Style Summary report repeats records when exported to EXCEL when more than one transaction is selected [End] [T20180323.0003]
*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [End][T20170721.0006]
*B611409,1 AHH 11/09/2017 Style summary report export incorrect data [location details]= Yes [T20170905.0013][Start]
		If llRPWhDeta
			Select (lcTrns)
		Endif
*B611409,1 AHH 11/09/2017 Style summary report export incorrect data [location details]= Yes [T20170905.0013][End]
		Set Skip To
*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [Begin][T20170721.0006]
*!*	    SET FILTER TO lPrnOTS OR lPrnIOTS
		If llOnlyOTS
			Set Filter To lPrnOTS Or lPrnIOTS
		Else
			Set Filter To NotScale ='Y' And Empty(cEndMaj)
		Endif
*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [End][T20170721.0006]
		Locate
	Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
	lfClearMemory()
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

*!* B611348,1 AHH 02/07/2017 How to export reports in excel  [End]
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
		Do gfDispRe With Eval('lcRPFormNa')

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ELSE
		loProgress.Percent = 0.9
		loProgress.DESCRIPTION = "Printing Report..."
		loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
		PRIVATE loProxy
*loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
		loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
		loogScroll   = oAriaEnvironment.REPORT
		oAriaEnvironment.REPORT.OGLastForm = Eval('lcRPFormNa')
		oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
		loProgress.Percent = 1.0
		loProgress.DESCRIPTION = "Printing Report..."
		loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*!* B611348,1 AHH 02/07/2017 How to export reports in excel  [Begin]
*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [Begin][T20170721.0006]
*!*	  IF (oAriaApplication.gcDevice = "FILE" .AND. loogScroll.cTextRepType = "EXCEL") AND llOnlyOTS
	If (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL") And Alen(laRPRepTar,1) = 1
*!* B611367,1 AHH 24/07/2017 Style summary report doesnot export unlocated to Excel [End][T20170721.0006]
		Set Filter To
		Locate
	Endif
*!* B611348,1 AHH 02/07/2017 How to export reports in excel  [End]
Endif

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

Function lfwRepWhen
Local lnResult1

*--Get the mask of Major Segment & Color Segment
lnMajorlen=Len(gfItemMask("PM","",lcInvType))
lnColorLen=Len(gfItemMask("PN","",lcInvType))

*--Make Temp. File From Item Location File
If !llFirstTim
	lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC "
	lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"
	lnResult1 = loogScroll.oRDA.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",Set("Datasession" ))
	llFirstTim = .T.
Endif

If lnResult1 >=1
	lnBuffering = CursorGetProp("Buffering",lcFabDye)
	=CursorSetProp("Buffering",3,lcFabDye)
	Select (lcFabDye)
	Index On Style+CWARECODE+DYELOT Tag lcFabDye
Endif

*-- Check the cost access
If llCostAccs
	llShowCost = lcRPShow $ 'CB'
Else
	llShowCost = .F.
Endif

If llShowCost
	lcCostMth = Alltrim(Upper(gfGetMemVar('M_COST_MET')))
Endif
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

Function lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
Dimension laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
For lnI = lnMajSeg + 1 To Alen(laMajSeg,1)
	lnNonMajPo = Iif(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
	If laMajSeg[lnI,1] = 'F' And !llStopConc
		lcFreeClr  = Iif(Empty(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
		lcNonMajPi = Iif(Empty(lcNonMajPi),laMajSeg[lnI,3],;
			lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
		lcNonMajT  = Iif(Empty(lcNonMajT),Padr(laMajSeg[lnI,2],Len(laMajSeg[lnI,3])),;
			lcNonMajT + laMajSeg[lnI-1,6] + Padr(laMajSeg[lnI,2],Len(laMajSeg[lnI,3])))
	Endif
*-- If you Find Color Type or Find Free Type and current type not Free.
	If laMajSeg[lnI,1] = 'C' Or (!Empty(lcFreeClr) And laMajSeg[lnI,1] != 'F')
		If laMajSeg[lnI,1] = 'C'
			lnClrPo    = laMajSeg[lnI,4]
			lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
			lcNonMajPi = laMajSeg[lnI,3]
			lcNonMajT  = Padr(laMajSeg[lnI,2],Len(laMajSeg[lnI,3]))
			Exit
		Else
*-- this means that another type is found rather than color or free
*-- and so we neednot to concat. to free variables
			llStopConc = .T.
		Endif
	Endif   && end If you Find Color Type or Find Free Type and current type not Free.
Endfor    && end Loop Around Non Major elements.

Store Len(lcNonMajPi) To lnFreeLen , lnColorLen
lcColorTt = Alltrim(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

*--Retrive the value of some Setups
Dimension laSetUp[5,2]
laSetUp[1,1] = 'M_WAREHOUS' &&
laSetUp[2,1] = 'M_WARELOC'  &&
laSetUp[3,1] = 'M_DYELOT'   &&
laSetUp[4,1] = 'M_COST_MET' &&
laSetUp[5,1] = 'M_STYCNFG' &&

=gfGetMemVar(@laSetUp)

llMultiWH = Alltrim(laSetUp[1,2]) = 'Y'
llTrakLoc = Alltrim(laSetUp[2,2]) = 'Y'
llDyelot  = Alltrim(laSetUp[3,2]) = 'Y'
lcCstMeth = Alltrim(laSetUp[4,2])
llConfig  = Alltrim(Upper(laSetUp[5,1])) = 'Y'

llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')

Return ''

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

Function lfvPrint

If llCostAccs
	llShowCost = lcRPShow $ 'CB'
Else
	llShowCost = .F.
Endif

If llShowCost
	lcCostMth = Alltrim(Upper(gfGetMemVar('M_COST_MET')))
Endif

llShowSale = lcRPShow $ 'SB'

If (lcLastSel $ 'SCB' And lcRPShow = 'N') Or (lcLastSel = 'N' And lcRPShow $ 'SCB')
	Dimension laRPRepTar[1]
	laRPRepTar = Space(0)
Endif

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

Function lfvPrnReps

= lfTransArr()

*-- This is to clear target array in case the user change what he want to
*-- print
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= lfOGMover(@laRPRepSou,@laRPRepTar,LANG_ICSTYREP_PrintedTranscations ,.T.,"lfvPrtSize",.F.,.T.)
= lfOGMover(@laRPRepSou,@laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_PrintedTranscations,oAriaApplication.GetHeaderText("LANG_ICSTYREP_PrintedTranscations",AHEADERFILE)) ,.T.,"lfvPrtSize",.F.,.T.)
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
Function RefreshTrans
Local lcTransactionsStr, lnTarget
lcTransactionsStr = ""
If !Empty(laRPRepTar)
	For lnTarget = 1 To Alen(laRPRepTar,1)
		lcTransactionsStr = lcTransactionsStr + ", " + laRPRepTar[lnTarget]
	Endfor
	lcTransactionsStr = Substr(lcTransactionsStr,3)
Endif
Return lcTransactionsStr
Endfunc
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

Function lfTransArr

If Type('lcTrns')='U'
	lcTrns   = loogScroll.gfTempName()
	lcStyTmp = loogScroll.gfTempName()
	lcTotTmp = loogScroll.gfTempName()
	lcDummy  = loogScroll.gfTempName()
Endif

If Type('laRPRepSou[1,1]') = 'U' Or Empty(laRPRepSou[1,1])
	Dimension laRPRepSou[1]
	Store Space(0) To laRPRepSou
Endif

If Type('laRPRepTar[1,1]') = 'U' Or Empty(laRPRepTar[1,1])
	Dimension laRPRepTar[1]
	Store Space(0) To laRPRepTar
Endif
*E124814,1 HMA 07/04/2005,remove plan Trans. from the selected trans. while Print Location Bins =Yes [Begin]
* If Configuration=Yes & Print Bins=Yes   ,Remove PLAN & Remove DYELOT
* If Configuration=Yes & Print Bins=No    ,Remove PLAN & Remove DYELOT
* If Configuration=NO & Print Bins=Yes    ,Remove PLAN & Don't Remove DYELOT
* If Configuration=No & Print Bins=No     ,Don't Remove PLAN &Don't Remove DYELOT


If !llRpConfig And !llRPPrnLoc

	Dimension laAllTrns[15,2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laAllTrns[01,1] = LANG_ICSTYREP_WIP
*laAllTrns[02,1] = LANG_ICSTYREP_Stock
*laAllTrns[03,1] = LANG_ICSTYREP_Dyelot
*laAllTrns[04,1] = LANG_ICSTYREP_Plan
*laAllTrns[05,1] = LANG_ICSTYREP_Unallocated
*laAllTrns[06,1] = LANG_ICSTYREP_OTS
*laAllTrns[07,1] = LANG_ICSTYREP_Imm_OTS
*laAllTrns[08,1] = LANG_ICSTYREP_Orders
*laAllTrns[09,1] = LANG_ICSTYREP_Work_orders
*laAllTrns[10,1] = LANG_ICSTYREP_Intransit
*laAllTrns[11,1] = LANG_ICSTYREP_Book
*laAllTrns[12,1] = LANG_ICSTYREP_Shipped
*laAllTrns[13,1] = LANG_ICSTYREP_Allocated
*laAllTrns[14,1] = LANG_ICSTYREP_Return
*laAllTrns[15,1] = LANG_ICSTYREP_Return_auth
	laAllTrns[01,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
	laAllTrns[02,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
	laAllTrns[03,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE))
	laAllTrns[04,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))
	laAllTrns[05,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
	laAllTrns[06,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))
	laAllTrns[07,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))
	laAllTrns[08,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Orders",AHEADERFILE))
	laAllTrns[09,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Work_orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Work_orders",AHEADERFILE))
	laAllTrns[10,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Intransit,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Intransit",AHEADERFILE))
	laAllTrns[11,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Book,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Book",AHEADERFILE))
	laAllTrns[12,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Shipped,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Shipped",AHEADERFILE))
	laAllTrns[13,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Allocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Allocated",AHEADERFILE))
	laAllTrns[14,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return",AHEADERFILE))
	laAllTrns[15,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return_auth,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return_auth",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


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

Else
	If (llRpConfig And llRPPrnLoc) Or (llRpConfig And !llRPPrnLoc)
		Dimension laAllTrns[13,2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laAllTrns[01,1] = LANG_ICSTYREP_WIP
*laAllTrns[02,1] = LANG_ICSTYREP_Stock
*laAllTrns[03,1] = LANG_ICSTYREP_Unallocated
*laAllTrns[04,1] = LANG_ICSTYREP_OTS
*laAllTrns[05,1] = LANG_ICSTYREP_Imm_OTS
*laAllTrns[06,1] = LANG_ICSTYREP_Orders
*laAllTrns[07,1] = LANG_ICSTYREP_Work_orders
*laAllTrns[08,1] = LANG_ICSTYREP_Intransit
*laAllTrns[09,1] = LANG_ICSTYREP_Book
*laAllTrns[10,1] = LANG_ICSTYREP_Shipped
*laAllTrns[11,1] = LANG_ICSTYREP_Allocated
*laAllTrns[12,1] = LANG_ICSTYREP_Return
*laAllTrns[13,1] = LANG_ICSTYREP_Return_auth
		laAllTrns[01,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
		laAllTrns[02,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
		laAllTrns[03,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
		laAllTrns[04,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))
		laAllTrns[05,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))
		laAllTrns[06,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Orders",AHEADERFILE))
		laAllTrns[07,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Work_orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Work_orders",AHEADERFILE))
		laAllTrns[08,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Intransit,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Intransit",AHEADERFILE))
		laAllTrns[09,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Book,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Book",AHEADERFILE))
		laAllTrns[10,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Shipped,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Shipped",AHEADERFILE))
		laAllTrns[11,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Allocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Allocated",AHEADERFILE))
		laAllTrns[12,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return",AHEADERFILE))
		laAllTrns[13,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return_auth,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return_auth",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
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
	Else
		If !llRpConfig And llRPPrnLoc
			Dimension laAllTrns[14,2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laAllTrns[01,1] = LANG_ICSTYREP_WIP
*laAllTrns[02,1] = LANG_ICSTYREP_Stock
*laAllTrns[03,1] = LANG_ICSTYREP_Dyelot
*laAllTrns[04,1] = LANG_ICSTYREP_Unallocated
*laAllTrns[05,1] = LANG_ICSTYREP_OTS
*laAllTrns[06,1] = LANG_ICSTYREP_Imm_OTS
*laAllTrns[07,1] = LANG_ICSTYREP_Orders
*laAllTrns[08,1] = LANG_ICSTYREP_Work_orders
*laAllTrns[09,1] = LANG_ICSTYREP_Intransit
*laAllTrns[10,1] = LANG_ICSTYREP_Book
*laAllTrns[11,1] = LANG_ICSTYREP_Shipped
*laAllTrns[12,1] = LANG_ICSTYREP_Allocated
*laAllTrns[13,1] = LANG_ICSTYREP_Return
*laAllTrns[14,1] = LANG_ICSTYREP_Return_auth
			laAllTrns[01,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
			laAllTrns[02,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
			laAllTrns[03,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE))
			laAllTrns[04,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
			laAllTrns[05,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))
			laAllTrns[06,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))
			laAllTrns[07,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Orders",AHEADERFILE))
			laAllTrns[08,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Work_orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Work_orders",AHEADERFILE))
			laAllTrns[09,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Intransit,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Intransit",AHEADERFILE))
			laAllTrns[10,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Book,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Book",AHEADERFILE))
			laAllTrns[11,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Shipped,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Shipped",AHEADERFILE))
			laAllTrns[12,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Allocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Allocated",AHEADERFILE))
			laAllTrns[13,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return",AHEADERFILE))
			laAllTrns[14,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return_auth,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return_auth",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
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
		Endif
	Endif
ENDIF




*--Fill the transaction array according to the Print Costing Option.[BEGIN]
*--S For sales value
*--C For cost value
*--B For both
*--N For none

If Upper(lcRPShow) $ 'SCB'
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
	If llRpConfig Or llRPPrnLoc
		Dimension laRPRepSou[5]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laRPRepSou[01] = LANG_ICSTYREP_WIP
*laRPRepSou[02] = LANG_ICSTYREP_Stock
*laRPRepSou[03] = LANG_ICSTYREP_Unallocated
*laRPRepSou[04] = LANG_ICSTYREP_OTS
*laRPRepSou[05] = LANG_ICSTYREP_Imm_OTS
		laRPRepSou[01] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
		laRPRepSou[02] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
		laRPRepSou[03] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
		laRPRepSou[04] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))
		laRPRepSou[05] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	Else
		Dimension laRPRepSou[6]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laRPRepSou[01] = LANG_ICSTYREP_WIP
*laRPRepSou[02] = LANG_ICSTYREP_Stock
*laRPRepSou[03] = LANG_ICSTYREP_Plan
*laRPRepSou[04] = LANG_ICSTYREP_Unallocated
*laRPRepSou[05] = LANG_ICSTYREP_OTS
*laRPRepSou[06] = LANG_ICSTYREP_Imm_OTS
		laRPRepSou[01] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
		laRPRepSou[02] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
		laRPRepSou[03] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))
		laRPRepSou[04] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
		laRPRepSou[05] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))
		laRPRepSou[06] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
	Endif
Else
	Dimension laRPRepSou[14]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laRPRepSou[01] = LANG_ICSTYREP_WIP
*laRPRepSou[02] = LANG_ICSTYREP_Stock
*laRPRepSou[03] = LANG_ICSTYREP_Plan
*laRPRepSou[04] = LANG_ICSTYREP_Unallocated
*laRPRepSou[05] = LANG_ICSTYREP_OTS
*laRPRepSou[06] = LANG_ICSTYREP_Imm_OTS
*laRPRepSou[07] = LANG_ICSTYREP_Orders
*laRPRepSou[08] = LANG_ICSTYREP_Work_orders
*laRPRepSou[09] = LANG_ICSTYREP_Intransit
*laRPRepSou[10] = LANG_ICSTYREP_Book
*laRPRepSou[11] = LANG_ICSTYREP_Shipped
*laRPRepSou[12] = LANG_ICSTYREP_Allocated
*laRPRepSou[13] = LANG_ICSTYREP_Return
*laRPRepSou[14] = LANG_ICSTYREP_Return_auth
	laRPRepSou[01] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
	laRPRepSou[02] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
	laRPRepSou[03] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))
	laRPRepSou[04] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
	laRPRepSou[05] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))
	laRPRepSou[06] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))
	laRPRepSou[07] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Orders",AHEADERFILE))
	laRPRepSou[08] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Work_orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Work_orders",AHEADERFILE))
	laRPRepSou[09] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Intransit,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Intransit",AHEADERFILE))
	laRPRepSou[10] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Book,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Book",AHEADERFILE))
	laRPRepSou[11] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Shipped,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Shipped",AHEADERFILE))
	laRPRepSou[12] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Allocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Allocated",AHEADERFILE))
	laRPRepSou[13] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return",AHEADERFILE))
	laRPRepSou[14] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return_auth,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return_auth",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

Endif
*--Fill the transaction array according to the Print Costing Option.[END]

*--Fill the transaction array according to the Print Configuration option.[BEGIN]
*IF llRpConfig AND ! UPPER(lcRPShow) $ 'SCB'
If (llRpConfig Or llRPPrnLoc) And ! Upper(lcRPShow) $ 'SCB'
	Dimension laRPRepSou[13]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laRPRepSou[01] = LANG_ICSTYREP_WIP
*laRPRepSou[02] = LANG_ICSTYREP_Stock
*laRPRepSou[03] = LANG_ICSTYREP_Unallocated
*laRPRepSou[04] = LANG_ICSTYREP_OTS
*laRPRepSou[05] = LANG_ICSTYREP_Imm_OTS
*laRPRepSou[06] = LANG_ICSTYREP_Orders
*laRPRepSou[07] = LANG_ICSTYREP_Work_orders
*laRPRepSou[08] = LANG_ICSTYREP_Intransit
*laRPRepSou[09] = LANG_ICSTYREP_Book
*laRPRepSou[10] = LANG_ICSTYREP_Shipped
*laRPRepSou[11] = LANG_ICSTYREP_Allocated
*laRPRepSou[12] = LANG_ICSTYREP_Return
*laRPRepSou[13] = LANG_ICSTYREP_Return_auth
	laRPRepSou[01] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
	laRPRepSou[02] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
	laRPRepSou[03] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
	laRPRepSou[04] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))
	laRPRepSou[05] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))
	laRPRepSou[06] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Orders",AHEADERFILE))
	laRPRepSou[07] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Work_orders,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Work_orders",AHEADERFILE))
	laRPRepSou[08] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Intransit,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Intransit",AHEADERFILE))
	laRPRepSou[09] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Book,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Book",AHEADERFILE))
	laRPRepSou[10] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Shipped,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Shipped",AHEADERFILE))
	laRPRepSou[11] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Allocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Allocated",AHEADERFILE))
	laRPRepSou[12] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return",AHEADERFILE))
	laRPRepSou[13] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return_auth,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return_auth",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

Endif
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
Function lfvConfig

If llRpConfig
	= lfTransArr() &&fill again the transaction menu after removing PLAN transaction.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llPlanFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_Plan) > 0
	llPlanFoun = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lnArrLen = Alen(laRPRepTar)
	If llPlanFoun  && delete PLAN from the selected transaction menu(target array).
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= ADEL(laRPRepTar,ASCAN(laRPRepTar,LANG_ICSTYREP_Plan))
		= Adel(laRPRepTar,Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		If Alen(laRPRepTar,1) > 1
			Dimension laRPRepTar[lnArrLen - 1]
		Else
			Store Space(0) To laRPRepTar
		Endif
	Endif
Endif


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

Function lfvSort

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llPlanFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_Plan) > 0
llPlanFoun = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]


If (lcRpSortBy = "W" Or llRPWhDeta) And llPlanFoun
*-- "Plan" cannot be printed while sorting by Location or printing locations detail
*-- <OK>
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
		= gfModalGen("INM42147B00000","Dialog")
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

	lnArrLen = Alen(laRPRepTar)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= ADEL(laRPRepTar,ASCAN(laRPRepTar,LANG_ICSTYREP_Plan))
	= Adel(laRPRepTar,Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	If Alen(laRPRepTar,1) > 1
		Dimension laRPRepTar[lnArrLen - 1]
	Else
		Store Space(0) To laRPRepTar
	Endif
Endif

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
Function lfClrRead
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
Function lfvPrnWhDet
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
Function lfvPrnBins

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llPlanFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_Plan) > 0
llPlanFoun = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]


If llRPPrnLoc
*-- "Plan" cannot be printed while Print Location Bins=Yes
	lnArrLen = Alen(laRPRepTar)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= ADEL(laRPRepTar,ASCAN(laRPRepTar,LANG_ICSTYREP_Plan))
	= Adel(laRPRepTar,Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	If Alen(laRPRepTar,1) > 1
		Dimension laRPRepTar[lnArrLen - 1]
	Else
		Store Space(0) To laRPRepTar
	Endif
Endif


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
Function lfvOts

Do Case
Case lcRPOTSSig = 'P'
	lnRPOTSMin = 1
Case lcRPOTSSig = 'N'
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*lnRPOTSMin = -1
	lnRPOTSMax = -1
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
Endcase

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
Function lfPrtSzScl
loogScroll.EnableObject('LLRPSCALE',LLRPPRTSIZ)
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
Function lfSRSty
Parameters lcParm

Do Case
Case lcParm = 'S'  && Set code
*-- open this file in another alias to set order to Style Major
*-- unique index.
	Use (oAriaApplication.DataDir+'Style') Again Alias STYLE_X Order Tag Style   In  0
	Select Style
	Set Order To Tag Cstyle
	Set Relation To Style.Style Into STYLE_X
	Locate
Case lcParm = 'R'  && Reset code
	Use In STYLE_X
	Select Style
	Set Order To Tag Style
Endcase
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

Function lfOTSbStat
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llOTSFoun = ASCAN(laRPRepTar,LANG_ICSTYREP_OTS) > 0 OR ASCAN(laRPRepTar,LANG_ICSTYREP_Imm_OTS) > 0
llOTSFoun = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))) > 0 Or;
	ASCAN(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]


If llOTSFoun  &&if found OTS one of the selected trans.
	If !(lcRpSortBy = "W" ) And  !llRPWhDeta
		llStatOB = .T.
		llStatOS = .T.
		llStatOM = .T.
	Else
*-- if OTS is selected but the sort by location or print location detail
*-- then make the OTS based on WIP "W" and disable this option "OTS based on"
*-- because this will be from StyDye file which has not plan fields
		lcRPOTSB = 'W'
		llStatOB = .F.
		llStatOS = .T.
		llStatOM = .T.
	Endif
	If llRpConfig &&if print configuration=yes,disable this object with default value='WIP'
		lcRPOTSB = 'W'
		llStatOB = .F.
	Endif
Else
	llStatOB = .F.
	llStatOS = .F.
	llStatOM = .F.
Endif

loogScroll.EnableObject('LCRPOTSB', llStatOB)
loogScroll.EnableObject("LCRPOTSSIG",llStatOS)
loogScroll.EnableObject("LNRPOTSMIN",llStatOM)
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
loogScroll.EnableObject("LNRPOTSMAX",llStatOM)
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

Function lfDyeDtStat

*-- just to adjust the status of "Print Dyelots for stock" in lfwRepWhen function

*-- if the system support dyelots
If llDyelot
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llStat = ASCAN(laRPRepTar,LANG_ICSTYREP_Stock) > 0
	llStat = Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]

Else
	llStat = .F.
Endif

If !llStat
	llRPPrnDye = .F.
Endif

loogScroll.EnableObject("LLRPPRNDYE", llStat)
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

Function lfWhDtStat

*-- just to adjust the status of "Print location detail" in lfwRepWhen function
*-- Show if the system multible or single location

*-- if it is not multi location OR it is multi location but the sort by
*-- location, we neednot location detail
llStat = llMultiWH And lcRpSortBy <> 'W'

loogScroll.EnableObject("LLRPWHDETA", llStat)

loogScroll.EnableObject("LLRPPRNLOC", llTrakLoc And ((llMultiWH And llRPWhDeta) Or !llMultiWH Or lcRpSortBy = 'W'))


If !llRPWhDeta And llMultiWH
	llRPPrnLoc=.F. &&set default value=NO if "print location detail" option is disabled
Endif

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

Function lfWhsOptSt
loogScroll.EnableObject("STYDYE.CWARECODE",(lcRpSortBy = "W" ) Or llRPWhDeta,"laOGFxFlt")
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

Function lfPrtSizSt
loogScroll.EnableObject('LLRPPRTSIZ', Alen(laRPRepTar,1) = 1)
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
Function lfChngForm
*-- if sort by location.
If lcRpSortBy = "W"
*-- if have one transaction only.
	If Alen(laRPRepTar,1) = 1 And !Empty(laRPRepTar)
		lcRPFormNa = "ICSTYBYL"
	Else
		lcRPFormNa = "ICSTYBYW"
	Endif
	llRPWhDeta = .F.

Else  && Sort by anything rather than location.

*-- if have one transaction only.
	If (lcRpSortBy == "S" ) And Alen(laRPRepTar,1) = 1 And !Empty(laRPRepTar)
		lcRPFormNa = "ICSTYBYS"
	Else
		lcRPFormNa = "ICSTYBYO"
	Endif

Endif

lcNo1 = lcRepMode
lcNo2 = lcOGPlatForm
=lfRepPltFr(lcRPFormNa)

If lcRepMode # lcNo1 Or lcOGPlatForm # lcNo2
	lcRepMode    = lcNo1
	lcOGPlatForm = lcNo2
Endif
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
Function lfVarPos
Parameters lcItmInFlt
Private lnItmPos
lnItmPos = Ascan(loogScroll.laOGObjType,Upper(lcItmInFlt))
If lnItmPos > 0
	lnItmPos = Asubscript(loogScroll.laOGObjType,lnItmPos,1)
Endif
Return lnItmPos

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
Function lfMajTtGet
Return gfItemMask("HM")

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
Function lfPvRun
*--lcsty1 var to get exp
*--lcsty2 var to get colors and concatenate it to lcsty1
Private lcSty1,lcSty2
Store 0 To lcSty1,lcSty2

*-- get color length
Declare laItemSeg[1]
Store 0 To lnColorLen
=gfItemMask(@laItemSeg)
For lnCount = 1 To Alen(laItemSeg,1)
	If laItemSeg[lnCount,1]='C'
		lnColorLen = Len(laItemSeg[lnCount,3])
		Exit
	Endif
Endfor

*-- get color from array and change it
*-- get color position
lnClrSgPo = Ascan(loogScroll.laOgFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)',1,Alen(loogScroll.laOgFxFlt,1),1,10)
If lnClrSgPo = 0
	lcSty1 = ""
	Return .T.
Endif

*-- Get first color
lcSty1 = Substr(loogScroll.laOgFxFlt[lnClrSgPo,6],1,lnColorLen)

*-- loop for No. of Occurance of Separator "|" in Color exp. and add 1 to last color
For lnCounter = 1 To Occur("|",loogScroll.laOgFxFlt[lnClrSgPo,6])+1

*--get from second color to rest color
	If lnCounter > 1
*-- get  position of "|"
		lnFirstPos  = Atc('|',loogScroll.laOgFxFlt[lnClrSgPo,6],lnCounter-1)
*-- we add one to positon to substr after "|"
		lcSty2      = Substr(loogScroll.laOgFxFlt[lnClrSgPo,6],lnFirstPos+1,lnColorLen)
	Endif

	If !Empty(lcSty2)
*--Concatenate expression
		lcSty1 = lcSty1 + '|' + lcSty2
	Else

*--for chose first color only
		lcSty1 = lcSty1
	Endif
Endfor

loogScroll.laOgFxFlt[lnClrSgPo,6] = lcSty1

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

Function lfExtScale

*-- This is to check if the system use extended size or not
Return gfGetMemVar('M_USEEXSSC')

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

Function lfMajPic
lcMajPic = "@! " + gfItemMask("PM")
Return lcMajPic

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
Function lfSrvDep
Parameters lcParm

Do Case
Case lcParm = 'S'  && Set code
*-- open this file in another alias to set order to Department.
*-- unique index.
	Select ICDeptHd
	lcDeptTag = Order()
	Index On Dept Tag DeptUnique Of (oAriaApplication.WorkDir+lcStyTag+".CDX") Unique
	Set Order To DeptUnique
Case lcParm = 'R'  && Reset code
	Set Index To
	Set Order To (lcDeptTag)
Endcase
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
Function lfSumFab1
Parameters lcFab,lccomp
Private lnFabRec

Local lnAlias

lnAlias = Select()

lnTotcomp = 0
Select(lcFabDye)
If Reccount() != 0
	lnFabRec = Recno('ITEM')
	Select(lcFabDye)
	Locate
	If Seek(Substr(lcFab,1,lnMajorlen))
		Sum &lccomp To lnTotcomp While Substr(Style,1,lnMajorlen)= Substr(lcFab,1,lnMajorlen) And Empty(DYELOT)
	Endif
	Select Item
	If Between(lnFabRec,1,Reccount())
		Go lnFabRec
	Endif
Endif
Select(lnAlias)
Return Int(lnTotcomp)

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
Function lfStySum
Parameters lcSty,lccomp,lnAddToVar
Private lnStyRec
lnStyRec = Iif(Between(Recno('STYLE'),1,Reccount('STYLE')),Recno('STYLE'),1)

lnTotcomp = 0
Select STYLE_X
Set Order To Style
If Seek(Alltrim(lcSty))
	Sum &lccomp To lnTotcomp While CSTYMAJOR = lcSty
Endif

Select Style
Go lnStyRec

Do Case
Case lnAddToVar = 1
	lnO_T_S = lnTotcomp
Case lnAddToVar = 2
	lnO_T_S = lnO_T_S + lnTotcomp
Case lnAddToVar = 3
	lnO_T_S = lnO_T_S - lnTotcomp
Endcase

Return Int(lnTotcomp)

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
Function lfvPrtSize
Lparameters lnButtn

*-- if Remove or the option of printing the size is "Yes"
If Inlist(lnButtn,3,4) Or LLRPPRTSIZ
	Return .T.
Endif
Private lnMesButtn,llReturn

llReturn = .T.


*-- If more than one transaction are selected have a massege to tell that the
*-- option of the sizes can't be "No" then return it "Yes" and disable it
If (lnButtn = 2) Or ((Alen(laTarget,1) = 1) And !Empty(laTarget[1]))
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
		lnMesButtn = gfModalGen('QRM42215B00012',.F.,"",.F.,"")
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

	llReturn = lnMesButtn = 1
	LLRPPRTSIZ = llReturn

Endif && end If more than one transaction are selected

Return (llReturn)
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
Function lfSWOneTrn

Private lcStyorder,lcStyDyeorder
Store '' To lcStyorder,lcStyDyeorder
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

Dimension laTranAray[8],laTrnNonAr[8]
Store ''   To lcMastFile , lcChldFile , lcCost_Val , laTranAray ,;
	lcRepTarVl , lcPrnTitle , lcRevFile , lcCst_VlPn  ,;
	lcCst_VlOp , lcCst_VlGd , lcNonMjDes, lcExtndFlt  ,;
	laTrnNonAr , lcCst_VlCl , lcLocBins , lcPrintSty,lcCst_VlLc
Store '' To lcCst_VlC1,lcCst_VlC2,lcCst_VlP1,lcCst_VlP2,lcCst_VlO1,lcCst_VlO2,lcCst_VlG1,lcCst_VlG2


Store [""] To lcStyGroup , lcLocGroup , lcDyeGroup

Store 0 To lnEndOfSty , lnEndOfLoc , lnEndOfDye ,;
	lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
	lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11,lnMaxCnt,;
	lnNonPrice,lnNonCost

Store 0 To lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
	lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,lnStySz10,lnStySz11,;
	lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
	lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
	lnNonPrcOp,lnNonCstOp,lnNonPrcGd,lnNonCstGd

Store 0 To lnLocSz1,lnLocSz2,lnLocSz3,lnLocSz4,;
	lnLocSz5,lnLocSz6,lnLocSz7,lnLocSz8,lnLocSz9,lnLocSz10,lnLocSz11,;
	lnNonPrcLc,lnNonCstLc

Store 0 To lnClrSz1,lnClrSz2,lnClrSz3,lnClrSz4,;
	lnClrSz5,lnClrSz6,lnClrSz7,lnClrSz8,lnClrSz9,lnClrSz10,lnClrSz11,;
	lnClrPrice,lnClrCost,lnCstAveVl


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
Store .F. To llChkAllSz
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
llPrnClrLn = .F.
= lfTranAray()  && Fill Transaction Arrays.

If Empty(laTranAray[1])
	Return
Endif


lnMajorlen = Len(gfItemMask("PM"))  && Major Length

lcPrntLoc = Space(6)
lcPrntSty = Space(lnMajorlen)
lcPrntNon = Space(19 - lnMajorlen)
lnSelect=Select()
Select(lcStyle)
lcStyorder=Order()
Set Order To
RevStyle=loogScroll.gfTempName()


*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

IF TYPE('lcXMLFileName') <> 'C'

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [Start]
	oProgress = Newobject('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
	oProgress.TotalProgress = 100
	lcCaptionVal = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_CollData,oAriaApplication.GetHeaderText("LANG_ICSTYREP_CollData",AHEADERFILE))
	oProgress.lblFirstLabel.Caption = lcCaptionVal
	oProgress.Show()
	oProgress.CurrentProgress(10)
*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [End]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][Start]
lcDelSetBefCop = SET("Deleted")
SET DELETED OFF 
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][End]
Copy To oAriaApplication.WorkDir+RevStyle+".dbf"
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][Start]
SET DELETED &lcDelSetBefCop.
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][End]


*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

IF TYPE('lcXMLFileName') <> 'C'

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [Start]
	oProgress.CurrentProgress(20)
*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [End]


*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

If File(oAriaApplication.WorkDir +RevStyle+".DBF")
	If !Used(RevStyle)
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][Start]
*USE (oAriaApplication.WorkDir +RevStyle+".DBF") IN 0
		Use (oAriaApplication.WorkDir +RevStyle+".DBF") In 0 Exclusive
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][End]
	Endif
	Select(RevStyle)
	Index On Style Tag RevStyle

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [Start]
		oProgress.CurrentProgress(40)
*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [End]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

Endif
If !Empty(lcStyorder)
	Set Order To (lcStyorder) In &lcStyle

Endif
Select(lcStyDye)
lcStyDyeorder=Order()
Set Order To
RevStyDy=loogScroll.gfTempName()
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][Start]
lcDelSetBefCop = SET("Deleted")
SET DELETED OFF 
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][End]
Copy To oAriaApplication.WorkDir+RevStyDy+".dbf"
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][Start]
SET DELETED &lcDelSetBefCop.
*! B612671,1 MMT 05/09/2023 Some styles total lines not printed if there is deleted records in style table[T-ERP-20230505.0001][End]
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [Start]
	oProgress.CurrentProgress(60)
*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [End]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

If File(oAriaApplication.WorkDir +RevStyDy+".DBF")
	If !Used(RevStyDy)
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][Start]
*USE (oAriaApplication.WorkDir +RevStyDy+".DBF") IN 0
		Use (oAriaApplication.WorkDir +RevStyDy+".DBF") In 0 Exclusive
*! E303079,1 MMT 04/01/2012 Fixing Media issues[T20120304.0004][End]
	Endif
	Select(RevStyDy)
	Index On  Style+CWARECODE+DYELOT Tag RevStyDy

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [Start]
		oProgress.CurrentProgress(70)
*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [End]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

	Index On  CWARECODE+Style+DYELOT Tag RevStyDyW

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [Start]
		oProgress.CurrentProgress(80)
*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [End]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*Set Order To Tag lcStyle  In &lcStyle
Set Order To Tag (lcStyle)  In &lcStyle
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Set Order To Tag RevStyle In &RevStyle
If !Empty(lcStyDyeorder)
	Set Order To (lcStyDyeorder) In &lcStyDye
Endif
Select(lnSelect)

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [Start]
	oProgress.CurrentProgress(100)
	oProgress = Null
*B612019,1 Es 03/02/2020 The Style Summary report does not display the style total when report is sorted by Location and only one transaction is selected. [End]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
lcStyGroup = [PADR(&lcStyDye..Style,lnMajorLen)]

*-- if Sort by Style.
If lcRpSortBy == 'S'
	Set Order To Tag lcStyDye In &lcStyDye
	Set Order To Tag RevStyDy In &RevStyDy
	If llConfig
		llSetup= llRpConfig
	Else
		llSetup= llRPPrnDye
	Endif
	llPrintClr = llRPWhDeta Or llSetup && Get sizes from StyDye file.
	lcMastFile = '&lcStyDye'
	lcChldFile = '&lcStyle'
	lcRevFile  = '&REVSTYDY'
	llSChkDyeB = .F.
	If llPrintClr
		llSChkDyeB = .T.
		lcMastFile = '&lcStyDye'
		lcChldFile = '&lcStyle'
		lcRevFile  = '&REVSTYDY'
*-- if print locations with out print Configuration.
		If llRPWhDeta And !llSetup
			lcExtndFlt = [EMPTY(Dyelot)]
		Endif
	Else       && else Print style records only without any other details.
		lcMastFile = '&lcStyle'
		lcChldFile = '&lcStyDye'
		lcRevFile  = '&REVSTYLE'
	Endif      && end if print location detail or print Configuration.
Else         && else sort by location.
	llSChkDyeB = .T.
	llPrintClr = .F.
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*Set Order To Tag lcStyDyeW In &lcStyDye
	Set Order To Tag (lcStyDyeW) In &lcStyDye
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	Set Order To Tag RevStyDyW In &RevStyDy
	lcMastFile = '&lcStyDye'
	lcChldFile = '&lcStyle'
	lcRevFile  = '&REVSTYDY'
	lcLocGroup = [&lcStyDye..CWARECODE]
Endif  && end if sort by style or location

*-- What is line cost and Style Cost [Begin]
*-- Update this lines to calculate the stock value on the cost method in IC settings [start]

*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lcLineCost  = [Ave_Cost]
*  lcStyCost   = [&lcStyle..Ave_Cost]
*ELSE
*B608407,1 WAM 01/14/2008 (End)

If lcCstMeth = "S"
	lcLineCost  = [&lcStyle..TotCost]
	lcStyCost   = [&lcStyle..TotCost]
Else
	lcLineCost  = [Ave_Cost]
	lcStyCost   = [&lcStyle..Ave_Cost]
Endif
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)

*-- Update this lines to calculate the stock value on the cost method in IC settings [end]
*-- Prepair report cost and price variables [Begin]
Do Case
*-- Case show Unit Price and Unit Cost only.
Case lcRPShow = 'S'
*-- Per Header.
*B608189,1 TMI [Start] set the "Unit_price" to the correct alignment
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcCost_Val = IIF(lcRepMode = LANG_ICSTYREP_TextMode,''+LANG_ICSTYREP_Unit_Price + '  '+LANG_ICSTYREP_Sales_Value;
,SPACE(3)+LANG_ICSTYREP_Unit_Price + SPACE(3) + LANG_ICSTYREP_Sales_Value)
	lcCost_Val = Iif(lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE)),;
		''+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Price ,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Price",AHEADERFILE))+ '  '+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Sales_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Sales_Value",AHEADERFILE));
		,Space(3)+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Price ,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Price",AHEADERFILE))+ Space(3) +;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Sales_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Sales_Value",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*B608189,1 TMI [End  ]
*-- Per Line.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
' ' + TRANSFORM(lnNonPrice,'999999999.99'))"

		lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
                 ' ' + TRANSFORM(lnNonPrice,'999999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Else

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*	      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
*!*	      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99'))"
		lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
		lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnNonPrice,'999999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Endif
*-- Per Non Majors from style file.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
' ' + TRANSFORM(lnClrPrice,'999999999.99'))"
		lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(&lcStyle..PRICEA,'999999999.99') +;
                 ' ' + TRANSFORM(lnClrPrice,'999999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Else

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*	      lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
*!*	      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99'))"
		lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(&lcStyle..PRICEA,'999999999.99') )"
		lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrPrice,'999999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Endif
*-- Per Location Group.
	lcCst_VlLc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcLc,'999999999.99')"
*-- Per Style (Major) Group.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		lcCst_VlOp = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcOp,'999999999.99')"
	Else
		lcCst_VlO1 = "' '"
		lcCst_VlO2 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
	Endif

*-- Per Grand Band.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		lcCst_VlGd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcGd,'999999999.99')"
	Else
		lcCst_VlG1 = "' '"
		lcCst_VlG2 = "TRANSFORM(lnNonPrcGd,'999999999.99')"
	Endif

*-- Case show cost values only.
Case lcRPShow = 'C'
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lcCost_Val = IIF(lcRepMode = LANG_ICSTYREP_TextMode,'  '+LANG_ICSTYREP_Avrg_Cost + '  '+LANG_ICSTYREP_Cost_Value;
,SPACE(2)+' '+LANG_ICSTYREP_Avrg_Cost + SPACE(4) + LANG_ICSTYREP_Cost_Value)

*ELSE
*B608407,1 WAM 01/14/2008 (End)
	If lcCstMeth = "S"
		lcCost_Val = Iif(lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE)),;
			'  '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Cost,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Cost",AHEADERFILE)) + '  '+;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Value",AHEADERFILE));
			,Space(2)+' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Cost ,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Cost",AHEADERFILE))+;
			SPACE(4) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Value",AHEADERFILE)))
	Else
		lcCost_Val = Iif(lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE)),;
			'  '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Avrg_Cost,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Avrg_Cost",AHEADERFILE)) +;
			'  '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Value",AHEADERFILE));
			,Space(2)+' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Avrg_Cost ,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Avrg_Cost",AHEADERFILE))+;
			SPACE(4) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Value",AHEADERFILE)))
	Endif
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' +;
TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') +;
' ' + TRANSFORM(lnNonCost,'99999999.99'))"
		lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' +;
                 TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') +;
                 ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Else

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*	      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',;
*!*	                   TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') )"
*!*	      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"
		lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',;
                   TRANSFORM(IIF(lcMastFile = '&lcStyle' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') )"
		lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnNonCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Endif

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(EVALUATE(lcStyCost),'9999999.99') +;
' ' + TRANSFORM(lnClrCost,'99999999.99'))"
		lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(EVALUATE(lcStyCost),'9999999.99') +;
                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Else

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*	      lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(EVALUATE(lcStyCost),'9999999.99') )"
*!*	      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"
		lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(EVALUATE(lcStyCost),'9999999.99') )"
		lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[end]

	Endif

	lcCst_VlLc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstLc,'99999999.99')"

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		lcCst_VlOp = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
	Else
		lcCst_VlO1 = "' '"
		lcCst_VlO2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
	Endif

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		lcCst_VlGd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
	Else
		lcCst_VlG1 = "' '"
		lcCst_VlG2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
	Endif

*-- Case show both total Price and cost values.
Case lcRPShow = 'B'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*      lcCost_Val =  IIF(lcRepMode = LANG_ICSTYREP_TextMode,LANG_ICSTYREP_Sales_Value + '  '+LANG_ICSTYREP_Cost_Value;
*!*                                          ,SPACE(1)+LANG_ICSTYREP_Sales_Value +SPACE(3)+ LANG_ICSTYREP_Cost_Value)
	lcCost_Val =  Iif(lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE)),;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Sales_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Sales_Value",AHEADERFILE)) + '  '+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Value",AHEADERFILE));
		,Space(1)+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Sales_Value ,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Sales_Value",AHEADERFILE))+;
		SPACE(3)+ Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Value,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Value",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*	      lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnNonPrice,'999999999.99') +;
*!*	                   ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
		lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(lnNonPrice,'999999999.99') +;
                   ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Else

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*	      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99') )"
*!*	      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"

		lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer AND  !llChkAllSz,'',TRANSFORM(lnNonPrice,'999999999.99') )"
		lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer AND  !llChkAllSz,'',TRANSFORM(lnNonCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Endif

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnClrPrice,'999999999.99') +;
' ' + TRANSFORM(lnClrCost,'99999999.99'))"
		lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',' ' + TRANSFORM(lnClrPrice,'999999999.99') +;
                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Else

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*!*	      lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99') )"
*!*	      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"

		lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrPrice,'999999999.99') )"
		lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer AND !llChkAllSz,'',TRANSFORM(lnClrCost,'99999999.99'))"
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Endif

*: B608756,1 MMT 12/04/2008 Fix bug of Not displaying Sales value when sort by location[Start]
*lcCst_VlLc = "' ' + TRANSFORM(lnNonPrcLc,'999999999.99') +;
' ' + TRANSFORM(lnNonCstLc,'99999999.99')"
	lcCst_VlLc = "Space(2)+ TRANSFORM(lnNonPrcLc,'999999999.99') +;
                  SPACE(5)+ TRANSFORM(lnNonCstLc,'99999999.99')"
*: B608756,1 MMT 12/04/2008 Fix bug of Not displaying Sales value when sort by location[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		lcCst_VlOp = "' ' + TRANSFORM(lnNonPrcOp,'999999999.99') +;
                    ' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
	Else
		lcCst_VlO1 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
		lcCst_VlO2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
	Endif

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRepMode = LANG_ICSTYREP_TextMode
	If lcRepMode = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_TextMode,oAriaApplication.GetHeaderText("LANG_ICSTYREP_TextMode",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		lcCst_VlGd = "' ' + TRANSFORM(lnNonPrcGd,'999999999.99') +;
                    ' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
	Else
		lcCst_VlG1 = "TRANSFORM(lnNonPrcGd,'999999999.99')"
		lcCst_VlG2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
	Endif
Endcase
*-- Prepair report cost and price variables [End  ]

*-- lcRevFlt   : Variable hold Filter for REVSTYDY.
*-- laTrnRevAr : Array Hold Reference to printed transaction From REVSTYDY.


Dimension laTrnRevAr [8]
For lnI = 1 To 8
*B608406,1 WAM 01/14/2008 When Stock is selected, replace lcStydye with the new temp file
*laTrnRevAr [lnI] = STRTRAN(latrnNonAr[lnI],'&lcStyle',lcRevFile)
	laTrnRevAr [lnI] = Strtran(laTrnNonAr[lnI],Iif(lcRepTarVl == "SOH",lcStyDye,lcStyle),lcRevFile)
*B608406,1 WAM 01/14/2008 (End)
Endfor

*: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
*!*	lcRevFlt = "EVAL(laTrnRevAr[1])+EVAL(laTrnRevAr[2])+"+;
*!*	           "EVAL(laTrnRevAr[3])+EVAL(laTrnRevAr[4])+EVAL(laTrnRevAr[5])+"+;
*!*	           "EVAL(laTrnRevAr[6])+EVAL(laTrnRevAr[7])+EVAL(laTrnRevAr[8]) <> 0"

lcRevFlt = "EVAL(laTrnRevAr[1])<> 0 OR EVAL(laTrnRevAr[2]) <> 0 OR"+;
	"EVAL(laTrnRevAr[3])<> 0 OR EVAL(laTrnRevAr[4]) <> 0 OR EVAL(laTrnRevAr[5]) OR"+;
	"EVAL(laTrnRevAr[6])<> 0 OR EVAL(laTrnRevAr[7]) <> 0 OR EVAL(laTrnRevAr[8]) <> 0"
*: B608166,1 SSH 07/15/2007
If !Empty(lcExtndFlt)
	lcRpExp = lcRpExp + lcExtndFlt
	lcRevFlt = lcRevFlt + [ AND ] + lcExtndFlt
	Select (RevStyDy)
	Set Filter To &lcExtndFlt
Endif
If !llRPShwZer
	Select (lcRevFile)
	Set Filter To &lcRevFlt
Endif
If !llRPShwZer &&if don't show zeros quantities
	If llSChkDyeB And llRPPrnDye
		If !Empty(lcRpExp)

*:*B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
*!*	      lcRpExp = lcRpExp + " AND (EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
*!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
*!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"

			lcRpExp = lcRpExp + " AND (EVAL(laTrnNonar[1]) <> 0 OR EVAL(laTrnNonar[2])<>0 OR " +;
				"EVAL(laTrnNonar[3]) <> 0 OR EVAL(laTrnNonar[4]) <>0 OR EVAL(laTrnNonar[5]) <>0 OR " +;
				"EVAL(laTrnNonar[6]) <> 0 OR EVAL(laTrnNonar[7]) <>0 OR EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"
*: B608166,1 SSH 07/15/2007 [END]
		Else
*: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
*!*	      lcRpExp = lcRpExp + " (EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
*!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
*!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"

			lcRpExp = lcRpExp + " (EVAL(laTrnNonar[1]) <> 0 OR EVAL(laTrnNonar[2])<>0 OR"+;
				"EVAL(laTrnNonar[3]) <> 0 OR EVAL(laTrnNonar[4])<>0 OR EVAL(laTrnNonar[5])<>0 OR "+;
				"EVAL(laTrnNonar[6]) <>0 OR EVAL(laTrnNonar[7])<>0 OR EVAL(laTrnNonar[8]) <> 0 OR &lcStyDye..TOTSTK <> 0 )"
*: B608166,1 SSH 07/15/2007 [END]
		Endif
	Else
*B608507,1 WAM 04/08/2008 Fix filter to check inventory in style file not in STYDYE
		For lnI = 1 To 8
			laTrnNonAr[lnI] = Strtran(laTrnNonAr[lnI],Iif(lcRepTarVl == "SOH",lcStyDye,lcStyle),lcMastFile)
		Endfor
*B608507,1 WAM 04/08/2008 (End)

		If !Empty(lcRpExp)
*: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
*!*	      lcRpExp = lcRpExp + " AND EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
*!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
*!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0"

			lcRpExp = lcRpExp + " AND EVAL(laTrnNonar[1]) <> 0 OR EVAL(laTrnNonar[2]) <>0 OR "+;
				"EVAL(laTrnNonar[3]) <>0 OR EVAL(laTrnNonar[4])<>0 OR EVAL(laTrnNonar[5]) <>0 OR "+;
				"EVAL(laTrnNonar[6])<>0 OR EVAL(laTrnNonar[7])<>0 OR EVAL(laTrnNonar[8]) <> 0"
*: B608166,1 SSH 07/15/2007 [END]
		Else
*: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
*!*	      lcRpExp = lcRpExp + " EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
*!*	                "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
*!*	                "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0"
			lcRpExp = lcRpExp + " EVAL(laTrnNonar[1])<>0 OR EVAL(laTrnNonar[2]) <>0 OR "+;
				"EVAL(laTrnNonar[3])<>0 OR EVAL(laTrnNonar[4])<>0 OR EVAL(laTrnNonar[5])<>0 OR "+;
				"EVAL(laTrnNonar[6])<>0 OR EVAL(laTrnNonar[7])<>0 OR EVAL(laTrnNonar[8]) <> 0"
*: B608166,1 SSH 07/15/2007 [END]
		Endif
	ENDIF
*! E612104,1 MMT 10/14/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start]
	If ("OTS" $ lcRepTarVl)
	  *B612540,1 MMT 03/30/2022 Style report deos not print styles that has some sizes with negative or positive OTS Qty if the Sum of the sizes OTS is not negative or positive[T20220323.0001][Start] 
*!*			lcRpExp = "("+lcRpExp + ") AND ("+ '(((lcRPOTSSig ="P") And (TOTSTK+Iif(lcRPOTSB="W",TOTWIP,totplan) -TOTORD >= lnRPOTSMin)) Or ((lcRPOTSSig ="N") And '+;
*!*				'(TOTSTK+Iif(lcRPOTSB="W",TOTWIP,totplan)-TOTORD < lnRPOTSMax)) Or '+;
*!*				' (lcRPOTSSig $ "LB" And (TOTSTK+Iif(lcRPOTSB="W",TOTWIP,totplan)-TOTORD < lnRPOTSMax And TOTSTK+Iif(lcRPOTSB="W",TOTWIP,totplan)-TOTORD >= lnRPOTSMin))))'
      lcRpExp = "("+lcRpExp + ") AND ("+ '(((lcRPOTSSig ="P") And ((STK1+Iif(lcRPOTSB="W",WIP1,plan1) -ORD1 >= lnRPOTSMin) or '+;
		'(STK2+Iif(lcRPOTSB="W",WIP2,plan2) -ORD2 >= lnRPOTSMin) OR '+;
		'(STK3+Iif(lcRPOTSB="W",WIP3,plan3) -ORD3 >= lnRPOTSMin) OR '+; 
		'(STK4+Iif(lcRPOTSB="W",WIP4,plan4) -ORD4 >= lnRPOTSMin) OR '+;
		'(STK5+Iif(lcRPOTSB="W",WIP5,plan5) -ORD5 >= lnRPOTSMin) OR '+;
		'(STK6+Iif(lcRPOTSB="W",WIP6,plan6) -ORD6 >= lnRPOTSMin) OR '+;
        '(STK7+Iif(lcRPOTSB="W",WIP7,plan7) -ORD7 >= lnRPOTSMin) OR '+;
		'(STK8+Iif(lcRPOTSB="W",WIP8,plan8) -ORD8 >= lnRPOTSMin))) Or ((lcRPOTSSig ="N") And '+;
			'((STK1+Iif(lcRPOTSB="W",WIP1,plan1)-ORD1 < lnRPOTSMax) OR '+;
			'(STK2+Iif(lcRPOTSB="W",WIP2,plan2)-ORD2 < lnRPOTSMax) OR '+;
			'(STK3+Iif(lcRPOTSB="W",WIP3,plan3)-ORD3 < lnRPOTSMax) OR '+;
			'(STK4+Iif(lcRPOTSB="W",WIP4,plan4)-ORD4 < lnRPOTSMax) OR '+;
			'(STK5+Iif(lcRPOTSB="W",WIP5,plan5)-ORD5 < lnRPOTSMax) OR '+;
			'(STK6+Iif(lcRPOTSB="W",WIP6,plan6)-ORD6 < lnRPOTSMax) OR '+;
			'(STK7+Iif(lcRPOTSB="W",WIP7,plan7)-ORD7 < lnRPOTSMax) OR '+;
			'(STK8+Iif(lcRPOTSB="W",WIP8,plan8)-ORD8 < lnRPOTSMax) )) Or '+;
			' (lcRPOTSSig $ "LB" And (TOTSTK+Iif(lcRPOTSB="W",TOTWIP,totplan)-TOTORD < lnRPOTSMax And TOTSTK+Iif(lcRPOTSB="W",TOTWIP,totplan)-TOTORD >= lnRPOTSMin))))'
 	  *B612540,1 MMT 03/30/2022 Style report deos not print styles that has some sizes with negative or positive OTS Qty if the Sum of the sizes OTS is not negative or positive[T20220323.0001][End] 			
	ENDIF
*! E612104,1 MMT 10/14/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
Endif




*-- Set Relation between Master and Child Files.
Select (lcStyle)

If Atc("INTO &lcscal",Set("Relation")) > 0
	Set Relation Off Into &lcscal
ENDIF

Set Relation To "S" + Scale Into &lcscal Additive   && To print Scales.
Select (lcMastFile)


If Atc("INTO &lcChldFile",Set("Relation")) > 0
	Set Relation Off Into &lcChldFile
ENDIF

Set Relation To Style Into (lcChldFile) Additive

lcNnMajTl = gfItemMask('HN')  && Non Major title.
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
lfClearMemory()
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]


If !Empty(lcRpExp)
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
		Do gfDispRe With Eval('lcRPFormNa') , 'FOR ' + lcRpExp
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ELSE

		Select (lcStyle)
		SET FILTER TO &lcRpExp
		LOCATE
		IF EOF()
			SET FILTER TO
			RETURN
		ENDIF
		loProgress.Percent = 0.9
		loProgress.DESCRIPTION = "Printing Report..."
*XX

*XX
		loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
		PRIVATE loProxy
*loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
		loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
		loogScroll   = oAriaEnvironment.REPORT
		oAriaEnvironment.REPORT.OGLastForm =  Eval('lcRPFormNa')

		oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)

		loProgress.Percent = 1.0
		loProgress.DESCRIPTION = "Printing Report..."
		loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

	ENDIF

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

ELSE
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start

	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
		Do gfDispRe With Eval('lcRPFormNa')
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ELSE
		loProgress.Percent = 0.9
		loProgress.DESCRIPTION = "Printing Report..."
		loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
		PRIVATE loProxy
*XX

*XX

*loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
		loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
		loogScroll   = oAriaEnvironment.REPORT
		oAriaEnvironment.REPORT.OGLastForm = Eval('lcRPFormNa')
		oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
		loProgress.Percent = 1.0
		loProgress.DESCRIPTION = "Printing Report..."
		loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]
Endif

If File(oAriaApplication.WorkDir +RevStyDy+".DBF")
	If Used(RevStyDy)
		Use In &RevStyDy
	Endif
	Erase oAriaApplication.WorkDir+RevStyDy+".DBF"
	Erase oAriaApplication.WorkDir+"RevStyDy.CDX"
	Erase oAriaApplication.WorkDir+"RevStyDyW.CDX"
Endif

If File(oAriaApplication.WorkDir +RevStyle+".DBF")
	If Used(RevStyle)
		Use In &RevStyle
	Endif
	Erase oAriaApplication.WorkDir+RevStyle+".DBF"
	Erase oAriaApplication.WorkDir+"RevStyle.CDX"
Endif

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

Function lfSlctFox

Lparameters lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
Local lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
Private laIndex
Dimension laIndex[1,2]

If Type("loRDA1") <> 'O'
	loRDA1 = Createobject("RemoteDataAccess")
Endif
lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM  " + lcTable + Iif(Type('lcWhereCond') = 'C' And !Empty(lcWhereCond)," WHERE " + lcWhereCond ,"")

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
* Important Note :loRDA1.sqlrun caused problem in the cusrsor it return there are wrong fields value
* so When All table will be converted to SQL when can use it with SQL connection string

*lnConnectionHandlar = loRDA1.sqlrun(lcSqlStatment,lcCursor,,oAriaApplication.cAriaNativeDataFilesConStr,3,;
'BROWSE',SET("DATASESSION"))
*IF lnConnectionHandlar = 1
lcSqlStatment   = lcSqlStatment   + " INTO CURSOR &lcCursor READWRITE"
&lcSqlStatment.
If Used(lcCursor)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	lnBuffering = CursorGetProp("Buffering",lcCursor)
	=CursorSetProp("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
	=lfCrtindex(lcCursor)
	Select (lcCursor)
	For lnI = 1 To Alen(laIndex,1)
		lcIndex = laIndex[lnI,1]
		lcTag   = laIndex[lnI,2]
		Index On &lcIndex. Tag (lcTag) &&OF (lcCursor)
	Endfor
	lcTag = laIndex[1,2]
	Set Order To Tag (lcTag)

Else
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
	lfClearMemory()
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*=loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	Return .F.
Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
lfClearMemory()
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Return
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
Function lfCrtindex
Lparameters lcTable

Do Case
Case Upper(lcTable) = lcStyle
	Dimension laIndex[1,2]
	laIndex[1,1] = 'STYLE'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*laIndex[1,2] = 'LCSTYLE'
	laIndex[1,2] = lcStyle
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Case Upper(lcTable) = lcStyDye
	Dimension laIndex[2,2]
	laIndex[1,1] = 'STYLE+CWARECODE+DYELOT'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*laIndex[1,2] = 'LCSTYDYE'
	laIndex[1,2] = lcStyDye
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laIndex[2,1] = 'CWARECODE+STYLE+DYELOT'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*laIndex[2,2] = 'LCSTYDYEW'
	laIndex[2,2] = lcStyDyeW
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
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
Case Upper(lcTable) = lcWhsLoc
	Dimension laIndex[2,2]
	laIndex[1,1] = 'STYLE+COLOR+CWARECODE+CLOCATION'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*laIndex[1,2] = 'LCWHSLOCST'
	laIndex[1,2] = LCWHSLOCST
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laIndex[2,1] = 'CWARECODE+CLOCATION+STYLE+COLOR'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*laIndex[2,2] = 'LCWHSLOC'
	laIndex[2,2] = lcWhsLoc
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Case Upper(lcTable) = lcscal
	Dimension laIndex[1,2]
	laIndex[1,1] = 'TYPE+SCALE+PREPAK'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*laIndex[1,2] = 'lcscal'
	laIndex[1,2] =lcscal
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Case Upper(lcTable) = lcWareHous
	Dimension laIndex[1,2]
	laIndex[1,1] = 'CWARECODE'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*laIndex[1,2] = 'LCWAREHOUS'
	laIndex[1,2] = lcWareHous
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Case Upper(lcTable) = lcSegVal
	Dimension laIndex[1,2]
	laIndex[1,1] = 'CISEGNO+CISEGVAL'
	laIndex[1,2] = 'LCSEGVAL'
Endcase


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
Function lfTranAray



Private lnFindTran
*xxxx
IF TYPE('lcXMLFileName') = 'C'
	Dimension laAllTrns[1,2]
	lfTransArr()
*!*		laAllTrns[01,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
*!*		laAllTrns[01,2] = "WIP"
ENDIF
*xxxx
lnFindTran = Ascan(laAllTrns,laRPRepTar[1])



If lnFindTran > 0
	lnFindTran = Asubscript(laAllTrns,lnFindTran,1)
	lcRepTarVl = Alltrim(laAllTrns[lnFindTran,2])
Else
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_ICSTYREP_Error NOWAIT

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

		Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Error,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Error",AHEADERFILE)) Nowait

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*N000682,1 11/20/2012 MMT Globlization changes[End]
	Return
Endif

lcPrnTitle = ''  && Print title for transaction, printed in report header.
*-- Fill Transaction Arrays [Begin]

Do Case
*------------------------------------- Direct Cases ----------------------
*-----------------------------------------------------------------------------
*-- Wip Case...
Case lcRepTarVl == "WIP"
	=lfDirctVal("WIP")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_WIP
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_WIP,oAriaApplication.GetHeaderText("LANG_ICSTYREP_WIP",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Stock on hand Case...
Case lcRepTarVl == "SOH"
	=lfDirctVal("STK")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Stock
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
*-- Plan Case...
Case lcRepTarVl == "PLA"
	=lfDirctVal("PLAN")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Plan
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Plan,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Plan",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*-- Order Case...
Case lcRepTarVl == "ORD"
	=lfDirctVal("ORD")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Ordered
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Ordered,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Ordered",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*-- Work order Case...
Case lcRepTarVl == "WORD"
	=lfDirctVal("NWO")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Work_Ordered
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Work_Ordered,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Work_Ordered",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*-- Intransit Case...
Case lcRepTarVl == "INT"
	=lfDirctVal("INTRANS")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Intransit
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Intransit,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Intransit",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Shipped Case...
Case lcRepTarVl == "SHP"
	=lfDirctVal("SHP")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Shipped
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Shipped,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Shipped",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Credit memo return Case...
Case lcRepTarVl == "RET"
	=lfDirctVal("RET")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Return
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Return Authorization Case...
Case lcRepTarVl == "RETA"
	=lfDirctVal("RA")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Return_Auth1
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Return_Auth1,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Return_Auth1",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Allocation Case...
Case lcRepTarVl == "ALO"
	=lfDirctVal("ALO")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Allocated
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Allocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Allocated",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*------------------------------------- Calculated Cases ----------------------
*-----------------------------------------------------------------------------

*-- UnAllocate Case...
Case lcRepTarVl == "UALO"
	=lfInDirect("STK","-ALO")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Unallocated
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unallocated,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unallocated",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Book Case...
Case lcRepTarVl == "BOK"
	=lfInDirect("SHP","+ORD")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Booked
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Booked,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Booked",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*------------------------------------- Special Cases ----------------------
*-----------------------------------------------------------------------------
*-- OTS Case...
Case lcRepTarVl == "OTS"
	=lfInDirect("STK","-ORD","+" + Iif(lcRPOTSB="W","WIP","PLAN"),.T.)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Open_to_sell
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Open_to_sell,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Open_to_sell",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- IOTS Case...
Case lcRepTarVl == "IOTS"
	=lfInDirect("STK","-ORD",'',.T.)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPrnTitle = LANG_ICSTYREP_Imm_Open_to_sell
	lcPrnTitle = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_Open_to_sell,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_Open_to_sell",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


Endcase
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
Function lfDirctVal
Parameter lcDirctVal

Private lnI
lnI = 0
Store '' To laTranAray , laTrnNonAr  && Initially null values.
For lnI = 1 To 8
	laTranAray[lnI] = lcDirctVal + Str(lnI,1)
	If lcDirctVal="STK"
		laTrnNonAr[lnI] = "&lcStyDye.." + lcDirctVal + Str(lnI,1)
	Else
		laTrnNonAr[lnI] = "&lcStyle.." + lcDirctVal + Str(lnI,1)
	Endif
Endfor

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
Function lfInDirect
Parameter lcValue1,lcValue2,lcValue3,llSpecCond
Private lcClrVal2,lcClrVal3,lnI

lcClrVal2 = Strtran(lcValue2,"-","-&lcStyle..")
lcClrVal2 = Strtran(lcClrVal2,"+","+&lcStyle..")

If Type('lcValue3') $ 'UL' Or Empty(lcValue3)
	lcValue3 = ''
Else
	lcClrVal3 = Strtran(lcValue3,"-","-&lcStyle..")
	lcClrVal3 = Strtran(lcClrVal3,"+","+&lcStyle..")
Endif

lnI = 0
Store '' To laTranAray , laTrnNonAr  && Intially null values.
For lnI = 1 To 8
	laTranAray[lnI] = lcValue1 + Str(lnI,1) + lcValue2 + Str(lnI,1) +;
		IIF(Empty(lcValue3) , "" , lcValue3 + Str(lnI,1))


	laTrnNonAr[lnI] = "&lcStyle.." + lcValue1 + Str(lnI,1) + lcClrVal2 + Str(lnI,1) +;
		IIF(Empty(lcValue3) , "" , lcClrVal3 + Str(lnI,1))
	If llSpecCond
		laTranAray[lnI] = [IIF(((lcRpOTSSig = 'P') AND ] + laTranAray[lnI] +;
			[ < 0) OR ((lcRpOTSSig = 'N') AND ] + laTranAray[lnI] +;
			[ > 0),0,] + laTranAray[lnI] + [)]

		laTrnNonAr[lnI] = [IIF(((lcRpOTSSig = 'P') AND ] + laTrnNonAr[lnI] +;
			[ < 0) OR ((lcRpOTSSig = 'N') AND ] + laTrnNonAr[lnI] +;
			[ > 0),0,] + laTrnNonAr[lnI] + [)]
	Endif

Endfor
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

Function lfCrTmp

*!*	B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][Start]

*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
*DIMENSION laFileStru[181,18]
*!* Dimension laFileStru[182,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*  Dimension laFileStru[183,18]
Dimension laFileStru[12,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]

*!*	B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][End]

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


*B612323,1 Es 03/04/2021 Style Summary Report does not export sizes description when exported to Excel [T20210122.0001][Start]
lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+8,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
laFileStru[lnArrayLen+1,1] = 'SZ1'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 5
laFileStru[lnArrayLen+1,4] = 0


laFileStru[lnArrayLen+2,1] = 'SZ2'
laFileStru[lnArrayLen+2,2] = 'C'
laFileStru[lnArrayLen+2,3] = 5
laFileStru[lnArrayLen+2,4] = 0

laFileStru[lnArrayLen+3,1] = 'SZ3'
laFileStru[lnArrayLen+3,2] = 'C'
laFileStru[lnArrayLen+3,3] = 5
laFileStru[lnArrayLen+3,4] = 0

laFileStru[lnArrayLen+4,1] = 'SZ4'
laFileStru[lnArrayLen+4,2] = 'C'
laFileStru[lnArrayLen+4,3] = 5
laFileStru[lnArrayLen+4,4] = 0

laFileStru[lnArrayLen+5,1] = 'SZ5'
laFileStru[lnArrayLen+5,2] = 'C'
laFileStru[lnArrayLen+5,3] = 5
laFileStru[lnArrayLen+5,4] = 0

laFileStru[lnArrayLen+6,1] = 'SZ6'
laFileStru[lnArrayLen+6,2] = 'C'
laFileStru[lnArrayLen+6,3] = 5
laFileStru[lnArrayLen+6,4] = 0

laFileStru[lnArrayLen+7,1] = 'SZ7'
laFileStru[lnArrayLen+7,2] = 'C'
laFileStru[lnArrayLen+7,3] = 5
laFileStru[lnArrayLen+7,4] = 0

laFileStru[lnArrayLen+8,1] = 'SZ8'
laFileStru[lnArrayLen+8,2] = 'C'
laFileStru[lnArrayLen+8,3] = 5
laFileStru[lnArrayLen+8,4] = 0
*B612323,1 Es 03/04/2021 Style Summary Report does not export sizes description when exported to Excel [T20210122.0001][End]

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
If llWipSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'WIP1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'WIP2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'WIP3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'WIP4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'WIP5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'WIP6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'WIP7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'WIP8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotWIP'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llStockSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'SOH1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'SOH2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'SOH3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'SOH4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'SOH5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'SOH6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'SOH7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'SOH8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotSOH'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llPlanSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'Pla1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 10
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'Pla2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 10
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'Pla3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 10
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'Pla4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 10
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'Pla5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 10
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'Pla6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 10
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'Pla7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 10
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'Pla8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 10
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotPla'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 10
	laFileStru[lnArrayLen+9,4] = 0

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llUnAllocSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	laFileStru[lnArrayLen+1,1] = 'UAlo1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 10
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'UAlo2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 10
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'UAlo3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 10
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'UAlo4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 10
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'UAlo5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 10
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'UAlo6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 10
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'UAlo7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 10
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'UAlo8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 10
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotUAlo'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 10
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOTSSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	laFileStru[lnArrayLen+1,1] = 'OTS1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'OTS2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'OTS3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'OTS4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'OTS5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'OTS6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'OTS7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'OTS8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotOTS'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llImmOTSSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	laFileStru[lnArrayLen+1,1] = 'IOTS1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'IOTS2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'IOTS3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'IOTS4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'IOTS5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'IOTS6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'IOTS7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'IOTS8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotIOTS'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOrderSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	laFileStru[lnArrayLen+1,1] = 'Ord1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'Ord2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'Ord3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'Ord4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'Ord5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'Ord6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'Ord7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'Ord8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotOrd'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llWorkOrderSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'WOrd1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'WOrd2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'WOrd3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'WOrd4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'WOrd5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'WOrd6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'WOrd7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'WOrd8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotWOrd'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llInTransSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'Int1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'Int2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'Int3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'Int4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'Int5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'Int6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'Int7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'Int8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotInt'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llBookSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'Bok1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'Bok2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'Bok3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'Bok4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'Bok5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'Bok6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'Bok7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'Bok8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotBok'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llShipSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'Shp1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'Shp2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'Shp3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'Shp4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'Shp5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'Shp6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'Shp7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'Shp8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotShp'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llAllocSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'Alo1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'Alo2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'Alo3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'Alo4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'Alo5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'Alo6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'Alo7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'Alo8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotAlo'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llReturnSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	laFileStru[lnArrayLen+1,1] = 'Ret1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'Ret2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'Ret3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'Ret4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'Ret5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'Ret6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'Ret7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'Ret8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotRet'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llRetAuthSel
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+9,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	laFileStru[lnArrayLen+1,1] = 'RetA1'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 8
	laFileStru[lnArrayLen+1,4] = 0

	laFileStru[lnArrayLen+2,1] = 'RetA2'
	laFileStru[lnArrayLen+2,2] = 'N'
	laFileStru[lnArrayLen+2,3] = 8
	laFileStru[lnArrayLen+2,4] = 0

	laFileStru[lnArrayLen+3,1] = 'RetA3'
	laFileStru[lnArrayLen+3,2] = 'N'
	laFileStru[lnArrayLen+3,3] = 8
	laFileStru[lnArrayLen+3,4] = 0

	laFileStru[lnArrayLen+4,1] = 'RetA4'
	laFileStru[lnArrayLen+4,2] = 'N'
	laFileStru[lnArrayLen+4,3] = 8
	laFileStru[lnArrayLen+4,4] = 0

	laFileStru[lnArrayLen+5,1] = 'RetA5'
	laFileStru[lnArrayLen+5,2] = 'N'
	laFileStru[lnArrayLen+5,3] = 8
	laFileStru[lnArrayLen+5,4] = 0

	laFileStru[lnArrayLen+6,1] = 'RetA6'
	laFileStru[lnArrayLen+6,2] = 'N'
	laFileStru[lnArrayLen+6,3] = 8
	laFileStru[lnArrayLen+6,4] = 0

	laFileStru[lnArrayLen+7,1] = 'RetA7'
	laFileStru[lnArrayLen+7,2] = 'N'
	laFileStru[lnArrayLen+7,3] = 8
	laFileStru[lnArrayLen+7,4] = 0

	laFileStru[lnArrayLen+8,1] = 'RetA8'
	laFileStru[lnArrayLen+8,2] = 'N'
	laFileStru[lnArrayLen+8,3] = 8
	laFileStru[lnArrayLen+8,4] = 0

	laFileStru[lnArrayLen+9,1] = 'TotRetA'
	laFileStru[lnArrayLen+9,2] = 'N'
	laFileStru[lnArrayLen+9,3] = 8
	laFileStru[lnArrayLen+9,4] = 0

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

laFileStru[lnArrayLen+1,1] = 'HasDye'
laFileStru[lnArrayLen+1,2] = 'L'
laFileStru[lnArrayLen+1,3] = 0
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

laFileStru[lnArrayLen+1,1] = 'lPrnOTS'
laFileStru[lnArrayLen+1,2] = 'L'
laFileStru[lnArrayLen+1,3] = 0
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

laFileStru[lnArrayLen+1,1] = 'lPrnIOTS'
laFileStru[lnArrayLen+1,2] = 'L'
laFileStru[lnArrayLen+1,3] = 0
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

laFileStru[lnArrayLen+1,1] = 'Season'
laFileStru[lnArrayLen+1,2] = 'C'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*  laFileStru[lnArrayLen+1,3] = 6
laFileStru[lnArrayLen+1,3] = 30
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
laFileStru[lnArrayLen+1,4] = 0


lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* laFileStru[143,1] = 'Division'
laFileStru[lnArrayLen+1,1] = 'CDIVISION'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]
laFileStru[lnArrayLen+1,2] = 'C'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*     laFileStru[lnArrayLen+1,3] = 6
laFileStru[lnArrayLen+1,3] = 30
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]


laFileStru[lnArrayLen+1,1] = 'FGroup'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 7
laFileStru[lnArrayLen+1,4] = 0


lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
*  laFileStru[145,1] = 'SGroup'
laFileStru[lnArrayLen+1,1] = 'CSTYGROUP'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]


laFileStru[lnArrayLen+1,2] = 'C'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*    laFileStru[lnArrayLen+1,3] = 6
laFileStru[lnArrayLen+1,3] = 30
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]


laFileStru[lnArrayLen+1,1] = 'Loc'
laFileStru[lnArrayLen+1,2] = 'M'
laFileStru[lnArrayLen+1,3] = 0
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]


laFileStru[lnArrayLen+1,1] = 'HasLoc'
laFileStru[lnArrayLen+1,2] = 'L'
laFileStru[lnArrayLen+1,3] = 1
laFileStru[lnArrayLen+1,4] = 0

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
If llWipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVWIP'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llStockSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVSOH'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llPlanSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVPLA'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVOTS'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llImmOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVIOTS'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llBookSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVBOK'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llShipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVSHP'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llReturnSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVRet'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llRetAuthSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVRetA'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]

	laFileStru[lnArrayLen+1,1] = 'nStkVAlo'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llUnAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVUAlo'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llInTransSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVInt'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llWorkOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVWOrd'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nStkVOrd'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llWipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVWIP'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llStockSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVSOH'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llPlanSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVPLA'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVOTS'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llImmOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]

	laFileStru[lnArrayLen+1,1] = 'nSalVIOTS'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llBookSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVBOK'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llShipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]

	laFileStru[lnArrayLen+1,1] = 'nSalVSHP'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llReturnSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVRet'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llRetAuthSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVRetA'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVAlo'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llUnAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVUAlo'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llInTransSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVInt'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llWorkOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]


	laFileStru[lnArrayLen+1,1] = 'nSalVWOrd'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	lnArrayLen = Alen(laFileStru,1)
	Dimension laFileStru[lnArrayLen+1,18]

	laFileStru[lnArrayLen+1,1] = 'nSalVOrd'
	laFileStru[lnArrayLen+1,2] = 'N'
	laFileStru[lnArrayLen+1,3] = 18
	laFileStru[lnArrayLen+1,4] = 2
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]


laFileStru[lnArrayLen+1,1] = 'cEndMaj'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 1
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

laFileStru[lnArrayLen+1,1] = 'cEndSort'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 1
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]


laFileStru[lnArrayLen+1,1] = 'cEndRep'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 1
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]


laFileStru[lnArrayLen+1,1] = 'NotScale'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 1
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]



laFileStru[lnArrayLen+1,1] = 'MatType'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 20
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

laFileStru[lnArrayLen+1,1] = 'Dept'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 5
laFileStru[lnArrayLen+1,4] = 0

lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
laFileStru[lnArrayLen+1,1] = 'Color'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 30
laFileStru[lnArrayLen+1,4] = 0
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
*!*B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][Start]
lnArrayLen = Alen(laFileStru,1)
Dimension laFileStru[lnArrayLen+1,18]

laFileStru[lnArrayLen+1,1] = 'Desc'
laFileStru[lnArrayLen+1,2] = 'C'
laFileStru[lnArrayLen+1,3] = 30
laFileStru[lnArrayLen+1,4] = 0
*!*B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][END]

Dimension laIndx[2,2]
laIndx[1,1] = "WareCode+StyCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
laIndx[1,2] = 'WareSort'
laIndx[2,1] = "StyCode+WareCode+Dyelot+NotScale"
laIndx[2,2] = 'StySort'

For lnCnt=7 To 16
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
*FOR lnInc=1 TO 181
*!*	B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][start]
*!*	    For lnInc=1 To 182
	For lnInc=1 To  Alen(laFileStru,1)&&183
*!*	B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][end]
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
		Store Space(0) To laFileStru[lnInc,lnCnt]
	Endfor
Endfor
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
*FOR lnInc=1 TO 181
*!*	B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][start]
*!*	   For lnInc=1 To 182
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
For lnInc=1 To  Alen(laFileStru,1)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*!*	B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][end]

*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
	Store 0 To laFileStru[lnInc,17] , laFileStru[lnInc,18]
Endfor

=gfCrtTmp(lcTrns,@laFileStru,@laIndx,lcTrns,.F.)


Select(lcTrns)
=Afields(laFileStru)
Dimension laIndx[1,2]
Do Case
Case lcRpSortBy == 'S'   && Sort by Style
	laIndx[1,1] = "StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
	laIndx[1,2] = 'StySort'
Case lcRpSortBy == 'SE'  && Sort by season
	laIndx[1,1] = "Season+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
	laIndx[1,2] = 'SeaSort'
Case lcRpSortBy == 'D'   && Sort by division

*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
*  laIndx[1,1] = "Division+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
	laIndx[1,1] = "CDIVISION+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]



	laIndx[1,2] = 'DivSort'
Case lcRpSortBy == 'FG'  && Sort by Primary Fabric
	laIndx[1,1] = "FGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
	laIndx[1,2] = 'FGrpSort'
Case lcRpSortBy == 'SG'  && Sort by Style Group
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
*   laIndx[1,1] = "SGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
	laIndx[1,1] = "CSTYGROUP+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

	laIndx[1,2] = 'SGrpSort'
Case lcRpSortBy == 'MT'  && Sort by Material Type
	laIndx[1,1] = "MatType+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
	laIndx[1,2] = 'MatTypSort'
Case lcRpSortBy == 'DE'  && Sort by Department
	laIndx[1,1] = "DEPT+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
	laIndx[1,2] = 'DeptSort'
*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [Start]
Case lcRpSortBy == 'W'  && Sort by Department
	laIndx[1,1] = "CEndRep+WareCode+CendMaj+CEndSort+StyCode+Dyelot+NotScale"
	laIndx[1,2] = 'WareSort2'
*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [End]
Endcase

=gfCrtTmp(lcStyTmp,@laFileStru,@laIndx,lcStyTmp,.F.)


Do Case
Case lcRpSortBy == 'W'   && Sort by Location
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+WareCode+cStyMajor+NotScale"
	laIndx[1,2] = 'WareSort'
Case lcRpSortBy == 'S'   && Sort by Style
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+cStyMajor+NotScale"
	laIndx[1,2] = 'StySort'
Case lcRpSortBy == 'SE'  && Sort by season
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Season+cStyMajor+NotScale"
	laIndx[1,2] = 'SeaSort'
Case lcRpSortBy == 'D'   && Sort by division
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Division+cStyMajor+NotScale"
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+CDIVISION+cStyMajor+NotScale"
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

	laIndx[1,2] = 'DivSort'
Case lcRpSortBy == 'FG'  && Sort by Primary Fabric
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+FGroup+cStyMajor+NotScale"
	laIndx[1,2] = 'FGrpSort'
Case lcRpSortBy == 'SG'  && Sort by Style Group
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+SGroup+cStyMajor+NotScale"
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+CSTYGROUP+cStyMajor+NotScale"
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

	laIndx[1,2] = 'SGrpSort'
Case lcRpSortBy == 'MT'   && Sort by Material Type
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+MatType+cStyMajor+NotScale"
	laIndx[1,2] = 'MatTypSort'
Case lcRpSortBy == 'DE'   && Sort by Department
	laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Dept+cStyMajor+NotScale"
	laIndx[1,2] = 'DeptSort'
Endcase

=gfCrtTmp(lcTotTmp,@laFileStru,@laIndx,lcTotTmp,.F.)

lnI=Alen(laFileStru,1)
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*DIMENSION laFileStru[lnI+3,18]
Dimension laFileStru[lnI+4,18]
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]
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
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
laFileStru[lnI+4,1] = 'cDUMM'
laFileStru[lnI+4,2] = 'C'
laFileStru[lnI+4,3] = 1
laFileStru[lnI+4,4] = 0
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]

For lnCnt=7 To 16
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*FOR lnInc=1 TO 3
	For lnInc=1 To 4
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]
		Store Space(0) To laFileStru[lnI+lnInc,lnCnt]
	Endfor
Endfor
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*FOR lnInc=1 TO 3
For lnInc=1 To 4
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]
	Store 0 To laFileStru[lnI+lnInc,17] , laFileStru[lnI+lnInc,18]
Endfor
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*=gfCrtTmp(lcDummy,@laFileStru,"cRecNo",lcDummy,.F.)
=gfCrtTmp(lcDummy,@laFileStru,"cDUMM+cRecNo",lcDummy,.F.)
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]

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

Function lfDummFill

*-- The need of "laTarArray" array is to hold the target transactions beside
*-- adding element for dyelot if "Print dyelot detail" is selected, and that
*-- is instead of adding this element to laRPRepTar array inorder to not change
*-- it because "Dyelot" should not appear in laRPRepTar array(in the mover).

= Acopy(laRPRepTar,laTarArray)

If lcRpSortBy = 'W'
	If llRPPrnDye
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnStkPo = ASCAN(laTarArray,LANG_ICSTYREP_Stock)
		lnStkPo = Ascan(laTarArray,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Stock,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Stock",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		If lnStkPo > 0
			Dimension laTarArray[ALEN(laTarArray)+1]
			=Ains(laTarArray,lnStkPo+1)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laTarArray[lnStkPo+1] = LANG_ICSTYREP_Dyelot
			laTarArray[lnStkPo+1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		Endif
	Else
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnDyePo = ASCAN(laTarArray,LANG_ICSTYREP_Dyelot)
		lnDyePo = Ascan(laTarArray,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		If lnDyePo > 0
			= Adel(laTarArray,lnDyePo)
			Dimension laTarArray[ALEN(laTarArray,1)-1]
		Endif
	Endif
Endif

For lnI = 1 To Alen(laTarArray)
*-- lnElemNo  = ASUBSCRIPT(laAllTrns,ASCAN(laAllTrns,laTarArray[lnI]),1)
	lnElemNo = Ascan(laAllTrns,laTarArray[lnI],1,Alen(laAllTrns,1),1,9)

	If !(Upper(laAllTrns[lnElemNo,2]) $ "IOTS")
		llOnlyOTS = .F.
	Endif

	lcAllTrns = lcAllTrns + "," +laAllTrns[lnElemNo,2]+ ","
	If !(Upper(laAllTrns[lnElemNo,2]) $ "OTS,IOTS,BOK") And Upper(laAllTrns[lnElemNo,2]) <> "UALO"
		lcTargFlds = Iif(Empty(lcTargFlds),lcTargFlds,lcTargFlds+",") + ;
			laAllTrns[lnElemNo,2]+'1,'+laAllTrns[lnElemNo,2]+'2,'+;
			laAllTrns[lnElemNo,2]+'3,'+laAllTrns[lnElemNo,2]+'4,'+;
			laAllTrns[lnElemNo,2]+'5,'+laAllTrns[lnElemNo,2]+'6,'+;
			laAllTrns[lnElemNo,2]+'7,'+laAllTrns[lnElemNo,2]+'8,'+;
			'Tot'+laAllTrns[lnElemNo,2]  &&Example  OTS1,OTS2,.....,TotOTS

		= lfSourFlds(Upper(laAllTrns[lnElemNo,2]))
	Endif
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*INSERT INTO (lcDummy) (Item                 ,Label          ,cRecNo        ) ;
VALUES (laAllTrns[lnElemNo,2],laTarArray[lnI],PADL(lnI,2,'0'))
	Insert Into (lcDummy) (Item                 ,Label          ,cRecNo        ,cDUMM) ;
		VALUES (laAllTrns[lnElemNo,2],laTarArray[lnI],Padl(lnI,2,'0'),Space(1))
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]
Endfor
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*INSERT INTO (lcDummy) (Item,Label,cRecNo             ) ;
VALUES (''  ,''   ,PADL(RECNO(),2,'0'))
Insert Into (lcDummy) (Item,Label,cRecNo             ,cDUMM) ;
	VALUES (''  ,''   ,Padl(Recno(),2,'0'),Space(1))
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]
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

Function lfSourFlds
Parameters lcTrnFld

Private lcTrnFld
Do Case
Case lcTrnFld = "WIP"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP"
Case lcTrnFld = "SOH"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TotSTK"
Case lcTrnFld = "PLA"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"PLAN1,PLAN2,PLAN3,PLAN4,PLAN5,PLAN6,PLAN7,PLAN8,TotPLAN"
Case lcTrnFld = "ORD"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TotORD"
Case lcTrnFld = "WORD"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"NWO1,NWO2,NWO3,NWO4,NWO5,NWO6,NWO7,NWO8,NTotWO"
Case lcTrnFld = "INT"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"INTRANS1,INTRANS2,INTRANS3,INTRANS4,INTRANS5,INTRANS6,INTRANS7,INTRANS8,TotINTRN"
Case lcTrnFld = "SHP"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"SHP1,SHP2,SHP3,SHP4,SHP5,SHP6,SHP7,SHP8,TotSHP"
Case lcTrnFld = "ALO"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"ALO1,ALO2,ALO3,ALO4,ALO5,ALO6,ALO7,ALO8,TotALO"
Case lcTrnFld == "RET"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"RET1,RET2,RET3,RET4,RET5,RET6,RET7,RET8,TotRET"
Case lcTrnFld = "RETA"
	lcSourFlds = lcSourFlds + Iif(Empty(lcSourFlds),"",",") + ;
		"RA1,RA2,RA3,RA4,RA5,RA6,RA7,RA8,TotRA"
Endcase





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

Function lfDatCollW

Private lcStyl1,lcDyelot,llHasDye,lcScale,lcWareCode

llHasDye = .F.
llAdded  = .F.    && 4 ONLY.

Select (lcStyle)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*Set Order To Tag lcStyle
Set Order To Tag (lcStyle)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
If Atc("INTO &lcscal",Set("Relation")) > 0
	Set Relation Off Into &lcscal
Endif
Set Relation To 'S' + Scale Into &lcscal Additive





Select (lcStyDye)
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*B608163,1 TMI [Start] do not use the asscending order
*SET ORDER TO TAG lcStyDyeW DESCENDING
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*Set Order To Tag lcStyDyeW
Set Order To Tag (lcStyDyeW)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*B608163,1 TMI [End  ]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]

If Atc("INTO &lcStyle",Set("Relation")) > 0
	Set Relation Off Into &lcStyle
Endif
Set Relation To Style Into &lcStyle Additive


*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
	oProgress = Newobject('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
	oProgress.TotalProgress = Reccount()
	lcCaptionVal = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_CollData,oAriaApplication.GetHeaderText("LANG_ICSTYREP_CollData",AHEADERFILE))
	oProgress.lblFirstLabel.Caption = lcCaptionVal
	oProgress.Show()
	lcCaptionVal = Alltrim(lcCaptionVal)+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Style,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Style",AHEADERFILE))+" : "

	lnCntRecProg  = 1

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
lcStyl1  = Space(19)
lcScale = Space(3)
lcWareCode= Space(6)

Scan

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		oProgress.lblFirstLabel.Caption = lcCaptionVal + Style
		oProgress.CurrentProgress(lnCntRecProg)
		lnCntRecProg  = lnCntRecProg + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

	llAdded = .T.
*--Collect data From StyDye in case of (dyelots for Stock)=Yes ,or (print configuration)=Yes
	If llConfig
		llSetup= llRpConfig
	Else
		llSetup= llRPPrnDye
	Endif
	If llSetup
		If lcStyl1 <> &lcStyDye..Style
			llHasDye = !Empty(DYELOT)
		Endif
		lcStyl1  = &lcStyDye..Style
	Endif
	If !Empty(DYELOT)
		Loop
	Endif

	Do lpInsInTmp With '&lcStyDye'

*: B608166,1 SSH 07/15/2007 Commented out to check for each size not the sum of all sizes.
*!*	  IF !llRPShwZer AND ;
*!*	     (&lcTrns..TotWip  + &lcTrns..TotSOH + &lcTrns..TotPLA  + &lcTrns..TotOTS + ;
*!*	      &lcTrns..TotIOTS + &lcTrns..TotBOK + &lcTrns..TotSHP  + &lcTrns..TotRet + ;
*!*	      &lcTrns..TotRetA + &lcTrns..TotAlo + &lcTrns..TotUAlo + &lcTrns..TotInt + ;
*!*	      &lcTrns..TotWOrd + &lcTrns..TotOrd) = 0

	If !llRPShwZer And lfZeroStok()
*: B608166,1 SSH 07/15/2007 end
		Select (lcTrns)
		Delete
		llAdded = .F.
	Else
		If llOnlyOTS And !&lcTrns..lPrnOTS And !&lcTrns..lPrnIOTS
			Select (lcTrns)
			Delete
			llAdded = .F.
		Else
			Select (lcTrns)
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
*SCATTER FIELDS cStyMajor,StyCode,WareCode,Dyelot,Scale,Price,TotCost,Ave_Cost MEMVAR
			Scatter Fields CSTYMAJOR,StyCode,WareCode,DYELOT,Scale,Price,TotCost,Ave_Cost,Color Memvar
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]
			Select (lcTotTmp)
			If !Seek('A  '+&lcTrns..WareCode+&lcTrns..CSTYMAJOR)
				Append Blank
				Gather Memvar
				Replace cEndMaj With "A",NotScale With 'Y'
			Endif
			= lfUpdTotals("Maj",lcTrns)

			If !Seek('AA '+&lcTrns..WareCode)
				Append Blank
				Gather Memvar
				Replace cEndMaj With "A",cEndSort With "A",NotScale With "Y"
			Endif
			= lfUpdTotals("Sor",lcTrns)

			If !Seek('AAA')
				Append Blank
				Gather Memvar
				Replace cEndMaj With "A",cEndSort With "A",CENDREP With "A",NotScale With "Y"
*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [Start]
			Else
				Replace  WareCode  With  m.WareCode
				Replace  CSTYMAJOR With m.CSTYMAJOR

*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [End]
			Endif
			= lfUpdTotals("Rep",lcTrns)
*------------------------------------------------------*
		Endif
		If llRPPrnLoc
			If Seek(&lcStyDye..Style+Space(6)+&lcStyDye..CWARECODE,lcWhsLoc)
				Select (lcWhsLoc)
				lcLoc = ''
				Scan Rest While Style+Color+CWARECODE = ;
						&lcStyDye..Style+Space(6)+&lcStyDye..CWARECODE ;
						FOR !Empty(cLocation)
					llHasLoc = .T.

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcLoc =  lcLoc + IIF(EMPTY(lcLoc),LANG_ICSTYREP_Bins+'                 ',SPACE(2)) + &lcWhsLoc..cLocation
					lcLoc =  lcLoc + Iif(Empty(lcLoc),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Bins,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Bins",AHEADERFILE))+'                 ',Space(2)) + &lcWhsLoc..cLocation
*N000682,1 11/20/2012 MMT Globlization changes[End]

				Endscan
				If llHasLoc
					Select (lcTrns)
					Replace HasLoc With llHasLoc,;
						Loc    With lcLoc
				Endif
			Endif
		Endif
	Endif

	If llAdded
		If lcScale <> &lcscal..Scale Or lcWareCode <> &lcTrns..WareCode
			Do lpInsInTmp With '&lcStyDye',.T.
			lcScale = &lcscal..Scale
			lcWareCode = &lcTrns..WareCode
		Endif
	Endif

Endscan
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
lfClearMemory()

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

	oProgress = Null

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Select (lcTrns)
If !Empty(oAriaApplication.WorkDir+lcTotTmp)
	Append From (oAriaApplication.WorkDir+lcTotTmp)
Endif

Select (lcStyle)
Set Order To
Set Relation To

Select (lcStyDye)
Set Order To
Set Relation To

Do lpWareRela
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

Procedure lpInsInTmp
Parameters lcFromFile,llScale


Private lcFileToUse,lcCostFld,lcFromFile

Private lnCurAlias


lnCurAlias = Select(0)

lcFileToUse = Iif(lcFromFile='&lcStyDye',(lcTrns),(lcStyTmp))

Select(lcFileToUse)
Append Blank
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]

* Replace StyCode   With &lcStyle..Style,;
StyDesc   With &lcStyle..Desc1,;
CSTYMAJOR With &lcStyle..CSTYMAJOR,;
STATUS    With &lcStyle..Status,;
SCALE     With &lcStyle..Scale,;
Price     With &lcStyle..PriceA,;
TotCost   With &lcStyle..TotCost,;
SEASON    With &lcStyle..SEASON,;
Division  With &lcStyle..CDIVISION,;
FGroup    With &lcStyle..FABRIC,;
SGroup    With &lcStyle..CSTYGROUP,;
Dept      With &lcStyle..Dept,;
Desc      With &lcStyle..Desc

Replace StyCode   With &lcStyle..Style,;
	StyDesc   With &lcStyle..Desc1,;
	CSTYMAJOR With &lcStyle..CSTYMAJOR,;
	STATUS    With &lcStyle..Status,;
	SCALE     With &lcStyle..Scale,;
	Price     With &lcStyle..PriceA,;
	TotCost   With &lcStyle..TotCost,;
	SEASON    With &lcStyle..SEASON,;
	CDIVISION With &lcStyle..CDIVISION,;
	FGroup    With &lcStyle..FABRIC,;
	CSTYGROUP With &lcStyle..CSTYGROUP,;
	Dept      With &lcStyle..Dept,;
	Desc      With &lcStyle..Desc

*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

*B612323,1 Es 03/04/2021 Style Summary Report does not export sizes description when exported to Excel [T20210122.0001][Start]
Replace SZ1   WITH &lcscal..Sz1,;
	SZ2    With &lcscal..Sz2,;
	SZ3 With &lcscal..Sz3,;
	SZ4    With &lcscal..Sz4,;
	SZ5     With &lcscal..Sz5,;
	SZ6     With &lcscal..Sz6,;
	SZ7     With &lcscal..Sz7,;
	SZ8     With &lcscal..Sz8
*B612323,1 Es 03/04/2021 Style Summary Report does not export sizes description when exported to Excel [T20210122.0001][End]



*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Replace SEASON    With gfCodDes(&lcStyle..SEASON,Padr('SEASON',10)),;
	CDIVISION With gfCodDes(&lcStyle..CDIVISION,Padr('CDIVISION',10)),;
	CSTYGROUP With gfCodDes(&lcStyle..CSTYGROUP,Padr('CSTYGROUP',10))

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]



*!*	B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][END]
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[Start]
Replace Color With gfCodDes(Substr(&lcStyle..Style,lnClrPo,lnColorLen),'COLOR     ')
*B610131,1 MMT 10/24/2012 Style summary report does not export color desc. to excel[End]

If Used(lcFabric) And Reccount(lcFabric) > 0
	If Seek(&lcStyle..FABRIC,lcFabric)
		Replace  MatType   With  &lcFabric..ITEM_TYPE
	Endif
Endif
If lcFromFile = '&lcStyDye'
	Replace WareCode  With &lcStyDye..CWARECODE,;
		DYELOT    With &lcStyDye..DYELOT,;
		WareDesc  With &lcStyDye..Desc,;
		HasDye    With llHasDye
Endif
*!*B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][Start]
If lcFromFile='&lcStyle'
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*!*	    Replace WareCode With &lcStyle..cDefWare,;
*!*	      WareDesc With Iif(Seek(&lcStyle..cDefWare,'&lcWareHous','LCWAREHOUS'),&lcWareHous..cDesc,'')
	Replace WareCode With &lcStyle..cDefWare,;
		WareDesc With Iif(Seek(&lcStyle..cDefWare,'&lcWareHous',lcWareHous),&lcWareHous..cDesc,'')
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Endif
*!*B611394,1 AHH 22/08/2017 Unable to run style summary report[T20170810.0024][END]
Replace Ave_Cost  With &lcFromFile..Ave_Cost,;
	GL_LINK   With Iif(lcFromFile='&lcStyle',&lcStyle..Link_Code,&lcStyDye..GL_LINK)

If llScale
	Replace NotScale With 'N'
Else
	Replace NotScale With 'Y'



	If !Empty(lcSourFlds)
*--in case of print dyelots for stock and no details for another transactions
		If llRPPrnDye And !llRPWhDeta And lcRpSortBy <> 'W'&& Retrieve all quantities from style file
			If lcFromFile='&lcStyle'
				Select (lcFromFile)
				Scatter Fields &lcSourFlds. To laAllVal
				Select (lcFileToUse)
				Gather Fields &lcTargFlds. From laAllVal
			Else && Scan in StyDye File
				Select (lcFromFile)  &&Retrieve only stock values for dyelots from StyDye file
				lcStkMastFlds='STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK'
				Scatter Fields &lcStkMastFlds. To laAllVal
				Select (lcFileToUse)
				lcStkTempFlds='SOH1,SOH2,SOH3,SOH4,SOH5,SOH6,SOH7,SOH8,TOTSOH'
				Gather Fields &lcStkTempFlds. From laAllVal
			Endif
		Else
			If !llConfig &&the rest of dyelot cases
*--if !llRpPrnDye AND  llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
*--if  llRpPrnDye AND  llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
*--if !llRpPrnDye AND !llRpWhsDeta,lcFromFile='&lcStyle'

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
				If !(lcFromFile = '&lcStyle' And llRPWhDeta)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

					Select (lcFromFile)
					Scatter Fields &lcSourFlds. To laAllVal
					Select (lcFileToUse)
					Gather Fields &lcTargFlds. From laAllVal

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
				Endif
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


			Else
*--if !llRpConfig AND !llRPWhDeta,lcFromFile='&lcStyle'
*--if !llRpConfig AND  llRpWhsDeta,lcFromFile='&lcStyDye' ,plan doesn't exist(NO Problem)
*--if llRpConfig AND  llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
*--if llRpConfig AND !llRpWhsDeta,lcFromFile='&lcStyDye'  ,plan doesn't exist(NO Problem)
*--Sorting by location also use this part of code.

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
				If !(lcFromFile = '&lcStyle' And llRPWhDeta)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

					Select (lcFromFile)
					Scatter Fields &lcSourFlds. To laAllVal
					Select (lcFileToUse)
					Gather Fields &lcTargFlds. From laAllVal

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
				Endif
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

			Endif
		Endif
	Endif


*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
	If !(lcFromFile = '&lcStyle' And llRPWhDeta)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

		If ",UALO," $ Upper(lcAllTrns)
			= lfUAloCalc()
			Select (lcFileToUse)
			Gather Fields &lcUALOFlds. From laUAloVal
		Endif
		If ",IOTS," $ Upper(lcAllTrns)
			Store 0 To laIOTSVal
			=lfOTSCalc('IOTS')
			Select (lcFileToUse)
			Gather Fields &lcIOTSFlds. From laIOTSVal
		Endif
		If ",OTS," $ Upper(lcAllTrns)
			Store 0 To laOTSVal
			=lfOTSCalc('OTS')
			Select (lcFileToUse)
			Gather Fields &lcOTSFlds. From laOTSVal
		Endif
		If ",BOK," $ Upper(lcAllTrns)
			=lfBokCalc()
			Select (lcFileToUse)
			Gather Fields &lcBokFlds. From laBokVal
		Endif

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
	Endif
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

	Select (lcFileToUse)

	If llShowCost
		If lcRpSortBy = 'W'
			lcCostFld = 'Ave_Cost'
		Else
			If lcFromFile = '&lcStyDye'
				lcCostFld = 'Ave_Cost'
			Else
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lcCostFld = 'Ave_Cost'
*ELSE
*B608407,1 WAM 01/14/2008 (End)

				If lcCstMeth = "S"
					lcCostFld = 'TotCost'
				Else
					lcCostFld = 'Ave_Cost'
				Endif
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)
			Endif
		Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		lnUsedCnt = 0
		If llWipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
			laStkVal[lnUsedCnt +1] = TotWip * &lcCostFld.
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
			lnUsedCnt = lnUsedCnt +1
		Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  laStkVal[02] = &lcFromFile..nStkVal
*ELSE
*B608407,1 WAM 01/14/2008 (End)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		If llStockSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
			laStkVal[lnUsedCnt +1] = Iif(lcCstMeth = "S" , &lcStyle..TotCost * &lcStyle..ToTSTK , &lcFromFile..nStkVal)
			lnUsedCnt = lnUsedCnt +1
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llPlanSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
			laStkVal[lnUsedCnt +1] = TotPLA  * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotOTS  * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llImmOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotIOTS * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llBookSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotBOK  * &lcCostFld.
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llShipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotSHP  * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llReturnSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotRet  * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llRetAuthSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotRetA * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotAlo  * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llUnAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotUAlo * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llInTransSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotInt  * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llWorkOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]

			laStkVal[lnUsedCnt +1] = TotWOrd * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laStkVal[lnUsedCnt +1] = TotOrd  * &lcCostFld.
			lnUsedCnt = lnUsedCnt +1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]

		Gather Fields &lcStkFlds. From laStkVal

	Endif
	If llShowSale
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		lnUsedCnt = 0
		If llWipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
			laSalVal[lnUsedCnt +1] = TotWip  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llStockSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotSOH  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llPlanSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotPLA  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
			laSalVal[lnUsedCnt +1] = TotOTS  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llImmOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotIOTS * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llBookSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotBOK  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llShipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotSHP  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llReturnSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotRet  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llRetAuthSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotRetA * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotAlo  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llUnAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]

			laSalVal[lnUsedCnt +1] = TotUAlo * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llInTransSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotInt  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llWorkOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]

			laSalVal[lnUsedCnt +1] = TotWOrd * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
		If llOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

			laSalVal[lnUsedCnt +1] = TotOrd  * Price
			lnUsedCnt = lnUsedCnt + 1
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

		Gather Fields &lcSalFlds. From laSalVal
	Endif
Endif
Select(lnCurAlias)


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

Function lfOTSCalc
Parameter lcItm



Private lnI,lcArray,lcArrayEle,lcSZ,lcArTotEle1,lcArTotEle2,lcArTotEle3

lcArray = "la" + lcItm + "Val"
lcArTotEle  = lcArray + "[9]"

Store 0 To &lcArray

For lnI = 1 To 8
	lcSz = Alltrim(Str(lnI))

	lcArrayEle  = lcArray + "[" + lcSz + "]"

	&lcArrayEle = &lcFromFile..Stk&lcSz+;
		IIF(lcItm='IOTS',0,Iif(lcRPOTSB='W',;
		&lcFromFile..WIP&lcSz,;
		&lcStyle..Plan&lcSz))-;
		&lcFromFile..Ord&lcSz


	Do Case
	Case lcRPOTSSig = 'P' And &lcArrayEle <= 0
		&lcArrayEle = 0
	Case lcRPOTSSig = 'N' And &lcArrayEle >= 0
		&lcArrayEle = 0
	Endcase
	&lcArTotEle = &lcArTotEle + &lcArrayEle
Endfor
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*!*	IF (lcRPOTSSig ='P' AND &lcArTotEle >= lnRPOTSMin) OR ;
*!*	   (lcRPOTSSig ='N' AND &lcArTotEle <= lnRPOTSMin)
If lcRPOTSSig ='A' Or ((Inlist(lcRPOTSSig ,'L','B','P') And &lcArTotEle >= lnRPOTSMin) Or ;
		(Inlist(lcRPOTSSig ,'L','B','N') And &lcArTotEle <= lnRPOTSMax))
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
	Do Case
	Case lcItm = 'OTS'
		Replace lPrnOTS  With .T.
	Case lcItm = 'IOTS'
		Replace lPrnIOTS With .T.
	Endcase
Else
	&lcArTotEle = 0

*B611608,1 Es 06/27/2018  Modify this issue -> "The IOTS Qty displayed in Exported data to Excel is incorrect" [Start]
	Store 0 To &lcArray
*B611608,1 Es 06/27/2018  Modify this issue -> "The IOTS Qty displayed in Exported data to Excel is incorrect" [End]

Endif

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
Function lfUAloCalc

Private lcSz,lcArrayEle

Store 0 To laUAloVal

For lnI = 1 To 8
	lcSz = Alltrim(Str(lnI))
	lcArrayEle  = "laUaloVal" + "[" + lcSz + "]"
	&lcArrayEle = &lcFromFile..Stk&lcSz - &lcFromFile..ALO&lcSz
	laUAloVal[9] = laUAloVal[9] + &lcArrayEle
Endfor

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

Function lfBokCalc
Private lcSz,lcArrayEle

Store 0 To laBokVal

For lnI = 1 To 8
	lcSz = Alltrim(Str(lnI))
	lcArrayEle  = "laBokVal" + "[" + lcSz + "]"
	&lcArrayEle = &lcFromFile..Shp&lcSz+&lcFromFile..Ord&lcSz
	laBokVal[9] = laBokVal[9] + &lcArrayEle
Endfor

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

Function lfUpdTotals
Parameters lcRecType,lcUseFile
Private lcRecType,lcUseFile


Do Case
Case lcRecType = "Maj"
	lcUsedArr = "laT"
	Replace cEndMaj With "A"
Case lcRecType = "Sor"
	lcUsedArr = "laSorT"
	Replace cEndMaj  With "A" ,;
		cEndSort With "A"
Case lcRecType = "Rep"
	lcUsedArr = "laRepT"
	Replace cEndMaj  With "A" ,;
		cEndSort With "A" ,;
		CENDREP  With "A"
Endcase
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
If llWipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]
	Replace WIP1      With WIP1+&lcUseFile..WIP1 ,;
		WIP2      With WIP2+&lcUseFile..WIP2 ,;
		WIP3      With WIP3+&lcUseFile..WIP3 ,;
		WIP4      With WIP4+&lcUseFile..WIP4 ,;
		WIP5      With WIP5+&lcUseFile..WIP5 ,;
		WIP6      With WIP6+&lcUseFile..WIP6 ,;
		WIP7      With WIP7+&lcUseFile..WIP7 ,;
		WIP8      With WIP8+&lcUseFile..WIP8 ,;
		TotWip    With TotWip+&lcUseFile..TotWip
	Replace nStkVWIP    With nStkVWIP +&lcUseFile..nStkVWIP
	Replace nSalVWIP    With nSalVWIP +&lcUseFile..nSalVWIP
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llStockSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	Replace    SOH1      With SOH1+&lcUseFile..SOH1 ,;
		SOH2      With SOH2+&lcUseFile..SOH2 ,;
		SOH3      With SOH3+&lcUseFile..SOH3 ,;
		SOH4      With SOH4+&lcUseFile..SOH4 ,;
		SOH5      With SOH5+&lcUseFile..SOH5 ,;
		SOH6      With SOH6+&lcUseFile..SOH6 ,;
		SOH7      With SOH7+&lcUseFile..SOH7 ,;
		SOH8      With SOH8+&lcUseFile..SOH8 ,;
		TotSOH    With TotSOH+&lcUseFile..TotSOH
	Replace     nSalVSOH    With nSalVSOH +&lcUseFile..nSalVSOH
	Replace   nStkVSOH    With nStkVSOH +&lcUseFile..nStkVSOH

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llPlanSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	Replace PLA1      With PLA1+&lcUseFile..PLA1 ,;
		PLA2      With PLA2+&lcUseFile..PLA2 ,;
		PLA3      With PLA3+&lcUseFile..PLA3 ,;
		PLA4      With PLA4+&lcUseFile..PLA4 ,;
		PLA5      With PLA5+&lcUseFile..PLA5 ,;
		PLA6      With PLA6+&lcUseFile..PLA6 ,;
		PLA7      With PLA7+&lcUseFile..PLA7 ,;
		PLA8      With PLA8+&lcUseFile..PLA8 ,;
		TotPLA    With TotPLA+&lcUseFile..TotPLA
	Replace     nSalVPLA    With nSalVPLA +&lcUseFile..nSalVPLA
	Replace     nStkVPLA    With nStkVPLA +&lcUseFile..nStkVPLA
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llUnAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	Replace     UALO1     With UALO1+&lcUseFile..UALO1 ,;
		UALO2     With UALO2+&lcUseFile..UALO2 ,;
		UALO3     With UALO3+&lcUseFile..UALO3 ,;
		UALO4     With UALO4+&lcUseFile..UALO4 ,;
		UALO5     With UALO5+&lcUseFile..UALO5 ,;
		UALO6     With UALO6+&lcUseFile..UALO6 ,;
		UALO7     With UALO7+&lcUseFile..UALO7 ,;
		UALO8     With UALO8+&lcUseFile..UALO8 ,;
		TotUAlo   With TotUAlo+&lcUseFile..TotUAlo
	Replace     nSalVUALO   With nSalVUALO+&lcUseFile..nSalVUALO
	Replace     nStkVUALO   With nStkVUALO+&lcUseFile..nStkVUALO


*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]


	Replace OTS1      With OTS1+&lcUseFile..OTS1 ,;
		OTS2      With OTS2+&lcUseFile..OTS2 ,;
		OTS3      With OTS3+&lcUseFile..OTS3 ,;
		OTS4      With OTS4+&lcUseFile..OTS4 ,;
		OTS5      With OTS5+&lcUseFile..OTS5 ,;
		OTS6      With OTS6+&lcUseFile..OTS6 ,;
		OTS7      With OTS7+&lcUseFile..OTS7 ,;
		OTS8      With OTS8+&lcUseFile..OTS8 ,;
		TotOTS    With TotOTS+&lcUseFile..TotOTS
	Replace     nSalVOTS    With nSalVOTS +&lcUseFile..nSalVOTS
	Replace     nStkVOTS    With nStkVOTS +&lcUseFile..nStkVOTS


*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llImmOTSSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]


	Replace IOTS1     With IOTS1+&lcUseFile..IOTS1 ,;
		IOTS2     With IOTS2+&lcUseFile..IOTS2 ,;
		IOTS3     With IOTS3+&lcUseFile..IOTS3 ,;
		IOTS4     With IOTS4+&lcUseFile..IOTS4 ,;
		IOTS5     With IOTS5+&lcUseFile..IOTS5 ,;
		IOTS6     With IOTS6+&lcUseFile..IOTS6 ,;
		IOTS7     With IOTS7+&lcUseFile..IOTS7 ,;
		IOTS8     With IOTS8+&lcUseFile..IOTS8 ,;
		TotIOTS   With TotIOTS+&lcUseFile..TotIOTS
	Replace     nSalVIOTS   With nSalVIOTS+&lcUseFile..nSalVIOTS
	Replace     nStkVIOTS   With nStkVIOTS+&lcUseFile..nStkVIOTS


*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]


	Replace ORD1      With ORD1+&lcUseFile..ORD1 ,;
		ORD2      With ORD2+&lcUseFile..ORD2 ,;
		ORD3      With ORD3+&lcUseFile..ORD3 ,;
		ORD4      With ORD4+&lcUseFile..ORD4 ,;
		ORD5      With ORD5+&lcUseFile..ORD5 ,;
		ORD6      With ORD6+&lcUseFile..ORD6 ,;
		ORD7      With ORD7+&lcUseFile..ORD7 ,;
		ORD8      With ORD8+&lcUseFile..ORD8 ,;
		TotOrd    With TotOrd+&lcUseFile..TotOrd
	Replace     nSalVORD    With nSalVORD +&lcUseFile..nSalVORD
	Replace     nStkVORD    With nStkVORD +&lcUseFile..nStkVORD

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llWorkOrderSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	Replace   WORD1     With WORD1+&lcUseFile..WORD1 ,;
		WORD2     With WORD2+&lcUseFile..WORD2 ,;
		WORD3     With WORD3+&lcUseFile..WORD3 ,;
		WORD4     With WORD4+&lcUseFile..WORD4 ,;
		WORD5     With WORD5+&lcUseFile..WORD5 ,;
		WORD6     With WORD6+&lcUseFile..WORD6 ,;
		WORD7     With WORD7+&lcUseFile..WORD7 ,;
		WORD8     With WORD8+&lcUseFile..WORD8 ,;
		TotWOrd   With TotWOrd+&lcUseFile..TotWOrd

	Replace     nSalVWORD   With nSalVWORD+&lcUseFile..nSalVWORD
	Replace     nStkVWORD   With nStkVWORD+&lcUseFile..nStkVWORD
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llInTransSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]

	Replace INT1      With INT1+&lcUseFile..INT1 ,;
		INT2      With INT2+&lcUseFile..INT2 ,;
		INT3      With INT3+&lcUseFile..INT3 ,;
		INT4      With INT4+&lcUseFile..INT4 ,;
		INT5      With INT5+&lcUseFile..INT5 ,;
		INT6      With INT6+&lcUseFile..INT6 ,;
		INT7      With INT7+&lcUseFile..INT7 ,;
		INT8      With INT8+&lcUseFile..INT8 ,;
		TotInt    With TotInt+&lcUseFile..TotInt
	Replace     nSalVINT    With nSalVINT +&lcUseFile..nSalVINT
	Replace     nStkVINT    With nStkVINT +&lcUseFile..nStkVINT
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llBookSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	Replace BOK1      With BOK1+&lcUseFile..BOK1 ,;
		BOK2      With BOK2+&lcUseFile..BOK2 ,;
		BOK3      With BOK3+&lcUseFile..BOK3 ,;
		BOK4      With BOK4+&lcUseFile..BOK4 ,;
		BOK5      With BOK5+&lcUseFile..BOK5 ,;
		BOK6      With BOK6+&lcUseFile..BOK6 ,;
		BOK7      With BOK7+&lcUseFile..BOK7 ,;
		BOK8      With BOK8+&lcUseFile..BOK8 ,;
		TotBOK    With TotBOK+&lcUseFile..TotBOK
	Replace     nSalVBOK    With nSalVBOK +&lcUseFile..nSalVBOK
	Replace     nStkVBOK    With nStkVBOK +&lcUseFile..nStkVBOK

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llShipSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	Replace SHP1      With SHP1+&lcUseFile..SHP1 ,;
		SHP2      With SHP2+&lcUseFile..SHP2 ,;
		SHP3      With SHP3+&lcUseFile..SHP3 ,;
		SHP4      With SHP4+&lcUseFile..SHP4 ,;
		SHP5      With SHP5+&lcUseFile..SHP5 ,;
		SHP6      With SHP6+&lcUseFile..SHP6 ,;
		SHP7      With SHP7+&lcUseFile..SHP7 ,;
		SHP8      With SHP8+&lcUseFile..SHP8 ,;
		TotSHP    With TotSHP+&lcUseFile..TotSHP
	Replace     nSalVSHP    With nSalVSHP +&lcUseFile..nSalVSHP
	Replace     nStkVSHP    With nStkVSHP +&lcUseFile..nStkVSHP

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llAllocSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	Replace   ALO1      With ALO1+&lcUseFile..ALO1 ,;
		ALO2      With ALO2+&lcUseFile..ALO2 ,;
		ALO3      With ALO3+&lcUseFile..ALO3 ,;
		ALO4      With ALO4+&lcUseFile..ALO4 ,;
		ALO5      With ALO5+&lcUseFile..ALO5 ,;
		ALO6      With ALO6+&lcUseFile..ALO6 ,;
		ALO7      With ALO7+&lcUseFile..ALO7 ,;
		ALO8      With ALO8+&lcUseFile..ALO8 ,;
		TotAlo    With TotAlo+&lcUseFile..TotAlo
	Replace     nSalVALO    With nSalVALO +&lcUseFile..nSalVALO
	Replace     nStkVALO    With nStkVALO +&lcUseFile..nStkVALO
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llReturnSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	Replace RET1      With RET1+&lcUseFile..RET1 ,;
		RET2      With RET2+&lcUseFile..RET2 ,;
		RET3      With RET3+&lcUseFile..RET3 ,;
		RET4      With RET4+&lcUseFile..RET4 ,;
		RET5      With RET5+&lcUseFile..RET5 ,;
		RET6      With RET6+&lcUseFile..RET6 ,;
		RET7      With RET7+&lcUseFile..RET7 ,;
		RET8      With RET8+&lcUseFile..RET8 ,;
		TotRet    With TotRet+&lcUseFile..TotRet
	Replace     nSalVRET    With nSalVRET +&lcUseFile..nSalVRET
	Replace     nStkVRET    With nStkVRET +&lcUseFile..nStkVRET

*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
If llRetAuthSel
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	Replace RETA1     With RETA1+&lcUseFile..RETA1 ,;
		RETA2     With RETA2+&lcUseFile..RETA2 ,;
		RETA3     With RETA3+&lcUseFile..RETA3 ,;
		RETA4     With RETA4+&lcUseFile..RETA4 ,;
		RETA5     With RETA5+&lcUseFile..RETA5 ,;
		RETA6     With RETA6+&lcUseFile..RETA6 ,;
		RETA7     With RETA7+&lcUseFile..RETA7 ,;
		RETA8     With RETA8+&lcUseFile..RETA8 ,;
		TotRetA   With TotRetA+&lcUseFile..TotRetA
	Replace     nStkVRETA   With nStkVRETA+&lcUseFile..nStkVRETA
	Replace     nSalVRETA   With nSalVRETA+&lcUseFile..nSalVRETA
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
Endif
*! B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]



















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

Procedure lpWareRela

Select(lcDummy)
Set Order To (lcDummy)
Select(lcTrns)
Set Order To WARESORT
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*SET RELATION TO IIF(NotScale='Y','',SPACE(10)) INTO (lcDummy) ADDITIVE
Set Relation To Iif(NotScale='Y',Space(1),Space(11)) Into (lcDummy) Additive
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]
Select (lcStyDye)
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*  Set Order To lcStyDyeW Ascending
Set Order To (lcStyDyeW) Ascending
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Select (lcDummy)
If !llRpConfig
*B611410,1 AHH 10/09/2017 Error while printing style summary report sorted by location[T20170831.0004][Start]
*!*	    Set Relation To Iif(&lcDummy..Label=Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE)) And  &lcTrns..NotScale='Y',;
*!*	      &lcTrns..WareCode+&lcTrns..StyCode ,;
*!*	      SPACE(25)) Into &lcStyDye Additive
	lcRelExp = "Iif(&lcDummy..Label = "+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE))+" And  &lcTrns..NotScale='Y',&lcTrns..WareCode+&lcTrns..StyCode,SPACE(25))"
	Set Relation To &lcRelExp. Into &lcStyDye Additive
*B611410,1 AHH 10/09/2017 Error while printing style summary report sorted by location[T20170831.0004][End]
Else
	Set Relation To Iif(&lcTrns..NotScale='Y',;
		&lcTrns..WareCode+&lcTrns..StyCode ,;
		SPACE(25)) Into &lcStyDye Additive
Endif
Select(lcTrns)

*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [Start]
*B610311,1 TMI 05/05/2013 [Start] set the order in the correct alias
*SET ORDER TO WareSort2
Set Order To WareSort2 In (lcStyTmp)
*B610311,1 TMI 05/05/2013 [End  ]
*! B610317,1 HIA 04/28/2013 T20130220.0022 - IC - Style summary report incorrect [End]

Set Skip To (lcDummy),&lcStyDye
Set Relation To 'S' + Scale Into &lcscal Additive
Set Relation To  WareCode Into &lcWareHous Additive
Set Order To Tag lcStyle In &lcStyle
Set Relation To  StyCode Into &lcStyle Additive


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

Function lfDatCollS


Private llHasDye,lcWare,lnI,lcPrvMaj,lcScale

Do Case
Case lcRpSortBy == 'S'
	lcSortKey = "''"
Case lcRpSortBy == 'SE'
	lcSortKey = lcStyTmp+'.Season'
Case lcRpSortBy == 'D'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* lcSortKey = lcStyTmp+'.Division'
	lcSortKey = lcStyTmp+'.CDIVISION'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

Case lcRpSortBy == 'FG'
	lcSortKey = lcStyTmp+'.FGroup'
Case lcRpSortBy == 'SG'

*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
*lcSortKey = lcStyTmp+'.SGroup'
	lcSortKey = lcStyTmp+'.CSTYGROUP'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

Case lcRpSortBy == 'MT'
	lcSortKey = lcStyTmp+'.MatType'
Case lcRpSortBy == 'DE'
	lcSortKey = lcStyTmp+'.Dept'
Endcase


llHasDye = .F.

lcStyleWare = Space(25)

Select (lcStyDye)
Set Order To Tag lcStyDye Descending

Select (lcStyle)
Set Order To Tag lcStyle

If Atc("INTO &lcscal",Set("Relation")) > 0
	Set Relation Off Into &lcscal
Endif
Set Relation To 'S' + Scale Into &lcscal Additive

If Atc("INTO &lcStyDye",Set("Relation")) > 0
	Set Relation Off Into &lcStyDye
Endif
Set Relation To Style Into &lcStyDye Additive


lcPrvMaj = Space(19)

lnLocNo = 0
Select (lcStyle)

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
	oProgress = Newobject('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
	oProgress.TotalProgress = Reccount()
	lcCaptionVal = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_CollData,oAriaApplication.GetHeaderText("LANG_ICSTYREP_CollData",AHEADERFILE))
	oProgress.lblFirstLabel.Caption = lcCaptionVal
	oProgress.Show()
	lcCaptionVal = Alltrim(lcCaptionVal)+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Style,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Style",AHEADERFILE))+" : "
	lnCntRecProg  = 1

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
lcScale = Space(3)

Scan
	llAdded = .T.

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
		oProgress.lblFirstLabel.Caption = Alltrim(lcCaptionVal)+ Style
		oProgress.CurrentProgress(lnCntRecProg)
		lnCntRecProg  = lnCntRecProg + 1
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
	ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

	Do lpInsInTmp With '&lcStyle'

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*IF !llRPShwZer AND ;
(&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS +;
&lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet +;
&lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt +;
&lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*!*	    If !llRPWhDeta And  !llRPShwZer And ;
*!*	        (&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS +;
*!*	        &lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet +;
*!*	        &lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt +;
*!*	        &lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[Start]
*!*		If !llRPWhDeta And  !llRPShwZer And ;
*!*				(Iif(llWipSel,&lcStyTmp..TotWip,0)+;
*!*				IIF(llStockSel,&lcStyTmp..TotSOH,0)+;
*!*				IIF(llPlanSel,&lcStyTmp..TotPLA,0)+;
*!*				IIF(llOTSSel ,&lcStyTmp..TotOTS ,0)+;
*!*				IIF(llImmOTSSel,&lcStyTmp..TotIOTS,0)+;
*!*				IIF(llWorkOrderSel,&lcStyTmp..TotWOrd,0)+;
*!*				IIF(llBookSel,&lcStyTmp..TotBOK ,0)+;
*!*				IIF(llShipSel,&lcStyTmp..TotSHP ,0)+;
*!*				IIF(llReturnSel,&lcStyTmp..TotRet ,0)+;
*!*				IIF(llRetAuthSel,&lcStyTmp..TotRetA ,0)+;
*!*				IIF(llUnAllocSel,&lcStyTmp..TotUAlo ,0)+;
*!*				IIF(llAllocSel,&lcStyTmp..TotAlo ,0)+;
*!*				IIF(llOrderSel,&lcStyTmp..TotOrd,0)+;
*!*				IIF(llInTransSel,&lcStyTmp..TotInt ,0)) = 0
	If !llRPWhDeta And  !llRPShwZer And ;
			(Iif(llWipSel,&lcStyTmp..TotWip,0) = 0 AND;
			IIF(llStockSel,&lcStyTmp..TotSOH,0) = 0 AND;
			IIF(llPlanSel,&lcStyTmp..TotPLA,0) = 0 AND;
			IIF(llOTSSel ,&lcStyTmp..TotOTS ,0) = 0 AND;
			IIF(llImmOTSSel,&lcStyTmp..TotIOTS,0) = 0 AND;
			IIF(llWorkOrderSel,&lcStyTmp..TotWOrd,0) = 0 AND;
			IIF(llBookSel,&lcStyTmp..TotBOK ,0) = 0 AND;
			IIF(llShipSel,&lcStyTmp..TotSHP ,0) = 0 AND;
			IIF(llReturnSel,&lcStyTmp..TotRet ,0) = 0 AND;
			IIF(llRetAuthSel,&lcStyTmp..TotRetA ,0) = 0 AND;
			IIF(llUnAllocSel,&lcStyTmp..TotUAlo ,0) = 0 AND;
			IIF(llAllocSel,&lcStyTmp..TotAlo ,0) = 0 AND;
			IIF(llOrderSel,&lcStyTmp..TotOrd,0) = 0 AND;
			IIF(llInTransSel,&lcStyTmp..TotInt ,0) = 0) 
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[End]
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

		Select (lcStyTmp)
		Delete
		llAdded = .F.
	Else

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*IF llOnlyOTS AND !&lcStyTmp..lPrnOTS AND !&lcStyTmp..lPrnIOTS
		If !llRPWhDeta And llOnlyOTS And !&lcStyTmp..lPrnOTS And !&lcStyTmp..lPrnIOTS
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

			Select (lcStyTmp)
			Delete
			llAdded = .F.
		Else

*------------------------------------------------------*
			Select (lcStyTmp)
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* Scatter Fields CSTYMAJOR,StyCode,WareCode,DYELOT,SEASON,Division,FGroup,SGroup,MatType,Dept,Scale,Price,TotCost,Ave_Cost Memvar
			Scatter Fields CSTYMAJOR,StyCode,WareCode,DYELOT,SEASON,CDIVISION,FGroup,CSTYGROUP,MatType,Dept,Scale,Price,TotCost,Ave_Cost Memvar
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]


			Select (lcTotTmp)
			If !Seek('A  '+&lcSortKey+&lcStyTmp..CSTYMAJOR)
				Append Blank
			Endif
			Gather Memvar
			Replace cEndMaj With "A",NotScale With 'Y'

			= lfUpdTotals("Maj",lcStyTmp)

			If !(lcRpSortBy == 'S')
				If !Seek('AA '+&lcSortKey)
					Append Blank
				Endif
				Gather Memvar
				Replace cEndMaj With "A",cEndSort With "A",NotScale With 'Y'

				= lfUpdTotals("Sor",lcStyTmp)
			Endif

			If !Seek('AAA')
				Append Blank
			Endif
			Gather Memvar
			Replace cEndMaj With "A",cEndSort With "A",CENDREP With "A",NotScale With 'Y'

			= lfUpdTotals("Rep",lcStyTmp)

*------------------------------------------------------*
*--Collect data From StyDye for Location Details ,or dyelots for Stock,or print configuration=Yes
			If llConfig
				llSetup= llRpConfig
			Else
				llSetup= llRPPrnDye
			Endif
			If llRPWhDeta Or llSetup
				lnselAlias=Select()
				Select (lcStyDye)
				Scan While Style = &lcStyle..Style
					llNew       = lcStyleWare # &lcStyDye..Style+&lcStyDye..CWARECODE
					lcStyleWare = &lcStyDye..Style+&lcStyDye..CWARECODE
					lnLocNo     = Iif(llNew, 0, lnLocNo)
					If llSetup And llNew
						llHasDye = !Empty(DYELOT)
					Endif
					If !Empty(DYELOT)
						Loop
					Endif


					Do lpInsInTmp With '&lcStyDye'




*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
					If llRPWhDeta And llOnlyOTS And !&lcTrns..lPrnOTS And !&lcTrns..lPrnIOTS
						Select (lcTrns)
						Loop
					Endif
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*        If llRPWhDeta And  !llRPShwZer And ;
(&lcTrns..TotWip  + &lcTrns..TotSOH + &lcTrns..TotPLA  + &lcTrns..TotOTS +;
&lcTrns..TotIOTS + &lcTrns..TotBOK + &lcTrns..TotSHP  + &lcTrns..TotRet +;
&lcTrns..TotRetA + &lcTrns..TotAlo + &lcTrns..TotUAlo + &lcTrns..TotInt +;
&lcTrns..TotWOrd + &lcTrns..TotOrd) = 0
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[Start]
*!*						If llRPWhDeta And  !llRPShwZer And ;
*!*								(Iif(llWipSel,&lcTrns..TotWip,0)+;
*!*								IIF(llStockSel,&lcTrns..TotSOH,0)+;
*!*								IIF(llPlanSel,&lcTrns..TotPLA,0)+;
*!*								IIF(llOTSSel ,&lcTrns..TotOTS ,0)+;
*!*								IIF(llImmOTSSel,&lcTrns..TotIOTS ,0)+;
*!*								IIF(llWorkOrderSel,&lcTrns..TotWOrd,0)+;
*!*								IIF(llBookSel,&lcTrns..TotBOK ,0)+;
*!*								IIF(llShipSel,&lcTrns..TotSHP ,0)+;
*!*								IIF(llReturnSel,&lcTrns..TotRet ,0)+;
*!*								IIF(llRetAuthSel,&lcTrns..TotRetA ,0)+;
*!*								IIF(llUnAllocSel,&lcTrns..TotUAlo ,0)+;
*!*								IIF(llAllocSel,&lcTrns..TotAlo ,0)+;
*!*								IIF(llOrderSel,&lcTrns..TotOrd,0)+;
*!*								IIF(llInTransSel,&lcTrns..TotInt ,0)) = 0
					If llRPWhDeta And  !llRPShwZer And ;
							(Iif(llWipSel,&lcTrns..TotWip,0)=0 AND;
							IIF(llStockSel,&lcTrns..TotSOH,0)=0 AND;
							IIF(llPlanSel,&lcTrns..TotPLA,0)=0 AND;
							IIF(llOTSSel ,&lcTrns..TotOTS ,0)=0 AND;
							IIF(llImmOTSSel,&lcTrns..TotIOTS ,0)=0 AND;
							IIF(llWorkOrderSel,&lcTrns..TotWOrd,0)=0 AND;
							IIF(llBookSel,&lcTrns..TotBOK ,0)=0 AND;
							IIF(llShipSel,&lcTrns..TotSHP ,0)=0 AND;
							IIF(llReturnSel,&lcTrns..TotRet ,0)=0 AND;
							IIF(llRetAuthSel,&lcTrns..TotRetA ,0)=0 AND;
							IIF(llUnAllocSel,&lcTrns..TotUAlo ,0)=0 AND;
							IIF(llAllocSel,&lcTrns..TotAlo ,0)=0 AND;
							IIF(llOrderSel,&lcTrns..TotOrd,0)=0 AND;
							IIF(llInTransSel,&lcTrns..TotInt ,0)=0) 
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[End]							
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
						Select (lcTrns)
						Delete
						Loop
					Endif

					Select(lcStyTmp)
					Locate
					Locate For StyCode = &lcStyle..Style
					If Found()

						= lfUpdTotals("",lcTrns)
						If ",IOTS," $ Upper(lcAllTrns)
							=lfOTSCalcW('IOTS')
						Endif
						If ",OTS," $ Upper(lcAllTrns)
							=lfOTSCalcW('OTS')
						Endif
					Endif


					Select (lcTotTmp)
					If Seek('A  '+&lcSortKey+&lcStyTmp..CSTYMAJOR)
						= lfUpdTotals("Maj",lcTrns)
					Endif

					If !(lcRpSortBy == 'S')
						If Seek('AA '+&lcSortKey)
							= lfUpdTotals("Sor",lcTrns)
						Endif
					Endif


					If Seek('AAA')
						= lfUpdTotals("Rep",lcTrns)
					Endif
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


					If llOnlyOTS And !&lcTrns..lPrnOTS And !&lcTrns..lPrnIOTS
						Select (lcStyTmp)
					Else
						If llRPWhDeta And llRPPrnLoc
							If Seek(&lcStyDye..Style+Space(6)+&lcStyDye..CWARECODE,lcWhsLoc)
								Select (lcWhsLoc)
								lcLoc = ''
								Scan Rest While Style+Color+CWARECODE = ;
										&lcStyDye..Style+Space(6)+&lcStyDye..CWARECODE ;
										FOR !Empty(cLocation)
									llHasLoc = .T.

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcLoc =  lcLoc + IIF(EMPTY(lcLoc),LANG_ICSTYREP_Bins+'                 ',SPACE(2)) + &lcWhsLoc..cLocation
									lcLoc =  lcLoc + Iif(Empty(lcLoc),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Bins,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Bins",AHEADERFILE))+'                 ',Space(2)) + &lcWhsLoc..cLocation
*N000682,1 11/20/2012 MMT Globlization changes[End]

								Endscan
								If llHasLoc
									Select (lcTrns)
									Replace HasLoc With llHasLoc,;
										Loc    With lcLoc
								Endif
							Endif
						Endif
					Endif
				Endscan
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
* If llRPWhDeta And  !llRPShwZer And ;
(&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS +;
&lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet +;
&lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt +;
&lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[Start]	
*!*					If llRPWhDeta And  !llRPShwZer And ;
*!*							(Iif(llWipSel,&lcStyTmp..TotWip,0)+;
*!*							IIF(llStockSel,&lcStyTmp..TotSOH,0)+;
*!*							IIF(llPlanSel,&lcStyTmp..TotPLA,0)+;
*!*							IIF(llOTSSel ,&lcStyTmp..TotOTS ,0)+;
*!*							IIF(llImmOTSSel,&lcStyTmp..TotIOTS ,0)+;
*!*							IIF(llWorkOrderSel,&lcStyTmp..TotWOrd,0)+;
*!*							IIF(llBookSel,&lcStyTmp..TotBOK ,0)+;
*!*							IIF(llShipSel,&lcStyTmp..TotSHP ,0)+;
*!*							IIF(llReturnSel,&lcStyTmp..TotRet ,0)+;
*!*							IIF(llRetAuthSel,&lcStyTmp..TotRetA ,0)+;
*!*							IIF(llUnAllocSel,&lcStyTmp..TotUAlo ,0)+;
*!*							IIF(llAllocSel,&lcStyTmp..TotAlo ,0)+;
*!*							IIF(llOrderSel,&lcStyTmp..TotOrd,0)+;
*!*							IIF(llInTransSel,&lcStyTmp..TotInt ,0)) = 0
				If llRPWhDeta And  !llRPShwZer And ;
						(Iif(llWipSel,&lcStyTmp..TotWip,0)=0 AND;
						IIF(llStockSel,&lcStyTmp..TotSOH,0)=0 AND;
						IIF(llPlanSel,&lcStyTmp..TotPLA,0)=0 AND;
						IIF(llOTSSel ,&lcStyTmp..TotOTS ,0)=0 AND;
						IIF(llImmOTSSel,&lcStyTmp..TotIOTS ,0)=0 AND;
						IIF(llWorkOrderSel,&lcStyTmp..TotWOrd,0)=0 AND;
						IIF(llBookSel,&lcStyTmp..TotBOK ,0)=0 AND;
						IIF(llShipSel,&lcStyTmp..TotSHP ,0)=0 AND;
						IIF(llReturnSel,&lcStyTmp..TotRet ,0)=0 AND;
						IIF(llRetAuthSel,&lcStyTmp..TotRetA ,0)=0 AND;
						IIF(llUnAllocSel,&lcStyTmp..TotUAlo ,0)=0 AND;
						IIF(llAllocSel,&lcStyTmp..TotAlo ,0)=0 AND;
						IIF(llOrderSel,&lcStyTmp..TotOrd,0)=0 AND;
						IIF(llInTransSel,&lcStyTmp..TotInt ,0)=0 ) 
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[End]	
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
					Select (lcStyTmp)
					Delete
					llAdded = .F.
					Select(lcTotTmp)
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*!*	            Delete For StyCode = &lcStyle..Style And  Iif(cEndSort = "A",(&lcTotTmp..TotWip  + &lcTotTmp..TotSOH + &lcTotTmp..TotPLA  + &lcStyTmp..TotOTS +;
*!*	              &lcTotTmp..TotIOTS + &lcTotTmp..TotBOK + &lcTotTmp..TotSHP  + &lcTotTmp..TotRet +;
*!*	              &lcTotTmp..TotRetA + &lcTotTmp..TotAlo + &lcTotTmp..TotUAlo + &lcTotTmp..TotInt +;
*!*	              &lcTotTmp..TotWOrd + &lcTotTmp..TotOrd) = 0   ,.T.)
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[Start]
*!*						Delete For StyCode = &lcStyle..Style And  Iif(cEndSort = "A",(Iif(llWipSel,&lcStyTmp..TotWip,0)+;
*!*							IIF(llStockSel,&lcStyTmp..TotSOH,0)+;
*!*							IIF(llPlanSel,&lcStyTmp..TotPLA,0)+;
*!*							IIF(llOTSSel ,&lcStyTmp..TotOTS ,0)+;
*!*							IIF(llImmOTSSel,&lcStyTmp..TotIOTS ,0)+;
*!*							IIF(llWorkOrderSel,&lcStyTmp..TotWOrd,0)+;
*!*							IIF(llBookSel,&lcStyTmp..TotBOK ,0)+;
*!*							IIF(llShipSel,&lcStyTmp..TotSHP ,0)+;
*!*							IIF(llReturnSel,&lcStyTmp..TotRet ,0)+;
*!*							IIF(llRetAuthSel,&lcStyTmp..TotRetA ,0)+;
*!*							IIF(llUnAllocSel,&lcStyTmp..TotUAlo ,0)+;
*!*							IIF(llAllocSel,&lcStyTmp..TotAlo ,0)+;
*!*							IIF(llOrderSel,&lcStyTmp..TotOrd,0)+;
*!*							IIF(llInTransSel,&lcStyTmp..TotInt ,0)) = 0  ,.T.)
					Delete For StyCode = &lcStyle..Style And  Iif(cEndSort = "A",(Iif(llWipSel,&lcStyTmp..TotWip,0)=0 AND;
						IIF(llStockSel,&lcStyTmp..TotSOH,0)=0 AND;
						IIF(llPlanSel,&lcStyTmp..TotPLA,0)=0 AND;
						IIF(llOTSSel ,&lcStyTmp..TotOTS ,0)=0 AND;
						IIF(llImmOTSSel,&lcStyTmp..TotIOTS ,0)=0 AND;
						IIF(llWorkOrderSel,&lcStyTmp..TotWOrd,0)=0 AND;
						IIF(llBookSel,&lcStyTmp..TotBOK ,0)=0 AND;
						IIF(llShipSel,&lcStyTmp..TotSHP ,0)=0 AND;
						IIF(llReturnSel,&lcStyTmp..TotRet ,0)=0 AND;
						IIF(llRetAuthSel,&lcStyTmp..TotRetA ,0)=0 AND;
						IIF(llUnAllocSel,&lcStyTmp..TotUAlo ,0)=0 AND;
						IIF(llAllocSel,&lcStyTmp..TotAlo ,0)=0 AND;
						IIF(llOrderSel,&lcStyTmp..TotOrd,0)=0 AND;
						IIF(llInTransSel,&lcStyTmp..TotInt ,0)=0) ,.T.)
*! B612682,1 MMT 06/14/2023 The report does not print style has OTS and immediate IOTS same value with opposite sign if show zero qty option is NO[End]						
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
				Endif
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

				Select(lnselAlias)
			Endif
		Endif
	Endif




	If llAdded

		If lcScale <> &lcscal..Scale
			Do lpInsInTmp With '&lcStyle',.T.
			lcScale = &lcscal..Scale
		Endif
	Endif

Endscan


*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
	oProgress = Null
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]

*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

Select (lcStyTmp)
If !Empty(oAriaApplication.WorkDir+lcTotTmp)
	Append From (oAriaApplication.WorkDir+lcTotTmp)
Endif



Select (lcStyle)
Set Order To
Set Relation To

Select (lcStyDye)
Set Order To
Set Relation To

Do lpStyleRela



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

Procedure lpStyleRela

Set Order To StySort In (lcTrns)
Select(lcStyTmp)
Do Case
Case lcRpSortBy == 'S'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl  = LANG_ICSTYREP_Style
	lcSortTtl  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Style,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Style",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lcGroupExp = "''"
	lcSortExp  = "''"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(LANG_ICSTYREP_Description),LEN(ALLTRIM(lcRepNMTtl)))-LEN(lcMajTtl),0))]
*lcNMajExp  = [ALLTRIM(lcRepNMTtl) + SPACE(1) + PADL(": ",2+MAX(MAX(LEN(LANG_ICSTYREP_Description),LEN(lcMajTtl))-LEN(ALLTRIM(lcRepNMTtl)),0))]
*lcDescExp  = [LANG_ICSTYREP_Description + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(lcMajTtl),LEN(ALLTRIM(lcRepNMTtl)))-LEN(LANG_ICSTYREP_Description),0))]
	lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))),LEN(ALLTRIM(lcRepNMTtl)))-LEN(lcMajTtl),0))]
	lcNMajExp  = [ALLTRIM(lcRepNMTtl) + SPACE(1) + PADL(": ",2+MAX(MAX(LEN(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))),LEN(lcMajTtl))-LEN(ALLTRIM(lcRepNMTtl)),0))]
	lcDescExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		[' + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(lcMajTtl),LEN(ALLTRIM(lcRepNMTtl)))-LEN(']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+['),0))]
*N000682,1 11/20/2012 MMT Globlization changes[End]

Case lcRpSortBy == 'SE'
	lcSortFld  = '.Season'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl  = LANG_ICSTYREP_Season
	lcSortTtl  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Season,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Season",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lcGroupExp = lcStyTmp+'.Season'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortExp  = [LANG_ICSTYREP_Season+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Season),0))]
*lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Season)-LEN(lcMajTtl),0))]
*lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
*lcDescExp  = [LANG_ICSTYREP_Description + SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
	lcSortExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Season,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Season",AHEADERFILE))+;
		['+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Season,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Season",AHEADERFILE))+['),0))]
	lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Season,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Season",AHEADERFILE))+[')-LEN(lcMajTtl),0))]
	lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+[')-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
	lcDescExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		[' + SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+['),0))]
*N000682,1 11/20/2012 MMT Globlization changes[End]

Case lcRpSortBy == 'D'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
*  lcSortFld  = '.Division'
	lcSortFld  = '.CDIVISION'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]


*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl  = LANG_ICSTYREP_Division
	lcSortTtl  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Division,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Division",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
*  lcGroupExp = lcStyTmp+'.Division'
	lcGroupExp = lcStyTmp+'.CDIVISION'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]


*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortExp  = [LANG_ICSTYREP_Division+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Division),0))]
*lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Division)-LEN(lcMajTtl),0))]
*lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
*lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
	lcSortExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Division,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Division",AHEADERFILE))+;
		['+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Division,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Division",AHEADERFILE))+['),0))]
	lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Division,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Division",AHEADERFILE))+[')-LEN(lcMajTtl),0))]
	lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		[')-LEN(ALLTRIM(lcRepNMTtl)),0))]
	lcDescExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		['+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+['),0))]
*N000682,1 11/20/2012 MMT Globlization changes[End]

Case lcRpSortBy == 'FG'
	lcSortFld  = '.FGroup'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl  = LANG_ICSTYREP_Primary_Fabric
	lcSortTtl  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Primary_Fabric,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Primary_Fabric",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lcGroupExp = lcStyTmp+'.FGroup'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortExp  = [LANG_ICSTYREP_Primary_Fabric+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Primary_Fabric),0))]
*lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Primary_Fabric)-LEN(lcMajTtl),0))]
*lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
*lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
	lcSortExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Primary_Fabric,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Primary_Fabric",AHEADERFILE))+;
		['+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Primary_Fabric,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Primary_Fabric",AHEADERFILE))+['),0))]
	lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Primary_Fabric,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Primary_Fabric",AHEADERFILE))+;
		[')-LEN(lcMajTtl),0))]
	lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		[')-LEN(ALLTRIM(lcRepNMTtl)),0))]
	lcDescExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		['+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+['),0))]
*N000682,1 11/20/2012 MMT Globlization changes[End]

Case lcRpSortBy == 'SG'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
*lcSortFld  = '.SGroup'
	lcSortFld  = '.CSTYGROUP'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl  = LANG_ICSTYREP_Style_Group
	lcSortTtl  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Style_Group,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Style_Group",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [Start]
* lcGroupExp = lcStyTmp+'.SGroup'
	lcGroupExp = lcStyTmp+'.CSTYGROUP'
*B611662,1 Es 09/09/2018 Modify the issue "Style Summary report exports the Division and Style group as code not description to Excel" [End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortExp  = [LANG_ICSTYREP_Style_Group+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Style_Group),0))]
*lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Style_Group)-LEN(lcMajTtl),0))]
*lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
*lcDescExp  = [LANG_ICSTYREP_Description + SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
	lcSortExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Style_Group,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Style_Group",AHEADERFILE))+;
		['+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Style_Group,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Style_Group",AHEADERFILE))+['),0))]
	lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Style_Group,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Style_Group",AHEADERFILE))+[')-LEN(lcMajTtl),0))]
	lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+[')-LEN(ALLTRIM(lcRepNMTtl)),0))]
	lcDescExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		[' + SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+['),0))]
*N000682,1 11/20/2012 MMT Globlization changes[End]

Case lcRpSortBy == 'MT'
	lcSortFld  = '.MatType'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl  = LANG_ICSTYREP_Material_type
	lcSortTtl  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Material_type,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Material_type",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lcGroupExp = lcStyTmp+'.MatType'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortExp  = [LANG_ICSTYREP_Material_type + SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Material_type),0))]
*lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Material_type)-LEN(lcMajTtl),0))]
*lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0))]
*lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
	lcSortExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Material_type,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Material_type",AHEADERFILE))+;
		[' + SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Material_type,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Material_type",AHEADERFILE))+['),0))]
	lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Material_type,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Material_type",AHEADERFILE))+[')-LEN(lcMajTtl),0))]
	lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		[')-LEN(ALLTRIM(lcRepNMTtl)),0))]
	lcDescExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		['+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+['),0))]
*N000682,1 11/20/2012 MMT Globlization changes[End]

Case lcRpSortBy == 'DE'
	lcSortFld  = '.Dept'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortTtl  = LANG_ICSTYREP_Department
	lcSortTtl  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Department,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Department",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lcGroupExp = lcStyTmp+'.Dept'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcSortExp  = [LANG_ICSTYREP_Department+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(LANG_ICSTYREP_Department),0))]
*lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(LANG_ICSTYREP_Department)-LEN(lcMajTtl),0))]
*lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(LANG_ICSTYREP_Description)-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
*lcDescExp  = [LANG_ICSTYREP_Description+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(LANG_ICSTYREP_Description),0))]
	lcSortExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Department,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Department",AHEADERFILE))+;
		['+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Department,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Department",AHEADERFILE))+['),0))]
	lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN(']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Department,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Department",AHEADERFILE))+[')-LEN(lcMajTtl),0))]
	lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+[')-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
	lcDescExp  = [']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+;
		['+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN(']+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Description,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Description",AHEADERFILE))+['),0))]
*N000682,1 11/20/2012 MMT Globlization changes[End]
Endcase

Set Order To (lcDummy) In (lcDummy)
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*  Set Order To Tag lcStyDye Ascending In &lcStyDye
Set Order To Tag (lcStyDye) Ascending In &lcStyDye
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
Select(lcStyTmp)
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][Start]
*SET RELATION TO IIF(NotScale='Y','',SPACE(10)) INTO (lcDummy) ADDITIVE
Set Relation To Iif(NotScale='Y',Space(1),Space(11)) Into (lcDummy) Additive
*! B610376,1 MMT 06/11/2013 Fix the problem of missing report data due to VFP9 SP2[T20130606.0026][End]
Select (lcDummy)
Set Relation To Iif(&lcStyTmp..NotScale='Y',&lcStyTmp..StyCode,Space(10)) Into (lcTrns) Additive
Select (lcTrns)
Set Relation To Iif(&lcStyTmp..NotScale='Y',StyCode+WareCode,Space(10)) Into &lcStyDye Additive
Select (lcStyTmp)
Set Skip To (lcDummy),(lcTrns),&lcStyDye Additive
Set Relation To 'S' + Scale Into &lcscal Additive
Set Order To Tag lcStyle In &lcStyle
Set Relation To StyCode Into &lcStyle Additive

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

Function lfCostSalPr


lcReturn = ''

If llShowCost
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lcReturn = SPACE(15) + LANG_ICSTYREP_Average_cost +' : '
*ELSE
*B608407,1 WAM 01/14/2008 (End)
	If lcCstMeth = "S"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcReturn = SPACE(15) + LANG_ICSTYREP_Unit_cost + ' :    '
		lcReturn = Space(15) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Cost,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_cost",AHEADERFILE)) + ' :    '
*N000682,1 11/20/2012 MMT Globlization changes[End]

	Else
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcReturn = SPACE(15) + LANG_ICSTYREP_Average_cost +' : '
		lcReturn = Space(15) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Average_cost,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Average_cost",AHEADERFILE)) +' : '
*N000682,1 11/20/2012 MMT Globlization changes[End]

	Endif
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)


	If lcRpSortBy == 'W'

*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..Ave_Cost,10,2))
*ELSE
*B608407,1 WAM 01/14/2008 (End)

		If lcCstMeth = "S"
			lcReturn = lcReturn + Alltrim(Str(&lcTrns..TotCost,10,2))
		Else
			lcReturn = lcReturn + Alltrim(Str(&lcTrns..Ave_Cost,10,2))
		Endif
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)

	Else
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".Ave_Cost"),10,2))
*ELSE
*B608407,1 WAM 01/14/2008 (End)

		If lcCstMeth = "S"
			lcReturn = lcReturn + Alltrim(Str(Eval(lcStyTmp+".TotCost"),10,2))
		Else
			lcReturn = lcReturn + Alltrim(Str(Eval(lcStyTmp+".Ave_Cost"),10,2))
		Endif
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)

	Endif

	If llShowSale
		If lcRpSortBy == 'W'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcReturn = lcReturn + SPACE(15)+LANG_ICSTYREP_Unit_Price+'  : ' + ALLTRIM(STR(EVAL(lcTrns+".Price"),12,2))
			lcReturn = lcReturn + Space(15)+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Price,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Price",AHEADERFILE))+'  : ' + Alltrim(Str(Eval(lcTrns+".Price"),12,2))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		Else
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcReturn = lcReturn + SPACE(15)+LANG_ICSTYREP_Unit_Price+'  : ' + ALLTRIM(STR(EVAL(lcStyTmp+".Price"),12,2))
			lcReturn = lcReturn + Space(15)+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Price,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Price",AHEADERFILE))+'  : ' + Alltrim(Str(Eval(lcStyTmp+".Price"),12,2))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		Endif
	Endif
Else
	If llShowSale
		If lcRpSortBy == 'W'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcReturn = SPACE(15) + LANG_ICSTYREP_Unit_Price+' : ' + ALLTRIM(STR(EVAL(lcTrns+".Price"),12,2))
			lcReturn = Space(15) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Price,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Price",AHEADERFILE))+' : ' + Alltrim(Str(Eval(lcTrns+".Price"),12,2))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		Else
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcReturn = SPACE(15) + LANG_ICSTYREP_Unit_Price+' : ' + ALLTRIM(STR(EVAL(lcStyTmp+".Price"),12,2))
			lcReturn = Space(15) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Unit_Price,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Unit_Price",AHEADERFILE))+' : ' + Alltrim(Str(Eval(lcStyTmp+".Price"),12,2))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		Endif
	Endif
Endif

Return lcReturn

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

Function lfDyltItm
Parameters lcFld

Private lcFld,lnRetVal

lnRetVal = 0

Do Case
Case lcFld = 'Tot'
	lnRetVal = &lcStyDye..ToTSTK
Case lcFld = 'nStkVal'
	lnRetVal = &lcStyDye..nStkVal
Case lcFld = 'Price'
	lnRetVal = (&lcStyDye..ToTSTK )* (&lcStyle..PriceA)
Otherwise
	lnRetVal = Eval(lcStyDye+'.Stk'+lcFld)
Endcase

Return lnRetVal

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
Function lfGetCosts
Parameters lnThisCost
Private lnCurrAlis , lcSeekExpr
lnCurrAlis = Select(0)
Select RevStyDy
lcSeekExpr = Iif(lcRpSortBy== "S",&lcStyDye..Style+&lcStyDye..CWARECODE,;
	&lcStyDye..CWARECODE+&lcStyDye..Style)
If Seek(lcSeekExpr)
	lnThisCost = Ave_Cost
Else
	lnThisCost = 0
Endif
Select (lnCurrAlis)
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
Function lfInitVals

*: B608711,1 MMT 10/08/2008 Fix bug of Wrong Positive OTS when use print BOTH OTS [Start]
*STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,;
lnSize5,lnSize6,lnSize7,lnSize8,lnSize9,lcSize10,lnSize11,;
lnNonPrice,lnNonCost
Store 0 To lnSize1,lnSize2,lnSize3,lnSize4,;
	lnSize5,lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11,;
	lnNonPrice,lnNonCost
*: B608711,1 MMT 10/08/2008 Fix bug of Wrong Positive OTS when use print BOTH OTS [End]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
Store .F. To llChkAllSz
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

If lcMastFile = "&lcStyDye" And llPrintClr
	Store 0 To lnClrSz1,lnClrSz2,lnClrSz3,lnClrSz4,;
		lnClrSz5,lnClrSz6,lnClrSz7,lnClrSz8,lnClrSz9,lnClrSz10,lnClrSz11,;
		lnClrPrice,lnClrCost
Endif
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
Function lfPrintHdr
Private lcPrintHdr

lcOldAlias = Select(0)



If lcRpSortBy == "S"
	lcPrintHdr = lfStyHeadr()  && Sort by style header.
Else
	lcPrintHdr = lfLocHeadr()  && Sort by location header.
Endif
Select(lcOldAlias)

Return lcPrintHdr
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
Function lfStyHeadr
Private lcPrintHdr , lcSize , lnAllSizes

Store '' To lcNonMjDes , lcPrintHdr , lcLocBins
lnAllSizes = 0
=lfInitVals()  && Assign initial values to all line variables.
*-- Evaluate all sizes values. [Begin]
For lnAllSizes = 1 To 8
	lcSize  = "lnSize"+Str(lnAllSizes,1)
	&lcSize = Evaluate(laTranAray[lnAllSizes])
	lnSize9 = lnSize9 + &lcSize

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
	If !llRPShwZer And &lcSize <> 0
		llChkAllSz = .T.
	Endif
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


	If lcRPOTSSig $ 'BL'
		If &lcSize > 0
			lnSize10 = lnSize10+ &lcSize && variable hold sum of +ve OTS Qtys
		Else
			lnSize11 = lnSize11+ &lcSize && variable hold sum of -ve OTS Qtys
		Endif
	Endif

Endfor


*-- Evaluate all sizes values. [End  ]

*-- Zero values if OTS or IOTS and condition is true. [Begin]
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMin)))
If ("OTS" $ lcRepTarVl) And (((lcRPOTSSig ="P") And (lnSize9 < lnRPOTSMin)) Or ((lcRPOTSSig ="N") And (lnSize9 > lnRPOTSMax)) Or (lcRPOTSSig $ 'LB' And (lnSize9 > lnRPOTSMax And lnSize9 < lnRPOTSMin)))
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]

	Store 0 To lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
		lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11

Endif
*-- Zero values if OTS or IOTS and condition is true. [End  ]



*-- if user want to print zeros or total value not equal zero.
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*IF llRpShwZer OR lnSize9 <> 0
If llRPShwZer Or lnSize9 <> 0 Or (!llRPShwZer And llChkAllSz)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
	lnNonPrice = lnSize9 * (&lcStyle..PriceA)
	If lcMastFile = "&lcStyle" Or Empty(DYELOT)

*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="S", &lcStyle..TotCost,Ave_Cost)*lnSize9)
*ELSE
*B608407,1 WAM 01/14/2008 (End)

		lnNonCost  = Iif("SOH" $ lcRepTarVl , Iif(lcCstMeth="S" , (&lcStyle..TotCost) * lnSize9 ,nStkVal) , Iif(lcCstMeth="S", &lcStyle..TotCost,Ave_Cost)*lnSize9)
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)


*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lnCstAveVl = IIF("SOH" $ lcRepTarVl , nStkVal/lnSize9 , IIF(lcCstMeth="S" , ;
&lcStyle..TotCost,Ave_Cost))
*ELSE
		lnCstAveVl = Iif("SOH" $ lcRepTarVl , Iif(lcCstMeth="S" , &lcStyle..TotCost ,nStkVal/lnSize9 ) , Iif(lcCstMeth="S" , ;
			&lcStyle..TotCost,Ave_Cost))
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)

	Else
		If !llRPWhDeta
			If lcCstMeth="A"
				= lfGetCosts(@lnCstAveVl)
			Else
				lnCstAveVl = &lcStyle..TotCost
			Endif
		Endif
		lnNonCost = lnSize9 * lnCstAveVl
	Endif

	=lfNonMjDes()  && Evaluate Non Major Description.

*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*  IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
	If lnFrstRec1 = 0 Or lnFrstRec1 = Recno()
		lnFrstRec1 = Recno()
		lcPrntSty = Space(lnMajorlen)
		lcScalCode = Space(3)
	Endif
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*  ENDIF
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

*-- if it is new style group.
	If !(Padr(&lcStyle..Style,lnMajorlen)==lcPrntSty)
* B607997,1 T20070119.0022 AYM 03/06/2007 Problem not printing first col_line if the prev. style ends with same color [Begin]
		lcPrntNon = Space(19 - lnMajorlen)
* B607997,1 T20070119.0022 AYM 03/06/2007 Problem not printing first col_line if the prev. style ends with same color [End]
		lcPrntSty = Padr(&lcStyle..Style,lnMajorlen)
*-- if not print sizes "lcPrintHdr" will not include scales
		If !LLRPPRTSIZ
			lcPrintHdr = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,15) +;
				lfEndOfGrp(@lnEndOfSty,Padr(&lcStyle..Style,lnMajorlen)),35) + Space(3)
		Else
			If llTextMode
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
				lcPrintHdr = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,12) +;
					lfEndOfGrp(@lnEndOfSty,Padr(&lcStyle..Style,lnMajorlen)),28) + Space(0) +;
					IIF(lcScalCode = &lcStyle..Scale,'',Iif(llRpScale,;
					PADL(Alltrim(&lcscal..Sz1),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz2),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz3),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz4),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz5),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz6),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz7),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz8),5),''))
*E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [End]
			Else
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
*!*	        lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
*!*	                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
				lcPrintHdr = Padr(Alltrim(Evaluate(lcStyGroup)) + ' ' + &lcStyle..Desc +;
					lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]

				For I = 1 To 8
					Z = Str(I,1)
					laScals[I] = Padl(Alltrim(&lcscal..Sz&Z),5)
				Endfor
			Endif

		Endif  && end of if not print sizes "lcPrintHdr" will not include scales

		lnMaxCnt   = &lcscal..Cnt
		=lfSumGroup("lnStySz","lnSize",.T.) && Calculate Style initial totals.
		lnNonPrcOp = lnNonPrice             && Calculate Total Price.
		lnNonCstOp = lnNonCost              && Calculate Total Cost.

		lcScalCode = &lcStyle..Scale

	Else  && it is the same style
		If llRpScale
			If &lcStyle..Scale <> lcScalCode
*-- if not print sizes "lcPrintHdr" will not include scales
				If !LLRPPRTSIZ
					lcPrintHdr = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,15) +;
						lfEndOfGrp(@lnEndOfSty,Padr(&lcStyle..Style,lnMajorlen)),35) + Space(3)
				Else
					If llTextMode

						lcPrintHdr = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,12) +;
							lfEndOfGrp(@lnEndOfSty,Padr(&lcStyle..Style,lnMajorlen)),28) + Space(0) +;
							PADL(Alltrim(&lcscal..Sz1),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz2),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz3),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz4),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz5),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz6),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz7),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz8),5)
						lcPrintHdr = Strtran(lcPrintHdr,Substr(lcPrintHdr,1,35),Space(35))
					Else
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
*!*	            lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
*!*	                         lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
						lcPrintHdr = Padr(Alltrim(Evaluate(lcStyGroup)) + ' ' + &lcStyle..Desc +;
							lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]

						For I = 1 To 8
							Z = Str(I,1)
							laScals[I] = Padl(Alltrim(&lcscal..Sz&Z),5)
						Endfor
					Endif

				Endif  && end of if not print sizes "lcPrintHdr" will not include scales
				lcScalCode = &lcStyle..Scale
			Else
				lcPrintHdr = ''
			Endif
		Endif

		lnMaxCnt = Max(lnMaxCnt,&lcscal..Cnt)

*-- if line have quantities then calculate style group totals.
		If lnSize1 <> 0 Or lnSize2 <> 0 Or lnSize3 <> 0 Or lnSize4 <> 0 Or ;
				lnSize5 <> 0 Or lnSize6 <> 0 Or lnSize7 <> 0 Or lnSize8 <> 0

*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*      IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
			If lnFrstRec1 = 0 Or lnFrstRec1 = Recno()
				lnFrstRec1 = Recno()
				Store 0 To lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
					lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,lnStySz10,lnStySz11,;
					lnNonPrcOp,lnNonCstOp
			Endif
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*      ENDIF
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

			=lfSumGroup("lnStySz","lnSize")       && Calculate Style totals.
			If lcMastFile = '&lcStyle' Or Empty(DYELOT)
				lnNonPrcOp = lnNonPrcOp + lnNonPrice  && Calculate Total Price.
				lnNonCstOp = lnNonCstOp + lnNonCost   && Calculate Total Cost.
			Endif
		Endif

	Endif   && end if it is new style.


*-- if line have quantities then calculate grand totals.
	If lnSize1 <> 0 Or lnSize2 <> 0 Or lnSize3 <> 0 Or lnSize4 <> 0 Or ;
			lnSize5 <> 0 Or lnSize6 <> 0 Or lnSize7 <> 0 Or lnSize8 <> 0
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*    IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
		If lnFrstRec = 0 Or lnFrstRec = Recno()
			lnFrstRec = Recno()
			Store 0 To lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
				lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
				lnNonPrcGd,lnNonCstGd
		Endif
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*   ENDIF
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

		=lfSumGroup("lnGrdSz","lnSize")      && Calculate Grand totals.
		If lcMastFile = '&lcStyle' Or Empty(DYELOT)
			lnNonPrcGd = lnNonPrcGd + lnNonPrice && Calculate Total Price.
			lnNonCstGd = lnNonCstGd + lnNonCost  && Calculate Total Cost.
		Endif
	Endif

*-- Evaluate all sizes values from style file Case Master file is StyDye
*-- and print location detail, or print style dyelots . [Begin]
	If lcMastFile = "&lcStyDye" And llPrintClr And   !(Right(Style,19 - lnMajorlen)==lcPrntNon)
		llPrnClrLn = .T.
		lcPrntNon  = Right(Style,19 - lnMajorlen)
		lnAllSizes = 0
		lnClrSz9   = 0
		lnClrSz10   = 0
		lnClrSz11   = 0
*!*B611899,1 Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][Begin]
		lnClrCost = 0
*!*B611899,1Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][End]
		For lnAllSizes = 1 To 8
			lcSize   = "lnClrSz"+Str(lnAllSizes,1)
			&lcSize  = Evaluate(laTrnNonAr[lnAllSizes])
			lnClrSz9 = lnClrSz9 + &lcSize
			If lcRPOTSSig $ 'BL'
				If &lcSize > 0
					lnClrSz10 = lnClrSz10 + &lcSize && variable hold sum of +ve OTS Qtys
				Else
					lnClrSz11 = lnClrSz11 + &lcSize && variable hold sum of -ve OTS Qtys
				Endif
			Endif
		Endfor
		lnClrPrice = lnClrSz9 * (&lcStyle..PriceA)

*! B609307,1 HES 06/19/2010 Fic bug of Style Summary report with Loc detail incorrect totals by colour [Start]
		If Alen(laRPRepTar,1) = 1 And llRPWhDeta
			lcOldAlis = Select(0)
			lnlocPos = Ascan(loogScroll.laOgFxFlt,"STYDYE.CWARECODE")
			lcFileLoc = ''
			llLocSelected  =.F.
			If lnlocPos > 0
				lnlocPos  = Asubscript(loogScroll.laOgFxFlt,lnlocPos ,1)
				If !Empty(loogScroll.laOgFxFlt[lnlocPos  ,6]) And Used(loogScroll.laOgFxFlt[lnlocPos  ,6])
					lcFileLoc = loogScroll.laOgFxFlt[lnlocPos  ,6]
					Select(lcFileLoc)
					Locate
					If !Eof()
						llLocSelected  = .T.
					Endif
				Endif
			Endif
			Select (lcStyDye)
			lcStyDyeKey=Evaluate(Key())
			For lnT =1 To 8
				lcT = Str(lnT,1)
				lnClrSz&lcT.  = 0
			Endfor
*!*B611899,1 Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][Begin]
			lnClrCost = 0
*!*B611899,1Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][End]

			lnClrSz9 = 0

			Select (lcStyDye)
			lcSty = &lcStyle..Style
			=Seek(lcSty)
			Scan Rest While Style+CWARECODE+DYELOT = lcSty For Empty(DYELOT) And Iif(llLocSelected,Seek(CWARECODE,lcFileLoc),.T.)
				For lnT =1 To 8
					lcT = Str(lnT,1)
					lnClrSz&lcT. = lnClrSz&lcT. +  Evaluate(laTranAray[lnT])
					lnClrSz9 = lnClrSz9 +  Evaluate(laTranAray[lnT])
				Endfor
*!*B611899,1 Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][Begin]
				If "SOH" $ lcRepTarVl And lcCstMeth<>"S"
					lnClrCost = lnClrCost +  nStkVal
				Endif
*!*B611899,1Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][End]

			Endscan
			Select (lcStyDye)
			=Seek(lcStyDyeKey)
			Select(lcOldAlis)
		Endif
*! B609307,1 HES 06/19/2010 Fic bug of Style Summary report with Loc detail incorrect totals by colour [End  ]

*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lnClrCost = IIF("SOH" $ lcRepTarVl , &lcStyle..nStkVal , IIF(lcCstMeth="S",;
&lcStyle..TotCost,&lcStyle..Ave_Cost)*lnClrSz9)
*ELSE
*B608407,1 WAM 01/14/2008 (End)

*!*B611899,1 Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][Begin]
*lnClrCost = Iif("SOH" $ lcRepTarVl , Iif(lcCstMeth="S", &lcStyle..TotCost * lnClrSz9 , &lcStyle..nStkVal ) , Iif(lcCstMeth="S",;
&lcStyle..TotCost,&lcStyle..Ave_Cost)*lnClrSz9)
		lnClrCost = Iif("SOH" $ lcRepTarVl , Iif(lcCstMeth="S", &lcStyle..TotCost * lnClrSz9 , lnClrCost) , Iif(lcCstMeth="S",;
			&lcStyle..TotCost,&lcStyle..Ave_Cost)*lnClrSz9)
*!*B611899,1 Heba 12/09/2019 - Aria 5 - Rebalance Issue [T20190905.0003][End]
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)

	Else
		llPrnClrLn = .F.
	Endif
*-- and print location detail, or print style dyelots . [End  ]

Endif     && end if user want to print zeros or total value not equal zero.

Return lcPrintHdr
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
Function lfSumGroup
Parameters lcSummedVr,lcLineVar,llPrimary
*-- if you loop style file or you loop location record in stydye file.
If lcMastFile = '&lcStyle' Or Empty(DYELOT)
	Private lnAllSizes,lcVar,lcVal
	lnAllSizes = 0
	For lnAllSizes = 1 To 9
		lcVar  = lcSummedVr + Str(lnAllSizes,1)
		lcVal  = lcLineVar + Str(lnAllSizes,1)
*-- if it is new group.
		If llPrimary
			&lcVar = &lcVal
		Else  && another line in the same group.
			&lcVar = &lcVar + &lcVal
		Endif
	Endfor
	If lcRPOTSSig $ 'BL'
		For lnAllSizes = 10 To 11
			lcVar  = lcSummedVr + Str(lnAllSizes,2)
			lcVal  = lcLineVar + Str(lnAllSizes,2)
*-- if it is new group.
			If llPrimary
				&lcVar = &lcVal
			Else  && another line in the same group.
				&lcVar = &lcVar + &lcVal
			Endif
		Endfor
	Endif
Endif  && end if you loop style file or you loop location record in stydye file.
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
Function lfNonMjDes
Local lnI , lcTemp

lcTemp = ''
lnI = 0


*-- Loop Around Non Major elements.
For lnI = lnMajSeg + 1 To Alen(laMajSeg,1)
	lcTemp = ''
	Do Case
*-- Free, Other, Make, or Quality Segment.
	Case laMajSeg[lnI,1] $ "FOTQ"
		If Seek(Str(lnI,1)+Substr(Style,laMajSeg[lnI,4],Len(laMajSeg[lnI,3])),lcSegVal)
			lcTemp = Alltrim(&lcSegVal..cISgValSd)
		Endif
*-- Season, Color, Division, or Style group Segment.
	Case laMajSeg[lnI,1] $ "ZCDG"
		Do Case
		Case laMajSeg[lnI,1] = "Z"
			lcCodeExpr = "SEASON"
		Case laMajSeg[lnI,1] = "C"
			lcCodeExpr = "COLOR"
		Case laMajSeg[lnI,1] = "D"
			lcCodeExpr = "CDIVISION"
		Otherwise
			lcCodeExpr = "CSTYGROUP"
		Endcase

		lcTemp = Alltrim(gfCodDes(Substr(Style,laMajSeg[lnI,4],Len(laMajSeg[lnI,3])),lcCodeExpr,.T.))

*-- Size Seqment case.
	Otherwise

		If Seek("S"+Substr(Style,laMajSeg[lnI,4],Len(laMajSeg[lnI,3])),lcscal)
			lcTemp = Alltrim(&lcscal..cScl_desc)
		Endif

	Endcase
	lcNonMjDes = Iif(Empty(lcNonMjDes),lcTemp,lcNonMjDes + Iif(Empty(lcTemp),'','-') + lcTemp)
Endfor    && end Loop Around Non Major elements.
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
Function lfEndOfGrp
Parameters lcVariable,lcEqualExp
Private lnCurAlis
*--T20070628.0010 AKM [Start]
*Screen.Visible = .T.
*SET STEP ON
*--T20070628.0010 AKM [End]

lnCurAlis = Select(0)
lcTagName = Iif(lcMastFile = '&lcStyle','RevStyle',Iif(lcRpSortBy == "S",'RevStyDy','RevStyDyW'))
Select (lcRevFile)
lcFilter = Set("Filter")
Set Filter To

Set Order To Tag lcTagName Descending

=Seek(lcEqualExp)

*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[Start]
*B608163,1TMI [Start] if one transaction and sort by Location - other sort
If Alen(laRPRepTar,1)=1 And lcRPFormNa $ 'ICSTYBYS|ICSTYBYL'
*- Get the correct expression to remove unneeded lines based on
	Local lcRmvExpr
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
	If  'OTS' $ laRPRepTar[1] And (lcRPOTSSig = 'A')
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[END]
		lcRmvExpr = laTranAray[1]+' <> 0 OR '+;
			laTranAray[2]+' <> 0 OR '+;
			laTranAray[3]+' <> 0 OR '+;
			laTranAray[4]+' <> 0 OR '+;
			laTranAray[5]+' <> 0 OR '+;
			laTranAray[6]+' <> 0 OR '+;
			laTranAray[7]+' <> 0 OR '+;
			laTranAray[8]
	Endif
*: B608711,2 MMT 10/28/2008 Fix bug of some Styles subtotal is not printed when use print BOTH OTS [End]

	lcKey = Key()
	If !llRPShwZer

*B612349,1 Es 03/22/2021  Sometimes the Style Summary report does not display the Style total line [T20210226.0002][Start]
*!*			Locate Rest While &lcKey = lcEqualExp ;
*!*				FOR &lcRmvExpr <> 0
		Locate Rest While &lcKey = lcEqualExp ;
			FOR &lcRmvExpr <> 0 AND IIF(!Empty(lcRpExp),EVALUATE(lcRpExp),.T.)
*B612349,1 Es 03/22/2021  Sometimes the Style Summary report does not display the Style total line [T20210226.0002][End]
	Endif
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*IF 'OTS' $ laRpRepTar[1] AND (lcRPOTSSig='P' OR lcRPOTSSig='N')
	If 'OTS' $ laRPRepTar[1] And (lcRPOTSSig $ 'LBPN')
		If (lcRPOTSSig='P' Or lcRPOTSSig='N')
			lcSgn = Iif(lcRPOTSSig='P', '>= lnRPOTSMin' , '<= lnRPOTSMax')
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[END]
			=Seek(lcEqualExp)
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*!*	    LOCATE REST WHILE &lcKey = lcEqualExp ;
*!*	                  FOR &lcRmvExpr &lcSgn lnRPOTSMin .AND. IIF(!llRPShwZer,&lcRmvExpr <> 0,.T.)
*B612349,1 Es 03/22/2021  Sometimes the Style Summary report does not display the Style total line [T20210226.0002][Start]
*!*				Locate Rest While &lcKey = lcEqualExp ;
*!*					FOR &lcRmvExpr &lcSgn  .And. Iif(!llRPShwZer,&lcRmvExpr <> 0,.T.)
			Locate Rest While &lcKey = lcEqualExp ;
				FOR &lcRmvExpr &lcSgn  .And. Iif(!llRPShwZer,&lcRmvExpr <> 0,.T.) AND IIF(!Empty(lcRpExp),EVALUATE(lcRpExp),.T.)
*B612349,1 Es 03/22/2021  Sometimes the Style Summary report does not display the Style total line [T20210226.0002][End]

		Else
			=Seek(lcEqualExp)
*! B611747, HMS 03/20/2019  - Issue with Style Summary OTS report [T20190318.0002][Begin]
*Locate Rest While &lcKey = lcEqualExp ;
FOR &lcRmvExpr >= lnRPOTSMin Or &lcRmvExpr <= lnRPOTSMax .And. Iif(!llRPShwZer,&lcRmvExpr <> 0,.T.)
*B612349,1 Es 03/22/2021  Sometimes the Style Summary report does not display the Style total line [T20210226.0002][Start]
*!*				Locate Rest While &lcKey = lcEqualExp ;
*!*					FOR (&lcRmvExpr >= lnRPOTSMin Or &lcRmvExpr <= lnRPOTSMax) .And. Iif(!llRPShwZer,&lcRmvExpr <> 0,.T.)
			Locate Rest While &lcKey = lcEqualExp ;
				FOR (&lcRmvExpr >= lnRPOTSMin Or &lcRmvExpr <= lnRPOTSMax) .And. Iif(!llRPShwZer,&lcRmvExpr <> 0,.T.);
				AND IIF(!Empty(lcRpExp),EVALUATE(lcRpExp),.T.)
*B612349,1 Es 03/22/2021  Sometimes the Style Summary report does not display the Style total line [T20210226.0002][End]

*! B611747, HMS 03/20/2019  - Issue with Style Summary OTS report [T20190318.0002][End]

		Endif
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
	Endif
Endif
*B608163,1TMI [End  ]
*: B608346,1 MMT 11/06/2007 Fix bug of error due to missing code done by Tarek[End]
lcVariable = Iif(Between(Recno(),1,Reccount()),Recno(),1)
Set Order To Tag lcTagName Ascending
Set Filter To &lcFilter
Select (lnCurAlis)

Return ''
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

Function lfLastItem

If NotScale = 'Y'
	lcLastItem = &lcDummy..Label
	lcLastSty  = StyCode
Endif

Return ''

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

Function lfLastSty

If NotScale = 'Y'
	lcLastSty = &lcTrns..StyCode
Endif

Return ''

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
Function lfLocHeadr
Private lcPrintHdr , lcSize , lnAllSizes
Store '' To lcNonMjDes , lcPrintHdr , lcLocBins , lcPrintSty
lnAllSizes = 0
=lfInitVals()  && Assign ititial values to all line variables.

*-- Evaluate all sizes values. [Begin]
For lnAllSizes = 1 To 8
	lcSize  = "lnSize"+Str(lnAllSizes,1)
	&lcSize = Evaluate(laTranAray[lnAllSizes])
	lnSize9 = lnSize9 + &lcSize
	If lcRPOTSSig $ 'BL'
		If &lcSize > 0
			lnSize10 = lnSize10+ &lcSize && variable hold sum of +ve OTS Qtys
		Else
			lnSize11 = lnSize11+ &lcSize && variable hold sum of -ve OTS Qtys
		Endif
	Endif
Endfor
*-- Evaluate all sizes values. [End  ]

*-- Zero values if OTS or IOTS and condition is true. [Begin]
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMin)))
If ("OTS" $ lcRepTarVl) And (((lcRPOTSSig ="P") And (lnSize9 < lnRPOTSMin)) Or ((lcRPOTSSig ="N") And (lnSize9 > lnRPOTSMax)) Or (lcRPOTSSig $ 'LB' And lnSize9 > lnRPOTSMax And lnSize9 < lnRPOTSMin))
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[END]
	Store 0 To lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
		lnSize6,lnSize7,lnSize8,lnSize9,lnSize10,lnSize11
Endif
*-- Zero values if OTS or IOTS and condition is true. [End  ]

*-- if user want to print zeros or total value not equal zero.
*: B608166,1 SSH 07/15/2007 Change the condition to be on size level not total level
*IF llRpShwZer OR lnSize9
If llRPShwZer Or lnSize1 <> 0 Or lnSize2 <> 0 Or lnSize3 <> 0 Or lnSize4 <> 0 Or lnSize5 <> 0 Or lnSize6<> 0 Or lnSize7 <> 0 Or lnSize8 <> 0 Or lnSize9 <> 0
*: B608166,1 SSH 07/15/2007 Change the condition to be on size level not total level
	lnNonPrice = lnSize9 * (&lcStyle..PriceA)
*-- if Location record.
	If Empty(DYELOT)

*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*IF llLinkGlJl
*  lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="A", Ave_Cost,&lcStyle..TotCost)*lnSize9)
*  lnCstAveVl = IIF("SOH" $ lcRepTarVl,nStkVal/lnSize9,IIF(lcCstMeth="A",Ave_Cost,&lcStyle..TotCost))
*ELSE
*B608407,1 WAM 01/14/2008 (End)

		lnNonCost  = Iif("SOH" $ lcRepTarVl , Iif(lcCstMeth = "S" , &lcStyle..TotCost *lnSize9 , nStkVal )       , Iif(lcCstMeth="A", Ave_Cost,&lcStyle..TotCost)*lnSize9)
		lnCstAveVl = Iif("SOH" $ lcRepTarVl , Iif(lcCstMeth = "S" , &lcStyle..TotCost          , nStkVal/lnSize9), Iif(lcCstMeth="A",Ave_Cost,&lcStyle..TotCost))
*B608407,1 WAM 01/14/2008 Print sandard cost when costing method is Standard
*ENDIF
*B608407,1 WAM 01/14/2008 (End)

		=lfNonMjDes()  && Evaluate Non Major Description.

	Else  && Dyelot Record value.
		If llRPPrnDye
			If lcCstMeth="A"
				= lfGetCosts(@lnCstAveVl)
			Else
				lnCstAveVl = &lcStyle..TotCost
			Endif
		Endif
		lnNonCost = lnSize9 * lnCstAveVl
	Endif

*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
	If lnFrstRec1 = 0 Or lnFrstRec1 = Recno()
		lnFrstRec1 = Recno()
		lcPrntLoc  = Space(6)
		lcPrntSty  = Space(lnMajorlen)
		lcScalCode = Space(3)
	Endif
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*ENDIF
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

*-- if it is new location.
	If !(CWARECODE==lcPrntLoc)
		lcPrntLoc = CWARECODE
		lcPrintHdr = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Location,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Location",AHEADERFILE))+" : " + CWARECODE +;
			lfEndOfGrp(@lnEndOfLoc,CWARECODE)

		=lfSumGroup("lnLocSz","lnSize",.T.) && Calculate Location initial totals.
		lnNonPrcLc = lnNonPrice             && Calculate Total Price.
		lnNonCstLc = lnNonCost              && Calculate Total Cost.

	Else  && it is the same location

		If lnSize1 <> 0 Or lnSize2 <> 0 Or lnSize3 <> 0 Or lnSize4 <> 0 Or ;
				lnSize5 <> 0 Or lnSize6 <> 0 Or lnSize7 <> 0 Or lnSize8 <> 0

*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*      IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
			If lnFrstRec1 = 0 Or lnFrstRec1 = Recno()
				lnFrstRec1 = Recno()
				Store 0 To lnLocSz1,lnLocSz2,lnLocSz3,lnLocSz4,;
					lnLocSz5,lnLocSz6,lnLocSz7,lnLocSz8,lnLocSz9,lnLocSz10,lnLocSz11,;
					lnNonPrcLc,lnNonCstLc
			Endif
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*      ENDIF
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

			=lfSumGroup("lnLocSz","lnSize")         && Calculate Style totals.
			If Empty(DYELOT)
				lnNonPrcLc = lnNonPrcLc + lnNonPrice  && Calculate Total Price.
				lnNonCstLc = lnNonCstLc + lnNonCost   && Calculate Total Cost.
			Endif
		Endif

	Endif   && end if it is new location.

*-- if it is new style.
	If !(CWARECODE + Padr(&lcStyle..Style,lnMajorlen)==lcPrntSty)
		lcPrntSty  = CWARECODE + Padr(&lcStyle..Style,lnMajorlen)
*-- if not print sizes "lcPrintHdr" will not include scales
		If !LLRPPRTSIZ
			lcPrintSty = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,15) +;
				lfEndOfGrp(@lnEndOfSty,lcPrntSty),35) + Space(3)
		Else
			If llTextMode
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
				lcPrintSty = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,12) +;
					lfEndOfGrp(@lnEndOfSty,lcPrntSty),29) + Space(1) +;
					IIF(lcScalCode = &lcStyle..Scale,'',Iif(!llRpScale,'',;
					PADL(Alltrim(&lcscal..Sz1),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz2),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz3),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz4),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz5),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz6),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz7),5) + Space(2) +;
					PADL(Alltrim(&lcscal..Sz8),5)))

*E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [End]
			Else
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
*!*	        lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
*!*	                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
				lcPrintSty = Padr(Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,15) +;
					lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]
				For I = 1 To 8
					Z = Str(I,1)
					laScals[I] = Padl(Alltrim(&lcscal..Sz&Z),5)
				Endfor
			Endif

		Endif     && end if not print sizes "lcPrintHdr" will not include scales
		lnMaxCnt   = &lcscal..Cnt
		=lfSumGroup("lnStySz","lnSize",.T.) && Calculate Style initial totals.
		lnNonPrcOp = lnNonPrice             && Calculate Total Price.
		lnNonCstOp = lnNonCost              && Calculate Total Cost.

		lcScalCode = &lcStyle..Scale

	Else  && it is the same style

		If llRpScale
			If &lcStyle..Scale <> lcScalCode
				If !LLRPPRTSIZ
					lcPrintHdr = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,15) +;
						lfEndOfGrp(@lnEndOfSty,Padr(&lcStyle..Style,lnMajorlen)),35) + Space(3)
				Else
					If llTextMode
						lcPrintHdr = Padr(Alltrim(lcMajTtl) + ' :' +Alltrim(Evaluate(lcStyGroup)) + ' ' + Padr(&lcStyle..Desc,12) +;
							lfEndOfGrp(@lnEndOfSty,lcPrntSty),29) + Space(1) +;
							PADL(Alltrim(&lcscal..Sz1),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz2),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz3),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz4),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz5),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz6),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz7),5) + Space(2) +;
							PADL(Alltrim(&lcscal..Sz8),5)
						lcPrintHdr = Strtran(lcPrintHdr,Substr(lcPrintHdr,1,35),Space(35))
					Else
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[Start]
*!*	            lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(&lcStyle..Desc,15) +;
*!*	                         lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
						lcPrintSty = Padr(Alltrim(Evaluate(lcStyGroup)) + ' ' + &lcStyle..Desc +;
							lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
*! B608976,1 MMT 08/24/2009 Fix bug of incomplete style short desc. in single transcation layout[End]
						For I = 1 To 8
							Z = Str(I,1)
							laScals[I] = Padl(Alltrim(&lcscal..Sz&Z),5)
						Endfor
					Endif
				Endif  && end of if not print sizes "lcPrintHdr" will not include scales
				lcScalCode = &lcStyle..Scale
			Else
				lcPrintHdr = ''
			Endif
		Endif

		lnMaxCnt = Max(lnMaxCnt,&lcscal..Cnt)
		If lnSize1 <> 0 Or lnSize2 <> 0 Or lnSize3 <> 0 Or lnSize4 <> 0 Or ;
				lnSize5 <> 0 Or lnSize6 <> 0 Or lnSize7 <> 0 Or lnSize8 <> 0

*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
			If lnFrstRec1 = 0 Or lnFrstRec1 = Recno()
				lnFrstRec1 = Recno()
				Store 0 To lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
					lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,lnStySz10,lnStySz11,;
					lnNonPrcOp,lnNonCstOp
			Endif
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*      ENDIF
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

			=lfSumGroup("lnStySz","lnSize")       && Calculate Style totals.
			If Empty(DYELOT)
				lnNonPrcOp = lnNonPrcOp + lnNonPrice  && Calculate Total Price.
				lnNonCstOp = lnNonCstOp + lnNonCost   && Calculate Total Cost.
			Endif
		Endif

	Endif   && end if it is new style.

	If lnSize1 <> 0 Or lnSize2 <> 0 Or lnSize3 <> 0 Or lnSize4 <> 0 Or ;
			lnSize5 <> 0 Or lnSize6 <> 0 Or lnSize7 <> 0 Or lnSize8 <> 0

*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*    IF oAriaApplication.gcDevice = LANG_ICSTYREP_Printer AND _PCOPIES > 1
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]
		If lnFrstRec = 0 Or lnFrstRec = Recno()
			lnFrstRec = Recno()
			Store 0 To lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
				lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
				lnNonPrcGd,lnNonCstGd
		Endif
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[Start]
*    ENDIF
*B607896,1 MMT 12/21/2006 - Style Summary report, Style total duplicated when[End]

		=lfSumGroup("lnGrdSz","lnSize")      && Calculate Grand totals.
		If Empty(DYELOT)
			lnNonPrcGd = lnNonPrcGd + lnNonPrice && Calculate Total Price.
			lnNonCstGd = lnNonCstGd + lnNonCost  && Calculate Total Cost.
		Endif
	Endif
Endif     && end if user want to print zeros or total value not equal zero.
*-- not equal zero. [End  ]
Return lcPrintHdr
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

Function lfLstToItm

If NotScale = 'Y'
	lcLstToItm = &lcDummy..Item
Endif

Return ''

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

Function lfPrnLoc

If NotScale = 'Y'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*    IF (llRPWhDeta OR lcRpSortBy == 'W') AND llRPPrnLoc AND ;
*!*     EVAL(lcDummy+'.Label') <> LANG_ICSTYREP_Dyelot AND EMPTY(&lcStyDye..Dyelot)
	If (llRPWhDeta Or lcRpSortBy == 'W') And llRPPrnLoc And ;
			EVAL(lcDummy+'.Label') <> Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE)) And Empty(&lcStyDye..DYELOT)
*N000682,1 11/20/2012 MMT Globlization changes[End]

		If Alltrim(&lcDummy..Item) $ 'IOTS'
			Do Case
			Case lcRPOTSSig ='P'
				lcRet = Iif(!llRPPrnLoc,'',;
					IIF(Alltrim(&lcDummy..Item) $ 'IOTS',;
					IIF(Eval(lcStyTmp+'.Tot'+Alltrim(&lcDummy..Item))>=lnRPOTSMin,&lcTrns..Loc,'');
					,&lcTrns..Loc))
			Case lcRPOTSSig ='N'
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*!*	          lcRet = IIF(!llRPWhDeta,'',;
*!*	                      IIF(ALLTRIM(&lcDummy..Item) $ 'IOTS',;
*!*	                          IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..Item))<=lnRPOTSMin,&lcTrns..Loc,'');
*!*	                              ,&lcTrns..Loc))
				lcRet = Iif(!llRPWhDeta,'',;
					IIF(Alltrim(&lcDummy..Item) $ 'IOTS',;
					IIF(Eval(lcStyTmp+'.Tot'+Alltrim(&lcDummy..Item))<=lnRPOTSMax,&lcTrns..Loc,'');
					,&lcTrns..Loc))

			Case lcRPOTSSig $ 'LB'
				lcRet = Iif(!llRPWhDeta,'',;
					IIF(Alltrim(&lcDummy..Item) $ 'IOTS',;
					IIF(Eval(lcStyTmp+'.Tot'+Alltrim(&lcDummy..Item))<=lnRPOTSMax Or Iif(Eval(lcStyTmp+'.Tot'+Alltrim(&lcDummy..Item))>=lnRPOTSMin,;
					&lcTrns..Loc,'');
					,&lcTrns..Loc))
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[End]
			Endcase
		Else
			lcRet = &lcTrns..Loc
		Endif

	Else
		lcRet = ''
	Endif

Else
	lcRet = ''
Endif

Return Alltrim(lcRet)

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

Function lfPrnStyItm

Parameters lcFld



Private lcFld,lnRet,lcUseFile


lcUseFile = Iif(lcRpSortBy == "W",lcTrns,lcStyTmp)
lnRet = ''
If NotScale = 'Y'
	If &lcscal..Cnt >= Val(lcFld) Or !Empty(&lcUseFile..cEndMaj+&lcUseFile..cEndSort+&lcUseFile..CENDREP)
		If !Empty(&lcDummy..Item)
			If (lcRpSortBy == "W" And &lcDummy..Label <> Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Dyelot,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Dyelot",AHEADERFILE)) And Empty(&lcUseFile..DYELOT)) Or ;
					lcRpSortBy <> "W"
				If lcFld = 'ItmLabel'
					If !(Alltrim(&lcDummy..Item) $ 'IOTS')
						lnRet = &lcDummy..Label
					Else
						If (Alltrim(&lcDummy..Item) = 'IOTS' And lPrnIOTS) Or ;
								(Alltrim(&lcDummy..Item) = 'OTS' And lPrnOTS)   Or ;
								!Empty(&lcUseFile..cEndMaj)
							lnRet = &lcDummy..Label
						Else
							lnRet = ''
						Endif
					Endif
				Else
					If lcFld = 'WhsLabel'
						If llRPWhDeta And Empty(cEndMaj)
							If !(Alltrim(&lcDummy..Item) $ 'IOTS')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnRet = LANG_ICSTYREP_Location+' '
								lnRet = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Location,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Location",AHEADERFILE))+' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

							Else
								If (Alltrim(&lcDummy..Item) = 'IOTS' And lPrnIOTS) Or ;
										(Alltrim(&lcDummy..Item) = 'OTS' And lPrnOTS)   Or ;
										!Empty(&lcUseFile..cEndMaj)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnRet = LANG_ICSTYREP_Location+' '
									lnRet = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Location,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Location",AHEADERFILE))+' '
*N000682,1 11/20/2012 MMT Globlization changes[End]

								Else
									lnRet = ''
								Endif
							Endif
						Else
							lnRet = ''
						Endif
					Else
						If !Inlist(lcFld,'Price','Cost')
							lnRet = Eval(lcUseFile+"."+Iif(lcFld='Tot','Tot'+Alltrim(&lcDummy..Item),;
								ALLTRIM(&lcDummy..Item)+lcFld))

							If Alltrim(&lcDummy..Item) $ 'IOTS' And Empty(cEndMaj)
								If !( (Alltrim(&lcDummy..Item) = 'IOTS' And lPrnIOTS) Or ;
										(Alltrim(&lcDummy..Item) = 'OTS' And lPrnOTS) )

									lnRet = ''
								Else
									If Between(lcFld,"1","8")
										Do Case
										Case lcRPOTSSig = 'P' And lnRet <= 0

*B611640 ,1 HMS 08/06/2018, Aria 5 - ESTEE LAUDER STYLE & SUMMARY REPORT [T20180718.0015 ][Begin]
* lnRet = ''
											lnRet = 0
*B611640 ,1 HMS 08/06/2018, Aria 5 - ESTEE LAUDER STYLE & SUMMARY REPORT [T20180718.0015 ][End]
										Case lcRPOTSSig = 'N' And lnRet >= 0
*B611640 ,1 HMS , Aria 5 - ESTEE LAUDER STYLE & SUMMARY REPORT [T20180718.0015 ][Begin]
*lnRet = ''
											lnRet = 0
*B611640 ,1 HMS , Aria 5 - ESTEE LAUDER STYLE & SUMMARY REPORT [T20180718.0015 ][End]
										Endcase
									Endif
								Endif
							Endif
						Else
							If lcFld = 'Cost'
								If !Empty(cEndMaj) Or ;
										!( (Alltrim(&lcDummy..Item) = 'IOTS' And !lPrnIOTS) Or ;
										(Alltrim(&lcDummy..Item) = 'OTS'  And !lPrnOTS ) )
									lnRet = Eval('nStkV'+Alltrim(&lcDummy..Item))
								Else
									lnRet = ''
								Endif
							Endif
							If lcFld = 'Price'
								If !Empty(cEndMaj) Or ;
										!( (Alltrim(&lcDummy..Item) = 'IOTS' And !lPrnIOTS) Or ;
										(Alltrim(&lcDummy..Item) = 'OTS'  And !lPrnOTS ) )

									lnRet = Eval('nSalV'+Alltrim(&lcDummy..Item))
								Else
									lnRet = ''
								Endif
							Endif
						Endif
					Endif
				Endif
			Endif
		Endif
	Endif
Endif
Return lnRet
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
Function lfPrntBin
If (lcRpSortBy == "L" Or llRPPrnLoc) And Empty(DYELOT) And (llRPShwZer Or lnSize9 <> 0)
	Private lnCurrAls , lcCurrOrd
	lnCurrAls = Select(0)
	Select (lcWhsLoc)
	lcCurrOrd = Order()
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*Set Order To Tag LCWHSLOCST
	Set Order To Tag (LCWHSLOCST)
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	If Seek(&lcStyDye..Style+Space(6)+&lcStyDye..CWARECODE)
		Private lnBinNum
		lnBinNum = 0
		Scan Rest While Style       +Color   +CWARECODE       +cLocation = ;
				&lcStyDye..Style+Space(6)+&lcStyDye..CWARECODE
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcLocBins = IIF(EMPTY(lcLocBins),LANG_ICSTYREP_Bin+' : ',lcLocBins+', ') + ALLTRIM(cLocation)
			lcLocBins = Iif(Empty(lcLocBins),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Bin,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Bin",AHEADERFILE))+' : ',lcLocBins+', ') + Alltrim(cLocation)
*N000682,1 11/20/2012 MMT Globlization changes[End]

			lnBinNum = lnBinNum + 1
		Endscan
		If lnBinNum > 1
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcLocBins = STRTRAN(lcLocBins,LANG_ICSTYREP_Bin+" :",LANG_ICSTYREP_Bin+" :")
			lcLocBins = Strtran(lcLocBins,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Bin,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Bin",AHEADERFILE))+" :",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Bin,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Bin",AHEADERFILE))+" :")
*N000682,1 11/20/2012 MMT Globlization changes[End]

		Endif
	Endif
	Set Order To &lcCurrOrd
	Select (lnCurrAls)
Endif
Return ''
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

Function lfPrnWhsItm
Parameters lcFld

Private lcFld,lnRet


lnRet = ''

If NotScale = 'Y'
	If llRPWhDeta And Empty(&lcStyDye..DYELOT) And Empty(&lcStyTmp..cEndMaj) And !Empty(&lcDummy..Item)
		If lcFld = 'WhsCode'
			If !(Alltrim(&lcDummy..Item) $ 'IOTS')
				lnRet = &lcTrns..WareCode
			Else
				If (Alltrim(&lcDummy..Item) = 'IOTS' And &lcTrns..lPrnIOTS) Or ;
						(Alltrim(&lcDummy..Item) = 'OTS'  And &lcTrns..lPrnOTS)
					lnRet = &lcTrns..WareCode
				Else
					lnRet = ''
				Endif
			Endif
		Else
			If !Inlist(lcFld,'Price','Cost')
				lnRet = Eval(lcTrns+'.'+Iif(lcFld='Tot','Tot'+Alltrim(&lcDummy..Item),;
					ALLTRIM(&lcDummy..Item)+lcFld))

				If Alltrim(&lcDummy..Item) $ 'IOTS'
					If !( (Alltrim(&lcDummy..Item) = 'IOTS' And &lcTrns..lPrnIOTS) Or ;
							(Alltrim(&lcDummy..Item) = 'OTS'  And &lcTrns..lPrnOTS) )
						lnRet = ''
					Else
						If Between(lcFld,"1","8")
							Do Case
							Case lcRPOTSSig = 'P' And lnRet <= 0
								lnRet = ''
							Case lcRPOTSSig = 'N' And lnRet >= 0
								lnRet = ''
							Endcase
						Endif
					Endif
				Endif
			Else
				If lcFld = 'Cost'
					If !Empty(&lcStyTmp..cEndMaj) Or ;
							!( (Alltrim(&lcDummy..Item) = 'IOTS' And !&lcTrns..lPrnIOTS) Or ;
							(Alltrim(&lcDummy..Item) = 'OTS'  And !&lcTrns..lPrnOTS ) )
						lnRet = Eval(lcTrns+'.nStkV'+Alltrim(&lcDummy..Item))
					Else
						lnRet = ''
					Endif
				Endif
				If lcFld = 'Price'
					If !Empty(&lcStyTmp..cEndMaj) Or ;
							!( (Alltrim(&lcDummy..Item) = 'IOTS' And !&lcTrns..lPrnIOTS) Or ;
							(Alltrim(&lcDummy..Item) = 'OTS'  And !&lcTrns..lPrnOTS ) )
						lnRet = Eval(lcTrns+'.nSalV'+Alltrim(&lcDummy..Item))
					Else
						lnRet = ''
					Endif
				Endif
			Endif
		Endif
	Else
		lnRet = ''
	Endif
Endif

Return lnRet


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
Function lfRstGrdVr

Store 0 To lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
	lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnGrdSz10,lnGrdSz11,;
	lnNonPrcGd,lnNonCstGd

Return ''
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
Function lfTotTtl



Private lcRet

If lcRpSortBy == 'W'
	If NotScale = 'N'
		lcRet = ''
	Else
		If Empty(cEndMaj)
			If lcLastSty=Eval(lcTrns+'.StyCode')
				lcRet = ''
			Else
				lcRet = lfPrnLoc()
			Endif
		Else
			If Eval(lcDummy+'.cRecNo')<>'01' Or Empty(Eval(lcDummy+'.Item'))
				lcRet = ''
			Else
				If Empty(cEndSort)
					If Empty(Eval(lcStyDye+'.Dyelot'))

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+' : '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+IIF(!EMPTY(EVAL(lcTrns+".cStyMajor")),' for : ',':')+EVAL(lcTrns+".cStyMajor")
						lcRet = lcMajTtl+' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Total,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Total",AHEADERFILE))+Iif(!Empty(Eval(lcTrns+".cStyMajor")),' for : ',':')+Eval(lcTrns+".cStyMajor")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

					Else
						lcRet = ''
					Endif
				Else
					If Empty(CENDREP)
						If Empty(Eval(lcStyDye+'.Dyelot'))

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcRet = LANG_ICSTYREP_Warehouse_Total+' : '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = LANG_ICSTYREP_Warehouse_Total+' for : '+ EVAL(lcTrns+'.WareCode')
							lcRet = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Warehouse_Total,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Warehouse_Total",AHEADERFILE))+' for : '+ Eval(lcTrns+'.WareCode')
*N000682,1 11/20/2012 MMT Globlization changes[End]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

						Else
							lcRet =''
						Endif
					Else
						If Empty(Eval(lcStyDye+'.Dyelot'))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = LANG_ICSTYREP_Grand_Total+' : '
							lcRet = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Grand_Total,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Grand_Total",AHEADERFILE))+' : '
*N000682,1 11/20/2012 MMT Globlization changes[End]

						Else
							lcRet =''
						Endif
					Endif
				Endif
			Endif
		Endif
	Endif

Else
	If NotScale = 'N'
		lcRet = ''
	Else
		If Empty(cEndMaj) Or Empty(Eval(lcDummy+'.Item')) Or Eval(lcDummy+'.Label')=lcLastItem
			lcRet = ''
		Else
			If Eval(lcDummy+'.cRecNo')<>'01'
				lcRet = ''
			Else
				If Empty(cEndSort)
					If Empty(Eval(lcStyDye+'.Dyelot'))

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+' : '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = lcMajTtl+' '+LANG_ICSTYREP_Total+IIF(!EMPTY(EVAL(lcStyTmp+".cStyMajor")),' for : ',':')+EVAL(lcStyTmp+".cStyMajor")
						lcRet = lcMajTtl+' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Total,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Total",AHEADERFILE))+Iif(!Empty(Eval(lcStyTmp+".cStyMajor")),' for : ',':')+Eval(lcStyTmp+".cStyMajor")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

					Else
						lcRet = ''
					Endif
				Else
					If Empty(CENDREP)
						If Empty(Eval(lcStyDye+'.Dyelot'))

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*lcRet = lcSortTtl + ' '+LANG_ICSTYREP_Total+' : '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = lcSortTtl + ' '+LANG_ICSTYREP_Total+IIF(!EMPTY(EVAL(lcStyTmp+lcSortFld)),' for : ',':')+EVAL(lcStyTmp+lcSortFld)
							lcRet = lcSortTtl + ' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Total,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Total",AHEADERFILE))+Iif(!Empty(Eval(lcStyTmp+lcSortFld)),' for : ',':')+Eval(lcStyTmp+lcSortFld)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]

						Else
							lcRet =''
						Endif
					Else
						If Empty(Eval(lcStyDye+'.Dyelot'))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = LANG_ICSTYREP_Grand_Total+' : '
							lcRet = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Grand_Total,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Grand_Total",AHEADERFILE))+' : '
*N000682,1 11/20/2012 MMT Globlization changes[End]

						Else
							lcRet =''
						Endif
					Endif
				Endif
			Endif
		Endif
	Endif
Endif

Return lcRet

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

Function lfScale
Private  lcRet

If NotScale = 'N' And Iif(lcRpSortBy == 'W' ,  &lcTrns..Scale #  lcScale1 , &lcStyTmp..Scale #  lcScale1 )
*E124814,1 HMA 06/04/2005,Show scale only if print size scale=Yes [Begin]
	If llRpScale
		If llTextMode
			lcRet = Space(15) + ;
				PADL(Alltrim(&lcscal..Sz1),8) + Space(1) + Padl(Alltrim(&lcscal..Sz2),8) + Space(1) +;
				PADL(Alltrim(&lcscal..Sz3),8) + Space(1) + Padl(Alltrim(&lcscal..Sz4),8) + Space(1) +;
				PADL(Alltrim(&lcscal..Sz5),8) + Space(1) + Padl(Alltrim(&lcscal..Sz6),8) + Space(1) +;
				PADL(Alltrim(&lcscal..Sz7),8) + Space(1) + Padl(Alltrim(&lcscal..Sz8),8) + Space(1) +;
				PADL(Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Total,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Total",AHEADERFILE)),8)
			If llShowCost
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Cost_Val
				lcRet = lcRet + Space(1) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Val,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Val",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

			Endif
			If llShowSale
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Sales_Val
				lcRet = lcRet + Space(1) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Sales_Val,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Sales_Val",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

			Endif
		Else
*-- we will let it old the scale.

			lcRet=''

*B608096,1 MMT 05/24/2007 fix bug of not displaying the Scale [Start]
			lcRet = Space(15) + ;
				PADL(Alltrim(&lcscal..Sz1),8) + Space(1) + Padl(Alltrim(&lcscal..Sz2),8) + Space(1) +;
				PADL(Alltrim(&lcscal..Sz3),8) + Space(1) + Padl(Alltrim(&lcscal..Sz4),8) + Space(1) +;
				PADL(Alltrim(&lcscal..Sz5),8) + Space(1) + Padl(Alltrim(&lcscal..Sz6),8) + Space(1) +;
				PADL(Alltrim(&lcscal..Sz7),8) + Space(1) + Padl(Alltrim(&lcscal..Sz8),8) + Space(1) +;
				PADL("Total",8)


*B608096,1 MMT 05/24/2007 fix bug of not displaying the Scale [End]
			For I = 1 To 8
				Z = Str(I,1)
				laScalWs[I] = Padl(Alltrim(&lcscal..Sz&Z),5)
			Endfor
			For I = 1 To 8
				Z = Str(I,1)
				laScals[I] = Padl(Alltrim(&lcscal..Sz&Z),5)
			Endfor

		Endif

		lcScale1 = &lcStyle..Scale

	Else &&if don't print Size Scale
		If llTextMode
			lcRet = Space(95)
			If llShowCost
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Cost_Val
				lcRet = lcRet + Space(1) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Cost_Val,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Cost_Val",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

			Endif
			If llShowSale
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRet = lcRet + SPACE(1) + LANG_ICSTYREP_Sales_Val
				lcRet = lcRet + Space(1) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Sales_Val,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Sales_Val",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

			Endif
		Else
*-- we will let it old the scale.
			lcRet=''
		Endif
		lcScale1 = &lcStyle..Scale
	Endif
Else
	lcRet = ''
Endif
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
Return lcRet


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
Function lfvSalVal
If llRpSalVal
	lcRPShow = "S"
Else
	lcRPShow = "N"
Endif
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
Function lfFillDumy

*Positive|Negative|Both|Net|All~P|N|B|A|L
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lcRpSortBy $ 'SW' AND ALEN(laRPRepTar,1) = 1 AND (ASCAN(laRPRepTar,LANG_ICSTYREP_OTS) > 0  .OR.  ASCAN(laRPRepTar,LANG_ICSTYREP_Imm_OTS) > 0)
If lcRpSortBy $ 'SW' And Alen(laRPRepTar,1) = 1 And (Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_OTS",AHEADERFILE))) > 0  .Or.  Ascan(laRPRepTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Imm_OTS,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Imm_OTS",AHEADERFILE))) > 0)
*N000682,1 11/20/2012 MMT Globlization changes[End]
	Dimension laOTSDesc[5,1],laOTSTarget[5,1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laOTSDesc[1] = LANG_ICSTYREP_Positive
*laOTSDesc[2] = LANG_ICSTYREP_Negative
*laOTSDesc[3] = LANG_ICSTYREP_Both
*laOTSDesc[4] = LANG_ICSTYREP_Net
*laOTSDesc[5] = LANG_ICSTYREP_All
	laOTSDesc[1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Positive,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Positive",AHEADERFILE))
	laOTSDesc[2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Negative,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Negative",AHEADERFILE))
	laOTSDesc[3] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Both,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Both",AHEADERFILE))
	laOTSDesc[4] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Net,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Net",AHEADERFILE))
	laOTSDesc[5] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_All,oAriaApplication.GetHeaderText("LANG_ICSTYREP_All",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


	laOTSTarget[1] = 'P'
	laOTSTarget[2] = 'N'
	laOTSTarget[3] = 'B'
	laOTSTarget[4] = 'A'
	laOTSTarget[5] = 'L'
Else
	Dimension laOTSDesc[3,1],laOTSTarget[3,1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laOTSDesc[1] = LANG_ICSTYREP_Positive
*laOTSDesc[2] = LANG_ICSTYREP_Negative
*laOTSDesc[3] = LANG_ICSTYREP_Net
	laOTSDesc[1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Positive,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Positive",AHEADERFILE))
	laOTSDesc[2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Negative,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Negative",AHEADERFILE))
	laOTSDesc[3] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Net,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Net",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]
	laOTSTarget[1] = 'P'
	laOTSTarget[2] = 'N'
	laOTSTarget[3] = 'A'
	If lcRPOTSSig $'BL'
		lcRPOTSSig='A'
	Endif
Endif


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
Function lfPrtConfigTtl

lcPrtConfigTtl =""
If Empty(cEndMaj) And llRpConfig And !Empty(Eval(lcDummy+".Label")) And !Empty(Eval(lcStyDye+".Dyelot")) And Eval(lcTrns+".HasDye") And !llPrtConfigTtl
	llPrtConfigTtl=.T.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*RETURN LANG_ICSTYREP_Configuration
	Return Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ICSTYREP_Configuration,oAriaApplication.GetHeaderText("LANG_ICSTYREP_Configuration",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

Else
	llPrtConfigTtl=.F.
	Return ''
Endif



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

Function lfPrnDyeItm
Parameters lcFld

Private lcFld,lnRet,lcUseFile,lcStyDyeVal,lcStyDyeTot,lcStyDyeVal1,lcStyDyeTot1,lcStyDyeVal2,lcStyDyeTot2
Store '' To lcStyDyeVal,lcStyDyeTot,lcStyDyeVal1,lcStyDyeTot1,lcStyDyeVal2,lcStyDyeTot2
Do Case
*------------------------------------- Direct Cases ----------------------
*-- Wip Case...
Case Alltrim(&lcDummy..Item) == 'WIP'
	lcStyDyeVal="WIP"
	lcStyDyeTot="TOTWIP"

*-- Stock on hand Case...
Case Alltrim(&lcDummy..Item) == 'SOH'
	lcStyDyeVal="STK"
	lcStyDyeTot="TOTSTK"

*-- Order Case...
Case Alltrim(&lcDummy..Item) == 'ORD'
	lcStyDyeVal="ORD"
	lcStyDyeTot="TOTORD"

*-- Work order Case...
Case Alltrim(&lcDummy..Item) == 'WORD'
	lcStyDyeVal="NWO"
	lcStyDyeTot="NTOTWO"

*-- Intransit Case...
Case Alltrim(&lcDummy..Item) == 'INT'
	lcStyDyeVal="INTRANS"
	lcStyDyeTot="TOTINTRN"

*-- Shipped Case...
Case Alltrim(&lcDummy..Item) == 'SHP'
	lcStyDyeVal="SHP"
	lcStyDyeTot="TOTSHP"

*-- Credit memo return Case...
Case Alltrim(&lcDummy..Item) == 'RET'
	lcStyDyeVal="RET"
	lcStyDyeTot="TOTRET"

*-- Return Authorization Case...
Case Alltrim(&lcDummy..Item) == 'RETA'
	lcStyDyeVal="RA"
	lcStyDyeTot="TOTRA"

*-- Allocation Case...
Case Alltrim(&lcDummy..Item) == 'ALO'
	lcStyDyeVal="ALO"
	lcStyDyeTot="TOTALO"

*------------------------------------- InDirect Cases ----------------------

*-- UnAllocation Case...
Case Alltrim(&lcDummy..Item) == 'UALO'
	lcStyDyeVal1="STK"
	lcStyDyeTot1="TOTSTK"
	lcStyDyeVal2="ALO"
	lcStyDyeTot2="TOTALO"

*-- Imm. OTS  Case...
Case Alltrim(&lcDummy..Item) == 'IOTS'
	lcStyDyeVal1="STK"
	lcStyDyeTot1="TOTSTK"
	lcStyDyeVal2="ORD"
	lcStyDyeTot2="TOTORD"

*-- Booked  Case...
Case Alltrim(&lcDummy..Item) == 'BOK'
	lcStyDyeVal1="SHP"
	lcStyDyeTot1="TOTSHP"
	lcStyDyeVal2="ORD"
	lcStyDyeTot2="TOTORD"

*-- OTS  Case...
Case Alltrim(&lcDummy..Item) == 'OTS'
	lcStyDyeVal="STK"
	lcStyDyeTot="TOTSTK"
	lcStyDyeVal1="ORD"
	lcStyDyeTot1="TOTORD"
	lcStyDyeVal2="WIP"
	lcStyDyeTot2="TOTWIP"

Endcase

lcUseFile = Iif(lcRpSortBy == "W",lcTrns,lcStyTmp)
lnRet = ''
If NotScale = 'Y'
	If &lcscal..Cnt >= Val(lcFld) Or !Empty(&lcUseFile..cEndMaj+&lcUseFile..cEndSort+&lcUseFile..CENDREP)
		If !Empty(&lcDummy..Item)
			If !Empty(&lcStyDye..DYELOT)  && all cases for sorting
				If !Inlist(lcFld,'Price','Cost')
					If !Inlist(Alltrim(&lcDummy..Item),'OTS','IOTS','BOK','UALO')
						lnRet = Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot,lcStyDyeVal+lcFld))
					Else
						Do Case
						Case Alltrim(&lcDummy..Item) == 'OTS'
							lnRet = Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot,lcStyDyeVal+lcFld))- Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot1,lcStyDyeVal1+lcFld)) + Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot2,lcStyDyeVal2+lcFld))
						Case Alltrim(&lcDummy..Item) == 'BOK'
							lnRet = Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot1,lcStyDyeVal1+lcFld))+ Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot2,lcStyDyeVal2+lcFld))
						Case Inlist(Alltrim(&lcDummy..Item),'UALO','IOTS')
							lnRet = Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot1,lcStyDyeVal1+lcFld))- Eval(lcStyDye+"."+Iif(lcFld='Tot',lcStyDyeTot2,lcStyDyeVal2+lcFld))
						Endcase
						If Alltrim(&lcDummy..Item) $ 'IOTS' And Empty(cEndMaj)
							If !( (Alltrim(&lcDummy..Item) = 'IOTS' And lPrnIOTS) Or ;
									(Alltrim(&lcDummy..Item) = 'OTS' And lPrnOTS) )
								lnRet = ''
							Else
								If Between(lcFld,"1","8")
									Do Case
									Case lcRPOTSSig = 'P' And lnRet <= 0
										lnRet = ''
									Case lcRPOTSSig = 'N' And lnRet >= 0
										lnRet = ''
									Endcase
								Endif
							Endif
						Endif
					Endif
				Else
					If lcFld = 'Price'
						If Empty(cEndMaj) And !Empty(Eval(lcStyDye+'.Dyelot'))
							If !Inlist(Alltrim(&lcDummy..Item),'OTS','IOTS','UALO')
								lnRet = Eval(lcStyDye+'.'+lcStyDyeTot)*Eval(lcStyle+'.PriceA')
							Else
								Do Case
								Case Alltrim(&lcDummy..Item) == 'OTS'
									lnRet = (Eval(lcStyDye+"."+lcStyDyeTot)- Eval(lcStyDye+"."+lcStyDyeTot1) + Eval(lcStyDye+"."+lcStyDyeTot2))* Eval(lcStyle+'.PriceA')
								Case Inlist(Alltrim(&lcDummy..Item),'UALO','IOTS')
									lnRet = (Eval(lcStyDye+"."+lcStyDyeTot1)- Eval(lcStyDye+"."+lcStyDyeTot2))* Eval(lcStyle+'.PriceA')
								Endcase
								If Alltrim(&lcDummy..Item) $ 'IOTS' And Empty(cEndMaj)
									If !( (Alltrim(&lcDummy..Item) = 'IOTS' And lPrnIOTS) Or ;
											(Alltrim(&lcDummy..Item) = 'OTS' And lPrnOTS) )
										lnRet = ''
									Endif
								Endif
							Endif
						Else
							lnRet = ''
						Endif
					Endif
				Endif
			Endif
		Endif
	Endif
Endif

Return lnRet


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

Function lfBldSqlCur
Parameters lcFilter,lcFound,lcCursor,lcFldName,lcSntFld
Local   lnPosition,lnFltPos,lnRow,lcExpression
Store 0 To lnPosition,lnFltPos,lnRow
Store '' To lcExpression
lnFltPos = Ascan(loogScroll.laOgFxFlt,lcFilter)
If lnFltPos > 0
	lnRow = Asubscript(loogScroll.laOgFxFlt,lnFltPos,1)
	lcTmpCur = loogScroll.laOgFxFlt[lnRow,6]
	If !Empty(lcTmpCur)  &&user selected some styles.
		Select &lcTmpCur
		&lcFound = (Reccount() > 0)
		If &lcFound
			&lcCursor = loogScroll.gfSQLTempName('',lcFldName,lcTmpCur,lcSntFld) && SQL Temp File
			If Empty(&lcCursor)
*-- SQL connection Error. Can't open The Report
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
				IF TYPE('lcXMLFileName') <> 'C'
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

					=gfModalGen('TRM00416B40011','ALERT')
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][Start
				ENDIF
*E612104,1 Es 04/26/2020 Modify the Style Summary report to filter data based on a new option "Show Only styles below minimum inventory level" and work from the Request builder screen [P20200316.0001][End]

				Return .F.
			Endif
		Endif
	Endif
Endif
Return &lcCursor

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
Function lfZeroStok
Private lnOldAls,ll2Return,lnInd,lcInd,lnValue

lnOldAls = Select(0)
ll2Return=.T.
lnValue=0
Select(lcTrns)
For lnInd=1 To 8
	lcInd = Alltrim(Str(lnInd))
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*!*	    lnValue=lnValue+Eval(lcTrns+".Wip"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".SOH"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".PLA"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".OTS"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".IOTS"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".BOK"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".SHP"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".Ret"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".RetA"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".Alo"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".UAlo"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".Int"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".WOrd"+lcInd)
*!*	    lnValue=lnValue+Eval(lcTrns+".Ord"+lcInd)
	lnValue=lnValue+Iif(llWipSel,Eval(lcTrns+".Wip"+lcInd),0)
	lnValue=lnValue+Iif(llStockSel,Eval(lcTrns+".SOH"+lcInd),0)
	lnValue=lnValue+Iif(llPlanSel,Eval(lcTrns+".PLA"+lcInd),0)
	lnValue=lnValue+Iif(llOTSSel ,Eval(lcTrns+".OTS"+lcInd),0)
	lnValue=lnValue+Iif(llImmOTSSel,Eval(lcTrns+".IOTS"+lcInd),0)
	lnValue=lnValue+Iif(llBookSel,Eval(lcTrns+".BOK"+lcInd),0)
	lnValue=lnValue+Iif(llShipSel,Eval(lcTrns+".SHP"+lcInd),0)
	lnValue=lnValue+Iif(llReturnSel,Eval(lcTrns+".Ret"+lcInd),0)
	lnValue=lnValue+Iif(llRetAuthSel,Eval(lcTrns+".RetA"+lcInd),0)
	lnValue=lnValue+Iif(llAllocSel,Eval(lcTrns+".Alo"+lcInd),0)
	lnValue=lnValue+Iif(llUnAllocSel,Eval(lcTrns+".UAlo"+lcInd),0)
	lnValue=lnValue+Iif(llInTransSel,Eval(lcTrns+".Int"+lcInd),0)
	lnValue=lnValue+Iif(llWorkOrderSel,Eval(lcTrns+".WOrd"+lcInd),0)
	lnValue=lnValue+Iif(llOrderSel,Eval(lcTrns+".Ord"+lcInd),0)
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][End]
	ll2Return = ll2Return And (lnValue=0)
Endfor
Select(lnOldAls)
Return(ll2Return)
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
Function lfGetnext
Skip
llGetNext = NotScale == 'Y'
lcGetNext = NotScale
Skip -1
Return llGetNext
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
Function lfTotSz
Local llRet
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*llRet = (!llRpShwZer AND lnSize9=0) OR (llPrintClr AND !llRpWhDeta AND EMPTY(Dyelot))
llRet = (!llRPShwZer And lnSize9=0 And !llChkAllSz) Or (llPrintClr And !llRPWhDeta And Empty(DYELOT))
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]
Return llRet
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
Function lfWhCr
Local llRet
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[Start]
*llRet = (!llRpShwZer AND lnSize9=0) OR (!llRpPrnDye AND !EMPTY(Dyelot))
llRet = (!llRPShwZer And lnSize9=0 And !llChkAllSz) Or (!llRPPrnDye And !Empty(DYELOT))
Return llRet
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

Function lfOTSCalcW
Parameter lcItm
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[Start]
*!*	IF (lcRPOTSSig ='P' AND IIF(lcItm='OTS' , TotOTS,TotIOTS)  >= lnRPOTSMin) OR ;
*!*	   (lcRPOTSSig ='N' AND IIF(lcItm='OTS' , TotOTS,TotIOTS) <= lnRPOTSMin)
If lcRPOTSSig ='A' Or ((lcRPOTSSig $ 'LBP' And Iif(lcItm='OTS' , TotOTS,TotIOTS)  >= lnRPOTSMin) Or ;
		(lcRPOTSSig $'LBN' And Iif(lcItm='OTS' , TotOTS,TotIOTS) <= lnRPOTSMax))
*B609889,1 MMT 04/11/2012 Style summary report deos not show OTS in case Sort by Style Group[ENd]
	Do Case
	Case lcItm = 'OTS'
		Replace lPrnOTS  With .T.
	Case lcItm = 'IOTS'
		Replace lPrnIOTS With .T.
	Endcase
Else
	If lcItm='OTS'
		Replace TotOTS With 0
	Else
		Replace TotIOTS With 0
	Endif
Endif

*!*************************************************************
*! Name      : lfChkTotal
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/19/2008
*! Purpose   : Check if Total is Zero
*!*************************************************************
Function lfChkTotal

Private lnOldAls,ll2Return,lnInd,lcInd,lnValue
ll2Return=.T.
lnValue=0
For lnInd=1 To 8
	lcInd   =  Alltrim(Str(lnInd))
	lnValue = Eval(Alltrim(&lcDummy..Item)+lcInd)
	ll2Return = ll2Return And (lnValue=0)
Endfor
Return(ll2Return)
*: B608565,1 MMT 06/19/2008 Fix bug of wrong totals when print location detail is Yes[End]


*!*************************************************************
*! Name      : lfChkTotal
*! Developer : Tarek Ibrahim
*! Date      : 01/4/2011
*! Purpose   : Check if Total is Zero
*!*************************************************************
*E302831,1 TMI 01/04/2011 [Start] enable/disable the new option 'Print long description'
Function lfLongDscSup
Local llEnable

llEnable = lcRpSortBy $ "WS" And Alen(laRPRepTar,1) = 1 And !Empty(laRPRepTar)
loogScroll.EnableObject('llRpLongDsc', llEnable )
*- End of lfLongDscSup.
*E302831,1 TMI 01/04/2011 [End  ]
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][Start]
*!*************************************************************
*! Name      : lfClearMemory
*! Developer : Mariam Mazhar
*! Date      : 10/17/2019
*! Purpose   : Clear Memory
*!*************************************************************
Function lfClearMemory
Declare Integer SetProcessWorkingSetSize In kernel32 As SetProcessWorkingSetSize  ;
	Integer hProcess , ;
	Integer dwMinimumWorkingSetSize , ;
	Integer dwMaximumWorkingSetSize
Declare Integer GetCurrentProcess In kernel32 As GetCurrentProcess
nProc = GetCurrentProcess()
bb = SetProcessWorkingSetSize(nProc,-1,-1)
*B611928,1 MMT 10/13/2019 - Enhance Report performance[T20181224.0003][END]