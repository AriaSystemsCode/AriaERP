*!***********************************************************************
*: Program file       : ICAVTSHP
*: Program description: Available to ship
*: Module             : Inventory Control (IC)
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*: Tracking Job Number: C202091
*:**********************************************************************
*: Calls:
*:         Programs   :
*:         Screens    :
*: Global Function    :gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe,
*:                     gfRltFld,gfCodDes,gfOptMsg
*:**********************************************************************
*: Called From:
*:**********************************************************************
*: Passed Parameters  :
*:***************************************************************************
*: Example : DO ICSTYLST
*:***************************************************************************
*:                   Option grid Filter contains
*:1-Title                          is                          lcRPTitle
*:2-Domestic/Imported              Domestic/Imported/Both      lcRPDomImp
*:3-Print Style Picture            YES/NO                      llRpPrnPic
*:4-Style                          In List                     STYLE.CSTYMAJOR
*:5-Primary Fabric                 In List                     STYLE.FABRIC
*:6-Season                         In List                     STYLE.SEASON
*:7-Division                       In List                     STYLE.CDIVISION
*:8-Style Group                    In List                     STYLE.CSTYGROUP
*:9-Status                         In List                     STYLE.STATUS
*:10-Only This Color               In List                     SUBSTR(STYLE.Style,lnClrPo,lnColorLen)
*:***************************************************************************
*:                         Tables Used
*:                        _______________
*:01- Style
*:02- Scale
*:03- Objlink
*:04- Objects
*:***************************************************************************
*Modifications:
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[T20060908.0003]
*B610134,1 MMT 10/31/2012 Report prints repeated lines if print picture is set to Yes[T20121023.0008]
*B611473,1 MHM 11/14/2017 Ats report cannot handle extended size scale case Issue6
*B611478,1 MHM style came out wrong while export ats reports issue 23
*B611485,1 MHM error while preview in case of extended scale issue 6
*:***************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include r:\aria4xp\reports\ic\icstylst.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]

lcStTime   = Time()
llDontPrnt = .F.
lcTime     = gfGetTime()
lnMajLen   = Len(Substr(lcMajPic,4))
Set Step On
If ((oAriaApplication.gcDevice = "FILE" .And. loOgScroll.cTextRepType = "EXCEL"))
	= lfUpdtemp()
Else
	llDummy    = loOgScroll.llOGFltCh And lfCollData()

	lcEdTime   = Time()
	lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
	Select FStyle
	Go Top

	Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTED,oAriaApplication.GetHeaderText("LANG_SELECTED",AHEADERFILE)) +' ' + Alltrim(Str(Reccount())) +;
		' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECIN,oAriaApplication.GetHeaderText("LANG_RECIN",AHEADERFILE))+' ' + Alltrim(Str(lnInterval,6,2)) + ' '+;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SECO,oAriaApplication.GetHeaderText("LANG_SECO",AHEADERFILE)) Nowait
	loOgScroll.cCROrientation = 'P'

	Do gfDispRe With Eval('lcRepNam')
Endif
*!*************************************************************
*! Name      : lfCollData
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
Function lfCollData
Create Cursor FStyle ( cvensty C (19) ,Style C (19),Desc C (20), ccolor C(20),CCLRDESC C(30),CSTYGROUPP C (6),ROYALTYY C (6),nsugretpr N (12, 2),;
	Ats1 N(7), Ats2 N(7), Ats3 N(7), Ats4 N(7), Ats5 N(7), Ats6 N(7), Ats7 N(7), Ats8 N(7), Ats9 N(7), Ats10 N(7), Ats11 N(7),;
	Ats12 N(7), Ats13 N(7), Ats14 N(7), Ats15 N(7), Ats16 N(7), Ats17 N(7), Ats18 N(7), Ats19 N(7), Ats20 N(7), Ats21 N(7), Ats22 N(7),;
	Ats23 N(7), Ats24 N(7), Ats25 N(7), Ats26 N(7), Ats27 N(7), Ats28 N(7), Ats29 N(7), Ats30 N(7), Ats31 N(7), Ats32 N(7),;
	Atstotal N(7),TotalAmount N(7),;
	DATE D,locationn C (8), CSTNAM C(30),storee C(8),CustomerPo C(15),Start D, Complete D,terms C(6),shipvia C(6),salesrep1 C(3),salesrep2 C(3),;
	Ordered1 N(7,2),Ordered2 N(7,2),Ordered3 N(7,2),Ordered4 N(7,2),Ordered5 N(7,2),Ordered6 N(7,2),Ordered7 N(7,2),Ordered8 N(7,2),;
	Ordered9 N(7,2),Ordered10 N(7,2),Ordered11 N(7,2),Ordered12 N(7,2),Ordered13 N(7,2),Ordered14 N(7,2),Ordered15 N(7,2),;
	Ordered16 N(7,2),Ordered17 N(7,2),Ordered18 N(7,2),Ordered19 N(7,2),Ordered20 N(7,2),Ordered21 N(7,2),Ordered22 N(7,2),Ordered23 N(7,2),;
	Ordered24 N(7,2),Ordered25 N(7,2),Ordered26 N(7,2),Ordered27 N(7,2),Ordered28 N(7,2),Ordered29 N(7,2),Ordered30 N(7,2),Ordered31 N(7,2),;
	Ordered32 N(7,2),Discount N(5),;
	STATUS C (1), season C (6), cstymajor C (19), cdivision C (6),;
	PATTERN C (10), fabric C (7),  nstyweight N (5, 2),;
	qty_ctn N (3), pricea N (12, 2), priceb N (12, 2), pricec N (12, 2), commission L, Scale C (3), prepak C (1))
If lcRpDomImp <>  'B'
	lcRpExp = lcRpExp + 'AND' + Iif(lcRpDomImp = 'I' , ' !Make' , ' Make' )
Endif
lcStyMaj  = Space(0)
Select Style
Set Order To Style
lnScaleCount = 0
Scan For Evaluate(lcRpExp)
	If (lcStyMaj # Style.cstymajor)
		m.ccolor = Space(0)
	Endif
	m.ccolor= Substr(Style.Style,lnClrPo,lnColorLen) + " "
	m.CCLRDESC = fnDesc()
	If (lcStyMaj # Style.cstymajor)
		Scatter Memvar Memo
		m.ROYALTYY=m.ROYALTY
		m.nsugretpr=m.nsugretpri
		m.CSTYGROUPP=m.CSTYGROUP
		m.locationn=m.location
		m.Ats1=m.stk1
		m.Ats2=m.stk2
		m.Ats3=m.stk3
		m.Ats4=m.stk4
		m.Ats5=m.stk5
		m.Ats6=m.stk6
		m.Ats7=m.stk7
		m.Ats8=m.stk8
		lnScaleCount = 1
		Insert Into FStyle From Memvar
	Else
		lcAlias = Select(0)
		Select FStyle

		Replace ccolor With m.ccolor
*B611485,1 MHM error while preview in case of extended scale issue 6
*!*			For i = 1 To 8
*!*				lcStrscal = Alltrim(Str(i+lnScaleCount*8))
*!*				lcStrscal1 = Alltrim(Str(i))
*!*				Replace ats&lcStrscal With m.stk&lcStrscal1
*!*			Endfor
*B611485,1 MHM error while preview in case of extended scale issue 6
		lnScaleCount = lnScaleCount + 1
		Select (lcAlias)
	Endif
	lcStyMaj = Style.cstymajor
Endscan
*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
Function lfCollTime
Parameters lcStart,lcEnd
lnStHour  = Iif(Val(Left(lcStart,2)) = 0,Val(Left(lcStart,2))+24,Val(Left(lcStart,2)))
lnEndHour = Iif(Val(Left(lcEnd,2))   = 0,Val(Left(lcEnd,2))  +24,Val(Left(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * Val(Substr(lcStart,4,2)) + Val(Right(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * Val(Substr(lcEnd,4,2))   + Val(Right(lcEnd,2))
Return (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfNonMaj
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
lcColorTt = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ONLYTHIS,oAriaApplication.GetHeaderText("LANG_ONLYTHIS",AHEADERFILE)) +' ' + Alltrim(lcNonMajT)
*N000682,1 MMT 02/11/2013 Globalization changes[End]
*-- Compute Free/Color Items in Style Structure. [End]
Return ''

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
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
*-- Check the cost access

lnClrSgPo = Asubscript(laOGVrFlt,;
	ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

Do Case
Case lcFreeClr = 'C'
	laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
	= lfOGShowGet('laOGVrFlt[' + Alltrim(Str(lnClrSgPo)) + ',6]')
*laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
*= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
Case lcFreeClr = 'F'
*laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
*= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
	laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
	= lfOGShowGet('laOGVrFlt[' + Alltrim(Str(lnClrSgPo)) + ',6]')
Endcase
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfAdjPrnArr
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Prepare the arrays that hold the print by values
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjPrnArr()
*!*************************************************************
Function lfAdjPrnArr
Dimension laRPPrnDsp[2,1],laRPPrnRet[2,1]

laRPPrnDsp[1,1]=lcMajTtl
laRPPrnDsp[2,1]=lcNonMajT

laRPPrnRet[1,1]='S'
laRPPrnRet[2,1]='C'

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
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
*! Name      : lfvStyle
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
Function lfvStyle

lcStyle = Varread()

lcTag = Order('STYLE')

Set Order To cStyle In Style

If Lastkey() = 13 And !Mdown()
	If Seek(&lcStyle.,'Style')
		&lcStyle = Style.cstymajor
	Else
		&lcStyle = gfStyBrw('M',"","",.F.)
	Endif
Else
	&lcStyle = ''
Endif
Set Order To lcTag In Style

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
Function lfvFabric
lcFabObj = Varread()

lcFab    = &lcFabObj

llUseByMe = .F.

If !Used('FABRIC')
	llUseByMe = .T.
	Use (gcDataDir+'FABRIC') In 0 Share
Endif

lcTag = Order('FABRIC')

Set Order To fabric In fabric

If Lastkey() = 13 And !Mdown()
	If Seek(lcFab,'FABRIC')
		&lcFabObj = fabric.fabric
	Else
		= FaBrow(@lcFab,'*')
		&lcFabObj = lcFab
	Endif
Else
	&lcFabObj = ''
Endif

Set Order To fabric In fabric

If llUseByMe
	Use In fabric
Endif


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
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
*B802264,1
Function lfSRVSty
Parameters lcParm
Do Case
Case lcParm = 'S'  && Set code
*-- open this file in another alias to set order to Style Major
*-- unique index.
	Use (gcDataDir+'Style') Again Alias STYLE_X Order Tag Style In 0
	Select Style
	Set Order To Tag cStyle
	Set Relation To Style.Style Into STYLE_X
	Go Top In Style
Case lcParm = 'R'  && Reset code
	Use In STYLE_X
	Select Style
	Set Order To Tag Style
Endcase
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
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
*B802264,1
Function lfStySum
Parameters lcSty,lccomp,lnAddToVar
Private lnStyRec
lnTotcomp = 0

If Reccount('STYLE') != 0
	lnStyRec = Recno('STYLE')
	Select STYLE_X
	Sum &lccomp To lnTotcomp While Style = Alltrim(lcSty)
	Select Style
	If Between(lnStyRec,1,Reccount())
		Go lnStyRec
	Endif
	Do Case
	Case lnAddToVar = 1
		lnO_T_S = lnTotcomp
	Case lnAddToVar = 2
		lnO_T_S = lnO_T_S + lnTotcomp
	Case lnAddToVar = 3
		lnO_T_S = lnO_T_S - lnTotcomp
	Endcase
Endif
Return Int(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfvBins      *:E301271,1
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Valid Function for Bins
*!           :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvBins()
*!*************************************************************
Function lfvBins
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
*= gfMover(@laRpSource,@laRpTarget,'Style Bins',.T.,'')
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,'Style Bin',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STYLEBIN,oAriaApplication.GetHeaderText("LANG_STYLEBIN",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 MMT 02/11/2013 Globalization changes[End]
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]

*!*************************************************************
*! Name      : lfFillBin     *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Function to fill bins arrays
*!           :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
Function lfFillBin
llUSdBy =gfOpenFile(gcDataDir+'WHSLOC' ,'WHSLOC','SH')
Dime laRpSource[1,1]
Dime laRpTarget[1,1]
Select WHSLOC
Select Distinct CLOCATION From WHSLOC Where !Empty(CLOCATION)Into Array laRpSource
Use In Iif(llUSdBy,'WHSLOC',0)

*!*************************************************************
*! Name      : lfClearRed *:E301271,1
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Functio to clear read
*!           :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfClearRed()
*!*************************************************************
Function lfClearRed
Clear Read

*****************************************
Function lfGetStr
Private lcTmpAlias
lcTmpAlias  = Select(0)
lcSizStr    = ''
lcPrePakStr = ''
lcatastr = ""
lntotal = 0
*****************************************
*B611473,1 MHM 11/14/2017 Ats report cannot handle extended size scale case Issue6
*!*	Select Scale
*!*	If Seek('S'+FStyle.Scale)
*!*	  lcSizStr = Sz1 + ' ' + Sz2 + ' ' + Sz3 + ' ' + Sz4 + ' ' + Sz5 + ' ';
*!*	    + Sz6 + ' ' + Sz7 + ' ' + Sz8
*!*	Endif
Set Step On
Select Scale
lcscale=Substr(FStyle.Scale,1,2)
If Seek('S'+lcscale)
	Scan Rest While Scale  = lcscale
		For i = 1 To Scale.Cnt
			lcstr = Alltrim(Str(i))
			lcSizStr = lcSizStr +Padr(Alltrim(Scale.SZ&lcstr),6,' ')
			lcatastr = lcatastr+Padr(Alltrim(Str(FStyle.ats&lcstr)),6,' ')
			lntotal = lntotal + FStyle.ats&lcstr.
		Endfor
	Endscan
Endif
*B611473,1 MHM 11/14/2017 Ats report cannot handle extended size scale case Issue6
*C202059 , MHM 08/21/2017 Custom report Available to ship
If Seek('P'+FStyle.Scale+FStyle.prepak)
	lcPrePakStr = Str(Pp1) + Str(Pp2) + Str(Pp3) + Str(Pp4) + Str(Pp5) + ;
		STR(Pp6) + Str(Pp7) + Str(Pp8)
Endif


Select (lcTmpAlias)
*C202059 , MHM 08/21/2017 Custom report Available to ship
Return lntotal
*C202059 , MHM 08/21/2017 Custom report Available to ship
*C202059 , MHM 08/21/2017 Custom report Available to ship

*!**************************************************************
* Name      : lfModeVld
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
* Purpose   : Report Mode Validation
* Job No.   : B# 803531,1
*!*************************************************************
* Called from : Option Grid
*!*************************************************************
* Passed Parameters  : None
*!*************************************************************
* Returns            : None
*!*************************************************************
* Example   : =lfModeVld()
*!*************************************************************
*-B803531,1 RAMY [start]
Function lfModeVld
Clear Read
*-- end of lfModeVld.

*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
*!*************************************************************
*! Name      : lfCreatExp
*! Developer : MARIAM MAZHAR (MMT)
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
Function lfCreatExp
If Type('llCallFromScr') = 'L'
	=Acopy(loOgScroll.laOGFxFlt , laFxExpr)
	=Acopy(loOgScroll.laOGVrFlt , laVrExpr)
Endif

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
Function RefreshStatus
Local lcStatusStr, lnTarget
lcStatusStr = ""
If !Empty(laRpTarget)
	For lnTarget = 1 To Alen(laRpTarget,1)
		lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
	Endfor
	lcStatusStr = Substr(lcStatusStr,3)
Endif
Return lcStatusStr
Endfunc
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]

*C202059 , MHM 08/21/2017 Custom report Available to ship
*!*************************************************************
*! Name      : Retail
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
Function Retail
Select FStyle
lnretail = FStyle.nsugretpr
Return lnretail
Endfunc
*C202059 , MHM 08/21/2017 Custom report Available to ship
*!*************************************************************
*! Name      : Vensty
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
Function Vensty
Select FStyle
lcvendor = FStyle.cvensty
Return lcvendor
Endfunc
*C202059 , MHM 08/21/2017 Custom report Available to ship
*!*************************************************************
*! Name      : fnDesc
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************

Function fnDesc
lcdecrp = " "
If !Used('CODES')
	= gfopentable('CODES', 'CODES')
	lcdec = 'N' + ccolor + 'N' + 'COLOR'
	= Seek(lcdec, 'CODES', 'CODES')
	lcdecrp = codes.cdiscrep
Else
	lcdec = 'N' + Padr(ccolor,6,' ') + 'N' + 'COLOR'
	= Seek(lcdec, 'CODES', 'CODES')
	lcdecrp = Alltrim(codes.cdiscrep)
Endif
Return lcdecrp
Endfunc
*C202059 , MHM 08/21/2017 Custom report Available to ship
*!*************************************************************
*! Name      : TotAmount
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
Function TotAmount
lctotam = lnretail*lntotal
Return lctotam
Endfunc
*C202059 , MHM 08/21/2017 Custom report Available to ship

*new
*!*************************************************************
*! Name      : lfUpdtemp
*! Developer : Mohamedhamdy MHM
*! Date      : 08/21/2017
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
Function lfUpdtemp
Create Cursor FStyle ( cvensty C (19) ,Style C (19),Desc C (20), ccolor C(20),CCLRDESC C(30),CSTYGROUPP C (6),ROYALTYY C (6),nsugretpr C (12),Image C(5),;
	Ats1 C(7), Ats2 C(7), Ats3 C(7), Ats4 C(7), Ats5 C(7), Ats6 C(7), Ats7 C(7), Ats8 C(7), Ats9 C(7), Ats10 C(7), Ats11 C(7),;
	Ats12 C(7), Ats13 C(7), Ats14 C(7), Ats15 C(7), Ats16 C(7), Ats17 C(7), Ats18 C(7), Ats19 C(7), Ats20 C(7), Ats21 C(7), Ats22 C(7),;
	Ats23 C(7), Ats24 C(7), Ats25 C(7), Ats26 C(7), Ats27 C(7), Ats28 C(7), Ats29 C(7), Ats30 C(7), Ats31 C(7), Ats32 C(7),;
	Atstotal C(10),TotalAmount C(15),;
	DATE C(8),Time C(8),locationn C (8), CSTNAM C(30),storee C(8),CustomerPo C(15),StartDate C(20), CompleteDate C(20),terms C(6),shipvia C(10),salesrep1 C(15),salesrep2 C(15),;
	Ordered1 C(20),Ordered2 C(20),Ordered3 C(20),Ordered4 C(20),Ordered5 C(20),Ordered6 C(20),Ordered7 C(20),Ordered8 C(20),;
	Ordered9 C(20),Ordered10 C(20),Ordered11 C(20),Ordered12 C(20),Ordered13 C(20),Ordered14 C(20),Ordered15 C(20),;
	Ordered16 C(20),Ordered17 C(20),Ordered18 C(20),Ordered19 C(20),Ordered20 C(20),Ordered21 C(20),Ordered22 C(20),Ordered23 C(20),;
	Ordered24 C(20),Ordered25 C(20),Ordered26 C(20),Ordered27 C(20),Ordered28 C(20),Ordered29 C(20),Ordered30 C(20),Ordered31 C(20),;
	Ordered32 C(20),Discount C(15),DiscountCost C(30))
If lcRpDomImp <>  'B'
	lcRpExp = lcRpExp + 'AND' + Iif(lcRpDomImp = 'I' , ' !Make' , ' Make' )
Endif
Select 0
Select * , Substr(Scale,1,2) lcScale1 Where &lcRpExp From Style Order By lcScale1,Style Into Cursor tempStyle
Set Step On
lnMaxNumber = 0
If _Tally > 0

* Hold the scale to check the scale chnaged
	Store 0 To lnScaleCount, lnrecno
	Store "" To lcStyMaj, lcscale, lccolor

	Select tempStyle
	Scan
		Scatter Memvar Memo
		m.ccolor= Substr(tempStyle.Style,lnClrPo,lnColorLen) + " "
		m.CCLRDESC = fnDesc()
		If (lcStyMaj # tempStyle.cstymajor Or lccolor # m.ccolor)
			lnScaleCount = 1
* check the scale is changed or not
			If !(Substr(m.scale,1,2) == lcscale)
&& Add header line
				lcscale = Substr(m.scale,1,2)
				Insert Into FStyle (cvensty,Style,Desc,ccolor,CCLRDESC,CSTYGROUPP,ROYALTYY,nsugretpr,Image,;
					Ats1,Ats2,Ats3, Ats4, Ats5, Ats6, Ats7, Ats8, Ats9, Ats10, Ats11,;
					Ats12,Ats13,Ats14,Ats15, Ats16, Ats17, Ats18,Ats19,Ats20,Ats21, Ats22,;
					Ats23,Ats24,Ats25,Ats26, Ats27,Ats28,Ats29,Ats30,Ats31,Ats32,;
					Atstotal,TotalAmount,Date,Time,locationn,CSTNAM,storee,CustomerPo,StartDate,CompleteDate,terms,shipvia,salesrep1,salesrep2,;
					Ordered1,Ordered2,Ordered3,Ordered4,Ordered5,Ordered6,Ordered7,Ordered8,;
					Ordered9,Ordered10,Ordered11,Ordered12,Ordered13,Ordered14,Ordered15,Ordered16,Ordered17,Ordered18,Ordered19,Ordered20,;
					Ordered21,Ordered22,Ordered23,Ordered24,Ordered25,Ordered26,Ordered27,Ordered28,Ordered29,Ordered30,Ordered31,Ordered32,Discount,DiscountCost);
					VALUES('Vendor Style','Style','Description','Color Code','Color Description','Group','Line','Unit Retail','Image',;
					'','','','','','','','','','','','','','','','','','','','','',;
					'','','','','','','','','','','','ATStotal','Total Amount','Date','Time','Location','Customer','Store','Customer PO','StartDate','Complete Date','Terms','Ship Via',;
					'salesrep1','salesrep2','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','Discount%','DiscountCost')

				Select Scale
				If Seek('S'+ tempStyle.Scale )
					Select FStyle
					lnMaxNumber = Max(lnMaxNumber, Scale.Cnt)
					For i = 1 To Scale.Cnt
						lcstr = Alltrim(Str(i))
						Replace ats&lcstr. With "ATS "+ Scale.SZ&lcstr.
						Replace Ordered&lcstr. With "Ordered "+ Scale.SZ&lcstr.
					Endfor
				Endif
				lnrecno = Recno('Fstyle')

			Endif
			Set Step On
			m.ROYALTYY  = m.ROYALTY
			m.nsugretpr = Alltrim(Str(m.nsugretpri))
			m.CSTYGROUPP = m.CSTYGROUP
			m.locationn = m.location
			m.DATE=Dtoc(Date())
			m.Time= Alltrim(Str(Hour(Datetime())))+":"+Alltrim(Str(Minute(Datetime( ))))
			m.nsugretpr = Alltrim(Str(m.nsugretpri))
			m.CSTYGROUPP = m.CSTYGROUP
			m.locationn = m.location
			m.Ats1=Alltrim(Str(m.stk1))
			m.Ats2=Alltrim(Str(m.stk2))
			m.Ats3=Alltrim(Str(m.stk3))
			m.Ats4=Alltrim(Str(m.stk4))
			m.Ats5=Alltrim(Str(m.stk5))
			m.Ats6=Alltrim(Str(m.stk6))
			m.Ats7=Alltrim(Str(m.stk7))
			m.Ats8=Alltrim(Str(m.stk8))
			m.Atstotal=Alltrim(Str(m.totstk))
			m.TotalAmount=Alltrim(Str(m.totstk * Val(m.nsugretpr)))
			m.START = Dtoc(m.START)
			m.nstyweight = Alltrim(Str(m.nstyweight))
			m.qty_ctn = Alltrim(Str(m.qty_ctn ))
			m.pricea  = Alltrim(Str(m.pricea ))
			m.priceb  = Alltrim(Str(m.priceb ))
			m.pricec  = Alltrim(Str(m.pricec ))
			m.commission = Iif(m.commission,"True","False")
			m.disc_pcnt  = Alltrim(Str(m.disc_pcnt))
			m.gros_price = Alltrim(Str(m.gros_price ))
			m.nsugretpri = Alltrim(Str(m.nsugretpri ))
*B611478,1 MHM style came out wrong while export ats reports issue 23
* m.Style = Strtran(m.Style,Alltrim(m.scale),'')
			m.Style = Substr(m.Style,1,15)
*B611478,1 MHM style came out wrong while export ats reports issue 23

			Insert Into FStyle From Memvar


		Else
			lcAlias = Select(0)


&& update the sizes for base style
			Select FStyle
			Replace ccolor With m.ccolor In FStyle

			Select Scale
			If Seek('S'+ tempStyle.Scale )
				lnMaxNumber = Max(lnMaxNumber, Scale.Cnt+lnScaleCount*8)
				For i = 1 To Scale.Cnt
					lcStrscal = Alltrim(Str(i+lnScaleCount*8))
					lcStrscal1 = Alltrim(Str(i))

					Replace ats&lcStrscal With Alltrim(Str(m.stk&lcStrscal1))  In FStyle

				Endfor
				Replace FStyle.Atstotal With Alltrim(Str(m.totstk + Val(FStyle.Atstotal)))   In FStyle
				Replace FStyle.TotalAmount With Alltrim(Str(Val(FStyle.Atstotal) * Val(FStyle.nsugretpr)))  In FStyle

			Endif

			lnError = 0
&& update the sizes for style Header line
			Try

				lnrecnoCurrent = Recno('Fstyle')
				Go lnrecno In FStyle
				Select Scale
				If Seek('S'+ tempStyle.Scale )
					lnMaxNumber = Max(lnMaxNumber, Scale.Cnt+lnScaleCount*8)
					For i = 1 To Scale.Cnt
						lcstr = Alltrim(Str(i))
						lcStrscal = Alltrim(Str(i+lnScaleCount*8))
						Replace ats&lcStrscal. With "ATS "+ Scale.SZ&lcstr. In FStyle
						Replace Ordered&lcStrscal. With "Ordered "+ Scale.SZ&lcstr. In FStyle
					Endfor
				Endif

				Go lnrecnoCurrent In FStyle

			Catch To lnError

			Endtry
			lnScaleCount = lnScaleCount + 1
			Select (lcAlias)
		Endif
		lcStyMaj = tempStyle.cstymajor
		lccolor = m.ccolor
	Endscan
Endif
Select FStyle
Locate
lctype = Iif(loOgScroll.cTextRepType=="EXCEL","XLS","XL5")
lcFile = oAriaApplication.WorkDir +  loOgScroll.gfTempName()+ "." + lctype
Export To &lcFile. Type Xls
* open and add the image
= lfAddimage(lcFile)
Endfunc
*************************************************************
Function lfAddimage
Lparameters lcFile
Set Step On
Try
	oExcel = Createobject("Excel.Application")
	With oExcel
		.Visible = .F.
		oExcel.Workbooks.Open(lcFile)
		With .ActiveWorkBook.ActiveSheet
&& Remove the not used size's columns
			lnMaxNumber = Min(lnMaxNumber,32)
			If (lnMaxNumber+1)<32
				lnMaxNumber = lnMaxNumber  +1
&& Get the corresponging column
				For i=lnMaxNumber  To 32
*lcstr123 = Alltrim(Str(i))
*oExcel.ActiveWorkBook.ActiveSheet.Columns(lnMaxNumber+55).Delete()
					oExcel.ActiveWorkBook.ActiveSheet.Columns(i+55).ColumnWidth = 0
				Endfor
				For i=lnMaxNumber  To 32
*lcstr123 = Alltrim(Str(i))
*oExcel.ActiveWorkBook.ActiveSheet.Columns(lnMaxNumber+9).Delete()
					oExcel.ActiveWorkBook.ActiveSheet.Columns(i+9).ColumnWidth = 0
				Endfor
			Endif
			For i = 1 To Reccount('Fstyle')
				Select FStyle
				Go i
				If !(Alltrim(Upper(FStyle.Style)) = 'STYLE')
					oExcel.ActiveWorkBook.ActiveSheet
					Local loRange,loRange1
					loRange  = 'I'+Alltrim(Str(i+1))+":"+'I'+Alltrim(Str(i+1))
					loRange1 = .Range(loRange)
					Try
						If llRpPrnPic='Y'
							If Seek(Alltrim(FStyle.Style),'style','style')
								lcImagePath = gfGetStyleImageInfo("S",Style.cstymajor, .T.)
*lcImagePath  = 'X:\Aria4XP\Reports\IC\u.JPG'
								If !Empty(lcImagePath) And File(lcImagePath)
									.Shapes.AddPicture( lcImagePath, .T., .T., ;
										loRange1.Left, loRange1.Top, loRange1.Width, loRange1.Height)
								Endif
							Endif
						Endif
					Catch To lnError
					Endtry
				Endif
			Endfor
		Endwith
	Endwith
Catch To lnError
Endtry
oExcel.ActiveWorkBook.ActiveSheet.Rows(1).Delete()
Set Step On
lcFile = Alltrim(oAriaApplication.gcOutFile)
If  lctype = "XLS"
	oExcel.ActiveWorkBook.SaveAs(lcFile,39)
Else
	oExcel.ActiveWorkBook.SaveAs(lcFile)
Endif
oExcel.Quit()
Endfunc
