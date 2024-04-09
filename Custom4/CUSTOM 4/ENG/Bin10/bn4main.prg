*!B609511,1 MMT 01/24/2011 Check if use Bin location  before calling automic picking function in bn4main[T20100802.0018]
*!C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003]
*!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(T20100129.0008)
*!B609651,1 MMT 07/26/2011 Receiving program assign wrong Bins to lines in case of receiving to more than one loc.[T20110708.0004]
*!B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[T20111013.0017]
*!B609714,2 MMT 11/17/2011 cannot Browse Bin Invjl from Style screen for certain style-warehouse[T20110621.0057]
*!B610341,1 SAB 11/17/2011 Fix Automatic Allocation screen hang issue [T20130111.0006]


*!T20071102.0018,10/C200876 TMI 07/07/2008 [Start] NOTE : VERY SERIOUS
* This project, i.e. the bin location custom, has a main bad coding design that is , it is based on make a full copy of
* a global function and make the desired changes on it. This violates the standard programming principles that the code
* should not be repeated
* This may be lead to a maintainance nightmare ( as in the Pragmatic Programmer book )
*T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]

*T20071102.0018,10/C200876 TMI 07/28/2008 [Start]
* This ticket was estimated as 500 working hours
* It was supposed to be divided into subtasks , this has not happened, I think this is a mistake,
* I think also that all who shared in this ticket has fall in the same mistake till it reached to the programmer
* My duty at that time was to refuse this ticket as 500 and arguing to subdivide it, but I noticed that after a long while
* And this is one of my mistakes
* T20071102.0018,10/C200876 TMI 07/28/2008 [End  ]
*!C201527,1 HIA 10/21/2012 Change Allocation Calculation [T20121002.0002]
*!B610192,1 HIA 01/13/2013 PO- cannot change location on receiving inter location po [T20121219.0013]
*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001]
*!**************************************************************************
*! Name      : BN4MAIN.PRG
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 26/08/2007
*! Purpose   : David Luke Ltd Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*T20071102.0018,10/C200876 TMI 05/16/2008 [Start]
NOTE SERIOUSLY IMPORTANT
*Due to the long period that this project wasted then all the standard attachments need to be borrowed from the VSS
*and to update them with the triggers saved in the standard files in the folder t:\aria4xp\tmi\aria4xp
*T20071102.0018,10/C200876 TMI 05/16/2008 [End  ]

NOTE 11/29/2007
*=*	Aria27 exe's
*==    ======
*=*	C200875==> Aria27 added fields and sycconfg lines
*=*	C200898==> Bin Location Triggers used in Aria4xp
*=*	C200910==> Triggers of IC,PO,SO
*=*	C200980==> Triggers of AL,AR,RM,POCSSH
*      C200986==> ALPKTKDL  form
*                 ICPINVBN  custom report

*  I done B608546, I allowed to search the key in browse screen with the second field is keyed

*=*	Aria4 exe's
*==    ======
*=*	C200895==> SQL-tables
*=*	C200876==> ( to be for standards only )
*=*	C200983==> ( to be for Customs only   )
*=*	C200987==>  ALPKTKDL  form
*               ICPINVBN  custom report

*   B608639, this bug fixes the browse.scx to not show a decimals

** the following exe's are no more needed
** supplement attachemnts == > C201022// This attachment will tell you what last standard files needs to be checked from the VSS
*** B608548, this bug fixes the BROWSE.SCX screen to not trim the passed key  && the current aria.exe is now updated

*:***************************************************************************

** Modifications
*T20071102.0018,10/C200876 TMI 29/10/2008 [Start] force to open the WHBINLOC file
*B608813,1 MMT 03/04/2209 Fix bugs of wrong browse of Bin when user change company[T20081106.0001]
*B608826,1 TMI 03/24/2009 include/fix some problems for updating the consolidated invoice
*B608850,1 TMI 04/16/2009 Call the ALORDAL from SOORD.SCX screen
*B608844,2 TMI 04/18/2009 if lines are repeated in BININVJL due to zeroing stock, increment the LINENO filed value
*B608850,2 TMI 04/20/2009 reWriting the lfAlSavAut
*B608890,1 AHS 06/08/2009 Uninformative message on Void credit memo where this is not enough bin inventory
*BB08893,1 MMT 06/11/2009 Fix bug of not opening Allocation screen from Sales order screen  [T20080812.0001]
*B608894,1 TMI 06/14/2009 locate the pointer in the scale file using the correct scale
*B608898,3 TMI 06/25/2009 use the lcLocation instead of PKBINLOC.CLOCATION
*B608939,1 TMI 07/15/2009 use the INVLINE.LINENO field instead of &lcDetfile..lineno [T20090226.0002 ]
*t20070214.0006  TMI 8/18/2009 the function lfDispScr is in BN4MAIN.FXP and DIRMAIN.fxp so a confilet exists, so releasing this by specifying the IN program called from
*B608980,1 TMI 08/23/2009 show custom message for the cancel FOR BIN10 [T20070214.0006 ]
*B609079,1 HES 09/11/2009 Error Message on Allocation by style [T20091008.0001]
*B609146,1 TMI 02/10/2010 suggested to cure the duplication problems at DCC [T20100128.0002]
*B609146,1 TMI 02/15/2010 Prevent 'Opening allocation screen' while saving,
*B609146,1 TMI            instead open it from options menu [T20100128.0002]
*B609532,1 TMI 02/19/2011 Can't cancel balance of Shipment [T20101209.0014]
*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [T20110621.0044]
*C201454,1 SAB 01/17/2012 Enable Receiving to bin location in Inter Location PO case [T20110621.0044]
*B609831,1 SAB 02/15/2012 Fix problems in issue# 23 in the project [T20110621.0044]
*B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[T20120113.0062]
*B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [T20110621.0044]
*C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [T20110621.0044]
*B609928,1 MMT 05/20/2012 Don't give message in case of setups blk/pick is set to NO[T20120320.0004]
*C201487,1 MMT 05/28/2012 Custom option to display Bin location Audit trail  from BININVJL[T20120330.0009]
*C201487,2 MMT 06/07/2012 Filter by location while displaying Bin location Audit trail  from BININVJL[T20120330.0009]
*B609983,1 MMT 07/02/2012 Incorrect receiving session used while adjustment in case of bin location is used[T20120621.0006]
*B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006]
*B610048,1 MMT 08/16/2012 Style WIP/Intrans Qtys are not update while receiving shipments if Bin Location is used[T20120708.0002]
*B610066,1 MMT 09/02/2012 Add Records to Bin Temp. in Automatic/Manual style selection[T20120816.0012]
*B610066,2 MMT 09/12/2012 don't add Records to Bin Temp. in Automatic/Manual style selection if location field is empty[T20120816.0012]
*B610066,3 MMT 10/03/2012 Incorrect records added to WHBINLOC table[T20120816.0012]
*B610066,4 MMT 10/09/2012 user can issue qty from style un-assigned bins[T20120816.0012]
*B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[T20120816.0012]
*B610153,2 SAB 11/13/2012 Fix problem of not showing Order Charges on Sales Order Option [T20121102.0002]
*B610160,1 SAB 11/25/2012 Hide Assing bin Message when receiving Material PO [T20121113.0007]
*C201540,1 SAB 12/11/2012 Hide Do you want to pick Sales Order message for bid sales order [T20121128.0003]
*B610181,1 SAB 12/24/2012 Fix generating multible pick tickt records when create pick tickt from sales order [T20121220.0002]
*C201552,1 HES Fix the "Ti" string to be "To" [T20121030.0049]
*B610264,1 HIA 03/06/2013 Aria4xp - SO - Pick process on saving the sales order [T20130204.0009]
*B610350,1 TMI 06/02/2013 keep the session # unchanged to prevent some problems [T20130222.0001] 
*B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010]
*B610658,1 TMI 01/19/2014 Fix a problem that the binlocjl file is not being updated correctly when create and void 
*B610658,1 TMI            invoice in case of the qty being issued from is received in several sessions[T20131212.0016] 
*B610671,1 TMI 02/05/2014 Change the way lines are added to BININVJL in LOCKING  [T20131212.0016] 
*B610676,1 TMI 02/10/2014 call the standard gfStyCrl from the adjustment program  [T20131211.0107] 
*C201605,1 TMI 02/20/2014 update the cowner with BININVTR in case of transferring between bins in the same warehouse [T20140110.0015] 
*B610685,1 TMI 03/11/2014 Modify function lfDLSBNPOR in BN4MAIN.PRG to use variable lcWareHouseCode instead of m.cwarecode in the seek and scan statements that are done on STYINVJL table [T20140224.0028] 
*B610745,1 TMI 06/12/2014 Don't check for binloc in case of noninv style, update the lfRMCKSVBN and lfDLSAVRM functions[T20140514.0006]
*                         if you select a damaged style it is the damnaged style code you have to check to see if it is a non inv style not the actual style on the credit memo
*B610762,1 tmi 6/24/2014 [start] don't allow stock be less than allocated [T20140604.0001]
*B610792,1 TMI 08/04/2014 Show only styles with low stock, not all styles [T20140731.0001] 
*B610831,1 TMI 09/01/2014 stop saving when there is not enough stock in bins [T20140603.0014 ] 
*B610858,1 TMI 09/15/2014 14:28 [Start] make sure that the style and stydye are updated [T20140603.0014]
*B610860,1 TMI 09/18/2014 prevent stock being less than allocation in icinvt_a.scx [T20140813.0019] 
*:***************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*Testing code, must be removed for the working version
IF UPPER(SYS(0)) == 'TARIK # TMI'
  lcErrHndlr = ON('ERROR')
  ON ERROR

  IF !lfChkTrgEnt(loFormSet,lcEvntFun)
    ON ERROR &lcErrHndlr
    RETURN
  ENDIF

ENDIF
*Testing code, must be removed for the working version

*--Run the function.
llRetValue = EVAL(lcFunToRun)


*Testing code, must be removed for the working version
IF UPPER(SYS(0)) == 'TARIK # TMI'
  ON ERROR &lcErrHndlr
ENDIF
*Testing code, must be removed for the working version

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfIsUseBin
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : check if binlocation is used
*:***************************************************************************
FUNCTION lfIsUseBin

LOCAL llUseBin,llTrackBins
STORE .F. TO llUseBin,llTrackBins

llTrackBins = gfGetMemVar('M_WARELOC') = 'Y'  && Keep track of bins
llTrackBins = IIF(TYPE('llTrackBins')='L',llTrackBins,.F.)
IF llTrackBins
  llUseBin  = gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  llUseBin = IIF(TYPE('llUseBin')='L',llUseBin,.F.)
ENDIF
RETURN llTrackBins .AND. llUseBin
*-- end of lfIsUseBin.

*- Triggers called from ICSTYLE.SCX screen

*:**************************************************************************
*:* Name        : lfCSTPRICE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : call the ICSZDAT screen that updates the memo fields for bin class data
*:***************************************************************************
*:* Called from : ICSTYLE.SCX
*:***************************************************************************
FUNCTION lfICSZDAT

LOCAL lnCntBar
IF !lfIsUseBin()
  RETURN
ENDIF

lnCntBar = CNTBAR('_lPopOpt')+1
DEFINE BAR lnCntBar OF _lPopOpt PROMPT '\<Bins related size data' SKIP FOR ;
  _SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'S'
  
ON SELECTION BAR lnCntBar OF _lPopOpt DO FORM (oAriaApplication.ScreenHome+'IC\ICSZDAT.SCX') WITH _SCREEN.ACTIVEFORM.PARENT  

*- Add the options "Bin Locations" to the "Style" option menu pad
*-- Count Options BARS to add the new BAR at the end of the Popup
lnCntBar = CNTBAR('_lPopOpt')+1
lcForExp = IIF(gfGetMemVar('M_USEEXSSC'),[OR _Screen.ActiveForm.Parent.llAllColors],'')
*-- Define New options (Bin Locations),This Option Menu will be enable in the Edit mode.
DEFINE BAR lnCntBar OF _lPopOpt PROMPT '\<Bin Locations Look Up' SKIP FOR ;
  (_SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'S') &lcForExp
ON SELECTION BAR lnCntBar OF _lPopOpt DO lfvBinBrow IN BN4MAIN WITH _SCREEN.ACTIVEFORM.PARENT 
*C201487,1 MMT 05/28/2012 Custom option to display Bin location Audit trail  from BININVJL[Start]
IF lfIsUseBin()
  lnCntBar2 = CNTBAR('_lPopOpt')+1
  DEFINE BAR lnCntBar2 OF _lPopOpt PROMPT 'Bin Location Audit Trail' SKIP FOR ;
    (_SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'S' OR _SCREEN.ACTIVEFORM.PARENT.AriaForm1.pgfStyleInfo.ACTIVEPAGE <> 2)
  ON SELECTION BAR lnCntBar2 OF _lPopOpt DO lfvBinJlBrow IN BN4MAIN WITH _SCREEN.ACTIVEFORM.PARENT  
ENDIF
*C201487,1 MMT 05/28/2012 Custom option to display Bin location Audit trail  from BININVJL[END]
*-- end of lfICSZDAT.
*:**************************************************************************
*:* Name        : lfSvBnSzDa
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Save the size data related to bins
*:***************************************************************************
*:* Called from : ICSTYLE.SaveFiles Method
*:***************************************************************************
FUNCTION lfSvBnSzDa
LOCAL lnSlct,lcFldLst,lcColorFil,lcStyle,lcTmpBnDat
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

*- Be sure that the file is created and is filled with data
=lfCrTmpSzd()

lcFldLst = 'CPRIMCLSS, CSECCLSS, CREMCLSS, NREPTRIG, NREPQTY, NREPTOLR'

lcTmpBnDat = 'B'+SUBSTR(loFormSet.lcColorFil,2)
lcColorFil = loFormSet.lcColorFil

SELECT (lcColorFil)
=SEEK(loFormSet.lcStyleKey,lcColorFil)
SCAN REST WHILE STYLE = loFormSet.lcStyleKey

  lcStyle = &lcColorFil..STYLE

  SELECT (lcTmpBnDat)
  =SEEK(&lcColorFil..SCALE,lcTmpBnDat)

  SCATTER FIELDS &lcFldLst. MEMVAR MEMO

  *--Update Style
  SELECT STYLE
  =SEEK(lcStyle)
  GATHER FIELDS &lcFldLst. MEMVAR MEMO
  =gfReplace('')

ENDSCAN

*- release the temp bindata file
USE IN (lcTmpBnDat)
ERASE (oAriaApplication.WorkDir+lcTmpBnDat+'.DBF')
ERASE (oAriaApplication.WorkDir+lcTmpBnDat+'.CDX')
ERASE (oAriaApplication.WorkDir+lcTmpBnDat+'.FPT')

SELECT (lnSlct)
*-- end of lfSvBnSzDa.

*:**************************************************************************
*:* Name        : lfSTYFLDEF
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Update CFLATHANG field with its default value
*:***************************************************************************
*:* Called from : ICSTYLE.SCX , when adding a new STYLE
*:***************************************************************************
FUNCTION lfSTYFLDEF
LOCAL lnFPos,lnSlct
lnSlct = SELECT(0)

IF loFormSet.Activemode = 'A'
  *- Initialize the array that holds user fields with the desired value
  lnFPos = ASCAN(loFormSet.laUserFields,'CFLATHANG',1,ALEN(loFormSet.laUserFields,1),1)
  IF lnFPos>0

    *T20071102.0018,10/C200876 TMI 09/11/2008 [Start] due to a bug in Aria4xp that it does not accept the default value of a field hard code the default value and comment these lines
    *!*	    IF !USED('SYDFIELD')
    *!*	      =gfOpenTable(oAriaApplication.SysPath+'SYDFIELD','CFLD_NAME','SH')
    *!*	    ENDIF
    *!*	    =gfSEEK('CFLATHANG','SYDFIELD')
    *!*	    lcDefVal = ALLTRIM(SYDFIELD.MVENTRIES)+'|'
    *!*	    lcDefVal = SUBSTR(lcDefVal,AT('@',lcDefVal)+1)
    *!*	    lcDefVal = SUBSTR(lcDefVal,1,AT('|',lcDefVal)-1)
    *T20071102.0018,10/C200876 TMI 09/11/2008 [End  ]

    lnFPos = ASUBSCRIPT(loFormSet.laUserFields,lnFPos,1)
    *loFormSet.laUserFields[lnFPos,6] = lcDefVal
    loFormSet.laUserFields[lnFPos,6] = 'F'


    SELECT STYLE
    REPLACE CFLATHANG WITH 'F'

  ENDIF

ENDIF

SELECT (lnSlct)
*-- end of lfSTYFLDEF.

*:**************************************************************************
*:* Name        : lfCrTmpSzd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Create temp file to hold sizes data related to bins
*:***************************************************************************
*:* Called from : icszdat.scx, from ICSTYLE
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrTmpSzd()
*:***************************************************************************
FUNCTION lfCrTmpSzd
PARAMETERS loObj

IF TYPE('loObj')='O'
  loFormSet = loObj
ENDIF

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnSlct,laFStru,lnI,llOpnScale,llOpnStyle
lnSlct = SELECT(0)

STORE .F. TO llOpnScale,llOpnStyle
IF !USED('SCALE')
  llOpnStyle = gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF
IF !USED('STYLE')
  llOpnScale = gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF

PRIVATE lcTmpBnDat
lcTmpBnDat = 'B'+SUBSTR(loFormSet.lcColorFil,2)

IF !USED(lcTmpBnDat)

  IF FILE(oAriaApplication.WorkDir+lcTmpBnDat+'.DBF')

    SELECT 0
    USE (oAriaApplication.WorkDir+lcTmpBnDat+'.DBF') ORDER 1 EXCLUSIVE

  ELSE

    DIMENSION laFStru[24,4]
    lnFStru = ALEN(laFStru,1)

    lnI = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SCALE'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 3
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'CDIM1'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'CNT'
    laFStru[lnI,2] = 'N'
    laFStru[lnI,3] = 1
    laFStru[lnI,4] = 0

    *- This field is for refrence only, if style is changed use this to zap this file
    lnI = lnI + 1
    laFStru[lnI,1] = 'CSTYMAJOR'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = loFormSet.lnstylewid
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ1'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ2'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ3'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ4'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ5'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ6'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ7'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'SZ8'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 5
    laFStru[lnI,4] = 0

    *- The following are memo fields , each field saves 8 values , an information per size,
    *- methods are developed to get and put data to these fields

    lnI = lnI + 1
    laFStru[lnI,1] = 'CPRIMCLSS'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 8
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'CSECCLSS'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 8
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'CREMCLSS'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 8
    laFStru[lnI,4] = 0

    && Numeric fields, 4 places per size
    lnI = lnI + 1
    laFStru[lnI,1] = 'NREPTRIG'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 40
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'NREPQTY'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 40
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'NREPTOLR'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 40
    laFStru[lnI,4] = 0

    *- the following fields to hold the old value to be used in the screen editing
    lnI = lnI + 1
    laFStru[lnI,1] = 'CPRIMCLSS2'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 8
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'CSECCLSS2'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 8
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'CREMCLSS2'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 8
    laFStru[lnI,4] = 0

    && Numeric fields, 4 places per size
    lnI = lnI + 1
    laFStru[lnI,1] = 'NREPTRIG2'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 40
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'NREPQTY2'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 40
    laFStru[lnI,4] = 0

    lnI = lnI + 1
    laFStru[lnI,1] = 'NREPTOLR2'
    laFStru[lnI,2] = 'C'
    laFStru[lnI,3] = 40
    laFStru[lnI,4] = 0

    CREATE TABLE (oAriaApplication.WorkDir+lcTmpBnDat) FROM ARRAY laFStru
    INDEX ON SCALE TAG SCALE

  ENDIF

ENDIF

IF RECCOUNT(lcTmpBnDat) = 0

  LOCAL lcKey,lcScale
  IF loFormSet.llAllColors OR loFormSet.llAllScales
    lcKey = EVALUATE(KEY())
    lcScale = LEFT(loFormSet.Ariaform1.pgfStyleInfo.Page1.kbScale.Keytextbox.VALUE,loFormSet.lnScalelen)

    SELECT SCALE
    =SEEK('S'+lcScale)
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+lcScale

      SCATTER MEMVAR
      m.CSTYMAJOR = loFormSet.Ariaform1.kbStyleMajor.Keytextbox.VALUE
      *T20071102.0018,10/C200876 TMI 11/03/2008 [Start] create a seek variable
      PRIVATE lcSeekSty
      lcSeekSty = loFormSet.lcStyMajor
      *T20071102.0018,10/C200876 TMI 11/03/2008 [End  ]

      =lfUp_TmpBnDat()

    ENDSCAN
    =SEEK(lcKey)
  ELSE
    *T20071102.0018,10/C200876 TMI 05/29/2008 [Start] if not selecting all colors or all scales then seek to the specific line of this scale
    *lcScale = LEFT(loFormSet.Ariaform1.pgfStyleInfo.Page1.kbScale.Keytextbox.Value,loFormSet.lnScalelen)
    lcScale = loFormSet.Ariaform1.pgfStyleInfo.Page1.kbScale.Keytextbox.VALUE
    *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]

    SELECT SCALE
    =SEEK('S'+lcScale)

    SCATTER MEMVAR
    m.CSTYMAJOR = loFormSet.Ariaform1.kbStyleMajor.Keytextbox.VALUE

    *T20071102.0018,10/C200876 TMI 11/03/2008 [Start] create a seek variable
    PRIVATE lcSeekSty
    lcSeekSty = loFormSet.lcStyMajor+loFormSet.lcSepart+;
      loFormSet.Ariaform1.kbNonMajor.Keytextbox.VALUE
    *T20071102.0018,10/C200876 TMI 11/03/2008 [End  ]


    =lfUp_TmpBnDat()

  ENDIF
ENDIF

*- Close style, scale files if not opened before
IF llOpnStyle
  =gfCloseTable('STYLE')
ENDIF
IF llOpnScale
  =gfCloseTable('SCALE')
ENDIF

SELECT (lnSlct)
*-- end of lfCrTmpSzd.

*:**************************************************************************
*:* Name        : lfPOPMEMVR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : populate the memory variables of bin data from the style
*                 file to not be overwritten by the next gather next command
*                 in the ICSTYLE.SCX
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPOPMEMVR()
*:***************************************************************************
FUNCTION lfPOPMEMVR

LOCAL lcFldLst
lcFldLst = 'CPRIMCLSS, CSECCLSS, CREMCLSS, NREPTRIG, NREPQTY, NREPTOLR'
SCATTER FIELDS &lcFldLst MEMVAR MEMO

*-- end of lfPOPMEMVR.

*:**************************************************************************
*:* Name        : lfUp_TmpBnDat
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : apply the replace command to the file lcTmpBnDat
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUp_TmpBnDat()
*:***************************************************************************
FUNCTION lfUp_TmpBnDat

SELECT STYLE
*T20071102.0018,10/C200876 TMI 10/29/2008 [Start] get a line from the already saved colors to get the class information
*=gfSEEK(loFormSet.lcStyMajor)
*LOCATE REST WHILE STYLE = loFormSet.lcStyMajor FOR SCALE = SCALE.SCALE
=gfSEEK(lcSeekSty,'STYLE')
LOCATE REST WHILE STYLE = lcSeekSty FOR SCALE = SCALE.SCALE AND RECNO()>0
*T20071102.0018,10/C200876 TMI 10/29/2008 [End  ]

SELECT (lcTmpBnDat)
INSERT INTO (lcTmpBnDat) FROM MEMVAR
REPLACE CPRIMCLSS WITH IIF(loFormSet.ActiveMode = 'A',REPLICATE('A',SCALE.CNT)   ,STYLE.CPRIMCLSS );
  CSECCLSS  WITH IIF(loFormSet.ActiveMode = 'A',REPLICATE('A',SCALE.CNT)   ,STYLE.CSECCLSS  );
  CREMCLSS  WITH IIF(loFormSet.ActiveMode = 'A',REPLICATE('A',SCALE.CNT)   ,STYLE.CREMCLSS  );
  NREPTRIG  WITH IIF(loFormSet.ActiveMode = 'A',REPLICATE('   0|',SCALE.CNT),STYLE.NREPTRIG );
  NREPQTY   WITH IIF(loFormSet.ActiveMode = 'A',REPLICATE('   0|',SCALE.CNT),STYLE.NREPQTY  );
  NREPTOLR  WITH IIF(loFormSet.ActiveMode = 'A',REPLICATE('   0|',SCALE.CNT),STYLE.NREPTOLR )

*-- end of lfUp_TmpBnDat.

*:**************************************************************************
*:* Name        : lfGetSzData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Access the data in the memo fields for sizes
*:***************************************************************************
FUNCTION lfGetSzData

*** DO NOT REMOVE THIS FUNCTION ***

*-- end of lfGetSzData.

*:*************************************************************
*! Name        : lfDefnMnu
*! Developer   : Ahmed Salah (SSH)
*! Date        :05/16/2008
*! Module      : IC
*! Purpose     : Define Bin Locations valid
*:*************************************************************
FUNCTION lfDefnMnu

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL llFound
STORE .F. TO llFound
FOR lnCount = 1 TO CNTPAD(loFormSet.cHostFormName)
  IF PRMPAD(loFormSet.cHostFormName, GETPAD(loFormSet.cHostFormName, lnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF (loFormSet.chostformname) PROMPT "\<Options" KEY ALT+P , ' '
  ON PAD _Option OF (loFormSet.chostformname) ACTIVATE POPUP _OptionPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF

lnBarNo = 0
LOCAL lnI
FOR lnI = 1 TO CNTBAR(LEFT('_OPTIONPOP',10))
  IF 'Bins Detail' $ PRMBAR('_OPTIONPOP',lnI)
    lnBarNo = lnI
  ENDIF
ENDFOR
IF lnBarNo = 0
  lnBarNo = CNTBAR(LEFT('_OPTIONPOP',10)) + 1
ENDIF

DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bins Detail" ;
  SKIP FOR _SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'SA'

ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfBinData 

*T20071102.0018,10/C200876 TMI 07/01/2008 [Start] Add a new option to browse styles in a certain bin
DEFINE BAR lnBarNo+1 OF _OPTIONPOP PROMPT "\<Which styles are in a bin" ;
  SKIP FOR _SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'SA'
ON SELECTION BAR lnBarNo+1 OF _OPTIONPOP DO lfWhichStyInBin WITH _SCREEN.ACTIVEFORM.PARENT 
*T20071102.0018,10/C200876 TMI 07/01/2008 [End  ]

*!***************************************************************************
*!* Name        : lfBinData
*!* Developer   : Ahmed Salah
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Show the Bin Detail's window
*!***************************************************************************
*!* Called from : BN4MAIN.prg -->lfDEFNMNU()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfBinData()
*!***************************************************************************
*!
FUNCTION lfBinData

LOCAL lcOldAls
lcOldAls = SELECT(0)

DO FORM (oAriaApplication.ScreenHome+'IC\ICFLTHN.SCX') WITH _SCREEN.ACTIVEFORM.PARENT

*B610350,1 TMI 06/02/2013 [Start] close tables
=lfCloseTbl('WHBINLOC')
*B610350,1 TMI 06/02/2013 [End  ] 


SELECT(lcOldAls)

*-- End of Function lfBinData.

*:**************************************************************************
*:* Name        : lfWhichStyInBin
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/01/2008
*:* Purpose     : Look up which styles are in a bin
*:***************************************************************************
*:* Called from : BN4MAIN.PRG
*:***************************************************************************
FUNCTION lfWhichStyInBin
PARAMETERS loFormSet

LOCAL lcKey,lnSlct
lnSlct = SELECT(0)

*T20071102.0018,10/C200876 TMI 07/02/2008 [Start] do not use this way, instead use a refrence
*!*	lcSvSess = SET("Datasession")
*!*	SET DATASESSION TO (loFormSet.DataSessionID)
*T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

IF !USED('WHBINLOC')
  = gfOpenTable(oAriaApplication.DataDir + 'WHBINLOC', 'WHBINLOC', 'SH')
ENDIF
SELECT WHBINLOC
=gfSetOrder('WHBINLOC')
IF !USED('WHSLOC')
  = gfOpenTable(oAriaApplication.DataDir + 'WHSLOC', 'WHSLOC', 'SH')
ENDIF
SELECT WHSLOC
=gfSetOrder('WHSLOC')

lcKey = loFormSet.AriaForm1.kbLocation.Keytextbox.VALUE + loFormSet.ariaform1.grdBins.column1.text1.VALUE

IF !gfSeek(lcKey,'WHBINLOC')
  =gfModalGen('INM00000B00000','','','','No styles exists in the selected Bin.')
    *B610350,1 TMI 06/02/2013 [Start] close these tables
    =lfCloseTbl('WHBINLOC')
    *=lfCloseTbl('WHSLOC')    
    *B610350,1 TMI 06/02/2013 [End  ] 
  RETURN
ENDIF

lcBrFields = "Style,"
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  lcBrFields = lcBrFields + "Qty&lcI :H='Stk&lcI':P='999999',"
ENDFOR
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  lcBrFields = lcBrFields + "Alo&lcI :H='Alo&lcI':P='999999',"
ENDFOR
*bulk/pick flag, capacity, flat pack flag  and section
lcBrFields = lcBrFields + "CBLKPCK          :H='Bulk/Pick flag',"+;
  "WHSLOC.NCAPACITY :H='Capacity',      "+;
  "WHSLOC.CFLATHANG :H='flat pack flag',"+;
  "CSECTION         :H='Section'"

SELECT WHBINLOC
SET RELATION TO CWARECODE+CLOCATION INTO WHSLOC
LOCATE
=AriaBrow("'"+lcKey+"'",'Look up which styles are in a bin',.F.,.F.,.F.,.F.)

*T20071102.0018,10/C200876 TMI 07/02/2008 [Start] comment this line
*SET DATASESSION TO (lcSvSess )
*T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

    *B610350,1 TMI 06/02/2013 [Start] close these tables
    =lfCloseTbl('WHBINLOC')
    *=lfCloseTbl('WHSLOC')    
    *B610350,1 TMI 06/02/2013 [End  ] 
  
SELECT (lnSlct)
*-- end of lfWhichStyInBin. 

*:**************************************************************************
*:* Name        : lfUPWTHDEF
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : *- Define the default values for the new added bin
* CALLED FROM : ICLOCN , THE NEW BUTTONG THAT ADDS A NEW BINLOCATION ID
*:***************************************************************************
FUNCTION lfUPWTHDEF

LOCAL lnSlct
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

SELECT CurLocBins
REPLACE cFlatHang  WITH 'F' ;
  cBinClass  WITH 'A' ;
  cBlkPck    WITH 'B' ;
  CREPLENISH WITH 'N'

SELECT (lnSlct)
*-- end of lfUPWTHDEF  .

*:**************************************************************************
*:* Name        : lfCHKBNSTK
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Check if bin has a stock in whbinloc file before deleting it
*:***************************************************************************
*:* Called from : ICLOCN.REMOVE button
*:***************************************************************************
FUNCTION lfCHKBNSTK

LOCAL lnSlct,lcBin,llDelete
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

llDelete = .T.
lcKey = PADR(loFormSet.AriaForm1.kbLocation.Keytextbox.VALUE,6) + CurLocBins.CLOCATION
IF !USED('WHBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

SELECT WHBINLOC
=gfSeek(lcKey,'WHBINLOC')
SCAN REST WHILE CWARECODE+CLOCATION+STYLE = lcKey ;
    FOR QTY1<>0 .OR. QTY2<>0 .OR. QTY3<>0 .OR. QTY4<>0 .OR. QTY5<>0 .OR. QTY6<>0 .OR. QTY7<>0 .OR. QTY8<>0
  llDelete = .F.
  EXIT
ENDSCAN
IF !llDelete
  =gfModalGen('INM00000B00000','','','',"Can not remove, this bin contains non-zero stock for some styles.")
ENDIF

SELECT (lnSlct)
RETURN llDelete
*-- end of lfCHKBNSTK.
*:**************************************************************************
*:* Name        : lfCrtBinLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Create the temp files that will hold the bin Lines data
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrtBinLn()
*:***************************************************************************
FUNCTION lfCrtBinLn

lcBinLine = "_"+SUBSTR(ALIAS(),2)
=AFIELDS(laFileStru)
CREATE CURSOR (lcBinLine) FROM ARRAY laFileStru
INDEX ON PO + STYLE + STR(LINENO,6) TAG (lcBinLine)
*-- end of lfCrtBinLn.

*!***************************************************************************
*!* Name        : lfOpnFiles
*!* Developer   : Ahmed Salah
*!* Date        :05/16/2008
*!* Purpose     : Open the Needed files for Bin Location System and Check if
*!*             : Setting (Use Bin Location) is yes or no
*!***************************************************************************
*!* Called from : SMDLRBL.PRG
*!***************************************************************************
*!* Parameters  : lcFilesExp --> Files that need to open.
*!*             : lcTages    --> Tags for the opened files.
*!*             : lcCurPik   --> Current Piktkt.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOpnFiles()
*!***************************************************************************
FUNCTION lfOpnFiles
PARAMETERS lcFilesExp,lcTages,lcCurPik
*B608826,1 TMI 21/03/2009 [Start] save current alias
LOCAL lnSlct
lnSlct = SELECT(0)
*B608826,1 TMI 21/03/2009 [End  ]

DIMENSION laOpnFiles[1],laOpnTages[1]

=GFSUBSTR(lcFilesExp,@laOpnFiles,',')
=GFSUBSTR(lcTages,@laOpnTages,',')

FOR I=1 TO ALEN(laOpnFiles,1)
  IF !USED(laOpnFiles[I])
    =gfOpenTable(oAriaApplication.DataDir+laOpnFiles[I],laOpnTages[I],'SH')
  ELSE
    SELECT (laOpnFiles[I])
    =gfSetOrder(laOpnTages[I])
  ENDIF
ENDFOR
*B608826,1 TMI 21/03/2009 [Start] return to current alias
SELECT (lnSlct)
*B608826,1 TMI 21/03/2009 [End  ]
RETURN .T.
*-- End of Function lfOpnFiles.

*:**************************************************************************
*:* Name        : lfCSTFLDEF
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Update cBlkPck field with its default value
*:***************************************************************************
*:* Called from : ARCUST.SCX , when adding a new customer
*:***************************************************************************
FUNCTION lfCSTFLDEF
LOCAL lnFPos,lnSlct
lnSlct = SELECT(0)

IF loFormSet.Activemode = 'A'
  *- Initialize the array that holds user fields with the desired value
  lnFPos = ASCAN(loFormSet.laUserFields,'CBLKPCK ',1,ALEN(loFormSet.laUserFields,1),1)
  IF lnFPos>0

    lnFPos = ASUBSCRIPT(loFormSet.laUserFields,lnFPos,1)
    loFormSet.laUserFields[lnFPos,6] = 'B'

    SELECT CUSTOMER
    REPLACE CBLKPCK WITH 'B'

  ENDIF

ENDIF

SELECT (lnSlct)
*-- end of lfCSTFLDEF.

*:**************************************************************************
*:* Name        : lfCSTFLSAV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : check if the CBLKPCK field is filled in save process
*:***************************************************************************
*:* Called from : ARCUST.SCX, SAVING PROCESS
*:***************************************************************************
FUNCTION lfCSTFLSAV

NOTE :< TO BE DONE >:

*-- end of lfCSTFLSAV.

*:**************************************************************************
*:* Name        : lfORDBPDEF
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : OrdHdr cBlkPck default value , this value is copied from the customer
*                 default value and can be edited
*:***************************************************************************
*:* Called from : ORDHDR , when adding a new sales order
*:***************************************************************************
FUNCTION lfORDBPDEF

IF !lfIsUseBin()
  RETURN
ENDIF
*B609928,1 MMT 05/20/2012 Don't give message in case of setups blk/pick is set to NO[Start]
IF !gfGetMemVar('M_BULKPICK')
  RETURN .T.
ENDIF
*B609928,1 MMT 05/20/2012 Don't give message in case of setups blk/pick is set to NO[END]

LOCAL lnSlct
lnSlct = SELECT(0)

IF loFormSet.Activemode = 'A'

  *- Initialize the array that holds user fields with the desired value
  lnFPos = ASCAN(loFormSet.laUserFields,'CBLKPCK ',1,ALEN(loFormSet.laUserFields,1),1)
  IF lnFPos>0
    lnFPos = ASUBSCRIPT(loFormSet.laUserFields,lnFPos,1)
    loFormSet.laUserFields[lnFPos,6] = CUSTOMER.CBLKPCK

    SELECT (loFormSet.oFormEnvironment.lcOrdHdr)
    IF RECCOUNT()>0
      REPLACE CBLKPCK WITH CUSTOMER.CBLKPCK
    ENDIF

    *- add a check that if the customer.CBLKPCK field is empty then go out this customer
    IF !EMPTY(loFormSet.Ariaform1.keyAccount.Keytextbox.VALUE) .AND. ;
        EMPTY(loFormSet.laUserFields[lnFPos,6])
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The Bulk/Pick flag is empty for this customer, pls update it from the customers screen.')
      loFormSet.Ariaform1.keyAccount.keytextbox.VALUE = ''
      loFormSet.Ariaform1.keyAccount.TAG = '1'
      SELECT (lnSlct)
      RETURN .F.
    ENDIF
  ENDIF

ENDIF

SELECT (lnSlct)
*-- end of lfORDBPDEF.

*!***************************************************************************
*!* Name        : lfPORECPAD
*!* Developer   : Ahmed Salah
*!* Date        :05/16/2008
*!* Purpose     : Open the Needed files for Bin Location System and Check if
*!*             : Setting (Use Bin Location) is yes or no
*!***************************************************************************
*!* Called from : SMDLRBL.PRG
*!***************************************************************************
*!* Parameters  : lcFilesExp --> Files that need to open.
*!*             : lcTages    --> Tags for the opened files.
*!*             : lcCurPik   --> Current Piktkt.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOpnFiles()
*!***************************************************************************
*!
FUNCTION lfPORECPAD

IF lfIsUseBin()
  *--check if the option pad is already defined on the sysmenu
  LOCAL llFound
  STORE .F. TO llFound
  FOR lnCount = 1 TO CNTPAD(loFormSet.cHostFormName)
    IF PRMPAD(loFormSet.cHostFormName, GETPAD(loFormSet.cHostFormName, lnCount)) = 'Options'
      llfound = .T.
      EXIT
    ENDIF
  ENDFOR
  IF !llfound
    DEFINE PAD _Option OF (loFormSet.chostformname) PROMPT "\<Options" KEY ALT+P , ' '
    ON PAD _Option OF (loFormSet.chostformname) ACTIVATE POPUP _OptionPOP
    DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  ENDIF

  lnBarNo = CNTBAR(LEFT('_OPTIONPOP',10)) + 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Receive into Multiple Bins" SKIP FOR ;
    _SCREEN.ACTIVEFORM.PARENT.ActiveMode = 'S' .OR. ;
    EMPTY(_SCREEN.ACTIVEFORM.PARENT.AriaForm1.KbPONo.Keytextbox.VALUE)

  *t20070214.0006   releasing the conflect
  *ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispScr
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispScr IN BN4MAIN.FXP
  *t20070214.0006

ENDIF


*:**************************************************************************
*:* Name        : lfDispScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     :
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDispScr()
*:***************************************************************************
FUNCTION lfDispScr

*B609532,1 TMI 02/19/2011 [Start] if this is a cancel only mode then do not open this screen
IF TYPE('_screen.ActiveForm.parent.llCancelOnly')='L' AND _SCREEN.ACTIVEFORM.PARENT.llCancelOnly = .T.
  RETURN .F.
ENDIF
*B609532,1 TMI 02/19/2011 [End  ]

DO FORM (oAriaApplication.ScreenHome+'PO\POSHPBN.SCX') WITH _SCREEN.ACTIVEFORM.PARENT


*!*************************************************************
*! Name        : lfADDFILDS
*! Developer   : Ahmed Salah - (SSH)
*! Date        :05/16/2008
*! Module      : PO
*! Purpose     : Add Custom Fields for temp file
*!*************************************************************
FUNCTION lfADDFILDS

IF !lfIsUseBin()
  RETURN
ENDIF

lnFStru = ALEN(laFStru,1)

DIMENSION laFStru[lnFStru+8,18]

laFStru[lnFStru+1,1] = 'Loc1'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 10
laFStru[lnFStru+1,4] = 0

laFStru[lnFStru+2,1] = 'Loc2'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 10
laFStru[lnFStru+2,4] = 0

laFStru[lnFStru+3,1] = 'Loc3'
laFStru[lnFStru+3,2] = 'C'
laFStru[lnFStru+3,3] = 10
laFStru[lnFStru+3,4] = 0

laFStru[lnFStru+4,1] = 'Loc4'
laFStru[lnFStru+4,2] = 'C'
laFStru[lnFStru+4,3] = 10
laFStru[lnFStru+4,4] = 0

laFStru[lnFStru+5,1] = 'Loc5'
laFStru[lnFStru+5,2] = 'C'
laFStru[lnFStru+5,3] = 10
laFStru[lnFStru+5,4] = 0

laFStru[lnFStru+6,1] = 'Loc6'
laFStru[lnFStru+6,2] = 'C'
laFStru[lnFStru+6,3] = 10
laFStru[lnFStru+6,4] = 0

laFStru[lnFStru+7,1] = 'Loc7'
laFStru[lnFStru+7,2] = 'C'
laFStru[lnFStru+7,3] = 10
laFStru[lnFStru+7,4] = 0

laFStru[lnFStru+8,1] = 'Loc8'
laFStru[lnFStru+8,2] = 'C'
laFStru[lnFStru+8,3] = 10
laFStru[lnFStru+8,4] = 0


FOR lnI = 7 TO 16
  FOR lnJ = 1 TO 8
    STORE '' TO laFStru[lnFStru+lnJ,lnI]
  ENDFOR
ENDFOR
FOR lnJ = 1 TO 8
  STORE 0 TO laFStru[lnFStru+lnJ,17],laFStru[lnFStru+lnJ,18]
ENDFOR

*:**************************************************************************
*:* Name        : lfDLCHKSAV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Check before saving the bins data
*:***************************************************************************
*:* Called from : POSREC.SCX when save is clicked
*:***************************************************************************
FUNCTION lfDLCHKSAV

IF !lfIsUseBin()
  RETURN
ENDIF
*B610160,1 SAB 11/28/2012 Hide Assing bin Message when receiving Material PO [Start]
IF loFormSet.lcInvType = "0002"
  RETURN
ENDIF
*B610160,1 SAB 11/28/2012 Hide Assing bin Message when receiving Material PO [End]

STORE .F. TO llmessag , llChckLoc , llChckCls , llNotAssgn
llReturn = .T.
lcWareCode=SPACE(6)
lcTmpLine = loFormSet.lcTmpLine
lcBinLine = "_"+SUBSTR(lcTmpLine,2)

DIMENSION laStyArr[1]
STORE '' TO laStyArr
SELECT (lcTmpLine)
LOCAL lcFilt
lcFilt = FILTER()
SET FILTER TO
lcOldOrder = ORDER()
SET ORDER TO POSLN
LOCATE

PRIVATE lcStyLocs
lcStyLocs = ''

*- be sure the STYLE file is open
IF !USED('STYLE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF

*B609651,1 MMT 07/26/2011 Receiving program assign wrong Bins to lines in case of receiving to more than one loc.[START]
llMoreThanloc = .F.
SELECT DISTINCT cWareCode FROM (lcTmpLine) WHERE  TRANCD $'1'+IIF(loFormSet.llMFCall,'3','4') INTO CURSOR 'LocCnt'
IF RECCOUNT('LocCnt') > 1
  llMoreThanloc = .T.
ENDIF
*B609651,1 MMT 07/26/2011 Receiving program assign wrong Bins to lines in case of receiving to more than one loc.[END]

*B609532,1 TMI 02/19/2011 [Start] check if this is a cancel only mode, if so proceed as the standard
SELECT (lcTmpLine)
LOCATE FOR TRANCD = '5'
IF FOUND()
  LOCATE FOR TRANCD = '2'
  IF !FOUND()
    RETURN .T.
  ENDIF
  LOCATE
ENDIF
*B609532,1 TMI 02/19/2011 [End  ]

*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
IF loFormSet.lcPType == 'N' AND loFormSet.lcInvType == "0001"
  *- Call the function that fill lcBinLine temp file in Inter Location case
  *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [Start]
  *=lfIntLcBn()
  *RETURN .T.
  *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
  *RETURN lfIntLcBn()
  llReturn = lfIntLcBn()
  SELECT (lcTmpLine)
  SET ORDER TO &lcOldOrder
  *- reset the filter
  SET FILTER TO &lcFilt
  LOCATE
  RETURN llReturn
  *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[END]
  *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [End]
ENDIF
*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

*C201454,1 SAB 01/17/2012 Enable Receiving to bin location in Inter Location PO case [Start]
LOCAL lcccType
DO CASE
CASE loFormSet.lcPType = 'O'
  lcccType = 'N'
OTHERWISE
  lcccType = 'P'
ENDCASE
*C201454,1 SAB 01/17/2012 Enable Receiving to bin location in Inter Location PO case [End]

SCAN FOR TRANCD $'1'+IIF(loFormSet.llMFCall,'3','4')
  PRIVATE lcSeekSty
  lcSeekSty = IIF(TRANCD='1',&lcTmpLine..STYLE,&lcTmpLine..cRetSty)
  IF SEEK(&lcTmpLine..PO + lcSeekSty + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
    SELECT(lcBinLine)
    *!B610192,1 HIA 01/13/2013 PO- cannot change location on receiving inter location po [T20121219.0013][Start]
    *SCAN REST WHILE PO + STYLE + STR(LINENO,6) = &lcTmpLine..PO + lcSeekSty +STR(&lcTmpLine..LINENO,6)
    llRecFound = .F.
    SCAN REST WHILE PO + STYLE + STR(LINENO,6) = &lcTmpLine..PO + lcSeekSty +STR(&lcTmpLine..LINENO,6) FOR cWarecode= &lcTmpLine..cWarecode
      llRecFound = .T.
      *!B610192,1 HIA 01/13/2013 PO- cannot change location on receiving inter location po [T20121219.0013][End]
      m.loc1 = &lcBinLine..loc1
      m.loc2 = &lcBinLine..loc2
      m.loc3 = &lcBinLine..loc3
      m.loc4 = &lcBinLine..loc4
      m.loc5 = &lcBinLine..loc5
      m.loc6 = &lcBinLine..loc6
      m.loc7 = &lcBinLine..loc7
      m.loc8 = &lcBinLine..loc8
      FOR Lni = 1 TO 8
        Lci = ALLT(STR(Lni,2))
        IF &lcBinLine..Qty&lcI<>0 AND EMPTY(m.Loc&LcI)
          llChckLoc = .T.
          IF !(lcSeekSty $ lcStyLocs)
            lcStyLocs = lcStyLocs + lcSeekSty + CHR(13)
          ENDIF
          =SEEK(lcSeekSty,'STYLE','Style')
          IF EMPTY(laStyArr[1])
            laStyArr[1] = STYLE.cflathang
          ELSE
            IF !(laStyArr[1] = STYLE.cflathang)
              STORE .F. TO llChckCls ,llReturn
              lcWareCode=&lcTmpLine..cWareCode
              EXIT
            ENDIF
          ENDIF
        ENDIF
      ENDFOR
      lcWareCode=&lcTmpLine..cWareCode
    ENDSCAN
    *!B610192,1 HIA 01/13/2013 PO- cannot change location on receiving inter location po [T20121219.0013][Start]
    IF !llRecFound
      IF !(lcSeekSty $ lcStyLocs)
        lcStyLocs = lcStyLocs + lcSeekSty + CHR(13)
        STORE .F. TO llReturn
        llNotAssgn = .T.
      ENDIF
    ENDIF
    *!B610192,1 HIA 01/13/2013 PO- cannot change location on receiving inter location po [T20121219.0013][End]
  ELSE
    SCATTER MEMVAR MEMO
    lcWareCode=m.cWareCode
    INSERT INTO (lcBinLine) FROM MEMVAR
    REPLACE &lcBinLine..STYLE WITH lcSeekSty
    SELECT &lcBinLine
    IF ((Qty1<>0 .AND. EMPTY(Loc1)) .OR. ;
        (Qty2<>0 .AND. EMPTY(Loc2)) .OR. ;
        (Qty3<>0 .AND. EMPTY(Loc3)) .OR. ;
        (Qty4<>0 .AND. EMPTY(Loc4)) .OR. ;
        (Qty5<>0 .AND. EMPTY(Loc5)) .OR. ;
        (Qty6<>0 .AND. EMPTY(Loc6)) .OR. ;
        (Qty7<>0 .AND. EMPTY(Loc7)) .OR. ;
        (Qty8<>0 .AND. EMPTY(Loc8))) .AND. !lcSeekSty $ lcStyLocs
      lcStyLocs = lcStyLocs + lcSeekSty + CHR(13)
      STORE .F. TO llReturn
      llNotAssgn = .T.
    ENDIF
    *- if line is not added before to the lcBinLine then it is also not in lcTmpLine, then add it with TranCd = '2'
    LOCAL lnRecNo
    lnRecNo = RECNO(lcTmpLine)
    SELECT &lcTmpLine
    SCATTER MEMVAR MEMO
    *C201454,1 SAB 01/17/2012 Enable Receiving to bin location in Inter Location PO case [Start]
    *IF !SEEK('P'+PO+STYLE+STR(LINENO,6)+'2',lcTmpLine)
    IF !SEEK(lcccType+PO+STYLE+STR(LINENO,6)+'2',lcTmpLine)
      *C201454,1 SAB 01/17/2012 Enable Receiving to bin location in Inter Location PO case [End]
      m.TRANCD = '2'
      INSERT INTO (lcTmpLine) FROM MEMVAR
    ENDIF
    GOTO (lnRecNo)
  ENDIF
ENDSCAN

SELECT (lcBinLine)
LOCATE
IF llChckCls
  lcMsg2 = 'You have mixed Flat and Hanging styles on this transaction - please enter the bin locations manually'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llReturn = .F.
  STORE .F. TO llShow,llCSave
  SELECT (lcTmpLine)
  SET ORDER TO &lcOldOrder
  *- reset the filter
  SET FILTER TO &lcFilt
  LOCATE
  RETURN llReturn
ENDIF

*B609651,1 MMT 07/26/2011 Receiving program assign wrong Bins to lines in case of receiving to more than one loc.[START]
IF (llChckLoc OR llNotAssgn) AND llMoreThanloc
  lcMsg2 = 'There is more than one warehouse assigned to this shipment receipt. Receiving must be done using Receive into multiple bins'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llReturn = .F.
  STORE .F. TO llShow,llCSave
  SELECT (lcTmpLine)
  SET ORDER TO &lcOldOrder
  *- reset the filter
  SET FILTER TO &lcFilt
  LOCATE
  RETURN llReturn
ENDIF
*B609651,1 MMT 07/26/2011 Receiving program assign wrong Bins to lines in case of receiving to more than one loc.[END]

IF llChckLoc OR llNotAssgn
  lcMsg2 = "There is a line or more that didn't assign to bins, you have to assign bin location before saving." + ;
    " Do you want to assign bin location now to not assigned Quantities?"
  lcLogMsg  = lcMsg2 + CHR(13) + CHR(13) + ;
    "Style(s) with missing locations:"+CHR(13)+;
    lcStyLocs
  *llOk = .F.
  lnOk = 0
  lcLogTtl = 'Missing Locations'
  DO FORM (oAriaApplication.ScreenHome+'SMMSG.SCX') WITH loFormSet,lcLogMsg

  *lnChoose = IIF(llOk,1,2)
  lnChoose = lnOk

  IF lnChoose = 1
    IF !USED('WHSLOC')
      =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    IF !USED('STYLE')
      =gfOpenTable(oAriaapplication.datadir+'style','STYLE','SH')
    ENDIF
    DIME laTempData[3]
    STORE '' TO lcBinLoc , laTempData

    DO FORM (oAriaApplication.ScreenHome+'PO\POdlbnSC.SCX') WITH lcWareCode

    llReturn = .T.
    IF EMPTY(lcBinLoc)
      lcMsg2 = 'You have to assign bin location before saving .'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llReturn = .F.
      STORE .F. TO llShow,llCSave
      SELECT (lcTmpLine)
      SET FILTER TO &lcFilt
      SET ORDER TO &lcOldOrder
      LOCATE
      RETURN llReturn
    ENDIF

    SELECT (lcBinLine)
    SCAN FOR llReturn
      =SEEK(&lcBinLine..STYLE,'Style')
      LOCAL lnNo
      FOR lnNo = 1 TO 8
        lcI = STR(lnNo,1)
        IF &lcBinLine..QTY&lcI <> 0
          llContinue = !EMPTY(laTempData[2])
          IF llContinue
            llContinue = PADR(laTempData[2],1) $ PADR(SUBSTR(STYLE.cPrimClss,lnNo,1),1)+;
              PADR(SUBSTR(STYLE.cSecClss,lnNo,1),1)+;
              PADR(SUBSTR(STYLE.cRemClss,lnNo,1),1)
          ENDIF
          IF !llContinue
            *--You cannot transfer from the same warehouse.
            lcMsg2 = 'The style ' + STYLE.STYLE + ' size &lcI has bin class is different from ' + laTempData[2] +' of the bin ' + laTempData[1]
            =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
            *  reset the filter
            SELECT (lcTmpLine)
            SET ORDER TO &lcOldOrder
            SET FILTER TO &lcFilt
            LOCATE
            RETURN .F.
          ENDIF
        ENDIF
      ENDFOR
      IF !(laTempData[3] = STYLE.cflathang)
        *--'You cannot saving the style to a bin of a different Hang/Flat.'
        lcMsg2 = 'You cannot saving the style to a bin of a different Hang/Flat.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        STORE .F. TO llShow,llCSave,llReturn
        SELECT (lcTmpLine)
        SET ORDER TO &lcOldOrder
        SET FILTER TO &lcFilt
        LOCATE
        RETURN llReturn
      ENDIF
      FOR I = 1 TO 8
        LCI = ALLTRIM(STR(I))
        IF EMPTY(&lcBinLine..LOC&LCI) AND &lcBinLine..QTY&LCI > 0
          REPLACE &lcBinLine..LOC&LCI  WITH laTempData[1]
        ENDIF
      ENDFOR
      SCATTER MEMVAR MEMO
      *C201454,1 SAB 01/17/2012 Enable Receiving to bin location in Inter Location PO case [Start]
      *=SEEK('P'+PO+STYLE+STR(LINENO,6)+'2',lcTmpLine)
      =SEEK(lcccType+PO+STYLE+STR(LINENO,6)+'2',lcTmpLine)
      *C201454,1 SAB 01/17/2012 Enable Receiving to bin location in Inter Location PO case [End]
      SELECT &lcTmpLine
      GATHER FIELDS LOC1,LOC2,LOC3,LOC4,LOC5,LOC6,LOC7,LOC8 MEMVAR
    ENDSCAN
    SELECT (lcTmpLine)
    SET ORDER TO &lcOldOrder
    *- Reset the filter
    SET FILTER TO &lcFilt
    LOCATE
    RETURN llReturn
  ELSE
    *--mhm
    IF lnChoose = 2
      *--mhm
      STORE .F. TO llShow,llCSave,llReturn
      SELECT (lcTmpLine)
      SET ORDER TO &lcOldOrder
      *- reset the filter
      SET FILTER TO &lcFilt
      LOCATE
      RETURN llReturn
      *--mhm
    ELSE

      STORE .F. TO llShow,llCSave
      SELECT (lcTmpLine)
      *T20071102.0018,10/C200876 TMI 06/29/2008 [Start] loop for all lines
      *SCAN FOR TranCD ='2'
      *IF !SEEK(&lcTmpLine..PO + &lcTmpLine..Style + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
      *    DELETE
      SCAN
        =SEEK(&lcTmpLine..PO + &lcTmpLine..STYLE + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
        SELECT &lcBinLine
        IF EMPTY(LOC1) AND EMPTY(LOC2) AND EMPTY(LOC3) AND EMPTY(LOC4) AND ;
            EMPTY(LOC5) AND EMPTY(LOC6) AND EMPTY(LOC7) AND EMPTY(LOC8)
          SELECT (lcTmpLine)
          DELETE
        ENDIF
      ENDSCAN
      *T20071102.0018,10/C200876 TMI 06/29/2008 [End  ]


      *T20071102.0018,10/C200876 TMI 06/29/2008 [Start] no need for this code , we just need to remove the lines that are not updated in lcBinLine temp cursor
      *!*		      ELSE
      *!*		        SELECT(lcBinLine)
      *!*		        STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
      *!*				SCAN REST WHILE PO + STYLE + STR(LINENO,6) = &lcTmpLine..PO + lcSeekSty +STR(&lcTmpLine..LINENO,6)
      *!*				      m.Qty1 = &lcBinLine..Qty1 + m.Qty1
      *!*				      m.Qty2 = &lcBinLine..Qty2 + m.Qty1
      *!*				      m.Qty3 = &lcBinLine..Qty3 + m.Qty1
      *!*				      m.Qty4 = &lcBinLine..Qty4 + m.Qty1
      *!*				      m.Qty5 = &lcBinLine..Qty5 + m.Qty1
      *!*				      m.Qty6 = &lcBinLine..Qty6 + m.Qty1
      *!*				      m.Qty7 = &lcBinLine..Qty7 + m.Qty1
      *!*				      m.Qty8 = &lcBinLine..Qty8 + m.Qty1
      *!*				      m.TotQty = m.TotQty+ m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
      *!*		        ENDSCAN
      *!*		        =SEEK(&lcTmpLine..PO + &lcTmpLine..Style + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
      *!*			    SCATTER MEMVAR MEMO
      *!*			    =SEEK('P'+PO+STYLE+STR(LINENO,6)+'2',lcTmpLine)
      *!*			    SELECT &lcTmpLine
      *!*			    GATHER FIELDS LOC1,LOC2,LOC3,LOC4,LOC5,LOC6,LOC7,LOC8 MEMVAR
      *!*		        SELECT (lcTmpLine)
      *!*		        REPLACE Qty1 WITH m.Qty1
      *!*		        REPLACE Qty2 WITH m.Qty2
      *!*		        REPLACE Qty3 WITH m.Qty3
      *!*		        REPLACE Qty4 WITH m.Qty4
      *!*		        REPLACE Qty5 WITH m.Qty5
      *!*		        REPLACE Qty6 WITH m.Qty6
      *!*		        REPLACE Qty7 WITH m.Qty7
      *!*		        REPLACE Qty8 WITH m.Qty8
      *!*		      ENDIF
      *!*		  ENDSCAN
      *!*	      SELECT (lcTmpLine)
      *!*		  SCAN
      *!*		    If TranCD ='2'
      *!*			    IF EMPTY(loc1) and EMPTY(loc2) and EMPTY(loc3) and EMPTY(loc4) and EMPTY(loc5) and EMPTY(loc6) and EMPTY(loc7) and EMPTY(loc8)
      *!*			      SKIP -1
      *!*			      DELETE
      *!*			      SKIP
      *!*			      DELETE
      *!*			    ENDIF
      *!*		    Endif
      *!*		  Endscan
      *T20071102.0018,10/C200876 TMI 06/29/2008 [End  ]

      SELECT (lcTmpLine)
      SET ORDER TO &lcOldOrder
      *- reset the filter
      SET FILTER TO &lcFilt
      LOCATE

      RETURN .T.

    ENDIF
    *--mhm

  ENDIF
ENDIF

SELECT (lcTmpLine)
SET ORDER TO &lcOldOrder
SET FILTER TO &lcFilt
LOCATE
RETURN llReturn

*!***************************************************************************
*!* Name        : lfDLSBNPOR
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Update BininvJL from Styinvjl
*!***************************************************************************
*!* Called from : Poupdate.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSBNPOR()
*!***************************************************************************
FUNCTION lfDLSBNPOR

IF !lfIsUseBin()
  RETURN .F.
ENDIF

PRIVATE lcStyOrd,lnAlias,lcCurTmp
lnAlias   = SELECT(0)

lcTmpLine = ALIAS()
lcBinLine = "_"+SUBSTR(lcTmpLine,2)
lnGetRec  = RECNO()
lnCurLin  = LINENO
lcStybn   = IIF(!EMPTY(&lcTmpLine..STYLE),&lcTmpLine..STYLE,m.STYLE)

lcStybn = IIF(!EMPTY(&lcTmpLine..cRetSty),&lcTmpLine..cRetSty,lcStybn)
lcPobn    = IIF(!EMPTY(&lcTmpLine..PO),&lcTmpLine..Po,m.Po)

*B610066,2 MMT 09/12/2012 don't add Records to Bin Temp. in Automatic/Manual style selection if location field is empty[Start]
*B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
IF (TYPE('lcRecvType')='C' AND !EMPTY(lcRecvType) AND lcRecvType ="N") AND lcInvType = "0001"
  lcWareHouseCode = IIF(!EMPTY(&lcTmpLine..Vendor),SUBSTR(&lcTmpLine..Vendor,1,6),m.cWareCode)
ELSE
  *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[END]
  lcWareHouseCode = IIF(!EMPTY(&lcTmpLine..cWareCode),&lcTmpLine..cWareCode,m.cWareCode)
  *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
ENDIF
*B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[END]
*B610066,2 MMT 09/12/2012 don't add Records to Bin Temp. in Automatic/Manual style selection if location field is empty[END]


*B610048,1 MMT 08/16/2012 Style WIP/Intrans Qtys are not update while receiving shipments if Bin Location is used[Start]
IF USED('STYLE')
  SELECT STYLE
  =gfTableUpdate()
ENDIF
*B610048,1 MMT 08/16/2012 Style WIP/Intrans Qtys are not update while receiving shipments if Bin Location is used[End]

=lfOpnFiles("STYINVJL,BININVJL,WHBINLOC,WHSLOC,Style","STYINVJL,STYINVJL,WHBINLOC,WHSLOC,Style","")

*- put each bin in a separate line
lcCurTmp = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+ lcCurTmp);
  (nstk1 N(7),nstk2 N(7),nstk3 N(7),nstk4 N(7),;
  nstk5 N(7),nstk6 N(7),nstk7 N(7),nstk8 N(7),;
  nTotstk N(7),Binloc C(10))
INDEX ON Binloc TAG (lcCurTmp)

=SEEK(lcPobn+lcStybn+STR(lnCurLin,6),lcBinLine)
SELECT(lcBinLine)
*B610066,2 MMT 09/12/2012 don't add Records to Bin Temp. in Automatic/Manual style selection if location field is empty[Start]
*SCAN REST WHILE PO + STYLE + STR(Lineno,6) = lcPobn+lcStybn+STR(lnCurLin,6)
SCAN REST WHILE PO + STYLE + STR(LINENO,6) = lcPobn+lcStybn+STR(lnCurLin,6) FOR cwarecode =lcWareHouseCode
  *B610066,2 MMT 09/12/2012 don't add Records to Bin Temp. in Automatic/Manual style selection if location field is empty[End]
  SELECT (lcCurTmp)
  FOR N = 1 TO 8
    lcN = ALLTRIM(STR(N,1))
    IF &lcBinLine..Qty&LCN<> 0
      IF !SEEK(&lcBinLine..LOC&lcN,lcCurTmp)
        APPEND BLANK
        REPLACE nstk&LCN WITH &lcBinLine..Qty&lcN ;
          nTotstk  WITH &lcBinLine..Qty&lcN ;
          Binloc   WITH &lcBinLine..loc&lcN
      ELSE
        REPLACE nstk&LCN WITH &lcBinLine..Qty&LCN + nstk&LCN ,;
          nTotstk  WITH nTotstk + &lcBiNLine..Qty&LCN
      ENDIF
    ENDIF
  ENDFOR
ENDSCAN

=SEEK(lcPobn+lcStybn+STR(lnCurLin,6),lcBinLine)
*B610066,3 MMT 10/03/2012 Incorrect records added to WHBINLOC table[T20120816.0012][Start]
SELECT(lcBinLine)
LOCATE REST WHILE PO + STYLE + STR(LINENO,6) = lcPobn+lcStybn+STR(lnCurLin,6) FOR cwarecode =lcWareHouseCode
*B610066,3 MMT 10/03/2012 Incorrect records added to WHBINLOC table[T20120816.0012][End]
SELECT (lcCurTmp)
LOCATE
IF EOF()
  SELECT(lnAlias)
  IF BETWEEN(lnGetRec,1,RECCOUNT())
    GOTO lnGetRec
  ENDIF
  IF USED(lcCurTmp)
    USE IN &lcCurTmp
  ENDIF
  *--Erase the lcCurTmp file.
  =lfEraseFil(lcCurTmp)
  RETURN
ENDIF

SELECT STYINVJL
*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
*<tmi interlocation PO>[start] comment and update
*=SEEK(lcStybn + m.cwarecode + lcGlSession)
lnSvLine = M.LINENO
lcSvWH   = m.cWareCode
IF lcRecvType = 'N' AND lcInvType = "0001"
  m.cWareCode = PADR(m.Vendor,6)
  m.LineNo = 0
ENDIF
*<tmi interlocation PO>[End] comment and update
*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

*B610685,1 TMI 03/11/2014 19:13 [Start] use lcWareHouseCode instead of m.cwarecode
*=SEEK(lcStybn + m.cwarecode + lcGlSession)
=SEEK(lcStybn + lcWareHouseCode + lcGlSession)
*B610685,1 TMI 03/11/2014 19:13 [End  ] 

*B610685,1 TMI 03/11/2014 19:16 [Start]  use lcWareHouseCode instead of m.cwarecode
*SCAN REST WHILE STYLE+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(LINENO,6) = lcStybn + m.cwarecode + lcGlSession
SCAN REST WHILE STYLE+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(LINENO,6) = lcStybn + lcWareHouseCode + lcGlSession
  *B610685,1 TMI 03/11/2014 19:16 [End  ] 
  IF (dtrdate = ldTrDate) AND (ctrcode = lcPobn) AND (LINENO = m.lineno)
    EXIT
  ENDIF
ENDSCAN
SCATTER MEMVAR MEMO

*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
*<tmi interlocation PO>[start] restore the m.LineNo to use it in the bininvjl
IF lcRecvType = 'N' AND lcInvType = "0001"
  m.LineNo = lnSvLine
ENDIF
*<tmi interlocation PO>[End  ]
*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

*-  Prepare the bininvjl cursor
=gfSEEK(lcStybn+m.cwarecode+m.csession+DTOS(m.dtrdate)+m.Ctrcode+STR(m.lineno,6),'BININVJL')

SELECT (lcCurTmp)
SCAN
  SCATT MEMVAR MEMO
  m.cLocation = m.Binloc
  m.nStkval   = m.nTotstk * m.nCost
  SELECT BININVJL
  IF !SEEK(lcStybn+m.cwarecode+m.csession+DTOS(m.dtrdate)+m.Ctrcode+STR(m.lineno,6),'BININVJL')
    =gfAppend('BININVJL',.T.)
  ELSE

    *- This else part is strange , I think it is not entered, it may be removed
    llBinFound = .F.
    SCAN REST WHILE STYLE+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(LINENO,6) = ;
        m.STYLE+m.cwarecode+m.csession+DTOS(m.dtrdate)+m.Ctrcode+STR(m.lineno,6) ;
        FOR ALLTRIM(cLocation) = ALLTRIM(m.clocation)
      llBinFound = .T.
    ENDSCAN
    IF !llBinFound   && Note : this part of code may not be reached
      =gfAppend('BININVJL',.T.)
    ENDIF
  ENDIF
  llBinFound = .F.

  *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [Start]
  =gfAdd_Info('BININVJL')
  *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [End]

  =SEEK(&lcBinLine..cWareCode+&lcCurTmp..Binloc,'WHSLOC')
  SELECT WHSLOC
  *-- populate only the needed memvars , do not repeat populating m.Location field
  SCATTER FIELDS CBINCLASS, CBLKPCK, CSECTION MEMVAR
  IF !SEEK(&lcBinLine..cWareCode+&lcCurTmp..Binloc+&lcBinLine..STYLE,'WHSLOC')
    INSERT INTO WHSLOC FROM MEMVAR
  ENDIF

  IF !gfSEEK(&lcBinLine..cWareCode+&lcCurTmp..Binloc+&lcBinLine..STYLE,'WHBINLOC')
    lcRepl = 'Style      WITH [' + &lcBinLine..STYLE     + '] ' +;
      'cWareCode  WITH [' + &lcBinLine..cWareCode + '] ' +;
      'clocation  WITH [' + &lcCurTmp..Binloc     + '] ' +;
      'cbinclass  WITH [' + m.cbinclass           + '] ' +;
      'cBlkPck    WITH [' + m.cBlkPck             + '] ' +;
      'cSection   WITH [' + m.cSection            + '] ' +;
      'cAdd_User  WITH [' + oAriaApplication.User_ID  + '] '+;
      'dAdd_Date  WITH {^'+STR(YEAR(DATE()),4)+'-'+STR(MONTH(DATE()),2)+'-'+STR(DAY(DATE()),2) +'} '+;
      'cAdd_Time  WITH [' + gfGetTime()           + '] '

    SELECT WHBINLOC
    =gfAppend()
    =gfReplace(lcRepl)
  ENDIF
  SELECT WHBINLOC
  lcRepl = 'Qty1    WITH  ' + STR( Qty1+&lcCurTmp..nStk1 ) + ' '+;
    'Qty2    WITH  ' + STR( Qty2+&lcCurTmp..nStk2 ) + ' '+;
    'Qty3    WITH  ' + STR( Qty3+&lcCurTmp..nStk3 ) + ' '+;
    'Qty4    WITH  ' + STR( Qty4+&lcCurTmp..nStk4 ) + ' '+;
    'Qty5    WITH  ' + STR( Qty5+&lcCurTmp..nStk5 ) + ' '+;
    'Qty6    WITH  ' + STR( Qty6+&lcCurTmp..nStk6 ) + ' '+;
    'Qty7    WITH  ' + STR( Qty7+&lcCurTmp..nStk7 ) + ' '+;
    'Qty8    WITH  ' + STR( Qty8+&lcCurTmp..nStk8 )
  =gfReplace(lcRepl)

  lcRepl = 'TOTQTY  WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8'
  =gfReplace(lcRepl)

  SELECT WHBINLOC
  llSuccessUpd = gfTableUpdate()
  =lfErrLogRecord(llSuccessUpd)

ENDSCAN

SELECT BININVJL
llSuccessUpd = gfTableUpdate()
=lfErrLogRecord(llSuccessUpd)

IF USED(lcCurTmp)
  USE IN &lcCurTmp
ENDIF
*--Erase the lcCurTmp file.
=lfEraseFil(lcCurTmp)

SELECT(lnAlias)
IF BETWEEN(lnGetRec,1,RECCOUNT())
  GOTO lnGetRec
ENDIF

*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
*<tmi interlocation PO> restore the wh code [start]
IF lcRecvType = 'N' AND lcInvType = "0001"
  m.cWareCode = lcSvWH
ENDIF
*<tmi interlocation PO> restore the wh code [end  ]
*E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

*-- End of Function lfDLSBNPOR.


*:**************************************************************************
*:* Name        : lfEraseFil
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : erasing a temp file
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfEraseFil()
*:***************************************************************************
FUNCTION lfEraseFil
PARAMETERS lcFile

IF USED(lcFile)
  USE IN (lcFile)
ENDIF
ERASE (oAriaApplication.WorkDir+lcFile+".DBF")
ERASE (oAriaApplication.WorkDir+lcFile+".CDX")
ERASE (oAriaApplication.WorkDir+lcFile+".FPT")
*-- end of lfEraseFil.

*:**************************************************************************
*:* Name        : lfDlModBrw
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : adjust the browse grid in the ICINVTA.SCX screen
*:***************************************************************************
*:* Called from : ICINVTA.SCX
*:***************************************************************************
FUNCTION lfDlModBrw

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnSlct,lcFileNama,loBrowObj,lcI
lnSlct = SELECT(0)

*- Add new fields to the temp file lcTempAdj to save binlocation information
SELECT TempAdj
LOCAL lcDbf,laFStru,lnI
DIMENSION laFStru[1,18]
lcDbf = DBF()
lnFStru = AFIELDS(laFStru)
*USE

DIMENSION laFStru[lnFStru+8,18]
lnI = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'WareHFr'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 6
laFStru[lnFStru+lnI,4] = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'WareHTo'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 6
laFStru[lnFStru+lnI,4] = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'LocFrom'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 10
laFStru[lnFStru+lnI,4] = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'LocTo'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 10
laFStru[lnFStru+lnI,4] = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'ClassFrom'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 1
laFStru[lnFStru+lnI,4] = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'ClassTo'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 1
laFStru[lnFStru+lnI,4] = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'FlatFrom'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 1
laFStru[lnFStru+lnI,4] = 0

lnI = lnI + 1
laFStru[lnFStru+lnI,1] = 'FlatTo'
laFStru[lnFStru+lnI,2] = 'C'
laFStru[lnFStru+lnI,3] = 1
laFStru[lnFStru+lnI,4] = 0

FOR lnI = 1 TO ALEN(laFStru,1)-lnFStru
  STORE .F. TO laFStru[lnFStru+lnI,5],laFStru[lnFStru+lnI,6]
  STORE ''  TO laFStru[lnFStru+lnI,7],laFStru[lnFStru+lnI,8],laFStru[lnFStru+lnI,9],laFStru[lnFStru+lnI,10],laFStru[lnFStru+lnI,11],;
    laFStru[lnFStru+lnI,12],laFStru[lnFStru+lnI,13],laFStru[lnFStru+lnI,14],laFStru[lnFStru+lnI,15],laFStru[lnFStru+lnI,16]
  STORE 0  TO laFStru[lnFStru+lnI,17],laFStru[lnFStru+lnI,18]
ENDFOR

*- Save the current grid data sources
loBrowObj = loFormSet.Ariaform1.grdInventory
DIMENSION laColumns[loBrowObj.ColumnCount,3]
FOR lnI = 1 TO loBrowObj.COLUMNCOUNT
  laColumns[lnI,1] = loBrowObj.COLUMNS(lnI).CONTROLSOURCE
  laColumns[lnI,2] = loBrowObj.COLUMNS(lnI).Header1.CAPTION
  laColumns[lnI,3] = loBrowObj.COLUMNS(lnI).WIDTH
ENDFOR

CREATE DBF (oAriaApplication.WorkDir+loFormSet.lcTempAdj) FROM ARRAY laFStru
USE
SELECT 0
USE (oAriaApplication.WorkDir+loFormSet.lcTempAdj) EXCLUSIVE ALIAS 'TempAdj'
CURSORSETPROP("Buffering",1)
INDEX ON STYLE+Dyelot+LocFrom TAG (loFormSet.lcTempAdj)
SET RELATION TO STYLE INTO STYLE

*- Restore columns definitions
loBrowObj.RECORDSOURCE = 'TempAdj'
loBrowObj.COLUMNCOUNT = ALEN(laColumns,1)
FOR lnI = 1 TO ALEN(laColumns,1)
  loBrowObj.COLUMNS(lnI).CONTROLSOURCE = laColumns[lnI,1]
  loBrowObj.COLUMNS(lnI).Header1.CAPTION = laColumns[lnI,2]
  loBrowObj.COLUMNS(lnI).WIDTH = laColumns[lnI,3]
ENDFOR

*- Add columns to the browse grid
loBrowObj.COLUMNCOUNT = loBrowObj.COLUMNCOUNT + 1
lcI = ALLTRIM(STR(loBrowObj.COLUMNCOUNT))
*loBrowObj.Column9.Header1.Caption = 'B/Location '+&lcFileNama..LocFrom
loBrowObj.COLUMN&lcI..Header1.CAPTION = 'B/Location'
loBrowObj.COLUMN&lcI..COLUMNORDER = 3
loBrowObj.COLUMN&lcI..CONTROLSOURCE = 'TempAdj.LocFrom'

*- Show the To Location in transfer case only
IF loFormSet.lcType = 'T'
  lcI = ALLTRIM(STR(loBrowObj.COLUMNCOUNT))
  loBrowObj.COLUMNCOUNT = loBrowObj.COLUMNCOUNT + 1
  * C201552,1 HES Fix the "Ti" string to be "To" [Start]
  loBrowObj.COLUMN&lcI..Header1.CAPTION = 'To B/Loc.'
  * C201552,1 HES Fix the "Ti" string to be "To" [End  ]
  loBrowObj.COLUMN&lcI..COLUMNORDER = 4
  loBrowObj.COLUMN&lcI..CONTROLSOURCE = 'TempAdj.LocTo'
ENDIF

SELECT (lnSlct)
*-- end of lfDlModBrw.

*:**************************************************************************
*:* Name        : lfADJOPTN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Add an option item to the ICINVTA.SCX screen
*:***************************************************************************
*:* Called from : ICINVTA.SCX
*:***************************************************************************
FUNCTION lfADJOPTN

*- Add a new bar in the option pad
LOCAL llFound,lnSlct
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF


STORE .F. TO llFound
FOR lnCount = 1 TO CNTPAD(loFormSet.cHostFormName)
  IF PRMPAD(loFormSet.cHostFormName, GETPAD(loFormSet.cHostFormName, lnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF (loFormSet.chostformname) PROMPT "\<Options" KEY ALT+P , ' '
  ON PAD _Option OF (loFormSet.chostformname) ACTIVATE POPUP _OptionPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
lnBarNo = 0
LOCAL lnI
FOR lnI = 1 TO CNTBAR(LEFT('_OPTIONPOP',10))
  IF 'Bin Selected' $ PRMBAR('_OPTIONPOP',lnI)
    lnBarNo = lnI
  ENDIF
ENDFOR
IF lnBarNo = 0
  lnBarNo = CNTBAR(LEFT('_OPTIONPOP',10)) + 1
ENDIF
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bin Selected" ;
  SKIP FOR _SCREEN.ACTIVEFORM.PARENT.ActiveMode = 'S'

ON SELECTION BAR lnBarNo  OF _OPTIONPOP DO lfChkAdj WITH 1  

IF loFormSet.lcType = 'T'
  lnBarNo = lnBarNo + 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bin to transfer to" ;
    SKIP FOR _SCREEN.ACTIVEFORM.PARENT.ActiveMode = 'S'
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfChkAdj WITH 2  
ENDIF
SELECT (lnSlct)

*-- end of lfADJOPTN.


*:**************************************************************************
*:* Name        : lfChkAdj
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Calls the lfDLCHKADJ passing 1 or 2 to change the From bin or the To bin
*:***************************************************************************
FUNCTION lfChkAdj
PARAMETERS lnWareNo

PRIVATE lcWareH,lnWhNo
lnWhNo = lnWareNo

lcWareH = IIF(lnWareNo = 1 , _SCREEN.ACTIVEFORM.PARENT.AriaForm1.kbLocation.Keytextbox.VALUE   ,;
  _SCREEN.ACTIVEFORM.PARENT.AriaForm1.kbToLocation.Keytextbox.VALUE )

IF EMPTY(lcWareH)
  RETURN
ENDIF

=lfDLCHKADJ(.T.)

*-- end of lfChkAdj.

*:**************************************************************************
*:* Name        : lfDLCHKADJ
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Check if selected WH has bins, if not then do not select this warehous
*:***************************************************************************
*:* Called from : ICINVTA.SCX
*:***************************************************************************
FUNCTION lfDLCHKADJ
PARAMETERS llFromOption


IF llFromOption
  loFormSet = _SCREEN.ACTIVEFORM.PARENT
ENDIF

LOCAL lcSvOrd,llHasBins
llHasBins = .T.

IF !USED('WHSLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
ENDIF

SELECT WHSLOC
lcSvOrd = ORDER()
=gfSetOrder('WhsLoc')
IF !gfSEEK(lcWareH ,'WHSLOC')
  lcMsg2 = 'No bin locations found for the warehouse ' +lcWareH
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llHasBins = .F.
ENDIF
=gfSetOrder(lcSvOrd)

IF llHasBins
  *- from within this screen check if two locations are equal in the case of transfer
  DO FORM (oAriaApplication.ScreenHome+'ICDLBIN.SCX') WITH loFormSet,lnWhNo TO llHasBins
  IF loFormSet.AriaForm1.kbLocation.Keytextbox.ENABLED
    loFormSet.AriaForm1.cmdNew.ENABLED = llHasBins
    loFormSet.AriaForm1.cmdRemove.ENABLED = llHasBins
  ENDIF
ENDIF

*B610350,1 TMI 06/02/2013 [Start]  close tables
=lfCloseTbl('WHBINLOC')
*=lfCloseTbl('WHSLOC')
*B610350,1 TMI 06/02/2013 [End  ]  
RETURN llHasBins

*:**************************************************************************
*:* Name        : lfUPDTMADJ
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : A trigger that updates the '_'+lcTempAdj temp cursor with the current selected frombin & toBin
*:***************************************************************************
*:* Called from : ICINVTA.SCX
*:***************************************************************************
FUNCTION lfUPDTMADJ
LOCAL lnSlct,lcFormNamA
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

lcFormNamA = '_' + loFormSet.lcTempAdj
IF USED(lcFormNamA)
  SELECT TempAdj
  SCATTER MEMVAR

  SELECT &lcFormNamA
  LOCATE
  IF !EOF()
    GATHER MEMVAR
  ENDIF
ENDIF
SELECT (lnSlct)
*-- end of lfUPDTMADJ.

*!***************************************************************************
*!* Name        : lfDLCHKTRN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check Transaction
*!***************************************************************************
*!* Called from : ICSTYTR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCHKTRN()
*!***************************************************************************
FUNCTION lfDLCHKTRN
LOCAL lnSlct,lcSvOrd,llClass,llContinue,lcFldNo

lcFldNo = STR(lnFldNo,1)
*- if not changed , exit
IF loQtyBrkDwn2.txtQty&lcFldNo..VALUE = loQtyBrkDwn2.txtQty&lcFldNo..OldValue
  RETURN
ENDIF
IF loQtyBrkDwn2.txtQty&lcFldNo..VALUE = 0 .AND. loFormSet.lcType $ 'AT'
  RETURN
ENDIF

IF !lfIsUseBin()
  RETURN
ENDIF

*--check if Bin transfer to same class as or not
PRIVATE lcFileNama
lcFileNama = '_'+loFormSet.lcTempAdj

lnSlct = SELECT(0)

llContinue = .T.
SELECT WHSLOC
lcSvOrd = ORDER()
=gfSetOrder('WHSLOC')

lcFromWare = loFormSet.AriaForm1.kbLocation.Keytextbox.VALUE
lcToWare   = loFormSet.AriaForm1.kbToLocation.Keytextbox.VALUE

*IF gfSEEK(IIF(loFormSet.lcType = 'T',loFormSet.lcToWare+&lcFileNamA..LocTo,lcFromWare+&lcFileNamA..LocFrom),'WHSLOC')

IF gfSEEK(lcFromWare+&lcFileNamA..LocFrom,'WHSLOC')
  llClass = PADR(WhsLoc.CBINCLASS,1) $ PADR(SUBSTR(STYLE.CPRIMCLSS,lnFldNo),1) + ;
    PADR(SUBSTR(STYLE.CSECCLSS ,lnFldNo),1) + ;
    PADR(SUBSTR(STYLE.CREMCLSS ,lnFldNo),1)
  IF !llClass
    lcMsg2 = 'You cannot Issue the style Size with a bin of a different class.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    loQtyBrkDwn2.txtQty&lcFldNo..VALUE = loQtyBrkDwn2.txtQty&lcFldNo..OldValue
    llContinue = .F.
  ENDIF
  IF llContinue .AND. loFormSet.lcType = 'T' .AND. gfSeek(lcToWare+&lcFileNamA..LocTo,'WHSLOC')
    llClass = PADR(WhsLoc.CBINCLASS,1) $ PADR(SUBSTR(STYLE.CPRIMCLSS,lnFldNo),1) + ;
      PADR(SUBSTR(STYLE.CSECCLSS ,lnFldNo),1) + ;
      PADR(SUBSTR(STYLE.CREMCLSS ,lnFldNo),1)
    IF !llClass
      lcMsg2 = 'You cannot transfer the style Size to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      loQtyBrkDwn2.txtQty&lcFldNo..VALUE = loQtyBrkDwn2.txtQty&lcFldNo..OldValue
      llContinue = .F.
    ENDIF
  ENDIF
ENDIF

IF llContinue .AND. ;
    loFormSet.lcType = 'A' AND loQtyBrkDwn2.txtQty&lcFldNo..VALUE < 0
  =gfSeek(lcFromWare+&lcFileNamA..LocFrom+STYLE.STYLE,'WHBINLOC')
  IF ABS(loQtyBrkDwn2.txtQty&lcFldNo..VALUE) > (WHBINLOC.QTY&lcFldNo - WHBINLOC.ALO&lcFldNo)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You can not reduce inventory more than the available ( ' + ;
      ALLTRIM(STR(WHBINLOC.QTY&lcFldNo - WHBINLOC.ALO&lcFldNo)) + " )")
    loQtyBrkDwn2.txtQty&lcFldNo..VALUE = loQtyBrkDwn2.txtQty&lcFldNo..OldValue
    llContinue = .F.
  ENDIF
  *B610762,1 tmi 6/24/2014 [start] don't allow stock be less than allocated
  SET STEP ON
  IF llContinue and ABS(loQtyBrkDwn2.txtQty&lcFldNo..VALUE) > (STYLE.STK&lcFldNo - STYLE.ALO&lcFldNo)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You can not reduce inventory more than the available ( ' + ;
      ALLTRIM(STR(STYLE.STK&lcFldNo - STYLE.ALO&lcFldNo)) + " )")
    loQtyBrkDwn2.txtQty&lcFldNo..VALUE = loQtyBrkDwn2.txtQty&lcFldNo..OldValue
    llContinue = .F.
  ENDIF
  *B610762,1 tmi 6/24/2014 [end  ] don't allow stock be less than allocated
  
ENDIF

SELECT WHSLOC
=gfSetOrder(lcSvOrd)

SELECT (lnSlct)
RETURN llContinue
*-- End of Function lfDLCHKTRN.
************************************************************
*! Name      : lfSTKALOCHK
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/18/2014
*! Purpose   : prevent stock being less than allocation in icinvt_a.scx
************************************************************
*B610860,1 TMI 09/18/2014 20:16 
FUNCTION lfSTKALOCHK
LOCAL loFrm
loFrm = loQtyBrkDwn2.Parent.Parent
IF loFrm.lcType='T' and loFrm.lcFromWare <> loFrm.lcToWare 
  *- if the transfered qty will make the stock less than the allocated then don't continue
  if loQtyBrkDwn2.txtQty&lcQtyNo..Value <> loQtyBrkDwn2.txtQty&lcQtyNo..OldValue and ;
     STYDYE.STK&lcQtyNo - loQtyBrkDwn2.txtQty&lcQtyNo..Value < STYDYE.ALO&lcQtyNo
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Stock qty can not be less than allocated')
    Return .F.
  Endif
Endif 
*- End of lfSTKALOCHK.

*:**************************************************************************
*:* Name        : lfTRNSMSG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 5/28/2008
*:* Purpose     : Show a message that the transferred stock is less than the availabel stock
*:***************************************************************************
*:* Called from : icinvt_a.scx
*:***************************************************************************
FUNCTION lfTRNSMSG
IF !lfIsUseBin()
  RETURN
ENDIF
LOCAL lcPice
lcPice = ALLTRIM(STR(lnStockVal)) + IIF(lnStockVal=1,'','s')
=gfModalGen('INM00000B00000',.F.,.F.,.F.,'You can only transfer up to &lcPice piece')
*-- end of lfTRNSMSG.

*:**************************************************************************
*:* Name        : lfDLVLDSTY
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*!* Purpose     : Validate Style in case of IC Adj. and IC Physical.
*:***************************************************************************
*:* Called from : ICINVT_A.SCX
*:***************************************************************************
FUNCTION lfDLVLDSTY
LOCAL lnSlct,lcSvOrd,llValidChoice
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

SET DATASESSION TO loFormSet.DATASESSIONID

llValidChoice = .T.

LOCAL lcFileNamA,lcBin
lcFileNamA = '_' + loFormSet.lcTempAdj
lcBin      = IIF(loIcInvT_a.llNew,&lcFileNamA..LocFrom,TempAdj.LocFrom)

IF llValidChoice
  IF EMPTY(STYLE.cflathang) OR !STYLE.cflathang $ 'HF'
    lcMsg2 = 'The style has no assigned value in the Hang/Flat field.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    llValidChoice = .F.
  ENDIF
ENDIF

IF llValidChoice
  SELECT WHSLOC
  lcSvOrd = ORDER('WHSLOC')
  =gfSetOrder('WHSLOC')

  *- Open WHBINLOC file
  =lfOpnFiles('WHBINLOC','WHBINLOC','')

  lcFromWare = loFormSet.AriaForm1.kbLocation.Keytextbox.VALUE
  lcToWare   = loFormSet.AriaForm1.kbToLocation.Keytextbox.VALUE

ENDIF

IF llValidChoice
  IF loFormSet.lcType = 'T'
    IF !gfSEEK(lcFromWare + lcBin + STYLE.STYLE ,'WHBINLOC')
      lcMsg = 'The style '+STYLE.STYLE+' has no stock in the Wh/Bin : ' + ALLTRIM(lcFromWare)+ ' / ' + ALLTRIM(lcBin) + '.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
      llValidChoice = .F.
    ENDIF
  ENDIF
ENDIF

IF llValidChoice
  LOCAL lcStyle,lcDyelot,lnRenco
  lcStyle  = STYLE.STYLE
  lcDyelot = SPACE(10)

  IF SEEK(lcStyle+lcDyelot+&lcFileNamA..LocFrom,'TempAdj')
    *- change the text of the message
    lcMsg2 = 'This style has been already added to the bin '+&lcFileNamA..LocFrom
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    llValidChoice = .F.
  ENDIF
  =gfSeek(lcStyle,'STYLE')
ENDIF

IF llValidChoice
  =gfSEEK(lcFromWare+lcBin,'WHSLOC')
  IF !(WhsLoc.cFlatHang = STYLE.cflathang)
    lcMsg2 = 'You cannot Adjust a style with a bin of a different Hang/Flat.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    llValidChoice = .F.
  ENDIF
ENDIF

lcMsg2 = 'You cannot Adjust a style with a bin of a different class.'
IF loFormSet.lcType = 'T'
  lcMsg2 = 'The style must be of the same class as the transferred FROM Bin.'
ENDIF
llValidChoice = llValidChoice .AND. lfChkClass(lcMsg2)

IF llValidChoice .AND. loFormSet.lcType = 'T'
  =gfSEEK(lcToWare+&lcFileNamA..LocTO,'WHSLOC')
  IF !(WhsLoc.cFlatHang = STYLE.cflathang)
    lcMsg2 = 'The TO bin and the style cannot be with a different Hang/Flat.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    llValidChoice = .F.
  ENDIF
  lcMsg2 = 'The style must be of the same class as the transferred TO Bin.'
  llValidChoice = llValidChoice .AND. lfChkClass(lcMsg2)
ENDIF

IF llValidChoice
  SELECT WHSLOC
  SCATTER MEMVAR MEMO
  IF !gfSEEK(lcFromWare+lcBin+STYLE.STYLE,'WHSLOC')
    m.cWareCode = lcFromWare
    m.style     = STYLE.STYLE
    m.cLocation = lcBin
    =gfAppend('WHSLOC',.T.)
  ENDIF
ENDIF

SELECT WHSLOC
=gfSetOrder(lcSvOrd)

SELECT (lnSlct)
RETURN llValidChoice
*-- End of Function lfDLVLDSTY.

*:**************************************************************************
*:* Name        : lfChkClass
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Check that the style and bin has the same class
*:***************************************************************************
FUNCTION lfChkClass
PARAMETERS lcMsg2
LOCAL lnCount,llContinue
llContinue = .T.

IF EMPTY(WhsLoc.cBinClass)
  lcMsg2 = 'The selected bin has not assigned class value'
  llContinue = .F.
ENDIF

=SEEK('S'+STYLE.SCALE,'SCALE')

IF llContinue
  llClass = .F.

  *- at least one size has the same class as the selected bin
  FOR lnCount = 1 TO SCALE.CNT
    IF llClass
      EXIT
    ENDIF
    *- Check that at least one size meets the class of the selected bin, when dealing with sizes another check will be done per size
    IF WhsLoc.cBinClass = PADR(SUBSTR(STYLE.cPrimClss,lnCount,1),1) .OR. ;
        WhsLoc.cBinClass = PADR(SUBSTR(STYLE.cSecClss ,lnCount,1),1) .OR. ;
        WhsLoc.cBinClass = PADR(SUBSTR(STYLE.cRemClss ,lnCount,1),1)
      llClass = .T.
    ENDIF
  ENDFOR
  *- All sizes not meet the bin class type
  IF !llClass
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    llContinue = .F.
  ENDIF
ENDIF

RETURN llContinue
*-- end of lfChkClass.

*:**************************************************************************
*:* Name        : lfADJGTINF
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Get information needed in case of adjust,physical and transfer inventory
*:***************************************************************************
*:* Called from : ICINVT_A.SCX
*:***************************************************************************
FUNCTION lfADJGTINF
LOCAL lnSlct,lcFileNamA,lcStyle
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

*B610350,3 TMI 06/17/2013 [Start] open the table WHBINLOC if it is not 
IF !USED('WHBINLOC')
  gfOpenTable('WHBINLOC','WHBINLOC','SH')
ENDIF 
*B610350,3 TMI 06/17/2013 [End  ] 

*--Get Style & Dyelot values.
lcStyle  = PADR(loIcInvT_a.AriaForm1.cntStyle.VALUE,19)
lcFileNamA = '_' + loFormSet.lcTempAdj

SELECT STYLE
=gfSEEK(lcStyle)

DIMENSION laTrgStk[9]
laTrgStk = 0
IF loFormSet.lcType = 'T'
  =gfSEEK(loIcInvT_a.lcToWare+&lcFileNama..LocTo+lcStyle,'WHBINLOC')
  *--Read target warehouse current style stock.
  SELECT WHBINLOC
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laTrgStk[lnI] = WHBINLOC.QTY&lcI - WHBINLOC.ALO&lcI
  ENDFOR
  laTrgStk[9] = laTrgStk[1]+laTrgStk[2]+laTrgStk[3]+laTrgStk[4]+laTrgStk[5]+laTrgStk[6]+laTrgStk[7]+laTrgStk[8]

  loIcInvT_a.AriaForm1.lblQBkdn1.CAPTION = 'From Bin'
  loIcInvT_a.AriaForm1.lblQBkdn3.CAPTION = 'To Bin'

ENDIF

*--Read current style stock.
SELECT WHBINLOC
*=gfSEEK(loIcInvT_a.lcToWare+&lcFileNama..LocFrom+lcStyle,'WHBINLOC')
=gfSEEK(loIcInvT_a.lcFromWare+&lcFileNama..LocFrom+lcStyle,'WHBINLOC')
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
  Alo1,Alo2,Alo3,Alo4,Alo5,Alo6,Alo7,Alo8,TotAlo MEMVAR
*--Set the stock values.
FOR lnI=1 TO 8
  lcI=STR(lnI,1)
  loIcInvT_a.AriaForm1.cntQtyBrkdwn1.txtQty&lcI..VALUE = m.Qty&lcI - m.Alo&lcI
  IF loFormSet.lcType = 'P'
    loIcInvT_a.AriaForm1.cntQtyBrkdwn2.txtQty&lcI..VALUE = m.Qty&lcI - m.Alo&lcI
  ELSE
    IF loFormSet.lcType = 'T'
      *--Case of Transfer , The transfered qty should be less than Stock
      IF m.Qty&lcI - m.Alo&lcI <= 0
        loIcInvT_a.AriaForm1.cntQtyBrkdwn2.txtQty&lcI..ENABLED = .F.
      ENDIF
      loIcInvT_a.AriaForm1.cntQtyBrkdwn3.txtQty&lcI..VALUE = laTrgStk[lnI]
    ELSE && loFormSet.lcType = A or P
      loIcInvT_a.AriaForm1.cntQtyBrkdwn3.txtQty&lcI..VALUE = m.Qty&lcI - m.Alo&lcI
    ENDIF
  ENDIF
  IF !loIcInvT_a.llNew
    loIcInvT_a.AriaForm1.cntQtyBrkdwn2.txtQty&lcI..VALUE = TempAdj.Adj&lcI
    DO CASE
    CASE loFormSet.lcType = 'P'
      loIcInvT_a.AriaForm1.cntQtyBrkdwn3.txtQty&lcI..VALUE = TempAdj.Adj&lcI
    CASE loFormSet.lcType = 'A'
      loIcInvT_a.AriaForm1.cntQtyBrkdwn3.txtQty&lcI..VALUE = m.Qty&lcI - m.Alo&lcI + TempAdj.Adj&lcI
    CASE loFormSet.lcType = 'T'
      loIcInvT_a.AriaForm1.cntQtyBrkdwn3.txtQty&lcI..VALUE = laTrgStk[lnI] + TempAdj.Adj&lcI
    ENDCASE
  ENDIF
ENDFOR
*--Set Total stock value.
loIcInvT_a.AriaForm1.cntQtyBrkdwn1.txtTotQty.VALUE = m.TotQty - m.TotAlo
IF loFormSet.lcType = 'P'
  loIcInvT_a.AriaForm1.cntQtyBrkdwn2.txtTotQty.VALUE = m.TotQty - m.TotAlo
ELSE
  IF loFormSet.lcType = 'T'
    loIcInvT_a.AriaForm1.cntQtyBrkdwn3.txtTotQty.VALUE = laTrgStk[9]
  ELSE && loFormSet.lcType = 'A'
    loIcInvT_a.AriaForm1.cntQtyBrkdwn3.txtTotQty.VALUE = m.TotQty - m.TotAlo
  ENDIF
ENDIF

SELECT(lnSlct)
*-- end of lfADJGTINF.

*:**************************************************************************
*:* Name        : lfUPDBNADJ
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Update the temp file lcTempAdj with the added bin data
*:***************************************************************************
*:* Called from : ICINVT_A.SCX
*:***************************************************************************
FUNCTION lfUPDBNADJ
LOCAL lnSlct,lcFileNamA
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

lcFileNamA = '_' + loFormSet.lcTempAdj
SELECT &lcFileNamA
LOCATE
SCATTER MEMVAR
SELECT TempAdj
GATHER MEMVAR FIELDS LocFrom,LocTo,ClassFrom,ClassTo,FlatFrom,FlatTo

SELECT (lnSlct)
*-- end of lfUPDBNADJ.

*:**************************************************************************
*:* Name        : lfDlSavDat
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Save data
*:***************************************************************************
*:* Called from : ICINVSAV.PRG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDlSavDat()
*:***************************************************************************
FUNCTION lfDlSavDat

LOCAL lnSlct,lcSvOrd1
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

SELECT WHSLOC
lcSvOrd1 = ORDER('WHSLOC')
=gfSetOrder(lcSvOrd1)

=lfOpnFiles("STYINVJL,BININVJL,WHBINLOC,WHSLOC","STYINVJL,STYINVJL,WHBINLOC,WHSLOC","")

SELECT (lcTmpAdj)
LOCATE
IF lcType = 'T'
  SCAN FOR TOTADJ > 0

    *--from location
    IF gfSEEK(&lcTmpAdj..cFromWare +&lcTmpAdj..LocFrom +&lcTmpAdj..STYLE,'WHBINLOC')
      SELECT WHBINLOC
      lcReplace = 'Qty1       WITH '+STR( Qty1- &lcTmpAdj..Adj1 ) +' '+ ;
        'Qty2       WITH '+STR( Qty2- &lcTmpAdj..Adj2 ) +' '+ ;
        'Qty3       WITH '+STR( Qty3- &lcTmpAdj..Adj3 ) +' '+ ;
        'Qty4       WITH '+STR( Qty4- &lcTmpAdj..Adj4 ) +' '+ ;
        'Qty5       WITH '+STR( Qty5- &lcTmpAdj..Adj5 ) +' '+ ;
        'Qty6       WITH '+STR( Qty6- &lcTmpAdj..Adj6 ) +' '+ ;
        'Qty7       WITH '+STR( Qty7- &lcTmpAdj..Adj7 ) +' '+ ;
        'Qty8       WITH '+STR( Qty8- &lcTmpAdj..Adj8 )
      =gfReplace(lcReplace)

      lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
      =gfReplace(lcReplace)

      IF WHBINLOC.Qty1 = 0 .AND. WHBINLOC.Qty2 = 0 .AND. WHBINLOC.Qty3 = 0 .AND. WHBINLOC.Qty4 = 0 .AND. ;
          WHBINLOC.Qty5 = 0 .AND. WHBINLOC.Qty6 = 0 .AND. WHBINLOC.Qty7 = 0 .AND. WHBINLOC.Qty8 = 0 .AND. ;
          gfGetMemVar('M_DELZRBNR')
        =gfDelete()
      ENDIF
    ENDIF

    *--To location
    SELECT WHBINLOC
    lcReplace = ''
    IF !gfSEEK(&lcTmpAdj..cToware +&lcTmpAdj..LocTo +&lcTmpAdj..STYLE,'WHBINLOC')

      SELECT WHSLOC
      =gfSEEK(&lcTmpAdj..cfromware +&lcTmpAdj..LocTo +SPACE(19),'WHSLOC')

      SELECT WHBINLOC
      =gfAppend()

      lcReplace = lcReplace +;
        'STYLE      WITH ['+&lcTmpAdj..STYLE   +'] '+;
        'CWARECODE  WITH ['+&lcTmpAdj..cToware +'] '+;
        'clocation  WITH ['+&lcTmpAdj..LocTo +'] '

      lcReplace = lcReplace +;
        'cBlkPck   WITH ['+WHSLOC.cBlkPck   +'] '+;
        'cSection  WITH ['+WHSLOC.cSection  +'] '+;
        'cBinClass WITH ['+WHSLOC.cBinClass +'] '
    ENDIF

    lcReplace = lcReplace +;
      'Qty1       WITH '+STR( Qty1 + &lcTmpAdj..Adj1) +' '+;
      'Qty2       WITH '+STR( Qty2 + &lcTmpAdj..Adj2) +' '+;
      'Qty3       WITH '+STR( Qty3 + &lcTmpAdj..Adj3) +' '+;
      'Qty4       WITH '+STR( Qty4 + &lcTmpAdj..Adj4) +' '+;
      'Qty5       WITH '+STR( Qty5 + &lcTmpAdj..Adj5) +' '+;
      'Qty6       WITH '+STR( Qty6 + &lcTmpAdj..Adj6) +' '+;
      'Qty7       WITH '+STR( Qty7 + &lcTmpAdj..Adj7) +' '+;
      'Qty8       WITH '+STR( Qty8 + &lcTmpAdj..Adj8)

    =gfReplace(lcReplace)

    lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
    =gfReplace(lcReplace)

  ENDSCAN

ELSE

  SCAN

    SELECT WHBINLOC
    lcReplace = ''
    IF !gfSEEK(&lcTmpAdj..cfromware +&lcTmpAdj..LocFrom +&lcTmpAdj..STYLE,'WHBINLOC')

      SELECT WHSLOC
      =gfSEEK(&lcTmpAdj..cfromware +&lcTmpAdj..LocFrom +SPACE(19),'WHSLOC')

      SELECT WHBINLOC
      =gfAppend()
      lcReplace = lcReplace + ;
        'STYLE      WITH ['+&lcTmpAdj..STYLE     +'] '+;
        'CWARECODE  WITH ['+&lcTmpAdj..cFromWare +'] '+;
        'clocation  WITH ['+&lcTmpAdj..LocFrom +'] '

      lcReplace = lcReplace +;
        'cBlkPck   WITH ['+WHSLOC.cBlkPck   +'] '+;
        'cSection  WITH ['+WHSLOC.cSection  +'] '+;
        'cBinClass WITH ['+WHSLOC.cBinClass +'] '

    ENDIF

    IF lcType = 'A'
      LOCAL lcKey,lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8
      STORE 0 TO lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8

      SELECT &lcTmpAdj
      lcKey = EVALUATE(KEY())
      SCAN REST WHILE STYLE+DYELOT+LOCFROM = lcKey
        lnAdj1 = lnAdj1 + &lcTmpAdj..adj1
        lnAdj2 = lnAdj2 + &lcTmpAdj..adj2
        lnAdj3 = lnAdj3 + &lcTmpAdj..adj3
        lnAdj4 = lnAdj4 + &lcTmpAdj..adj4
        lnAdj5 = lnAdj5 + &lcTmpAdj..adj5
        lnAdj6 = lnAdj6 + &lcTmpAdj..adj6
        lnAdj7 = lnAdj7 + &lcTmpAdj..adj7
        lnAdj8 = lnAdj8 + &lcTmpAdj..adj8
      ENDSCAN
      SKIP -1

      SELECT WHBINLOC
      lcReplace = lcReplace + ;
        'Qty1       WITH '+ STR( Qty1 + lnAdj1 ) +' '+;
        'Qty2       WITH '+ STR( Qty2 + lnAdj2 ) +' '+;
        'Qty3       WITH '+ STR( Qty3 + lnAdj3 ) +' '+;
        'Qty4       WITH '+ STR( Qty4 + lnAdj4 ) +' '+;
        'Qty5       WITH '+ STR( Qty5 + lnAdj5 ) +' '+;
        'Qty6       WITH '+ STR( Qty6 + lnAdj6 ) +' '+;
        'Qty7       WITH '+ STR( Qty7 + lnAdj7 ) +' '+;
        'Qty8       WITH '+ STR( Qty8 + lnAdj8 )
    ELSE
      lcReplace = lcReplace + ;
        'Qty1       WITH '+ STR( &lcTmpAdj..adj1 ) +' '+;
        'Qty2       WITH '+ STR( &lcTmpAdj..adj2 ) +' '+;
        'Qty3       WITH '+ STR( &lcTmpAdj..adj3 ) +' '+;
        'Qty4       WITH '+ STR( &lcTmpAdj..adj4 ) +' '+;
        'Qty5       WITH '+ STR( &lcTmpAdj..adj5 ) +' '+;
        'Qty6       WITH '+ STR( &lcTmpAdj..adj6 ) +' '+;
        'Qty7       WITH '+ STR( &lcTmpAdj..adj7 ) +' '+;
        'Qty8       WITH '+ STR( &lcTmpAdj..adj8 )
    ENDIF
    *    lcReplace = lcReplace + ;
    lfAddInfo()

    =gfReplace(lcReplace)

    lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
    =gfReplace(lcReplace)

    IF WHBINLOC.Qty1 = 0 .AND. WHBINLOC.Qty2 = 0 .AND. WHBINLOC.Qty3 = 0 .AND. WHBINLOC.Qty4 = 0 .AND. ;
        WHBINLOC.Qty5 = 0 .AND. WHBINLOC.Qty6 = 0 .AND. WHBINLOC.Qty7 = 0 .AND. WHBINLOC.Qty8 = 0 .AND. ;
        gfGetMemVar('M_DELZRBNR')
      =gfDelete()
    ENDIF
  ENDSCAN
ENDIF

SELECT WHSLOC
=gfSetOrder(lcSvOrd1)

*- Update Line# field in the Physical Inv. transactions
IF lcType = 'P'  .AND. lcCostMth $ 'FL'

  SELECT BININVJL

  *- Get the cursor view cursor name
  LOCAL lnCursorUpdate
  SELECT WHBINLOC
  FOR lnCursorUpdate = 1 TO ALEN(oAriaApplication.laRemoteTable,1)
    IF oAriaApplication.laRemoteTable[lnCursorUpdate].lcCursorView = 'BININVJL'
      EXIT
    ENDIF
  ENDFOR

  LOCAL lnI
  SELECT (oAriaApplication.laRemoteTable[lnCursorUpdate].lcCursorUpdate)
  SET ORDER TO
  GO TOP
  lnI = 1
  SCAN
    IF cIRType = 'I'
      REPLACE LINENO WITH lnI
      lnI = lnI + 1
    ENDIF
  ENDSCAN
ENDIF

**-- updating the bininvjl & whbinloc files
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF

llUpdate = .T.
llUpdate = llUpdate AND gfTableUpdate(lcTranCode,'BININVJL')
llUpdate = llUpdate AND gfTableUpdate(lcTranCode,'WHBINLOC')
llUpdate = llUpdate AND gfTableUpdate(lcTranCode,'STYINVJL')

IF llUpdate
  =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ELSE
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
ENDIF

*- clear the temp file ('_'+lcTmpAdj)
SELECT ('_'+loIcinva.lcTempAdj)
ZAP

SELECT (lnSlct)
*-- End of Function lfDlSavDat.

*:**************************************************************************
*:* Name        : lfSLCTMODE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : change to select mode
*:***************************************************************************
FUNCTION lfSLCTMODE
LOCAL lnSlct,lcTmpFileA
lnSlct = SELECT(0)

*- clear the temp file lcTmpAdj
lcTmpFileA = '_'+loFormSet.lcTempAdj
IF USED(lcTmpFileA)
  SELECT (lcTmpFileA)
  ZAP
ENDIF

SELECT (lnSlct)
*-- end of lfSLCTMODE.

*:**************************************************************************
*:* Name        : lfDLUPDTGL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Do not update the GL if the movement were between bins in the same warehous
*:***************************************************************************
*:* Called from : ICINVSAV.PRG
*:***************************************************************************
FUNCTION lfDLUPDTGL
LOCAL llSkipGlUpdate

*B610685,1 TMI 03/11/2014 19:19 [Start] comment these lines as the two variables are already defined in the calling program "prgs\ic\icinvsav.prg"
*lcFromWare = loFormSet.AriaForm1.kbLocation.Keytextbox.VALUE
*lcToWare   = loFormSet.AriaForm1.kbToLocation.Keytextbox.VALUE
*B610685,1 TMI 03/11/2014 19:19 [End  ] 

llSkipGlUpdate = .F.
*B610685,1 TMI 03/11/2014 19:20 [Start] comment this line as the variable lcType is already defined in the calling program "prgs\ic\icinvsav.prg"
*IF lfIsUseBin() .AND. ;
    loFormSet.lcType = 'T' .AND. ;
    lcFromWare == lcToWare
IF lfIsUseBin() .AND. ;
    lcType = 'T' .AND. ;
    lcFromWare == lcToWare
  *B610685,1 TMI 03/11/2014 19:20 [End  ] 
  llSkipGlUpdate = .T.

ENDIF

RETURN llSkipGlUpdate
*-- End of Function lfDLUPDTGL.
*!***************************************************************************
*!* Name        : lfDLSVBNI
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update BinInvJL & from issue Styinvjl
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg , ICSTYTR.Prg
*!***************************************************************************
*!* Calls       : lfOpnFiles()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSVBNI()
*!***************************************************************************
FUNCTION lfDLSVBNI

LOCAL lnSlct
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lcStyOrd

=lfOpnFiles("STYINVJL,BININVJL","STYINVJL,STYINVJL","")

*C201605,1 TMI 02/20/2014 19:50 [Start] check if this is a transfer within the same warehous
llTrnsferSameWH =  lcType = 'T' .AND. lcFromWare == lcToWare
*C201605,1 TMI 02/20/2014 19:52 [End  ] 

SELECT STYINVJL
lnCurrec = RECNO()
lcSty1 = STYLE
lcWarhos = &lcTmpAdj..cFromWare

IF !gfSEEK(&lcTmpAdj..STYLE+&lcTmpAdj..cFromWare+lcIsuSessNo)
  lcWarhos = &lcTmpAdj..cToWare
  =gfSEEK(lcSty1 + lcWarhos + lcIsuSessNo)
ENDIF
*B608785 TMI 01/19/2009 [Start] use the variable lnMaxLnNo to store LINENO field
LOCAL lnMaxLnNo
lnMaxLnNo = 0
*B608785 TMI 01/19/2009 [End  ]
SCAN WHILE STYLE+cWareCode+cSession = lcsty1 + lcWarhos + lcIsuSessNo
  *C201605,1 TMI 02/20/2014 20:00 [Start] update the cowner field with BININVTR
  IF llTrnsferSameWH
    REPLACE COWNER WITH 'BININVTR'
  ENDIF
  *C201605,1 TMI 02/20/2014 20:00 [End  ] 
  SCATT MEMVAR MEMO
  IF cirtype = 'I'
    m.clocation = &lcTmpAdj..LocFrom
  ELSE
    m.clocation = &lcTmpAdj..LocTO
    IF EMPTY(&lcTmpAdj..LocTO)
      m.clocation = &lcTmpAdj..LocFrom
    ENDIF
  ENDIF
  SELECT BININVJL
  *B608785 TMI 01/19/2009 [Start] check repeated lines, if so the icrement the LINENO
  =lfIncLineNO(@lnMaxLnNo)
  *B608785 TMI 01/19/2009 [End  ]
  =gfAppend('BININVJL',.T.)
ENDSCAN

SELECT STYINVJL
TRY
  GOTO lnCurrec
CATCH
ENDTRY

SELECT(lnSlct)
*-- End of Function lfDLSVBNI.

*:***************************************************************************
* USE
*:***************************************************************************
*:**************************************************************************
*:* Name        : lfIncLineNO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/20/2009
*:* Purpose     : used to increment the m.LINENO variable to keep the key of BININVJL
*:***************************************************************************
FUNCTION lfIncLineNO
LPARAMETERS lnMaxLnNo
LOCATE
SCAN
  IF CWARECODE+CLOCATION+STYLE+CTRCODE+cirtype+CSESSION+crsession+DTOS(DTRDATE) = ;
      m.CWARECODE+m.CLOCATION+m.STYLE+m.CTRCODE+m.cirtype+m.CSESSION+m.crsession+DTOS(m.DTRDATE)
    lnMaxLnNo = MAX(LINENO,lnMaxLnNo)
    m.LineNo = lnMaxLnNo+1
    *B608844,2 TMI 04/18/2009 [Start] comment this out
    *EXIT
    *B608844,2 TMI 04/18/2009 [End  ]
  ENDIF
ENDSCAN
*-- end of lfIncLineNO.

*!***************************************************************************
*!* Name        : lfDLSVBNR
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update BinInvJL & from RECEIVE Styinvjl
*!***************************************************************************
*!* Called from : ICINVSAV.PRG --> FOR PRGS. ICSTYTR.PRG,ICSTYPH.PRG,ICSYTAD.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSVBNR()
*!***************************************************************************
FUNCTION lfDLSVBNR
LOCAL lnSlct
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lcStyOrd,lnAlias,lnCrRec
*--Open the needed files
IF !lfOpnFiles("STYINVJL,BININVJL","STYINVJL,STYINVJL","")
  RETURN
ENDIF

*C201605,1 TMI 02/20/2014 19:50 [Start] check if this is a transfer within the same warehous
llTrnsferSameWH =  lcType = 'T' .AND. lcFromWare == lcToWare
*C201605,1 TMI 02/20/2014 19:52 [End  ] 

SELECT STYINVJL
lnCrRec = RECNO()
IF gfSEEK(STYLE+&lcTmpAdj..cToWare+lcRcvSessNo)
  *C201605,1 TMI 02/20/2014 20:00 [Start] update the cowner field with BININVTR
  IF llTrnsferSameWH
    REPLACE COWNER WITH 'BININVTR'
  ENDIF
  *C201605,1 TMI 02/20/2014 20:00 [End  ] 
  SCATT MEMVAR MEMO
  m.clocation = &lcTmpAdj..LocTo    && ????
  SELECT BININVJL
  =gfAppend('BININVJL',.T.)
ENDIF
SELECT STYINVJL
TRY
  GOTO lnCrRec
CATCH
ENDTRY

SELECT (lnSlct)
*-- End of Function lfDLSVBNR.

*!***************************************************************************
*!* Name        : lfDLSTYCRL
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Trigger to Validate Post Style Per Bin Location in inv. Locking
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSTYCRL()
*!***************************************************************************
FUNCTION lfDLSTYCRL
LOCAL lnSlct
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF
IF TYPE('lcType') = 'U'
  *T20071102.0018,10/C200876 TMI 05/21/2008 [Start]
  *PRIVATE lcFileNama
  *lcFileNamA = lcTmpQuery +"A"
  *lcBinLoc = &lcFileNamA..cLocation

  *B608785 TMI 01/20/2009 [Start] is being populated in lfDlpstlck
  *lcBinLoc = MDINVNTL.clocation
  *B608785 TMI 01/20/2009 [End  ]

  SELECT MDINVNTH
  lcScFields ='CBATTYPE,cLkBatch,TYPE,content,DATE,DPOSTDATE,CLOCATION,CWARECODE'
  SCATTER FIELDS &lcScFields TO laData
  *T20071102.0018,10/C200876 TMI 05/21/2008 [End  ]
  
   

  *B608785 TMI 01/20/2009 [Start] use the standard function , and then update the bininvjl file
  *lnRet = lfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[6],'',@laAdjust,laAdjust[10],;
  '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
  lnRet = gfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[6],'',@laAdjust,laAdjust[10],;
    '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)


  *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[START]
  lcCostMethod = gfGetMemVar('M_Cost_Meth')
  *B610671,1 TMI 02/05/2014 21:29 [Start] comment this part
  *IF lcCostMethod = 'F'
  *  llDeleteIssue = .F.
  *  IF !USED('TmpJrBn')
  *    CREATE CURSOR 'TmpJrBn' (CBATTYPE C(1),cLkBatch C(6),STYLE C(19),CWARECODE C(6),cSessNo C(6))
  *    SELECT 'TmpJrBn'
  *    INDEX ON CBATTYPE+cLkBatch+STYLE+CWARECODE TAG 'TmpJrBn'
  *    INSERT INTO 'TmpJrBn' VALUES(MDINVNTH.CBATTYPE,MDINVNTH.cLkBatch ,lcPStyle,MDINVNTH.CWARECODE,'')
  *  ELSE
  *    IF SEEK(MDINVNTH.CBATTYPE+MDINVNTH.cLkBatch +lcPStyle+MDINVNTH.CWARECODE,'TmpJrBn')
  *      llDeleteIssue = .T.
  *    ELSE
  *      INSERT INTO 'TmpJrBn' VALUES(MDINVNTH.CBATTYPE,MDINVNTH.cLkBatch ,lcPStyle,MDINVNTH.CWARECODE,'')
  *    ENDIF
  *  ENDIF
  *ENDIF
  *B610671,1 TMI 02/05/2014 21:30 [End  ] 
  *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[END]
  IF !USED('BININVJL')
    =gfOpenTable(oAriaApplication.DataDir+'BININVJL','Styinvjl','SH')
  ENDIF
  LOCAL lcKey,lcSvOrd,lnLnNO
  SELECT STYINVJL
  lcSvOrd = ORDER()
  SET ORDER TO
  GO BOTTOM
  *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[START]
  *lcKey = STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE
  lcSessionNo = CSESSION
  lcKey = STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)
  *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[End]
  SET ORDER TO STYINVJL   && STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)
  =SEEK(lcKey)
  lnMaxLnNo = 0
  *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[START]
  *SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = lcKey
  
  *B610671,1 TMI 02/05/2014 21:33 [Start] comment this
  *lcDelSet = SET("Deleted")
  *SET DELETED OFF
  *B610671,1 TMI 02/05/2014 21:33 [End  ] 
  
  =gfSeek(lcKey)  
  
  *B610671,1 TMI 02/05/2014 21:30 [Start] comment this loop
  *SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = lcKey FOR (CTRCODE = lcSessionNo  OR EMPTY(CTRCODE))
  *  IF lcCostMethod = 'F'
  *    IF !llDeleteIssue
  *      REPLACE cSessNo WITH lcSessionNo IN  'TmpJrBn'
  *    ENDIF
  *    lcInvRSession =IIF(STYINVJL.CIRTYPE ='I',STYINVJL.crsession,'')
  *    IF STYINVJL.CIRTYPE ='I' AND llDeleteIssue
  *      FOR lnCnSty =  1 TO 8
  *        lcCnSty = STR(lnCnSty ,1)
  *        REPLACE STYDYE.STK&lcCnSty. WITH STYDYE.STK&lcCnSty.+ (-1 * STYINVJL.nSTK&lcCnSty.)
  *      ENDFOR
  *      REPLACE STYDYE.TotStk WITH STYDYE.STK1+STYDYE.STK2+STYDYE.STK3+STYDYE.STK4+STYDYE.STK5+STYDYE.STK6+STYDYE.STK7+STYDYE.STK8,;
  *        STYDYE.nSTkVAL WITH STYDYE.TotStk * STYDYE.AVE_COST
  *      FOR lnCnSty =  1 TO 8
  *        lcCnSty = STR(lnCnSty ,1)
  *        REPLACE STYLE.STK&lcCnSty. WITH STYLE.STK&lcCnSty. +(-1 * STYINVJL.nSTK&lcCnSty.)
  *      ENDFOR
  *      REPLACE STYLE.TotStk WITH STYLE.STK1+STYLE.STK2+STYLE.STK3+STYLE.STK4+STYLE.STK5+STYLE.STK6+STYLE.STK7+STYLE.STK8,;
  *        STYLE.nSTkVAL WITH STYLE.TOTSTK * STYLE.AVE_COST
  *      SELECT STYINVJL
  *      DELETE
  *    ENDIF
  *
  *    IF !EMPTY(lcInvRSession)
  *      IF gfSeek(STYINVJL.CWARECODE+PADR(lcBinLoc,10)+STYINVJL.STYLE,'BININVJL','WHBINSTY')&&
  *        *+STYINVJL.CTRCODE+STR(STYINVJL.LINENO,6) +STYINVJL.cirtype+STYINVJL.CSESSION+STYINVJL.crsession+DTOS(STYINVJL.DTRDATE)
  *        SELECT BININVJL
  *        LOCATE FOR crsession = STYINVJL.crsession AND cirtype ='R'
  *        IF FOUND()
  *          SCATTER MEMO MEMVAR
  *          m.CSESSION = TmpJrBn.cSessNo
  *          m.CISESSION = TmpJrBn.cSessNo
  *          m.ctrcode = IIF(m.cTrType $ "12" AND EMPTY(cTrCode),cSession,m.cTrCode) &&SPACE(6)
  *          m.cirtype = 'I'
  *          m.ctrtype ='9'
  *          FOR lnZCnt = 1 TO 8
  *            lcZCnt = STR(lnZCnt ,1)
  *            m.nSTK&lcZCnt. = -1 * m.nSTK&lcZCnt.
  *          ENDFOR
  *          m.nTOTSTK =  -1 * m.nTOTSTK
  *          m.cadjreason = lcAdjReason
  *          m.Reference = ''
  *          m.dtrdate = ladata[6]
  *          =gfAppend('BININVJL',.T.)
  *        ENDIF
  *      ENDIF
  *    ENDIF
  *    SELECT STYINVJL
  *    IF DELETED() OR STYINVJL.CIRTYPE ='I'
  *      LOOP
  *    ENDIF
  *  ENDIF
  *  *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[END]
  *  SCATTER MEMVAR
  *  SELECT BININVJL
  *  m.cLocation = lcBinLoc
  *  =lfIncLineNO(@lnMaxLnNo)
  *  =gfAppend('BININVJL',.T.)
  *ENDSCAN
  *B610671,1 TMI 02/05/2014 21:31 [End  ] 
  *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[START]
  *B610671,1 TMI 02/05/2014 21:33 [Start] comment this
  *IF lcCostMethod = 'F'
  *  SET DELETED  &lcDelSet.
  *  IF llDeleteIssue
  *    SELECT STYINVJL
  *    =gfTableUpdate()
  *  ENDIF
  *ENDIF
  *B610671,1 TMI 02/05/2014 21:33 [End  ] 
  
  *B610671,1 TMI 02/05/2014 21:31 [Start] add lines to the BININVJL by looping over the MDINVNTL file
  lcTmpRSession = gfTempName()
  SELECT MDINVNTL
  =SEEK(  loFormset.lcPostScVar  )
  SCAN REST WHILE CBATTYPE+CLKBATCH+STYLE+COLOR+DYELOT+CLOCATION = loFormset.lcPostScVar

    lcBinLoc = MDINVNTL.clocation

    SELECT STYINVJL
    SCATTER MEMVAR 
    m.cLocation = MDINVNTL.clocation
    
    IF lcCostMethod $ 'FL'
      *1.	ISSUE
      *MDINVTL represents a line pre bin, you could get the lines of the style from the BININVJL and then apply the select statement of the function lfIsueCost on the BININVJL and get a temp cursor lcBININVJL
      *Index on sessions + bin
      *issue total on command 
      *remove lines with 0 stock
      *add all these sessions to BININVJL as issue using the session # from STYINVJL
      =gfSeek(MDINVNTL.CWARECODE+MDINVNTL.CLOCATION+MDINVNTL.STYLE,'BININVJL','WHBINSTY') 
      SELECT BININVJL 
      lcTmpCur = gfTempName()
      SELECT * FROM BININVJL INTO CURSOR (lcTmpCur)
      SELECT (lcTmpCur)
      INDEX ON CRSESSION TAG CRSESSION
      TOTAL ON CRSESSION TO (oAriaApplication.WorkDir+lcTmpRSession)
      SELECT 0
      USE (oAriaApplication.WorkDir+lcTmpRSession)
      DELETE FOR nstk1+nstk2+nstk3+nstk4+nstk5+nstk6+nstk7+nstk8 = 0
      LOCATE 
      SCAN         
        m.LINENO = m.LINENO + 1
        m.CSESSION = STYINVJL.CSESSION
        m.CISESSION = STYINVJL.CSESSION
        m.cirtype = 'I'
        m.ctrcode = IIF(m.cTrType $ "12" AND EMPTY(cTrCode),m.CSESSION,m.cTrCode)
        m.ctrtype ='9'
        FOR lnZCnt = 1 TO 8
          lcZCnt = STR(lnZCnt ,1)
          m.nSTK&lcZCnt. = -1 * nSTK&lcZCnt.
        ENDFOR
        m.nTOTSTK =  -1 * nTOTSTK
        m.nCost = MDINVNTL.COST
        m.nStkVal = MDINVNTL.COST*m.nTOTSTK 
        
        m.cadjreason = lcAdjReason
        m.Reference = ''
        m.dtrdate = ladata[6]
        
        SELECT BININVJL
        =gfAppend('BININVJL',.T.)
      ENDSCAN 
      USE IN (lcTmpRSession)
      ERASE (oAriaApplication.WorkDir+lcTmpRSession+'.*')
      USE IN (lcTmpCur)
      
    ELSE

      *- in case of not FIFO/LIFO costing method issue only the current record
      m.CSESSION = STYINVJL.CSESSION
      m.CISESSION = STYINVJL.CSESSION
      m.cirtype = 'I'
      m.ctrcode = IIF(m.cTrType $ "12" AND EMPTY(cTrCode),m.CSESSION,m.cTrCode)
      m.ctrtype ='9'
      FOR lnZCnt = 1 TO 8
        lcZCnt = STR(lnZCnt ,1)
        m.nSTK&lcZCnt. = -1 * MDINVNTL.OldSTK&lcZCnt.
      ENDFOR
      m.nTOTSTK =  -1 * MDINVNTL.OLDTOTSTK
      m.nStkVal = MDINVNTL.COST*m.nTOTSTK
      
      m.cadjreason = lcAdjReason
      m.Reference = ''
      m.dtrdate = ladata[6]
      SELECT BININVJL
      =gfAppend('BININVJL',.T.)
      
    ENDIF 
      
    *2. RECEIVE
    *Loop over MDINVTL and add a line to BININVJL per bin, use the stock values and the same above session # as Receive
    *Chang the scan loop to include the updates WHBINLOC while adding lines to bininvjl
    *Consider if there are any other files 
 
    m.nstk1 = MDINVNTL.STK1
    m.nstk2 = MDINVNTL.STK2
    m.nstk3 = MDINVNTL.STK3
    m.nstk4 = MDINVNTL.STK4
    m.nstk5 = MDINVNTL.STK5
    m.nstk6 = MDINVNTL.STK6
    m.nstk7 = MDINVNTL.STK7
    m.nstk8 = MDINVNTL.STK8
    m.ntotstk = MDINVNTL.totSTK
    m.nStkVal = MDINVNTL.totSTK*MDINVNTL.Cost
    
    m.CSESSION = STYINVJL.CSESSION
    m.CISESSION = ''
    m.CRSESSION = STYINVJL.CSESSION
    m.cirtype = 'R'
    m.ctrcode = IIF(m.cTrType $ "12" AND EMPTY(cTrCode),m.CSESSION,m.cTrCode)
    m.ctrtype ='9'
    m.cadjreason = lcAdjReason
    m.Reference = ''
    m.dtrdate = ladata[6]
    SELECT BININVJL
    =gfAppend('BININVJL',.T.)
    *B610671,1 TMI 02/05/2014 21:31 [End  ] 
  
    *B609847,1 MMT 02/28/2012 Inventory locking add extra issue records in Styinvjl table ub case of FIFO costing method and Bin-location used[END]
    SELECT STYINVJL
    SET ORDER TO &lcSvOrd
    *B608785 TMI 01/20/2009 [End  ]
    lcBatLin = loFormSet.lcBatLin
    SELECT WHBINLOC
  
    lcOldOrder = ORDER()
    =gfSetOrder('Whbinloc')  &&CWARECODE+CLOCATION+STYLE
    *B608785 TMI 01/20/2009 [Start] get the lines of the specified location only
    *=gfSEEK(&lcBatLin..cWarecode+&lcBatLin..clocation,'WHBINLOC')
    =gfSEEK(laData[8]+lcBinLoc,'WHBINLOC')
    *B608785 TMI 01/20/2009 [End  ]
  
    *B608785 TMI 01/22/2009 [Start]
    IF !USED('WHSLOC')
      =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    SELECT WHSLOC
    =gfSetOrder('WHSLOC')
    *B608785 TMI 01/22/2009 [End  ]
  
    SELECT (lcBatLin)
    SCAN FOR &lcBatLin..clocation = lcBinLoc
      *B608785 TMI 01/22/2009 [Start]
      *IF SEEK(&lcBatLin..cWarecode + &lcBatLin..clocation+&lcBatLin..Style,'WHBINLOC')
  
      =gfSEEK(&lcBatLin..cWareCode+&lcBatLin..cLocation,'WHSLOC')
      SELECT WHSLOC
      SCATTER FIELDS CBINCLASS, CBLKPCK, CSECTION MEMVAR
      IF !gfSeek(&lcBatLin..cWarecode + &lcBatLin..clocation+&lcBatLin..STYLE,'WHSLOC')
        m.cWarecode = &lcBatLin..cWarecode
        m.cLocation = &lcBatLin..cLocation
        m.Style     = &lcBatLin..STYLE
        gfAppend('WHSLOC',.T.)
        =gfTableUpdate()
      ENDIF
  
      IF !SEEK(&lcBatLin..cWarecode + &lcBatLin..clocation+&lcBatLin..STYLE,'WHBINLOC')
        SELECT WHBINLOC
        lcReplace = 'Style      WITH [' + &lcBatLin..STYLE     + '] ' +;
          'cWareCode  WITH [' + &lcBatLin..cWareCode + '] ' +;
          'clocation  WITH [' + &lcBatLin..cLocation + '] ' +;
          'cbinclass  WITH [' + m.cbinclass          + '] ' +;
          'cBlkPck    WITH [' + m.cBlkPck            + '] ' +;
          'cSection   WITH [' + m.cSection           + '] ' +;
          'cAdd_User  WITH [' + oAriaApplication.User_ID  + '] '+;
          'dAdd_Date  WITH {^'+STR(YEAR(DATE()),4)+'-'+STR(MONTH(DATE()),2)+'-'+STR(DAY(DATE()),2) +'} '+;
          'cAdd_Time  WITH [' + gfGetTime()           + '] '
  
        SELECT WHBINLOC
        =gfAppend()
        =gfReplace(lcReplace)
      ENDIF
      *B608785 TMI 01/22/2009 [End  ]
      SELECT WHBINLOC
      lcReplace =  'Qty1   WITH '+STR( MAX(&lcBatLin..stk1,0) )+' '+;
        'Qty2   WITH '+STR( MAX(&lcBatLin..stk2,0) )+' '+;
        'Qty3   WITH '+STR( MAX(&lcBatLin..stk3,0) )+' '+;
        'Qty4   WITH '+STR( MAX(&lcBatLin..stk4,0) )+' '+;
        'Qty5   WITH '+STR( MAX(&lcBatLin..stk5,0) )+' '+;
        'Qty6   WITH '+STR( MAX(&lcBatLin..stk6,0) )+' '+;
        'Qty7   WITH '+STR( MAX(&lcBatLin..stk7,0) )+' '+;
        'Qty8   WITH '+STR( MAX(&lcBatLin..stk8,0) )+' '
      =gfReplace(lcReplace)
      lcReplace =  'TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8'
      =gfReplace(lcReplace)
      *B608785 TMI 01/22/2009 [Start]
      *ENDIF
      *B608785 TMI 01/22/2009 [End  ]
  
      *B608785 TMI 01/23/2009 [Start] check if the whbinloc goes to zero then remove it
      IF WHBINLOC.Qty1 = 0 .AND. WHBINLOC.Qty2 = 0 .AND. WHBINLOC.Qty3 = 0 .AND. WHBINLOC.Qty4 = 0 .AND. ;
          WHBINLOC.Qty5 = 0 .AND. WHBINLOC.Qty6 = 0 .AND. WHBINLOC.Qty7 = 0 .AND. WHBINLOC.Qty8 = 0 .AND. ;
          gfGetMemVar('M_DELZRBNR')
        =gfDelete()
      ENDIF
      *B608785 TMI 01/23/2009 [End  ]
    ENDSCAN
    SELECT WHBINLOC
    =gfTableUpdate()
    =gfSetOrder(lcOldOrder)
  
    *B608785 TMI 01/20/2009 [Start] update the BININVJL
    SELECT BININVJL
    =gfTableUpdate()
    *B608785 TMI 01/20/2009 [End  ]
  
    *B610671,1 TMI 02/06/2014 18:43 [Start] close the scan loop over the MDINVNTL 
  ENDSCAN
  *B610671,1 TMI 02/06/2014 18:43 [End  ] 

  SELECT MDINVNTL
  
ELSE

  SELECT &lcTmpAdj
  *[Tarek:  Don't enable this change for now, i.e. B610676, until a redesign for this case take place ]
  *B610676,1 TMI 02/10/2014 14:12 [Start] use the standard functin instead of the custom function 
  lnRet=lfStyCrl(lcAdjTyp,STYLE,lcAdjWareH,Dyelot,DATE,'',@laAdjust,lnACost,;
    lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  *lnRet = gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)    
  *B610676,1 TMI 02/10/2014 14:13 [End  ] 
  
  *B610831,1 TMI 09/01/2014 11:22 [Start] check if lnRet is 0
  IF lnRet = 0
    SELECT &lcTmpAdj
    BLANK
    DELETE 
  ENDIF 
  *B610831,1 TMI 09/01/2014 11:22 [End  ] 
    
ENDIF


*B608785 TMI 01/20/2009 [Start]
SELECT (lnSlct)
*B608785 TMI 01/20/2009 [End  ]
*-- End of Function lfDLSTYCRL.

*!***************************************************************************
*!* Name        : lfSTYCRL
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : A Copy of GfStyCrl to be Applicable with Bin Location
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSTYCRL()
*!***************************************************************************
FUNCTION lfSTYCRL
PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,laAdjStk,;
  lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,lnStarStep,lcTmpLFile,lcStepFld,;
  laGLInvAry,lnLineNo,lcLastRSess,lcAdjRef,laLockInfo, lnTranCost


*--Initialize function variables.
PRIVATE lcOldWAr,laOldstk,llChekUncmp,lnSAveCost,lnWAveCost,lnSOldStk,lnSOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
  lcAdjAcct,lcTmpJour,lcInvJour,lnRetStep,llUInvtry,lnSStkVal,lnWStkVal,lnTranCost,lnStkVal,lnLineNo,;
  lcLastRSess,lcAdjRef,lnDyeCost,lnPrvQty,lnPrvVal

STORE 0 TO lnStkVal,lnTranCost,lnSaveCost,lnSOldStk,lnSOldCst,lnWaveCost,lnWOldStk,lnWOldCst,laOldstk

IF TYPE('lnLineNo') = 'L'
  lnLineNo = 0
ENDIF
IF TYPE('lcLastRSess') = 'L'
  lcLastRSess = SPACE(6)
ENDIF
IF TYPE('lcAdjRef') = 'L'
  lcAdjRef = SPACE(6)
ENDIF
*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
*IF lcTrType$'29' AND !(TYPE('lcType') = 'U')
IF lcTrType$'129' AND !(TYPE('lcType') = 'U')
  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]
  PRIVATE lcFileNama
  lcBinLoc = &lcTmpAdj..LocFrom
ENDIF

lcSysType = gfGetMemVar('M_SYSTYPE')
IF lcSysType = 'P'
  PRIVATE llOpnWarhs
  llOpnWarhs = .F.
  IF !USED('WAREHOUS')
    llOpnWarhs = gfOpenTable(oAriaApplication.DataDir+"WAREHOUS","WAREHOUS","SH")
  ENDIF
  IF gfSEEK(lcWareCode,'WAREHOUS') AND WAREHOUS.cSiteId <> gcCurSite
    IF llOpnWarhs
      USE IN STYINVJL
    ENDIF
    RETURN (1)
  ENDIF
ENDIF
IF !USED('BININVJL')
  =gfOpenTable(oAriaApplication.DataDir+'BININVJL','Styinvjl','SH')
ENDIF
IF !USED('WHBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

*--Style and Warehouse Average Cost,Old Stock and Old Cost variables.

DIME laOldstk[9]
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcSDyelot  = IIF(EMPTY(lcSDyelot),SPACE(10),lcSDyelot)

*--Check Uncomplete session flag if steps are passed as value not as zero.
llChekUncmp = ( lnStarStep <> 0 )
*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,IIF(EMPTY(laGLInvAry[1,1]),.F.,.T.))
*--Return step to continue for after exit the function.
lnRetStep   = 0
*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_Cost_Meth')

*--Check the existing of the style and
*--Point the record in style and style dyelot files.
IF ! gfSEEK(lcStyle,'STYLE') OR !gfSEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  *--The style ???? record are missing,
  *--Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  =gfModalGen('TRM42114B42000','DIALOG',lcStyle)
  RETURN (0)
ENDIF


*--Check if StyInvJL file is Open.
llOpnJurnl = gfOpenTable(oAriaApplication.DataDir+"StyInvJl","StyInvJl","SH")

*T20060817.0014,3 tmi [start] important to add minuse signe in issuing case
*--Case of Invoice or Void credit memo transactions, Put the Qty as
*--negative values since it is an Issue transaction.
*--If it is an Inventory adjustment or Receve return P/o the Qty must
*--Passes to this function as negative values since we have a Receive
*--case for this Inventory adj. and Receive P/o.
IF lcTrType $ '38'
  FOR lnI=1 TO 9
    laAdjStk[lnI] = -(ABS(laAdjStk[lnI]))
  ENDFOR
ENDIF
*T20060817.0014,3

*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '123689I' , 'I' , 'R' )

*--Check the style Inventory Yes or No.
llUInvtry = STYLE.lInvSty
*--Get the Old Stock and Cost before updateing the new tansaction.

lnSOldStk = STYLE.TotStk
lnSOldCst = ABS(IIF(STYLE.TotStk=0,STYLE.Ave_Cost,STYLE.nStkVal / STYLE.TotStk))
lnWOldStk = STYDYE.TotStk

IF lcTrType = '9'
  lnWOldCst = IIF(laLockInfo[9]=0,0,laLockInfo[10]/laLockInfo[9])
ELSE
  lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,STYDYE.nStkVal / StyDye.TotStk))
ENDIF

*--Stock Value variable for style and StyDye.
lnSStkVal = IIF(lcTrType $ '29',0,STYLE.nStkVal )
lnOldSVal = IIF(lcTrType $ '29',STYDYE.nStkVal,0)

IF lcTrType = '9'
  lnWStkVal = laLockInfo[10]
ELSE
  lnWStkVal = IIF(lcTrType $ '29',0,STYDYE.nStkVal)
ENDIF

PRIVATE lnDyeCost
lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = StyDye.TotStk
lnPrvVal = StyDye.nStkVal
IF !EMPTY(lcSDyelot) AND gfSEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  lnPrvQty = StyDye.TotStk
  lnPrvVal = StyDye.TotStk * lnDyeCost
ENDIF

IF lcIRType = 'I' AND (lcTrType <> 'I' OR lcCostMeth $ 'FL')

  DO CASE
  CASE lcCostMeth = 'A'   && Average.
    lnNewCost = IIF((gfGetMemVar('M_WareHouse')='Y'),lnWOldCst ,lnSOldCst )

  CASE lcCostMeth = 'S'   && Standard.
    lnNewCost = STYLE.TotCost

  CASE lcCostMeth $ 'FL'  && FIFO or LIFO.
    *--In this case may be has more than cost so we hold this costs
    *--in lcTmpJour file that the following function will return.
    lcTmpJour = gfTempName()
    IF ! lfIsueCost(.F.)
      SELECT (lcOldWAr)
      RETURN (0)
    ENDIF
  ENDCASE
ENDIF

IF lcTrType = '9'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
  SELECT STYDYE
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
ENDIF

*--Temp Old Stock Array used in Physical or Markdown
*--issue the old stock first and then receive the pysical quantity.
*-- In all other cases this array is Zero.
IF lcTrType $ '2'
  SELECT WHBINLOC
  IF !EMPTY(lcBinLoc)
    =gfSEEK(lcWareCode+lcBinLoc+lcStyle,'WHBINLOC')
  ENDIF
  FOR lnCnt = 1 TO 8
    lcCnt = STR(lnCnt, 1)
    laOldstk[lnCnt] = WHBINLOC.Qty&lcCnt
  ENDFOR
  laOldstk[9] = WHBINLOC.TotQty
  lnOldSVal = laOldstk[9] * STYDYE.AVE_COST
  lnPrvQty  = laOldstk[9]
  lnPrvVal  = laOldstk[9] * STYDYE.AVE_COST
ENDIF

*--Calculate Avarage Cost for Style and StyDye records.

*-- 1)  Update Style journal file. -------------------------------------
*--Read session no.
*--If receiving transaction and costing methos Lifo or Fifo make sure
*--that the session not duplecated fir same key.
lcRISessn = IIF( TYPE('lcRISessn')='C', lcRISessn ,'' )
IF lcIRType = 'R' AND lcCostMeth $ 'FL' AND !EMPTY(lcRISessn)
  lnJrlRec  = IIF(EOF('STYINVJL'),0,RECNO('STYINVJL'))
  lcRISessn = IIF(gfSEEK(lcStyle+lcWareCode+lcRISessn,'STYINVJL'),'',lcRISessn)
  IF lnJrlRec<>0
    GOTO lnJrlRec IN STYINVJL
  ENDIF
ENDIF
*--Read session no.
IF EMPTY(lcRISessn)
  lcRISessn = gfSequence('GLSESSION')
ENDIF

*--Read the adjustment code reason to get the GL Account.
IF !EMPTY(lcAdjCdRsn)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjCdRsn , @laTrmRltFd , "CADJREASON")
ENDIF

*--Initialize next step to continue.
lnTmpStp = lnStarStep

*--Update journal for Issue Transaction ,FIFO or LIFO method.
IF lcIRType = 'I' AND lcCostMeth $ 'FL'

  lnIssTCst = 0
  lnIssTStk = 0

  SELECT (lcTmpJour)
  *T20060817.0014,3   TMI [Start] remove the "cISession" from the key to not update key field within the scan loop
  INDEX ON STYLE+cWareCode+cDyelot+cRSession TAG &lcTmpJour
  *T20060817.0014,3   TMI [End  ]
  SCAN
    REPLACE cSession  WITH lcRISessn,;
      cISession WITH cSession,;
      cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,cTrCode)
    SCATTER MEMVAR
    *T20071102.0018,9 TMI 04/29/2008 [Start] this function is no more used in aria4xp , it must be removed from the code
    *                     currently I made it to always return .T.
    *                     You should make a more larger changes to use the standard gfStycrl function , not this one
    IF lfCheckUnCmp(lnTmpStp)
      *T20071102.0018,9 TMI 04/29/2008 [End  ]

      SELECT STYINVJL
      APPEND BLANK
      GATHER MEMVAR
      lnIssTCst = lnIssTCst + m.nTotStk * m.nCost
      lnIssTStk = lnIssTStk + m.nTotStk
      REPLACE REFERENCE  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
        cAdjReason WITH lcAdjCdRsn,;
        cAdjAcct   WITH lcAdjAcct,;
        nStkVal    WITH nTotStk * nCost,;
        LINENO     WITH lnLineNo,;
        nPrvSQty   WITH lnPrvQty,;
        nPrvSval   WITH lnPrvVal

      REPLACE cAdjRef    WITH lcAdjRef

      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')

      *--in case of Inventory lock.
      IF lcTrType='9'
        SCATT MEMVAR MEMO
        m.clocation = lcBinLoc
        SELECT BININVJL
        =gfAppend('BININVJL',.T.)
      ENDIF

      *--Update Uncomplete session Step.
      *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
      *=lfUpdStep(lnTmpStp)
      *T20071102.0018,9 TMI 04/29/2008 [End  ]

      *--Call TraceKey global function.
      =gfTraceKey('STYINVJL',STYINVJL.STYLE+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.LINENO,6),'A')
    ENDIF
    lnTmpStp = lnTmpStp + 1

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfStyWarDy()
  ENDSCAN
  IF lnIssTStk <> 0
    lnIssAvg = ABS(lnIssTCst / lnIssTStk )
  ELSE
    lnIssAvg = 0
  ENDIF

  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  =lfEraseFil(lcTmpJour)

ELSE  && Not LIFO or FIFO or Receiving.

  *--Create an issue record for Physical inventory or
  *--Markdown inventory transaction in Style inventory Journal.

  IF lcTrType $ '29'
    IF lfDoPhys('I')
      =lfIsuJlTr()
      STORE 0 TO lnPrvQty,lnPrvVal
    ENDIF
  ENDIF

  IF !(lcTrType $ '29') AND lcIRType = 'R' AND lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    *-- This is to create 2 records in journal file
    *-- one for rec. the qty with it's old cost
    *-- the other for issue the qty with it's new cost
    =lfAdjRec()
  ENDIF

  *--Create a main record in journal file.
  IF !(lcTrType $ '29') OR (lcTrType $ '29' AND lfDoPhys('R'))
    IF lfCheckUnCmp(lnTmpStp)
      SELECT STYINVJL
      lnStkVal = laAdjStk[9] * lnNewCost
      APPEND BLANK
      REPLACE cSession   WITH lcRISessn,;
        STYLE      WITH lcStyle,;
        cWareCode  WITH lcWareCode,;
        cDyelot    WITH lcSDyelot,;
        dTrDate    WITH ldTrDate,;
        cTrType    WITH lcTrType,;
        cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
        nCost      WITH lnNewCost,;
        cIRType    WITH lcIRType,;
        nStk1      WITH laAdjStk[1],;
        nStk2      WITH laAdjStk[2],;
        nStk3      WITH laAdjStk[3],;
        nStk4      WITH laAdjStk[4],;
        nStk5      WITH laAdjStk[5],;
        nStk6      WITH laAdjStk[6],;
        nStk7      WITH laAdjStk[7],;
        nStk8      WITH laAdjStk[8],;
        nTotStk    WITH laAdjStk[9],;
        nStkVal    WITH lnStkVal   ,;
        REFERENCE  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
        lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
        cAdjReason WITH lcAdjCdRsn ,;
        cAdjAcct   WITH lcAdjAcct  ,;
        cISession  WITH IIF(cIRType='I',cSession,''),;
        cRSession  WITH IIF(cIRType='R',cSession,''),;
        LINENO     WITH lnLineNo,;
        nPrvSQty   WITH lnPrvQty,;
        nPrvSVal   WITH lnPrvVal

      REPLACE cAdjRef    WITH lcAdjRef

      REPLACE nTranCost WITH lnTranCost

      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')
      *--in case of inventory lock
      IF lcTrType='9'
        SCATT MEMVAR MEMO
        m.clocation = lcBinLoc
        SELECT BININVJL
        =gfAppend('BININVJL',.T.)
      ENDIF

      *--Update Uncomplete session Step.
      *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
      *=lfUpdStep(lnTmpStp)
      *T20071102.0018,9 TMI 04/29/2008 [End  ]

      *--Call TraceKey global function.
      =gfTraceKey('STYINVJL',STYINVJL.STYLE+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.LINENO,6),'A')
    ENDIF
    lnTmpStp = lnTmpStp + 1

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfStyWarDy()

    IF lcTrType = '9'
      = lfLkAdjRec()
    ENDIF
  ENDIF
ENDIF

*--Initialize next step to continue when return.
lnRetStep = lnTmpStp

*T20071102.0018 TMI [Start] This file may be used
*!*	*--Close style journal if this function open it.
*!*	IF llOpnJurnl AND USED("StyInvJl")
*!*	  USE IN STYINVJL
*!*	ENDIF
*T20071102.0018 TMI [End  ]

SELECT (lcOldWAr)
RETURN IIF(llChekUncmp , lnRetStep , 1 )
*-- End of Function lfSTYCRL.


*:**************************************************************************
*:* Name        : lfAddInfo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Add/Edit user , time & date
*:***************************************************************************
FUNCTION lfAddInfo
LPARAMETERS llAdd
LOCAL lcRet,lcDate
lcRet = ''
lcDate = STR(YEAR(DATE()),4)+'-'+STR(MONTH(DATE()),2)+'-'+STR(DAY(DATE()))
IF llAdd
  lcRet = IIF(TYPE('cAdd_User')<>'U', 'cAdd_User  WITH ['+oAriaApplication.User_ID+'] ' , '' )+;
    IIF(TYPE('dAdd_Date')<>'U', 'dAdd_Date  WITH {^'+lcDate+'} ' , '' )                 +;
    IIF(TYPE('cAdd_Time')<>'U', 'cAdd_Time  WITH ['+gfGetTime() + '] ' , '' )
ELSE
  lcRet = IIF(TYPE('cEDIT_User')<>'U', 'cEdit_User  WITH ['+oAriaApplication.User_ID+'] ' , '' )+;
    IIF(TYPE('dEDIT_Date')<>'U', 'dEdit_Date  WITH {^'+lcDate+'} ' , '' ) +;
    IIF(TYPE('cEDIT_Time')<>'U', 'cEdit_Time  WITH ['+gfGetTime() + '] ' , '' )
ENDIF
RETURN lcRet
*-- end of lfAddInfo.

*!***************************************************************************
*!* Name        : lfDLRCVQTY
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : get received data screen for Bins logic
*!***************************************************************************
*!* Called from : POSTREC.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLRCVQTY()
*!***************************************************************************
FUNCTION lfDLRCVQTY

IF !lfIsUseBin()
  RETURN
ENDIF

lnAlias = SELECT()
=lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC","")

PRIVATE laBinLoc
DIMENSION laBinLoc[8]
laBinLoc = ''

SELECT (loFormSet.lcTmpLine)
lnRecno = RECNO()
SCATTER FIELDS PO,STYLE,LINENO MEMVAR
SCAN REST FOR PO+STYLE+STR(LINENO ,6) = m.PO+m.Style+STR(m.Lineno ,6) AND TRANCD>'2'
  SCATTER FIELDS LOC1,LOC2,LOC3,LOC4,LOC5,LOC6,LOC7,LOC8 TO laBinLoc
  EXIT
ENDSCAN
TRY
  GOTO lnRecno IN (loFormSet.lcTmpLine)
CATCH
ENDTRY

DO FORM (oAriaApplication.ScreenHome+'PODLRCV.SCX')
*- save the labinloc array in a mem file
SAVE TO (oAriaApplication.WorkDir+loFormSet.lcTmpLine+'.mem') ALL LIKE laBinLoc*

SELECT(lnAlias)
*-- End of Function lfDLRCVQTY.

*:**************************************************************************
*:* Name        : lfUPBNLOC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Update bin location fields for 2nd/ damaged quality fields
*:***************************************************************************
*:* Called from : lfvEditQty IN POSTREC.PRG
*:***************************************************************************
FUNCTION lfUPBNLOC

IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lnSlct,lnRecno,m.PO,m.Style,m.Lineno,laBinloc
LOCAL lcTmpLine
*T20071102.0018,10/C200876 TMI 05/26/2008 [Start]
*lcTmpLine = loFormSet.lcTmpLine
lcTmpLine = lcTmpLn
*T20071102.0018,10/C200876 TMI 05/26/2008 [End  ]
RESTORE FROM (oAriaApplication.WorkDir+lcTmpLine+'.mem') ADDITIVE
ERASE (oAriaApplication.WorkDir+lcTmpLine+'.mem')

lnSlct = SELECT()

SELECT &lcTmpLine
lnRecno = RECNO()
SCATTER FIELDS PO,STYLE,LINENO MEMVAR
SCAN REST FOR PO+STYLE+STR(LINENO ,6) = m.PO+m.Style+STR(m.Lineno ,6) AND TRANCD>'2'
  GATHER FIELDS LOC1,LOC2,LOC3,LOC4,LOC5,LOC6,LOC7,LOC8 FROM laBinLoc
ENDSCAN

TRY
  GOTO lnRecno IN &lcTmpLine
CATCH
ENDTRY


SELECT (lnSlct)
*-- end of lfUPBNLOC.

*!*************************************************************
*!* Name      : lfUpdGLDist()
*!* Developer : Timour A. K.
*!* Date      : 01/22/98
*!* Module    : Inventory Control (IC)
*!* Purpose   : Update Temp G/L Distribution file.
*!*************************************************************
*:      [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:      [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:      [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:      [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*!*************************************************************
*! Call      : GLDIST
*!*************************************************************
*! Example   : =lfUpdGLDist()
*!*************************************************************
FUNCTION lfUpdGLDist
*-- llNegStkAd Showes if it is main record (Start)
*-- or it is adj. record because the stock is less than Zero
*-- AAMER 11/22/98
PARAMETERS llNegStkAd,llLockAdj

*-- llNegStkAd Showes if it is main record (End)
PRIVATE lnCurAlias

*--Donot update if no GL used.
IF ! llGLUsed
  RETURN
ENDIF

*-- This means it is Main Record (Start)

IF !llNegStkAd
  *-- This means it is Main Record (End)

  *--Update Gl for Main inventory record for Isue or Receive.
  *- Receiving Trans.(+1,2,4,5,+6,7):    None
  *-  => +/-  lnAmount = Total Recv. Qty * New Recv. Cost
  *- Issue Trans.(-1,-3,-6,-8,-2)     :  None
  *-  => +/-  lnAmount = Total Issue Qty * Issue Cost
  FOR lnAln=1 TO ALEN(laGLInvAry,1)

    laGLInvAry[lnAln,5] = STYINVJL.cTrCode

    IF lfCheckUnCmp(lnTmpStp)

      lnGLEnAmount = STYINVJL.nStkVal * laGLInvAry[lnAln,3]

      DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2],lnGLEnAmount,laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
        IIF(llLockAdj,laLockInfo[11],laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7],laGLInvAry[lnAln,8]     ,;
        laGLInvAry[lnAln,9],laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
      DO CASE
      CASE &laGLInvAry[lnAln,9]..catg_Key = '006'
        lnCurAlias = SELECT(0)
        SELECT StyInvJl
        REPLACE cICAcnt WITH &laGLInvAry[lnAln,9]..GLAccount
        SELECT (lnCurAlias)

        *--update cadjact field in all cases

        *--Updae cAdjAcct if it is empty in all catg_keys (Start) AAMER 04/13/99
        *--not if it is empty and catg_key = '007'
      CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR EMPTY(StyInvJl.cAdjAcct)
        *--Updae cAdjAcct if it is empty in all catg_keys (End)
        lnCurAlias = SELECT(0)
        SELECT StyInvJl
        REPLACE cAdjAcct WITH &laGLInvAry[lnAln,9]..GLAccount
        SELECT (lnCurAlias)
      ENDCASE

      *--Update Uncomplete session Step.
      *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
      *=lfUpdStep(lnTmpStp)
      *T20071102.0018,9 TMI 04/29/2008 [End  ]
    ENDIF
    lnTmpStp = lnTmpStp + 1
  ENDFOR

  *-- This means it is Adj. Record (Start)
ELSE

  lcStyLink = IIF(EMPTY(StyDye.GL_Link),STYLE.Link_Code,StyDye.GL_Link)
  DO GLDIST WITH lcStyLink,'006',StyInvJl.nStkVal,'IA',;
    StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
    laGLInvAry[1,8],laGLInvAry[1,9],'','','',''
  lnCurAlias = SELECT(0)
  SELECT StyInvJl
  REPLACE cICAcnt WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  *--Update Uncomplete session Step.
  *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
  *=lfUpdStep(lnTmpStp)
  *T20071102.0018,9 TMI 04/29/2008 [End  ]
  lnTmpStp = lnTmpStp + 1

  DO GLDIST WITH lcStyLink,'007',-StyInvJl.nStkVal,'IA',StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],;
    StyInvJl.DtrDate),laGLInvAry[1,7],laGLInvAry[1,8],laGLInvAry[1,9],StyInvJl.cAdjAcct,'','',''

  IF EMPTY(StyInvJl.cAdjAcct)
    lnCurAlias = SELECT(0)
    SELECT StyInvJl
    REPLACE cAdjAcct WITH &laGLInvAry[1,9]..GLAccount
    SELECT (lnCurAlias)
  ENDIF
  *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
  *--Update Uncomplete session Step.
  *=lfUpdStep(lnTmpStp)
  lnTmpStp = lnTmpStp + 1
ENDIF
RETURN
*!***************************************************************************
*!* Name        : lfStyWarDy
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update the StyDye file
*!***************************************************************************
*!* Called from : BN4MAIN.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfStyWarDy()
*!***************************************************************************
FUNCTION lfStyWarDy

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
PRIVATE llNew_Cost
llNew_Cost = .F.

*-- what we do in case of avarage cost lcCostMeth = 'A' ??
IF lcIRType = 'R' .AND. !(lcTrType = '1' .AND. lcCostMeth $ 'FLA') .AND. !(lcTrType = '4' .AND. lcCostMeth $ 'FL' ) ;
    .AND. ( !(lcTrType $ '29') .OR. (lcTrType $ '29' AND lfDoPhys('R')))
  llNew_Cost = .T.
ENDIF
lnValDiff=0
*--1 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYDYE
  =SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  =RLOCK()
  lnPrvStk  = TotStk   && Old Stock
  lnStkVal  = nStkVal  && Old Stock Value
  lnAveCost = Ave_Cost && Old Average Cost
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
    Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
    Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
    Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
    Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
    Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
    Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
    Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
    TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
  IF StyInvJl.nTotStk > 0                && Receive transaction
    IF lnPrvStk < 0                      && The stock was negative

      IF StyInvJl.nTotStk + lnPrvStk < 0   && The stock still negative after receiving
        IF llNew_Cost
          lnStkVal = lnStkVal + StyInvJl.nTotStk * lnNewCost
        ELSE
          lnStkVal = lnStkVal +  StyInvJl.nTotStk * lnAveCost
        ENDIF
      ELSE                               && Use the transaction cost if the stock will be > 0
        lnStkVal = (StyInvJl.nTotStk+lnPrvStk) * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
      ENDIF
    ELSE
      lnStkVal = lnStkVal + (StyInvJl.nTotStk * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost))
    ENDIF
  ELSE                                   && Issue transaction
    IF lnPrvStk = 0                      && If it is the 1st transaction for this style or the stock became 0.
      lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
    ENDIF
    lnStkVal = TotStk * IIF(llNew_Cost,lnNewCost,IIF(lnprvstk=0,lnAveCost,lnstkval/lnprvstk))
  ENDIF
  IF TotStk = 0 AND StyInvJl.nTotStk > 0
    lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
  ENDIF
  IF TotStk > 0
    lnAveCost = IIF(llNew_Cost,lnNewCost,lnStkVal/TotStk)
  ENDIF
  IF lcIRType='I' AND !EMPTY(lcLastRSess)
    lnAveCost = IIF(TotStk=0,StyInvJl.nCost,lnStkVal/TotStk)
  ENDIF
  lnValDiff = lnStkVal - nStkVal
  REPLACE StyDye.nStkVal  WITH IIF(TotStk=0,0,lnStkVal),;
    StyDye.Ave_Cost WITH lnAveCost
  UNLOCK
  *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
  *--Update Uncomplete session Step.
  *=lfUpdStep(lnStarStep)
  *--Call TraceKey global function.
  =gfTraceKey('STYDYE',STYDYE.STYLE+STYDYE.cWareCode+STYDYE.Dyelot,'M')
  lnPrvQty  = TotStk
  lnPrvVal  = nStkVal
  lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
ENDIF
lnTmpStp = lnTmpStp + 1
*--2 ) Update Stock and Avarege cost in Style file. ------------
IF lfCheckUnCmp(lnStarStep+1)
  SELECT STYLE
  =RLOCK()
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
    Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
    Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
    Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
    Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
    Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
    Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
    Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
    TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
    nStkVal  WITH nStkVal + lnValDiff ,;
    Ave_Cost WITH IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
  UNLOCK
  *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
  *--Update Uncomplete session Step.
  *=lfUpdStep(lnStarStep+1)
  *--Call TraceKey global function.
  =gfTraceKey('STYLE',STYLE.STYLE,'M')
ENDIF
lnTmpStp = lnTmpStp + 1
*--3 )  Update Stock in Style Dyelot file Dyelot record. --------

*- in Case Of Reciving PO and the cost method is Avarage Cost
IF lcIRType = 'R' .AND. lcTrType = '6' .AND. lcCostMeth = 'A'
  =SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  REPLACE STYDYE.Ave_Cost WITH STYLE.Ave_Cost
ENDIF
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  IF lfCheckUnCmp(lnStarStep+2)
    SELECT STYDYE
    =RLOCK()
    REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
      Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
      Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
      Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
      Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
      Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
      Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
      Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
      TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
    UNLOCK
    *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
    *--Update Uncomplete session Step.
    *=lfUpdStep(lnStarStep+2)
    *--Call TraceKey global function.
    =gfTraceKey('STYDYE',STYDYE.STYLE+STYDYE.cWareCode+STYDYE.Dyelot,'M')
    lnPrvQty = TotStk
    lnPrvVal = TotStk * lnDyeCost
  ENDIF
ENDIF
lnTmpStp = lnTmpStp + 1
SELECT(lnCurAlias)
*-- End of Function lfStyWarDy.

*!***************************************************************************
*!* Name        : lfDoPhys
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check if we can issue or receive in case of inventory Locking.
*!***************************************************************************
*!* Called from : BN4MAIN.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : .T. OR .F.
*!***************************************************************************
*!* Example     : = lfDoPhys()
*!***************************************************************************
FUNCTION lfDoPhys
PARAMETERS lcRI

PRIVATE lcRI
lnRet = .F.
DO CASE
CASE lcRI = 'I'
  *-- Issue if the old stock value or the old stock qty doen't equal zero
  *-- or the cost value has changed or the balance has changed.
  lnRet = lnOldSVal   # 0 OR laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
    laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR lnWOldCst # lnNewCost OR ;
    laOldstk[9] # laAdjStk[9]
CASE lcRI = 'R'
  *-- Receive only if the Issue record hasn't been issued or the new balance is not zero or cost value has changed
  lnRet = !(lnOldSVal   # 0 OR laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
    laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR lnWOldCst   # lnNewCost OR ;
    laOldstk[9] # laAdjStk[9]      )   OR laAdjStk[9] # 0 OR lnWOldCst   # lnNewCost
ENDCASE
RETURN lnRet
*-- End of Function lfDoPhys.

*!***************************************************************************
*!* Name        : lfLkAdjRec
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     :
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfLkAdjRec()
*!***************************************************************************
FUNCTION lfLkAdjRec
PRIVATE lcJourTag,lnCurAlias,lnTotQty,lnTotVal,lnTranVal,lnDiffere
lnCurAlias = SELECT(0)
SELECT StyInvJl
lcJourTag = ORDER('StyInvJl')
SET ORDER TO StyInvJl
lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0
IF SEEK(lcStyle+lcWareCode+lcRISessn,'StyInvJl')
  SCAN REST WHILE STYLE+cWareCode+cSession+DTOS(dTrDate)+cTrCode=lcStyle+lcWareCode+lcRISessn FOR cDyelot=lcSDyelot AND !lLockFlg
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
    lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
  ENDSCAN
  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0 AND lfCheckUnCmp(lnTmpStp)
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn       ,;
      STYLE      WITH lcStyle         ,;
      cWareCode  WITH lcWareCode      ,;
      cDyelot    WITH lcSDyelot       ,;
      dTrDate    WITH laLockInfo[11]  ,;
      cTrType    WITH lcTrType        ,;
      cTrCode    WITH IIF(EMPTY(lcTrCode),lcRISessn,lcTrCode),;
      cIRType    WITH IIF(lnDiffere<0,'I','R'),;
      nStkVal    WITH lnDiffere       ,;
      REFERENCE  WITH 'Mark Down Adjustement Value',;
      cAdjReason WITH lcAdjCdRsn      ,;
      cAdjAcct   WITH lcAdjAcct       ,;
      cISession  WITH IIF(lnDiffere<0,cSession,''),;
      cRSession  WITH IIF(lnDiffere>0,cSession,''),;
      nPrvSQty   WITH lnPrvQty        ,;
      nPrvSVal   WITH lnPrvVal        ,;
      cAdjRef    WITH lcAdjRef        ,;
      nTranCost  WITH lnTranCost
    =gfTraceKey('STYINVJL',STYINVJL.STYLE+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.LINENO,6),'A')
    lnTmpStp = lnTmpStp + 1
    =lfUpdGLDist(.F.,.T.)
    =lfStyWarDy()
  ENDIF
ENDIF
SET ORDER TO (lcJourTag) IN StyInvJl
SELECT (lnCurAlias)
*-- End of Function lfLkAdjRec.
*!***************************************************************************
*!* Name        : lfIsuJlTr
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : This function will update the journal file with Issue transaction
*!*             : record(s) for Physical or markdown transactions only.
*!*             : If the method is Standard or Average we create only one Issue record
*!*             : for old stock before physical transaction was done Else if the method
*!*             : is LIFO or FIFO we create issue records depends on all open receivings
*!*             : exist in journal.
*!***************************************************************************
*! Notes        : This Function Copied from Gfstycrl.prg with some modification
*!              : from Mohamed Shokry to be Suitable for the bin location system
*!***************************************************************************
*!* Called from : BN4MAIN.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfIsuJlTr()
*!***************************************************************************
FUNCTION lfIsuJlTr

IF lcCostMeth $ 'FL'  && ISSUE FIFO or LIFO.
  *--Get the open receivings.
  lcTmpJour = gfTempName()
  =lfIsueCost(.T.)
  SELECT (lcTmpJour)
  SCAN
    IF nTotStk <> 0 OR nStkVal <> 0
      REPLACE cSession  WITH lcRISessn,cISession WITH cSession,cTrCode WITH IIF(cTrType $ "12",cSession,cTrCode)
      SCATTER MEMVAR
      IF lfCheckUnCmp(lnTmpStp)
        SELECT STYINVJL
        APPEND BLANK
        GATHER MEMVAR
        REPLACE REFERENCE  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
          cAdjReason WITH lcAdjCdRsn      ,;
          cAdjAcct   WITH lcAdjAcct       ,;
          nStkVal    WITH IIF(nTotStk = 0,nStkVal,nTotStk * nCost),;
          nPrvSQty   WITH lnPrvQty        ,;
          nPrvSval   WITH lnPrvVal        ,;
          cAdjRef    WITH lcAdjRef        ,;
          lLockFlg   WITH (lcTrType='9')  ,;
          nTranCost  WITH lnTranCost

        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')
        *-- in case of inventory lock
        IF lcTrType='9'
          SCATT MEMVAR MEMO
          m.clocation   = lcBinLoc
          *B608785 TMI 01/20/2009 [Start]
          *INSERT INTO ('BININVJL') FROM MEMVAR
          SELECT BININVJL
          =gfAppend('BININVJL',.T.)
          *B608785 TMI 01/20/2009 [End  ]
        ENDIF

        *!*	        *T20071102.0018 TMI [Start]
        *!*	        IF lcTrType$'12'
        *!*	          SCATT MEMVAR MEMO
        *!*	          m.clocation   = lcBinLoc
        *!*	          SELECT BININVJL
        *!*	          =gfAppend('BININVJL',.T.)
        *!*	        ENDIF
        *!*	        *T20071102.0018 TMI [End  ]

        *--Update Uncomplete session Step.
        *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
        *=lfUpdStep(lnTmpStp)

        *--Call TraceKey global function.
        =gfTraceKey('STYINVJL',STYINVJL.STYLE+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.LINENO,6),'A')
      ENDIF
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
  ENDSCAN
  USE
  *--Erase the temp. journal file.
  =lfEraseFil(lcTmpJour)
ELSE
  IF lfCheckUnCmp(lnTmpStp)
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn      ,;
      STYLE      WITH lcStyle        ,;
      cWareCode  WITH lcWareCode     ,;
      cDyelot    WITH lcSDyelot      ,;
      dTrDate    WITH ldTrDate       ,;
      cTrType    WITH lcTrType       ,;
      cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
      nCost      WITH lnWOldCst      ,;
      cIRType    WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
      cISession  WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
      cRSession  WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
      nStk1      WITH -(laOldstk[1]) ,;
      nStk2      WITH -(laOldstk[2]) ,;
      nStk3      WITH -(laOldstk[3]) ,;
      nStk4      WITH -(laOldstk[4]) ,;
      nStk5      WITH -(laOldstk[5]) ,;
      nStk6      WITH -(laOldstk[6]) ,;
      nStk7      WITH -(laOldstk[7]) ,;
      nStk8      WITH -(laOldstk[8]) ,;
      nTotStk    WITH -(laOldstk[9]) ,;
      nStkVal    WITH -(lnOldSVal)   ,;
      lLockFlg   WITH (lcTrType='9') ,;
      REFERENCE  WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
      cAdjReason WITH lcAdjCdRsn     ,;
      cAdjAcct   WITH lcAdjAcct      ,;
      nPrvSQty   WITH lnPrvQty       ,;
      nPrvSVal   WITH lnPrvVal       ,;
      cAdjRef    WITH lcAdjRef       ,;
      nTranCost  WITH lnTranCost
    *-- Call global function to add audit fields info.
    =gfAdd_Info('STYINVJL')
    *-- in case of inventory lock.
    IF lcTrType='9'
      SCATT MEMVAR MEMO
      m.clocation   = lcBinLoc
      *B608785 TMI 01/20/2009 [Start]
      *INSERT INTO ('BININVJL') FROM MEMVAR
      SELECT BININVJL
      =gfAppend('BININVJL',.T.)
      *B608785 TMI 01/20/2009 [End  ]

    ENDIF

    *!*	    *T20071102.0018 TMI [Start]
    *!*	    IF lcTrType$'12'
    *!*	      SCATT MEMVAR MEMO
    *!*	      m.clocation   = lcBinLoc
    *!*	      SELECT BININVJL
    *!*	      =gfAppend('BININVJL',.T.)
    *!*	    ENDIF
    *!*	    *T20071102.0018 TMI [End  ]

    *--Update Uncomplete session Step.
    *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
    *=lfUpdStep(lnTmpStp)
    *--Call TraceKey global function.
    =gfTraceKey('STYINVJL',STYINVJL.STYLE+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.LINENO,6),'A')
    lnTmpStp = lnTmpStp + 1
    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfStyWarDy()
  ENDIF
ENDIF
RETURN
*-- End of Function lfIsuJlTr.
*!***************************************************************************
*!* Name        : lfIsueCost
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : This function will get the journal records for the receiving
*!*             : transactions that will be applied on the current Issue
*!*             : transaction and take the cost of the receiving to be a new
*!*             : cost of issueing,depends on cost method FIFO or LIFO.
*!***************************************************************************
*! Notes        : This Function Copied from Gfstycrl.prg with some modification
*!              : from Mohamed Shokry to be Suitable for the bin location system
*!***************************************************************************
*!* Called from : BN4MAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfIsueCost()
*!***************************************************************************
FUNCTION lfIsueCost
PARAMETERS llForPhys
PRIVATE lcAlias,llContnu,laTotRcvd
lcAlias = ALIAS()
SELECT BININVJL

*B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][Start]
llCreateDbf = .T.
*B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][End]

*--Create the Temp journal file with open receiving transactions.
*B609983,1 MMT 07/02/2012 Incorrect receiving session used while adjustment in case of bin location is used[T20120621.0006][Start]
*!*	lcSqlStatment = ;
*!*	[SELECT Style,cWareCode,cLocation,cDyelot,cSession,cRSession,cISession,dTrDate,cTrType,cTrCode,cIRType,nCost,]+;
*!*	[       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',SUM(nStk4) AS 'nStk4',]+;
*!*	[       SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',]+;
*!*	[       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', 'N' AS 'lNeeded', SPACE(6) as cAdjRef ]+;
*!*	[   FROM  BININVJL               ]+;
*!*	[   WHERE Style + cWareCode + cDyelot LIKE ']+lcStyle + lcWareCode + lcSDyelot+[']+;
*!*	[       AND CHARINDEX(cLocation,']+lcBinLoc+[')>0]+;
*!*	[   GROUP BY Style,cWareCode,cLocation,cDyelot,cSession,cRSession,cISession,dTrDate,cTrType,cTrCode,cIRType,nCost]+;
*!*	[   ORDER BY Style,cWareCode,cDyelot,CLOCATION,cRSession ]
lcSqlStatment = ;
  [SELECT Style,cWareCode,cLocation,cDyelot,cSession,cRSession,cISession,dTrDate,cTrType,cTrCode,cIRType,nCost,]+;
  [       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',SUM(nStk4) AS 'nStk4',]+;
  [       SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',]+;
  [       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', 'N' AS 'lNeeded', SPACE(6) as cAdjRef ]+;
  [   FROM  BININVJL               ]+;
  [   WHERE Style + cWareCode + cDyelot LIKE ']+lcStyle + lcWareCode + lcSDyelot+[']+;
  [       AND CHARINDEX(cLocation,']+lcBinLoc+[')>0]+;
  [   GROUP BY Style,cWareCode,cLocation,cDyelot,cRSession,cSession,cISession,dTrDate,cTrType,cTrCode,cIRType,nCost]+;
  [   ORDER BY Style,cWareCode,cDyelot,CLOCATION,cRSession ]
*B609983,1 MMT 07/02/2012 Incorrect receiving session used while adjustment in case of bin location is used[T20120621.0006][END]
lcCursor = lcTmpJour
lcTable = 'BININVJL'

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,;
  oAriaApplication.ActiveCompanyConStr,3,'SAVE',SET("DataSession"))
IF lnConnectionHandlar<>-1
  SELECT (lcTmpJour)
  DELETE ALL FOR nStk1=0 AND nStk2=0 AND nStk3=0 AND nStk4=0 AND nStk5=0 AND nStk6=0 AND nStk7=0 AND nStk8=0

  GO TOP
  IF EOF()
    IF !llForPhys
      *--No open receiving exist for style XXXX , This transaction line will be ignored.
      =gfModalGen('TRM42116B42000','DIALOG',lcStyle)
      USE
      RETURN .F.
    ELSE  && Get issue for Physical transactions.
      APPEND BLANK
      REPLACE cSession  WITH lcRISessn,;
        STYLE     WITH lcStyle,;
        cWareCode WITH lcWareCode,;
        cDyelot   WITH lcSDyelot,;
        nCost     WITH lnWOldCst,;
        dTrDate   WITH ldTrDate,;
        cTrType   WITH lcTrType,;
        cTrCode   WITH lcTrCode,;
        cIRType   WITH "I",;
        nStkVal   WITH lnOldSVal

      REPLACE cAdjRef    WITH lcAdjRef

      RETURN
    ENDIF
  ENDIF

ELSE
  *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][Start]
  llCreateDbf = .F.
  *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][End]

  SELECT BININVJL
  COPY STRUCTURE TO (oAriaApplication.WorkDir+lcTmpJour)
  SELECT 0
  *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][Start]
  *USE (oAriaApplication.WorkDir+lcTmpJour)
  USE (oAriaApplication.WorkDir+lcTmpJour) EXCLUSIVE
  *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][END]
ENDIF

*--For Not Phyical.
IF !llForPhys
  SELECT (lcTmpJour)
  *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][Start]
  llContnu  = .F.
  IF llCreateDbf
    *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][End]
    COPY TO (oAriaApplication.WorkDir+lcTmpJour)
    USE IN (lcTmpJour)
    USE (oAriaApplication.WorkDir+lcTmpJour) EXCL
    *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][Start]
  ENDIF
  *B609983,2 MMT 07/16/2012 Error 'File is in used'while doing -ve adjustment if bin location is used[T20120621.0006][End]
  *--Indexing the file on Ascending or Descending expresion depends on LIFO or FIFO method.
  IF lcCostMeth = 'F'
    INDEX ON STYLE+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
  ELSE
    INDEX ON STYLE+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
  ENDIF
  GO TOP

  *--Start checking the only needed open receinving transaction for this
  *--issue transaction and put zero for all not needed receivings.

  *--Array to Hold the accomulation of the receiving untill it cover the issue quantity needed.
  DIME laTotRcvd[9]
  laTotRcvd = 0
  SCAN
    llContnu  = .F.
    FOR I=1 TO 8
      Z=STR(I,1)
      IF ABS(laAdjStk[I]) > laTotRcvd[I]
        *- receive from the correct bin
        llContnu = .T.  && to gurantee that the llContnu is updated
      ENDIF
      *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[START]
      *IF ABS(laAdjStk[I]) > laTotRcvd[I] .AND. cLocation = &lcTmpLFile..Binloc&Z
      IF ABS(laAdjStk[I]) > laTotRcvd[I] .AND. cLocation = lcBinLoc
        *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]

        llContnu = .T.
        laTotRcvd[I] = laTotRcvd[I] + nStk&Z
        laTotRcvd[9] = nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        IF ABS(laAdjStk[I]) <= laTotRcvd[I]
          REPLACE nStk&Z  WITH nStk&Z - (laTotRcvd[I] - ABS(laAdjStk[I]))
          REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
          REPLACE lNeeded WITH 'Y'   &&  use string value instead the logical value
        ELSE
          REPLACE lNeeded WITH 'Y'
        ENDIF
      ELSE
        REPLACE nStk&Z  WITH 0
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE lNeeded WITH 'Y'   &&  use string value instead the logical value
      ENDIF
    ENDFOR
    IF !llContnu
      EXIT
    ENDIF
  ENDSCAN
  *--Check if all Issue quantity are covered by the receivings.
  IF llContnu
    FOR I=1 TO 8
      Z=STR(I,1)
      IF laAdjStk[I] < 0 AND ABS(laAdjStk[I]) > laTotRcvd[I]
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        =gfModalGen('TRM42115B42000','DIALOG',lcStyle)
        USE
        RETURN .F.
      ENDIF
    ENDFOR
  ENDIF
  *--Delete all not needed receiving transactions.
  DELETE ALL FOR nTotStk = 0 OR lNeeded = 'N'
ENDIF

*--Change it to Issue transactions,to use it in updating master Journal file.
REPLACE ALL cIRType WITH "I"      ,;
  dTrDate WITH ldTrDate ,;
  cTrType WITH lcTrType ,;
  cTrCode WITH lcTrCode ,;
  nStk1   WITH -nStk1   ,;
  nStk2   WITH -nStk2   ,;
  nStk3   WITH -nStk3   ,;
  nStk4   WITH -nStk4   ,;
  nStk5   WITH -nStk5   ,;
  nStk6   WITH -nStk6   ,;
  nStk7   WITH -nStk7   ,;
  nStk8   WITH -nStk8   ,;
  nTotStk WITH -nTotStk ,;
  nStkVal WITH -nStkVal

SELECT (lcAlias)
RETURN .T.
*-- End of Function lfIsueCost.

*!***************************************************************************
*!* Name        : lfAdjRec
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add Receiving record and Issuing record in StyInvJl.
*!***************************************************************************
*!* Called from : BN4MAIN.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAdjRec()
*!***************************************************************************
FUNCTION lfAdjRec

IF lfCheckUnCmp(lnTmpStp)
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn     ,;
    STYLE      WITH lcStyle       ,;
    cWareCode  WITH lcWareCode    ,;
    cDyelot    WITH lcSDyelot     ,;
    dTrDate    WITH ldTrDate      ,;
    cTrType    WITH '1'           ,;
    cTrCode    WITH lcRISessn     ,;
    nCost      WITH lnWOldCst     ,;
    cIRType    WITH "R"           ,;
    nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[1],laAdjStk[1]),;
    nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[2],laAdjStk[2]),;
    nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[3],laAdjStk[3]),;
    nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[4],laAdjStk[4]),;
    nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[5],laAdjStk[5]),;
    nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[6],laAdjStk[6]),;
    nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[7],laAdjStk[7]),;
    nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[8],laAdjStk[8]),;
    nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
    nStkVal    WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-lnWStkVal,nTotStk * lnWOldCst),;
    REFERENCE  WITH "Auto cost adj. " + cTrCode ,;
    cAdjReason WITH lcAdjCdRsn    ,;
    cAdjAcct   WITH lcAdjAcct     ,;
    cRSession  WITH cSession      ,;
    nPrvSQty   WITH lnPrvQty      ,;
    nPrvSVal   WITH lnPrvVal      ,;
    cAdjRef    WITH lcAdjRef      ,;
    nTranCost  WITH lnTranCost

  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
  *=lfUpdStep(lnTmpStp)

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.STYLE+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.LINENO,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*-- Iss. with the new cost
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn      ,;
    STYLE      WITH lcStyle        ,;
    cWareCode  WITH lcWareCode     ,;
    cDyelot    WITH lcSDyelot      ,;
    dTrDate    WITH ldTrDate       ,;
    cTrType    WITH '1'            ,;
    cTrCode    WITH lcRISessn      ,;
    nCost      WITH lnNewCost      ,;
    cIRType    WITH "I",;
    nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[1],-laAdjStk[1]),;
    nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[2],-laAdjStk[2]),;
    nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[3],-laAdjStk[3]),;
    nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[4],-laAdjStk[4]),;
    nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[5],-laAdjStk[5]),;
    nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[6],-laAdjStk[6]),;
    nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[7],-laAdjStk[7]),;
    nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[8],-laAdjStk[8]),;
    nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
    nStkVal    WITH nTotStk * lnNewCost,;
    REFERENCE  WITH "Auto cost adj. " + cTrCode ,;
    cAdjReason WITH lcAdjCdRsn     ,;
    cAdjAcct   WITH lcAdjAcct      ,;
    cISession  WITH cSession       ,;
    nPrvSQty   WITH lnPrvQty       ,;
    nPrvSVal   WITH lnPrvVal       ,;
    cAdjRef    WITH lcAdjRef       ,;
    nTranCost  WITH lnTranCost
  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  *T20071102.0018,9 TMI 04/29/2008 [Start] this function is stopped used in aria4xp
  *=lfUpdStep(lnTmpStp)

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.STYLE+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.LINENO,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1
*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()
*-- End of Function lfAdjRec.

*!***************************************************************************
*!* Name        : lfUpdStep
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update uncomplete session step.
*!***************************************************************************
*!* Called from : BN4MAIN.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdStep()
*!***************************************************************************
FUNCTION lfUpdStep
PARA lnCurntStep
PRIVATE lnAlas
IF llChekUncmp
  lnAlas = SELECT()
  SELECT (lcTmpLFile)
  =RLOCK()
  REPLACE &lcStepFld WITH lnCurntStep
  UNLOCK
  SELECT(lnAlas)
ENDIF
RETURN
*-- End of Function lfUpdStep.

*!***************************************************************************
*!* Name        : lfCheckUnCmp
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Function To Check uncomplete session Steps.
*!***************************************************************************
*!* Called from : BN4MAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : .T. for Check , .F. for Already checked no need to check.
*!***************************************************************************
*!* Example     : = lfCheckUnCmp()
*!***************************************************************************
FUNCTION lfCheckUnCmp
PARAMETERS lnStepNo

*T20071102.0018,9 TMI 04/29/2008 [Start] No use of this function in Aria4xp
RETURN .T.
*T20071102.0018,9 TMI 04/29/2008 [End  ]

IF !llChekUncmp
  RETURN .T.
ELSE
  RETURN ( &lcTmpLFile..&lcStepFld < lnStepNo )
ENDIF
*-- End of Function lfCheckUnCmp.

*!***************************************************************************
*!* Name        : lfTRMDSTY
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate Style in case of IC Transfeer (modify ).
*!***************************************************************************
*!* Called from : ICSTYTR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfTRMDSTY()
*!***************************************************************************
FUNCTION lfTRMDSTY
PRIVATE lcFileNama,lnCurAlias

IF !lfIsUseBin()
  RETURN
ENDIF

lcFileNama = lcTmpAdj+"A"
lnCurAlias = SELECT()

IF SEEK(lcFromWare+&lcFileNamA..LocFrom+M.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.tOTstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount    = 0
    laTStk[lnCount]  = 0
    laOTStk[lnCount] = 0
  ENDFOR
  m.tOTstk   = 0
  laOTStk[9] = 0
  laTStk[9]  = 0
ENDIF
IF SEEK(lcToWare+&lcFileNamA..LocTo+M.Style,'WHBINLOC')
  laTStk[1]  =WHBINLOC.Qty1 - WHBINLOC.Alo1 +m.adj1
  laTStk[2]  =WHBINLOC.Qty2 - WHBINLOC.Alo2 +m.adj2
  laTStk[3]  =WHBINLOC.Qty3 - WHBINLOC.Alo3 +m.adj3
  laTStk[4]  =WHBINLOC.Qty4 - WHBINLOC.Alo4 +m.adj4
  laTStk[5]  =WHBINLOC.Qty5 - WHBINLOC.Alo5 +m.adj5
  laTStk[6]  =WHBINLOC.Qty6 - WHBINLOC.Alo6 +m.adj6
  laTStk[7]  =WHBINLOC.Qty7 - WHBINLOC.Alo7 +m.adj7
  laTStk[8]  =WHBINLOC.Qty8 - WHBINLOC.Alo8 +m.adj8
  laTStk[9]  =WHBINLOC.TotQty - WHBINLOC.TotAlo +m.Totadj
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = laTStk[lnCount]
  ENDFOR
  laOTStk[9] =laTStk[9]
ELSE
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
  laTStk[9] = 0
ENDIF
SELECT (lnCurAlias)
*-- End of Function lfTRMDSTY.

*!***************************************************************************
*!* Name        : lfDLMDYSTY
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate in case of Modify IC Adj. And Physical.
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg
*!***************************************************************************
*!* Calls       : None
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLMDYSTY()
*!***************************************************************************
FUNCTION lfDLMDYSTY

PRIVATE lcFileNama ,lnCurAlias
IF !lfIsUseBin()
  RETURN
ENDIF

lcFileNama = lcTmpAdj+"A"
lnCurAlias = SELECT()

IF SEEK(lcFromWare+&lcFileNamA..LocFrom+m.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.Totstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  IF lcType = 'P'
    m.Adj1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
    m.Adj2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
    m.Adj3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
    m.Adj4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
    m.Adj5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
    m.Adj6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
    m.Adj7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
    m.Adj8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
    m.TotAdj =WHBINLOC.TotQty - WHBINLOC.TotAlo
  ENDIF
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount]  = m.Stk&lcCount
    laOTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9]  = m.Totstk
  laOTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8
    lcCount = ALLTRIM(STR(lnCount))
    IF lcType = 'P'
      m.Adj&lcCount = 0
      m.TotAdj = 0
    ENDIF
    m.Stk&lcCount    = 0
    laTStk[lnCount]  = 0
    laOTStk[lnCount] = 0
  ENDFOR
  laTStk[9]  = 0
  laOTStk[9] = 0
  m.tOTstk   = 0
ENDIF
SELECT(lnCurAlias)
*-- End of Function lfDLMDYSTY.
*!***************************************************************************
*!* Name        : lfDLASSBIN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Assign bin location to temp file in Inventory locking
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLASSBIN()
*!***************************************************************************
FUNCTION lfDLASSBIN

LOCAL lcTmpQuery
lcTmpQuery = loFormset.lcTmpQuery

*T20071102.0018,10/C200876 TMI 07/07/2008 [Start]
*IF SEEK(m.STYLE+m.Color,lcTmpQuery) AND m.clocation = &lcTmpQuery..Bin
IF SEEK(m.clocation+m.STYLE+m.Color,lcTmpQuery)
  *T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]
  m.BIN     = m.clocation
  SELECT (lcTmpQuery)
  REPLACE Stock  WITH Stock + m.Stock ;
    oStock WITH oStock + m.oStock
ELSE
  m.BIN     = m.clocation
  INSERT INTO (lcTmpQuery) FROM MEMVAR
ENDIF
*--create Temp to store Bin Location Variable in
PRIVATE lcFileNamA
lcFileNamA = lcTmpQuery+"A"
CREATE CURSOR (lcFileNamA) (cLocation C(10))
APPEND BLANK

*-- End of Function lfDLASSBIN.

*!***************************************************************************
*!* Name        : lfDLBRWBIN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add a Bin Location Field to Inventory Locking's Browse Screen
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLBRWBIN()
*!***************************************************************************
FUNCTION lfDLBRWBIN

IF !lfIsUseBin()
  RETURN
ENDIF
** Note : the last column must be visible = .F., so assign your column to the last column exists and the new added one let it be invisible
LOCAL lcI
WITH loFormSet.ariaFORM1.grditems
  lcI = ALLTRIM(STR(.COLUMNCOUNT ))
  .COLUMN&lcI..VISIBLE = .T.
  .COLUMN&lcI..CONTROLSOURCE = loFormSet.lcTmpQuery+'.BIN'
  .COLUMN&lcI..header1.CAPTION = 'Bin'
  .COLUMN&lcI..WIDTH = 80
  .COLUMN&lcI..COLUMNORDER = 3

  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lcI = ALLTRIM(STR(loFormSet.ariaFORM1.grditems.COLUMNCOUNT))
  .COLUMN&lcI..VISIBLE = .F.
ENDWITH

*!*	*- Add bin fields to the key
*!*	SELECT (loFormSet.lcTmpQuery)
*!*	INDEX ON Style+Color+Bin TAG ItmClrBn

*- Add a property to the formset llUseBin, use it instead of recalling the function lfisUseBin every time when entering the
*- lfDLSELREC
loFormSet.ADDPROPERTY('llIsUseBin',lfIsUseBin())

*- Open the WHBINLOC file to not check its open every time entering the lfDLSELREC trigger
IF !USED('WHBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHBINLST','SH')
ENDIF

*-- End of Function lfDLBRWBIN.

*!***************************************************************************
*!* Name        : lfDLDELALL
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Delete all Records from the Temp File in the inventory
*!*             : Locking Screen when Saving using Bin Location.
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLDELALL()
*!***************************************************************************
FUNCTION lfDLDELALL
LOCAL lcDetLin,lcBatLin
lcDetLin = loFormset.lcDetLin
lcBatLin = loFormSet.lcBatLin

DELETE ALL FOR cbattype+cLkBatch+STYLE+COLOR+DYELOT =;
  lcKey+lcBatchno +EVALUATE(loMainForm.lcDetLin+'.Style')+EVALUATE(loMainForm.lcDetLin+'.Color');
  AND &lcBatLin..clocation = &lcDetLin..clocation AND IIF(loMainForm.llDyelot AND lfStyDye(EVALUATE(loMainForm.lcDetLin+'.Style'),EVALUATE(loMainForm.lcDetLin+'.Color')),!EMPTY(EVALUATE(loMainForm.lcDetLin+'.DYELOT')),.T.)

*-- End of Function lfDLDELALL.

*!***************************************************************************
*!* Name        : lfDLPSTLCK
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid function for posting batch
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLPSTLCK()
*!***************************************************************************
FUNCTION lfDLPSTLCK

lcBinLoc = MDINVNTL.clocation
PRIVATE lcFileNama
*T20071102.0018,10/C200876 TMI 05/20/2008 [Start]
lcTmpQuery = loFormSet.lcTmpQuery
lcStyPic  = gfItemMask("HI")
*T20071102.0018,10/C200876 TMI 05/20/2008 [End  ]
lcFileNamA = lcTmpQuery+"A"
REPLACE &lcFileNamA..cLocation WITH MDINVNTL.clocation
*B608785 TMI 01/22/2009 [Start] get old stock
lnTempCst= IIF(MDINVNTL.OldTotStk<>0, MDINVNTL.nStkVal/MDINVNTL.OldTotStk,0)
*!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(Start)
lnTempCst = MDINVNTL.OldCost
*!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(End)
*B608785 TMI 01/22/2009 [End  ]

*B610671,1 TMI 02/05/2014 11:51 [Start] fill the array for all bins included, i.e. remove the lcBinLoc criteria
*SCAN REST WHILE cbattype+cLkBatch+STYLE+COLOR+DYELOT = lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
    AND clocation  = lcBinLoc FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0                  ;
    OR  OLDSTK3<>0 OR OLDSTK4<>0 OR OLDSTK5<>0 OR OLDSTK6<>0 OR OLDSTK7<>0 OR OLDSTK8<>0)       ;
    OR  TotStk <> 0 )
SCAN REST WHILE cbattype+cLkBatch+STYLE+COLOR+DYELOT = lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1 ;
    FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0                  ;
          OR  OLDSTK3<>0 OR OLDSTK4<>0 OR OLDSTK5<>0 OR OLDSTK6<>0 OR OLDSTK7<>0 OR OLDSTK8<>0)   ;
          OR  TotStk <> 0 )
    *B610671,1 TMI 02/05/2014 11:51 [End  ] 
  WAIT WINDOW NOWAIT "Posting Batch# " + MDINVNTH.cLkBatch + ;
    IIF(loFormSet.llMatModul,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,' '+lcStyPic +[\ ]+STYLE)
  *B608785 TMI 01/22/2009 [Start] show progress bar
  loFormSet.oProgress.CurrentProgress(lnCountLn)
  *B608785 TMI 01/22/2009 [End  ]

  lnCountLn = lnCountLn + 1
  *B608785 TMI 01/19/2009 [Start] replace the gfThermo with a wait winodw
  *=gfThermo(loFormSet.lnTotRec,lnCountLn,'Posting')
  WAIT WINDOW NOWAIT cbattype+cLkBatch+STYLE+COLOR+DYELOT
  *B608785 TMI 01/19/2009 [End  ]
  *----Start Calculate Stk for posting
  *B608785 TMI 01/22/2009 [Start] use the field cReason instead
  *lcAdjReason = MDINVNTL.cAdjReason
  lcAdjReason = MDINVNTL.cReason
  *B608785 TMI 01/22/2009 [End  ]
  IF !loFormSet.llMatModul
    FOR lnIndex = 1 TO 8
      lcSub = STR(lnIndex,1)
      laAdjust[lnIndex] = laAdjust[lnIndex] + MDINVNTL.Stk&lcSub
      laOldStk[lnIndex] = laOldStk[lnIndex] + MDINVNTL.OldStk&lcSub
    ENDFOR
  ENDIF
  laAdjust[9]  = laAdjust[9] + MDINVNTL.TOTSTK
  laAdjust[10] = MDINVNTL.COST
  laOldStk[9]  = laOldStk[9] + MDINVNTL.OldTOTSTK
ENDSCAN

*B608785 TMI 01/19/2009 [Start] remove this code
*!*	FOR lnRest = lnCountLn TO loFormSet.lnTotRec
*!*	  loFormSet.oProgress.CurrentProgress(lnRest)
*!*	ENDFOR
*B608785 TMI 01/19/2009 [End  ]

*-- End of Function lfDLPSTLCK.

*!***************************************************************************
*!* Name        : lfDLSELREC
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Select Bin Location for inventory locking
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSELREC()
*!***************************************************************************
FUNCTION lfDLSELREC
PRIVATE lcOldOrder,lnOldAlias
LOCAL lcWareCode

lcWareCode = loFormSet.Ariaform1.kbLocation.Keytextbox.VALUE
lnOldAlias = SELECT(0)

*!*	IF !lfIsUseBin()
*!*	  RETURN .F.
*!*	ENDIF

*!*	IF !USED('WHBINLOC')
*!*	  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHBINLST','SH')
*!*	ENDIF

lcTmpQuery = loFormSet.lcTmpQuery
*T20071102.0018,10/C200876 TMI 05/28/2008 [Start]
*lcOldOrder = ORDER(lcTmpQuery)
*SELECT (lcTmpQuery)
*INDEX ON Style+Color+Bin TAG ItmClrBn
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]

*T20071102.0018,10/C200876 TMI 07/07/2008 [Start]
*T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]

*T20071102.0018,10/C200876 TMI 05/28/2008 [Start]
*T20071102.0018,10/C200876 TMI 07/07/2008 [Start] use the progress bar
*WAIT WINDOW NOWAIT 'Checking style:'+STYDYE.style+STYDYE.cwarecode
WAIT CLEAR
WITH loFormSet.oProgress
  .lblFirstLabel.CAPTION = 'Checking style:'+STYDYE.STYLE+STYDYE.cwarecode
  loFormSet.lnCurrCounter = loFormSet.lnCurrCounter + 1
  .CurrentProgress(loFormSet.lnCurrCounter)
ENDWITH
*T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]

*B608785 TMI 01/19/2009 [Start] no need for checking the WHSLOC file
*!*	IF SEEK(STYDYE.style+m.Color+STYDYE.cwarecode,'WHSLOC','WHSLOCST')
*!*	  SELECT WHSLOC
*!*	  lcSvOrd = ORDER('WHSLOC')
*!*	  SET ORDER TO WHSLOCST  && STYLE+COLOR+CWARECODE+CLOCATION
*!*
*!*	  SCAN REST WHILE STYLE+COLOR+CWARECODE+CLOCATION = STYDYE.style+'      '+STYDYE.cwarecode ;
*!*	            FOR !EMPTY(CLOCATION)
*!*	    STORE .T. TO llSec,llBin
*!*	    IF llLoc
*!*	      llBin = IIF(!EMPTY(laRpTarget)          ,ASCAN(laRpTarget          ,WHSLOC.cLocation) <> 0,.T.)
*!*	      llSec = IIF(!EMPTY(loFormSet.laScTarget),ASCAN(loFormSet.laScTarget,WHSLOC.cSection ) <> 0,.T.)
*!*	    ENDIF
*!*	    *T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]

*!*	    *T20071102.0018,10/C200876 TMI 05/28/2008 [Start]
*!*	    *IF gfSEEK(STYDYE.style+STYDYE.cwarecode,'WHBINLOC')
*!*	    IF llSec AND llBin AND ;
*!*	      gfSEEK(STYDYE.style+STYDYE.cwarecode+WHSLOC.CLOCATION,'WHBINLOC')
*!*	      *T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]

IF gfSEEK(STYDYE.STYLE+STYDYE.cwarecode,'WHBINLOC')
  *B608785 TMI 01/19/2009 [End  ]

  SELECT WHBINLOC
  SCAN REST WHILE STYLE+cwarecode+clocation = STYDYE.STYLE+STYDYE.cwarecode

    *B608785 TMI 01/19/2009 [Start] check the style is included in the selected bin and section
    STORE .T. TO llSec,llBin
    IF llLoc
      llBin = IIF(!EMPTY(laRpTarget)          ,ASCAN(laRpTarget          ,WHBINLOC.cLocation) <> 0,.T.)
      llSec = IIF(!EMPTY(loFormSet.laScTarget),ASCAN(loFormSet.laScTarget,WHBINLOC.cSection ) <> 0,.T.)
    ENDIF
    IF !(llSec AND llBin)
      LOOP
    ENDIF
    *B608785 TMI 01/19/2009 [End  ]
    m.stk1 = WHBINLOC.Qty1
    m.stk2 = WHBINLOC.Qty2
    m.stk3 = WHBINLOC.Qty3
    m.stk4 = WHBINLOC.Qty4
    m.stk5 = WHBINLOC.Qty5
    m.stk6 = WHBINLOC.Qty6
    m.stk7 = WHBINLOC.Qty7
    m.stk8 = WHBINLOC.Qty8

    m.Totstk  = WHBINLOC.TotQty
    m.STOCK   = WHBINLOC.TotQty
    m.oSTOCK  = WHBINLOC.TotQty
    *B608785 TMI 01/22/2009 [Start] put the refrence of the STYDYE to ave_cost field
    *m.nStkVal = (Ave_Cost*WHBINLOC.TotQty)
    m.nStkVal = (STYDYE.Ave_Cost*WHBINLOC.TotQty)
    *B608785 TMI 01/22/2009 [End  ]
    m.BIN     = WHBINLOC.clocation

    *T20071102.0018,10/C200876 TMI 05/28/2008 [Start]
    *!*	    llBin = .T.
    *!*	    IF llLoc AND !EMPTY(laRpTarget)
    *!*	      IF !EMPTY(WHBINLOC.clocation) AND ASCAN(laRpTarget,ALLTRIM(WHBINLOC.clocation)) <> 0
    *!*	        llBin = .T.
    *!*	      ELSE
    *!*	        llBin = .F.
    *!*	      ENDIF
    *!*	    ENDIF

    *!*	    llSec = .T.
    *!*	    IF llLoc AND !EMPTY(loFormSet.laScTarget)
    *!*	      IF !EMPTY(WHBINLOC.cSection) AND ASCAN(loFormSet.laScTarget,ALLTRIM(WHBINLOC.cSection)) <> 0
    *!*	        llSec = .T.
    *!*	      ELSE
    *!*	        llSec = .F.
    *!*	      ENDIF
    *!*	    ENDIF
    *T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]

    *T20071102.0018,10/C200876 TMI 05/29/2008 [Start]
    *IF llSec AND llBin
    *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]

    *- Check if the style/bin exists in a non-posted batch
    SELECT MDINVNTL
    SET ORDER TO MDINVNTLS   && STYLE+COLOR+DYELOT+CLOCATION+CBATTYPE+CLKBATCH
    WAIT WINDOW 'Adding Styel:'+WHBINLOC.CWARECODE+'-'+WHBINLOC.CLOCATION+'-'+WHBINLOC.STYLE NOWAIT
    STORE .T. TO llCheck

    *B608785 TMI 01/19/2009 [Start]
    *IF SEEK(m.Style+m.Color+STYDYE.DYELOT+WHSLOC.CLOCATION,'MDINVNTL')
    IF SEEK(m.Style+m.Color+STYDYE.DYELOT+WHBINLOC.CLOCATION,'MDINVNTL')
      *B608785 TMI 01/19/2009 [End  ]

      *T20071102.0018,10/C200876 TMI 05/29/2008 [Start]
      *SCAN REST WHILE style+color+cbattype+cLkBatch = m.Style+m.Color FOR IIF(llWareHus,cWareCode = lcWareCode,.T.)

      *B608785 TMI 01/19/2009 [Start]
      *SCAN REST WHILE STYLE+COLOR+DYELOT+CLOCATION+CBATTYPE+CLKBATCH = m.Style+m.Color+STYDYE.DYELOT+WHSLOC.CLOCATION;
      FOR IIF(llWareHus,cWareCode = lcWareCode,.T.)
      SCAN REST WHILE STYLE+COLOR+DYELOT+CLOCATION+CBATTYPE+CLKBATCH = ;
          m.STYLE+m.Color+STYDYE.DYELOT+WHBINLOC.CLOCATION ;
          FOR IIF(llWareHus,cWareCode = lcWareCode,.T.)
        *B608785 TMI 01/19/2009 [End  ]

        *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]
        IF SEEK('S'+MDINVNTL.cLkBatch,'MDINVNTH') AND MDINVNTH.TYPE $ 'HML'
          llCheck = .F.
          *T20071102.0018,10/C200876 TMI 05/28/2008 [Start]
          EXIT
          *T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]
        ENDIF
      ENDSCAN
    ENDIF

    *- Check if the style/bin is added to the lcTmpQuery
    *T20071102.0018,10/C200876 TMI 07/07/2008 [Start] seek key is : bin+style+color
    *llCheck = llCheck .AND. !SEEK(m.Style + m.Color + m.BIN , lcTmpQuery )
    llCheck = llCheck .AND. !SEEK(m.BIN + m.Style + m.Color , lcTmpQuery )
    *T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]

    IF llCheck
      INSERT INTO (lcTmpQuery) FROM MEMVAR
      SELECT(lcTmpQuery)
      REPLACE nStkVal WITH m.nStkVal
    ENDIF
  ENDSCAN
ENDIF

*B608785 TMI 01/19/2009 [Start] no need for checking the WHSLOC file
*!*	  ENDSCAN
*!*	  *T20071102.0018,10/C200876 TMI 05/28/2008 [Start]
*!*	  SELECT WHSLOC
*!*	  SET ORDER TO &lcSvOrd
*!*	ENDIF
*!*	*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]
*B608785 TMI 01/19/2009 [End  ]


*T20071102.0018,10/C200876 TMI 05/28/2008 [Start]
*SELECT &lcTmpQuery
*SET ORDER TO &lcOldOrder
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]
SELECT (lnOldAlias)
*-- End of Function lfDLSELREC.

*!***************************************************************************
*!* Name        : lfDLSELTMP
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : select Records according to the bin location in case of
*!*             : pushing Edit Button in the Inventory Locking Screen.
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSELTMP()
*!***************************************************************************
FUNCTION lfDLSELTMP
*T20071102.0018,10/C200876 TMI 05/20/2008 [Start]
lcTmpQuery = loFormSet.lcTmpQuery
lcBatLin = loFormSet.lcBatLin
llDyelot = loFormSet.llDyelot
lcDetLin = loFormSet.lcDetLin
*T20071102.0018,10/C200876 TMI 05/20/2008 [End  ]
lcLocBin = &lcTmpQuery..Bin
SELECT *,.F. AS llNew FROM (lcBatLin) WHERE STYLE = lcSty AND cLocation = lcLocBin ;
  AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (oAriaApplication.WorkDir + lcDetLin)
INDEX ON STYLE+COLOR+dyelot+clocation TAG (lcDetLin)
FOR I = 1 TO 8
  Z = STR (I,1)
  lnStk&Z = Stk&Z
ENDFOR
lnMCost   = COST
lnOldCost = OldCOST
lcReason  = cReason
lcStyDesc = IIF(SEEK(lcSty,'Style'),STYLE.Desc1,'')
lnNewTot  = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
*-- End of Function lfDLSELTMP.

*!***************************************************************************
*!* Name        : lfDLTMPBAT
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Get temp batch per bin location
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLTMPBAT()
*!***************************************************************************
FUNCTION lfDLTMPBAT

IF loFormset.llMatModul .OR. !lfIsUseBin()
  RETURN .F.
ENDIF

m.Bin = EVALUATE(loFormset.lcTmpQuery+'.BIN')
*T20071102.0018,10/C200876 TMI 07/07/2008 [Start] initialize variables
STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk,;
  m.OldStk1,m.OldStk2,m.OldStk3,m.OldStk4,m.OldStk5,m.OldStk6,m.OldStk7,m.OldStk8,m.OldTotStk
m.Dyelot    = STYDYE.Dyelot
m.Scale     = STYLE.SCALE
m.clocation = m.Bin

IF loformset.activemode = 'V'
  =gfSEEK('S'+loformset.ariaform1.kbbatchNo.keytextbox.VALUE+STYDYE.STYLE+'                '+M.bin,'MDINVNTL')  && CBATTYPE+CLKBATCH+STYLE+COLOR+DYELOT+CLOCATION
ENDIF
*T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]
IF gfSEEK(STYDYE.STYLE+STYDYE.cwarecode+m.Bin,'WHBINLOC')
  SELECT WHBINLOC
  SCAN REST WHILE STYLE+cwarecode+clocation = STYDYE.STYLE+STYDYE.cwarecode+m.Bin
    FOR lnInd = 1 TO 8
      INDEX = STR(lnInd,1)
      m.OldStk&INDEX = IIF(loformset.activemode = 'V',MDINVNTL.OldStk&INDEX, WHBINLOC.Qty&INDEX )
      m.Stk&INDEX    = IIF(loformset.activemode = 'V',MDINVNTL.Stk&INDEX   , WHBINLOC.Qty&INDEX )
    ENDFOR
    m.OldTotStk = IIF(loformset.activemode = 'V',MDINVNTL.OldTotStk,WHBINLOC.TotQty)
    m.TotStk    = IIF(loformset.activemode = 'V',MDINVNTL.TotStk   ,WHBINLOC.TotQty)
    *T20071102.0018,10/C200876 TMI 07/07/2008 [Start]
    IF loformset.activemode = 'V'
      m.Cost = MDINVNTL.COST
    ENDIF
    *T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]
    *T20071102.0018,10/C200876 TMI 07/07/2008 [Start] move these lines up
    *m.Dyelot    = STYDYE.Dyelot
    *m.Scale     = Style.Scale
    *m.clocation = m.Bin
    *T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]
    INSERT INTO (loFormset.lcBatLin) FROM MEMVAR
  ENDSCAN
  *T20071102.0018,10/C200876 TMI 07/07/2008 [Start] always add a line to the lcBatLin temp file
ELSE
  *T20071102.0018,10/C200876 TMI 07/08/2008 [Start]
  IF loformset.activemode = 'V'
    SELECT MDINVNTL
    SCATTER MEMVAR
  ENDIF
  *T20071102.0018,10/C200876 TMI 07/08/2008 [End  ]
  INSERT INTO (loFormset.lcBatLin) FROM MEMVAR
  *T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]
ENDIF
*-- End of Function lfDLTMPBAT.

*!***************************************************************************
*!* Name        : lfDLTMPBAT
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Get temp batch per bin location
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLTMPBAT()
*!***************************************************************************
FUNCTION x_lfDLTMPBAT

lcTmpQuery = loFormSet.lcTmpQuery
SELECT(lcTmpQuery)
GOTO TOP
SCAN
  SCATTER MEMVAR MEMO
  SELECT IIF(loFormSet.llMatModul,'FABDYE','STYDYE')
  REPLACE dLlokDate WITH laData[5]
  m.Style     = IIF(loFormSet.llMatModul,m.ITEM,m.Style)
  m.Creason   = IIF(loFormSet.llMatModul,'MATERIAL LOCK INVENTORY     ' ,'STYLE LOCK INVENTORY     ')
  m.cWareCode = ladata[8]
  lnAlias = SELECT(0)
  SELECT STYDYE
  =SEEK(m.Style+IIF(EMPTY(m.Color),'',m.Color)+IIF(llWareHus,ladata[8],''))
  DO CASE
  CASE  lcCostMeth = 'S'  && Standard
    m.Cost    = STYLE.TotCost
    m.OldCost = STYLE.TotCost
  OTHERWISE       && 'A' Average
    m.Cost    = STYDYE.Ave_Cost
    m.OldCost = STYDYE.Ave_Cost
  ENDCASE

  SCAN REST WHILE STYLE+cwarecode+dyelot = m.Style+IIF(llWareHus,ladata[8],'')
    IF SEEK(STYDYE.STYLE+STYDYE.cwarecode+m.Bin,'WHBINLOC')
      SELECT WHBINLOC
      SCAN REST WHILE STYLE+cwarecode+clocation = STYDYE.STYLE+STYDYE.cwarecode+m.Bin
        FOR lnInd = 1 TO 8
          INDEX = STR(lnInd,1)
          m.OldStk&INDEX = WHBINLOC.Qty&INDEX
          m.Stk&INDEX    = WHBINLOC.Qty&INDEX
        ENDFOR
        m.OldTotStk = WHBINLOC.TotQty
        m.TotStk    = WHBINLOC.TotQty
        m.Dyelot    = STYDYE.Dyelot
        m.Scale     = STYLE.SCALE
        m.clocation = &lcTmpQuery..Bin
        INSERT INTO (lcBatLin) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN

ENDSCAN
*-- End of Function lfDLTMPBAT.


*!***************************************************************************
*!* Name        : lfvBinBrow
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Function to browse Bin Locations.
*!***************************************************************************
*!* Called from : lfDavMenu --> BN4MAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvBinBrow()
*!***************************************************************************
*T20071102.0018,7 TMI
FUNCTION lfvBinBrow
PARAMETERS loFormSet

  
PRIVATE lcbrfields ,lnAlias,lcFile_Ttl,lcWhExp,lcSeekExp,lnClrLen,lnClrStPos,;
  lnStyLen,lnStyStPos,lnScaLen,lnScaStPos,lnScaleCnt
LOCAL lnI,lcI,llFound

STORE '' TO lcFile_Ttl,lcWhExp,lcSeekExp
STORE 0 TO lnClrLen,lnClrStPos,lnStyLen,lnStyStPos,lnScaLen,lnScaStPos

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Begin]
*B608813,1 MMT 03/04/2209 Fix bugs of wrong browse of Bin when user change company[Start]
*lnDataSess = SET("Datasession" )
*SET DATASESSION TO loFormSet.DATASESSIONID
*B608813,1 MMT 03/04/2209 Fix bugs of wrong browse of Bin when user change company[End]
*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]

*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
IF USED("syustatc")
  SELECT syustatc
  SET ORDER TO CUSER_ID
ELSE
  SELECT 0
  lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
  IF oAriaApplication.multiinst
    lcSQLDICPATH =  oAriaApplication.ClientA4Path+'SQLDictionary\'
  ENDIF
  USE (lcSQLDICPATH +"syustatc") ORDER CUSER_ID
  = CURSORSETPROP('Buffering', 5, 'SYUSTATC' )  && Enable
ENDIF
*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]

lnAlias=SELECT(0)
=lfChkStrct()
IF !USED('WHBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHBINLST','SH')
ENDIF
IF !USED('SCALE')
  *B610350,1 TMI 06/02/2013 [Start] define variable to be checked later in this function
  LOCAL llOpnScl 
  llOpnScl = .T.
  *B610350,1 TMI 06/02/2013 [End  ] 
  =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF

LOCAL lcWareCode
lcWareCode = loFormSet.Ariaform1.pgfStyleInfo.Page2.cboLocation.VALUE  &&Warehouse code.
lcWareCode = IIF(lcWareCode = '******' , 'All' , lcWareCode )

lcMajor  = PADR(loFormSet.Ariaform1.kbStyleMajor.Keytextbox.VALUE,lnStyLen)
lcNonMjr = loFormSet.Ariaform1.kbNonMajor.Keytextbox.VALUE
lcNMKey  = loFormSet.lcSepart+IIF(AT('*',lcNonMjr)<>0,SUBSTR(lcNonMjr,1,AT('*',lcNonMjr)-1),lcNonMjr)

lcScale  = loFormSet.AriaForm1.pgfStyleInfo.page1.cntPlan.SCALE

STORE '' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
lnScaleCnt = 8
IF !gfGetMemVar('M_USEEXSSC') .OR. (!'*' $ lcNonMjr)

  SELECT SCALE
  =SEEK('S'+lcScale)
  lnScaleCnt = SCALE.CNT
  FOR lnI = 1 TO lnScaleCnt
    lcI = STR(lnI,1)
    lcSz&lcI = SCALE.SZ&lcI
  ENDFOR

ELSE

  lnScaleCnt = 8
  FOR lnI = 1 TO lnScaleCnt
    lcI = STR(lnI,1)
    lcSz&lcI = 'Qty'+lcI
  ENDFOR

ENDIF

llFound = .F.
SELECT WHBINLOC
DO CASE
CASE loFormSet.llAllColors AND ALLTRIM(lcWareCode) = "All"
  =gfSetOrder('WHBINLST')    && STYLE+CWARECODE+CLOCATION
  lcSeekExp = 'lcmajor+lcnmkey'
  llFound = gfSEEK(&lcSeekExp,'WHBINLOC')

CASE loFormSet.llAllColors AND ALLTRIM(lcWareCode) <> "All"
  =gfSetOrder('WHSTYBIN')    && CWARECODE+STYLE+CLOCATION
  lcSeekExp = 'PADR(ALLTRIM(lcWareCode),6)+lcmajor+lcnmkey'
  llFound = gfSEEK(&lcSeekExp,'WHBINLOC')

CASE !loFormSet.llAllColors AND ALLTRIM(lcWareCode) = "All"
  =gfSetOrder('WHBINLST')    && STYLE+CWARECODE+CLOCATION
  lcSeekExp = 'lcmajor+lcnmkey'
  llFound = gfSEEK(&lcSeekExp,'WHBINLOC')

CASE !loFormSet.llAllColors AND ALLTRIM(lcWareCode) <> "All"
  =gfSetOrder('WHBINLST')    && STYLE+CWARECODE+CLOCATION
  lcSeekExp = 'lcmajor+lcnmkey'
  *B609714,2 MMT 11/17/2011 cannot Browse Bin Invjl from Style screen for certain style-warehouse[Start]
  *!*    IF gfSEEK(&lcSeekExp,'WHBINLOC')
  *!*      LOCATE REST WHILE CWARECODE = lcWareCode
  *!*      llFound = FOUND()
  IF gfSEEK(PADR(&lcSeekExp,19)+lcWareCode,'WHBINLOC')
    llFound = .T.
    *B609714,2 MMT 11/17/2011 cannot Browse Bin Invjl from Style screen for certain style-warehouse[End]
  ENDIF

ENDCASE

IF !llFound
  lcMsg = 'No bin location found for Style '+ ;
    lcMajor+IIF(LEN(lcNMKey)=1,'',lcNMKey)+ ;
    IIF(!EMPTY(lcWareCode),' Warehouse ' +lcWareCode,'')
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Begin]
  *B608813,1 MMT 03/04/2209 Fix bugs of wrong browse of Bin when user change company[Start]
  *SET DATASESSION TO lnDataSess
  *B608813,1 MMT 03/04/2209 Fix bugs of wrong browse of Bin when user change company[End]
  =gfCloseTable('WHBINLOC')
  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]
  
    *B610350,1 TMI 06/02/2013 [Start]  close tables
    =lfCloseTbl('WHBINLOC')
    *=IIF(TYPE('llOpnScl')='L',gfCloseTbl('SCALE'),'')
    *B610350,1 TMI 06/02/2013 [End  ]  

  RETURN
ENDIF

*-- if user Select a style with all colors (if not extended Size scale) and all locations then browse for Cwarecode,Locations,Color
*-- if user Select a style with all colors (if not extended Size scale) and a location then browse for Locations,Color
*-- if user Select a style with a color and a location then browse for Locations
*-- if user Select a style with a color and all locations then browse for cWarecode,Locations

lcBrFields = ''
DO CASE
CASE !loFormSet.llAllColors AND ALLTRIM(lcWareCode) <> "All"
  lcBrFields = [clocation :15 :H='Location',]

CASE !loFormSet.llAllColors AND ALLTRIM(lcWareCode) = "All"
  lcBrFields = [cWareCode :15 :H='Warehouse',clocation :15 :H='Location',]

CASE loFormSet.llAllColors AND ALLTRIM(lcWareCode) ="All"
  lcBrFields = [cWareCode :15 :H='Warehouse',clocation :15 :H='Location',lcColor=gfCodDes(SUBSTR(STYLE,lnClrStPos,lnClrLen), "COLOR"):H='Colour',]

CASE loFormSet.llAllColors AND ALLTRIM(lcWareCode) <>"All"
  lcBrFields = [lcColor=gfCodDes(SUBSTR(STYLE,lnClrStPos,lnClrLen), "COLOR"):H='Colour',clocation :15 :H='Location',]

ENDCASE

*T20071102.0018,10/C200876 TMI 10/28/2008 [Start] add all fields to the browse, with not needed ones with title ' - '
*FOR lnI=1 TO lnScaleCnt
FOR lnI=1 TO 8
  *T20071102.0018,10/C200876 TMI 10/28/2008 [End  ]
  lcI=STR(lnI,1)
  IF !EMPTY(lcSz&lcI)
    *T20071102.0018,10/C200876 TMI 07/17/2008 [Start]
    *lcBrFields = lcBrFields + "Qty"+lcI+" :H=PADL(lcSz"+lcI+",5) :P='9999999',"
    *T20071102.0018,10/C200876 TMI 10/29/2008 [Start] Add 'I' for Inventory
    *lcBrFields = lcBrFields + "Qty&lcI :H='"+lcSz&lcI+"' :P='9999999',"
    lcBrFields = lcBrFields + "Qty&lcI :H='I "+lcSz&lcI+"' :P='9999999',"
    *T20071102.0018,10/C200876 TMI 10/29/2008 [End  ]
    *T20071102.0018,10/C200876 TMI 07/17/2008 [End  ]
    *T20071102.0018,10/C200876 TMI 10/28/2008 [Start]
  ELSE
    lcBrFields = lcBrFields + "Qty&lcI :H='   ' :P='9999999',"
    *T20071102.0018,10/C200876 TMI 10/28/2008 [End  ]
  ENDIF
ENDFOR
lcBrFields = lcBrFields + "TotQty :H='Total ' :P='999999999',"

*--Add allocated data  **PADL(lcSz"+lcI+",5)
*T20071102.0018,10/C200876 TMI 10/28/2008 [Start] add all fields to the browse, with not needed ones with title ' - '
*FOR lnI=1 TO lnScaleCnt
FOR lnI=1 TO 8
  *T20071102.0018,10/C200876 TMI 10/28/2008 [End  ]
  lcI=STR(lnI,1)
  IF !EMPTY(lcSz&lcI)
    *T20071102.0018,10/C200876 TMI 10/29/2008 [Start] Add 'A' for allocated
    *lcBrFields = lcBrFields + "Alo&lcI :H='"+lcSz&lcI+"' :P='9999999',"
    lcBrFields = lcBrFields + "Alo&lcI :H='A "+lcSz&lcI+"' :P='9999999',"
    *T20071102.0018,10/C200876 TMI 10/29/2008 [End  ]
    *T20071102.0018,10/C200876 TMI 10/28/2008 [Start]
  ELSE
    lcBrFields = lcBrFields + "Alo&lcI :H='   ' :P='9999999',"
    *T20071102.0018,10/C200876 TMI 10/28/2008 [End  ]
  ENDIF
ENDFOR
lcWhExp = IIF(ALLTRIM(lcWareCode) = "All",'.T.',"cWareCode='"+lcWareCode+"'")
lcBrFields = lcBrFields + "TotAlo :H='Total Alocated ' :P='999999999'"
*T20071102.0018,10/C200876 TMI 05/28/2008 [Start] Adding the Bulk/Pick Flag, section code, Flat/Hang and Replenishment Flag fields
lcBrFields = lcBrFields + ",WHSLOC.cblkpck    :H='Bulk/Pick'"  ;
  + ",WHSLOC.csection   :H='Section'"    ;
  + ",WHSLOC.cflathang  :H='Flat/Hang'"  ;
  + ",WHSLOC.creplenish :H='Replenishment'"
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]

*T20071102.0018,10/C200876 TMI 07/31/2008 [Start]
DIMENSION laInputMask[1,2]
laInputMask=''
*T20071102.0018,10/C200876 TMI 11/03/2008 [Start]
*=lfDefInputMaskArr(@laInputMask,lnScaleCnt,'QTY','999999')
*=lfDefInputMaskArr(@laInputMask,lnScaleCnt,'ALO','999999')
=lfDefInputMaskArr(@laInputMask,8,'QTY','999999')
=lfDefInputMaskArr(@laInputMask,8,'ALO','999999')
*T20071102.0018,10/C200876 TMI 11/03/2008 [End  ]
=lfDefInputMaskArr(@laInputMask,1,'TOTQTY','999999')
=lfDefInputMaskArr(@laInputMask,1,'TOTALO','999999')
*T20071102.0018,10/C200876 TMI 07/31/2008 [End  ]


SELECT WHBINLOC
*T20071102.0018,10/C200876 TMI 05/28/2008 [Start] make a relation with the WHSLOC file
LOCAL lcSvOrder
IF !USED('WHSLOC')
  *B610350,1 TMI 06/02/2013 [Start] define variable to be checked later in this function
  LOCAL llOpnWhsLoc
  llOpnWhsLoc = .T.
  *B610350,1 TMI 06/02/2013 [End  ] 
  =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
ENDIF
SELECT WHSLOC
lcSvOrder = ORDER()
=gfSetOrder('WHSLOC')
SELECT WHBINLOC
SET RELATION TO WHBINLOC.cwarecode+WHBINLOC.clocation+SPACE(19) INTO WHSLOC
LOCATE
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]


IF ORDER()='WHSTYBIN'
  lcKey = "PADR('"+lcWareCode+"',6) FOR STYLE='"+lcMajor+lcNMKey+"'"
  *T20071102.0018,10/C200876 TMI 07/21/2008 [Start] Add a prefrence to the lookup browse
  =ARIABROW(lcKey,'Bin Location',;
    gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  *!*	  =ARIABROW(lcKey,'Bin Location',;
  *!*	            gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok',,,,,,,,,,,'WHBINLOC')
  *T20071102.0018,10/C200876 TMI 07/21/2008 [End  ]
ELSE
  lcKey = "'"+lcMajor+lcNMKey+"' FOR "+lcWhExp
  *T20071102.0018,10/C200876 TMI 07/21/2008 [Start]
  =ARIABROW(lcKey,'Bin Location',;
    gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
  *!*	  =ARIABROW(lcKey,'Bin Location',;
  *!*	            gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok',,,,,,,,,,,'WHBINLOC')
  *T20071102.0018,10/C200876 TMI 07/21/2008 [End  ]
ENDIF

*T20071102.0018,10/C200876 TMI 05/28/2008 [Start] restore the selected order
SELECT WHSLOC
=gfSetOrder(lcSvOrder)
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]

*T20071102.0018,10/C200876 TMI 29/10/2008 [Start] close the WHBINLOC file
=gfCloseTable('WHBINLOC')
*T20071102.0018,10/C200876 TMI 29/10/2008 [End  ]

SELECT(lnAlias)
*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Begin]
*B608813,1 MMT 03/04/2209 Fix bugs of wrong browse of Bin when user change company[Start]
*SET DATASESSION TO lnDataSess
*B608813,1 MMT 03/04/2209 Fix bugs of wrong browse of Bin when user change company[End]
*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]

    *B610350,1 TMI 06/02/2013 [Start]  close tables
    *=IIF(TYPE('llOpnScl')='L',lfCloseTbl('SCALE'),'')
    *=IIF(TYPE('llOpnWhsLoc')='L',lfCloseTbl('WHSLOC'),'')
    *B610350,1 TMI 06/02/2013 [End  ]  

*-- End of Function lfvBinBrow. 

*:**************************************************************************
*:* Name        : lfDefInputMaskArr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/04/2008
*:* Purpose     : Define needed fields for the laInputMask Array
*:***************************************************************************
FUNCTION lfDefInputMaskArr
LPARAMETERS laArrToDef,lnFldNo,lcFldNm,lcInputMask
LOCAL lnArrLen,lnI,lnJ
lnArrLen = ALEN(laArrToDef,1)
lnNewLenght = lnFldNo + lnArrLen
DIMENSION laArrToDef[lnNewLenght,2]

lnJ = 1
FOR lnI = lnArrLen+1 TO lnNewLenght
  laArrToDef[lnI,1] = lcFldNm+ALLTRIM(STR(lnJ))
  laArrToDef[lnI,2] = lcInputMask

  lnJ = lnJ + 1
ENDFOR

IF lnFldNo=1
  laArrToDef[lnNewLenght,1] = SUBSTR(laArrToDef[lnNewLenght,1],1,LEN(laArrToDef[lnNewLenght,1])-1)
ENDIF
*-- end of lfDefInputMaskArr.

*!***************************************************************************
*!* Name        : lfChngQty
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Prevent User to Change the Pack Qty and guide him to do that
*!*             : in the Pick Ticket Screen.
*!***************************************************************************
*!* Called from : AlPlist.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChngQty()
*!***************************************************************************
FUNCTION lfChngQty

IF !lfIsUseBin()
  RETURN
ENDIF

*T20071102.0018,10/C200876 TMI 06/02/2008 [Start] this function needs to be changed today

IF gfGetMemVar('M_DLUSEBIN')
  IF SUBSTR(loFormSet.NAME,4) = 'ALPLIST' AND lnStyQty <> lcOldVal
    =gfModalGen("INM00000B00000","DIALOG",.F.,.F.,'You can not amend Qty. here,please do that ' + ;
      'at the pick ticket screen')
    IF _CUROBJ = OBJNUM(lnBrCtnQty)
      lnBrCtnQty = lcOldVal
      SHOW GET lnBrCtnQty
    ENDIF
    IF _CUROBJ = OBJNUM(lnStyQty)
      lnStyQty = lcOldVal
      SHOW GET lnStyQty
    ENDIF
  ENDIF
  IF SUBSTR(loFormSet.NAME,4) = 'ALAUTP'
    lcPakQtyFl = 'm.'+VARREAD()
    IF &lcPakQtyFl <> lnOldQty
      =gfModalGen("INM00000B00000","DIALOG",.F.,.F.,'You can not amend Qty. here,please do that ' + ;
        'at the pick ticket screen')
      &lcPakQtyFl = lnOldQty
      SHOW GET &lcPakQtyFl
    ENDIF
  ENDIF
  RETURN .F.
ELSE
  RETURN .T.
ENDIF
*-- End of Function lfChngQty.

*!***************************************************************************
*!* Name        : lfAlSavAut
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Save Data to Automatic Allocation screen
*!***************************************************************************
*!* Called from : ALAutAL.PRG (ALLOCATION BY STYLE) , MBIMAIN.PRG (Allocate from
*!*             : sales order screen (CP#200236)
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAlSavAut()
*!***************************************************************************
*T20071102.0018,7 TMI 03/23/2008
*B608850,1 TMI 04/18/2009 [Start] rewrite this function
*FUNCTION lfAlSavAut
FUNCTION x_lfAlSavAut
*B608850,1 TMI 04/18/2009 [End  ]
LOCAL lnSlct,lnDataSession

lcAlocOrd = PADR(gfGetMemvar('M_ALOCORD'),1)
DO CASE
CASE lcAlocOrd = 'Y'
  *- go to alloaction
CASE lcAlocOrd = 'N'
  RETURN
CASE lcAlocOrd = 'I'
  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'Do you want to pick the created Sales Order?') <> 1
    RETURN
  ENDIF
OTHERWISE
  RETURN
ENDCASE

lnSlct = SELECT(0)
*C200876 TMI 02/12/2009 [Start] comment this code and move it to inside the init event of alordal screen
*!*	lnDataSession = loFormSet.DataSessionID

*!*	PRIVATE oFormAlOrdAl
*!*	DO FORM (oAriaApplication.ScreenHome+'AL\ALORDAL.SCX') NAME oFormAlOrdAl WITH OrdHdr.Order NOSHOW

*!*	WITH oFormAlOrdAl
*!*	  SET DATASESSION TO .DataSessionID
*!*	  .Visible = .F.
*!*	  .Name = 'AWRALORDAL'      && to cope with the aria naming style for the screens when be run
*!*	  .cParentScreenName = 'AWRALORDAL'
*!*	  .MInittriggers()
*!*
*!*	  .llForceAlc = .F.
*!*	  .llgenpktk = .T.
*!*	  SELECT (.lc_TmpOrdL)
*!*	  SCAN
*!*	    .llFLine = .T.
*!*	    .AriaForm1.cmbPick.Click(.T.)
*!*	  ENDSCAN
*!*
*!*	  .SaveFiles
*!*	ENDWITH

*!*	oFormAlOrdAl.Release
*!*	RELEASE oFormAlOrdAl
*!*	SET DATASESSION TO lnDataSession
*!*	loFormSet.mOptionsMenuPad()


*B608850,1 TMI 04/18/2009 [Start]
*!*    PRIVATE llCalledFromSoord
*!*    llCalledFromSoord = .T.
*!*    *B608850,1   TMI 04/15/2009 [Start]
*!*    LOCAL lcOrder
*!*    lcOrder = ORDHDR.ORDER
*!*    *B608850,1   TMI 04/15/2009 [End  ]
*!*    oAriaApplication.DoProgram("AWRALORDAL",'"'+OrdHdr.Order+'"',.F.,'AL')
*!*    TRY
*!*    loFormSet.mOptionsMenuPad()
*!*    CATCH
*!*    ENDTRY
*!*    *C200876 TMI 02/12/2009 [End  ]


PRIVATE oFormAlOrdAl,lcOrd
lcOrd = OrdHdr.ORDER
DO FORM (oAriaApplication.ScreenHome+'AL\ALORDAL.SCX') NAME oFormAlOrdAl WITH lcOrd LINKED

lnSvDataSession = SET("Datasession")
WITH oFormAlOrdAl
  SET DATASESSION TO .DATASESSIONID
  .NAME = 'AWRALORDAL'      && to cope with the aria naming style for the screens when be run
  .cParentScreenName = 'AWRALORDAL'
  .MInittriggers()
  .llForceAlc = .F.
  .llgenpktk = .T.
  .CHANGEMODE('E')

  SELECT (.lc_TmpOrdL)
  SCAN
    .llFLine = .T.
    .AriaForm1.cmbPick.CLICK(.T.)
  ENDSCAN

  .SaveFiles
ENDWITH

STORE 0 TO lnQty,lnPik
SELECT ORDLINE
=gfSeek('O'+lcOrd,'ORDLINE')
lcPikTkt = ORDLINE.PIKTKT
SUM TOTQTY,TOTPIK TO lnQty,lnPik ;
  REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrd

DO CASE
CASE lnPik = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No line has been picked')
CASE lnPik = lnQty
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order &lcOrd totally picked with piktkt# &lcPikTkt')
CASE lnPik < lnQty
  =oAriaApplication.DoProgram("AWRALORDAL",'"'+OrdHdr.ORDER+'"',.F.,'AL')
ENDCASE

SET DATASESSION TO (lnSvDataSession)

IF lnPik = 0 OR lnPik = lnQty
  TRY
    oFormAlOrdAl.RELEASE
  CATCH
  ENDTRY
ELSE
  oFormAlOrdAl.ACTIVATE
ENDIF


*B608850,1 TMI 04/18/2009 [End  ]


*:**************************************************************************
*:* Name        : lfAlSavAut
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2009
*:* Purpose     : allocation process
*:***************************************************************************
*:* Called from : SO
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAlSavAut()
*:***************************************************************************
FUNCTION lfAlSavAut
LOCAL lnSlct,lnDataSession
lnSlct = SELECT(0)

*B608850,1 TMI 04/19/2009 [Start]
IF TYPE('loFormSet.lcLastOrder')='U'
  loFormSet.ADDPROPERTY('lcLastOrder')
ENDIF
loFormSet.lcLastOrder = ''
*B608850,1 TMI 04/19/2009 [End  ]

lcAlocOrd = PADR(gfGetMemvar('M_ALOCORD'),1)
DO CASE
CASE lcAlocOrd = 'Y'
  *- do alloaction without inquering
CASE lcAlocOrd = 'N'
  RETURN
  *C201540,1 SAB 12/11/2012 Hide Do you want to pick Sales Order message for bid sales order [Start]
  *CASE lcAlocOrd = 'I'
CASE lcAlocOrd = 'I' .AND. ORDHDR.STATUS = 'O'
  *C201540,1 SAB 12/11/2012 Hide Do you want to pick Sales Order message for bid sales order [End]
  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'Do you want to pick the created Sales Order?') <> 1
    RETURN
  ENDIF
OTHERWISE
  RETURN
ENDCASE

lcOrder = ORDHDR.ORDER
LOCAL lnQty,lnPik,lnOldPik
STORE 0 TO lnQty,lnPik,lnOldPik
=gfSeek('O'+lcOrder,'ORDLINE')
**B609146,1 [Start] select ordline
SELECT ORDLINE
**B609146,1 [End  ]

*- get qty/picked style before picking
SUM TOTPIK TO lnOldPik ;
  REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder

*!C201527,1 HIA 10/21/2012 Change Allocation Calculation [T20121002.0002][Begin]
DECLARE laRltdFld[1,2]
STORE '' TO laRltdFld,LCAUTOAL
laRltdFld[1,1] = "AUTOALCC"
laRltdFld[1,2] = 'LCAUTOAL'
=gfRltFld(ORDHDR.cDivision,@laRltdFld,'CDIVISION')
LCAUTOAL = IIF(EMPTY(LCAUTOAL),"O",LCAUTOAL)
*!C201527,1 HIA 10/21/2012 Change Allocation Calculation [T20121002.0002][End]

IF lfBinAlloc(lcOrder)   && the allocation process
  SELECT ORDLINE
  =gfSeek('O'+lcOrder,'ORDLINE')
  lcPikTkt = ORDLINE.PIKTKT
  *- get the picked qty to compare with
  SUM TOTQTY,TOTPIK TO lnQty,lnPik ;
    REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder
ENDIF

DO CASE
CASE lnPik = 0 OR lnPik = lnOldPik
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No stock available to pick on any order line or no changes has been occured in the picked qtys')
CASE lnPik = lnQty
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order &lcOrder totally picked with piktkt# &lcPikTkt')
CASE lnPik < lnQty
  *B609146,1 TMI [START]  information message to open sales order allocation screen
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,[Please open the SALES ORDER ALLOCATION screen to edit allocated qty's.])
  **B609146,1 TMI [END  ]

  *- assign the order# to show from within the SO screen
  *BB08893,1 MMT 06/11/2009 Fix bug of not opening Allocation screen from Sales order screen  [Start]
  *loFormSet.lcLastOrder = ORDHDR.ORDER
  loFormSet.lcLastOrder = lcOrder
  *BB08893,1 MMT 06/11/2009 Fix bug of not opening Allocation screen from Sales order screen  [End]
  *B609146,2 TMI [START] no need for this part as there is a new option to open sales order allocation screen added for DCC
  *IF loFormSet.ActiveMode = 'V'  && The mode now changed to view mode, last was EDIT mode
  **B609146,1 [Start] ask the user if he want to open the allocation screen
  *IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'Some lines picked partially, do you need to open the allocation screen?') = 1
  *  **B609146,1 [End  ]
  *  =lfCALLALOC(.T.)
  *  **B609146,1 [Start]
  *ENDIF
  **B609146,1 [End  ]
  *ENDIF
  *B609146,2 TMI [END] no need for this part as there is a new option to open sales order allocation screen added for DCC

ENDCASE

**B609146,1 [Start] restore the old alias
SELECT (lnSlct)
**B609146,1 [End  ]

*-- end of lfAlSavAut.

*:**************************************************************************
*:* Name        : lfCALLALOC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/22/2009
*:* Purpose     : calls the ALORDAL screen from within the SO screen
*:***************************************************************************
*:* Called from : SO
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCALLALOC()
*:***************************************************************************
FUNCTION lfCALLALOC
PARAMETERS llEditMode
IF IIF(llEditMode,.T.,loFormSet.ActiveMode = 'S') AND TYPE('loFormSet.lcLastOrder') = 'C' AND !EMPTY(loFormSet.lcLastOrder)
  **B609146,1 [Start] send the parameter using a variable
  *=oAriaApplication.DoProgram("AWRALORDAL",'"'+loFormSet.lcLastOrder+'"',.F.,'AL')
  LOCAL lcParam
  lcParam = loFormSet.lcLastOrder
  =oAriaApplication.DoProgram("AWRALORDAL",'"'+lcParam+'"',.F.,'AL')
  **B609146,1 [End  ]
  loFormSet.lcLastOrder = ''
ENDIF
*-- end of lfCALLALOC.

*:**************************************************************************
*:* Name        : lfBinAlloc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2009
*:* Purpose     : ALLOCATE
*:***************************************************************************
FUNCTION lfBinAlloc
PARAMETERS lcOrder

LOCAL lnSlct,lnQYT,lnAlo
lnSlct = SELECT(0)
STORE 0 TO lnQYT,lnAlo
*B610160,1 SAB 11/25/2012 Hide Assing bin Message when receiving Material PO [Start]
*=lfOpnFiles("WHBINLOC,PKBINLOC,PIKTKT","WHBINLST,PKLINE,ORDPIK",'')
IF lfIsUseBin()
  =lfOpnFiles("WHBINLOC,PKBINLOC,PIKTKT","WHBINLST,PKLINE,ORDPIK",'')
ENDIF
*B610160,1 SAB 11/25/2012 Hide Assing bin Message when receiving Material PO [End]

* order of WHBINLST  : STYLE+CWARECODE+CLOCATION

*- just to check if there are lines to be allocated
laReady2Alo = 0
SELECT ORDLINE
=gfSEEK('O'+lcOrder,'ORDLINE')
*B609511,1 MMT 01/24/2011 Check if use Bin location  before calling automic picking function in bn4main[Start]
IF lfisusebin()
  *B609511,1 MMT 01/24/2011 Check if use Bin location  before calling automic picking function in bn4main[End]
  SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder
    IF gfSeek(ORDLINE.STYLE+ORDLINE.CWARECODE,'WHBINLOC')
      SELECT WHBINLOC
      LOCATE
      SUM TOTQTY-TOTALO TO laReady2Alo
      IF laReady2Alo>0
        EXIT
      ENDIF
    ENDIF
  ENDSCAN
  IF laReady2Alo = 0  && no lines to be allocated
    SELECT (lnSlct)
    RETURN .F.
  ENDIF
  *B609511,1 MMT 01/24/2011 Check if use Bin location  before calling automic picking function in bn4main[Start]
ENDIF
*B609511,1 MMT 01/24/2011 Check if use Bin location  before calling automic picking function in bn4main[End]

*- check that the stock values in bins and the WH are consistent
LOCAL lnDataSessionID
lnDataSessionID = loFormSet.DATASESSIONID
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
loMainForm = loFormSet
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
*- create this object to hide the loFormSet object of the SO SCREEN, so we can able to call the ALORDAL
PRIVATE loFormSet
loFormSet = CREATEOBJECT('CUSTOM')
loFormSet.NAME = 'AWRALORDAL'
loFormSet.ADDPROPERTY('lc_TmpOrdL')
loFormSet.ADDPROPERTY('DataSessionId',lnDataSessionID)
loFormSet.ADDPROPERTY('llGenPkTk',.T.)
loFormSet.ADDPROPERTY('llUseDyes',gfGetMemVar('M_DYELOT')='Y')

*- Call the "lfCHKFIRST" function
SELECT ORDLINE
SET KEY TO 'O'+lcOrder
loFormSet.lc_TmpOrdL = 'ORDLINE'
IF !lfCHKFIRST(.T.)
  RETURN .F.
  SELECT ORDLINE
  SET KEY TO
ENDIF
SELECT ORDLINE
SET KEY TO
=gfSeek('O'+lcOrder,'ORDLINE')

*- Create temporaroy ORDLINE file
loFormSet.lc_TmpOrdL = gfTempName()
SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + 10, 18]
laFileStru[lnFileStru + 1,1] = 'lForceAlc'
laFileStru[lnFileStru + 1,2] = 'L'
laFileStru[lnFileStru + 1,3] = 1
laFileStru[lnFileStru + 1,4] = 0
laFileStru[lnFileStru + 2,1] = 'cStatus'
laFileStru[lnFileStru + 2,2] = 'C'
laFileStru[lnFileStru + 2,3] = 1
laFileStru[lnFileStru + 2,4] = 0
laFileStru[lnFileStru + 3,1] = 'nForcdQty'
laFileStru[lnFileStru + 3,2] = 'N'
laFileStru[lnFileStru + 3,3] = 6
laFileStru[lnFileStru + 3,4] = 0
laFileStru[lnFileStru + 4,1] = 'lVldWare'
laFileStru[lnFileStru + 4,2] = 'L'
laFileStru[lnFileStru + 4,3] = 1
laFileStru[lnFileStru + 4,4] = 0
laFileStru[lnFileStru + 5,1] = 'lVldDye'
laFileStru[lnFileStru + 5,2] = 'L'
laFileStru[lnFileStru + 5,3] = 1
laFileStru[lnFileStru + 5,4] = 0
laFileStru[lnFileStru + 6,1] = 'nSteps'
laFileStru[lnFileStru + 6,2] = 'N'
laFileStru[lnFileStru + 6,3] = 2
laFileStru[lnFileStru + 6,4] = 0
laFileStru[lnFileStru + 7,1] = 'lAllocated'
laFileStru[lnFileStru + 7,2] = 'L'
laFileStru[lnFileStru + 7,3] = 1
laFileStru[lnFileStru + 7,4] = 0
laFileStru[lnFileStru + 8,1] = 'cReason'
laFileStru[lnFileStru + 8,2] = 'C'
laFileStru[lnFileStru + 8,3] = 6
laFileStru[lnFileStru + 8,4] = 0
laFileStru[lnFileStru + 9,1] = 'nSel'
laFileStru[lnFileStru + 9,2] = 'N'
laFileStru[lnFileStru + 9,3] = 1
laFileStru[lnFileStru + 9,4] = 0
laFileStru[lnFileStru + 10,1] = 'lSelect'
laFileStru[lnFileStru + 10,2] = 'L'
laFileStru[lnFileStru + 10,3] = 1
laFileStru[lnFileStru + 10,4] = 0

FOR lnCount = 1 TO 10
  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
    laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
    laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
    laFileStru[lnFileStru+lnCount,16]
  STORE 0  TO laFileStru[lnFileStru+lnCount,17],  laFileStru[lnFileStru+lnCount,18]
ENDFOR

DECLARE laIndeces[3,2]
laIndeces[1,1] = 'cOrdType + ORDER  + STORE + STYLE + STR(LINENO,6)'
laIndeces[1,2] = 'ORDLINST'
laIndeces[2,1] = 'cOrdType + ORDER  + STR(LineNo,6)'
laIndeces[2,2] = 'ORDLINE'
laIndeces[3,1] = 'cOrdType + ORDER  + STORE + CWARECODE + STYLE + STR(LineNo,6)'
laIndeces[3,2] = 'ORDLINWR'
=gfCrtTmp(loFormSet.lc_tmpordl, @laFileStru, @laIndeces)

*- Get the order lines into a temp file to use in allocation process
SELECT (loFormSet.lc_tmpordl)
ZAP
SELECT ORDLINE
IF gfSEEK('O'+lcOrder,'ORDLINE')
  SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder
    m.lSelect = .T.
    SCATTER MEMVAR MEMO
    m.lForceAlc  = .F.
    m.Picked = .T.
    m.nSel = 1
    m.cStatus    = ' '
    m.lVldWare   = (gfGetMemVar('M_WareHouse')='N')  .OR. ;
      SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
    m.lVldDye    = (gfGetMemVar('M_DYELOT')='N') .OR. STYLE.cDye_flg = 'N'  ;
      .OR. !EMPTY(m.Dyelot) .AND. SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
    m.nForcdQty  = 0
    m.lAllocated = .T.
    INSERT INTO (loFormSet.lc_tmpordl) FROM MEMVAR
  ENDSCAN
ENDIF


PRIVATE oAlObj
oAlObj = CREATEOBJECT("AL")

*- there are qty's to allocate

STORE ' ' TO lcChgPkTKt
STORE ' ' TO lcNewPikTkt
*-- Scan only the records that were not saved
*-- Update incomplete session with the current object number
llAcordMsg = .F.        && flag to know if release po/ct   message is diplayed
llAlocMsg  = .F.        && flag to know if alocate message is diplayed
llOkCancel = .F.        && flag to know if user agree to cancel alocation
llUpdPoCt  = .F.        && flag to know if user agree to update po/ct

SELECT (loFormSet.lc_TmpOrdL)
SET ORDER TO TAG ORDLINWR
LOCATE
SCAN
  =SEEK(CORDTYPE+ORDER+STR(LINENO,6),'ORDLINE')
  =SEEK(STYLE + cWareCode + SPACE(10), 'STYDYE')
  *!C201527,1 HIA 10/21/2012 Change Allocation Calculation [T20121002.0002] [Begin]
  *REPLACE PIK1 WITH PIK1 + MAX( MIN(STYDYE.STK1 - STYDYE.ALO1,QTY1-PIK1) , 0 ) ;
  *        PIK2 WITH PIK2 + MAX( MIN(STYDYE.STK2 - STYDYE.ALO2,QTY2-PIK2) , 0 ) ;
  *        PIK3 WITH PIK3 + MAX( MIN(STYDYE.STK3 - STYDYE.ALO3,QTY3-PIK3) , 0 ) ;
  *        PIK4 WITH PIK4 + MAX( MIN(STYDYE.STK4 - STYDYE.ALO4,QTY4-PIK4) , 0 ) ;
  *        PIK5 WITH PIK5 + MAX( MIN(STYDYE.STK5 - STYDYE.ALO5,QTY5-PIK5) , 0 ) ;
  *        PIK6 WITH PIK6 + MAX( MIN(STYDYE.STK6 - STYDYE.ALO6,QTY6-PIK6) , 0 ) ;
  *        PIK7 WITH PIK7 + MAX( MIN(STYDYE.STK7 - STYDYE.ALO7,QTY7-PIK7) , 0 ) ;
  *        PIK8 WITH PIK8 + MAX( MIN(STYDYE.STK8 - STYDYE.ALO8,QTY8-PIK8) , 0 ) ;
  *        TOTPIK WITH PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8
  IF LCAUTOAL = "A"
    REPLACE PIK1 WITH PIK1 + MAX( MIN(STYDYE.STK1 - STYDYE.ALO1,QTY1-PIK1) , 0 ) ;
      PIK2 WITH PIK2 + MAX( MIN(STYDYE.STK2 - STYDYE.ALO2,QTY2-PIK2) , 0 ) ;
      PIK3 WITH PIK3 + MAX( MIN(STYDYE.STK3 - STYDYE.ALO3,QTY3-PIK3) , 0 ) ;
      PIK4 WITH PIK4 + MAX( MIN(STYDYE.STK4 - STYDYE.ALO4,QTY4-PIK4) , 0 ) ;
      PIK5 WITH PIK5 + MAX( MIN(STYDYE.STK5 - STYDYE.ALO5,QTY5-PIK5) , 0 ) ;
      PIK6 WITH PIK6 + MAX( MIN(STYDYE.STK6 - STYDYE.ALO6,QTY6-PIK6) , 0 ) ;
      PIK7 WITH PIK7 + MAX( MIN(STYDYE.STK7 - STYDYE.ALO7,QTY7-PIK7) , 0 ) ;
      PIK8 WITH PIK8 + MAX( MIN(STYDYE.STK8 - STYDYE.ALO8,QTY8-PIK8) , 0 ) ;
      TOTPIK WITH PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8

  ELSE
    *B610264,1 HIA 03/06/2013 Aria4xp - SO - Pick process on saving the sales order [T20130204.0009][Start]
    *!*	  REPLACE PIK1 WITH PIK1 + MAX( MIN(STYDYE.STK1 - STYDYE.ORD1,QTY1-PIK1) , 0 ) ;
    *!*	          PIK2 WITH PIK2 + MAX( MIN(STYDYE.STK2 - STYDYE.ORD2,QTY2-PIK2) , 0 ) ;
    *!*	          PIK3 WITH PIK3 + MAX( MIN(STYDYE.STK3 - STYDYE.ORD3,QTY3-PIK3) , 0 ) ;
    *!*	          PIK4 WITH PIK4 + MAX( MIN(STYDYE.STK4 - STYDYE.ORD4,QTY4-PIK4) , 0 ) ;
    *!*	          PIK5 WITH PIK5 + MAX( MIN(STYDYE.STK5 - STYDYE.ORD5,QTY5-PIK5) , 0 ) ;
    *!*	          PIK6 WITH PIK6 + MAX( MIN(STYDYE.STK6 - STYDYE.ORD6,QTY6-PIK6) , 0 ) ;
    *!*	          PIK7 WITH PIK7 + MAX( MIN(STYDYE.STK7 - STYDYE.ORD7,QTY7-PIK7) , 0 ) ;
    *!*	          PIK8 WITH PIK8 + MAX( MIN(STYDYE.STK8 - STYDYE.ORD8,QTY8-PIK8) , 0 ) ;
    *!*	          TOTPIK WITH PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8

    REPLACE PIK1 WITH PIK1 + MAX( MIN(STYDYE.STK1 - STYDYE.ORD1 + QTY1,QTY1-PIK1) , 0 ) ;
      PIK2 WITH PIK2 + MAX( MIN(STYDYE.STK2 - STYDYE.ORD2 + QTY2,QTY2-PIK2) , 0 ) ;
      PIK3 WITH PIK3 + MAX( MIN(STYDYE.STK3 - STYDYE.ORD3 + QTY3,QTY3-PIK3) , 0 ) ;
      PIK4 WITH PIK4 + MAX( MIN(STYDYE.STK4 - STYDYE.ORD4 + QTY4,QTY4-PIK4) , 0 ) ;
      PIK5 WITH PIK5 + MAX( MIN(STYDYE.STK5 - STYDYE.ORD5 + QTY5,QTY5-PIK5) , 0 ) ;
      PIK6 WITH PIK6 + MAX( MIN(STYDYE.STK6 - STYDYE.ORD6 + QTY6,QTY6-PIK6) , 0 ) ;
      PIK7 WITH PIK7 + MAX( MIN(STYDYE.STK7 - STYDYE.ORD7 + QTY7,QTY7-PIK7) , 0 ) ;
      PIK8 WITH PIK8 + MAX( MIN(STYDYE.STK8 - STYDYE.ORD8 + QTY8,QTY8-PIK8) , 0 ) ;
      TOTPIK WITH PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8

    *B610264,1 HIA 03/06/2013 Aria4xp - SO - Pick process on saving the sales order [T20130204.0009][End]
  ENDIF
  *!C201527,1 HIA 10/21/2012 Change Allocation Calculation [T20121002.0002] [End]
  SCATTER MEMVAR MEMO

  =SEEK(CORDTYPE+ORDER+STR(LINENO,6),'ORDLINE')
  IF TOTPIK > ORDLINE.TOTPIK
    *-- If this is a new pick, generate a new picking ticket
    IF !ORDLINE.Picked .OR. cStatus = 'M'
      *-- Generate a different picking ticket per store/warehouse
      IF loFormSet.llGenPkTk
        IF lcChgPkTKt <> m.Order + m.Store + m.cWareCode
          IF EMPTY(m.Piktkt) OR M.PikTkt = REPLICATE("*",FSIZE("PIKTKT"))
            lcChgPkTKt = m.Order + m.Store + m.cWareCode
            lcNewPikTkt = oAlObj.lfGetPkTkt(m.Order, ORDHDR.cDivision, m.Store, m.cWareCode)
          ENDIF
        ENDIF
        lcPikTktNo = IIF(EMPTY(m.Piktkt) OR M.PikTkt = REPLICATE("*",FSIZE("PIKTKT")),lcNewPikTkt,m.PikTkt)
        *B610181,1 SAB 12/24/2012 Fix generating multible pick tickt records when create pick tickt from sales order [Start]
        *IF !SEEK(m.Order + lcPikTktNo, 'PIKTKT')
        IF !gfSeek(m.Order + lcPikTktNo, 'PIKTKT', 'ORDPIK')
          *B610181,1 SAB 12/24/2012 Fix generating multible pick tickt records when create pick tickt from sales order [End]
          INSERT INTO PIKTKT;
            (Piktkt, Account, STORE, ORDER, DATE, cWareCode, CustPo, STATUS);
            VALUES;
            (lcPikTktNo, m.account, m.Store, m.Order, oAriaApplication.SystemDate, m.cWarecode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')
          =gfAdd_Info('PIKTKT')
          *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
          IF ASCAN(loMainForm.laEvntTrig,PADR('UPDPCKPACK',10),1,ALEN(loMainForm.laEvntTrig,1),1) > 0
            =loMainForm.mDoTrigger(PADR('UPDPCKPACK',10))
          ENDIF
          *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
        ENDIF
      ENDIF
    ELSE
      lcPikTktNo = m.Piktkt
    ENDIF   &&EndIF !ORDLINE.Picked

    *-- in the order header
    IF cStatus = 'M'

      *-- Update allocated and ordered quantities in STYDYE
      *-- Update Warehouse record in STYDYE file.
      SELECT STYDYE
      IF SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
        REPLACE Ord1   WITH Ord1   + MAX(m.Qty1  -ORDLINE.Qty1,   0),;
          Ord2   WITH Ord2   + MAX(m.Qty2  -ORDLINE.Qty2,   0),;
          Ord3   WITH Ord3   + MAX(m.Qty3  -ORDLINE.Qty3,   0),;
          Ord4   WITH Ord4   + MAX(m.Qty4  -ORDLINE.Qty4,   0),;
          Ord5   WITH Ord5   + MAX(m.Qty5  -ORDLINE.Qty5,   0),;
          Ord6   WITH Ord6   + MAX(m.Qty6  -ORDLINE.Qty6,   0),;
          Ord7   WITH Ord7   + MAX(m.Qty7  -ORDLINE.Qty7,   0),;
          Ord8   WITH Ord8   + MAX(m.Qty8  -ORDLINE.Qty8,   0),;
          TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6   + Ord7 + Ord8
        REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1 ,  0),;
          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2 ,  0),;
          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3 ,  0),;
          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4 ,  0),;
          Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5 ,  0),;
          Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6 ,  0),;
          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7 ,  0),;
          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8 ,  0),;
          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6 + Alo7 + Alo8
      ENDIF
      *-- If dyelots, update dyelot record in STYDYE file
      *-- with the allocated quantities only
      IF loFormSet.llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND.;
          SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
        REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,  0),;
          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,  0),;
          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,  0),;
          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,  0),;
          Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,  0),;
          Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,  0),;
          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,  0),;
          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,  0),;
          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6   + Alo7 + Alo8
      ENDIF
      *-- Update the style  record in STYLE file.
      SELECT STYLE
      REPLACE Ord1   WITH Ord1   + MAX(m.Qty1  -ORDLINE.Qty1,   0),;
        Ord2   WITH Ord2   + MAX(m.Qty2  -ORDLINE.Qty2,   0),;
        Ord3   WITH Ord3   + MAX(m.Qty3  -ORDLINE.Qty3,   0),;
        Ord4   WITH Ord4   + MAX(m.Qty4  -ORDLINE.Qty4,   0),;
        Ord5   WITH Ord5   + MAX(m.Qty5  -ORDLINE.Qty5,   0),;
        Ord6   WITH Ord6   + MAX(m.Qty6  -ORDLINE.Qty6,   0),;
        Ord7   WITH Ord7   + MAX(m.Qty7  -ORDLINE.Qty7,   0),;
        Ord8   WITH Ord8   + MAX(m.Qty8  -ORDLINE.Qty8,   0),;
        TotOrd WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6   + Ord7 + Ord8
      REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,  0),;
        Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,  0),;
        Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,  0),;
        Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,  0),;
        Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,  0),;
        Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,  0),;
        Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,  0),;
        Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,  0),;
        TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6   + Alo7 + Alo8

    ELSE
      *-- Update allocated and ordered quantities in STYDYE
      *-- Update Warehouse record in STYDYE file.
      SELECT STYDYE
      IF SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle) + m.cWareCode + SPACE(10))
        REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,0),;
          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,0),;
          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,0),;
          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,0),;
          Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,0),;
          Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,0),;
          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,0),;
          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,0),;
          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6 + Alo7 + Alo8
      ENDIF
      *-- If dyelots, update dyelot record in STYDYE file
      IF loFormSet.llUseDyes .AND. STYLE.cDye_flg = 'Y' .AND.;
          SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle)+ m.cWareCode + m.Dyelot, 'STYDYE')
        REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,  0),;
          Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,  0),;
          Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,  0),;
          Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,  0),;
          Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,  0),;
          Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,  0),;
          Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,  0),;
          Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,  0),;
          TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6   + Alo7 + Alo8
      ENDIF

      *-- Update the style  record in STYLE file.
      SELECT STYLE
      *- Read ALternate style
      =SEEK(IIF(EMPTY(m.AltStyle),m.Style,m.AltStyle) )
      REPLACE Alo1   WITH MAX(Alo1 + m.Pik1 - ORDLINE.Pik1,0),;
        Alo2   WITH MAX(Alo2 + m.Pik2 - ORDLINE.Pik2,0),;
        Alo3   WITH MAX(Alo3 + m.Pik3 - ORDLINE.Pik3,0),;
        Alo4   WITH MAX(Alo4 + m.Pik4 - ORDLINE.Pik4,0),;
        Alo5   WITH MAX(Alo5 + m.Pik5 - ORDLINE.Pik5,0),;
        Alo6   WITH MAX(Alo6 + m.Pik6 - ORDLINE.Pik6,0),;
        Alo7   WITH MAX(Alo7 + m.Pik7 - ORDLINE.Pik7,0),;
        Alo8   WITH MAX(Alo8 + m.Pik8 - ORDLINE.Pik8,0),;
        TotAlo WITH Alo1 + Alo2 + Alo3 + Alo4 + Alo5 + Alo6  + Alo7 + Alo8
    ENDIF

    *-- Update ORDLINE
    SELECT ORDLINE
    lcCrPkTkt   = ORDLINE.PikTkt
    m.PikTkt = IIF(EMPTY(lcPikTktNo),lcCrPkTkt,lcPikTktNo)
    m.PikDate = IIF(m.PikTkt <> "******", PIKTKT.DATE, oAriaApplication.SystemDate)
    GATHER MEMVAR MEMO

    *:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27[Start]
    IF lcPikTktNo <> '******' AND 'AS' $ oAriaApplication.CompanyInstalledModules AND SEEK(m.cWareCode,'WareHous','WareHous') AND ;
        SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT','ACCFACT') AND  SEEK(EDIACPRT.cPartCode+'940','EDIPD','PARTTRANS')
      SELECT EDITRANS
      IF !SEEK('940'+PADR(lcPikTktNo,40)+'W'+WareHous.cThrdPLPr,'EDITRANS','TYPEKEY')
        INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
          ('940',lcPikTktNo,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
      ENDIF
      REPLACE cStatus WITH 'N'
      =gfAdd_Info('EDITRANS')
    ENDIF

    *- call the custom pick ticket function
    =lfSVBNALOC()
  ENDIF

ENDSCAN

*B610858,1 TMI 09/15/2014 14:28 [Start] check if tables are buffered the call gltableupdate, otherwise call flush
SELECT style
IF CURSORGETPROP("Buffering")<>1
  =gfTableUpdate(.T.)
ELSE
  FLUSH in style    
ENDIF 

SELECT stydye
IF CURSORGETPROP("Buffering")<>1
  =gfTableUpdate(.T.)
ELSE
  FLUSH in stydye
ENDIF 
*B610858,1 TMI 09/15/2014 14:28 [End  ] 

SELECT (lnSlct)
RETURN .T.
*-- end of lfBinAlloc.



*:**************************************************************************
*:* Name        : lfSVBNALOC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : save allocation
*:***************************************************************************
FUNCTION lfSVBNALOC

IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lnPicked,lnRemain,lcAsecend,llPickOne,lcStyle,lnOldAlias,laPikQty,lcBinloc
PRIVATE lcPikTkt,lcSortBin,lcFlatHang,lcWareCode,lcOrdFile,lcPrmClss,lcSecClss,lcRemClss


DIMENSION laPikQty[9]
STORE 0 TO laPikQty,lnRemain,lnOldAlias,lnPicked
STORE '' TO lcPikTkt,lcOrdFile
STORE .F. TO llPickOne
*T20071102.0018,10/C200876 TMI 10/23/2008 [Start]
*A=Automatic Allocation  S=Allocation by STyle  O=Allocation by Order
lcALLOCATBY = ''
*T20071102.0018,10/C200876 TMI 10/23/2008 [End  ]

DO CASE
CASE SUBSTR(loFormSet.NAME,4) = 'ALAUTAL'
  lcTmpOrdLn = loFormSet.lcTmpOrdLn

  IF !USED(lcTmpOrdLn)
    lcTmpOrdLn = 'ORDLINE'
  ENDIF
  *T20071102.0018,10/C200876 TMI 10/23/2008 [Start]
  lcALLOCATBY = 'A'
  *T20071102.0018,10/C200876 TMI 10/23/2008 [End  ]

  *C201070 TMI 26/02/2009 [Start] comment out
  *!*	  *C201070 TMI 25/02/2009 [Start] do not allocate a line no selected
  *!*	  IF &lcTmpOrdLn..lnSel <> 1
  *!*	    RETURN
  *!*	  ENDIF
  *!*	  *C201070 TMI 25/02/2009 [End  ]
  *C201070 TMI 26/02/2009 [End  ]

CASE SUBSTR(loFormSet.NAME,4) = 'ALORDAL'
  lcTmpOrdLn = loFormSet.lc_tmpOrdL

  IF !USED(lcTmpOrdLn)
    lcTmpOrdLn = 'ORDLINE'
  ENDIF
  *T20071102.0018,10/C200876 TMI 10/23/2008 [Start]
  lcALLOCATBY = 'O'
  *T20071102.0018,10/C200876 TMI 10/23/2008 [End  ]

CASE SUBSTR(loFormSet.NAME,4) = 'ALSTYAL'
  lcTmpOrdLn = 'ORDLINE1'

  *T20071102.0018,10/C200876 TMI 10/23/2008 [Start]
  lcALLOCATBY = 'S'
  *T20071102.0018,10/C200876 TMI 10/23/2008 [End  ]

CASE SUBSTR(loFormSet.NAME,4) $ '|ALPKTKT|ALRELPI'
  lcTmpOrdLn = 'ORDLINE'

ENDCASE


lnOldAlias = SELECT(0)
lcS = IIF(SUBSTR(loFormSet.NAME,4) = 'ALSTYAL','1','')
lcPikTkt  = IIF(!EMPTY(&lcTmpOrdLn..PikTkt) .AND. &lcTmpOrdLn..PikTkt<>'******',&lcTmpOrdLn..PikTkt,;
  IIF(!EMPTY(ALLTRIM(ORDLINE&lcS..PIKTKT)),ORDLINE&lcS..PIKTKT,PIKTKT.PIKTKT))

*- nothing to do if this is not a piktkt
IF lcPikTkt = "******"
  RETURN
ENDIF

*B609079,1 HES 09/11/2009 Error Message on Allocation by style [Start]
*!*	SELECT ORDLINE
*!*	*T20071102.0018,10/C200876 TMI 10/23/2008 [Start] save a refrence of who allocated
*!*	replace ORDLINE.CALLOCATBY WITH lcALLOCATBY
*!*	*T20071102.0018,10/C200876 TMI 10/23/2008 [End  ]
SELECT ('ORDLINE'+ lcS)
REPLACE CALLOCATBY WITH lcALLOCATBY
*B609079,1 HES 09/11/2009 Error Message on Allocation by style [End]

=lfOpnFiles("WHBINLOC,WHSLOC,PKBINLOC","WHBINLST,WHSLOC,PKLINE",lcPikTkt)

*PKBINLOC order PKLINE :  PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION

IF !USED('SCALE')
  =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF

*B608894,1 TMI 06/14/2009 11:44:54 AM [Start] locate the pointer in the scale file using the correct scale
*=gfSeek('S'+STYLE.SCALE,'SCALE')
=gfSeek('S'+&lcTmpOrdLn..SCALE,'SCALE')
*B608894,1 TMI 06/14/2009 11:44:59 AM [End  ]

llPickOne = gfGetMemVar('M_PICKONE')  && Check if Always Pick from on bin location Yes or No
lcBinloc  = gfTempName()
lcSortBin = gfTempName()

lcStyle    = IIF(SEEK(ORDLINE&lcS..STYLE,'STYDYE'),StyDye.STYLE,ORDLINE&lcS..STYLE)   && sus p : if ORDLINE is correct located
lcFlatHang = IIF(SEEK(ORDLINE&lcS..STYLE,'STYLE'),STYLE.cFlatHang,'F')

lcWareCode= &lcTmpOrdLn..CWARECODE

SELECT WHBINLOC
SET RELATION TO
SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE

WAIT WINDOW NOWAIT 'Allocating :'+lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LINENO,6)+lcStyle
*--if use Bulk/Pick Set to Yes.
IF gfGetMemVar('M_BULKPICK')

  IF gfSEEK(lcStyle + lcWareCode ,'WHBINLOC')
    && note
    * this seek can be used in the function "lfUpdPkBin", put it in a variable and use this variable as follows
    * llFound
    * if llFound is .f.  , add a line , update it, set llFound to .t.
    * if llFound is .t., use the SEEK in the downloaded temp cursor and update it
    IF gfSEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LINENO,6)+lcStyle,'PKBINLOC')

      *--Check if user Amend in the Piktkt By Decreased the picked qty
      IF !lfChkPkDcr()
        SELECT(lnOldAlias)
        RETURN
      ENDIF

    ENDIF

    SELECT WHBINLOC
    lcAsecend = IIF(ORDHDR.cBlkPck='P','DESCENDING','')
    *--IF we're in sales order screen and this a new order, so until this point Filed CBLKPCK still not Saved yet so I'll
    *--Depend on laUSrfields that hold the field name and value .
    IF SUBSTR(loFormSet.NAME,4)='SOORD' AND loFormSet.ActiveMode = 'A'
      FOR J = 1 TO ALEN(loFormSet.laUserFields,1)
        IF ALLTRIM(loFormSet.laUserFields[J,1]) = "CBLKPCK"
          lcAsecend = IIF('P' $ loFormSet.laUserFields[J,6],'DESCENDING','')
        ENDIF
      ENDFOR
    ENDIF

    SELECT Whbinloc.STYLE, Whbinloc.cWarecode, Whbinloc.cLocation,;
      (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
      (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
      (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
      (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
      (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
      FROM  Whbinloc, Whsloc;
      WHERE Whsloc.cLocation = Whbinloc.cLocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.STYLE=SPACE(19) ;
      INTO TABLE (oAriaApplication.WorkDir+lcBinLoc)

    SELECT &lcBinLoc

    FOR lnCnt = 1 TO SCALE.CNT
      lcCnt = STR(lnCnt,1)

      IF (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]) > 0

        IF llPickOne         && Always Pick From one Bin Location / Yes.
          *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field and Qty. for the current Size.
          SELECT STYLE,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
            FROM  (oAriaApplication.WorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend, qty&lcCnt DESCENDING INTO CURSOR &lcSortBin
        ELSE                && Always Pick From one Bin Location / No.
          *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field Only for the current Size.
          SELECT STYLE,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
            FROM  (oAriaApplication.WorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend INTO CURSOR &lcSortBin
        ENDIF

        lnRemain = &lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]
        SCAN FOR &lcSortBin..QTY&lcCnt > 0
          SELECT WHBINLOC
          IF &lcSortBin..QTY&lcCnt >= lnRemain

            IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
              lcReplace = 'Alo&lcCnt WITH '+STR( MAX(WHBINLOC.Alo&lcCnt+lnRemain,0) )
              =gfReplace(lcReplace)

              lcReplace = 'TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8'
              =gfReplace(lcReplace)

              =lfUpdPkBin(lnRemain,lcCnt,lcPikTkT,OrdHdr.ORDER,&lcTmpOrdLn..LINENO)
              EXIT
            ENDIF

          ELSE

            IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
              lcReplace = 'Alo&lcCnt WITH '+STR(WHBINLOC.Alo&lcCnt + &lcSortBin..Qty&lcCnt)
              =gfReplace(lcReplace)

              lcReplace = 'TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8'
              =gfReplace(lcReplace)

              lnRemain = lnRemain - &lcSortBin..Qty&lcCnt
              *--Updates Record in the PkBinLoc File
              =lfUpdPkBin(&lcSortBin..Qty&lcCnt,lcCnt,lcPikTkT,OrdHdr.ORDER,&lcTmpOrdLn..LINENO)
            ENDIF
          ENDIF
        ENDSCAN

      ENDIF
      lnPicked = 0
      lnRemain = 0
    ENDFOR
  ENDIF

ELSE  &&*--if use Bulk/Pick Set to No.

  IF gfSEEK(lcStyle + lcWareCode,'WHBINLOC')

    IF gfSEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LINENO,6)+lcStyle,'PKBINLOC')

      *--Check if user Amend in the Piktkt By Decreased the picked qty
      IF !lfChkPkDcr()
        SELECT(lnOldAlias)
        RETURN
      ENDIF

    ENDIF

    SELECT WHBINLOC.STYLE, WHBINLOC.CWARECODE, WHBINLOC.CLOCATION,;
      (WHBINLOC.QTY1-WHBINLOC.ALO1) AS QTY1,(WHBINLOC.QTY2-WHBINLOC.ALO2) AS QTY2  ,;
      (WHBINLOC.QTY3-WHBINLOC.ALO3) AS QTY3,(WHBINLOC.QTY4-WHBINLOC.ALO4) AS QTY4  ,;
      (WHBINLOC.QTY5-WHBINLOC.ALO5) AS QTY5,(WHBINLOC.QTY6-WHBINLOC.ALO6) AS QTY6  ,;
      (WHBINLOC.QTY7-WHBINLOC.ALO7) AS QTY7,(WHBINLOC.QTY8-WHBINLOC.ALO8) AS QTY8  ,;
      (WHBINLOC.TOTQtY-WHBINLOC.TOTALO) AS TOTQTY,WHSLOC.CBINCLASS,WHSLOC.CFLATHANG ;
      FROM  WHBINLOC, WHSLOC;
      WHERE WHSLOC.CLOCATION = WHBINLOC.CLOCATION AND WHSLOC.CWARECODE=WHBINLOC.CWARECODE AND WHSLOC.STYLE=SPACE(19) ;
      AND WHSLOC.CFLATHANG = lcFlatHang ;
      INTO  TABLE (oAriaApplication.WorkDir+lcBinLoc)

    FOR I = 1 TO SCALE.CNT
      lcI = STR(I,1)
      lcPrmClss = SUBSTR(STYLE.CPRIMCLSS,I,1)
      lcSecClss = SUBSTR(STYLE.CSECCLSS ,I,1)
      lcRemClss = SUBSTR(STYLE.CREMCLSS ,I,1)
      IF !EMPTY(&lcTmpOrdLn..Pik&LCI) AND (&lcTmpOrdLn..Pik&LCI - laPikQty[I]) > 0
        IF llPickOne              && Always Pick From one Bin Location / Yes.
          *-- Select statment from (lcBinLoc) file to sort it by Bin Location Qty. for the current Size.
          SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG;
            FROM  (oAriaApplication.WorkDir+lcBinLoc) ;
            WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0 ;
            ORDER BY Qty&LCI DESCENDING;
            INTO  CURSOR &lcSortBin
        ELSE   && Always Pick From one Bin Location / No.
          *-- Select statment from (lcBinLoc) file to sort it by Bin Class and Bin Location Alph. for the current Size.
          *-- Here I Create a new field called CBINTYPE, I fill it with '1' if CBINCLASS = lcPrmClss and So on
          SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG,;
            IIF(CBINCLASS=lcPrmClss,'1',IIF(CBINCLASS=lcSecClss,'2','3')) AS CBINTYPE ;
            FROM  (oAriaApplication.WorkDir+lcBinLoc) ;
            WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0;
            ORDER BY CBINTYPE,CLOCATION ;
            INTO  CURSOR &lcSortBin
        ENDIF

        IF RECCOUNT(lcSortBin)>0
          SELECT(lcSortBin)
          lnRemain = &lcTmpOrdLn..Pik&LCI - laPikQty[I]
          SCAN
            IF &lcSortBin..QTY&LCI >= lnRemain
              IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                SELECT WHBINLOC
                lcReplace = 'Alo&lcI WITH '+STR( MAX(WHBINLOC.Alo&lcI+lnRemain,0) )
                =gfReplace(lcReplace)

                lcReplace = 'TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8'
                =gfReplace(lcReplace)

                =lfUpdPkBin(lnRemain,LCI,lcPikTkt,&lcTmpOrdLn..ORDER,&lcTmpOrdLn..LINENO,lcFlatHang,&lcSortBin..cBinClass)
                EXIT
              ENDIF
            ELSE
              IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                SELECT WHBINLOC
                lcReplace = 'Alo&lcI WITH '+STR(WHBINLOC.Alo&lcI + &lcSortBin..Qty&lcI)
                =gfReplace(lcReplace)

                lcReplace = 'TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8'
                =gfReplace(lcReplace)

                lnRemain = lnRemain - &lcSortBin..Qty&LCI
                =lfUpdPkBin(&lcSortBin..Qty&LCI,LCI,lcPikTkt,&lcTmpOrdLn..ORDER,&lcTmpOrdLn..LINENO,lcFlatHang,&lcSortBin..cBinClass)
              ENDIF
            ENDIF
          ENDSCAN

        ENDIF
      ENDIF
    ENDFOR
  ENDIF

ENDIF

NOTE this needs to be changed to two lines only of calling gfTableUpdate()
PRIVATE lcTranCode
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF

llUpdated = .T.
llUpdated = llUpdated AND gfTableUpdate(lcTranCode,'PKBINLOC')
llUpdated = llUpdated AND gfTableUpdate(lcTranCode,'WHBINLOC')

IF llUpdated
  =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
  *as this function does not show warning messages as the program is good ,tmi
  *-- Check if the allocation Qtys. is Equal to the ordline Picked Qty. or not
  =lfChkPktk((lcTmpOrdLn),lcPikTkT,lcStyle,&lcTmpOrdLn..LINENO)
ELSE
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  lcMsg = 'Updable to update allocation data'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
ENDIF

=lfEraseFil(lcBinLoc)

SELECT(lnOldAlias)
*-- end of lfSVBNALOC.


*loFormSet.
*:**************************************************************************
*:* Name        : lfChkPkDcr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Check if user Amend in the Piktkt By Decreased the picked qty
*:***************************************************************************
*:* Called from : lfSvBnAloc
*:***************************************************************************
*:* Note : This function do the same functionality as lfALRELORD, I done it
*          due to some complexity of the code
*:***************************************************************************
FUNCTION lfChkPkDcr
*T20071102.0018,10/C200876 TMI 05/28/2008 [Start] not to make "laPikQty" local , it is used in the calling function
*LOCAL lnSlct,laPikQty,llContinue
LOCAL lnSlct,llContinue
*T20071102.0018,10/C200876 TMI 05/28/2008 [End  ]

lnSlct = SELECT(0)
llContinue = .T.

DIMENSION laPikQty[9]
STORE 0 TO laPikQty

SELECT PKBINLOC
SUM REST QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laPikQty ;
  WHILE PIKTKT + CWARECODE + STR(LINENO,6) + STYLE + CLOCATION = ;
  lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LINENO,6)+lcStyle

*T20071102.0018,10/C200876 TMI 05/29/2008 [Start]
*IF (&lcTmpOrdLn..TotPik - laPikQty[9]) < 0
*B610685,1 TMI 03/11/2014 21:09 [Start] Modify function lfChkPkDcr to use pik field per size not on total value only (in the 1st IF statement and the last IF that in the ELSE of the 1st IF)
*IF (&lcTmpOrdLn..TotPik - laPikQty[9]) < 0 .OR. IIF(SUBSTR(loFormSet.NAME,4) = 'ALORDAL',ORDLINE.PIKTKT='******',.F.)
IF ((&lcTmpOrdLn..Pik1 - laPikQty[1]) < 0) OR ;
   ((&lcTmpOrdLn..Pik2 - laPikQty[2]) < 0) OR ;
   ((&lcTmpOrdLn..Pik3 - laPikQty[3]) < 0) OR ;
   ((&lcTmpOrdLn..Pik4 - laPikQty[4]) < 0) OR ;   
   ((&lcTmpOrdLn..Pik5 - laPikQty[5]) < 0) OR ;
   ((&lcTmpOrdLn..Pik6 - laPikQty[6]) < 0) OR ;
   ((&lcTmpOrdLn..Pik7 - laPikQty[7]) < 0) OR ;
   ((&lcTmpOrdLn..Pik8 - laPikQty[8]) < 0) OR ;
   IIF(SUBSTR(loFormSet.NAME,4) = 'ALORDAL',ORDLINE.PIKTKT='******',.F.)
  *B610685,1 TMI 03/11/2014 21:11 [End  ] 
  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]
  SELECT PKBINLOC
  IF SEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LINENO,6)+lcStyle,'PKBINLOC')
    SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
        lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LINENO,6)+lcStyle
      IF SEEK(PKBINLOC.STYLE+PKBINLOC.CWARECODE+PKBINLOC.CLOCATION ,'WHBINLOC')
        SELECT WHBINLOC
        lcReplace = 'ALO1       WITH '+STR( MAX(WHBINLOC.ALO1-PKBINLOC.Qty1,0) ) +' '+ ;
          'ALO2       WITH '+STR( MAX(WHBINLOC.ALO2-PKBINLOC.Qty2,0) ) +' '+ ;
          'ALO3       WITH '+STR( MAX(WHBINLOC.ALO3-PKBINLOC.Qty3,0) ) +' '+ ;
          'ALO4       WITH '+STR( MAX(WHBINLOC.ALO4-PKBINLOC.Qty4,0) ) +' '+ ;
          'ALO5       WITH '+STR( MAX(WHBINLOC.ALO5-PKBINLOC.Qty5,0) ) +' '+ ;
          'ALO6       WITH '+STR( MAX(WHBINLOC.ALO6-PKBINLOC.Qty6,0) ) +' '+ ;
          'ALO7       WITH '+STR( MAX(WHBINLOC.ALO7-PKBINLOC.Qty7,0) ) +' '+ ;
          'ALO8       WITH '+STR( MAX(WHBINLOC.ALO8-PKBINLOC.Qty8,0) )
        =gfReplace(lcReplace)

        lcReplace = 'TotAlo     WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 '
        =gfReplace(lcReplace)

        *T20071102.0018,10/C200876 TMI 07/02/2008 [Start] update the laPickQty
        laPikQty[1] = laPikQty[1] - PKBINLOC.Qty1
        laPikQty[2] = laPikQty[2] - PKBINLOC.Qty2
        laPikQty[3] = laPikQty[3] - PKBINLOC.Qty3
        laPikQty[4] = laPikQty[4] - PKBINLOC.Qty4
        laPikQty[5] = laPikQty[5] - PKBINLOC.Qty5
        laPikQty[6] = laPikQty[6] - PKBINLOC.Qty6
        laPikQty[7] = laPikQty[7] - PKBINLOC.Qty7
        laPikQty[8] = laPikQty[8] - PKBINLOC.Qty8
        *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

      ENDIF

      SELECT PKBINLOC
      =gfDelete()

    ENDSCAN

    *T20071102.0018,10/C200876 TMI 07/02/2008 [Start] update the laPickQty array
    laPikQty[9] = laPikQty[1]+laPikQty[2]+laPikQty[3]+laPikQty[4]+laPikQty[5]+laPikQty[6]+laPikQty[7]+laPikQty[8]
    *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

    *- Update
    SELECT WHBINLOC
    = gfTableUpdate()
    SELECT PKBINLOC
    =gfTableUpdate()

    *- Re download the WHBINLOC & PKBINLOC data
    =gfSEEK(lcStyle + lcWareCode ,'WHBINLOC')
    =gfSEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LINENO,6)+lcStyle,'PKBINLOC')

    *T20071102.0018,10/C200876 TMI 05/29/2008 [Start]
    IF SUBSTR(loFormSet.NAME,4) = 'ALORDAL' AND ORDLINE.PIKTKT='******'
      llContinue = .F.
    ENDIF
    *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]

  ENDIF

ELSE

  *- If nothing changed then do not continue
  *B610685,1 TMI 03/11/2014 21:11 [Start] use comparison for each pik field
  *IF  &lcTmpOrdLn..TotPik = laPikQty[9]
  IF (&lcTmpOrdLn..Pik1 = laPikQty[1]) AND ;
     (&lcTmpOrdLn..Pik2 = laPikQty[2]) AND ;
     (&lcTmpOrdLn..Pik3 = laPikQty[3]) AND ;
     (&lcTmpOrdLn..Pik4 = laPikQty[4]) AND ;   
     (&lcTmpOrdLn..Pik5 = laPikQty[5]) AND ;
     (&lcTmpOrdLn..Pik6 = laPikQty[6]) AND ;
     (&lcTmpOrdLn..Pik7 = laPikQty[7]) AND ;
     (&lcTmpOrdLn..Pik8 = laPikQty[8]) 
    *B610685,1 TMI 03/11/2014 21:12 [End  ] 
    llContinue = .F.
  ENDIF

ENDIF

SELECT (lnSlct)
RETURN llContinue
*-- end of lfChkPkDcr.

*!***************************************************************************
*!* Name        : lfAlRelOrd
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Release piktkt in Order Allocation screen and Style Allocation Screen
*!***************************************************************************
*!* Called from : ALORDAL.PRG , ALSTYAL.PRG , ALRELPI.PRG , ALPKTKT
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfALRELORD()
*!***************************************************************************
FUNCTION lfALRELORD

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnSlct
lnSlct = SELECT(0)

PRIVATE lcFile
DO CASE
CASE SUBSTR(loFormSet.NAME,4) = 'ALAUTAL'
  lcFile = 'ORDLINE'

CASE SUBSTR(loFormSet.NAME,4) $  'ALORDAL|ALPKTKT|ALRELPI'
  lcFile = 'ORDLINE'

CASE SUBSTR(loFormSet.NAME,4) = 'ALSTYAL'
  lcFile = loFormSet.lcTmpOrdline

ENDCASE

*-- Checking For Use bin location Yes/No Or lcPikTkt = "******" and Open the Needed Files.
=lfOpnFiles("WHBINLOC,PKBINLOC","WHBINLST,PKLINE",&lcFile..PikTkt)

SELECT PKBINLOC
IF gfSEEK(&lcFile..PikTkt+&lcFile..cWareCode+STR(&lcFile..LINENO,6)+&lcFile..STYLE,'PKBINLOC')

  =gfSEEK(&lcFile..STYLE+&lcFile..cWareCode,'WHBINLOC')

  SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
      &lcFile..PikTkt+&lcFile..cWareCode+STR(&lcFile..LINENO,6)+&lcFile..STYLE
    IF SEEK(STYLE+ cWareCode+clocation ,'WHBINLOC')
      SELECT WHBINLOC
      lcReplace = 'ALO1   WITH '+ STR( MAX(ALO1 - PKBINLOC.Qty1 , 0 ) )+' '+;
        'ALO2   WITH '+ STR( MAX(ALO2 - PKBINLOC.Qty2 , 0 ) )+' '+;
        'ALO3   WITH '+ STR( MAX(ALO3 - PKBINLOC.Qty3 , 0 ) )+' '+;
        'ALO4   WITH '+ STR( MAX(ALO4 - PKBINLOC.Qty4 , 0 ) )+' '+;
        'ALO5   WITH '+ STR( MAX(ALO5 - PKBINLOC.Qty5 , 0 ) )+' '+;
        'ALO6   WITH '+ STR( MAX(ALO6 - PKBINLOC.Qty6 , 0 ) )+' '+;
        'ALO7   WITH '+ STR( MAX(ALO7 - PKBINLOC.Qty7 , 0 ) )+' '+;
        'ALO8   WITH '+ STR( MAX(ALO8 - PKBINLOC.Qty8 , 0 ) )
      =gfReplace(lcReplace)

      lcReplace = 'TotALO WITH '+ STR( ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8 )
      =gfReplace(lcReplace)
    ENDIF

    SELECT PKBINLOC
    =gfDelete()

  ENDSCAN

  *T20071102.0018,10/C200876 TMI 05/29/2008 [Start] Save the variables to a temp file as the gfTableUpdate changes the
  **                                                memory variables values
  LOCAL lcTmpFl
  lcTmpFl = gfTempName()
  SAVE TO (oAriaApplication.WorkDir+lcTmpFl+'.MEM')
  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]

  *- Update
  SELECT WHBINLOC
  =gfTableUpdate()
  SELECT PKBINLOC
  =gfTableUpdate()

  *T20071102.0018,10/C200876 TMI 05/29/2008 [Start] restore the saved values to the temp files
  RESTORE FROM (oAriaApplication.WorkDir+lcTmpFl+'.MEM') ADDITIVE
  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]

ENDIF

SELECT (lnSlct)

*-- End of Function lfALRELORD.

*!***************************************************************************
*!* Name        : lfALMODAUT
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Modify Picked Qty in Automatic Allocation
*!***************************************************************************
*!* Called from : ALAUTAL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfALMODAUT()
*!***************************************************************************
FUNCTION lfALMODAUT

IF !lfIsUseBin()
  RETURN
ENDIF

lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order

lcSize = ALLT(STR(lnParm,2))

=lfOpnFiles("WHBINLOC,PKBINLOC","WHBINLOC,PKBINPKT","")

DO CASE
CASE SUBSTR(loFormSet.NAME,4) = 'ALAUTAL'
  lcTmpOrdLn = loFormSet.lcTmpOrdLn

  IF !USED(lcTmpOrdLn)
    lcTmpOrdLn = 'ORDLINE'
  ENDIF
ENDCASE

IF gfSEEK(&lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode,'PKBINLOC')

  SELECT PKBINLOC
  SCAN REST WHILE PikTkt+cWareCode = &lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode ;
      FOR STYLE = &lcTmpOrdLn..STYLE

    IF gfSEEK(PKBINLOC.cWareCode+PKBINLOC.cLocation+PKBINLOC.STYLE ,'WHBINLOC')
      lcReplace ='Alo&lcSize WITH '+STR( WHBINLOC.Alo&lcSize + lnPik - lcOldVal ) + ;
        'TotAlo     WITH '+STR( WHBINLOC.TotAlo     + lnPik - lcOldVal )
      SELECT WHBINLOC
      =gfReplace(lcReplace)

      * Note : add the code to check if the Location has 0 qty then delete it based on an option
    ENDIF

    SELECT PKBINLOC
    lcReplace = 'Qty&lcSize WITH '+STR(Qty&lcSize + lnPik - lcOldVal) + ' ' +;
      'TotQty     WITH '+STR(TotQty     + lnPik - lcOldVal)
    =gfReplace(lcReplace)
    IF TotQty = 0
      gfDelete()
    ENDIF
  ENDSCAN

  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'Ok to update the picked qty?') = 1
    *- Update
    SELECT WHBINLOC
    =gfTableUpdate()
    SELECT PKBINLOC
    =gfTableUpdate()
    =lfChkPktk((lcTmpOrdLn),lcPikTkT,&lcTmpOrdLn..STYLE,&lcTmpOrdLn..LINENO)
  ELSE
    *- Release the updated temp cursors
    =gfSeek(CHR(255),'WHBINLOC')
    =gfSeek(CHR(255),'PKBINLOC')
  ENDIF
  *T20071102.0018,7 TMI [End  ]
ENDIF

SELECT(lnAlias)

*-- End of Function lfALMODAUT.

*:**************************************************************************
*:* Name        : lfPkAloQty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : Deny Force Allocation from within the ALPKTKT screen
*:***************************************************************************
*:* Called from : ALPKTKT.PRG lfAloQty Fn.
*:***************************************************************************
FUNCTION lfPkAloQty

LOCAL llContinue
llContinue = .T.

LOCAL loO,lc_tmpOrdl,lcI,lcKey,lnSlct
lnSlct = SELECT(0)

loO = loPkTktScr.AriaForm1.pgfPikTkt.pgDetail.cntalloc.txtQty&lcparm
lc_tmpOrdl = loPkTktScr.lc_tmpOrdl
lcI = lcparm
IF EMPTY(loO.VALUE)
  RETURN
ENDIF

IF !lfIsUseBin()
  RETURN
ENDIF

SELECT ORDLINE
lcKey = EVALUATE(KEY())
=SEEK(&lc_tmpOrdl..CORDTYPE+&lc_tmpOrdl..ORDER+STR(&lc_tmpOrdl..LINENO,6),'ORDLINE')
IF STYDYE.Stk&lcI - STYDYE.Alo&lcI - (loO.VALUE - ORDLINE.PIK&lcI) < 0
  loO.VALUE = loO.OldValue
  loO.REFRESH
  llContinue = .F.
ENDIF

=SEEK(lcKey,'ORDLINE')

SELECT (lnSlct)
RETURN llContinue
*-- end of lfPkAloQty.

*!***************************************************************************
*!* Name        : lfUpdPkBin
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Update the PKBinLoc file with the Picked Qty.
*!***************************************************************************
*!* Called from : BN4MAIN.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdPkBin()
*!***************************************************************************
*T20071102.0018,7 TMI [Start]
FUNCTION lfUpdPkBin
PARAMETER lnQty,lcSize,lcPkTkt,lcOrder,lnLineNo,lcFltHng,lcClss

PRIVATE lcOldOrd
lnAlias    = SELECT()
IF lnQty = 0
  RETURN
ENDIF
IF EMPTY(lcFltHng)
  lcFltHng = STYLE.cFlatHang
ENDIF
IF EMPTY(lcClss)
  lnSz = INT(VAL(lcSize))
  lcClss = IIF(!EMPTY(SUBSTR(STYLE.cPrimClss,lnSz,1)),SUBSTR(STYLE.cPrimClss,lnSz,1),;
    IIF(!EMPTY(SUBSTR(STYLE.cSecClss,lnSz,1)),SUBSTR(STYLE.cSecClss,lnSz,1),;
    SUBSTR(STYLE.cRemClss,lnSz,1)))
ENDIF

IF !SEEK(lcPkTkt+lcWareCode+STR(lnLineNo,6) + lcStyle + WHBINLOC.cLocation ,'PKBINLOC')
  SELECT PKBINLOC
  SCATTER MEMVAR BLANK  && zeroing first all field variables

  m.STYLE      = WHBINLOC.STYLE
  m.CWARECODE  = WHBINLOC.cWareCode
  m.PIKTKT     = lcPkTkt
  m.ORDER      = lcOrder
  m.cFlatHang  = lcFltHng
  m.clocation  = WHBINLOC.cLocation
  m.Qty&lcSize = MAX(lnQty,0)
  m.TotQty     = MAX(lnQty,0)
  m.cBinClass&lcSize = lcClss
  m.LINENO     = lnLineNo

  m.cAdd_user  = oAriaApplication.User_ID
  m.dAdd_Date  = DATE()
  m.cAdd_time  = gfGetTime()

  =gfAppend('PKBINLOC',.T.)

ELSE

  SELECT PKBINLOC
  lcReplace = 'Qty&lcSize WITH '+STR(Qty&lcSize + lnQty ) + ' ' + ;
    'TotQty     WITH '+STR(TotQty     + lnQty ) + ' ' + ;
    'cBinClass&lcSize WITH "'+lcClss + '"'
  =gfReplace(lcReplace)

ENDIF

SELECT(lnAlias)
*--End of Function lfUpdPkBin.

*!***************************************************************************
*!* Name        : lfChkPktk
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Check if the Picked Qtys. that updated in the Pkbinloc and
*!*             : Whbinloc are equal to the OrdLine Picked Qtys.
*!***************************************************************************
*!* Called from : BN4MAIN.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChkPktk()
*!***************************************************************************
FUNCTION lfChkPktk
PARAMETER lcTempFile,lcPktkt,lcSty,lnLine

LOCAL lnSlct
lnSlct = SELECT(0)

IF EMPTY(lcPktkt)
  RETURN
ENDIF

PRIVATE lcOldOrd
DIMENSION laPkQty[9]
STORE 0 TO laPkQty


SELECT PKBINLOC
lcOldOrd = ORDER()
=gfSetOrder('PKLINE')
IF !gfSEEK(lcPktkt+&lcTempFile..cWareCode ,'PKBINLOC')
  lcMsg2 = "No line updated to Bin Location Files Please Contact Aria Support"
  && note : this message needs to refer to an information so the user knows where is the error   note04/09/2008
  &&        it needs to be enhanced to show a detailed log file not a general message

  *C201070 TMI 26/02/2009 [Start] comment this line
  *=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  *C201070 TMI 26/02/2009 [End  ]
ELSE
  SELECT PKBINLOC
  =SEEK(lcPktkt+&lcTempFile..cWareCode+STR(lnLine,6)+lcSty,'PKBINLOC')
  SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPktkt+&lcTempFile..cWareCode+STR(lnLine,6)+lcSty
    laPkQty[1] = laPkQty[1]+ PKBINLOC.QTY1
    laPkQty[2] = laPkQty[2]+ PKBINLOC.QTY2
    laPkQty[3] = laPkQty[3]+ PKBINLOC.QTY3
    laPkQty[4] = laPkQty[4]+ PKBINLOC.QTY4
    laPkQty[5] = laPkQty[5]+ PKBINLOC.QTY5
    laPkQty[6] = laPkQty[6]+ PKBINLOC.QTY6
    laPkQty[7] = laPkQty[7]+ PKBINLOC.QTY7
    laPkQty[8] = laPkQty[8]+ PKBINLOC.QTY8
  ENDSCAN
  laPkQty[9]= laPkQty[1]+laPkQty[2]+laPkQty[3]+laPkQty[4]+laPkQty[5]+laPkQty[6]+laPkQty[7]+laPkQty[8]

  LOCAL lnTotPik
  SELECT &lcTempFile
  *- I used this sum to overcome an error in the standard when changing the qty in the allocation
  lnTotPik = Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8

  IF  lnTotPik <> laPkQty[9]
    lcMsg2 = "Picked Qty " + ALLTRIM(STR(lnTotPik)) +  " Not equal bin Picked Qty " + ALLTRIM(STR(laPkQty[9])) +;
      " for Style " + &lcTempFile..STYLE + " Piktkt " + lcPktkt + " Please contact aria support "
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  ENDIF
ENDIF

SELECT PKBINLOC
=gfSetOrder(lcOldOrd)

SELECT (lnSlct)
*-- End of Function lfChkPktk.

*!***************************************************************************
*!* Name        : lfChkFirst
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Check if the stock in Bin files and Standard aria files match
*!***************************************************************************
*!* Called from : ALORDAL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChkFirst()
*!***************************************************************************
FUNCTION lfChkFirst
*B608850,1 TMI 04/19/2009 [Start] send "llFrmMain" parameter from lfBinAlloc to use this function
PARAMETERS llFrmMain
*B608850,1 TMI 04/19/2009 [End  ]
IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lcTempRec,lnOldAlias,llCanSave,lcTmpSty,lcTmpWare,lcScanExp,lcOrdSty,lcOrdOrder,lcTempPik,lcLogFile,lnLogRecNo,lcTmpOrder
DIMENSION laPiked[8],laToPik[9]
STORE 0 TO lcTempRec,lnOldAlias,laPiked,laToPik,lnLogRecNo
STORE '' TO lcTmpSty,lcTmpWare,lcScanExp,lcOrdSty,lcOrdOrder,lcTempPik,lcLogFile,lcTmpOrder
STORE .T. TO llCanSave
lcLogFile  = gfTempName()
lnOldAlias = SELECT(0)
*-- Create Temp file that will hold any error occurs while importing.
CREATE TABLE (oAriaApplication.WorkDir+lcLogFile) (nRecNo N(5) , cError C(200),nFirst N(1))
INDEX ON ALLTRIM(STR(nFirst))+PADL(nRecNo,FSIZE('nRecNo')) TAG LogRecno
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'Problems that prevent allocation',0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)

*-- Checking For Use bin location Yes/No Or lcPikTkt = "******" and Open the Needed Files.
=lfOpnFiles("WHBINLOC,PKBINLOC,WHSLOC","WHBINLST,PKLINE,WHSLOC",'')

lcCurFile = ''

DO CASE
CASE SUBSTR(loFormSet.NAME,4) = 'ALAUTAL'
  lcScanExp = "FOR lnSel=1 .AND. TotPik > 0 .AND. nProcNo < 4"
  lcCurFile = loFormSet.lcTmpOrdLn

CASE SUBSTR(loFormSet.NAME,4) = 'ALORDAL'
  *T20071102.0018,10/C200876 TMI 10/05/2008 [Start] Add a FOR filter when called from allocation by order
  *lcScanExp = ''
  *B608850,1 TMI 04/19/2009 [Start] if called from lfBinAllo, then no filter expression is needed, "set key to" is used
  IF EMPTY(llFrmMain)
    *B608850,1 TMI 04/19/2009 [End  ]

    lcScanExp = "FOR nSel=1"

    *B608850,1 TMI 04/19/2009 [Start]
  ENDIF
  *B608850,1 TMI 04/19/2009 [End  ]

  *T20071102.0018,10/C200876 TMI 10/05/2008 [End  ]

  lcCurFile = loFormSet.lc_TmpOrdL


CASE SUBSTR(loFormSet.NAME,4) = 'ALSTYAL'
  lcCurFile = 'ORDLINE'
  lcOrdSty  = &lcCurFile..STYLE
  lcOrdOrder= &lcCurFile..ORDER
  lcScanExp = 'REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =lcOrdSty FOR ORDER=lcOrdOrder'

  *T20071102.0018,10/C200876 TMI 10/05/2008 [Start] comment these lines
  *!*	  *T20071102.0018,08 TMI [Start]
  *!*	CASE SUBSTR(loFormSet.Name,4) = 'SOORD'

  *!*	  *T20071102.0018,9 TMI 04/30/2008 [Start] for the moment do not run this part for SOORD , this is a separate issue
  *!*	  return
  *!*	  *T20071102.0018,9 TMI 04/30/2008 [End  ]

  *!*	  lcCurFile = loFormSet.oFormEnvironment.lcOrdLine
  *!*	  lcOrdSty  = &lcCurFile..Style
  *!*	  lcOrdOrder= &lcCurFile..Order
  *!*	  lcScanExp = ''

  *!*	  IF gfModalGen('QRM00000B32000','DIALOG',.F.,.F.,'Would you like to allocate the order?') = 2
  *!*	    RETURN
  *!*	  ENDIF
  *T20071102.0018,10/C200876 TMI 10/05/2008 [End  ]

ENDCASE

SELECT (lcCurFile)
lcTempRec = RECNO()

LOCATE
*!B610341,1 SAB 11/17/2011 Fix Automatic Allocation screen hang issue [T20130111.0006][Start]
*SCAN &lcScanExp
*  *T20071102.0018,10/C200876 TMI 10/05/2008 [Start] comment these lines as this condition is added above within the case statement
*  *!*	  *T20071102.0018,10/C200876 TMI 09/23/2008 [Start] skip non selected lines
*  *!*	  IF NSEL<>1
*  *!*	    LOOP
*  *!*	  ENDIF
*  *!*	  *T20071102.0018,10/C200876 TMI 09/23/2008 [End  ]
*  lcTmpSty  = &lcCurFile..STYLE
*  lcTmpWare = IIF(TYPE('lcPikWare') = 'C',lcPikWare,&lcCurFile..cWareCode)
*  lcTempPik = &lcCurFile..PikTkt
*  lcTmpOrder= &lcCurFile..ORDER
*  IF gfSEEK(lcTmpSty+lcTmpWare,'STYDYE')
*    lcWhCurs = gfTempName()
*
*    lcSqlStatement = "SELECT style,cwarecode," +;
*      "SUM(Whbinloc.Qty1) AS QTY1,SUM(Whbinloc.Qty2) AS QTY2,SUM(Whbinloc.Qty3) AS QTY3,SUM(Whbinloc.Qty4) AS QTY4,"+;
*      "SUM(Whbinloc.Qty5) AS QTY5,SUM(Whbinloc.Qty6) AS QTY6,SUM(Whbinloc.Qty7) AS QTY7,SUM(Whbinloc.Qty8) AS QTY8 "+;
*      "FROM Whbinloc "+;
*      "WHERE Whbinloc.style = '"+lcTmpSty + "' AND Whbinloc.cwarecode = '" + lcTmpWare + "'"+;
*      "GROUP BY style,cwarecode"
*    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatement,lcWhCurs,"WHBINLOC",;
*      oAriaApplication.Activecompanyconstr,3,'SAVE',loFormSet.DATASESSIONID)
*    IF lnConnectionHandlar = 1
*      SELECT &lcWhCurs
*      *C201070 TMI 25/02/2009 [Start] sum style qty in whbinloc
*      *IF RECCOUNT(lcWhCurs)=0
*      *C201070 TMI 26/02/2009 [Start] comment out the IF part, as it is dealt with in the standard screen, i.e when there is a style with no stock then this line will not be allocated
*      *!*	      LOCAL laSum[8]
*      *!*	      SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 TO laSum
*      *!*	      IF lnSum = 0
*      *!*	        IF lcScanExp = "FOR nSel=1"  && do not allocate this line
*      *!*	          *C201070 TMI 26/02/2009 [Start] comment out
*      *!*	          *SELECT &lcCurFile
*      *!*	          *REPLACE &lcCurFile..nSel WITH 0
*      *!*	          *C201070 TMI 26/02/2009 [End  ]
*      *!*	        ENDIF
*      *!*	        *C201070 TMI 25/02/2009 [End  ]
*      *!*	        INSERT INTO (lcLogFile) (NRECNO,CERROR,nFirst) VALUES ;
*      *!*	              (lnLogRecNo+1,'Order : (' + lcTmpOrder +')'+' Style : (' +lcTmpSty +') has no stock in Bins' ,1)
*      *!*	      ELSE
*      *C201070 TMI 26/02/2009 [End  ]
*
*      *T20080804.0011   / TMI 09/25/2008 [Start] if there is inbalance with qty in bins are greater than those in
*      *                                          stydye then allow allocation in this case
*      *IF QTY1 <> STYDYE.STK1 OR ;
*      QTY2 <> STYDYE.STK2 OR ;
*      QTY3 <> STYDYE.STK3 OR ;
*      QTY4 <> STYDYE.STK4 OR ;
*      QTY5 <> STYDYE.STK5 OR ;
*      QTY6 <> STYDYE.STK6 OR ;
*      QTY7 <> STYDYE.STK7 OR ;
*      QTY8 <> STYDYE.STK8
*      IF QTY1 < STYDYE.STK1 OR ;
*          QTY2 < STYDYE.STK2 OR ;
*          QTY3 < STYDYE.STK3 OR ;
*          QTY4 < STYDYE.STK4 OR ;
*          QTY5 < STYDYE.STK5 OR ;
*          QTY6 < STYDYE.STK6 OR ;
*          QTY7 < STYDYE.STK7 OR ;
*          QTY8 < STYDYE.STK8
*        *T20080804.0011   / TMI 09/25/2008 [End  ]
*        INSERT INTO (lcLogFile) (NRECNO,CERROR,nFirst) VALUES ;
*          (lnLogRecNo+1,'Order : (' + lcTmpOrder +')'+ ' Style : (' +lcTmpSty +')  has a mismatch between ' + ;
*          'stock in warehouse and the accumulated stock in bins. Please run rebalance database for Inventory Control, then rebalance bin locations' ,1)
*        *C201070 TMI 26/02/2009 [Start] close the above
*        *!*	        ENDIF
*        *C201070 TMI 26/02/2009 [End  ]
*      ENDIF
*    ENDIF
*  ENDIF
*ENDSCAN
=gfOpenTable("WHBINLOC",'WHBINLST','SH',"WHBINLOC_A")
SELECT (lcCurFile)
LOCATE  
SCAN &lcScanExp
  WAIT WINDOW 'Checking the stock of Style:'+&lcCurFile..STYLE NOWAIT
  lcTmpSty  = &lcCurFile..STYLE
  lcTmpWare = IIF(TYPE('lcPikWare') = 'C',lcPikWare,&lcCurFile..cWareCode)
  lcTempPik = &lcCurFile..PikTkt
  lcTmpOrder= &lcCurFile..ORDER
  IF gfSEEK(lcTmpSty+lcTmpWare,'STYDYE')
    =gfSeek(lcTmpSty+lcTmpWare,"WHBINLOC_A",'WHBINLST')
    SELECT WHBINLOC_A
    SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 to lnQty1, lnQty2, lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8 REST WHILE STYLE+CWARECODE+CLOCATION=lcTmpSty+lcTmpWare 
    IF lnQTY1 < STYDYE.STK1 OR ;
       lnQTY2 < STYDYE.STK2 OR ;
       lnQTY3 < STYDYE.STK3 OR ;
       lnQTY4 < STYDYE.STK4 OR ;
       lnQTY5 < STYDYE.STK5 OR ;
       lnQTY6 < STYDYE.STK6 OR ;
       lnQTY7 < STYDYE.STK7 OR ;
       lnQTY8 < STYDYE.STK8
        INSERT INTO (lcLogFile) (NRECNO,CERROR,nFirst) VALUES ;
          (lnLogRecNo+1,'Order : (' + lcTmpOrder +')'+ ' Style : (' +lcTmpSty +')  has a mismatch between ' + ;
          'stock in warehouse and the accumulated stock in bins. Please run rebalance database for Inventory Control, then rebalance bin locations' ,1)
      ENDIF
  ENDIF
ENDSCAN
*!B610341,1 SAB 11/17/2011 Fix Automatic Allocation screen hang issue [T20130111.0006][End]

IF RECCOUNT(lcLogFile)>3
  llCanSave = .F.

  *C201070 TMI 26/02/2009 [Start] change the message to just reveals that there is a problem
  *IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'There are some problems so can not continue picking process, Do you want to view the log report?') = 1
  LOCAL lcMsg
  lcMsg = 'There are stock mismatch problem,'+CHR(13)+'View the log report?'
  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,lcMsg) = 1
    *C201070 TMI 26/02/2009 [End  ]
    lcLogTtl = "Log Report"
    lcLogMsg = ''
    SELECT (lcLogFile)
    GO TOP
    SCAN
      IF &lcLogFile..NRECNO <> 0
        lcLogMsg = lcLogMsg + CHR(10) + CHR(13)+ ALLTRIM(STR(&lcLogFile..nRecNo)) + ' - ' + &lcLogFile..cError
      ELSE
        lcLogMsg = lcLogMsg + CHR(10) + CHR(13)+ &lcLogFile..cError
      ENDIF
      SELECT (lcLogFile)
    ENDSCAN
    DO FORM (oAriaApplication.ScreenHome+'SMMSG.SCX') WITH loFormSet,lcLogMsg,.T.
  ENDIF
ENDIF

SELECT (lcCurFile)
TRY
  GOTO (lcTempRec)
CATCH
ENDTRY

SELECT(lnOldAlias)
RETURN llCanSave
*-- End of Function lfChkFirst.
*LOFORMSET
*!***************************************************************************
*!* Name        : LFCHKPACK
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Allocation (AL)
*!* Purpose     : Prevent user from change piktkt Qty. if it has a packing List.
*!***************************************************************************
*!* Called from : ALORDAL.PRG -
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFCHKPACK()
*!***************************************************************************
FUNCTION LFCHKPACK

* NOTE : THIS needs to be checked when called from ALORDAL.SCX screen
PRIVATE lnOldAlias , llusedPack , lcOldOrder

lnOldAlias = SELECT(0)
llusedPack = USED('PACK_HDR')
lcOldOrder = IIF(llusedPack,ORDER('PACK_HDR'),"")
=lfOpnFiles("PACK_HDR","PACK_HDR",'')

DO CASE
CASE SUBSTR(loFormSet.NAME,4) = 'ALAUTAL'
  lcTmpOrdLn = loFormSet.lcTmpOrdLn
  IF gfSEEK(EVAL(lcTmpOrdLn+'.PikTkt'),'PACK_HDR')
    =gfModalGen("INM44060B00000" , "DIALOG" , PADR(EVAL(lcTmpOrdLn+'.PikTkt'),6))
    loFormSet.AriaForm1.cntDetail.txtPik&lcParm..VALUE = lcOldVal
    RETURN .F.
  ENDIF

CASE SUBSTR(loFormSet.NAME,4) = 'ALORDAL'
  IF gfSEEK(ORDLINE.PikTkt,'PACK_HDR')
    =gfModalGen("INM44060B00000" , "DIALOG" , PADR(ORDLINE.PikTkt,6))
    &lcObjName = lcOldVal
    RETURN .F.
  ENDIF

ENDCASE

IF !llusedPack
  USE IN PACK_HDR
ELSE
  IF !EMPTY(lcOldOrder)
    SELECT PACK_HDR
    =gfSetOrder(lcOldOrder)
  ENDIF
ENDIF
*-- End of Function LFCHKPACK.

*!***************************************************************************
*!* Name        : lfARCHKORD
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Check if there is a piktkt for the order or not
*!***************************************************************************
*!* Called from : ARIINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARCHKORD()
*!***************************************************************************
FUNCTION lfARCHKORD

IF TYPE('XORDER') = 'C' .AND. EMPTY(XORDER)
  RETURN
ENDIF

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnSlct,llContinue,lcSvOrd
lnSlct = SELECT(0)

SELECT PIKTKT
lcSvOrd = ORDER()
=gfSetOrder('ORDPIK')   && ORDER+PIKTKT

llContinue = .F.
IF gfSEEK(ORDHDR.ORDER,'PIKTKT')
  SELECT PIKTKT
  LOCATE REST FOR ORDER+PIKTKT = ORDHDR.ORDER AND STATUS = 'O'
  llContinue = FOUND()
ENDIF

IF !llContinue
  lcMsg = 'There is no open pick ticket for order ' +ORDHDR.ORDER + ' , cannot ship . you have to allocate it first'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  IF TYPE('XORDER') = 'C'
    loFormSet.AriaForm1.keyOrder.Keytextbox.VALUE = ''
    XORDER = ''
  ENDIF
ENDIF

SELECT PIKTKT
=gfSetOrder(lcSvOrd)   && old order setting

SELECT (lnSlct)
RETURN llContinue
*-- End of Function lfARCHKORD.

*:**************************************************************************
*:* Name        : lfCHKLNALC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/28/2009
*:* Purpose     : check if the current line is allocated in ORDLINE
*:***************************************************************************
*:* Called from : ARIINV.SCX lfGetOrder
*:***************************************************************************
FUNCTION lfCHKLNALC

IF !lfIsUseBin()
  RETURN
ENDIF
LOCAL llRet
llRet = !EMPTY(ORDLINE.PIKTKT) AND ORDLINE.PIKTKT <> '******'  && the line is picked
RETURN llRet
*-- end of lfCHKLNALC.

*!***************************************************************************
*!* Name        : lfARDFNMNU
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Add a new entry to the Option Menu - "Bin Locations"
*!***************************************************************************
*!* Called from : ARDINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARDFNMNU()
*!***************************************************************************
FUNCTION lfARDFNMNU

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnBarNo
lnBarNo = CNTBAR('_INQURYPOP') + 1
*-- Define New options (Bin Locations),This Option Menu will be enable in the Edit mode.
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT "\<Bin Locations" SKIP FOR _SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'S'
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfARGETBIN IN BN4MAIN.FXP WITH _SCREEN.ACTIVEFORM.PARENT  
RETURN

*-- End of Function lfARDFNMNU.

*!***************************************************************************
*!* Name        : lfARGETBIN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Screen of get bins in case of AR.
*!***************************************************************************
*!* Called from : lfARDFNMNU in BN4MAIN.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARGETBIN()
*!***************************************************************************
FUNCTION lfARGETBIN
PARAMETERS loFormSet,lcKey

IF !lfIsUseBin()
  RETURN
ENDIF

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
*! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
*! Comment the code for the Entry B609714 in this fn.

*!*	*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
*!*	lnOld_DataSession_ID = SET("DATASESSION")
*!*	SET DATASESSION TO loFormSet.DATASESSIONID
*!*	IF USED("syustatc")
*!*	  SELECT syustatc
*!*	  SET ORDER TO CUSER_ID
*!*	ELSE
*!*	  SELECT 0
*!*	  lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
*!*	  IF oAriaApplication.multiinst
*!*	    lcSQLDICPATH =  oAriaApplication.ClientA4Path+'SQLDictionary\'
*!*	  ENDIF
*!*	  USE (lcSQLDICPATH +"syustatc") ORDER CUSER_ID
*!*	  = CURSORSETPROP('Buffering', 5, 'SYUSTATC' )  && Enable
*!*	ENDIF
*!*	*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]


PRIVATE lnAlias,lcOldBinlc
LOCAL lnDataSess,lnAlias
lnAlias = SELECT()

IF !USED(loFormSet.lcInvLine)
  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
  *! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
  *! Comment the code for the Entry B609714 in this fn.

  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
  *!*	  SET DATASESSION TO lnOld_DataSession_ID
  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]
  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]


  RETURN
ENDIF

STORE "" TO lcOldBinlc
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLST,WHSLOC","")

  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
  *! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
  *! Comment the code for the Entry B609714 in this fn.

  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
  *!*	  SET DATASESSION TO lnOld_DataSession_ID
  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]


  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]



  RETURN
ENDIF
lcBinLoc = ''

SELECT(loFormSet.lcInvLine)
IF EOF() .OR. TOTQTY = 0

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
*! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
*! Comment the code for the Entry B609714 in this fn.

*!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
*!*	  SET DATASESSION TO lnOld_DataSession_ID
*!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]

  *B610350,1 TMI 06/02/2013 [Start] close these tables
  =lfCloseTbl('WHBINLOC')
  *=lfCloseTbl('WHSLOC')
  *B610350,1 TMI 06/02/2013 [End  ] 

  RETURN
ENDIF


SCATTER MEMVAR MEMO
PUSH KEY
ON KEY
DO FORM (oAriaApplication.ScreenHome+'AR\ARBNLOC.SCX') WITH loFormSet,lcKey
POP KEY

  *B610350,1 TMI 06/02/2013 [Start] close these tables
  =lfCloseTbl('WHBINLOC')
  *=lfCloseTbl('WHSLOC')
  =lfCloseTbl('PKBINLOC')
  *B610350,1 TMI 06/02/2013 [End  ] 

SELECT(lnAlias)

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
*! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
*! Comment the code for the Entry B609714 in this fn.

*!*	*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
*!*	SET DATASESSION TO lnOld_DataSession_ID
*!*	*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]


*-- End of Function lfARGETBIN.

*!***************************************************************************
*!* Name        : lfDLINTVAR
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Add new fields to the Temp. file (lcInvLine)
*!***************************************************************************
*!* Called from : Ardinv.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLINTVAR()
*!***************************************************************************
FUNCTION lfDLINTVAR

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnCount,lcI
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+16,18]
FOR lnCount = 1 TO 8
  lcI = STR(lnCount,1)
  laFileStru[lnFileStru+ lnCount , 1] = 'BinLoc' + lcI
  laFileStru[lnFileStru+ lnCount , 2] = 'C'
  laFileStru[lnFileStru+ lnCount , 3] = 10
  laFileStru[lnFileStru+ lnCount , 4] = 0

  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
    laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
    laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
    laFileStru[lnFileStru+lnCount,16]
  STORE 0  TO laFileStru[lnFileStru+lnCount,17],  laFileStru[lnFileStru+lnCount,18]

ENDFOR

FOR lnCount = 9 TO 16
  lcI = STR(lnCount-8,1)
  laFileStru[lnFileStru+ lnCount , 1] = 'BinQty' + lcI
  laFileStru[lnFileStru+ lnCount , 2] = 'N'
  laFileStru[lnFileStru+ lnCount , 3] = 7
  laFileStru[lnFileStru+ lnCount , 4] = 0

  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
    laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
    laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
    laFileStru[lnFileStru+lnCount,16]
  STORE 0  TO laFileStru[lnFileStru+lnCount,17],  laFileStru[lnFileStru+lnCount,18]

ENDFOR
*-- End of Function lfDLINTVAR.

*!***************************************************************************
*!* Name        : lfARCKSVBN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Check if bin Locations assigned to the invoice styles or not
*!***************************************************************************
*!* Called from : ARDINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARCKSVBN()
*!***************************************************************************
FUNCTION lfARCKSVBN

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnSlct,lcKey
lnSlct = SELECT(0)

lcInvLine = loFormSet.lcInvLine
SELECT (lcInvLine)
*B608939,1 TMI 07/19/2009 07:21:27 PM [Start] save current key
LOCAL lcKeyLn
lcKeyLn = EVALUATE(KEY())
*B608939,1 TMI 07/19/2009 07:21:28 PM [End  ]
LOCATE

SCAN
  m.loc1 = &lcInvLine..Binloc1
  m.loc2 = &lcInvLine..Binloc2
  m.loc3 = &lcInvLine..Binloc3
  m.loc4 = &lcInvLine..Binloc4
  m.loc5 = &lcInvLine..Binloc5
  m.loc6 = &lcInvLine..Binloc6
  m.loc7 = &lcInvLine..Binloc7
  m.loc8 = &lcInvLine..Binloc8
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    *--Don't assign Bin Location if Not Style.linvsty
    *- include all styles in check
    IF !EMPTY(&lcInvLine..Qty&lcI) .AND. EMPTY(m.loc&lcI)
      lcMsg2 = 'You cannot save style '+&lcInvLine..STYLE + ' without assign bin location.'
      lcKey = EVALUATE(KEY())
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)

      *T20071102.0018,9 TMI 04/24/2008 [Start] This is a good suggestion ,but when I select line 1 and Line 2 has missed bin , it shows the bins of line 1
      DO lfARGETBIN IN BN4MAIN.FXP WITH loFormSet,lcKey
      *T20071102.0018,9 TMI 04/24/2008 [End  ]

      *B608939,1 TMI 07/19/2009 07:23:56 PM [Start] relocate key value
      SELECT (lcInvLine)
      =SEEK(lcKeyLn)
      *B608939,1 TMI 07/19/2009 07:23:57 PM [End  ]

      SELECT (lnSlct)
      RETURN .F.
    ENDIF
  ENDFOR
ENDSCAN

*B608939,1 TMI 07/19/2009 07:23:56 PM [Start] relocate key value
SELECT (lcInvLine)
=SEEK(lcKeyLn)
*B608939,1 TMI 07/19/2009 07:23:57 PM [End  ]

SELECT (lnSlct)
*-- End of Function lfARCKSVBN.


*!***************************************************************************
*!* Name        : lfDLVODINV
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Save Data to Void Invoice screen
*!***************************************************************************
*!* Called from : Ardinv.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVODINV()
*!***************************************************************************
FUNCTION lfDLVODINV

IF !lfIsUseBin()
  RETURN
ENDIF

*--Open the needed files with indexes
IF !lfOpnFiles("WHBINLOC,IVBINLOC,PKBINLOC,STYINVJL,BININVJL","WHBINLST,IVBINLIN,PKBINLIN,STYINVJL,STYINVJL","")
  RETURN
ENDIF

*-- In case of Voiding Consolidated invoices
IF INVHDR.CONSOL = 'Y'
  =lfConsVoid()
  =lfConsUpd()
  RETURN
ENDIF

*T20071102.0018,10/C200876 TMI 05/13/2008 [Start]
*=lfDLARVOID()
*T20071102.0018,10/C200876 TMI 05/13/2008 [End  ]

IF gfSEEK(InvLine.Invoice+InvLine.STYLE+STR(InvLine.LINENO,6)+InvHdr.cWareCode ,'IVBINLOC')

  LOCAL lcSvOrd
  lcSvOrd = ORDER('STYINVJL')
  SELECT STYINVJL
  =gfSetOrder('STYDATE')       && STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE

  *T20071102.0018,10/C200876 TMI 05/29/2008 [Start]
  *=SEEK(INVLINE.Style+INVLINE.cWareCode+DTOS(INVHDR.VDATE),'STYINVJL')
  =SEEK(INVLINE.STYLE+INVLINE.cWareCode+DTOS(oAriaApplication.SystemDate),'STYINVJL')
  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]
  SELECT STYINVJL
  *T20071102.0018,10/C200876 TMI 05/29/2008 [Start]
  *SCAN REST WHILE STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = InvLine.style+InvLine.cwarecode+DTOS(INVHDR.VDATE) ;
  FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND LINENO = Invline.Lineno
  *B610658,1 TMI 01/19/2014 14:16 [Start] comment this code 
  *                                       instead : loop through the PKBINLOC and create the void invoice lines in the 
  *                                       BININVJL same way you created the invoice lines   
  *SCAN REST WHILE STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = InvLine.STYLE+InvLine.cwarecode+DTOS(oAriaApplication.SystemDate) ;
  *    FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND LINENO = Invline.LINENO
  *  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]
  *  SCATTER MEMVAR MEMO
  *  =SEEK(InvLine.Invoice+InvLine.STYLE+STR(InvLine.LINENO,6)+InvHdr.cWareCode,'IVBINLOC')
  *  SELECT IVBINLOC
  *  SCAN REST WHILE INVOICE+STYLE+STR(LINENO,6)+CWARECODE+CLOCATION+ORDER = ;
  *      InvLine.Invoice+InvLine.STYLE+STR(InvLine.LINENO,6)+InvHdr.cWareCode
  *    m.nStk1 = IVBINLOC.QTY1
  *    m.nStk2 = IVBINLOC.QTY2
  *    m.nStk3 = IVBINLOC.QTY3
  *    m.nStk4 = IVBINLOC.QTY4
  *    m.nStk5 = IVBINLOC.QTY5
  *    m.nStk6 = IVBINLOC.QTY6
  *    m.nStk7 = IVBINLOC.QTY7
  *    m.nStk8 = IVBINLOC.QTY8
  *    m.nTotStk = m.nStk1+m.nStk2+m.nStk3+m.nStk4+m.nStk5+m.nStk6+m.nStk7+m.nStk8
  *    m.NSTKVAL = m.nTotStk*m.nCost
  *    *T20071102.0018,10/C200876 TMI 07/02/2008 [Start] Update the field cLocation
  *    m.cLocation = IVBINLOC.cLocation
  *    *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]
  *    SELECT BININVJL
  *    =gfAppend('BININVJL',.T.)
  *  ENDSCAN
  *ENDSCAN

  **TMI** Create a temp file to loop through instead of the standard file,
  *       I did that to be able to decrement from the stock values in the file I loop over
  LOCAL lcSTYINVJL_Curs,laStru
  lcSTYINVJL_Curs = gfTempName()
  SELECT STYINVJL
  DIMENSION laStru[1,18]
  afields(laStru)
  create cursor &lcSTYINVJL_Curs from array laStru
  index on STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE tag stydate
  select styinvjl
  SCAN REST WHILE STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = InvLine.STYLE+InvLine.cwarecode+DTOS(oAriaApplication.SystemDate) ;
      FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND LINENO = Invline.LINENO
    scatter memv memo
    sele &lcSTYINVJL_Curs 
    append blank
    gath memv memo
  endscan
  sele &lcSTYINVJL_Curs
  replace nstk1 with abs(nstk1) ;
          nstk2 with abs(nstk2) ;
          nstk3 with abs(nstk3) ;
          nstk4 with abs(nstk4) ;
          nstk5 with abs(nstk5) ;
          nstk6 with abs(nstk6) ;
          nstk7 with abs(nstk7) ;
          nstk8 with abs(nstk8) all 
  Locate  

  *loop through the PKBINLOC and create the void invoice lines in the BININVJL same way you created the invoice lines   
  =gfSEEK(InvHdr.PIKTKT+invhdr.cWareCode,'PKBINLOC')  
  SELECT PKBINLOC
  LOCATE 
  SCAN FOR STYLE = invline.STYLE and LINENO = invline.lineno
    dime laOrgQty[9] 
    SCATT FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO laOrgQty
    
    SELECT (lcSTYINVJL_Curs)
    LOCATE 
    SCAN FOR STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = InvLine.STYLE+InvLine.cwarecode+DTOS(oAriaApplication.SystemDate) ;
        AND cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND LINENO = Invline.LINENO
      SCATTER MEMVAR MEMO
    
      IF &lcSTYINVJL_Curs..nstk1+&lcSTYINVJL_Curs..nstk2+&lcSTYINVJL_Curs..nstk3+&lcSTYINVJL_Curs..nstk4+;
         &lcSTYINVJL_Curs..nstk5+&lcSTYINVJL_Curs..nstk6+&lcSTYINVJL_Curs..nstk7+&lcSTYINVJL_Curs..nstk8= 0 ;
         OR laOrgQty[1]+laOrgQty[2]+laOrgQty[3]+laOrgQty[4]+laOrgQty[5]+laOrgQty[6]+laOrgQty[7]+laOrgQty[8]=0
        LOOP
      ENDIF 
      STORE 0 TO NSTK1,NSTK2,NSTK3,NSTK4,NSTK5,NSTK6,NSTK7,NSTK8,NTOTSTK
      local lnI,lcI
      for lnI=1 to 8
        lcI = str(lnI,1)
        lnQ = min(laOrgQty[lnI],&lcSTYINVJL_Curs..nstk&lcI)
        m.nStk&lcI = lnQ
        m.ntotstk = m.ntotstk + lnQ
        replace &lcSTYINVJL_Curs..nstk&lcI with &lcSTYINVJL_Curs..nstk&lcI - lnQ
        laOrgQty[lnI] = laOrgQty[lnI] - lnQ
      endfor

      m.NSTKVAL = m.nTotStk*m.nCost
      m.cLocation = PKBINLOC.cLocation
  
      SELECT BININVJL
      =gfAppend('BININVJL',.T.)
    ENDSCAN     
  Endscan  
  
  USE IN (lcSTYINVJL_Curs)
  *B610658,1 TMI 01/19/2014 14:18 [End  ] 
  
  lcSvOrd = ORDER('STYINVJL')
  SELECT STYINVJL
  =gfSetOrder('STYDATE')       && STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE

  SELECT IVBINLOC
  *T20071102.0018,10/C200876 TMI 05/29/2008 [Start]
  LOCATE
  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]
  SCAN REST WHILE Invoice+STYLE+STR(LINENO,6)+cWareCode+cLocation= ;
      InvLine.Invoice+InvLine.STYLE+STR(InvLine.LINENO,6)+InvHdr.cWareCode

    *- locate the style in style file
    =gfSEEK(IvBinLoc.STYLE,'STYLE')

    IF !STYLE.LINVSTY  && update whbinloc only for inv. styles
      LOOP
    ENDIF

    SELECT WHBINLOC
    IF !gfSEEK(IvBinLoc.STYLE+ IvBinLoc.cWareCode+IvBinLoc.cLocation ,'WHBINLOC')
      *T20071102.0018,10/C200876 TMI 10/12/2008 [Start] comment this definition , as it moved outside the if statement
      *lcReplace = lcReplace + ;
      "STYLE     WITH '"+ IvBinLoc.Style     +"' "+;
      "CWARECODE WITH '"+ IvBinLoc.cWareCode +"' "+;
      "cLocation WITH '"+ IvBinLoc.cLocation +"' "+;
      "cAdd_User  WITH '"+ oAriaApplication.User_ID +"' "+;
      "dAdd_Date  WITH '"+ DTOS(DATE())             +"' "+;
      "cAdd_Time  WITH '"+ gfGetTime()              +"' "
      lcReplace = "STYLE     WITH '"+ IvBinLoc.STYLE     +"' "+;
        "CWARECODE WITH '"+ IvBinLoc.cWareCode +"' "+;
        "cLocation WITH '"+ IvBinLoc.cLocation +"' "+;
        "cAdd_User  WITH '"+ oAriaApplication.User_ID +"' "+;
        "dAdd_Date  WITH {^"+STR(YEAR(oAriaApplication.SystemDate),4)+"-"+STR(MONTH(oAriaApplication.SystemDate),2)+"-"+STR(DAY(oAriaApplication.SystemDate),2) +"} "+;
        "cAdd_Time  WITH '"+ gfGetTime()              +"' "
      *T20071102.0018,10/C200876 TMI 10/12/2008 [End  ]
      =gfAppend()
      *T20071102.0018,10/C200876 TMI 10/12/2008 [Start]
      =gfReplace(lcReplace)
      *T20071102.0018,10/C200876 TMI 10/12/2008 [End  ]
    ENDIF

    *T20071102.0018,10/C200876 TMI 10/13/2008 [Start] Update the allocated qty only if there is a related line in the pkbinloc file
    *lcReplace = 'Qty1   WITH '+STR( Qty1+IvBinLoc.Qty1 ) +' '+ ;
    'Qty2   WITH '+STR( Qty2+IvBinLoc.Qty2 ) +' '+ ;
    'Qty3   WITH '+STR( Qty3+IvBinLoc.Qty3 ) +' '+ ;
    'Qty4   WITH '+STR( Qty4+IvBinLoc.Qty4 ) +' '+ ;
    'Qty5   WITH '+STR( Qty5+IvBinLoc.Qty5 ) +' '+ ;
    'Qty6   WITH '+STR( Qty6+IvBinLoc.Qty6 ) +' '+ ;
    'Qty7   WITH '+STR( Qty7+IvBinLoc.Qty7 ) +' '+ ;
    'Qty8   WITH '+STR( Qty8+IvBinLoc.Qty8 ) +' '+ ;
    'Alo1   WITH '+STR( Alo1+IvBinLoc.Qty1 ) +' '+ ;
    'Alo2   WITH '+STR( Alo2+IvBinLoc.Qty2 ) +' '+ ;
    'Alo3   WITH '+STR( Alo3+IvBinLoc.Qty3 ) +' '+ ;
    'Alo4   WITH '+STR( Alo4+IvBinLoc.Qty4 ) +' '+ ;
    'Alo5   WITH '+STR( Alo5+IvBinLoc.Qty5 ) +' '+ ;
    'Alo6   WITH '+STR( Alo6+IvBinLoc.Qty6 ) +' '+ ;
    'Alo7   WITH '+STR( Alo7+IvBinLoc.Qty7 ) +' '+ ;
    'Alo8   WITH '+STR( Alo8+IvBinLoc.Qty8 )
    lcReplace = 'Qty1   WITH '+STR( Qty1+IvBinLoc.Qty1 ) +' '+ ;
      'Qty2   WITH '+STR( Qty2+IvBinLoc.Qty2 ) +' '+ ;
      'Qty3   WITH '+STR( Qty3+IvBinLoc.Qty3 ) +' '+ ;
      'Qty4   WITH '+STR( Qty4+IvBinLoc.Qty4 ) +' '+ ;
      'Qty5   WITH '+STR( Qty5+IvBinLoc.Qty5 ) +' '+ ;
      'Qty6   WITH '+STR( Qty6+IvBinLoc.Qty6 ) +' '+ ;
      'Qty7   WITH '+STR( Qty7+IvBinLoc.Qty7 ) +' '+ ;
      'Qty8   WITH '+STR( Qty8+IvBinLoc.Qty8 ) +' '
    *T20071102.0018,10/C200876 TMI 10/13/2008 [End  ]
    =gfReplace(lcReplace)

    *T20071102.0018,10/C200876 TMI 10/13/2008 [Start] Update only the qty fields
    *T20071102.0018,10/C200876 TMI 10/13/2008 [Start]
    *lcReplace = 'TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '+;
    'TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 '
    lcReplace = 'TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
    *T20071102.0018,10/C200876 TMI 10/13/2008 [End  ]
    =gfReplace(lcReplace)

    IF gfSEEK(InvHdr.PIKTKT+IvBinLoc.cWareCode+IvBinLoc.cLocation+IvBinLoc.STYLE+STR(IvBinLoc.LINENO,6),'PKBINLOC')
      SELECT PKBINLOC
      lcReplace = 'lInvoiced   WITH .F.'
      =gfReplace(lcReplace)
      *T20071102.0018,10/C200876 TMI 10/13/2008 [Start] Update the alocated qty in whbinloc
      *T20071102.0018 TMI 11/11/2008 [Start]
      SELECT WHBINLOC
      *T20071102.0018 TMI 11/11/2008 [End  ]
      lcReplace = 'Alo1   WITH '+STR( Alo1+IvBinLoc.Qty1 ) +' '+ ;
        'Alo2   WITH '+STR( Alo2+IvBinLoc.Qty2 ) +' '+ ;
        'Alo3   WITH '+STR( Alo3+IvBinLoc.Qty3 ) +' '+ ;
        'Alo4   WITH '+STR( Alo4+IvBinLoc.Qty4 ) +' '+ ;
        'Alo5   WITH '+STR( Alo5+IvBinLoc.Qty5 ) +' '+ ;
        'Alo6   WITH '+STR( Alo6+IvBinLoc.Qty6 ) +' '+ ;
        'Alo7   WITH '+STR( Alo7+IvBinLoc.Qty7 ) +' '+ ;
        'Alo8   WITH '+STR( Alo8+IvBinLoc.Qty8 )
      *T20071102.0018 TMI 11/11/2008 [Start] Comment this and move it above the lcReplace line assignment
      *SELECT WHBINLOC
      *T20071102.0018 TMI 11/11/2008 [End  ]
      =gfReplace(lcReplace)
      lcReplace = 'TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 '
      =gfReplace(lcReplace)
      *T20071102.0018,10/C200876 TMI 10/13/2008 [End  ]
    ENDIF
    SELECT IVBINLOC
    =gfDELETE()
  ENDSCAN

  SELECT IVBINLOC
  =gfTableUpdate()

  SELECT WHBINLOC
  =gfTableUpdate()

  SELECT PKBINLOC
  =gfTableUpdate()

  *T20071102.0018,10/C200876 TMI 05/29/2008 [Start] update BININVJL
  SELECT BININVJL
  =gfTableUpdate()
  *T20071102.0018,10/C200876 TMI 05/29/2008 [End  ]
ENDIF

*-- End of Function lfDLVODINV.

*!***************************************************************************
*!* Name        : lfDLARVOID
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Update BinInvJL & WHBINLOC file in case of void Invoice
*!***************************************************************************
*!* Called from : lfDLVODINV IN BN4MAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLARVOID()
*!***************************************************************************
*T20071102.0018,7 TMI [Start]
FUNCTION lfDLARVOID

PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv,lnInvLinNo
lnCurAlias = SELECT()
lnInvLinNo = INVLINE.LINENO
lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()
*--create tmpfile for invoice bin location
SELECT IVBINLOC
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (oAriaApplication.WorkDir+lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON STYLE+cWareCode+STR(LINENO,6)+cLocation TAG (lcTmpIvbin)

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (oAriaApplication.WorkDir+lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON STYLE+cWareCode+clocation TAG (lcTmpBnInv)

SELECT IVBINLOC
=gfSEEK(InvHdr.Invoice+INVLINE.STYLE+STR(INVLINE.LINENO,6))
SCAN REST WHILE Invoice+STYLE+STR(LINENO,6)+cWareCode+cLocation = InvHdr.Invoice+INVLINE.STYLE+STR(INVLINE.LINENO,6)
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN

SELECT InvLine
lnCurRec = RECNO()
=SEEK(INVHDR.INVOICE+STR(lnInvLinNo,6))
SCAN REST WHILE invoice+STR(LINENO,6) = INVHDR.INVOICE+STR(lnInvLinNo,6)
  =SEEK(STYLE+cWareCode,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE STYLE+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LINENO,6)= ;
      InvLine.STYLE+InvLine.cwarecode FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND STR(LINENO,6)==STR(lnInvLinNo,6)
    SCATTER MEMVAR MEMO
    =SEEK(STYLE+cWareCode+STR(LINENO,6),lcTmpIvbin)
    SELECT(lcTmpIvbin)
    SCAN REST WHILE STYLE+cWareCode+STR(LINENO,6)+cLocation = STYINVJL.STYLE+STYINVJL.cWareCode+STR(STYINVJL.LINENO,6)

      llChkQty = .F.
      FOR lnI  = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
          llChkQty = .T.
        ELSE
          llChkQty = .F.
          EXIT
        ENDIF
      ENDFOR
      IF llChkQty
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        SELECT(lcTmpIvbin)
        FOR LNI = 1 TO 8
          LCI = ALLTRIM(STR(LNI))
          REPLACE Qty&LCI WITH Qty&LCI + ABS(m.nStk&LCI)
        ENDFOR
        REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
        EXIT
      ENDIF
      IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
            REPLACE nStk&lcI WITH  &lcTmpIvbin..Qty&lcI
            m.nStk&lcI = m.nStk&lcI - &lcTmpIvbin..Qty&lcI
            m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH 0
          ELSE
            REPLACE nStk&lcI WITH m.nStk&lcI
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.nStk&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.nStk&lcI
            m.nTotStk  = m.nTotStk  + m.nStk&lcI
            m.nStk&lcI = 0
          ENDIF
        ENDFOR
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE nstkval WITH nTotStk * nCost
      ENDIF
    ENDSCAN
  ENDSCAN
ENDSCAN
SELECT InvLine
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GOTO lnCurRec
ENDIF
SELECT (lcTmpBnInv)
DELETE FOR NSTK1+NSTK2+NSTK3+NSTK4+NSTK5+NSTK6+NSTK7+NSTK8 = 0
GO TOP
SCAN
  SCATTER MEMVAR MEMO
  SELECT BININVJL
  =gfAppend('BININVJL',.T.)
ENDSCAN

SELECT STYINVJL
=gfSetOrder(lcSvOrd)

=lfEraseFil(lcTmpBnInv)
=lfEraseFil(lcTmpIvbin)

SELECT (lnCurAlias)
*-- End of Function lfDLARVOID.

*!***************************************************************************
*!* Name        : lfConsVoid
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Update BinInvJL & WHBINLOC file in case of void Invoice in
*!*             : case of consolidated invoice
*!***************************************************************************
*!* Called from : lfDLVODINV IN BN4MAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfConsVoid()
*!***************************************************************************
*B608826,1 TMI 23/03/2009 [Start] rewrite this function to work the same way as creating invoice lines and updating BININVJL
*FUNCTION lfConsVoid
FUNCTION x_lfConsVoid
*B608826,1 TMI 23/03/2009 [End  ]
*!*	PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv,lnInvLinNo
*!*	lnCurAlias = SELECT()
*!*	lnInvLinNo = INVLINE.LINENO
*!*	lcTmpIvbin = gfTempName()
*!*	lcTmpBnInv = gfTempName()

*!*	*--create tmpfile for invoice bin location
*!*	SELECT IVBINLOC
*!*	DIMENSION laFileStru[1,4]
*!*	=AFIELDS(laFileStru)
*!*	CREATE TABLE (oAriaApplication.WorkDir+lcTmpIvbin) FROM ARRAY laFileStru
*!*	INDEX ON Style+cWareCode+STR(LineNo,6)+cLocation TAG (lcTmpIvbin)

*!*	*--create tmpfile for bin invintory file
*!*	SELECT BININVJL
*!*	DIMENSION laFileStru[1,4]
*!*	=AFIELDS(laFileStru)
*!*	CREATE TABLE (oAriaApplication.WorkDir+lcTmpBnInv) FROM ARRAY laFileStru
*!*	INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)

*!*	SELECT CONSINVL
*!*	=SEEK(INVHDR.INVOICE)
*!*	SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = INVHDR.INVOICE FOR STYLE = INVLINE.Style
*!*	  SELECT IVBINLOC
*!*	  =gfSEEK(InvHdr.Invoice+INVLINE.STYLE+STR(CONSINVL.LineNo,6),'IVBINLOC')
*!*	  SCAN REST WHILE Invoice+Style+STR(LineNo,6)+cWareCode+cLocation = InvHdr.Invoice+INVLINE.STYLE+STR(CONSINVL.LineNo,6)
*!*	    SCATTER MEMVAR MEMO
*!*	    INSERT INTO (lcTmpIvbin) FROM MEMVAR
*!*	  ENDSCAN
*!*	ENDSCAN

*!*	SELECT InvLine
*!*	lnCurRec = RECNO()
*!*	=SEEK(INVHDR.INVOICE+STR(lnInvLinNo,6))
*!*	SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE+STR(lnInvLinNo,6)
*!*	  =SEEK(Style+cWareCode,'STYINVJL')
*!*	  SELECT STYINVJL
*!*	  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LineNo,6)= ;
*!*	       InvLine.style+InvLine.cwarecode FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND STR(LINENO,6)==STR(lnInvLinNo,6)
*!*	    SCATTER MEMVAR MEMO
*!*	    *- TMI 22/03/2009 [Start] no need for this seek here, as it is called later
*!*	    *=SEEK(Style+cWareCode+STR(LineNo,6),lcTmpIvbin)
*!*	    *-  TMI 22/03/2009 [End  ]
*!*	    SELECT CONSINVL
*!*	    =SEEK(INVHDR.INVOICE)
*!*	    SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = INVHDR.INVOICE ;
*!*	                FOR STYLE = STYINVJL.Style
*!*	      m.Lineno= CONSINVL.LineNo
*!*	      m.nSTK1 = CONSINVL.QTY1
*!*	      m.nSTK2 = CONSINVL.QTY2
*!*	      m.nSTK3 = CONSINVL.QTY3
*!*	      m.nSTK4 = CONSINVL.QTY4
*!*	      m.nSTK5 = CONSINVL.QTY5
*!*	      m.nSTK6 = CONSINVL.QTY6
*!*	      m.nSTK7 = CONSINVL.QTY7
*!*	      m.nSTK8 = CONSINVL.QTY8
*!*	      m.nTotStk = CONSINVL.TOTQTY
*!*	      SELECT(lcTmpIvbin)
*!*	      =SEEK(STYINVJL.Style+STYINVJL.cWareCode+STR(CONSINVL.LINENO,6))
*!*	      SCAN REST WHILE Style+cWareCode+STR(LineNo,6)+cLocation = STYINVJL.Style+STYINVJL.cWareCode+STR(CONSINVL.LINENO,6)
*!*	        llChkQty = .F.
*!*	        FOR lnI  = 1 TO 8
*!*	          lcI = ALLT(STR(lnI,2))
*!*	          *- This means :
*!*	          *- check the Consolidated line nstk1 qty is 0
*!*	          *- if it > 0 then check that the qty in IVBINLOC is more than that of Consolidated lines
*!*	          IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
*!*	            llChkQty = .T.
*!*	          ELSE
*!*	            llChkQty = .F.
*!*	            EXIT
*!*	          ENDIF
*!*	        ENDFOR
*!*	        IF llChkQty
*!*	          SELECT (lcTmpBnInv)
*!*	          m.clocation = &lcTmpIvbin..clocation
*!*	          APPEND BLANK
*!*	          GATHER MEMVAR MEMO
*!*	          SELECT(lcTmpIvbin)
*!*	          FOR LNI = 1 TO 8
*!*	            LCI = ALLTRIM(STR(LNI))
*!*	            REPLACE Qty&LCI WITH Qty&LCI + ABS(m.nStk&LCI)
*!*	          ENDFOR
*!*	          REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
*!*	          EXIT
*!*	        ENDIF
*!*	        IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
*!*	          SELECT (lcTmpBnInv)
*!*	          m.clocation = &lcTmpIvbin..clocation
*!*	          APPEND BLANK
*!*	          GATHER MEMVAR MEMO
*!*	          FOR lnI = 1 TO 8
*!*	            lcI = ALLT(STR(lnI,2))
*!*	            IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
*!*	              REPLACE nStk&lcI WITH  &lcTmpIvbin..Qty&lcI
*!*	              m.nStk&lcI = m.nStk&lcI - &lcTmpIvbin..Qty&lcI
*!*	              m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
*!*	              REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
*!*	              REPLACE &lcTmpIvbin..Qty&lcI WITH 0
*!*	            ELSE
*!*	              REPLACE nStk&lcI WITH m.nStk&lcI
*!*	              REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.nStk&lcI
*!*	              REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.nStk&lcI
*!*	              m.nTotStk  = m.nTotStk  + m.nStk&lcI
*!*	              m.nStk&lcI = 0
*!*	            ENDIF
*!*	          ENDFOR
*!*	          REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
*!*	          REPLACE nstkval WITH nTotStk * nCost
*!*	        ENDIF
*!*	      ENDSCAN
*!*	    ENDSCAN
*!*	  ENDSCAN
*!*	ENDSCAN

*!*	SELECT InvLine
*!*	IF BETWEEN(lnCurRec ,1,RECCOUNT())
*!*	  GOTO lnCurRec
*!*	ENDIF

*!*	SELECT (lcTmpBnInv)
*!*	SCAN
*!*	  SCATTER MEMVAR MEMO
*!*	  SELECT BININVJL
*!*	  =gfAppend('BININVJL',.T.)
*!*	ENDSCAN

*!*	=lfEraseFil(lcTmpBnInv)
*!*	=lfEraseFil(lcTmpIvbin)

*!*	SELECT BININVJL
*!*	*- TMI 23/03/2009 [Start] remove the parameters
*!*	*=gfTableUpdate('BININVJL',.T.)
*!*	=gfTableUpdate()
*!*	*- TMI 23/03/2009 [End  ]
*!*	SELECT (lnCurAlias)
*-- End of Function lfConsVoid.

*:**************************************************************************
*:* Name        : lfConsVoid
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 23/03/2009
*:* Purpose     : Update the BININVJL when voiding a consolidated invoice
*:***************************************************************************
*B608826,1 TMI 24/03/2009 re write this function
FUNCTION lfConsVoid
PRIVATE lnCurAlias,lnInvLinNo

lnCurAlias = SELECT()

LOCAL lcSvOrd
lcSvOrd = ORDER('STYINVJL')
SELECT STYINVJL
=gfSetOrder('STYDATE')       && STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE
=SEEK(INVLINE.STYLE+INVLINE.cWareCode+DTOS(oAriaApplication.SystemDate),'STYINVJL')    && seeks the updated line for this style occured today
SCAN REST WHILE STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = InvLine.STYLE+InvLine.cwarecode+DTOS(oAriaApplication.SystemDate) ;
    FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND LINENO = Invline.LINENO
  SCATTER MEMVAR MEMO

  =gfSEEK(INVLINE.Invoice+INVLINE.STYLE,'IVBINLOC')
  SELECT IVBINLOC
  SCAN REST WHILE INVOICE+STYLE+STR(LINENO,6)+CWARECODE+CLOCATION+ORDER = INVLINE.Invoice+INVLINE.STYLE

    m.Lineno= IVBINLOC.LINENO
    m.nSTK1 = IVBINLOC.QTY1
    m.nSTK2 = IVBINLOC.QTY2
    m.nSTK3 = IVBINLOC.QTY3
    m.nSTK4 = IVBINLOC.QTY4
    m.nSTK5 = IVBINLOC.QTY5
    m.nSTK6 = IVBINLOC.QTY6
    m.nSTK7 = IVBINLOC.QTY7
    m.nSTK8 = IVBINLOC.QTY8
    m.nTotStk = IVBINLOC.TOTQTY

    SELECT BININVJL
    =gfAppend('BININVJL',.T.)
  ENDSCAN
ENDSCAN

SELECT BININVJL
=gfTableUpdate()
SELECT (lnCurAlias)

*-- end of lfConsVoid.

*!***************************************************************************
*!* Name        : lfConsUpd
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Save Data to Void Invoice screen in case of consolidated invoice
*!***************************************************************************
*!* Called from : BN4MAIN.prg --> lfDLVODINV()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfConsUpd()
*!***************************************************************************
FUNCTION lfConsUpd

*B608826,1 TMI 25/03/2009 [Start] a comment
*- note that the lines are saved in IVBINLOC summed, i.e if style,Warecode,store,lineno are same event they are
*- from different orders, so this may influnce the loop below
*B608826,1 TMI 25/03/2009 [End  ]


*B608826,1 TMI 25/03/2009 [Start]
=gfSEEK(INVLINE.STYLE+INVLINE.cWareCode,'WHBINLOC')  && get style lines from WHBINLOC
=gfSeek(CHR(255),'IVBINLOC')   &&- empty the IVBINLOC cursor by searching a non existing string
LOCAL lcKey                    && save the key to not loop on the same INVOICE+STYLE+LINENO+CWARECODE in IVBINLOC twice
lcKey = '  '
*B608826,1 TMI 25/03/2009 [End  ]

SELECT CONSINVL
=SEEK(INVHDR.INVOICE)
SCAN REST WHILE Invoice+STORE+ORDER+STYLE+STR(LINENO,6) = INVHDR.INVOICE FOR STYLE = INVLINE.STYLE
  SELECT IVBINLOC
  IF gfSEEK(InvLine.Invoice+InvLine.STYLE+STR(CONSINVL.LINENO,6)+InvHdr.cWareCode ,'IVBINLOC')
    *B608826,1 TMI 25/03/2009 [Start] move gfSeek above to not called twice, replace it with SEEK
    *=gfSEEK(IvBinLoc.Style+ IvBinLoc.cWareCode,'WHBINLOC')
    =SEEK(IvBinLoc.STYLE+ IvBinLoc.cWareCode,'WHBINLOC')
    *B608826,1 TMI 25/03/2009 [End  ]
    SELECT IVBINLOC
    SCAN REST WHILE Invoice+STYLE+STR(LINENO,6)+cWareCode+cLocation= ;
        InvLine.Invoice+InvLine.STYLE+STR(CONSINVL.LINENO,6)+InvHdr.cWareCode
      *B608826,1 TMI 25/03/2009 [Start] check if the key changed
      IF lcKey <> InvLine.Invoice+InvLine.STYLE+STR(CONSINVL.LINENO,6)+InvHdr.cWareCode
        *B608826,1 TMI 25/03/2009 [End  ]
        SELECT WHBINLOC
        IF !SEEK(IvBinLoc.STYLE+ IvBinLoc.cWareCode+IvBinLoc.cLocation ,'WHBINLOC')
          lcReplace = 'STYLE      WITH ['+IvBinLoc.STYLE     + '] '+;
            'CWARECODE  WITH ['+IvBinLoc.cWareCode + '] '+;
            'clocation  WITH ['+IvBinLoc.cLocation + '] '+;
            'cAdd_User  WITH [' + oAriaApplication.User_ID  + '] '+;
            'dAdd_Date  WITH {^'+STR(YEAR(DATE()),4)+'-'+STR(MONTH(DATE()),2)+'-'+STR(DAY(DATE()),2) +'} '+;
            'cAdd_Time  WITH [' + gfGetTime()           + '] '
          =gfAppend()
          =gfReplace(lcReplace)
        ENDIF
        lcReplace ='Qty1   WITH '+STR( Qty1+IvBinLoc.Qty1 )+;
          'Qty2   WITH '+STR( Qty2+IvBinLoc.Qty2 )+;
          'Qty3   WITH '+STR( Qty3+IvBinLoc.Qty3 )+;
          'Qty4   WITH '+STR( Qty4+IvBinLoc.Qty4 )+;
          'Qty5   WITH '+STR( Qty5+IvBinLoc.Qty5 )+;
          'Qty6   WITH '+STR( Qty6+IvBinLoc.Qty6 )+;
          'Qty7   WITH '+STR( Qty7+IvBinLoc.Qty7 )+;
          'Qty8   WITH '+STR( Qty8+IvBinLoc.Qty8 )+;
          'Alo1   WITH '+STR( Alo1+IvBinLoc.Qty1 )+;
          'Alo2   WITH '+STR( Alo2+IvBinLoc.Qty2 )+;
          'Alo3   WITH '+STR( Alo3+IvBinLoc.Qty3 )+;
          'Alo4   WITH '+STR( Alo4+IvBinLoc.Qty4 )+;
          'Alo5   WITH '+STR( Alo5+IvBinLoc.Qty5 )+;
          'Alo6   WITH '+STR( Alo6+IvBinLoc.Qty6 )+;
          'Alo7   WITH '+STR( Alo7+IvBinLoc.Qty7 )+;
          'Alo8   WITH '+STR( Alo8+IvBinLoc.Qty8 )
        =gfReplace(lcReplace)

        lcReplace = 'TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '+;
          'TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 '
        =gfReplace(lcReplace)

        *B608826,1 TMI 25/03/2009 [Start] close the above IF
      ENDIF
      *B608826,1 TMI 25/03/2009 [End  ]

      *--Search in Consinvh File to get the piktkt Number.
      =SEEK(CONSINVL.INVOICE+CONSINVL.STORE+CONSINVL.ORDER,'CONSINVH')

      IF gfSEEK(CONSINVH.PIKTKT+IvBinLoc.cWareCode+IvBinLoc.cLocation+IvBinLoc.STYLE+STR(IvBinLoc.LINENO,6),'PKBINLOC')
        SELECT PKBINLOC
        lcReplace = 'lInvoiced   WITH .F.'
        =gfReplace(lcReplace)
      ENDIF
      SELECT IVBINLOC
      =gfDelete()
    ENDSCAN
  ENDIF
  *B608826,1 TMI 25/03/2009 [Start] saves the key
  lcKey = InvLine.Invoice+InvLine.STYLE+STR(CONSINVL.LINENO,6)+InvHdr.cWareCode
  *B608826,1 TMI 25/03/2009 [End  ]
ENDSCAN

SELECT IVBINLOC
=gfTableUpdate()

SELECT WHBINLOC
=gfTableUpdate()

SELECT PKBINLOC
=gfTableUpdate()

*-- End of Function lfConsUpd.
*!***************************************************************************
*!* Name        : lfChngShip
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Hold the old and new Qty if user changed in the Shipped Qty
*!***************************************************************************
*!* Called from : ARIINV.PRG -->lfvSizeQty
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChngShip()
*!***************************************************************************
FUNCTION lfChngShip

IF !lfIsUseBin()
  RETURN
ENDIF

loShpQty = _SCREEN.ACTIVEFORM.ACTIVECONTROL
IF loShpQty.VALUE <> loShpQty.OldValue
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,"You can not change Shipped Qty. here , please amend your pick ticket")
  loShpQty.VALUE = loShpQty.OldValue
  RETURN .F.
ENDIF
*-- End of Function lfChngShip.

*!***************************************************************************
*!* Name        : lfALSAVINV
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Save Data for Invoice Sales Order screen
*!***************************************************************************
*!* Called from : ARINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfALSAVINV()
*!***************************************************************************
FUNCTION lfALSAVINV

NOTE This function need to be enhanced so the two cases are written common in one case

LOCAL lnSlct
lnSlct = SELECT(0)

STORE '' TO lcPkStyle
STORE .F. TO llPickOne,llBlkPck
IF !lfIsUseBin()
  RETURN
ENDIF
*C201070 TMI 25/02/2009 [Start] open bininvjl
*IF !lfOpnFiles("WHBINLOC,PKBINLOC,IVBINLOC,WHSLOC","WHBINLST,PKLINE,IVBINLIN,WHSLOC","")
IF !lfOpnFiles("WHBINLOC,PKBINLOC,IVBINLOC,WHSLOC,BININVJL,STYINVJL","WHBINLST,PKLINE,IVBINLIN,WHSLOC,WHBINSTY,STYINVJL","")
  *C201070 TMI 25/02/2009 [End  ]
  RETURN
ENDIF

*--check in case of consolidate
*B608826,1 TMI 25/03/2009 [Start] commant this and rewrite it next
*!*	IF  &lcHdrFile..CONSOL = 'Y'
*!*	  RETURN
*!*	ENDIF

*- defer updating if this is an consolidated invoice line
IF &lcDetFile..CONSOL = 'Y'
  SAVE TO (oAriaApplication.WorkDir+lcDetFile+'.mem') ALL LIKE lcDetFile
  RETURN
ENDIF
*- get the conlolidated lines from lcDetFile
lcConsCur = 'Z'+SUBSTR(lcDetFile,2)
IF FILE(oAriaApplication.WorkDir+lcDetFile+'.mem')
  ERASE (oAriaApplication.WorkDir+lcDetFile+'.mem')
  SELECT * FROM &lcDetFile ;
    WHERE &lcExpr = &lcHdrFile..ACCOUNT .AND. &lcDetFile..CONSOL = 'Y' ;
    INTO CURSOR &lcConsCur
ENDIF
lcKey = &lcDetFile..STYLE+&lcDetFile..cWarecode+lcGlSession + DTOS(&lcHdrFile..InvDate)+&lcHdrFile..Invoice+STR(&lcDetFile..LINENO,6)
IF USED(lcConsCur)
  SELECT &lcConsCur
  LOCATE FOR ACCOUNT+CWARECODE+STYLE = ;
    &lcDetFile..ACCOUNT+&lcDetFile..CWARECODE+&lcDetFile..STYLE
  IF FOUND()
    *- get the consolidated line # to seek with it in STYINVJL file
    lcKey = &lcDetFile..STYLE+&lcDetFile..cWarecode+lcGlSession + DTOS(&lcHdrFile..InvDate)+&lcHdrFile..Invoice+STR(&lcConsCur..LINENO,6)
  ENDIF
ENDIF

*- Updating BININVJL sql file
IF !USED('STYINVJL')
  =gfOpenTable(oAriaApplication.DataDir+'STYINVJL','STYINVJL','SH')
ENDIF
*B608826,1 TMI 25/03/2009 [End  ]
*B610658,2 TMI 01/19/2014 16:57 [Start] create a temp table with the same alias as the STYINVJL
PRIVATE lcTmpStyinvjl
lcTmpStyinvjl = gfTempName()
SELE STYINVJL
SEEK lcKey
SET KEY TO lcKey
COPY TO (oAriaApplication.WorkDir+lcTmpStyinvjl) WITH CDX
SET KEY TO 
*B610658,2 TMI 01/21/2014 19:34 [Start] 
*USE IN STYINVJL
*B610658,2 TMI 01/21/2014 19:34 [End  ] 
SELE 0
USE (oAriaApplication.WorkDir+lcTmpStyinvjl) ORDER STYINVJL
REPLA NSTK1 WITH ABS(NSTK1) ;
      NSTK2 WITH ABS(NSTK2) ;
      NSTK3 WITH ABS(NSTK3) ;
      NSTK4 WITH ABS(NSTK4) ;
      NSTK5 WITH ABS(NSTK5) ;
      NSTK6 WITH ABS(NSTK6) ;
      NSTK7 WITH ABS(NSTK7) ;
      NSTK8 WITH ABS(NSTK8) all
*B610658,1 TMI 01/19/2014 16:57 [End  ] 

llPickOne = gfGetMemVar('M_PICKONE')  && Check if Always Pick from on bin location Yes or No.
llBlkPck  = gfGetMemVar('M_BULKPICK') && Check if use Bulk/Pick Yes or No.

=gfSEEK(&lcDetFile..STYLE+&lcDetFile..CWARECODE,'WHBINLOC')
=gfSEEK(lcInvNo,'IVBINLOC')
*B608826,1 TMI 25/03/2009 [Start]
*=gfSEEK('   ','BININVJL')  && empty the BININVJL cursor
DIMENSION laJLQty[9]
*B608826,1 TMI 25/03/2009 [End  ]

DO CASE
CASE SUBSTR(loFormSet.NAME,4) = 'ARIINV'

  IF gfSEEK(&lcDetFile..PIKTKT+&lcDetFile..CWARECODE+STR(&lcDetFile..LINENO,6)+&lcDetFile..STYLE,'PKBINLOC')

    SELECT PKBINLOC
    SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
        &lcDetFile..PIKTKT+&lcDetFile..CWARECODE+STR(&lcDetFile..LINENO,6)+&lcDetFile..STYLE

      llUpdInvoicedFld = .F.
      =gfSeek('S'+&lcDetFile..SCALE,'SCALE')
      *B608826,1 TMI 25/03/2009 [Start] initialize the laJLQty array to send values to update the BININVJL
      laJLQty = 0
      *B608826,1 TMI 25/03/2009 [End  ]
      FOR lnCnt = 1 TO SCALE.CNT
        lcCnt = STR(lnCnt,1)
        IF &lcDetFile..Pik&lcCnt <> 0
          *--Update PkbinLoc File

          llUpdInvoicedFld = .T.

          *--Update IvBinLoc File
          IF !SEEK(lcInvNo+&lcDetFile..STYLE+STR(&lcDetFile..LINENO,6)+&lcDetFile..cWareCode+PKBINLOC.cLocation ,'IVBINLOC')
            SELECT IVBINLOC
            lcReplace = "STYLE      WITH '"+ &lcDetFile..STYLE       +"' "+;
              "CWARECODE  WITH '"+ &lcDetFile..cWareCode   +"' "+;
              "ORDER      WITH '"+ &lcDetFile..ORDER       +"' "+;
              "Invoice    WITH '"+ lcInvNo                 +"' "+;
              "clocation  WITH '"+ PKBINLOC.cLocation      +"' "+;
              "Qty&lcCnt  WITH "+ STR(PKBINLOC.Qty&lcCnt)  +" "+;
              "TotQty     WITH "+ STR(PKBINLOC.Qty&lcCnt)  +" "+;
              "LineNo     WITH "+ STR(&lcDetFile..LINENO)  +" "+;
              "cAdd_user  WITH '"+ oAriaApplication.User_ID+"' "+;
              "dAdd_Date  WITH {^"+STR(YEAR(DATE()),4)+"-"+STR(MONTH(DATE()),2)+"-"+STR(DAY(DATE()),2) +"} "+;
              "cAdd_time  WITH '"+ gfGetTime() +"' "
            =gfAppend()
            =gfReplace(lcReplace)

          ELSE

            SELECT IVBINLOC
            *B608826,1 TMI 23/03/2009 [Start] the qty should be accumulated
            *lcReplace = 'Qty&lcCnt  WITH ' + STR( PKBINLOC.Qty&lcCnt )
            lcReplace = 'Qty&lcCnt  WITH Qty&lcCnt +' + STR( PKBINLOC.Qty&lcCnt )
            *B608826,1 TMI 23/03/2009 [End  ]
            =gfReplace(lcReplace)

            lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8'
            =gfReplace(lcReplace)
          ENDIF

          IF SEEK(PKBINLOC.STYLE+PKBINLOC.CWARECODE+PKBINLOC.CLOCATION,'WHBINLOC')
            lcReplace = 'Alo&lcCnt WITH '+STR( MAX(WHBINLOC.Alo&lcCnt - PKBINLOC.Qty&lcCnt,0) )+' '+;
              'TotAlo    WITH '+STR( MAX(WHBINLOC.TotAlo    - PKBINLOC.Qty&lcCnt,0) )+' '+;
              'Qty&lcCnt WITH '+STR( MAX(WHBINLOC.Qty&lcCnt - PKBINLOC.Qty&lcCnt,0) )+' '+;
              'TotQty    WITH '+STR( MAX(WHBINLOC.TotQty    - PKBINLOC.Qty&lcCnt,0) )
            SELECT WHBINLOC
            =gfReplace(lcReplace)
            *T20071102.0018,10/C200876 TMI 10/29/2008 [Start] check the setup option to delete the bin with zero stock
            *IF TotQty = 0
            *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
            *IF TotQty = 0 .AND. gfGetMemVar('M_DELZRBNR')
            IF Qty1 = 0 AND Qty2 = 0 AND  Qty3 = 0 AND  Qty4 = 0 AND  Qty5 = 0 AND  Qty6 = 0 AND  Qty7 = 0 AND  Qty8 = 0 .AND. gfGetMemVar('M_DELZRBNR')
            *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
              *T20071102.0018,10/C200876 TMI 10/29/2008 [End  ]
              =gfDELETE()
            ENDIF
          ENDIF
        ENDIF

        *B608826,1 TMI 25/03/2009 [Start] update the laJLQty array to send values to update the BININVJL
        laJLQty[lnCnt] = PKBINLOC.Qty&lcCnt
        laJLQty[9]     = laJLQty[9] + PKBINLOC.Qty&lcCnt
        *B608826,1 TMI 25/03/2009 [End  ]

      ENDFOR

      *B608826,1 TMI 25/03/2009 [Start] updating BININVJL
      *B608898,3 TMI 06/25/2009 05:42:53 PM [Start] send a parameter with the location to update the bininvjl with
      *=lfBinInvJLUpdate(lcKey,@laJLQty)
      =lfBinInvJLUpdate(lcKey,@laJLQty,PKBINLOC.cLocation)
      *B608898,3 TMI 06/25/2009 05:42:56 PM [End  ]
      *B608826,1 TMI 25/03/2009 [End  ]

      IF llUpdInvoicedFld
        SELECT PKBINLOC
        lcReplace ='lInvoiced  WITH .T.'
        =gfReplace(lcReplace)
      ENDIF

    ENDSCAN

    *- Updating tables
    SELECT PKBINLOC
    =gfTableUpdate()

    SELECT IVBINLOC
    =gfTableUpdate()

    SELECT WHBINLOC
    =gfTableUpdate()

    *B608826,1 TMI 25/03/2009 [Start] update BININVJL
    *SELECT BININVJL
    *=gfTableUpdate()
    *B608826,1 TMI 25/03/2009 [End  ]

  ENDIF

  *B610658,1 TMI 01/19/2014 16:57 [Start] CLOSE the temp table created for STYINVJL lcTmpStyinvjl
  *B610658,2 TMI 01/21/2014 19:35 [Start] 
  *if used('STYINVJL')
  *  USE IN STYINVJL
  *ENDIF 
  USE IN (lcTmpStyinvjl)
  *B610658,2 TMI 01/21/2014 19:35 [End  ] 
  ERASE (oAriaApplication.WorkDir+lcTmpStyinvjl+'.*')
  *B610658,1 TMI 01/19/2014 16:58 [End  ] 

CASE SUBSTR(loFormSet.NAME,4) = 'ARDINV'


  *B608939,1 TMI 07/15/2009 02:21:29 PM [Start] use the INVLINE.LINENO field instead of &lcDetfile..lineno
  *=gfSEEK(lcInvNo+&lcDetFile..Style+STR(&lcDetFile..LineNo,6)+&lcDetFile..cWareCode ,'IVBINLOC')
  =gfSEEK(lcInvNo+&lcDetFile..STYLE+STR(INVLINE.LINENO,6)+&lcDetFile..cWareCode ,'IVBINLOC')
  lcKey = &lcDetFile..STYLE+&lcDetFile..cWarecode+lcGlSession + DTOS(&lcHdrFile..InvDate)+&lcHdrFile..Invoice+STR(INVLINE.LINENO,6)
  *B608939,1 TMI 07/15/2009 02:24:20 PM [End  ]
  =gfSEEK(&lcDetFile..STYLE+&lcDetFile..CWARECODE,'WHBINLOC')

  SELECT (lcDetFile)
  =gfSeek('S'+&lcDetFile..SCALE,'SCALE')
  FOR lnCnt = 1 TO SCALE.CNT
    lcCnt = STR(lnCnt,1)
    IF !EMPTY(&lcDetFile..BinLoc&lcCnt)

      *B608826,1 TMI 25/03/2009 [Start] initialize laJLQty
      laJLQty = 0
      *B608826,1 TMI 25/03/2009 [End  ]

      *--Update IvBinLoc File
      *B608939,1 TMI 07/15/2009 02:25:12 PM [Start]  use invline.lineno
      *IF !SEEK(lcInvNo+&lcDetFile..Style+STR(&lcDetFile..LineNo,6)+&lcDetFile..cWareCode+&lcDetFile..BinLoc&lcCnt ,'IVBINLOC')
      IF !SEEK(lcInvNo+&lcDetFile..STYLE+STR(INVLINE.LINENO,6)+&lcDetFile..cWareCode+&lcDetFile..BinLoc&lcCnt ,'IVBINLOC')
        *B608939,1 TMI 07/15/2009 02:25:21 PM [End  ]
        SELECT IVBINLOC
        *B608939,1 TMI 07/15/2009 02:25:40 PM [Start] use invline.lineno
        *lcReplace = "STYLE      WITH '" + &lcDetFile..Style        + "' "+;
        "CWARECODE  WITH '" + &lcDetFile..cWareCode    + "' "+;
        "ORDER      WITH '" + &lcDetFile..Order        + "' "+;
        "Invoice    WITH '" + lcInvNo                  + "' "+;
        "clocation  WITH '" + &lcDetFile..BinLoc&lcCnt + "' "+;
        "Qty&lcCnt  WITH " + STR( &lcDetFile..Qty&lcCnt ) + " "+;
        "TotQty     WITH " + STR( &lcDetFile..Qty&lcCnt ) + " "+;
        "LineNo     WITH " + STR(&lcDetFile..LineNo,6)    + " "+;
        "cAdd_user  WITH '" +oAriaApplication.User_ID   + "' "+;
        "dAdd_Date  WITH {^"+STR(YEAR(DATE()),4)+"-"+STR(MONTH(DATE()),2)+"-"+STR(DAY(DATE()),2) +"} "+;
        "cAdd_time  WITH '" +gfGetTime()                + "' "
        lcReplace = "STYLE      WITH '" + &lcDetFile..STYLE        + "' "+;
          "CWARECODE  WITH '" + &lcDetFile..cWareCode    + "' "+;
          "ORDER      WITH '" + &lcDetFile..ORDER        + "' "+;
          "Invoice    WITH '" + lcInvNo                  + "' "+;
          "clocation  WITH '" + &lcDetFile..BinLoc&lcCnt + "' "+;
          "Qty&lcCnt  WITH " + STR( &lcDetFile..Qty&lcCnt ) + " "+;
          "TotQty     WITH " + STR( &lcDetFile..Qty&lcCnt ) + " "+;
          "LineNo     WITH " + STR(INVLINE.LINENO,6)    + " "+;
          "cAdd_user  WITH '" +oAriaApplication.User_ID   + "' "+;
          "dAdd_Date  WITH {^"+STR(YEAR(DATE()),4)+"-"+STR(MONTH(DATE()),2)+"-"+STR(DAY(DATE()),2) +"} "+;
          "cAdd_time  WITH '" +gfGetTime()                + "' "
        *B608939,1 TMI 07/15/2009 02:25:54 PM [End  ]
        =gfAppend()
        =gfReplace(lcReplace)
      ELSE

        SELECT IVBINLOC
        lcReplace = 'Qty&lcCnt  WITH '+STR( &lcDetFile..Qty&lcCnt )
        =gfReplace(lcReplace)

        lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8'
        =gfReplace(lcReplace)
      ENDIF

      *B608826,1 TMI 25/03/2009 [Start] update the BININVJL
      laJLQty[lnCnt] = &lcDetFile..Qty&lcCnt
      laJLQty[9]     = &lcDetFile..Qty&lcCnt
      *B608898,3 TMI 06/25/2009 05:47:50 PM [Start] send the "&lcDetFile..BinLoc&lcCnt" as the location to use
      *=lfBinInvJLUpdate(lcKey,@laJLQty)
      =lfBinInvJLUpdate(lcKey,@laJLQty,&lcDetFile..BinLoc&lcCnt)
      *B608898,3 TMI 06/25/2009 05:47:51 PM [End  ]
      *B608826,1 TMI 25/03/2009 [End  ]

      IF SEEK(&lcDetFile..STYLE+&lcDetFile..CWARECODE+&lcDetFile..BinLoc&lcCnt,'WHBINLOC')
        lcReplace = 'Qty&lcCnt WITH '+STR( MAX(WHBINLOC.Qty&lcCnt - &lcDetFile..Qty&lcCnt ,0) )+' '+;
          'TotQty    WITH '+STR( MAX(WHBINLOC.TotQty    - &lcDetFile..Qty&lcCnt ,0) )
        SELECT WHBINLOC
        =gfReplace(lcReplace)
        *T20071102.0018,10/C200876 TMI 10/29/2008 [Start] check the setup option to delete the bin with zero stock
        *IF TotQty = 0
        *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
        *IF TotQty = 0 .AND. gfGetMemVar('M_DELZRBNR')
        IF Qty1 = 0 AND Qty2 = 0 AND  Qty3 = 0 AND  Qty4 = 0 AND  Qty5 = 0 AND  Qty6 = 0 AND  Qty7 = 0 AND  Qty8 = 0 .AND. gfGetMemVar('M_DELZRBNR')
        *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
          *T20071102.0018,10/C200876 TMI 10/29/2008 [End  ]
          =gfDELETE()
        ENDIF
      ENDIF
    ENDIF
  ENDFOR

  *- Updating tables
  SELECT IVBINLOC
  =gfTableUpdate()

  SELECT WHBINLOC
  =gfTableUpdate()

  *B608826,1 TMI 25/03/2009 [Start] update BININVJL
  *SELECT BININVJL
  *=gfTableUpdate()
  *B608826,1 TMI 25/03/2009 [End  ]

ENDCASE

SELECT (lnSlct)
RETURN
*-- End of Function lfALSAVINV.

*:**************************************************************************
*:* Name        : lfDLARBIN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : update the BININVJL while saving an invoice
*:***************************************************************************
*:* Called from : ARINV.PRG ( ariinv, ardinv )
*:***************************************************************************
FUNCTION lfDLARBIN

*- The idea is follows :
*- IF this is a usual invoice, update the bininvjl from within the main loop in gfSaveInv as usual
*- IF this is a consolidated invoice THEN defer the update of the bininvjl after the loop completes as follows
*-                get the lines with order # is empty, from this get the styinvjl line, loop through the same style in
*-                lcDetFile and from it update the bininvjl


*- defer updating if this is an consolidated invoice line
IF &lcDetFile..CONSOL = 'Y'
  SAVE TO (oAriaApplication.WorkDir+lcDetFile+'.mem') ALL LIKE lcDetFile
  RETURN
ENDIF
*- get the conlolidated lines from lcDetFile
lcConsCur = 'Z'+SUBSTR(lcDetFile,2)
IF FILE(oAriaApplication.WorkDir+lcDetFile+'.mem')
  ERASE (oAriaApplication.WorkDir+lcDetFile+'.mem')
  SELECT * FROM &lcDetFile ;
    WHERE &lcExpr = &lcHdrFile..ACCOUNT .AND. &lcDetFile..CONSOL = 'Y' ;
    INTO CURSOR &lcConsCur
ENDIF
lcKey = &lcDetFile..STYLE+&lcDetFile..cWarecode+lcGlSession + DTOS(&lcHdrFile..InvDate)+&lcHdrFile..Invoice+STR(&lcDetFile..LINENO,6)
IF USED(lcConsCur)
  SELECT &lcConsCur
  LOCATE FOR ACCOUNT+CWARECODE+STORE+STYLE = ;
    &lcDetFile..ACCOUNT+&lcDetFile..CWARECODE+&lcDetFile..STORE+&lcDetFile..STYLE
  IF FOUND()
    *- get the consolidated line # to seek with it in STYINVJL file
    lcKey = &lcDetFile..STYLE+&lcDetFile..cWarecode+lcGlSession + DTOS(&lcHdrFile..InvDate)+&lcHdrFile..Invoice+STR(&lcConsCur..LINENO,6)
  ENDIF
ENDIF

*- Updating BININVJL sql file
IF !USED('STYINVJL')
  =gfOpenTable(oAriaApplication.DataDir+'STYINVJL','STYINVJL','SH')
ENDIF

**T20071102.0018,10/C200876 TMI 10/13/2008 [Start] get the dTrDate from the &lcHdrFile..InvDate field
**=SEEK(&lcDetFile..Style+&lcDetFile..cWarecode+lcGlSession,'STYINVJL')  && This is just to locate the line of STYINVJL to get the STYINVJL.DTRDATE field
**lcKey = &lcDetFile..Style+&lcDetFile..cWarecode+lcGlSession + DTOS(STYINVJL.DTRDATE)+&lcHdrFile..Invoice+STR(&lcDetFile..LineNo,6)
*B608826,1 TMI 21/03/2009 [Start] move this line above
*lcKey = &lcDetFile..Style+&lcDetFile..cWarecode+lcGlSession + DTOS(&lcHdrFile..InvDate)+&lcHdrFile..Invoice+STR(&lcDetFile..LineNo,6)
*B608826,1 TMI 21/03/2009 [End  ]
**T20071102.0018,10/C200876 TMI 10/13/2008 [End  ] get the dTrDate from the &lcHdrFile..InvDate field

=SEEK(lcKey,'STYINVJL')
SELECT STYINVJL
SCAN REST WHILE STYLE+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LINENO,6)= lcKey
  SCATTER MEMVAR MEMO
  =gfSEEK(&lcHdrFile..Invoice+&lcDetFile..STYLE+STR(&lcDetFile..LINENO,6)+&lcDetFile..cWareCode,'IVBINLOC')
  SELECT IVBINLOC
  SCAN REST WHILE INVOICE+STYLE+STR(LINENO,6)+CWARECODE+CLOCATION+ORDER = ;
      &lcHdrFile..Invoice+&lcDetFile..STYLE+STR(&lcDetFile..LINENO,6)+&lcDetFile..cWareCode
    *T20070214.0007 TMI 11/11/2008 [Start] change the refrence field with : "REFERENCE     WITH [" + STYINVJL.REFERENCE     + "] "+
    *lcReplace = "STYLE         WITH '" + STYINVJL.STYLE         + "' "+;
    "NSTK1         WITH " + STR(-IVBINLOC.Qty1)     + " "+;
    "NSTK2         WITH " + STR(-IVBINLOC.Qty2)     + " "+;
    "NSTK3         WITH " + STR(-IVBINLOC.Qty3)     + " "+;
    "NSTK4         WITH " + STR(-IVBINLOC.Qty4)     + " "+;
    "NSTK5         WITH " + STR(-IVBINLOC.Qty5)     + " "+;
    "NSTK6         WITH " + STR(-IVBINLOC.Qty6)     + " "+;
    "NSTK7         WITH " + STR(-IVBINLOC.Qty7)     + " "+;
    "NSTK8         WITH " + STR(-IVBINLOC.Qty8)     + " "+;
    "NTOTSTK       WITH " + STR(-IVBINLOC.TotQty)   + " "+;
    "CLOCATION     WITH '" + IVBINLOC.cLocation     + "' "+;
    "CADJACCT      WITH '" + STYINVJL.CADJACCT      + "' "+;
    "CADJREASON    WITH '" + STYINVJL.CADJREASON    + "' "+;
    "CADJREF       WITH '" + STYINVJL.CADJREF       + "' "+;
    "CDYELOT       WITH '" + STYINVJL.CDYELOT       + "' "+;
    "CICACNT       WITH '" + STYINVJL.CICACNT       + "' "+;
    "CIRTYPE       WITH '" + STYINVJL.CIRTYPE       + "' "+;
    "CISESSION     WITH '" + STYINVJL.CISESSION     + "' "+;
    "CLOTNO        WITH '" + STYINVJL.CLOTNO        + "' "+;
    "COPRCODE      WITH '" + STYINVJL.COPRCODE      + "' "+;
    "CRSESSION     WITH '" + STYINVJL.CRSESSION     + "' "+;
    "CSESSION      WITH '" + STYINVJL.CSESSION      + "' "+;
    "CTRCODE       WITH '" + STYINVJL.CTRCODE       + "' "+;
    "CTRTYPE       WITH '" + STYINVJL.CTRTYPE       + "' "+;
    "CWARECODE     WITH '" + STYINVJL.CWARECODE     + "' "+;
    "DTRDATE       WITH {^"+STR(YEAR(STYINVJL.DTRDATE),4)+"-"+STR(MONTH(STYINVJL.DTRDATE),2)+"-"+STR(DAY(STYINVJL.DTRDATE),2) +"} "+;
    "LINENO        WITH " + STR(STYINVJL.LINENO,6)  + " "+;
    "LLOCKFLG      WITH " + IIF(STYINVJL.LLOCKFLG,'.T.','.F.')  + " "+;
    "NCOST         WITH " + STR(STYINVJL.NCOST,9,2)    + " "+;
    "NPRVSQTY      WITH " + STR(STYINVJL.NPRVSQTY,9)   + " "+;
    "NPRVSVAL      WITH " + STR(STYINVJL.NPRVSVAL,12,2)+ " "+;
    "NSTKVAL       WITH " + STR(STYINVJL.NSTKVAL,12,2) + " "+;
    "REFERENCE     WITH '" + STYINVJL.REFERENCE     + "' "+;
    "CADD_TIME     WITH '" + STYINVJL.CADD_TIME     + "' "+;
    "CADD_USER     WITH '" + STYINVJL.CADD_USER     + "' "+;
    "DADD_DATE     WITH {^"+STR(YEAR(STYINVJL.DADD_DATE),4)+"-"+STR(MONTH(STYINVJL.DADD_DATE),2)+"-"+STR(DAY(STYINVJL.DADD_DATE),2) +"} "

    *B608826,1 TMI 21/03/2009 [Start] replace STYINVJL.LINENO with &lcDetFile..LINENO
    *lcReplace = "STYLE         WITH '" + STYINVJL.STYLE         + "' "+;
    "NSTK1         WITH " + STR(-IVBINLOC.Qty1)     + " "+;
    "NSTK2         WITH " + STR(-IVBINLOC.Qty2)     + " "+;
    "NSTK3         WITH " + STR(-IVBINLOC.Qty3)     + " "+;
    "NSTK4         WITH " + STR(-IVBINLOC.Qty4)     + " "+;
    "NSTK5         WITH " + STR(-IVBINLOC.Qty5)     + " "+;
    "NSTK6         WITH " + STR(-IVBINLOC.Qty6)     + " "+;
    "NSTK7         WITH " + STR(-IVBINLOC.Qty7)     + " "+;
    "NSTK8         WITH " + STR(-IVBINLOC.Qty8)     + " "+;
    "NTOTSTK       WITH " + STR(-IVBINLOC.TotQty)   + " "+;
    "CLOCATION     WITH '" + IVBINLOC.cLocation     + "' "+;
    "CADJACCT      WITH '" + STYINVJL.CADJACCT      + "' "+;
    "CADJREASON    WITH '" + STYINVJL.CADJREASON    + "' "+;
    "CADJREF       WITH '" + STYINVJL.CADJREF       + "' "+;
    "CDYELOT       WITH '" + STYINVJL.CDYELOT       + "' "+;
    "CICACNT       WITH '" + STYINVJL.CICACNT       + "' "+;
    "CIRTYPE       WITH '" + STYINVJL.CIRTYPE       + "' "+;
    "CISESSION     WITH '" + STYINVJL.CISESSION     + "' "+;
    "CLOTNO        WITH '" + STYINVJL.CLOTNO        + "' "+;
    "COPRCODE      WITH '" + STYINVJL.COPRCODE      + "' "+;
    "CRSESSION     WITH '" + STYINVJL.CRSESSION     + "' "+;
    "CSESSION      WITH '" + STYINVJL.CSESSION      + "' "+;
    "CTRCODE       WITH '" + STYINVJL.CTRCODE       + "' "+;
    "CTRTYPE       WITH '" + STYINVJL.CTRTYPE       + "' "+;
    "CWARECODE     WITH '" + STYINVJL.CWARECODE     + "' "+;
    "DTRDATE       WITH {^"+STR(YEAR(STYINVJL.DTRDATE),4)+"-"+STR(MONTH(STYINVJL.DTRDATE),2)+"-"+STR(DAY(STYINVJL.DTRDATE),2) +"} "+;
    "LINENO        WITH " + STR(STYINVJL.LINENO,6)  + " "+;
    "LLOCKFLG      WITH " + IIF(STYINVJL.LLOCKFLG,'.T.','.F.')  + " "+;
    "NCOST         WITH " + STR(STYINVJL.NCOST,9,2)    + " "+;
    "NPRVSQTY      WITH " + STR(STYINVJL.NPRVSQTY,9)   + " "+;
    "NPRVSVAL      WITH " + STR(STYINVJL.NPRVSVAL,12,2)+ " "+;
    "NSTKVAL       WITH " + STR(STYINVJL.NSTKVAL,12,2) + " "+;
    "REFERENCE     WITH [" + STYINVJL.REFERENCE     + "] "+;
    "CADD_TIME     WITH '" + STYINVJL.CADD_TIME     + "' "+;
    "CADD_USER     WITH '" + STYINVJL.CADD_USER     + "' "+;
    "DADD_DATE     WITH {^"+STR(YEAR(STYINVJL.DADD_DATE),4)+"-"+STR(MONTH(STYINVJL.DADD_DATE),2)+"-"+STR(DAY(STYINVJL.DADD_DATE),2) +"} "
    lcReplace = "STYLE         WITH '" + STYINVJL.STYLE         + "' "+;
      "NSTK1         WITH " + STR(-IVBINLOC.Qty1)     + " "+;
      "NSTK2         WITH " + STR(-IVBINLOC.Qty2)     + " "+;
      "NSTK3         WITH " + STR(-IVBINLOC.Qty3)     + " "+;
      "NSTK4         WITH " + STR(-IVBINLOC.Qty4)     + " "+;
      "NSTK5         WITH " + STR(-IVBINLOC.Qty5)     + " "+;
      "NSTK6         WITH " + STR(-IVBINLOC.Qty6)     + " "+;
      "NSTK7         WITH " + STR(-IVBINLOC.Qty7)     + " "+;
      "NSTK8         WITH " + STR(-IVBINLOC.Qty8)     + " "+;
      "NTOTSTK       WITH " + STR(-IVBINLOC.TotQty)   + " "+;
      "CLOCATION     WITH '" + IVBINLOC.cLocation     + "' "+;
      "CADJACCT      WITH '" + STYINVJL.CADJACCT      + "' "+;
      "CADJREASON    WITH '" + STYINVJL.CADJREASON    + "' "+;
      "CADJREF       WITH '" + STYINVJL.CADJREF       + "' "+;
      "CDYELOT       WITH '" + STYINVJL.CDYELOT       + "' "+;
      "CICACNT       WITH '" + STYINVJL.CICACNT       + "' "+;
      "CIRTYPE       WITH '" + STYINVJL.CIRTYPE       + "' "+;
      "CISESSION     WITH '" + STYINVJL.CISESSION     + "' "+;
      "CLOTNO        WITH '" + STYINVJL.CLOTNO        + "' "+;
      "COPRCODE      WITH '" + STYINVJL.COPRCODE      + "' "+;
      "CRSESSION     WITH '" + STYINVJL.CRSESSION     + "' "+;
      "CSESSION      WITH '" + STYINVJL.CSESSION      + "' "+;
      "CTRCODE       WITH '" + STYINVJL.CTRCODE       + "' "+;
      "CTRTYPE       WITH '" + STYINVJL.CTRTYPE       + "' "+;
      "CWARECODE     WITH '" + STYINVJL.CWARECODE     + "' "+;
      "DTRDATE       WITH {^"+STR(YEAR(STYINVJL.DTRDATE),4)+"-"+STR(MONTH(STYINVJL.DTRDATE),2)+"-"+STR(DAY(STYINVJL.DTRDATE),2) +"} "+;
      "LINENO        WITH " + STR(&lcDetFile..LINENO,6)  + " "+;
      "LLOCKFLG      WITH " + IIF(STYINVJL.LLOCKFLG,'.T.','.F.')  + " "+;
      "NCOST         WITH " + STR(STYINVJL.NCOST,9,2)    + " "+;
      "NPRVSQTY      WITH " + STR(STYINVJL.NPRVSQTY,9)   + " "+;
      "NPRVSVAL      WITH " + STR(STYINVJL.NPRVSVAL,12,2)+ " "+;
      "NSTKVAL       WITH " + STR(STYINVJL.NSTKVAL,12,2) + " "+;
      "REFERENCE     WITH [" + STYINVJL.REFERENCE     + "] "+;
      "CADD_TIME     WITH '" + STYINVJL.CADD_TIME     + "' "+;
      "CADD_USER     WITH '" + STYINVJL.CADD_USER     + "' "+;
      "DADD_DATE     WITH {^"+STR(YEAR(STYINVJL.DADD_DATE),4)+"-"+STR(MONTH(STYINVJL.DADD_DATE),2)+"-"+STR(DAY(STYINVJL.DADD_DATE),2) +"} "
    *B608826,1 TMI 21/03/2009 [End  ]
    *T20070214.0007 TMI 11/11/2008 [End  ]

    SELECT BININVJL
    *B608826,1 TMI 22/03/2009 [Start] use the order "WHBINSTY"
    =gfSetOrder('WHBINSTY')
    IF !gfSeek(STYINVJL.CWARECODE+IVBINLOC.CLOCATION+STYINVJL.STYLE+STYINVJL.CTRCODE+STR(&lcDetFile..LINENO,6)+;
        'I'+STYINVJL.CSESSION+STYINVJL.CRSESSION+DTOS(STYINVJL.DTRDATE),'BININVJL')
      *B608826,1 TMI 22/03/2009 [End  ]
      =gfAppend()
      =gfReplace(lcReplace)
      *B608826,1 TMI 22/03/2009 [Start] If line found before update it from pkbinloc.qty's fields
    ELSE
      =gfSEEK(&lcDetFile..PIKTKT+&lcDetFile..CWARECODE+STR(&lcDetFile..LINENO,6)+&lcDetFile..STYLE,'PKBINLOC')
      lcReplace = "NSTK1         WITH NSTK1 + " + STR(-PKBINLOC.Qty1)     + " "+;
        "NSTK2         WITH NSTK2 + " + STR(-PKBINLOC.Qty2)     + " "+;
        "NSTK3         WITH NSTK3 + " + STR(-PKBINLOC.Qty3)     + " "+;
        "NSTK4         WITH NSTK4 + " + STR(-PKBINLOC.Qty4)     + " "+;
        "NSTK5         WITH NSTK5 + " + STR(-PKBINLOC.Qty5)     + " "+;
        "NSTK6         WITH NSTK6 + " + STR(-PKBINLOC.Qty6)     + " "+;
        "NSTK7         WITH NSTK7 + " + STR(-PKBINLOC.Qty7)     + " "+;
        "NSTK8         WITH NSTK8 + " + STR(-PKBINLOC.Qty8)     + " "+;
        "NTOTSTK       WITH NTOTSTK+" + STR(-PKBINLOC.TotQty)
      =gfReplace(lcReplace)
    ENDIF
    *B608826,1 TMI 22/03/2009 [End  ]
  ENDSCAN
ENDSCAN

*- Update the bin journal table
SELECT BININVJL
=gfTableUpdate()

*-- end of lfDLARBIN.


*:**************************************************************************
*:* Name        : lfBinInvJLUpdate
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 25/03/2009
*:* Purpose     : updating the BININVJL
*:***************************************************************************
*:* Called from : lfALSAVINV
* this function is used instead of lfDLARBIN
*:***************************************************************************
*B608826,1 TMI 25/03/2009
FUNCTION lfBinInvJLUpdate
*B608898,3 TMI 06/25/2009 05:45:23 PM [Start] accepts the clocation as a parameter
*LPARAMETERS lcKey,laQty
LPARAMETERS lcKey,laQty,lcLocation
*B608898,3 TMI 06/25/2009 05:45:43 PM [End  ]

*B610658,1 TMI 01/19/2014 16:58 [Start] define the original qty array
dime laOrgQty[9] 
=acopy(laQty,laOrgQty)
*B610658,1 TMI 01/19/2014 16:58 [End  ] 

=SEEK(lcKey,'STYINVJL')
SELECT STYINVJL
*B610658,2 TMI 01/21/2014 19:36 [Start] use lcTmpStyinvjl instead
SELECT (lcTmpStyinvjl)
LOCATE 
*B610658,2 TMI 01/21/2014 19:36 [End  ] 
SCAN REST WHILE STYLE+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LINENO,6)= lcKey
  SCATTER MEMVAR MEMO
  
  *B610658,1 TMI 01/19/2014 16:58 [Start] check if we still able to update with qty, if either stk or orgqty is 0 then abort
  IF nstk1+nstk2+nstk3+nstk4+nstk5+nstk6+nstk7+nstk8= 0 OR laOrgQty[1]+laOrgQty[2]+laOrgQty[3]+laOrgQty[4]+laOrgQty[5]+laOrgQty[6]+laOrgQty[7]+laOrgQty[8]=0
    LOOP
  ENDIF 
  local lnI,lcI
  laQty[9] = 0
  for lnI=1 to 8
    lcI = str(lnI,1)
    lnQ = min(laOrgQty[lnI],nstk&lcI)
    laQty[lnI] = lnQ
    laQty[9] = laQty[9] + lnQ
    replace nstk&lcI with nstk&lcI - lnQ
    laOrgQty[lnI] = laOrgQty[lnI] - lnQ
  ENDFOR
  *B610658,1 TMI 01/19/2014 16:58 [End  ] 
    
  *B608898,3 TMI 06/25/2009 05:45:55 PM [Start] use the lcLocation instead of PKBINLOC.CLOCATION
  *lcReplace = "STYLE         WITH '" + STYINVJL.STYLE    + "' "+;
  "NSTK1         WITH " + STR(-laQty[1])     + " "+;
  "NSTK2         WITH " + STR(-laQty[2])     + " "+;
  "NSTK3         WITH " + STR(-laQty[3])     + " "+;
  "NSTK4         WITH " + STR(-laQty[4])     + " "+;
  "NSTK5         WITH " + STR(-laQty[5])     + " "+;
  "NSTK6         WITH " + STR(-laQty[6])     + " "+;
  "NSTK7         WITH " + STR(-laQty[7])     + " "+;
  "NSTK8         WITH " + STR(-laQty[8])     + " "+;
  "NTOTSTK       WITH " + STR(-laQty[9])     + " "+;
  "CLOCATION     WITH '" + PKBINLOC.cLocation     + "' "+;
  "CADJACCT      WITH '" + STYINVJL.CADJACCT      + "' "+;
  "CADJREASON    WITH '" + STYINVJL.CADJREASON    + "' "+;
  "CADJREF       WITH '" + STYINVJL.CADJREF       + "' "+;
  "CDYELOT       WITH '" + STYINVJL.CDYELOT       + "' "+;
  "CICACNT       WITH '" + STYINVJL.CICACNT       + "' "+;
  "CIRTYPE       WITH '" + STYINVJL.CIRTYPE       + "' "+;
  "CISESSION     WITH '" + STYINVJL.CISESSION     + "' "+;
  "CLOTNO        WITH '" + STYINVJL.CLOTNO        + "' "+;
  "COPRCODE      WITH '" + STYINVJL.COPRCODE      + "' "+;
  "CRSESSION     WITH '" + STYINVJL.CRSESSION     + "' "+;
  "CSESSION      WITH '" + STYINVJL.CSESSION      + "' "+;
  "CTRCODE       WITH '" + STYINVJL.CTRCODE       + "' "+;
  "CTRTYPE       WITH '" + STYINVJL.CTRTYPE       + "' "+;
  "CWARECODE     WITH '" + STYINVJL.CWARECODE     + "' "+;
  "DTRDATE       WITH {^"+STR(YEAR(STYINVJL.DTRDATE),4)+"-"+STR(MONTH(STYINVJL.DTRDATE),2)+"-"+STR(DAY(STYINVJL.DTRDATE),2) +"} "+;
  "LINENO        WITH " + STR(&lcDetFile..LINENO,6)  + " "+;
  "LLOCKFLG      WITH " + IIF(STYINVJL.LLOCKFLG,'.T.','.F.')  + " "+;
  "NCOST         WITH " + STR(STYINVJL.NCOST,9,2)    + " "+;
  "NPRVSQTY      WITH " + STR(STYINVJL.NPRVSQTY,9)   + " "+;
  "NPRVSVAL      WITH " + STR(STYINVJL.NPRVSVAL,12,2)+ " "+;
  "NSTKVAL       WITH " + STR(STYINVJL.NSTKVAL,12,2) + " "+;
  "REFERENCE     WITH [" + STYINVJL.REFERENCE     + "] "+;
  "CADD_TIME     WITH '" + STYINVJL.CADD_TIME     + "' "+;
  "CADD_USER     WITH '" + STYINVJL.CADD_USER     + "' "+;
  "DADD_DATE     WITH {^"+STR(YEAR(STYINVJL.DADD_DATE),4)+"-"+STR(MONTH(STYINVJL.DADD_DATE),2)+"-"+STR(DAY(STYINVJL.DADD_DATE),2) +"} "
  *B608939,1 TMI 07/15/2009 02:44:12 PM [Start] use styinvjl.lineno instead of &lcDetfile..lineno
  *lcReplace = "STYLE         WITH '" + STYINVJL.STYLE    + "' "+;
  "NSTK1         WITH " + STR(-laQty[1])     + " "+;
  "NSTK2         WITH " + STR(-laQty[2])     + " "+;
  "NSTK3         WITH " + STR(-laQty[3])     + " "+;
  "NSTK4         WITH " + STR(-laQty[4])     + " "+;
  "NSTK5         WITH " + STR(-laQty[5])     + " "+;
  "NSTK6         WITH " + STR(-laQty[6])     + " "+;
  "NSTK7         WITH " + STR(-laQty[7])     + " "+;
  "NSTK8         WITH " + STR(-laQty[8])     + " "+;
  "NTOTSTK       WITH " + STR(-laQty[9])     + " "+;
  "CLOCATION     WITH '" + lcLocation     + "' "+;
  "CADJACCT      WITH '" + STYINVJL.CADJACCT      + "' "+;
  "CADJREASON    WITH '" + STYINVJL.CADJREASON    + "' "+;
  "CADJREF       WITH '" + STYINVJL.CADJREF       + "' "+;
  "CDYELOT       WITH '" + STYINVJL.CDYELOT       + "' "+;
  "CICACNT       WITH '" + STYINVJL.CICACNT       + "' "+;
  "CIRTYPE       WITH '" + STYINVJL.CIRTYPE       + "' "+;
  "CISESSION     WITH '" + STYINVJL.CISESSION     + "' "+;
  "CLOTNO        WITH '" + STYINVJL.CLOTNO        + "' "+;
  "COPRCODE      WITH '" + STYINVJL.COPRCODE      + "' "+;
  "CRSESSION     WITH '" + STYINVJL.CRSESSION     + "' "+;
  "CSESSION      WITH '" + STYINVJL.CSESSION      + "' "+;
  "CTRCODE       WITH '" + STYINVJL.CTRCODE       + "' "+;
  "CTRTYPE       WITH '" + STYINVJL.CTRTYPE       + "' "+;
  "CWARECODE     WITH '" + STYINVJL.CWARECODE     + "' "+;
  "DTRDATE       WITH {^"+STR(YEAR(STYINVJL.DTRDATE),4)+"-"+STR(MONTH(STYINVJL.DTRDATE),2)+"-"+STR(DAY(STYINVJL.DTRDATE),2) +"} "+;
  "LINENO        WITH " + STR(&lcDetFile..LINENO,6)  + " "+;
  "LLOCKFLG      WITH " + IIF(STYINVJL.LLOCKFLG,'.T.','.F.')  + " "+;
  "NCOST         WITH " + STR(STYINVJL.NCOST,9,2)    + " "+;
  "NPRVSQTY      WITH " + STR(STYINVJL.NPRVSQTY,9)   + " "+;
  "NPRVSVAL      WITH " + STR(STYINVJL.NPRVSVAL,12,2)+ " "+;
  "NSTKVAL       WITH " + STR(STYINVJL.NSTKVAL,12,2) + " "+;
  "REFERENCE     WITH [" + STYINVJL.REFERENCE     + "] "+;
  "CADD_TIME     WITH '" + STYINVJL.CADD_TIME     + "' "+;
  "CADD_USER     WITH '" + STYINVJL.CADD_USER     + "' "+;
  "DADD_DATE     WITH {^"+STR(YEAR(STYINVJL.DADD_DATE),4)+"-"+STR(MONTH(STYINVJL.DADD_DATE),2)+"-"+STR(DAY(STYINVJL.DADD_DATE),2) +"} "
  
  *E303439,2 TMI 01/22/2014 11:52 [Start] replace the alias Styinvjl with the temp alias lcTmpStyinvjl  
  *lcReplace = "STYLE         WITH '" + STYINVJL.STYLE    + "' "+;
  *  "NSTK1         WITH " + STR(-laQty[1])     + " "+;
  *  "NSTK2         WITH " + STR(-laQty[2])     + " "+;
  *  "NSTK3         WITH " + STR(-laQty[3])     + " "+;
  *  "NSTK4         WITH " + STR(-laQty[4])     + " "+;
  *  "NSTK5         WITH " + STR(-laQty[5])     + " "+;
  *  "NSTK6         WITH " + STR(-laQty[6])     + " "+;
  *  "NSTK7         WITH " + STR(-laQty[7])     + " "+;
  *  "NSTK8         WITH " + STR(-laQty[8])     + " "+;
  *  "NTOTSTK       WITH " + STR(-laQty[9])     + " "+;
  *  "CLOCATION     WITH '" + lcLocation     + "' "+;
  *  "CADJACCT      WITH '" + STYINVJL.CADJACCT      + "' "+;
  *  "CADJREASON    WITH '" + STYINVJL.CADJREASON    + "' "+;
  *  "CADJREF       WITH '" + STYINVJL.CADJREF       + "' "+;
  *  "CDYELOT       WITH '" + STYINVJL.CDYELOT       + "' "+;
  *  "CICACNT       WITH '" + STYINVJL.CICACNT       + "' "+;
  *  "CIRTYPE       WITH '" + STYINVJL.CIRTYPE       + "' "+;
  *  "CISESSION     WITH '" + STYINVJL.CISESSION     + "' "+;
  *  "CLOTNO        WITH '" + STYINVJL.CLOTNO        + "' "+;
  *  "COPRCODE      WITH '" + STYINVJL.COPRCODE      + "' "+;
  *  "CRSESSION     WITH '" + STYINVJL.CRSESSION     + "' "+;
  *  "CSESSION      WITH '" + STYINVJL.CSESSION      + "' "+;
  *  "CTRCODE       WITH '" + STYINVJL.CTRCODE       + "' "+;
  *  "CTRTYPE       WITH '" + STYINVJL.CTRTYPE       + "' "+;
  *  "CWARECODE     WITH '" + STYINVJL.CWARECODE     + "' "+;
  *  "DTRDATE       WITH {^"+STR(YEAR(STYINVJL.DTRDATE),4)+"-"+STR(MONTH(STYINVJL.DTRDATE),2)+"-"+STR(DAY(STYINVJL.DTRDATE),2) +"} "+;
  *  "LINENO        WITH " + STR(STYINVJL.LINENO,6)  + " "+;
  *  "LLOCKFLG      WITH " + IIF(STYINVJL.LLOCKFLG,'.T.','.F.')  + " "+;
  *  "NCOST         WITH " + STR(STYINVJL.NCOST,9,2)    + " "+;
  *  "NPRVSQTY      WITH " + STR(STYINVJL.NPRVSQTY,9)   + " "+;
  *  "NPRVSVAL      WITH " + STR(STYINVJL.NPRVSVAL,12,2)+ " "+;
  *  "NSTKVAL       WITH " + STR(STYINVJL.NSTKVAL,12,2) + " "+;
  *  "REFERENCE     WITH [" + STYINVJL.REFERENCE     + "] "+;
  *  "CADD_TIME     WITH '" + STYINVJL.CADD_TIME     + "' "+;
  *  "CADD_USER     WITH '" + STYINVJL.CADD_USER     + "' "+;
  *  "DADD_DATE     WITH {^"+STR(YEAR(STYINVJL.DADD_DATE),4)+"-"+STR(MONTH(STYINVJL.DADD_DATE),2)+"-"+STR(DAY(STYINVJL.DADD_DATE),2) +"} "
  lcReplace = "STYLE         WITH '" + &lcTmpStyinvjl..STYLE    + "' "+;
    "NSTK1         WITH " + STR(-laQty[1])     + " "+;
    "NSTK2         WITH " + STR(-laQty[2])     + " "+;
    "NSTK3         WITH " + STR(-laQty[3])     + " "+;
    "NSTK4         WITH " + STR(-laQty[4])     + " "+;
    "NSTK5         WITH " + STR(-laQty[5])     + " "+;
    "NSTK6         WITH " + STR(-laQty[6])     + " "+;
    "NSTK7         WITH " + STR(-laQty[7])     + " "+;
    "NSTK8         WITH " + STR(-laQty[8])     + " "+;
    "NTOTSTK       WITH " + STR(-laQty[9])     + " "+;
    "CLOCATION     WITH '" + lcLocation     + "' "+;
    "CADJACCT      WITH '" + &lcTmpStyinvjl..CADJACCT      + "' "+;
    "CADJREASON    WITH '" + &lcTmpStyinvjl..CADJREASON    + "' "+;
    "CADJREF       WITH '" + &lcTmpStyinvjl..CADJREF       + "' "+;
    "CDYELOT       WITH '" + &lcTmpStyinvjl..CDYELOT       + "' "+;
    "CICACNT       WITH '" + &lcTmpStyinvjl..CICACNT       + "' "+;
    "CIRTYPE       WITH '" + &lcTmpStyinvjl..CIRTYPE       + "' "+;
    "CISESSION     WITH '" + &lcTmpStyinvjl..CISESSION     + "' "+;
    "CLOTNO        WITH '" + &lcTmpStyinvjl..CLOTNO        + "' "+;
    "COPRCODE      WITH '" + &lcTmpStyinvjl..COPRCODE      + "' "+;
    "CRSESSION     WITH '" + &lcTmpStyinvjl..CRSESSION     + "' "+;
    "CSESSION      WITH '" + &lcTmpStyinvjl..CSESSION      + "' "+;
    "CTRCODE       WITH '" + &lcTmpStyinvjl..CTRCODE       + "' "+;
    "CTRTYPE       WITH '" + &lcTmpStyinvjl..CTRTYPE       + "' "+;
    "CWARECODE     WITH '" + &lcTmpStyinvjl..CWARECODE     + "' "+;
    "DTRDATE       WITH {^"+STR(YEAR(&lcTmpStyinvjl..DTRDATE),4)+"-"+STR(MONTH(&lcTmpStyinvjl..DTRDATE),2)+"-"+STR(DAY(&lcTmpStyinvjl..DTRDATE),2) +"} "+;
    "LINENO        WITH " + STR(&lcTmpStyinvjl..LINENO,6)  + " "+;
    "LLOCKFLG      WITH " + IIF(&lcTmpStyinvjl..LLOCKFLG,'.T.','.F.')  + " "+;
    "NCOST         WITH " + STR(&lcTmpStyinvjl..NCOST,9,2)    + " "+;
    "NPRVSQTY      WITH " + STR(&lcTmpStyinvjl..NPRVSQTY,9)   + " "+;
    "NPRVSVAL      WITH " + STR(&lcTmpStyinvjl..NPRVSVAL,12,2)+ " "+;
    "NSTKVAL       WITH " + STR(&lcTmpStyinvjl..NSTKVAL,12,2) + " "+;
    "REFERENCE     WITH [" + &lcTmpStyinvjl..REFERENCE     + "] "+;
    "CADD_TIME     WITH '" + &lcTmpStyinvjl..CADD_TIME     + "' "+;
    "CADD_USER     WITH '" + &lcTmpStyinvjl..CADD_USER     + "' "+;
    "DADD_DATE     WITH {^"+STR(YEAR(&lcTmpStyinvjl..DADD_DATE),4)+"-"+STR(MONTH(&lcTmpStyinvjl..DADD_DATE),2)+"-"+STR(DAY(&lcTmpStyinvjl..DADD_DATE),2) +"} "
  *B608939,2 TMI 07/15/2009 02:45:09 PM [End  ]
  *B608898,3 TMI 06/25/2009 05:46:07 PM [End  ]

  SELECT BININVJL
  *B608898,3 TMI 06/25/2009 05:46:37 PM [Start] use the lcLocation instead of PKBINLOC.CLOCATION
  *IF !gfSEEK(STYINVJL.CWARECODE+PKBINLOC.CLOCATION+STYINVJL.STYLE+STYINVJL.CTRCODE+STR(&lcDetFile..LINENO,6)+;
  'I'+STYINVJL.CSESSION+STYINVJL.CRSESSION+DTOS(STYINVJL.DTRDATE),'BININVJL')
  *B608939,1 TMI 07/15/2009 02:45:53 PM [Start] use styinvjl.lineno instead of &lcdetfile..lineno
  *B610658,2 TMI 01/22/2014 16:45 [Start] 
  *IF !gfSEEK(STYINVJL.CWARECODE+lcLocation+STYINVJL.STYLE+STYINVJL.CTRCODE+STR(&lcDetFile..LINENO,6)+;
  'I'+STYINVJL.CSESSION+STYINVJL.CRSESSION+DTOS(STYINVJL.DTRDATE),'BININVJL')
  IF !gfSEEK(&lcTmpStyinvjl..CWARECODE+lcLocation+&lcTmpStyinvjl..STYLE+&lcTmpStyinvjl..CTRCODE+STR(&lcTmpStyinvjl..LINENO,6)+;
      'I'+&lcTmpStyinvjl..CSESSION+&lcTmpStyinvjl..CRSESSION+DTOS(&lcTmpStyinvjl..DTRDATE),'BININVJL')
    *B610658,2 TMI 01/22/2014 16:45 [End  ] 
    *B608939,1 TMI 07/15/2009 02:46:02 PM [End  ]
    *B608898,3 TMI 06/25/2009 05:47:03 PM [End  ]
    =gfAppend()
    =gfReplace(lcReplace)
  ELSE
    lcReplace = "NSTK1         WITH NSTK1 + " + STR(-laQty[1])     + " "+;
      "NSTK2         WITH NSTK2 + " + STR(-laQty[2])     + " "+;
      "NSTK3         WITH NSTK3 + " + STR(-laQty[3])     + " "+;
      "NSTK4         WITH NSTK4 + " + STR(-laQty[4])     + " "+;
      "NSTK5         WITH NSTK5 + " + STR(-laQty[5])     + " "+;
      "NSTK6         WITH NSTK6 + " + STR(-laQty[6])     + " "+;
      "NSTK7         WITH NSTK7 + " + STR(-laQty[7])     + " "+;
      "NSTK8         WITH NSTK8 + " + STR(-laQty[8])     + " "+;
      "NTOTSTK       WITH NTOTSTK+" + STR(-laQty[9])
    =gfReplace(lcReplace)
  ENDIF

  SELECT BININVJL
  =gfTableUpdate()

ENDSCAN
*-- end of lfBinInvJLUpdate.

*!***************************************************************************
*!* Name        : lfUseBnLoc
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : System Manager (SM)
*!* Purpose     : Disable [Use Bin Location] setting if it is 'Y' (to privent
*!*             : switching from Yes to No). that if there is any transaction
*!*             : in the Bininvjl file.
*!***************************************************************************
*!* Called from : IC.PRG (IC Application) with the Company Setup
*!***************************************************************************
*!* Calls       :
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUseBnLoc()
*!***************************************************************************
FUNCTION lfUseBnLoc

PRIVATE lnRecCount
IF !USED('BININVJL')
  =gfOpenTable(oAriaApplication.DataDir+'BININVJL','Styinvjl','SH')
ENDIF
IF USED('BININVJL')
  COUNT TO lnRecCount
ENDIF
IF lnRecCount = 0
  RETURN
ELSE
  IF gfGetMemVar('M_WARELOC')="N"
    =lfOGShowGet('M_DLUSEBIN',M_DLUSEBIN=.T.)
  ELSE
    IF gfGetMemVar('M_DLUSEBIN')
      =lfOGShowGet('M_WARELOC',M_WARELOC="N")
      =lfOGShowGet('M_DLUSEBIN',M_DLUSEBIN=.F.)
    ENDIF
  ENDIF
ENDIF
IF USED('BININVJL')
  USE IN BININVJL
ENDIF
*-- End of Function lfUseBnLoc.

*!***************************************************************************
*!* Name        : lfADDSTYBN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add new option called (Add bin to batch file) at the inventory
*!*             : Locking screen.
*!***************************************************************************
*!* Called from : ICINVLK.PRG --> lpMainProc.
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfADDSTYBN()
*!***************************************************************************
FUNCTION lfADDSTYBN

PRIVATE lnBarNo,llfound

IF loFormSet.llMatModul
  RETURN
ENDIF

IF !lfIsUseBin()
  RETURN
ENDIF


*--check if the option pad is already defined on the sysmenu
llFound = .F.
FOR lnCount = 1 TO CNTBAR('_OPTIONPOP')
  IF PRMBAR('_OPTIONPOP', LnCount) = 'Add bin to batch file'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  lnBarNo = CNTBAR('_OPTIONPOP') + 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT '\<Add bin to batch file' SKIP FOR ;
    _SCREEN.ACTIVEFORM.PARENT.ActiveMode<>'E'
  *T20071102.0018,10/C200876 TMI 06/03/2008 [Start]
  *ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfAdStyScr IN BN4MAIN
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfAdStyScr IN BN4MAIN WITH _SCREEN.ACTIVEFORM.PARENT  
  *T20071102.0018,10/C200876 TMI 06/03/2008 [End  ]

ENDIF

*-- End of Function lfADDSTYBN.

*!***************************************************************************
*!* Name        : lfAdStyScr
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Open a new Screen to Add a new Style/Bin to the Inventory
*!*             : Locking Batch file and therefore the whbinloc file
*!***************************************************************************
*!* Called from : BN4MAIN.prg --> lfADDSTYBN
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAdStyScr()
*!***************************************************************************
FUNCTION lfAdStyScr
PARAMETERS loFormSet


SET DATASESSION TO (loFormSet.DATASESSIONID)

STORE .F. TO llNewStyle,llbrowse
*T20071102.0018,10/C200876 TMI 06/03/2008 [Start]
*lcWareh = laData[8]
lcWareh = loFormSet.Ariaform1.kbLocation.Keytextbox.VALUE
*T20071102.0018,10/C200876 TMI 06/03/2008 [End  ]

IF !lfOpnFiles("WHSLOC,WHBINLOC","WHSLOC,WHBINLOC","")
  RETURN
ENDIF

lcBatLin = loFormSet.lcBatLin
lcBatInd = loFormSet.lcBatInd
llDyelot = loFormSet.llDyelot


*T20071102.0018,10/C200876 TMI 05/20/2008 [Start]
lcTmpQuery = loFormSet.lcTmpQuery
lcKey = 'S'
*T20071102.0018,10/C200876 TMI 05/20/2008 [End  ]


*T20071102.0018,10/C200876 TMI 06/03/2008 [Start]
*IF llLinkGL
IF loFormSet.llLinkGL
  *T20071102.0018,10/C200876 TMI 06/03/2008 [End  ]
  lnoldals = SELECT(0)
  IF !USED('CODES')
    =gfOpenTable(oAriaApplication.DataDir+'CODES','CODES','SH')   && CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME
  ENDIF
  SELECT CODES
  lcCodTag=TAG()
  SET ORDER TO TAG cCode_no
  IF !SEEK("N"+'CADJREASON','CODES')
    *--You have to edit the Adjustment reasons codes first, Cannot proceed.
    =gfModalGen('TRM42111B42001','DIALOG')
    SET ORDER TO TAG &lcCodTag
    *B610350,1 TMI 06/02/2013 [Start]  close tables
    *=lfCloseTbl('WHSLOC')
    =lfCloseTbl('WHBINLOC')
    *=lfCloseTbl('CODES')
    *B610350,1 TMI 06/02/2013 [End  ] 
    SELECT &lnoldals
    RETURN
  ELSE
    =SEEK("D"+'CADJREASON','CODES')
    lcDefAdjCd = Codes.Ccode_No
  ENDIF
  SET ORDER TO TAG &lcCodTag
  SELECT(lnoldals)
ENDIF

IF !USED(loFormSet.lcBatLin)
  TRY
    =lfTempFile(loFormSet)  && Run this from ICINVLCK.PRG
  CATCH
  ENDTRY
ENDIF

SELECT (loFormSet.lcBatLin)

SET ORDER TO (loFormset.lcBatind)
*T20071102.0018,10/C200876 TMI 06/03/2008 [Start]
*=SEEK(lcKey+laData[2]+&lcTmpQuery..Style+&lcTmpQuery..Color)
lcBatchNo = loFormSet.Ariaform1.KbBatchNo.Keytextbox.VALUE
=SEEK(lcKey+lcBatchNo+&lcTmpQuery..STYLE+&lcTmpQuery..COLOR)
*T20071102.0018,10/C200876 TMI 06/03/2008 [End  ]
lcSty = STYLE
lcClr = COLOR
=SEEK(lcSty,'Style')
llStyDye = (STYLE.cDye_Flg='Y')
SELECT (lcBatLin)
SET ORDER TO lcBatInd
*T20071102.0018,10/C200876 TMI 06/03/2008 [Start]
*=SEEK(lcKey+laData[2]+lcSty+lcClr)
=SEEK(lcKey+lcBatchNo+lcSty+lcClr)
*T20071102.0018,10/C200876 TMI 06/03/2008 [End  ]
IF llDyelot AND llStyDye
  *T20071102.0018,10/C200876 TMI 06/03/2008 [Start]
  *LOCATE REST WHILE cbattype+cLkBatch+style+color+DYELOT = lcKey+laData[2]+lcSty+lcClr FOR !EMPTY(DYELOT)
  LOCATE REST WHILE cbattype+cLkBatch+STYLE+COLOR+DYELOT = lcKey+lcBatchNo+lcSty+lcClr FOR !EMPTY(DYELOT)
  *T20071102.0018,10/C200876 TMI 06/03/2008 [End  ]
  IF FOUND()
    lcDyelot = DYELOT
  ELSE
    *T20071102.0018,10/C200876 TMI 06/03/2008 [Start]
    *=SEEK(lcKey+laData[2]+lcSty+lcClr)
    =SEEK(lcKey+lcBatchNo+lcSty+lcClr)
    *T20071102.0018,10/C200876 TMI 06/03/2008 [End  ]
  ENDIF
ENDIF
SELECT (lcBatLin)
lnLoc = IIF(ASCAN(laMloc,cLocation)<>0,ASCAN(laMloc,cLocation),1)
IF SEEK('S'+SCALE,'Scale')
  FOR I = 1 TO 8
    Z = STR (I,1)
    lcSz&Z = SCALE.Sz&Z
  ENDFOR
ENDIF
FOR I = 1 TO 8
  Z = STR (I,1)
  lnStk&Z = Stk&Z
ENDFOR
lnMCost   = COST
lnOldCost = OldCOST
lcReason  = cReason
lcStyDesc = IIF(SEEK(lcSty,'Style'),STYLE.Desc1,'')
lnNewTot  = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
lcLocBin = &lcTmpQuery..Bin
lcMDBrowTt = 'Detail '+lcStyPic

SELECT *,.F. AS llnew FROM (lcBatLin) WHERE STYLE = lcSty AND cLocation = lcLocBin ;
  AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (oAriaApplication.WorkDir + lcDetLin)
INDEX ON STYLE+COLOR+Dyelot+cLocation TAG (lcDetLin)

FOR I = 1 TO 8
  Z = STR (I,1)
  lnStk&Z = Stk&Z
ENDFOR
IF _TALLY = 0
  lcMes = lcStyPic +' '+ lcSty
  =gfModalGen('TRM42210B00000','DIALOG',lcMes)
    *B610350,1 TMI 06/02/2013 [Start]  close tables
    *=lfCloseTbl('WHSLOC')
    =lfCloseTbl('WHBINLOC')
    *=lfCloseTbl('CODES')
    *B610350,1 TMI 06/02/2013 [End  ] 
  RETURN
ENDIF
FOR lnInd = 1 TO ALEN(laAdjCode,1)
  IF laAdjCode[lnInd,2] = IIF(!EMPTY(&lcDetLin..cAdjReason),&lcDetLin..cAdjReason,lcDefAdjCd)
    lnAdjCode = LnInd
    SHOW GET lnAdjCode
  ENDIF
ENDFOR
DO FORM (oAriaApplication.ScreenHome+'ICINVBN.SCX')

    *B610350,1 TMI 06/02/2013 [Start]  close tables
    *=lfCloseTbl('WHSLOC')
    =lfCloseTbl('WHBINLOC')
    *=lfCloseTbl('CODES')
    *B610350,1 TMI 06/02/2013 [End  ] 

*-- End of Function lfAdStyScr.



*:**************************************************************************
*:* Name        : lfICINVBN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/03/2008
*:* Purpose     : open the 'ICINVBN' screen
*:***************************************************************************
*:* Called from : icinvlck
*:***************************************************************************
FUNCTION lfICINVBN

lcStyPic  = loFormSet.lcstytitle
DO FORM (oAriaApplication.ScreenHome+'ICINVBN.SCX') WITH loFormSet

*-- end of lfICINVBN.

*!***************************************************************************
*!* Name        : lfwhBnBrow
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : When Browse Temp. File
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfwhBnBrow()
*!***************************************************************************
FUNCTION lfwhBnBrow

SHOW WINDOW (lcMDBrowTt) REFRESH
lnMNewTot = IIF(loFormSet.llMatModul,&lcDetLin..OnHand,&lcDetLin..TotStk)
SHOW GET lnMNewTot
=lfGetAdjCd()
lcMReason = &lcDetLin..cReason
lcReason = &lcDetLin..cReason
lnMCost   = &lcDetLin..COST
lnOldCost = &lcDetLin..OldCOST
lcSty     = &lcDetLin..STYLE
lcStyDesc = IIF(SEEK(lcSty,'Style'),STYLE.Desc1,'')
SHOW GET lcMReason
SHOW GET lcReason
SHOW GET lnMCost
SHOW GET lnOldCost
SHOW GET lcStyDesc
SHOW GET lcSty

FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnStk&lcInd = STK&lcInd
  SHOW GET lnStk&lcInd
ENDFOR
lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
lnNewTot = IIF(loFormSet.llMatModul,OnHand,TOTSTK)

SHOW GET lnNewTot DISABLE
=lfRefresh('ICINVBN1')
=lfRefresh('ICINVBN5')

*-- End of Function lfwhBnBrow.
*!**************************************************************************
*!* Name        : lfVNStyBin
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Bin Locations valid for Receive into Multiple Bins Screen
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfVNStyBin()
*!***************************************************************************
FUNCTION lfVNStyBin
STORE '' TO lcBinLoc,lcWarehs
STORE 0 TO lnMsgOptn
lnMsgOptn = gfModalGen('QRM00000B00025','DIALOG',.F.,.F.,'Do you want to add a new Style ?')
IF lnMsgOptn = 1
  llNewStyle = .T.
  STORE '' TO lcsty
  _CUROBJ = OBJNUM(lcSty)
ELSE
  IF lnMsgOptn = 2
    llNewStyle = .F.
    =lfDLGETBIN()   && This may get error as the fn is commented
    =lfStyBnBrw()    && This may get error as the fn is commented
    SHOW GETS
    _CUROBJ = OBJNUM(lnStk1)
  ELSE
    RETURN
  ENDIF
ENDIF
RETURN
*-- End of Function lfVNStyBin.

*!***************************************************************************
*!* Name        : lfVRStyBin
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid function to validate Removing a record.
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Example     : = lfVRStyBin()
*!***************************************************************************
FUNCTION lfVRStyBin
IF llNew
  DELETE
  GO BOTTOM
ELSE
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This Style saved before in the batch file,it can not be deleted')
ENDIF
=lfStyBnBrw()
*-- End of Function lfVRStyBin.
*!***************************************************************************
*!* Name        : lfvNBinSty
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid function to validate style field.
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Example     : = lfvNBinSty()
*!***************************************************************************
FUNCTION lfvNBinSty
PRIVATE lnAlias,lcOldSty
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
STORE '' TO lcOldSty
lcOldSty = lcSty
lnAlias = SELECT(0)
llbrowse = llbrowse OR EMPTY(lcSty) OR '?' $ lcSty
IF llbrowse .OR. (!EMPTY(lcSty) .AND. !SEEK(lcSty,'STYLE'))
  IF !llbrowse .AND. !EMPTY(lcSty) .AND. !SEEK(lcSty,'STYLE')
    *-- give user message. to browse or reenter.
    IF gfModalGen("QRM00000B42014",.F.,.F.,.F.,'Style : '+lcSty+'is not found in the data file' ) = 2
      laData[1] = ''
      _CUROBJ = OBJNUM(IBSTYLE)
      RETURN
    ENDIF
  ENDIF
  lcSty = gfStyBrw('I',lcSty,"",.F.)
  llbrowse = .F.
ENDIF
lcSty = PADR(ALLTRIM(lcSty),19,' ')
SHOW GET lcSty
SELECT (lnAlias)
=lfDLGETBIN()
=lfStyBnBrw()
IF SEEK('S'+SCALE,'Scale')
  FOR I = 1 TO 8
    Z = STR (I,1)
    lcSz&Z = SCALE.Sz&Z
  ENDFOR
ENDIF
SHOW GETS
_CUROBJ = OBJNUM(lnStk1)
*-- End of Function lfvNBinSty.
*!***************************************************************************
*!* Name        : lfOkStyBin
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validation on Bin Location Selection in ICDLBIN.SCX in case
*!*             : of locking inventory
*!***************************************************************************
*!* Called from : BN4MAIN.Prg --> lfvOK
*!***************************************************************************
*!* Example     : = lfOkStyBin()
*!***************************************************************************
FUNCTION lfOkStyBin
SELECT (lcDetLin)
SCATTER MEMVAR MEMO
m.Clocation = lcBinLoc
IF llNewStyle AND SEEK(lcSty,'STYLE')
  SELECT STYLE
  SCATTER MEMVAR MEMO
  DO CASE
  CASE  lcCostMeth = 'S'  && Standard
    lnMCost    = STYLE.TotCost
    lnOldCost   = STYLE.TotCost
  OTHERWISE       && 'A' Average
    =SEEK(lcSty + laData[8],'STYDYE')
    lnMCost    = STYDYE.Ave_Cost
    lnOldCost   = STYDYE.Ave_Cost
  ENDCASE
  lcReason  = cReason
  lcStyDesc = IIF(SEEK(lcSty,'Style'),STYLE.Desc1,'')
ENDIF
IF SEEK(m.CBATTYPE+m.CLKBATCH+m.STYLE+m.COLOR+m.DYELOT+m.CLOCATION,lcBatLin)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This bin is already existing in the Batch , please Choose another' )
  RETURN
ENDIF
STORE 0 TO m.STK1,m.STK2,m.STK3,m.STK4,m.STK5,m.STK6,m.STK7,m.STK8,m.TOTSTK
STORE 0 TO lnSTK1,lnSTK2,lnSTK3,lnSTK4,lnSTK5,lnSTK6,lnSTK7,lnSTK8,lnNewTot
IF SEEK(IIF(ORDER('WHBINLOC')='WHBINLOC',m.cWareCode+m.CLOCATION+lcSty,lcSty+m.cWareCode+m.CLOCATION),'WHBINLOC')
  FOR lnStkI = 1 TO 8
    lcStkI = ALLTRIM(STR(lnStkI))
    m.OLDSTK&lcStkI = WHBINLOC.QTY&lcStkI
  ENDFOR
  m.OldTotStk = WHBINLOC.TotQty
ELSE
  STORE 0 TO m.OLDSTK1,m.OLDSTK2,m.OLDSTK3,m.OLDSTK4,m.OLDSTK5,m.OLDSTK6,m.OLDSTK7,m.OLDSTK8,m.OLDTOTSTK
ENDIF
m.COST = lnMCost
m.OldCost = lnOldCost
IF !SEEK(m.Style+m.color+m.dyelot+m.clocation,lcDetLin)
  IF SEEK(&lcDetLin..CWARECODE+m.CLOCATION+&lcDetLin..STYLE,'WHBINLOC')
    FOR lnSizStk = 1 TO 8
      lcSizStk = ALLTRIM(STR(lnSizStk))
      m.STK&lcSizStk = WHBINLOC.QTY&lcSizStk
      lnStk&lcSizStk = WHBINLOC.QTY&lcSizStk
      m.OLDSTK&lcSizStk = WHBINLOC.QTY&lcSizStk
    ENDFOR
    STORE WHBINLOC.TOTQTY TO m.TOTSTK,m.OLDTOTSTK,lnNewTot
    m.llNew = .T.
    INSERT INTO (lcDetLin) FROM MEMVAR
    =gfAdd_Info(lcDetLin)
  ELSE
    m.llNew = .T.
    INSERT INTO (lcDetLin) FROM MEMVAR
    =gfAdd_Info(lcDetLin)
  ENDIF
ENDIF
SELECT (lcDetLin)
LOCATE FOR STYLE+CLOCATION = m.STYLE+m.CLOCATION
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  REPLACE STK&lcInd WITH lnStk&lcInd
  SHOW GET lnStk&lcInd
ENDFOR
lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
REPLACE TOTSTK WITH lnNewTot
llNewStyle = .F.
*-- End of Function lfOkStyBin.
*!***************************************************************************
*!* Name        : lfvSizQty
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid Function Per Size/Quantity.
*!***************************************************************************
*!* Called from : ICINVBN.SPR --> Per Size
*!***************************************************************************
*!* Example     : = lfvSizQty()
*!***************************************************************************
FUNCTION lfvSizQty
IF MDOWN()
  RETURN
ENDIF
SELECT (lcDetLin)
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  REPLACE STK&lcInd WITH lnStk&lcInd
ENDFOR
lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
REPLACE TOTSTK WITH lnNewTot
=lfStyBnBrw()
SHOW GET lnNewTot DISABLE
*-- End of Function lfvSizQty.
*!***************************************************************************
*!* Name        : lfvNewCost
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid Function for input a new cost.
*!***************************************************************************
*!* Called from : ICINVBN.SPR --> lnMCost field
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvNewCost()
*!***************************************************************************
FUNCTION lfvNewCost
IF MDOWN()
  RETURN
ENDIF
SELECT (lcDetLin)
REPLACE COST WITH lnMCost

*-- End of Function lfvNewCost.

*!***************************************************************************
*!* Name        : lfVExitOk
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid Function for Save button in mdWn screen buton.
*!***************************************************************************
*!* Called from : ICINVBN.SPR --> Ok button
*!***************************************************************************
*!* Example     : = lfVExitOk()
*!***************************************************************************
FUNCTION lfVExitOk

*T20071102.0018,10/C200876 TMI 05/20/2008 [Start]
lcTmpQuery = loFormSet.lcTmpQuery
*T20071102.0018,10/C200876 TMI 05/20/2008 [End  ]

PRIVATE llBinFound
STORE .F. TO llBinFound
SELECT (lcDetLin)
SCAN
  SCATTER MEMVAR MEMO
  llBinFound = .F.
  SELECT(lcTmpQuery)
  =SEEK(m.STYLE+m.Color)
  SCAN REST WHILE STYLE+COLOR = m.STYLE+m.Color FOR BIN = &lcDetLin..clocation
    m.BIN = m.clocation
    REPLACE STOCK WITH &lcDetLin..TotStk ;
      Cost  WITH lnMCost
    llBinFound = .T.
  ENDSCAN
  IF !llBinFound AND !(m.TotStk =0 AND m.OldTotStk = 0)
    m.BIN     = m.clocation
    m.StyDesc = IIF(SEEK(m.Style,'Style'),STYLE.Desc1,'')
    m.Stock   = m.TotStk
    m.OStock  = m.OldTotStk
    INSERT INTO (lcTmpQuery) FROM MEMVAR
    REPLACE OCost WITH lnOldCost ;
      Cost WITH lnMCost
  ENDIF
ENDSCAN
SELECT (lcDetLin)
SCAN
  IF SEEK(LCKEY+LADATA[2]+&lcDetLin..STYLE+&lcDetLin..COLOR+&lcDetLin..Dyelot+&lcDetLin..cLocation,lcBatLin)
    SELECT (lcBatLin)
    DELETE
  ENDIF
ENDSCAN
SELECT (lcBatLin)
APPEND FROM (oAriaApplication.WorkDir+lcDetLin)
SELECT (lcDetLin)
SCAN
  IF !SEEK(CWARECODE+CLOCATION+STYLE,'WHBINLOC')
    SELECT WHBINLOC
    APPEND BLANK
    REPLACE STYLE      WITH &lcDetLin..STYLE     ,;
      CWARECODE  WITH &lcDetLin..CWARECODE ,;
      clocation  WITH &lcDetLin..cLOCATION ,;
      Qty1       WITH 0                    ,;
      Qty2       WITH 0                    ,;
      Qty3       WITH 0                    ,;
      Qty4       WITH 0                    ,;
      Qty5       WITH 0                    ,;
      Qty6       WITH 0                    ,;
      Qty7       WITH 0                    ,;
      Qty8       WITH 0                    ,;
      TotQty     WITH 0
    *--Update cBlkPck and cSection
    lcOldOrd = ORDER('WHSLOC')
    SET ORDER TO TAG WhsLoc IN  WHSLOC
    IF SEEK(&lcDetLin..CWARECODE +&lcDetLin..clocation +SPACE(19),'WHSLOC')
      SELECT WHBINLOC
      REPLACE cBlkPck   WITH WHSLOC.cBlkPck  ,;
        cSection  WITH WHSLOC.cSection ,;
        cBinClass WITH WHSLOC.cBinClass
    ENDIF
    SET ORDER TO TAG &lcOldOrd IN  WHSLOC
    =gfAdd_Info('WHBINLOC')
  ENDIF
ENDSCAN
ZAP
laData[3] = IIF(laData[3] = 'H','H','M')
SELECT (lcBatHdr)
GOTO TOP
REPLACE TYPE WITH laData[3]
CLEAR READ
IF USED(lcDetLin)
  USE IN (lcDetLin)
ENDIF
*--Erase the lcDetLin file.
=lfEraseFil(lcDetLin)

*-- End of Function lfVExitOk.
*!***************************************************************************
*!* Name        : LFFILLSEC
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : Fill Array laSource for section Mover at inventory locking Grid
*!***************************************************************************
*!* Called from : REPORTS\IC\ICSTYLST.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : CP#132193
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFFILLSEC()
*!***************************************************************************
FUNCTION lfFILLSEC
LOCAL lcWareCode

DIME laScSource[1,1]
DIME laScTarget[1,1]
IF TYPE('loFormSet')='O'
  lcWareCode = loFormSet.Ariaform1.kbLocation.Keytextbox.VALUE
  SELECT DISTINCT CSECTION ;
    FROM (oAriaApplication.DataDir+'WHSLOC') ;
    WHERE CWARECODE = lcWareCode AND !EMPTY(CSECTION) ;
    INTO ARRAY laScSource
ELSE
  SELECT DISTINCT CSECTION ;
    FROM (oAriaApplication.DataDir+'WHSLOC') ;
    WHERE !EMPTY(CSECTION) ;
    INTO ARRAY laScSource
ENDIF

*-- End of Function LFFILLSEC.

*:**************************************************************************
*:* Name        : lfShwScope
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/21/2008
*:* Purpose     : Calls the lfvScope fucntion with parameters changed
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfShwScope()
*:***************************************************************************
FUNCTION lfShwScope

DIMENSION laScSource[1,1],laScTarget[1,1]
STORE '' TO laScSource[1,1],laScTarget[1,1]
lnRpSectn = 0
lcExpr = gfOpGrid('ICSTYLST',.T.,.F.,.F.,.T.)

*- Add a property to hold the selected sections
loFormSet.ADDPROPERTY('laScTarget[1,1]')
loFormSet.ADDPROPERTY('laRpTarget[1,1]')

DIMENSION loFormSet.laScTarget[ALEN(laScTarget,1),MAX(ALEN(laScTarget,2),1)]
=ACOPY(laScTarget,loFormset.laScTarget)
DIMENSION loFormSet.laRpTarget[ALEN(laRpTarget,1),MAX(ALEN(laRpTarget,2),1)]
=ACOPY(laRpTarget,loFormset.laRpTarget)
*T20071102.0018,10/C200876 TMI 07/07/2008 [Start] Show the progress bar
llStySelect = .F.
lcStyFile = ''
lnStyPos = ASCAN(laVrExpr,"STYLE.CSTYMAJOR")
IF lnStyPos > 0
  lnStyPos  = ASUBSCRIPT(laVrExpr,lnStyPos,1)
  lcStyFile = laVrExpr[lnStyPos ,6]
ENDIF
IF !EMPTY(lcStyFile)  AND USED(lcStyFile)
  SELECT(lcStyFile)
  LOCATE
  IF !EOF()
    llStySelect = .T.
  ENDIF
ENDIF

loFormSet.oProgress.TotalProgress = IIF(llStySelect,RECCOUNT(lcStyFile),RECCOUNT('STYDYE'))
loFormSet.oProgress.SHOW()

IF TYPE('loFormSet.lnCurrCounter')='U'
  loFormSet.ADDPROPERTY('lnCurrCounter',0)
ENDIF
loFormSet.lnCurrCounter = 0
*T20071102.0018,10/C200876 TMI 07/07/2008 [End  ]

*-- end of lfShwScope.

*!***************************************************************************
*!* Name        : LFVSECTION
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : create Sections mover at inventory locking Grid
*!***************************************************************************
*!* Called from : Inventory Locking Grid --> ICSTYLST.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : CP#132193
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFVSECTION()
*!***************************************************************************
FUNCTION LFVSECTION

IF EMPTY(laScSource)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There are no defined Sections')
ELSE
  = gfMover(@laScSource,@laScTarget,'Sections',.T.,'')
ENDIF

*-- End of Function LFVSECTION.

*!************************************************************
*! Name      : RefreshStatus
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : function to Set the index for the SQL files
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION RefreshSecStatus

LOCAL lcStatusStr, lnTarget
lcStatusStr = ""
IF !EMPTY(laScTarget)
  FOR lnTarget = 1 TO ALEN(laScTarget,1)
    lcStatusStr = lcStatusStr + ", " + laScTarget[lnTarget]
  ENDFOR
  lcStatusStr = SUBSTR(lcStatusStr,3)
ENDIF
RETURN lcStatusStr
ENDFUNC

*!***************************************************************************
*!* Name        : LFBINSFLTR
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Inventory Control (IC)
*!* Purpose     : filter on Bins mover at inventory locking Grid according to
*!*             : selection of section mover
*!***************************************************************************
*!* Called from : REPORTS\IC\ICSTYLST.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : CP#132193
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFBINSFLTR()
*!***************************************************************************
FUNCTION LFBINSFLTR

IF !EMPTY(laScTarget)
  laRpSource = ''
  SELECT DISTINCT CLOCATION FROM WHSLOC WHERE CWARECODE =laData[8] AND !EMPTY(CLOCATION) AND ;
    ASCAN(laScTarget,CSECTION)>0 INTO ARRAY laRpSource
  IF EMPTY(laRpSource)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There are no assigned bins for the selected sections')
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.
*-- End of Function LFBINSFLTR.

*!***************************************************************************
*!* Name        : lfDVLDPOBN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Accept issue/Return Styles Per Bin
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDVLDPOBN()
*!***************************************************************************
FUNCTION lfDVLDPOBN

*T20071102.0018,10/C200876 TMI 08/03/2008 [Start]
RESTORE FROM (oAriaApplication.WorkDir+lcIssQtyFl) ADDITIVE
*T20071102.0018,10/C200876 TMI 08/03/2008 [End  ]
llChkQty = .F.
FOR lnI  = 1 TO 8
  lcI = ALLT(STR(lnI,2))
  IF (ABS(laIssQty[lnI]) <> 0 ) AND (ABS(laIssQty[lnI]) > m.Bin_Qty&lcI)
    llChkQty = .T.
    EXIT
  ELSE
    llChkQty = .F.
  ENDIF
ENDFOR

IF llIssue AND llChkQty
  lcStyle = m.Item
  *--The receiving quantity are not covered the issued quantity
  *--for Style XXXX , This transaction line will be ignored.
  lcMsg2 = 'There is not enough stock of component '+lcStyle+ ' on hand at this location - please adjust the Issue Quantity'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  RETURN .F.
ELSE
  =lfIssRetSty(m.Typ,m.cCatgTyp,'0001',m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
    m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
    IIF(llIssue,SPACE(6),loParentForm.lcSession),IIF(llIssue,loParentForm.lcSession,SPACE(6)))
ENDIF

*-- End of Function lfDVLDPOBN.
*!***************************************************************************
*!* Name        : lfDVLDPOAU
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Issue Lot cost items Per Bin
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDVLDPOAU()
*!***************************************************************************
FUNCTION lfDVLDPOAU

llChkQty = .F.
DIMENSION laOldQty[9]
STORE 0 TO laOldQty
IF !USED('WHBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF
*T20071102.0018,10/C200876 TMI 07/30/2008 [Start]
*lcStyle = &lcIssLtFile..Item
lcStyle = EVALUATE(loFormset.lcIssltfile+'.Item')
lcWarecode = EVALUATE(loFormset.lcIssltfile+'.cWareCode')
*T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]
SELECT WHBINLOC
lcOrdOrdr = ORDER()
=gfSetOrder('Whbinlst')
*T20071102.0018,10/C200876 TMI 07/30/2008 [Start]
*IF SEEK(lcStyle + laData[32])
IF gfSEEK(lcStyle + lcWarecode,'WHBINLOC')
  *T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]
  FOR lnCntr = 1 TO 9
    laOldQty[lnCntr] = laIssQty[lnCntr]
  ENDFOR
  SCAN REST WHILE STYLE+cwarecode+clocation = lcStyle + lcWarecode
    FOR lnI  = 1 TO 8
      lcI = ALLT(STR(lnI,2))
      IF (ABS(laOldQty[lnI]) <> 0 ) AND (ABS(laOldQty[lnI]) > (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI))
        laOldQty[lnI]    = laOldQty[lnI] + (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI)
        laOldQty[9]      = laOldQty[9] + (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI)
      ELSE
        laOldQty[9]      = laOldQty[9] - laOldQty[lnI]
        laOldQty[lnI]    = 0
      ENDIF
    ENDFOR
  ENDSCAN
ENDIF

IF  laOldQty[9] = laIssQty[9]
  *T20071102.0018,10/C200876 TMI 07/30/2008 [Start]
  *lcStyle = &lcIssLtFile..Item
  lcStyle = EVALUATE(loFormset.lcIssltfile+'.Item')
  *T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]
  *--The receiving quantity are not covered the issued quantity
  *--for Style XXXX , This transaction line will be ignored.
  lcMsg2 = 'There is not enough stock of component '+lcStyle+ ' on hand at this location - please adjust the Issue Quantity'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
ELSE
  SELECT (loFormSet.lcIssLtFile)
  =lfIssRetSty(Typ,cCatgTyp,cInvType,lcStyle,cWareCode,Dyelot,;
    @laIssQty,lnIssCost,loIsslt.lcOprCode  ,loIsslt.lcLotNo  ,ldIssDate,SPACE(6),loParentForm.lcSession)
ENDIF

*-- End of Function lfDVLDPOAU.

*!*************************************************************************
*!* Name        : lfSavMltBn
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Save Multiple Bins Location Qty. to whbnloc file
*!***************************************************************************
*!* Called from : BN4MAIN.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSavMltBn()
*!***************************************************************************
FUNCTION lfSavMltBn

PRIVATE lnAlias
lnAlias = SELECT(0)
*T20060817.0014,1 (B132562) TMI [Start] use different temp name since fox recognize only first 8 chars of a file alias
*lcBinLine = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)

IF !lfOpnFiles("STYINVJL,BININVJL,WHBINLOC,WHSLOC","STYINVJL,STYINVJL,WHBINLOC,WHSLOC","")
  RETURN
ENDIF
SELECT (lcBinLine)
LOCATE
SCAN
  FOR R = 1 TO 8
    Lcr = ALLT(STR(R,2))
    m.Loc&Lcr = &lcBinLine..Loc&Lcr
    IF !EMPTY(m.loc&lcR) AND &lcBinLine..QTY&LCR > 0
      IF !SEEK(&lcBinLine..cWareCode+m.loc&LCR+&lcBinLine..STYLE,'WHBINLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcBinLine..cWareCode+m.loc&LCR,'WHSLOC')
          m.cbinclass = WHSLOC.cbinclass
          m.cBlkPck   = WHSLOC.cBlkPck
          m.cSection  = WHSLOC.cSection
        ENDIF
        SET ORDER TO TAG WhsLocSt IN  WHSLOC
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE STYLE        WITH  &lcBinLine..STYLE     ,;
          cWareCode    WITH  &lcBinLine..cWareCode ,;
          clocation    WITH  m.loc&LCR             ,;
          Qty&LCR      WITH  &lcBinLine..Qty&LCR   ,;
          TOTQTY       WITH  &lcBinLine..Qty&LCR   ,;
          cbinclass    WITH  m.cbinclass           ,;
          cBlkPck      WITH  m.cBlkPck             ,;
          cSection     WITH  m.cSection
        =gfAdd_Info('WHBINLOC')
      ELSE
        REPLACE  WHBINLOC.Qty&LCR WITH WHBINLOC.Qty&LCR + &lcBinLine..Qty&LCR,;
          WHBINLOC.TOTQTY  WITH WHBINLOC.TOTQTY  + &lcBinLine..Qty&LCR
      ENDIF
      lcOldOrder = ORDER('WHSLOC')
      SET ORDER TO TAG WhsLoc IN  WHSLOC
      IF !SEEK(&lcBinLine..cWareCode+m.loc&LCR+&lcBinLine..STYLE,'WHSLOC')
        =SEEK(EVAL(lcBinLine+'.cWareCode')+m.loc&LCR,'WHSLOC')
        SCATT MEMVAR MEMO
        m.STYLE     = &lcBinLine..STYLE
        m.clocation = m.loc&LCR
        INSERT INTO WHSLOC FROM MEMVAR
      ENDIF
      SET ORDER TO TAG &lcOldOrder IN  WHSLOC
    ENDIF
  ENDFOR
ENDSCAN
SELECT(lnAlias)
*-- End of Function lfSavMltBn.
*!***************************************************************************
*!* Name        : lfDLVLDWRH
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Valid Warehouse with bin location
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVLDWRH()
*!***************************************************************************
FUNCTION lfDLVLDWRH

STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
STORE '' TO lcBinLoc
lcIssWare = laStyWare[lnIssWare,2]
=SEEK(m.Item+lcIssWare+SPACE(10),'StyDye')
lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,STYLE.TotCost)
*-- End of Function lfDLVLDWRH.

*!***************************************************************************
*!* Name        : lfLDFNPORT
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Call Return Screen with modification of Bin Location
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfLDFNPORT()
*!***************************************************************************
FUNCTION lfLDFNPORT

lnAlias = SELECT()
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC","")
  RETURN
ENDIF
PRIVATE m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
STORE '' TO lcBinLoc
DO FORM (oAriaApplication.ScreenHome+'PODLSISS.SCX') WITH .F.
SELECT(lnAlias)

*-- End of Function lfLDFNPORT  .

*!***************************************************************************
*!* Name        : lfLDFNPOIS
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Call Issue Screen with modification of Bin Location
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfLDFNPOIS()
*!***************************************************************************
FUNCTION lfLDFNPOIS

lnAlias = SELECT()
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC","")
  RETURN
ENDIF
PRIVATE m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
STORE '' TO lcBinLoc
DO FORM (oAriaApplication.ScreenHome+'PODLSISS.SCX') WITH .T.
SELECT(lnAlias)

*-- End of Function lfLDFNPOIS.

*!***************************************************************************
*!* Name        : lFVPOBNLC
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Validate bin location
*!***************************************************************************
*!* Called from : PODLSISS.SCX -->MFCSSH.PRG -->Issue/return in costing sheet
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lFVPOBNLC()
*!***************************************************************************

*T20071102.0018,10/C200876 TMI 07/31/2008 [Start]
* Copied as it is to the screen PODLSISS.SCX
*T20071102.0018,10/C200876 TMI 07/31/2008 [End  ]

*!*	FUNCTION lFVPOBNLC

*!*	PRIVATE llFromSeek
*!*	STORE .F.TO llFromSeek
*!*	IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSBIN","")
*!*	  RETURN
*!*	ENDIF
*!*	SELECT WHSLOC
*!*	IF EMPTY(lcBinLoc) AND !llbrowse
*!*	  SET ORDER TO TAG WHSLOCST IN  WHSLOC
*!*	  RETURN
*!*	ENDIF
*!*	IF llbrowse OR (!EMPTY(lcBinLoc) AND !SEEK(lcBinLoc+lcIssWare))
*!*	  IF llIssue
*!*	    llbrowse =.F.
*!*	    lnScaleCnt = Scale.cnt
*!*	    lcBrFields = "clocation  :H='Bin',cBinClass  :H='Bin Class',"
*!*	    FOR lnI=1 TO lnScaleCnt
*!*	      lcI=STR(lnI,1)
*!*	      IF !EMPTY(Scale.Sz&lcI)
*!*	        lcBrFields = lcBrFields + "Qty"+lcI+" :H=PADL(Scale.Sz"+lcI+",5) :P='9999999',"
*!*	      ENDIF
*!*	    ENDFOR
*!*	    lcBrFields = lcBrFields + "TotQty :H='Total ' :P='999999999',"
*!*	    FOR lnI=1 TO lnScaleCnt
*!*	      lcI=STR(lnI,1)
*!*	      IF !EMPTY(Scale.Sz&lcI)
*!*	        lcBrFields = lcBrFields + "Alo"+lcI+" :H='Alo '+PADL(Scale.Sz"+lcI+",5)  :P='9999999',"
*!*	      ENDIF
*!*	    ENDFOR
*!*	    lcBrFields = lcBrFields + "TotAlo :H='Total Allocated ' :P='999999999'"
*!*	    DIMENSION laTempData[3]
*!*	    STORE '' TO laTempData
*!*	    =SEEK(ALLTRIM(lcBinLoc))
*!*	    SELECT WHBINLOC
*!*	    lcForExpr = " cWareCode = lcIssWare  AND (TotQty-TotAlo>0) AND STYLE = m.Item"
*!*	    =gfBrows([FOR &lcForExpr],'cLocation,cBinClass','laTempData','WHBINLOC')
*!*	    lcBinLoc=laTempData[1]
*!*	  ELSE
*!*	    llbrowse =.F.
*!*	    lcBrFields = "clocation  :H='Bin',cBinClass  :H='Bin Class'"
*!*	    DIME laTempData[3]
*!*	    STORE '' TO laTempData
*!*	    =SEEK(ALLTRIM(lcBinLoc))
*!*	    lcForExpr = " cWareCode = lcIssWare  AND EMPTY(Style)"
*!*	    =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
*!*	    lcBinLoc=laTempData[1]
*!*	  ENDIF
*!*	  SELECT WHBINLOC
*!*	  IF SEEK(lcIssWare+lcBinLoc+m.Item)
*!*	    m.Bin_Qty1 = Qty1 - Alo1
*!*	    m.Bin_Qty2 = Qty2 - Alo2
*!*	    m.Bin_Qty3 = Qty3 - Alo3
*!*	    m.Bin_Qty4 = Qty4 - Alo4
*!*	    m.Bin_Qty5 = Qty5 - Alo5
*!*	    m.Bin_Qty6 = Qty6 - Alo6
*!*	    m.Bin_Qty7 = Qty7 - Alo7
*!*	    m.Bin_Qty8 = Qty8 - Alo8
*!*	  ELSE
*!*	    STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
*!*	  ENDIF
*!*	  SHOW GETS
*!*	ELSE
*!*	  SELECT WHBINLOC
*!*	  IF SEEK(lcIssWare+lcBinLoc+m.Item)
*!*	    m.Bin_Qty1 = Qty1 - Alo1
*!*	    m.Bin_Qty2 = Qty2 - Alo2
*!*	    m.Bin_Qty3 = Qty3 - Alo3
*!*	    m.Bin_Qty4 = Qty4 - Alo4
*!*	    m.Bin_Qty5 = Qty5 - Alo5
*!*	    m.Bin_Qty6 = Qty6 - Alo6
*!*	    m.Bin_Qty7 = Qty7 - Alo7
*!*	    m.Bin_Qty8 = Qty8 - Alo8
*!*	  ELSE
*!*	    STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
*!*	  ENDIF
*!*	  SHOW GETS
*!*	ENDIF
*!*	SET ORDER TO TAG WHSLOCST IN  WHSLOC
*-- End of Function lFVPOBNLC.

*!***************************************************************************
*!* Name        : lfDLCHKBIN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : validate bin location
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCHKBIN()
*!***************************************************************************
FUNCTION lfDLCHKBIN

IF EMPTY(lcBinLoc)
  lcMsg2 = 'You have to select a bin location'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  RETURN .F.
ENDIF

IF !llIssue
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    IF EVALUATE('m.Iss_Qty'+lcCount) > EVALUATE('m.Used_qty'+lcCount)
      *E300725,1 Message : 38057
      *E300725,1 Return quantity cannot exceed issued quantity
      *E300725,1 Button : 00000
      *E300725,1 Ok
      =gfModalGen('TRM38057B00000','ALERT')
      RETURN .F.
    ENDIF
  ENDFOR
ENDIF
RETURN
*-- End of Function lfDLCHKBIN.

*!***************************************************************************
*!* Name        : lfDLUPDQTY
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Update BinInvJL & WHBINLOC file
*!***************************************************************************
*!* Called from : MFCSSH.prg 1 - from manual Issue (TYPE('lcBinLoc') <> 'U')
*!*             :            2 - from Automatic Issue (TYPE('lcBinLoc') = 'U')
*!*             :            3 - from Delete Po Cost sheet llFrmDel
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLUPDQTY()
*!***************************************************************************
FUNCTION lfDLUPDQTY

PRIVATE lcOldOrder
DIMENSION laOldQty[9]
STORE 0 TO laOldQty
*T20071102.0018,10/C200876 TMI 07/30/2008 [Start]

IF !lfOpnFiles("WHBINLOC,BININVJL,STYINVJL","WHBINLOC,STYINVJL,STYINVJL","")
  *IF !lfOpnFiles("WHBINLOC,BININVJL","WHBINLOC,STYINVJL","")
  *T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]
  RETURN
ENDIF
*T20071102.0018,10/C200876 TMI 07/30/2008 [Start]
SELECT STYINVJL
GOTO RECCOUNT()
*SCATT MEMVAR MEMO
lcGlSession = STYINVJL.csession
*lcGlSession = loParentForm.lcSession
lcWareCode = m.cwarecode
*T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]

*T20071102.0018,10/C200876 TMI 07/30/2008 [Start]
*IF TYPE('lcBinLoc') = 'U' AND !llFromDel
IF TYPE('lcBinLoc') = 'U' AND !llFrmDel
  *T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]
  m.Item = EVALUATE(loFormSet.lcIssLtFile+'.Item')
  SELECT WHBINLOC
  lcOrdOrdr = ORDER()
  =gfSetOrder('Whbinlst')
  IF gfSEEK(m.Item + lcWareCode)
    SELECT STYINVJL
    =gfSEEK(m.Item + lcWareCode + lcGlSession)
    SCAN REST WHILE STYLE+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(LINENO,6) = m.Item + lcWareCode + lcGlSession
      SCATT MEMVAR MEMO
      SELECT WHBINLOC
      =gfSEEK(m.Item + lcWareCode,'WHBINLOC')
      SCAN REST WHILE STYLE+cwarecode+clocation = m.Item + lcWareCode
        IF m.nTOtStk <> 0
          STORE 0 TO laOldQty
          FOR lnI  = 1 TO 8
            lcI = ALLT(STR(lnI,2))
            IF (m.nStk&lcI = 0 ) OR Qty&lcI - ALO&lcI + m.nStk&lcI > 0
              laOldQty[lnI]  = m.nStk&lcI
              laOldQty[9]    = laOldQty[9] + m.nStk&lcI
              m.nTOtStk      = m.nTOtStk - m.nStk&lcI
              m.nStk&lcI     = 0
            ELSE
              laOldQty[lnI] = -1 * Qty&lcI
              laOldQty[9]   = laOldQty[9] -1 * Qty&lcI
              m.nStk&lcI    = m.nStk&lcI + Qty&lcI
              m.nTOtStk     = m.nTOtStk + Qty&lcI
            ENDIF
          ENDFOR
          IF laOldQty[9] <>0
            lcReplace = 'QTY1   WITH '+STR( QTY1+laOldQty[1] )+' '+;
              'QTY2   WITH '+STR( QTY2+laOldQty[2] )+' '+;
              'QTY3   WITH '+STR( QTY3+laOldQty[3] )+' '+;
              'QTY4   WITH '+STR( QTY4+laOldQty[4] )+' '+;
              'QTY5   WITH '+STR( QTY5+laOldQty[5] )+' '+;
              'QTY6   WITH '+STR( QTY6+laOldQty[6] )+' '+;
              'QTY7   WITH '+STR( QTY7+laOldQty[7] )+' '+;
              'QTY8   WITH '+STR( QTY8+laOldQty[8] )
            =gfReplace(lcReplace)

            lcReplace = 'TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
            =gfReplace(lcReplace)

            m.clocation = clocation
            SELECT BININVJL
            =gfAppend('BININVJL',.T.)
            lcReplace = 'NSTK1   WITH '+STR(laOldQty[1])+' '+;
              'NSTK2   WITH '+STR(laOldQty[2])+' '+;
              'NSTK3   WITH '+STR(laOldQty[3])+' '+;
              'NSTK4   WITH '+STR(laOldQty[4])+' '+;
              'NSTK5   WITH '+STR(laOldQty[5])+' '+;
              'NSTK6   WITH '+STR(laOldQty[6])+' '+;
              'NSTK7   WITH '+STR(laOldQty[7])+' '+;
              'NSTK8   WITH '+STR(laOldQty[8])+' '+;
              'NTOTSTK WITH '+STR(laOldQty[9])+' '+;
              'nStkval WITH '+STR(laOldQty[9] * nCost,12,2)
            =gfReplace(lcReplace)

          ENDIF
        ELSE
          EXIT
        ENDIF
      ENDSCAN
    ENDSCAN

    SELECT BININVJL
    =gfTableUpdate()

    SELECT WHBINLOC
    =gfTableUpdate()

  ENDIF

  SELECT WHBINLOC
  =gfSetOrder(lcOrdOrdr)
ELSE
  *T20071102.0018,10/C200876 TMI 07/30/2008 [Start]
  *IF llFromDel
  IF llFrmDel
    *T20071102.0018,10/C200876 TMI 07/30/2008 [End  ]
    IF !USED('WHSLOC')
      =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    STORE '' TO lcBinLoc , laTempData
    DO FORM (oAriaApplication.ScreenHome+'PO\POdlbnSC.SCX')
    IF EMPTY(lcBinLoc)
      DO WHILE EMPTY(lcBinLoc)
        lcMsg2 = 'You have to assign bin location before saving .'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        DO FORM (oAriaApplication.ScreenHome+'PO\POdlbnSC.SCX')
      ENDDO
    ENDIF
    m.Iss_Qty1 = laIssued[1]
    m.Iss_Qty2 = laIssued[2]
    m.Iss_Qty3 = laIssued[3]
    m.Iss_Qty4 = laIssued[4]
    m.Iss_Qty5 = laIssued[5]
    m.Iss_Qty6 = laIssued[6]
    m.Iss_Qty7 = laIssued[7]
    m.Iss_Qty8 = laIssued[8]
    m.Item = STYINVJL.STYLE
  ENDIF
  SELECT STYINVJL
  =gfSEEK(m.Item + m.cWareCode + lcGlSession)
  SCAN REST WHILE STYLE+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(LINENO,6) =;
      m.ITEM + m.cWareCode + lcGlSession
    SCATTER MEMVAR MEMO
    m.clocation = lcBinLoc
    SELECT BININVJL
    m.cAdd_user  = oAriaApplication.User_ID
    m.dAdd_Date  = DATE()
    m.cAdd_time  = gfGetTime()
    =gfAppend('BININVJL',.T.)
  ENDSCAN

  SELECT WHBINLOC
  IF gfSEEK(m.cWareCode+lcBinLoc+m.Item,'WHBINLOC')
    IF cirtype = 'R'
      lcReplace = 'QTY1 WITH '+STR( QTY1+m.Iss_Qty1 )+' '+;
        'QTY2 WITH '+STR( QTY2+m.Iss_Qty2 )+' '+;
        'QTY3 WITH '+STR( QTY3+m.Iss_Qty3 )+' '+;
        'QTY4 WITH '+STR( QTY4+m.Iss_Qty4 )+' '+;
        'QTY5 WITH '+STR( QTY5+m.Iss_Qty5 )+' '+;
        'QTY6 WITH '+STR( QTY6+m.Iss_Qty6 )+' '+;
        'QTY7 WITH '+STR( QTY7+m.Iss_Qty7 )+' '+;
        'QTY8 WITH '+STR( QTY8+m.Iss_Qty8 )+' '
      =gfReplace(lcReplace)

      lcReplace = 'TotQty WITH   Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
      =gfReplace(lcReplace)

    ELSE

      lcReplace = 'QTY1 WITH '+STR( QTY1-m.Iss_Qty1 )+' '+;
        'QTY2 WITH '+STR( QTY2-m.Iss_Qty2 )+' '+;
        'QTY3 WITH '+STR( QTY3-m.Iss_Qty3 )+' '+;
        'QTY4 WITH '+STR( QTY4-m.Iss_Qty4 )+' '+;
        'QTY5 WITH '+STR( QTY5-m.Iss_Qty5 )+' '+;
        'QTY6 WITH '+STR( QTY6-m.Iss_Qty6 )+' '+;
        'QTY7 WITH '+STR( QTY7-m.Iss_Qty7 )+' '+;
        'QTY8 WITH '+STR( QTY8-m.Iss_Qty8 )+' '
      =gfReplace(lcReplace)

      lcReplace = 'TotQty WITH   Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
      =gfReplace(lcReplace)

      *T20071102.0018,10/C200876 TMI 10/29/2008 [Start] check the setup option to delete the bin with zero stock
      *IF TOTQTY = 0
      IF TotQty = 0 .AND. gfGetMemVar('M_DELZRBNR')
        *T20071102.0018,10/C200876 TMI 10/29/2008 [End  ]
        =gfDELETE()
      ENDIF
    ENDIF
  ELSE
    IF cirtype = 'R'
      m.clocation = lcBinLoc
      m.Style     = m.Item
      SELECT WHSLOC
      lcOldOrder = ORDER('WHSLOC')
      =gfSetOrder('WHSLOC')
      =gfSEEK(m.cWareCode+m.clocation,'WHSLOC')
      m.cbinclass = WHSLOC.cbinclass
      SELECT WHBINLOC
      m.cAdd_user  = oAriaApplication.User_ID
      m.dAdd_Date  = DATE()
      m.cAdd_time  = gfGetTime()
      =gfAppend('WHBINLOC',.T.)
      lcReplace = 'QTY1 WITH '+STR( QTY1+m.Iss_Qty1 )+' '+;
        'QTY2 WITH '+STR( QTY2+m.Iss_Qty2 )+' '+;
        'QTY3 WITH '+STR( QTY3+m.Iss_Qty3 )+' '+;
        'QTY4 WITH '+STR( QTY4+m.Iss_Qty4 )+' '+;
        'QTY5 WITH '+STR( QTY5+m.Iss_Qty5 )+' '+;
        'QTY6 WITH '+STR( QTY6+m.Iss_Qty6 )+' '+;
        'QTY7 WITH '+STR( QTY7+m.Iss_Qty7 )+' '+;
        'QTY8 WITH '+STR( QTY8+m.Iss_Qty8 )+' '
      =gfReplace(lcReplace)

      lcReplace = 'TotQty WITH   Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
      =gfReplace(lcReplace)

      SELECT WHSLOC
      IF !gfSEEK(m.cWareCode+m.clocation+m.Style)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =gfAdd_Info('WHSLOC')
      ENDIF
      =gfSetOrder(lcOldOrder)
    ENDIF
  ENDIF

  SELECT BININVJL
  =gfTableUpdate()

  SELECT WHBINLOC
  =gfTableUpdate()
ENDIF

*- Close the table STYINVJL
=gfCloseTable('STYINVJL')

*-- End of Function lfDLUPDQTY.



*!***************************************************************************
*!* Name        : lfADDRMFLD
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Add Custom Fields for temp file
*!***************************************************************************
*!* Called from : RMSAVE.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfADDRMFLD()
*!***************************************************************************
FUNCTION lfADDRMFLD

IF !lfIsUseBin()
  RETURN
ENDIF

*T20071102.0018,10/C200876 TMI 05/27/2008 [Start]
LOCAL lnCrMemLin
*T20071102.0018,10/C200876 TMI 05/27/2008 [End  ]

lnCrMemLin = ALEN(laCrMemLin,1)
DIMENSION laCrMemLin[lnCrMemLin+8,18]
FOR I = 1 TO 8
  lcI = ALLTRIM(STR(I))
  laCrMemLin[lnCrMemLin + I , 1] = 'BinLoc&lcI'
  laCrMemLin[lnCrMemLin + I , 2] = 'C'
  laCrMemLin[lnCrMemLin + I , 3] = 10
  laCrMemLin[lnCrMemLin + I , 4] = 0
ENDFOR
*-- End of Function lfADDRMFLD.

*!***************************************************************************
*!* Name        : lfDLSAVRM
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Save Credit Memo in WHBINLOC File
*!***************************************************************************
*!* Called from : RMCRMEM.PRG -->lpSavScr
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSAVRM()
*!***************************************************************************
FUNCTION lfDLSAVRM

IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lnCurAlias,lcRmOrder
lnCurAlias = ALIAS()
STORE "" TO lcRmOrder

IF !lfOpnFiles("STYINVJL,BININVJL,RMBINLOC,WHBINLOC","STYINVJL,STYINVJL,RMBINCRM,WHBINLST","")
  RETURN
ENDIF

SELECT RMBINLOC
lcRmOrder = ORDER('RMBINLOC')
=gfSetOrder('RMWARLINE')

DIMENSION laBinArr[8]
PRIVATE lcCurTmp
lcCurTmp = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir + lcCurTmp)(NSTK1 N(7),NSTK2 N(7),NSTK3 N(7),NSTK4 N(7),NSTK5 N(7),NSTK6 N(7),NSTK7 N(7),;
  NSTK8 N(7), nTotstk N(7),LINENO N(6), Binloc C(10) )
INDEX ON STR(LINENO,6)+Binloc  TAG (lcCurTmp)

*T20071102.0018,10/C200876 TMI 05/17/2008 [Start]
lcCrMemLin = loFormset.Ariaform1.RMCRMBUS.lcCrMemlin
*T20071102.0018,10/C200876 TMI 05/17/2008 [End  ]

SELECT (lcCrMemLin)
LOCATE
IF EOF()
  RETURN
ENDIF
PRIVATE llChkUpdt
SCAN
  *B610745,1 TMI 06/12/2014 16:02 [Start] ignore non inventory styles  
  =SEEK(IIF(!EMPTY(CRETSTY),CRETSTY,STYLE),'STYLE')
  IF !STYLE.LINVSTY
    LOOP
  ENDIF 
  *B610745,1 TMI 06/12/2014 16:02 [End  ] 
  =SEEK(STYLE,'STYLE')
  *-- Assign location per size
  FOR lncI = 1 TO 8
    lccI = ALLTRIM(STR(lncI,1))
    laBinArr[lncI] =  &lcCrMemLin..BinLoc&lccI
  ENDFOR

  SELECT (lcCrMemLin)
  *T20060817.0014   TMI [Start] get the style to seek with
  PRIVATE lcSeekSty
  lcSeekSty = IIF(EMPTY(cRetsty),STYLE,cRetsty)
  =SEEK(lcSeekSty ,'STYLE')

  *- Gather each bin in a separate line
  SELECT (lcCurTmp)
  FOR lni = 1 TO 8
    lcI = ALLTRIM(STR(lni,1))
    IF &lcCrMemLin..Qty&lcI<> 0

      *- if style is not included in the WHSLOC file , add it
      if !used('WHSLOC')
        =gfOpenTable('WHSLOC','WHSLOC','SH')
      endif 
      *T*X*
      IF !SEEK(&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lccI+lcSeekSty,'WHSLOC')
        =SEEK(&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lccI,'WHSLOC')
        SCATT MEMVAR MEMO
        m.cWareCode = &lcCrMemHdr..cWareCode
        m.style = lcSeekSty
        m.clocation = &lcCrMemLin..BinLoc&lccI
        INSERT INTO WHSLOC FROM MEMVAR
      ENDIF

      IF !SEEK(PADL(ALLTRIM(&lcCrMemLin..cRet_LINNO),6,' ')+laBinArr[lni],lcCurTmp)
        APPEND BLANK
        REPLACE NSTK&lcI WITH &lcCrMemLin..Qty&lcI,;
          nTotstk  WITH &lcCrMemLin..Qty&lcI,;
          Binloc   WITH laBinArr[lni]       ,;
          LINENO   WITH ROUND(VAL(&lcCrMemLin..cRet_LINNO),0)
      ELSE
        REPLACE NSTK&lcI WITH NSTK&lcI+&lcCrMemLin..Qty&lcI,;
          nTotstk  WITH nTotstk + &lcCrMemLin..Qty&lcI
      ENDIF
    ENDIF
  ENDFOR


  =gfSEEK(lcSeekSty+&lcCrMemHdr..cWareCode,'WHBINLOC')

  *- Update the WHBINLOC file
  FOR lnCount = 1  TO 8
    lcCount = ALLTRIM(STR(lnCount))
    IF !EMPTY(&lcCrMemLin..BinLoc&lcCount)
      *--Updating Bin location with Returns in case of damaged and 2nd Quality.
      IF STYLE.LINVSTY
        SELECT WHBINLOC
        IF !SEEK(lcSeekSty+&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lcCount,'WHBINLOC')
          lcReplace =  'Style       WITH "'+lcSeekSty                   +'" '+;
            'cWareCode   WITH "'+&lcCrMemHdr..cWareCode      +'" '+;
            'clocation   WITH "'+&lcCrMemLin..BinLoc&lcCount +'" '+;
            'CBLKPCK     WITH "'+WHSLOC.CBLKPCK              +'" '+;
            'CBINCLASS   WITH "'+WHSLOC.CBINCLASS            +'" '+;
            'CSECTION    WITH "'+WHSLOC.CSECTION             +'" '+;
            'cAdd_User  WITH [' + oAriaApplication.User_ID  + '] '+;
            'dAdd_Date  WITH {^'+STR(YEAR(DATE()),4)+'-'+STR(MONTH(DATE()),2)+'-'+STR(DAY(DATE()),2) +'} '+;
            'cAdd_Time  WITH [' + gfGetTime()           + '] '
          =gfAdd_Info('WHBINLOC')
          =gfAppend()
          =gfReplace(lcReplace)
        ENDIF
        lcReplace = 'Qty&lcCount WITH ' +STR(Qty&lcCount+&lcCrMemLin..Qty&lcCount)+' '+ ;
          'TotQty      WITH ' +STR(TotQty     +&lcCrMemLin..Qty&lcCount)+' '
        =gfReplace(lcReplace)
      ENDIF
    ENDIF
  ENDFOR

  *- Locate a line in STYINVJL
  NOTE this code segment is poor, it can just be replaced by one line , do it.
  SELECT STYINVJL
  LOCAL lcSty
  lcSty = IIF(!EMPTY(&lcCrMemLin..Cretsty),&lcCrMemLin..Cretsty,&lcCrMemLin..STYLE)
  IF !SEEK(lcSty+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(&lcCrMemHdr..CrDate)+&lcCrMemHdr..CrMemo)
    lcRmGlSess = PADL(INT(VAL(lcRmGlSess)+1),6,'0')
    =SEEK(lcSty+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(&lcCrMemHdr..CrDate)+&lcCrMemHdr..CrMemo)
  ENDIF
  SCATTER MEMVAR MEMO

  =gfSEEK(&lcCrMemLin..crmemo+&lcCrMemHdr..cWareCode+PADL(ALLTRIM(&lcCrMemLin..cRet_LINNO),6,' ')+&lcCrMemLin..STYLE,'RMBINLOC')

  *- Update BININVJL & RMBINLOC
  lnCurCost = m.nCost
  SELECT (lcCurTmp)
  SCAN
    SCATTER MEMVAR MEMO
    m.CLOCATION = m.BINLOC
    m.nCost     = lnCurCost
    m.NSTKval   = m.nTotstk * m.nCost
    SELECT BININVJL
    =gfAppend('BININVJL',.T.)
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      IF &lcCurTmp..NSTK&lcI=0
        LOOP
      ENDIF
      SELECT RMBINLOC
      IF SEEK(&lcCrMemLin..crmemo+&lcCrMemHdr..cWareCode+PADL(ALLTRIM(&lcCrMemLin..cRet_LINNO),6,' ')+lcSty+&lcCurTmp..Binloc,'RMBINLOC')
        lcReplace = 'Qty&lcI  WITH '+STR(&lcCurTmp..NSTK&lcI)+' '+;
          'TotQty   WITH '+STR(TotQty+&lcCurTmp..NSTK&lcI)
        =gfReplace(lcReplace)
      ELSE
        lcReplace = 'Style       WITH "'+lcSty+'" '+;
          'cWareCode   WITH "'+&lcCrMemHdr..cWareCode+'" '+;
          'LineNo      WITH '+&lcCrMemLin..cRet_LINNO+' '+;
          'clocation   WITH "'+&lcCurTmp..Binloc +'" '+;
          'CrMemo      WITH "'+&lcCrMemLin..crmemo+'" '+;
          'Qty&lcI     WITH '+STR(&lcCurTmp..NSTK&lcI)+' '+;
          'TotQty      WITH '+STR(&lcCurTmp..NSTK&lcI)+' '+;
          'cAdd_User  WITH [' + oAriaApplication.User_ID  + '] '+;
          'dAdd_Date  WITH {^'+STR(YEAR(DATE()),4)+'-'+STR(MONTH(DATE()),2)+'-'+STR(DAY(DATE()),2) +'} '+;
          'cAdd_Time  WITH [' + gfGetTime()           + '] '
        =gfAppend()
        =gfReplace(lcReplace)
      ENDIF
    ENDFOR
  ENDSCAN
  SELECT (lcCurTmp)
  ZAP

  SELECT RMBINLOC
  =gfTableUpdate()

  SELECT WHBINLOC
  =gfTableUpdate()   && error when saving data

  SELECT BININVJL
  =gfTableUpdate()
ENDSCAN

SELECT (lnCurAlias)

*-- End of Function lfDLSAVRM.

*!***************************************************************************
*!* Name        : lfRMDFNMNU
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Define Bin Locations valid
*!***************************************************************************
*!* Called from : RMCRMEM.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRMDFNMNU()
*!***************************************************************************
FUNCTION lfRMDFNMNU

IF !lfIsUseBin()
  RETURN
ENDIF

LOCAL lnBarNo
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bins Detail" SKIP FOR _SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'S'
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDLGTRMBN IN BN4MAIN WITH _SCREEN.ACTIVEFORM.PARENT  

*-- End of Function lfRMDFNMNU.


*!***************************************************************************
*!* Name        : lfDLGTRMBN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Screen of get bins in case of RM.
*!***************************************************************************
*!* Called from :
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLGTRMBN()
*!***************************************************************************
FUNCTION lfDLGTRMBN
PARAMETERS loFormSet

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
*! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
*! Comment the code for the Entry B609714 in this fn.

*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
*!*	lnData_Sess = SET("Datasession")
*!*	SET DATASESSION TO loFormSet.DATASESSIONID
*!*	IF USED("syustatc")
*!*	  SELECT syustatc
*!*	  SET ORDER TO CUSER_ID
*!*	ELSE
*!*	  SELECT 0
*!*	  lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
*!*	  IF oAriaApplication.multiinst
*!*	    lcSQLDICPATH =  oAriaApplication.ClientA4Path+'SQLDictionary\'
*!*	  ENDIF
*!*	  USE (lcSQLDICPATH +"syustatc") ORDER CUSER_ID
*!*	  = CURSORSETPROP('Buffering', 5, 'SYUSTATC' )  && Enable
*!*	ENDIF
*!*	*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]
*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]


LOCAL lnAlias
lnAlias = SELECT()

IF !lfOpnFiles("WHSLOC","WHSLOC","")
  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
  *! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
  *! Comment the code for the Entry B609714 in this fn.

  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
  *!*	  SET DATASESSION TO lnData_Sess
  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]
  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]

  RETURN
ENDIF

lcCrMemLin = loFormset.Ariaform1.RMCRMBUS.lcCrMemlin
IF !USED(lcCrMemLin)
  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
  *! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
  *! Comment the code for the Entry B609714 in this fn.

  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
  *!*	  SET DATASESSION TO lnData_Sess
  *!*	  *B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]

  *!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]

  *B610350,1 TMI 06/02/2013 [Start]  close table
  *=lfCloseTbl("WHSLOC")
  *B610350,1 TMI 06/02/2013 [End  ] 
  
  RETURN
ENDIF



SELECT(lcCrMemLin)
SCATTER MEMVAR MEMO
PUSH KEY
ON KEY
DO FORM (oAriaApplication.ScreenHome+'RM\RMDLBIN.SCX') WITH loFormSet
POP KEY
SELECT(lnAlias)

  *B610350,1 TMI 06/02/2013 [Start]  close table
  *=lfCloseTbl("WHSLOC")
  *B610350,1 TMI 06/02/2013 [End  ] 

*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [Start]
*! As per Mariam, review note, on the ticket T20130103.0001, with date 26 - March - 2013, 10:10 AM
*! Comment the code for the Entry B609714 in this fn.
*!*	*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[Start]
*!*	SET DATASESSION TO lnData_Sess
*!*	*B609714,1 MMT 10/25/2011 Error while saving Inventory adjustemnt at DCC[END]
*!B610271,1 HIA 03/18/2013 Aria XP - constant file is missing messages [T20130103.0001] [End]


*-- End of Function lfDLGTRMBN.

*!***************************************************************************
*!* Name        : lfRMCKSVBN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Define Bin Locations valid
*!***************************************************************************
*!* Called from : RMCRMEM.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRMCKSVBN()
*!***************************************************************************
FUNCTION lfRMCKSVBN

IF !lfIsUseBin()
  RETURN
ENDIF

STORE .F. TO  llmessag

*--check location if found or not to prevent user to save empty Bin
llChckLoc = .F.
llChckCls = .F.
DIMENSION laStyArr[1]
STORE '' TO laStyArr

*T20071102.0018,10/C200876 TMI 05/17/2008 [Start]
lcCrMemLin = loFormset.Ariaform1.RMCRMBUS.lcCrMemlin
*T20071102.0018,10/C200876 TMI 05/17/2008 [End  ]

SELECT (lcCrMemLin)
LOCATE
SCAN
  IF llmessag
    EXIT
  ENDIF
  m.loc1 = &lcCrMemLin..Binloc1
  m.loc2 = &lcCrMemLin..Binloc2
  m.loc3 = &lcCrMemLin..Binloc3
  m.loc4 = &lcCrMemLin..Binloc4
  m.loc5 = &lcCrMemLin..Binloc5
  m.loc6 = &lcCrMemLin..Binloc6
  m.loc7 = &lcCrMemLin..Binloc7
  m.loc8 = &lcCrMemLin..Binloc8
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    IF !EMPTY(&lcCrMemLin..Qty&lcI)
      IF EMPTY(m.loc&lcI)
        *B610745,1 TMI 06/12/2014 15:59 [Start] check only LINVSTY styles
        =SEEK(IIF(!EMPTY(CRETSTY),CRETSTY,STYLE),'STYLE')
        IF !STYLE.LINVSTY
          LOOP
        ENDIF 
        *B610745,1 TMI 06/12/2014 15:59 [End  ] 
        =SEEK(STYLE,'STYLE')
        llChckLoc = .T.
        IF EMPTY(laStyArr[1])
          laStyArr[1] = STYLE.cflathang
        ELSE
          IF !(laStyArr[1] = STYLE.cflathang)
            llChckCls = .T.
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDFOR
ENDSCAN

SELECT (lcCrMemLin)
LOCATE
IF llChckCls
  lcMsg2 = 'You have mixed Flat and Hanging styles on this transaction - please enter the bin locations manually'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llmessag = .T.
  STORE .F. TO llShow,llCSave
  RETURN llCSave
ENDIF

IF llChckLoc
  lcMsg2 = 'You have to assign bin location before saving . Do you want to assign bin location now ?'
  lnChoose =gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,lcMsg2)
  IF lnChoose = 1
    IF !USED('WHSLOC')
      =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    lcWarecode = loFormset.AriaForm1.pgfReCrmem.pgHeader.cntHeader.cboLocation.VALUE
    STORE '' TO lcBinLoc , laTempData
    DO FORM (oAriaApplication.ScreenHome+'PO\POdlbnSC.SCX') WITH lcWarecode
    IF EMPTY(lcBinLoc)
      lcMsg2 = 'You have to assign bin location before saving .'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      RETURN llCSave
    ENDIF

    =SEEK(&lcCrMemLin..STYLE,'Style')
    IF !(WhsLoc.cFlatHang = STYLE.cflathang)
      *-- 'You cannot saving the style to a bin of a different Hang/Flat.'
      lcMsg2 = 'You cannot saving the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      RETURN llCSave
    ENDIF
  ELSE
    llmessag = .T.
    STORE .F. TO llShow,llCSave
    RETURN llCSave
  ENDIF
  SELECT (lcCrMemLin)
  SCAN
    REPLACE &lcCrMemLin..Binloc1  WITH laTempData[1],;
      &lcCrMemLin..Binloc2  WITH laTempData[1],;
      &lcCrMemLin..Binloc3  WITH laTempData[1],;
      &lcCrMemLin..Binloc4  WITH laTempData[1],;
      &lcCrMemLin..Binloc5  WITH laTempData[1],;
      &lcCrMemLin..Binloc6  WITH laTempData[1],;
      &lcCrMemLin..Binloc7  WITH laTempData[1],;
      &lcCrMemLin..Binloc8  WITH laTempData[1]
  ENDSCAN
ENDIF
RETURN
*-- End of Function lfRMCKSVBN.

*!***************************************************************************
*!* Name        : lfDLVODRM
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Save Data to Void Credit memo screen
*!***************************************************************************
*!* Called from : RMCRMEM.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVODRM()
*!***************************************************************************
FUNCTION lfDLVODRM

IF !lfIsUseBin()
  RETURN
ENDIF

STORE "" TO lcRetStyle
lcRetStyle = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.STYLE)
lcVRGLSess = loFormset.Ariaform1.RMCRMBUS.lcVRGLSess

*- Update BinInvJL & WHBINLOC file in case of void Credit memo

*T20071102.0018,10/C200876 TMI 05/21/2008 [Start]
*USE IN BININVJL   && pls check the saving in this file in the whole process preceeding this line
*T20071102.0018,10/C200876 TMI 05/21/2008 [End  ]

IF !lfOpnFiles("WHBINLOC,STYINVJL,BININVJL,RMBINLOC","WHBINLOC,STYINVJL,STYINVJL,RMBINCRM","")
  RETURN
ENDIF

=lfDLRMVOID()

SELECT WHBINLOC
=gfSetOrder('WHBINLST')

*- This loop is to update the WHBINLOC & PKBINLOC
IF gfSEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode ,'RMBINLOC')
  *T20071102.0018,10/C200876 TMI 10/22/2008 [Start] download lines from whbinloc using gfSeek
  *=SEEK(RMBINLOC.Style+RMBINLOC.cWareCode ,'WHBINLOC')
  =gfSEEK(RMBINLOC.STYLE+RMBINLOC.cWareCode ,'WHBINLOC')
  *T20071102.0018,10/C200876 TMI 10/22/2008 [End  ]

  SELECT RMBINLOC
  SCAN REST WHILE crmemo+STYLE+cwarecode+clocation = RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode
    IF RMBINLOC.LINENO = VAL(RETLINE.CRET_LINNO)
      IF SEEK(RMBINLOC.STYLE+RMBINLOC.cWareCode+RMBINLOC.cLocation,'WHBINLOC') .AND. ;
          SEEK(RMBINLOC.STYLE,'STYLE') .AND. STYLE.LINVSTY
        SELECT WHBINLOC
        lcReplace = 'Qty1   WITH '+STR( Qty1-RmBinLoc.Qty1 ) +' '+;
          'Qty2   WITH '+STR( Qty2-RmBinLoc.Qty2 ) +' '+;
          'Qty3   WITH '+STR( Qty3-RmBinLoc.Qty3 ) +' '+;
          'Qty4   WITH '+STR( Qty4-RmBinLoc.Qty4 ) +' '+;
          'Qty5   WITH '+STR( Qty5-RmBinLoc.Qty5 ) +' '+;
          'Qty6   WITH '+STR( Qty6-RmBinLoc.Qty6 ) +' '+;
          'Qty7   WITH '+STR( Qty7-RmBinLoc.Qty7 ) +' '+;
          'Qty8   WITH '+STR( Qty8-RmBinLoc.Qty8 ) +' '
        =gfReplace(lcReplace)
        lcReplace = 'TotQty WITH   Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
        =gfReplace(lcReplace)
        IF TotQty =0
          =gfDELETE()
        ENDIF
      ENDIF
      SELECT RMBINLOC
      =gfDELETE()
    ENDIF
  ENDSCAN
ENDIF

SELECT BININVJL
=gfTableUpdate()

SELECT RMBINLOC
=gfTableUpdate()

SELECT WHBINLOC
=gfTableUpdate()

*-- End of Function lfDLVODRM.

*!***************************************************************************
*!* Name        : lfDLRMVOID
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Update BinInvJL & WHBINLOC file in case of void Credit memo
*!***************************************************************************
*!* Called from : BN4MAIN.PRG --> lfDLVODRM
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLRMVOID()
*!***************************************************************************
FUNCTION lfDLRMVOID

PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv
lnCurAlias = SELECT()

lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()

*--create tmpfile for invoice bin location
SELECT RMBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (oAriaApplication.WorkDir+lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON STYLE+cWareCode+clocation TAG (lcTmpIvbin)

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (oAriaApplication.WorkDir+lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON STYLE+cWareCode+clocation TAG (lcTmpBnInv)

SELECT RMBINLOC
=gfSEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cwarecode)
SCAN REST WHILE crmemo+STYLE+cwarecode+clocation = RETHDR.CRMEMO + lcRetStyle + RETHDR.cwarecode ;
    FOR RMBINLOC.LINENO = VAL(RETLINE.CRET_LINNO)
  SCATT MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN

SELECT RETLINE
lnCurRec = RECNO()
=gfSEEK(lcRetStyle+RETHDR.cwarecode+lcVRGLSess,'STYINVJL')
SELECT STYINVJL
SCAN REST WHILE STYLE+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(LINENO,6)= lcRetStyle+RETHDR.cwarecode + lcVRGLSess ;
    FOR ctrtype = '8' AND ctrcode = RETHDR.CRMEMO AND STR(LINENO,6) = PADL(ALLTRIM(RETLINE.CRET_LINNO),6,' ')
  SCATTER MEMVAR MEMO
  =SEEK(STYLE+CWARECODE,lcTmpIvbin)
  SELECT(lcTmpIvbin)
  SCAN REST WHILE STYLE+CWARECODE+CLOCATION = STYINVJL.STYLE+STYINVJL.CWARECODE
    llChkQty = .F.
    FOR lnI  = 1 TO 8
      lcI = ALLT(STR(lnI,2))
      IF (m.NSTK&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.NSTK&lcI))
        llChkQty = .T.
      ELSE
        llChkQty = .F.
        EXIT
      ENDIF
    ENDFOR
    IF llChkQty
      SELECT (lcTmpBnInv)
      m.clocation = &lcTmpIvbin..clocation
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT(lcTmpIvbin)
      FOR lnI = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        REPLACE Qty&lcI WITH Qty&lcI + ABS(m.NSTK&lcI)
      ENDFOR
      REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
      EXIT
    ENDIF
    IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
      SELECT (lcTmpBnInv)
      m.clocation = &lcTmpIvbin..clocation
      APPEND BLANK
      GATHER MEMVAR MEMO
      FOR lnI = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF ABS(m.NSTK&lcI) > &lcTmpIvbin..Qty&lcI
          REPLACE NSTK&lcI WITH  &lcTmpIvbin..Qty&lcI
          m.NSTK&lcI = m.NSTK&lcI - &lcTmpIvbin..Qty&lcI
          m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
          REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
          REPLACE &lcTmpIvbin..Qty&lcI WITH 0
        ELSE
          REPLACE NSTK&lcI WITH m.NSTK&lcI
          REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.NSTK&lcI
          REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.NSTK&lcI
          m.nTotStk  = m.nTotStk  + m.NSTK&lcI
          m.NSTK&lcI = 0
        ENDIF
      ENDFOR
      REPLACE nTotStk WITH NSTK1+NSTK2+NSTK3+NSTK4+NSTK5+NSTK6+NSTK7+NSTK8
      REPLACE NSTKval WITH nTotStk * nCost
    ENDIF
  ENDSCAN
ENDSCAN

SELECT RETLINE
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec
ENDIF

SELECT (lcTmpBnInv)
DELETE FOR NSTK1+NSTK2+NSTK3+NSTK4+NSTK5+NSTK6+NSTK7+NSTK8 = 0
GO TOP
SCAN
  SCATTER MEMVAR MEMO
  SELECT BININVJL
  =gfAppend('BININVJL',.T.)
ENDSCAN

*--Erase the lcTmpBnInv and lcTmpIvbin file.
=lfEraseFil(lcTmpBnInv)
=lfEraseFil(lcTmpIvbin)
SELECT (lnCurAlias)
*-- End of Function lfDLRMVOID.

*!***************************************************************************
*!* Name        : lfRMCKVDBN
*!* Developer   : TMI - TAREK MOHAMMED IBRAHEIM
*!* Date        :05/16/2008
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Validate void credit memo
*!***************************************************************************
*!* Called from : RMCRMEM.PRG --> lpderscr
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRMCKVDBN()
*!***************************************************************************
FUNCTION lfRMCKVDBN

IF !lfIsUseBin()
  RETURN
ENDIF

STORE .F. TO llMessage
STORE '' TO lcMsgstr
IF !lfOpnFiles("WHBINLOC,RMBINLOC","WHBINLST,RMBINCRM","")
  RETURN
ENDIF
STORE "" TO lcRetStyle

*T20071102.0018,10/C200876 TMI 07/02/2008 [Start] This line should be inside the loop
*lcRetStyle = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.Style)
*T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

*T20071102.0018,10/C200876 TMI 07/02/2008 [Start] we need here to check only that this credit memo exists in the RMBINLOC file
*IF gfSEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode ,'RMBINLOC')
IF gfSEEK(RETHDR.CRMEMO,'RMBINLOC')
  *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

  *T20071102.0018,10/C200876 TMI 07/02/2008 [Start] this data download should be inside the scan loop
  *=gfSEEK(RMBINLOC.Style+ RMBINLOC.cWareCode + RMBINLOC.cLocation ,'WHBINLOC')
  *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

  *T20071102.0018,10/C200876 TMI 07/02/2008 [Start] Add another loop on the RETLINE file
  SELECT RETLINE
  =SEEK(RETHDR.CRMEMO,'RETLINE')
  SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = RETHDR.CRMEMO
    *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]

    SELECT RMBINLOC
    *T20071102.0018,10/C200876 TMI 07/02/2008 [Start]
    lcRetStyle = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.STYLE)
    =SEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode,'RMBINLOC')
    *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]
    SCAN REST WHILE crmemo+STYLE+cwarecode+clocation = RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode
      *- do not check for stock availability for non inv. styles
      IF SEEK(lcRetStyle,'STYLE') .AND. !STYLE.LINVSTY
        LOOP
      ENDIF
      IF gfSEEK(RMBINLOC.STYLE+ RMBINLOC.cWareCode + RMBINLOC.cLocation ,'WHBINLOC')
        SELECT WHBINLOC
        FOR lnCurCont = 1 TO 8
          lcCurCont = ALLTRIM(STR(lnCurCont))
          IF Qty&lcCurCont < RmBinLoc.Qty&lcCurCont
            llMessage = .T.
            *B608890,1 AHS 06/08/2009 Uninformative message on Void credit memo where this is not enough bin inventory [Start]
            *lcMsgstr  = lcMsgstr + "Size"+ lcCurCont + " "
            *B610792,1 TMI 08/04/2014 14:56 [Start] Show only styles with low stock, not all styles
            IF NOT RMBINLOC.STYLE $ lcMsgstr
              lcMsgstr  = lcMsgstr + RMBINLOC.STYLE + ","
            ENDIF 
            *B610792,1 TMI 08/04/2014 14:56 [End  ] 
          ENDIF
        ENDFOR
        *B610792,1 TMI 08/04/2014 14:56 [Start] comment this
        *lcMsgstr  = lcMsgstr + RMBINLOC.STYLE + " "
        *B610792,1 TMI 08/04/2014 14:56 [End  ] 
        *B608890,1 AHS 06/08/2009 Uninformative message on Void credit memo where this is not enough bin inventory [End]
      ELSE
        llMessage = .T.
        FOR lnCurCont = 1 TO 8
          lcCurCont = ALLTRIM(STR(lnCurCont))
          IF !EMPTY(RMBINLOC.Qty&lcCurCont)
            *B610792,1 TMI 08/04/2014 14:56 [Start] show only styles with low stock
            *lcMsgstr  = lcMsgstr + "Size"+ lcCurCont + " "
            IF NOT RMBINLOC.STYLE $ lcMsgstr
              lcMsgstr  = lcMsgstr + RMBINLOC.STYLE + ","
            ENDIF 
            EXIT
            *B610792,1 TMI 08/04/2014 14:57 [End  ] 
          ENDIF
        ENDFOR
      ENDIF
      *T20071102.0018,10/C200876 TMI 07/02/2008 [Start] This message notification can be showed after the end of the RETLINE scan loop
      *IF llMessage
      *  lcMsgstr  = "No enough stock for bin "+ RMBINLOC.clocation + "Sizes " + lcMsgstr
      *  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsgstr)
      *ENDIF
    ENDSCAN
    *T20071102.0018,10/C200876 TMI 07/02/2008 [Start] close the added scan for the RETLINE file , and show a notification message
  ENDSCAN
  IF llMessage
    *B610792,1 TMI 08/04/2014 14:58 [Start] remove last comma
    lcMsgstr = LEFT(lcMsgstr,LEN(lcMsgstr)-1)
    *B610792,1 TMI 08/04/2014 14:58 [End  ] 
    *B608890,1 AHS 06/08/2009 Uninformative message on Void credit memo where this is not enough bin inventory [Start]
    *lcMsgstr  = "No enough stock for bin "+ RMBINLOC.clocation + "Sizes :" + lcMsgstr
    lcMsgstr  = "There is no enough bin inventory for Styles:"+ lcMsgstr +". Cannot void"
    *B608890,1 AHS 06/08/2009 Uninformative message on Void credit memo where this is not enough bin inventory [End]
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsgstr)
  ENDIF
  *T20071102.0018,10/C200876 TMI 07/02/2008 [End  ]
ENDIF
llReturn= IIF(llMessage,.F.,.T.)
RETURN llReturn
*-- End of Function lfRMCKVDBN.

*!*************************************************************
*! Name      : lfChkStrct
*! Developer : Nader Nabil (NNA)
*! Date      : 02/14/2006
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from        : SOACJERE.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]

  CASE laItemSeg[lnCount,1]='F'
    lnStyLen  = LEN(laItemSeg[lnCount,3])
    lnStyStPos = laItemSeg[lnCount,4]

  CASE laItemSeg[lnCount,1]='S'
    lnScaLen   = LEN(laItemSeg[lnCount,3])
    lnScaStPos = laItemSeg[lnCount,4]
  ENDCASE
ENDFOR
*--End of lfChkStrct.

*:**************************************************************************
*:* Name        : lfGFSTYCRL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : calls the lfStyCrl function for bin locations instead of the global one
*:***************************************************************************
*:* Called from : gpSaveInv in ARINV.PRG
*:***************************************************************************
FUNCTION lfGFSTYCRL

LOCAL lnSlct
lnSlct = SELECT(0)

IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lnRet,lnK,lcK

lcBinLoc = ''
*T20071102.0018,9 TMI 04/30/2008 [Start]
DO CASE
CASE SUBSTR(loFormSet.NAME,4) = 'ARDINV'
  *T20071102.0018,9 TMI 04/30/2008 [End  ]
  FOR lnK = 1 TO 8
    lcK = STR(lnK,1)
    *T20071102.0018,9 TMI 04/29/2008 [Start]
    *lcBinLoc = lcBinLoc + &lcDetFile..Binloc&lcK + '|'
    lcBinLoc = lcBinLoc + IIF(!EMPTY(&lcDetFile..Binloc&lcK), &lcDetFile..Binloc&lcK+'|' , '' )
    *T20071102.0018,9 TMI 04/29/2008 [End  ]
  ENDFOR

  *T20071102.0018,9 TMI 04/30/2008 [Start]
CASE SUBSTR(loFormSet.NAME,4) = 'ARIINV'
  SELECT PKBINLOC
  SCAN
    lcBinLoc = lcBinLoc + PKBINLOC.CLOCATION +'|'
  ENDSCAN
ENDCASE
*T20071102.0018,9 TMI 04/30/2008 [End  ]

*B610676,1 TMI 02/10/2014 20:55 [Start] 
* This function had been depricated by the fix C200876 to use the standard gfStyCrl function instead
*B610676,1 TMI 02/10/2014 20:55 [End  ] 

lnRet = IIF(m.TotQty=0 OR CONSOL = 'Y',13,;
  lfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
  @laAdjStk,m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))

SELECT (lnSlct)
RETURN lnRet
*-- end of lfGFSTYCRL.
*****

*:**************************************************************************
*:* Name        : lfRMSTYCRL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : calls the lfStyCrl function for bin locations instead of the global one
*:***************************************************************************
*:* Called from : lpDelScr IN RMCRMEM.PRG
*:***************************************************************************
FUNCTION lfRMSTYCRL

IF !lfIsUseBin()
  RETURN
ENDIF

PRIVATE lnRet,lnSlct,lcSvOrd,lnL,lcL
lnSlct = SELECT()

IF !USED('BININVJL')
  =gfOpenTable(oAriaApplication.DataDir+'BININVJL','MFGOPR')
ENDIF

*** I found that in case of voided credit memo the loop is on the file RETLINE and the temp file lcCrMemLin is not filled so
*** I will fill it myself to use the BINLOC1,...,8 fields


*T20071102.0018,10/C200876 TMI 05/17/2008 [Start]
lcCrMemLin = loFormset.Ariaform1.RMCRMBUS.lcCrMemlin
lcVRGLSess = loFormset.Ariaform1.RMCRMBUS.lcVRGLSess
*T20071102.0018,10/C200876 TMI 05/17/2008 [End  ]

SELECT BININVJL
lcSvOrd = ORDER()
=gfSetOrder('MFGOPR')

*T20071102.0018,10/C200876 TMI 05/18/2008 [Start] I put this line for rush, to amend the problems come from the laData fields
lcScFields = "CrMemo , Account , RaNo , Store , CrDate , Status , "+;
  "Invoice , Order , cFacCode , Cartons , CustPo , "+;
  "Reference , Reason , cDivision , cIntr_Inv , SalesRep1 , "+;
  "CommPcnt1 , CommAmt1 , SalesRep2 , CommPcnt2 , "+;
  "CommAmt2 , Pieces , Gross_Amt , Disc_Amt , Amount , "+;
  "Tax_Amt , Other , TotCredit , cWareCode , nPstAmt , "+;
  "cCurrCode , nCurrUnit , nExRate , link_code , cFrgtAcnt , "+;
  "cTaxAcnt , cArAcnt , cRetNote1 , cRetNote2 , cTermCode , "+;
  "dPostDate"
SELECT &lcCrMemHdr
SCATTER FIELDS &lcScFields TO laData
*T20071102.0018,10/C200876 TMI 05/18/2008 [End  ]


lcBinLoc = ''
SELECT BININVJL
=gfSEEK(laData[1],'BININVJL')
SCAN REST WHILE CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE = laData[1] ;
    FOR CTRTYPE = '7'
  lcBinLoc = lcBinLoc + CLOCATION + '|'
  SELECT &lcCrMemLin
  &&- ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD
  IF !SEEK(RETLINE.ACCOUNT+RETLINE.STYLE+RETLINE.CRET_LINNO+RETLINE.CRET_TRNCD,lcCrMemLin)
    INSERT INTO &lcCrMemLin (ACCOUNT,STYLE,CRET_LINNO,CRET_TRNCD) VALUES (RETLINE.ACCOUNT,RETLINE.STYLE,RETLINE.CRET_LINNO,RETLINE.CRET_TRNCD)
  ENDIF
  FOR lnL = 1 TO 8
    lcL = STR(lnL,1)
    IF !EMPTY(BININVJL.nStk&lcL)
      REPLACE BINLOC&lcL WITH BININVJL.CLOCATION
    ENDIF
  ENDFOR
ENDSCAN

SELECT (lnSlct)
lnRet = lfStyCrl('8',IIF(!EMPTY(cRetSty) , cRetSty , STYLE),;
  laData[29],Dyelot,CrDate,CrMemo,@laAdjQty,RetLine.Cost,;
  lcRefer,lcVRGLSess,'',lnWhichLin,lcCrMemLin,'nSteps',@laGLDistAr,;
  VAL(RetLine.cRet_LinNo) , lcRecSess)

SELECT BININVJL
=gfSetOrder(lcSvOrd)
RETURN lnRet
*-- end of lfRMSTYCRL.


*:***************************************************************************
* Check if the trigger is entered
* Tmi
* 04/23/2008
*:***************************************************************************
FUNCTION lfChkTrgEnt
PARAMETERS loFormSet,lcPrg
IF TYPE('loFormSet')#'O'
  RETURN
ENDIF

LOCAL lnSlct,lcSvOrd
lnSlct = SELECT(0)

IF UPPER(SYS(0)) == 'TARIK # TMI'

  IF !USED('SYCTRIGG')
    USE (oAriaApplication.SysPath+'syctrigg') IN 0 SHARED ORDER 1
  ENDIF
  SELECT SYCTRIGG
  lcSvOrd = ORDER()
  SET ORDER TO 1
  IF SEEK(PADR(ALLTRIM(SUBSTR(loFormSet.NAME,4)),10)+lcPrg,'SYCTRIGG')
    REPLACE COWNER WITH '<Entered>' ;
      DEDIT_DATE WITH DATE() ;
      CEDIT_TIME WITH TIME()

    IF SYCTRIGG.IGNORE
      RETURN .F.
    ENDIF

    IF 'SUSP' $ ALLTRIM(UPPER(CEDIT_USER))

      _SCREEN.VISIBLE = .T.
      DEBUG
      SUSP

    ENDIF

  ENDIF

  SET ORDER TO &lcSvOrd

ENDIF

SELECT (lnSlct)
*-- end of lfChkTrgEnt.


*:**************************************************************************
*:* Name        : lfErrLogRecord
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        :05/16/2008
*:* Purpose     : recording errors may appear while saving to sql tables
*:***************************************************************************
*:* Called from : BN4MAIN.PRG
*:***************************************************************************
*:* Parameters : llSuccessUpd
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErrLogRecord()
*:***************************************************************************
FUNCTION lfErrLogRecord
PARAMETERS llSuccess

** < write code here > **

*-- end of lfErrLogRecord.

*:**************************************************************************
*:* Name        : lfChkBin
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/09/2008
*:* Purpose     : Checks a bin locatin selected
*:***************************************************************************
*:* Called from : ALPKBIN
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkBin()
*:***************************************************************************
* Try to write this function as global as you can so it can by used to ckeck a selected bin from anywhere in the application
FUNCTION lfChkBin
PARAMETERS lcBin,lcWareCode,laTempData

*-- If no data entered , no check is needed
IF EMPTY(lcBin) .OR. EMPTY(lcWareCode)
  RETURN ''
ENDIF

LOCAL lnAlias,lcSvOrd,lcWH,llBrow,lcSeekKey
lnAlias = SELECT(0)

IF !USED('WHSLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
ENDIF
lcSvOrd = ORDER('WHSLOC')
SELECT WHSLOC
=gfSetOrder('WHSLOC')

llBrow = "?"$lcBin

IF !llBrow
  lcSeekKey = lcWareCode+lcBin+SPACE(25)
  llBrow = !gfSeek(lcSeekKey)
ENDIF

DIMENSION laTempData[3]
STORE '' TO laTempData

IF !EMPTY(lcBin) AND llBrow
  lcWH = ALLTRIM(lcWareCode)
  lcKey = "PADR('"+lcWH+"',6) FOR EMPTY(Style)"
  lcFile_Ttl = lcWH+" Bins"
  lcBrFields = "clocation  :H='Bin',bn= cBinClass+'-'+CFLATHANG  :H='Bin Class/FH'"
  =AriaBrow(lcKey,'Bin Locations',.F.,.F.,.F.,.F.,'',.T.,'CLOCATION,CBINCLASS,CFLATHANG','laTempData')
  lcBin = laTempData[1]
ENDIF
SELECT WHSLOC
=gfSetOrder(lcSvOrd)

IF EMPTY(lcBin )
  SELECT(lnAlias)
  RETURN ''
ENDIF

laTempData[2] = WHSLOC.cBinClass
laTempData[3] = WHSLOC.cFlatHang

SELECT(lnAlias)
RETURN lcBin
*-- end of lfChkBin.


*:**************************************************************************
*:* Name        : lfChkBinClsFlt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/09/2008
*:* Purpose     : check bin class and flat/hang againest a selected styles
*:***************************************************************************
* NOTE : This function assumes that the STYLE file is opened and the line is located on the needed style
*:***************************************************************************
FUNCTION lfChkBinClsFlt
LPARAMETERS lcSz,lcClass,lcFlthng

LOCAL llOk
llOk = .T.

IF llOk
  IF EMPTY(lcFlthng)
    lcMsg = 'You cannot Select a bin with no Flat/Hang assigned.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
    llOk = .F.
  ENDIF
ENDIF

IF llOk
  IF lcFlthng <> STYLE.CFLATHANG
    lcMsg = 'You cannot Select a bin with different Flat/Hang.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
    llOk = .F.
  ENDIF
ENDIF

IF llOk
  llClass = PADR(ALLTRIM(lcClass),1) = PADR(SUBSTR(STYLE.cPrimClss,&lcSz,1),1) .OR. ;
    PADR(ALLTRIM(lcClass),1) = PADR(SUBSTR(STYLE.cSecClss ,&lcSz,1),1) .OR. ;
    PADR(ALLTRIM(lcClass),1) = PADR(SUBSTR(STYLE.cRemClss ,&lcSz,1),1)
  IF !llClass
    *--You cannot transfer from the same warehouse.
    lcMsg = 'You cannot Issue the style to a bin of a different class.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
    llOk = .F.
  ENDIF
ENDIF

RETURN llOk
*-- end of lfChkBinClsFlt.






*:***************************************************************************
*They need that if there is inbalnace then if the bin has more stock than the needed in the pick then the
*program do not prevent the picking process.


FUNCTION lfCLSFLS

=gfCloseTable('WHBINLOC')

*-- end of lfCLSFLS.



*:**************************************************************************
*:* Name        : lfALOCFLY
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/12/2009
*:* Purpose     : Allocate on the fly from the sales order screen
*:***************************************************************************
*:* Called from : ALORDAL.init
*:***************************************************************************
*C200876 TMI 02/12/2009
FUNCTION lfALOCFLY
LOCAL lnSlct
lnSlct = SELECT(0)

IF TYPE('llCalledFromSoord')='U'   && this variable is defined in "lfAlSavAut" trigger, to deny running it if the screen is called in the usual way
  RETURN
ENDIF

WITH loAlOrdAl
  *C201070 TMI 26/02/2009 [Start]
  *!*	  .oToolBar.cmdEdit.Click()
  *!*	  .ChangeMode('E')
  *C201070 TMI 26/02/2009 [End  ]

  .llForceAlc = .F.
  .llgenpktk = .T.
  *C201070 TMI 26/02/2009 [Start] I failed to make next lines work successfully, skip them
  *  RETURN && for test
  *C201070 TMI 26/02/2009 [End  ]

  *C201070 TMI 26/02/2009 [Start]
  *B608850,1   TMI 04/15/2009 [Start]
  *KEYBOARD '{ENTER}'
  *B608850,1   TMI 04/15/2009 [End  ]

  *B608850,1   TMI 04/15/2009 [Start] confirm allocate all
  *.AriaForm1.cmbPickAll.Click()
  .AriaForm1.cmbPickAll.CLICK(.T.)

  SELECT (loAlOrdAl.lc_TmpOrdL)
  LOCATE
  REPLACE PICKED WITH .F. ;
    FOR TOTPIK = 0
  *B608850,1   TMI 04/15/2009 [End  ]

  .SaveFiles
  *C201070 TMI 26/02/2009 [End  ]
ENDWITH

*B608850,1   TMI 04/15/2009 [Start]
RELEASE llCalledFromSoord
SELECT ORDLINE
LOCAL lcSvOrd,lcPikTkt
lcSvOrd = ORDER()
gfSetOrder('ORDLINE')
lcOrder = loAlOrdAl.cOrderNo
=gfSeek('O'+lcOrder,'ORDLINE')
lcPikTkt = ORDLINE.PIKTKT
SUM TOTQTY,TOTPIK TO lnQty,lnPik ;
  REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder
gfSetOrder(lcSvOrd)

llReturn = .F.
DO CASE
CASE lnPik = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No line has been picked')
CASE lnPik = lnQty
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order totally picked with piktkt# &lcPikTkt')
CASE lnPik < lnQty
  llReturn = .T.
ENDCASE

IF !llReturn
  RETURN .F.
ELSE
  loAlOrdAl.changemode('V')
  loAlOrdAl.ACTIVATE
  loAlOrdAl.Seekrecord('O'+loAlOrdAl.cOrderNo)
ENDIF
SELECT (lnSlct )

*C200876 TMI 02/12/2009 [End  ]
*B608850,1   TMI 04/15/2009 [End  ]

*-- end of lfALOCFLY.

*!*	      *B608826,1 TMI 19/03/2009 [Start] update the styinvjl line by line where CONSOL = 'N' and then call the standard
*!*	      *                                 gfStyCrl
*!*	    ELSE
*!*	      IF ASCAN(loFORMSET.laEvntTrig,'GFSTYCRL')<>0 AND &lcDetFile..CONSOL = 'N' AND m.TotQty <> 0
*!*	        *- note : make this to a trigger
*!*	        SELECT &lcDetFile
*!*	        SCATTER MEMVAR
*!*	        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laAdjStk
*!*
*!*	        =gfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,@laAdjStk,;
*!*	                     m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))
*!*	      ENDIF
*!*	      *B608826,1 TMI 19/03/2009 [End  ]









*:**************************************************************************
*:* Name        : lfCallAlloc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/22/2009
*:* Purpose     : this function calls the
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCallAlloc()
*:***************************************************************************
FUNCTION lfCallAlloc

*-- end of lfCallAlloc.




*:**************************************************************************
*:* Name        : SHCANMSG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/23/2009
*:* Purpose     : show a cancel message for bin10 when some lines are cancelled
*:***************************************************************************
*:* Called from : lpDelScr SOORD.SCX
*:***************************************************************************
*B608980,1
FUNCTION lfSHCANMSG
=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some order lines have been picked, pls cancel all picked lines before cancelling the order')

*-- end of SHCANMSG.



*:**************************************************************************
*:* Name        : lfSoOpnAlc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/15/2010
*:* Purpose     : Define the "open the allocation screen" option from the sales order screen
*:***************************************************************************
*:* Called from : SOORD.SCX
*:***************************************************************************
**B609146,1 TMI
FUNCTION lfSoOpnAlc
LOCAL lnBarNo
lnBarNo = CNTBAR('_INQURYPOP') + 1
*-- Define New options to open the ALORDAL screen from the SOORD.SCX screen
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT "\<Sales Order Allocation" SKIP FOR _SCREEN.ACTIVEFORM.PARENT.ActiveMode $ 'SEA'
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfSoOpnAlcScr IN BN4MAIN.FXP
*!B610153,2 SAB 11/13/2012 Fix problem of not showing Order Charges on Sales Order Option [Start]
loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1
*!B610153,2 SAB 11/13/2012 Fix problem of not showing Order Charges on Sales Order Option [End]

*:**************************************************************************
*:* Name        : lfSoOpnAlcScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/15/2010
*:* Purpose     : open the allocation screen from the sales order screen via a menu option
*:***************************************************************************
*:* Called from : lfSoOpnAlc
*:***************************************************************************
**B609146,1 TMI
FUNCTION lfSoOpnAlcScr
LOCAL lcParam

lcParam = _SCREEN.ACTIVEFORM.PARENT.Ariaform1.keyOrder.Keytextbox.VALUE
=oAriaApplication.DoProgram("AWRALORDAL",'"'+lcParam+'"',.F.,'AL')

*!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(Start)
*:**************************************************************************
*:* Name        : lfSTYADJREC
*:* Developer   : Mariam Mazhar(MMT)
*:* Date        : 06/08/2011
*:* Purpose     : Include Bin Records only while calculating diff.
*:***************************************************************************
FUNCTION lfSTYADJREC
PRIVATE lcJourTag,lnCurAlias,;
  lnTotQty,lnTotVal,lnTranVal,lnDiffere

IF !USED('BININVJL_ADJ')
  =gfOpenTable(oAriaApplication.DataDir+'BININVJL','Styinvjl','SH','BININVJL_ADJ')
ENDIF


lnCurAlias = SELECT(0)

SELECT StyInvJl
lcJourTag = ORDER('StyInvJl')
SET ORDER TO StyInvJl

lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0
IF SEEK(lcStyle+lcWareCode,'StyInvJl')
  SCAN REST WHILE STYLE+cWareCode+cSession+DTOS(dTrDate)+cTrCode = ;
      lcStyle+lcWareCode ;
      FOR   cDyelot = lcSDyelot AND !lLockFlg
    IF !(gfSeek(StyInvJl.STYLE+StyInvJl.CWARECODE+StyInvJl.CSESSION+DTOS(StyInvJl.DTRDATE)+StyInvJl.CTRCODE+STR(StyInvJl.LINENO,6),'BININVJL_ADJ') AND BININVJL_ADJ.cLocation = lcBinLoc)
      LOOP
    ENDIF
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
  ENDSCAN

  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn,;
      STYLE      WITH lcStyle,;
      cWareCode  WITH lcWareCode,;
      cDyelot    WITH lcSDyelot,;
      dTrDate    WITH laLockInfo[11],;
      cTrType    WITH lcTrType,;
      cTrCode    WITH IIF(EMPTY(lcTrCode),lcRISessn,lcTrCode),;
      cIRType    WITH IIF(lnDiffere<0,'I','R'),;
      nStkVal    WITH lnDiffere   ,;
      REFERENCE  WITH 'Mark Down Adjustement Value',;
      cAdjReason WITH lcAdjCdRsn ,;
      cAdjAcct   WITH lcAdjAcct  ,;
      cISession  WITH IIF(lnDiffere<0,cSession,''),;
      cRSession  WITH IIF(lnDiffere>0,cSession,''),;
      nPrvSQty   WITH lnPrvQty,;
      nPrvSVal   WITH lnPrvVal,;
      cAdjRef    WITH lcAdjRef

    =lfUpdGLDist(.F.,.T.)
    =lfStyWarDy()
  ENDIF
ENDIF

SET ORDER TO (lcJourTag) IN StyInvJl
SELECT (lnCurAlias)
*:**************************************************************************
*:* Name        : lfLkBinInv
*:* Developer   : Mariam Mazhar(MMT)
*:* Date        : 06/08/2011
*:* Purpose     : Update Lock Flag of Bin Records only
*:***************************************************************************
FUNCTION lfLkBinInv

IF !USED('BININVJL_ADJ')
  =gfOpenTable(oAriaApplication.DataDir+'BININVJL','Styinvjl','SH','BININVJL_ADJ')
ENDIF
SELECT STYINVJL
gfSetorder('STYINVJL')
=gfSEEK(lcItem+IIF(loFormSet.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.VALUE,6),''))
SCAN REST WHILE STYLE+cwarecode+csession+DTOS(dtrdate)+ctrcode = ;
    lcItem+IIF(loFormSet.llWareHus,PADR(loFormset.ariaform1.kblocation.keytextbox.VALUE,6),'')
  IF !(gfSeek(StyInvJl.STYLE+StyInvJl.CWARECODE+StyInvJl.CSESSION+DTOS(StyInvJl.DTRDATE)+StyInvJl.CTRCODE+STR(StyInvJl.LINENO,6),'BININVJL_ADJ') AND BININVJL_ADJ.cLocation = MDINVNTL.clocation)
    LOOP
  ENDIF
  gfREPLACE([lLockFlg WITH llFalg])
ENDSCAN
SELECT(lnAlias)
*!B609607,1 MMT 06/08/2011 Fix bug of repeated adj. record while Posting if Bin Location is used(End)




*! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
*:**************************************************************************
*:* Name        : lfIntLcBn
*:* Developer   : SAB - SABER A.RAZEK
*:* Date        : 11-30-2011
*:* Purpose     : Prepare Temp file that contain actual qty that will be issued from every bin
*:***************************************************************************
FUNCTION lfIntLcBn

IF !lfIsUseBin()
  RETURN
ENDIF
PRIVATE lnPicked,lnRemain,lcAsecend,llPickOne,lcStyle,lnOldAlias,laPikQty,lcBinloc, lcPickBin
PRIVATE lcPikTkt,lcSortBin,lcFlatHang,lcWareCode,lcOrdFile,lcPrmClss,lcSecClss,lcRemClss,llLineAdded


DIMENSION laPikQty[9]
STORE 0 TO laPikQty,lnRemain,lnOldAlias,lnPicked
STORE '' TO lcPikTkt,lcOrdFile
STORE .F. TO llPickOne

lnOldAlias = SELECT(0)

lcBinLine = "_"+SUBSTR(lcTmpLine,2)
SELECT (lcBinLine)
INDEX ON PO + STYLE + STR(LINENO,6) TAG (lcBinLine)

=lfOpnFiles("WHBINLOC,WHSLOC,PKBINLOC","WHBINLST,WHSLOC,PKLINE",lcPikTkt)

IF !USED('SCALE')
  =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF

*B609831,1 SAB 02/15/2012 Fix problems in issue# 23 in the project [Start]
*SELECT (lcTmpLine)
*LOCATE FOR TRANCD $'1'+IIF(loFormSet.llMFCall,'3','4')
*=gfSeek('S'+&lcTmpLine..SCALE,'SCALE')

**********************************************- This code moved inside the scan block

*B609831,1 SAB 02/15/2012 Fix problems in issue# 23 in the project [End]

llPickOne = gfGetMemVar('M_PICKONE')  && Check if Always Pick from on bin location Yes or No
*<tmi start>
llBULKPICK = gfGetMemVar('M_BULKPICK')
*<tmi end  >

lcBinloc  = gfTempName()
lcSortBin = gfTempName()

*B609831,1 SAB 02/15/2012 Fix problems in issue# 23 in the project [Start]
*lcStyle    = &lcTmpLine..STYLE
*lcFlatHang = IIF(SEEK(&lcTmpLine..STYLE,'STYLE'),STYLE.cFlatHang,'F')

**<tmi start>
**lcWareCode= &lcTmpLine..CWARECODE
*lcWareCode = PADR(&lcTmpLine..VENDOR,6)
**<tmi end  >

**********************************************- This code moved inside the scan block

*B609831,1 SAB 02/15/2012 Fix problems in issue# 23 in the project [End]

SELECT WHBINLOC
SET RELATION TO
SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE

*B609831,1 SAB 02/15/2012 Fix problems in issue# 23 in the project [Start]
*SELECT (lcTmpLine)
*SCAN FOR TRANCD $'1'+IIF(loFormSet.llMFCall,'3','4')
*B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
CREATE CURSOR (lcSortBin) (CwareCode C(6),STYLE C(19),cLocation C(10),Qty1 N(11,3),;
  Qty2 N(11,3),Qty3 N(11,3),Qty4 N(11,3),Qty5 N(11,3),Qty6 N(11,3),Qty7 N(11,3),Qty8 N(11,3),totQty N(12,3),cblkpck C(1),CBINCLASS C(2),CBINTYPE1 C(1),;
  CBINTYPE2 C(1),CBINTYPE3 C(1),CBINTYPE4 C(1),CBINTYPE5 C(1),CBINTYPE6 C(1),CBINTYPE7 C(1),CBINTYPE8 C(1))
SELECT(lcSortBin)
INDEX ON STYLE +CwareCode+clocation+cblkpck   TAG (lcSortBin)
IF llPickOne         && Always Pick From one Bin Location / Yes.
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty1,11,3)  TAG (lcSortBin+"1") ADDITIVE
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty2,11,3)  TAG (lcSortBin+"2") ADDITIVE
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty3,11,3)  TAG (lcSortBin+"3") ADDITIVE
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty4,11,3)  TAG (lcSortBin+"4") ADDITIVE
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty5,11,3)  TAG (lcSortBin+"5") ADDITIVE
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty6,11,3)  TAG (lcSortBin+"6") ADDITIVE
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty7,11,3)  TAG (lcSortBin+"7") ADDITIVE
  INDEX ON STYLE +CwareCode+cblkpck+STR(qty8,11,3)  TAG (lcSortBin+"8") ADDITIVE
ENDIF
IF !llBULKPICK
  INDEX ON STYLE +CwareCode+CBINTYPE1  TAG (lcSortBin+"T1") ADDITIVE
  INDEX ON STYLE +CwareCode+CBINTYPE2  TAG (lcSortBin+"T2") ADDITIVE
  INDEX ON STYLE +CwareCode+CBINTYPE3  TAG (lcSortBin+"T3") ADDITIVE
  INDEX ON STYLE +CwareCode+CBINTYPE4  TAG (lcSortBin+"T4") ADDITIVE
  INDEX ON STYLE +CwareCode+CBINTYPE5  TAG (lcSortBin+"T5") ADDITIVE
  INDEX ON STYLE +CwareCode+CBINTYPE6  TAG (lcSortBin+"T6") ADDITIVE
  INDEX ON STYLE +CwareCode+CBINTYPE7  TAG (lcSortBin+"T7") ADDITIVE
  INDEX ON STYLE +CwareCode+CBINTYPE8  TAG (lcSortBin+"T8") ADDITIVE
ENDIF
*B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
SELECT (lcTmpLine)
LOCATE FOR TRANCD $ '1'
SCAN FOR TRANCD $ '1'
  =gfSeek('S'+&lcTmpLine..SCALE,'SCALE')
  lcStyle    = &lcTmpLine..STYLE
  lcFlatHang = IIF(SEEK(&lcTmpLine..STYLE,'STYLE'),STYLE.cFlatHang,'F')
  lcWareCode = PADR(&lcTmpLine..VENDOR,6)

  *B609831,1 SAB 02/15/2012 Fix problems in issue# 23 in the project [End]

  *WAIT WINDOW NOWAIT 'Allocating :'+lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle &&SABER this line comented cause it gives error
  *--If use Bulk/Pick Set to Yes.
  *<tmi start>
  *IF gfGetMemVar('M_BULKPICK')
  IF llBULKPICK
    *<tmi end  >

    IF gfSEEK(lcStyle + lcWareCode ,'WHBINLOC')
      && note
      * this seek can be used in the function "lfUpdPkBin", put it in a variable and use this variable as follows
      * llFound
      * if llFound is .f.  , add a line , update it, set llFound to .t.
      * if llFound is .t., use the SEEK in the downloaded temp cursor and update it
      *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
      *!*        SELECT WHBINLOC
      *!*        SELECT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
      *!*               (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
      *!*               (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
      *!*             (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
      *!*             (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
      *!*             (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
      *!*             FROM  Whbinloc, Whsloc;
      *!*             WHERE Whsloc.cLocation = Whbinloc.cLocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.style=SPACE(19) ;
      *!*                   INTO TABLE (oAriaApplication.WorkDir+lcBinLoc)
      *!*
      *!*      SELECT &lcBinLoc
      IF !SEEK(lcStyle + lcWareCode ,lcSortBin)
        SELECT WHBINLOC
        SCAN REST WHILE STYLE+CWARECODE+CLOCATION = lcStyle + lcWareCode
          =gfSeek(Whbinloc.cWarecode+Whbinloc.cLocation+SPACE(19),'Whsloc','WHSLOC')
          INSERT INTO (lcSortBin) VALUES(Whbinloc.cWarecode,Whbinloc.STYLE, Whbinloc.cLocation,(Whbinloc.Qty1-Whbinloc.Alo1),(Whbinloc.Qty2-Whbinloc.Alo2),;
            (Whbinloc.Qty3-Whbinloc.Alo3),(Whbinloc.Qty4-Whbinloc.Alo4),(Whbinloc.Qty5-Whbinloc.Alo5),(Whbinloc.Qty6-Whbinloc.Alo6),;
            (Whbinloc.Qty7-Whbinloc.Alo7),(Whbinloc.Qty8-Whbinloc.Alo8),(Whbinloc.TotQty-Whbinloc.TotAlo),Whsloc.cblkpck,"","","","","","","","","")
        ENDSCAN
      ENDIF
      *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
      FOR lnCnt = 1 TO SCALE.CNT
        lcCnt = STR(lnCnt,1)
        *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
        *!*          IF llPickOne         && Always Pick From one Bin Location / Yes.
        *!*            *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field and Qty. for the current Size.
        *!*            SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
        *!*                  FROM  (oAriaApplication.WorkDir+lcBinLoc) ORDER BY cblkpck,qty&lcCnt DESCENDING INTO CURSOR &lcSortBin
        *!*          ELSE                && Always Pick From one Bin Location / No.
        *!*            *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field Only for the current Size.
        *!*            SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
        *!*                  FROM  (oAriaApplication.WorkDir+lcBinLoc) ORDER BY cblkpck INTO CURSOR &lcSortBin
        *!*          ENDIF
        IF llPickOne         && Always Pick From one Bin Location / Yes.
          SELECT(lcSortBin)
          SET ORDER TO (lcSortBin+lcCnt) DESCENDING
        ENDIF
        *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
        *-SABER -------------------[Start]  Fill the lcBinLine temp file
        lnRemain = &lcTmpLine..Qty&lcCnt
        *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
        *SCAN FOR &lcSortBin..QTY&lcCnt > 0
        SELECT(lcSortBin)
        =SEEK(lcStyle + lcWareCode)
        SCAN REST WHILE STYLE +CwareCode+cblkpck =lcStyle + lcWareCode FOR &lcSortBin..QTY&lcCnt > 0
          *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
          IF lnRemain <= 0
            EXIT
          ENDIF
          lnPicked  = MIN(lnRemain, &lcSortBin..QTY&lcCnt)
          lcPickBin = &lcSortBin..cLocation
          llLineAdded = .F.

          *- Seek for the line in the lcBinLine temp with the current PO+STYLE+LINENO if not found add new line scatter
          *- from the lcTmpLine, if found check if the LOC of the current size is empty if yes replace the QTY if no
          *- add new line scatter from the current lcBinLine
          IF SEEK(&lcTmpLine..PO + &lcTmpLine..STYLE + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
            *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [Start]
            *SCAN REST WHILE PO+STYLE+STR(LINENO,6) = &lcTmpLine..PO + &lcTmpLine..Style + STR(&lcTmpLine..LINENO,6)
            *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
            *SCAN REST WHILE PO+STYLE+STR(NLINENO,6) = &lcTmpLine..PO + &lcTmpLine..Style + STR(&lcTmpLine..LINENO,6)
            SELECT(lcBinLine)
            SCAN REST WHILE PO+STYLE+STR(LINENO,6) = &lcTmpLine..PO + &lcTmpLine..STYLE + STR(&lcTmpLine..LINENO,6)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
              *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [End]
              IF !EMPTY(&lcBinLine..Loc&lcCnt)
                LOOP
              ENDIF

              *- Update Quantity
              REPLACE &lcBinLine..Loc&lcCnt WITH lcPickBin ,;
                &lcBinLine..QTY&lcCnt WITH (lnPicked * -1),;
                &lcBinLine..TOTQTY    WITH &lcBinLine..TOTQTY + (lnPicked * -1)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
              REPLACE &lcSortBin..QTY&lcCnt  WITH &lcSortBin..QTY&lcCnt -lnPicked
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
              lnRemain = lnRemain - lnPicked
              llLineAdded = .T.
              EXIT
            ENDSCAN

            IF !llLineAdded
              *- Insert new line from the current line in lcBinLine temp file
              SELECT (lcBinLine)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
              =SEEK(&lcTmpLine..PO + &lcTmpLine..STYLE + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
              SCATTER MEMVAR MEMO
              STORE '' TO m.Loc1, m.Loc2, m.Loc3, m.Loc4, m.Loc5, m.Loc6, m.Loc7, m.Loc8
              STORE 0 TO m.Qty1, m.Qty2, m.Qty3, m.Qty4, m.Qty5, m.Qty6, m.Qty7, m.Qty8, m.TOTQty
              APPEND BLANK
              GATHER MEMVAR MEMO

              *- Update Quantity
              REPLACE &lcBinLine..Loc&lcCnt WITH lcPickBin ,;
                &lcBinLine..QTY&lcCnt WITH (lnPicked * -1),;
                &lcBinLine..TOTQTY    WITH &lcBinLine..TOTQTY + (lnPicked * -1)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
              REPLACE &lcSortBin..QTY&lcCnt  WITH &lcSortBin..QTY&lcCnt -lnPicked
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
              lnRemain = lnRemain - lnPicked
              llLineAdded = .T.
            ENDIF
          ELSE
            *- Insert new line from the current line in lcTmpLine temp file
            SELECT (lcTmpLine)
            SCATTER MEMVAR MEMO
            STORE '' TO m.Loc1, m.Loc2, m.Loc3, m.Loc4, m.Loc5, m.Loc6, m.Loc7, m.Loc8
            STORE 0 TO m.Qty1, m.Qty2, m.Qty3, m.Qty4, m.Qty5, m.Qty6, m.Qty7, m.Qty8, m.TOTQty
            SELECT (lcBinLine)
            APPEND BLANK
            GATHER MEMVAR MEMO

            *- Update Quantity
            REPLACE &lcBinLine..Loc&lcCnt WITH lcPickBin ,;
              &lcBinLine..QTY&lcCnt WITH (lnPicked * -1) ,;
              &lcBinLine..TOTQTY    WITH &lcBinLine..TOTQTY + (lnPicked * -1),;
              &lcBinLine..cWareCode WITH lcWareCode
            *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
            REPLACE &lcSortBin..QTY&lcCnt  WITH &lcSortBin..QTY&lcCnt -lnPicked
            **B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[ENd]

            lnRemain = lnRemain - lnPicked
          ENDIF
        ENDSCAN
        *-SABER -------------------[Start]

        *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [Start]
        IF lnRemain > 0
          *C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [Start]
          *=gfModalGen('INM00000B00000','','','','One or more style doesn’t have enough inventory.')
          =gfModalGen('INM00000B00000','','','',"Style " + lcStyle + " doesn’t have enough inventory, Can't Proceed.")
          *C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [End]
          LOCAL lnAlias, lnBuffrMode
          lnAlias = SELECT()
          SELECT (lcBinLine)
          lnBuffrMode = CURSORGETPROP("Buffering")
          CURSORSETPROP("Buffering",1)
          ZAP
          CURSORSETPROP("Buffering",lnBuffrMode)
          SELECT (lnAlias)
          RETURN .F.
        ENDIF
        *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [End]
        lnPicked = 0
        lnRemain = 0
      ENDFOR
      *B610066,4 MMT 10/09/2012 user can issue qty from style un-assigned bins[T20120816.0012][Start]
    ELSE
      =gfModalGen('INM00000B00000','','','',"Style " + lcStyle + " doesn’t have enough inventory, Can't Proceed.")
      LOCAL lnAlias, lnBuffrMode
      lnAlias = SELECT()
      SELECT (lcBinLine)
      lnBuffrMode = CURSORGETPROP("Buffering")
      CURSORSETPROP("Buffering",1)
      ZAP
      CURSORSETPROP("Buffering",lnBuffrMode)
      SELECT (lnAlias)
      RETURN .F.
      *B610066,4 MMT 10/09/2012 user can issue qty from style un-assigned bins[T20120816.0012][End]
    ENDIF

  ELSE  &&*--if use Bulk/Pick Set to No.

    IF gfSEEK(lcStyle + lcWareCode,'WHBINLOC')
      *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
      *!*      SELECT WHBINLOC.STYLE, WHBINLOC.CWARECODE, WHBINLOC.CLOCATION,;
      *!*            (WHBINLOC.QTY1-WHBINLOC.ALO1) AS QTY1,(WHBINLOC.QTY2-WHBINLOC.ALO2) AS QTY2  ,;
      *!*            (WHBINLOC.QTY3-WHBINLOC.ALO3) AS QTY3,(WHBINLOC.QTY4-WHBINLOC.ALO4) AS QTY4  ,;
      *!*            (WHBINLOC.QTY5-WHBINLOC.ALO5) AS QTY5,(WHBINLOC.QTY6-WHBINLOC.ALO6) AS QTY6  ,;
      *!*            (WHBINLOC.QTY7-WHBINLOC.ALO7) AS QTY7,(WHBINLOC.QTY8-WHBINLOC.ALO8) AS QTY8  ,;
      *!*            (WHBINLOC.TOTQtY-WHBINLOC.TOTALO) AS TOTQTY,WHSLOC.CBINCLASS,WHSLOC.CFLATHANG ;
      *!*            FROM  WHBINLOC, WHSLOC;
      *!*            WHERE WHSLOC.CLOCATION = WHBINLOC.CLOCATION AND WHSLOC.CWARECODE=WHBINLOC.CWARECODE AND WHSLOC.STYLE=SPACE(19) ;
      *!*                  AND WHSLOC.CFLATHANG = lcFlatHang ;
      *!*            INTO  TABLE (oAriaApplication.WorkDir+lcBinLoc)
      IF !SEEK(lcStyle + lcWareCode ,lcSortBin)
        SELECT WHBINLOC
        SCAN REST WHILE STYLE+CWARECODE+CLOCATION = lcStyle + lcWareCode
          IF gfSeek(Whbinloc.cWarecode+Whbinloc.cLocation+SPACE(19),'Whsloc','WHSLOC') AND WHSLOC.CFLATHANG = lcFlatHang
            STORE '' TO lcInType1,lcInType2,lcInType3,lcInType4,lcInType5,lcInType6,lcInType8,lcInType7
            FOR lnCnt = 1 TO SCALE.CNT
              lcCnt = STR(lnCnt,1)
              lcPrmClss = SUBSTR(STYLE.CPRIMCLSS,lnCnt ,1)
              lcSecClss = SUBSTR(STYLE.CSECCLSS ,lnCnt ,1)
              lcRemClss = SUBSTR(STYLE.CREMCLSS ,lnCnt ,1)
              lcInType&lcCnt. = IIF(WHSLOC.CBINCLASS=lcPrmClss,'1',IIF(WHSLOC.CBINCLASS=lcSecClss,'2','3'))
            ENDFOR

            INSERT INTO (lcSortBin) VALUES(Whbinloc.cWarecode,Whbinloc.STYLE, Whbinloc.cLocation,(Whbinloc.Qty1-Whbinloc.Alo1),(Whbinloc.Qty2-Whbinloc.Alo2),;
              (Whbinloc.Qty3-Whbinloc.Alo3),(Whbinloc.Qty4-Whbinloc.Alo4),(Whbinloc.Qty5-Whbinloc.Alo5),(Whbinloc.Qty6-Whbinloc.Alo6),;
              (Whbinloc.Qty7-Whbinloc.Alo7),(Whbinloc.Qty8-Whbinloc.Alo8),(Whbinloc.TotQty-Whbinloc.TotAlo),Whsloc.cblkpck,;
              WHSLOC.CBINCLASS,lcInType1,lcInType2,lcInType3,lcInType4,lcInType5,lcInType6,lcInType7,lcInType8)
          ENDIF
        ENDSCAN
      ENDIF
      *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]

      FOR lnCnt = 1 TO SCALE.CNT
        lcCnt = STR(lnCnt,1)
        lcPrmClss = SUBSTR(STYLE.CPRIMCLSS,lnCnt ,1)
        lcSecClss = SUBSTR(STYLE.CSECCLSS ,lnCnt ,1)
        lcRemClss = SUBSTR(STYLE.CREMCLSS ,lnCnt ,1)
        *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
        *!*          IF llPickOne              && Always Pick From one Bin Location / Yes.
        *!*            *-- Select statment from (lcBinLoc) file to sort it by Bin Location Qty. for the current Size.
        *!*            SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG;
        *!*                FROM  (oAriaApplication.WorkDir+lcBinLoc) ;
        *!*                WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&lcCnt>0 ;
        *!*                ORDER BY Qty&lcCnt DESCENDING;
        *!*                INTO  CURSOR &lcSortBin
        *!*          ELSE   && Always Pick From one Bin Location / No.
        *!*              *-- Select statment from (lcBinLoc) file to sort it by Bin Class and Bin Location Alph. for the current Size.
        *!*              *-- Here I Create a new field called CBINTYPE, I fill it with '1' if CBINCLASS = lcPrmClss and So on
        *!*            SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG,;
        *!*                IIF(CBINCLASS=lcPrmClss,'1',IIF(CBINCLASS=lcSecClss,'2','3')) AS CBINTYPE ;
        *!*                FROM  (oAriaApplication.WorkDir+lcBinLoc) ;
        *!*                WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&lcCnt>0;
        *!*                ORDER BY CBINTYPE,CLOCATION ;
        *!*                INTO  CURSOR &lcSortBin
        *!*          ENDIF
        IF llPickOne              && Always Pick From one Bin Location / Yes.
          SELECT(lcSortBin)
          SET ORDER TO (lcSortBin+lcCnt) DESCENDING
        ELSE
          SELECT(lcSortBin)
          SET ORDER TO (lcSortBin+"T"+lcCnt)
        ENDIF
        *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]

        *-SABER -------------------[Start]  Fill the lcBinLine temp file
        lnRemain = &lcTmpLine..Qty&lcCnt
        *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
        *SCAN FOR &lcSortBin..QTY&lcCnt > 0
        SELECT(lcSortBin)
        =SEEK(lcStyle + lcWareCode)
        SCAN REST WHILE STYLE +CwareCode+cblkpck = lcStyle + lcWareCode FOR INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND &lcSortBin..QTY&lcCnt > 0
          *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
          IF lnRemain <= 0
            EXIT
          ENDIF
          lnPicked  = MIN(lnRemain, &lcSortBin..QTY&lcCnt)
          lcPickBin = &lcSortBin..cLocation
          llLineAdded = .F.

          *- Seek for the line in the lcBinLine temp with the current PO+STYLE+LINENO if not found add new line scatter
          *- from the lcTmpLine, if found check if the LOC of the current size is empty if yes replace the QTY if no
          *- add new line scatter from the current lcBinLine
          IF SEEK(&lcTmpLine..PO + &lcTmpLine..STYLE + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
            *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [Start]
            *SCAN REST WHILE PO+STYLE+STR(LINENO,6) = &lcTmpLine..PO + &lcTmpLine..Style + STR(&lcTmpLine..LINENO,6)
            *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
            *SCAN REST WHILE PO+STYLE+STR(NLINENO,6) = &lcTmpLine..PO + &lcTmpLine..Style + STR(&lcTmpLine..LINENO,6)
            SELECT(lcBinLine)
            SCAN REST WHILE PO+STYLE+STR(LINENO,6) = &lcTmpLine..PO + &lcTmpLine..STYLE + STR(&lcTmpLine..LINENO,6)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
              *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [End]
              IF !EMPTY(&lcBinLine..Loc&lcCnt)
                LOOP
              ENDIF

              *- Update Quantity
              REPLACE &lcBinLine..Loc&lcCnt WITH lcPickBin ,;
                &lcBinLine..QTY&lcCnt WITH (lnPicked * -1),;
                &lcBinLine..TOTQTY    WITH &lcBinLine..TOTQTY + (lnPicked * -1)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
              REPLACE &lcSortBin..QTY&lcCnt  WITH &lcSortBin..QTY&lcCnt -lnPicked
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]

              lnRemain = lnRemain - lnPicked
              llLineAdded = .T.
              EXIT
            ENDSCAN

            IF !llLineAdded
              *- Insert new line from the current line in lcBinLine temp file
              SELECT (lcBinLine)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
              =SEEK(&lcTmpLine..PO + &lcTmpLine..STYLE + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]
              SCATTER MEMVAR MEMO
              STORE '' TO m.Loc1, m.Loc2, m.Loc3, m.Loc4, m.Loc5, m.Loc6, m.Loc7, m.Loc8
              STORE 0 TO m.Qty1, m.Qty2, m.Qty3, m.Qty4, m.Qty5, m.Qty6, m.Qty7, m.Qty8, m.TOTQty
              APPEND BLANK
              GATHER MEMVAR MEMO

              *- Update Quantity
              REPLACE &lcBinLine..Loc&lcCnt WITH lcPickBin ,;
                &lcBinLine..QTY&lcCnt WITH (lnPicked * -1),;
                &lcBinLine..TOTQTY    WITH &lcBinLine..TOTQTY + (lnPicked * -1)
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
              REPLACE &lcSortBin..QTY&lcCnt  WITH &lcSortBin..QTY&lcCnt -lnPicked
              *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]

              lnRemain = lnRemain - lnPicked
              llLineAdded = .T.
            ENDIF
          ELSE
            *- Insert new line from the current line in lcTmpLine temp file
            SELECT (lcTmpLine)
            SCATTER MEMVAR MEMO
            STORE '' TO m.Loc1, m.Loc2, m.Loc3, m.Loc4, m.Loc5, m.Loc6, m.Loc7, m.Loc8
            STORE 0 TO m.Qty1, m.Qty2, m.Qty3, m.Qty4, m.Qty5, m.Qty6, m.Qty7, m.Qty8, m.TOTQty
            SELECT (lcBinLine)
            APPEND BLANK
            GATHER MEMVAR MEMO

            *- Update Quantity
            REPLACE &lcBinLine..Loc&lcCnt WITH lcPickBin ,;
              &lcBinLine..QTY&lcCnt WITH (lnPicked * -1),;
              &lcBinLine..TOTQTY    WITH &lcBinLine..TOTQTY + (lnPicked * -1),;
              &lcBinLine..cWareCode WITH lcWareCode
            *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[Start]
            REPLACE &lcSortBin..QTY&lcCnt  WITH &lcSortBin..QTY&lcCnt -lnPicked
            *B610118,1 MMT 11/08/2012 Issue Inter-location PO screen does not issue from the correct Bins[End]

            lnRemain = lnRemain - lnPicked
          ENDIF
        ENDSCAN
        *-SABER -------------------[Start]

        *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [Start]
        IF lnRemain > 0
          *C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [Start]
          *=gfModalGen('INM00000B00000','','','','One or more style doesn’t have enough inventory.')
          =gfModalGen('INM00000B00000','','','',"Style " + lcStyle + " doesn’t have enough inventory, Can't Proceed.")
          *C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [End]
          LOCAL lnAlias, lnBuffrMode
          lnAlias = SELECT()
          SELECT (lcBinLine)
          lnBuffrMode = CURSORGETPROP("Buffering")
          CURSORSETPROP("Buffering",1)
          ZAP
          CURSORSETPROP("Buffering",lnBuffrMode)
          SELECT (lnAlias)
          RETURN .F.
        ENDIF
        *B609856,1 SAB 02/15/2012 Fix problems in issue# 25 in the project [End]

        lnPicked = 0
        lnRemain = 0
      ENDFOR
      *B610066,4 MMT 10/09/2012 user can issue qty from style un-assigned bins[T20120816.0012][Start]
    ELSE
      =gfModalGen('INM00000B00000','','','',"Style " + lcStyle + " doesn’t have enough inventory, Can't Proceed.")
      LOCAL lnAlias, lnBuffrMode
      lnAlias = SELECT()
      SELECT (lcBinLine)
      lnBuffrMode = CURSORGETPROP("Buffering")
      CURSORSETPROP("Buffering",1)
      ZAP
      CURSORSETPROP("Buffering",lnBuffrMode)
      SELECT (lnAlias)
      RETURN .F.
      *B610066,4 MMT 10/09/2012 user can issue qty from style un-assigned bins[T20120816.0012][End]
    ENDIF

  ENDIF

ENDSCAN && of the loop that traverse the lcTmpLine

*<tmi start
SELECT &lcBinLine
SET FILTER TO
LOCATE
DELETE FOR EMPTY(LOC1+LOC2+LOC3+LOC4+LOC5+LOC6+LOC7+LOC8) OR QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 = 0
LOCATE
*<tmi end
SELECT(lnOldAlias)
*-- end of lfIntLcBn.

*! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]
*C201487,1 MMT 05/28/2012 Custom option to display Bin location Audit trail  from BININVJL[Start]
*:**************************************************************************
*:* Name        : lfvBinJlBrow
*:* Developer   : MAriam Mazhar[MMT]
*:* Date        : 05/28/2012
*:* Purpose     : display Bin location Audit trail  from BININVJL
*:***************************************************************************
FUNCTION lfvBinJlBrow
LPARAMETERS loFormSetScr

#INCLUDE r:\aria4xp\screens\ic\icstyle.h
*B610350,1 TMI 06/02/2013 [Start] comment out this part
*lnDataSess = SET("Datasession" )
*SET DATASESSION TO loFormSetScr.DATASESSIONID
*B610350,1 TMI 06/02/2013 [End  ] 

IF USED("syustatc")
  SELECT syustatc
  SET ORDER TO CUSER_ID
ELSE
  SELECT 0
  lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
  IF oAriaApplication.multiinst
    lcSQLDICPATH =  oAriaApplication.ClientA4Path+'SQLDictionary\'
  ENDIF
  USE (lcSQLDICPATH +"syustatc") ORDER CUSER_ID
  = CURSORSETPROP('Buffering', 5, 'SYUSTATC' )  && Enable
ENDIF
IF loFormSetScr.llAllscales
  lnSzCount = IIF(gfSeek("S"+PADR(loFormSetScr.AriaForm1.pgfStyleInfo.page2.cboExScale.VALUE,3),"SCALE"),SCALE.CNT,8)
ELSE
  *B610350,3 TMI 06/18/2013 [Start] be sure that STYLE table is open
  =gfOpenTable('STYLE','STYLE','SH')
  =gfOpenTable('scale','scale','SH')
  *B610350,3 TMI 06/18/2013 [End  ] 
  =SEEK(loFormSetScr.lcstylekey,'STYLE','STYLE')
  lnSzCount = IIF(gfSeek("S"+STYLE.SCALE,"SCALE"),SCALE.CNT,8)
ENDIF
IF !USED('BININVJL_T')
  =gfOpenTable('BININVJL','STYDATE','SH','BININVJL_T')
ENDIF

SELECT 'BININVJL_T'
=gfSeek(loFormSetScr.lcstylekey,'BININVJL_T','STYDATE')

lcBrFields = [clocation :15 :H='Location',Style :25 :H='Style',]
lcBrFields = lcBrFields +  "TrnType=lfTrnType(BININVJL_T.cTrType,STYLE.Make,BININVJL_T.cIRType) :28 :H='"+LANG_1082+"',"
IF loFormSetScr.llMultiWh AND loFormSetScr.llStyMode
  lcBrFields = lcBrFields + "cWareCode :15 :H='"+LANG_1026+"',"
ENDIF
IF loFormSetScr.llDyelot
  IF loFormSetScr.llusestyleconfiguration
    IF lcDyelot = LANG_AllCONFG
      lcBrFields = lcBrFields + "cDyelot    :20 :H='"+LANG_1118+"',"
    ENDIF
  ELSE
    lcBrFields = lcBrFields + "cDyelot    :20 :H='"+LANG_1081+"',"
  ENDIF
ENDIF

lcBrFields = lcBrFields + "dTrDate :15 :H='"+LANG_1097+"',lcTrCd = IIF(EMPTY(cTrCode),cSession,cTrCode) :12 :H='"+LANG_1083+"',"
FOR lnI=1 TO 8
  lcI=STR(lnI,1)
  IF lnI <= lnSzCount
    lcBrFields = lcBrFields + "nStk"+lcI+" :H='"+SCALE.Sz&lcI+"' :P='9999999',"
  ELSE
    lcBrFields = lcBrFields + "nStk"+lcI+":R :H = '~':P = '@Z999999',"
  ENDIF
ENDFOR
IF !EMPTY(SCALE.Sz2)
  lcBrFields = lcBrFields + "nTotStk :H='"+LANG_1104+"' :P='999999999',"
ELSE
  lcBrFields = lcBrFields + "nTotStk :H='~' :P='999999999',"
ENDIF
IF loFormSetScr.llCostPrv
  lcBrFields = lcBrFields + "nCost   :H='"+LANG_1098+"' :P='99999.99',"
ENDIF
lcBrFields = lcBrFields + "Reference :H='"+LANG_1095+"',"
IF loFormSetScr.llCostPrv
  lcBrFields = lcBrFields + "nStkVal  :H='"+LANG_1099+"',"
ENDIF
lcBrFields = lcBrFields + "nPrvsQty :H='"+LANG_1100+"',"
IF loFormSetScr.llCostPrv
  lcBrFields = lcBrFields + "nPrvsVal :H='"+LANG_1101+"',"
ENDIF
lcBrFields = lcBrFields + "LineNo   :H='"+LANG_1102+"'"
SELECT 'BININVJL_T'
LOCATE
*C201487,2 MMT 06/07/2012 Filter by location while displaying Bin location Audit trail  from BININVJL[T20120330.0009][Start]
*=ARIABROW("",'Total Inventory',;
gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
SELECT 'BININVJL_T'
LOCATE
llStyMode  = (loFormSetScr.Ariaform1.pgfStyleInfo.Page2.cboLocation.VALUE="******")          && By style or By style/warehouse.
lcWareCode = IIF(!llStyMode,loFormSetScr.Ariaform1.pgfStyleInfo.Page2.cboLocation.VALUE,'')  &&Warehouse code.
=ARIABROW(IIF(llStyMode  ,"","FOR cWarecode = lcWareCode"),'Total Inventory',;
  gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
*C201487,2 MMT 06/07/2012 Filter by location while displaying Bin location Audit trail  from BININVJL[T20120330.0009][End]
*B610350,1 TMI 06/02/2013 [Start] comment out this part
*SET DATASESSION TO lnDataSess
*B610350,1 TMI 06/02/2013 [End  ] 
*C201487,1 MMT 05/28/2012 Custom option to display Bin location Audit trail  from BININVJL[END]

*B610350,1 TMI 06/02/2013 [Start]  close alias
=lfCloseTbl('BININVJL_T')
*B610350,1 TMI 06/02/2013 [End  ]    

*B610066,1 MMT 09/02/2012 Add Records to Bin Temp. in Automatic/Manual style selection[T20120816.0012][Start] 
*:**************************************************************************
*:* Name        : lfADDBNRCD
*:* Developer   : MAriam Mazhar[MMT]
*:* Date        : 09/02/2012
*:* Purpose     : Add Records to Bin Temp. in Automatic/Manual style selection
*:***************************************************************************
FUNCTION lfADDBNRCD
lcCurrAl = SELECT()
*B610066,2 MMT 09/12/2012 don't add Records to Bin Temp. in Automatic/Manual style selection if location field is empty[Start]
IF EMPTY(EVAL(loformset.lcTmpLine+'.CWARECODE'))
  RETURN
ENDIF
*B610066,2 MMT 09/12/2012 don't add Records to Bin Temp. in Automatic/Manual style selection if location field is empty[END]
LOCAL lcBinLine
lcBinLine = "_"+SUBSTR(loformset.lcTmpLine,2)
IF !USED(lcBinLine)
  SELECT(loformset.lcTmpLine)
  SELECT * FROM (loformset.lcTmpLine) WHERE .F. INTO CURSOR (lcBinLine) READWRITE
  INDEX ON PO + STYLE + STR(LINENO,6) TAG (lcBinLine)
  =CURSORSETPROP("Buffering",5,lcBinLine)
ENDIF
SELECT (loformset.lcTmpLine)
SCATTER MEMVAR MEMO
STORE ' ' TO m.LOC1,m.LOC2,m.LOC3,m.LOC4,m.LOC5,m.LOC6,m.LOC7,m.LOC8
INSERT INTO (lcBinLine) FROM MEMVAR
SELECT(lcCurrAl)
*B610066,1 MMT 09/02/2012 Add Records to Bin Temp. in Automatic/Manual style selection[T20120816.0012][END]


************************************************************
*! Name      : lfCloseTbl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 06/02/2013
*! Purpose   : close passed table name
************************************************************
*B610350,1 TMI 06/02/2013 
FUNCTION lfCloseTbl
PARAMETERS lcTbl
IF USED(lcTbl)
  gfCloseTable(lcTbl)
ENDIF 

*- End of lfCloseTbl.

************************************************************
*! Name      : lfSTKALOC
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 06/25/2014
*! Purpose   : add a trigger STKALOC that is called from beforesave in ICSTYAD that don't allow stock be less than allocated [T20140604.0001 ]
*! Tracking# : B610762,1 
************************************************************
FUNCTION lfSTKALOC
LOCAL llRet
llRet = .T.

*- lnStockAfter : colloect the stock of style after the adjustment transaction
*- lnCurrentAlloc : get the current allocated figure of the style-color-size
*- compare, proceed only if lnStockAfter >= lnCurrentAlloc


RETURN llRet
*- End of lfSTKALOC.