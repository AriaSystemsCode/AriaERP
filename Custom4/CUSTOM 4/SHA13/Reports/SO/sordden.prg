*!*****************************************************************************************
*! Name      : Sorddet.Prg  (Order detail report)
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/15/2002 08:16:22 PM
*! Entry no. :
*!*****************************************************************************************
*!Modifications :
*! B126314,1 HMA 03/08/2005 Display Style Group Discription
*! C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO
*! B129774,1 HMA 09/20/2005 print wrong quantities while summerize multistore orders
*! B128367,1 MMT 02/07/2006 Fix bug of not displaying distribution center in excel format
*! B131649,1 AYM 03/29/2006 Fix Problem of showing Sizes at begin of Page.
*! B129281,1 AYM 04/15/2006 Difference in SO Detail when printing in Graphic wand Text
*! E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence
*! B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open
*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
*! B607998,1 SSH 07/03/2007 not include parially allocated orders
*! B608640,1 WAM 08/03/2008 Fix style browse [T20080801.0002]
*! B608747,1 TMI 11/27/2008 When selecting a saved filter it is not apply
*! B608753,1 MMT 12/04/2008 Fix bug of wrong Data when use Option "Filter by Style.Season"[T20081020.0015]
*! B609259,1 MMT 05/19/2010 SO order detail report export to excel does not export some fields[T20100222.0002]
*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[T20100322.0039]
*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[T20100222.0002]
*! B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008]
*! B609608,1 MMT 06/09/2011 Order detail report deos not filter lines for selected store[T20110517.0003]
*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[T20111116.0011]
*! B609891,1 MMT 04/18/2012 Order Detail report does not show all order stores [T20120315.0045]
*! B609903,1 SAB 05/02/2012 Fix problem when user select summary format with sort by Style [T20120430.0011]
*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[T20120803.0006]
*! E303255,2 MMT 10/17/2012 Cosnider the user defined filters while filtering report data[T20120803.0006]
*! B610385,1 SAB 06/18/2013 Fix bug of clearing the selected records of SO details filter [T20130613.0011]
*! E303409,1 TMI 08/18/2013 Enhance the report by adding a new option to show the scale sizes when style,color changes [T20130618.0003]
*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[T20130829.0041]
*! B610529,1 TMI 09/25/2013 Order Detail report by Summary by Style Group - no group totals	[T20130917.0025]
*! B610634,1 MMT 12/25/2013 Order detail frx SORDDGMP.FRX not installed with R13 Media[T20131216.0009]
*!* B610757,1 HES 06/23/2014 Add pack_ID filter to the order detail report [T20141219.0020]
*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81]
*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030]
*! B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008]
*! E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001]
*! C202017,1 MHM 05/03/2017 problem in custom order detail [T20170501.0061]
*******************************************************************************************
*WAIT WINDOW "Order Detail Report................."
*-- Main program code                        BEGIN
#Include r:\aria4xp\reports\so\sorddet.h
loOGScroll.cCROrientation = 'P'
lcTime     = Time()                     && Variable to hold the Time
*!B131649,1 AYM ..[BEGIN]
llBofPage  =.T.                         && Variable to hold the Begin of Page
*!B131649,1 AYM ..[END]
lcStTime   = lcTime                     && Time in which we start collect data.
llPrntBoth = llRpOrdNot And llRpOrdLnt  && True if you print both types of notes.
*! B129281,1 AYM 04/15/2006 Difference in SO Detail when printing in Graphic and Text
*lnLastRec  = 2  && WHY 2 ???              && Record No. Of last record in order group.
lnLastRec  = 0
*! B129281,1 AYM 04/15/2006 Difference in SO Detail when printing in Graphic and Text

lcTitle    = ''                         && Title of Note.
lcNotes    = ''                         && Notes.
llNoIndex  = .F.                        && I don't make index for file.
lcHeader = ''
lcPhonPict = gfPhoneTem()
*-- Print totals if and only if [Not multi currency or user want any equavelent method or
*-- select only one currency to print]
llPrintTot = !llMultCurr Or (lcRpCurr <> "F") Or ;
	((!Empty(laOGFxFlt[lnCurrPos,6]) And Atc("|",laOGFxFlt[lnCurrPos,6])=0) Or ;
	(!Empty(laOGFxFlt[lnOrdPos ,6]) And Used(laOGFxFlt[lnOrdPos,6]) And Reccount(laOGFxFlt[lnOrdPos,6])=1))

llCurInGrp = !llPrintTot
Private laStores
Dimension laStores[1]
Private lcSvOrd
Select ORDLINE
lcSvOrd = Order('ORDLINE')

Store '' To lcLineCurr
Store .T. To llInnTotal,llOutTotal
llGrdTotal = llPrintTot

lcSeekVal  = ''                                   && Get Last record seek value.

llTextMode = (Upper(Alltrim(lcRepMode))=="TEXT")  && Print Text Format

lcStorCond = ''

lcOldScale = Space(3)
*E303409,1 TMI 08/18/2013 [Start] define lcOldStyle,lcOldStyClr
lcOldSty    = ' '
lcOldStyClr = ' '
*E303409,1 TMI 08/18/2013 [End  ]
llFirstSc = .F.

Dimension laStock[9],laWip[9]
Store '' To laStock,laWip               && Work process arrays

Store '' To lcGrpExp,lcSeaExp,lcDivExp,lcStatusEx,lcCatExp

*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = Set('STATUS BAR')
Set Status Bar On
*-- Show messages in status bar when collecting data. [begin]

*-- if it's first time you run option Grid, i.e: you have unknown variables.
If llFrTime
	lcStyTitle = Iif ('GFITEM' $ Alltrim(Upper(lcStyTitle)),;
		EVALUATE(lcStyTitle),lcStyTitle)  && style title.

	lnMajorLen = Len(gfItemMask("PM"))   && Style major length.
	*lcItemHeadr = gfItemMask("HI")

	*-- Create temporary file that holding order line data. [begin]
	lcWorkFile = gfTempName()
	lcTempLine = gfTempName()

	*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [Start]
	lcTempLineTag = gfTempName()
	*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [End]

	Dimension laTempStru[1,4]
	laTempStru = ''
	Select ORDLINE
	*!SET STEP ON
	= Afields(laTempStru)

	*! C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [START]
	*!*   DIMENSION laTempStru[ALEN(laTempStru,1) + 2, 18]

	*!*   *-- cTempKey :  field used in most sort by case as the master key ,
	*!*   *--          :  and in case of summarize multi store as the total amount.
	*!*   laTempStru[ALEN(laTempStru,1) -1  ,1] = 'cTempKey'
	*!*   laTempStru[ALEN(laTempStru,1) -1  ,2] = 'C'
	*!*   laTempStru[ALEN(laTempStru,1) -1  ,3] = 16
	*!*   laTempStru[ALEN(laTempStru,1) -1  ,4] = 0
	*!*   laTempStru[ALEN(laTempStru,1) -1  ,17] = 0
	*!*   laTempStru[ALEN(laTempStru,1) -1  ,18] = 0

	*!*   *-- cCurrCode :  used if multi currency only to sort by it.
	*!*   laTempStru[ALEN(laTempStru,1)  ,1] = 'cCurrCode'
	*!*   laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
	*!*   laTempStru[ALEN(laTempStru,1)  ,3] = 3
	*!*   laTempStru[ALEN(laTempStru,1)  ,4] = 0
	*!*   laTempStru[ALEN(laTempStru,1)  ,17] = 0
	*!*   laTempStru[ALEN(laTempStru,1)  ,18] = 0
	*! C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [END]

	*! C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [START]
	Dimension laTempStru[ALEN(laTempStru,1) + 4, 18]

	*-- cTempKey :  field used in most sort by case as the master key ,
	*--          :  and in case of summarize multi store as the total amount.
	laTempStru[ALEN(laTempStru,1) -3  ,1] = 'cTempKey'
	laTempStru[ALEN(laTempStru,1) -3  ,2] = 'C'
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
	*laTempStru[ALEN(laTempStru,1) -3  ,3] = 16
	laTempStru[ALEN(laTempStru,1) -3  ,3] = 28
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[END]
	laTempStru[ALEN(laTempStru,1) -3  ,4] = 0
	laTempStru[ALEN(laTempStru,1) -3  ,17] = 0
	laTempStru[ALEN(laTempStru,1) -3  ,18] = 0

	*-- cCurrCode :  used if multi currency only to sort by it.
	laTempStru[ALEN(laTempStru,1) -2 ,1] = 'cCurrCode'
	laTempStru[ALEN(laTempStru,1) -2 ,2] = 'C'
	laTempStru[ALEN(laTempStru,1) -2 ,3] = 3
	laTempStru[ALEN(laTempStru,1) -2 ,4] = 0
	laTempStru[ALEN(laTempStru,1) -2 ,17] = 0
	laTempStru[ALEN(laTempStru,1) -2 ,18] = 0

	*-- cCurrCode :  used if multi currency only to sort by it.
	laTempStru[ALEN(laTempStru,1) -1 ,1] = 'Dist_ctr'
	laTempStru[ALEN(laTempStru,1) -1 ,2] = 'C'
	laTempStru[ALEN(laTempStru,1) -1 ,3] = 8
	laTempStru[ALEN(laTempStru,1) -1 ,4] = 0
	laTempStru[ALEN(laTempStru,1) -1  ,17] = 0
	laTempStru[ALEN(laTempStru,1) -1  ,18] = 0

	*-- cCurrCode :  used if multi currency only to sort by it.
	laTempStru[ALEN(laTempStru,1)  ,1] = 'CType'
	laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)  ,3] = 1
	laTempStru[ALEN(laTempStru,1)  ,4] = 0
	laTempStru[ALEN(laTempStru,1)  ,17] = 0
	laTempStru[ALEN(laTempStru,1)  ,18] = 0
	*! C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [END]
	*! B609259,1 MMT 05/19/2010 SO order detail report export to excel does not export some fields[Start]

	*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
	*DIMENSION laTempStru[ALEN(laTempStru,1) + 13, 18]
	Dimension laTempStru[ALEN(laTempStru,1) + 16, 18]

	laTempStru[ALEN(laTempStru,1)-10  ,1] = 'CDIVISION'
	laTempStru[ALEN(laTempStru,1)-10  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-10  ,3] = 6
	laTempStru[ALEN(laTempStru,1)-10  ,4] = 0
	laTempStru[ALEN(laTempStru,1)-10  ,17] = 0
	laTempStru[ALEN(laTempStru,1)-10  ,18] = 0

	laTempStru[ALEN(laTempStru,1)-15  ,1] = 'DESC'
	laTempStru[ALEN(laTempStru,1)-15  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-15  ,3] = 20
	laTempStru[ALEN(laTempStru,1)-15  ,4] = 0
	laTempStru[ALEN(laTempStru,1)-15  ,17] = 0
	laTempStru[ALEN(laTempStru,1)-15  ,18] = 0

	laTempStru[ALEN(laTempStru,1)-14  ,1] = 'REP1'
	laTempStru[ALEN(laTempStru,1)-14  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-14  ,3] = 3
	laTempStru[ALEN(laTempStru,1)-14  ,4] = 0
	laTempStru[ALEN(laTempStru,1)-14  ,17] = 0
	laTempStru[ALEN(laTempStru,1)-14  ,18] = 0

	laTempStru[ALEN(laTempStru,1)-13  ,1] = 'REP2'
	laTempStru[ALEN(laTempStru,1)-13  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-13  ,3] = 3
	laTempStru[ALEN(laTempStru,1)-13  ,4] = 0
	laTempStru[ALEN(laTempStru,1)-13  ,17] = 0
	laTempStru[ALEN(laTempStru,1)-13  ,18] = 0
	*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

	laTempStru[ALEN(laTempStru,1)-12  ,1] = 'Status'
	laTempStru[ALEN(laTempStru,1)-12  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-12  ,3] = 1
	laTempStru[ALEN(laTempStru,1)-12  ,4] = 0
	laTempStru[ALEN(laTempStru,1)-12  ,17] = 0
	laTempStru[ALEN(laTempStru,1)-12  ,18] = 0

	laTempStru[ALEN(laTempStru,1)-11  ,1] = 'priority'
	laTempStru[ALEN(laTempStru,1)-11  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-11  ,3] = 3
	laTempStru[ALEN(laTempStru,1)-11  ,4] = 0
	laTempStru[ALEN(laTempStru,1)-11  ,17] = 0
	laTempStru[ALEN(laTempStru,1)-11  ,18] = 0

	*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
	*!*    laTempStru[ALEN(laTempStru,1)-10  ,1] = 'hdr_Rep1'
	*!*    laTempStru[ALEN(laTempStru,1)-10  ,2] = 'C'
	*!*    laTempStru[ALEN(laTempStru,1)-10  ,3] = 3
	*!*    laTempStru[ALEN(laTempStru,1)-10  ,4] = 0
	*!*    laTempStru[ALEN(laTempStru,1)-10  ,17] = 0
	*!*    laTempStru[ALEN(laTempStru,1)-10 ,18] = 0
	*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

	laTempStru[ALEN(laTempStru,1)-9  ,1] = 'cdesc'
	laTempStru[ALEN(laTempStru,1)-9 ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-9  ,3] = 35
	laTempStru[ALEN(laTempStru,1)-9 ,4] = 0
	laTempStru[ALEN(laTempStru,1)-9 ,17] = 0
	laTempStru[ALEN(laTempStru,1)-9,18] = 0

	laTempStru[ALEN(laTempStru,1)-8 ,1] = 'btname'
	laTempStru[ALEN(laTempStru,1)-8  ,2] = 'C'
	laTempStru[ALEN(laTempStru,1)-8  ,3] = 30
	laTempStru[ALEN(laTempStru,1)-8 ,4] = 0
	laTempStru[ALEN(laTempStru,1)-8  ,17] = 0
	laTempStru[ALEN(laTempStru,1)-8 ,18] = 0
	lnCntDWn = 7
	For lnM = 1 To 8
		lcM = Str(lnM,1)
		laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,1] = 'SZ'+lcM
		laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,2] = 'C'
		laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,3] = 5
		laTempStru[ALEN(laTempStru,1)-lnCntDWn  ,4] = 0
		laTempStru[ALEN(laTempStru,1)-lnCntDWn   ,17] = 0
		laTempStru[ALEN(laTempStru,1)-lnCntDWn  ,18] = 0
		lnCntDWn = lnCntDWn -  1
	Endfor
	*! B609259,1 MMT 05/19/2010 SO order detail report export to excel does not export some fields[End]

	*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start
	*StyGrpDesc
	*ClrCodeDesc
	Local lnColumns
	*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
	*lnColumns = ALEN(laTempStru,1) + 2
	*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
	*lnColumns = ALEN(laTempStru,1) + 3
	*! C201957,1 Sara.O 03/01/2017 Add Notes to Order Detail Export [Begin]
	*lnColumns = ALEN(laTempStru,1) + 6
	lnColumns = Alen(laTempStru,1) + 8
	*! C201957,1 Sara.O 03/01/2017 Add Notes to Order Detail Export [End]
	*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]
	*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]
	Dimension laTempStru[lnColumns, 18]
	laTempStru[lnColumns - 1 ,1] = 'StyGrpDsc'
	laTempStru[lnColumns - 1 ,2] = 'C'
	laTempStru[lnColumns - 1 ,3] = 30
	laTempStru[lnColumns - 1 ,4] = 0
	laTempStru[lnColumns - 1 ,17] = 0
	laTempStru[lnColumns - 1 ,18] = 0

	laTempStru[lnColumns ,1] = 'ClrCodDsc'
	laTempStru[lnColumns ,2] = 'C'
	laTempStru[lnColumns ,3] = 30
	laTempStru[lnColumns ,4] = 0
	laTempStru[lnColumns ,17] = 0
	laTempStru[lnColumns ,18] = 0
	*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
	*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
	laTempStru[lnColumns - 2 ,1] = 'approval'
	laTempStru[lnColumns - 2 ,2] = 'C'
	laTempStru[lnColumns - 2 ,3] = 10
	laTempStru[lnColumns - 2 ,4] = 0
	laTempStru[lnColumns - 2 ,17] = 0
	laTempStru[lnColumns - 2 ,18] = 0
	*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]
	*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
	laTempStru[lnColumns - 3 ,1] = 'Note2'
	laTempStru[lnColumns - 3 ,2] = 'C'
	laTempStru[lnColumns - 3 ,3] = 30
	laTempStru[lnColumns - 3 ,4] = 0
	laTempStru[lnColumns - 3 ,17] = 0
	laTempStru[lnColumns - 3 ,18] = 0

	laTempStru[lnColumns - 4 ,1] = 'Note1'
	laTempStru[lnColumns - 4 ,2] = 'C'
	laTempStru[lnColumns - 4 ,3] = 30
	laTempStru[lnColumns - 4 ,4] = 0
	laTempStru[lnColumns - 4 ,17] = 0
	laTempStru[lnColumns - 4 ,18] = 0

	laTempStru[lnColumns - 5 ,1] = 'ccontref'
	laTempStru[lnColumns - 5 ,2] = 'C'
	laTempStru[lnColumns - 5 ,3] = 30
	laTempStru[lnColumns - 5 ,4] = 0
	laTempStru[lnColumns - 5 ,17] = 0
	laTempStru[lnColumns - 5 ,18] = 0

	*! C201957,1 Sara.O 03/01/2017 Add Notes to Order Detail Export [Begin]
	laTempStru[lnColumns - 6 ,1] = 'note_mem1'
	laTempStru[lnColumns - 6 ,2] = 'M'
	laTempStru[lnColumns - 6 ,3] = 30
	laTempStru[lnColumns - 6 ,4] = 0
	laTempStru[lnColumns - 6 ,17] = 0
	laTempStru[lnColumns - 6 ,18] = 0

	laTempStru[lnColumns - 7 ,1] = 'note_mem2'
	laTempStru[lnColumns - 7 ,2] = 'M'
	laTempStru[lnColumns - 7 ,3] = 30
	laTempStru[lnColumns - 7 ,4] = 0
	laTempStru[lnColumns - 7 ,17] = 0
	laTempStru[lnColumns - 7 ,18] = 0
	*! C201957,1 Sara.O 03/01/2017 Add Notes to Order Detail Export [End]

	*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]
	*-- Create temporary file that holding order line data. [end]
	llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
Endif  && end if it's first time you run option Grid.

*-- Create temporary cursors from structure array. [begin]
If Empty(lcMastFile) Or !Used(lcMastFile)
	*-- Setting for report [begin]
	lcSetHour = Set('HOURS')
	Set Hours To 24
	*-- Setting for report [end]
	*-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes
	*--           : and notepad from notepad file.
	*--           : note that this name and temp. file is created
	*--           : one for every optional grid seasson run.
	lcNoteLns = gfTempName()
	*-- create temp. file that used if you have both types of notes. [begin]
	Create Cursor (lcNoteLns)  (cRecord C(2))
	*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
	*INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
	Index On cRecord Tag (lcNoteLns)
	*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
	For lnI = 1 To 2
		Append Blank
		Replace cRecord With "N"+Alltrim(Str(lnI))
	Endfor
	*-- create temp. file that used if you have both types of notes. [end]
	*-- Create work file.
	= lfCreatCur(lcWorkFile)  && Create work cursor.
	= lfCreatCur(lcTempLine)  && Create line cursor.
Endif
*-- Create temporary cursors from structure array. [end]
= lfGetRepVr()      && Get Report variables such as groups and index.
llOGFltCh=.T.
*-- If user change report critria, Collect report data.
If llClearFn Or llOGFltCh

	llClearFn = .F.

	lcStartSt = Dtos(Ctod(Substr(laOGFxFlt[lnStartPos,6],1,;
		ATC('|',laOGFxFlt[lnStartPos,6])-1)))
	lcStartEd = Dtos(Ctod(Substr(laOGFxFlt[lnStartPos,6],;
		ATC('|',laOGFxFlt[lnStartPos,6])+1)))

	*B802444,1 Depend on both sides Flag when collecting data [Begin]
	llSrtSides = Empty(Alltrim(lcStartSt+lcStartEd))
	*B802444,1 Depend on both sides Flag when collecting data [End  ]

	lcCompSt  = Dtos(Ctod(Substr(laOGFxFlt[lnCompPos,6],1,;
		ATC('|',laOGFxFlt[lnCompPos,6])-1)))
	lcCompEd  = Dtos(Ctod(Substr(laOGFxFlt[lnCompPos,6],;
		ATC('|',laOGFxFlt[lnCompPos,6])+1)))

	*B802444,1 Depend on both sides Flag when collecting data [Begin]
	llCmpSides = Empty(Alltrim(lcCompSt + lcCompEd))
	*B802444,1 Depend on both sides Flag when collecting data [End  ]

	lcStatusEx = [ORDHDR.STATUS $ lcRpStatus]

	*B802113,1 All in list codes Changed to be $ not INLIST function
	*B802113,1 1- Style group. [Begin]
	*-- if user select Style group, evaluate its expression.
	If !Empty(laOGFxFlt[lnGrpPos,6])
		lcGrpExp  = "&laOGFxFlt[lnGrpPos,1]." + ' $ laOGFxFlt[lnGrpPos,6]'
	Endif  && end if user select Style group, evaluate its expression.
	*B802113,1 1- Style group. [End  ]

	*B802113,1 All in list codes Changed to be $ not INLIST function
	*B802113,1 2- Season. [Begin]
	*-- if user select Season, evaluate its expression.
	If !Empty(laOGFxFlt[lnSeaPos,6])
		*lnPipeNum  = OCCUR('|',laOGFxFlt[lnSeaPos,6])
		*lcSeaExp   = IIF(lnPipeNum = 0 , '"' + laOGFxFlt[lnSeaPos,6] + '"' ,;
		*             lfPipeExpr(laOGFxFlt[lnSeaPos,6],lnPipeNum))
		*lcSeaExp   = 'INLIST(ORDHDR.SEASON,' + lcSeaExp + ')'
		lcSeaExp  = "&laOGFxFlt[lnSeaPos,1]." + ' $ laOGFxFlt[lnSeaPos,6]'
	Endif  && end if user select Season, evaluate its expression.
	*B802113,1 2- Season. [End  ]

	*B802113,1 All in list codes Changed to be $ not INLIST function
	*B802113,1 3- Division. [Begin]
	*-- if user select Division, evaluate its expression.
	If !Empty(laOGFxFlt[lnDivPos,6])
		*lnPipeNum  = OCCUR('|',laOGFxFlt[lnDivPos,6])
		*lcDivExp   = IIF(lnPipeNum = 0 , '"' + laOGFxFlt[lnDivPos,6] + '"' ,;
		*             lfPipeExpr(laOGFxFlt[lnDivPos,6],lnPipeNum))

		*lcDivExp   = 'INLIST(ORDHDR.CDIVISION,' + lcDivExp + ')'
		lcDivExp  = "&laOGFxFlt[lnDivPos,1]." + ' $ laOGFxFlt[lnDivPos,6]'
	Endif  && if user select Division, evaluate its expression.
	*B802113,1 3- Division. [End  ]

	*C101557,1 ,Add Order Category to filter. [Begin]
	If !Empty(laOGFxFlt[lnCatPos,6])
		lcCatExp  = "&laOGFxFlt[lnCatPos,1]." + ' $ laOGFxFlt[lnCatPos,6]'
	Endif
	*C101557,1 ,Add Order Category to filter. [End  ]

	*-- Evaluate Color/Free Expression. [begin]
	*-- Note that: We use either Only This XXX color object or direct XXX
	*--            Free object, and you must know that both types of
	*--            expressions can't be enable at the same time.

	*B802101,1 Adjust color filter to depend to variable postions. [Begin]
	* lcListExp = ''
	*-- if Style Non major have Color segment.
	* IF !EMPTY(laOGFxFlt[lnClrSgPos,6])
	*   lcListExp = IIF(OCCUR('|',laOGFxFlt[lnClrSgPos,6]) = 0,;
	*               PADR(laOGFxFlt[lnClrSgPos,6],6),;
	*               lfMakeExpr(laOGFxFlt[lnClrSgPos,6]))
	* ENDIF  && end if Style have Color segment.

	*-- if Style  Non major does not have Color segment
	*-- but have free segment(S).
	* IF !EMPTY(laOGFxFlt[lnFreSgPos,6])
	*   lcListExp = IIF(OCCUR('|',laOGFxFlt[lnFreSgPos,6]) = 0,;
	*               PADR(laOGFxFlt[lnFreSgPos,6],6),;
	*               lfMakeExpr(laOGFxFlt[lnFreSgPos,6]))
	* ENDIF  && end if Style  Non major does not have Color segment.

	*-- lcCrFrExp : Color Or free seg. expr.
	*-- if you have Style non major Coler or free segment.

	*lcCrFrExp = IIF(EMPTY(lcListExp),'',[SUBSTR(STYLE.STYLE,9,6) $ lcListExp])

	lcCrFrExp = ''
	If Empty(laOGFxFlt[lnClrSgPos,6])
		If !Empty(laOGFxFlt[lnFreSgPos,6])
			lcCrFrExp  = "&laOGFxFlt[lnFreSgPos,1]." + ' $ laOGFxFlt[lnFreSgPos,6]'
		Endif
	Else
		lcCrFrExp  = "&laOGFxFlt[lnClrSgPos,1]." + ' $ laOGFxFlt[lnClrSgPos,6]'
	Endif

	* lcCrFrExp  = IIF(EMPTY(lcListExp),'',IIF(EMPTY(laOGFxFlt[lnClrSgPos,6]),;
	*            "&laOGFxFlt[lnFreSgPos,1].","&laOGFxFlt[lnClrSgPos,1].") + ' $ lcListExp')
	*B802101,1 Adjust color filter to depend to variable postions. [End  ]
	*! C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [START]
	Store '' To lcPackExp
	lnPackPos  =lfItmPos('SPCK_HDR.PACK_ID')
	If lnPackPos > 0 Then
		lnPackPos = Asubscript(laOGFxFlt,lnPackPos,1)
		Acopy(laOGFxFlt,laPackFlt, lnPackPos  ,8)
		Dime laPackFlt[1,8]
		lcPackExp = gfGenFlt('laPackFlt',.T.,.T.)
	Endif
	lcPackExp = Iif(Empty(lcPackExp),".T.",lcPackExp)


	Private lnOrderPos
	lnOrderPos = lfItmPos('ORDHDR.ORDER')   && get Get Order Fixed filter Position
	If (Upper(laOGFxFlt[lnOrderPos,5]) = "IN LIST" And Used(laOGFxFlt[lnOrderPos,6]) And Reccount(laOGFxFlt[lnOrderPos,6])=1)
		If lfvChkOrd()
			Do lpvGMAStor
		Endif
	Endif
	*! C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [END]

	*-- Evaluate Color/Free Expression. [end]
	lcLastExpr = lcRpExp   && To later know that user change critria.


	*! B608753,1 MMT 12/04/2008 Fix bug of wrong Data when use Option "Filter by Style.Season"[Start]
	If !Empty(lcSeaExp)
		If lcRpSeaFlt = 'S'
			lcSeaExp  = Strtran(lcSeaExp,'ORDHDR.SEASON','STYLE.SEASON')
		Else
			lcSeaExp  = Strtran(lcSeaExp,'STYLE.SEASON','ORDHDR.SEASON')
		Endif
	Endif
	*! B608753,1 MMT 12/04/2008 Fix bug of wrong Data when use Option "Filter by Style.Season"[End]


	= lfScanData()  && Scan around master file.

	*-- Unrise all Critria variables.
	Store .F. To llChSelect,llChStatus,llChCoord,llChSumm,;
		llChAcc,llChStyle,llChFabric,llChRep,llChOrder,llChLoc

Else  &&  user does not change report critria.

	*-- if user Sort By .
	If lcLastTag != lcIndexTg
		Select (lcMastFile)
		Index On &lcIndexTg Tag (lcMastFile)
		lcLastTag = lcIndexTg  && To later know that user change Sort case.
	Endif  && end if user Sort By .
Endif       && end If user change report critria, Collect report data.

Select ORDLINE
Set Relation Off Into ORDHDR  && break relation.
Set Relation Off Into Style  && break relation.

Set Relation To Pack_id+cPkColor+cPckSize+cPkVersion Into Spck_Hdr Additive

*-- If Sort by Sales Rep. , set relation to Primary sales rep. file.
If lcRpSortBy = 'R'
	Select ORDHDR
	Set Relation To REP1 Into SALESREP Additive
Endif  && end If Sort by Sales Rep.

*-- Temporary File relations, in the way that help us in report form [begin]
Select (lcMastFile)
Set Relation To cOrdType + Order Into ORDHDR      && To order header file.
Set Relation To Style Into Style Additive         && To style file.
Set Relation To 'S' + Scale Into Scale Additive   && To scale file.
Set Relation To cWareCode Into WAREHOUS Additive  && To warehouse file.

lcCustRel = Iif(llRpSummMt,['M' + Account],;
	[IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])

Set Relation To &lcCustRel Into CUSTOMER Additive  && To customer file.

*-- If sort by style group , set relation to codes file.
If lcRpSortBy = 'G'
	*B610529,1 TMI 09/25/2013 [Start] the first 19 characters are the fabric
	*SET RELATION TO gcAct_Comp+SUBSTR(cTempKey,8,6) INTO CODES ADDITIVE
	Set Relation To gcAct_Comp+Substr(cTempKey,20,6) Into CODES Additive
	*B610529,1 TMI 09/25/2013 [End  ]
Endif  && end If sort by style group.

*-- If sort by fabric , set relation to fabric file.
If lcRpSortBy = 'F'
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
	*SET RELATION TO LEFT(cTempKey,7) INTO Fabric ADDITIVE
	Set Relation To Left(cTempKey,19) Into (lcTmpFab) Additive
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[END]
Endif  && end If sort by fabric.

*-- if you print both type of notes.
If llPrntBoth
	Select (lcMastFile)
	Set Relation To 'N' Into (lcNoteLns) Additive
	Set Skip To &lcNoteLns
Endif   && end if you print both type of notes.

*-- if you print order notepad, open master file in another alias to
*-- help us to know what last line in order group to print notepad
*-- after it, note we do this because we print notepad in detail band
*-- not in order group band .

If llRpOrdNot
	Use (gcWorkDir+lcMastFile) Order Tag (lcMastFile) In 0 Again Alias GETLAST
Endif  && end if you print order notepad.

lcScaleGrp = Iif(llRpScale,[STYLE.SCALE],[''])  && group to print size scale.

lcRepExpr = [IIF(llPrntBoth,;
             IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcMastFile) = lnLastRec ,.T.),;
             .T.)]    && Report expression.

*-- Select Master report file.
Select (lcMastFile)

*B803742,1 (Begin) filter lines for TOTQTY <>0
*B606069,1 RAE Stop filtering on TOTQTY <> 0 only [Start]
*SET FILTER TO TOTQTY <> 0
*-hfk, 08/10/2004, to add the customized filters in the filter expression [Start]
* DIFF [START]
If llOneMulti And !Empty(laStores)
	Set Filter To (TOTQTY <> 0 .Or. ORDHDR.cOrdType = 'T') ;
		.And. (Ascan(laStores,Alltrim(Store)) <> 0)
Else
	Set Filter To TOTQTY <> 0 .Or. ORDHDR.cOrdType = 'T'
Endif
* DIFF [END]

If Alen(loOGScroll.laOgVrFlt,1)>0
	lcNewExp = ''
	For lnVarCount = 1 To Alen(loOGScroll.laOgVrFlt,1)
		If Empty(lcNewExp)
			lcNewExp = lcNewExp  + Iif(Empty(loOGScroll.laOgVrFlt[lnVarCount,8]),'',loOGScroll.laOgVrFlt[lnVarCount,8])
		Else
			lcNewExp = lcNewExp + " .AND. " + Iif(Empty(loOGScroll.laOgVrFlt[lnVarCount,8]),'',loOGScroll.laOgVrFlt[lnVarCount,8])
		Endif
	Endfor
Endif

*-hfk, 08/10/2004, to add the customized filters in the filter expression [End]
*B609608,1 MMT 06/09/2011 Order detail report deos not filter lines for selected store[Start]
*!*	IF TYPE('lcNewExp')='U' .OR. EMPTY(lcNewExp)
*!*	  SET FILTER TO TOTQTY <> 0 .OR. OrdHdr.cOrdType = 'T'
*!*	ELSE
*!*	  SET FILTER TO (TOTQTY <> 0 .OR. OrdHdr.cOrdType = 'T') .AND. &lcNewExp
*!*	ENDIF
If (Type('lcNewExp')='U' .Or. Empty(lcNewExp))
	If llOneMulti And !Empty(laStores)
		Set Filter To (TOTQTY <> 0 .Or. ORDHDR.cOrdType = 'T') ;
			.And. (Ascan(laStores,Alltrim(Store)) <> 0)
	Else
		Set Filter To TOTQTY <> 0 .Or. ORDHDR.cOrdType = 'T'
	Endif
Else
	If llOneMulti And !Empty(laStores)
		Set Filter To (TOTQTY <> 0 .Or. ORDHDR.cOrdType = 'T') ;
			.And. (Ascan(laStores,Alltrim(Store)) <> 0) .And. &lcNewExp
	Else
		*! E303255,2 MMT 10/17/2012 Cosnider the user defined filters while filtering report data[Start]
		*SET FILTER TO TOTQTY <> 0 .OR. OrdHdr.cOrdType = 'T' .AND. &lcNewExp
		Set Filter To (TOTQTY <> 0 .Or. ORDHDR.cOrdType = 'T') .And. &lcNewExp
		*! E303255,2 MMT 10/17/2012 Cosnider the user defined filters while filtering report data[End]
	Endif
Endif
*B609608,1 MMT 06/09/2011 Order detail report deos not filter lines for selected store[End]
*B606069,1 RAE [End]
*B803742,1 (End)

Go Bottom
lnLastOne  = Recno()
Go Top    && Refresh Relation

*E301265,1 if dos mode do the following [Begin
If llTextMode
	Store ' ' To lcInnGrpIn,lcOutGrpIn,lcInnGrpOp,lcOutGrpOp
	Store '' To lcPrnInnL
	Store 0 To lnInnQty1,lnInnQty2,lnInnQty3,lnInnQty4,;
		lnInnQty5,lnInnQty6,lnInnQty7,lnInnQty8,;
		lnInnTtQty,lnGrInnAmt
	Store 0 To lnPrnInnQ1,lnPrnInnQ2,lnPrnInnQ3,lnPrnInnQ4,;
		lnPrnInnQ5,lnPrnInnQ6,lnPrnInnQ7,lnPrnInnQ8,;
		lnPrnInnTQ,lnPrnInnAm

	*C101569,1 Add sort by store if first sort is by order [Begin]
	llLstMulti = (ORDHDR.Multi = "Y")
	*C101569,1 Add sort by store if first sort is by order [End  ]

	=lfInnGrpOp()

	Store '' To lcPrnOutL
	Store 0 To lnOutQty1,lnOutQty2,lnOutQty3,lnOutQty4,;
		lnOutQty5,lnOutQty6,lnOutQty7,lnOutQty8,;
		lnOutTtQty,lnGrOutAmt
	Store 0 To lnPrnOutQ1,lnPrnOutQ2,lnPrnOutQ3,lnPrnOutQ4,;
		lnPrnOutQ5,lnPrnOutQ6,lnPrnOutQ7,lnPrnOutQ8,;
		lnPrnOutTQ,lnPrnOutAm

	=lfOutGrpOp()

	*-- Avoid do any thing in .FRXs
	lcRpSort2 = Iif(lcRpSrt2$"TYI","T",lcRpSrt2)
Endif
*E301265,1 if dos mode do the following [End

lcEdTime = Time()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)

*E301265,1 if dos mode do the following [Begin
If llTextMode
	*N000682 ,1 Thabet Handle globalization issues [Start]
	*  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
	Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selected,oAriaApplication.GetHeaderText("LANG_Selected",AHEADERFILE)) + Alltrim(Str(Reccount(lcMastFile))) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records_in,oAriaApplication.GetHeaderText("LANG_Records_in",AHEADERFILE)) + Alltrim(Str(lnInterval,6,2)) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Seconds,oAriaApplication.GetHeaderText("LANG_Seconds",AHEADERFILE)) Nowait
	* N000682 ,1 Thabet Handle globalization issues [END]
Else  && else window format
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*   WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMastFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 1
	Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selected,oAriaApplication.GetHeaderText("LANG_Selected",AHEADERFILE)) + Alltrim(Str(Reccount(lcMastFile))) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records_in,oAriaApplication.GetHeaderText("LANG_Records_in",AHEADERFILE)) + Alltrim(Str(lnInterval,6,2)) + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Seconds,oAriaApplication.GetHeaderText("LANG_Seconds",AHEADERFILE)) Timeout 1
	* N000682 ,1 Thabet Handle globalization issues [END]
Endif
loOGScroll.nfontsize = 7
* DIFF [START]
If !llRpPrTPak
	Do Case
	Case lcRpSortBy = 'A'    && Sort by Account#
		Index On  ACCOUNT+cOrdType+Order+Dist_ctr+Store+Pack_id+cPkColor+cPckSize+cPkVersion Tag lcPackINDX
	Case lcRpSortBy = 'O'    && Sort by Order#
		Index On cOrdType+Order+Dist_ctr+Store+Pack_id+cPkColor+cPckSize+cPkVersion Tag lcPackINDX
	Case lcRpSortBy = 'R'    && Sort by sales representative
		Index On Right(cTempKey,3)+Pack_id+cPkColor+cPckSize+cPkVersion+cOrdType+Order+Dist_ctr+Store Tag lcPackINDX
	Case lcRpSortBy = 'D'    && Sort by complete date
		Index On Dtos(Complete)+Pack_id+cPkColor+cPckSize+cPkVersion+cOrdType+Order+Dist_ctr+Store Tag lcPackINDX
	Case lcRpSortBy = 'W'    && Sort by location
		Index On cWareCode+Pack_id+cPkColor+cPckSize+cPkVersion+cOrdType+Order Tag lcPackINDX
	Otherwise      && Sort by style,fabric,stygroup
		Index On Pack_id+cPkColor+cPckSize+cPkVersion+Dtos(Complete)+cOrdType+Order+Dist_ctr+Store Tag lcPackINDX
	Endcase
	llOGFltCh = .T.   &&Always recollect data since a new index is created, to restore the original one
	=lfUpType()
Endif
* DIFF [END]

Do gfDispRe With Eval('lcRpForm') , 'FOR ' + lcRepExpr

Wait Clear
*-- If Sort by Sales Rep. , set relation to Primary sales rep. file.
If lcRpSortBy = 'R'
	Private lcCurSel
	lcCurSel = Alias()
	Select ORDHDR
	Set Relation Off Into SALESREP
	Select (lcCurSel)
Endif

If llRpOrdNot
	Use In GETLAST
Endif

Set Status Bar &lcStatusBr
Return .T.
**************************************************
*-- Main program code                          END
**************************************************

*-- Function section
*-------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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

*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.

If Type('lnVarbEnd') = 'C'
	lnVarbEnd = 0

	Declare laRpSource[5]
	If Type('laRpTarget[1]') = 'C'

		If Empty(laRpTarget[1])
			Declare laRpTarget[1]
		Else
			For lnI = 2 To Alen(laRpTarget)
				If Type('laRpTarget[lnI]') = 'U'
					laRpTarget[lnI] = ""
				Endif
			Endfor
		Endif

	Else
		Declare laRpTarget[1]
	Endif
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*!*	  STORE 'Bid'      TO laRpSource[1]
	*!*	  STORE 'Open'     TO laRpSource[2]
	*!*	  STORE 'Hold'     TO laRpSource[3]
	*!*	  STORE 'Complete' TO laRpSource[4]
	*!*	  STORE 'Cancelled' TO laRpSource[5]
	*STORE LANG_BID      TO laRpSource[1]
	*STORE LANG_OPEN     TO laRpSource[2]
	*STORE LANG_HOLD     TO laRpSource[3]
	*STORE LANG_COMPLETE TO laRpSource[4]
	*STORE LANG_CANCEL  TO laRpSource[5]
	Store Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BID,oAriaApplication.GetHeaderText("LANG_BID",AHEADERFILE))      To laRpSource[1]
	Store Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE))     To laRpSource[2]
	Store Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE))     To laRpSource[3]
	Store Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE)) To laRpSource[4]
	Store Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE))  To laRpSource[5]
	* N000682 ,1 Thabet Handle globalization issues [END]
	lcRpStatus = 'BOHCX'

	Set Order To ORDHDR In ORDHDR      && To use it to validate ORDER   # in option grid.
	Set Order To CUSTOMER In CUSTOMER  && To use it to validate ACCOUNT # in option grid.
	Set Order To Style In Style        && To use it to validate STYLE   # in option grid.
	Set Order To SALESREP In SALESREP  && To use it to validate REP     # in option grid.
	Set Order To WAREHOUS In WAREHOUS  && To use it to validate LOCATION# in option grid.
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
	*IF 'MA' $ gcCmpModules
	*  SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
	*ENDIF
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[END]
	lnAccPos   = lfItmPos('CUSTOMER.ACCOUNT')
	lnStyPos   = lfItmPos('STYLE.STYLE')
	lnFabPos   = lfItmPos('FABRIC.FABRIC')
	lnLocPos   = lfItmPos('WAREHOUS.CWARECODE')
	lnRepPos   = lfItmPos('SALESREP.REPCODE')
	lnOrdPos   = lfItmPos('ORDHDR.ORDER')
	lnStartPos = lfItmPos('ORDLINE.START')
	lnCompPos  = lfItmPos('ORDLINE.COMPLETE')
	lnGrpPos   = lfItmPos('STYLE.CSTYGROUP')
	lnSeaPos   = lfItmPos('STYLE.SEASON')
	lnDivPos   = lfItmPos('STYLE.CDIVISION')
	lnPriPos   = lfItmPos('ORDHDR.PRIORITY')

	*C101557,1 ,Add Order Category to filter. [Begin]
	lnCatPos   = lfItmPos('ORDHDR.CORDERCAT')
	*C101557,1 ,Add Order Category to filter. [End  ]

	lnClrSgPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
	*-- Convert all ceiling functions to use lfItmPos because [End..
	lnFreSgPos = lnClrSgPos + 1


	*B602590,1 Adjust currency symbol [Begin]
	If llMultCurr
		Set Order To CCURRCODE In SYCCURR  && To VALIDATE currency code.
		lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
	Endif
	*B602590,1 Adjust currency symbol [End  ]

Else
	*FOR lnElm = 1 TO ALEN(laOgObjType,1)
	*IF laOgObjType[lnElm,1] == "lcRpSortBy"
	*  _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
	*ENDIF
	*IF lcDummy = "Y" AND laOgObjType[lnElm,1] == "lcRpSelcBy"
	*  _CUROBJ= OBJNUM(&laOgObjType[lnElm,2].)+1
	*  lcDummy = "N"
	*ENDIF
	*ENDFOR
Endif  && END IF you first time enter when function.
*-- Disable/enable By account, style, fabric, location, sales representative. [begin]
*-- note that disable and enable is according to value of laRpFltVal.

*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
If !llFrstTime
	lcTmpFab = loOGScroll.gfTempName()
	lcSelected = " SELECT ITEMLOC.TOTWIP,ITEMLOC.TOTSTK,ITEM.CSTYMAJOR AS FABRIC,ITEM.[DESC] FROM ITEM (INDEX = CSTYLE)INNER JOIN ITEMLOC (INDEX = STYDYE) ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEM.CDEFWARE = ITEMLOC.CWARECODE "
	lcWhereCondition = " ITEMLOC.DYELOT = '   ' AND ITEM.CINVTYPE = 0002 AND ITEMLOC.CINVTYPE = 0002 "
	lcSqlStatement = lcSelected + " WHERE " + lcWhereCondition
	lnResult1 = loOGScroll.orda.SqlRun (lcSqlStatement,lcTmpFab,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",Set("Datasession" ))
	llFrstTime = .T.
	If lnResult1 >= 1
		lnBuffering = CursorGetProp("Buffering",lcTmpFab)
		=CursorSetProp("Buffering",3,lcTmpFab)
		Select (lcTmpFab)
		Index On Fabric Tag &lcTmpFab
		Set Order To Tag &lcTmpFab
	Endif
Endif
*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]

*B603946,1 Clear SELECT BY if user change only SELECT BY option from
*B603946,1 one type to another or user presses RESET button [Begin]
*STORE .T. TO llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
If lcRpSelcBy = "L" Or !(lcRpSelcBy == lcOldSelc)
	Store .T. To llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
Endif
lcOldSelc = lcRpSelcBy
*B603946,1 Clear SELECT BY if user change only SELECT BY option [End]

*C101569,1 Add sort by store if first sort is by order [Begin]
If lcRpSortBy = "O" And lcRpKind = "D" And Alen(laSort2Des,1) < 5
	Dimension laSort2Des[5,1] , laSort2Val[5,1]
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*laSort2Des[3,1] = "Store/Line#"
	laSort2Des[3,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STORELINE,oAriaApplication.GetHeaderText("LANG_STORELINE",AHEADERFILE))
	*N000682,1 11/20/2012 MMT Globlization changes[End]

	laSort2Val[3,1] = "T"
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*laSort2Des[4,1] = "Store/" + lcStyMajor
	laSort2Des[4,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STOREBACK,oAriaApplication.GetHeaderText("LANG_STOREBACK",AHEADERFILE))+ lcStyMajor
	* N000682 ,1 Thabet Handle globalization issues [END]

	laSort2Val[4,1] = "Y"
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*laSort2Des[5,1] = "DC/" + lcStyMajor  && Sort by DC/Style
	laSort2Des[5,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DCBACK,oAriaApplication.GetHeaderText("LANG_DCBACK",AHEADERFILE)) + lcStyMajor  && Sort by DC/Style
	* N000682 ,1 Thabet Handle globalization issues [Start]
	laSort2Val[5,1] = "I"
	ClearRead()
Endif
*C101569,1 Add sort by store if first sort is by order [End  ]

*-C200438,1  MHM add Sort by DC/Style to Account too [START]
If lcRpSortBy = "A" And lcRpKind = "D" And Alen(laSort2Des,1) = 2
	Dimension laSort2Des[3,1] , laSort2Val[3,1]
	laSort2Des[3,1] = "DC/" + lcStyMajor
	laSort2Val[3,1] = "I"
	ClearRead()
Endif

*B604748,1 Check the pre-saved filter setting. [Begin]
lcRpStatus = ' '
*-- Loop to make Status expression.
If !Empty(laRpTarget[1])
	For lnI = 1 To Alen(laRpTarget,1)
		* N000682 ,1 Thabet Handle globalization issues [Start]
		*!*	    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
		*!*	                              IIF(laRpTarget[lnI] = 'Open','O',;
		*!*	                              IIF(laRpTarget[lnI] = 'Hold','H',;
		*!*	                              IIF(laRpTarget[lnI] = 'Complete','C',;
		*!*	                              IIF(laRpTarget[lnI] = 'Cancelled','X','')))))
		lcRpStatus = lcRpStatus + Iif(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BID,oAriaApplication.GetHeaderText("LANG_BID",AHEADERFILE)) ,'B',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE)) ,'O',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE)) ,'H',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE)) ,'C',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE)) ,'X','')))))
		* N000682 ,1 Thabet Handle globalization issues [END]
	Endfor
Endif

lcRpStatus = Alltrim(lcRpStatus)
If Empty(lcRpStatus)
	lcRpStatus = "BOHCX"
	Declare laRpTarget[5]
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*!*	  laRpTarget[1] = "Bid"
	*!*	  laRpTarget[2] = "Open"
	*!*	  laRpTarget[3] = "Hold"
	*!*	  laRpTarget[4] = "Complete"
	*!*	  laRpTarget[5] = "Cancelled"
	*N000682,1 11/20/2012 MMT Globlization changes[Start]
	laRpTarget[1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BID,oAriaApplication.GetHeaderText("LANG_BID",AHEADERFILE))
	laRpTarget[2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE))
	laRpTarget[3] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE))
	laRpTarget[4] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE))
	laRpTarget[5] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE))
	* N000682 ,1 Thabet Handle globalization issues [END]
Endif
*B604748,1 Check the pre-saved filter setting. [End]

*E500342,1 Checking for the Order Status to Enable/Disable the 2 options which
*E500342,1 was added to the selection grid.
Do lpChkStat

*B608747 TMI 11/27/2008 [Start] resetting the clear variables as in the function lfvSelcBy so when the ClearRead that is
*                               called automatically from the when function rebuilds the temp files containing selected styles
llChSelect = .T.
llClearAcc = (lcRpSelcBy # 'A')
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')
llClearLoc = (lcRpSelcBy # 'L')
llClearRep = (lcRpSelcBy # 'R')
*B608747 TMI 11/27/2008 [End  ]

*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[Start]
If !Used('SPCK_HDRA')
	=gfOpenTable('SPCK_HDR','SPCK_HDRST','SH','SPCK_HDRA')
Endif
*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[End]

*! B610385,1 SAB 06/18/2013 Fix bug of clearing the selected records of SO details filter [T20130613.0011][Start]
If loOGScroll.lnOGSeting > 1
	Store .F. To llClearAcc, llClearSty, llClearFab, llClearLoc, llClearRep, llClearOrd
Endif
*! B610385,1 SAB 06/18/2013 Fix bug of clearing the selected records of SO details filter [T20130613.0011][End]

*-- end of lfwRepWhen.

*!**************************************************************************
*! Name      : lfwOldSelc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/08/2000
*! Purpose   : To store the Old Select By value
*!**************************************************************************
*! Example   : = lfwOldSelc()
*!**************************************************************************
Function lfwOldSelc

lcOldSelc = lcRpSelcBy
*-- End of lfwOldSelc.

*!**************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/10/2000
*! Purpose   : To get the position of the Variable in OG.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfVarPos()
*!**************************************************************************
*E500342,1 This function was added in order to get the Position of the 2 options
*E500342,1 Added in Selection grid so as to Enable/Disable them.
Function lfVarPos
Parameters lcItmInFlt

lcItmInFlt = Upper(Alltrim(lcItmInFlt))
Private lnItmPos
lnItmPos = Ascan(laOGObjType,lcItmInFlt)
If lnItmPos > 0
	lnItmPos = Asubscript(laOGObjType,lnItmPos,1)
Endif
Return lnItmPos
*-- End of lfVarPos.

*!**************************************************************************
*! Name      : lpChkStat
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/10/2000
*! Purpose   : To check for Order status in Order to Enable/Disable the 2
*!             Option added to the Selection grid.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : DO lpChkStat
*!**************************************************************************
*E500342,1 This function was added in order to check for the Order Status so
*E500342,1 as to Enable/Disbale the 2 options added in Selection grid.
Procedure lpChkStat
Private llStatus
llStatus = Iif('O' $ lcRpStatus Or 'H' $ lcRpStatus,.T.,.F.)
If !llStatus
	llRpPrtShp = .F.
	llRpAloOrd = .T.
Endif

*-- Disable (Partially shipped Orders & Include Allocated Order Lines) if Order Status
*-- contains only Complete or Cancelled
*LOCAL lnValPos
*lnValPos = lfVarPos('llRpPrtShp')
*IF lnValPos > 0
*  laOGObjCnt[lnValPos] = llStatus
*  =lfOGShowGet('llRpPrtShp')
*ENDIF
*-- MAB - New in version 4 instead of old lfOGShowGet
loOGScroll.EnableObject("llRpPrtShp",llStatus)

*lnValPos = lfVarPos('llRpAloOrd')
*IF lnValPos > 0
*  laOGObjCnt[lnValPos] = llStatus
*  =lfOGShowGet('llRpAloOrd')
*ENDIF
*-- MAB - New in version 4 instead of old lfOGShowGet
loOGScroll.EnableObject("llRpAloOrd",llStatus)
*-- End of lpChkStat.

*!**************************************************************************
*! Name      : lfvDisMesg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/10/2000
*! Purpose   : Diaplay message to inform user to ignore Complete,Cancel,Bid Orders
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfvDisMesg()
*!**************************************************************************
*E500342,1 This function was added in order to get the Position of the 2 options
*E500342,1 Added in Selection grid so as to Enable/Disable them.
Function lfvDisMesg
Parameters lcLine

If lcLine = "A"
	If llRpPrtShp And ('B' $ lcRpStatus Or 'C' $ lcRpStatus Or 'X' $ lcRpStatus)
		*-- Message < This option will be applied only on Hold and Open Orders, it will >
		*--         < ignore other orders status.                                       >
		*-- Buttons <                                OK                                 >
		= gfModalGen("INM32088B00000","Dialog")
	Endif
Else
	If !llRpAloOrd And ('B' $ lcRpStatus Or 'C' $ lcRpStatus Or 'X' $ lcRpStatus)
		*-- Message < This option will be applied only on Hold and Open Orders, it will >
		*--         < ignore other orders status.                                       >
		*-- Buttons <                                OK                                 >
		= gfModalGen("INM32088B00000","Dialog")
	Endif
Endif
*-- End of lfvDisMesg.

*!*************************************************************
*! Name      : lfScanData
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
*! Purpose   : Collect report data.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfSumStyle,lfSumMulti
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfScanData()
*!*************************************************************
*! Notes     : To easy understand documentaion of this function
*!           : keep in your mind that
*!           : 1- <ordhdr filter>  is for : open quantity, order status,;
*!           :                              season, division and priority.
*!           : 2- <ordline filter> is for : total quantity, line group,;
*!           :                              start date, complete date.
*!           : 3- <style group filter> is for   : style group
*!           : 4- <Coler Filter>  is for   : Color/Free.
*!           : 5- There is relation between ordline file and both ordhdr and ;
*!           :    style files, to easy lock its data.
*!           : 6- Because we have a field called cTempKey in temp. files
*!           :    we fill its memory(m.cTempKey) with the required data
*!           :    that help us collecting data rush more and fast printing
*!           :    in the report without evaluating values that spent time.
*!           : 7- lcSeekExp, is expression we sum rush more for it in case
*!           :    of summarize multi store orders.
*!*************************************************************
Function lfScanData


*-- If you find any data (i.e: not first time you run), clear it.
If Reccount(lcTempLine) > 0
	*-- We need temp. files to be files not cursor to open it in another alias [Begin]
	*USE IN (lcTempLine)
	*= lfCreatCur(lcTempLine)  && Create line cursor again.
	Select (lcTempLine)
	Zap

	*E301265,1==E301272,1 Rest any relation before data collection.
	Set Relation To

	*-- We need temp. files to be files not cursor to open it in another alias [End  ]
	llNoIndex = .T.
Endif  && end If you find any data.

*-- Change index due to changes to Sort By type.
If llNoIndex Or (lcLastTag != lcIndexTg)
	Select (lcTempLine)

	*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [Start]
	Index On Style + Dtos(Complete) + cOrdType + Order Tag (lcTempLineTag)
	*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [End]

	Index On &lcIndexTg Tag (lcTempLine)


	If llNoIndex
		llNoIndex = .F.
	Else
		lcLastTag = lcIndexTg
	Endif
Endif  && end if Change index.

*-- lcWorkTag : Variable which hold value of working file index.
*--           : Note that putting index this way help us in collecting
*--           : data rush more.
*B610529,1 TMI 09/25/2013 [Start] fabric now is 19
*lcWorkTag = IIF(lcRpSelcBy = 'A',[ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)]                ,;
IIF(lcRpSelcBy = 'S',[STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)]   ,;
IIF(lcRpSelcBy = 'F',[LEFT(cTempKey,7) +STYLE+CORDTYPE+ORDER+STR(LINENO,6)],;
IIF(lcRpSelcBy = 'W',[CWARECODE+CORDTYPE+ORDER+STR(LINENO,6)]              ,;
IIF(lcRpSelcBy = 'R',[RIGHT(cTempKey,3)+CORDTYPE+ORDER+STR(LINENO,6)]      ,;
lcIndexTg)))))
lcWorkTag = Iif(lcRpSelcBy = 'A',[ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)]                ,;
	IIF(lcRpSelcBy = 'S',[STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)]   ,;
	IIF(lcRpSelcBy = 'F',[LEFT(cTempKey,19) +STYLE+CORDTYPE+ORDER+STR(LINENO,6)],;
	IIF(lcRpSelcBy = 'W',[CWARECODE+CORDTYPE+ORDER+STR(LINENO,6)]              ,;
	IIF(lcRpSelcBy = 'R',[RIGHT(cTempKey,3)+CORDTYPE+ORDER+STR(LINENO,6)]      ,;
	lcIndexTg)))))
*B610529,1 TMI 09/25/2013 [End  ]
Select (lcWorkFile)
If Reccount(lcWorkFile) > 0
	*-- We need temp. files to be files not cursor to open it in another alias [Begin]
	*USE IN (lcWorkFile)
	*= lfCreatCur(lcWorkFile)  && Create work cursor again.
	Select (lcWorkFile)
	Zap

	*E301265,1==E301272,1 Rest any relation before data collection.
	Set Relation To
	*-- We need temp. files to be files not cursor to open it in another alias [End  ]

	Select (lcWorkFile)
Endif
Index On &lcWorkTag Tag (lcWorkFile)&&this line create .cdx in work dir (SMA)

*-- Relation with master order file to help data collecting. [begin]
Select ORDLINE
Set Relation To cOrdType + Order Into ORDHDR
Set Relation To Style Into Style Additive
*-- Relation with master order file to help data collecting. [end]

*-- llWorkDeal : Flag to know that we start dealing with work file.
*-- llLineDeal : Flag to know that we deal with temp. line file.
Store .F. To llWorkDeal , llLineDeal

*MAB HERE [Begin]
*lnOrdTrueP = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ORDER'),1)
*lcTOrdFile = laFxFltCur[ASUBSCRIPT(laFxFltCur,ASCAN(laFxFltCur,lnOrdTrueP),1),2]  && Name of order cursor (inlist func.).
*-- if you find order cursor.
*IF !EMPTY(lcTOrdFile) AND USED(lcTOrdFile)
*  SELECT (lcTOrdFile)
*  COUNT TO lnHaveRecs FOR !DELETED()
*  llWorkDeal = (lnHaveRecs > 0)  && .T. is there is data.
*ENDIF
*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
llPackSelected = .F.
lcCursorPack = ''
lnPosPack= Ascan(loOGScroll.laOGFxFlt,"ORDLINE.PACK_ID")
If lnPosPack = 0
	lnPosPack= Ascan(loOGScroll.laOGFxFlt,"ordline.pack_id")
Endif
If lnPosPack> 0
	lnPosPack = Asubscript(loOGScroll.laOGFxFlt,lnPosPack,1)
	lcCursorPack= loOGScroll.laOGFxFlt[lnPosPack,6]
	If !Empty(lcCursorPack)
		Select(lcCursorPack)
		Locate
		If !Eof()
			llPackSelected = .T.
		Endif
	Endif
Endif
*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][End]
lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !Empty(lcTOrdFile) And Used(lcTOrdFile) And Reccount(lcTOrdFile) > 0
*MAB HERE [End  ]
*-- If user select specific orders, collect data of this orders only. [begin]
If llWorkDeal
	Select (lcTOrdFile)
	*-- Scan order cursor.
	Scan
		Select ORDLINE
		Set Order To Tag ORDLINE
		If Iif(Empty(lcRpOrdTyp),Seek('O'+&lcTOrdFile..Order) Or Seek('T'+&lcTOrdFile..Order),Seek(lcRpOrdTyp+&lcTOrdFile..Order)) And;
				EVALUATE(lcStatusEx) And ;
				(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
				IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
				IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) And ;
				IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
				IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.) ;
				AND Iif(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))

			*C102262,1 (End)
			*E500342,1 Adding the 1st Option to the If condition [End]
			*E500342,1 Print Partially Shipped Orders (Y/N)
			*E500271,4 add the lcRpEdiFlt to the expression [End.]

			*-- Scan ordline file for rest order data.
			Scan Rest While cOrdType+Order+Str(Lineno,6) = cOrdType+&lcTOrdFile..Order
				*C102262,1 (End)
				*-- if <ordline filter> and <style group filter> and <Color Filter>
				*-- insert this data into workfile.
				*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
				*! B607998,1 SSH 07/03/2007 not include parially allocated orders
				*! SET STEP ON
				If Iif(llRpCorrGp ,!Empty(Group),.T.) And ;
						(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
						(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
						IIF(Empty(lcSeaExp),.T., Evaluate(lcSeaExp)) And ;
						IIF(Empty(lcDivExp),.T., Evaluate(lcDivExp)) And ;
						IIF(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp))  And ;
						IIF(Empty(lcCrFrExp) ,.T.,Evaluate(lcCrFrExp)) And ;
						IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
						IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik)))
					*! B607998,1 SSH [END]

					*B803674,1 Apply Partially Shipped on Order lines also [End]
					*E500342,1 Adding the 2 Options to the If condition [End]
					*E500342,1 Include Allocated Order Lines (Y/N)
					*E301421,1 ABD- [End]

					If lcRpSelcBy = 'A'
						lcTmpSty = laOGFxFlt[lnStyPos,6]
						If Used(lcTmpSty) And Reccount(lcTmpSty) > 0 And;
								!Seek(Substr(ORDLINE.Style,1,lnMajorLen),lcTmpSty)
							Loop
						Endif
					Endif
					*E500444,1 [End]
					*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
					If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
						Loop
					Endif
					*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]
					Scatter Memvar Memo

					*E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
					*E301421,1 ABD- And in case of Complete status we will print Booked Qty
					*E301421,1 ABD- Because the TotQty is equal to Zero. [Begin]

					*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
					*IF ORDHDR.STATUS = "C"
					If lcRpBkOp='B'
						*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
						*-- Get qty from book qty becauce qty =0
						For I = 1 To 8
							lcCount = Alltrim(Str(I))
							m.QTY&lcCount = BOOK&lcCount
						Endfor
						m.TOTQTY = TOTBOOK
					Endif
					*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
					If lcRpBkOp = 'A'
						For I = 1 To 8
							lcCount = Alltrim(Str(I))
							m.QTY&lcCount = QTY&lcCount - PIK&lcCount
						Endfor
						m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
					Endif
					*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]
					*E301421,1 ABD- [End]
					*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
					*m.cTempKey  = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
					m.cTempKey  = Padr(Style.cprifabric,19) + Padr(Style.CSTYGROUP,6) + Padr(ORDHDR.REP1,3)
					*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
					m.CCURRCODE = ORDHDR.CCURRCODE
					*B128367,1 MMT 02/07/2006 Fix bug of not displaying distribution center in excel formart [Start]
					=Seek('S'+m.ACCOUNT+m.Store,'CUSTOMER')
					m.Dist_ctr = CUSTOMER.Dist_ctr
					*B128367,1 MMT 02/07/2006 Fix bug of not displaying distribution center in excel formart [Start]

					*! B609259,1 MMT 05/19/2010 SO order detail report export to excel does not export some fields[Start]
					m.Status = ORDHDR.Status
					m.PRIORITY= ORDHDR.PRIORITY
					*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
					*m.hdr_Rep1 = Ordhdr.Rep1
					*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]
					=Seek('S' + Scale,'SCALE')
					=Seek(cWareCode,'WAREHOUS')
					m.cdesc = WAREHOUS.cdesc
					=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
					m.btname = CUSTOMER.btname
					m.SZ1  = Scale.SZ1
					m.SZ2  = Scale.SZ2
					m.SZ3  = Scale.SZ3
					m.SZ4  = Scale.SZ4
					m.SZ5  = Scale.SZ5
					m.SZ6  = Scale.SZ6
					m.SZ7  = Scale.SZ7
					m.SZ8  = Scale.SZ8
					*! B609259,1 MMT 05/19/2010 SO order detail report export to excel does not export some fields[End]
					*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
					m.CDIVISION = ORDHDR.CDIVISION
					m.REP1  = ORDHDR.REP1
					m.REP2  = ORDHDR.REP2
					=Seek(Style,'Style','Style')
					m.DESC  = Style.Desc

					*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

					*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
					lpGetClrSgStart()
					m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
					m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
					*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
					*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
					m.approval = ORDHDR.approval
					*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]

					*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
					m.ccontref = ORDHDR.ccontref
					m.Note1 = ORDHDR.Note1
					m.Note2 = ORDHDR.Note2
					*! C201957,1 Sara.O 03/01/2017 Add Notes to Order Detail Export [Begin]
					m.note_mem1 = m.note_mem
					*C202017,1 MHM 05/03/2017 problem in custom order detail [start]
					m.note_mem2 = ' '
					*C202017,1 MHM 05/03/2017 problem in custom order detail [END]
					
					If Seek('B' + Iif(ORDHDR.cOrdType = 'T','T','')+Order , 'NOTEPAD') And !Empty(Alltrim(NOTEPAD.mNotes))
						m.note_mem2 = Alltrim(NOTEPAD.mNotes)
					Endif
					*! C201957,1 Sara.O 03/01/2017 Add Notes to Order Detail Export [End]
					*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]
					Insert Into (lcWorkFile) From Memvar
				Endif  && end if <ordline filter>.
			Endscan  && end Scan ordline file for rest order data.
		Endif      && end if find first order record in ordline.
	Endscan      && end Scan order cursor.
	lcMastFile = lcWorkFile
	*!C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [START]
Else
	lnPackPos  =lfItmPos('SPCK_HDR.PACK_ID')
	*wael
	*lcPackFile = laOGFxFlt[lnPackPos,6]
	lcPackFile = Iif(lnPackPos>0,laOGFxFlt[lnPackPos,6],'')
	*wael
	llWorkDeal = !Empty(lcPackFile) And Used(lcPackFile) And Reccount(lcPackFile) > 0
	*-- If user select specific packs, collect data of this packs only. [begin]
	If llWorkDeal .And. lfCrPckInd()
		Set Index To (gcDataDir+'PACKINDX.IDX') Order PACKINDX
		Select (lcPackFile)
		Go Top
		Scan
			Select ORDLINE
			If Seek(&lcPackFile..Pack_id,'ORDLINE')
				Scan Rest While Pack_id+cPkColor+cPckSize+cPkVersion+Style = &lcPackFile..Pack_id
					If Iif(Empty(lcRpOrdTyp),.T.,ORDLINE.cOrdType = lcRpOrdTyp) .And. ;
							EVALUATE(lcStatusEx) And ;
							(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
							IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
							IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) And ;
							IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE $ laOGFxFlt[lnCurrPos,6],.T.) And ;
							IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.) ;
							AND Iif(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))
					Else
						Loop
					Endif
					*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
					*! B607998,1 SSH 07/03/2007 not include parially allocated orders
					If Iif(llRpCorrGp ,!Empty(Group),.T.) And ;
							(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
							(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
							IIF(Empty(lcSeaExp),.T., Evaluate(lcSeaExp)) And ;
							IIF(Empty(lcDivExp),.T., Evaluate(lcDivExp)) And ;
							IIF(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp))  And ;
							IIF(Empty(lcCrFrExp) ,.T.,Evaluate(lcCrFrExp)) And ;
							IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
							IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik)))
						*! B607998,1 SSH [END]
						If lcRpSelcBy = 'A'
							lcTmpSty = laOGFxFlt[lnStyPos,6]
							If Used(lcTmpSty) And Reccount(lcTmpSty) > 0 And;
									!Seek(Substr(ORDLINE.Style,1,lnMajorLen),lcTmpSty)
								Loop
							Endif
						Endif
						*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
						If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
							Loop
						Endif
						*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]
						Scatter Memvar Memo
						*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
						*IF ORDHDR.STATUS = "C"
						If lcRpBkOp='B'
							*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
							*-- Get qty from book qty becauce qty =0
							For I = 1 To 8
								lcCount = Alltrim(Str(I))
								m.QTY&lcCount = BOOK&lcCount
							Endfor
							m.TOTQTY = TOTBOOK
						Endif
						*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
						If lcRpBkOp = 'A'
							For I = 1 To 8
								lcCount = Alltrim(Str(I))
								m.QTY&lcCount = QTY&lcCount - PIK&lcCount
							Endfor
							m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
						Endif
						*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

						*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
						*m.cTempKey  = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
						m.cTempKey  = Padr(Style.cprifabric,19) + Padr(Style.CSTYGROUP,6) + Padr(ORDHDR.REP1,3)
						*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
						m.CCURRCODE = ORDHDR.CCURRCODE
						=Seek('S'+m.ACCOUNT+m.Store,'CUSTOMER')
						m.Dist_ctr = CUSTOMER.Dist_ctr
						*! B609259,1 MMT 05/19/2010 SO order detail report export to excel does not export some fields[Start]
						m.Status = ORDHDR.Status
						m.PRIORITY= ORDHDR.PRIORITY
						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
						*m.Ordhdr_Rep1 = Ordhdr.Rep1
						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]
						=Seek('S' + Scale,'SCALE')
						=Seek(cWareCode,'WAREHOUS')
						m.cdesc = WAREHOUS.cdesc
						=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
						m.btname = CUSTOMER.btname
						m.SZ1  = Scale.SZ1
						m.SZ2  = Scale.SZ2
						m.SZ3  = Scale.SZ3
						m.SZ4  = Scale.SZ4
						m.SZ5  = Scale.SZ5
						m.SZ6  = Scale.SZ6
						m.SZ7  = Scale.SZ7
						m.SZ8  = Scale.SZ8
						*! B609259,1 MMT 05/19/2010 SO order detail report export to excel does not export some fields[End]
						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
						m.CDIVISION = ORDHDR.CDIVISION
						m.REP1  = ORDHDR.REP1
						m.REP2  = ORDHDR.REP2
						=Seek(Style,'Style','Style')
						m.DESC  = Style.Desc
						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

						*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
						lpGetClrSgStart()
						m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
						m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
						*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
						*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
						m.approval = ORDHDR.approval
						*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]
						*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
						m.ccontref = ORDHDR.ccontref
						m.Note1 = ORDHDR.Note1
						m.Note2 = ORDHDR.Note2
						*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

						Insert Into (lcWorkFile) From Memvar
					Endif  && end if <ordline filter>.
				Endscan  && end Scan ordline file for rest order data.
			Endif      && end if find first order record in ordline.
		Endscan
		Set Order To &lcSvOrd In ORDLINE
	Endif
	lcMastFile = lcWorkFile
	*!C039139,1 SMM 04/04/2005 Merge Custom SO GMA to Standard SO [END]
Endif

*-- If user select specific orders, collect data of this orders only. [end]

*-- Know which type of select we use and its position [begin]
lnUsedItem = Iif(lcRpSelcBy = 'A',lnAccPos,Iif(lcRpSelcBy = 'S',lnStyPos,;
	IIF(lcRpSelcBy = 'F',lnFabPos,Iif(lcRpSelcBy = 'W',lnLocPos,;
	IIF(lcRpSelcBy = 'R',lnRepPos,0)))))

*MAB HERE [Begin]
*lnUsedItem = CEILING(ASCAN(laFxFltCur,lnUsedItem)/3)
*-- Know which type of select we use and its position [end]
*lcSlctFile = IIF(lnUsedItem = 0,'',laFxFltCur[lnUsedItem,2])  &&Name of selected cursor (inlist func.).
*-- if you find selected cursor.
*IF !EMPTY(lcSlctFile) AND USED(lcSlctFile)
*  SELECT (lcSlctFile)
*  COUNT TO lnHaveRecs FOR !DELETED()
*  llLineDeal = (lnHaveRecs > 0)
*ENDIF
If lnUsedItem > 0
	lcSlctFile = laOGFxFlt[lnUsedItem,6]
	llLineDeal = !Empty(lcSlctFile) And Used(lcSlctFile) And Reccount(lcSlctFile) > 0
Endif
*MAB HERE [End  ]

llRpStyLoc = (lcRpSelcBy = 'W') And llLineDeal

*-- If User select data by any select case, beside selecting orders.
*-- IMPORT must be good described before add any line in it.

If Reccount(lcWorkFile) > 0 And llLineDeal

	*B603104,1 Avoid duplicate records [Begin]
	*lcSlctKey = IIF(lcRpSelcBy = 'A',"ACCOUNT"   ,;
	IIF(lcRpSelcBy = 'S',"ALLTRIM(CSTYMAJOR)"     ,;
	IIF(lcRpSelcBy = 'W',"CWARECODE" ,;
	IIF(lcRpSelcBy = 'F',"FABRIC"    ,;
	"REPCODE"))))  && Field which we seek for in workfile.
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
	*!*	  lcSlctKey = IIF(lcRpSelcBy = 'A',"ACCOUNT"   ,;
	*!*	              IIF(lcRpSelcBy = 'S',"PADR(CSTYMAJOR,lnMajorLen)"     ,;
	*!*	              IIF(lcRpSelcBy = 'W',"CWARECODE" ,;
	*!*	              IIF(lcRpSelcBy = 'F',"FABRIC"    ,;
	*!*	              "REPCODE"))))  && Field which we seek for in workfile.
	lcSlctKey = Iif(lcRpSelcBy = 'A',"ACCOUNT"   ,;
		IIF(lcRpSelcBy = 'S',"PADR(CSTYMAJOR,lnMajorLen)"     ,;
		IIF(lcRpSelcBy = 'W',"CWARECODE" ,;
		IIF(lcRpSelcBy = 'F',"CSTYMAJOR"    ,;
		"REPCODE"))))  && Field which we seek for in workfile.
	*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
	*B603104,1 Avoid duplicate records [End  ]

	Private lcScaned
	Select (lcSlctFile)
	*-- Scan selected cursor
	Scan

		lcScaned = Evaluate(lcSlctKey)

		*-- if you find seeking critria in work file.
		If Seek(&lcSlctKey,lcWorkFile)
			Select (lcWorkFile)
			*-- scan work file for the rest data have the same seek critria.
			*SCAN REST WHILE &lcWorkTag = &lcSlctFile..&lcSlctKey
			Scan Rest While &lcWorkTag = lcScaned
				*-- if Summarize multi store orders.
				If llRpSummMt
					lcSeekExp = Style + Dtos(Complete) + cOrdType + Order
					*-- if you do not find this (style + order) in line file, add record for it.

					*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [Start]
					*IF !SEEK(lcSeekExp,lcTempLine)
					If !Seek(lcSeekExp,lcTempLine,lcTempLineTag)
						*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [End]

						Scatter Memvar Memo
						m.CCURRCODE = ORDHDR.CCURRCODE

						= lfSumStyle(lcWorkFile,lcSeekExp)  && sum for this style.
						Insert Into (lcTempLine) From Memvar
					Endif

				Else  && normal case, add line to temp. line file.
					Scatter Memvar Memo
					m.CCURRCODE = ORDHDR.CCURRCODE

					Insert Into (lcTempLine) From Memvar
				Endif
			Endscan  && end scan work file for the rest data have the same seek critria.
		Endif      && end if you find seeking critria in work file.
	Endscan      && end Scan selected cursor.
	lcMastFile = lcTempLine

Else  && User either Does not select orders or does not use any select type.

	*-- if User does not select orders but use select type.
	If llLineDeal
		*-- set files order [begin]
		lcOrdVar = Iif(Inlist(lcRpSelcBy,'S','F') , 'ORDLINES' , 'ORDLINE')
		Set Order To &lcOrdVar In ORDLINE

		*-- if select by account.
		If lcRpSelcBy = 'A'
			Set Order To ORDACCT In ORDHDR
		Else
			Set Order To ORDHDR In ORDHDR
		Endif
		*-- set files order [end]

		*-- Different select by cases.
		Do Case
		Case lcRpSelcBy = 'A'   && Account case

			Select ORDLINE
			Set Relation Off Into ORDHDR  && break relation.

			Select (lcSlctFile)
			*-- scan selected cursor.
			Scan
				*-- if you find this account in ordhdr file.
				If Seek(ACCOUNT,'ORDHDR')
					*E500271,4 add the lcRpEdiFlt to the expression [End.]

					Select ORDHDR
					*-- scan ordhdr file rest for this account.
					*E500342,1 Adding the 1st Option to the If condition [Begin]
					*E500342,1 Print Partially Shipped Orders (Y/N)
					*SCAN REST WHILE account+cordtype+order = &lcSlctFile..ACCOUNT
					Scan Rest While ACCOUNT+cOrdType+Order = &lcSlctFile..ACCOUNT For ;
							IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.)
						*E500342,1 Adding the 1st Option to the If condition [End]
						*E500342,1 Print Partially Shipped Orders (Y/N)
						*-- if order type is 'O' , <ordhdr filter>, and seek for this
						*-- order in order line file.
						*E500271,4 add the lcRpEdiFlt to the expression [Begin.]
						*C102262,1 (Begin) Get proper orders not only 'O' type.
						*IF CORDTYPE = 'O' AND ;
						EVALUATE(lcStatusEx) AND ;
						(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						SEEK('O'+ORDER,'ORDLINE')

						*E301621,1 Include Web Orders. [Begin]
						*IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
						*   EVALUATE(lcStatusEx) AND ;
						*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						*   SEEK('O'+ORDER,'ORDLINE')

						*B604687,1 Seek either by "O" or "T" in OrdLine file. [Begin]
						*IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
						*   EVALUATE(lcStatusEx) AND ;
						*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						*   SEEK('O'+ORDER,'ORDLINE') AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))
						If Iif(Empty(lcRpOrdTyp),cOrdType = 'O' Or cOrdType = 'T',cOrdType = lcRpOrdTyp) And ;
								EVALUATE(lcStatusEx) And ;
								(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
								IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
								IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,PRIORITY = laOGFxFlt[lnPriPos,6]) And ;
								IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
								IIF(Empty(lcRpOrdTyp),Seek('O'+Order,'ORDLINE') Or Seek('T'+Order,'ORDLINE'),Seek(lcRpOrdTyp+Order,'ORDLINE')) And ;
								IIF(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))
							*B604687,1 Seek either by "O" or "T" in OrdLine file. [End]

							*E301621,1 Include Web Orders. [End]

							*C102262,1 (End)
							*E500271,4 add the lcRpEdiFlt to the expression [End.]

							Select ORDLINE
							*-- scan ordline for rest order lines.
							Scan Rest While cOrdType+Order+Str(Lineno,6) = ORDHDR.cOrdType+ORDHDR.Order
								*-- if <ordline filter> and <style group filter> and <Color Filter>
								*E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
								*E301421,1 ABD- And in case of Complete status we will print Booked Qty
								*E301421,1 ABD- Because the TotQty is equal to Zero and change the IF
								*E301421,1 ABD- Condition to accept totQty = Zero. [Begin]
								*IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
								(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
								(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
								IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
								IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
								IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
								IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))

								*E500342,1 Adding the 2nd Option to the If condition [Begin]
								*E500342,1 Include Allocated Order Lines (Y/N)
								*IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
								*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
								*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
								*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
								*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
								*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
								*   IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp))
								*B803674,1 Apply Partially Shipped on Order lines also [Begin]
								*IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
								*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
								*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
								*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
								*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
								*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))   AND ;
								*   IIF(EMPTY(lcCrFrExp) ,.T.,EVALUATE(lcCrFrExp)) AND ;
								*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

								*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
								*! B607998,1 SSH 07/03/2007 not include parially allocated orders
								If Iif(llRpCorrGp ,!Empty(Group),.T.) And ;
										(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
										(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
										IIF(Empty(lcSeaExp),.T., Evaluate(lcSeaExp))  And ;
										IIF(Empty(lcDivExp),.T., Evaluate(lcDivExp))  And ;
										IIF(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp))   And ;
										IIF(Empty(lcCrFrExp) ,.T.,Evaluate(lcCrFrExp)) And ;
										IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
										IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik)))
									*! B607998,1 SSH [ENd]

									*B803674,1 Apply Partially Shipped on Order lines also [End]
									*E500342,1 Adding the 2 Options to the If condition [End]
									*E500342,1 Include Allocated Order Lines (Y/N)

									*E500444,1 HBG 03/19/2002 In case of print the report by Account , allow
									*E500444,1                the user to filter on particular styles[Begin]
									lcTmpSty = laOGFxFlt[lnStyPos,6]
									If Used(lcTmpSty) And Reccount(lcTmpSty) > 0 And;
											!Seek(Substr(ORDLINE.Style,1,lnMajorLen),lcTmpSty)
										Loop
									Endif
									*E500444,1 [End]

									*E301421,1 ABD- [End]
									*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
									If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
										Loop
									Endif
									*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]

									Scatter Memvar Memo

									*E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
									*E301421,1 ABD- And in case of Complete status we will print Booked Qty
									*E301421,1 ABD- Because the TotQty is equal to Zero. [Begin]
									*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
									*IF ORDHDR.STATUS = "C"
									If lcRpBkOp='B'
										*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
										*-- Get qty from book qty becauce qty =0
										For I = 1 To 8
											lcCount = Alltrim(Str(I))
											m.QTY&lcCount = BOOK&lcCount
										Endfor
										m.TOTQTY = TOTBOOK
									Endif
									*E301421,1 ABD- [End]
									*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
									If lcRpBkOp = 'A'
										For I = 1 To 8
											lcCount = Alltrim(Str(I))
											m.QTY&lcCount = QTY&lcCount - PIK&lcCount
										Endfor
										m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
									Endif
									*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

									*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
									*!*	                    m.cTempKey = PADR(STYLE.FABRIC,7) + ;
									*!*	                                 PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
									m.cTempKey = Padr(Style.cprifabric,19) + ;
										PADR(Style.CSTYGROUP,6) + Padr(ORDHDR.REP1,3)
									*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
									m.CCURRCODE = ORDHDR.CCURRCODE
									*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
									m.Status = ORDHDR.Status
									m.PRIORITY= ORDHDR.PRIORITY
									=Seek('S' + Scale,'SCALE')
									=Seek(cWareCode,'WAREHOUS')
									m.cdesc = WAREHOUS.cdesc
									=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
									m.btname = CUSTOMER.btname
									m.SZ1  = Scale.SZ1
									m.SZ2  = Scale.SZ2
									m.SZ3  = Scale.SZ3
									m.SZ4  = Scale.SZ4
									m.SZ5  = Scale.SZ5
									m.SZ6  = Scale.SZ6
									m.SZ7  = Scale.SZ7
									m.SZ8  = Scale.SZ8
									m.CDIVISION = ORDHDR.CDIVISION
									m.REP1  = ORDHDR.REP1
									m.REP2  = ORDHDR.REP2
									=Seek(Style,'Style','Style')
									m.DESC  = Style.Desc
									*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

									*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
									lpGetClrSgStart()
									m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
									m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
									*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
									*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
									m.approval = ORDHDR.approval
									*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[End]
									*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
									m.ccontref = ORDHDR.ccontref
									m.Note1 = ORDHDR.Note1
									m.Note2 = ORDHDR.Note2
									*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

									Insert Into (lcWorkFile) From Memvar
								Endif  && end if <ordline filter> and <style group filter>
							Endscan  && end scan ordline for rest order lines.
						Endif      && end if order type is 'O' , <ordhdr filter>.
					Endscan      && end scan ordhdr file rest for this account.
				Endif          && end if you find this account in ordhdr file.
			Endscan

			*-- Set relation again.
			Set Order To ORDHDR In ORDHDR
			Select ORDLINE
			Set Relation To cOrdType + Order Into ORDHDR

		Case lcRpSelcBy = 'S'   && Style case.

			Select (lcSlctFile)
			Scan
				*-- if you find this style in ordline file and <style group filter>

				*B603104,1 Avoid duplicate records [Begin]
				*IF SEEK(CSTYMAJOR,'ORDLINE') AND ;
				*   IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
				*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
				*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))
				*IF SEEK(PADR(CSTYMAJOR,lnMajorLen),'ORDLINE') AND ;
				*   IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
				*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
				*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))
				*B606653,1 ASH 11/13/2002 (Begin) ONLY evaluate the season filter in case of filter by style.season, else we will filter in the IF condition.
				If Seek(Padr(CSTYMAJOR,lnMajorLen),'ORDLINE') And ;
						IIF(Empty(lcGrpExp),.T., Evaluate(lcGrpExp)) And ;
						IIF(Empty(lcSeaExp) Or lcRpSeaFlt ='O',.T., Evaluate(lcSeaExp)) And ;
						IIF(Empty(lcDivExp),.T., Evaluate(lcDivExp))
					*B606653,1 ASH 11/13/2002 (End)
					*B603104,1 Avoid duplicate records [End  ]

					Select ORDLINE
					*-- scan ordline for the rest of this style.

					*B603104,1 Avoid duplicate records [Begin]
					*SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = &lcSlctFile..CSTYMAJOR

					*E500342,1 Adding the 1st Option to the If condition [Begin]
					*E500342,1 Print Partially Shipped Orders (Y/N)
					*SCAN REST WHILE STYLE+DTOS(complete)+cordtype+order+store+STR(lineno,6) = PADR(&lcSlctFile..CSTYMAJOR,lnMajorLen)
					Scan Rest While Style+Dtos(Complete)+cOrdType+Order+Store+Str(Lineno,6) = Padr(&lcSlctFile..CSTYMAJOR,lnMajorLen) ;
							FOR Iif(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.)
						*E500342,1 Adding the 1st Option to the If condition [End]
						*E500342,1 Print Partially Shipped Orders (Y/N)
						*B603104,1 Avoid duplicate records [End  ]

						*-- if <ordhdr filter> and <ordline filter> and <Color Filter>
						*E500271,4 add the lcRpEdiFlt to the expression [Begin.]
						*E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
						*E301421,1 ABD- And in case of Complete status we will print Booked Qty
						*E301421,1 ABD- Because the TotQty is equal to Zero and change the IF
						*E301421,1 ABD- Condition to accept totQty = Zero. [Begin]
						*IF CORDTYPE = 'O' AND TotQty > 0 AND ;
						EVALUATE(lcStatusEx) AND ;
						IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))

						*E500342,1 Adding the 2 Options to the If condition [Begin]
						*E500342,1 Include Allocated Order Lines (Y/N)
						*IF CORDTYPE = 'O' AND ;
						*   EVALUATE(lcStatusEx) AND ;
						*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
						*B803674,1 Apply Partially Shipped on Order lines also [Begin]
						*IF CORDTYPE = 'O' AND ;
						*   EVALUATE(lcStatusEx) AND ;
						*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
						*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
						*C102262,1 (Begin) Get proper orders not only 'O' type.
						*IF CORDTYPE = 'O' AND ;
						EVALUATE(lcStatusEx) AND ;
						IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
						IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
						IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

						*E301621,1 Include Web Orders. [Begin]
						*IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
						*   EVALUATE(lcStatusEx) AND ;
						*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
						*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
						*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
						*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
						*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
						*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
						*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
						*B606653,1 ASH 11/13/2002 (Begin) Add the season filter in case filtering by ordhdr.season.

						*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
						*! B607998,1 SSH 07/03/2007 not include parially allocated orders
						If Iif(Empty(lcRpOrdTyp),cOrdType = 'O' Or cOrdType = 'T',cOrdType = lcRpOrdTyp) And ;
								EVALUATE(lcStatusEx) And ;
								IIF(llRpCorrGp ,!Empty(Group),.T.) And ;
								(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
								(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
								(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
								IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
								IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) And ;
								IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
								IIF(Empty(lcCrFrExp),.T.,Evaluate(lcCrFrExp)) And ;
								IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
								IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik))) ;
								AND Iif(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.)) ;
								AND Iif(Empty(lcSeaExp),.T.,Iif(lcRpSeaFlt ='O',Eval(lcSeaExp),.T.))
							*! B607998,1 SSH [END]


							*B606653,1 ASH 11/13/2002 (End)
							*E301621,1 Include Web Orders. [End]

							*C102262,1 (End)
							*B803674,1 Apply Partially Shipped on Order lines also [End]
							*E500342,1 Adding the 2 Options to the If condition [End]
							*E500342,1 Include Allocated Order Lines (Y/N)

							*E301421,1 ABD-  [End]
							*E500271,4 add the lcRpEdiFlt to the expression [End.] 		  *! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
							If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
								Loop
							Endif
							*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]


							Scatter Memvar Memo

							*E301421,1 ABD- Add Feature to allow printing the Bid and Complete status,
							*E301421,1 ABD- And in case of Complete status we will print Booked Qty
							*E301421,1 ABD- Because the TotQty is equal to Zero. [Begin]
							*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
							*IF ORDHDR.STATUS = "C"
							If lcRpBkOp='B'
								*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
								*-- Get qty from book qty becauce qty =0
								For I = 1 To 8
									lcCount = Alltrim(Str(I))
									m.QTY&lcCount = BOOK&lcCount
								Endfor
								m.TOTQTY = TOTBOOK
							Endif
							*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
							If lcRpBkOp = 'A'
								For I = 1 To 8
									lcCount = Alltrim(Str(I))
									m.QTY&lcCount = QTY&lcCount - PIK&lcCount
								Endfor
								m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
							Endif
							*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

							*E301421,1 ABD- [End]
							*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
							*m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
							m.cTempKey = Padr(Style.cprifabric,19) + Padr(Style.CSTYGROUP,6) + Padr(ORDHDR.REP1,3)
							*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
							m.CCURRCODE = ORDHDR.CCURRCODE
							*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
							m.Status = ORDHDR.Status
							m.PRIORITY= ORDHDR.PRIORITY
							=Seek('S' + Scale,'SCALE')
							=Seek(cWareCode,'WAREHOUS')
							m.cdesc = WAREHOUS.cdesc
							=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
							m.btname = CUSTOMER.btname
							m.SZ1  = Scale.SZ1
							m.SZ2  = Scale.SZ2
							m.SZ3  = Scale.SZ3
							m.SZ4  = Scale.SZ4
							m.SZ5  = Scale.SZ5
							m.SZ6  = Scale.SZ6
							m.SZ7  = Scale.SZ7
							m.SZ8  = Scale.SZ8
							m.CDIVISION = ORDHDR.CDIVISION
							m.REP1  = ORDHDR.REP1
							m.REP2  = ORDHDR.REP2
							=Seek(Style,'Style','Style')
							m.DESC  = Style.Desc
							*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

							*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
							lpGetClrSgStart()
							m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
							m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
							*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
							*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
							m.approval = ORDHDR.approval
							*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]
							*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
							m.ccontref = ORDHDR.ccontref
							m.Note1 = ORDHDR.Note1
							m.Note2 = ORDHDR.Note2
							*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

							Insert Into (lcWorkFile) From Memvar
						Endif      && end if <ordhdr filter> and <ordline filter>
					Endscan      && end scan ordline for the rest of this style.
				Endif          && end if you find this style in ordline file and <style group>
			Endscan

		Case lcRpSelcBy = 'F'   && Fabric case

			Select ORDLINE
			Set Relation Off Into Style  && break relation.

			Select (lcSlctFile)
			Scan
				*-- in this case you can not rushmore data, there is no index in master files.
				Select Style
				*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
				*SET FILTER TO FABRIC = &lcSlctFile..FABRIC
				Set Filter To cprifabric = &lcSlctFile..CSTYMAJOR
				*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[END]
				*-- scan style file for fabric filter
				Scan
					*-- if <style group filter> and <Color Filter> and find this
					*-- style in order line file.
					If Iif(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp)) And ;
							IIF(Empty(lcSeaExp),.T.,  Evaluate(lcSeaExp)) And ;
							IIF(Empty(lcDivExp),.T.,  Evaluate(lcDivExp)) And ;
							IIF(Empty(lcCrFrExp),.T.,Evaluate(lcCrFrExp)) And ;
							SEEK(Style.Style,'ORDLINE')

						Select ORDLINE
						*-- scan ordline for the rest of this style.
						*E500342,1 Adding the 1st Option to the If condition [Begin]
						*E500342,1 Print Partially Shipped Orders (Y/N)
						*SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
						*                STYLE.STYLE
						Scan Rest While Style+Dtos(Complete)+cOrdType+Order+Store+Str(Lineno,6) = ;
								STYLE.Style For Iif(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.)
							*E500342,1 Adding the 1st Option to the If condition [End]
							*E500342,1 Print Partially Shipped Orders (Y/N)
							*-- if <ordhdr filter> and <ordline filter>
							*E500271,4 add the lcRpEdiFlt to the expression [Begin.]
							*--B603870,1 RAMY consider the case of completed orders [start]
							*IF CORDTYPE = 'O' AND TotQty > 0 AND ;
							EVALUATE(lcStatusEx) AND ;
							IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
							(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
							(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
							(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
							IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
							IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
							IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
							*E500271,4 add the lcRpEdiFlt to the expression [End.]

							*E500342,1 Adding the 2 Options to the If condition [Begin]
							*E500342,1 Include Allocated Order Lines (Y/N)
							*IF CORDTYPE = 'O' AND ;
							*   EVALUATE(lcStatusEx) AND ;
							*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
							*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
							*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
							*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
							*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
							*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
							*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
							*B803674,1 Apply Partially Shipped on Order lines also [Begin]
							*IF CORDTYPE = 'O' AND ;
							*   EVALUATE(lcStatusEx) AND ;
							*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
							*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
							*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
							*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
							*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
							*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
							*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
							*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
							*C102262,1 (Begin) Get proper orders not only 'O' type.
							*IF CORDTYPE = 'O' AND ;
							EVALUATE(lcStatusEx) AND ;
							IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
							(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
							(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
							(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
							IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
							IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
							IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
							IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
							IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

							*E301621,1 Include Web Orders. [Begin]
							*IF IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND ;
							*   EVALUATE(lcStatusEx) AND ;
							*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
							*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
							*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
							*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
							*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
							*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
							*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
							*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
							*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

							*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
							*! B607998,1 SSH 07/03/2007 not include parially allocated orders
							If Iif(Empty(lcRpOrdTyp),cOrdType = 'O' Or cOrdType = 'T',cOrdType = lcRpOrdTyp) And ;
									EVALUATE(lcStatusEx) And ;
									IIF(llRpCorrGp ,!Empty(Group),.T.) And ;
									(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
									(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
									(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
									IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
									IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) And ;
									IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
									IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
									IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik))) ;
									AND Iif(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))
								*! B607998,1 SSH[END]

								*E301621,1 Include Web Orders. [End]

								*C102262,1 (End)
								*B803674,1 Apply Partially Shipped on Order lines also [End]
								*E500342,1 Adding the 2 Options to the If condition [End]
								*E500342,1 Include Allocated Order Lines (Y/N)
								*--B603870,1 RAMY [end]
								*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
								If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
									Loop
								Endif
								*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]


								Scatter Memvar Memo
								*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
								*m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
								m.cTempKey = Padr(Style.cprifabric,19) + Padr(Style.CSTYGROUP,6) + Padr(ORDHDR.REP1,3)
								*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
								m.CCURRCODE = ORDHDR.CCURRCODE

								*--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
								*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
								*IF ORDHDR.STATUS = "C"
								If lcRpBkOp='B'
									*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
									*-- Get qty from book qty becauce qty =0
									For I = 1 To 8
										lcCount = Alltrim(Str(I))
										m.QTY&lcCount = BOOK&lcCount
									Endfor
									m.TOTQTY = TOTBOOK
								Endif
								*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
								If lcRpBkOp = 'A'
									For I = 1 To 8
										lcCount = Alltrim(Str(I))
										m.QTY&lcCount = QTY&lcCount - PIK&lcCount
									Endfor
									m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
								Endif
								*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

								*--B603870,1 RAMY [end]
								*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
								m.Status = ORDHDR.Status
								m.PRIORITY= ORDHDR.PRIORITY
								=Seek('S' + Scale,'SCALE')
								=Seek(cWareCode,'WAREHOUS')
								m.cdesc = WAREHOUS.cdesc
								=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
								m.btname = CUSTOMER.btname
								m.SZ1  = Scale.SZ1
								m.SZ2  = Scale.SZ2
								m.SZ3  = Scale.SZ3
								m.SZ4  = Scale.SZ4
								m.SZ5  = Scale.SZ5
								m.SZ6  = Scale.SZ6
								m.SZ7  = Scale.SZ7
								m.SZ8  = Scale.SZ8
								m.CDIVISION = ORDHDR.CDIVISION
								m.REP1  = ORDHDR.REP1
								m.REP2  = ORDHDR.REP2
								=Seek(Style,'Style','Style')
								m.DESC  = Style.Desc
								*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

								*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
								lpGetClrSgStart()
								m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
								m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
								*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
								*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
								m.approval = ORDHDR.approval
								*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]
								*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
								m.ccontref = ORDHDR.ccontref
								m.Note1 = ORDHDR.Note1
								m.Note2 = ORDHDR.Note2
								*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

								Insert Into (lcWorkFile) From Memvar
							Endif    && end if <ordhdr filter> and <ordline filter>
						Endscan    && end scan ordline for the rest of this style.
					Endif        && end if <style group filter> and find this style in order line file.
				Endscan        && end scan style file for fabric filter
			Endscan

			*-- Refilter style file [begin]
			Select Style
			Set Filter To
			*-- Refilter style file [end]

			*-- Restore style relation.
			Select ORDLINE
			Set Relation To Style Into Style Additive         && To style file.

		Case lcRpSelcBy = 'W'   && Location case

			Select ORDLINE
			Set Relation Off Into ORDHDR  && break relation.

			Select (lcSlctFile)
			Scan
				Select ORDHDR

				*-- filter to cwarecode and <ordhdr filter>
				*E500271,4 add the lcRpEdiFlt to the expression [Begin.]

				*E500342,1 Adding the 1st Option to the If condition [Begin]
				*E500342,1 Print Partially Shipped Orders (Y/N)
				*SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
				*              CORDTYPE = 'O' AND  ;
				*              EVALUATE(lcStatusEx) AND ;
				*              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				*              IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
				*              IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				*              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
				*C102262,1 (Begin) Get proper orders not only 'O' type.
				*SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
				CORDTYPE = 'O' AND  ;
				EVALUATE(lcStatusEx) AND ;
				(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
				IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
				IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)

				*E301621,1 Include Web Orders. [Begin]
				*SET FILTER TO cWareCode = &lcSlctFile..CWARECODE AND ;
				*              IIF(EMPTY(lcRpOrdTyp),CORDTYPE = 'O' OR CORDTYPE = 'T',CORDTYPE = lcRpOrdTyp) AND  ;
				*              EVALUATE(lcStatusEx) AND ;
				*              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				*              IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
				*              IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				*              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
				*              IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)
				Set Filter To cWareCode = &lcSlctFile..cWareCode And ;
					IIF(Empty(lcRpOrdTyp),cOrdType = 'O' Or cOrdType = 'T',cOrdType = lcRpOrdTyp) And  ;
					EVALUATE(lcStatusEx) And ;
					(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
					IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
					IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) And ;
					IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
					IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.) ;
					AND Iif(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))
				*E301621,1 Include Web Orders. [End]

				*C102262,1 (End)
				*E500342,1 Adding the 1st Option to the If condition [End]
				*E500342,1 Print Partially Shipped Orders (Y/N)
				*E500271,4 add the lcRpEdiFlt to the expression [End.]

				*-- scan ordhdr for this Location.
				Scan
					*C102262,1 (Begin) Get proper orders not only 'O' type.
					*= SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
					= Seek(Iif(Empty(lcRpOrdTyp),'O',lcRpOrdTyp)+Order,'ORDLINE')  && seek ordline for ordhdr order.
					If Eof('ORDLINE') And Empty(lcRpOrdTyp)
						= Seek('T'+Order,'ORDLINE')  && seek ordline for ordhdr order.
					Endif
					*C102262,1 (End)
					Select ORDLINE
					*-- Scan ordline file to get lines of this order
					*-- that evaluate critria.
					*C102262,1 (Begin) Get proper orders not only 'O' type.
					*SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER
					Scan Rest While cOrdType+Order+Str(Lineno,6) = cOrdType + ORDHDR.Order
						*C102262,1 (End)
						*-- if <ordline filter> and <style group filter> and <Color Filter>
						*--B603870,1 RAMY consider the case of completed orders [start]
						*IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
						IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
						IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
						IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))

						*E500342,1 Adding the 2 Options to the If condition [Begin]
						*E500342,1 Include Allocated Order Lines (Y/N)
						*IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
						*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
						*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
						*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
						*B803674,1 Apply Partially Shipped on Order lines also [Begin]
						*IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp))  AND ;
						*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
						*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
						*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
						*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

						*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
						*! B607998,1 SSH 07/03/2007 not include parially allocated orders
						If Iif(llRpCorrGp ,!Empty(Group),.T.) And ;
								(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
								(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
								IIF(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp))  And ;
								IIF(Empty(lcSeaExp),.T., Evaluate(lcSeaExp)) And ;
								IIF(Empty(lcDivExp),.T., Evaluate(lcDivExp)) And ;
								IIF(Empty(lcCrFrExp),.T.,Evaluate(lcCrFrExp)) And ;
								IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
								IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik)))
							*! B607998,1 SSH [END]

							*B803674,1 Apply Partially Shipped on Order lines also [End]
							*E500342,1 Adding the 2 Options to the If condition [End]
							*E500342,1 Include Allocated Order Lines (Y/N)
							*--B603870,1 RAMY [end]
							*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
							If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
								Loop
							Endif
							*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]

							Scatter Memvar Memo
							*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
							*m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
							m.cTempKey = Padr(Style.cprifabric,19) + Padr(Style.CSTYGROUP,6) + Padr(ORDHDR.REP1,3)
							*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
							m.CCURRCODE = ORDHDR.CCURRCODE

							*--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
							*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
							*IF ORDHDR.STATUS = "C"
							If lcRpBkOp='B'
								*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
								*-- Get qty from book qty becauce qty =0
								For I = 1 To 8
									lcCount = Alltrim(Str(I))
									m.QTY&lcCount = BOOK&lcCount
								Endfor
								m.TOTQTY = TOTBOOK
							Endif
							*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
							If lcRpBkOp = 'A'
								For I = 1 To 8
									lcCount = Alltrim(Str(I))
									m.QTY&lcCount = QTY&lcCount - PIK&lcCount
								Endfor
								m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
							Endif
							*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

							*--B603870,1 RAMY [end]
							*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
							m.Status = ORDHDR.Status
							m.PRIORITY= ORDHDR.PRIORITY
							=Seek('S' + Scale,'SCALE')
							=Seek(cWareCode,'WAREHOUS')
							m.cdesc = WAREHOUS.cdesc
							=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
							m.btname = CUSTOMER.btname
							m.SZ1  = Scale.SZ1
							m.SZ2  = Scale.SZ2
							m.SZ3  = Scale.SZ3
							m.SZ4  = Scale.SZ4
							m.SZ5  = Scale.SZ5
							m.SZ6  = Scale.SZ6
							m.SZ7  = Scale.SZ7
							m.SZ8  = Scale.SZ8
							m.CDIVISION = ORDHDR.CDIVISION
							m.REP1  = ORDHDR.REP1
							m.REP2  = ORDHDR.REP2
							=Seek(Style,'Style','Style')
							m.DESC  = Style.Desc
							*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

							*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
							lpGetClrSgStart()
							m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
							m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
							*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
							*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
							m.approval = ORDHDR.approval
							*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]
							*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
							m.ccontref = ORDHDR.ccontref
							m.Note1 = ORDHDR.Note1
							m.Note2 = ORDHDR.Note2
							*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

							Insert Into (lcWorkFile) From Memvar
						Endif    && end if <ordline filter>
					Endscan
				Endscan      && end scan ordhdr for this sales rep.
			Endscan

			*-- Refilter ordhdr file [begin]
			Select ORDHDR
			Set Filter To
			*-- Refilter ordhdr file [end]

			*-- Set relation again.
			Select ORDLINE
			Set Relation To cOrdType + Order Into ORDHDR

		Case lcRpSelcBy = 'R'   && Sales rep. case

			Select ORDLINE
			Set Relation Off Into ORDHDR  && break relation.

			Select (lcSlctFile)
			Scan
				Select ORDHDR
				*B802429,1 Fix Filtering Bug [Begin]
				*          Season and division are belong to OrdLine filter.
				*-- filter to repcode and <ordhdr filter>
				*SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
				CORDTYPE = 'O' AND ;
				EVALUATE(lcStatusEx) AND ;
				IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))  AND ;
				IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
				(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
				IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)

				*E500271,4 add the lcRpEdiFlt to the expression [Begin.]

				*E500342,1 Adding the 1st Option to the If condition [Begin]
				*E500342,1 Print Partially Shipped Orders (Y/N)
				*SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
				*              CORDTYPE = 'O' AND ;
				*              EVALUATE(lcStatusEx) AND ;
				*              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				*              (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
				*              (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				*              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.)
				*C102262,1 (Begin) Get proper orders not only 'O' type.
				*SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
				CORDTYPE = 'O' AND ;
				EVALUATE(lcStatusEx) AND ;
				(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				(EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
				(EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
				IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)

				*E301621,1 Include Web Orders. [Begin]
				*SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
				*              IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE = lcRpOrdTyp);
				*              EVALUATE(lcStatusEx) AND ;
				*              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				*              (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
				*              (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				*              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
				*              IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.)

				*B604748,1 Add an 'And' operator. [Begin]
				*SET FILTER TO REP1 = &lcSlctFile..REPCODE AND ;
				*              IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE = lcRpOrdTyp);
				*              EVALUATE(lcStatusEx) AND ;
				*              (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
				*              (EMPTY(lcCatExp) OR EVALUATE(lcCatExp)) AND ;
				*              (EMPTY(laOGFxFlt[lnPriPos,6]) OR ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) AND ;
				*              IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
				*              IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) ;
				*              AND IIF(llRpWebOrd,.T.,IIF(OrdHdr.lFromWeb,.F.,.T.))
				Set Filter To REP1 = &lcSlctFile..REPCODE And ;
					IIF(Empty(lcRpOrdTyp),cOrdType <> 'C',cOrdType = lcRpOrdTyp) And ;
					EVALUATE(lcStatusEx) And ;
					(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
					(Empty(lcCatExp) Or Evaluate(lcCatExp)) And ;
					(Empty(laOGFxFlt[lnPriPos,6]) Or ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6]) And ;
					IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
					IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.) ;
					AND Iif(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))
				*B604748,1 Add an 'And' operator. [End]

				*E301621,1 Include Web Orders. [End]

				*C102262,1 (End)
				*E500342,1 Adding the 1st Option to the If condition [End]
				*E500342,1 Print Partially Shipped Orders (Y/N)

				*E500271,4 add the lcRpEdiFlt to the expression [End.]

				*B802429,1 Fix Filtering Bug [End  ]

				*-- scan ordhdr for this sales rep.
				Scan
					*C102262,1 (Begin) Get proper orders not only 'O' type.
					*= SEEK('O'+ORDER,'ORDLINE')  && seek ordline for ordhdr order.
					= Seek(Iif(Empty(lcRpOrdTyp),'O',lcRpOrdTyp)+Order,'ORDLINE')  && seek ordline for ordhdr order.
					If Eof('ORDLINE') And Empty(lcRpOrdTyp)
						= Seek('T'+Order,'ORDLINE')  && seek ordline for ordhdr order.
					Endif
					*C102262,1 (End)
					Select ORDLINE
					*-- Scan ordline file to get lines of this order
					*-- that evaluate critria.
					*C102262,1 (Begin) Get proper orders not only 'O' type.
					*SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O' + ORDHDR.ORDER
					Scan Rest While cOrdType+Order+Str(Lineno,6) = cOrdType + ORDHDR.Order
						*C102262,1 (End)
						*-- if <ordline filter> and <style group filter> and <Color Filter>
						*--B603870,1 RAMY consider the case of completed orders [start]
						*IF TotQty > 0 AND IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						(EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
						(EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
						IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
						IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))

						*E500342,1 Adding the 2 Options to the If condition [Begin]
						*E500342,1 Include Allocated Order Lines (Y/N)
						*IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						*   (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
						*   (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
						*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
						*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
						*B803674,1 Apply Partially Shipped on Order lines also [Begin]
						*IF IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
						*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
						*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
						*   (EMPTY(lcSeaExp) OR EVALUATE(lcSeaExp)) AND ;
						*   (EMPTY(lcDivExp) OR EVALUATE(lcDivExp)) AND ;
						*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
						*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
						*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

						*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
						*! B607998,1 SSH 07/03/2007 not include parially allocated orders
						If Iif(llRpCorrGp ,!Empty(Group),.T.) And ;
								(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
								(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
								(Empty(lcSeaExp) Or Evaluate(lcSeaExp)) And ;
								(Empty(lcDivExp) Or Evaluate(lcDivExp)) And ;
								IIF(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp)) And ;
								IIF(Empty(lcCrFrExp),.T.,Evaluate(lcCrFrExp)) And ;
								IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
								IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik)))
							*! B607998,1 SSH [END]


							*B803674,1 Apply Partially Shipped on Order lines also [End]
							*E500342,1 Adding the 2 Options to the If condition [End]
							*E500342,1 Include Allocated Order Lines (Y/N)
							*--B603870,1 RAMY [end]
							*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
							If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
								Loop
							Endif
							*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]
							Scatter Memvar Memo
							*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
							*m.cTempKey = PADR(STYLE.FABRIC,7) + PADR(STYLE.CSTYGROUP,6) + PADR(ORDHDR.REP1,3)
							m.cTempKey = Padr(Style.cprifabric,19) + Padr(Style.CSTYGROUP,6) + Padr(ORDHDR.REP1,3)
							*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
							m.CCURRCODE = ORDHDR.CCURRCODE

							*--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
							*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
							*IF ORDHDR.STATUS = "C"
							If lcRpBkOp='B'
								*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
								*-- Get qty from book qty becauce qty =0
								For I = 1 To 8
									lcCount = Alltrim(Str(I))
									m.QTY&lcCount = BOOK&lcCount
								Endfor
								m.TOTQTY = TOTBOOK
							Endif
							*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
							If lcRpBkOp = 'A'
								For I = 1 To 8
									lcCount = Alltrim(Str(I))
									m.QTY&lcCount = QTY&lcCount - PIK&lcCount
								Endfor
								m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
							Endif
							*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

							*--B603870,1 RAMY [end]
							*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
							m.Status = ORDHDR.Status
							m.PRIORITY= ORDHDR.PRIORITY
							=Seek('S' + Scale,'SCALE')
							=Seek(cWareCode,'WAREHOUS')
							m.cdesc = WAREHOUS.cdesc
							=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
							m.btname = CUSTOMER.btname
							m.SZ1  = Scale.SZ1
							m.SZ2  = Scale.SZ2
							m.SZ3  = Scale.SZ3
							m.SZ4  = Scale.SZ4
							m.SZ5  = Scale.SZ5
							m.SZ6  = Scale.SZ6
							m.SZ7  = Scale.SZ7
							m.SZ8  = Scale.SZ8
							m.CDIVISION = ORDHDR.CDIVISION
							m.REP1  = ORDHDR.REP1
							m.REP2  = ORDHDR.REP2
							=Seek(Style,'Style','Style')
							m.DESC  = Style.Desc
							*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

							*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
							lpGetClrSgStart()
							m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
							m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
							*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
							*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
							m.approval = ORDHDR.approval
							*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[END]
							*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
							m.ccontref = ORDHDR.ccontref
							m.Note1 = ORDHDR.Note1
							m.Note2 = ORDHDR.Note2
							*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

							Insert Into (lcWorkFile) From Memvar
						Endif    && end if <ordline filter>
					Endscan
				Endscan      && end scan ordhdr for this sales rep.
			Endscan

			*-- Refilter ordhdr file [begin]
			Select ORDHDR
			Set Filter To
			*-- Refilter ordhdr file [end]

			*-- Set relation again.
			Select ORDLINE
			Set Relation To cOrdType + Order Into ORDHDR

		Endcase

		lcMastFile = lcWorkFile

	Else  && user does not use any select type.

		*-- if user does not select any orders [no data found],
		*-- in this case we select all file.
		If (Reccount(lcWorkFile) = 0) And !llWorkDeal
			Select ORDLINE
			Set Order To    && To activate rushmore.

			* IMPORT MUST BE GOOD DESCRIPED BEFORE ADD ANY LINE OF CODE.
			*-- if summarize multi store orders.
			If llRpSummMt
				*-- we again open ordline in another alias then using it
				*-- to sum style data, to avoid changing record pointer.
				Use (gcDataDir+'ORDLINE') Again Alias SUMMULTI Order Tag ORDLINES In 0

				m.cTempKey = 0  && initially define it to have total amount.

				Select ORDLINE
				*-- scan ordline file for full index expression (rushmore)
				Scan For Style+Dtos(Complete)+cOrdType+Order+Store+Str(Lineno,6) = ''
					lcSeekExp = Style + Dtos(Complete) + cOrdType + Order
					*-- if you does not find line in temp line file,;
					*-- and order type is 'O', <ordhdr filter>, <ordline filter> ;
					*-- and <style group filter> and <Color Filter>
					*E500271,4 add the lcRpEdiFlt to the expression [Begin.]
					*IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
					TotQty > 0 AND ;
					EVALUATE(lcStatusEx) AND ;
					IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
					IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
					(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
					*E500271,4 add the lcRpEdiFlt to the expression [End.]

					*E500342,1 Adding the 1st and 2nd Option to the If condition [Begin]
					*E500342,1 Print Partially Shipped Orders (Y/N)
					*E500342,1 Include Allocated Order lines (Y/N)
					*IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
					*   EVALUATE(lcStatusEx) AND ;
					*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
					*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
					*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
					*B803674,1 Apply Partially Shipped on Order lines also [Begin]
					*IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
					*   EVALUATE(lcStatusEx) AND ;
					*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
					*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
					*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
					*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
					*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))
					*C102262,1 (Begin) Get proper orders not only 'O' type.
					*IF !SEEK(lcSeekExp,lcTempLine) AND CORDTYPE = 'O' AND ;
					EVALUATE(lcStatusEx) AND ;
					IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
					IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
					(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
					IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
					IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
					IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

					*E301621,1 Include Web Orders. [Begin]
					*IF !SEEK(lcSeekExp,lcTempLine) AND IIF(EMPTY(lcRpOrdTyp),CORDTYPE <> 'C',CORDTYPE= lcRpOrdTyp) AND ;
					*   EVALUATE(lcStatusEx) AND ;
					*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp)) AND ;
					*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) AND ;
					*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
					*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
					*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
					*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

					*! E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [Start]
					*IF !SEEK(lcSeekExp,lcTempLine)

					*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
					*! B607998,1 SSH 07/03/2007 not include parially allocated orders
					If !Seek(lcSeekExp,lcTempLine,lcTempLineTag) And Iif(Empty(lcRpOrdTyp),cOrdType <> 'C',cOrdType= lcRpOrdTyp) And ;
							EVALUATE(lcStatusEx) And ;
							IIF(llRpCorrGp ,!Empty(Group),.T.) And ;
							(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
							(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
							IIF(Empty(lcSeaExp),.T., Evaluate(lcSeaExp)) And ;
							IIF(Empty(lcDivExp),.T., Evaluate(lcDivExp)) And ;
							(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
							IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
							IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  And ;
							IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
							IIF(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp)) And ;
							IIF(Empty(lcCrFrExp),.T.,Evaluate(lcCrFrExp)) And ;
							IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.) And ;
							IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
							IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik))) And ;
							IIF(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))
						*! B607998,1 SSH [END]


						*! E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [End]
						*E301621,1 Include Web Orders. [End]

						*C102262,1 (End)
						*B803674,1 Apply Partially Shipped on Order lines also [End]
						*E500342,1 Adding the 1st and 2nd Option to the If condition [End]
						*E500342,1 Print Partially Shipped Orders (Y/N)
						*E500342,1 Include Allocated Order lines (Y/N)
						*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
						If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
							Loop
						Endif
						*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]
						Scatter Memvar Memo
						m.CCURRCODE = ORDHDR.CCURRCODE

						*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
						*IF ORDHDR.STATUS = "C"
						If lcRpBkOp='B'
							*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
							*-- Get qty from book qty becauce qty =0
							For I = 1 To 8
								lcCount = Alltrim(Str(I))
								m.QTY&lcCount = BOOK&lcCount
							Endfor
							m.TOTQTY = TOTBOOK
						Endif
						*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
						If lcRpBkOp = 'A'
							For I = 1 To 8
								lcCount = Alltrim(Str(I))
								m.QTY&lcCount = QTY&lcCount - PIK&lcCount
							Endfor
							m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
						Endif
						*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
						m.Status = ORDHDR.Status
						m.PRIORITY= ORDHDR.PRIORITY
						=Seek('S' + Scale,'SCALE')
						=Seek(cWareCode,'WAREHOUS')
						m.cdesc = WAREHOUS.cdesc
						=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
						m.btname = CUSTOMER.btname
						m.SZ1  = Scale.SZ1
						m.SZ2  = Scale.SZ2
						m.SZ3  = Scale.SZ3
						m.SZ4  = Scale.SZ4
						m.SZ5  = Scale.SZ5
						m.SZ6  = Scale.SZ6
						m.SZ7  = Scale.SZ7
						m.SZ8  = Scale.SZ8
						m.CDIVISION = ORDHDR.CDIVISION
						m.REP1  = ORDHDR.REP1
						m.REP2  = ORDHDR.REP2
						=Seek(Style,'Style','Style')
						m.DESC  = Style.Desc
						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

						*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
						lpGetClrSgStart()
						m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
						m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
						*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
						*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
						m.approval = ORDHDR.approval
						*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[End]
						*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
						m.ccontref = ORDHDR.ccontref
						m.Note1 = ORDHDR.Note1
						m.Note2 = ORDHDR.Note2
						*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

						= lfSumMulti(lcSeekExp)  && summarize data.
						Insert Into (lcTempLine) From Memvar
					Endif    && end if you does not find line in temp line file,
				Endscan    && end scan file for full index expression (rushmore).
				Use In SUMMULTI

			Else  && Normal collection case for all data in ordline file.
				*-- scan ordline file for full index expression (rushmore)
				*C102262,1 (Begin) Get proper orders not only 'O' type.
				*SCAN FOR CORDTYPE + ORDER + STR(LINENO,6) = 'O'
				Scan For cOrdType + Order + Str(Lineno,6) = lcRpOrdTyp
					*C102262,1 (End)
					*-- if <ordhdr filter>, <ordline filter> and <style group filter>
					*-- and <Color Filter>.
					*E500271,4 add the lcRpEdiFlt to the expression [Begin.]

					*--B603870,1 RAMY consider the case of completed orders [start]
					*IF TotQty > 0 AND ;
					EVALUATE(lcStatusEx) AND ;
					IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					(llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					(llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
					IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
					(EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
					*E500271,4 add the lcRpEdiFlt to the expression [End.]

					*E500342,1 Adding the 1st and 2nd Option to the If condition [Begin]
					*E500342,1 Print Partially Shipped Orders (Y/N)
					*E500342,1 Include Allocated Order lines (Y/N)
					*IF EVALUATE(lcStatusEx) AND ;
					*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
					*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
					*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp))
					*B803674,1 Apply Partially Shipped on Order lines also [Begin]
					*IF EVALUATE(lcStatusEx) AND ;
					*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
					*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
					*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
					*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
					*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

					*E301621,1 Include Web Orders. [Begin]
					*IF EVALUATE(lcStatusEx) AND ;
					*   IIF(llRpCorrGp ,!EMPTY(Group),.T.) AND ;
					*   (llSrtSides OR BETWEEN(DTOS(START),lcStartSt,lcStartEd))  AND ;
					*   (llCmpSides OR BETWEEN(DTOS(COMPLETE),lcCompSt,lcCompEd)) AND ;
					*   IIF(EMPTY(lcSeaExp),.T., EVALUATE(lcSeaExp))   AND ;
					*   IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))  AND ;
					*   (EMPTY(lcRpEdiFlt) OR EVALUATE(lcRpEdiFlt)) AND  ;
					*   IIF(EMPTY(lcCatExp),.T., EVALUATE(lcCatExp)) AND ;
					*   IIF(EMPTY(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  AND ;
					*   IIF(llMultCurr AND !EMPTY(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) AND ;
					*   IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) AND ;
					*   IIF(EMPTY(lcCrFrExp),.T.,EVALUATE(lcCrFrExp)) AND ;
					*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,OrdHdr.Open > 0 AND OrdHdr.Ship > 0),.T.) AND ;
					*   IIF(llRpPrtShp,IIF(OrdHdr.Status $ 'BCX',.F.,TotQty > 0),.T.) AND ;
					*   IIF(llRpAloOrd,.T.,IIF(OrdHdr.Status $ 'BCX',.F.,EMPTY(PIKTKT)))

					*! B607998,1 SSH 07/03/2007 when option 'Include Allocated Orders' set to yes, report does
					*! B607998,1 SSH 07/03/2007 not include parially allocated orders
					If Evaluate(lcStatusEx) And ;
							IIF(llRpCorrGp ,!Empty(Group),.T.) And ;
							(llSrtSides Or Between(Dtos(Start),lcStartSt,lcStartEd))  And ;
							(llCmpSides Or Between(Dtos(Complete),lcCompSt,lcCompEd)) And ;
							IIF(Empty(lcSeaExp),.T., Evaluate(lcSeaExp))   And ;
							IIF(Empty(lcDivExp),.T., Evaluate(lcDivExp))  And ;
							(Empty(lcRpEdiFlt) Or Evaluate(lcRpEdiFlt)) And  ;
							IIF(Empty(lcCatExp),.T., Evaluate(lcCatExp)) And ;
							IIF(Empty(laOGFxFlt[lnPriPos,6]),.T.,ORDHDR.PRIORITY = laOGFxFlt[lnPriPos,6])  And ;
							IIF(llMultCurr And !Empty(laOGFxFlt[lnCurrPos,6]),ORDHDR.CCURRCODE$laOGFxFlt[lnCurrPos,6],.T.) And ;
							IIF(Empty(lcGrpExp),.T.,Evaluate(lcGrpExp)) And ;
							IIF(Empty(lcCrFrExp),.T.,Evaluate(lcCrFrExp)) And ;
							IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,ORDHDR.Open > 0 And ORDHDR.Ship > 0),.T.) And ;
							IIF(llRpPrtShp,Iif(ORDHDR.Status $ 'BCX',.F.,TOTQTY > 0),.T.) And ;
							IIF(llRpAloOrd,.T.,Iif(ORDHDR.Status $ 'BCX',.F.,(Empty(PIKTKT) .Or. TOTQTY<>TotPik))) And ;
							IIF(llRpWebOrd,.T.,Iif(ORDHDR.lFromWeb,.F.,.T.))
						*! B607998,1 SSH 07/03/2007 [END]


						*E301621,1 Include Web Orders. [End]

						*B803674,1 Apply Partially Shipped on Order lines also [End]
						*E500342,1 Adding the 1st and 2nd Option to the If condition [End]
						*E500342,1 Print Partially Shipped Orders (Y/N)
						*E500342,1 Include Allocated Order lines (Y/N)
						*--B603870,1 RAMY [end]
						*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][Start]
						If Iif(llPackSelected,!Seek(ORDLINE.Pack_id,lcCursorPack),.F.)
							Loop
						Endif
						*! B610768,1 MMT 07/10/2014 Order detail report does not filter by Pack ID correctly[T20131219.0020 - Issue#81][END]
						Scatter Memvar Memo
						*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
						*m.cTempKey = PADR(STYLE.FABRIC,7)+PADR(STYLE.CSTYGROUP,6)+PADR(ORDHDR.REP1,3)
						m.cTempKey = Padr(Style.cprifabric,19)+Padr(Style.CSTYGROUP,6)+Padr(ORDHDR.REP1,3)
						*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
						m.CCURRCODE = ORDHDR.CCURRCODE

						*--B603870,1 RAMY if the order is completed get the Book Qty insted of the Tot Qty [start]
						*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [STATR]
						*IF ORDHDR.STATUS = "C"
						If lcRpBkOp='B'
							*B607988,1 AYM 02/25/2007 Add option Show Booked or Open Qty to fix problem o adding Booked to Open [END]
							*-- Get qty from book qty becauce qty =0
							For I = 1 To 8
								lcCount = Alltrim(Str(I))
								m.QTY&lcCount = BOOK&lcCount
							Endfor
							m.TOTQTY = TOTBOOK
						Endif
						*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][Start]
						If lcRpBkOp = 'A'
							For I = 1 To 8
								lcCount = Alltrim(Str(I))
								m.QTY&lcCount = QTY&lcCount - PIK&lcCount
							Endfor
							m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
						Endif
						*B610969,1 MMT 03/23/2014 Change Show Qty option to include unallocated qty[T20150313.0008][End]

						*--B603870,1 RAMY [end]
						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[Start]
						m.Status = ORDHDR.Status
						m.PRIORITY= ORDHDR.PRIORITY
						=Seek('S' + Scale,'SCALE')
						=Seek(cWareCode,'WAREHOUS')
						m.cdesc = WAREHOUS.cdesc
						=Seek(Iif(Empty(m.Store) , 'M' + m.ACCOUNT,'S' + m.ACCOUNT + m.Store),'Customer')
						m.btname = CUSTOMER.btname
						m.SZ1  = Scale.SZ1
						m.SZ2  = Scale.SZ2
						m.SZ3  = Scale.SZ3
						m.SZ4  = Scale.SZ4
						m.SZ5  = Scale.SZ5
						m.SZ6  = Scale.SZ6
						m.SZ7  = Scale.SZ7
						m.SZ8  = Scale.SZ8
						m.CDIVISION = ORDHDR.CDIVISION
						m.REP1  = ORDHDR.REP1
						m.REP2  = ORDHDR.REP2
						=Seek(Style,'Style','Style')
						m.DESC  = Style.Desc
						*! B609259,2 MMT 06/02/2010 Export Division,Sales Reps., and Style desc to excel[End]

						*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][Start]
						lpGetClrSgStart()
						m.ClrCodDsc = gfCodDes(Substr(Style.Style ,lnClrStart, lnColorLen),"COLOR")
						m.StyGrpDsc = gfCodDes(Style.CSTYGROUP ,'CSTYGROUP')
						*! B609543,1 SAB 03/07/2011 Add 2 Fields to Sales order detail report [T20110211.0008][End]
						*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[Start]
						m.approval = ORDHDR.approval
						*! E303255,1 MMT 09/19/2012 Add Approval# to the exported data to excel in order detail report[End]
						*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][Start]
						m.ccontref = ORDHDR.ccontref
						m.Note1 = ORDHDR.Note1
						m.Note2 = ORDHDR.Note2
						*E303591,1 MMT 06/29/2015 Add notes and contract ref. fields to order detail report excel[T20150518.0001][End]

						Insert Into (lcTempLine) From Memvar
					Endif    && end if <ordhdr filter>, <ordline filter> and <style group filter>
				Endscan    && end scan file for full index expression (rushmore).
			Endif        && end if summarize multi store orders.
			Select ORDLINE
			Set Order To ORDLINE In ORDLINE
			lcMastFile = lcTempLine
		Endif          && end if user does not select any orders [no data found],
	Endif            && end if User does not select orders but use select type.
Endif  && end If User select data by any select case, beside selecting orders.

* IMPORT MUST BE GOOD DESCRIBED BEFORE ADD ANY LINE OF COOD
*-- if user select by orders only, and want to summarize data.
If (Reccount(lcTempLine) = 0 And Reccount(lcWorkFile) > 0) And llRpSummMt
	Select(lcWorkFile)
	*B129774,1 HMA 09/20/2005 print wrong quantities while summerize multistore orders [Begin]
	*-Change index while scan
	lcorder=Order()
	lcIdxTag=[STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)]
	Index On &lcIdxTag Tag (lcWorkFile)
	*B129774,1 HMA 09/20/2005 print wrong quantities while summerize multistore orders [End]
	Scan
		lcSeekExp = Style + Dtos(Complete) + cOrdType + Order
		*! E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [Start]
		If !Seek(lcSeekExp,lcTempLine,lcTempLineTag)
			*! E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [End]
			Scatter Memvar Memo
			m.CCURRCODE = ORDHDR.CCURRCODE

			= lfSumStyle(lcWorkFile,lcSeekExp)
			Insert Into (lcTempLine) From Memvar
		Endif
	Endscan
	lcMastFile = lcTempLine
	*B129774,1 HMA 09/20/2005 print wrong quantities while summerize multistore orders [Begin]
	*-return the old Order of work file.
	Set Order To Tag lcorder In (lcWorkFile)
	*B129774,1 HMA 09/20/2005 print wrong quantities while summerize multistore orders [End]
Endif

*-- if report master file is the work file and index
*-- does not match sort by case, reindex data.
If (lcMastFile = lcWorkFile) And (lcWorkTag != lcIndexTg)
	Select (lcWorkFile)
	Index On &lcIndexTg Tag (lcWorkFile)
Endif
*-- end of lfScanData.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
Function lfvOptMsg

Private laOptMsg
Declare laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 75                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
Function lfwOldVal

laOldVal = Evaluate(OGSYS18())
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvSelcBy
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
*! Purpose   : Validate select by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : In this function we want to know old value to disable
*!           : last object, and we transfer it to its corressponding
*!           : character because option grid returns its item number in popup.
*!*************************************************************
*! Example   : =lfvSelcBy()
*!*************************************************************
Function lfvSelcBy

lcDummy = "Y"
llChSelect = .T.
llClearAcc = (lcRpSelcBy # 'A')
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')
llClearLoc = (lcRpSelcBy # 'L')
llClearRep = (lcRpSelcBy # 'R')
ClearRead()
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfSelcObjs
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSelcObjs()
*!*************************************************************
Function lfSelcObjs
Parameters lnObjNum,lcObjState,llClearVal

If llClearVal And (lcObjState = 'D' And !Empty(laOGFxFlt[lnObjNum,6]))
	laOGFxFlt[lnObjNum,6] = ''
Endif
laOGObjCnt[lnObjNum + lnVarbEnd] = (lcObjState = 'E')
= lfOGShowGet('laOGFxFlt[' + Alltrim(Str(lnObjNum)) + ',6]')  && Enable / Disable Object .
*-- end of lfSelcObjs.

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
*! Purpose   : 1- Enable and disable some variavle objects due to sort case
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfObjState,lfPreObjs
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : 1- In this function we want to know old value to enable/disable
*!           :    objects due to some sort cases, and we transfer it to
*!           :    its corressponding character because option grid returns
*!           :    its item number in popup, the idea of enable/disable in
*!           :    this function is to full control printing and do not enable
*!           :    enabled button or disable disabled button.
*!           : 2- In some cases we rise summarization flag to Recollect data again.
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
Function lfvSortBy


If lcRpKind = 'D'
	If lcRpSortBy = "O"

		Dimension laSort2Des[4,1] , laSort2Val[4,1]
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
		*!*	    laSort2Des[3,1] = "Store/Line#"
		*!*	    laSort2Des[4,1] = "Store/" + lcStyMajor
		laSort2Des[3,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STORELINE,oAriaApplication.GetHeaderText("LANG_STORELINE",AHEADERFILE))
		laSort2Des[4,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STOREBACK,oAriaApplication.GetHeaderText("LANG_STOREBACK",AHEADERFILE))+ lcStyMajor
		*N000682,1 11/20/2012 MMT Globlization changes[End]

		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
		laSort2Val[3,1] = "T"
		laSort2Val[4,1] = "Y"

	Else  && if sort by any thing rather than order.

		*-- last sort is by order.
		If laOldVal = "O"
			Dimension laSort2Des[2,1] , laSort2Val[2,1]
			lcRpSrt2 = 'L'
		Endif

	Endif
	*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[Start]
	If lcRpSortBy = "S"
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
		*DIMENSION laSort2Des[2,1] , laSort2Val[2,1]
		Dimension laSort2Des[3,1] , laSort2Val[3,1]
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
		* N000682 ,1 Thabet Handle globalization issues [Start]
		*!*	    laSort2Des[1,1] = "Order#"
		*!*	    laSort2Des[2,1] = "Account"
		laSort2Des[1,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORDERNO,oAriaApplication.GetHeaderText("LANG_ORDERNO",AHEADERFILE))
		laSort2Des[2,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNT,oAriaApplication.GetHeaderText("LANG_ACCOUNT",AHEADERFILE))
		* N000682 ,1 Thabet Handle globalization issues [END]
		laSort2Val[1,1] = "O"
		laSort2Val[2,1] = "A"
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
		*lcRpSrt2 = 'O'
		* N000682 ,1 Thabet Handle globalization issues [Start]
		*laSort2Des[3,1] = "Complete Date"
		laSort2Des[3,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPDATE,oAriaApplication.GetHeaderText("LANG_COMPDATE",AHEADERFILE))
		* N000682 ,1 Thabet Handle globalization issues [END]
		laSort2Val[3,1] = "C"
		lcRpSrt2 = 'C'
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
	Else
		* N000682 ,1 Thabet Handle globalization issues [Start]
		*laSort2Des[1,1] = "Line#"
		laSort2Des[1,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LINENO,oAriaApplication.GetHeaderText("LANG_LINENO",AHEADERFILE))
		* N000682 ,1 Thabet Handle globalization issues [END]
		laSort2Des[2,1] = lcStyMajor
		laSort2Val[1,1] = "L"
		laSort2Val[2,1] = "S"
		lcRpSrt2 = 'L'
	Endif
	*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[End]
Endif

llRpScale  = Iif(lcRpSortBy $ 'SFG',.F.,.T.)

*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [Start]
*llRpSummMt = IIF(lcRpSortBy <> 'S' ,.F.,llRpSummMt)
llRpSummMt = .F.
*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [End]

lcRpStyPrn = Iif(lcRpSortBy <> 'S' ,'N',lcRpStyPrn)
llRpOrdNot = Iif(!(lcRpSortBy $ 'AO'),.F.,llRpOrdNot)
ClearRead()

*-- Notes [begin]
*-- if In case of sort by (STYLE,FABRIC OR STYLE GROUP) and old value is
*-- another sort type and user want to print sizes, we must disable
*-- reprint scale when diff. because it is the normal print case here,
*-- and vice versa.
*-- Notes [end]

If lcRpSortBy != laOldVal

	*-- Different sort by cases.
	Do Case
	Case lcRpSortBy = 'A'   && Sort by account

		*-- if report kind is detail
		If lcRpKind = 'D'
			If laOldVal = 'S'
				llChSumm = Iif(llRpSummMt,.T.,llChSumm)  && Rise summarize flag.
			Endif
		Endif
		*-- Enable/disable variable objects due to sort case. [end]

	Case lcRpSortBy = 'O'    && Sort by order
		If lcRpKind = 'D'
			If laOldVal = 'S'
				llChSumm = Iif(llRpSummMt,.T.,llChSumm)
			Endif
		Endif
		*-- Enable/disable variable objects due to sort case. [begin]

	Case lcRpSortBy = 'S'      && Sort by style

		If lcRpKind = 'D'

		Endif
		*-- Enable/disable variable objects due to sort case. [begin]

	Case lcRpSortBy = 'G'    && Sort by style group

		*-- Enable/disable variable objects due to sort case. [begin]
		If Inlist(laOldVal,'A','O','S')
			= lfPreObjs() && Prepair objects.
		Endif
		*-- Enable/disable variable objects due to sort case. [begin]

	Case lcRpSortBy = 'F'    && Sort by fabric

		*-- Enable/disable variable objects due to sort case. [begin]
		If Inlist(laOldVal,'A','O','S')
			= lfPreObjs()
		Endif
		*-- Enable/disable variable objects due to sort case. [begin]

	Case lcRpSortBy = 'W'    && Sort by location

		*-- Enable/disable variable objects due to sort case. [begin]
		If Inlist(laOldVal,'A','O','S')
			= lfPreObjs()
		Endif
		*-- Enable/disable variable objects due to sort case. [begin]

	Case lcRpSortBy = 'R'    && Sort by sales representative

		*-- Enable/disable variable objects due to sort case. [begin]
		If Inlist(laOldVal,'A','O','S')
			= lfPreObjs()
		Endif
		*-- Enable/disable variable objects due to sort case. [begin]

	Case lcRpSortBy = 'D'    && Sort by complete date

		*-- Enable/disable variable objects due to sort case. [begin]
		If Inlist(laOldVal,'A','O','S')
			= lfPreObjs()
		Endif
		*-- Enable/disable variable objects due to sort case. [begin]

	Endcase          && end Different sort by cases.
Endif
*-- end of lfvSortBy.


*!*************************************************************
*! Name      : lfPreObjs
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Enable/Disable controled objects in 4 sort cases
*!           : - Style group, Fabric, Sales Rep., Complete date
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPreObjs()
*!*************************************************************
Function lfPreObjs


If lcRpKind = 'D'
	If laOldVal = 'S'
		llChSumm = Iif(llRpSummMt,.T.,llChSumm)
	Endif
Endif
*-- end of lfPreObjs.

*!*************************************************************
*! Name      : lfObjState
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : This function used to calculate object number and call
*!           : global show function to enable/disable object due to passed state.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Passed Parameters  : 1- ('E' -> enable,'D' disable)
*!                    : 2- Object number
*!                    : 3- Object variable
*!                    : 2- Object value
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfObjState()
*!*************************************************************
Function lfObjState
Parameters lcObjState,lnObjNum,lcObjVar,laObjVal


If lnObjNum != 0
	*-- If you disable object you must restore its initial value.
	If lcObjState = 'D'
		&lcObjVar = laObjVal
	Endif

	laOGObjCnt[lnObjNum] = (lcObjState = 'E')    && Enable if 'E'
	laRpVarNow[lnObjNum] = laOGObjCnt[lnObjNum]  && Save Variable value.

	= lfOGShowGet(lcObjVar)  && Called to show object get.
Endif
*-- end of lfObjState.

*!*************************************************************
*! Name      : lfvSizes
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Control Form name, Enable/disable some objects.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSizes()
*!*************************************************************
Function lfvSizes


If !llRpPrTPak
	lcRpForm = "SORDDGMP"
Else
	lcRpForm = Iif(llRpSizes,'SORDDETA','SORDDETB')
Endif
= lfRepPltFr(lcRpForm)
llRpScale = llRpSizes
ClearRead()
*-- end of lfvSizes.

*!*************************************************************
*! Name      : lfvKind
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Enable/disable some objects due to report kind (Detail/Summary)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*!
Function lfvKind


If lcRpKind = 'S'
	Store .F. To llRpSummMt,llRpOrdLnt,llRpOrdNot
	lcRpSrt2 = "L"
Endif
ClearRead()
*-- end of lfvKind.

*!*************************************************************
*! Name      : lfvSumm
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Enable/disable Order line notes object
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfObjState
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSumm()
*!*************************************************************
Function lfvSumm

llRpOrdLnt = Iif(llRpSummMt ,.F.,llRpOrdLnt)
llChSumm   = .T.
ClearRead()
*-- end of lfvSumm.


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
Function lfSRVSty
Parameters lcParm

Do Case
Case lcParm = 'S'  && Set code
	*-- open this file in another alias to set order to Style Major
	*-- unique index.
	Use (gcDataDir+'Style') Again Alias STYLE_X Order Tag Style In 0
	Select Style
	Set Order To Tag Cstyle
	Set Relation To Style.Style Into STYLE_X
	Go Top In Style
	llChStyle = .T.
Case lcParm = 'R'  && Reset code
	Use In STYLE_X
	Select Style
	Set Order To Tag Style
	llClearSty = .F.
Otherwise      && Valid code
	*lcAlias = ALIAS()
	*SELECT STYLE
	*LOCATE FOR STYLE.Fabric = Fabric.Fabric
	*llHaveSty = FOUND()
	*-- If no styles found for this fabric
	*IF !llHaveSty
	*-- the following message is
	*-- No styles in fabric group XXX .
	*--           <Ok>
	*= gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
	*ENDIF
	*SELECT (lcAlias)
	*RETURN llHaveSty    && Record selected only if fabric found in style file.
Endcase
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
Function lfStySum
Parameters lcSty,lccomp,lnAddToVar

Private lnStyRec
lnTotcomp = 0

*MAB [Begin]
If Reccount('STYLE') != 0
	lnStyRec = Recno('STYLE')
	Select STYLE_X
	Set Order To Style
	If !Empty(Alltrim(lcSty)) And Seek(Alltrim(lcSty))
		*B608640,1 WAM 08/03/2008 Fix style browse
		*SUM &lcCOMP TO lnTotcomp WHILE Style = lcSty
		Sum &lccomp To lnTotcomp While Style = Alltrim(lcSty)
		*B608640,1 WAM 08/03/2008 (End)
	Endif
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
*MAB [Begin]
Return Int(lnTotcomp)
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
Function lfSRVFab
Parameters lcParm


Private lcAlias,llHaveSty
*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
*!*	DO CASE
*!*	  CASE lcParm = 'S'  && Set code
*!*	    *-- open this file in another alias to set order to primary fabric
*!*	    *-- unique index.
*!*	    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
*!*	    SELECT FABRIC
*!*	    SET ORDER TO TAG cFabric
*!*	    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
*!*	    GO TOP IN FABRIC
*!*	    llChFabric = .T.
*!*	  CASE lcParm = 'R'  && Reset code
*!*	    USE IN FABRIC_X
*!*	    SELECT FABRIC
*!*	    SET ORDER TO TAG FABRIC
*!*	    llClearFab = .F.
*!*	  OTHERWISE      && Valid code
*!*	    lcAlias = ALIAS()
*!*	    SELECT STYLE
*!*	    LOCATE FOR STYLE.Fabric = Fabric.Fabric
*!*	    llHaveSty = FOUND()
*!*	    *-- If no styles found for this fabric
*!*	    IF !llHaveSty
*!*	      *-- the following message is
*!*	      *-- No styles in fabric group XXX .
*!*	      *--           <Ok>
*!*	      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
*!*	    ENDIF
*!*	    SELECT (lcAlias)
*!*	    RETURN llHaveSty    && Record selected only if fabric found in style file.
*!*	ENDCASE
lcCurrSelFab = CSTYMAJOR
lcAlias = Alias()
Select Style
Locate For Style.cprifabric = lcCurrSelFab
llHaveSty = Found()
*-- If no styles found for this fabric
If !llHaveSty
	*-- the following message is
	*-- No styles in fabric group XXX .
	*--           <Ok>
	= gfModalGen("TRM32055B36000","Dialog",Alltrim(Item.CSTYMAJOR))
Endif
Select (lcAlias)
Return llHaveSty    && Record selected only if fabric found in style file.
*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
Function lfFabSum
Parameters lcFab,lccomp

*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
*!*	PRIVATE lnFabRec
*!*	lnTotcomp = 0
*!*	*MAB 05/11/1999 Avoiding do calculation for empty file [Begin]
*!*	*lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)
*!*	IF RECCOUNT() != 0
*!*	  lnFabRec = RECNO('FABRIC')

*!*	  SELECT Fabric_X
*!*	  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
*!*	  SELECT Fabric
*!*	  IF BETWEEN(lnFabRec,1,RECCOUNT())
*!*	    GO lnFabRec
*!*	  ENDIF
*!*	ENDIF
*!*	*MAB 05/11/1999 Avoiding do calculation for empty file [End  ]
*!*	RETURN INT(lnTotcomp)
Private lnItemRec
Local lnAlias
lnAlias = Select()
lnTotcomp = 0
Select(lcTmpFab)
If Reccount() != 0
	lnItemRec = Recno('ITEM')
	Select(lcTmpFab)
	Locate
	If Seek(Substr(lcFab,1,lnMajorLen))
		Sum &lccomp To lnTotcomp While Fabric = lcFab
	Endif
	Select Item
	If Between(lnItemRec,1,Reccount())
		Go lnItemRec
	Endif
Endif
Select(lnAlias)
Return Int(lnTotcomp)
*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]

*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change account flag, in range browse screen.
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
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
Function lfsrAcc
Parameters lcParm

Do Case
Case lcParm = 'S'
	llChAcc = .T.
	Go Top In CUSTOMER
Case lcParm = 'R'
	llClearAcc = .F.
Endcase
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfsrLoc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/01/98
*! Purpose   : Rise change Location flag, in range browse screen.
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
*! Example   : =lfsrLoc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
Function lfsrLoc
Parameters lcParm


Do Case
Case lcParm = 'S'
	llChLoc = .T.
	Go Top In WAREHOUS
Case lcParm = 'R'
	llClearLoc = .F.
Endcase
*-- end of lfsrLoc.

*!*************************************************************
*! Name      : lfsrRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change sales rep. flag, in range browse screen.
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
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
Function lfsrRep
Parameters lcParm


Do Case
Case lcParm = 'S'
	llChRep = .T.
	Go Top In SALESREP
Case lcParm = 'R'
	llClearRep = .F.
Endcase
*-- end of lfsrRep.

*!*************************************************************
*! Name      : lfSROrder
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change order flag, in range browse screen.
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
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
Function lfSROrder
Parameters lcParm


llChOrder = .T.
*B802122,1 Set Relation with customer file[Begin]
Do Case
Case lcParm = 'S'

	Select ORDHDR
	lcCustRel = Iif(llRpSummMt,['M' + Account],;
		[IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)])

	Set Order To CUSTOMER In CUSTOMER
	Set Relation To &lcCustRel Into CUSTOMER && To customer file.
	Go Top

Case lcParm = 'R'
	Select ORDHDR
	Set Relation Off Into CUSTOMER && To customer file.
	llClearOrd = .F.

Endcase
*B802122,1 Set Relation with customer file[End  ]
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
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
*! Name      : lfSumStyle
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Summarize multi store styles using one file for scan and sum.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : sum file, sum expression
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfSumStyle()
*!*************************************************************
Function lfSumStyle
Parameters lcSumFile,lcSumExpr

*-- initial value for sum variables.
Store 0 To m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,;
	m.QTY7,m.QTY8,m.TOTQTY,m.cTempKey
lnRecNum = Recno(lcSumFile)
Sum  QTY1 , QTY2 , QTY3 , QTY4 , QTY5 , QTY6 , QTY7 , QTY8 , TOTQTY , TOTQTY*Price ;
	TO  m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY,m.cTempKey   ;
	REST While Style + Dtos(Complete) + cOrdType + Order = lcSumExpr

m.cTempKey = Str(m.cTempKey,16,2)  && Total amount.
Go lnRecNum In (lcSumFile)
*-- end of lfSumStyle.

*!*************************************************************
*! Name      : lfSumMulti
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Summarize multi store styles using two aliass
*!           : from same file for scan and sum,
*!           : in this case ordline file is used with out any
*!           ; order to activiate rushmore, thus we open another
*!           ; alias for make sum in the fastest way.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : sum expression
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfSumMulti()
*!*************************************************************
Function lfSumMulti
Parameters lcSumExpr


Store 0 To m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,;
	m.QTY7,m.QTY8,m.TOTQTY,m.cTempKey

Select SUMMULTI  && Order line alias (sum for all file)
= Seek(lcSumExpr)

Sum  QTY1 , QTY2 , QTY3 , QTY4 , QTY5 , QTY6 , QTY7 , QTY8 , TOTQTY , TOTQTY*Price ;
	TO  m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY, m.cTempKey   ;
	REST While Style + Dtos(Complete) + cOrdType + Order = lcSumExpr

m.cTempKey = Str(m.cTempKey,16,2)
*-- end of lfSumMulti.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Mohamed Badran (MAB)
*! Date        : 05/27/1998
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDDETA.FRX, OR SORDDETB.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
Function lfGetNotes


lcTitle = ''     && Title of the note (Line Note OR NotePad).
lcNotes = ''     && Note Data.

*-- if you print both notes.
If llPrntBoth
	*-- Note that the following Scheme
	*-- ....... cRecord = 'N1' ............. Line Notepad.
	*-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
	Do Case
	Case &lcNoteLns..cRecord = 'N1' And !Empty(Alltrim(&lcMastFile..note_mem))
		lcTitle = 'Order : ' + Order + ' - Style : ' + Style + ' - Line #  ' + Alltrim(Str(Lineno)) + '    Notes.'
		*lcNotes  =  ALLTRIM(Note_Mem) + CHR(10) + CHR(13)
		lcNotes  =  Alltrim(note_mem) + Chr(10)
		*C102262,1 (Begin)  Print EDI Tempp orders notepad.
		*CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
	Case &lcNoteLns..cRecord = 'N2' And Seek('B' + Iif(ORDHDR.cOrdType = 'T','T','')+Order , 'NOTEPAD') And !Empty(Alltrim(NOTEPAD.mNotes))
		*C102262,1 (End)

		lcTitle = Iif(Recno(lcMastFile) = lnLastRec,;
			'Order :' + Order + ' Notepad.','')
		*lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
		IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
		lcNotes  = Iif(Recno(lcMastFile) = lnLastRec,Alltrim(NOTEPAD.mNotes),'')+;
			IIF(lcRpSortBy = 'A',Chr(10) ,'')
	Endcase
Else && Else You print either Line or Order/contract Notepad.
	*-- Note that the following Scheme
	*-- ....... llRoOrdLnt ............. Line Notepad.
	*-- ....... llRoOrdNot ............. Order or Contract Notepad.
	Do Case
	Case llRpOrdLnt And !Empty(Alltrim(&lcMastFile..note_mem))
		lcTitle = 'Order : ' + Order + ' - Style : ' + Style + ' - Line #  ' + Alltrim(Str(Lineno)) + '    Notes.'
		*lcNotes  =  ALLTRIM(Note_Mem) + CHR(10) + CHR(13)
		lcNotes  =  Alltrim(note_mem) + Chr(10)
		*C102262,1 (Begin)  Print EDI Tempp orders notepad.
		*CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
	Case llRpOrdNot And Seek('B' + Iif(ORDHDR.cOrdType = 'T','T','')+Order , 'NOTEPAD') And !Empty(Alltrim(NOTEPAD.mNotes))
		*C102262,1 (End)
		lcTitle = Iif(Recno(lcMastFile) = lnLastRec,;
			'Order :' + Order + ' Notepad.','')
		*lcNotes  = IIF(RECNO(lcMastFile) = lnLastRec,ALLTRIM(NotePad.mNotes),'')+;
		IIF(lcRpSortBy = 'A',CHR(10) + CHR(13),'')
		lcNotes  = Iif(Recno(lcMastFile) = lnLastRec,Alltrim(NOTEPAD.mNotes),'')+;
			IIF(lcRpSortBy = 'A',Chr(10),'')
	Endcase
Endif
Return ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Calculate last Record in order details. [ORDER GROUP]
*!           : we use another alias to unchange record pointer of report file.
*!*************************************************************
*! Called from : [SORDDETA.FRX OR SORDDETB.FRX, ORDER GROUP HEADER BAND]
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : NULL
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
Function lfLastRec


If llRpOrdNot
	Private lcThAlias , lcCurrOrd , lcToSeekVl
	lcThAlias = Alias()           && Save Current Alias.
	lcCurrOrd  = Order()
	lcToSeekVl = Evaluate(lcSeekVal)
	Select GETLAST
	Set Order To (lcCurrOrd) Descending
	=Seek(lcToSeekVl)
	lnLastRec = Recno('GETLAST')  && Evaluate record Number of last record in detail lines.
	Set Order To (lcCurrOrd) Ascending
	Select (lcThAlias)             && Restore Alias.
Endif
Return ''
*-- end of lfLastRec.

*!*************************************************************
*! Name      : lfvCoorGrp
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change print coordinate groups flag.
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
*! Example   : =lfvCoorGrp()
*!*************************************************************
Function lfvCoorGrp


llChCoord = .T.
*-- end of lfvCoorGrp.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
Function lfClearRep


llClearFn = .T.
*-- Close temp. opended files, if it used.

*-- Delete temporary line file.
If Used('GETLAST')
	Use In GETLAST
Endif

If Used(lcTempLine)
	Use In (lcTempLine)

	If File(gcWorkDir+lcTempLine+'.DBF')
		Erase(gcWorkDir+lcTempLine+'.DBF')
	Endif

	If File(gcWorkDir+lcTempLine+'.CDX')
		Erase(gcWorkDir+lcTempLine+'.CDX')
	Endif

	If File(gcWorkDir+lcTempLine+'.FPT')
		Erase(gcWorkDir+lcTempLine+'.FPT')
	Endif
Endif

*-- Delete temporary work file.
If Used(lcWorkFile)
	Use In (lcWorkFile)

	If File(gcWorkDir+lcWorkFile+'.DBF')
		Erase(gcWorkDir+lcWorkFile+'.DBF')
	Endif

	If File(gcWorkDir+lcWorkFile+'.CDX')
		Erase(gcWorkDir+lcWorkFile+'.CDX')
	Endif

	If File(gcWorkDir+lcWorkFile+'.FPT')
		Erase(gcWorkDir+lcWorkFile+'.FPT')
	Endif

Endif

If Used(lcNoteLns)
	Use In (lcNoteLns)
Endif

*-- if user change setting [enter report <Preview> or <Run>]
If !llFrTime
	Set Hours To &lcSetHour
Endif  && end if user change setting [enter report <Preview> or <Run>].

*E301272,1 Restore old currency setting before exit.
If llMultCurr
	Set Currency To lcCurrSymb
	Set Currency &lcCurAlign
Endif
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/27/1998
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
Function lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]


lnMajSeg  = gfItemMask('SM')  && No. of major segments.
Dimension laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- if you does not find Non Major Type Color Code.
If !lfNMajType('C',lnMajSeg)
	= lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
Endif  && end if you does not find Non Major Type Color Code.

Store Len(lcNonMajPi) To lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + Alltrim(lcNonMajTl) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]


*B602590,1 Adjust currency symbol [Begin]
*-- if multi currency evaluate currency arrays [Begin]
If llMultCurr
	Dimension laCurrVal[1,1]

	If !Used('SYCCURR')
		=gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
	Endif

	Select Distinct CCURRCODE From SYCCURR Order By CCURRCODE Into Array laCurrVal
	Dimension laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]

	Select SYCCURR
	Set Order To CCURRCODE  && To VALIDATE currency code.
	For lnI = 1 To Alen(laCurrVal,1)
		= Seek(Alltrim(laCurrVal[lnI,1]))
		laCurrVal[lnI,1]  = Padr(laCurrVal[lnI,1],3)
		laCurrSmbl[lnI,1] = Alltrim(Padr(CCURRSMBL,3))
		laCurrDesc[lnI,1] = CCURRCODE + ' - ' + Alltrim(CCURRDESC)
	Endfor
Endif
*-- if multi currency evaluate currency arrays [Begin]
*B602590,1 Adjust currency symbol [Begin]

Return ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfNMajType
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/27/1998
*! Purpose   : Mask NonMajor segments .
*!*************************************************************
*! Called from : lfEvalSegs.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajType()
*!*************************************************************
Function lfNMajType
Parameters lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
For lnI = lnMajSegs + 1 To Alen(laMajSegs,1)

	If laMajSegs[lnI,1] = lcNMajType

		lcFree_Clr = Iif(Empty(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
		lnNonMajSt = Iif(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

		lcNonMajPi = Iif(Empty(lcNonMajPi),laMajSegs[lnI,3],;
			lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

		lcNonMajTl = Iif(Empty(lcNonMajTl),Padr(laMajSegs[lnI,2],Len(laMajSegs[lnI,3])),;
			lcNonMajTl + laMajSegs[lnI-1,6] + Padr(laMajSegs[lnI,2],Len(laMajSegs[lnI,3])))

	Endif

	*-- If you Find Color Type or Find Free Type and current type not Free.
	If laMajSegs[lnI,1] = 'C' Or (!Empty(lcFree_Clr) And laMajSegs[lnI,1] != 'F')
		Exit
	Endif   && end If you Find Color Type or Find Free Type and current type not Free.

Endfor    && end Loop Around Non Major elements.

Return !Empty(lcFree_Clr)
*-- end of lfNMajType.

*!*************************************************************
*! Name      : lfMakeExpr
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/18/98
*! Purpose   : Make expression for operator is either BETWEEN or INLIST.
*!*************************************************************
*! Called From : lfSscanData.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Operator expression.
*!*************************************************************
*! Example   : = lfMakeExpr()
*!*************************************************************
*:B802101,1 MAB 04/01/1999 This function is no longer in use after fix this bug.
Function lfMakeExpr
Parameters lcString


Private lnPipeNo,lcExpr

lnPipeNo = Occur('|',lcString)

lcExpr = ''
For lnI = 1 To lnPipeNo
	lcExpr    = Iif(Empty(lcExpr),"'" +;
		PADR(Substr(lcString,1,Atc('|',lcString)-1),6) + "'",;
		lcExpr + "," + "'" +;
		PADR(Substr(lcString,1,Atc('|',lcString)-1),6) + "'")
	lcString  = Substr(lcString,Atc('|',lcString)+1)
Endfor
Return (lcExpr + "," + "'" + Padr(lcString,6) + "'")
*-- end of lfMakeExpr.

*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
*! Purpose   : 1- Put both index and group expressions for all sort cases.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!*************************************************************
*! Example   : = lfGetRepVr()
*!*************************************************************
Function lfGetRepVr
*-- lcOutHeadL : Left  title of outer group header.
*-- lcOutHeadR : Right title of outer group header.
*-- lcInnHeadL : Left  title of inner group header.
*-- lcInnHeadR : Right title of inner group header.


lcHeader =''
If lcRpSortBy = 'S'
	lcOutHeadL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
	lcOutHeadR = [Style.cStyMajor]
	lcInnHeadL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19)+ '  : ']
	lcInnHeadR = [SUBSTR(Style,lnMajorLen + 2) + '  ----  ' + ALLTRIM(STYLE.Desc1)]
Else
	Store [''] To lcOutHeadL,lcOutHeadR,lcInnHeadL,lcInnHeadR
Endif

*B802246,1 in Case of ACCOUNT,ORDER,LOCATION,SALESREP another Sort by (Sort# 2)
*appear in option grid (Sort by Line#/Style)

lcLineCurr = lfCurrPrnt()

*-- Different sort by cases.
Do Case
Case lcRpSortBy = 'A'   && Sort by account

	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
	If lcRpSrt2 = 'L'    && Sort by Line#
		If llCurInGrp
			lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Else
			lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Endif
	Else      && Else Sort by Style
		If lcRpSrt2 = 'I'    && Sort by DC/Style
			If llCurInGrp
				*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][Start]
				*lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+ORDER+DIST_CTR+STORE+STYLE'
				lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+DIST_CTR+STORE+STYLE+ORDER'
				*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][End]
			Else
				*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][Start]
				*lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+DIST_CTR+STORE+STYLE'
				lcIndexTg = 'ACCOUNT+CORDTYPE+DIST_CTR+STORE+STYLE+ORDER'
				*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][End]
			Endif
		Else  && Else Sort by Style
			If llCurInGrp
				lcIndexTg = 'ACCOUNT+CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
			Else
				lcIndexTg = 'ACCOUNT+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
			Endif
		Endif
	Endif
	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]

	*-- report variables data account case [begin]
	lcSubTitle = 'Account Number'  && Report sort title
	lcInnGrp   = [ORDER]           && Inner .FRX group field.

	*B802246,1 changing these lines so as in layout, Customer PO#
	*field need to be closer to its title "Cust PO #"   [Begin.]
	*lcInnFootL = ['Order # ' + ORDER + '   Cust PO #']
	*lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*',ALLTRIM(OrdHdr.CustPo)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
	If llTextMode

		If lcRpSrt2 = 'I'    && Sort by DC/Style
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][Start]
			*lcStorCond = IIF(llTextMode,[.T.],[ORDHDR.MULTI="Y"])
			lcStorCond = [.T.]
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][End]
			lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
			lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE +"D.C. : "+CUSTOMER.DIST_CTR ,"")]
			lcInnFootR = ['']
		Else

			lcInnFootL = [ORDER + ', Cust PO#'+] +;
				[IIF(Ordhdr.MultiPo,'*Multi PO*',PADL(OrdHdr.CustPo,10)) + ' Enter in ' + ALLTRIM(DTOC(OrdHdr.Entered))]
			*********************
			*      lcOutFootL = [Customer.Account+' '+PADR(ALLTRIM(CUSTOMER.BtName),20)+"/"+EVALUATE(lcLineCurr)+', ' + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]
			*********************
			lcOutFootL = [Customer.Account+' '+PADR(ALLTRIM(CUSTOMER.BtName),15)+"/"+EVALUATE(lcLineCurr)+', ' + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]
			lcHeader ='Order'
		Endif
	Else
		If lcRpSrt2 = 'I'    && Sort by DC/Style
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][Start]
			*lcStorCond = IIF(llTextMode,[.T.],[ORDHDR.MULTI="Y"])
			lcStorCond = [.T.]
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][End]
			lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
			lcInnFootR = ['']
			lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE +"D.C. : "+CUSTOMER.DIST_CTR ,"")]
		Else
			lcInnFootL = ['Order # ' + ORDER + '     Cust PO #']
			lcInnFootR = [IIF(Ordhdr.MultiPo,'*Multi PO*', PADL(OrdHdr.CustPo,15)) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]  && Right title of inner group footer.
		Endif
		*B802246,1 [End.]

		lcOutFootL = ['Account # ' + Customer.Account]  && Left title of outer group footer.
		*************
		lcOutFootR = [PADR(ALLTRIM(CUSTOMER.BtName),30) +"/"+EVALUATE(lcLineCurr)+ ", " + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]  && Right title of outer group footer.
		*************
		*      lcOutFootR = [PADR(ALLTRIM(CUSTOMER.BtName),25) +"/"+EVALUATE(lcLineCurr)+ ", " + TRANSFORM(CUSTOMER.Phone1,lcPhonPict)]  && Right title of outer group footer.
		*-- report variables data account case [end]
	Endif
	If llCurInGrp
		*man
		*lcSeekVal  = [ACCOUNT+'O'+ORDER]
		*C102262,1 (Begin) Sort on proper orders not only 'O' type.
		*lcSeekVal  = [ACCOUNT+cCurrCode+'O'+ORDER]
		lcSeekVal  = [ACCOUNT+cCurrCode+cOrdType+ORDER]
		*C102262,1 (End)

		lcOutGrp   = [ACCOUNT+CCURRCODE]         && Outer .FRX group field.
	Else
		*man
		*lcSeekVal  = [ACCOUNT+cCurrCode+'O'+ORDER]
		*C102262,1 (Begin) Sort on proper orders not only 'O' type.
		*lcSeekVal  =  [ACCOUNT+'O'+ORDER]
		lcSeekVal  =  [ACCOUNT+cOrdType+ORDER]
		*C102262,1 (End)
		lcOutGrp   = [ACCOUNT]         && Outer .FRX group field.
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [End  ]

Case lcRpSortBy = 'O'    && Sort by order
	lcSubTitle = 'Order'
	lcInnGrp   = ['']
	lcInnFootL = ['']

	*C101569,1 Add sort by store if first sort is by order [Begin]
	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
	*IF lcRpSrt2 = 'L'    && Sort by Line#
	*  IF llCurInGrp
	*    lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
	*  ELSE
	*    lcIndexTg = 'CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
	*  ENDIF
	*ELSE                  && Else Sort by Style
	*  IF llCurInGrp
	*    lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
	*  ELSE
	*    lcIndexTg = 'CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
	*  ENDIF
	*ENDIF

	Do Case
	Case lcRpSrt2 = 'L'    && Sort by Line#
		If llCurInGrp
			lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Else
			lcIndexTg = 'CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Endif
	Case lcRpSrt2 = 'S'    && Sort by Style
		If llCurInGrp
			lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
		Else
			lcIndexTg = 'CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
		Endif

	Case lcRpSrt2 $ 'TY'

		If llCurInGrp
			lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STORE'
		Else
			lcIndexTg = 'CORDTYPE+ORDER+STORE'
		Endif

		*-- Sort by Store/Line#
		If lcRpSrt2="T"
			lcIndexTg = lcIndexTg + '+STR(LINENO,6)+STYLE'
		Else  && Sort by Store/Style
			lcIndexTg = lcIndexTg + '+STYLE+STR(LINENO,6)'
		Endif

		lcStorCond = Iif(llTextMode,[.T.],[ORDHDR.MULTI="Y"])
		lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
		lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE,"")]

	Case lcRpSrt2 $ 'I'    && Sort by DC/Style

		If llCurInGrp
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][Start]
			*lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+DIST_CTR+STORE+STYLE'
			lcIndexTg = 'CCURRCODE+CORDTYPE+DIST_CTR+STORE+STYLE+ORDER'
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][End]
		Else
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][Start]
			*lcIndexTg = 'CORDTYPE+ORDER+DIST_CTR+STORE+STYLE'
			lcIndexTg = 'CORDTYPE+DIST_CTR+STORE+STYLE+ORDER'
			*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][End]
		Endif
		*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][Start]
		*lcStorCond = IIF(llTextMode,[.T.],[ORDHDR.MULTI="Y"])
		lcStorCond = [.T.]
		*! B610907,1 MMT 11/11/2014 Order detail report not grouping by store in graphic format in case not multi store orders[T20141110.0030][End]
		lcInnGrp   = [IIF(EVALUATE(lcStorCond),STORE,"")]
		lcInnFootL = [IIF(EVALUATE(lcStorCond),"Store : "+STORE +"D.C. : "+CUSTOMER.DIST_CTR ,"")]
	Endcase
	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]
	*-- report variables data order case [begin]
	*C101569,1 Add sort by store if first sort is by order [End  ]

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
	If llTextMode
		If lcRpSrt2 $ 'TY'
			lcOutFootL = [Order+ '/' + EVALUATE(lcLineCurr)+] +;
				[IIF(ORDHDR.MULTI="Y",',Acct#' + Account,',Stor#' + Store) + ',Entered :' + ALLTRIM(DTOC(OrdHdr.Entered))]
			lcHeader   = 'Order/Curr.'
		Else
			lcOutFootL = [Order+ '/' + EVALUATE(lcLineCurr)+] +;
				[',Acct#' + Account + ',Entered in:' + ALLTRIM(DTOC(OrdHdr.Entered))]
			lcHeader   = 'Order/Curr.'
		Endif

	Else
		lcInnFootR = ['']
		If lcRpSrt2 $ 'TY'
			lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + Order + IIF(EVALUATE(lcStorCond),' Account:',' Store :')]
			lcOutFootR = [IIF(EVALUATE(lcStorCond),Account,Store) + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
			lcHeader   = ' Curr.'+'       '+'Acct. '
		Else
			lcOutFootL = [EVALUATE(lcLineCurr)+ "/"+'Order # ' + Order + '    Account # ']
			lcOutFootR = [Account  + '  Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
			lcHeader   = ' Curr.'+'       '+'Acct. '
		Endif

		*-- report variables data order case [end]
	Endif
	If llCurInGrp
		*C102262,1 (Begin) Sort on proper orders not only 'O' type.
		*lcSeekVal = [cCurrCode+'O'+ORDER]
		lcSeekVal = [cCurrCode+cOrdType+ORDER]
		*C102262,1 (End)
		lcOutGrp  = [CCURRCODE+ORDER]
	Else
		*C102262,1 (Begin) Sort on proper orders not only 'O' type.
		*lcSeekVal = ['O'+ORDER]
		lcSeekVal = [cOrdType+ORDER]
		*C102262,1 (End)
		lcOutGrp  = [ORDER]
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end ]

Case lcRpSortBy = 'S'      && Sort by style

	If llCurInGrp
		*! B609903,1 SAB 05/02/2012 Fix problem when user select summary format with sort by Style [T20120430.0011][Start]
		*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[Start]
		*!*        lcIndexTg = 'STYLE+CCURRCODE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
		*!*        lcSeekVal = [STYLE+CCURRCODE]
		*!*        lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)+CCURRCODE]
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
		* IF lcRpSrt2 = 'A'
		lcIndexTg = 'STYLE+CCURRCODE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
		lcSeekVal = [STYLE+CCURRCODE]
		lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)+CCURRCODE]
		*! B609903,1 SAB 05/02/2012 Fix problem when user select summary format with sort by Style [T20120430.0011][End]
		Do Case
		Case lcRpSrt2 = 'A'
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
			lcIndexTg = 'STYLE+CCURRCODE+ACCOUNT+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
			lcSeekVal = [STYLE+CCURRCODE+ACCOUNT]
			lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)+CCURRCODE]
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
			*ELSE
		Case lcRpSrt2 = 'O'
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
			lcIndexTg = 'STYLE+CCURRCODE+CORDTYPE+ORDER+DTOS(COMPLETE)+STR(LINENO,6)'
			lcSeekVal = [STYLE+CCURRCODE+CORDTYPE+ORDER]
			lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)+CCURRCODE]
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
			*ENDIF
		Case lcRpSrt2 = 'C'
			lcIndexTg = 'STYLE+CCURRCODE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
			lcSeekVal = [STYLE+CCURRCODE]
			lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)+CCURRCODE]
		Endcase
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
		*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[End]
	Else
		*! B609903,1 SAB 05/02/2012 Fix problem when user select summary format with sort by Style [T20120430.0011][Start]
		*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[Start]
		*!*        lcIndexTg = 'STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
		*!*        lcSeekVal = [STYLE]
		*!*        lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)]
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
		*IF lcRpSrt2 = 'A'
		lcIndexTg = 'STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
		lcSeekVal = [STYLE]
		lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)]
		*! B609903,1 SAB 05/02/2012 Fix problem when user select summary format with sort by Style [T20120430.0011][End]
		Do Case
		Case lcRpSrt2 = 'A'
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
			lcIndexTg = 'STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
			lcSeekVal = [STYLE]
			lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)+ACCOUNT]
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
			*ELSE
		Case  lcRpSrt2 = 'O'
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
			lcIndexTg = 'STYLE+CORDTYPE+ORDER+DTOS(COMPLETE)+STR(LINENO,6)'
			lcSeekVal = [STYLE+CORDTYPE+ORDER]
			lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)]
			*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[Start]
			*ENDIF
		Case  lcRpSrt2 = 'C'
			lcIndexTg = 'STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STR(LINENO,6)'
			lcSeekVal = [STYLE]
			lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)]
		Endcase
		*! B609846,1 MMT 02/27/2012 Order detail Sort#2 should be defaulted to Complete Date in case of Sort#1 is by Style[END]
		*! E302698,1 MMT 05/25/2010 add Sort2 in case of Sort by STyle and Print SKU# when Sort1 by Style and Sort2 by Account[End]
	Endif

	*   lcOutGrp  = [SUBSTR(Style,1,lnMajorLen)]

	*-- report variables data style case [begin]
	lcStyTitle = Iif ('GFITEM' $ Alltrim(Upper(lcStyTitle)),;
		EVALUATE(lcStyTitle),lcStyTitle)

	lcSubTitle = lcStyTitle
	lcInnGrp   = [SUBSTR(Style,lnMajorLen + 2)]

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
	If llTextMode
		lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + ' : '+]+;
			[SUBSTR(Style,lnMajorLen + 2)]
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
		*lcOutFootL = [ALLTRIM(Style.cStyMajor) + "/"+EVALUATE(lcLineCurr)+ ", "+ '(' + ALLTRIM(Style.Fabric) + ')']
		lcOutFootL = [ALLTRIM(Style.cStyMajor) + "/"+EVALUATE(lcLineCurr)+ ", "+ '(' + ALLTRIM(Style.cprifabric) + ')']
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
		lcHeader   = ''
	Else
		lcInnFootL = [PADR(SUBSTR(lcStyTitle,lnMajorLen + 2),19) + '     : ']
		lcInnFootR = [SUBSTR(Style,lnMajorLen + 2)]
		lcOutFootL = [PADR(SUBSTR(lcStyTitle,1,lnMajorLen),19) + '  : ']
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
		*lcOutFootR = [ALLTRIM(Style.cStyMajor) +"/"+EVALUATE(lcLineCurr)+ ", "+ '( ' + ALLTRIM(Style.Fabric) + ' )']
		lcOutFootR = [ALLTRIM(Style.cStyMajor) +"/"+EVALUATE(lcLineCurr)+ ", "+ '( ' + ALLTRIM(Style.cprifabric) + ' )']
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
		lcHeader   = ''
		*-- report variables data style case [end]
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end ]

Case lcRpSortBy = 'G'    && Sort by style group
	*B610529,1 TMI 09/25/2013 [Start] fabric now is 19 , start here from 20
	*IF llCurInGrp
	*  lcIndexTg = 'SUBSTR(cTempKey,8,6)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
	*  lcSeekVal = [SUBSTR(cTempKey,8,6)+CCURRCODE+STYLE]
	*  lcOutGrp  = [SUBSTR(cTempKey,8,6)+CCURRCODE]
	*ELSE
	*  lcIndexTg = 'SUBSTR(cTempKey,8,6)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
	*  lcSeekVal = [SUBSTR(cTempKey,8,6)+STYLE]
	*  lcOutGrp  = [SUBSTR(cTempKey,8,6)]
	*ENDIF
	If llCurInGrp
		lcIndexTg = 'SUBSTR(cTempKey,20,6)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		lcSeekVal = [SUBSTR(cTempKey,20,6)+CCURRCODE+STYLE]
		lcOutGrp  = [SUBSTR(cTempKey,20,6)+CCURRCODE]
	Else
		lcIndexTg = 'SUBSTR(cTempKey,20,6)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		lcSeekVal = [SUBSTR(cTempKey,20,6)+STYLE]
		lcOutGrp  = [SUBSTR(cTempKey,20,6)]
	Endif
	*B610529,1 TMI 09/25/2013 [End  ]

	*-- report variables data Style group case [begin]
	lcSubTitle = 'Style Group'
	lcInnGrp   = [STYLE]

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
	If llTextMode
		lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
		*B610529,1 TMI 09/25/2013 [Start] the first 19 characters are the fabric
		*lcOutFootL = [gfCodDes(SUBSTR(cTempKey,8,6),'CSTYGROUP',.T.)+"/"+EVALUATE(lcLineCurr)]
		lcOutFootL = [gfCodDes(SUBSTR(cTempKey,20,6),'CSTYGROUP',.T.)+"/"+EVALUATE(lcLineCurr)]
		*B610529,1 TMI 09/25/2013 [End  ]
		lcHeader   ='       '+lcStyTitle+'   '+'Description '
	Else
		lcInnFootL = ['Style  ( ' + Style + ' )  :']
		lcInnFootR = [ALLTRIM(Style.Desc)]
		*B610529,1 TMI 09/25/2013 [Start] the first 19 characters are the fabric
		*lcOutFootL = ['Group  ( ' + SUBSTR(cTempKey,8,6) + ' )  :']
		lcOutFootL = ['Group  ( ' + SUBSTR(cTempKey,20,6) + ' )  :']
		*B610529,1 TMI 09/25/2013 [End  ]
		*B126314,1 HMA 03/08/2005 Display Style Group Discription  [Begin]
		*lcOutFootR = [Codes.cDiscrep+"/"+EVALUATE(lcLineCurr)]
		*B610529,1 TMI 09/25/2013 [Start] the first 19 characters are the fabric
		*lcOutFootR = [gfCodDes(SUBSTR(cTempKey,8,6),'CSTYGROUP')+"/"+EVALUATE(lcLineCurr)]
		lcOutFootR = [gfCodDes(SUBSTR(cTempKey,20,6),'CSTYGROUP')+"/"+EVALUATE(lcLineCurr)]
		*B610529,1 TMI 09/25/2013 [End  ]
		*B126314,1 HMA 03/08/2005 Display Style Group Discription  [End]
		lcHeader   = '   '+lcStyTitle+'    '+' Description '
		*-- report variables data Style group case [end]
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end

Case lcRpSortBy = 'F'    && Sort by fabric
	*B610529,1 TMI 09/25/2013 [Start] fabric now is 19
	*IF llCurInGrp
	*  lcIndexTg = 'LEFT(cTempKey,7)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
	*  lcSeekVal = [LEFT(cTempKey,7)+CCURRCODE+STYLE]
	*  lcOutGrp  = [LEFT(cTempKey,7)+CCURRCODE]
	*ELSE
	*  lcIndexTg = 'LEFT(cTempKey,7)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
	*  lcSeekVal = [LEFT(cTempKey,7)+STYLE]
	*  lcOutGrp  = [LEFT(cTempKey,7)]
	*ENDIF
	If llCurInGrp
		lcIndexTg = 'LEFT(cTempKey,19)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		lcSeekVal = [LEFT(cTempKey,19)+CCURRCODE+STYLE]
		lcOutGrp  = [LEFT(cTempKey,19)+CCURRCODE]
	Else
		lcIndexTg = 'LEFT(cTempKey,19)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		lcSeekVal = [LEFT(cTempKey,19)+STYLE]
		lcOutGrp  = [LEFT(cTempKey,19)]
	Endif
	*B610529,1 TMI 09/25/2013 [End  ]

	*-- report variables data fabric case [begin]
	lcSubTitle = 'Primary Fabric'
	lcInnGrp   = [STYLE]

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
	If llTextMode
		lcInnFootL = ['Style (' + Style + ') :'+ALLTRIM(Style.Desc)]
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
		*lcOutFootL = [LEFT(cTempKey,7) + ':'+ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
		lcOutFootL = [LEFT(cTempKey,19) + ':'+ALLTRIM(EVALUATE(lcTmpFab+'.Desc'))+"/"+EVALUATE(lcLineCurr)]
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
		lcHeader = '       '+lcStyTitle +'   '+'Description'
	Else
		lcHeader = '         '+lcStyTitle +'  '+'       Description'
		lcInnFootL = ['Style  ( ' + Style + ' )  :']
		lcInnFootR = [ALLTRIM(Style.Desc)]
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[START]
		*!*	      lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,7) + ' )  :']
		*!*	      lcOutFootR = [ALLTRIM(Fabric.Desc)+"/"+EVALUATE(lcLineCurr)]
		lcOutFootL = ['Fabric  ( ' + LEFT(cTempKey,19) + ' )  :']
		lcOutFootR = [ALLTRIM(EVALUATE(lcTmpFab+'.Desc'))+"/"+EVALUATE(lcLineCurr)]
		*! B610502,1 MMT 09/08/2013 Fix Order Detail Report to use the SQL Fabric tables[End]
		*-- report variables data fabric case [end]
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end

Case lcRpSortBy = 'W'    && Sort by location
	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
	If lcRpSrt2 = 'L'       && Sort by Line#
		If llCurInGrp
			lcIndexTg = 'CWARECODE+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		Else
			lcIndexTg = 'CWARECODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		Endif
	Else                     && Else Sort by Style
		If llCurInGrp
			lcIndexTg = 'CWARECODE+CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Else
			lcIndexTg = 'CWARECODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Endif
	Endif

	If llCurInGrp
		lcOutGrp   = [CWARECODE+CCURRCODE]
	Else
		lcOutGrp   = [CWARECODE]
	Endif
	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]

	*-- report variables data location case [begin]

	lcSubTitle = 'Location'
	lcInnGrp   = ['']

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
	lcInnFootL = ['']
	If llTextMode
		lcOutFootL = [cWareCode + ':'+ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]
		lcHeader = '       Store/Curr.'
	Else
		lcHeader = '     '+'   '+'         Store/Currency'
		lcInnFootR = ['']
		lcOutFootL = ['Location # ' + cWareCode + ' :']
		lcOutFootR = [ALLTRIM(Warehous.cDesc)+"/"+EVALUATE(lcLineCurr)]
		*-- report variables data location case [end]
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end]

Case lcRpSortBy = 'R'    && Sort by sales representative
	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [Begin.]
	If lcRpSrt2 = 'L'       && Sort by Line#
		If llCurInGrp
			lcIndexTg = 'RIGHT(cTempKey,3)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		Else
			lcIndexTg = 'RIGHT(cTempKey,3)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		Endif
	Else                     && Else Sort by Style
		If llCurInGrp
			lcIndexTg = 'RIGHT(cTempKey,3)+CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Else
			lcIndexTg = 'RIGHT(cTempKey,3)+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
		Endif
	Endif
	*B802246,1 lcRpSrt2 means Sort# 2 which appear in OG is by Line#/Style [End.]

	If llCurInGrp
		lcOutGrp   = [RIGHT(cTempKey,3)+CCURRCODE]
	Else
		lcOutGrp   = [RIGHT(cTempKey,3)]
	Endif

	*-- report variables data sales Rep. case [begin]
	lcSubTitle = 'Primary Sales Representative'
	lcInnGrp   = ['']

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end
	lcInnFootL = ['']
	If llTextMode
		lcOutFootL = [RIGHT(cTempKey,3) + ':'+PADR(SalesRep.Name,21)+"/"+EVALUATE(lcLineCurr)]
		lcHeader = '  '+'  SalesRep. Name'+'/'+'Currency'
	Else
		lcHeader = '         '+'  SalesRep. Name'+'/'+'Currency'
		lcInnFootR = ['']
		lcOutFootL = ['Primary Sales Rep. # ' + RIGHT(cTempKey,3) + ' :']
		lcOutFootR = [SalesRep.Name+"/"+EVALUATE(lcLineCurr)]
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end]

	*-- report variables data sales Rep. case [end]

Case lcRpSortBy = 'D'    && Sort by complete date
	If llCurInGrp
		lcIndexTg = 'DTOS(COMPLETE)+CCURRCODE+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		lcOutGrp  = [DTOS(COMPLETE)+CCURRCODE]
	Else
		lcIndexTg = 'DTOS(COMPLETE)+STYLE+CORDTYPE+ORDER+STR(LINENO,6)'
		lcOutGrp  = [DTOS(COMPLETE)]
	Endif

	*-- report variables data Complete date case [begin]
	lcSubTitle = 'Complete Date'
	lcInnGrp   = ['']

	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [Begin]
	lcInnFootL = ['']
	If llTextMode
		*      lcOutFootL = ["Date Completed : " + DTOC(Complete)+"/"+EVALUATE(lcLineCurr)]
		lcOutFootL = ["Date Completed : " + DTOC(Complete)+" "+EVALUATE(lcLineCurr)]
		lcHeader = '      Complete'+'   '+'Curr.'
	Else
		lcHeader = '           Comp.'+'  '+'Curr.'
		lcInnFootR = ['']
		*      lcOutFootL = ['******']
		lcOutFootL = ["Date Completed : "]
		lcOutFootR = [DTOC(Complete)+" "+EVALUATE(lcLineCurr)]
		*      lcOutFootR = [DTOC(Complete)+"/"+EVALUATE(lcLineCurr)]
		*-- report variables data Complete date case [end]
	Endif
	*E301265,1==E301272,1 Define report variables in all sort cases in both text and graphic
	*E301265,1==E301272,1 format keep in mind multi currency situation. [end

	*E301272,1 multi currency situation sort by currency.
Case lcRpSortBy = 'U'    && Sort by currency

	If lcRpSrt2 = 'L'    && Sort by Line#
		lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STR(LINENO,6)+STYLE'
	Else                  && Else Sort by Style
		lcIndexTg = 'CCURRCODE+CORDTYPE+ORDER+STYLE+STR(LINENO,6)'
	Endif

	*-- report variables data order case [begin]
	lcSubTitle = 'Currency'
	lcInnGrp   = [ORDER]

	If llTextMode
		lcInnFootL = [Order+', Account# '+Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
		lcOutFootL = ["Currency : " + lfCurrDesc()]
		lcHeader   = 'Order'
	Else
		lcInnFootR = ['Order:'] &&why?????????
		lcInnFootL = [Order+', Account# '+Account  + ' Entered in: ' + ALLTRIM(DTOC(OrdHdr.Entered))]
		lcOutFootL = ['Currency : ']
		lcOutFootR = [lfCurrDesc()]
		lcHeader   = 'Order'
		*-- report variables data order case [end]
	Endif

	*C102262,1 (Begin) Sort on proper orders not only 'O' type.
	*lcSeekVal = [cCurrCode+'O'+ORDER]
	lcSeekVal = [cCurrCode+cOrdType+ORDER]
	*C102262,1 (End)
	lcOutGrp  = [CCURRCODE]

Endcase          && end Different sort by cases.

*E301272,1 if it is currency format and sort by any thing rather than currency.
If llCurInGrp And (lcRpSortBy <> 'U')
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*lcSubTitle = lcSubTitle+"/Currency"
	lcSubTitle = lcSubTitle+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BACKCURRENCY,oAriaApplication.GetHeaderText("LANG_BACKCURRENCY",AHEADERFILE))
	* N000682 ,1 Thabet Handle globalization issues [END]
Endif
*-- end of lfGetRepVr.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : Create cursor
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : Cursor Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCreatCur()
*!*************************************************************
Function lfCreatCur
Parameters lcCurName


*-- We need temp. files to be files not cursor to open it in another alias [Begin]

*CREATE CURSOR (lcCurName) ;
*   FROM ARRAY laTempStru
Local lnField, lnCol
For lnField = 1 To Alen(laTempStru,1)
	For lnCol = 7 To 16
		laTempStru[lnField, lnCol] = ""
	Endfor
	Store 0 To laTempStru[lnField, 17],laTempStru[lnField, 18]
Endfor

Create Table (gcWorkDir+lcCurName) ;
	FROM Array laTempStru
*-- We need temp. files to be files not cursor to open it in another alias [End  ]
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfPipeExpr
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : Mask inlist expression.
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
*! Example   : = lfPipeExpr('AS|BS|CS',2)
*!*************************************************************
*:B802113,1 MAB 04/01/1999 This function is no longer in use after fix this bug.
Function lfPipeExpr
Parameters lcString,lnPipeNo


Private lcExpr
lcExpr = ''

For lnI = 1 To lnPipeNo
	lcExpr    = Iif(Empty(lcExpr),"'" +;
		SUBSTR(lcString,1,Atc('|',lcString)-1) + "'",;
		lcExpr + "," + "'" +;
		SUBSTR(lcString,1,Atc('|',lcString)-1) + "'")
	lcString      = Substr(lcString,Atc('|',lcString)+1)
Endfor

Return (lcExpr + "," + "'" + lcString + "'")
*-- end of lfPipeExpr.

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : - Evaluate Status expression.
*!           : - Raise change status flag.
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
*! Example   : = lfvOStatus()
*!*************************************************************
Function lfvOStatus
Private lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
* N000682 ,1 Thabet Handle globalization issues [Start]
*=gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
=gfMover(@laRpSource,@laRpTarget,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORDERSTATUS,oAriaApplication.GetHeaderText("LANG_ORDERSTATUS",AHEADERFILE)),.T.,'')  && call mover function.
* N000682 ,1 Thabet Handle globalization issues [Start]

lcRpStatus = ' '
*-- Loop to make Status expression.

If !Empty(laRpTarget[1])
	For lnI = 1 To Alen(laRpTarget,1)
		* N000682 ,1 Thabet Handle globalization issues [Start]
		*!*	    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
		*!*	                              IIF(laRpTarget[lnI] = 'Open','O',;
		*!*	                              IIF(laRpTarget[lnI] = 'Hold','H',;
		*!*	                              IIF(laRpTarget[lnI] = 'Complete','C',;
		*!*	                              IIF(laRpTarget[lnI] = 'Cancelled','X','')))))
		lcRpStatus = lcRpStatus + Iif(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BID,oAriaApplication.GetHeaderText("LANG_BID",AHEADERFILE)),'B',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE)),'O',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_HOLD,oAriaApplication.GetHeaderText("LANG_HOLD",AHEADERFILE)),'H',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE)),'C',;
			IIF(laRpTarget[lnI] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE)),'X','')))))
		* N000682 ,1 Thabet Handle globalization issues [END]
	Endfor  && end Loop to make Status expression.
Endif

lcRpStatus = Iif(Empty(lcRpStatus),'BOHCX',Alltrim(lcRpStatus))

*-- if length of current selected status differ from previous length
If Len(lcOldStat) != Len(lcRpStatus)
	llOGFltCh = .T.

Else  && else if length of current selected status equal previous length

	*-- loop to check if it's the same selected status or not.
	For lnJ = 1 To Len(lcOldStat)
		lcCurrChr = Substr(lcOldStat,lnJ,lnJ)
		If !(lcCurrChr $ lcRpStatus)
			llOGFltCh = .T.
			Exit
		Endif
	Endfor  && end loop to check if it's the same selected status or not.
Endif

Do lpChkStat
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : lfGetWork
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : - Compute work proccessing
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : = lfGetWork()
*!*************************************************************
Function lfGetWork
Store '' To laStock,laWip,lnStkOrWip
*-- Calculate Wip and Stock Values [Begin]

*-- If User select specific locations
If llRpStyLoc
	lcRepAlias = Alias()
	Select (lcSlctFile)
	Store 0 To lnS1,lnS2,lnS3,lnS4,lnS5,lnS6,lnS7,lnS8,lnS9
	Store 0 To lnW1,lnW2,lnW3,lnW4,lnW5,lnW6,lnW7,lnW8,lnW9
	*-- Scan for Selected locations only.
	Scan
		*-- If you find this style location in Stydye file, compute its work proc.
		If Seek(&lcMastFile..Style + &lcSlctFile..cWareCode, 'STYDYE')

			Select STYDYE
			Sum Rest While Style+cWareCode+Dyelot = ;
				&lcMastFile..Style + &lcSlctFile..cWareCode ;
				Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk,;
				Wip1,Wip2,Wip3,Wip4,Wip5,Wip6,Wip7,Wip8,TotWip ;
				TO  lnStk1,lnStk2,lnStk3,lnStk4,lnStk5,lnStk6,lnStk7,lnStk8,lnTotStk,;
				lnWip1,lnWip2,lnWip3,lnWip4,lnWip5,lnWip6,lnWip7,lnWip8,lnTotWip

			*-- if you print stock or (stock and wip)
			If Inlist(lcRpStyPrn,'S','P')
				lnS1 = lnS1 + lnStk1
				lnS2 = lnS2 + lnStk2
				lnS3 = lnS3 + lnStk3
				lnS4 = lnS4 + lnStk4
				lnS5 = lnS5 + lnStk5
				lnS6 = lnS6 + lnStk6
				lnS7 = lnS7 + lnStk7
				lnS8 = lnS8 + lnStk8
				lnS9 = lnS9 + lnTotStk

			Endif  && end if you print stock or (stock and wip)

			*-- if if you print wip or (stock and wip)
			If Inlist(lcRpStyPrn,'W','P')
				lnW1 = lnW1 + lnWip1
				lnW2 = lnW2 + lnWip2
				lnW3 = lnW3 + lnWip3
				lnW4 = lnW4 + lnWip4
				lnW5 = lnW5 + lnWip5
				lnW6 = lnW6 + lnWip6
				lnW7 = lnW7 + lnWip7
				lnW8 = lnW8 + lnWip8
				lnW9 = lnW9 + lnTotWip

			Endif  && end if you print wip or (stock and wip)

		Endif  && end If you find this style location in Stydye file.
	Endscan  && end Scan for Selected locations only.
	Select (lcRepAlias)

	*-- if you print stock or (stock and wip) and total stock not equal 0
	If Inlist(lcRpStyPrn,'S','P') And (lnS9 # 0)
		laStock[1] = Iif(lnS1 = 0 ,'',Transform(lnS1,'99999'))
		laStock[2] = Iif(lnS2 = 0 ,'',Transform(lnS2,'99999'))
		laStock[3] = Iif(lnS3 = 0 ,'',Transform(lnS3,'99999'))
		laStock[4] = Iif(lnS4 = 0 ,'',Transform(lnS4,'99999'))
		laStock[5] = Iif(lnS5 = 0 ,'',Transform(lnS5,'99999'))
		laStock[6] = Iif(lnS6 = 0 ,'',Transform(lnS6,'99999'))
		laStock[7] = Iif(lnS7 = 0 ,'',Transform(lnS7,'99999'))
		laStock[8] = Iif(lnS8 = 0 ,'',Transform(lnS8,'99999'))
		laStock[9] = Iif(lnS9 = 0 ,'',Transform(lnS9,'9999999'))
	Endif

	*-- if you print wip or (stock and wip) and total wip not equal 0
	If Inlist(lcRpStyPrn,'W','P') And (lnW9 # 0)
		laWip[1] = Iif(lnW1 = 0 ,'',Transform(lnW1,'99999'))
		laWip[2] = Iif(lnW2 = 0 ,'',Transform(lnW2,'99999'))
		laWip[3] = Iif(lnW3 = 0 ,'',Transform(lnW3,'99999'))
		laWip[4] = Iif(lnW4 = 0 ,'',Transform(lnW4,'99999'))
		laWip[5] = Iif(lnW5 = 0 ,'',Transform(lnW5,'99999'))
		laWip[6] = Iif(lnW6 = 0 ,'',Transform(lnW6,'99999'))
		laWip[7] = Iif(lnW7 = 0 ,'',Transform(lnW7,'99999'))
		laWip[8] = Iif(lnW8 = 0 ,'',Transform(lnW8,'99999'))
		laWip[9] = Iif(lnW9 = 0 ,'',Transform(lnW9,'9999999'))
	Endif

	lnStkOrWip = Transform(lnS9 + lnW9,'999999')  && Calculate wip + stock values
	lnStkOrWip = Iif(Val(lnStkOrWip) = 0 , '' , lnStkOrWip)

Else  && User does not select specific locations.

	If Inlist(lcRpStyPrn,'S','P')
		laStock[1] = Iif(Style.Stk1 = 0,'',Transform(Style.Stk1,'99999'))
		laStock[2] = Iif(Style.Stk2 = 0,'',Transform(Style.Stk2,'99999'))
		laStock[3] = Iif(Style.Stk3 = 0,'',Transform(Style.Stk3,'99999'))
		laStock[4] = Iif(Style.Stk4 = 0,'',Transform(Style.Stk4,'99999'))
		laStock[5] = Iif(Style.Stk5 = 0,'',Transform(Style.Stk5,'99999'))
		laStock[6] = Iif(Style.Stk6 = 0,'',Transform(Style.Stk6,'99999'))
		laStock[7] = Iif(Style.Stk7 = 0,'',Transform(Style.Stk7,'99999'))
		laStock[8] = Iif(Style.Stk8 = 0,'',Transform(Style.Stk8,'99999'))
		laStock[9] = Iif(Style.TotStk = 0,'',Transform(Style.TotStk,'9999999'))
	Endif

	If Inlist(lcRpStyPrn,'W','P')
		laWip[1] = Iif(Style.Wip1 = 0,'',Transform(Style.Wip1,'99999'))
		laWip[2] = Iif(Style.Wip2 = 0,'',Transform(Style.Wip2,'99999'))
		laWip[3] = Iif(Style.Wip3 = 0,'',Transform(Style.Wip3,'99999'))
		laWip[4] = Iif(Style.Wip4 = 0,'',Transform(Style.Wip4,'99999'))
		laWip[5] = Iif(Style.Wip5 = 0,'',Transform(Style.Wip5,'99999'))
		laWip[6] = Iif(Style.Wip6 = 0,'',Transform(Style.Wip6,'99999'))
		laWip[7] = Iif(Style.Wip7 = 0,'',Transform(Style.Wip7,'99999'))
		laWip[8] = Iif(Style.Wip8 = 0,'',Transform(Style.Wip8,'99999'))
		laWip[9] = Iif(Style.TotWip = 0,'',Transform(Style.TotWip,'9999999'))
	Endif

	*-- Calculate Wip and Stock Values [End]

	lnStkOrWip = Transform(Style.TotStk + Style.TotWip,'999999')  && Calculate wip + stock values
	lnStkOrWip = Iif(Val(lnStkOrWip) = 0 , '' , lnStkOrWip)

Endif  && end If User select specific locations .

Return ''
*-- end of lfGetWork.

*!*************************************************************
*! Name      : lfWorkEnd
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : - End Compute work proccessing
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : = lfWorkEnd()
*!*************************************************************
Function lfWorkEnd


Store '' To laStock,laWip,lnStkOrWip
Return ''
*-- end of lfWorkEnd.

*!*************************************************************
*! Name      : lfArrDumy
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/27/99
*! Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************
*B802418,1 Adjust array dimensions
*-- Function to fill select by and sort by arrays.
Function lfArrDumy
Private lnSelElms , lnSrtElms


lnSelElms = 4
lnSrtElms = 6
Dimension laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
	laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1],;
	laSort2Des[2,1],laSort2Val[2,1]

*-- Sort 2 elementes [Begin]
* N000682 ,1 Thabet Handle globalization issues [Start]
*laSort2Des[1,1] = "Line#"
laSort2Des[1,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LINENO,oAriaApplication.GetHeaderText("LANG_LINENO",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]
laSort2Des[2,1] = lcStyMajor
laSort2Val[1,1] = "L"
laSort2Val[2,1] = "S"

*-- Sort 2 elementes [End  ]

*-- Sort 1 base elements [Begin]
* N000682 ,1 Thabet Handle globalization issues [Start]
*!*	laSortDesc[1,1] = 'Account'
*!*	laSortDesc[2,1] = 'Order'
*!*	laSortDesc[3,1] = lcStyMajor
*!*	laSortDesc[4,1] = lcStyMajor + ' Group'
*!*	laSortDesc[5,1] = 'Primary Sales Representative'
*!*	laSortDesc[6,1] = 'Complete Date'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
laSortDesc[1,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNT,oAriaApplication.GetHeaderText("LANG_ACCOUNT",AHEADERFILE))
laSortDesc[2,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORDER,oAriaApplication.GetHeaderText("LANG_ORDER",AHEADERFILE))
laSortDesc[3,1] = lcStyMajor
laSortDesc[4,1] = lcStyMajor + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GROUP,oAriaApplication.GetHeaderText("LANG_GROUP",AHEADERFILE))
laSortDesc[5,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PRIMSALES,oAriaApplication.GetHeaderText("LANG_PRIMSALES",AHEADERFILE))
laSortDesc[6,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPDATE,oAriaApplication.GetHeaderText("LANG_COMPDATE",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]
laSortVal[1,1]  = 'A'
laSortVal[2,1]  = 'O'
laSortVal[3,1]  = 'S'
laSortVal[4,1]  = 'G'
laSortVal[5,1]  = 'R'
laSortVal[6,1]  = 'D'
*-- Sort 1 base elements [End  ]

*-- Fill Select by array base elements. [Begin]
* N000682 ,1 Thabet Handle globalization issues [Start]
*!*	laSlctDesc[1,1] = 'Account'
*!*	laSlctDesc[2,1] = 'Primary Sales Representative'
*!*	laSlctDesc[3,1] = lcStyMajor
*!*	laSlctDesc[4,1] = 'All'
laSlctDesc[1,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCOUNT,oAriaApplication.GetHeaderText("LANG_ACCOUNT",AHEADERFILE))
laSlctDesc[2,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PRIMSALES,oAriaApplication.GetHeaderText("LANG_PRIMSALES",AHEADERFILE))
laSlctDesc[3,1] = lcStyMajor
laSlctDesc[4,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,oAriaApplication.GetHeaderText("LANG_ALL",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [End]
laSlctVal[1,1]  = 'A'
laSlctVal[2,1]  = 'R'
laSlctVal[3,1]  = 'S'
laSlctVal[4,1]  = 'L'
*-- Fill Select by array base elements. [End  ]

If llMultLoc
	lnSelElms = lnSelElms + 1
	lnSrtElms = lnSrtElms + 1
	Dimension laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
		laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]

	=Ains(laSortDesc,5,1)
	=Ains(laSortVal,5,1)
	=Ains(laSlctDesc,2,1)
	=Ains(laSlctVal,2,1)
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*STORE 'Location' TO laSortDesc[5,1],laSlctDesc[2,1]
	Store Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LOCATION,oAriaApplication.GetHeaderText("LANG_LOCATION",AHEADERFILE))  To laSortDesc[5,1],laSlctDesc[2,1]
	* N000682 ,1 Thabet Handle globalization issues [END]
	Store 'W' To laSortVal[5,1],laSlctVal[2,1]
Endif

If 'MA' $ gcCmpModules
	lnSelElms = lnSelElms + 1
	lnSrtElms = lnSrtElms + 1
	Dimension laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1],;
		laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]

	=Ains(laSortDesc,5,1)
	=Ains(laSortVal,5,1)
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*lnInsFabIn = ASCAN(laSlctDesc,'All',1)
	lnInsFabIn = Ascan(laSlctDesc,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,oAriaApplication.GetHeaderText("LANG_ALL",AHEADERFILE)),1)
	* N000682 ,1 Thabet Handle globalization issues [END]
	=Ains(laSlctDesc,lnInsFabIn,1)
	=Ains(laSlctVal,lnInsFabIn,1)
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*STORE 'Fabric' TO laSortDesc[5,1],laSlctDesc[lnInsFabIn,1]
	Store  Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FABRIC,oAriaApplication.GetHeaderText("LANG_FABRIC",AHEADERFILE)) To laSortDesc[5,1],laSlctDesc[lnInsFabIn,1]
	* N000682 ,1 Thabet Handle globalization issues [End]
	Store 'F' To laSortVal[5,1],laSlctVal[lnInsFabIn,1]
Endif

*E301272,1 Add sort by currency if multi currency company.
If llMultCurr
	lnSrtElms = lnSrtElms + 1
	Dimension laSortDesc[lnSrtElms,1],laSortVal[lnSrtElms,1]
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*laSortDesc[ALEN(laSortDesc,1),1] = "Currency"
	laSortDesc[ALEN(laSortDesc,1),1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRENCY,oAriaApplication.GetHeaderText("LANG_CURRENCY",AHEADERFILE))
	* N000682 ,1 Thabet Handle globalization issues [End]
	laSortVal[ALEN(laSortDesc,1),1]  = "U"
Endif
*-- end of lfArrDumy.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
Function lfItmPos
Parameters lcItmInFlt
Private lnItmPos

lnItmPos = Ascan(laOGFxFlt,lcItmInFlt)
If lnItmPos > 0
	lnItmPos = Asubscript(laOGFxFlt,lnItmPos,1)
Endif
Return lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfvCurr
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/31/99
*! Purpose   : set currency symbol
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCurr()
*!*************************************************************
Function lfvCurr
*-- end of lfvCurr.


*!**************************************************************************
*! Name      : lfAssignSc
*! Developer : Sameh (SSE)
*! Date      : 05/17/99
*! Purpose   : to save the current Scale after printing it in order not to
*!             print it except when Scale changes
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfAssignSc()
*!**************************************************************************
*B802246,1
Function lfAssignSc
*B604559,1 (Begin) Remark the following line and don't get the scale unless
*B604559,1         it's printed once.
*lcOldScale = Scale
*IF llFirstSc
lcOldScale = Scale
*ENDIF
llFirstSc = .T.
*B604559,1 (End)
*E303409,1 TMI 08/18/2013 [Start] update the lcOldSty,lcOldStyClr
lcOldSty    = Substr(Style.Style ,1,lnNonMajSt-1)
lcOldStyClr = Substr(Style.Style ,1,lnNonMajSt+lnColorLen-1)
*E303409,1 TMI 08/18/2013 [End  ]

Return ''
*-- end of lfAssignSc.

*!**************************************************************************
*! Name      : lfScaleSzWhen
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/17/2013
*! Purpose   : the when expression of the size scale
*!**************************************************************************
*E303409,1 TMI 08/18/2013
Function lfScaleSzWhen
Parameters lcSz
Local llRet
llRet = (Inlist(lcRpSortBy,'S','F','G') And llBofPage ) Or Inlist(lcRpSortBy,'S','F','G') And ;
	!Empty(Scale.Sz&lcSz) And ;
	IIF(!(lcRpKind='D' And lcRpSortBy='S' And llRpSizes) , Scale <> lcOldScale ,;
	iif(lcRpPrScSz='S',lcOldSty <> Substr(Style ,1,lnNonMajSt-1),;
	lcOldStyClr <> Substr(Style ,1,lnNonMajSt+lnColorLen-1)))
Return llRet
*!**************************************************************************
*! Name      : lfScalePgH
*! Developer : Sameh (SSE)
*! Date      : 05/24/99
*! Purpose   : to empty lcOldScale var. in each Page Header Band in
*!             order to be printed once at the start of the Page if
*!             the Scale is not changed
*!**************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : All FRXs
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : ....
*!**************************************************************************
*! Example   : = lfScalePgH()
*!**************************************************************************
*B802246,1
Function lfScalePgH
lcOldScale = Space(3)
Return ''
*-- end of lfScalePgH.

*!*************************************************************
*! Name      : lfInnGrpIn
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/17/1999
*! Purpose   : Evaluate inner group values, when you enter group header.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!**************************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfInnGrpIn()
*!*************************************************************
*E301265,1
Function lfInnGrpIn

lcInnGrpIn  = Evaluate(lcInnGrp)
Return ''
*-- end of lfInnGrpIn.

*!*************************************************************
*! Name      : lfOutGrpIn
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/17/1999
*! Purpose   : Evaluate outer group values, when you enter group header.
*!*************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfOutGrpIn()
*!*************************************************************
*E301265,1
Function lfOutGrpIn
lcOutGrpIn = Evaluate(lcOutGrp)
Return ''
*-- end of lfOutGrpIn.

*!*************************************************************
*! Name      : lfInnGrpOp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/17/1999
*! Purpose   : Evaluate inner group values, when you in group Footer.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfInnGrpOp()
*!*************************************************************
*E301265,1
Function lfInnGrpOp
*C101569,1 Add sort by store if first sort is by order [Begin]
llLstMulti = (ORDHDR.Multi = "Y")
*C101569,1 Add sort by store if first sort is by order [End  ]
lcInnGrpOp = Evaluate(lcInnGrp)
lcPrnInnL  = Evaluate(lcInnFootL)

lnPrnInnQ1 = lnInnQty1
lnPrnInnQ2 = lnInnQty2
lnPrnInnQ3 = lnInnQty3
lnPrnInnQ4 = lnInnQty4
lnPrnInnQ5 = lnInnQty5
lnPrnInnQ6 = lnInnQty6
lnPrnInnQ7 = lnInnQty7
lnPrnInnQ8 = lnInnQty8
lnPrnInnTQ = lnInnTtQty
lnPrnInnAm = lnGrInnAmt
Return ''
*-- end of lfInnGrpOp.

*!*************************************************************
*! Name      : lfOutGrpOp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/17/1999
*! Purpose   : Evaluate outer group values, when you in group Footer.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfOutGrpOp()
*!*************************************************************
*E301265,1
Function lfOutGrpOp

lcOutGrpOp = Evaluate(lcOutGrp)
lcPrnOutL  = Evaluate(lcOutFootL)

lnPrnOutQ1 = lnOutQty1
lnPrnOutQ2 = lnOutQty2
lnPrnOutQ3 = lnOutQty3
lnPrnOutQ4 = lnOutQty4
lnPrnOutQ5 = lnOutQty5
lnPrnOutQ6 = lnOutQty6
lnPrnOutQ7 = lnOutQty7
lnPrnOutQ8 = lnOutQty8
lnPrnOutTQ = lnOutTtQty
lnPrnOutAm = lnGrOutAmt
Return ''
*-- end of lfOutGrpOp.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies exchange rates.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301272,1
Function lfvCurDisp

*llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCurrPrnt()
*!*************************************************************
*!E301272,1
Function lfCurrPrnt
Private lcCurrCode


*-- Not Multi Currency Or it is and any Equavelent method.
If !llMultCurr Or lcRpCurr <> "F"
	lcCurrCode = [gcBaseCurr]
Else && Multi Currency and Print forign currency.
	lcCurrCode = [Ordhdr.cCurrCode]
Endif
Return lcCurrCode
*-- end of lfCurrPrnt.

*!*************************************************************
*! Name      : lfChCurSm
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Share with last function to Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : All FRXs (DOS Format)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChCurSm()
*!*************************************************************
*!E301272,1
Function lfChCurSm
Private lcCurrCurr
lcCurrCurr = Alltrim(Evaluate(lcLineCurr))

Local lnRemoteResult, lcSelectCommand
lcSelectCommand = [SELECT cCurrency, cCurrencyI FROM SYCINT WHERE cCurrCode = '] + lcCurrCurr + [']
lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand,"","SYCINT","",;
	oAriaApplication.SystemConnectionString,3,"")

If lnRemoteResult >= 1
	Select SYCINT
	Locate
	If !Found()
		Return ""
	Endif
Else
	Return ""
Endif

lcCurrRtL = Alltrim(cCurrency)
lcCurrSet = Alltrim(cCurrencyI)
Set Currency To lcCurrSet
Set Currency &lcCurrRtL

Return ''
*-- end of lfChCurSm.
*
*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Example   : =lfCurrDesc()
*!*************************************************************
*!E301272,1
Function lfCurrDesc
Private lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = Alltrim(CCURRCODE)
lnCurVlPos = Ascan(laCurrVal,lcCurrVal)
If lnCurVlPos > 0
	lcCurDesc  = laCurrDesc[lnCurVlPos,1]
Endif
Return lcCurDesc
*-- end of lfCurrDesc.

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!
Function lfvEdiOrd
lcRpEdiFlt = ""
Do Case
	* -- Orders
Case lcRpEdiPrn="O"
	lcRpEdiFlt = [OrdHdr.cOrdType = 'O' AND !OrdHdr.lEdiOrder]
	lcRpOrdTyp = 'O'
	* -- EDI recieved orders
Case lcRpEdiPrn="R"
	lcRpEdiFlt = [OrdHdr.cOrdType = 'O' AND OrdHdr.lEdiOrder]
	lcRpOrdTyp = 'O'
	* -- EDI temp orders
Case lcRpEdiPrn="T"
	lcRpEdiFlt = [OrdHdr.cOrdType = 'T']
	lcRpOrdTyp = 'T'
Case lcRpEdiPrn="B"
	* -- Both Orders + EDI Orders.
	lcRpEdiFlt = [OrdHdr.cOrdType = 'O']
	lcRpOrdTyp = 'O'
Case lcRpEdiPrn="A"
	* -- All
	lcRpEdiFlt = ''
	lcRpOrdTyp = ''
Endcase
llClearOrd = .T.
*-- end of lfvEdiOrd.

*!*****************************************************************************************
*! Name      : RefreshStatus
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/15/2002 11:18:46 PM
*! Purpose   :
*! Entry no. :
*!*****************************************************************************************
*!
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
*-- end of RefreshStatus.

*!*****************************************************************************************
*! Name      : lfFillArry
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/15/2002 11:18:46 PM
*! Purpose   : Fill the seasons array.
*! Entry no. :
*!*****************************************************************************************
*!
Function lfFillArry
Dimension laSeasnS[2,2]
* N000682 ,1 Thabet Handle globalization issues [Start]
*!*	  laSeasnS[1,1] = lcStyMajor +' season'
*!*	  laSeasnS[2,1] = 'Order header season'
laSeasnS[1,1] = lcStyMajor +Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SEASON,oAriaApplication.GetHeaderText("LANG_SEASON",AHEADERFILE))
laSeasnS[2,1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORDHDRSEASON,oAriaApplication.GetHeaderText("LANG_ORDHDRSEASON",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]
Dimension laSeasnV[2,2]
laSeasnV[1,1] = 'S'
laSeasnV[2,1] = 'O'
Endfunc
*-- end of lfFillArry.



*!*************************************************************
*! Name      : lfvPrtPck
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/09/2001
*! Purpose   : Function To Validate Print Pack details Yes/No
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfvPrtPck()
*!*************************************************************
*C200438,1
Function lfvPrtPck

lfvSizes()

*!* IF !llRpPrTPak
*!*   lcRpForm = "SORDDGMP"
*!* ELSE
*!*   lcRpForm = "SORDDETA"
*!*   IF lcRpKind = 'S'
*!*     lcRpForm = "SORDDGMB"
*!*   ELSE
*!*     lcRpForm = "SORDDGMA"
*!*   ENDIF



*!**************************************************************************
*! Name      : lfSRPackId
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/09/2002
*! Purpose   : Go top in the Spck_Hdr file when browse.
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSRPackId()
*!**************************************************************************
*C200438,1
Function lfSRPackId
Parameters OpGrdParm
Select Spck_Hdr

Do Case
Case  OpGrdParm='S'
	lcTmpIndex =gfTempName()
	Select  Pack_id,cPkColor,cPckSize,cPkVersion From Spck_Hdr Into Dbf (gcWorkDir + lcTmpIndex)
	Use In Spck_Hdr
	Use In (lcTmpIndex)
	*!* B610757,1 HES 06/23/2014 Add pack_ID filter to the order detail report [START]
	*!*	    USE (gcWorkDir + lcTmpIndex) IN 0 ALIAS SPCK_HDR
	Use (gcWorkDir + lcTmpIndex) In 0 Alias Spck_Hdr Exclusive
	*!* B610757,1 HES 06/23/2014 Add pack_ID filter to the order detail report [END  ]
	Select Spck_Hdr
	Index On Pack_id+cPkColor+cPckSize+cPkVersion Tag SPCK_HDRVR Unique
	Go Top
Case  OpGrdParm='R'
	Use In Spck_Hdr
	=gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SPCK_HDRVR','SH')
Endcase

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/14/2002
*! Purpose   : Function To get Size discreption
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
*C200438,1
Function lfGetGmSz
Parameter lcPackSize

lnAlias = Select(0)
Private lcNombr,lnScalePos
If !Empty(lcPackSize)
	*--svae Scale Positon
	lnScalePos = Recno('SCALE')
	=Seek('S'+Left(lcPackSize,1),'SCALE')
	lcNombr = Right(lcPackSize,1)
	lcSize=Eval('SCALE.SZ'+lcNombr)
	If Between(lnScalePos,1,Reccount('SCALE'))
		Goto lnScalePos In Scale
	Endif
Else
	lcSize ='*****'
Endif

Select(lnAlias)
Return lcSize


*!***************************************************************************
*! Name      : lfvChkOrd
*! Developer : Adel Mohhamed El Gazzar (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to check if the user selects ONE multi store order.
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  =lfvChkOrd()
*!***************************************************************************
*C102351,1
Function lfvChkOrd

Go Top In (laOGFxFlt[lnOrderPos,6])
llOneMulti = (Seek('O'+Evaluate(laOGFxFlt[lnOrderPos,6]+'.Order'),'OrdHdr') Or ;
	SEEK('C'+Evaluate(laOGFxFlt[lnOrderPos,6]+'.Order'),'OrdHdr') Or ;
	SEEK('T'+Evaluate(laOGFxFlt[lnOrderPos,6]+'.Order'),'OrdHdr')) And ;
	ORDHDR.Multi = 'Y'
If !llOneMulti
	Store '' To laStores , laStorTar
Endif

Return llOneMulti




*:**************************************************************************
*:* Name        : lfCrPckInd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/21/2003
*:* Purpose     :  Create the index on Pack_id field if not exist , or update it if found
*:***************************************************************************
*:* Called from : lfScanData
*:***************************************************************************
*B607305,1
Function lfCrPckInd
Private lnResp,lcSvAls,lcSvOrd
Local lcErrHndl,llErr
lnResp = 1
lcSvAls = Select()
lcSvOrd = Order('ORDLINE')

If !File(gcDataDir+'PACKINDX.IDX') .Or. llRpUpPInd
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*  lnResp = gfModalGen('QRM00000B04004',.F.,.F.,.F.,;
	'Do you want to create/update temp. index for Packs in OrdLine file?'+CHR(13)+'This may take time..')
	* N000682 ,1 Thabet Handle globalization issues [Start]
	lnResp = gfModalGen('QRM00000B04004',.F.,.F.,.F.,;
		IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Do_you_want_to_create,oAriaApplication.GetHeaderText("LANG_Do_you_want_to_create",AHEADERFILE)) +Chr(13)+ Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_This_may_take_time,oAriaApplication.GetHeaderText("LANG_This_may_take_time",AHEADERFILE)))
	* N000682 ,1 Thabet Handle globalization issues [END]
	If lnResp = 1
		* N000682 ,1 Thabet Handle globalization issues [Start]
		*WAIT WINDOW NOWAIT 'Creating/Updating temp. index for Packs in OrdLine file..'
		Wait Window Nowait Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_Updating_temp_index,oAriaApplication.GetHeaderText("LANG_Creating_Updating_temp_index",AHEADERFILE))
		*N000682,1 11/20/2012 MMT Globlization changes[End]

		* N000682 ,1 Thabet Handle globalization issues [END]
		Select ORDLINE
		lcErrHndl = On("ERROR")
		On Error llErr = .T.
		Erase("gcDataDir+'PACKINDX.IDX')
		If !llErr
			Index On Pack_id+cPkColor+cPckSize+cPkVersion+Style To (gcDataDir+'PACKINDX.IDX')
		Endif
		On Error &lcErrHndl
	Endif
Endif
If !Empty(lcSvOrd )
	Set Order To &lcSvOrd In ORDLINE
Endif
Select (lcSvAls)
Return (lnResp=1)
*-- end of lfCrPckInd.

*!*************************************************************
*! Name      : lfUpType
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/09/2001
*! Purpose   : Function To Update Type
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfUpType()
*!*************************************************************
*C200438,1
Function lfUpType
Private lcPack_id , lcorder , lcStore
Store '' To lcPack_id , lcorder ,lcStore
Scan
	If Empty(Pack_id)
		Replace cType With 'S'
	Else
		*--mhm2003
		*REPLACE cType WITH 'P'
		*C200438,4 TMI [Start] Correct the expression "lcPack_id = lcPack_id"
		*C200438,4                                 to "lcPack_id =   Pack_id"
		*IF lcPack_id = lcPack_id AND lcOrder = Order AND lcStore = STORE
		If lcPack_id = Pack_id And lcorder = Order And lcStore = Store
			*C200438,4 TMI [End  ]
			Dele
		Else
			Replace cType With 'P'
			*C200438,4 TMI [Start] Correct the expression "lcPack_id = lcPack_id"
			*C200438,4                                 to "lcPack_id =   Pack_id"
			*lcPack_id = lcPack_id
			lcPack_id = Pack_id
			*C200438,4 TMI [End  ]
			lcorder = Order
			lcStore = Store
		Endif
		*--MHM2003
	Endif
Endscan


*!*************************************************************
*! Name      : lfGetPkQty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/09/2001
*! Purpose   : Function To get Pack Qty
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfGetPkQty()
*!*************************************************************
*C200438,1
Function lfGetPkQty
Private lnPackQty , lcAlias,lcSpkOrd ,lnrecNo

lnAlias = Select(0)

*! B610634,1 MMT 12/25/2013 Order detail frx SORDDGMP.FRX not installed with R13 Media[Start]
If !Used('SPCK_LIN')
	=gfOpenTable('SPCK_LIN','SPCK_LIN','SH')
Endif
*! B610634,1 MMT 12/25/2013 Order detail frx SORDDGMP.FRX not installed with R13 Media[End]

Select SPCK_LIN
lcSpkOrd = Order('SPCK_LIN')
Set Order To Tag Spck_linvr

Select (lcMastFile)
lnrecNo= Recno()
If !Seek("P"+ACCOUNT+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')
	=Seek("P*****"+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')
Endif
lnPackQty = Iif(SPCK_LIN.TOTQTY<>0,TOTQTY,0)/Iif(SPCK_LIN.TOTQTY<>0,SPCK_LIN.TOTQTY,1)

Select SPCK_LIN
Set Order To Tag &lcSpkOrd
Select (lcMastFile)

If Between(lnrecNo,1,Reccount())
	Goto lnrecNo
Endif

Select(lnAlias)
Return lnPackQty


*:**************************************************************************
*:* Name        : lfGetPkPcs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/25/2003
*:* Purpose     : Sum the number of pieces of the current pack
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetPkPcs()
*:***************************************************************************
Function lfGetPkPcs
Private lnAlias,lcSpkOrd,lnrecNo,lcPack,lnTotPcs
lnAlias = Select(0)
Select SPCK_LIN
lcSpkOrd = Order('SPCK_LIN')
Set Order To Tag Spck_linvr

Select (lcMastFile)
lnrecNo= Recno()
If !Seek("P"+ACCOUNT+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')
	=Seek("P*****"+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')
Endif
lcPack = 'P'+SPCK_LIN.ACCOUNT+Pack_id+cPkColor+cPckSize+cPkVersion
Select SPCK_LIN
lnTotPcs = 0
Scan Rest While Type+ACCOUNT+Pack_id+cPkColor+cPckSize+cPkVersion+Style = lcPack
	lnTotPcs = lnTotPcs + SPCK_LIN.TOTQTY
Endscan

Select SPCK_LIN
Set Order To Tag &lcSpkOrd
Select (lcMastFile)
If Between(lnrecNo,1,Reccount())
	Goto lnrecNo
Endif

Select(lnAlias)
Return lnTotPcs

*!*************************************************************
*! Name      : lfGetPkPrc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 05/09/2001
*! Purpose   : Function To get Size discreption
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfGetPkPrc()
*!*************************************************************
*C200438,1
Function lfGetPkPrc
Private lnPackPrc , lcAlias,lcSpkOrd ,lnrecNo

lnAlias = Select(0)
Select SPCK_LIN
lcSpkOrd = Order('SPCK_LIN')
Set Order To Tag Spck_linvr

Select (lcMastFile)
lnrecNo = Recno()
If !Seek("P"+ACCOUNT+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')
	=Seek("P*****"+Pack_id+cPkColor+cPckSize+cPkVersion+Style,'SPCK_LIN')
Endif
lnPackPrc = SPCK_LIN.nPck_Price

Select SPCK_LIN
Set Order To Tag &lcSpkOrd
Select (lcMastFile)

If Between(lnrecNo,1,Reccount())
	Goto lnrecNo
Endif

Select(lnAlias)

Return lnPackPrc

*!**************************************************************************
*! Name      : lpvGMAStor
*! Developer : Sameh Saiid Ezzat (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to select order's stores.
*! Reference : B604784,1
*!**************************************************************************
*! Example   : =lpvGMAStor()
*!**************************************************************************
*
Function lpvGMAStor
Private lcorder
Dimension laStorScr[1]
laStorScr = ''

lcorder   = Evaluate(laOGFxFlt[lnOrderPos,6]+'.Order')

*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [Start]
*IF (SEEK('O'+lcOrder,'OrdHdr') AND SEEK('O'+lcOrder,'OrdLine')) OR ;
(SEEK('C'+lcOrder,'OrdHdr') AND SEEK('C'+lcOrder,'OrdLine')) OR ;
(SEEK('T'+lcOrder,'OrdHdr') AND SEEK('T'+lcOrder,'OrdLine'))

If (Seek('O'+lcorder,'OrdHdr','OrdHdr') And Seek('O'+lcorder,'OrdLine','OrdLine')) Or ;
		(Seek('C'+lcorder,'OrdHdr','OrdHdr') And Seek('C'+lcorder,'OrdLine','OrdLine')) Or ;
		(Seek('T'+lcorder,'OrdHdr','OrdHdr') And Seek('T'+lcorder,'OrdLine','OrdLine'))
	*E302321,1 MMT 11/02/2006 Enhancement on Summerize multiStore option appearence [End]

	Select ORDLINE
	Scan While cOrdType+Order = ORDHDR.cOrdType+lcorder
		*! B609891,1 MMT 04/18/2012 Order Detail report does not show all order stores [Start]
		*IF ASCAN(laStorScr,ALLTRIM(Store)) = 0
		If Ascan(laStorScr,Store) = 0
			*! B609891,1 MMT 04/18/2012 Order Detail report does not show all order stores [End]
			If !Empty(laStorScr[ALEN(laStorScr,1)])
				Dimension laStorScr[ALEN(laStorScr,1)+1]
			Endif
			laStorScr[ALEN(laStorScr,1)] = Alltrim(Store)
		Endif
	Endscan
Endif

If !Empty(laStorScr)
	If llOGFltCh
		Dimension laStorTar[1]
		laStorTar = .F.
	Endif
	* N000682 ,1 Thabet Handle globalization issues [Start]
	*= gfMover(@laStorScr,@laStorTar,'Select Stores ',.T.)
	= gfMover(@laStorScr,@laStorTar,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELSTORE,oAriaApplication.GetHeaderText("LANG_SELSTORE",AHEADERFILE)),.T.)
	* N000682 ,1 Thabet Handle globalization issues [End]
	laStores = ""
	If !Empty(laStorTar[1])
		=Acopy(laStorTar,laStores)
	Endif
Endif
*-- End of lpvGMAStor.

*!**************************************************************************
*! Name      : lfBofPage
*! Developer : Sameh Saiid Ezzat (ADEL)
*! Date      : 07/15/2001
*! Purpose   : Function to set begin of bage off
*! Reference : B131649,1 AYM
*!**************************************************************************
*! Example   : =lfBofPage()
*!**************************************************************************
Function  lfBofPage
llBofPage= .T.
Return ''

*-- End of lfBofPage.


*!**************************************************************************
*! Name      : lfBofPage
*! Developer : AYMAN MAHMOUD  AHMED (AYM)
*! Date      : 03/29/2006
*! Purpose   : Function to set begin of bage off
*! Reference : B131649,1 AYM
*!**************************************************************************
*! Example   : =lfBofPage()
*!**************************************************************************
Function  lfEofPage
llBofPage=.F.
Return ''

*-- End of lfEofPage.

*!**************************************************************************
*! Name      : lfSort2Vld
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/29/2006
*! Purpose   : Function to validate Sort# 2 option
*! Reference : E302321
*!**************************************************************************
Function lfSort2Vld
llRpSummMt = .F.
ClearRead()

*!**************************************************************************
*! Name      : lpGetClrSgStart
*! Developer : Saber Saber (SAB)
*! Date      : 03/07/2011
*! Purpose   : Pro to get Color Segment Start Position
*! Reference : B609543
*!**************************************************************************
Procedure lpGetClrSgStart
If (lnClrStart == 0)
	Local laItemSeg, lnColorSegmantLength, lnCount
	lnColorSegmantLength = 0
	Declare laItemSeg[1,1]
	lnCount =gfItemMask(@laItemSeg)
	For lnCount = 1 To Alen(laItemSeg,1)
		If laItemSeg[lnCount,1]='C'
			lnClrStart = laItemSeg[lnCount,4]
			Exit
		Endif
	Endfor
Endif
Endproc

