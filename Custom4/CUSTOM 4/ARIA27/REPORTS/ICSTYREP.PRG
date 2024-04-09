*:***************************************************************************
*: Program file  : ICSTYREP
*: Program desc. : Style transactions report
*: For Report    : ICSTYBYW.FRX,ICSTYBYO.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSTYREP
*:*************************************************************************************************
*: Modifications : 
*: B602607   : AAMER add option for printing zero qtys.
*: B602961,1 : MAB 06/02/1999 Print from master file if Sort by Style Or By Location
*:           :                and user want to print one transaction.
*: B802364,1 : MAB 06/28/1999 If sort by any thing rather than Style/Location and select 
*: B802364,1 :                one transaction only, report must be old form.
*: B802345,1 : MAB 06/28/1999 Style Filter is In Range filter instead of Between filter.
*: B802576,1 : RAMY 09/09/1999 Fixed the cost field when transaction is 
*                              STOCK passed on IC settings .
*: B603198,1 : AHM 10/06/1999 Suprress Non Maj. Seg from Syrepuvr
*: B802813,1   AHM 11/21/1999 Fixing the bug of Variable TOTCOST not found
*: B802813,1                  in case of costing method is standard and 
*: B802813,1                  print stock only and print dyelots of stock
*: B802905,1   BWA 03/01/2000 1) Fix the bug of when select any transaction ,we will get
*:                               the size scales for the first record only of the style 
*:                               and the rest of the report will be printed without 
*:                               changing the header for the size scales.
*:                            2)  Modify an option to Print size scale =(Y/N).
*: B803043,1 ADEL 02/15/2000 Fix the bug of printing Zeros for ALLOCATED quantities when choosing
*:                           to print any one or more transactions beside ALLOCATED transactions.
*: B603294,1 KHM 05/15/2000 Fix the bug of showing no styles in the report when
*: B603294,1                choosing only one transaction that has a zero 
*: B603294,1                quantity in all sizes and you chose 'N' in the 
*: B603294,1                show zero qty option.
*: E301158,1 HBG 05/28/2000  - Add new option for printing sizes or not. If 
*:                           one transaction is selected sizes can be printed or not,
*:                           If more than one transaction is selected sizes must be printed.
*:                           - Make new format for ICSTYBYS.FRX &
*:                           ICSTYBYL.FRX for printing size "No".
*: B603692,1 AMH 06/29/2000 Fix the bug of not printing the subtotal of style when
*: B603692,1                choosing only one transaction and you chose 'N' in the 
*: B603692,1                show zero qty option.
*: B603675,1 AMH 07/02/2000 Fix the bug of Printing Return Auth. Qty. Replaced with  
*: B603675,1                data of Return Qty. in case of more than one transaction
*: B603670,1 ADEL 07/04/2000 Show only the Finished Good Inv. warehouses.
*: B603773,1 SSE 07/25/2000 Print Standard Cost in Case of Costing method Standard &
*: B603773,1                Average Cost if not Standard   
*: B603667,1 MHM 07/26/2000 Change Color Code
*: B603713,4 AAN 09/25/2000 Resize Price and PriceA from 10 to 12
*: E301481,1 ABD 02/06/2001 Print the report in graphic mode  to read the arabic data.
*: B604163,1 ABD 02/18/2001 Fix bug that we must print the average cost instead 
*: B604163,1 ABD            of the unit cost in all case without looking to cost method.
*: B804262,1 BWA 07/05/2001 Fix the bug of Sales value is not printing in the style summery report in case stock transaction.[DOS FRX]
*: B604657,1 ASH 07/17/2001 Fix the bug of color description is not printed if it is more than 13 characters. [All DOS FRX's]
*: B804338,1 TMI 08/20/2001 When prints 2 copies of the reports the grand totals are duplicated
*: B804338,1 TMI            Also the alignment of the totals are not correct.This because the totals
*: B804338,1 TMI            are created in string variables and are evaluated in run time.Non alignment was because
*: B804338,1 TMI            the variying lenght of charachters in windows depending on font type
*: B804338,1 TMI            The soloution was to separate each total in one variable in one text box on the report.
*: B604949,1 BWA 01/10/2001 Fix the bug of the 2nd line in case the extended size scale doesn't appear.[Fix also in ICSTYBYL.FRX]
*: B605101,1 BWA 11/12/2001 Fix the bug of get a message there is no records to display although there is data to print.
*: B605109,1 BWA 11/12/2001 Fix the bug of not printing the description of the style.[FIX IN FRX ICSTYBYS]
*: B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.
*: B605518,1 BWA 02/19/2002 Fix the bug of expression is missed.[FIX IN ICSTYBYL.FRX]
*: B604876,1 BWA 04/18/2002 Fix sevral bugs in the program.
*: B606071,1 BWA 07/07/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.
*: B606379,1 BWA 08/15/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.
*: B606596,1 BWA 12/11/2002 Fix the bug of not printing the scale size in case more style have the same scale.
*: B606756,1 BWA 01/05/2003 Fix the bug of not printing the color and style descreption in some cases.[FIX IN FRX]
*: B606972,1 WAB 03/09/2003 fix the problem of not printing the negative OTS in case of sorting by location
*: B119818,1 MMM 10/15/2003 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard
*: B119818,1 MMM              and Link to GL=yes but Generate Gl Cost=No
*: B119818,1 MMM            2-Fix Bug of Printing wrong cost value based on any selection except
*: B119818,1 MMM              (By style and by location) with selection Print Location detail(YES).
*: B119983,1 MMM 11/05/2003 Fix Bug of not printing  subtotals when only this color is selected.
*: B037622,1 NNA 02/03/2004 Change The Sort By item (Fabric Group) To (Primary Fabric) in The Grid Screen and The Program 
*: B121770,1 NNA 03/01/2004 Fix Bug of the net setting (Print OTS Quantities) not show the net amount available whether it be P. or N.
*: B122005,1 NNA 03/07/2004 Fix bug when printing Both cost and sales value and transaction type (Stock)
*: B122005,1 NNA            and show sizes,the cost values last digites is cut off (fix in prg. - Icstybyl.frx - IcstybyS.Frx)
*: C121346,1 TMI 04/26/2004 Add a new sort by option : sort by material type ,a field fabric.item_type is used for styles those have primary fabric
*: B038431,1 NNA 08/24/2004 Fix bug of An error Massage [Variable 'llAdded' Not Found] that Appears 
*: B038431,1 NNA            run the Report and Sorted by Location and there is no Transcations for any Style
*: B123878,1 BWA 09/08/2004 Fix the bug of getting error "Missing expression" when run the report.
*: B123985,1 BWA 09/16/2004 Fix the bug of not printing the "Style Total" and "Grand Total" in ICSTYBYO.FRX (Fix in Frx)
*: B125652,1 BWA 12/16/2004 Fix the bug printing the data in a very long time.
*: B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard
*: B129994,1 MMR            2-Fix bug of printing color name only with the first style in case 
*: B129994,1 MMR              sort by style and print locatin detail=yes
*: B130981,1 EIH 02/16/2006 Fix bug of printing "Grand Total :" not at the end of the report in case of sort by 'Style Group' 
*: B132603,1 TMI 07/03/2006 Fix a bug : when selecting only one transaction the report is very slow at REV03
*: B607838,1 TMI 11/19/2006 fix a bug that when selecting a warehous not all "Totals " lines are displayed       ( T20061113.0005 ) 
*: B608089,1 TMI 05/20/2007 Add a new option "Include Zero Stock" , so that the filter added in B607838 will be based on , not on the original option "llRPShwZer"
*: B608237,1 NNA 08/23/2007 remove the modification that done by B608089 and let the report works as it used to work before this modification
*: B608368,1 NNA 11/30/2007 Ticket:[T20071026.0002] Fix an error of (Internal consistency error) that was happen only with GPS 
*: B608368,1 NNA            (a british customer) this bug fixed in the Frx(icstybyo.frx) only at statment [EVAL(lcSortExp) + EVAL(lcStyTmp+lcSortFld)]
*:************************************************************************************************************************************
*
lcTime     =  gfGetTime()

*B602961,1 [Begin]

*B606596,1 BWA 12/11/2002 Variable hold the style.[START]
PRIVATE lcStylScl
STORE SPACE(0) TO lcStylScl
*B606596,1 BWA 12/11/2002.[END]

*B606071,1 BWA 07/07/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
*B606071,1 BWA 07/07/2002 Variable hold the first record for the printing file.[START]
lnFrstRec = 0

*B606379,1 BWA 08/15/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
*B606379,1 BWA 08/15/2002 Variable hold the first record for the printing file.[START]
lnFrstRec1 = 0
*B606379,1 BWA 08/15/2002.[END]

*B606071,1 BWA 07/07/2002.[END]

*B604876,1 BWA 04/18/2002 about the bug of printing 2 lines in the 2nd page , fix in the FRX.[START]
*#No1
*About the bug of there are lines printed in the next page from the first page.
*The fix in icstybyo.frx about this bug
*The fix is : 1) I add a variable called "lncount" to be able to print the scale one time when the page no = 1
*             2) I add to the supexpr filed of the scales the condition of the page and the lncount
*B604876,1 BWA 04/18/2002.[END]

*B119818 ,1 MMM 10/15/2003 Adding Variable To Hold The Generate Cost Entries Value. [Start]
*B129994,1  MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
*llGnGlCst= ALLTRIM(gfGetMemVar('M_GL_COST')) = 'Y'
*:B129994,1.[End]
*B119818 ,1 MMM [End]
*B605553,1 BWA 02/19/2002 Check the link to Gl.[START]
llLinkGlJl = ALLTRIM(gfGetMemVar('M_LINK_GL')) = 'Y'
*B605553,1 BWA 02/19/2002 .[END]

*E301481,1 ABD -Print the report in graphic mode  to read the arabic data to 
*E301481,1 ABD -Do that you should now if you print in dos or graphic mode. [Begin]
*-- llTextMode :  Hold .t. or .f. upon print in text format (Dos Mode) or graphic format.
llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format
DIMENSION laScals[8]
STORE "" TO laScals
*E301481,1 ABD -[End]

*-- if sort by Style or By Location and user select one transaction to print.
IF lcRPSortBy $ 'SW' AND ALEN(laRPRepTar,1) = 1 AND !EMPTY(laRPRepTar)
  =lfSWOneTrn()  && Call Style,Location One transaction Function.
  RETURN
ENDIF
*B602961,1 [End  ]
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

DIMENSION laAllVal[1],laUAloVal[9],laIOTSVal[9],laOTSVal[9],laBokVal[9],;
          laStkVal[14],laSalVal[14]
STORE 0 TO laAllVal,laUAloVal,laIOTSVal,laOTSVal,laBokVal,laStkVal,laSalVal
 
STORE 0 TO lnMajSV,lnSorSV,lnRepSV
*-- This is to create and declare the variables that hold the totals 
*-- in the Group footer

STORE .T. TO llOnlyOTS

*B802905,1 BWA 03/01/2000 Fix the bug of not printing the size scale and modify a new option[START]
lcScale1 = SPACE(3)
*B802905,1 BWA [END]
IF ALEN(laRPRepTar,1) > 0 AND !EMPTY(laRPRepTar)
  IF llRPPrnLoc AND !llMultiWH
    llRPWhDeta = .T.
  ENDIF
  = lfCrTmp()
  = lfDummFill()
  WAIT "Collecting data..." WINDOW NOWAIT
  IF lcRPSortBy = 'W'
    = lfDatCollW()
  ELSE
    = lfDatCollS()
  ENDIF
  WAIT CLEAR
ELSE
  *-- "You have to select transaction to be printed"
  *-- <OK>
  = gfModalGen("INM42146B00000","Dialog")  
  RETURN
ENDIF  

lnOldRec = 0
lcOldFld = SPACE(1)

GO TOP
IF EOF()
  WAIT "No records to display" WINDOW
ELSE

  *B604876,1 BWA 04/18/2002 Fix the bug of not printing the scales in the 
  *header in case sort by Season and division.[START]
  IF lcRPSortBy == 'SE' OR lcRPSortBy == 'D'
    FOR LnLop = 1 TO 8
      lcSiz = 'SZ' + ALLTRIM(STR(LnLop))
      laScals[LnLop] = SCALE.&lcSiz
    ENDFOR
  ENDIF

  *#No2 
  *The bug of the 2 styles one is domestic and the other is not.
  *The fix in the program and in the frx
  *In the Frx in the fileds of browsing the style , color and descreption I add this condintion :
  *IIF( lcRPSortBy $ 'SED'  AND CENDREP = "A"  , "Grand Total : "
  *The fix in the program.
  * Search fo the record that Ahmed put it in the end of the records to print the grand total and replace
  *in it the last season and division to be able to print it in the end of the report.

  IF lcRPSortBy == 'SE' OR lcRPSortBy == 'D'
    GO BOTTOM
    STORE SPACE(6) TO lcSeason , lcDivision
    lcSeason   = Season
    lcDivision = Division
    LOCATE FOR CENDREP = "A"
    REPLACE Season   WITH lcSeason ,;
            Division WITH lcDivision
    LOCATE
  ENDIF
  *B604876,1 BWA 04/18/2002.[END]
  
  
  *B130981,1 EIH 02/16/2006 Fix bug of printing "Grand Total :" not at the end of the report in case of sort by 'Style Group' [Begin]
  IF lcRPSortBy == 'SG'
    GOTO BOTTOM
    SCATTER MEMVAR MEMO
    LOCATE FOR cEndMaj == 'A' AND cEndSort == 'A' AND cEndRep == 'A'
    REPLACE SGROUP   WITH  M.SGROUP   ,;
            STYCODE  WITH  M.STYCODE  ,;
            WARECODE WITH  M.WARECODE  ,;
            DYELOT   WITH  M.DYELOT          
    LOCATE        
  ENDIF
  *B130981,1 EIH 02/16/2006 [End]
  
  DO gfDispRe WITH EVAL('lcRPFormNa')
ENDIF

IF USED(lcTrns)
  SELECT(lcTrns)
  SET RELATION TO
  USE IN (lcTrns)
ENDIF  
IF USED(lcDummy)
  SELECT(lcDummy)
  SET RELATION TO
  USE IN (lcDummy)
ENDIF  
IF USED(lcStyTmp)
  SELECT(lcStyTmp)
  SET RELATION TO
  USE IN (lcStyTmp)
ENDIF  
IF USED(lcTotTmp)
  SELECT(lcTotTmp)
  SET RELATION TO
  USE IN (lcTotTmp)
ENDIF  


ERASE (gcWorkDir+lcTrns+".DBF")
ERASE (gcWorkDir+lcTrns+".CDX")
ERASE (gcWorkDir+lcDummy+".DBF")
ERASE (gcWorkDir+lcDummy+".CDX")
ERASE (gcWorkDir+lcStyTmp+".DBF")
ERASE (gcWorkDir+lcStyTmp+".CDX")
ERASE (gcWorkDir+lcTotTmp+".DBF")
ERASE (gcWorkDir+lcTotTmp+".CDX")

*-- Function section

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
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

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
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
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

DIMENSION laSetUp[4,2]
laSetUp[1,1] = 'M_WAREHOUS'
laSetUp[2,1] = 'M_WARELOC'
laSetUp[3,1] = 'M_DYELOT'
laSetUp[4,1] = 'M_COST_MET'

=gfGetMemVar(@laSetUp)
llMultiWH = ALLTRIM(laSetUp[1,2]) = 'Y'
llTrakLoc = ALLTRIM(laSetUp[2,2]) = 'Y'
llDyelot  = ALLTRIM(laSetUp[3,2]) = 'Y'
*aan
lcCstMeth = ALLTRIM(laSetUp[4,2])

llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')
*
RETURN ''

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
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

FUNCTION lfwRepWhen

*-- Check the cost access

*MAB HERE
*llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')

*B603198 (Start)
*Comment this part because therse is no need to do that as long as suppresing
*the Non Maj. Seg. will be done by suppres exp. in Syrepuvr
*lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]


*DO CASE
*  CASE lcFreeClr = 'C'
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
*  CASE lcFreeClr = 'F'
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
*  OTHERWISE
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
*ENDCASE
*B603198 (End)

*-- Disable/enable Only This colors, Free Segment. [end]

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
*E301158,1 (HBG) 05/29/2000 to adjust the status of "Print Sizes" and "Print Size scal" [Begin]
= lfPrtSizSt()
= lfPrtSzScl()
*E301158,1 (HBG) 05/29/2000 [End]

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfPrtSizSt
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/29/2000
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
*!E301158,1

FUNCTION lfPrtSizSt

PRIVATE lnVarSzPos 

lnVarSzPos = ASCAN(laOGObjType,'llRpPrtSiz')
IF lnVarSzPos > 0
  lnVarSzPos = ASUBSCRIPT(laOGObjType,lnVarSzPos,1)
  laOGObjCnt[lnVarSzPos] = ALEN(laRPRepTar,1) = 1 
ENDIF
= lfOGShowGet('llRpPrtSiz')

*-- end of lfPrtSizSt
*!*************************************************************
*! Name      : lfPrtSzScl
*! Developer : Hend Ghanem (HBG)
*! Date      : 06/04/2000
*! Purpose   : adjust the status of "Print Sizes Scal"
*!*************************************************************
*! Called from : lfwRepWhen & lfvPrnReps
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrtSzScl()
*!*************************************************************
*!E301158,1

FUNCTION lfPrtSzScl

PRIVATE lnVarSzPos 

lnVarSzPos = ASCAN(laOGObjType,'llRpScale')
IF lnVarSzPos > 0
  lnVarSzPos = ASUBSCRIPT(laOGObjType,lnVarSzPos,1)
  laOGObjCnt[lnVarSzPos] = llRpPrtSiz
ENDIF
= lfOGShowGet('llRpScale')

*-- end of lfPrtSzScl

*!*************************************************************
*! Name      : lfvPrnReps
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate transactions to be printed option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrnReps()
*!*************************************************************

FUNCTION lfvPrnReps
= lfTransArr()

*-- This is to clear target array in case the user change what he want to 
*-- print

*E301158,1 (HBG) 05/29/2000 Add an option to print sizes and validate it with # of transactions selected[Begin]

*= gfMover(@laRPRepSou,@laRPRepTar,"Transactions to be printed",.T.,.F.,.F.,.T.)
= gfMover(@laRPRepSou,@laRPRepTar,"Transactions to be printed",.T.,"lfvPrtSize",.F.,.T.)
= lfPrtSizSt()
= lfPrtSzScl()

*E301158,1 (HBG) 05/29/2000 [End  ]

*-- Mab Here [Begin]
=lfChngForm()  && Change report form.
*-- Mab Here [End  ]

=lfvSort()    &&this is to check plan report

= lfOTSbStat()
= lfDyeDtStat()
*-- end of lfvPrnReps.

*!*************************************************************
*! Name      : lfvPrtSize
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/28/2000
*! Purpose   : Validate Sizes to be printed 
*!*************************************************************
*! Called from : gfMover()
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrtSize()
*!*************************************************************
*!E301158,1
FUNCTION lfvPrtSize
PARAMETERS lnButtn

PRIVATE lnMesButtn,llReturn

llReturn = .T.

IF INLIST(lnButtn,1,2)
  *-- if the option of printing the size is "No" ,check how many transaction are selected
  IF !llRpPrtSiz
 
    *-- If more than one transaction are selected have a massege to tell that the 
    *-- option of the sizes can't be "No" then return it "Yes" and disable it 
    
    IF ( ALEN(laTarget,1) = 1 AND !EMPTY(laTarget) ) OR lnButtn = 2

      lnMesButtn = gfModalGen('QRM42215B00012',.F.,"",.F.,"")
      
      llReturn = lnMesButtn = 1
      llRpPrtSiz = llReturn
      
    ENDIF && end If more than one transaction are selected
  
  ENDIF   && if the option of printing the size is "No" check how many transaction are selected
ENDIF

RETURN (llReturn)

*-- end of lfvPrtSize


*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : To get major segment Picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfvWareHo
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWareHo()
*!*************************************************************

FUNCTION lfvWareHo

lcWareHo = VARREAD()

lcTag = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
  *B603670,1 (Begin) Only Finished goods inventory locations can be selected.
  *&lcWareHo = gfBrowWare(.T.)  
  &lcWareHo = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  *B603670,1 (End)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF

SET ORDER TO WAREHOUS IN WAREHOUS

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************

FUNCTION lfvFabric

lcFabObj = VARREAD()

lcFab    = &lcFabObj

llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')

SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF

SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF  

*!*************************************************************
*! Name      : lfvSort
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
llPlanFoun = ASCAN(laRPRepTar,"Plan") > 0

IF (lcRPSortBy = "W" OR llRPWhDeta) AND llPlanFoun

  *-- "Plan" cannot be printed while sorting by Location or printing locations detail
  *-- <OK>
  = gfModalGen("INM42147B00000","Dialog")  

  lnArrLen = ALEN(laRPRepTar)
  = ADEL(laRPRepTar,ASCAN(laRPRepTar,"Plan"))
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

*!*************************************************************
*! Name      : lfvPrnWhDet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate print location detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfWhsOptSt
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : To adjust enabling status of "Location in list"
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Nonen
*!*************************************************************
*! Example     : = lfWhsOptSt()
*!*************************************************************

FUNCTION lfWhsOptSt
llWhsStat =  lcRPSortBy = 'W' OR llRPWhDeta

*MAB HERE
lnWhsPo = ASCAN(laOGVrFlt,'STYDYE.CWARECODE')
IF lnWhsPo > 0
  lnWhsPo = ASUBSCRIPT(laOGVrFlt,lnWhsPo,1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnWhsPo] = llWhsStat
  = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnWhsPo)) + ',6]')
ENDIF  
*--

*!*************************************************************
*! Name      : lfvPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate print Cost and price option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfOTSbStat
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : to adjust the status of "OTS based on" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOTSbStat()
*!*************************************************************

FUNCTION lfOTSbStat

lnOTSSig = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPOTSSig'),1)
lnOTSMin = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lnRPOTSMin'),1)
lnOTSB   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPOTSB'),1)

llOTSFoun = ASCAN(laRPRepTar,"OTS") > 0 OR ASCAN(laRPRepTar,"Imm. OTS") > 0

IF llOTSFoun
  IF (!(lcRPSortBy == "W") AND  !llRPWhDeta)
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
ELSE
    llStatOB = .F.
    llStatOS = .F.
    llStatOM = .F.
ENDIF

laOGObjCnt[lnOTSB]   = llStatOB
laOGObjCnt[lnOTSSig] = llStatOS
laOGObjCnt[lnOTSMin] = llStatOM

= lfOGShowGet('lcRPOTSB')
= lfOGShowGet('lcRPOTSSig')
= lfOGShowGet('lnRPOTSMin')

*!*************************************************************
*! Name      : lfvOts
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : to validate "OTS based on" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
    lnRPOTSMin = -1
ENDCASE

*!*************************************************************
*! Name      : lfDyeDtStat
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : to adjust the status of "Print Dyelots for stock" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
  llStat = ASCAN(laRPRepTar,"Stock") > 0
ELSE
  llStat = .F.
ENDIF  

*MAB HERE
lnPrnDyeP = ASCAN(laOGObjType,'llRPPrnDye')
IF lnPrnDyeP > 0
  lnPrnDyeP = ASUBSCRIPT(laOGObjType,lnPrnDyeP,1)
  laOGObjCnt[lnPrnDyeP] = llStat
  IF !llStat
    llRPPrnDye = .F.
  ENDIF
  = lfOGShowGet('llRPPrnDye')
ENDIF 


*!*************************************************************
*! Name      : lfWhDtStat
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : to adjust the status of "Print location detail" option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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

*--B801193 (Start)
*llMultiWH = ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS')))  = 'Y'        

*MAB HERE
*DIMENSION laSetUp[2,2]
*laSetUp[1,1] = 'M_WAREHOUS'
*laSetUp[2,1] = 'M_WARELOC'
*=gfGetMemVar(@laSetUp)
*llMultiWH = ALLTRIM(laSetUp[1,2]) = 'Y'
*llTrakLoc = ALLTRIM(laSetUp[2,2]) = 'Y'
*--B801193 (End)

*-- if it is not multi location OR it is multi location but the sort by
*-- location, we neednot location detail
llStat = llMultiWH AND lcRPSortBy <> 'W'

*MAB HERE [Begin]
lnPWDPo = ASCAN(laOGObjType,'llRPWhDeta')
IF lnPWDPo > 0
  lnPWDPo = ASUBSCRIPT(laOGObjType,lnPWDPo,1)
  laOGObjCnt[lnPWDPo] = llStat
  = lfOGShowGet('llRPWhDeta')
ENDIF

lnPLoPo = ASCAN(laOGObjType,'llRPPrnLoc')
IF lnPLoPo > 0
  lnPLoPo = ASUBSCRIPT(laOGObjType,lnPLoPo,1)
  laOGObjCnt[lnPLoPo] = llTrakLoc AND ((llMultiWH AND llRPWhDeta) OR !llMultiWH OR lcRPSortBy = 'W')
  = lfOGShowGet('llRPPrnLoc')
ENDIF  
*MAB HERE [End  ]

*--B801193 (Start)
*laOGObjCnt[lnPLoPo] = llRPWhDeta OR (lcRPSortBy = 'W')
*--B801193 (End)


*!*************************************************************
*! Name      : lfTransArr
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

lcTrns   = gfTempName()
lcStyTmp = gfTempName()
lcTotTmp = gfTempName()
lcDummy  = gfTempName()

IF TYPE('laRPRepSou[1,1]') = 'U' OR EMPTY(laRPRepSou[1,1])
  DIMENSION laRPRepSou[1]
  STORE SPACE(0) TO laRPRepSou
ENDIF

IF TYPE('laRPRepTar[1,1]') = 'U' OR EMPTY(laRPRepTar[1,1])
  DIMENSION laRPRepTar[1]
  STORE SPACE(0) TO laRPRepTar
ENDIF

DIMENSION laAllTrns[15,2]
laAllTrns[01,1] = "WIP"
laAllTrns[02,1] = "Stock"
laAllTrns[03,1] = "Dyelot"
laAllTrns[04,1] = "Plan"
laAllTrns[05,1] = "Unallocated"
laAllTrns[06,1] = "OTS"
laAllTrns[07,1] = "Imm. OTS"
laAllTrns[08,1] = "Orders"
laAllTrns[09,1] = "Work orders"
laAllTrns[10,1] = "Intransit"
laAllTrns[11,1] = "Book"
laAllTrns[12,1] = "Shipped"
laAllTrns[13,1] = "Allocated"
laAllTrns[14,1] = "Return"
laAllTrns[15,1] = "Return auth."

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


*--S For sales value
*--C For cost value 
*--B For both
*--N For none

IF UPPER(lcRPShow) $ 'SCB'
  DIMENSION laRPRepSou[6]
  laRPRepSou[01] = "WIP"
  laRPRepSou[02] = "Stock"
  laRPRepSou[03] = "Plan"
  laRPRepSou[04] = "Unallocated"
  laRPRepSou[05] = "OTS"
  laRPRepSou[06] = "Imm. OTS"
ELSE
  DIMENSION laRPRepSou[14]
  laRPRepSou[01] = "WIP"
  laRPRepSou[02] = "Stock"
  laRPRepSou[03] = "Plan"
  laRPRepSou[04] = "Unallocated"
  laRPRepSou[05] = "OTS"
  laRPRepSou[06] = "Imm. OTS"
  laRPRepSou[07] = "Orders"
  laRPRepSou[08] = "Work orders"
  laRPRepSou[09] = "Intransit"
  laRPRepSou[10] = "Book"
  laRPRepSou[11] = "Shipped"
  laRPRepSou[12] = "Allocated"
  laRPRepSou[13] = "Return"
  laRPRepSou[14] = "Return auth."
ENDIF

*!*************************************************************
*! Name      : lfCrTmp
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyCode'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyDesc'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 60
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cStyMajor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Status'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Scale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Price'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 12
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'GL_Link'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotCost'
laFileStru[lnI,2] = 'N'
*AAN B#603713 [Begin]
*laFileStru[lnI,3] = 9
laFileStru[lnI,3] = 13
*AAN B#603713 [End]
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ave_Cost'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 15
laFileStru[lnI,4] = 7

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WareCode'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WareDesc'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotWIP'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SOH8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotSOH'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

*: B603713,4 AAN Resize the following fields from 8 to 10[Start].
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla1'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla2'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla3'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla4'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla5'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla6'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla7'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pla8'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotPla'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo1'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo2'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo3'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo4'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo5'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo6'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo7'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UAlo8'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotUAlo'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 8
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0
*: B603713,4 AAN Resize the following fields from 8 to 10[End].
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotOTS'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'IOTS8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotIOTS'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WOrd8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotWOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Int8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotInt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotBok'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotShp'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Alo8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotAlo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ret8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotRet'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'RetA8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotRetA'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'HasDye'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 0
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lPrnOTS'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 0
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lPrnIOTS'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 0
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Season'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Division'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'FGroup'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

*C121346,1  TMI [Start] Add a field item_type to sort by, fill it from FABRIC.ITEM_TYPE file
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'ITEM_TYPE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0
*C121346,1  TMI [End  ] 

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SGroup'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Loc'
laFileStru[lnI,2] = 'M'
laFileStru[lnI,3] = 0
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'HasLoc'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0
*: B603713,4 AAN Resize the following fields from 11.2 to 16.2[Start].
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVWIP'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVSOH'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVPLA'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVOTS'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVIOTS'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVBOK'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVSHP'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVRet'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVRetA'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVAlo'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVUAlo'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVInt'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVWOrd'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStkVOrd'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 11
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2
*: B603713,4 AAN Resize the following fields from 11.2 to 16.2[Start].
*B603713,4 AAN Resize field from 14 to 16[start].
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVWIP'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVSOH'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVPLA'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVOTS'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVIOTS'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVBOK'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVSHP'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVRet'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVRetA'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVAlo'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVUAlo'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVInt'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVWOrd'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nSalVOrd'
laFileStru[lnI,2] = 'N'
*laFileStru[lnI,3] = 14
laFileStru[lnI,3] = 18
laFileStru[lnI,4] = 2
*B603713,4 AAN Resize field from 14 to 16[start].

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cEndMaj'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cEndSort'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cEndRep'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NotScale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

DIMENSION laIndx[2,2]
laIndx[1,1] = "WareCode+StyCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
laIndx[1,2] = "WareSort"
laIndx[2,1] = "StyCode+WareCode+Dyelot+NotScale"
laIndx[2,2] = "StySort"
=gfCrtTmp(lcTrns,@laFileStru,@laIndx)
SELECT(lcTrns)
=AFIELDS(laFileStru)
DIMENSION laIndx[1,2]
DO CASE
  CASE lcRPSortBy == 'S'
    laIndx[1,1] = "StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
    laIndx[1,2] = "StySort"
  CASE lcRPSortBy == 'SE'
    laIndx[1,1] = "Season+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
    laIndx[1,2] = "SeaSort"
  CASE lcRPSortBy == 'D'
    laIndx[1,1] = "Division+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
    laIndx[1,2] = "DivSort"
  CASE lcRPSortBy == 'FG'
    laIndx[1,1] = "FGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
    laIndx[1,2] = "FGrpSort"
  *C121346,1  TMI [Start] Sort by item_type if there is primary fabric entered
  CASE lcRPSortBy == 'MT'
    laIndx[1,1] = "ITEM_TYPE+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
    laIndx[1,2] = "MatTypSort"
  *C121346,1  TMI [End  ]     
  CASE lcRPSortBy == 'SG'
    laIndx[1,1] = "SGroup+StyCode+WareCode+Dyelot+cEndMaj+cEndSort+cEndRep+NotScale"
    laIndx[1,2] = "SGrpSort"
ENDCASE
=gfCrtTmp(lcStyTmp,@laFileStru,@laIndx)

DO CASE
  CASE lcRPSortBy == 'W'
    laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+WareCode+cStyMajor+NotScale"
    laIndx[1,2] = "WareSort"
  CASE lcRPSortBy == 'S'
    laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+cStyMajor+NotScale"
    laIndx[1,2] = "StySort"
  CASE lcRPSortBy == 'SE'
    laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Season+cStyMajor+NotScale"
    laIndx[1,2] = "SeaSort"
  CASE lcRPSortBy == 'D'
    laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+Division+cStyMajor+NotScale"
    laIndx[1,2] = "DivSort"
  CASE lcRPSortBy == 'FG'
    laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+FGroup+cStyMajor+NotScale"
    laIndx[1,2] = "FGrpSort"
*C121346,1  TMI [Start] Add sort by item_type
  CASE lcRPSortBy == 'MT'
    laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+ITEM_TYPE+cStyMajor+NotScale"
    laIndx[1,2] = "FGrpSort"
*C121346,1  TMI [End  ] 
  CASE lcRPSortBy == 'SG'
    laIndx[1,1] = "cEndMaj+cEndSort+cEndRep+SGroup+cStyMajor+NotScale"
    laIndx[1,2] = "SGrpSort"
ENDCASE
=gfCrtTmp(lcTotTmp,@laFileStru,@laIndx)

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Item'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Label'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 14
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cRecNo'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 2
laFileStru[lnI,4] = 0

=gfCrtTmp(lcDummy,@laFileStru,"cRecNo",lcDummy)

*!*************************************************************
*! Name      : lfDummFill
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : To fill target array
*!*************************************************************
*! Called from : Option Grid
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

IF lcRPSortBy = 'W'
  IF llRPPrnDye
    lnStkPo = ASCAN(laTarArray,"Stock")
    IF lnStkPo > 0
      DIMENSION laTarArray[ALEN(laTarArray)+1]
      =AINS(laTarArray,lnStkPo+1)
      laTarArray[lnStkPo+1] = "Dyelot"
    ENDIF
  ELSE
    lnDyePo = ASCAN(laTarArray,"Dyelot")
    IF lnDyePo > 0
      = ADEL(laTarArray,lnDyePo)
      DIMENSION laTarArray[ALEN(laTarArray,1)-1]
    ENDIF
  ENDIF
ENDIF

FOR lnI = 1 TO ALEN(laTarArray)
  lnElemNo  = ASUBSCRIPT(laAllTrns,ASCAN(laAllTrns,laTarArray[lnI]),1)
  IF !(UPPER(laAllTrns[lnElemNo,2]) $ "IOTS")
    llOnlyOTS = .F.
  ENDIF

  lcAllTrns = lcAllTrns + "," +laAllTrns[lnElemNo,2]+ ","
  *B803043,1 (Begin) Program does not take "ALO" as it does not get into the condition as
  *B803043,1         ("ALO" $ "UALO,OTS,IOTS,BOK" ) IS .T. so change it.
  *IF !(UPPER(laAllTrns[lnElemNo,2]) $ "UALO,OTS,IOTS,BOK")
  IF !(UPPER(laAllTrns[lnElemNo,2]) $ "OTS,IOTS,BOK") AND UPPER(laAllTrns[lnElemNo,2]) <> "UALO"
  *B803043,1 (End)
    lcTargFlds = IIF(EMPTY(lcTargFlds),lcTargFlds,lcTargFlds+",") + ;
                 laAllTrns[lnElemNo,2]+'1,'+laAllTrns[lnElemNo,2]+'2,'+;
                 laAllTrns[lnElemNo,2]+'3,'+laAllTrns[lnElemNo,2]+'4,'+;
                 laAllTrns[lnElemNo,2]+'5,'+laAllTrns[lnElemNo,2]+'6,'+;
                 laAllTrns[lnElemNo,2]+'7,'+laAllTrns[lnElemNo,2]+'8,'+;
                 'Tot'+laAllTrns[lnElemNo,2]

    = lfSourFlds(UPPER(laAllTrns[lnElemNo,2]))
  ENDIF

  INSERT INTO (lcDummy) (Item                 ,Label          ,cRecNo        ) ;
                 VALUES (laAllTrns[lnElemNo,2],laTarArray[lnI],PADL(lnI,2,'0'))
ENDFOR
INSERT INTO (lcDummy) (Item,Label,cRecNo             ) ;
               VALUES (''  ,''   ,PADL(RECNO(),2,'0'))

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
  *B603675,1 AMH 07/02/2000 Fix the bug of Printing Return Auth. Qty. (Start)
  *CASE lcTrnFld = "RET"
  CASE lcTrnFld == "RET"
  *B603675,1 AMH 07/02/2000 Fix the bug of Printing Return Auth. Qty. (End)
    lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
                  "RET1,RET2,RET3,RET4,RET5,RET6,RET7,RET8,TotRET"
  CASE lcTrnFld = "RETA"
    lcSourFlds = lcSourFlds + IIF(EMPTY(lcSourFlds),"",",") + ;
                  "RA1,RA2,RA3,RA4,RA5,RA6,RA7,RA8,TotRA"
ENDCASE

*!*************************************************************
*! Name      : lfCostSalPr
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : To construct the string that hold the cost & price
*!             label according to print one or both of them
*!*************************************************************
*! Called from : Option Grid
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
  *B604163,1 ABD - Fix bug that we must print the average cost instead 
  *B604163,1 ABD - Of the unit cost in all case without looking to cost method. [Begin]
  *lcReturn = SPACE(15) + 'Unit cost : '

  *B605553,1 BWA 02/19/2002 Change the header of the fields in the FRX due to the method of the cost.[START]
  *lcReturn = SPACE(15) + 'Average cost : '
  *B119818 ,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
  *                          and Link to GL=yes but Generate Gl Cost=No [Start]
  *IF llLinkGlJl
  *B129994,1  MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
  *IF !llGnGlCst AND lcCstMeth = "S"
  IF lcCstMeth = "S"
  *B129994,1 MMR .[End] 
    lcReturn = SPACE(15) + 'Unit cost :    '
  *B119818 ,1 MMM [End] 
  ELSE
    lcReturn = SPACE(15) + 'Average cost : '
  ENDIF
  *B605553,1 BWA 02/19/2002.[END]

  *B604163,1 ABD - [End]
  
  IF lcRPSortBy == 'W'

    *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.[START]
    *lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..Ave_Cost,10,2))
    *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
    *                         and Link to GL=yes but Generate Gl Cost=No [Start]
    *IF llLinkGlJl
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *IF !llGnGlCst AND lcCstMeth = "S" 
    IF lcCstMeth = "S" 
    *B129994,1 MMR.[End]  
    *B119818 ,1 MMM [End]
      lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..TotCost,10,2))
    ELSE
      lcReturn = lcReturn + ALLTRIM(STR(&lcTrns..Ave_Cost,10,2))
    ENDIF

  ELSE
    *B604163,1 ABD - Fix bug that we must print the average cost instead 
    *B604163,1 ABD - Of the unit cost in all case without looking to cost method. [Begin]
    *IF lcCostMth='S'
    *  *AAN B#603713 [Begin]
    *  *lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".TotCost"),9,2))
    *  lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".TotCost"),13,2))
    *  *AAN B#603713 [End]
    *ELSE

    *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.[START]
    *lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".Ave_Cost"),10,2))
    *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
    *                         and Link to GL=yes but Generate Gl Cost=No [Start]
    *IF llLinkGlJl
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *IF !llGnGlCst AND lcCstMeth = "S"
    IF lcCstMeth = "S"
    *B129994,1 MMR.[End]    
    *B119818,1 MMM [End]   
      lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".TotCost"),10,2))
    ELSE
      lcReturn = lcReturn + ALLTRIM(STR(EVAL(lcStyTmp+".Ave_Cost"),10,2))      
    ENDIF
    *B605553,1 BWA 02/19/2002.[END]
    *ENDIF
    *B604163,1 ABD - [End]

  ENDIF

  IF llShowSale
    IF lcRPSortBy == 'W'
      lcReturn = lcReturn + SPACE(15)+'Unit Price  : ' + ALLTRIM(STR(EVAL(lcTrns+".Price"),12,2))
    ELSE
      lcReturn = lcReturn + SPACE(15)+'Unit Price  : ' + ALLTRIM(STR(EVAL(lcStyTmp+".Price"),12,2))
    ENDIF
  ENDIF
ELSE
  IF llShowSale
    IF lcRPSortBy == 'W'
      lcReturn = SPACE(15) + 'Unit Price : ' + ALLTRIM(STR(EVAL(lcTrns+".Price"),12,2))
    ELSE
      lcReturn = SPACE(15) + 'Unit Price : ' + ALLTRIM(STR(EVAL(lcStyTmp+".Price"),12,2))
    ENDIF
  ENDIF
ENDIF

RETURN lcReturn

*!*************************************************************
*! Name      : lfCheckDye
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Check if the system uses dyelot
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCheckDye()
*!*************************************************************

FUNCTION lfCheckDye
*MAB HERE this function is nolonger in use.
RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*!*************************************************************
*! Name      : lfExtScale
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Check if the system uses extended size scale
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfDatCollW
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

PRIVATE lcStyle,lcDyelot,llHasDye,lcScale

llHasDye = .F.

*B038431,1 NNA 08/24/2004 (Begin) Define The [llAdded] before the Scan Statement Because if there
*B038431,1 NNA 			  is not trans. Matches with the Report Criteria this Var. Will not be Found
llAdded = .F. 
*B038431,1 NNA (End)

SELECT STYLE
SET ORDER TO STYLE
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE

SELECT STYDYE
SET ORDER TO STYDYEW DESCENDING
SET RELATION TO Style INTO STYLE ADDITIVE

lcStyle  = SPACE(19)

lcScale = SPACE(3)

SCAN FOR &lcRpExp

  *B605553,4 BWA 03/02/2002 Fix the bug of Variable "laStyScale" not found.[START]
  *IF EMPTY(lcScale)
  IF EMPTY(lcScale) OR TYPE('laStyScale') = "U"
  *B605553,4 BWA 03/02/2002.[END]

    lcScale = Scale.Scale
  ENDIF
  
  llAdded = .T. 

  IF llRPPrnDye
    IF lcStyle <> STYDYE.Style
      llHasDye = !EMPTY(Dyelot)
    ENDIF
    lcStyle  = STYDYE.Style
  ENDIF
  IF !EMPTY(Dyelot)
    LOOP
  ENDIF
  
  DO lpInsInTmp WITH 'STYDYE'
  
  *--B602607 (Start)
  IF !llRPShwZer AND ;
     (&lcTrns..TotWip  + &lcTrns..TotSOH + &lcTrns..TotPLA  + &lcTrns..TotOTS + ;
      &lcTrns..TotIOTS + &lcTrns..TotBOK + &lcTrns..TotSHP  + &lcTrns..TotRet + ;
      &lcTrns..TotRetA + &lcTrns..TotAlo + &lcTrns..TotUAlo + &lcTrns..TotInt + ;
      &lcTrns..TotWOrd + &lcTrns..TotOrd) = 0
    SELECT (lcTrns)
    DELETE
    llAdded = .F.
  ELSE
  *--B602607 (End)
    IF llOnlyOTS AND !&lcTrns..lPrnOTS AND !&lcTrns..lPrnIOTS
      SELECT (lcTrns)
      DELETE
      llAdded = .F.
    ELSE
      *------------------------------------------------------*
      SELECT (lcTrns)
      SCATTER FIELDS cStyMajor,StyCode,WareCode,Dyelot,Scale,Price,TotCost,Ave_Cost MEMVAR

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
      ENDIF
      = lfUpdTotals("Rep",lcTrns)
      *------------------------------------------------------*
    ENDIF
    IF llRPPrnLoc
      IF SEEK(STYDYE.Style+SPACE(6)+STYDYE.cWareCode,'WHSLOC')
        SELECT WHSLOC
        lcLoc = ''
        SCAN REST WHILE Style+Color+cWareCode = ;
                        STYDYE.Style+SPACE(6)+STYDYE.cWareCode ;
                        FOR !EMPTY(cLocation)
          llHasLoc = .T.
        
          lcLoc =  lcLoc + IIF(EMPTY(lcLoc),'Bins                 ',SPACE(2)) + WHSLOC.cLocation
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
    SELECT (lcTrns)
    IF lcScale = Scale.Scale
      SCATTER TO laStyScale
    ELSE
      APPEND BLANK
      GATHER FROM laStyScale
      REPLACE NotScale WITH "N"
      lcScale = Scale.Scale
    ENDIF
  ENDIF

ENDSCAN

IF llAdded
  SELECT (lcTrns)
  APPEND BLANK
  GATHER FROM laStyScale
  REPLACE NotScale WITH "N" 
ENDIF

SELECT (lcTrns)
APPEND FROM (gcWorkDir+lcTotTmp)

SELECT STYLE
SET ORDER TO
SET RELATION TO

SELECT STYDYE
SET ORDER TO
SET RELATION TO

DO lpWareRela

*!*************************************************************
*! Name      : lfDatCollS
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

PRIVATE lcStyle,llHasDye,lcWare,lnI,lcPrvMaj,lcScale

DO CASE
  CASE lcRPSortBy == 'S'
    lcSortKey = "''"
  CASE lcRPSortBy == 'SE'
    lcSortKey = lcStyTmp+'.Season'
  CASE lcRPSortBy == 'D'
    lcSortKey = lcStyTmp+'.Division'
  CASE lcRPSortBy == 'FG'
    lcSortKey = lcStyTmp+'.FGroup'
  *C121346,1  TMI [Start] Sort by item_type
  CASE lcRPSortBy == 'MT'
    lcSortKey = lcStyTmp+'.ITEM_TYPE'
  *C121346,1  TMI [End  ] 
  CASE lcRPSortBy == 'SG'
    lcSortKey = lcStyTmp+'.SGroup'
ENDCASE

llHasDye = .F.

lcStyleWare = SPACE(25)

SELECT STYDYE
SET ORDER TO STYDYE DESCENDING

SELECT STYLE
SET ORDER TO STYLE
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE
SET RELATION TO Style INTO STYDYE ADDITIVE
*C121346,1  TMI [Start] Add a relation to fabric file
SET RELATION TO FABRIC INTO FABRIC ADDITIVE
*C121346,1  TMI [End  ] 

IF llRPWhDeta
  = ACOPY(laOGHdFlt,laMyHeddin)
  = ACOPY(laOGFxFlt,laMyFixed)
  = ACOPY(laOGVrFlt,laMyVaria)

  lnI = 0
  FOR lnI = 1 TO ALEN(laOGHdFlt,1)
    IF !('STYLE.' $ laOGHdFlt[lnI,1])
      laOGHdFlt[lnI,6] = SPACE(0)
    ENDIF
  ENDFOR

  lnI = 0
  FOR lnI = 1 TO ALEN(laOGFxFlt,1)
    IF !('STYLE.' $ laOGFxFlt[lnI,1])
      laOGFxFlt[lnI,6] = SPACE(0)
    ENDIF
  ENDFOR

  lnI = 0
  FOR lnI = 1 TO ALEN(laOGVrFlt,1)
    IF !('STYLE.' $ laOGVrFlt[lnI,1])
      laOGVrFlt[lnI,6] = SPACE(0)
    ENDIF
  ENDFOR

  lcHdnExp = gfGenFlt('laOGHdFlt',.T.)
  lcFxdExp = gfGenFlt('laOGFxFlt',.T.)
  lcVarExp = gfGenFlt('laOGVrFlt',.T.)

  lcMyRepExp = IIF(!EMPTY(lcHdnExp),"(" + lcHdnExp + ")","")
  lcMyRepExp = lcMyRepExp + IIF(!EMPTY(lcFxdExp)," AND (" + lcFxdExp + ")","")
  lcMyRepExp = lcMyRepExp + IIF(!EMPTY(lcVarExp)," AND (" + lcVarExp + ")","")

  lcMyRepExp = IIF(EMPTY(lcMyRepExp),"''",lcMyRepExp)

  = ACOPY(laMyHeddin,laOGHdFlt)
  = ACOPY(laMyFixed,laMyHeddin)
  = ACOPY(laMyVaria,laOGVrFlt)

ELSE
  lcMyRepExp = lcRpExp  
ENDIF

lcPrvMaj = SPACE(19)

lnLocNo = 0
SELECT STYLE

lcScale = SPACE(3)

SCAN FOR &lcMyRepExp
  
  llAdded = .T.
    
  DO lpInsInTmp WITH 'STYLE'
  
  *--B602607 (Start)
  IF !llRPShwZer AND ;
     (&lcStyTmp..TotWip  + &lcStyTmp..TotSOH + &lcStyTmp..TotPLA  + &lcStyTmp..TotOTS +;
      &lcStyTmp..TotIOTS + &lcStyTmp..TotBOK + &lcStyTmp..TotSHP  + &lcStyTmp..TotRet +;
      &lcStyTmp..TotRetA + &lcStyTmp..TotAlo + &lcStyTmp..TotUAlo + &lcStyTmp..TotInt +;
      &lcStyTmp..TotWOrd + &lcStyTmp..TotOrd) = 0
    SELECT (lcStyTmp)
    DELETE
    llAdded = .F.
  ELSE
  *--B602607 (End)
    IF llOnlyOTS AND !&lcStyTmp..lPrnOTS AND !&lcStyTmp..lPrnIOTS
      SELECT (lcStyTmp)
      DELETE
      llAdded = .F.
    ELSE
      *------------------------------------------------------*
      SELECT (lcStyTmp)
      *C121346,1  TMI [Start] Add item_type field
      *SCATTER FIELDS cStyMajor,StyCode,WareCode,Dyelot,Season,Division,FGroup,SGroup,Scale,Price,TotCost,Ave_Cost MEMVAR
      SCATTER FIELDS cStyMajor,StyCode,WareCode,Dyelot,Season,Division,FGroup,SGroup,Scale,Price,TotCost,Ave_Cost,Item_type MEMVAR
      *C121346,1  TMI [End  ] 
      SELECT (lcTotTmp)
      IF !SEEK('A  '+&lcSortKey+&lcStyTmp..cStyMajor)
        APPEND BLANK
      ENDIF
      GATHER MEMVAR
      REPLACE cEndMaj WITH "A",NotScale WITH 'Y'
      = lfUpdTotals("Maj",lcStyTmp)

      IF !(lcRPSortBy == 'S')
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
      IF llRPWhDeta OR llRPPrnDye
        SELECT STYDYE
        SCAN WHILE Style = STYLE.Style FOR &lcRpExp
          llNew       = lcStyleWare # STYDYE.Style+STYDYE.cWareCode
          lcStyleWare = STYDYE.Style+STYDYE.cWareCode
          lnLocNo     = IIF(llNew, 0, lnLocNo)
          IF llRPPrnDye AND llNew
            llHasDye = !EMPTY(Dyelot)
          ENDIF
          IF !EMPTY(Dyelot)
            LOOP
          ENDIF

          DO lpInsInTmp WITH 'STYDYE'
          IF llOnlyOTS AND !&lcTrns..lPrnOTS AND !&lcTrns..lPrnIOTS
            SELECT (lcStyTmp)
          ELSE
            IF llRPWhDeta AND llRPPrnLoc
              IF SEEK(STYDYE.Style+SPACE(6)+STYDYE.cWareCode,'WHSLOC')
                SELECT WHSLOC
                lcLoc = ''
                SCAN REST WHILE Style+Color+cWareCode = ;
                                STYDYE.Style+SPACE(6)+STYDYE.cWareCode ;
                          FOR !EMPTY(cLocation)
                  llHasLoc = .T.
            
                  lcLoc =  lcLoc + IIF(EMPTY(lcLoc),'Bins                 ',SPACE(2)) + WHSLOC.cLocation
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
      ENDIF    
    ENDIF
  ENDIF

  IF llAdded
    IF lcScale <> Scale.Scale
      DO lpInsInTmp WITH 'STYLE',.T.
      lcScale = Scale.Scale
    ENDIF
  ENDIF

ENDSCAN

SELECT (lcStyTmp)
APPEND FROM (gcWorkDir+lcTotTmp)

SELECT STYLE
SET ORDER TO
SET RELATION TO

SELECT STYDYE
SET ORDER TO
SET RELATION TO

DO lpStyleRela

*!*************************************************************
*! Name      : lpInsInTmp
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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

lcFileToUse = IIF(lcFromFile='STYDYE',(lcTrns),(lcStyTmp))

SELECT(lcFileToUse)
APPEND BLANK
REPLACE StyCode   WITH STYLE.Style,;
        StyDesc   WITH STYLE.Desc1,;
        cStyMajor WITH STYLE.cStyMajor,;
        Status    WITH STYLE.Status,;
        Scale     WITH STYLE.Scale,;
        Price     WITH STYLE.PriceA,;               
        TOTCOST   WITH STYLE.TOTCOST,;
        Season    WITH STYLE.Season,;
        Division  WITH STYLE.cDivision,;
        FGroup    WITH STYLE.Fabric,;
        SGroup    WITH STYLE.cStyGroup
*C121346,1  TMI [Start] Update Item_type field
REPLACE ITEM_TYPE WITH ALLTRIM(gfCodDes(FABRIC.ITEM_TYPE,'ITEM_TYPE'))
*C121346,1  TMI [End  ] 
IF lcFromFile = 'STYDYE'
  REPLACE WareCode  WITH STYDYE.cWareCode,;
          Dyelot    WITH STYDYE.Dyelot,;
          WareDesc  WITH STYDYE.Desc,;
          HasDye    WITH llHasDye
ENDIF
        
REPLACE AVE_COST  WITH &lcFromFile..AVE_COST,;
        GL_LINK   WITH IIF(lcFromFile='STYLE',STYLE.Link_Code,STYDYE.GL_LINK)

IF llScale
  REPLACE NotScale WITH 'N'
ELSE  
  REPLACE NotScale WITH 'Y'
  
  IF !EMPTY(lcSourFlds)
    SELECT (lcFromFile)
    SCATTER FIELDS &lcSourFlds. TO laAllVal
    SELECT (lcFileToUse)
    GATHER FIELDS &lcTargFlds. FROM laAllVal
  ENDIF
  
  IF ",UALO," $ UPPER(lcAllTrns)
    = lfUAloCalc()
    SELECT (lcFileToUse)
    GATHER FIELDS &lcUALOFlds. FROM laUAloVal
  ENDIF
  IF ",IOTS," $ UPPER(lcAllTrns)
    STORE 0 To laIOTSVal
    =lfOTSCalc('IOTS')
    SELECT (lcFileToUse)
    GATHER FIELDS &lcIOTSFlds. FROM laIOTSVal
  ENDIF
  IF ",OTS," $ UPPER(lcAllTrns)
    STORE 0 To laOTSVal 
    =lfOTSCalc('OTS')
    SELECT (lcFileToUse)
    GATHER FIELDS &lcOTSFlds. FROM laOTSVal
  ENDIF
  IF ",BOK," $ UPPER(lcAllTrns)
    =lfBokCalc()
    SELECT (lcFileToUse)
    GATHER FIELDS &lcBokFlds. FROM laBokVal
  ENDIF

  SELECT (lcFileToUse)

  IF llShowCost
  
    IF lcRPSortBy = 'W'
      lcCostFld = 'Ave_Cost'
    ELSE
      IF lcFromFile = 'STYDYE'
        lcCostFld = 'Ave_Cost'
      ELSE
        *B604163,1 ABD - Fix bug that we must print the average cost instead 
        *B604163,1 ABD - Of the unit cost in all case without looking to cost method. [Begin]
        *IF lcCostMth='S'
        *  lcCostFld = 'TotCost'
        *ELSE

        *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.[START]
        *lcCostFld = 'Ave_Cost'
        *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
        *                         and Link to GL=yes but Generate Gl Cost=No [Start]
        *IF llLinkGlJl
        *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
        *IF !llGnGlCst AND lcCstMeth = "S"
        IF lcCstMeth = "S"
        *B129994,1 MMR.[End]  
        *B119818,1 MMM [End] 
          lcCostFld = 'TotCost'
        ELSE
          lcCostFld = 'Ave_Cost'
        ENDIF
        *B605553,1 BWA 02/19/2002.[END]
        *ENDIF
        *B604163,1 ABD - [End]
      ENDIF
    ENDIF

    laStkVal[01] = TotWip * &lcCostFld.

    *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard 
    *B605553,1 BWA 02/19/2002 and this change in case sorting by anything else style and location.[START]
    *laStkVal[02] = &lcFromFile..nStkVal
    *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
    *                         and Link to GL=yes but Generate Gl Cost=No [Start]
    *IF llLinkGlJl
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *IF llGnGlCst
    *B119818,1 MMM [End]   
      *laStkVal[02] = &lcFromFile..nStkVal
    *ELSE
    *B129994,1 MMR.[End]
      *B119818,1 MMM 10/20/2003 Fix Bug of Printing wrong cost value based on any selection except
      *                         (By style and by location) with selection Print Location detail(YES) [Start] 
      *laStkVal[02] = IIF(lcCstMeth = "S" , Style.TotCost * Style.ToTSTK , &lcFromFile..nStkVal)
      laStkVal[02] = IIF(lcCstMeth = "S" , Style.TotCost * &lcFromFile..Totstk , &lcFromFile..nStkVal) 
      *B119818,1 MMM [End]  
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *ENDIF
    *B129994,1 MMR.[End]
    *B605553,1 BWA 02/19/2002.[END]

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
    GATHER FIELDS &lcStkFlds. From laStkVal
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
    GATHER FIELDS &lcSalFlds. From laSalVal
  ENDIF  
ENDIF
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfOTSCalc
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
PRIVATE lnI,lcArray,lcArrayEle,lcSz,lcArTotEle
lcArray = "la" + lcItm + "Val"
lcArTotEle  = lcArray + "[9]"

STORE 0 TO &lcArray

FOR lnI = 1 TO 8
  lcSz = ALLTRIM(STR(lnI))
  lcArrayEle  = lcArray + "[" + lcSz + "]"

  *B604876,1 BWA 04/18/2002 Select the plan field from the style file .[START]
  *&lcArrayEle = EVAL(lcFromFile+'.STK'+lcSz)+;
                IIF(lcItm='IOTS',0,IIF(lcRPOTSB='W',;
                                       EVAL(lcFromFile+'.WIP'+lcSz),;
                                       EVAL(lcFromFile+'.Plan'+lcSz)))-;
                                       EVAL(lcFromFile+'.Ord'+lcSz)
  &lcArrayEle = EVAL(lcFromFile+'.STK'+lcSz)+;
                IIF(lcItm='IOTS',0,IIF(lcRPOTSB='W',;
                    EVAL(lcFromFile+'.WIP'+lcSz),;
                    EVAL('STYLE.Plan'+lcSz)))-;
                    EVAL(lcFromFile+'.Ord'+lcSz)

  *B604876,1 BWA 04/18/2002.[END]
  DO CASE 
    CASE lcRPOTSSig = 'P' AND &lcArrayEle <= 0
      &lcArrayEle = 0
    CASE lcRPOTSSig = 'N' AND &lcArrayEle >= 0
      &lcArrayEle = 0
  ENDCASE
  &lcArTotEle = &lcArTotEle + &lcArrayEle
ENDFOR
*B121770,1 NNA 03/01/2004 (Begin) ask about the (Net) option too to print it
*IF (lcRPOTSSig ='P' AND &lcArTotEle >= lnRPOTSMin) OR ;
   (lcRPOTSSig ='N' AND &lcArTotEle <= lnRPOTSMin)
IF (lcRPOTSSig ='P' AND &lcArTotEle >= lnRPOTSMin) OR ;
   (lcRPOTSSig ='N' AND &lcArTotEle <= lnRPOTSMin) OR ;
   (lcRPOTSSig ='A')   
*B121770,1 NNA (End)
  DO CASE
    CASE lcItm = 'OTS'
      REPLACE lPrnOTS  WITH .T.
    CASE lcItm = 'IOTS'
      REPLACE lPrnIOTS WITH .T.
  ENDCASE
  *B121770,1 NNA 03/01/2004 (Begin) Put the Total Of (OTS Or IOTS) if we Chose The Net Option to get 
  *B121770,1 NNA                    the Positive and negative amount
*ELSE
  *&lcArTotEle = 0  
  *B121770,1 NNA (End)
ENDIF

*!*************************************************************

FUNCTION lfUAloCalc

PRIVATE lcSz,lcArrayEle

STORE 0 TO laUAloVal

FOR lnI = 1 TO 8
  lcSz = ALLTRIM(STR(lnI))
  lcArrayEle  = "laUaloVal" + "[" + lcSz + "]"
  &lcArrayEle = EVAL(lcFromFile+".STK"+lcSz)-EVAL(lcFromFile+'.ALO'+lcSz)
  laUAloVal[9] = laUAloVal[9] + &lcArrayEle
ENDFOR

*!*************************************************************

FUNCTION lfBokCalc
PRIVATE lcSz,lcArrayEle

STORE 0 TO laBokVal

FOR lnI = 1 TO 8
  lcSz = ALLTRIM(STR(lnI))
  lcArrayEle  = "laBokVal" + "[" + lcSz + "]"
  &lcArrayEle = EVAL(lcFromFile+".Shp"+lcSz)+EVAL(lcFromFile+'.Ord'+lcSz)
  laBokVal[9] = laBokVal[9] + &lcArrayEle
ENDFOR

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
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Create relation between the dummy file and data
*!             temp. file when it is sorted by location
*!*************************************************************
*! Called from : Option Grid
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
SELECT STYDYE
SET ORDER TO STYDYEW ASCENDING
SELECT (lcDummy)
SET RELATION TO IIF(&lcDummy..Label='Dyelot' AND &lcTrns..NotScale='Y',;
                    &lcTrns..WareCode+&lcTrns..StyCode ,;
                    SPACE(25)) INTO STYDYE ADDITIVE
SELECT(lcTrns)
SET SKIP TO (lcDummy),STYDYE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO WareCode INTO WAREHOUS ADDITIVE
SET ORDER TO STYLE IN STYLE
SET RELATION TO StyCode INTO STYLE ADDITIVE

*!*************************************************************
*! Name      : lpStyleRela
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
  CASE lcRPSortBy == 'S'
    lcSortTtl  = 'Style'
    lcGroupExp = "''"
    lcSortExp  = "''"
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(MAX(LEN('Description'),LEN(ALLTRIM(lcRepNMTtl)))-LEN(lcMajTtl),0))]
    lcNMajExp  = [ALLTRIM(lcRepNMTtl) + SPACE(1) + PADL(": ",2+MAX(MAX(LEN('Description'),LEN(lcMajTtl))-LEN(ALLTRIM(lcRepNMTtl)),0))]
    lcDescExp  = ['Description'      + SPACE(1) + PADL(': ',2+MAX(MAX(LEN(lcMajTtl),LEN(ALLTRIM(lcRepNMTtl)))-LEN('Description'),0))]
  CASE lcRPSortBy == 'SE'
    lcSortFld  = '.Season'
    lcSortTtl  = "Season"
    lcGroupExp = lcStyTmp+'.Season'
    lcSortExp  = ['Season'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Season'),0))]
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Season')-LEN(lcMajTtl),0))]
    lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0)) ]
    lcDescExp  = ['Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))]
  CASE lcRPSortBy == 'D'
    lcSortFld  = '.Division'
    lcSortTtl  = 'Division'
    lcGroupExp = lcStyTmp+'.Division'
    lcSortExp  = ['Division'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Division'),0))]
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Division')-LEN(lcMajTtl),0))]
    lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
    lcDescExp  = ['Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))]
  CASE lcRPSortBy == 'FG'
    lcSortFld  = '.FGroup'
*B037622,1 NNA 02/03/2003 (Begin) Change (Fabric Group) With (Primary Fabric)
    *lcSortTtl  = 'Fabric Group'
    *lcGroupExp = lcStyTmp+'.FGroup'    
    *lcSortExp  = ['Fabric Group'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Fabric Group'),0))]
    *lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Fabric Group')-LEN(lcMajTtl),0))]
    lcSortTtl  = 'Primary Fabric'    
    lcGroupExp = lcStyTmp+'.FGroup'
    lcSortExp  = ['Primary Fabric'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Primary Fabric'),0))]
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Primary Fabric')-LEN(lcMajTtl),0))]
*B037622,1 NNA (End)
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Primary Fabric')-LEN(lcMajTtl),0))]
    lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
    lcDescExp  = ['Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))]
*C121346,1  TMI [Start] Add the sort by item type
  CASE lcRPSortBy == 'MT'
    lcSortFld  = '.ITEM_TYPE'
    lcSortTtl  = 'Material Type'
    lcGroupExp = lcStyTmp+'.ITEM_TYPE'
    lcSortExp  = ['Material Type'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Material Type'),0))]
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Material Type')-LEN(lcMajTtl),0))]
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Material Type')-LEN(lcMajTtl),0))]
    lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
    lcDescExp  = ['Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))]
*C121346,1  TMI [End  ] 
  CASE lcRPSortBy == 'SG'
    lcSortFld  = '.SGroup'
    lcSortTtl  = 'Style Group'
    lcGroupExp = lcStyTmp+'.SGroup'
    lcSortExp  = ['Style Group'+SPACE(1)+PADL(": ",2+MAX(LEN(lcMajTtl)-LEN('Style Group'),0))]
    lcMajExp   = [lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Style Group')-LEN(lcMajTtl),0))]
    lcNMajExp  = [ALLTRIM(lcRepNMTtl)+SPACE(1)+ PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0))]
    lcDescExp  = ['Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))]
ENDCASE

SET ORDER TO (lcDummy) IN (lcDummy)
SET ORDER TO STYDYE ASCENDING IN STYDYE

SELECT(lcStyTmp)
SET RELATION TO IIF(NotScale='Y','',SPACE(10)) INTO (lcDummy) ADDITIVE
SELECT (lcDummy)
SET RELATION TO IIF(&lcStyTmp..NotScale='Y',&lcStyTmp..StyCode,SPACE(10)) INTO (lcTrns) ADDITIVE
SELECT (lcTrns)
SET RELATION TO IIF(&lcStyTmp..NotScale='Y',StyCode+WareCode,SPACE(10)) INTO STYDYE ADDITIVE
SELECT (lcStyTmp)
SET SKIP TO (lcDummy),(lcTrns),STYDYE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET ORDER TO STYLE IN STYLE
SET RELATION TO StyCode INTO STYLE ADDITIVE

*!*************************************************************
*! Name      : lfLastItem
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
  lcLastItem = &lcDummy..Label
  lcLastSty  = StyCode
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfLastSty
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
*! Name      : lfLstToSty
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
*! Example     : =lfLstToSty()
*!*************************************************************

FUNCTION lfLstToItm

IF NotScale = 'Y'
  lcLstToItm = &lcDummy..Item
ENDIF

RETURN ''

*!*************************************************************
*! Name      : lfPrnStyItm
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
lcUseFile = IIF(lcRPSortBy = "W",lcTrns,lcStyTmp)
lnRet = ''
IF NotScale = 'Y'
IF Scale.Cnt >= VAL(lcFld) OR !EMPTY(&lcUseFile..cEndMaj+&lcUseFile..cEndSort+&lcUseFile..cEndRep)
  IF !EMPTY(&lcDummy..Item)
    IF (lcRPSortBy = "W" AND &lcDummy..Label <> 'Dyelot' AND EMPTY(&lcUseFile..Dyelot)) OR ;
        lcRPSortBy <> "W"
      IF lcFld = 'ItmLabel'
        IF !(ALLTRIM(&lcDummy..Item) $ 'IOTS')
          lnRet = &lcDummy..Label
        ELSE
          IF (ALLTRIM(&lcDummy..Item) = 'IOTS' AND lPrnIOTS) OR ;
             (ALLTRIM(&lcDummy..Item) = 'OTS' AND lPrnOTS)   OR ;
             !EMPTY(&lcUseFile..cEndMaj)
            lnRet = &lcDummy..Label
          ELSE
            lnRet = ''
          ENDIF
        ENDIF
      ELSE
        IF lcFld = 'WhsLabel'
          IF llRPWhDeta AND EMPTY(cEndMaj)
            IF !(ALLTRIM(&lcDummy..Item) $ 'IOTS')
              lnRet = 'Location '
            ELSE
              IF (ALLTRIM(&lcDummy..Item) = 'IOTS' AND lPrnIOTS) OR ;
                 (ALLTRIM(&lcDummy..Item) = 'OTS' AND lPrnOTS)   OR ;
                 !EMPTY(&lcUseFile..cEndMaj)
                lnRet = 'Location '
              ELSE
                lnRet = ''
              ENDIF
            ENDIF
          ELSE
            lnRet = ''
          ENDIF
        ELSE
          IF !INLIST(lcFld,'Price','Cost') 
            lnRet = EVAL(lcUseFile+"."+IIF(lcFld='Tot','Tot'+ALLTRIM(&lcDummy..Item),;
                                                       ALLTRIM(&lcDummy..Item)+lcFld))
          
            IF ALLTRIM(&lcDummy..Item) $ 'IOTS' AND EMPTY(cEndMaj)
              IF !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND lPrnIOTS) OR ;
                    (ALLTRIM(&lcDummy..Item) = 'OTS' AND lPrnOTS) )
               
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
                 !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !lPrnIOTS) OR ;
                    (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !lPrnOTS ) )
                 lnRet = EVAL('nStkV'+ALLTRIM(&lcDummy..Item))
              ELSE
                lnRet = ''
              ENDIF
            ENDIF
            IF lcFld = 'Price'
              IF !EMPTY(cEndMaj) OR ;
                 !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !lPrnIOTS) OR ;
                    (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !lPrnOTS ) )

                lnRet = EVAL('nSalV'+ALLTRIM(&lcDummy..Item))
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
*! Name      : lfPrnWhsItm
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
IF llRPWhDeta AND EMPTY(STYDYE.Dyelot) AND EMPTY(&lcStyTmp..cEndMaj) AND !EMPTY(&lcDummy..Item)
  IF lcFld = 'WhsCode'
    IF !(ALLTRIM(&lcDummy..Item) $ 'IOTS')
      lnRet = &lcTrns..WareCode
    ELSE
      IF (ALLTRIM(&lcDummy..Item) = 'IOTS' AND &lcTrns..lPrnIOTS) OR ;
         (ALLTRIM(&lcDummy..Item) = 'OTS'  AND &lcTrns..lPrnOTS) 
        lnRet = &lcTrns..WareCode
      ELSE
        lnRet = ''
      ENDIF
    ENDIF
  ELSE
    IF !INLIST(lcFld,'Price','Cost') 
      lnRet = EVAL(lcTrns+'.'+IIF(lcFld='Tot','Tot'+ALLTRIM(&lcDummy..Item),;
                                              ALLTRIM(&lcDummy..Item)+lcFld))
          
      IF ALLTRIM(&lcDummy..Item) $ 'IOTS'
        IF !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND &lcTrns..lPrnIOTS) OR ;
              (ALLTRIM(&lcDummy..Item) = 'OTS'  AND &lcTrns..lPrnOTS) )
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
           !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !&lcTrns..lPrnIOTS) OR ;
              (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !&lcTrns..lPrnOTS ) )
          lnRet = EVAL(lcTrns+'.nStkV'+ALLTRIM(&lcDummy..Item))
        ELSE
          lnRet = ''
        ENDIF
      ENDIF
      IF lcFld = 'Price'
        IF !EMPTY(&lcStyTmp..cEndMaj) OR ;
           !( (ALLTRIM(&lcDummy..Item) = 'IOTS' AND !&lcTrns..lPrnIOTS) OR ;
              (ALLTRIM(&lcDummy..Item) = 'OTS'  AND !&lcTrns..lPrnOTS ) )
          lnRet = EVAL(lcTrns+'.nSalV'+ALLTRIM(&lcDummy..Item))
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

*!*************************************************************
*! Name      : lfDyltItm
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
    lnRetVal = StyDye.TotStk
  CASE lcFld = 'nStkVal'
    lnRetVal = StyDye.nStkVal
  CASE lcFld = 'Price'
    lnRetVal = StyDye.TotStk*Style.PriceA
  OTHERWISE
    lnRetVal = EVAL('StyDye.Stk'+lcFld)
ENDCASE
  
RETURN lnRetVal
  
*!*************************************************************
*! Name      : lfPrnLoc
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
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
IF (llRPWhDeta OR lcRPSortBy = 'W') AND llRPPrnLoc AND ;
   EVAL(lcDummy+'.Label')<>'Dyelot' AND EMPTY(STYDYE.Dyelot)
  IF ALLTRIM(&lcDummy..Item) $ 'IOTS'
    DO CASE
      CASE lcRPOTSSig ='P'
        lcRet = IIF(!llRPPrnLoc,'',;
                    IIF(ALLTRIM(&lcDummy..Item) $ 'IOTS',;
                        IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..Item))>=lnRPOTSMin,&lcTrns..Loc,'');
                            ,&lcTrns..Loc))
      CASE lcRPOTSSig ='N'
        lcRet = IIF(!llRPWhDeta,'',;
                    IIF(ALLTRIM(&lcDummy..Item) $ 'IOTS',;
                        IIF(EVAL(lcStyTmp+'.Tot'+ALLTRIM(&lcDummy..Item))<=lnRPOTSMin,&lcTrns..Loc,'');
                            ,&lcTrns..Loc))
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

*******************************

FUNCTION lfScale

*B802905,1 BWA 03/01/2000 Fix the bug of not printing the size scale and modify a new option[START]
*IF NotScale = 'N'

*B604876,1 BWA 04/18/2002 Fix the bug of not printing the header in the Location sort.[START]
*IF NotScale = 'N' AND &lcStyTmp..SCALE #  lcScale1
IF NotScale = 'N' AND IIF(lcRPSortBy = 'W' ,  &lcTrns..SCALE #  lcScale1 , &lcStyTmp..SCALE #  lcScale1 )
*B604876,1 BWA 04/18/2002.[END]

  *B802905,1 BWA [END]
  *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
  *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
  IF llTextMode
  *E301481,1 ABD - [End]
  
    lcRet = SPACE(15) + ;
            PADL(ALLTRIM(Scale.Sz1),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz2),8) + SPACE(1) +;
            PADL(ALLTRIM(Scale.Sz3),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz4),8) + SPACE(1) +;
            PADL(ALLTRIM(Scale.Sz5),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz6),8) + SPACE(1) +;
            PADL(ALLTRIM(Scale.Sz7),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz8),8) + SPACE(1) +;
            PADL("Total",8)
  
    IF llShowCost
      lcRet = lcRet + SPACE(1) + " Cost Val."
    ENDIF
    IF llShowSale
      lcRet = lcRet + SPACE(1) + "Sales Val."
    ENDIF
    *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
    *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
  ELSE
    *-- we will let it old the scale.
    lcRet = SPACE(15) + ;
            PADL(ALLTRIM(Scale.Sz1),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz2),8) + SPACE(1) +;
            PADL(ALLTRIM(Scale.Sz3),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz4),8) + SPACE(1) +;
            PADL(ALLTRIM(Scale.Sz5),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz6),8) + SPACE(1) +;
            PADL(ALLTRIM(Scale.Sz7),8) + SPACE(1) + PADL(ALLTRIM(Scale.Sz8),8) + SPACE(1) +;
            PADL("Total",8)
    FOR I = 1 To 8
      Z = STR(I,1)
      laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
    ENDFOR
  
    IF llShowCost
      lcRet = lcRet + SPACE(1) + " Cost Val."
    ENDIF
    IF llShowSale
      lcRet = lcRet + SPACE(10) + "Sales Val."
    ENDIF
  ENDIF
  *E301481,1 ABD - [End]

  
  *B802905,1 BWA 03/01/2000 Fix the bug of not printing the size scale and modify a new option[START]
  lcScale1 = STYLE.SCALE
  *B802905,1 BWA [END]
  
ELSE
  lcRet = ''
ENDIF

RETURN lcRet  

*******************

FUNCTION lfTotTtl

PRIVATE lcRet

IF lcRPSortBy = 'W'
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
          lcRet = lcMajTtl+' Total : '
        ELSE
          IF EMPTY(cEndRep)
            lcRet = 'Warehouse Total : '
          ELSE
            lcRet = 'Grand Total : '
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
          lcRet = lcMajTtl+' Total : '
        ELSE
          IF EMPTY(cEndRep)
            lcRet = lcSortTtl + ' Total : '
          ELSE
            lcRet = 'Grand Total : '
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

RETURN lcRet


*!*************************************************************
*! Name      : lfSWOneTrn
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Proccess function when User select one transaction and sort by Style or Location.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSWOneTrn()
*!*************************************************************
*B602961,1
FUNCTION lfSWOneTrn
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
*-- lnNonPrice : PriceA * lnSize9
*-- lnNonCost  : Ave_Cost * lnSize9

*** Master is StyDye and Sort by style then print non major from style file ***
*-- lnClrSz1 TO ---> lnClrSz8  : Variables Hold Size quantities.
*-- lnClrSz9    : Variable Hold total for all sizes.
*-- lnClrPrice : PriceA * lnSize9
*-- lnClrCost  : Ave_Cost * lnSize9

*** Location Group ***
*-- lnLocSz1 TO ---> lnLocSz8  : Variables Hold Size quantities.
*-- lnLocSz9    : Variable Hold total for all sizes.
*-- lnNonPrcLc  : Total Price.
*-- lnNonCstLc  : Total Cost.

*** Style Group ***
*-- lnStySz1 TO ---> lnStySz8  : Variables Hold Size quantities.
*-- lnStySz9    : Variable Hold total for all sizes.
*-- lnNonPrcOp  : Total Price.
*-- lnNonCstOp  : Total Cost.

*** Summary Band ***
*-- lnGrdSz1 TO ---> lnGrdSz8  : Variables Hold Size quantities.
*-- lnGrdSz9    : Variable Hold total for all sizes.
*-- lnNonPrcGd  : Total Price.
*-- lnNonCstGd  : Total Cost.
*-- Report Numeric and calculated values [End  ]

*B603294,1 KHM 05/15/2000 (Begin) Initializing this variable to be used in
*B603294,1                checking the changing of the scale
lcScalCode = ' '
*B603294,1 KHM 05/15/2000 (End)

DIMENSION laTranAray[8],laTrnNonAr[8]
STORE ''   TO lcMastFile , lcChldFile , lcCost_Val , laTranAray ,;
              lcRepTarVl , lcPrnTitle , lcRevFile , lcCst_VlPn  ,;
              lcCst_VlOp , lcCst_VlGd , lcNonMjDes, lcExtndFlt  ,;
              laTrnNonAr , lcCst_VlCl , lcLocBins , lcPrintSty,lcCst_VlLc
*B804338,1 TMI [Start] Define a variable to hold Totals
STORE '' TO lcCst_VlC1,lcCst_VlC2,lcCst_VlP1,lcCst_VlP2,lcCst_VlO1,lcCst_VlO2,lcCst_VlG1,lcCst_VlG2
*B804338,1 TMI [End  ]
              

STORE [""] TO lcStyGroup , lcLocGroup , lcDyeGroup

STORE 0 TO lnEndOfSty , lnEndOfLoc , lnEndOfDye ,;
           lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
           lnSize6,lnSize7,lnSize8,lnSize9,lnMaxCnt,;
           lnNonPrice,lnNonCost

STORE 0 TO lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
           lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,;
           lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
           lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,;
           lnNonPrcOp,lnNonCstOp,lnNonPrcGd,lnNonCstGd

STORE 0 TO lnLocSz1,lnLocSz2,lnLocSz3,lnLocSz4,;
           lnLocSz5,lnLocSz6,lnLocSz7,lnLocSz8,lnLocSz9,;
           lnNonPrcLc,lnNonCstLc

STORE 0 TO lnClrSz1,lnClrSz2,lnClrSz3,lnClrSz4,;
           lnClrSz5,lnClrSz6,lnClrSz7,lnClrSz8,lnClrSz9,;
           lnClrPrice,lnClrCost,lnCstAveVl

llPrnClrLn = .F.           
= lfTranAray()  && Fill Transaction Arrays.
IF EMPTY(laTranAray[1])
  RETURN
ENDIF

lnMajorLen = LEN(gfItemMask("PM"))  && Major Length

lcPrntLoc = SPACE(6)
lcPrntSty = SPACE(lnMajorLen)
lcPrntNon = SPACE(19 - lnMajorLen)
*B129994,1  MMR            2-Fix bug of printing color name only with the first style in case 
*B129994,1  MMR              sort by style and print locatin detail=yes[Start]
lcChngSty = SPACE(lnMajorLen)
*:B129994,1  MMR.[End]
*-- Open files in another alias to get end of groups, and also get dyelot cost from location line.
USE (gcDataDir+'STYDYE.DBF') IN 0 AGAIN ALIAS RevStyDy
USE (gcDataDir+'STYLE.DBF') IN 0 AGAIN ALIAS RevStyle
SET ORDER TO STYLE IN STYLE
SET ORDER TO STYLE IN RevStyle

lcStyGroup = [PADR(STYDYE.STYLE,lnMajorLen)]

*-- if Sort by Style.
IF lcRPSortBy = 'S'
  SET ORDER TO STYDYE IN STYDYE
  SET ORDER TO STYDYE IN RevStyDy

  llPrintClr = llRpWhDeta OR llRpPrnDye  && Get sizes from Style file.
  *-- if print location detail or print stock dyelot detail

    *B605101,1 BWA 11/12/2001 Fix the bug of get a message there is no records to display although there is data to print.[START]
    llSChkDyeB = .F.
    *B605101,1 BWA 11/12/2001.[END]

  IF llPrintClr
    *B605101,1 BWA 11/12/2001 Fix the bug of get a message there is no records to display although there is data to print.[START]
    llSChkDyeB = .T.
    *B605101,1 BWA 11/12/2001.[END]

    lcMastFile = 'STYDYE'
    lcChldFile = 'STYLE'
    lcRevFile  = 'REVSTYDY'
    
    *-- if print locations with out print dyelot for stock.
    IF llRpWhDeta AND !llRpPrnDye
      lcExtndFlt = [EMPTY(Dyelot)]
    ENDIF
    
  ELSE       && else Print style records only without any other details.
    
    lcMastFile = 'STYLE'
    lcChldFile = 'STYDYE'
    lcRevFile  = 'REVSTYLE'

  ENDIF      && end if print location detail or print stock dyelot detail

ELSE         && else sort by location.

  *B605101,1 BWA 11/12/2001 Fix the bug of get a message there is no records to display although there is data to print.[START]
  llSChkDyeB = .T.
  *B605101,1 BWA 11/12/2001.[END]

  llPrintClr = .F.
  SET ORDER TO STYDYEW IN STYDYE
  SET ORDER TO STYDYEW IN RevStyDy

  lcMastFile = 'STYDYE'
  lcChldFile = 'STYLE'
  lcRevFile  = 'REVSTYDY'
  lcLocGroup = [STYDYE.CWARECODE]
  
ENDIF  && end if sort by style

*-- What is line cost and Style Cost [Begin]
* Update this lines to calculate the stock value on the 
* cost method in IC settings [start]

*--B802576,1 RAMY Change these lines to get the cost passed on IC 
*--B802576,1      setting [start]
*lcLineCost = [IIF((lcCstMeth="A") OR ("SOH" $ lcRepTarVl),Ave_Cost,Style.TotCost)]
*lcStyCost  = [IIF((lcCstMeth="A") OR ("SOH" $ lcRepTarVl),Style.Ave_Cost,Style.TotCost)]

*B603773,1 SSE 07/25/2000 set cost to Standard if Costing method is Standard else
*B603773,1                Average cost in all other costing methods. 
*lcLineCost = [IIF((lcCstMeth="A") , Ave_Cost , Style.TotCost)]
*lcStyCost  = [IIF((lcCstMeth="A") , Style.Ave_Cost , Style.TotCost)]

*B604163,1 ABD - Fix bug that we must print the average cost instead 
*B604163,1 ABD - Of the unit cost in all case without looking to cost method. [Begin]
*lcLineCost = [IIF((lcCstMeth="S") , Style.TotCost , Ave_Cost)]
*lcStyCost  = [IIF((lcCstMeth="S") , Style.TotCost , Style.Ave_Cost)]

*B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard in case the sort style.[START]
*lcLineCost  = [Ave_Cost]
*lcStyCost   = [Style.Ave_Cost]
*B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
*                         and Link to GL=yes but Generate Gl Cost=No [Start]
*IF llLinkGlJl
*B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
*IF !llGnGlCst AND lcCstMeth = "S"
IF lcCstMeth = "S"
*B129994,1 MMR.[End]
*B119818 ,1 MMM [End]    
  lcLineCost  = [Style.TotCost]
  lcStyCost   = [Style.TotCost]
ELSE
  lcLineCost  = [Ave_Cost]
  lcStyCost   = [Style.Ave_Cost]
ENDIF
*B605553,1 BWA 02/19/2002.[END]

*B604163,1 ABD - [End]

*--B802576,1 RAMY Change these lines to get the cost passed on IC [End]

* Update this lines to calculate the stock value on the 
* cost method in IC settings [end]

*-- What is line cost and Style Cost [End  ]

*-- Prepair report cost and price variables [Begin]
DO CASE
  *-- Case show Unit Price and Unit Cost only.
  CASE lcRpShow = 'S'
    *-- Per Header.
    *B804338,1 TMI [Start] Adjust Title alignment in Graphics Printing mode
    *lcCost_Val = ' Unit_Price' + ' Sales_Value'
    lcCost_Val = IIF(lcRepMode = 'Text',' Unit_Price' + ' Sales_Value';
                                       ,SPACE(3)+' Unit_Price' + SPACE(5) + ' Sales_Value')
    *B804338,1 TMI [End  ]
         
    *-- Per Line.
    *B603713,4 [Begin]
    *lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(STYLE.PRICEA,'9999999.99') +;
                 ' ' + TRANSFORM(lnNonPrice,'99999999.99'))"
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]        
      lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(STYLE.PRICEA,'999999999.99') +;
                 ' ' + TRANSFORM(lnNonPrice,'999999999.99'))"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlPn  to two 
    ELSE                 
      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(STYLE.PRICEA,'999999999.99') )"
      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99'))"
    ENDIF
    *B804338,1 TMI [End  ]
    
    *-- Per Non Majors from style file.
    *lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(STYLE.PRICEA,'9999999.99') +;
                 ' ' + TRANSFORM(lnClrPrice,'99999999.99'))"
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]    
      lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(STYLE.PRICEA,'999999999.99') +;
                 ' ' + TRANSFORM(lnClrPrice,'999999999.99'))"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlCl  to two 
    ELSE             
      lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(STYLE.PRICEA,'999999999.99') )"
      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99'))"
    ENDIF
    *B804338,1 TMI [End  ]                                                       

    *-- Per Location Group.
    *lcCst_VlLc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcLc,'99999999.99')"
    lcCst_VlLc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcLc,'999999999.99')"

    *-- Per Style (Major) Group.
    *lcCst_VlOp = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcOp,'99999999.99')"
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlOp = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcOp,'999999999.99')"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlOp  to two 
    ELSE
      lcCst_VlO1 = "' '"
      lcCst_VlO2 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
    ENDIF
    *B804338,1 TMI [End  ]
    
    *-- Per Grand Band.
    *lcCst_VlGd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcGd,'99999999.99')"
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlGd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonPrcGd,'999999999.99')"  
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlGd  to two 
    ELSE      
      lcCst_VlG1 = "' '"  
      lcCst_VlG2 = "TRANSFORM(lnNonPrcGd,'999999999.99')"  
    ENDIF
    *B804338,1 TMI [End  ]
    
    *B603713,4 [End]

  *-- Case show cost values only.
  CASE lcRpShow = 'C'
    *B604163,1 ABD - Fix bug that we must print the average cost instead 
    *B604163,1 ABD - Of the unit cost in all case without looking to cost method. [Begin]
    *lcCost_Val = '  Unit_Cost' + '  Cost_Value'
    *B804338,1 TMI [Start] Adjust Title alignment in Graphics Printing mode
    *lcCost_Val = '  Avrg_Cost' + '  Cost_Value'

    *B605553,1 BWA 02/19/2002 Change the header of the fields in the FRX due to the method of the cost.[START]
    *lcCost_Val = IIF(lcRepMode = 'Text','  Avrg_Cost' + '  Cost_Value';
                                       ,SPACE(2)+'  Avrg_Cost' + SPACE(5) + '  Cost_Value')
    *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
    *                         and Link to GL=yes but Generate Gl Cost=No [Start]
    *IF llLinkGlJl
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *IF !llGnGlCst AND lcCstMeth = "S"
    IF lcCstMeth = "S"
    *B129994,1 MMR.[End]
    *B119818,1 MMM [End]    
      lcCost_Val = IIF(lcRepMode = 'Text','  Unit_Cost' + '  Cost_Value';
                                         ,SPACE(2)+'  Unit_Cost' + SPACE(5) + '  Cost_Value')
    ELSE
      lcCost_Val = IIF(lcRepMode = 'Text','  Avrg_Cost' + '  Cost_Value';
                                         ,SPACE(2)+'  Avrg_Cost' + SPACE(5) + '  Cost_Value')
    ENDIF
    *B605553,1 BWA 02/19/2002.[END]

    *B804338,1 TMI [End  ]    
    *B604163,1 ABD - [End]

    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' +;
                 TRANSFORM(IIF(lcMastFile = 'STYLE' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') +;
                 ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlPn  to two 
    ELSE
      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',;
                   TRANSFORM(IIF(lcMastFile = 'STYLE' OR EMPTY(DYELOT),EVALUATE(lcLineCost),lnCstAveVl)  ,'9999999.99') )"
      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"
    ENDIF
    *B804338,1 TMI [End  ]
    
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(EVALUATE(lcStyCost),'9999999.99') +;
                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlCl  to two 
    ELSE

      *B123878,1 BWA 09/08/2004 Fix the bug of getting error "Missing expression" when run the report.[START]
      *lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(EVALUATE(lcStyCost),'9999999.99') )"
      lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(EVALUATE(lcStyCost),'9999999.99') )"
      *B123878,1 BWA 09/08/2004.[END]

      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"                                  
    ENDIF
    *B804338,1 TMI [End  ]
    
    lcCst_VlLc = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstLc,'99999999.99')"

    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlOp = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlOp  to two 
    ELSE
      lcCst_VlO1 = "' '"
      lcCst_VlO2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
    ENDIF
    *B804338,1 TMI [End  ]
        
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlGd = "' ' + SPACE(10) +' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlGd  to two 
    ELSE   
      lcCst_VlG1 = "' '"
      lcCst_VlG2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
    ENDIF
    *B804338,1 TMI [End  ]

  *-- Case show both total Price and cost values.
  CASE lcRpShow = 'B'
    *B804338,1 TMI [Start] Adjust Title alignment in Graphics Printing mode
    *lcCost_Val = 'Sales_Value' + '  Cost_Value'
     *B122005,1 NNA 03/07/2004 (Begin) Decrease the space between (S_value and C_Value) To Show The Cost_Value
     *lcCost_Val =  IIF(lcRepMode = 'Text','Sales_Value' + '  Cost_Value';
                                        ,SPACE(1)+'Sales_Value' +SPACE(5)+ '  Cost_Value')
     lcCost_Val =  IIF(lcRepMode = 'Text','Sales_Value' + '  Cost_Value';
                                        ,SPACE(1)+'Sales_Value' +SPACE(2)+ 'Cost_Value')
     *B122005,1 NNA (End)
    *B804338,1 TMI [End  ]
    
    *B603713,4 [Begin]
    *lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnNonPrice,'9999999.99') +;
                 ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlPn = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnNonPrice,'999999999.99') +;
                   ' ' + TRANSFORM(lnNonCost,'99999999.99'))"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlPn  to two 
    ELSE
      lcCst_VlP1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonPrice,'999999999.99') )"
      lcCst_VlP2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnNonCost,'99999999.99'))"
    ENDIF
    *B804338,1 TMI [End  ]
    
    *lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnClrPrice,'9999999.99') +;
                 ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
    
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlCl = "IIF(lnSize9=0 AND !llRpShwZer,'',' ' + TRANSFORM(lnClrPrice,'999999999.99') +;
                   ' ' + TRANSFORM(lnClrCost,'99999999.99'))"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlCl  to two 
    ELSE
      lcCst_VlC1 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrPrice,'999999999.99') )"
      lcCst_VlC2 = "IIF(lnSize9=0 AND !llRpShwZer,'',TRANSFORM(lnClrCost,'99999999.99'))"
    ENDIF
    *B804338,1 TMI [End  ]
    *lcCst_VlLc = "' ' + TRANSFORM(lnNonPrcLc,'9999999.99') +;
                  ' ' + TRANSFORM(lnNonCstLc,'99999999.99')"
    lcCst_VlLc = "' ' + TRANSFORM(lnNonPrcLc,'999999999.99') +;
                  ' ' + TRANSFORM(lnNonCstLc,'99999999.99')"

    *lcCst_VlOp = "' ' + TRANSFORM(lnNonPrcOp,'9999999.99') +;
                  ' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlOp = "' ' + TRANSFORM(lnNonPrcOp,'999999999.99') +;
                    ' ' + TRANSFORM(lnNonCstOp,'99999999.99')"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlOp  to two 
    ELSE
      lcCst_VlO1 = "TRANSFORM(lnNonPrcOp,'999999999.99')"
      lcCst_VlO2 = "TRANSFORM(lnNonCstOp,'99999999.99')"
    ENDIF
    *B804338,1 TMI [End  ]
    
    *lcCst_VlGd = "' ' + TRANSFORM(lnNonPrcGd,'9999999.99') +;
                  ' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
    *B804338,1 TMI [Start] Add if condition in the text mode - no change
    IF lcRepMode = 'Text'
    *B804338,1 TMI [End  ]
      lcCst_VlGd = "' ' + TRANSFORM(lnNonPrcGd,'999999999.99') +;
                    ' ' + TRANSFORM(lnNonCstGd,'99999999.99')"
    *B804338,1 TMI [Start] Separate the variable  lcCst_VlGd  to two 
    ELSE                
      lcCst_VlG1 = "TRANSFORM(lnNonPrcGd,'999999999.99')"
      lcCst_VlG2 = "TRANSFORM(lnNonCstGd,'99999999.99')"
    ENDIF
    *B804338,1 TMI [End  ]
    *B603713,4 [End]                  
ENDCASE
*-- Prepair report cost and price variables [End  ]

*-- Optimize Report expression by remove master file Alias name from it.
lcRpExp = STRTRAN(lcRpExp,lcMastFile+'.','')

*: B603692,1 AMH 06/29/2000 Fix SubTotal of style for only one Transaction (Start)
*-- lcRevFlt   : Variable hold Filter for REVSTYDY.
*-- laTrnRevAr : Array Hold Reference to printed transaction From REVSTYDY.
DIMENSION laTrnRevAr [8]
FOR lnI = 1 TO 8
  laTrnRevAr [lnI] = STRTRAN(latrnNonAr[lnI],'STYLE',lcRevFile)
ENDFOR
lcRevFlt = "EVAL(laTrnRevAr[1])+EVAL(laTrnRevAr[2])+"+;
           "EVAL(laTrnRevAr[3])+EVAL(laTrnRevAr[4])+EVAL(laTrnRevAr[5])+"+;
           "EVAL(laTrnRevAr[6])+EVAL(laTrnRevAr[7])+EVAL(laTrnRevAr[8]) <> 0"
IF !EMPTY(lcExtndFlt)
  lcRpExp = lcRpExp + [ AND ] + lcExtndFlt
  lcRevFlt = lcRevFlt + [ AND ] + lcExtndFlt
  SELECT REVSTYDY
  SET FILTER TO &lcExtndFlt
ENDIF
IF !llRPShwZer
  SELECT (lcRevFile)
  SET FILTER TO &lcRevFlt
ENDIF
*: B603692,1 AMH 06/29/2000 Fix SubTotal of style for only one Transaction (End)

*B603294,1 KHM 05/15/2000 (Begin) Check if not Show Zero qty then adding
*B603294,1                to the filter the checking of the quantity where 
*B603294,1                the total of size should not equal to Zero. 
*B603294,1                In other words, do not show a style that has 
*B603294,1                a zero qty in all sizes.
IF !llRPShwZer

  *B605101,1 BWA 11/12/2001 Fix the bug of get a message there is no records to display although there is data to print.[START]
  *lcRpExp = lcRpExp + " AND EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
  *          "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
  *          "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0"

  IF llSChkDyeB
    lcRpExp = lcRpExp + " AND (EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
              "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
              "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0 OR STYDYE.TOTSTK <> 0 )"
  ELSE
    lcRpExp = lcRpExp + " AND EVAL(laTrnNonar[1])+EVAL(laTrnNonar[2])+"+;
              "EVAL(laTrnNonar[3])+EVAL(laTrnNonar[4])+EVAL(laTrnNonar[5])+"+;
              "EVAL(laTrnNonar[6])+EVAL(laTrnNonar[7])+EVAL(laTrnNonar[8]) <> 0"
  ENDIF
  *B605101,1 BWA 11/12/2001.[END]

ENDIF
*B603294,1 KHM 05/15/2000 (End)

*B606972,1 WAB (Start) -- we have to replace the style file from the expr. to be Stydye
IF lcRPSortBy = 'W' 
  lnFindTran = ASCAN(laAllTrns,laRpRepTar[1])
  IF lnFindTran > 0
    lnFindTran = ASUBSCRIPT(laAllTrns,lnFindTran,1)
    lcRepTrans = ALLTRIM(laAllTrns[lnFindTran,2])
  ENDIF
  IF (lcRepTrans = 'OTS'  .OR. lcRepTrans = 'IOTS' )
    FOR lnI = 1 TO ALEN(laTrnNonar)
      lcExp = STRTRAN(laTrnNonar[lnI],"STYLE.","STYDYE.")
      laTrnNonar[lnI] = lcExp
    ENDFOR
  ENDIF
ENDIF
*B606972,1 WAB (End)

*-- Set Relation between Master and Child Files.
SELECT STYLE
SET RELATION TO "S" + SCALE INTO SCALE  && To print Scales.

SELECT (lcMastFile)
SET RELATION TO STYLE INTO (lcChldFile) ADDITIVE

lcNnMajTl = gfItemMask('HN')  && Non Major title.
*B132603,1  TMI [Start] convert the lcRpExp to a filter on a key , this may enhance speed of retriving line by line
*DO gfDispRe WITH EVAL('lcRPFormNa') , 'FOR ' + lcRpExp
lcSvOrd = ORDER(lcMastFile)
IF llOgFltCh
  lcKey = KEY()
  WAIT WINDOW NOWAIT 'Getting data ...'
  *B607838,1 TMI [Start] include the condition that the totstk <> 0 in the filter expression
  *INDEX ON &lcKey TO (gcWorkDir+lcTmpIdx) FOR EVAL(lcRpExp) 

  *B608237,1 NNA 08/23/2007 (Begin) remove the modification that done by B608089 and let the report works as it used to work before this modification
  *B608237,3 TMI 08/28/2007 by removing the option of filtering on zero stock
  *              note : this may cause a problem a REV03 that uses this option
  **B608089,1  TMI [Start] let this filter be based on a new option , not on the old option llRPShwZer
  **lcTmpExp = lcRpExp + IIF(llRPShwZer,'',' AND TOTSTK<>0')
  *lcTmpExp = lcRpExp + IIF(llRpZerStk,'',' AND TOTSTK<>0')
  lcTmpExp = lcRpExp                                           && added by TMI 08/28/2007 *B608237,3
  **B608089,1  TMI [End  ] 
  *B608237,1 NNA (End)

  *B607838,1 TMI [End  ] 
  INDEX ON &lcKey TO (gcWorkDir+lcTmpIdx) FOR EVAL(lcTmpExp)
  WAIT CLEAR
ENDIF
SET INDEX TO (gcWorkDir+lcTmpIdx)
GO TOP
IF !EOF()
  DO gfDispRe WITH EVAL('lcRPFormNa')
ELSE
  =gfModalGen('INM00052B00000','DIALOG') 
ENDIF

*- Restore previous order setting
SELECT (lcMastFile)
CLOSE INDEXES
SET ORDER TO &lcSvOrd
*B132603,1  TMI [End  ] 

USE IN RevStyDy
USE IN RevStyle
*-- end of lfSWOneTrn.

*!*************************************************************
*! Name      : lfTranAray
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Fill Transaction array with proper values to be evaluated in .FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfTranAray()
*!*************************************************************
*B602961,1
FUNCTION lfTranAray
PRIVATE lnFindTran
lnFindTran = ASCAN(laAllTrns,laRpRepTar[1])
IF lnFindTran > 0
  lnFindTran = ASUBSCRIPT(laAllTrns,lnFindTran,1)
  lcRepTarVl = ALLTRIM(laAllTrns[lnFindTran,2])
ELSE
  WAIT WINDOW 'Error, No Transaction was selected !!!' NOWAIT
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
    lcPrnTitle = 'WIP'

  *-- Stock on hand Case...
  CASE lcRepTarVl == "SOH"
    =lfDirctVal("STK")
    lcPrnTitle = 'Stock'

  *-- Plan Case...
  CASE lcRepTarVl == "PLA"
    =lfDirctVal("PLAN")
    lcPrnTitle = 'Plan'

  *-- Order Case...
  CASE lcRepTarVl == "ORD"
    =lfDirctVal("ORD")
    lcPrnTitle = 'Ordered'

  *-- Work order Case...
  CASE lcRepTarVl == "WORD"
    =lfDirctVal("NWO")
    lcPrnTitle = 'Work Ordered'

  *-- Intransit Case...
  CASE lcRepTarVl == "INT"
    =lfDirctVal("INTRANS")
    lcPrnTitle = 'Intransit'

  *-- Shipped Case...
  CASE lcRepTarVl == "SHP"
    =lfDirctVal("SHP")
    lcPrnTitle = 'Shipped'

  *-- Credit memo return Case...
  CASE lcRepTarVl == "RET"
    =lfDirctVal("RET")
    lcPrnTitle = 'Return'

  *-- Return Authorization Case...
  CASE lcRepTarVl == "RETA"
    =lfDirctVal("RA")
    lcPrnTitle = 'Return Auth.'

  *-- Allocation Case...
  CASE lcRepTarVl == "ALO"
    =lfDirctVal("ALO")
    lcPrnTitle = 'Allocated'

  *------------------------------------- Calculated Cases ----------------------
  *-----------------------------------------------------------------------------

  *-- UnAllocate Case...
  CASE lcRepTarVl == "UALO"
    =lfInDirect("STK","-ALO")
    lcPrnTitle = 'Unallocated'

  *-- Book Case...
  CASE lcRepTarVl == "BOK"
    =lfInDirect("SHP","+ORD")
    lcPrnTitle = 'Booked'

  *------------------------------------- Special Cases ----------------------
  *-----------------------------------------------------------------------------
  *-- OTS Case...
  CASE lcRepTarVl == "OTS"
    =lfInDirect("STK","-ORD","+" + IIF(lcRpOTSB="W","WIP","PLAN"),.T.)
    lcPrnTitle = 'Open to sell'

  *-- IOTS Case...
  CASE lcRepTarVl == "IOTS"
    =lfInDirect("STK","-ORD",'',.T.)
    lcPrnTitle = 'Imm. Open to sell'

ENDCASE
*-- Fill Transaction Arrays [End  ]
*-- end of lfTranAray.

*!*************************************************************
*! Name      : lfDirctVal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Evaluate Transaction to be printed.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDirctVal()
*!*************************************************************
*B602961,1
FUNCTION lfDirctVal
PARAMETER lcDirctVal
PRIVATE lnI
lnI = 0
STORE '' TO laTranAray , laTrnNonAr  && Intially null values.
FOR lnI = 1 TO 8
  laTranAray[lnI] = lcDirctVal + STR(lnI,1)
  laTrnNonAr[lnI] = "STYLE." + lcDirctVal + STR(lnI,1)
ENDFOR  
*-- end of lfDirctVal.

*!*************************************************************
*! Name      : lfInDirect
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Evaluate Indirect Transactions to be printed.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInDirect()
*!*************************************************************
*B602961,1
FUNCTION lfInDirect
PARAMETER lcValue1,lcValue2,lcValue3,llSpecCond
PRIVATE lcClrVal2,lcClrVal3,lnI

lcClrVal2 = STRTRAN(lcValue2,"-","-STYLE.")
lcClrVal2 = STRTRAN(lcClrVal2,"+","+STYLE.")

IF TYPE('lcValue3') $ 'UL' OR EMPTY(lcValue3)
  lcValue3 = ''
ELSE
  lcClrVal3 = STRTRAN(lcValue3,"-","-STYLE.")
  lcClrVal3 = STRTRAN(lcClrVal3,"+","+STYLE.")
ENDIF

lnI = 0
STORE '' TO laTranAray , laTrnNonAr  && Intially null values.
FOR lnI = 1 TO 8
  laTranAray[lnI] = lcValue1 + STR(lnI,1) + lcValue2 + STR(lnI,1) +;
                    IIF(EMPTY(lcValue3) , "" , lcValue3 + STR(lnI,1))

  laTrnNonAr[lnI] = "STYLE." + lcValue1 + STR(lnI,1) + lcClrVal2 + STR(lnI,1) +;
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
*B602961,1
FUNCTION lfChngForm
*-- if sort by location.
IF lcRPSortBy =='W'
  *-- if have one transaction only.
  IF ALEN(laRPRepTar,1) = 1 AND !EMPTY(laRPRepTar)
    lcRPFormNa = "ICSTYBYL"
  ELSE
    lcRPFormNa = "ICSTYBYW"
  ENDIF
  llRPWhDeta = .F.

ELSE  && Sort by anything rather than location.

  *-- if have one transaction only.

  *B802364,1 if sort by any thing rather than Style/Location. [Begin]
  *IF ALEN(laRPRepTar,1) = 1 AND !EMPTY(laRPRepTar)
  IF lcRPSortBy == "S" AND ALEN(laRPRepTar,1) = 1 AND !EMPTY(laRPRepTar)
  *B802364,1 if sort by any thing rather than Style/Location. [End..]
    lcRPFormNa = "ICSTYBYS"
  ELSE
    lcRPFormNa = "ICSTYBYO"
  ENDIF
    
ENDIF

*B604876,1 BWA 04/18/2002 Fix the bug of when toggling between text abd Graphics it return to the Graphics.[START]
lcNo1 = lcRepMode
lcNo2 = lcOGPlatForm
*B604876,1 BWA 04/18/2002.[END]

=lfRepPltFr(lcRPFormNa)

*B604876,1 BWA 04/18/2002 Fix the bug of when toggling between text abd Graphics it return to the Graphics.[START]
IF lcRepMode # lcNo1 OR lcOGPlatForm # lcNo2
  lcRepMode    = lcNo1
  lcOGPlatForm = lcNo2
  SHOW GET lcRepMode
ENDIF
*B604876,1 BWA 04/18/2002.[END]

*-- end of lfChngForm.

*!*************************************************************
*! Name      : lfEndOfGrp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Calculate End of any Group
*!*************************************************************
*! Passed Parameters : 1- Variable Name passed by reference.
*!                   : 2- Seek value.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfEndOfGrp()
*!*************************************************************
*B602961,1
FUNCTION lfEndOfGrp
PARAMETERS lcVariable,lcEqualExp
PRIVATE lnCurAlis
lnCurAlis = SELECT(0)
lcTagName = IIF(lcMastFile = 'STYLE','STYLE',IIF(lcRPSortBy = "S",'STYDYE','STYDYEW'))
SELECT (lcRevFile)

*B119983,1 MMM 11/05/2003 Fix Bug of not printing  subtotals when only this color is selected.[Start]
lcOldFilter=FILTER()                          &&Variable to hold old filter.
lcColors=laogvrflt[int(ascan(laogvrflt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")/7)+mod(ascan(laogvrflt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)"),7),6]        &&Variabe to hold selected colors.
lclocarr='"'+ STRTRAN(lccolors,'|','","')+'"'
lnCnt = 0                                       &&VAriable to hold the number of records Per Style.

*B125652,1 BWA 12/16/2004 Fix the bug printing the data in a very long time.[START]
*COUNT TO lnCnt FOR SUBSTR(Style,1,lnClrPo)=PADR(STYLE.STYLE,lnMajorLen)
PRIVATE lcOrder , llSeekVal , lcHoldWarH , lcFulIndex
STORE SPACE(0) TO lcHoldWarH , lcFulIndex
SELECT (lcRevFile)
lcOrder = ORDER()

IF lcMastFile = 'STYLE'
  SET ORDER TO TAG STYLE
  lcFulIndex = [STYLE]
  llSeekVal = SEEK(PADR(STYLE.STYLE,lnMajorLen))
ELSE
  IF lcRPSortBy = "S"
    SET ORDER TO TAG STYDYE
    lcFulIndex = [STYLE + CWARECODE + DYELOT]
    llSeekVal = SEEK(PADR(STYLE.STYLE,lnMajorLen))
  ELSE
    SET ORDER TO TAG STYDYEW
    lcFulIndex = [CWARECODE + STYLE + DYELOT]
    lcHoldWarH = &lcRevFile..cWarecode
    llSeekVal = SEEK(lcHoldWarH + PADR(STYLE.STYLE,lnMajorLen))
  ENDIF
ENDIF

IF llSeekVal
  SCAN REST WHILE lcFulIndex = ""
    IF PADR(STYLE,lnMajorLen) = PADR(STYLE.STYLE,lnMajorLen) AND IIF(lcRPSortBy # "S" AND lcMastFile # 'STYLE', CWARECODE == lcHoldWarH , .T.)
      lnCnt = lnCnt + 1
      IF lnCnt > 1
        EXIT
      ENDIF
    ELSE
	  EXIT
    ENDIF
  ENDSCAN
ENDIF

SET ORDER TO &lcOrder
*B125652,1 BWA 12/16/2004.[END]

IF NOT EMPTY(lcColors) and lnCnt<>1  
  SET FILTER TO INLIST(SUBSTR(Style,lnClrPo,lnColorLen),&lcLocArr)
ENDIF
*B119983,1 MMM. [End] 
SET ORDER TO TAG (lcTagName) DESCENDING
=SEEK(lcEqualExp)

*B607838,1 TMI [Start] locate the correct record that represents the end of style records in the case of a warehous is selected
PRIVATE lcKey
lcKey = KEY()
LOCATE REST WHILE &lcKey = lcEqualExp FOR &lcRpExp
*B607838,1 TMI [End  ] 

lcVariable = IIF(BETWEEN(RECNO(),1,RECCOUNT()),RECNO(),1)
SET ORDER TO TAG (lcTagName) ASCENDING
*B119983,1 MMM 11/05/2003 [Start] Return the filter to the original one.
SET FILTER TO &lcOldFilter
*B119983,1 MMM. [End] 

SELECT (lnCurAlis)
RETURN ''
*-- end of lfEndOfGrp.

*!*************************************************************
*! Name      : lfPrintHdr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Evaluate all report variables and print header.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Report Header
*!*************************************************************
*! Example     : =lfPrintHdr()
*!*************************************************************
*B602961,1
FUNCTION lfPrintHdr
PRIVATE lcPrintHdr

*B606596,1 BWA 12/11/2002 Check if the style change reinitial the variable hold the scale.[START]
IF lcStylScl # PADR(STYLE.STYLE,lnMajorLen)
  lcStylScl = PADR(STYLE.STYLE,lnMajorLen)
  lcScalCode = ' '
ENDIF
*B606596,1 BWA 12/11/2002.[END]

WAIT WINDOW  STYLE.STYLE NOWAIT
IF lcRPSortBy = "S"
  lcPrintHdr = lfStyHeadr()  && Sort by style header.
ELSE
  lcPrintHdr = lfLocHeadr()  && Sort by location header.
ENDIF
RETURN lcPrintHdr
*-- end of lfPrintHdr.

*!*************************************************************
*! Name      : lfStyHeadr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Evaluate By Style all report variables and print header.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Report Header
*!*************************************************************
*! Example     : =lfStyHeadr()
*!*************************************************************
*B602961,1
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
ENDFOR
*-- Evaluate all sizes values. [End  ]

*-- Zero values if OTS or IOTS and condition is true. [Begin]

IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMin)))
  STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
             lnSize6,lnSize7,lnSize8,lnSize9
ENDIF
*-- Zero values if OTS or IOTS and condition is true. [End  ]

*-- if user want to print zeros or total value not equal zero.
IF llRpShwZer OR lnSize9 <> 0
  
  lnNonPrice = lnSize9 * Style.PriceA
  IF lcMastFile = "STYLE" OR EMPTY(DYELOT)

* Update this lines to calculate the stock value on the 
* cost method in IC settings [start]
*    lnNonCost  = IIF("SOH" $ lcRepTarVl,nStkVal,IIF(lcCstMeth="A",Ave_Cost,Style.TotCost)*lnSize9)
    
    * Update this lines to get the stock value passed on cost method 
    * in IC settings [start]
    *B802813,1 (Start)
    *lnNonCost  = IIF(lcCstMeth="S" , TotCost*lnSize9 , ;
                 IIF("SOH" $ lcRepTarVl , nStkVal , Ave_Cost*lnSize9))

    *B603773,1 SSE 07/25/2000 Change the cost to be Standard Cost in case of Standard Costing 
    *B603773,1                method else Average Cost in all cases [Begin]
    *lnNonCost  = IIF(lcCstMeth="S" , Style.TotCost*lnSize9 , ;
    *             IIF("SOH" $ lcRepTarVl , nStkVal , Ave_Cost*lnSize9))

    *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.[START]
    *lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="S", Style.TotCost,Ave_Cost)*lnSize9)
    *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
    *                          and Link to GL=yes but Generate Gl Cost=No [Start]
    *IF llLinkGlJl
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *IF llGnGlCst
    *B119818,1 MMM [End]   
      *lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="S", Style.TotCost,Ave_Cost)*lnSize9)
    *ELSE
    *B129994,1 MMR.[End]
      lnNonCost  = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth="S" , Style.TotCost * lnSize9 , nStkVal) , IIF(lcCstMeth="S", Style.TotCost,Ave_Cost)*lnSize9)
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *ENDIF
    *B129994,1 MMR.[End]
    *B605553,1 BWA 02/19/2002.[END]

    *B603773,1 [End]

    *B802813,1 (End)

        * Update this lines to get the stock value [end]
    *B603773,1 SSE 07/25/2000 Change the cost to be Standard Cost in case of Standard Costing 
    *B603773,1                method else Average Cost in all cases [Begin]
    *lnCstAveVl = IIF("SOH" $ lcRepTarVl,nStkVal/lnSize9,IIF(lcCstMeth="A",Ave_Cost,Style.TotCost))

    *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.[START]
    *lnCstAveVl = IIF("SOH" $ lcRepTarVl , nStkVal/lnSize9 , IIF(lcCstMeth="S" , ;
                 Style.TotCost,Ave_Cost))
    *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
    *                          and Link to GL=yes but Generate Gl Cost=No [Start]
    *IF llLinkGlJl
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *IF llGnGlCst
    *B119818,1 MMM [End]  
      *lnCstAveVl = IIF("SOH" $ lcRepTarVl , nStkVal/lnSize9 , IIF(lcCstMeth="S" , ;
                   Style.TotCost,Ave_Cost))
    *ELSE
    *B129994,1 MMR.[End]
      lnCstAveVl = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth="S" , Style.TotCost , nStkVal/lnSize9 ) , IIF(lcCstMeth="S" , ;
                   Style.TotCost,Ave_Cost))
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *ENDIF
    *B129994,1 MMR.[End]
    *B603773,1 [End]

* Update this lines to calculate the stock value on the 
*  cost method in IC settings [end]

  ELSE
    IF !llRpWhDeta
      IF lcCstMeth="A"
        = lfGetCosts(@lnCstAveVl)
      ELSE
        lnCstAveVl = Style.TotCost
      ENDIF  
    ENDIF  
    lnNonCost = lnSize9 * lnCstAveVl
  ENDIF  
  
  =lfNonMjDes()  && Evaluate Non Major Description.

  *B606379,1 BWA 08/15/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
  IF gcDevice = "PRINTER" AND _PCOPIES > 1
    IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
      lnFrstRec1 = RECNO()
      lcPrntSty = SPACE(lnMajorLen)
      lcScalCode = SPACE(3)
    ENDIF
  ENDIF
  *B606379,1 BWA 08/15/2002.[END]

  *-- if it is new style group.
  IF !(PADR(STYLE.STYLE,lnMajorLen)==lcPrntSty)
    lcPrntSty = PADR(STYLE.STYLE,lnMajorLen)
    *B603294,1 KHM 05/15/2000 (Begin) Adding the scale description in case
    *B603294,1                of the changing of the sacle code.    
    *lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                 lfEndOfGrp(@lnEndOfSty,PADR(STYLE.STYLE,lnMajorLen)),35) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz1),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz2),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz3),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz4),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz5),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz6),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz7),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz8),5)
    *E301158,1 (HBG) 05/30/2000 [Begin]
    *-- if not print sizes "lcPrintHdr" will not include scales
    IF !llRpPrtSiz
      lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                 lfEndOfGrp(@lnEndOfSty,PADR(STYLE.STYLE,lnMajorLen)),35) + SPACE(3)
    ELSE 
      *E301158,1 (HBG) 05/30/2000 [End  ]
      *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
      *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
      IF llTextMode
        *E301481,1 ABD - [End]
   
        lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                     lfEndOfGrp(@lnEndOfSty,PADR(STYLE.STYLE,lnMajorLen)),35) + SPACE(3) +;
                     IIF(lcScalCode = Style.Scale,'',;
                     PADL(ALLTRIM(Scale.Sz1),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz2),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz3),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz4),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz5),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz6),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz7),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz8),5))
        *E301158,1 (HBG) 05/30/2000 [Begin]
        *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
        *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
      ELSE
        lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
        FOR I = 1 To 8
          Z = STR(I,1)
          laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
        ENDFOR
      ENDIF
      *E301481,1 ABD - [End]

    ENDIF  && end of if not print sizes "lcPrintHdr" will not include scales
    *E301158,1 (HBG) 05/30/2000 [End ]            
    *B603294,1 KHM 05/15/2000 (End)
    
      lnMaxCnt   = SCALE.Cnt
      =lfSumGroup("lnStySz","lnSize",.T.) && Calculate Style initial totals.
      lnNonPrcOp = lnNonPrice             && Calculate Total Price.
      lnNonCstOp = lnNonCost              && Calculate Total Cost.      

      *B603294,1 KHM 05/15/2000 (Begin) Assigning the scale of the style
      *B603294,1                to lcScaleCode.
      lcScalCode = Style.Scale
      *B603294,1 KHM 05/15/2000 (End)

  ELSE  && it is the same style
    *B802905,1 BWA 03/01/2000 Fix the bug of not printing the size scale and modify a new option[START]
    IF llRpScale
      *B603294,1 KHM 05/15/2000 (Begin) Commenting the following lines
      *B603294,1                 in order to do not print the scale 
      *B603294,1                 description as long as it has not been changed.
      *lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                 lfEndOfGrp(@lnEndOfSty,PADR(STYLE.STYLE,lnMajorLen)),35) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz1),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz2),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz3),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz4),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz5),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz6),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz7),5) + SPACE(3) +;
                 PADL(ALLTRIM(Scale.Sz8),5)
          *lcPrintHdr = STRTRAN(lcPrintHdr,SUBSTR(lcPrintHdr,1,35),SPACE(35))
      IF Style.Scale <> lcScalCode
        *E301158,1 (HBG) 05/30/2000 [Begin]
        *-- if not print sizes "lcPrintHdr" will not include scales
        IF !llRpPrtSiz
          lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
          lfEndOfGrp(@lnEndOfSty,PADR(STYLE.STYLE,lnMajorLen)),35) + SPACE(3)
        ELSE
          *E301158,1 (HBG) 05/30/2000 [End  ]
          *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
          *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
          IF llTextMode
            *E301481,1 ABD - [End]

            lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                         lfEndOfGrp(@lnEndOfSty,PADR(STYLE.STYLE,lnMajorLen)),35) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz1),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz2),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz3),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz4),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz5),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz6),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz7),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz8),5)
            lcPrintHdr = STRTRAN(lcPrintHdr,SUBSTR(lcPrintHdr,1,35),SPACE(35))
            *E301158,1 (HBG) 05/30/2000 [Begin]

            *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
            *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
          ELSE
            lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                         lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
            FOR I = 1 To 8
              Z = STR(I,1)
              laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
            ENDFOR
          ENDIF
          *E301481,1 ABD - [End]

        ENDIF  && end of if not print sizes "lcPrintHdr" will not include scales
        *E301158,1 (HBG) 05/30/2000 [End]
        lcScalCode = Style.Scale
      ELSE
        lcPrintHdr = ''
      ENDIF
      *B603294,1 KHM 05/15/2000 (End)
    ENDIF      
    *B802905,1 BWA 03/01/2000 [END]
 
    lnMaxCnt = MAX(lnMaxCnt,SCALE.Cnt)
    
    *-- if line have quantities then calculate style group totals.
    IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
       lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

      *B606379,1 BWA 08/15/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
      IF gcDevice = "PRINTER" AND _PCOPIES > 1
        IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
          lnFrstRec1 = RECNO()
          STORE 0 TO lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
             lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,;
             lnNonPrcOp,lnNonCstOp
        ENDIF
      ENDIF
      *B606379,1 BWA 08/15/2002.[END]

      =lfSumGroup("lnStySz","lnSize")       && Calculate Style totals.
      IF lcMastFile = 'STYLE' OR EMPTY(Dyelot)
        lnNonPrcOp = lnNonPrcOp + lnNonPrice  && Calculate Total Price.
        lnNonCstOp = lnNonCstOp + lnNonCost   && Calculate Total Cost.
      ENDIF
    ENDIF
      
  ENDIF   && end if it is new style.

  *-- if line have quantities then calculate grand totals.
  IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
     lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

    *B606071,1 BWA 07/07/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
    IF gcdevice = "PRINTER" AND _PCOPIES > 1
      IF lnFrstRec = 0 OR lnFrstRec = RECNO()
        lnFrstRec = RECNO()
        STORE 0 TO lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
                   lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnNonPrcGd,lnNonCstGd
      ENDIF
    ENDIF
    *B606071,1 BWA 07/07/2002.[END]

    =lfSumGroup("lnGrdSz","lnSize")      && Calculate Grand totals.
    IF lcMastFile = 'STYLE' OR EMPTY(Dyelot)
      lnNonPrcGd = lnNonPrcGd + lnNonPrice && Calculate Total Price.
      lnNonCstGd = lnNonCstGd + lnNonCost  && Calculate Total Cost.
    ENDIF
  ENDIF  

  *-- Evaluate all sizes values from style file Case Master file is StyDye 
  *-- and print location detail, or print style dyelots . [Begin]
  *B129994,1  MMR            2-Fix bug of printing color name only with the first style in case 
  *B129994,1  MMR              sort by style and print locatin detail=yes[Start]
  *IF lcMastFile = "STYDYE" AND llPrintClr AND !(RIGHT(STYLE,19 - lnMajorLen)==lcPrntNon)
  IF lcMastFile = "STYDYE" AND llPrintClr 
    IF ALLTRIM(STYLE.CSTYMAJOR)<>ALLTRIM(lcChngSty) OR !(RIGHT(STYLE,19 - lnMajorLen)==lcPrntNon)
      lcChngSty=ALLTRIM(STYLE.CSTYMAJOR)
  *B129994,1  MMR.[End]
      llPrnClrLn = .T.
      lcPrntNon  = RIGHT(STYLE,19 - lnMajorLen)
      lnAllSizes = 0
      lnClrSz9   = 0
      FOR lnAllSizes = 1 TO 8
        lcSize   = "lnClrSz"+STR(lnAllSizes,1)
        &lcSize  = EVALUATE(laTrnNonAr[lnAllSizes])
        lnClrSz9 = lnClrSz9 + &lcSize
      ENDFOR
      lnClrPrice = lnClrSz9 * Style.PriceA

      *B603773,1 SSE 07/25/2000 Change the cost to be Standard Cost in case of Standard Costing 
      *B603773,1                method else Average Cost in all cases [Begin]
      *lnClrCost = IIF("SOH" $ lcRepTarVl,Style.nStkVal,IIF(lcCstMeth="A",Style.Ave_Cost,Style.TotCost)*lnClrSz9)

      *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.[START]
      *lnClrCost  = IIF("SOH" $ lcRepTarVl , Style.nStkVal , IIF(lcCstMeth="S",;
      *             Style.TotCost,Style.Ave_Cost)*lnClrSz9)
      *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
      *                          and Link to GL=yes but Generate Gl Cost=No [Start]
      *IF llLinkGlJl
      *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
      *IF llGnGlCst
      *B119818,1 MMM [End]     
        *lnClrCost = IIF("SOH" $ lcRepTarVl , Style.nStkVal , IIF(lcCstMeth="S",;
                    Style.TotCost,Style.Ave_Cost)*lnClrSz9)
      *ELSE
      *B129994,1 MMR.[End]
        lnClrCost = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth="S", Style.TotCost * lnClrSz9 , Style.nStkVal ) , IIF(lcCstMeth="S",;
                    Style.TotCost,Style.Ave_Cost)*lnClrSz9)
      *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
      *ENDIF
      *B129994,1 MMR.[End]
      *B605553,1 BWA 02/19/2002.[END]


      *AAN
      *lcLineCost = [IIF((lcCstMeth="S") , Style.TotCost , Ave_Cost)]
      *lcStyCost  = [IIF((lcCstMeth="S") , Style.TotCost , Style.Ave_Cost)]
      *AAN                 
                 
                 
      *B603773,1 [End]
    *B129994,1  MMR            2-Fix bug of printing color name only with the first style in case 
    *B129994,1  MMR              sort by style and print locatin detail=yes[Start]
    ENDIF
    *B129994,1  MMR.[End]
  ELSE
    llPrnClrLn = .F.
  ENDIF  
  *-- and print location detail, or print style dyelots . [End  ]
ENDIF     && end if user want to print zeros or total value not equal zero.
RETURN lcPrintHdr
*-- end of lfStyHeadr.

*!*************************************************************
*! Name      : lfSumGroup
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Get summation of any group.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSumGroup()
*!*************************************************************
*B602961,1
FUNCTION lfSumGroup
PARAMETERS lcSummedVr,lcLineVar,llPrimary
*-- if you loop style file or you loop location record in stydye file.
IF lcMastFile = 'STYLE' OR EMPTY(Dyelot)
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
ENDIF  && end if you loop style file or you loop location record in stydye file.
*-- end of lfSumGroup.


*!*************************************************************
*! Name      : lfNonMjDes
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Evaluate Non Major Code and Description
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNonMjDes()
*!*************************************************************
*B602961,1
FUNCTION lfNonMjDes
PRIVATE lnI , lcTemp
STORE '' TO lcTemp 
lnI = 0
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lcTemp = ''
  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSeg[lnI,1] $ "FOTQ"
      IF SEEK(STR(lnI,1)+SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),"ICSEGVAL")
        lcTemp = ALLTRIM(ICSEGVAL.cISgValSd)
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
      IF SEEK("S"+SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),"SCALE")
        lcTemp = ALLTRIM(SCALE.cScl_desc)
      ENDIF
    
  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
ENDFOR    && end Loop Around Non Major elements.
*-- end of lfNonMjDes.

*!*************************************************************
*! Name      : lfInitVals
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Initially Zero all line values.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInitVals()
*!*************************************************************
*B602961,1
FUNCTION lfInitVals
STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,;
           lnSize5,lnSize6,lnSize7,lnSize8,lnSize9,;
           lnNonPrice,lnNonCost
IF lcMastFile = "STYDYE" AND llPrintClr
  STORE 0 TO lnClrSz1,lnClrSz2,lnClrSz3,lnClrSz4,;
             lnClrSz5,lnClrSz6,lnClrSz7,lnClrSz8,lnClrSz9,;
             lnClrPrice,lnClrCost
ENDIF           
*-- end of lfInitVals.

*!*************************************************************
*! Name      : lfGetCosts
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : If print dyelots for stock get cost from location record.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetCosts()
*!*************************************************************
*B602961,1
FUNCTION lfGetCosts
PARAMETERS lnThisCost
PRIVATE lnCurrAlis , lcSeekExpr
lnCurrAlis = SELECT(0)
SELECT REVSTYDY
lcSeekExpr = IIF(lcRPSortBy = "S",STYDYE.STYLE+STYDYE.CWARECODE,;
                                  STYDYE.CWARECODE+STYDYE.STYLE)
IF SEEK(lcSeekExpr)
  lnThisCost = Ave_Cost
ELSE
  lnThisCost = 0
ENDIF
SELECT (lnCurrAlis)
*-- end of lfGetCosts.

*!*************************************************************
*! Name      : lfPrntBin
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Print location bins.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrntBin()
*!*************************************************************
*B602961,1
FUNCTION lfPrntBin

IF (lcRPSortBy = "L" OR llRpPrnLoc) AND EMPTY(Dyelot) AND (llRpShwZer OR lnSize9 <> 0)
  PRIVATE lnCurrAls , lcCurrOrd
  lnCurrAls = SELECT(0)
  SELECT WHSLOC
  lcCurrOrd = ORDER()
  SET ORDER TO WHSLOCST
  IF SEEK(StyDye.Style+SPACE(6)+StyDye.cWareCode)
    PRIVATE lnBinNum
    lnBinNum = 0
    SCAN REST WHILE Style       +Color   +cWareCode       +cLocation = ;
                    StyDye.Style+SPACE(6)+StyDye.cWareCode
      lcLocBins = IIF(EMPTY(lcLocBins),'Bin : ',lcLocBins+', ') + ALLTRIM(cLocation)
      lnBinNum = lnBinNum + 1
    ENDSCAN
    IF lnBinNum > 1
      lcLocBins = STRTRAN(lcLocBins,"Bin :","Bins :")
    ENDIF
  ENDIF
  SET ORDER TO &lcCurrOrd
  SELECT (lnCurrAls)
ENDIF

RETURN ''
*-- end of lfPrntBin.

*!*************************************************************
*! Name      : lfvSalVal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Validation If user does not have cost previlages,
*!           : title is to print price (Y/N).
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvSalVal()
*!*************************************************************
*B602961,1
FUNCTION lfvSalVal
IF llRpSalVal
  lcRpShow = "S"
ELSE
  lcRpShow = "N"
ENDIF
=lfvPrint()  && Call normal validate.
*-- end of lfvSalVal.

*!*************************************************************
*! Name      : lfLocHeadr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/27/1999
*! Purpose   : Evaluate By Location all report variables and print header.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Report Header
*!*************************************************************
*! Example     : =lfLocHeadr()
*!*************************************************************
*B602961,1
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
ENDFOR
*-- Evaluate all sizes values. [End  ]

*-- Zero values if OTS or IOTS and condition is true. [Begin]
IF ("OTS" $ lcRepTarVl) AND (((lcRpOTSSig ="P") AND (lnSize9 < lnRpOTSMin)) OR ((lcRpOTSSig ="N") AND (lnSize9 > lnRpOTSMin)))
  STORE 0 TO lnSize1,lnSize2,lnSize3,lnSize4,lnSize5,;
             lnSize6,lnSize7,lnSize8,lnSize9
ENDIF
*-- Zero values if OTS or IOTS and condition is true. [End  ]

*-- if user want to print zeros or total value not equal zero.
IF llRpShwZer OR lnSize9 <> 0
  
  lnNonPrice = lnSize9 * Style.PriceA
  *-- if Location record.
  IF EMPTY(DYELOT)

    *B605553,1 BWA 02/19/2002 Use the standard cost in case no link to Gl and the cost method is standard.[START]
    *lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="A", Ave_Cost,Style.TotCost)*lnSize9)
    *lnCstAveVl = IIF("SOH" $ lcRepTarVl,nStkVal/lnSize9,IIF(lcCstMeth="A",Ave_Cost,Style.TotCost))
    *B119818,1 MMM 10/15/2003 Fix Bug of not Using Standard Cost in Case Costing method=Standard
    *                          and Link to GL=yes but Generate Gl Cost=No [Start]
    *IF llLinkGlJl
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *IF llGnGlCst
    *B119818,1 MMM [End]  
      *lnNonCost  = IIF("SOH" $ lcRepTarVl , nStkVal , IIF(lcCstMeth="A", Ave_Cost,Style.TotCost)*lnSize9)
      *lnCstAveVl = IIF("SOH" $ lcRepTarVl,nStkVal/lnSize9,IIF(lcCstMeth="A",Ave_Cost,Style.TotCost))
    *ELSE
    *B129994,1 MMR.[End]
      lnNonCost  = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth = "S" , Style.TotCost *lnSize9 , nStkVal )       , IIF(lcCstMeth="A", Ave_Cost,Style.TotCost)*lnSize9)
      lnCstAveVl = IIF("SOH" $ lcRepTarVl , IIF(lcCstMeth = "S" , Style.TotCost          , nStkVal/lnSize9), IIF(lcCstMeth="A",Ave_Cost,Style.TotCost))
    *B129994,1 MMR 10/17/2005 1-Fix Bug of not printing Standard Cost in Case Costing method=Standard.
    *ENDIF
    *B129994,1 MMR.[End]
    *B605553,1 BWA 02/19/2002.[END]

    =lfNonMjDes()  && Evaluate Non Major Description.
  
  ELSE  && Dyelot Record value.
    IF llRpPrnDye
      IF lcCstMeth="A"
        = lfGetCosts(@lnCstAveVl)
      ELSE
        lnCstAveVl = Style.TotCost
      ENDIF
    ENDIF  
    lnNonCost = lnSize9 * lnCstAveVl
  ENDIF  

  *B606379,1 BWA 08/15/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
  IF gcDevice = "PRINTER" AND _PCOPIES > 1
    IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
      lnFrstRec1 = RECNO()
      lcPrntLoc  = SPACE(6)
      lcPrntSty  = SPACE(lnMajorLen)
      lcScalCode = SPACE(3)
    ENDIF
  ENDIF
  *B606379,1 BWA 08/15/2002.[END]

  *-- if it is new location.
  IF !(cWareCode==lcPrntLoc)
    lcPrntLoc = cWareCode
    lcPrintHdr = "Location : " + cWareCode +;
                 lfEndOfGrp(@lnEndOfLoc,cWareCode)

      =lfSumGroup("lnLocSz","lnSize",.T.) && Calculate Location initial totals.
      lnNonPrcLc = lnNonPrice             && Calculate Total Price.
      lnNonCstLc = lnNonCost              && Calculate Total Cost.      

  ELSE  && it is the same location

    IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
       lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

      *B606379,1 BWA 08/15/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
      IF gcDevice = "PRINTER" AND _PCOPIES > 1
        IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
          lnFrstRec1 = RECNO()
          STORE 0 TO lnLocSz1,lnLocSz2,lnLocSz3,lnLocSz4,;
                     lnLocSz5,lnLocSz6,lnLocSz7,lnLocSz8,lnLocSz9,;
                     lnNonPrcLc,lnNonCstLc
        ENDIF
      ENDIF
      *B606379,1 BWA 08/15/2002.[END]

      =lfSumGroup("lnLocSz","lnSize")         && Calculate Style totals.
      IF EMPTY(Dyelot)
        lnNonPrcLc = lnNonPrcLc + lnNonPrice  && Calculate Total Price.
        lnNonCstLc = lnNonCstLc + lnNonCost   && Calculate Total Cost.      
      ENDIF
    ENDIF
      
  ENDIF   && end if it is new location.
  
    *-- if it is new style.
  IF !(cWareCode + PADR(STYLE.STYLE,lnMajorLen)==lcPrntSty)
    lcPrntSty  = cWareCode + PADR(STYLE.STYLE,lnMajorLen)
    *E301158,1 (HBG) 05/30/2000 [Begin]
    *-- if not print sizes "lcPrintHdr" will not include scales
    IF !llRpPrtSiz
      lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                   lfEndOfGrp(@lnEndOfSty,lcPrntSty),35) + SPACE(3)
    ELSE
      *E301158,1 (HBG) 05/30/2000 [End  ]
      *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
      *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
      IF llTextMode
        *E301481,1 ABD - [End]

        *B604949,1 BWA 01/10/2001 Fix the bug of the 2nd line in case the extended size scale doesn't appear[START]
        *lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz1),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz2),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz3),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz4),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz5),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz6),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz7),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz8),5)

        lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35) + SPACE(3) +;
                     IIF(lcScalCode = Style.Scale,'',;
                     PADL(ALLTRIM(Scale.Sz1),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz2),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz3),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz4),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz5),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz6),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz7),5) + SPACE(3) +;
                     PADL(ALLTRIM(Scale.Sz8),5))

        *B604949,1 BWA 01/10/2001.[END]
        *E301158,1 (HBG) 05/30/2000 [Begin]
        *E301481,1 ABD - Check if  Print the report in graphic mode  or in text mode. 
        *E301481,1 ABD - If print in graphic mode fill the scale array . [Begin]
      ELSE

        lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                     lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
        FOR I = 1 To 8
          Z = STR(I,1)
          laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
        ENDFOR
      ENDIF
      *E301481,1 ABD - [End]

    ENDIF     && end if not print sizes "lcPrintHdr" will not include scales
    *E301158,1 (HBG) 05/30/2000 [End ]
      lnMaxCnt   = SCALE.Cnt
      =lfSumGroup("lnStySz","lnSize",.T.) && Calculate Style initial totals.
      lnNonPrcOp = lnNonPrice             && Calculate Total Price.
      lnNonCstOp = lnNonCost              && Calculate Total Cost.

    *B604949,1 BWA 01/10/2001 Fix the bug of the 2nd line in case the extended size scale doesn't appear[START]
    lcScalCode = Style.Scale
    *B604949,1 BWA 01/10/2001.[END]

  ELSE  && it is the same style

    *B604949,1 BWA 01/10/2001 Fix the bug of the 2nd line in case the extended size scale doesn't appear[START]
    *lcPrintSty = ''
    IF llRpScale
      IF Style.Scale <> lcScalCode
        IF !llRpPrtSiz
          lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                      lfEndOfGrp(@lnEndOfSty,PADR(STYLE.STYLE,lnMajorLen)),35) + SPACE(3)
        ELSE
          IF llTextMode
            lcPrintHdr = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                         lfEndOfGrp(@lnEndOfSty,lcPrntSty),35) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz1),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz2),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz3),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz4),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz5),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz6),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz7),5) + SPACE(3) +;
                         PADL(ALLTRIM(Scale.Sz8),5)
            lcPrintHdr = STRTRAN(lcPrintHdr,SUBSTR(lcPrintHdr,1,35),SPACE(35))
          ELSE
            lcPrintSty = PADR(ALLTRIM(lcMajTtl) + ' :' +ALLTRIM(EVALUATE(lcStyGroup)) + ' ' + PADR(Style.Desc,15) +;
                         lfEndOfGrp(@lnEndOfSty,lcPrntSty),35)
            FOR I = 1 To 8
              Z = STR(I,1)
              laScals[I] = PADL(ALLTRIM(Scale.Sz&z),5)
            ENDFOR
          ENDIF
        ENDIF  && end of if not print sizes "lcPrintHdr" will not include scales
        lcScalCode = Style.Scale
      ELSE
        lcPrintHdr = ''
      ENDIF
    ENDIF      
    *B604949,1 BWA 01/10/2001.[END]

    lnMaxCnt = MAX(lnMaxCnt,SCALE.Cnt)
    IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
       lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

      *B606379,1 BWA 08/15/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
      IF gcDevice = "PRINTER" AND _PCOPIES > 1
        IF lnFrstRec1 = 0 OR lnFrstRec1 = RECNO()
          lnFrstRec1 = RECNO()
          STORE 0 TO lnStySz1,lnStySz2,lnStySz3,lnStySz4,;
             lnStySz5,lnStySz6,lnStySz7,lnStySz8,lnStySz9,;
             lnNonPrcOp,lnNonCstOp
        ENDIF
      ENDIF
      *B606379,1 BWA 08/15/2002.[END]

      =lfSumGroup("lnStySz","lnSize")       && Calculate Style totals.
      IF EMPTY(Dyelot)
        lnNonPrcOp = lnNonPrcOp + lnNonPrice  && Calculate Total Price.
        lnNonCstOp = lnNonCstOp + lnNonCost   && Calculate Total Cost.      
      ENDIF
    ENDIF
      
  ENDIF   && end if it is new style.

  IF lnSize1 <> 0 OR lnSize2 <> 0 OR lnSize3 <> 0 OR lnSize4 <> 0 OR ;
     lnSize5 <> 0 OR lnSize6 <> 0 OR lnSize7 <> 0 OR lnSize8 <> 0

    *B606071,1 BWA 07/07/2002 Fix the bug of duplicating the totals when selecting to print more than 1 copy.[START]
    IF gcdevice = "PRINTER" AND _PCOPIES > 1
      IF lnFrstRec = 0 OR lnFrstRec = RECNO()
        lnFrstRec = RECNO()
        STORE 0 TO lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
                   lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,lnNonPrcGd,lnNonCstGd
      ENDIF
    ENDIF
    *B606071,1 BWA 07/07/2002.[END]

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
*! Name      : lfsrSty
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/28/1999
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
*B802345,1
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/28/1999
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
*B802345,1
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*B802905,1 BWA 03/01/2000 Fix the bug of not printing the size scale and modify a new option[START]
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/01/2000
*! Purpose   : 
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
CLEAR READ

*B802905,1 BWA 03/01/2000 [END]
*!*************************************************************
*! Name      : lfPvRun
*! Developer : Mohamed Shokry (MHM)
*! Date      : 07/26/2000
*! Purpose   : change color code in filter array
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
*B603667,1
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
lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
*-- Get first color 
lcsty1 = SUBSTR(laOgVrFlt[lnClrSgPo,6],1,lnColorLen)

*-- loop for No. of Occurance of Separator "|" in Color exp. and add 1 to last color 
FOR lnCounter = 1 TO OCCUR("|",laOgVrFlt[lnClrSgPo,6])+1

  *--get from second color to rest color 
  IF lnCounter > 1
    *-- get  position of "|"
    lnFirstPos  = ATC('|',laOgVrFlt[lnClrSgPo,6],lnCounter-1)
    *-- we add one to positon to substr after "|"
    lcSty2      = SUBSTR(laOgVrFlt[lnClrSgPo,6],lnFirstPos+1,lnColorLen)
  ENDIF

  IF !EMPTY(lcSty2)
    *--Concatenate expression
    lcSty1 = lcsty1 + '|' + lcSty2
  ELSE

    *--for chose first color only
    lcSty1 = lcsty1
  ENDIF
ENDFOR    

laOgVrFlt[lnClrSgPo,6] = lcSty1

*-- end of lfPvRun


*B804338,1 TMI [Start] When prints 2 copies of the reports the grand totals are duplicated
*:**************************************************************************
*:* Name        : lfRstGrdVr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/19/2001
*:* Purpose     : Reset grand totals variables
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRstGrdVr()
*:***************************************************************************
FUNCTION lfRstGrdVr
STORE 0 TO lnGrdSz1,lnGrdSz2,lnGrdSz3,lnGrdSz4,;
           lnGrdSz5,lnGrdSz6,lnGrdSz7,lnGrdSz8,lnGrdSz9,;
           lnNonPrcGd,lnNonCstGd
RETURN ''
*--End of lfRstGrdVr.
*B804338,1 TMI [End  ]