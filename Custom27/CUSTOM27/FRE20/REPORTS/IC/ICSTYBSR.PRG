*:***************************************************************************
*: Program file  : ICSTYBSR.PRG
*: Program desc. : CUSTOMIZED BOUGHT VS. SOLD REPORT FOR FRE20.
*: For Report    : ICSTYBSR.FRX , ICSTYBSS.FRX
*! Date          : 14/05/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION(AL)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102608
*:***************************************************************************
*: Calls : 
*:    Functions  : lfCreatTmp , lfColctDat , lfExport   , lfAddField , lfPricOrd
*:               : lfvStyle   , lfSetSTY   , lfMajTtGet , lfMajPic   , lfNonMaj
*:               : lfvDateRng , lfvPbOk    , lfvPrnReps , lfwRepWhen , lfChngForm
*;               : lfSubTot   , lfGropTot  , lfGrndTot  , lfWipColct
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSTYBSR
*:***************************************************************************
*: Modifications : 
*:***************************************************************************
*:B606784,1 WAB 12/18/2002 Fix the wrong caluclation of all the fields .
*:B606828,1 WAB 12/29/2002 /fix som problems in the Total qty of all the columns
*:***************************************************************************

*-- Get the Variables its Initial value.
STORE .F. TO llPrnPrce1 , llPrnPrce2 , llPrnPrce3 , llPrnPrce4 , llPrnPrce5 , llPrnPrce6 , llPrnPrce7 , llPrnPrce8
STORE 0   TO lnSllThr    &&To check that the division and the TTL Sold fields were selected.

*--THE STYLE LENGTH
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

*--In case the user didn't selecte any transaction to preview all of them.
llSelAll = EMPTY(laRPRepTar)
IF llSelAll
  DIMENSION laRPRepTar[1]
ENDIF

*--Functions to creat the temp. file and collecting the data.
=lfCreatTmp()
=lfColctDat()

*-- Browsing the report.
SELECT (lcTmpFile)
LOCATE

IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SELECT (lcTmpFile)
SET RELATION TO "S" + &lcTmpFile..ScalTmp INTO SCALE ADDITIVE
DO gfDispRe WITH EVAL('lcRPForm')

*--Export the temp. file to excel file.
IF llRPExport
  =lfExport()
ENDIF

SELECT (lcTmpFile)
SET RELATION TO
WAIT CLEAR
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

*--In case the user select to export to Excel file.
IF llRPExport
  IF lcRpKind =='S'
    CREATE Table (lcExprtFil) (Style C(19) , Type C(11) , Tot_Units C(6) , Tot_Value C(10))
  ELSE
    CREATE Table (lcExprtFil) (Style C(19) , Type C(11) ,;
                               Size1 C(6) , Value1 C(7) , Size2 C(6) , Value2 C(7) ,;
                               Size3 C(6) , Value3 C(7) , Size4 C(6) , Value4 C(7) ,;
                               Size5 C(6) , Value5 C(7) , Size6 C(6) , Value6 C(7) ,;
                               Size7 C(6) , Value7 C(7) , Size8 C(6) , Value8 C(7) ,;
                               Tot_Units C(6) , Tot_Value C(10))
  ENDIF
ENDIF

*--Creat the Temp. file that hold the data.
CREATE Table (lcTmpFile) (Style C(19)   , SellThru N(5) , Season C(6)  , Divsion C(6)  ,;
                          StyGroup C(6) , PricA N(12,2) , llPrnTotSt L , llPrnGrpSt L  ,;
                          llPrnGrnd L   , ScalTmp C(3)                                 ,;
                          llPrceBot L   , llPrceWip L   , llPrceStk L  , llPrceOord L  ,;
                          llPrceAvlb L  , llPrceShp L   , llPrceTtl L  , llPrceRet L)

DIMENSION laTempStru[1,4]
SELECT (lcTmpFile)
=AFIELDS(laTempStru)

*--BSiz >> for the Bought line.
*--BAmt >> for the Bought amount.

IF ASCAN(laRPRepTar, "Bought") > 0 .OR. llSelAll
  STORE .T. TO llBought

  *--To be able to print the percent field.
  lnSllThr = lnSllThr + 1

  *--To print the price one time.
  STORE .T. TO llPrnPrce1

  =lfAddField("laTempStru","BSiz1","N",5,0)
  =lfAddField("laTempStru","BSiz2","N",5,0)
  =lfAddField("laTempStru","BSiz3","N",5,0)
  =lfAddField("laTempStru","BSiz4","N",5,0)
  =lfAddField("laTempStru","BSiz5","N",5,0)
  =lfAddField("laTempStru","BSiz6","N",5,0)
  =lfAddField("laTempStru","BSiz7","N",5,0)
  =lfAddField("laTempStru","BSiz8","N",5,0)

  =lfAddField("laTempStru","BAmt1","N",7,0)
  =lfAddField("laTempStru","BAmt2","N",7,0)
  =lfAddField("laTempStru","BAmt3","N",7,0)
  =lfAddField("laTempStru","BAmt4","N",7,0)
  =lfAddField("laTempStru","BAmt5","N",7,0)
  =lfAddField("laTempStru","BAmt6","N",7,0)
  =lfAddField("laTempStru","BAmt7","N",7,0)
  =lfAddField("laTempStru","BAmt8","N",7,0)

  =lfAddField("laTempStru","TotBSiz","N",7,0)
  =lfAddField("laTempStru","TotBAmt","N",12,0)
ENDIF

*--WSiz >> for the Wip line.
*--WAmt >> for the Wip amount.

IF ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll
  STORE .T. TO llWIP

  *--To print the price one time.
  IF !llPrnPrce1
    STORE .T. TO llPrnPrce2
  ENDIF

  =lfAddField("laTempStru","WSiz1","N",5,0)
  =lfAddField("laTempStru","WSiz2","N",5,0)
  =lfAddField("laTempStru","WSiz3","N",5,0)
  =lfAddField("laTempStru","WSiz4","N",5,0)
  =lfAddField("laTempStru","WSiz5","N",5,0)
  =lfAddField("laTempStru","WSiz6","N",5,0)
  =lfAddField("laTempStru","WSiz7","N",5,0)
  =lfAddField("laTempStru","WSiz8","N",5,0)

  =lfAddField("laTempStru","WAmt1","N",7,0)
  =lfAddField("laTempStru","WAmt2","N",7,0)
  =lfAddField("laTempStru","WAmt3","N",7,0)
  =lfAddField("laTempStru","WAmt4","N",7,0)
  =lfAddField("laTempStru","WAmt5","N",7,0)
  =lfAddField("laTempStru","WAmt6","N",7,0)
  =lfAddField("laTempStru","WAmt7","N",7,0)
  =lfAddField("laTempStru","WAmt8","N",7,0)

  =lfAddField("laTempStru","TotWSiz","N",7,0)
  =lfAddField("laTempStru","TotWAmt","N",12,0)
ENDIF

*--SSiz >> for the Stock line.
*--SAmt >> for the Stock amount.

IF ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll
  STORE .T. TO llStock

  *--To print the price one time.
  IF !llPrnPrce1 AND !llPrnPrce2
    STORE .T. TO llPrnPrce3
  ENDIF

  =lfAddField("laTempStru","SSiz1","N",5,0)
  =lfAddField("laTempStru","SSiz2","N",5,0)
  =lfAddField("laTempStru","SSiz3","N",5,0)
  =lfAddField("laTempStru","SSiz4","N",5,0)
  =lfAddField("laTempStru","SSiz5","N",5,0)
  =lfAddField("laTempStru","SSiz6","N",5,0)
  =lfAddField("laTempStru","SSiz7","N",5,0)
  =lfAddField("laTempStru","SSiz8","N",5,0)

  =lfAddField("laTempStru","SAmt1","N",7,0)
  =lfAddField("laTempStru","SAmt2","N",7,0)
  =lfAddField("laTempStru","SAmt3","N",7,0)
  =lfAddField("laTempStru","SAmt4","N",7,0)
  =lfAddField("laTempStru","SAmt5","N",7,0)
  =lfAddField("laTempStru","SAmt6","N",7,0)
  =lfAddField("laTempStru","SAmt7","N",7,0)
  =lfAddField("laTempStru","SAmt8","N",7,0)

  =lfAddField("laTempStru","TotSSiz","N",7,0)
  =lfAddField("laTempStru","TotSAmt","N",12,0)
ENDIF

*--OSiz >> for the Open orders line.
*--OAmt >> for the Open orders amount.

IF ASCAN(laRPRepTar, "Open Orders") > 0  .OR. llSelAll
  STORE .T. TO llOpenOrd

  *--To print the price one time.
  IF !llPrnPrce1 AND !llPrnPrce2 AND !llPrnPrce3
    STORE .T. TO llPrnPrce4
  ENDIF

  =lfAddField("laTempStru","OSiz1","N",5,0)
  =lfAddField("laTempStru","OSiz2","N",5,0)
  =lfAddField("laTempStru","OSiz3","N",5,0)
  =lfAddField("laTempStru","OSiz4","N",5,0)
  =lfAddField("laTempStru","OSiz5","N",5,0)
  =lfAddField("laTempStru","OSiz6","N",5,0)
  =lfAddField("laTempStru","OSiz7","N",5,0)
  =lfAddField("laTempStru","OSiz8","N",5,0)

  =lfAddField("laTempStru","OAmt1","N",7,0)
  =lfAddField("laTempStru","OAmt2","N",7,0)
  =lfAddField("laTempStru","OAmt3","N",7,0)
  =lfAddField("laTempStru","OAmt4","N",7,0)
  =lfAddField("laTempStru","OAmt5","N",7,0)
  =lfAddField("laTempStru","OAmt6","N",7,0)
  =lfAddField("laTempStru","OAmt7","N",7,0)
  =lfAddField("laTempStru","OAmt8","N",7,0)

  =lfAddField("laTempStru","TotOSiz","N",7,0)
  =lfAddField("laTempStru","TotOAmt","N",12,0)
ENDIF

*--ASiz >> for the Available line.
*--AAmt >> for the Available amount.

IF ASCAN(laRPRepTar, "Available") > 0 .OR. llSelAll
  STORE .T. TO llAvalabl

  *--To print the price one time.
  IF !llPrnPrce1 AND !llPrnPrce2 AND !llPrnPrce3 AND !llPrnPrce4
    STORE .T. TO llPrnPrce5
  ENDIF

  =lfAddField("laTempStru","ASiz1","N",5,0)
  =lfAddField("laTempStru","ASiz2","N",5,0)
  =lfAddField("laTempStru","ASiz3","N",5,0)
  =lfAddField("laTempStru","ASiz4","N",5,0)
  =lfAddField("laTempStru","ASiz5","N",5,0)
  =lfAddField("laTempStru","ASiz6","N",5,0)
  =lfAddField("laTempStru","ASiz7","N",5,0)
  =lfAddField("laTempStru","ASiz8","N",5,0)

  =lfAddField("laTempStru","AAmt1","N",7,0)
  =lfAddField("laTempStru","AAmt2","N",7,0)
  =lfAddField("laTempStru","AAmt3","N",7,0)
  =lfAddField("laTempStru","AAmt4","N",7,0)
  =lfAddField("laTempStru","AAmt5","N",7,0)
  =lfAddField("laTempStru","AAmt6","N",7,0)
  =lfAddField("laTempStru","AAmt7","N",7,0)
  =lfAddField("laTempStru","AAmt8","N",7,0)

  =lfAddField("laTempStru","TotASiz","N",7,0)
  =lfAddField("laTempStru","TotAAmt","N",12,0)
ENDIF

*--PSiz >> for the Shipped line.
*--PAmt >> for the Shipped amount.

IF ASCAN(laRPRepTar, "Shipped") > 0 .OR. llSelAll
  STORE .T. TO llShppd

  *--To print the price one time.
  IF !llPrnPrce1 AND !llPrnPrce2 AND !llPrnPrce3 AND !llPrnPrce4 AND !llPrnPrce5
    STORE .T. TO llPrnPrce6
  ENDIF

  =lfAddField("laTempStru","PSiz1","N",5,0)
  =lfAddField("laTempStru","PSiz2","N",5,0)
  =lfAddField("laTempStru","PSiz3","N",5,0)
  =lfAddField("laTempStru","PSiz4","N",5,0)
  =lfAddField("laTempStru","PSiz5","N",5,0)
  =lfAddField("laTempStru","PSiz6","N",5,0)
  =lfAddField("laTempStru","PSiz7","N",5,0)
  =lfAddField("laTempStru","PSiz8","N",5,0)

  =lfAddField("laTempStru","PAmt1","N",7,0)
  =lfAddField("laTempStru","PAmt2","N",7,0)
  =lfAddField("laTempStru","PAmt3","N",7,0)
  =lfAddField("laTempStru","PAmt4","N",7,0)
  =lfAddField("laTempStru","PAmt5","N",7,0)
  =lfAddField("laTempStru","PAmt6","N",7,0)
  =lfAddField("laTempStru","PAmt7","N",7,0)
  =lfAddField("laTempStru","PAmt8","N",7,0)

  =lfAddField("laTempStru","TotPSiz","N",7,0)
  =lfAddField("laTempStru","TotPAmt","N",12,0)
ENDIF

*--TSiz >> for the TTL Sold line.
*--TAmt >> for the TTL Sold amount.

IF ASCAN(laRPRepTar, "TTL Sold") > 0 .OR. llSelAll
  STORE .T. TO llTTLSold

  *--To be able to print the percent field.
  lnSllThr = lnSllThr + 1

  *--To print the price one time.
  IF !llPrnPrce1 AND !llPrnPrce2 AND !llPrnPrce3 AND !llPrnPrce4 AND !llPrnPrce5 AND !llPrnPrce6
    STORE .T. TO llPrnPrce7
  ENDIF

  =lfAddField("laTempStru","TSiz1","N",5,0)
  =lfAddField("laTempStru","TSiz2","N",5,0)
  =lfAddField("laTempStru","TSiz3","N",5,0)
  =lfAddField("laTempStru","TSiz4","N",5,0)
  =lfAddField("laTempStru","TSiz5","N",5,0)
  =lfAddField("laTempStru","TSiz6","N",5,0)
  =lfAddField("laTempStru","TSiz7","N",5,0)
  =lfAddField("laTempStru","TSiz8","N",5,0)

  =lfAddField("laTempStru","TAmt1","N",7,0)
  =lfAddField("laTempStru","TAmt2","N",7,0)
  =lfAddField("laTempStru","TAmt3","N",7,0)
  =lfAddField("laTempStru","TAmt4","N",7,0)
  =lfAddField("laTempStru","TAmt5","N",7,0)
  =lfAddField("laTempStru","TAmt6","N",7,0)
  =lfAddField("laTempStru","TAmt7","N",7,0)
  =lfAddField("laTempStru","TAmt8","N",7,0)

  =lfAddField("laTempStru","TotTSiz","N",7,0)
  =lfAddField("laTempStru","TotTAmt","N",12,0)
ENDIF

*--RSiz >> for the Returns line.
*--RAmt >> for the Returns amount.

IF ASCAN(laRPRepTar, "Returns") > 0 .OR. llSelAll
  STORE .T. TO llReturns

  *--To print the price one time.
  IF !llPrnPrce1 AND !llPrnPrce2 AND !llPrnPrce3 AND !llPrnPrce4 AND !llPrnPrce5 AND !llPrnPrce6 AND !llPrnPrce7
    STORE .T. TO llPrnPrce8
  ENDIF

  =lfAddField("laTempStru","RSiz1","N",5,0)
  =lfAddField("laTempStru","RSiz2","N",5,0)
  =lfAddField("laTempStru","RSiz3","N",5,0)
  =lfAddField("laTempStru","RSiz4","N",5,0)
  =lfAddField("laTempStru","RSiz5","N",5,0)
  =lfAddField("laTempStru","RSiz6","N",5,0)
  =lfAddField("laTempStru","RSiz7","N",5,0)
  =lfAddField("laTempStru","RSiz8","N",5,0)

  =lfAddField("laTempStru","RAmt1","N",7,0)
  =lfAddField("laTempStru","RAmt2","N",7,0)
  =lfAddField("laTempStru","RAmt3","N",7,0)
  =lfAddField("laTempStru","RAmt4","N",7,0)
  =lfAddField("laTempStru","RAmt5","N",7,0)
  =lfAddField("laTempStru","RAmt6","N",7,0)
  =lfAddField("laTempStru","RAmt7","N",7,0)
  =lfAddField("laTempStru","RAmt8","N",7,0)

  =lfAddField("laTempStru","TotRSiz","N",7,0)
  =lfAddField("laTempStru","TotRAmt","N",12,0)
ENDIF

CREATE Table (lcTmpFile) FROM ARRAY laTempStru
ZAP
INDEX ON StyGroup + Divsion + Season + Style TAG (lcTmpFile) OF (gcWorkDir + lcTmpFile + ".CDX")

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Add fields to the the array of file structer.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + 1
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/18/2002
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfcolctDat()
*!*************************************************************
FUNCTION lfcolctDat

STORE SPACE(lnLenth) TO lcHldStyl
STORE SPACE(6) TO lcGrpStyl

SELECT STYLE
IF !(' "S" + STYLE.SCALE ' $ SET('RELATION'))
  SET RELATION TO "S" + STYLE.SCALE INTO SCALE ADDITIVE
ENDIF

SELECT STYLE
SCAN FOR &lcRpExp

  WAIT WINDOW 'Selecting Style(s) For The Report ...' + STYLE.STYLE NOWAIT
  
  *B606828,1 WAB (Start) - to prevent any dublication in the Style file
  lcExCHeck= IIF(lcRPSortBy='S','ALLGRP'+'ALLDIV'+'ALLSEA',CSTYGROUP+CDIVISION+SEASON)

  IF SEEK(lcExCheck+Style,lcTmpFile)
    LOOP
  ENDIF
  *B606828,1 WAB (End)
  
  *--To use this field in printing the Style total in the DOS frx.
  IF lcHldStyl # LEFT(STYLE.STYLE,lnLenth) AND !EMPTY(lcHldStyl)
    SELECT (lcTmpFile)
    REPLACE llPrnTotSt WITH .T.
  ENDIF
  lcHldStyl = LEFT(STYLE.STYLE,lnLenth)

  SELECT (lcTmpFile)
  APPEND BLANK
  *B606784,1 -WAB (Start) - in case of sorting by style no need to print total for group of for diviision
  *B606784,1			  - So that we have to fill all the Group,Season,Division with the same value
  *B606784,1			  - So All records will be Like the same Group/Season/Division
  *REPLACE STYLE    WITH STYLE.STYLE     ,;
  *        ScalTmp  WITH STYLE.SCALE     ,;
  *        SEASON   WITH STYLE.SEASON    ,;
  *        DIVSION  WITH STYLE.CDIVISION ,;
  *        STYGROUP WITH STYLE.CSTYGROUP ,;
  *        PricA    WITH STYLE.PRICEA
  REPLACE STYLE    WITH STYLE.STYLE     ,;
          ScalTmp  WITH STYLE.SCALE     ,;
          SEASON   WITH IIF(lcRPSortBy='G',STYLE.SEASON,'ALLSEA')    ,;
          DIVSION  WITH IIF(lcRPSortBy='G',STYLE.CDIVISION,'ALLDIV') ,;
          STYGROUP WITH IIF(lcRPSortBy='G',STYLE.CSTYGROUP,'ALLGRP') ,;
          PricA    WITH STYLE.PRICEA
  *B606784,1 -WAB (End)
  *--BSiz >> for the Bought line.
  *--BAmt >> for the Bought amount.
  IF ASCAN(laRPRepTar, "Bought") > 0 .OR. llSelAll
    SELECT STYLE
    IF !('STYLE.STYLE' $ SET('RELATION'))
      SET RELATION TO STYLE.STYLE INTO POSLN   ADDITIVE
      SET RELATION TO STYLE.STYLE INTO CUTTKTL ADDITIVE
       SELECT POSLN
      SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE
      SELECT CUTTKTL
      SET RELATION TO CUTTKTL.CUTTKT INTO CUTTKTH ADDITIVE
  ENDIF
    *B606784,1 -WAB (Start)-Comment the Next lines and Rewrited again because it is wrong calculation for Open qty
    *IF !STYLE.MAKE
    *  =SEEK(STYLE.STYLE , "POSLN")
    *  SELECT POSLN
    *  SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = STYLE.STYLE FOR POSHDR.STATUS = "O"
    *    IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , POSHDR.ENTERED  = POSHDR.ENTERED ,;
    *      IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , POSHDR.ENTERED <= ldRPPHDate , BETWEEN(POSHDR.ENTERED,ldRPPLDate,ldRPPHDate)))
    *      IF POSLN.Trancd = "1"
    *        SELECT (lcTmpFile)
    *        REPLACE llPrceBot WITH .T.
    *        REPLACE BSiz1 WITH BSiz1 + POSLN.QTY1                   ,;
    *                BSiz2 WITH BSiz2 + POSLN.QTY2                   ,;
    *                BSiz3 WITH BSiz3 + POSLN.QTY3                   ,;
    *                BSiz4 WITH BSiz4 + POSLN.QTY4                   ,;
    *                BSiz5 WITH BSiz5 + POSLN.QTY5                   ,;
    *                BSiz6 WITH BSiz6 + POSLN.QTY6                   ,;
    *                BSiz7 WITH BSiz7 + POSLN.QTY7                   ,;
    *                BSiz8 WITH BSiz8 + POSLN.QTY8                   ,;
    *                BAmt1 WITH BAmt1 + (POSLN.QTY1 * STYLE.PRICEA ) ,;
    *                BAmt2 WITH BAmt2 + (POSLN.QTY2 * STYLE.PRICEA ) ,;
    *                BAmt3 WITH BAmt3 + (POSLN.QTY3 * STYLE.PRICEA ) ,;
    *                BAmt4 WITH BAmt4 + (POSLN.QTY4 * STYLE.PRICEA ) ,;
    *                BAmt5 WITH BAmt5 + (POSLN.QTY5 * STYLE.PRICEA ) ,;
    *                BAmt6 WITH BAmt6 + (POSLN.QTY6 * STYLE.PRICEA ) ,;
    *                BAmt7 WITH BAmt7 + (POSLN.QTY7 * STYLE.PRICEA ) ,;
    *                BAmt8 WITH BAmt8 + (POSLN.QTY8 * STYLE.PRICEA )
    *        REPLACE TotBSiz WITH TotBSiz + BSiz1 + BSiz2 + BSiz3 + BSiz4 + BSiz5 + BSiz6 + BSiz7 + BSiz8 ,;
    *                TotBAmt WITH TotBAmt + BAmt1 + BAmt2 + BAmt3 + BAmt4 + BAmt5 + BAmt6 + BAmt7 + BAmt8
    *      ENDIF
    *    ENDIF
    *  ENDSCAN
    *ENDIF
    *IF STYLE.MAKE
    *  =SEEK(STYLE.STYLE , "CUTTKTL")
    *  SELECT CUTTKTL
    *  SCAN REST WHILE STYLE + CUTTKT + TRANCD = STYLE.STYLE FOR CUTTKTH.STATUS = "O"
    *    IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , CUTTKTH.ENTERED  = CUTTKTH.ENTERED ,;
    *      IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , CUTTKTH.ENTERED <= ldRPPHDate , BETWEEN(CUTTKTH.ENTERED,ldRPPLDate,ldRPPHDate)))
    *      IF CUTTKTL.Trancd = "1"
    *        SELECT (lcTmpFile)
    *        REPLACE llPrceBot WITH .T.
    *        REPLACE BSiz1 WITH BSiz1 + CUTTKTL.QTY1                   ,;
    *                BSiz2 WITH BSiz2 + CUTTKTL.QTY2                   ,;
    *                BSiz3 WITH BSiz3 + CUTTKTL.QTY3                   ,;
    *                BSiz4 WITH BSiz4 + CUTTKTL.QTY4                   ,;
    *                BSiz5 WITH BSiz5 + CUTTKTL.QTY5                   ,;
    *                BSiz6 WITH BSiz6 + CUTTKTL.QTY6                   ,;
    *                BSiz7 WITH BSiz7 + CUTTKTL.QTY7                   ,;
    *                BSiz8 WITH BSiz8 + CUTTKTL.QTY8                   ,;
    *                BAmt1 WITH BAmt1 + (CUTTKTL.QTY1 * STYLE.PRICEA ) ,;
    *                BAmt2 WITH BAmt2 + (CUTTKTL.QTY2 * STYLE.PRICEA ) ,;
    *                BAmt3 WITH BAmt3 + (CUTTKTL.QTY3 * STYLE.PRICEA ) ,;
    *                BAmt4 WITH BAmt4 + (CUTTKTL.QTY4 * STYLE.PRICEA ) ,;
    *                BAmt5 WITH BAmt5 + (CUTTKTL.QTY5 * STYLE.PRICEA ) ,;
    *                BAmt6 WITH BAmt6 + (CUTTKTL.QTY6 * STYLE.PRICEA ) ,;
    *                BAmt7 WITH BAmt7 + (CUTTKTL.QTY7 * STYLE.PRICEA ) ,;
    *                BAmt8 WITH BAmt8 + (CUTTKTL.QTY8 * STYLE.PRICEA )
    *        REPLACE TotBSiz WITH BSiz1 + BSiz2 + BSiz3 + BSiz4 + BSiz5 + BSiz6 + BSiz7 + BSiz8 ,;
    *                TotBAmt WITH BAmt1 + BAmt2 + BAmt3 + BAmt4 + BAmt5 + BAmt6 + BAmt7 + BAmt8
    *      ENDIF
    *    ENDIF
    *  ENDSCAN
    *--Decale Variabel to hold the Open Qty
    STORE 0 TO lnBghtQty1,lnBghtQty2,lnBghtQty3,lnBghtQty4,lnBghtQty5,lnBghtQty6,lnBghtQty7,lnBghtQty8
    IF !STYLE.MAKE .AND. SEEK(STYLE.STYLE , "POSLN")
      SELECT POSLN
      *--we will scan on the TranCd = '145' and execlude '2' becasue as per B606784
      *--the Bought field = Total Recipt + open WIP
      *B606828,1 WAB (Start) - get only the Receipt qty and we will add the WIP after calucalationg it 
      *SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = STYLE.STYLE  ;
      *     FOR POSHDR.STATUS = "O" AND POSLN.TranCD $ '145'
      SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = STYLE.STYLE  ;
           FOR  POSLN.TranCD = '2'
      *B606828,1 WAB (End) 
           
        IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , POSHDR.ENTERED  = POSHDR.ENTERED ,;
           IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , POSHDR.ENTERED <= ldRPPHDate , BETWEEN(POSHDR.ENTERED,ldRPPLDate,ldRPPHDate)))
           
          *B606828,1 WAB (Start) - no need to this variable becasue we will get the reciept qty only
          *lnSign = IIF(POSLN.TranCD = '1',1,-1)
          *B606828,1 WAB (End) 
          
          FOR lnCount = 1 TO SCALE.Cnt
            lcI = STR(lnCount,1)
            *B606828,1 WAB (Start) - only the receipt qty
            *lnBghtQty&lcI = lnBghtQty&lcI + (POSLN.QTY&lcI * lnSign)
            lnBghtQty&lcI = lnBghtQty&lcI + POSLN.QTY&lcI 
            *B606828,1 WAB (End) 
            
          ENDFOR
        ENDIF  
      ENDSCAN 
    ENDIF
    IF STYLE.MAKE .AND. SEEK(STYLE.STYLE , "CUTTKTL")
      SELECT CUTTKTL
      *--we will scan on the TranCd = '134' and execlude '2' becasue as per B606784
      *--the Bought field = Total Recipt + open WIP
      *B606828,1 WAB (Start) - we will gte only the receipt qty and then we will add the WIP qty after get it
      *SCAN REST WHILE STYLE + CUTTKT + TRANCD = STYLE.STYLE FOR CUTTKTH.STATUS = "O" AND CUTTKTL.TranCD # '2'
      SCAN REST WHILE STYLE + CUTTKT + TRANCD = STYLE.STYLE FOR  CUTTKTL.TranCD = '2'
      *B606828,1 WAB (End)  
      
        IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , CUTTKTH.ENTERED  = CUTTKTH.ENTERED ,;
          IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , CUTTKTH.ENTERED <= ldRPPHDate , BETWEEN(CUTTKTH.ENTERED,ldRPPLDate,ldRPPHDate)))
          
          *B606828,1 WAB (Start)  - No need to this variable
          *lnSign = IIF(CUTTKTL.TranCD = '1',1,-1)
          *B606828,1 WAB (End)  
          
          FOR lnCount = 1 TO SCALE.Cnt
            lcI = STR(lnCount,1)
            
            *B606828,1 WAB (Start)  
            *lnBghtQty&lcI = lnBghtQty&lcI + (CUTTKTL.Qty&lcI * lnSign)
            lnBghtQty&lcI = lnBghtQty&lcI + CUTTKTL.Qty&lcI 
            *B606828,1 WAB (End)  
          ENDFOR
        ENDIF
      ENDSCAN 
    ENDIF
    SELECT (lcTmpFile)
    REPLACE llPrceBot WITH .T.							  ,;
            BSiz1 WITH BSiz1 + lnBghtQty1                   ,;
            BSiz2 WITH BSiz2 + lnBghtQty2                   ,;
            BSiz3 WITH BSiz3 + lnBghtQty3                   ,;
            BSiz4 WITH BSiz4 + lnBghtQty4                   ,;
            BSiz5 WITH BSiz5 + lnBghtQty5                   ,;
            BSiz6 WITH BSiz6 + lnBghtQty6                   ,;
            BSiz7 WITH BSiz7 + lnBghtQty7                   ,;
            BSiz8 WITH BSiz8 + lnBghtQty8                   ,;
            BAmt1 WITH BAmt1 + (lnBghtQty1 * STYLE.PRICEA ) ,;
            BAmt2 WITH BAmt2 + (lnBghtQty2 * STYLE.PRICEA ) ,;
            BAmt3 WITH BAmt3 + (lnBghtQty3 * STYLE.PRICEA ) ,;
            BAmt4 WITH BAmt4 + (lnBghtQty4 * STYLE.PRICEA ) ,;
            BAmt5 WITH BAmt5 + (lnBghtQty5 * STYLE.PRICEA ) ,;
            BAmt6 WITH BAmt6 + (lnBghtQty6 * STYLE.PRICEA ) ,;
            BAmt7 WITH BAmt7 + (lnBghtQty7 * STYLE.PRICEA ) ,;
            BAmt8 WITH BAmt8 + (lnBghtQty8 * STYLE.PRICEA ) ,;
            TotBSiz WITH  BSiz1 + BSiz2 + BSiz3 + BSiz4 + BSiz5 + BSiz6 + BSiz7 + BSiz8 ,;
            TotBAmt WITH  BAmt1 + BAmt2 + BAmt3 + BAmt4 + BAmt5 + BAmt6 + BAmt7 + BAmt8
     *B606784,1 -WAB (End)
  
    *--Section break the relations for this section.
    SELECT STYLE
    SET RELATION TO
    SELECT POSLN
    SET RELATION TO
    SELECT CUTTKTL
    SET RELATION TO
  ENDIF

  *--WSiz >> for the Wip line.
  *--WAmt >> for the Wip amount.
  *B606784,1 -WAB (Start)-Comment the Next lines and Rewrited again because it is wrong calculation for Open qty
  **IF ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll  
    *SELECT STYLE
    *IF !('STYLE.STYLE' $ SET('RELATION'))
    *  SET RELATION TO STYLE.STYLE INTO POSLN   ADDITIVE
    *  SELECT POSLN
    *  SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE
    *ENDIF
    *IF SEEK(STYLE.STYLE , "POSLN")
    *  SELECT POSLN
    *  SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = STYLE.STYLE FOR POSHDR.STATUS = "O" AND POSLN.TRANCD = "1"
    *    IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , POSHDR.ENTERED  = POSHDR.ENTERED ,;
    *      IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , POSHDR.ENTERED <= ldRPPHDate , BETWEEN(POSHDR.ENTERED,ldRPPLDate,ldRPPHDate)))
    *      SELECT (lcTmpFile)
    *      REPLACE llPrceWip WITH .T.
    *      REPLACE WSiz1 WITH WSiz1 + STYLE.WIP1                   ,;
    *              WSiz2 WITH WSiz2 + STYLE.WIP2                   ,;
    *              WSiz3 WITH WSiz3 + STYLE.WIP3                   ,;
    *              WSiz4 WITH WSiz4 + STYLE.WIP4                   ,;
    *              WSiz5 WITH WSiz5 + STYLE.WIP5                   ,;
    *              WSiz6 WITH WSiz6 + STYLE.WIP6                   ,;
    *              WSiz7 WITH WSiz7 + STYLE.WIP7                   ,;
    *              WSiz8 WITH WSiz8 + STYLE.WIP8                   ,;
    *              WAmt1 WITH WAmt1 + (STYLE.WIP1 * STYLE.PRICEA ) ,;
    *              WAmt2 WITH WAmt2 + (STYLE.WIP2 * STYLE.PRICEA ) ,;
    *              WAmt3 WITH WAmt3 + (STYLE.WIP3 * STYLE.PRICEA ) ,;
    *              WAmt4 WITH WAmt4 + (STYLE.WIP4 * STYLE.PRICEA ) ,;
    *              WAmt5 WITH WAmt5 + (STYLE.WIP5 * STYLE.PRICEA ) ,;
    *              WAmt6 WITH WAmt6 + (STYLE.WIP6 * STYLE.PRICEA ) ,;
    *              WAmt7 WITH WAmt7 + (STYLE.WIP7 * STYLE.PRICEA ) ,;
    *              WAmt8 WITH WAmt8 + (STYLE.WIP8 * STYLE.PRICEA )
    *      REPLACE TotWSiz WITH TotWSiz + WSiz1 + WSiz2 + WSiz3 + WSiz4 + WSiz5 + WSiz6 + WSiz7 + WSiz8 ,;
    *              TotWAmt WITH TotWAmt + WAmt1 + WAmt2 + WAmt3 + WAmt4 + WAmt5 + WAmt6 + WAmt7 + WAmt8
    *    ENDIF
    *  ENDSCAN
    *ENDIF
  *--in case of the user select the Availble field we have to calculate the filed  
  IF ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll  .OR. ASCAN(laRPRepTar, "Available") > 0  .OR. ;
     ASCAN(laRPRepTar, "Bought") > 0 
    SELECT STYLE
    IF !('STYLE.STYLE' $ SET('RELATION'))
      SET RELATION TO STYLE.STYLE INTO POSLN   ADDITIVE
      SET RELATION TO STYLE.STYLE INTO CUTTKTL ADDITIVE
 
      SELECT POSLN
      SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE

      SELECT CUTTKTL
      SET RELATION TO CUTTKTL.CUTTKT INTO CUTTKTH ADDITIVE
    ENDIF
    *B606828,1 WAB (Start) - we have to calucltae the WIP correctyl and handle the case of the 
    *B606828,1 WAB 		   - Over Recieve so we have to calulate every WIP of each PO indiveduale
    *IF !STYLE.MAKE .AND. SEEK(STYLE.STYLE , "POSLN")
    *  SELECT POSLN
    *  SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = STYLE.STYLE  ;
    *       FOR POSHDR.STATUS = "O" AND POSLN.TranCD $ '1245'
    *    IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , POSHDR.ENTERED  = POSHDR.ENTERED ,;
    *      IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , POSHDR.ENTERED <= ldRPPHDate , BETWEEN(POSHDR.ENTERED,ldRPPLDate,ldRPPHDate)))
    *      
    *      lnSign = IIF(POSLN.TranCD = '1',1,-1)
    *      FOR lnCount = 1 TO 8
    *        lcI = STR(lnCount,1)
    *        lnWIPQty&lcI = lnWIPQty&lcI + (POSLN.QTY&lcI * lnSign)
    *      ENDFOR
    *    ENDIF  
    *  ENDSCAN 
    *ENDIF
    *IF STYLE.MAKE .AND. SEEK(STYLE.STYLE , "CUTTKTL")
    *  SELECT CUTTKTL
    *  SCAN REST WHILE STYLE + CUTTKT + TRANCD = STYLE.STYLE FOR CUTTKTH.STATUS = "O" AND CUTTKTL.TranCD $ '12345'
    *  IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , CUTTKTH.ENTERED  = CUTTKTH.ENTERED ,;
    *      IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , CUTTKTH.ENTERED <= ldRPPHDate , BETWEEN(CUTTKTH.ENTERED,ldRPPLDate,ldRPPHDate)))
    *      lnSign = IIF(CUTTKTL.TranCD = '1',1,-1)
    *      FOR lnCount = 1 TO 8
    *        lcI = STR(lnCount,1)
    *        lnWIPQty&lcI = lnWIPQty&lcI + (CUTTKTL.Qty&lcI * lnSign)
    *      ENDFOR
    *    ENDIF
    *  ENDSCAN 
    *ENDIF
    STORE 0 TO lnBghtQty1,lnBghtQty2,lnBghtQty3,lnBghtQty4,lnBghtQty5,lnBghtQty6,lnBghtQty7,lnBghtQty8
    STORE 0 TO lnWIPQty1,lnWIPQty2,lnWIPQty3,lnWIPQty4,lnWIPQty5,lnWIPQty6,lnWIPQty7,lnWIPQty8
    IF !STYLE.MAKE .AND. SEEK(STYLE.STYLE , "POSLN")
      SELECT POSLN
      lcStyle = STyle 
      DO WHILE STYLE = lcStyle
        lcPoNo = PO
        Store 0 TO lnPoWip1,lnPoWip2,lnPoWip3,lnPoWip4,lnPoWip5,lnPoWip6,lnPoWip7,lnPoWip8
        SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = lcStyle+'P'+lcPoNO  ;
                  FOR   POSHDR.STATUS $ "HO" AND POSLN.TranCD $ '1245'
          IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , POSHDR.ENTERED  = POSHDR.ENTERED ,;
             IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , POSHDR.ENTERED <= ldRPPHDate , BETWEEN(POSHDR.ENTERED,ldRPPLDate,ldRPPHDate)))
            FOR lnCount = 1 TO SCALE.Cnt
              lcI = STR(lnCount,1)
              IF Posln.TranCD = '1'
                lnPoWip&lcI = lnPoWip&lcI + POSLN.QTY&lcI 
              ELSE
                lnPoWip&lcI = lnPoWip&lcI - POSLN.QTY&lcI 
              ENDIF
            ENDFOR
          ENDIF 
        ENDSCAN
        FOR lnCount = 1 TO Scale.Cnt
          lcI = STR(lnCount,1)
          lnWIPQty&lcI = lnWIPQty&lcI + MAX(lnPoWip&lcI,0)
        ENDFOR
      ENDDO  
    ENDIF
    IF STYLE.MAKE .AND. SEEK(STYLE.STYLE , "CUTTKTL")
      SELECT CUTTKTL
      lcStyle = Style 
      DO WHILE Style = lcStyle
        lcCutktNo = CUTTKT
        Store 0 TO lnCUTWip1,lnCUTWip2,lnCUTWip3,lnCUTWip4,lnCUTWip5,lnCUTWip6,lnCUTWip7,lnCUTWip8
        SCAN REST WHILE STYLE + CUTTKT + TRANCD = lcStyle+lcCutKtNo  FOR !(CUTTKTH.STATUS $ "CX")
          IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , CUTTKTH.ENTERED  = CUTTKTH.ENTERED ,;
            IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , CUTTKTH.ENTERED <= ldRPPHDate , BETWEEN(CUTTKTH.ENTERED,ldRPPLDate,ldRPPHDate)))
            FOR lnCount = 1 TO Scale.Cnt
              lcI = STR(lnCount,1)
              IF CUTTKTL.TranCD = '1'
                lnCUTWip&lcI = lnCUTWip&lcI + CUTTKTL.QTY&lcI 
              ELSE
                lnCUTWip&lcI = lnCUTWip&lcI - CUTTKTL.QTY&lcI 
              ENDIF
            ENDFOR
          ENDIF
        ENDSCAN 
        FOR lnCount = 1 TO Scale.Cnt
          lcI = STR(lnCount,1)
          lnWIPQty&lcI = lnWIPQty&lcI + MAX(lnCUTWip&lcI,0)
        ENDFOR
      ENDDO
    ENDIF
    *B606828,1 WAB (End) 

    SELECT (lcTmpFile)
    IF ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll
      REPLACE llPrceWip WITH .T.
      REPLACE WSiz1   WITH WSiz1 + lnWIPQty1                   ,;
              WSiz2   WITH WSiz2 + lnWIPQty2                   ,;
              WSiz3   WITH WSiz3 + lnWIPQty3                   ,;
              WSiz4   WITH WSiz4 + lnWIPQty4                   ,;
              WSiz5   WITH WSiz5 + lnWIPQty5                   ,;
              WSiz6   WITH WSiz6 + lnWIPQty6                   ,;
              WSiz7   WITH WSiz7 + lnWIPQty7                   ,;
              WSiz8   WITH WSiz8 + lnWIPQty8                   ,;
              WAmt1   WITH WAmt1 + (lnWIPQty1 * STYLE.PRICEA ) ,;
              WAmt2   WITH WAmt2 + (lnWIPQty2 * STYLE.PRICEA ) ,;
              WAmt3   WITH WAmt3 + (lnWIPQty3 * STYLE.PRICEA ) ,;
              WAmt4   WITH WAmt4 + (lnWIPQty4 * STYLE.PRICEA ) ,;
              WAmt5   WITH WAmt5 + (lnWIPQty5 * STYLE.PRICEA ) ,;
              WAmt6   WITH WAmt6 + (lnWIPQty6 * STYLE.PRICEA ) ,;
              WAmt7   WITH WAmt7 + (lnWIPQty7 * STYLE.PRICEA ) ,;
              WAmt8   WITH WAmt8 + (lnWIPQty8 * STYLE.PRICEA ) ,;
              TotWSiz WITH WSiz1 + WSiz2 + WSiz3 + WSiz4 + WSiz5 + WSiz6 + WSiz7 + WSiz8 ,;
              TotWAmt WITH WAmt1 + WAmt2 + WAmt3 + WAmt4 + WAmt5 + WAmt6 + WAmt7 + WAmt8
    ENDIF
    *--if the user select the Available qty wwe have to add the WIP to the Asiz* fields
    IF ASCAN(laRPRepTar, "Available") > 0 .OR. llSelAll
      REPLACE llPrceAvlb WITH .T.
      REPLACE ASiz1   WITH ASiz1  + lnWIPQty1                   ,;
              ASiz2   WITH ASiz2 + lnWIPQty2                   ,;
              ASiz3   WITH ASiz3 + lnWIPQty3                   ,;
              ASiz4   WITH ASiz4 + lnWIPQty4                   ,;
              ASiz5   WITH ASiz5 + lnWIPQty5                   ,;
              ASiz6   WITH ASiz6 + lnWIPQty6                   ,;
              ASiz7   WITH ASiz7 + lnWIPQty7                   ,;
              ASiz8   WITH ASiz8 + lnWIPQty8                   ,;
              AAmt1   WITH AAmt1 + (lnWIPQty1 * STYLE.PRICEA ) ,;
              AAmt2   WITH AAmt2 + (lnWIPQty2 * STYLE.PRICEA ) ,;
              AAmt3   WITH AAmt3 + (lnWIPQty3 * STYLE.PRICEA ) ,;
              AAmt4   WITH AAmt4 + (lnWIPQty4 * STYLE.PRICEA ) ,;
              AAmt5   WITH AAmt5 + (lnWIPQty5 * STYLE.PRICEA ) ,;
              AAmt6   WITH AAmt6 + (lnWIPQty6 * STYLE.PRICEA ) ,;
              AAmt7   WITH AAmt7 + (lnWIPQty7 * STYLE.PRICEA ) ,;
              AAmt8   WITH AAmt8 + (lnWIPQty8 * STYLE.PRICEA ) ,;
              TotASiz WITH ASiz1 + ASiz2 + ASiz3 + ASiz4 + ASiz5 + ASiz6 + ASiz7 + ASiz8 ,;
              TotAAmt WITH AAmt1 + AAmt2 + AAmt3 + AAmt4 + AAmt5 + AAmt6 + AAmt7 + AAmt8
     
    ENDIF
    *B606828,1 WAB (Start) - Now we will add the WIP to the Bought fields 
    IF ASCAN(laRPRepTar, "Bought") > 0 .OR. llSelAll
      SELECT (lcTmpFile)
      REPLACE llPrceBot WITH .T.							  ,;
              BSiz1 WITH BSiz1 + lnWIPQty1                   ,;
              BSiz2 WITH BSiz2 + lnWIPQty2                   ,;
              BSiz3 WITH BSiz3 + lnWIPQty3                   ,;
              BSiz4 WITH BSiz4 + lnWIPQty4                   ,;
              BSiz5 WITH BSiz5 + lnWIPQty5                   ,;
              BSiz6 WITH BSiz6 + lnWIPQty6                   ,;
              BSiz7 WITH BSiz7 + lnWIPQty7                   ,;
              BSiz8 WITH BSiz8 + lnWIPQty8                   ,;
              BAmt1 WITH BAmt1 + (lnWIPQty1 * STYLE.PRICEA ) ,;
              BAmt2 WITH BAmt2 + (lnWIPQty2 * STYLE.PRICEA ) ,;
              BAmt3 WITH BAmt3 + (lnWIPQty3 * STYLE.PRICEA ) ,;
              BAmt4 WITH BAmt4 + (lnWIPQty4 * STYLE.PRICEA ) ,;
              BAmt5 WITH BAmt5 + (lnWIPQty5 * STYLE.PRICEA ) ,;
              BAmt6 WITH BAmt6 + (lnWIPQty6 * STYLE.PRICEA ) ,;
              BAmt7 WITH BAmt7 + (lnWIPQty7 * STYLE.PRICEA ) ,;
              BAmt8 WITH BAmt8 + (lnWIPQty8 * STYLE.PRICEA ) ,;
              TotBSiz WITH  BSiz1 + BSiz2 + BSiz3 + BSiz4 + BSiz5 + BSiz6 + BSiz7 + BSiz8 ,;
              TotBAmt WITH  BAmt1 + BAmt2 + BAmt3 + BAmt4 + BAmt5 + BAmt6 + BAmt7 + BAmt8
    ENDIF
    *B606828,1 WAB (End) 

    *--Section break the relations for this section.
    SELECT STYLE
    SET RELATION TO
    SELECT POSLN
    SET RELATION TO
    SELECT CUTTKTL
    SET RELATION TO
  ENDIF
  *B606784,1 -WAB (END)
  
  *--SSiz >> for the Stock line.
  *--SAmt >> for the Stock amount.
  IF ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll
    SELECT (lcTmpFile)
    REPLACE llPrceStk WITH .T.
    REPLACE SSiz1 WITH STYLE.STK1                   ,;
            SSiz2 WITH STYLE.STK2                   ,;
            SSiz3 WITH STYLE.STK3                   ,;
            SSiz4 WITH STYLE.STK4                   ,;
            SSiz5 WITH STYLE.STK5                   ,;
            SSiz6 WITH STYLE.STK6                   ,;
            SSiz7 WITH STYLE.STK7                   ,;
            SSiz8 WITH STYLE.STK8                   ,;
            SAmt1 WITH (STYLE.STK1 * STYLE.PRICEA ) ,;
            SAmt2 WITH (STYLE.STK2 * STYLE.PRICEA ) ,;
            SAmt3 WITH (STYLE.STK3 * STYLE.PRICEA ) ,;
            SAmt4 WITH (STYLE.STK4 * STYLE.PRICEA ) ,;
            SAmt5 WITH (STYLE.STK5 * STYLE.PRICEA ) ,;
            SAmt6 WITH (STYLE.STK6 * STYLE.PRICEA ) ,;
            SAmt7 WITH (STYLE.STK7 * STYLE.PRICEA ) ,;
            SAmt8 WITH (STYLE.STK8 * STYLE.PRICEA )

    REPLACE TotSSiz WITH SSiz1 + SSiz2 + SSiz3 + SSiz4 + SSiz5 + SSiz6 + SSiz7 + SSiz8 ,;
            TotSAmt WITH SAmt1 + SAmt2 + SAmt3 + SAmt4 + SAmt5 + SAmt6 + SAmt7 + SAmt8
  ENDIF

  *--OSiz >> for the Open orders line.
  *--OAmt >> for the Open orders amount.
  *B606784,1 -WAB (Start)-Comment the Next lines and Rewrited again because we want to calculate the availbel qty also
  *IF ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll 
  *  SELECT STYLE
  *  IF !('STYLE.STYLE' $ SET('RELATION'))
  *    SET RELATION TO STYLE.STYLE INTO ORDLINE ADDITIVE
  *    SELECT ORDLINE
  *    SET RELATION TO "O" + ORDLINE.ORDER INTO ORDHDR ADDITIVE
  *  ENDIF
  *  IF SEEK(STYLE.STYLE , "ORDLINE")
  *    SELECT ORDLINE
  *    SCAN REST WHILE STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER + STORE + STR(LINENO,6) = STYLE.STYLE FOR ORDHDR.STATUS $ "OH"
  *      IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , ORDHDR.ENTERED  = ORDHDR.ENTERED ,;
  *        IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , ORDHDR.ENTERED <= ldRPPHDate , BETWEEN(ORDHDR.ENTERED,ldRPPLDate,ldRPPHDate)))
  *        SELECT (lcTmpFile)
  *        REPLACE llPrceOord WITH .T.
  *        REPLACE OSiz1 WITH OSiz1 + ORDLINE.QTY1                    ,;
  *                OSiz2 WITH OSiz2 + ORDLINE.QTY2                    ,;
  *                OSiz3 WITH OSiz3 + ORDLINE.QTY3                    ,;
  *                OSiz4 WITH OSiz4 + ORDLINE.QTY4                    ,;
  *                OSiz5 WITH OSiz5 + ORDLINE.QTY5                    ,;
  *                OSiz6 WITH OSiz6 + ORDLINE.QTY6                    ,;
  *                OSiz7 WITH OSiz7 + ORDLINE.QTY7                    ,;
  *                OSiz8 WITH OSiz8 + ORDLINE.QTY8                    ,;
  *                OAmt1 WITH OAmt1 + (ORDLINE.QTY1 * ORDLINE.PRICE ) ,;
  *                OAmt2 WITH OAmt2 + (ORDLINE.QTY2 * ORDLINE.PRICE ) ,;
  *                OAmt3 WITH OAmt3 + (ORDLINE.QTY3 * ORDLINE.PRICE ) ,;
  *                OAmt4 WITH OAmt4 + (ORDLINE.QTY4 * ORDLINE.PRICE ) ,;
  *                OAmt5 WITH OAmt5 + (ORDLINE.QTY5 * ORDLINE.PRICE ) ,;
  *                OAmt6 WITH OAmt6 + (ORDLINE.QTY6 * ORDLINE.PRICE ) ,;
  *                OAmt7 WITH OAmt7 + (ORDLINE.QTY7 * ORDLINE.PRICE ) ,;
  *                OAmt8 WITH OAmt8 + (ORDLINE.QTY8 * ORDLINE.PRICE )
  *        REPLACE TotOSiz WITH TotOSiz + OSiz1 + OSiz2 + OSiz3 + OSiz4 + OSiz5 + OSiz6 + OSiz7 + OSiz8 ,;
  *                TotOAmt WITH TotOAmt + OAmt1 + OAmt2 + OAmt3 + OAmt4 + OAmt5 + OAmt6 + OAmt7 + OAmt8
  *    ENDSCAN
  *  ENDIF
  *  *--Section break the relations for this section.
  *  SELECT STYLE
  *  SET RELATION TO
  *  SELECT ORDLINE
  *  SET RELATION TO
  *ENDIF
  *--calculate the availbe qty also  
  IF ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll .OR. ASCAN(laRPRepTar, "Available") > 0 
    SELECT STYLE
    IF !('STYLE.STYLE' $ SET('RELATION'))
      SET RELATION TO STYLE.STYLE INTO ORDLINE ADDITIVE

      SELECT ORDLINE
      SET RELATION TO "O" + ORDLINE.ORDER INTO ORDHDR ADDITIVE
    ENDIF

    IF SEEK(STYLE.STYLE , "ORDLINE")
      SELECT ORDLINE
      STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8
      STORE 0 TO lnOrdAmt1,lnOrdAmt2,lnOrdAmt3,lnOrdAmt4,lnOrdAmt5,lnOrdAmt6,lnOrdAmt7,lnOrdAmt8
      SCAN REST WHILE STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER + STORE + STR(LINENO,6) = STYLE.STYLE FOR ORDHDR.STATUS $ "OH"

        IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , ORDHDR.ENTERED  = ORDHDR.ENTERED ,;
          IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , ORDHDR.ENTERED <= ldRPPHDate , BETWEEN(ORDHDR.ENTERED,ldRPPLDate,ldRPPHDate)))
          FOR lnCount = 1 TO Scale.Cnt
            lcI = STR(lnCount,1)
            lnOrdQty&lcI = lnOrdQty&lcI + ORDLINE.QTY&lcI
            lnOrdAmt&lcI = lnOrdAmt&lcI + (ORDLINE.QTY&lcI * ORDLINE.PRICE )
          ENDFOR
        ENDIF
      ENDSCAN
      IF ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll 
        SELECT (lcTmpFile)
        REPLACE llPrceOord WITH .T.
        REPLACE OSiz1   WITH OSiz1 + lnOrdQty1 ,;
                OSiz2   WITH OSiz2 + lnOrdQty2 ,;
                OSiz3   WITH OSiz3 + lnOrdQty3 ,;
                OSiz4   WITH OSiz4 + lnOrdQty4 ,;
                OSiz5   WITH OSiz5 + lnOrdQty5 ,;
                OSiz6   WITH OSiz6 + lnOrdQty6 ,;
                OSiz7   WITH OSiz7 + lnOrdQty7 ,;
                OSiz8   WITH OSiz8 + lnOrdQty8 ,;
                OAmt1   WITH OAmt1 + lnOrdAmt1 ,;
                OAmt2   WITH OAmt2 + lnOrdAmt2 ,;
                OAmt3   WITH OAmt3 + lnOrdAmt3 ,;
                OAmt4   WITH OAmt4 + lnOrdAmt4 ,;
                OAmt5   WITH OAmt5 + lnOrdAmt5 ,;
                OAmt6   WITH OAmt6 + lnOrdAmt6 ,;
                OAmt7   WITH OAmt7 + lnOrdAmt7 ,;
                OAmt8   WITH OAmt8 + lnOrdAmt8 ,;
                TotOSiz WITH OSiz1 + OSiz2 + OSiz3 + OSiz4 + OSiz5 + OSiz6 + OSiz7 + OSiz8 ,;
                TotOAmt WITH OAmt1 + OAmt2 + OAmt3 + OAmt4 + OAmt5 + OAmt6 + OAmt7 + OAmt8
      ENDIF
      *--iF the user select the Availble qty
      IF ASCAN(laRPRepTar, "Available") > 0 .OR. llSelAll
        SELECT (lcTmpFile)
        REPLACE llPrceAvlb WITH .T.
        REPLACE ASiz1   WITH ASiz1 - lnOrdQty1 ,;
                ASiz2   WITH ASiz2 - lnOrdQty2 ,;
                ASiz3   WITH ASiz3 - lnOrdQty3 ,;
                ASiz4   WITH ASiz4 - lnOrdQty4 ,;
                ASiz5   WITH ASiz5 - lnOrdQty5 ,;
                ASiz6   WITH ASiz6 - lnOrdQty6 ,;
                ASiz7   WITH ASiz7 - lnOrdQty7 ,;
                ASiz8   WITH ASiz8 - lnOrdQty8 ,;
                AAmt1   WITH AAmt1 - lnOrdAmt1 ,;
                AAmt2   WITH AAmt2 - lnOrdAmt2 ,;
                AAmt3   WITH AAmt3 - lnOrdAmt3 ,;
                AAmt4   WITH AAmt4 - lnOrdAmt4 ,;
                AAmt5   WITH AAmt5 - lnOrdAmt5 ,;
                AAmt6   WITH AAmt6 - lnOrdAmt6 ,;
                AAmt7   WITH AAmt7 - lnOrdAmt7 ,;
                AAmt8   WITH AAmt8 - lnOrdAmt8 ,;
                TotASiz WITH (ASiz1 + ASiz2 + ASiz3 + ASiz4 + ASiz5 + ASiz6 + ASiz7 + ASiz8) ,;
                TotAAmt WITH (AAmt1 + AAmt2 + AAmt3 + AAmt4 + AAmt5 + AAmt6 + AAmt7 + AAmt8)
      ENDIF
    ENDIF
    *--Section break the relations for this section.
    SELECT STYLE
    SET RELATION TO
    SELECT ORDLINE
    SET RELATION TO
  ENDIF
  *B606784,1 -WAB (end)

  *--ASiz >> for the Available line.
  *--AAmt >> for the Available amount.
  *B606784,1 -WAB --No need to Calculate the Availble Qty by add the WIP and the STOK and substract the ORDER
  *B606784,1 -WAB   we will add only the Stk Qty becasue while caulcate the WIP we have add it to the Availbel qty
  *B606784,1 -WAB   and while calculate the Opder qty we have substracted from the Availble qty
  *--This section in case the user select the 3 parts of the wip [ WIP + STOCK - ORDER]
  *llExpr = .F.
  *llExpr = (ASCAN(laRPRepTar, "Available") > 0         ;
  *      AND ASCAN(laRPRepTar, "WIP") > 0               ;
  *      AND ASCAN(laRPRepTar, "Stock") > 0             ;
  *      AND ASCAN(laRPRepTar, "Open Orders") > 0  )    ;
  *      .OR. llSelAll
  *IF llExpr
  *  SELECT (lcTmpFile)
  *  REPLACE llPrceAvlb WITH .T.
  *  REPLACE ASiz1 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz1 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz1 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz1 , 0 ) ,;
  *          ASiz2 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz2 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz2 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz2 , 0 ) ,;
  *          ASiz3 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz3 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz3 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz3 , 0 ) ,;
  *          ASiz4 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz4 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz4 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz4 , 0 )
  *
  *  REPLACE ASiz5 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz5 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz5 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz5 , 0 ) ,;
  *          ASiz6 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz6 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz6 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz6 , 0 ) ,;
  *          ASiz7 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz7 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz7 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz7 , 0 ) ,;
  *          ASiz8 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WSiz8 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SSiz8 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , OSiz8 , 0 )
  *  REPLACE AAmt1 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt1 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt1 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz1 * STYLE.PRICEA ) , 0 ) ,;
  *          AAmt2 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt2 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt2 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz2 * STYLE.PRICEA ) , 0 ) ,;
  *          AAmt3 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt3 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt3 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz3 * STYLE.PRICEA ) , 0 ) ,;
  *          AAmt4 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt4 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt4 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz4 * STYLE.PRICEA ) , 0 )
  *  REPLACE AAmt5 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt5 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt5 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz5 * STYLE.PRICEA ) , 0 ) ,;
  *          AAmt6 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt6 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt6 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz6 * STYLE.PRICEA ) , 0 ) ,;
  *          AAmt7 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt7 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt7 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz7 * STYLE.PRICEA ) , 0 ) ,;
  *          AAmt8 WITH IIF(ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll , WAmt8 , 0 ) + IIF(ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll , SAmt8 , 0 ) - IIF(ASCAN(laRPRepTar, "Open Orders") > 0 .OR. llSelAll , (OSiz8 * STYLE.PRICEA ) , 0 )
  *  REPLACE TotASiz WITH ASiz1 + ASiz2 + ASiz3 + ASiz4 + ASiz5 + ASiz6 + ASiz7 + ASiz8 ,;
  *          TotAAmt WITH AAmt1 + AAmt2 + AAmt3 + AAmt4 + AAmt5 + AAmt6 + AAmt7 + AAmt8
  *ENDIF
  **--In case there is one of the parts of the equation is not selected.
  *llExpr2 = .F.
  *llExpr2 = ASCAN(laRPRepTar, "Available") > 0         ;
  *       AND ( ASCAN(laRPRepTar, "WIP") = 0            ;
  *       .OR. ASCAN(laRPRepTar, "Stock") = 0           ;
  *       .OR. ASCAN(laRPRepTar, "Open Orders") = 0  )
  *IF llExpr2
  *  =lfWipColct()
  *ENDIF

  **we will add only the Stk Qty becasue while caulcate the WIP we have add it to the Availbel qty
  **and while calculate the Opder qty we have substracted from the Availble qty
  IF ASCAN(laRPRepTar, "Available") > 0 .OR. llSelAll
    SELECT (lcTmpFile)
    REPLACE ASiz1   WITH ASiz1 + STYLE.STK1                   ,;
            ASiz2   WITH ASiz2 + STYLE.STK2                   ,;
            ASiz3   WITH ASiz3 + STYLE.STK3                   ,;
            ASiz4   WITH ASiz4 + STYLE.STK4                   ,;
            ASiz5   WITH ASiz5 + STYLE.STK5                   ,;
            ASiz6   WITH ASiz6 + STYLE.STK6                   ,;
            ASiz7   WITH ASiz7 + STYLE.STK7                   ,;
            ASiz8   WITH ASiz8 + STYLE.STK8                   ,;
            AAmt1   WITH AAmt1 + (STYLE.STK1 * STYLE.PRICEA ) ,;
            AAmt2   WITH AAmt2 + (STYLE.STK2 * STYLE.PRICEA ) ,;
            AAmt3   WITH AAmt3 + (STYLE.STK3 * STYLE.PRICEA ) ,;
            AAmt4   WITH AAmt4 + (STYLE.STK4 * STYLE.PRICEA ) ,;
            AAmt5   WITH AAmt5 + (STYLE.STK5 * STYLE.PRICEA ) ,;
            AAmt6   WITH AAmt6 + (STYLE.STK6 * STYLE.PRICEA ) ,;
            AAmt7   WITH AAmt7 + (STYLE.STK7 * STYLE.PRICEA ) ,;
            AAmt8   WITH AAmt8 + (STYLE.STK8 * STYLE.PRICEA ) ,;
            TotASiz WITH ASiz1 + ASiz2 + ASiz3 + ASiz4 + ASiz5 + ASiz6 + ASiz7 + ASiz8 ,;
            TotAAmt WITH AAmt1 + AAmt2 + AAmt3 + AAmt4 + AAmt5 + AAmt6 + AAmt7 + AAmt8
  ENDIF
  *B606784,1 -WAB  (END)

  
  *--PSiz >> for the Shipped line.
  *--PAmt >> for the Shipped amount.
  
  IF ASCAN(laRPRepTar, "Shipped") > 0 .OR. llSelAll
  
    SELECT STYLE
    IF !('STYLE.STYLE' $ SET('RELATION'))
      SET RELATION TO STYLE.STYLE INTO INVLINE ADDITIVE
 
      SELECT INVLINE
      SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE
    ENDIF

    IF SEEK(STYLE.STYLE , "INVLINE")
      SELECT INVLINE
      SCAN REST WHILE STYLE + INVOICE + STR(LINENO,6) = STYLE.STYLE FOR INVHDR.Status <> 'V'

        IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , INVLINE.INVDATE  = INVLINE.INVDATE ,;
          IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , INVLINE.INVDATE <= ldRPPHDate , BETWEEN(INVLINE.INVDATE,ldRPPLDate,ldRPPHDate)))
          lnPrcOrd = lfPricOrd()
          SELECT (lcTmpFile)
          REPLACE llPrceShp WITH .T.
          REPLACE PSiz1 WITH PSiz1 + INVLINE.QTY1            ,;
                  PSiz2 WITH PSiz2 + INVLINE.QTY2            ,;
                  PSiz3 WITH PSiz3 + INVLINE.QTY3            ,;
                  PSiz4 WITH PSiz4 + INVLINE.QTY4            ,;
                  PSiz5 WITH PSiz5 + INVLINE.QTY5            ,;
                  PSiz6 WITH PSiz6 + INVLINE.QTY6            ,;
                  PSiz7 WITH PSiz7 + INVLINE.QTY7            ,;
                  PSiz8 WITH PSiz8 + INVLINE.QTY8            ,;
                  PAmt1 WITH PAmt1 + INVLINE.QTY1 * lnPrcOrd ,;
                  PAmt2 WITH PAmt2 + INVLINE.QTY2 * lnPrcOrd ,;
                  PAmt3 WITH PAmt3 + INVLINE.QTY3 * lnPrcOrd ,;
                  PAmt4 WITH PAmt4 + INVLINE.QTY4 * lnPrcOrd ,;
                  PAmt5 WITH PAmt5 + INVLINE.QTY5 * lnPrcOrd ,;
                  PAmt6 WITH PAmt6 + INVLINE.QTY6 * lnPrcOrd ,;
                  PAmt7 WITH PAmt7 + INVLINE.QTY7 * lnPrcOrd ,;
                  PAmt8 WITH PAmt8 + INVLINE.QTY8 * lnPrcOrd

          *B606828,1 WAB (Start) - the total is incorrect 
          *REPLACE TotPSiz WITH TotPSiz + PSiz1 + PSiz2 + PSiz3 + PSiz4 + PSiz5 + PSiz6 + PSiz7 + PSiz8 ,;
          *        TotPAmt WITH TotPAmt + PAmt1 + PAmt2 + PAmt3 + PAmt4 + PAmt5 + PAmt6 + PAmt7 + PAmt8
          REPLACE TotPSiz WITH  PSiz1 + PSiz2 + PSiz3 + PSiz4 + PSiz5 + PSiz6 + PSiz7 + PSiz8 ,;
                  TotPAmt WITH  PAmt1 + PAmt2 + PAmt3 + PAmt4 + PAmt5 + PAmt6 + PAmt7 + PAmt8
         *B606828,1 WAB (End) 
        ENDIF
      ENDSCAN
    ENDIF

    *--Section break the relations for this section.
    SELECT STYLE
    SET RELATION TO
    SELECT INVLINE
    SET RELATION TO
  ENDIF

  *--TSiz >> for the TTL Sold line.
  *--TAmt >> for the TTL Sold amount.
  IF ASCAN(laRPRepTar, "TTL Sold") > 0 .OR. llSelAll
    *--For the sum of all open order lines.
    IF SEEK(STYLE.STYLE , "ORDLINE")

      SELECT STYLE
      IF !('STYLE.STYLE' $ SET('RELATION'))
        SET RELATION TO STYLE.STYLE INTO ORDLINE ADDITIVE

        SELECT ORDLINE
        SET RELATION TO "O" + ORDLINE.ORDER INTO ORDHDR ADDITIVE
      ENDIF

      SELECT ORDLINE
      SCAN REST WHILE STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER + STORE + STR(LINENO,6) = STYLE.STYLE FOR ORDHDR.STATUS $ "OH"

        IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , ORDHDR.ENTERED  = ORDHDR.ENTERED ,;
          IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , ORDHDR.ENTERED <= ldRPPHDate , BETWEEN(ORDHDR.ENTERED,ldRPPLDate,ldRPPHDate)))

          SELECT (lcTmpFile)
          IF ORDHDR.STATUS $ "OH"
            REPLACE llPrceTtl WITH .T.
            REPLACE TSiz1 WITH TSiz1 + ORDLINE.QTY1                    ,;
                    TSiz2 WITH TSiz2 + ORDLINE.QTY2                    ,;
                    TSiz3 WITH TSiz3 + ORDLINE.QTY3                    ,;
                    TSiz4 WITH TSiz4 + ORDLINE.QTY4                    ,;
                    TSiz5 WITH TSiz5 + ORDLINE.QTY5                    ,;
                    TSiz6 WITH TSiz6 + ORDLINE.QTY6                    ,;
                    TSiz7 WITH TSiz7 + ORDLINE.QTY7                    ,;
                    TSiz8 WITH TSiz8 + ORDLINE.QTY8                    ,;
                    TAmt1 WITH TAmt1 + (ORDLINE.QTY1 * ORDLINE.PRICE ) ,;
                    TAmt2 WITH TAmt2 + (ORDLINE.QTY2 * ORDLINE.PRICE ) ,;
                    TAmt3 WITH TAmt3 + (ORDLINE.QTY3 * ORDLINE.PRICE ) ,;
                    TAmt4 WITH TAmt4 + (ORDLINE.QTY4 * ORDLINE.PRICE ) ,;
                    TAmt5 WITH TAmt5 + (ORDLINE.QTY5 * ORDLINE.PRICE ) ,;
                    TAmt6 WITH TAmt6 + (ORDLINE.QTY6 * ORDLINE.PRICE ) ,;
                    TAmt7 WITH TAmt7 + (ORDLINE.QTY7 * ORDLINE.PRICE ) ,;
                    TAmt8 WITH TAmt8 + (ORDLINE.QTY8 * ORDLINE.PRICE )

          *B606828,1 WAB (Start) - the total is incorrect 
          *REPLACE TotTSiz WITH TotTSiz + TSiz1 + TSiz2 + TSiz3 + TSiz4 + TSiz5 + TSiz6 + TSiz7 + TSiz8 ,;
          *          TotTAmt WITH TotTAmt + TAmt1 + TAmt2 + TAmt3 + TAmt4 + TAmt5 + TAmt6 + TAmt7 + TAmt8
          REPLACE TotTSiz WITH  TSiz1 + TSiz2 + TSiz3 + TSiz4 + TSiz5 + TSiz6 + TSiz7 + TSiz8 ,;
                  TotTAmt WITH  TAmt1 + TAmt2 + TAmt3 + TAmt4 + TAmt5 + TAmt6 + TAmt7 + TAmt8
          *B606828,1 WAB (End) 
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF

    *--Section break the relations for this section.
    SELECT STYLE
    SET RELATION TO
    SELECT ORDLINE
    SET RELATION TO

    *--For the sum of all Shipped order lines(exclude cancel pieces) for "OH" orders.
    IF SEEK(STYLE.STYLE , "INVLINE")
      SELECT STYLE
      IF !('STYLE.STYLE' $ SET('RELATION'))
        SET RELATION TO STYLE.STYLE INTO ORDLINE ADDITIVE

        SELECT INVLINE
        SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE
        SET RELATION TO INVLINE.STYLE INTO ORDLINE ADDITIVE

        SELECT ORDLINE
        SET RELATION TO "O" + ORDLINE.ORDER INTO ORDHDR ADDITIVE
      ENDIF

      SELECT INVLINE
      SCAN REST WHILE STYLE + INVOICE + STR(LINENO,6) = STYLE.STYLE FOR INVHDR.STATUS # "V"

        IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , INVHDR.INVDATE  = INVHDR.INVDATE ,;
          IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , INVHDR.INVDATE <= ldRPPHDate , BETWEEN(INVHDR.INVDATE,ldRPPLDate,ldRPPHDate)))

          IF ORDHDR.STATUS $ "OH"
            SELECT (lcTmpFile)
            REPLACE TSiz1 WITH TSiz1 + INVLINE.QTY1                    ,;
                    TSiz2 WITH TSiz2 + INVLINE.QTY2                    ,;
                    TSiz3 WITH TSiz3 + INVLINE.QTY3                    ,;
                    TSiz4 WITH TSiz4 + INVLINE.QTY4                    ,;
                    TSiz5 WITH TSiz5 + INVLINE.QTY5                    ,;
                    TSiz6 WITH TSiz6 + INVLINE.QTY6                    ,;
                    TSiz7 WITH TSiz7 + INVLINE.QTY7                    ,;
                    TSiz8 WITH TSiz8 + INVLINE.QTY8                    ,;
                    TAmt1 WITH TAmt1 + (INVLINE.QTY1 * ORDLINE.PRICE ) ,;
                    TAmt2 WITH TAmt2 + (INVLINE.QTY2 * ORDLINE.PRICE ) ,;
                    TAmt3 WITH TAmt3 + (INVLINE.QTY3 * ORDLINE.PRICE ) ,;
                    TAmt4 WITH TAmt4 + (INVLINE.QTY4 * ORDLINE.PRICE ) ,;
                    TAmt5 WITH TAmt5 + (INVLINE.QTY5 * ORDLINE.PRICE ) ,;
                    TAmt6 WITH TAmt6 + (INVLINE.QTY6 * ORDLINE.PRICE ) ,;
                    TAmt7 WITH TAmt7 + (INVLINE.QTY7 * ORDLINE.PRICE ) ,;
                    TAmt8 WITH TAmt8 + (INVLINE.QTY8 * ORDLINE.PRICE )

            REPLACE TotTSiz WITH TSiz1 + TSiz2 + TSiz3 + TSiz4 + TSiz5 + TSiz6 + TSiz7 + TSiz8 ,;
                    TotTAmt WITH TAmt1 + TAmt2 + TAmt3 + TAmt4 + TAmt5 + TAmt6 + TAmt7 + TAmt8
          ENDIF
        ENDIF
      ENDSCAN

      *--Section break the relations for this section.
      SELECT STYLE
      SET RELATION TO
      SELECT INVLINE
      SET RELATION TO
      SELECT ORDLINE
      SET RELATION TO
    ENDIF
  ENDIF

  *--RSiz >> for the Returns line.
  *--RAmt >> for the Returns amount.
  IF ASCAN(laRPRepTar, "Returns") > 0 .OR. llSelAll

    SELECT STYLE
    IF !('STYLE.STYLE' $ SET('RELATION'))
      SET RELATION TO STYLE.STYLE INTO RETLINE ADDITIVE
 
      SELECT RETLINE
      SET RELATION TO RETLINE.CRMEMO INTO RETHDR ADDITIVE
    ENDIF

    IF SEEK(STYLE.STYLE , "RETLINE")
      SELECT RETLINE
    
      *B606784,1 -WAB (Start) Wrong calculate the Returned qty becasue in the Credit memo lines there are 2 trans.
      *B606784,1 -WAB         Code '2' for the Credit qty and '4' for the Cancel qty (in case of 
      *B606784,1 -WAB         return partial an credit meemo from Returned authoritation)
      *SCAN REST WHILE STYLE + CRMEMO = STYLE.STYLE FOR RETHDR.STATUS # "V"
      SCAN REST WHILE STYLE + CRMEMO = STYLE.STYLE FOR RETHDR.STATUS # "V" .AND. RETLINE.cRet_TrnCd = '2'
      *B606784,1 -WAB (end)
        IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , CRDATE  = CRDATE ,;
          IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , CRDATE <= ldRPPHDate , BETWEEN(CRDATE,ldRPPLDate,ldRPPHDate)))

          SELECT (lcTmpFile)
          REPLACE llPrceRet WITH .T.
          REPLACE RSiz1 WITH RSiz1 + RETLINE.QTY1                   ,;
                  RSiz2 WITH RSiz2 + RETLINE.QTY2                   ,;
                  RSiz3 WITH RSiz3 + RETLINE.QTY3                   ,;
                  RSiz4 WITH RSiz4 + RETLINE.QTY4                   ,;
                  RSiz5 WITH RSiz5 + RETLINE.QTY5                   ,;
                  RSiz6 WITH RSiz6 + RETLINE.QTY6                   ,;
                  RSiz7 WITH RSiz7 + RETLINE.QTY7                   ,;
                  RSiz8 WITH RSiz8 + RETLINE.QTY8                   ,;
                  RAmt1 WITH RAmt1 + (RETLINE.QTY1 * RETLINE.PRICE) ,;
                  RAmt2 WITH RAmt2 + (RETLINE.QTY2 * RETLINE.PRICE) ,;
                  RAmt3 WITH RAmt3 + (RETLINE.QTY3 * RETLINE.PRICE) ,;
                  RAmt4 WITH RAmt4 + (RETLINE.QTY4 * RETLINE.PRICE) ,;
                  RAmt5 WITH RAmt5 + (RETLINE.QTY5 * RETLINE.PRICE) ,;
                  RAmt6 WITH RAmt6 + (RETLINE.QTY6 * RETLINE.PRICE) ,;
                  RAmt7 WITH RAmt7 + (RETLINE.QTY7 * RETLINE.PRICE) ,;
                  RAmt8 WITH RAmt8 + (RETLINE.QTY8 * RETLINE.PRICE)

          *B606828,1 WAB (Start) - the total is incorrect  
          *REPLACE TotRSiz WITH TotRSiz + RSiz1 + RSiz2 + RSiz3 + RSiz4 + RSiz5 + RSiz6 + RSiz7 + RSiz8 ,;
          *        TotRAmt WITH TotRAmt + RAmt1 + RAmt2 + RAmt3 + RAmt4 + RAmt5 + RAmt6 + RAmt7 + RAmt8
          REPLACE TotRSiz WITH  RSiz1 + RSiz2 + RSiz3 + RSiz4 + RSiz5 + RSiz6 + RSiz7 + RSiz8 ,;
                  TotRAmt WITH  RAmt1 + RAmt2 + RAmt3 + RAmt4 + RAmt5 + RAmt6 + RAmt7 + RAmt8
         *B606828,1 WAB (End) 
        ENDIF
      ENDSCAN
    ENDIF

    *--Section break the relations for this section.
    SELECT STYLE
    SET RELATION TO
    SELECT RETLINE
    SET RELATION TO
  ENDIF
ENDSCAN


*--To use this field in printing the Style total in the DOS frx.
IF lcHldStyl # LEFT(STYLE.STYLE,lnLenth) AND !EMPTY(lcHldStyl)
  SELECT (lcTmpFile)
  REPLACE llPrnTotSt WITH .T.
ENDIF

SELECT (lcTmpFile)
SCAN
  *--To use this field in printing the Group total in the DOS frx.
  *B606784,1 -WAB  (START)
  *IF lcGrpStyl # STYLE.CSTYGROUP AND !EMPTY(lcGrpStyl)
  IF lcRPSortBy='G' .AND. lcGrpStyl # STYLE.CSTYGROUP AND !EMPTY(lcGrpStyl)
  *B606784,1 -WAB  (END)
  
    REPLACE llPrnGrpSt WITH .T.
  ENDIF
  lcGrpStyl = STYLE.CSTYGROUP
ENDSCAN

*--To update the last group.
IF EOF(lcTmpFile) .AND. lcRPSortBy='G' 
  SELECT (lcTmpFile)
  GOTO BOTTOM
  REPLACE llPrnGrpSt WITH .T.
ENDIF

*--To use this field in export to excel and the variable in the last record.
SELECT (lcTmpFile)
REPLACE llPrnGrnd WITH .T.

SELECT STYLE
SET RELATION TO

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfPricOrd
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function to get the price from the ordline.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPricOrd()
*!*************************************************************
FUNCTION lfPricOrd
PRIVATE lcAlsOrd , lckeyOrd , lcTagOrd , lnPricOrd

STORE 0 TO lnPricOrd
lcAlsOrd = SELECT(0)
SELECT ORDLINE
lckeyOrd = EVAL(KEY())
lcTagOrd = ORDER()
SET ORDER TO TAG ORDLINE

IF SEEK("O" + INVLINE.ORDER , "ORDLINE")
  SCAN REST WHILE CORDTYPE + ORDER + STR(LINENO,6) = "O" + INVLINE.ORDER FOR ORDLINE.STYLE = INVLINE.STYLE
    lnPricOrd = ORDLINE.PRICE
  ENDSCAN
ENDIF

SET ORDER TO TAG &lcTagOrd
=SEEK(lckeyOrd)
RETURN lnPricOrd

*--End of lfPricOrd.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Validate style.
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

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Go top in the style IN RANGE.
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
*! Example   : =lfSetSty()
*!*************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE
   GO TOP IN STYLE

  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSty.
*!*************************************************************
*! Name      : lfMajTtGet
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : To get the style major segement title.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Get major seg. picture.
*!*************************************************************
*! Called from : Option Grid.
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

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : To get the style nonmajor segement structure.
*!*************************************************************
*! Called from : Option Grid.
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
      *-- this means that another type is found rather than color or free.
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Showes date range screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = ldRPPLDate
ldTo   = ldRPPHDate

lcTitle = 'Transaction Date'
DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************
FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldRPPLDate = ldFrom
  ldRPPHDate = ldTo
  CLEAR READ
ENDIF

*--End of lfvPbOk.
*!*************************************************************
*! Name      : lfvPrnReps
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function for the transaction mover.
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

IF TYPE('laRPRepSou[1,1]') = 'U' OR EMPTY(laRPRepSou[1,1])
  DIMENSION laRPRepSou[1]
  STORE SPACE(0) TO laRPRepSou
ENDIF

IF TYPE('laRPRepTar[1,1]') = 'U' OR EMPTY(laRPRepTar[1,1])
  DIMENSION laRPRepTar[1]
  STORE SPACE(0) TO laRPRepTar
ENDIF

DIMENSION laRPRepSou[8]
laRPRepSou[01] = "Bought"
laRPRepSou[02] = "WIP"
laRPRepSou[03] = "Stock"
laRPRepSou[04] = "Open Orders"
laRPRepSou[05] = "Available"
laRPRepSou[06] = "Shipped"
laRPRepSou[07] = "TTL Sold"
laRPRepSou[08] = "Returns"

= gfMover(@laRPRepSou,@laRPRepTar,"Transactions to be printed",.T.,.F.,.F.,.T.)

STORE .F. TO llBought , llWIP , llStock , llOpenOrd , llAvalabl , llShppd , llTTLSold , llReturns
*--In case the user didn't selecte any transaction to preview all of them.
llSelAll = EMPTY(laRPRepTar)
IF llSelAll
  DIMENSION laRPRepTar[1]
ENDIF

*--Srection to get the variables its values to use them in the FRX.
IF ASCAN(laRPRepTar, "Bought") > 0 .OR. llSelAll
  STORE .T. TO llBought
ENDIF
IF ASCAN(laRPRepTar, "WIP") > 0 .OR. llSelAll
  STORE .T. TO llWIP
ENDIF
IF ASCAN(laRPRepTar, "Stock") > 0 .OR. llSelAll
  STORE .T. TO llStock
ENDIF
IF ASCAN(laRPRepTar, "Open Orders") > 0  .OR. llSelAll
  STORE .T. TO llOpenOrd
ENDIF
IF ASCAN(laRPRepTar, "Available") > 0 .OR. llSelAll
  STORE .T. TO llAvalabl
ENDIF
IF ASCAN(laRPRepTar, "Shipped") > 0 .OR. llSelAll
  STORE .T. TO llShppd
ENDIF
IF ASCAN(laRPRepTar, "TTL Sold") > 0 .OR. llSelAll
  STORE .T. TO llTTLSold
ENDIF
IF ASCAN(laRPRepTar, "Returns") > 0 .OR. llSelAll
  STORE .T. TO llReturns
ENDIF

*--End of lfvPrnReps.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Option Grid When function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lcRepMode    = "Text"
lcOGPlatForm = "DOS"
SHOW GET lcRepMode

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfChngForm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Change printed form.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfChngForm()
*!*************************************************************
FUNCTION lfChngForm

IF lcRpKind =='S'
  lcRpForm = "ICSTYBSS"
ELSE
  lcRpForm = "ICSTYBSR"
ENDIF
=lfRepPltFr(lcRpForm)

*--End of lfChngForm.
*!*************************************************************
*! Name      : lfExport
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function to Export the temp. file to excel file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfExport

DIMENSION laTempEx[1,4]
SELECT (lcTmpFile)
=AFIELDS(laTempEx)

*--Arrays hold the subtotal and Grand total values.  [1St for the quanity and 2nd for the values]
DIMENSION laSubTotQ[80]  , laSubTotV[72]
DIMENSION laGropTotQ[80] , laGropTotV[72]
DIMENSION laGrndTotQ[80] , laGrndTotV[72]

STORE 0 TO laSubTotQ , laSubTotV
STORE 0 TO laGropTotQ , laGropTotV
STORE 0 TO laGrndTotQ , laGrndTotV

SCAN
  SELECT (lcExprtFil)
  APPEND BLANK
  IF lcRpKind =='S'
    REPLACE Style WITH &lcTmpFile..STYLE
  ELSE
    REPLACE Style WITH &lcTmpFile..STYLE ,;
            Size1 WITH SCALE.SZ1         ,;
            Size2 WITH SCALE.SZ2         ,;
            Size3 WITH SCALE.SZ3         ,;
            Size4 WITH SCALE.SZ4         ,;
            Size5 WITH SCALE.SZ5         ,;
            Size6 WITH SCALE.SZ6         ,;
            Size7 WITH SCALE.SZ7         ,;
            Size8 WITH SCALE.SZ8
  ENDIF


  *--transaction Bought.
  IF ASCAN(laTempEx , "BSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "Bought"                          ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotBSiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotBAmt))

    laSubTotQ[9] = laSubTotQ[9] + &lcTmpFile..TotBSiz
    laSubTotV[9] = laSubTotV[9] + &lcTmpFile..TotBAmt

    laGropTotQ[9] = laGropTotQ[9] + &lcTmpFile..TotBSiz
    laGropTotV[9] = laGropTotV[9] + &lcTmpFile..TotBAmt

    laGrndTotQ[9] = laGrndTotQ[9] + &lcTmpFile..TotBSiz
    laGrndTotV[9] = laGrndTotV[9] + &lcTmpFile..TotBAmt

    IF lcRpKind =='D'
      FOR lnL1 = 1 TO 8
        lcBqty = "BSiz"  + ALLTRIM(STR(lnL1))
        lcBval = "BAmt"  + ALLTRIM(STR(lnL1))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL1))
        lcVal  = "Value" + ALLTRIM(STR(lnL1))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL1] = laSubTotQ[lnL1] + &lcTmpFile..&lcBqty
          laSubTotV[lnL1] = laSubTotV[lnL1] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL1] = laGropTotQ[lnL1] + &lcTmpFile..&lcBqty
          laGropTotV[lnL1] = laGropTotV[lnL1] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL1] = laGrndTotQ[lnL1] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL1] = laGrndTotV[lnL1] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--transaction WIP.
  IF ASCAN(laTempEx , "WSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "WIP"                             ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotWSiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotWAmt))

    laSubTotQ[18] = laSubTotQ[18] + &lcTmpFile..TotWSiz
    laSubTotV[18] = laSubTotV[18] + &lcTmpFile..TotWAmt

    laGropTotQ[18] = laGropTotQ[18] + &lcTmpFile..TotWSiz
    laGropTotV[18] = laGropTotV[18] + &lcTmpFile..TotWAmt

    laGrndTotQ[18] = laGrndTotQ[18] + &lcTmpFile..TotWSiz
    laGrndTotV[18] = laGrndTotV[18] + &lcTmpFile..TotWAmt

    IF lcRpKind =='D'
      FOR lnL2 = 1 TO 8
        lcBqty = "WSiz"  + ALLTRIM(STR(lnL2))
        lcBval = "WAmt"  + ALLTRIM(STR(lnL2))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL2))
        lcVal  = "Value" + ALLTRIM(STR(lnL2))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL2 + 9] = laSubTotQ[lnL2 + 9] + &lcTmpFile..&lcBqty
          laSubTotV[lnL2 + 9] = laSubTotV[lnL2 + 9] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL2 + 9] = laGropTotQ[lnL2 + 9] + &lcTmpFile..&lcBqty
          laGropTotV[lnL2 + 9] = laGropTotV[lnL2 + 9] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL2 + 9] = laGrndTotQ[lnL2 + 9] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL2 + 9] = laGrndTotV[lnL2 + 9] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--transaction Stock.
  IF ASCAN(laTempEx , "SSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "Stock"                           ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotSSiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotSAmt))

    laSubTotQ[27] = laSubTotQ[27] + &lcTmpFile..TotSSiz
    laSubTotV[27] = laSubTotV[27] + &lcTmpFile..TotSAmt

    laGropTotQ[27] = laGropTotQ[27] + &lcTmpFile..TotSSiz
    laGropTotV[27] = laGropTotV[27] + &lcTmpFile..TotSAmt

    laGrndTotQ[27] = laGrndTotQ[27] + &lcTmpFile..TotSSiz
    laGrndTotV[27] = laGrndTotV[27] + &lcTmpFile..TotSAmt

    IF lcRpKind =='D'
      FOR lnL3 = 1 TO 8
        lcBqty = "SSiz"  + ALLTRIM(STR(lnL3))
        lcBval = "SAmt"  + ALLTRIM(STR(lnL3))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL3))
        lcVal  = "Value" + ALLTRIM(STR(lnL3))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL3 + 18] = laSubTotQ[lnL3 + 18] + &lcTmpFile..&lcBqty
          laSubTotV[lnL3 + 18] = laSubTotV[lnL3 + 18] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL3 + 18] = laGropTotQ[lnL3 + 18] + &lcTmpFile..&lcBqty
          laGropTotV[lnL3 + 18] = laGropTotV[lnL3 + 18] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL3 + 18] = laGrndTotQ[lnL3 + 18] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL3 + 18] = laGrndTotV[lnL3 + 18] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--transaction Open Order.
  IF ASCAN(laTempEx , "OSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "Open Order"                      ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotOSiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotOAmt))

    laSubTotQ[36] = laSubTotQ[36] + &lcTmpFile..TotOSiz
    laSubTotV[36] = laSubTotV[36] + &lcTmpFile..TotOAmt

    laGropTotQ[36] = laGropTotQ[36] + &lcTmpFile..TotOSiz
    laGropTotV[36] = laGropTotV[36] + &lcTmpFile..TotOAmt

    laGrndTotQ[36] = laGrndTotQ[36] + &lcTmpFile..TotOSiz
    laGrndTotV[36] = laGrndTotV[36] + &lcTmpFile..TotOAmt

    IF lcRpKind =='D'
      FOR lnL4 = 1 TO 8
        lcBqty = "OSiz"  + ALLTRIM(STR(lnL4))
        lcBval = "OAmt"  + ALLTRIM(STR(lnL4))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL4))
        lcVal  = "Value" + ALLTRIM(STR(lnL4))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL4 + 27] = laSubTotQ[lnL4 + 27] + &lcTmpFile..&lcBqty
          laSubTotV[lnL4 + 27] = laSubTotV[lnL4 + 27] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL4 + 27] = laGropTotQ[lnL4 + 27] + &lcTmpFile..&lcBqty
          laGropTotV[lnL4 + 27] = laGropTotV[lnL4 + 27] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL4 + 27] = laGrndTotQ[lnL4 + 27] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL4 + 27] = laGrndTotV[lnL4 + 27] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--Transaction Available.
  IF ASCAN(laTempEx , "ASIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "Available"                       ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotASiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotAAmt))

    laSubTotQ[45] = laSubTotQ[45] + &lcTmpFile..TotASiz
    laSubTotV[45] = laSubTotV[45] + &lcTmpFile..TotAAmt

    laGropTotQ[45] = laGropTotQ[45] + &lcTmpFile..TotASiz
    laGropTotV[45] = laGropTotV[45] + &lcTmpFile..TotAAmt

    laGrndTotQ[45] = laGrndTotQ[45] + &lcTmpFile..TotASiz
    laGrndTotV[45] = laGrndTotV[45] + &lcTmpFile..TotAAmt

    IF lcRpKind =='D'
      FOR lnL5 = 1 TO 8
        lcBqty = "ASiz"  + ALLTRIM(STR(lnL5))
        lcBval = "AAmt"  + ALLTRIM(STR(lnL5))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL5))
        lcVal  = "Value" + ALLTRIM(STR(lnL5))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL5 + 36] = laSubTotQ[lnL5 + 36] + &lcTmpFile..&lcBqty
          laSubTotV[lnL5 + 36] = laSubTotV[lnL5 + 36] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL5 + 36] = laGropTotQ[lnL5 + 36] + &lcTmpFile..&lcBqty
          laGropTotV[lnL5 + 36] = laGropTotV[lnL5 + 36] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL5 + 36] = laGrndTotQ[lnL5 + 36] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL5 + 36] = laGrndTotV[lnL5 + 36] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--transaction Shipped.
  IF ASCAN(laTempEx , "PSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "Shipped"                         ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotPSiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotPAmt))

    laSubTotQ[54] = laSubTotQ[54] + &lcTmpFile..TotPSiz
    laSubTotV[54] = laSubTotV[54] + &lcTmpFile..TotPAmt

    laGropTotQ[54] = laGropTotQ[54] + &lcTmpFile..TotPSiz
    laGropTotV[54] = laGropTotV[54] + &lcTmpFile..TotPAmt

    laGrndTotQ[54] = laGrndTotQ[54] + &lcTmpFile..TotPSiz
    laGrndTotV[54] = laGrndTotV[54] + &lcTmpFile..TotPAmt

    IF lcRpKind =='D'
      FOR lnL6 = 1 TO 8
        lcBqty = "PSiz"  + ALLTRIM(STR(lnL6))
        lcBval = "PAmt"  + ALLTRIM(STR(lnL6))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL6))
        lcVal  = "Value" + ALLTRIM(STR(lnL6))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL6 + 45] = laSubTotQ[lnL6 + 45] + &lcTmpFile..&lcBqty
          laSubTotV[lnL6 + 45] = laSubTotV[lnL6 + 45] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL6 + 45] = laGropTotQ[lnL6 + 45] + &lcTmpFile..&lcBqty
          laGropTotV[lnL6 + 45] = laGropTotV[lnL6 + 45] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL6 + 45] = laGrndTotQ[lnL6 + 45] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL6 + 45] = laGrndTotV[lnL6 + 45] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--transaction TOTAL Sold.
  IF ASCAN(laTempEx , "TSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "TTL Sold"                        ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotTSiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotTAmt))

    laSubTotQ[63] = laSubTotQ[63] + &lcTmpFile..TotTSiz
    laSubTotV[63] = laSubTotV[63] + &lcTmpFile..TotTAmt

    laGropTotQ[63] = laGropTotQ[63] + &lcTmpFile..TotTSiz
    laGropTotV[63] = laGropTotV[63] + &lcTmpFile..TotTAmt

    laGrndTotQ[63] = laGrndTotQ[63] + &lcTmpFile..TotTSiz
    laGrndTotV[63] = laGrndTotV[63] + &lcTmpFile..TotTAmt

    IF lcRpKind =='D'
      FOR lnL7 = 1 TO 8
        lcBqty = "TSiz"  + ALLTRIM(STR(lnL7))
        lcBval = "TAmt"  + ALLTRIM(STR(lnL7))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL7))
        lcVal  = "Value" + ALLTRIM(STR(lnL7))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL7 + 54] = laSubTotQ[lnL7 + 54] + &lcTmpFile..&lcBqty
          laSubTotV[lnL7 + 54] = laSubTotV[lnL7 + 54] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL7 + 54] = laGropTotQ[lnL7 + 54] + &lcTmpFile..&lcBqty
          laGropTotV[lnL7 + 54] = laGropTotV[lnL7 + 54] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL7 + 54] = laGrndTotQ[lnL7 + 54] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL7 + 54] = laGrndTotV[lnL7 + 54] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--transaction Returns.
  IF ASCAN(laTempEx , "RSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type      WITH "Returns"                         ,;
            Tot_Units WITH ALLTRIM(STR(&lcTmpFile..TotRSiz)) ,;
            Tot_Value WITH ALLTRIM(STR(&lcTmpFile..TotRAmt))

    laSubTotQ[72] = laSubTotQ[72] + &lcTmpFile..TotRSiz
    laSubTotV[72] = laSubTotV[72] + &lcTmpFile..TotRAmt

    laGropTotQ[72] = laGropTotQ[72] + &lcTmpFile..TotRSiz
    laGropTotV[72] = laGropTotV[72] + &lcTmpFile..TotRAmt

    laGrndTotQ[72] = laGrndTotQ[72] + &lcTmpFile..TotRSiz
    laGrndTotV[72] = laGrndTotV[72] + &lcTmpFile..TotRAmt

    IF lcRpKind =='D'
      FOR lnL8 = 1 TO 8
        lcBqty = "RSiz"  + ALLTRIM(STR(lnL8))
        lcBval = "RAmt"  + ALLTRIM(STR(lnL8))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL8))
        lcVal  = "Value" + ALLTRIM(STR(lnL8))

        IF &lcTmpFile..&lcBqty # 0
          REPLACE &lcSiz WITH ALLTRIM(STR(&lcTmpFile..&lcBqty)) ,;
                  &lcVal WITH ALLTRIM(STR(&lcTmpFile..&lcBval))

          *--Arrays for the subtotal.
          laSubTotQ[lnL8 + 63] = laSubTotQ[lnL8 + 63] + &lcTmpFile..&lcBqty
          laSubTotV[lnL8 + 63] = laSubTotV[lnL8 + 63] + &lcTmpFile..&lcBval

          *--Arrays for the Group total.
          laGropTotQ[lnL8 + 63] = laGropTotQ[lnL8 + 63] + &lcTmpFile..&lcBqty
          laGropTotV[lnL8 + 63] = laGropTotV[lnL8 + 63] + &lcTmpFile..&lcBval

          *--Arrays for the Grand total.
          laGrndTotQ[lnL8 + 63] = laGrndTotQ[lnL8 + 63] + &lcTmpFile..&lcBqty
          laGrndTotV[lnL8 + 63] = laGrndTotV[lnL8 + 63] + &lcTmpFile..&lcBval

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--transaction SELL THRU.
  *--transaction Bought.
  IF ASCAN(laTempEx , "BSIZ1") > 0 AND ASCAN(laTempEx , "TSIZ1") > 0
    SELECT (lcExprtFil)
    APPEND BLANK
    REPLACE Type WITH "SELL THRU"

    IF lcRpKind =='D'
      FOR lnL9 = 1 TO 8
        lcBqty = "BSiz"  + ALLTRIM(STR(lnL9))
        lcTqty = "TSiz"  + ALLTRIM(STR(lnL9))
        lcSiz  = "Size"  + ALLTRIM(STR(lnL9))

        IF &lcTmpFile..&lcBqty # 0 AND &lcTmpFile..&lcTqty # 0
          REPLACE &lcSiz WITH STR((&lcTmpFile..&lcTqty / &lcTmpFile..&lcBqty) *100 , 5 , 2) + "%"

          *--Arrays for the subtotal.
          *---------->>>>>>>>>>>>>> &&lcBqty      / &&lcTqty

          laSubTotQ[lnL9 + 72] = (laSubTotQ[lnL9 + 54] / laSubTotQ[lnL9]) * 100

          *--Arrays for the Group total.
          laGropTotQ[lnL9 + 72] = (laGropTotQ[lnL9 + 54] / laGropTotQ[lnL9]) * 100

          *--Arrays for the Grand total.
          laGrndTotQ[lnL9 + 72] = (laGrndTotQ[lnL9 + 54] / laGrndTotQ[lnL9]) * 100

        ENDIF
      ENDFOR
    ENDIF
  ENDIF

  *--Section of the subtotal.
  SELECT (lcTmpFile)
  IF llPrnTotSt
    =lfSubTot()

    *--Reinitialize the subtotal arrays.
    STORE 0 TO laSubTotQ , laSubTotV
  ENDIF

  *--Section of the Group Total.
  SELECT (lcTmpFile)
  IF llPrnGrpSt
    =lfGropTot()

    *--Reinitialize the subtotal arrays.
    STORE 0 TO laGropTotQ , laGropTotV
  ENDIF

  *--Section of the Grand Total.
  SELECT (lcTmpFile)
  IF llPrnGrnd
    =lfGrndTot()

    *--Reinitialize the subtotal arrays.
    STORE 0 TO laGrndTotQ , laGrndTotV
  ENDIF

ENDSCAN

SELECT (lcExprtFil)
EXPORT TO ("C:\"+'AVAIL') TYPE XLS

*--End of lfExport.
*!*************************************************************
*! Name      : lfSubTot
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function to calculate the sub total.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSubTot

SELECT (lcExprtFil)
APPEND BLANK
IF lcRpKind =='S'
  REPLACE Style     WITH LEFT(&lcTmpFile..STYLE,lnLenth)  ,;
          Type      WITH "(ALL color)"                    ,;
          Tot_Units WITH "======"                         ,;
          Tot_Value WITH "=========="
ELSE
  REPLACE Style     WITH LEFT(&lcTmpFile..STYLE,lnLenth)  ,;
          Type      WITH "(ALL color)"                    ,;
          Size1     WITH "======"                         ,;
          Size2     WITH "======"                         ,;
          Size3     WITH "======"                         ,;
          Size4     WITH "======"                         ,;
          Size5     WITH "======"                         ,;
          Size6     WITH "======"                         ,;
          Size7     WITH "======"                         ,;
          Size8     WITH "======"                         ,;
          Tot_Units WITH "======"                         ,;
          Tot_Value WITH "=========="
ENDIF

*--transaction Bought.
IF ASCAN(laTempEx , "BSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Bought"                   ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[9])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[9]))

  IF lcRpKind =='D'
    FOR lnL1 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL1))
      lcVal  = "Value" + ALLTRIM(STR(lnL1))

      IF laSubTotQ[lnL1] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL1])) ,;
                &lcVal WITH ALLTRIM(STR(laSubTotV[lnL1]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction WIP.
IF ASCAN(laTempEx , "WSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "WIP"                       ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[18])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[18]))

  IF lcRpKind =='D'
    FOR lnL2 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL2))
      lcVal  = "Value" + ALLTRIM(STR(lnL2))

      IF laSubTotQ[lnL2 + 9] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL2 + 9])) ,;
                &lcVal WITH ALLTRIM(STR(laSubTotV[lnL2 + 9]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Stock.
IF ASCAN(laTempEx , "SSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Stock"                     ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[27])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[27]))

  IF lcRpKind =='D'
    FOR lnL3 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL3))
      lcVal  = "Value" + ALLTRIM(STR(lnL3))

      IF laSubTotQ[lnL3 + 18] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL3 + 18])) ,;
                &lcVal WITH ALLTRIM(STR(laSubTotV[lnL3 + 18]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Open Order.
IF ASCAN(laTempEx , "OSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Open Order"                ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[36])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[36]))

  IF lcRpKind =='D'
    FOR lnL4 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL4))
      lcVal  = "Value" + ALLTRIM(STR(lnL4))

     IF laSubTotQ[lnL4 + 27] # 0
       REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL4 + 27])) ,;
               &lcVal WITH ALLTRIM(STR(laSubTotV[lnL4 + 27]))
     ENDIF
   ENDFOR
 ENDIF
ENDIF

*--transaction Available.
IF ASCAN(laTempEx , "ASIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Available"                 ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[45])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[45]))

  IF lcRpKind =='D'
    FOR lnL5 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL5))
      lcVal  = "Value" + ALLTRIM(STR(lnL5))

      IF laSubTotQ[lnL5 + 36] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL5 + 36])) ,;
                &lcVal WITH ALLTRIM(STR(laSubTotV[lnL5 + 36]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Shipped.
IF ASCAN(laTempEx , "PSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Shipped"                   ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[54])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[54]))

  IF lcRpKind =='D'
    FOR lnL6 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL6))
      lcVal  = "Value" + ALLTRIM(STR(lnL6))

      IF laSubTotQ[lnL6 + 45] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL6 + 45])) ,;
                &lcVal WITH ALLTRIM(STR(laSubTotV[lnL6 + 45]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction TOTAL Sold.
IF ASCAN(laTempEx , "TSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "TTL Sold"                  ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[63])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[63]))

  IF lcRpKind =='D'
    FOR lnL7 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL7))
      lcVal  = "Value" + ALLTRIM(STR(lnL7))

      IF laSubTotQ[lnL7 + 54] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL7 + 54])) ,;
                &lcVal WITH ALLTRIM(STR(laSubTotV[lnL7 + 54]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Returns.
IF ASCAN(laTempEx , "RSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Returns"                   ,;
          Tot_Units WITH ALLTRIM(STR(laSubTotQ[72])) ,;
          Tot_Value WITH ALLTRIM(STR(laSubTotV[72]))

  IF lcRpKind =='D'
    FOR lnL8 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL8))
      lcVal  = "Value" + ALLTRIM(STR(lnL8))

      IF laSubTotQ[lnL8 + 63] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laSubTotQ[lnL8 + 63])) ,;
                &lcVal WITH ALLTRIM(STR(laSubTotV[lnL8 + 63]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction SELL THRU.
*--transaction Bought.
IF ASCAN(laTempEx , "BSIZ1") > 0 AND ASCAN(laTempEx , "TSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type WITH "SELL THRU"

  IF lcRpKind =='D'
    FOR lnL9 = 1 TO 8
      lcBqty = "BSiz"  + ALLTRIM(STR(lnL9))
      lcTqty = "TSiz"  + ALLTRIM(STR(lnL9))
      lcSiz  = "Size"  + ALLTRIM(STR(lnL9))

      IF laSubTotQ[lnL9 + 72] # 0
        REPLACE &lcSiz WITH STR(laSubTotQ[lnL9 + 72] , 5 , 2) + "%"
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--End of lfSubTot.
*!*************************************************************
*! Name      : lfGropTot
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function to calculate the Group total.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGropTot

SELECT (lcExprtFil)
APPEND BLANK
IF lcRpKind =='S'
  REPLACE Style     WITH "GROUP :"            ,;
          Type      WITH &lcTmpFile..StyGroup ,;
          Tot_Units WITH "======"             ,;
          Tot_Value WITH "=========="
ELSE
  REPLACE Style     WITH "GROUP :"            ,;
          Type      WITH &lcTmpFile..StyGroup ,;
          Size1     WITH "======"             ,;
          Size2     WITH "======"             ,;
          Size3     WITH "======"             ,;
          Size4     WITH "======"             ,;
          Size5     WITH "======"             ,;
          Size6     WITH "======"             ,;
          Size7     WITH "======"             ,;
          Size8     WITH "======"             ,;
          Tot_Units WITH "======"             ,;
          Tot_Value WITH "=========="
ENDIF

*--transaction Bought.
IF ASCAN(laTempEx , "BSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Bought"                    ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[9])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[9]))

  IF lcRpKind =='D'
    FOR lnL1 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL1))
      lcVal  = "Value" + ALLTRIM(STR(lnL1))

      IF laGropTotQ[lnL1] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL1])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL1]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction WIP.
IF ASCAN(laTempEx , "WSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "WIP"                        ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[18])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[18]))

  IF lcRpKind =='D'
    FOR lnL2 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL2))
      lcVal  = "Value" + ALLTRIM(STR(lnL2))

      IF laGropTotQ[lnL2 + 9] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL2 + 9])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL2 + 9]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Stock.
IF ASCAN(laTempEx , "SSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Stock"                      ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[27])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[27]))

  IF lcRpKind =='D'
    FOR lnL3 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL3))
      lcVal  = "Value" + ALLTRIM(STR(lnL3))

      IF laGropTotQ[lnL3 + 18] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL3 + 18])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL3 + 18]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Open Order.
IF ASCAN(laTempEx , "OSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Open Order"                 ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[36])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[36]))

  IF lcRpKind =='D'
    FOR lnL4 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL4))
      lcVal  = "Value" + ALLTRIM(STR(lnL4))

      IF laGropTotQ[lnL4 + 27] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL4 + 27])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL4 + 27]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Available.
IF ASCAN(laTempEx , "ASIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Available"                  ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[45])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[45]))

  IF lcRpKind =='D'
    FOR lnL5 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL5))
      lcVal  = "Value" + ALLTRIM(STR(lnL5))

      IF laGropTotQ[lnL5 + 36] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL5 + 36])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL5 + 36]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Shipped.
IF ASCAN(laTempEx , "PSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Shipped"                    ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[54])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[54]))

  IF lcRpKind =='D'
    FOR lnL6 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL6))
      lcVal  = "Value" + ALLTRIM(STR(lnL6))

      IF laGropTotQ[lnL6 + 45] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL6 + 45])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL6 + 45]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction TOTAL Sold.
IF ASCAN(laTempEx , "TSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "TTL Sold"                   ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[63])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[63]))

  IF lcRpKind =='D'
    FOR lnL7 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL7))
      lcVal  = "Value" + ALLTRIM(STR(lnL7))

      IF laGropTotQ[lnL7 + 54] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL7 + 54])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL7 + 54]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Returns.
IF ASCAN(laTempEx , "RSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Returns"                    ,;
          Tot_Units WITH ALLTRIM(STR(laGropTotQ[72])) ,;
          Tot_Value WITH ALLTRIM(STR(laGropTotV[72]))

  IF lcRpKind =='D'
    FOR lnL8 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL8))
      lcVal  = "Value" + ALLTRIM(STR(lnL8))

      IF laGropTotQ[lnL8 + 63] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGropTotQ[lnL8 + 63])) ,;
                &lcVal WITH ALLTRIM(STR(laGropTotV[lnL8 + 63]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction SELL THRU.
*--transaction Bought.
IF ASCAN(laTempEx , "BSIZ1") > 0 AND ASCAN(laTempEx , "TSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type WITH "SELL THRU"

  IF lcRpKind =='D'
    FOR lnL9 = 1 TO 8
      lcBqty = "BSiz"  + ALLTRIM(STR(lnL9))
      lcTqty = "TSiz"  + ALLTRIM(STR(lnL9))
      lcSiz  = "Size"  + ALLTRIM(STR(lnL9))

      IF laGropTotQ[lnL9 + 72] # 0
        REPLACE &lcSiz WITH STR(laGropTotQ[lnL9 + 72] , 5 , 2) + "%"
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--End of lfGropTot.
*!*************************************************************
*! Name      : lfGrndTot
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function to calculate the Grand total.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGrndTot

SELECT (lcExprtFil)
APPEND BLANK
IF lcRpKind =='S'
  REPLACE Style     WITH "Grand Totals :" ,;
          Tot_Units WITH "======"         ,;
          Tot_Value WITH "=========="
ELSE
  REPLACE Style     WITH "Grand Totals :" ,;
          Size1     WITH "======"         ,;
          Size2     WITH "======"         ,;
          Size3     WITH "======"         ,;
          Size4     WITH "======"         ,;
          Size5     WITH "======"         ,;
          Size6     WITH "======"         ,;
          Size7     WITH "======"         ,;
          Size8     WITH "======"         ,;
          Tot_Units WITH "======"         ,;
          Tot_Value WITH "=========="
ENDIF

*--transaction Bought.
IF ASCAN(laTempEx , "BSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Bought"                    ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[9])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[9]))

  IF lcRpKind =='D'
    FOR lnL1 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL1))
      lcVal  = "Value" + ALLTRIM(STR(lnL1))

      IF laGrndTotQ[lnL1] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL1])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL1]))
      ENDIF
   ENDFOR
 ENDIF
ENDIF

*--transaction WIP.
IF ASCAN(laTempEx , "WSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "WIP"                        ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[18])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[18]))

  IF lcRpKind =='D'
    FOR lnL2 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL2))
      lcVal  = "Value" + ALLTRIM(STR(lnL2))

      IF laGrndTotQ[lnL2 + 9] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL2 + 9])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL2 + 9]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Stock.
IF ASCAN(laTempEx , "SSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Stock"                      ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[27])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[27]))

  IF lcRpKind =='D'
    FOR lnL3 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL3))
      lcVal  = "Value" + ALLTRIM(STR(lnL3))

      IF laGrndTotQ[lnL3 + 18] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL3 + 18])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL3 + 18]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Open Order.
IF ASCAN(laTempEx , "OSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Open Order"                 ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[36])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[36]))

  IF lcRpKind =='D'
    FOR lnL4 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL4))
      lcVal  = "Value" + ALLTRIM(STR(lnL4))

      IF laGrndTotQ[lnL4 + 27] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL4 + 27])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL4 + 27]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Available.
IF ASCAN(laTempEx , "ASIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Available"                  ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[45])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[45]))

  IF lcRpKind =='D'
    FOR lnL5 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL5))
      lcVal  = "Value" + ALLTRIM(STR(lnL5))

      IF laGrndTotQ[lnL5 + 36] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL5 + 36])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL5 + 36]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Shipped.
IF ASCAN(laTempEx , "PSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Shipped"                    ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[54])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[54]))

  IF lcRpKind =='D'
    FOR lnL6 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL6))
      lcVal  = "Value" + ALLTRIM(STR(lnL6))

      IF laGrndTotQ[lnL6 + 45] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL6 + 45])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL6 + 45]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction TOTAL Sold.
IF ASCAN(laTempEx , "TSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "TTL Sold"                   ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[63])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[63]))

  IF lcRpKind =='D'
    FOR lnL7 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL7))
      lcVal  = "Value" + ALLTRIM(STR(lnL7))

      IF laGrndTotQ[lnL7 + 54] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL7 + 54])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL7 + 54]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction Returns.
IF ASCAN(laTempEx , "RSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type      WITH "Returns"                    ,;
          Tot_Units WITH ALLTRIM(STR(laGrndTotQ[72])) ,;
          Tot_Value WITH ALLTRIM(STR(laGrndTotV[72]))

  IF lcRpKind =='D'
    FOR lnL8 = 1 TO 8
      lcSiz  = "Size"  + ALLTRIM(STR(lnL8))
      lcVal  = "Value" + ALLTRIM(STR(lnL8))

      IF laGrndTotQ[lnL8 + 63] # 0
        REPLACE &lcSiz WITH ALLTRIM(STR(laGrndTotQ[lnL8 + 63])) ,;
                &lcVal WITH ALLTRIM(STR(laGrndTotV[lnL8 + 63]))
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--transaction SELL THRU.
*--transaction Bought.
IF ASCAN(laTempEx , "BSIZ1") > 0 AND ASCAN(laTempEx , "TSIZ1") > 0
  SELECT (lcExprtFil)
  APPEND BLANK
  REPLACE Type WITH "SELL THRU"

  IF lcRpKind =='D'
    FOR lnL9 = 1 TO 8
      lcBqty = "BSiz"  + ALLTRIM(STR(lnL9))
      lcTqty = "TSiz"  + ALLTRIM(STR(lnL9))
      lcSiz  = "Size"  + ALLTRIM(STR(lnL9))

      IF laGrndTotQ[lnL9 + 72] # 0
        REPLACE &lcSiz WITH STR(laGrndTotQ[lnL9 + 72] , 5 , 2) + "%"
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*--End of lfGrndTot.
*!*************************************************************
*! Name      : lfWipColct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 14/05/2002
*! Purpose   : Function to calculate WIP in case the user 
*!           : didn't select one of the components of the equation.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfWipColct

SELECT (lcTmpFile)
REPLACE llPrceAvlb WITH .T.
    
SELECT STYLE
IF !('STYLE.STYLE' $ SET('RELATION'))
  SET RELATION TO STYLE.STYLE INTO POSLN   ADDITIVE
 
  SELECT POSLN
  SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE

ENDIF

IF SEEK(STYLE.STYLE , "POSLN")
  SELECT POSLN
  SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = STYLE.STYLE FOR POSHDR.STATUS = "O" AND POSLN.TRANCD = "1"

    IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , POSHDR.ENTERED  = POSHDR.ENTERED ,;
      IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , POSHDR.ENTERED <= ldRPPHDate , BETWEEN(POSHDR.ENTERED,ldRPPLDate,ldRPPHDate)))

      SELECT (lcTmpFile)
      REPLACE ASiz1 WITH ASiz1 + STYLE.WIP1                   ,;
              ASiz2 WITH ASiz2 + STYLE.WIP2                   ,;
              ASiz3 WITH ASiz3 + STYLE.WIP3                   ,;
              ASiz4 WITH ASiz4 + STYLE.WIP4                   ,;
              ASiz5 WITH ASiz5 + STYLE.WIP5                   ,;
              ASiz6 WITH ASiz6 + STYLE.WIP6                   ,;
              ASiz7 WITH ASiz7 + STYLE.WIP7                   ,;
              ASiz8 WITH ASiz8 + STYLE.WIP8                   ,;
              AAmt1 WITH AAmt1 + (STYLE.WIP1 * STYLE.PRICEA ) ,;
              AAmt2 WITH AAmt2 + (STYLE.WIP2 * STYLE.PRICEA ) ,;
              AAmt3 WITH AAmt3 + (STYLE.WIP3 * STYLE.PRICEA ) ,;
              AAmt4 WITH AAmt4 + (STYLE.WIP4 * STYLE.PRICEA ) ,;
              AAmt5 WITH AAmt5 + (STYLE.WIP5 * STYLE.PRICEA ) ,;
              AAmt6 WITH AAmt6 + (STYLE.WIP6 * STYLE.PRICEA ) ,;
              AAmt7 WITH AAmt7 + (STYLE.WIP7 * STYLE.PRICEA ) ,;
              AAmt8 WITH AAmt8 + (STYLE.WIP8 * STYLE.PRICEA )

      REPLACE TotASiz WITH TotASiz + ASiz1 + ASiz2 + ASiz3 + ASiz4 + ASiz5 + ASiz6 + ASiz7 + ASiz8 ,;
              TotAAmt WITH TotAAmt + AAmt1 + AAmt2 + AAmt3 + AAmt4 + AAmt5 + AAmt6 + AAmt7 + AAmt8
    ENDIF
  ENDSCAN
ENDIF

*--Section break the relations for this section.
SELECT STYLE
SET RELATION TO
SELECT POSLN
SET RELATION TO

SELECT (lcTmpFile)
REPLACE ASiz1 WITH ASiz1 + STYLE.STK1                   ,;
        ASiz2 WITH ASiz2 + STYLE.STK2                   ,;
        ASiz3 WITH ASiz3 + STYLE.STK3                   ,;
        ASiz4 WITH ASiz4 + STYLE.STK4                   ,;
        ASiz5 WITH ASiz5 + STYLE.STK5                   ,;
        ASiz6 WITH ASiz6 + STYLE.STK6                   ,;
        ASiz7 WITH ASiz7 + STYLE.STK7                   ,;
        ASiz8 WITH ASiz8 + STYLE.STK8                   ,;
        AAmt1 WITH AAmt1 + (STYLE.STK1 * STYLE.PRICEA ) ,;
        AAmt2 WITH AAmt2 + (STYLE.STK2 * STYLE.PRICEA ) ,;
        AAmt3 WITH AAmt3 + (STYLE.STK3 * STYLE.PRICEA ) ,;
        AAmt4 WITH AAmt4 + (STYLE.STK4 * STYLE.PRICEA ) ,;
        AAmt5 WITH AAmt5 + (STYLE.STK5 * STYLE.PRICEA ) ,;
        AAmt6 WITH AAmt6 + (STYLE.STK6 * STYLE.PRICEA ) ,;
        AAmt7 WITH AAmt7 + (STYLE.STK7 * STYLE.PRICEA ) ,;
        AAmt8 WITH AAmt8 + (STYLE.STK8 * STYLE.PRICEA )

REPLACE TotASiz WITH TotASiz + ASiz1 + ASiz2 + ASiz3 + ASiz4 + ASiz5 + ASiz6 + ASiz7 + ASiz8 ,;
        TotAAmt WITH TotAAmt + AAmt1 + AAmt2 + AAmt3 + AAmt4 + AAmt5 + AAmt6 + AAmt7 + AAmt8


SELECT STYLE
IF !('STYLE.STYLE' $ SET('RELATION'))
  SET RELATION TO STYLE.STYLE INTO ORDLINE ADDITIVE

  SELECT ORDLINE
  SET RELATION TO "O" + ORDLINE.ORDER INTO ORDHDR ADDITIVE
ENDIF

IF SEEK(STYLE.STYLE , "ORDLINE")
  SELECT ORDLINE
  SCAN REST WHILE STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER + STORE + STR(LINENO,6) = STYLE.STYLE FOR ORDHDR.STATUS $ "OH"

    IF IIF(ldRPPLDate = {} .AND. ldRPPHDate = {} , ORDHDR.ENTERED  = ORDHDR.ENTERED ,;
       IIF(ldRPPLDate = {} .AND. ldRPPHDate <>{} , ORDHDR.ENTERED <= ldRPPHDate , BETWEEN(ORDHDR.ENTERED,ldRPPLDate,ldRPPHDate)))

      SELECT (lcTmpFile)
      REPLACE ASiz1 WITH ASiz1 - ORDLINE.QTY1                    ,;
              ASiz2 WITH ASiz2 - ORDLINE.QTY2                    ,;
              ASiz3 WITH ASiz3 - ORDLINE.QTY3                    ,;
              ASiz4 WITH ASiz4 - ORDLINE.QTY4                    ,;
              ASiz5 WITH ASiz5 - ORDLINE.QTY5                    ,;
              ASiz6 WITH ASiz6 - ORDLINE.QTY6                    ,;
              ASiz7 WITH ASiz7 - ORDLINE.QTY7                    ,;
              ASiz8 WITH ASiz8 - ORDLINE.QTY8                    ,;
              AAmt1 WITH AAmt1 - (ORDLINE.QTY1 * ORDLINE.PRICE ) ,;
              AAmt2 WITH AAmt2 - (ORDLINE.QTY2 * ORDLINE.PRICE ) ,;
              AAmt3 WITH AAmt3 - (ORDLINE.QTY3 * ORDLINE.PRICE ) ,;
              AAmt4 WITH AAmt4 - (ORDLINE.QTY4 * ORDLINE.PRICE ) ,;
              AAmt5 WITH AAmt5 - (ORDLINE.QTY5 * ORDLINE.PRICE ) ,;
              AAmt6 WITH AAmt6 - (ORDLINE.QTY6 * ORDLINE.PRICE ) ,;
              AAmt7 WITH AAmt7 - (ORDLINE.QTY7 * ORDLINE.PRICE ) ,;
              AAmt8 WITH AAmt8 - (ORDLINE.QTY8 * ORDLINE.PRICE )

      REPLACE TotASiz WITH TotASiz - (ASiz1 + ASiz2 + ASiz3 + ASiz4 + ASiz5 + ASiz6 + ASiz7 + ASiz8) ,;
              TotAAmt WITH TotAAmt - (AAmt1 + AAmt2 + AAmt3 + AAmt4 + AAmt5 + AAmt6 + AAmt7 + AAmt8)
    ENDIF
  ENDSCAN
ENDIF

*--Section break the relations for this section.
SELECT STYLE
SET RELATION TO
SELECT ORDLINE
SET RELATION TO

SELECT (lcTmpFile)
STORE 0 TO lnTotASiz , lnTotAAmt
FOR lnLop = 1 TO 8
  lnFld = ALLTRIM(STR(lnLop))
  IF ASiz&lnFld # 0
    lnTotASiz = lnTotASiz + ASiz&lnFld
    lnTotAAmt = lnTotAAmt + AAmt&lnFld
  ELSE
    REPLACE AAmt&lnFld WITH 0
  ENDIF
ENDFOR

SELECT (lcTmpFile)
REPLACE TotASiz WITH lnTotASiz ,;
        TotAAmt WITH lnTotAAmt

*--End of lfWipColct.