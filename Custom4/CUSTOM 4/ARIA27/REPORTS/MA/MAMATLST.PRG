*:***************************************************************************
*: Program file  : MAMATLST
*: Program desc. : Material master list
*: For Report    : 
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
*: Example : DO ICSTYLST
*:***************************************************************************
*B602602,1  HS  04/18/99  Make the "Item type" filter option work.
*B602353,1  HS  04/18/99  When printing the "On Hand Amount" use the field
*B602353,1                "nStkVal" instead of recalculating the "On Hand
*B602353,1                Amount" in all cases except the case of using
*B602353,1                the Standard Cost Method.
*B802256,1 SSH 12/05/1999 Change every costs picture to become 3 decimal. 
*B802245,1 HDM 05/22/1999 Increase on hand amount Picture by 1 Digit & decrease
*                         on Order Picture by 1 Digit
*E500266,1 MAN 05/27/1999 Add Fabric & Color to the OG
*B802600,1 HDM 09/21/1999 Remove Fabric from option grid filter as 
*                         there's another filter [Item range]
*                         that does the same data filtering
*E301322,1 HDM 09/27/1999 Modify the program as we changed the report to FRX
*                         Formate instead of @x,y Say
*:E301271,1 SSH Add Option to select Bins
*:B603291,1 HDM 11/21/1999 Dyelot field disabled while the material 
*:                         module uses dyelots
*:B803423,1 ADEL 07/26/2000 Adjust the TEXT frx (MAMATLSA) for the Item Desc field to be 20 chrs.
*:C102161,1 KAM  05/02/2001 support print warehouse detail and
*:C102161,1 KAM             print only this WH options
*:B604525,1 HBG 01/07/2001 Print 'On Hand Amount ,  On Order Qty/Amount ,  Usage qty'
*:B604525,1                For selected Warehouse 
*:B607075,1 ABD 03/24/2003 Problems with Inventory Report, some style appear 
*:B607075,1                With zero onhand while it have a stock in fabric and fabdye files.
*:***************************************************************************
*-- lcVenFile Variable that hold Vendor file name
lcVenFile = 'ApVendor'

*-- llDontPrn variable that is used to prevent printing if there is not
*--           any record matches the report criteria
llDontPrn = .F.

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'
R_TITLE  = 'MATERIAL MASTER LIST '

*-- llCostAccs variable that showes if the user has costing access or not
llCostAccs = gfUserPriv('IC','ICSTYLE')
qCostPrv   = llCostAccs

*-- lcMtCstMth variable that hold the system costing method
lcMtCstMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET'))) 

*-- llDyelot   variable that showes if the system uses dyelot or not
*-- llRPPrnDye variable in option grid that showes if the user choes to print
*--            dyelot
XDYELOT_S = llDyelot
XDYELOT_P = llRPPrnDye

*** LOOP FOR REPORT CRITERIA ***
XVENDOR   = lcRPVendor
LFAB      = lcRPFrmFab
HFAB      = lcRPToFab
XTITLE    = lcRPTitle
XSORT     = lcRPSortBy
XITEM_TYP = lcRPItmTyp
XRFORM    = lcRPForm
XONHAND_Q = IIF(llRPOnhand,'Y','N')

XFILTER = ''
XFILTER = XFILTER + IIF(!EMPTY(XVENDOR),'VENDOR=XVENDOR','')

*B602602,1 Add this line to add the "Item type" filtration [Begin]
*xFilter = xFilter + IIF(EMPTY(xFilter) AND EMPTY(xItem_Typ) ,;
                        '' , ' .AND. ') + 'Item_Type = xItem_Typ'
                        
                        
IF EMPTY(xFilter)
  IF !EMPTY(xItem_Typ)
    XFILTER = 'Item_Type = xItem_Typ'
  ENDIF
ELSE
  IF !EMPTY(xItem_Typ)
    XFILTER = XFILTER + ' .AND. Item_Type = xItem_Typ'
  ENDIF
  
ENDIF

*B602602,1 Add this line to add the "Item type" filtration [End]

IF !EMPTY(lcRPFrmFab) AND !EMPTY(lcRPToFab)
  IF EMPTY(XFILTER)
    XFILTER = XFILTER +' FABRIC>=LFAB.AND.FABRIC<=HFAB'
  ELSE
    XFILTER = XFILTER +' .AND.FABRIC>=LFAB.AND.FABRIC<=HFAB'
  ENDIF
ENDIF
IF XONHAND_Q= 'Y'
  IF EMPTY(XFILTER)
    XFILTER = XFILTER + 'ONHAND <> 0'
  ELSE
    XFILTER = XFILTER +' .AND. ONHAND<>0'
  ENDIF
ENDIF
*C102161,1 KAM  set relation between fabric and fabdye[start]
SELECT FABRIC
SET RELATION TO Fabric + Color + ALLTRIM(lcRpWare) INTO FABDYE ADDITIVE
*C102161,1 KAM [end]
DO lpPrint
*IF !llDontPrn
*  DO ENDREPORT
*ENDIF  


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

*lcFormName = 'MAMATLSA.FRX'
*B603291,1[START] We should get the dyelot setup from MA module
*           Not IC module setup
*llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')
llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
*B603291,1[END]

lnPrnDyeP = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPPrnDye'),1)
laOGObjCnt[lnPrnDyeP] = llDyelot
= lfOGShowGet('llRPPrnDye')
*lcRpRName = 'MAMATLSA.FRX'


*!*************************************************************
*! Name      : lfvVendor
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************

FUNCTION lfvVendor

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcRPVendor) .AND. ;
   ('?' $ lcRPVendor .OR. !SEEK(lcRPVendor , 'APVENDOR'))
  =gfApVnBrow(@lcRPVendor)
ENDIF

*!*************************************************************
*! Name      : lfvFabBet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Showes range screen for fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabBet()
*!*************************************************************

FUNCTION lfvFabBet

*-- Here do the between screen

lcTitle = 'Item'

STORE lcRPFrmFab TO lcFromFab
STORE lcRPToFab TO lcToFab

DO (gcRepHome + gcAct_Appl + '\FabRng.SPR')

lcRPFrmFab = lcFromFab
lcRpToFab  = lcToFab
 
*!*************************************************************
*! Name      : lfvFabRng
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Vaildate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabRng()
*!*************************************************************

FUNCTION lfvFabRng

*-- this is the validation of from_item

lcRngVarNa = VARREAD()
lcRngVarVa = &lcRngVarNa.

IF !EMPTY(lcRngVarVa)
  SELECT FABRIC
  IF !SEEK(lcRngVarVa)
    DO FABROW WITH lcRngVarVa,'*'
    &lcRngVarNa = lcRngVarVa
  ENDIF
ENDIF

SHOW GET &lcRngVarNa

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Vailadet range screen's OK button 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************

FUNCTION lfvpbOk

*-- this is the validation of to_item

IF !EMPTY(lcFromFab) .OR. !EMPTY(lcToFab)
  IF EMPTY(lcToFab) 
    lcToFab = lcFromFab
  ENDIF
  IF lcFromFab > lcToFab
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFromFab)
  ELSE
    lcRPFrmFab = lcFromFab
    lcRPToFab  = lcToFab
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lpPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************

PROCEDURE lpPrint



DO WHILE .T.
  SELECT FABRIC
  *-- HDM E301322,1 [Start] Change the way we collect report data
  *--                       to include dyelots Information too
  
  =AFIELDS(laFileStru)
  lnArrLen = ALEN(laFileStru,1)
  *C102161,1 KAM[start]
  *--KAM increase the size of array
  *DIMENSION laFileStru[lnArrLen+6,4]
  DIMENSION laFileStru[lnArrLen+9,4]
  *C102161,1 KAM
  laFileStru[lnArrLen+1,1] = 'Dyelot'
  laFileStru[lnArrLen+1,2] = 'C'
  laFileStru[lnArrLen+1,3] = 10
  laFileStru[lnArrLen+1,4] = 0

  laFileStru[lnArrLen+2,1] = 'UseDye'
  laFileStru[lnArrLen+2,2] = 'L'
  laFileStru[lnArrLen+2,3] = 1
  laFileStru[lnArrLen+2,4] = 0

  laFileStru[lnArrLen+3,1] = 'NEXTDYE'
  laFileStru[lnArrLen+3,2] = 'L'
  laFileStru[lnArrLen+3,3] = 1
  laFileStru[lnArrLen+3,4] = 0

  laFileStru[lnArrLen+4,1] = 'ClrDesc'
  laFileStru[lnArrLen+4,2] = 'C'
  laFileStru[lnArrLen+4,3] = 12
  laFileStru[lnArrLen+4,4] = 0
  
  laFileStru[lnArrLen+5,1] = 'ItemDesc'
  laFileStru[lnArrLen+5,2] = 'C'
  laFileStru[lnArrLen+5,3] = 20
  laFileStru[lnArrLen+5,4] = 0

  *C102161,1 KAM[start]
  *--kam add field for warehouse ,itemtype  and nextware for using in printing
  *laFileStru[lnArrLen+6,1] = 'lLast'
  *laFileStru[lnArrLen+6,2] = 'L'
  *laFileStru[lnArrLen+6,3] = 1
  *laFileStru[lnArrLen+6,4] = 0

  laFileStru[lnArrLen+6,1] = 'cWARECODE'
  laFileStru[lnArrLen+6,2] = 'C'
  laFileStru[lnArrLen+6,3] = 6
  laFileStru[lnArrLen+6,4] = 0
  
  
  laFileStru[lnArrLen+7,1] = 'ITEMTYPE'
  laFileStru[lnArrLen+7,2] = 'C'
  laFileStru[lnArrLen+7,3] = 1
  laFileStru[lnArrLen+7,4] = 0


  laFileStru[lnArrLen+8,1] = 'NEXTWARE'
  laFileStru[lnArrLen+8,2] = 'L'
  laFileStru[lnArrLen+8,3] = 1
  laFileStru[lnArrLen+8,4] = 0

  laFileStru[lnArrLen+9,1] = 'lLast'
  laFileStru[lnArrLen+9,2] = 'L'
  laFileStru[lnArrLen+9,3] = 1
  laFileStru[lnArrLen+9,4] = 0
*C102161,1 KAM [end]
  
  WORKFILE = gfTEMPNAME()
  CREATE TABLE (gcWorkDir + WORKFILE + '.DBF') FROM ARRAY laFileStru
  IF USED (WORKFILE)
    USE IN (WORKFILE)
  ENDIF
  USE &gcWorkDir.&WORKFILE EXCLUSIVE IN 0
  *C102161,1 KAM [start]
  *--put indexing here and create another index for using in search for warehouse
  IF XSORT='I'
    INDEX ON FABRIC+COLOR+ITEMTYPE TAG &WORKFILE
    lcGrpField = 'FABRIC'
  ELSE
    INDEX ON VENDOR+FABRIC+COLOR+ITEMTYPE TAG &WORKFILE
    lcGrpField = 'VENDOR'
  ENDIF
  INDEX ON FABRIC+cWARECODE+ITEMTYPE TAG "lcIdxFle" &&index using by KAM
  SET ORDER TO TAG &WORKFILE
  *--add lcRpware to filter expression 
  IF !EMPTY(lcRpWARE)
    IF !EMPTY(XFILTER)
      XFILTER = XFILTER + '  AND '
    ENDIF
    XFILTER = XFILTER +  ' FABDYE.cWARECODE=lcRpWare '
  ENDIF
  *C102161,1 KAM [end]
 *-- Start Collecting Data
  SELECT FABRIC
  IF !EMPTY(XFILTER)
    XFILTER = XFILTER + ".AND."
  ENDIF  
  XFILTER = XFILTER + lcRpExp
  IF !EMPTY(XFILTER)
    LOCATE ALL FOR &XFILTER
  ELSE
    GOTO TOP
  ENDIF
  IF EOF()
    WAIT WINDOW 'NO RECORDS SELECTED FOR REPORT!' NOWAIT
    llDontPrn = .T.
    RETURN
  ENDIF
  IF EMPTY(XFILTER)
    XFILTER = '.T.'
  ENDIF
  SCAN FOR &XFILTER
    WAIT WINDOW 'Collecting Data For Fabric ' + Fabric.fabric + ' Color ' + Fabric.Color NOWAIT
    lcClrDesc = SUBSTR(gfCodDes(COLOR,'COLOR'),1,12)
    XTYP_DESC = SUBSTR(gfCodDes(ITEM_tYPE,'ITEM_TYPE'),1,20)
    SCATTE MEMVAR MEMO
    SELECT (WORKFILE)
    APPEND BLANK
    GATHER MEMVAR MEMO
    *C102161,1 KAM[start] replace itemtype='A' for each record for that fabric
    *REPLACE UseDye   WITH .F.,;
            NEXTDYE  WITH (llDyelot AND llRPPrnDye AND FABRIC.CDYE_FLG = 'Y' AND SEEK(FABRIC.FABRIC+FABRIC.COLOR,'FABDYE')),;
            ClrDesc  WITH lcClrDesc,;
            ItemDesc WITH XTYP_DESC
    *B607075,1 ABD - Problems with Inventory Report, some style appear 
    *B607075,1 ABD - With zero onhand while it have a stock in fabric and fabdye files,
    *B607075,1 ABD - that becasue the program seek in the fabdye Although he madE a relation with fabdye. [Begin]

    *REPLACE UseDye   WITH .F.,;
            NEXTDYE  WITH (llDyelot AND llRPPrnDye AND FABRIC.CDYE_FLG = 'Y' AND SEEK(FABRIC.FABRIC+FABRIC.COLOR,'FABDYE')),;
            ClrDesc  WITH lcClrDesc,;
            ItemDesc WITH XTYP_DESC,;
            ITEMTYPE WITH 'A'
    REPLACE UseDye   WITH .F.,;
            NEXTDYE  WITH (llDyelot AND llRPPrnDye AND FABRIC.CDYE_FLG = 'Y' AND !EOF('FABDYE')),;
            ClrDesc  WITH lcClrDesc,;
            ItemDesc WITH XTYP_DESC,;
            ITEMTYPE WITH 'A'
    *B607075,1 ABD - [End]
    *--in case only this warehouse we use fabdye.onhand
     IF !EMPTY(lcRpWarE)
       REPLACE ONHAND WITH FABDYE.ONHAND
       *B604525,1 HBG 01/07/2001 Print 'On Hand Amount ,  On Order Qty/Amount ,  Usage qty'
       *B604525,1                For selected Warehouse [Begin]
       REPLACE ONORDER    WITH FABDYE.ONORDER 
       REPLACE USAGE      WITH FABDYE.USAGE 
       REPLACE nStkVal    WITH FABDYE.nStkVal
       REPLACE nAveCstBuy WITH FABDYE.nAveCstBuy
       *B604525,1 [End]
     ENDIF
    *C102161,1 KAM [end]

    IF llDyelot AND llRPPrnDye AND FABRIC.CDYE_FLG = 'Y'
      *-- So We Have Dyelots to print
      SELECT FABDYE
      IF SEEK(FABRIC.FABRIC+FABRIC.COLOR)
        *B607075,1 ABD - Scan for a Specific warehouse. [Begin]
        *SCAN REST WHILE FABRIC+COLOR = FABRIC.FABRIC+FABRIC.COLOR
        lcScnExp = ' FOR '+ IIF(EMPTY(lcRpWare),'.T.', ' Fabdye.CwareCode = lcRpWare ')
        SCAN REST WHILE FABRIC+COLOR = FABRIC.FABRIC+FABRIC.COLOR &lcScnExp
        *B607075,1 ABD - [End]
        
          WAIT WINDOW 'Collecting Dyelots Information For Fabric ' + Fabric.fabric + ' Color ' + Fabric.Color NOWAIT
           SCATTE MEMVAR MEMO
          SELECT (WORKFILE)
          APPEND BLANK
          GATHER MEMVAR MEMO
          *C102161,1 KAM  add itemtype to replacement statement[start]
           *REPLACE CDYE_FLG WITH FABRIC.CDYE_FLG,;
                    UseDye   WITH .T.
           *--KAM add replace itemtype for each deylot record
           REPLACE CDYE_FLG WITH FABRIC.CDYE_FLG,;
                   UseDye   WITH .T.,;
                   ITEMTYPE WITH 'B'
           *C102161,1 KAM [end]              
        ENDSCAN
      ENDIF
    ENDIF
    *C102161,1 KAM [start]
    IF llRPPntW        && check if user select print warehous details
      SELECT(WORKFILE)
      GO BOTTOM
      lnRecFld=RECNO()
      IF lfGetWLine(FABRIC.FABRIC,FABRIC.COLOR)         && calling function to get warehous detail
        SELECT(WORKFILE)
        IF BETWEEN(lnRecFld,1,RECCOUN())
          GO lnRecFld
          REPLACE  NEXTDYE WITH  .T. ,;     
                   NEXTWARE WITH .T. ,;     
                   ITEMTYPE WITH 'B'
        ENDIF
      ENDIF        
    ENDIF
    *C102161,1 KAM [end]
  ENDSCAN
  SELECT (WORKFILE)
  Z = LTRIM(STR(RECCOUNT(),7))
  WAIT WINDOW 'SORTING ' + Z +' RECORDS FOR MATERIAL MASTER LIST ...' NOWAIT
  *C102161,1 KAM [start]
  *--KAM STOPPING THE INDEX STATEMENTS ( at the top we have create the index )
  * SORT TO WORKFILE INDEX
  *IF XSORT='I'
  * INDEX ON FABRIC+COLOR TAG &WORKFILE
  * lcGrpField = 'FABRIC'
  *ELSE
  *  INDEX ON VENDOR+FABRIC+COLOR TAG &WORKFILE
  *  lcGrpField = 'VENDOR'
  *ENDIF
  *C102161,1KAM[end]
  SET ORDER TO TAG &WORKFILE
  SET RELATION TO VENDOR INTO &lcVenFile
  
  *WAIT WINDOW 'Selecting records for report ...' NOWAIT
  *IF EMPTY(XFILTER)
  *  COPY TO &gcWorkDir.&WORKFILE
  *ELSE
  *  COPY REST TO &gcWorkDir.&WORKFILE FOR &XFILTER
  *ENDIF
  SELECT FABRIC
  SET RELATION TO
  SELECT (WORKFILE)
  GO BOTTOM
  REPLACE lLast WITH .T.
  GO TOP
  
  DO gfDispRe WITH EVAL('lcRPForm') &&, 'FOR ' + lcRpExp
  
RETURN
*-- HDM E301322,1 [End]

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT910                                  MATERIAL MASTER LIST - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*
*
*ITEM                 COLOR        VENDOR     UOM BUY       COST  COST BUY  COST USE  REORDER   ONHAND     ONHAND  ONORDER    ONORDER
*....DESCRIPTION....  DESCRIPTION. LOCATION   UOM USE        TAX                          USAGE              AMOUNT              AMOUNT
*ITEM TYPE                         PATTERN    CONV.        QUOTA
*WEIGHT               INTRODUCED   LEADTIME              FREIGHT  ..........................CONTENTS..........................
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*XXXXXXX              XXXXXX       XXXXXXXX   XXX      999999.99 999999.99 999999.99 99999999 99999999 9999999.99 99999999 9999999.99
*XXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXX XXXXX      XXX      999999.99                       99999999
*XX XXXXXXXXXXXXXXX                XXXXXXXXXX 9999.99  999999.99
*XXXXXXXXXXXXXXX      MM/DD/YY     XX                  999999.99  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*<-------------------------------------<< DYELOTS >>--------------------------------------------------------------------------------->
*                     DYELOT #                            ONHAND    USAGE
*                     XXXXXXXXXX                        99999999 99999999

  DIMENSION XTOTAL(4),GTOTAL(4)
  XTOTAL = 0.00
  GTOTAL = 0.00
  PAGENO = 0
  ROW    = 99
  XTIME  = TIME()
  IF XSORT='I'
    BREAK  = 'FABRIC'
  ELSE
    BREAK  = 'VENDOR'
  ENDIF
  SELECT (WORKFILE)
  GOTO TOP
  HBREAK=SPACE(1)
  IF !EMPTY(BREAK)
     HBREAK = &BREAK
  ENDIF
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  
  
  XPRT_DYE=.F.         && flag used when printing dyelot info. to not to repeat
                       && the fabric/clr info when switching to new page ...
  SELECT (WORKFILE)

  *-- [REPORT] LOOP
  IF XRFORM='B'
    DO MAT911               && PROCEDURE (TO EXECUTE FORMAT B)
  ENDIF

   DO WHILE (INKEY() <>32 .AND. XRFORM='A')
     IF ROW >=53
        PAGENO = PAGENO+1
        XREPORT='MAT910'
        XTITLE=' '
        R_TITLE='MATERIAL MASTER LIST'
        DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH

        IF lcMtCstMth $ 'ALIF'
          @ 05,00 SAY '                                                           STD.    AVE.      AVE.'
        ELSE
          @ 05,00 SAY '                                                           STD.    STD.      STD.'
        ENDIF
        @ 06,00 SAY 'ITEM                 COLOR        VENDOR     UOM BUY      PRICE  COST BUY  COST USE  REORDER   ONHAND       ONHAND ONORDER    ONORDER '
        @ 07,00 SAY '....DESCRIPTION....  DESCRIPTION  LOCATION   UOM USE        TAX                        USAGE                AMOUNT             AMOUNT'
        @ 08,00 SAY 'ITEM TYPE            WIDTH        PATTERN      CONV.      QUOTA'
        @ 09,00 SAY 'WEIGHT               INTRODUCED   LEADTIME              FREIGHT  ..........................CONTENTS.......................... '

        ROW=10
        IF XPRT_DYE            && IF IN THE MIDDLE OF PRINTING DYELOTS
          @ 10,00 SAY REPLICATE('-',40)
          @ 10,41 SAY '<< DYELOTS >>'
          @ 10,54 SAY REPLICATE('-',77)
          @ 11,21 SAY 'DYELOT #'
          @ 11,57 SAY 'ONHAND'
          @ 11,67 SAY 'USAGE'
          ROW=12
        ENDIF
     ENDIF

     DO WHILE !EMPTY(BREAK)
       SELECT (WORKFILE)
       IF &BREAK = HBREAK
         EXIT
       ENDIF
       ROW=ROW+1
       @ ROW,00 SAY REPLICATE('=',132)
       ROW = ROW+1
       @ ROW,000 SAY '* SUB TOTAL * '
       @ ROW,020 SAY HBREAK
       @ ROW,093 SAY XTOTAL(1)  PICTURE '99999999'
       @ ROW,103 SAY XTOTAL(2)  PICTURE '99999999.99'
       @ ROW,113 SAY XTOTAL(3)  PICTURE '99999999'
       @ ROW,122 SAY XTOTAL(4)  PICTURE '9999999.99'
       ROW = ROW+1
       @ ROW,00 SAY REPLICATE('=',132)
       STORE 0 TO XTOTAL
       HBREAK = &BREAK
       EXIT
     ENDDO
     *--------------------- END SUBTOTALS ----------------------------
     IF !XPRT_DYE
       IF EOF()
          EXIT
       ENDIF
       IF ROW >=53
          ROW = 99
          LOOP
       ENDIF
       SELECT (WORKFILE)
       XCLR=COLOR
       Z = ' '
       CLRDESC   = SUBSTR(gfCodDes(XCLR,'COLOR'),1,12)
       XTYP_DESC = gfCodDes(&WORKFILE->ITEM_tYPE,'ITEM_TYPE')
       SELECT (WORKFILE)
       @ ROW,000 SAY REPLICATE('-',132)
       ROW = ROW+1
       @ ROW,000 SAY FABRIC
       @ ROW,021 SAY COLOR
       @ ROW,034 SAY VENDOR
       @ ROW,045 SAY UOMBUY

       IF qCostPrv        && COSTING ACCESS.
         @ ROW,054 SAY nFabCost PICTURE '99999.999'
         @ ROW,064 SAY IIF(lcMtCstMth $ 'ALIF' ,nAveCstBuy,CostBuy) PICTURE '99999.999'
         @ ROW,074 SAY (IIF(lcMtCstMth $ 'ALIF' ,nAveCstBuy,CostBuy)/Conv) PICTURE '99999.999'
       ENDIF
       @ ROW,084 SAY REORDER PICTURE '99999999'
       @ ROW,093 SAY ONHAND PICTURE '99999999'

       IF qCostPrv          && COSTING ACCES.
         
         *B602353,1 Change this line to use the field "nStkVal" instead of
         *          recalculating the On Hand Amount in all cases except
         *          the case of using the Standard Cost Method [Begin]
         *@ ROW,102 SAY (ONHAND*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
         *          PICTURE '9999999.99'
         *-- B802245,1 HDM 05/22/1999  re-position on hand amount field [START]
         *@ ROW,102 SAY IIF(lcMtCstMth $ 'ALIF' ,;
                           nStkVal , OnHand * (CostBuy / Conv)) ;
                   PICTURE '9999999.99'

         
         @ ROW,103 SAY IIF(lcMtCstMth $ 'ALIF' ,;
                           nStkVal , OnHand * (CostBuy / Conv)) ;
                   PICTURE '99999999.99'
         *-- B802245,1 HDM 05/22/1999  [END]
         *B602353,1 Change this line to use the field "nStkVal" [End]
         
       ENDIF
       *-- B802245,1 HDM 05/22/1999  re-position on order amount field[START]
       *@ ROW,113 SAY ONORDER PICTURE '99999999'
       
       @ ROW,113 SAY ONORDER PICTURE '9999999'
       *-- B802245,1 HDM[END]

       IF qCostPrv          && COSTING ACCESS.
         @ ROW,122 SAY (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
                   PICTURE '9999999.99'
       ENDIF
       ROW = ROW+1

       @ ROW,000 SAY DESC
       @ ROW,021 SAY CLRDESC
       @ ROW,034 SAY LOC
       @ ROW,045 SAY UOMUSE

       IF qCostPrv          && COSTING ACCESS.
         *B#802256,1 SSH 12/05/1999  Change every costs picture to become 3 decimal. 
         *@ ROW,054 SAY nItem_Tax PICTURE '999999.99'
         @ ROW,053 SAY nItem_Tax PICTURE '999999.999'
         *B#802256,1 SSH (END)
         @ ROW,084 SAY USAGE PICTURE '99999999'
       ENDIF
       ROW = ROW+1

       @ ROW,000 SAY ITEM_tYPE + ' ' + SUBSTR(XTYP_DESC,1,13)
       @ ROW,021 SAY WIDTH
       @ ROW,034 SAY PATTERN
       @ ROW,043 SAY CONV PICTURE '9999.999'
       IF qCostPrv          && COSTING ACCESS.
         *B#802256,1 SSH 12/05/1999  Change every costs picture to become 3 decimal. 
         *@ ROW,054 SAY nItemQuota PICTURE '999999.99'
         @ ROW,053 SAY nItemQuota PICTURE '999999.999'
         *B#802256,1 SSH(END)
       ENDIF
       ROW = ROW+1

       @ ROW,000 SAY cFabWeight PICTURE '!!!!!!!!!!!!!!!'
       @ ROW,021 SAY INTRODUCED
       @ ROW,034 SAY LEADTIME

       IF qCostPrv          && COSTING ACCES.
         *B#802256,1 SSH 12/05/1999  Change every costs picture to become 3 decimal. 
         *@ ROW,054 SAY nItm_Frt PICTURE '999999.99'
         @ ROW,053 SAY nItm_Frt PICTURE '999999.999'
         *B#802256,1 SSH (END)
       ENDIF
       @ ROW,065 SAY CONTENT
       ROW=ROW+1
     ENDIF

     IF XDYELOT_P .AND. &WORKFILE->CDYE_FLG='Y'   && WANT TO  PRINT DYELOTS AND THIS FABRIC COMES IN DYELOTS
       IF !XPRT_DYE
         SELECT FABDYE
         IF SEEK(&WORKFILE->FABRIC+&WORKFILE->COLOR)
           @ ROW,00 SAY REPLICATE('-',40)
           @ ROW,41 SAY '<< DYELOTS >>'
           @ ROW,54 SAY REPLICATE('-',77)
           @ ROW+1,21 SAY 'DYELOT #'
           @ ROW+1,57 SAY 'ONHAND'
           @ ROW+1,67 SAY 'USAGE'
           ROW=ROW+2
         ENDIF
       ENDIF
       DO WHILE !EOF() .AND. (&WORKFILE->FABRIC+&WORKFILE->COLOR = FABRIC+COLOR)
         SELECT FABDYE
         @ ROW,21 SAY DYELOT
         @ ROW,55 SAY ONHAND PICTURE '99999999'
         @ ROW,64 SAY USAGE PICTURE '99999999'
         ROW=ROW+1
         SKIP
         IF ROW >=53
           ROW = 99
           XPRT_DYE=.T.
           EXIT
         ENDIF
       ENDDO
       SELECT (WORKFILE)
       IF ROW >=53
         ROW = 99
         IF !EOF() .AND. (&WORKFILE->FABRIC+&WORKFILE->COLOR = FABRIC+COLOR)
            LOOP
         ENDIF
       ENDIF
     ENDIF
     XTOTAL(1) = XTOTAL(1) + ONHAND

     IF qCostPrv            && COSTING ACCESS.
       
       *B602353,1 Change this line to use the field "nStkVal" instead of
       *          recalculating the On Hand Amount in all cases except
       *          the case of using the Standard Cost Method [Begin]
       *XTOTAL(2) = XTOTAL(2) + (ONHAND* (IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
       xTotal(2) = xTotal(2) + IIF(lcMtCstMth $ 'ALIF' ,;
                                   nStkVal , OnHand * (CostBuy /Conv))
       
       *B602353,1 Change this line to use the field "nStkVal" [End]
       
       XTOTAL(4) = XTOTAL(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
     ENDIF
     XTOTAL(3) = XTOTAL(3) + ONORDER

     GTOTAL(1) = GTOTAL(1) + ONHAND
     
     *B602353,1 Change this line to use the field "nStkVal" instead of
     *          recalculating the On Hand Amount in all cases except
     *          the case of using the Standard Cost Method [Begin]
     *GTOTAL(2) = GTOTAL(2) + (ONHAND*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
     GTotal(2) = GTotal(2) + IIF(lcMtCstMth $ 'ALIF' ,;
                                 nStkVal , OnHand * (CostBuy /Conv))
     
     *B602353,1 Change this line to use the field "nStkVal" [End]
     
     GTOTAL(3) = GTOTAL(3) + ONORDER
     GTOTAL(4) = GTOTAL(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))

     SELECT (WORKFILE)
     SKIP
     XPRT_DYE=.F.
  ENDDO
  *------------------ END MAIN REPORT LOOP --------------------
  X = 2
  IF !EMPTY(BREAK)
     X =1
  ENDIF
  ROW=ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  ROW = ROW+1
  @ ROW,000 SAY ' GRAND TOTAL ->'
  IF XRFORM = 'A'
    @ ROW,093 SAY GTOTAL(1)  PICTURE '99999999'
    @ ROW,114 SAY GTOTAL(3)  PICTURE '99999999'
    ROW = ROW+1
    *--B802245,1 HDM[START]
    *@ ROW,100 SAY GTOTAL(2)  PICTURE '999999999.99'
    @ ROW,102SAY GTOTAL(2)  PICTURE '999999999.99'
    *--B802245,1 HDM[End]
    @ ROW,120 SAY GTOTAL(4)  PICTURE '999999999.99'
  ELSE
    @ ROW,085 SAY GTOTAL(1)  PICTURE '99999999'
    @ ROW,105 SAY GTOTAL(3)  PICTURE '99999999'
    ROW = ROW+1  
    @ ROW,093 SAY GTOTAL(2)  PICTURE '999999999.99'
    @ ROW,112 SAY GTOTAL(4)  PICTURE '999999999.99'  
  ENDIF
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  EXIT
ENDDO

*---------------------------
*   END MAT910.PRG
*---------------------------

******************************************************************************
* PROG: MAT911.PRG
* DESC: FORMAT <B> OF THE REPORT
* DATE: 04/07/93
* AUTH: TIMOUR ABDALLA KHALIL
*!Modifications:
*!      03/27/94 ARH
*!         1) Changed QPW(14) with qCostPrv
*******************************************************************************
PROCEDURE MAT911
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT910                                  MATERIAL MASTER LIST - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*
* ITEM    COLOR  VENDOR   DESC.         PRICE       TAX     QUOTA   FREIGHT     TOTAL   ONHAND     ONHAND  ONORDER    ONORDER   USAGE
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* XXXXXXX XXXXXX XXXXXXXX XXXXXXXXX 99999.999 99999.999 99999.999 99999.999 99999.999 99999999 9999999.99 99999999 9999999.9999999999
*<------------------------------------------------------------------------------------------------------------->
XPRT_DYE=.F.

DO WHILE INKEY() <>32
  IF ROW >=53
    PAGENO = PAGENO+1
    XREPORT='MAT910'
    XTITLE=' '
    R_TITLE='MATERIAL MASTER LIST'
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    @ 05,00 SAY ' ITEM    COLOR  VENDOR   DESC.         PRICE       TAX     QUOTA   FREIGHT     TOTAL   ONHAND      ONHAND ONORDER    ONORDER   USAGE'
    @ 06,00 SAY '                                                                                COST               AMOUNT             AMOUNT'
    @ 07,00 SAY REPLICATE('*',132)
    ROW=08
  ENDIF

  IF XPRT_DYE            && IF IN THE MIDDLE OF PRINTING DYELOTS
    @ ROW,00 SAY REPLICATE('-',40)
    @ ROW,41 SAY '<< DYELOTS >>'
    @ ROW,54 SAY REPLICATE('-',77)
    @ ROW+1,21 SAY 'DYELOT #'
    @ ROW+1,87 SAY 'ONHAND'
    @ ROW+1,127 SAY 'USAGE'
    ROW=ROW+2
  ENDIF

  DO WHILE !EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    ROW=ROW+1
    @ ROW,002 SAY "SUB TOTAL ->"
    @ ROW,015 SAY HBREAK
    @ ROW,085 SAY XTOTAL(1)  PICTURE '99999999'
    
    *--B802245,1 HDM[Start]
    *@ ROW,094 SAY XTOTAL(2)  PICTURE '9999999.99'
    @ ROW,094 SAY XTOTAL(2)  PICTURE '99999999.99'
    *--B802245,1 HDM[End]
    
    @ ROW,105 SAY XTOTAL(3)  PICTURE '99999999'
    @ ROW,114 SAY XTOTAL(4)  PICTURE '9999999.99'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    STORE 0 TO XTOTAL
    HBREAK = &BREAK
    EXIT
  ENDDO
  *--------------------- END SUBTOTALS ----------------------------

  IF !XPRT_DYE
    IF EOF()
       EXIT
    ENDIF

    IF ROW >=53
      ROW = 99
      LOOP
    ENDIF
    SELECT (WORKFILE)
    XCLR=COLOR
    Z = ' '
    CLRDESC   = SUBSTR(gfCodDes(XCLR,'COLOR'),1,12)
    XTYP_DESC = gfCodDes(&WORKFILE->ITEM_tYPE,'ITEM_TYPE')
    SELECT (WORKFILE)
    ROW = ROW+1
    @ ROW,001 SAY FABRIC
    @ ROW,009 SAY COLOR
    @ ROW,016 SAY VENDOR
    @ ROW,025 SAY DESC    PICTURE 'XXXXXXXXX'
    @ ROW,035 SAY nFabCost    PICTURE '99999.999'
    @ ROW,045 SAY nItem_Tax     PICTURE '99999.999'
    @ ROW,055 SAY nItemQuota   PICTURE '99999.999'
    @ ROW,065 SAY nItm_Frt PICTURE '99999.999'
    @ ROW,075 SAY IIF(lcMtCstMth $ 'ALIF' ,nAveCstBuy,CostBuy) PICTURE '99999.999'
    @ ROW,085 SAY ONHAND  PICTURE '99999999'

    *B602353,1 Change this line to use the field "nStkVal" instead of
    *          recalculating the On Hand Amount in all cases except
    *          the case of using the Standard Cost Method [Begin]
    *@ ROW,094 SAY (ONHAND*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
    *          PICTURE '9999999.99'          
    
    *-- B802245,1 HDM[Start]
    *@ ROW,094 SAY IIF(lcMtCstMth $ 'ALIF' ,;
                      nStkVal , OnHand * (CostBuy /Conv)) ;
              PICTURE '9999999.99'          
    
    @ ROW,094 SAY IIF(lcMtCstMth $ 'ALIF' ,;
                      nStkVal , OnHand * (CostBuy /Conv)) ;
              PICTURE '99999999.99'          
    *--B802245,1 HDM[End]
    *B602353,1 Change this line to use the field "nStkVal" [End]
    
    @ ROW,105 SAY ONORDER PICTURE '99999999'
    @ ROW,114 SAY (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
              PICTURE '9999999.99'
    @ ROW,124 SAY USAGE   PICTURE '99999999'
  ENDIF

  IF XDYELOT_P .AND. &WORKFILE->CDYE_FLG='Y'          && WANT TO  PRINT DYELOTS AND THIS FABRIC COMES IN DYELOTS
    IF !XPRT_DYE
      SELECT FABDYE
      IF SEEK(&WORKFILE->FABRIC+&WORKFILE->COLOR)
         ROW=ROW+1
         @ ROW,00 SAY REPLICATE('-',40)
         @ ROW,41 SAY '<< DYELOTS >>'
         @ ROW,54 SAY REPLICATE('-',77)
         ROW=ROW+1
         @ ROW,21 SAY 'DYELOT #'
         @ ROW,87 SAY 'ONHAND'
         @ ROW,127 SAY 'USAGE'
         ROW=ROW+1
      ENDIF
    ENDIF
    DO WHILE !EOF() .AND. (&WORKFILE->FABRIC+&WORKFILE->COLOR = FABRIC+COLOR)
      IF !EMPTY(FabDye.DYELOT)
        @ ROW,21 SAY FabDye.DYELOT
        @ ROW,85 SAY FabDye.ONHAND PICTURE '99999999'
        @ ROW,124 SAY FabDye.USAGE PICTURE '99999999'
        ROW=ROW+1
      ENDIF
      SKIP
      IF ROW >=53
        ROW = 99
        XPRT_DYE=.T.
        EXIT
      ENDIF
    ENDDO
    SELECT (WORKFILE)
    IF ROW >=53
      ROW = 99
      IF !EOF() .AND. (&WORKFILE->FABRIC+&WORKFILE->COLOR = FABRIC+COLOR)
       LOOP
      ENDIF
    ENDIF
  ENDIF
  XPRT_DYE=.F.
  XTOTAL(1) = XTOTAL(1) + ONHAND

  IF qCostPrv
     
     *B602353,1 Change this line to use the field "nStkVal" instead of
     *          recalculating the On Hand Amount in all cases except
     *          the case of using the Standard Cost Method [Begin]
     *XTOTAL(2) = XTOTAL(2) + (ONHAND* (IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
     xTotal(2) = xTotal(2) + IIF(lcMtCstMth $ 'ALIF' ,;
                                 nStkVal , OnHand * (CostBuy /Conv))
     
     *B602353,1 Change this line to use the field "nStkVal" [End]
     
     XTOTAL(4) = XTOTAL(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
  ENDIF
  XTOTAL(3) = XTOTAL(3) + ONORDER

  GTOTAL(1) = GTOTAL(1) + ONHAND
  
  *B602353,1 Change this line to use the field "nStkVal" instead of
  *          recalculating the On Hand Amount in all cases except
  *          the case of using the Standard Cost Method [Begin]
  GTotal(2) = GTotal(2) + IIF(lcMtCstMth $ 'ALIF' ,;
                              nStkVal , OnHand * (CostBuy /Conv))
  
  *B602353,1 Change this line to use the field "nStkVal" [End]
  
  GTOTAL(3) = GTOTAL(3) + ONORDER
  GTOTAL(4) = GTOTAL(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))

  SELECT (WORKFILE)
  IF !EOF()
   SKIP
  ELSE
   EXIT
  ENDIF
  XPRT_DYE=.F.
ENDDO
*-- EOF(MAT911)
*-- HDM B802600,1 [Start] No Need For This Function
*--               Remove Fabric from option grid filter as 
*--               there's another filter [Item range]
*--               that does the same data filtering

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Mohamed Abdel Salam
*! Date      : 05/27/1999
*! Purpose   : Vaildate the fabric code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
*! E500266,1 MAN Added this function
*!*************************************************************
*FUNCTION lfvFabric
*PRIVATE lcObjNam , lcObjVal , lcOldOrd 

*lcObjNam = SYS(18)
*lcObjVal = EVALUATE(SYS(18))

*SELECT FABRIC
*lcOldOrd = ORDER()
*SET ORDER TO TAG FABRIC IN FABRIC

*IF !EMPTY(lcObjVal) .AND. ('?' $ lcObjVal .OR. !SEEK(lcObjVal,'FABRIC'))
*  =FABROW (@lcObjVal ,'*')
*ENDIF 
*&lcObjNam    = lcObjVal 

*SELECT FABRIC
*SET ORDER TO &lcOldOrd

*-- HDM B802600,1 [End]


*!*************************************************************
*! Name      : lfvBins *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Functio for bin
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
FUNCTION lfvBins

= gfMover(@laRpSource,@laRpTarget,'Style Bin',.T.,'')

*!*************************************************************
*! Name      : lfFillBin  *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Fill Bins
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
FUNCTION lfFillBin

llUSdBy =gfOpenFile(gcDataDir+'WHSLOC' ,'WHSLOC','SH')
DIME laRpSource[1,1]
DIME laRpTarget[1,1]
SELECT WHSLOC
SELECT DISTINCT CLOCATION FROM WHSLOC WHERE !EMPTY(CLOCATION)INTO ARRAY laRpSource
USE IN IIF(llUSdBy,'WHSLOC',0)

*!*************************************************************
*! Name      : lfClearRed  *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/17/99
*! Purpose   : clear read
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfClearRed()
*!*************************************************************
FUNCTION lfClearRed
CLEAR READ

*!*************************************************************
*! Name      : lfvpbFabOk  
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/11/20001
*! Purpose   : clear read
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfvpbFabOk()
*!*************************************************************
FUNCTION lfvpbFabOk
*-- this is the validation of to_item
IF !EMPTY(lcFromFab) .OR. !EMPTY(lcToFab)
  IF EMPTY(lcToFab) 
    lcToFab = lcFromFab
  ENDIF
  IF lcFromFab > lcToFab
    WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
   _CUROBJ = OBJNUM(lcFromFab)
  ELSE
    CLEAR READ
  ENDIF
ELSE
  *-- CLEAR READ exits the active READ, 
  *-- and returns control to the previous READ level if the active READ 
  *-- is nested.
  CLEAR READ  
ENDIF
*!*************************************************************
*! Name      : lfvWarehs
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : Valid function of the Order Warehouse
*!*************************************************************
*! Called from : Option grid [Order Warehouse Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*C102161,1 KAM
FUNCTION lfvWarehs
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
*--IF The user want to Browse or if the Warehouse he entered is not in the
*--file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , lcOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*--end function lfvWarehs
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Get fields and some of the Option grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*C102161,1 KAM
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
RETURN

*--end function lfwOldVal
!*************************************************************
*! Name      : lfGetWLine
*! Developer : Abdel-Rahim Koura -KAM
*! Date      : 05/02/20001
*! Purpose   : get the onhand amount for warehouses
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfGetWLine()
*!*************************************************************
*C102161,1 KAM
FUNCTION lfGetWLine
PARAMETERS lcFaricPar,lcColorPar
PRIVATE lcOldArea,lnRecNo,llRetVal,lcExp

IF !EMPTY(lcRpWare)
  lcExp='REST WHILE FABDYE.FABRIC+FABDYE.COLOR= lcFaricPar+lcColorPar ;
        AND FABDYE.cWARECODE=lcRpWare  FOR EMPTY(FABDYE.dyelot) '
ELSE
  lcExp='REST WHILE FABDYE.FABRIC+FABDYE.COLOR= lcFaricPar+lcColorPar ;
          FOR EMPTY(FABDYE.dyelot)' 
ENDIF
lcOldArea=SELECT()
STORE .F. TO llRetVal
lnRecNo  =RECNO()
SELECT (WORKFILE)
lcOrder  =ORDER()
SET ORDER TO TAG "lcIdxFle"
SELECT FABDYE
IF SEEK(lcFaricPar+lcColorPar)
  * SCAN REST WHILE FABDYE.FABRIC+FABDYE.COLOR= lcFaricPar+lcColorPar
  SCAN &lcExp 
    SCATTER MEMVAR MEMO
    lcSrchKey=FABDYE.FABRIC+FABDYE.COLOR+FABDYE.cWARECODE+'C'
    SELECT (WORKFILE)
    IF !SEEK(lcSrchKey) 
       STORE .T. TO llRetVal
      INSERT INTO (WORKFILE) FROM MEMVAR
    ELSE
      REPLACE ONHAND WITH ONHAND +M.ONHAND
    ENDIF  
    *wab
    REPLACE CONV  WITH FABRIC.CONV
    *wab
     REPLACE COLOR WITH FABDYE.COLOR
     REPLACE UseDye   WITH .T. 
     REPLACE ITEMTYPE WITH 'C'
   ENDSCAN   && FABDYE.FABRIC+FABDYE.COLOR= lcFaricPar+lcColorPar
ENDIF &&SEEK(lcFabric)
SET ORDER TO TAG &WORKFILE IN (WORKFILE)
SELECT(lcOldArea)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF   
RETURN llRetVal