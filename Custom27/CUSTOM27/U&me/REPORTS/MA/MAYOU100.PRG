************************************************************************
*: Program file   : MAYOU100.PRG (C# 102176)
*: Program desc.  : Material Master List For U&ME.
*: System         : Aria Apparel System
*: Developer      : Mohamed Shokry (MHM)
*: Module         : MATERIAL (MA)
*:************************************************************************
*: Calls :PROCEDURES :
*:        FUNCTIONS  : 
************************************************************************
*: Passed Parameters  : None
************************************************************************
*--Declarition Section [Start]
R_WIDTH  = 'W'                         && STANDARD REPORT IS 'WIDE'
R_TITLE  = 'MATERIAL MASTER LIST '
XDYELOT_P = llRPPrnDye
llApIstall = (OCCURS('AP',gcCmpModules)<>0)
lcMtCstMth = gfGetMemVar('M_MATCSTMT') 
* check to user for access to costing  or not
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')     

lcVendor   = lcRPVendor                && Vendor Code
lcLowItem  = lcRPFrmFab                && Lower range of the item code
lcHigItem  = lcRPToFab                 && Higher range of the item code
lcSortBy   = lcRPSortBy                && Sort By option.Item,Vendor,Location
lcItemType = lcRPItmTyp                && Item Type
lcOnHndQty = IIF(llRPOnhand,'Y','N')   && Print Qty on hand.[Y/N]

*-- lcFilter is used to filterize the Fabric file.
lcFilter  = '.T.'                     
lcFilter  = IIF(!EMPTY(lcVendor),lcFilter+' AND Vendor = lcVendor',lcFilter)
lcFilter  = IIF(!EMPTY(lcItemType),lcFilter+' AND Item_Type=lcItemType',lcFilter)
lcFilter  = IIF(!EMPTY(lcLowItem),lcFilter+' AND Fabric >= lcLowItem',lcFilter)  
lcHigItem = IIF(!EMPTY(lcHigItem),lcHigItem,lcLowItem)
lcFilter  = IIF(!EMPTY(lcHigItem),lcFilter+' AND Fabric <= lcHigItem',lcFilter)  
lcFilter = IIF(lcOnHndQty='Y',lcFilter + ' AND OnHand <> 0',lcFilter)
*--Declarition Section [END]

*-- system instead of our vendor file if the system was install AP module or not
IF llApIstall
  SELECT ApVendor
  lcVenFile = 'ApVendor'
ELSE
  SELECT Vendor
  lcVenFile = 'Vendor'
ENDIF

*-- [3] SELECT REPORT FILE & INITIALIZE MEMORY

SELECT FABRIC
WAIT WINDOW  'Selecting records for report ...' NOWAIT
LOCATE FOR &lcFilter
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM361212B00000','DIALOG' )
  RETURN
ENDIF

WORKFILE = gfTempName()
COPY REST TO &gcWorkDir.&WORKFILE FOR &lcFilter

SELECT 0
=gfOpenFile(gcWorkDir+WORKFILE,'','EX')

* SORT TO WORKFILE INDEX
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'SORTING &Z RECORDS FOR MATERIAL MASTER LIST ...' NOWAIT

DO CASE
  CASE lcSortBy ='I'
    INDEX ON FABRIC+COLOR TAG &WORKFILE
  CASE lcSortBy ='V'
    INDEX ON VENDOR+FABRIC+COLOR TAG &WORKFILE
  CASE lcSortBy ='L'
    INDEX ON LOC+FABRIC+COLOR TAG &WORKFILE
ENDCASE

SET ORDER TO TAG &WORKFILE

SET RELATION TO VENDOR INTO &lcVenFile

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*YOU100                                  MATERIAL MASTER LIST - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*
*
*ITEM                COLOR        VENDOR    UOM BUY       COST  COST BUY  COST USE  REORDER   ONHAND     ONHAND  ONORDER    ONORDER
*....DESCRIPTION.... DESCRIPTION  LOCATION  UOM USE        TAX                          USAGE              AMOUNT              AMOUNT
*ITEM TYPE                         PATTERN  CONV.        QUOTA
*WEIGHT              INTRODUCED   LEADTIME             FREIGHT  ..........................CONTENTS..........................
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*XXXXXXX             XXXXXX       XXXXXXXX  XXX     999999.99 999999.99 999999.99 99999999 9999999.99 9999999.99 9999999.99 9999999.99
*XXXXXXXXXXXXXXXXXXX XXXXXXXXXXXX XXXXX     XXX     999999.99                       99999999
*XXXXXX XXXXXXXXXXXX              XXXXXXXX  9999.99 999999.99
*XXXXXXXXXXXXXXX     MM/DD/YY     XX                999999.99  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*<-------------------------------------<< DYELOTS >>--------------------------------------------------------------------------------->
*                     DYELOT #                            ONHAND    USAGE
*                     XXXXXXXXXX                        99999999 99999999

DIMENSION XTOTAL(4),GTOTAL(4)
XTOTAL = 0.00
GTOTAL = 0.00
PAGENO = 0
ROW    = 99
XTIME  = TIME()
   

DO CASE
  CASE lcSortBy ='I'
    BREAK       = 'FABRIC'
  CASE lcSortBy ='V'
    BREAK       = 'VENDOR'
  CASE lcSortBy ='L'
    BREAK       = 'LOC'
ENDCASE

   
SELECT (WORKFILE)
LOCATE
HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
WAIT WINDOW "Report printing - <SPACE BAR> to abort" NOWAIT

XPRT_DYE=.F.         && flag used when printing dyelot info. to not to repeat
                     && the fabric/clr info when switching to new page ...
SELECT (WORKFILE)

*-- [REPORT] LOOP
IF lcRPForm ='B'
  DO MAT911               && PROCEDURE (TO EXECUTE FORMAT B)
ENDIF

DO WHILE (INKEY() <>32 .AND. lcRPForm ='A')
  IF ROW >=53
    PAGENO = PAGENO+1
    *XREPORT='YOU100'
    XTITLE=' '
    R_TITLE='MATERIAL MASTER LIST'
    DO RPT_HDR WITH 'MAYOU100',XTITLE,R_WIDTH
    IF lcMtCstMth $ 'AL'
      @ 05,00 SAY '                                                       STD.    AVE.      AVE.'
    ELSE
      @ 05,00 SAY '                                                       STD.    STD.      STD.'
    ENDIF
    @ 06,00 SAY 'ITEM                COLOR        VENDOR    UOM BUY    PRICE  COST BUY  COST USE  REORDER     ONHAND     ONHAND    ONORDER    ONORDER  '

    @ 07,00 SAY '....DESCRIPTION.... DESCRIPTION  LOCATION  UOM USE      TAX                        USAGE                AMOUNT                AMOUNT'
    @ 08,00 SAY 'ITEM   TYPE         WIDTH        PATTERN     CONV.    QUOTA'
    @ 09,00 SAY 'WEIGHT              INTRODUCED   LEADTIME           FREIGHT  ..........................CONTENTS.......................... '

    ROW=10
    IF XPRT_DYE            && IF IN THE MIDDLE OF PRINTING DYELOTS
      @ 10,00 SAY REPLICATE('-',40)
      @ 10,41 SAY '<< DYELOTS >>'
      @ 10,54 SAY REPLICATE('-',77)
      @ 11,20 SAY 'DYELOT #'
      @ 11,54 SAY 'ONHAND'
      @ 11,63 SAY 'USAGE'
      ROW=12
    ENDIF  
  ENDIF

  DO WHILE LEN(TRIM(BREAK)) <>0
    SELECT (WORKFILE)
    lcHbreak = HBREAK
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    ROW = ROW+1
    @ ROW,000 SAY '* SUB TOTAL * '
    @ ROW,020 SAY HBREAK
    @ ROW,089 SAY XTOTAL(1)  PICTURE '9999999.99'
    @ ROW,100 SAY XTOTAL(2)  PICTURE '9999999.99'
    @ ROW,111 SAY XTOTAL(3)  PICTURE '9999999.99'
    @ ROW,122 SAY XTOTAL(4)  PICTURE '9999999.99'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    X = 1
    DO WHILE X<=4
      GTOTAL(X) = GTOTAL(X) + XTOTAL(X)
      X = X+1
    ENDDO
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
    *-- Get Codes Descreprion
    CLRDESC   = gfCodDes( XCLR , 'COLOR     ')
    XTYP_DESC = gfCodDes( &WORKFILE->ITEM_tYPE , 'MFGCODE   ')
    SELECT (WORKFILE)
    IF HBREAK = lcHbreak
      @ ROW,000 SAY REPLICATE('-',132)
    ENDIF  
    ROW = ROW+1
    @ ROW,000 SAY FABRIC
    @ ROW,020 SAY COLOR
    @ ROW,033 SAY VENDOR
    @ ROW,042 SAY UOMBUY

    IF llCostPrv        && COSTING ACCESS.
      @ ROW,050 SAY NFABCOST PICTURE '99999.999'           
      @ ROW,060 SAY IIF(lcMtCstMth $ 'AL' ,nAveCstBuy,CostBuy) PICTURE '99999.999'        && MFM 03/05/95.
  
      *-- Use average cost instead of cost use if system costing method
      *-- is average.
      @ ROW,070 SAY (IIF(lcMtCstMth $ 'AL' ,nAveCstBuy,CostBuy)/Conv) PICTURE '99999.999'  && MFM 03/05/95.
    ENDIF
    @ ROW,080 SAY REORDER PICTURE '99999999'
    @ ROW,089 SAY ONHAND PICTURE '9999999.99'
  
    IF llCostPrv          && COSTING ACCES.
      *-- Use average cost instead of cost use if system costing method
      *-- is average.
      @ ROW,100 SAY (ONHAND*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv)) ;
                PICTURE '9999999.99'
    ENDIF
    @ ROW,111 SAY ONORDER PICTURE '9999999.99'
    IF llCostPrv          && COSTING ACCESS.

      *-- Use average cost instead of cost use if system costing method
      *-- is average.
      @ ROW,122 SAY (ONORDER*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv)) ;
                 PICTURE '9999999.99'
    ENDIF
    ROW = ROW+1
    * LINE #2
    @ ROW,000 SAY LEFT(DESC,19)
    @ ROW,020 SAY LEFT(CLRDESC,12)
    @ ROW,033 SAY LOC
    @ ROW,042 SAY UOMUSE
 
    IF llCostPrv          && COSTING ACCESS.
      @ ROW,050 SAY NITEM_TAX PICTURE '999999.99'
      @ ROW,080 SAY USAGE PICTURE '99999999'
    ENDIF
    ROW = ROW+1
    *LINE #3
    @ ROW,000 SAY ITEM_tYPE
    @ ROW,006 SAY LEFT(XTYP_DESC,12)
    @ ROW,020 SAY WIDTH
    @ ROW,033 SAY LEFT(PATTERN,8)
    @ ROW,042 SAY CONV PICTURE '9999.99'

    IF llCostPrv          && COSTING ACCESS.
      @ ROW,050 SAY NITEMQUOTA PICTURE '999999.99'
    ENDIF
    ROW = ROW+1
    * LINE #4
    @ ROW,000 SAY CFABWEIGHT PICTURE '!!!!!!!!!!!!!!!'
    @ ROW,020 SAY INTRODUCED
    @ ROW,033 SAY LEADTIME

    IF llCostPrv          && COSTING ACCES.
      @ ROW,050 SAY NITM_FRT PICTURE '999999.99'
    ENDIF
    @ ROW,061 SAY CONTENT
    ROW=ROW+1
  ENDIF

  IF XDYELOT_P .AND. &WORKFILE->CDYE_FLG = 'Y'   && WANT TO  PRINT DYELOTS AND THIS FABRIC COMES IN DYELOTS
    IF !XPRT_DYE
      SELECT FABDYE
      IF SEEK(&WORKFILE->FABRIC+&WORKFILE->COLOR)
        @ ROW,00 SAY REPLICATE('-',40)
        @ ROW,41 SAY '<< DYELOTS >>'
        @ ROW,50 SAY REPLICATE('-',77)
        @ ROW+1,20 SAY 'DYELOT #'
        @ ROW+1,54 SAY 'ONHAND'
        @ ROW+1,63 SAY 'USAGE'
        ROW=ROW+2
      ENDIF
    ENDIF
    DO WHILE !EOF() .AND. (&WORKFILE->FABRIC+&WORKFILE->COLOR = FABRIC+COLOR)
      SELECT FABDYE
      @ ROW,20 SAY DYELOT
      @ ROW,50 SAY ONHAND PICTURE '9999999.99'
      @ ROW,60 SAY USAGE PICTURE '99999999'
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

  IF llCostPrv            && COSTING ACCESS.
    *-- Use average cost instead of cost use if system costing method
    *-- is average.
    XTOTAL(2) = XTOTAL(2) + (ONHAND*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv))
    XTOTAL(4) = XTOTAL(4) + (ONORDER*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv))
    *-- END MFM 03/05/95.
  ENDIF
  XTOTAL(3) = XTOTAL(3) + ONORDER
  SELECT (WORKFILE)
  SKIP
  XPRT_DYE=.F.
ENDDO
*------------------ END MAIN REPORT LOOP --------------------
X = 2
IF LEN(TRIM(BREAK)) =0
  X =1
ENDIF
ROW=ROW+1
@ ROW,00 SAY REPLICATE('*',132)
ROW = ROW+1
@ ROW,000 SAY ' GRAND TOTAL ->'
IF lcRPForm = 'A'
  @ ROW,089 SAY GTOTAL(1)  PICTURE '9999999.99'
  @ ROW,100 SAY GTOTAL(2)  PICTURE '9999999.99'
  @ ROW,111 SAY GTOTAL(3)  PICTURE '9999999.99'
  @ ROW,122 SAY GTOTAL(4)  PICTURE '9999999.99'
ELSE
  @ ROW,085 SAY GTOTAL(1)  PICTURE '99999999'
  @ ROW,094 SAY GTOTAL(2)  PICTURE '9999999.99'
  @ ROW,105 SAY GTOTAL(3)  PICTURE '99999999'
  @ ROW,114 SAY GTOTAL(4)  PICTURE '9999999.99'
ENDIF
ROW = ROW+1
@ ROW,00 SAY REPLICATE('*',132)

DO ENDREPORT      && End the report or display on screen.
SET DEVICE TO SCREEN
RETURN

*---------------------------
*   END MAYOU100.PRG
*---------------------------

*!*************************************************************
*! Name      : MAT911
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2001
*! Purpose   : FORMAT <B> OF THE REPORT
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
*! Example   : do MAT911
*!*************************************************************
PROCEDURE MAT911
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*YOU100                                  MATERIAL MASTER LIST - 123456789012345678901234567890                                 PAGE: 1234
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
    *XREPORT='YOU100'
    XTITLE=' '
    R_TITLE='MATERIAL MASTER LIST'
    DO RPT_HDR WITH 'MAYOU100',XTITLE,R_WIDTH
    @ 05,00 SAY ' ITEM    COLOR  VENDOR   DESC.         PRICE       TAX     QUOTA   FREIGHT     TOTAL   ONHAND     ONHAND  ONORDER    ONORDER   USAGE'
    @ 06,00 SAY '                                                                                COST              AMOUNT              AMOUNT'
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

  DO WHILE LEN(TRIM(BREAK)) <>0
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    ROW=ROW+1
    @ ROW,002 SAY "SUB TOTAL ->"
    @ ROW,015 SAY HBREAK
    @ ROW,085 SAY XTOTAL(1)  PICTURE '99999999'
    @ ROW,094 SAY XTOTAL(2)  PICTURE '9999999.99'
    @ ROW,105 SAY XTOTAL(3)  PICTURE '99999999'
    @ ROW,114 SAY XTOTAL(4)  PICTURE '9999999.99'
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    X = 1
    DO WHILE X<=4
       GTOTAL(X) = GTOTAL(X) + XTOTAL(X)
       X = X+1
    ENDDO
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
    *-- Get Codes Descreprion
    CLRDESC   = gfCodDes( XCLR , 'COLOR     ')
    XTYP_DESC = gfCodDes( &WORKFILE->ITEM_tYPE , 'MFGCODE   ')
    SELECT (WORKFILE)
    ROW = ROW+1
    @ ROW,001 SAY FABRIC
    @ ROW,009 SAY COLOR
    @ ROW,016 SAY VENDOR
    @ ROW,025 SAY DESC        PICTURE 'XXXXXXXXX'  
    @ ROW,035 SAY NFABCOST    PICTURE '99999.999'
    @ ROW,045 SAY NITEM_TAX         PICTURE '99999.999'
    @ ROW,055 SAY NITEMQUOTA       PICTURE '99999.999'
    @ ROW,065 SAY NITM_FRT    PICTURE '99999.999'
    @ ROW,075 SAY IIF(lcMtCstMth $ 'AL' ,nAveCstBuy,CostBuy) PICTURE '99999.999'
    @ ROW,085 SAY ONHAND  PICTURE '99999999'

    *-- Use average cost instead of cost use if system costing method
    *-- is average.
    @ ROW,094 SAY (ONHAND*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv)) ;
              PICTURE '9999999.99'
    @ ROW,105 SAY ONORDER PICTURE '99999999'
    @ ROW,114 SAY (ONORDER*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv)) ;
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
      @ ROW,21 SAY DYELOT
      @ ROW,85 SAY ONHAND PICTURE '99999999'
      @ ROW,124 SAY USAGE PICTURE '99999999'
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
  XPRT_DYE=.F.
  XTOTAL(1) = XTOTAL(1) + ONHAND

  IF llCostPrv
     *-- Use average cost instead of cost use if system costing method
     *-- is average.
     XTOTAL(2) = XTOTAL(2) + (ONHAND*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv))
     XTOTAL(4) = XTOTAL(4) + (ONORDER*(IIF(lcMtCstMth $ 'AL',nAveCstBuy,CostBuy)/Conv))
  ENDIF
  XTOTAL(3) = XTOTAL(3) + ONORDER
  SELECT (WORKFILE)
  IF !EOF()
   SKIP
  ELSE
   EXIT
  ENDIF
  XPRT_DYE=.F.
ENDDO
*-- EOF(MAT911)
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2001
*! Purpose   : To validate the vendor code.
*!*************************************************************
*! Called from : Option Grid
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2001
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
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2001
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
*! Name      : lfvpbFabOk  
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2001
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

