*:***************************************************************************
*: Program file  : MAROB470
*: Program desc. : Customized Material Master List FOR Robyn
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Material (MA)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Robyn Merdith
*:***************************************************************************
*C101337,1 KHM 11/18/98
*:***************************************************************************
*Modifications
*B604528,1 TMI 06/14/2001  rename 'lfvpbOk' to 'lfvpbFabOk' for FabRng Screen
*:***************************************************************************

*-- llDyelot is the variable that shows whether the system uses dylots or not.
XDYELOT_S = llDyelot

*-- llRpPrndye is the varibale that shows if the user chooses to print 
*-- the dyelot or not.
XDYELOT_P = llRPPrnDye

*-- To get the system's costing method.
lcMtCstMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET'))) 

*-- To hold the vendor file.
lcVenFile = 'ApVendor'

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'
R_TITLE  = 'MATERIAL MASTER LIST '

*-- llCostAccs variable that showes if the user has costing access or not
llCostAccs = gfUserPriv('IC','ICSTYLE')
qCostPrv   = llCostAccs

*-- Assigning the necessary varibales that have been choosed in the OG.
lcVendor   = lcRPVendor                && Vendor Code
lcLowItem  = lcRPFrmFab                && Lower range of the item code
lcHigItem  = lcRPToFab                 && Higher range of the item code
XTITLE     = ''
lcSortBy   = lcRPSortBy                && Sort By option.Item,Vendor,Location
lcItemType = lcRPItmTyp                && Item Type
lcForm     = lcRPForm                  && Report format [A/B]
lcOnHndQty = IIF(llRPOnhand,'Y','N')   && Print Qty on hand.[Y/N]
lcMatPrice = IIF(llRPMaPric,'Y','N')   && Print the material price [Y/N]
lcPrintLoc = IIF(llRPMaPric,'Y','N')   && Print the location [Y/N]
lcLowLoc   = lcRpFrmLoc                && Lower range of the Bins
lcHigLoc   = lcRpToLoc                 && Higher range of the Bins

*-- lcFilter is used to filterize the Fabric file.
lcFilter  = '.T.'                     
lcFilter  = IIF(!EMPTY(lcVendor),lcFilter+' AND Vendor = lcVendor',lcFilter)
lcFilter  = IIF(!EMPTY(lcItemType),lcFilter+' AND Item_Type=lcItemType',lcFilter)
lcFilter  = IIF(!EMPTY(lcLowItem),lcFilter+' AND Fabric >= lcLowItem',lcFilter)  
lcHigItem = IIF(!EMPTY(lcHigItem),lcHigItem,lcLowItem)
lcFilter  = IIF(!EMPTY(lcHigItem),lcFilter+' AND Fabric <= lcHigItem',lcFilter)  
lcFilter = IIF(lcOnHndQty='Y',lcFilter + ' AND OnHand <> 0',lcFilter)

*-- lcLocFltr to filterize the Bins
lcLocFltr  = '.T.'
lcLocFltr  = IIF(!EMPTY(lcLowLoc),lcLocFltr + ' AND cLocation >= lcLowLoc',;
                 lcLocFltr)
lcLocFltr  = IIF(!EMPTY(lcHigLoc),lcLocFltr + ' AND cLocation <= lcHigLoc',;
                 lcLocFltr) 

llPrint    = .T.

*-- To hold the WhsLoc file after being filtered.
lcTempLoc  = gfTempName()
lcTempLoc1 = gfTempName()

*-- Print the report
=lfPrnRep()


*!*************************************************************
*! Name      : lfPrnRep
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : To print form A.
*!*************************************************************
FUNCTION lfPrnRep

lnSelect = IIF(llRPPrnDye,1,2)

DO WHILE .T.
  SELECT Fabric
  SET FILTER TO
  GOTO TOP

  IF !EMPTY(lcFilter)
    LOCATE ALL FOR &lcFilter
  ENDIF  
  IF EOF()    
    =gfModalGen('TRM36121B00000','DIALOG' )
    SET DEVICE TO SCREEN
    RETURN
  ENDIF

  WORKFILE = gfTempName()
  COPY REST TO &gcWorkDir.&WORKFILE FOR &lcFilter

  *-- If printing the dyelot
  IF XDYELOT_S 
    IF lnSelect = 1
      = gfOpenFile (gcDataDir+'FabDye','FabDye','SH')
    ENDIF
  ENDIF  

  = gfOpenFile (gcWorkDir+WORKFILE,' ','EX')

  * SORT TO WORKFILE INDEX
   Z = LTRIM(STR(RECCOUNT(),7))

  IF lcSortBy='V'
    INDEX ON Vendor+Fabric+Color TAG &WORKFILE     
  ELSE
    INDEX ON Fabric+Color TAG &WORKFILE
  ENDIF
  SET ORDER TO TAG &WORKFILE

  SET RELATION TO VENDOR INTO &lcVenFile

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
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

  DIMENSION lnTotal(4),lnGrdTotal(4)
  lnTotal = 0
  lnGrdTotal = 0
  PAGENO = 0
  ROW    = 99

  DO CASE
    CASE lcSortBy = 'I'
      BREAK  = 'FABRIC'
    CASE lcSortBy = 'V'
      BREAK  = 'VENDOR'
    CASE lcSortBy = 'L'
      BREAK  = 'cLocation'
      SELECT WhsLoc
      COPY STRUCTURE TO &gcWorkDir.&lcTempLoc
      SET FILTER TO &lcLocFltr
      SET ORDER TO WhsLocSt
      = gfOpenFile (gcWorkDir+lcTempLoc,' ','EX')
      INDEX ON cWareCode+cLocation+Style+Color TAG (lcTempLoc1)
      INDEX ON Style+Color+cWareCode+cLocation TAG (lcTempLoc)  ADDITIVE
      SET ORDER TO (lcTempLoc)
      SELECT (WORKFILE)
      SCAN
        IF SEEK(PADR(Fabric,19,' ')+Color,'WhsLoc')
          SELECT WhsLoc        
          SCAN REST WHILE Style+Color+cWareCode+cLocation=;
                          PADR(&WorkFile..Fabric,19,' ')+&WorkFile..Color
            SCATTER MEMVAR MEMO
            SELECT (lcTempLoc)
            APPEND BLANK
            GATHER MEMVAR MEMO  
          ENDSCAN
        ENDIF  
      ENDSCAN
      SELECT (lcTempLoc)      
      GOTO TOP
      IF EOF()
        =gfModalGen('TRM36121B00000','DIALOG' )
        SET DEVICE TO SCREEN
        RETURN
      ENDIF
      SELECT (lcTempLoc)
      SET ORDER TO (lcTempLoc1)
  ENDCASE    

  IF lcSortBy <> 'L'  
    SELECT (WORKFILE)
  ELSE
    SELECT (lcTempLoc)
  ENDIF
  
  GOTO TOP
  HBREAK=SPACE(1)
  IF !EMPTY(BREAK)
     HBREAK = &BREAK
  ENDIF
  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT
  llPrnDye=.F.         && flag used when printing dyelot info. to do not repeat
                       && the fabric/clr info when switching to new page ...
   IF lcSortBy <> 'L'
      SELECT (WORKFILE)
    ELSE
      SELECT (lcTempLoc)
    ENDIF  

  *-- [REPORT] LOOP
  IF lcForm = 'B'
    =lfFormB()
  ENDIF

  DO WHILE INKEY() <>32 .AND. lcForm = 'A'
    WAIT WINDOW "Fabric/Color :" + IIF(lcSortBy <> 'L',;
                 ALLTRIM(Fabric)+"/"+Color,ALLTRIM(Style)+"/"+Color) NOWAIT
    =IIF(Row >= 53,lfPrnHdr(),.F.)
    DO WHILE !EMPTY(BREAK)
      IF lcSortBy <> 'L'
        SELECT (WORKFILE)
      ELSE
        SELECT (lcTempLoc)
      ENDIF  
      IF &BREAK = HBREAK
        EXIT
      ENDIF
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW+1
      @ ROW,000 SAY '* SUB TOTAL * '
      @ ROW,020 SAY HBREAK      
      IF lcMatPrice = 'Y' 
        @ ROW,093 SAY lnTotal(1)  PICTURE '99999999'
      ENDIF

      IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
        @ ROW,102 SAY lnTotal(2)  PICTURE '9999999.99'
      ENDIF

      IF lcSortBy <> 'L'
        @ ROW,113 SAY lnTotal(3)  PICTURE '99999999'
      ENDIF      

      IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
        @ ROW,122 SAY lnTotal(4)  PICTURE '9999999.99'
      ENDIF
       
      ROW = ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      STORE 0 TO lnTotal
      HBREAK = &BREAK
      EXIT
    ENDDO
    IF EOF()
      EXIT
    ENDIF  
    *--------------------- END SUBTOTALS ----------------------------
    IF lcSortBy = 'L'
      IF SEEK(LEFT(Style,7)+Color,WorkFile)
        llPrint = .T.
      ELSE  
        llPrint = .F.
      ENDIF  
    ENDIF

    IF llPrint
      IF !llPrnDye
        IF EOF()
          EXIT
        ENDIF
        IF ROW >=53
          ROW = 99
          LOOP
        ENDIF
        SELECT (WORKFILE)
        lcColor = COLOR
        Z = ' '
        *-- To get the color and color description from the codes file.
        SELECT Codes    
        lcClrDesc = gfCodDes(lcColor , 'COLOR')
        lcClrDesc   = SUBSTR(lcClrDesc,1,12)       
        lcTypeDesc = gfCodDes(&WORKFILE->Item_Type, 'ITEM_TYPE')
        SELECT (WORKFILE)
        @ ROW,000 SAY REPLICATE('-',132)
        ROW = ROW+1
        @ ROW,000 SAY FABRIC
        @ ROW,021 SAY COLOR
        @ ROW,034 SAY VENDOR
        @ ROW,045 SAY UOMBUY
        IF qCostPrv AND lcMatPrice = 'Y'
          @ ROW,054 SAY nFabCOST PICTURE '99999.999'
          @ ROW,064 SAY IIF(lcMtCstMth $ 'ALIF' ,nAveCstBuy,CostBuy) PICTURE '99999.999'
          @ ROW,074 SAY (IIF(lcMtCstMth $ 'ALIF' ,nAveCstBuy,CostBuy)/Conv) PICTURE '99999.999'
        ENDIF
        @ ROW,084 SAY REORDER PICTURE '99999999'
        @ ROW,093 SAY ONHAND PICTURE '99999999'       

        IF qCostPrv  AND lcMatPrice = 'Y'
          @ ROW,102 SAY (ONHAND*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
                    PICTURE '9999999.99'
        ENDIF
        @ ROW,113 SAY ONORDER PICTURE '99999999' 
        IF qCostPrv  AND lcMatPrice = 'Y'
          @ ROW,122 SAY (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
                    PICTURE '9999999.99'
        ENDIF

        ROW = ROW+1

        * LINE #2
        @ ROW,000 SAY DESC
        @ ROW,021 SAY lcClrDesc
        @ ROW,034 SAY LOC
        @ ROW,045 SAY UOMUSE
        IF qCostPrv  AND lcMatPrice = 'Y'
          @ ROW,054 SAY nItem_TAX PICTURE '999999.99'
        ENDIF
        IF qCostPrv 
          @ ROW,084 SAY USAGE PICTURE '99999999'
        ENDIF
        ROW = ROW+1

        *LINE #3
        @ ROW,000 SAY RIGHT(ITEM_tYPE,2)
        @ ROW,003 SAY LEFT(ALLTRIM(lcTypeDesc),15)
        @ ROW,021 SAY WIDTH
        @ ROW,034 SAY PATTERN
        @ ROW,045 SAY CONV PICTURE '9999.99'

        IF qCostPrv AND lcMatPrice = 'Y'
          @ ROW,054 SAY nItemQUOTA PICTURE '999999.99'
        ENDIF

        ROW = ROW+1

        * LINE #4
        @ ROW,000 SAY cFabWEIGHT PICTURE '!!!!!!!!!!!!!!!'
        @ ROW,021 SAY INTRODUCED
        @ ROW,034 SAY LEADTIME

        IF qCostPrv AND lcMatPrice = 'Y'
          @ ROW,054 SAY nItm_FRT PICTURE '999999.99'
        ENDIF

        @ ROW,065 SAY CONTENT
        ROW=ROW+1
      ENDIF
      IF XDYELOT_S 
        IF lnSelect = 1 .AND. &WORKFILE->cDye_Flg='Y'   && WANT TO  PRINT DYELOTS AND THIS FABRIC COMES IN DYELOTS
          IF !llPrnDye
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
              llPrnDye=.T.
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
      ENDIF  

      IF lcSortBy $ 'IV' 
        lcFabric = PADR(Fabric,19,' ')
        lcClr    = Color
        SELECT WhsLoc
        SET ORDER TO TAG WhSLocST
        IF lcPrintLoc = 'Y' AND SEEK(lcFabric+lcClr,'WhSLoc')
          SELECT WhsLoc
          =IIF(Row >= 53,lfPrnHdr(),.F.)
          Row = Row + 1 
          lnCol = 1       
          SCAN REST WHILE Style+Color+CwareCode+cLocation = lcFabric+lcClr
            IF lnCol + LEN(ALLTRIM(cLocation)) <= 130
              @ Row,lnCol SAY ALLTRIM(cLocation)
              ROW=ROW+1
              lnCol = lnCol + LEN(ALLTRIM(cLocation)) + 1
            ELSE
              =IIF(Row >= 53,lfPrnHdr(),.F.)
              Row = Row + 1
              lnCol = 1
              @ Row,lnCol SAY ALLTRIM(cLocation)
              lnCol = lnCol + LEN(ALLTRIM(cLocation)) + 1  
            ENDIF  
          ENDSCAN       
        ENDIF
      ENDIF  
      SELECT (WorkFile)
      IF lcMatPrice = 'Y' 
        lnTotal(1) = lnTotal(1) + ONHAND
      ENDIF  

      IF qCostPrv AND lcMatPrice = 'Y' AND lcSortBy <> 'L'
        lnTotal(2) = lnTotal(2) + (ONHAND* (IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
        lnTotal(4) = lnTotal(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
      ENDIF
      IF lcMatPrice = 'Y'  AND lcSortBy <> 'L'
        lnTotal(3) = lnTotal(3) + ONORDER
      ENDIF  
      IF lcMatPrice = 'Y' 
        lnGrdTotal(1) = lnGrdTotal(1) + ONHAND
      ENDIF
      IF lcMatPrice = 'Y'  AND lcSortBy <> 'L'   
        lnGrdTotal(2) = lnGrdTotal(2) + (ONHAND*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
      ENDIF
      IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
        lnGrdTotal(3) = lnGrdTotal(3) + ONORDER 
      ENDIF        

      IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
        lnGrdTotal(4) = lnGrdTotal(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
      ENDIF
    ENDIF
    IF lcSortBy = 'L'
      SELECT (lcTempLoc)
    ELSE       
      SELECT (WORKFILE)
    ENDIF       
    IF !EOF()
      SKIP
    ENDIF
    llPrnDye=.F.    
  ENDDO
  WAIT CLEAR
  *------------------ END MAIN REPORT LOOP --------------------
  X = 2
  IF !EMPTY(BREAK)
     X =1
  ENDIF
  ROW=ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  ROW = ROW+1
  @ ROW,000 SAY ' GRAND TOTAL ->'
  IF lcForm = 'A'
    IF lcMatPrice = 'Y' 
      @ ROW,093 SAY lnGrdTotal(1)  PICTURE '99999999'
    ENDIF  
    IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
      @ ROW,113 SAY lnGrdTotal(3)  PICTURE '99999999'    
    ENDIF
    IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
      ROW = ROW+1
      @ ROW,100 SAY lnGrdTotal(2)  PICTURE '999999999.99'
      @ ROW,120 SAY lnGrdTotal(4)  PICTURE '999999999.99'
    ENDIF
  ELSE
    IF lcMatPrice = 'Y' 
      @ ROW,085 SAY lnGrdTotal(1)  PICTURE '99999999'
    ENDIF  
    IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
      @ ROW,105 SAY lnGrdTotal(3)  PICTURE '99999999'
    ENDIF
    IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
      ROW = ROW+1  
      @ ROW,092 SAY lnGrdTotal(2)  PICTURE '999999999.99'
      @ ROW,112 SAY lnGrdTotal(4)  PICTURE '999999999.99'  
    ENDIF
  ENDIF
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  EXIT
ENDDO
DO ENDREPORT
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfPrnRep
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : To print the report header.
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO+1

R_TITLE='CUSTOMIZED MATERIAL MASTER LIST'
DO RPT_HDR WITH 'MAROB4700',XTITLE,R_WIDTH
IF lcMtCstMth $ 'ALIF'
  @ 05,00 SAY '                                                           STD.    AVE.      AVE.'
ELSE
  @ 05,00 SAY '                                                           STD.    STD.      STD.'
ENDIF
@ 06,00 SAY 'ITEM                 COLOR        VENDOR     UOM BUY      PRICE  COST BUY  COST USE  REORDER   ONHAND     ONHAND  ONORDER    ONORDER  '
@ 07,00 SAY '....DESCRIPTION....  DESCRIPTION  LOCATION   UOM USE        TAX                        USAGE              AMOUNT              AMOUNT'
@ 08,00 SAY 'ITEM TYPE            WIDTH        PATTERN      CONV.      QUOTA'
@ 09,00 SAY 'WEIGHT               INTRODUCED   LEADTIME              FREIGHT  ..........................CONTENTS.......................... '
ROW=10
IF llPrnDye            && IF IN THE MIDDLE OF PRINTING DYELOTS
  @ 10,00 SAY REPLICATE('-',40)
  @ 10,41 SAY '<< DYELOTS >>'
  @ 10,54 SAY REPLICATE('-',77)
  @ 11,21 SAY 'DYELOT #'
  @ 11,57 SAY 'ONHAND'
  @ 11,67 SAY 'USAGE'
  ROW=12
ENDIF


*!*************************************************************
*! Name      : lfPrnRep
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : To print Form B.
*!*************************************************************
FUNCTION lfFormB
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT910                                  MATERIAL MASTER LIST - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*
* ITEM    COLOR  VENDOR   DESC.         PRICE       TAX     QUOTA   FREIGHT     TOTAL   ONHAND     ONHAND  ONORDER    ONORDER   USAGE
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* XXXXXXX XXXXXX XXXXXXXX XXXXXXXXX 99999.999 99999.999 99999.999 99999.999 99999.999 99999999 9999999.99 99999999 9999999.9999999999
*<------------------------------------------------------------------------------------------------------------->
llPrnDye=.F.
DO WHILE INKEY() <>32
  WAIT WINDOW "Fabric/Color :" + IIF(lcSortBy <> 'L',;
               ALLTRIM(Fabric)+"/"+Color,ALLTRIM(Style)+"/"+Color) NOWAIT
  IF ROW >=53
    PAGENO = PAGENO+1    
    R_TITLE='CUSTOMIZED MATERIAL MASTER LIST'
    DO RPT_HDR WITH 'MAROB4700',XTITLE,R_WIDTH
    @ 05,00 SAY ' ITEM    COLOR  VENDOR   DESC.         PRICE       TAX     QUOTA   FREIGHT     TOTAL   ONHAND     ONHAND  ONORDER    ONORDER   USAGE'
    @ 06,00 SAY '                                                                                COST              AMOUNT              AMOUNT'
    @ 07,00 SAY REPLICATE('*',132)
    ROW=08
  ENDIF

  IF llPrnDye            && IF IN THE MIDDLE OF PRINTING DYELOTS
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
    @ ROW,085 SAY lnTotal(1)  PICTURE '99999999'
    
    IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
      @ ROW,094 SAY lnTotal(2)  PICTURE '9999999.99'
    ENDIF
    IF lcSortBy <> 'L'
      @ ROW,105 SAY lnTotal(3)  PICTURE '99999999'
    ENDIF 
    IF lcMatPrice = 'Y' AND lcSortBy <> 'L'
      @ ROW,114 SAY lnTotal(4)  PICTURE '9999999.99'
    ENDIF
      
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',132)
    STORE 0 TO lnTotal
    HBREAK = &BREAK
    EXIT
  ENDDO
  *--------------------- END SUBTOTALS ----------------------------
  IF lcSortBy = 'L'
    IF SEEK(LEFT(Style,7)+Color,WorkFile)
      llPrint = .T.
    ELSE  
      llPrint = .F.
    ENDIF  
  ENDIF
  IF llPrint
    IF !llPrnDye
      IF EOF()
         EXIT
      ENDIF
      IF ROW >=53
        ROW = 99
        LOOP
      ENDIF
      SELECT (WORKFILE)
      lcColor=COLOR
      Z = ' '
      *-- To get the color and color description from the codes file.
      SELECT Codes    
      lcClrDesc  = gfCodDes(lcColor , 'COLOR')
      lcClrDesc  = SUBSTR(lcClrDesc,1,12)       
      lcTypeDesc = gfCodDes(&WORKFILE->Item_Type, 'ITEM_TYPE')

      SELECT (WORKFILE)
      ROW = ROW+1
      @ ROW,001 SAY FABRIC
      @ ROW,009 SAY COLOR
      @ ROW,016 SAY VENDOR
      @ ROW,025 SAY DESC    PICTURE 'XXXXXXXXX'

      IF lcMatPrice = 'Y'
        @ ROW,035 SAY nFabCOST    PICTURE '99999.999'
        @ ROW,045 SAY nItem_TAX     PICTURE '99999.999'
        @ ROW,055 SAY nItemQUOTA   PICTURE '99999.999'
        @ ROW,065 SAY nItm_FRT PICTURE '99999.999'
        @ ROW,075 SAY IIF(lcMtCstMth $ 'ALIF' ,nAveCstBuy,CostBuy) PICTURE '99999.999'
      ENDIF
    
      @ ROW,085 SAY ONHAND  PICTURE '99999999'

      IF lcMatPrice = 'Y'
        @ ROW,094 SAY (ONHAND*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
                      PICTURE '9999999.99'          
      ENDIF
    
      @ ROW,105 SAY ONORDER PICTURE '99999999'

     IF lcMatPrice = 'Y'
        @ ROW,114 SAY (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv)) ;
                      PICTURE '9999999.99'
      ENDIF
      @ ROW,124 SAY USAGE   PICTURE '99999999'
    ENDIF
    IF XDYELOT_S 
      IF lnSelect = 1 .AND. &WORKFILE->cDye_Flg='Y'          && WANT TO  PRINT DYELOTS AND THIS FABRIC COMES IN DYELOTS
        IF !llPrnDye
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
          IF !EMPTY(DYELOT)
            @ ROW,21 SAY DYELOT
            @ ROW,85 SAY ONHAND PICTURE '99999999'
            @ ROW,124 SAY USAGE PICTURE '99999999'
            ROW=ROW+1
          ENDIF
          SKIP
          IF ROW >=53
            ROW = 99
            llPrnDye=.T.
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
    ENDIF  
    IF lcSortBy $ 'IV' 
      lcFabric = PADR(Fabric,19,' ')
      lcClr    = Color
      SELECT WhsLoc
      SET ORDER TO TAG WhSLocST
      IF lcPrintLoc = 'Y' AND SEEK(lcFabric+lcClr,'WhSLoc')
        SELECT WhsLoc
        =IIF(Row >= 53,lfPrnHdr(),.F.)
        Row = Row + 1 
        lnCol = 1       
        SCAN REST WHILE Style+Color+CwareCode+cLocation = lcFabric+lcClr
          IF lnCol + LEN(ALLTRIM(cLocation)) <= 130
            @ Row,lnCol SAY ALLTRIM(cLocation)
            lnCol = lnCol + LEN(ALLTRIM(cLocation)) + 1
          ELSE
            =IIF(Row >= 53,lfPrnHdr(),.F.)
            Row = Row + 1
            lnCol = 1
            @ Row,lnCol SAY ALLTRIM(cLocation)
            lnCol = lnCol + LEN(ALLTRIM(cLocation)) + 1  
          ENDIF  
        ENDSCAN
        Row = Row + 1
      ENDIF
    ENDIF  
    SELECT (WorkFile)

    llPrnDye=.F.
    lnTotal(1) = lnTotal(1) + ONHAND

    IF qCostPrv AND lcMatPrice = 'Y'
       lnTotal(2) = lnTotal(2) + (ONHAND* (IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
       lnTotal(4) = lnTotal(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
    ENDIF
  
    lnTotal(3) = lnTotal(3) + ONORDER

    lnGrdTotal(1) = lnGrdTotal(1) + ONHAND

    IF lcMatPrice = 'Y'
      lnGrdTotal(2) = lnGrdTotal(2) + (ONHAND*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
    ENDIF 
  
    lnGrdTotal(3) = lnGrdTotal(3) + ONORDER

    IF lcMatPrice = 'Y'
     lnGrdTotal(4) = lnGrdTotal(4) + (ONORDER*(IIF(lcMtCstMth $ 'ALIF',nAveCstBuy,CostBuy)/Conv))
    ENDIF
    
  ENDIF  
  IF lcSortBy = 'L'
    SELECT (lcTempLoc)
  ELSE       
    SELECT (WORKFILE)
  ENDIF
  IF !EOF()
    SKIP
  ELSE
    EXIT
  ENDIF
  llPrnDye=.F.
ENDDO
WAIT CLEAR
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- Going to the top of the warehous file in order to get the first 
*-- warehous code.
GOTO TOP IN WareHous

IF !USED(lcLocFile)
  lcLocFile = gfTempName()
  SELECT WhsLoc.cLocation ;
    FROM WhsLoc,WareHous ;
    WHERE WhsLoc.cWareCode+WhsLoc.cLocation+WhsLoc.Style+WhsLoc.Color = WareHous.cWareCode;
    INTO CURSOR(lcLocFile);
    GROUP BY WhsLoc.cLocation;
    ORDER BY WhsLoc.cLocation
ENDIF

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Khalid Mohi El-Din Mohamed KHM
*! Date      : 11/22/1998
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
*! Developer : Khalid Mohi El-Din Mohamed KHM
*! Date      : 11/22/1998
*! Purpose   : Showes range screen for fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabBet()
*!*************************************************************
FUNCTION lfvFabBet

*-- Here do the between screen
lcTitle = 'Item'

STORE lcRPFrmFab TO lcFromFab
STORE lcRPToFab TO lcToFab

DO (gcRepHome + gcAct_Appl + '\FabRng.SPR')

lcRPFrnFab = lcFromFab
lcRpToFab  = lcToFab
 
*!*************************************************************
*! Name      : lfvFabRng
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : To validate the fabric code.
*!*************************************************************
*! Called from : Option Grid
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
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : Vailadet range screen's OK button 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************
*B604528,1 TMI [Start] Rename 'lfvpbOk' to 'lfvpbFabOk' 
*FUNCTION lfvpbOk
FUNCTION lfvpbFabOk
*B604528,1 TMI [End  ] Rename 'lfvpbOk' to 'lfvpbFabOk' 

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
*! Name      : lfvLocRang
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : To validate the locations range.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvLocRang()
*!*************************************************************
FUNCTION lfvLocRang
PRIVATE lcObjNam , lcObjVal , llObjRet

*-- Setting the order to WhsLoc in the Bins file (WhsLoc)
SET ORDER TO WhsLoc IN WhsLoc

lcObjNam = SYS(18)
lcObjVal = EVALUATE(SYS(18))


IF lcObjNam = "LCOGVALUEF" 
  IF !EMPTY(lcObjVal) AND !SEEK(WareHous.cWareCode+lcObjVal,'WhsLoc')
    lcRpFrmLoc = lfBrowBins()
    lcObjVal   = lcRpFrmLoc
  ENDIF
ELSE
  IF !EMPTY(lcObjVal) AND !SEEK(WareHous.cWareCode+lcObjVal,'WhsLoc')
    lcRpToLoc = lfBrowBins()
    lcObjVal  = lcRpToLoc
  ENDIF 
ENDIF
&lcObjNam = lcObjVal


*!*************************************************************
*! Name      : lfBrowBins
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : To browse the bins of the selected warehous
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfBrowBins()
*!*************************************************************
FUNCTION lfBrowBins
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

SELECT (lcLocFile)
DIMENSION laTemp[1]
laTemp = ''      && Array to hold the Selected value

lcBrFields = "cLocation:R :H='Bins'"
  
lcFile_Ttl = "Bins"
lcBrowCond = ""
        
= gfBrows(lcBrowCond,'cLocation','laTemp')
    
IF !EMPTY(laTemp[1])
  RETURN(laTemp[1])
ELSE
  RETURN(SPACE(10))
ENDIF

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : To validate the sort by option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy

CLEAR READ