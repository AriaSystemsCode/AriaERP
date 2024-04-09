*:************************************************************************
*: Program file  : MAMATST.PRG
*: Program desc. : Fabric Master List For Style
*: For Report    : (@x,y SAY)
*: System        : ARIA APPAREL SYSTEM 2.7
*: Module        : MATERIALS (MA).
*: Developer     : IHB
*:************************************************************************
*: Calls         : 
*:    Procedures : None
*:    Functions  : 
*:                 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO MAMATST
*:************************************************************************
*: This Report Program is due to E301099,1
*:************************************************************************
*: Modifications:
*:          B802245,1 HDM 05/23/1999 Adjust onhand amount picture
*:************************************************************************
*-- Option Grid Layout :
*-- Styles In The Range                 : .. to ..
*-- Primary Fabric Code                 : ..
*-- Only These Seasons                  : one /more /all /none
*-- Only These Divisions                : one /more /all /none
*-- Only This Style Group               : one /more /all /none
*-- Pattern                             : ..
*-- Only These Colors                   : one /more /all /none
*-- Vendor                              : ..
*-- Item Type                           : one /all
*-- Item Range                          : .. to ..
*-- Sort By (I)tem, (V)endor            : I
*-- Report Form (A)/(B)                 : A
*-- Print Only Items With Onhand Qty.   : N
*-- Print Dyelot Details (Y/N)          : N 
*:************************************************************************
*B802256,1 SSH 12/05/1999  Change every costs picture to become 3 decimal. 
*B802990,1 HDM 01/30/2000  Correct the spelling of INTREDUCED to INTRODUCED
*B803586,1 ADEL 08/27/2000 Adjust 'Width' and 'Pattern' fields.
*B604528,1 TMI 06/14/2001  rename 'lfvpbOk' to 'lfvpbFabOk' for FabRng Screen
*B604627,1 HBG 17/07/2001 Fix bug of conversion field prints **** 
*B605264,1 RAE 01/10/2002 Display all styles if it domestic or not
*B038431,1 NNA 09/09/2004 Fix bug that if you didn't define item code structure you'll get error
*B038431,1 NNA            [Variable 'laSeg' Not Found ] when you run this report 
*:************************************************************************
R_TITLE  = 'MATERIAL MASTER LIST FOR STYLE'
*-- XDYELOT_S : identify whether the system uses dyelots or not
*-- XAVG_COST : identify costing method
*-- LSTYLE    : low style
*-- HSTYLE    : high style
*-- XFABRIC   : primary fabric
*-- XSTATUS   : added filter to the style file ('A')
XDYELOT_S = llDyelot

*-- lcMtCstMth variable that hold the system costing method
lcMtCstMth = ALLTRIM(UPPER(gfGetMemVar('M_COST_MET'))) 
XAVG_COST = IIF(lcMtCstMth = 'A', .T., .F.)

*-- llCostPrv print cost privilage (per user)
qCostPrv = gfUserPriv('IC','ICSTYLE','COSTING')

LSTYLE   = SUBSTR(laOGFxFlt[1,6],1,((LEN(laOGFxFlt[1,6])-1)/2))
HSTYLE   = SUBSTR(laOGFxFlt[1,6],((LEN(laOGFxFlt[1,6])-1)/2)+2,((LEN(laOGFxFlt[1,6])-1)/2))
XFABRIC  = laOGFxFlt[2,6]
XSTATUS  = 'A'
lcImport = 'N'

DO STY910      && set the filter for the style file

*-- after returning to the caller 
*-- the current alias is STYLE and it is filtered on the option grid
*-- filter (lcRpExp) modified with the hidden filter (STATUS='A' AND MAKE=.T.) 
IF EOF() .OR. (LEN(TRIM(HSTYLE)) > 0 .AND. STYLE > HSTYLE)
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

*-- XVENDOR   : vendor
*-- LFAB      : low fabric
*-- HFAB      : high fabric
*-- XSORT     : sort by (Item or Vendor) 
*-- XITEM_TYP : item type
*-- XTYP_DESC : item type description
*-- XRFORM    : print form (A or B)
*-- XONHAND_Q : print only items with onhand quantity (N/Y) 
XVENDOR   = lcRpVendor
LFAB      = lcRpFrmFab
HFAB      = lcRpToFab
XSORT     = lcRpSortBy
XITEM_TYP = lcRpItmTyp
XTYP_DESC = gfCodDes(XITEM_tYP,'ITEM_TYPE')     && space(15)
XRFORM    = lcRpForm
XONHAND_Q = lcRpOnHand

SELECT FABRIC
SET RELATION TO
SET FILTER   TO
GOTO TOP

lcRpExp  = ' '
IF !EMPTY(XVENDOR)
  lcRpExp  = lcRpExp + 'VENDOR = XVENDOR'
ENDIF

IF !EMPTY(XITEM_TYP)
  IF !EMPTY(lcRpExp)
    lcRpExp  = lcRpExp + ' .AND. ITEM_TYPE = XITEM_TYP'
  ELSE  
    lcRpExp  = lcRpExp + 'ITEM_TYPE = XITEM_TYP'
  ENDIF
ENDIF  

IF !EMPTY(LFAB)
  IF !EMPTY(lcRpExp)
    lcRpExp  = lcRpExp + ' .AND. FABRIC >= LFAB .AND. FABRIC <= HFAB'
  ELSE
    lcRpExp  = lcRpExp + ' FABRIC >= LFAB .AND. FABRIC <= HFAB'
  ENDIF  
ELSE
  IF !EMPTY(HFAB)
    IF !EMPTY(lcRpExp)
      lcRpExp  = lcRpExp + ' .AND. FABRIC <= HFAB'
    ELSE
      lcRpExp  = lcRpExp + ' FABRIC <= HFAB'
    ENDIF  
  ENDIF
ENDIF  

IF lcRpOnHand = .T.     && yes = .t. no = .f.
  IF !EMPTY(lcRpExp)
    lcRpExp  = lcRpExp + ' .AND. ONHAND <> 0'
  ELSE
    lcRpExp  = lcRpExp + ' ONHAND <> 0'
  ENDIF  
ENDIF

*-- note that the suppress expression in syrepuvr is used for 
*-- enabling/disabling the dyelot object.
*-- In dyelot system ask if the user wants to print the dyelot details.
*-- XDYELOT_P : stores if the system uses dyelots and the user wants to
*--             print dyelot details .T. else it is .F.
XDYELOT_P = llRpPrnDye       && yes = .t.  no = .f.

lcTempFile = gfTempName()    &&global function gives temporarily name for the workfile.
SELECT FABRIC
COPY STRUCTURE TO (gcWorkDir+lcTempFile)
=gfOpenFile(gcWorkDir+lcTempFile,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
INDEX ON FABRIC+COLOR TAG (lcTempFile)

WORKFILE = gfTempName()    &&global function gives temporarily name for the workfile.

*-- data collection.
*-- Added the fabric emptyness check to prevent displaing
*-- empty records for every empty fabric.
SELECT STYLE
SCAN
  XFABRIC = FABRIC
  IF !EMPTY(XFABRIC)
    SELECT (lcTempFile)
    IF !SEEK(XFABRIC)
      SELECT FABRIC
      IF SEEK(XFABRIC)
        IF !EMPTY(lcRpExp)
          COPY TO (gcWorkDir+WORKFILE) WHILE XFABRIC = FABRIC FOR &lcRpExp
        ELSE
          COPY TO (gcWorkDir+WORKFILE) WHILE XFABRIC = FABRIC
        ENDIF
        SELECT (lcTempFile)
        APPEND FROM (gcWorkDir+WORKFILE)
      ENDIF
    ENDIF
  ENDIF
  SELECT STYLE
ENDSCAN
*-- select report file and initialize memory
*-- major print loop
DO WHILE .T.
  SELECT (lcTempFile)
  GO TOP
  IF EOF()
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF
  SELECT FABRIC
  SET RELATION TO
  
  SELECT (lcTempFile)
 
  IF XSORT = 'V'
    *B803586,1 (Begin) Fix the bug of 'Invalid function arguemnt...'
    *INDEX ON VENDOR+FABRIC+COLOR TO TAG (lcTempFile)
    INDEX ON VENDOR+FABRIC+COLOR TAG (lcTempFile)
    *B803586,1 (End)
  ENDIF
  SET RELATION TO VENDOR INTO APVENDOR

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT960                                  MATERIAL MASTER LIST FOR STYLE- 123456789012345678901234567890                     PAGE: 1234
*MM/DD/YY
*
*
*ITEM                 COLOR        VENDOR     UOM BUY       COST  COST BUY  COST USE  REORDER   ONHAND     ONHAND  ONORDER    ONORDER
*....DESCRIPTION....  DESCRIPTION. LOCATION   UOM USE        TAX                          USAGE              AMOUNT              AMOUNT
*ITEM TYPE                         PATTERN    CONV.        QUOTA
*                                  LEADTIME              FREIGHT  ..........................CONTENTS..........................          &&MODI WAM09/16/93
*                     INTRODUCED   LEADTIME              FREIGHT  ..........................CONTENTS..........................          &&MODI MFM 12/28/93
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*XXXXXXX              XXXXXX       XXXXXXXX   XXX      999999.99 999999.99 999999.99 99999999 99999999 9999999.99 99999999 9999999.99
*XXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXX XXXXX      XXX      999999.99                     99999999
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

   SELECT (lcTempFile)
   GOTO TOP
   HBREAK = SPACE(1)
   IF !EMPTY(BREAK)
     HBREAK = &BREAK
   ENDIF
   SET DEVICE TO PRINT
   XPRT_DYE=.F.         && flag used when printing dyelot info. to not to repeat
                        && the fabric/clr info when switching to new page .

   SELECT (lcTempFile)
   *-- report major print loop 
   IF XRFORM='B'
     DO MAT911               && procedure to execute format B
   ENDIF
   
   *-- print loop for format A
   DO WHILE (INKEY() <>32 .AND. XRFORM='A')
      IF ROW >= 53
         PAGENO = PAGENO+1
         XREPORT='MAMATST'
         XTITLE=' '
         R_TITLE='MATERIAL MASTER LIST FOR STYLE'
         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
         @ 06,00 SAY 'ITEM                 COLOR        VENDOR     UOM BUY       COST  COST BUY  COST USE  REORDER   ONHAND       ONHAND ONORDER   ONORDER'
         @ 07,00 SAY '....DESCRIPTION....  DESCRIPTION  LOCATION   UOM USE        TAX                        USAGE                AMOUNT            AMOUNT'
         @ 08,00 SAY 'ITEM TYPE            WIDTH        PATTERN      CONV.      QUOTA'
         *B802990,1 [Start] Correct the spelling of INTREDUCED to INTRODUCED
         *@ 09,00 SAY 'WEIGHT               INTREDUCED   LEADTIME              FREIGHT  ..........................CONTENTS..........................'
         @ 09,00 SAY 'WEIGHT               INTRODUCED   LEADTIME              FREIGHT  ..........................CONTENTS..........................'
         *B802990,1 [eND]
         ROW=10
         
         *-- if in the middle of printing dyelots
         IF XPRT_DYE
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
         IF &BREAK = HBREAK
            EXIT
         ENDIF
         ROW=ROW+1
         @ ROW,00 SAY REPLICATE('=',132)
         ROW = ROW+1
         @ ROW,000 SAY '* SUB TOTAL * '
         @ ROW,020 SAY HBREAK
         @ ROW,093 SAY XTOTAL(1)  PICTURE '99999999'
         @ ROW,102 SAY XTOTAL(2)  PICTURE '999999999.99'
         @ ROW,113 SAY XTOTAL(3)  PICTURE '99999999'
         @ ROW,120 SAY XTOTAL(4)  PICTURE '9999999.99'
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
      *-- end subtotals
      IF .NOT. XPRT_DYE
        IF EOF()
           EXIT
        ENDIF
        IF ROW >=53
           ROW = 99
           LOOP
        ENDIF

        SELECT (lcTempFile)
        XCLR=COLOR
        Z = ' '
        CLRDESC   = SUBSTR(gfCodDes(COLOR,'COLOR'),1,12)
        XTYP_DESC = gfCodDes(ITEM_tYPE,'ITEM_tYPE')
         
        SELECT (lcTempFile)
        @ ROW,000 SAY REPLICATE('-',132)
        ROW = ROW+1
        @ ROW,000 SAY FABRIC
        @ ROW,021 SAY COLOR
        @ ROW,034 SAY VENDOR
        @ ROW,045 SAY UOMBUY
        
        *-- costing access
        IF qCostPrv
          *B#802256,1 SSH 12/05/1999  Change picture to become 3 decimal. 
          *@ ROW,054 SAY NFABCOST PICTURE '999999.99'
          *--HDM 802245 [Start]
          *@ ROW,054 SAY NFABCOST PICTURE '999999.999'
          @ ROW,053 SAY NFABCOST PICTURE '999999.999'
          *--HDM 802245 [End]
          
          *B#802256,1 SSH(END).
          @ ROW,063 SAY COSTBUY PICTURE '999999.999'
          *-- Use average cost instead of cost use if system costing method
          *-- is average.
          *B#802256,1 SSH 12/05/1999  Change picture to become 3 decimal. 
          *@ ROW,074 SAY IIF(XAVG_COST,NFAVE_COST,COSTUSE) PICTURE '999999.99'
          @ ROW,073 SAY IIF(XAVG_COST,NFAVE_COST,COSTUSE) PICTURE '999999.999'
          *B#802256,1 SSH(END).
        ENDIF
        @ ROW,084 SAY REORDER PICTURE '99999999'
        @ ROW,093 SAY ONHAND PICTURE '99999999'
        
        *-- costing access
        IF qCostPrv
          *-- Use average cost instead of cost use if system costing method
          *-- is average.
          *--HDM B802245,1 Adjust onhand amount picture[START]
          *@ ROW,102 SAY (ONHAND*IIF(XAVG_COST,NFAVE_COST,COSTUSE)) ;
                    PICTURE '9999999.99'

          @ ROW,102 SAY (ONHAND*IIF(XAVG_COST,NFAVE_COST,COSTUSE)) ;
                    PICTURE '999999999.99'

          *--HDM B802245,1 [END]
        ENDIF
        @ ROW,113 SAY ONORDER PICTURE '99999999'
        IF qCostPrv          && COSTING ACCESS.
          *-- Use average cost instead of cost use if system costing method
          *-- is average.
          @ ROW,122 SAY (ONORDER*IIF(XAVG_COST,NFAVE_COST,COSTUSE)) ;
                    PICTURE '9999999.99'
        ENDIF
        ROW = ROW+1
        
        *-- line #2
        @ ROW,000 SAY DESC
        @ ROW,021 SAY CLRDESC
        @ ROW,034 SAY LOC
        @ ROW,045 SAY UOMUSE
        
        *-- costing access
        IF qCostPrv
          *B#802256,1 SSH 12/05/1999  Change picture to become 3 decimal. 
          *@ ROW,054 SAY NITEM_TAX PICTURE '999999.99'
          *-- HDM [START]
          *@ ROW,054 SAY NITEM_TAX PICTURE '999999.999'
          @ ROW,053 SAY NITEM_TAX PICTURE '999999.999'
          *-- HDM [END]
          *B#802256,1 SSH(END)
          @ ROW,084 SAY USAGE PICTURE '99999999'
        ENDIF
        ROW = ROW+1
        *-- line #3
        *B803586,1 (Begin) Reduce XTYP_DESC  as it is in Material Master List.
        *@ ROW,000 SAY ITEM_tYPE
        *@ ROW,003 SAY XTYP_DESC
        @ ROW,000 SAY ALLTRIM(ITEM_TYPE)+' '+SUBSTR(XTYP_DESC,1,13)
        *B803586,1 (End)
        @ ROW,021 SAY WIDTH
        @ ROW,034 SAY PATTERN
        *B#802256,1 SSH 12/05/1999  Change picture to become 3 decimal. 
        *@ ROW,045 SAY CONV PICTURE '9999.99'
        *B604627,1 HBG 17/07/2001 Fix bug of conversion field prints **** [Begin]
        *@ ROW,045 SAY CONV PICTURE '9999.999'
        @ ROW,043 SAY CONV PICTURE '999999.99'
        *B604627,1 [End]
        *B#802256,1 SSH(END)
        *-- costing access 
        IF qCostPrv
          *B#802256,1 SSH 12/05/1999  Change picture to become 3 decimal. 
          *@ ROW,054 SAY NITEMQUOTA PICTURE '999999.99'
          *B803586,1 (Begin) Begin less one space.
         *@ ROW,054 SAY NITEMQUOTA PICTURE '999999.999'
          @ ROW,053 SAY NITEMQUOTA PICTURE '999999.999'
          *B803586,1 (End)
          *B#802256,1 SSH(END)
        ENDIF
        ROW = ROW+1

        *-- line #4 
        @ ROW,000 SAY CFABWEIGHT PICTURE '!!!!!!!!!!!!!!!'
        @ ROW,021 SAY INTRODUCED
        @ ROW,034 SAY LEADTIME
        *-- costing acccess
        IF qCostPrv
          *B#802256,1 SSH 12/05/1999  Change picture to become 3 decimal. 
          *@ ROW,054 SAY NITM_FRT PICTURE '999999.99'
          @ ROW,053 SAY NITM_FRT PICTURE '999999.999'
          *B#802256,1 SSH(END)
        ENDIF
        @ ROW,065 SAY CONTENT
        ROW=ROW+1
      ENDIF
      *-- want to print dyelots and this fabric comes in dyelots
      IF XDYELOT_P .AND. &lcTempFile..CDYE_FLG='Y'
        IF .NOT. XPRT_DYE
          SELECT FABDYE
          IF SEEK(&lcTempFile..FABRIC+&lcTempFile..COLOR)
            @ ROW,00 SAY REPLICATE('-',40)
            @ ROW,41 SAY '<< DYELOTS >>'
            @ ROW,54 SAY REPLICATE('-',77)
            @ ROW+1,21 SAY 'DYELOT #'
            @ ROW+1,57 SAY 'ONHAND'
            @ ROW+1,67 SAY 'USAGE'
            ROW=ROW+2
          ENDIF
        ENDIF
        DO WHILE (.NOT. EOF()) .AND. (&lcTempFile..FABRIC+&lcTempFile..COLOR = FABRIC+COLOR)
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
        IF ROW >=53
           ROW = 99
           IF (.NOT. EOF()) .AND. (&lcTempFile..FABRIC+&lcTempFile..COLOR = FABRIC+COLOR)
              SELECT &lcTempFile
              LOOP
           ENDIF
        ENDIF
        SELE &lcTempFile
      ENDIF
      XTOTAL(1) = XTOTAL(1) + ONHAND
      *-- costing access
      IF qCostPrv
        *-- Use average cost instead of cost use if system costing method
        *-- is average.
        XTOTAL(2) = XTOTAL(2) + (ONHAND*IIF(XAVG_COST,NFAVE_COST,COSTUSE))
        XTOTAL(4) = XTOTAL(4) + (ONORDER*IIF(XAVG_COST,NFAVE_COST,COSTUSE))
      ENDIF
      XTOTAL(3) = XTOTAL(3) + ONORDER
      SELECT &lcTempFile
      SKIP
      XPRT_DYE=.F.
   ENDDO
   *-- end main report loop
   X = 2
   IF LEN(TRIM(BREAK)) =0
      X =1
   ENDIF
   ROW=ROW+1
   @ ROW,00 SAY REPLICATE('*',132)
   ROW = ROW+1
   @ ROW,000 SAY ' GRAND TOTAL ->'
   IF XRFORM = 'A'
     @ ROW,093 SAY GTOTAL(1)  PICTURE '99999999'
     @ ROW,102 SAY GTOTAL(2)  PICTURE '999999999.99'
     @ ROW,113 SAY GTOTAL(3)  PICTURE '99999999'
     @ ROW,121 SAY GTOTAL(4)  PICTURE '9999999.99'
   ELSE
     @ ROW,084 SAY GTOTAL(1)  PICTURE '99999999'
     @ ROW,093 SAY GTOTAL(2)  PICTURE '999999999.99'
     @ ROW,104 SAY GTOTAL(3)  PICTURE '99999999'
     @ ROW,113 SAY GTOTAL(4)  PICTURE '9999999.99'
   ENDIF
   ROW = ROW+1
   @ ROW,00 SAY REPLICATE('*',132)
   EXIT
ENDDO
DO ENDREPORT                   && end report 
SET DEVICE TO SCREEN           && return set device to screen 
RETURN                         && return to the caller which is gfopgrid (option grid program)
*-- end of report code

*:************************************************************************
*! PROGRAM     : MAT911.PRG
*! DESCRIPTION : FORMAT <B> OF THE REPORT
*! DATE        : 04/07/93
*! AUTHER      : TIMOUR ABDALLA KHALIL
*:************************************************************************
PROCEDURE MAT911
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MAT910                                  MATERIAL MASTER LIST - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.
*ITEM    COLOR  VENDOR   DESC.         PRICE       TAX     QUOTA    FREIGT     TOTAL   ONHAND     ONHAND  ONORDER    ONORDER    USAGE
*                                                                               COST              AMOUNT              AMOUNT
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.
*XXXXXXX XXXXXX XXXXXXXX XXXXXXXXX 99999.999 99999.999 99999.999 99999.999 99999.999 99999999 9999999.99 99999999 9999999.99 99999999
*<------------------------------------------------------------------------------------------------------------->
XPRT_DYE = .F.
DO WHILE INKEY() <>32
  IF ROW >=53
    PAGENO = PAGENO+1
    XREPORT='MAMATST'
    XTITLE=' '
    R_TITLE='MATERIAL MASTER LIST FOR STYLE'
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    @ 05,00 SAY 'ITEM    COLOR  VENDOR   DESC.         PRICE       TAX     QUOTA    FREIGT     TOTAL   ONHAND      ONHAND ONORDER    ONORDER    USAGE'
    @ 06,00 SAY '                                                                               COST               AMOUNT             AMOUNT'
    @ 07,00 SAY REPLICATE('*',132)
    ROW = 08
  ENDIF

  IF XPRT_DYE            && if in the middle of printing dyelots
    @ ROW,00 SAY REPLICATE('-',40)
    @ ROW,41 SAY '<< DYELOTS >>'
    @ ROW,54 SAY REPLICATE('-',77)
    @ ROW+1,21 SAY 'DYELOT #'
    @ ROW+1,86 SAY 'ONHAND'
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
    @ ROW,084 SAY XTOTAL(1)  PICTURE '99999999'
    @ ROW,093 SAY XTOTAL(2)  PICTURE '999999999.99'
    @ ROW,104 SAY XTOTAL(3)  PICTURE '99999999'
    @ ROW,113 SAY XTOTAL(4)  PICTURE '9999999.99'
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
  *-- end subtotals

  IF !XPRT_DYE
    IF EOF()
       EXIT
    ENDIF

    IF ROW >=53
      ROW = 99
      LOOP
    ENDIF
    SELECT (lcTempFile)
    CLRDESC   = SUBSTR(gfCodDes(COLOR,'COLOR'),1,12)
    XTYP_DESC = gfCodDes(ITEM_TYPE,'ITEM_TYPE')

    SELECT (lcTempFile)
    ROW = ROW + 1
    @ ROW,000 SAY FABRIC
    @ ROW,008 SAY COLOR
    @ ROW,015 SAY VENDOR
    @ ROW,024 SAY DESC        PICTURE 'XXXXXXXXX'
    IF qCostPrv
      @ ROW,034 SAY NFABCOST    PICTURE '99999.999'
      @ ROW,044 SAY NITEM_TAX   PICTURE '99999.999'
      @ ROW,054 SAY NITEMQUOTA  PICTURE '99999.999'
      @ ROW,064 SAY NITM_FRT    PICTURE '99999.999'
      @ ROW,074 SAY COSTBUY     PICTURE '99999.999'
    ENDIF
    @ ROW,084 SAY ONHAND      PICTURE '99999999'
    IF qCostPrv
      *-- Use average cost instead of cost use if system
      *-- costing method is average.
      
      *-- HDM B802245,1 [Start]
      *@ ROW,093 SAY (ONHAND*IIF(XAVG_COST,NFAVE_COST,COSTUSE)) PICTURE '9999999.99'
      @ ROW,093 SAY (ONHAND*IIF(XAVG_COST,NFAVE_COST,COSTUSE)) PICTURE '999999999.99'
      *-- HDM B802245,1 [End]
      
      @ ROW,104 SAY ONORDER PICTURE '99999999'
      @ ROW,113 SAY (ONORDER*IIF(XAVG_COST,NFAVE_COST,COSTUSE)) PICTURE '9999999.99'
      @ ROW,124 SAY USAGE   PICTURE '99999999'
    ENDIF
  ENDIF
  
  *-- if want to print dyelots and this fabric comes in dyelots
  IF XDYELOT_P .AND. &lcTempFile..CDYE_FLG = 'Y' 
    IF !XPRT_DYE
      SELECT FABDYE
      IF SEEK(&lcTempFile..FABRIC+&lcTempFile..COLOR)
         ROW=ROW+1
         @ ROW,00 SAY REPLICATE('-',40)
         @ ROW,41 SAY '<< DYELOTS >>'
         @ ROW,54 SAY REPLICATE('-',78)
         ROW = ROW + 1
         @ ROW,21 SAY 'DYELOT #'
         @ ROW,86 SAY 'ONHAND'
         @ ROW,127 SAY 'USAGE'
         ROW = ROW + 1
      ENDIF
    ENDIF
    DO WHILE (.NOT. EOF()) .AND. (&lcTempFile..FABRIC+&lcTempFile..COLOR = FABRIC+COLOR)
      @ ROW,21  SAY DYELOT
      @ ROW,084 SAY ONHAND PICTURE '99999999'
      @ ROW,124 SAY USAGE  PICTURE '99999999'
      ROW=ROW+1
      SKIP
      IF ROW >=53
        ROW = 99
        XPRT_DYE=.T.
        EXIT
      ENDIF
    ENDDO
    IF ROW >=53
      ROW = 99
      IF (.NOT. EOF()) .AND. (&lcTempFile..FABRIC+&lcTempFile..COLOR = FABRIC+COLOR)
       SELECT (lcTempFile)
       LOOP
      ENDIF
    ENDIF
    SELECT (lcTempFile)
  ENDIF
  XPRT_DYE=.F.
  XTOTAL(1) = XTOTAL(1) + ONHAND

  IF qCostPrv
     *-- Use average cost instead of cost use if system costing method
     *-- is average.
     XTOTAL(2) = XTOTAL(2) + (ONHAND*IIF(XAVG_COST,NFAVE_COST,COSTUSE))
     XTOTAL(4) = XTOTAL(4) + (ONORDER*IIF(XAVG_COST,NFAVE_COST,COSTUSE))
  ENDIF
  XTOTAL(3) = XTOTAL(3) + ONORDER
  SELECT (lcTempFile)
  IF .NOT. EOF()
    SKIP
  ELSE
    EXIT
  ENDIF
  XPRT_DYE=.F.
ENDDO

*!*************************************************************
*! Name      : STY910
*! Developer : IHB
*! Date      : 12/20/1998
*! Purpose   : Set the filter for the style file.
*!*************************************************************
*! Called from : MAMATST
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO STY910
*!*************************************************************
PROCEDURE STY910

*B605264 RAE [BEGIN] Display all styles if it domestic or not
*lcRpExp = lcRpExp + " .AND. STATUS = XSTATUS .AND. MAKE = .T."
lcRpExp = lcRpExp + " .AND. STATUS = XSTATUS "
*B605264 RAE [END]
SELECT STYLE
SET FILTER TO &lcRpExp
RETURN

*!*************************************************************
*! Name      : lfvStyMaj
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate style major in range entered in grid.
*!*************************************************************
*! Calls     : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStyMaj()
*!*************************************************************
FUNCTION lfvStyMaj
lcMStyle = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcMStyle = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcMStyle) AND !SEEK(lcMStyle,'STYLE')
  lcMStyle = gfStyBrw('M',"","",.F.)
  &lcFldLocNam = IIF(!EMPTY(lcMStyle),lcMStyle,lcOldValue)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'

*--Set needed orders in grid. 
SELECT STYLE
SET ORDER TO TAG Style
SELECT FABDYE
SET ORDER TO TAG FabDye
SELECT FABRIC
SET ORDER TO TAG Fabric

lcOldValue = ' '
llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')
lnPrnDyeP = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRpPrnDye'),1)
laOGObjCnt[lnPrnDyeP] = llDyelot
= lfOGShowGet('llRpPrnDye')

*--Get the style major picture.
lcStyPict = '@! '+gfItemMask('PM')
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()
RETURN

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]

*B038431,1 NNA 09/09/2004 (Begin) Check if there is item code structure or not
IF lfSegInfo()
*B038431,1 NNA (End)

  =gfItemMask(@laMajSeg)
  FOR lnCnt=1 TO ALEN(laMajSeg,1)
    *--Check for existance of color segment in style structure.
    IF laMajSeg[lnCnt,1]='C'
      *--Get the color length and width.
      lnClrSrt = laMajSeg[lnCnt,4]
      lnClrEnd = LEN(laMajSeg[lnCnt,3])
      EXIT
    ENDIF
  ENDFOR

*B038431,1 NNA (Begin)
ENDIF
*B038431,1 NNA (End)

RETURN

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Timour A. K.
*! Date      : 04/04/98
*! Purpose   : Validate primary fabric and fabrics in range
*!             entered in grid.
*!*************************************************************
*! Calls     : FaBrow()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
lcFabric = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcFabric = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcFabric) AND !SEEK(lcFabric,'FABRIC')
  = FaBrow(@lcFabric,'*')
  &lcFldLocNam = IIF(!EMPTY(lcFabric),lcFabric,lcOldValue)
ENDIF
RETURN

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

lcRPFrnFab = lcFromFab
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
*B604528,1 TMI [Start] rename 'lfvpbOk' to 'lfvpbFabOk'
*FUNCTION lfvpbOk
FUNCTION lfvpbFabOk
*B604528,1 TMI [End  ]

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
*! Name      : lfSegInfo
*! Developer : Nader Nabil (NNA)
*! Date      : 09/09/2004
*! Purpose   : Check if The item code Structure found or not.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns : False or True
*!*************************************************************
*! Example   : =lfSegInfo()
*!*************************************************************
*!038431 (Bld41)
FUNCTION lfSegInfo
lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  llOgtrmnat = .T.
  CLEAR READ  
  RETURN .F.
ENDIF
SELECT(lnalias)
RETURN
*-- End of function lfSegInfo.