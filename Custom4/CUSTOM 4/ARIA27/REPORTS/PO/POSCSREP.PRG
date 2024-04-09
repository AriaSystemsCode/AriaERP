*:***************************************************************************
*: Program file  : POSCSREP
*: Program desc. : Style Cost Sheet Report
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO )
*: Developer     : Mohamed Atia Badran  (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ENDREPORT
*:    Functions  : gfModalGen,gfCodDes,gfGetMemVar,gfUserPriv,gfItemMask,
*:               : lfwRepWhen,lfEvalSegs,lfNMajType,lfSRVSty,lfStySum,lfSRVFab,
*:               : lfFabSum,lfRecreate,lfClearRep,lfChanged,lfFillForm,
*:               : lfFormDefa 
*:    Procedure  : lpStyClr,lpStyOnly,lpSCPrn,lpCPrn
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POSCSREP
*:***************************************************************************
*: This Report Program is due to E300967,4 ...
*E301170,1 MAB 03/15/1999 Use variable detect OG filter changes.
*B802028,1 ADEL 03/25/1999 Fix the bug of printing the first item (fabric or trim..) only
*B802028,1                 and repeating the colors when printing by Style format and
*B802028,1                 cause a difference between the total cost sheet when 
*B802028,1                 printing by STYLE/COLOR format and By Style format. 
*B802028,1                 2) Show one line for the fabric in case of fabric color
*B802028,1                   color as style color.
*B802157,1 Reham On 05/11/1999
*B802157,1 Print the total cost by size.
*B802313,1 Reham On 06/06/1999
*B802313,1 Fix the error of acumlating the unit & total cost 
*B802313,1 if press preview several times.
*B802451,1 Reham On 08/15/1999
*B802451,1 1- Prevent printing duplicate cost items.
*B802451,1 2- Change the looping condition to be by Non-Major instead of colors.
*B603348,1  ABD 02/07/2000 Increase The Picture Of The purchase price per item [UnTCost]
*B603348,1  To 9999999.999 & the Picture Of The TotCost [ ToTCost ] To 999999999999.99 
*B603348,1  Instead of the fields.
*B602887,1 WMA 05/21/2000 in case (use multi currency) only
*B602887,1                1- Remove the sub totals & totals from total cost column.
*B602887,1                2- Add new column to calculate the equivelant total cost 
*B602887,1                   with the its currency & the sub totals & totals in this column.
*B603713,10 HBG 11/22/2000 Increase The Picture Of The pricea , priceb , pricec
*B606065,1 KHM 06/05/2002 Fix the problem of printing the MfgCode twice
*B120793,1 MMM 12/07/2003 Fix bug of not showing the style in the styles list if the first 
*B120793,1                Color of the style have cancelled status.         
*B126622,1 NNA 04/06/2005  Fix bug that when you print report by Style only you'll find that
*B126622,1 NNA             the report calculate the style cost incorrectly that because it 
*B126622,1 NNA             accumulate all costs without getting the Average.
*B131001,1 MHM 03/14/2006  Fix bug of wrong cost calculation in style cost sheet.
*B040210,1 TMI 05/08/2006 Fix the following bugs
*B040210,1 TMI            1) with extended scales only the first scale is printed
*B040210,1 TMI            2) The colors are duplicated
*B040210,1 TMI            3) When multiple styles were selected , we want to begin a printing every single style on new page
*B040210,1 TMI            4) in printing The fonts are bigger, the space between lines is smaller
*****************************************************************************
*

*B602887,1 WMA define Variables [Begin]

**-- define Variables --**
STORE SPACE(0) TO lnECost,lcCurCod
STORE 0 TO lnETotCost,lnLinTot,lnLenVal
STORE .F. TO llMulCurr

**-- open needed files --**
=gfOpenFile(gcSysHome+'SYCCURR','Ccurrcode','SH')

**-- chick if use multi currency --**
llMulCurr  = gfGetMemVar('llMulCurr')
*B602887,1 WMA [End]

*B126622,1 NNA 04/06/2005 (BEGIN) Define new variable to hold the current Temp file's Style
STORE '' TO lcCurStyle
*B126622,1 NNA (END)

*B802157,1 Reham On 05/11/1999    *** Begin ***
*B802157,1 Get the extended size scale system variable.
llMScale = gfGetMemVar('M_USEEXSSC')
*B802157,1 Reham On 05/11/1999    *** End   ***

*B040210,1  TMI [Start] Get Extended Size Scale Width
lnExtWid = gfGetMemVar('M_EXTWIDTH')
*B040210,1  TMI [End  ] 

*B802451,1 Reham On 08/08/1999    *** Begin ***
*B802451,1 Get the Major length.
lnMajorLen = LEN(gfItemMask("PM"))
*B802451,1 Reham On 08/08/1999    *** End   ***

*-- if first time you (preview or run ) or user change critria.

*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes.[Begin
*IF llClearFn OR !(lcRpExp == lcLastExpr)   OR ;
*   (llChStyle  OR llChFabric OR llChStyClr)

IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variable detect OG filter changes.[End

*  lcRpExp = [STYLE = '' AND SEEK(CSTYMAJOR,'BOM') AND ] + lcRpExp
  llClearFn  = .F.  && do clear function flag
  llChStyClr = .F.  && change color/style or color selection flag
  llChFabric = .F.  && change fabric filter flag.
  llChStyle  = .F.  && change style filter flag.
  
  IF RECCOUNT(WORKFILE) > 0 
    USE IN (WORKFILE)
    = lfRecreate()
  ENDIF

  SELECT (WORKFILE)
  IF lcRpStyClr='C'
    *B802451,1 Reham On 08/11/1999   *** Begin ***
    *B802451,1 Change the index to be like the current bom index.
    *INDEX ON cItmMajor + SCLR + TYP + IIF(!(CCATGTYP $ "PMD"),ITEM,MFGCODE) +
    *         ICLR + STR(RECNO(),7) TAG &WORKFILE

    *B126622,1 NNA 04/06/2005 (Begin) Fix the index to by style and type to get arrange by costing type
    *INDEX ON cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
    INDEX ON cItmMajor + Typ + cSeekStyle + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
    *B126622,1 NNA (End)

    *B802451,1 Reham On 08/11/1999   *** End   ***
    DO lpStyClr           && PRINT BY COLOR

  ELSE
    *B802451,1 Reham On 08/11/1999   *** Begin ***
    *B802451,1 Change the index to be like the current bom index.
    *INDEX ON cItmMajor + TYP + SCLR + IIF(CCATGTYP # "M",ITEM,MFGCODE) +;
    *         ICLR + STR(RECNO(),7) TAG &WORKFILE

    *B126622,1 NNA 04/06/2005 (Begin) Fix the index to by style and type to get arrange by costing type
    *INDEX ON cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
    INDEX ON cItmMajor + Typ + cSeekStyle + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
    *B126622,1 NNA (End)

    *B802451,1 Reham On 08/11/1999   *** End   ***
    DO lpStyOnly          && PRINT FOR ALL COLORS
  ENDIF           

ENDIF  && end if first time you (preview or run ) or user change critria.
  
IF RECCOUNT(WORKFILE) = 0
  =gfModalGen(.f.,.f.,.f.,.f.,'There are no records to display...! ')  
  RETURN
ENDIF

* INITIALIZE PRINTER
CLEAR TYPEAHEAD
SET DEVICE TO PRINT

SELECT (WORKFILE)
GO TOP
*-- call Print functions.
IF lcRpStyClr='C'
  DO lpSCPrn           && PRINT BY COLOR
ELSE
  DO lpCPrn            && PRINT FOR ALL COLORS
ENDIF

DO ENDREPORT
SET DEVICE TO SCREEN
*-- end of report code.

*----------- Procedures and Functions section -----------------
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lpStyClr
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Collect report data in style/color case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpStyClr
*!*************************************************************
PROCEDURE lpStyClr
SELECT STYLE
*-- Scan all selected styles.
*B120793,1 MMM 12/07/2003 Fix bug of not showing the style in the styles list if the first 
*B120793,1                Color of the style have cancelled status.[Start]         
*B126622,1 NNA 04/06/2005 (BEGIN) Comment this line because lcRepExp already has the style.status ='A'
*lcRpExp = lcRpExp + ' AND Style.Status = "A"'
*B126622,1 NNA (END)
*B120793,1 MMM.[End]

SCAN FOR &lcRpExp
  XSTYLE = cStyMajor

  *B802451,1 Reham On 08/08/1999   *** Begin ***
  *B802451,1 Prevent printing duplicate cost items.
  *XSTYCLR = IIF(lcFree_Clr = 'C',SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******')
  XSTYCLR = SUBSTR(STYLE.STYLE,lnMajorLen+1)
  *B802451,1 Reham On 08/08/1999   *** Begin ***

  SELECT BOM
  = SEEK(XSTYLE)

  *-- scan style in BOM file
  *B802451,1 Reham On 08/12/1999   *** Begin ***
  *B802451,1 Scan for any record similar to the current style.
  *-- scan style in BOM file
  *SCAN REST WHILE (cItmMajor = XSTYLE)
    *IF ('******' $ cItmMask) OR (cItmMask = STYLE.STYLE)
  SCAN REST WHILE (cItmMajor = XSTYLE) FOR LIKE(STRTRAN(cItmMask,'*','?') , Style.Style)
  *B802451,1 Reham On 08/12/1999   *** End   ***

   
      XUNTCOST=0
      
      DO CASE
        CASE cCatgTyp $ "PMD"
      
          SCATTER MEMVAR MEMO
          m.SClr       = XSTYCLR
          m.cSeekStyle = STYLE.STYLE
          
          *B802451,1 Reham On 08/08/1999   *** Begin ***
          *B802451,1 Prevent printing duplicate cost items.
          *m.ICLR       = IIF(m.ICLR = '******', m.SClr, m.ICLR)
          *B802451,1 Reham On 08/08/1999   *** End   ***
          
          INSERT INTO (WORKFILE) FROM MEMVAR

        OTHERWISE
        
          SCATTER MEMVAR MEMO
          *B802451,1 Reham On 08/08/1999   *** Begin ***
          IF cCatgTyp = "F" .OR. (cCatgTyp = "T" .AND. Trim_Invt)
            *B802451,1 Prevent printing duplicate cost items.
            *XKEY= PADR(ITEM,7) + IIF(ICLR='******',XSTYCLR,ICLR)
            XKEY= PADR(ITEM,7) + IIF("*" $ ICLR , IIF(lcFree_Clr = 'C',SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),ICLR) , ICLR)
            *B802451,1 Reham On 08/08/1999   *** End   ***
                        
            IF SEEK(XKEY,'FABRIC')
              XUNTCOST= FABRIC.COSTUSE
              XFABDESC= FABRIC.DESC
            ELSE
              XUNTCOST=0
              XFABDESC=SPACE(0)
            ENDIF

            m.DESC     = IIF(ICLR = '******',XFABDESC,m.DESC)
            m.UNTCOST  = XUNTCOST
            m.TOTCOST  = ROUND(m.UNTCOST * m.nBomTOTQTY,2)

          ENDIF
          m.cSeekStyle = STYLE.STYLE
          m.SClr     = XSTYCLR
          
          *B802451,1 Reham On 08/08/1999   *** Begin ***
          *B802451,1 Prevent printing duplicate cost items.
          *m.IClr = IIF((ICLR = '******') OR ('******' $ ITEM),XSTYCLR,ICLR)
          *IF EMPTY(m.ICLR) AND (cCatgTyp = 'S')
          *  m.IClr = SUBSTR(m.ITEM,lnNonMajSt,lnColorLen)
          *ENDIF
          *B802451,1 Reham On 08/08/1999   *** End   ***
          
          INSERT INTO (WORKFILE) FROM MEMVAR

      ENDCASE

    *B802451,1 Reham On 08/12/1999   *** Begin ***
    *ENDIF
    *B802451,1 Reham On 08/12/1999   *** End   ***
  ENDSCAN  && end scan style in BOM file

ENDSCAN    && end Scan all selected styles.
*-- end of lpStyClr.

*!*************************************************************
*! Name      : lpStyOnly
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Collect report data in style only case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpStyOnly
*!*************************************************************
PROCEDURE lpStyOnly
lcOldMajor = ''
*B120793,1 MMM 12/07/2003 Fix bug of not showing the style in the styles list if the first 
*B120793,1                Color of the style have cancelled status.[Start]         

*B126622,1 NNA 04/06/2005 (BEGIN) Comment this line because lcRepExp already has the style.status ='A'
*lcRpExp = lcRpExp + ' AND Style.Status = "A"'
*B126622,1 NNA (END)

*B120793,1 MMM.[End]
SELECT STYLE
*-- Scan all selected styles.
SCAN FOR &lcRpExp

  SELECT BOM
  *-- scan style in BOM file
  SCAN FOR cItmMajor = STYLE.cStyMajor
    IF (lcOldMajor # STYLE.cStyMajor) OR ;
       (cCatgTyp $ 'FS' AND '******' $ cItmMask)
      SCATTER MEMVAR MEMO
      m.cSeekStyle = IIF('******' $ cItmMask , STYLE.STYLE , cItmMask)

      *B802451,1 Reham On 08/08/1999   *** Begin ***
      *B802451,1 Prevent printing duplicate cost items.
      *m.SClr       = IIF(lcFree_Clr = 'C',SUBSTR(m.cSeekStyle,lnNonMajSt,lnColorLen),'******') 
      *B040210,1  TMI [Start] get only the color without the scale part - in case of extended case
      *m.SClr       = SUBSTR(m.cSeekStyle,lnMajorLen+1) 
      m.SClr       = SUBSTR(m.cSeekStyle,lnNonMajSt,lnColorLen)
      *B040210,1  TMI [End  ] 
      *B802451,1 Reham On 08/08/1999   *** End   ***

      *B802028,1 (Begin) Remark the following line.                     
      *m.IClr = IIF((ICLR = '******') OR ('******' $ ITEM),;
               m.SClr,ICLR)
      *B802028,1 (End)

      *B802451,1 Reham On 08/08/1999   *** Begin ***
      *B802451,1 Prevent printing duplicate cost items.
      IF EMPTY(m.ICLR) AND (cCatgTyp = 'S')
        m.IClr = SUBSTR(m.ITEM,lnNonMajSt,lnColorLen)
      ENDIF
      *B802451,1 Reham On 08/08/1999   *** End   ***

      INSERT INTO (WORKFILE) FROM MEMVAR
    ENDIF
  ENDSCAN  && end scan style in BOM file

  lcOldMajor = STYLE.cStyMajor
  *MAB NOW LIKE LAST ONE
  *B802028,1 (Begin) Do not get record for each color.
  SELECT STYLE
  lcStyMaj = SUBSTR(STYLE,1,lnMajorLen)
  LOCATE FOR SUBSTR(STYLE,1,lnMajorLen) > lcStyMaj

  *B126622,1 NNA 04/06/2005 (Begin) After the Previous Command,Cursor moves to the first record for
  *B126622,1 NNA            the next style and after EndScan Cursor moves to the second Record So
  *B126622,1 NNA            I Move to the Previous Record to move to the First Record for the next style
  *B126622,1 NNA            with EndScan
  skip -1
  *B126622,1 NNA (End)
  
  *B802028,1 (End)
ENDSCAN  && end Scan all selected styles.

*-- end of lpStyOnly.

*!*************************************************************
*! Name      : lpSCPrn
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Print report data in style/color case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpSCPrn
*!*************************************************************
PROCEDURE lpSCPrn
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
* STYLE               ITEM   MFG               UOM      UNIT     UNIT       TOTAL
* COLOR  ITEM         COLOR  CODE DESCRIPTION  USE  QUANTITY     COST        COST
* XXXXXX XXXXXXXXXXXX XXXXXX  XX  XXXXXXXXXXXX XXX   999.999  999.999   999999.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
BREAK  = 'CITMMAJOR + SCLR'
PAGENO = 0
ROW    = 99
XTIME  = TIME()
XREPORT='MFCSREP'
R_TITLE ='Style Cost Sheet Report'

*B802157,1 Reham On 05/11/1999   *** Begin ***
*B802157,1  Variable to calculate cost by size.
lnCstBySiz = 0
*B802157,1 Reham On 05/11/1999   *** End   ***

*B802313,1 Reham On 06/06/1999   *** Begin ***
*B802313,1 Variable hold the totl count of the current scale.
lnSCnt    = 8
*B802313,1 Variable hold the sizes selected for the current cost item.
lnBomSzs  = 8
*B802313,1 Variable hold the current unit cost.
lnUntCost = 0
*B802313,1 Variable hold the current total cost.
lnTotCost = 0
*B802313,1 Reham On 06/06/1999   *** End   ***

STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5
XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
XPRV_TYP = TYP
HBREAK=SPACE(1)
lcSayStyle = cSeekStyle
IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF

*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------
DO WHILE .T.

  IF ROW >=55
    PAGENO = PAGENO+1
    
    *B602887,1 WMA if use multi currency call local header Procedure [Begin]
    *DO RPT_HDR WITH XREPORT,'',R_WIDTH
    IF llMulCurr
      DO lfPrHeader WITH PAGENO
    ELSE
      DO RPT_HDR WITH XREPORT,'',R_WIDTH
    ENDIF  
    *B602887,1 WMA [End]
    
    *B603348,1 ABD Remark The next Lines & Change The headre to Indicate the Printing .[ Begin ]
    *@ 05,00 SAY '              ITEM   MFG                      UOM      UNIT     UNIT       TOTAL'
    *@ 06,00 SAY ' ITEM         COLOR  CODE   DESCRIPTION       USE  QUANTITY     COST        COST'
   
    *B602887,1 WMA if use multi currency add new column in the header [Begin]
     *@ 05,00 SAY '              ITEM   MFG                    UOM    UNIT        UNIT        TOTAL'
     *@ 06,00 SAY ' ITEM         COLOR  CODE   DESCRIPTION     USE QUANTIT        COST         COST'
    IF llMulCurr
      @ 05,00 SAY '              ITEM   MFG                    UOM    UNIT        UNIT        TOTAL           TOTAL'
      @ 06,00 SAY ' ITEM         COLOR  CODE   DESCRIPTION     USE QUANTIT        COST         COST           EQUV.'
    ELSE
      @ 05,00 SAY '              ITEM   MFG                    UOM    UNIT        UNIT        TOTAL'
      @ 06,00 SAY ' ITEM         COLOR  CODE   DESCRIPTION     USE QUANTIT        COST         COST'
    ENDIF
    *B602887,1 WMA [End]
    
    *B603348,1 ABD [ End ]

    *B602887,1 WMA [Begin]
    *@ 07,00 SAY REPLICATE('=',80)
    IF llMulCurr
      @ 07,00 SAY REPLICATE('=',96)
    ELSE
      @ 07,00 SAY REPLICATE('=',80)
    ENDIF  
    *B602887,1 WMA [End]
    ROW=08
  ENDIF
  

  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
  
    *B602887,1 WMA [Begin]
    *@ ROW,00 SAY REPLICATE('=',80)
    IF llMulCurr
      @ ROW,00 SAY REPLICATE('=',96)
    ELSE
      @ ROW,00 SAY REPLICATE('=',80)
    ENDIF  
    *B602887,1 WMA [End]
    
    *- If user can access cost.
    IF qCostPrv   && COSTING ACCESS
      ROW = ROW+1
      *B603348,1 ABD Remark The next Lines & Change The headre to Indicate the total Cost .[ Begin ]
      *@ ROW,028 SAY '**** TOTAL COST FOR : ' + PADR(lcSayStyle,19)
      @ ROW,025 SAY '**** TOTAL COST FOR  : ' + PADR(lcSayStyle,19)  
      *B603348,1 ABD [ End ]
      XSTYCST=XTOT1+XTOT2+XTOT3+XTOT4+XTOT5
      
      *B802157,1 Reham On 05/11/1999   *** Begin ***
      *B802157,1 Print the calculated cost by size var.
      *@ ROW,070 SAY XSTYCST PICTURE '9999999.99'
      *B603348,1 ABD Remark The next Lines & Change The Picture Of Printing to '999999999'99 .[ Begin ]
      *@ ROW,070 SAY lnCstBySiz PICTURE '9999999.99'
      
      *B602887,1 WMA if use multi currency Print total cost With Equivalent [Begin]
      *@ ROW,068 SAY lnCstBySiz PICTURE '999999999.99'
      IF llMulCurr
        @ ROW,084 SAY lnETotCost PICTURE '999999999.99'
      ELSE
        @ ROW,068 SAY lnCstBySiz PICTURE '999999999.99'
      ENDIF
      *B602887,1 WMA [End]
      
      *B603348,1 ABD [ End ]
      *B802157,1 Reham On 05/11/1999   *** End   ***

      ROW=ROW+1
      *B603348,1 ABD Remark The next Lines & Move SELLING PRICE to the Left &
      *B603348,1     Change The Picture Of Printing to '999999999'99 .[ Begin ]
      *@ ROW,005 SAY 'SELLING PRICE :'
      *@ ROW,20 SAY XPRICEA PICTURE '9999.99'
      *@ ROW,028 SAY '** GROSS PROFIT FOR : ' + PADR(lcSayStyle,19)
      *@ ROW,70 SAY XPRICEA - XSTYCST PICTURE '9999999.99'
      *B603713,10 HBG 11/22/2000 Increase Picture Of The pricea [Begin]
      *@ ROW,003 SAY 'SELLING PRICE :'
      *@ ROW,17 SAY XPRICEA PICTURE '9999.99'
      @ ROW,000 SAY 'SELLING PRICE :'      
      @ ROW,14 SAY XPRICEA PICTURE '999999999.99'
      *-- Fix bug of price is get out from the report lyout
      *@ ROW,026 SAY '** GROSS PROFIT FOR : ' + PADR(lcSayStyle,19)
      @ ROW,026 SAY '** GROSS PROFIT FOR: ' + PADR(lcSayStyle,19)
      *B603713,10 [End]
      
      *B602887,1 WMA if use multi currency print gross profit with Equivalent [Begin]
      *@ ROW,68 SAY XPRICEA - XSTYCST PICTURE '999999999.99'
      IF llMulCurr
        *@ ROW,68 SAY XPRICEA - XSTYCST PICTURE '999999999.99'
		@ ROW,084 SAY XPRICEA - lnETotCost PICTURE '999999999.99'
		lnETotCost = 0 
		lnLinTot = 0 
      ELSE
        @ ROW,68 SAY XPRICEA - XSTYCST PICTURE '999999999.99'
      ENDIF
      *B602887,1 WMA [End]
      
      *B603348,1 ABD [ End ]
      
      
      ROW=ROW+1 
	  *B602887,1 WMA [Begin]
      *@ ROW,00 SAY REPLICATE('=',80)
	  IF llMulCurr
	    @ ROW,00 SAY REPLICATE('=',96)
	  ELSE
	    @ ROW,00 SAY REPLICATE('=',80)
	  ENDIF  
	  *B602887,1 WMA [End]
     
    ENDIF
   
    ROW=ROW+1
    
    *B802157,1 Reham On 05/11/1999   *** Begin ***
    *B802157,1  Variable to calculate cost by size.
    lnCstBySiz = 0
    *B802157,1 Reham On 05/11/1999   *** End   ***
    
    *B802313,1 Reham On 06/06/1999   *** Begin ***
    *B802313,1 Variable hold the totl count of the current scale.
    lnSCnt   = 8
    *B802313,1 Variable hold the sizes selected for the current cost item.
    lnBomSzs = 8
    *B802313,1 Variable hold the current unit cost.
    lnUntCost = 0
    *B802313,1 Variable hold the current total cost.
    lnTotCost = 0
    *B802313,1 Reham On 06/06/1999   *** End   ***
    
    STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

    XNEW_STY=.T.

    HBREAK = &BREAK
    lcSayStyle = cSeekStyle
    EXIT
  ENDDO

  IF EOF()
    EXIT
  ENDIF

  IF XNEW_STY
    XNEW_STY=.F.
    
    @ ROW,000 SAY PADR(lcStyTitle,19) + '->' + PADR(cSeekStyle,19) + '  '
    IF SEEK(cSeekStyle,'STYLE')
      XSTYDESC = STYLE.DESC1
      XPRICEA  = STYLE.PRICEA
    ELSE
      XSTYDESC=SPACE(0)
      XPRICEA =0
    ENDIF

    @ ROW,42 SAY PADR(XSTYDESC,38)
    ROW=ROW+1

    *B602887,1 WMA [Begin]
    IF llMulCurr
      lnLenVal = 96
    ELSE
      lnLenVal = 80
    ENDIF
    *B602887,1 WMA [End]
    
    DO CASE
      CASE Typ = "1"
        XHDR = '< '+lcSlbl1+' >'
         *B602887,1 WMA [Begin]
         *lnLen=INT(((80-LEN(XHDR))/2))
         lnLen=INT(((lnLenVal-LEN(XHDR))/2))
         *B602887,1 WMA [End]
         @ ROW,00 SAY REPLICATE('-',lnLen)
         @ ROW,lnLen+1 SAY XHDR
         *B602887,1 WMA [Begin]
         *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
		 @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen)) 	 
         *B602887,1 WMA [End]
      CASE Typ = "2"
        XHDR='< '+lcSlbl2+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
       *B602887,1 WMA [End]
      CASE Typ = "3"
        XHDR='< '+lcSlbl3+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]
      CASE Typ = "4"
        XHDR='< '+lcSlbl4+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
		lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]
      CASE Typ = "5"
        XHDR='< '+lcSlbl5+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
		@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]
      ENDCASE

    ROW=ROW+1
  ENDIF
  
  *B602887,1 WMA if use multi currency get currency code [Begin]   
  IF llMulCurr
    DO CASE
      CASE cCatgTyp = "P"
        lcCurCod = Style.CPriceCur
      CASE cCatgTyp $ "FTS"
        lcCurCod = gcBaseCurr
      OTHERWISE     
        lcCurCod = Style.CDutyCur      
    ENDCASE 
  ENDIF
  *B602887,1 WMA [End]   
  
  @ ROW,01 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
  @ ROW,14 SAY ICLR
  
  IF cCatgTyp = "M"
    
    *B606065,1 KHM 06/05/2002 (Begin) Commenting this line because the desc field has the
    *B606065,1                MFGcode.
    *@ ROW,21 SAY PADR(MFGCODE,6)
    *B606065,1 KHM 06/05/2002 (End)
    
  ENDIF
  *B603348,1 ABD Remark The next Lines & Move them to the Left 
  *B603348,1     & Print 15 charcter from Desc .[ Begin ]
  *@ ROW,28 SAY SUBSTR(DESC,1,17)
  *@ ROW,46 SAY UOM  
  *@ ROW,52 SAY nBomTOTQTY PICTURE '999.999'  

  *B606065,1 KHM 06/05/2002 (Begin) Changing the column position and the Num. of printed characters
  *@ ROW,28 SAY SUBSTR(DESC,1,15)
  @ ROW,21 SAY SUBSTR(DESC,1,17)
  *B606065,1 KHM 06/05/2002 (End)
  
  @ ROW,44 SAY UOM
  @ ROW,48 SAY nBomTOTQTY PICTURE '999.999'
  *B603348,1 ABD [ End ]

  *B802157,1 Reham On 05/11/1999   *** Begin ***
  *B802157,1 Call local function to calculate cost.
  
  Z = Typ
  =lfCostBySz()
  *B802157,1 Reham On 05/11/1999   *** End   ***

  *- if user can access cost
  IF qCostPrv         && COSTING ACCESS
    *B802313,1 Reham on 06/06/1999   *** Begin ***
    *B802313,1 Print the variables instead of the fields.
    *@ ROW,61 SAY UNTCOST PICTURE '999.999'
    *@ ROW,71 SAY TOTCOST PICTURE '999999.99'
    *B603348,1  ABD Increase The Picture Of The purchase price per item [UnTCost]
    *B603348,1  To 9999999.999 & the Picture Of The TotCost [ ToTCost ] To 999999999999.99 
    *B603348,1  Instead of the fields.
    *@ ROW,61 SAY lnUntCost PICTURE '999.999'
    *@ ROW,71 SAY lnTotCost PICTURE '999999.99'
    @ ROW,56 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,68 SAY lnTotCost PICTURE '999999999.99'
    *B602887,1 WMA if use multi currency print cost with Equivalent [Begin]
    IF llMulCurr
      @ ROW,80 SAY IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
   	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,84 SAY lnECost PICTURE '999999999.99'
      lnETotCost = lnETotCost + lnECost
      lnLinTot = lnLinTot + lnECost
    ENDIF
    *B602887,1 WMA [End]
    
    *B603348,1  ABD [ End ]
    *B802313,1 Reham on 06/06/1999   *** End   ***
  ENDIF
  ROW=ROW+1
  Z = Typ

  *B802157,1 Reham On 05/11/1999   *** Begin ***
  *B802157,1 Already calculated in the local function to calculate cost.
  *XTOT&z=XTOT&z+TOTCOST
  *B802157,1 Reham On 05/11/1999   *** End   ***

  SELE &WORKFILE
  SKIP
  IF XPRV_TYP<>TYP

    IF qCostPrv          && COSTING ACCESS

      DO CASE
        *CASE XPRV_TYP='1'
        *  @ ROW,027 SAY ' '+lcSlbl1+' TOTAL :=>'
        *  @ ROW,070 SAY XTOT1 PICTURE '9999999.99'
        *CASE XPRV_TYP='2'
        *  @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
        *  @ ROW,070 SAY XTOT2 PICTURE '9999999.99'
        *CASE XPRV_TYP='3'
        *  @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
        *  @ ROW,070 SAY XTOT3 PICTURE '9999999.99'
        *CASE XPRV_TYP='4'
        *  @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
        *  @ ROW,070 SAY XTOT4 PICTURE '9999999.99'
        *CASE xPrv_Typ = "5"
        *  @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
        *  @ ROW,070 SAY XTOT5 PICTURE '9999999.99'
        
        CASE XPRV_TYP='1'
          @ ROW,027 SAY ' '+lcSlbl1+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT1 PICTURE '9999999999.99'          
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT1 PICTURE '9999999999.99'          
          ENDIF       
          *B602887,1 WMA [End]
        CASE XPRV_TYP='2'
          @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
        CASE XPRV_TYP='3'
          @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE
            @ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
        CASE XPRV_TYP='4'
          @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0           
          ELSE  
            @ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
        CASE xPrv_Typ = "5"
          @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT5 PICTURE '9999999999.99' 
          IF llMulCurr 
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0          
          ELSE
            @ ROW,067 SAY XTOT5 PICTURE '9999999999.99' 
          ENDIF        
          *B602887,1 WMA [End]
      ENDCASE
    ENDIF

    XPRV_TYP=TYP
    ROW=ROW+1
    IF &BREAK=HBREAK
      
      *B602887,1 WMA [Begin]
      IF llMulCurr
        lnLenVal = 96
      ELSE
        lnLenVal = 80
      ENDIF
      *B602887,1 WMA [End]
      
      DO CASE
        CASE Typ = "1"
          XHDR='< '+lcSlbl1+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]
        CASE Typ = "2"
          XHDR='< '+lcSlbl2+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]
        CASE Typ = "3"
          XHDR='< '+lcSlbl3+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]
        CASE Typ = "4"
          XHDR='< '+lcSlbl4+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]
        CASE Typ = "5"
          XHDR='< '+lcSlbl5+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))    
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))    
          *B602887,1 WMA [End]
      ENDCASE  
      
      ROW=ROW+1
    ENDIF
  ENDIF
ENDDO
*-- end of lpSCPrn

*!*************************************************************
*! Name      : lpCPrn
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Print report data in style only case
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCPrn
*!*************************************************************
PROCEDURE lpCPrn
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*   STYLE: 123456789012 12345678901234567890      STATUS: 123456
*
*   SEASON..: 12 1234567890   DIVISION: XX XXXXXXXXXX   GROUP : XX XXXXXXXXXX
*   PRICEA  : 9999.99         PRICEB  : 9999.99         PRICEC: 9999.99
*   SIZE (X): 111 222 333 444 555 666 777 888
*   PREPK(X): 111 222 333 444 555 666 777 888
*   COLORS  : 111111 XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX
*             XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX XXXXXX
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                      ITEM  MFG                UOM     UNIT     UNIT       TOTAL
*      ITEM            COLOR CODE DESCRIPTION   USE QUANTITY     COST        COST
*      XXXXXXX  XXX   XX  XXXXXXXXXXXXXXXXXXXX  XXX  999.999  999.999   999999.99
*


*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*STYLE         ITEM  MFG                        UOM      UNIT     UNIT       TOTAL
*COLOR ITEM    COLOR CODE ....DESCRIPTION.....  USE  QUANTITY     COST        COST
* XXX  XXXXXXX  XXX   XX  XXXXXXXXXXXXXXXXXXXX  XXX   999.999  999.999   999999.99
*               XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX XXX-XXX
*
BREAK  = 'CITMMAJOR'
PAGENO = 0
ROW    = 99
XTIME  = TIME()
XREPORT='MFCSREP'
R_TITLE ='Style Cost Sheet Report'

*B802157,1 Reham On 05/11/1999   *** Begin ***
*B802157,1  Variable to calculate cost by size.
lnCstBySiz = 0
*B802157,1 Reham On 05/11/1999   *** End   ***

*B802313,1 Reham On 06/06/1999   *** Begin ***
*B802313,1 Variable hold the totl count of the current scale.
lnSCnt   = 8
*B802313,1 Variable hold the sizes selected for the current cost item.
lnBomSzs = 8
*B802313,1 Variable hold the current unit cost.
lnUntCost = 0
*B802313,1 Variable hold the current total cost.
lnTotCost = 0
*B802313,1 Reham On 06/06/1999   *** End   ***

STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
XPRV_TYP = TYP

HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF
*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------

*B126622,1 NNA 04/06/2005 (BEGIN) get the Avg_Cost for Style major / Fabric
SELECT(WORKFILE)
SCAN
  =lfGetAvg()
ENDSCAN
GO TOP
*B126622,1 NNA (END)
*B040210,1  TMI [Start] Define the xstyle variable
XSTYLE = ' '
*B040210,1  TMI [End  ] 
DO WHILE .T.
  IF ROW >=53
    PAGENO = PAGENO+1
    *B602887,1 WMA if use multi currency call local header Procedure [Begin]
    *DO RPT_HDR WITH XREPORT,'',R_WIDTH
    IF llMulCurr
      DO lfPrHeader WITH PAGENO
    ELSE
      DO RPT_HDR WITH XREPORT,'',R_WIDTH
    ENDIF  
    *B602887,1 WMA [End]
    
    *B603348,1 ABD Remark The next Lines & Change The headre to Indicate the Printing .[ Begin ]    
    *@ 05,00 SAY '                      ITEM  MFG                UOM     UNIT     UNIT       TOTAL'
    *@ 06,00 SAY '      ITEM            COLOR CODE   DESCRIPTION USE QUANTITY     COST        COST'
    
    *B602887,1 WMA if use multi currency add new column in the header [Begin]
    *@ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL'
    *@ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST'
    IF llMulCurr
      @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL           TOTAL'
      @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST           EQUV.'  
    ELSE
      @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL'
      @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST'
    ENDIF
    *B602887,1 WMA [End]
    
    *B603348,1 ABD [ End ]
    *B602887,1 WMA [Begin]
    *@ 07,00 SAY REPLICATE('=',80)
    IF llMulCurr
      @ 07,00 SAY REPLICATE('=',96)
    ELSE
      @ 07,00 SAY REPLICATE('=',80)
    ENDIF  
    *B602887,1 WMA [End]
    ROW=08
  ENDIF
  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    *B602887,1 WMA [Begin]
    *@ ROW,00 SAY REPLICATE('=',80)
    IF llMulCurr
      @ ROW,00 SAY REPLICATE('=',96)
    ELSE
      @ ROW,00 SAY REPLICATE('=',80)
    ENDIF     
    *B602887,1 WMA [End]
    ROW = ROW+1
    *B603348,1 ABD Remark The next Lines & Change The headre to Indicate the total Cost .[ Begin ]
    *@ ROW,029 SAY '**** TOTAL COST FOR : '+SUBSTR(HBREAK,1,12)+'  '+SUBSTR(HBREAK,13,6)
    @ ROW,022 SAY '**** TOTAL COST FOR : '+SUBSTR(HBREAK,1,12)+'  '+SUBSTR(HBREAK,13,6)
    *B603348,1 ABD [ End ]

    *B126622,1 NNA 04/06/2005 (Begin) Comment this line because there is no need to it
    *XSTYCST=XTOT1+XTOT2+XTOT3+XTOT4+XTOT5
    *--Get the grand total from the lcAvgCost file
    SELECT(lcAvgCost)
    GO TOP
    IF llMulCurr
      SUM AVGCOST TO lnETotCost FOR CSTYLE = HBREAK
    ELSE
      SUM AVGCOST TO lnCstBySiz FOR CSTYLE = HBREAK
    ENDIF
    SELECT(WORKFILE)
    *B126622,1 NNA (END)

    IF qCostPrv         && COSTING ACCESS
      *B802157,1 Reham On 05/11/1999   *** Begin ***
      *B802157,1 
      *@ ROW,070 SAY XSTYCST PICTURE '999999.99'
      
      *B603348,1 ABD Remark The next Lines & Change The Picture Of Printing to '999999999'99 .[ Begin ]
      *@ ROW,070 SAY lnCstBySiz PICTURE '9999999.99'
      *B602887,1 WMA if use multi currency Print total cost With Equivalent[Begin]
      *@ ROW,068 SAY lnCstBySiz PICTURE '999999999.99'
      IF llMulCurr
   		@ ROW,084 SAY lnETotCost PICTURE '999999999.99'
   		lnETotCost = 0 
		lnLinTot = 0 
      ELSE
        @ ROW,068 SAY lnCstBySiz PICTURE '999999999.99'
      ENDIF     
      *B602887,1 WMA [End]
      *B603348,1 ABD [ End ] 
      
      *B802157,1 Reham On 05/11/1999   *** End   ***
    ENDIF
    ROW=ROW+1
    *B602887,1 WMA [Begin]
    *@ ROW,00 SAY REPLICATE('=',80)
    IF llMulCurr
      @ ROW,00 SAY REPLICATE('=',96)
    ELSE
      @ ROW,00 SAY REPLICATE('=',80)
    ENDIF   
    *B602887,1 WMA [End]
    ROW=ROW+1

    *B802157,1 Reham On 05/11/1999   *** Begin ***
    *B802157,1  Variable to calculate cost by size.
    lnCstBySiz = 0
    *B802157,1 Reham On 05/11/1999   *** End   ***
    
    *B802313,1 Reham On 06/06/1999   *** Begin ***
    *B802313,1 Variable hold the totl count of the current scale.
    lnSCnt   = 8
    *B802313,1 Variable hold the sizes selected for the current cost item.
    lnBomSzs = 8
    *B802313,1 Variable hold the current unit cost.
    lnUntCost = 0
    *B802313,1 Variable hold the current total cost.
    lnTotCost = 0
    *B802313,1 Reham On 06/06/1999   *** End   ***
    
    STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5

    XNEW_STY=.T.

    HBREAK = &BREAK
    EXIT
  ENDDO

  IF EOF()
    EXIT
  ENDIF

  IF XNEW_STY
    *B040210,1  TMI [Start] if new style print it in a new page
    IF !EMPTY(XSTYLE) .AND. &WORKFILE..cItmMajor <> PADR(XSTYLE,19)
      XSTYLE = ALLTRIM(&WORKFILE..cItmMajor)    
      DO WHILE ROW < 53
        ROW = ROW + 1
        @ ROW,1 SAY ''
      ENDDO
      LOOP
    ENDIF
    *B040210,1  TMI [End  ] 
    XSTYLE = ALLTRIM(&WORKFILE..cItmMajor)    
    XNEW_STY=.F.    
    @ ROW,000 SAY ALLTRIM(lcStyMajor) + '->' + XSTYLE
    
    *B131001,1 MHM 03/14/2006  Fix bug of not seeking on the correct record[Start].
    *= SEEK (cSeekStyle,'STYLE')
    = SEEK (SUBSTR(cSeekStyle,1,lnMajorLen),'STYLE')
    *B131001,1 MHM [End].
    
    @ ROW,039 SAY PADR(STYLE.DESC1,41)
    ROW=ROW+2
    *------- NOW DISPLAY ALL STYLE INFO ----*
    XSEAS_DATA = IIF(STYLE.SEASON = 'Y     ','YEAR ROUND',gfCodDes(STYLE.SEASON , 'SEASON'))    && Get Season from codes file. 
    XDIVI_DATA = gfCodDes(STYLE.CDIVISION , 'CDIVISION')    && Get Division from codes file.  
    XGROU_DATA = gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP')    && Get Style group from codes file.  
    *B603348,1 ABD Remark The next Lines & move Them to the left to Indicate the Printing .[ Begin ]
    *@ ROW,04 SAY 'SEASON.:'
    *@ ROW,12 SAY PADR(XSEAS_DATA,17)    
    *@ ROW,31 SAY 'DIVISON:'
    *@ ROW,39 SAY PADR(XDIVI_DATA,17)
    *@ ROW,57 SAY 'GROUP.:'
    *@ ROW,65 SAY SUBSTR(XGROU_DATA,1,15)
    @ ROW,02 SAY 'SEASON.:'
    @ ROW,09 SAY PADR(XSEAS_DATA,17)
    @ ROW,27 SAY 'DIVISON:'
    @ ROW,35 SAY PADR(XDIVI_DATA,17)
    @ ROW,59 SAY 'GROUP.:'
    @ ROW,66 SAY SUBSTR(XGROU_DATA,1,14)
    *B603348,1 ABD [ End ]

    ROW = ROW + 1
    
    *B603348,1 ABD Remark The next Lines & move Them to the left to Indicate the Printing .[ Begin ]
    *@ ROW,04 SAY 'PRICEA :'
    *@ ROW,13 SAY STYLE.PRICEA PICTURE '9999.99'
    *@ ROW,30 SAY 'PRICEB :'
    *@ ROW,39 SAY STYLE.PRICEB PICTURE '9999.99'
    *@ ROW,62 SAY 'PRICEC :'
    *@ ROW,72 SAY STYLE.PRICEC PICTURE '9999.99'
    @ ROW,02 SAY 'PRICEA :'
    *B603713,10 HBG 11/22/2000 Increase Picture Of The pricea [Begin]
    *@ ROW,14 SAY STYLE.PRICEA PICTURE '999999.99'

    *B131001,1 MHM 03/14/2006  Fix bug of not adjesting the position[Start].
    *@ ROW,14 SAY STYLE.PRICEA PICTURE '999999999.99'
    @ ROW,11 SAY STYLE.PRICEA PICTURE '999999999.99'
    *B131001,1 MHM 03/14/2006  [End].

    *B603713,10 [End]
    @ ROW,27 SAY 'PRICEB :'
    *B603713,10 HBG 11/22/2000 Increase Picture Of The priceb [Begin]
    *@ ROW,40 SAY STYLE.PRICEB PICTURE '999999.99'

    *B131001,1 MHM 03/14/2006  Fix bug of not adjesting the position[Start].
    *@ ROW,40 SAY STYLE.PRICEB PICTURE '999999999.99'
    @ ROW,37 SAY STYLE.PRICEB PICTURE '999999999.99'
    *B131001,1 MHM 03/14/2006  [End].

    *B603713,10 [End]
    @ ROW,59 SAY 'PRICEC :'
    *B603713,10 HBG 11/22/2000 Increase Picture Of The priceb [Begin]
    *@ ROW,71 SAY STYLE.PRICEC PICTURE '999999.99'

    *B131001,1 MHM 03/14/2006  Fix bug of not adjesting the position[Start].
    *@ ROW,71 SAY STYLE.PRICEC PICTURE '999999999.99'
    @ ROW,68 SAY STYLE.PRICEC PICTURE '999999999.99'
    *B131001,1 MHM 03/14/2006  [End].

    *B603713,10 [End]
    *B603348,1 ABD [ End ]

    ROW = ROW + 1

    IF qCostPrv         && COSTING ACCESS
      *B603348,1 ABD Remark The next Lines & move Them to the left to Indicate the Printing .[ Begin ]
      *@ ROW,04 SAY 'PROFITA:'
      *@ ROW,13 SAY STYLE.PRICEA - STYLE.TOTCOST PICTURE '9999.99'
      *@ ROW,30 SAY 'PROFITB:'
      *@ ROW,39 SAY STYLE.PRICEB - STYLE.TOTCOST PICTURE '9999.99'
      *@ ROW,62 SAY 'PROFITC:'
      *@ ROW,72 SAY STYLE.PRICEC - STYLE.TOTCOST PICTURE '9999.99'
      @ ROW,02 SAY 'PROFITA:'
      @ ROW,10 SAY STYLE.PRICEA - STYLE.TOTCOST PICTURE '9999999999.99'
      @ ROW,27 SAY 'PROFITB:'

      *B131001,1 MHM 03/14/2006  Fix bug of Geting - profit [Start]
      *@ ROW,36 SAY STYLE.PRICEB - STYLE.TOTCOST PICTURE '9999999999.99'
      @ ROW,36 SAY IIF(STYLE.PRICEB >0 ,(STYLE.PRICEB - STYLE.TOTCOST),0) PICTURE '9999999999.99'
      *B131001,1 MHM 03/14/2006   [End]

      @ ROW,59 SAY 'PROFITC:'

      *B131001,1 MHM 03/14/2006  Fix bug of Geting - profit [Start]
      *@ ROW,67 SAY STYLE.PRICEC - STYLE.TOTCOST PICTURE '9999999999.99'
      @ ROW,67 SAY IIF(STYLE.PRICEC >0,(STYLE.PRICEC - STYLE.TOTCOST),0) PICTURE '9999999999.99'
      *B131001,1 MHM 03/14/2006  [End]
      
      *B603348,1 ABD [ End ]

      ROW=ROW+1
    ENDIF
    *B603348,1 ABD Remark The next Lines & move Them to the left 2 character
    *B603348,1     To Indicate the Printing .[ Begin ]
    *@ ROW,04 SAY 'SIZE (' + STYLE.SCALE + ')'
    @ ROW,02 SAY 'SIZE (' + STYLE.SCALE + ')'
    *B603348,1 ABD [ End ]
 
   *B040210,1  TMI [Start] comment this line
   *=SEEK('S' + STYLE.SCALE,'SCALE')    
   *B040210,1  TMI [End  ] 
   *B603348,1 ABD Remark The next Lines & move Them to the left 2 character
   *B603348,1     To Indicate the Printing .[ Begin ]
    *COL = 14
    COL = 12
    *B603348,1 ABD [ End ]
    
    *B040210,1  TMI [Start] print all scales from scale file - case of extended 
    lnSlct = SELECT()
    lnSclCount = 1       &&  variable to add new line for scale if it is changed
    lcScl = 'S'+SUBSTR(STYLE.SCALE,1,lnExtWid)
    =SEEK(lcScl,'SCALE')    
    SELECT SCALE
    SCAN REST WHILE TYPE+SCALE+PREPAK = lcScl    
      IF lnSclCount>1
        ROW = ROW + 1
        COL = 12
        @ ROW,02 SAY 'SIZE (' + SCALE.SCALE + ')'
      ENDIF
      *B040210,1  TMI [End  ] 
      FOR lnI = 1 TO 8 
        lcSc = STR(lnI,1)
        @ ROW,COL SAY PADL(ALLTRIM(SCALE.SZ&lcSc),5) PICTURE '@Z XXXXX' 
        COL = COL + 6
      ENDFOR
      *B040210,1  TMI [Start] variable to add new line for scale if it is changed
      lnSclCount = lnSclCount + 2      
    ENDSCAN
    =SEEK(lcScl,'SCALE')    
    SELECT (lnSlct)
    *B040210,1  TMI [End  ] 
      
    
    *B603348,1 ABD Remark The next Lines & move Them to the left 5 character
    *B603348,1     To Indicate the Printing .[ Begin ]
    *@ ROW,62 SAY 'PATTERN:'
    *@ ROW,70 SAY ALLTRIM(STYLE.PATTERN)
    
    @ ROW,57 SAY 'PATTERN:'
    @ ROW,67 SAY ALLTRIM(STYLE.PATTERN)
    *B603348,1 ABD [ End ]
    
    ROW = ROW + 1
    *B603348,1 ABD Remark The next Lines & move Them to the left 2 character
    *B603348,1     To Indicate the Printing .[ Begin ]
    *@ ROW,04 SAY 'PREPK(' + STYLE.PREPAK + '):'    
    @ ROW,02 SAY 'PREPK(' + STYLE.PREPAK + '):'
    *B603348,1 ABD [ End ]

    
    IF !EMPTY(STYLE.PREPAK)
      = SEEK('P' + STYLE.SCALE + STYLE.PREPAK ,'SCALE')
      STORE 0 TO XPREPAK
      X   = 1

      *B603348,1 ABD Remark The next Lines & move Them to the left 2 character
      *B603348,1     To Indicate the Printing .[ Begin ]
      *COL = 16
      COL = 14
      *B603348,1 ABD [ End ]

      
      DO WHILE X<=8
        Y=STR(X,1)
        @ ROW,COL SAY SCALE.PP&Y PICTURE '@Z 999'
        X = X+1
        COL = COL + 6
      ENDDO
    ENDIF

    * LINE 8
    *B603348,1 ABD Remark The next Lines & move Them to the left 2 character
    *B603348,1     To Indicate the Printing .[ Begin ]
    *COL=14
    COL=12
    *B603348,1 ABD [ End ]

    ROW=ROW+1
    IF ROW <= 53
      *B603348,1 ABD Remark The next Lines & move Them to the left 2 character
      *B603348,1     To Indicate the Printing .[ Begin ]
      *@ ROW,04 SAY 'COLORS  :'
      @ ROW,02 SAY 'COLORS  :'
      *B603348,1 ABD [ End ] 

    ENDIF
    
    DO WHILE ALLTRIM(STYLE.CSTYMAJOR) == XSTYLE
      IF EOF()
        EXIT
      ENDIF
      IF ROW > 53 &&>>> 
        PAGENO = PAGENO+1
        DO RPT_HDR WITH XREPORT,'',R_WIDTH
        *B603348,1 ABD Remark The next Lines & Change The headre to Indicate the Printing .[ Begin ]
        *@ 05,00 SAY '                      ITEM  MFG                UOM     UNIT     UNIT       TOTAL'
        *@ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION   USE QUANTITY     COST        COST'
        @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL'
        @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST'
        *B603348,1 ABD [ End ]
        
        @ 07,00 SAY REPLICATE('=',80)
        ROW=08
        @ ROW,04 SAY 'COLORS  :'
      ENDIF
      
      @ ROW,COL SAY IIF(lcFree_Clr = 'C',; 
              SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******')
      
      *B040210,1  TMI [Start] consider the extended case where the style distribute over many lines
      *SKIP IN STYLE
      lcStyClr = SUBSTR(STYLE.STYLE,1,lnNonMajSt+lnColorLen-1)
      SELECT STYLE
      *- Get the first line of the next colour
      LOCATE REST WHILE STYLE = lcStyClr FOR STYLE <> lcStyClr
          *--  this line is equavelant to the following scan loop
          *--      SCAN REST WHILE STYLE = lcStyClr
          *--      ENDSCAN      

      SELECT (WORKFILE)
      *B040210,1  TMI [End  ] 
      
      ROW = IIF( COL >= 60 , ROW + 1  , ROW )
      COL = IIF( COL <= 60 , COL + 7 , 14 )
      IF STYLE.STYLE<> XSTYLE .AND. COL = 14
        ROW = ROW - 1
      ENDIF
    ENDDO

    ROW=ROW+1
    
    *B602887,1 WMA [Begin]
    IF llMulCurr
      lnLenVal = 96
    ELSE
      lnLenVal = 80
    ENDIF
    *B602887,1 WMA [End]
    
    *B040210,1  TMI [Start] Calculate # of rows for the next section, if it spanes to the next page then start printing it in 
    *                       a new page
    =lfCalcRows()
    *B040210,1  TMI [End  ] 

    DO CASE
      CASE Typ = "1"
        XHDR='< '+lcSlbl1+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]
      CASE Typ = "2"
        XHDR='< '+lcSlbl2+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]
      CASE Typ = "3"
        XHDR='< '+lcSlbl3+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]
      CASE Typ = "4"
        XHDR='< '+lcSlbl4+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]

      CASE Typ = "5"
        XHDR='< '+lcSlbl5+' >'
        *B602887,1 WMA [Begin]
        *lnLen=INT(((80-LEN(XHDR))/2))
        lnLen=INT(((lnLenVal-LEN(XHDR))/2))
        *B602887,1 WMA [End]
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        *B602887,1 WMA [Begin]
        *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
        *B602887,1 WMA [End]
    ENDCASE

    ROW=ROW+1
  ENDIF         && END OF NEW STYLE
  
  *B602887,1 WMA if use multi currency get currency code [Begin]
  IF llMulCurr
    = SEEK(cSeekStyle,'STYLE')
    DO CASE
      CASE cCatgTyp = "P"
        lcCurCod = Style.CPriceCur
      CASE cCatgTyp $ "FTS"
        lcCurCod = gcBaseCurr
      OTHERWISE     
       lcCurCod = Style.CDutyCur      
    ENDCASE 
  ENDIF
  *B602887,1 WMA [End]

  *B603348,1  ABD Remark The next Line & Move ITEM  To The  Left. [ Begin ]
  *@ ROW,06 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
   @ ROW,01 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
  *B603348,1  ABD [ End ]
  
  IF SCLR = '******'
  *B603348,1  ABD Remark The next Line & Move COLOR To The  Left. [ Begin ]
    *@ ROW,22 SAY IIF(ICLR # '******',ICLR,;
                 IIF(lcFree_Clr = 'C',;
                 SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******'))
    @ ROW,17 SAY IIF(ICLR # '******',ICLR,;
                 IIF(lcFree_Clr = 'C',;
                 SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******'))
                 
  *B603348,1  ABD [ End ]
  ENDIF
  IF SUBSTR(cItmMask,lnNonMajSt,lnColorLen) = '******'
    *B603348,1  ABD Remark The next Line & Move ICLR To The  Left. [ Begin ]
    *@ ROW,22 SAY ICLR
    @ ROW,17 SAY ICLR
    *B603348,1  ABD [ End ]
  ENDIF
  
  IF cCatgTyp = "M"
    *B603348,1  ABD Remark The next Line & Move MFGCODE To The  Left. [ Begin ]  
    *@ ROW,28 SAY PADR(MFGCODE,6)

    *B606065,1 KHM 06/05/2002 (Begin) Commenting this line because the desc field has the
    *B606065,1                MFGcode.
    *@ ROW,24 SAY PADR(MFGCODE,6)
    *B606065,1 KHM 06/05/2002 (End)
    
    *B603348,1 ABD [ End ]

  ENDIF
  
  *B603348,1 ABD Remark The next Lines & move Them to the left 2 character
  *B603348,1     To Indicate the Printing .[ Begin ]
  *@ ROW,35 SAY SUBSTR(DESC,1,11)
  *@ ROW,47 SAY UOM
  *@ ROW,52 SAY nBomTOTQTY PICTURE '999.999'
  
  *B606065,1 KHM 06/05/2002 (Begin) Changing the column position and the Num. of printed characters
  *@ ROW,33 SAY SUBSTR(DESC,1,11)
  @ ROW,24 SAY SUBSTR(DESC,1,17)
  *B606065,1 KHM 06/05/2002 (End)
  
  @ ROW,45 SAY UOM
  @ ROW,49 SAY nBomTOTQTY PICTURE '999.999'
  *B603348,1 ABD [ End ]

  *B802157,1 Reham On 05/11/1999   *** Begin ***
  *B802157,1 Call local function to calculate cost.
  Z = Typ
  =lfCostBySz()
  *B802157,1 Reham On 05/11/1999   *** End   ***

  IF qCostPrv             && COSTING ACCESS
    *B802313,1 Reham on 06/06/1999   *** Begin ***
    *B802313,1 Print the variables instead of the fields.
    *@ ROW,61 SAY UNTCOST PICTURE '999.999'
    *@ ROW,71 SAY TOTCOST PICTURE '999999.99'
    
    *B603348,1 ABD Remark The next Lines & Change The Picture Instead of the fields &
    *B603348,1      move Them to the left 4 characterTo Indicate the Printing .[ Begin ]
    *@ ROW,61 SAY lnUntCost PICTURE '999.999'
    *@ ROW,71 SAY lnTotCost PICTURE '999999.99'
    @ ROW,57 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,69 SAY lnTotCost PICTURE '99999999.99'
    *B602887,1 WMA if use multi currency print cost with Equivalent [Begin]
    IF llMulCurr
	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,80 SAY IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
	  lnECost = lfAmntDisp(lnTotCost,gdSysDate,lcCurCod)
      @ ROW,84 SAY lnECost PICTURE '999999999.99'

      *B126622,1 NNA 04/06/2005 (Begin) Comment this line to calculate it in another place because
      *B126622,1 NNA            in this line it Accumulate the cost
      *lnETotCost = lnETotCost + lnECost
      *B126622,1 NNA (End)
      lnLinTot = lnLinTot + lnECost
    ENDIF
    *B602887,1 WMA [End]

    
    *B603348,1 ABD [ End ]

    *B802313,1 Reham on 06/06/1999   *** End   ***
  ENDIF
  ROW=ROW+1

  Z = Typ
  
  *B802157,1 Reham On 05/11/1999   *** Begin ***
  *B802157,1 Already calculated in the local function to calculate cost.
  *XTOT&z=XTOT&z+TOTCOST
  *B802157,1 Reham On 05/11/1999   *** End   ***
  
  * 
  *---- NOW DISPLAY THE FABRIC AND STYLE COLOR ASSIGNEMENTS ----*
  *IF SCLR<>'******' .AND. cCatgTyp $ "FS"
  *COL = 15
  *@ ROW,06 SAY 'COLORS  :'

  *MAB NOW LIKE LAST ONE
  *B802028,1 (Begin) Remark the following and just say the style color and
  *B802028,1         current item color.
  *DO WHILE XPRV_TYP=TYP .AND. cItmMajor = XSTYLE
  *IF EOF()
  *EXIT
  *ENDIF
  *IF ROW > 53
  *PAGENO = PAGENO+1
  *DO RPT_HDR WITH XREPORT,'',R_WIDTH
  *@ 05,00 SAY '                      ITEM  MFG                UOM     UNIT     UNIT       TOTAL'
  *@ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION   USE QUANTITY     COST        COST'
  *@ 07,00 SAY REPLICATE('=',80)
  *ROW=08
  *@ ROW,04 SAY 'COLORS  :'
  *ENDIF
  *@ ROW,COL SAY SCLR
  *@ ROW,COL+6 SAY '-'+ ICLR
  *SKIP
  *ROW = IIF( COL >= 51 , ROW + 1  , ROW )
  *COL = IIF( COL <= 51 , COL + 14 , 15 )
  *IF (XPRV_TYP<>TYP .OR.  cItmMajor <> XSTYLE) .AND. COL = 15
  *ROW = ROW - 1
  *ENDIF
  *ENDDO
  IF SUBSTR(cItmMask,lnNonMajSt,lnColorLen) <>'******' .AND. cCatgTyp $ "FS"
    COL = 15
    @ ROW,06 SAY 'COLORS  :'
    @ ROW,COL SAY SCLR
    @ ROW,COL+6 SAY '-'+ ICLR
    *B802028,1 (End)
    ROW=ROW+1
  ENDIF
  IF .NOT. EOF() .AND. (XPRV_TYP=TYP)
    
    *B126622,1 NNA 04/06/2005 (BEGIN) Hold Current Style.
    lcCurStyle = &WORKFILE..CITMMAJOR
    *B126622,1 NNA (END)
    
    SKIP IN (WORKFILE)
  ENDIF
  IF XPRV_TYP<>TYP
    IF qCostPrv           && COSTING ACCES
      DO CASE
      *  CASE XPRV_TYP='1'
      *    @ ROW,027 SAY ' '+lcSlbl1+' TOTAL :=>'
      *    @ ROW,070 SAY XTOT1 PICTURE '9999999.99'
      *  CASE XPRV_TYP='2'
      *    @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
      *    @ ROW,070 SAY XTOT2 PICTURE '9999999.99'
      *  CASE XPRV_TYP='3'
      *    @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
      *    @ ROW,070 SAY XTOT3 PICTURE '9999999.99'
      *  CASE XPRV_TYP='4'
      *    @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
      *    @ ROW,070 SAY XTOT4 PICTURE '9999999.99'
      *  CASE xPrv_Typ = "5"
      *    @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
      *    @ ROW,070 SAY XTOT5 PICTURE '9999999.99'
        CASE XPRV_TYP='1'
          @ ROW,027 SAY ' '+lcSlbl1+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT1 PICTURE '9999999999.99'
          IF llMulCurr 

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            lnLinTot = 0
            SUM AVGCOST TO lnLinTot FOR CSTYLE = lcCurStyle AND TYPE = '1'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)
            
            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0 
          ELSE

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            XTOT1 = 0
            SUM AVGCOST TO XTOT1 FOR CSTYLE = lcCurStyle AND TYPE = '1'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,067 SAY XTOT1 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
        CASE XPRV_TYP='2'
          @ ROW,027 SAY ' '+lcSlbl2+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
          IF llMulCurr 

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            lnLinTot = 0
            SUM AVGCOST TO lnLinTot FOR CSTYLE = lcCurStyle AND TYPE = '2'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0          
          ELSE

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            XTOT2 = 0
            SUM AVGCOST TO XTOT2 FOR CSTYLE = lcCurStyle AND TYPE = '2'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
        CASE XPRV_TYP='3'
          @ ROW,027 SAY ' '+lcSlbl3+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
          IF llMulCurr 

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            lnLinTot = 0
            SUM AVGCOST TO lnLinTot FOR CSTYLE = lcCurStyle AND TYPE = '3'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0                   
          ELSE

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            XTOT3 = 0
            SUM AVGCOST TO XTOT3 FOR CSTYLE = lcCurStyle AND TYPE = '3'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
        CASE XPRV_TYP='4'
          @ ROW,027 SAY ' '+lcSlbl4+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
          IF llMulCurr 

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            lnLinTot = 0
            SUM AVGCOST TO lnLinTot FOR CSTYLE = lcCurStyle AND TYPE = '4'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0                            
          ELSE

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            XTOT4 = 0
            SUM AVGCOST TO XTOT4 FOR CSTYLE = lcCurStyle AND TYPE = '4'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
        CASE xPrv_Typ = "5"
          @ ROW,027 SAY ' '+lcSlbl5+' TOTAL :=>'
          *B602887,1 WMA if use multi currency print total cost with Equivalent [Begin]
          *@ ROW,067 SAY XTOT5 PICTURE '9999999999.99'
          IF llMulCurr 

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            lnLinTot = 0
            SUM AVGCOST TO lnLinTot FOR CSTYLE = lcCurStyle AND TYPE = '5'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,083 SAY lnLinTot PICTURE '9999999999.99'
            lnLinTot = 0                   
          ELSE

            *B126622,1 NNA 04/06/2005 (Begin) IF Multi Currency get Total of the Cost type(1) from lcAvgCost
            SELECT(lcAvgCost)
            XTOT5 = 0
            SUM AVGCOST TO XTOT5 FOR CSTYLE = lcCurStyle AND TYPE = '5'
            SELECT(WORKFILE)
            *B126622,1 NNA (End)

            @ ROW,067 SAY XTOT5 PICTURE '9999999999.99'
          ENDIF
          *B602887,1 WMA [End]
      ENDCASE
    ENDIF

    XPRV_TYP=TYP
    ROW=ROW+1

    IF &BREAK=HBREAK

      *B602887,1 WMA [Begin]
      IF llMulCurr
        lnLenVal = 96
      ELSE
        lnLenVal = 80
      ENDIF
      *B602887,1 WMA [End]

      *B040210,1  TMI [Start] if this section spans to the next page then print it wholly in the next page
      =lfCalcRows()
      *B040210,1  TMI [End  ] 

      DO CASE
        CASE Typ = "1"
          XHDR='< '+lcSlbl1+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]
          
        CASE Typ = "2"
          XHDR='< '+lcSlbl2+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]

        CASE Typ = "3"
          XHDR='< '+lcSlbl3+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]
          
        CASE Typ = "4"
          XHDR='< '+lcSlbl4+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]

        CASE Typ = "5"
          XHDR='< '+lcSlbl5+' >'
          *B602887,1 WMA [Begin]
          *lnLen=INT(((80-LEN(XHDR))/2))
          lnLen=INT(((lnLenVal-LEN(XHDR))/2))
          *B602887,1 WMA [End]
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          *B602887,1 WMA [Begin]
          *@ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(lnLenVal-2-LEN(XHDR)-lnLen))
          *B602887,1 WMA [End]
      ENDCASE

      ROW=ROW+1
    ENDIF
  ENDIF
ENDDO
*-- end of lpCPrn.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfAdrShift
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
IF EMPTY(lcSlbl1)

  IF lcFree_Clr # 'C'
    laOGObjCnt[1] = .F.    && Disble if there is no color segment.
    = lfOGShowGet('LCRPSTYCLR')  && Called to show object get.
  ENDIF

  XAVG_COST = (gfGetMemVar('M_COST_MET') = 'A')
  *MAN
  *qCostPrv  = gfUserPriv('MF','MFCSREP')
  qCostPrv = gfUserPriv('IC','ICSTYLE','COSTING')

  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO CODES IN CODES        && To use it to validate STYLE   # in option grid.
  SET ORDER TO SCALE IN SCALE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO BOM IN BOM            && To use it to validate STYLE   # in option grid.

  IF 'MA' $ gcComp_Mdl
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate STYLE   # in option grid.
  ENDIF  

  DIMENSION laTempStru[1,4]
  SELECT BOM
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 2, 4]

  *-- cSeekStyle     :  field save style to seek it in style file.
  laTempStru[ALEN(laTempStru,1) - 1 ,1] = 'cSeekStyle'
  laTempStru[ALEN(laTempStru,1) - 1 ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) - 1 ,3] = 19
  laTempStru[ALEN(laTempStru,1) - 1 ,4] = 0

  *-- SCLR     :  field save style color
  laTempStru[ALEN(laTempStru,1)  ,1] = 'SCLR'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  *B802451,1 Reham On 08/08/1999   *** Begin ***
  *B802451,1 Increase the field width to equal style width.
  *laTempStru[ALEN(laTempStru,1)  ,3] = 6
  laTempStru[ALEN(laTempStru,1)  ,3] = 19
  *B802451,1 Reham On 08/08/1999   *** End   ***
  laTempStru[ALEN(laTempStru,1)  ,4] = 0
  *-- Create temporary file that holding order line data. [end]

  = lfRecreate()

  lcStyleTyp  = "I"
  FOR lnCount = 1 TO 5
    lcCount  = STR(lnCount,1)
    *lcSlbl&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'COST'+lcCount)
    lcSlbl&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'SLBL'+lcCount)
  ENDFOR 

ENDIF  
*-- end of lfwRepWhen.

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
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
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
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 

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
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
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
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
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
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR Fabric = Fabric.Fabric AND ;
             !MAKE AND lDetCost AND STATUS = 'A'
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- !   No active imported styles use detail costing in fabric group XXX .
      *--                  <Ok>
      = gfModalGen("INM34122B36000","Dialog",ALLTRIM(Fabric.Fabric))
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
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
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*-- MAB Create Temporary cursor.
*-- E300967 MAB 8/18/1998
FUNCTION lfRecreate
CREATE CURSOR (WORKFILE) FROM ARRAY laTempStru

*B126622,1 NNA 04/06/2005 (BEGIN) Create new temp file to hold the Avg cost for Style/fabric
CREATE CURSOR (lcAvgCost)(Cstyle C(19),TYPE C(1),FABRIC C(7),AVGCOST N(10,2))
INDEX ON (CSTYLE + TYPE + FABRIC) TAG (lcAvgCost) OF (lcAvgCost)
*B126622,1 NNA (END)
*-- end of lfRecreate.

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
FUNCTION lfClearRep
llClearFn = .T.
*-- Close temp. opended files, if it used.

IF USED(WORKFILE)
 USE IN (WORKFILE)
ENDIF
*-- end of lfClearRep.

*-- MAB rise flag tell us that user change report format.
*-- E300967 MAB 8/18/1998
FUNCTION lfChanged
llChStyClr = .T.
*-- END OF lfChanged.

*!*************************************************************
*! Name      : lfFillForm
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Fill report format arrays called from dummy variable in rep. gen.
*!*************************************************************
*! Called from : Dummy variable in Syrepuvr.dbf file
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillForm()
*!*************************************************************
FUNCTION lfFillForm
IF lcFree_Clr = 'C'
  DIMENSION laFormDesc[2,1] , laFormVal[2,1]
  laFormDesc[1,1] = lcStyMajor + ' / ' + lcNonMajTl
  laFormDesc[2,1] = lcStyMajor + ' only...'

  laFormVal[1,1]  = 'C'
  laFormVal[2,1]  = 'S'
ELSE
  DIMENSION laFormDesc[1,1] , laFormVal[1,1]
  laFormDesc[1,1] = lcStyMajor + ' only...'

  laFormVal[1,1]  = 'S'
ENDIF
*-- end of lfFillForm.

*-- MAB Set default type for form format. 
*-- E300967 MAB 8/18/1998
FUNCTION lfFormDefa
RETURN IIF(lcFree_Clr = 'C','C','S')
*-- end of lfFormDefa.

*!*************************************************************
*! Name      : lfCostBySz
*! Developer : Reham Al-Allamy
*! Date      : 05/10/1999
*! Purpose   : Calculate cost for each cost element y size.
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCostBySz()
*!*************************************************************
*
FUNCTION lfCostBySz
PRIVATE lcCurAlias , lnStyRec , lnSclRec
 
*B802157,1 Save current alias...
lcCurAlias = ALIAS()
*B802157,1 Save the record pointer in the style file.
lnStyRec   = RECNO("STYLE")
*B802157,1 Save the record pointer in the scale file.
lnSclRec   = RECNO("SCALE")

*B802157,1 Variable hold the scale count.
lnSCnt   = 8
lnBomSzs = 8

SELECT (WORKFILE)
*B802157,1 If the sizes field have sizes.
IF !EMPTY(MSizes)
  *B802157,1 Seek with the current style from BOM file in style file.
  IF SEEK(ALLTRIM(cItmMajor) , 'STYLE') 
    SELECT STYLE
    LOCATE REST FOR Style.cStyMajor = &WORKFILE..cItmMajor .AND. ;
                    LIKE(STRTRAN(&WORKFILE..cItmMask , "*" , "?") , STYLE.Style)
    *B802157,1 If found the style in the style file.
    IF FOUND()
      *B802157,1 Seek with the style scale in the scale file.
      IF SEEK('S'+STYLE.Scale,'SCALE')
        *B802157,1 Get the scale count for the current style.
        lnSCnt=SCALE.Cnt
      ENDIF
    ENDIF
  ENDIF
  
  *B802157,1 If the system extended size scale & current category type is style
  *B802157,1 and the cross reference between the parent style & its child has sizes.
  SELECT (WORKFILE)
  IF llMScale AND cCatgTyp='S' AND !EMPTY(MSzCrosref)
    *B802157,1 Variable hold the selected sizes that the current cost applied to it.
    lnBomSzs=0
    *B802157,1 Loop in the lines of the memo field that hold the cross reference
    *B802157,1 sizes between the parent style & its child.
    FOR lnMln=1 TO MEMLINES(MSzCrosref)
      *B802157,1 If the style scale is the same as the current scale in the current line 
      *B802157,1 of the memo field that hold the cross reference sizes between the 
      *B802157,1 parent style & its child.
      IF STYLE.Scale=SUBSTR( MLINE(MSzCrosref,lnMln) ,1 , 3)
        *B802157,1 Calculate the selected sizes from this scale.
        lnBomSzs=lnBomSzs+1
      ENDIF
    ENDFOR
  ELSE
    *B802157,1 If the system is not extended size scale & the current category 
    *B802157,1 type is not style, use the sizes memo field.
    
    *B802157,1 Default the selected sizes to the scale count.
    lnBomSzs=lnSCnt
    *B802157,1 Loop in the lines of the sizes memo field.
    FOR lnMln=1 TO MEMLINES(MSizes)
      *B802157,1 If the style scale exist in the current line of the sizes memo field.
      IF OCCURS(STYLE.Scale,MLINE(MSizes,lnMln)) <> 0
        *B802157,1 Get the occurence of this scale, means the sizes selected from 
        *B802157,1 this sizes memo field.
        lnBomSzs=OCCURS(',',MLINE(MSizes,lnMln))+1
        EXIT
      ENDIF
    ENDFOR
  ENDIF
*B131001,1 MHM 03/14/2006  Fix bug of wrong cost calculation in style cost sheet.[Start]
ELSE
  lnCstBySiz = lnCstBySiz + TotCost
*B131001,1 MHM [End]
  
ENDIF 

*B802157,1 Calculate cost by size.
*B802157,1 Multiple tot cost by the selected sizes only & divide it by total sizes of the scale.

*B126622,1 NNA 04/06/2005 (Begin) Comment this line to calculate it in another place because
*B126622,1 NNA            in this line it Accumulate the cost
*lnCstBySiz = lnCstBySiz + ROUND((lnBomSzs/lnSCnt)*TotCost,2)
*B126622,1 NNA (END)

*B802157,1 Calculate cost by size.
XTOT&z     = XTOT&z + ROUND((lnBomSzs/lnSCnt)*TotCost,2)

*B802313,1 Reham on 06/06/1999   *** Begin ***
*REPLACE UntCost WITH UntCost * (lnBomSzs/lnSCnt) ;
*        TotCost WITH ROUND(TotCost * (lnBomSzs/lnSCnt),2)
*B802313,1 Variable hold the current unit cost.
lnUntCost = UntCost * (lnBomSzs/lnSCnt)
*B802313,1 Variable hold the current total cost.
lnTotCost = ROUND(TotCost * (lnBomSzs/lnSCnt),2)
*B802313,1 Reham on 06/06/1999   *** End   ***

*B802157,1 Restore the record pointer in the style file.
IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE")
  GOTO lnStyRec IN STYLE
ENDIF
*B802157,1 Restore the record pointer in the scale file.
IF lnSclRec > 0 .AND. lnSclRec <= RECCOUNT("SCALE")
  GOTO lnSclRec IN SCALE
ENDIF

*B802157,1 Restore the original alias...
SELECT (lcCurAlias)


*B602887,1 WMA [Begin]
*!********************************************************************
*! Name      : lfAmntDisp
*! Developer : Mohamed Hassan
*! Date      : 12/26/95
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcCurrCode   && Currency Code
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
FUNCTION lfAmntDisp
PARAMETER lnAmount,ldExRateDt,lcCurrCode

PRIVATE lnAmount,ldExRateDt,lcExSin1,lcExSin2,lnSavAlias
lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)

lnExRate   = 0
lnUnit     = 0
    
IF lcCurrCode = gcBaseCurr
  lnExRate = 1
  lnUnit   = 1
ELSE
  lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
  **lnExRate   = gfChkRate('lnUnit',"USD",ldExRateDt,.F.)
ENDIF

*B601526,1 Add this lines [Begin]
lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
*B601526,1 Add this lines [End]

lcExSin2   = ' '
lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

*E301214,1 Restore alias [Begin]
SELECT (lnSavAlias)
*E301214,1 Restore alias [End  ]

RETURN lnAmount
*B602887,1 WMA [End]



*B602887,1 WMA [Begin]
*!*************************************************************
*! Name      : lfPrHeader
*! Developer : Wael Mohammed Anwar
*! Date      : 05/21/2000
*! Purpose   : Report header with 93 char 
*!*************************************************************
*! Called from : lpSCPrn
*!*************************************************************
*! Passed Parameters : HPAGENO
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :  DO lfPrHeader WITH PAGENO
*!*************************************************************
*
PROCEDURE lfPrHeader
PARAMETERS HPAGENO  
PRIVATE lnCoNameCol

* MFCSREP                                 Omar Ramadan                               05/20/00 ~
* 12:15:58                           Style Cost Sheet Report                         PAGE#    1
*                                        
* *********************************************************************************************

lnCoNameCol = 47 - (LEN(TRIM(gccom_name)) / 2)
@ 01,00 SAY 'MFCSREP'
@ 01,lnCoNameCol SAY gccom_name
@ 01,86 SAY gdsysdate
@ 02,00 SAY TIME()
@ 02,36 SAY 'Style Cost Sheet Report'
@ 02,86 SAY 'PAGE#' 
@ 02,92 SAY HPAGENO PICTURE '9999'
@ 04,00 SAY REPLICATE('*',96)
*-- end of lfPrHeader.

*B602887,1 WMA [End]
*!*************************************************************
*! Name      : lfGetAvg
*! Developer : Nader Nabil (NNA)
*! Date      : 04/06/2005
*! Purpose   : Get the fabric's Avg. Cost
*!*************************************************************
*! Called from : Report Code
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetAvg()
*!*************************************************************
*!B126622,1
FUNCTION lfGetAvg
PRIVATE lnOldAlias,lnRecNo,lcStyle,lcType,lcFabric,lnCount,lnCost,lnAvgCost
STORE 0  TO lnOldAlias , lnRecNo , lnCount , lnCost , lnAvgCost
STORE '' TO lcStyle , lcType , lcFabric
lnOldAlias = SELECT(0)
lnRecNo = RECNO(WORKFILE)
IF SEEK(&WORKFILE..CITMMAJOR + &WORKFILE..TYP + SUBSTR(&WORKFILE..ITEM,1,7),lcAvgCost) ;
   OR EOF(WORKFILE)
  RETURN
ELSE
  SELECT (WORKFILE)
  lcStyle  = &WORKFILE..CITMMAJOR
  lcType   = &WORKFILE..TYP 
  lcFabric = &WORKFILE..ITEM
  lnRecNo  = RECNO(WORKFILE)
  LOCATE
  =SEEK (lcStyle+lcType)
  SCAN REST WHILE  cItmMajor + Typ = lcStyle+lcType FOR ITEM = SUBSTR(lcFabric,1,7)
    
    *B131001,1 MHM 03/14/2006  Fix bug of wrong cost calculation in style cost sheet.[Start]
    lCcatgTyp = CcatgTyp
    *B131001,1 MHM [End]

    IF SUBSTR(CITMMASK,LNMAJORLEN+2,LNCOLORLEN)=PADR('*',LNCOLORLEN,'*') AND CcatgTyp $ 'FTS'
      lnCost = lnCost + TotCost
    ELSE
      lnCount = lnCount + 1
      lnAvgCost = lnAvgCost + TotCost
    ENDIF 
  ENDSCAN
  
  *B131001,1 MHM 03/14/2006  Fix bug of wrong cost calculation in style cost sheet.[Start]
  IF lCcatgTyp = 'M'
    lnCost = lnCost + lnAvgCost
  ELSE
    lnCost = lnCost + IIF(lnAvgCost>0,(lnAvgCost/lnCount),lnAvgCost)
  ENDIF
  *B131001,1 MHM [End]
  
    INSERT INTO (lcAvgCost)(Cstyle,TYPE,FABRIC,AVGCOST) VALUES (lcStyle,lcType,SUBSTR(lcFabric,1,7),lnCost)
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT(WORKFILE))
  GOTO lnRecNo IN (WORKFILE)
ENDIF
SELECT(lnOldAlias)
*-->End Of Function lfGetAvg.

*:**************************************************************************
*:* Name        : lfCalcRows
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2006
*:* Purpose     : Calculate # of rows for the next section, if it spanes to the next page then start printing it in 
*:*                         a new page
*:***************************************************************************
FUNCTION lfCalcRows

PRIVATE lnSlct,lnRecno,lcKey,lnCount,lnK,llRet,laMxTyp,lcStars
IF &WORKFILE..TYP = ' '
  RETURN .F.
ENDIF

*- Get max type for this style
DIMENSION laMxTyp[1]
laMxTyp = ' '
SELECT MAX(TYP) FROM &WORKFILE WHERE CITMMAJOR = PADR(XSTYLE,19) INTO ARRAY laMxTyp

lnSlct = SELECT()
lnRecno = RECNO(WORKFILE)
llRet = .F.

SELECT &WORKFILE
lcKey = CITMMAJOR+TYP
=SEEK(lcKey,WORKFILE)

lcStars = REPLICATE('*',lnColorLen)
lnCount = IIF(TYP < laMxTyp[1] , 2 , 5 )

SCAN REST WHILE CITMMAJOR+TYP+CSEEKSTYLE+CITMMASK+MFGCODE+ITEM+ICLR = lcKey
  lnCount = lnCount + IIF( SUBSTR(CITMMASK,lnNonMajSt,lnColorLen) = lcStars , 1 , 2 )
ENDSCAN

IF ROW+lnCount >= 55
  llRet = .T.
  DO WHILE ROW < 55
    ROW = ROW + 1
    @ROW,1 SAY ''
  ENDDO  
  
  *- go to new page and add a new header
  PAGENO = PAGENO+1
  IF llMulCurr
    DO lfPrHeader WITH PAGENO
  ELSE
    DO RPT_HDR WITH XREPORT,'',R_WIDTH
  ENDIF  

  IF llMulCurr
    @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL           TOTAL'
    @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST           EQUV.'  
  ELSE
    @ 05,00 SAY '                      ITEM  MFG              UOM    UNIT        UNIT       TOTAL'
    @ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION USE QUANTIT        COST        COST'
  ENDIF
  
  IF llMulCurr
    @ 07,00 SAY REPLICATE('=',96)
  ELSE
    @ 07,00 SAY REPLICATE('=',80)
  ENDIF  
  @ 8,000 SAY ALLTRIM(lcStyMajor) + '->' + XSTYLE
  ROW = 9
ENDIF  

IF BETWEEN(lnRecno,1,RECCOUNT(WORKFILE))
  SELECT &WORKFILE
  GO (lnRecno)
ENDIF

SELECT (lnSlct)
RETURN llRet
*-- end of lfCalcRows.