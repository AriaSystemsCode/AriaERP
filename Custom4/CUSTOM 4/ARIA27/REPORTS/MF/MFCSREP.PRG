*:***************************************************************************
*: Program file  : MFCSREP
*: Program desc. : Style Cost Sheet Report
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Manufacturing (MF )
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
*: Example : DO MFCSREP
*:***************************************************************************
*: This Report Program is due to E300967,1 ...
*E301170,1 MAB  03/15/1999 Use variables detect OG filter changes.
*B802028,1 ADEL 03/25/1999 1)Fix the bug of printing the first item (fabric or trim..) only
*B802028,1                  and repeating the colors when printing by Style format and
*B802028,1                  cause a difference between the total cost sheet when 
*B802028,1                  printing by STYLE/COLOR format and By Style format. 
*B802028,1                  2) Show one line for the fabric in case of fabric color
*B802028,1                    color as style color.
*B802157,1 Reham On 05/09/1999
*B802157,1 Print the total cost by size.
*B802313,1 Reham On 06/06/1999
*B802313,1 Fix the error of acumlating the unit & total cost 
*B802313,1 if press preview several times.
*B802451,1 Reham On 08/08/1999
*B802451,1 1- Prevent printing duplicate cost items.
*B802451,1 2- Change the looping condition to be by Non-Major instead of colors.
*B603348,1  ABD 02/07/2000 Increase The Picture Of The purchase price per item [UnTCost]
*B603348,1                 To 9999999.999 & the Picture Of The TotCost [ ToTCost ] To 999999999999.99 
*B603348,1                 Instead of the fields.
*B603713,10 HBG 11/22/2000 Increase The Picture Of The pricea , priceb , pricec
*B606065,1 KHM 06/05/2002 Fix the problem of printing the MfgCode twice
*B124433,1 BWA 09/23/2004 Fix the bug of not printing the hold styles.
*B607968,1 TMI [Start] get fabric cost from bom.dbf , not from fabric.dbf file 
*****************************************************************************
*-- if first time you (preview or run ) or user change critria.

*B802157,1 Reham On 05/09/1999    *** Begin ***
*B802157,1 Get the extended size scale system variable.
llMScale = gfGetMemVar('M_USEEXSSC')
*B802157,1 Reham On 05/09/1999    *** End   ***

*B802451,1 Reham On 08/08/1999    *** Begin ***
*B802451,1 Get the Major length.
lnMajorLen = LEN(gfItemMask("PM"))
*B802451,1 Reham On 08/08/1999    *** End   ***

*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [Begin
*IF llClearFn OR !(lcRpExp == lcLastExpr)   OR ;
*   (llChStyle  OR llChFabric OR llChStyClr)
IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [End

  lcRpExp = [STYLE = '' AND SEEK(CSTYMAJOR,'BOM') AND ] + lcRpExp 
  llClearFn  = .F.  && do clear function flag
  llChStyClr = .F.  && change color/style or color selection flag
  llChFabric = .F.  && change fabric filter flag.
  llChStyle  = .F.  && change style filter flag.
  
  IF RECCOUNT(WORKFILE) > 0
    USE IN (WORKFILE)
    = lfRecreate()
  ENDIF

  IF lcRpStyClr='C'
    DO lpStyClr           && PRINT BY COLOR
  ELSE
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
IF RECCOUNT(WORKFILE) = 0
  SELECT (WORKFILE)
  *B802451,1 Reham On 08/11/1999   *** Begin ***
  *B802451,1 Change the index to be like the current bom index.
  *INDEX ON cItmMajor + SCLR + TYP + IIF(CCATGTYP # "M",ITEM,MFGCODE) +;
  *         ICLR + STR(RECNO(),7) TAG &WORKFILE
  INDEX ON cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
  *B802451,1 Reham On 08/11/1999   *** End   ***
ENDIF

SELECT STYLE
*-- Scan all selected styles.
SCAN FOR &lcRpExp
  XSTYLE  = cStyMajor
  
  *B802451,1 Reham On 08/08/1999   *** Begin ***
  *B802451,1 Prevent printing duplicate cost items.
  *XSTYCLR = IIF(lcFree_Clr = 'C',SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******')
  XSTYCLR = SUBSTR(STYLE.STYLE,lnMajorLen+1)
  *B802451,1 Reham On 08/08/1999   *** Begin ***
  
  SELECT BOM
  = SEEK(XSTYLE)

  *B802451,1 Reham On 08/12/1999   *** Begin ***
  *B802451,1 Scan for any record similar to the current style.
  *-- scan style in BOM file
  *SCAN REST WHILE (cItmMajor = XSTYLE)
  SCAN REST WHILE (cItmMajor = XSTYLE) FOR LIKE(STRTRAN(cItmMask,'*','?') , Style.Style)
  *B802451,1 Reham On 08/12/1999   *** End   ***
  
    XUNTCOST=0

    DO CASE
      CASE cCatgTyp = "M"
        SCATTER MEMVAR MEMO
        m.SClr       = XSTYCLR
        m.cSeekStyle = STYLE.STYLE
        
        *B802451,1 Reham On 08/08/1999   *** Begin ***
        *B802451,1 Prevent printing duplicate cost items.
        *m.ICLR       = IIF(m.ICLR = '******', m.SClr, m.ICLR)
        *B802451,1 Reham On 08/08/1999   *** End   ***
        
        INSERT INTO (WORKFILE) FROM MEMVAR
      OTHERWISE
        
        IF LIKE(STRTRAN(citmmask,'*','?'),STYLE.STYLE)
          SCATTER MEMVAR MEMO
          DO CASE
            *B802451,1 Reham On 08/08/1999   *** Begin ***
            *CASE cCatgTyp = "F" 
            CASE cCatgTyp = "F" .OR. (cCatgTyp = "T" .AND. Trim_Invt)
              *B802451,1 Prevent printing duplicate cost items.
              *XKEY= PADR(ITEM,7) + IIF(ICLR='******',XSTYCLR,ICLR)
              XKEY= PADR(ITEM,7) + IIF("*" $ ICLR , IIF(lcFree_Clr = 'C',SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),ICLR) , ICLR)
              *B802451,1 Reham On 08/08/1999   *** End   ***
              
              *B607968,1 TMI [Start] get fabric cost from bom.dbf , not from fabric.dbf file by the above scatter command
              *IF SEEK(XKEY,'FABRIC')
              *  XUNTCOST= FABRIC.COSTUSE
              *  XFABDESC= FABRIC.DESC
              *ELSE
              *  XUNTCOST=0
              *  XFABDESC=SPACE(0)
              *ENDIF
              *B607968,1 TMI [End  ] 

              m.SClr     = XSTYCLR
              m.DESC     = IIF(ICLR = '******',XFABDESC,m.DESC)
              
              *B802451,1 Reham On 08/08/1999   *** Begin ***
              *B802451,1 Prevent printing duplicate cost items.
              *m.ICLR     = IIF(ICLR = '******',XSTYCLR,ICLR)
              *B802451,1 Reham On 08/08/1999   *** End   ***
              
              *B607968,1 TMI [Start] get fabric cost from bom.dbf , not from fabric.dbf file by the above scatter command
              *m.UNTCOST  = XUNTCOST
              *B607968,1 TMI [End  ] 
              m.TOTCOST  = ROUND(m.UNTCOST * m.nBomTOTQTY,2)

            CASE cCatgTyp $ "TS"            
              m.SClr = XSTYCLR
              
              *B802451,1 Reham On 08/08/1999   *** Begin ***
              *B802451,1 Prevent printing duplicate cost items.
              *m.IClr = IIF(ICLR = '******',XSTYCLR,ICLR)
              *B802451,1 Reham On 08/08/1999   *** End   ***
          ENDCASE
          m.cSeekStyle = STYLE.STYLE
          
          *B802451,1 Reham On 08/08/1999   *** Begin ***
          *B802451,1 Prevent printing duplicate cost items.
          *m.ICLR       = IIF(m.ICLR = '******', m.SClr, m.ICLR) 
          *B802451,1 Reham On 08/08/1999   *** End   ***
          
          INSERT INTO (WORKFILE) FROM MEMVAR
        ENDIF
    ENDCASE
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

IF RECCOUNT(WORKFILE) = 0
  SELECT (WORKFILE)
  *B802451,1 Reham On 08/11/1999   *** Begin ***
  *B802451,1 Change the index to be like the current bom index.
  *INDEX ON cItmMajor + TYP + SCLR + IIF(CCATGTYP # "M",ITEM,MFGCODE) +;
  *         ICLR + STR(RECNO(),7) TAG &WORKFILE
  INDEX ON cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + Item + IClr TAG &WORKFILE
  *B802451,1 Reham On 08/11/1999   *** End   ***
ENDIF           

lcOldMajor = ''

SELECT STYLE
*-- Scan all selected styles.
SCAN FOR &lcRpExp

  SELECT BOM
  *-- scan style in BOM file
  SCAN FOR cItmMajor = STYLE.cStyMajor AND !lMaterial
    IF (lcOldMajor # STYLE.cStyMajor) OR (cCatgTyp # 'M' AND '******' $ cItmMask)
      SCATTER MEMVAR MEMO
      m.cSeekStyle = IIF('******' $ cItmMask , STYLE.STYLE , cItmMask)
      
      *B802451,1 Reham On 08/08/1999   *** Begin ***
      *B802451,1 Prevent printing duplicate cost items.
      *m.SClr       = IIF(lcFree_Clr = 'C',SUBSTR(m.cSeekStyle,lnNonMajSt,lnColorLen),'******') 
      m.SClr       = SUBSTR(m.cSeekStyle,lnMajorLen+1) 
      *B802451,1 Reham On 08/08/1999   *** End   ***
      
      *B802028,1 (Begin) Remark the following line.
      *m.ICLR       = IIF(m.ICLR = '******', m.SClr,m.ICLR)
      *B802028,1  (End)     
      
      *B802451,1 Reham On 08/08/1999   *** Begin ***
      *B802451,1 Prevent printing duplicate cost items.
      *IF EMPTY(m.ICLR) AND (cCatgTyp # 'M')
      *  m.IClr = SUBSTR(m.ITEM,lnNonMajSt,lnColorLen)
      *ENDIF
      *B802451,1 Reham On 08/08/1999   *** End   ***
      
      INSERT INTO (WORKFILE) FROM MEMVAR
    ENDIF
  ENDSCAN  && end scan style in BOM file

  lcOldMajor = STYLE.cStyMajor
  *B802028,1 (Begin) Do not get record for each color.
  SELECT STYLE
  lcStyMaj = SUBSTR(STYLE,1,lnMajorLen)
  LOCATE FOR SUBSTR(STYLE,1,lnMajorLen) > lcStyMaj
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

*B802157,1 Reham On 05/09/1999   *** Begin ***
*B802157,1  Variable to calculate cost by size.
lnCstBySiz = 0
*B802157,1 Reham On 05/09/1999   *** End   ***

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
    DO RPT_HDR WITH XREPORT,'',R_WIDTH
    *B603348,1  ABD Remark The next Line & Move Them To  The  Left. [ Begin ]
    *@ 05,00 SAY '              ITEM   MFG                      UOM      UNIT     UNIT       TOTAL'
    *@ 06,00 SAY ' ITEM         COLOR  CODE   DESCRIPTION       USE  QUANTITY     COST        COST'
    @ 05,00 SAY '              ITEM  MFG                   UOM     UNIT        UNIT         TOTAL'
    @ 06,00 SAY ' ITEM         COLOR CODE DESCRIPTION      USE QUANTITY        COST          COST'
    *B603348,1  ABD [ End ] 
    @ 07,00 SAY REPLICATE('=',80)
    ROW=08
  ENDIF

  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    @ ROW,00 SAY REPLICATE('=',80)

    *- If user can access cost.
    IF qCostPrv   && COSTING ACCESS
      ROW = ROW+1
      *B603348,1  ABD Remark The next Line & Move Tota Cost To The  Left. [ Begin ]      
      *@ ROW,028 SAY '**** TOTAL COST FOR : ' + PADR(lcSayStyle,19)
      *B603713,10 HBG 11/22/2000 Fix bug of price get out of the layout [Begin]
      *@ ROW,022 SAY '**** TOTAL COST FOR : ' + PADR(lcSayStyle,19)      
      @ ROW,025 SAY '**** TOTAL COST FOR :' + PADR(lcSayStyle,19)      
      *B603713,10 [End]
      *B603348,1  ABD [ End ]
      XSTYCST=XTOT1+XTOT2+XTOT3+XTOT4+XTOT5
 
      *B802157,1 Reham On 05/09/1999   *** Begin ***
      *B802157,1 
      *@ ROW,070 SAY XSTYCST PICTURE '9999999.99'
      *B603348,1  ABD Remark The next Line & Move Tota Cost To The  Left & Change The Picture. [ Begin ]
      *@ ROW,070 SAY lnCstBySiz PICTURE '9999999.99'
      @ ROW,067 SAY lnCstBySiz PICTURE '9999999999.99'
      *B603348,1  ABD [ End ]
      
      *B802157,1 Reham On 05/09/1999   *** End   ***
      
      ROW=ROW+1
      *B603348,1 ABD Remark The next Line & Move SELLING PRICE & Other Filed To
      *B603348,1     The  Left & Change The Picture Instead of the fields. [ Begin ]
      *@ ROW,005 SAY 'SELLING PRICE :'
      *@ ROW,20 SAY XPRICEA PICTURE '9999.99'
      *@ ROW,028 SAY '** GROSS PROFIT FOR : ' + PADR(lcSayStyle,19)
      *@ ROW,70 SAY XPRICEA - XSTYCST PICTURE '9999999.99'
      *ROW=ROW+1
      *@ ROW,00 SAY REPLICATE('=',80)
      
      @ ROW,000 SAY 'SELLING PRICE :'
      *B603713,10 HBG 11/22/2000 Increase Picture Of The pricea [Begin]
      *@ ROW,15 SAY XPRICEA PICTURE '9999.99'
      @ ROW,15 SAY XPRICEA PICTURE '999999999.99'
      *-- Fix bug of price get out of the layout
      *@ ROW,023 SAY '** GROSS PROFIT FOR: '+ PADR(lcSayStyle,19)
      @ ROW,023 SAY '**GROSS PROFIT FOR:'+ PADR(lcSayStyle,19)
      *B603713,10 [End]
      @ ROW,65 SAY XPRICEA - XSTYCST PICTURE '999999999999.99'
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',80)
      *B603348,1 ABD [ End ]       
      
    ENDIF

    ROW=ROW+1

    *B802157,1 Reham On 05/09/1999   *** Begin ***
    *B802157,1  Variable to calculate cost by size.
    lnCstBySiz = 0
    *B802157,1 Reham On 05/09/1999   *** End   ***
    
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

    DO CASE
      CASE Typ = "1"
        XHDR = '< '+lcSlbl1+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "2"
        XHDR='< '+lcSlbl2+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "3"
        XHDR='< '+lcSlbl3+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "4"
        XHDR='< '+lcSlbl4+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "5"
        XHDR='< '+lcSlbl5+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

    ENDCASE

    ROW=ROW+1
  ENDIF
  @ ROW,01 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
  
  IF cCatgTyp = 'S'
    @ ROW,14 SAY SUBSTR(ITEM,lnNonMajSt,lnColorLen)
  ELSE
    @ ROW,14 SAY ICLR
  ENDIF
  
  IF cCatgTyp = "M"
    *B606065,1 KHM 06/05/2002 (Begin) Commenting this line because the desc field has the
    *B606065,1                MFGcode.
    *@ ROW,20 SAY PADR(MFGCODE,6)
    *B606065,1 KHM 06/05/2002 (End)
  ENDIF


  *B603348,1  ABD Remark The next Line & Move them The  Left. [ Begin ] 
  *@ ROW,35 SAY SUBSTR(DESC,1,11)
  *@ ROW,47 SAY UOM
  *@ ROW,52 SAY nBomTOTQTY PICTURE '999.999'  

  *B606065,1 KHM 06/05/2002 (Begin) Changing the column position and the Num. of printed characters
  *@ ROW,27 SAY SUBSTR(DESC,1,11)
  @ ROW,21 SAY SUBSTR(DESC,1,17)
  *B606065,1 KHM 06/05/2002 (End)
  
  @ ROW,43 SAY UOM
  @ ROW,47 SAY nBomTOTQTY PICTURE '999.999'  
  *B603348,1  ABD [ End ]
  
  *B802157,1 Reham On 05/09/1999   *** Begin ***
  *B802157,1 Call local function to calculate cost.
	  Z = Typ
  =lfCostBySz()
  *B802157,1 Reham On 05/09/1999   *** End   ***
  
  *- if user can access cost
  IF qCostPrv         && COSTING ACCESS
    *B802313,1 Reham on 06/06/1999   *** Begin ***
    *B802313,1 Print the variables instead of the fields.
    *@ ROW,61 SAY UNTCOST PICTURE '999.999'
    *@ ROW,71 SAY TOTCOST PICTURE '999999.99'
  
    *B603348,1  ABD Remark The next Line & Move Tota Cost To The  Left
    *B603348,1      & Change The Picture Of UntCost 999999.999 & TotCost 999999999.99 
    *B603348,1      Instead of the fields. [ Begin ]
    *@ ROW,61 SAY lnUntCost PICTURE '999.999'
    *@ ROW,71 SAY lnTotCost PICTURE '999999.99'
    @ ROW,55 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,67 SAY lnTotCost PICTURE '9999999999.99'
    *B603348,1  ABD [ End ]
    
    *B802313,1 Reham on 06/06/1999   *** End   ***
  ENDIF
  ROW=ROW+1
  Z = Typ

  *B802157,1 Reham On 05/09/1999   *** Begin ***
  *B802157,1 Already calculated in the local function to calculate cost.
  *XTOT&z=XTOT&z+TOTCOST
  *B802157,1 Reham On 05/09/1999   *** End   ***

  SELE &WORKFILE
  SKIP
  IF XPRV_TYP<>TYP

    IF qCostPrv          && COSTING ACCESS
      *B603348,1  ABD Remark The next Lines & Move Them  to the Left & Change The Picture 
      *B603348,1      Instead of the Variable. [ Begin ]
      *DO CASE
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
      *ENDCASE
      DO CASE
        CASE XPRV_TYP='1'
          @ ROW,025 SAY ' '+lcSlbl1+' TOTAL :=>'
          @ ROW,067 SAY XTOT1 PICTURE '9999999999.99'
        CASE XPRV_TYP='2'
          @ ROW,025 SAY ' '+lcSlbl2+' TOTAL :=>'
          @ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
        CASE XPRV_TYP='3'
          @ ROW,025 SAY ' '+lcSlbl3+' TOTAL :=>'
          @ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
        CASE XPRV_TYP='4'
          @ ROW,025 SAY ' '+lcSlbl4+' TOTAL :=>'
          @ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
        CASE xPrv_Typ = "5"
          @ ROW,025 SAY ' '+lcSlbl5+' TOTAL :=>'
          @ ROW,067 SAY XTOT5 PICTURE '9999999999.99'
      ENDCASE
      *B603348,1  ABD [ End ]
    ENDIF

    XPRV_TYP=TYP
    ROW=ROW+1
    IF &BREAK=HBREAK
      DO CASE
        CASE Typ = "1"
          XHDR='< '+lcSlbl1+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "2"
          XHDR='< '+lcSlbl2+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "3"
          XHDR='< '+lcSlbl3+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "4"
          XHDR='< '+lcSlbl4+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "5"
          XHDR='< '+lcSlbl5+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
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

*B802157,1 Reham On 05/09/1999   *** Begin ***
*B802157,1  Variable to calculate cost by size.
lnCstBySiz = 0
*B802157,1 Reham On 05/09/1999   *** End   ***

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
DO WHILE .T.

  IF ROW >=53
    PAGENO = PAGENO+1
    DO RPT_HDR WITH XREPORT,'',R_WIDTH
    *B603348,1  ABD Remark The next Line & Move Them To  The  Left. [ Begin ]
    *@ 05,00 SAY '                      ITEM  MFG                UOM     UNIT     UNIT       TOTAL'
    *@ 06,00 SAY '      ITEM            COLOR CODE   DESCRIPTION USE QUANTITY     COST        COST'
    @ 05,00 SAY '               ITEM     MFG                UOM    UNIT        UNIT         TOTAL'
    @ 06,00 SAY ' ITEM          COLOR    CODE   DESCRIPTION USE QUANTIT        COST          COST'
    *B603348,1  ABD [ End ]
    @ 07,00 SAY REPLICATE('=',80)
    ROW=08
  ENDIF

  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    @ ROW,00 SAY REPLICATE('=',80)
    ROW = ROW+1

    *B603348,1  ABD Remark The next Line & Move Tota Cost To The  Left. [ Begin ]      
    *@ ROW,029 SAY '**** TOTAL COST FOR : '+SUBSTR(HBREAK,1,12)+'  '+SUBSTR(HBREAK,13,6)   &&TAK06/16/94
    @ ROW,022 SAY '**** TOTAL COST FOR : '+SUBSTR(HBREAK,1,12)+'  '+SUBSTR(HBREAK,13,6)   &&TAK06/16/94
    *B603348,1  ABD [ End ]

    
    

    XSTYCST=XTOT1+XTOT2+XTOT3+XTOT4+XTOT5

    IF qCostPrv         && COSTING ACCESS
      *B802157,1 Reham On 05/09/1999   *** Begin ***
      *B802157,1 
      *@ ROW,070 SAY XSTYCST PICTURE '999999.99'
      
      *B603348,1  ABD Remark The next Line & Move Tota Cost To The  Left & Chnage The Picture. [ Begin ]
      *@ ROW,070 SAY lnCstBySiz PICTURE '9999999.99'
      @ ROW,067 SAY lnCstBySiz PICTURE '9999999999.99'
      *B603348,1  ABD [ End ]
      *B802157,1 Reham On 05/09/1999   *** End   ***
    ENDIF
    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('=',80)
    ROW=ROW+1

    *B802157,1 Reham On 05/09/1999   *** Begin ***
    *B802157,1  Variable to calculate cost by size.
    lnCstBySiz = 0
    *B802157,1 Reham On 05/09/1999   *** End   ***
    
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
    XNEW_STY=.F.
    XSTYLE = ALLTRIM(&WORKFILE..cItmMajor)
    @ ROW,000 SAY ALLTRIM(lcStyMajor) + '->' + XSTYLE
    
    = SEEK (cSeekStyle,'STYLE')

    @ ROW,039 SAY PADR(STYLE.DESC1,41)
    ROW=ROW+2
    *------- NOW DISPLAY ALL STYLE INFO ----*
    XSEAS_DATA = IIF(STYLE.SEASON = 'Y     ','YEAR ROUND',gfCodDes(STYLE.SEASON , 'SEASON'))    && Get Season from codes file. 
    XDIVI_DATA = gfCodDes(STYLE.CDIVISION , 'CDIVISION')    && Get Division from codes file.  
    XGROU_DATA = gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP')    && Get Style group from codes file.  

    @ ROW,04 SAY 'SEASON.:'
    @ ROW,12 SAY PADR(XSEAS_DATA,17)
    *B603348,1  ABD Remark The next Lines & Move Division One Character Left. [ Begin ]
    *@ ROW,31 SAY 'DIVISON:'
    @ ROW,30 SAY 'DIVISON:'    
    *B603348,1  ABD [ End ]
    @ ROW,39 SAY PADR(XDIVI_DATA,17)
    
    *B603348,1  ABD Remark The next Lines & Move 'GROUP' One Character Left. [ Begin ]
    *@ ROW,58 SAY 'GROUP.:'
    @ ROW,56 SAY 'GROUP. :'
    *B603348,1  ABD [ End ]    
    @ ROW,65 SAY SUBSTR(XGROU_DATA,1,15)
    ROW = ROW + 1
    @ ROW,04 SAY 'PRICEA :'
    
    *B603348,1  ABD Remark The next Lines & Move 'PriceA' to the Right [ Begin ]        
    *@ ROW,13 SAY STYLE.PRICEA PICTURE '9999.99'
    *B603713,10 HBG 11/22/2000 Increase Picture Of The pricea [Begin]
    *@ ROW,20 SAY STYLE.PRICEA PICTURE '9999.99'    
    @ ROW,20 SAY STYLE.PRICEA PICTURE '999999999.99'   
    *B603713,10 [End]
    *B603348,1  ABD [ End ]
    
    @ ROW,30 SAY 'PRICEB :'
    
    *B603348,1  ABD Remark The next Lines & Move 'PriceA' to the Right [ Begin ]        
    *@ ROW,39 SAY STYLE.PRICEB PICTURE '9999.99'
    *B603713,10 HBG 11/22/2000 Increase Picture Of The priceb [Begin]
    *@ ROW,46 SAY STYLE.PRICEB PICTURE '9999.99'    
    @ ROW,46 SAY STYLE.PRICEB PICTURE '999999999.99'
    *B603713,10 [End]
    *B603348,1  ABD [ End ]
    
    *B603348,1  ABD Remark The next Lines & Move 'PriceC' to the  Left. [ Begin ]    
    *@ ROW,62 SAY 'PRICEC :'
    *@ ROW,72 SAY STYLE.PRICEC PICTURE '9999.99'
    @ ROW,56 SAY 'PRICEC :'
    *B603713,10 HBG 11/22/2000 Increase Picture Of The pricec [Begin]
    *@ ROW,73 SAY STYLE.PRICEC PICTURE  '9999.99'
    @ ROW,73 SAY STYLE.PRICEC PICTURE  '999999999.99'    
    *B603713,10 [End]
    *B603348,1  ABD [ End ]    
    ROW = ROW + 1

    IF qCostPrv         && COSTING ACCESS
      *B603348,1  ABD Increase The Picture Of The TotCost [ ToTCost ] To '999999999999.99'
      *B603348,1  ABD Instead of the fields. [ Begin ] 
      *@ ROW,04 SAY 'PROFITA:'
      *@ ROW,13 SAY STYLE.PRICEA - STYLE.TOTCOST PICTURE '9999.99'
      *@ ROW,30 SAY 'PROFITB:'
      *@ ROW,39 SAY STYLE.PRICEB - STYLE.TOTCOST PICTURE '9999.99'
      *@ ROW,62 SAY 'PROFITC:'
      *@ ROW,72 SAY STYLE.PRICEC - STYLE.TOTCOST PICTURE '9999.99'
      @ ROW,04 SAY 'PROFITA:'
      @ ROW,12 SAY STYLE.PRICEA - STYLE.TOTCOST PICTURE '999999999999.99'
      @ ROW,30 SAY 'PROFITB:'
      @ ROW,38 SAY STYLE.PRICEB - STYLE.TOTCOST PICTURE '999999999999.99'
      @ ROW,56 SAY 'PROFITC:'
      @ ROW,65 SAY STYLE.PRICEC - STYLE.TOTCOST PICTURE '999999999999.99'
      *B603348,1  ABD [ End ] 
      ROW=ROW+1
    ENDIF

    @ ROW,04 SAY 'SIZE (' + STYLE.SCALE + ')'
    
    = SEEK('S' + STYLE.SCALE,'SCALE')    

    COL = 14
    FOR lnI = 1 TO 8 
      lcSc = STR(lnI,1)
      @ ROW,COL SAY PADL(ALLTRIM(SCALE.SZ&lcSc),5) PICTURE '@Z XXXXX' 
      COL = COL + 6
    ENDFOR

    @ ROW,62 SAY 'PATTERN:'
    @ ROW,70 SAY ALLTRIM(STYLE.PATTERN)
    ROW = ROW + 1
    @ ROW,04 SAY 'PREPK(' + STYLE.PREPAK + '):'

    IF !EMPTY(STYLE.PREPAK)
      = SEEK('P' + STYLE.SCALE + STYLE.PREPAK ,'SCALE')
      STORE 0 TO XPREPAK
      X   = 1
      COL = 16
      DO WHILE X<=8
        Y=STR(X,1)
        @ ROW,COL SAY SCALE.PP&Y PICTURE '@Z 999'
        X = X+1
        COL = COL + 6
      ENDDO
    ENDIF

    * LINE 8
    COL=14
    ROW=ROW+1
    IF ROW <= 53
        @ ROW,04 SAY 'COLORS  :'
    ENDIF
    
    DO WHILE ALLTRIM(STYLE.CSTYMAJOR) == XSTYLE
      IF EOF()
        EXIT
      ENDIF
      IF ROW > 53
        PAGENO = PAGENO+1
        DO RPT_HDR WITH XREPORT,'',R_WIDTH
        *B603348,1  ABD Remark The next Line & Move Them To  The  Left. [ Begin ]
        *@ 05,00 SAY '                      ITEM  MFG                UOM     UNIT     UNIT       TOTAL'
        *@ 06,00 SAY '      ITEM            COLOR CODE DESCRIPTION   USE QUANTITY     COST        COST'
         @ 05,00 SAY '               ITEM     MFG                UOM    UNIT        UNIT         TOTAL'
         @ 06,00 SAY ' ITEM          COLOR    CODE   DESCRIPTION USE QUANTIT        COST          COST'
        *B603348,1  ABD [ End ] 
        @ 07,00 SAY REPLICATE('=',80)
        ROW=08
        @ ROW,04 SAY 'COLORS  :'
      ENDIF

      @ ROW,COL SAY IIF(lcFree_Clr = 'C',;
              SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),'******')
      
      SKIP IN STYLE
      ROW = IIF( COL >= 60 , ROW + 1  , ROW )
      COL = IIF( COL <= 60 , COL + 7 , 14 )   &&TAK06/16/94
      IF STYLE.STYLE<> XSTYLE .AND. COL = 14
        ROW = ROW - 1
      ENDIF
    ENDDO

    ROW=ROW+1

    DO CASE
      CASE Typ = "1"
        XHDR='< '+lcSlbl1+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "2"
        XHDR='< '+lcSlbl2+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "3"
        XHDR='< '+lcSlbl3+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "4"
        XHDR='< '+lcSlbl4+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

      CASE Typ = "5"
        XHDR='< '+lcSlbl5+' >'
        lnLen=INT(((80-LEN(XHDR))/2))
        @ ROW,00 SAY REPLICATE('-',lnLen)
        @ ROW,lnLen+1 SAY XHDR
        @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
    ENDCASE

    ROW=ROW+1
  ENDIF         && END OF NEW STYLE
 *B603348,1  ABD Remark The next Line & Move Item To The  Left. [ Begin ]
  *@ ROW,06 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
  @ ROW,01 SAY IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
 *B603348,1  ABD [ End ]
  IF SCLR = '******'
    *B603348,1  ABD Remark The next Line & Move Color To The  Left. [ Begin ]
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
    *B603348,1  ABD [ End]
  ENDIF
  
  IF cCatgTyp = "M"
    *B603348,1  ABD Remark The next Line & Move MFGCODE To The  Left. [ Begin ]
    *@ ROW,28 SAY PADR(MFGCODE,6)

    *B606065,1 KHM 06/05/2002 (Begin) Commenting this line because the desc field has the
    *B606065,1                MFGcode.
    * @ ROW,24 SAY PADR(MFGCODE,6)
    *B606065,1 KHM 06/05/2002 (End)
     
    *B603348,1  ABD [ End ]
  ENDIF
  *B603348,1  ABD Remark The next Line & Move Code Cost To The  Left. [ Begin ]
  *@ ROW,35 SAY SUBSTR(DESC,1,11)
  *@ ROW,47 SAY UOM
  *@ ROW,52 SAY nBomTOTQTY PICTURE '999.999'

  *B606065,1 KHM 06/05/2002 (Begin) Changing the column position and the Num. of printed characters
  *@ ROW,31 SAY SUBSTR(DESC,1,11)
  @ ROW,24 SAY SUBSTR(DESC,1,17)
  *B606065,1 KHM 06/05/2002 (End)

  @ ROW,43 SAY UOM
  @ ROW,47 SAY nBomTOTQTY PICTURE '999.999'  
  *B603348,1  ABD [ End ]
  

  *B802157,1 Reham On 05/09/1999   *** Begin ***
  *B802157,1 Call local function to calculate cost.
  Z = Typ
  =lfCostBySz()
  *B802157,1 Reham On 05/09/1999   *** End   ***
  
  IF qCostPrv             && COSTING ACCESS
    *B802313,1 Reham on 06/06/1999   *** Begin ***
    *B802313,1 Print the variables instead of the fields.
    *@ ROW,61 SAY UNTCOST PICTURE '999.999'
    *@ ROW,71 SAY TOTCOST PICTURE '999999.99'
    
    *B603348,1  ABD Remark The next Line & Move Tota Cost To The  Left
    *B603348,1      & Picture Of UntCost 999999.999 & TotCost 999999999.99
    *B603348,1      Instead of the fields. [ Begin ]     
    *@ ROW,61 SAY lnUntCost PICTURE '999.999'
    *@ ROW,71 SAY lnTotCost PICTURE '999999.99'

    @ ROW,55 SAY lnUntCost PICTURE '9999999.999'
    @ ROW,67 SAY lnTotCost PICTURE '9999999999.99'
    *B603348,1  ABD [ End ]
    *B802313,1 Reham on 06/06/1999   *** End   ***
  ENDIF
  ROW=ROW+1

  Z = Typ
  * 
  *B802157,1 Reham On 05/09/1999   *** Begin ***
  *B802157,1 Already calculated in the local function to calculate cost.
  *XTOT&z=XTOT&z+TOTCOST
  *B802157,1 Reham On 05/09/1999   *** End   ***

  *MAB NOW WHERE THE END
  *---- NOW DISPLAY THE FABRIC AND STYLE COLOR ASSIGNEMENTS ----*
  *B802028,1 (Begin) Remark the following and just say the style color and
  *B802028,1         current item color.    
  *IF SCLR<>'******' .AND. cCatgTyp <> "M"
  *COL = 15
  *@ ROW,06 SAY 'COLORS  :'
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
  *COL = IIF( COL <= 51 , COL + 14 , 15 )     &&TAK 06/16/94
  *IF (XPRV_TYP<>TYP .OR.  cItmMajor <> XSTYLE) .AND. COL = 15
  *ROW = ROW - 1
  *ENDIF
  *ENDDO
  IF SUBSTR(cItmMask,lnNonMajSt,lnColorLen)<>'******' .AND. cCatgTyp <> "M"
    COL = 15
    @ ROW,06 SAY 'COLORS  :'    
    @ ROW,COL SAY SCLR
    @ ROW,COL+6 SAY '-'+ ICLR
    *B802028,1 (End)
    ROW=ROW+1
  ENDIF
  
  IF .NOT. EOF() .AND. (XPRV_TYP=TYP)
    SKIP IN (WORKFILE)
  ENDIF

  IF XPRV_TYP<>TYP

    IF qCostPrv           && COSTING ACCES
      *B603348,1  ABD Remark The next Lines & Move Them  to the Left & Change The Picture  
      *B603348,1       Of The Variable Instead of the variable. [ Begin ]
      *DO CASE
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
      *ENDCASE
      DO CASE
        CASE XPRV_TYP='1'
          @ ROW,025 SAY ' '+lcSlbl1+' TOTAL :=>'
          @ ROW,067 SAY XTOT1 PICTURE '9999999999.99'
        CASE XPRV_TYP='2'
          @ ROW,025 SAY ' '+lcSlbl2+' TOTAL :=>'
          @ ROW,067 SAY XTOT2 PICTURE '9999999999.99'
        CASE XPRV_TYP='3'
          @ ROW,025 SAY ' '+lcSlbl3+' TOTAL :=>'
          @ ROW,067 SAY XTOT3 PICTURE '9999999999.99'
        CASE XPRV_TYP='4'
          @ ROW,025 SAY ' '+lcSlbl4+' TOTAL :=>'
          @ ROW,067 SAY XTOT4 PICTURE '9999999999.99'
        CASE xPrv_Typ = "5"
          @ ROW,025 SAY ' '+lcSlbl5+' TOTAL :=>'
          @ ROW,067 SAY XTOT5 PICTURE '9999999999.99'
      ENDCASE
    *B603348,1  ABD [ End ]
      
    ENDIF
    XPRV_TYP=TYP
    ROW=ROW+1
    IF &BREAK=HBREAK

      DO CASE
        CASE Typ = "1"
          XHDR='< '+lcSlbl1+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "2"
          XHDR='< '+lcSlbl2+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "3"
          XHDR='< '+lcSlbl3+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "4"
          XHDR='< '+lcSlbl4+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))

        CASE Typ = "5"
          XHDR='< '+lcSlbl5+' >'
          lnLen=INT(((80-LEN(XHDR))/2))
          @ ROW,00 SAY REPLICATE('-',lnLen)
          @ ROW,lnLen+1 SAY XHDR
          @ ROW,(LEN(XHDR)+lnLen+2) SAY REPLICATE('-',(78-LEN(XHDR)-lnLen))
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
  SET ORDER TO FABRIC IN FABRIC      && To use it to validate STYLE   # in option grid.
  SET ORDER TO CODES IN CODES        && To use it to validate STYLE   # in option grid.
  SET ORDER TO SCALE IN SCALE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO BOM IN BOM            && To use it to validate STYLE   # in option grid.

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

  lcStyleTyp  = "M"
  FOR lnCount = 1 TO 5
    lcCount  = STR(lnCount,1)
    lcSlbl&lcCount = gfGetMemVar('M_C'+lcStyleTyp+'COST'+lcCount)
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

    *--B124433,1 BWA 09/23/2004 Fix the bug of not printing the hold styles.[START]
    *LOCATE FOR Fabric = Fabric.Fabric AND ;
    *         MAKE AND STATUS = 'A'
    LOCATE FOR Fabric = Fabric.Fabric AND ;
             MAKE AND STATUS $ 'AH'
    *--B124433,1 BWA 09/23/2004.[END]

    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- !   No active doestic styles in fabric group XXX .
      *--                  <Ok>
      = gfModalGen("INM38145B36000","Dialog",ALLTRIM(Fabric.Fabric))
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
CREATE CURSOR (WORKFILE) ;
   FROM ARRAY laTempStru
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
ENDIF 

*B802157,1 Calculate cost by size.
*B802157,1 Multiple tot cost by the selected sizes only & divide it by total sizes of the scale.
lnCstBySiz = lnCstBySiz + ROUND((lnBomSzs/lnSCnt)*TotCost,2)
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
