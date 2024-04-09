*:************************************************************************
*: Program file  : SOMOM100.PRG 
*: Program desc. : CUSTOMER ACTIVITY BY SEASON/STATE based on style
*: System        : ARIA APPAREL SYSTEM 2.7
*: Module        : SALES ORDER (SO)
*: Developer     : Ahmed Ibrahim (AMM)
*: Date          : 11/08/2000
*: Reference     : *C102046,1
*:************************************************************************
*: Calls         : 
*:    Procedures : NONE
*:    Functions  : lfvStates, lfwOldVal, lfStitle, lfwRunGrid, lfsrRep
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO SOSSNAN
*:************************************************************************
*: This Report Program is due to *C102046,1  and the original entry is E301185,1
*: Revised by SSE 
*:************************************************************************
*: Modifications      :
*:************************************************************************

*-- Option Grid Layout :
*-- Range of dates entered.......(orders)...........:  from .. to ..
*-- Range of completion dates....(orders)...........:  from .. to ..
*-- Show <B>ooked <S>hipped <C>ancelled <O>pen <A>ll:  default All
*-- Only orders with this primary salesrep..........:  range
*-- Only customers in this region...................:  list (codes)
*-- Only customers in these '+ PADR(LOWER(QSTATE)+'s',24,'.')+': list (codes)
*-- Only this division..............................:  list (codes)
*-- Show the following seasons......................:  list (codes)
*-- Print Non-Active accounts.......................:  Y/N

*-- limit selected seasons to not being greater than 6 seasons
*-- laSeasons : array to carry the selected seasons plus 'all'
*-- lnSeaPos  : season position in option grid fixed filter array
*-- lnSeaLen  : season array length
DECLARE laSeasons[6]
STORE '' TO laSeasons
STORE 0  TO lnSeaPos
lnSeaPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.SEASON'),1)
=GFSUBSTR(laOGFxFlt[lnSeaPos,6],@laSeasons,'|')
lnSeaLen = ALEN(laSeasons)
*B802340,1 IHB Include 'all' for season filter [start]
*-- we have to take into acoount the 'all' representation
*-- of the season.
*IF lnSeaLen > 6
  *wait window " Not Allowed to Select More Than 6 Seasons "
IF lnSeaLen > 5
  WAIT WINDOW " Not Allowed to Select More Than 5 Seasons "
*B802340,1 IHB [end]
  RETURN
ELSE
  XTSEASONS = laOGFxFlt[lnSeaPos,6]
ENDIF
*-- lnEDATEPos  : entered date position
*-- lnCDATEPos  : completion date position
*-- lnREGIONPos : region position
*-- lnREPPos    : sales representative position
STORE 0  TO lnEDATEPos, lnCDATEPos, lnREGIONPos, lnREPPos
lnEDATEPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ENTERED'),1)
lnCDATEPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.COMPLETE'),1)
lnREGIONPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.REGION'),1)
lnREPPos    = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.REP1'),1)
XLEDATE     = CTOD(SUBSTR(laOGFxFlt[lnEDATEPos,6],1,10))
XHEDATE     = CTOD(SUBSTR(laOGFxFlt[lnEDATEPos,6],12,10))
XLCDATE     = CTOD(SUBSTR(laOGFxFlt[lnCDATEPos,6],1,10))
XHCDATE     = CTOD(SUBSTR(laOGFxFlt[lnCDATEPos,6],12,10))
XSHOW_WHAT  = lcRpShow
XREGION     = laOGFxFlt[lnREGIONPos,6]
XREP        = laOGFxFlt[lnREPPos,6]           && carrying the temp name
XZERO       = lcRpNonAc
lcPhonPict  = gfPhoneTem()                    && Variable to hold the Phone Format
*-- build the filter
XC_FILTER = ' '  && customer file filter
XO_FILTER = ' '  && order header file filter
*--- setup the customer filter
XC_FILTER = "TYPE='M' "  && only master accounts
IF .NOT. EMPTY(XREGION)
  XC_FILTER = XC_FILTER + '.AND. REGION $ XREGION'
ENDIF
*-- lnTSTATESPos : states position
STORE 0  TO lnTSTATESPos
lnTSTATESPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.CADDRESS4'),1)
XTSTATES      = laOGFxFlt[lnTSTATESPos,6]  && selected states
IF !EMPTY(XTSTATES)
  XC_FILTER = XC_FILTER + " .AND. CADDRESS4 $ laOGFxFlt[lnTSTATESPos,6]"
ENDIF

IF DTOC(XLEDATE)<> ' ' .OR. DTOC(XHEDATE)<> ' '
  XO_FILTER = XO_FILTER + 'ENTERED >= XLEDATE .AND. ENTERED <= XHEDATE'
ENDIF
IF DTOC(XLCDATE)<> ' ' .OR. DTOC(XHCDATE) <> ' '
  IF !EMPTY(XO_FILTER)
    XO_FILTER = XO_FILTER + ' .AND. COMPLETE >= XLCDATE .AND. COMPLETE <= XHCDATE'
  ELSE
    XO_FILTER = XO_FILTER + 'COMPLETE >= XLCDATE .AND. COMPLETE <= XHCDATE'
  ENDIF
ENDIF
*-- xrep carry the temp name , that temp holds the selected salesrep(s)
*-- due to range - if seleted any salesrep(s). 
IF USED(XREP) AND RECCOUNT(XREP) <> 0
  IF !EMPTY(XO_FILTER)
    XO_FILTER = XO_FILTER + ' .AND. SEEK (REP1,XREP)'
  ELSE
    XO_FILTER = XO_FILTER + 'SEEK (REP1,XREP)'
  ENDIF  
ENDIF
*-- XTSEASONS : holds the slected seasons
*B802340,1 IHB Include 'all' for season filter [start]
*IF LEN(XTSEASONS) <> 0
*B802340,1 IHB [end]
  *C102046,1 remove season from order filter and make it on style level
  *IF !EMPTY(XO_FILTER)
    *B802340,1 IHB Include 'all' for season filter [start]
    *XO_FILTER = XO_FILTER + " .AND. SEASON $ laOGFxFlt[lnSeaPos,6]"
  *  XO_FILTER = XO_FILTER + " .AND. (SEASON $ laOGFxFlt[lnSeaPos,6] .OR. SEASON = '*')"
    *B802340,1 IHB [end]
  *ELSE
    *B802340,1 IHB Include 'all' for season filter [start]
    *XO_FILTER = XO_FILTER + "SEASON $ laOGFxFlt[lnSeaPos,6]"
  *  XO_FILTER = XO_FILTER + "(SEASON $ laOGFxFlt[lnSeaPos,6] .OR. SEASON = '*')"
    *B802340,1 IHB [end]
  *ENDIF  
  XS_Filter = IIF(EMPTY(laOGFxFlt[lnSeaPos,6]),'', "STYLE.SEASON $ laOGFxFlt[lnSeaPos,6]"  )
  *C102046,1 end
*B802340,1 IHB Include 'all' for season filter [start]
*ENDIF
*B802340,1 IHB [end]
*-- lnDIVPos  : division position
*-- XDIVISION : selected division(s)
STORE 0   TO lnDIVPos
lnDIVPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.CDIVISION'),1)
XDIVISION = laOGFxFlt[lnDIVPos,6]
IF !EMPTY(XDIVISION)
  IF !EMPTY(XO_FILTER)
    XO_FILTER = XO_FILTER + " .AND. cDIVISION $ XDIVISION"
  ELSE
    XO_FILTER = XO_FILTER + "cDIVISION $ XDIVISION"
  ENDIF  
ENDIF
*-- setup the order filter
*E500271,4 add Field LEDIORDER to the filter expression [Begin.]
IF !EMPTY(lcRpEdiFlt)
  IF !EMPTY(XO_FILTER)
    XO_FILTER = XO_FILTER + [ AND ]
  ENDIF
  XO_FILTER = XO_FILTER + lcRpEdiFlt
ENDIF
*E500271,4 add Field LEDIORDER to the filter expression [End.]

WORKFILE = gfTempName()
SELECT CUSTOMER
GOTO TOP
LOCATE ALL FOR &XC_FILTER
*-- if no recrds are selected
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
COPY REST TO (gcWorkDir+WORKFILE) FOR &XC_FILTER FIELD ACCOUNT,BTNAME,CADDRESS4,PHONE1
=gfOpenFile(gcWorkDir+WORKFILE,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
*-- sort to workfile index
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW " SORTING &Z RECORDS FOR THE REPORT ... " NOWAIT
INDEX ON CADDRESS4+ACCOUNT TAG &WORKFILE
SET ORDER TO TAG &WORKFILE
*C102046,1 Open style and ordline files
=gfOpenFile(gcDataDir+'ORDCANLN','ORDCANLN','SH')
=gfOpenFile(gcDataDir+'INVHDR','INVHDR','SH')
=gfOpenFile(gcDataDir+'INVLINE','INVLINEO','SH')
SET RELATION TO INVOICE INTO INVHDR
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH') 
SET RELATION TO STYLE INTO STYLE,cOrdType+Order+STR(lineno,6) INTO ORDCANLN,;
                Order+STR(lineno,6) INTO INVLINE
*C102046,1 end
*-- report layout
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ST ACCT  .....NAME..... STATUS   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR
*XX XXXXX XXXXXXXXXXXXXX XXXXXX 9999999 99999999 9999999 99999999 9999999 99999999 9999999 99999999 9999999 99999999 9999999 99999999

DO WHILE .T.
   *-- XSTOT        : array that holds totals by seasons
   *-- XTOT , XGTOT : holds subtotals and grand totals
   *B802340,1 IHB Include 'all' for season filter [start]
   *DIMENSION XSTOT_OP(lnSeaLen,2),XTOT_OP(lnSeaLen,2),XGTOT_OP(lnSeaLen,2)     && OPEN
   *DIMENSION XSTOT_BK(lnSeaLen,2),XTOT_BK(lnSeaLen,2),XGTOT_BK(lnSeaLen,2)     && BOOKED
   *DIMENSION XSTOT_SH(lnSeaLen,2),XTOT_SH(lnSeaLen,2),XGTOT_SH(lnSeaLen,2)     && SHIPPED
   *DIMENSION XSTOT_CA(lnSeaLen,2),XTOT_CA(lnSeaLen,2),XGTOT_CA(lnSeaLen,2)     && CANCELLED
   DIMENSION XSTOT_OP(lnSeaLen+1,2),XTOT_OP(lnSeaLen+1,2),XGTOT_OP(lnSeaLen+1,2)     && OPEN
   DIMENSION XSTOT_BK(lnSeaLen+1,2),XTOT_BK(lnSeaLen+1,2),XGTOT_BK(lnSeaLen+1,2)     && BOOKED
   DIMENSION XSTOT_SH(lnSeaLen+1,2),XTOT_SH(lnSeaLen+1,2),XGTOT_SH(lnSeaLen+1,2)     && SHIPPED
   DIMENSION XSTOT_CA(lnSeaLen+1,2),XTOT_CA(lnSeaLen+1,2),XGTOT_CA(lnSeaLen+1,2)     && CANCELLED
   *B802340,1 IHB [end]
   STORE 0 TO XSTOT_OP,XTOT_OP,XGTOT_OP
   STORE 0 TO XSTOT_SH,XTOT_SH,XGTOT_SH
   STORE 0 TO XSTOT_CA,XTOT_CA,XGTOT_CA
   STORE 0 TO XSTOT_BK,XTOT_BK,XGTOT_BK
   XPRT_SUB = .F.  && flag used to determine if subtotals should be printed
   PAGENO   = 0
   ROW      = 99
   XTIME    = TIME()
   BREAK    = 'SUBSTR(CADDRESS4,1,3)'
   R_TITLE= 'CUSTOMER ACTIVITY BY SEASON/'+UPPER(lcSTitle)
   XTITLE=''
   IF (.NOT. EMPTY(XLCDATE)) .OR. (.NOT. EMPTY(XHCDATE))
      XTITLE='ORDERS FOR COMPLETION DATES := '+DTOC(XLCDATE) + ' TO '+DTOC(XHCDATE)
   ELSE
     IF (.NOT. EMPTY(XLEDATE)) .OR. (.NOT. EMPTY(XHEDATE))
      XTITLE='ORDERS FOR ENTERED DATES := '+DTOC(XLEDATE) + ' TO '+DTOC(XHEDATE)
     ENDIF
   ENDIF
   SELECT &WORKFILE
   GOTO TOP
   HBREAK = SPACE(1)
   IF LEN(TRIM(BREAK)) <>0
      HBREAK = &BREAK
   ENDIF
   SET DEVICE TO PRINT
   SELECT &WORKFILE
   GOTO TOP
   *-- major print loop
   DO WHILE INKEY() <> 32
      IF ROW >=55
         PAGENO = PAGENO+1
         IF BREAK <> ' '
           *C102046,1 SSE change name of report [Begin] 
           *XREPORT = 'SOSSNAN'
           XREPORT = 'SOMOM100'
           *C102046,1 SSE change name of report [End] 
         ENDIF
         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
         XCOL=31
         
         *B802340,1 IHB Include 'all' for season filter [start]
         *AHMED
         *lcSeason = 'ALL SEASONS'
         lcSeason = IIF(empty(XS_Filter),'ALL SEASONS' ,'  TOTAL')
         @ 05,XCOL SAY lcSeason
         XCOL=XCOL+17
         *B802340,1 IHB [end]
         
         FOR X = 1 TO ALEN(laSeasons)
           STORE '' TO lcSeason
           IF !EMPTY(laSeasons[X])
             lcSeason  = gfCodDes(laSeasons[X]  , 'SEASON'   )
             @ 05,XCOL SAY SUBSTR(lcSeason,1,13)
           ENDIF
           XCOL=XCOL+17
         ENDFOR  
         *-- the maximum width of a printed report is 130
         *-- so, that report must be shorten by 2 characters.
         @ 06,000 SAY SUBSTR(UPPER(lcSTitle),1,2)+' ACCT  .....NAME..... STATUS   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR   UNITS   DOLLAR'
         @ 07,000 SAY REPLICATE('-',130)
         ROW = 8
      ENDIF
      DO WHILE .NOT. EMPTY(BREAK)
         IF &BREAK = HBREAK
            EXIT
         ENDIF
         IF XPRT_SUB  && print subtotal for this state only when any customers were printed
           @ ROW,00 SAY REPLICATE('=',130)
           ROW = ROW+1
           @ ROW,001 SAY '** SUB TOTAL **  '+HBREAK
           IF XSHOW_WHAT = 'A'  && show all types of order
             @ROW,24 SAY 'BOOKED'
             XCOL=31
    
             *B802340,1 IHB Include 'all' for season filter [start]
             *FOR X = 1 TO ALEN(laSeasons)
             FOR X = 1 TO ALEN(laSeasons)+1
             *B802340,1 IHB [end]
                
                @ROW,XCOL SAY XTOT_BK(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XTOT_BK(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
             ENDFOR  
             ROW=ROW+1
             @ROW,24 SAY 'CANCLD'
             XCOL=31
             
             *B802340,1 IHB Include 'all' for season filter [start]
             *FOR X = 1 TO ALEN(laSeasons)
             FOR X = 1 TO ALEN(laSeasons)+1
             *B802340,1 IHB [end]
                
                @ROW,XCOL SAY XTOT_CA(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XTOT_CA(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
             ENDFOR  
             ROW=ROW+1
             @ROW,24 SAY 'SHIPED'
             XCOL=31
      
             *B802340,1 IHB Include 'all' for season filter [start]
             *FOR X = 1 TO ALEN(laSeasons)
             FOR X = 1 TO ALEN(laSeasons)+1
             *B802340,1 IHB [end]
             
                @ROW,XCOL SAY XTOT_SH(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XTOT_SH(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
             ENDFOR  
             ROW=ROW+1
             @ROW,24 SAY 'OPEN  '
             XCOL=31
     
             *B802340,1 IHB Include 'all' for season filter [start]
             *FOR X = 1 TO ALEN(laSeasons)
             FOR X = 1 TO ALEN(laSeasons)+1
             *B802340,1 IHB [end]
             
                @ROW,XCOL SAY XTOT_OP(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XTOT_OP(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
             ENDFOR  
           ELSE    &&  only one order type is needed
             DO CASE
              CASE XSHOW_WHAT = 'B'
               @ROW,24 SAY 'BOOKED'
               XCOL=31
               
               *B802340,1 IHB Include 'all' for season filter [start]
               *FOR X = 1 TO ALEN(laSeasons)
               FOR X = 1 TO ALEN(laSeasons)+1
               *B802340,1 IHB [end]
               
                   @ROW,XCOL SAY XTOT_BK(X,1) PICTURE '9999999'
                   @ROW,XCOL+8 SAY ROUND(XTOT_BK(X,2),0) PICTURE '99999999'
                  XCOL=XCOL+17
               ENDFOR  
              CASE XSHOW_WHAT='C'
               @ROW,24 SAY 'CANCLD'
               XCOL=31
               
               *B802340,1 IHB Include 'all' for season filter [start]
               *FOR X = 1 TO ALEN(laSeasons)
               FOR X = 1 TO ALEN(laSeasons)+1
               *B802340,1 IHB [end]
               
                   @ROW,XCOL SAY XTOT_CA(X,1) PICTURE '9999999'
                   @ROW,XCOL+8 SAY ROUND(XTOT_CA(X,2),0) PICTURE '99999999'
                  XCOL=XCOL+17
               ENDFOR  
              CASE XSHOW_WHAT='S'
               @ROW,24 SAY 'SHIPED'
               XCOL=31

               *B802340,1 IHB Include 'all' for season filter [start]
               *FOR X = 1 TO ALEN(laSeasons)
               FOR X = 1 TO ALEN(laSeasons)+1
               *B802340,1 IHB [end]
               
                   @ROW,XCOL SAY XTOT_SH(X,1) PICTURE '9999999'
                   @ROW,XCOL+8 SAY ROUND(XTOT_SH(X,2),0) PICTURE '99999999'
                  XCOL=XCOL+17
               ENDFOR  
              CASE XSHOW_WHAT='O'
               @ROW,24 SAY 'OPEN  '
               XCOL=31
   
               *B802340,1 IHB Include 'all' for season filter [start]
               *FOR X = 1 TO ALEN(laSeasons)
               FOR X = 1 TO ALEN(laSeasons)+1
               *B802340,1 IHB [end]
               
                   @ROW,XCOL SAY XTOT_OP(X,1) PICTURE '9999999'
                   @ROW,XCOL+8 SAY ROUND(XTOT_OP(X,2),0) PICTURE '99999999'
                  XCOL=XCOL+17
               ENDFOR  
             ENDCASE
           ENDIF
           ROW = ROW+1
           @ ROW,00 SAY REPLICATE('=',130)
           ROW = ROW+1
         ENDIF          && end of print subtotals
         *-- initialize subtotals

         *B802340,1 IHB Include 'all' for season filter [start]
         *FOR X = 1 TO ALEN(laSeasons)
         FOR X = 1 TO ALEN(laSeasons)+1
         *B802340,1 IHB [end]
         
           FOR Y = 1 TO 2
             XGTOT_OP(X,Y)=XGTOT_OP(X,Y)+XTOT_OP(X,Y)
             XGTOT_BK(X,Y)=XGTOT_BK(X,Y)+XTOT_BK(X,Y)
             XGTOT_CA(X,Y)=XGTOT_CA(X,Y)+XTOT_CA(X,Y)
             XGTOT_SH(X,Y)=XGTOT_SH(X,Y)+XTOT_SH(X,Y)
           ENDFOR
         ENDFOR
         STORE 0 TO XTOT_OP
         STORE 0 TO XTOT_BK
         STORE 0 TO XTOT_CA
         STORE 0 TO XTOT_SH
         HBREAK = &BREAK
         XPRT_SUB=.F.      && for the next state
         EXIT
      ENDDO
      *-- end subtotals
      IF EOF()
         EXIT
      ENDIF
      IF ROW >=55
         ROW = 99
         LOOP
      ENDIF
      SELECT &WORKFILE
      *-- now scan through the order header file for the order filter
      *-- and calculate the figures for all seasons
      STORE 0 TO XSTOT_OP
      STORE 0 TO XSTOT_CA
      STORE 0 TO XSTOT_BK
      STORE 0 TO XSTOT_SH
      SELECT ORDHDR
      SEEK &WORKFILE->ACCOUNT
      IF EMPTY(XO_FILTER)
        XO_FILTER = " .T. "
      ENDIF

      XO_FILTER = STRTRAN(XO_FILTER,"OrdHdr.","")
      *ahmed
      ************************************************************************************
      *SCAN WHILE ACCOUNT = &WORKFILE->ACCOUNT FOR &XO_FILTER ;
      								.AND. ( STATUS <> 'X' .OR. BULK = 'N')
        
        *B802340,1 IHB Include 'all' for season filter [start]
      *  IF SEASON = '*'
      *    XSTOT_OP(1,1) = XSTOT_OP(1,1) + OPEN
      *    XSTOT_OP(1,2) = XSTOT_OP(1,2) + OPENAMT
      *    XSTOT_BK(1,1) = XSTOT_BK(1,1) + BOOK
      *    XSTOT_BK(1,2) = XSTOT_BK(1,2) + BOOKAMT
      *    XSTOT_CA(1,1) = XSTOT_CA(1,1) + CANCEL
      *    XSTOT_CA(1,2) = XSTOT_CA(1,2) + CANCELAMT
      *    XSTOT_SH(1,1) = XSTOT_SH(1,1) + SHIP
      *    XSTOT_SH(1,2) = XSTOT_SH(1,2) + SHIPAMT
      *  ENDIF
        *B802340,1 IHB [end]
        
        *B802340,1 IHB Include 'all' for season filter [start]
        *FOR X = 1 TO ALEN(laSeasons)
      *  FOR X = 2 TO ALEN(laSeasons)+1
          *IF laSeasons(X) = SEASON
      *    IF laSeasons(X-1) = SEASON
          *B802340,1 IHB [end]
      *      XSTOT_OP(X,1) = XSTOT_OP(X,1) + OPEN
      *      XSTOT_OP(X,2) = XSTOT_OP(X,2) + OPENAMT
      *      XSTOT_BK(X,1) = XSTOT_BK(X,1) + BOOK
      *      XSTOT_BK(X,2) = XSTOT_BK(X,2) + BOOKAMT
      *      XSTOT_CA(X,1) = XSTOT_CA(X,1) + CANCEL
      *      XSTOT_CA(X,2) = XSTOT_CA(X,2) + CANCELAMT
      *      XSTOT_SH(X,1) = XSTOT_SH(X,1) + SHIP
      *      XSTOT_SH(X,2) = XSTOT_SH(X,2) + SHIPAMT
      *    ENDIF
      *  ENDFOR  
      *ENDSCAN
      SCAN WHILE ACCOUNT = &WORKFILE->ACCOUNT FOR &XO_FILTER ;
             .AND. ( STATUS <> 'X' .OR. BULK = 'N')
        IF !EMPTY(XS_Filter)
          IF SEEK(Ordhdr.cOrdType+ordhdr.Order,'ORDLINE')
            SELECT ORDLINE
            SCAN WHILE cOrdType+Order = Ordhdr.cOrdType+ordhdr.Order FOR &XS_Filter
              lnSesPos = ASCAN(laSeasons,Style.Season)+1
              XSTOT_OP(lnSesPos,1) = XSTOT_OP(lnSesPos,1) + OrdLine.TotQty
              XSTOT_OP(lnSesPos,2) = XSTOT_OP(lnSesPos,2) + OrdLine.TotQty*OrdLine.Price
              XSTOT_BK(lnSesPos,1) = XSTOT_BK(lnSesPos,1) + OrdLine.TotBook
              XSTOT_BK(lnSesPos,2) = XSTOT_BK(lnSesPos,2) + OrdLine.TotBook*OrdLine.Price

              XSTOT_OP(1,1) = XSTOT_OP(1,1) + OrdLine.TotQty
              XSTOT_OP(1,2) = XSTOT_OP(1,2) + OrdLine.TotQty*OrdLine.Price
              XSTOT_BK(1,1) = XSTOT_BK(1,1) + OrdLine.TotBook
              XSTOT_BK(1,2) = XSTOT_BK(1,2) + OrdLine.TotBook*OrdLine.Price

              IF !EOF('ORDCANLN')
                SELECT ORDCANLN
                SCAN WHILE cOrdType+Order+STR(LineNo,6) = OrdLine.cOrdType+OrdLine.Order+STR(OrdLine.LineNo,6)
                  XSTOT_CA(lnSesPos,1) = XSTOT_CA(lnSesPos,1) + OrdCanLn.TotQty
                  XSTOT_CA(lnSesPos,2) = XSTOT_CA(lnSesPos,2) + OrdCanLn.TotQty*OrdLine.Price
 
                  XSTOT_CA(1,1) = XSTOT_CA(1,1) + OrdCanLn.TotQty
                  XSTOT_CA(1,2) = XSTOT_CA(1,2) + OrdCanLn.TotQty*OrdLine.Price

                ENDSCAN
              ENDIF
              SELECT ORDLINE
              IF !EOF('INVLINE') .AND. InvHdr.Status # 'V'
                SELECT INVLINE
                SCAN WHILE Order+STR(LineNo,6) = OrdLine.order+STR(OrdLine.lineno,6)
                  XSTOT_SH(lnSesPos,1) = XSTOT_SH(lnSesPos,1) + InvLine.TotQty
                  XSTOT_SH(lnSesPos,2) = XSTOT_SH(lnSesPos,2) + InvLine.TotQty*InvLine.Price

                  XSTOT_SH(1,1) = XSTOT_SH(1,1) + InvLine.TotQty
                  XSTOT_SH(1,2) = XSTOT_SH(1,2) + InvLine.TotQty*InvLine.Price
                ENDSCAN
                SELECT ORDLINE
              ENDIF
            ENDSCAN

          ENDIF
        ELSE
          XSTOT_OP(1,1) = XSTOT_OP(1,1) + OPEN
          XSTOT_OP(1,2) = XSTOT_OP(1,2) + OPENAMT
          XSTOT_BK(1,1) = XSTOT_BK(1,1) + BOOK
          XSTOT_BK(1,2) = XSTOT_BK(1,2) + BOOKAMT
          XSTOT_CA(1,1) = XSTOT_CA(1,1) + CANCEL
          XSTOT_CA(1,2) = XSTOT_CA(1,2) + CANCELAMT
          XSTOT_SH(1,1) = XSTOT_SH(1,1) + SHIP
          XSTOT_SH(1,2) = XSTOT_SH(1,2) + SHIPAMT
        ENDIF
      ENDSCAN
      *ahmed
      ************************************************************************
      SELECT &WORKFILE
      STORE 0 TO lnTot
      
      *B802340,1 IHB Include 'all' for season filter [start]
      *FOR X = 1 TO ALEN(laSeasons)
      FOR X = 1 TO ALEN(laSeasons)+1
      *B802340,1 IHB [end]
        
        lnTot = lnTot + XSTOT_BK(X,1)
      ENDFOR
      IF XSHOW_WHAT = 'A'
        IF (XZERO = 'Y') .OR. (lnTot <> 0)
          XPRT_SUB=.T.          && print subtotals for this state
          @ ROW,00 SAY SUBSTR(CADDRESS4,1,3)
          @ ROW,03 SAY ACCOUNT
          @ ROW,09 SAY SUBSTR(BTNAME,1,14)
          @ROW,24 SAY 'BOOKED'
          XCOL=31
          
          *B802340,1 IHB Include 'all' for season filter [start]
          *FOR X = 1 TO ALEN(laSeasons)
          FOR X = 1 TO ALEN(laSeasons)+1
          *B802340,1 IHB [end]
          
            @ROW,XCOL SAY XSTOT_BK(X,1) PICTURE '9999999'
            @ROW,XCOL+8 SAY ROUND(XSTOT_BK(X,2),0) PICTURE '99999999'
            XCOL=XCOL+17
          ENDFOR  
          ROW=ROW+1
          *C102046,1 adjust location
          *@ ROW,09 SAY PHONE1  PICTURE lcPhonPict
          @ ROW,03 SAY PHONE1  PICTURE lcPhonPict
          *C102046,1 end
          @ROW,24 SAY 'CANCLD'
          XCOL=31
          
          *B802340,1 IHB Include 'all' for season filter [start]
          *FOR X = 1 TO ALEN(laSeasons)
          FOR X = 1 TO ALEN(laSeasons)+1
          *B802340,1 IHB [end]
          
             @ROW,XCOL SAY XSTOT_CA(X,1) PICTURE '9999999'
             @ROW,XCOL+8 SAY ROUND(XSTOT_CA(X,2),0) PICTURE '99999999'
            XCOL=XCOL+17
          ENDFOR  
          ROW=ROW+1
          @ROW,24 SAY 'SHIPED'
          XCOL=31
          
          *B802340,1 IHB Include 'all' for season filter [start]
          *FOR X = 1 TO ALEN(laSeasons)
          FOR X = 1 TO ALEN(laSeasons)+1
          *B802340,1 IHB [end]
          
            @ROW,XCOL SAY XSTOT_SH(X,1) PICTURE '9999999'
            @ROW,XCOL+8 SAY ROUND(XSTOT_SH(X,2),0) PICTURE '99999999'
            XCOL=XCOL+17
          ENDFOR  
          ROW = ROW+1
          @ROW,24 SAY 'OPEN  '
          XCOL = 31
          
          *B802340,1 IHB Include 'all' for season filter [start]
          *FOR X = 1 TO ALEN(laSeasons)
          FOR X = 1 TO ALEN(laSeasons)+1
          *B802340,1 IHB [end]
          
            @ROW,XCOL SAY XSTOT_OP(X,1) PICTURE '9999999'
            @ROW,XCOL+8 SAY ROUND(XSTOT_OP(X,2),0) PICTURE '99999999'
            XCOL=XCOL+17
          ENDFOR  
          ROW=ROW+1
        ENDIF
      ELSE    &&  only one order type is needed
        DO CASE
          CASE XSHOW_WHAT = 'B'
            IF (XZERO = 'Y') .OR. (lnTot <> 0)
              XPRT_SUB=.T.
              @ ROW,00 SAY SUBSTR(CADDRESS4,1,3)
              @ ROW,03 SAY ACCOUNT
              @ ROW,09 SAY SUBSTR(BTNAME,1,14)
              @ROW,24 SAY 'BOOKED'
              XCOL = 31
              
              *B802340,1 IHB Include 'all' for season filter [start]
              *FOR X = 1 TO ALEN(laSeasons)
              FOR X = 1 TO ALEN(laSeasons)+1
              *B802340,1 IHB [end]
             
                @ROW,XCOL SAY XSTOT_BK(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XSTOT_BK(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
              ENDFOR  
              ROW = ROW + 1
            ENDIF
          CASE XSHOW_WHAT = 'C'
            STORE 0 TO lnTotCa

            *B802340,1 IHB Include 'all' for season filter [start]
            *FOR X = 1 TO ALEN(laSeasons)
            FOR X = 1 TO ALEN(laSeasons)+1
            *B802340,1 IHB [end]
            
              lnTotCa = lnTotCa + XSTOT_CA(X,1)
            ENDFOR
            IF (XZERO = 'Y') .OR. ( lnTotCa <> 0)
              XPRT_SUB=.T.
              @ ROW,00 SAY SUBSTR(CADDRESS4,1,3)
              @ ROW,03 SAY ACCOUNT
              @ ROW,09 SAY SUBSTR(BTNAME,1,14)
              @ ROW,24 SAY 'CANCLD'
              XCOL = 31
              *B802340,1 IHB Include 'all' for season filter [start]
              *FOR X = 1 TO ALEN(laSeasons)
              FOR X = 1 TO ALEN(laSeasons)+1
              *B802340,1 IHB [end]
                @ROW,XCOL SAY XSTOT_CA(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XSTOT_CA(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
              ENDFOR  
              ROW = ROW + 1
            ENDIF
          CASE XSHOW_WHAT = 'S'
            STORE 0 TO lnTotSh
            
            *B802340,1 IHB Include 'all' for season filter [start]
            *FOR X = 1 TO ALEN(laSeasons)
            FOR X = 1 TO ALEN(laSeasons)+1
            *B802340,1 IHB [end]
            
              lnTotSh = lnTotSh + XSTOT_SH(X,1)
            ENDFOR
            IF (XZERO = 'Y') .OR. ( lnTotSh <> 0)
              XPRT_SUB=.T.
              @ ROW,00 SAY SUBSTR(CADDRESS4,1,3)
              @ ROW,03 SAY ACCOUNT
              @ ROW,09 SAY SUBSTR(BTNAME,1,14)
              @ROW,24 SAY 'SHIPED'
              XCOL = 31
            
              *B802340,1 IHB Include 'all' for season filter [start]
              *FOR X = 1 TO ALEN(laSeasons)
              FOR X = 1 TO ALEN(laSeasons)+1
              *B802340,1 IHB [end]
            
                @ROW,XCOL SAY XSTOT_SH(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XSTOT_SH(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
              ENDFOR  
              ROW = ROW + 1
            ENDIF
          CASE XSHOW_WHAT='O'
            STORE 0 TO lnTotOp
            
            *B802340,1 IHB Include 'all' for season filter [start]
            *FOR X = 1 TO ALEN(laSeasons)
            FOR X = 1 TO ALEN(laSeasons)+1
            *B802340,1 IHB [end]
            
              lnTotOp = lnTotOp + XSTOT_OP(X,1)
            ENDFOR
            IF (XZERO = 'Y') .OR. ( lnTotOp <> 0)
              XPRT_SUB=.T.
              @ ROW,00 SAY SUBSTR(CADDRESS4,1,3)
              @ ROW,03 SAY ACCOUNT
              @ ROW,09 SAY SUBSTR(BTNAME,1,14)
              @ROW,24 SAY 'OPEN  '
              XCOL = 31
            
              *B802340,1 IHB Include 'all' for season filter [start]
              *FOR X = 1 TO ALEN(laSeasons)
              FOR X = 1 TO ALEN(laSeasons)+1
              *B802340,1 IHB [end]
            
                @ROW,XCOL SAY XSTOT_OP(X,1) PICTURE '9999999'
                @ROW,XCOL+8 SAY ROUND(XSTOT_OP(X,2),0) PICTURE '99999999'
                XCOL=XCOL+17
              ENDFOR  
              ROW = ROW + 1
            ENDIF
        ENDCASE
      ENDIF
      *-- update the subtotals
            
      *B802340,1 IHB Include 'all' for season filter [start]
      *FOR X = 1 TO ALEN(laSeasons)
      FOR X = 1 TO ALEN(laSeasons)+1
      *B802340,1 IHB [end]
            
        FOR Y = 1 TO 2
          XTOT_OP(X,Y) = XTOT_OP(X,Y) + XSTOT_OP(X,Y)
          XTOT_BK(X,Y) = XTOT_BK(X,Y) + XSTOT_BK(X,Y)
          XTOT_CA(X,Y) = XTOT_CA(X,Y) + XSTOT_CA(X,Y)
          XTOT_SH(X,Y) = XTOT_SH(X,Y) + XSTOT_SH(X,Y)
        ENDFOR
      ENDFOR
      SELECT &WORKFILE
      SKIP
   ENDDO
   *-- grand totals
   ROW = ROW + 2
   @ ROW,00 SAY REPLICATE('*',130)
   ROW = ROW+1
   @ ROW,000 SAY '**** GRAND TOTAL *****'
   IF XSHOW_WHAT = 'A'
     @ROW,24 SAY 'BOOKED'
     XCOL = 31
            
     *B802340,1 IHB Include 'all' for season filter [start]
     *FOR X = 1 TO ALEN(laSeasons)
     FOR X = 1 TO ALEN(laSeasons)+1
     *B802340,1 IHB [end]
            
       @ROW,XCOL SAY XGTOT_BK(X,1) PICTURE '9999999'
       @ROW,XCOL+8 SAY ROUND(XGTOT_BK(X,2),0) PICTURE '99999999'
       XCOL=XCOL+17
     ENDFOR  
     ROW = ROW + 1
     @ROW,24 SAY 'CANCLD'
     XCOL = 31
            
     *B802340,1 IHB Include 'all' for season filter [start]
     *FOR X = 1 TO ALEN(laSeasons)
     FOR X = 1 TO ALEN(laSeasons)+1
     *B802340,1 IHB [end]
            
       @ROW,XCOL SAY XGTOT_CA(X,1) PICTURE '9999999'
       @ROW,XCOL+8 SAY ROUND(XGTOT_CA(X,2),0) PICTURE '99999999'
       XCOL=XCOL+17
     ENDFOR  
     ROW = ROW + 1
     @ROW,24 SAY 'SHIPED'
     XCOL = 31
            
     *B802340,1 IHB Include 'all' for season filter [start]
     *FOR X = 1 TO ALEN(laSeasons)
     FOR X = 1 TO ALEN(laSeasons)+1
     *B802340,1 IHB [end]
            
       @ROW,XCOL SAY XGTOT_SH(X,1) PICTURE '9999999'
       @ROW,XCOL+8 SAY ROUND(XGTOT_SH(X,2),0) PICTURE '99999999'
       XCOL = XCOL + 17
     ENDFOR  
     ROW = ROW + 1
     @ROW,24 SAY 'OPEN  '
     XCOL = 31
            
     *B802340,1 IHB Include 'all' for season filter [start]
     *FOR X = 1 TO ALEN(laSeasons)
     FOR X = 1 TO ALEN(laSeasons)+1
     *B802340,1 IHB [end]
            
       @ROW,XCOL SAY XGTOT_OP(X,1) PICTURE '9999999'
       @ROW,XCOL+8 SAY ROUND(XGTOT_OP(X,2),0) PICTURE '99999999'
       XCOL = XCOL + 17
     ENDFOR  
   ELSE
     DO CASE
       CASE XSHOW_WHAT = 'B'
         @ROW,24 SAY 'BOOKED'
         XCOL = 31
            
         *B802340,1 IHB Include 'all' for season filter [start]
         *FOR X = 1 TO ALEN(laSeasons)
         FOR X = 1 TO ALEN(laSeasons)+1
         *B802340,1 IHB [end]
            
           @ROW,XCOL SAY XGTOT_BK(X,1) PICTURE '9999999'
           @ROW,XCOL+8 SAY ROUND(XGTOT_BK(X,2),0) PICTURE '99999999'
           XCOL = XCOL + 17
         ENDFOR  
       CASE XSHOW_WHAT = 'C'
         @ROW,24 SAY 'CANCLD'
         XCOL = 31
            
         *B802340,1 IHB Include 'all' for season filter [start]
         *FOR X = 1 TO ALEN(laSeasons)
         FOR X = 1 TO ALEN(laSeasons)+1
         *B802340,1 IHB [end]
            
           @ROW,XCOL SAY XGTOT_CA(X,1) PICTURE '9999999'
           @ROW,XCOL+8 SAY ROUND(XGTOT_CA(X,2),0) PICTURE '99999999'
           XCOL = XCOL + 17
         ENDFOR  
       CASE XSHOW_WHAT = 'S'
         @ROW,24 SAY 'SHIPED'
         XCOL = 31
            
         *B802340,1 IHB Include 'all' for season filter [start]
         *FOR X = 1 TO ALEN(laSeasons)
         FOR X = 1 TO ALEN(laSeasons)+1
         *B802340,1 IHB [end]
            
           @ROW,XCOL SAY XGTOT_SH(X,1) PICTURE '9999999'
           @ROW,XCOL+8 SAY ROUND(XGTOT_SH(X,2),0) PICTURE '99999999'
           XCOL = XCOL + 17
         ENDFOR  
       CASE XSHOW_WHAT = 'O'
         @ROW,24 SAY 'OPEN  '
         XCOL = 31
            
         *B802340,1 IHB Include 'all' for season filter [start]
         *FOR X = 1 TO ALEN(laSeasons)
         FOR X = 1 TO ALEN(laSeasons)+1
         *B802340,1 IHB [end]
            
           @ROW,XCOL SAY XGTOT_OP(X,1) PICTURE '9999999'
           @ROW,XCOL+8 SAY ROUND(XGTOT_OP(X,2),0) PICTURE '99999999'
           XCOL = XCOL + 17
         ENDFOR  
     ENDCASE
   ENDIF
   ROW = ROW + 1
   @ ROW,00 SAY REPLICATE('=',130)
   EXIT
ENDDO
DO ENDREPORT                   && end report
*C102046,1 clear relation
SELECT INVLINE
SET RELATION TO
SELECT ORDLINE
SET RELATION TO
*C102046,1 end
SET DEVICE TO SCREEN           && return set device to screen 
gcDevice = 'SCREEN'
RETURN
*-- end of report code...
*--------------------- Functions' And Procedures' Section -----
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfvStates
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : Valid for the states selection
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStates()
*!*************************************************************
FUNCTION lfvStates
PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal
lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = PADR(EVALUATE(SYS(18)),6)  && Varible to hold  the value of the current GET field
*-- IF The user want to Browse or if the state code he entered is not in the file.
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR('STATE',10) + ALLTRIM(lcStateVal),'CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = 'State Codes'
   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'
   *-- HDM Fixing Bug of 'cComp_ID' Not Found[START]
   *IF gfBrows('FOR ccomp_id+cfld_name+ccode_no+cdiscrep = ;
     gcAct_Comp+"STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')

   IF gfBrows('FOR cDefCode + cfld_name+ccode_no+cdiscrep = ;
     "N" + "STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')
   
     *-- HDM Fixing Bug of 'cComp_ID' Not Found[End]
   
     lcStateVal = laCodeFld[1]
   ENDIF
   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF    
*-- If user type inValid code does not have ? Accept it.
lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
&lcStateObj = lcStateVal
*-- end of lfvStates.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
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
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfStitle
*! Developer : Hossam El Etreby(HDM)
*! Date      : 08/16/1998
*! Purpose   : To get the Title for the STATE ,ZIP
*!             according to its country
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************
FUNCTION lfStitle
IF !USED('SYCCOMP')
  = gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
ENDIF
SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
lcZipTitle = SYCINT.CPART5LAB
RETURN (SYCINT.CPART4LAB)
*-- end of lfStitle.

*!*************************************************************
*! Name      : lfwRunGrid
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : valid function when run grid.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRunGrid()
*!*************************************************************
FUNCTION lfwRunGrid
R_WIDTH = 'W'

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
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChRep = .T.
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- end of lfsrRep.

*!*************************************************************************
*!
*!              Function: lfvSource
*!
*!*************************************************************************
*FUNCTION lfvSource
*IF lcOGVldEnt = 'SEASON' AND lsOGSource > 6
*  wait window " Not Allowed to Select More Than 6 Seasons "
*  RETURN .F.
*ENDIF


*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!E500271,4
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
*-- end of lfvEdiOrd.

