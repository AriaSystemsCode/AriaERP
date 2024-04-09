*:************************************************************************
*: Program file  : SOSSANHR.PRG   
*: Program desc. : Customized State/Season analysis report (for Har10)
*:         System: ARIA APPAREL SYSTEM 2.7
*: Module        : SALES ORDER (SO)
*:      Developer: TMI - TAREK MOHAMED IBRAHIM
*: Date          : 07/10/2002
*: Refresnce     : C102661
*:************************************************************************
*: Calls         : 
*:    Procedures : NONE
*:    Functions  : lfvStates, lfwOldVal, lfStitle, lfwRunGrid, lfsrRep
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO SOSSNAN
*:************************************************************************
*: This Report Program is due to C102661
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
STORE .F. TO llShowRprt
lnSeaPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.SEASON'),1)
=GFSUBSTR(laOGFxFlt[lnSeaPos,6],@laSeasons,'|')
lnSeaLen = ALEN(laSeasons)
IF lnSeaLen > 5
  WAIT WINDOW " Not Allowed to Select More Than 5 Seasons "
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
  IF !EMPTY(XO_FILTER)
    XO_FILTER = XO_FILTER + " .AND. (SEASON $ laOGFxFlt[lnSeaPos,6] .OR. SEASON = '*')"
  ELSE
    XO_FILTER = XO_FILTER + "(SEASON $ laOGFxFlt[lnSeaPos,6] .OR. SEASON = '*')"
  ENDIF  
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
IF !EMPTY(lcRpEdiFlt)
  IF !EMPTY(XO_FILTER)
    XO_FILTER = XO_FILTER + [ AND ]
  ENDIF
  XO_FILTER = XO_FILTER + lcRpEdiFlt
ENDIF

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
COPY REST TO (gcWorkDir+WORKFILE) FOR &XC_FILTER FIELD ACCOUNT,BTNAME,CADDRESS3,CADDRESS4,PHONE1
=gfOpenFile(gcWorkDir+WORKFILE,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
*-- sort to workfile index
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW " SORTING &Z RECORDS FOR THE REPORT ... " NOWAIT
INDEX ON CADDRESS4+ACCOUNT TAG &WORKFILE
SET ORDER TO TAG &WORKFILE

*-- report layout
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ST NAME................         BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR
*XX XXXXXXXXXXXXXXXXXXXX        9999999 99999999 9999999 99999999 9999999 99999999 9999999 99999999 9999999 99999999 9999999 99999999

DO WHILE .T.
   *-- XSTOT        : array that holds totals by seasons
   *-- XTOT , XGTOT : holds subtotals and grand totals
   DIMENSION XSTOT_BK(lnSeaLen+1,2),XTOT_BK(lnSeaLen+1,2),XGTOT_BK(lnSeaLen+1,2)     && BOOKED
   STORE 0 TO XSTOT_BK,XTOT_BK,XGTOT_BK
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
         XREPORT = 'SOSSNAN'
       ENDIF
       DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
       XCOL=31
         
       lcSeason = 'ALL SEASONS'
       @ 05,XCOL SAY lcSeason
       XCOL=XCOL+17
         
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
       @ 06,000 SAY SUBSTR(UPPER(lcSTitle),1,2)+' NAME.......                  BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR  BOOKED   DOLLAR'
       @ 07,000 SAY REPLICATE('-',132)
       ROW = 8
     ENDIF
     DO WHILE .NOT. EMPTY(BREAK)
       IF &BREAK = HBREAK
          EXIT
       ENDIF

         *-- initialize subtotals
       FOR X = 1 TO ALEN(laSeasons)+1
         FOR Y = 1 TO 2
             XGTOT_BK(X,Y)=XGTOT_BK(X,Y)+XTOT_BK(X,Y)
           ENDFOR
         ENDFOR
         STORE 0 TO XTOT_BK
         HBREAK = &BREAK
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
      STORE 0 TO XSTOT_BK
      SELECT ORDHDR
      SEEK &WORKFILE->ACCOUNT
      IF EMPTY(XO_FILTER)
        XO_FILTER = " .T. "
      ENDIF

      XO_FILTER = STRTRAN(XO_FILTER,"OrdHdr.","")
     
      SCAN WHILE ACCOUNT = &WORKFILE->ACCOUNT FOR &XO_FILTER ;
      								.AND. ( STATUS <> 'X' .OR. BULK = 'N')
        
        IF SEASON = '*'
          XSTOT_BK(1,1) = XSTOT_BK(1,1) + BOOK
          XSTOT_BK(1,2) = XSTOT_BK(1,2) + BOOKAMT
        ENDIF
        
        FOR X = 2 TO ALEN(laSeasons)+1
          IF laSeasons(X-1) = SEASON
            XSTOT_BK(X,1) = XSTOT_BK(X,1) + BOOK
            XSTOT_BK(X,2) = XSTOT_BK(X,2) + BOOKAMT
          ENDIF
        ENDFOR  
      ENDSCAN
      SELECT &WORKFILE
      STORE 0 TO lnTot
      
      FOR X = 1 TO ALEN(laSeasons)+1
        
        lnTot = lnTot + XSTOT_BK(X,1)
      ENDFOR
        IF (XZERO = 'Y') .OR. (lnTot <> 0)
          @ ROW,00 SAY SUBSTR(CADDRESS4,1,3)
          @ ROW,03 SAY SUBSTR(BTNAME,1,27)
          XCOL=31
          llShowRprt = .T. &&*C102661,4 TMI 
          FOR X = 1 TO ALEN(laSeasons)+1          
            @ROW,XCOL SAY XSTOT_BK(X,1) PICTURE '9999999'
            @ROW,XCOL+8 SAY ROUND(XSTOT_BK(X,2),0) PICTURE '99999999'
            XCOL=XCOL+17
          ENDFOR  
          IF llRpCity
            ROW=ROW+1
            @ ROW,03 SAY SUBSTR(CADDRESS3,1,15)
            @ ROW,18 SAY PHONE1  PICTURE lcPhonPict
          ENDIF
          ROW=ROW+1
        ENDIF

      *-- update the subtotals            
      FOR X = 1 TO ALEN(laSeasons)+1            
        FOR Y = 1 TO 2
          XTOT_BK(X,Y) = XTOT_BK(X,Y) + XSTOT_BK(X,Y)
        ENDFOR
      ENDFOR
      SELECT &WORKFILE
      SKIP
   ENDDO
   
   *-- grand totals
   ROW = ROW + 2
   @ ROW,00 SAY REPLICATE('*',132)
   ROW = ROW+1
   @ ROW,000 SAY '**** GRAND TOTAL *****'
     @ROW,24 SAY 'BOOKED'
     XCOL = 31            
     FOR X = 1 TO ALEN(laSeasons)+1            
       @ROW,XCOL SAY XGTOT_BK(X,1) PICTURE '9999999'
       @ROW,XCOL+8 SAY ROUND(XGTOT_BK(X,2),0) PICTURE '99999999'
       XCOL=XCOL+17
     ENDFOR  
     ROW = ROW + 1
   @ ROW,00 SAY REPLICATE('=',132)
   EXIT
ENDDO

IF llShowRprt  &&*C102661,4 TMI
  DO ENDREPORT                   && end report
*C102661,4 TMI [Start] 
ELSE
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')  
  SET PRINTER TO
ENDIF  
*C102661,4 TMI [End  ] 

SET DEVICE TO SCREEN           && return set device to screen 
gcDevice = 'SCREEN'
RETURN
*-- end of report code...
*--------------------- Functions' And Procedures' Section -----
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lfvStates
*! Developer : TMI - Tarek Mohammed Ibrahim
*! Date      : 07/10/2002
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
   IF gfBrows('FOR cDefCode + cfld_name+ccode_no+cdiscrep = ;
     "N" + "STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')
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
*! Developer : TMI - Tarek Mohammed Ibrahim
*! Date      : 07/10/2002
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
*! Developer : TMI - Tarek Mohammed Ibrahim
*! Date      : 07/10/2002
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
*! Developer : TMI - Tarek Mohammed Ibrahim
*! Date      : 07/10/2002
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
*! Developer : TMI - Tarek Mohammed Ibrahim
*! Date      : 07/10/2002
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

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : TMI - Tarek Mohammed Ibrahim
*! Date      : 07/10/2002
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

