*:************************************************************************
*: Program file  : ARPINVZ.PRG  
*: Program desc. : PRINT INVOICES  8 1/2" x 8 1/2" 
*:               : This is converted from aria26 "inv810z.prg" (for Retango)
*:         System: Aria Apparel Series.
*:      Developer: TMI -  TAREK MOHAMED IBRAHIM
*:           Date: 09/01/2002
*:      Refers to: C102695,1
*:************************************************************************
*Modifications
*:************************************************************************

XNOTE_LOOP = .F.

XTAX      = (gfGetMemVar('M_TAX') = 'Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC')
XTAX_RATE = gfGetMemVar('M_TAX_RATE')
XTAX_METH = gfGetMemVar('M_TAX_METH')
XDUNS = gfGetMemVar('XDUNS')
lcTaxRefr = gfGetMemVar('M_TAX_REFE')

NEWDOC = .T.
ROW    = 0
XCOUNT = 0

SET ORDER TO TAG CODES IN CODES
SET ORDER TO TAG SCALE IN SCALE
SET ORDER TO TAG ORDHDR IN ORDHDR
SET ORDER TO TAG CONSINVH IN CONSINVH
***

*C102695,4 TMI [Start] Add the print to factor address
IF EMPTY(laSettings)
  llPrnFact = .T.
ELSE
  llPrnFact = EVAL(laSettings[2,1])
ENDIF
*C102695,4 TMI [End  ] 


lnNotLine = 1
*--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE ''     TO HLINE4
ENDIF
STORE TRIM(LACOMPADD[5]) TO HLINE5      && for the phone


*--Section to get the FromZone --*
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'
*--End Section to get the FromZone --*

*--SECTION TO GET THE COLOR LENGTH
STORE "" TO lnClrLnCd , lnClrPosCd
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnCd  = LEN(laItemSeg[lnCount,3])
    lnClrPosCd = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

XINVNAME = lcPrnComp

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SET DEVICE TO PRINT

SELECT INVHDR
GO TOP

DO WHILE INKEY() <> 32

   SELECT INVHDR
   IF NEWDOC
     LOCATE REST FOR &lcRpExp
   ENDIF
   IF EOF()
     EXIT
   ENDIF
   
   WAIT WINDOW NOWAIT 'PRINTING INVOICES - <Space Bar> TO ABORT'

   *-----------------------------------------------------------------
   * Get invoice header, line items, and financial history records.
   * If any records are not found, skip to next invoice.
   * Initialize document totals.
   *-----------------------------------------------------------------
   XINVOICE = INVOICE
   
   IF NEWDOC
      
      XNOLINES = .F.
      STORE 0.00 TO XPIECES, XSUBTOTAL

      *** This variable will hold the number of lines to be
      *** used to print the totals (MERCHANDISE,DISCOUNT,TAX,FREIGHT).
   
      XTOT_LINES = 1
      IF XTAX  
        XTOT_LINES = XTOT_LINES + 1
      ENDIF
      IF DISCOUNT <> 0
        XTOT_LINES = XTOT_LINES + 1 
      ENDIF
      IF FREIGHT + INSUR + COD <> 0
        XTOT_LINES = XTOT_LINES + 1 
      ENDIF

      NEWDOC    = .F.
      XORDER    = ORDER
      XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
      XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
      XORDER    = ORDER
      XPIKTKT   = PIKTKT
      XACCOUNT  = ACCOUNT
      XSTORE    = STORE
      XSEASON   = SEASON
      XDIVISION = CDIVISION
      
      *** GET THE BILL TO AND SHIP ADDRESS
      SELECT CUSTOMER
      SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)

      =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
      XBTNAME  = lcSolTName
      XBTADDR1 = laSoldTo[1]
      XBTADDR2 = laSoldTo[2]
      XBTADDR3 = TRIM(laSoldTo[3])
      IF LEN(TRIM(XBTADDR2)) =0
         XBTADDR2 = XBTADDR3
         XBTADDR3 = ''
      ENDIF
      XSTNAME  = lcShpTName
      XSTADDR1 = laShipTo[1]
      XSTADDR2 = laShipTo[2]
      XSTADDR3 = TRIM(laShipTo[3])
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF

      ***
      ****** FIND THE INVOICE LINES
      ***
      SELECT INVLINE
      SEEK XINVOICE
      IF EOF()
         XNOLINES=.T.
      ENDIF
      *** GET THE DESCRIPTION ABOUT THE CODES

      PTERMS     = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
      PSHIPVIA   = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
      PSPCINST   = gfCodDes(INVHDR.SPCINST,'SPCINST')
      PDIVISION  = gfCodDes(INVHDR.CDIVISION,'CDIVISION')  
      = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')     

      DO CASE
        CASE 'G' $ lcUpsType
          XZN = gfGetZone(lcUpsType,lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))   
          XZN = IIF(!EMPTY(XZN),'('+ XZN +')' ,XZN)
        CASE '2' $ lcUpsType
          XZN  = '(12)'
        CASE 'N' $ lcUpsType
          XZN  = '(22)'
        OTHERWISE
          XZN  = ''
      ENDCASE

      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      PRINTFCTR = llPrnFact  &&(XINVFACTOR='Y')
      STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3

      DO WHILE PRINTFCTR
         IF EMPTY(INVHDR.CFACCODE)
            PRINTFCTR = .F.
            EXIT
         ENDIF
         SELECT SYCFACT
         XKEY = INVHDR.CFACCODE
         SEEK XKEY

         DO CASE
           CASE EOF()
             PRINTFCTR = .F.
             STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
           CASE FOUND()
             XFNAME  = SYCFACT.cfaccomp
             XFADDR1 = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
             XFADDR2 = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
             XFADDR3 = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
             IF XFADDR2 = ' '
                XFADDR2 = XFADDR3
                XFADDR3 = ' '
             ENDIF
         ENDCASE
         EXIT
      ENDDO

      CURLOOP = '1'
      STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
   ENDIF

   IF XNOLINES .AND. INVHDR.STATUS <> 'V'  
     LOOP
   ENDIF


   SELECT INVHDR
   IF XCOUNT = 22
     ROW     = 0
     XCOUNT  = 0
     TOPROW  = ROW
   ELSE
     XCOUNT  = XCOUNT + 1
     TOPROW  = ROW
   ENDIF

   IF XINVNAME='Y'
     @ ROW,02 SAY HLINE1 
     ROW = TOPROW + 1
     SELE INVHDR
   ENDIF
   IF XINVNAME='Y'
     @ ROW,02 SAY HLINE2
   ENDIF
   IF .NOT. EMPTY(XDUNS)
     @ ROW,34 SAY 'DUNS: '+ TRIM(XDUNS)
   ENDIF
   IF PRINTFCTR   
      @ ROW,58 SAY 'R E M I T   T O:'
   ENDIF

   * LINE 2
   ROW = TOPROW +2
   IF XINVNAME='Y'
      @ ROW,02 SAY HLINE3
   ENDIF
   IF PRINTFCTR
      @ ROW,58 SAY XFNAME
   ENDIF

   * LINE 3
   ROW = TOPROW +3
   IF XINVNAME='Y'
      @ ROW,02 SAY HLINE4
   ENDIF
   IF PRINTFCTR
      @ ROW,58 SAY XFADDR1
   ENDIF

   * LINE 4
   ROW = TOPROW +4
   IF XINVNAME='Y'
      @ ROW,02 SAY HLINE5
   ENDIF
   IF PRINTFCTR
      @ ROW,58 SAY XFADDR2
   ENDIF

   * LINE 5
   ROW = TOPROW + 5
   IF PRINTFCTR
      @ ROW,58 SAY XFADDR3
   ENDIF

   * LINE 7
   ROW = TOPROW + 7
   @ ROW,33 SAY 'INVOICE: '+XINVOICE

   * LINE 10
   ROW = TOPROW + 9
   @ ROW,10 SAY XBTNAME
   IF CURLOOP = '1'    
      @ ROW,58 SAY XSTNAME
   ENDIF
   ROW = TOPROW + 10
   @ ROW,10 SAY XBTADDR1
   IF CURLOOP = '1'
      @ ROW,58 SAY XSTADDR1
   ENDIF

   ROW = TOPROW + 11
   @ ROW,10 SAY XBTADDR2
   IF CURLOOP = '1'
      @ ROW,58 SAY XSTADDR2
   ENDIF

   ROW = TOPROW + 12
   @ ROW,10 SAY XBTADDR3
   IF CURLOOP = '1'
      @ ROW,58 SAY XSTADDR3
   ENDIF

   ROW = TOPROW + 15
   @ ROW,01 SAY ACCOUNT
   @ ROW,11 SAY ORDER
   @ ROW,21 SAY SUBSTR(ALLT(PTERMS),1,15)
   @ ROW,37 SAY SUBSTR(ALLT(PSHIPVIA),1,15)
   @ ROW,53 SAY IIF(CURLOOP='1',XSTORE,'')
   @ ROW,62 SAY DEPT      
   @ ROW,68 SAY REP1
   @ ROW,72 SAY REP2
   @ ROW,76 SAY IIF(INVHDR.STATUS='V','** VOID **',CUSTPO)

   ROW = TOPROW + 17
   SELECT INVLINE

   IF .NOT. XNOTE_LOOP        
     trec1   = RECNO()
     tscale1 = " "
     tscale2 = " "
     tscale3 = " "
     do while .T.
       if invoice<>xinvoice .or. (tscale1<>" " .and. tscale2<>" " .and. tscale3<>" ")
         exit
       endif
       if tscale1=" "
         tscale1=scale
       endif
       if scale<>tscale1 .and. tscale2=" "
         tscale2=scale
       endif
       if scale<>tscale1 .and. scale<>tscale2
         tscale3=scale
       endif
       skip
     enddo
   
     SELE INVLINE
     IF .NOT. XNOLINES
       goto trec1
     ENDIF
   ENDIF                                      

   SELECT SCALE
   trec1 = recno()
   i = 49
   Y = ' '
   do while i < 52
     ti = chr (i)
     SEEK 'S'+TSCALE&TI
     if .not. eof ()

       @ ROW,34 SAY PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')

       row=row+1
     endif
     i=i+1
   enddo
   goto trec1
   CNT = 1
   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE = ' '
   ROW    = TOPROW + 21

   *** To compute the maximum row to print the lines in just one page.
   IF .NOT. XNOTE_LOOP 
     MAXROW = GETMAXROW('INVOICE=XINVOICE',ROW,(TOPROW+39),XTOT_LINES)
   ENDIF  

   DO WHILE CURLOOP = '1' .AND. .NOT. XNOTE_LOOP
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW  
        EXIT
      ENDIF

      IF TOTQTY = 0
        SKIP
        LOOP
      ENDIF

      XCOLOR = SUBSTR(INVLINE.STYLE,lnClrPosCd,lnClrLnCd)
      KEY = INVLINE.STYLE
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE

      @ ROW,01 SAY STYLE
      @ ROW,21 SAY SUBSTR(STYLE.DESC,1,12)      
      @ ROW,34 SAY QTY1   PICTURE '@Z 999'
      @ ROW,38 SAY QTY2   PICTURE '@Z 999'
      @ ROW,42 SAY QTY3   PICTURE '@Z 999'
      @ ROW,46 SAY QTY4   PICTURE '@Z 999'
      @ ROW,50 SAY QTY5   PICTURE '@Z 999'
      @ ROW,54 SAY QTY6   PICTURE '@Z 999'
      @ ROW,58 SAY QTY7   PICTURE '@Z 999'
      @ ROW,62 SAY QTY8   PICTURE '@Z 999'
      @ ROW,65 SAY TOTQTY PICTURE '99999'   
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT

      @ ROW,71  SAY PRICE     PICTURE '9999.99'
      @ ROW,79  SAY XLINETOT  PICTURE '999999.99'

      ROW = ROW + 1

      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

   SET DEVICE TO PRINT
   
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')

   IF ENDPAGE = '1' .AND. .NOT. XNOTE_LOOP                 
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
             
      ROW = TOPROW + 43
      @ ROW,01 SAY INVHDR.PIKTKT
      
      @ ROW,09 SAY INVHDR.SHIPDATE
      
      @ ROW,17 SAY PSHIPVIA
      *C102695,4 TMI [Start] Shift to right
      *@ ROW,43 SAY '***'
      @ ROW,47 SAY '***'
      *C102695,4 TMI [End  ] 
      @ ROW,51 SAY '*****'
      @ ROW,73 SAY '******.**'
      ROW = TOPROW + 51
      LOOP
   ENDIF

   *C102695,4 TMI [Start] Ask to print notes
   IF llRpInvNot
     *C102695,4 TMI [End  ] 
     SELECT NOTEPAD
     lnOldMemW = SET("MEMOWIDTH")
     SET MEMOWIDTH TO 75
   
     IF TYPE + KEY <> 'C' + XINVOICE
       SEEK 'C' + XINVOICE       
       *C102695,4 TMI [Start] comment this line
       *lnMemLins = MEMLINES(NOTEPAD.MNOTES)  
       *C102695,4 TMI [End  ] 
     ENDIF
   
     IF TYPE + KEY = 'C' + XINVOICE
       *C102695,4 TMI [Start] get mem lines
       lnMemLins = MEMLINES(NOTEPAD.MNOTES)  
       *C102695,4 TMI [End  ] 
       MAXROW = IIF (lnMemLins >= (TOPROW+39) - (ROW+1), (TOPROW+39) - 1,;
                     IIF (lnMemLins >= ((TOPROW+39)-(ROW+1))-XTOT_LINES ,;
                     (ROW+1+lnMemLins)-1 , (TOPROW+39)-XTOT_LINES ))
     
       @ ROW,02 SAY '* -- N O T E S -- *' 
       ROW = ROW + 1 
     
       DO WHILE lnNotLine <= lnMemLins

         IF ROW >= MAXROW
           XNOTE_LOOP = .T.
           EXIT
         ELSE
           XNOTE_LOOP = .F.
           @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
           ROW = ROW + 1
         ENDIF
         lnNotLine = lnNotLine + 1
       ENDDO
     
       IF .NOT. XNOTE_LOOP
         @ ROW,02 SAY '* -- END OF NOTES -- *'
         lnNotLine = 1
         ROW = ROW + 1
       ELSE
         @ ROW+1,12 SAY 'C O N T I N U E D ...'
         ROW = TOPROW + 43
         @ ROW,01 SAY INVHDR.PIKTKT
     
         @ ROW,09 SAY INVHDR.SHIPDATE
         @ ROW,17 SAY PSHIPVIA
         *C102695,4 TMI [Start] Shift to right
         *@ ROW,43 SAY '***'
         @ ROW,47 SAY '***'
         *C102695,4 TMI [End  ] 
         @ ROW,51 SAY '*****'
         @ ROW,73 SAY '******.**'
         ROW = TOPROW + 51
         LOOP
       ENDIF
     ENDIF

     SET MEMOWIDTH TO lnOldMemW
     *C102695,4 TMI [Start] close the above if 
   ENDIF
   *C102695,4 TMI [End  ] 

   SELECT INVHDR
   @ ROW,11 SAY '** MERCHANDISE **'
   @ ROW,50 SAY 'TOTAL PIECES'
   @ ROW,65 SAY XPIECES PICTURE '99999'  &&@ ROW,66 SAY XPIECES PICTURE '99999'
   @ ROW,78 SAY XSUBTOTAL PICTURE '9999999.99'     
   ROW = ROW + 1

   IF DISCOUNT <> 0
      @ ROW,11 SAY 'LESS : DISCOUNT'
      @ ROW,77 SAY DISCOUNT PICTURE '99999999.99'   
      ROW = ROW + 1
   ENDIF

   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC

     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,78 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,78 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
   ENDIF  

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,11 SAY 'FREIGHT'
      @ ROW,78 SAY WKAMT PICTURE '9999999.99'       
      ROW = ROW + 1
   ENDIF

   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC

     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  

     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,78 SAY INVHDR.TAX_AMT PICTURE '9999999.99'
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,78 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  

   ENDIF  
   ROW = TOPROW + 38  &&ROW = TOPROW + 39
   @ ROW,08 SAY XNOTE1
   ROW = ROW + 1
   @ ROW,08 SAY XNOTE2
   ROW = TOPROW + 40   &&ROW = TOPROW + 41
   IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ ROW,01 SAY 'APPROVAL: ' + INVHDR.APPROVAL
   ENDIF
   ROW = TOPROW + 41
   @ ROW,63 SAY 'INVOICE: '+INVHDR.INVOICE

   ROW = TOPROW + 43
   @ ROW,01 SAY INVHDR.PIKTKT
   @ ROW,09 SAY INVHDR.SHIPDATE
   @ ROW,17 SAY PSHIPVIA
   *C102695,4 TMI [Start] Shift to right
   *@ ROW,43 SAY CARTONS                  PICTURE '@Z 999'
   @ ROW,47 SAY CARTONS                  PICTURE '@Z 999'
   *C102695,4 TMI [End  ] 
   @ ROW,51 SAY WEIGHT                   PICTURE '@Z 99999'
   @ ROW,72 SAY INVHDR.TOTALCHG         PICTURE '9999999.99' 

   ROW = TOPROW + 51
   SELECT INVHDR
   SEEK XINVOICE
   *C102695,4 TMI [Start] replace print flag if only output is to printer
   IF gcDevice='PRINTER'  
     *C102695,4 TMI [End  ] 
     REPLACE PRTFLAG WITH 'P'
     *C102695,4 TMI [Start] 
   ENDIF
   *C102695,4 TMI [End  ] 
   SELECT INVHDR
   SKIP
   NEWDOC = .T.
ENDDO

DO ENDREPORT

SET DEVICE TO SCREEN

RETURN

***********************************************************************
* FUNC : GETMAXROW.
* DESC : Function to compute the MAXROW for printing the invoice lines
*        and the invoice Notepad. 
* Auth : Tarek Mohamed Ibrahim (TMI).
* Date : 09/03/2002
* PARM : XCONDITION --> We will count the lines according to this condition.
*        XSTART ------> The start row of printing.
*        XEND --------> The last row of printing.
*        XTOT_LINES --> Number of rows to print the totals.
***********************************************************************
FUNCTION GETMAXROW
PARAMETERS XCONDITION,XSTART,XEND,XTOT_LINES

XREC_NO = RECNO()
COUNT REST TO XLINES_NO WHILE &XCONDITION
GOTO XREC_NO
MAXROW = IIF (XLINES_NO >= XEND-XSTART, XEND - 1        ,;
              IIF (XLINES_NO >= (XEND-XSTART)-XTOT_LINES ,;
                  (XSTART+XLINES_NO)-1 , XEND-XTOT_LINES ))
RETURN (MAXROW)

*!*************************************************************
*! Name : gfGetZone.
*! Auth : Tarek Mohamed Ibrahim (TMI).
*! Date : 09/03/2002
*! Synopsis : Get the zone to be printed in the invoice format.
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,lcSyszone),'',&lcSyszone..ZONE)