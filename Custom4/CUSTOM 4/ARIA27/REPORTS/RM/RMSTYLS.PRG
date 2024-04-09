*:************************************************************************
*: Program file  : RMSTYLS.PRG 
*: Program desc. : DETAILED LISTING OF ALL THE RETURNS
*: For Report    : (@x,y SAY)
*:         System: ARIA APPAREL SYSTEM 2.7
*: Module        : Return Merchandise (RM)
*:      Developer: IHB
*:************************************************************************
*: Calls         : 
*:    Procedures : RA911
*:    Functions  : lfvAcc, lfWOldVal, lfvWareCod, lfvAccount,
*:               : lfwRepWhen, lfOldValue, lfvFabric, lfsrvSty, lfAdjSeg,
*:               : lfvpbDateOk, lfvDateRng, lfsrLoc
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO RMSTYLS
*:************************************************************************
*: This Report Program is due to E301133,1
*:************************************************************************
*: Modifications
*: B802945,1 HDM 01/26/2000 Fixing bug of Variable 'XREASON' Not Found
*: B603688,1 BWA 06/19/2000 Fix the bug of including voided credit memos
*: B603688,1                which is distorting the figures - We need to 
*: B603688,1                omit them from the report.
*: B603746,1 WAB 07/24/2000 Fix the Bug of No Reason found in the report when u select (BOTH)
*: B603161,1 WAB 07/27/2000 Fix the bug of accumulate a diffrent Currencies .SO WE LET THE 
*: B603161,1 WAB            Report support multi currency format.
*: B603713,7 KAM 01/12/2000 change the fields width amount,price
*: B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*: B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*: B604062,1 KAM 12/13/2000 Fix bug print subtotal for group blow not group abov
*: B604062,1 KAM            in case R/A sorted by reason
*: E301643,1 MHM 07/29/2001 Add Electronic status to RM module to link with CRM 
*: B605304,1 WAB 12/31/2001 Fix the bug of ariable 'lcrpexp3' does not exist
*: B605458,1 ABD 04/17/2002 Fix bug that the report doesn't show the damaged Qty.
*: B132331,1 TMI 06/07/2006 Fix a bug that when selecting Open R/A with specific account no data is returned
*:************************************************************************
*-- Option Grid Layout :
*-- Range of dates returned    : from .. to ..
*-- Range of styles            : subdivided according to style segments
*-- Only this fabric           : ..
*-- Range of accounts          : range of accounts
*-- Print size breakdown       : <Y>es <N>o       default Y
*-- Print the amount           : <Y>es <N>o       default Y
*-- Report                     : <O>pen R/A <R>eceived returns or <B>oth  default R
*-- Only this warehouse        : range of warehouse
*-- Note that the range of styles option is subdivided according to the style code
*-- structure (e.g. if we have style code structure consists of two segments
*-- free and color then the range of styles option in the option grid will appear
*-- as two options one representing the free segment and the other for the color)
*-- llMultiWH : company use multiwarehouse or not
*-- lcSTitle  : get style header title
*-- XTOT      : array to calculate qtys. , totalqty. , qtys.*price , dmgqtys.*price
*-- CHOICE    : sort by
*-- SIZEB     : print size breakdown
*-- XAMOUNT   : print the amount
*-- XREPORT   : either Open R/A , Received returns or Both
*-- NULDATE   : check entered null date.
*-- XTITLE    : report title
*-- llDyelot  : .T. if company uses dyelots
llDyelot      = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')
llMultiWH     = llMultWare
lcSTitle      = gfItemMask('HI')
lcMaj         = gfItemMask('PM')     && Get the major pict. of the style
lnMajSize     = LEN(lcMaj)           && Length of the major
lnNonSeg      = gfItemMask('PN')     && Get the nonmajor pict. of the style
lnNonSegSize  = LEN(lnNonSeg)        && Length of the major
CHOICE        = lcRpSort
SIZEB         = lcRpPSize
XAMOUNT       = lcRpAmt
XREPORT       = lcRpRpt
NULDATE       = CTOD('  /  /  ')
XTITLE        = lcRpTitle
DIMENSION XTOT(2,12)
STORE 0 TO XTOT

*B605458,1 ABD - Fix bug that the report doesn't show the damaged Qty , I will define 3 new array
*B605458,1 ABD - To Hold the 2nd Qty. & damage. [Begin]
DIMENSION la2ndQty(3,12),laDmgdQty(3,12),laGntTotal(3,12)
STORE 0 TO la2ndQty , laDmgdQty , laGntTotal
*B605458,1 ABD - [End]


*-- filter adjustment
*E301643,1 MHM 29/07/2001 [start]
*--Cheking of Modules instaled flags.
llCrIstall = (OCCURS('CR',gcCmpModules)<>0)
*E301643,1 MHM 29/07/2001 [end]

*B605304,1 WAB (Start)
lcRpExp3 = ''
*B605304,1 WAB (End)
*--E301643,1 mhm add electronic status to the report [Start]
*-- XREPORT  : 'E' means Electronic R/A
*-- lcRpExp3 : report expression in case of XREPORT = 'E' means Electronic. 
*--E301643,1 mhm add electronic status to the report [End]

*-- XREPORT  : 'wO' means open R/A
*-- XREPORT  : 'R' means recieved returns
*-- XREPORT  : 'A' means ALl
*-- lcRpExp  : report expression in case of XREPORT = 'O' means open R/A or both
*-- lcRpExp2 : report expression in case of XREPORT = 'R' means received returns
*-- if nothing is seleted then lcRpExp will be .T.
*-- setting the necessrary relations
SELECT RALINE
SET RELATION TO
SET RELATION TO RANO INTO RETAUTH, STYLE INTO STYLE  ADDITIVE
SELECT RETLINE
SET RELATION TO
SET RELATION TO CRMEMO INTO RETHDR, STYLE INTO STYLE ADDITIVE
*-- lcRpExp2 will be applied in case of xreport $ 'OA'
*-- lcRpExp  will be applied in case of xreport $ 'RA'

*--E301643,1 mhm add electronic status to the report [Start]
*-- lcRpExp3 will be applied in case of xreport $ 'EA'
*--E301643,1 mhm add electronic status to the report [End]

*B603688,1 BWA 06/19/2000 Omit the voided credit memos from the report.[START]
DO CASE
  CASE lcRpVoid = 'T'
    lcRpExp = IIF(!EMPTY(lcRpExp) , lcRpExp , ".T.") + " AND RETHDR.STATUS <> 'V' "
  CASE lcRpVoid = 'V'
    lcRpExp = IIF(!EMPTY(lcRpExp) , lcRpExp , ".T.") + " AND RETHDR.STATUS = 'V' "
ENDCASE

*B603688,1 [END]

lcRpExp2  = lcRpExp

*--E301643,1 mhm add electronic status to Filter Exp. [Start]
IF llCrIstall
  lcRpExp3  = lcRpExp
ENDIF  
*--E301643,1 mhm  [End]

*-- if selected open R/A or both

*--E301643,1 mhm Change Status Both to ALL [Start]
*IF XREPORT $ 'OB'
IF XREPORT $ 'OA'
*--E301643,1 mhm [End]
  *B603161,1 WAB - in case of multi currency and the user choose Both Or open R/A
  *B603161,1 WAB -  we must change the  fixed filter RETHDR.cCurrCode to be RETAUTH.CCURRCODE
  *B603161,1 WAB - (START)
  IF llMultCurr  
    IF lnCurrPos     > 0
      lcCurFiltr     = laOGFxFlt[lnCurrPos,6]
      lcRpExp2 =STRTRAN(UPPER(lcRpExp2), 'RETHDR.CCURRCODE', 'RETAUTH.CCURRCODE', 1,1)
    ENDIF
  ENDIF  
  *B603161,1 WAB - (END)

  lcRpExp2 = lcRpExp2 + " AND RETAUTH->STATUS = 'O'"
  *B132331,1  TMI [Start] if open RA is selected remove the refrence to retline from the expression
  lcRpExp2 = STRTRAN(UPPER(lcRpExp2),'RETLINE.ACCOUNT+RETLINE.INVOICE+RETLINE.STYLE+RETLINE.CRMEMO','RALINE.ACCOUNT')
  *B132331,1  TMI [End  ] 
  *-- if entered ldate then hdate must be filled
  *-- if entered hdate then ldate not essentially to be filled
  *-- if selected date
  IF !EMPTY(LDATE) AND !EMPTY(HDATE)
    lcRpExp2 = lcRpExp2 + " AND BETWEEN(RETAUTH->RADATE,LDATE,HDATE)"
  ELSE  && else if hdate has value
    IF EMPTY(LDATE) AND !EMPTY(HDATE)
      lcRpExp2 = lcRpExp2 + " AND RETAUTH->RADATE <= HDATE"
    ENDIF
  ENDIF  && endif if selected date
ENDIF  && end if selected open R/A or both
*-- if selected receive returns or both

*--E301643,1 mhm Change Status Both to ALL [Start]
*IF XREPORT $ 'RB'
IF XREPORT $ 'RA'
*--E301643,1  [End]

  lcRpExp = lcRpExp + " AND RETHDR->STATUS <> 'X'"
  *-- if selected date
  IF !EMPTY(LDATE) AND !EMPTY(HDATE)
    lcRpExp = lcRpExp + " AND BETWEEN(RETHDR->CRDATE,LDATE,HDATE)"
  ELSE  && else hdate has value
    IF EMPTY(LDATE) AND !EMPTY(HDATE)
      lcRpExp = lcRpExp + " AND  RETHDR->CRDATE <= HDATE"
    ENDIF
  ENDIF  && end if selected date
  *-- substitute RETAUTH.CWARECODE with RETHDR.CWARECODE
  lcRpExp  = STRTRAN(lcRpExp,'RETAUTH.CWARECODE','RETHDR.CWARECODE')
ENDIF  && end if selected receive returns or both

*--E301643,1 mhm Add Electronic status [Start]
IF XREPORT $ 'EA'
  lcRpExp3 = lcRpExp3 + " AND RETAUTH->STATUS = 'E'"
  *-- if selected date
  IF !EMPTY(LDATE) AND !EMPTY(HDATE)
    lcRpExp3 = lcRpExp3 + " AND BETWEEN(RETHDR->CRDATE,LDATE,HDATE)"
  ELSE  && else hdate has value
    IF EMPTY(LDATE) AND !EMPTY(HDATE)
      lcRpExp3 = lcRpExp3 + " AND  RETHDR->CRDATE <= HDATE"
    ENDIF
  ENDIF  && end if selected date
  *-- substitute RETAUTH.CWARECODE with RETHDR.CWARECODE
  lcRpExp3  = STRTRAN(lcRpExp3,'RETAUTH.CWARECODE','RETHDR.CWARECODE')
ENDIF  && end if selected Electronic or both

*--E301643,1 mhm  [End]

SORTFIELD = ' '
BREAK     = ' '
XSORT     = CHOICE
XSW       = .T.        && Switch for print subtotal name by account or style
*-- case selected open R/A,receive returns or both
DO CASE
  CASE XREPORT = 'O'
    R_TITLE  = 'DETAILED LISTING OF OPEN R/A'
    XNO = 'RANO'  && return authorization number

  *--E301643,1 mhm Add Electronic status [Start]
  CASE XREPORT = 'E'
    R_TITLE  = 'DETAILED LISTING OF ELECTRONIC R/A'
    XNO = 'RANO'  && return authorization electronic number
  *--E301643,1 mhm [End]

  CASE XREPORT = 'R'
    R_TITLE  = 'DETAILED LISTING OF RETURNS'
    XNO = 'CRMEMO'  && credit memo number

  *--E301643,1 mhm Change Both to ALL [Start]
  *CASE XREPORT = 'B'
    *R_TITLE  = 'DETAILED LISTING OF OPEN R/A AND RETURNS'
  CASE XREPORT = 'A'
    IF llCrIstall
      R_TITLE  = 'DETAILED LISTING OF OPEN R/A ,ELECTRONIC R/A AND RETURNS'
    ELSE
      R_TITLE  = 'DETAILED LISTING OF OPEN R/A AND RETURNS'
    ENDIF  
  *--E301643,1 mhm Add Electronic status [Start]

ENDCASE  && end case selected open R/A,receive returns or both
*-- case selected sort by style,account or reason
DO CASE
  CASE CHOICE = 'S'
    *B603161,1 WAB - add cCurrCode to the index expretion in case of multi Currency & the user choose 
    *B603161,1 WAB - display currency by Foriegn Currency
    *B603161,1 WAB -(START)
    *SORTFIELD = 'STYLE + &XNO'
    *BREAK     = 'STYLE'
    SORTFIELD = IIF(llMultCurr .AND. lcRpCurr = "F" ,'STYLE + cCurrCode + &XNO','STYLE + &XNO')
    BREAK     = IIF(llMultCurr .AND. lcRpCurr = "F" ,'STYLE + cCurrCode ','STYLE')
    *B603161,1 WAB -(END)
    XSORT     = ' - SORT BY STYLE'
    XTOT_NAME = 'S'
  CASE CHOICE = 'A'
    *B603161,1 WAB - add cCurrCode to the index expretion in case of multi Currency & the user choose 
    *B603161,1 WAB - display currency by Foriegn Currency
    *B603161,1 WAB -(START)
    *SORTFIELD = 'ACCOUNT + STYLE + &XNO'
    *BREAK     = 'ACCOUNT'
    SORTFIELD = IIF(llMultCurr .AND. lcRpCurr = "F",'ACCOUNT + cCurrCode + STYLE + &XNO','ACCOUNT + STYLE + &XNO')
    BREAK     = IIF(llMultCurr .AND. lcRpCurr = "F",'ACCOUNT + cCurrCode','ACCOUNT')
    *B603161,1 WAB -(END)
    XSORT     = ' - SORT BY ACCOUNT'
    XTOT_NAME = 'A'
  CASE CHOICE = 'R'
    *B603161,1 WAB - add cCurrCode to the index expretion in case of multi Currency & the user choose 
    *B603161,1 WAB - display currency by Foriegn Currency
    *B603161,1 WAB -(START)
    *SORTFIELD = 'REASON + STYLE + &XNO'
    *BREAK     = 'REASON'
    SORTFIELD = IIF(llMultCurr .AND. lcRpCurr = "F",'REASON + cCurrCode + STYLE + &XNO','REASON + STYLE + &XNO')
    BREAK     = IIF(llMultCurr .AND. lcRpCurr = "F",'REASON + cCurrCode','REASON')
    *B603161,1 WAB -(END)
    XSORT     = ' - SORT BY REASON'
    XTOT_NAME = 'R'
ENDCASE  && end case selected sort by style,account or reason

*-- XBOTH_FLG : both open R/A and receive returns are selected
*-- XNAME     : identify return authorization or credit memo file
*-- XSUB_TOT  : subtotal
XBOTH_FLG = .F.
XNAME     = ' '
XSUB_TOT  = 'SUB_TOTAL:'
*-- case selected report , credit memo or return authoriztion or both
DO CASE
  CASE XREPORT = 'O'
    SELECT RALINE
    LOCATE ALL FOR &lcRpExp2 
    *-- if norecords are selected
    IF EOF()
      *-- Message : There are no records to display...!
      *--                < Ok > 
      =gfModalGen('TRM00052B40011','ALERT')
      RETURN
    ELSE  && else assign file and filter names
      XFILE  = 'RALINE'
      FILTER = lcRpExp2
      XNAME  = 'RALTEMP'
      DMGQTY = 0
      XRECNO = RECNO()
    ENDIF  && endif if norecords are selected
  
  *--E301643,1 mhm Add Electronic status [Start]
  CASE XREPORT = 'E'
    SELECT RALINE
    LOCATE ALL FOR &lcRpExp3
    *-- if norecords are selected
    IF EOF()
      *-- Message : There are no records to display...!
      *--                < Ok > 
      =gfModalGen('TRM00052B40011','ALERT')
      RETURN
    ELSE  && else assign file and filter names
      XFILE  = 'RALINE'
      FILTER = lcRpExp3
      XNAME  = 'RALTEMP'
      DMGQTY = 0
      XRECNO = RECNO()
    ENDIF  && endif if norecords are selected
  *--E301643,1 mhm Add Electronic status [Start]
  
  CASE XREPORT = 'R'
    SELECT RETLINE
    LOCATE ALL FOR &lcRpExp
    *-- if norecords are selected
    IF EOF()
      *-- Message : There are no records to display...!
      *--                < Ok > 
      =gfModalGen('TRM00052B40011','ALERT')
      RETURN
    ELSE  && else assign file and filter names
      FILTER   = lcRpExp
      XFILE    = 'RETLINE'
      XNAME    = 'RETLTEMP'
      XRECNO   = RECNO()
    ENDIF  && endif if norecords are selected
  *--E301643,1 mhm Change Both to ALL [Start]
  *CASE XREPORT = 'B'
  CASE XREPORT = 'A'
  *--E301643,1 mhm [End]
    DIMENSION XTOT_B(2,12)
    STORE 0 TO XTOT_B
    SELECT RALINE

    *--E301643,1 mhm Add Electronic status To filter if CR module installed[Start]
    *LOCATE ALL FOR &lcRpExp2
    *IF FOUND()
    *  XNAME   = 'RALTEMP'
    *  XRAL    = RECNO()
    *ENDIF
    IF llCrIstall
      LOCATE ALL FOR &lcRpExp2 
      IF FOUND()
        XNAME   = 'RALTEMP'
        XRAL    = RECNO()
      ELSE  
        LOCATE ALL FOR &lcRpExp3
        IF FOUND()
          XNAME   = 'RALTEMP'
          XRAL    = RECNO()
        ENDIF
      ENDIF
    ELSE
      LOCATE ALL FOR &lcRpExp2 
      IF FOUND()
        XNAME   = 'RALTEMP'
        XRAL    = RECNO()
      ENDIF
    ENDIF  
    
    *--E301643,1 mhm  [End]

    IF FOUND()
      XNAME   = 'RALTEMP'
      XRAL    = RECNO()
    ENDIF
    SELECT RETLINE
    LOCATE ALL FOR &lcRpExp
    IF FOUND()
      XNAME1 = 'RETLTEMP'
      XREL   = RECNO()
    ENDIF
    *-- case found records or not
    DO CASE
      CASE !FOUND('RALINE') .AND. !FOUND('RETLINE')
        =gfModalGen('TRM00052B40011','ALERT')
        RETURN
      CASE FOUND('RALINE') .AND. FOUND('RETLINE')
        XBOTH_FLG = .T.
        XNO      = 'TYPE+RANO+CRMEMO'
    ENDCASE  && end case found records or not
ENDCASE  && end case selected report
*-- if only RALINE or RETLINE selected records

IF !XBOTH_FLG
  *-- if selected both
  *--E301643,1 mhm Change status From BOTH to ALL [Start]
  *IF XREPORT = 'B'
  IF XREPORT = 'A'
  *--E301643,1 mhm  [End]

    XFILE = IIF(EMPTY(XNAME),'RETLINE','RALINE')
    
    *--E301643,1 mhm Add Electronic status to filter [Start]
    *FILTER= IIF(EMPTY(XNAME),lcRpExp,lcRpExp2)
    IF llCrIstall
      lcExpEl = lcRpExp2 +' .OR.  '+ lcRpExp3
      FILTER= IIF(EMPTY(XNAME),lcRpExp,lcExpEl )
    ELSE
      FILTER= IIF(EMPTY(XNAME),lcRpExp,lcRpExp2)
    ENDIF  
    *--E301643,1 mhm  [end]

    XNO   = IIF(EMPTY(XNAME),'CRMEMO','RANO')
    DMGQTY= IIF(EMPTY(XNAME),DMGQTY,0)
    XTYPE = IIF(EMPTY(XNAME),'RCV','R/A')
    XRECNO= IIF(EMPTY(XNAME),XREL,XRAL)
  ENDIF  && end if selected both
  XNAME = gfTempName()    &&global function gives temporarily name for the workfile.
  SELECT &XFILE
  *B603161,1 WAB - when we collecting the data from RETLINE OR RALINE we need to get the Currency 
  *B603161,1 WAB - code & the currency unit & the exchage rate from the header file so we get the 
  *B603161,1 WAB - fileds name structure & add the 3fileds in case of multi Currency 
  *B603161,1 WAB -(START)
  *COPY TO (gcWorkDir+XNAME) FOR &FILTER    && appends all needed records.
  IF llMultCurr 												&& in case of multyi Currency
    =AFIELDS(lafilfield)										&& get the file structure 		
    *-lcFileds -----> variable to hold the the fields name 
    lcFields = ''				
    FOR lnCount = 1 TO ALEN(lafilfield,1)
      lcFields = lcFields + lafilfield[lnCount,1] + ' , '
    ENDFOR
    *-- add the currency code , the currency unit, the exchange rate
    lcFields = lcFields + IIF(XFILE='RETLINE','RETHDR','RETAUTH') + '.cCurrCode , '
    lcFields = lcFields + IIF(XFILE='RETLINE','RETHDR','RETAUTH') + '.nCurrUnit , '
    lcFields = lcFields + IIF(XFILE='RETLINE','RETHDR','RETAUTH') + '.nExRate'
    COPY TO (gcWorkDir+XNAME) FIELDS &lcFields FOR &FILTER    && appends all needed records.
  ELSE
    COPY TO (gcWorkDir+XNAME) FOR &FILTER    && appends all needed records.
  ENDIF
  *B603161,1 WAB -(END)

  =gfOpenFile(gcWorkDir+XNAME,' ','EX') &&global function opens DBF (with-optionally- its index) share or exculsive.
  XFILE = IIF(XFILE='RALINE','RETAUTH','RETHDR')
  SET RELATION TO IIF(EMPTY(&XFILE..STORE),'M'+ACCOUNT,'S'+ACCOUNT+&XFILE..STORE) INTO CUSTOMER
ELSE  && else use separate structure
  *-- modified structure to include only the style instead of style and color
  *-- also adding dyelot c(10) to the structure
  XNAME = gfTempName()    &&global function gives temporarily name for the workfile.
  *B603746,1 WAB - the reason field length is c(6) not c(2)
  *B603746,1 WAB - START
  *CREATE CURSOR (XNAME) ;
   (STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
    REASON C(02), PRICE N(8,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
    QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(10,2), NEWFIELD C(10))
  *B603161,1 WAB - when we collecting the data from RETLINE & RALINE we need to get the Currency 
  *B603161,1 WAB - code & the currency unit & the exchage rate from the header file so we get the 
  *B603161,1 WAB - fileds name structure & add the 3fileds in case of multi Currency 
  *B603161,1 WAB -(START)
  IF llMultCurr 
   *B603713,7  KAM 10/12/2000 - change width of price,amount fields [start]
   *CREATE CURSOR (XNAME) ;
   (STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
    REASON C(06), PRICE N(8,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
    QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(10,2), NEWFIELD C(10),;
    cCurrCode C(3) , nCurrUnit N(4,0) , nExRate N(9,4))

   *B605458,1 ABD - Define New Field. [Begin]
   *CREATE CURSOR (XNAME) ;
   *(STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
   * REASON C(06), PRICE N(12,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
   * QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(13,2), NEWFIELD C(10),;
   * cCurrCode C(3) , nCurrUnit N(4,0) , nExRate N(9,4))

   CREATE CURSOR (XNAME) ;
   (STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
    REASON C(06), PRICE N(12,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
    QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(13,2), NEWFIELD C(10),;
    cCurrCode C(3) , nCurrUnit N(4,0) , nExRate N(9,4) , cstyGrade C(1))
    *B605458,1 ABD - [End]

   *B603713,7  KAM [end]
  ELSE
    *B603161,1 WAB -(END)
    *B603713,7 KAM 10/12/20000 change the width fields amount,price [start]
    *CREATE CURSOR (XNAME) ;
      (STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
      REASON C(06), PRICE N(8,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
      QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(10,2), NEWFIELD C(10))


    *B605458,1 ABD - Add new Field to the curser. [Begin]
    *CREATE CURSOR (XNAME) ;
    *  (STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
    *  REASON C(06), PRICE N(12,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
    *  QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(13,2), NEWFIELD C(10))

    CREATE CURSOR (XNAME) ;
      (STYLE  C(19), ACCOUNT C(05), TYPE C(01), DYELOT C(10), RANO C(06), CRMEMO C(06) ,;
      REASON C(06), PRICE N(12,2), QTY1    N(05), QTY2 N(05), QTY3 N(05), QTY4   N(05), QTY5 N(05) ,;
      QTY6   N(05), QTY7  N(05) , QTY8    N(05), TOTQTY N(10), DMGQTY N(06), AMOUNT N(13,2), NEWFIELD C(10) , cstyGrade C(1))

      *B605458,1 ABD - [End]
  
  *B603713,7 KAM [end] 
  *B603161,1 WAB -(START) endif of if llMulCurr 
  ENDIF
  *B603746,1 WAB -(END)
  
  XTEMP = gfTempName()    &&global function gives temporarily name for the workfile.
  SELECT RALINE

  *B603161,1 WAB - when we collecting the data from RALINE we need to get the Currency 
  *B603161,1 WAB - code & the currency unit & the exchage rate from the header file so we get the 
  *B603161,1 WAB - fileds name structure & add the 3fileds in case of multi Currency 
  *B603161,1 WAB -(START)
  *COPY TO (gcWorkDir+XTEMP) FOR &lcRpExp2
  IF llMultCurr 
    =AFIELDS(lafilfield)

    *-lcFileds -----> variable to hold the the fields name 
    lcFields = ''

    FOR lnCount = 1 TO ALEN(lafilfield,1)
      lcFields = lcFields + lafilfield[lnCount,1] + ' , '
    ENDFOR

    *-- add the currency code , the currency unit, the exchange rate
    lcFields = lcFields + 'RETAUTH.cCurrCode , '
    lcFields = lcFields + 'RETAUTH.nCurrUnit , '
    lcFields = lcFields + 'RETAUTH.nExRate'

    *E301643,1 MHM add Electronic filter in case of all [Start]
    *COPY TO (gcWorkDir+XTEMP) FIELDS &lcFields FOR &lcRpExp2
    IF llCrIstall
      lcExprstn = lcRpExp2 + ' OR ' + lcRpExp3
      COPY TO (gcWorkDir+XTEMP) FIELDS &lcFields FOR &lcExprstn
    ELSE  
      COPY TO (gcWorkDir+XTEMP) FIELDS &lcFields FOR &lcRpExp2 
    ENDIF
    *E301643,1 MHM [End]

  ELSE
    
    *E301643,1 MHM add Electronic filter in case of all [Start]
    *COPY TO (gcWorkDir+XTEMP) FOR &lcRpExp2
    IF llCrIstall
      lcExprstn = lcRpExp2 + ' OR ' + lcRpExp3
      COPY TO (gcWorkDir+XTEMP) FOR &lcExprstn
    ELSE
      COPY TO (gcWorkDir+XTEMP) FOR &lcRpExp2 
    ENDIF  
    *E301643,1 MHM  [End]
    
  ENDIF
  *B603161,1 WAB -(END)

  SELECT &XNAME
  APPEND FROM (gcWorkDir+XTEMP)
  SELECT RETLINE

  *B603161,1 WAB - when we collecting the data from RETLINE  we need to get the Currency 
  *B603161,1 WAB - code & the currency unit & the exchage rate from the header file so we get the 
  *B603161,1 WAB - fileds name structure & add the 3fileds in case of multi Currency 
  *B603161,1 WAB -(START)
  *COPY TO (gcWorkDir+XTEMP) FOR &lcRpExp
  IF llMultCurr 
    =AFIELDS(lafilfield)

    *-lcFields -----> variable to hold the the fields name 
    lcFields = ''

    FOR lnCount = 1 TO ALEN(lafilfield,1)
      lcFields = lcFields + lafilfield[lnCount,1] + ' , '
    ENDFOR

    *-- add the currency code , the currency unit, the exchange rate
    lcFields = lcFields + 'RETHDR.cCurrCode , '
    lcFields = lcFields + 'RETHDR.nCurrUnit , '
    lcFields = lcFields + 'RETHDR.nExRate'
    COPY TO (gcWorkDir+XTEMP) FIELDS &lcFields FOR &lcRpExp
  ELSE
    COPY TO (gcWorkDir+XTEMP) FOR &lcRpExp
  ENDIF
  *B603161,1 WAB -(END)
  
  SELECT &XNAME
  APPEND FROM (gcWorkDir+XTEMP)
  REPLACE ALL TYPE WITH '1' FOR !EMPTY(CRMEMO)
  XFILE = IIF(EMPTY(TYPE),'RETAUTH','RETHDR')
  SET RELATION TO IIF(EMPTY(&XFILE..STORE),'M'+ACCOUNT,'S'+ACCOUNT+&XFILE..STORE) INTO CUSTOMER
ENDIF  && end if only RALINE or RETLINE selected records

REPLACE ALL TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
SELECT &XNAME
INDEX ON &SORTFIELD TAG &XNAME
GO TOP
IF LEN(TRIM(BREAK))<>0
  HBREAK=&BREAK
ENDIF
IF EMPTY(XTITLE) .AND. LDATE<>NULDATE
  XTITLE= 'FROM: '+DTOC(LDATE)+' THRU: '+DTOC(HDATE)
ENDIF
XCUSTOMER = CUSTOMER.STNAME
*-- subtotal will be per style nonmajor portion
*-- so xstyle initially equal major part and xcolor initially equal nonmajor.
*-- on the same manner x_style will equal the style major length and x_color
*-- will be the style nonmajor length.
*-- we have also to check if the length of nonmajor equal zero (if feasible)
XSTYLE    = SUBSTR(STYLE,1,lnMajSize)
XCOLOR    = RIGHT(STYLE,lnNonSegSize)
X_STYLE   = SPACE(lnMajSize)
X_COLOR   = SPACE(lnNonSegSize)
XSUB      = .T.
ROW       = 99
PAGENO    = 0
R_WIDTH   = 'W'
*B603713,7 KAM 10/24/2000 increase the size of "=" [start]
*R_LEN     = 130
R_LEN     = 131
*B603713,7 KAM [end]

*B603161,1 WAB -(START) Initial variable .
*--lcCurrCode ---> variable hold the currrent Currency code of retline of the ratline
lcCurrCode = IIF(llMultCurr,cCurrCode,"")
*B603161,1 WAB -(END)
*-- report layout in case of no dyelots
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9
*     STYLETITLE     ACCT# TYP CR/RA  REASON      STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT
*1234567890123456789 12345 XXX 123456 12 1234567  1234567  1234567.99  1234567  123456.99

*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*     STYLETITLE     ACCT# TYP CR/RA  REASON      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT
*123456789012 123456 12345 R/A 123456 12 1234567 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          R/A 123456 12 1234567 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          R/A 123456 12 1234567 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          -----------------------------------------------------------------------------------------------------------
*                          RCV 123456 12 1234567 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          RCV 123456 12 1234567 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          -----------------------------------------------------------------------------------------------------------
*   SUBTOTALS              R/A                   99999 99999 99999 99999......
*                          RCV                   99999 99999 99999 99999.....
*   GRAND TOTAL            R/A                   99999 99999 99999 99999......
*                          RCV                   99999 99999 99999 99999......
*

*-- report layout in case of dyelots
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0.
*     STYLETITLE     ACCT# TYP CR/RA  DYELOT     STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT
*1234567890123456789 12345 XXX 123456 1234567890 1234567  1234567.99  1234567  123456.99

*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*     STYLETITLE     ACCT# TYP CR/RA  DYELOT      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT
*1234567890123456789 12345 R/A 123456 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          R/A 123456 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          R/A 123456 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          -----------------------------------------------------------------------------------------------------------
*                          RCV 123456 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          RCV 123456 1234567890 XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX XXXXX 123456 1234567.99 123456 123456.99
*                          -----------------------------------------------------------------------------------------------------------
*   SUBTOTALS              R/A                   99999 99999 99999 99999......
*                          RCV                   99999 99999 99999 99999.....
*   GRAND TOTAL            R/A                   99999 99999 99999 99999......
*                          RCV                   99999 99999 99999 99999......
*

*-- major print loop
SET DEVICE TO PRINT
*-- if use dyelot print dyelot in the place of reason

IF llDyelot
  *B603161,1 WAB -(START)- we must still inside teh loop when EOF() to print last sub stotal
  *DO WHILE !EOF() .AND. INKEY() <> 32
  DO WHILE INKEY() <> 32
  *B603161,1 WAB -(END)

    *-- if new page
    IF ROW > 53
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH 'RMSTYLS',XTITLE,R_WIDTH
      *-- case selected report type
      DO CASE
        CASE XREPORT = 'R'
          IF SIZEB = 'Y'
            *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  CRMEMO  DYELOT     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT'
            
            *B605458,1 ABD - Remark the next line & update the title Position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  CRMEMO  DYELOT     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
            @ 05,00 SAY lcSTitle+' ACCT# TYP  CRMEMO  DYELOT    QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY       STK.AMT'
            *B605458,1 ABD - [End]
                        
            *B603161,1 WAB -(END)
          ELSE
            *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT'
            *B605458,1 ABD - Remark the next line & update the title Position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      STK.QTY    STK.AMOUNT  '
            @ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      STK.QTY      STK.AMOUNT'
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ENDIF
        CASE XREPORT = 'O'
          IF SIZEB = 'Y'
            *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   DYELOT      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT'
            *B605458,1 ABD - Remark the next line & update the title Position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   DYELOT      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
            *B605458,1 ABD - Remark the next line & update the title Position. [Begin]
             *@ 05,00 SAY lcSTitle+' ACCOUNT#   RANO    DYELOT    QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
             @ 05,00 SAY lcSTitle+' ACCOUNT#   RANO    DYELOT    QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY       STK.AMT'             
             *B605458,1 ABD - [End]
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ELSE
            *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   DYELOT      STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT'
            *B605458,1 ABD - Remark the next line & update the title Position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   DYELOT      STK.QTY    STK.AMOUNT  '
             @ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   DYELOT      STK.QTY      STK.AMOUNT'
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ENDIF

        *--E301643,1 mhm Add Electronic status to print[Start]
        CASE XREPORT = 'E'
          IF SIZEB = 'Y'
            @ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   DYELOT      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
          ELSE
            @ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   DYELOT      STK.QTY    STK.AMOUNT  '          
          ENDIF
        *--E301643,1 mhm [End]

        *--E301643,1 mhm Change status from BOTH to ALL [Start]
        *CASE XREPORT = 'B'
        CASE XREPORT = 'A'
        *--E301643,1 mhm [End]

          IF SIZEB = 'Y'
            *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT'
            *B605458,1 ABD - Remark the next line & update the title Position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
            @ 05,00 SAY lcSTitle+' ACCT# TYP  CR/RA   DYELOT    QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY       STK.AMT'
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ELSE
            *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT'
            *B605458,1 ABD - Remark the next line & update the title Position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      STK.QTY    STK.AMOUNT  '
            @ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  DYELOT      STK.QTY      STK.AMOUNT'            
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ENDIF
      ENDCASE  && endcase selected report type
      @ 06,00 SAY REPLICATE('=',R_LEN)
      ROW = 7
    ENDIF && end if new page
    *-- printing the break (either style or customer or reason)

    *B603161,1 WAB -(START) also in case of eof() print last subtotal
    *IF HBREAK <> &BREAK
    IF HBREAK <> &BREAK .OR. EOF()
    *B603161,1 WAB -(END)
      @ ROW,00 SAY REPLICATE('-',R_LEN)
      ROW = ROW + 1
      @ ROW,00 SAY XSUB_TOT
      *-- case breake is Style or Customer or Reason
      DO CASE
        CASE XTOT_NAME = 'S'

          *B603161,1 WAB -(START) print the currency code in the sub total line in case of multi Currency
          *@ ROW,12 SAY HBREAK
      
          lcCurrency = IIF(llMultCurr .AND. lcRpCurr = "F","Currency: "+SUBSTR(HBREAK,LEN(Style)+1,3),"")
          @ ROW,11 SAY SUBSTR(HBREAK,1,LEN(Style))+lcCurrency
      
          *B603161,1 WAB -(END)

        CASE XTOT_NAME = 'A'
      
          *B603161,1 WAB -(START) print the currency code in the sub total line in case of multi Currency
          *@ ROW,12 SAY SUBSTR(XCUSTOMER,1,18)
      
          lcCurrency = IIF(llMultCurr .AND. lcRpCurr = "F","Currency: "+SUBSTR(HBREAK,LEN(Account)+1,3),"")
          @ ROW,12 SAY SUBSTR(XCUSTOMER,1,18)+lcCurrency
      
          *B603161,1 WAB -(END)
      
        CASE XTOT_NAME = 'R'
          *B802945,1 [Start] Read the reason code from the temp file
          *@ ROW,12 SAY SUBSTR(gfCodDes(XREASON,'REASON'),1,18)
      
          *B603161,1 WAB -(START) print the currency code in the sub total line in case of multi Currency
          *@ ROW,12 SAY SUBSTR(gfCodDes(REASON,'REASON'),1,18)
           
          lcCurrency = IIF(llMultCurr .AND. lcRpCurr = "F","Currency: "+SUBSTR(HBREAK,7,3),"")
          *B604062,1 KAM calling function with xreason instead of reason[start]
          *@ ROW,12 SAY SUBSTR(gfCodDes(REASON,'REASON'),1,18)+lcCurrency
          @ ROW,12 SAY SUBSTR(gfCodDes(XREASON,'REASON'),1,18)+lcCurrency 
          *B604062,1 KAM [end]
          *B603161,1 WAB -(END)
         
         *B802945,1 [End]
          
      ENDCASE  && end case breake
      *-- if printed both
      IF XBOTH_FLG

        *B603161,1 WAB -(START) Rearange the "R/a:" text position
        *@ ROW,48 SAY 'R/A :'

        @ ROW,44 SAY 'R/A:'

        *B603161,1 WAB -(END)

      ENDIF  && endif printed both
      SELECT &XNAME
      IF SIZEB = 'Y'        && print size breakdown
      
        *B605458,1 ABD - Print '1ST' in case we print the Received Return. [Begin] 
        IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
          @ ROW,44 SAY '1ST:'
        ENDIF  
        *B605458,1 ABD - [End]
        
        @ ROW,051-3 SAY XTOT(1,1) PICTURE '99999'
        @ ROW,057-3 SAY XTOT(1,2) PICTURE '99999'
        @ ROW,063-3 SAY XTOT(1,3) PICTURE '99999'
        @ ROW,069-3 SAY XTOT(1,4) PICTURE '99999'
        @ ROW,075-3 SAY XTOT(1,5) PICTURE '99999'
        @ ROW,081-3 SAY XTOT(1,6) PICTURE '99999'
        @ ROW,087-3 SAY XTOT(1,7) PICTURE '99999'
        @ ROW,093-3 SAY XTOT(1,8) PICTURE '99999'
        @ ROW,099-3 SAY XTOT(1,9) PICTURE '9999999'

        IF XAMOUNT = 'Y'

           *B603161,1 WAB -(START) add digit to the field picture 
           *@ ROW,106-3 SAY XTOT(1,10) PICTURE '9999999.99'
           *B603713,7 KAM 10/15/2000 increas the element which contain total [start]
           *@ ROW,107-3 SAY XTOT(1,10) PICTURE '99999999.99'
           @ ROW,107-3 SAY XTOT(1,10) PICTURE '99999999999.99' 
           *B603713,7 KAM [end]
           *B603161,1 WAB -(END) 

        *B605458,1 ABD - Print '2ND:' in case we receive 2nd Quality.  [Begin]
        IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
          *-- Print the 2nd Qty        
          ROW = Row + 1
          @ ROW,044 SAY '2ND:'
      
          
          @ ROW,051-3 SAY la2ndQty(1,1) PICTURE '99999'
          @ ROW,057-3 SAY la2ndQty(1,2) PICTURE '99999'
          @ ROW,063-3 SAY la2ndQty(1,3) PICTURE '99999'
          @ ROW,069-3 SAY la2ndQty(1,4) PICTURE '99999'
          @ ROW,075-3 SAY la2ndQty(1,5) PICTURE '99999'
          @ ROW,081-3 SAY la2ndQty(1,6) PICTURE '99999'
          @ ROW,087-3 SAY la2ndQty(1,7) PICTURE '99999'
          @ ROW,093-3 SAY la2ndQty(1,8) PICTURE '99999'
          @ ROW,099-3 SAY la2ndQty(1,9) PICTURE '9999999'
          
          IF XAMOUNT = 'Y'
            @ ROW,104 SAY la2ndQty(1,10) PICTURE '99999999999.99'
          ENDIF
          *-- Print the Damge Qty        
          ROW = Row + 1
          @ ROW,044 SAY 'DMG:'
          @ ROW,051-3 SAY laDmgdQty(1,1) PICTURE '99999'
          @ ROW,057-3 SAY laDmgdQty(1,2) PICTURE '99999'
          @ ROW,063-3 SAY laDmgdQty(1,3) PICTURE '99999'
          @ ROW,069-3 SAY laDmgdQty(1,4) PICTURE '99999'
          @ ROW,075-3 SAY laDmgdQty(1,5) PICTURE '99999'
          @ ROW,081-3 SAY laDmgdQty(1,6) PICTURE '99999'
          @ ROW,087-3 SAY laDmgdQty(1,7) PICTURE '99999'
          @ ROW,093-3 SAY laDmgdQty(1,8) PICTURE '99999'
          @ ROW,099-3 SAY laDmgdQty(1,9) PICTURE '9999999'

          IF XAMOUNT = 'Y'
            @ ROW,104 SAY laDmgdQty(1,10) PICTURE '99999999999.99'
          ENDIF
        ENDIF
        *B605458,1 ABD - [End]

        ENDIF
        *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *IF XREPORT = 'R'  .OR. XREPORT = 'B'
        *  @ ROW,117-3 SAY XTOT(1,11) PICTURE '9999999'
        *  IF XAMOUNT = 'Y'
        *    @ ROW,124-3 SAY XTOT(1,12) PICTURE '9999999.99'
        *  ENDIF
        *ENDIF
        *B603161,1 WAB -(END)

        STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
        STORE 0 TO XTOT(1,5),XTOT(1,6),XTOT(1,7),XTOT(1,8)
        STORE 0 TO XTOT(1,9),XTOT(1,10),XTOT(1,11),XTOT(1,12)

        *B605458,1 ABD - assign 0 to the array that Hold the 2nd and damage Qty. [Begin]
        STORE 0 TO la2ndQty(1,1) ,la2ndQty(1,2) ,la2ndQty(1,3) ,la2ndQty(1,4) ,la2ndQty(1,5)  ,;
                   la2ndQty(1,6) ,la2ndQty(1,7) ,la2ndQty(1,8) ,la2ndQty(1,9) ,la2ndQty(1,10) ,;
                   laDmgdQty(1,1),laDmgdQty(1,2),laDmgdQty(1,3),laDmgdQty(1,4),laDmgdQty(1,5) ,;
                   laDmgdQty(1,6),laDmgdQty(1,7),laDmgdQty(1,8),laDmgdQty(1,9),laDmgdQty(1,10)
        
        *B605458,1 ABD - [End]

        
        IF XBOTH_FLG
          ROW = ROW + 1

          *B603161,1 WAB -(START) Rearange the Rcv text position
          *@ ROW,37 SAY 'RCV :'
          *B605458,1 ABD - Print '1ST:' in case we receive 1ST Quality.[Begin]
          *@ ROW,44 SAY 'RCV:'
          @ ROW,44 SAY '1ST:'
          *B605458,1 ABD - [End]
          
          *B603161,1 WAB -(END)


          @ ROW,051-3 SAY XTOT_B(1,1) PICTURE '99999'
          @ ROW,057-3 SAY XTOT_B(1,2) PICTURE '99999'
          @ ROW,063-3 SAY XTOT_B(1,3) PICTURE '99999'
          @ ROW,069-3 SAY XTOT_B(1,4) PICTURE '99999'
          @ ROW,075-3 SAY XTOT_B(1,5) PICTURE '99999'
          @ ROW,081-3 SAY XTOT_B(1,6) PICTURE '99999'
          @ ROW,087-3 SAY XTOT_B(1,7) PICTURE '99999'
          @ ROW,093-3 SAY XTOT_B(1,8) PICTURE '99999'
          @ ROW,099-3 SAY XTOT_B(1,9) PICTURE '9999999'
          
          IF XAMOUNT = 'Y'
            *B603161,1 WAB -(START) add digit to the field picture 
            *@ ROW,106-3 SAY XTOT_B(1,10) PICTURE '9999999.99'
            *B603713,7 KAM 10/24/2000 increase the picture size[start]
            *@ ROW,107-3 SAY XTOT_B(1,10) PICTURE '99999999.99'
            @ ROW,107-3 SAY XTOT_B(1,10) PICTURE '99999999999.99'
            *B603713,7 KAM [end] 
            *B603161,1 WAB -(END)
          ENDIF
          
          
         *B605458,1 ABD - Print the 2nd quality qty. [Begin]
         *-- Print 2nd Q.
          ROW = ROW + 1
          @ ROW,44 SAY '2ND:'
          @ ROW,051-3 SAY la2ndQty(2,1) PICTURE '99999'
          @ ROW,057-3 SAY la2ndQty(2,2) PICTURE '99999'
          @ ROW,063-3 SAY la2ndQty(2,3) PICTURE '99999'
          @ ROW,069-3 SAY la2ndQty(2,4) PICTURE '99999'
          @ ROW,075-3 SAY la2ndQty(2,5) PICTURE '99999'
          @ ROW,081-3 SAY la2ndQty(2,6) PICTURE '99999'
          @ ROW,087-3 SAY la2ndQty(2,7) PICTURE '99999'
          @ ROW,093-3 SAY la2ndQty(2,8) PICTURE '99999'
          @ ROW,099-3 SAY la2ndQty(2,9) PICTURE '9999999'
          
          IF XAMOUNT = 'Y'
            @ ROW,107-3  SAY la2ndQty(2,10) PICTURE '99999999999.99'
          ENDIF
         
         *-- Print the damge Qt.
         ROW = ROW + 1
         @ ROW,44 SAY 'DMG:'
         
          @ ROW,051-3 SAY laDmgdQty(2,1) PICTURE '99999'
          @ ROW,057-3 SAY laDmgdQty(2,2) PICTURE '99999'
          @ ROW,063-3 SAY laDmgdQty(2,3) PICTURE '99999'
          @ ROW,069-3 SAY laDmgdQty(2,4) PICTURE '99999'
          @ ROW,075-3 SAY laDmgdQty(2,5) PICTURE '99999'
          @ ROW,081-3 SAY laDmgdQty(2,6) PICTURE '99999'
          @ ROW,087-3 SAY laDmgdQty(2,7) PICTURE '99999'
          @ ROW,093-3 SAY laDmgdQty(2,8) PICTURE '99999'
          @ ROW,099-3 SAY laDmgdQty(2,9) PICTURE '9999999'

          IF XAMOUNT = 'Y'
            @ ROW,107-3  SAY laDmgdQty(2,10) PICTURE '99999999999.99'
          ENDIF
          
          STORE 0 To la2ndQty(2,1) ,la2ndQty(2,2) ,la2ndQty(2,3) ,la2ndQty(2,4) ,la2ndQty(2,5)  ,;
                     la2ndQty(2,6) ,la2ndQty(2,7) ,la2ndQty(2,8) ,la2ndQty(2,9) ,la2ndQty(2,10) ,;
                     laDmgdQty(2,1),laDmgdQty(2,2),laDmgdQty(2,3),laDmgdQty(2,4),laDmgdQty(2,5) ,;
                     laDmgdQty(2,6),laDmgdQty(2,7),laDmgdQty(2,8),laDmgdQty(2,9),laDmgdQty(2,10)
          *B605458,1 ABD - [End]


          *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *@ ROW,117-3 SAY XTOT_B(1,11) PICTURE '9999999'
          *IF XAMOUNT = 'Y'
          *  @ ROW,124-3 SAY XTOT_B(1,12) PICTURE '9999999.99'
          *ENDIF            
          *B603161,1 WAB -(END)

          STORE 0 TO XTOT_B(1,1),XTOT_B(1,2),XTOT_B(1,3),XTOT_B(1,4)
          STORE 0 TO XTOT_B(1,5),XTOT_B(1,6),XTOT_B(1,7),XTOT_B(1,8)
          STORE 0 TO XTOT_B(1,9),XTOT_B(1,10),XTOT_B(1,11),XTOT_B(1,12)
        ENDIF
      ELSE  && else print size breakdown equal 'N'
        
        *B605458,1 ABD - Print '1ST:' in case we print receive qty.  [Begin]
        IF XREPORT $ 'R' .OR. (XREPORT $ 'A' .AND. !XBOTH_FLG)
          @ ROW,045 SAY '1ST:'
        ENDIF
        *B605458,1 ABD - [End]
        
        @ ROW,52-3 SAY XTOT(1,1) PICTURE '9999999'
        IF XAMOUNT= 'Y'
          *B603161,1 WAB -(START) add digit to the field picture 
          *@ ROW,61-3 SAY XTOT(1,2) PICTURE '9999999.99'
          *B603713,7 KAM 10/24/2000 increase the picture size[start]  
          *@ ROW,62-3 SAY XTOT(1,2) PICTURE '99999999.99'
          @ ROW,62-3 SAY XTOT(1,2) PICTURE '9999999999.99'
          *B603713,7 KAM [end] 
          *B603161,1 WAB -(END)
        ENDIF

        *B605458,1 ABD - Print Titel '2nd' and 2nd quantity. [Begin]
        IF XREPORT $ 'R' .OR. (XREPORT $ 'A' .AND. !XBOTH_FLG)
          ROW = ROW + 1
          @ ROW,045 SAY '2ND:'
          @ ROW,52-3 SAY la2ndQty(1,1) PICTURE '9999999'
          IF XAMOUNT= 'Y'          
            @ ROW,62-3 SAY la2ndQty(1,2) PICTURE '9999999999.99'
          ENDIF
          
          ROW = ROW + 1
          @ ROW,045 SAY 'DMG:'
          @ ROW,52-3 SAY laDmgdQty(1,1) PICTURE '9999999'
          IF XAMOUNT= 'Y'          
            @ ROW,62-3 SAY laDmgdQty(1,2) PICTURE '9999999999.99'
          ENDIF
       ENDIF
       *B605458,1 ABD - [End]
        *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *IF XREPORT = 'R'  .OR. XREPORT = 'B'
        *  @ ROW,73-3 SAY XTOT(1,3) PICTURE '9999999'
        *  IF XAMOUNT = 'Y'
        *    @ ROW,82-3 SAY XTOT(1,4) PICTURE '999999.99'
        *  ENDIF
        *ENDIF
        *B603161,1 WAB -(END) 

        STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
        
        *B605458,1 ABD - Assign zero to the arrays. [Begin]
        STORE 0 TO la2ndQty(1,1) , la2ndQty(1,2) , laDmgdQty(1,1), laDmgdQty(1,2)
        *B605458,1 ABD - [End]
        
        IF XBOTH_FLG
          ROW = ROW + 1

          *B603161,1 WAB -(START) Rearange the RCV: text position
          *@ ROW,37-3 SAY 'RCV :'
          *@ ROW ,44 SAY 'RCV:'
          @ ROW,44 SAY '1ST:'
          *B603161,1 WAB -(END) 

          @ ROW,52-3 SAY XTOT_B(1,1) PICTURE '9999999'
          IF XAMOUNT= 'Y'
            *B603161,1 WAB -(START) add digit to the field picture 
            *@ ROW,61-3 SAY XTOT_B(1,2) PICTURE '9999999.99'
            *B603713,7 KAM 10/24/2000 increase the picture size[start] 
            *@ ROW,62-3 SAY XTOT_B(1,2) PICTURE '99999999.99'
            @ ROW,62-3 SAY XTOT_B(1,2) PICTURE '9999999999.99'
            *B603713,7 KAM [end] 
            *B603161,1 WAB -(END) 
          ENDIF

          *B605458,1 ABD - Print the 2ns and damge Qty. [Begin]
          IF XREPORT $ 'A'
            ROW = ROW + 1
            @ ROW,044 SAY '2ND:'
            @ ROW,52-3 SAY la2ndQty(2,1) PICTURE '9999999'
            IF XAMOUNT= 'Y'          
              @ ROW,62-3 SAY la2ndQty(2,2) PICTURE '9999999999.99'
            ENDIF
          
            ROW = ROW + 1
            @ ROW,044 SAY 'DMG:'
            @ ROW,52-3 SAY laDmgdQty(2,1) PICTURE '9999999'
            IF XAMOUNT= 'Y'          
              @ ROW,62-3 SAY laDmgdQty(2,2) PICTURE '9999999999.99'
            ENDIF
          ENDIF
          *B605458,1 ABD - [End]
           
          *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *@ ROW,73-3 SAY XTOT_B(1,3) PICTURE '9999999'
          *IF XAMOUNT = 'Y'
          *  @ ROW,82-3 SAY XTOT_B(1,4) PICTURE '999999.99'
          *ENDIF
          *B603161,1 WAB -(END) 


          STORE 0 TO XTOT_B(1,1),XTOT_B(1,2),XTOT_B(1,3),XTOT_B(1,4)
          *B605458,1 ABD - assign zero to the arrys that hold the 2nd and damge Qty. [Begin]
          STORE 0 TO la2ndQty(2,1),la2ndQty(2,2),laDmgdQty(2,1) ,laDmgdQty(2,2) 
          *B605458,1 ABD - [End]
        ENDIF
      ENDIF
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',R_LEN)
      ROW = ROW + 1
      SELECT &XNAME
      *B603161,1 WAB - we remove !EOF() from the loop condition to print last subtotal
      *B603161,1 WAB -(START) so we must check if it eof() to exit from loop
      IF EOF()
        EXIT
      ENDIF 
      *B603161,1 WAB -(END) 
      HBREAK=&BREAK
      XCUSTOMER = CUSTOMER.STNAME
    ENDIF
    IF XBOTH_FLG
      IF EMPTY(TYPE)
        XTYPE = 'R/A'
        XSUB  = .T.
        XNO   = 'RANO'
      ELSE
        XTYPE = 'RCV'
        XNO   = 'CRMEMO'
        XSUB  = .F.
      ENDIF
    ENDIF
    *B604062,1 KAM add assignment xreason variable[start]
    XREASon=REASON
    *B604062,1 [end]
            
    *-- group by style nonmajor
    *-- if another nonmajor for that style
 
    *B603161,1 WAB -(START) print style also in case if the currency code is changed in case of multi currency
    *IF X_STYLE+X_COLOR <> SUBSTR(STYLE,1,lnMajSize)+RIGHT(STYLE,lnNonSegSize)

    IF X_STYLE+X_COLOR <> SUBSTR(STYLE,1,lnMajSize)+RIGHT(STYLE,lnNonSegSize) .OR. ;
      ( llMultCurr .AND. lcRpCurr = "F" .AND. lcCurrCode <> cCurrCode )

      lcCurrCode = IIF(llMultCurr,cCurrCode,"")			&& get the new cCurrCode
     
    *B603161,1 WAB -(END)
    
      @ ROW,00 SAY STYLE PICT '!!!!!!!!!!!!!!!!!!!'
      X_STYLE  = SUBSTR(STYLE,1,lnMajSize)
      X_COLOR  = RIGHT(STYLE,lnNonSegSize)
    ENDIF  && endif another nonmajor for that style
    @ ROW,20 SAY ACCOUNT
    *--E301643,1 mhm Change status From Both to ALL[Start]
    *IF XREPORT = 'B'
    *B605458,1 ABD - Print Titel for printed recored like 2nd in case we print
    *B605458,1 ABD -  2nd quality.. [Begin]
    *IF XREPORT = 'A'
    *--E301643,1 mhm  [Start]
    *@ ROW,26 SAY XTYPE
    IF XREPORT $ 'RA'
      DO CASE
      CASE cStygrade = '1'
        @ ROW,26 SAY 'RCV'
      CASE cStygrade = '2'
        @ ROW,26 SAY '2ND'
      CASE cStygrade = '3'
        @ ROW,26 SAY 'DMG'
      CASE EMPTY(cStygrade)
          @ ROW,26 SAY XTYPE
      ENDCASE
      *B605458,1 ABD - [End]
    ENDIF
    
    *B605458,1 ABD - remark the next line and change the position.[Begin]
    *@ ROW,30 SAY &XNO
    *@ ROW,37 SAY DYELOT    
    @ ROW,31 SAY &XNO
    @ ROW,39 SAY ALLTRIM(DYELOT)
    *B605458,1 ABD - [End]

    SELECT &XNAME
    *-- if print size breakedown
    IF SIZEB = 'Y'
      @ ROW,051-3 SAY QTY1 PICTURE '99999'
      @ ROW,057-3 SAY QTY2 PICTURE '99999'
      @ ROW,063-3 SAY QTY3 PICTURE '99999'
      @ ROW,069-3 SAY QTY4 PICTURE '99999'
      @ ROW,075-3 SAY QTY5 PICTURE '99999'
      @ ROW,081-3 SAY QTY6 PICTURE '99999'
      @ ROW,087-3 SAY QTY7 PICTURE '99999'
      @ ROW,093-3 SAY QTY8 PICTURE '99999'
      @ ROW,099-3 SAY TOTQTY PICTURE '9999999'

      IF XAMOUNT = 'Y'

        *B603161,1 WAB -(START) print the amount by the equivalent  amount in case of multi currency
        *@ ROW,106-3 SAY TOTQTY*PRICE PICTURE '9999999.99'

        IF llMultCurr .AND. lcRpCurr <> "F"
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field 
          *@ ROW,107-3 SAY lfBaseAmt(TOTQTY*PRICE) PICTURE '99999999.99'
          @ ROW,107-3 SAY lfBaseAmt(TOTQTY*PRICE) PICTURE '9999999999.99'
          *B603713,7 KAM [end] 
        ELSE
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field 
          *@ ROW,107-3 SAY TOTQTY*PRICE PICTURE '99999999.99'
          @ ROW,107-3 SAY TOTQTY*PRICE PICTURE '9999999999.99' 
          *B603713,7 KAM [end] 
        ENDIF
        *B603161,1 WAB -(END) 

      ENDIF
      *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
      *IF XREPORT = 'R' .OR. (XREPORT = 'B' .AND. XTYPE ='RCV')
      *  @ ROW,117-3 SAY DMGQTY PICTURE '9999999'
      *   IF XAMOUNT = 'Y'
      *     @ ROW,124-3 SAY DMGQTY*PRICE PICTURE '9999999.99'
      *   ENDIF
      *ENDIF
      *B603161,1 WAB -(END) 

      *B605458,1 ABD - Calculate the grand total to print at the end of the report. [Begin]
      IF XREPORT $ 'AR' .AND. !EMPTY(cStygrade)
        DO CASE
          CASE cStygrade = '1'
            laGntTotal(1,1) = laGntTotal(1,1) + QTY1
            laGntTotal(1,2) = laGntTotal(1,2) + QTY2
            laGntTotal(1,3) = laGntTotal(1,3) + QTY3
            laGntTotal(1,4) = laGntTotal(1,4) + QTY4
            laGntTotal(1,5) = laGntTotal(1,5) + QTY5
            laGntTotal(1,6) = laGntTotal(1,6) + QTY6
            laGntTotal(1,7) = laGntTotal(1,7) + QTY7
            laGntTotal(1,8) = laGntTotal(1,8) + QTY8
            laGntTotal(1,9) = laGntTotal(1,9) + TotQty
            
          CASE cStygrade = '2'
            laGntTotal(2,1) = laGntTotal(2,1) + QTY1
            laGntTotal(2,2) = laGntTotal(2,2) + QTY2
            laGntTotal(2,3) = laGntTotal(2,3) + QTY3
            laGntTotal(2,4) = laGntTotal(2,4) + QTY4
            laGntTotal(2,5) = laGntTotal(2,5) + QTY5
            laGntTotal(2,6) = laGntTotal(2,6) + QTY6
            laGntTotal(2,7) = laGntTotal(2,7) + QTY7
            laGntTotal(2,8) = laGntTotal(2,8) + QTY8
            laGntTotal(2,9) = laGntTotal(2,9) + TotQty

          CASE cStygrade = '3'
            laGntTotal(3,1) = laGntTotal(3,1) + QTY1
            laGntTotal(3,2) = laGntTotal(3,2) + QTY2
            laGntTotal(3,3) = laGntTotal(3,3) + QTY3
            laGntTotal(3,4) = laGntTotal(3,4) + QTY4
            laGntTotal(3,5) = laGntTotal(3,5) + QTY5
            laGntTotal(3,6) = laGntTotal(3,6) + QTY6
            laGntTotal(3,7) = laGntTotal(3,7) + QTY7
            laGntTotal(3,8) = laGntTotal(3,8) + QTY8
            laGntTotal(3,9) = laGntTotal(3,9) + TotQty
        ENDCASE
      ENDIF

      IF XREPORT $ 'AR' .AND. !EMPTY(cStygrade)
        IF llMultCurr .AND. lcRpCurr <> "F"
          DO CASE
            CASE cStygrade = '1'
              laGntTotal(1,10) = laGntTotal(1,10)  + lfBaseAmt(TOTQTY * PRICE) 
            CASE cStygrade = '2'
              laGntTotal(2,10) = laGntTotal(2,10)  + lfBaseAmt(TOTQTY * PRICE) 
            CASE cStygrade = '3'
              laGntTotal(3,10) = laGntTotal(3,10)  + lfBaseAmt(TOTQTY * PRICE) 
            ENDCASE  
         ELSE
           DO CASE
             CASE cStygrade = '1'
               laGntTotal(1,10) = laGntTotal(1,10)  + TOTQTY * PRICE
             CASE cStygrade = '2'
               laGntTotal(2,10) = laGntTotal(2,10)  + TOTQTY * PRICE
             CASE cStygrade = '3'
               laGntTotal(3,10) = laGntTotal(3,10)  + TOTQTY * PRICE
            ENDCASE  
         ENDIF
      ENDIF           
      *B605458,1 ABD - [End]
      
      IF XSUB
        *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
        *XTOT(1,1)  = XTOT(1,1)  + QTY1
        *XTOT(1,2)  = XTOT(1,2)  + QTY2
        *XTOT(1,3)  = XTOT(1,3)  + QTY3
        *XTOT(1,4)  = XTOT(1,4)  + QTY4
        *XTOT(1,5)  = XTOT(1,5)  + QTY5
        *XTOT(1,6)  = XTOT(1,6)  + QTY6
        *XTOT(1,7)  = XTOT(1,7)  + QTY7
        *XTOT(1,8)  = XTOT(1,8)  + QTY8
        *XTOT(1,9)  = XTOT(1,9)  + TOTQTY
        DO CASE
          CASE XREPORT = 'O' .OR. (cStygrade = '1' .OR.  EMPTY(cStygrade))
           *B605458,1 ABD - [End]
            XTOT(1,1)  = XTOT(1,1)  + QTY1
            XTOT(1,2)  = XTOT(1,2)  + QTY2
            XTOT(1,3)  = XTOT(1,3)  + QTY3
            XTOT(1,4)  = XTOT(1,4)  + QTY4
            XTOT(1,5)  = XTOT(1,5)  + QTY5
            XTOT(1,6)  = XTOT(1,6)  + QTY6
            XTOT(1,7)  = XTOT(1,7)  + QTY7
            XTOT(1,8)  = XTOT(1,8)  + QTY8
            XTOT(1,9)  = XTOT(1,9)  + TOTQTY
          *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
        CASE cStygrade = '2'
            *-- la2ndQty 
          la2ndQty(1,1) = la2ndQty(1,1) + QTY1
          la2ndQty(1,2) = la2ndQty(1,2) + QTY2
          la2ndQty(1,3) = la2ndQty(1,3) + QTY3
          la2ndQty(1,4) = la2ndQty(1,4) + QTY4
          la2ndQty(1,5) = la2ndQty(1,5) + QTY5
          la2ndQty(1,6) = la2ndQty(1,6) + QTY6
          la2ndQty(1,7) = la2ndQty(1,7) + QTY7
          la2ndQty(1,8) = la2ndQty(1,8) + QTY8
          la2ndQty(1,9) = la2ndQty(1,9) + TOTQTY

        
        CASE cStygrade = '3'
            *-- laDmgdQty
          laDmgdQty(1,1) = laDmgdQty(1,1) + QTY1
          laDmgdQty(1,2) = laDmgdQty(1,2) + QTY2 
          laDmgdQty(1,3) = laDmgdQty(1,3) + QTY3 
          laDmgdQty(1,4) = laDmgdQty(1,4) + QTY4 
          laDmgdQty(1,5) = laDmgdQty(1,5) + QTY5 
          laDmgdQty(1,6) = laDmgdQty(1,6) + QTY6 
          laDmgdQty(1,7) = laDmgdQty(1,7) + QTY7 
          laDmgdQty(1,8) = laDmgdQty(1,8) + QTY8 
          laDmgdQty(1,9) = laDmgdQty(1,9) + TotQty
        ENDCASE
        *B605458,1 ABD - [END]
        
        *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *XTOT(1,11) = XTOT(1,11) + DMGQTY
        *B603161,1 WAB -(END) 

        IF XAMOUNT = 'Y'
          *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
          *B603161,1 WAB -(START) 
          *XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE
          *XTOT(1,12) = XTOT(1,12) + DMGQTY * PRICE
 
          IF llMultCurr .AND. lcRpCurr <> "F"
           
            *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
            *XTOT(1,10) = XTOT(1,10) + lfBaseAmt(TOTQTY * PRICE)             
            IF XREPORT $ 'AR'
              DO CASE
                CASE cStygrade = '1' .OR.  EMPTY(cStygrade)
                  XTOT(1,10)      = XTOT(1,10)     + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '2'
                  la2ndQty (1,10) = la2ndQty(1,10) + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '3'
                  laDmgdQty(1,10) = laDmgdQty(1,10)+ lfBaseAmt(TOTQTY * PRICE) 
              ENDCASE
            ELSE
              XTOT(1,10)          = XTOT(1,10)     + lfBaseAmt(TOTQTY * PRICE)
            ENDIF  
            *B605458,1 ABD - [End]
            
          ELSE
            *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
            *XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE 
            IF XREPORT $ 'AR'
              DO CASE
                CASE cStygrade = '1' .OR.  EMPTY(cStygrade)
                  XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE
                CASE  cStygrade = '2'
                  la2ndQty (1,10) = la2ndQty(1,10) + TOTQTY * PRICE 
                CASE  cStygrade = '3'
                  laDmgdQty(1,10) = laDmgdQty(1,10)+ TOTQTY * PRICE
              ENDCASE
            ELSE
              *B605458,1 ABD - [End]
              XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE
              *B605458,1 ABD - End if for if statment.[Begin]              
            ENDIF
            *B605458,1 ABD - [End]
                      
          ENDIF
          *B603161,1 WAB -(END) 
 
        ENDIF
        XTOT(2,1)  = XTOT(2,1)  + QTY1
        XTOT(2,2)  = XTOT(2,2)  + QTY2
        XTOT(2,3)  = XTOT(2,3)  + QTY3
        XTOT(2,4)  = XTOT(2,4)  + QTY4
        XTOT(2,5)  = XTOT(2,5)  + QTY5
        XTOT(2,6)  = XTOT(2,6)  + QTY6
        XTOT(2,7)  = XTOT(2,7)  + QTY7
        XTOT(2,8)  = XTOT(2,8)  + QTY8
        XTOT(2,9)  = XTOT(2,9)  + TOTQTY
        
        *B603161,1 WAB -(START) Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *XTOT(2,11) = XTOT(2,11) + DMGQTY
        *B603161,1 WAB -(END) 
        
        IF XAMOUNT = 'Y'

          *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
          *B603161,1 WAB -(START) 
          *XTOT(2,10) = XTOT(2,10) + TOTQTY * PRICE
          *XTOT(2,12) = XTOT(2,12) + DMGQTY * PRICE          

          IF llMultCurr .AND. lcRpCurr <> "F"
            XTOT(2,10) = XTOT(2,10) + lfBaseAmt(TOTQTY * PRICE)
          ELSE
            XTOT(2,10) = XTOT(2,10) + TOTQTY * PRICE
          ENDIF
          *B603161,1 WAB -(END)
          
        ENDIF
      ENDIF
      
      IF XBOTH_FLG
        IF !EMPTY(TYPE)
          *B605458,1 ABD - Accumulate on the type. [Begin]
          DO CASE
            CASE cStygrade = '1'
              *B605458,1 ABD - [End]
              XTOT_B(1,1)  = XTOT_B(1,1)  + QTY1
              XTOT_B(1,2)  = XTOT_B(1,2)  + QTY2
              XTOT_B(1,3)  = XTOT_B(1,3)  + QTY3
              XTOT_B(1,4)  = XTOT_B(1,4)  + QTY4
              XTOT_B(1,5)  = XTOT_B(1,5)  + QTY5
              XTOT_B(1,6)  = XTOT_B(1,6)  + QTY6
              XTOT_B(1,7)  = XTOT_B(1,7)  + QTY7
              XTOT_B(1,8)  = XTOT_B(1,8)  + QTY8
              XTOT_B(1,9)  = XTOT_B(1,9)  + TOTQTY
              *B605458,1 ABD - Accumulate on the type. [Begin]
            CASE cStygrade = '2'
              *-- la2ndQty 
              la2ndQty(2,1) = la2ndQty(2,1) + QTY1
              la2ndQty(2,2) = la2ndQty(2,2) + QTY2
              la2ndQty(2,3) = la2ndQty(2,3) + QTY3
              la2ndQty(2,4) = la2ndQty(2,4) + QTY4
              la2ndQty(2,5) = la2ndQty(2,5) + QTY5
              la2ndQty(2,6) = la2ndQty(2,6) + QTY6
              la2ndQty(2,7) = la2ndQty(2,7) + QTY7
              la2ndQty(2,8) = la2ndQty(2,8) + QTY8
              la2ndQty(2,9) = la2ndQty(2,9) + TOTQTY

            CASE cStygrade = '3'
              *-- laDmgdQty
              laDmgdQty(2,1) = laDmgdQty(2,1) + QTY1
              laDmgdQty(2,2) = laDmgdQty(2,2) + QTY2 
              laDmgdQty(2,3) = laDmgdQty(2,3) + QTY3 
              laDmgdQty(2,4) = laDmgdQty(2,4) + QTY4 
              laDmgdQty(2,5) = laDmgdQty(2,5) + QTY5 
              laDmgdQty(2,6) = laDmgdQty(2,6) + QTY6 
              laDmgdQty(2,7) = laDmgdQty(2,7) + QTY7 
              laDmgdQty(2,8) = laDmgdQty(2,8) + QTY8 
              laDmgdQty(2,9) = laDmgdQty(2,9) + TotQty
          ENDCASE
          *B605458,1 ABD - [End]


          *B603161,1 WAB -(START)-Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *XTOT_B(1,11) = XTOT_B(1,11) + DMGQTY
          *B603161,1 WAB -(END) 

          IF XAMOUNT = 'Y'

            *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
            *B603161,1 WAB -(START) 
            *XTOT_B(1,10) = XTOT_B(1,10) + TOTQTY * PRICE
            *XTOT_B(1,12) = XTOT_B(1,12) + DMGQTY * PRICE
      
            IF llMultCurr .AND. lcRpCurr <> "F"
              *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
              *XTOT_B(1,10) = XTOT_B(1,10) + lfBaseAmt(TOTQTY * PRICE)
              DO CASE
                CASE cStygrade = '1'
                  XTOT_B(1,10)    = XTOT_B(1,10)    + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '2'
                  la2ndQty(2,10)  = la2ndQty(2,10)  + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '3'
                  laDmgdQty(2,10) = laDmgdQty(2,10) + lfBaseAmt(TOTQTY * PRICE) 
              ENDCASE  
              *B605458,1 ABD - [End]
              
            ELSE
              *B605458,1 ABD - Accumulate on the type. [Begin]
              XTOT_B(1,10) = XTOT_B(1,10) + TOTQTY * PRICE
              DO CASE
                CASE cStygrade = '1'
                  XTOT_B(1,10) = XTOT_B(1,10) + TOTQTY * PRICE
                CASE cStygrade = '2'
                  la2ndQty(2,10) = la2ndQty(2,10) + TOTQTY * PRICE
                CASE cStygrade = '3'
                  laDmgdQty(2,10) = laDmgdQty(2,10) + TOTQTY * PRICE
              ENDCASE
              *B605458,1 ABD - [End]
              
            ENDIF
            *B603161,1 WAB -(END) 
      
          ENDIF
          XTOT_B(2,1)  = XTOT_B(2,1)  + QTY1
          XTOT_B(2,2)  = XTOT_B(2,2)  + QTY2
          XTOT_B(2,3)  = XTOT_B(2,3)  + QTY3
          XTOT_B(2,4)  = XTOT_B(2,4)  + QTY4
          XTOT_B(2,5)  = XTOT_B(2,5)  + QTY5
          XTOT_B(2,6)  = XTOT_B(2,6)  + QTY6
          XTOT_B(2,7)  = XTOT_B(2,7)  + QTY7
          XTOT_B(2,8)  = XTOT_B(2,8)  + QTY8
          XTOT_B(2,9)  = XTOT_B(2,9)  + TOTQTY
          
          
          *B603161,1 WAB -(START)-Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *XTOT_B(2,11) = XTOT_B(2,11) + DMGQTY
          *B603161,1 WAB -(END) 

          IF XAMOUNT = 'Y'
            *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
            *B603161,1 WAB -(START) 
            *XTOT_B(2,10) = XTOT_B(2,10) + TOTQTY * PRICE
            *XTOT_B(2,12) = XTOT_B(2,12) + DMGQTY * PRICE
            IF llMultCurr .AND. lcRpCurr <> "F"
              XTOT_B(2,10) = XTOT_B(2,10) + lfBaseAmt(TOTQTY * PRICE)
            ELSE
              XTOT_B(2,10) = XTOT_B(2,10) + TOTQTY * PRICE
            ENDIF
            *B603161,1 WAB -(START) 
          ENDIF
        ENDIF
      ENDIF
    ELSE  && else if print size breakedown equal 'N'
      @ ROW,52-3 SAY TOTQTY PICTURE '9999999'
     
      *B605458,1 ABD - Accumulate grand total on the type. [Begin]
      IF XREPORT $ 'RA' .AND. !EMPTY(cStygrade)
        DO CASE
          CASE cStygrade = '1'
            laGntTotal(1,1) = laGntTotal(1,1) + TotQty
          CASE cStygrade = '2'
            laGntTotal(2,1) = laGntTotal(2,1) + TotQty
          CASE cStygrade = '3'
            laGntTotal(3,1) = laGntTotal(3,1) + TotQty
        ENDCASE    
      ENDIF  
      IF XREPORT $ 'AR' .AND. !EMPTY(cStygrade)
        IF llMultCurr .AND. lcRpCurr <> "F"
          DO CASE
            CASE cStygrade = '1'
              laGntTotal(1,2) = laGntTotal(1,2)  + lfBaseAmt(TOTQTY * PRICE) 
            CASE cStygrade = '2'
              laGntTotal(2,2) = laGntTotal(2,2)  + lfBaseAmt(TOTQTY * PRICE) 
            CASE cStygrade = '3'
              laGntTotal(3,2) = laGntTotal(3,2)  + lfBaseAmt(TOTQTY * PRICE) 
            ENDCASE  
        ELSE
          DO CASE
            CASE cStygrade = '1'
              laGntTotal(1,2) = laGntTotal(1,2)  + TOTQTY * PRICE
            CASE cStygrade = '2'
              laGntTotal(2,2) = laGntTotal(2,2)  + TOTQTY * PRICE
            CASE cStygrade = '3'
              laGntTotal(3,2) = laGntTotal(3,2)  + TOTQTY * PRICE
            ENDCASE  
        ENDIF
      ENDIF  
      *B605458,1 ABD - [End]

      
      IF XAMOUNT = 'Y'
        *B603161,1 WAB -print amount by the equivalent amount in case of multi currency
        *B603161,1 WAB -(START)- & add digit to the field picture
        *@ ROW,61-3 SAY TOTQTY*PRICE PICTURE '9999999.99'

        IF llMultCurr .AND. lcRpCurr <> "F"
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field  [start]
          *@ ROW,62-3 SAY lfBaseAmt(TOTQTY * PRICE) PICTURE '99999999.99'
           @ ROW,62-3 SAY lfBaseAmt(TOTQTY * PRICE) PICTURE '9999999999.99'
          *B603713,7 KAM [end]
        ELSE
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field  [start]
          *@ ROW,62-3 SAY TOTQTY*PRICE PICTURE '99999999.99'
          @ ROW,62-3 SAY TOTQTY*PRICE PICTURE '9999999999.99' 
          *B603713,7 KAM [end]
        ENDIF
        *B603161,1 WAB -(END) 
        
      ENDIF

      *B603161,1 WAB -(START)-Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
      *IF XREPORT = 'R' .OR. (XREPORT = 'B' .AND. XTYPE = 'RCV')
      *  @ ROW,73-3 SAY DMGQTY PICTURE '9999999'
      *   IF XAMOUNT = 'Y'
      *     @ ROW,82-3 SAY DMGQTY*PRICE PICTURE '999999.99'
      *  ENDIF
      *ENDIF
      *B603161,1 WAB -(END)
      
      IF XSUB
        *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
        *XTOT(1,1) = XTOT(1,1) + TOTQTY
        *XTOT(2,1) = XTOT(2,1) + TOTQTY
        XTOT(2,1) = XTOT(2,1) + TOTQTY
        IF XREPORT $ 'RA'
          DO CASE
            CASE cStygrade = '1' .OR. EMPTY(cStygrade)
              XTOT(1,1) = XTOT(1,1) + TOTQTY
            CASE cStygrade = '2'
              la2ndQty(1,1)  = la2ndQty(1,1) + TOTQTY
              la2ndQty(2,1)  = la2ndQty(2,1) + TOTQTY
            CASE cStygrade = '3'
              laDmgdQty(1,1) = laDmgdQty(1,1)+ TOTQTY
              laDmgdQty(2,1) = laDmgdQty(2,1)+ TOTQTY
          ENDCASE    
        ELSE
          XTOT(1,1) = XTOT(1,1) + TOTQTY
        ENDIF  
        *B605458,1 ABD - [End]
        
        

        *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *XTOT(1,3) = XTOT(1,3) + DMGQTY
        *XTOT(2,3) = XTOT(2,3) + DMGQTY
        *B603161,1 WAB -(END)



        IF XAMOUNT = 'Y'

          *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
          *B603161,1 WAB -(START) 
          *XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
          *XTOT(1,4) = XTOT(1,4) + DMGQTY * PRICE
          *XTOT(2,2) = XTOT(2,2) + TOTQTY * PRICE
          *XTOT(2,4) = XTOT(2,4) + DMGQTY * PRICE

          IF llMultCurr .AND. lcRpCurr <> "F"
            *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
            *XTOT(1,2) = XTOT(1,2) +  lfBaseAmt(TOTQTY * PRICE)
            *XTOT(2,2) = XTOT(2,2) +  lfBaseAmt(TOTQTY * PRICE) 

            XTOT(2,2) = XTOT(2,2) + lfBaseAmt(TOTQTY * PRICE)
            IF XREPORT $ 'RA'
              DO CASE
                CASE cStygrade = '1' .OR. EMPTY(cStygrade)
                  XTOT(1,2) = XTOT(1,2) + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '2'
                  la2ndQty(1,2)  = la2ndQty(1,2) + lfBaseAmt(TOTQTY * PRICE) 
                  la2ndQty(2,2)  = la2ndQty(2,2) + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '3'
                  laDmgdQty(1,2) = laDmgdQty(1,2)+ lfBaseAmt(TOTQTY * PRICE) 
                  laDmgdQty(2,2) = laDmgdQty(2,2)+ lfBaseAmt(TOTQTY * PRICE) 
              ENDCASE
            ELSE
              XTOT(1,2) = XTOT(1,2) + lfBaseAmt(TOTQTY * PRICE) 
            ENDIF
            *B605458,1 ABD - [End]
          ELSE
            
            *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
            *XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
            *XTOT(2,2) = XTOT(2,2) + TOTQTY * PRICE
            XTOT(2,2) = XTOT(2,2) + TOTQTY * PRICE
            IF XREPORT $ 'RA'
              DO CASE
                CASE cStygrade = '1'  .OR. EMPTY(cStygrade)
                  *B605458,1 ABD - [End]
                  XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
                  *B605458,1 ABD -  Accumulate on the type. [Begin]
                CASE cStygrade = '2'
                  la2ndQty(1,2)  = la2ndQty(1,2) + TOTQTY * PRICE
                  la2ndQty(2,2)  = la2ndQty(2,2) + TOTQTY * PRICE
                CASE cStygrade = '3'
                  laDmgdQty(1,2) = laDmgdQty(1,2)+ TOTQTY * PRICE
                  laDmgdQty(2,2) = laDmgdQty(2,2)+ TOTQTY * PRICE
              ENDCASE
            ELSE
              XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
            ENDIF
            *B605458,1 ABD - [End]

            
          ENDIF
          *B603161,1 WAB -(END)

        ENDIF
      ENDIF
      IF XBOTH_FLG
        IF !EMPTY(TYPE)
          *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
          *XTOT_B(1,1) = XTOT_B(1,1) + TOTQTY
          *XTOT_B(2,1) = XTOT_B(2,1) + TOTQTY
          XTOT_B(2,1) = XTOT_B(2,1) + TOTQTY

          IF XREPORT $ 'RA'
            DO CASE
              CASE cStygrade = '1' .OR. EMPTY(cStygrade)
                XTOT_B(1,1) = XTOT_B(1,1) + TOTQTY
              CASE cStygrade = '2'
                la2ndQty(1,1)  = la2ndQty(1,1) + TOTQTY
                la2ndQty(2,1)  = la2ndQty(2,1) + TOTQTY
              CASE cStygrade = '3'
                laDmgdQty(1,1) = laDmgdQty(1,1)+ TOTQTY
                laDmgdQty(2,1) = laDmgdQty(2,1)+ TOTQTY
            ENDCASE
          ELSE
            XTOT_B(1,1) = XTOT_B(1,1) + TOTQTY  
          ENDIF
          *B605458,1 ABD - [End]
  
          *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *XTOT_B(1,3) = XTOT_B(1,3) + DMGQTY
          *XTOT_B(2,3) = XTOT_B(2,3) + DMGQTY
          *B603161,1 WAB -(END)

          IF XAMOUNT = 'Y'

            *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
            *B603161,1 WAB -(START) 
            *XTOT_B(1,2) = XTOT_B(1,2) + TOTQTY * PRICE
            *XTOT_B(1,4) = XTOT_B(1,4) + DMGQTY * PRICE
            *XTOT_B(2,2) = XTOT_B(2,2) + TOTQTY * PRICE
            *XTOT_B(2,4) = XTOT_B(2,4) + DMGQTY * PRICE


            IF llMultCurr .AND. lcRpCurr <> "F"
              *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
              *XTOT_B(1,2) = XTOT_B(1,2) + lfBaseAmt(TOTQTY * PRICE) 
              XTOT_B(2,2) = XTOT_B(2,2) + lfBaseAmt(TOTQTY * PRICE) 
              IF XREPORT $ 'RA'
                DO CASE
                  CASE cStygrade = '1'  .OR. EMPTY(cStygrade)
                    XTOT_B(1,2) = XTOT_B(1,2) + lfBaseAmt(TOTQTY * PRICE)
                  CASE cStygrade = '2'
                    la2ndQty(1,2)  = la2ndQty(1,2) + lfBaseAmt(TOTQTY * PRICE)
                    la2ndQty(2,2)  = la2ndQty(2,2) + lfBaseAmt(TOTQTY * PRICE)
                  CASE cStygrade = '3'
                    laDmgdQty(1,2) = laDmgdQty(1,2)+ lfBaseAmt(TOTQTY * PRICE)
                    laDmgdQty(2,2) = laDmgdQty(2,2)+ lfBaseAmt(TOTQTY * PRICE)
                 ENDCASE
               ELSE
                 XTOT_B(1,2) = XTOT_B(1,2) + lfBaseAmt(TOTQTY * PRICE)
              ENDIF  
              *B605458,1 ABD - [End]
            ELSE
              *B605458,1 ABD - Remark the next line and Accumulate on the type. [Begin]
              *XTOT_B(1,2) = XTOT_B(1,2) + TOTQTY * PRICE
              XTOT_B(2,2) = XTOT_B(2,2) + TOTQTY * PRICE

              IF XREPORT $ 'RA'
                DO CASE
                  CASE cStygrade = '1'  .OR. EMPTY(cStygrade)
                    XTOT_B(1,2) = XTOT_B(1,2) + TOTQTY * PRICE
                  CASE cStygrade = '2'
                    la2ndQty(1,2)  = la2ndQty(1,2) + TOTQTY * PRICE
                    la2ndQty(2,2)  = la2ndQty(2,2) + TOTQTY * PRICE
                  CASE cStygrade = '3'
                    laDmgdQty(1,2) = laDmgdQty(1,2)+ TOTQTY * PRICE
                    laDmgdQty(2,2) = laDmgdQty(2,2)+ TOTQTY * PRICE
                 ENDCASE
               ELSE
                 XTOT_B(1,2) = XTOT_B(1,2) + lfBaseAmt(TOTQTY * PRICE)
              ENDIF  
              
              *B605458,1 ABD - [End]
            ENDIF
            *B603161,1 WAB -(END)

          ENDIF
        ENDIF
      ENDIF
    ENDIF  && endif print size breakedown
    ROW = ROW + 1
    SELECT &XNAME
    SKIP
  ENDDO
ELSE  && no dyelot
  *B603161,1 WAB - (START) remove !EOF() from the loop condition to print last subtotal
  *DO WHILE !EOF() .AND. INKEY() <> 32
  DO WHILE INKEY() <> 32
  *B603161,1 WAB -(END)
  
    *-- if new page
    IF ROW > 53
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH 'RMSTYLS',XTITLE,R_WIDTH
      *-- case selected report type
      DO CASE
        CASE XREPORT = 'R'
          IF SIZEB = 'Y'

            *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  CRMEMO REASON      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT'

            *B605458,1 ABD - Remark the next line and change the printing position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  CRMEMO REASON                     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
             @ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON                     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY       STK.AMT'
            *B605458,1 ABD - [End]
            
            *B603161,1 WAB -(END)
          ELSE
            *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON      STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT'
            *B605458,1 ABD - Remark the next line and change the printing position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON                     STK.QTY    STK.AMOUNT  '
            @ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON                     STK.QTY      STK.AMOUNT'
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
           ENDIF
        CASE XREPORT = 'O'
          IF SIZEB = 'Y'

            *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT'
            *B605458,1 ABD - Remark the next line and change the printing position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON                     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
            @ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON                     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY       STK.AMT'
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ELSE
            *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON      STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT'
            *B605458,1 ABD - Remark the next line and change the printing position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON                     STK.QTY    STK.AMOUNT  '
            @ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON                     STK.QTY      STK.AMOUNT'
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ENDIF

        *--E301643,1 mhm Add Electronic status [Start]
        CASE XREPORT = 'E'
          IF SIZEB = 'Y'
            @ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON                     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT  '
          ELSE
            @ 05,00 SAY lcSTitle+' ACCOUNT#  RANO   REASON                     STK.QTY    STK.AMOUNT  '
          ENDIF
        *--E301643,1 mhm  [End]

        *--E301643,1 mhm Change Status from Both to All [Start]
        *CASE XREPORT = 'B'
        CASE XREPORT = 'A'
        *--E301643,1 mhm [End]
        
          IF SIZEB = 'Y'

            *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON      QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY   STK.AMT DMG.QTY  DMG.AMT'
            *B605458,1 ABD - Remark the next line and change the printing position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON                     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY     STK.AMT'
            @ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON                     QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8 STK.QTY       STK.AMT'            
            *B605458,1 ABD - [End]
           *B603161,1 WAB -(END)
          ELSE
            *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON      STK.QTY  STK.AMOUNT  DMG.QTY    DMG.AMT'
            *B605458,1 ABD - Remark the next line and change the printing position. [Begin]
            *@ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON                     STK.QTY    STK.AMOUNT  '
            @ 05,00 SAY lcSTitle+' ACCT# TYP CR/RA  REASON                     STK.QTY      STK.AMOUNT'
            *B605458,1 ABD - [End]
            *B603161,1 WAB -(END)
          ENDIF
      ENDCASE  && endcase selected report type
      @ 06,00 SAY REPLICATE('=',R_LEN)
      ROW = 7
    ENDIF  && endif new page
    *-- print the breake
    *B603161,1 WAB -(START) also in case of eof() print last subtotal
    *IF HBREAK <> &BREAK 
    IF HBREAK <> &BREAK .OR. EOF()
    *B603161,1 WAB -(END)
      @ ROW,00 SAY REPLICATE('-',R_LEN)
      ROW = ROW + 1
      @ ROW,00 SAY XSUB_TOT
      DO CASE
        CASE XTOT_NAME = 'S'
  
          *B603161,1 WAB -(START)-print the currency code in the subtotal line in case of multi currency
          *@ ROW,12 SAY HBREAK    && maximum of 19 in 2.7
   
          lcCurrency = IIF(llMultCurr .AND. lcRpCurr = "F","Currency: "+SUBSTR(HBREAK,LEN(Style)+1,3),"")
          @ ROW,11 SAY SUBSTR(HBREAK,1,LEN(Style))+lcCurrency
         *B603161,1 WAB -(END)

        CASE XTOT_NAME = 'A'

          *B603161,1 WAB -(START)-print the currency code in the subtotal line in case of multi currency
          *@ ROW,12 SAY SUBSTR(XCUSTOMER,1,18)

          lcCurrency = IIF(llMultCurr .AND. lcRpCurr = "F","Currency: "+SUBSTR(HBREAK,LEN(Account)+1,3),"")
          @ ROW,11 SAY SUBSTR(XCUSTOMER,1,18)+lcCurrency
          *B603161,1 WAB -(END)

        CASE XTOT_NAME = 'R'

          *B603161,1 WAB -(START)-print the currency code in the subtotal line in case of multi currency
          *@ ROW,12 SAY SUBSTR(gfCodDes(XREASON,'REASON'),1,18)
    
          lcCurrency = IIF(llMultCurr .AND. lcRpCurr = "F","Currency: "+SUBSTR(HBREAK,7,3),"")
          
          *B604062,1 KAM calling function with xreason instead of reason[start]
          *@ ROW,11 SAY SUBSTR(gfCodDes(REASON,'REASON'),1,18)+lcCurrency
          @ ROW,11 SAY SUBSTR(gfCodDes(XREASON,'REASON'),1,18)+lcCurrency 
          *B604062,1 KAM[end]
                    
         *B603161,1 WAB -(END)
    
      ENDCASE
      IF XBOTH_FLG

        *B603161,1 WAB -(START) Rearange the R/A text position
        *@ ROW,37 SAY 'R/A :'

        @ ROW,44 SAY 'R/A:'
        *B603161,1 WAB -(END)

      ENDIF
      SELECT &XNAME
      *-- if print size breakdown
      IF SIZEB = 'Y'
        *B603161,1 WAB - commet this line & rewirte it agin with new print position & add digit 
        *B603161,1 WAB -to the amount field picture & Remove damage qty & damage amount cause 
        *B603161,1 WAB -(START) The 2 fields not used in ARIA27 
        *@ ROW,051-03 SAY XTOT(1,1) PICTURE '99999'
        *@ ROW,057-03 SAY XTOT(1,2) PICTURE '99999'
        *@ ROW,063-03 SAY XTOT(1,3) PICTURE '99999'
        *@ ROW,069-03 SAY XTOT(1,4) PICTURE '99999'
        *@ ROW,075-03 SAY XTOT(1,5) PICTURE '99999'
        *@ ROW,081-03 SAY XTOT(1,6) PICTURE '99999'
        *@ ROW,087-03 SAY XTOT(1,7) PICTURE '99999'
        *@ ROW,093-03 SAY XTOT(1,8) PICTURE '99999'
        *@ ROW,099-03 SAY XTOT(1,9) PICTURE '9999999'
        *IF XAMOUNT = 'Y'
        *  @ ROW,106-03 SAY XTOT(1,10) PICTURE '9999999.99'
        *ENDIF
        *IF XREPORT = 'R'  .OR. XREPORT = 'B'
        *  @ ROW,117-03 SAY XTOT(1,11) PICTURE '9999999'
        *  IF XAMOUNT = 'Y'
        *    @ ROW,124-03 SAY XTOT(1,12) PICTURE '9999999.99'
        *  ENDIF
        *ENDIF
        
        
        *B605458,1 ABD - Print titel 1st quality in case we print the received line. [Begin]
        IF XREPORT $ 'R' .OR. (XREPORT $ 'A' .AND. !XBOTH_FLG)
          @ ROW,045 SAY '1ST QUALITY'
        ENDIF  
        *B605458,1 ABD - [End]
        
        @ ROW,066-03 SAY XTOT(1,1) PICTURE '99999'
        @ ROW,072-03 SAY XTOT(1,2) PICTURE '99999'
        @ ROW,078-03 SAY XTOT(1,3) PICTURE '99999'
        @ ROW,084-03 SAY XTOT(1,4) PICTURE '99999'
        @ ROW,090-03 SAY XTOT(1,5) PICTURE '99999'
        @ ROW,096-03 SAY XTOT(1,6) PICTURE '99999'
        @ ROW,102-03 SAY XTOT(1,7) PICTURE '99999'
        @ ROW,108-03 SAY XTOT(1,8) PICTURE '99999'
        @ ROW,114-03 SAY XTOT(1,9) PICTURE '9999999'


        IF XAMOUNT = 'Y'
          *B603713,7 KAM 10/15/2000  increas width of element which contain total[stART]
          *@ ROW,122-03 SAY XTOT(1,10) PICTURE '99999999.99'
          *B605458,1 ABD - Remark the next line & update the Position. [Begin]
          *@ ROW,122-03 SAY XTOT(1,10) PICTURE '99999999999.99'
          @ ROW,122-04 SAY XTOT(1,10) PICTURE '99999999999.99'
          *B605458,1 ABD - [End]
          *B603713,7 KAM [end]
        ENDIF
        *B603161,1 WAB -(END)
        
        *B605458,1 ABD - Print 2nd QUALITY in case we print the received line. [Begin]
        IF XREPORT $ 'R' .OR. (XREPORT $ 'A' .AND. !XBOTH_FLG)
          *-- Print the 2nd Qty        
          ROW = Row + 1
          @ ROW,045 SAY '2ND QUALITY'
      
          
          @ ROW,066-03 SAY la2ndQty(1,1) PICTURE '99999'
          @ ROW,072-03 SAY la2ndQty(1,2) PICTURE '99999'
          @ ROW,078-03 SAY la2ndQty(1,3) PICTURE '99999'
          @ ROW,084-03 SAY la2ndQty(1,4) PICTURE '99999'
          @ ROW,090-03 SAY la2ndQty(1,5) PICTURE '99999'
          @ ROW,096-03 SAY la2ndQty(1,6) PICTURE '99999'
          @ ROW,102-03 SAY la2ndQty(1,7) PICTURE '99999'
          @ ROW,108-03 SAY la2ndQty(1,8) PICTURE '99999'
          @ ROW,114-03 SAY la2ndQty(1,9) PICTURE '9999999'
          
          IF XAMOUNT = 'Y'
            @ ROW,122-04 SAY la2ndQty(1,10) PICTURE '99999999999.99'
          ENDIF
          *-- Print the Damge Qty        
          ROW = Row + 1
          @ ROW,045 SAY 'DAMGE'
          @ ROW,066-03 SAY laDmgdQty(1,1) PICTURE '99999'
          @ ROW,072-03 SAY laDmgdQty(1,2) PICTURE '99999'
          @ ROW,078-03 SAY laDmgdQty(1,3) PICTURE '99999'
          @ ROW,084-03 SAY laDmgdQty(1,4) PICTURE '99999'
          @ ROW,090-03 SAY laDmgdQty(1,5) PICTURE '99999'
          @ ROW,096-03 SAY laDmgdQty(1,6) PICTURE '99999'
          @ ROW,102-03 SAY laDmgdQty(1,7) PICTURE '99999'
          @ ROW,108-03 SAY laDmgdQty(1,8) PICTURE '99999'
          @ ROW,114-03 SAY laDmgdQty(1,9) PICTURE '9999999'

          IF XAMOUNT = 'Y'
            @ ROW,122-04 SAY laDmgdQty(1,10) PICTURE '99999999999.99'
          ENDIF
          
          
        ENDIF
        *B605458,1 ABD - [End]

        STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
        STORE 0 TO XTOT(1,5),XTOT(1,6),XTOT(1,7),XTOT(1,8)
        STORE 0 TO XTOT(1,9),XTOT(1,10),XTOT(1,11),XTOT(1,12)
        
        *B605458,1 ABD - assign zero to the array that print the 2nd & damage Qty. [Begin]
        STORE 0 TO la2ndQty(1,1) ,la2ndQty(1,2) ,la2ndQty(1,3) ,la2ndQty(1,4) ,la2ndQty(1,5)  ,;
                   la2ndQty(1,6) ,la2ndQty(1,7) ,la2ndQty(1,8) ,la2ndQty(1,9) ,la2ndQty(1,10) ,;
                   laDmgdQty(1,1),laDmgdQty(1,2),laDmgdQty(1,3),laDmgdQty(1,4),laDmgdQty(1,5) ,;
                   laDmgdQty(1,6),laDmgdQty(1,7),laDmgdQty(1,8),laDmgdQty(1,9),laDmgdQty(1,10)
        *B605458,1 ABD - [End]
                   
        
        IF XBOTH_FLG
          ROW = ROW + 1
    
          *B603161,1 WAB -(START) commit this line & rewrite agine with new print position &
          *B603161,1 WAB -Rearrange the Rcv text position  & add digit to the field picture
          *B603161,1 WAB - Remove damage qty & damage amount cause The 2 fields not used in
          *B603161,1 WAB -(START) ARIA27 
          
          *@ ROW,37 SAY 'RCV :'
          *@ ROW,051-03 SAY XTOT_B(1,1) PICTURE '99999'
          *@ ROW,057-03 SAY XTOT_B(1,2) PICTURE '99999'
          *@ ROW,063-03 SAY XTOT_B(1,3) PICTURE '99999'
          *@ ROW,069-03 SAY XTOT_B(1,4) PICTURE '99999'
          *@ ROW,075-03 SAY XTOT_B(1,5) PICTURE '99999'
          *@ ROW,081-03 SAY XTOT_B(1,6) PICTURE '99999'
          *@ ROW,087-03 SAY XTOT_B(1,7) PICTURE '99999'
          *@ ROW,093-03 SAY XTOT_B(1,8) PICTURE '99999'
          *@ ROW,099-03 SAY XTOT_B(1,9) PICTURE '9999999'
          *IF XAMOUNT = 'Y'
          *  @ ROW,106-03 SAY XTOT_B(1,10) PICTURE '9999999.99'
          *ENDIF
          *@ ROW,117-03 SAY XTOT_B(1,11) PICTURE '9999999'
          *IF XAMOUNT = 'Y'
          *  @ ROW,124-03 SAY XTOT_B(1,12) PICTURE '9999999.99'
          *ENDIF
          *@ ROW,44 SAY 'RCV:'
          @ ROW,44 SAY '1ST:'

          *B605458,1 ABD - Print titel '1ST QUALITY' . [Begin]
          @ ROW,050 SAY '1ST QUALITY'
          *B605458,1 ABD - [End]
          
          @ ROW,066-03 SAY XTOT_B(1,1) PICTURE '99999'
          @ ROW,072-03 SAY XTOT_B(1,2) PICTURE '99999'
          @ ROW,078-03 SAY XTOT_B(1,3) PICTURE '99999'
          @ ROW,084-03 SAY XTOT_B(1,4) PICTURE '99999'
          @ ROW,090-03 SAY XTOT_B(1,5) PICTURE '99999'
          @ ROW,096-03 SAY XTOT_B(1,6) PICTURE '99999'
          @ ROW,102-03 SAY XTOT_B(1,7) PICTURE '99999'
          @ ROW,108-03 SAY XTOT_B(1,8) PICTURE '99999'
          @ ROW,114-03 SAY XTOT_B(1,9) PICTURE '9999999'

          IF XAMOUNT = 'Y'
            *B603713,7 KAM 10/24/2000 increase the picture size[start] 
            *@ ROW,122-03 SAY XTOT_B(1,10) PICTURE '99999999.99'
            *B605458,1 ABD - Remark the next line & update the Position. [Begin]
            *@ ROW,122-03 SAY XTOT_B(1,10) PICTURE '99999999999.99'
            @ ROW,122-04 SAY XTOT_B(1,10) PICTURE '99999999999.99'
            *B605458,1 ABD - [End]
            *B603713,7 KAM [end]
          ENDIF
         *B603161,1 WAB -(END)
          

          *B605458,1 ABD - Print Titel 2nd QUALITY. [Begin]
          *-- Print 2nd Q.
          ROW = ROW + 1
          @ ROW,050 SAY '2ND QUALITY'
          *B605458,1 ABD - [End] [Begin]

          @ ROW,066-03 SAY la2ndQty(2,1) PICTURE '99999'
          @ ROW,072-03 SAY la2ndQty(2,2) PICTURE '99999'
          @ ROW,078-03 SAY la2ndQty(2,3) PICTURE '99999'
          @ ROW,084-03 SAY la2ndQty(2,4) PICTURE '99999'
          @ ROW,090-03 SAY la2ndQty(2,5) PICTURE '99999'
          @ ROW,096-03 SAY la2ndQty(2,6) PICTURE '99999'
          @ ROW,102-03 SAY la2ndQty(2,7) PICTURE '99999'
          @ ROW,108-03 SAY la2ndQty(2,8) PICTURE '99999'
          @ ROW,114-03 SAY la2ndQty(2,9) PICTURE '9999999'
          
          IF XAMOUNT = 'Y'
            @ ROW,122-04 SAY la2ndQty(2,10) PICTURE '99999999999.99'
          ENDIF
         
         *-- Print the damge Qt.
         ROW = ROW + 1
         *B605458,1 ABD - Print damage titel . [Begin]
         @ ROW,050 SAY 'DAMGE'
         *B605458,1 ABD - [End]

          @ ROW,066-03 SAY laDmgdQty(2,1) PICTURE '99999'
          @ ROW,072-03 SAY laDmgdQty(2,2) PICTURE '99999'
          @ ROW,078-03 SAY laDmgdQty(2,3) PICTURE '99999'
          @ ROW,084-03 SAY laDmgdQty(2,4) PICTURE '99999'
          @ ROW,090-03 SAY laDmgdQty(2,5) PICTURE '99999'
          @ ROW,096-03 SAY laDmgdQty(2,6) PICTURE '99999'
          @ ROW,102-03 SAY laDmgdQty(2,7) PICTURE '99999'
          @ ROW,108-03 SAY laDmgdQty(2,8) PICTURE '99999'
          @ ROW,114-03 SAY laDmgdQty(2,9) PICTURE '9999999'

          IF XAMOUNT = 'Y'
            @ ROW,122-04 SAY laDmgdQty(2,10) PICTURE '99999999999.99'
          ENDIF
          

          STORE 0 TO XTOT_B(1,1),XTOT_B(1,2),XTOT_B(1,3),XTOT_B(1,4)
          STORE 0 TO XTOT_B(1,5),XTOT_B(1,6),XTOT_B(1,7),XTOT_B(1,8)
          STORE 0 TO XTOT_B(1,9),XTOT_B(1,10),XTOT_B(1,11),XTOT_B(1,12)
          
          *B605458,1 ABD - assign zero to the damge and 2nd qty array. [Begin]
          STORE 0 To la2ndQty(2,1) ,la2ndQty(2,2) ,la2ndQty(2,3) ,la2ndQty(2,4) ,la2ndQty(2,5)  ,;
                     la2ndQty(2,6) ,la2ndQty(2,7) ,la2ndQty(2,8) ,la2ndQty(2,9) ,la2ndQty(2,10) ,;
                     laDmgdQty(2,1),laDmgdQty(2,2),laDmgdQty(2,3),laDmgdQty(2,4),laDmgdQty(2,5) ,;
                     laDmgdQty(2,6),laDmgdQty(2,7),laDmgdQty(2,8),laDmgdQty(2,9),laDmgdQty(2,10)
          
          *B605458,1 ABD - [End]
        ENDIF
      ELSE  && else if print size breakdown equal 'N'
        *B603161,1 WAB - commit this line & rewrite agin with new printing position & Remove damage 
        *B603161,1 WAB -qty & damage amount cause The 2 fields not used in ARIA27 & add digit to 
        *B603161,1 WAB -(START) the field picture
        *@ ROW,52-03 SAY XTOT(1,1) PICTURE '9999999'
        *IF XAMOUNT= 'Y'
        *  *@ ROW,61-03 SAY XTOT(1,2) PICTURE '9999999.99'
        *ENDIF
        *IF XREPORT = 'R'  .OR. XREPORT = 'B'
        *  @ ROW,73-03 SAY XTOT(1,3) PICTURE '9999999'
        *  IF XAMOUNT = 'Y'
        *    @ ROW,82-03 SAY XTOT(1,4) PICTURE '999999.99'
        *  ENDIF
        *ENDIF
        *B605458,1 ABD - Print 1ST QUALITY in case we print the received qty. [Begin]
        IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
          @ ROW,045 SAY '1ST QUALITY'
        ENDIF  
        *B605458,1 ABD - [End]
        @ ROW,67-03 SAY XTOT(1,1) PICTURE '9999999'
        IF XAMOUNT= 'Y'
          *B603713,7 KAM 10/24/2000 increase picture size [start]
          *@ ROW,77-03 SAY XTOT(1,2) PICTURE '99999999.99'
          @ ROW,77-03 SAY XTOT(1,2) PICTURE '9999999999.99'
          *B603713,7 KAM [end]
        ENDIF
        *B603161,1 WAB -(END)

        IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
          ROW = ROW + 1
          @ ROW,045 SAY '2ND QUALITY'
          @ ROW,67-03 SAY la2ndQty(1,1) PICTURE '9999999'
          IF XAMOUNT= 'Y'          
            @ ROW,77-03 SAY la2ndQty(1,2) PICTURE '9999999999.99'
          ENDIF
          
          ROW = ROW + 1
          @ ROW,045 SAY 'DAMGE'
          @ ROW,67-03 SAY laDmgdQty(1,1) PICTURE '9999999'
          IF XAMOUNT= 'Y'          
            @ ROW,77-03 SAY laDmgdQty(1,2) PICTURE '9999999999.99'
          ENDIF

        ENDIF


        STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
        *B605458,1 ABD - [End]
        
        IF XBOTH_FLG
          ROW = ROW + 1

          *B603161,1 WAB -(START)- Rearrange the Rcv text  Position
          *@ ROW,37 SAY 'RCV :'
          *@ ROW,52-03 SAY XTOT_B(1,1) PICTURE '9999999'

          *B605458,1 ABD - Print Titel '2nd' and 2nd quantity. [Begin]
          *@ ROW,44 SAY 'RCV:'
          @ ROW,44 SAY '1ST:'
          @ ROW,67-03 SAY XTOT_B(1,1) PICTURE '9999999'
          *B605458,1 ABD - [End]
          *B603161,1 WAB -(END)
          
          
          IF XAMOUNT= 'Y'

            *B603161,1 WAB -(START)- & add digit to the field picture
            *@ ROW,61-03 SAY XTOT_B(1,2) PICTURE '9999999.99'
            *B603713,7 KAM 10/24/2000 increase picture size [start] 
            *@ ROW,77-03 SAY XTOT_B(1,2) PICTURE '99999999.99'
            @ ROW,77-03 SAY XTOT_B(1,2) PICTURE '9999999999.99'
            *B603713,7 KAM [end]
            *B603161,1 WAB -(END)

          ENDIF


          *B605458,1 ABD - Print Titel '2nd' and 2nd quantity. [Begin]
          ROW = ROW + 1
          @ ROW,44 SAY '2ND:'
          @ ROW,67-03 SAY la2ndQty(1,1) PICTURE '9999999'
          IF XAMOUNT= 'Y'          
            @ ROW,77-03 SAY la2ndQty(1,2) PICTURE '9999999999.99'
          ENDIF
        
          ROW = ROW + 1
          @ ROW,44 SAY 'DMG:'
          @ ROW,67-03 SAY laDmgdQty(1,1) PICTURE '9999999'
          IF XAMOUNT= 'Y'          
            @ ROW,77-03 SAY laDmgdQty(1,2) PICTURE '9999999999.99'
          ENDIF
         *B605458,1 ABD - [End]


          *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *@ ROW,73-03 SAY XTOT_B(1,3) PICTURE '9999999'
          *IF XAMOUNT = 'Y'
          *  @ ROW,82-03 SAY XTOT_B(1,4) PICTURE '999999.99'
          *ENDIF
          *B603161,1 WAB -(END)

          STORE 0 TO XTOT_B(1,1),XTOT_B(1,2),XTOT_B(1,3),XTOT_B(1,4)
          STORE 0 TO la2ndQty(1,1),la2ndQty(1,2),laDmgdQty(1,1),laDmgdQty(1,2)
        ENDIF
      ENDIF  && endif print size breakdown
      STORE 0 TO la2ndQty(1,1),la2ndQty(1,2),laDmgdQty(1,1),laDmgdQty(1,2)
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',R_LEN)
      ROW = ROW + 1
      SELECT &XNAME
      *B603161,1 WAB - we remove !EOF() from the loop condition to print last subtotal
      *B603161,1 WAB -(START) so we must check if it eof() to exit from loop
      IF EOF()
        EXIT
      ENDIF
      *B603161,1 WAB -(END) 
      HBREAK=&BREAK
      XCUSTOMER = CUSTOMER.STNAME
    ENDIF
    IF XBOTH_FLG
      IF EMPTY(TYPE)
        XTYPE = 'R/A'
        XSUB  = .T.
        XNO   = 'RANO'
      ELSE
        XTYPE = 'RCV'
        XNO   = 'CRMEMO'
        XSUB  = .F.
      ENDIF
    ENDIF
    *B604062,1 KAM add assigment xreason variable[start] 
    XREASON=REASON
    *B604062,1 KAM [end] 
    
    *-- group by nonmajor
    *B603161,1 WAB -(START) print style also in case if the currency code is changed in case of multi currency
    *IF X_STYLE+X_COLOR <> SUBSTR(STYLE,1,lnMajSize)+RIGHT(STYLE,lnNonSegSize)

    IF X_STYLE+X_COLOR <> SUBSTR(STYLE,1,lnMajSize)+RIGHT(STYLE,lnNonSegSize) .OR. ;
      ( llMultCurr .AND. lcRpCurr = "F" .AND. lcCurrCode <> cCurrCode )

      lcCurrCode = IIF(llMultCurr,cCurrCode,"")			&& get the new Currency Code

    *B603161,1 WAB -(END)

      @ ROW,00 SAY STYLE PICT '!!!!!!!!!!!!!!!!!!!'
      X_STYLE  = SUBSTR(STYLE,1,lnMajSize)
      X_COLOR  = RIGHT(STYLE,lnNonSegSize)
    ENDIF
    @ ROW,20 SAY ACCOUNT
    
    *--E301643,1 mhm change status from Both to All [Start]
    *IF XREPORT = 'B'

    *B605458,1 ABD - Remark the next line and print 2nd in case print 2nd qty line. [Begin] 
    *IF XREPORT = 'A'
    *--E301643,1 mhm [End]
    *@ ROW,26 SAY XTYPE
    IF XREPORT $ 'RA'
      DO CASE
        CASE cStygrade = '1'
          @ ROW,26 SAY 'REC'
        CASE cStygrade = '2'
          @ ROW,26 SAY '2ND'
        CASE cStygrade = '3'
          @ ROW,26 SAY 'DMG'
        CASE EMPTY(cStygrade)
          @ ROW,26 SAY XTYPE
        ENDCASE  
      *B605458,1 ABD - [End]
    ENDIF
    @ ROW,30 SAY &XNO
    @ ROW,37 SAY REASON
    XREASON = REASON
    XRETU_DATA = gfCodDes(XREASON,'REASON')
    SELECT &XNAME
    *B603161,1 WAB -comment next line abd rewirte agin with onther position & print amount by 
    *B603161,1 WAB - the equivalent amount in case of multi currency & add digit to the field 
    *B603161,1 WAB -picture & Remove damage qty & damage amount cause The 2 fields not used in 
    *B603161,1 WAB -(START)- ARIA27 
    *@ ROW,40 SAY SUBSTR(XRETU_DATA,1,7)
    *IF SIZEB = 'Y'
    *  @ ROW,051-03 SAY QTY1 PICTURE '99999'
    *  @ ROW,057-03 SAY QTY2 PICTURE '99999'
    *  @ ROW,063-03 SAY QTY3 PICTURE '99999'
    *  @ ROW,069-03 SAY QTY4 PICTURE '99999'
    *  @ ROW,075-03 SAY QTY5 PICTURE '99999'
    *  @ ROW,081-03 SAY QTY6 PICTURE '99999'
    *  @ ROW,087-03 SAY QTY7 PICTURE '99999'
    *  @ ROW,093-03 SAY QTY8 PICTURE '99999'
    *  @ ROW,099-03 SAY TOTQTY PICTURE '9999999'
    * IF XAMOUNT = 'Y'
    *   @ ROW,106-03 SAY TOTQTY*PRICE PICTURE '9999999.99'
    * ENDIF   
    * IF XREPORT = 'R' .OR. (XREPORT = 'B' .AND. XTYPE ='RCV')
    *   @ ROW,117-03 SAY DMGQTY PICTURE '9999999'
    *   IF XAMOUNT = 'Y'
    *     @ ROW,124-03 SAY DMGQTY*PRICE PICTURE '9999999.99'
    *   ENDIF
    * ENDIF
    @ ROW,44 SAY SUBSTR(XRETU_DATA,1,17)
    IF SIZEB = 'Y'
      @ ROW,066-03 SAY QTY1 PICTURE '99999'
      @ ROW,072-03 SAY QTY2 PICTURE '99999'
      @ ROW,078-03 SAY QTY3 PICTURE '99999'
      @ ROW,084-03 SAY QTY4 PICTURE '99999'
      @ ROW,090-03 SAY QTY5 PICTURE '99999'
      @ ROW,096-03 SAY QTY6 PICTURE '99999'
      @ ROW,102-03 SAY QTY7 PICTURE '99999'
      @ ROW,108-03 SAY QTY8 PICTURE '99999'
      @ ROW,114-03 SAY TOTQTY PICTURE '9999999'
      IF XAMOUNT = 'Y'
        IF llMultCurr .AND. lcRpCurr <> "F"
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field  [start]
          *@ ROW,122-03 SAY lfBaseAmt(TOTQTY * PRICE) PICTURE '99999999.99'
          @ ROW,122-03 SAY lfBaseAmt(TOTQTY * PRICE) PICTURE '9999999999.99'
          *B603713,7 KAM [end] 
        ELSE
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field  [start]
          *@ ROW,122-03 SAY TOTQTY*PRICE PICTURE '99999999.99'
          @ ROW,122-03 SAY TOTQTY*PRICE PICTURE '9999999999.99' 
          *B603713,7 KAM [end]
        ENDIF
      ENDIF
    *B603161,1 WAB -(END)

      *-- Accumulate for subtotal
      IF XSUB
        *B605458,1 ABD - Accumulate the 1st Qty. [Begin] 
        
        DO CASE
          CASE XREPORT = 'O' .OR. (cStygrade = '1' .OR.  EMPTY(cStygrade))
            *B605458,1 ABD - [End]
            XTOT(1,1)  = XTOT(1,1)  + QTY1
            XTOT(1,2)  = XTOT(1,2)  + QTY2
            XTOT(1,3)  = XTOT(1,3)  + QTY3
            XTOT(1,4)  = XTOT(1,4)  + QTY4
            XTOT(1,5)  = XTOT(1,5)  + QTY5
            XTOT(1,6)  = XTOT(1,6)  + QTY6
            XTOT(1,7)  = XTOT(1,7)  + QTY7
            XTOT(1,8)  = XTOT(1,8)  + QTY8
            XTOT(1,9)  = XTOT(1,9)  + TOTQTY
          *B605458,1 ABD -  Accumulate the 2nd and damge Qty. [Begin] 
        CASE cStygrade = '2'
            *-- la2ndQty 
          la2ndQty(1,1) = la2ndQty(1,1) + QTY1
          la2ndQty(1,2) = la2ndQty(1,2) + QTY2
          la2ndQty(1,3) = la2ndQty(1,3) + QTY3
          la2ndQty(1,4) = la2ndQty(1,4) + QTY4
          la2ndQty(1,5) = la2ndQty(1,5) + QTY5
          la2ndQty(1,6) = la2ndQty(1,6) + QTY6
          la2ndQty(1,7) = la2ndQty(1,7) + QTY7
          la2ndQty(1,8) = la2ndQty(1,8) + QTY8
          la2ndQty(1,9) = la2ndQty(1,9) + TOTQTY

        
        CASE cStygrade = '3'
            *-- laDmgdQty
          laDmgdQty(1,1) = laDmgdQty(1,1) + QTY1
          laDmgdQty(1,2) = laDmgdQty(1,2) + QTY2 
          laDmgdQty(1,3) = laDmgdQty(1,3) + QTY3 
          laDmgdQty(1,4) = laDmgdQty(1,4) + QTY4 
          laDmgdQty(1,5) = laDmgdQty(1,5) + QTY5 
          laDmgdQty(1,6) = laDmgdQty(1,6) + QTY6 
          laDmgdQty(1,7) = laDmgdQty(1,7) + QTY7 
          laDmgdQty(1,8) = laDmgdQty(1,8) + QTY8 
          laDmgdQty(1,9) = laDmgdQty(1,9) + TotQty
        ENDCASE
        *B605458,1 ABD - [End]
        
        *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *XTOT(1,11) = XTOT(1,11) + DMGQTY
        *B603161,1 WAB -(END)
        
        IF XAMOUNT = 'Y'

          *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
          *B603161,1 WAB -(START) 
          *XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE
          *XTOT(1,12) = XTOT(1,12) + DMGQTY * PRICE
          
          IF llMultCurr .AND. lcRpCurr <> "F"
            
            *B605458,1 ABD - Remark the next line and acummlate the 2nd and damge qty on the type. [Begin]
            *XTOT(1,10) = XTOT(1,10) + lfBaseAmt(TOTQTY * PRICE)             
            IF XREPORT $ 'AR'
              DO CASE
                CASE cStygrade = '1' .OR.  EMPTY(cStygrade)
                  XTOT(1,10)      = XTOT(1,10)     + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '2'
                  la2ndQty (1,10) = la2ndQty(1,10) + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '3'
                  laDmgdQty(1,10) = laDmgdQty(1,10)+ lfBaseAmt(TOTQTY * PRICE) 
              ENDCASE
            ELSE
              XTOT(1,10)          = XTOT(1,10)     + lfBaseAmt(TOTQTY * PRICE)
            ENDIF  
            *B605458,1 ABD - [End]
          ELSE
            *B605458,1 ABD - Remark the next line and acummlate the 2nd and damge qty on the type. [Begin]
            *XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE 
            IF XREPORT $ 'AR'
              DO CASE
                CASE cStygrade = '1' .OR.  EMPTY(cStygrade)
                  XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE
                CASE  cStygrade = '2'
                  la2ndQty (1,10) = la2ndQty(1,10) + TOTQTY * PRICE 
                CASE  cStygrade = '3'
                  laDmgdQty(1,10) = laDmgdQty(1,10)+ TOTQTY * PRICE
              ENDCASE
                  
            ELSE
              XTOT(1,10) = XTOT(1,10) + TOTQTY * PRICE
            ENDIF        
            *B605458,1 ABD - [End]
            
          ENDIF
          *B603161,1 WAB -(END)
        ENDIF
        XTOT(2,1)  = XTOT(2,1)  + QTY1
        XTOT(2,2)  = XTOT(2,2)  + QTY2
        XTOT(2,3)  = XTOT(2,3)  + QTY3
        XTOT(2,4)  = XTOT(2,4)  + QTY4
        XTOT(2,5)  = XTOT(2,5)  + QTY5
        XTOT(2,6)  = XTOT(2,6)  + QTY6
        XTOT(2,7)  = XTOT(2,7)  + QTY7
        XTOT(2,8)  = XTOT(2,8)  + QTY8
        XTOT(2,9)  = XTOT(2,9)  + TOTQTY
        *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *XTOT(2,11) = XTOT(2,11) + DMGQTY
        *B603161,1 WAB -(END)

        IF XAMOUNT = 'Y'

          *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
          *B603161,1 WAB -(START) 
          *XTOT(2,10) = XTOT(2,10) + TOTQTY * PRICE
          *XTOT(2,12) = XTOT(2,12) + DMGQTY * PRICE

          IF llMultCurr .AND. lcRpCurr <> "F"
            XTOT(2,10) = XTOT(2,10) + lfBaseAmt(TOTQTY * PRICE) 
          ELSE
            XTOT(2,10) = XTOT(2,10) + TOTQTY * PRICE
          ENDIF
          *B603161,1 WAB -(END)
        ENDIF
      ENDIF
      
      *B605458,1 ABD - Accumulate the 2nd and damge qty on the type. [Begin]
      IF XREPORT $ 'AR' .AND. !EMPTY(cStygrade)
        DO CASE
          CASE cStygrade = '1'
            laGntTotal(1,1) = laGntTotal(1,1) + QTY1
            laGntTotal(1,2) = laGntTotal(1,2) + QTY2
            laGntTotal(1,3) = laGntTotal(1,3) + QTY3
            laGntTotal(1,4) = laGntTotal(1,4) + QTY4
            laGntTotal(1,5) = laGntTotal(1,5) + QTY5
            laGntTotal(1,6) = laGntTotal(1,6) + QTY6
            laGntTotal(1,7) = laGntTotal(1,7) + QTY7
            laGntTotal(1,8) = laGntTotal(1,8) + QTY8
            laGntTotal(1,9) = laGntTotal(1,9) + TotQty
            
          CASE cStygrade = '2'
            laGntTotal(2,1) = laGntTotal(2,1) + QTY1
            laGntTotal(2,2) = laGntTotal(2,2) + QTY2
            laGntTotal(2,3) = laGntTotal(2,3) + QTY3
            laGntTotal(2,4) = laGntTotal(2,4) + QTY4
            laGntTotal(2,5) = laGntTotal(2,5) + QTY5
            laGntTotal(2,6) = laGntTotal(2,6) + QTY6
            laGntTotal(2,7) = laGntTotal(2,7) + QTY7
            laGntTotal(2,8) = laGntTotal(2,8) + QTY8
            laGntTotal(2,9) = laGntTotal(2,9) + TotQty

          CASE cStygrade = '3'
            laGntTotal(3,1) = laGntTotal(3,1) + QTY1
            laGntTotal(3,2) = laGntTotal(3,2) + QTY2
            laGntTotal(3,3) = laGntTotal(3,3) + QTY3
            laGntTotal(3,4) = laGntTotal(3,4) + QTY4
            laGntTotal(3,5) = laGntTotal(3,5) + QTY5
            laGntTotal(3,6) = laGntTotal(3,6) + QTY6
            laGntTotal(3,7) = laGntTotal(3,7) + QTY7
            laGntTotal(3,8) = laGntTotal(3,8) + QTY8
            laGntTotal(3,9) = laGntTotal(3,9) + TotQty
        ENDCASE
      ENDIF        
        
      IF XREPORT $ 'AR' .AND. !EMPTY(cStygrade)
       IF llMultCurr .AND. lcRpCurr <> "F" 
         DO CASE
           CASE cStygrade = '1'
             laGntTotal(1,10) = laGntTotal(1,10)  + lfBaseAmt(TOTQTY * PRICE) 
           CASE cStygrade = '2'
             laGntTotal(2,10) = laGntTotal(2,10)  + lfBaseAmt(TOTQTY * PRICE) 
           CASE cStygrade = '3'
             laGntTotal(3,10) = laGntTotal(3,10)  + lfBaseAmt(TOTQTY * PRICE) 
           ENDCASE  
       ELSE
         DO CASE
           CASE cStygrade = '1'
             laGntTotal(1,10) = laGntTotal(1,10)  + TOTQTY * PRICE
           CASE cStygrade = '2'
             laGntTotal(2,10) = laGntTotal(2,10)  + TOTQTY * PRICE
           CASE cStygrade = '3'
             laGntTotal(3,10) = laGntTotal(3,10)  + TOTQTY * PRICE
           ENDCASE  
       ENDIF
      ENDIF
      *B605458,1 ABD - [End]
      IF XBOTH_FLG
        IF !EMPTY(TYPE)
          *B605458,1 ABD - Accumulate the 2nd and damge qty on the type. [Begin]
          DO CASE
            CASE cStygrade = '1'
              *B605458,1 ABD - [End]
              XTOT_B(1,1)  = XTOT_B(1,1)  + QTY1
              XTOT_B(1,2)  = XTOT_B(1,2)  + QTY2
              XTOT_B(1,3)  = XTOT_B(1,3)  + QTY3
              XTOT_B(1,4)  = XTOT_B(1,4)  + QTY4
              XTOT_B(1,5)  = XTOT_B(1,5)  + QTY5
              XTOT_B(1,6)  = XTOT_B(1,6)  + QTY6
              XTOT_B(1,7)  = XTOT_B(1,7)  + QTY7
              XTOT_B(1,8)  = XTOT_B(1,8)  + QTY8
              XTOT_B(1,9)  = XTOT_B(1,9)  + TOTQTY
          
              *B605458,1 ABD - Accumulate the 2nd and damge qty on the type. [Begin]
            CASE cStygrade = '2'
              *-- la2ndQty 
              la2ndQty(2,1) = la2ndQty(2,1) + QTY1
              la2ndQty(2,2) = la2ndQty(2,2) + QTY2
              la2ndQty(2,3) = la2ndQty(2,3) + QTY3
              la2ndQty(2,4) = la2ndQty(2,4) + QTY4
              la2ndQty(2,5) = la2ndQty(2,5) + QTY5
              la2ndQty(2,6) = la2ndQty(2,6) + QTY6
              la2ndQty(2,7) = la2ndQty(2,7) + QTY7
              la2ndQty(2,8) = la2ndQty(2,8) + QTY8
              la2ndQty(2,9) = la2ndQty(2,9) + TOTQTY
           
            
            CASE cStygrade = '3'
              *-- laDmgdQty
              laDmgdQty(2,1) = laDmgdQty(2,1) + QTY1
              laDmgdQty(2,2) = laDmgdQty(2,2) + QTY2 
              laDmgdQty(2,3) = laDmgdQty(2,3) + QTY3 
              laDmgdQty(2,4) = laDmgdQty(2,4) + QTY4 
              laDmgdQty(2,5) = laDmgdQty(2,5) + QTY5 
              laDmgdQty(2,6) = laDmgdQty(2,6) + QTY6 
              laDmgdQty(2,7) = laDmgdQty(2,7) + QTY7 
              laDmgdQty(2,8) = laDmgdQty(2,8) + QTY8 
              laDmgdQty(2,9) = laDmgdQty(2,9) + TotQty
          ENDCASE
          *B605458,1 ABD - [End]
          
          *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *XTOT_B(1,11) = XTOT_B(1,11) + DMGQTY
          *B603161,1 WAB -(END)

          IF XAMOUNT = 'Y'

            *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
            *B603161,1 WAB -(START) 
            *XTOT_B(1,10) = XTOT_B(1,10) + TOTQTY * PRICE
            *XTOT_B(1,12) = XTOT_B(1,12) + DMGQTY * PRICE

            IF llMultCurr .AND. lcRpCurr <> "F"
              *B605458,1 ABD - Accumulate the 2nd and damge qty on the type. [Begin]
              *XTOT_B(1,10) = XTOT_B(1,10) + lfBaseAmt(TOTQTY * PRICE) 
              DO CASE
                CASE cStygrade = '1'
                  XTOT_B(1,10)    = XTOT_B(1,10)    + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '2'
                  la2ndQty(2,10)  = la2ndQty(2,10)  + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '3'
                  laDmgdQty(2,10) = laDmgdQty(2,10) + lfBaseAmt(TOTQTY * PRICE) 
              ENDCASE  
              *B605458,1 ABD - [End]
  
            ELSE
              *B605458,1 ABD - Accumulate the 2nd and damge qty on the type. [Begin]
              *XTOT_B(1,10) = XTOT_B(1,10) + TOTQTY * PRICE
              DO CASE
                CASE cStygrade = '1'
                  XTOT_B(1,10) = XTOT_B(1,10) + TOTQTY * PRICE
                CASE cStygrade = '2'
                  la2ndQty(2,10) = la2ndQty(2,10) + TOTQTY * PRICE
                CASE cStygrade = '3'
                  laDmgdQty(2,10) = laDmgdQty(2,10) + TOTQTY * PRICE
              ENDCASE
              *B605458,1 ABD - [End]
              
            ENDIF
            *B603161,1 WAB -(END)

          ENDIF
          XTOT_B(2,1)  = XTOT_B(2,1)  + QTY1
          XTOT_B(2,2)  = XTOT_B(2,2)  + QTY2
          XTOT_B(2,3)  = XTOT_B(2,3)  + QTY3
          XTOT_B(2,4)  = XTOT_B(2,4)  + QTY4
          XTOT_B(2,5)  = XTOT_B(2,5)  + QTY5
          XTOT_B(2,6)  = XTOT_B(2,6)  + QTY6
          XTOT_B(2,7)  = XTOT_B(2,7)  + QTY7
          XTOT_B(2,8)  = XTOT_B(2,8)  + QTY8
          XTOT_B(2,9)  = XTOT_B(2,9)  + TOTQTY

          *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *XTOT_B(2,11) = XTOT_B(2,11) + DMGQTY
          *B603161,1 WAB -(END) 

          IF XAMOUNT = 'Y'

            *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
            *B603161,1 WAB -(START) 
            *XTOT_B(2,10) = XTOT_B(2,10) + TOTQTY * PRICE
            *XTOT_B(2,12) = XTOT_B(2,12) + DMGQTY * PRICE

            IF llMultCurr .AND. lcRpCurr <> "F"
              XTOT_B(2,10) = XTOT_B(2,10) + lfBaseAmt(TOTQTY * PRICE) 
            ELSE
              XTOT_B(2,10) = XTOT_B(2,10) + TOTQTY * PRICE
            ENDIF
            *B603161,1 WAB -(END) 

          ENDIF
        ENDIF
      ENDIF
    ELSE
      *B603161,1 WAB -print amount by the equivalent amount in case of multi currency
      *B603161,1 WAB -(START)- & add digit to the field picture
      *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27       *@ ROW,52-03 SAY TOTQTY PICTURE '9999999'
      *@ ROW,52-03 SAY TOTQTY PICTURE '9999999'      
      *IF XAMOUNT = 'Y'
      *  @ ROW,61-03 SAY TOTQTY*PRICE PICTURE '9999999.99'
      *ENDIF
      *IF XREPORT = 'R' .OR. (XREPORT = 'B' .AND. XTYPE = 'RCV')
      *  @ ROW,73-03 SAY DMGQTY PICTURE '9999999'
      *   IF XAMOUNT = 'Y'
      *     @ ROW,82-03 SAY DMGQTY*PRICE PICTURE '999999.99'
      *  ENDIF
      *ENDIF
      @ ROW,67-03 SAY TOTQTY PICTURE '9999999'
      IF XAMOUNT = 'Y'
        IF llMultCurr .AND. lcRpCurr <> "F"
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field  [start]
          *@ ROW,77-03 SAY lfBaseAmt(TOTQTY * PRICE) PICTURE '99999999.99'
          @ ROW,77-03 SAY lfBaseAmt(TOTQTY * PRICE) PICTURE '9999999999.99'
          *B603713,7 KAM [end]
        ELSE
          *B603713,7 KAM 10/12/2000 we have to change the picture as result of 
          *B603713,7 KAM             change of price field  [start]
          *@ ROW,77-03 SAY TOTQTY*PRICE PICTURE '99999999.99'
          @ ROW,77-03 SAY TOTQTY*PRICE PICTURE '9999999999.99' 
          *B603713,7 KAM [end ]
        ENDIF
      ENDIF
      *B603161,1 WAB -(END)

      *B605458,1 ABD - Accumulate the 2nd and damge qty on the type. [Begin]
      IF XREPORT $ 'RA' .AND. !EMPTY(cStygrade)
        DO CASE
          CASE cStygrade = '1'
            laGntTotal(1,1) = laGntTotal(1,1) + TotQty
          CASE cStygrade = '2'
            laGntTotal(2,1) = laGntTotal(2,1) + TotQty
          CASE cStygrade = '3'
            laGntTotal(3,1) = laGntTotal(3,1) + TotQty
        ENDCASE    
      ENDIF  
      IF XREPORT $ 'AR' .AND. !EMPTY(cStygrade)
        IF llMultCurr .AND. lcRpCurr <> "F"
          DO CASE
            CASE cStygrade = '1'
              laGntTotal(1,2) = laGntTotal(1,2)  + lfBaseAmt(TOTQTY * PRICE) 
            CASE cStygrade = '2'
              laGntTotal(2,2) = laGntTotal(2,2)  + lfBaseAmt(TOTQTY * PRICE) 
            CASE cStygrade = '3'
              laGntTotal(3,2) = laGntTotal(3,2)  + lfBaseAmt(TOTQTY * PRICE) 
            ENDCASE  
        ELSE
          DO CASE
            CASE cStygrade = '1'
              laGntTotal(1,2) = laGntTotal(1,2)  + TOTQTY * PRICE
            CASE cStygrade = '2'
              laGntTotal(2,2) = laGntTotal(2,2)  + TOTQTY * PRICE
            CASE cStygrade = '3'
              laGntTotal(3,2) = laGntTotal(3,2)  + TOTQTY * PRICE
            ENDCASE  
         ENDIF
       ENDIF
      *B605458,1 ABD - [End]
  
      IF XSUB
        
        *B605458,1 ABD - Remark the next line & Accumulate the 2nd and damge qty on the type. [Begin]
        *XTOT(1,1) = XTOT(1,1) + TOTQTY        
        *XTOT(2,1) = XTOT(2,1) + TOTQTY
        
        IF XREPORT $ 'RA'
          *B605458,1 ABD - [End]
          XTOT(2,1) = XTOT(2,1) + TOTQTY
          DO CASE
            CASE cStygrade = '1' .OR. EMPTY(cStygrade)
              XTOT(1,1) = XTOT(1,1) + TOTQTY
              *B605458,1 ABD - Accumulate the 2nd and damge qty on the type. [Begin]
            CASE cStygrade = '2'
              la2ndQty(1,1)  = la2ndQty(1,1) + TOTQTY
              la2ndQty(2,1)  = la2ndQty(2,1) + TOTQTY
            CASE cStygrade = '3'
              laDmgdQty(1,1) = laDmgdQty(1,1)+ TOTQTY
              laDmgdQty(2,1) = laDmgdQty(2,1)+ TOTQTY
          ENDCASE    
        ELSE
          XTOT(1,1) = XTOT(1,1) + TOTQTY
          XTOT(2,1) = XTOT(2,1) + TOTQTY
        ENDIF  
          *B605458,1 ABD -  [End]

        *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
        *XTOT(1,3) = XTOT(1,3) + DMGQTY
        *XTOT(2,3) = XTOT(2,3) + DMGQTY
        *B603161,1 WAB -(END)

        IF XAMOUNT = 'Y'
          *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
          *B603161,1 WAB -(START) 
          *XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
          *XTOT(1,4) = XTOT(1,4) + DMGQTY * PRICE
          *XTOT(2,2) = XTOT(2,2) + TOTQTY * PRICE
          *XTOT(2,4) = XTOT(2,4) + DMGQTY * PRICE

          IF llMultCurr .AND. lcRpCurr <> "F"
            *B605458,1 ABD - Remark the next line & Accumulate the 2nd and damge qty on the type. [Begin]
            *XTOT(1,2) = XTOT(1,2) + lfBaseAmt(TOTQTY * PRICE) 
            *XTOT(2,2) = XTOT(2,2) + lfBaseAmt(TOTQTY * PRICE) 
            XTOT(2,2) = XTOT(2,2) + lfBaseAmt(TOTQTY * PRICE) 
            IF XREPORT $ 'RA'
              DO CASE
                CASE cStygrade = '1'
                  XTOT(1,2) = XTOT(1,2) + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '2'
                  la2ndQty(1,2)  = la2ndQty(1,2) + lfBaseAmt(TOTQTY * PRICE) 
                  la2ndQty(2,2)  = la2ndQty(2,2) + lfBaseAmt(TOTQTY * PRICE) 
                CASE cStygrade = '3'
                  laDmgdQty(1,2) = laDmgdQty(1,2)+ lfBaseAmt(TOTQTY * PRICE) 
                  laDmgdQty(2,2) = laDmgdQty(2,2)+ lfBaseAmt(TOTQTY * PRICE) 
              ENDCASE
            ELSE
              XTOT(1,2) = XTOT(1,2) + lfBaseAmt(TOTQTY * PRICE) 
            ENDIF
              *B605458,1 ABD - [End]
            
          ELSE
    
            *B605458,1 ABD - Remark the next line & Accumulate the 2nd and damge qty on the type. [Begin]
            *XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
            *XTOT(2,2) = XTOT(2,2) + TOTQTY * PRICE
            XTOT(2,2) = XTOT(2,2) + TOTQTY * PRICE
            IF XREPORT $ 'RA'
              *B605458,1 ABD - [End]
              DO CASE
                CASE cStygrade = '1' .OR. EMPTY(cStygrade)
                  XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
                  *B605458,1 ABD - Remark the next line & Accumulate the 2nd and damge qty on the type. [Begin]
                CASE cStygrade = '2'
                  la2ndQty(1,2)  = la2ndQty(1,2) + TOTQTY * PRICE
                  la2ndQty(2,2)  = la2ndQty(2,2) + TOTQTY * PRICE
                CASE cStygrade = '3'
                  laDmgdQty(1,2) = laDmgdQty(1,2)+ TOTQTY * PRICE
                  laDmgdQty(2,2) = laDmgdQty(2,2)+ TOTQTY * PRICE
              ENDCASE
            ELSE
              XTOT(1,2) = XTOT(1,2) + TOTQTY * PRICE
            ENDIF
            *B605458,1 ABD - [End]
            
          ENDIF
          *B603161,1 WAB -(END)
        ENDIF
      ENDIF
      IF XBOTH_FLG
        IF !EMPTY(TYPE)
  
          *B605458,1 ABD - Remark the next line & Accumulate the 2nd and damge qty on the type. [Begin]
          *XTOT_B(1,1) = XTOT_B(1,1) + TOTQTY
          XTOT_B(2,1) = XTOT_B(2,1) + TOTQTY
          IF XREPORT $ 'RA'
            DO CASE
              CASE cStygrade = '1'
                *B605458,1 ABD - [END]
                XTOT_B(1,1) = XTOT_B(1,1) + TOTQTY
                *B605458,1 ABD - Remark the next line & Accumulate the 2nd and damge qty on the type. [Begin]
              CASE cStygrade = '2'
                la2ndQty(1,1)  = la2ndQty(1,1) + TOTQTY
                la2ndQty(2,1)  = la2ndQty(2,1) + TOTQTY
              CASE cStygrade = '3'
                laDmgdQty(1,1) = laDmgdQty(1,1)+ TOTQTY
                laDmgdQty(2,1) = laDmgdQty(2,1)+ TOTQTY
            ENDCASE
          ELSE
            XTOT_B(1,1) = XTOT_B(1,1) + TOTQTY
          ENDIF
            *B605458,1 ABD - [End]
          
          
          *B603161,1 WAB -(START)Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
          *XTOT_B(1,3) = XTOT_B(1,3) + DMGQTY
          *XTOT_B(2,3) = XTOT_B(2,3) + DMGQTY
          *B603161,1 WAB -(END)

          IF XAMOUNT = 'Y'

            *B603161,1 WAB -Remove damage qty & damage amount cause The 2 fields not used in ARIA27 
            *B603161,1 WAB -& acumulate total amount the equivalent amount in case of multi currency
            *B603161,1 WAB -(START) 
            *XTOT_B(1,2) = XTOT_B(1,2) + TOTQTY * PRICE
            *XTOT_B(1,4) = XTOT_B(1,4) + DMGQTY * PRICE
            *XTOT_B(2,2) = XTOT_B(2,2) + TOTQTY * PRICE
            *XTOT_B(2,4) = XTOT_B(2,4) + DMGQTY * PRICE

            IF llMultCurr .AND. lcRpCurr <> "F"
              XTOT_B(1,2) = XTOT_B(1,2) + lfBaseAmt(TOTQTY * PRICE) 
              XTOT_B(2,2) = XTOT_B(2,2) + lfBaseAmt(TOTQTY * PRICE) 
            ELSE
              XTOT_B(1,2) = XTOT_B(1,2) + TOTQTY * PRICE
              XTOT_B(2,2) = XTOT_B(2,2) + TOTQTY * PRICE
            ENDIF
            *B603161,1 WAB -(END)

          ENDIF
        ENDIF
      ENDIF
    ENDIF
    ROW = ROW + 1
    SELECT &XNAME
    SKIP
  ENDDO
ENDIF  && endif used dyelots
DO RA911       && calculate grand total
DO ENDREPORT                   && end report 
SET DEVICE TO SCREEN           && return set device to screen 
*RETURN                         && return to the caller which is gfopgrid (option grid program)
*-- end of report code
*--------------------- Functions' Section ---------------------
*--------------------------------------------------------------
****************************************************************************
* PROC:RA911
* DESC:PRINT THE GRAND TOTAL
* DATE:04/07/93
* AUTH:SHEREIN MOHAMED HAMDAN
****************************************************************************
PROCEDURE RA911

@ ROW,00 SAY REPLICATE('=',R_LEN)
ROW = ROW + 1
@ ROW,10 SAY 'GRAND TOTAL=>'
*-- if dyelots are used
IF llDyelot
  IF XBOTH_FLG
    *B603161,1 WAB -(START)- Rearrange the R/A text position
    *@ ROW,37+11 SAY 'R/A :'
    @ ROW,44 SAY 'R/A:'
    *B603161,1 WAB -(END)
  ENDIF
  
  IF SIZEB = 'Y'
  
     *B605458,1 ABD - Print grand total for the 2nd and damge qty. [Begin]
    IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
      ROW= ROW -1 
      = lfPrntGTot()
      ROW = ROW + 1
      @ ROW,32 SAY 'TOTAL RECEIVED'
      @ ROW,47 SAY ":"
    ENDIF  
    *B605458,1 ABD - [End]
  
    @ ROW,051-3 SAY XTOT(2,1) PICTURE '99999'
    @ ROW,057-3 SAY XTOT(2,2) PICTURE '99999'
    @ ROW,063-3 SAY XTOT(2,3) PICTURE '99999'
    @ ROW,069-3 SAY XTOT(2,4) PICTURE '99999'
    @ ROW,075-3 SAY XTOT(2,5) PICTURE '99999'
    @ ROW,081-3 SAY XTOT(2,6) PICTURE '99999'
    @ ROW,087-3 SAY XTOT(2,7) PICTURE '99999'
    @ ROW,093-3 SAY XTOT(2,8) PICTURE '99999'
    @ ROW,099-3 SAY XTOT(2,9) PICTURE '9999999'


    *B603161,1 WAB -print Total amount by the equivalent amount in case of multi currency & 
    *B603161,1 WAB -display curency not by forein in other word if user choose display currency 
    *B603161,1 WAB -by foreign Currency there is no grand total for amount    
    *B603161,1 WAB -Remove total damage qty & total damage amount cause The 2 fields not used 
    *B603161,1 WAB -(START) in ARIA27 
    *IF XAMOUNT = 'Y'
      *@ ROW,106-3 SAY XTOT(2,10) PICTURE '9999999.99'
    *ENDIF      
    *IF XREPORT = 'R'   .OR. XREPORT = 'B'
    *  @ ROW,117-3 SAY XTOT(2,11) PICTURE '9999999'
    *  IF XAMOUNT = 'Y'
    *    @ ROW,124-3 SAY XTOT(2,12) PICTURE '9999999.99'
    *  ENDIF
    *ENDIF
    
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      *B603713,7 KAM 10/15/2000 increase element which contain total [start] 
      *@ ROW,107-3 SAY XTOT(2,10) PICTURE '99999999.99'
      @ ROW,107-3 SAY XTOT(2,10) PICTURE '999999999999.99'
      *B603713,7 KAM [end]
    ENDIF
    *B603161,1 WAB -(END) 
 
    *B605458,1 ABD - Print grand total for the 2nd and damge qty. [Begin]
    IF XBOTH_FLG
     = lfPrntGTot()
    ENDIF    
    *B605458,1 ABD - [End]


    IF XBOTH_FLG
      ROW = ROW +1
      *B603161,1 WAB -(START)- Rearrange the RCV text position
      *@ ROW,37 SAY 'RCV :'
      *B605458,1 ABD - Print titel for total received. [Begin] 
      *@ ROW,44 SAY 'RCV:'
      @ ROW,32 SAY 'TOTAL RECEIVED'
      @ ROW,47 SAY ":"
      *B605458,1 ABD - [End]
      
      *B603161,1 WAB -(END)

      @ ROW,051-3 SAY XTOT_B(2,1) PICTURE '99999'
      @ ROW,057-3 SAY XTOT_B(2,2) PICTURE '99999'
      @ ROW,063-3 SAY XTOT_B(2,3) PICTURE '99999'
      @ ROW,069-3 SAY XTOT_B(2,4) PICTURE '99999'
      @ ROW,075-3 SAY XTOT_B(2,5) PICTURE '99999'
      @ ROW,081-3 SAY XTOT_B(2,6) PICTURE '99999'
      @ ROW,087-3 SAY XTOT_B(2,7) PICTURE '99999'
      @ ROW,093-3 SAY XTOT_B(2,8) PICTURE '99999'
      @ ROW,099-3 SAY XTOT_B(2,9) PICTURE '9999999'
      *B603161,1 WAB -print Total amount by the equivalent amount in case of multi currency & 
      *B603161,1 WAB -display curency not by forein in other word if user choose display currency 
      *B603161,1 WAB -by foreign Currency there is no grand total for amount & Remove total 
      *B603161,1 WAB -damage qty & total damage amount cause The 2 fields not used in ARIA27 
      *B603161,1 WAB -(START) 
      *IF XAMOUNT = 'Y'
      *  @ ROW,106-3 SAY XTOT_B(2,10) PICTURE '9999999.99'
      *ENDIF
      *@ ROW,117-3 SAY XTOT_B(2,11) PICTURE '9999999'
      *IF XAMOUNT = 'Y'
      *  @ ROW,124-3 SAY XTOT_B(2,12) PICTURE '9999999.99'
      *ENDIF

      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        *B603713,7 KAM 10/15/2000 increase element which contain total[start]
        *@ ROW,107-3 SAY XTOT_B(2,10) PICTURE '99999999.99'
        @ ROW,107-3 SAY XTOT_B(2,10) PICTURE '999999999999.99'
        *B603713,7 KAM[end]
      ENDIF        
      *B603161,1 WAB -(END)
    
    ENDIF
    
  ELSE
  
    *B605458,1 ABD - Print grand total for the 2nd and damge qty. [Begin]
    IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
      ROW= ROW -1 
      = lfPrntGTot()
      ROW = ROW + 1
      @ ROW,32 SAY 'TOTAL RECEIVED'
      @ ROW,47 SAY ":"
    ENDIF  
    *B605458,1 ABD - [End]

    @ ROW,52-3 SAY XTOT(2,1) PICTURE '9999999'

    *B603161,1 WAB -print Total amount by the equivalent amount in case of multi currency & 
    *B603161,1 WAB -display curency not by forein in other word if user choose display currency 
    *B603161,1 WAB -by foreign Currency there is no grand total for amount & Remove total 
    *B603161,1 WAB -damage qty & total damage amount cause The 2 fields not used in ARIA27 
    *B603161,1 WAB -(START) 
    *IF XAMOUNT = 'Y'
    *  @ ROW,61-3 SAY XTOT(2,2) PICTURE '9999999.99'
    *ENDIF
    *IF XREPORT = 'R'  .OR. XREPORT = 'B'
    *  @ ROW,73-3 SAY XTOT(2,3) PICTURE '9999999'
    *  IF XAMOUNT = 'Y'
    *    @ ROW,82-3 SAY XTOT(2,4) PICTURE '999999.99'
    *  ENDIF
    *ENDIF

    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      *B603713,7 KAM 10/24/2000 increase picture size 
      *@ ROW,62-3 SAY XTOT(2,2) PICTURE '99999999.99'
      @ ROW,62-3 SAY XTOT(2,2) PICTURE '9999999999.99'
      *B603713,7 KAM [end]
    ENDIF      
    *B603161,1 WAB -(END)

    IF XBOTH_FLG
      ROW = ROW + 1
      
      *B605458,1 ABD - Print grand total for the 2nd and damge qty. [Begin]
      ROW= ROW -1 
      = lfPrntGTot()
      ROW = ROW + 1
      @ ROW,32 SAY 'TOTAL RECEIVED'
      @ ROW,47 SAY ":"
      *B605458,1 ABD - [End]
      

      *B603161,1 WAB -(START) Rearange the 'RCV' text position
      *@ ROW,37-3 SAY 'RCV :'
      *B605458,1 ABD - PRint titel for the total received. [Begin] 
      *@ ROW,44 SAY 'RCV:'
      *B605458,1 ABD - [End]
      
      *B603161,1 WAB -(END)

      @ ROW,52-3 SAY XTOT_B(2,1) PICTURE '9999999'
      
      *B603161,1 WAB -print Total amount by the equivalent amount in case of multi currency & 
      *B603161,1 WAB -display curency not by forein in other word if user choose display currency 
      *B603161,1 WAB -by foreign Currency there is no grand total for amount & Remove total 
      *B603161,1 WAB -damage qty & total damage amount cause The 2 fields not used in ARIA27 
      *B603161,1 WAB -(START) 
      *IF XAMOUNT = 'Y'
      *  @ ROW,61-3 SAY XTOT_B(2,2) PICTURE '9999999.99'
      *ENDIF
      *@ ROW,73-3 SAY XTOT_B(2,3) PICTURE '9999999' 
      *IF XAMOUNT = 'Y'
      *  @ ROW,82-3 SAY XTOT_B(2,4) PICTURE '999999.99'
      *ENDIF

      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        *B603713,7 KAM 10/15/2000 increase element which contain total [start]
        *@ ROW,62-3 SAY XTOT_B(2,2) PICTURE '99999999.99'
        @ ROW,62-3 SAY XTOT_B(2,2) PICTURE '9999999999.99'
        *B603713,7 KAM [end]
      ENDIF
      *B603161,1 WAB -(END)

    ENDIF
  ENDIF
ELSE  && else no dyelots are used
  *B603161,1 WAB - commit this line & rearanger he printing position &
  *B603161,1 WAB -print Total amount by the equivalent amount in case of multi currency & 
  *B603161,1 WAB -display curency not by forein in other word if user choose display currency 
  *B603161,1 WAB -by foreign Currency there is no grand total for amount & Remove total 
  *B603161,1 WAB -damage qty & total damage amount cause The 2 fields not used in ARIA27 
  *B603161,1 WAB -(START) 
  *IF XBOTH_FLG
  *  @ ROW,37 SAY 'R/A :'
  *ENDIF
  *IF SIZEB = 'Y'
  *  @ ROW,051-03 SAY XTOT(2,1) PICTURE '99999'
  *  @ ROW,057-03 SAY XTOT(2,2) PICTURE '99999'
  *  @ ROW,063-03 SAY XTOT(2,3) PICTURE '99999'
  *  @ ROW,069-03 SAY XTOT(2,4) PICTURE '99999'
  *  @ ROW,075-03 SAY XTOT(2,5) PICTURE '99999'
  *  @ ROW,081-03 SAY XTOT(2,6) PICTURE '99999'
  *  @ ROW,087-03 SAY XTOT(2,7) PICTURE '99999'
  *  @ ROW,093-03 SAY XTOT(2,8) PICTURE '99999'
  *  @ ROW,099-03 SAY XTOT(2,9) PICTURE '9999999'
  *  IF XAMOUNT = 'Y'
  *    @ ROW,106-03 SAY XTOT(2,10) PICTURE '9999999.99'
  *  ENDIF
  *  IF XREPORT = 'R'   .OR. XREPORT = 'B'
  *    @ ROW,117-03 SAY XTOT(2,11) PICTURE '9999999'
  *    IF XAMOUNT = 'Y'
  *      @ ROW,124-03 SAY XTOT(2,12) PICTURE '9999999.99'
  *    ENDIF
  *  ENDIF
  *  IF XBOTH_FLG
  *    ROW = ROW +1
  *    @ ROW,37 SAY 'RCV :'
  *    @ ROW,051-03 SAY XTOT_B(2,1) PICTURE '99999'
  *    @ ROW,057-03 SAY XTOT_B(2,2) PICTURE '99999'
  *    @ ROW,063-03 SAY XTOT_B(2,3) PICTURE '99999'
  *    @ ROW,069-03 SAY XTOT_B(2,4) PICTURE '99999'
  *    @ ROW,075-03 SAY XTOT_B(2,5) PICTURE '99999'
  *    @ ROW,081-03 SAY XTOT_B(2,6) PICTURE '99999'
  *    @ ROW,087-03 SAY XTOT_B(2,7) PICTURE '99999'
  *    @ ROW,093-03 SAY XTOT_B(2,8) PICTURE '99999'
  *    @ ROW,099-03 SAY XTOT_B(2,9) PICTURE '9999999'
  *    IF XAMOUNT = 'Y'
  *       @ ROW,106-03 SAY   PICTURE '9999999.99'
  *    ENDIF
  *    @ ROW,117-03 SAY XTOT_B(2,11) PICTURE '9999999'
  *    IF XAMOUNT = 'Y'
  *      @ ROW,124-03 SAY XTOT_B(2,12) PICTURE '9999999.99'
  *    ENDIF
  *  ENDIF
  *ELSE
  *  @ ROW,52-03 SAY XTOT(2,1) PICTURE '9999999'
  *  IF XAMOUNT = 'Y'
  *    @ ROW,61-03 SAY XTOT(2,2) PICTURE '9999999.99'
  *  ENDIF
  *  IF XREPORT = 'R'  .OR. XREPORT = 'B'
  *    @ ROW,73-03 SAY XTOT(2,3) PICTURE '9999999'
  *    IF XAMOUNT = 'Y'
  *      @ ROW,82-03 SAY XTOT(2,4) PICTURE '999999.99'
  *    ENDIF
  *  ENDIF
  *  IF XBOTH_FLG
  *    ROW = ROW + 1
  *    @ ROW,37 SAY 'RCV :'
  *    @ ROW,52-03 SAY XTOT_B(2,1) PICTURE '9999999'
  *    IF XAMOUNT = 'Y'
  *      @ ROW,61-03 SAY XTOT_B(2,2) PICTURE '9999999.99'
  *    ENDIF
  *    @ ROW,73-03 SAY XTOT_B(2,3) PICTURE '9999999' 
  *    IF XAMOUNT = 'Y'
  *      @ ROW,82-03 SAY XTOT_B(2,4) PICTURE '999999.99'
  *    ENDIF
  *  ENDIF
  IF XBOTH_FLG
    *@ ROW,44 SAY 'R/A:'
    *B605458,1 ABD - Print titel R/A fro raturn authour. [Begin] 
    *@ ROW,44 SAY 'R/A:'
    @ ROW,44 SAY 'R/A'
    @ ROW,60 SAY ":"
    *B605458,1 ABD - [End]
    

    
  ENDIF
  IF SIZEB = 'Y'
    *B605458,1 ABD - Print grand total for the 2nd and damge qty. [Begin]
    IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
      ROW = ROW - 1
      = lfPrntGTot()
      ROW = ROW + 1
      @ ROW,44 SAY 'TOTAL RECEIVED'
      @ ROW,60 SAY ":"
    ENDIF 
    *B605458,1 ABD - [End]
    
    @ ROW,066-03 SAY XTOT(2,1) PICTURE '99999'
    @ ROW,072-03 SAY XTOT(2,2) PICTURE '99999'
    @ ROW,078-03 SAY XTOT(2,3) PICTURE '99999'
    @ ROW,084-03 SAY XTOT(2,4) PICTURE '99999'
    @ ROW,090-03 SAY XTOT(2,5) PICTURE '99999'
    @ ROW,096-03 SAY XTOT(2,6) PICTURE '99999'
    @ ROW,102-03 SAY XTOT(2,7) PICTURE '99999'
    @ ROW,108-03 SAY XTOT(2,8) PICTURE '99999'
    @ ROW,114-03 SAY XTOT(2,9) PICTURE '9999999'
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      *B603713,7 KAM 10/15/2000 increase element which contain total [start] 
      *@ ROW,122-03 SAY XTOT(2,10) PICTURE '99999999.99'
      @ ROW,122-03 SAY XTOT(2,10) PICTURE '999999999999.99'
      *B603713,7 KAM [end]
    ENDIF
    
    *B605458,1 ABD - Print grand total for the 2nd and damge qty. [Begin]
    IF XBOTH_FLG
     = lfPrntGTot()
    ENDIF    
    *B605458,1 ABD - [End]
      
    IF XBOTH_FLG
      ROW = ROW +1
      *B605458,1 ABD - print titel total received for received qty.  [Begin] 
      *@ ROW,44 SAY 'RCV :'
       @ ROW,44 SAY 'TOTAL RECEIVED'
       @ ROW,60 SAY ":"
      *B605458,1 ABD - [End]
      @ ROW,066-03 SAY XTOT_B(2,1) PICTURE '99999'
      @ ROW,072-03 SAY XTOT_B(2,2) PICTURE '99999'
      @ ROW,078-03 SAY XTOT_B(2,3) PICTURE '99999'
      @ ROW,084-03 SAY XTOT_B(2,4) PICTURE '99999'
      @ ROW,090-03 SAY XTOT_B(2,5) PICTURE '99999'
      @ ROW,096-03 SAY XTOT_B(2,6) PICTURE '99999'
      @ ROW,102-03 SAY XTOT_B(2,7) PICTURE '99999'
      @ ROW,108-03 SAY XTOT_B(2,8) PICTURE '99999'
      @ ROW,114-03 SAY XTOT_B(2,9) PICTURE '9999999'
      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
         *B603713,7 KAM 10/15/2000 increase element which contain total [start]        
         *@ ROW,122-03 SAY XTOT_B(2,10) PICTURE '99999999.99'
         @ ROW,122-03 SAY XTOT_B(2,10) PICTURE '999999999999.99'
         *B603713,7 KAM [end]
      ENDIF
    ENDIF
  ELSE
  
    *B605458,1 ABD - Print titel total received for the received line & Print grand total for the 2nd and damge qty. [Begin]
    IF XREPORT $ 'R' .OR. (XREPORT = 'A' .AND. !XBOTH_FLG)
      ROW= ROW -1 
      = lfPrntGTot()
      @ ROW,44 SAY 'TOTAL RECEIVED'
      @ ROW,60 SAY ":"
    ENDIF  
    *B605458,1 ABD - [End]
    
    @ ROW,67-03 SAY XTOT(2,1) PICTURE '9999999'
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      *B603713,7 KAM increase picture size [start]
      *@ ROW,77-03 SAY XTOT(2,2) PICTURE '99999999.99'
      @ ROW,77-03 SAY XTOT(2,2) PICTURE '9999999999.99'
      *B603713,7 KAM [end]
    ENDIF
    

    
    IF XBOTH_FLG

      *B605458,1 ABD - Print grand total for the 2nd and damge qty & titel . [Begin]
      = lfPrntGTot()
      @ ROW,44 SAY 'TOTAL RECEIVED'
      @ ROW,60 SAY ":"
      *ROW = ROW + 1
      *@ ROW,44 SAY 'RCV :'
      *B605458,1 ABD - [End]
      @ ROW,67-03 SAY XTOT_B(2,1) PICTURE '9999999'
      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        *B603713,7 KAM 10/15/2000 increase element which contain total [start]
        *@ ROW,77-03 SAY XTOT_B(2,2) PICTURE '99999999.99'
        @ ROW,77-03 SAY XTOT_B(2,2) PICTURE '9999999999.99'
        *B603713,7 KAM [end]
      ENDIF
    ENDIF


    
  ENDIF
  *B603161,1 WAB -(END) 
ENDIF  && endif dyelots are used
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',R_LEN)
RETURN

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo
ldFrom = LDATE
LDTO   = HDATE
lcTitle = 'Date range'
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPX')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
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
*! Example     : = lfvpbDateOk()
*!*************************************************************
*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*FUNCTION lfvpbDateOk
FUNCTION lfvpbOk
  *B603955,1 ABD - [End]

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  LDate = ldFrom
  HDate = ldTo
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
FUNCTION lfAdjSeg
STORE 0 TO lnFPos, lnDPos, lnZPos, lnGPos, lnCPos, lnOPos, lnTPos, ;
           lnQPos, lnSPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it, 
  *-- display the style major instead (style major will browse from the 
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1 
    LOOP
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'F'
      *-- If there are more than one "FREE" segment , get first one only
      lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style'
      ENDIF
    CASE laMajSeg[lnC,1] = 'D'
      lnDPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Division'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Z'
      lnZPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Season'
      ENDIF
    CASE laMajSeg[lnC,1] = 'G'
      lnGPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style Group'
      ENDIF
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Color'
      ENDIF
    CASE laMajSeg[lnC,1] = 'O'
      *-- If there are more than one "OTHER" segment , get first one only
      lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Other'
      ENDIF
    CASE laMajSeg[lnC,1] = 'T'
      lnTPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Make'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Q'
      lnQPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Quality'
      ENDIF
    CASE laMajSeg[lnC,1] = 'S'
      lnSPos = lnC
       IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Scale'
      ENDIF
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

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
SELECT FABRIC
SET ORDER TO TAG Fabric
SET ORDER TO SCALE IN SCALE  
SET ORDER TO SEGVAL IN ICSEGVAL
SET ORDER TO Codes    IN CODES   
lcOldValue = ' '
llMultWare = ALLTRIM(gfGetMemVar('M_WareHouse')) = 'Y' 
*llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*B603161,1 WAB -get the position of the fixed filter rethdr.cCurrCode . so we can replace the fixed 
*B603161,1 WAB -(START) filter with RETAUTH.CCURRCODE if the user choose open R/A
IF llMultCurr
  lnCurrPos  = lfItmPos('RETHDR.CCURRCODE')
ENDIF  
*B603161,1 WAB -(START) 
RETURN

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , llObjRet
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
*-- IF The user wants to Browse or the Account is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvWareCod
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : Validation function for the Warehouse Code field
*!*************************************************************
*! Called from : Warehouse Code field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvWareCod
PRIVATE lcObjName , lcObjVal
lcObjName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
*--IF The user want to Browse or if the Warehouse he entered is not in the
*--file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , lcOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF && end of if    
*-- end of lfvWareCod function .

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : Old Value for fbric/warehouse (in OG) 
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfWOldVal
*!*************************************************************
*-- SYS(18) Returns the name in upper-case of the memory variable, 
*-- array element or field used to create the current @ ... GET control.
FUNCTION lfWOldVal
lcOldVal = EVALUATE(SYS(18))
*-- end of lfWOldVal function .

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/12/1998
*! Purpose   : Validate function for the Customer Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc
PRIVATE lcItsName , lcItsVal , llObjRet
lcItsName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF    && End of IF
*-- end of lfvAcc.

*!*************************************************************
*! Name      : lfsrLoc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/01/98
*! Purpose   : Rise change Location flag, in range browse screen.
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
*! Example   : =lfsrLoc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrLoc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    *llChLoc = .T.
    GO TOP IN WAREHOUS
  CASE lcParm = 'R'
    llClearLoc = .F.
ENDCASE
*-- end of lfsrLoc.

*!*************************************************************
*! Name      : lfFillVars
*: Developer : WAB - Walid A. Wahab
*! Date      : 07/27/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*!B603161,1
*!*************************************************************
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF
  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF
*!*************************************************************
*! Name      : lfClearRep
*: Developer : WAB - Walid A. Wahab
*! Date      : 07/27/2000
*! Purpose   : Function called when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*!B603161,1
*!*************************************************************
FUNCTION lfClearRep

*---close files we open it
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT 
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfItmPos
*: Developer : WAB - Walid A. Wahab
*! Date      : 07/27/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfItmPos()
*!*************************************************************
*!B603161,1
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos


*!*************************************************************
*! Name      : lfBaseAmt
*: Developer : WAB - Walid A. Wahab
*! Date      : 07/27/2000
*! Purpose   : Compute equivalent based amount
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfBaseAmt()
*!*************************************************************
*!B603161,1
*!*************************************************************
FUNCTION lfBaseAmt
PARAMETERS lnAmntCurr

*-- call function to get the equivalent amount .
IF lnAmntCurr <> 0
  lnAmntCurr = gfAmntDisp(lnAmntCurr,lcRpCurr,ldRpExDate,lcRpTmpNam)
ENDIF
RETURN lnAmntCurr




*kam add code of gfCodDes for testing

FUNCTION gfCodDes
*E301238,1 Add New logical parameter control print both code and description for editable codes,
*E301238,1 else print description only. [Begin]
*PARAMETERS lcCodeVal, lcFldName
PARAMETERS lcCodeVal, lcFldName , llChkEdit
*E301238,1 else print description only. [End  ]

PRIVATE lcSavOrder,lcReturnVal,lcSavSelct

*E301278,4 Rise array flag and the program is the same if it is direct value. [Begin]
llRetArray = (TYPE("lcCodeVal[1,1]") = "C")
*IF TYPE("lcCodeVal") <> "C" OR TYPE("lcFldName") <> "C"
IF !llRetArray AND (TYPE("lcCodeVal") <> "C" OR TYPE("lcFldName") <> "C")
  RETURN ''
ENDIF

IF !llRetArray
  lcCodeVal   = PADR(UPPER(lcCodeVal),6)
  lcFldName   = PADR(UPPER(lcFldName),10)
ENDIF  
*E301278,4 Rise array flag and the program is the same if it is direct value. [End  ]

lcReturnVal = ""
lcSavSelct  = ALIAS()      && Variable to save the currently selected file.

llUseCodes = .F.
IF !USED("CODES")
  USE (gcDataDir+"Codes") IN 0
  llUseCodes = .T.
ENDIF

SELECT CODES               && Select CODES file
lcSavOrder = SYS(22)       && Save the file order
SET ORDER TO TAG CODES     && Change the order

*E301278,4 if it is an array loop it to get all values. [Begin]
*-- if pass array of codes.
IF llRetArray
  PRIVATE lnArrLen , lnCodeLen
  lnCodeLen = 6
  lnArrLen  = 0
  FOR lnArrLen = 1 TO ALEN(lcCodeVal,1)
    lcCodeVal[lnArrLen,1] = PADR(UPPER(lcCodeVal[lnArrLen,1]),6)
    lcCodeVal[lnArrLen,2] = PADR(UPPER(lcCodeVal[lnArrLen,2]),10)
    IF EMPTY(lcCodeVal[lnArrLen,1]) .OR. lcCodeVal[lnArrLen,1] = "*"
      lcCurFlt = FILTER()
      lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
      SET FILTER TO
      *MAN Changed lcCodeVal to lcCodeVal[lnArrLen,1] Start
      IF !SEEK(SPACE(1)+LEFT(lcCodeVal[lnArrLen,1],1))
        APPEND BLANK
        REPLACE cFld_Name  WITH IIF(lcCodeVal[lnArrLen,1] = '*','ALL','N/A') ;
                cCode_No   WITH IIF(lcCodeVal[lnArrLen,1] = '*','*','')      ;
                cDiscrep   WITH IIF(lcCodeVal[lnArrLen,1] = '*','All','N/A') ;
                cRltField  WITH 'N'
      ENDIF  
      lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      SET FILTER TO &lcCurFlt.
      IF BETWEEN(lnFltRec,1,RECCOUNT())
        GO lnFltRec
      ENDIF    

    ELSE
  
      IF SEEK('N' + lcCodeVal[lnArrLen,1] + "N" + lcCodeVal[lnArrLen,2])  
        lcCodeVal[lnArrLen,3] = CODES.cDiscrep
      ELSE
        lcCodeVal[lnArrLen,3] = ''       && In case of this code record is deleted
      ENDIF  

    ENDIF

    IF !EMPTY(lcCodeVal[lnArrLen,3]) AND gfIsEdtble(ALLTRIM(lcCodeVal[lnArrLen,2]) , @lnCodeLen)
       lcCodeVal[lnArrLen,3] = PADR(lcCodeVal[lnArrLen,1],lnCodeLen) + '-' + lcCodeVal[lnArrLen,3]
    ENDIF

  ENDFOR
*E301278,4 if it is an array loop it to get all values. [End  ]

*E301278,4 else it is direct value then do original code [Begin]
ELSE && Pass one code only

  IF EMPTY(lcCodeVal) .OR. lcCodeVal = "*"
    *B801042,1 MAN Save Filter Start
    lcCurFlt = FILTER()
    lnFltRec = IIF(EOF() .OR. BOF(), 0, RECNO())
    SET FILTER TO
    *B801042,1 MAN Save Filter End
    *E300789,1 Heaham (Start)
    *IF !SEEK(SPACE(2)+lcCodeVal)
    *MAN Some Programs call the function with "******"
    *IF !SEEK(SPACE(1)+lcCodeVal)
    IF !SEEK(SPACE(1)+LEFT(lcCodeVal,1))
    *E300789,1 Heaham (End)
      APPEND BLANK
      REPLACE cFld_Name  WITH IIF(lcCodeVal = '*','ALL','N/A') ;
              cCode_No   WITH IIF(lcCodeVal = '*','*','')      ;
              cDiscrep   WITH IIF(lcCodeVal = '*','All','N/A') ;
              cRltField  WITH 'N'
    ENDIF  
    lcReturnVal = CODES.cDiscrep
    *B801042,1 MAN Restore Filter Start
    SET FILTER TO &lcCurFlt.
    IF BETWEEN(lnFltRec,1,RECCOUNT())
      GO lnFltRec
    ENDIF    
    *B801042,1 MAN Restore Filter End
  ELSE
    *E300789,1 Heaham (Start)
    *IF SEEK(gcAct_Comp + lcCodeVal + "N" + lcFldName)
    IF SEEK('N' + lcCodeVal + "N" + lcFldName)  
    *E300789,1 Heaham (End)
      lcReturnVal = CODES.cDiscrep
    ELSE
      lcReturnVal = ''       && In case of this code record is deleted
    ENDIF  
  ENDIF  

  *E301238,1 Add Code + Description if editable code, else nothing to do. [Begin]
  PRIVATE lnCodeLen
  lnCodeLen = 6
  IF llChkEdit AND !EMPTY(lcReturnVal) AND gfIsEdtble(ALLTRIM(lcFldName) , @lnCodeLen)
    lcReturnVal = PADR(lcCodeVal,lnCodeLen) + '-' + lcReturnVal
  ENDIF
  *E301238,1 Add Code + Description if editable code, else nothing to do. [End  ]

*E301278,4 else it is direct value then do original code [End  ]

ENDIF

SET ORDER TO &lcSavOrder
IF llUseCodes
  USE IN Codes
ENDIF
SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)
RETURN lcReturnVal
*-- end of gfCodDes.
*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Mohamed Shokry
*! Date      : 08/28/2001
*! Purpose   : Validate Reports
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************
*!E301643,1 
FUNCTION lfSortDumy
IF llCrIstall
  DIMENSION laSortItem[4,1], laSortVal[4,1]
  
  laSortItem[1]= 'Open R/A'  
  laSortItem[2]= 'Electronic R/A' 
  laSortItem[3]= 'Received Returns'
  laSortItem[4]= 'All'
  
  laSortVal[1] = 'O'
  laSortVal[2] = 'E'
  laSortVal[3] = 'R'
  laSortVal[4] = 'A'
  
ELSE
  DIMENSION laSortItem[3,1] , laSortVal[3,1]
  
  laSortItem[1]= 'Open R/A'  
  laSortItem[2]= 'Received Returns'
  laSortItem[3]= 'Both'
  
  laSortVal[1] = 'O'
  laSortVal[2] = 'R'
  laSortVal[3] = 'A'
ENDIF           


*!*************************************************************
*! Name      : lfPrntGTot
*! Developer : Abdou ElGendy [ABD]
*! Date      : 04/18/2002
*! Purpose   : Print the G. Total Qty. B# 605458,1 
*!*************************************************************
*! Called from : Program.
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrntGTot()
*!*************************************************************
*B605458,1 ABD - [Begin]
FUNCTION lfPrntGTot

IF llDyelot

  IF SIZEB = 'Y'
    *-- Print Frst Quality
    ROW = ROW +1
    @ ROW,32 SAY '1ST QUALITY'
    @ ROW,47 SAY ":"
    @ ROW,051-3 SAY laGntTotal(1,1) PICTURE '99999'
    @ ROW,057-3 SAY laGntTotal(1,2) PICTURE '99999'
    @ ROW,063-3 SAY laGntTotal(1,3) PICTURE '99999'
    @ ROW,069-3 SAY laGntTotal(1,4) PICTURE '99999'
    @ ROW,075-3 SAY laGntTotal(1,5) PICTURE '99999'
    @ ROW,081-3 SAY laGntTotal(1,6) PICTURE '99999'
    @ ROW,087-3 SAY laGntTotal(1,7) PICTURE '99999'
    @ ROW,093-3 SAY laGntTotal(1,8) PICTURE '99999'
    @ ROW,099-3 SAY laGntTotal(1,9) PICTURE '9999999'
  
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      @ ROW,107-03 SAY laGntTotal(1,10) PICTURE '999999999999.99'
    ENDIF

    *- Print 2nd Quality      
    ROW = ROW +1
    @ ROW,32 SAY '2ND QUALITY'
    @ ROW,47 SAY ":"
    @ ROW,051-3 SAY laGntTotal(2,1) PICTURE '99999'
    @ ROW,057-3 SAY laGntTotal(2,2) PICTURE '99999'
    @ ROW,063-3 SAY laGntTotal(2,3) PICTURE '99999'
    @ ROW,069-3 SAY laGntTotal(2,4) PICTURE '99999'
    @ ROW,075-3 SAY laGntTotal(2,5) PICTURE '99999'
    @ ROW,081-3 SAY laGntTotal(2,6) PICTURE '99999'
    @ ROW,087-3 SAY laGntTotal(2,7) PICTURE '99999'
    @ ROW,093-3 SAY laGntTotal(2,8) PICTURE '99999'
    @ ROW,099-3 SAY laGntTotal(2,9) PICTURE '9999999'

  
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      @ ROW,107-03 SAY laGntTotal(2,10) PICTURE '999999999999.99'
    ENDIF
  
    *- Print Damge Qty. 
    ROW = ROW +1
    @ ROW,32 SAY 'DAMAGE'
    @ ROW,47 SAY ":"

    @ ROW,051-3 SAY laGntTotal(3,1) PICTURE '99999'
    @ ROW,057-3 SAY laGntTotal(3,2) PICTURE '99999'
    @ ROW,063-3 SAY laGntTotal(3,3) PICTURE '99999'
    @ ROW,069-3 SAY laGntTotal(3,4) PICTURE '99999'
    @ ROW,075-3 SAY laGntTotal(3,5) PICTURE '99999'
    @ ROW,081-3 SAY laGntTotal(3,6) PICTURE '99999'
    @ ROW,087-3 SAY laGntTotal(3,7) PICTURE '99999'
    @ ROW,093-3 SAY laGntTotal(3,8) PICTURE '99999'
    @ ROW,099-3 SAY laGntTotal(3,9) PICTURE '9999999'

  
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      @ ROW,107-03 SAY laGntTotal(3,10) PICTURE '999999999999.99'
    ENDIF
  ELSE
    IF XREPORT $ 'RA'
      *-- Print First Qty.
      ROW = ROW + 1
      @ ROW,32 SAY '1ST QUALITY'
      @ ROW,47 SAY ":"
      @ ROW,052-03 SAY laGntTotal(1,1) PICTURE '9999999'

      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        @ ROW,62-3 SAY laGntTotal(1,2) PICTURE '9999999999.99'
      ENDIF
      
      ROW = ROW + 1

      *-- Print 2nd Qty.
      @ ROW,32 SAY '2ND QUALITY'
      @ ROW,47 SAY ":"
      @ ROW,052-03 SAY laGntTotal(2,1) PICTURE '9999999'
      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        @ ROW,62-3 SAY laGntTotal(2,2) PICTURE '9999999999.99'
      ENDIF
      
      ROW = ROW + 1

      *-- Print Damge Qty.
      @ ROW,32 SAY 'DAMGE'
      @ ROW,47 SAY ":"
      @ ROW,052-03 SAY laGntTotal(3,1) PICTURE '9999999'
      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        @ ROW,62-3 SAY laGntTotal(3,2) PICTURE '9999999999.99'
      ENDIF
      
    ENDIF  
  ENDIF


ELSE

  IF SIZEB = 'Y'
    *-- Print Frst Quality
    ROW = ROW +1
    @ ROW,44 SAY '1ST QUALITY'
    @ ROW,60 SAY ":"
    @ ROW,066-03 SAY laGntTotal(1,1) PICTURE '99999'
    @ ROW,072-03 SAY laGntTotal(1,2) PICTURE '99999'
    @ ROW,078-03 SAY laGntTotal(1,3) PICTURE '99999'
    @ ROW,084-03 SAY laGntTotal(1,4) PICTURE '99999'
    @ ROW,090-03 SAY laGntTotal(1,5) PICTURE '99999'
    @ ROW,096-03 SAY laGntTotal(1,6) PICTURE '99999'
    @ ROW,102-03 SAY laGntTotal(1,7) PICTURE '99999'
    @ ROW,108-03 SAY laGntTotal(1,8) PICTURE '99999'
    @ ROW,114-03 SAY laGntTotal(1,9) PICTURE '9999999'
  
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      @ ROW,122-03 SAY laGntTotal(1,10) PICTURE '999999999999.99'
    ENDIF
        
    *- Print 2nd Quality      
    ROW = ROW +1
    @ ROW,44 SAY '2ND QUALITY'
    @ ROW,60 SAY ":"
    @ ROW,066-03 SAY laGntTotal(2,1) PICTURE '99999'
    @ ROW,072-03 SAY laGntTotal(2,2) PICTURE '99999'
    @ ROW,078-03 SAY laGntTotal(2,3) PICTURE '99999'
    @ ROW,084-03 SAY laGntTotal(2,4) PICTURE '99999'
    @ ROW,090-03 SAY laGntTotal(2,5) PICTURE '99999'
    @ ROW,096-03 SAY laGntTotal(2,6) PICTURE '99999'
    @ ROW,102-03 SAY laGntTotal(2,7) PICTURE '99999'
    @ ROW,108-03 SAY laGntTotal(2,8) PICTURE '99999'
    @ ROW,114-03 SAY laGntTotal(2,9) PICTURE '9999999'
  
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      @ ROW,122-03 SAY laGntTotal(2,10) PICTURE '999999999999.99'
    ENDIF
  
    *- Print Damge Qty. 
    ROW = ROW +1
    @ ROW,44 SAY 'DAMAGE'
    @ ROW,60 SAY ":"
    @ ROW,066-03 SAY laGntTotal(3,1) PICTURE '99999'
    @ ROW,072-03 SAY laGntTotal(3,2) PICTURE '99999'
    @ ROW,078-03 SAY laGntTotal(3,3) PICTURE '99999'
    @ ROW,084-03 SAY laGntTotal(3,4) PICTURE '99999'
    @ ROW,090-03 SAY laGntTotal(3,5) PICTURE '99999'
    @ ROW,096-03 SAY laGntTotal(3,6) PICTURE '99999'
    @ ROW,102-03 SAY laGntTotal(3,7) PICTURE '99999'
    @ ROW,108-03 SAY laGntTotal(3,8) PICTURE '99999'
    @ ROW,114-03 SAY laGntTotal(3,9) PICTURE '9999999'
  
    IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
      @ ROW,122-03 SAY laGntTotal(3,10) PICTURE '999999999999.99'
    ENDIF
  ELSE
  
    IF XREPORT $ 'RA'
      *-- Print First Qty.
      ROW = ROW + 1
      @ ROW,44 SAY '1ST QUALITY'
      @ ROW,60 SAY ":"
      @ ROW,067-03 SAY laGntTotal(1,1) PICTURE '9999999'
      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        @ ROW,77-03 SAY laGntTotal(1,2) PICTURE '9999999999.99'
      ENDIF
      
      ROW = ROW + 1

      *-- Print 2nd Qty.
      @ ROW,44 SAY '2ND QUALITY'
      @ ROW,60 SAY ":"
      @ ROW,067-03 SAY laGntTotal(2,1) PICTURE '9999999'
      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        @ ROW,77-03 SAY laGntTotal(2,2) PICTURE '9999999999.99'
      ENDIF

      ROW = ROW + 1

      *-- Print Damge Qty.
      @ ROW,44 SAY 'DAMGE'
      @ ROW,60 SAY ":"
      @ ROW,067-03 SAY laGntTotal(3,1) PICTURE '9999999'
      IF XAMOUNT = 'Y' .AND. (!llMultCurr .OR. lcRpCurr # "F")
        @ ROW,77-03 SAY laGntTotal(3,2) PICTURE '9999999999.99'
      ENDIF
      
      ROW = ROW + 1
    ENDIF  
  ENDIF
ENDIF
      
*-- End OF lfPrntGTot
*B605458,1 ABD - [End]
*!*************************************************************x