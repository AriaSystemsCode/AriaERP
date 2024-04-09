*:************************************************************************
*: Program file  : SRBOKNG.Prg          (N037465)
*: Program desc. : SALESREP  Boking
*: System        : Aria Advantage Series VER. 4 XP
*: Module        : SR
*: Developer     : Mariam Mazhar
*: Date          : 03/05/2006 2:45:40
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SRBOKNG
*:************************************************************************
*:MODIFICATIONS :
*!E302310,1 AYM 11/21/2006 Converting SRBOKNG to graphics landscape. (T20060809.0036)
*!B608169,1 TMI 07/18/2007 collect the count of accounts per Rep/Currency , Rep 
*!B608410,1 WAM 01/20/2008 Fix relation settings into customer table
*!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[T20101118.0007]
*!B609686,1 WAM 10/04/2011 Exclude EDI temporary orders from sales rep. booking report [T20110811.0011]
*:************************************************************************

lcTime = TIME()
IF loOGScroll.llOGFltCh
  IF USED(lcOrdhTmp)
    USE IN (lcOrdhTmp)
  ENDIF
  IF USED(lcOrdhTmp1)
    USE IN (lcOrdhTmp1)
  ENDIF
  IF USED(lcCustTmp)
    USE IN (lcCustTmp)
  ENDIF

  ERASE(oAriaApplication.WorkDir+lcOrdhTmp+'.DBF')
  ERASE(oAriaApplication.WorkDir+lcOrdhTmp+'.CDX')
  ERASE(oAriaApplication.WorkDir+lcOrdhTmp1+'.DBF')
  ERASE(oAriaApplication.WorkDir+lcOrdhTmp1+'.CDX')
  ERASE(oAriaApplication.WorkDir+lcCustTmp+'.DBF')

  lcStartEntDate = ""
  lcEndEntDate   = ""

  lcGrpCurr = ""
  llSameGrp = .F.

  llNodata = .F. && flag to check if there any data satisfies the selection criteria

   =lfCollectingData()

ENDIF 
*------ Collecting data  [End ] --------
IF llNodata 
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN 
ENDIF 
SELECT &lcOrdhTmp
LOCATE 
IF RECCOUNT(lcOrdhTmp)> 0 
  WAIT WINDOW 'SELECTED ' + ALLTRIM(STR(RECCOUNT(lcOrdhTmp))) +  ' RECORDS FOR REPORT' TIMEOUT 1
ENDIF   
*!E302310 : AYM..converting SRBOKNG to graphics landscape  ... Begin
IF UPPER(ALLTRIM(lcRpForm))='SRBOKNGA'
 loogScroll.cCROrientation = 'P'
ELSE
 loogScroll.cCROrientation = 'L'
ENDIF
*loogScroll.cCROrientation = 'P'
*!E302310 : AYM..converting SRBOKNG to graphics landscape  ... End

*B608169,1 TMI [Start] collect the count of accounts per Rep/Currency , Rep in two cursors
IF UPPER(ALLTRIM(lcRpForm))=='SRBOKNG'
  SELECT IIF(FLAG='2',REP2,REP1) AS REP , COUNT(DISTINCT ACCOUNT) AS COUNT FROM &lcOrdhTmp GROUP BY REP INTO CURSOR ACNTS
  INDEX ON REP TAG ACNTS
  IF  llMultCurr .AND. lcRpCurr = 'F'
    SELECT IIF(FLAG='2',REP2,REP1) AS REP , CCURRCODE , COUNT(DISTINCT ACCOUNT) AS COUNT FROM &lcOrdhTmp GROUP BY REP,CCURRCODE INTO CURSOR CURR
  ELSE
    CREATE CURSOR CURR (REP C(3),CCURRCODE C(3),COUNT N(3))
  ENDIF
  INDEX ON REP+CCURRCODE TAG CURR
  SELECT &lcOrdhTmp
  *B608410,1 WAM 01/20/2008 Do not overwrite relation into customer table already set 
  *SET RELATION TO IIF(FLAG='2',REP2,REP1) INTO ACNTS,;
                  IIF(FLAG='2',REP2,REP1)+CCURRCODE INTO CURR
  SET RELATION TO IIF(FLAG='2',REP2,REP1) INTO ACNTS,;
                  IIF(FLAG='2',REP2,REP1)+CCURRCODE INTO CURR ADDITIVE
  *B608410,1 WAM 01/20/2008 (End)
  LOCATE
ENDIF
*B608169,1 TMI [End  ] 

DO gfDispRe WITH EVAL('lcRpForm')
IF lcRpDivPrnt = 'Y'
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.ENABLED=.F.
ELSE
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.ENABLED=.T.
ENDIF

*B608169,1 TMI [Start] close opened cursors
IF UPPER(ALLTRIM(lcRpForm))=='SRBOKNG'
  IF USED('ACNTS')
    USE IN ACNTS
  ENDIF
  IF USED('CURR')
    USE IN CURR
  ENDIF
ENDIF
*B608169,1 TMI [End  ] 


*!*************************************************************
*! Name      : lfCollectingData
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/05/2006
*! Purpose   : Convert a list of values into a cursor
*!*************************************************************
*!
FUNCTION lfCollectingData

*B609686,1 WAM 10/04/2011 Exclude EDI temporary orders from sales rep. booking report
*lcFilterExp = ".T."
lcFilterExp = "CORDTYPE='O'"
*B609686,1 WAM 10/04/2011 (End)


*--Account filter
lnAccPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.ACCOUNT')
IF lnAccPos <> 0 
  lnAccPos     = ASUBSCRIPT(loOGScroll.laogFxflt,lnAccPos,1)
  lcAccFile    = loOGScroll.laogFxflt[lnAccPos,6]
  IF !EMPTY(lcAccFile) AND USED(lcAccFile) AND RECCOUNT(lcAccFile) > 0
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.ACCOUNT,'&lcAccFile')"
  ENDIF  
ENDIF 

*--SalesRep filter
lnRepPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.REP1')
IF lnRepPos <> 0 
  lnRepPos     = ASUBSCRIPT(loOGScroll.laogFxflt,lnRepPos,1)
  lcRepFile    = loOGScroll.laogFxflt[lnRepPos,6]
  IF !EMPTY(lcRepFile) AND USED(lcRepFile) AND RECCOUNT(lcRepFile) > 0  
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" (Seek(ORDHDR.REP1,'&lcRepFile') OR Seek(ORDHDR.REP2,'&lcRepFile'))"
  ENDIF  
ENDIF 

*--Order filter
lnOrderPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.ORDER')
IF lnOrderPos <> 0 
  lnOrderPos   = ASUBSCRIPT(loOGScroll.laogFxflt,lnOrderPos,1)
  lcOrdFile    = loOGScroll.laogFxflt[lnOrderPos,6]
  IF !EMPTY(lcOrdFile) AND USED(lcOrdFile) AND RECCOUNT(lcOrdFile) > 0  
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.ORDER,'&lcOrdFile')"
  ENDIF  
ENDIF 

*--Currency
IF llMultCurr
  lnCurrPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.CCURRCODE')
  IF lnCurrPos <> 0 
    lnCurrPos    = ASUBSCRIPT(loOGScroll.laogFxflt,lnCurrPos ,1)
    IF !EMPTY(loOGScroll.laogFxflt[lnCurrPos,6])
      lcTempCurr = loOGScroll.gfTempName()
      IF lfConvertToCursor(loOGScroll.laogFxflt[lnCurrPos,6],lcTempCurr,'CCURRCODE')
        lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.CCURRCODE,'&lcTempCurr')"    
      ENDIF 
    ENDIF 
  ENDIF 
ENDIF 

*--variable filter 
*--Entered date filter
lnEntPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.ENTERED')
IF lnEntPos  <> 0 
  lnEntPos       = ASUBSCRIPT(loOGScroll.laogVRflt,lnEntPos ,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnEntPos,6])
    lcStartEntDate = SUBSTR(loOGScroll.laogVRflt[lnEntPos,6],1,10)
    lcEndEntDate   = SUBSTR(loOGScroll.laogVRflt[lnEntPos,6],12,21)
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" BETWEEN(ORDHDR.ENTERED,CTOD('&lcStartEntDate'),CTOD('&lcEndEntDate'))"
  ENDIF  
ENDIF 

*--Start date filter
lnStartPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.START')
IF lnStartPos   <> 0 
  lnStartPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnStartPos ,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnStartPos ,6])
    lcStartstrtDate = SUBSTR(loOGScroll.laogVRflt[lnStartPos ,6],1,10)
    lcEndstrtDate   = SUBSTR(loOGScroll.laogVRflt[lnStartPos ,6],12,21)
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" BETWEEN(ORDHDR.START,CTOD('&lcStartstrtDate'),CTOD('&lcEndstrtDate'))"
  ENDIF  
ENDIF 

*--Complete date filter
lnCompPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.COMPLETE')
IF lnCompPos <> 0 
  lnCompPos  = ASUBSCRIPT(loOGScroll.laogVRflt,lnCompPos,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnCompPos ,6])
    lcStartComDate = SUBSTR(loOGScroll.laogVRflt[lnCompPos,6],1,10)
    lcEndComDate   = SUBSTR(loOGScroll.laogVRflt[lnCompPos ,6],12,21)
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" BETWEEN(ORDHDR.COMPLETE,CTOD('&lcStartComDate'),CTOD('&lcEndComDate'))"
  ENDIF  
ENDIF 

*--Specail instructions filter
lnSpcInstPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.SPCINST')
IF lnSpcInstPos  <> 0 
  lnSpcInstPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnSpcInstPos ,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnSpcInstPos ,6])
    lcTempSpc = loOGScroll.gfTempName()
    IF lfConvertToCursor(loOGScroll.laogVRflt[lnSpcInstPos ,6],lcTempSpc,'SPCINST')
      lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.SPCINST,'&lcTempSpc')"    
    ENDIF 
  ENDIF 
ENDIF 

*Cust Po filter 
lnCustPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.CUSTPO')
IF lnCustPos <> 0 
  lnCustPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnCustPos,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnCustPos,6])
    lcCustPo = loOGScroll.laogVRflt[lnCustPos,6]
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" ORDHDR.CUSTPO = '&lcCustPo'"
  ENDIF 
ENDIF 


*order priority 
lnPriPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.PRIORITY')
IF lnPriPos <> 0 
  lnPriPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnPriPos,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnPriPos,6])
    lcPriority  = loOGScroll.laogVRflt[lnPriPos,6]
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" ORDHDR.PRIORITY = '&lcPriority'"
  ENDIF 
ENDIF 

*Order Status filter 
lnStPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.STATUS')
IF lnStPos <> 0 
  lnStPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnStPos,1)
  IF !EMPTY(loOGScroll.laogVrflt[lnStPos,6])
    lcStatus = STRTRAN(loOGScroll.laogVrflt[lnStPos,6],"|","','")
    lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" INLIST(ORDHDR.STATUS,'&lcStatus')"      
  ENDIF 
ENDIF 

*Season filter
lnSeaPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.SEASON')
IF lnSeaPos <> 0 
  lnSeaPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnSeaPos,1)
  IF !EMPTY(loOGScroll.laogVrflt[lnSeaPos,6])
    lcTempSea = loOGScroll.gfTempName()
    IF lfConvertToCursor(loOGScroll.laogvrflt[lnSeaPos,6],lcTempSea,'SEASON')
      lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.SEASON,'&lcTempSea')"    
    ENDIF 
  ENDIF 
ENDIF 

*division filter
lnDivPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.CDIVISION')
IF lnDivPos <> 0 
  lnDivPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnDivPos,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnDivPos,6])
    lcTempDiv = loOGScroll.gfTempName()
    IF lfConvertToCursor(loOGScroll.laogvrflt[lnDivPos,6],lcTempDiv,'CDIVISION')
      lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.CDIVISION,'&lcTempDiv')"    
    ENDIF 
  ENDIF 
ENDIF 


*Term Code filter 
lnTermPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.CTERMCODE')
IF lnTermPos <> 0 
  lnTermPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnTermPos,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnTermPos ,6])
    lcTempTerm = loOGScroll.gfTempName()
    IF lfConvertToCursor(loOGScroll.laogVRflt[lnTermPos ,6],lcTempTerm,'CTERMCODE')
      lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.CTERMCODE,'&lcTempTerm')"    
    ENDIF 
  ENDIF 
ENDIF 


*ORDHDR.SHIPVIA
lnShipViaPos = ASCAN(loOGScroll.laogVRflt,'ORDHDR.SHIPVIA')
IF lnShipViaPos <> 0 
  lnShipViaPos = ASUBSCRIPT(loOGScroll.laogVRflt,lnShipViaPos,1)
  IF !EMPTY(loOGScroll.laogVRflt[lnShipViaPos,6])
    lcTempShip = loOGScroll.gfTempName()
    IF lfConvertToCursor(loOGScroll.laogVRflt[lnShipViaPos,6],lcTempShip,'SHIPVIA')
      lcFilterExp  = lcFilterExp +IIF(!EMPTY(lcFilterExp)," AND ", "")+" Seek(ORDHDR.SHIPVIA,'&lcTempShip')"    
    ENDIF 
  ENDIF 
ENDIF 

SELECT OrdHdr
LOCATE FOR &lcFilterExp  
IF EOF()
  llNodata = .T.
  RETURN
ENDIF

*------ Collecting data  [BEGIN] --------
COPY REST TO (oAriaApplication.WorkDir+lcOrdhTmp) FOR &lcFilterExp
SELECT CUSTOMER
COPY STRUCTURE TO (oAriaApplication.WorkDir+lcCustTmp) FIELDS SalesRep,Account

llRep_div = ALLTRIM(gfGetMemVar('M_REP_COMM' , oAriaApplication.ActiveCompanyID)) = 'D'
IF llRep_div
  = gfOpenFile(oAriaApplication.DataDir+'REP_DIV',oAriaApplication.DataDir+'REP_DIV','SH')
ENDIF
*MT
*=gfOpenFile(oAriaApplication.WorkDir+lcCustTmp,'','SH')
=gfOpenFile(oAriaApplication.WorkDir+lcCustTmp,'','EX')
*MT
INDEX ON  SalesRep+Account TAG &lcCustTmp
*MT
*=gfOpenFile(oAriaApplication.WorkDir+lcOrdhTmp,'','SH')
=gfOpenFile(oAriaApplication.WorkDir+lcOrdhTmp,'','EX')
*MT

*-- Function to Collect the correct data.
= lfCollect()

SELECT &lcOrdhTmp

IF !llRpPrntCo
  SET FILTER TO IIF(Flag<>"2",Comm1 <> 0,.T.)
ENDIF

LOCATE 
IF EOF()
  llNodata = .T.
  RETURN
ENDIF


lcGrpCurr = cCurrCode
llSameGrp = .T.

IF llMultCurr
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[Start]
  IF UPPER(ALLTRIM(lcRpForm))='SRBOKNGA' OR lcRpSortBy = 'O' 
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[End]
    INDEX ON IIF(FLAG='2',Rep2+cCurrCode+Order, Rep1+cCurrCode+Order) TAG &lcOrdhTmp
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[Start]
  ELSE
    INDEX ON IIF(FLAG='2',Rep2+cCurrCode+ACCOUNT+Order, Rep1+cCurrCode+ACCOUNT+Order) TAG &lcOrdhTmp
  ENDIF
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[End]
ELSE
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[Start]
  IF UPPER(ALLTRIM(lcRpForm))='SRBOKNGA' OR lcRpSortBy = 'O'
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[End]
    INDEX ON IIF(FLAG='2',Rep2+Order , Rep1+Order ) TAG &lcOrdhTmp
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[Start]
  ELSE
    INDEX ON IIF(FLAG='2',Rep2+ACCOUNT+Order , Rep1+ACCOUNT+Order) TAG &lcOrdhTmp
  ENDIF
  *!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[End]
ENDIF


SET RELATION TO IIF(STORE=SPACE(8),'M'+Account,'S'+Account+STORE) INTO CUSTOMER
LOCATE 


*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =fSeTOrdr()
*!**************************************************************************
*! Note      : symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSeTOrdr 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
    SELECT ORDHDR
    lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER && To customer file.
    GO TOP
  CASE OpGrdParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
ENDCASE
*!**************************************************************************
*! Name      : lfSetAcc
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid  && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetAcc()
*!**************************************************************************
*! Note      :  symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSetAcc
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'  && Set code
    SELECT CUSTOMER
    SET ORDER TO TAG CUSTOMER
    GO TOP IN CUSTOMER
  CASE OpGrdParm = 'R'  && Reset code
    SELECT CUSTOMER
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP 
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfDydWhen 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : set Last Order Start Ship Date = today()+dye 
*!             visibility window
*!**************************************************************************
*! Calls     : gfGetmemVar()
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfDydWhen()
*!**************************************************************************
FUNCTION lfDydWhen
PRIVATE lnarrPos
lnarrpos = INT(ASCAN(loogscroll.laOGFxFlt,'ORDHDR.START') / 7 + 0.9)
IF EMPTY(loogscroll.laOGFxFlt[lnarrpos,6])
  loogscroll.laOGFxFlt[lnarrpos,6] = DATE()+gfGetmemVar('M_DOVW',oAriaApplication.ActiveCompanyID)
ENDIF
*!**************************************************************************
*! Name      : lfGetPos  
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : Get Starting Position & lenth Of Color
*!**************************************************************************
*! Calls     : ()
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfGetPos()
*!**************************************************************************
FUNCTION lfGetPos
PRIVATE lnClrPos
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
 lnClrPos = int(ASCAN(laMajSeg,'C')/7+0.9)
 lnStrtPos = laMajSeg[lnClrPos,4] 
 lnFldLngth= LEN(laMajSeg[lnClrPos,3])
*!*************************************************************
*! Name      : lfFillVars
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE (oAriaApplication.SysPath+"SYCCOMP.DBF") ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (oAriaApplication.SysPath+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (oAriaApplication.SysPath+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
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

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfClearRep
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*B603547,1
FUNCTION lfClearRep
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
*-- End Of lfClearRep.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
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
*B603547,1
FUNCTION lfwRepWhen


**B603547,1 Evaluate Currency filter postion. [Begin]
IF llMultCurr
  lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
ELSE
  lcRpCurr = "O"
ENDIF  
*B603547,1 Evaluate Currency filter postion. [End  ]

*:B803305,1 ABD  variable that get the position of RepCode.[begin]
lnOrdPos = lfItmPos('ORDHDR.REP1')
*:B803305,1 ABD  [End]


*-- end of lfwRepWhen.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*!B603547,1 
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.

*!*************************************************************
*! Name      : lfSetCurr
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : From the FRX.
*!*************************************************************
*! Example   : =lfSetCurr()
*!*************************************************************
*B603547,1
Function lfSetCurr
lcGrpCurr = cCurrCode
llSameGrp = .T.
Return ""
*-- End Of lfSetCurr.
*!*************************************************************
*! Name      : lfSameCurr
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : From the FRX.
*!*************************************************************
*! Example     : =lfSameCurr()
*!*************************************************************
*B603547,1
Function lfSameCurr
IF (lcRpCurr= "F") AND llSameGrp
  llSameGrp = (lcGrpCurr = cCurrCode)
ENDIF

Return ""

*-- End Of lfSameCurr.
*!*************************************************************
*! Name      : lfCollect
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : function to collect the correct data.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCollect()
*!*************************************************************

FUNCTION lfCollect

lcTOrdFile = loOGScroll.laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
IF llWorkDeal
  SELECT (lcOrdhTmp)  
  COPY STRUCTURE TO (oAriaApplication.WorkDir+lcOrdhTmp1)
  =gfOpenFile(oAriaApplication.WorkDir+lcOrdhTmp1,'','SH')
  SELECT (lcOrdhTmp)
  SCAN
    *--If NOT based on Division or Based but Neither Rep1 nor Rep2 was assigned any dividion,
    *--go normally
    IF !llRep_div OR (!SEEK(Rep1,'REP_DIV') AND !SEEK(Rep2,'REP_DIV'))
      DO CASE 
        *-- REP1 is the required
        CASE !EMPTY(REP1) .AND. EMPTY(REP2)
          REPLACE FLAG WITH '1'
        *-- Both rep and rep2 are required
        CASE !EMPTY(REP1) .AND.  SEEK(REP1,lcTOrdFile)
          REPLACE FLAG WITH '1'
          IF !EMPTY(REP2) .AND. SEEK(REP2,lcTOrdFile)
            SCATTER MEMVAR MEMO
            M.fLAG = '2'
            INSERT INTO (lcOrdhTmp1) From MEMVAR
          ENDIF
        *-- Both rep and rep2 are required
        CASE !EMPTY(REP1) .AND.  !SEEK(REP1,lcTOrdFile) .AND. SEEK(REP2,lcTOrdFile)
          REPLACE FLAG WITH '2'
        *-- Rep2 is the required
        CASE EMPTY(REP1) .AND. SEEK(REP2,lcTOrdFile)
          REPLACE FLAG WITH '2'
      ENDCASE
    ELSE
      *--If this order has Rep2 and this order division is the division assigned to
      *--Rep2 in REP_DIV file, then this order belongs to Rep2 not Rep1
      IF !EMPTY(Rep2) AND SEEK(Rep2+cDivision,'REP_DIV') AND cDivision = REP_DIV.cDivision
        *-- Print it only when the user wants to print it.     
        IF SEEK(REP2,lcTOrdFile)
          REPLACE Flag WITH '2'
        ELSE
          *--This record belongs to Rep2 But the user doesn't want to print it.
          BLANK
          DELETE
        ENDIF
      ELSE
        *--Otherwise it belongs to Rep1, check if he wants to print it.
        IF !SEEK(REP1,lcTOrdFile)
          *--This record belongs to Rep1 But the use doesn't want to print it.
          BLANK
          DELETE
        ENDIF  
      ENDIF
    ENDIF
  ENDSCAN
  APPEND FROM (oAriaApplication.WorkDir+lcOrdhTmp1)
ELSE
  SELECT(lcOrdhTmp)
  *--If Sales rep based on division
  IF llRep_div
    COPY STRUCTURE TO (oAriaApplication.WorkDir+lcOrdhTmp1)
    =gfOpenFile(oAriaApplication.WorkDir+lcOrdhTmp1,'','SH')
    SELECT (lcOrdhTmp)
    SCAN
      IF SEEK(Rep1,'REP_DIV') OR SEEK(Rep2,'REP_DIV')
        *--If this order has Rep2 and this order division is the division assigned to
        *--Rep2 in REP_DIV file, then this order belongs to Rep2 not Rep1
        *--Otherwise do nothing as the current record already belongs to Rep1
        IF !EMPTY(Rep2) AND SEEK(Rep2+cDivision,'REP_DIV') AND cDivision = REP_DIV.cDivision
          REPLACE Flag WITH '2'
        ENDIF
*!*        ELSE
*!*          *--If neither Rep1 nor Rep2 was assigned a Division, Work Normally.
*!*          SCATTER MEMVAR MEMO
*!*          M.fLAG = '2'
*!*          INSERT INTO (lcOrdhTmp1) From MEMVAR
      ENDIF
    ENDSCAN
    SELECT &lcOrdhTmp
    APPEND FROM (oAriaApplication.WorkDir+lcOrdhTmp1)
  ELSE
    *--If Sales rep based on Company, go normally.
    COPY ALL TO (oAriaApplication.WorkDir+lcOrdhTmp1) FOR .NOT. EMPTY(Rep2) .AND. .NOT. EMPTY(Comm2)
    *SELECT(lcOrdhTmp1)
    =gfOpenFile(oAriaApplication.WorkDir+lcOrdhTmp1,'','SH')
    REPLACE ALL FLAG WITH '2'
    SELECT &lcOrdhTmp
    APPEND FROM (oAriaApplication.WorkDir+lcOrdhTmp1)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/05/2006
*! Purpose   : Convert a list of values into a cursor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcString,lcNewFile,lcField
lcFieldName = lcField
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1]=lcFieldName 

DO CASE 
CASE  ALLTRIM(lcFieldName) = 'CCURRCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 3 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CTERMCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'SHIPVIA'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'SPCINST'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
  
OTHERWISE &&SUBSTR(STYLE.STYLE,)
  lcFieldName = 'UnField'
  laTempacstru[1,1]= lcFieldName 
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcString
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : lfvForm
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/05/2006
*! Purpose   : select suitable form dependent on division selection
*!*************************************************************
FUNCTION lfvForm


IF lcRpDivPrnt = 'Y'
  lcRpForm = "SRBOKNGA"
  loOgScroll.lcOGLastForm = "SRBOKNGA"
  loOgScroll.lcRpFrxMod = "Graphics"
  *=lfRepPltFr(lcRpForm)
  lcRpFrxMod = loOgScroll.lcRpFrxMod 
  loogscroll.Parent.cntVariables.lcRepMode = loOgScroll.lcRpFrxMod 
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.displayValue   = loogscroll.Parent.cntVariables.lcRepMode
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.ENABLED=.F.

ELSE
  lcRpForm = 'SRBOKNG'
  loOgScroll.lcOGLastForm = "SRBOKNG"
  *=lfRepPltFr(lcRpForm)
  lcRpFrxMod = loOgScroll.lcRpFrxMod 
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.ENABLED=.T.
ENDIF

*!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[Start]
CLEARREAD()
*!E302816,1 MMT 12/14/2010 Add Sort by Option to Booking report[End]