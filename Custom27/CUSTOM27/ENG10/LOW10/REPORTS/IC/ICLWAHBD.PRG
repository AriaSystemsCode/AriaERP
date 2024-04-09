*:***************************************************************************
*: Program file  : ICLWAHDB
*: Program desc. : Ahead & Behind Report 
*: Customer      : Low Profile ( LOW10 )
*: For Report    : ICLWAHBD.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Entry         : *037373      
*:               : This is a Graphics report that will be exported to Microsoft Excel format
*:               : The layout of the XL sheet is how the report must look after it has been Exported.
*:***************************************************************************
*:Modifications  :
*:C123380,1 NNA 10/26/2004 Add new field (Shipped to Date) to the report to get the total shipment Qty.
*:C123651,1 NNA 10/27/2004 Fix bug that [Actual To Date] & [Call of To Date] don't take consider of
*:C123651,1 NNA            To date from the Option Grid so these data to the system Date
*C127561,1 MMR 07/10/2005 Add New columns and Print Titles on each page in The Excel
*C127561,1 MMR            and add option print decimal yes/no with modification in FRX
*:***************************************************************************

Private lcCollect , lnClrLen , lnClrPos

*- If no season selected do not continue
IF EMPTY(lcRpSeason)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You must select a season.')
  RETURN
ENDIF

*C123380,1 NNA 10/26/2004 (Begin) Open the Shpmthdr File
=gfOpenFile(gcDataDir+'SHPMTHDR',gcDataDir+'SHPMTHDR','SH')
*C123380,1 NNA (End)

lcStarTime = TIME()
*- The end of week date
*C037373,4  TMI [Start] an "From-To' screen added instead
*ldRpToDt = ldRpFromDt + 7
*C037373,4  TMI [End  ] 

lcCollect = gfTempName()

*C123651,1 NNA 10/27/2004 (BEGIN) use Coninvl file to get the Consolidated Qty.
IF !USED('CONSINVL')
  = gfOpenFile(gcDataDir+'CONSINVL',gcDataDir+'CINVLINE','SH') 
ENDIF
*C123651,1 NNA (END)

STORE 0 TO lnClrLen , lnClrPos
*- Get color position and lenght
=lfClrData()

*- Create needed temp file
=lfCrtTmpFl()

*- Collect data
=lfCollect()

*- Export or print
SELECT &lcCollect
LOCATE
IF !EOF()
  IF llRpExport
    =lfExprt2XL()
  ELSE
    DO gfDispRe WITH gcRepHome + 'IC\' + EVAL('lcRpForm')
  ENDIF  
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records to display.')
ENDIF  

*- Close temp created file
IF USED(lcCollect)
  USE IN &lcCollect
ENDIF  


*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2004
*:* Purpose     : When function for the OG
*:***************************************************************************
FUNCTION lfwRepWhen
*-- if it is the default case
IF lnOgSeting = 1
  SHOW GET pbRun DISABLE
  SHOW GET pbOGpreview DISABLE
ENDIF
*-- end of lfwRepWhen.
*:**************************************************************************
*:* Name        : lfCrtTmpFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Create temp files needed
*:***************************************************************************
FUNCTION lfCrtTmpFl
PRIVATE laFileStru,laIndx,lnI

DIMENSION laFileStru[1,4]
lnI = 1
laFileStru[lnI,1] = 'STYLE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CSTYMAJOR'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESC'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 20
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'COLORDES'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CPURCDES'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SeasonDES'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotCntrct'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'ACTWEEK'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'ACTTODATE'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PLANWEEK'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PLANTODATE'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PLUSMINUS'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STKVAL'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'INVWEEK'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'INVTODATE'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'INVVAL'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 13
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PriceA'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 12
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STKVALCP'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 12
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STKVALSP'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 12
laFileStru[lnI,4] = 2

*C123380,1 NNA 10/26/2004 (Begin) Add new Field to the Temp File
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SHPTODATE'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0
*C123380,1 NNA (End)
	
DIMENSION laIndx[1,2]
laIndx[1,1] = "STYLE"
laIndx[1,2] = "STYLE"

CREATE TABLE (gcWorkDir+lcCollect) FROM ARRAY laFileStru
INDEX ON STYLE TAG STYLE

*-- end of lfCrtTmpFl.

*:**************************************************************************
*:* Name        : lfClrData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Get color data
*:***************************************************************************
FUNCTION lfClrData
PRIVATE laItemSeg
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*-- end of lfClrData.

*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Collect data
*:***************************************************************************
FUNCTION lfCollect

*- Set appropriate orders
SET ORDER TO STYLE IN STYLE
SET ORDER TO ORDLINES IN ORDLINE
SET ORDER TO ORDCANLN IN ORDCANLN
SET ORDER TO POSLNS IN POSLN
SET ORDER TO INVLINEO IN INVLINE
SET ORDER TO INVHDR IN INVHDR
*C037373,4  TMI [Start] set order of STYINVJL file
SET ORDER TO STYINVJL IN STYINVJL
*C037373,4  TMI [End  ] 

SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR
LOCATE

*C037373,3  TMI [Start] Set relation for OrdLine and OrdHdr files
SET ORDER TO ORDHDR IN ORDHDR
SELECT ORDLINE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR
LOCATE
*C037373,3  TMI [End  ] 
SELECT STYLE
LOCATE
SCAN FOR &lcRpExp 
  IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
    SELECT &lcCollect
    SCATTER MEMVAR MEMORY BLANK
  ENDIF
  
  m.STYLE = STYLE.STYLE
  m.CSTYMAJOR = STYLE.CSTYMAJOR
  m.DESC = STYLE.DESC
  m.COLORDES = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) + '-' + ;
               ALLTRIM(gfCodDes(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),'COLOR'))
  m.CPURCDES = gfCodDes(STYLE.CPURCODE,'CPURCODE')
  m.SeasonDES = gfCodDes(STYLE.SEASON,'SEASON')
  m.PriceA = STYLE.PRICEA
  lnAlocated = 0
  
  SELECT ORDLINE
  =SEEK(STYLE.STYLE,'ORDLINE')
  *C037373,3  TMI [Start] Ask for the order status , include only Open , Hold and Complete orders
  *SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
            FOR ORDLINE.SEASON = STYLE.SEASON
  SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
            FOR ORDLINE.SEASON = STYLE.SEASON ;
            .AND. ORDHDR.STATUS $ 'OHC'
  *C037373,3  TMI [End  ]                        
    
    *- Get canceled from OrdCanLn file
    lnCanceled = 0
    IF SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'ORDCANLN')
      SELECT ORDCANLN
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
        lnCanceled = lnCanceled + ORDCANLN.TOTQTY
      ENDSCAN
    ENDIF    

    *- Total contract ( = Ordered - Cancelled )
    m.TotCntrct = m.TotCntrct + ORDLINE.TOTBOOK - lnCanceled

    *- Call off to date / selected week  ( Allocation )
    
    *C123651,1 NNA 10/27/2004 (Begin) Get the Allocated Qty from the Ordline file
    *IF !EMPTY(ORDLINE.PIKDATE)
    IF !EMPTY(ORDLINE.PIKDATE) .AND. ORDLINE.PIKDATE <= ldRpToDt
      m.INVTODATE = m.INVTODATE + ORDLINE.TOTPIK
    *C123651,1 NNA (END)
      
      *C037373,4  TMI [Start] Get lnAlocated,m.INVTODATE from style file
      *lnAlocated = lnAlocated + ORDLINE.TOTPIK
      *m.INVTODATE = m.INVTODATE + ORDLINE.TOTPIK
      *C037373,4  TMI [End  ] 
      IF BETWEEN(ORDLINE.PIKDATE,ldRpFromDt,ldRpToDt)
        m.INVWEEK = m.INVWEEK + ORDLINE.TOTPIK
      ENDIF
    ENDIF
  
    *C037373,5  TMI [Start] Calculate InvWeek from Invline directly since there may be Consolidated invoices , in this case no order# is recorded
    *-* *- Call off to date / selected week  ( Invoicing )
    *-* IF SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'INVLINE')  && key : ORDER+STR(LINENO,6)+INVOICE
    *-*   SELECT INVLINE
    *-*   SCAN REST WHILE ORDER+STR(LINENO,6)+INVOICE = ORDLINE.ORDER+STR(ORDLINE.LINENO,6) ;
    *-*        FOR INVHDR.STATUS <> 'V'
    *-*     *C037373,4  TMI [Start] Get m.INVTODATE from style file
    *-*     *m.INVTODATE = m.INVTODATE + INVLINE.TOTQTY
    *-*     *C037373,4  TMI [End  ] 
    *-*     IF BETWEEN(INVLINE.INVDATE,ldRpFromDt,ldRpToDt)
    *-*       m.INVWEEK = m.INVWEEK + INVLINE.TOTQTY
    *-*     ENDIF        
    *-*   ENDSCAN
    *-* ENDIF    
    *C037373,5  TMI [End  ] 
  ENDSCAN
  *C123651,1 NNA 10/27/2004 (Begin) Get the invoiced Qty from the Invline file
  lcOldOrdr = ORDER('INVLINE')
  SELECT INVLINE
  SET ORDER TO Invlines
  IF SEEK(STYLE.STYLE,'INVLINE')      
    SCAN REST WHILE style+invoice+STR(lineno,6) = STYLE.STYLE ;
              FOR INVLINE.SEASON = STYLE.SEASON .AND. INVHDR.STATUS <> 'V'
      IF INVLINE.INVDATE <= ldRpToDt          && From the First to [To Date]
        m.INVTODATE = m.INVTODATE + INVLINE.TOTQTY
      ENDIF
    ENDSCAN
  ENDIF
  SET ORDER TO lcOldOrdr IN INVLINE
  *C123651,1 NNA (END)


    *C123380,1 NNA 10/26/2004 (Begin) Sum TotQty that Shipped Or Received (TranCD 2 Or 3) for Current Style
    *-Shipped to date 
    IF SEEK(STYLE.STYLE,'POSLN')  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
      SELECT POSLN
      SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE ;
           FOR INLIST(TranCd,'2','3') .AND. !EMPTY(SHIPNO)
        SELECT SHPMTHDR
        IF SEEK(POSLN.SHIPNO) AND SHPMTHDR.ENTERED <= ldRpToDt
          m.ShpToDate = m.ShpToDate + POSLN.TotQty
        ENDIF
      ENDSCAN
    ENDIF   
   *C123380,1 NNA (End)

  *C037373,5  TMI [Start] Get InvWeek from InvLine file
  *- Call off to date / selected week  ( Invoicing )
  lcSvInvOrd = ORDER('INVLINE')
  SET ORDER TO INVLINES IN INVLINE
  GO TOP
  IF SEEK(STYLE.STYLE,'INVLINE')
    SELECT INVLINE
    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE ;
              FOR BETWEEN(INVLINE.INVDATE,ldRpFromDt,ldRpToDt) ;
                  .AND. INVHDR.STATUS <> 'V'
      m.INVWEEK = m.INVWEEK + INVLINE.TOTQTY
    ENDSCAN
  ENDIF
  SET ORDER TO &lcSvInvOrd IN INVLINE
  *C037373,5  TMI [End  ] 

  *C037373,4  TMI [Start] Get lnAlocated from style.totalo
  lnAlocated = STYLE.TOTALO
  *C037373,4  TMI [End  ] 

  *- Available Stock	This is the stock figure - Allocated figure	
  m.STKVAL = m.STKVAL + STYLE.TOTSTK - lnAlocated 
  
  *C037373,3  TMI [Start] fix the Wrong calculations
  *- Stock Value at CP	/ SP
  *m.STKVALCP = m.STKVALCP + m.STKVAL*STYLE.AVE_COST 
  *m.STKVALSP = m.STKVALSP + m.STKVAL*STYLE.PRICEA 
  m.STKVALCP = m.STKVALCP + (STYLE.TOTSTK - lnAlocated)*STYLE.AVE_COST 
  m.STKVALSP = m.STKVALSP + (STYLE.TOTSTK - lnAlocated)*STYLE.PRICEA 
  *C037373,3  TMI [End  ] 
  
  *C037373,4  TMI [Start] Get m.INVTODATE from style file
  *C037373,6  TMI [Start] increment invtodate for the same style color
  *m.INVTODATE = STYLE.TOTSHP + STYLE.TOTALO

  *C123651,1 NNA 10/27/2004 (Begin) Stop this Command to Get the invoiced and Allocated Qty 
  *C123651,1 NNA            from the Invline & ordline files ( in the Previous Lines)
  *m.INVTODATE = m.INVTODATE + STYLE.TOTSHP + STYLE.TOTALO
  *C123651,1 NNA (End)
  
  *C037373,6  TMI [End  ] 
  *C037373,4  TMI [End  ]   

  *- Call off Value	: Total Invoiced value  + Allocated To give a total figure  	
  m.INVVAL = m.INVTODATE * STYLE.PRICEA

  *-Actual to date / This Week	
  IF SEEK(STYLE.STYLE,'POSLN')  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
    SELECT POSLN
    SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE ;
         FOR TRANCD = '2'
    
      *C123651,1 NNA 10/27/2004 (Begin) Take [to Date] in Consider
      IF POSLN.DATE <= ldRpToDt
      *C123651,1 NNA (END)

        m.ACTTODATE = m.ACTTODATE + POSLN.TOTQTY

      *C123651,1 NNA (BEGIN)
      ENDIF
      *C123651,1 NNA (END)

      IF BETWEEN(POSLN.DATE,ldRpFromDt,ldRpToDt)
        m.ACTWEEK = m.ACTWEEK + POSLN.TOTQTY
      ENDIF
    ENDSCAN
  ENDIF   
  
  *C037373,4  TMI [Start] Get adjustments from STYINVJL file
  IF SEEK(STYLE.STYLE,'STYINVJL')
    SELECT STYINVJL
    SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = STYLE.STYLE ;
              FOR CTRTYPE = '1'

      *C123651,1 NNA 10/27/2004 (Begin) Take [to Date] in Consider
      IF STYINVJL.DTRDATE <= ldRpToDt
      *C123651,1 NNA (END)

        m.ACTTODATE = m.ACTTODATE + STYINVJL.NTOTSTK

      *C123651,1 NNA (BEGIN)
      ENDIF
      *C123651,1 NNA (END)
      
      IF BETWEEN(STYINVJL.DTRDATE,ldRpFromDt,ldRpToDt)
        m.ACTWEEK = m.ACTWEEK + STYINVJL.NTOTSTK
      ENDIF
    ENDSCAN
  ENDIF
  *C037373,4  TMI [End  ] 
  
  SELECT &lcCollect
  IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
    APPEND BLANK  
  ENDIF
  GATHER MEMVAR MEMORY
  
ENDSCAN

*-- end of lfCollect.

*:**************************************************************************
*:* Name        : lfExprt2XL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Export data to Excel file
*:***************************************************************************
FUNCTION lfExprt2XL

WAIT WINDOW NOWAIT 'Pls wait...'
*--copy file ICLWAHBD.XLS to our new file 
lcRpPath = gcWorkDir
lcRpDefPth = gcDef_Path && Save The DefPath.

*--to be sure that it is EXCEL Name
lcRpFilNam = 'exported.xls'

lcRpDefPth = ALLTRIM(lcRpDefPth)
lcSorCop = ALLTRIM(lcRpDefPth) +'ICLWAHBD.XLS'
lcTarCop = lcRpPath + lcRpFilNam

PRIVATE lcErrStr,llError
lcErrStr = ON('ERROR')
llError = .T.
ON ERROR llError = .T.
DO WHILE llError
  llError = .F.
  COPY FILE &lcSorCop TO  &lcTarCop  
  IF llError
    WAIT WINDOW 'Error while creating the Excel file, Press ESC to exit or press any other key to retry.'
    IF LASTKEY() = 27
      RETURN
    ENDIF
  ENDIF
ENDDO  
ON ERROR &lcErrStr

lnLen = LEN(lcRpFilNam)
lcRpFilNam =LEFT(lcRpFilNam,lnLen-4)

lcTempMemo = gfTempName()

SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 

lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')

lcLib=SYS(2004)+"foxtools.fll"

IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'IC\'+[ICLOW10.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF

WAIT CLEAR
*-- end of lfExprt2XL.
*:**************************************************************************
*:* Name        : ifsrvStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Set-Reset-Valid function for style
*:***************************************************************************
FUNCTION lfsrvStyle
PARAMETERS lcParam
PRIVATE lnSlct
lnSlct = SELECT()
SELECT STYLE
DO CASE
  CASE lcParam = 'S'
    SET ORDER TO CSTYLE IN STYLE
    LOCATE              
  CASE lcParam = 'R'
    SET ORDER TO STYLE IN STYLE
ENDCASE
SELECT (lnSlct)
*-- end of ifsrvStyle.

*:**************************************************************************
*:* Name        : lfSeason
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2004
*:* Purpose     : Valid function to collect seasons in one string
*:***************************************************************************
FUNCTION lfvSeason
PRIVATE lnSesPos
lnSesPos = ASCAN(laOgFxFlt,'STYLE.SEASON')
IF lnSesPos > 0
  lnSesPos = ASUBSCRIPT(laOgFxFlt , lnSesPos , 1 )
  lcRpSeason = laOgFxFlt[lnSesPos,6]
  =lfvEx2XL()
ENDIF
*-- end of lfSeason.

*:**************************************************************************
*:* Name        : lfPurcode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2004
*:* Purpose     : Valid function to collect Purchased codes in on string
*:***************************************************************************
FUNCTION lfvPurcode
PRIVATE lnPurcdPos
lnPurcdPos = ASCAN(laOgFxFlt,'STYLE.CPURCODE')
IF lnPurcdPos > 0
  lnPurcdPos = ASUBSCRIPT(laOgFxFlt , lnPurcdPos , 1 )
  lcRpPurcod = laOgFxFlt[lnPurcdPos,6]
  =lfvEx2XL()
ENDIF
*-- end of lfPurcode.

*:**************************************************************************
*:* Name        : lfvEx2XL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/11/2004
*:* Purpose     : Valid function for Export to Excel choise
*:***************************************************************************
FUNCTION lfvEx2XL
lcPrvStat = IIF(EMPTY(lcRpSeason).OR.EMPTY(lcRpPurcod),'DISABLE','ENABLE')
SHOW GET pbOGpreview &lcPrvStat

lcRunStat = IIF(EMPTY(lcRpSeason).OR.EMPTY(lcRpPurcod).OR.llRpExport,'DISABLE','ENABLE')
SHOW GET pbRun &lcRunStat
*-- end of lfvEx2XL.

*:**************************************************************************
*:* Name        : lfvDtRng
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/29/2004
*:* Purpose     : Date Range function
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvDtRng

lcTitle = 'Enter Date Range'
ldFrom = ldRpFromDt 
ldTo   = ldRpToDt

DO daterng.spx

*-- end of lfvDtRng.

*:**************************************************************************
*:* Name        : lfvpbok
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/29/2004
*:* Purpose     : Ok button of the date range screen
*:***************************************************************************
*:* Called from : daterng.spx screen
*:***************************************************************************
FUNCTION lfvpbok
ldRpFromDt = ldFrom
ldRpToDt   = ldTo   

CLEAR READ
*-- end of lfvpbok.

*:**************************************************************************
*:* Name        : lfConInv
*:* Developer   : NADER NABIL (NNA)
*:* Date        : 10/27/2004
*:* Purpose     : GET Call off to date / selected week  ( Allocation )
*:***************************************************************************
*:* Called from : .PRG
*:***************************************************************************
FUNCTION lfConInv
PRIVATE lnOldAlias , lcOldOrdr
lnOldAlias = SELECT(0)
lcOldOrdr = ORDER('INVLINE')

SELECT INVLINE
SET ORDER TO Invlines

SELECT INVLINE
IF SEEK(STYLE.STYLE,'INVLINE')      
  SCAN REST WHILE style+invoice+STR(lineno,6) = STYLE.STYLE ;
           FOR INVLINE.SEASON = STYLE.SEASON .AND. INVHDR.STATUS <> 'V'
    IF INVLINE.INVDATE <= ldRpToDt          && From the First to [To Date]
      m.INVTODATE = m.INVTODATE + INVLINE.TOTQTY
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO lcOldOrdr IN INVLINE


*-- end of lfConInv.