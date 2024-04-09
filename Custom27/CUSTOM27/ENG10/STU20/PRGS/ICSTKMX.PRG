*:****************************************************************
*: Program file  : ICSTKMX.PRG
*: Program desc. : Stock matrix enquiry screen for STU20 
*: For screen    : ICSTKMX.SPR
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Inventory Control (IC2)
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Date          : 05/11/2004
*: Tracking #    : *C122477,1
*:***************************************************************************
* Modifications
*C122477,3 TMI 06/20/2004 As per Tony, Add to each line Style/color/scale
*:***************************************************************************

*-- laData     is array to hold the values of the header file.
*-- laScrMode  is array to hold the values of screen mode.
*-- laKeyField is array to hold the information of the key fields.
DIMENSION laData[1],laScrMode[4],laKeyField[1,4]

*-- Save the value of arrays to reuse it when return from another session
STORE SPACE(0) TO laData
*C122477,4  TMI [Start] Define an array to save values collected in the temp file lcTmpFl
DIMENSION laStyle[20,30],laClr[1],laSz[8],laRGB[20,8],laTotals[24],laRGBTot[24]
STORE '  ' TO laStyle,laClr,laSz,laRGB,laTotals
lnMaxSclLn = 16                                     && Max scale lines used currently
ln4 = 0
lnColor = 1
laRGBTot = 'RGB(0,0,0,192,192,192)'
*C122477,4  TMI [End  ] 

*-- This is to prevent the global save
laDefProc[9] = .F.                 

lnBrwFldNo = 8  && No of fields to show in browse
DIMENSION laSize[lnBrwFldNo]
laSize = ''

*-- Logical Variables saved when goto another session.
*-- glFromBrow : This is a flag to control a trapping for browse screens
*-- llBrowse   : This is a flag set it when we push a Bit_Map picture, otherwise reset it.
*-- llNoShow   : This is a flag reset it to execute (lpShow) first time on screen entry .
STORE .F. TO glFromBrow,llBrowse,llNoShow

*-- Numeric Variables saved when goto another session.
*-- lnMajorLen : This is to hold the value of mask width to make the key field sizable
*-- lnMrk      : This is to hold the value of current record to move (>) shape to this record
STORE 0 TO lnMajorLen,lnMrk

STORE SPACE(0) TO lcOldValue,lcScFields

STORE '' TO lcIcStkMx0,lcIcStkMx1,lcIcStkMx2,lcStyMaj,lcMjrPct,lcDESC1,lcLastSty,;
            lcTmpFl,lcTmpPos
STORE 1 TO lnStatus,lnScaleLen,lnBrwGrp
                  
lcWinName = "Stock Matrix Enquiry."
lcBrowFlds = ""
lcBrTtl = ' '
lnOldValue = 0
llBrowse = .F.
llOpenPos = .F.   && This variable is used originally to allow to browse open PO's in the same browse
                  && of the styles , it is now has no use
lcClrSpr = ' '

DECLARE laWareHouses[1,2]
laWareHouses = ' '

STORE ' ' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
DIMEN laTot[8]
laTot = 0

lcMjrPct  = gfItemMask('PM')
lnMajorLen = LEN(lcMjrPct)
STORE 0 TO lnClrLen,lnClrPos
=lfGetClrD() 
lcItemPct = gfItemMask('PI')
lcSepart  = SUBSTR(lcItemPct,lnMajorLen+1,1)

lcDFWHSME  = gfGetMemVar('M_DFWHSME',gcAct_Comp)
lcOldWH = lcDFWHSME

*-- If you can not setup break excuation 
IF !gfSetup()
  RETURN
ENDIF

=gfOpenFile(gcDataDir+'POSLN','POSLNS')
=gfOpenFile(gcDataDir+'POSHDR','POSHDR')
SELECT POSLN
IF !'POSHDR' $ SET('RELATION')
  SET RELATION TO CSTYTYPE+PO INTO POSHDR ADDITIVE
  GO TOP
ENDIF
= gfOpenFile(gcDataDir+'Codes','','SH')
*C122477,8  TMI [Start] STYPRICE file
=gfOpenFile(gcDataDir+'STYPRICE','STYPRICE','SH')
*C122477,8  TMI [End  ] 

lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.

*-- If this is a new base window this means that it is a new session
*-- thus create new values for predefined variables, and assign new 
*-- variables unique to the screen.
IF !WEXIST(gcBaseWind)
  *-- Windows names.
  lcIcStkMx0 = gfTempName()
  lcIcStkMx1 = gfTempName()
  lcIcStkMx2 = gfTempName()
  
  STORE STRTRAN(lcMjrPct,'X',' ') TO lcStyMaj,lcLastSty
  lcDESC1   = ''

  SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
  IF EMPTY(lcDFWHSME)
    SELECT WAREHOUS
    GO TOP
    lcDFWHSME = WAREHOUS.CWARECODE 
    lcOldWH = lcDFWHSME
  ENDIF
  *C122477,5  TMI [Start] comment these lines
  *lnWareHouse = ASCAN(laWareHouses,lcDFWHSME)
  *lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
  *SHOW GET lnWareHouse ENABLE
  *C122477,5  TMI [End  ] 
  
  lcScFields = 'STYLE'

  *- if Item code structure has not been setuped return  
  IF EMPTY(lcItemPct) OR EMPTY(lcMjrPct)
    *-- Item structure not found , Can't proceed . 
    =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
    glQuitting=.T.
    RETURN
  ENDIF 
  
  SELECT SYCEXCH
  SET ORDER TO TAG CURRENCY DESC

  =lfCrtTmpFl()
  
ENDIF 						&& Ending base window not existing IF.

PUSH KEY                                       && To save the the current keys functions.
*C122477,4  TMI [Start] *No need for this line
*ON KEY LABEL ALT+B ACTIVATE WINDOW (lcWinName) && To activate the browse screen when pressing ALT+B
*C122477,4  TMI [End  ] 

*C122477,4  TMI [Start] Remove definition of the following keys
*-* ON KEY LABEL CTRL+LEFTARROW  DO lfPrNxBrow WITH -1
*-* ON KEY LABEL CTRL+RIGHTARROW DO lfPrNxBrow WITH 1 
*-* ON KEY LABEL CTRL+UPARROW    DO lfGoTop
*-* ON KEY LABEL CTRL+DNARROW    DO lfGoBtm
ON KEY LABEL CTRL+LEFTARROW  DO lfGtOthrSz WITH -1
ON KEY LABEL CTRL+RIGHTARROW DO lfGtOthrSz WITH  1
*C122477,4  TMI [End  ] 

ON KEY LABEL ALT+C           DO lfvClear
DO (gcScrDir+'ICSTKMX.SPX')
POP KEY

*-- CleanUp code
RELEASE WINDOW (lcWinName)                     && Release browse window before exit.
POP KEY                                        && Restore old keys functions.

*-- If you quit the program (i.e: close it not open another session)
IF glQuitting

  *-- Erase temp. StyDye File
  IF USED(lcTmpFl)
    USE IN (lcTmpFl)
    ERASE (gcWorkDir+lcTmpFl+".DBF")
    ERASE (gcWorkDir+lcTmpFl+".CDX")
    ERASE (gcWorkDir+lcTmpFl+".FPT")
  ENDIF
  IF USED(lcTmpPos)
    USE IN (lcTmpPos)
    ERASE (gcWorkDir+lcTmpPos+".DBF")
    ERASE (gcWorkDir+lcTmpPos+".CDX")
    ERASE (gcWorkDir+lcTmpPos+".FPT")
  ENDIF
  
  llReturn = .T.
ENDIF 				&& Ending true quiting IF.

*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/97
*! Purpose   : Handling the screen modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfWHBrow,lfwWareBrs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
PROCEDURE lpShow
PRIVATE lnFldGrpCn
lnAlias = SELECT(0)

*-- The following case is to control the screen modes.
DO CASE
  *-- SELECT MODE
  CASE laScrMode[1]
      
    lcBrowFlds = ''
    SELECT (lcTmpFl)
    ZAP
    
    DIMENSION laSize[8]
    laSize = '     '
    =lfBrowsLn()
    
    *C122477,4  TMI [Start] No need for this line
    *=lfGrpNo()
    *C122477,4  TMI [End  ] 
    
    lnStatus = 1
    STORE SPACE(LEN(lcMjrPct)) TO lcStyMaj,lcLastSty,lcDESC1

    SHOW GET lcDESC1
    SHOW GET lnStatus
        
    SHOW GET pbClear     DISABLE
    SHOW GET pbSave      DISABLE
    SHOW GET lcStyMaj    ENABLE
    SHOW GET ibStyle     ENABLE    
    *C122477,5  TMI [Start] comment this line
    *SHOW GET lnWareHouse ENABLE      
    *C122477,5  TMI [End  ] 
    SHOW GET pbPO        ENABLE
    
    _CUROBJ = OBJNUM(lcStyMaj)
  
    lcDFWHSME  = gfGetMemVar('M_DFWHSME',gcAct_Comp)
    IF EMPTY(lcDFWHSME)
      SELECT WAREHOUS
      GO TOP
      lcDFWHSME = WAREHOUS.CWARECODE 
    ENDIF
    *C122477,3  TMI [Start] Populate the new vairable lcOldWH
    lcOldWH = lcDFWHSME    
    *C122477,3  TMI [End  ] 

    *C122477,5  TMI [Start] Commnet these lines    
    *-* lnWareHouse = ASCAN(laWareHouses,lcDFWHSME)
    *-* lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
    *-* SHOW GET lnWareHouse ENABLE
    *C122477,5  TMI [End  ] 
    
    llOpenPos = .F.
    SHOW GET llOpenPos DISABLE
    SHOW GET pbPOs DISABLE
    
    =lfRefresh(lcICSTKMX0)
    *C122477,4  TMI [Start] Comment this line
    *=lfRefresh(lcICSTKMX2)
    *C122477,4  TMI [End  ] 

    *-- Redefine new data
    STORE STRTRAN(lcMjrPct,'X',' ') TO lcStyMaj,lcLastSty
    lnStatus = 1
    lcDesc1 = ''
     
    SELECT (lcTmpFl)
    ZAP
     
    _CUROBJ = OBJNUM(lcStyMaj)

  *- View Mode
  CASE laScrMode[2]
  
    SHOW GET pbPOs   ENABLE
    SHOW GET pbClear ENABLE
    SHOW GET pbQuit  ENABLE
    
    lnFctr = IIF(llOpenPos,3,1)
    lnFldGrpCn = CEILING(ALEN(laSize)/(lnBrwFldNo*lnFctr))

    *C122477,5  TMI [Start] We have the buttons pbPr,pbNx
    *-* lcPrevStat = IIF(lnBrwGrp = 1 , 'DISABLE' , 'ENABLE')
    *-* lcNextStat = IIF(lnBrwGrp = lnFldGrpCn , 'DISABLE' , 'ENABLE' )
    *-* SHOW GET pbPrev &lcPrevStat
    *-* SHOW GET pbNext &lcNextStat
    *C122477,5  TMI [End  ] 
    
    *C122477,5  TMI [Start] commnet this line
    *-* SHOW GET lnWareHouse ENABLE
    *C122477,5  TMI [End  ] 
    
    SHOW GET lnStatus DISABLE
    
    *C122477,4  TMI [Start] *No need for this line
    *=lfwBrowUp()
    *C122477,4  TMI [End  ] 
    
    =lfRefresh(lcICSTKMX0)
    
ENDCASE 			&& Ending the CASE that control screen modes  

*- Disabel control panel buttons
SHOW GET pbTop      DISABLE
SHOW GET pbPrvs     DISABLE
SHOW GET pbNxt      DISABLE
SHOW GET pbBtm      DISABLE
SHOW GET pbBrws     DISABLE
SHOW GET pbSlct     DISABLE
SHOW GET pbEdt      DISABLE
SHOW GET pbDlt      DISABLE
SHOW GET PBCPTASK   DISABLE
SHOW GET PBCPCALNDR DISABLE
SHOW GET PBCPCALC   DISABLE
SHOW GET PBNOTEPAD  DISABLE
SHOW GET PBAUDTRAIL DISABLE

SHOW GET lcDFWHSME ENABLE
*C122477,3  TMI [Start] Enable browse button for WH
SHOW GET ibWH ENABLE
*C122477,3  TMI [End  ] 

*-- Refreshing data in both [screen fields(SAYs and GETs)] and [browse window] 
=lfBrowsLn()

*-- Restore the ALIAS we enter the function with it.
SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfvStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Valid function for style,collects all POs for the selected style
*:***************************************************************************
FUNCTION lfvStyle
PARAMETERS llChWH
PRIVATE lcI 
IF !llBrowse .AND. EMPTY(lcStyMaj)
  RETURN
ENDIF  
IF llBrowse .OR. ;
  ( lcLastSty <> lcStyMaj .AND. !SEEK(lcStyMaj,'Style') )
  SELECT STYLE
  LOCATE
  lcStyMaj = gfStyBrw('M',"","",.F.)
  llBrowse = .F.
  IF EMPTY(lcStyMaj)
    STORE SPACE(lnMajorLen) TO lcLastSty,lcStyMaj
    _CUROBJ = OBJNUM(lcStyMaj)
  ENDIF

ENDIF

IF (!EMPTY(lcStyMaj) .AND. lcLastSty <> lcStyMaj) .OR. llChWH
  lcLastSty = lcStyMaj
  SHOW GET Dumy ENABLE
  SHOW GET lcStyMaj DISABLE
  SHOW GET ibStyle DISABLE
  
  lnStatus = AT(STYLE.STATUS,'.AHX')
  SHOW GET lnStatus
  
  =SEEK('S'+STYLE.SCALE,'SCALE')
  
  lcDesc1 = STYLE.DESC1
  SHOW GET lcDesc1
  
  SELECT &lcTmpPos
  ZAP

  SELECT &lcTmpFl
  ZAP
  
  *:***************************************************************************1
  
  lnBrwGrp = 1
 
  *-Get colors
  DIMENSION laClr[1]
  laClr = ''
  SELECT DISTINCT SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) ;
     FROM STYLE ;
     WHERE STYLE = PADR(lcStyMaj,lnMajorLen) ;
     INTO ARRAY laClr  
  
  =SEEK(gcBaseCurr+'EUR','SYCEXCH')
  
  *-- loop all color for the current style.
  FOR lnClrs = 1 TO ALEN(laClr,1)
  
    *WAIT WINDOW NOWAIT 'Please Wait ... Collecting data for Color# : ' + laClr[lnClrs]
    STORE 1 TO lnSequNo , lnSizeNo
    *C122477,3  TMI [Start] Initialize lnSequNo with 0
    STORE 0 TO lnSequNo
    *C122477,3  TMI [End  ] 
  
    *C122477,3  TMI [Start] comment these lines
    *-* SELECT (lcTmpFl)
    *-* APPEND BLANK
    *-* REPLACE COLOR  WITH laClr[lnClrs] ;
    *-*         BrwGrp WITH lnSequNo
    *C122477,3  TMI [End  ] 

    =SEEK(lcStyMaj,'STYLE')
    lcSCALE = LEFT(Style.Scale,lnScaleLen)
    
    *-- Get all sizes for current style.
    SELECT SCALE
    GO TOP
    =SEEK('S'+LEFT(Style.Scale,lnScaleLen))
    SCAN REST WHILE type+scale+prepak = 'S' + lcSCALE
      *-- get the sales order once for the current scale.
      lcSeekSty = PADR( lcStyMaj+lcSepart+PADR(laClr[lnClrs],lnClrLen)+lcClrSpr+Scale.Scale , 19 )
      
      =SEEK(lcSeekSty,'STYLE')
      =SEEK(lcSeekSty+lcDFWHSME,'STYDYE')
      =SEEK(lcSeekSty,'POSLN')
      *C122477,8  TMI [Start] locate the style in STYPRICE file
      =SEEK(lcSeekSty+'EUR','STYPRICE')
      *C122477,8  TMI [End  ] 
      
      */factor number
      lnFctr = IIF(llOpenPos,3,1)
      
      SELECT (lcTmpFl)
      *C122477,3  TMI [Start] Add a line for each scale line
      lnSequNo = lnSequNo + 1           
      APPEND BLANK
      REPLACE COLOR  WITH laClr[lnClrs] ;
              BrwGrp WITH lnSequNo ;
              SCALE  WITH SCALE.SCALE
      *C122477,3  TMI [End  ] 
      FOR lnI = 1 To 8
        lcI = STR(lnI,1)
        *-- get every size at the scale, if empty get out from the for loop.
        IF Empty(SCALE.SZ&lcI)
          EXIT
        ELSE
          *C122477,3  TMI [Start] let lcSizeno be same as lcI variable
          lcSizeNo = ALLTRIM(STR(lnI))
          *C122477,3  TMI [End  ] 
          SELECT (lcTmpFl)
          IF NOT llOpenPos
            
            *C122477,8  TMI [Start] get EUR price from STYPRICE file
            * REPLACE PRICEA        WITH STYLE.PRICEA ;
            *         EUR           WITH STYLE.PRICEA*SYCEXCH.NEXRATE ;
            *         DESC          WITH STYLE.DESC ;
            *         Stk&lcSizeNo  WITH STYDYE.Stk&lcI ; 
            *         Ord&lcSizeNo  WITH STYDYE.Ord&lcI ;
            *         Fre&lcSizeNo  WITH STYDYE.Stk&lcI - STYDYE.Ord&lcI 
            REPLACE PRICEA        WITH STYLE.PRICEA ;
                    EUR           WITH IIF(STYPRICE.PRICEA>0,STYPRICE.PRICEA,STYLE.PRICEA*SYCEXCH.NEXRATE) ;
                    DESC          WITH STYLE.DESC ;
                    Stk&lcSizeNo  WITH STYDYE.Stk&lcI ; 
                    Ord&lcSizeNo  WITH STYDYE.Ord&lcI ;
                    Fre&lcSizeNo  WITH STYDYE.Stk&lcI - STYDYE.Ord&lcI 
            *C101945,1 TMI [End  ] 

          ELSE
            
            SELECT POSLN
            lcPO = POSLN.CSTYTYPE+POSLN.PO
            DO WHILE STYLE = lcSeekSty
              IF lcPO <> POSLN.CSTYTYPE+POSLN.PO
                INSERT INTO &lcTmpFl (COLOR,BRWGRP) VALUES (laClr[lnClrs] , lnSequNo)
                lcPO = POSLN.CSTYTYPE+POSLN.PO
              ENDIF
              REPLACE &lcTmpFl..PO WITH POSLN.PO              
              SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = lcSeekSty+lcPO ;
                        FOR POSHDR.STATUS = 'O'              
                DO CASE
                CASE POSLN.TRANCD = '1'
                  &lcTmpFl..OPN&lcSizeNo WITH &lcTmpFl..OPN&lcSizeNo + POSLN.QTY&lcI 
                ENDCASE
              ENDSCAN
            ENDDO
            
          ENDIF        
          lnSizeNo = lnSizeNo + 1
        ENDIF
        *C122477,3  TMI [Start] Increment sequence # variable 
        lnSequNo = lnSequNo + 1
        *C122477,3  TMI [End  ] 
      ENDFOR  && 8 Scale 
    ENDSCAN  && Scale file
  ENDFOR  && of Colors array laClr
  WAIT CLEAR
  
  SHOW GET llOpenPos ENABLE
  SHOW GET pbPOs ENABLE
  
  *C122477,5  TMI [Start] no need to this funciton , the browse is removed
  *=lfFilter()  
  *C122477,5  TMI [End  ] 
  
  *:***************************************************************************1
  *- Get sizes array
  DIMENSION laExtSz[1],laSize[1]
  STORE ' ' TO laExtSz[1],laSize[1]
  SELECT Scale,CNT ;
    FROM SCALE ;
    WHERE Type+Scale='S'+SUBSTR(lcSCALE,1,lnScaleLen) ;
    ORDER BY 1 ;
    INTO ARRAY laExtSz
  SELECT SCALE
  =SEEK('S'+laExtSz[1],'SCALE')

  lnIncrmnt = 0
  FOR lnCount = 1 TO ALEN(laExtSz,1)
    =SEEK('S'+laExtSz[lnCount,1],'SCALE')
    FOR lnJ = 1 TO laExtSz[lnCount,2]
      lcZ = STR(lnJ,1)
      DIMENSION laSize[lnIncrmnt+lnJ]
      laSize[lnIncrmnt+lnJ] = ALLTRIM(SCALE.SZ&lcZ)
    ENDFOR
    lnIncrmnt = lnIncrmnt + laExtSz[lnCount,2]
  ENDFOR
  
  SHOW GET pbPrev DISABLE
  lnFctr = IIF(llOpenPos,3,1)
  lcNxStat = IIF( CEILING(ALEN(laSize)/(lnBrwFldNo*lnFctr)) > 1 , 'ENABLE' , 'DISABLE' )
  SHOW GET pbNext &lcNxStat

  IF RECCOUNT(lcTmpFl) > 0
    SHOW GET pbClear ENABLE
    SHOW GET pbSave  ENABLE

    SELECT (lcTmpFl)
    GO TOP
  ENDIF
  
  laScrMode = .F.
  laScrMode[2] = .T.
  
  SHOW GET pbBrws DISABLE
  
  =lfBrowsLn()

  *C122477,4  TMI [Start] Refill laStyle array with colled data
  lnColor = 1
  =lfUpdDat()
  *C122477,4  TMI [End  ] 
  
  =lfRefresh(lcICSTKMX0)

  *C122477,4  TMI [Start]  Comment this line
  *=lfRefresh(lcIcStkMx2)  
  *C122477,4  TMI [End  ] 
ENDIF
*-- end of lfvStyle.


*:**************************************************************************
*:* Name        : lfFilter
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/17/2004
*:* Purpose     : Filter browse on group and get totals
*:***************************************************************************
*:* Called from : lfvStyle
*:***************************************************************************
FUNCTION lfFilter
  SELECT (lcTmpFl)
  *C122477,3  TMI [Start] do not filter on groups, show all lines 
  *SET FILTER TO INLIST(BrwGrp , 99 , lnBrwGrp )
  *C122477,3  TMI [End  ] 
  GO TOP
  SUM STK1,ORD1,FRE1,;
      STK2,ORD2,FRE2,;
      STK3,ORD3,FRE3,;
      STK4,ORD4,FRE4,;
      STK5,ORD5,FRE5,;
      STK6,ORD6,FRE6,;
      STK7,ORD7,FRE7,;
      STK8,ORD8,FRE8 ;
      FOR BRWGRP <> 99 ;
      TO ARRAY laTotal 
  LOCATE FOR BRWGRP = 99
  IF !FOUND()    
    APPEND BLANK
    REPLACE DESC   WITH '' ;
            BRWGRP WITH 99 ;
            COLOR  WITH CHR(255)
    APPEND BLANK
    REPLACE DESC   WITH 'Totals:' ;
            BRWGRP WITH 99 ;
            COLOR  WITH CHR(255)
  ELSE
    SKIP
  ENDIF
  GATHER FIELDS ;
      STK1,ORD1,FRE1,;
      STK2,ORD2,FRE2,;
      STK3,ORD3,FRE3,;
      STK4,ORD4,FRE4,;
      STK5,ORD5,FRE5,;
      STK6,ORD6,FRE6,;
      STK7,ORD7,FRE7,;
      STK8,ORD8,FRE8 FROM laTotal
  
GO TOP
*-- end of lfFilter.
*:**************************************************************************
*:* Name        : lfvClear
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Clear recieved qtys for the selected style
*:***************************************************************************
FUNCTION lfvClear
laScrMode = .F.
laScrMode[1] = .T.
*C122477,4  TMI [Start] Empty laStyle array
STORE '  ' TO laStyle ,laClr,laSz,laRGB,laTotals
laRGB = 'RGB(0,0,0,192,192,192)'
laRGBTot = 'RGB(0,0,0,192,192,192)'
ln4 = 0
lnColor = 1
SHOW GET pbPr DISABLE
SHOW GET pbNx DISABLE
*C122477,4  TMI [End  ] 
SHOW GETS
*-- end of lfvClear.

*:**************************************************************************
*:* Name        : lfvQuit
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Close the screen
*:***************************************************************************
FUNCTION lfvQuit
=gfCPClose()
CLEAR READ

*-- end of lfvQuit.

*:**************************************************************************
*:* Name        : lfClose
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 24/02/2005
*:* Purpose     : Close a file
*:***************************************************************************
FUNCTION lfClose
PARAMETERS lcFile
IF USED(lcFile)
  USE IN &lcFile
ENDIF

*-- end of lfClose.
*:**************************************************************************
*:* Name        : lfBrowsLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/17/2004
*:* Purpose     : Browse details
*:***************************************************************************
*:* Called from : postrcv.spx
*:***************************************************************************
FUNCTION lfBrowsLn
*-* since the browse is removed this function now is used only to update the laSz array and lcSz variables

SELECT (lcTmpFl)
lnMrk  = RECNO(lcTmpFl)

*lcBrTtl = 'Prices'+SPACE(20)
STORE ' ' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
IF NOT llOpenPos

  *C122477,4  TMI [Start] no need for this variable
  *-* lcBrowFlds = "lcMarker=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
  *-*              "PRICEA :R :H=CHR(163) :5 ,"+;
  *-*              "EUR    :R :H='E':5,"+;
  *-*              "DESC   :R :H='':8"
  *-* *C122477,3  TMI [Start] Locate the correct scale in scale file
  *C122477,4  TMI [End  ] 
  
  =SEEK('S'+&lcTmpFl..SCALE,'SCALE')  
  *C122477,3  TMI [End  ] 
  
  FOR lnFld = 1 TO lnBrwFldNo  
    lcF = LTRIM(STR(lnFld))
    lnK = lnBrwFldNo*(lnBrwGrp-1)+lnFld    
    *C122477,3  TMI [Start] ignore useing the array laSize
    *-* IF lnK <= ALEN(laSize,1)
      *-* lcBrowFlds = lcBrowFlds + IIF(EMPTY(laSize[lnK]) , "" ,;
      *-*               ",STK&lcF:H='Stk':3:R"+;   
      *-*               ",ORD&lcF:H='Ord':4:R"+;
      *-*               ",FRE&lcF:H='Free':4:R")
      *-* lcSz&lcF = PADR(laSize[lnK],5)
    *-* ENDIF                 
    *C122477,4  TMI [Start] *No need for this line
    *-* lcBrowFlds = lcBrowFlds + IIF(EMPTY(SCALE.SZ&lcF) , "" ,;
    *-*               ",STK&lcF:H='Stk':3:R"+;   
    *-*               ",ORD&lcF:H='Ord':4:R"+;
    *-*               ",FRE&lcF:H='Free':4:R")
    *C122477,4  TMI [End  ] 
    
    lcSz&lcF = SCALE.SZ&lcF
  
    *C122477,3  TMI [End  ]
    *C122477,4  TMI [Start] Update laSz array with scales 
    laSz[lnFld] = SCALE.SZ&lcF
    *C122477,4  TMI [End  ]  
  ENDFOR  
  
ELSE
  
  lcBrowFlds = "lcMarker=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
               "PO    :R :H='PO':5"
  FOR lnFld = 1 TO lnBrwFldNo*3
    lcF = LTRIM(STR(lnFld))
    lnK = lnBrwFldNo * 3 * (lnBrwGrp-1) + lnFld
    IF lnK <= ALEN(laSize,1)
      lcBrowFlds = lcBrowFlds + IIF(EMPTY(laSize[lnK]) , "" ,;
                    ",OPN&lcF:H='"+laSize[lnK]+"':3:R")
    ENDIF                 
  ENDFOR  

ENDIF  

*C122477,4  TMI [Start] Ignote rest of this funciton 
RETURN
*C122477,4  TMI [End  ] 

lcWinName = ALLTRIM(lcWinName)
BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       WHEN lfwBrowUp();
       TITLE lcWinName ;
       WINDOW (lcICSTKMX1) IN WINDOW (gcBaseWind)


ACTIVATE WINDOW (lcWinName)
SHOW WINDOW (lcWinName) REFRESH
=lfRefresh(lcWinName)

*-- end of lfBrowsLn.

*:**************************************************************************
*:* Name        : lfwBrowUp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : When browse funtion
*:***************************************************************************
FUNCTION lfwBrowUp
lnMrk = RECNO(lcTmpFl)
SHOW WINDOW (lcWinName) REFRESH

*-- end of lfwBrowUp.

*:**************************************************************************
*:* Name        : lfGetClrD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : Get color information
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetClrD()
*:***************************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount &&Tmi 07/15/2002
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
    EXIT
  ENDIF
ENDFOR

*:**************************************************************************
*:* Name        : lfCrtTmpFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : Create temp file to collect data.
*:***************************************************************************
FUNCTION lfCrtTmpFl

*Create temp file that shows stock/open/free qtys
lcTmpFl  = gfTempName()

*- Build Structure array 
lnFileStru = 0

lnFileStru = lnFileStru + 1 
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'BrwGrp'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 2
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'COLOR'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = lnClrLen
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1 
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'PRICEA'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 12
laFileStru[lnFileStru,4] = 2

lnFileStru = lnFileStru + 1 
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'EUR'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 12
laFileStru[lnFileStru,4] = 2

lnFileStru = lnFileStru + 1 
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'DESC'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 20
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1 
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'PO'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

*C122477,3  TMI [Start] Show each scale in one line in the browse
lnFileStru = lnFileStru + 1 
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'SCALE'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 3
laFileStru[lnFileStru,4] = 0
*C122477,3  TMI [End  ] 

FOR lnFld = 1 TO lnBrwFldNo
  lcFld = LTRIM(STR(lnFld))
  
  lnFileStru = lnFileStru + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'STK'+lcFld
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 7
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'ORD'+lcFld
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 7
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru + 1
  DIMENSION laFileStru[lnFileStru,4]
  laFileStru[lnFileStru,1] = 'FRE'+lcFld
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 7
  laFileStru[lnFileStru,4] = 0

ENDFOR


*C122477,4  TMI [Start] Do not add these fields , another temp file is created for open PO's
*-* *- Add 24 fields to hold the open po's
*-* FOR lnFld = 1 TO lnBrwFldNo*3
*-*   lcFld = LTRIM(STR(lnFld))  
*-*   lnFileStru = lnFileStru + 1
*-*   DIMENSION laFileStru[lnFileStru,4]
*-*   laFileStru[lnFileStru,1] = 'OPN'+lcFld
*-*   laFileStru[lnFileStru,2] = 'N'
*-*   laFileStru[lnFileStru,3] = 6
*-*   laFileStru[lnFileStru,4] = 0
*-* ENDFOR
*C122477,4  TMI [End  ] 

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'TOTSTK'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'TOTORD'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru + 1
DIMENSION laFileStru[lnFileStru,4]
laFileStru[lnFileStru,1] = 'TOTFRE'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,4] = 0

CREATE TABLE (gcWorkDir+lcTmpFl) FROM ARRAY laFileStru
INDEX ON COLOR TAG &lcTmpFl

*- Create temp file that shows open Po's
lcTmpPos = gfTempName()
SELECT POSLN
=AFIELDS(laFileStru)

CREATE TABLE (gcWorkDir+lcTmpPos) FROM ARRAY laFileStru
INDEX ON CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD TAG &lcTmpPos

*-- end of lfCrtTmpFl.

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2004
*:* Purpose     : *-Open needed files and set relations
*:***************************************************************************
FUNCTION lfOpenFls

*-Open needed files 
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
=gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
=gfOpenFile(gcDataDir+'CODES','CODES','SH')

*-Locate scale to eof , to show no scale in the first browse
=SEEK(CHR(255),'SCALE')

*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lpTab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/25/2004
*:* Purpose     : Tab function key
*:***************************************************************************
PROCEDURE lpTab
*C122477,4  TMI [Start] 
ACTIVATE WINDOW (lcIcStkMx0)
_CUROBJ=OBJNUM(lcStyMaj)  
RETURN
*C122477,4  TMI [End  ] 

*C122477,3  TMI [Start] Write lpTab useing Case statment
IF WONTOP() = lcWinName
  IF laScrMode[1]
    ACTIVATE WINDOW (lcIcStkMx0)
    _CUROBJ=OBJNUM(lcStyMaj)  
  ELSE
    *C122477,3  TMI [END  ] 
    ACTIVATE WINDOW (lcIcStkMx2)
    _CUROBJ=OBJNUM(pbPOs)  
    *C122477,3  TMI [Start] Close the if Statement
  ENDIF
  *C122477,3  TMI [End  ] 
ELSE
  IF WONTOP() = (lcIcStkMx2)
    ACTIVATE WINDOW (lcIcStkMx0)
    _CUROBJ=OBJNUM(lcStyMaj)  
  ELSE      
    _CUROBJ=_CUROBJ+1
  ENDIF
ENDIF
*:**************************************************************************
*:* Name        : lpBackTab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/25/2004
*:* Purpose     : lpBackTab function key
*:***************************************************************************
PROCEDURE lpBackTab

IF WONTOP() = lcWinName
  ACTIVATE WINDOW (lcIcStkMx0)
  _CUROBJ=OBJNUM(lcStyMaj)  
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*:**************************************************************************
*:* Name        : lfGrpNo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/05/2003
*:* Purpose     : GROUP size No/Total No of Size group , The return value is 
*:*               displayed in the screen
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGrpNo()
*:***************************************************************************
*C#200431,1
FUNCTION lfGrpNo
PRIVATE lcRet,lnFctr
lnFctr = IIF(llOpenPos,3,1)
lcRet = PADL(lnBrwGrp,2)+'/'+PADR(CEILING(ALEN(laSize)/(lnBrwFldNo*lnFctr)),2)
RETURN lcRet
*-- end of lfGrpNo.

*:**************************************************************************
*:* Name        : lfvWareCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/18/2004
*:* Purpose     : Recollect data when warhouse changed
*:***************************************************************************
FUNCTION lfvWareCode
*C122477,3  TMI [Start] Call browse for WareHous
IF llBrowse 
  llBrowse = .F.
  lcDFWHSME = gfBrowWare(.F.)
ELSE  
  *C122477,3  TMI [End  ] 
  IF lcDFWHSME <> lcOldValue
    IF !SEEK(lcDFWHSME,'WAREHOUS')
      lcDFWHSME = gfBrowWare(.F.)
    ENDIF
    IF EMPTY(lcDFWHSME)
      lcDFWHSME = lcOldValue
    ENDIF
  ENDIF
  *C122477,3  TMI [Start] Close the If statement
ENDIF  
*C122477,3  TMI [End  ]   

*C122477,3  TMI [Start] 
*IF !EMPTY(lcStyMaj) .AND. lcDFWHSME <> lcOldValue
IF !EMPTY(lcStyMaj) .AND. lcOldWH <> lcDFWHSME    
  lcOldWH = lcDFWHSME
  *C122477,3  TMI [End  ] 
  =lfvStyle(.T.)
ENDIF
*C122477,3  TMI [Start] Refresh lcDFWHSME variable
SHOW GET lcDFWHSME
*C122477,4  TMI [Start] 
IF LASTKEY() <> 15 && it is not a Backtab
  *C122477,4  TMI [End  ] 
  _CUROBJ = OBJNUM(pbClear)
  *C122477,4  TMI [Start]   
ELSE
  IF EMPTY(lcStyMaj)
    _CUROBJ = OBJNUM(lcStyMaj)
  ELSE
    _CUROBJ = OBJNUM(pbPOs)
  ENDIF
ENDIF  
*C122477,4  TMI [End  ] 
*C122477,4  TMI [Start] Comment this line
*=lfRefresh(lcICSTKMX2)
*C122477,4  TMI [End  ] 

*C122477,3  TMI [End  ] 
*-- end of lfvWareCode.

*:**************************************************************************
*:* Name        : lfvPO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/18/2004
*:* Purpose     : Browse open Po's for the selected style
*:***************************************************************************
*:* Called from : icstkmx.scx Screen
*:***************************************************************************
FUNCTION lfvPO
=lfvStyle(.T.)
*-- end of lfvPO.

*:**************************************************************************
*:* Name        : lfvOpenPos
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/31/2004
*:* Purpose     : Browse Open POs
*:***************************************************************************
FUNCTION lfvOpenPos
PRIVATE lnSlct,lcSvOrd,lcPo,lcSty,lnFctr,lcPoBrTtl,lnRcno
PRIVATE lnOrd,lnCnt
lnSlct = SELECT()
lnRcno = RECNO()

GO TOP IN STYLE  && This line is to prevent an error occured in gpCtrShow global procedure

GO TOP IN &lcTmpPos
*IF EOF(lcTmpPos) .OR. &lcTmpPos..CWARECODE <> lcDFWHSME
  SELECT &lcTmpPos
  ZAP
  
  SELECT POSLN  
  lcSvOrd = ORDER('POSLN')
  SET ORDER TO POSLNS IN POSLN
  
  DIMENSION laOrder[1]
  laOrder[1] = ''
  lnOrd = 0
  *- First collect Open POs that include this style into the array laOrder
  IF SEEK(lcStyMaj,'POSLN')
    SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = lcStyMaj
      *C122477,7  TMI [Start] include orders of status H,O and A
      *IF POSHDR.STATUS = 'O' .AND. POSLN.CSTYTYPE $ 'PN' .AND. POSLN.CWARECODE = lcDFWHSME
      IF POSHDR.STATUS  $ 'HAO' .AND. POSLN.CSTYTYPE $ 'PN' .AND. POSLN.CWARECODE = lcDFWHSME
        *C122477,7  TMI [End  ] 
        IF ASCAN(laOrder,POSLN.CSTYTYPE+POSLN.PO)=0
          lnOrd = lnOrd + 1
          DIMENSION laOrder[lnOrd]
          laOrder[lnOrd] = POSLN.CSTYTYPE+POSLN.PO
        ENDIF
      ENDIF
    ENDSCAN              
  ENDIF
  
  *- Now loop on the orders that contains this style
  SELECT POSLN
  SET ORDER TO POSLN
  GO TOP
  FOR lnCnt = 1 TO ALEN(laOrder)
    SCATTER MEMVAR BLANK
    *C122477,3  TMI [Start] Locate the correct style in POSLN file
    *=SEEK(laOrder[lnCnt],'POSLN')
    =SEEK(laOrder[lnCnt]+lcStyMaj,'POSLN')
    *C122477,3  TMI [End  ] 
    M.CSTYTYPE  = POSLN.CSTYTYPE
    M.PO        = POSLN.PO
    M.STYLE     = POSLN.STYLE
    *C122477,3  TMI [Start] Ask for trancd = '4'
    *-* SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = laOrder[lnCnt] + lcStyMaj ;
    *-*             FOR TRANCD $ '125'
    SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = laOrder[lnCnt] + lcStyMaj ;
                FOR TRANCD $ '1245'
      *C122477,3  TMI [End  ] 
      lnLnFound = SEEK(laOrder[lnCnt]+POSLN.STYLE+STR(POSLN.LINENO,6)+'1',lcTmpPos)
      IF IIF(TRANCD $ '1' , CWARECODE = lcDFWHSME , lnLnFound )
        *C122477,3  TMI [Start] take in considration trancd = '4' case
        *lnFctr = IIF(TRANCD = '1',1,IIF(TRANCD $ '25', -1 , 0))
        lnFctr = IIF(TRANCD = '1',1,IIF(TRANCD $ '245', -1 , 0))
        *C122477,3  TMI [End  ] 
        IF !lnLnFound
          INSERT INTO &lcTmpPOs (CSTYTYPE,PO,STYLE,LINENO,TRANCD) VALUES (POSLN.CSTYTYPE,POSLN.PO,POSLN.STYLE,POSLN.LINENO,'1')
        ENDIF
        SELECT &lcTmpPOs
        REPLACE QTY1 WITH QTY1 + lnFctr*POSLN.QTY1 ;
                QTY2 WITH QTY2 + lnFctr*POSLN.QTY2 ;
                QTY3 WITH QTY3 + lnFctr*POSLN.QTY3 ;
                QTY4 WITH QTY4 + lnFctr*POSLN.QTY4 ;
                QTY5 WITH QTY5 + lnFctr*POSLN.QTY5 ;
                QTY6 WITH QTY6 + lnFctr*POSLN.QTY6 ;
                QTY7 WITH QTY7 + lnFctr*POSLN.QTY7 ;
                QTY8 WITH QTY8 + lnFctr*POSLN.QTY8
      ENDIF
    ENDSCAN
  ENDFOR

*ENDIF

lcPoBrTtl = 'Style Open POs qtys'
*C122477,4  TMI [Start] Show style desc and scale desc
*-* lcBrFields = "Po     :H='Po#'   ,"+;
*-*         	 "Style  :H='Style ',"+;
*-*         	 "QTY1   :H='Qty1'  ,"+;
*-*         	 "QTY2   :H='Qty2'  ,"+;
*-*         	 "QTY3   :H='Qty3'  ,"+;
*-*         	 "QTY4   :H='Qty4'  ,"+;
*-*         	 "QTY5   :H='Qty5'  ,"+;
*-*         	 "QTY6   :H='Qty6'  ,"+;
*-*         	 "QTY7   :H='Qty7'  ,"+;
*-*         	 "QTY8   :H='Qty8'  ,"+;
*-*         	 "POSHDR.AVAILABLE :H='Available Date' "
SELECT SCALE
SCATTER FIELDS SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8 TO laSzDsc
*C122477,7  TMI [Start] Add a new field to show poshdr.status field
*-* lcBrFields = "Po     :H='Po#'   ,"+;
*-*         	 "Style  :H='Style ',"+;
*-*         	 "Desc=Style.desc1 :H='Desc.' :35 ,"+;
*-*         	 "QTY1   :H='"+laSzDsc[1]+"'  ,"+;
*-*         	 "QTY2   :H='"+laSzDsc[2]+"'  ,"+;
*-*         	 "QTY3   :H='"+laSzDsc[3]+"'  ,"+;
*-*         	 "QTY4   :H='"+laSzDsc[4]+"'  ,"+;
*-*         	 "QTY5   :H='"+laSzDsc[5]+"'  ,"+;
*-*         	 "QTY6   :H='"+laSzDsc[6]+"'  ,"+;
*-*         	 "QTY7   :H='"+laSzDsc[7]+"'  ,"+;
*-*         	 "QTY8   :H='"+laSzDsc[8]+"'  ,"+;
*-*         	 "POSHDR.AVAILABLE :H='Available Date' "
lcBrFields = "Po     :H='Po#'   ,"+;
        	 "Style  :H='Style ',"+;
        	 "Desc=Style.desc1 :H='Desc.' :30 ,"+;
        	 "QTY1   :H='"+laSzDsc[1]+"'  ,"+;
        	 "QTY2   :H='"+laSzDsc[2]+"'  ,"+;
        	 "QTY3   :H='"+laSzDsc[3]+"'  ,"+;
        	 "QTY4   :H='"+laSzDsc[4]+"'  ,"+;
        	 "QTY5   :H='"+laSzDsc[5]+"'  ,"+;
        	 "QTY6   :H='"+laSzDsc[6]+"'  ,"+;
        	 "QTY7   :H='"+laSzDsc[7]+"'  ,"+;
        	 "QTY8   :H='"+laSzDsc[8]+"'  ,"+;
        	 "POSHDR.AVAILABLE :13 :H='Avail. Date',"+;
        	 "POSHDR.STATUS :H='PO Status' "
*C122477,7  TMI [End  ] 
*C122477,4  TMI [End  ] 
WAIT CLEAR

STORE ' ' TO lcFieldsNam, lcArrName
SELECT &lcTmpPos
SET RELATION TO CSTYTYPE+PO INTO POSHDR
*C122477,6  TMI [Start] add a relation to style file
PRIVATE lnSvStyRec
lnSvStyRec = RECNO('STYLE')
SET RELATION TO STYLE INTO STYLE ADDITIVE
*C122477,6  TMI [End  ] 

GO TOP
DELETE FOR QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 = 0
GO TOP
=ARIABROW('',lcPoBrTtl,gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2,'',;
         	'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')

SELECT &lcTmpPos
SET RELATION OFF INTO POSHDR
*C122477,6  TMI [Start] remove relation to style file
SET RELATION OFF INTO STYLE
IF BETWEEN(lnSvStyRec,1,RECCOUNT('STYLE'))
  GOTO (lnSvStyRec) IN STYLE
ENDIF
*C122477,6  TMI [End  ] 

SELECT (lnSlct)
IF BETWEEN(lnRcno,1,RECCOUNT(lnSlct))
  GOTO (lnRcno)
ENDIF  
*-- end of lfvOpenPos.

*:**************************************************************************
*:* Name        : lfGtOthrSz
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/28/2004
*:* Purpose     : Get other four sizes
*:***************************************************************************
FUNCTION lfGtOthrSz
PARAMETERS lnSgn
*C122477,4  TMI [Start] go out if sizes less than 5 
IF EMPTY(lcSz5)
  RETURN
ENDIF
*C122477,4  TMI [End  ] 

IF lnSgn = 1
  ln4 = 12
  SHOW GET pbPr ENABLE
  SHOW GET pbNx DISABLE
ELSE
  ln4 = 0
  SHOW GET pbPr DISABLE
  SHOW GET pbNx ENABLE
ENDIF
=lfRefresh(lcICSTKMX0)
*-- end of lfGtOthrSz.

*:**************************************************************************
*:* Name        : lfClrChng
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/28/2004
*:* Purpose     : Change values in the display when color is changed
*:***************************************************************************
FUNCTION lfClrChng
=lfUpdDat()
=lfRefresh(lcICSTKMX0)
*-- end of lfClrChng.

*:**************************************************************************
*:* Name        : lfUpdDat
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/29/2004
*:* Purpose     : Update data
*:***************************************************************************
FUNCTION lfUpdDat
  PRIVATE lnJ
  laStyle = ' '
  SELECT * FROM &lcTmpFl WHERE COLOR = laClr[lnColor] INTO ARRAY laStyle
  *- If laStyle has less than 16 lines add them
  lnLnCnt = ALEN(laStyle,1)
  DIMENSION laStyle[16,ALEN(laStyle,2)]

  FOR lnRow = 1 TO lnMaxSclLn
    FOR lnCol = IIF(lnRow <= lnLnCnt , SCALE.CNT*3+7,0)+1 TO ALEN(laStyle,2)        
      laStyle[lnRow,lnCol] = '  '
    ENDFOR
  ENDFOR
  
  lcNxStat = IIF(!EMPTY(lcSz5),'ENABLE','DISABLE')
  SHOW GET pbNx &lcNxStat
  lcClrStat = IIF(ALEN(laClr)>1,'ENABLE','DISABLE')
  SHOW GET lnColor &lcClrStat
  *- Update the laRGB array
  DIMENSION laRGB[ALEN(laStyle,1),ALEN(laStyle,2)]
  =ACOPY(laStyle,laRGB)
  laRGB = 'RGB(0,0,0,192,192,192)'
  FOR lnX = 1 TO lnLnCnt
    FOR lnY = 0 TO 7
      IF !EMPTY(laSz[lnY+1])
        laRGB[lnX,lnY*3+8 ] = 'RGB(0,0,0,192,192,192)'
        laRGB[lnX,lnY*3+9 ] = 'RGB(0,0,0,0,255,255)'
        laRGB[lnX,lnY*3+10] = 'RGB(0,0,0,'+IIF(laStyle[lnX,lnY*3+10]>=0,'255,255,0)','255,0,0)')
      ENDIF
    ENDFOR
  ENDFOR
  
  laTotals = 0
  *- Show totals
  SELECT SUM(STK1),SUM(ORD1),SUM(FRE1),;
         SUM(STK2),SUM(ORD2),SUM(FRE2),;
         SUM(STK3),SUM(ORD3),SUM(FRE3),;
         SUM(STK4),SUM(ORD4),SUM(FRE4),;
         SUM(STK5),SUM(ORD5),SUM(FRE5),;
         SUM(STK6),SUM(ORD6),SUM(FRE6),;
         SUM(STK7),SUM(ORD7),SUM(FRE7),;
         SUM(STK8),SUM(ORD8),SUM(FRE8) ;
         FROM &lcTmpFl ;
         WHERE COLOR = laClr[lnColor] ;
         INTO ARRAY laTotals

  laRGBTot = 'RGB(0,0,0,192,192,192)'
  FOR lnJ = 0 TO 7
    IF !EMPTY(laSz[lnJ+1])
      laRGBTot[lnJ*3+1] = 'RGB(0,0,0,192,192,192)'      
      laRGBTot[lnJ*3+2] = 'RGB(0,0,0,73,245,245)'
      laRGBTot[lnJ*3+3] = 'RGB(0,0,0,'+IIF(laTotals[lnJ*3+3]>=0,'230,235,137)','252,112,174)')
    ELSE
      STORE ' ' TO laTotals[lnJ*3+1],laTotals[lnJ*3+2],laTotals[lnJ*3+3]
    ENDIF
  ENDFOR
         
*-- end of lfUpdDat.
