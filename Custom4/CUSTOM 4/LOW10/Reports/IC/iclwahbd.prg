*:***************************************************************************
*: Program file  : ICLWAHDB
*: Program desc. : Ahead & Behind Report 
*: Customer      : Low Profile ( LOW10 )
*: For Report    : ICLWAHBD.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : MMT - Mariam Mazhar Tawfik
*: Entry         : C127561
*:               : This is a Graphics report that will be exported to Microsoft Excel format
*:               : The layout of the XL sheet is how the report must look after it has been Exported.
*:***************************************************************************
*:Modifications  :
*:***************************************************************************
#INCLUDE r:\ARIA4XP\REPORTS\IC\ICLWAHBD.H


*- If no season selected do not continue
IF EMPTY(lcRpSeason) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_Season_Warning)
  RETURN
ENDIF

loogScroll.cCROrientation = 'L'
*
lcStarTime = TIME()
*- The end of week date
IF loOGScroll.llOGFltCh

  STORE 0 TO lnClrLen , lnClrPos
  

  *- Get color position and lenght
  =lfClrData()

  *- Create needed temp file
  =lfCrtTmpFl()

  *- Collect data
  
  IF !lluse_config   
     =lfCollect()
  ELSE
    =lfCollectDyelot()
  ENDIF    
ENDIF 

*- Export or print
SELECT &lcCollect
LOCATE
IF !EOF()
  IF llRpExport
    =lfExprt2XL()
  ELSE
  *
    DO gfDispRe WITH oAriaApplication.ReportHome + 'IC\' + EVAL('lcRpForm')
  ENDIF  
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_No_recrod)

ENDIF  

*!*  *- Close temp created file
*!*  IF USED(lcCollect)
*!*    USE IN &lcCollect
*!*  ENDIF  


*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : When function for the OG
*:***************************************************************************
FUNCTION lfwRepWhen

*Open files using remote table class
loStyle    = CreateObject("RemoteTable","Style","CStyle",'Style',SET("DATASESSION")) 
loScale    = CreateObject("RemoteTable","Scale","Scale",'Scale',SET("DATASESSION")) 
loOrdline  = CreateObject("RemoteTable","Ordline","Ordline",'Ordline',SET("DATASESSION")) 
loOrdHdr   = CreateObject("RemoteTable","OrdHdr","OrdHdr",'OrdHdr',SET("DATASESSION")) 
loCustomer = CreateObject("RemoteTable","Customer","Customer",'Customer',SET("DATASESSION")) 
loInvHdr   = CreateObject("RemoteTable","InvHdr","InvHdr",'InvHdr',SET("DATASESSION")) 
loInvLine  = CreateObject("RemoteTable","InvLine","InvLineo",'InvLine',SET("DATASESSION")) 
loCodes    = CreateObject("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION")) 
loPoshdr   = CreateObject("RemoteTable","Poshdr","Poshdr",'Poshdr',SET("DATASESSION")) 
loPosln    = CreateObject("RemoteTable","Posln","Poslns",'Posln',SET("DATASESSION")) 
loOrdcanln = CreateObject("RemoteTable","Ordcanln","Ordcanln",'Ordcanln',SET("DATASESSION")) 
loStyInvJl = CreateObject("RemoteTable","StyInvJl","StyInvJl",'StyInvJl',SET("DATASESSION")) 
loSHpmthdr = CreateObject("RemoteTable","SHpmthdr","SHpmthdr",'SHpmthdr',SET("DATASESSION")) 
loConsInvl = CreateObject("RemoteTable","CONSINVL","CINVLINE",'CONSINVL',SET("DATASESSION")) 
loStyDye   = CreateObject("RemoteTable","StyDye","StyDye",'StyDye',SET("DATASESSION")) 


*-- if it is the default case
IF lnOgSeting = 1 

  loogscroll.parent.ogtoolbar.cntPrint.cmdPrint.Enabled = .F.
  loogscroll.parent.ogtoolbar.cntPrint.cmdExport.Enabled = .F.
  loogscroll.parent.ogtoolbar.cntPrint.cmdPreview.Enabled = .F.
  loogscroll.parent.ogtoolbar.cntExternal.cmdEmail.Enabled = .F.

ENDIF
*-- end of lfwRepWhen.
*:**************************************************************************
*:* Name        : lfCrtTmpFl
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
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


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SHPTODATE'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

  
DIMENSION laIndx[1,2]
laIndx[1,1] = "STYLE"
laIndx[1,2] = "STYLE"

 = gfCrtTmp(lcCollect,@laFileStru,"STYLE",'STYLE',.T.)
*-- end of lfCrtTmpFl.

*:**************************************************************************
*:* Name        : lfClrData
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
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
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : Collect data
*:***************************************************************************
FUNCTION lfCollect

*- Set appropriate orders

loStyle.Setorder('STYLE')
loORDLINE.Setorder('ORDLINES')
loORDCANLN.Setorder('ORDCANLN')
loPOSLN.Setorder('POSLNS')
loINVLINE.Setorder('INVLINEO')
loINVHDR.Setorder('INVHDR')
loSTYINVJL.Setorder('STYINVJL')

*--Collecting data

STORE {} to ldRpToDt,ldRpFromDt
lnDatePos = ASCAN(loOgScroll.laOgfxFlt,"ORDLINE.PIKDATE")
IF lnDatePos > 0 
  lnDatePos = ASUBSCRIPT(loOgScroll.laOgfxFlt,lnDatePos,1)
  lcDateValue =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnDatePos,6]),loOgScroll.laOgFXFlt[lnDatePos,6],'')
  IF !EMPTY(lcDateValue)
    ldRpToDt=CTOD(SUBSTR(lcDateValue,ATC('|',lcDateValue)+1))
    ldRpFromDt = CTOD(SUBSTR(lcDateValue,1,ATC('|',lcDateValue)-1))
  ENDIF    
ENDIF 

llStyleSelected = .F.
lcStyleSel = ""
lnStylePos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
IF lnStylePos > 0 
  lnStylePos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStylePos ,1)
  lcStyleSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnStylePos,6]),loOGScroll.laOgFxFlt[lnStylePos,6],'')
  IF !EMPTY(lcStyleSel) AND USED(lcStyleSel) AND RECCOUNT(lcStyleSel) > 0
    llStyleSelected = .T.
  ENDIF 
ENDIF   


IF llStyleSelected
  SELECT(lcStyleSel)
  SCAN 
    loStyle.Seek(ALLTRIM(cStyMajor))
    SELECT style
    SCAN REST WHILE Style= ALLTRIM(&lcStyleSel..cstymajor) AND ;
      Style.CPURCODE = lcRpPurcod   and  style.SEASON = lcRpSeason 
    
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
      
      loORDLINE.SEEK(STYLE.STYLE)
      SELECT ORDLINE
      SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
            FOR ORDLINE.SEASON = STYLE.SEASON ;
            .AND. loORDHDR.Seek(ORDLINE.CORDTYPE+ORDLINE.ORDER) AND ORDHDR.STATUS $ 'OHC'
    
        *- Get canceled from OrdCanLn file
        lnCanceled = 0
        IF loORDCANLN.SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6))
          SELECT ORDCANLN
          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
            lnCanceled = lnCanceled + ORDCANLN.TOTQTY
          ENDSCAN
        ENDIF    
   
        *- Total contract ( = Ordered - Cancelled )
        m.TotCntrct = m.TotCntrct + ORDLINE.TOTBOOK - lnCanceled

        *- Call off to date / selected week  ( Allocation )
        IF !EMPTY(ORDLINE.PIKDATE)  AND !EMPTY(DTOS(ldRpToDt)) .AND. ORDLINE.PIKDATE <= ldRpToDt
          m.INVTODATE = m.INVTODATE + ORDLINE.TOTPIK
          IF !EMPTY(DTOS(ldRpToDt))  AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(ORDLINE.PIKDATE,ldRpFromDt,ldRpToDt)
            m.INVWEEK = m.INVWEEK + ORDLINE.TOTPIK
          ENDIF
        ENDIF
      ENDSCAN
      lcOldOrdr = ORDER('INVLINE')
      loINVLINE.Setorder('Invlines')
      SELECT INVLINE
      IF loINVLINE.SEEK(STYLE.STYLE,'INVLINE')      
        SCAN REST WHILE style+invoice+STR(lineno,6) = STYLE.STYLE ;
              FOR INVLINE.SEASON = STYLE.SEASON .AND. ;
              loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
              
          IF !EMPTY(DTOS(ldRpToDt)) AND INVLINE.INVDATE <= ldRpToDt          && From the First to [To Date]
            m.INVTODATE = m.INVTODATE + INVLINE.TOTQTY
          ENDIF
        ENDSCAN
      ENDIF
      loINVLINE.Setorder(lcOldOrdr)
*      SET ORDER TO lcOldOrdr IN INVLINE
       *-Shipped to date 
      IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
        SELECT POSLN
        SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE ;
             FOR INLIST(TranCd,'2','3') .AND. !EMPTY(SHIPNO)
            
          IF loSHPMTHDR.SEEK(POSLN.SHIPNO) AND !EMPTY(DTOS(ldRpToDt)) AND SHPMTHDR.ENTERED <= ldRpToDt
            SELECT SHPMTHDR
            m.ShpToDate = m.ShpToDate + POSLN.TotQty
          ENDIF
        ENDSCAN
      ENDIF   

      *- Call off to date / selected week  ( Invoicing )
      lcSvInvOrd = ORDER('INVLINE')
      loINVLINE.setorder('INVLINES')
      GO TOP
      IF loINVLINE.SEEK(STYLE.STYLE)
        SELECT INVLINE
        SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE ;
              FOR IIF(!EMPTY(DTOS(ldRpFromDt)) AND !EMPTY(DTOS(ldRpToDt)),;
              BETWEEN(INVLINE.INVDATE,ldRpFromDt,ldRpToDt),.T.) ;
                  .AND. loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
           m.INVWEEK = m.INVWEEK + INVLINE.TOTQTY
        ENDSCAN
      ENDIF
      loINVLINE.setorder(lcSvInvOrd )
      *    SET ORDER TO &lcSvInvOrd IN INVLINE
      lnAlocated = STYLE.TOTALO
      *- Available Stock  This is the stock figure - Allocated figure  
      m.STKVAL = m.STKVAL + STYLE.TOTSTK - lnAlocated 
      *- Stock Value at CP  / SP
      m.STKVALCP = m.STKVALCP + (STYLE.TOTSTK - lnAlocated)*STYLE.AVE_COST 
      m.STKVALSP = m.STKVALSP + (STYLE.TOTSTK - lnAlocated)*STYLE.PRICEA 
      *- Call off Value  : Total Invoiced value  + Allocated To give a total figure    
      m.INVVAL = m.INVTODATE * STYLE.PRICEA

      *-Actual to date / This Week  
      IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
        SELECT POSLN
        SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE ;
           FOR TRANCD = '2'
    
          IF !EMPTY(DTOS(ldRpToDt)) AND POSLN.DATE <= ldRpToDt
            m.ACTTODATE = m.ACTTODATE + POSLN.TOTQTY
          ENDIF
 
          IF !EMPTY(DTOS(ldRpToDt)) AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(POSLN.DATE,ldRpFromDt,ldRpToDt)
            m.ACTWEEK = m.ACTWEEK + POSLN.TOTQTY
          ENDIF
        ENDSCAN
      ENDIF   
    
      IF loSTYINVJL.SEEK(STYLE.STYLE)
        SELECT STYINVJL
        SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = STYLE.STYLE ;
              FOR CTRTYPE = '1'
          IF !EMPTY(DTOS(ldRpToDt)) AND STYINVJL.DTRDATE <= ldRpToDt
            m.ACTTODATE = m.ACTTODATE + STYINVJL.NTOTSTK
          ENDIF

          IF !EMPTY(DTOS(ldRpToDt)) AND  !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(STYINVJL.DTRDATE,ldRpFromDt,ldRpToDt)
            m.ACTWEEK = m.ACTWEEK + STYINVJL.NTOTSTK
          ENDIF
        ENDSCAN
      ENDIF

  
      SELECT &lcCollect
      IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
        APPEND BLANK  
      ENDIF
      GATHER MEMVAR MEMORY
    ENDSCAN 
  ENDSCAN 
ELSE
    IF !loStyle.llNative 
      loStyle.sqlrun("SELECT * FROM Style WHERE  Style.CPURCODE = '"+lcRpPurcod+"' AND  style.SEASON = '"+lcRpSeason +"'",'Style')
    ENDIF 
    SELECT style
    SCAN FOR  Style.CPURCODE = lcRpPurcod   and  style.SEASON = lcRpSeason 
    
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
      
      loORDLINE.SEEK(STYLE.STYLE)
      SELECT ORDLINE
      SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
            FOR ORDLINE.SEASON = STYLE.SEASON ;
            .AND. loORDHDR.Seek(ORDLINE.CORDTYPE+ORDLINE.ORDER) AND ORDHDR.STATUS $ 'OHC'
    
        *- Get canceled from OrdCanLn file
        lnCanceled = 0
        IF loORDCANLN.SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6))
          SELECT ORDCANLN
          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
            lnCanceled = lnCanceled + ORDCANLN.TOTQTY
          ENDSCAN
        ENDIF    
   
        *- Total contract ( = Ordered - Cancelled )
        m.TotCntrct = m.TotCntrct + ORDLINE.TOTBOOK - lnCanceled

        *- Call off to date / selected week  ( Allocation )
        IF !EMPTY(ORDLINE.PIKDATE)  AND !EMPTY(DTOS(ldRpToDt)) .AND. ORDLINE.PIKDATE <= ldRpToDt
          m.INVTODATE = m.INVTODATE + ORDLINE.TOTPIK
          IF !EMPTY(DTOS(ldRpToDt))  AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(ORDLINE.PIKDATE,ldRpFromDt,ldRpToDt)
            m.INVWEEK = m.INVWEEK + ORDLINE.TOTPIK
          ENDIF
        ENDIF
      ENDSCAN
      lcOldOrdr = ORDER('INVLINE')
      loINVLINE.Setorder('Invlines')
      SELECT INVLINE
      IF loINVLINE.SEEK(STYLE.STYLE,'INVLINE')      
        SCAN REST WHILE style+invoice+STR(lineno,6) = STYLE.STYLE ;
              FOR INVLINE.SEASON = STYLE.SEASON .AND. ;
              loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
              
          IF !EMPTY(DTOS(ldRpToDt)) AND INVLINE.INVDATE <= ldRpToDt          && From the First to [To Date]
            m.INVTODATE = m.INVTODATE + INVLINE.TOTQTY
          ENDIF
        ENDSCAN
      ENDIF
      loINVLINE.Setorder(lcOldOrdr)
*      SET ORDER TO lcOldOrdr IN INVLINE
       *-Shipped to date 
      IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
        SELECT POSLN
        SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE ;
             FOR INLIST(TranCd,'2','3') .AND. !EMPTY(SHIPNO)
            
          IF loSHPMTHDR.SEEK(POSLN.SHIPNO) AND !EMPTY(DTOS(ldRpToDt)) AND SHPMTHDR.ENTERED <= ldRpToDt
            SELECT SHPMTHDR
            m.ShpToDate = m.ShpToDate + POSLN.TotQty
          ENDIF
        ENDSCAN
      ENDIF   

      *- Call off to date / selected week  ( Invoicing )
      lcSvInvOrd = ORDER('INVLINE')
      loINVLINE.setorder('INVLINES')
      GO TOP
      IF loINVLINE.SEEK(STYLE.STYLE)
        SELECT INVLINE
        SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE ;
              FOR IIF(!EMPTY(DTOS(ldRpFromDt)) AND !EMPTY(DTOS(ldRpToDt)),;
              BETWEEN(INVLINE.INVDATE,ldRpFromDt,ldRpToDt),.T.) ;
                  .AND. loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
           m.INVWEEK = m.INVWEEK + INVLINE.TOTQTY
        ENDSCAN
      ENDIF
      loINVLINE.setorder(lcSvInvOrd )
      *    SET ORDER TO &lcSvInvOrd IN INVLINE
      lnAlocated = STYLE.TOTALO
      *- Available Stock  This is the stock figure - Allocated figure  
      m.STKVAL = m.STKVAL + STYLE.TOTSTK - lnAlocated 
      *- Stock Value at CP  / SP
      m.STKVALCP = m.STKVALCP + (STYLE.TOTSTK - lnAlocated)*STYLE.AVE_COST 
      m.STKVALSP = m.STKVALSP + (STYLE.TOTSTK - lnAlocated)*STYLE.PRICEA 
      *- Call off Value  : Total Invoiced value  + Allocated To give a total figure    
      m.INVVAL = m.INVTODATE * STYLE.PRICEA

      *-Actual to date / This Week  
      IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
        SELECT POSLN
        SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE ;
           FOR TRANCD = '2'
    
          IF !EMPTY(DTOS(ldRpToDt)) AND POSLN.DATE <= ldRpToDt
            m.ACTTODATE = m.ACTTODATE + POSLN.TOTQTY
          ENDIF
 
          IF !EMPTY(DTOS(ldRpToDt)) AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(POSLN.DATE,ldRpFromDt,ldRpToDt)
            m.ACTWEEK = m.ACTWEEK + POSLN.TOTQTY
          ENDIF
        ENDSCAN
      ENDIF   
    
      IF loSTYINVJL.SEEK(STYLE.STYLE)
        SELECT STYINVJL
        SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = STYLE.STYLE ;
              FOR CTRTYPE = '1'
          IF !EMPTY(DTOS(ldRpToDt)) AND STYINVJL.DTRDATE <= ldRpToDt
            m.ACTTODATE = m.ACTTODATE + STYINVJL.NTOTSTK
          ENDIF

          IF !EMPTY(DTOS(ldRpToDt)) AND  !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(STYINVJL.DTRDATE,ldRpFromDt,ldRpToDt)
            m.ACTWEEK = m.ACTWEEK + STYINVJL.NTOTSTK
          ENDIF
        ENDSCAN
      ENDIF

  
      SELECT &lcCollect
      IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
        APPEND BLANK  
      ENDIF
      GATHER MEMVAR MEMORY
    ENDSCAN 
    
    
ENDIF 

*-- end of lfCollect.

*:**************************************************************************
*:* Name        : lfExprt2XL
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : Export data to Excel file
*:***************************************************************************
FUNCTION lfExprt2XL

WAIT WINDOW NOWAIT LANG_Wait
*--copy file ICLWAHBD.XLS to our new file 
lcRpPath = oAriaApplication.WorkDir 
lcRpDefPth = oAriaApplication.Defaultpath && Save The DefPath.

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
    WAIT WINDOW LANG_Error
    IF LASTKEY() = 27
      RETURN
    ENDIF
  ENDIF
ENDDO  
ON ERROR &lcErrStr

lnLen = LEN(lcRpFilNam)
lcRpFilNam =LEFT(lcRpFilNam,lnLen-4)

lcTempMemo = loOGScroll.gfTempName()
SAVE TO (oAriaApplication.WorkDir +lcTempMemo+'.MEM') 
lcCommLine = (oAriaApplication.WorkDir +lcTempMemo+'.MEM')



  =lfExport(lcCommLine)

WAIT CLEAR
*-- end of lfExprt2XL.
*:**************************************************************************
*:* Name        : ifsrvStyle
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : Set-Reset-Valid function for style
*:***************************************************************************
FUNCTION lfsrvStyle
PARAMETERS lcParam
PRIVATE lnSlct
lnSlct = SELECT()
SELECT STYLE
DO CASE
  CASE lcParam = 'S'
    loStyle.SetOrder('CSTYLE')
    SELECT STYLE

    LOCATE              
  CASE lcParam = 'R'
    loStyle.SetOrder('STYLE')
    SELECT STYLE

ENDCASE
SELECT (lnSlct)
*-- end of ifsrvStyle.

*:**************************************************************************
*:* Name        : lfSeason
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : Valid function to collect seasons in one string
*:***************************************************************************
FUNCTION lfvSeason
PRIVATE lnSesPos
lnSesPos = ASCAN(loOgScroll.laOgFxFlt,'STYLE.SEASON')
IF lnSesPos > 0
  lnSesPos = ASUBSCRIPT(loOgScroll.laOgFxFlt , lnSesPos , 1 )
  lcRpSeason = loOgScroll.laOgFxFlt[lnSesPos,6]
  =lfvEx2XL()
ENDIF
*-- end of lfSeason.

*:**************************************************************************
*:* Name        : lfPurcode
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : Valid function to collect Purchased codes in on string
*:***************************************************************************
FUNCTION lfvPurcode
PRIVATE lnPurcdPos
lnPurcdPos = ASCAN(loOgScroll.laOgFxFlt,'STYLE.CPURCODE')
IF lnPurcdPos > 0
  lnPurcdPos = ASUBSCRIPT(loOgScroll.laOgFxFlt , lnPurcdPos , 1 )
  lcRpPurcod = loOgScroll.laOgFxFlt[lnPurcdPos,6]
  =lfvEx2XL()
ENDIF
*-- end of lfPurcode.

*:**************************************************************************
*:* Name        : lfvEx2XL
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : Valid function for Export to Excel choise
*:***************************************************************************
FUNCTION lfvEx2XL

llPrvStat = IIF(EMPTY(lcRpSeason).OR.EMPTY(lcRpPurcod),.F.,.T.)
loogscroll.parent.ogtoolbar.cntPrint.cmdpreview.Enabled = llPrvStat 

llRunStat = IIF(EMPTY(lcRpSeason).OR.EMPTY(lcRpPurcod).OR.llRpExport,.F.,.T.)
loogscroll.parent.ogtoolbar.cntPrint.cmdprint.Enabled = llRunStat 
loogscroll.parent.ogtoolbar.cntPrint.cmdExport.Enabled = llRunStat 
loogscroll.parent.ogtoolbar.cntExternal.cmdEmail.Enabled = llRunStat 


*-- end of lfvEx2XL.


*:**************************************************************************
*:* Name        : lfvDtRng
*:* Developer   : MMT
*:* Date        : 10/05/2005
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
*:* Developer   : MMT
*:* Date        : 10/05/2005
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
*:* Developer   : MMT
*:* Date        : 10/05/2005
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
*:**************************************************************************
*:* Name        : lfExport
*:* Developer   : MMT
*:* Date        : 10/05/2005
*:* Purpose     : export to excel
*:***************************************************************************
*:* Called from : .PRG
*:***************************************************************************
FUNCTION lfExport
LPARAMETERS lcMemoFile



ON ERROR DO lfError 

IF TYPE('lcMemoFile')#'C'
  RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

LOCAL xlsheet,XLApp,tmpsheet

tmpsheet = GetObject(gcWorkDir+lcRpFilNam,'excel.sheet')

XLApp = tmpsheet.application



XLApp.WorkBooks.Add(oAriaApplication.WorkDir +lcRpFilNam)

lnRow = 5
IF !USED(lcCollect)


  USE (oAriaApplication.WorkDir +lcCollect) ORDER 1

ENDIF

XLApp.SHEETS(1).PageSetup.PrintTitleRows = "$1:$4"                                                    

SELECT (lcCollect)  
LOCATE
WITH XLApp.Sheets("Style")

  .Cells(2,2).Value = CPURCDES
  .Cells(3,2).Value = SeasonDES


  .Cells(2,7).Value = IIF(!EMPTY(DTOS(ldRpFromDt)),ldRpFromDt,"")
  .Cells(2,10).Value = IIF(!EMPTY(DTOS(ldRpToDt)),ldRpToDt,"")



 Store 0 TO lnTotCnt , lnTotShp,lnTotActW,lnTotActD,lnTotStk,lnTotCllW,lnTotCllD,lnTotCp,lnTotCllVlueW,lnTotCllVlue,lnTotCntVlu,lnTotBalnce

  DO WHILE !EOF()
    lcStyle = SUBSTR(&lcCollect..CSTYMAJOR,1,lnClrPos-2)
    lcFr = ALLTRIM(STR(lnRow))
    SCAN REST WHILE STYLE = lcStyle
       WAIT WINDOW NOWAIT 'Updating Excel file with style :'+STYLE
      .Cells(lnRow,1).Value  = &lcCollect..CSTYMAJOR
      .Cells(lnRow,2).Value  = &lcCollect..DESC
      .Cells(lnRow,3).Value  = &lcCollect..COLORDES
      .Cells(lnRow,4).Value  = &lcCollect..Dyelot
      .Cells(lnRow,5).Value  = &lcCollect..TOTCNTRCT
     
 
 
     TtlContVal=  &lcCollect..TOTCNTRCT * &lcCollect..PRICEA
      .Cells(lnRow,6).Value  = &lcCollect..ShpToDate
      .Cells(lnRow,7).Value  = &lcCollect..ACTWEEK
      .Cells(lnRow,9).Value  = &lcCollect..ACTTODATE
      .Cells(lnRow,12).Value = &lcCollect..STKVAL
      .Cells(lnRow,13).Value = &lcCollect..INVWEEK
      .Cells(lnRow,14).Value = &lcCollect..INVTODATE
      .Cells(lnRow,15).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,15).Value = &lcCollect..PRICEA
      .Cells(lnRow,16).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,16).Value = &lcCollect..STKVALCP
      .Cells(lnRow,17).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,17).Value = &lcCollect..STKVALSP
      .Cells(lnRow,18).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,18).Value = &lcCollect..PRICEA *  &lcCollect..INVWEEK
      .Cells(lnRow,19).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,19).Value = &lcCollect..INVVAL
      .Cells(lnRow,20).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,20).Value =  TtlContVal
      .Cells(lnRow,21).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
      .Cells(lnRow,21).Value = TtlContVal - &lcCollect..INVVAL
 
      lcTo = ALLTRIM(STR(lnRow))          
      lnRow = lnRow + 1 
 
     lnTotCnt               =lnTotCnt +&lcCollect..TOTCNTRCT
     lnTotShp               =lnTotShp+&lcCollect..ShpToDate
     lnTotActW              =lnTotActW+&lcCollect..ACTWEEK
     lnTotActD              =lnTotActD+ &lcCollect..ACTTODATE
     lnTotStk               =lnTotStk+&lcCollect..STKVAL
     lnTotCllW              =lnTotCllW+&lcCollect..INVWEEK
     lnTotCllD              =lnTotCllD+&lcCollect..INVTODATE
     lnTotCp                =lnTotCp+&lcCollect..STKVALCP
     lnTotCllVlueW          =lnTotCllVlueW+ &lcCollect..PRICEA *  &lcCollect..INVWEEK
     lnTotCllVlue           =lnTotCllVlue+&lcCollect..INVVAL
     lnTotCntVlu            = lnTotCntVlu+&lcCollect..TOTCNTRCT * &lcCollect..PRICEA 
     lnTotBalnce            =lnTotBalnce+((&lcCollect..TOTCNTRCT * &lcCollect..PRICEA)- &lcCollect..INVVAL)
 
    ENDSCAN
    lnRow = lnRow + 1
 
    .Cells(lnRow,3).Value = 'Subtotal'
    .Cells(lnRow,3).Font.FontStyle = 'Bold'
 
    *- Update the summatino row for current style
    .Cells(lnRow,5).Value = '=SUM(E&lcFr.:E&lcTo.)'
    .Cells(lnRow,6).Value = '=SUM(F&lcFr.:F&lcTo.)'
   
    .Cells(lnRow,7).Value = '=SUM(G&lcFr.:G&lcTo.)'
    .Cells(lnRow,9).Value = '=SUM(I&lcFr.:I&lcTo.)'
    .Cells(lnRow,12).Value = '=SUM(L&lcFr.:L&lcTo.)'
    .Cells(lnRow,13).Value = '=SUM(M&lcFr.:M&lcTo.)'
    .Cells(lnRow,14).Value = '=SUM(N&lcFr.:N&lcTo.)'
    .Cells(lnRow,16).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,16).Value = '=SUM(P&lcFr.:P&lcTo.)'
    .Cells(lnRow,18).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,18).Value = '=SUM(R&lcFr.:R&lcTo.)'  
    .Cells(lnRow,19).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,19).Value = '=SUM(S&lcFr.:S&lcTo.)'
    .Cells(lnRow,20).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,20).Value = '=SUM(T&lcFr.:T&lcTo.)' 
    .Cells(lnRow,21).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,21).Value = '=SUM(U&lcFr.:U&lcTo.)' 
 
    *- Set summation row to Bold
    .Cells(lnRow,5).Font.FontStyle = 'Bold'
    .Cells(lnRow,6).Font.FontStyle = 'Bold'

     .Cells(lnRow,7).Font.FontStyle = 'Bold'
    .Cells(lnRow,9).Font.FontStyle = 'Bold'
    .Cells(lnRow,12).Font.FontStyle = 'Bold'
    .Cells(lnRow,13).Font.FontStyle = 'Bold'
    .Cells(lnRow,14).Font.FontStyle = 'Bold'
    .Cells(lnRow,16).Font.FontStyle = 'Bold'
    .Cells(lnRow,18).Font.FontStyle = 'Bold'
 
    .Cells(lnRow,19).Font.FontStyle = 'Bold'
    .Cells(lnRow,20).Font.FontStyle = 'Bold'
    .Cells(lnRow,21).Font.FontStyle = 'Bold'
     lnRow = lnRow + 2
  ENDDO
    lnRow = lnRow + 1
    .Cells(lnRow,4).Value = 'Dept total'
    .Cells(lnRow,4).Font.FontStyle = 'Bold'
    .Cells(lnRow,5).Value =lnTotCnt  
    .Cells(lnRow,6).Value =lnTotShp  
    .Cells(lnRow,7).Value = lnTotActW 
    .Cells(lnRow,9).Value =lnTotActD  
    .Cells(lnRow,12).Value = lnTotStk 
    .Cells(lnRow,13).Value =lnTotCllW  
    .Cells(lnRow,14).Value = lnTotCllD
    .Cells(lnRow,16).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,16).Value = lnTotCp 
    .Cells(lnRow,18).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,18).Value = lnTotCllVlueW 
    .Cells(lnRow,19).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,19).Value = lnTotCllVlue     
    .Cells(lnRow,20).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,20).Value =lnTotCntVlu  
    .Cells(lnRow,21).NUMBERFORMAT = IIF(llRpDec,'[$£-809]#,##0.00','[$£-809]#,##0')
    .Cells(lnRow,21).Value =lnTotBalnce          
    .Cells(lnRow,5).Font.FontStyle = 'Bold'
    .Cells(lnRow,6).Font.FontStyle = 'Bold'
    .Cells(lnRow,7).Font.FontStyle = 'Bold'
    .Cells(lnRow,9).Font.FontStyle = 'Bold'
    .Cells(lnRow,12).Font.FontStyle = 'Bold'
    .Cells(lnRow,13).Font.FontStyle = 'Bold'
    .Cells(lnRow,14).Font.FontStyle = 'Bold'
    .Cells(lnRow,15).Font.FontStyle = 'Bold'
    .Cells(lnRow,16).Font.FontStyle = 'Bold'
    .Cells(lnRow,18).Font.FontStyle = 'Bold'
    .Cells(lnRow,19).Font.FontStyle = 'Bold'
    .Cells(lnRow,20).Font.FontStyle = 'Bold'
    .Cells(lnRow,21).Font.FontStyle = 'Bold'
ENDWITH
*C127561,1 MMR [End] 
XLApp.Visible = .T.

ERASE (lcMemoFile)
*:**************************************************************************
*:* Name        : lfError
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/05/2004
*:* Purpose     : Quit if any error 
*:***************************************************************************

FUNCTION lfError
=gfModalGen("TRM000000B00000","DIALOG",'','','Error while creating the Excel file , please retry.')
READ EVENTS
*--End of lfError

*:**************************************************************************
*:* Name        : lfCollectDyelot
*:* Developer   : MMT - Mariam Mazhar Tawfik
*:* Date        : 10/12/2004
*:* Purpose     : used to collect data in case of dyelot used 
  *:***************************************************************************
FUNCTION lfCollectDyelot

loStyle.Setorder('STYLE')
loORDLINE.Setorder('ORDLINES')
loORDCANLN.Setorder('ORDCANLN')
loPOSLN.Setorder('POSLNS')
loINVLINE.Setorder('INVLINEO')
loINVHDR.Setorder('INVHDR')
loSTYINVJL.Setorder('STYINVJL')

*--Collecting data

STORE {} to ldRpToDt,ldRpFromDt
lnDatePos = ASCAN(loOgScroll.laOgfxFlt,"ORDLINE.PIKDATE")
IF lnDatePos > 0 
  lnDatePos = ASUBSCRIPT(loOgScroll.laOgfxFlt,lnDatePos,1)
  lcDateValue =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnDatePos,6]),loOgScroll.laOgFXFlt[lnDatePos,6],'')
  IF !EMPTY(lcDateValue)
    ldRpToDt=CTOD(SUBSTR(lcDateValue,ATC('|',lcDateValue)+1))
    ldRpFromDt = CTOD(SUBSTR(lcDateValue,1,ATC('|',lcDateValue)-1))
  ENDIF    
ENDIF 

llStyleSelected = .F.
lcStyleSel = ""
lnStylePos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
IF lnStylePos > 0 
  lnStylePos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnStylePos ,1)
  lcStyleSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnStylePos,6]),loOGScroll.laOgFxFlt[lnStylePos,6],'')
  IF !EMPTY(lcStyleSel) AND USED(lcStyleSel) AND RECCOUNT(lcStyleSel) > 0
    llStyleSelected = .T.
  ENDIF 
ENDIF   


IF llStyleSelected
  SELECT(lcStyleSel)
  SCAN 
    loStyle.Seek(ALLTRIM(cStyMajor))
    SELECT style
    SCAN REST WHILE Style= ALLTRIM(&lcStyleSel..cstymajor) AND ;
    Style.CPURCODE = lcRpPurcod   and  style.SEASON = lcRpSeason 
       loStyDye.Seek(STYLE.STYLE)
       SELECT Stydye 
       SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE FOR  !EMPTY(DYELOT)
         IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
          SELECT &lcCollect
          SCATTER MEMVAR MEMORY BLANK
         ENDIF
         m.STYLE = STYLE.STYLE
         m.Dyelot = Stydye.Dyelot
         m.CSTYMAJOR = STYLE.CSTYMAJOR
         m.DESC = STYLE.DESC
         m.COLORDES = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) + '-' + ;
               ALLTRIM(gfCodDes(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),'COLOR'))
         m.CPURCDES = gfCodDes(STYLE.CPURCODE,'CPURCODE')
         m.SeasonDES = gfCodDes(STYLE.SEASON,'SEASON')
         m.PriceA = STYLE.PRICEA
         lnAlocated = 0
      
        loORDLINE.SEEK(STYLE.STYLE)
        SELECT ORDLINE
        SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
              FOR ORDLINE.SEASON = STYLE.SEASON  AND ORDLINE.Dyelot = Stydye.Dyelot;
             .AND. loORDHDR.Seek(ORDLINE.CORDTYPE+ORDLINE.ORDER) AND ORDHDR.STATUS $ 'OHC'
              
    
          *- Get canceled from OrdCanLn file
          lnCanceled = 0
          IF loORDCANLN.SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6))
            SELECT ORDCANLN
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
              lnCanceled = lnCanceled + ORDCANLN.TOTQTY
            ENDSCAN
          ENDIF    
   
          *- Total contract ( = Ordered - Cancelled )
          m.TotCntrct = m.TotCntrct + ORDLINE.TOTBOOK - lnCanceled

          *- Call off to date / selected week  ( Allocation )
          IF !EMPTY(ORDLINE.PIKDATE)  AND !EMPTY(DTOS(ldRpToDt)) .AND. ORDLINE.PIKDATE <= ldRpToDt
            m.INVTODATE = m.INVTODATE + ORDLINE.TOTPIK
            IF !EMPTY(DTOS(ldRpToDt))  AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(ORDLINE.PIKDATE,ldRpFromDt,ldRpToDt)
              m.INVWEEK = m.INVWEEK + ORDLINE.TOTPIK
            ENDIF
          ENDIF
        ENDSCAN
        lcOldOrdr = ORDER('INVLINE')
        loINVLINE.Setorder('Invlines')
        SELECT INVLINE
        IF loINVLINE.SEEK(STYLE.STYLE,'INVLINE')      
          SCAN REST WHILE style+invoice+STR(lineno,6) = STYLE.STYLE ;
              FOR INVLINE.SEASON = STYLE.SEASON .AND. INVLINE.DYELOT = Stydye.DYELOT .AND.  ;
              loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
              
            IF !EMPTY(DTOS(ldRpToDt)) AND INVLINE.INVDATE <= ldRpToDt          && From the First to [To Date]
              m.INVTODATE = m.INVTODATE + INVLINE.TOTQTY
            ENDIF
          ENDSCAN
        ENDIF
        loINVLINE.Setorder(lcOldOrdr)
*       SET ORDER TO lcOldOrdr IN INVLINE
        *-Shipped to date 
        IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
          SELECT POSLN
          SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE  AND posln.DYELOT = Stydye.DYELOT ;
             FOR INLIST(TranCd,'2','3') .AND. !EMPTY(SHIPNO)
            
            IF loSHPMTHDR.SEEK(POSLN.SHIPNO) AND !EMPTY(DTOS(ldRpToDt)) AND SHPMTHDR.ENTERED <= ldRpToDt
              SELECT SHPMTHDR
              m.ShpToDate = m.ShpToDate + POSLN.TotQty
            ENDIF
          ENDSCAN
        ENDIF   

        *- Call off to date / selected week  ( Invoicing )
        lcSvInvOrd = ORDER('INVLINE')
        loINVLINE.setorder('INVLINES')
        GO TOP
        IF loINVLINE.SEEK(STYLE.STYLE)
          SELECT INVLINE
          SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE  AND  INVLINE.DYELOT = Stydye.DYELOT ;
              FOR IIF(!EMPTY(DTOS(ldRpFromDt)) AND !EMPTY(DTOS(ldRpToDt)),;
              BETWEEN(INVLINE.INVDATE,ldRpFromDt,ldRpToDt),.T.) ;
                  .AND. loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
             m.INVWEEK = m.INVWEEK + INVLINE.TOTQTY
          ENDSCAN
        ENDIF
        loINVLINE.setorder(lcSvInvOrd )
        *    SET ORDER TO &lcSvInvOrd IN INVLINE
        lnAlocated = STYLE.TOTALO
        *- Available Stock  This is the stock figure - Allocated figure  
        m.STKVAL = m.STKVAL + STYLE.TOTSTK - lnAlocated 
        *- Stock Value at CP  / SP
        m.STKVALCP = m.STKVALCP + (STYLE.TOTSTK - lnAlocated)*STYLE.AVE_COST 
        m.STKVALSP = m.STKVALSP + (STYLE.TOTSTK - lnAlocated)*STYLE.PRICEA 
        *- Call off Value  : Total Invoiced value  + Allocated To give a total figure    
        m.INVVAL = m.INVTODATE * STYLE.PRICEA

        *-Actual to date / This Week  
        IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
          SELECT POSLN
          SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE AND POSLN.DYELOT = Stydye.DYELOT ;
           FOR TRANCD = '2'
    
            IF !EMPTY(DTOS(ldRpToDt)) AND POSLN.DATE <= ldRpToDt
              m.ACTTODATE = m.ACTTODATE + POSLN.TOTQTY
            ENDIF
 
            IF !EMPTY(DTOS(ldRpToDt)) AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(POSLN.DATE,ldRpFromDt,ldRpToDt)
              m.ACTWEEK = m.ACTWEEK + POSLN.TOTQTY
            ENDIF
          ENDSCAN
        ENDIF    
        
        IF loSTYINVJL.SEEK(STYLE.STYLE)
          SELECT STYINVJL
          SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = STYLE.STYLE ;
            AND  STYINVJL.CDYELOT = Stydye.DYELOT FOR CTRTYPE = '1'
            IF !EMPTY(DTOS(ldRpToDt)) AND STYINVJL.DTRDATE <= ldRpToDt
              m.ACTTODATE = m.ACTTODATE + STYINVJL.NTOTSTK
            ENDIF

            IF !EMPTY(DTOS(ldRpToDt)) AND  !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(STYINVJL.DTRDATE,ldRpFromDt,ldRpToDt)
              m.ACTWEEK = m.ACTWEEK + STYINVJL.NTOTSTK
            ENDIF
          ENDSCAN
        ENDIF
  
        SELECT &lcCollect
        IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
          APPEND BLANK  
        ENDIF
        GATHER MEMVAR MEMORY
      ENDSCAN 
    ENDSCAN   
  ENDSCAN 
ELSE
    IF !loStyle.llNative 
      loStyle.sqlrun("SELECT * FROM Style WHERE  Style.CPURCODE = '"+lcRpPurcod+"' AND  style.SEASON = '"+lcRpSeason +"'",'Style')
    ENDIF 
    SELECT style
    SCAN FOR  Style.CPURCODE = lcRpPurcod   and  style.SEASON = lcRpSeason 
      loStyDye.Seek(STYLE.STYLE)
      SELECT Stydye 
      SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE FOR  !EMPTY(DYELOT)
        IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
          SELECT &lcCollect
          SCATTER MEMVAR MEMORY BLANK
        ENDIF
    
        m.STYLE = STYLE.STYLE
        m.dyelot = STYDYE.Dyelot
        m.CSTYMAJOR = STYLE.CSTYMAJOR
        m.DESC = STYLE.DESC
        m.COLORDES = SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) + '-' + ;
                ALLTRIM(gfCodDes(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),'COLOR'))
        m.CPURCDES = gfCodDes(STYLE.CPURCODE,'CPURCODE')
        m.SeasonDES = gfCodDes(STYLE.SEASON,'SEASON')
        m.PriceA = STYLE.PRICEA
        lnAlocated = 0
      
        loORDLINE.SEEK(STYLE.STYLE)
        SELECT ORDLINE
        SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
              FOR ORDLINE.SEASON = STYLE.SEASON AND ORDLINE.Dyelot = Stydye.Dyelot;
             .AND. loORDHDR.Seek(ORDLINE.CORDTYPE+ORDLINE.ORDER) AND ORDHDR.STATUS $ 'OHC'
    
          *- Get canceled from OrdCanLn file
          lnCanceled = 0
          IF loORDCANLN.SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6))
            SELECT ORDCANLN
            SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDLINE.CORDTYPE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
              lnCanceled = lnCanceled + ORDCANLN.TOTQTY
            ENDSCAN
          ENDIF    
   
          *- Total contract ( = Ordered - Cancelled )
          m.TotCntrct = m.TotCntrct + ORDLINE.TOTBOOK - lnCanceled

          *- Call off to date / selected week  ( Allocation )
          IF !EMPTY(ORDLINE.PIKDATE)  AND !EMPTY(DTOS(ldRpToDt)) .AND. ORDLINE.PIKDATE <= ldRpToDt
            m.INVTODATE = m.INVTODATE + ORDLINE.TOTPIK
            IF !EMPTY(DTOS(ldRpToDt))  AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(ORDLINE.PIKDATE,ldRpFromDt,ldRpToDt)
              m.INVWEEK = m.INVWEEK + ORDLINE.TOTPIK
            ENDIF
          ENDIF
        ENDSCAN
        lcOldOrdr = ORDER('INVLINE')
        loINVLINE.Setorder('Invlines')
        SELECT INVLINE
        IF loINVLINE.SEEK(STYLE.STYLE,'INVLINE')      
          SCAN REST WHILE style+invoice+STR(lineno,6) = STYLE.STYLE ;
              FOR INVLINE.SEASON = STYLE.SEASON .AND. INVLINE.Dyelot = Stydye.Dyelot AND ;
              loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
              
             IF !EMPTY(DTOS(ldRpToDt)) AND INVLINE.INVDATE <= ldRpToDt          && From the First to [To Date]
              m.INVTODATE = m.INVTODATE + INVLINE.TOTQTY
            ENDIF
          ENDSCAN
        ENDIF
        loINVLINE.Setorder(lcOldOrdr)
*        SET ORDER TO lcOldOrdr IN INVLINE
       *-Shipped to date 
        IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
          SELECT POSLN
          SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE  AND POSLN.Dyelot = Stydye.Dyelot;
             FOR INLIST(TranCd,'2','3') .AND. !EMPTY(SHIPNO)
            
            IF loSHPMTHDR.SEEK(POSLN.SHIPNO) AND !EMPTY(DTOS(ldRpToDt)) AND SHPMTHDR.ENTERED <= ldRpToDt
              SELECT SHPMTHDR
              m.ShpToDate = m.ShpToDate + POSLN.TotQty
            ENDIF
          ENDSCAN
        ENDIF   

        *- Call off to date / selected week  ( Invoicing )
        lcSvInvOrd = ORDER('INVLINE')
        loINVLINE.setorder('INVLINES')
        GO TOP
        IF loINVLINE.SEEK(STYLE.STYLE)
          SELECT INVLINE
          SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE  AND Invline.Dyelot = Stydye.Dyelot;
              FOR IIF(!EMPTY(DTOS(ldRpFromDt)) AND !EMPTY(DTOS(ldRpToDt)),;
              BETWEEN(INVLINE.INVDATE,ldRpFromDt,ldRpToDt),.T.) ;
                  .AND. loINVHDR.Seek(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V'
             m.INVWEEK = m.INVWEEK + INVLINE.TOTQTY
          ENDSCAN
        ENDIF
        loINVLINE.setorder(lcSvInvOrd )
        *    SET ORDER TO &lcSvInvOrd IN INVLINE
        lnAlocated = STYLE.TOTALO
        *- Available Stock  This is the stock figure - Allocated figure  
        m.STKVAL = m.STKVAL + STYLE.TOTSTK - lnAlocated 
        *- Stock Value at CP  / SP
        m.STKVALCP = m.STKVALCP + (STYLE.TOTSTK - lnAlocated)*STYLE.AVE_COST 
        m.STKVALSP = m.STKVALSP + (STYLE.TOTSTK - lnAlocated)*STYLE.PRICEA 
        *- Call off Value  : Total Invoiced value  + Allocated To give a total figure    
        m.INVVAL = m.INVTODATE * STYLE.PRICEA

        *-Actual to date / This Week  
        IF loPOSLN.SEEK(STYLE.STYLE)  && key : STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
          SELECT POSLN
          SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE  AND POsln.Dyelot = Stydye.Dyelot;
            FOR TRANCD = '2'
    
            IF !EMPTY(DTOS(ldRpToDt)) AND POSLN.DATE <= ldRpToDt
              m.ACTTODATE = m.ACTTODATE + POSLN.TOTQTY
            ENDIF
 
            IF !EMPTY(DTOS(ldRpToDt)) AND !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(POSLN.DATE,ldRpFromDt,ldRpToDt)
              m.ACTWEEK = m.ACTWEEK + POSLN.TOTQTY
            ENDIF
          ENDSCAN 
        ENDIF   
    
        IF loSTYINVJL.SEEK(STYLE.STYLE)
          SELECT STYINVJL
          SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = STYLE.STYLE  AND STYINVJL.CDyelot = Stydye.Dyelot;
              FOR CTRTYPE = '1'
            IF !EMPTY(DTOS(ldRpToDt)) AND STYINVJL.DTRDATE <= ldRpToDt
              m.ACTTODATE = m.ACTTODATE + STYINVJL.NTOTSTK
            ENDIF

            IF !EMPTY(DTOS(ldRpToDt)) AND  !EMPTY(DTOS(ldRpFromDt)) AND BETWEEN(STYINVJL.DTRDATE,ldRpFromDt,ldRpToDt)
              m.ACTWEEK = m.ACTWEEK + STYINVJL.NTOTSTK
            ENDIF
          ENDSCAN
        ENDIF
        SELECT &lcCollect
        IF !SEEK(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),lcCollect)
          APPEND BLANK  
        ENDIF
        GATHER MEMVAR MEMORY
      ENDSCAN 
   ENDSCAN    
ENDIF 

*-- end of lfCollect.

