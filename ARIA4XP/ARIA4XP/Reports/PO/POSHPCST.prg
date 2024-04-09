*!*************************************************************
*! Name      : POSHPCST
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/13/2008
*! Report    : Shipment cost sheet report 
*! Entry     : E302483,[T20061128.0001]
*!*************************************************************
*! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[T20061128.0001]
*! E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*!*************************************************************
#INCLUDE r:\Aria4XP\Reports\PO\POSHPCST.h

IF EMPTY(laRpMTarget[1])
  =gfModalGen('INM00000B34000',.F.,.F.,.F.,LANG_ONE_MFG)
  RETURN .F.
ENDIF 

IF alEN(laRpMTarget) > 5
  =gfModalGen('INM00000B34000',.F.,.F.,.F.,LANG_FIVE_MFG)
  RETURN .F.
ENDIF 


IF loOgScroll.llOGFltCh
  *--collecting data 
  lfCreatTemp()
  =lfCollectData()
ELSE
  IF !USED(lcRepfile)
    USE oAriaApplication.WorkDir +  lcRepfile  + ".DBF" IN 0  
  ENDIF   
ENDIF   


SELECT(lcRepfile)&&the file will print from it
IF lnRpMargin <> 0 AND lnRpMarginB <> 0 
  DELETE FOR NOT (MArgin < lnRpMarginB  AND MArgin > lnRpMargin)
ELSE
  IF lnRpMargin <> 0 
    DELETE FOR NOT (MArgin > lnRpMargin)
  ELSE
    IF lnRpMarginB <> 0
      DELETE FOR NOT (MArgin < lnRpMarginB)
    ENDIF   
  ENDIF 
ENDIF   



SELECT(lcRepfile)&&the file will print from it
lnTotRec = 0 
COUNT FOR !DELETED() TO lnTotRec 




IF lnTotRec =0   &&if the file is empty
*--no records to display
  =gfModalGen('TRM00052B34000','ALERT')
  RETURN .F.
ENDIF  &&endif the file is not empty



DIMENSION loOGScroll.laCRParams[2,2]
loOGScroll.laCRParams[1,1] = 'ReportName'
loOGScroll.laCRParams[1,2] = LANG_REPORT_TITLE 

loOGScroll.laCRParams[2,1] = 'ShowMarg'
loOGScroll.laCRParams[2,2] = IIF(gfUserPriv('PO','POACFRV','SHOWMRG'),1,0)





DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRepFile+'.DBF' 
USE IN (lcRepFile)&&close the file will display from to be opened exclusive

= gfDispRe()



*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/13/2008
*! Purpose   : Report When Function
*!*************************************************************
FUNCTION lfwRepWhen
DECLARE laRpSource[4],laRpTarget[1]

lnStatus = lcRpStatus

STORE 'Open'      TO laRpSource[1]
STORE 'Completed' TO laRpSource[2]
STORE 'Bid'  TO laRpSource[3]
STORE 'Hold'  TO laRpSource[4]
lcRpStatus = ''

=gfOpenTable('POSLN','POSLN','SH')
=gfOpenTable('POSLN','POSLN','SH','POSLN_A')
=gfOpenTable('POSHDR','POSHDR','SH','POSHDR')
=gfOpenTable('SHPMTHDR','SHPMTHDR','SH','SHPMTHDR')
=gfOpenTable('SHPRLFLD','SHPRLFLD','SH','SHPRLFLD')
=gfOpenTable('STYLE','STYLE','SH','STYLE')
=gfOpenTable('shpmfgdt','shpmfgdt','SH','shpmfgdt')
=gfOpenTable('APVENDOR','VENCODE','SH','APVENDOR')
=gfOpenTable('shpcstln','shpcstln','SH','shpcstln')
=gfOpenTable('CUTPICK','CUTPICK','SH','CUTPICK')
=gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR')
= gfOpenTable('CODES' ,'CCODE_NO','SH','CODES_A')
= gfOpenTable('bomline' ,'bomlnshp','SH','bomline')



lfFillMfg()
lfPrntMfg()


*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 01/13/2008
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 

*!*************************************************************
*! Name      : RefreshStatusM
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 01/13/2008
*! Purpose   : Return the selected MFG in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatusM
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpMTarget)
    FOR lnTarget = 1 TO ALEN(laRpMTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpMTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
*!*************************************************************
*! Name        : lfsrvShp  
*! Developer   : Mariam Mazhar(MMT)
*! Date        : 05/09/2004
*! Purpose     : Collect data for warehouse
*!*************************************************************
*! Called from : 
*!*************************************************************
FUNCTION lfsrvShp  
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    SELECT shpmthdr
    SET FILTER TO ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P' 
  CASE lcParm = 'R'  && Reset code
    SELECT shpmthdr
    SET FILTER TO ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P' 
ENDCASE
*!*************************************************************
*! Name      : lfStatus
*! Developer : Mariam Mazhar (MAB)
*! Date      : 01/13/2008
*! Purpose   : Translate the Status field from symbol to be a complete word 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfStatus()
*!*************************************************************
FUNCTION lfStatus
PARAMETERS lcStatSym
lcStatus  = ""
DO CASE 
  CASE lcStatSym ='O'
    lcStatus = 'Open'
    
  CASE lcStatSym ='H'
    lcStatus = 'On Hold'
    
  CASE lcStatSym ='B'
    lcStatus = 'Bid'
    
  CASE lcStatSym ='C'
    lcStatus = 'Completed'
ENDCASE     
RETURN lcStatus
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/13/2008
*! Purpose   : Valid function for STSTUS Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

*= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,'Select Report Status',"",.T.,.F.) && call mover function.
lcRpStatus = ' '
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
      IIF(laRpTarget[lnI] = 'Bid','B',;
      IIF(laRpTarget[lnI] = 'Complete','C',IIF(laRpTarget[lnI] = 'On Hold',"H",''))))

  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length
IF LEN(lcOldStat) != LEN(lcRpStatus)
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*!*************************************************************
*! Name      : lfCollectData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/13/2008
*! Purpose   : Data Collecting
*!*************************************************************
FUNCTION lfCollectData




WAIT WINDOW "Collecting Data....." NOWAIT 
lcCursorShip = ""
llShipSelect = .F.
lnPosShip = ASCAN(loOgScroll.laOgFXFlt,"SHPMTHDR.SHIPNO")
IF lnPosShip  >0 
  lnPosShip = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosShip ,1)
  lcCursorShip = loOgScroll.laOgFxFlt[lnPosShip ,6]
ENDIF       
IF !EMPTY(lcCursorShip)
  SELECT(lcCursorShip)
  LOCATE 
   IF !EOF()
    llShipSelect = .T.
  ENDIF 
ENDIF 

llVendSelect = .F.
lcCursorVend =""
lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.CVENDCODE")
IF lnPosVend>0 
  lnPosVend= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
  lcCursorVend = loOgScroll.laOgFxFlt[lnPosVend,6]
ENDIF       
IF !EMPTY(lcCursorVend)
  SELECT(lcCursorVend)
  LOCATE 
   IF !EOF()
    llVendSelect = .T.
  ENDIF 
ENDIF 


llStySelect = .F.
lcCursorSty = ""
lnPosSty = ASCAN(loOgScroll.laOgFXFlt,"STYLE.STYLE")
IF lnPosSty >0 
  lnPosSty= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
  lcCursorSty = loOgScroll.laOgFxFlt[lnPosSty,6]
ENDIF       
IF !EMPTY(lcCursorSty)
  SELECT(lcCursorSty)
  LOCATE 
   IF !EOF()
    llStySelect = .T.
  ENDIF 
ENDIF 

llUseSeason  = .F.
llUseDiv     = .F.
llUseMFGCod = .F.


llUseSeason  = .F.
lnSeaPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
IF lnSeaPos > 0 
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnSeaPos,6]),loOgScroll.laOgFXFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel) 
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
  ENDIF   
ENDIF   

*DIVISION
llUseDiv  = .F.
lnDivPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CDIVISION")
IF lnDivPos > 0 
  lnDivPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnDivPos,1)
  lcDivSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnDivPos,6]),loOgScroll.laOgFXFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel) 
    lcDivFile = loOGScroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
  ENDIF   
ENDIF   

*MFG Operation Code
llUseMFGCod  = .F.
lnMFGPos = ASCAN(loOgScroll.laOgFXFlt,"SHPRLFLD.COPRCODE")
IF lnMFGPos > 0 
  lnMFGPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnMFGPos ,1)
  lcMFGSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnMFGPos ,6]),loOgScroll.laOgFXFlt[lnMFGPos ,6],'')
  IF !EMPTY(lcMFGSel) 
    lcMFGFile = loOGScroll.gfTempName()
    llUseMFGCod = IIF(LEN(lcMFGSel )>0,.T.,.F.) AND lfConvertToCursor(lcMFGSel,'CMFGCODE',lcMFGFile )
  ENDIF   
ENDIF   

lnStyMajor = LEN(gfItemMask('PM'))
lnClrLnGl   = 0
lnClrPosGL = 0

DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR


*lnRpMargin
*lnRpMarginB
DIMENSION laCostItem[1]
IF llShipSelect 
  SELECT POSLN
  gfSetOrder('poslnsh')
  SELECT(lcCursorShip)
  SCAN 
    IF gfSeek("PP"+SHIPNO,"SHPMTHDR") AND;
      IIF(!EMPTY(lcRpStatus),SHPMTHDR.Status $ lcRpStatus,.T.) AND NOT ISNULL(SHPMTHDR.ctemplate)
      
   	  
      =gfSeek(SHPMTHDR.ctemplate,'shpcstln')
      IF EMPTY(laRpMTarget[1])	
        SELECT CCODE_NO+'-'+CDESCRIP, '*'+CCODE_NO+'*',0,CCATGTYP,CBOMTYP,.F.;
        FROM SHPCSTLN ;
        WHERE CTEMPLATE = PADR(lcTempValue,6);
        ORDER BY LINENO ;
        INTO ARRAY laCostItem

        IF !EMPTY(laCostItem)
          FOR lnI = 1 TO ALEN(laCostItem,1)
            IF laCostItem[lnI,4] = 'D'
              laCostItem[lnI,2] = '*'+'######'+'*'
            ENDIF
          ENDFOR
        ENDIF  
  	ELSE
      SELECT SHPCSTLN.CCODE_NO+'-'+SHPCSTLN.CDESCRIP, '*'+SHPCSTLN.CCODE_NO+'*',0,SHPCSTLN.CCATGTYP,SHPCSTLN.CBOMTYP,.F.;
      FROM SHPCSTLN WHERE CTEMPLATE = PADR(SHPMTHDR.ctemplate,6) AND ASCAN(laRpMTarget,SHPCSTLN.CCODE_NO) > 0;
   	  ORDER BY LINENO ;
   	  INTO ARRAY laCostItem
    
      IF !EMPTY(laCostItem)
        FOR lnI = 1 TO ALEN(laCostItem,1)
          IF laCostItem[lnI,4] = 'D'
            laCostItem[lnI,2] = '*'+'######'+'*'
          ENDIF
        ENDFOR
      ENDIF  
    ENDIF   
  
    IF EMPTY(laCostItem[1])
      SELECT(lcCursorShip)
      LOOP 
    ENDIF   
  
  
  *POSLN.TRANCD = '3' AND 
  
    SELECT  shpcstln
    IF gfSeek(&lcCursorShip..SHIPNO+"PP","POSLN")
      SELECT POSLN
      SCAN REST WHILE shipno+ cbusdocu+ cstytype+po+ cinvtype+ style+ STR(lineno,6)+ trancd = &lcCursorShip..SHIPNO+"PP" FOR ;
                      IIF(llStySelect  ,SEEK(POSLN.Style,lcCursorSty),.T.) AND;
                      gfSeek(POSLN.Style,'Style') AND IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND ;
                      IIF(llUseDiv  ,SEEK(STYLE.CDIVISION,lcDivFile ),.T.) ;
                      AND IIF(llVendSelect  ,gfSeek("PP"+POSLN.PO,"POSHDR") AND SEEK(POSHDR.Vendor,lcCursorVend),.T.) 
            
        =gfSeek("PP"+POSLN.PO,"POSHDR")  
        M.SHIPNO = SHPMTHDR.shipno
        m.Status = SHPMTHDR.Status
        m.NoCart = SHPMTHDR.cartons
        m.CVESSEL = SHPMTHDR.CVESSEL
        m.REFERENCE = SHPMTHDR.reference
        m.Container = SHPMTHDR.airwayb
        m.ETA = SHPMTHDR.ETA
        m.totqtyhdr = SHPMTHDR.totqtyhdr
        m.RecQty = SHPMTHDR.recv_can + SHPMTHDR.recv_dam+ SHPMTHDR.recv_stk
        m.DutyExRate  = POSHDR.NDUTYRAT
        =gfSeek(SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6),'SHPRLFLD')
        m.EnteredBy = SHPRLFLD.Cadd_user + "  " + DTOC(SHPRLFLD.dAdd_Date)
        
        *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
        m.DutyExRate = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
         *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

        
        SELECT SHPRLFLD
        SCAN REST while  shipno+ po+ STR(lineno,6) = SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6)
        
          m.PO = POSLN.PO
          m.CVenName = IIF(gfSeek(POSHDR.VENDOR,'APVENDOR'),apvendor.cvencomp ,"")
          m.Style = SUBSTR(POSLN.Style,1,lnStyMajor)
          m.Fstyle = POSLN.Style
          m.lineno = POSLN.lineno
          m.StyDesc = style.desc
          m.cClrDesc =  gfCodDes(SUBSTR(POSLN.Style , lnClrPosGL , lnClrLnGl),'COLOR')
          m.QtyOrg   =  POsln.TOTQTY
          m.POCurr =   POSHDR.cpricecur
          m.POprice = POSLN.NFCOST1
          m.NfCost1  = POSLN.NFCOST1
          m.PRICEA = IIF(SHPRLFLD.PRICEA = 0,STYLE.PRICEA,SHPRLFLD.PRICEA)
          
          m.Royal   = SHPRLFLD.NROYALTY
          

          
          IF m.PRICEA <> 0  AND m.QtyOrg <> 0
            m.RoyalPer = (m.Royal * 100) / (m.PRICEA*m.QtyOrg)
          ENDIF   
          
          * m.Royal    = m.PRICEA*m.RoyalPer*m.QtyOrg/100  
          
          M.NDUTYRAT   = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
          M.SoQty  = Style.TotOrd
          
          =gfSeek("PP"+POSLN.PO+'0001'+POSLN.Style+STR(POSLN.LINENo,6)+"1",'POSLN_A','POSLN')
          
          M.PoQty  = POSLN_A.totQTY
          
          PRIVATE laRoyalty,lnRylrate
          DECLARE laRoyalty[1,2]
          lnRylrate = 0
          laRoyalty[1,1] = 'NRYLRATE'
          laRoyalty[1,2] = 'lnRylrate'
          
          =gfRltFld(STYLE.ROYALTY,@laRoyalty,'ROYALTY   ')
          
          IF m.Royal     = 0
            m.RoyalPer = lnRylrate 
            m.Royal    = m.PRICEA*lnRylrate*m.QtyOrg/100  
          ENDIF 
              
          m.SPRICE   =  m.PRICEA
          m.TrdDisc  = SHPRLFLD.NTRDISC
          m.SetDisc  = SHPRLFLD.STLMNTDISC
          m.NPRICERAT = IIF(SHPRLFLD.NPRICERAT = 0 , POSHDr.NPRICERAT , SHPRLFLD.NPRICERAT)      
          m.NCURRUNIT = POSHDr.NCURRUNIT
              
              
           *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
          IF m.PRICEA <> 0  AND m.QtyOrg <> 0
            m.RoyalPer = m.Royal *100/( m.PRICEA*m.qtyorg *(1 -  m.TrdDisc/100))
          ENDIF   
           *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

              
          IF m.SetDisc = 0 OR m.TrdDisc   = 0
            IF gfSEEK('2'+POSLN.PO+POSLN.STYLE,'CUTPICK')
              =gfSEEK('O'+CUTPICK.ORDER,'ORDHDR')
              IF m.TrdDisc   = 0
                m.TrdDisc   = ORDHDR.DISC
              ENDIF 
              IF m.SetDisc = 0
                lnSTDisc = 0
                laSTDisc[1,1] = 'NTERDISCR'
                laSTDisc[1,2] = 'lnSTDisc'
                =gfRltFld(ORDHDR.ctermcode,@laSTDisc,'CTERMCODE ')
                 m.SetDisc  =lnSTDisc
              ENDIF   
            ENDIF   
          ENDIF 

          m.Net = m.SPRICE *(1 - m.TrdDisc/100)*(1 - m.SetDisc/100) 
  
          FOR lnCount = 1 TO ALEN(laCostItem,1)
            lcCount = STR(lnCount,1)
            m.PERCNTOF&lcCount = SUBSTR(laCostItem[lnCount,1],ATC('-',laCostItem[lnCount,1])+1)        
            
            IF ASCAN(laRpPTarget,laCostItem[lnCount,1]) > 0
              m.llprTot&lcCount  = .T. 
            ELSE
              m.llprTot&lcCount  = .F. 
            ENDIF   
          ENDFOR
              
          SELECT (lcRepFile)
          APPEND BLANK 
          GATHER MEMO MEMVAR   
        ENDSCAN 
      ENDSCAN 
          
      STORE '' TO lcPUnMeth
        
      lcPMethod = gfGetExSin(@lcPUnMeth,POSHDR.CPRICECUR)
      lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
      lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
      
      lcDPUnMeth = ''
      lcDPMethod  = gfGetExSin(@lcDPUnMeth,POSHDR.CDUTYCUR)
      lcDPMethod  = IIF(EMPTY(lcDPMethod ),'*',lcDPMethod )
      lcDPUnMeth = IIF(EMPTY(lcDPUnMeth),'/',lcDPUnMeth)

          
      =gfSeek("I2"+SHPMTHDR.shipno,"BOMLINE")
      
       
      SELECT BOMLINE
      SCAN FOR LINENO <> 0
        lnNfCost1 = BOMLINE.UNITCOST 
        m.NfCost1  = BOMLINE.UNITCOST 
        IF SEEK(SHIPNo+STYLE+STR(LINENO,6),lcRepFile)    
          SELECT (lcRepFile)
          DO CASE
   		      CASE BOMLINE.CCATGTYP = 'P'
              
               *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
              *REPLACE POprice WITH lnNfCost1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT  ,;
                      NfCost1 WITH  BOMLINE.UNITCOST 
               REPLACE NfCost1 WITH  BOMLINE.UNITCOST
               
               *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
               REPLACE poPrice WITH NfCost1
               *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]

                       
               *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]        
                      

            CASE BOMLINE.CCATGTYP $ 'DM'
            
              lnPos = ASCAN(laCostItem,'*'+BOMLINE.MFGCODE+'*')
              IF lnPos > 0
                lnPos = ASUBSCRIPT(laCostItem,lnPos,1)
                IF BOMLINE.MFGCODE = '######'            && Check the case of more than one duty is entered
                  llCheck = .F.
                  lnFr = lnPos
                  FOR lnI = lnFr TO ALEN(laCostItem,1)
                    IF BOMLINE.MFGCODE = '######' .AND. BOMLINE.CBOMTYP = laCostItem[lnI,5]
                      lnPos = lnI
                	    EXIT
                    ENDIF 
                  ENDFOR
               	ENDIF
         
               	SELECT(lcRepFile)
               	lcI = ALLTRIM(STR(lnPos))
               	REPLACE COST&lcI.Cod WITH BOMLINE.MFGCODE  ;
                  	    COST&lcI     WITH BOMLINE.UNITCOST*(BOMLINE.UNITQTY * &lcRepFile..QtyOrg) ;
                      	COSTSTAT&lcI WITH BOMLINE.CCOSTSTAT,;
                       	UNTCST&lcI WITH IIF(QtyOrg>0,COST&lcI/QtyOrg,0),;
                       	COST&lcI.PER WITH BOMLINE.Npercent
                           
                REPLACE PERCNTOF&lcI WITH SUBSTR(laCostItem[lnPos,1],ATC('-',laCostItem[lnPos,1])+1)        
                

                
            
            
               IF POSHDr.CDUTYCUR <> oAriaApplication.BaseCurrency  
                 STORE '' TO lcUntSin                   
                 =gfSEEK('PP'+&lcRepFile..PO,'POSHDR')
                 lnDutyUnit  = IIF(POSHDr.nDCurUnit=0,1,POSHDr.nDCurUnit)
                 lnDutyRate  = IIF(POSHDr.nDutyRat=0,1,POSHDr.nDutyRat)
                 lcExSign = gfGetExSin(@lcUntSin,POSHDR.CDUTYCUR)
                 REPLACE COST&lcI WITH COST&lcI &lcExSign lnDutyRate &lcUntSin lnDutyUnit  
               ENDIF
             ELSE
               LOOP   
             ENDIF
             
            OTHERWISE 
              LOOP   
          ENDCASE
          
        
        ENDIF 
      ENDSCAN 
    ENDIF 
  ENDIF 
  ENDSCAN
ELSE

*AND POSLN.TRANCD = '3' 
  IF llStySelect 
    SELECT POSLN
    gfSetOrder('poslns')
    SELECT (lcCursorSty)
    SCAN FOR gfSeek(&lcCursorSty..Style,'Style') AND IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND ;
             IIF(llUseDiv  ,SEEK(STYLE.CDIVISION,lcDivFile ),.T.) AND gfSeek('0001'+&lcCursorSty..Style+'PP',"posln",'poslns')
      SELECT POSLN    
      SCAN REST WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+ STR(lineno,6)+ trancd ='0001'+&lcCursorSty..Style+'PP';
           FOR !EMPTY(SHIPNO) AND gfSeek("PP"+SHIPNO,"SHPMTHDR") AND IIF(!EMPTY(lcRpStatus),SHPMTHDR.Status $ lcRpStatus,.T.);
           AND NOT ISNULL(SHPMTHDR.ctemplate) AND IIF(llVendSelect  ,gfSeek("PP"+POSLN.PO,"POSHDR") AND SEEK(POSHDR.VENDOR,lcCursorVend),.T.)
           
           ***
           =gfSeek(SHPMTHDR.ctemplate,'shpcstln')
          IF EMPTY(laRpMTarget[1])  
            SELECT CCODE_NO+'-'+CDESCRIP, '*'+CCODE_NO+'*',0,CCATGTYP,CBOMTYP,.F.;
            FROM SHPCSTLN ;
            WHERE CTEMPLATE = PADR(lcTempValue,6);
            ORDER BY LINENO ;
            INTO ARRAY laCostItem

            IF !EMPTY(laCostItem)
              FOR lnI = 1 TO ALEN(laCostItem,1)
                IF laCostItem[lnI,4] = 'D'
                  laCostItem[lnI,2] = '*'+'######'+'*'
                ENDIF
              ENDFOR
            ENDIF  
          ELSE
            SELECT SHPCSTLN.CCODE_NO+'-'+SHPCSTLN.CDESCRIP, '*'+SHPCSTLN.CCODE_NO+'*',0,SHPCSTLN.CCATGTYP,SHPCSTLN.CBOMTYP,.F.;
            FROM SHPCSTLN WHERE CTEMPLATE = PADR(SHPMTHDR.ctemplate,6) AND ASCAN(laRpMTarget,SHPCSTLN.CCODE_NO) > 0;
            ORDER BY LINENO ;
            INTO ARRAY laCostItem
    
            IF !EMPTY(laCostItem)
              FOR lnI = 1 TO ALEN(laCostItem,1)
                IF laCostItem[lnI,4] = 'D'
                  laCostItem[lnI,2] = '*'+'######'+'*'
                ENDIF
              ENDFOR
            ENDIF  
          ENDIF   
  
          IF EMPTY(laCostItem[1])
            SELECT POSlN
            LOOP 
          ENDIF   
          
          =gfSeek("PP"+POSLN.PO,"POSHDR")  
          M.SHIPNO = SHPMTHDR.shipno
          m.Status = SHPMTHDR.Status
          m.NoCart = SHPMTHDR.cartons
          m.CVESSEL = SHPMTHDR.CVESSEL
          m.REFERENCE = SHPMTHDR.reference
          m.Container = SHPMTHDR.airwayb
          m.ETA = SHPMTHDR.ETA
          m.totqtyhdr = SHPMTHDR.totqtyhdr
          m.RecQty = SHPMTHDR.recv_can + SHPMTHDR.recv_dam+ SHPMTHDR.recv_stk
          m.DutyExRate  = POSHDR.NDUTYRAT
          =gfSeek(SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6),'SHPRLFLD')
          m.EnteredBy = SHPRLFLD.Cadd_user + "  " + DTOC(SHPRLFLD.dAdd_Date)
    
           *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
          m.DutyExRate = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
           *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

          SELECT SHPRLFLD
          SCAN REST while  shipno+ po+ STR(lineno,6) = SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6)
        
            m.PO = POSLN.PO
            m.CVenName = IIF(gfSeek(POSHDR.VENDOR,'APVENDOR'),apvendor.cvencomp ,"")
            m.Style = SUBSTR(POSLN.Style,1,lnStyMajor)
            m.Fstyle = POSLN.Style
            m.lineno = POSLN.lineno
            m.StyDesc = style.desc
            m.cClrDesc =  gfCodDes(SUBSTR(POSLN.Style , lnClrPosGL , lnClrLnGl),'COLOR')
            m.QtyOrg   =  POsln.TOTQTY
            m.POCurr =   POSHDR.cpricecur
            m.POprice = POSLN.NFCOST1
            m.NfCost1  = POSLN.NFCOST1
            m.PRICEA = IIF(SHPRLFLD.PRICEA = 0,STYLE.PRICEA,SHPRLFLD.PRICEA)
            
            m.Royal   = SHPRLFLD.NROYALTY
            IF m.PRICEA <> 0  AND m.QtyOrg <> 0
              m.RoyalPer = (m.Royal * 100) / (m.PRICEA*m.QtyOrg)
            ENDIF   
            
            M.NDUTYRAT   = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
            M.SoQty  = Style.TotOrd
            
            =gfSeek("PP"+POSLN.PO+'0001'+POSLN.Style+STR(POSLN.LINENo,6)+"1",'POSLN_A','POSLN')
          
            M.PoQty  = POSLN_A.totQTY
            
            PRIVATE laRoyalty,lnRylrate
            DECLARE laRoyalty[1,2]
            lnRylrate = 0
            laRoyalty[1,1] = 'NRYLRATE'
            laRoyalty[1,2] = 'lnRylrate'
            
            =gfRltFld(STYLE.ROYALTY,@laRoyalty,'ROYALTY   ')
            
            IF m.Royal     = 0
              m.RoyalPer = lnRylrate 
              m.Royal    = m.PRICEA*lnRylrate*m.QtyOrg/100  
            ENDIF 
              
            m.SPRICE   =  m.PRICEA
            m.TrdDisc  = SHPRLFLD.NTRDISC
            m.SetDisc  = SHPRLFLD.STLMNTDISC
            m.NPRICERAT = IIF(SHPRLFLD.NPRICERAT = 0 , POSHDr.NPRICERAT , SHPRLFLD.NPRICERAT)      
            m.NCURRUNIT = POSHDr.NCURRUNIT
                
            
            *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
            IF m.PRICEA <> 0  AND m.QtyOrg <> 0
              m.RoyalPer = m.Royal *100/( m.PRICEA*m.qtyorg *(1 -  m.TrdDisc/100))
            ENDIF   
             *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
    
                
            IF m.SetDisc = 0 OR m.TrdDisc   = 0
              IF gfSEEK('2'+POSLN.PO+POSLN.STYLE,'CUTPICK')
                =gfSEEK('O'+CUTPICK.ORDER,'ORDHDR')
                IF m.TrdDisc   = 0
                  m.TrdDisc   = ORDHDR.DISC
                ENDIF 
                IF m.SetDisc = 0
                  lnSTDisc = 0
                  laSTDisc[1,1] = 'NTERDISCR'
                  laSTDisc[1,2] = 'lnSTDisc'
                  =gfRltFld(ORDHDR.ctermcode,@laSTDisc,'CTERMCODE ')
                   m.SetDisc  =lnSTDisc
                ENDIF   
              ENDIF   
            ENDIF 

            m.Net = m.SPRICE *(1 - m.TrdDisc/100)*(1 - m.SetDisc/100) 
    
            FOR lnCount = 1 TO ALEN(laCostItem,1)
              lcCount = STR(lnCount,1)
              m.PERCNTOF&lcCount = SUBSTR(laCostItem[lnCount,1],ATC('-',laCostItem[lnCount,1])+1)        
              
              IF ASCAN(laRpPTarget,laCostItem[lnCount,1]) > 0
                m.llprTot&lcCount  = .T. 
              ELSE
                m.llprTot&lcCount  = .F. 
              ENDIF   
            ENDFOR
              
            SELECT (lcRepFile)
            APPEND BLANK 
            GATHER MEMO MEMVAR   
        ENDSCAN 
          
        STORE '' TO lcPUnMeth
        
        lcPMethod = gfGetExSin(@lcPUnMeth,POSHDR.CPRICECUR)
        lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
        lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
        
        lcDPUnMeth = ''
        lcDPMethod  = gfGetExSin(@lcDPUnMeth,POSHDR.CDUTYCUR)
        lcDPMethod  = IIF(EMPTY(lcDPMethod ),'*',lcDPMethod )
        lcDPUnMeth = IIF(EMPTY(lcDPUnMeth),'/',lcDPUnMeth)

          
        =gfSeek("I2"+SHPMTHDR.shipno+POSLN.PO+STR(POSLN.LINENo,6),"BOMLINE")
      
       
        SELECT BOMLINE
        SCAN FOR LINENO <> 0
          lnNfCost1 = BOMLINE.UNITCOST 
          m.NfCost1  = BOMLINE.UNITCOST 
          IF SEEK(SHIPNo+STYLE+STR(LINENO,6),lcRepFile)    
            SELECT (lcRepFile)
            DO CASE
               CASE BOMLINE.CCATGTYP = 'P'
                
                 *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
                *REPLACE POprice WITH lnNfCost1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT  ,;
                        NfCost1 WITH  BOMLINE.UNITCOST 
                REPLACE NfCost1 WITH  BOMLINE.UNITCOST
                
                *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
                REPLACE poPrice WITH NfCost1
                *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]

                                
                 *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
                        

              CASE BOMLINE.CCATGTYP $ 'DM'
              
                lnPos = ASCAN(laCostItem,'*'+BOMLINE.MFGCODE+'*')
                IF lnPos > 0
                  lnPos = ASUBSCRIPT(laCostItem,lnPos,1)
                  IF BOMLINE.MFGCODE = '######'            && Check the case of more than one duty is entered
                    llCheck = .F.
                    lnFr = lnPos
                    FOR lnI = lnFr TO ALEN(laCostItem,1)
                      IF BOMLINE.MFGCODE = '######' .AND. BOMLINE.CBOMTYP = laCostItem[lnI,5]
                        lnPos = lnI
                        EXIT
                      ENDIF 
                    ENDFOR
                   ENDIF
           
                   SELECT(lcRepFile)
                   lcI = ALLTRIM(STR(lnPos))
                   REPLACE COST&lcI.Cod WITH BOMLINE.MFGCODE  ;
                          COST&lcI     WITH BOMLINE.UNITCOST*(BOMLINE.UNITQTY * &lcRepFile..QtyOrg) ;
                          COSTSTAT&lcI WITH BOMLINE.CCOSTSTAT,;
                           UNTCST&lcI WITH IIF(QtyOrg>0,COST&lcI/QtyOrg,0),;
                           COST&lcI.PER WITH BOMLINE.Npercent
                             
                  REPLACE PERCNTOF&lcI WITH SUBSTR(laCostItem[lnPos,1],ATC('-',laCostItem[lnPos,1])+1)        
              
              
              
                 IF POSHDr.CDUTYCUR <> oAriaApplication.BaseCurrency                     
                   lcUntSin =''
                   =gfSEEK('PP'+&lcRepFile..PO,'POSHDR')
                   lnDutyUnit  = IIF(POSHDr.nDCurUnit=0,1,POSHDr.nDCurUnit)
                   lnDutyRate  = IIF(POSHDr.nDutyRat=0,1,POSHDr.nDutyRat)
                   lcExSign = gfGetExSin(@lcUntSin,POSHDR.CDUTYCUR)
                   REPLACE COST&lcI WITH COST&lcI &lcExSign lnDutyRate &lcUntSin lnDutyUnit  
                 ENDIF
               ELSE
                 LOOP   
               ENDIF
               
              OTHERWISE 
                LOOP   
            ENDCASE
          ENDIF 
        ENDSCAN 
      ENDSCAN   
    ENDSCAN
  ELSE
  *AND POSLN.TRANCD = '3' 
    IF llVendSelect  
      SELECT POSLN
      gfSetOrder('POSLN')
      SELECT (lcCursorVend)
      SCAN 
        IF gfSeek(&lcCursorVend..CVENDCODE +'PP','POSHDR','poshdrv')
          SELECT POSHDR
          SCAN REST WHILE vendor+ cbusdocu+ cstytype+ po = &lcCursorVend..CVENDCODE +'PP'
            IF gfSeek('PP'+POSHDR.PO,'POSLN','POSLN')
              SELECT POSlN
              SCAN REST WHILE  cbusdocu+ cstytype+ po+ cinvtype+ style+ STR(lineno,6)+ trancd = 'PP'+POSHDR.PO FOR ;
                 !EMPTY(SHIPNO) AND gfSeek("PP"+SHIPNO,"SHPMTHDR") AND ;
                 IIF(!EMPTY(lcRpStatus),SHPMTHDR.Status $ lcRpStatus,.T.) AND  NOT ISNULL(SHPMTHDR.ctemplate) AND ;
                 gfSeek(POSLN.Style,'Style') AND IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND ;
                 IIF(llUseDiv  ,SEEK(STYLE.CDIVISION,lcDivFile ),.T.)
                 
                 ***
                           ***
                   =gfSeek(SHPMTHDR.ctemplate,'shpcstln')
                  IF EMPTY(laRpMTarget[1])  
                    SELECT CCODE_NO+'-'+CDESCRIP, '*'+CCODE_NO+'*',0,CCATGTYP,CBOMTYP,.F.;
                    FROM SHPCSTLN ;
                    WHERE CTEMPLATE = PADR(lcTempValue,6);
                    ORDER BY LINENO ;
                    INTO ARRAY laCostItem

                    IF !EMPTY(laCostItem)
                      FOR lnI = 1 TO ALEN(laCostItem,1)
                        IF laCostItem[lnI,4] = 'D'
                          laCostItem[lnI,2] = '*'+'######'+'*'
                        ENDIF
                      ENDFOR
                    ENDIF  
                  ELSE
                    SELECT SHPCSTLN.CCODE_NO+'-'+SHPCSTLN.CDESCRIP, '*'+SHPCSTLN.CCODE_NO+'*',0,SHPCSTLN.CCATGTYP,SHPCSTLN.CBOMTYP,.F.;
                    FROM SHPCSTLN WHERE CTEMPLATE = PADR(SHPMTHDR.ctemplate,6) AND ASCAN(laRpMTarget,SHPCSTLN.CCODE_NO) > 0;
                    ORDER BY LINENO ;
                    INTO ARRAY laCostItem
            
                    IF !EMPTY(laCostItem)
                      FOR lnI = 1 TO ALEN(laCostItem,1)
                        IF laCostItem[lnI,4] = 'D'
                          laCostItem[lnI,2] = '*'+'######'+'*'
                        ENDIF
                      ENDFOR
                    ENDIF  
                  ENDIF   
          
                  IF EMPTY(laCostItem[1])
                    SELECT POSlN
                    LOOP 
                  ENDIF   
                  
                  =gfSeek("PP"+POSLN.PO,"POSHDR")  
                  M.SHIPNO = SHPMTHDR.shipno
                  m.Status = SHPMTHDR.Status
                  m.NoCart = SHPMTHDR.cartons
                  m.CVESSEL = SHPMTHDR.CVESSEL
                  m.REFERENCE = SHPMTHDR.reference
                  m.Container = SHPMTHDR.airwayb
                  m.ETA = SHPMTHDR.ETA
                  m.totqtyhdr = SHPMTHDR.totqtyhdr
                  m.RecQty = SHPMTHDR.recv_can + SHPMTHDR.recv_dam+ SHPMTHDR.recv_stk
                  m.DutyExRate  = POSHDR.NDUTYRAT
                  =gfSeek(SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6),'SHPRLFLD')
                  m.EnteredBy = SHPRLFLD.Cadd_user + "  " + DTOC(SHPRLFLD.dAdd_Date)
            
                  *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
			      m.DutyExRate = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
			      *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

                  SELECT SHPRLFLD
                  SCAN REST while  shipno+ po+ STR(lineno,6) = SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6)
                
                    m.PO = POSLN.PO
                    m.CVenName = IIF(gfSeek(POSHDR.VENDOR,'APVENDOR'),apvendor.cvencomp ,"")
                    m.Style = SUBSTR(POSLN.Style,1,lnStyMajor)
                    m.Fstyle = POSLN.Style
                    m.lineno = POSLN.lineno
                    m.StyDesc = style.desc
                    m.cClrDesc =  gfCodDes(SUBSTR(POSLN.Style , lnClrPosGL , lnClrLnGl),'COLOR')
                    m.QtyOrg   =  POsln.TOTQTY
                    m.POCurr =   POSHDR.cpricecur
                    m.POprice = POSLN.NFCOST1
                    m.NfCost1  = POSLN.NFCOST1
                    m.PRICEA = IIF(SHPRLFLD.PRICEA = 0,STYLE.PRICEA,SHPRLFLD.PRICEA)
                    
                    m.Royal   = SHPRLFLD.NROYALTY
                    IF m.PRICEA <> 0  AND m.QtyOrg <> 0
                      m.RoyalPer = (m.Royal * 100) / (m.PRICEA*m.QtyOrg)
                    ENDIF   
                    
                    M.NDUTYRAT   = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
                    M.SoQty  = Style.TotOrd
                    
                    =gfSeek("PP"+POSLN.PO+'0001'+POSLN.Style+STR(POSLN.LINENo,6)+"1",'POSLN_A','POSLN')
                  
                    M.PoQty  = POSLN_A.totQTY
                    
                    PRIVATE laRoyalty,lnRylrate
                    DECLARE laRoyalty[1,2]
                    lnRylrate = 0
                    laRoyalty[1,1] = 'NRYLRATE'
                    laRoyalty[1,2] = 'lnRylrate'
                    
                    =gfRltFld(STYLE.ROYALTY,@laRoyalty,'ROYALTY   ')
                    
                    IF m.Royal     = 0
                      m.RoyalPer = lnRylrate 
                      m.Royal    = m.PRICEA*lnRylrate*m.QtyOrg/100  
                    ENDIF 
                      
                    m.SPRICE   =  m.PRICEA
                    m.TrdDisc  = SHPRLFLD.NTRDISC
                    m.SetDisc  = SHPRLFLD.STLMNTDISC
                    m.NPRICERAT = IIF(SHPRLFLD.NPRICERAT = 0 , POSHDr.NPRICERAT , SHPRLFLD.NPRICERAT)      
                    m.NCURRUNIT = POSHDr.NCURRUNIT
                        
                    *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
		           IF m.PRICEA <> 0  AND m.QtyOrg <> 0
                     m.RoyalPer = m.Royal *100/( m.PRICEA*m.qtyorg *(1 -  m.TrdDisc/100))
			       ENDIF   
		            *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
    
                    IF m.SetDisc = 0 OR m.TrdDisc   = 0
                      IF gfSEEK('2'+POSLN.PO+POSLN.STYLE,'CUTPICK')
                        =gfSEEK('O'+CUTPICK.ORDER,'ORDHDR')
                        IF m.TrdDisc   = 0
                          m.TrdDisc   = ORDHDR.DISC
                        ENDIF 
                        IF m.SetDisc = 0
                          lnSTDisc = 0
                          laSTDisc[1,1] = 'NTERDISCR'
                          laSTDisc[1,2] = 'lnSTDisc'
                          =gfRltFld(ORDHDR.ctermcode,@laSTDisc,'CTERMCODE ')
                           m.SetDisc  =lnSTDisc
                        ENDIF   
                      ENDIF   
                    ENDIF 

                    m.Net = m.SPRICE *(1 - m.TrdDisc/100)*(1 - m.SetDisc/100) 
            
                    FOR lnCount = 1 TO ALEN(laCostItem,1)
                      lcCount = STR(lnCount,1)
                      m.PERCNTOF&lcCount = SUBSTR(laCostItem[lnCount,1],ATC('-',laCostItem[lnCount,1])+1)        
                      
                      IF ASCAN(laRpPTarget,laCostItem[lnCount,1]) > 0
                        m.llprTot&lcCount  = .T. 
                      ELSE
                        m.llprTot&lcCount  = .F. 
                      ENDIF   
                      
                      
                      
                    ENDFOR
                      
                    SELECT (lcRepFile)
                    APPEND BLANK 
                    GATHER MEMO MEMVAR   
                ENDSCAN 
                  
                STORE '' TO lcPUnMeth
                
                lcPMethod = gfGetExSin(@lcPUnMeth,POSHDR.CPRICECUR)
                lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
                lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
                
                lcDPUnMeth = ''
                lcDPMethod  = gfGetExSin(@lcDPUnMeth,POSHDR.CDUTYCUR)
                lcDPMethod  = IIF(EMPTY(lcDPMethod ),'*',lcDPMethod )
                lcDPUnMeth = IIF(EMPTY(lcDPUnMeth),'/',lcDPUnMeth)

                  
                =gfSeek("I2"+SHPMTHDR.shipno+POSLN.PO+STR(POSLN.LINENo,6),"BOMLINE")
              
               
                SELECT BOMLINE
                SCAN FOR LINENO <> 0
                  lnNfCost1 = BOMLINE.UNITCOST 
                  m.NfCost1  = BOMLINE.UNITCOST 
                  IF SEEK(SHIPNo+STYLE+STR(LINENO,6),lcRepFile)    
                    SELECT (lcRepFile)
                    DO CASE
                       CASE BOMLINE.CCATGTYP = 'P'
                      
                         *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
                        *REPLACE POprice WITH lnNfCost1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT  ,;
                                NfCost1 WITH  BOMLINE.UNITCOST 
                         REPLACE NfCost1 WITH  BOMLINE.UNITCOST
                         
                         *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
			             REPLACE poPrice WITH NfCost1
             			 *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]

                                         
                         *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
                                

                      CASE BOMLINE.CCATGTYP $ 'DM'
                      
                        lnPos = ASCAN(laCostItem,'*'+BOMLINE.MFGCODE+'*')
                        IF lnPos > 0
                          lnPos = ASUBSCRIPT(laCostItem,lnPos,1)
                          IF BOMLINE.MFGCODE = '######'            && Check the case of more than one duty is entered
                            llCheck = .F.
                            lnFr = lnPos
                            FOR lnI = lnFr TO ALEN(laCostItem,1)
                              IF BOMLINE.MFGCODE = '######' .AND. BOMLINE.CBOMTYP = laCostItem[lnI,5]
                                lnPos = lnI
                                EXIT
                              ENDIF 
                            ENDFOR
                           ENDIF
                   
                           SELECT(lcRepFile)
                           lcI = ALLTRIM(STR(lnPos))
                           REPLACE COST&lcI.Cod WITH BOMLINE.MFGCODE  ;
                                  COST&lcI     WITH BOMLINE.UNITCOST*(BOMLINE.UNITQTY * &lcRepFile..QtyOrg) ;
                                  COSTSTAT&lcI WITH BOMLINE.CCOSTSTAT,;
                                   UNTCST&lcI WITH IIF(QtyOrg>0,COST&lcI/QtyOrg,0),;
                                   COST&lcI.PER WITH BOMLINE.Npercent
                                     
                          REPLACE PERCNTOF&lcI WITH SUBSTR(laCostItem[lnPos,1],ATC('-',laCostItem[lnPos,1])+1)        
                      
                      
                         IF POSHDr.CDUTYCUR <> oAriaApplication.BaseCurrency                     
                           lcUntSin =''
                           =gfSEEK('PP'+&lcRepFile..PO,'POSHDR')
                           lnDutyUnit  = IIF(POSHDr.nDCurUnit=0,1,POSHDr.nDCurUnit)
                           lnDutyRate  = IIF(POSHDr.nDutyRat=0,1,POSHDr.nDutyRat)
                           lcExSign = gfGetExSin(@lcUntSin,POSHDR.CDUTYCUR)
                           REPLACE COST&lcI WITH COST&lcI &lcExSign lnDutyRate &lcUntSin lnDutyUnit  
                         ENDIF
                       ELSE
                         LOOP   
                       ENDIF
                       
                      OTHERWISE 
                        LOOP   
                    ENDCASE
                  ENDIF 
                ENDSCAN 
                         **
                 
                 
                 
              
              
              ENDSCAN 
            ENDIF 
          ENDSCAN 
        ENDIF 
      ENDSCAN 
    ELSE
    
*    POSLN.TRANCD = '3' AND
      SELECT POSLN
      gfSetOrder('poslnsh')
      IF gfSeek("PP","SHPMTHDR") 
        SELECT SHPMTHDR
        SCAN REST WHILE cbusdocu+ cshptype+ shipno = "PP" FOR IIF(!EMPTY(lcRpStatus),SHPMTHDR.Status $ lcRpStatus,.T.) and;
          NOT ISNULL(SHPMTHDR.ctemplate)
          
          IF gfSeek(SHPMTHDR.SHIPNO+"PP","POSLN")
            SELECT POSLN
            SCAN REST WHILE shipno+ cbusdocu+ cstytype+po+ cinvtype+ style+ STR(lineno,6)+ trancd = SHPMTHDR.SHIPNO+"PP" FOR ;
                 gfSeek(POSLN.Style,'Style') AND IIF(llUseSeason,SEEK(Style.Season,lcSeaFile),.T.) AND ;
                 IIF(llUseDiv  ,SEEK(STYLE.CDIVISION,lcDivFile ),.T.) 

                   =gfSeek(SHPMTHDR.ctemplate,'shpcstln')
                  IF EMPTY(laRpMTarget[1])  
                    SELECT CCODE_NO+'-'+CDESCRIP, '*'+CCODE_NO+'*',0,CCATGTYP,CBOMTYP,.F.;
                    FROM SHPCSTLN ;
                    WHERE CTEMPLATE = PADR(lcTempValue,6);
                    ORDER BY LINENO ;
                    INTO ARRAY laCostItem

                    IF !EMPTY(laCostItem)
                      FOR lnI = 1 TO ALEN(laCostItem,1)
                        IF laCostItem[lnI,4] = 'D'
                          laCostItem[lnI,2] = '*'+'######'+'*'
                        ENDIF
                      ENDFOR
                    ENDIF  
                  ELSE
                    SELECT SHPCSTLN.CCODE_NO+'-'+SHPCSTLN.CDESCRIP, '*'+SHPCSTLN.CCODE_NO+'*',0,SHPCSTLN.CCATGTYP,SHPCSTLN.CBOMTYP,.F.;
                    FROM SHPCSTLN WHERE CTEMPLATE = PADR(SHPMTHDR.ctemplate,6) AND ASCAN(laRpMTarget,SHPCSTLN.CCODE_NO) > 0;
                    ORDER BY LINENO ;
                    INTO ARRAY laCostItem
            
                    IF !EMPTY(laCostItem)
                      FOR lnI = 1 TO ALEN(laCostItem,1)
                        IF laCostItem[lnI,4] = 'D'
                          laCostItem[lnI,2] = '*'+'######'+'*'
                        ENDIF
                      ENDFOR
                    ENDIF  
                  ENDIF   
          
                  IF EMPTY(laCostItem[1])
                    SELECT POSlN
                    LOOP 
                  ENDIF   
                  
                  =gfSeek("PP"+POSLN.PO,"POSHDR")  
                  M.SHIPNO = SHPMTHDR.shipno
                  m.Status = SHPMTHDR.Status
                  m.NoCart = SHPMTHDR.cartons
                  m.CVESSEL = SHPMTHDR.CVESSEL
                  m.REFERENCE = SHPMTHDR.reference
                  m.Container = SHPMTHDR.airwayb
                  m.ETA = SHPMTHDR.ETA
                  m.totqtyhdr = SHPMTHDR.totqtyhdr
                  m.RecQty = SHPMTHDR.recv_can + SHPMTHDR.recv_dam+ SHPMTHDR.recv_stk
                  m.DutyExRate  = POSHDR.NDUTYRAT
                  =gfSeek(SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6),'SHPRLFLD')
                  m.EnteredBy = SHPRLFLD.Cadd_user + "  " + DTOC(SHPRLFLD.dAdd_Date)
            
                  *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
			      m.DutyExRate = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
  		          *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

                  SELECT SHPRLFLD
                  SCAN REST while  shipno+ po+ STR(lineno,6) = SHPMTHDR.shipno +POSLN.po+STR(POSLN.LINENo,6)
                
                    m.PO = POSLN.PO
                    m.CVenName = IIF(gfSeek(POSHDR.VENDOR,'APVENDOR'),apvendor.cvencomp ,"")
                    m.Style = SUBSTR(POSLN.Style,1,lnStyMajor)
                    m.Fstyle = POSLN.Style
                    m.lineno = POSLN.lineno
                    m.StyDesc = style.desc
                    m.cClrDesc =  gfCodDes(SUBSTR(POSLN.Style , lnClrPosGL , lnClrLnGl),'COLOR')
                    m.QtyOrg   =  POsln.TOTQTY
                    m.POCurr =   POSHDR.cpricecur
                    m.POprice = POSLN.NFCOST1
                    m.NfCost1  = POSLN.NFCOST1
                    m.PRICEA = IIF(SHPRLFLD.PRICEA = 0,STYLE.PRICEA,SHPRLFLD.PRICEA)
                    
                    m.Royal   = SHPRLFLD.NROYALTY
                    IF m.PRICEA <> 0  AND m.QtyOrg <> 0
                      m.RoyalPer = (m.Royal * 100) / (m.PRICEA*m.QtyOrg)
                    ENDIF   
                    
                    M.NDUTYRAT   = IIF(SHPRLFLD.NDUTYRAT = 0 , POSHDr.NDUTYRAT , SHPRLFLD.NDUTYRAT) 
                    M.SoQty  = Style.TotOrd
                    
                    =gfSeek("PP"+POSLN.PO+'0001'+POSLN.Style+STR(POSLN.LINENo,6)+"1",'POSLN_A','POSLN')
                  
                    M.PoQty  = POSLN_A.totQTY
                    
                    PRIVATE laRoyalty,lnRylrate
                    DECLARE laRoyalty[1,2]
                    lnRylrate = 0
                    laRoyalty[1,1] = 'NRYLRATE'
                    laRoyalty[1,2] = 'lnRylrate'
                    
                    =gfRltFld(STYLE.ROYALTY,@laRoyalty,'ROYALTY   ')
                    
                    IF m.Royal     = 0
                      m.RoyalPer = lnRylrate 
                      m.Royal    = m.PRICEA*lnRylrate*m.QtyOrg/100  
                    ENDIF 
                      
                    m.SPRICE   =  m.PRICEA
                    m.TrdDisc  = SHPRLFLD.NTRDISC
                    m.SetDisc  = SHPRLFLD.STLMNTDISC
                    m.NPRICERAT = IIF(SHPRLFLD.NPRICERAT = 0 , POSHDr.NPRICERAT , SHPRLFLD.NPRICERAT)      
                    m.NCURRUNIT = POSHDr.NCURRUNIT
                        
                    *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
			        IF m.PRICEA <> 0  AND m.QtyOrg <> 0
            		  m.RoyalPer = m.Royal *100/( m.PRICEA*m.qtyorg *(1 -  m.TrdDisc/100))
  			        ENDIF   
			        *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
 
                     
                        
                    IF m.SetDisc = 0 OR m.TrdDisc   = 0
                      IF gfSEEK('2'+POSLN.PO+POSLN.STYLE,'CUTPICK')
                        =gfSEEK('O'+CUTPICK.ORDER,'ORDHDR')
                        IF m.TrdDisc   = 0
                          m.TrdDisc   = ORDHDR.DISC
                        ENDIF 
                        IF m.SetDisc = 0
                          lnSTDisc = 0
                          laSTDisc[1,1] = 'NTERDISCR'
                          laSTDisc[1,2] = 'lnSTDisc'
                          =gfRltFld(ORDHDR.ctermcode,@laSTDisc,'CTERMCODE ')
                           m.SetDisc  =lnSTDisc
                        ENDIF   
                      ENDIF   
                    ENDIF 

                    m.Net = m.SPRICE *(1 - m.TrdDisc/100)*(1 - m.SetDisc/100) 
            
                    FOR lnCount = 1 TO ALEN(laCostItem,1)
                      lcCount = STR(lnCount,1)
                      m.PERCNTOF&lcCount = SUBSTR(laCostItem[lnCount,1],ATC('-',laCostItem[lnCount,1])+1)        
                      
                      IF ASCAN(laRpPTarget,laCostItem[lnCount,1]) > 0
                        m.llprTot&lcCount  = .T. 
                      ELSE
                        m.llprTot&lcCount  = .F. 
                      ENDIF   
                      
                      
                      
                    ENDFOR
                      
                    SELECT (lcRepFile)
                    APPEND BLANK 
                    GATHER MEMO MEMVAR   
                ENDSCAN 
                  
                STORE '' TO lcPUnMeth
                
                lcPMethod = gfGetExSin(@lcPUnMeth,POSHDR.CPRICECUR)
                lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
                lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
                
                lcDPUnMeth = ''
                lcDPMethod  = gfGetExSin(@lcDPUnMeth,POSHDR.CDUTYCUR)
                lcDPMethod  = IIF(EMPTY(lcDPMethod ),'*',lcDPMethod )
                lcDPUnMeth = IIF(EMPTY(lcDPUnMeth),'/',lcDPUnMeth)

                  
                =gfSeek("I2"+SHPMTHDR.shipno+POSLN.PO+STR(POSLN.LINENo,6),"BOMLINE")
              
               
                SELECT BOMLINE
                SCAN FOR LINENO <> 0
                  lnNfCost1 = BOMLINE.UNITCOST 
                  m.NfCost1  = BOMLINE.UNITCOST 
                  IF SEEK(SHIPNo+STYLE+STR(LINENO,6),lcRepFile)    
                    SELECT (lcRepFile)
                    DO CASE
                       CASE BOMLINE.CCATGTYP = 'P'
                        
                        *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
                        *REPLACE POprice WITH lnNfCost1 &lcPMethod NPRICERAT &lcPUnMeth NCURRUNIT  ,;
                                NfCost1 WITH  BOMLINE.UNITCOST 
                        REPLACE NfCost1 WITH  BOMLINE.UNITCOST
                        
                        
                        *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[START]
				        REPLACE poPrice WITH NfCost1
		                *B608510,3 MMT 05/19/2008 Convert Shipment Cost sheet program to ARIA4[End]

                                
                        *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
                                

                      CASE BOMLINE.CCATGTYP $ 'DM'
                      
                        lnPos = ASCAN(laCostItem,'*'+BOMLINE.MFGCODE+'*')
                        IF lnPos > 0
                          lnPos = ASUBSCRIPT(laCostItem,lnPos,1)
                          IF BOMLINE.MFGCODE = '######'            && Check the case of more than one duty is entered
                            llCheck = .F.
                            lnFr = lnPos
                            FOR lnI = lnFr TO ALEN(laCostItem,1)
                              IF BOMLINE.MFGCODE = '######' .AND. BOMLINE.CBOMTYP = laCostItem[lnI,5]
                                lnPos = lnI
                                EXIT
                              ENDIF 
                            ENDFOR
                           ENDIF
                   
                           SELECT(lcRepFile)
                           lcI = ALLTRIM(STR(lnPos))
                           REPLACE COST&lcI.Cod WITH BOMLINE.MFGCODE  ;
                                  COST&lcI     WITH BOMLINE.UNITCOST*(BOMLINE.UNITQTY * &lcRepFile..QtyOrg) ;
                                  COSTSTAT&lcI WITH BOMLINE.CCOSTSTAT,;
                                   UNTCST&lcI WITH IIF(QtyOrg>0,COST&lcI/QtyOrg,0),;
                                   COST&lcI.PER WITH BOMLINE.Npercent
                                     
                          REPLACE PERCNTOF&lcI WITH SUBSTR(laCostItem[lnPos,1],ATC('-',laCostItem[lnPos,1])+1)        
                      
                      
                         IF POSHDr.CDUTYCUR <> oAriaApplication.BaseCurrency                     
                           lcUntSin =''
                           =gfSEEK('PP'+&lcRepFile..PO,'POSHDR')
                           lnDutyUnit  = IIF(POSHDr.nDCurUnit=0,1,POSHDr.nDCurUnit)
                           lnDutyRate  = IIF(POSHDr.nDutyRat=0,1,POSHDr.nDutyRat)
                           lcExSign = gfGetExSin(@lcUntSin,POSHDR.CDUTYCUR)
                           REPLACE COST&lcI WITH COST&lcI &lcExSign lnDutyRate &lcUntSin lnDutyUnit  
                         ENDIF
                       ELSE
                         LOOP   
                       ENDIF
                       
                      OTHERWISE 
                        LOOP   
                    ENDCASE
                  ENDIF 
                ENDSCAN 
                         **

                  
            ENDSCAN       
          ENDIF 
        ENDSCAN 
      ENDIF 
    ENDIF 
  ENDIF
ENDIF



SELECT (lcRepFile)
SCAN 
  lnD = ASCAN(laCostItem,'*######*')
  IF lnD > 0
    =gfSeek('PP'+PO,'PoSHDR')
    STORE '' TO lcPUnMeth
    
  	lcPMethod = gfGetExSin(@lcPUnMeth,POSHDR.CPRICECUR)
  	lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
    
    lcDPUnMeth = ''
    lcDPMethod  = gfGetExSin(@lcDPUnMeth,POSHDR.CDUTYCUR)
    lcDPMethod  = IIF(EMPTY(lcDPMethod ),'*',lcDPMethod )
    lcDPUnMeth = IIF(EMPTY(lcDPUnMeth),'/',lcDPUnMeth)

 
    lnNDCURUNIT = POSHDR.NDCURUNIT 
    lnDutable = (NfCost1 &lcDPMethod NDUTYRAT &lcDPUnMeth lnNDCURUNIT )*QtyOrg 
  
    FOR lnJ = 1 TO ALEN(laCostItem,1)
      lcJ = ALLTRIM(STR(lnJ))
      
      =gfSeek(ShipNo + SUBSTR(laCostItem[lnJ ,1],1,ATC('-',laCostItem[lnJ ,1])-1),'shpmfgdt')
      
      IF COSTSTAT&lcJ = '1' AND IIF(laCostItem[lnJ ,2] <> '*######*',shpmfgdt.llduty,.T.)
        lnDutable = lnDutable + COST&lcJ
      ENDIF
    ENDFOR
    
    lnD = ASUBSCRIPT(laCostItem,lnD,1)
    lcD = ALLTRIM(STR(lnD))
    IF lnDutable>0
      REPLACE COST&lcD.PER WITH COST&lcD*100/lnDutable
      REPLACE PERCNTOF&lcD WITH SUBSTR(laCostItem[lnD,1],ATC('-',laCostItem[lnD,1])+1)                
    ENDIF
  ENDIF   
  
   *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  *lnSum = POprice *QtyOrg
  lnSum = (POprice/NPRICERAT) *QtyOrg
   *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
   
  FOR lnI = 1 TO ALEN(laCostItem,1)
    lcI = ALLTRIM(STR(lnI))
    IF COST&lcI > 0
 	   lnSum = lnSum + COST&lcI      
    ENDIF
  ENDFOR
  
  
  lnSum = lnSum + Royal
  
  lnTOTPrice = SPRICE*QtyOrg*(1 - TrdDisc/100)*(1 - SetDisc/100) 
  REPLACE Cost     WITH lnSum/QtyOrg ,;
		      Margin   WITH IIF(lnTOTPrice >0,100 - (lnSum/(lnTOTPrice))*100 ,0)
ENDSCAN 

lcCursorShip = ""
llShipSelect = .F.

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 01/13/2008
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CMFGCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
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
*! Name      : lfCreatTemp
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/13/2008
*! Purpose   : Function to Create Temp. files
*!*************************************************************
FUNCTION lfCreatTemp
DIMENSION laFileStruct[72,4]

laFileStruct[1,1] = "SHIPNO"
laFileStruct[1,2] = "C"
laFileStruct[1,3] = 6
laFileStruct[1,4] = 0

laFileStruct[2,1] = "Status"
laFileStruct[2,2] = "C"
laFileStruct[2,3] = 10
laFileStruct[2,4] = 0

laFileStruct[3,1] = "NoCart"
laFileStruct[3,2] = "N"
laFileStruct[3,3] = 5
laFileStruct[3,4] = 0

laFileStruct[4,1] = "CVESSEL"
laFileStruct[4,2] = "C"
laFileStruct[4,3] = 30
laFileStruct[4,4] = 0

laFileStruct[5,1] = "REFERENCE"
laFileStruct[5,2] = "C"
laFileStruct[5,3] = 30
laFileStruct[5,4] = 0

laFileStruct[6,1] = "Container"
laFileStruct[6,2] = "C"
laFileStruct[6,3] = 15
laFileStruct[6,4] = 0

laFileStruct[7,1] = "ETA"
laFileStruct[7,2] = "D"
laFileStruct[7,3] = 8
laFileStruct[7,4] = 0

laFileStruct[8,1] = "totqtyhdr"
laFileStruct[8,2] = "N"
laFileStruct[8,3] = 9
laFileStruct[8,4] = 0

laFileStruct[9,1] = "RecQty"
laFileStruct[9,2] = "N"
laFileStruct[9,3] = 10
laFileStruct[9,4] = 0

laFileStruct[10,1] = "DutyExRate"
laFileStruct[10,2] = "N"
laFileStruct[10,3] = 5
laFileStruct[10,4] = 2

laFileStruct[11,1] = "EnteredBy"
laFileStruct[11,2] = "C"
laFileStruct[11,3] = 40
laFileStruct[11,4] = 0


laFileStruct[12,1] = "PO"
laFileStruct[12,2] = "C"
laFileStruct[12,3] = 6
laFileStruct[12,4] = 0

laFileStruct[13,1] = "CVenName"
laFileStruct[13,2] = "C"
laFileStruct[13,3] = 30
laFileStruct[13,4] = 0


laFileStruct[14,1] = "Style"
laFileStruct[14,2] = "C"
laFileStruct[14,3] = 19
laFileStruct[14,4] = 0


laFileStruct[15,1] = "StyDesc"
laFileStruct[15,2] = "C"
laFileStruct[15,3] = 30
laFileStruct[15,4] = 0

laFileStruct[16,1] = "cClrDesc"
laFileStruct[16,2] = "C"
laFileStruct[16,3] = 30
laFileStruct[16,4] = 0

laFileStruct[17,1] = "QtyOrg"
laFileStruct[17,2] = "N"
laFileStruct[17,3] = 5
laFileStruct[17,4] = 0

laFileStruct[18,1] = "POCurr"
laFileStruct[18,2] = "C"
laFileStruct[18,3] = 3
laFileStruct[18,4] = 0

laFileStruct[19,1] = "POprice"
laFileStruct[19,2] = "N"
laFileStruct[19,3] = 7
laFileStruct[19,4] = 2


laFileStruct[20,1] = "Comm"
laFileStruct[20,2] = "N"
laFileStruct[20,3] = 5
laFileStruct[20,4] = 2

laFileStruct[21,1] = "Royal"
laFileStruct[21,2] = "N"

 *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*laFileStruct[21,3] = 5
laFileStruct[21,3] = 7
 *! B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

laFileStruct[21,4] = 2

laFileStruct[22,1] = "RoyalPer"
laFileStruct[22,2] = "N"
laFileStruct[22,3] = 5
laFileStruct[22,4] = 2

laFileStruct[23,1] = "SPRICE"
laFileStruct[23,2] = "N"
laFileStruct[23,3] = 7
laFileStruct[23,4] = 2

laFileStruct[24,1] = "TrdDisc"
laFileStruct[24,2] = "N"
laFileStruct[24,3] = 5
laFileStruct[24,4] = 2


laFileStruct[25,1] = "SetDisc"
laFileStruct[25,2] = "N"
laFileStruct[25,3] = 5
laFileStruct[25,4] = 2

laFileStruct[26,1] = "Net"
laFileStruct[26,2] = "N"
laFileStruct[26,3] = 7
laFileStruct[26,4] = 2

laFileStruct[27,1] = "Cost"
laFileStruct[27,2] = "N"
laFileStruct[27,3] =7
laFileStruct[27,4] = 2

laFileStruct[28,1] = "Margin"
laFileStruct[28,2] = "N"
laFileStruct[28,3] = 7
laFileStruct[28,4] = 2

laFileStruct[29,1] = "TotalFr"
laFileStruct[29,2] = "N"
laFileStruct[29,3] = 8
laFileStruct[29,4] = 2

laFileStruct[30,1] = "SoQty"
laFileStruct[30,2] = "N"
laFileStruct[30,3] = 6
laFileStruct[30,4] = 0

laFileStruct[31,1] = "PoQty"
laFileStruct[31,2] = "N"
laFileStruct[31,3] = 6
laFileStruct[31,4] = 0

laFileStruct[32,1] = "LINENO"
laFileStruct[32,2] = "N"
laFileStruct[32,3] = 5
laFileStruct[32,4] = 0

laFileStruct[33,1] = "NPRICERAT"
laFileStruct[33,2] = "N"
laFileStruct[33,3] = 5
laFileStruct[33,4] = 2

laFileStruct[34,1] = 'NCURRUNIT'
laFileStruct[34,2] = 'N'
laFileStruct[34,3] = 4
laFileStruct[34,4] = 0


laFileStruct[35,1] = 'NDUTYRAT'
laFileStruct[35,2] = 'N'
laFileStruct[35,3] = 9
laFileStruct[35,4] = 4

laFileStruct[36,1] = 'Fstyle'
laFileStruct[36,2] = 'C'
laFileStruct[36,3] = 19
laFileStruct[36,4] = 0

laFileStruct[37,1] = "NfCost1"
laFileStruct[37,2] = "N"
laFileStruct[37,3] = 7
laFileStruct[37,4] = 2




FOR lnI = 1 TO 5
  lcI = ALLTRIM(STR(lnI))
  lnLn = IIF(lnI=1, 38,lnLn +1)
  laFileStruct[lnLn,1] = 'COST' + lcI
  laFileStruct[lnLn,2] = 'N'
  laFileStruct[lnLn,3] = 10
  laFileStruct[lnLn,4] = 4

  lnLn = lnLn + 1
  laFileStruct[lnLn,1] = 'COST' + lcI + 'COD'
  laFileStruct[lnLn,2] = 'C'
  laFileStruct[lnLn,3] = 6
  laFileStruct[lnLn,4] = 0

  lnLn = lnLn + 1
  laFileStruct[lnLn,1] = 'COST' + lcI + 'PER'
  laFileStruct[lnLn,2] = 'N' 
  laFileStruct[lnLn,3] = 6
  laFileStruct[lnLn,4] = 2
    
  lnLn = lnLn + 1
  laFileStruct[lnLn,1] = 'COSTSTAT' + lcI
  laFileStruct[lnLn,2] = 'C'
  laFileStruct[lnLn,3] = 1
  laFileStruct[lnLn,4] = 0

  lnLn = lnLn + 1
  laFileStruct[lnLn,1] = 'PERCNTOF' + lcI
  laFileStruct[lnLn,2] = 'C'
  laFileStruct[lnLn,3] = 10
  laFileStruct[lnLn,4] = 0
    
  lnLn = lnLn + 1
  laFileStruct[lnLn,1] = 'UNTCST' + lcI
  laFileStruct[lnLn,2] = 'N'
  laFileStruct[lnLn,3] = 10
  laFileStruct[lnLn,4] = 4


  lnLn = lnLn + 1
  laFileStruct[lnLn,1] = 'llprTot' + lcI
  laFileStruct[lnLn,2] = 'L'
  laFileStruct[lnLn,3] = 1
  laFileStruct[lnLn,4] = 0
ENDFOR



gfCrtTmp(lcRepFile   ,@laFileStruct,"SHIPNO+Fstyle+STR(LINENO,6)",lcRepFile,.F.)

*!*************************************************************
*! Name      : lfvMfgOp
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/13/2008
*! Purpose   : Function to open MFG mover
*!*************************************************************
FUNCTION lfvMfgOp
gfMover(@laRpMSource,@laRpMTarget,LANG_MFG_OP,.T.,"",.T.,.F.) && call mover function.

*!*************************************************************
*! Name      : lfFillMfg
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/13/2008
*! Purpose   : Function to fill bins arrays
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
FUNCTION lfFillMfg
IF !USED('CODES_A')
  = gfOpenTable('CODES' ,'CCODE_NO','SH','CODES_A')
ENDIF  
 
DIMENSION  laRpMSource[1,1]
DIMENSION  laRpMTarget[1,1]
PRIVATE lnCnt,lcCnt,lnElmnts

DIMENSION laTarget[1],laRpMSource[1]
STORE '' TO laRpMSource,laTarget

STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7
=gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7',oAriaApplication.ActiveCompanyID)

lnElmnts = 0
FOR lnCnt = 1 TO 7
  lcCnt = STR(lnCnt,1)
  IF M_CITYPE&lcCnt = 'D'
    lnElmnts = lnElmnts + 1
    DIMENSION laRpMSource[lnElmnts]
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
*!*	    laRpMSource[lnElmnts] = ;
*!*	              PADR(M_CISLBL&lcCnt,6,' ') + '-' + ;
*!*	              PADR(M_CISLBL&lcCnt, 30) + ;
*!*	              'D'+STR(lnCnt,1)
    laRpMSource[lnElmnts] = ;
              PADR(M_CISLBL&lcCnt,6,' ') + '-' + ;
              PADR(M_CISLBL&lcCnt, 60) + ;
              'D'+STR(lnCnt,1)
   *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
  ENDIF
ENDFOR

SELECT CODES_A
IF gfSEEK('N'+'MFGCODE','CODES_A')
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N' + 'MFGCODE' ;
            FOR CRLTFIELD = 'N'
    lnElmnts = lnElmnts + 1
    DIMENSION laRpMSource[lnElmnts]
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
    *laRpMSource[lnElmnts] = PADR(cCode_No,6,' ') + '-' + PADR(cDiscrep,30) + 'M '
    laRpMSource[lnElmnts] = PADR(cCode_No,6,' ') + '-' + PADR(cDiscrep,60) + 'M '
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][ENd]
  ENDSCAN
ENDIF  



*!*************************************************************
*! Name      : lfPrntMfg
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/13/2008
*! Purpose   : Function to fill bins arrays
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
FUNCTION lfPrntMfg
IF !USED('CODES_A')
  = gfOpenTable('CODES' ,'CCODE_NO','SH','CODES_A')
ENDIF  
 
DIMENSION  laRpPTarget[1,1]
DIMENSION  laRpPSource[1,1]

PRIVATE lnCnt,lcCnt,lnElmnts

DIMENSION laTarget[1],laRpPSource[1]
STORE '' TO laRpPSource,laTarget

STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7
=gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7',oAriaApplication.ActiveCompanyID)

lnElmnts = 0
FOR lnCnt = 1 TO 7
  lcCnt = STR(lnCnt,1)
  IF M_CITYPE&lcCnt = 'D'
    lnElmnts = lnElmnts + 1
    DIMENSION laRpPSource[lnElmnts]
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
*!*	    laRpPSource[lnElmnts] = ;
*!*	              PADR(M_CISLBL&lcCnt,6,' ') + '-' + ;
*!*	              PADR(M_CISLBL&lcCnt, 30) + ;
*!*	              'D'+STR(lnCnt,1)
    laRpPSource[lnElmnts] = ;
              PADR(M_CISLBL&lcCnt,6,' ') + '-' + ;
              PADR(M_CISLBL&lcCnt, 60) + ;
              'D'+STR(lnCnt,1)
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
  ENDIF
ENDFOR

SELECT CODES_A
IF gfSEEK('N'+'MFGCODE','CODES_A')
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N' + 'MFGCODE' ;
            FOR CRLTFIELD = 'N'
    lnElmnts = lnElmnts + 1
    DIMENSION laRpPSource[lnElmnts]
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
    *laRpPSource[lnElmnts] = PADR(cCode_No,6,' ') + '-' + PADR(cDiscrep,30) + 'M '
    laRpPSource[lnElmnts] = PADR(cCode_No,6,' ') + '-' + PADR(cDiscrep,60) + 'M '
    *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
  ENDSCAN
ENDIF  

*!*************************************************************
*! Name      : lfvPrntot
*! Developer : Mariam Mazhar [MMT]
*! Date      : 01/13/2008
*! Purpose   : Function to open MFG mover
*!*************************************************************
FUNCTION lfvPrntot
gfMover(@laRpPSource,@laRpPTarget,LANG_MFG_OP,.T.,"",.T.,.F.) && call mover function.

*!*************************************************************
*! Name      : RefreshStatusP
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 01/13/2008
*! Purpose   : Return the selected MFG in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatusP
LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpPTarget)
    FOR lnTarget = 1 TO ALEN(laRpPTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpPTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
