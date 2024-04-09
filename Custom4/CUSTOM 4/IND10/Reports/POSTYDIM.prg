*:***************************************************************************
*: Program file  : POSTYDIM
*: Program desc. : Custom report to have DIMENSIONS/CUBE per STYLE 
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC),Purchase order [PO]
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/03/2009 
*! Entry No.     : C201113 - [T20081202.0021]
*!***************************************************************************
*: Modifications:
*:C201113,2 MMT 03/17/2009 Convert Transcation type option to Mover[T20081202.0021]
*!***************************************************************************

IF loOgScroll.llOGFltCh 
  WAIT WINDOW 'Collecting data....' NOWAIT
  lfCreatTemp()
  lfCollectData()
ELSE
  IF FILE(oAriaApplication.WorkDir+lcRpHdrTmp+'.DBF')
    USE oAriaApplication.WorkDir+lcRpHdrTmp+'.DBF' IN 0
  ENDIF   
ENDIF 

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRpHdrTmp+'.DBF' 


IF USED(lcRpHdrTmp)
  SELECT (lcRpHdrTmp)
  LOCATE 
  IF EOF()
    = gfModalGen('TRM00052B40011','ALERT')
    RETURN .F.
  ENDIF 
  USE IN (lcRpHdrTmp)
ENDIF   

DIMENSION loOGScroll.laCRParams[6,2]
loOGScroll.laCRParams[1,1] = 'Layout'

IF lcRPFormat = 'D' && if details report
  loogScroll.cCROrientation = 'L'
  loOGScroll.laCRParams[1,2] = 'Detail'
  loOgScroll.lcOGLastForm = "POSTYDID"
ELSE &&summary report
  loogScroll.cCROrientation = 'P'
  loOGScroll.laCRParams[1,2] = 'Summary'
  loOgScroll.lcOGLastForm = "POSTYDIS"
ENDIF

loOGScroll.laCRParams[2,1] = 'ReportName'
loOGScroll.laCRParams[2,2] = 'Style PO Dimensions Summary Report' 
loOGScroll.laCRParams[3,1] = 'SortBy'
loOGScroll.laCRParams[3,2] = lcRPSortBy

loOGScroll.laCRParams[4,1] = 'PrnDec'
loOGScroll.laCRParams[4,2] = lnRpPrtdec

loOGScroll.laCRParams[5,1] = 'lcStyleT'
loOGScroll.laCRParams[5,2] = gfItemMask('H')

lcTypes = ''
IF TYPE("laRpTarget[1,1]") <> 'U' AND !EMPTY(laRpTarget[1,1])
  FOR lnC = 1 TO ALEN(laRpTarget,1)
    lcTypes = lcTypes + STR(laRpTarget[lnC,2],1)
  ENDFOR 
ELSE
 lcTypes = '123'
ENDIF   

loOGScroll.laCRParams[6,1] = 'lcTypes'
loOGScroll.laCRParams[6,2] = lcTypes
*loOGScroll.laCRParams[6,2] = gfItemMask('H')



= gfDispRe( loOgScroll.lcOGLastForm )

*************************************************************
*! Name      : lfMajInfGet
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
*! Purpose   : To get the title and picture of style major segement 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajInfGet()
*!*************************************************************

FUNCTION lfMajGet

lcMajPic = gfItemMask("PM")
lnMajPic = LEN(lcMajPic)
lcMajPic = "@! " + lcMajPic
lcMajTtl = gfItemMask("HM")

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
*! Purpose   : When Function of report
*!*************************************************************
FUNCTION lfwRepWhen
=gfOpenTable('Style','Style')
=gfOpenTable('POSHDR','POSHDR')
=gfOpenTable('POSLN','POSLN')
=gfOpenTable('StyInvJl','STYINVJL')
*:C201113,2 MMT 03/17/2009 Convert Transcation type option to Mover[Start]
DIMENSION  laRpSource[3,2]
DIMENSION  laRpTarget[1,1]
STORE '' TO laRpTarget
laRpSource[1,1] = 'Ordered'
laRpSource[1,2] = 1
laRpSource[2,1] = 'Received'
laRpSource[2,2] = 2
laRpSource[3,1] = 'In Transit'
laRpSource[3,2] = 3
*:C201113,2 MMT 03/17/2009 Convert Transcation type option to Mover[End]

*!*************************************************************
*! Name      : lfSortDumy
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
*! Purpose   : Validate sortby
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

FUNCTION lfSortDumy
PARAMETERS lcPar
DIMENSION laSortItem[3,1], laSortVal[3,1]
laSortItem[1]= 'PO Number'  
laSortItem[2]= 'Style' 
laSortItem[3]= 'Vendor'
laSortVal[1] = 'P'
laSortVal[2] = 'S'
laSortVal[3] = 'V'



*!*************************************************************
*! Name      : lfSRVPo
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
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
*! Example   : =lfSRVPo()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVPo
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO Vencode IN APVENDOR
    SELECT POSHDR
    SET ORDER TO TAG POSHDR
    SET RELATION TO VENDOR INTO APVENDOR
    GO TOP IN POSHDR

  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO 
ENDCASE
*-- end of lfSRVPo
*!*************************************************************
*! Name      : lfsrvSty
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
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
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfCreatTemp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
*! Purpose   : Create Temp For report data collection
*!*************************************************************
FUNCTION lfCreatTemp
DIMENSION laFileStruc[10,4]
laFileStruc[1,1]= 'Style'
laFileStruc[1,2]= 'C'
laFileStruc[1,3]= 19
laFileStruc[1,4]= 0

laFileStruc[2,1]= 'nmspackqty'
laFileStruc[2,2]= 'N'
laFileStruc[2,3]= 6
laFileStruc[2,4]= 0

laFileStruc[3,1]= 'nmspackhgt'
laFileStruc[3,2]= 'N'
laFileStruc[3,3]= 6
laFileStruc[3,4]= 2

laFileStruc[4,1]= 'nmspackwdt'
laFileStruc[4,2]= 'N'
laFileStruc[4,3]= 6
laFileStruc[4,4]= 2

laFileStruc[5,1]= 'nmspacklen'
laFileStruc[5,2]= 'N'
laFileStruc[5,3]= 6
laFileStruc[5,4]= 2

laFileStruc[6,1]= 'nmspackwgt'
laFileStruc[6,2]= 'N'
laFileStruc[6,3]= 6
laFileStruc[6,4]= 2

laFileStruc[7,1]= 'TYPE'
laFileStruc[7,2]= 'C'
laFileStruc[7,3]= 1
laFileStruc[7,4]= 0

laFileStruc[8,1]= 'cvendcode'
laFileStruc[8,2]= 'C'
laFileStruc[8,3]= 8
laFileStruc[8,4]= 0

laFileStruc[9,1]= 'PO'
laFileStruc[9,2]= 'C'
laFileStruc[9,3]= 6
laFileStruc[9,4]= 0

laFileStruc[10,1]= 'TOTQTY'
laFileStruc[10,2]= 'N'
laFileStruc[10,3]= 9
laFileStruc[10,4]= 0

=gfCrtTmp(lcRpHdrTmp,@laFileStruc,'Style+PO',lcRpHdrTmp,.F.)

*!*************************************************************
*! Name      : lfCollectData
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
*! Purpose   : report data collection
*!*************************************************************
FUNCTION lfCollectData

*Selected Vendor
llVendSelected = .F.
lnPosVen = ASCAN(loOgScroll.laOgFXFlt,'POSHDR.VENDOR')
IF lnPosVen  > 0 
  lnPosVen = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVen ,1)
  lcVendSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosVen ,6]),loOGScroll.laOgFxFlt[lnPosVen ,6],'')
  IF !EMPTY(lcVendSel ) AND USED(lcVendSel )
    SELECT(lcVendSel )
    LOCATE
    IF !EOF()
      llVendSelected = .T.
    ENDIF 
  ENDIF 
ENDIF     

*Selected Style
llStySelected = .F.
lnPosSty = ASCAN(loOgScroll.laOgFXFlt,'STYLE.CSTYMAJOR')
IF lnPosSty > 0 
  lnPosSty = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSty,1)
  lcStySel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosSty,6]),loOGScroll.laOgFxFlt[lnPosSty,6],'')
  IF !EMPTY(lcStySel) AND USED(lcStySel)
    SELECT(lcStySel)
    LOCATE
    IF !EOF()
      llStySelected = .T.
    ENDIF 
  ENDIF 
ENDIF     

*Selected PO
llPOSelected = .F.
lnPosPO = ASCAN(loOgScroll.laOgFXFlt,'POSHDR.PO')
IF lnPosPO > 0 
  lnPosPO = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPO,1)
  lcPOSel =IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosPO,6]),loOGScroll.laOgFxFlt[lnPosPO,6],'')
  IF !EMPTY(lcPOSel ) AND USED(lcPOSel )
    SELECT(lcPOSel )
    LOCATE
    IF !EOF()
      llPOSelected = .T.
    ENDIF 
  ENDIF 
ENDIF 

* Style Group
llUseGrp  = .F.
lcGrpFile = ''
lnGrpPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYGROUP")
IF lnGrpPos  > 0 
  lnGrpPos  = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnGrpPos ,1)
  lcGrpSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnGrpPos ,6]),loOgScroll.laOgFXFlt[lnGrpPos ,6],'')
  IF !EMPTY(lcGrpSel) 
    lcGrpFile = loOGScroll.gfTempName()
    llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
  ENDIF   
ENDIF       

*Date Received
llRecDate = .F.
ldRecSDate = {}
ldRecEDate = {}
lnRecPos = ASCAN(loOgScroll.laOgFXFlt,"POSLN.COMPLETE")
IF lnRecPos > 0 
  lnRecPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnRecPos ,1)
  lcRecSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnRecPos ,6]),loOgScroll.laOgFXFlt[lnRecPos ,6],'')
  IF !EMPTY(lcRecSel)
	ldRecSDate = CTOD(SUBSTR(lcRecSel,1,10))
	ldRecEDate = CTOD(SUBSTR(lcRecSel,12,21))
	llRecDate = .T.
  ENDIF 
ENDIF 


*Date Completed
llCompDate = .F.
ldCmpSDate = {}
ldCmpEDate = {}
lnCmpPos = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.COMPLETE")
IF lnCmpPos > 0 
  lnCmpPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnCmpPos ,1)
  lcCmpSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnCmpPos ,6]),loOgScroll.laOgFXFlt[lnCmpPos ,6],'')
  IF !EMPTY(lcCmpSel )
	ldCmpSDate = CTOD(SUBSTR(lcCmpSel ,1,10))
	ldCmpEDate = CTOD(SUBSTR(lcCmpSel ,12,21))
	llCompDate = .T.
  ENDIF 
ENDIF 


*Status
llStatus = .F.
lcStatus = ''
lnStPos = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.STATUS")
IF lnStPos > 0 
  lnStPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnStPos ,1)
  lcStatus =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnStPos ,6]),loOgScroll.laOgFXFlt[lnStPos ,6],'')
  IF !EMPTY(lcStatus)
	llStatus = .T.
  ENDIF 
ENDIF 


*Entered Completed
llEntDate = .F.
ldEntSDate = {}
ldEntEDate = {}
lnEntPos = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.ENTERED")
IF lnEntPos  > 0 
  lnEntPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnEntPos ,1)
  lcEntSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnEntPos ,6]),loOgScroll.laOgFXFlt[lnEntPos ,6],'')
  IF !EMPTY(lcEntSel)
	ldEntSDate = CTOD(SUBSTR(lcEntSel ,1,10))
	ldEntEDate = CTOD(SUBSTR(lcEntSel ,12,21))
	llEntDate = .T.
  ENDIF 
ENDIF 

*:C201113,2 MMT 03/17/2009 Convert Transcation type option to Mover[Start]
lcTranTyp =""
IF TYPE("laRpTarget[1,1]") <> 'U' AND !EMPTY(laRpTarget[1,1])
  FOR lnC = 1 TO ALEN(laRpTarget,1)
    lcTranTyp = lcTranTyp + STR(laRpTarget[lnC,2],1)
  ENDFOR 
ELSE
 lcTranTyp = '123'
ENDIF   
*:C201113,2 MMT 03/17/2009 Convert Transcation type option to Mover[End]


DO CASE 
  CASE llStySelected
    SELECT Posln
    =gfSetOrder('POSLNS')
    SELECT (lcStySel)
    LOCATE 
    SCAN 
      =gfSeek(ALLTRIM(&lcStySel..cStyMajor),'Style')
      SELECT Style
      SCAN REST WHILE Style = ALLTRIM(&lcStySel..cStyMajor) FOR IIF(llUseGrp,SEEK(Style.CstyGroup,lcGrpFile),.T.)
        WAIT WINDOW 'Collecting data for style '+ ALLTRIM(Style.Style)  NOWAIT
        m.Style = Style.Style
        m.nmspackqty = Style.nmspackqty 
        m.nmspackhgt = Style.nmspackhgt
        m.nmspackwdt = Style.nmspackwdt
        m.nmspacklen = Style.nmspacklen
        m.nmspackwgt = Style.nmspackwgt 
        SELECT Posln
        =gfSqlRun("Select POSLN.*,POSHDR.VENDOR,POSHDR.Status,"+;
        		   "POSHDR.Entered As Ent_date,"+;
        		   "poshdr.Complete As Cmp_date From POSLN INNER JOIN POSHDR ON POSHDR.CSTYTYPE ="+;
        		   "POSLN.CSTYTYPE AND POSHDR.CBUSDOCU = POSLN.CBUSDOCU AND "+;
        		   "POSHDR.PO  = POSLN.PO WHERE POSLN.CINVTYPE ='0001' AND "+;
        		   "POSLN.STYLE = '"+STYLE.STYLE+"' AND POSLN.CSTYTYPE = 'P' and POSLN.CBUSDOCU= 'P'",'POSLN')
        SELECT Posln
        LOCATE 
        SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = '0001'+Style.Style+'PP' FOR ;
           IIF(llPOSelected,SEEK(POSLN.PO,lcPOSel),.T.) AND ;
           POSLN.TRANCD $ lcTranTyp ;
           AND IIF(llVendSelected,SEEK(POSLN.VENDOR,lcVendSel ),.T.) AND IIF(llStatus ,POSLN.STatus $ lcStatus ,.T.) AND ;
           IIF(llEntDate ,BETWEEN(POSLN.Ent_date,ldEntSDate ,ldEntEDate ),.T.) AND ;
           IIF(llCompDate ,BETWEEN(POSLN.Cmp_date ,ldCmpSDate ,ldCmpEDate ),.T.) 
           
           IF POSLN.TRANCD = '2' AND llRecDate 
             =gfSeek(Style.Style+POSLN.CWARECODE+POSLN.CRSESSION ,'STYINVJL')
             IF !BETWEEN(STYINVJL.DTRDATE,ldRecSDate ,ldRecEDate )
               LOOP 
             ENDIF 
           ENDIF
           m.TYPE =  POSLN.TRANCD 
           m.cvendcode = POSLN.VENDOR
		   m.PO = POSLN.PO
		   m.TotQty = POSLN.TOTQTY
		   INSERT INTO (lcRpHdrTmp) FROM MEMVAR 
        ENDSCAN 
      ENDSCAN 
    ENDSCAN 
  CASE !llStySelected  AND  llPOSelected 
    SELECT Posln
    =gfSetOrder('POSLN')
    SELECT (lcPOSel)
    SCAN 
      =gfSeek('PP'+&lcPOSel..PO,'POSHDR')
      IF llStatus AND !(POSHDR.STatus $ lcStatus) 
        LOOP 
      ENDIF 
      IF llVendSelected AND !SEEK(POSHDR.VENDOR,lcVendSel)
        LOOP 
      ENDIF 
      IF llEntDate AND !BETWEEN(POSHDR.ENTERED,ldEntSDate ,ldEntEDate )
        LOOP 
      ENDIF 
      IF llCompDate AND !BETWEEN(POSHDR.Complete ,ldCmpSDate ,ldCmpEDate)
        LOOP 
      ENDIF 
      =gfSeek('PP'+&lcPOSel..PO,'POSLN')
      SELECT POSLN
      SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD ='PP'+&lcPOSel..PO ;
      			FOR POSLN.TRANCD $ lcTranTyp AND gfSeek(POSLN.STYLE,'STYLE','STYLE') AND IIF(llUseGrp,SEEK(Style.CstyGroup,lcGrpFile),.T.)
      				                                                               
      
      	 IF POSLN.TRANCD = '2' AND llRecDate 
           =gfSeek(Style.Style+POSLN.CWARECODE+POSLN.CRSESSION ,'STYINVJL')
           IF !BETWEEN(STYINVJL.DTRDATE,ldRecSDate ,ldRecEDate )
             LOOP 
           ENDIF 
         ENDIF
         WAIT WINDOW 'Collecting data for style '+ ALLTRIM(Style.Style)  NOWAIT
         m.Style = Style.Style
         m.nmspackqty = Style.nmspackqty 
         m.nmspackhgt = Style.nmspackhgt
         m.nmspackwdt = Style.nmspackwdt
         m.nmspacklen = Style.nmspacklen
         m.nmspackwgt = Style.nmspackwgt 
         m.TYPE = POSLN.TRANCD 
         m.cvendcode = POSLN.VENDOR
		 m.PO = POSLN.PO
		 m.TotQty = POSLN.TOTQTY
		 INSERT INTO (lcRpHdrTmp) FROM MEMVAR 
      ENDSCAN 
    ENDSCAN 
  OTHERWISE 
    SELECT Posln
    =gfSetOrder('POSLN')
    =gfSeek('PP','POSHDR')
    SELECT POSHDR 
    SCAN 
      IF llStatus AND !(POSHDR.STatus $ lcStatus) 
        LOOP 
      ENDIF 
      IF llVendSelected AND !SEEK(POSHDR.VENDOR,lcVendSel)
        LOOP 
      ENDIF 
      IF llEntDate AND !BETWEEN(POSHDR.ENTERED,ldEntSDate ,ldEntEDate )
        LOOP 
      ENDIF 
      IF llCompDate AND !BETWEEN(POSHDR.Complete ,ldCmpSDate ,ldCmpEDate)
        LOOP 
      ENDIF 
      =gfSeek('PP'+POSHDR.PO,'POSLN')
      SELECT POSLN
      SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD ='PP'+POSHDR.PO ;
      			FOR POSLN.TRANCD $ lcTranTyp AND gfSeek(POSLN.STYLE,'STYLE','STYLE');
      			AND IIF(llUseGrp,SEEK(Style.CstyGroup,lcGrpFile),.T.)
      				                                                               
      
      	 IF POSLN.TRANCD = '2' AND llRecDate 
           =gfSeek(Style.Style+POSLN.CWARECODE+POSLN.CRSESSION ,'STYINVJL')
           IF !BETWEEN(STYINVJL.DTRDATE,ldRecSDate ,ldRecEDate )
             LOOP 
           ENDIF 
         ENDIF
         WAIT WINDOW 'Collecting data for style '+ ALLTRIM(Style.Style)  NOWAIT
         m.Style = Style.Style
         m.nmspackqty = Style.nmspackqty 
         m.nmspackhgt = Style.nmspackhgt
         m.nmspackwdt = Style.nmspackwdt
         m.nmspacklen = Style.nmspacklen
         m.nmspackwgt = Style.nmspackwgt 
         m.TYPE = POSLN.TRANCD 
         m.cvendcode = POSLN.VENDOR
         m.TotQty = POSLN.TOTQTY
		 m.PO = POSLN.PO
		 INSERT INTO (lcRpHdrTmp) FROM MEMVAR 
      ENDSCAN 
    ENDSCAN 
ENDCASE 
SELECT (lcRpHdrTmp)


*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2009
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
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

*:C201113,2 MMT 03/17/2009 Convert Transcation type option to Mover[Start]
FUNCTION lfvTrnType
= lfOGMover(@laRpSource,@laRpTarget,'Tarnsaction Type',.T.,'')  && call mover function.
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 03/17/2009
*! Purpose   : Return the selected Type in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  IF TYPE('laRpTarget[1,1]') = 'U'
    RETURN 
  ENDIF 
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget[1,1])
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget,1]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
*:C201113,2 MMT 03/17/2009 Convert Transcation type option to Mover[End]