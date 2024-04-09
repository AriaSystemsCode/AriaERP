*:***************************************************************************
*: Program file  : MAWEA700.PRG
*: Program desc. : Material CUSTOMER REQUIREMENT
*! Date          : 01/23/2007  (C200734) T20070118.0035
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (MA)
*: Developer     : MAriam Mazhar (MMT)
*:***************************************************************************


  lcWareFile  = ''
  llUseWare   = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'WAREHOUS.CWARECODE'),1)
  IF lnPosition > 0
    lcWareFile = LOOGSCROLL.laOGVRFlt[lnPosition,6]
    llUseWare = IIF(!EMPTY(lcWareFile) .AND. USED(lcWareFile) .AND. RECCOUNT(lcWareFile)>0,.T.,.F.)
  ENDIF
  IF llUseWare
    SELECT(lcWareFile)
    LOCATE 
    IF EOF()
      llUseWare = .F.
    ENDIF 
  ENDIF 
  
  IF !llUseWare 
    =gfModalGen('TRM00250B00000','DIALOG', 'location ') 
    RETURN .F.
  ENDIF
IF loOGScroll.llOGFltCh  
  WAIT 'Selecting Records...' WINDOW NOWAIT
  lfCreatTemp()
  lfCollect()
ELSE
 IF FILE(oAriaApplication.WorkDir +  lcTmpFab + ".DBF")
    USE oAriaApplication.WorkDir +  lcTmpFab + ".DBF" IN 0 
 ENDIF  
ENDIF 



IF !USED(lcTmpFab) OR  RECCOUNT(lcTmpFab) = 0
  =gfModalGen('TRM00052B40011','ALERT')
  IF USED(lcTmpFab)
    SELECT (lcTmpFab)
    USE
  ENDIF    
  RETURN
ENDIF 


DIMENSION loOgScroll.laCRParams[1,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'MATERIAL REQUIREMENTS REPORT'

DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTmpFab + ".DBF"

SELECT(lcTmpFab)
USE
gfDispRe()
RETURN 
*!*************************************************************
*! Name      : lfOGWHEN
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/23/2007
*! Purpose   : OG when function
*!*************************************************************
FUNCTION lfOGWHEN
IF !llFirst 
	lcSqlStatment   = "SELECT ItemLoc.STYLE,ItemLoc.TOTWIP  AS ONORDER,ItemLoc.TOTSTK AS ONHAND ,"+;
					 "ItemLoc.TOTORD,WIP1,DYELOT,ITEM.CSTYMAJOR   FROM ItemLoc(INDEX = STYDYE),ITEM(INDEX = CSTYLE) WHERE ITEMLOC.dyelot ='' AND ITEMLOC.STYLE = ITEM.STYLE AND ITEMLOC.CINVTYPE = ITEM.CINVTYPE "
	lcSqlStatment   = lcSqlStatment   +" AND ITEM.CINVTYPE = 0002"
	lcTable ='ItemLoc'
	lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursorLoc,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
	                                      'BROWSE',SET("DATASESSION"))

	IF lnConnectionHandlar >= 1
	  SELECT(lcCursorLoc)
	  lnBuffering = CURSORGETPROP("Buffering",lcCursorLoc)
	   =CURSORSETPROP("Buffering",3,lcCursorLoc)
	  SELECT (lcCursorLoc)
	  INDEX ON CSTYMAJOR TAG &lcCursorLoc
	  SET ORDER TO TAG &lcCursorLoc
	  SET RELATION TO 
	  LOCATE 
	ENDIF 
	llFirst = .T.
ENDIF 

=gfOpenTable('ITEMLOC','stydyew','SH')
=gfOpenTable('ITEM','STYLE','SH')
=gfOpenTable('CTKTBOM','CTKTYP','SH')
=gfOpenTable('POSHDR','POSHDR','SH')
=gfOpenTable('POSLN','POSLN','SH')
=gfOpenTable('UOM','UOMCODE','SH')


*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/23/2007
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfsumfab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias
lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcCursorLoc)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcCursorLoc)
  LOCATE 
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE cstymajor=lcFab AND DYELOT =''
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  

SELECT(lnAlias)

RETURN INT(lnTotcomp)
*-- end of lfFabSum.
*:**************************************************************************
*:* Name        : lfCreatTemp
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 01/23/2007
*:* Purpose     : Function to adjust date
*:***************************************************************************
FUNCTION lfCreatTemp

DIMENSION laTempacstru[9,4]

laTempacstru[1 ,1] = 'cFabric'
laTempacstru[1 ,2] = 'C'
laTempacstru[1 ,3] = 7
laTempacstru[1 ,4] = ''

laTempacstru[2 ,1] = 'CColor'
laTempacstru[2 ,2] = 'C'
laTempacstru[2 ,3] = 6
laTempacstru[2 ,4] = ''

laTempacstru[3 ,1] = 'FabDesc'
laTempacstru[3 ,2] = 'C'
laTempacstru[3 ,3] = 20
laTempacstru[3 ,4] = ''

laTempacstru[4 ,1] = 'nOnHand'
laTempacstru[4 ,2] = 'N'
laTempacstru[4 ,3] = 12
laTempacstru[4 ,4] = 2

laTempacstru[5 ,1] = 'nReqQty'
laTempacstru[5,2]  = 'N'
laTempacstru[5 ,3] = 12
laTempacstru[5 ,4] = 3

laTempacstru[6 ,1] = 'nOnOrder'
laTempacstru[6,2]  = 'N'
laTempacstru[6 ,3] = 12
laTempacstru[6 ,4] = 3

laTempacstru[7 ,1] = 'Item'
laTempacstru[7,2]  = 'C'
laTempacstru[7 ,3] = 19
laTempacstru[7 ,4] = ''

laTempacstru[8 ,1] = 'cwarecode'
laTempacstru[8,2]  = 'C'
laTempacstru[8 ,3] = 6
laTempacstru[8 ,4] = ''

laTempacstru[9 ,1] = 'cWAREDESC'
laTempacstru[9,2]  = 'C'
laTempacstru[9 ,3] = 35
laTempacstru[9 ,4] = ''


= gfCrtTmp(lcTmpFab,@laTempacstru,'cFabric+cColor',lcTmpFab,.F.)
*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 01/15/2007
*:* Purpose     : Function to Collect data
*:***************************************************************************
FUNCTION lfCollect

*Item
lcItemFile  = ''
llUseItem   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'ITEM.CSTYMAJOR'),1)
IF lnPosition > 0
  lcItemFile = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  llUseItem   = IIF(!EMPTY(lcItemFile) .AND. USED(lcItemFile) .AND. RECCOUNT(lcItemFile)>0,.T.,.F.)
ENDIF
IF llUseItem   
  SELECT(lcItemFile)
  LOCATE 
  IF EOF()
    llUseItem = .F.
  ENDIF 
ENDIF 

*Color
llUseClr = .F.
lnClrPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'RIGHT(ITEM.STYLE,lnColorLen)'),1)
IF lnClrPos > 0
  lcClrStr = LOOGSCROLL.laOGVRFlt[lnClrPos,6]
  lcClrFile = loOGScroll.gfTempName()
  llUseClr = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'CSTYCLR',lcClrFile)
ENDIF

*Type
llUseTyp = .F.
lnTypPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'ITEM.ITEM_TYPE'),1)
IF lnTypPos > 0
  lcTypStr = LOOGSCROLL.laOGVRFlt[lnTypPos,6]
  lcTypFile = loOGScroll.gfTempName()
  llUseTyp = IIF(LEN(lcTypStr)>0,.T.,.F.) AND lfConvertToCursor(lcTypStr,'CITEMTYPE',lcTypFile)
ENDIF

SELECT UOM
DIMENSION laFileStruct[1,18]
AFIELDS(laFileStruct)
lcTempUom = loogscroll.gftempname()
= gfCrtTmp(lcTempUom ,@laFileStruct,'cuomcode',lcTempUom ,.T.)

*collectng data
IF llUseWare
  SELECT POSLN
  gfSetorder('POSLNS')
  SELECT ITEM
  gfSetorder('style')
  SELECT ITEMLOC
  gfSetorder('stydyew')
  SELECT CTktBom
  gfSetorder('ctktitem')
  SELECT (lcWareFile)
  LOCATE 
  SCAN
    SELECT ITEMLOC 
    gfSQLRUN("Select itemLOC.[Desc],itemloc.totstk,itemloc.cwarecode,itemloc.cinvtype,itemloc.style,itemloc.dyelot"+;
             ",ITEM.ITEM_TYPE,item.[Desc],ITEM.CCONVBUY FROM itemloc(index= stydyew) INNER JOIN "+;
             " item(index=Style) ON item.cinvtype = itemloc.cinvtype AND item.style=itemloc.style "+;
             " where itemloc.cinvtype ='0002' AND  itemloc.cwarecode ='"+&lcWareFile..cwarecode+"' AND itemloc.dyelot =''")
             
    LOCATE          
    IF EOF()
      SELECT (lcWareFile)
      LOOP 
    ELSE
    
      SELECT POSLN
      
      =gfSQLRUN("SELECT posln.cinvtype,posln.style,posln.cbusdocu,posln.cstytype,"+;
                "posln.po,posln.[lineno],posln.trancd,POSLN.QTY1 "+;
                " FROM posln  INNER JOIN poshdr "+;
                " ON  posln.po=poshdr.po and posln.cbusdocu =poshdr.cbusdocu AND  posln.cstytype =poshdr.cstytype "+;
                " WHERE   "+;
                " poshdr.cwarecode ='"+&lcWareFile..cwarecode+"' AND PoSHdr.Status = 'O' AND POSHDR.Cstytype = 'M'   AND  posln.cinvtype = '0002'") 

     * =gfSQLRUN("SELECT posln.cinvtype,POSLN.QTY1,posln.style,posln.cbusdocu,"+;
                " posln.cstytype,posln.po,posln.[lineno],posln.trancd "+; 
                " FROM posln(INDEX=poslns) INNER JOIN poshdr (INDEX = poshdr) "+;
                " ON poshdr.cbusdocu = posln.cbusdocu AND "+;
                " poshdr.cstytype = posln.cstytype AND poshdr.po =posln.po "+;
                " WHERE PoSHdr.Status = 'O' AND "+;
                "  poshdr.cwarecode ='"+&lcWareFile..cwarecode+"' AND POSHDR.Cstytype = 'M'"+;
                "  AND posln.cinvtype = '0002'")

      
      *=gfSQLRUN("SELECT posln.cinvtype,POSLN.QTY1,posln.style,posln.cbusdocu,posln.cstytype,posln.po,posln.[lineno],posln.trancd "+;
                " FROM posln(INDEX=poslns) INNER JOIN poshdr (INDEX = poshdr)"+;
                " ON poshdr.cstytype = posln.cstytype AND "+;
                " poshdr.cbusdocu = posln.cbusdocu and poshdr.po =posln.po "+;
                " WHERE "+;
                " POSLN.Cstytype = 'M' AND posln.cinvtype = '0002' AND "+; 
                " poshdr.cwarecode ='"+&lcWareFile..cwarecode+"' AND PoSHdr.Status = 'O'")
               

      
      SELECT CTktBom 
      

      =gfSqlrun("SELECT CTktBom.cinvtype,CTktBom.item,CTktBom.cimtyp,CTktBom.cuttkt,CTktBom.mfgcode,CTktBom.dyelot,cTktBom.Req_Qty,cTktBom.Used_Qty "+;
                " FROM CTktBom(INDEX =CTktBom) INNER JOIN poshdr(index=poshdr) ON poshdr.cstytype ='P' AND poshdr.cbusdocu ='P' "+;
                " AND poshdr.po = CTktBom.CutTkt where ctktbom.cinvtype ='0002' AND ctktbom.cimtyp ='I' "+;
                " AND ctktbom.cwarecode ='"+&lcWareFile..cwarecode +"'  AND  POSHDR.STATUS ='O'")
                

       SELECT itemloc 
       SCAN REST WHILE cwarecode+cinvtype+style+dyelot = &lcWareFile..cwarecode+'0002';
          FOR IIF(llUseItem,SEEK(SUBSTR(ITEMLOC.Style,1,lnMajorlen),lcItemFile),.T.) ;
          AND IIF(llUseClr,SEEK(RIGHT(ITEMLOC.STYLE,lnColorLen),lcClrFile),.T.) AND ;
          IIF(llUseTyp,SEEK(ITEMLOC.ITEM_TYPE,lcTypFile),.T.)
          
          m.cFabric = SUBSTR(ITEMLOC.Style,1,lnMajorlen)
          m.cColor  = RIGHT(ITEMLOC.STYLE,lnColorLen)
          m.FabDesc = itemLOC.Desc
          m.nOnHand = itemloc.totstk
          m.item    = itemLOC.Style
          m.cwarecode = &lcWareFile..cwarecode
          m.cWAREDESC = IIF(SEEK(&lcWareFile..cwarecode,'WAREHOUS','WAREHOUS') or gfSeek(&lcWareFile..cwarecode,'WAREHOUS','WAREHOUS'),WAREHOUS.cdesc,'')
          INSERT INTO (lcTmpFab) FROM MEMVAR 
          WAIT WINDOW 'Selecting Fabric/Color : '+m.cFabric+'/'+m.cColor NOWAIT        
          
          SELECT CTktBom 
          IF SEEK('0002'+m.item+'I')
*          =gfSqlrun("SELECT CTktBom.* FROM CTktBom(INDEX =ctktitem) INNER JOIN poshdr(index=poshdr) ON poshdr.cstytype ='P' AND poshdr.cbusdocu ='P' "+;
                    " AND poshdr.po = CTktBom.CutTkt where ctktbom.cimtyp ='I' AND  ctktbom.cinvtype ='0002'"+;
                    " AND ctktbom.cwarecode ='"+&lcWareFile..cwarecode +"'  AND  ctktbom.item ='"+m.item+"' AND  POSHDR.STATUS ='O'")
                    
            SCAN REST WHILE cinvtype+ item+ cimtyp+cuttkt+ mfgcode+ dyelot='0002'+m.item+'I'
              SELECT (lcTmpFab)
              REPLACE nReqQty WITH nReqQty + (cTktBom.Req_Qty - cTktBom.Used_Qty)
            ENDSCAN     
          ENDIF   
          SELECT POSLN
          IF SEEK('0002'+m.item,'POSLN','POSLNS')
          *gfSQLRUN("Select posln.* "+;
                    " from posln(index=poslns) inner join poshdr (index = poshdr)"+;
                    " on poshdr.po =posln.po and poshdr.cstytype = posln.cstytype and "+;
                    " poshdr.cbusdocu = posln.cbusdocu and posln.cinvtype = '0002'"+;
                    " and posln.style ='"+m.item+"' AND POSLN.Cstytype = 'M' and poshdr.cwarecode ='"+&lcWareFile..cwarecode+"' and PoSHdr.Status = 'O'")
            lnOrdQty = 0
            SCAN REST WHILE cinvtype+ style+ cbusdocu+ cstytype+ po+STR(lineno,6)+ trancd = '0002'+m.item
              IF !SEEK(ITEMLOC.CCONVBUY,lcTempUom)
                 =gfSeek(ITEMLOC.CCONVBUY,'UOM')
                 SELECT UOM
                 SCATTER MEMO MEMVAR 
                 INSERT INTO (lcTempUom) FROM MEMVAR 
              ENDIF 
              SELECT POSLN 
              IF TranCd = '1'
                lnOrdQty = lnOrdQty + (POSLN.QTY1 * &lcTempUom..NCONF)
              ELSE
                lnOrdQty = lnOrdQty - (POSLN.QTY1 * &lcTempUom..NCONF)
              ENDIF
            ENDSCAN 
            REPLACE  nOnOrder WITH IIF(lnOrdQty<0,0,lnOrdQty) IN (lcTmpFab)
          ENDIF 
       ENDSCAN 
     ENDIF 
  ENDSCAN 
ENDIF 

*!*************************************************************
*! Name      : lfConvertToCursor
*:* Developer   : Mariam Mazhar [MMT]
*:* Date        : 01/23/2007
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'CITEMTYPE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYCLR'
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

